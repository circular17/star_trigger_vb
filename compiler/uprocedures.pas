unit uprocedures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl, usctypes;

type
  TIntegerList = specialize TFPGList<Integer>;

  { TCodeLine }

  TCodeLine = class
    LineNumber: integer;
    Tokens: TStringList;
    constructor Create(ATokens: TStringList; ALineNumber: integer);
    destructor Destroy; override;
  end;
  TCustomCodeLineList = specialize TFPGList<TCodeLine>;

  { TCodeLineList }

  TCodeLineList = class(TCustomCodeLineList)
    procedure FreeAll;
  end;

var
  Procedures: array of record
    Name: string;
    ParamCount: integer;
    Instructions: TInstructionList;
    Code: TCodeLineList;
    StartIP: integer;
    ReturnType: string;
    ReturnBitCount: integer;
    Expanded, StackChecked: boolean;
    Calls: TIntegerList;
    ExprTempVarInt: integer;
    InnerScope: integer;
  end;
  ProcedureCount: integer;

function CreateProcedure(AName: string; AParamCount: integer; AReturnType: string): integer;
function ProcedureIndexOf(AName: string; AParamCount: integer): integer;
function GetProcedureExprTempVarInt(AProcId, ABitCount: integer): integer;
function ProcedureReturnVar(AProcId: integer): integer;
function ProcessSubStatement(ALine: TStringList): integer;

var
  Events: array of record
    Players: TPlayers;
    Conditions: TConditionList;
    Instructions: TInstructionList;
    Code: TCodeLineList;
    Preserve: boolean;
    InnerScope: integer;
  end;
  EventCount: integer;

function CreateEvent(APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;
function ProcessEventStatement(ALine: TStringList; APlayers: TPlayers; ACheckStop: boolean): integer;

procedure ClearProceduresAndEvents;

const
  SubNewScope = 1;

var
  MainProg: TInstructionList;
  MainCode: TCodeLineList;

implementation

uses uparsevb, utriggerinstructions, uvariables, uparseconditions, utriggerconditions;

{ TCodeLine }

constructor TCodeLine.Create(ATokens: TStringList; ALineNumber: integer);
begin
  Tokens := TStringList.Create;
  Tokens.AddStrings(ATokens);
  LineNumber := ALineNumber;
end;

destructor TCodeLine.Destroy;
begin
  Tokens.Free;
  inherited Destroy;
end;

{ TCodeLineList }

procedure TCodeLineList.FreeAll;
var
  i: Integer;
begin
  if self <> nil then
  begin
    for i := 0 to Count-1 do
      Items[i].Free;
    Free;
  end;
end;

function CreateProcedure(AName: string; AParamCount: integer; AReturnType: string): integer;
begin
  if ProcedureIndexOf(AName, AParamCount)<>-1 then
    raise exception.Create('Procedure already declared with this signature');
  CheckReservedWord(AName);

  if ProcedureCount >= length(Procedures) then
    setlength(Procedures, ProcedureCount*2+4);
  result := ProcedureCount;
  inc(ProcedureCount);

  with Procedures[result] do
  begin
    Name := AName;
    ParamCount:= AParamCount;
    Instructions := TInstructionList.Create;
    Instructions.Add(TCommentInstruction.Create('Sub '+AName));
    Code := TCodeLineList.Create;
    StartIP := -1;
    ReturnType := AReturnType;
    ReturnBitCount:= GetBitCountOfType(AReturnType);
    Expanded := false;
    StackChecked := false;
    Calls := TIntegerList.Create;
    ExprTempVarInt := -1;
    InnerScope := result+2;
  end;
end;

function ProcedureIndexOf(AName: string; AParamCount: integer): integer;
var
  i: Integer;
begin
  for i := 0 to ProcedureCount-1 do
  begin
    if (CompareText(AName, Procedures[i].Name)=0) and
      (AParamCount = Procedures[i].ParamCount) then
      exit(i);
  end;
  exit(-1);
end;

function GetProcedureExprTempVarInt(AProcId, ABitCount: integer): integer;
begin
  if Procedures[AProcId].ExprTempVarInt = -1 then
    Procedures[AProcId].ExprTempVarInt:= AllocateTempInt(ABitCount)
  else
  begin
    if IntVars[Procedures[AProcId].ExprTempVarInt].BitCount < ABitCount then
      IntVars[Procedures[AProcId].ExprTempVarInt].BitCount := ABitCount;
  end;
  result := Procedures[AProcId].ExprTempVarInt;
end;

function ProcedureReturnVar(AProcId: integer): integer;
begin
  result := GetProcedureExprTempVarInt(AProcId, Procedures[AProcId].ReturnBitCount);
end;

function CreateEvent(APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;
begin
  if AConditions.IsArithmetic then
    raise exception.Create('Arithmetic expressions cannot be used in event conditions');

  if EventCount >= length(Events) then
    setlength(Events, EventCount*2+4);
  result := EventCount;
  inc(EventCount);

  with Events[result] do
  begin
    Players := APlayers;
    Conditions := AConditions;
    Instructions := TInstructionList.Create;
    Instructions.Add(TCommentInstruction.Create('Event'));
    Code := TCodeLineList.Create;
    Preserve := APreserve;
    InnerScope := -1-result;
  end;
end;

function ProcessSubStatement(ALine: TStringList): integer;
var
  index: Integer;
  name: String;
  isFunc: boolean;
  returnType: string;
  paramCount: integer;
begin
  index := 0;
  isFunc := TryToken(ALine,index,'Function');
  if not isFunc then ExpectToken(ALine,index,'Sub');

  name := ALine[index];
  if not IsValidVariableName(name) then
    raise exception.Create('Invalid procedure name');
  inc(index);

  paramCount := 0;
  if TryToken(ALine,index,'(') then
  begin
    ExpectToken(ALine,index,')');
  end;

  if isFunc then
  begin
    ExpectToken(ALine,index,'As');
    if TryToken(ALine,index,'Boolean') then
      returnType := 'Boolean'
    else
    begin
      if index >= ALine.Count then
        returnType := ''
      else
      begin
        returnType := ALine[index];
        inc(index);
      end;

      if not IsIntegerType(returnType) then
        raise exception.Create('Expecting return type (Byte, UInt16, UInt24, Boolean)');
    end;
  end else
    returnType := 'Void';

  if index < ALine.Count then
    raise exception.Create('End of line expect but "' + ALine[index] + '" found');

  if CompareText(name,'New')=0 then
  begin
    if returnType <> 'Void' then
      raise exception.Create('The sub New cannot have a return value');
    if paramCount <> 0 then
      raise exception.Create('The sub New takes no parameter');
    exit(-1);
  end else
    result := CreateProcedure(name,paramCount,returnType);
end;

function ProcessEventStatement(ALine: TStringList; APlayers: TPlayers; ACheckStop: boolean): integer;
var
  index, varIdx: Integer;
  conds: TConditionList;
begin
  index := 0;
  ExpectToken(ALine,index,'On');

  conds := ExpectConditions(GlobalScope, ALine,index,APlayers);
  if ACheckStop then
  begin
    varIdx := GetStopEventBoolVar;
    conds.Add( TSwitchCondition.Create(BoolVars[varIdx].Switch,False) );
  end;

  result := CreateEvent(APlayers, conds, not TryToken(ALine,index,'Once'));

  if index < ALine.Count then
    raise exception.Create('End of line expected');
end;

procedure ClearProceduresAndEvents;
var
  i: Integer;
begin
  for i := 0 to ProcedureCount-1 do
    with Procedures[i] do
    begin
      Instructions.FreeAll;
      Calls.Free;
      Code.FreeAll;
    end;
  ProcedureCount:= 0;

  for i := 0 to EventCount-1 do
    with Events[i] do
    begin
      Instructions.FreeAll;
      Conditions.FreeAll;
      Code.FreeAll;
    end;
  EventCount:= 0;

  if Assigned(MainProg) then MainProg.FreeAll;
  MainProg := TInstructionList.Create;
  MainProg.Add(TCommentInstruction.Create('Sub New'));
  if Assigned(MainCode) then MainCode.FreeAll;
  MainCode := TCodeLineList.Create;
end;

initialization

  MainProg := TInstructionList.Create;
  MainCode := TCodeLineList.Create;

finalization

  ClearProceduresAndEvents;
  MainProg.FreeAll;
  MainCode.FreeAll;

end.

