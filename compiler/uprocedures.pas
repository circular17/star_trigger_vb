unit uprocedures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl, usctypes, uscope;

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
    WiderScope: integer;
    LineNumber: integer;
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
    Players: TPlayers;
  end;
  ProcedureCount: integer;

function CreateProcedure(AWiderScope: integer; ALineNumber: integer; AName: string; AParamCount: integer; AReturnType: string; APlayers: TPlayers): integer;
function ProcedureIndexOf(AScope: integer; AName: string; AParamCount: integer): integer;
function GetProcedureExprTempVarInt(AProcId, ABitCount: integer): integer;
function ProcedureReturnVar(AProcId: integer): integer;
function ProcessSubStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers = []): integer;

var
  Events: array of record
    LineNumber: integer;
    Players: TPlayers;
    Conditions: TConditionList;
    Instructions: TInstructionList;
    Code: TCodeLineList;
    Preserve: boolean;
    InnerScope: integer;
  end;
  EventCount: integer;

function CreateEvent(AWiderScope: integer; ALineNumber: integer; APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;
function ProcessEventStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers): integer;

var
  ClassDefinitions: array of record
    Name: string;
    Threads: TPlayers;
    InnerScope: integer;
  end;
  ClassCount: integer;

function CreateClass(AWiderScope: integer; AThreads: TPlayers; AName: string): integer;
function ClassIndexOf(AName: string): integer;

procedure ClearProceduresAndEvents;

var
  MainProg: TInstructionList;
  MainCode: TCodeLineList;
  MainSubScope: integer;

implementation

uses uparsevb, utriggerinstructions, uvariables, uparseconditions, utriggerconditions;

function CreateClass(AWiderScope: integer; AThreads: TPlayers; AName: string
  ): integer;
begin
  if ClassCount >= length(ClassDefinitions) then
    setlength(ClassDefinitions, length(ClassDefinitions)*2+4);
  result := ClassCount;
  with ClassDefinitions[result] do
  begin
    Name:= AName;
    Threads:= AThreads;
    InnerScope:= NewScope(AWiderScope, AName);
  end;
  inc(ClassCount);
end;

function ClassIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to ClassCount-1 do
    if CompareText(AName, ClassDefinitions[i].Name)=0 then
      exit(i);
  result := -1;
end;

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

function CreateProcedure(AWiderScope: integer; ALineNumber: integer; AName: string; AParamCount: integer; AReturnType: string; APlayers: TPlayers): integer;
begin
  if ProcedureIndexOf(AWiderScope, AName, AParamCount)<>-1 then
    raise exception.Create('Procedure already declared with this signature');
  CheckReservedWord(AName);

  if plCurrentPlayer in APlayers then
    raise exception.Create('Me cannot be used in Sub or Function definition');

  if (APlayers = []) or (plAllPlayers in APlayers)
    or ([plForce1,plForce2,plForce3,plForce4] <= APlayers)
    or ([plNeutralPlayers,plAllies,plFoes] <= APlayers)
    or ([plPlayer1,plPlayer2,plPlayer3,plPlayer4,plPlayer5,plPlayer6,plPlayer7,plPlayer8] <= APlayers)
  then APlayers := [plAllPlayers];

  if ProcedureCount >= length(Procedures) then
    setlength(Procedures, ProcedureCount*2+4);
  result := ProcedureCount;
  inc(ProcedureCount);

  with Procedures[result] do
  begin
    Name := AName;
    WiderScope := AWiderScope;
    LineNumber:= ALineNumber;
    ParamCount:= AParamCount;
    Instructions := TInstructionList.Create;
    Code := TCodeLineList.Create;
    StartIP := -1;
    ReturnType := AReturnType;
    ReturnBitCount:= GetBitCountOfType(AReturnType);
    Expanded := false;
    StackChecked := false;
    Calls := TIntegerList.Create;
    ExprTempVarInt := -1;
    InnerScope := NewScope(AWiderScope, 'Name');
    Players:= APlayers;
  end;
end;

function ProcedureIndexOf(AScope: integer; AName: string; AParamCount: integer): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to ProcedureCount-1 do
    begin
      if (Procedures[i].WiderScope = AScope) and
        (CompareText(AName, Procedures[i].Name)=0) and
        (AParamCount = Procedures[i].ParamCount) then
        exit(i);
    end;
    AScope := GetWiderScope(AScope);
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

function CreateEvent(AWiderScope: integer; ALineNumber: integer; APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;
begin
  if AConditions.IsArithmetic then
    raise exception.Create('Arithmetic expressions cannot be used in event conditions');

  if EventCount >= length(Events) then
    setlength(Events, EventCount*2+4);
  result := EventCount;
  inc(EventCount);

  with Events[result] do
  begin
    LineNumber:= ALineNumber;
    Players := APlayers;
    Conditions := AConditions;
    Instructions := TInstructionList.Create;
    Code := TCodeLineList.Create;
    Preserve := APreserve;
    InnerScope := NewScope(AWiderScope, '_event' + inttostr(result+1));
  end;
end;

function ProcessSubStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers): integer;
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

  if index >= ALine.Count then
    raise exception.Create('Expecting name');

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

  if CompareText(name,'Main')=0 then
  begin
    if returnType <> 'Void' then
      raise exception.Create('The sub Main cannot have a return value');
    if paramCount <> 0 then
      raise exception.Create('The sub Main takes no parameter');
    exit(-1);
  end else
    result := CreateProcedure(AScope,ALineNumber,name,paramCount,returnType,APlayers);
end;

function ProcessEventStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers): integer;
var
  index: Integer;
  conds: TConditionList;
  preserve: Boolean;
begin
  index := 0;
  preserve := true;
  ExpectToken(ALine,index,'On');

  conds := ExpectConditions(AScope, ALine,index,APlayers);
  result := CreateEvent(AScope, ALineNumber, APlayers, conds, preserve);

  if index < ALine.Count then
    raise exception.Create('End of line expected');
end;

procedure ClearProceduresAndEvents;
var
  i: Integer;
begin
  ClassCount := 0;

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
  if Assigned(MainCode) then MainCode.FreeAll;
  MainCode := TCodeLineList.Create;
  MainSubScope := -1;
end;

initialization

  MainProg := TInstructionList.Create;
  MainCode := TCodeLineList.Create;

finalization

  ClearProceduresAndEvents;
  MainProg.FreeAll;
  MainCode.FreeAll;

end.

