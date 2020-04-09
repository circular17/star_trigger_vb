unit uprocedures;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

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

type
  TParameterDefinition = record
    Name: string;
    VarType: string;
  end;

type
    { TProcedureDefinition }

  TProcedureDefinition = record
    Name: string;
    WiderScope: integer;
    LineNumber: integer;
    Params: array of TParameterDefinition;
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
    function GetParamCount: integer;
  end;

var
  Procedures: array of TProcedureDefinition;
  ProcedureCount: integer;

function CreateProcedure(AWiderScope: integer; ALineNumber: integer; AName: string; AParams: array of TParameterDefinition; AReturnType: string; APlayers: TPlayers): integer;
function ProcedureIndexOf(AScope: integer; AName: string; AParamCount: integer): integer;
function GetProcedureExprTempVarInt(AProcId, ABitCount: integer): integer;
function ProcedureReturnVar(AProcId: integer): integer;
function ProcessSubStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers = []): integer;
function TryFunction(AScope: integer; ALine: TStringList; var AIndex: integer; out AFuncName: string): boolean;
function ProcedureCanHaveUIntParameter(AScope: integer; AName: string; AParamIndex: integer): boolean;
function ProcedureCanHaveBoolParameter(AScope: integer; AName: string; AParamIndex: integer): boolean;

var
  Events: array of record
    LineNumber: integer;
    Players: TPlayers;
    ConditionsCode: TCodeLine;
    Conditions: TConditionList;
    Instructions: TInstructionList;
    Code: TCodeLineList;
    Preserve: boolean;
    InnerScope: integer;
  end;
  EventCount: integer;

function CreateEvent(AWiderScope: integer; ALineNumber: integer; APlayers: TPlayers; AConditionsCode: TCodeLine; APreserve: boolean): integer;
function ProcessEventStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers): integer;

var
  ClassDefinitions: array of record
    Name: string;
    Threads: TPlayers;
    InnerScope: integer;
    UniqueThread: boolean;
  end;
  ClassCount: integer;

function CreateClass(AWiderScope: integer; AThreads: TPlayers; AName: string): integer;
function ClassIndexOf(AName: string): integer;
function TryClassName(ALine: TStringList; var AIndex: integer; AUniqueThreadOnly: boolean = false): integer;

procedure ClearProceduresAndEvents;

var
  MainProg: TInstructionList;
  MainCode: TCodeLineList;
  MainSubScope: integer;

implementation

uses uparsevb, utriggerinstructions, uvariables, uparseconditions, utriggerconditions;

function CreateClass(AWiderScope: integer; AThreads: TPlayers; AName: string): integer;
begin
  if ClassCount >= length(ClassDefinitions) then
    setlength(ClassDefinitions, length(ClassDefinitions)*2+4);
  result := ClassCount;
  with ClassDefinitions[result] do
  begin
    Name:= AName;
    Threads:= AThreads;
    InnerScope:= NewScope(AWiderScope, AName);
    UniqueThread:= IsUniquePlayer(AThreads);
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

function TryClassName(ALine: TStringList; var AIndex: integer; AUniqueThreadOnly: boolean = false): integer;
var
  i: Integer;
begin
  for i := 0 to ClassCount-1 do
    if (not AUniqueThreadOnly or ClassDefinitions[i].UniqueThread) and
       TryToken(ALine, AIndex, ClassDefinitions[i].Name) then
      exit(i);
  result := -1;
end;

{ TProcedureDefinition }

function TProcedureDefinition.GetParamCount: integer;
begin
  result := length(Params);
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

function CreateProcedure(AWiderScope: integer; ALineNumber: integer; AName: string; AParams: array of TParameterDefinition; AReturnType: string; APlayers: TPlayers): integer;
var
  i: Integer;
  scopeName: String;
begin
  if ProcedureIndexOf(AWiderScope, AName, length(AParams))<>-1 then
    raise exception.Create('Procedure already declared with this signature');
  CheckReservedWord(AName);

  if plCurrentPlayer in APlayers then
    raise exception.Create('Me cannot be used in Sub or Function definition');

  if (APlayers = []) or (plAllPlayers in APlayers)
    or ([plForce1,plForce2,plForce3,plForce4] <= APlayers)
    or ([plNeutralPlayers,plAllies,plFoes] <= APlayers)
    or ([plPlayer1,plPlayer2,plPlayer3,plPlayer4,plPlayer5,plPlayer6,plPlayer7,plPlayer8] <= APlayers)
  then APlayers := [plAllPlayers];

  for i := 0 to high(AParams) do
    if (AParams[i].VarType = 'Boolean') and not IsUniquePlayer(APlayers) then
      raise exception.Create('Boolean parameters not allowed for multithreading');

  if ProcedureCount >= length(Procedures) then
    setlength(Procedures, ProcedureCount*2+4);
  result := ProcedureCount;
  inc(ProcedureCount);

  with Procedures[result] do
  begin
    Name := AName;
    WiderScope := AWiderScope;
    LineNumber:= ALineNumber;
    scopeName := Name+'(';
    setlength(Params, length(AParams));
    for i := 0 to high(Params) do
    begin
      Params[i] := AParams[i];
      if i > 0 then scopeName += ', ';
      scopeName += Params[i].VarType;
    end;
    scopeName += ')';
    Instructions := TInstructionList.Create;
    Code := TCodeLineList.Create;
    StartIP := -1;
    ReturnType := AReturnType;
    ReturnBitCount:= GetBitCountOfType(AReturnType);
    Expanded := false;
    StackChecked := false;
    Calls := TIntegerList.Create;
    ExprTempVarInt := -1;
    InnerScope := NewScope(AWiderScope, scopeName);
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
        (AParamCount = Procedures[i].GetParamCount) then
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

function CreateEvent(AWiderScope: integer; ALineNumber: integer; APlayers: TPlayers; AConditionsCode: TCodeLine; APreserve: boolean): integer;
begin
  if EventCount >= length(Events) then
    setlength(Events, EventCount*2+4);
  result := EventCount;
  inc(EventCount);

  with Events[result] do
  begin
    LineNumber:= ALineNumber;
    Players := APlayers;
    Conditions := nil;
    ConditionsCode := AConditionsCode;
    Instructions := TInstructionList.Create;
    Code := TCodeLineList.Create;
    Preserve := APreserve;
    InnerScope := NewScope(AWiderScope, '_event' + inttostr(result+1));
  end;
end;

function ProcessSubStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers): integer;
var
  index, i: Integer;
  name: String;
  isFunc: boolean;
  returnType, varName, varType: string;
  params: array of TParameterDefinition;
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

  params := nil;
  if TryToken(ALine,index,'(') then
  begin
    repeat
      if PeekToken(ALine,index,'ByVal') or PeekToken(ALine,index,'ByRef') then
        raise exception.Create('Parameters are always passed by value');
      if TryIdentifier(ALine,index, varName, false) then
      begin
        if IsVarNameUsed(AScope, varName, 0) then
          raise exception.Create('Name already used in this scope');
        ExpectToken(ALine,index,'As');
        if not TryUnsignedIntegerType(ALine, index, varType) then
        begin
          if TryToken(ALine, index, 'Boolean') then
            varType := 'Boolean'
          else
          if TryToken(ALine, index, 'String') then
            raise exception.Create('String type not accepted here')
          else
            raise exception.Create('Expecting variable type');
        end;
        setlength(params, length(params)+1);
        params[high(params)].Name:= varName;
        params[high(params)].VarType:= varType;
      end else
       break;
    until not TryToken(ALine, index, ',');
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
    if length(params) <> 0 then
      raise exception.Create('The sub Main takes no parameter');
    exit(-1);
  end else
  begin
    result := CreateProcedure(AScope,ALineNumber,name,params,returnType,APlayers);
    if IsUniquePlayer(APlayers) then
    begin
      for i := 0 to high(params) do
      begin
        if params[i].VarType = 'Boolean' then
          CreateBoolVar(Procedures[result].InnerScope, params[i].Name, svClear)
        else
          CreateIntVar(Procedures[result].InnerScope, params[i].Name,
                       0, GetBitCountOfType(params[i].VarType));
      end;
    end else
      for i := 0 to high(params) do
        CreateMultithreadIntVar(Procedures[result].Players,
                                Procedures[result].InnerScope,
                                params[i].Name, GetBitCountOfType(params[i].VarType));
  end;
end;

function TryFunction(AScope: integer; ALine: TStringList; var AIndex: integer;
  out AFuncName: string): boolean;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to ProcedureCount-1 do
    if (Procedures[i].ReturnType <> 'Void') and
      (Procedures[i].WiderScope = AScope) and
      TryToken(ALine, AIndex, Procedures[i].Name) then
      begin
        AFuncName:= Procedures[i].Name;
        exit(true);
      end;
    AScope := GetWiderScope(AScope);
  end;
  AFuncName := '';
  result := false;
end;

function ProcedureCanHaveUIntParameter(AScope: integer; AName: string; AParamIndex: integer): boolean;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to ProcedureCount-1 do
      if (Procedures[i].WiderScope = AScope) and
        (CompareText(AName, Procedures[i].Name)=0) then
      begin
        if (length(Procedures[i].Params) > AParamIndex) and
           IsIntegerType(Procedures[i].Params[AParamIndex].VarType) then
           exit(true);
      end;
    AScope := GetWiderScope(AScope);
  end;
  result := false;
end;

function ProcedureCanHaveBoolParameter(AScope: integer; AName: string; AParamIndex: integer): boolean;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to ProcedureCount-1 do
      if (Procedures[i].WiderScope = AScope) and
        (CompareText(AName, Procedures[i].Name)=0) then
      begin
        if (length(Procedures[i].Params) > AParamIndex) and
           (Procedures[i].Params[AParamIndex].VarType = 'Boolean') then
           exit(true);
      end;
    AScope := GetWiderScope(AScope);
  end;
  result := false;
end;

function ProcessEventStatement(AScope: integer; ALineNumber: integer; ALine: TStringList; APlayers: TPlayers): integer;
var
  index: Integer;
  preserve: Boolean;
  conds: TCodeLine;
begin
  index := 0;
  preserve := true;
  ExpectToken(ALine,index,'On');

  conds := TCodeLine.Create(ALine, ALineNumber);
  conds.Tokens.Delete(0);
  result := CreateEvent(AScope, ALineNumber, APlayers, conds, preserve);
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
      ConditionsCode.Free;
      Code.FreeAll;
    end;
  EventCount:= 0;

  if Assigned(MainProg) then MainProg.FreeAll;
  MainProg := TInstructionList.Create;
  if Assigned(MainCode) then MainCode.FreeAll;
  MainCode := TCodeLineList.Create;
  MainSubScope := -1;
end;

function IsProcOrClassNameUsedImplementation(AScope: integer; AName: string; AParamCount: integer): boolean;
begin
  result := (ProcedureIndexOf(AScope, AName, AParamCount) <> -1) or
            (ClassIndexOf(AName) <> -1) or
            (CompareText('CountIf',AName)=0) or (CompareText('UnitCount',AName) = 0) or
            (CompareText('KillCount',AName) = 0) or (CompareText('DeathCount',AName) = 0);
end;

initialization

  MainProg := TInstructionList.Create;
  MainCode := TCodeLineList.Create;
  IsProcOrClassNameUsed:= @IsProcOrClassNameUsedImplementation;

finalization

  ClearProceduresAndEvents;
  MainProg.FreeAll;
  MainCode.FreeAll;

end.

