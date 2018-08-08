unit ureadprog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl, usctypes;

function ReadProg(AFilename: string; out AMainThread: TPlayer): boolean;

type
  TIntegerList = specialize TFPGList<Integer>;

var
  MainProg: TInstructionList;
  Procedures: array of record
    Name: string;
    ParamCount: integer;
    Instructions: TInstructionList;
    StartIP: integer;
    ReturnType: string;
    ReturnIntVar: integer;
    Done: boolean;
    Calls: TIntegerList;
  end;
  ProcedureCount: integer;

function CreateProcedure(AName: string; AParamCount: integer; AReturnType: string): integer;
function ProcedureIndexOf(AName: string; AParamCount: integer): integer;
function ProcedureReturnVar(AProcId: integer): integer;

var
  Events: array of record
    Players: TPlayers;
    Conditions: TConditionList;
    Instructions: TInstructionList;
    Preserve: boolean;
  end;
  EventCount: integer;

function CreateEvent(APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;

var HyperTriggers: boolean;

implementation

uses uparsevb, uvariables, uexpressions;

function TryNeutralConditionFunction(ALine: TStringList; var AIndex: Integer; ANegation: boolean): TCondition;
var
  op: TConditionOperator;
  intVal: Integer;
begin
  result := nil;
  if TryToken(ALine,AIndex,'ElapsedTime') then
  begin
    op := TryConditionOperator(ALine,AIndex);
    if op = coNone then
      raise exception.Create('Comparison expected');

    if ANegation then op := NotConditionOperator[op];

    intVal := ExpectInteger(ALine,AIndex);
    case op of
    coEqual: result := TElapsedTimeCondition.Create(icmExactly, intVal);
    coGreaterThanOrEqual: result := TElapsedTimeCondition.Create(icmAtLeast, intVal);
    coLowerThanOrEqual: result := TElapsedTimeCondition.Create(icmAtMost, intVal);
    coGreaterThan: if intVal = maxLongint then
                      result := TNeverCondition.Create
                   else
                      result := TElapsedTimeCondition.Create(icmAtLeast, intVal+1);
    coLowerThan: if intVal = 0 then
                      result := TNeverCondition.Create
                   else
                      result := TElapsedTimeCondition.Create(icmAtMost, intVal-1);
    coNotEqual: result := TNotCondition.Create([TElapsedTimeCondition.Create(icmExactly, intVal)]);
    else
      raise exception.Create('Unhandled case');
    end;
    exit;
  end;
end;

function TryPlayerConditionFunction(ALine: TStringList; var AIndex: Integer; APlayer: TPlayer; ANegation: boolean): TCondition;
var
  unitType, locStr: String;
  op: TConditionOperator;
  intVal: Integer;

  procedure ExpectCurrentPlayer;
  begin
    If APlayer <> plCurrentPlayer then raise exception.Create('Checking against Min/Max can only be done with the current player ("Me")');
  end;

begin
  if TryToken(ALine,AIndex,'UnitCount') then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      unitType := ExpectString(ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
        locStr := ExpectString(ALine,AIndex)
      else
        locStr := '';
      ExpectToken(ALine,AIndex,')');
    end else
    begin
      unitType := 'Any unit';
      locStr := '';
    end;
    op := TryConditionOperator(ALine,AIndex);
    if op = coNone then
      raise exception.Create('Comparison expected');

    if ANegation then op := NotConditionOperator[op];

    if TryToken(ALine,AIndex,'Min') then
    begin
      ExpectCurrentPlayer;
      case op of
      coEqual,coLowerThanOrEqual:
        result := TCompareUnitCountCondition.Create(unitType,locStr,False);
      coGreaterThanOrEqual:
        result := TAlwaysCondition.Create;
      coGreaterThan,coNotEqual:
        result := TNotCondition.Create([TCompareUnitCountCondition.Create(unitType,locStr,False)]);
      coLowerThan:
        result := TNeverCondition.Create;
      else
        raise exception.Create('Unhandled case');
      end;
    end
    else
    if TryToken(ALine,AIndex,'Max') then
    begin
      ExpectCurrentPlayer;
      case op of
      coEqual,coGreaterThanOrEqual:
        result := TCompareUnitCountCondition.Create(unitType,locStr,true);
      coLowerThanOrEqual:
        result := TAlwaysCondition.Create;
      coLowerThan,coNotEqual:
        result := TNotCondition.Create([TCompareUnitCountCondition.Create(unitType,locStr,true)]);
      coGreaterThan:
        result := TNeverCondition.Create;
      else
        raise exception.Create('Unhandled case');
      end;
    end
    else
    begin
      intVal := ExpectInteger(ALine,AIndex);
      case op of
      coEqual: result := TBringCondition.Create(APlayer, unitType, locStr, icmExactly, intVal);
      coGreaterThanOrEqual: result := TBringCondition.Create(APlayer, unitType, locStr, icmAtLeast, intVal);
      coLowerThanOrEqual: result := TBringCondition.Create(APlayer, unitType, locStr, icmAtMost, intVal);
      coGreaterThan: if intVal = maxLongint then
                        result := TNeverCondition.Create
                     else
                        result := TBringCondition.Create(APlayer, unitType, locStr, icmAtLeast, intVal+1);
      coLowerThan: if intVal = 0 then
                        result := TNeverCondition.Create
                     else
                        result := TBringCondition.Create(APlayer, unitType, locStr, icmAtMost, intVal-1);
      coNotEqual: result := TNotCondition.Create([TBringCondition.Create(APlayer, unitType, locStr, icmExactly, intVal)]);
      else
        raise exception.Create('Unhandled case');
      end;
    end;

    exit;
  end else
  if TryToken(ALine,AIndex,'KillCount') then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      unitType := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
    end else
    begin
      unitType := 'Any unit';
    end;
    op := TryConditionOperator(ALine,AIndex);
    if op = coNone then
      raise exception.Create('Comparison expected');

    if ANegation then op := NotConditionOperator[op];

    if TryToken(ALine,AIndex,'Min') then
    begin
      ExpectCurrentPlayer;
      case op of
      coEqual,coLowerThanOrEqual:
        result := TCompareKillCountCondition.Create(unitType,False);
      coGreaterThanOrEqual:
        result := TAlwaysCondition.Create;
      coGreaterThan,coNotEqual:
        result := TNotCondition.Create([TCompareKillCountCondition.Create(unitType,False)]);
      coLowerThan:
        result := TNeverCondition.Create;
      else
        raise exception.Create('Unhandled case');
      end;
    end
    else
    if TryToken(ALine,AIndex,'Max') then
    begin
      ExpectCurrentPlayer;
      case op of
      coEqual,coGreaterThanOrEqual:
        result := TCompareKillCountCondition.Create(unitType,true);
      coLowerThanOrEqual:
        result := TAlwaysCondition.Create;
      coLowerThan,coNotEqual:
        result := TNotCondition.Create([TCompareKillCountCondition.Create(unitType,true)]);
      coGreaterThan:
        result := TNeverCondition.Create;
      else
        raise exception.Create('Unhandled case');
      end;
    end
    else
    begin
      intVal := ExpectInteger(ALine,AIndex);
      case op of
      coEqual: result := TKillCountCondition.Create(APlayer, unitType, icmExactly, intVal);
      coGreaterThanOrEqual: result := TKillCountCondition.Create(APlayer, unitType, icmAtLeast, intVal);
      coLowerThanOrEqual: result := TKillCountCondition.Create(APlayer, unitType, icmAtMost, intVal);
      coGreaterThan: if intVal = maxLongint then
                        result := TNeverCondition.Create
                     else
                        result := TKillCountCondition.Create(APlayer, unitType, icmAtLeast, intVal+1);
      coLowerThan: if intVal = 0 then
                        result := TNeverCondition.Create
                     else
                        result := TKillCountCondition.Create(APlayer, unitType, icmAtMost, intVal-1);
      coNotEqual: result := TNotCondition.Create([TKillCountCondition.Create(APlayer, unitType, icmExactly, intVal)]);
      else
        raise exception.Create('Unhandled case');
      end;
    end;
    exit;
  end else
  if TryToken(ALine,AIndex,'DeathCount') then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      unitType := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
    end else
    begin
      unitType := 'Any unit';
    end;
    op := TryConditionOperator(ALine,AIndex);
    if op = coNone then
      raise exception.Create('Comparison expected');

    if ANegation then op := NotConditionOperator[op];

    intVal := ExpectInteger(ALine,AIndex);
    case op of
    coEqual: result := TIntegerCondition.Create(APlayer, unitType, icmExactly, intVal);
    coGreaterThanOrEqual: result := TIntegerCondition.Create(APlayer, unitType, icmAtLeast, intVal);
    coLowerThanOrEqual: result := TIntegerCondition.Create(APlayer, unitType, icmAtMost, intVal);
    coGreaterThan: if intVal = maxLongint then
                      result := TNeverCondition.Create
                   else
                      result := TIntegerCondition.Create(APlayer, unitType, icmAtLeast, intVal+1);
    coLowerThan: if intVal = 0 then
                      result := TNeverCondition.Create
                   else
                      result := TIntegerCondition.Create(APlayer, unitType, icmAtMost, intVal-1);
    coNotEqual: result := TNotCondition.Create([TIntegerCondition.Create(APlayer, unitType, icmExactly, intVal)]);
    else
      raise exception.Create('Unhandled case');
    end;
    exit;
  end else
  if TryToken(ALine,AIndex,'OpponentCount') then
  begin
    op := TryConditionOperator(ALine,AIndex);
    if op = coNone then
      raise exception.Create('Comparison expected');

    if ANegation then op := NotConditionOperator[op];

    intVal := ExpectInteger(ALine,AIndex);
    case op of
    coEqual: result := TOpponentCountCondition.Create(APlayer, icmExactly, intVal);
    coGreaterThanOrEqual: result := TOpponentCountCondition.Create(APlayer, icmAtLeast, intVal);
    coLowerThanOrEqual: result := TOpponentCountCondition.Create(APlayer, icmAtMost, intVal);
    coGreaterThan: if intVal = maxLongint then
                      result := TNeverCondition.Create
                   else
                      result := TOpponentCountCondition.Create(APlayer, icmAtLeast, intVal+1);
    coLowerThan: if intVal = 0 then
                      result := TNeverCondition.Create
                   else
                      result := TOpponentCountCondition.Create(APlayer, icmAtMost, intVal-1);
    coNotEqual: result := TNotCondition.Create([TOpponentCountCondition.Create(APlayer, icmExactly, intVal)]);
    else
      raise exception.Create('Unhandled case');
    end;
    exit;
  end else
    result := nil;
end;

function ExpectCondition(ALine: TStringList; var AIndex: integer; AThreads: TPlayers): TCondition;
var
  intVal: Integer;
  op: TConditionOperator;
  boolNot: Boolean;
  scalar: TScalarVariable;
  boolVal: boolean;
  pl: TPlayer;
begin
  if TryBoolean(ALine,AIndex,boolVal) then
  begin
    if boolVal then
      result := TAlwaysCondition.Create
    else
      result := TNeverCondition.Create
  end else
  begin
    boolNot := TryToken(ALine,AIndex,'Not');

    scalar := TryScalarVariable(ALine,AIndex);
    if scalar.VarType = svtNone then
    begin
      pl := TryParsePlayer(ALine,AIndex);
      if pl <> plNone then
      begin
        ExpectToken(ALine,AIndex,'.');
        result := TryPlayerConditionFunction(ALine, AIndex, pl, boolNot);
        if result = nil then raise exception.Create('Expecting player condition');
        exit;
      end;

      result := TryNeutralConditionFunction(ALine, AIndex, boolNot);
      if result <> nil then exit;

      if TryBoolean(ALine,AIndex,boolVal) then
      begin
        if boolVal xor boolNot then
          result := TAlwaysCondition.Create
        else
          result := TNeverCondition.Create;
        exit;
      end else
      begin
       result := TryPlayerConditionFunction(ALine, AIndex, plCurrentPlayer, boolNot);
       if result = nil then
         raise exception.Create('Expecting variable or function')
       else
       if not IsUniquePlayer(AThreads) or (AThreads = [plCurrentPlayer]) then
       begin
         result.Free;
         raise exception.Create('You need to specify the player to which it applies ("Me" for each player)')
       end;
       exit;
      end;
    end;

    case scalar.VarType of
    svtInteger:
    begin
      op := TryConditionOperator(ALine,AIndex);
      if op = coNone then
        raise exception.Create('Comparison expected')
      else
      begin
        if boolNot then op := NotConditionOperator[op];

        if (scalar.Player in [plNone,plCurrentPlayer]) and TryToken(ALine,AIndex,'Min') then
        begin
          case op of
          coEqual,coLowerThanOrEqual:
            result := TCompareIntegerCondition.Create(scalar.UnitType,False);
          coGreaterThanOrEqual:
            result := TAlwaysCondition.Create;
          coGreaterThan,coNotEqual:
            result := TNotCondition.Create([TCompareIntegerCondition.Create(scalar.UnitType,False)]);
          coLowerThan:
            result := TNeverCondition.Create;
          else
            raise exception.Create('Unhandled case');
          end;
        end
        else
        if (scalar.Player in [plNone,plCurrentPlayer]) and TryToken(ALine,AIndex,'Max') then
        begin
          case op of
          coEqual,coGreaterThanOrEqual:
            result := TCompareIntegerCondition.Create(scalar.UnitType,true);
          coLowerThanOrEqual:
            result := TAlwaysCondition.Create;
          coLowerThan,coNotEqual:
            result := TNotCondition.Create([TCompareIntegerCondition.Create(scalar.UnitType,true)]);
          coGreaterThan:
            result := TNeverCondition.Create;
          else
            raise exception.Create('Unhandled case');
          end;
        end
        else
        begin
          intVal := ExpectInteger(ALine,AIndex);
          case op of
          coEqual: result := TIntegerCondition.Create(scalar.Player, scalar.UnitType, icmExactly, intVal);
          coGreaterThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.UnitType, icmAtLeast, intVal);
          coLowerThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.UnitType, icmAtMost, intVal);
          coGreaterThan: if intVal = maxLongint then
                            result := TNeverCondition.Create
                         else
                            result := TIntegerCondition.Create(scalar.Player, scalar.UnitType, icmAtLeast, intVal+1);
          coLowerThan: if intVal = 0 then
                            result := TNeverCondition.Create
                         else
                            result := TIntegerCondition.Create(scalar.Player, scalar.UnitType, icmAtMost, intVal-1);
          coNotEqual: result := TNotCondition.Create([TIntegerCondition.Create(scalar.Player, scalar.UnitType, icmExactly, intVal)]);
          else
            raise exception.Create('Unhandled case');
          end;
        end;
      end;
    end;
    svtSwitch:
      result := TSwitchCondition.Create(scalar.Switch, not boolNot)
    else
        raise exception.Create('Unhandled case');
    end;
  end;
end;

function ExpectConditions(ALine: TStringList; var AIndex: integer; AThreads: TPlayers): TConditionList;
var
  i, j: Integer;
begin
  result := TConditionList.Create;
  try
    repeat
      result.Add(ExpectCondition(ALine,AIndex,AThreads));
      if not TryToken(ALine,AIndex,'And') then break;
    until false;
  except on ex:exception do
    begin
      result.Free;
      raise exception.Create(ex.Message);
    end;
  end;
  for i := result.Count-1 downto 0 do
    if (result[i] is TAlwaysCondition) and (result.Count > 1) then
    begin
      result[i].Free;
      result.Delete(i);
    end else
    if result[i] is TNeverCondition then
    begin
      for j := result.Count-1 downto 0 do
        if j <> i then
        begin
          result[j].Free;
          result.Delete(j);
        end;
    end;
end;

function TryUnitProperties(ALine: TStringList; var AIndex: integer; out AProp: TUnitProperties): boolean;
var idx, intVal: integer;
  name: String;
  valueType: string;
  boolVal: boolean;

  procedure ValueInteger(AMin,AMax: integer);
  begin
    if valueType <> 'Integer' then
      raise exception.Create('Expecting integer value');
    if (intVal < AMin) or (intVal > AMax) then
      raise exception.Create('Value out of range (' + inttostr(AMin)+' to '+Inttostr(AMax)+')');
  end;
  procedure ValueBool;
  begin
    if valueType <> 'Boolean' then
      raise exception.Create('Expecting bool value');
  end;

begin
  AProp.Life := 100;
  AProp.Shield := 100;
  AProp.Energy := 25;
  AProp.Resource := 0;
  AProp.HangarCount := 0;
  AProp.Burrowed:= false;
  AProp.Cloaked:= false;
  AProp.Hallucinated:= false;
  AProp.Invincible:= false;
  AProp.Lifted := false;
  result := false;

  idx := AIndex;
  intVal := 0;

  if not TryToken(ALine,idx,'{') then exit;
  if not TryToken(ALine,idx,'.') then exit;

  while true do
  begin
    if idx >= ALine.Count then raise exception.Create('Unexpected end of line');
    name := ALine[idx];
    if not IsValidVariableName(name) then raise exception.Create('Expecting variable name but "' + name + '" found');
    inc(idx);
    ExpectToken(ALine,idx,'=');
    if TryInteger(ALine,idx,intVal) then valueType := 'Integer'
    else if TryBoolean(ALine,idx,boolVal) then valueType := 'Boolean'
    else raise exception.Create('Expecting boolean or integer value');

    if CompareText(name,'Life')=0 then
    begin
      ValueInteger(0,100);
      AProp.Life := intVal;
    end else
    if CompareText(name,'Shield')=0 then
    begin
      ValueInteger(0,100);
      AProp.Shield := intVal;
    end else
    if CompareText(name,'Energy')=0 then
    begin
      ValueInteger(0,100);
      AProp.Energy := intVal;
    end else
    if CompareText(name,'Resource')=0 then
    begin
      ValueInteger(0,maxLongint);
      AProp.Resource := intVal;
    end else
    if CompareText(name,'HangarCount')=0 then
    begin
      ValueInteger(0,maxLongint);
      AProp.HangarCount := intVal;
    end else
    if CompareText(name,'Burrowed')=0 then
    begin
      ValueBool;
      AProp.Burrowed := boolVal;
    end else
    if CompareText(name,'Cloaked')=0 then
    begin
      ValueBool;
      AProp.Cloaked := boolVal;
    end else
    if CompareText(name,'Hallucinated')=0 then
    begin
      ValueBool;
      AProp.Hallucinated := boolVal;
    end else
    if CompareText(name,'Invincible')=0 then
    begin
      ValueBool;
      AProp.Invincible := boolVal;
    end else
    if CompareText(name,'Lifted')=0 then
    begin
      ValueBool;
      AProp.Lifted := boolVal;
    end;

    if not TryToken(ALine,idx,',') then
    begin
      ExpectToken(ALine,idx,'}');
      break;
    end else
      ExpectToken(ALine,idx,'.');
  end;
  AIndex := idx;
  result := true;
end;

function TryUnitPropVar(ALine: TStringList; var AIndex: integer): integer;
var
  idxProp: Integer;
  prop: TUnitProperties;
begin
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
  begin
    idxProp := UnitPropIndexOf(ALine[AIndex]);
    if idxProp <> -1 then
    begin
      inc(AIndex);
      exit(idxProp);
    end;
  end;

  if TryUnitProperties(ALine,AIndex,prop) then
  begin
    result := FindOrCreateUnitProperty(prop);
  end else
    exit(-1);
end;

function IsVarNameUsed(AName: string; AParamCount: integer): boolean;
begin
  result := (IntVarIndexOf(AName)<>-1) or (BoolVarIndexOf(AName)<>-1) or (IntArrayIndexOf(AName)<>-1) or
    (ProcedureIndexOf(AName,AParamCount)<>-1) or (UnitPropIndexOf(AName) <> -1) or (StringIndexOf(AName)<>-1) or
    (SoundIndexOf(AName)<>-1);
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
    StartIP := -1;
    ReturnType := AReturnType;
    Done := false;
    Calls := TIntegerList.Create;
    ReturnIntVar := -1;
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

function ProcedureReturnVar(AProcId: integer): integer;
begin
  if Procedures[AProcId].ReturnIntVar = -1 then
    Procedures[AProcId].ReturnIntVar:= CreateIntVar('_' + Procedures[AProcId].Name + '_result', 0);
  result := Procedures[AProcId].ReturnIntVar;
end;

function CreateEvent(APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;
begin
  if EventCount >= length(Events) then
    setlength(Events, EventCount*2+4);
  result := EventCount;
  inc(EventCount);

  with Events[result] do
  begin
    Players := APlayers;
    Conditions := AConditions;
    Instructions := TInstructionList.Create;
    Preserve := APreserve;
  end;
end;

function ParseIntArray(ALine: TStringList; var AIndex: integer): ArrayOfInteger;
var count: integer;
begin
  setlength(result, 4);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count >= length(result) then
      setlength(result, Count*2 + 4);

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if not TryInteger(ALine, AIndex, result[count]) then
      raise exception.Create('Expecting integer or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ParseBoolArray(ALine: TStringList; var AIndex: integer): ArrayOfSwitchValue;
var count: integer;
  boolVal: boolean;
begin
  setlength(result, 4);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count >= length(result) then
      setlength(result, Count*2 + 4);

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if TryBoolean(ALine,AIndex,boolVal) then
      result[Count] := BoolToSwitch[boolval]
    else if TryToken(ALine,AIndex,'Rnd') then
      result[Count] := svRandomize
    else
      raise exception.Create('Expecting boolean or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ProcessSub(ALine: TStringList): integer;
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
    if TryToken(ALine,index,'Integer') then
      returnType := 'Integer'
    else if TryToken(ALine,index,'Boolean') then
      returnType := 'Boolean'
    else
      raise exception.Create('Expecting return type (Integer, Boolean)');
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

function ProcessEvent(ALine: TStringList; APlayers: TPlayers): integer;
var
  index: Integer;
  conds: TConditionList;
begin
  index := 0;
  ExpectToken(ALine,index,'When');

  conds := ExpectConditions(ALine,index,APlayers);

  result := CreateEvent(APlayers, conds, not TryToken(ALine,index,'Once'));

  if index < ALine.Count then
    raise exception.Create('End of line expected');
end;

procedure ProcessDim(ALine: TStringList; AProg: TInstructionList; AInit0: boolean);
var
  index: Integer;
  varName, varType, filename, text: String;
  arraySize: integer;
  isArray: boolean;
  timeMs, intVal: integer;
  arrValues: ArrayOfInteger;
  boolVal: boolean;
  prop: TUnitProperties;
  arrBoolValues: ArrayOfSwitchValue;
  Constant: boolean;

  procedure ExpectArraySize;
  begin
    if not TryInteger(ALine,index,arraySize) then
      raise exception.Create('Expecting array size or "]"');
    if (arraySize < 1) or (arraySize > MaxIntArraySize) then
      raise Exception.Create('Array size can go from 1 to ' + inttostr(MaxIntArraySize));
  end;

  procedure SetupIntVar(AIntVar: integer; AExpr : TExpression = nil);
  begin
    with IntVars[AIntVar] do
    begin
      if AExpr <> nil then
      begin
        if AExpr.IsConstant then
        begin
          Value := AExpr.ConstElement;
          AExpr.Free;
        end
        else
        begin
          if Constant then
            raise exception.Create('Expression is not constant');
          AExpr.AddToProgram( AProg, Player, UnitType, simSetTo);
          AExpr.Free;
          exit;
        end;
      end;
      if not Constant and (AInit0 or (Value <> 0)) then
      begin
        if Randomize then
          AProg.Add( TSetIntegerInstruction.Create(Player,UnitType, simRandomize, Value) )
        else
          AProg.Add( TSetIntegerInstruction.Create(Player,UnitType, simSetTo, Value) );
      end;
    end;
  end;

  procedure SetupBoolVar(ABoolVar: integer);
  begin
    with BoolVars[ABoolVar] do
    begin
      if not Constant and (AInit0 or (Value <> svClear)) then
        AProg.Add( TSetSwitchInstruction.Create(Switch, Value) );
    end;
  end;

  procedure SetupIntArray(AIntArray: integer);
  var
    i: Integer;
  begin
    with IntArrays[AIntArray] do
    begin
      if not Constant then
        for i := 1 to Size do
          SetupIntVar(Vars[i-1]);
    end;
  end;

  procedure SetupBoolArray(ABoolArray: integer);
  var
    i: Integer;
  begin
    with BoolArrays[ABoolArray] do
    begin
      if not Constant then
        for i := 1 to Size do
          SetupBoolVar(Vars[i-1]);
    end;
  end;


begin
  index := 0;
  if TryToken(ALine,index,'Const') then
      Constant := true
    else
    begin
      Constant := false;
    ExpectToken(ALine,index,'Dim');
    end;

    index := 1;
  while index < ALine.Count do
    begin
    if index > 1 then ExpectToken(ALine,index,',');

      isArray:= false;
    varName := ALine[index];
      arraySize := 0;
      if not IsValidVariableName(varName) then
        raise exception.Create('Invalid variable name');

      inc(index);

    if TryToken(ALine,index,'(') then
      begin
        isArray:= true;
      if not TryToken(ALine,index,')') then
        begin
          ExpectArraySize;
        ExpectToken(ALine,index,')');
        end;
      end;

    if TryToken(ALine,index,'As') then
      begin
      if index >= ALine.Count then
          raise Exception.Create('Expecting variable type');

      if TryToken(ALine,index,'Integer') then varType := 'Integer'
      else if TryToken(ALine,index,'Boolean') then varType := 'Boolean'
      else if TryToken(ALine,index,'String') then varType := 'String'
      else if TryToken(ALine,index,'UnitProperties') then varType := 'UnitProperties'
      else if TryToken(ALine,index,'Sound') then varType := 'Sound'
      else raise Exception.Create('Unknown type : ' + ALine[index]);

      if not isArray and (varType <> 'UnitProperties') and TryToken(ALine,index,'(') then
        begin
          isArray := true;
        if not TryToken(ALine,index,')') then
          begin
            ExpectArraySize;
          ExpectToken(ALine,index,')');
          end;
        end;
      end else
        varType := '?';

      if IsVarNameUsed(varName, integer(isArray)) then
        raise exception.Create('This name is already in use');

    if TryToken(ALine,index,'=') then
      begin
        if isArray then
        begin
          if varType = '?' then
          begin
            raise exception.Create('Array type not specified');
          end else
          if varType = 'Integer' then
          begin
          arrValues := ParseIntArray(ALine,index);
            if (arraySize <> 0) and (length(arrValues) <> arraySize) then
              raise exception.Create('Array size mismatch');
            if arraySize = 0 then
            begin
              if (length(arrValues) < 1) or (length(arrValues) > MaxIntArraySize) then
                raise exception.Create('Integer array size can go from 1 to ' + inttostr(MaxIntArraySize));
              arraySize:= length(arrValues);
            end;
            SetupIntArray(CreateIntArray(varName, arraySize, arrValues, Constant));
          end else if varType = 'Boolean' then
          begin
          arrBoolValues := ParseBoolArray(ALine,index);
            if (arraySize <> 0) and (length(arrValues) <> arraySize) then
              raise exception.Create('Array size mismatch');
            if arraySize = 0 then
            begin
              if (length(arrBoolValues) < 1) or (length(arrBoolValues) > MaxBoolArraySize) then
                raise exception.Create('Boolean array size can go from 1 to ' + inttostr(MaxBoolArraySize));
              arraySize:= length(arrBoolValues);
            end;
            SetupBoolArray(CreateBoolArray(varName, arraySize, arrBoolValues, Constant));
          end else
            raise exception.Create(varType+' arrays not supported');
        end else
        if varType = 'Sound' then
        begin
        ExpectToken(ALine,index,'{');
          filename := '';
          timeMs := -1;
        if not TryToken(ALine,index,'}') then
          while true do
          begin
          ExpectToken(ALine,index,'.');
          if TryToken(ALine,index,'Filename') then
            begin
            ExpectToken(ALine,index,'=');
            filename := ExpectString(ALine,index);
            end else
          if TryToken(ALine,index,'Duration') then
            begin
            ExpectToken(ALine,index,'=');
            timeMs := ExpectInteger(ALine,index);
            end else
              raise exception.Create('Unknown field. Expecting Filename or Duration');
          if not TryToken(ALine,index,',') then
            begin
            ExpectToken(ALine,index,'}');
              break;
            end;
          end;
          if filename = '' then raise exception.Create('Filename not specified');
          if timeMs = -1 then raise exception.Create('Duration not specified');
          CreateSound(varName, filename, timeMs, Constant);
        end else
        if varType = 'UnitProperties' then
        begin
        if TryUnitProperties(ALine,index,prop) then
          begin
            CreateUnitProp(varName, prop, Constant);
          end;
        end else
        if varType = 'String' then
        begin
        CreateString(varName,ExpectString(ALine,index), Constant);
        end else
        if varType = 'Boolean' then
        begin
        if TryToken(ALine,index,'Rnd') then
        begin
          if TryToken(ALine,index,'(') then ExpectToken(ALine,index,')');
          SetupBoolVar(CreateBoolVar(varName, svRandomize, Constant));
        end
          else
        if TryBoolean(ALine,index,boolVal) then
            SetupBoolVar(CreateBoolVar(varName, BoolToSwitch[boolVal], Constant))
          else
            raise exception.Create('Expecting boolean constant');
        end else
        if varType = 'Integer' then
        begin
        SetupIntVar(CreateIntVar(varName, 0, true, Constant), TryExpression(ALine,index,true));
        end else
        begin
          if varType <> '?' then raise exception.Create('Unhandled case');

        if TryBoolean(ALine,index,boolVal) then
            SetupBoolVar(CreateBoolVar(varName, BoolToSwitch[boolVal], Constant))
          else
        if TryInteger(ALine,index,intVal) then
            SetupIntVar(CreateIntVar(varName, intVal, false, Constant))
        else if TryToken(ALine,index,'Rnd') then
            raise exception.Create('Cannot determine if integer or boolean')
          else
        if TryString(ALine,index,text) then
            CreateString(varName,text, Constant)
          else
            raise exception.Create('Expecting constant value');
        end;
      end else
      begin
        if varType = '?' then
          raise Exception.Create('Variable type not specified');

        if isArray then
        begin
          if arraySize= 0 then
            raise exception.Create('Array size not specified');
          if varType = 'Boolean' then
            SetupBoolArray(CreateBoolArray(varName, arraySize, [], Constant))
          else if varType = 'Integer' then
            SetupIntArray(CreateIntArray(varName, arraySize, [], Constant))
          else raise Exception.Create(varType+' arrays not supported')
        end else
        begin
          if varType = 'Boolean' then
            SetupBoolVar(CreateBoolVar(varName, svClear, Constant))
          else if VarType = 'Integer' then
            SetupIntVar(CreateIntVar(varName, 0, false, Constant))
          else raise exception.Create('Initial value needed for '+varType);
        end;
      end;
    end;
end;

procedure ProcessDim(ADeclaration: string; AProg: TInstructionList; AInit0: boolean);
var
  line: TStringList;
begin
  line := ParseLine(ADeclaration);
  try
    ProcessDim(line, AProg, AInit0);
  finally
    line.Free;
  end;
end;

procedure AppendConditionalInstruction(AProg: TInstructionList; AConditions: TConditionList; AIfTrue, AIfFalse: TInstruction);
begin
  if (AConditions.Count = 1) and (AConditions[0] is TAlwaysCondition) then
  begin
    AProg.Add(AIfTrue);
    AConditions[0].Free;
    AConditions.Free;
    AIfFalse.Free;
  end else
  if (AConditions.Count = 1) and (AConditions[0] is TNeverCondition) then
  begin
    AProg.Add(AIfFalse);
    AConditions[0].Free;
    AConditions.Free;
    AIfTrue.Free;
  end else
  begin
    AProg.Add(TIfInstruction.Create(AConditions));
    AProg.Add(AIfTrue);
    AProg.Add(TElseInstruction.Create);
    AProg.Add(AIfFalse);
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

function TryNeutralAction(AProg: TInstructionList; ALine: TStringList; var AIndex: Integer; AThreads: TPlayers): boolean;
var
  scenario: String;
  conds: TConditionList;
begin
  if TryToken(ALine,AIndex,'CountdownPaused') then
  begin
    result := true;
    ExpectToken(ALine,AIndex,'=');
    conds := ExpectConditions(ALine,AIndex,AThreads);
    AppendConditionalInstruction(AProg, conds, TPauseCountdownInstruction.Create(true),
                                        TPauseCountdownInstruction.Create(false));
  end else
  if TryToken(ALine,AIndex,'GamePaused') then  //not really neutral in the sense that each player can have a limited amount of pause
  begin
    result := true;
    ExpectToken(ALine,AIndex,'=');
    conds := ExpectConditions(ALine,AIndex,AThreads);
    AppendConditionalInstruction(AProg, conds, TPauseGameInstruction.Create(true),
                                        TPauseGameInstruction.Create(false));
  end else
  if TryToken(ALine,AIndex,'NextScenario') then
  begin
    ExpectToken(ALine,AIndex,'=');
    scenario := ExpectString(ALine,AIndex);
    AProg.Add(TSetNextScenarioInstruction.Create(scenario));
  end else
    result := false;
end;

function TryPlayerAction(AProg: TInstructionList; ALine: TStringList; var AIndex: Integer; APlayer: TPlayer; AThreads: TPlayers): boolean;
var
  intVal, propIndex, propVal, timeMs, varIdx, tempInt, i: integer;
  unitType, locStr, destLocStr, orderStr, filename, text: String;
  boolVal, textDefined: boolean;
  destPl: TPlayer;
  props: TUnitProperties;
  prop: TSetUnitProperty;
  alliance: TAllianceStatus;
  pl: TPlayer;
  conds: TConditionList;
  expr: TExpression;
  subInstr: TInstructionList;

  procedure CheckCurrentPlayer;
  begin
    if APlayer <> plCurrentPlayer then
      raise exception.Create('This action can only be done with the current player "Me"');
  end;

  function ParseOptionalQuantity: integer;
  begin
    if TryToken(ALine,AIndex,'All') then
    begin
      result := -1;
      ExpectToken(ALine,AIndex,',');
    end else
    if TryInteger(ALine,AIndex,result) then
    begin
      ExpectToken(ALine,AIndex,',');
    end else
      result := -1; //All by default
  end;

begin
  result := true;
  if TryToken(ALine,AIndex,'CreateUnit') then
  begin
    ExpectToken(ALine,AIndex,'(');
    expr := TryExpression(ALine,AIndex,True);
    if expr.IsConstant and (expr.ConstElement < 1) then
    begin
      expr.Free;
      raise exception.Create('Quantity must be at least 1');
    end;
    ExpectToken(ALine,AIndex,',');
    unitType := ExpectString(ALine,AIndex);
    ExpectToken(ALine,AIndex,',');
    locStr := ExpectString(ALine,AIndex);
    if TryToken(ALine,AIndex,',') then
    begin
      propIndex := TryUnitPropVar(ALine,AIndex);
      if propIndex = -1 then
        raise exception.Create('Unit properties expected');
    end else
      propIndex := -1;
    ExpectToken(ALine,AIndex,')');

    if expr.IsConstant then
      AProg.Add(TCreateUnitInstruction.Create(APlayer, expr.ConstElement, unitType, locStr, propIndex))
    else
    begin
      tempInt := AllocateTempInt;
      expr.AddToProgram(AProg, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
      for i := 10 downto 0 do
      begin
        subInstr := TInstructionList.Create;
        subInstr.Add( TCreateUnitInstruction.Create(APlayer, 1 shl i, unitType, locStr, propIndex) );
        subInstr.Add( TSetIntegerInstruction.Create(IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSubtract, 1 shl i) );
        AProg.Add( TFastIfInstruction.Create( [TIntegerCondition.Create( IntVars[tempInt].Player,IntVars[tempInt].UnitType, icmAtLeast, 1 shl i)], subInstr) );
      end;
      ReleaseTempInt(tempInt);
    end;

  end else
  if TryToken(ALine,AIndex,'KillUnit') or TryToken(ALine,AIndex,'RemoveUnit') then
  begin
    boolVal:= upcase(ALine[AIndex-1][1])='K';
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectString(ALine,AIndex);

    if TryToken(ALine,AIndex,')') then
      locStr:= ''
    else
    begin
      ExpectToken(ALine,AIndex,',');
      locStr := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
    end;
    AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, boolVal));
  end else
  if TryToken(ALine,AIndex,'GiveUnit') then
  begin
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectString(ALine,AIndex);
    ExpectToken(ALine,AIndex,',');
    locStr := ExpectString(ALine,AIndex);
    ExpectToken(ALine,AIndex,',');
    destPl := TryParsePlayer(ALine,AIndex);
    if destPl = plNone then raise Exception.Create('Expecting player');
    ExpectToken(ALine,AIndex,')');

    AProg.Add(TGiveUnitInstruction.Create(APlayer, intVal, unitType, locStr, destPl));
  end else
  if TryToken(ALine,AIndex,'Unit') then
  begin
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectString(ALine,AIndex);
    ExpectToken(ALine,AIndex,',');
    locStr := ExpectString(ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    ExpectToken(ALine,AIndex,'.');

    if TryToken(ALine,AIndex,'Properties') then
    begin
      ExpectToken(ALine,AIndex,'=');
      if not TryUnitProperties(ALine,AIndex,props) then
      begin
        if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
        begin
          propIndex:= UnitPropIndexOf(ALine[AIndex]);
          if propIndex = -1 then
            raise exception.Create('Expecting unit properties');

          inc(AIndex);
          props := UnitPropVars[propIndex].Value;
        end else
        raise exception.Create('Expecting unit properties');
      end;

      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supLife, props.Life));
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supShield, props.Shield));
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supEnergy, props.Energy));
      if CompareTexT(unitType, 'Any unit')=0 then
        AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supResource, props.Resource));
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supHangarCount, props.HangarCount));

      if intVal = -1 then
        AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supInvincible, integer(props.Invincible)));

    end else
    if TryToken(ALine,AIndex,'Location') then
    begin
       ExpectToken(ALine,AIndex,'=');
       destLocStr := ExpectString(ALine,AIndex);

       AProg.Add(TTeleportUnitInstruction.Create(APlayer, intVal, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'Teleport') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectString(ALine,AIndex);
       ExpectToken(ALine,AIndex,')');

       AProg.Add(TTeleportUnitInstruction.Create(APlayer, intVal, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'MoveOrder') or TryToken(ALine,AIndex,'PatrolOrder') or TryToken(ALine,AIndex,'AttackOrder') then
    begin
      orderStr := ALine[AIndex-1];
      orderStr := lowercase(copy(orderStr,1,length(orderStr)-5));
       if intVal <> -1 then
         raise exception.Create('Cannot specify quantity for an order (use All quantity instead)');
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectString(ALine,AIndex);
       ExpectToken(ALine,AIndex,')');

       AProg.Add(TOrderUnitInstruction.Create(APlayer, unitType, locStr, destLocStr, orderStr));
    end else
    if TryToken(ALine,AIndex,'Kill') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, true));
    end else
    if TryToken(ALine,AIndex,'Remove') then
    begin
      AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, false));
    end else
    if TryToken(ALine,AIndex,'Give') then
    begin
      ExpectToken(ALine,AIndex,'(');
      destPl:= TryParsePlayer(ALine,AIndex);
      if destPl = plNone then raise exception.Create('Expecting player');
      ExpectToken(ALine,AIndex,')');

      AProg.Add(TGiveUnitInstruction.Create(APlayer, intVal, unitType, locStr, destPl));
    end else
    if TryToken(ALine,AIndex,'ToggleInvincibility') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,'(');

      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supInvincible, -1));
    end else
    if TryToken(ALine,AIndex,'ToggleDoodadState') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,'(');

      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supDoodadState, -1));
    end else
    begin
      if TryToken(ALine,AIndex,'Life') then prop := supLife else
      if TryToken(ALine,AIndex,'Shield') then prop := supShield else
      if TryToken(ALine,AIndex,'Energy') then prop := supEnergy else
      if TryToken(ALine,AIndex,'Resource') then prop := supResource else
      if TryToken(ALine,AIndex,'HangarCount') then prop := supHangarCount else
      if TryToken(ALine,AIndex,'Invincible') then prop := supInvincible else
      if TryToken(ALine,AIndex,'DoodadState') then prop := supDoodadState else
        raise exception.Create('Expecting property name');
      ExpectToken(ALine,AIndex,'=');

      if prop in[supInvincible,supDoodadState] then
      begin
        conds := ExpectConditions(ALine,AIndex,AThreads);
        AppendConditionalInstruction(AProg, conds,
           TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, prop, 1),
           TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, prop, 0));
      end
      else
      begin
        propVal:= ExpectInteger(ALine,AIndex);
        AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, prop, propVal));
      end;
    end;

  end else
  begin
    if TryToken(ALine,AIndex,'CenterView') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      locStr := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TCenterViewInstruction.Create(locStr));
    end else
    if TryToken(ALine,AIndex,'MinimapPing') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      locStr := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TCenterViewInstruction.Create(locStr));
    end else
    if TryToken(ALine,AIndex,'Print') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      text := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TDisplayTextMessageInstruction.Create(true, text));
    end else
    if TryToken(ALine,AIndex,'TalkingPortrait') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      unitType := ExpectString(ALine,AIndex);
      ExpectToken(ALine,AIndex,',');
      timeMs := ExpectInteger(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TTalkingPortraitInstruction.Create(unitType, timeMs));
    end else
    if TryToken(ALine,AIndex,'MissionObjectives') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'=');
      text := ExpectString(ALine,AIndex);
      AProg.Add(TSetMissionObjectivesInstruction.Create(text));
    end else
    if TryToken(ALine,AIndex,'Leaderboard') then
    begin
      ExpectToken(ALine,AIndex,'.');
      if TryToken(ALine,AIndex,'Computers') then
      begin
        ExpectToken(ALine,AIndex,'=');
        conds := ExpectConditions(ALine,AIndex,AThreads);
        AppendConditionalInstruction(AProg, conds,
          TLeaderBoardIncludeComputersInstruction.Create(1),
          TLeaderBoardIncludeComputersInstruction.Create(0));
      end else
      if TryToken(ALine,AIndex,'ToggleComputers') then
      begin
        AProg.Add(TLeaderBoardIncludeComputersInstruction.Create(-1));
      end else
      if TryToken(ALine,AIndex,'Show') then
      begin
        ExpectToken(ALine,AIndex,'(');
        textDefined:= TryString(ALine,AIndex,text);
        if textDefined then ExpectToken(ALine,AIndex,',');

        if TryToken(ALine,AIndex,'MineralsAndGas') or
         TryToken(ALine,AIndex,'OreAndGas') then
        begin
          if not textDefined then
          begin
            intVal := MaxLongInt;
            if TryToken(ALine,AIndex,',') then
            begin
              if not TryInteger(ALine,AIndex,intVal) then
                raise exception.Create('Expecting integer value');
            end;
            AProg.Add(TShowLeaderboardOreAndGasIconInstruction.Create(intVal));
          end else
            AProg.Add(TShowLeaderboardValueInstruction.Create('Minerals and gas','OreAndGas',-1));
        end else
        begin
          if TryInteger(ALine,AIndex,intVal) then
            ExpectToken(ALine,AIndex,'-')
          else intVal := -1;

          if TryToken(ALine,AIndex,'MineralsAndGas') or
            TryToken(ALine,AIndex,'OreAndGas') or
            TryToken(ALine,AIndex,'Gas') or
            TryToken(ALine,AIndex,'Minerals') or
            TryToken(ALine,AIndex,'Ore') then
          begin
            varIdx := IntArrayIndexOf(ALine[AIndex-1]);
            if varIdx = -1 then raise exception.Create('Unable to find variable');
            if not textDefined then text := LowerCase(IntArrays[varIdx].UnitType);
            AProg.Add(TShowLeaderboardValueInstruction.Create(text, IntArrays[varIdx].UnitType, intVal));
          end else
          if TryToken(ALine,AIndex,'TotalScore') or TryToken(ALine,AIndex,'CustomScore') or
            TryToken(ALine,AIndex,'UnitScore') or TryToken(ALine,AIndex,'BuildingScore') or TryToken(ALine,AIndex,'UnitAndBuildingScore')  or
            TryToken(ALine,AIndex,'KillScore') or TryToken(ALine,AIndex,'RazingScore') or TryToken(ALine,AIndex,'KillAndRazingScore') then
          begin
            varIdx := IntArrayIndexOf(ALine[AIndex-1]);
            if varIdx = -1 then raise exception.Create('Unable to find variable');
            if not textDefined then text := LowerCase(IntArrays[varIdx].UnitType);
            AProg.Add(TShowLeaderboardValueInstruction.Create(text, copy(IntArrays[varIdx].UnitType,1,length(IntArrays[varIdx].UnitType)-6), intVal));
          end else
          if TryToken(ALine,AIndex,'KillCount') then
          begin
            if TryToken(ALine,AIndex,'(') then
            begin
              unitType := ExpectString(ALine,AIndex);
              ExpectToken(ALine,AIndex,')');
            end else
            begin
              unitType := 'Any unit';
            end;
            if not textDefined then text := 'kills';
            AProg.Add(TShowLeaderboardKillCountInstruction.Create(text, unitType, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitCount') then
          begin
            if TryToken(ALine,AIndex,'(') then
            begin
              unitType := ExpectString(ALine,AIndex);
              if TryToken(ALine,AIndex,',') then
                locStr := ExpectString(ALine,AIndex)
              else
                locStr := '';
              ExpectToken(ALine,AIndex,')');
            end else
            begin
              unitType := 'Any unit';
              locStr := '';
            end;
            if not textDefined then text := 'units';
            AProg.Add(TShowLeaderboardUnitCountInstruction.Create(text, unitType, locStr, intVal));
          end else
            raise exception.Create('Expecting sorting variable');
        end;
        ExpectToken(ALine,AIndex,')');
      end else
        raise exception.Create('Unknown field of leaderboard (Show, Computers, ToggleComputers)');

    end else
    if TryToken(ALine,AIndex,'Alliance') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      pl := TryParsePlayer(ALine,AIndex);
      if pl = plNone then raise exception.Create('Expecting player identifier');
      ExpectToken(ALine,AIndex,')');
      ExpectToken(ALine,AIndex,'=');
      if TryToken(ALine,AIndex,'Ennemy') then alliance := asEnnemy
      else if TryToken(ALine,AIndex,'Ally') then alliance := asAlly
      else if TryToken(ALine,AIndex,'AlliedVictory') then alliance := asAlliedVictory
      else raise exception.Create('Expecting alliance status (Ennemy, Ally, AlliedVictory)');

      AProg.Add(TSetAllianceStatus.Create(pl, alliance));
    end else
    if TryToken(ALine,AIndex,'NextScenario') then
    begin
      raise exception.Create('Changing scenario cannot be done for a specific player');
    end else
    if TryToken(ALine,AIndex,'RunAIScript') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      filename := ExpectString(ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
        locStr := ExpectString(ALine,AIndex)
      else
        locStr := '';
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TRunAIScriptInstruction.Create(filename, locStr));
    end else
    if TryToken(ALine,AIndex,'Defeat') then
    begin
      CheckCurrentPlayer;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TEndGameInstruction.Create(egDefeat));
    end else
    if TryToken(ALine,AIndex,'Draw') then
    begin
      CheckCurrentPlayer;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TEndGameInstruction.Create(egDraw));
    end else
    if TryToken(ALine,AIndex,'Victory') then
    begin
      CheckCurrentPlayer;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TEndGameInstruction.Create(egVictory));
    end else
    if TryToken(ALine,AIndex,'UnitSpeech') then
    begin
      ExpectToken(ALine,AIndex,'=');
      conds := ExpectConditions(ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
        TUnitSpeechInstruction.Create(true),
        TUnitSpeechInstruction.Create(false));
    end else
      result := false;
  end;
end;

function ParseOption(ALine: TStringList): boolean;
var
  index: Integer;

  procedure CheckEndOfLine;
  begin
    if index <> ALine.Count then
      raise exception.Create('Expecting end of line');
  end;

begin
  index := 0;
  if TryToken(ALine,index,'Option') then
  begin
    if TryToken(ALine,index,'Hyper') then
    begin
      if TryToken(ALine,index,'On') then HyperTriggers:= true
      else If TryToken(ALine,index,'Off') then HyperTriggers:= false
      else raise exception.Create('Expecting On or Off');
    end else
      raise exception.Create('Unknown option');
    CheckEndOfLine;
    exit(true);
  end;
  exit(false);
end;

procedure ParseInstruction(ALine: TStringList; AProg: TInstructionList; AThreads: TPlayers; AProcId: integer);
var
  index, intVal, idxArr, i, idxSound, sw, idxVar: integer;
  params: TStringList;
  name, assignOp: String;
  done, boolVal: boolean;
  scalar: TScalarVariable;
  conds: TConditionList;
  ints: ArrayOfInteger;
  sim: TSetIntegerMode;
  pl: TPlayer;
  expr: TExpression;

  procedure CheckEndOfLine;
  begin
    if index <> ALine.Count then
      raise exception.Create('Expecting end of line');
  end;

begin
  if (ALine.Count = 0) or ((ALine.Count = 1) and (ALine[0] = '')) then exit;

    index := 0;
  if TryToken(ALine,index,'Return') then
    begin
      if AProcId <> -1 then
      begin
        if Procedures[AProcId].ReturnType = 'Integer' then
        begin
        if TryInteger(ALine,index,intVal) then
            AProg.Add( TTransferIntegerInstruction.Create(intVal, itCopyIntoAccumulator) )
          else
          begin
            idxVar := ProcedureReturnVar(AProcId);
          expr := TryExpression(ALine,index,true);
            expr.AddToProgram(AProg, IntVars[idxVar].Player,IntVars[idxVar].UnitType, simSetTo);
            expr.Free;
          end;
        end else
        if Procedures[AProcId].ReturnType = 'Boolean' then
        begin
        if TryBoolean(ALine,index,boolVal) then
          begin
            idxVar := GetBoolResultVar;
            sw := BoolVars[idxVar].Switch;
            AProg.Add( TSetSwitchInstruction.Create(sw, BoolToSwitch[boolVal]) );
          end else
            raise exception.Create('Expecting boolean value');
        end;
      end;
      AProg.Add( TReturnInstruction.Create );
      CheckEndOfLine;
    end
    else
  if TryToken(ALine,index,'EndIf') then
    begin
      CheckEndOfLine;
      AProg.Add(TEndIfInstruction.Create);
    end else
  if TryToken(ALine,index,'End') then
    begin
    if TryToken(ALine,index,'While') then
      begin
        CheckEndOfLine;
        AProg.Add(TEndWhileInstruction.Create);
      end else
    if TryToken(ALine,index,'If') then
      begin
        CheckEndOfLine;
        AProg.Add(TEndIfInstruction.Create);
      end else
        raise exception.Create('Unknown end instruction');
    end else
  if TryToken(ALine,index,'While') then
    begin
    conds := ExpectConditions(ALine,index,AThreads);
      if (conds.Count = 1) and (conds[0] is TAlwaysCondition) then
        raise exception.Create('Infinite loop not allowed. You can use When True instead');
      AProg.Add(TWhileInstruction.Create(conds));
      CheckEndOfLine;
    end else
  if TryToken(ALine,index,'If') then
    begin
    conds := ExpectConditions(ALine,index,AThreads);
      AProg.Add(TIfInstruction.Create(conds));
    ExpectToken(ALine,index,'Then');
      CheckEndOfLine;
    end else
  if TryToken(ALine,index,'Else') then
    begin
      CheckEndOfLine;
      AProg.Add(TElseInstruction.Create);
    end else
    begin
      done := false;
    scalar := TryScalarVariable(ALine,index);
      if scalar.VarType <> svtNone then
      begin
      if TryToken(ALine,index,'=') then
        begin
          done := true;
          if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
          case scalar.VarType of
          svtInteger: begin
            expr := TryExpression(ALine, index, true);
              expr.AddToProgram(AProg, scalar.Player, scalar.UnitType, simSetTo);
              expr.Free;
            end;
          svtSwitch:
            begin
            intVal := ParseRandom(ALine, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                if intVal = 2 then
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Switch, svRandomize))
                else
                  raise exception.Create('Boolean can have only 2 values');
              end else
              begin
              conds := ExpectConditions(ALine,index,AThreads);
                if (conds.Count = 1) and (conds[0] is TSwitchCondition) and
                   (TSwitchCondition(conds[0]).Switch = scalar.Switch) then
                begin
                  //a = Not a
                  if not TSwitchCondition(conds[0]).Value then
                    AProg.Add(TSetSwitchInstruction.Create(scalar.Switch, svToggle));
                  conds[0].Free;
                  conds.Free;
                end else
                  AppendConditionalInstruction(AProg, conds,
                    TSetSwitchInstruction.Create(scalar.Switch, svSet),
                    TSetSwitchInstruction.Create(scalar.Switch, svClear));
              end;
            end;
          else raise exception.Create('Unhandled case');
          end;
        end else
      If TryToken(ALine,index,'+') or TryToken(ALine,index,'-') then
        begin
        assignOp := ALine[index-1];
        if TryToken(ALine,index,'=') then
          begin
            done := true;
            if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
            if scalar.VarType = svtInteger then
            begin
            expr := TryExpression(ALine,index, true);
              if assignOp='+' then
                expr.AddToProgram(AProg, scalar.Player, scalar.UnitType, simAdd)
              else if assignOp='-' then
                expr.AddToProgram(AProg, scalar.Player, scalar.UnitType, simSubtract);
              expr.Free;
            end
            else raise Exception.Create('Integer variables only can be incremented/decremented');
          end;
        end;

      end;

    idxSound := SoundIndexOf(ALine[0]);
      if idxSound <> -1 then
      begin
        index := 1;
      ExpectToken(ALine,index,'.');
      ExpectToken(ALine,index,'Play');
      if TryToken(ALine,index,'(') then ExpectToken(ALine,index,')');

        AProg.Add(TPlayWAVInstruction.Create(SoundVars[idxSound].Filename, SoundVars[idxSound].DurationMs));
        done := true;
      end;

    idxArr := IntArrayIndexOf(ALine[0]);
      if idxArr <> -1 then
      begin
        index := 1;
      If TryToken(ALine,index,'+') or TryToken(ALine,index,'-') then
        assignOp := ALine[index-1] else assignOp := '';

      if TryToken(ALine,index,'=') then
        begin
          done := true;
          if IntArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');

        ints := ParseIntArray(ALine,index);
          if length(ints) <> IntArrays[idxArr].Size then
            raise exception.Create('Array size mismatch');
          CheckEndOfLine;
          if assignOp = '+' then sim := simAdd
          else if assignOp = '-' then sim := simSubtract
          else sim := simSetTo;
          for i := 0 to high(ints) do
            with IntVars[IntArrays[idxArr].Vars[i]] do
            AProg.Add(TSetIntegerInstruction.Create(Player, UnitType, sim, ints[i]));
        end;
      end;

      if not done then
      begin
        index := 0;
      pl := TryParsePlayer(ALine,index);
        if pl <> plNone then
        begin
          done := true;

        ExpectToken(ALine,index,'.');
        if index >= ALine.Count then
            raise exception.Create('Expecting action but end of line found');

        if not TryPlayerAction(AProg,ALine,index,pl, AThreads) then
          raise exception.Create('Expecting action but "' + ALine[index] + '" found');
          CheckEndOfLine;
        end;
      end;

      index := 0;
    if TryNeutralAction(AProg,ALine,index, AThreads) then
      begin
        CheckEndOfLine;
        done := true;
      end;

    if TryPlayerAction(AProg,ALine,index,plCurrentPlayer, AThreads) then
      begin
        if AThreads = [plCurrentPlayer] then
          raise exception.Create('You need to specify which players does the action');
        CheckEndOfLine;
        done := true;
      end;


      if not done then
      begin
        index := 0;
        name := '';
      while (index < ALine.Count) and IsValidVariableName(ALine[index]) do
        begin
          if index > 0 then name += ' ';
        name += ALine[index];
          inc(index);
        end;

      if (index = ALine.Count) or
        ((index < ALine.Count) and (ALine[index] = '(')) then
        begin
          params := TStringList.Create;
        if TryToken(ALine,index,'(') then
          begin
          while not TryToken(ALine,index,')') do
            begin
            if params.Count > 0 then ExpectToken(ALine,index,',');
            if TryToken(ALine,index,',') then
                raise exception.Create('Empty parameters not allowed');
            params.Add(ALine[index]);
              inc(index);
            end;
          end;
          CheckEndOfLine;

          if CompareText(name,'Wait')=0 then
          begin
            if params.Count <> 1 then
              raise exception.Create('Procedure takes only one parameter');
            AProg.Add(TWaitInstruction.Create(StrToInt(params[0])));
          end
          else
            AProg.Add(TCallInstruction.Create(name, params));
          params.Free;
        end else
        if scalar.VarType = svtNone then
          raise exception.Create('Unknown instruction "' + name + '"')
        else
          raise exception.Create('Expecting assignment');
      end;
    end;
end;

function RemoveTrailingCommentAndTabs(AText: string): string;
var inStr: boolean;
  i: Integer;
begin
  inStr := false;
  for i := 1 to length(AText) do
  begin
    if AText[i] = '"' then inStr := not inStr;
    if (AText[i] = '''') and not inStr then
      exit(copy(AText,1,i-1));
    if (AText[i] in[#0..#31]) and not inStr then AText[i] := ' ';
  end;
  exit(AText);
end;

function ReadProg(AFilename: string; out AMainThread: TPlayer): boolean;
var
  t: TextFile;
  line: TStringList;
  index: integer;
  lineNumber: integer;

  procedure ReadNextLine;
  var s, lastToken: string;
    extraLine: TStringList;
  begin
    ReadLn(t, s);
    inc(lineNumber);
    if Assigned(line) then FreeAndNil(line);
    line := ParseLine(s);
    index := 0;
    while not Eof(t) and (line.Count > 0) do
    begin
      lastToken := line[line.Count-1];
      if (lastToken = '&') or (lastToken = '+') or (lastToken = '-') or (lastToken = '*') or (lastToken = '\')
       or (CompareText(lastToken,'Or')=0) or (CompareText(lastToken,'And')=0) or (CompareText(lastToken,'Xor')=0) then
      begin
        readln(t, s);
        inc(lineNumber);
        extraLine := ParseLine(s);
        line.AddStrings(extraLine);
        extraLine.Free;
      end else break;
    end;
  end;
var
  errorCount: integer;
  inSub, inEvent, i: integer;
  inSubNew, subNewDeclared, done: boolean;
  players: TPlayers;
  pl: TPlayer;


begin
  AMainThread := plNone;

  HyperTriggers := false;
  InitVariables;
  MainProg.Clear;

  PredefineIntArray('Ore','ore');
  PredefineIntArray('Minerals','ore');
  PredefineIntArray('Gas','gas');
  PredefineIntArray('OreAndGas','ore and gas');
  PredefineIntArray('MineralsAndGas','ore and gas');
  PredefineIntArray('UnitScore','Units Score');
  PredefineIntArray('BuildingScore','Buildings Score');
  PredefineIntArray('UnitAndBuildingScore','Units and buildings Score');
  PredefineIntArray('KillScore','Kills Score');
  PredefineIntArray('RazingScore','Razings Score');
  PredefineIntArray('KillAndRazingScore','Kills and razings Score');
  PredefineIntArray('CustomScore','Custom Score');
  PredefineIntArray('TotalScore','Total Score');
  PredefineIntVar('Countdown', plNone, 'Countdown');
  ProcessDim('Const vbCr = Chr(13)',MainProg, False);
  ProcessDim('Const vbLf = Chr(10)',MainProg, False);
  ProcessDim('Const vbTab = Chr(9)',MainProg, False);
  ProcessDim('Const vbCrLf = vbCr & vbLf',MainProg, False);

  AssignFile(t, AFilename);
  Reset(t);
  lineNumber:= 0;
  line := nil;

  errorCount := 0;
  inSub := -1;
  inEvent := -1;
  inSubNew := false;
  subNewDeclared := false;

  while not Eof(t) and (errorCount < 3) do
  begin
    ReadNextLine;
    if line.Count = 0 then continue;

    try
      if TryToken(line,index,'Dim') or TryToken(line,index,'Const') then
      begin
        if inEvent <> -1 then
          ProcessDim(line, Events[inEvent].Instructions, true)
        else if inSub <> -1 then
          ProcessDim(line, Procedures[inSub].Instructions, true)
        else
          ProcessDim(line, MainProg, false);
      end
      else if TryToken(line,index, 'Sub') or TryToken(line,index,'Function') then
      begin
        if (inSub<>-1) or (inEvent <> -1) or inSubNew then
          raise exception.Create('Nested procedures not allowed');
        inSub := ProcessSub(line);
        if inSub = -1 then
        begin
          if subNewDeclared then raise exception.Create('Sub New already declared');
          inSubNew:= true;
          subNewDeclared := true;
        end;
      end
      else if (inEvent <> -1) and TryToken(line,index, 'Return') then
      begin
        if not Events[inEvent].Preserve then
          raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
        Events[inEvent].Instructions.Add(TReturnInstruction.Create);
      end
      else if TryToken(line,index, 'Stop') then
      begin
        if inEvent <> -1 then
        begin
          if Events[inEvent].Preserve then
          begin
            Events[inEvent].Preserve := false;
            for i := 0 to Events[inEvent].Instructions.Count-1 do
              if Events[inEvent].Instructions[i] is TReturnInstruction then
                raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
          end;
          Events[inEvent].Instructions.Add(TReturnInstruction.Create);
        end else
          raise exception.Create('Stop instruction only allowed in event');
      end
      else if (inSub = -1) and (inEvent = -1) and not inSubNew and TryToken(line,index,'As') then
      begin
        players := ParseAs(line);
        if eof(t) then raise exception.Create('End of file not expected');
        ReadNextLine;
        if TryToken(line,index,'Sub') or TryToken(line,index,'Function') then
        begin
          inSub := ProcessSub(line);
          if inSub <> -1 then
            raise exception.Create('You cannot specify the player for a sub or function except for Sub New');
          if subNewDeclared then raise exception.Create('Sub New already declared');
          inSubNew := true;
          subNewDeclared:= true;
          if not IsUniquePlayer(players) or (players = [plCurrentPlayer]) then
            raise exception.Create('If you specify a player for Sub New, it must be one specific player');
          for pl := plPlayer1 to plPlayer8 do
            if pl in players then AMainThread:= pl;
          if AMainThread = plNone then raise exception.Create('This player can''t be used as a main thread');
        end else
          inEvent := ProcessEvent(line, players)
      end
      else if TryToken(line,index,'When') then
      begin
        if (inSub<>-1) or (inEvent <> -1) or inSubNew then
          raise exception.Create('Nested events not allowed');
        if not subNewDeclared then
          raise exception.Create('Events on main thread must appear after Sub New');
        inEvent := ProcessEvent(line, [AMainThread])
      end else
      begin
        done := false;
        if TryToken(line,index,'End') then
        begin
          if TryToken(line,index,'Sub') then
      begin
        if inSubNew then inSubNew := false
        else
        begin
          if inSub= -1 then
            raise Exception.create('Not in a procedure');
          if Procedures[inSub].ReturnType <> 'Void' then raise exception.Create('Expecting "End Function"');
          with Procedures[inSub].Instructions do
            if (Count = 0) or not (Items[Count-1] is TReturnInstruction) then
              Add(TReturnInstruction.Create);
          inSub := -1;
        end;
            done := true;
      end
          else if TryToken(line,index,'Function') then
      begin
        if inSub= -1 then
          raise Exception.create('Not in a function');
        if Procedures[inSub].ReturnType = 'Void' then raise exception.Create('Expecting "End Sub" instead');
        with Procedures[inSub].Instructions do
          if (Count = 0) or not (Items[Count-1] is TReturnInstruction) then
            Add(TReturnInstruction.Create);
        inSub := -1;
            done := true;
      end
          else if TryToken(line,index,'When') then
      begin
        if inEvent= -1 then
          raise Exception.create('Not in an event');
        with Events[inEvent] do
          while Instructions[Instructions.Count-1] is TReturnInstruction do
          begin
            Instructions[Instructions.Count-1].Free;
            Instructions.Delete(Instructions.Count-1);
          end;
        inEvent := -1;
            done := true;
          end;
        end;

        if not done then
      begin
        if inSub<>-1 then
            ParseInstruction(line, Procedures[inSub].Instructions,[plAllPlayers], inSub)
        else if inEvent<>-1 then
            ParseInstruction(line, Events[inEvent].Instructions, Events[inEvent].Players, -1)
        else if inSubNew then
        begin
          if AMainThread = plNone then
              ParseInstruction(line, MainProg, [plCurrentPlayer], -1)
          else
              ParseInstruction(line, MainProg, [AMainThread], -1);
        end
        else
            if not ParseOption(line) then
            raise exception.Create('Unexpected instruction. Please put initialization code into Sub New.');
      end;
      end;
    except
      on ex:Exception do
      begin
        writeln('Line ', lineNumber, ': ', ex.Message);
        errorCount += 1;
      end;
    end;
  end;

  line.Free;
  CloseFile(t);

  result := errorCount = 0;
end;

var i,j: integer;

initialization

  MainProg := TInstructionList.Create;

finalization

  for i := 0 to MainProg.Count-1 do
    MainProg[i].Free;

  MainProg.Free;

  for i := 0 to ProcedureCount-1 do
    with Procedures[i] do
    begin
      for j := 0 to Instructions.Count-1 do
        Instructions[j].Free;
      Instructions.Free;
      Calls.Free;
    end;

  for i := 0 to EventCount-1 do
    with Events[i] do
    begin
      for j := 0 to Instructions.Count-1 do
        Instructions[j].Free;
      Instructions.Free;
      Conditions.FreeAll;
    end;

end.

