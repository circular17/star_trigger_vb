unit ureadprog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl, usctypes;

function ReadProg(AFilename: string): boolean;

type
  TIntegerList = specialize TFPGList<Integer>;

var
  MainProg: TInstructionList;
  Procedures: array of record
    Name: string;
    ParamCount: integer;
    Instructions: TInstructionList;
    StartIP: integer;
    Done: boolean;
    Calls: TIntegerList;
  end;
  ProcedureCount: integer;

function CreateProcedure(AName: string; AParamCount: integer): integer;
function ProcedureIndexOf(AName: string; AParamCount: integer): integer;

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

uses uparsevb, uvariables;

function ExpectString(ALine: TStringList; var AIndex: integer): string; forward;

function TryInteger(ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
var errPos, idxVar: integer;
  s: String;
begin
  AValue := 0;
  if AIndex < ALine.Count then
  begin
    val(ALine[AIndex], AValue, errPos);
    if errPos = 0 then
    begin
      inc(AIndex);
      exit(true);
    end else
    begin
      if TryToken(ALine,AIndex,'Asc') then
      begin
        ExpectToken(ALine,AIndex,'(');
        s := ExpectString(ALine,AIndex);
        ExpectToken(ALine,AIndex,')');
        if s = '' then
          AValue := 0
        else
          AValue := ord(s[1]);
        exit(true)
      end;

      idxVar := IntVarIndexOf(ALine[AIndex]);
      if (idxVar<>-1) and IntVars[idxVar].Constant then
      begin
        AValue := IntVars[idxVar].Value;
        inc(AIndex);
        exit(true);
      end;
      exit(false);
    end;
  end
  else exit(false);
end;

function TryBoolean(ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
var
  idxVar: Integer;
begin
  AValue := false;
  if TryToken(ALine,AIndex,'False') then exit(true)
  else if TryToken(ALine,AIndex,'True') then
  begin
    AValue := true;
    exit(true)
  end else
  begin
    if AIndex < ALine.Count then
    begin
      idxVar := BoolVarIndexOf(ALine[AIndex]);
      if (idxVar<>-1) and BoolVars[idxVar].Constant then
      begin
        AValue := (BoolVars[idxVar].Value = svSet);
        inc(AIndex);
        exit(true);
      end;
    end;
    exit(false);
  end;
end;

function ExpectInteger(ALine: TStringList; var AIndex: integer): integer;
begin
  result := 0;
  if not TryInteger(ALine,AIndex,result) then
  begin
    if AIndex > ALine.Count then
      raise exception.Create('Integer expected but end of line found') else
      raise exception.Create('Integer expected but "' + ALine[AIndex] + '" found');
  end;
end;

type
  TScalarVariableType = (svtNone, svtInteger, svtSwitch);
  TScalarVariable = record
    VarType: TScalarVariableType;
    Name: string;
    Index: integer;
    Player: TPlayer;
    Constant: boolean;
    IntValue: integer;
    BoolValue: boolean;
  end;

function TryScalarVariable(ALine: TStringList; var AIndex: integer): TScalarVariable;
var varIdx, arrayIndex, idx: integer;
  pl: TPlayer;
  unitType: String;
begin
  result.VarType := svtNone;
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
  begin
    varIdx := IntVarIndexOf(ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
      result.VarType := svtInteger;
      result.Player := IntVars[varIdx].Player;
      result.Name := IntVars[varIdx].UnitType;
      result.Index := -1;
      result.Constant:= IntVars[varIdx].Constant;
      result.IntValue:= IntVars[varIdx].Value;
      result.BoolValue:= IntVars[varIdx].Value<>0;
    end else
    begin
      varIdx := BoolVarIndexOf(ALine[AIndex]);
      if varIdx <> -1 then
      begin
        inc(AIndex);
        result.VarType := svtSwitch;
        result.Player := plNone;
        result.Name := '';
        result.Index := BoolVars[varIdx].Switch;
        result.Constant:= BoolVars[varIdx].Constant;
        result.IntValue := integer(BoolVars[varIdx].Value = svSet);
        result.BoolValue:= BoolVars[varIdx].Value = svSet;
      end else
      begin
        varIdx := IntArrayIndexOf(ALine[AIndex]);
        if varIdx <> -1 then
        begin
          Inc(AIndex);
          if TryToken(ALine,AIndex,'(') then
          begin
            if TryToken(ALine,AIndex,'Me') then
            begin
              pl := plCurrentPlayer;
              if IntArrays[varIdx].Constant then
                raise exception.Create('Cannot access a constant via Me');
              result.IntValue:= 0;
            end
            else
            begin
              arrayIndex := ExpectInteger(ALine, AIndex);
              if (arrayIndex < 1) or (arrayIndex > IntArrays[varIdx].Size) then
                raise exception.Create('Array index out of bounds');
              pl := IntToPlayer(arrayIndex);
              result.IntValue:= IntArrays[varIdx].Values[arrayIndex];
            end;
            ExpectToken(ALine, AIndex, ')');
            result.VarType := svtInteger;
            result.Player := pl;
            result.Name := IntArrays[varIdx].UnitType;
            result.Index := -1;
            result.Constant:= IntArrays[varIdx].Constant;
            result.BoolValue:= result.IntValue<>0;
          end else
          begin
            Dec(AIndex);
          end;
        end else
        begin
          idx := AIndex;
          pl := TryParsePlayer(ALine,idx);
          if pl <> plNone then
          begin
            if TryToken(ALine,idx,'.') then
            begin
              if TryToken(ALine,idx,'DeathCount') then
              begin
                if TryToken(ALine,idx,'(') then
                begin
                  unitType := ExpectString(ALine,idx);
                  ExpectToken(ALine,idx,')');
                end else
                  unitType := 'Any unit';

                result.VarType := svtInteger;
                result.Player := pl;
                result.Name := unitType;
                result.Index := -1;
                result.Constant:= False;
                result.IntValue:= 0;
                result.BoolValue:= false;

                AIndex := idx;
              end else
              if idx < ALine.Count then
              begin
                varIdx := IntArrayIndexOf(ALine[idx]);
                if (varIdx <> -1) and IntArrays[varIdx].Predefined then
                begin
                  inc(idx);

                  result.VarType := svtInteger;
                  result.Player := pl;
                  result.Name := IntArrays[varIdx].UnitType;
                  result.Index := -1;
                  result.Constant:= False;
                  result.IntValue:= 0;
                  result.BoolValue:= false;

                  AIndex := idx;
                end;
              end;

            end;
          end;
        end;
      end;
    end;
  end;
end;

function TryString(ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
var
  scalar: TScalarVariable;
  idxVar, intVal: Integer;
  boolVal: boolean;
  idx: integer;
  firstElem: boolean;
begin
  idx := AIndex;
  AStr := '';
  firstElem := true;
  repeat
    if idx >= ALine.Count then
    begin
      if ARaiseException then raise exception.Create('Expecting string but end of line found');
      exit(false);
    end;

    if TryToken(ALine,idx,'Chr') then
    begin
      ExpectToken(ALine,idx,'(');
      intVal := ExpectInteger(ALine,idx);
      ExpectToken(ALine,idx,')');
      AStr += chr(intVal);
    end else
    if copy(ALine[idx],1,1) = '"' then
    begin
      AStr += RemoveQuotes(ALine[idx]);
      Inc(idx);
    end else
    if TryInteger(ALine,idx,intVal) then
    begin
      AStr += inttostr(intVal);
      if firstElem then
      begin
        if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    if TryBoolean(ALine,idx,boolVal) then
    begin
      AStr += BoolToStr(boolVal, 'True', 'False');
      if firstElem then
      begin
        if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    begin
     idxVar := StringIndexOf(ALine[idx]);
     if idxVar <> -1 then
     begin
       AStr += StringVars[idxVar].Value;
       inc(idx);
     end else
     begin
       scalar := TryScalarVariable(ALine,idx);
       if scalar.VarType <> svtNone then
       begin
         if not scalar.Constant then
         begin
           if ARaiseException then raise exception.Create('Only constants can be used in a string');
           exit(false);
         end;

         case scalar.VarType of
         svtInteger: AStr += inttostr(scalar.IntValue);
         svtSwitch: AStr += BoolToStr(scalar.BoolValue, 'True', 'False');
         else raise exception.Create('Unhandled case');
         end;

         if firstElem then
         begin
           if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
           firstElem := false;
           continue;
         end;
       end else
       begin
         if ARaiseException then raise exception.Create('Expecting string but "' + ALine[idx] + '" found');
         exit(false);
       end;
     end;
    end;
    firstElem := false;
  until not TryToken(ALine,idx,'&');
  AIndex := idx;
  result := true;
end;

function ExpectString(ALine: TStringList; var AIndex: integer): string;
begin
  TryString(ALine,AIndex,result,True);
end;

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

    scalar := TryScalarVariable(ALine,AIndex);
    if scalar.VarType = svtNone then
    begin
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
            result := TCompareIntegerCondition.Create(scalar.Name,False);
          coGreaterThanOrEqual:
            result := TAlwaysCondition.Create;
          coGreaterThan,coNotEqual:
            result := TNotCondition.Create([TCompareIntegerCondition.Create(scalar.Name,False)]);
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
            result := TCompareIntegerCondition.Create(scalar.Name,true);
          coLowerThanOrEqual:
            result := TAlwaysCondition.Create;
          coLowerThan,coNotEqual:
            result := TNotCondition.Create([TCompareIntegerCondition.Create(scalar.Name,true)]);
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
          coEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmExactly, intVal);
          coGreaterThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtLeast, intVal);
          coLowerThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtMost, intVal);
          coGreaterThan: if intVal = maxLongint then
                            result := TNeverCondition.Create
                         else
                            result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtLeast, intVal+1);
          coLowerThan: if intVal = 0 then
                            result := TNeverCondition.Create
                         else
                            result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtMost, intVal-1);
          coNotEqual: result := TNotCondition.Create([TIntegerCondition.Create(scalar.Player, scalar.Name, icmExactly, intVal)]);
          else
            raise exception.Create('Unhandled case');
          end;
        end;
      end;
    end;
    svtSwitch:
      result := TSwitchCondition.Create(scalar.Index, not boolNot)
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

function CreateProcedure(AName: string; AParamCount: integer): integer;
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
    Done := false;
    Calls := TIntegerList.Create;
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
  setlength(result, MaxArraySize);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count = MaxArraySize then
      raise exception.Create('Array can contain at most ' + inttostr(MaxArraySize) + ' values');

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if not TryInteger(ALine, AIndex, result[count]) then
      raise exception.Create('Expecting integer or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ProcessSub(ADeclaration: string): integer;
var
  line: TStringList;
  index: Integer;
  name: String;
begin
  line := ParseLine(ADeclaration);
  index := 0;
  ExpectToken(line,index,'Sub');

  name := line[index];
  if not IsValidVariableName(name) then
    raise exception.Create('Invalid procedure name');
  inc(index);

  if TryToken(line,index,'(') then
  begin
    ExpectToken(line,index,')');
  end;

  result := CreateProcedure(name,0);

  if index <> line.Count then
    raise exception.Create('End of line expected');
end;

function ProcessEvent(ADeclaration: string; APlayers: TPlayers): integer;
var
  line: TStringList;
  index: Integer;
  conds: TConditionList;
begin
  line := ParseLine(ADeclaration);
  index := 0;
  ExpectToken(line,index,'When');

  conds := ExpectConditions(line,index,APlayers);

  result := CreateEvent(APlayers, conds, not TryToken(line,index,'Once'));

  if index < line.Count then
    raise exception.Create('End of line expected');
end;

procedure ProcessDim(ADeclaration: string; AConstant: boolean);
var line: TStringList;
  index: Integer;
  varName, varType, filename, text: String;
  varValue, arraySize: integer;
  isArray: boolean;
  rndVal, timeMs: integer;
  arrValues: ArrayOfInteger;
  boolVal: boolean;
  prop: TUnitProperties;

  procedure ExpectArraySize;
  begin
    if not TryInteger(line,index,arraySize) then
      raise exception.Create('Expecting array size or "]"');
    if (arraySize < 1) or (arraySize > MaxArraySize) then
      raise Exception.Create('Array size can go from 1 to ' + inttostr(MaxArraySize));
  end;

begin
  line := ParseLine(ADeclaration);
  index := 0;
  try
    if TryToken(line,index,'Const') then
      AConstant := true
    else
    begin
      AConstant := false;
      ExpectToken(line,index,'Dim');
    end;

    index := 1;
    while index < line.Count do
    begin
      if index > 1 then ExpectToken(line,index,',');

      isArray:= false;
      varName := line[index];
      varValue := 0;
      arraySize := 0;
      if not IsValidVariableName(varName) then
        raise exception.Create('Invalid variable name');

      inc(index);

      if TryToken(line,index,'(') then
      begin
        isArray:= true;
        if not TryToken(line,index,')') then
        begin
          ExpectArraySize;
          ExpectToken(line,index,')');
        end;
      end;

      if TryToken(line,index,'As') then
      begin
        if index >= line.Count then
          raise Exception.Create('Expecting variable type');

        if TryToken(line,index,'Integer') then varType := 'Integer'
        else if TryToken(line,index,'Boolean') then varType := 'Boolean'
        else if TryToken(line,index,'String') then varType := 'String'
        else if TryToken(line,index,'UnitProperties') then varType := 'UnitProperties'
        else if TryToken(line,index,'Sound') then varType := 'Sound'
        else raise Exception.Create('Unknown type : ' + line[index]);

        if not isArray and (varType <> 'UnitProperties') and TryToken(line,index,'(') then
        begin
          isArray := true;
          if not TryToken(line,index,')') then
          begin
            ExpectArraySize;
            ExpectToken(line,index,')');
          end;
        end;
      end else
        varType := '?';

      if IsVarNameUsed(varName, integer(isArray)) then
        raise exception.Create('This name is already in use');

      if TryToken(line,index,'=') then
      begin
        if isArray then
        begin
          arrValues := ParseIntArray(line,index);
          if (arraySize <> 0) and (length(arrValues) <> arraySize) then
            raise exception.Create('Array size mismatch');
          if arraySize = 0 then
          begin
            if (length(arrValues) < 1) or (length(arrValues) > MaxArraySize) then
              raise exception.Create('Array size can go from 1 to ' + inttostr(MaxArraySize));
            arraySize:= length(arrValues);
          end;
          CreateIntArray(varName, arraySize, arrValues, AConstant);
        end else
        if varType = 'Sound' then
        begin
          ExpectToken(line,index,'{');
          filename := '';
          timeMs := -1;
          if not TryToken(line,index,'}') then
          while true do
          begin
            ExpectToken(line,index,'.');
            if TryToken(line,index,'Filename') then
            begin
              ExpectToken(line,index,'=');
              filename := ExpectString(line,index);
            end else
            if TryToken(line,index,'Duration') then
            begin
              ExpectToken(line,index,'=');
              timeMs := ExpectInteger(line,index);
            end else
              raise exception.Create('Unknown field. Expecting Filename or Duration');
            if not TryToken(line,index,',') then
            begin
              ExpectToken(line,index,'}');
              break;
            end;
          end;
          if filename = '' then raise exception.Create('Filename not specified');
          if timeMs = -1 then raise exception.Create('Duration not specified');
          CreateSound(varName, filename, timeMs, AConstant);
        end else
        if varType = 'UnitProperties' then
        begin
          if TryUnitProperties(line,index,prop) then
          begin
            CreateUnitProp(varName, prop, AConstant);
          end;
        end else
        if varType = 'String' then
        begin
          CreateString(varName,ExpectString(line,index), AConstant);
        end else
        begin
          if TryBoolean(line,index,boolVal) then
          begin
            if varType = '?' then varType := 'Boolean';
            if varType <> 'Boolean' then
              raise Exception.Create('Value is not of expected type');
            CreateBoolVar(varName, BoolToSwitch[boolVal], AConstant);
          end
          else
          if TryString(line,index,text) then
          begin
            CreateString(varName,text, AConstant);
          end else
          begin
            rndVal := ParseRandom(line, index);
            if rndVal > 0 then
            begin
              if varType = 'Boolean' then
              begin
                if rndVal <> 2 then raise Exception.Create('Boolean can have only 2 values');
                CreateBoolVar(varName, svRandomize, AConstant);
              end
              else
                CreateIntVar(varName, rndVal, true, AConstant);

            end else
            begin
              if varType = 'Boolean' then
                raise Exception.Create('Expecting boolean value');

              varValue := ExpectInteger(line,index);

              CreateIntVar(varName, varValue, False, AConstant);
            end;
          end;
        end;
      end else
      begin
        if varType = '?' then
          raise Exception.Create('Variable type not specified');

        if isArray then
        begin
          if varType = 'Boolean' then
            raise Exception.Create('Array of booleans not supported, use integers instead')
          else
          begin
            if arraySize= 0 then arraySize := MaxArraySize;
            CreateIntArray(varName, arraySize, [], AConstant);
          end;
        end else
        begin
          if varType = 'Boolean' then
            CreateBoolVar(varName, svClear, AConstant)
          else
            CreateIntVar(varName, 0, false, AConstant);
        end;
      end;
    end;
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
  intVal, propIndex, propVal, timeMs, varIdx: integer;
  unitType, locStr, destLocStr, orderStr, filename, text: String;
  boolVal, textDefined: boolean;
  destPl: TPlayer;
  props: TUnitProperties;
  prop: TSetUnitProperty;
  alliance: TAllianceStatus;
  pl: TPlayer;
  conds: TConditionList;

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
    if TryInteger(ALine,AIndex,intVal) then
    begin
      if intVal <= 0 then raise exception.Create('Quantity must be at least 1');
      ExpectToken(ALine,AIndex,',');
    end else
      intVal := 1;

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
    AProg.Add(TCreateUnitInstruction.Create(APlayer, intVal, unitType, locStr, propIndex));

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
  if TryToken(ALine,AIndex,'GetUnit') then
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

procedure ParseInstruction(AText: string; AProg: TInstructionList; AThreads: TPlayers);
var
  line: TStringList;
  index, intVal, idxArr, i, idxSound: integer;
  params: TStringList;
  name, assignOp, msg: String;
  done: boolean;
  scalar, scalar2: TScalarVariable;
  players: TPlayers;
  conds: TConditionList;
  ints: ArrayOfInteger;
  sim: TSetIntegerMode;
  pl: TPlayer;

  procedure CheckEndOfLine;
  begin
    if index <> line.Count then
      raise exception.Create('Expecting end of line');
  end;

begin
  line := ParseLine(AText);
  try
    if (line.Count = 0) or ((line.Count = 1) and (line[0] = '')) then exit;

    index := 0;
    if TryToken(line,index,'Return') then
      AProg.Add( TReturnInstruction.Create )
    else
    if TryToken(line,index,'Option') then
    begin
      if TryToken(line,index,'Hyper') then
      begin
        if TryToken(line,index,'On') then HyperTriggers:= true
        else If TryToken(line,index,'Off') then HyperTriggers:= false
        else raise exception.Create('Expecting On or Off');
      end else
        raise exception.Create('Unknown option');
    end else
    if TryToken(line,index,'EndIf') then
    begin
      CheckEndOfLine;
      AProg.Add(TEndIfInstruction.Create);
    end else
    if TryToken(line,index,'End') then
    begin
      if TryToken(line,index,'While') then
      begin
        CheckEndOfLine;
        AProg.Add(TEndWhileInstruction.Create);
      end else
      if TryToken(line,index,'If') then
      begin
        CheckEndOfLine;
        AProg.Add(TEndIfInstruction.Create);
      end else
        raise exception.Create('Unknown end instruction');
    end else
    if TryToken(line,index,'While') then
    begin
      conds := ExpectConditions(line,index,AThreads);
      if (conds.Count = 1) and (conds[0] is TAlwaysCondition) then
        raise exception.Create('Infinite loop not allowed. You can use When True instead');
      AProg.Add(TWhileInstruction.Create(conds));
      CheckEndOfLine;
    end else
    if TryToken(line,index,'If') then
    begin
      conds := ExpectConditions(line,index,AThreads);
      AProg.Add(TIfInstruction.Create(conds));
      ExpectToken(line,index,'Then');
      CheckEndOfLine;
    end else
    if TryToken(line,index,'Else') then
    begin
      CheckEndOfLine;
      AProg.Add(TElseInstruction.Create);
    end else
    begin
      done := false;
      scalar := TryScalarVariable(line,index);
      if scalar.VarType <> svtNone then
      begin
        if TryToken(line,index,'=') then
        begin
          done := true;
          if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
          case scalar.VarType of
          svtInteger:
            begin
              intVal := ParseRandom(line, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simRandomize, intVal));
              end else
              begin
                if TryInteger(line,index,intVal) then
                begin
                  CheckEndOfLine;
                  AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simSetTo, intVal));
                end else
                begin
                  scalar2 := TryScalarVariable(line, index);
                  if scalar2.VarType = svtInteger then
                  begin
                    if (scalar2.Name <> scalar.Name) or (scalar2.Player <> scalar.Player) then
                    begin
                      AProg.Add(TTransferIntegerInstruction.Create(scalar2.Player, scalar2.Name, itCopyToAccumulator));
                      AProg.Add(TTransferIntegerInstruction.Create(scalar.Player, scalar.Name, itCopyFromAccumulator));
                    end;
                  end else
                    raise exception.Create('Expecting integer value');
                end;

              end;
            end;
          svtSwitch:
            begin
              intVal := ParseRandom(line, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                if intVal = 2 then
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svRandomize))
                else
                  raise exception.Create('Boolean can have only 2 values');
              end else
              begin
                conds := ExpectConditions(line,index,AThreads);
                if (conds.Count = 1) and (conds[0] is TSwitchCondition) and
                   (TSwitchCondition(conds[0]).Switch = scalar.Index) then
                begin
                  //a = Not a
                  if not TSwitchCondition(conds[0]).Value then
                    AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svToggle));
                  conds[0].Free;
                  conds.Free;
                end else
                  AppendConditionalInstruction(AProg, conds,
                    TSetSwitchInstruction.Create(scalar.Index, svSet),
                    TSetSwitchInstruction.Create(scalar.Index, svClear));
              end;
            end;
          else raise exception.Create('Unhandled case');
          end;
        end else
        If TryToken(line,index,'+') or TryToken(line,index,'-') then
        begin
          assignOp := line[index-1];
          if TryToken(line,index,'=') then
          begin
            done := true;
            if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
            if scalar.VarType = svtInteger then
            begin
              if TryInteger(line,index,intVal) then
              begin
                CheckEndOfLine;
                if assignOp='+' then
                  AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simAdd, intVal))
                else
                  AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simSubtract, intVal));
              end else
              begin
                scalar2 := TryScalarVariable(line,index);
                if scalar2.VarType = svtInteger then
                begin
                  AProg.Add(TTransferIntegerInstruction.Create(scalar2.Player, scalar2.Name, itCopyToAccumulator));
                  AProg.Add(TTransferIntegerInstruction.Create(scalar.Player, scalar.Name, itAddFromAccumulator));
                end else
                raise exception.Create('Expecting integer value');
              end;

            end
            else raise Exception.Create('Integer variables only can be incremented/decremented');
          end;
        end;

      end;

      idxSound := SoundIndexOf(line[0]);
      if idxSound <> -1 then
      begin
        index := 1;
        ExpectToken(line,index,'.');
        ExpectToken(line,index,'Play');
        if TryToken(line,index,'(') then ExpectToken(line,index,')');

        AProg.Add(TPlayWAVInstruction.Create(SoundVars[idxSound].Filename, SoundVars[idxSound].DurationMs));
        done := true;
      end;

      idxArr := IntArrayIndexOf(line[0]);
      if idxArr <> -1 then
      begin
        index := 1;
        If TryToken(line,index,'+') or TryToken(line,index,'-') then
          assignOp := line[index-1] else assignOp := '';

        if TryToken(line,index,'=') then
        begin
          done := true;
          if IntArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');

          ints := ParseIntArray(line,index);
          if length(ints) <> IntArrays[idxArr].Size then
            raise exception.Create('Array size mismatch');
          CheckEndOfLine;
          if assignOp = '+' then sim := simAdd
          else if assignOp = '-' then sim := simSubtract
          else sim := simSetTo;
          for i := 0 to high(ints) do
            AProg.Add(TSetIntegerInstruction.Create(IntToPlayer(i+1), IntArrays[idxArr].UnitType, sim, ints[i]));
        end;
      end;

      if not done then
      begin
        index := 0;
        pl := TryParsePlayer(line,index);
        if pl <> plNone then
        begin
          done := true;

          ExpectToken(line,index,'.');
          if index >= line.Count then
            raise exception.Create('Expecting action but end of line found');

          if not TryPlayerAction(AProg,line,index,pl, AThreads) then
            raise exception.Create('Expecting action but "' + line[index] + '" found');
          CheckEndOfLine;
        end;
      end;

      index := 0;
      if TryNeutralAction(AProg,line,index, AThreads) then
      begin
        CheckEndOfLine;
        done := true;
      end;

      if TryPlayerAction(AProg,line,index,plCurrentPlayer, AThreads) then
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
        while (index < line.Count) and IsValidVariableName(line[index]) do
        begin
          if index > 0 then name += ' ';
          name += line[index];
          inc(index);
        end;

        if (index = line.Count) or
          ((index < line.Count) and (line[index] = '(')) then
        begin
          if CompareText(name,'Print')=0 then
          begin
            ExpectToken(line,index,'(');
            players := ParsePlayers(line, index);
            ExpectToken(line,index,',');
            if index >= line.Count then raise exception.Create('Message expected');
            msg := ExpectString(line,index);
            ExpectToken(line,index,')');
            AProg.Add(TDisplayTextMessageInstruction.Create(true, msg, players));
          end else
          begin
            params := TStringList.Create;
            if TryToken(line,index,'(') then
            begin
              while not TryToken(line,index,')') do
              begin
                if params.Count > 0 then ExpectToken(line,index,',');
                if TryToken(line,index,',') then
                  raise exception.Create('Empty parameters not allowed');
                params.Add(line[index]);
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
          end;
        end else
        if scalar.VarType = svtNone then
          raise exception.Create('Unknown instruction "' + name + '"')
        else
          raise exception.Create('Expecting assignment');
      end;
    end;
  finally
    line.Free;
  end;
end;

function ReadProg(AFilename: string): boolean;
var t: TextFile;
  s: string;
  lineNumber, errorCount: integer;
  inSub, inEvent, i: integer;
  players: TPlayers;
begin
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
  ProcessDim('Const vbCr = Chr(13)',True);
  ProcessDim('Const vbLf = Chr(10)',True);
  ProcessDim('Const vbTab = Chr(9)',True);
  ProcessDim('Const vbCrLf = vbCr & vbLf',True);

  AssignFile(t, AFilename);
  Reset(t);
  lineNumber:= 1;
  errorCount := 0;
  inSub := -1;
  inEvent := -1;
  while not Eof(t) and (errorCount < 3) do
  begin
    ReadLn(t, s);
    s := Trim(s);
    try
      if compareText(copy(s, 1, 4), 'Dim ') = 0 then ProcessDim(s, false)
      else if compareText(copy(s, 1, 6), 'Const ') = 0 then ProcessDim(s, true)
      else if CompareText(copy(s, 1, 4), 'Sub ') = 0 then
      begin
        if (inSub<>-1) or (inEvent <> -1) then
          raise exception.Create('Nested procedures not allowed');
        inSub := ProcessSub(s)
      end
      else if (CompareText(s, 'Return') = 0) and (inEvent <> -1) then
      begin
        if not Events[inEvent].Preserve then
          raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
        Events[inEvent].Instructions.Add(TReturnInstruction.Create);
      end
      else if CompareText(s, 'Stop') = 0 then
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
      else if CompareText(s, 'End Sub') = 0 then
      begin
        if inSub= -1 then
          raise Exception.create('Not in a procedure');
        Procedures[inSub].Instructions.Add(TReturnInstruction.Create);
        inSub := -1;
      end
      else if (inSub = -1) and (inEvent = -1) and (CompareText(copy(s,1,3),'As ') = 0) then
      begin
        players := ParseAs(s);
        if eof(t) then raise exception.Create('End of file not expected');
        readLn(t,s);
        inEvent := ProcessEvent(s, players)
      end
      else if CompareText(copy(s, 1, 5), 'When ') = 0 then
      begin
        if (inSub<>-1) or (inEvent <> -1) then
          raise exception.Create('Nested events not allowed');
        inEvent := ProcessEvent(s, [])
      end
      else if CompareText(s, 'End When') = 0 then
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
      end
      else
      begin
        if inSub<>-1 then
          ParseInstruction(s, Procedures[inSub].Instructions,[plAllPlayers])
        else if inEvent<>-1 then
          ParseInstruction(s, Events[inEvent].Instructions, Events[inEvent].Players)
        else
          ParseInstruction(s, MainProg, [plCurrentPlayer]);
      end;
    except
      on ex:Exception do
      begin
        writeln('Line ', lineNumber, ': ', ex.Message);
        errorCount += 1;
      end;
    end;
    inc(lineNumber);
  end;
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

