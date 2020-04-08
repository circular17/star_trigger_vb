unit uparseconditions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, uparsevb, uexpressions, usctypes;

function ExpectConditions(AScope: integer; ALine: TStringList; var AIndex: integer; AThreads: TPlayers; AAllowOr: boolean = true): TConditionList;

implementation

uses
  uvariables, utriggerconditions, uparsescalar, uprocedures, uarithmetic;

function DuplicateParameterValuesImplementation(AParamValues: ArrayOfParameterValue): ArrayOfParameterValue;
var
  i: Integer;
begin
  setlength(result, length(AParamValues));
  for i := 0 to high(result) do
  begin
    if Assigned(AParamValues[i].Condition) then
      result[i].Condition := (AParamValues[i].Condition as TCondition).Duplicate;
    if Assigned(AParamValues[i].Expression) then
      result[i].Expression := (AParamValues[i].Expression as TExpression).Duplicate;
  end;
end;

function ExpectBooleanConstantImplementation(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): boolean;
var
  cond: TConditionList;
begin
  cond := ExpectConditions(AScope, ALine, AIndex, AThreads, true);
  if (cond.Count = 1) and (cond[0] is TAlwaysCondition) then result := true
  else if (cond.Count = 1) and (cond[0] is TNeverCondition) then result := false
  else
  begin
    cond.FreeAll;
    raise exception.Create('Constant expression expected');
  end;
  cond.FreeAll;
end;

function TryNeutralConditionFunction(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: Integer; ANegation: boolean): TCondition;
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

    intVal := ExpectIntegerConstant(AThreads, AScope, ALine,AIndex,false);
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

function TryPlayerConditionFunction(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: Integer; APlayer: TPlayer; ANegation: boolean): TCondition;
var
  unitType: TStarcraftUnit;
  locStr: String;
  op: TConditionOperator;
  intVal, idxVar: Integer;

  procedure ExpectCurrentPlayer;
  begin
    If APlayer <> plCurrentPlayer then raise exception.Create('Checking against Min/Max can only be done with the current player ("Me")');
  end;

begin
  if TryToken(ALine,AIndex,'UnitCount') then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      unitType := ExpectUnitType(AThreads, AScope, ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
        locStr := ExpectStringConstant(AThreads, AScope, ALine,AIndex)
      else
        locStr := '';
      ExpectToken(ALine,AIndex,')');
    end else
    begin
      unitType := suAnyUnit;
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
      intVal := ExpectIntegerConstant(AThreads, AScope, ALine,AIndex,false);
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
      unitType := ExpectUnitType(AThreads, AScope, ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
    end else
    begin
      unitType := suAnyUnit;
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
      intVal := ExpectIntegerConstant(AThreads, AScope, ALine,AIndex,false);
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
      unitType := ExpectUnitType(AThreads, AScope, ALine, AIndex);
      ExpectToken(ALine,AIndex,')');
    end else
    begin
      unitType := suAnyUnit;
    end;
    op := TryConditionOperator(ALine,AIndex);
    if op = coNone then
      raise exception.Create('Comparison expected');

    if ANegation then op := NotConditionOperator[op];

    intVal := ExpectIntegerConstant(AThreads, AScope, ALine,AIndex,false);
    case op of
    coEqual: result := CreateIntegerCondition(APlayer, unitType, icmExactly, intVal);
    coGreaterThanOrEqual: result := CreateIntegerCondition(APlayer, unitType, icmAtLeast, intVal);
    coLowerThanOrEqual: result := CreateIntegerCondition(APlayer, unitType, icmAtMost, intVal);
    coGreaterThan: if intVal = maxLongint then
                      result := TNeverCondition.Create
                   else
                      result := CreateIntegerCondition(APlayer, unitType, icmAtLeast, intVal+1);
    coLowerThan: if intVal = 0 then
                      result := TNeverCondition.Create
                   else
                      result := CreateIntegerCondition(APlayer, unitType, icmAtMost, intVal-1);
    coNotEqual: result := TNotCondition.Create([CreateIntegerCondition(APlayer, unitType, icmExactly, intVal)]);
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

    intVal := ExpectIntegerConstant(AThreads, AScope, ALine,AIndex,false);
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
  if TryToken(ALine,AIndex,'Present') then
  begin
    idxVar := GetPlayerPresenceBoolVar(APlayer);
    result := TSwitchCondition.Create(BoolVars[idxVar].Switch,not ANegation);
    exit;
  end else
    result := nil;
end;

function TryArithmeticCondition(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; ANot: boolean): TCondition;
var
  op: TConditionOperator;
  merged, secondExpr, firstExpr: TExpression;
  mergeComp: TIntegerConditionMode;
  mergeCompValue, oldIndex: integer;

  procedure AppendFirst;
  var
    i: Integer;
  begin
    for i := 0 to firstExpr.Elements.Count-1 do
      merged.Elements.Add(firstExpr.Elements[i]);
    firstExpr.Elements.Clear;
    merged.ConstElement += firstExpr.ConstElement;
  end;

  procedure AppendSecond;
  var
    i: Integer;
  begin
    for i := 0 to secondExpr.Elements.Count-1 do
      merged.Elements.Add(secondExpr.Elements[i]);
    secondExpr.Elements.Clear;
    merged.ConstElement += secondExpr.ConstElement;
  end;

begin
  oldIndex := AIndex;
  firstExpr := TryExpression(AThreads, AScope, ALine,AIndex,false);
  if firstExpr = nil then exit(nil);
  op := TryConditionOperator(ALine,AIndex);
  if op = coNone then
  begin
    firstExpr.Free;
    AIndex := oldIndex;
    exit(nil);
  end;
  secondExpr := nil;
  try
    secondExpr := TryExpression(AThreads, AScope, ALine,AIndex,True);
    if ANot then
    begin
      op := NotConditionOperator[op];
      ANot := false;
    end;

    merged := TExpression.Create;
    case op of
    coGreaterThan,coGreaterThanOrEqual: begin
        AppendFirst;
        secondExpr.NegateAll;
        AppendSecond;
        if op = coGreaterThanOrEqual then merged.ConstElement += 1;
        mergeComp := icmAtLeast;
        mergeCompValue := 1;
      end;
    coLowerThan,coLowerThanOrEqual: begin
        AppendSecond;
        firstExpr.NegateAll;
        AppendFirst;
        if op = coLowerThanOrEqual then merged.ConstElement += 1;
        mergeComp := icmAtLeast;
        mergeCompValue := 1;
      end;
    coEqual,coNotEqual: begin
        AppendSecond;
        firstExpr.NegateAll;
        AppendFirst;
        if (merged.NegativeCount > 0) and (merged.PositiveCount = 0) then
          merged.NegateAll;
        merged.ConstElement += 1;
        mergeComp := icmExactly;
        mergeCompValue := 1;
        ANot := (op = coNotEqual);
      end;
    else raise exception.Create('Case not handled');
    end;

    if merged.ConstElement < 0 then
    begin
      mergeCompValue += -merged.ConstElement;
      merged.ConstElement := 0;
    end;
    result := TArithmeticCondition.Create(merged, mergeComp, mergeCompValue);
    if ANot then result := TNotCondition.Create([result]);

  finally
    firstExpr.Free;
    secondExpr.Free;
  end;
end;

function ExpectCondition(AScope: integer; ALine: TStringList; var AIndex: integer; AThreads: TPlayers): TCondition;
var
  intVal, afterNotIndex, funcIdx: Integer;
  op: TConditionOperator;
  boolNot: Boolean;
  scalar: TScalarVariable;
  boolVal: boolean;
  pl: TPlayer;
  paramValues: ArrayOfParameterValue;
  funcName: string;

begin
  if TryBoolean(AThreads, AScope, ALine,AIndex,boolVal) then
  begin
    if boolVal then
      result := TAlwaysCondition.Create
    else
      result := TNeverCondition.Create
  end else
  begin
    boolNot := TryToken(ALine,AIndex,'Not');

    afterNotIndex := AIndex;
    scalar := TryScalarVariable(AThreads, AScope, ALine,AIndex);
    if scalar.VarType = svtNone then
    begin
      pl := TryParsePlayer(AThreads, AScope, ALine,AIndex);
      if pl <> plNone then
      begin
        ExpectToken(ALine,AIndex,'.');
        result := TryPlayerConditionFunction(AThreads, AScope, ALine, AIndex, pl, boolNot);
        if result = nil then raise exception.Create('Expecting player condition');
        exit;
      end;

      result := TryNeutralConditionFunction(AThreads, AScope, ALine, AIndex, boolNot);
      if result <> nil then exit;

      if TryFunction(AScope, ALine, AIndex, funcName) then
      begin
        try
          paramValues := ParseProcedureParameterValues(AThreads, AScope, funcName, ALine, AIndex);
          funcIdx := ProcedureIndexOf(AScope, funcName, length(paramValues));
          if funcIdx = -1 then raise exception.Create('Procedure not found');
          if Procedures[funcIdx].ReturnType <> 'Boolean' then
            raise exception.Create('Expecting boolean value but found ' + Procedures[funcIdx].ReturnType);
          result := TCallFunctionCondition.Create(AScope, funcName, paramValues);
          paramValues := nil;
        finally
          FreeParameterValues(paramValues);
        end;
        exit;
      end else
      if TryBoolean(AThreads, AScope, ALine,AIndex,boolVal) then
      begin
        if boolVal xor boolNot then
          result := TAlwaysCondition.Create
        else
          result := TNeverCondition.Create;
        exit;
      end else
      begin
        result := TryPlayerConditionFunction(AThreads, AScope, ALine, AIndex, plCurrentPlayer, boolNot);
        if (result <> nil) and (not IsUniquePlayer(AThreads) or (AThreads = [])) then
        begin
          result.Free;
          if AThreads = [] then
            raise exception.Create('You need to specify the player to which it applies ("Me" for main thread)')
          else
            raise exception.Create('You need to specify the player to which it applies ("Me" for each player)');
        end else
        begin
          if result = nil then result := TryArithmeticCondition(AThreads, AScope, ALine, AIndex, boolNot);
          if result = nil then
          begin
            if AIndex >= ALine.Count then
              raise exception.Create('Expecting condition but end of line found')
            else
              raise exception.Create('Expecting condition but "' + ALine[AIndex] + '" found')
          end;
        end;
        exit;
      end;
    end;

    if scalar.Constant then
    begin
      AIndex := afterNotIndex;
      result := TryArithmeticCondition(AThreads, AScope, ALine, AIndex, boolNot);
      if result = nil then raise exception.Create('Expecting condition');
      exit;
    end;

    case scalar.VarType of
    svtInteger:
    begin
      op := TryConditionOperator(ALine,AIndex);
      if op = coNone then
      begin
        AIndex := afterNotIndex;
        result := TryArithmeticCondition(AThreads, AScope, ALine, AIndex, boolNot);
        if result = nil then raise exception.Create('Expecting condition');
      end
      else
      begin
        if boolNot then op := NotConditionOperator[op];

        if (scalar.Player in [plNone,plCurrentPlayer]) and TryToken(ALine,AIndex,'Min') then
        begin
          case op of
          coEqual,coLowerThanOrEqual:
            result := CreateCompareIntegerCondition(scalar.UnitType,False);
          coGreaterThanOrEqual:
            result := TAlwaysCondition.Create;
          coGreaterThan,coNotEqual:
            result := TNotCondition.Create([CreateCompareIntegerCondition(scalar.UnitType,False)]);
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
            result := CreateCompareIntegerCondition(scalar.UnitType,true);
          coLowerThanOrEqual:
            result := TAlwaysCondition.Create;
          coLowerThan,coNotEqual:
            result := TNotCondition.Create([CreateCompareIntegerCondition(scalar.UnitType,true)]);
          coGreaterThan:
            result := TNeverCondition.Create;
          else
            raise exception.Create('Unhandled case');
          end;
        end
        else
        begin
          if not TryIntegerConstant(AThreads, AScope, ALine, AIndex, intVal) then
          begin
            AIndex := afterNotIndex;
            result := TryArithmeticCondition(AThreads, AScope, ALine, AIndex, boolNot);
            if result = nil then raise exception.Create('Expecting condition');
            exit;
          end;
          case op of
          coEqual: result := CreateIntegerCondition(scalar.Player, scalar.UnitType, icmExactly, intVal);
          coGreaterThanOrEqual: result := CreateIntegerCondition(scalar.Player, scalar.UnitType, icmAtLeast, intVal);
          coLowerThanOrEqual: result := CreateIntegerCondition(scalar.Player, scalar.UnitType, icmAtMost, intVal);
          coGreaterThan: if intVal = maxLongint then
                            result := TNeverCondition.Create
                         else
                            result := CreateIntegerCondition(scalar.Player, scalar.UnitType, icmAtLeast, intVal+1);
          coLowerThan: if intVal = 0 then
                            result := TNeverCondition.Create
                         else
                            result := CreateIntegerCondition(scalar.Player, scalar.UnitType, icmAtMost, intVal-1);
          coNotEqual: result := TNotCondition.Create([CreateIntegerCondition(scalar.Player, scalar.UnitType, icmExactly, intVal)]);
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

function ExpectConditions(AScope: integer; ALine: TStringList; var AIndex: integer; AThreads: TPlayers; AAllowOr: boolean = true): TConditionList;
var
  i, j: Integer;
  orCond: TOrCondition;
  andExpr: TConditionList;
begin
  result := TConditionList.Create;
  try
    repeat
      if TryToken(ALine,AIndex,'(') then
      begin
        andExpr := ExpectConditions(AScope, ALine,AIndex,AThreads);
        for i := 0 to andExpr.Count-1 do
          result.Add(andExpr[i]);
        andExpr.Free;
        ExpectToken(ALine,AIndex,')');
      end else
        result.Add(ExpectCondition(AScope, ALine,AIndex,AThreads));
      if AAllowOr and TryToken(ALine,AIndex,'Or') then
      begin
        if result.Count = 1 then
        begin
          orCond := TOrCondition.Create([result[0]]);
          result.Delete(0);
        end
        else
        begin
          orCond := TOrCondition.Create([TAndCondition.Create(result)]);
          result := TConditionList.Create;
        end;
        result.Add(orCond);
        repeat
          andExpr := ExpectConditions(AScope, ALine,AIndex,AThreads,false);
          if andExpr.Count > 1 then
            orCond.Conditions.Add(TAndCondition.Create(andExpr))
          else
          begin
            orCond.Conditions.Add(andExpr[0]);
            andExpr.Free;
          end;
        until not TryToken(ALine,AIndex,'Or');
        for i := orCond.Conditions.Count-1 downto 0 do
          if (orCond.Conditions[i] is TNeverCondition) and (orCond.Conditions.Count > 1) then
          begin
            orCond.Conditions[i].Free;
            orCond.Conditions.Delete(i);
          end else
          if orCond.Conditions[i] is TAlwaysCondition then
          begin
            for j := orCond.Conditions.Count-1 downto 0 do
              if j <> i then
              begin
                orCond.Conditions[j].Free;
                orCond.Conditions.Delete(j);
              end;
          end;
        if orCond.Conditions.Count = 1 then
        begin
          result.Remove(orCond);
          result.Add(orCond.Conditions[0].Duplicate);
          orCond.Free;
        end;
      end;
      if not TryToken(ALine,AIndex,'And') then break;
    until false;
  except on ex:exception do
    begin
      result.FreeAll;
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

function ParseProcedureParameterValuesImplementation(AThreads: TPlayers; AScope: integer; funcName: string; ALine: TStringList; var AIndex: integer): ArrayOfParameterValue;
var
  expr: TExpression;
  cond: TCondition;
  params: ArrayOfParameterValue;
  canUInt, canBool: Boolean;
begin
  params := nil;
  try
    if TryToken(ALine, AIndex, '(') then
    begin
      while not PeekToken(ALine, AIndex, ')') do
      begin
        canUInt := ProcedureCanHaveUIntParameter(AScope, funcName, length(params));
        if canUInt then
        begin
          expr := TryExpression(AThreads, AScope, ALine, AIndex, false, false);
          if expr <> nil then
          begin
            setlength(params, length(params)+1);
            params[high(params)].Expression := expr;
            if not TryToken(ALine, AIndex, ',') then break;
            continue;
          end;
        end;
        canBool := ProcedureCanHaveBoolParameter(AScope, funcName, length(params));
        if canBool then
        begin
          cond := ExpectCondition(AScope, ALine, AIndex, AThreads);
          setlength(params, length(params)+1);
          params[high(params)].Condition := cond;
          if not TryToken(ALine, AIndex, ',') then break;
          continue;
        end;
        if canUInt then
          raise exception.Create('Integer parameter expected')
        else
          raise exception.Create('No parameter expected here');
      end;
      ExpectToken(ALine,AIndex,')');
    end;
    result := params;
    params := nil;
  finally
    FreeParameterValues(params);
  end;
end;

initialization

  ExpectBooleanConstant := @ExpectBooleanConstantImplementation;
  ParseProcedureParameterValues := @ParseProcedureParameterValuesImplementation;
  DuplicateParameterValues := @DuplicateParameterValuesImplementation;

end.

