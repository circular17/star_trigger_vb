unit uparseconditions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, uparsevb, uexpressions, usctypes;

function ExpectConditions(ALine: TStringList; var AIndex: integer; AThreads: TPlayers; AAllowOr: boolean = true): TConditionList;

implementation

uses
  uvariables;

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

    intVal := ExpectIntegerConstant(ALine,AIndex);
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
      intVal := ExpectIntegerConstant(ALine,AIndex);
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
      intVal := ExpectIntegerConstant(ALine,AIndex);
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

    intVal := ExpectIntegerConstant(ALine,AIndex);
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

    intVal := ExpectIntegerConstant(ALine,AIndex);
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

function TryArithmeticCondition(ALine: TStringList; var AIndex: integer; ANot: boolean): TCondition;
var
  op: TConditionOperator;
  merged, secondExpr, firstExpr: TExpression;
  mergeComp: TIntegerConditionMode;
  mergeCompValue: integer;

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
  firstExpr := TryExpression(ALine,AIndex,false);
  if firstExpr = nil then exit(nil);
  op := TryConditionOperator(ALine,AIndex);
  if op = coNone then exit(nil);
  secondExpr := TryExpression(ALine,AIndex,True);
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
  end;

  firstExpr.Free;
  secondExpr.Free;

  if merged.ConstElement < 0 then
  begin
    mergeCompValue += -merged.ConstElement;
    merged.ConstElement := 0;
  end;
  result := TArithmeticCondition.Create(merged, mergeComp, mergeCompValue);
  if ANot then result := TNotCondition.Create([result]);
end;

function ExpectCondition(ALine: TStringList; var AIndex: integer; AThreads: TPlayers): TCondition;
var
  intVal, afterNotIndex: Integer;
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

    afterNotIndex := AIndex;
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
       if result = nil then result := TryArithmeticCondition(ALine, AIndex, boolNot);
       if result = nil then
       begin
         if AIndex >= ALine.Count then
           raise exception.Create('Expecting condition but end of line found')
         else
           raise exception.Create('Expecting condition but "' + ALine[AIndex] + '" found')
       end
       else
       if not IsUniquePlayer(AThreads) or (AThreads = []) then
       begin
         result.Free;
         if AThreads = [] then
           raise exception.Create('You need to specify the player to which it applies ("Me" for main thread)')
         else
           raise exception.Create('You need to specify the player to which it applies ("Me" for each player)');
       end;
       exit;
      end;
    end;

    if scalar.Constant then
    begin
      AIndex := afterNotIndex;
      result := TryArithmeticCondition(ALine, AIndex, boolNot);
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
        result := TryArithmeticCondition(ALine, AIndex, boolNot);
        if result = nil then raise exception.Create('Expecting condition');
      end
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
          if not TryIntegerConstant(ALine,AIndex,intVal) then
          begin
            AIndex := afterNotIndex;
            result := TryArithmeticCondition(ALine, AIndex, boolNot);
            if result = nil then raise exception.Create('Expecting condition');
            exit;
          end;
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

function ExpectConditions(ALine: TStringList; var AIndex: integer; AThreads: TPlayers; AAllowOr: boolean = true): TConditionList;
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
        andExpr := ExpectConditions(ALine,AIndex,AThreads);
        for i := 0 to andExpr.Count-1 do
          result.Add(andExpr[i]);
        andExpr.Free;
        ExpectToken(ALine,AIndex,')');
      end else
        result.Add(ExpectCondition(ALine,AIndex,AThreads));
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
          andExpr := ExpectConditions(ALine,AIndex,AThreads,false);
          if andExpr.Count > 1 then
            orCond.Conditions.Add(TAndCondition.Create(andExpr))
          else
          begin
            orCond.Conditions.Add(andExpr[0]);
            andExpr.Free;
          end;
        until not TryToken(ALine,AIndex,'Or');
      end;
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


end.

