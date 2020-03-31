unit ureadinstruction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, uinstructions;

procedure ParseInstruction(AScope: integer; ALine: TStringList; AProg: TInstructionList; AThreads: TPlayers; AMainThread: TPlayer; AProcId: integer; AInSubMain: boolean);

implementation

uses utriggerconditions, uparsevb, uparseconditions,
  utriggerinstructions, uparsescalar, uexpressions, umapinfo, uparsecomplex,
  uvariables, uprocedures;

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

function TryNeutralAction(AScope: integer; AProg: TInstructionList; ALine: TStringList; var AIndex: Integer; AThreads: TPlayers): boolean;
var
  scenario: String;
  conds: TConditionList;
begin
  if TryToken(ALine,AIndex,'CountdownPaused') then
  begin
    result := true;
    ExpectToken(ALine,AIndex,'=');
    conds := ExpectConditions(AScope, ALine,AIndex,AThreads);
    AppendConditionalInstruction(AProg, conds, TPauseCountdownInstruction.Create(true),
                                        TPauseCountdownInstruction.Create(false));
  end else
  if TryToken(ALine,AIndex,'GamePaused') then  //not really neutral in the sense that each player can have a limited amount of pause
  begin
    result := true;
    ExpectToken(ALine,AIndex,'=');
    conds := ExpectConditions(AScope, ALine,AIndex,AThreads);
    AppendConditionalInstruction(AProg, conds, TPauseGameInstruction.Create(true),
                                        TPauseGameInstruction.Create(false));
  end else
  if TryToken(ALine,AIndex,'NextScenario') then
  begin
    ExpectToken(ALine,AIndex,'=');
    scenario := ExpectStringConstant(AScope, ALine,AIndex);
    AProg.Add(TSetNextScenarioInstruction.Create(scenario));
  end else
  if TryToken(ALine,AIndex,'Wait') then
  begin
    ExpectToken(ALine,AIndex,'(');
    AProg.Add(TWaitInstruction.Create(ExpectIntegerConstant(AScope, ALine,AIndex,false)));
    ExpectToken(ALine,AIndex,')');
  end else
    result := false;
end;

function TryPlayerAction(AScope: integer; AProg: TInstructionList; ALine: TStringList; var AIndex: Integer; APlayer: TPlayer; AThreads: TPlayers): boolean;
var
  intVal, propIndex, propVal, timeMs, tempInt, i: integer;
  unitType: TStarcraftUnit;
  locStr, destLocStr, orderStr, filename, text: String;
  textDefined: boolean;
  destPl: TPlayer;
  props: TUnitProperties;
  prop: TSetUnitProperty;
  alliance: TAllianceStatus;
  pl: TPlayer;
  conds: TConditionList;
  expr: TExpression;
  subInstr: TInstructionList;
  players: TPlayers;
  unitOrder: TUnitOrder;

  procedure CheckCurrentPlayer;
  begin
    if APlayer <> plCurrentPlayer then
      raise exception.Create('This action can only be done with the current player "Me". Use instruction "Do As" for multithreading');
  end;

  function ParseOptionalQuantity(ACommaAfter: boolean = true): integer;
  begin
    if TryToken(ALine,AIndex,'All') then
    begin
      result := -1;
      if ACommaAfter then ExpectToken(ALine,AIndex,',');
    end else
    if TryIntegerConstant(AScope, ALine,AIndex,result) then
    begin
      if ACommaAfter then ExpectToken(ALine,AIndex,',');
    end else
      result := -1; //All by default
  end;

begin
  result := true;
  if TryToken(ALine,AIndex,'CreateUnit') then
  begin
    ExpectToken(ALine,AIndex,'(');
    expr := TryExpression(AScope,ALine,AIndex,True);
    if expr.IsConstant and (expr.ConstElement < 1) then
    begin
      expr.Free;
      raise exception.Create('Quantity must be at least 1');
    end;
    try
      ExpectToken(ALine,AIndex,',');
      unitType := ExpectUnitType(AScope,ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
        locStr := ExpectStringConstant(AScope,ALine,AIndex)
      else
        locStr := MapInfo.AnywhereLocationName;
      if TryToken(ALine,AIndex,',') then
      begin
        propIndex := TryUnitPropVar(AScope,ALine,AIndex);
        if propIndex = -1 then
          raise exception.Create('Unit properties expected');
      end else
        propIndex := -1;
      ExpectToken(ALine,AIndex,')');

      if propIndex = -1 then
      begin
        if TryToken(ALine,AIndex,'With') then
        begin
          propIndex := TryUnitPropVar(AScope,ALine,AIndex);
          if propIndex = -1 then
            raise exception.Create('Unit properties expected');
        end;
      end;

      if expr.IsConstant then
        AProg.Add(TCreateUnitInstruction.Create(APlayer, expr.ConstElement, unitType, locStr, propIndex+1))
      else
      begin
        tempInt := AllocateTempInt(8);
        expr.AddToProgram(AProg, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
        for i := 7 downto 0 do
        begin
          subInstr := TInstructionList.Create;
          subInstr.Add( TCreateUnitInstruction.Create(APlayer, 1 shl i, unitType, locStr, propIndex+1) );
          subInstr.Add( CreateSetIntegerInstruction(IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSubtract, 1 shl i) );
          AProg.Add( TFastIfInstruction.Create( [CreateIntegerCondition( IntVars[tempInt].Player,IntVars[tempInt].UnitType, icmAtLeast, 1 shl i)], subInstr) );
        end;
        ReleaseTempInt(tempInt);
      end;
    finally
      expr.Free;
    end;
  end else
  if TryToken(ALine,AIndex,'Location') then
  begin
    ExpectToken(ALine,AIndex,'(');
    locStr := ExpectStringConstant(AScope,ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    ExpectToken(ALine,AIndex,'.');
    if TryToken(ALine,AIndex,'Attract') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectStringConstant(AScope,ALine,AIndex);
       ExpectToken(ALine,AIndex,')');
       AProg.Add(TMoveLocationInstruction.Create(APlayer, suUnusedCaveIn, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'CenterOn') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectStringConstant(AScope,ALine,AIndex);
       ExpectToken(ALine,AIndex,')');
       AProg.Add(TMoveLocationInstruction.Create(APlayer, suUnusedCaveIn, destLocStr, locStr));
    end else
      raise exception.create('Expecting location method');
  end else
  if TryToken(ALine,AIndex,'Units') then
  begin
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectUnitType(AScope,ALine,AIndex);
    if not TryToken(ALine,AIndex,',') then
      locStr := MapInfo.AnywhereLocationName
    else
      locStr := ExpectStringConstant(AScope,ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    ExpectToken(ALine,AIndex,'.');

    if TryToken(ALine,AIndex,'Properties') then
    begin
      ExpectToken(ALine,AIndex,'=');
      if not TryUnitProperties(AScope,ALine,AIndex,props) then
      begin
        if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
        begin
          propIndex:= UnitPropIndexOf(AScope,ALine[AIndex]);
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
      if unitType = suAnyUnit then
        AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supResource, props.Resource))
      else
        if props.Resource <> 0 then
          raise exception.Create('Resource cannot be applied to a specific unit');
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supHangarCount, props.HangarCount));

      if intVal = -1 then
      begin
        if props.Invincible then
          AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvEnable))
        else
          AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvDisable));
      end;

    end else
    if TryToken(ALine,AIndex,'Location') then
    begin
       ExpectToken(ALine,AIndex,'=');
       destLocStr := ExpectStringConstant(AScope,ALine,AIndex);

       AProg.Add(TTeleportUnitInstruction.Create(APlayer, intVal, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'Teleport') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectStringConstant(AScope,ALine,AIndex);
       ExpectToken(ALine,AIndex,')');

       AProg.Add(TTeleportUnitInstruction.Create(APlayer, intVal, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'AttractLocation') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectStringConstant(AScope,ALine,AIndex);
       ExpectToken(ALine,AIndex,')');
       if intVal <> -1 then raise exception.Create('Cannot specify quantity for this action (use All quantity instead)');
       AProg.Add(TMoveLocationInstruction.Create(APlayer, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'MoveOrder') or TryToken(ALine,AIndex,'PatrolOrder') or TryToken(ALine,AIndex,'AttackOrder') then
    begin
      orderStr := ALine[AIndex-1];
      if compareText(orderStr,'PatrolOrder')=0 then unitOrder := uoPatrol
      else if compareText(orderStr,'AttackOrder')=0 then unitOrder := uoAttack
      else unitOrder := uoMove;

      if intVal <> -1 then
        raise exception.Create('Cannot specify quantity for an order (use All quantity instead)');
      ExpectToken(ALine,AIndex,'(');
      destLocStr := ExpectStringConstant(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TOrderUnitInstruction.Create(APlayer, unitType, locStr, destLocStr, unitOrder));
    end else
    if TryToken(ALine,AIndex,'Kill') then
    begin
      if TryToken(ALine,AIndex,'(') then
      begin
        if not TryToken(ALine,AIndex,')') then
        begin
          intVal := ExpectIntegerConstant(AScope, ALine, AIndex, false);
          ExpectToken(ALine,AIndex,')');
        end;
      end;
      AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, true));
    end else
    if TryToken(ALine,AIndex,'Remove') then
    begin
      if TryToken(ALine,AIndex,'(') then
      begin
        if not TryToken(ALine,AIndex,')') then
        begin
          intVal := ExpectIntegerConstant(AScope, ALine, AIndex, false);
          ExpectToken(ALine,AIndex,')');
        end;
      end;
      AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, false));
    end else
    if TryToken(ALine,AIndex,'Give') then
    begin
      ExpectToken(ALine,AIndex,'(');
      destPl:= TryParsePlayer(AScope,ALine,AIndex);
      if destPl = plNone then raise exception.Create('Expecting player');
      if TryToken(ALine,AIndex,',')  then
        intVal := ExpectIntegerConstant(AScope, ALine, AIndex, false);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TGiveUnitInstruction.Create(APlayer, intVal, unitType, locStr, destPl));
    end else
    if TryToken(ALine,AIndex,'ToggleInvincibility') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,'(');

      if intVal <> -1 then
        raise exception.Create('Cannot specify quantity here (use All quantity instead)');
      AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvToggle));
    end else
    if TryToken(ALine,AIndex,'ToggleDoodadState') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,'(');

      if intVal <> -1 then
        raise exception.Create('Cannot specify quantity here (use All quantity instead)');
      AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufDoodadState, ufvToggle));
    end else
    if TryToken(ALine,AIndex,'Invincible') then
    begin
      if intVal <> -1 then
        raise exception.Create('Cannot specify quantity here (use All quantity instead)');

      ExpectToken(ALine,AIndex,'=');
      conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvEnable),
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvDisable));
    end else
    if TryToken(ALine,AIndex,'DoodadState') then
    begin
      if intVal <> -1 then
        raise exception.Create('Cannot specify quantity here (use All quantity instead)');

      ExpectToken(ALine,AIndex,'=');
      conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufDoodadState, ufvEnable),
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufDoodadState, ufvDisable));
    end else
    if TryToken(ALine,AIndex,'Burrowed') or
      TryToken(ALine,AIndex,'Cloaked') or
      TryToken(ALine,AIndex,'Hallucinated') or
      TryToken(ALine,AIndex,'Lifted') then
    begin
      raise exception.Create('This property cannot be changed after the unit is created');
    end else
    begin
      if TryToken(ALine,AIndex,'Life') then prop := supLife else
      if TryToken(ALine,AIndex,'Shield') then prop := supShield else
      if TryToken(ALine,AIndex,'Energy') then prop := supEnergy else
      if TryToken(ALine,AIndex,'Resource') then prop := supResource else
      if TryToken(ALine,AIndex,'HangarCount') then prop := supHangarCount else
        raise exception.Create('Expecting property name');
      ExpectToken(ALine,AIndex,'=');

      propVal:= ExpectIntegerConstant(AScope,ALine,AIndex,false);
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, prop, propVal));
    end;

  end else
  begin
    if TryToken(ALine,AIndex,'CenterView') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      locStr := ExpectStringConstant(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TCenterViewInstruction.Create(locStr));
    end else
    if TryToken(ALine,AIndex,'MinimapPing') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      locStr := ExpectStringConstant(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TCenterViewInstruction.Create(locStr));
    end else
    if TryToken(ALine,AIndex,'Print') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      text := ExpectStringConstant(AScope,ALine,AIndex,true);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TDisplayTextMessageInstruction.Create(true, text));
    end else
    if TryToken(ALine,AIndex,'TalkingPortrait') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      unitType := ExpectUnitType(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,',');
      timeMs := ExpectIntegerConstant(AScope,ALine,AIndex,false);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TTalkingPortraitInstruction.Create(unitType, timeMs));
    end else
    if TryToken(ALine,AIndex,'MissionObjectives') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'=');
      text := ExpectStringConstant(AScope,ALine,AIndex);
      AProg.Add(TSetMissionObjectivesInstruction.Create(text));
    end else
    if TryToken(ALine,AIndex,'Leaderboard') then
    begin
      ExpectToken(ALine,AIndex,'.');
      if TryToken(ALine,AIndex,'Computers') then
      begin
        ExpectToken(ALine,AIndex,'=');
        conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
        AppendConditionalInstruction(AProg, conds,
          TLeaderBoardIncludeComputersInstruction.Create(ufvEnable),
          TLeaderBoardIncludeComputersInstruction.Create(ufvDisable));
      end else
      if TryToken(ALine,AIndex,'ToggleComputers') then
      begin
        AProg.Add(TLeaderBoardIncludeComputersInstruction.Create(ufvToggle));
      end else
      if TryToken(ALine,AIndex,'Show') then
      begin
        ExpectToken(ALine,AIndex,'(');
        textDefined:= TryStringConstant(AScope,ALine,AIndex,text);
        if textDefined then ExpectToken(ALine,AIndex,',');

        if TryToken(ALine,AIndex,'MineralsAndGas') or
         TryToken(ALine,AIndex,'OreAndGas') then
        begin
          if not textDefined then
          begin
            intVal := MaxLongInt;
            if TryToken(ALine,AIndex,',') then
            begin
              if not TryIntegerConstant(AScope,ALine,AIndex,intVal) then
                raise exception.Create('Expecting integer value');
            end;
            AProg.Add(TShowLeaderboardOreAndGasIconInstruction.Create(intVal));
          end else
            AProg.Add(TShowLeaderboardResourceInstruction.Create('minerals and gas', srOreAndGas,-1));
        end else
        begin
          if TryInteger(AScope,ALine,AIndex,intVal) then
            ExpectToken(ALine,AIndex,'-')
          else intVal := -1;

          if TryToken(ALine,AIndex,'MineralsAndGas') or TryToken(ALine,AIndex,'OreAndGas') then
          begin
            if not textDefined then text := 'minerals and gas';
            AProg.Add(TShowLeaderboardResourceInstruction.Create(text, srOreAndGas, intVal));
          end else
          if TryToken(ALine,AIndex,'Minerals') or TryToken(ALine,AIndex,'Ore') then
          begin
            if not textDefined then text := 'minerals';
            AProg.Add(TShowLeaderboardResourceInstruction.Create(text, srOre, intVal));
          end else
          if TryToken(ALine,AIndex,'Gas') then
          begin
            if not textDefined then text := 'gas';
            AProg.Add(TShowLeaderboardResourceInstruction.Create(text, srGas, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitScore') then
          begin
            if not textDefined then text := 'unit score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssUnitScore, intVal));
          end else
          if TryToken(ALine,AIndex,'BuildingScore') then
          begin
            if not textDefined then text := 'building score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssBuildingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitAndBuildingScore') then
          begin
            if not textDefined then text := 'unit and building score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssUnitAndBuildingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'KillScore') then
          begin
            if not textDefined then text := 'kill score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssKillScore, intVal));
          end else
          if TryToken(ALine,AIndex,'RazingScore') then
          begin
            if not textDefined then text := 'razing score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssRazingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'KillAndRazingScore') then
          begin
            if not textDefined then text := 'kill and razing score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssKillAndRazingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'TotalScore') then
          begin
            if not textDefined then text := 'total score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssTotalScore, intVal));
          end else
          if TryToken(ALine,AIndex,'CustomScore') then
          begin
            if not textDefined then text := 'custom score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssCustomScore, intVal));
          end else
          if TryToken(ALine,AIndex,'KillCount') then
          begin
            if TryToken(ALine,AIndex,'(') then
            begin
              unitType := ExpectUnitType(AScope,ALine,AIndex);
              ExpectToken(ALine,AIndex,')');
            end else
            begin
              unitType := suAnyUnit;
            end;
            if not textDefined then text := 'kills';
            AProg.Add(TShowLeaderboardKillCountInstruction.Create(text, unitType, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitCount') then
          begin
            if TryToken(ALine,AIndex,'(') then
            begin
              unitType := ExpectUnitType(AScope,ALine,AIndex);
              if TryToken(ALine,AIndex,',') then
                locStr := ExpectStringConstant(AScope,ALine,AIndex)
              else
                locStr := MapInfo.AnywhereLocationName;
              ExpectToken(ALine,AIndex,')');
            end else
            begin
              unitType := suAnyUnit;
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
      ExpectToken(ALine,AIndex,'.');
      if TryToken(ALine,AIndex,'Ennemy') then alliance := asEnnemy
      else if TryToken(ALine,AIndex,'Ally') then alliance := asAlly
      else if TryToken(ALine,AIndex,'AlliedVictory') then alliance := asAlliedVictory
      else raise exception.Create('Expecting alliance status (Ennemy, Ally, AlliedVictory)');
      ExpectToken(ALine,AIndex,'(');
      players := ExpectPlayers(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');

      for pl := low(TPlayer) to high(TPlayer) do
        if pl in players then AProg.Add(TSetAllianceStatus.Create(pl, alliance));
    end else
    if TryToken(ALine,AIndex,'NextScenario') then
    begin
      raise exception.Create('Changing scenario cannot be done for a specific player');
    end else
    if TryToken(ALine,AIndex,'RunAIScript') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      filename := ExpectStringConstant(AScope,ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
        locStr := ExpectStringConstant(AScope,ALine,AIndex)
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
      conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
        TUnitSpeechInstruction.Create(true),
        TUnitSpeechInstruction.Create(false));
    end else
      result := false;
  end;
end;

procedure ParseInstruction(AScope: integer; ALine: TStringList; AProg: TInstructionList; AThreads: TPlayers; AMainThread: TPlayer; AProcId: integer; AInSubMain: boolean);
var
  index, intVal, idxArr, i, idxSound, sw, idxVar, idxMsg: integer;
  params: TStringList;
  name, assignOp, text: String;
  done, boolVal: boolean;
  scalar: TScalarVariable;
  conds: TConditionList;
  ints: ArrayOfInteger;
  sim: TSetIntegerMode;
  pl: TPlayer;
  expr: TExpression;
  bools: ArrayOfSwitchValue;

  procedure CheckEndOfLine;
  begin
    if index < ALine.Count then
      raise exception.Create('Expecting end of line but "' + ALine[index] + '" found');
  end;

begin
  for index := 0 to ALine.Count-1 do
    if CompareText(ALine[index],'Present') = 0 then
    begin
      if (AThreads <> []) and (AThreads <> [AMainThread]) then
        raise exception.Create('"Present" is only valid in the main thread');
      done := false;
      for i := 0 to AProg.Count-1 do
        if AProg[i] is TWaitForPresenceDefinedInstruction then
        begin
          done := true;
          break;
        end;
      if not done then
        AProg.Add(TWaitForPresenceDefinedInstruction.Create);
    end;

  index := 0;
  if PeekToken(ALine,index,'Sub') or PeekToken(ALine,index,'Function') then
    raise exception.Create('Nested procedures or functions not allowed');
  if PeekToken(ALine,index,'On') then
    raise exception.Create('Nested events not allowed');

  if TryToken(ALine,index,'Return') then
  begin
    if AProcId <> -1 then
    begin
      if IsIntegerType(Procedures[AProcId].ReturnType) then
      begin
        if TryIntegerConstant(AScope,ALine,index,intVal) then
        begin
          if (intVal < 0) or (intVal >= 1 shl Procedures[AProcId].ReturnBitCount) then
            raise exception.Create('Value out of bounds')
          else
            AProg.Add( TTransferIntegerInstruction.Create(intVal, itCopyIntoAccumulator) );
        end
        else
        begin
          expr := TryExpression(AScope,ALine,index,true);
          if expr.CanPutInAccumulator then
          begin
            expr.AddToProgramInAccumulator(AProg);
            AProg.Add(TTransferIntegerInstruction.Create((1 shl Procedures[AProcId].ReturnBitCount)-1, itLimitAccumulator));
          end else
          begin
            idxVar := ProcedureReturnVar(AProcId);
            expr.AddToProgram(AProg, IntVars[idxVar].Player,IntVars[idxVar].UnitType, simSetTo);
            AProg.Add(TTransferIntegerInstruction.Create( IntVars[idxVar].Player,IntVars[idxVar].UnitType, itCopyIntoAccumulator));
          end;
          expr.Free;
        end;
      end else
      if Procedures[AProcId].ReturnType = 'Boolean' then
      begin
        idxVar := GetBoolResultVar;
        sw := BoolVars[idxVar].Switch;
        if TryBoolean(AScope,ALine,index,boolVal) then
        begin
          AProg.Add( TSetSwitchInstruction.Create(sw, BoolToSwitch[boolVal]) );
        end else
        begin
          conds := ExpectConditions(AScope, ALine, index, AThreads, true);
          AProg.Add( TIfInstruction.Create(conds) );
          AProg.Add( TSetSwitchInstruction.Create(sw, svSet) );
          AProg.Add( TElseInstruction.Create );
          AProg.Add( TSetSwitchInstruction.Create(sw, svClear) );
          AProg.Add( TEndIfInstruction.Create );
        end;
      end;
    end;
    AProg.Add( TReturnInstruction.Create );
    CheckEndOfLine;
  end
  else
  if TryToken(ALine,index,'Exit') then
  begin
    if TryToken(ALine,index,'Sub') then
    begin
      if (AProcId = -1) and not AInSubMain then raise exception.Create('Not in a subroutine');
      if not AInSubMain and (Procedures[AProcId].ReturnType <> 'Void') then raise exception.Create('Currently in a function, not a subroutine');
      AProg.Add( TReturnInstruction.Create );
      CheckEndOfLine;
    end else
    if TryToken(ALine,index,'Function') then
    begin
      if AProcId = -1 then raise exception.Create('Not in a function');
      if Procedures[AProcId].ReturnType = 'Void' then raise exception.Create('Currently in a subroutine, not a function');
      raise exception.Create('Exit Function not allowed. Use Return to specify value');
    end else
      raise exception.Create('Unexpected instruction');
  end else
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
    conds := ExpectConditions(AScope,ALine,index,AThreads);
    if (conds.Count = 1) and (conds[0] is TAlwaysCondition) then
      raise exception.Create('Infinite loop not allowed. You can use an event "On True" instead though');
    AProg.Add(TWhileInstruction.Create(conds));
    CheckEndOfLine;
  end else
  if TryToken(ALine,index,'If') then
  begin
    conds := ExpectConditions(AScope,ALine,index,AThreads);
    AProg.Add(TIfInstruction.Create(conds));
    ExpectToken(ALine,index,'Then');
    CheckEndOfLine;
  end else
  if TryToken(ALine,index,'ElseIf') then
  begin
    conds := ExpectConditions(AScope,ALine,index,AThreads);
    AProg.Add(TElseIfInstruction.Create(conds));
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
    scalar := TryScalarVariable(AScope,ALine,index);
    if scalar.VarType <> svtNone then
    begin
      if TryToken(ALine,index,'=') then
      begin
        done := true;
        if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
        if scalar.ReadOnly then raise exception.Create('This value is read-only');
        case scalar.VarType of
        svtInteger: begin
            expr := TryExpression(AScope,ALine, index, true);
            if expr = nil then
              raise exception.Create('Unhandled case');
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
              conds := ExpectConditions(AScope,ALine,index,AThreads);
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
          if scalar.ReadOnly then raise exception.Create('This value is read-only');
          if scalar.VarType = svtInteger then
          begin
            expr := TryExpression(AScope,ALine,index, true);
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

    if not done then
    begin
      idxSound := TrySoundVariable(AScope,ALine,index);
      if idxSound <> -1 then
      begin
        ExpectToken(ALine,index,'.');
        ExpectToken(ALine,index,'Play');
        if TryToken(ALine,index,'(') then ExpectToken(ALine,index,')');

        AProg.Add(TPlayWAVInstruction.Create(SoundVars[idxSound].Filename, SoundVars[idxSound].DurationMs));
        done := true;
      end;
    end;

    if not done then
    begin
      idxArr := TryIntegerArray(AScope,ALine,index);
      if idxArr <> -1 then
      begin
        If TryToken(ALine,index,'+') or TryToken(ALine,index,'-') then
          assignOp := ALine[index-1] else assignOp := '';

        if TryToken(ALine,index,'=') then
        begin
          done := true;
          if IntArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');

          ints := ParseIntArray(AScope,ALine,index);
          if length(ints) <> IntArrays[idxArr].Size then
            raise exception.Create('Array size mismatch');
          CheckEndOfLine;
          if assignOp = '+' then sim := simAdd
          else if assignOp = '-' then sim := simSubtract
          else sim := simSetTo;
          for i := 0 to high(ints) do
            with IntVars[IntArrays[idxArr].Vars[i]] do
            AProg.Add(CreateSetIntegerInstruction(Player, UnitType, sim, ints[i]));
        end else
        begin
          if assignOp = '' then
            raise exception.Create('Expecting index with "(" or assignment')
            else raise exception.Create('Expecting "="');
        end;
      end;
    end;

    if not done and TryToken(ALine,index,'Present') then
    begin
      if TryToken(ALine,index,'=') then
        raise exception.Create('Array is readonly')
      else
        dec(index);
    end else
    if not done then
    begin
      idxArr := TryBooleanArray(AScope,ALine,index);
      if idxArr <> -1 then
      begin
        if TryToken(ALine,index,'=') then
        begin
          done := true;

          if BoolArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');
          if BoolArrays[idxArr].ReadOnly then raise exception.Create('Array is readonly');

          bools := ParseBoolArray(AScope,ALine,index);
          if length(bools) <> BoolArrays[idxArr].Size then
            raise exception.Create('Array size mismatch');
          CheckEndOfLine;
          for i := 0 to high(bools) do
            with BoolVars[BoolArrays[idxArr].Vars[i]] do
              AProg.Add(TSetSwitchInstruction.Create(Switch, bools[i]));
        end else
          dec(index);
      end;
    end;

    if not done then
    begin
      index := 0;
      pl := TryParsePlayer(AScope,ALine,index);
      if pl <> plNone then
      begin
        done := true;

        ExpectToken(ALine,index,'.');

        if (pl <> plCurrentPlayer) and TryToken(ALine,index,'Print') then
        begin
          if (AThreads <> []) and (AThreads <> [AMainThread]) then
            raise exception.Create('Printing for any player is only possible from main thread');

          ExpectToken(ALine,index,'(');
          text := ExpectStringConstant(AScope,ALine,index,true);
          ExpectToken(ALine,index,')');
          idxMsg := FindOrCreateMessage(text, [pl]);
          AProg.Add( TPrintForAnyPlayerInstruction.Create(idxMsg) );

        end else
        if not TryPlayerAction(AScope,AProg,ALine,index,pl, AThreads) then
        begin
          if index >= ALine.Count then
            raise exception.Create('Expecting action but end of line found')
          else
            raise exception.Create('Expecting action but "' + ALine[index] + '" found');
        end;
        CheckEndOfLine;
      end;
    end;

    index := 0;
    if TryNeutralAction(AScope,AProg,ALine,index, AThreads) then
    begin
      CheckEndOfLine;
      done := true;
    end;

    if TryPlayerAction(AScope,AProg,ALine,index,plCurrentPlayer, AThreads) then
    begin
      if AThreads = [] then
        raise exception.Create('You need to specify which players does the action ("Me" for main thread)');
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

      if IsReservedWord(name) then
        raise exception.Create('Unexpected reserved word');

      if (name <> '') and
        ((index = ALine.Count) or
         PeekToken(ALine, index, '(')) then
      begin
        params := TStringList.Create;
        try
          if TryToken(ALine,index,'(') then
          begin
            while not TryToken(ALine,index,')') do
            begin
              if params.Count > 0 then ExpectToken(ALine,index,',');
              if TryToken(ALine,index,',') then
                raise exception.Create('Empty parameters not allowed');
              if index < ALine.Count then
                params.Add(ALine[index])
              else
                raise exception.Create('Parameter expected but end of line found');

              inc(index);
            end;
          end;
          CheckEndOfLine;

          AProg.Add(TCallInstruction.Create(name, params));
          params := nil;
        finally
          params.Free;
        end;
      end else
      if scalar.VarType = svtNone then
      begin
        if name <> '' then
          raise exception.Create('Unknown variable "' + name + '"')
      end
      else
        raise exception.Create('Expecting assignment');
    end;
  end;
end;

end.

