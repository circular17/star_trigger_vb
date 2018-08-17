unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

procedure CreateTriggers(AMainThread: TPlayer; ASourceCode: string = '');

implementation

uses utriggercode, uarithmetic, ureadprog, uinstructions, uvariables, uparsevb,
  uexpressions, utriggerinstructions, utriggerconditions, utrigedittypes,
  uprocedures;

var
  HyperWaitVar: integer;

procedure ConfigureHyperWait;
begin
  if HyperTriggersOption then
  begin
    if HyperWaitVar = -1 then
    begin
      HyperWaitVar := IntArrayIndexOf(GlobalScope, '_hyperwait');
      if HyperWaitVar = -1 then
        HyperWaitVar := CreateIntArray(GlobalScope,'_hyperwait', MaxTriggerPlayers, [], 24);
    end;
  end;
end;

var
  MessageSysIP: integer;
  GlobalExprTempVarInt: integer;

function GetGlobalExprTempVarInt(ABitCount: integer): integer;
begin
  if GlobalExprTempVarInt = -1 then
    GlobalExprTempVarInt:= AllocateTempInt(ABitCount)
  else
  begin
    if IntVars[GlobalExprTempVarInt].BitCount < ABitCount then
      IntVars[GlobalExprTempVarInt].BitCount := ABitCount;
  end;
  result := GlobalExprTempVarInt;
end;

procedure WriteMessageTriggers(AMainThread: TPlayer);
var
  i: Integer;
  proc: TInstructionList;
  condSysIP, condSysParam: TCondition;

begin
  If MessageSysIP = -1 then exit;
  condSysIP := CheckSysIP( MessageSysIP, AMainThread );
  for i := 0 to MessageCount-1 do
  begin
    proc := TInstructionList.Create;
    proc.Add( TDisplayTextMessageInstruction.Create(True, Messages[i].Text) );
    condSysParam := CheckSysParam( i, AMainThread );
    WriteProg(Messages[i].Players, [condSysIP, condSysParam], proc, -1, -1, True);
    condSysParam.Free;
    proc.FreeAll;
  end;

  proc := TInstructionList.Create;
  proc.Add( SetNextSysIP(0) );
  WriteProg([AMainThread], [condSysIP], proc, -1,-1, true);
  condSysIP.Free;
  proc.FreeAll;
end;

procedure ExpandInstructions(AProg: TInstructionList; AInProc: integer; APlayers: TPlayers; AExpanded: TInstructionList; AEndIP: integer);

  function AddWaitCondition(AConditions: TConditionList; ANextIP: integer): TWaitConditionInstruction;
  var
    arithm: TArithmeticCondition;
    tempInt: Integer;
    tempExpand: TInstructionList;
  begin
    if (AConditions.Count = 1) and (AConditions[0] is TArithmeticCondition) then
    begin
      arithm := TArithmeticCondition(AConditions[0]);
      if arithm.Expression.CanPutInAccumulator then
      begin
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgramInAccumulator(tempExpand);
        ExpandInstructions(tempExpand, AInProc, APlayers, AExpanded, -1);
        tempExpand.FreeAll;

        result := TWaitConditionInstruction.Create(CompareAccumulator(arithm.CompareMode,arithm.CompareValue), ANextIP);
        AExpanded.Add(result);
      end
      else
      begin
        if AInProc = -1 then
          tempInt := GetGlobalExprTempVarInt(arithm.GetBitCount)
        else
          tempInt := GetProcedureExprTempVarInt(AInProc, arithm.GetBitCount);
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgram(tempExpand, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
        ExpandInstructions(tempExpand, AInProc, APlayers, AExpanded, -1);
        tempExpand.FreeAll;
        result := TWaitConditionInstruction.Create(CreateIntegerCondition(IntVars[tempInt].Player,IntVars[tempInt].UnitType, arithm.CompareMode,arithm.CompareValue), ANextIP);
        AExpanded.Add(result);
      end;
    end else
    if (AConditions.Count = 1) and (AConditions[0] is TAndCondition) then
    begin
      result := AddWaitCondition(TAndCondition(AConditions[0]).Conditions, ANextIP);
      exit;
    end else
    begin
      if AConditions.IsComputed then
      begin
        if AInProc = -1 then
          tempInt := GetGlobalExprTempVarInt(8)
        else
          tempInt := GetProcedureExprTempVarInt(AInProc, 8);
        tempExpand := TInstructionList.Create;
        AConditions.Compute(tempExpand, IntVars[tempInt].Player,IntVars[tempInt].UnitType);
        ExpandInstructions(tempExpand, AInProc, APlayers, AExpanded, -1);
        tempExpand.FreeAll;
        result := TWaitConditionInstruction.Create(CreateIntegerCondition(IntVars[tempInt].Player,IntVars[tempInt].UnitType, icmAtLeast, 1), ANextIP);
        AExpanded.Add(result);
      end else
      begin
        result := TWaitConditionInstruction.Create(AConditions.Duplicate, ANextIP);
        AExpanded.Add(result);
      end;
    end;
  end;

var
  i, expo, j, nextIP, procIdx: Integer;
  rndInstr: TRandomizeIntegerInstruction;
  switches: array of integer;
  call: TCallInstruction;
  nesting, elseIndex: integer;
  startWhileIP, k, varIdx: integer;
  ifInstr: TIfInstruction;
  waitInstr: TWaitConditionInstruction;
  splitInstr: TSplitInstruction;
  thenPart,elsePart: TInstructionList;
  notCond: TNotCondition;
  tempExpand, tempPart: TInstructionList;
  doAs: TDoAsInstruction;
  pl: TPlayer;
  endDoAs: TEndDoAsInstruction;
  endDoIP: integer;
  err: string;

begin
  err := '';
  i := -1;
  endDoIP := -1;
  while i < AProg.Count-1 do
  begin
    inc(i);

    if AProg[i] is TTransferIntegerInstruction then
    begin
      tempExpand := TInstructionList.Create;
      ExpandIntegerTransfer(TTransferIntegerInstruction(AProg[i]), tempExpand);
      ExpandInstructions(tempExpand, AInProc, APlayers, AExpanded, -1);
      tempExpand.FreeAll;
      Continue;
    end else
    if AProg[i] is TPrintForAnyPlayerInstruction then
    begin
       if MessageSysIP = -1 then MessageSysIP := NewSysIP;
       AExpanded.Add( SetNextSysIP( MessageSysIP ) );
       AExpanded.Add( SetSysParam( TPrintForAnyPlayerInstruction(AProg[i]).Msg ) );
       AExpanded.Add( TWaitConditionInstruction.Create( CheckSysIP(0), NewIP ) );
       Continue;
    end else
    if AProg[i] is TWaitForPresenceDefinedInstruction then
    begin
       varIdx := GetPlayerPresenceDefinedVar;
       AExpanded.Add(TWaitConditionInstruction.Create(TSwitchCondition.Create(BoolVars[varIdx].Switch, true), NewIP));
       Continue;
    end else
    if AProg[i] is TDoAsInstruction then
    begin
      doAs := TDoAsInstruction(AProg[i]);
      if doAs.Players - APlayers <> [] then
      begin
        varIdx := GetStopEventBoolVar;
        AExpanded.Add( TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svSet) );
        AExpanded.Add( TWaitForPlayersInstruction.Create( doAs.Players - APlayers, False ) );
      end;
      nextIP := NewIP;
      for pl := low(TPlayer) to high(TPlayer) do
        if pl in (doAs.Players - APlayers) then
          AExpanded.Add( SetNextIP(nextIP, pl));

      endDoIP := NewIP;
      if APlayers <= doAs.Players then
        AExpanded.Add( TSplitInstruction.Create(nextIP, nextIP, doAs.Players) )
      else
        AExpanded.Add( TSplitInstruction.Create(nextIP, EndDoIP, doAs.Players) );
      continue;
    end else
    if AProg[i] is TEndDoAsInstruction then
    begin
      endDoAs := TEndDoAsInstruction(AProg[i]);
      if endDoAs.Players - APlayers <> [] then
      begin
        AExpanded.Add( TDropThreadInstruction.Create(0, EndDoIP, endDoAs.Players - APlayers, APlayers) );
        AExpanded.Add( TWaitForPlayersInstruction.Create( endDoAs.Players - APlayers, True ) );
        AExpanded.Add( TSetSwitchInstruction.Create(BoolVars[GetStopEventBoolVar].Switch, svSet) );
      end;
      continue;
    end else
    if HyperTriggersOption and (AProg[i] is TWaitInstruction) then
    begin
      ConfigureHyperWait;

      AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSetTo, TWaitInstruction(AProg[i]).DelayMs));

      startWhileIP:= NewIP;
      AExpanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));

      waitInstr := TWaitConditionInstruction.Create(CreateIntegerCondition(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, icmAtLeast, 1), NewIP);
      AExpanded.Add(waitInstr);

      AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSubtract, 140));

      splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);

      AExpanded.Add(splitInstr);
      AExpanded.Add(TChangeIPInstruction.Create(NewIP, -1));

      continue;
    end else
    if AProg[i] is TWhileInstruction then
    begin
      nesting := 1;
      for j := i+1 to AProg.Count-1 do
      begin
        if AProg[j] is TWhileInstruction then inc(nesting) else
        if AProg[j] is TEndWhileInstruction then
        begin
          dec(nesting);
          if nesting = 0 then
          begin
            startWhileIP:= NewIP;
            AExpanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));
            waitInstr := AddWaitCondition(TWhileInstruction(AProg[i]).Conditions, NewIP);

            tempExpand := TInstructionList.Create;
            for k := i+1 to j-1 do
              tempExpand.Add(AProg[k].Duplicate);
            ExpandInstructions(tempExpand, AInProc, APlayers, AExpanded, startWhileIP);
            tempExpand.FreeAll;

            splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);
            AExpanded.Add(splitInstr);
            i := j;
            break;
          end;
        end;
      end;
      if nesting <> 0 then
      begin
        err := 'The number of End While do not match the number of While';
        break;
      end;
      continue;
    end else
    if AProg[i] is TIfInstruction then
    begin
      nesting := 1;
      elseIndex := -1;
      for j := i+1 to AProg.Count-1 do
      begin
        if AProg[j] is TIfInstruction then inc(nesting) else
        if (AProg[j] is TElseInstruction) and (nesting = 1) then
        begin
          if elseIndex <> -1 then
          begin
            err := 'Only one Else is allowed for one If statement';
            break;
          end;
          elseIndex:= j;
        end else
        if (AProg[j] is TElseIfInstruction) and (nesting = 1) then
        begin
          if elseIndex = -1 then elseIndex := j
          else if AProg[elseIndex] is TElseInstruction then
          begin
            err := 'ElseIf is not allowed after an Else instruction';
            break;
          end;
        end else
        if AProg[j] is TEndIfInstruction then
        begin
          dec(nesting);
          if nesting = 0 then
          begin
            thenPart := TInstructionList.Create;
            elsePart := TInstructionList.Create;

            if elseIndex = -1 then
            begin
              for k := i+1 to j-1 do
                thenPart.Add(AProg[k].Duplicate);
            end else
            begin
              for k := i+1 to elseIndex-1 do
                thenPart.Add(AProg[k].Duplicate);
              for k := elseIndex+1 to j-1 do
                elsePart.Add(AProg[k].Duplicate);

              if AProg[elseIndex] is TElseIfInstruction then
              begin
                elsePart.Insert(0, TIfInstruction.Create(TElseIfInstruction(AProg[elseIndex]).Conditions.Duplicate));
                elsePart.Add(TEndIfInstruction.Create);
              end;
            end;

            ifInstr := TIfInstruction(AProg[i]);
            nextIP := NewIP;
            //if condition is a Not, it can be done be swapping Then part and Else part
            if (ifInstr.Conditions.Count = 1) and (ifInstr.Conditions[0] is TNotCondition) then
            begin
              tempPart := thenPart;
              thenPart := elsePart;
              elsePart := tempPart;

              notCond := TNotCondition(ifInstr.Conditions[0]);
              AddWaitCondition(notCond.Conditions, nextIP);
            end
            else
              AddWaitCondition(ifInstr.Conditions, nextIP);

            ExpandInstructions(thenPart, AInProc, APlayers, AExpanded, -1);
            thenPart.FreeAll;

            splitInstr := TSplitInstruction.Create(nextIP,-1);
            AExpanded.Add(splitInstr);

            if (j = AProg.Count-1) and (AEndIP <> -1) then
            begin
              splitInstr.EndIP:= AEndIP;
              ExpandInstructions(elsePart, AInProc, APlayers, AExpanded, AEndIP);
              elsePart.FreeAll;
            end
            else
            begin
              nextIP := NewIP;
              splitInstr.EndIP:= nextIP;
              ExpandInstructions(elsePart, AInProc, APlayers, AExpanded, nextIP);
              elsePart.FreeAll;
              AExpanded.Add(TChangeIPInstruction.Create(nextIP, 0));
            end;

            i := j;
            break;
          end;
        end;
      end;
      if err<>'' then break;
      if nesting <> 0 then
      begin
        err := 'The number of End If do not match the number of If';
        break;
      end;
      continue;
    end else
    if AProg[i] is TElseInstruction then
    begin
      err := 'Else instruction does not match an If instruction';
      break;
    end else
    if AProg[i] is TEndIfInstruction then
    begin
      err := 'End If instruction does not match an If instruction';
      break;
    end else
    if AProg[i] is TRandomizeIntegerInstruction then
    begin
      rndInstr := TRandomizeIntegerInstruction(AProg[i]);
      AExpanded.Add(CreateSetIntegerInstruction(rndInstr.Player, rndInstr.UnitType, simSetTo, 0));

      //randomize bools
      expo := GetExponentOf2(rndInstr.Range);
      setlength(switches, expo);
      for j := 0 to expo-1 do
      begin
        switches[j] := AllocateTempBool;
        AExpanded.Add( TSetSwitchInstruction.Create(switches[j], svRandomize) );
      end;

      //add powers of 2
      for j := 0 to high(switches) do
        AExpanded.Add( TFastIfInstruction.Create([TSwitchCondition.Create(switches[j], true)],
                      [CreateSetIntegerInstruction(rndInstr.Player, rndInstr.UnitType, simAdd, 1 shl j)]) );

      for j := 0 to expo-1 do
        ReleaseTempBool(switches[j]);

      continue;
    end else
    if AProg[i] is TCallInstruction then
    begin
      call := TCallInstruction(AProg[i]);

      procIdx := ProcedureIndexOf(call.Name, length(call.Params));
      if procIdx = -1 then
      begin
        err := 'Procedure not found "' + call.Name + '" with ' + Inttostr(length(call.Params)) + ' parameter(s)';
        break;
      end;

      if (call.ReturnType <> 'Void') and not
      ((IsIntegerType(Procedures[procIdx].ReturnType) and IsIntegerType(call.ReturnType))
      or (Procedures[procIdx].ReturnType = call.ReturnType)) then
      begin
        err := 'Expecting ' + call.ReturnType + ' return type but ' + Procedures[procIdx].ReturnType + ' found';
        break;
      end;

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;

      If AInProc <> -1 then
      begin
        if Procedures[AInProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AInProc].Calls.Add(procIdx);
      end;

      nextIP := NewIP;
      AddSysCall(AExpanded, nextIP, Procedures[procIdx].StartIP);

      continue;
    end else
    if AProg[i] is TReturnInstruction then
    begin
      if AInProc<>-1 then AddSysReturn(AExpanded)
      else
        AExpanded.Add( TSplitInstruction.Create(NewIP, -1));
      continue;
    end;
    AExpanded.Add(AProg[i].Duplicate);
  end;

  if err <> '' then
    raise exception.Create(err);
end;

procedure WriteHyperTriggers;
var
  actions: array of TInstruction;
  i: Integer;
begin
  if not HyperTriggersOption then exit;

  setlength(actions,63);
  actions[0] := TCommentInstruction.Create('Hyper triggers');
  for i := 1 to high(actions) do
    actions[i] := TWaitInstruction.Create(15);

  for i := 1 to 4 do
    WriteTrigger([plAllPlayers], [], actions, -1, true);
  for i := 0 to high(actions) do actions[i].Free;
end;

procedure WritePlayerPresenceTopTrigger({%H-}AMainThread: TPlayer);
var
  proc: TInstructionList;
  pl: TPlayer;
begin
  //at the begining of the cycle, set presence
  proc := TInstructionList.Create;
  for pl := plPlayer1 to plPlayer8 do
    if IsPlayerPresenceUsed(pl) then
    begin
      proc.Add(TSetSwitchInstruction.Create(BoolVars[GetPlayerPresenceBoolVar(pl)].Switch, svSet));
      WriteProg([pl], [], proc, -1, -1, true);
      proc[0].Free;
      proc.Clear;
    end;
  proc.Free;
end;

procedure WritePlayerPresenceBottomTrigger(AMainThread: TPlayer);
var
  proc: TInstructionList;
  pl: TPlayer;
begin
  //at the end of main thread cycle, clear presence flags (they will be set until next main thread cycle)
  proc := TInstructionList.Create;
  for pl := plPlayer1 to plPlayer8 do
    if IsPlayerPresenceUsed(pl) then
      proc.Add(TSetSwitchInstruction.Create(BoolVars[GetPlayerPresenceBoolVar(pl)].Switch, svClear));
  if proc.Count > 0 then
  begin
    proc.Add(TSetSwitchInstruction.Create(BoolVars[GetPlayerPresenceDefinedVar].Switch, svSet)); //will be until next cycle
    WriteProg([AMainThread], [], proc, -1, -1, true);
  end;
  proc.FreeAll;
end;

procedure WriteTriggerForSourceCode(ASourceCode: string);
var
  never: TNeverCondition;
begin
  never := TNeverCondition.Create;
  WriteTrigger([plNeutralPlayers], [never], [TCommentInstruction.Create('BroodBasic source code'), TCommentInstruction.Create(ASourceCode)], -1, false);
  never.Free;
end;

procedure CreateTriggers(AMainThread: TPlayer; ASourceCode: string = '');
var
  i, EndIP, tempInt, j: Integer;
  allProcDone: Boolean;
  players: TPlayers;
  noSysIP: TCondition;
  proc : TInstructionList;
  computedConds, nonComputedConds: TConditionList;
  tempCond: TCondition;
  MainProgExpanded: TInstructionList;
  EventsExpanded, ProceduresExpanded: array of TInstructionList;

begin
  InitTriggerCode;
  InitArithmetic;

  HyperWaitVar := -1;
  MessageSysIP := -1;
  GlobalExprTempVarInt:= -1;
  EndIP := 0;

  MainProgExpanded := nil;
  setlength(EventsExpanded, EventCount);
  setlength(ProceduresExpanded, ProcedureCount);

  try
    MainProgExpanded := TInstructionList.Create;
    ExpandInstructions(MainProg, -1, [AMainThread], MainProgExpanded, EndIP);

    for i := 0 to EventCount-1 do
    begin
      EventsExpanded[i] := TInstructionList.Create;
      ExpandInstructions(Events[i].Instructions, -1, Events[i].Players, EventsExpanded[i], EndIP);
    end;

    repeat
      allProcDone := true;
      for i := 0 to ProcedureCount-1 do
        if (Procedures[i].StartIP <> -1) and not Procedures[i].Expanded then
        begin
          allProcDone:= false;
          ProceduresExpanded[i] := TInstructionList.Create;
          ExpandInstructions(Procedures[i].Instructions, i, [plAllPlayers], ProceduresExpanded[i], -1);
          Procedures[i].Expanded := true;
        end;
    until allProcDone;

    if ASourceCode <> '' then
      WriteTriggerForSourceCode(ASourceCode);

    WritePlayerPresenceTopTrigger(AMainThread);
    WriteMessageTriggers(AMainThread);

    WriteProg([AMainThread], [], MainProgExpanded, -1, EndIP, False);

    noSysIP := CheckSysIP(0);
    for i := 0 to ProcedureCount-1 do
      if Procedures[i].StartIP <> -1 then
        WriteProg([plAllPlayers], [noSysIP], ProceduresExpanded[i], Procedures[i].StartIP, EndIP, true);
    noSysIP.Free;

    for i := 0 to EventCount-1 do
    begin
      players := Events[i].Players;
      if players = [] then players := [AMainThread];

      if Events[i].Conditions.IsComputed then
      begin
        tempInt := GetGlobalExprTempVarInt(8);
        proc := TInstructionList.Create;
        nonComputedConds := TConditionList.Create;

        computedConds := TConditionList.Create;
        for j := 0 to Events[i].Conditions.Count-1 do
          if Events[i].Conditions[j].IsComputed then
            computedConds.Add(Events[i].Conditions[j])
          else
            nonComputedConds.Add(Events[i].Conditions[j]);
        computedConds.Compute(proc, IntVars[tempInt].Player, IntVars[tempInt].UnitType);
        WriteProg(players, [], proc, EndIP, EndIP, Events[i].Preserve);
        computedConds.Free;
        proc.Free;

        tempCond := CreateIntegerCondition(IntVars[tempInt].Player, IntVars[tempInt].UnitType, icmAtLeast, 1);
        nonComputedConds.Add(tempCond);
        WriteProg(players, nonComputedConds, EventsExpanded[i], EndIP, EndIP, Events[i].Preserve);
        tempCond.Free;
        nonComputedConds.Free;
      end else
        WriteProg(players, Events[i].Conditions, EventsExpanded[i], EndIP, EndIP, Events[i].Preserve);
    end;

    //write generated code at the end of the file
    WriteStackTriggers;
    WriteArithmeticTriggers;
    WritePlayerPresenceBottomTrigger(AMainThread);

    //it is recommended to put hyper CompiledTriggers at the end
    WriteHyperTriggers;

  finally
    MainProgExpanded.FreeAll;
    for i := 0 to high(EventsExpanded) do
      EventsExpanded[i].FreeAll;
    for i := 0 to high(ProceduresExpanded) do
      ProceduresExpanded[i].FreeAll;
  end;

  if CompiledTriggers.Count > 65534 then
    raise exception.Create('Too many triggers');
end;

end.

