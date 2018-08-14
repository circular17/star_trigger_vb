unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

procedure CreateTriggers(AMainThread: TPlayer; ASourceCode: string = '');

implementation

uses utriggercode, uarithmetic, ureadprog, uinstructions, uvariables, uparsevb,
  uexpressions, utriggerinstructions, utriggerconditions, utrigedittypes;

var
  HyperWaitVar: integer;

procedure ConfigureHyperWait;
begin
  if HyperTriggers then
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

procedure ExpandInstructions(AProg: TInstructionList; AInProc: integer; APlayers: TPlayers);
var
  expanded: TInstructionList;

  function AddWaitCondition(AConditions: TConditionList; ANextIP: integer): TWaitConditionInstruction;
  var
    arithm: TArithmeticCondition;
    tempInt, k: Integer;
    tempExpand: TInstructionList;
  begin
    if (AConditions.Count = 1) and (AConditions[0] is TArithmeticCondition) then
    begin
      arithm := TArithmeticCondition(AConditions[0]);
      if arithm.Expression.CanPutInAccumulator then
      begin
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgramInAccumulator(tempExpand);
        ExpandInstructions(tempExpand, AInProc, APlayers);
        for k := 0 to tempExpand.Count-1 do
          expanded.Add(tempExpand[k]);
        tempExpand.Free;

        result := TWaitConditionInstruction.Create(CompareAccumulator(arithm.CompareMode,arithm.CompareValue), ANextIP);
        expanded.Add(result);
      end
      else
      begin
        if AInProc = -1 then
          tempInt := GetGlobalExprTempVarInt(arithm.GetBitCount)
        else
          tempInt := GetProcedureExprTempVarInt(AInProc, arithm.GetBitCount);
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgram(tempExpand, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
        ExpandInstructions(tempExpand, AInProc, APlayers);
        for k := 0 to tempExpand.Count-1 do
          expanded.Add(tempExpand[k]);
        tempExpand.Free;
        result := TWaitConditionInstruction.Create(CreateIntegerCondition(IntVars[tempInt].Player,IntVars[tempInt].UnitType, arithm.CompareMode,arithm.CompareValue), ANextIP);
        expanded.Add(result);
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
        ExpandInstructions(tempExpand, AInProc, APlayers);
        for k := 0 to tempExpand.Count-1 do
          expanded.Add(tempExpand[k]);
        tempExpand.Free;
        result := TWaitConditionInstruction.Create(CreateIntegerCondition(IntVars[tempInt].Player,IntVars[tempInt].UnitType, icmAtLeast, 1), ANextIP);
        expanded.Add(result);
      end else
      begin
        result := TWaitConditionInstruction.Create(AConditions, ANextIP);
        expanded.Add(result);
      end;
    end;
  end;

var
  i, expo, j, nextIP, procIdx: Integer;
  rndInstr: TRandomizeIntegerInstruction;
  switches: array of integer;
  call: TCallInstruction;
  ret: TReturnInstruction;
  nesting, elseIndex, endIfIndex: integer;
  startWhileIP, k, varIdx: integer;
  ifInstr: TIfInstruction;
  waitInstr: TWaitConditionInstruction;
  splitInstr: TSplitInstruction;
  thenPart,elsePart: TInstructionList;
  notCond: TNotCondition;
  tempExpand: TInstructionList;
  doAs: TDoAsInstruction;
  pl: TPlayer;
  endDoAs: TEndDoAsInstruction;
  endDoIP: integer;

begin
  expanded := TInstructionList.Create;
  i := -1;
  endDoIP := -1;
  while i < AProg.Count-1 do
  begin
    inc(i);

    if AProg[i] is TTransferIntegerInstruction then
    begin
      tempExpand := TInstructionList.Create;
      ExpandIntegerTransfer(TTransferIntegerInstruction(AProg[i]), tempExpand);
      AProg[i].Free;
      ExpandInstructions(tempExpand, AInProc, APlayers);
      for j := 0 to tempExpand.Count-1 do
        expanded.Add(tempExpand[j]);
      tempExpand.Free;
      Continue;
    end else
    if AProg[i] is TPrintForAnyPlayerInstruction then
    begin
       if MessageSysIP = -1 then MessageSysIP := NewSysIP;
       expanded.Add( SetNextSysIP( MessageSysIP ) );
       expanded.Add( SetSysParam( TPrintForAnyPlayerInstruction(AProg[i]).Msg ) );
       expanded.Add( TWaitConditionInstruction.Create( CheckSysIP(0), NewIP ) );
       AProg[i].Free;
       Continue;
    end else
    if AProg[i] is TWaitForPresenceDefinedInstruction then
    begin
       AProg[i].Free;
       varIdx := GetPlayerPresenceDefinedVar;
       expanded.Add(TWaitConditionInstruction.Create(TSwitchCondition.Create(BoolVars[varIdx].Switch, true), NewIP));
       Continue;
    end else
    if AProg[i] is TDoAsInstruction then
    begin
      doAs := TDoAsInstruction(AProg[i]);
      if doAs.Players - APlayers <> [] then
      begin
        varIdx := GetStopEventBoolVar;
        expanded.Add( TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svSet) );
        expanded.Add( TWaitForPlayersInstruction.Create( doAs.Players - APlayers, False ) );
      end;
      nextIP := NewIP;
      for pl := low(TPlayer) to high(TPlayer) do
        if pl in (doAs.Players - APlayers) then
          expanded.Add( SetNextIP(nextIP, pl));

      endDoIP := NewIP;
      if APlayers <= doAs.Players then
        expanded.Add( TSplitInstruction.Create(nextIP, nextIP, doAs.Players) )
      else
        expanded.Add( TSplitInstruction.Create(nextIP, EndDoIP, doAs.Players) );
      AProg[i].Free;
      continue;
    end else
    if AProg[i] is TEndDoAsInstruction then
    begin
      endDoAs := TEndDoAsInstruction(AProg[i]);
      if endDoAs.Players - APlayers <> [] then
      begin
        expanded.Add( TDropThreadInstruction.Create(0, EndDoIP, endDoAs.Players - APlayers, APlayers) );
        expanded.Add( TWaitForPlayersInstruction.Create( endDoAs.Players - APlayers, True ) );
        expanded.Add( TSetSwitchInstruction.Create(BoolVars[GetStopEventBoolVar].Switch, svSet) );
      end;
      AProg[i].Free;
      continue;
    end else
    if HyperTriggers and (AProg[i] is TWaitInstruction) then
    begin
      ConfigureHyperWait;

      expanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSetTo, TWaitInstruction(AProg[i]).DelayMs));
      AProg[i].Free;

      startWhileIP:= NewIP;
      expanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));

      waitInstr := TWaitConditionInstruction.Create(CreateIntegerCondition(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, icmAtLeast, 1), NewIP);
      expanded.Add(waitInstr);

      expanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSubtract, 140));

      splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);

      expanded.Add(splitInstr);
      expanded.Add(TChangeIPInstruction.Create(NewIP, -1));

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
            expanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));
            waitInstr := AddWaitCondition(TWhileInstruction(AProg[i]).Conditions, NewIP);
            TWhileInstruction(AProg[i]).Conditions := nil;

            AProg[j].Free;
            splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);
            AProg[j] := splitInstr;

            AProg.Insert(j+1, TChangeIPInstruction.Create(NewIP, -1) );
            break;
          end;
        end;
      end;
      if nesting <> 0 then raise exception.Create('The number of End While do not match the number of While');
      continue;
    end else
    if AProg[i] is TIfInstruction then
    begin
      nesting := 1;
      elseIndex := -1;
      for j := i+1 to AProg.Count-1 do
      begin
        if AProg[j] is TIfInstruction then inc(nesting) else
        if (AProg[j] is TElseInstruction) and (nesting = 1) then elseIndex:= j else
        if AProg[j] is TEndIfInstruction then
        begin
          dec(nesting);
          if nesting = 0 then
          begin
            if elseIndex = -1 then
            begin
              splitInstr := TSplitInstruction.Create(-1,-1);
              AProg.Insert(j, splitInstr);
              elseIndex := j;
              endIfIndex := j+1;
            end else
            begin
              AProg[elseIndex].Free;
              splitInstr := TSplitInstruction.Create(-1,-1);
              AProg[elseIndex] := splitInstr;
              endIfIndex := j;
            end;

            ifInstr := TIfInstruction(AProg[i]);
            //if condition is a Not, it can be done be swapping Then part and Else part
            if (ifInstr.Conditions.Count = 1) and (ifInstr.Conditions[0] is TNotCondition) then
            begin
              notCond := TNotCondition(ifInstr.Conditions[0]);
              ifInstr.Conditions := notCond.Conditions;
              notCond.Conditions := TConditionList.Create;
              notCond.Free;

              thenPart := TInstructionList.Create;
              for k := i+1 to elseIndex-1 do thenPart.Add(AProg[k]);
              elsePart := TInstructionList.Create;
              for k := elseIndex+1 to endIfIndex-1 do elsePart.Add(AProg[k]);
              elseIndex := i+1+elsePart.Count;
              for k := 0 to elsePart.Count-1 do AProg[i+1+k] := elsePart[k];
              AProg[elseIndex] := splitInstr;
              for k := 0 to thenPart.Count-1 do AProg[elseIndex+1+k] := thenPart[k];
              thenPart.Free;
              elsePart.Free;
            end;

            nextIP := NewIP;
            AddWaitCondition(ifInstr.Conditions, nextIP);
            ifInstr.Conditions := nil;
            ifInstr.Free;

            TSplitInstruction(AProg[elseIndex]).ResumeIP:= nextIP;

            nextIP := NewIP;
            TSplitInstruction(AProg[elseIndex]).EndIP:= nextIP;
            AProg[endIfIndex].Free;
            AProg[endIfIndex] := TChangeIPInstruction.Create(nextIP, 0);
            break;
          end;
        end;
      end;
      if nesting <> 0 then raise exception.Create('The number of End If do not match the number of If');
      continue;
    end else
    if AProg[i] is TElseInstruction then
    begin
      raise exception.Create('Else instruction does not match an If instruction');
    end else
    if AProg[i] is TEndIfInstruction then
    begin
      raise exception.Create('End If instruction does not match an If instruction');
    end else
    if AProg[i] is TRandomizeIntegerInstruction then
    begin
      rndInstr := TRandomizeIntegerInstruction(AProg[i]);
      expanded.Add(CreateSetIntegerInstruction(rndInstr.Player, rndInstr.UnitType, simSetTo, 0));

      //randomize bools
      expo := GetExponentOf2(rndInstr.Range);
      setlength(switches, expo);
      for j := 0 to expo-1 do
      begin
        switches[j] := AllocateTempBool;
        expanded.Add( TSetSwitchInstruction.Create(switches[j], svRandomize) );
      end;

      //add powers of 2
      for j := 0 to high(switches) do
        expanded.Add( TFastIfInstruction.Create([TSwitchCondition.Create(switches[j], true)],
                      [CreateSetIntegerInstruction(rndInstr.Player, rndInstr.UnitType, simAdd, 1 shl j)]) );

      for j := 0 to expo-1 do
        ReleaseTempBool(switches[j]);

      rndInstr.Free;
      continue;
    end else
    if AProg[i] is TCallInstruction then
    begin
      call := TCallInstruction(AProg[i]);

      procIdx := ProcedureIndexOf(call.Name, length(call.Params));
      if procIdx = -1 then
        raise Exception.Create('Procedure not found "' + call.Name + '" with ' + Inttostr(length(call.Params)) + ' parameter(s)');

      if (call.ReturnType <> 'Void') and not
      ((IsIntegerType(Procedures[procIdx].ReturnType) and IsIntegerType(call.ReturnType))
      or (Procedures[procIdx].ReturnType = call.ReturnType)) then
        raise exception.Create('Expecting ' + call.ReturnType + ' return type but ' + Procedures[procIdx].ReturnType + ' found');

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;

      If AInProc <> -1 then
      begin
        if Procedures[AInProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AInProc].Calls.Add(procIdx);
      end;

      nextIP := NewIP;
      AddSysCall(expanded, nextIP, Procedures[procIdx].StartIP);

      call.Free;
      continue;
    end else
    if AProg[i] is TReturnInstruction then
    begin
      ret := TReturnInstruction(AProg[i]);
      if AInProc<>-1 then AddSysReturn(expanded)
      else
        expanded.Add( TSplitInstruction.Create(NewIP, -1));
      ret.Free;
      continue;
    end;
    expanded.Add(AProg[i]);
  end;

  AProg.Clear;
  for i := 0 to expanded.Count-1 do
    AProg.Add(expanded[i]);
  expanded.Free;
end;

procedure WriteHyperTriggers;
var
  actions: array of TInstruction;
  i: Integer;
begin
  if not HyperTriggers then exit;

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
  proc.Free;
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

begin
  InitTriggerCode;
  InitArithmetic;

  HyperWaitVar := -1;
  MessageSysIP := -1;
  GlobalExprTempVarInt:= -1;
  EndIP := 0;

  ExpandInstructions(MainProg, -1, [AMainThread]);

  for i := 0 to EventCount-1 do
    ExpandInstructions(Events[i].Instructions, -1, Events[i].Players);

  repeat
    allProcDone := true;
    for i := 0 to ProcedureCount-1 do
      if (Procedures[i].StartIP <> -1) and not Procedures[i].Expanded then
      begin
        allProcDone:= false;

        ExpandInstructions(Procedures[i].Instructions, i, [plAllPlayers]);
        Procedures[i].Expanded := true;
      end;
  until allProcDone;

  if ASourceCode <> '' then
    WriteTriggerForSourceCode(ASourceCode);

  WritePlayerPresenceTopTrigger(AMainThread);
  WriteMessageTriggers(AMainThread);

  WriteProg([AMainThread], [], MainProg, -1, EndIP, False);

  noSysIP := CheckSysIP(0);
  for i := 0 to ProcedureCount-1 do
    if Procedures[i].StartIP <> -1 then
      WriteProg([plAllPlayers], [noSysIP], Procedures[i].Instructions, Procedures[i].StartIP, EndIP, true);
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
      WriteProg(players, nonComputedConds, Events[i].Instructions, EndIP, EndIP, Events[i].Preserve);
      tempCond.Free;
      nonComputedConds.Free;
    end else
      WriteProg(players, Events[i].Conditions, Events[i].Instructions, EndIP, EndIP, Events[i].Preserve);
  end;

  //write generated code at the end of the file
  WriteStackTriggers;
  WriteArithmeticTriggers;
  WritePlayerPresenceBottomTrigger(AMainThread);

  //it is recommended to put hyper triggers at the end
  WriteHyperTriggers
end;

end.

