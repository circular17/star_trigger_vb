unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
procedure WriteUnitProperties(AFilename: string);

implementation

uses utriggercode, uarithmetic, ureadprog, uinstructions, uvariables, uparsevb, uexpressions;

var
  HyperWaitVar: integer;

procedure ConfigureHyperWait;
begin
  if HyperTriggers then
  begin
    if HyperWaitVar = -1 then
    begin
      HyperWaitVar := IntArrayIndexOf('_hyperwait');
      if HyperWaitVar = -1 then
        HyperWaitVar := CreateIntArray('_hyperwait', MaxTriggerPlayers, [], 24);
    end;
  end;
end;

var
  MessageSysIP: integer;
  GlobalExprTempVar: integer;

function GetGlobalExprTempVar(ABitCount: integer): integer;
begin
  if GlobalExprTempVar = -1 then
    GlobalExprTempVar:= AllocateTempInt(ABitCount)
  else
  begin
    if IntVars[GlobalExprTempVar].BitCount < ABitCount then
      IntVars[GlobalExprTempVar].BitCount := ABitCount;
  end;
  result := GlobalExprTempVar;
end;

procedure WriteMessageTriggers(AOutput: TStringList; AMainThread: TPlayer);
var
  i: Integer;
  proc: TInstructionList;
  condSysIP, condSysParam: TCondition;

begin
  If MessageSysIP = -1 then exit;
  condSysIP := CheckSysIP( MessageSysIP, AMainThread );
  AOutput.Add('// Messages //');
  for i := 0 to MessageCount-1 do
  begin
    proc := TInstructionList.Create;
    proc.Add( TDisplayTextMessageInstruction.Create(True, Messages[i].Text) );
    condSysParam := CheckSysParam( i, AMainThread );
    WriteProg(AOutput, Messages[i].Players, [condSysIP, condSysParam], proc, -1, -1, True);
    condSysParam.Free;
    proc.FreeAll;
  end;

  proc := TInstructionList.Create;
  proc.Add( SetNextSysIP(0) );
  WriteProg(AOutput, [AMainThread], [condSysIP], proc, -1,-1, true);
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
          tempInt := GetGlobalExprTempVar(arithm.GetBitCount)
        else
          tempInt := GetProcedureExprTempVar(AInProc, arithm.GetBitCount);
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgram(tempExpand, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
        ExpandInstructions(tempExpand, AInProc, APlayers);
        for k := 0 to tempExpand.Count-1 do
          expanded.Add(tempExpand[k]);
        tempExpand.Free;
        result := TWaitConditionInstruction.Create(TIntegerCondition.Create(IntVars[tempInt].Player,IntVars[tempInt].UnitType, arithm.CompareMode,arithm.CompareValue), ANextIP);
        expanded.Add(result);
      end;
    end else
    begin
      result := TWaitConditionInstruction.Create(AConditions, ANextIP);
      expanded.Add(result);
    end;
  end;

var
  i, expo, j, nextIP, procIdx: Integer;
  sdi: TSetIntegerInstruction;
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
  endDoIP, tempInt: integer;

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

      expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSetTo, TWaitInstruction(AProg[i]).DelayMs));
      AProg[i].Free;

      startWhileIP:= NewIP;
      expanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));

      waitInstr := TWaitConditionInstruction.Create(TIntegerCondition.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, icmAtLeast, 1), NewIP);
      expanded.Add(waitInstr);

      expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSubtract, 140));

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
    if AProg[i] is TSetIntegerInstruction then
    begin
      sdi := TSetIntegerInstruction(AProg[i]);
      if sdi.Mode = simRandomize then
      begin
        expanded.Add(TSetIntegerInstruction.Create(sdi.Player, sdi.UnitType, simSetTo, 0));

        //insert randomize instruction
        expo := GetExponentOf2(sdi.Value);
        NeedTempBools(expo);
        setlength(switches, expo);
        for j := 0 to expo-1 do
        begin
          switches[j] := BoolVars[TempBools[j]].Switch;
          expanded.Add( TSetSwitchInstruction.Create(switches[j], svRandomize) );
        end;

        expanded.Add( TAddIntegerFromSwitchesInstruction.Create(sdi.Player, sdi.UnitType, switches) );

        sdi.Free;
        continue;
      end;
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

procedure WriteFile(AFilename: string; ALines: TStringList);
var
  t: TextFile;
  i, j: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to IntArrayCount-1 do
    if not IntArrays[i].Predefined and not IntArrays[i].Constant then
    begin
      writeln(t, '// ', IntArrays[i].Name, '(1 to '+ intToStr(MaxTriggerPlayers)+') stored in "', IntArrays[i].UnitType, '" //');
      for j := 1 to IntArrays[i].Size do
        with IntVars[IntArrays[i].Vars[j-1]] do
          if UnitType <> IntArrays[i].UnitType then
            writeln(t, '// ', IntArrays[i].Name, '('+ intToStr(j)+') stored in "', UnitType, '" of "', PlayerToStr(Player),'" //');
    end;
  for i := 0 to IntVarCount-1 do
    if not IntVars[i].Predefined and not IntVars[i].Constant then
      writeln(t, '// ', IntVars[i].Name, ' stored in "', IntVars[i].UnitType, '" of "', PlayerToStr(IntVars[i].Player),'" //');
  for i := 0 to BoolArrayCount-1 do
    if not BoolArrays[i].Constant then
      writeln(t, '// ', BoolArrays[i].Name, '(1 to '+inttostr(BoolArrays[i].Size)+') stored in ' +
      '"Switch', BoolVars[BoolArrays[i].Vars[0]].Switch, '" to '+
      '"Switch', BoolVars[BoolArrays[i].Vars[BoolArrays[i].Size-1]].Switch,'" //');
  for i := 0 to BoolVarCount-1 do
    if not BoolVars[i].Constant and (BoolVars[i].BoolArray = -1) then
      writeln(t, '// ', BoolVars[i].Name, ' stored in "Switch', BoolVars[i].Switch, '" //');
  writeln(t);
  for i := 0 to ALines.Count-1 do
    writeln(t,ALines[i]);
  CloseFile(t);
end;

procedure WriteHyperTriggers(AOutput: TStringList);
var
  wait: TWaitInstruction;
  actions: array of string;
  i: Integer;
begin
  if not HyperTriggers then exit;

  AOutput.Add('// Hyper Triggers //');

  wait := TWaitInstruction.Create(15);

  setlength(actions,63);
  for i := 0 to high(actions) do
    actions[i] := wait.tostring;

  for i := 1 to 4 do
    WriteTrigger(AOutput, [plAllPlayers], [], actions, -1, true);

  wait.Free;
end;

procedure WritePlayerPresenceTopTrigger(AOutput: TStringList; AMainThread: TPlayer);
var
  proc: TInstructionList;
  pl: TPlayer;
  tempOutput: TStringList;
  i: Integer;
  someEntry: Boolean;
begin
  //at the begining of the cycle, set presence
  tempOutput := TstringList.Create;
  tempOutput.Add('// Player presence //');
  proc := TInstructionList.Create;
  someEntry := false;
  for pl := plPlayer1 to plPlayer8 do
    if IsPlayerPresenceUsed(pl) then
    begin
      someEntry := true;
      proc.Add(TSetSwitchInstruction.Create(BoolVars[GetPlayerPresenceBoolVar(pl)].Switch, svSet));
      WriteProg(tempOutput, [pl], [], proc, -1, -1, true);
      proc[0].Free;
      proc.Clear;
    end;
  proc.Free;

  if someEntry then
  begin
    for i := 0 to tempOutput.Count-1 do
      AOutput.Insert(i, tempOutput[i]);
  end;
  tempOutput.Free;
end;

procedure WritePlayerPresenceBottomTrigger(AOutput: TStringList; AMainThread: TPlayer);
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
    WriteProg(AOutput, [AMainThread], [], proc, -1, -1, true);
  end;
  proc.Free;
end;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
var
  i, EndIP: Integer;
  mainOutput: TStringList;
  allProcDone: Boolean;
  players: TPlayers;
  noSysIP: TCondition;

begin
  InitTriggerCode;
  InitArithmetic;

  HyperWaitVar := -1;
  MessageSysIP := -1;
  GlobalExprTempVar:= -1;
  mainOutput := TStringList.Create;
  EndIP := 0;

  ExpandInstructions(MainProg, -1, [AMainThread]);

  for i := 0 to EventCount-1 do
    ExpandInstructions(Events[i].Instructions, -1, Events[i].Players);

  repeat
    allProcDone := true;
    for i := 0 to ProcedureCount-1 do
      if (Procedures[i].StartIP <> -1) and not Procedures[i].Done then
      begin
        allProcDone:= false;

        ExpandInstructions(Procedures[i].Instructions, i, [plAllPlayers]);
        Procedures[i].Done := true;
      end;
  until allProcDone;

  WriteMessageTriggers(mainOutput, AMainThread);

  mainOutput.Add('// Program //');
  WriteProg(mainOutput, [AMainThread], [], MainProg, -1, EndIP, False);

  noSysIP := CheckSysIP(0);
  for i := 0 to ProcedureCount-1 do
    if Procedures[i].StartIP <> -1 then
    begin
      mainOutput.Add('// Sub ' + Procedures[i].Name + ' //');
      WriteProg(mainOutput, [plAllPlayers], [noSysIP], Procedures[i].Instructions, Procedures[i].StartIP, EndIP, true);
    end;
  noSysIP.Free;

  for i := 0 to EventCount-1 do
  begin
    mainOutput.Add('// When //');
    players := Events[i].Players;
    if players = [] then players := [AMainThread];
    WriteProg(mainOutput, players, Events[i].Conditions, Events[i].Instructions, EndIP, EndIP, Events[i].Preserve);
  end;

  //write generated code at the end of the file
  WriteStackTriggers(mainOutput);
  WriteArithmeticTriggers(mainOutput);
  WritePlayerPresenceBottomTrigger(mainOutput, AMainThread);
  WritePlayerPresenceTopTrigger(mainOutput, AMainThread);

  //it is recommended to put hyper triggers at the end
  WriteHyperTriggers(mainOutput);

  WriteFile(AFilename, mainOutput);
  mainOutput.Free;
end;

procedure WriteUnitProperties(AFilename: string);
var
  t: TextFile;
  i: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to UnitPropCount-1 do
  with UnitPropVars[i].Value do
  begin
    writeln(t,'Unit Property ' + inttostr(i) + ' // ' + UnitPropVars[i].Name);
    writeln(t);
    writeln(t,'HP: ' + inttostr(Life));
    writeln(t,'SP: ' + inttostr(Shield));
    writeln(t,'EP: ' + inttostr(Energy));
    writeln(t,'Res: ' + inttostr(Resource));
    writeln(t,'Hangar Units: ' + inttostr(HangarCount));
    write(t,'Flags: ');
    if Hallucinated then write(t,'HALLUCINATED');
    if Invincible then write(t,'INVINCIBLE');
    if Burrowed then write(t,'BURROWED');
    if Lifted then write(t,'INTRANSIT');
    if Cloaked then write(t,'CLOAKED');
    writeln(t);
    writeln(t);
  end;
  CloseFile(t);
end;

end.

