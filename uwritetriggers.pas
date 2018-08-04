unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
procedure WriteUnitProperties(AFilename: string);

implementation

uses utriggercode, uarithmetic, ureadprog, uinstructions, uvariables;

const
  PredefineProcedures: array[0..33] of string =
('Center View', //("Location");
'Defeat', //();
'Draw', //();
'Leader Board Control At Location', //("Label", "Unit Name", "Location");
'Leader Board Control', //("Label", "Unit Name");
'Leaderboard Greed', //(Resource Amount(#));
'Leader Board Kills', //("Label", "Unit Name");
'Leader Board Points', //("Label", Score Type);
'Leader Board Resources', //("Label", Resource Type);
'Leaderboard Goal Control At Location', //("Label", "Unit Name", Goal Amount(#), "Location");
'Leaderboard Goal Control', //("Label", "Unit Name", Goal Amount(#));
'Leaderboard Goal Kills', //("Label", "Unit Name", Goal Amount(#));
'Leaderboard Goal Points', //("Label", Score Type, Goal Amount(#));
'Leaderboard Goal Resources', //("Label", Goal Amount(#), Resource Type);
'Leaderboard Computer Players', //(State);
'Minimap Ping', //("Location");
'Mute Unit Speech', //();
'Pause Game', //();
'Pause Timer', //();
'Play WAV', //("WAV path", WAV length in ms);
'Preserve Trigger', //();
'Run AI Script', //("Script Name");
'Run AI Script At Location', //("Script Name", "Location");
'Set Alliance Status', //("Players", Alliance Status);
'Set Countdown Timer', //(Edit Type, Time in Seconds);
'Set Mission Objectives', //("Text here");
'Set Next Scenario', //("Scenario name");
'Set Switch', //("Switch Name", set/clear/toggle/randomize);
'Talking Portrait', //("Unit Name", Time in ms);
'Transmission', //(Always Display, "Text here", "Unit Name", "Location", Edit Type, Time in ms, "WAV path", WAV length in ms);
'Unmute Unit Speech', //();
'Unpause Game', //();
'Unpause Timer', //();
'Victory' //();
);

function IsPredefinedProcedure(AName: string): boolean;
var
  i: Integer;
begin
  for i := low(PredefineProcedures) to high(PredefineProcedures) do
    if CompareText(PredefineProcedures[i],AName)=0 then exit(true);
  exit(false);
end;

// output instructions that have already been expanded

var
  PrintedBoolVar: integer;
  PrintingMessageIntVar: integer;
  Messages: array of record
    Text: string;
    Players: TPlayers;
    Always: boolean;
  end;
  MessageCount: integer;

function AddMessage(AText: string; APlayers: TPlayers; AAlways: boolean): integer;
begin
  if MessageCount >= length(Messages) then
    setlength(Messages, MessageCount*2+4);

  result := MessageCount;
  inc(MessageCount);

  Messages[result].Text:= AText;
  Messages[result].Players:= APlayers;
  Messages[result].Always := AAlways;

  if PrintedBoolVar = -1 then
  begin
    PrintedBoolVar:= BoolVarIndexOf('_printed');
    if PrintedBoolVar = -1 then
      PrintedBoolVar:= CreateBoolVar('_printed', svClear);

    PrintingMessageIntVar:= IntVarIndexOf('_message');
    if PrintingMessageIntVar = -1 then
      PrintingMessageIntVar:= CreateIntVar('_message', 0);
  end;
end;

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
        HyperWaitVar := CreateIntArray('_hyperwait', MaxArraySize, []);
    end;
  end;
end;

procedure ExpandInstructions(AProg: TInstructionList; AInProc: integer; APlayers: TPlayers);
var
  i, expo, j, nextIP, procIdx: Integer;
  sdi: TSetIntegerInstruction;
  expanded: TInstructionList;
  switches: array of integer;
  call: TCallInstruction;
  ret: TReturnInstruction;
  nesting, elseIndex, endIfIndex: integer;
  startWhileIP, msgIdx, k: integer;
  ifInstr: TIfInstruction;
  disp: TDisplayTextMessageInstruction;
  waitInstr: TWaitConditionInstruction;
  splitInstr: TSplitInstruction;
  thenPart,elsePart: TInstructionList;
  notCond: TNotCondition;

begin
  expanded := TInstructionList.Create;
  i := -1;
  while i < AProg.Count-1 do
  begin
    inc(i);

    if AProg[i] is TTransferIntegerInstruction then
    begin
      ExpandIntegerTransfer(TTransferIntegerInstruction(AProg[i]), expanded);
      AProg[i].Free;
      Continue;
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

      expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSubtract, 80));

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
            waitInstr := TWaitConditionInstruction.Create(TWhileInstruction(AProg[i]).Conditions, NewIP);
            TWhileInstruction(AProg[i]).Conditions := nil;
            expanded.Add(waitInstr);

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
            expanded.Add(TWaitConditionInstruction.Create(ifInstr.Conditions, nextIP));
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
      begin
        if IsPredefinedProcedure(call.Name) then
        begin
          expanded.Add(call);
          continue;
        end else
          raise Exception.Create('Procedure not found "' + call.Name + '" with ' + Inttostr(length(call.Params)) + ' parameter(s)');
      end;

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;

      If AInProc <> -1 then
      begin
        if Procedures[AInProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AInProc].Calls.Add(procIdx);
      end;

      nextIP := NewIP;
      AddSysPush(expanded, nextIP);
      expanded.Add( TJumpReturnInstruction.Create(Procedures[procIdx].StartIP, nextIP) );

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
    end else
    if AProg[i] is TDisplayTextMessageInstruction then
    begin
      disp := TDisplayTextMessageInstruction(AProg[i]);
      if not ((disp.Players = [plCurrentPlayer]) or (disp.Players = APlayers)) then
      begin
        msgIdx := AddMessage(disp.Text, disp.Players, disp.Always);
        expanded.Add( TWaitConditionInstruction.Create(
                         TIntegerCondition.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType, icmExactly, 0), NewIP ) );
        expanded.Add( TSetIntegerInstruction.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType, simSetTo, msgIdx+1) );
        expanded.Add( TWaitConditionInstruction.Create(
                         TSwitchCondition.Create(BoolVars[PrintedBoolVar].Switch, true), NewIP ) );
        expanded.Add( TSetIntegerInstruction.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType, simSetTo, 0) );
        expanded.Add( TSetSwitchInstruction.Create(BoolVars[PrintedBoolVar].Switch, svClear) );
        disp.Free;
        Continue;
      end;
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
  i: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to IntArrayCount-1 do
    if not IntArrays[i].Predefined and not IntArrays[i].Constant then
      writeln(t, '// ', IntArrays[i].Name, '('+ intToStr(IntArrays[i].Size)+') stored in "', IntArrays[i].UnitType, '" //');
  for i := 0 to IntVarCount-1 do
    if not IntVars[i].Predefined and not IntVars[i].Constant then
      writeln(t, '// ', IntVars[i].Name, ' stored in "', IntVars[i].UnitType, '" of "', PlayerToStr(IntVars[i].Player),'" //');
  for i := 0 to BoolVarCount-1 do
    if not BoolVars[i].Constant then
      writeln(t, '// ', BoolVars[i].Name, ' stored in "Switch', BoolVars[i].Switch, '" //');
  writeln(t);
  for i := 0 to ALines.Count-1 do
    writeln(t,ALines[i]);
  CloseFile(t);
end;

procedure WriteMessages(AOutput: TStringList);
var
  msgIdxCond: TIntegerCondition;
  msgInstr: TDisplayTextMessageInstruction;
  setFlag: TSetSwitchInstruction;
  i: Integer;
begin
  If MessageCount = 0 then exit;
  AOutput.Add('// Messages //');

  msgIdxCond := TIntegerCondition.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType,
                icmExactly, 0);
  msgInstr := TDisplayTextMessageInstruction.Create(True,'',[plCurrentPlayer]);
  setFlag := TSetSwitchInstruction.Create(BoolVars[PrintedBoolVar].Switch, svSet);
  for i := 0 to MessageCount-1 do
  begin
    msgIdxCond.Value := i+1;
    msgInstr.Always:= Messages[i].Always;
    msgInstr.Text := Messages[i].Text;
    WriteTrigger(AOutput, Messages[i].Players, [msgIdxCond.ToString], [msgInstr.ToString, setFlag.ToString], -1, True);
  end;
  msgIdxCond.Free;
  msgInstr.Free;
  setFlag.Free;
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

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
var
  i, j, EndIP: Integer;
  initSub, combineProg: TInstructionList;
  mainOutput: TStringList;
  allProcDone: Boolean;
  players: TPlayers;

begin
  InitTriggerCode;
  InitArithmetic;

  PrintedBoolVar:= -1;
  PrintingMessageIntVar:= -1;
  MessageCount := 0;
  HyperWaitVar := -1;

  initSub := TInstructionList.Create;
  for i := 0 to IntArrayCount-1 do
  with IntArrays[i] do
  if not Constant then
  begin
    for j := 1 to Size do
      if Values[j] <> 0 then
        initSub.Add(TSetIntegerInstruction.Create(IntToPlayer(j), UnitType, simSetTo, Values[j]))
  end;

  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Value <> 0) and not IntVars[i].Constant then
    begin
      if IntVars[i].Randomize then
        initSub.Add(TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simRandomize, IntVars[i].Value))
      else
        initSub.Add(TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSetTo, IntVars[i].Value));
    end;

  for i := 0 to BoolVarCount-1 do
    if (BoolVars[i].Value in [svSet,svRandomize]) and not BoolVars[i].Constant then
      initSub.Add(TSetSwitchInstruction.Create(BoolVars[i].Switch, BoolVars[i].Value));

  mainOutput := TStringList.Create;

  EndIP := 0;

  ExpandInstructions(initSub, -1, [AMainThread]);
  ExpandInstructions(MainProg, -1, [AMainThread]);

  for i := 0 to EventCount-1 do
    ExpandInstructions(Events[i].Instructions, -1, [AMainThread]);

  repeat
    allProcDone := true;
    for i := 0 to ProcedureCount-1 do
      if (Procedures[i].StartIP <> -1) and not Procedures[i].Done then
      begin
        allProcDone:= false;

        ExpandInstructions(Procedures[i].Instructions, i, [AMainThread]);
        Procedures[i].Done := true;
      end;
  until allProcDone;

  mainOutput.Add('// Program //');

  if initSub.Count > 0 then
  begin
    combineProg := TInstructionList.Create;
    for i := 0 to initSub.Count-1 do
      combineProg.Add(initSub[i]);
    for i := 0 to MainProg.Count-1 do
      combineProg.Add(MainProg[i]);

    WriteProg(mainOutput, [AMainThread], [], combineProg, -1, EndIP, False);

    combineProg.Free;

    for i := 0 to initSub.Count-1 do
      initSub[i].Free;
  end else
    WriteProg(mainOutput, [AMainThread], [], MainProg, -1, EndIP, False);

  initSub.Free;

  for i := 0 to ProcedureCount-1 do
    if Procedures[i].StartIP <> -1 then
    begin
      mainOutput.Add('// Sub ' + Procedures[i].Name + ' //');
      WriteProg(mainOutput, [plAllPlayers], [], Procedures[i].Instructions, Procedures[i].StartIP, EndIP, true);
    end;

  for i := 0 to EventCount-1 do
  begin
    mainOutput.Add('// When //');
    players := Events[i].Players;
    if players = [] then players := [AMainThread];
    WriteProg(mainOutput, players, Events[i].Conditions, Events[i].Instructions, EndIP, EndIP, Events[i].Preserve);
  end;

  //write generated code at the end of the file
  WriteMessages(mainOutput);
  WriteStackTriggers(mainOutput);
  WriteArithmeticTriggers(mainOutput);

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

