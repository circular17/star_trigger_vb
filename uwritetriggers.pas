unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ureadprog, uinstructions;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
procedure WriteSwitches(AFilename: string);

implementation

const
  PredefineProcedures: array[0..50] of string =
('Center View', //("Location");
'Create Unit', //("Players", "Unit Name", Unit Amount(#), "Location");
'Create Unit with Properties', //("Players", "Unit Name", Unit Amount(#), "Location", CUWP Slot(#));
'Defeat', //();
'Display Text Message', //(Always Display, "Text here");
'Draw', //();
'Give Units to Player', //("Units owned by Players", "Given to Players", "Unit Name", Unit Amount(#), "Location");
'Kill Unit', //("Players", "Unit Name");
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
'Modify Unit Energy', //("Players", "Unit Name", Energy%, Unit Amount(#), "Location");
'Modify Unit Hanger Count', //("Players", "Unit Name", Hanger Amount(#) Added, Unit Amount(#), "Location");
'Modify Unit Hit Points', //("Players", "Unit Name", Health%, Unit Amount(#), "Anywhere");
'Modify Unit Resource Amount', //(#)("Players", Resource Amount(#), Unit Amount(#), "Location");
'Modify Unit Shield Points', //("Players", "Unit Name", Shield%, Unit Amount(#), "Location");
'Move Location', //("Players", "Unit Name", "Location moved", "Location");
'Move Unit', //("Players", "Unit Name", Unit Amount(#), "Location from", "Location to");
'Mute Unit Speech', //();
'Order', //("Players", "Unit Name", "Location from", "Location to", Movement Type);
'Pause Game', //();
'Pause Timer', //();
'Play WAV', //("WAV path", WAV length in ms);
'Preserve Trigger', //();
'Remove Unit', //("Players", "Unit Name");
'Remove Unit At Location', //("Players", "Unit Name", Unit Amount(#), "Location");
'Run AI Script', //("Script Name");
'Run AI Script At Location', //("Script Name", "Location");
'Set Alliance Status', //("Players", Alliance Status);
'Set Countdown Timer', //(Edit Type, Time in Seconds);
'Set Doodad State', //("Players", "Unit Name", "Location", State);
'Set Invincibility', //("Players", "Unit Name", "Location", State);
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

//instruction pointer

var
  IPVar, CurIPValue: integer;

function SetNextIP(AValue: integer): TInstruction;
begin
  result := TSetIntInstruction.Create(plCurrentPlayer, IntArrays[IPVar].UnitType, simSetTo, AValue);
end;

function NewIP: integer;
begin
  Inc(CurIPValue);
  result := CurIPValue;
end;

function UniquePlayer(APlayers: TPlayers): boolean;
var count: integer;
  pl: TPlayer;
begin
  count := 0;
  for pl := succ(plNone) to high(TPlayer) do
  begin
    if pl in APlayers then
    begin
      if pl > plPlayer12 then exit(false);
      inc(count);
    end;
  end;
  exit(count=1);
end;

//basic trigger output function

procedure WriteTrigger(AOutput: TStringList; APlayers: TPlayers;
                       AConditions: array of string;
                       AActions: array of string;
                       ANextIP: integer;
                       APreserve: boolean);
var
  i: Integer;
  pl: TPlayer;
  firstPl: boolean;
  s: string;
begin
  s := 'Trigger(';
  firstPl:= true;
  for pl := low(TPlayer) to high(TPlayer) do
    if pl in APlayers then
    begin
      if not firstPl then s += ',';
      s += '"' + PlayerToStr(pl) + '"';
      firstPl:= false;
    end;
  s += '){';
  AOutput.Add(s);
  AOutput.Add('Conditions:');
  for i := 0 to high(AConditions) do
    AOutput.Add(#9 + AConditions[i] + ';');
  if length(AConditions) = 0 then
    AOutput.Add(#9 + 'Always();');
  AOutput.Add('Actions:');
  for i := 0 to high(AActions) do
    if AActions[i] <> '' then
      AOutput.Add(#9 + AActions[i] + ';');
  if ANextIP > 0 then
    AOutput.Add(#9 + SetNextIP(ANextIP).ToStringAndFree + ';');
  if APreserve then
    AOutput.Add(#9 + 'Preserve Trigger();');
  AOutput.Add('}');
  AOutput.Add('');
end;

// output instructions that have already been expanded

var
  MaxStackBits: integer;  //number of bits for one value on the stack (max is 32)
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

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: boolean = false);
var
  i,j: Integer;
  condStr, instrStr: array of string;
  instrCount: integer;

  addFromSwitch: TAddDeathFromSwitchInstruction;
  NextIP: integer;
  add2: TSetIntInstruction;
  add2prog, remain: TInstructionList;
  switchCheck, waitPush: TSwitchCondition;
  jumpRet: TJumpReturnInstruction;
  push: TPushInstruction;
  waitCond: TWaitConditionInstruction;

begin
  setlength(condStr, length(AConditions) );
  for i := 0 to high(AConditions) do
    condStr[i] := AConditions[i].ToString;

  if AIPStart <> -1 then
  begin
    setlength(condStr, length(condStr) + 1);
    condStr[High(condStr)] := TIntegerCondition.Create(plCurrentPlayer, IntArrays[IPVar].UnitType, dcmExactly, AIPStart).ToStringAndFree;
  end;

  setlength(instrStr, AProg.Count+1);
  instrCount := 0;
  if ((AProg.Count > 0) and not (AProg[0] is TJumpReturnInstruction) and not
    (AProg[0] is TChangeIPInstruction) and not (AProg[0] is TIfInstruction)
    and not (AProg[0] is TElseInstruction) ) and (AIPStart > 0) and (AReturnIP <> -1) then
  begin
    instrStr[0] := SetNextIP(0).ToStringAndFree;
    inc(instrCount);
  end;

  for i := 0 to AProg.Count-1 do
  begin
    if AProg[i] is TAddDeathFromSwitchInstruction then
    begin
      if not UniquePlayer(APlayers) then
        raise exception.Create('You cannot use implicit switches for multiple players at once');

      addFromSwitch := TAddDeathFromSwitchInstruction(AProg[i]);

      NextIP:= NewIP;

      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), NextIP, APreserve or ATempPreserve);

      //add powers of 2
      switchCheck := TSwitchCondition.Create('?', true);
      add2prog := TInstructionList.Create;
      add2 := TSetIntInstruction.Create(addFromSwitch.Player, addFromSwitch.UnitType, simAdd, 0);
      add2prog.add(add2);
      for j := 0 to high(addFromSwitch.Switches) do
      begin
        switchCheck.SwitchName:= addFromSwitch.Switches[j];
        add2.Value := 1 shl j;
        WriteProg(AOutput, APlayers, [switchCheck], add2prog, NextIP, -1, APreserve, ATempPreserve);
      end;
      add2.Free;
      add2prog.Free;
      switchCheck.Free;

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [], remain, NextIP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;

      exit;
    end else
    if AProg[i] is TJumpReturnInstruction then
    begin
      jumpRet := TJumpReturnInstruction(AProg[i]);

      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), jumpRet.DestIP, APreserve or ATempPreserve);

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [], remain, jumpRet.ReturnIP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;

      exit;
    end else
    if AProg[i] is TPushInstruction then
    begin
      push := TPushInstruction(AProg[i]);

      setlength(instrStr, instrCount + MaxStackBits + 1);
      for j := 0 to MaxStackBits-1 do
      begin
        if push.Value and (1 shl j) = 0 then
          instrStr[instrCount] := TSetSwitchInstruction.Create(BoolVars[TempBools[j]].Name, svClear).ToStringAndFree
        else
          instrStr[instrCount] := TSetSwitchInstruction.Create(BoolVars[TempBools[j]].Name, svSet).ToStringAndFree;
        inc(instrCount);
      end;
      instrStr[instrCount] := TSetSwitchInstruction.Create(push.SwitchName, svSet).ToStringAndFree;

      WriteTrigger(AOutput, APlayers, condStr, instrStr, push.NextIP, APreserve or ATempPreserve);

      //carry on with the rest of the prog after the push
      waitPush := TSwitchCondition.Create(push.SwitchName, False);
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [waitPush], remain, push.NextIP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;
      waitPush.Free;

      exit;
    end else
    if AProg[i] is TWaitConditionInstruction then
    begin
      waitCond := TWaitConditionInstruction(AProg[i]);
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), waitCond.IP, APreserve or ATempPreserve);

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [waitCond.Condition], remain, waitCond.IP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;

      exit;
    end else
    if AProg[i] is TElseInstruction then
    begin
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), TElseInstruction(AProg[i]).EndIP, APreserve or ATempPreserve);

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [], remain, TElseInstruction(AProg[i]).ThenElseIP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;

      exit;
    end else
    if AProg[i] is TChangeIPInstruction then
    begin
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), TChangeIPInstruction(AProg[i]).IP, APreserve or ATempPreserve);

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [], remain, TChangeIPInstruction(AProg[i]).IP, AReturnIP, APreserve, TChangeIPInstruction(AProg[i]).Preserve);
      remain.Free;

      exit;
    end;
    instrStr[instrCount] := AProg[i].ToString;
    inc(instrCount);
  end;

  WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), AReturnIP, APreserve or ATempPreserve);
end;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean);

var cond: array of TCondition;
  i: Integer;
begin
  setlength(cond,AConditions.Count);
  for i := 0 to AConditions.Count-1 do
    cond[i] := AConditions[i];
  WriteProg(AOutput, APlayers, cond, AProg, AIPStart, AReturnIP, APreserve);
end;

//stack

var
  SPVar: integer;
  StackArray: integer;
  ReturningBoolVar, PushingBoolVar: Integer;
  StackSize: integer;     //number of values that can be on the stack (max is MaxArraySize)

procedure NeedStack;
begin
  if SPVar = -1 then
  begin
    SPVar := IntVarIndexOf('_sp');
    if SPVar = -1 then
      SPVar := CreateIntVar('_sp', 0);

    StackArray:= IntArrayIndexOf('_stack');
    if StackArray = -1 then
      StackArray := CreateIntArray('_stack', MaxArraySize, []);

    ReturningBoolVar := BoolVarIndexOf('_return');
    if ReturningBoolVar = -1 then
      ReturningBoolVar := CreateBoolVar('_return', svClear);

    PushingBoolVar := BoolVarIndexOf('_push');
    if PushingBoolVar = -1 then
      PushingBoolVar := CreateBoolVar('_push', svClear);
  end;
end;

procedure DetermineStackValueSize;
var
  i,depth: Integer;

  function GetDepthRec(AProc: integer; ADepth: integer): integer;
  var j,sub: integer;
  begin
    result := ADepth;
    if ADepth = MaxArraySize then exit;

    with Procedures[AProc] do
      for j := 0 to Calls.Count-1 do
      begin
        sub:= GetDepthRec(Calls[j], ADepth+1);
        if sub > result then result := sub;
      end;
  end;

begin
  MaxStackBits:= 1;
  while (1 shl MaxStackBits) - 1 < CurIPValue do inc(MaxStackBits);

  StackSize := 1;
  for i := 0 to ProcedureCount-1 do
  if Procedures[i].StartIP <> -1 then
  begin
    depth := GetDepthRec(i, 1);
    if depth > StackSize then StackSize:= depth;
  end;

  if StackArray <> -1 then
  begin
    IntArrays[StackArray].Size:= StackSize;
    NeedTempBools(MaxStackBits);
  end;
end;

procedure WriteStackTriggers(AOutput: TStringList; AMainThread: TPlayer);
var proc: TInstructionList;
  cond: TConditionList;
  spCond, valCond: TIntegerCondition;
  switchCond, pushCond, returnCond: TSwitchCondition;
  subStackVal,addIP,addStackVal: TSetIntInstruction;
  sp, bit: integer;

  procedure EmptyProc;
  var
    i: Integer;
  begin
    for i := 0 to proc.Count-1 do
      proc[i].Free;
    proc.Clear;
  end;

  procedure EmptyCond;
  var
    i: Integer;
  begin
    for i := 0 to cond.Count-1 do
      cond[i].Free;
    cond.Clear;
  end;

begin
  if SPVar = -1 then exit;

  proc := TInstructionList.Create;
  cond := TConditionList.Create;

  //return handler

  //SP -= 1, IP := 0
  returnCond := TSwitchCondition.Create(BoolVars[ReturningBoolVar].Name, True);
  cond.Add(returnCond);
  proc.Add(TSetIntInstruction.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, simSubtract, 1));
  proc.Add(TSetIntInstruction.Create(AMainThread, IntArrays[IPVar].UnitType, simSetTo, 0));
  WriteProg(AOutput, [AMainThread], cond, proc, -1, -1, True);
  EmptyProc;

  //copy stack value to IP
  spCond := TIntegerCondition.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, dcmExactly, 0);
  valCond := TIntegerCondition.Create(plNone, IntArrays[StackArray].UnitType, dcmAtLeast, 0);
  cond.Add(spCond);
  cond.Add(valCond);
  for sp := 0 to StackSize-1 do
  begin
    valCond.Player := IntToPlayer(sp+1);

    subStackVal := TSetIntInstruction.Create(plNone, IntArrays[StackArray].UnitType, simSubtract, 0);
    addIP := TSetIntInstruction.Create(AMainThread, IntArrays[IPVar].UnitType, simAdd, 0);
    proc.Add(subStackVal);
    proc.Add(addIP);

    spCond.Value := sp;
    for bit := MaxStackBits-1 downto 0 do
    begin
      valCond.Value := 1 shl bit;

      subStackVal.Player := valCond.Player;
      subStackVal.Value := valCond.Value;
      addIP.Value := valCond.Value;

      WriteProg(AOutput, [AMainThread], cond, proc, -1, -1, True);
    end;

    EmptyProc;
    proc.Add(TSetSwitchInstruction.Create(BoolVars[ReturningBoolVar].Name, svClear));
    WriteProg(AOutput, [AMainThread], [returnCond,spCond], proc, -1, -1, True);
    EmptyProc;
  end;
  EmptyCond;

  //stack overflow handler
  cond.Add(TSwitchCondition.Create(BoolVars[PushingBoolVar].Name, true));
  cond.Add(TIntegerCondition.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, dcmAtLeast, StackSize));
  proc.Add(TDisplayTextMessageInstruction.Create(True, 'Stack overflow', [plAllPlayers]));
  proc.Add(TWaitInstruction.Create(4000));
  WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
  EmptyCond;
  EmptyProc;

  //push handler
  NeedTempBools(MaxStackBits);
  pushCond := TSwitchCondition.Create(BoolVars[PushingBoolVar].Name, true);
  spCond := TIntegerCondition.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, dcmExactly, 0);
  switchCond := TSwitchCondition.Create('?', true);
  cond.Add(pushCond);
  cond.Add(spCond);
  cond.Add(switchCond);
  for sp := 0 to StackSize-1 do
  begin
    spCond.Value:= sp;
    addStackVal := TSetIntInstruction.Create(IntToPlayer(sp+1), IntArrays[StackArray].UnitType, simAdd, 0);
    proc.Add(addStackVal);
    for bit := 0 to MaxStackBits-1 do
    begin
      switchCond.SwitchName := BoolVars[TempBools[bit]].Name;
      addStackVal.Value := 1 shl bit;

      WriteProg(AOutput, [AMainThread], cond, proc, -1, -1, True);
    end;
    EmptyProc;

    proc.add(TSetIntInstruction.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, simAdd, 1));
    proc.Add(TSetSwitchInstruction.Create(BoolVars[PushingBoolVar].Name, svClear));
    WriteProg(AOutput, [AMainThread], [pushCond, spCond], proc, -1, -1, True);
    EmptyProc;
  end;

  cond.Free;
  proc.Free;
end;

procedure ExpandInstructions(AProg: TInstructionList; AInProc: integer; APlayers: TPlayers);
var
  i, expo, j, nextIP, procIdx: Integer;
  sdi: TSetIntInstruction;
  expanded: TInstructionList;
  switches: array of string;
  call: TCallInstruction;
  ret: TReturnInstruction;
  nesting, elseIndex, endIfIndex: integer;
  startWhileIP, msgIdx: integer;
  ifInstr: TIfInstruction;
  elseInstr: TElseInstruction;
  disp: TDisplayTextMessageInstruction;
  waitInstr: TWaitConditionInstruction;

begin
  expanded := TInstructionList.Create;
  i := -1;
  while i < AProg.Count-1 do
  begin
    inc(i);

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
            expanded.Add(TChangeIPInstruction.Create(startWhileIP, true));
            waitInstr := TWaitConditionInstruction.Create(TWhileInstruction(AProg[i]).Condition, NewIP);
            TWhileInstruction(AProg[i]).Condition := nil;
            expanded.Add(waitInstr);

            AProg[j].Free;
            elseInstr := TElseInstruction.Create;
            elseInstr.ThenElseIP:= waitInstr.IP;
            elseInstr.EndIP:= startWhileIP;
            AProg[j] := elseInstr;

            AProg.Insert(j+1, TChangeIPInstruction.Create(NewIP, false) );
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
              AProg.Insert(j, TElseInstruction.Create);
              elseIndex := j;
              endIfIndex := j+1;
            end else
              endIfIndex := j;

            nextIP := NewIP;
            ifInstr := TIfInstruction(AProg[i]);
            expanded.Add(TWaitConditionInstruction.Create(ifInstr.Condition, nextIP));
            ifInstr.Condition := nil;
            ifInstr.Free;

            TElseInstruction(AProg[elseIndex]).ThenElseIP:= nextIP;

            nextIP := NewIP;
            TElseInstruction(AProg[elseIndex]).EndIP:= nextIP;
            AProg[endIfIndex].Free;
            AProg[endIfIndex] := TChangeIPInstruction.Create(nextIP, false);
            break;
          end;
        end;
      end;
      if nesting <> 0 then raise exception.Create('The number of End If do not match the number of If');
      continue;
    end else
    if AProg[i] is TElseInstruction then
    begin
      if TElseInstruction(AProg[i]).EndIP = -1 then
        raise exception.Create('Else instruction does not match an If instruction');
    end else
    if AProg[i] is TEndIfInstruction then
    begin
      raise exception.Create('End If instruction does not match an If instruction');
    end else
    if AProg[i] is TSetIntInstruction then
    begin
      sdi := TSetIntInstruction(AProg[i]);
      if sdi.Mode = simRandomize then
      begin
        expanded.Add(TSetIntInstruction.Create(sdi.Player, sdi.UnitType, simSetTo, 0));

        //insert randomize instruction
        expo := GetExponentOf2(sdi.Value);
        NeedTempBools(expo);
        setlength(switches, expo);
        for j := 0 to expo-1 do
        begin
          switches[j] := BoolVars[TempBools[j]].Name;
          expanded.Add( TSetSwitchInstruction.Create(switches[j], svRandomize) );
        end;

        expanded.Add( TAddDeathFromSwitchInstruction.Create(sdi.Player, sdi.UnitType, switches) );

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

      If AInProc <> -1 then
      begin
        if Procedures[AInProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AInProc].Calls.Add(procIdx);
      end;
      NeedStack;
      nextIP := NewIP;
      expanded.Add( TPushInstruction.Create(nextIP, BoolVars[PushingBoolVar].Name, NewIP) );

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;
      expanded.Add( TJumpReturnInstruction.Create(Procedures[procIdx].StartIP, nextIP) );

      call.Free;
      continue;
    end else
    if AProg[i] is TReturnInstruction then
    begin
      ret := TReturnInstruction(AProg[i]);
      expanded.Add( TSetSwitchInstruction.Create(BoolVars[ReturningBoolVar].Name, svSet) );
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
                         TIntegerCondition.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType, dcmExactly, 0), NewIP ) );
        expanded.Add( TSetIntInstruction.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType, simSetTo, msgIdx+1) );
        expanded.Add( TWaitConditionInstruction.Create(
                         TSwitchCondition.Create(BoolVars[PrintedBoolVar].Name, true), NewIP ) );
        expanded.Add( TSetIntInstruction.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType, simSetTo, 0) );
        expanded.Add( TSetSwitchInstruction.Create(BoolVars[PrintedBoolVar].Name, svClear) );
        disp.Free;
      end;
      Continue;
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
    if not IntArrays[i].Predefined then
      writeln(t, '// ', IntArrays[i].Name, '('+ intToStr(IntArrays[i].Size)+') stored in "', IntArrays[i].UnitType, '" //');
  for i := 0 to IntVarCount-1 do
    writeln(t, '// ', IntVars[i].Name, ' stored in "', IntVars[i].UnitType, '" of "', PlayerToStr(IntVars[i].Player),'" //');
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
  msgIdxCond := TIntegerCondition.Create(IntVars[PrintingMessageIntVar].Player, IntVars[PrintingMessageIntVar].UnitType,
                dcmExactly, 0);
  msgInstr := TDisplayTextMessageInstruction.Create(True,'',[plCurrentPlayer]);
  setFlag := TSetSwitchInstruction.Create(BoolVars[PrintedBoolVar].Name, svSet);
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

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
var
  i, j, EndIP: Integer;
  initSub, combineProg: TInstructionList;
  mainOutput: TStringList;
  allProcDone: Boolean;

begin
  CurIPValue := 0;

  IPVar := IntArrayIndexOf('_ip');
  if IPVar = -1 then
    IPVar := CreateIntArray('_ip', MaxArraySize, []);

  SPVar := -1;
  ReturningBoolVar := -1;
  StackArray:= -1;
  PrintedBoolVar:= -1;
  PrintingMessageIntVar:= -1;
  MessageCount := 0;

  initSub := TInstructionList.Create;
  for i := 0 to IntArrayCount-1 do
  with IntArrays[i] do
  begin
    for j := 1 to Size do
      if Values[j] <> 0 then
        initSub.Add(TSetIntInstruction.Create(IntToPlayer(j), UnitType, simSetTo, Values[j]))
  end;

  for i := 0 to IntVarCount-1 do
    if IntVars[i].Value <> 0 then
    begin
      if IntVars[i].Randomize then
        initSub.Add(TSetIntInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simRandomize, IntVars[i].Value))
      else
        initSub.Add(TSetIntInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSetTo, IntVars[i].Value));
    end;

  for i := 0 to BoolVarCount-1 do
    if BoolVars[i].Value in [svSet,svRandomize] then
      initSub.Add(TSetSwitchInstruction.Create(BoolVars[i].Name, BoolVars[i].Value));

  mainOutput := TStringList.Create;

  EndIP := NewIP;

  ExpandInstructions(initSub, -1, [AMainThread]);
  ExpandInstructions(MainProg, -1, [AMainThread]);

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

  DetermineStackValueSize;

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
      WriteProg(mainOutput, [AMainThread], [], Procedures[i].Instructions, Procedures[i].StartIP, EndIP, true);

  WriteMessages(mainOutput);

  //write stack code at the end of the file
  WriteStackTriggers(mainOutput, AMainThread);

  WriteFile(AFilename, mainOutput);
  mainOutput.Free;
end;

procedure WriteSwitches(AFilename: string);
var t: TextFile;
  i: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to BoolVarCount-1 do
    writeln(t, BoolVars[i].Switch, #9, '"', BoolVars[i].Name, '"');
  writeln(t);
  CloseFile(t);
end;

end.

