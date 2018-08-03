unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ureadprog, uinstructions;

const ArithmeticMaxBits = 8;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
procedure WriteUnitProperties(AFilename: string);

implementation

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

//instruction pointer

var
  IPVar, CurIPValue, SysIPVar, CurSysIPValue: integer;

function SetNextIP(AValue: integer): TInstruction;
begin
  result := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[IPVar].UnitType, simSetTo, AValue);
end;

function CheckIP(AValue: integer): TCondition;
begin
  result := TIntegerCondition.Create(plCurrentPlayer, IntArrays[IPVar].UnitType, icmExactly, AValue);
end;

function NewIP: integer;
begin
  Inc(CurIPValue);
  result := CurIPValue;
end;

function GetSysIPVar: integer;
begin
  if SysIPVar = -1 then
  begin
    SysIPVar := IntArrayIndexOf('_sysIp');
    if SysIPVar = -1 then
      SysIPVar := CreateIntArray('_sysIp', MaxArraySize, []);
  end;
  result := SysIPVar;
end;

function SetNextSysIP(AValue: integer): TInstruction;
begin
  result := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, simSetTo, AValue);
end;

function CheckSysIP(AValue: integer): TCondition;
begin
  result := TIntegerCondition.Create(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, icmExactly, AValue);
end;

function NewSysIP: integer;
begin
  Inc(CurSysIPValue);
  result := CurSysIPValue;
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
  actionCount: integer;
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
  actionCount := 0;
  for i := 0 to high(AActions) do
    if AActions[i] <> '' then
    begin
      AOutput.Add(#9 + AActions[i] + ';');
      actionCount += 1;
    end;
  if ANextIP <> -1 then
  begin
    AOutput.Add(#9 + SetNextIP(ANextIP).ToStringAndFree + ';');
    actionCount += 1;
  end;
  if APreserve then
  begin
    AOutput.Add(#9 + 'Preserve Trigger();');
    actionCount += 1;
  end;
  AOutput.Add('}');
  AOutput.Add('');
  if actionCount > 64 then raise exception.Create('Too many actions in trigger');
end;

// output instructions that have already been expanded

var
  MaxStackBits: integer;  //number of bits for one value on the stack (max is 32)
  ReturnSysIP, PushSysIP, SysParamArray: integer;

  PrintedBoolVar: integer;
  PrintingMessageIntVar: integer;
  Messages: array of record
    Text: string;
    Players: TPlayers;
    Always: boolean;
  end;
  MessageCount: integer;

  BusyIP : integer;

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

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0); forward;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);
var
  i,j: Integer;
  condStr, instrStr: array of string;
  instrCount: integer;

  addFromSwitch: TAddIntegerFromSwitchesInstruction;
  NextIP: integer;
  add2: TSetIntegerInstruction;
  add2prog, remain: TInstructionList;
  waitPush: TCondition;
  switchCheck: TSwitchCondition;
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
    condStr[High(condStr)] := CheckIP(AIPStart).ToStringAndFree;
  end;

  setlength(instrStr, AProg.Count+1);
  instrCount := 0;
  if ((AProg.Count > 0) and not (AProg[0] is TJumpReturnInstruction) and not
    (AProg[0] is TChangeIPInstruction) and not (AProg[0] is TWaitConditionInstruction)
    and not (AProg[0] is TSplitInstruction) ) and (AIPStart > 0) and (AReturnIP <> -1) then
  begin
    instrStr[0] := SetNextIP(BusyIP).ToStringAndFree;
    inc(instrCount);
  end;

  for i := 0 to AProg.Count-1 do
  begin
    if AProg[i] is TAddIntegerFromSwitchesInstruction then
    begin
      if not UniquePlayer(APlayers) then
        raise exception.Create('You cannot use implicit switches for multiple players at once');

      addFromSwitch := TAddIntegerFromSwitchesInstruction(AProg[i]);

      NextIP:= NewIP;

      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), NextIP, APreserve or (ATempPreserve > 0));

      //add powers of 2
      switchCheck := TSwitchCondition.Create(0, true);
      add2prog := TInstructionList.Create;
      add2 := TSetIntegerInstruction.Create(addFromSwitch.Player, addFromSwitch.UnitType, simAdd, 0);
      add2prog.add(add2);
      for j := 0 to high(addFromSwitch.Switches) do
      begin
        switchCheck.Switch:= addFromSwitch.Switches[j];
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

      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), jumpRet.DestIP, APreserve or (ATempPreserve > 0));

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

      setlength(instrStr, instrCount + 2);
      instrStr[instrCount] := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, simSetTo, push.Value).ToStringAndFree;
      instrStr[instrCount+1] := SetNextSysIP(PushSysIP).ToStringAndFree;

      WriteTrigger(AOutput, APlayers, condStr, instrStr, push.NextIP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog after the push
      waitPush := CheckSysIP(0);
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
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), waitCond.IP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, waitCond.Conditions, remain, waitCond.IP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;

      exit;
    end else
    if AProg[i] is TSplitInstruction then
    begin
      if TSplitInstruction(AProg[i]).EndIP = -1 then TSplitInstruction(AProg[i]).EndIP := AReturnIP;

      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), TSplitInstruction(AProg[i]).EndIP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [], remain, TSplitInstruction(AProg[i]).ResumeIP, AReturnIP, APreserve, ATempPreserve);
      remain.Free;

      exit;
    end else
    if AProg[i] is TChangeIPInstruction then
    begin
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), TChangeIPInstruction(AProg[i]).IP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog
      remain := TInstructionList.Create;
      for j := i+1 to AProg.Count-1 do
        remain.Add(AProg[j]);
      WriteProg(AOutput, APlayers, [], remain, TChangeIPInstruction(AProg[i]).IP, AReturnIP, APreserve, ATempPreserve + TChangeIPInstruction(AProg[i]).Preserve);
      remain.Free;

      exit;
    end;
    instrStr[instrCount] := AProg[i].ToString;
    inc(instrCount);
  end;

  WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), AReturnIP, APreserve or (ATempPreserve > 0));
end;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);
var cond: array of TCondition;
  i: Integer;
begin
  setlength(cond,AConditions.Count);
  for i := 0 to AConditions.Count-1 do
    cond[i] := AConditions[i];
  WriteProg(AOutput, APlayers, cond, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve);
end;

procedure NeedSysParam;
begin
  SysParamArray:= IntArrayIndexOf('_sysParam');
  if SysParamArray = -1 then
    SysParamArray := CreateIntArray('_sysParam', MaxArraySize, []);
end;

//stack

var
  SPVar: integer;
  StackArray: integer;
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

    NeedSysParam;

    ReturnSysIP := NewSysIP;
    PushSysIP:= NewSysIP;
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
    IntArrays[StackArray].Size:= StackSize;
end;

procedure WriteStackTriggers(AOutput: TStringList);
var proc: TInstructionList;
  cond: TConditionList;
  spCond, valCond: TIntegerCondition;
  returnCond, pushCond: TCondition;
  subStackVal,addIP,addStackVal, subVal: TSetIntegerInstruction;
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

  AOutput.Add('// Return //');
  //return handler

  //SP -= 1, IP := 0
  returnCond := CheckSysIP(ReturnSysIP);
  cond.Add(returnCond);
  proc.Add(TSetIntegerInstruction.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, simSubtract, 1));
  proc.Add(SetNextIP(0));
  WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
  EmptyProc;

  //copy stack value to IP
  spCond := TIntegerCondition.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, icmExactly, 0);
  valCond := TIntegerCondition.Create(plNone, IntArrays[StackArray].UnitType, icmAtLeast, 0);
  cond.Add(spCond);
  cond.Add(valCond);
  for sp := 0 to StackSize-1 do
  begin
    valCond.Player := IntToPlayer(sp+1);

    subStackVal := TSetIntegerInstruction.Create(plNone, IntArrays[StackArray].UnitType, simSubtract, 0);
    addIP := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[IPVar].UnitType, simAdd, 0);
    proc.Add(subStackVal);
    proc.Add(addIP);

    spCond.Value := sp;
    for bit := MaxStackBits-1 downto 0 do
    begin
      valCond.Value := 1 shl bit;

      subStackVal.Player := valCond.Player;
      subStackVal.Value := valCond.Value;
      addIP.Value := valCond.Value;

      WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
    end;

    EmptyProc;
    proc.Add(SetNextSysIP(0));
    WriteProg(AOutput, [plAllPlayers], [returnCond,spCond], proc, -1, -1, True);
    EmptyProc;
  end;
  EmptyCond;

  AOutput.Add('// Push //');

  //stack overflow handler
  cond.Add(CheckSysIP(PushSysIP));
  cond.Add(TIntegerCondition.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, icmAtLeast, StackSize));
  proc.Add(TDisplayTextMessageInstruction.Create(True, 'Stack overflow', [plAllPlayers]));
  proc.Add(TWaitInstruction.Create(4000));
  WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
  EmptyCond;
  EmptyProc;

  //push handler
  pushCond := CheckSysIP(PushSysIP);
  spCond := TIntegerCondition.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, icmExactly, 0);
  valCond := TIntegerCondition.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, icmAtLeast, 0);
  cond.Add(pushCond);
  cond.Add(spCond);
  cond.Add(valCond);
  for sp := 0 to StackSize-1 do
  begin
    spCond.Value:= sp;
    addStackVal := TSetIntegerInstruction.Create(IntToPlayer(sp+1), IntArrays[StackArray].UnitType, simAdd, 0);
    proc.Add(addStackVal);
    subVal := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, simSubtract, 0);
    proc.Add(subVal);
    for bit := MaxStackBits-1 downto 0 do
    begin
      valCond.Value := 1 shl bit;
      addStackVal.Value := valCond.Value;
      subVal.Value := valCond.Value;

      WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
    end;
    EmptyProc;

    proc.add(TSetIntegerInstruction.Create(IntVars[SPVar].Player, IntVars[SPVar].UnitType, simAdd, 1));
    proc.Add(SetNextSysIP(0));
    WriteProg(AOutput, [plAllPlayers], [pushCond, spCond], proc, -1, -1, True);
    EmptyProc;
  end;

  cond.Free;
  proc.Free;
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

var
  AddToAccSysIP, AddFromAccSysIP, AccArray: integer;
  TransferProcs: array of record
    AddToAcc,AddFromAcc: boolean;
  end;

procedure NeedTransfer;
begin
  if AddToAccSysIP = -1 then
  begin
    AddToAccSysIP:= NewSysIP;
    AddFromAccSysIP:= NewSysIP;

    AccArray := IntArrayIndexOf('_accumulator');
    if AccArray = -1 then
      AccArray := CreateIntArray('_accumulator', MaxArraySize, []);

    NeedSysParam;

    TransferProcs := nil;
    setlength(TransferProcs, IntVarCount);
  end;
end;

function GetTransferParam(APlayer: TPlayer; AUnitType: string; ASysIP: integer): integer;
var
  i: Integer;
begin
  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Player = APlayer) and (IntVars[i].UnitType = AUnitType) then
    begin
      if ASysIP = AddToAccSysIP then TransferProcs[i].AddToAcc:= true;
      if ASysIP = AddFromAccSysIP then TransferProcs[i].AddFromAcc:= true;
      exit(i);
    end;
  raise exception.Create('Unable to find variable');
end;

procedure WriteTransferTriggers(AOutput: TStringList);
var
  condIP: TCondition;
  proc: TInstructionList;

  procedure EmptyProc;
  var
    i: Integer;
  begin
    for i := 0 to proc.Count-1 do
      proc[i].Free;
    proc.Clear;
  end;

var
  i, j: integer;
  condSwitch, condRestore: TSwitchCondition;
  addAcc: TSetIntegerInstruction;
  condValue, condVar: TIntegerCondition;
  setSw: TSetSwitchInstruction;
  switchRestore: integer;
begin
  if AddToAccSysIP = -1 then exit;

  NeedTempBools(ArithmeticMaxBits+1);
  switchRestore := BoolVars[TempBools[ArithmeticMaxBits]].Switch;

  AOutput.Add('// Add To Accumulator //');;

  //clear temp bits
  condIP := CheckSysIP(AddToAccSysIP);
  proc := TInstructionList.Create;
  for i := 0 to ArithmeticMaxBits-1 do
    proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[i]].Switch, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchRestore, svSet)); //restore done in "Add From Accumulator"
  WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
  EmptyProc;

  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddToAcc then
  begin
    //transfer to bools
    condVar := TIntegerCondition.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, icmExactly, i);
    condValue := TIntegerCondition.Create(IntVars[i].Player, IntVars[i].UnitType, icmAtLeast, 0);
    addAcc := TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSubtract, 0);
    setSw := TSetSwitchInstruction.Create(0, svSet);
    proc.Add(addAcc);
    proc.Add(setSw);
    for j := ArithmeticMaxBits-1 downto 0 do
    begin
      condValue.Value := 1 shl j;
      addAcc.Value := condValue.Value;
      setSw.Switch := BoolVars[TempBools[j]].Switch;
      WriteProg(AOutput, [plAllPlayers], [condIP, condVar, condValue], proc, -1,-1, True);
    end;
    EmptyProc;
    condValue.Free;
  end;

  // add from temp bits
  condSwitch := TSwitchCondition.Create(0,true);
  addAcc := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, 0);
  proc.Add(addAcc);
  for i := ArithmeticMaxBits-1 downto 0 do
  begin
    condSwitch.Switch := BoolVars[TempBools[i]].Switch;
    addAcc.Value := 1 shl i;
    WriteProg(AOutput, [plAllPlayers], [condIP, condSwitch], proc, -1,-1, True);
  end;
  EmptyProc;
  condSwitch.Free;

  // return
  proc.Add(SetNextSysIP(0));
  WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
  EmptyProc;
  condIP.Free;

  AOutput.Add('// Add From Accumulator //');;

  // clear temp bits
  condIP := CheckSysIP(AddFromAccSysIP);
  proc := TInstructionList.Create;
  for i := 0 to ArithmeticMaxBits-1 do
    proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[i]].Switch, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchRestore, svSet));
  WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
  EmptyProc;

  //transfer to bools
  condValue := TIntegerCondition.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, 0);
  addAcc := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, 0);
  setSw := TSetSwitchInstruction.Create(0, svSet);
  proc.Add(addAcc);
  proc.Add(setSw);
  for j := ArithmeticMaxBits-1 downto 0 do
  begin
    condValue.Value := 1 shl j;
    addAcc.Value := condValue.Value;
    setSw.Switch := BoolVars[TempBools[j]].Switch;
    WriteProg(AOutput, [plAllPlayers], [condIP, condValue], proc, -1,-1, True);
  end;
  EmptyProc;
  condValue.Free;

  //add or restore
  condRestore := TSwitchCondition.Create(switchRestore,true);
  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddFromAcc or TransferProcs[i].AddToAcc then
  begin
    //add to dest (or restore source)
    condVar := TIntegerCondition.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, icmExactly, i);
    condSwitch := TSwitchCondition.Create(0,true);
    addAcc := TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simAdd, 0);
    proc.Add(addAcc);
    for j := ArithmeticMaxBits-1 downto 0 do
    begin
      condSwitch.Switch := BoolVars[TempBools[j]].Switch;
      addAcc.Value := 1 shl j;
      WriteProg(AOutput, [plAllPlayers], [condRestore, condVar, condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;
    condSwitch.Free;
    condVar.Free;
  end;

  // clear restore switch
  proc.Add(TSetSwitchInstruction.Create(switchRestore, svClear));
  WriteProg(AOutput, [plAllPlayers], [condRestore], proc, -1,-1, True);
  EmptyProc;
  condRestore.Free;

  // return
  proc.Add(SetNextSysIP(0));
  WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
  EmptyProc;

  condIP.Free;

  proc.Free;

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
  transf: TTransferIntegerInstruction;
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
      NeedTransfer;
      transf := TTransferIntegerInstruction(AProg[i]);
      case transf.Action of
      itCopyToAccumulator: expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, 0));
      itCopyFromAccumulator: expanded.Add(TSetIntegerInstruction.Create(transf.Player, transf.UnitType, simSetTo, 0));
      end;
      nextIP := -1;
      case transf.Action of
      itCopyToAccumulator, itAddToAccumulator: nextIP := AddToAccSysIP;
      itCopyFromAccumulator, itAddFromAccumulator: nextIP := AddFromAccSysIP;
      end;
      if nextIP <> -1 then
      begin
        expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, simSetTo, GetTransferParam(transf.Player,transf.UnitType,nextIP)));
        expanded.Add(SetNextSysIP(nextIP));
        expanded.Add(TWaitConditionInstruction.Create(CheckSysIP(0), NewIP));
      end;
      transf.Free;
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

      If AInProc <> -1 then
      begin
        if Procedures[AInProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AInProc].Calls.Add(procIdx);
      end;
      NeedStack;
      nextIP := NewIP;
      expanded.Add( TPushInstruction.Create(nextIP, NewIP) );

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;
      expanded.Add( TJumpReturnInstruction.Create(Procedures[procIdx].StartIP, nextIP) );

      call.Free;
      continue;
    end else
    if AProg[i] is TReturnInstruction then
    begin
      ret := TReturnInstruction(AProg[i]);
      if AInProc<>-1 then
      begin
        NeedStack;
        expanded.Add( SetNextSysIP(ReturnSysIP) );
      end;
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

begin
  CurIPValue := 0;

  IPVar := IntArrayIndexOf('_ip');
  if IPVar = -1 then
    IPVar := CreateIntArray('_ip', MaxArraySize, []);

  //IntArrays[IPVar].UnitType:= 'Gas'; //debug

  CurSysIPValue:= 0;
  SysIPVar := -1;

  SPVar := -1;
  ReturnSysIP := -1;
  PushSysIP:= -1;
  SysParamArray := -1;

  StackArray:= -1;
  PrintedBoolVar:= -1;
  PrintingMessageIntVar:= -1;
  MessageCount := 0;
  HyperWaitVar := -1;

  AddToAccSysIP:= -1;
  AddFromAccSysIP:= -1;

  BusyIP := NewIP;

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

  for i := 0 to EventCount-1 do
    ExpandInstructions(Events[i].Instructions, -1, [AMainThread]);

  DetermineStackValueSize;

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
      WriteProg(mainOutput, [AMainThread], [], Procedures[i].Instructions, Procedures[i].StartIP, EndIP, true);
    end;

  for i := 0 to EventCount-1 do
  begin
    mainOutput.Add('// When //');
    WriteProg(mainOutput, [AMainThread], Events[i].Conditions, Events[i].Instructions, EndIP, EndIP, Events[i].Preserve);
  end;

  //write generated code at the end of the file
  WriteMessages(mainOutput);
  WriteStackTriggers(mainOutput);
  WriteTransferTriggers(mainOutput);

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

