unit utriggercode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uinstructions, usctypes;

const
  MaxStackSize = 6;

procedure InitTriggerCode;

//basic trigger output function
procedure WriteTrigger(AOutput: TStringList; APlayers: TPlayers;
                       AConditions: array of string;
                       AActions: array of string;
                       ANextIP: integer;
                       APreserve: boolean);

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);

function NewIP: integer;
function SetNextIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TInstruction;
function CheckIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;

function NewSysIP: integer;
function SetNextSysIP(AValue: integer; AMode: TSetIntegerMode = simSetTo): TInstruction;
function CheckSysIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
procedure CheckSysIPRange(AMinValue, AMaxValue: integer; out ACond1, ACond2: TCondition);

function SetSysParam(AValue: integer): TInstruction;
function CheckSysParam(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;

procedure AddSysCall(AInstructions: TInstructionList; AReturnIP, ACalledIP: integer);
procedure AddSysReturn(AInstructions: TInstructionList);
procedure WriteStackTriggers(AOutput: TStringList);

implementation

uses ureadprog, uvariables;

// instruction pointer for regular blocks

var
  IPVar, CurIPValue: integer;
  BusyIP : integer;

function NewIP: integer;
begin
  Inc(CurIPValue);
  result := CurIPValue;
end;

function SetNextIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TInstruction;
begin
  result := TSetIntegerInstruction.Create(APlayer, IntArrays[IPVar].UnitType, simSetTo, AValue);
end;

function CheckIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
begin
  result := TIntegerCondition.Create(APlayer, IntArrays[IPVar].UnitType, icmExactly, AValue);
end;

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

// instruction pointer for system calls

var
  SysIPVar, CurSysIPValue: integer;
  SysParamArray: integer; //parameter for system functions

function GetSysIPVar: integer;
begin
  if SysIPVar = -1 then
  begin
    SysIPVar := IntArrayIndexOf(GlobalScope, '_sysIp');
    if SysIPVar = -1 then
      SysIPVar := CreateIntArray(GlobalScope, '_sysIp', MaxTriggerPlayers, [], 16);
  end;
  result := SysIPVar;
end;

function SetNextSysIP(AValue: integer; AMode: TSetIntegerMode): TInstruction;
begin
  result := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, AMode, AValue);
end;

function CheckSysIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
begin
  result := TIntegerCondition.Create(APlayer, IntArrays[GetSysIPVar].UnitType, icmExactly, AValue);
end;

procedure CheckSysIPRange(AMinValue, AMaxValue: integer; out ACond1,
  ACond2: TCondition);
begin
  ACond1 := TIntegerCondition.Create(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, icmAtLeast, AMinValue);
  ACond2 := TIntegerCondition.Create(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, icmAtMost, AMaxValue);
end;

procedure NeedSysParam;
begin
  if SysParamArray = -1 then
  begin
    SysParamArray:= IntArrayIndexOf(GlobalScope, '_sysParam');
    if SysParamArray = -1 then
      SysParamArray := CreateIntArray(GlobalScope, '_sysParam', MaxTriggerPlayers, [], 16);
  end;
end;

function SetSysParam(AValue: integer): TInstruction;
begin
  NeedSysParam;
  result := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, simSetTo, AValue)
end;

function CheckSysParam(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
begin
  NeedSysParam;
  result := TIntegerCondition.Create(APlayer, IntArrays[SysParamArray].UnitType, icmExactly, AValue)
end;

function NewSysIP: integer;
begin
  Inc(CurSysIPValue);
  result := CurSysIPValue;

  NeedSysParam;
end;

//stack

var
  SPArrayVar: integer;
  StackArrays: array[1..MaxStackSize] of integer;
  StackSize: integer;     //number of values that can be on the stack (max is MaxArraySize)
  MaxStackBits: integer;  //number of bits for one value on the stack (max is 32)
  ReturnSysIP, PushSysIP, WaitReturnIP: integer; //return and push functions

procedure NeedStack;
begin
  if SPArrayVar = -1 then
  begin
    SPArrayVar := IntArrayIndexOf(GlobalScope, '_sp');
    if SPArrayVar = -1 then
      SPArrayVar := CreateIntArray(GlobalScope, '_sp', MaxTriggerPlayers, [], 16);

    ReturnSysIP := NewSysIP;
    PushSysIP:= NewSysIP;
    WaitReturnIP := NewIP;
  end;
end;

function SetReturnSysIP: TInstruction;
begin
  NeedStack;
  result := SetNextSysIP(ReturnSysIP);
end;

function SetPushSysIP: TInstruction;
begin
  NeedStack;
  result := SetNextSysIP(PushSysIP);
end;

procedure DetermineStackValueSize;
var
  i,depth: Integer;

  function GetDepthRec(AProc: integer; ADepth: integer): integer;
  var j,sub: integer;
  begin
    result := ADepth;
    if Procedures[AProc].StackChecked then raise exception.Create('Recursive calls not allowed (' + Procedures[AProc].Name + ')');
    Procedures[AProc].StackChecked := true;
    if ADepth = MaxStackSize+1 then exit;

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

  if StackSize > MaxStackSize then
  begin
    Writeln('Warning: calls will exceed stack size');
    StackSize := MaxStackSize;
  end;

  for i := 1 to StackSize do
  begin
    StackArrays[i]:= IntArrayIndexOf(GlobalScope, '_stackValue'+inttostr(i));
    if StackArrays[i] = -1 then
      StackArrays[i] := CreateIntArray(GlobalScope, '_stackValue'+inttostr(i), MaxTriggerPlayers, [], 16);
  end;
end;

// write instruction block

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer;
                    AFirstInstr, ALastInstr: integer); forward;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer;
                    AFirstInstr, ALastInstr: integer);
var
  i,j: Integer;
  condStr, instrStr: array of string;
  instrCount: integer;

  addFromSwitch: TAddIntegerFromSwitchesInstruction;
  NextIP, whileIP, beforeIP: integer;
  add2: TSetIntegerInstruction;
  add2prog: TInstructionList;
  switchCheck: TSwitchCondition;
  dropThread: TDropThreadInstruction;
  waitCond: TWaitConditionInstruction;
  fastIf: TFastIfInstruction;
  changeIP: TChangeIPInstruction;
  proc: TInstructionList;
  waitFor: TWaitForPlayersInstruction;
  pl: TPlayer;
  ipCheck: TCondition;
  splitInstr: TSplitInstruction;
  waitFound: Boolean;

begin
  setlength(condStr, length(AConditions) );
  for i := 0 to high(AConditions) do
  begin
    if AConditions[i].IsComputed then
      raise exception.Create('Computed conditions cannot be used as trigger conditions');
    condStr[i] := AConditions[i].ToString;
  end;

  if AIPStart <> -1 then
  begin
    setlength(condStr, length(condStr) + 1);
    condStr[High(condStr)] := CheckIP(AIPStart).ToStringAndFree;
  end;

  setlength(instrStr, ALastInstr - AFirstInstr + 1 + 1);
  instrCount := 0;
  if (AIPStart <> -1) and (AReturnIP <> -1) then
  begin
    i := AFirstInstr;
    waitFound := false;
    while (i <= ALastInstr) and not (AProg[i] is TDropThreadInstruction) and not
        (AProg[i] is TChangeIPInstruction) and not (AProg[i] is TWaitConditionInstruction)
        and not (AProg[i] is TWaitForPlayersInstruction)
        and not (AProg[i] is TSplitInstruction) do
    begin
      if AProg[i] is TWaitInstruction then waitFound := true;
      inc(i);
    end;
    if waitFound then
    begin
      instrStr[0] := SetNextIP(BusyIP).ToStringAndFree;
      inc(instrCount);
    end;
  end;

  for i := AFirstInstr to ALastInstr do
  begin
    if AProg[i] is TAddIntegerFromSwitchesInstruction then
    begin
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
      WriteProg(AOutput, APlayers, [], AProg, NextIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
      exit;
    end else
    if AProg[i] is TDropThreadInstruction then
    begin
      dropThread := TDropThreadInstruction(AProg[i]);

      if dropThread.PlayersToResume <> [] then
      begin
        NextIP := NewIP;
        WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), nextIP, APreserve or (ATempPreserve > 0));

        proc := TInstructionList.Create;
        if APlayers * (dropThread.PlayersToResume - dropThread.PlayersToDrop) <> [] then
          WriteProg(AOutput, APlayers * (dropThread.PlayersToResume - dropThread.PlayersToDrop), [], proc, nextIP, dropThread.ResumeIP, APreserve or (ATempPreserve > 0));

        if APlayers * dropThread.PlayersToDrop <> [] then
          WriteProg(AOutput, APlayers * dropThread.PlayersToDrop, [], proc, nextIP, dropThread.DropIP, APreserve or (ATempPreserve > 0));
        proc.Free;

        //carry on with the rest of the prog
        WriteProg(AOutput, dropThread.PlayersToResume, [], AProg, dropThread.ResumeIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
      end else
        WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), dropThread.DropIP, APreserve or (ATempPreserve > 0));

      exit;
    end else
    if AProg[i] is TWaitForPlayersInstruction then
    begin
      waitFor := TWaitForPlayersInstruction(AProg[i]);

      if (length(AConditions) = 0) and (instrCount = 0) and (AIPStart <> -1) then
        beforeIP := AIPStart
      else
      begin
        beforeIP := NewIP;
        WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), beforeIP, APreserve or (ATempPreserve > 0));
      end;

      proc := TInstructionList.Create;
      whileIP := NewIP;
      WriteProg(AOutput, APlayers, [], proc, beforeIP,whileIP, true);

      if waitFor.AwaitPresenceDefined then
      begin
        switchCheck := TSwitchCondition.Create( BoolVars[ GetPlayerPresenceDefinedVar() ].Switch, false);
        WriteProg(AOutput, APlayers, [switchCheck], proc, whileIP, beforeIP, true);
        switchCheck.Free;
      end;

      switchCheck := TSwitchCondition.Create( -1, true);
      for pl := low(TPlayer) to high(TPlayer) do
        if pl in waitFor.Players then
        begin
          switchCheck.Switch := BoolVars[ GetPlayerPresenceBoolVar(pl) ].Switch;
          ipCheck := TIntegerCondition.Create(pl, IntArrays[IPVar].UnitType, icmAtLeast, 1);
          WriteProg(AOutput, APlayers, [switchCheck, ipCheck], proc, whileIP, beforeIP, true);
        end;

      switchCheck.Free;
      proc.FreeAll;

      //carry on with the rest of the prog
      WriteProg(AOutput, APlayers, [], AProg, whileIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
      exit;
    end else
    if AProg[i] is TWaitConditionInstruction then
    begin
      waitCond := TWaitConditionInstruction(AProg[i]);
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), waitCond.IP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog
      WriteProg(AOutput, APlayers, waitCond.Conditions, AProg, waitCond.IP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
      exit;
    end else
    if AProg[i] is TSplitInstruction then
    begin
      splitInstr := TSplitInstruction(AProg[i]);
      if splitInstr.EndIP = -1 then splitInstr.EndIP := AReturnIP;

      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), splitInstr.EndIP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog
      if splitInstr.ChangePlayers <> [] then APlayers := splitInstr.ChangePlayers;
      WriteProg(AOutput, APlayers, [], AProg, splitInstr.ResumeIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
      exit;
    end else
    if AProg[i] is TChangeIPInstruction then
    begin
      changeIP := TChangeIPInstruction(AProg[i]);
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), changeIP.IP, APreserve or (ATempPreserve > 0));

      //carry on with the rest of the prog
      WriteProg(AOutput, APlayers, [], AProg, changeIP.IP, AReturnIP,
                APreserve, ATempPreserve + changeIP.Preserve,  i+1, ALastInstr);
      exit;
    end else
    if AProg[i] is TFastIfInstruction then
    begin
      NextIP:= NewIP;
      WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), NextIP, APreserve or (ATempPreserve > 0));

      j := i;
      while (j <= ALastInstr) and (AProg[j] is TFastIfInstruction) do
      begin
        fastIf := TFastIfInstruction(AProg[j]);
        WriteProg(AOutput, APlayers, fastIf.Conditions, fastIf.Instructions, NextIP, NextIP, APreserve, ATempPreserve);
        inc(j);
      end;

      //carry on with the rest of the prog
      WriteProg(AOutput, APlayers, [], AProg, NextIP, AReturnIP, APreserve, ATempPreserve, j, ALastInstr);
      exit;
    end;
    instrStr[instrCount] := AProg[i].ToString;
    inc(instrCount);
  end;

  WriteTrigger(AOutput, APlayers, condStr, slice(instrStr, instrCount), AReturnIP, APreserve or (ATempPreserve > 0));
end;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);
begin
  WriteProg(AOutput, APlayers, AConditions, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve, 0, AProg.Count-1);
end;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer;
                    AFirstInstr, ALastInstr: integer);
var cond: array of TCondition;
  i: Integer;
begin
  setlength(cond,AConditions.Count);
  for i := 0 to AConditions.Count-1 do
    cond[i] := AConditions[i];
  WriteProg(AOutput, APlayers, cond, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve, AFirstInstr, ALastInstr);
end;

procedure WriteProg(AOutput: TStringList; APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);
begin
  WriteProg(AOutput, APlayers, AConditions, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve, 0, AProg.Count-1);
end;

procedure AddSysCall(AInstructions: TInstructionList; AReturnIP, ACalledIP: integer);
begin
  AInstructions.Add(SetSysParam(AReturnIP));
  AInstructions.Add(SetPushSysIP);
  AInstructions.Add(TSplitInstruction.Create(AReturnIP, ACalledIP));
end;

procedure AddSysReturn(AInstructions: TInstructionList);
begin
  AInstructions.Add( SetReturnSysIP );
  AInstructions.Add( TSplitInstruction.Create(NewIP, WaitReturnIP));
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
  if SPArrayVar = -1 then exit;

  DetermineStackValueSize;

  proc := TInstructionList.Create;
  cond := TConditionList.Create;

  AOutput.Add('// Return //');
  //return handler

  //SP -= 1, IP := 0
  returnCond := CheckSysIP(ReturnSysIP);
  cond.Add(returnCond);
  proc.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, simSubtract, 1));
  proc.Add(SetNextIP(0));
  WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
  EmptyProc;

  //copy stack value to IP
  spCond := TIntegerCondition.Create(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, icmExactly, 0);
  valCond := TIntegerCondition.Create(plCurrentPlayer, '', icmAtLeast, 0);
  cond.Add(spCond);
  cond.Add(valCond);
  for sp := 0 to StackSize-1 do
  begin
    valCond.UnitType := IntArrays[StackArrays[sp+1]].UnitType;

    subStackVal := TSetIntegerInstruction.Create(plCurrentPlayer, valCond.UnitType, simSubtract, 0);
    addIP := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[IPVar].UnitType, simAdd, 0);
    proc.Add(subStackVal);
    proc.Add(addIP);

    spCond.Value := sp;
    for bit := MaxStackBits-1 downto 0 do
    begin
      valCond.Value := 1 shl bit;

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
  cond.Add(TIntegerCondition.Create(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, icmAtLeast, StackSize));
  proc.Add(TDisplayTextMessageInstruction.Create(True, 'Stack overflow'));
  proc.Add(TWaitInstruction.Create(4000));
  WriteProg(AOutput, [plAllPlayers], cond, proc, -1, -1, True);
  EmptyCond;
  EmptyProc;

  //push handler
  pushCond := CheckSysIP(PushSysIP);
  spCond := TIntegerCondition.Create(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, icmExactly, 0);
  valCond := TIntegerCondition.Create(plCurrentPlayer, IntArrays[SysParamArray].UnitType, icmAtLeast, 0);
  cond.Add(pushCond);
  cond.Add(spCond);
  cond.Add(valCond);
  for sp := 0 to StackSize-1 do
  begin
    spCond.Value:= sp;
    addStackVal := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[StackArrays[sp+1]].UnitType, simAdd, 0);
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

    proc.add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, simAdd, 1));
    proc.Add(SetNextSysIP(0));
    WriteProg(AOutput, [plAllPlayers], [pushCond, spCond], proc, -1, -1, True);
    EmptyProc;
  end;

  cond.Free;
  proc.Free;
end;

procedure InitTriggerCode;
begin
  IPVar := IntArrayIndexOf(GlobalScope, '_ip');
  if IPVar = -1 then
    IPVar := CreateIntArray(GlobalScope, '_ip', MaxTriggerPlayers, [], 16);
  CurIPValue := 0;

  //IntArrays[IPVar].UnitType:= 'Gas'; //debug

  CurSysIPValue:= 0;
  SysIPVar := -1;

  SPArrayVar := -1;
  ReturnSysIP := -1;
  PushSysIP:= -1;
  SysParamArray := -1;

  BusyIP := NewIP;
end;

end.

