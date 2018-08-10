unit uarithmetic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes;

const
  MaxTempBools = MaxSwitches;

procedure ExpandIntegerTransfer(ATransfer: TTransferIntegerInstruction; AExpanded: TInstructionList);
function CompareAccumulator(AMode: TIntegerConditionMode; AValue: integer): TCondition;
procedure InitArithmetic;
procedure WriteArithmeticTriggers(AOutput: TStringList);
function GetExponentOf2(AValue: integer): integer;
function IsPowerOf2(ANumber: integer): boolean;

var
  TempBools: array[0..MaxTempBools-1] of integer;
  TempBoolCount: integer;
  ArithmeticMaxInputBits, ExtraShiftBits: integer;

procedure NeedTempBools(AQuantity: integer);

implementation

uses utriggercode, uvariables, math;

procedure NeedTempBools(AQuantity: integer);
var
  idx: Integer;
begin
  if AQuantity > MaxTempBools then
    raise exception.Create('Too many temporary booleans');

  while TempBoolCount < AQuantity do
  begin
    idx := CreateBoolVar('_bool'+intToStr(TempBoolCount+1), svClear);
    TempBools[TempBoolCount] := idx;
    inc(TempBoolCount);
  end;
end;

function GetExponentOf2(AValue: integer): integer;
begin
  result := 0;
  while AValue > 1 do
  begin
    AValue := AValue shr 1;
    inc(result);
  end;
end;

function IsPowerOf2(ANumber: integer): boolean;
begin
  if ANumber <= 0 then exit(false);
  while ANumber > 1 do
  begin
    if (ANumber and 1) <> 0 then exit(false);
    ANumber := ANumber shr 1;
  end;
  exit(ANumber = 1);
end;

var
  AddIntoAccSysIP, SubtractIntoAccSysIP, SubtractAccSysIP, AccArray: integer;
  AddAccSysIP: array[0..23] of integer;
  TransferProcs: array of record
    AddIntoAcc,SubIntoAcc,AddAcc,AddShifted,SubAcc: boolean;
    BitCount: integer;
  end;

procedure NeedAcc;
begin
  if AccArray = -1 then
  begin
    AccArray := IntArrayIndexOf('_accumulator');
    if AccArray = -1 then
      AccArray := CreateIntArray('_accumulator', MaxTriggerPlayers, [], 32);
  end;
end;

procedure NeedTransfer;
var
  i: Integer;
begin
  if AddIntoAccSysIP = -1 then
  begin
    AddIntoAccSysIP:= NewSysIP;
    for i := 0 to 23 do
      AddAccSysIP[i]:= NewSysIP;
    SubtractIntoAccSysIP:= NewSysIP;
    SubtractAccSysIP:= NewSysIP;

    NeedAcc;

    TransferProcs := nil;
  end;
end;

function GetTransferParam(APlayer: TPlayer; AUnitType: string; ASysIP: integer): integer;
var
  i: Integer;
begin
  if IntVarCount > length(TransferProcs) then
  begin
    i := length(TransferProcs);
    setlength(TransferProcs, IntVarCount);
    while i < length(TransferProcs) do
    begin
      TransferProcs[i].BitCount:= IntVars[i].BitCount;
      inc(i);
    end;
  end;
  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Player = APlayer) and (IntVars[i].UnitType = AUnitType) then
    begin
      if ASysIP = AddIntoAccSysIP then
      begin
        TransferProcs[i].AddIntoAcc:= true;
        if IntVars[i].BitCount > ArithmeticMaxInputBits then
          ArithmeticMaxInputBits:= IntVars[i].BitCount;
      end;
      if ASysIP = SubtractIntoAccSysIP then
      begin
        TransferProcs[i].SubIntoAcc:= true;
        if IntVars[i].BitCount > ArithmeticMaxInputBits then
          ArithmeticMaxInputBits:= IntVars[i].BitCount;
      end;
      if ASysIP = AddAccSysIP[0] then
        TransferProcs[i].AddAcc:= true;
      if (ASysIP > AddAccSysIP[0]) and (ASysIP <= AddAccSysIP[23]) then
      begin
        TransferProcs[i].AddAcc:= true;
        TransferProcs[i].AddShifted:= true;
        ExtraShiftBits := max(ASysIP - AddAccSysIP[0], ExtraShiftBits);
      end;
      if ASysIP = SubtractAccSysIP then
        TransferProcs[i].SubAcc:= true;

      exit(i);
    end;
  raise exception.Create('Unable to find variable');
end;

procedure WriteArithmeticTriggers(AOutput: TStringList);
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
  condSwitch, condSub: TSwitchCondition;
  addAcc, setValue: TSetIntegerInstruction;
  condValue: TIntegerCondition;
  condVar, condIP2: TCondition;
  setSw: TSetSwitchInstruction;
  switchCopyVarToBits, switchAddToVarFromBits,
  switchCopyAccToBits, switchNegateBits2,
  switchAddToAccFromBits, switchSubtractIntoAccFromBits,
  switchClearSysIP, switchShift16, switchShift8, switchShift4,
  switchShift2, switchShift1, switchShiftOverflow: integer;
  hasAddFromBits, hasSubFromBits, hasAddAcc, hasSubAcc, hasShift: boolean;
  TotalBits,ArithmeticMaxAccBits: integer;

  procedure WriteShiftProg(AShift: integer; ASwitchSub: integer);
  var j: integer;
  begin
    condSub := TSwitchCondition.Create(ASwitchSub,true);
    condSwitch := TSwitchCondition.Create(-1, true);
    proc.Add(TSetSwitchInstruction.Create(switchShiftOverflow, svSet));
    for j := TotalBits-1 downto TotalBits-AShift do
    if (j >= 0) and (j < ArithmeticMaxAccBits) then
    begin
      condSwitch.Switch := BoolVars[TempBools[j]].Switch;
      WriteProg(AOutput,[plAllPlayers], [condSub,condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;

    setSw := TSetSwitchInstruction.Create(-1, svSet);
    proc.Add(setSw);
    for j := TotalBits-AShift-1 downto 0 do
    if j < ArithmeticMaxAccBits then
    begin
      condSwitch.Value := true;
      condSwitch.Switch := BoolVars[TempBools[j]].Switch;
      setSw.Switch:= BoolVars[TempBools[j+AShift]].Switch;
      setSw.Value := svSet;
      WriteProg(AOutput,[plAllPlayers], [condSub,condSwitch], proc, -1,-1, True);
      condSwitch.Value := false;
      setSw.Value := svClear;
      WriteProg(AOutput,[plAllPlayers], [condSub,condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;

    for j := min(AShift-1, ArithmeticMaxAccBits-1) downto 0 do
      proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[j]].Switch, svClear));
    WriteProg(AOutput,[plAllPlayers], [condSub], proc, -1,-1,true);
    EmptyProc;

    condSub.Free;
  end;

begin
  if AddIntoAccSysIP = -1 then exit;

  TotalBits := min(ArithmeticMaxInputBits+ExtraShiftBits,24);
  ArithmeticMaxAccBits := min(ArithmeticMaxInputBits+4,TotalBits);
  NeedTempBools(TotalBits+13);
  switchCopyVarToBits := BoolVars[TempBools[TotalBits]].Switch;
  switchAddToVarFromBits := BoolVars[TempBools[TotalBits+1]].Switch;
  switchCopyAccToBits := BoolVars[TempBools[TotalBits+2]].Switch;;
  switchNegateBits2 := BoolVars[TempBools[TotalBits+3]].Switch;
  switchAddToAccFromBits := BoolVars[TempBools[TotalBits+4]].Switch;
  switchSubtractIntoAccFromBits := BoolVars[TempBools[TotalBits+5]].Switch;
  switchClearSysIP := BoolVars[TempBools[TotalBits+6]].Switch;
  switchShift16 := BoolVars[TempBools[TotalBits+7]].Switch;
  switchShift8 := BoolVars[TempBools[TotalBits+8]].Switch;
  switchShift4 := BoolVars[TempBools[TotalBits+9]].Switch;
  switchShift2 := BoolVars[TempBools[TotalBits+10]].Switch;
  switchShift1 := BoolVars[TempBools[TotalBits+11]].Switch;
  switchShiftOverflow := BoolVars[TempBools[TotalBits+12]].Switch;

  hasAddFromBits:= false;
  hasSubFromBits:= false;
  hasAddAcc := false;
  hasSubAcc := false;
  hasShift := false;
  for i := 0 to high(TransferProcs) do
  begin
    if TransferProcs[i].AddIntoAcc then hasAddFromBits:= true;
    if TransferProcs[i].SubIntoAcc then hasSubFromBits:= true;
    if TransferProcs[i].AddAcc then hasAddAcc:= true;
    if TransferProcs[i].SubAcc then hasSubAcc:= true;
    if TransferProcs[i].AddShifted then hasShift:= true;
  end;

  proc := TInstructionList.Create;

  AOutput.Add('// Add or subtract into accumulator //');;

  //clear temp bits
  for i := 0 to TotalBits-1 do
    proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[i]].Switch, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchCopyVarToBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchCopyAccToBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchNegateBits2, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchAddToAccFromBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchSubtractIntoAccFromBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchShift16, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchShift8, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchShift4, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchShift2, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchShift1, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchShiftOverflow, svClear));

  WriteProg(AOutput, [plAllPlayers], [], proc, -1, -1, True);
  EmptyProc;

  if hasAddFromBits then
  begin
    //adding into accumulator: copying to bits and adding to accumualtor
    condIP := CheckSysIP(AddIntoAccSysIP);
    proc.Add(TSetSwitchInstruction.Create(switchCopyVarToBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToAccFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svSet));
    WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
    EmptyProc;
    condIP.Free;
  end;

  if hasSubFromBits then
  begin
    //subtracting into accumulator: copying to bits and subtracting to accumualtor
    condIP := CheckSysIP(SubtractIntoAccSysIP);
    proc.Add(TSetSwitchInstruction.Create(switchCopyVarToBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchSubtractIntoAccFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svSet));
    WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
    EmptyProc;
    condIP.Free;
  end;

  //copy source variable into temp bits (and later restore it)
  condSub := TSwitchCondition.Create(switchCopyVarToBits,true);
  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddIntoAcc or TransferProcs[i].SubIntoAcc then
  begin
    //transfer to bools
    condVar := CheckSysParam(i);
    condValue := TIntegerCondition.Create(IntVars[i].Player, IntVars[i].UnitType, icmAtLeast, 0);
    addAcc := TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSubtract, 0);
    setSw := TSetSwitchInstruction.Create(0, svSet);
    proc.Add(addAcc);
    proc.Add(setSw);
    for j := min(TransferProcs[i].BitCount-1, ArithmeticMaxInputBits-1) downto 0 do
    begin
      condValue.Value := 1 shl j;
      addAcc.Value := condValue.Value;
      setSw.Switch := BoolVars[TempBools[j]].Switch;
      WriteProg(AOutput, [plAllPlayers], [condSub, condVar, condValue], proc, -1,-1, True);
    end;
    EmptyProc;
    condValue.Free;
  end;
  condSub.Free;

  if hasAddFromBits then
  begin
    //adding temp bits to accumulator
    condSub := TSwitchCondition.Create(switchAddToAccFromBits,true);
    condSwitch := TSwitchCondition.Create(0,true);
    addAcc := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, 0);
    proc.Add(addAcc);
    for i := ArithmeticMaxInputBits-1 downto 0 do
    begin
      condSwitch.Switch := BoolVars[TempBools[i]].Switch;
      addAcc.Value := 1 shl i;
      WriteProg(AOutput, [plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;
    condSwitch.Free;
    condSub.Free;
  end;

  if hasSubFromBits then
  begin
    //subtracting temp bits from accumulator
    condSub := TSwitchCondition.Create(switchSubtractIntoAccFromBits,true);
    condSwitch := TSwitchCondition.Create(0,true);
    addAcc := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, 0);
    proc.Add(addAcc);
    for i := ArithmeticMaxInputBits-1 downto 0 do
    begin
      condSwitch.Switch := BoolVars[TempBools[i]].Switch;
      addAcc.Value := 1 shl i;
      WriteProg(AOutput, [plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;
    condSwitch.Free;
    condSub.Free;
  end;

  if hasShift then
  begin
    AOutput.Add('// Init bit shifting //');
    if ExtraShiftBits >= 16 then
    begin
      CheckSysIPRange(AddAccSysIP[16],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(16,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift16, svSet));
      WriteProg(AOutput, [plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ExtraShiftBits >= 8 then
    begin
      CheckSysIPRange(AddAccSysIP[8],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(8,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift8, svSet));
      WriteProg(AOutput, [plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ExtraShiftBits >= 4 then
    begin
      CheckSysIPRange(AddAccSysIP[4],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(4,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift4, svSet));
      WriteProg(AOutput, [plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ExtraShiftBits >= 2 then
    begin
      CheckSysIPRange(AddAccSysIP[2],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(2,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift2, svSet));
      WriteProg(AOutput, [plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    CheckSysIPRange(AddAccSysIP[1],AddAccSysIP[23],condIP,condIP2);
    proc.Add(SetNextSysIP(1,simSubtract));
    proc.Add(TSetSwitchInstruction.Create(switchShift1, svSet));
    WriteProg(AOutput, [plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
    EmptyProc; condIP.Free; condIP2.Free;
  end;

  AOutput.Add('// Add or subtract from accumulator //');

  //checking initial overflow
  condIP := CheckSysIP(AddAccSysIP[0]);
  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddAcc then
  begin
    condVar := CheckSysParam(i);
    condValue := TIntegerCondition.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, 1 shl TransferProcs[i].BitCount);
    proc.Add(TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSetTo, (1 shl TransferProcs[i].BitCount)-1));
    proc.Add(SetNextSysIP(0));
    WriteProg(AOutput, [plAllPlayers], [condIP, condVar, condValue], proc, -1,-1, True);
    EmptyProc;
    condValue.Free;
    condVar.Free;
  end;
  condIP.Free;

  if hasAddAcc then
  begin
    //adding from accumulator: copying accumulator to bits and adding from bits
    condIP := CheckSysIP(AddAccSysIP[0]);
    proc.Add(TSetSwitchInstruction.Create(switchCopyAccToBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svSet));
    WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
    EmptyProc;
    condIP.Free;
  end;

  if hasSubAcc then
  begin
    //subtracting from accumulator: copying accumulator to bits, negating bits, adding from bits, subtracting not 0
    condIP := CheckSysIP(SubtractAccSysIP);
    proc.Add(TSetSwitchInstruction.Create(switchCopyAccToBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchNegateBits2, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svSet));
    WriteProg(AOutput, [plAllPlayers], [condIP], proc, -1, -1, True);
    EmptyProc;
    condIP.Free;
  end;

  if hasAddAcc or hasSubAcc then
  begin
    //copy acc to bits
    condSub := TSwitchCondition.Create(switchCopyAccToBits,true);
    condValue := TIntegerCondition.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, 0);
    addAcc := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, 0);
    setSw := TSetSwitchInstruction.Create(0, svSet);
    proc.Add(addAcc);
    proc.Add(setSw);
    for j := ArithmeticMaxAccBits-1 downto 0 do
    begin
      condValue.Value := 1 shl j;
      addAcc.Value := condValue.Value;
      setSw.Switch := BoolVars[TempBools[j]].Switch;
      WriteProg(AOutput, [plAllPlayers], [condSub, condValue], proc, -1,-1, True);
    end;
    EmptyProc;
    condValue.Free;

    if hasShift then // restore accumulator for successive shifted adds
    begin
      condSwitch := TSwitchCondition.Create(0,true);
      addAcc := TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, 0);
      proc.Add(addAcc);
      for j := ArithmeticMaxAccBits-1 downto 0 do
      begin
        condSwitch.Switch := BoolVars[TempBools[j]].Switch;
        addAcc.Value := 1 shl j;
        WriteProg(AOutput, [plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
      end;
      EmptyProc;
      condSwitch.Free;
    end;
    condSub.Free;
  end;

  if hasShift then
  begin
    AOutput.Add('// Handle bit shifting //');
    if ExtraShiftBits >= 16 then WriteShiftProg(16, switchShift16);
    if ExtraShiftBits >= 8 then WriteShiftProg(8, switchShift8);
    if ExtraShiftBits >= 4 then WriteShiftProg(4, switchShift4);
    if ExtraShiftBits >= 2 then WriteShiftProg(2, switchShift2);
    WriteShiftProg(1, switchShift1);

    condSub := TSwitchCondition.Create(switchShiftOverflow,true);
    for i := 0 to TotalBits-1 do
      proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[i]].Switch, svSet));
    WriteProg(AOutput, [plAllPlayers], [condSub], proc, -1,-1, True);
    EmptyProc;
    condSub.Free;
  end;

  if hasSubAcc then
  begin
    //negating bits
    condSub := TSwitchCondition.Create(switchNegateBits2,true);
    for i := 0 to TotalBits-1 do
      proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[i]].Switch, svToggle));
    WriteProg(AOutput, [plAllPlayers], [condSub], proc, -1,-1, True);
    EmptyProc;
    condSub.Free;
  end;

  //restore source variable or add to it
  condSub := TSwitchCondition.Create(switchAddToVarFromBits,true);
  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddIntoAcc or TransferProcs[i].SubIntoAcc or TransferProcs[i].AddAcc or TransferProcs[i].SubAcc then
  begin
    condVar := CheckSysParam(i);
    condSwitch := TSwitchCondition.Create(0,true);
    addAcc := TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simAdd, 0);
    proc.Add(addAcc);
    for j := min(TransferProcs[i].BitCount-1, TotalBits-1) downto 0 do
    begin
      condSwitch.Switch := BoolVars[TempBools[j]].Switch;
      addAcc.Value := 1 shl j;
      WriteProg(AOutput, [plAllPlayers], [condSub, condVar, condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;
    condSwitch.Free;
    condVar.Free;
  end;
  condSub.Free;

  //checking resulting overflow
  condIP := CheckSysIP(AddAccSysIP[0]);
  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddAcc then
  begin
    condVar := CheckSysParam(i);
    condValue := TIntegerCondition.Create(IntVars[i].Player, IntVars[i].UnitType, icmAtLeast, 1 shl TransferProcs[i].BitCount);
    setValue := TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSetTo, (1 shl TransferProcs[i].BitCount)-1);
    proc.Add(setValue);
    WriteProg(AOutput, [plAllPlayers], [condIP, condVar, condValue], proc, -1,-1, True);
    EmptyProc;
    condValue.Free;
    condVar.Free;
  end;
  condIP.Free;

  if hasSubAcc then
  begin
    //subtracting not 0
    condSub := TSwitchCondition.Create(switchNegateBits2,true);
    for i := 0 to high(TransferProcs) do
    if TransferProcs[i].SubIntoAcc then
    begin
      condVar := CheckSysParam(i);
      proc.Add( TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSubtract, (1 shl min(TransferProcs[i].BitCount, TotalBits) )-1) );
      WriteProg(AOutput, [plAllPlayers], [condSub, condVar], proc, -1,-1, True);
      EmptyProc;
      condVar.Free;
    end;
    condSub.Free;
  end;

  condSub := TSwitchCondition.Create(switchClearSysIP,true);
  proc.Add(SetNextSysIP(0));
  WriteProg(AOutput, [plAllPlayers], [condSub], proc, -1,-1, True);
  EmptyProc;
  condSub.Free;


  proc.Free;

end;

procedure ExpandIntegerTransfer(ATransfer: TTransferIntegerInstruction; AExpanded: TInstructionList);
var
  nextIP: Integer;
begin
  if ATransfer.UnitType = 'Const' then
  begin
    NeedAcc;
    case ATransfer.Action of
    itCopyIntoAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, ATransfer.Value));
    itAddIntoAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, ATransfer.Value));
    itSubtractIntoAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, ATransfer.Value));
    itRandomizeAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simRandomize, ATransfer.Value));
    itLimitAccumulator: AExpanded.Add(TFastIfInstruction.Create([TIntegerCondition.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, ATransfer.Value+1)],
                                         [TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, ATransfer.Value)]) );
    else
      raise exception.Create('Unhandled case');
    end;
  end else
  begin
    NeedTransfer;
    case ATransfer.Action of
    itCopyIntoAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, 0));
    itCopyAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(ATransfer.Player, ATransfer.UnitType, simSetTo, 0));
    end;
    nextIP := -1;
    case ATransfer.Action of
    itCopyIntoAccumulator, itAddIntoAccumulator: nextIP := AddIntoAccSysIP;
    itCopyAccumulator,itAddAccumulator:
      begin
        nextIP := AddAccSysIP[ATransfer.Shift];
      end;
    itSubtractIntoAccumulator: nextIP := SubtractIntoAccSysIP;
    itSubtractAccumulator: nextIP := SubtractAccSysIP;
    end;
    if nextIP <> -1 then
    begin
      AExpanded.Add(SetSysParam(GetTransferParam(ATransfer.Player,ATransfer.UnitType,nextIP)));
      AExpanded.Add(SetNextSysIP(nextIP));
      AExpanded.Add(TWaitConditionInstruction.Create(CheckSysIP(0), NewIP));
    end;
  end;
end;

function CompareAccumulator(AMode: TIntegerConditionMode; AValue: integer
  ): TCondition;
begin
  NeedAcc;
  result := TIntegerCondition.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, AMode, AValue);
end;

procedure InitArithmetic;
var
  i: Integer;
begin
  TempBoolCount:= 0;
  AddIntoAccSysIP:= -1;
  for i := 0 to 23 do
    AddAccSysIP[i]:= -1;
  AccArray := -1;
  ArithmeticMaxInputBits:= 0;
  ExtraShiftBits:= 0;
end;


end.

