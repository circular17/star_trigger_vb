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
  ArithmeticMaxBits: integer;

procedure NeedTempBools(AQuantity: integer);

implementation

uses utriggercode, uvariables;

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
  AddIntoAccSysIP, AddAccSysIP, SubtractIntoAccSysIP, SubtractAccSysIP, AccArray: integer;
  TransferProcs: array of record
    AddIntoAcc,SubIntoAcc,AddAcc,SubAcc: boolean;
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
    AddAccSysIP:= NewSysIP;
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
        TransferProcs[i].AddIntoAcc:= true;
      if ASysIP = SubtractIntoAccSysIP then
        TransferProcs[i].SubIntoAcc:= true;
      if ASysIP = AddAccSysIP then
        TransferProcs[i].AddAcc:= true;
      if ASysIP = SubtractAccSysIP then
        TransferProcs[i].SubAcc:= true;

      if IntVars[i].BitCount > ArithmeticMaxBits then ArithmeticMaxBits:= IntVars[i].BitCount;
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
  condVar: TCondition;
  setSw: TSetSwitchInstruction;
  switchCopyVarToBits, switchAddToVarFromBits,
  switchCopyAccToBits, switchNegateBits2,
  switchAddToAccFromBits, switchSubtractIntoAccFromBits,
  switchClearSysIP: integer;
  hasAddFromBits, hasSubFromBits, hasAddAcc, hasSubAcc: boolean;
begin
  if AddIntoAccSysIP = -1 then exit;

  NeedTempBools(ArithmeticMaxBits+7);
  switchCopyVarToBits := BoolVars[TempBools[ArithmeticMaxBits]].Switch;
  switchAddToVarFromBits := BoolVars[TempBools[ArithmeticMaxBits+1]].Switch;
  switchCopyAccToBits := BoolVars[TempBools[ArithmeticMaxBits+3]].Switch;;
  switchNegateBits2 := BoolVars[TempBools[ArithmeticMaxBits+2]].Switch;
  switchAddToAccFromBits := BoolVars[TempBools[ArithmeticMaxBits+4]].Switch;
  switchSubtractIntoAccFromBits := BoolVars[TempBools[ArithmeticMaxBits+5]].Switch;
  switchClearSysIP := BoolVars[TempBools[ArithmeticMaxBits+6]].Switch;

  hasAddFromBits:= false;
  hasSubFromBits:= false;
  hasAddAcc := false;
  hasSubAcc := false;
  for i := 0 to high(TransferProcs) do
  begin
    if TransferProcs[i].AddIntoAcc then hasAddFromBits:= true;
    if TransferProcs[i].SubIntoAcc then hasSubFromBits:= true;
    if TransferProcs[i].AddAcc then hasAddAcc:= true;
    if TransferProcs[i].SubAcc then hasSubAcc:= true;
  end;

  proc := TInstructionList.Create;

  AOutput.Add('// Add or subtract into accumulator //');;

  //clear temp bits
  for i := 0 to ArithmeticMaxBits-1 do
    proc.Add(TSetSwitchInstruction.Create(BoolVars[TempBools[i]].Switch, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchCopyVarToBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchCopyAccToBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchNegateBits2, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchAddToAccFromBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchSubtractIntoAccFromBits, svClear));
  proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svClear));

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
    for j := TransferProcs[i].BitCount-1 downto 0 do
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
    for i := ArithmeticMaxBits-1 downto 0 do
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
    for i := ArithmeticMaxBits-1 downto 0 do
    begin
      condSwitch.Switch := BoolVars[TempBools[i]].Switch;
      addAcc.Value := 1 shl i;
      WriteProg(AOutput, [plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;
    condSwitch.Free;
    condSub.Free;
  end;

  AOutput.Add('// Add or subtract from accumulator //');

  //checking initial overflow
  condIP := CheckSysIP(AddAccSysIP);
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
    condIP := CheckSysIP(AddAccSysIP);
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
    for j := ArithmeticMaxBits-1 downto 0 do
    begin
      condValue.Value := 1 shl j;
      addAcc.Value := condValue.Value;
      setSw.Switch := BoolVars[TempBools[j]].Switch;
      WriteProg(AOutput, [plAllPlayers], [condSub, condValue], proc, -1,-1, True);
    end;
    EmptyProc;
    condValue.Free;
    condSub.Free;
  end;

  if hasSubAcc then
  begin
    //negating bits
    condSub := TSwitchCondition.Create(switchNegateBits2,true);
    for i := 0 to ArithmeticMaxBits-1 do
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
    for j := TransferProcs[i].BitCount-1 downto 0 do
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
  condIP := CheckSysIP(AddAccSysIP);
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
      proc.Add( TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSubtract, (1 shl ArithmeticMaxBits)-1) );
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
    itCopyAccumulator, itAddAccumulator: nextIP := AddAccSysIP;
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
begin
  TempBoolCount:= 0;
  AddIntoAccSysIP:= -1;
  AddAccSysIP:= -1;
  AccArray := -1;
  ArithmeticMaxBits:= 0;
end;


end.

