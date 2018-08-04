unit uarithmetic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions;

const
  ArithmeticMaxBits = 8;
  MaxTempBools = 32;

procedure ExpandIntegerTransfer(ATransfer: TTransferIntegerInstruction; AExpanded: TInstructionList);
procedure InitArithmetic;
procedure WriteArithmeticTriggers(AOutput: TStringList);
function GetExponentOf2(AValue: integer): integer;
function IsPowerOf2(ANumber: integer): boolean;

var
  TempBools: array[0..MaxTempBools-1] of integer;
  TempBoolCount: integer;

procedure NeedTempBools(AQuantity: integer);

implementation

uses utriggercode, uvariables, usctypes;

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
  condSwitch, condRestore: TSwitchCondition;
  addAcc: TSetIntegerInstruction;
  condValue: TIntegerCondition;
  condVar: TCondition;
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
    condVar := CheckSysParam(i);
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
    condVar := CheckSysParam(i);
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

procedure ExpandIntegerTransfer(ATransfer: TTransferIntegerInstruction; AExpanded: TInstructionList);
var
  nextIP: Integer;
begin
  NeedTransfer;
  case ATransfer.Action of
  itCopyToAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, 0));
  itCopyFromAccumulator: AExpanded.Add(TSetIntegerInstruction.Create(ATransfer.Player, ATransfer.UnitType, simSetTo, 0));
  end;
  nextIP := -1;
  case ATransfer.Action of
  itCopyToAccumulator, itAddToAccumulator: nextIP := AddToAccSysIP;
  itCopyFromAccumulator, itAddFromAccumulator: nextIP := AddFromAccSysIP;
  end;
  if nextIP <> -1 then
  begin
    AExpanded.Add(SetSysParam(GetTransferParam(ATransfer.Player,ATransfer.UnitType,nextIP)));
    AExpanded.Add(SetNextSysIP(nextIP));
    AExpanded.Add(TWaitConditionInstruction.Create(CheckSysIP(0), NewIP));
  end;
end;

procedure InitArithmetic;
begin
  TempBoolCount:= 0;
  AddToAccSysIP:= -1;
  AddFromAccSysIP:= -1;
end;


end.

