unit uarithmetic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes, uscope;

const
  MaxTempBools = MaxSwitches;

procedure ExpandIntegerTransfer(ATransfer: TTransferIntegerInstruction; AExpanded: TInstructionList);
function CompareAccumulator(AMode: TIntegerConditionMode; AValue: integer): TCondition;
function IsAccumulatorVariable({%H-}APlayer: TPlayer; AUnit: TStarcraftUnit): boolean;
procedure InitArithmetic;
procedure WriteArithmeticTriggers;
function GetExponentOf2(AValue: integer): integer;
function IsPowerOf2(ANumber: integer): boolean;

var
  TempBools: array[0..MaxTempBools-1] of record
    BoolVar: integer;
    Used: boolean
  end;
  TempBoolCount: integer;
  ArithmeticScopeValue: integer;
  ArithmeticMaxInputBits, ExtraShiftBits: integer;
  ShiftUsed1, ShiftUsed2, ShiftUsed4, ShiftUsed8, ShiftUsed16: boolean;

function GetArithmeticScope: integer;
function AllocateTempBool: integer;
procedure ReleaseTempBool(ASwitch: integer);
function GetMultiplicandIntArray(AMaxBitCount: integer): integer;

type
  { TCallFunctionCondition }

  TCallFunctionCondition = class(TCondition)
    Scope: integer;
    Name: string;
    ParamValues: ArrayOfParameterValue;
    function IsArithmetic: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean); override;
    function ToBasic({%H-}AUseVariables: boolean): string; override;
    function Priority: integer; override;
    function Duplicate: TCondition; override;
    constructor Create(AScope: integer; AName: string; AParamValues: ArrayOfParameterValue);
    destructor Destroy; override;
  end;

implementation

uses utriggercode, uvariables, math, utriggerinstructions, utriggerconditions;

{ TCallFunctionCondition }

function TCallFunctionCondition.IsArithmetic: boolean;
begin
  Result:= true;
end;

procedure TCallFunctionCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean);
begin
  if AFirst then
  begin
    AProg.Add(TCallInstruction.Create(Scope, Name, DuplicateParameterValues(ParamValues), 'Boolean'));
    if not IsAccumulatorVariable(APlayer, AUnitType) then
    begin
      AProg.Add(TIfInstruction.Create(CreateIntegerCondition(APlayer, AUnitType, icmAtLeast, 1)));
      AProg.Add(TTransferIntegerInstruction.Create(1, itCopyIntoAccumulator));
      AProg.Add(TElseInstruction.Create);
      AProg.Add(TTransferIntegerInstruction.Create(0, itCopyIntoAccumulator));
      AProg.Add(TEndIfInstruction.Create);
    end;
  end else
  begin
    AProg.Add(TIfInstruction.Create(CreateIntegerCondition(APlayer, AUnitType, icmAtLeast, 1)) );
    AProg.Add(TCallInstruction.Create(Scope, Name, DuplicateParameterValues(ParamValues), 'Boolean'));
    if not IsAccumulatorVariable(APlayer, AUnitType) then
      AProg.Add(TFastIfInstruction.Create([CreateIntegerCondition(APlayer, AUnitType, icmAtMost, 0)],
              [TTransferIntegerInstruction.Create(0, itCopyIntoAccumulator)]));
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

function TCallFunctionCondition.ToBasic(AUseVariables: boolean): string;
begin
  result := Name+'()';
end;

function TCallFunctionCondition.Priority: integer;
begin
  result := 10;
end;

function TCallFunctionCondition.Duplicate: TCondition;
begin
  result := TCallFunctionCondition.Create(Scope, Name, DuplicateParameterValues(ParamValues));
end;

constructor TCallFunctionCondition.Create(AScope: integer;
  AName: string; AParamValues: ArrayOfParameterValue);
var
  i: Integer;
begin
  Scope := AScope;
  Name := AName;
  setlength(ParamValues, length(AParamValues));
  for i := 0 to high(ParamValues) do
    ParamValues[i] := AParamValues[i];
end;

destructor TCallFunctionCondition.Destroy;
begin
  FreeParameterValues(ParamValues);
  inherited Destroy;
end;

function GetMultiplicandIntArray(AMaxBitCount: integer): integer;
begin
  result := IntArrayIndexOf(GetArithmeticScope,'Multiplicand');
  if result = -1 then
    result := CreateIntArray(GetArithmeticScope,'Multiplicand', MaxTriggerPlayers, [], AMaxBitCount)
  else
    if IntArrays[result].BitCount < AMaxBitCount then IntArrays[result].BitCount := AMaxBitCount;
end;

function GetArithmeticScope: integer;
begin
  if ArithmeticScopeValue = -1 then
    ArithmeticScopeValue := NewScope(RunTimeScope, 'Arithmetic');
  result := ArithmeticScopeValue;
end;

function AllocateTempBool: integer;
var
  idx, i: Integer;
begin
  for i := 0 to TempBoolCount-1 do
    if not TempBools[i].Used then
    begin
      TempBools[i].Used := true;
      exit(BoolVars[TempBools[i].BoolVar].Switch);
    end;

  if TempBoolCount >= MaxTempBools then
    raise exception.Create('Too many temporary booleans');

  idx := CreateBoolVar(GetArithmeticScope, 'Boolean('+intToStr(TempBoolCount+1)+')', svClear);
  TempBools[TempBoolCount].BoolVar := idx;
  TempBools[TempBoolCount].Used := true;
  inc(TempBoolCount);

  result := BoolVars[idx].Switch;
end;

procedure ReleaseTempBool(ASwitch: integer);
var
  i: Integer;
begin
  for i := 0 to TempBoolCount-1 do
    if BoolVars[TempBools[i].BoolVar].Switch = ASwitch then
    begin
      TempBools[i].Used := false;
      exit;
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
    AccArray := IntArrayIndexOf(GetArithmeticScope, 'Accumulator', false);
    if AccArray = -1 then
      AccArray := CreateIntArray(GetArithmeticScope, 'Accumulator', MaxTriggerPlayers, [], 32);
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

function GetTransferParam(APlayer: TPlayer; AUnitType: TStarcraftUnit; ASysIP: integer): integer;
var
  i, shift: Integer;
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
        shift := ASysIP - AddAccSysIP[0];
        ExtraShiftBits := max(shift, ExtraShiftBits);
        if (shift and 1) = 1 then ShiftUsed1 := true;
        if (shift and 2) = 2 then ShiftUsed2 := true;
        if (shift and 4) = 4 then ShiftUsed4 := true;
        if (shift and 8) = 8 then ShiftUsed8 := true;
        if (shift and 16) = 16 then ShiftUsed16 := true;
      end;
      if ASysIP = SubtractAccSysIP then
        TransferProcs[i].SubAcc:= true;

      exit(i);
    end;
  raise exception.Create('Unable to find variable');
end;

procedure WriteArithmeticTriggers;
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
  addAcc, setValue: TInstruction;
  condValue: TCondition;
  condVar, condIP2: TCondition;
  setSw: TSetSwitchInstruction;
  switchCopyVarToBits, switchAddToVarFromBits,
  switchCopyAccToBits, switchNegateBits2,
  switchAddToAccFromBits, switchSubtractIntoAccFromBits,
  switchClearSysIP, switchShift16, switchShift8, switchShift4,
  switchShift2, switchShift1, switchShiftOverflow: integer;
  hasAddFromBits, hasSubFromBits, hasAddAcc, hasSubAcc, hasShift: boolean;
  TotalBits,ArithmeticMaxAccBits: integer;

  ValueBools: array of integer;

  procedure WriteShiftProg(AShift: integer; ASwitchSub: integer);
  var j: integer;
    setSw2: TSetSwitchInstruction;
  begin
    condSub := TSwitchCondition.Create(ASwitchSub,true);
    condSwitch := TSwitchCondition.Create(-1, true);
    proc.Add(TSetSwitchInstruction.Create(switchShiftOverflow, svSet));
    for j := TotalBits-1 downto TotalBits-AShift do
    if (j >= 0) and (j < ArithmeticMaxAccBits) then
    begin
      condSwitch.Switch := ValueBools[j];
      WriteProg([plAllPlayers], [condSub,condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;

    setSw := TSetSwitchInstruction.Create(-1, svSet);
    proc.Add(setSw);
    setSw2 := TSetSwitchInstruction.Create(-1, svClear);
    proc.Add(setSw2);
    for j := TotalBits-AShift-1 downto 0 do
    if j < ArithmeticMaxAccBits then
    begin
      condSwitch.Value := true;
      condSwitch.Switch := ValueBools[j];
      setSw.Switch:= ValueBools[j+AShift];
      setSw2.Switch:= ValueBools[j];
      WriteProg([plAllPlayers], [condSub,condSwitch], proc, -1,-1, True);
    end;
    EmptyProc;

    for j := min(AShift-1, ArithmeticMaxAccBits-1) downto 0 do
      proc.Add(TSetSwitchInstruction.Create(ValueBools[j], svClear));
    WriteProg([plAllPlayers], [condSub], proc, -1,-1,true);
    EmptyProc;

    condSwitch.Free;
    condSub.Free;
  end;

begin
  if AddIntoAccSysIP = -1 then exit;

  TotalBits := min(ArithmeticMaxInputBits+ExtraShiftBits,24);
  ArithmeticMaxAccBits := min(ArithmeticMaxInputBits+4,TotalBits);
  setlength(ValueBools, TotalBits);
  for i := 0 to TotalBits-1 do
    ValueBools[i] := AllocateTempBool;

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

  switchCopyVarToBits := AllocateTempBool;
  switchAddToVarFromBits := AllocateTempBool;
  if hasAddAcc or hasSubAcc then switchCopyAccToBits:= AllocateTempBool
                            else switchCopyAccToBits := -1;
  if hasSubAcc then switchNegateBits2 := AllocateTempBool else switchNegateBits2 := -1;
  if hasAddFromBits then switchAddToAccFromBits := AllocateTempBool
                    else switchAddToAccFromBits := -1;
  if hasSubFromBits then switchSubtractIntoAccFromBits := AllocateTempBool
                    else switchSubtractIntoAccFromBits := -1;
  switchClearSysIP := AllocateTempBool;
  if ShiftUsed16 then switchShift16 := AllocateTempBool else switchShift16 := -1;
  if ShiftUsed8 then switchShift8 := AllocateTempBool else switchShift8 := -1;
  if ShiftUsed4 then switchShift4 := AllocateTempBool else switchShift4 := -1;
  if ShiftUsed2 then switchShift2 := AllocateTempBool else switchShift2 := -1;
  if ShiftUsed1 then switchShift1 := AllocateTempBool else switchShift1 := -1;
  if hasShift then switchShiftOverflow := AllocateTempBool else switchShiftOverflow := -1;

  proc := TInstructionList.Create;

  proc.Add(TCommentInstruction.Create('Arithmetic'));
  //clear temp bits
  for i := 0 to TotalBits-1 do
    proc.Add(TSetSwitchInstruction.Create(ValueBools[i], svClear));
  if switchCopyVarToBits <> -1 then    proc.Add(TSetSwitchInstruction.Create(switchCopyVarToBits, svClear));
  if switchAddToVarFromBits <> -1 then proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svClear));
  if switchCopyAccToBits <> -1 then    proc.Add(TSetSwitchInstruction.Create(switchCopyAccToBits, svClear));
  if switchNegateBits2 <> -1 then      proc.Add(TSetSwitchInstruction.Create(switchNegateBits2, svClear));
  if switchAddToAccFromBits <> -1 then proc.Add(TSetSwitchInstruction.Create(switchAddToAccFromBits, svClear));
  if switchSubtractIntoAccFromBits<>-1 then proc.Add(TSetSwitchInstruction.Create(switchSubtractIntoAccFromBits, svClear));
  if switchClearSysIP <> -1 then       proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svClear));
  if switchShift16 <> -1 then          proc.Add(TSetSwitchInstruction.Create(switchShift16, svClear));
  if switchShift8 <> -1 then           proc.Add(TSetSwitchInstruction.Create(switchShift8, svClear));
  if switchShift4 <> -1 then           proc.Add(TSetSwitchInstruction.Create(switchShift4, svClear));
  if switchShift2 <> -1 then           proc.Add(TSetSwitchInstruction.Create(switchShift2, svClear));
  if switchShift1 <> -1 then           proc.Add(TSetSwitchInstruction.Create(switchShift1, svClear));
  if switchShiftOverflow <> -1 then    proc.Add(TSetSwitchInstruction.Create(switchShiftOverflow, svClear));

  WriteProg([plAllPlayers], [], proc, -1, -1, True);
  EmptyProc;

  if hasAddFromBits then
  begin
    //adding into accumulator: copying to bits and adding to accumualtor
    condIP := CheckSysIP(AddIntoAccSysIP);
    proc.Add(TSetSwitchInstruction.Create(switchCopyVarToBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToVarFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchAddToAccFromBits, svSet));
    proc.Add(TSetSwitchInstruction.Create(switchClearSysIP, svSet));
    WriteProg([plAllPlayers], [condIP], proc, -1, -1, True);
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
    WriteProg([plAllPlayers], [condIP], proc, -1, -1, True);
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
    for j := min(TransferProcs[i].BitCount-1, ArithmeticMaxInputBits-1) downto 0 do
    begin
      condValue := CreateIntegerCondition(IntVars[i].Player, IntVars[i].UnitType, icmAtLeast, 1 shl j);

      addAcc := CreateSetIntegerInstruction(IntVars[i].Player, IntVars[i].UnitType, simSubtract, 1 shl j);
      setSw := TSetSwitchInstruction.Create(ValueBools[j], svSet);
      proc.Add(addAcc);
      proc.Add(setSw);
      WriteProg([plAllPlayers], [condSub, condVar, condValue], proc, -1,-1, True);
      EmptyProc;

      condValue.Free;
    end;
    condVar.Free;
  end;
  condSub.Free;

  if hasAddFromBits then
  begin
    //adding temp bits to accumulator
    condSub := TSwitchCondition.Create(switchAddToAccFromBits,true);
    condSwitch := TSwitchCondition.Create(0,true);
    for i := ArithmeticMaxInputBits-1 downto 0 do
    begin
      condSwitch.Switch := ValueBools[i];

      addAcc := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, 1 shl i);
      proc.Add(addAcc);
      WriteProg([plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
      EmptyProc;
    end;
    condSwitch.Free;
    condSub.Free;
  end;

  if hasSubFromBits then
  begin
    //subtracting temp bits from accumulator
    condSub := TSwitchCondition.Create(switchSubtractIntoAccFromBits,true);
    condSwitch := TSwitchCondition.Create(0,true);
    for i := ArithmeticMaxInputBits-1 downto 0 do
    begin
      condSwitch.Switch := ValueBools[i];
      addAcc := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, 1 shl i);
      proc.Add(addAcc);
      WriteProg([plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
      EmptyProc;
    end;
    condSwitch.Free;
    condSub.Free;
  end;

  if hasShift then
  begin
    if ShiftUsed16 then
    begin
      CheckSysIPRange(AddAccSysIP[16],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(16,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift16, svSet));
      WriteProg([plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ShiftUsed8 then
    begin
      CheckSysIPRange(AddAccSysIP[8],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(8,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift8, svSet));
      WriteProg([plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ShiftUsed4 then
    begin
      CheckSysIPRange(AddAccSysIP[4],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(4,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift4, svSet));
      WriteProg([plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ShiftUsed2 then
    begin
      CheckSysIPRange(AddAccSysIP[2],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(2,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift2, svSet));
      WriteProg([plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;

    if ShiftUsed1 then
    begin
      CheckSysIPRange(AddAccSysIP[1],AddAccSysIP[23],condIP,condIP2);
      proc.Add(SetNextSysIP(1,simSubtract));
      proc.Add(TSetSwitchInstruction.Create(switchShift1, svSet));
      WriteProg([plAllPlayers], [condIP,condIP2], proc, -1,-1, True);
      EmptyProc; condIP.Free; condIP2.Free;
    end;
  end;

  //checking initial overflow
  condIP := CheckSysIP(AddAccSysIP[0]);
  for i := 0 to high(TransferProcs) do
  if TransferProcs[i].AddAcc then
  begin
    condVar := CheckSysParam(i);
    condValue := CreateIntegerCondition(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, 1 shl TransferProcs[i].BitCount);
    proc.Add(CreateSetIntegerInstruction(IntVars[i].Player, IntVars[i].UnitType, simSetTo, (1 shl TransferProcs[i].BitCount)-1));
    proc.Add(SetNextSysIP(0));
    WriteProg([plAllPlayers], [condIP, condVar, condValue], proc, -1,-1, True);
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
    WriteProg([plAllPlayers], [condIP], proc, -1, -1, True);
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
    WriteProg([plAllPlayers], [condIP], proc, -1, -1, True);
    EmptyProc;
    condIP.Free;
  end;

  if hasAddAcc or hasSubAcc then
  begin
    //copy acc to bits
    condSub := TSwitchCondition.Create(switchCopyAccToBits,true);
    for j := ArithmeticMaxAccBits-1 downto 0 do
    begin
      condValue := CreateIntegerCondition(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, 1 shl j);

      addAcc := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, 1 shl j);
      setSw := TSetSwitchInstruction.Create(ValueBools[j], svSet);
      proc.Add(addAcc);
      proc.Add(setSw);
      WriteProg([plAllPlayers], [condSub, condValue], proc, -1,-1, True);
      EmptyProc;

      condValue.Free;
    end;

    // restore accumulator for successive additions
    condSwitch := TSwitchCondition.Create(0,true);
    for j := ArithmeticMaxAccBits-1 downto 0 do
    begin
      condSwitch.Switch := ValueBools[j];
      addAcc := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, 1 shl j);
      proc.Add(addAcc);
      WriteProg([plAllPlayers], [condSub, condSwitch], proc, -1,-1, True);
      EmptyProc;
    end;
    condSwitch.Free;

    condSub.Free;
  end;

  if hasShift then
  begin
    if ShiftUsed16 then WriteShiftProg(16, switchShift16);
    if ShiftUsed8 then WriteShiftProg(8, switchShift8);
    if ShiftUsed4 then WriteShiftProg(4, switchShift4);
    if ShiftUsed2 then WriteShiftProg(2, switchShift2);
    if ShiftUsed1 then WriteShiftProg(1, switchShift1);

    condSub := TSwitchCondition.Create(switchShiftOverflow,true);
    for i := 0 to TotalBits-1 do
      proc.Add(TSetSwitchInstruction.Create(ValueBools[i], svSet));
    WriteProg([plAllPlayers], [condSub], proc, -1,-1, True);
    EmptyProc;
    condSub.Free;
  end;

  if hasSubAcc then
  begin
    //negating bits
    condSub := TSwitchCondition.Create(switchNegateBits2,true);
    for i := 0 to TotalBits-1 do
      proc.Add(TSetSwitchInstruction.Create(ValueBools[i], svToggle));
    WriteProg([plAllPlayers], [condSub], proc, -1,-1, True);
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
    for j := min(TransferProcs[i].BitCount-1, TotalBits-1) downto 0 do
    begin
      condSwitch.Switch := ValueBools[j];
      addAcc := CreateSetIntegerInstruction(IntVars[i].Player, IntVars[i].UnitType, simAdd, 1 shl j);
      proc.Add(addAcc);
      WriteProg([plAllPlayers], [condSub, condVar, condSwitch], proc, -1,-1, True);
      EmptyProc;
    end;
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
    condValue := CreateIntegerCondition(IntVars[i].Player, IntVars[i].UnitType, icmAtLeast, 1 shl TransferProcs[i].BitCount);
    setValue := CreateSetIntegerInstruction(IntVars[i].Player, IntVars[i].UnitType, simSetTo, (1 shl TransferProcs[i].BitCount)-1);
    proc.Add(setValue);
    WriteProg([plAllPlayers], [condIP, condVar, condValue], proc, -1,-1, True);
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
    if TransferProcs[i].SubAcc then
    begin
      condVar := CheckSysParam(i);
      proc.Add( CreateSetIntegerInstruction(IntVars[i].Player, IntVars[i].UnitType, simSubtract, (1 shl TotalBits )-1) );
      WriteProg([plAllPlayers], [condSub, condVar], proc, -1,-1, True);
      EmptyProc;
      condVar.Free;
    end;
    condSub.Free;
  end;

  condSub := TSwitchCondition.Create(switchClearSysIP,true);
  proc.Add(SetNextSysIP(0));
  WriteProg([plAllPlayers], [condSub], proc, -1,-1, True);
  EmptyProc;
  condSub.Free;


  proc.Free;

  ReleaseTempBool(switchCopyVarToBits);
  ReleaseTempBool(switchAddToVarFromBits);
  ReleaseTempBool(switchCopyAccToBits);
  ReleaseTempBool(switchNegateBits2);
  ReleaseTempBool(switchAddToAccFromBits);
  ReleaseTempBool(switchSubtractIntoAccFromBits);
  ReleaseTempBool(switchClearSysIP);
  ReleaseTempBool(switchShift16);
  ReleaseTempBool(switchShift8);
  ReleaseTempBool(switchShift4);
  ReleaseTempBool(switchShift2);
  ReleaseTempBool(switchShift1);
  ReleaseTempBool(switchShiftOverflow);

end;

procedure ExpandIntegerTransfer(ATransfer: TTransferIntegerInstruction; AExpanded: TInstructionList);
var
  nextIP: Integer;
begin
  if ATransfer.UnitType = suConst then
  begin
    NeedAcc;
    case ATransfer.Action of
    itCopyIntoAccumulator: AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, ATransfer.Value));
    itAddIntoAccumulator: AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simAdd, ATransfer.Value));
    itSubtractIntoAccumulator: AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simSubtract, ATransfer.Value));
    itRandomizeAccumulator: AExpanded.Add(TRandomizeIntegerInstruction.Create(plCurrentPlayer, IntArrays[AccArray].UnitType, ATransfer.Value));
    itLimitAccumulator: AExpanded.Add(TFastIfInstruction.Create([CreateIntegerCondition(plCurrentPlayer, IntArrays[AccArray].UnitType, icmAtLeast, ATransfer.Value+1)],
                                         [CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, ATransfer.Value)]) );
    else
      raise exception.Create('Unhandled case');
    end;
  end else
  begin
    NeedTransfer;
    case ATransfer.Action of
    itCopyIntoAccumulator: AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[AccArray].UnitType, simSetTo, 0));
    itCopyAccumulator: AExpanded.Add(CreateSetIntegerInstruction(ATransfer.Player, ATransfer.UnitType, simSetTo, 0));
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
  result := CreateIntegerCondition(plCurrentPlayer, IntArrays[AccArray].UnitType, AMode, AValue);
end;

function IsAccumulatorVariable(APlayer: TPlayer; AUnit: TStarcraftUnit): boolean;
begin
  if AccArray <> -1 then
    result := IntArrays[AccArray].UnitType = AUnit
  else
    result := false;
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
  ShiftUsed1 := false;
  ShiftUsed2 := false;
  ShiftUsed4 := false;
  ShiftUsed8 := false;
  ShiftUsed16 := false;
  ArithmeticScopeValue:= -1;
end;


end.

