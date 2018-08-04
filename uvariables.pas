unit uvariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes;

const
  MaxArraySize = MaxTriggerPlayers;

var
  IntArrays: array of record
    Predefined, Constant: boolean;
    Name: string;
    Size: integer;
    UnitType: string;
    Values: array[1..MaxArraySize] of integer;
  end;
  IntArrayCount: integer;
  CurIntArrayUnitNameIndex: integer;

function CreateIntArray(AName: string; ASize: integer; AValues: array of integer; AConstant: boolean = false): integer;
function PredefineIntArray(AName: string; AUnitType: string): integer;
function IntArrayIndexOf(AName: string): integer;

var
  IntVars: array of record
    Predefined, Constant: boolean;
    Name: string;
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Randomize: boolean;
    AddToAcc,AddFromAcc: boolean;
  end;
  IntVarCount: integer;
  CurIntVarPlayer: TPlayer;
  CurIntVarUnitNameIndex: integer;

function CreateIntVar(AName: string; AValue: integer; ARandomize: boolean = false; AConstant: boolean = false): integer;
function PredefineIntVar(AName: string; APlayer: TPlayer; AUnitType: string): integer;
function IntVarIndexOf(AName: string): integer;

var
  BoolVars: array of record
    Constant: boolean;
    Name: string;
    Switch: integer;
    Value: TSwitchValue;
  end;
  BoolVarCount: integer;
  CurBoolVarSwitch: integer;

function CreateBoolVar(AName: string; AValue: TSwitchValue; AConstant: boolean = false): integer;
function BoolVarIndexOf(AName: string): integer;

var
  UnitPropVars: array of record
    Name: string;
    Value: TUnitProperties;
  end;
  UnitPropCount: integer;

function CreateUnitProp(AName: string; AValue: TUnitProperties; AConstant: boolean): integer;
function UnitPropIndexOf(AName: string): integer;
function FindOrCreateUnitProperty(AProp: TUnitProperties): integer;

var
  StringVars: array of record
    Name: string;
    Value: string;
  end;
  StringCount: integer;

function CreateString(AName: string; AValue: string; AConstant: boolean): integer;
function StringIndexOf(AName: string): integer;

implementation

uses uparsevb;

function CreateIntArray(AName: string; ASize: integer;
  AValues: array of integer; AConstant: boolean): integer;
var
  i: Integer;
begin
  CheckReservedWord(AName);

  if length(AValues)>ASize then
    raise exception.Create('Too many elements in array values');

  if IntArrayCount = 0 then
    CurIntArrayUnitNameIndex:= high(NonKillableUnits)+1;

  if not AConstant then
  begin
    if (CurIntArrayUnitNameIndex = 1) or
      ((IntVarCount > 0) and (CurIntArrayUnitNameIndex = CurIntVarUnitNameIndex+1)) then
      raise exception.Create('Too many integer variables');

    dec(CurIntArrayUnitNameIndex);
  end;

  if IntArrayCount >= length(IntArrays) then
    setlength(IntArrays, IntArrayCount*2+4);

  result := IntArrayCount;
  inc(IntArrayCount);

  with IntArrays[result] do
  begin
    Constant := AConstant;
    Predefined := false;
    Name := AName;
    Size:= ASize;
    if AConstant then UnitType := 'Const'
    else UnitType := NonKillableUnits[CurIntArrayUnitNameIndex];

    for i := 0 to high(AValues) do
      Values[i+1] := AValues[i];
    for i := high(AValues)+1 to MaxArraySize-1 do
      Values[i+1] := 0;

    for i := 1 to Size do
      PredefineIntVar(Name+'('+IntToStr(i)+')', IntToPlayer(i), UnitType);
    PredefineIntVar(Name+'(Me)', plCurrentPlayer, UnitType);
  end;
end;

function PredefineIntArray(AName: string; AUnitType: string): integer;
var
  i: Integer;
begin
  if IntArrayCount = 0 then
    CurIntArrayUnitNameIndex:= high(NonKillableUnits)+1;

  if IntArrayCount >= length(IntArrays) then
    setlength(IntArrays, IntArrayCount*2+4);

  result := IntArrayCount;
  inc(IntArrayCount);

  with IntArrays[result] do
  begin
    Constant := false;
    Predefined := true;
    Name := AName;
    Size:= MaxArraySize;
    UnitType := AUnitType;
    for i := 1 to MaxArraySize do
    begin
      Values[i] := 0;
      PredefineIntVar(Name+'('+IntToStr(i)+')', IntToPlayer(i), UnitType);
    end;
    PredefineIntVar(Name+'(Me)', plCurrentPlayer, UnitType);
  end;
end;

function IntArrayIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to IntArrayCount-1 do
    if CompareText(IntArrays[i].Name, AName)=0 then exit(i);
  exit(-1);
end;

function CreateIntVar(AName: string; AValue: integer; ARandomize: boolean;
  AConstant: boolean): integer;
begin
  CheckReservedWord(AName);

  if AConstant and ARandomize then
    raise exception.Create('A constant cannot be random');

  if IntVarCount = 0 then
  begin
    CurIntVarPlayer:= succ(IntToPlayer(MaxArraySize));
    CurIntVarUnitNameIndex:= 1;
  end;

  if not AConstant then
  begin
    if CurIntVarPlayer = plPlayer1 then
    begin
      If (CurIntVarUnitNameIndex = high(NonKillableUnits)) or
        ((IntArrayCount > 0) and (CurIntVarUnitNameIndex = CurIntArrayUnitNameIndex-1)) then
        raise exception.Create('Too many integer variables');

      CurIntVarPlayer:= IntToPlayer(MaxArraySize);
      Inc(CurIntVarUnitNameIndex);
    end else
      CurIntVarPlayer:= Pred(CurIntVarPlayer);
  end;

  if IntVarCount >= length(IntVars) then
    setlength(IntVars, IntVarCount*2+4);

  result := IntVarCount;
  inc(IntVarCount);

  with IntVars[result] do
  begin
    Constant := AConstant;
    Predefined := false;
    Name := AName;
    if AConstant then
    begin
      Player := plNone;
      UnitType := 'Const';
    end else
    begin
      Player := CurIntVarPlayer;
      UnitType := NonKillableUnits[CurIntVarUnitNameIndex];
    end;
    Value := AValue;
    Randomize:= ARandomize;
  end;
end;

function PredefineIntVar(AName: string; APlayer: TPlayer; AUnitType: string): integer;
begin
  if IntVarCount = 0 then
  begin
    CurIntVarPlayer:= succ(IntToPlayer(MaxArraySize));
    CurIntVarUnitNameIndex:= 1;
  end;

  if IntVarCount >= length(IntVars) then
    setlength(IntVars, IntVarCount*2+4);

  result := IntVarCount;
  inc(IntVarCount);

  with IntVars[result] do
  begin
    Predefined := true;
    Name := AName;
    Player := APlayer;
    UnitType := AUnitType;
    Value := 0;
    Randomize:= false;
  end;
end;

function IntVarIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to IntVarCount-1 do
    if CompareText(IntVars[i].Name, AName)=0 then exit(i);
  exit(-1);
end;

function CreateBoolVar(AName: string; AValue: TSwitchValue; AConstant: boolean
  ): integer;
begin
  CheckReservedWord(AName);

  if AConstant and (AValue = svRandomize) then
    raise exception.Create('A constant cannot be random');

  if BoolVarCount = 0 then
  begin
    CurBoolVarSwitch:= 256;
  end;

  if not AConstant then
  begin
    if CurBoolVarSwitch = 0 then
      raise Exception.Create('Too many boolean variables');
  end;

  if BoolVarCount >= length(BoolVars) then
    setlength(BoolVars, BoolVarCount*2+4);

  result := BoolVarCount;
  inc(BoolVarCount);

  with BoolVars[result] do
  begin
    Constant := AConstant;
    Name := AName;
    if AConstant then Switch := 0 else Switch:= CurBoolVarSwitch;
    Value := AValue;
  end;

  Dec(CurBoolVarSwitch);
end;

function BoolVarIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to BoolVarCount-1 do
    if CompareText(BoolVars[i].Name, AName)=0 then exit(i);
  exit(-1);
end;

function CreateUnitProp(AName: string; AValue: TUnitProperties; AConstant: boolean): integer;
begin
  CheckReservedWord(AName);

  if not AConstant then
    raise Exception.Create('Unit properties must be constant');

  if UnitPropCount >= length(UnitPropVars) then
    setlength(UnitPropVars, UnitPropCount*2+4);
  result := UnitPropCount;
  inc(UnitPropCount);

  with UnitPropVars[result] do
  begin
    Name := AName;
    Value := AValue;
  end;
end;

function UnitPropIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to UnitPropCount-1 do
    if CompareText(AName, UnitPropVars[i].Name) = 0 then exit(i);
  exit(-1);
end;

function FindOrCreateUnitProperty(AProp: TUnitProperties): integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to UnitPropCount-1 do
  begin
    if CompareMem(@UnitPropVars[i].Value, @AProp, sizeof(TUnitProperties)) then
    begin
      result := i;
      break;
    end;
  end;
  if result = -1 then
    result := CreateUnitProp('_prop'+inttostr(UnitPropCount+1), AProp, True);
end;

function CreateString(AName: string; AValue: string; AConstant: boolean): integer;
begin
  CheckReservedWord(AName);

  if not AConstant then
    raise Exception.Create('Strings must be constant');

  if StringCount >= length(StringVars) then
    setlength(StringVars, StringCount*2+4);
  result := StringCount;
  inc(StringCount);

  with StringVars[result] do
  begin
    Name := AName;
    Value := AValue;
  end;
end;

function StringIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to StringCount-1 do
    if CompareText(StringVars[i].Name,AName)=0 then exit(i);
  exit(-1);
end;

end.

