unit uvariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes;

const
  MaxBoolArraySize = MaxSwitches;
  MaxIntArraySize = MaxBoolArraySize;

procedure InitVariables;

var
  IntArrays: array of record
    Predefined, Constant: boolean;
    Name: string;
    Size: integer;
    UnitType: string;
    Values: array of integer;
    Vars: array of integer;
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
function AllocateTempInt: integer;
procedure ReleaseTempInt(ATempInt: integer);

var
  BoolArrays: array of record
    Constant, ReadOnly: boolean;
    Name: string;
    Size: integer;
    Values: array of TSwitchValue;
    Vars: array of integer;
  end;
  BoolArrayCount: integer;

function CreateBoolArray(AName: string; ASize: integer; AValues: array of TSwitchValue; AConstant: boolean = false): integer;
function PredefineBoolArray(AName: string; ASize: integer; AVars: array of integer): integer;
function BoolArrayIndexOf(AName: string): integer;

var
  BoolVars: array of record
    Constant, ReadOnly: boolean;
    Name: string;
    Switch: integer;
    Value: TSwitchValue;
    BoolArray: integer;
  end;
  BoolVarCount: integer;
  CurBoolVarSwitch: integer;

function CreateBoolVar(AName: string; AValue: TSwitchValue; AConstant: boolean = false): integer;
function BoolVarIndexOf(AName: string): integer;
function GetBoolResultVar: integer;

var
  PlayerPresenceVar: array[1..MaxTriggerPlayers] of integer;
  PlayerPresenceDefinedVar: integer;

function GetPlayerPresenceBoolVar(APlayer: TPlayer): integer;
function IsPlayerPresenceUsed(APlayer: TPlayer): boolean;
function GetPlayerPresenceDefinedVar: integer;
function GetPlayerPresentArray: integer;

var
  StopEventBoolVar: integer;

function GetStopEventBoolVar: integer;

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
  SoundVars: array of record
    Name: string;
    Filename: string;
    DurationMs: integer;
  end;
  SoundCount: integer;

function CreateSound(AName: string; AFilename: string; ADurationMs: integer; AConstant: boolean): integer;
function SoundIndexOf(AName: string): integer;

{var
  StringArrays: array of record
    Name: string;
    Values: array of string;
  end;
}
var
  StringVars: array of record
    Name: string;
    Value: string;
  end;
  StringCount: integer;

function CreateString(AName: string; AValue: string; AConstant: boolean): integer;
function StringIndexOf(AName: string): integer;

var
  Messages: array of record
    Text: string;
    Players: TPlayers;
  end;
  MessageCount: integer;

function FindOrCreateMessage(AText: string; APlayers: TPlayers): integer;

implementation

uses uparsevb;

var BoolResultVar: integer;

procedure InitVariables;
var
  i: Integer;
begin
  BoolVarCount:= 0;
  IntVarCount:= 0;
  IntArrayCount:= 0;
  StringCount := 0;
  UnitPropCount := 0;
  SoundCount := 0;
  BoolResultVar := -1;
  for i := low(PlayerPresenceVar) to high(PlayerPresenceVar) do
    PlayerPresenceVar[i] := -1;
  PlayerPresenceDefinedVar := -1;
  StopEventBoolVar := -1;
  MessageCount := 0;
end;

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

    setlength(Values, Size);
    for i := 0 to high(AValues) do
      Values[i] := AValues[i];
    for i := high(AValues)+1 to high(Values) do
      Values[i] := 0;

    setlength(Vars, Size);
    for i := 1 to Size do
      if (i <= MaxTriggerPlayers) or Constant then
      begin
        if i <= MaxTriggerPlayers then
          Vars[i-1] := PredefineIntVar(Name+'('+IntToStr(i)+')', IntToPlayer(i), UnitType)
        else
          Vars[i-1] := PredefineIntVar(Name+'('+IntToStr(i)+')', plNone, UnitType);
        IntVars[Vars[i-1]].Value := Values[i-1];
        IntVars[Vars[i-1]].Constant:= Constant;
      end
      else
      begin
        Vars[i-1] := CreateIntVar(Name+'('+IntToStr(i)+')', Values[i-1], false, Constant);
        IntVars[Vars[i-1]].Predefined := true;
      end;

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
    Size:= MaxTriggerPlayers;
    UnitType := AUnitType;
    setlength(Values, Size);
    setlength(Vars, Size);
    for i := 1 to MaxTriggerPlayers do
    begin
      Values[i-1] := 0;
      Vars[i-1] := PredefineIntVar(Name+'('+IntToStr(i)+')', IntToPlayer(i), UnitType);
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
    CurIntVarPlayer:= succ(IntToPlayer(MaxTriggerPlayers));
    CurIntVarUnitNameIndex:= 1;
  end;

  if not AConstant then
  begin
    if CurIntVarPlayer = plPlayer1 then
    begin
      If (CurIntVarUnitNameIndex = high(NonKillableUnits)) or
        ((IntArrayCount > 0) and (CurIntVarUnitNameIndex = CurIntArrayUnitNameIndex-1)) then
        raise exception.Create('Too many integer variables');

      CurIntVarPlayer:= IntToPlayer(MaxTriggerPlayers);
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
    CurIntVarPlayer:= succ(IntToPlayer(MaxTriggerPlayers));
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

var
  TempInts: array of record
    IntVar: integer;
  end;
  TempIntCount: integer;

function AllocateTempInt: integer;
begin
  if TempIntCount >= length(tempInts) then
    setlength(TempInts, TempIntCount*2+4);
  TempInts[TempIntCount].IntVar:= CreateIntVar('_tempInt',0);
  result := TempInts[TempIntCount].IntVar;
  inc(TempIntCount);
end;

procedure ReleaseTempInt(ATempInt: integer);
var
  i, j: Integer;
begin
  for i := 0 to high(tempInts) do
    if TempInts[i].IntVar = ATempInt then
    begin
      for j := i to high(tempInts)-1 do
        tempInts[j] := tempInts[j+1];
      dec(TempIntCount);
      exit;
    end;
  raise exception.Create('Integer variable not found');
end;

function CreateBoolArray(AName: string; ASize: integer;
  AValues: array of TSwitchValue; AConstant: boolean): integer;
var
  i: Integer;
begin
  CheckReservedWord(AName);

  if length(AValues)>ASize then
    raise exception.Create('Too many elements in value array');

  if BoolArrayCount >= length(BoolArrays) then
    setlength(BoolArrays, BoolArrayCount*2+4);

  result := BoolArrayCount;
  inc(BoolArrayCount);

  with BoolArrays[result] do
  begin
    Constant := AConstant;
    ReadOnly := AConstant;
    Name := AName;
    Size:= ASize;

    setlength(Values, Size);
    for i := 0 to high(AValues) do
      Values[i] := AValues[i];
    for i := high(AValues)+1 to high(Values) do
      Values[i] := svClear;

    SetLength(Vars, Size);
    for i := 1 to Size do
      if Constant then
        Vars[i-1] := -1
      else
      begin
        Vars[i-1] := CreateBoolVar(Name+'('+inttostr(i)+')', Values[i-1], Constant);
        BoolVars[Vars[i-1]].BoolArray := result;
      end;
  end;
end;

function PredefineBoolArray(AName: string; ASize: integer;
  AVars: array of integer): integer;
var
  i: Integer;
begin
  CheckReservedWord(AName);

  if length(AVars)<>ASize then
    raise exception.Create('Variable array do not match array size');

  if BoolArrayCount >= length(BoolArrays) then
    setlength(BoolArrays, BoolArrayCount*2+4);

  result := BoolArrayCount;
  inc(BoolArrayCount);

  with BoolArrays[result] do
  begin
    Constant := False;
    Name := AName;
    Size:= ASize;

    setlength(Values, Size);
    SetLength(Vars, Size);
    for i := 0 to Size-1 do
    begin
      Vars[i] := AVars[i];
      Values[i] := BoolVars[Vars[i]].Value;
      BoolVars[Vars[i]].BoolArray:= result;
    end;
  end;
end;

function BoolArrayIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to BoolArrayCount-1 do
    if CompareText(BoolArrays[i].Name, AName)=0 then exit(i);
  exit(-1);
end;

function CreateBoolVar(AName: string; AValue: TSwitchValue; AConstant: boolean = false): integer;
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
    ReadOnly := AConstant;
    Name := AName;
    if AConstant then Switch := 0 else Switch:= CurBoolVarSwitch;
    Value := AValue;
    BoolArray := -1;
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

function GetBoolResultVar: integer;
begin
  if BoolResultVar = -1 then
    BoolResultVar := CreateBoolVar('_boolResult', svClear);
  result := BoolResultVar;
end;

function GetPlayerPresenceBoolVar(APlayer: TPlayer): integer;
var
  num: Integer;
begin
  num := ord(APlayer)-ord(plPlayer1)+1;
  if (num < low(PlayerPresenceVar)) or (num > high(PlayerPresenceVar)) then
    raise exception.Create('Player presence can be checked only for specific players');

  if PlayerPresenceVar[num] = -1 then
  begin
    PlayerPresenceVar[num] := BoolVarIndexOf('_presence'+Inttostr(num));
    if PlayerPresenceVar[num] = -1 then
      PlayerPresenceVar[num] := CreateBoolVar('_presence'+Inttostr(num), svClear);
  end;
  result := PlayerPresenceVar[num];
end;

function IsPlayerPresenceUsed(APlayer: TPlayer): boolean;
var
  num: Integer;
begin
  num := ord(APlayer)-ord(plPlayer1)+1;
  if (num < low(PlayerPresenceVar)) or (num > high(PlayerPresenceVar)) then
    raise exception.Create('Player presence can be checked only for specific players');

  result := (PlayerPresenceVar[num] <> -1);
end;

function GetPlayerPresenceDefinedVar: integer;
begin
  If PlayerPresenceDefinedVar = -1 then
  begin
    PlayerPresenceDefinedVar := BoolVarIndexOf('_presenceDefined');
    if PlayerPresenceDefinedVar = -1 then
      PlayerPresenceDefinedVar := CreateBoolVar('_presenceDefined', svClear);
  end;
  result := PlayerPresenceDefinedVar;
end;

function GetPlayerPresentArray: integer;
var bools: array of integer;
  i: Integer;
begin
  result := BoolArrayIndexOf('Present');
  if result = -1 then
  begin
    setlength(bools, MaxTriggerPlayers);
    for i := 0 to high(bools) do
      bools[i] := GetPlayerPresenceBoolVar(TPlayer(ord(plPLayer1)+i));
    result := PredefineBoolArray('Present',MaxTriggerPlayers,bools);
  end;
end;

function GetStopEventBoolVar: integer;
begin
  if StopEventBoolVar = -1 then
  begin
    StopEventBoolVar := BoolVarIndexOf('_stopEvent');
    if StopEventBoolVar = -1 then
      StopEventBoolVar := CreateBoolVar('_stopEvent', svClear);
  end;
  result := StopEventBoolVar;
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

function CreateSound(AName: string; AFilename: string; ADurationMs: integer; AConstant: boolean): integer;
begin
  CheckReservedWord(AName);

  if not AConstant then
    raise Exception.Create('Sounds must be constant');

  if SoundCount >= length(SoundVars) then
    setlength(SoundVars, SoundCount*2+4);
  result := SoundCount;
  inc(SoundCount);

  with SoundVars[result] do
  begin
    Name := AName;
    Filename:= AFilename;
    DurationMs:= ADurationMs;
  end;
end;

function SoundIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to SoundCount-1 do
    if CompareText(SoundVars[i].Name,AName)=0 then exit(i);
  exit(-1);
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

function FindOrCreateMessage(AText: string; APlayers: TPlayers): integer;
var
  i: Integer;
begin
  for i := 0 to MessageCount-1 do
    if (Messages[i].Text = AText) and (Messages[i].Players = APlayers) then
      exit(i);

  if MessageCount >= length(Messages) then
    setlength(Messages, MessageCount*2+4);
  result := MessageCount;
  inc(MessageCount);
  with Messages[result] do
  begin
    Text:= AText;
    Players:= APlayers;
  end;
end;

end.

