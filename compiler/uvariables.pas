unit uvariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, uscope;

const
  MaxBoolArraySize = MaxSwitches;
  MaxIntArraySize = MaxBoolArraySize;
  MaxStringArraySize = MaxBoolArraySize;
  MaxUnitProperties = 64;

var
  HyperTriggersOption: boolean;

procedure InitVariables;

var
  IntArrays: array of record
    Predefined, Constant: boolean;
    Name: string;
    Size: integer;
    UnitType: TStarcraftUnit;
    Values: array of integer;
    Vars: array of integer;
    BitCount: integer;
    Scope: integer;
    Deleted: boolean;
  end;
  IntArrayCount: integer;
  CurIntArrayUnitNameIndex: integer;

function CreateIntArray(AScope: integer; AName: string; ASize: integer; AValues: array of integer; ABitCount: integer; AConstant: boolean = false; AMultithread: boolean = false): integer;
function PredefineIntArray(AScope: integer; AName: string; AUnitType: TStarcraftUnit; ABitCount: integer): integer;
function IntArrayIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;

var
  IntVars: array of record
    Scope: integer;
    Predefined, Constant: boolean;
    Name: string;
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    BitCount: integer;
    Value: integer;
    Randomize: boolean;
    AddToAcc,AddFromAcc: boolean;
    IntArray: integer;
  end;
  IntVarCount: integer;
  CurIntVarPlayer: TPlayer;
  CurIntVarUnitNameIndex: integer;

function CreateIntVar(AScope: integer; AName: string; AValue: integer; ABitCount: integer; ARandomize: boolean = false; AConstant: boolean = false): integer;
function CreateMultithreadIntVar(AThreads: TPlayers; AScope: integer; AName: string; ABitCount: integer): integer;
function PredefineIntVar(AScope: integer; AName: string; APlayer: TPlayer; AUnitType: TStarcraftUnit; ABitCount: integer): integer;
function IntVarIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;
function AllocateTempInt(ABitCount: integer): integer;
procedure ReleaseTempInt(ATempInt: integer);

var
  BoolArrays: array of record
    Constant, ReadOnly: boolean;
    Name: string;
    Size: integer;
    Values: array of TSwitchValue;
    Vars: array of integer;
    Scope: integer;
  end;
  BoolArrayCount: integer;

function CreateBoolArray(AScope: integer; AName: string; ASize: integer; AValues: array of TSwitchValue; AConstant: boolean = false): integer;
function PredefineBoolArray(AScope: integer; AName: string; ASize: integer; AVars: array of integer): integer;
function BoolArrayIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;

var
  BoolVars: array of record
    Constant, ReadOnly: boolean;
    Name: string;
    Switch: integer;
    Value: TSwitchValue;
    BoolArray: integer;
    Scope: integer;
  end;
  BoolVarCount: integer;
  CurBoolVarSwitch: integer;

function CreateBoolVar(AScope: integer; AName: string; AValue: TSwitchValue; AConstant: boolean = false): integer;
function BoolVarIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;

var
  PlayerPresenceVar: array[1..MaxTriggerPlayers] of integer;
  PlayerPresenceDefinedVar: integer;

function GetPlayerPresenceBoolVar(APlayer: TPlayer): integer;
function IsPlayerPresenceUsed(APlayer: TPlayer): boolean;
function GetPlayerPresenceDefinedVar: integer;
function GetPlayerPresentArray: integer;

var
  RunEventBoolVar: integer;

function GetRunEventBoolVar: integer;
function RunEventBoolVarUsed: boolean;

var
  UnitPropVars: array of record
    Name: string;
    Value: TUnitProperties;
    Scope: integer;
  end;
  UnitPropCount: integer;

function CreateUnitProp(AScope: integer; AName: string; AValue: TUnitProperties; AConstant: boolean): integer;
function UnitPropIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;
function FindOrCreateUnitProperty(AProp: TUnitProperties): integer;
procedure CompileUnitProperties;

var
  SoundVars: array of record
    Scope: integer;
    Name: string;
    Filename: string;
    DurationMs: integer;
  end;
  SoundCount: integer;

function CreateSound(AScope: integer; AName: string; AFilename: string; ADurationMs: integer; AConstant: boolean): integer;
function SoundIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;

var
  StringVars: array of record
    Name: string;
    Value: string;
    Scope: integer;
  end;
  StringCount: integer;

function CreateString(AScope: integer; AName: string; AValue: string; AConstant: boolean): integer;
function StringIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;

var
  StringArrays: array of record
    Name: string;
    Values: array of string;
    Scope: integer;
    Size: integer;
  end;
  StringArrayCount: integer;

function CreateStringArray(AScope: integer; AName: string; ASize: integer; AValues: array of string; AConstant: boolean = false): integer;
function StringArrayIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean = true): integer;

var
  Messages: array of record
    Text: string;
    Players: TPlayers;
  end;
  MessageCount: integer;

function FindOrCreateMessage(AText: string; APlayers: TPlayers): integer;

implementation

uses uparsevb, uunitpropchunk;

var
  TempInts8: array of record
    IntVar: integer;
    Used: boolean;
  end;
  TempInt8Count: integer;
  TempInts16: array of record
    IntVar: integer;
    Used: boolean;
  end;
  TempInt16Count: integer;
  TempInts24: array of record
    IntVar: integer;
    Used: boolean;
  end;
  TempInt24Count: integer;

procedure InitVariables;
var
  i: Integer;
begin
  BoolVarCount:= 0;
  BoolArrayCount := 0;
  IntVarCount:= 0;
  IntArrayCount:= 0;
  StringCount := 0;
  StringArrayCount := 0;
  UnitPropCount := 0;
  SoundCount := 0;
  for i := low(PlayerPresenceVar) to high(PlayerPresenceVar) do
    PlayerPresenceVar[i] := -1;
  PlayerPresenceDefinedVar := -1;
  RunEventBoolVar := -1;
  MessageCount := 0;
  TempInt8Count := 0;
  TempInt16Count := 0;
  TempInt24Count := 0;
  HyperTriggersOption := false;
end;

function CreateIntArray(AScope: integer; AName: string; ASize: integer;
  AValues: array of integer; ABitCount: integer; AConstant: boolean;
  AMultithread: boolean): integer;
var
  i, sharedIntVar: Integer;
begin
  CheckReservedWord(AName);

  if length(AValues)>ASize then
    raise exception.Create('Too many elements in array values');

  for i := 0 to high(AValues) do
    if (AValues[i]<0) or (AValues[i] >= 1 shl ABitCount) then
      raise exception.Create('Value out of bounds');

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
    Scope := AScope;
    Constant := AConstant;
    Predefined := false;
    Deleted := false;
    Name := AName;
    Size:= ASize;
    if AConstant then UnitType := suConst
    else UnitType := NonKillableUnits[CurIntArrayUnitNameIndex];
    BitCount:= ABitCount;

    setlength(Values, Size);
    for i := 0 to high(AValues) do
      Values[i] := AValues[i];
    for i := high(AValues)+1 to high(Values) do
      Values[i] := 0;

    setlength(Vars, Size);
    if AMultithread and not AConstant then
    begin
      Name := AName+'.Thread';
      sharedIntVar := PredefineIntVar(AScope, AName, plCurrentPlayer, UnitType, ABitCount);
      IntVars[sharedIntVar].IntArray := result;
      IntVars[sharedIntVar].Value := Values[0];
      for i := 1 to Size do
        Vars[i-1] := sharedIntVar;
    end else
    begin
      for i := 1 to Size do
        if (i <= MaxTriggerPlayers) or Constant then
        begin
          if i <= MaxTriggerPlayers then
            Vars[i-1] := PredefineIntVar(AScope, Name+'('+IntToStr(i)+')', IntToPlayer(i), UnitType, ABitCount)
          else
            Vars[i-1] := PredefineIntVar(AScope, Name+'('+IntToStr(i)+')', plNone, UnitType, ABitCount);
          IntVars[Vars[i-1]].Value := Values[i-1];
          IntVars[Vars[i-1]].Constant:= Constant;
          IntVars[Vars[i-1]].IntArray := result;
        end
        else
        begin
          Vars[i-1] := CreateIntVar(AScope, Name+'('+IntToStr(i)+')', Values[i-1], ABitCount, false, Constant);
          IntVars[Vars[i-1]].Predefined := true;
          IntVars[Vars[i-1]].IntArray := result;
        end;

      IntVars[PredefineIntVar(AScope, Name+'(Me)', plCurrentPlayer, UnitType, ABitCount)].IntArray := result;
    end;
  end;
end;

function PredefineIntArray(AScope: integer; AName: string; AUnitType: TStarcraftUnit;
  ABitCount: integer): integer;
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
    Scope := AScope;
    Constant := false;
    Predefined := true;
    Name := AName;
    Size:= MaxTriggerPlayers;
    UnitType := AUnitType;
    setlength(Values, Size);
    setlength(Vars, Size);
    BitCount:= ABitCount;
    for i := 1 to MaxTriggerPlayers do
    begin
      Values[i-1] := 0;
      Vars[i-1] := PredefineIntVar(AScope, Name+'('+IntToStr(i)+')', IntToPlayer(i), UnitType, ABitCount);
      IntVars[Vars[i-1]].IntArray := result;
    end;
    IntVars[PredefineIntVar(AScope, Name+'(Me)', plCurrentPlayer, UnitType, ABitCount)].IntArray := result;
  end;
end;

function IntArrayIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean
  ): integer;
var
  i: Integer;
begin
  for i := 0 to IntArrayCount-1 do
    if not IntArrays[i].Deleted and (IntArrays[i].Scope = AScope)
       and (CompareText(IntArrays[i].Name, AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to IntArrayCount-1 do
        if not IntArrays[i].Deleted and (IntArrays[i].Scope = AScope)
           and (CompareText(IntArrays[i].Name, AName)=0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;
  end;
  exit(-1);
end;

function CreateIntVar(AScope: integer; AName: string; AValue: integer;
  ABitCount: integer; ARandomize: boolean; AConstant: boolean): integer;
begin
  CheckReservedWord(AName);

  if AConstant and ARandomize then
    raise exception.Create('A constant cannot be random');

  if (AValue < 0) or (AValue > 1 shl ABitCount) or ((AValue = 1 shl ABitCount) and not ARandomize) then
    raise exception.Create('Value out of bounds');

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
    Scope := AScope;
    Constant := AConstant;
    Predefined := false;
    Name := AName;
    if AConstant then
    begin
      Player := plNone;
      UnitType := suConst;
    end else
    begin
      Player := CurIntVarPlayer;
      UnitType := NonKillableUnits[CurIntVarUnitNameIndex];
    end;
    Value := AValue;
    BitCount:= ABitCount;
    Randomize:= ARandomize;
    IntArray := -1;
  end;
end;

function CreateMultithreadIntVar(AThreads: TPlayers; AScope: integer;
  AName: string; ABitCount: integer): integer;
var
  arrIndex, maxPlayer: Integer;
begin
  maxPlayer := GetMaxPlayerNumber(AThreads);
  if maxPlayer > 8 then maxPlayer:= 8;
  arrIndex := CreateIntArray(AScope, AName, maxPlayer, [], ABitCount, false, true);
  result := IntArrays[arrIndex].Vars[0];
end;

function PredefineIntVar(AScope: integer; AName: string; APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ABitCount: integer): integer;
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
    Scope := AScope;
    Predefined := true;
    Constant := false;
    Name := AName;
    Player := APlayer;
    UnitType := AUnitType;
    BitCount:= ABitCount;
    Value := 0;
    Randomize:= false;
    AddToAcc := false;
    AddFromAcc:= false;
    IntArray := -1;
  end;
end;

function IntVarIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean): integer;
var
  i: Integer;
begin
  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Scope = AScope) and (CompareText(IntVars[i].Name, AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to IntVarCount-1 do
       if (IntVars[i].Scope = AScope) and (CompareText(IntVars[i].Name, AName)=0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;
  end;
  exit(-1);
end;

function AllocateTempInt(ABitCount: integer): integer;
var
  i: Integer;
begin
  if ABitCount > 16 then
  begin
    for i := 0 to TempInt24Count-1 do
      if not TempInts24[i].Used then
      begin
        TempInts24[i].Used := true;
        exit(TempInts24[i].IntVar);
      end;

    if TempInt24Count >= length(tempInts24) then
      setlength(TempInts24, TempInt24Count*2+4);
    TempInts24[TempInt24Count].IntVar:= CreateMultithreadIntVar([plAllPlayers],
      RunTimeScope, 'UInt24('+inttostr(TempInt24Count+1)+')', 24);
    TempInts24[TempInt24Count].Used := true;
    result := TempInts24[TempInt24Count].IntVar;
    inc(TempInt24Count);
  end else
  if ABitCount > 8 then
  begin
    for i := 0 to TempInt16Count-1 do
      if not TempInts16[i].Used then
      begin
        TempInts16[i].Used := true;
        exit(TempInts16[i].IntVar);
      end;

    if TempInt16Count >= length(tempInts16) then
      setlength(TempInts16, TempInt16Count*2+4);
    TempInts16[TempInt16Count].IntVar:= CreateMultithreadIntVar([plAllPlayers],
      RunTimeScope, 'UInt16('+inttostr(TempInt24Count+1)+')', 16);
    TempInts16[TempInt16Count].Used := true;
    result := TempInts16[TempInt16Count].IntVar;
    inc(TempInt16Count);
  end else
  begin
    for i := 0 to TempInt8Count-1 do
      if not TempInts8[i].Used then
      begin
        TempInts8[i].Used := true;
        exit(TempInts8[i].IntVar);
      end;

    if TempInt8Count >= length(tempInts8) then
      setlength(TempInts8, TempInt8Count*2+4);
    TempInts8[TempInt8Count].IntVar:= CreateMultithreadIntVar([plAllPlayers],
      RunTimeScope, 'UInt8('+inttostr(TempInt24Count+1)+')', 8);
    TempInts8[TempInt8Count].Used := true;
    result := TempInts8[TempInt8Count].IntVar;
    inc(TempInt8Count);
  end;
end;

procedure ReleaseTempInt(ATempInt: integer);
var
  i: Integer;
begin
  for i := 0 to TempInt24Count-1 do
    if TempInts24[i].IntVar = ATempInt then
    begin
      tempInts24[i].Used:= false;
      exit;
    end;
  for i := 0 to TempInt16Count-1 do
    if TempInts16[i].IntVar = ATempInt then
    begin
      tempInts16[i].Used:= false;
      exit;
    end;
  for i := 0 to TempInt8Count-1 do
    if TempInts8[i].IntVar = ATempInt then
    begin
      tempInts8[i].Used:= false;
      exit;
    end;
  raise exception.Create('Integer variable not found');
end;

function CreateBoolArray(AScope: integer; AName: string; ASize: integer;
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
    Scope := AScope;
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
        Vars[i-1] := CreateBoolVar(AScope, Name+'('+inttostr(i)+')', Values[i-1], Constant);
        BoolVars[Vars[i-1]].BoolArray := result;
      end;
  end;
end;

function PredefineBoolArray(AScope: integer; AName: string; ASize: integer;
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
    Scope := AScope;
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

function BoolArrayIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean
  ): integer;
var
  i: Integer;
begin
  for i := 0 to BoolArrayCount-1 do
    if (BoolArrays[i].Scope = AScope) and (CompareText(BoolArrays[i].Name, AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope:= GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to BoolArrayCount-1 do
        if (BoolArrays[i].Scope = AScope) and (CompareText(BoolArrays[i].Name, AName)=0) then exit(i);
      AScope:= GetWiderScope(AScope);
    end;
  end;
  exit(-1);
end;

function CreateBoolVar(AScope: integer; AName: string; AValue: TSwitchValue;
  AConstant: boolean): integer;
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
    Scope := AScope;
    Constant := AConstant;
    ReadOnly := AConstant;
    Name := AName;
    if AConstant then Switch := 0 else Switch:= CurBoolVarSwitch;
    Value := AValue;
    BoolArray := -1;
  end;

  Dec(CurBoolVarSwitch);
end;

function BoolVarIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean
  ): integer;
var
  i: Integer;
begin
  for i := 0 to BoolVarCount-1 do
    if (BoolVars[i].Scope = AScope) and (CompareText(BoolVars[i].Name, AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to BoolVarCount-1 do
        if (BoolVars[i].Scope = AScope) and (CompareText(BoolVars[i].Name, AName)=0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;
  end;
  exit(-1);
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
    PlayerPresenceVar[num] := BoolVarIndexOf(RunTimeScope, 'Present('+Inttostr(num)+')');
    if PlayerPresenceVar[num] = -1 then
      PlayerPresenceVar[num] := CreateBoolVar(RunTimeScope, 'Present('+Inttostr(num)+')', svClear);
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
    PlayerPresenceDefinedVar := BoolVarIndexOf(RunTimeScope, 'PresenceDefined');
    if PlayerPresenceDefinedVar = -1 then
      PlayerPresenceDefinedVar := CreateBoolVar(RunTimeScope, 'PresenceDefined', svClear);
  end;
  result := PlayerPresenceDefinedVar;
end;

function GetPlayerPresentArray: integer;
var bools: array of integer;
  i: Integer;
begin
  result := BoolArrayIndexOf(RunTimeScope, 'Present');
  if result = -1 then
  begin
    setlength(bools, MaxTriggerPlayers);
    for i := 0 to high(bools) do
      bools[i] := GetPlayerPresenceBoolVar(TPlayer(ord(plPLayer1)+i));
    result := PredefineBoolArray(RunTimeScope, 'Present',MaxTriggerPlayers,bools);
  end;
end;

function GetRunEventBoolVar: integer;
begin
  if RunEventBoolVar = -1 then
  begin
    RunEventBoolVar := BoolVarIndexOf(RunTimeScope, 'DoEvents');
    if RunEventBoolVar = -1 then
      RunEventBoolVar := CreateBoolVar(RunTimeScope, 'DoEvents', svClear);
  end;
  result := RunEventBoolVar;
end;

function RunEventBoolVarUsed: boolean;
begin
  result := RunEventBoolVar <>-1;
end;

function CreateUnitProp(AScope: integer; AName: string;
  AValue: TUnitProperties; AConstant: boolean): integer;
begin
  CheckReservedWord(AName);

  if not AConstant then
    raise Exception.Create('Unit properties must be constant');

  if UnitPropCount >= MaxUnitProperties then
    raise exception.Create('Too many unit properties (maximum is '+Inttostr(MaxUnitProperties)+')');

  if UnitPropCount >= length(UnitPropVars) then
    setlength(UnitPropVars, UnitPropCount*2+4);
  result := UnitPropCount;
  inc(UnitPropCount);

  with UnitPropVars[result] do
  begin
    Scope := AScope;
    Name := AName;
    Value := AValue;
  end;
end;

function UnitPropIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean
  ): integer;
var
  i: Integer;
begin
  for i := 0 to UnitPropCount-1 do
    if (UnitPropVars[i].Scope = AScope) and (CompareText(AName, UnitPropVars[i].Name) = 0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to UnitPropCount-1 do
        if (UnitPropVars[i].Scope = AScope) and (CompareText(AName, UnitPropVars[i].Name) = 0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;

  end;
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
    result := CreateUnitProp(RunTimeScope, 'UnitProperties'+inttostr(UnitPropCount+1), AProp, True);
end;

procedure CompileUnitProperties;
var
  i: Integer;
begin
  for i := 0 to UnitPropCount-1 do
  with uunitpropchunk.CompiledUnitProperties[i+1] do
  begin
    Used := true;
    Data.ValidFlags := FLAG_CLOAK or FLAG_BURROW or FLAG_IN_TRANSIT or
                       FLAG_HALLUCINATE or FLAG_INVINCIBLE;
    Data.ValidData:= FLAG_HITPOINTS or FLAG_SHIELD or FLAG_ENERGY or
                     FLAG_RESOURCE_AMOUNT or FLAG_HANGAR_COUNT;
    Data.Flags := 0;
    if UnitPropVars[i].Value.Cloaked then inc(Data.Flags, FLAG_CLOAK);
    if UnitPropVars[i].Value.Burrowed then inc(Data.Flags, FLAG_BURROW);
    if UnitPropVars[i].Value.Lifted then inc(Data.Flags, FLAG_IN_TRANSIT);
    if UnitPropVars[i].Value.Hallucinated then inc(Data.Flags, FLAG_HALLUCINATE);
    if UnitPropVars[i].Value.Invincible then inc(Data.Flags, FLAG_INVINCIBLE);
    Data.HitPoints:= UnitPropVars[i].Value.Life;
    Data.Shield:= UnitPropVars[i].Value.Shield;
    Data.Energy:= UnitPropVars[i].Value.Energy;
    Data.ResourceAmount:= UnitPropVars[i].Value.Resource;
    Data.HangarCount:= UnitPropVars[i].Value.HangarCount;
  end;
  for i := UnitPropCount to MaxUnitProperties-1 do
    uunitpropchunk.CompiledUnitProperties[i+1].Used := false;
end;

function CreateSound(AScope: integer; AName: string; AFilename: string;
  ADurationMs: integer; AConstant: boolean): integer;
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
    Scope := AScope;
    Name := AName;
    Filename:= AFilename;
    DurationMs:= ADurationMs;
  end;
end;

function SoundIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean): integer;
var
  i: Integer;
begin
  for i := 0 to SoundCount-1 do
    if (SoundVars[i].Scope = AScope) and (CompareText(SoundVars[i].Name,AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to SoundCount-1 do
        if (SoundVars[i].Scope = AScope) and (CompareText(SoundVars[i].Name,AName)=0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;
  end;
  exit(-1);
end;

function CreateString(AScope: integer; AName: string; AValue: string;
  AConstant: boolean): integer;
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
    Scope := AScope;
    Name := AName;
    Value := AValue;
  end;
end;

function StringIndexOf(AScope: integer; AName: string; ACheckGlobal: boolean
  ): integer;
var
  i: Integer;
begin
  for i := 0 to StringCount-1 do
    if (StringVars[i].Scope = AScope) and (CompareText(StringVars[i].Name,AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to StringCount-1 do
        if (StringVars[i].Scope = AScope) and (CompareText(StringVars[i].Name,AName)=0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;
  end;
  exit(-1);
end;

function CreateStringArray(AScope: integer; AName: string; ASize: integer;
  AValues: array of string; AConstant: boolean): integer;
var
  i: Integer;
begin
  CheckReservedWord(AName);

  if not AConstant then
    raise Exception.Create('Array of strings must be constant');

  if length(AValues)>ASize then
    raise exception.Create('Too many elements in value array');

  if StringArrayCount >= length(StringArrays) then
    setlength(StringArrays, StringArrayCount*2+4);
  result := StringArrayCount;
  inc(StringArrayCount);

  with StringArrays[result] do
  begin
    Scope := AScope;
    Name := AName;
    Size := ASize;
    Values := nil;
    setlength(Values,ASize);
    for i := 0 to high(AValues) do
      Values[i] := AValues[i];
  end;
end;

function StringArrayIndexOf(AScope: integer; AName: string;
  ACheckGlobal: boolean): integer;
var
  i: Integer;
begin
  for i := 0 to StringArrayCount-1 do
    if (StringArrays[i].Scope = AScope) and (CompareText(StringArrays[i].Name,AName)=0) then exit(i);
  if ACheckGlobal then
  begin
    AScope := GetWiderScope(AScope);
    while AScope <> -1 do
    begin
      for i := 0 to StringArrayCount-1 do
        if (StringArrays[i].Scope = AScope) and (CompareText(StringArrays[i].Name,AName)=0) then exit(i);
      AScope := GetWiderScope(AScope);
    end;
  end;
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

