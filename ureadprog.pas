unit ureadprog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl;

type
  TIntegerList = specialize TFPGList<Integer>;
  TUnitProperties = record
    Life, Shield, Energy: byte;
    Resource, HangarCount: integer;
    Invincible, Burrowed, Lifted, Hallucinated, Cloaked: boolean;
  end;

function ReadProg(AFilename: string): boolean;

const
  MaxTempBools = 32;
  MaxArraySize = 8;

const
  PlayerIdentifiers : array[TPlayer] of string =
    ('', 'Player1', 'Player2', 'Player3', 'Player4',
    'Player5', 'Player6', 'Player7', 'Player8',
    'Player9', 'Player10', 'Player11', 'Player12',
    '', 'CurrentPlayer', 'Foes', 'Allies',
    'NeutralPlayers', 'AllPlayers',
    'Force1', 'Force2', 'Force3', 'Force4',
    '', '', '', '', 'NonAlliedVictoryPlayers');

const
  NonKillableUnits: array[1..51] of string =
('Data Disc',
'Goliath Turret',
'Tank Turret(Tank Mode)',
'Nuclear Missile',
'Alan Schezar Turret',
'Edmund Duke Turret',
'Tank Turret (Siege Mode)',
'Scanner Sweep',
'Unused - Cargo Ship',
'Unused - Mercenary Gunship',
'Map Revealer',
'Disruption Web',
'Unused Zerg Building1',
'Unused Zerg Building2',
'Unused Protoss Building1',
'Unused Protoss Building2',
'Khaydarin Crystal Formation',
'Mineral Field (Type 1)',
'Mineral Field (Type 2)',
'Mineral Field (Type 3)',
'Cave-in',
'Cantina',
'Mining Platform',
'Independant Command Center',
'Independant Starport',
'Independant Jump Gate',
'Ruins',
'Kyadarin Crystal Formation',
'Vespene Geyser',
'Zerg Marker',
'Terran Marker',
'Protoss Marker',
'Zerg Beacon',
'Terran Beacon',
'Protoss Beacon',
'Zerg Flag Beacon',
'Terran Flag Beacon',
'Protoss Flag Beacon',
'Dark Swarm',
'Floor Hatch',
'Left Upper Level Door',
'Right Upper Level Door',
'Left Pit Door',
'Right Pit Door',
'Start Location',
'Flag',
'Uraj Crystal',
'Khalis Crystal',
'Psi Emitter',
'Khaydarin Crystal',
'Cave');

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

  TempBools: array[0..MaxTempBools-1] of integer;
  TempBoolCount: integer;

function CreateBoolVar(AName: string; AValue: TSwitchValue; AConstant: boolean = false): integer;
function BoolVarIndexOf(AName: string): integer;
procedure NeedTempBools(AQuantity: integer);

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

function VarNameUsed(AName: string): boolean;

var
  MainProg: TInstructionList;
  Procedures: array of record
    Name: string;
    ParamCount: integer;
    Instructions: TInstructionList;
    StartIP: integer;
    Done: boolean;
    Calls: TIntegerList;
  end;
  ProcedureCount: integer;

function CreateProcedure(AName: string; AParamCount: integer): integer;
function ProcedureIndexOf(AName: string; AParamCount: integer): integer;

var
  Events: array of record
    Conditions: TConditionList;
    Instructions: TInstructionList;
    Preserve: boolean;
  end;
  EventCount: integer;

function CreateEvent(AConditions: TConditionList; APreserve: boolean): integer;

function ParseRandom(ALine: TStringList; var AIndex: integer): integer;
function IsPowerOf2(ANumber: integer): boolean;
function GetExponentOf2(AValue: integer): integer;
function RemoveQuotes(AQuotedText: string): string;

procedure CheckReservedWord(AText: string);

var HyperTriggers: boolean;

implementation

type
  ArrayOfInteger = array of integer;
  TConditionOperator = (coNone, coEqual, coGreaterThan, coLowerThan, coGreaterThanOrEqual, coLowerThanOrEqual, coNotEqual);

const
  NotConditionOperator : array[TConditionOperator] of TConditionOperator = (coNone, coNotEqual, coLowerThanOrEqual, coGreaterThanOrEqual, coLowerThan, coGreaterThan, coEqual);

function GetExponentOf2(AValue: integer): integer;
begin
  result := 0;
  while AValue > 1 do
  begin
    AValue := AValue shr 1;
    inc(result);
  end;
end;

function RemoveQuotes(AQuotedText: string): string;
begin
  if (length(AQuotedText)<2) or (AQuotedText[1]<>'"') or (AQuotedText[length(AQuotedText)]<>'"') then
    raise exception.Create('Quotes not found');

  result := StringReplace(StringReplace(StringReplace(copy(AQuotedText, 2, length(AQuotedText)-2), '\\', #0, [rfReplaceAll]), '\"', '"', [rfReplaceAll]), #0, '\', [rfReplaceAll]);
end;

procedure CheckReservedWord(AText: string);
const
  ReservedWords: array[1..49] of string =
    ('Dim','As','Const','Sub','When','End','If','EndIf', 'Then','Else','Not','And','Or','Xor','While','Option','Return','On','Off','Hyper','Boolean','Byte','Integer','String','True','False',
     'For','To','Step','Next','Do','Loop','Until','Len','Chr','Asc','ElseIf','Select','Case','Exit','Function','LBound','UBound','Me','Now','ReDim', 'Preserve', 'Rnd','New');
var
  i: Integer;
  pl: TPlayer;
begin
  for i := low(reservedWords) to high(reservedWords) do
    if ComparetexT(reservedWords[i],AText)=0 then raise exception.Create('"' + reservedWords[i] + '" is a word reserved');
  for pl := low(TPlayer) to high(TPlayer) do
    if CompareText(PlayerIdentifiers[pl], AText)=0 then raise exception.Create('"' + PlayerIdentifiers[pl] + '" is a player identifier');
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


function CreateIntArray(AName: string; ASize: integer;
  AValues: array of integer; AConstant: boolean): integer;
var
  i: Integer;
begin
  if VarNameUsed(AName) then
    raise Exception.Create('Name already in use');
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
  if VarNameUsed(AName) then
    raise Exception.Create('Name already in use');
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
  if VarNameUsed(AName) then
    raise Exception.Create('Name already in use');
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

function ParseLine(ALine: string): TStringList;
var
  p,start: Integer;
  inSpace, inStr, prevBackslash: boolean;
  token: string;
  i,j: Integer;
begin
  result := TStringList.Create;

  p := 1;
  start := 1;
  inSpace := true;
  inStr := false;
  prevBackslash := false;
  while p <= length(ALine) do
  begin
    if inStr or not (ALine[p] in[#0..' ']) then
    begin
      if not inStr and not inSpace and not (ALine[p] in['A'..'Z','a'..'z','_','0'..'9']) then
      begin
        token := copy(ALine, start, p-start);
        result.Add(token);
        start := p;
      end;

      if inSpace then
      begin
        inSpace := false;
        start := p;
      end;
      if (ALine[p] = '"') and not inStr then inStr := true else
      if (ALine[p] = '"') and inStr then
      begin
        if not prevBackslash then
        begin
          inStr := false;
          token := copy(ALine, start, p-start+1);
          result.Add(token);
          inSpace := true;
        end;
      end;
      if not prevBackslash and inStr and (ALine[p]='\') then prevBackslash:= true
      else prevBackslash:= false;

      if not inStr and not inSpace and not (ALine[p] in['A'..'Z','a'..'z','_','0'..'9']) then
      begin
        token := copy(ALine, start, p-start+1);
        result.Add(token);
        inSpace := true;
      end;
    end else
    begin
      if not inSpace then
      begin
        token := copy(ALine, start, p-start);
        result.Add(token);
        inSpace := true;
        start := p;
      end;
    end;

    inc(p);
  end;
  if inStr then
    raise Exception.Create('String over end of line');
  if not inSpace then
  begin
    token := copy(ALine,start,length(ALine)-start+1);
    result.Add(token);
  end;
  for i := 0 to result.Count-1 do
  begin
    if result[i] = '''' then
    begin
      for j := result.Count-1 downto i do
        result.Delete(j);
      break;
    end;
  end;
end;

function IsValidVariableName(AText: string): boolean;
begin
  if AText = '' then exit(false);
  if not (AText[1] in['A'..'Z','a'..'z']) then exit(false);
  if pos('"', AText)<>0 then exit(false);
  exit(true);
end;

function TryToken(ALine: TStringList; var AIndex: integer; AToken: string): boolean;
begin
  if (AIndex < ALine.Count) and (CompareText(ALine[AIndex],AToken) = 0) then
  begin
    inc(AIndex);
    exit(true);
  end else
    exit(false);
end;

procedure ExpectToken(ALine: TStringList; var AIndex: integer; AToken: string);
begin
  if not TryToken(ALine,AIndex,AToken) then
  begin
    if AIndex >= ALine.Count then
      raise exception.Create('"'+AToken+'" expected but end of line found')
    else
      raise exception.Create('"'+AToken+'" expected but "' + ALine[AIndex] + '" found');
  end;
end;

function TryInteger(ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
var errPos, idxVar: integer;
begin
  AValue := 0;
  if AIndex < ALine.Count then
  begin
    val(ALine[AIndex], AValue, errPos);
    if errPos = 0 then
    begin
      inc(AIndex);
      exit(true);
    end else
    begin
      idxVar := IntVarIndexOf(ALine[AIndex]);
      if (idxVar<>-1) and IntVars[idxVar].Constant then
      begin
        AValue := IntVars[idxVar].Value;
        inc(AIndex);
        exit(true);
      end;
      exit(false);
    end;
  end
  else exit(false);
end;

function TryBoolean(ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
var
  idxVar: Integer;
begin
  AValue := false;
  if TryToken(ALine,AIndex,'False') then exit(true)
  else if TryToken(ALine,AIndex,'True') then
  begin
    AValue := true;
    exit(true)
  end else
  begin
    if AIndex < ALine.Count then
    begin
      idxVar := BoolVarIndexOf(ALine[AIndex]);
      if (idxVar<>-1) and BoolVars[idxVar].Constant then
      begin
        AValue := (BoolVars[idxVar].Value = svSet);
        inc(AIndex);
        exit(true);
      end;
    end;
    exit(false);
  end;
end;

function ExpectInteger(ALine: TStringList; var AIndex: integer): integer;
begin
  result := 0;
  if not TryInteger(ALine,AIndex,result) then
  begin
    if AIndex > ALine.Count then
      raise exception.Create('Integer expected but end of line found') else
      raise exception.Create('Integer expected but "' + ALine[AIndex] + '" found');
  end;
end;

function TryConditionOperator(ALine: TStringList; var AIndex: integer): TConditionOperator;
begin
  result := coNone;
  if TryToken(ALine,AIndex,'<') then result := coLowerThan;
  if TryToken(ALine,AIndex,'>') then
  begin
    case result of
    coLowerThan: result := coNotEqual;
    else result := coGreaterThan;
    end;
  end;
  if (result <> coNotEqual) and TryToken(ALine,AIndex,'=') then
  begin
    case result of
    coLowerThan: result := coLowerThanOrEqual;
    coGreaterThan: result := coGreaterThanOrEqual;
    else result := coEqual;
    end;
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

function TryParsePlayer(ALine: TStringList; var AIndex: integer): TPlayer;
var
  pl: TPlayer;
begin
  for pl := succ(plNone) to high(TPlayer) do
    if CompareText(ALine[AIndex],PlayerIdentifiers[pl])=0 then
    begin
      inc(AIndex);
      exit(pl);
    end;
  exit(plNone);
end;

type
  TScalarVariableType = (svtNone, svtInteger, svtSwitch);
  TScalarVariable = record
    VarType: TScalarVariableType;
    Name: string;
    Index: integer;
    Player: TPlayer;
    Constant: boolean;
    IntValue: integer;
    BoolValue: boolean;
  end;

function TryScalarVariable(ALine: TStringList; var AIndex: integer): TScalarVariable;
var varIdx, arrayIndex: integer;
begin
  result.VarType := svtNone;
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
  begin
    varIdx := IntVarIndexOf(ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
      result.VarType := svtInteger;
      result.Player := IntVars[varIdx].Player;
      result.Name := IntVars[varIdx].UnitType;
      result.Index := -1;
      result.Constant:= IntVars[varIdx].Constant;
      result.IntValue:= IntVars[varIdx].Value;
      result.BoolValue:= IntVars[varIdx].Value<>0;
    end else
    begin
      varIdx := BoolVarIndexOf(ALine[AIndex]);
      if varIdx <> -1 then
      begin
        inc(AIndex);
        result.VarType := svtSwitch;
        result.Player := plNone;
        result.Name := '';
        result.Index := BoolVars[varIdx].Switch;
        result.Constant:= BoolVars[varIdx].Constant;
        result.IntValue := integer(BoolVars[varIdx].Value = svSet);
        result.BoolValue:= BoolVars[varIdx].Value = svSet;
      end else
      begin
        varIdx := IntArrayIndexOf(ALine[AIndex]);
        if varIdx <> -1 then
        begin
          Inc(AIndex);
          if TryToken(ALine,AIndex,'(') then
          begin
            arrayIndex := ExpectInteger(ALine, AIndex);
            if (arrayIndex < 1) or (arrayIndex > IntArrays[varIdx].Size) then
              raise exception.Create('Array index out of bounds');
            ExpectToken(ALine, AIndex, ')');
            result.VarType := svtInteger;
            result.Player := IntToPlayer(arrayIndex);
            result.Name := IntArrays[varIdx].UnitType;
            result.Index := -1;
            result.Constant:= IntArrays[varIdx].Constant;
            result.IntValue:= IntArrays[varIdx].Values[arrayIndex];
            result.BoolValue:= result.IntValue<>0;
          end else
          begin
            Dec(AIndex);
          end;
        end;
      end;
    end;
  end;
end;

function ExpectString(ALine: TStringList; var AIndex: integer): string;
var
  scalar: TScalarVariable;
  idxVar, intVal: Integer;
  boolVal: boolean;
begin

  result := '';
  repeat
    if AIndex >= ALine.Count then
      raise exception.Create('Expecting string but end of line found');

    if copy(ALine[AIndex],1,1) = '"' then
    begin
      result += RemoveQuotes(ALine[AIndex]);
      Inc(AIndex);
    end else
    if TryInteger(ALine,AIndex,intVal) then
    begin
      result += inttostr(intVal);
    end else
    if TryBoolean(ALine,AIndex,boolVal) then
    begin
      result += BoolToStr(boolVal, 'True', 'False');
    end else
    begin
     idxVar := StringIndexOf(ALine[AIndex]);
     if idxVar <> -1 then
     begin
       result += StringVars[idxVar].Value;
       inc(AIndex);
     end else
     begin
       scalar := TryScalarVariable(ALine,AIndex);
       if scalar.VarType <> svtNone then
       begin
         if not scalar.Constant then
           raise exception.Create('Only constants can be used in a string');

         case scalar.VarType of
         svtInteger: result += inttostr(scalar.IntValue);
         svtSwitch: result += BoolToStr(scalar.BoolValue, 'True', 'False');
         else raise exception.Create('Unhandled case');
         end;
       end else
       raise exception.Create('Expecting string but "' + ALine[AIndex] + '" found');
     end;
    end;
  until not TryToken(ALine,AIndex,'&');
end;

function ExpectCondition(ALine: TStringList; var AIndex: integer): TCondition;
var
  intVal: Integer;
  op: TConditionOperator;
  boolNot: Boolean;
  scalar: TScalarVariable;
  boolVal: boolean;
  unitType, locStr: String;
  pl: TPlayer;
begin
  if TryBoolean(ALine,AIndex,boolVal) then
  begin
    if boolVal then
      result := TAlwaysCondition.Create
    else
      result := TNeverCondition.Create
  end else
  begin
    boolNot := TryToken(ALine,AIndex,'Not');

    pl := TryParsePlayer(ALine,AIndex);
    if pl <> plNone then
    begin
      ExpectToken(ALine,AIndex,'.');
      if TryToken(ALine,AIndex,'Bring') then
      begin
        ExpectToken(ALine,AIndex,'(');
        unitType := ExpectString(ALine,AIndex);
        ExpectToken(ALine,AIndex,',');
        locStr := ExpectString(ALine,AIndex);
        ExpectToken(ALine,AIndex,')');
        op := TryConditionOperator(ALine,AIndex);
        if op = coNone then
          raise exception.Create('Comparison expected');

        if boolNot then op := NotConditionOperator[op];

        intVal := ExpectInteger(ALine,AIndex);
        case op of
        coEqual: result := TBringCondition.Create(pl, unitType, locStr, icmExactly, intVal);
        coGreaterThanOrEqual: result := TBringCondition.Create(pl, unitType, locStr, icmAtLeast, intVal);
        coLowerThanOrEqual: result := TBringCondition.Create(pl, unitType, locStr, icmAtMost, intVal);
        coGreaterThan: if intVal = maxLongint then
                          result := TNeverCondition.Create
                       else
                          result := TBringCondition.Create(pl, unitType, locStr, icmAtLeast, intVal+1);
        coLowerThan: if intVal = 0 then
                          result := TNeverCondition.Create
                       else
                          result := TBringCondition.Create(pl, unitType, locStr, icmAtMost, intVal-1);
        coNotEqual: result := TNotCondition.Create([TBringCondition.Create(pl, unitType, locStr, icmExactly, intVal)]);
        else
          raise exception.Create('Unhandled case');
        end;
        exit;
      end
      else
        raise exception.Create('Expecting player condition');
    end;

    scalar := TryScalarVariable(ALine,AIndex);
    if scalar.VarType = svtNone then
    begin
      if TryBoolean(ALine,AIndex,boolVal) then
      begin
        if boolVal xor boolNot then
          result := TAlwaysCondition.Create
        else
          result := TNeverCondition.Create;
        exit;
      end else
        raise exception.Create('Expecting variable name');
    end;

    case scalar.VarType of
    svtInteger:
    begin
      op := TryConditionOperator(ALine,AIndex);
      if op = coNone then
        raise exception.Create('Comparison expected')
      else
      begin
        if boolNot then op := NotConditionOperator[op];

        intVal := ExpectInteger(ALine,AIndex);
        case op of
        coEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmExactly, intVal);
        coGreaterThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtLeast, intVal);
        coLowerThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtMost, intVal);
        coGreaterThan: if intVal = maxLongint then
                          result := TNeverCondition.Create
                       else
                          result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtLeast, intVal+1);
        coLowerThan: if intVal = 0 then
                          result := TNeverCondition.Create
                       else
                          result := TIntegerCondition.Create(scalar.Player, scalar.Name, icmAtMost, intVal-1);
        coNotEqual: result := TNotCondition.Create([TIntegerCondition.Create(scalar.Player, scalar.Name, icmExactly, intVal)]);
        else
          raise exception.Create('Unhandled case');
        end;
      end;
    end;
    svtSwitch:
      result := TSwitchCondition.Create(scalar.Index, not boolNot)
    else
        raise exception.Create('Unhandled case');
    end;
  end;
end;

function ExpectConditions(ALine: TStringList; var AIndex: integer): TConditionList;
var
  i, j: Integer;
begin
  result := TConditionList.Create;
  try
    repeat
      result.Add(ExpectCondition(ALine,AIndex));
      if not TryToken(ALine,AIndex,'And') then break;
    until false;
  except on ex:exception do
    begin
      result.Free;
      raise exception.Create(ex.Message);
    end;
  end;
  for i := result.Count-1 downto 0 do
    if (result[i] is TAlwaysCondition) and (result.Count > 1) then
    begin
      result[i].Free;
      result.Delete(i);
    end else
    if result[i] is TNeverCondition then
    begin
      for j := result.Count-1 downto 0 do
        if j <> i then
        begin
          result[j].Free;
          result.Delete(j);
        end;
    end;
end;

function TryUnitProperties(ALine: TStringList; var AIndex: integer; out AProp: TUnitProperties): boolean;
var idx, intVal: integer;
  name: String;
  valueType: string;
  boolVal: boolean;

  procedure ValueInteger(AMin,AMax: integer);
  begin
    if valueType <> 'Integer' then
      raise exception.Create('Expecting integer value');
    if (intVal < AMin) or (intVal > AMax) then
      raise exception.Create('Value out of range (' + inttostr(AMin)+' to '+Inttostr(AMax)+')');
  end;
  procedure ValueBool;
  begin
    if valueType <> 'Boolean' then
      raise exception.Create('Expecting bool value');
  end;

begin
  AProp.Life := 100;
  AProp.Shield := 100;
  AProp.Energy := 100;
  AProp.Resource := 0;
  AProp.HangarCount := 0;
  AProp.Burrowed:= false;
  AProp.Cloaked:= false;
  AProp.Hallucinated:= false;
  AProp.Invincible:= false;
  AProp.Lifted := false;
  result := false;

  idx := AIndex;
  intVal := 0;

  if not TryToken(ALine,idx,'{') then exit;
  if not TryToken(ALine,idx,'.') then exit;

  while true do
  begin
    if idx >= ALine.Count then raise exception.Create('Unexpected end of line');
    name := ALine[idx];
    if not IsValidVariableName(name) then raise exception.Create('Expecting variable name but "' + name + '" found');
    inc(idx);
    ExpectToken(ALine,idx,'=');
    if TryInteger(ALine,idx,intVal) then valueType := 'Integer'
    else if TryBoolean(ALine,idx,boolVal) then valueType := 'Boolean'
    else raise exception.Create('Expecting boolean or integer value');

    if CompareText(name,'Life')=0 then
    begin
      ValueInteger(0,100);
      AProp.Life := intVal;
    end else
    if CompareText(name,'Shield')=0 then
    begin
      ValueInteger(0,100);
      AProp.Shield := intVal;
    end else
    if CompareText(name,'Energy')=0 then
    begin
      ValueInteger(0,100);
      AProp.Energy := intVal;
    end else
    if CompareText(name,'Resource')=0 then
    begin
      ValueInteger(0,maxLongint);
      AProp.Resource := intVal;
    end else
    if CompareText(name,'HangarCount')=0 then
    begin
      ValueInteger(0,maxLongint);
      AProp.HangarCount := intVal;
    end else
    if CompareText(name,'Burrowed')=0 then
    begin
      ValueBool;
      AProp.Burrowed := boolVal;
    end else
    if CompareText(name,'Cloaked')=0 then
    begin
      ValueBool;
      AProp.Cloaked := boolVal;
    end else
    if CompareText(name,'Hallucinated')=0 then
    begin
      ValueBool;
      AProp.Hallucinated := boolVal;
    end else
    if CompareText(name,'Invincible')=0 then
    begin
      ValueBool;
      AProp.Invincible := boolVal;
    end else
    if CompareText(name,'Lifted')=0 then
    begin
      ValueBool;
      AProp.Lifted := boolVal;
    end;

    if not TryToken(ALine,idx,',') then
    begin
      ExpectToken(ALine,idx,'}');
      break;
    end else
      ExpectToken(ALine,idx,'.');
  end;
  AIndex := idx;
  result := true;
end;

function TryUnitPropVar(ALine: TStringList; var AIndex: integer): integer;
var
  idxProp: Integer;
  prop: TUnitProperties;
begin
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
  begin
    idxProp := UnitPropIndexOf(ALine[AIndex]);
    if idxProp <> -1 then
    begin
      inc(AIndex);
      exit(idxProp);
    end;
  end;

  if TryUnitProperties(ALine,AIndex,prop) then
  begin
    result := FindOrCreateUnitProperty(prop);
  end else
    exit(-1);
end;

function CreateUnitProp(AName: string; AValue: TUnitProperties; AConstant: boolean): integer;
begin
  if VarNameUsed(AName) then
    raise Exception.Create('Name already in use');
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
  if VarNameUsed(AName) then
    raise Exception.Create('Name already in use');
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

function VarNameUsed(AName: string): boolean;
begin
  result := (IntVarIndexOf(AName)<>-1) or (BoolVarIndexOf(AName)<>-1) or (IntArrayIndexOf(AName)<>-1) or
    (ProcedureIndexOf(AName,0)<>-1) or (UnitPropIndexOf(AName) <> -1) or (StringIndexOf(AName)<>-1);
end;

function CreateProcedure(AName: string; AParamCount: integer): integer;
begin
  if ProcedureIndexOf(AName, AParamCount)<>-1 then
    raise exception.Create('Procedure already declared with this signature');
  CheckReservedWord(AName);

  if ProcedureCount >= length(Procedures) then
    setlength(Procedures, ProcedureCount*2+4);
  result := ProcedureCount;
  inc(ProcedureCount);

  with Procedures[result] do
  begin
    Name := AName;
    ParamCount:= AParamCount;
    Instructions := TInstructionList.Create;
    StartIP := -1;
    Done := false;
    Calls := TIntegerList.Create;
  end;
end;

function ProcedureIndexOf(AName: string; AParamCount: integer): integer;
var
  i: Integer;
begin
  for i := 0 to ProcedureCount-1 do
  begin
    if (CompareText(AName, Procedures[i].Name)=0) and
      (AParamCount = Procedures[i].ParamCount) then
      exit(i);
  end;
  exit(-1);
end;

function CreateEvent(AConditions: TConditionList; APreserve: boolean): integer;
begin
  if EventCount >= length(Events) then
    setlength(Events, EventCount*2+4);
  result := EventCount;
  inc(EventCount);

  with Events[result] do
  begin
    Conditions := AConditions;
    Instructions := TInstructionList.Create;
    Preserve := APreserve;
  end;
end;

function ParseRandom(ALine: TStringList; var AIndex: integer): integer;
var errPos: integer;
begin
  if (AIndex < ALine.Count) and (CompareText(ALine[AIndex], 'Rnd') = 0) then
  begin
    inc(AIndex);
    if (AIndex < ALine.Count) and (ALine[AIndex] = '*') then
    begin
      inc(AIndex);
      if AIndex >= ALine.Count then
        raise exception.Create('Expecting integer value');
      val(ALine[AIndex], result, errPos);
      if errPos > 0 then
        raise exception.Create('Expecting integer value');
      inc(AIndex);

      if not IsPowerOf2(result) then
        raise exception.Create('Expecting power of 2');

      if result < 2 then
        raise Exception.Create('Value must be greated or equal to 2');
    end else
      exit(2);
  end else
    exit(-1);
end;

function ParseIntArray(ALine: TStringList; var AIndex: integer): ArrayOfInteger;
var count: integer;
begin
  setlength(result, MaxArraySize);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count = MaxArraySize then
      raise exception.Create('Array can contain at most ' + inttostr(MaxArraySize) + ' values');

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if not TryInteger(ALine, AIndex, result[count]) then
      raise exception.Create('Expecting integer or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ParsePlayers(ALine: TStringList; var AIndex: integer): TPlayers;
var
  pl: TPlayer;
begin
  result := [];
  if TryToken(ALine, AIndex, '{') then
  begin
    repeat
      pl := TryParsePlayer(ALine,AIndex);
      if pl = plNone then raise exception.Create('Expecting player but "' + ALine[AIndex] + '" found');
      result += [pl];

      if TryToken(ALine, aIndex, '}') then break
      else ExpectToken(ALine,AIndex,',');
    until false;
  end else
  begin
    pl := TryParsePlayer(ALine,AIndex);
    if pl = plNone then raise exception.Create('Expecting player but "' + ALine[AIndex] + '" found');
    result := [pl];
  end;
end;

function ProcessSub(ADeclaration: string): integer;
var
  line: TStringList;
  index: Integer;
  name: String;
begin
  line := ParseLine(ADeclaration);
  index := 0;
  ExpectToken(line,index,'Sub');

  name := line[index];
  if not IsValidVariableName(name) then
    raise exception.Create('Invalid procedure name');
  inc(index);

  if TryToken(line,index,'(') then
  begin
    ExpectToken(line,index,')');
  end;

  result := CreateProcedure(name,0);

  if index <> line.Count then
    raise exception.Create('End of line expected');
end;

function ProcessEvent(ADeclaration: string): integer;
var
  line: TStringList;
  index: Integer;
  conds: TConditionList;
begin
  line := ParseLine(ADeclaration);
  index := 0;
  ExpectToken(line,index,'When');

  conds := ExpectConditions(line,index);

  result := CreateEvent(conds, not TryToken(line,index,'Once'));

  if index < line.Count then
    raise exception.Create('End of line expected');
end;

procedure ProcessDim(ADeclaration: string; AConstant: boolean);
var line: TStringList;
  index: Integer;
  varName, varType: String;
  varValue, arraySize: integer;
  isArray: boolean;
  rndVal: integer;
  arrValues: ArrayOfInteger;
  boolVal: boolean;
  prop: TUnitProperties;

  procedure ExpectArraySize;
  begin
    if not TryInteger(line,index,arraySize) then
      raise exception.Create('Expecting array size or "]"');
    if (arraySize < 1) or (arraySize > MaxArraySize) then
      raise Exception.Create('Array size can go from 1 to ' + inttostr(MaxArraySize));
  end;

begin
  line := ParseLine(ADeclaration);
  index := 0;
  try
    if TryToken(line,index,'Const') then
      AConstant := true
    else
    begin
      AConstant := false;
      ExpectToken(line,index,'Dim');
    end;

    index := 1;
    while index < line.Count do
    begin
      if index > 1 then ExpectToken(line,index,',');

      isArray:= false;
      varName := line[index];
      varValue := 0;
      arraySize := 0;
      if not IsValidVariableName(varName) then
        raise exception.Create('Invalid variable name');
      inc(index);

      if TryToken(line,index,'(') then
      begin
        isArray:= true;
        if not TryToken(line,index,')') then
        begin
          ExpectArraySize;
          ExpectToken(line,index,')');
        end;
      end;

      if TryToken(line,index,'As') then
      begin
        if index >= line.Count then
          raise Exception.Create('Expecting variable type');

        if TryToken(line,index,'Integer') then varType := 'Integer'
        else if TryToken(line,index,'Boolean') then varType := 'Boolean'
        else if TryToken(line,index,'String') then varType := 'String'
        else if TryToken(line,index,'UnitProperties') then varType := 'UnitProperties'
        else raise Exception.Create('Unknown type : ' + line[index]);

        if not isArray and (varType <> 'UnitProperties') and TryToken(line,index,'(') then
        begin
          isArray := true;
          if not TryToken(line,index,')') then
          begin
            ExpectArraySize;
            ExpectToken(line,index,')');
          end;
        end;
      end else
        varType := '?';

      if TryToken(line,index,'=') then
      begin
        if isArray then
        begin
          arrValues := ParseIntArray(line,index);
          if (arraySize <> 0) and (length(arrValues) <> arraySize) then
            raise exception.Create('Array size mismatch');
          if arraySize = 0 then
          begin
            if (length(arrValues) < 1) or (length(arrValues) > MaxArraySize) then
              raise exception.Create('Array size can go from 1 to ' + inttostr(MaxArraySize));
            arraySize:= length(arrValues);
          end;
          CreateIntArray(varName, arraySize, arrValues, AConstant);
        end else
        if varType = 'UnitProperties' then
        begin
          if TryUnitProperties(line,index,prop) then
          begin
            CreateUnitProp(varName, prop, AConstant);
          end;
        end else
        if varType = 'String' then
        begin
          CreateString(varName,ExpectString(line,index), AConstant);
        end else
        begin
          if TryBoolean(line,index,boolVal) then
          begin
            if varType = '?' then varType := 'Boolean';
            if varType <> 'Boolean' then
              raise Exception.Create('Value is not of expected type');
            CreateBoolVar(varName, BoolToSwitch[boolVal], AConstant);
          end
          else
          if (index < line.Count) and (copy(line[index],1,1)='"') then
          begin
            CreateString(varName,ExpectString(line,index), AConstant);
          end else
          begin
            rndVal := ParseRandom(line, index);
            if rndVal > 0 then
            begin
              if varType = 'Boolean' then
              begin
                if rndVal <> 2 then raise Exception.Create('Boolean can have only 2 values');
                CreateBoolVar(varName, svRandomize, AConstant);
              end
              else
                CreateIntVar(varName, rndVal, true, AConstant);

            end else
            begin
              if varType = 'Boolean' then
                raise Exception.Create('Expecting boolean value');

              varValue := ExpectInteger(line,index);

              CreateIntVar(varName, varValue, False, AConstant);
            end;
          end;
        end;
      end else
      begin
        if varType = '?' then
          raise Exception.Create('Variable type not specified');

        if isArray then
        begin
          if varType = 'Boolean' then
            raise Exception.Create('Array of booleans not supported, use integers instead')
          else
          begin
            if arraySize= 0 then arraySize := MaxArraySize;
            CreateIntArray(varName, arraySize, [], AConstant);
          end;
        end else
        begin
          if varType = 'Boolean' then
            CreateBoolVar(varName, svClear, AConstant)
          else
            CreateIntVar(varName, 0, false, AConstant);
        end;
      end;
    end;
  finally
    line.Free;
  end;

end;

procedure ParseInstruction(AText: string; AProg: TInstructionList);
var
  line: TStringList;
  index, intVal, idxArr, i, propIndex, propVal: integer;
  params: TStringList;
  name, assignOp, msg, locStr, unitType, destLocStr, orderStr: String;
  done, boolVal: boolean;
  scalar, scalar2: TScalarVariable;
  players: TPlayers;
  conds: TConditionList;
  ints: ArrayOfInteger;
  sim: TSetIntegerMode;
  pl, destPl: TPlayer;
  prop : TSetUnitProperty;
  props: TUnitProperties;

  procedure CheckEndOfLine;
  begin
    if index <> line.Count then
      raise exception.Create('Expecting end of line');
  end;

  function ParseOptionalQuantity: integer;
  begin
    if TryToken(line,index,'All') then
    begin
      result := -1;
      ExpectToken(line,index,',');
    end else
    if TryInteger(line,index,result) then
    begin
      ExpectToken(line,index,',');
    end else
      result := -1; //All by default
  end;

begin
  line := ParseLine(AText);
  try
    if (line.Count = 0) or ((line.Count = 1) and (line[0] = '')) then exit;

    index := 0;
    if TryToken(line,index,'Return') then
      AProg.Add( TReturnInstruction.Create )
    else
    if TryToken(line,index,'Option') then
    begin
      if TryToken(line,index,'Hyper') then
      begin
        if TryToken(line,index,'On') then HyperTriggers:= true
        else If TryToken(line,index,'Off') then HyperTriggers:= false
        else raise exception.Create('Expecting On or Off');
      end else
        raise exception.Create('Unknown option');
    end else
    if TryToken(line,index,'EndIf') then
    begin
      CheckEndOfLine;
      AProg.Add(TEndIfInstruction.Create);
    end else
    if TryToken(line,index,'End') then
    begin
      if TryToken(line,index,'While') then
      begin
        CheckEndOfLine;
        AProg.Add(TEndWhileInstruction.Create);
      end else
      if TryToken(line,index,'If') then
      begin
        CheckEndOfLine;
        AProg.Add(TEndIfInstruction.Create);
      end else
        raise exception.Create('Unknown end instruction');
    end else
    if TryToken(line,index,'While') then
    begin
      conds := ExpectConditions(line,index);
      if (conds.Count = 1) and (conds[0] is TAlwaysCondition) then
        raise exception.Create('Infinite loop not allowed. You can use When True instead');
      AProg.Add(TWhileInstruction.Create(conds));
      CheckEndOfLine;
    end else
    if TryToken(line,index,'If') then
    begin
      conds := ExpectConditions(line,index);
      AProg.Add(TIfInstruction.Create(conds));
      ExpectToken(line,index,'Then');
      CheckEndOfLine;
    end else
    if TryToken(line,index,'Else') then
    begin
      CheckEndOfLine;
      AProg.Add(TElseInstruction.Create);
    end else
    begin
      done := false;
      scalar := TryScalarVariable(line,index);
      if scalar.VarType <> svtNone then
      begin
        if TryToken(line,index,'=') then
        begin
          done := true;
          if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
          case scalar.VarType of
          svtInteger:
            begin
              intVal := ParseRandom(line, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simRandomize, intVal));
              end else
              begin
                if TryInteger(line,index,intVal) then
                begin
                  CheckEndOfLine;
                  AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simSetTo, intVal));
                end else
                begin
                  scalar2 := TryScalarVariable(line, index);
                  if scalar2.VarType = svtInteger then
                  begin
                    if (scalar2.Name <> scalar.Name) or (scalar2.Player <> scalar.Player) then
                    begin
                      AProg.Add(TTransferIntegerInstruction.Create(scalar2.Player, scalar2.Name, itCopyToAccumulator));
                      AProg.Add(TTransferIntegerInstruction.Create(scalar.Player, scalar.Name, itCopyFromAccumulator));
                    end;
                  end else
                    raise exception.Create('Expecting integer value');
                end;

              end;
            end;
          svtSwitch:
            begin
              intVal := ParseRandom(line, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                if intVal = 2 then
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svRandomize))
                else
                  raise exception.Create('Boolean can have only 2 values');
              end else
              begin
                conds := ExpectConditions(line,index);
                if (conds.Count = 1) and (conds[0] is TSwitchCondition) and
                   (TSwitchCondition(conds[0]).Switch = scalar.Index) then
                begin
                  //a = Not a
                  if not TSwitchCondition(conds[0]).Value then
                    AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svToggle));
                  conds[0].Free;
                  conds.Free;
                end else
                if (conds.Count = 1) and (conds[0] is TAlwaysCondition) then
                begin
                  //a = True
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svSet));
                  conds[0].Free;
                  conds.Free;
                end else
                if (conds.Count = 1) and (conds[0] is TNeverCondition) then
                begin
                  //a = False
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svClear));
                  conds[0].Free;
                  conds.Free;
                end else
                begin
                  AProg.Add(TIfInstruction.Create(conds));
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svSet));
                  AProg.Add(TElseInstruction.Create);
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Index, svClear));
                  AProg.Add(TEndIfInstruction.Create);
                end;
              end;
            end;
          else raise exception.Create('Unhandled case');
          end;
        end else
        If TryToken(line,index,'+') or TryToken(line,index,'-') then
        begin
          assignOp := line[index-1];
          if TryToken(line,index,'=') then
          begin
            done := true;
            if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
            if scalar.VarType = svtInteger then
            begin
              if TryInteger(line,index,intVal) then
              begin
                CheckEndOfLine;
                if assignOp='+' then
                  AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simAdd, intVal))
                else
                  AProg.Add(TSetIntegerInstruction.Create(scalar.Player, scalar.Name, simSubtract, intVal));
              end else
              begin
                scalar2 := TryScalarVariable(line,index);
                if scalar2.VarType = svtInteger then
                begin
                  AProg.Add(TTransferIntegerInstruction.Create(scalar2.Player, scalar2.Name, itCopyToAccumulator));
                  AProg.Add(TTransferIntegerInstruction.Create(scalar.Player, scalar.Name, itAddFromAccumulator));
                end else
                raise exception.Create('Expecting integer value');
              end;

            end
            else raise Exception.Create('Integer variables only can be incremented/decremented');
          end;
        end;

      end;

      idxArr := IntArrayIndexOf(line[0]);
      if idxArr <> -1 then
      begin

        index := 1;
        If TryToken(line,index,'+') or TryToken(line,index,'-') then
          assignOp := line[index-1] else assignOp := '';

        if TryToken(line,index,'=') then
        begin
          done := true;
          if IntArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');

          ints := ParseIntArray(line,index);
          if length(ints) <> IntArrays[idxArr].Size then
            raise exception.Create('Array size mismatch');
          CheckEndOfLine;
          if assignOp = '+' then sim := simAdd
          else if assignOp = '-' then sim := simSubtract
          else sim := simSetTo;
          for i := 0 to high(ints) do
            AProg.Add(TSetIntegerInstruction.Create(IntToPlayer(i+1), IntArrays[idxArr].UnitType, sim, ints[i]));
        end;
      end;

      if not done then
      begin
        index := 0;
        pl := TryParsePlayer(line,index);
        if pl <> plNone then
        begin
          done := true;

          ExpectToken(line,index,'.');
          if index >= line.Count then
            raise exception.Create('Expecting action but end of line found');

          if TryToken(line,index,'CreateUnit') then
          begin
            ExpectToken(line,index,'(');
            if TryInteger(line,index,intVal) then
            begin
              if intVal <= 0 then raise exception.Create('Quantity must be at least 1');
              ExpectToken(line,index,',');
            end else
              intVal := 1;

            unitType := ExpectString(line,index);
            ExpectToken(line,index,',');
            locStr := ExpectString(line,index);
            if TryToken(line,index,',') then
            begin
              propIndex := TryUnitPropVar(line,index);
              if propIndex = -1 then
                raise exception.Create('Unit properties expected');
            end else
              propIndex := -1;
            ExpectToken(line,index,')');
            CheckEndOfLine;

            AProg.Add(TCreateUnitInstruction.Create(pl, intVal, unitType, locStr, propIndex));

          end else
          if TryToken(line,index,'KillUnit') or TryToken(line,index,'RemoveUnit') then
          begin
            boolVal:= upcase(line[index-1][1])='K';
            ExpectToken(line,index,'(');
            intVal := ParseOptionalQuantity;
            unitType := ExpectString(line,index);

            if TryToken(line,index,')') then
              locStr:= ''
            else
            begin
              ExpectToken(line,index,',');
              locStr := ExpectString(line,index);
              ExpectToken(line,index,')');
            end;
            CheckEndOfLine;
            AProg.Add(TKillUnitInstruction.Create(pl, intVal, unitType, locStr, True));
          end else
          if TryToken(line,index,'GiveUnit') then
          begin
            ExpectToken(line,index,'(');
            intVal := ParseOptionalQuantity;
            unitType := ExpectString(line,index);
            ExpectToken(line,index,',');
            locStr := ExpectString(line,index);
            ExpectToken(line,index,',');
            destPl := TryParsePlayer(line,index);
            if destPl = plNone then raise Exception.Create('Expecting player');
            ExpectToken(line,index,')');
            CheckEndOfLine;

            AProg.Add(TGiveUnitInstruction.Create(pl, intVal, unitType, locStr, destPl));
          end else
          if TryToken(line,index,'GetUnit') then
          begin
            ExpectToken(line,index,'(');
            intVal := ParseOptionalQuantity;
            unitType := ExpectString(line,index);
            ExpectToken(line,index,',');
            locStr := ExpectString(line,index);
            ExpectToken(line,index,')');
            ExpectToken(line,index,'.');

            if TryToken(line,index,'Properties') then
            begin
              ExpectToken(line,index,'=');
              if not TryUnitProperties(line,index,props) then
              begin
                if (index < line.Count) and IsValidVariableName(line[index]) then
                begin
                  propIndex:= UnitPropIndexOf(line[index]);
                  if propIndex = -1 then
                    raise exception.Create('Expecting unit properties');

                  inc(index);
                  props := UnitPropVars[propIndex].Value;
                end else
                raise exception.Create('Expecting unit properties');
              end;
              CheckEndOfLine;

              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supLife, props.Life));
              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supShield, props.Shield));
              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supEnergy, props.Energy));
              if CompareTexT(unitType, 'Any unit')=0 then
                AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supResource, props.Resource));
              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supHangarCount, props.HangarCount));

              if intVal = -1 then
                AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supInvincible, integer(props.Invincible)));

            end else
            if TryToken(line,index,'Location') then
            begin
               ExpectToken(line,index,'=');
               destLocStr := ExpectString(line,index);
               CheckEndOfLine;

               AProg.Add(TTeleportUnitInstruction.Create(pl, intVal, unitType, locStr, destLocStr));
            end else
            if TryToken(line,index,'Teleport') then
            begin
               ExpectToken(line,index,'(');
               destLocStr := ExpectString(line,index);
               ExpectToken(line,index,')');
               CheckEndOfLine;

               AProg.Add(TTeleportUnitInstruction.Create(pl, intVal, unitType, locStr, destLocStr));
            end else
            if TryToken(line,index,'MoveOrder') or TryToken(line,index,'PatrolOrder') or TryToken(line,index,'AttackOrder') then
            begin
              orderStr := line[index-1];
              orderStr := lowercase(copy(orderStr,1,length(orderStr)-5));
               if intVal <> -1 then
                 raise exception.Create('Cannot specify quantity for an order (use All quantity instead)');
               ExpectToken(line,index,'(');
               destLocStr := ExpectString(line,index);
               ExpectToken(line,index,')');
               CheckEndOfLine;

               AProg.Add(TOrderUnitInstruction.Create(pl, unitType, locStr, destLocStr, orderStr));
            end else
            if TryToken(line,index,'Kill') then
            begin
              if TryToken(line,index,'(') then ExpectToken(line,index,'(');
              CheckEndOfLine;
              AProg.Add(TKillUnitInstruction.Create(pl, intVal, unitType, locStr, true));
            end else
            if TryToken(line,index,'Remove') then
            begin
              CheckEndOfLine;
              AProg.Add(TKillUnitInstruction.Create(pl, intVal, unitType, locStr, false));
            end else
            if TryToken(line,index,'Give') then
            begin
              ExpectToken(line,index,'(');
              destPl:= TryParsePlayer(line,index);
              if destPl = plNone then raise exception.Create('Expecting player');
              ExpectToken(line,index,')');
              CheckEndOfLine;

              AProg.Add(TGiveUnitInstruction.Create(pl, intVal, unitType, locStr, destPl));
            end else
            if TryToken(line,index,'ToggleInvincibility') then
            begin
              if TryToken(line,index,'(') then ExpectToken(line,index,'(');
              CheckEndOfLine;

              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supInvincible, -1));
            end else
            if TryToken(line,index,'ToggleDoodadState') then
            begin
              if TryToken(line,index,'(') then ExpectToken(line,index,'(');
              CheckEndOfLine;

              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, supDoodadState, -1));
            end else
            begin
              if TryToken(line,index,'Life') then prop := supLife else
              if TryToken(line,index,'Shield') then prop := supShield else
              if TryToken(line,index,'Energy') then prop := supEnergy else
              if TryToken(line,index,'Resource') then prop := supResource else
              if TryToken(line,index,'HangarCount') then prop := supHangarCount else
              if TryToken(line,index,'Invincible') then prop := supInvincible else
              if TryToken(line,index,'DoodadState') then prop := supDoodadState else
                raise exception.Create('Expecting property name');
              ExpectToken(line,index,'=');
              if prop in[supInvincible,supDoodadState] then
              begin
                if not TryBoolean(line,index,boolVal) then
                  raise exception.Create('Expecting boolean');
                propVal := integer(boolVal);
              end
              else propVal:= ExpectInteger(line,index);
              CheckEndOfLine;

              AProg.Add(TSetUnitPropertyInstruction.Create(pl, intVal, unitType, locStr, prop, propVal));
            end;

          end else
            raise exception.Create('Expecting action but "' + line[index] + '" found');
        end;
      end;

      if not done then
      begin
        index := 0;
        name := '';
        while (index < line.Count) and IsValidVariableName(line[index]) do
        begin
          if index > 0 then name += ' ';
          name += line[index];
          inc(index);
        end;

        if (index = line.Count) or
          ((index < line.Count) and (line[index] = '(')) then
        begin
          if CompareText(name,'Print')=0 then
          begin
            ExpectToken(line,index,'(');
            players := ParsePlayers(line, index);
            ExpectToken(line,index,',');
            if index >= line.Count then raise exception.Create('Message expected');
            msg := ExpectString(line,index);
            ExpectToken(line,index,')');
            AProg.Add(TDisplayTextMessageInstruction.Create(true, msg, players));
          end else
          begin
            params := TStringList.Create;
            if TryToken(line,index,'(') then
            begin
              while not TryToken(line,index,')') do
              begin
                if params.Count > 0 then ExpectToken(line,index,',');
                if TryToken(line,index,',') then
                  raise exception.Create('Empty parameters not allowed');
                params.Add(line[index]);
                inc(index);
              end;
            end;
            CheckEndOfLine;

            if CompareText(name,'Wait')=0 then
            begin
              if params.Count <> 1 then
                raise exception.Create('Procedure takes only one parameter');
              AProg.Add(TWaitInstruction.Create(StrToInt(params[0])));
            end
            else
              AProg.Add(TCallInstruction.Create(name, params));
            params.Free;
          end;
        end else
        if scalar.VarType = svtNone then
          raise exception.Create('Unknown instruction')
        else
          raise exception.Create('Expecting assignment');
      end;
    end;
  finally
    line.Free;
  end;
end;

function ReadProg(AFilename: string): boolean;
var t: TextFile;
  s: string;
  lineNumber, errorCount: integer;
  inSub, inEvent: integer;
begin
  HyperTriggers := false;
  BoolVarCount:= 0;
  IntVarCount:= 0;
  IntArrayCount:= 0;
  TempBoolCount:= 0;
  StringCount := 0;
  MainProg.Clear;

  PredefineIntArray('Ore','Ore');
  PredefineIntArray('Minerals','Ore');
  PredefineIntArray('Gas','Gas');
  PredefineIntArray('OreAndGas','Ore And Gas');
  PredefineIntArray('UnitScore','Units Score');
  PredefineIntArray('UnitsScore','Units Score');
  PredefineIntArray('BuildingScore','Buildings Score');
  PredefineIntArray('BuildingsScore','Buildings Score');
  PredefineIntArray('KillScore','Kills Score');
  PredefineIntArray('KillsScore','Kills Score');
  PredefineIntArray('RazingScore','Razings Score');
  PredefineIntArray('RazingsScore','Razings Score');
  PredefineIntArray('CustomScore','Custom Score');

  AssignFile(t, AFilename);
  Reset(t);
  lineNumber:= 1;
  errorCount := 0;
  inSub := -1;
  inEvent := -1;
  while not Eof(t) and (errorCount < 3) do
  begin
    ReadLn(t, s);
    s := Trim(s);
    try
      if compareText(copy(s, 1, 4), 'Dim ') = 0 then ProcessDim(s, false)
      else if compareText(copy(s, 1, 6), 'Const ') = 0 then ProcessDim(s, true)
      else if CompareText(copy(s, 1, 4), 'Sub ') = 0 then
      begin
        if (inSub<>-1) or (inEvent <> -1) then
          raise exception.Create('Nested procedures not allowed');
        inSub := ProcessSub(s)
      end
      else if CompareText(s, 'End Sub') = 0 then
      begin
        if inSub= -1 then
          raise Exception.create('Not in a procedure');
        Procedures[inSub].Instructions.Add(TReturnInstruction.Create);
        inSub := -1;
      end
      else if CompareText(copy(s, 1, 5), 'When ') = 0 then
      begin
        if (inSub<>-1) or (inEvent <> -1) then
          raise exception.Create('Nested events not allowed');
        inEvent := ProcessEvent(s)
      end
      else if CompareText(s, 'End When') = 0 then
      begin
        if inEvent= -1 then
          raise Exception.create('Not in an event');
        inEvent := -1;
      end
      else
      begin
        if inSub<>-1 then
          ParseInstruction(s, Procedures[inSub].Instructions)
        else if inEvent<>-1 then
          ParseInstruction(s, Events[inEvent].Instructions)
        else
          ParseInstruction(s, MainProg);
      end;
    except
      on ex:Exception do
      begin
        writeln('Line ', lineNumber, ': ', ex.Message);
        errorCount += 1;
      end;
    end;
    inc(lineNumber);
  end;
  CloseFile(t);
  result := errorCount = 0;
end;

var i,j: integer;

initialization

  MainProg := TInstructionList.Create;

finalization

  for i := 0 to MainProg.Count-1 do
    MainProg[i].Free;

  MainProg.Free;

  for i := 0 to ProcedureCount-1 do
    with Procedures[i] do
    begin
      for j := 0 to Instructions.Count-1 do
        Instructions[j].Free;
      Instructions.Free;
      Calls.Free;
    end;

  for i := 0 to EventCount-1 do
    with Events[i] do
    begin
      for j := 0 to Instructions.Count-1 do
        Instructions[j].Free;
      Instructions.Free;
      Conditions.FreeAll;
    end;

end.

