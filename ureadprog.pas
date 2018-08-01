unit ureadprog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl;

type
  TIntegerList = specialize TFPGList<Integer>;

function ReadProg(AFilename: string): boolean;

const
  MaxTempBools = 32;
  MaxArraySize = 8;

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
    Predefined: boolean;
    Name: string;
    Size: integer;
    UnitType: string;
    Values: array[1..MaxArraySize] of integer;
  end;
  IntArrayCount: integer;
  CurIntArrayUnitNameIndex: integer;

function CreateIntArray(AName: string; ASize: integer; AValues: array of integer): integer;
function IntArrayIndexOf(AName: string): integer;

var
  IntVars: array of record
    Name: string;
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Randomize: boolean;
  end;
  IntVarCount: integer;
  CurIntVarPlayer: TPlayer;
  CurIntVarUnitNameIndex: integer;

function CreateIntVar(AName: string; AValue: integer; ARandomize: boolean = false): integer;
function IntVarIndexOf(AName: string): integer;

var
  BoolVars: array of record
    Name: string;
    Switch: integer;
    Value: TSwitchValue;
  end;
  BoolVarCount: integer;
  CurBoolVarSwitch: integer;

  TempBools: array[0..MaxTempBools-1] of integer;
  TempBoolCount: integer;

function CreateBoolVar(AName: string; AValue: TSwitchValue): integer;
function BoolVarIndexOf(AName: string): integer;
procedure NeedTempBools(AQuantity: integer);

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

function ParseRandom(ALine: TStringList; var AIndex: integer): integer;
function IsPowerOf2(ANumber: integer): boolean;
function GetExponentOf2(AValue: integer): integer;
function RemoveQuotes(AQuotedText: string): string;

implementation

type
  ArrayOfInteger = array of integer;
  TConditionOperator = (coNone, coEqual, coGreaterThan, coLowerThan, coGreaterThanOrEqual, coLowerThanOrEqual, coNotEqual);

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

function CreateIntArray(AName: string; ASize: integer; AValues: array of integer): integer;
var
  i: Integer;
begin
  if (IntVarIndexOf(AName)<>-1) or (BoolVarIndexOf(AName)<>-1) or (IntArrayIndexOf(AName)<>-1) or
    (ProcedureIndexOf(AName,0)<>-1) then
    raise Exception.Create('Name already in use');

  if length(AValues)>ASize then
    raise exception.Create('Too many elements in array values');

  if IntArrayCount = 0 then
    CurIntArrayUnitNameIndex:= high(NonKillableUnits)+1;

  if (CurIntArrayUnitNameIndex = 1) or
    ((IntVarCount > 0) and (CurIntArrayUnitNameIndex = CurIntVarUnitNameIndex+1)) then
    raise exception.Create('Too many integer variables');

  dec(CurIntArrayUnitNameIndex);

  if IntArrayCount >= length(IntArrays) then
    setlength(IntArrays, IntArrayCount*2+4);

  result := IntArrayCount;
  inc(IntArrayCount);

  with IntArrays[result] do
  begin
    Predefined := false;
    Name := AName;
    Size:= ASize;
    UnitType := NonKillableUnits[CurIntArrayUnitNameIndex];
    for i := 0 to high(AValues) do
      Values[i+1] := AValues[i];
    for i := high(AValues)+1 to MaxArraySize-1 do
      Values[i+1] := 0;
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
    Predefined := true;
    Name := AName;
    Size:= MaxArraySize;
    UnitType := AUnitType;
    for i := 1 to MaxArraySize do
      Values[i] := 0;
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

function CreateIntVar(AName: string; AValue: integer; ARandomize: boolean): integer;
begin
  if (IntVarIndexOf(AName)<>-1) or (BoolVarIndexOf(AName)<>-1) or (IntArrayIndexOf(AName)<>-1) or
    (ProcedureIndexOf(AName,0)<>-1) then
    raise Exception.Create('Name already in use');

  if IntVarCount = 0 then
  begin
    CurIntVarPlayer:= IntToPlayer(MaxArraySize);
    CurIntVarUnitNameIndex:= 1;
  end else
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
    Name := AName;
    Player := CurIntVarPlayer;
    UnitType := NonKillableUnits[CurIntVarUnitNameIndex];
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

function CreateBoolVar(AName: string; AValue: TSwitchValue): integer;
begin
  if (IntVarIndexOf(AName)<>-1) or (BoolVarIndexOf(AName)<>-1) or (IntArrayIndexOf(AName)<>-1) or
    (ProcedureIndexOf(AName,0)<>-1) then
    raise Exception.Create('Name already in use');

  if BoolVarCount = 0 then
  begin
    CurBoolVarSwitch:= 256;
  end;

  if CurBoolVarSwitch = 0 then
    raise Exception.Create('Too many boolean variables');

  if BoolVarCount >= length(BoolVars) then
    setlength(BoolVars, BoolVarCount*2+4);

  result := BoolVarCount;
  inc(BoolVarCount);

  with BoolVars[result] do
  begin
    Name := AName;
    Switch:= CurBoolVarSwitch;
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

function TryInteger(ALine: TStringList; var AIndex: integer; var AValue: integer): boolean;
var errPos: integer;
begin
  if AIndex < ALine.Count then
  begin
    val(ALine[AIndex], AValue, errPos);
    if errPos = 0 then
    begin
      inc(AIndex);
      exit(true);
    end else
      exit(false);
  end
  else exit(false);
end;

function ExpectInteger(ALine: TStringList; var AIndex: integer): integer;
begin
  result := 0;
  if not TryInteger(ALine,AIndex,result) then
    raise exception.Create('Integer expected');
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

type
  TScalarVariableType = (svtNone, svtInteger, svtSwitch);
  TScalarVariable = record
    VarType: TScalarVariableType;
    Name: string;
    Player: TPlayer;
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
    end else
    begin
      varIdx := BoolVarIndexOf(ALine[AIndex]);
      if varIdx <> -1 then
      begin
        inc(AIndex);
        result.VarType := svtSwitch;
        result.Player := plNone;
        result.Name := BoolVars[varIdx].Name;
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
          end else
          begin
            Dec(AIndex);
          end;
        end;
      end;
    end;
  end;
end;

function ExpectCondition(ALine: TStringList; var AIndex: integer): TCondition;
var
  varIdx, intVal: Integer;
  op: TConditionOperator;
  boolNot: Boolean;
  scalar: TScalarVariable;
begin
  if TryToken(ALine,AIndex,'True') then
    result := TAlwaysCondition.Create else
  begin
    boolNot := TryToken(ALine,AIndex,'Not');

    scalar := TryScalarVariable(ALine,AIndex);
    if scalar.VarType = svtNone then
      raise exception.Create('Expecting variable name');

    case scalar.VarType of
    svtInteger:
    begin
      if boolNot then raise exception.Create('Not operator is invalid with integers');
      op := TryConditionOperator(ALine,AIndex);
      if op = coNone then
        raise exception.Create('Comparison expected')
      else
      begin
        intVal := ExpectInteger(ALine,AIndex);
        case op of
        coEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, dcmExactly, intVal);
        coGreaterThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, dcmAtLeast, intVal);
        coLowerThanOrEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, dcmAtMost, intVal);
        coGreaterThan: if intVal = maxLongint then
                          result := TNeverCondition.Create
                       else
                          result := TIntegerCondition.Create(scalar.Player, scalar.Name, dcmAtLeast, intVal+1);
        coLowerThan: if intVal = 0 then
                          result := TNeverCondition.Create
                       else
                          result := TIntegerCondition.Create(scalar.Player, scalar.Name, dcmAtMost, intVal-1);
        coNotEqual: result := TIntegerCondition.Create(scalar.Player, scalar.Name, dcmNotEqualTo, intVal-1);
        else
          raise exception.Create('Unhandled case');
        end;
      end;
    end;
    svtSwitch:
      result := TSwitchCondition.Create(BoolVars[varIdx].Name, not boolNot)
    else
        raise exception.Create('Unhandled case');
    end;
  end;
end;


function CreateProcedure(AName: string; AParamCount: integer): integer;
begin
  if ProcedureIndexOf(AName, AParamCount)<>-1 then
    raise exception.Create('Procedure already declared with this signature');

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

function ParsePlayer(ALine: TStringList; var AIndex: integer): TPlayer;
var s: string;
  i: integer;
  pl: TPlayer;
begin
  s := '';
  i := 0;
  while AIndex < ALine.Count do
  begin
    if TryInteger(ALine, AIndex, i) then
    begin
      if s <> '' then s += ' ';
      s += inttostr(i);
    end else
    if IsValidVariableName(ALine[AIndex]) then
    begin
      if s <> '' then s += ' ';
      s += ALine[AIndex];
      inc(AIndex);
    end else
      break;
  end;
  for pl := succ(plNone) to high(TPlayer) do
    if CompareText(PlayerToStr(pl),s)=0 then exit(pl);

  raise exception.Create('Expecting player but "' + s + '" found');
end;

function ParsePlayers(ALine: TStringList; var AIndex: integer): TPlayers;
begin
  result := [];
  if TryToken(ALine, AIndex, '{') then
  begin
    repeat
      result += [ParsePlayer(ALine,AIndex)];

      if TryToken(ALine, aIndex, '}') then break
      else ExpectToken(ALine,AIndex,',');
    until false;
  end else
    result := [ParsePlayer(ALine,AIndex)];
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

procedure ProcessDim(ADeclaration: string);
var line: TStringList;
  index: Integer;
  varName, varType: String;
  varValue, arraySize: integer;
  isArray: boolean;
  rndVal: integer;
  arrValues: ArrayOfInteger;

  procedure ExpectArraySize;
  begin
    if not TryInteger(line,index,arraySize) then
      raise exception.Create('Expecting array size or "]"');
    if (arraySize < 1) or (arraySize > MaxArraySize) then
      raise Exception.Create('Array size can go from 1 to ' + inttostr(MaxArraySize));
  end;

begin
  line := ParseLine(ADeclaration);
  try
    if (line.Count = 0) or (CompareText(line[0], 'Dim') <> 0) then
     raise Exception.Create('This is not a Dim instruction');

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
        else raise Exception.Create('Unknown type : ' + line[index]);

        if not isArray and TryToken(line,index,'(') then
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
          CreateIntArray(varName, arraySize, arrValues);
        end else
        begin
          if (CompareText(line[index], 'False') = 0) or (CompareText(line[index], 'True') = 0) then
          begin
            if varType = '?' then varType := 'Boolean';
            if varType <> 'Boolean' then
              raise Exception.Create('Value is not of expected type');
            CreateBoolVar(varName, BoolToSwitch[StrToBool(line[index])]);
            inc(index);
          end
          else
          begin
            rndVal := ParseRandom(line, index);
            if rndVal > 0 then
            begin
              if varType = 'Boolean' then
              begin
                if rndVal <> 2 then raise Exception.Create('Boolean can have only 2 values');
                CreateBoolVar(varName, svRandomize);
              end
              else
                CreateIntVar(varName, rndVal, true);

            end else
            begin
              if varType = 'Boolean' then
                raise Exception.Create('Expecting boolean value');

              varValue := ExpectInteger(line,index);

              CreateIntVar(varName, varValue);
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
            CreateIntArray(varName, arraySize, []);
          end;
        end else
        begin
          if varType = 'Boolean' then
            CreateBoolVar(varName, svClear)
          else
            CreateIntVar(varName, 0);
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
  index, intVal: integer;
  params: TStringList;
  name, assignOp, msg: String;
  cond: TCondition;
  done: boolean;
  scalar: TScalarVariable;
  players: TPlayers;

  procedure CheckEndOfLine;
  begin
    if index <> line.Count then
      raise exception.Create('Expecting end of line');
  end;

begin
  line := ParseLine(AText);
  try
    if (line.Count = 0) or ((line.Count = 1) and (line[0] = '')) then exit;

    index := 0;
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
      exit;
    end else
    if TryToken(line,index,'While') then
    begin
      cond := ExpectCondition(line,index);
      AProg.Add(TWhileInstruction.Create(cond));
      CheckEndOfLine;
    end else
    if TryToken(line,index,'If') then
    begin
      cond := ExpectCondition(line,index);
      AProg.Add(TIfInstruction.Create(cond));
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
          case scalar.VarType of
          svtInteger:
            begin
              intVal := ParseRandom(line, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                AProg.Add(TSetIntInstruction.Create(scalar.Player, scalar.Name, simRandomize, intVal));
              end else
              begin
                if TryInteger(line,index,intVal) then
                begin
                  CheckEndOfLine;
                  AProg.Add(TSetIntInstruction.Create(scalar.Player, scalar.Name, simSetTo, intVal));
                end else
                  raise exception.Create('Expecting integer value');
              end;
            end;
          svtSwitch:
            begin
              intVal := ParseRandom(line, index);
              if intVal > 0 then
              begin
                CheckEndOfLine;
                if intVal = 2 then
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Name, svRandomize))
                else
                  raise exception.Create('Boolean can have only 2 values');
              end else
              begin
                if (CompareText(line[index],'False') = 0) or (CompareText(line[index],'True') = 0) then
                begin
                  inc(index);
                  CheckEndOfLine;
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Name, BoolToSwitch[StrToBool(line[index-1])]));
                end else
                if CompareText(line[index],'Not') = 0 then
                begin
                  inc(index);
                  if (CompareText(line[index],'False') = 0) or (CompareText(line[index],'True') = 0) then
                  begin
                    inc(index);
                    CheckEndOfLine;
                    AProg.Add(TSetSwitchInstruction.Create(scalar.Name, BoolToSwitch[not StrToBool(line[index-1])]));
                  end else
                  if (index = line.Count-1) and (line[index] = line[0]) then
                  begin
                    inc(index);
                    CheckEndOfLine;
                    AProg.Add(TSetSwitchInstruction.Create(scalar.Name, svToggle));
                  end else
                    raise exception.Create('Not handled');
                end else
                  raise exception.Create('Expecting boolean value');
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
            if scalar.VarType = svtInteger then
            begin
              if TryInteger(line,index,intVal) then
              begin
                CheckEndOfLine;
                if assignOp='+' then
                  AProg.Add(TSetIntInstruction.Create(scalar.Player, scalar.Name, simAdd, intVal))
                else
                  AProg.Add(TSetIntInstruction.Create(scalar.Player, scalar.Name, simSubtract, intVal));
              end else
                raise exception.Create('Expecting integer value');
            end
            else raise Exception.Create('Integer variables only can be incremented/decremented');
          end;
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
            msg := line[index];
            inc(index);
            ExpectToken(line,index,')');
            AProg.Add(TDisplayTextMessageInstruction.Create(true, RemoveQuotes(msg), players));
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
  inSub: integer;
begin
  BoolVarCount:= 0;
  IntVarCount:= 0;
  IntArrayCount:= 0;
  TempBoolCount:= 0;
  MainProg.Clear;

  PredefineIntArray('Ore','Ore');
  PredefineIntArray('Minerals','Ore');
  PredefineIntArray('Gas','Gas');
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
  while not Eof(t) and (errorCount < 3) do
  begin
    ReadLn(t, s);
    s := Trim(s);
    try
      if compareText(copy(s, 1, 4), 'Dim ') = 0 then ProcessDim(s)
      else if CompareText(copy(s, 1, 4), 'Sub ') = 0 then
      begin
        if inSub<>-1 then
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
      else
      begin
        if inSub<>-1 then
          ParseInstruction(s, Procedures[inSub].Instructions)
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

end.

