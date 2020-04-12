unit uparsescalar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, uscope, uinstructions;

type
  TScalarVariableType = (svtNone, svtInteger, svtSwitch);
  TScalarVariable = record
    VarType: TScalarVariableType;
    Constant, ReadOnly: boolean;
    UnitType: TStarcraftUnit;
    Player: TPlayer;
    Switch: integer;
    IntValue: integer;
    BoolValue: boolean;
  end;

function TryScalarVariable(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): TScalarVariable;
function TryUnitType(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AUnit: TStarcraftUnit): boolean;
function ExpectUnitTypeIdentifier(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
function TryUnitTypeConst(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AWiderScope: boolean = true): TStarcraftUnit;
function ExpectUnitType(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
function TryInteger(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
function TryIntegerVariable(AScope: integer; ALine: TStringList; var AIndex: integer; ACheckWiderScope: boolean = true): integer;
function TryIntegerConstantVariable(AScope: integer; ALine: TStringList; var AIndex: integer; ACheckWiderScope: boolean = true): integer;
function TryIntegerArray(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): integer;
function TryPredefinedIntegerArray(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function ExpectInteger(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function ExpectConstantIndex(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AAcceptNegative: boolean): integer;
function ParseRandom(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryBoolean(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
function TryBooleanVariable(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): integer;
function TryBooleanArray(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): integer;
function TryStringVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryStringArray(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TrySoundVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryUnitPropertiesVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;

type
  TExpectBooleanConstantFunc = function(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): boolean;
  TExpectIntegerConstantFunc = function(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AAcceptNegative: boolean): integer;
  TTryIntegerConstantFunc = function(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
  TExpectStringConstantFunc = function(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;
  TExpectMultiStringConstantFunc = function(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): TMultistring;
  TParseProcedureParameterValuesFunc = function(AThreads: TPlayers; AScope: integer; funcName: string; ALine: TStringList; var AIndex: integer): ArrayOfParameterValue;

var
  ExpectBooleanConstant: TExpectBooleanConstantFunc;
  ExpectIntegerConstant: TExpectIntegerConstantFunc;
  TryIntegerConstant: TTryIntegerConstantFunc;
  ExpectStringConstant: TExpectStringConstantFunc;
  ExpectMultiStringConstant: TExpectMultiStringConstantFunc;
  ParseProcedureParameterValues: TParseProcedureParameterValuesFunc;

implementation

uses uparsevb, uvariables, uprocedures, umapinfo;

function TryUnitType(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AUnit: TStarcraftUnit): boolean;
var
  u: TStarcraftUnit;
  i: Integer;
  wantUnit: Boolean;
  customName: string;
begin
  if TryToken(ALine,AIndex,'CustomUnit') then
  begin
    ExpectToken(ALine, AIndex, '(');
    AUnit := suNone;
    customName := ExpectStringConstant(AThreads, AScope, ALine, AIndex, true);
    ExpectToken(ALine, AIndex, ')');
    for u := low(TStarcraftUnit) to suFactories do
      if MapInfo.CustomUnitName[u] = customName then
      begin
        AUnit := u;
        exit(true);
      end;
    raise exception.Create('Custom unit name not found');
  end;

  if TryToken(ALine,AIndex,'Unit') then
  begin
    ExpectToken(ALine,AIndex,'.');
    wantUnit := true;
  end else
    wantUnit := false;

  for i := 0 to StarcraftUnitPrefixes.Count-1 do
    if TryToken(ALine, AIndex, StarcraftUnitPrefixes[i]) then
    begin
      ExpectToken(ALine,AIndex,'.');
      for u := low(TStarcraftUnit) to suFactories do
        if (StarcraftUnitSplitIdentifier[u].Prefix = StarcraftUnitPrefixes[i]) and
          TryToken(ALine, AIndex, StarcraftUnitSplitIdentifier[u].PartialName) then
          begin
            AUnit := u;
            exit(true);
          end;
      raise exception.Create('Excepting unit name');
    end;

  for u := low(TStarcraftUnit) to suFactories do
    if (StarcraftUnitSplitIdentifier[u].Prefix = '') and
      TryToken(ALine, AIndex, StarcraftUnitIdentifier[u]) then
        begin
          AUnit := u;
          exit(true);
        end;

  if wantUnit then raise exception.Create('Excepting unit category');
  AUnit := suNone;
  exit(false);
end;

function ExpectUnitTypeIdentifier(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
var
  ident: string;
begin
  if not TryUnitType(AThreads, AScope, ALine, AIndex, result) then
  begin
    if not TryIdentifier(ALine, AIndex, ident, false) then
      raise exception.Create('Expecting identifier')
      else raise exception.Create('Unknown unit type "' + ident + '"');
  end;
end;

function TryUnitTypeConst(AThreads: TPlayers; AScope: integer;
  ALine: TStringList; var AIndex: integer; AWiderScope: boolean
  ): TStarcraftUnit;
var
  i, classIdx: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to UnitConstCount-1 do
      if (UnitConsts[i].Scope = AScope) and
        TryToken(ALine, AIndex, UnitConsts[i].Name) then
        exit(UnitConsts[i].Value);
    if not AWiderScope then break;
    AScope := GetWiderScope(AScope);
  end;

  if AWiderScope then
  begin
    classIdx := TryClassName(ALine, AIndex);
    if classIdx <> -1 then
    begin
      ExpectToken(ALine, AIndex, '.');
      result := TryUnitTypeConst(AThreads, ClassDefinitions[classIdx].InnerScope, ALine, AIndex, false);
      exit;
    end;
  end;

  result := suNone;
end;

function ExpectUnitType(AThreads: TPlayers; AScope: integer; ALine: TStringList;
  var AIndex: integer): TStarcraftUnit;
begin
  result := TryUnitTypeConst(AThreads, AScope, ALine, AIndex, true);
  if result = suNone then
    result := ExpectUnitTypeIdentifier(AThreads, AScope, ALine, AIndex);
end;

function TryInteger(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
var errPos, idxVar, oldIndex, bitDepth: integer;
  s, ident, strValue, typeStr: String;
  boolValue: boolean;
  pl: TPlayer;
begin
  if TryToken(ALine,AIndex,'Asc') then
  begin
    ExpectToken(ALine,AIndex,'(');
    s := ExpectStringConstant(AThreads, AScope, ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    if s = '' then
      AValue := 0
    else
      AValue := ord(s[1]);
    exit(true)
  end;

  if TryToken(ALine,AIndex,'LBound') then
  begin
    ExpectToken(ALine,AIndex,'(');
    if TryToken(ALine,AIndex,'Present') or
       (TryBooleanArray(AScope, ALine, AIndex) <> -1) or
       (TryIntegerArray(AScope, ALine, AIndex) <> -1) or
       (TryStringArray(AScope, ALine, AIndex) <> -1) then
    begin
      AValue := 1;
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end;
    if not TryIdentifier(ALine,AIndex,ident, false) then
      raise exception.Create('Identifier expected')
      else raise exception.Create('Unknown array "' + ident + '"');
  end;

  if TryToken(ALine,AIndex,'UBound') then
  begin
    ExpectToken(ALine,AIndex,'(');
    if TryToken(ALine,AIndex,'Present') then
    begin
      AValue := MaxTriggerPlayers;
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end;
    idxVar := TryBooleanArray(AScope, ALine, AIndex);
    if idxVar <> -1 then
    begin
      AValue := BoolArrays[idxVar].Size;
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end;
    idxVar := TryIntegerArray(AScope, ALine, AIndex);
    if idxVar <> -1 then
    begin
      AValue := IntArrays[idxVar].Size;
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end;
    idxVar :=TryStringArray(AScope, ALine, AIndex);
    if idxVar <> -1 then
    begin
      AValue := StringArrays[idxVar].Size;
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end;
    if not TryIdentifier(ALine,AIndex,ident, false) then
      raise exception.Create('Identifier expected')
      else raise exception.Create('Unknown array "' + ident + '"');
  end;

  if TryToken(ALine,AIndex,'Len') then
  begin
    ExpectToken(ALine,AIndex,'(');
    strValue := ExpectStringConstant(AThreads, AScope, ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    AValue := length(strValue);
    exit(true);
  end;

  if TryToken(ALine,AIndex,'CByte') or TryToken(ALine,AIndex,'CUInt8') or
     TryToken(ALine,AIndex,'CUShort') or TryToken(ALine,AIndex,'CUInt16') or
     TryToken(ALine,AIndex,'CUInt24') or TryToken(ALine,AIndex,'CUInt') then
  begin
    typeStr := ALine[AIndex-1];
    delete(typeStr, 1, 1);
    bitDepth := GetBitCountOfType(typeStr);
    if bitDepth = 0 then bitDepth := 24;
    ExpectToken(ALine, AIndex, '(');
    if not PeekToken(ALine,AIndex,'Me') then
      pl := TryParsePlayer(AThreads, AScope, ALine, AIndex)
      else pl := plNone;
    if pl <> plNone then
    begin
      if pl in [plPlayer1..plPlayer12] then
        AValue := ord(pl) - ord(plPlayer1)+1
        else raise exception.Create('This player does not have a number');
    end else
    if TryBoolean(AThreads, AScope, ALine, AIndex, boolValue) then
    begin
      if boolValue then
        AValue := 1
        else AValue := 0;
    end else
    begin
      s := ExpectStringConstant(AThreads, AScope, ALine, AIndex, true);
      if copy(s,1,2)='&H' then s := '$'+copy(s,3,length(s)-2);
      val(s, AValue, errPos);
      if errPos > 0 then
        raise exception.Create('Not a valid integer');
    end;
    if (AValue < 0) or (AValue > 1 shl bitDepth) then
      raise exception.create('Integer out of range');
    ExpectToken(ALine, AIndex, ')');
    exit(true);
  end;

  idxVar := TryIntegerConstantVariable(AScope, ALine, AIndex);
  if idxVar<>-1 then
  begin
    AValue := IntVars[idxVar].Value;
    exit(true);
  end;

  oldIndex := AIndex;
  idxVar := TryUnitPropertiesVariable(AScope, ALine, AIndex);
  if idxVar <> -1 then
  begin
    if TryToken(ALine, AIndex, '.') then
    begin
      if TryToken(ALine, AIndex, 'Life') then
      begin
        AValue := UnitPropVars[idxVar].Value.Life;
        exit(true);
      end else
      if TryToken(ALine, AIndex, 'Shield') then
      begin
        AValue := UnitPropVars[idxVar].Value.Shield;
        exit(true);
      end else
      if TryToken(ALine, AIndex, 'Energy') then
      begin
        AValue := UnitPropVars[idxVar].Value.Energy;
        exit(true);
      end else
      if TryToken(ALine, AIndex, 'Resource') then
      begin
        AValue := UnitPropVars[idxVar].Value.Resource;
        exit(true);
      end else
      if TryToken(ALine, AIndex, 'HangarCount') then
      begin
        AValue := UnitPropVars[idxVar].Value.HangarCount;
        exit(true);
      end;
    end;
    AIndex := oldIndex;
  end;

  idxVar := TrySoundVariable(AScope, ALine, AIndex);
  if idxVar <> -1 then
  begin
    if TryToken(ALine, AIndex, '.') then
    begin
      if TryToken(ALine, AIndex, 'Duration') then
      begin
        AValue := SoundVars[idxVar].DurationMs;
        exit(true);
      end;
    end;
    AIndex := oldIndex;
  end;

  if AIndex < ALine.Count then
  begin
    s := ALine[AIndex];
    if copy(s,1,2)='&H' then s := '$'+copy(s,3,length(s)-2);
    val(s, AValue, errPos);
    if errPos = 0 then
    begin
      inc(AIndex);
      exit(true);
    end;
  end;

  AValue := 0;
  exit(false);
end;

function TryIntegerVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer; ACheckWiderScope: boolean = true): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to IntVarCount-1 do
      if (IntVars[i].Scope = AScope) and
       (pos('.', IntVars[i].Name) = 0) and (pos('(', IntVars[i].Name) = 0)
       and TryToken(ALine, AIndex, IntVars[i].Name) then exit(i);
    if not ACheckWiderScope then break;
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryIntegerConstantVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer; ACheckWiderScope: boolean = true): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to IntVarCount-1 do
      if (IntVars[i].Scope = AScope) and
       (pos('.', IntVars[i].Name) = 0) and (pos('(', IntVars[i].Name) = 0)
        and IntVars[i].Constant and TryToken(ALine, AIndex, IntVars[i].Name) then exit(i);
    if not ACheckWiderScope then break;
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryIntegerArray(AScope: integer; ALine: TStringList;
  var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to IntArrayCount-1 do
      if (IntArrays[i].Scope = AScope) and (pos('.', IntArrays[i].Name) = 0) and
        (not AConstantOnly or IntArrays[i].Constant) and
        TryToken(ALine, AIndex, IntArrays[i].Name) then exit(i);
    if not ACheckWiderScope then break;
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryPredefinedIntegerArray(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to IntArrayCount-1 do
      if (IntArrays[i].Scope = AScope) and (pos('.', IntArrays[i].Name) = 0) and
         IntArrays[i].Predefined and TryToken(ALine, AIndex, IntArrays[i].Name) then exit(i);
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function ExpectInteger(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): integer;
begin
  result := 0;
  if not TryInteger(AThreads, AScope, ALine,AIndex,result) then
  begin
    if AIndex > ALine.Count then
      raise exception.Create('Integer expected but end of line found') else
      raise exception.Create('Integer expected but "' + ALine[AIndex] + '" found');
  end;
end;

function ExpectConstantIndex(AThreads: TPlayers; AScope: integer;
  ALine: TStringList; var AIndex: integer; AAcceptNegative: boolean): integer;
var
  uniquePlayer, pl: TPlayer;
  oldIndex, idxClass: Integer;
begin
  oldIndex := AIndex;
  for pl := plPlayer1 to plPlayer12 do
    if TryToken(ALine,AIndex,PlayerIdentifiers[pl]) then
    begin
      if not PeekToken(ALine,AIndex,'.') then
        exit(ord(pl)-ord(plPlayer1)+1)
      else
      begin
        AIndex := oldIndex;
        break;
      end;
    end;

  idxClass := TryClassName(ALine,AIndex,true);
  if idxClass <> -1 then
  begin
    if not PeekToken(ALine,AIndex,'.') then
      exit(ord(GetUniquePlayer(ClassDefinitions[idxClass].Threads))-ord(plPlayer1)+1)
    else
      AIndex := oldIndex;
  end;
  uniquePlayer := GetUniquePlayer(AThreads);
  if (uniquePlayer in [plPlayer1..plPlayer12]) and
     TryToken(ALine,AIndex,'Me') then
    result := ord(uniquePlayer)-ord(plPlayer1)+1
    else result := ExpectIntegerConstant(AThreads, AScope, ALine,AIndex,AAcceptNegative);
end;

function ParseRandom(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): integer;
begin
  if (AIndex < ALine.Count) and (CompareText(ALine[AIndex], 'Rnd') = 0) then
  begin
    inc(AIndex);
    if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
    if TryToken(ALine,AIndex,'*') then
    begin
      if not TryInteger(AThreads, AScope, ALine, AIndex, result) then
        raise exception.Create('Expecting integer constant');

      if result < 2 then
        raise Exception.Create('Value must be greated or equal to 2');
    end else
      exit(2);
  end else
    exit(-1);
end;

function TryBoolean(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
var
  idxVar, arrIndex, oldIndex, intVal: Integer;
  strVal: string;
begin
  AValue := false;
  if TryToken(ALine,AIndex,'False') then exit(true)
  else if TryToken(ALine,AIndex,'True') then
  begin
    AValue := true;
    exit(true)
  end else
  if TryToken(ALine,AIndex,'CBool') then
  begin
    ExpectToken(ALine,AIndex,'(');
    if TryIntegerConstant(AThreads, AScope, ALine, AIndex, intVal) then
    begin
      AValue := (intVal <> 0);
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end else
    begin
      strVal := ExpectStringConstant(AThreads, AScope, ALine, AIndex, true);
      if compareText(strVal,'False')=0 then AValue := false
      else if compareText(strVal,'True')=0 then AValue := true
      else raise exception.Create('Value cannot be converted to boolean');
      ExpectToken(ALine,AIndex,')');
      exit(true);
    end;
  end else
  begin
    oldIndex := AIndex;

    idxVar := TryUnitPropertiesVariable(AScope, ALine, AIndex);
    if idxVar <> -1 then
    begin
      if TryToken(ALine, AIndex, '.') then
      begin
        if TryToken(ALine, AIndex, 'Invincible') then
        begin
          AValue := UnitPropVars[idxVar].Value.Invincible;
          exit(true);
        end else
        if TryToken(ALine, AIndex, 'Burrowed') then
        begin
          AValue := UnitPropVars[idxVar].Value.Burrowed;
          exit(true);
        end else
        if TryToken(ALine, AIndex, 'Lifted') then
        begin
          AValue := UnitPropVars[idxVar].Value.Lifted;
          exit(true);
        end else
        if TryToken(ALine, AIndex, 'Hallucinated') then
        begin
          AValue := UnitPropVars[idxVar].Value.Hallucinated;
          exit(true);
        end else
        if TryToken(ALine, AIndex, 'Cloaked') then
        begin
          AValue := UnitPropVars[idxVar].Value.Cloaked;
          exit(true);
        end;
      end;
      AIndex := oldIndex;
    end;

    idxVar := TryBooleanVariable(AScope, ALine, AIndex, true);
    if idxVar <> -1 then
    begin
      AValue := (BoolVars[idxVar].Value = svSet);
      exit(true);
    end;
    idxVar := TryBooleanArray(AScope, ALine, AIndex, true);
    if idxVar <> -1 then
    begin
      if TryToken(ALine,AIndex,'(') then
      begin
        arrIndex := ExpectConstantIndex(AThreads, AScope, ALine,AIndex,true);
        if (arrIndex < 1) or (arrIndex > BoolArrays[idxVar].Size) then
          raise exception.Create('Index out of bounds');
        AValue := BoolArrays[idxVar].Values[arrIndex-1] = svSet;
        ExpectToken(ALine,AIndex,')');
        exit(true);
      end;
      AIndex := oldIndex;
    end;
    exit(false);
  end;
end;

function TryBooleanVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to BoolVarCount-1 do
      if (BoolVars[i].Scope = AScope) and
        (BoolVars[i].BoolArray = -1) and (not AConstantOnly or BoolVars[i].Constant) and
        TryToken(ALine, AIndex, BoolVars[i].Name) then exit(i);
    if not ACheckWiderScope then break;
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryBooleanArray(AScope: integer; ALine: TStringList;
  var AIndex: integer; AConstantOnly: boolean = false; ACheckWiderScope: boolean = true): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to BoolArrayCount-1 do
      if (BoolArrays[i].Scope = AScope) and
        (not AConstantOnly or BoolArrays[i].Constant) and
        TryToken(ALine, AIndex, BoolArrays[i].Name) then exit(i);
    if not ACheckWiderScope then break;
    AScope := GetWiderScope(AScope);
  end;
  if not AConstantOnly and ACheckWiderScope and TryToken(ALine,AIndex,'Present') then
    exit(GetPlayerPresentArray);
  result := -1;
end;

function TryStringVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to StringCount-1 do
      if (StringVars[i].Scope = AScope) and
        TryToken(ALine, AIndex, StringVars[i].Name) then exit(i);
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryStringArray(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to StringArrayCount-1 do
      if (StringArrays[i].Scope = AScope) and
        TryToken(ALine, AIndex, StringArrays[i].Name) then exit(i);
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TrySoundVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  while AScope <> -1 do
  begin
    for i := 0 to SoundCount-1 do
      if (SoundVars[i].Scope = AScope) and
        TryToken(ALine, AIndex, SoundVars[i].Name) then exit(i);
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryUnitPropertiesVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  idxProp: Integer;
begin
  while AScope <> -1 do
  begin
    for idxProp := 0 to UnitPropCount-1 do
      if (UnitPropVars[idxProp].Scope = AScope)
         and TryToken(ALine, AIndex, UnitPropVars[idxProp].Name) then
        exit(idxProp);
    AScope := GetWiderScope(AScope);
  end;
  result := -1;
end;

function TryScalarVariable(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer;
  AConstantOnly: boolean; ACheckWiderScope: boolean): TScalarVariable;
var
  idx: integer;

var
  varIdx, arrayIndex, classIdx: integer;
  pl: TPlayer;
  unitType: TStarcraftUnit;
begin
  result.VarType := svtNone;
  if AConstantOnly then
    varIdx := TryIntegerConstantVariable(AScope, ALine, AIndex, ACheckWiderScope)
    else varIdx := TryIntegerVariable(AScope, ALine, AIndex, ACheckWiderScope);
  if varIdx <> -1 then
  begin
    result.VarType := svtInteger;
    result.Player := IntVars[varIdx].Player;
    result.UnitType := IntVars[varIdx].UnitType;
    result.Switch := -1;
    result.Constant:= IntVars[varIdx].Constant;
    result.ReadOnly := result.Constant;
    result.IntValue:= IntVars[varIdx].Value;
    result.BoolValue:= IntVars[varIdx].Value<>0;
    exit;
  end;

  varIdx := TryBooleanVariable(AScope, ALine, AIndex, AConstantOnly, ACheckWiderScope);
  if varIdx <> -1 then
  begin
    result.VarType := svtSwitch;
    result.Player := plNone;
    result.UnitType := suSwitch;
    result.Switch := BoolVars[varIdx].Switch;
    result.Constant:= BoolVars[varIdx].Constant;
    result.ReadOnly := result.Constant;
    result.BoolValue:= BoolVars[varIdx].Value = svSet;
    result.IntValue := integer(result.BoolValue);
    exit;
  end;

  if not AConstantOnly and ACheckWiderScope and TryToken(ALine,AIndex,'Switch') then
  begin
    ExpectToken(ALine,AIndex,'(');
    result.VarType := svtSwitch;
    result.Player := plNone;
    result.UnitType := suSwitch;
    result.Switch := ExpectConstantIndex(AThreads,AScope,ALine,AIndex,true);
    if (result.Switch < 1) or (result.Switch > 256) then raise exception.Create('Switch index out of bounds (1 to 256)');
    result.Constant:= False;
    result.ReadOnly := False;
    result.BoolValue:= false;
    result.IntValue := 0;
    ExpectToken(ALine,AIndex,')');
    exit;
  end;

  if not AConstantOnly and ACheckWiderScope and TryToken(ALine,AIndex,'Present') then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      arrayIndex := ExpectConstantIndex(AThreads, AScope, ALine,AIndex,true);
      if (arrayIndex < 1) or (arrayIndex > MaxTriggerPlayers) then
        raise exception.Create('Array index out of bounds');
      ExpectToken(ALine, AIndex, ')');

      result.VarType := svtSwitch;
      result.Player := plNone;
      result.UnitType := suSwitch;
      varIdx := GetPlayerPresenceBoolVar(TPlayer(ord(plPlayer1)+arrayIndex-1));
      result.Switch := BoolVars[varIdx].Switch;
      result.Constant:= false;
      result.ReadOnly := true;
      result.BoolValue:= false;
      result.IntValue := 0;
      exit;
    end else
      Dec(AIndex);
  end;

  varIdx := TryBooleanArray(AScope, ALine, AIndex, AConstantOnly, ACheckWiderScope);
  if varIdx <> -1 then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      arrayIndex := ExpectConstantIndex(AThreads, AScope, ALine,AIndex,true);
      if (arrayIndex < 1) or (arrayIndex > BoolArrays[varIdx].Size) then
        raise exception.Create('Array index out of bounds');
      ExpectToken(ALine, AIndex, ')');

      result.VarType := svtSwitch;
      result.Player := plNone;
      result.UnitType := suSwitch;
      result.Switch := BoolVars[BoolArrays[varIdx].Vars[arrayIndex-1]].Switch;
      result.Constant:= BoolArrays[varIdx].Constant;
      result.ReadOnly := result.Constant;
      result.BoolValue:= BoolArrays[varIdx].Values[arrayIndex-1] = svSet;
      result.IntValue := integer(result.BoolValue);
      exit;
    end else
      Dec(AIndex);
  end;

  varIdx := TryIntegerArray(AScope, ALine, AIndex, AConstantOnly, ACheckWiderScope);
  if varIdx <> -1 then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      if TryToken(ALine,AIndex,'Me') and not IntArrays[varIdx].Constant then
      begin
        for pl := plPlayer1 to plPlayer12 do
          if pl in AThreads then
          begin
            if ord(pl)-ord(plPlayer1)+1 > IntArrays[varIdx].Size then
              raise exception.Create('Array index out of bounds');
          end;
        if (plAllPlayers in AThreads) and (MaxTriggerPlayers > IntArrays[varIdx].Size) then
          raise exception.Create('Array index out of bounds');
        result.IntValue:= 0;
        result.UnitType := IntArrays[varIdx].UnitType;
        result.Player := plCurrentPlayer;
      end
      else
      begin
        arrayIndex := ExpectConstantIndex(AThreads, AScope, ALine, AIndex,true);
        if (arrayIndex < 1) or (arrayIndex > IntArrays[varIdx].Size) then
          raise exception.Create('Array index out of bounds');
        pl := IntToPlayer(arrayIndex);
        result.IntValue:= IntArrays[varIdx].Values[arrayIndex-1];
        with IntVars[IntArrays[varIdx].Vars[arrayIndex-1]] do
        begin
          result.Player := Player;
          result.UnitType := UnitType;
        end;
      end;
      ExpectToken(ALine, AIndex, ')');
      result.VarType := svtInteger;
      result.Switch := -1;
      result.Constant:= IntArrays[varIdx].Constant;
      result.ReadOnly := result.Constant;
      result.BoolValue:= result.IntValue<>0;
      exit;
    end else
      Dec(AIndex);
  end;

  if ACheckWiderScope then
  begin
    idx := AIndex;
    classIdx := TryClassName(ALine, idx);
    if classIdx <> -1 then
    begin
      if TryToken(ALine,idx,'.') then
      begin
        result := TryScalarVariable(AThreads, ClassDefinitions[classIdx].InnerScope, ALine, idx, AConstantOnly, false);
        if result.VarType <> svtNone then
        begin
          if not IsUniquePlayer(ClassDefinitions[classIdx].Threads) then
            raise exception.Create('Ambiguous player');
          AIndex := idx;
          exit;
        end;
      end;
    end;
  end;

  if AConstantOnly or not ACheckWiderScope then exit;
  idx := AIndex;
  pl := TryParsePlayer(AThreads, AScope, ALine,idx);
  if pl <> plNone then
  begin
    if TryToken(ALine,idx,'.') then
    begin
      if TryToken(ALine,idx,'DeathCount') then
      begin
        if TryToken(ALine,idx,'(') then
        begin
          unitType := ExpectUnitType(AThreads, AScope, ALine,idx);
          ExpectToken(ALine,idx,')');
        end else
          unitType := suAnyUnit;

        result.VarType := svtInteger;
        result.Player := pl;
        result.UnitType := unitType;
        result.Switch := -1;
        result.Constant:= False;
        result.ReadOnly := result.Constant;
        result.IntValue:= 0;
        result.BoolValue:= false;

        AIndex := idx;
        exit;
      end;
    end;
  end;
end;

end.
