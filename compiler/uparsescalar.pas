unit uparsescalar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes;

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

function TryScalarVariable(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false): TScalarVariable;
function ExpectUnitType({%H-}AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
function IsUnitType(AName: string): boolean;
function TryIdentifier(ALine: TStringList; var AIndex: integer; out AIdentifier: string; AcceptsReservedWords: boolean): boolean;
function TryInteger(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
function TryIntegerVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryIntegerConstantVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryIntegerArray(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false): integer;
function TryPredefinedIntegerArray(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function ExpectInteger(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryBoolean(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
function TryBooleanVariable(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false): integer;
function TryBooleanArray(AScope: integer; ALine: TStringList; var AIndex: integer; AConstantOnly: boolean = false): integer;
function TryStringVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryStringArray(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TrySoundVariable(AScope: integer; ALine: TStringList; var AIndex: integer): integer;

type
  TExpectBooleanConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer): boolean;
  TExpectIntegerConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer; AAcceptNegative: boolean): integer;
  TTryIntegerConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
  TExpectStringConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;

var
  ExpectBooleanConstant: TExpectBooleanConstantFunc;
  ExpectIntegerConstant: TExpectIntegerConstantFunc;
  TryIntegerConstant: TTryIntegerConstantFunc;
  ExpectStringConstant: TExpectStringConstantFunc;

implementation

uses uparsevb, uvariables;

function ExpectUnitType(AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
var
  u: TStarcraftUnit;
  ident: string;
  i: Integer;
begin
  if TryToken(ALine,AIndex,'Unit') then
    ExpectToken(ALine,AIndex,'.');

  for i := 0 to StarcraftUnitPrefixes.Count-1 do
    if TryToken(ALine, AIndex, StarcraftUnitPrefixes[i]) then
    begin
      ExpectToken(ALine,AIndex,'.');
      for u := low(TStarcraftUnit) to suFactories do
        if (StarcraftUnitSplitIdentifier[u].Prefix = StarcraftUnitPrefixes[i]) and
          TryToken(ALine, AIndex, StarcraftUnitSplitIdentifier[u].PartialName) then
            exit(u);
    end;

  for u := low(TStarcraftUnit) to suFactories do
    if (StarcraftUnitSplitIdentifier[u].Prefix = '') and
      TryToken(ALine, AIndex, StarcraftUnitIdentifier[u]) then
        exit(u);

  if not TryIdentifier(ALine, AIndex, ident, false) then
    raise exception.Create('Expecting identifier')
    else raise exception.Create('Unknown unit type "' + ident + '"');
end;

function IsUnitType(AName: string): boolean;
var
  u: TStarcraftUnit;
  posDot, i: integer;
  partialName: String;
begin
  for i := 0 to StarcraftUnitPrefixes.Count-1 do
    if CompareText(StarcraftUnitPrefixes[i],AName)=0 then exit(true);

  for u := low(TStarcraftUnit) to suFactories do
  begin
    posDot := pos('.',StarcraftUnitIdentifier[u]);
    if posDot <> 0 then
    begin
      partialName := copy(StarcraftUnitIdentifier[u], posDot+1,
                      length(StarcraftUnitIdentifier[u]) - posDot);
      if CompareText(partialName,AName)=0 then exit(true);
    end;
  end;
  exit(false);
end;

function TryIdentifier(ALine: TStringList; var AIndex: integer; out AIdentifier: string; AcceptsReservedWords: boolean): boolean;
begin
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) and
   (AcceptsReservedWords or not IsReservedWord(ALine[AIndex])) then
  begin
    AIdentifier:= ALine[AIndex];
    inc(AIndex);
    exit(true);
  end else
  begin
    AIdentifier:= '';
    exit(false);
  end;
end;

function TryInteger(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
var errPos, idxVar: integer;
  s, ident, strValue: String;
begin
  if TryToken(ALine,AIndex,'Asc') then
  begin
    ExpectToken(ALine,AIndex,'(');
    s := ExpectStringConstant(AScope, ALine,AIndex);
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
    strValue := ExpectStringConstant(AScope, ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    AValue := length(strValue);
    exit(true);
  end;

  idxVar := TryIntegerConstantVariable(AScope, ALine, AIndex);
  if idxVar<>-1 then
  begin
    AValue := IntVars[idxVar].Value;
    exit(true);
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
  var AIndex: integer): integer;
var
  i: Integer;
begin
  for i := 0 to IntVarCount-1 do
    if ((IntVars[i].Scope = AScope) or (IntVars[i].Scope = GlobalScope)) and
      (IntVars[i].IntArray = -1) and
      TryToken(ALine, AIndex, IntVars[i].Name) then exit(i);
  result := -1;
end;

function TryIntegerConstantVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  for i := 0 to IntVarCount-1 do
    if ((IntVars[i].Scope = AScope) or (IntVars[i].Scope = GlobalScope)) and
      IntVars[i].Constant and
      TryToken(ALine, AIndex, IntVars[i].Name) then exit(i);
  result := -1;
end;

function TryIntegerArray(AScope: integer; ALine: TStringList;
  var AIndex: integer; AConstantOnly: boolean = false): integer;
var
  i: Integer;
begin
  for i := 0 to IntArrayCount-1 do
    if ((IntArrays[i].Scope = AScope) or (IntArrays[i].Scope = GlobalScope)) and
      (not AConstantOnly or IntArrays[i].Constant) and
      TryToken(ALine, AIndex, IntArrays[i].Name) then exit(i);
  result := -1;
end;

function TryPredefinedIntegerArray(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  for i := 0 to IntArrayCount-1 do
    if ((IntArrays[i].Scope = AScope) or (IntArrays[i].Scope = GlobalScope)) and
       IntArrays[i].Predefined and TryToken(ALine, AIndex, IntArrays[i].Name) then exit(i);
  result := -1;
end;

function ExpectInteger(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
begin
  result := 0;
  if not TryInteger(AScope, ALine,AIndex,result) then
  begin
    if AIndex > ALine.Count then
      raise exception.Create('Integer expected but end of line found') else
      raise exception.Create('Integer expected but "' + ALine[AIndex] + '" found');
  end;
end;

function TryBoolean(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
var
  idxVar,idx, arrIndex: Integer;
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
      idxVar := BoolVarIndexOf(AScope, ALine[AIndex]);
      if (idxVar<>-1) and BoolVars[idxVar].Constant then
      begin
        AValue := (BoolVars[idxVar].Value = svSet);
        inc(AIndex);
        exit(true);
      end;
      idxVar := BoolArrayIndexOf(AScope, ALine[AIndex]);
      if (idxVar <> -1) and BoolArrays[idxVar].Constant then
      begin
        idx := AIndex+1;
        if TryToken(ALine,idx,'(') then
        begin
          arrIndex := ExpectIntegerConstant(AScope, ALine,idx,true);
          if (arrIndex < 1) or (arrIndex > BoolArrays[idxVar].Size) then
            raise exception.Create('Index out of bounds');
          AValue := BoolArrays[idxVar].Values[arrIndex-1] = svSet;
          ExpectToken(ALine,idx,')');
          AIndex := idx;
          exit(true);
        end;
      end;
    end;
    exit(false);
  end;
end;

function TryBooleanVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer; AConstantOnly: boolean = false): integer;
var
  i: Integer;
begin
  for i := 0 to BoolVarCount-1 do
    if ((BoolVars[i].Scope = AScope) or (BoolVars[i].Scope = GlobalScope)) and
      (BoolVars[i].BoolArray = -1) and (not AConstantOnly or BoolVars[i].Constant) and
      TryToken(ALine, AIndex, BoolVars[i].Name) then exit(i);
  result := -1;
end;

function TryBooleanArray(AScope: integer; ALine: TStringList;
  var AIndex: integer; AConstantOnly: boolean = false): integer;
var
  i: Integer;
begin
  for i := 0 to BoolArrayCount-1 do
    if ((BoolArrays[i].Scope = AScope) or (BoolArrays[i].Scope = GlobalScope)) and
      (not AConstantOnly or BoolArrays[i].Constant) and
      TryToken(ALine, AIndex, BoolArrays[i].Name) then exit(i);
  if TryToken(ALine,AIndex,'Present') then
    exit(GetPlayerPresentArray);
  result := -1;
end;

function TryStringVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  for i := 0 to StringCount-1 do
    if ((StringVars[i].Scope = AScope) or (StringVars[i].Scope = GlobalScope)) and
      TryToken(ALine, AIndex, StringVars[i].Name) then exit(i);
  result := -1;
end;

function TryStringArray(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
var
  i: Integer;
begin
  for i := 0 to StringArrayCount-1 do
    if ((StringArrays[i].Scope = AScope) or (StringArrays[i].Scope = GlobalScope)) and
      TryToken(ALine, AIndex, StringArrays[i].Name) then exit(i);
  result := -1;
end;

function TrySoundVariable(AScope: integer; ALine: TStringList;
  var AIndex: integer): integer;
var
  i: Integer;
begin
  for i := 0 to SoundCount-1 do
    if ((SoundVars[i].Scope = AScope) or (SoundVars[i].Scope = GlobalScope)) and
      TryToken(ALine, AIndex, SoundVars[i].Name) then exit(i);
  result := -1;
end;

function TryScalarVariable(AScope: integer; ALine: TStringList; var AIndex: integer;
  AConstantOnly: boolean): TScalarVariable;
var varIdx, arrayIndex, idx: integer;
  pl: TPlayer;
  unitType: TStarcraftUnit;
begin
  result.VarType := svtNone;
  if AConstantOnly then
    varIdx := TryIntegerConstantVariable(AScope, ALine, AIndex)
    else varIdx := TryIntegerVariable(AScope, ALine, AIndex);
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

  varIdx := TryBooleanVariable(AScope, ALine, AIndex, AConstantOnly);
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

  if not AConstantOnly and TryToken(ALine,AIndex,'Switch') then
  begin
    ExpectToken(ALine,AIndex,'(');
    result.VarType := svtSwitch;
    result.Player := plNone;
    result.UnitType := suSwitch;
    result.Switch := ExpectIntegerConstant(AScope,ALine,AIndex,true);
    if (result.Switch < 1) or (result.Switch > 256) then raise exception.Create('Switch index out of bounds (1 to 256)');
    result.Constant:= False;
    result.ReadOnly := False;
    result.BoolValue:= false;
    result.IntValue := 0;
    ExpectToken(ALine,AIndex,')');
    exit;
  end;

  if not AConstantOnly and TryToken(ALine,AIndex,'Present') then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      arrayIndex := ExpectIntegerConstant(AScope, ALine,AIndex,true);
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

  varIdx := TryBooleanArray(AScope, ALine, AIndex, AConstantOnly);
  if varIdx <> -1 then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      arrayIndex := ExpectIntegerConstant(AScope, ALine,AIndex,true);
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

  varIdx := TryIntegerArray(AScope, ALine, AIndex, AConstantOnly);
  if varIdx <> -1 then
  begin
    if TryToken(ALine,AIndex,'(') then
    begin
      if TryToken(ALine,AIndex,'Me') then
      begin
        if IntArrays[varIdx].Constant then
          raise exception.Create('Constant arrays cannot be indexed by "Me"');
        result.IntValue:= 0;
        result.UnitType := IntArrays[varIdx].UnitType;
        result.Player := plCurrentPlayer;
      end
      else
      begin
        arrayIndex := ExpectIntegerConstant(AScope, ALine, AIndex,true);
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

  idx := AIndex;
  if AConstantOnly then exit;
  pl := TryParsePlayer(AScope, ALine,idx);
  if pl <> plNone then
  begin
    if TryToken(ALine,idx,'.') then
    begin
      if TryToken(ALine,idx,'DeathCount') then
      begin
        if TryToken(ALine,idx,'(') then
        begin
          unitType := ExpectUnitType(AScope, ALine,idx);
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
      end else
      if idx < ALine.Count then
      begin
        varIdx := TryPredefinedIntegerArray(AScope, ALine, idx);
        if (varIdx <> -1) and IntArrays[varIdx].Predefined then
        begin
          result.VarType := svtInteger;
          if (pl in[plPlayer1..plPlayer8]) and
            (ord(pl) - ord(plPlayer1) + 1 > IntArrays[varIdx].Size) then
              raise exception.Create('This player is not included in this array. Index is out of bounds');
          result.Player := pl;
          result.UnitType := IntArrays[varIdx].UnitType;
          result.Switch := -1;
          result.Constant:= False;
          result.ReadOnly := result.Constant;
          result.IntValue:= 0;
          result.BoolValue:= false;

          AIndex := idx;
          exit;
        end else
          dec(varIdx);
      end;

    end;
  end;
end;

end.

