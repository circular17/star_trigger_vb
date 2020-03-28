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

function TryScalarVariable(AScope: integer; ALine: TStringList; var AIndex: integer): TScalarVariable;
function ExpectUnitType({%H-}AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
function IsUnitType(AName: string): boolean;
function TryIdentifier(ALine: TStringList; var AIndex: integer; out AIdentifier: string; AcceptsReservedWords: boolean): boolean;
function TryInteger(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
function ExpectInteger(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryBoolean(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;

type
  TExpectIntegerConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
  TTryIntegerConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
  TExpectStringConstantFunc = function(AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;

var
  ExpectIntegerConstant: TExpectIntegerConstantFunc;
  TryIntegerConstant: TTryIntegerConstantFunc;
  ExpectStringConstant: TExpectStringConstantFunc;

implementation

uses uparsevb, uvariables;

function ExpectUnitType(AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
var
  u: TStarcraftUnit;
  ident: string;
begin
  if TryToken(ALine,AIndex,'Unit') then
    ExpectToken(ALine,AIndex,'.');
  if not TryIdentifier(ALine, AIndex, ident, false) then raise exception.Create('Expecting identifier');
  for u := low(TStarcraftUnit) to suFactories do
    if CompareText(StarcraftUnitIdentifier[u],ident)=0 then exit(u);
  raise exception.Create('Unknown unit type');
end;

function IsUnitType(AName: string): boolean;
var
  u: TStarcraftUnit;
begin
  for u := low(TStarcraftUnit) to suFactories do
    if CompareText(StarcraftUnitIdentifier[u],AName)=0 then exit(true);
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
  AValue := 0;
  if AIndex < ALine.Count then
  begin
    s := ALine[AIndex];
    if copy(s,1,2)='&H' then s := '$'+copy(s,3,length(s)-2);
    val(s, AValue, errPos);
    if errPos = 0 then
    begin
      inc(AIndex);
      exit(true);
    end else
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
        if not TryIdentifier(ALine,AIndex,ident, false) then
          raise exception.Create('Identifier expected');
        ExpectToken(ALine,AIndex,')');
        idxVar := IntArrayIndexOf(AScope, ident);
        if idxVar <> -1 then
        begin
          AValue := 1;
          exit(true);
        end;
        idxVar := BoolArrayIndexOf(AScope, ident);
        if idxVar <> -1 then
        begin
          AValue := 1;
          exit(true);
        end;
        idxVar := StringArrayIndexOf(AScope, ident);
        if idxVar <> -1 then
        begin
          AValue := 1;
          exit(true);
        end;

      end;

      if TryToken(ALine,AIndex,'UBound') then
      begin
        ExpectToken(ALine,AIndex,'(');
        if not TryIdentifier(ALine,AIndex,ident, false) then
          raise exception.Create('Identifier expected');
        ExpectToken(ALine,AIndex,')');
        idxVar := IntArrayIndexOf(AScope, ident);
        if idxVar <> -1 then
        begin
          AValue := IntArrays[idxVar].Size;
          exit(true);
        end;
        idxVar := BoolArrayIndexOf(AScope, ident);
        if idxVar <> -1 then
        begin
          AValue := BoolArrays[idxVar].Size;
          exit(true);
        end;
        idxVar := StringArrayIndexOf(AScope, ident);
        if idxVar <> -1 then
        begin
          AValue := StringArrays[idxVar].Size;
          exit(true);
        end;
      end;

      if TryToken(ALine,AIndex,'Len') then
      begin
        ExpectToken(ALine,AIndex,'(');
        strValue := ExpectStringConstant(AScope, ALine,AIndex);
        ExpectToken(ALine,AIndex,')');
        AValue := length(strValue);
        exit(true);
      end;

      idxVar := IntVarIndexOf(AScope, ALine[AIndex]);
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
          arrIndex := ExpectIntegerConstant(AScope, ALine,idx);
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

function TryScalarVariable(AScope: integer; ALine: TStringList; var AIndex: integer): TScalarVariable;
var varIdx, arrayIndex, idx: integer;
  pl: TPlayer;
  unitType: TStarcraftUnit;
begin
  result.VarType := svtNone;
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
  begin
    varIdx := IntVarIndexOf(AScope, ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
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

    varIdx := BoolVarIndexOf(AScope, ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
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

    if CompareText(ALine[AIndex],'Switch')=0 then
    begin
      inc(AIndex);
      ExpectToken(ALine,AIndex,'(');
      result.VarType := svtSwitch;
      result.Player := plNone;
      result.UnitType := suSwitch;
      result.Switch := ExpectIntegerConstant(AScope,ALine,AIndex);
      if (result.Switch < 1) or (result.Switch > 256) then raise exception.Create('Switch index out of bounds (1 to 256)');
      result.Constant:= False;
      result.ReadOnly := False;
      result.BoolValue:= false;
      result.IntValue := 0;
      ExpectToken(ALine,AIndex,')');
      exit;
    end;

    varIdx := BoolArrayIndexOf(AScope, ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
      if TryToken(ALine,AIndex,'(') then
      begin
        arrayIndex := ExpectIntegerConstant(AScope, ALine,AIndex);
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
    end else
    if (CompareText(ALine[AIndex],'Present')=0) then
    begin
      Inc(AIndex);
      if TryToken(ALine,AIndex,'(') then
      begin
        arrayIndex := ExpectIntegerConstant(AScope, ALine,AIndex);
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

    varIdx := IntArrayIndexOf(AScope, ALine[AIndex]);
    if varIdx <> -1 then
    begin
      Inc(AIndex);
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
          arrayIndex := ExpectIntegerConstant(AScope, ALine, AIndex);
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
          varIdx := IntArrayIndexOf(AScope, ALine[idx]);
          if (varIdx <> -1) and IntArrays[varIdx].Predefined then
          begin
            inc(idx);

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
          end;
        end;

      end;
    end;
  end;
end;

end.

