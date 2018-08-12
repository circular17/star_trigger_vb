unit uparsevb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes;

type
  ArrayOfInteger = array of integer;
  TConditionOperator = (coNone, coEqual, coGreaterThan, coLowerThan, coGreaterThanOrEqual, coLowerThanOrEqual, coNotEqual);

const
  NotConditionOperator : array[TConditionOperator] of TConditionOperator = (coNone, coNotEqual, coLowerThanOrEqual, coGreaterThanOrEqual, coLowerThan, coGreaterThan, coEqual);

function RemoveQuotes(AQuotedText: string): string;
procedure CheckReservedWord(AText: string);
function IsReservedWord(AText: string): boolean;
function ParseLine(ALine: string): TStringList;
function IsValidVariableName(AText: string): boolean;
function TryToken(ALine: TStringList; var AIndex: integer; AToken: string): boolean;
procedure ExpectToken(ALine: TStringList; var AIndex: integer; AToken: string);
function TryConditionOperator(ALine: TStringList; var AIndex: integer): TConditionOperator;
function ParseRandom(ALine: TStringList; var AIndex: integer): integer;
function TryParsePlayer(ALine: TStringList; var AIndex: integer): TPlayer;
function ExpectPlayers(ALine: TStringList; var AIndex: integer): TPlayers;
function GetBitCountOfType(AName: string): integer;
function IsIntegerType(AName: string): boolean;
function BitCountNeededFor(AValue: integer): integer;

const
  ImplementedReservedWords: array[1..41] of string =
    ('Dim','As','Const','Sub','When','End','If','EndIf', 'Then','Else','Not','And','Or','While','Option','Return',
     'On','Off','Hyper','Boolean','Byte','UInt8','UShort','UInt16','UInt24','String','True','False',
     'Do','Len','Chr','Asc','Exit','Function','LBound','UBound','Me','Rnd','New','Min','Max');

  NotImplementedReservedWords: array[1..19] of string =
     ('For','To','Step','Next','Loop','Until','ElseIf','Select','Case',  //reserved and planned to implement
     'SByte','Short','Int16','Int24','UInteger','Integer','Xor','Date','ReDim','Preserve'); //reserved but not planned to implement

implementation

uses uarithmetic;

function RemoveQuotes(AQuotedText: string): string;
begin
  if (length(AQuotedText)<2) or (AQuotedText[1]<>'"') or (AQuotedText[length(AQuotedText)]<>'"') then
    raise exception.Create('Quotes not found');

  result := StringReplace(copy(AQuotedText,2,length(AQuotedText)-2), '""', '"', [rfReplaceAll]);
end;

procedure CheckReservedWord(AText: string);
var
  pl: TPlayer;
begin
  if IsReservedWord(AText) then raise exception.Create('"' + AText + '" is a word reserved');
  for pl := low(TPlayer) to high(TPlayer) do
    if CompareText(PlayerIdentifiers[pl], AText)=0 then raise exception.Create('"' + PlayerIdentifiers[pl] + '" is a player identifier');
end;

function IsReservedWord(AText: string): boolean;
var
  i: Integer;
begin
  for i := low(ImplementedReservedWords) to high(ImplementedReservedWords) do
    if CompareText(ImplementedReservedWords[i],AText)=0 then exit(true);
  for i := low(NotImplementedReservedWords) to high(NotImplementedReservedWords) do
    if CompareText(NotImplementedReservedWords[i],AText)=0 then exit(true);
  exit(false);
end;

function ParseLine(ALine: string): TStringList;
var
  p,start: Integer;
  inSpace, inStr: boolean;
  token: string;
  i,j: Integer;
begin
  result := TStringList.Create;

  p := 1;
  start := 1;
  inSpace := true;
  inStr := false;
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
        if not ((p < length(ALine)) and (ALine[p+1] = '"')) then
        begin
          inStr := false;
          token := copy(ALine, start, p-start+1);
          result.Add(token);
          inSpace := true;
        end;
      end;

      if not inStr and not inSpace and not (ALine[p] in['A'..'Z','a'..'z','_','0'..'9']) and
        not ((Aline[p]='&') and (p < length(ALine)) and (ALine[p+1] in['h','H'])) then
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

function ParseRandom(ALine: TStringList; var AIndex: integer): integer;
var errPos: integer;
begin
  if (AIndex < ALine.Count) and (CompareText(ALine[AIndex], 'Rnd') = 0) then
  begin
    inc(AIndex);
    if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
    if TryToken(ALine,AIndex,'*') then
    begin
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

function TryParsePlayer(ALine: TStringList; var AIndex: integer): TPlayer;
var
  pl: TPlayer;
begin
  if AIndex >= ALine.Count then exit(plNone);
  for pl := succ(plNone) to high(TPlayer) do
    if CompareText(ALine[AIndex],PlayerIdentifiers[pl])=0 then
    begin
      inc(AIndex);
      exit(pl);
    end;
  exit(plNone);
end;

function ExpectPlayers(ALine: TStringList; var AIndex: integer): TPlayers;
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

    if plCurrentPlayer in result then raise exception.Create('Current player does not define a player');
    if result = [] then raise exception.Create('No player specified');
  end else
  begin
    pl := TryParsePlayer(ALine,AIndex);
    if pl = plNone then raise exception.Create('Expecting player but "' + ALine[AIndex] + '" found');
    result := [pl];
  end;
end;

function GetBitCountOfType(AName: string): integer;
begin
  if (CompareText(AName,'Byte')=0) or (CompareText(AName,'UInt8')=0) then result := 8
  else if (CompareText(AName,'UShort')=0) or (CompareText(AName,'UInt16')=0) then result := 16
  else if (CompareText(AName,'UInt24')=0) then result := 24
  else result := 0;
end;

function IsIntegerType(AName: string): boolean;
begin
  result := GetBitCountOfType(AName) > 0;
end;

function BitCountNeededFor(AValue: integer): integer;
begin
  if AValue >= 1 shl 24 then raise exception.Create('Value too big') else
  if AValue >= 1 shl 16 then result := 24 else
  if AValue >= 1 shl 8 then result := 16 else
    result := 8;
end;

end.
