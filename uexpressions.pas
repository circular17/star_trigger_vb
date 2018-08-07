unit uexpressions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, uinstructions;

type
  TScalarVariableType = (svtNone, svtInteger, svtSwitch);
  TScalarVariable = record
    VarType: TScalarVariableType;
    Constant: boolean;
    UnitType: string;
    Player: TPlayer;
    Switch: integer;
    IntValue: integer;
    BoolValue: boolean;
  end;

function ExpectString(ALine: TStringList; var AIndex: integer): string;
function TryIdentifier(ALine: TStringList; var AIndex: integer; out AIdentifier: string): boolean;
function TryInteger(ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
function ExpectInteger(ALine: TStringList; var AIndex: integer): integer;
function TryBoolean(ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
function TryScalarVariable(ALine: TStringList; var AIndex: integer): TScalarVariable;
function TryString(ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
function TryExpression(ALine: TStringList; var AIndex: integer; AProg: TInstructionList;
                       ADestPlayer: TPlayer; ADestUnitType: string;
                       AMode: TSetIntegerMode; ARaiseException: boolean): boolean;

implementation

uses uparsevb, uvariables, fgl;

function TryIdentifier(ALine: TStringList; var AIndex: integer; out AIdentifier: string): boolean;
begin
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
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

function TryInteger(ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
var errPos, idxVar: integer;
  s, ident, strValue: String;
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
      if TryToken(ALine,AIndex,'Asc') then
      begin
        ExpectToken(ALine,AIndex,'(');
        s := ExpectString(ALine,AIndex);
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
        if not TryIdentifier(ALine,AIndex,ident) then
          raise exception.Create('Identifier expected');
        ExpectToken(ALine,AIndex,')');
        idxVar := IntArrayIndexOf(ident);
        if idxVar <> -1 then
        begin
          AValue := 1;
          exit(true);
        end;
        idxVar := BoolArrayIndexOf(ident);
        if idxVar <> -1 then
        begin
          AValue := 1;
          exit(true);
        end;
      end;

      if TryToken(ALine,AIndex,'UBound') then
      begin
        ExpectToken(ALine,AIndex,'(');
        if not TryIdentifier(ALine,AIndex,ident) then
          raise exception.Create('Identifier expected');
        ExpectToken(ALine,AIndex,')');
        idxVar := IntArrayIndexOf(ident);
        if idxVar <> -1 then
        begin
          AValue := IntArrays[idxVar].Size;
          exit(true);
        end;
        idxVar := BoolArrayIndexOf(ident);
        if idxVar <> -1 then
        begin
          AValue := BoolArrays[idxVar].Size;
          exit(true);
        end;
      end;

      if TryToken(ALine,AIndex,'Len') then
      begin
        ExpectToken(ALine,AIndex,'(');
        strValue := ExpectString(ALine,AIndex);
        ExpectToken(ALine,AIndex,')');
        AValue := length(strValue);
        exit(true);
      end;

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

function TryBoolean(ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
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
      idxVar := BoolVarIndexOf(ALine[AIndex]);
      if (idxVar<>-1) and BoolVars[idxVar].Constant then
      begin
        AValue := (BoolVars[idxVar].Value = svSet);
        inc(AIndex);
        exit(true);
      end;
      idxVar := BoolArrayIndexOf(ALine[AIndex]);
      if (idxVar <> -1) and BoolArrays[idxVar].Constant then
      begin
        idx := AIndex+1;
        if TryToken(ALine,idx,'(') then
        begin
          arrIndex := ExpectInteger(ALine,idx);
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

function TryScalarVariable(ALine: TStringList; var AIndex: integer): TScalarVariable;
var varIdx, arrayIndex, idx: integer;
  pl: TPlayer;
  unitType: String;
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
      result.UnitType := IntVars[varIdx].UnitType;
      result.Switch := -1;
      result.Constant:= IntVars[varIdx].Constant;
      result.IntValue:= IntVars[varIdx].Value;
      result.BoolValue:= IntVars[varIdx].Value<>0;
      exit;
    end;

    varIdx := BoolVarIndexOf(ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
      result.VarType := svtSwitch;
      result.Player := plNone;
      result.UnitType := '';
      result.Switch := BoolVars[varIdx].Switch;
      result.Constant:= BoolVars[varIdx].Constant;
      result.BoolValue:= BoolVars[varIdx].Value = svSet;
      result.IntValue := integer(result.BoolValue);
      exit;
    end;

    varIdx := BoolArrayIndexOf(ALine[AIndex]);
    if varIdx <> -1 then
    begin
      inc(AIndex);
      if TryToken(ALine,AIndex,'(') then
      begin
        arrayIndex := ExpectInteger(ALine,AIndex);
        if (arrayIndex < 1) or (arrayIndex > BoolArrays[varIdx].Size) then
          raise exception.Create('Array index out of bounds');

        result.VarType := svtSwitch;
        result.Player := plNone;
        result.UnitType := '';
        result.Switch := BoolVars[BoolArrays[varIdx].Vars[arrayIndex-1]].Switch;
        result.Constant:= BoolArrays[varIdx].Constant;
        result.BoolValue:= BoolArrays[varIdx].Values[arrayIndex-1] = svSet;
        result.IntValue := integer(result.BoolValue);
        exit;
      end else
        Dec(AIndex);
    end;

    varIdx := IntArrayIndexOf(ALine[AIndex]);
    if varIdx <> -1 then
    begin
      Inc(AIndex);
      if TryToken(ALine,AIndex,'(') then
      begin
        if TryToken(ALine,AIndex,'Me') then
        begin
          if IntArrays[varIdx].Constant then
            raise exception.Create('Cannot access a constant via Me');
          result.IntValue:= 0;
          result.UnitType := IntArrays[varIdx].UnitType;
          result.Player := plCurrentPlayer;
        end
        else
        begin
          arrayIndex := ExpectInteger(ALine, AIndex);
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
        result.BoolValue:= result.IntValue<>0;
        exit;
      end else
        Dec(AIndex);
    end;

    idx := AIndex;
    pl := TryParsePlayer(ALine,idx);
    if pl <> plNone then
    begin
      if TryToken(ALine,idx,'.') then
      begin
        if TryToken(ALine,idx,'DeathCount') then
        begin
          if TryToken(ALine,idx,'(') then
          begin
            unitType := ExpectString(ALine,idx);
            ExpectToken(ALine,idx,')');
          end else
            unitType := 'Any unit';

          result.VarType := svtInteger;
          result.Player := pl;
          result.UnitType := unitType;
          result.Switch := -1;
          result.Constant:= False;
          result.IntValue:= 0;
          result.BoolValue:= false;

          AIndex := idx;
          exit;
        end else
        if idx < ALine.Count then
        begin
          varIdx := IntArrayIndexOf(ALine[idx]);
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

function TryString(ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
var
  scalar: TScalarVariable;
  idxVar, intVal: Integer;
  boolVal: boolean;
  idx: integer;
  firstElem: boolean;
begin
  idx := AIndex;
  AStr := '';
  firstElem := true;
  repeat
    if idx >= ALine.Count then
    begin
      if ARaiseException then raise exception.Create('Expecting string but end of line found');
      exit(false);
    end;

    if TryToken(ALine,idx,'Chr') then
    begin
      ExpectToken(ALine,idx,'(');
      intVal := ExpectInteger(ALine,idx);
      ExpectToken(ALine,idx,')');
      AStr += chr(intVal);
    end else
    if copy(ALine[idx],1,1) = '"' then
    begin
      AStr += RemoveQuotes(ALine[idx]);
      Inc(idx);
    end else
    if TryInteger(ALine,idx,intVal) then
    begin
      AStr += inttostr(intVal);
      if firstElem then
      begin
        if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    if TryBoolean(ALine,idx,boolVal) then
    begin
      AStr += BoolToStr(boolVal, 'True', 'False');
      if firstElem then
      begin
        if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    begin
     idxVar := StringIndexOf(ALine[idx]);
     if idxVar <> -1 then
     begin
       AStr += StringVars[idxVar].Value;
       inc(idx);
     end else
     begin
       scalar := TryScalarVariable(ALine,idx);
       if scalar.VarType <> svtNone then
       begin
         if not scalar.Constant then
         begin
           if ARaiseException then raise exception.Create('Only constants can be used in a string');
           exit(false);
         end;

         case scalar.VarType of
         svtInteger: AStr += inttostr(scalar.IntValue);
         svtSwitch: AStr += BoolToStr(scalar.BoolValue, 'True', 'False');
         else raise exception.Create('Unhandled case');
         end;

         if firstElem then
         begin
           if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
           firstElem := false;
           continue;
         end;
       end else
       begin
         if ARaiseException then raise exception.Create('Expecting string but "' + ALine[idx] + '" found');
         exit(false);
       end;
     end;
    end;
    firstElem := false;
  until not TryToken(ALine,idx,'&');
  AIndex := idx;
  result := true;
end;

type

  { TExpressionNode }

  TExpressionNode = class
    Negative: boolean;
    constructor Create(ANegative: boolean);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); virtual; abstract;
    function AlwaysClearAccumulator: boolean; virtual; abstract;
  end;

  TExpressionNodeList = specialize TFPGList<TExpressionNode>;

  { TVariableNode }

  TVariableNode = class(TExpressionNode)
    Player: TPlayer;
    UnitType: string;
    constructor Create(ANegative: boolean; APlayer: TPlayer; AUnitType: string);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
  end;

  { TFunctionCallNode }

  TFunctionCallNode = class(TExpressionNode)
    Name: string;
    constructor Create(ANegative: boolean; AName: string);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
  end;

  { TRandomNode }

  TRandomNode = class(TExpressionNode)
    Range: integer;
    constructor Create(ANegative: boolean; ARange: integer);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
  end;

function TryExpression(ALine: TStringList; var AIndex: integer;
  AProg: TInstructionList; ADestPlayer: TPlayer; ADestUnitType: string; AMode: TSetIntegerMode; ARaiseException: boolean): boolean;
var
  elements: TExpressionNodeList;
  constElement, intValue: integer;
  neg, firstElem: boolean;
  idx, i: integer;
  scalar: TScalarVariable;
  name: string;

  procedure FreeElems;
  var
    i: Integer;
  begin
    for i := 0 to elements.Count-1 do
      elements[i].Free;
    elements.Free;
  end;

  function NegativeCount: integer;
  var
    i: Integer;
  begin
    result := 0;
    for i := 0 to elements.Count-1 do
      if elements[i].Negative then result += 1;
  end;

  function PositiveCount: integer;
  var
    i: Integer;
  begin
    result := 0;
    for i := 0 to elements.Count-1 do
      if not elements[i].Negative then result += 1;
  end;

  procedure NegateAll;
  var
    i: Integer;
  begin
    for i := 0 to elements.Count-1 do
      elements[i].Negative := not elements[i].Negative;
    constElement:= -constElement;
  end;

begin
  elements := TExpressionNodeList.Create;
  idx := AIndex;
  neg := false;
  constElement := 0;
  if TryToken(ALine,idx,'+') then neg := false
  else if TryToken(ALine,idx,'-') then neg := true;
  repeat
    if TryInteger(ALine,idx,intValue) then
    begin
      if neg then constElement -= intValue
      else constElement += intValue;
    end else
    begin
      scalar:= TryScalarVariable(ALine,idx);
      case scalar.VarType of
      svtInteger: elements.Add(TVariableNode.Create(neg,scalar.Player,scalar.UnitType));
      svtSwitch:
        begin
          FreeElems;
          if ARaiseException then
            raise exception.Create('Expecting integer value but boolean found');
          exit(false);
        end
      else
        begin
          if TryIdentifier(ALine,idx,name) then  //function call?
          begin
            if TryToken(ALine,idx,'(') then ExpectToken(ALine,idx,')');
            elements.Add(TFunctionCallNode.Create(neg, name));
          end else
          begin
            FreeElems;
            if ARaiseException then
              raise exception.Create('Integer expected');
            exit(false);
          end;
        end;
      end;
    end;

    if TryToken(ALine,idx,'+') then neg := false
    else if TryToken(ALine,idx,'-') then neg := true
    else break;
  until false;

  if AMode = simSubtract then
  begin
    AMode := simAdd;
    NegateAll;
  end;

  for i := elements.Count-1 downto 0 do
    if elements[i] is TVariableNode then
    with TVariableNode(elements[i]) do
    begin
      if (Player = ADestPlayer) and
         (UnitType = ADestUnitType) then
      begin
        if (AMode = simSetTo) and not Negative then
        begin
          AMode := simAdd;
          elements[i].Free;
          elements.Delete(i);
        end else
        if (AMode = simAdd) and Negative then
        begin
          AMode := simSetTo;
          elements[i].Free;
          elements.Delete(i);
        end;
      end;
    end;

  if (PositiveCount = 0) and (NegativeCount = 0) then
  begin
    if (AMode = simSetTo) or (constElement <> 0) then
      AProg.Add( TSetIntegerInstruction.Create( ADestPlayer, ADestUnitType, AMode, constElement) )
  end
  else
  begin
    if AMode = simSetTo then
    begin
      if constElement > 0 then
      begin
        AProg.Add( TSetIntegerInstruction.Create( ADestPlayer, ADestUnitType, simSetTo, constElement) );
        constElement := 0;
      end else
      begin
        AProg.Add( TSetIntegerInstruction.Create( ADestPlayer, ADestUnitType, simSetTo, 0) );
      end;
    end else
    begin
      if constElement > 0 then
      begin
        AProg.Add( TSetIntegerInstruction.Create( ADestPlayer, ADestUnitType, simAdd, constElement) );
        constElement := 0;
      end;
    end;

    firstElem := true;
    for i := 0 to elements.Count-1 do
      if not elements[i].Negative then
      begin
        if not firstElem and elements[i].AlwaysClearAccumulator then
        begin
          AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itAddAccumulator ) );
          firstElem := true;
        end;
        elements[i].LoadIntoAccumulator(firstElem, AProg);
        firstElem := false;
      end;
    if not firstElem then
      AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itAddAccumulator ) );

    firstElem := true;
    for i := 0 to elements.Count-1 do
      if elements[i].Negative then
      begin
        if not firstElem and elements[i].AlwaysClearAccumulator then
        begin
          AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itSubtractAccumulator ) );
          firstElem:= true;
        end;
        elements[i].LoadIntoAccumulator(firstElem, AProg);
        firstElem := false;
      end;
    if not firstElem then
      AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itSubtractAccumulator ) );

    if constElement < 0 then
      AProg.Add( TSetIntegerInstruction.Create( ADestPlayer, ADestUnitType, simSubtract, -constElement) );
  end;

  FreeElems;
  AIndex := idx;
  exit(true);
end;

function ExpectString(ALine: TStringList; var AIndex: integer): string;
begin
  TryString(ALine,AIndex,result,True);
end;

constructor TRandomNode.Create(ANegative: boolean; ARange: integer);
begin
  inherited Create(ANegative);
  Range := ARange;
end;

procedure TRandomNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
begin
  if AClearAcc then
    AProg.Add( TTransferIntegerInstruction.Create(Range, itRandomizeAccumulator) )
  else
    raise exception.Create('Unhandled case');
end;

function TRandomNode.AlwaysClearAccumulator: boolean;
begin
  result := true;
end;

constructor TFunctionCallNode.Create(ANegative: boolean; AName: string);
begin
  inherited Create(ANegative);
  Name := AName;
end;

procedure TFunctionCallNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
begin
  if AClearAcc then
    AProg.Add(TCallInstruction.Create(Name,[],'Integer'))
  else
    raise exception.Create('Unhandled case');
end;

function TFunctionCallNode.AlwaysClearAccumulator: boolean;
begin
  result := true;
end;

{ TVariableNode }

constructor TVariableNode.Create(ANegative: boolean; APlayer: TPlayer;
  AUnitType: string);
begin
  inherited Create(ANegative);
  Player:= APlayer;
  UnitType := AUnitType;
end;

procedure TVariableNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
begin
  if AClearAcc then
    AProg.Add(TTransferIntegerInstruction.Create(Player,UnitType,itCopyIntoAccumulator))
  else
    AProg.Add(TTransferIntegerInstruction.Create(Player,UnitType,itAddIntoAccumulator));
end;

function TVariableNode.AlwaysClearAccumulator: boolean;
begin
  result := false;
end;

{ TExpressionNode }

constructor TExpressionNode.Create(ANegative: boolean);
begin
  Negative:= ANegative;
end;

end.
