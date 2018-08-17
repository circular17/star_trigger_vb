unit uparsecomplex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvariables, usctypes, uinstructions;

type
  ArrayOfInteger = array of integer;
  ArrayOfString = array of string;

function TryUnitPropVar(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryUnitProperties(AScope: integer; ALine: TStringList; var AIndex: integer; out AProp: TUnitProperties): boolean;
function ParseStringArray(AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfString;
function ParseBoolArray(AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfSwitchValue;
function ParseIntArray(AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfInteger;
function TryStringConstant(AScope: integer; ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;

function IsVarNameUsed(AScope: integer; AName: string; AParamCount: integer): boolean;
procedure ProcessDim(AScope: integer; ADeclaration: string; AProg: TInstructionList; AInit0: boolean; out AWarning: string);
procedure ProcessDim(AScope: integer; ALine: TStringList; AProg: TInstructionList; AInit0: boolean; out AWarning: string);

implementation

uses uparsevb, uparsescalar, uexpressions, utriggerinstructions, uprocedures;

function TryUnitProperties(AScope: integer; ALine: TStringList; var AIndex: integer; out AProp: TUnitProperties): boolean;
var idx, intVal: integer;
  name: String;
  valueType: string;
  boolVal: boolean;

  procedure ValueInteger(AMin,AMax: integer);
  begin
    if valueType <> 'UInt24' then
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
  AProp.Energy := 25;
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
    if TryIntegerConstant(AScope, ALine,idx,intVal) then valueType := 'UInt24'
    else if TryBoolean(AScope, ALine,idx,boolVal) then valueType := 'Boolean'
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
      ValueInteger(0,65535);
      AProp.Resource := intVal;
    end else
    if CompareText(name,'HangarCount')=0 then
    begin
      ValueInteger(0,65535);
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
    end else
      raise exception.Create('Unknown member "'+name+'"');

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

function TryUnitPropVar(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
var
  idxProp: Integer;
  prop: TUnitProperties;
begin
  if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
  begin
    idxProp := UnitPropIndexOf(AScope, ALine[AIndex]);
    if idxProp <> -1 then
    begin
      inc(AIndex);
      exit(idxProp);
    end;
  end;

  if TryUnitProperties(AScope, ALine,AIndex,prop) then
  begin
    result := FindOrCreateUnitProperty(prop);
  end else
    exit(-1);
end;

function ParseIntArray(AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfInteger;
var count: integer;
begin
  setlength(result, 4);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count >= length(result) then
      setlength(result, Count*2 + 4);

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if not TryIntegerConstant(AScope, ALine, AIndex, result[count]) then
      raise exception.Create('Expecting integer or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ParseBoolArray(AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfSwitchValue;
var count: integer;
  boolVal: boolean;
begin
  setlength(result, 4);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count >= length(result) then
      setlength(result, Count*2 + 4);

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if TryBoolean(AScope, ALine,AIndex,boolVal) then
      result[Count] := BoolToSwitch[boolval]
    else if TryToken(ALine,AIndex,'Rnd') then
    begin
      result[Count] := svRandomize;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
    end
    else
      raise exception.Create('Expecting boolean or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ParseStringArray(AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfString;
var count: integer;
begin
  setlength(result, 4);
  count := 0;
  ExpectToken(ALine, AIndex, '{');
  while not TryToken(ALine, AIndex, '}') do
  begin
    if Count >= length(result) then
      setlength(result, Count*2 + 4);

    if Count > 0 then ExpectToken(ALine, AIndex, ',');
    if not TryStringConstant(AScope, ALine, AIndex, result[count]) then
      raise exception.Create('Expecting string or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function TryStringConstant(AScope: integer; ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
var
  scalar: TScalarVariable;
  idxVar, intVal: Integer;
  boolVal: boolean;
  idx, i: integer;
  firstElem, found: boolean;
  ident: string;
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
      intVal := ExpectIntegerConstant(AScope, ALine,idx);
      ExpectToken(ALine,idx,')');
      AStr += chr(intVal);
    end else
    if copy(ALine[idx],1,1) = '"' then
    begin
      AStr += RemoveQuotes(ALine[idx]);
      Inc(idx);
    end else
    if TryIntegerConstant(AScope, ALine,idx,intVal) then
    begin
      AStr += inttostr(intVal);
      if firstElem then
      begin
        if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    if TryBoolean(AScope, ALine,idx,boolVal) then
    begin
      AStr += BoolToStr(boolVal, 'True', 'False');
      if firstElem then
      begin
        if not ((idx < ALine.Count) and (ALine[idx] = '&')) then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    if TryToken(ALine,idx,'AI') then
    begin
      ExpectToken(ALine,idx,'.');
      if not TryIdentifier(ALine,idx,ident, false) then raise exception.Create('Expecting script name');
      found := false;
      for i := low(AIScripts) to high(AIScripts) do
        if CompareText(ident, AIScripts[i].Identifier)=0 then
        begin
          AStr += AIScripts[i].Code;
          found := true;
          break;
        end;
      if not found then raise exception.Create('Unknown AI script');
    end else
    begin
     idxVar := StringIndexOf(AScope, ALine[idx]);
     if idxVar <> -1 then
     begin
       AStr += StringVars[idxVar].Value;
       inc(idx);
     end else
     begin
       idxVar := StringArrayIndexOf(AScope, ALine[idx]);
       if idxVar <> -1 then
       begin
         inc(idx);
         ExpectToken(ALine,idx,'(');
         if TryToken(ALine,idx,'Me') then
            raise exception.Create('Constant arrays cannot be indexed by "Me"');
         intVal := ExpectIntegerConstant(AScope,ALine,idx);
         ExpectToken(ALine,idx,')');
         if (intVal < 1) or (intVal > StringArrays[idxVar].Size) then
           raise exception.Create('Index out of bounds');

         AStr += StringArrays[idxVar].Values[intVal-1];
       end else
       begin
         scalar := TryScalarVariable(AScope, ALine,idx);
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
    end;
    firstElem := false;
  until not TryToken(ALine,idx,'&');
  AIndex := idx;
  result := true;
end;

function ExpectStringConstantImplementation(AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;
var
  intVal: integer;
  boolVal: boolean;
begin
  if not TryStringConstant(AScope, ALine,AIndex,result,True) then
  begin
    if AConvertToString then
    begin
      if TryIntegerConstant(AScope,ALine,AIndex,intVal) then
        result := IntToStr(intVal)
      else if TryBoolean(AScope,ALine,AIndex,boolVal) then
        result := BoolToStr(boolVal, 'True','False')
      else
        raise exception.Create('No value found');
    end else
      raise exception.Create('No string found');
  end;
end;

function IsVarNameUsed(AScope: integer; AName: string; AParamCount: integer): boolean;
begin
  result := (IntVarIndexOf(AScope, AName, False)<>-1) or (BoolVarIndexOf(AScope, AName, False)<>-1) or
            (IntArrayIndexOf(AScope, AName, False)<>-1) or (BoolArrayIndexOf(AScope, AName, False)<>-1) or
            (ProcedureIndexOf(AName,AParamCount)<>-1) or (UnitPropIndexOf(AScope, AName, False) <> -1) or
            (StringIndexOf(AScope, AName, False)<>-1) or (StringArrayIndexOf(AScope, AName, False)<>-1) or
            (SoundIndexOf(AScope, AName, False)<>-1) or
            (CompareText('AI',AName) = 0) or (CompareText('Present',AName) = 0) or
            (CompareText('CountIf',AName)=0) or IsUnitType(AName) or (CompareText('Alliance',AName) = 0) or
            (CompareText('Unit',AName) = 0) or (CompareText('Leaderboard',AName) = 0) or
            (CompareText('Switch',AName) = 0);
end;

procedure ProcessDim(AScope: integer; ALine: TStringList; AProg: TInstructionList; AInit0: boolean; out AWarning: string);
var
  index: Integer;
  varName, varType, filename, text: String;
  arraySize, bitCount: integer;
  isArray: boolean;
  timeMs, intVal: integer;
  arrValues: ArrayOfInteger;
  boolVal: boolean;
  prop: TUnitProperties;
  arrBoolValues: ArrayOfSwitchValue;
  Constant: boolean;
  strValues: ArrayOfString;

  procedure ExpectArraySize;
  begin
    if not TryIntegerConstant(AScope,ALine,index,arraySize) then
      raise exception.Create('Expecting array size or "]"');
    if (arraySize < 1) or (arraySize > MaxIntArraySize) then
      raise Exception.Create('Array size can go from 1 to ' + inttostr(MaxIntArraySize));
  end;

  procedure SetupIntVar(AIntVar: integer; AExpr : TExpression = nil);
  begin
    with IntVars[AIntVar] do
    begin
      if AExpr <> nil then
      begin
        if AExpr.IsConstant then
        begin
          Value := AExpr.ConstElement;
          AExpr.Free;
        end
        else
        begin
          if Constant then
            raise exception.Create('Expression is not constant');
          AExpr.AddToProgram( AProg, Player, UnitType, simSetTo);
          AExpr.Free;
          exit;
        end;
      end;
      if not Constant and (AInit0 or (Value <> 0)) then
      begin
        if Randomize then
          AProg.Add( TRandomizeIntegerInstruction.Create(Player,UnitType, Value) )
        else
          AProg.Add( CreateSetIntegerInstruction(Player,UnitType, simSetTo, Value) );
      end;
    end;
  end;

  procedure SetupBoolVar(ABoolVar: integer);
  begin
    with BoolVars[ABoolVar] do
    begin
      if not Constant and (AInit0 or (Value <> svClear)) then
        AProg.Add( TSetSwitchInstruction.Create(Switch, Value) );
    end;
  end;

  procedure SetupIntArray(AIntArray: integer);
  var
    i: Integer;
  begin
    with IntArrays[AIntArray] do
    begin
      if not Constant then
        for i := 1 to Size do
          SetupIntVar(Vars[i-1]);
    end;
  end;

  procedure SetupBoolArray(ABoolArray: integer);
  var
    i: Integer;
  begin
    with BoolArrays[ABoolArray] do
    begin
      if not Constant then
        for i := 1 to Size do
          SetupBoolVar(Vars[i-1]);
    end;
  end;


begin
  AWarning:= '';
  index := 0;
  if TryToken(ALine,index,'Const') then
    Constant := true
  else
  begin
    Constant := false;
    ExpectToken(ALine,index,'Dim');
  end;

  index := 1;
  if index >= ALine.Count then
    raise exception.Create('Variable name expected');

  while index < ALine.Count do
  begin
    if index > 1 then ExpectToken(ALine,index,',');

    isArray:= false;
    varName := ALine[index];
    arraySize := 0;
    bitCount := 0;
    if not IsValidVariableName(varName) then
      raise exception.Create('Invalid variable name');

    inc(index);

    if TryToken(ALine,index,'(') then
    begin
      isArray:= true;
      if not TryToken(ALine,index,')') then
      begin
        ExpectArraySize;
        ExpectToken(ALine,index,')');
      end;
    end;

    if TryToken(ALine,index,'As') then
    begin
      if index >= ALine.Count then
        raise Exception.Create('Expecting variable type');

      if TryToken(ALine,index,'Integer') or TryToken(ALine,index,'UInteger') then
        raise exception.Create('Please specify the bit count of the integer by using Byte, UInt16 or UInt24');

      bitCount := GetBitCountOfType(ALine[index]);
      if (bitCount<>0) then
      begin
        varType := ALine[index];
        inc(index);
      end
      else if TryToken(ALine,index,'Boolean') then varType := 'Boolean'
      else if TryToken(ALine,index,'String') then varType := 'String'
      else if TryToken(ALine,index,'UnitProperties') then varType := 'UnitProperties'
      else if TryToken(ALine,index,'Sound') then varType := 'Sound'
      else raise Exception.Create('Unknown type : ' + ALine[index]);

      if not isArray and (varType <> 'UnitProperties') and TryToken(ALine,index,'(') then
      begin
        isArray := true;
        if not TryToken(ALine,index,')') then
        begin
          ExpectArraySize;
          ExpectToken(ALine,index,')');
        end;
      end;
    end else
      varType := '?';

    if IsVarNameUsed(AScope,varName, integer(isArray)) then
      raise exception.Create('The name "' + varName + '" is already in use');

    if TryToken(ALine,index,'=') then
    begin
      if isArray then
      begin
        if varType = '?' then
        begin
          raise exception.Create('Array type not specified');
        end else
        if IsIntegerType(varType) then
        begin
          arrValues := ParseIntArray(AScope,ALine,index);
          if (arraySize <> 0) and (length(arrValues) <> arraySize) then
            raise exception.Create('Array size mismatch');
          if arraySize = 0 then
          begin
            if (length(arrValues) < 1) or (length(arrValues) > MaxIntArraySize) then
              raise exception.Create('Integer array size can go from 1 to ' + inttostr(MaxIntArraySize));
            arraySize:= length(arrValues);
          end;
          SetupIntArray(CreateIntArray(AScope,varName, arraySize, arrValues, bitCount, Constant));
        end else if varType = 'Boolean' then
        begin
          arrBoolValues := ParseBoolArray(AScope,ALine,index);
          if (arraySize <> 0) and (length(arrBoolValues) <> arraySize) then
            raise exception.Create('Array size mismatch (expecting ' + inttostr(arraySize) + ' but ' + inttostr(length(arrValues)) + ' found)');
          if arraySize = 0 then
          begin
            if (length(arrBoolValues) < 1) or (length(arrBoolValues) > MaxBoolArraySize) then
              raise exception.Create('Boolean array size can go from 1 to ' + inttostr(MaxBoolArraySize));
            arraySize:= length(arrBoolValues);
          end;
          SetupBoolArray(CreateBoolArray(AScope,varName, arraySize, arrBoolValues, Constant));
        end else if varType = 'String' then
        begin
          strValues := ParseStringArray(AScope,ALine,index);
          if (arraySize <> 0) and (length(strValues) <> arraySize) then
            raise exception.Create('Array size mismatch');
          if arraySize = 0 then
          begin
            if (length(strValues) < 1) or (length(strValues) > MaxStringArraySize) then
              raise exception.Create('Integer array size can go from 1 to ' + inttostr(MaxStringArraySize));
            arraySize:= length(strValues);
          end;
          CreateStringArray(AScope,varName, arraySize, strValues, Constant);
        end else
          raise exception.Create(varType+' arrays not supported');
      end else
      if varType = 'Sound' then
      begin
        ExpectToken(ALine,index,'{');
        filename := '';
        timeMs := -1;
        if not TryToken(ALine,index,'}') then
        while true do
        begin
          ExpectToken(ALine,index,'.');
          if TryToken(ALine,index,'Filename') then
          begin
            ExpectToken(ALine,index,'=');
            filename := ExpectStringConstant(AScope,ALine,index);
          end else
          if TryToken(ALine,index,'Duration') then
          begin
            ExpectToken(ALine,index,'=');
            timeMs := ExpectIntegerConstant(AScope,ALine,index);
          end else
            raise exception.Create('Unknown field. Expecting Filename or Duration');
          if not TryToken(ALine,index,',') then
          begin
            ExpectToken(ALine,index,'}');
            break;
          end;
        end;
        if filename = '' then raise exception.Create('Filename not specified');
        if timeMs = -1 then raise exception.Create('Duration not specified');
        CreateSound(AScope,varName, filename, timeMs, Constant);
      end else
      if varType = 'UnitProperties' then
      begin
        if TryUnitProperties(AScope,ALine,index,prop) then
        begin
          CreateUnitProp(AScope,varName, prop, Constant);
        end;
      end else
      if varType = 'String' then
      begin
        CreateString(AScope,varName,ExpectStringConstant(AScope,ALine,index), Constant);
      end else
      if varType = 'Boolean' then
      begin
        if TryToken(ALine,index,'Rnd') then
        begin
          if TryToken(ALine,index,'(') then ExpectToken(ALine,index,')');
          SetupBoolVar(CreateBoolVar(AScope,varName, svRandomize, Constant));
        end
        else
        if TryBoolean(AScope,ALine,index,boolVal) then
          SetupBoolVar(CreateBoolVar(AScope,varName, BoolToSwitch[boolVal], Constant))
        else
          raise exception.Create('Expecting boolean constant');
      end else
      if IsIntegerType(varType) then
      begin
        SetupIntVar(CreateIntVar(AScope,varName, 0, bitCount, false, Constant), TryExpression(AScope,ALine,index,true));
      end else
      begin
        if varType <> '?' then raise exception.Create('Unhandled case');

        if TryBoolean(AScope,ALine,index,boolVal) then
          SetupBoolVar(CreateBoolVar(AScope,varName, BoolToSwitch[boolVal], Constant))
        else
        if TryIntegerConstant(AScope,ALine,index,intVal) then
        begin
          bitCount := BitCountNeededFor(intVal);
          if not Constant then AWarning := 'Assuming ' + inttostr( bitCount) + ' bit value for "' + varName + '". Please specify integer type (Byte, UInt16, UInt24)';
          SetupIntVar(CreateIntVar(AScope,varName, intVal, bitCount, false, Constant));
        end
        else if TryToken(ALine,index,'Rnd') then
          raise exception.Create('Cannot determine if integer or boolean')
        else
        if TryStringConstant(AScope,ALine,index,text) then
          CreateString(AScope,varName,text, Constant)
        else
          raise exception.Create('Expecting constant value');
      end;
    end else
    begin
      if varType = '?' then
        raise Exception.Create('Variable type not specified');

      if isArray then
      begin
        if arraySize= 0 then
          raise exception.Create('Array size not specified');
        if varType = 'Boolean' then
          SetupBoolArray(CreateBoolArray(AScope,varName, arraySize, [], Constant))
        else if IsIntegerType(varType) then
          SetupIntArray(CreateIntArray(AScope,varName, arraySize, [], bitCount, Constant))
        else if varType = 'String' then
          CreateStringArray(AScope,varName, arraySize, [], Constant)
        else raise Exception.Create(varType+' arrays not supported')
      end else
      begin
        if varType = 'Boolean' then
          SetupBoolVar(CreateBoolVar(AScope,varName, svClear, Constant))
        else if IsIntegerType(varType) then
          SetupIntVar(CreateIntVar(AScope,varName, 0, bitCount, false, Constant))
        else raise exception.Create('Initial value needed for '+varType);
      end;
    end;
  end;
end;

procedure ProcessDim(AScope: integer; ADeclaration: string; AProg: TInstructionList; AInit0: boolean; out AWarning: string);
var
  line: TStringList;
begin
  line := ParseLine(ADeclaration);
  try
    ProcessDim(AScope, line, AProg, AInit0, AWarning);
  finally
    line.Free;
  end;
end;

initialization

  ExpectStringConstant := @ExpectStringConstantImplementation;

end.

