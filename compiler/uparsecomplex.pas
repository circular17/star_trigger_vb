unit uparsecomplex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvariables, usctypes, uinstructions, uscope, uparsescalar;

type
  ArrayOfInteger = array of integer;
  ArrayOfString = array of string;

function TryUnitPropertiesVariableOrDefinition(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryUnitPropertiesDefinition(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AProp: TUnitProperties): boolean;
function ParseStringArray(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfString;
function ParseBoolArray(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfSwitchValue;
function ParseIntArray(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfInteger;
function TryStringConstant(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
function TryMultiStringConstant(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AMultiStr: TMultiString; ARaiseException: boolean = false): boolean;

procedure ProcessDim(AThreads: TPlayers; AScope: integer; ADeclaration: string; AProg: TInstructionList; AInit0, AAllowMultithread: boolean; out AWarning: string);
procedure ProcessDim(AThreads: TPlayers; AScope: integer; ALine: TStringList; AProg: TInstructionList; AInit0, AMultithreadInit: boolean; out AWarning: string);

implementation

uses uparsevb, uexpressions, utriggerinstructions, uprocedures,
  umapinfo;

function TryUnitPropertiesDefinition(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AProp: TUnitProperties): boolean;
var intVal, oldIndex: integer;
  name: String;
  boolVal: boolean;

  procedure ValueInteger(AMin,AMax: integer);
  begin
    ExpectToken(ALine,AIndex,'=');
    intVal := ExpectIntegerConstant(AThreads, AScope, ALine, AIndex, false);
    if (intVal < AMin) or (intVal > AMax) then
      raise exception.Create('Value out of range (' + inttostr(AMin)+' to '+Inttostr(AMax)+')');
  end;
  procedure ValueBool;
  begin
    ExpectToken(ALine,AIndex,'=');
    boolVal := ExpectBooleanConstant(AThreads, AScope, ALine, AIndex);
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

  oldIndex := AIndex;
  intVal := 0;

  if not TryToken(ALine,AIndex,'{') then exit;
  if not TryToken(ALine,AIndex,'.') then
  begin
    AIndex := oldIndex;
    exit;
  end;

  while true do
  begin
    if TryToken(ALine,AIndex,'Life') then
    begin
      ValueInteger(0,100);
      AProp.Life := intVal;
    end else
    if TryToken(ALine,AIndex,'Shield') then
    begin
      ValueInteger(0,100);
      AProp.Shield := intVal;
    end else
    if TryToken(ALine,AIndex,'Energy') then
    begin
      ValueInteger(0,100);
      AProp.Energy := intVal;
    end else
    if TryToken(ALine,AIndex,'Resource') then
    begin
      ValueInteger(0,65535);
      AProp.Resource := intVal;
    end else
    if TryToken(ALine,AIndex,'HangarCount') then
    begin
      ValueInteger(0,65535);
      AProp.HangarCount := intVal;
    end else
    if TryToken(ALine,AIndex,'Burrowed') then
    begin
      ValueBool;
      AProp.Burrowed := boolVal;
    end else
    if TryToken(ALine,AIndex,'Cloaked') then
    begin
      ValueBool;
      AProp.Cloaked := boolVal;
    end else
    if TryToken(ALine,AIndex,'Hallucinated') then
    begin
      ValueBool;
      AProp.Hallucinated := boolVal;
    end else
    if TryToken(ALine,AIndex,'Invincible') then
    begin
      ValueBool;
      AProp.Invincible := boolVal;
    end else
    if TryToken(ALine,AIndex,'Lifted') then
    begin
      ValueBool;
      AProp.Lifted := boolVal;
    end else
    begin
      if AIndex >= ALine.Count then raise exception.Create('Unexpected end of line') else
      if not TryIdentifier(ALine, AIndex, name, false) then raise exception.Create('Expecting member name')
      else raise exception.Create('Unknown member "'+name+'"');
    end;

    if not TryToken(ALine,AIndex,',') then
    begin
      ExpectToken(ALine,AIndex,'}');
      break;
    end else
      ExpectToken(ALine,AIndex,'.');
  end;
  result := true;
end;

function TryUnitPropertiesVariableOrDefinition(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): integer;
var
  idxProp, triedScope: Integer;
  prop: TUnitProperties;
begin
  triedScope := AScope;
  while triedScope <> -1 do
  begin
    for idxProp := 0 to UnitPropCount-1 do
      if (UnitPropVars[idxProp].Scope = triedScope)
         and TryToken(ALine, AIndex, UnitPropVars[idxProp].Name) then
        exit(idxProp);
    triedScope := GetWiderScope(triedScope);
  end;

  if TryUnitPropertiesDefinition(AThreads, AScope, ALine,AIndex,prop) then
  begin
    result := FindOrCreateUnitProperty(prop);
  end else
    exit(-1);
end;

function ParseIntArray(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfInteger;
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
    if not TryIntegerConstant(AThreads, AScope, ALine, AIndex, result[count]) then
      raise exception.Create('Expecting integer or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ParseBoolArray(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfSwitchValue;
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
    if TryToken(ALine,AIndex,'Rnd') then
    begin
      result[Count] := svRandomize;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
    end else
      result[Count] := BoolToSwitch[ExpectBooleanConstant(AThreads, AScope, ALine, AIndex)];
    inc(count);
  end;
  setlength(result, count);
end;

function ParseStringArray(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): ArrayOfString;
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
    if not TryStringConstant(AThreads, AScope, ALine, AIndex, result[count]) then
      raise exception.Create('Expecting string or "}"');
    inc(count);
  end;
  setlength(result, count);
end;

function ExpectAIScript(ALine: TStringList; var AIndex: integer): integer;
var
  curIdent, nextIdent: String;
  endIdentPos, startIdentPos, i: Integer;
  found: Boolean;
begin
  curIdent := '';
  repeat
    found := false;
    startIdentPos := length(curIdent);
    for i := low(AIScripts) to high(AIScripts) do
      if (curIdent = '') or AIScripts[i].Identifier.StartsWith(curIdent) then
      begin
        endIdentPos := AIScripts[i].Identifier.IndexOf('.', startIdentPos);
        if endIdentPos = -1 then
        begin
          if TryToken(ALine,AIndex,AIScripts[i].Identifier.Substring(startIdentPos)) then
            exit(i);
        end else
        begin
          nextIdent := AIScripts[i].Identifier.Substring(startIdentPos, endIdentPos-startIdentPos);
          if TryToken(ALine,AIndex,nextIdent) then
          begin
            ExpectToken(ALine,AIndex,'.');
            curIdent := curIdent+nextIdent+'.';
            found := true;
            break;
          end;
        end;
      end;
    if not found then
    begin
      if not TryIdentifier(ALine, AIndex, curIdent, false) then
        raise exception.Create('Expecting AI identifier')
        else raise exception.Create('Unknown AI script');
    end;
  until false;
  result := -1;
end;

function TryStringConstant(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
var
  scalar: TScalarVariable;
  idxVar, intVal: Integer;
  boolVal: boolean;
  idx, aiIndex, oldIndex: integer;
  firstElem: boolean;
  uniquePlayer: TPlayer;
begin
  idx := AIndex;
  AStr := '';

  firstElem := true;
  repeat
    if TryToken(ALine,idx,'Me') then
    begin
      uniquePlayer := GetUniquePlayer(AThreads);
      if (uniquePlayer in [plPlayer1..plPlayer12]) and
         not (firstElem and not ARaiseException and not PeekToken(ALine,idx,'&')) then
      begin
        AStr += inttostr(ord(uniquePlayer)-ord(plPlayer1)+1);
        firstElem := false;
        continue;
      end else
      begin
        dec(idx);
        if not firstElem and ARaiseException then
          raise exception.Create('Player "Me" is ambiguous');
      end;
    end;

    if TryToken(ALine,idx,'CStr') then
    begin
      ExpectToken(ALine,idx,'(');
      AStr += ExpectStringConstant(AThreads, AScope, ALine, idx, true);
      ExpectToken(ALine,idx,')');
    end else
    if TryToken(ALine,idx,'Chr') then
    begin
      ExpectToken(ALine,idx,'(');
      intVal := ExpectIntegerConstant(AThreads, AScope, ALine,idx,false);
      if intVal > 255 then raise exception.Create('Value out of bounds');
      ExpectToken(ALine,idx,')');
      AStr += chr(intVal);
    end else
    if TryToken(ALine,idx,'Align') then
    begin
      ExpectToken(ALine,idx,'.');
      if TryToken(ALine,idx,'Left') then AStr += '' else
      if TryToken(ALine,idx,'Right') then AStr += #$12 else
      if TryToken(ALine,idx,'Center') then AStr += #$13 else
      if TryToken(ALine,idx,'Clear') then AStr += #$0C else
        raise exception.Create('Expecting alignment identifier');
    end else
    if TryToken(ALine,idx,'Color') then
    begin
      ExpectToken(ALine,idx,'.');
      if TryToken(ALine,idx,'PaleBlue') then AStr += #2 else
      if TryToken(ALine,idx,'Yellow') then AStr += #3 else
      if TryToken(ALine,idx,'White') then AStr += #4 else
      if TryToken(ALine,idx,'Gray') then AStr += #5 else
      if TryToken(ALine,idx,'Grey') then AStr += #5 else
      if TryToken(ALine,idx,'Red') then AStr += #6 else
      if TryToken(ALine,idx,'Green') then AStr += #7 else
      if TryToken(ALine,idx,'RedPlayer') then AStr += #8 else
      if TryToken(ALine,idx,'Invisible') then AStr += #$0B else
      if TryToken(ALine,idx,'BluePlayer') then AStr += #$0E else
      if TryToken(ALine,idx,'TealPlayer') then AStr += #$0F else
      if TryToken(ALine,idx,'PurplePlayer') then AStr += #$10 else
      if TryToken(ALine,idx,'OrangePlayer') then AStr += #$11 else
      if TryToken(ALine,idx,'BrownPlayer') then AStr += #$15 else
      if TryToken(ALine,idx,'WhitePlayer') then AStr += #$16 else
      if TryToken(ALine,idx,'YellowPlayer') then AStr += #$17 else
      if TryToken(ALine,idx,'GreenPlayer') then AStr += #$18 else
      if TryToken(ALine,idx,'BrightYellowPlayer') then AStr += #$19 else
      if TryToken(ALine,idx,'Cyan') then AStr += #$1A else
      if TryToken(ALine,idx,'PinkPlayer') then AStr += #$1B else
      if TryToken(ALine,idx,'DarkCyanPlayer') then AStr += #$1C else
      if TryToken(ALine,idx,'GrayGreen') then AStr += #$1D else
      if TryToken(ALine,idx,'BlueGray') then AStr += #$1E else
      if TryToken(ALine,idx,'Turquoise') then AStr += #$1F else
        raise exception.Create('Expecting color identifier');
    end else
    if (idx < ALine.Count) and (copy(ALine[idx],1,1) = '"') then
    begin
      AStr += RemoveQuotes(ALine[idx]);
      Inc(idx);
    end else
    if TryInteger(AThreads, AScope, ALine,idx,intVal) then
    begin
      AStr += inttostr(intVal);
      if firstElem then
      begin
        if not TryToken(ALine, idx, '&') then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    if TryBoolean(AThreads, AScope, ALine, idx, boolVal) then
    begin
      AStr += BoolToStr(boolVal, 'True', 'False');
      if firstElem then
      begin
        if not TryToken(ALine, idx, '&') then exit(false);
        firstElem := false;
        continue;
      end;
    end else
    if TryToken(ALine,idx,'AI') then
    begin
      ExpectToken(ALine,idx,'.');
      aiIndex := ExpectAIScript(ALine,idx);
      AStr += AIScripts[aiIndex].Code;
    end else
    begin
     idxVar := TryStringVariable(AScope, ALine, idx);
     if idxVar <> -1 then
     begin
       AStr += StringVars[idxVar].Value;
     end else
     begin
       idxVar := TryStringArray(AScope, ALine, idx);
       if idxVar <> -1 then
       begin
         ExpectToken(ALine,idx,'(');
         intVal := ExpectConstantIndex(AThreads,AScope,ALine,idx,true);
         ExpectToken(ALine,idx,')');
         if (intVal < 1) or (intVal > StringArrays[idxVar].Size) then
           raise exception.Create('Index out of bounds');

         AStr += StringArrays[idxVar].Values[intVal-1];
       end else
       begin
         oldIndex := idx;
         idxVar := TrySoundVariable(AScope, ALine, idx);
         if (idxVar <> -1) and TryToken(ALine, idx, '.') and TryToken(ALine, idx, 'Filename') then
         begin
           AStr += SoundVars[idxVar].Filename;
         end else
         begin
           idx := oldIndex;
           scalar := TryScalarVariable(AThreads, AScope, ALine, idx, true);
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
               if not TryToken(ALine, idx, '&') then exit(false);
               firstElem := false;
               continue;
             end;
           end else
           begin
             if ARaiseException then
             begin
               if idx >= ALine.Count then
                 raise exception.Create('Expecting string but end of line found')
                 else raise exception.Create('Expecting string but "' + ALine[idx] + '" found');
             end;
             exit(false);
           end;
         end;
       end;
     end;
    end;
    firstElem := false;
  until not TryToken(ALine,idx,'&');
  AIndex := idx;
  result := true;
end;

function TryMultiStringConstant(AThreads: TPlayers; AScope: integer;
  ALine: TStringList; var AIndex: integer; out AMultiStr: TMultiString;
  ARaiseException: boolean): boolean;
var
  endIndex, oldIndex: Integer;
  pl: TPlayer;
begin
  oldIndex := AIndex;
  endIndex := -1;
  for pl := succ(plNone) to high(TPlayer) do
    if pl in AThreads then
    begin
      AIndex := oldIndex;
      if not TryStringConstant([pl], AScope, ALine, AIndex, AMultiStr[pl], ARaiseException) then
        exit(false);
      if endIndex = -1 then endIndex := AIndex else
      if endIndex <> AIndex then
      begin
        if ARaiseException then
          raise exception.Create('Inconsistent string expression length');
        exit(false);
      end;
    end else
      AMultiStr[pl] := '';
  result := true;
end;

function ExpectStringConstantImplementation(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;
var
  intVal: integer;
  boolVal: boolean;
begin
  if not TryStringConstant(AThreads, AScope, ALine, AIndex, result, True) then
  begin
    if AConvertToString then
    begin
      if TryIntegerConstant(AThreads, AScope, ALine, AIndex, intVal) then
        result := IntToStr(intVal)
      else if TryBoolean(AThreads, AScope, ALine, AIndex, boolVal) then
        result := BoolToStr(boolVal, 'True','False')
      else
        raise exception.Create('No value found');
    end else
      raise exception.Create('No string found');
  end;
end;

function ExpectMultiStringConstantImplementation(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): TMultistring;
var
  endIndex, oldIndex: Integer;
  pl: TPlayer;
begin
  oldIndex := AIndex;
  endIndex := -1;
  for pl := succ(plNone) to high(TPlayer) do
    if pl in AThreads then
    begin
      AIndex := oldIndex;
      result[pl] := ExpectStringConstantImplementation([pl], AScope, ALine, AIndex, AConvertToString);
      if endIndex = -1 then endIndex := AIndex else
      if endIndex <> AIndex then
        raise exception.Create('Inconsistent string expression length');
    end else
      result[pl] := '';
end;

procedure ProcessDim(AThreads: TPlayers; AScope: integer; ALine: TStringList; AProg: TInstructionList; AInit0, AMultithreadInit: boolean; out AWarning: string);
var
  index: Integer;
  varName, varType, text: String;
  arraySize, bitCount: integer;
  isArray: boolean;
  intVal: integer;
  arrValues: ArrayOfInteger;
  boolVal: boolean;
  prop: TUnitProperties;
  arrBoolValues: ArrayOfSwitchValue;
  Constant, multiThreadVar, hasNew: boolean;
  strValues: ArrayOfString;
  su: TStarcraftUnit;

  procedure ExpectArraySize;
  begin
    if not TryIntegerConstant(AThreads, AScope, ALine, index, arraySize) then
      raise exception.Create('Expecting array size or "]"');
    if (arraySize < 1) or (arraySize > MaxIntArraySize) then
      raise Exception.Create('Array size can go from 1 to ' + inttostr(MaxIntArraySize));
  end;

  procedure SetupIntVar(AVar: integer; AExpr : TExpression = nil); overload;
  begin
    with IntVars[AVar] do
    begin
      IsTimer:= varType = 'Timer';
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

  procedure SetupIntVar(AName: string; AValue: integer; ABitCount: integer; AConstant: boolean = false; AExpr : TExpression = nil); overload;
  var intVar: integer;
  begin
    if multiThreadVar and not AConstant then
    begin
      if not AMultithreadInit then
      begin
        if Assigned(AExpr) then
        begin
          if not AExpr.IsConstant or (AExpr.ConstElement <> 0) then
          begin
            FreeAndNil(AExpr);
            raise exception.Create('Multithread variable cannot be initialized');
          end;
          FreeAndNil(AExpr);
        end;
        if AValue <> 0 then
          raise exception.Create('Multithread variable cannot be initialized');
      end;
      intVar := CreateMultithreadIntVar(AThreads, AScope, AName, ABitCount);
      if AMultithreadInit then
        SetupIntVar(intVar, AExpr)
      else
        IntVars[intVar].IsTimer := (varType = 'Timer');
    end else
    begin
      intVar := CreateIntVar(AScope, AName, AValue, ABitCount, false, AConstant);
      SetupIntVar(intVar, AExpr);
    end;
  end;

  procedure SetupBoolVar(ABoolVar: integer); overload;
  begin
    with BoolVars[ABoolVar] do
    begin
      if not Constant and (AInit0 or (Value <> svClear)) then
        AProg.Add( TSetSwitchInstruction.Create(Switch, Value) );
    end;
  end;

  procedure SetupBoolVar(AName: string; AValue: TSwitchValue; AConstant: boolean); overload;
  var boolVar: integer;
  begin
    if multiThreadVar and not AConstant then
      raise exception.Create('Multithread boolean variables not implemented');
    boolVar := CreateBoolVar(AScope, AName, AValue, AConstant);
    SetupBoolVar(boolVar);
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

  procedure ExpectSoundDefinition;
  var
    filename, wavName: string;
    filenameSpecified: boolean;
    timeMs, j: integer;
  begin
    ExpectToken(ALine,index,'{');
    filename := '';
    filenameSpecified := false;
    timeMs := -1;
    if not TryToken(ALine,index,'}') then
    while true do
    begin
      ExpectToken(ALine,index,'.');
      if not filenameSpecified and TryToken(ALine,index,'Filename') then
      begin
        ExpectToken(ALine,index,'=');
        if index >= ALine.Count then
        begin
          for j := WavMinIndex to WavMaxIndex do
          begin
            wavName := MapInfo.WavName[j];
            if wavName <> '' then AddToCompletionList(StrToBasic(wavName));
          end;
        end;
        filename := ExpectStringConstant(AThreads, AScope, ALine, index);
        filenameSpecified := true;
      end else
      if (timeMs = -1) and TryToken(ALine,index,'Duration') then
      begin
        ExpectToken(ALine,index,'=');
        timeMs := ExpectIntegerConstant(AThreads, AScope, ALine, index, false);
      end else
        raise exception.Create('Unknown field. Expecting Filename or Duration');
      if not TryToken(ALine,index,',') then
      begin
        ExpectToken(ALine,index,'}');
        break;
      end;
    end;
    if filename = '' then raise exception.Create('Filename not specified');
    if timeMs = -1 then timeMs := 0;
    CreateSound(AScope,varName, filename, timeMs, true);
  end;

begin
  multiThreadVar := (AThreads <> []) and not IsUniquePlayer(AThreads);
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
    if index >= ALine.Count then break;

    isArray:= false;
    varName := ALine[index];
    arraySize := 0;
    bitCount := 0;
    hasNew := false;
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
      if PeekToken(ALine,index,'Integer') or PeekToken(ALine,index,'UInteger') then
        raise exception.Create('Please specify the bit count of the integer by using Byte, UInt16 or UInt24');

      if TryUnsignedIntegerType(ALine,index,varType) then
        bitCount := GetBitCountOfType(varType)
      else
      begin
        bitCount := 0;
        if not Constant and TryToken(ALine,index,'New') then hasNew := true;
        if TryToken(ALine,index,'Boolean') then varType := 'Boolean'
        else if TryToken(ALine,index,'String') then varType := 'String'
        else if TryToken(ALine,index,'UnitProperties') then varType := 'UnitProperties'
        else if TryToken(ALine,index,'Unit') then varType := 'Unit'
        else if TryToken(ALine,index,'Sound') then varType := 'Sound'
        else if TryToken(ALine,index,'Timer') then begin varType := 'Timer'; bitCount := 24; end
        else
        begin
          if index >= ALine.Count then
            raise Exception.Create('Expecting variable type')
          else
            raise Exception.Create('Unknown type : ' + ALine[index]);
        end;
      end;

      if hasNew and (varType <> 'UnitProperties') and (varType <> 'Sound') then
        raise exception.Create('The type ' + varType + ' is not a class');

      if not isArray and (varType <> 'UnitProperties') and (varType <> 'Unit')
        and (varType <> 'Sound') and TryToken(ALine,index,'(') then
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

    if isArray and not Constant and multiThreadVar then
      raise exception.Create('Array variables not handled in multithreading');

    if IsVarNameUsed(AScope, varName, integer(isArray)) then
      raise exception.Create('The name "' + varName + '" is already in use');

    if hasNew then
    begin
      ExpectToken(ALine,index,'With');
      if varType = 'UnitProperties' then
      begin
        if TryUnitPropertiesDefinition(AThreads, AScope, ALine, index, prop) then
        begin
          CreateUnitProp(AScope,varName, prop, true);
        end else
          raise exception.Create('Expecting unit properties');
      end else
      if varType = 'Sound' then
      begin
        ExpectSoundDefinition;
      end else
        raise exception.Create('Type cannot be instantiated');
    end else
    if TryToken(ALine,index,'=') then
    begin
      if isArray then
      begin
        if varType = '?' then
        begin
          raise exception.Create('Array type not specified');
        end else
        if IsIntegerType(varType) or (varType = 'Timer') then
        begin
          arrValues := ParseIntArray(AThreads, AScope, ALine, index);
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
          arrBoolValues := ParseBoolArray(AThreads, AScope, ALine, index);
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
          strValues := ParseStringArray(AThreads, AScope, ALine, index);
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
        if not Constant then
        begin
          ExpectToken(ALine, index, 'New');
          ExpectToken(ALine, index, 'Sound');
          if TryToken(ALine, index, '(') then ExpectToken(ALine, index, ')');
          ExpectToken(ALine, index, 'With');
        end else
          raise exception.Create('Declare Sound classes using Dim');

        ExpectSoundDefinition;
      end else
      if varType = 'UnitProperties' then
      begin
        if not Constant then
        begin
          ExpectToken(ALine, index, 'New');
          ExpectToken(ALine, index, 'UnitProperties');
          if TryToken(ALine, index, '(') then ExpectToken(ALine, index, ')');
          ExpectToken(ALine, index, 'With');
        end else
          raise exception.Create('Declare UnitProperties classes using Dim');

        if TryUnitPropertiesDefinition(AThreads, AScope, ALine, index, prop) then
        begin
          CreateUnitProp(AScope,varName, prop, Constant);
        end else
          raise exception.Create('Expecting unit properties');
      end else
      if varType = 'Unit' then
      begin
        if not Constant then raise exception.Create('Unit types are constant');
        su := ExpectUnitType(AThreads, AScope, ALine, index);
        CreateUnitConst(AScope, varName, su);
      end else
      if varType = 'String' then
      begin
        CreateString(AScope,varName,ExpectStringConstant(AThreads,AScope,ALine,index), Constant);
      end else
      if varType = 'Boolean' then
      begin
        if TryToken(ALine,index,'Rnd') then
        begin
          if TryToken(ALine,index,'(') then ExpectToken(ALine,index,')');
          SetupBoolVar(varName, svRandomize, Constant);
        end
        else
        begin
          boolVal := ExpectBooleanConstant(AThreads, AScope, ALine, index);
          SetupBoolVar(varName, BoolToSwitch[boolVal], Constant);
        end;
      end else
      if IsIntegerType(varType) or (varType = 'Timer') then
      begin
        SetupIntVar(varName, 0, bitCount, Constant,
                    TryExpression(AThreads, AScope, ALine, index, true));
      end else
      if Constant and TryUnitType(AThreads, AScope, ALine, index, su) then
      begin
        CreateUnitConst(AScope, varName, su);
      end else
      begin
        if varType <> '?' then raise exception.Create('Unhandled case');

        if TryBoolean(AThreads, AScope, ALine, index, boolVal) then
          SetupBoolVar(varName, BoolToSwitch[boolVal], Constant)
        else
        if TryIntegerConstant(AThreads, AScope, ALine, index, intVal) then
        begin
          bitCount := BitCountNeededFor(intVal);
          if not Constant then AWarning := 'Assuming ' + inttostr( bitCount) + ' bit value for "' + varName + '". Please specify integer type (Byte, UInt16, UInt24)';
          SetupIntVar(varName, intVal, bitCount, Constant, nil);
        end
        else if TryToken(ALine,index,'Rnd') then
          raise exception.Create('Cannot determine if integer or boolean')
        else
        if TryStringConstant(AThreads,AScope,ALine,index,text) then
          CreateString(AScope,varName,text, Constant)
        else
          raise exception.Create('Expecting constant value');
      end;
    end else
    begin
      if Constant then
        raise exception.Create('Value not specified');
      if varType = '?' then
        raise Exception.Create('Variable type not specified');

      if isArray then
      begin
        if arraySize= 0 then
          raise exception.Create('Array size not specified');
        if varType = 'Boolean' then
          SetupBoolArray(CreateBoolArray(AScope,varName, arraySize, [], Constant))
        else if IsIntegerType(varType) or (varType = 'Timer') then
          SetupIntArray(CreateIntArray(AScope,varName, arraySize, [], bitCount, Constant))
        else if varType = 'String' then
          CreateStringArray(AScope,varName, arraySize, [], Constant)
        else raise Exception.Create(varType+' arrays not supported')
      end else
      begin
        if varType = 'Boolean' then
          SetupBoolVar(varName, svClear, Constant)
        else if IsIntegerType(varType) or (varType = 'Timer') then
          SetupIntVar(varName, 0, bitCount, Constant)
        else raise exception.Create('Initial value needed for '+varType);
      end;
    end;
  end;
end;

procedure ProcessDim(AThreads: TPlayers; AScope: integer; ADeclaration: string; AProg: TInstructionList; AInit0, AAllowMultithread: boolean; out AWarning: string);
var
  line: TStringList;
begin
  line := ParseLine(ADeclaration);
  try
    ProcessDim(AThreads, AScope, line, AProg, AInit0, AAllowMultithread, AWarning);
  finally
    line.Free;
  end;
end;

initialization

  ExpectStringConstant := @ExpectStringConstantImplementation;
  ExpectMultiStringConstant := @ExpectMultiStringConstantImplementation;

end.

