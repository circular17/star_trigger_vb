unit ureadprog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl, usctypes;

function ReadProg(AFilename: string; out AMainThread: TPlayer): boolean;
function ReadProg(ALines: TStrings; out AMainThread: TPlayer): boolean;
function IsTokenOverEndOfLine(ALastToken:string): boolean;

type
  TIntegerList = specialize TFPGList<Integer>;

const
  SubNewScope = 1;

var
  MainProg: TInstructionList;
  Procedures: array of record
    Name: string;
    ParamCount: integer;
    Instructions: TInstructionList;
    StartIP: integer;
    ReturnType: string;
    ReturnBitCount: integer;
    Expanded, StackChecked: boolean;
    Calls: TIntegerList;
    ExprTempVarInt: integer;
    InnerScope: integer;
  end;
  ProcedureCount: integer;

function CreateProcedure(AName: string; AParamCount: integer; AReturnType: string): integer;
function ProcedureIndexOf(AName: string; AParamCount: integer): integer;
function GetProcedureExprTempVarInt(AProcId, ABitCount: integer): integer;
function ProcedureReturnVar(AProcId: integer): integer;

var
  Events: array of record
    Players: TPlayers;
    Conditions: TConditionList;
    Instructions: TInstructionList;
    Preserve: boolean;
    InnerScope: integer;
  end;
  EventCount: integer;

  ReadProgErrors: TStringList;
  LastScope: integer;

function CreateEvent(APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;

var HyperTriggers: boolean;

implementation

uses uparsevb, uvariables, uexpressions, uparseconditions, utriggerinstructions, utriggerconditions, umapinfo;

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

function IsVarNameUsed(AScope: integer; AName: string; AParamCount: integer): boolean;
begin
  result := (IntVarIndexOf(AScope, AName, False)<>-1) or (BoolVarIndexOf(AScope, AName, False)<>-1) or
            (IntArrayIndexOf(AScope, AName, False)<>-1) or (BoolArrayIndexOf(AScope, AName, False)<>-1) or
            (ProcedureIndexOf(AName,AParamCount)<>-1) or (UnitPropIndexOf(AScope, AName, False) <> -1) or
            (StringIndexOf(AScope, AName, False)<>-1) or (SoundIndexOf(AScope, AName, False)<>-1) or
            (CompareText('AI',AName) = 0) or (CompareText('Present',AName) = 0) or
            (CompareText('CountIf',AName)=0) or IsUnitType(AName) or (CompareText('Alliance',AName) = 0) or
            (CompareText('Unit',AName) = 0) or (CompareText('Leaderboard',AName) = 0);
end;

function CreateProcedure(AName: string; AParamCount: integer; AReturnType: string): integer;
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
    Instructions.Add(TCommentInstruction.Create('Sub '+AName));
    StartIP := -1;
    ReturnType := AReturnType;
    ReturnBitCount:= GetBitCountOfType(AReturnType);
    Expanded := false;
    StackChecked := false;
    Calls := TIntegerList.Create;
    ExprTempVarInt := -1;
    InnerScope := result+2;
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

function GetProcedureExprTempVarInt(AProcId, ABitCount: integer): integer;
begin
  if Procedures[AProcId].ExprTempVarInt = -1 then
    Procedures[AProcId].ExprTempVarInt:= AllocateTempInt(ABitCount)
  else
  begin
    if IntVars[Procedures[AProcId].ExprTempVarInt].BitCount < ABitCount then
      IntVars[Procedures[AProcId].ExprTempVarInt].BitCount := ABitCount;
  end;
  result := Procedures[AProcId].ExprTempVarInt;
end;

function ProcedureReturnVar(AProcId: integer): integer;
begin
  result := GetProcedureExprTempVarInt(AProcId, Procedures[AProcId].ReturnBitCount);
end;

function CreateEvent(APlayers: TPlayers; AConditions: TConditionList; APreserve: boolean): integer;
begin
  if AConditions.IsArithmetic then
    raise exception.Create('Arithmetic expressions cannot be used in When clause');

  if EventCount >= length(Events) then
    setlength(Events, EventCount*2+4);
  result := EventCount;
  inc(EventCount);

  with Events[result] do
  begin
    Players := APlayers;
    Conditions := AConditions;
    Instructions := TInstructionList.Create;
    Instructions.Add(TCommentInstruction.Create('When'));
    Preserve := APreserve;
    InnerScope := -1-result;
  end;
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

function ProcessSub(ALine: TStringList): integer;
var
  index: Integer;
  name: String;
  isFunc: boolean;
  returnType: string;
  paramCount: integer;
begin
  index := 0;
  isFunc := TryToken(ALine,index,'Function');
  if not isFunc then ExpectToken(ALine,index,'Sub');

  name := ALine[index];
  if not IsValidVariableName(name) then
    raise exception.Create('Invalid procedure name');
  inc(index);

  paramCount := 0;
  if TryToken(ALine,index,'(') then
  begin
    ExpectToken(ALine,index,')');
  end;

  if isFunc then
  begin
    ExpectToken(ALine,index,'As');
    if TryToken(ALine,index,'Boolean') then
      returnType := 'Boolean'
    else
    begin
      if index >= ALine.Count then
        returnType := ''
      else
      begin
        returnType := ALine[index];
        inc(index);
      end;

      if not IsIntegerType(returnType) then
        raise exception.Create('Expecting return type (Byte, UInt16, UInt24, Boolean)');
    end;
  end else
    returnType := 'Void';

  if index < ALine.Count then
    raise exception.Create('End of line expect but "' + ALine[index] + '" found');

  if CompareText(name,'New')=0 then
  begin
    if returnType <> 'Void' then
      raise exception.Create('The sub New cannot have a return value');
    if paramCount <> 0 then
      raise exception.Create('The sub New takes no parameter');
    exit(-1);
  end else
    result := CreateProcedure(name,paramCount,returnType);
end;

function ProcessEvent(ALine: TStringList; APlayers: TPlayers; ACheckStop: boolean): integer;
var
  index, varIdx: Integer;
  conds: TConditionList;
begin
  index := 0;
  ExpectToken(ALine,index,'When');

  conds := ExpectConditions(GlobalScope, ALine,index,APlayers);
  if ACheckStop then
  begin
    varIdx := GetStopEventBoolVar;
    conds.Add( TSwitchCondition.Create(BoolVars[varIdx].Switch,False) );
  end;

  result := CreateEvent(APlayers, conds, not TryToken(ALine,index,'Once'));

  if index < ALine.Count then
    raise exception.Create('End of line expected');
end;

procedure ProcessDim(AScope: integer; ALineNumber: integer; ALine: TStringList; AProg: TInstructionList; AInit0: boolean);
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
            filename := ExpectString(AScope,ALine,index);
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
        CreateString(AScope,varName,ExpectString(AScope,ALine,index), Constant);
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
          if not Constant then ReadProgErrors.Add('Line ' + inttostr( ALineNumber) + ': assuming ' + inttostr( bitCount) + ' bit value for "' + varName + '". Please specify integer type (Byte, UInt16, UInt24)');
          SetupIntVar(CreateIntVar(AScope,varName, intVal, bitCount, false, Constant));
        end
        else if TryToken(ALine,index,'Rnd') then
          raise exception.Create('Cannot determine if integer or boolean')
        else
        if TryString(AScope,ALine,index,text) then
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

procedure ProcessDim(AScope: integer; ALineNumber: integer; ADeclaration: string; AProg: TInstructionList; AInit0: boolean);
var
  line: TStringList;
begin
  line := ParseLine(ADeclaration);
  try
    ProcessDim(AScope, ALineNumber, line, AProg, AInit0);
  finally
    line.Free;
  end;
end;

procedure AppendConditionalInstruction(AProg: TInstructionList; AConditions: TConditionList; AIfTrue, AIfFalse: TInstruction);
begin
  if (AConditions.Count = 1) and (AConditions[0] is TAlwaysCondition) then
  begin
    AProg.Add(AIfTrue);
    AConditions[0].Free;
    AConditions.Free;
    AIfFalse.Free;
  end else
  if (AConditions.Count = 1) and (AConditions[0] is TNeverCondition) then
  begin
    AProg.Add(AIfFalse);
    AConditions[0].Free;
    AConditions.Free;
    AIfTrue.Free;
  end else
  begin
    AProg.Add(TIfInstruction.Create(AConditions));
    AProg.Add(AIfTrue);
    AProg.Add(TElseInstruction.Create);
    AProg.Add(AIfFalse);
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

function TryNeutralAction(AScope: integer; AProg: TInstructionList; ALine: TStringList; var AIndex: Integer; AThreads: TPlayers): boolean;
var
  scenario: String;
  conds: TConditionList;
begin
  if TryToken(ALine,AIndex,'CountdownPaused') then
  begin
    result := true;
    ExpectToken(ALine,AIndex,'=');
    conds := ExpectConditions(AScope, ALine,AIndex,AThreads);
    AppendConditionalInstruction(AProg, conds, TPauseCountdownInstruction.Create(true),
                                        TPauseCountdownInstruction.Create(false));
  end else
  if TryToken(ALine,AIndex,'GamePaused') then  //not really neutral in the sense that each player can have a limited amount of pause
  begin
    result := true;
    ExpectToken(ALine,AIndex,'=');
    conds := ExpectConditions(AScope, ALine,AIndex,AThreads);
    AppendConditionalInstruction(AProg, conds, TPauseGameInstruction.Create(true),
                                        TPauseGameInstruction.Create(false));
  end else
  if TryToken(ALine,AIndex,'NextScenario') then
  begin
    ExpectToken(ALine,AIndex,'=');
    scenario := ExpectString(AScope, ALine,AIndex);
    AProg.Add(TSetNextScenarioInstruction.Create(scenario));
  end else
  if TryToken(ALine,AIndex,'Wait') then
  begin
    ExpectToken(ALine,AIndex,'(');
    AProg.Add(TWaitInstruction.Create(ExpectIntegerConstant(AScope, ALine,AIndex)));
    ExpectToken(ALine,AIndex,')');
  end else
    result := false;
end;

function TryPlayerAction(AScope: integer; AProg: TInstructionList; ALine: TStringList; var AIndex: Integer; APlayer: TPlayer; AThreads: TPlayers): boolean;
var
  intVal, propIndex, propVal, timeMs, tempInt, i: integer;
  unitType: TStarcraftUnit;
  locStr, destLocStr, orderStr, filename, text: String;
  boolVal, textDefined: boolean;
  destPl: TPlayer;
  props: TUnitProperties;
  prop: TSetUnitProperty;
  alliance: TAllianceStatus;
  pl: TPlayer;
  conds: TConditionList;
  expr: TExpression;
  subInstr: TInstructionList;
  players: TPlayers;
  unitOrder: TUnitOrder;

  procedure CheckCurrentPlayer;
  begin
    if APlayer <> plCurrentPlayer then
      raise exception.Create('This action can only be done with the current player "Me". Use instruction "Do As" for multithreading');
  end;

  function ParseOptionalQuantity: integer;
  begin
    if TryToken(ALine,AIndex,'All') then
    begin
      result := -1;
      ExpectToken(ALine,AIndex,',');
    end else
    if TryIntegerConstant(AScope, ALine,AIndex,result) then
    begin
      ExpectToken(ALine,AIndex,',');
    end else
      result := -1; //All by default
  end;

begin
  result := true;
  if TryToken(ALine,AIndex,'CreateUnit') then
  begin
    ExpectToken(ALine,AIndex,'(');
    expr := TryExpression(AScope,ALine,AIndex,True);
    if expr.IsConstant and (expr.ConstElement < 1) then
    begin
      expr.Free;
      raise exception.Create('Quantity must be at least 1');
    end;
    try
      ExpectToken(ALine,AIndex,',');
      unitType := ExpectUnitType(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,',');
      locStr := ExpectString(AScope,ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
      begin
        propIndex := TryUnitPropVar(AScope,ALine,AIndex);
        if propIndex = -1 then
          raise exception.Create('Unit properties expected');
      end else
        propIndex := -1;
      ExpectToken(ALine,AIndex,')');

      if propIndex = -1 then
      begin
        if TryToken(ALine,AIndex,'With') then
        begin
          propIndex := TryUnitPropVar(AScope,ALine,AIndex);
          if propIndex = -1 then
            raise exception.Create('Unit properties expected');
        end;
      end;

      if expr.IsConstant then
        AProg.Add(TCreateUnitInstruction.Create(APlayer, expr.ConstElement, unitType, locStr, propIndex))
      else
      begin
        tempInt := AllocateTempInt(8);
        expr.AddToProgram(AProg, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
        for i := 7 downto 0 do
        begin
          subInstr := TInstructionList.Create;
          subInstr.Add( TCreateUnitInstruction.Create(APlayer, 1 shl i, unitType, locStr, propIndex) );
          subInstr.Add( CreateSetIntegerInstruction(IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSubtract, 1 shl i) );
          AProg.Add( TFastIfInstruction.Create( [CreateIntegerCondition( IntVars[tempInt].Player,IntVars[tempInt].UnitType, icmAtLeast, 1 shl i)], subInstr) );
        end;
        ReleaseTempInt(tempInt);
      end;
    finally
      expr.Free;
    end;
  end else
  if TryToken(ALine,AIndex,'KillUnit') or TryToken(ALine,AIndex,'RemoveUnit') then
  begin
    boolVal:= upcase(ALine[AIndex-1][1])='K';
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectUnitType(AScope,ALine,AIndex);

    if TryToken(ALine,AIndex,')') then
      locStr:= ''
    else
    begin
      ExpectToken(ALine,AIndex,',');
      locStr := ExpectString(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
    end;
    AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, boolVal));
  end else
  if TryToken(ALine,AIndex,'GiveUnit') then
  begin
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectUnitType(AScope,ALine,AIndex);
    ExpectToken(ALine,AIndex,',');
    locStr := ExpectString(AScope,ALine,AIndex);
    ExpectToken(ALine,AIndex,',');
    destPl := TryParsePlayer(ALine,AIndex);
    if destPl = plNone then raise Exception.Create('Expecting player');
    ExpectToken(ALine,AIndex,')');

    AProg.Add(TGiveUnitInstruction.Create(APlayer, intVal, unitType, locStr, destPl));
  end else
  if TryToken(ALine,AIndex,'Units') then
  begin
    ExpectToken(ALine,AIndex,'(');
    intVal := ParseOptionalQuantity;
    unitType := ExpectUnitType(AScope,ALine,AIndex);
    if not TryToken(ALine,AIndex,',') then
      locStr := MapInfo.AnywhereLocationName
    else
      locStr := ExpectString(AScope,ALine,AIndex);
    ExpectToken(ALine,AIndex,')');
    ExpectToken(ALine,AIndex,'.');

    if TryToken(ALine,AIndex,'Properties') then
    begin
      ExpectToken(ALine,AIndex,'=');
      if not TryUnitProperties(AScope,ALine,AIndex,props) then
      begin
        if (AIndex < ALine.Count) and IsValidVariableName(ALine[AIndex]) then
        begin
          propIndex:= UnitPropIndexOf(AScope,ALine[AIndex]);
          if propIndex = -1 then
            raise exception.Create('Expecting unit properties');

          inc(AIndex);
          props := UnitPropVars[propIndex].Value;
        end else
        raise exception.Create('Expecting unit properties');
      end;

      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supLife, props.Life));
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supShield, props.Shield));
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supEnergy, props.Energy));
      if unitType = suAnyUnit then
        AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supResource, props.Resource))
      else
        if props.Resource <> 0 then
          raise exception.Create('Resource cannot be applied to a specific unit');
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, supHangarCount, props.HangarCount));

      if intVal = -1 then
      begin
        if props.Invincible then
          AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvEnable))
        else
          AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvDisable));
      end;

    end else
    if TryToken(ALine,AIndex,'Location') then
    begin
       ExpectToken(ALine,AIndex,'=');
       destLocStr := ExpectString(AScope,ALine,AIndex);

       AProg.Add(TTeleportUnitInstruction.Create(APlayer, intVal, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'Teleport') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectString(AScope,ALine,AIndex);
       ExpectToken(ALine,AIndex,')');

       AProg.Add(TTeleportUnitInstruction.Create(APlayer, intVal, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'AttractLocation') then
    begin
       ExpectToken(ALine,AIndex,'(');
       destLocStr := ExpectString(AScope,ALine,AIndex);
       ExpectToken(ALine,AIndex,')');
       if intVal <> -1 then raise exception.Create('Cannot specify quantity for this action (use All quantity instead)');
       AProg.Add(TMoveLocationInstruction.Create(APlayer, unitType, locStr, destLocStr));
    end else
    if TryToken(ALine,AIndex,'MoveOrder') or TryToken(ALine,AIndex,'PatrolOrder') or TryToken(ALine,AIndex,'AttackOrder') then
    begin
      orderStr := ALine[AIndex-1];
      if compareText(orderStr,'PatrolOrder')=0 then unitOrder := uoPatrol
      else if compareText(orderStr,'AttackOrder')=0 then unitOrder := uoAttack
      else unitOrder := uoMove;

      if intVal <> -1 then
        raise exception.Create('Cannot specify quantity for an order (use All quantity instead)');
      ExpectToken(ALine,AIndex,'(');
      destLocStr := ExpectString(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TOrderUnitInstruction.Create(APlayer, unitType, locStr, destLocStr, unitOrder));
    end else
    if TryToken(ALine,AIndex,'Kill') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, true));
    end else
    if TryToken(ALine,AIndex,'Remove') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TKillUnitInstruction.Create(APlayer, intVal, unitType, locStr, false));
    end else
    if TryToken(ALine,AIndex,'Give') then
    begin
      ExpectToken(ALine,AIndex,'(');
      destPl:= TryParsePlayer(ALine,AIndex);
      if destPl = plNone then raise exception.Create('Expecting player');
      ExpectToken(ALine,AIndex,')');

      AProg.Add(TGiveUnitInstruction.Create(APlayer, intVal, unitType, locStr, destPl));
    end else
    if TryToken(ALine,AIndex,'ToggleInvincibility') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,'(');

      AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvToggle));
    end else
    if TryToken(ALine,AIndex,'ToggleDoodadState') then
    begin
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,'(');

      AProg.Add(TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufDoodadState, ufvToggle));
    end else
    if TryToken(ALine,AIndex,'Invincible') then
    begin
      ExpectToken(ALine,AIndex,'=');
      conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvEnable),
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufInvincible, ufvDisable));
    end else
    if TryToken(ALine,AIndex,'DoodadState') then
    begin
      ExpectToken(ALine,AIndex,'=');
      conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufDoodadState, ufvEnable),
         TSetUnitFlagInstruction.Create(APlayer, unitType, locStr, sufDoodadState, ufvDisable));
    end else
    if TryToken(ALine,AIndex,'Burrowed') or
      TryToken(ALine,AIndex,'Cloaked') or
      TryToken(ALine,AIndex,'Hallucinated') or
      TryToken(ALine,AIndex,'Lifted') then
    begin
      raise exception.Create('This property cannot be changed after the unit is created');
    end else
    begin
      if TryToken(ALine,AIndex,'Life') then prop := supLife else
      if TryToken(ALine,AIndex,'Shield') then prop := supShield else
      if TryToken(ALine,AIndex,'Energy') then prop := supEnergy else
      if TryToken(ALine,AIndex,'Resource') then prop := supResource else
      if TryToken(ALine,AIndex,'HangarCount') then prop := supHangarCount else
        raise exception.Create('Expecting property name');
      ExpectToken(ALine,AIndex,'=');

      propVal:= ExpectIntegerConstant(AScope,ALine,AIndex);
      AProg.Add(TSetUnitPropertyInstruction.Create(APlayer, intVal, unitType, locStr, prop, propVal));
    end;

  end else
  begin
    if TryToken(ALine,AIndex,'CenterView') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      locStr := ExpectString(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TCenterViewInstruction.Create(locStr));
    end else
    if TryToken(ALine,AIndex,'MinimapPing') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      locStr := ExpectString(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TCenterViewInstruction.Create(locStr));
    end else
    if TryToken(ALine,AIndex,'Print') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      text := ExpectString(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TDisplayTextMessageInstruction.Create(true, text));
    end else
    if TryToken(ALine,AIndex,'TalkingPortrait') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      unitType := ExpectUnitType(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,',');
      timeMs := ExpectIntegerConstant(AScope,ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TTalkingPortraitInstruction.Create(unitType, timeMs));
    end else
    if TryToken(ALine,AIndex,'MissionObjectives') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'=');
      text := ExpectString(AScope,ALine,AIndex);
      AProg.Add(TSetMissionObjectivesInstruction.Create(text));
    end else
    if TryToken(ALine,AIndex,'Leaderboard') then
    begin
      ExpectToken(ALine,AIndex,'.');
      if TryToken(ALine,AIndex,'Computers') then
      begin
        ExpectToken(ALine,AIndex,'=');
        conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
        AppendConditionalInstruction(AProg, conds,
          TLeaderBoardIncludeComputersInstruction.Create(ufvEnable),
          TLeaderBoardIncludeComputersInstruction.Create(ufvDisable));
      end else
      if TryToken(ALine,AIndex,'ToggleComputers') then
      begin
        AProg.Add(TLeaderBoardIncludeComputersInstruction.Create(ufvToggle));
      end else
      if TryToken(ALine,AIndex,'Show') then
      begin
        ExpectToken(ALine,AIndex,'(');
        textDefined:= TryString(AScope,ALine,AIndex,text);
        if textDefined then ExpectToken(ALine,AIndex,',');

        if TryToken(ALine,AIndex,'MineralsAndGas') or
         TryToken(ALine,AIndex,'OreAndGas') then
        begin
          if not textDefined then
          begin
            intVal := MaxLongInt;
            if TryToken(ALine,AIndex,',') then
            begin
              if not TryIntegerConstant(AScope,ALine,AIndex,intVal) then
                raise exception.Create('Expecting integer value');
            end;
            AProg.Add(TShowLeaderboardOreAndGasIconInstruction.Create(intVal));
          end else
            AProg.Add(TShowLeaderboardResourceInstruction.Create('minerals and gas', srOreAndGas,-1));
        end else
        begin
          if TryInteger(AScope,ALine,AIndex,intVal) then
            ExpectToken(ALine,AIndex,'-')
          else intVal := -1;

          if TryToken(ALine,AIndex,'MineralsAndGas') or TryToken(ALine,AIndex,'OreAndGas') then
          begin
            if not textDefined then text := 'minerals and gas';
            AProg.Add(TShowLeaderboardResourceInstruction.Create(text, srOreAndGas, intVal));
          end else
          if TryToken(ALine,AIndex,'Minerals') or TryToken(ALine,AIndex,'Ore') then
          begin
            if not textDefined then text := 'minerals';
            AProg.Add(TShowLeaderboardResourceInstruction.Create(text, srOre, intVal));
          end else
          if TryToken(ALine,AIndex,'Gas') then
          begin
            if not textDefined then text := 'gas';
            AProg.Add(TShowLeaderboardResourceInstruction.Create(text, srGas, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitScore') then
          begin
            if not textDefined then text := 'unit score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssUnitScore, intVal));
          end else
          if TryToken(ALine,AIndex,'BuildingScore') then
          begin
            if not textDefined then text := 'building score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssBuildingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitAndBuildingScore') then
          begin
            if not textDefined then text := 'unit and building score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssUnitAndBuildingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'KillScore') then
          begin
            if not textDefined then text := 'kill score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssKillScore, intVal));
          end else
          if TryToken(ALine,AIndex,'RazingScore') then
          begin
            if not textDefined then text := 'razing score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssRazingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'KillAndRazingScore') then
          begin
            if not textDefined then text := 'kill and razing score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssKillAndRazingScore, intVal));
          end else
          if TryToken(ALine,AIndex,'TotalScore') then
          begin
            if not textDefined then text := 'total score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssTotalScore, intVal));
          end else
          if TryToken(ALine,AIndex,'CustomScore') then
          begin
            if not textDefined then text := 'custom score';
            AProg.Add(TShowLeaderboardScoreInstruction.Create(text, ssCustomScore, intVal));
          end else
          if TryToken(ALine,AIndex,'KillCount') then
          begin
            if TryToken(ALine,AIndex,'(') then
            begin
              unitType := ExpectUnitType(AScope,ALine,AIndex);
              ExpectToken(ALine,AIndex,')');
            end else
            begin
              unitType := suAnyUnit;
            end;
            if not textDefined then text := 'kills';
            AProg.Add(TShowLeaderboardKillCountInstruction.Create(text, unitType, intVal));
          end else
          if TryToken(ALine,AIndex,'UnitCount') then
          begin
            if TryToken(ALine,AIndex,'(') then
            begin
              unitType := ExpectUnitType(AScope,ALine,AIndex);
              if TryToken(ALine,AIndex,',') then
                locStr := ExpectString(AScope,ALine,AIndex)
              else
                locStr := '';
              ExpectToken(ALine,AIndex,')');
            end else
            begin
              unitType := suAnyUnit;
              locStr := '';
            end;
            if not textDefined then text := 'units';
            AProg.Add(TShowLeaderboardUnitCountInstruction.Create(text, unitType, locStr, intVal));
          end else
            raise exception.Create('Expecting sorting variable');
        end;
        ExpectToken(ALine,AIndex,')');
      end else
        raise exception.Create('Unknown field of leaderboard (Show, Computers, ToggleComputers)');

    end else
    if TryToken(ALine,AIndex,'Alliance') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      players := ExpectPlayers(ALine,AIndex);
      ExpectToken(ALine,AIndex,')');
      ExpectToken(ALine,AIndex,'=');
      ExpectToken(ALine,AIndex,'Alliance');
      ExpectToken(ALine,AIndex,'.');
      if TryToken(ALine,AIndex,'Ennemy') then alliance := asEnnemy
      else if TryToken(ALine,AIndex,'Ally') then alliance := asAlly
      else if TryToken(ALine,AIndex,'AlliedVictory') then alliance := asAlliedVictory
      else raise exception.Create('Expecting alliance status (Ennemy, Ally, AlliedVictory)');

      for pl := low(TPlayer) to high(TPlayer) do
        if pl in players then AProg.Add(TSetAllianceStatus.Create(pl, alliance));
    end else
    if TryToken(ALine,AIndex,'NextScenario') then
    begin
      raise exception.Create('Changing scenario cannot be done for a specific player');
    end else
    if TryToken(ALine,AIndex,'RunAIScript') then
    begin
      CheckCurrentPlayer;
      ExpectToken(ALine,AIndex,'(');
      filename := ExpectString(AScope,ALine,AIndex);
      if TryToken(ALine,AIndex,',') then
        locStr := ExpectString(AScope,ALine,AIndex)
      else
        locStr := '';
      ExpectToken(ALine,AIndex,')');
      AProg.Add(TRunAIScriptInstruction.Create(filename, locStr));
    end else
    if TryToken(ALine,AIndex,'Defeat') then
    begin
      CheckCurrentPlayer;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TEndGameInstruction.Create(egDefeat));
    end else
    if TryToken(ALine,AIndex,'Draw') then
    begin
      CheckCurrentPlayer;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TEndGameInstruction.Create(egDraw));
    end else
    if TryToken(ALine,AIndex,'Victory') then
    begin
      CheckCurrentPlayer;
      if TryToken(ALine,AIndex,'(') then ExpectToken(ALine,AIndex,')');
      AProg.Add(TEndGameInstruction.Create(egVictory));
    end else
    if TryToken(ALine,AIndex,'UnitSpeech') then
    begin
      ExpectToken(ALine,AIndex,'=');
      conds := ExpectConditions(AScope,ALine,AIndex,AThreads);
      AppendConditionalInstruction(AProg, conds,
        TUnitSpeechInstruction.Create(true),
        TUnitSpeechInstruction.Create(false));
    end else
      result := false;
  end;
end;

function ParseOption(ALine: TStringList): boolean;
var
  index: Integer;

  procedure CheckEndOfLine;
  begin
    if index <> ALine.Count then
      raise exception.Create('Expecting end of line');
  end;

begin
  index := 0;
  if TryToken(ALine,index,'Option') then
  begin
    if TryToken(ALine,index,'Hyper') then
    begin
      if TryToken(ALine,index,'On') then HyperTriggers:= true
      else If TryToken(ALine,index,'Off') then HyperTriggers:= false
      else raise exception.Create('Expecting On or Off');
    end else
      raise exception.Create('Unknown option');
    CheckEndOfLine;
    exit(true);
  end;
  exit(false);
end;

procedure ParseInstruction(AScope: integer; ALine: TStringList; AProg: TInstructionList; AThreads: TPlayers; AMainThread: TPlayer; AProcId: integer; AInSubNew: boolean);
var
  index, intVal, idxArr, i, idxSound, sw, idxVar, idxMsg: integer;
  params: TStringList;
  name, assignOp, text: String;
  done, boolVal, isPresent: boolean;
  scalar: TScalarVariable;
  conds: TConditionList;
  ints: ArrayOfInteger;
  sim: TSetIntegerMode;
  pl: TPlayer;
  expr: TExpression;
  bools: ArrayOfSwitchValue;

  procedure CheckEndOfLine;
  begin
    if index < ALine.Count then
      raise exception.Create('Expecting end of line but "' + ALine[index] + '" found');
  end;

begin
  if (ALine.Count = 0) or ((ALine.Count = 1) and (ALine[0] = '')) then exit;

  for index := 0 to ALine.Count-1 do
    if CompareText(ALine[index],'Present') = 0 then
    begin
      if (AThreads <> []) and (AThreads <> [AMainThread]) then
        raise exception.Create('"Present" is only valid in the main thread');
      done := false;
      for i := 0 to AProg.Count-1 do
        if AProg[i] is TWaitForPresenceDefinedInstruction then
        begin
          done := true;
          break;
        end;
      if not done then
        AProg.Add(TWaitForPresenceDefinedInstruction.Create);
    end;

  index := 0;
  if TryToken(ALine,index,'Return') then
  begin
    if AProcId <> -1 then
    begin
      if IsIntegerType(Procedures[AProcId].ReturnType) then
      begin
        if TryIntegerConstant(AScope,ALine,index,intVal) then
        begin
          if (intVal < 0) or (intVal >= 1 shl Procedures[AProcId].ReturnBitCount) then
            raise exception.Create('Value out of bounds')
          else
            AProg.Add( TTransferIntegerInstruction.Create(intVal, itCopyIntoAccumulator) );
        end
        else
        begin
          expr := TryExpression(AScope,ALine,index,true);
          if expr.CanPutInAccumulator then
          begin
            expr.AddToProgramInAccumulator(AProg);
            AProg.Add(TTransferIntegerInstruction.Create((1 shl Procedures[AProcId].ReturnBitCount)-1, itLimitAccumulator));
          end else
          begin
            idxVar := ProcedureReturnVar(AProcId);
            expr.AddToProgram(AProg, IntVars[idxVar].Player,IntVars[idxVar].UnitType, simSetTo);
            AProg.Add(TTransferIntegerInstruction.Create( IntVars[idxVar].Player,IntVars[idxVar].UnitType, itCopyIntoAccumulator));
          end;
          expr.Free;
        end;
      end else
      if Procedures[AProcId].ReturnType = 'Boolean' then
      begin
        if TryBoolean(AScope,ALine,index,boolVal) then
        begin
          idxVar := GetBoolResultVar;
          sw := BoolVars[idxVar].Switch;
          AProg.Add( TSetSwitchInstruction.Create(sw, BoolToSwitch[boolVal]) );
        end else
          raise exception.Create('Expecting boolean value');
      end;
    end;
    AProg.Add( TReturnInstruction.Create );
    CheckEndOfLine;
  end
  else
  if TryToken(ALine,index,'Exit') then
  begin
    if TryToken(ALine,index,'Sub') then
    begin
      if (AProcId = -1) and not AInSubNew then raise exception.Create('Not in a subroutine');
      if not AInSubNew and (Procedures[AProcId].ReturnType <> 'Void') then raise exception.Create('Currently in a function, not a subroutine');
      AProg.Add( TReturnInstruction.Create );
      CheckEndOfLine;
    end else
    if TryToken(ALine,index,'Function') then
    begin
      if AProcId = -1 then raise exception.Create('Not in a function');
      if Procedures[AProcId].ReturnType = 'Void' then raise exception.Create('Currently in a subroutine, not a function');
      raise exception.Create('Exit Function not allowed. Use Return to specify value');
    end else
      raise exception.Create('Unexpected instruction');
  end else
  if TryToken(ALine,index,'EndIf') then
  begin
    CheckEndOfLine;
    AProg.Add(TEndIfInstruction.Create);
  end else
  if TryToken(ALine,index,'End') then
  begin
    if TryToken(ALine,index,'While') then
    begin
      CheckEndOfLine;
      AProg.Add(TEndWhileInstruction.Create);
    end else
    if TryToken(ALine,index,'If') then
    begin
      CheckEndOfLine;
      AProg.Add(TEndIfInstruction.Create);
    end else
      raise exception.Create('Unknown end instruction');
  end else
  if TryToken(ALine,index,'While') then
  begin
    conds := ExpectConditions(AScope,ALine,index,AThreads);
    if (conds.Count = 1) and (conds[0] is TAlwaysCondition) then
      raise exception.Create('Infinite loop not allowed. You can use When True instead');
    AProg.Add(TWhileInstruction.Create(conds));
    CheckEndOfLine;
  end else
  if TryToken(ALine,index,'If') then
  begin
    conds := ExpectConditions(AScope,ALine,index,AThreads);
    AProg.Add(TIfInstruction.Create(conds));
    ExpectToken(ALine,index,'Then');
    CheckEndOfLine;
  end else
  if TryToken(ALine,index,'Else') then
  begin
    CheckEndOfLine;
    AProg.Add(TElseInstruction.Create);
  end else
  begin
    done := false;
    scalar := TryScalarVariable(AScope,ALine,index);
    if scalar.VarType <> svtNone then
    begin
      if TryToken(ALine,index,'=') then
      begin
        done := true;
        if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
        if scalar.ReadOnly then raise exception.Create('This value is read-only');
        case scalar.VarType of
        svtInteger: begin
            expr := TryExpression(AScope,ALine, index, true);
            if expr = nil then
              raise exception.Create('Unhandled case');
            expr.AddToProgram(AProg, scalar.Player, scalar.UnitType, simSetTo);
            expr.Free;
          end;
        svtSwitch:
          begin
            intVal := ParseRandom(ALine, index);
            if intVal > 0 then
            begin
              CheckEndOfLine;
              if intVal = 2 then
                AProg.Add(TSetSwitchInstruction.Create(scalar.Switch, svRandomize))
              else
                raise exception.Create('Boolean can have only 2 values');
            end else
            begin
              conds := ExpectConditions(AScope,ALine,index,AThreads);
              if (conds.Count = 1) and (conds[0] is TSwitchCondition) and
                 (TSwitchCondition(conds[0]).Switch = scalar.Switch) then
              begin
                //a = Not a
                if not TSwitchCondition(conds[0]).Value then
                  AProg.Add(TSetSwitchInstruction.Create(scalar.Switch, svToggle));
                conds[0].Free;
                conds.Free;
              end else
                AppendConditionalInstruction(AProg, conds,
                  TSetSwitchInstruction.Create(scalar.Switch, svSet),
                  TSetSwitchInstruction.Create(scalar.Switch, svClear));
            end;
          end;
        else raise exception.Create('Unhandled case');
        end;
      end else
      If TryToken(ALine,index,'+') or TryToken(ALine,index,'-') then
      begin
        assignOp := ALine[index-1];
        if TryToken(ALine,index,'=') then
        begin
          done := true;
          if scalar.Constant then raise exception.Create('Constant cannot be assigned to');
          if scalar.ReadOnly then raise exception.Create('This value is read-only');
          if scalar.VarType = svtInteger then
          begin
            expr := TryExpression(AScope,ALine,index, true);
            if assignOp='+' then
              expr.AddToProgram(AProg, scalar.Player, scalar.UnitType, simAdd)
            else if assignOp='-' then
              expr.AddToProgram(AProg, scalar.Player, scalar.UnitType, simSubtract);
            expr.Free;
          end
          else raise Exception.Create('Integer variables only can be incremented/decremented');
        end;
      end;

    end;

    idxSound := SoundIndexOf(AScope,ALine[0]);
    if idxSound <> -1 then
    begin
      index := 1;
      ExpectToken(ALine,index,'.');
      ExpectToken(ALine,index,'Play');
      if TryToken(ALine,index,'(') then ExpectToken(ALine,index,')');

      AProg.Add(TPlayWAVInstruction.Create(SoundVars[idxSound].Filename, SoundVars[idxSound].DurationMs));
      done := true;
    end;

    idxArr := IntArrayIndexOf(AScope,ALine[0]);
    if idxArr <> -1 then
    begin
      index := 1;
      If TryToken(ALine,index,'+') or TryToken(ALine,index,'-') then
        assignOp := ALine[index-1] else assignOp := '';

      if TryToken(ALine,index,'=') then
      begin
        done := true;
        if IntArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');

        ints := ParseIntArray(AScope,ALine,index);
        if length(ints) <> IntArrays[idxArr].Size then
          raise exception.Create('Array size mismatch');
        CheckEndOfLine;
        if assignOp = '+' then sim := simAdd
        else if assignOp = '-' then sim := simSubtract
        else sim := simSetTo;
        for i := 0 to high(ints) do
          with IntVars[IntArrays[idxArr].Vars[i]] do
          AProg.Add(CreateSetIntegerInstruction(Player, UnitType, sim, ints[i]));
      end;
    end;

    idxArr := BoolArrayIndexOf(AScope,ALine[0]);
    isPresent := CompareText(ALine[0],'Present')=0;
    if (idxArr <> -1) or isPresent then
    begin
      index := 1;
      if TryToken(ALine,index,'=') then
      begin
        done := true;
        if isPresent then raise exception.Create('Array is readonly');

        if BoolArrays[idxArr].Constant then raise exception.Create('Constant cannot be assigned to');
        if BoolArrays[idxArr].ReadOnly then raise exception.Create('Array is readonly');

        bools := ParseBoolArray(AScope,ALine,index);
        if length(bools) <> BoolArrays[idxArr].Size then
          raise exception.Create('Array size mismatch');
        CheckEndOfLine;
        for i := 0 to high(bools) do
          with BoolVars[BoolArrays[idxArr].Vars[i]] do
            AProg.Add(TSetSwitchInstruction.Create(Switch, bools[i]));
      end;
    end;

    if not done then
    begin
      index := 0;
      pl := TryParsePlayer(ALine,index);
      if pl <> plNone then
      begin
        done := true;

        ExpectToken(ALine,index,'.');
        if index >= ALine.Count then
          raise exception.Create('Expecting action but end of line found');

        if (pl <> plCurrentPlayer) and TryToken(ALine,index,'Print') then
        begin
          if (AThreads <> []) and (AThreads <> [AMainThread]) then
            raise exception.Create('Printing for any player is only possible from main thread');

          ExpectToken(ALine,index,'(');
          text := ExpectString(AScope,ALine,index);
          ExpectToken(ALine,index,')');
          idxMsg := FindOrCreateMessage(text, [pl]);
          AProg.Add( TPrintForAnyPlayerInstruction.Create(idxMsg) );

        end else
        if not TryPlayerAction(AScope,AProg,ALine,index,pl, AThreads) then
          raise exception.Create('Expecting action but "' + ALine[index] + '" found');
        CheckEndOfLine;
      end;
    end;

    index := 0;
    if TryNeutralAction(AScope,AProg,ALine,index, AThreads) then
    begin
      CheckEndOfLine;
      done := true;
    end;

    if TryPlayerAction(AScope,AProg,ALine,index,plCurrentPlayer, AThreads) then
    begin
      if AThreads = [] then
        raise exception.Create('You need to specify which players does the action ("Me" for main thread)');
      CheckEndOfLine;
      done := true;
    end;


    if not done then
    begin
      index := 0;
      name := '';
      while (index < ALine.Count) and IsValidVariableName(ALine[index]) do
      begin
        if index > 0 then name += ' ';
        name += ALine[index];
        inc(index);
      end;

      if (index = ALine.Count) or
        ((index < ALine.Count) and (ALine[index] = '(')) then
      begin
        params := TStringList.Create;
        if TryToken(ALine,index,'(') then
        begin
          while not TryToken(ALine,index,')') do
          begin
            if params.Count > 0 then ExpectToken(ALine,index,',');
            if TryToken(ALine,index,',') then
              raise exception.Create('Empty parameters not allowed');
            if index < ALine.Count then
              params.Add(ALine[index])
            else
              raise exception.Create('Parameter expected but end of line found');

            inc(index);
          end;
        end;
        CheckEndOfLine;

        AProg.Add(TCallInstruction.Create(name, params));
      end else
      if scalar.VarType = svtNone then
      begin
        if name = '' then raise exception.Create('Expecting identifier')
        else raise exception.Create('Unknown variable "' + name + '"')
      end
      else
        raise exception.Create('Expecting assignment');
    end;
  end;
end;

function RemoveTrailingCommentAndTabs(AText: string): string;
var inStr: boolean;
  i: Integer;
begin
  inStr := false;
  for i := 1 to length(AText) do
  begin
    if AText[i] = '"' then inStr := not inStr;
    if (AText[i] = '''') and not inStr then
      exit(copy(AText,1,i-1));
    if (AText[i] in[#0..#31]) and not inStr then AText[i] := ' ';
  end;
  exit(AText);
end;

function ReadProg(AFilename: string; out AMainThread: TPlayer): boolean;
var
  lines: TStringList;
begin
  lines := TStringList.Create;
  lines.LoadFromFile(AFilename);
  try
    result := ReadProg(lines, AMainThread);
  finally
    lines.free;
  end;
end;

procedure ClearProceduresAndEvents;
var
  i: Integer;
begin
  for i := 0 to ProcedureCount-1 do
    with Procedures[i] do
    begin
      Instructions.FreeAll;
      Calls.Free;
    end;
  ProcedureCount:= 0;

  for i := 0 to EventCount-1 do
    with Events[i] do
    begin
      Instructions.FreeAll;
      Conditions.FreeAll;
    end;
  EventCount:= 0;
end;

function IsTokenOverEndOfLine(ALastToken:string): boolean;
begin
  result := (ALastToken = '&') or (ALastToken = '+') or (ALastToken = '-') or (ALastToken = '*') or (ALastToken = '\')
         or (ALastToken = '=') or (ALastToken = ',')
         or (CompareText(ALastToken,'Or')=0) or (CompareText(ALastToken,'And')=0) or (CompareText(ALastToken,'Xor')=0);
end;

function ReadProg(ALines: TStrings; out AMainThread: TPlayer): boolean;
const MAX_ERRORS = 3;
var
  line: TStringList;
  index: integer;
  lineNumber: integer;

  function ReadOneLine: string;
  begin
    result := ALines[lineNumber];
    inc(lineNumber);
  end;

  function EOF: boolean;
  begin
    result := lineNumber >= ALines.Count;
  end;

  procedure ReadNextLine;
  var s, lastToken: string;
    extraLine: TStringList;
  begin
    s := ReadOneLine;
    if Assigned(line) then FreeAndNil(line);
    line := ParseLine(s);
    index := 0;
    while not EOF and (line.Count > 0) do
    begin
      lastToken := line[line.Count-1];
      if IsTokenOverEndOfLine(lastToken) then
      begin
        s := ReadOneLine;
        extraLine := ParseLine(s);
        if (extraLine.Count > 0) and (CompareText(extraLine[0],'End') <> 0) then
        begin
          line.AddStrings(extraLine);
          extraLine.Free;
        end
        else
        begin
          extraline.Free;
          dec(lineNumber);
          break;
        end;
      end else break;
    end;
  end;

  procedure CheckEndOfLine;
  begin
    if index <> line.Count then
      raise exception.Create('Expecting end of line');
  end;

var
  errorCount: integer;
  inSub, inEvent, i: integer;
  inSubNew, subNewDeclared, done, inDoAs: boolean;
  doPlayers: TPlayers;
  players: TPlayers;
  pl: TPlayer;
begin
  ReadProgErrors.Clear;
  ClearProceduresAndEvents;
  AMainThread := plNone;

  HyperTriggers := false;
  InitVariables;
  MainProg.FreeAll;
  MainProg := TInstructionList.Create;
  MainProg.Add(TCommentInstruction.Create('Sub New'));

  PredefineIntArray(GlobalScope,'Ore',suResourceOre,24);
  PredefineIntArray(GlobalScope,'Minerals',suResourceOre,24);
  PredefineIntArray(GlobalScope,'Gas',suResourceGas,24);
  PredefineIntArray(GlobalScope,'OreAndGas',suResourceOreAndGas,24);
  PredefineIntArray(GlobalScope,'MineralsAndGas',suResourceOreAndGas,24);
  PredefineIntArray(GlobalScope,'UnitScore',suScoreUnits,24);
  PredefineIntArray(GlobalScope,'BuildingScore',suScoreBuildings,24);
  PredefineIntArray(GlobalScope,'UnitAndBuildingScore',suScoreUnitsAndBuildings,24);
  PredefineIntArray(GlobalScope,'KillScore',suScoreKills,24);
  PredefineIntArray(GlobalScope,'RazingScore',suScoreRazings,24);
  PredefineIntArray(GlobalScope,'KillAndRazingScore',suScoreKillsAndRazings,24);
  PredefineIntArray(GlobalScope,'CustomScore',suScoreCustom,24);
  PredefineIntArray(GlobalScope,'TotalScore',suScoreTotal,24);
  PredefineIntVar(GlobalScope,'Countdown', plNone, suCountdown,16);
  ProcessDim(GlobalScope,0,'Const vbCr = Chr(13)',MainProg, False);
  ProcessDim(GlobalScope,0,'Const vbLf = Chr(10)',MainProg, False);
  ProcessDim(GlobalScope,0,'Const vbTab = Chr(9)',MainProg, False);
  ProcessDim(GlobalScope,0,'Const vbCrLf = vbCr & vbLf',MainProg, False);

  lineNumber:= 0;
  line := nil;

  errorCount := 0;
  inSub := -1;
  inEvent := -1;
  inSubNew := false;
  subNewDeclared := false;
  inDoAs := false;
  doPlayers := [];
  LastScope := GlobalScope;

  while not Eof and (errorCount < MAX_ERRORS) do
  begin
    try
      ReadNextLine;
      if line.Count = 0 then continue;

      if TryToken(line,index,'Dim') or TryToken(line,index,'Const') then
      begin
        if inEvent <> -1 then
          ProcessDim(Events[inEvent].InnerScope,lineNumber, line, Events[inEvent].Instructions, true)
        else if inSub <> -1 then
          ProcessDim(Procedures[inSub].InnerScope,lineNumber,line, Procedures[inSub].Instructions, true)
        else if inSubNew then
          ProcessDim(SubNewScope, lineNumber,line, MainProg, false)
        else
          ProcessDim(GlobalScope, lineNumber,line, MainProg, false);
      end
      else if TryToken(line,index, 'Sub') or TryToken(line,index,'Function') then
      begin
        if (inSub<>-1) or (inEvent <> -1) or inSubNew then
          raise exception.Create('Nested procedures not allowed');
        inSub := ProcessSub(line);
        if inSub = -1 then
        begin
          if subNewDeclared then raise exception.Create('Sub New already declared');
          inSubNew:= true;
          subNewDeclared := true;
        end;
      end
      else if (inEvent <> -1) and TryToken(line,index, 'Return') then
      begin
        if not Events[inEvent].Preserve then
          raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
        CheckEndOfLine;
        Events[inEvent].Instructions.Add(TReturnInstruction.Create);
      end
      else if TryToken(line,index, 'Stop') then
      begin
        if inEvent <> -1 then
        begin
          if Events[inEvent].Preserve then
          begin
            Events[inEvent].Preserve := false;
            for i := 0 to Events[inEvent].Instructions.Count-1 do
              if Events[inEvent].Instructions[i] is TReturnInstruction then
                raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
          end;
          CheckEndOfLine;
          Events[inEvent].Instructions.Add(TReturnInstruction.Create);
        end else
          raise exception.Create('Stop instruction only allowed in event');
      end
      else if (inSub = -1) and (inEvent = -1) and not inSubNew and TryToken(line,index,'As') then
      begin
        players := ExpectPlayers(line,index);
        if index < line.Count then
        begin
          for i := index-1 downto 0 do
            line.Delete(i);
          index := 0;
        end else
        begin
          if eof then raise exception.Create('End of file not expected');
          ReadNextLine;
        end;

        if TryToken(line,index,'Sub') or TryToken(line,index,'Function') then
        begin
          inSub := ProcessSub(line);
          if inSub <> -1 then
            raise exception.Create('You cannot specify the player for a sub or function except for Sub New');
          if subNewDeclared then raise exception.Create('Sub New already declared');
          inSubNew := true;
          subNewDeclared:= true;
          if not IsUniquePlayer(players) or (players = [plCurrentPlayer]) then
            raise exception.Create('If you specify a player for Sub New, it must be one specific player');
          for pl := plPlayer1 to plPlayer8 do
            if pl in players then AMainThread:= pl;
          if AMainThread = plNone then raise exception.Create('This player can''t be used as a main thread');
        end else
          inEvent := ProcessEvent(line, players, true)
      end
      else if TryToken(line,index,'When') then
      begin
        if (inSub<>-1) or (inEvent <> -1) or inSubNew then
          raise exception.Create('Nested events not allowed');
        inEvent := ProcessEvent(line, [], false)
      end else
      begin
        done := false;
        if TryToken(line,index,'End') then
        begin
          if TryToken(line,index,'Sub') then
          begin
            if inDoAs then raise exception.Create('Multi-thread instruction not finished');

            if inSubNew then inSubNew := false
            else
            begin
              if inSub= -1 then
                raise Exception.create('Not in a procedure');
              if Procedures[inSub].ReturnType <> 'Void' then raise exception.Create('Expecting "End Function"');
              with Procedures[inSub].Instructions do
                if (Count = 0) or not (Items[Count-1] is TReturnInstruction) then
                  Add(TReturnInstruction.Create);
              inSub := -1;
            end;
            CheckEndOfLine;
            done := true;
          end
          else if TryToken(line,index,'Function') then
          begin
            if inSub= -1 then
              raise Exception.create('Not in a function');
            if Procedures[inSub].ReturnType = 'Void' then raise exception.Create('Expecting "End Sub" instead');
            with Procedures[inSub].Instructions do
              if (Count = 0) or not (Items[Count-1] is TReturnInstruction) then
                Add(TReturnInstruction.Create);
            inSub := -1;
            CheckEndOfLine;
            done := true;
          end
          else if TryToken(line,index,'When') then
          begin
            if inDoAs then raise exception.Create('Multi-thread instruction not finished');

            if inEvent= -1 then
              raise Exception.create('Not in an event');
            with Events[inEvent] do
              while (Instructions.Count > 0) and (Instructions[Instructions.Count-1] is TReturnInstruction) do
              begin
                Instructions[Instructions.Count-1].Free;
                Instructions.Delete(Instructions.Count-1);
              end;
            inEvent := -1;
            CheckEndOfLine;
            done := true;
          end
          else if TryToken(line,index,'Do') then
          begin
            if inDoAs then
            begin
              inDoAs := false;
              if inSubNew then
                MainProg.Add(TEndDoAsInstruction.Create(doPlayers) )
              else
              if (inEvent<>-1) then
                Events[inEvent].Instructions.Add(TEndDoAsInstruction.Create(doPlayers) );
            end
            else raise exception.Create('Unexpected end of block');
            done := true;
          end;
        end else
        if TryToken(line,index,'Do') and TryToken(line,index,'As') then
        begin
          if inDoAs then raise exception.Create('Nested multi-thread instruction not allowed');

          doPlayers := ExpectPlayers(line,index);
          if (plAllPlayers in doPlayers) or ([plForce1,plForce2,plForce3,plForce4] <= doPlayers) then
            doPlayers := [plPlayer1,plPlayer2,plPlayer3,plPlayer4,
                      plPlayer5,plPlayer6,plPlayer7,plPlayer8];

          if inEvent<>-1 then
          begin
            if (Events[inEvent].Players <> []) and (Events[inEvent].Players <> [AMainThread]) then
              raise exception.Create('Multi-thread instruction only allowed in main thread');
            Events[inEvent].Instructions.Add(TDoAsInstruction.Create(doPlayers));
          end else
          if inSubNew then
          begin
            MainProg.Add(TDoAsInstruction.Create(doPlayers));
          end
          else
            raise exception.Create('Multi-thread instructions must be in the Sub New or in an event of the main thread');

          CheckEndOfLine;

          inDoAs := true;
          done := true;
        end;

        if not done then
        begin
          if inSub<>-1 then
            ParseInstruction(Procedures[inSub].InnerScope, line, Procedures[inSub].Instructions,[plAllPlayers], AMainThread, inSub,false)
          else if inEvent<>-1 then
          begin
            if inDoAs then
              ParseInstruction(Events[inEvent].InnerScope, line, Events[inEvent].Instructions, doPlayers, AMainThread, -1, false)
            else
              ParseInstruction(Events[inEvent].InnerScope, line, Events[inEvent].Instructions, Events[inEvent].Players, AMainThread, -1, false);
          end
          else if inSubNew then
          begin
            if inDoAs then
              ParseInstruction(SubNewScope, line, MainProg, doPlayers, AMainThread, -1, true)
            else if AMainThread = plNone then
              ParseInstruction(SubNewScope, line, MainProg, [], AMainThread, -1, true)
            else
              ParseInstruction(SubNewScope, line, MainProg, [AMainThread], AMainThread, -1, true);
          end
          else
            if not ParseOption(line) then
              raise exception.Create('Unexpected instruction. Please put initialization code into Sub New.');
        end;
      end;
    except
      on ex:Exception do
      begin
        ReadProgErrors.Add('Line ' + inttostr( lineNumber) + ': ' + ex.Message);
        errorCount += 1;
      end;
    end;
  end;

  line.Free;

  if errorCount < MAX_ERRORS then
  begin
    if inSub<>-1 then
    begin
      ReadProgErrors.Add('Line ' + inttostr( lineNumber) + ': Sub or Function not finished');
      Inc(errorCount);
      LastScope:= Procedures[inSub].InnerScope;
    end;

    if inEvent<>-1 then
    begin
      ReadProgErrors.Add('Line ' + inttostr( lineNumber) + ': event not finished');
      Inc(errorCount);
      LastScope:= Events[inEvent].InnerScope;
    end;

    if inSubNew then
    begin
      ReadProgErrors.Add('Line ' + inttostr( lineNumber) + ': Sub not finished');
      Inc(errorCount);
      LastScope:= SubNewScope;
    end;
  end;

  result := errorCount = 0;
end;

var i: integer;

initialization

  MainProg := TInstructionList.Create;
  ReadProgErrors := TStringList.Create;

finalization

  for i := 0 to MainProg.Count-1 do
    MainProg[i].Free;

  ReadProgErrors.Free;
  MainProg.Free;

  ClearProceduresAndEvents;

end.

