unit uexpressions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, uinstructions, fgl;

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

function ExpectString(AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;
function ExpectUnitType({%H-}AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
function IsUnitType(AName: string): boolean;
function TryIdentifier(ALine: TStringList; var AIndex: integer; out AIdentifier: string; AcceptsReservedWords: boolean): boolean;
function TryInteger(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
function TryIntegerConstant(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: integer): boolean;
function ExpectInteger(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function ExpectIntegerConstant(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
function TryBoolean(AScope: integer; ALine: TStringList; var AIndex: integer; out AValue: boolean): boolean;
function TryScalarVariable(AScope: integer; ALine: TStringList; var AIndex: integer): TScalarVariable;
function TryString(AScope: integer; ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;

type
  { TExpressionNode }

  TExpressionNode = class
    Negative: boolean;
    constructor Create(ANegative: boolean);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); virtual; abstract;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); virtual; abstract;
    function AlwaysClearAccumulator: boolean; virtual; abstract;
    function BitCount: integer; virtual; abstract;
    function Duplicate: TExpressionNode; virtual; abstract;
  end;

  TExpressionNodeList = specialize TFPGList<TExpressionNode>;

  { TConstantNode }

  TConstantNode = class(TExpressionNode)
    Value: integer;
    constructor Create(ANegative: boolean; AValue: integer);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;


  { TVariableNode }

  TVariableNode = class(TExpressionNode)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    constructor Create(ANegative: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TFunctionCallNode }

  TFunctionCallNode = class(TExpressionNode)
    Name: string;
    constructor Create(ANegative: boolean; AName: string);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TCountIfBoolNode }

  TCountIfBoolNode = class(TExpressionNode)
    BoolArray: integer;
    TestValue: boolean;
    constructor Create(ANegative: boolean; ABoolArray: integer; ATestValue: boolean);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TCountIfIntNode }

  TCountIfIntNode = class(TExpressionNode)
    IntArray: integer;
    TestValue: integer;
    constructor Create(ANegative: boolean; AIntArray: integer; ATestValue: integer);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TRandomNode }

  TRandomNode = class(TExpressionNode)
    Range: integer;
    constructor Create(ANegative: boolean; ARange: integer);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TExpression }

  TExpression = class
  private
    function GetCanPutInAccumulator: boolean;
    function GetIsConstant: boolean;
    function GetMaxBitCount: integer;
    function GetNegativeCount: integer;
    function GetPositiveCount: integer;
    procedure PutClearAccFirst;
  public
    Elements: TExpressionNodeList;
    ConstElement: integer;
    destructor Destroy; override;
    constructor Create;
    constructor Create(ANode: TExpressionNode);
    procedure NegateAll;
    procedure AddToProgram(AProg: TInstructionList;
                     ADestPlayer: TPlayer; ADestUnitType: TStarcraftUnit;
                     AMode: TSetIntegerMode);
    procedure AddToProgramInAccumulator(AProg: TInstructionList);
    property CanPutInAccumulator: boolean read GetCanPutInAccumulator;
    property NegativeCount: integer read GetNegativeCount;
    property PositiveCount: integer read GetPositiveCount;
    property IsConstant: boolean read GetIsConstant;
    property MaxBitCount: integer read GetMaxBitCount;
    function Duplicate: TExpression;
  end;

  { TMultiplyNode }

  TMultiplyNode = class(TExpressionNode)
    Expr: TExpression;
    Factor: integer;
    destructor Destroy; override;
    constructor Create(AExpr: TExpression; AFactor: integer);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function FactorBitSize: integer;
    function IsFewTimesVar: boolean;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TSubExpression }

  TSubExpression = class(TExpressionNode)
    Expr: TExpression;
    destructor Destroy; override;
    constructor Create(AExpr: TExpression);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TArithmeticCondition }

  TArithmeticCondition = class(TCondition)
    Expression: TExpression;
    CompareMode: TIntegerConditionMode;
    CompareValue: integer;
    destructor Destroy; override;
    constructor Create(AExpression: TExpression; ACompareMode: TIntegerConditionMode; ACompareValue: integer);
    function IsArithmetic: Boolean; override;
    function GetBitCount: integer;
    function Duplicate: TCondition; override;
  end;

function TryExpression(AScope: integer; ALine: TStringList; var AIndex: integer; ARaiseException: boolean; AAcceptCalls: boolean = true): TExpression;

implementation

uses uparsevb, uvariables, uarithmetic, utriggerconditions;

function ExpectUnitType(AScope: integer; ALine: TStringList; var AIndex: integer): TStarcraftUnit;
var
  u: TStarcraftUnit;
  ident: string;
begin
  ExpectToken(ALine,AIndex,'Unit');
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
        s := ExpectString(AScope, ALine,AIndex);
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
        strValue := ExpectString(AScope, ALine,AIndex);
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

function TryIntegerConstant(AScope: integer; ALine: TStringList; var AIndex: integer; out
  AValue: integer): boolean;
var
  expr: TExpression;
begin
  expr := TryExpression(AScope, ALine,AIndex,false,false);
  if expr = nil then
  begin
    result := TryInteger(AScope, ALine,AIndex,AValue);
    exit;
  end;
  if expr.Elements.Count > 0 then
  begin
    expr.Free;
    AValue := 0;
    exit(false);
  end;
  if expr.ConstElement < 0 then
  begin
    expr.Free;
    AValue := 0;
    exit(false);
  end;
  AValue := expr.ConstElement;
  expr.Free;
  exit(true);
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

function ExpectIntegerConstant(AScope: integer; ALine: TStringList; var AIndex: integer): integer;
var
  expr: TExpression;
begin
  expr := TryExpression(AScope, ALine,AIndex,false,false);
  if expr = nil then
  begin
     if AIndex >= ALine.Count then
      raise exception.Create('Integer expected but end of line found') else
      raise exception.Create('Integer expected but "' + ALine[AIndex] + '" found');
    exit(0);
  end;
  if expr.Elements.Count > 0 then
  begin
    expr.Free;
    raise exception.Create('Expression is not constant');
  end;
  if expr.ConstElement < 0 then
  begin
    expr.Free;
    raise exception.Create('Expression is negative');
  end;
  result := expr.ConstElement;
  expr.Free;
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

function TryString(AScope: integer; ALine: TStringList; var AIndex: integer; out AStr: string; ARaiseException: boolean = false): boolean;
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

function TryExpression(AScope: integer; ALine: TStringList; var AIndex: integer; ARaiseException: boolean; AAcceptCalls: boolean): TExpression;
var
  intValue: integer;
  neg, boolVal: boolean;
  idx, rnd, idxVar, intVal, j: integer;
  scalar: TScalarVariable;
  name: string;
  i: LongInt;
  node: TExpressionNode;
  subExpr: TExpression;

  function ParseSimpleNode: TExpressionNode;
  var
    expr: TExpression;
  begin
    result := nil;
    if TryToken(ALine,idx,'(') then
    begin
      expr := TryExpression(AScope, ALine,idx, ARaiseException, AAcceptCalls);
      if expr = nil then
      begin
        if ARaiseException then raise exception.Create('Expecting integer value');
        exit;
      end;
      ExpectToken(ALine,idx,')');
      result := TSubExpression.Create(expr);
    end else
    if TryInteger(AScope, ALine,idx,intValue) then
    begin
      result := TConstantNode.Create(neg, intValue);
    end else
    begin
      rnd := ParseRandom(ALine,idx);
      if rnd <> -1 then
        result := TRandomNode.Create(neg, rnd) else
      begin
        scalar:= TryScalarVariable(AScope, ALine,idx);
        case scalar.VarType of
        svtInteger: result := TVariableNode.Create(neg,scalar.Player,scalar.UnitType);
        svtSwitch:
          begin
            if ARaiseException then
              raise exception.Create('Expecting integer value but boolean found');
            exit;
          end
        else
          begin
            if TryToken(ALine,idx,'CountIf') then
            begin
              ExpectToken(ALine,idx,'(');
              if not TryIdentifier(ALine,idx,name, false) then
                raise exception.Create('Array identifier expected');
              ExpectToken(ALine,idx,',');

              if name = 'Present' then
                idxVar := GetPlayerPresentArray
              else
                idxVar := BoolArrayIndexOf(AScope, name);

              if idxVar <> -1 then
              begin
                if not TryBoolean(AScope, ALine,idx, boolVal) then
                  raise exception.Create('Boolean constant expected');
                result := TCountIfBoolNode.Create(neg, idxVar, boolVal);
              end else
              begin
                idxVar := IntArrayIndexOf(AScope, name);
                if idxVar <> -1 then
                begin
                  if not TryIntegerConstant(AScope, ALine,idx, intVal) then
                    raise exception.Create('Integer constant expected');
                  result := TCountIfIntNode.Create(neg, idxVar, intVal);
                end else
                  raise exception.Create('Unknown array variable "' + name + '"');
              end;
              ExpectToken(ALine,idx,')');
            end else
            begin
              if TryIdentifier(ALine,idx, name, false) then
              begin
                idxVar := StringIndexOf(AScope,name);
                if idxVar = -1 then idxVar := StringArrayIndexOf(AScope,name);
                if idxVar <> -1 then
                begin
                  if ARaiseException then
                    raise exception.Create('String is not valid in integer expression');
                  exit;
                end;

                if AAcceptCalls then  //function call?
                begin
                  if TryToken(ALine,idx,'(') then ExpectToken(ALine,idx,')');
                  result := TFunctionCallNode.Create(neg, name);
                end else
                begin
                  if ARaiseException then
                    raise exception.Create('Integer expected');
                  exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure MultiplyByConst(AValue: integer);
  var
    lastNode: TExpressionNode;
    mult: TMultiplyNode;
  begin
    lastNode := result.Elements[result.Elements.Count-1];
    if AValue < 0 then
    begin
      AValue := -AValue;
      lastNode.Negative := not lastNode.Negative;
    end;
    if AValue = 0 then
    begin
      lastNode.Free;
      result.Elements[result.Elements.Count-1] := TConstantNode.Create(false,0);
    end else
    if AValue <> 1 then
    begin
      if lastNode is TConstantNode then
      begin
        TConstantNode(lastNode).Value *= AValue;
      end else
      begin
        if lastNode is TSubExpression then
        begin
          mult := TMultiplyNode.Create(TSubExpression(lastNode).Expr, AValue);
          TSubExpression(lastNode).Expr := nil;
          lastNode.Free;
        end
        else
          mult := TMultiplyNode.Create(TExpression.Create(lastNode), AValue);
        result.Elements[result.Elements.Count-1] := mult;
      end;
    end;
  end;

  procedure MultiplyByExpr(AExpr: TExpression);
  var
    lastNode: TExpressionNode;
    mult: TMultiplyNode;
  begin
    lastNode := result.Elements[result.Elements.Count-1];
    if not (lastNode is TConstantNode) then
      raise exception.Create('One term of a multiplication must be a constant');

    mult := TMultiplyNode.Create(AExpr, TConstantNode(lastNode).Value);
    mult.Negative := lastNode.Negative;
    lastNode.Free;
    result.Elements[result.Elements.Count-1] := mult;
  end;

begin
  result := TExpression.Create;
  try
    idx := AIndex;
    neg := false;
    if TryToken(ALine,idx,'+') then neg := false
    else if TryToken(ALine,idx,'-') then neg := true;
    repeat
      node := ParseSimpleNode;
      if node = nil then
      begin
        if ARaiseException then
          raise exception.Create('Expression expected');
        FreeAndNil(result);
        exit;
      end;
      result.Elements.Add(node);

      while TryToken(ALine,idx,'*') do
      begin
        if TryToken(ALine,idx,'(') then
        begin
          subExpr := TryExpression(AScope, ALine,idx,ARaiseException,AAcceptCalls);
          ExpectToken(ALine,idx,')');
          if subExpr.IsConstant then
          begin
            MultiplyByConst(subExpr.ConstElement);
            subExpr.Free;
          end else
            MultiplyByExpr(subExpr);
        end else
        begin
          neg := false;
          if TryToken(ALine,idx,'+') then neg := false
          else if TryToken(ALine,idx,'-') then neg := true;

          node := ParseSimpleNode;
          if node = nil then
          begin
            result.Free;
            raise exception.Create('Expecting second term of multiplication');
          end;
          if node is TConstantNode then
          begin
            if TConstantNode(node).Negative then
              MultiplyByConst(-TConstantNode(node).Value)
            else
              MultiplyByConst(TConstantNode(node).Value);
            node.Free;
          end else
          begin
            if node is TSubExpression then
            begin
              MultiplyByExpr(TSubExpression(node).Expr);
              TSubExpression(node).Expr := nil;
              node.Free;
            end else
              MultiplyByExpr(TExpression.Create(node));
          end;
        end;
      end;

      if TryToken(ALine,idx,'+') then neg := false
      else if TryToken(ALine,idx,'-') then neg := true
      else break;
    until false;

    for i := result.Elements.Count-1 downto 0 do
      if result.Elements[i] is TSubExpression then
      begin
        with TSubExpression(result.Elements[i]) do
        begin
          if Negative then
          begin
            Expr.NegateAll;
            Negative := false;
          end;
          for j := 0 to Expr.Elements.Count-1 do
            result.Elements.Add(Expr.Elements[j]);
          Expr.Elements.Clear;
          result.ConstElement += Expr.ConstElement;
        end;
        result.Elements[i].Free;
        result.Elements.Delete(i);
      end;

    for i := result.Elements.Count-1 downto 0 do
      if result.Elements[i] is TConstantNode then
      begin
        if TConstantNode(result.Elements[i]).Negative then
          result.ConstElement -= TConstantNode(result.Elements[i]).Value
        else
          result.ConstElement += TConstantNode(result.Elements[i]).Value;
        result.Elements[i].Free;
        result.Elements.Delete(i);
      end;

    AIndex := idx;

    result.PutClearAccFirst;
  except
    on ex:exception do
    begin
      FreeAndNil(result);
      raise exception.Create(ex.Message);
    end;
  end;
end;

function ExpectString(AScope: integer; ALine: TStringList; var AIndex: integer; AConvertToString: boolean = false): string;
var
  intVal: integer;
  boolVal: boolean;
begin
  if not TryString(AScope, ALine,AIndex,result,True) then
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

{ TSubExpression }

destructor TSubExpression.Destroy;
begin
  Expr.Free;
  inherited Destroy;
end;

constructor TSubExpression.Create(AExpr: TExpression);
begin
  Expr := AExpr;
end;

procedure TSubExpression.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
begin
  if AClearAcc then
    Expr.AddToProgramInAccumulator(AProg)
  else
    raise exception.Create('Not handled');
end;

procedure TSubExpression.LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer;
  AUnitType: TStarcraftUnit; AProg: TInstructionList);
begin
  if AClearVar then
    Expr.AddToProgram(AProg,APlayer,AUnitType, simSetTo)
  else
    Expr.AddToProgram(AProg,APlayer,AUnitType, simAdd);
end;

function TSubExpression.AlwaysClearAccumulator: boolean;
begin
  result := true;
end;

function TSubExpression.BitCount: integer;
begin
  result := Expr.MaxBitCount;
end;

function TSubExpression.Duplicate: TExpressionNode;
begin
  result := TSubExpression.Create(Expr.Duplicate);
end;

{ TMultiplyNode }

destructor TMultiplyNode.Destroy;
begin
  Expr.Free;
  inherited Destroy;
end;

constructor TMultiplyNode.Create(AExpr: TExpression; AFactor: integer);
begin
  Expr := AExpr;
  Factor:= AFactor;
end;

procedure TMultiplyNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
var
  multiplicand, bit, i: Integer;
begin
  if IsFewTimesVar then
  begin
    if AClearAcc then AProg.Add(TTransferIntegerInstruction.Create(0, itCopyIntoAccumulator));
    for i := 1 to factor do
      Expr.Elements[0].LoadIntoAccumulator(False, AProg);
    if Expr.ConstElement <> 0 then
      AProg.Add(TTransferIntegerInstruction.Create(Expr.ConstElement*factor, itAddIntoAccumulator));
  end else
  begin
    if AClearAcc then
    begin
      if Expr.CanPutInAccumulator then
        Expr.AddToProgramInAccumulator(AProg)
      else
      begin
        multiplicand := GetMultiplicandIntArray(Expr.MaxBitCount);
        Expr.AddToProgram(AProg, plCurrentPlayer, IntArrays[multiplicand].UnitType, simSetTo);
        AProg.Add(TTransferIntegerInstruction.Create(plCurrentPlayer, IntArrays[multiplicand].UnitType, itCopyIntoAccumulator));
      end;

      multiplicand := GetMultiplicandIntArray(BitCount);
      AProg.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[multiplicand].UnitType, simSetTo,0));
      if factor <= 3 then
      begin
        for i := 1 to factor do
          AProg.Add(TTransferIntegerInstruction.Create(plCurrentPlayer, IntArrays[multiplicand].UnitType, itAddAccumulator, 0));
      end else
      begin
        for bit := 23 downto 0 do
          if (factor and (1 shl bit)) <> 0 then
            AProg.Add(TTransferIntegerInstruction.Create(plCurrentPlayer, IntArrays[multiplicand].UnitType, itAddAccumulator, bit));
      end;
      AProg.Add(TTransferIntegerInstruction.Create(plCurrentPlayer, IntArrays[multiplicand].UnitType, itCopyIntoAccumulator));

    end else
      raise exception.Create('Case not handled');
  end;
end;

procedure TMultiplyNode.LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer;
  AUnitType: TStarcraftUnit; AProg: TInstructionList);
var
  multiplicand, bit, i: Integer;
begin
  if Expr.CanPutInAccumulator then
    Expr.AddToProgramInAccumulator(AProg)
  else
  begin
    multiplicand := GetMultiplicandIntArray(Expr.MaxBitCount);
    Expr.AddToProgram(AProg, plCurrentPlayer, IntArrays[multiplicand].UnitType, simSetTo);
    AProg.Add(TTransferIntegerInstruction.Create(plCurrentPlayer, IntArrays[multiplicand].UnitType, itCopyIntoAccumulator));
  end;

  if AClearVar then
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType, simSetTo,0));
  if factor <= 3 then
  begin
    for i := 1 to factor do
      AProg.Add(TTransferIntegerInstruction.Create(APlayer,AUnitType, itAddAccumulator, 0));
  end else
  for bit := 23 downto 0 do
    if (factor and (1 shl bit)) <> 0 then
      AProg.Add(TTransferIntegerInstruction.Create(APlayer,AUnitType, itAddAccumulator, bit));
end;

function TMultiplyNode.FactorBitSize: integer;
var
  i: Integer;
begin
  for i := 1 to 23 do
    if (1 shl i) > abs(Factor) then exit(i);
  exit(24);
end;

function TMultiplyNode.IsFewTimesVar: boolean;
begin
  result := (Expr.Elements.Count = 1) and (Expr.Elements[0] is TVariableNode) and (factor <= 3);
end;

function TMultiplyNode.AlwaysClearAccumulator: boolean;
begin
  result := not IsFewTimesVar;
end;

function TMultiplyNode.BitCount: integer;
begin
  result := FactorBitSize + Expr.MaxBitCount;
  if result > 24 then result := 24;
end;

function TMultiplyNode.Duplicate: TExpressionNode;
begin
  result := TMultiplyNode.Create(Expr.Duplicate,Factor);
end;

{ TConstantNode }

constructor TConstantNode.Create(ANegative: boolean; AValue: integer);
begin
  Value := AValue;
  Negative:= ANegative;
end;

procedure TConstantNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
begin
  if AClearAcc then
    AProg.Add(TTransferIntegerInstruction.Create(Value, itCopyIntoAccumulator))
  else
    AProg.Add(TTransferIntegerInstruction.Create(Value, itAddIntoAccumulator));
end;

procedure TConstantNode.LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer;
  AUnitType: TStarcraftUnit; AProg: TInstructionList);
begin
  if AClearVar then
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType, simSetTo, Value))
  else
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType, simAdd, Value));
end;

function TConstantNode.AlwaysClearAccumulator: boolean;
begin
  result := false;
end;

function TConstantNode.BitCount: integer;
begin
  if Value >= 1 shl 16 then result := 24
  else if Value >= 1 shl 8 then result := 16
  else result := 8;
end;

function TConstantNode.Duplicate: TExpressionNode;
begin
  result := TConstantNode.Create(Negative, Value);
end;

{ TArithmeticCondition }

destructor TArithmeticCondition.Destroy;
begin
  Expression.Free;
  inherited Destroy;
end;

constructor TArithmeticCondition.Create(AExpression: TExpression;
  ACompareMode: TIntegerConditionMode; ACompareValue: integer);
begin
  Expression:= AExpression;
  CompareMode:= ACompareMode;
  CompareValue := ACompareValue;
end;

function TArithmeticCondition.IsArithmetic: Boolean;
begin
  Result:= true;
end;

function TArithmeticCondition.GetBitCount: integer;
begin
  result := Expression.MaxBitCount;
end;

function TArithmeticCondition.Duplicate: TCondition;
begin
  result := TArithmeticCondition.Create(Expression.Duplicate, CompareMode, CompareValue);
end;

{ TCountIfIntNode }

constructor TCountIfIntNode.Create(ANegative: boolean; AIntArray: integer;
  ATestValue: integer);
begin
  Negative := ANegative;
  IntArray := AIntArray;
  TestValue := ATestValue;
end;

procedure TCountIfIntNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
var
  proc: TInstructionList;
  i: Integer;
begin
  if AClearAcc then
    AProg.Add(TTransferIntegerInstruction.Create(0,itCopyIntoAccumulator));

  with IntArrays[IntArray] do
    for i := 0 to size-1 do
    begin
      proc := TInstructionList.Create;
      proc.Add(TTransferIntegerInstruction.Create(1,itAddIntoAccumulator));
      AProg.Add(TFastIfInstruction.Create( [CreateIntegerCondition(IntVars[Vars[i]].Player, IntVars[Vars[i]].UnitType, icmExactly, TestValue)], proc));
    end;
end;

procedure TCountIfIntNode.LoadIntoVariable(AClearVar: boolean;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList);
var
  proc: TInstructionList;
  i: Integer;
begin
  if AClearVar then
    AProg.Add( CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0) );

  with IntArrays[IntArray] do
    for i := 0 to size-1 do
    begin
      proc := TInstructionList.Create;
      proc.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simAdd,1));
      AProg.Add(TFastIfInstruction.Create( [CreateIntegerCondition(IntVars[Vars[i]].Player, IntVars[Vars[i]].UnitType, icmExactly, TestValue)], proc));
    end;
end;

function TCountIfIntNode.AlwaysClearAccumulator: boolean;
begin
  result := False;
end;

function TCountIfIntNode.BitCount: integer;
begin
  result := 8;
end;

function TCountIfIntNode.Duplicate: TExpressionNode;
begin
  result := TCountIfIntNode.Create(Negative,IntArray,TestValue);
end;

{ TCountIfBoolNode }

constructor TCountIfBoolNode.Create(ANegative: boolean; ABoolArray: integer;
  ATestValue: boolean);
begin
  Negative := ANegative;
  BoolArray := ABoolArray;
  TestValue := ATestValue;
end;

procedure TCountIfBoolNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
var
  proc: TInstructionList;
  i: Integer;
begin
  if AClearAcc then
    AProg.Add(TTransferIntegerInstruction.Create(0,itCopyIntoAccumulator));

  with BoolArrays[BoolArray] do
    for i := 0 to size-1 do
    begin
      proc := TInstructionList.Create;
      proc.Add(TTransferIntegerInstruction.Create(1,itAddIntoAccumulator));
      AProg.Add(TFastIfInstruction.Create( [TSwitchCondition.Create(BoolVars[Vars[i]].Switch, TestValue)], proc));
    end;
end;

procedure TCountIfBoolNode.LoadIntoVariable(AClearVar: boolean;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList);
var
  proc: TInstructionList;
  i: Integer;
begin
  if AClearVar then
    AProg.Add( CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0) );

  with BoolArrays[BoolArray] do
    for i := 0 to size-1 do
    begin
      proc := TInstructionList.Create;
      proc.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simAdd,1));
      AProg.Add(TFastIfInstruction.Create( [TSwitchCondition.Create(BoolVars[Vars[i]].Switch, TestValue)], proc));
    end;
end;

function TCountIfBoolNode.AlwaysClearAccumulator: boolean;
begin
  result := false;
end;

function TCountIfBoolNode.BitCount: integer;
begin
  result := 8;
end;

function TCountIfBoolNode.Duplicate: TExpressionNode;
begin
  result :=TCountIfBoolNode.Create(Negative,BoolArray,TestValue);
end;

{ TExpression }

function TExpression.GetCanPutInAccumulator: boolean;
var
  i: Integer;
begin
  for i := 0 to Elements.Count-1 do
    if ((i > 0) and (Elements[i].AlwaysClearAccumulator)) or Elements[i].Negative then exit(false);
  exit(true);
end;

function TExpression.GetIsConstant: boolean;
begin
  result := (PositiveCount = 0) and (NegativeCount = 0);
end;

function TExpression.GetMaxBitCount: integer;
var
  i: Integer;
begin
  result := 8;
  for i := 0 to Elements.Count-1 do
    if Elements[i].BitCount > result then
      result := Elements[i].BitCount;
end;

function TExpression.GetNegativeCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Elements.Count-1 do
    if Elements[i].Negative then result += 1;
end;

function TExpression.GetPositiveCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Elements.Count-1 do
    if not Elements[i].Negative then result += 1;
end;

procedure TExpression.PutClearAccFirst;
var
  i: Integer;
  clearAccPos: integer;
begin
  clearAccPos := 0;
  for i := 0 to Elements.Count-1 do
    if Elements[i].AlwaysClearAccumulator then
    begin
      if i > clearAccPos then
        Elements.Move(i, clearAccPos);
      inc(clearAccPoS);
    end;
end;

destructor TExpression.Destroy;
var
  i: Integer;
begin
  for i := 0 to Elements.Count-1 do
    Elements[i].Free;
  Elements.Free;
  inherited Destroy;
end;

constructor TExpression.Create;
begin
  Elements := TExpressionNodeList.Create;
  ConstElement := 0;
end;

constructor TExpression.Create(ANode: TExpressionNode);
begin
  if ANode is TSubExpression then
  begin
    Elements := TSubExpression(ANode).Expr.Elements;
    ConstElement := TSubExpression(ANode).Expr.ConstElement;
    if ANode.Negative then NegateAll;
    TSubExpression(ANode).Expr := nil;
    ANode.Free;
  end else
  begin
    Elements := TExpressionNodeList.Create;
    Elements.Add(ANode);
    ConstElement := 0;
  end;
end;

procedure TExpression.NegateAll;
var
  i: Integer;
begin
  for i := 0 to Elements.Count-1 do
    Elements[i].Negative := not Elements[i].Negative;
  ConstElement:= -ConstElement;
end;

procedure TExpression.AddToProgram(AProg: TInstructionList;
  ADestPlayer: TPlayer; ADestUnitType: TStarcraftUnit; AMode: TSetIntegerMode);
var
  i: LongInt;
  firstElem: Boolean;
  removedElem: TExpressionNode;
  bitCount: integer;
begin
  if AMode = simSubtract then
  begin
    NegateAll;
    AddToProgram(AProg, ADestPlayer, ADestUnitType, simAdd);
    NegateAll;
    exit;
  end;

  bitCount := 0;
  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Player = ADestPlayer) and (IntVars[i].UnitType = ADestUnitType) then
    begin
      bitCount:= IntVars[i].BitCount;
    end;
  if bitCount = 0 then bitCount := 16; //regular death count

  removedElem := nil;
  for i := Elements.Count-1 downto 0 do
    if Elements[i] is TVariableNode then
    with TVariableNode(Elements[i]) do
    begin
      if (Player = ADestPlayer) and
         (UnitType = ADestUnitType) then
      begin
        if (AMode = simSetTo) and not Negative then
        begin
          AMode := simAdd;
          removedElem := Elements[i];
          Elements.Delete(i);
          break;
        end else
        if (AMode = simAdd) and Negative then
        begin
          AMode := simSetTo;
          removedElem := Elements[i];
          Elements.Delete(i);
          break;
        end;
      end;
    end;

  if (PositiveCount = 1) and (NegativeCount = 0) and (ConstElement >= 0) then
  begin
    if ConstElement = 0 then
      Elements[0].LoadIntoVariable( AMode = simSetTo, ADestPlayer, ADestUnitType, AProg )
    else
    begin
      Elements[0].LoadIntoVariable( AMode = simSetTo, ADestPlayer, ADestUnitType, AProg );
      AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simAdd, ConstElement) );
      AProg.Add( TFastIfInstruction.Create( [CreateIntegerCondition( ADestPlayer, ADestUnitType, icmAtLeast, 1 shl bitCount)],
                    [CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, (1 shl bitCount) -1 )] ) );
    end;
  end else
  if (PositiveCount = 0) and (NegativeCount = 0) then
  begin
    if AMode = simSetTo then
    begin
      if (ConstElement < 0) or (ConstElement >= 1 shl bitCount) then
        raise exception.Create('Value out of bounds (' + IntToStr(ConstElement) + ')');

      AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, ConstElement) );
    end else
    if ConstElement <> 0 then
    begin
      if (ConstElement <= - (1 shl bitCount)) or (ConstElement >= 1 shl bitCount) then
        raise exception.Create('Value out of bounds (' + IntToStr(ConstElement) + ')');

      AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, AMode, ConstElement) );
      if ConstElement > 0 then
        AProg.Add( TFastIfInstruction.Create( [CreateIntegerCondition( ADestPlayer, ADestUnitType, icmAtLeast, 1 shl bitCount)],
                      [CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, (1 shl bitCount) -1 )] ) );
    end;
  end
  else
  begin
    if AMode = simSetTo then
    begin
      if ConstElement > 0 then
      begin
        AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, ConstElement) );
        ConstElement := 0;
      end else
      begin
        AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, 0) );
      end;
    end else
    begin
      if ConstElement > 0 then
      begin
        AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simAdd, ConstElement) );
        ConstElement := 0;
      end;
    end;

    firstElem := true;
    for i := 0 to Elements.Count-1 do
      if not Elements[i].Negative then
      begin
        if not firstElem and Elements[i].AlwaysClearAccumulator then
        begin
          AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itAddAccumulator ) );
          firstElem := true;
        end;
        Elements[i].LoadIntoAccumulator(firstElem, AProg);
        firstElem := false;
      end;
    if not firstElem then
      AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itAddAccumulator ) );

    firstElem := true;
    for i := 0 to Elements.Count-1 do
      if Elements[i].Negative then
      begin
        if not firstElem and Elements[i].AlwaysClearAccumulator then
        begin
          AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itSubtractAccumulator ) );
          firstElem:= true;
        end;
        Elements[i].LoadIntoAccumulator(firstElem, AProg);
        firstElem := false;
      end;
    if not firstElem then
      AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itSubtractAccumulator ) );

    if ConstElement < 0 then
      AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSubtract, -ConstElement) );
  end;

  if assigned(removedElem) then Elements.Add(removedElem);
end;

procedure TExpression.AddToProgramInAccumulator(AProg: TInstructionList);
var
  i: Integer;
  firstElem: Boolean;
begin
  firstElem := true;
  for i := 0 to Elements.Count-1 do
    if not Elements[i].Negative then
    begin
      Elements[i].LoadIntoAccumulator(firstElem, AProg);
      firstElem := false;
    end;

  if firstElem then
    AProg.Add( TTransferIntegerInstruction.Create( ConstElement, itCopyIntoAccumulator) )
  else
  if ConstElement <> 0 then
    AProg.Add( TTransferIntegerInstruction.Create( ConstElement, itAddIntoAccumulator) );
end;

function TExpression.Duplicate: TExpression;
var
  i: Integer;
begin
  result := TExpression.Create;
  for i := 0 to Elements.Count-1 do
    result.Elements.Add(Elements[i].Duplicate);
  result.ConstElement := ConstElement;
end;

constructor TRandomNode.Create(ANegative: boolean; ARange: integer);
begin
  inherited Create(ANegative);
  if not IsPowerOf2(Range) then
    raise exception.Create('Random can only be computed for powers of 2');
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

procedure TRandomNode.LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer;
  AUnitType: TStarcraftUnit; AProg: TInstructionList);
begin
  if AClearVar then
    AProg.Add( TRandomizeIntegerInstruction.Create(APlayer,AUnitType, Range) )
  else
  begin
    AProg.Add( TTransferIntegerInstruction.Create(Range, itRandomizeAccumulator) );
    AProg.Add( TTransferIntegerInstruction.Create(APlayer,AUnitType, itAddAccumulator) );
  end;
end;

function TRandomNode.AlwaysClearAccumulator: boolean;
begin
  result := true;
end;

function TRandomNode.BitCount: integer;
begin
  result := GetExponentOf2(Range);
  if result > 16 then result := 24
  else if result > 8 then result := 16
  else result := 8;
end;

function TRandomNode.Duplicate: TExpressionNode;
begin
  result := TRandomNode.Create(Negative,Range);
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
    AProg.Add(TCallInstruction.Create(Name,[],'UInt24'))
  else
    raise exception.Create('Unhandled case');
end;

procedure TFunctionCallNode.LoadIntoVariable(AClearVar: boolean;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList);
begin
  AProg.Add(TCallInstruction.Create(Name,[],'UInt24'));
  if AClearVar then
    AProg.Add(TTransferIntegerInstruction.Create(APlayer, AUnitType, itCopyAccumulator))
  else
    AProg.Add(TTransferIntegerInstruction.Create(APlayer, AUnitType, itAddAccumulator));
end;

function TFunctionCallNode.AlwaysClearAccumulator: boolean;
begin
  result := true;
end;

function TFunctionCallNode.BitCount: integer;
begin
  result := 24;
end;

function TFunctionCallNode.Duplicate: TExpressionNode;
begin
  result := TFunctionCallNode.Create(Negative,Name);
end;

{ TVariableNode }

constructor TVariableNode.Create(ANegative: boolean; APlayer: TPlayer;
  AUnitType: TStarcraftUnit);
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

procedure TVariableNode.LoadIntoVariable(AClearVar: boolean;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList);
begin
  AProg.Add(TTransferIntegerInstruction.Create(Player,UnitType,itCopyIntoAccumulator));
  if AClearVar then
    AProg.Add(TTransferIntegerInstruction.Create(APlayer, AUnitType, itCopyAccumulator))
  else
    AProg.Add(TTransferIntegerInstruction.Create(APlayer, AUnitType, itAddAccumulator));
end;

function TVariableNode.AlwaysClearAccumulator: boolean;
begin
  result := false;
end;

function TVariableNode.BitCount: integer;
var
  i: Integer;
begin
  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Player = Player) and (IntVars[i].UnitType = UnitType) then
      exit(IntVars[i].BitCount);
  result := 8;
end;

function TVariableNode.Duplicate: TExpressionNode;
begin
  result := TVariableNode.Create(Negative,Player,UnitType);
end;

{ TExpressionNode }

constructor TExpressionNode.Create(ANegative: boolean);
begin
  Negative:= ANegative;
end;

function TryParsePlayerExpressionImplementation(AScope: integer; ALine: TStringList; var AIndex: integer): TPlayer;
var
  idx, numPlayer: Integer;
begin
  result := plNone;
  idx := AIndex;
  if TryToken(ALine,idx,'Player') then
  begin
    if TryToken(ALine,idx,'(') then
    begin
      numPlayer := ExpectIntegerConstant(AScope, ALine,idx);
      ExpectToken(ALine,idx,')');
      if (numPlayer < 1) or (numPlayer > 12) then
        raise exception.Create('Player index out of bounds');
      result := TPlayer(ord(plPlayer1)+numPlayer-1);
      AIndex := idx;
    end;
  end;
end;

initialization

  TryParsePlayerExpression := @TryParsePlayerExpressionImplementation;

end.

