unit uexpressions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, uinstructions, fgl;

type
  { TExpressionNode }

  TExpressionNode = class
    Negative: boolean;
    constructor Create(ANegative: boolean);
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); virtual; abstract;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); virtual; abstract;
    function AlwaysClearAccumulator: boolean; virtual; abstract;
    function BitCount: integer; virtual; abstract;
    function ToBasic(AUseVariables: boolean): string; virtual; abstract;
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
    function ToBasic({%H-}AUseVariables: boolean): string; override;
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
    function ToBasic(AUseVariables: boolean): string; override;
    function Duplicate: TExpressionNode; override;
  end;

  { TFunctionCallNode }

  TFunctionCallNode = class(TExpressionNode)
    Scope: integer;
    Name: string;
    ParamValues: ArrayOfParameterValue;
    constructor Create(ANegative: boolean; AScope: integer; AName: string; AParamValues: ArrayOfParameterValue);
    destructor Destroy; override;
    procedure LoadIntoAccumulator(AClearAcc: boolean; AProg: TInstructionList); override;
    procedure LoadIntoVariable(AClearVar: boolean; APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList); override;
    function AlwaysClearAccumulator: boolean; override;
    function BitCount: integer; override;
    function ToBasic({%H-}AUseVariables: boolean): string; override;
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
    function ToBasic({%H-}AUseVariables: boolean): string; override;
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
    function ToBasic({%H-}AUseVariables: boolean): string; override;
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
    function ToBasic({%H-}AUseVariables: boolean): string; override;
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
    function ToBasic(AUseVariables: boolean): string;
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
    function ToBasic(AUseVariables: boolean): string; override;
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
    function ToBasic(AUseVariables: boolean): string; override;
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
    function ToBasic(AUseVariables: boolean): string; override;
    function Priority: integer; override;
    function Duplicate: TCondition; override;
  end;

function TryExpression(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; ARaiseException: boolean; AAcceptCalls: boolean = true; AConstOnly: boolean = false): TExpression;

implementation

uses uparsevb, uvariables, uarithmetic, utriggerconditions, uparsescalar, uprocedures;

function TryIntegerConstantImplementation(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; out
  AValue: integer): boolean;
var
  expr: TExpression;
  oldIndex: integer;
begin
  oldIndex := AIndex;
  expr := TryExpression(AThreads, AScope, ALine,AIndex, false, false, true);
  if expr = nil then
  begin
    result := TryInteger(AThreads, AScope, ALine, AIndex, AValue);
    exit;
  end;
  if expr.Elements.Count > 0 then
  begin
    expr.Free;
    AValue := 0;
    AIndex := oldIndex;
    exit(false);
  end;
  if expr.ConstElement < 0 then
  begin
    expr.Free;
    AValue := 0;
    AIndex := oldIndex;
    exit(false);
  end;
  AValue := expr.ConstElement;
  expr.Free;
  exit(true);
end;

function ExpectIntegerConstantImplementation(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; AAcceptNegative: boolean): integer;
var
  expr: TExpression;
begin
  expr := TryExpression(AThreads, AScope, ALine,AIndex,false,false);
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
  if not AAcceptNegative and (expr.ConstElement < 0) then
  begin
    expr.Free;
    raise exception.Create('Expression is negative');
  end;
  result := expr.ConstElement;
  expr.Free;
end;

function TryExpression(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer; ARaiseException: boolean; AAcceptCalls: boolean; AConstOnly: boolean): TExpression;
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
      expr := TryExpression(AThreads, AScope, ALine,idx, ARaiseException, AAcceptCalls);
      if expr = nil then
      begin
        if ARaiseException then raise exception.Create('Expecting integer value');
        exit;
      end;
      if not TryToken(ALine,idx,')') then
      begin
        expr.free;
        raise exception.Create('Expecting closing bracket ")"');
      end else
        result := TSubExpression.Create(expr);
    end else
    if TryInteger(AThreads, AScope, ALine,idx,intValue) then
    begin
      result := TConstantNode.Create(neg, intValue);
    end else
    begin
      rnd := ParseRandom(AThreads, AScope, ALine, idx);
      if rnd <> -1 then
        result := TRandomNode.Create(neg, rnd) else
      begin
        scalar:= TryScalarVariable(AThreads, AScope, ALine, idx, AConstOnly);
        case scalar.VarType of
        svtInteger: if scalar.Constant then
                        result := TConstantNode.Create(neg, scalar.IntValue)
                   else result := TVariableNode.Create(neg,scalar.Player,scalar.UnitType);
        svtSwitch:
          begin
            if ARaiseException then
              raise exception.Create('Expecting integer value but boolean found');
            exit;
          end
        else
          begin
            if not AConstOnly and TryToken(ALine,idx,'CountIf') then
            begin
              ExpectToken(ALine,idx,'(');
              idxVar := TryBooleanArray(AScope,ALine,idx);
              if idxVar <> -1 then
              begin
                ExpectToken(ALine,idx,',');
                boolVal := ExpectBooleanConstant(AThreads, AScope, ALine, idx);
                result := TCountIfBoolNode.Create(neg, idxVar, boolVal);
              end else
              begin
                idxVar := TryIntegerArray(AScope,ALine,idx);
                if idxVar <> -1 then
                begin
                  ExpectToken(ALine,idx,',');
                  if not TryIntegerConstant(AThreads, AScope, ALine,idx, intVal) then
                    raise exception.Create('Integer constant expected');
                  result := TCountIfIntNode.Create(neg, idxVar, intVal);
                end else
                begin
                  if not TryIdentifier(ALine,idx,name, false) then
                    raise exception.Create('Array identifier expected')
                    else raise exception.Create('Unknown array variable "' + name + '"');
                end;
              end;
              ExpectToken(ALine,idx,')');
            end else
            if TryToken(ALine, idx, 'Abs') then
            begin
              ExpectToken(ALine,idx,'(');
              intVal := ExpectIntegerConstant(AThreads, AScope, ALine, idx, true);
              result := TConstantNode.Create(false, abs(intVal));
              ExpectToken(ALine,idx,')');
            end else
            begin
              if AAcceptCalls and TryFunction(AScope, ALine, idx, name) then
              begin
                result := TFunctionCallNode.Create(neg, AScope, name,
                            ParseProcedureParameterValues(AThreads, AScope, name, ALine, idx));
              end else
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

                if IsPlayerIdentifier(name) then
                  raise exception.Create('Unexpected player identifier');

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
        begin
          if lastNode.Negative then
          begin
            lastNode.Negative:= not lastNode.Negative;
            mult := TMultiplyNode.Create(TExpression.Create(lastNode), AValue);
            mult.Negative := true;
          end else
            mult := TMultiplyNode.Create(TExpression.Create(lastNode), AValue);
        end;
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
        FreeAndNil(result);
        if ARaiseException then
          raise exception.Create('Integer expression expected');
        exit;
      end;
      result.Elements.Add(node);

      while TryToken(ALine,idx,'*') do
      begin
        if TryToken(ALine,idx,'(') then
        begin
          subExpr := TryExpression(AThreads, AScope, ALine,idx,ARaiseException,AAcceptCalls);
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
            FreeAndNil(result);
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

function TSubExpression.ToBasic(AUseVariables: boolean): string;
begin
  result := '(' + Expr.ToBasic(AUseVariables) + ')';
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

function TMultiplyNode.ToBasic(AUseVariables: boolean): string;
begin
  result := '(' + Expr.ToBasic(AUseVariables) + ') * '+inttostr(Factor);
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

function TConstantNode.ToBasic(AUseVariables: boolean): string;
begin
  result := IntToStr(Value);
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

function TArithmeticCondition.ToBasic(AUseVariables: boolean): string;
begin
  result := Expression.ToBasic(AUseVariables) + ' ' + IntConditionModeToBasic[CompareMode] + ' ' + IntToStr(CompareValue);
end;

function TArithmeticCondition.Priority: integer;
begin
  result := 5;
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

function TCountIfIntNode.ToBasic(AUseVariables: boolean): string;
begin
  result := 'CountIf(' + IntArrays[IntArray].Name + ', '+ inttostr(TestValue)+')';
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

function TCountIfBoolNode.ToBasic(AUseVariables: boolean): string;
begin
  result := 'CountIf(' + BoolArrays[BoolArray].Name + ', '+ BoolToStr(TestValue,'True','False')+')';
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
  firstAccElem: Boolean;
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
    firstAccElem := true;
    for i := 0 to Elements.Count-1 do
      if not Elements[i].Negative then
      begin
        if not firstAccElem and Elements[i].AlwaysClearAccumulator then
        begin
          if AMode = simSetTo then
          begin
            AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itCopyAccumulator ) );
            AMode := simAdd;
          end else
            AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itAddAccumulator ) );
          firstAccElem := true;
        end;
        Elements[i].LoadIntoAccumulator(firstAccElem, AProg);
        firstAccElem := false;
      end;

    if ConstElement > 0 then
    begin
      if not firstAccElem then
        AProg.Add( TTransferIntegerInstruction.Create( ConstElement, itAddIntoAccumulator ) )
        else
        begin
          if AMode = simSetTo then
          begin
            AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, ConstElement ) );
            AMode := simAdd;
          end
            else AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simAdd, ConstElement ) );
        end;
    end;

    if not firstAccElem then
    begin
      if AMode = simSetTo then // always copy accumulator because afterwards its value is negative
      begin
        AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itCopyAccumulator ) );
        AMode := simAdd;
      end else
        AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itAddAccumulator ) );
      firstAccElem := true;
    end;

    for i := 0 to Elements.Count-1 do
      if Elements[i].Negative then
      begin
        if not firstAccElem and Elements[i].AlwaysClearAccumulator then
        begin
          if AMode = simSetTo then
          begin
            AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, 0) );
            AMode := simAdd;
          end else
            AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itSubtractAccumulator ) );
          firstAccElem:= true;
        end;
        Elements[i].LoadIntoAccumulator(firstAccElem, AProg);
        firstAccElem := false;
      end;
    if not firstAccElem then
    begin
      if AMode = simSetTo then
      begin
        AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, 0) );
        AMode := simAdd;
      end else
        AProg.Add( TTransferIntegerInstruction.Create( ADestPlayer, ADestUnitType, itSubtractAccumulator ) );
    end;

    if ConstElement < 0 then
    begin
      if AMode = simSetTo then
      begin
        AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSetTo, 0) );
        AMode := simAdd;
      end else
        AProg.Add( CreateSetIntegerInstruction( ADestPlayer, ADestUnitType, simSubtract, -ConstElement) );
    end;
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

function TExpression.ToBasic(AUseVariables: boolean): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to Elements.Count-1 do
  begin
    if Elements[i].Negative then
    begin
      if i > 0 then result += ' ';
      result += '- ';
    end
    else if i > 0 then result += ' + ';
    result += Elements[i].ToBasic(AUseVariables);
  end;
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
  if not IsPowerOf2(ARange) then
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

function TRandomNode.ToBasic(AUseVariables: boolean): string;
begin
  result := 'Rnd * '+inttostr(Range);
end;

function TRandomNode.Duplicate: TExpressionNode;
begin
  result := TRandomNode.Create(Negative,Range);
end;

constructor TFunctionCallNode.Create(ANegative: boolean; AScope: integer; AName: string; AParamValues: ArrayOfParameterValue);
var
  i: Integer;
begin
  inherited Create(ANegative);
  Scope := AScope;
  Name := AName;
  setlength(ParamValues, length(AParamValues));
  for i := 0 to high(ParamValues) do
    ParamValues[i] := AParamValues[i];
end;

destructor TFunctionCallNode.Destroy;
begin
  FreeParameterValues(ParamValues);
  inherited Destroy;
end;

procedure TFunctionCallNode.LoadIntoAccumulator(AClearAcc: boolean;
  AProg: TInstructionList);
begin
  if AClearAcc then
    AProg.Add(TCallInstruction.Create(Scope, Name, ParamValues, 'UInt24'))
  else
    raise exception.Create('Unhandled case');
end;

procedure TFunctionCallNode.LoadIntoVariable(AClearVar: boolean;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AProg: TInstructionList);
begin
  AProg.Add(TCallInstruction.Create(Scope, Name, ParamValues, 'UInt24'));
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

function TFunctionCallNode.ToBasic(AUseVariables: boolean): string;
begin
  result := Name + '()';
end;

function TFunctionCallNode.Duplicate: TExpressionNode;
begin
  result := TFunctionCallNode.Create(Negative, Scope, Name, DuplicateParameterValues(ParamValues));
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

function TVariableNode.ToBasic(AUseVariables: boolean): string;
var
  i: Integer;
begin
  if AUseVariables then
    for i := 0 to IntVarCount-1 do
      if (IntVars[i].Player = Player) and (IntVars[i].UnitType = UnitType) then
        exit(IntVars[i].Name);
  result := PlayerIdentifiers[Player]+'.DeathCount('+StarcraftUnitIdentifier[UnitType]+')';
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

function TryParsePlayerExpressionImplementation(AThreads: TPlayers; AScope: integer; ALine: TStringList; var AIndex: integer): TPlayer;
var
  idx, numPlayer: Integer;
begin
  result := plNone;
  idx := AIndex;
  if TryToken(ALine,idx,'Player') then
  begin
    if TryToken(ALine,idx,'(') then
    begin
      numPlayer := ExpectConstantIndex(AThreads, AScope, ALine,idx,true);
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
  ExpectIntegerConstant := @ExpectIntegerConstantImplementation;
  TryIntegerConstant := @TryIntegerConstantImplementation;

end.

