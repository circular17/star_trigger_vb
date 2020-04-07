unit uinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

type
  TParameterValue = record
    Expression: TObject;
    Condition: TObject;
  end;
  ArrayOfParameterValue = array of TParameterValue;
  TDuplicateParameterValuesFunc = function(AParamValues: ArrayOfParameterValue): ArrayOfParameterValue;

procedure FreeParameterValues(var AParams: ArrayOfParameterValue);
var DuplicateParameterValues: TDuplicateParameterValuesFunc;

type
  { TInstruction }

  TInstruction = class
    function Duplicate: TInstruction; virtual; abstract;
  end;

  { TEmptyInstruction }

  TEmptyInstruction = class(TInstruction)
  end;

  TCustomInstructionList = specialize TFPGList<TInstruction>;

  { TInstructionList }

  TInstructionList = class(TCustomInstructionList)
    procedure FreeAll;
    function Duplicate: TInstructionList;
  end;

type
  { TCondition }

  TCondition = class
    function IsArithmetic: boolean; virtual;
    function IsComputed: boolean; virtual;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean); virtual;
    function ToBasic(AUseVariables: boolean): string; virtual; abstract;
    function Priority: integer; virtual; abstract;
    function Duplicate: TCondition; virtual; abstract;
  end;

  TCustomConditionList = specialize TFPGList<TCondition>;

  { TConditionList }

  TConditionList = class(TCustomConditionList)
    function IsComputed: boolean;
    function IsArithmetic: boolean;
    procedure Compute(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit);
    procedure FreeAll;
    function ToBasic(AUseVariables: boolean): string;
    function Duplicate: TConditionList;
  end;

  { TRandomizeIntegerInstruction }

  TRandomizeIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Range: integer;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ARange: integer);
    function Duplicate: TInstruction; override;
  end;

  TIntegerTransfer = (itCopyIntoAccumulator, itAddIntoAccumulator, itCopyAccumulator, itAddAccumulator,
                      itSubtractIntoAccumulator, itSubtractAccumulator, itRandomizeAccumulator, itLimitAccumulator);

  { TTransferIntegerInstruction }

  TTransferIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Action: TIntegerTransfer;
    Value: integer;
    Shift: integer;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; AAction: TIntegerTransfer; AShift: integer = 0);
    constructor Create(AValue: integer; AAction: TIntegerTransfer);
    function Duplicate: TInstruction; override;
  end;

  { TPrintForAnyPlayerInstruction }

  TPrintForAnyPlayerInstruction = class(TInstruction)
    Msg: integer;
    constructor Create(AMsg: integer);
    function Duplicate: TInstruction; override;
  end;

  { TCallInstruction }

  TCallInstruction = class(TInstruction)
    Scope: integer;
    Name: string;
    ParamValues: ArrayOfParameterValue;
    ReturnType: string;
    constructor Create(AScope: integer; AName: string; AParamValues: ArrayOfParameterValue; AReturnType: string = 'Void');
    function Duplicate: TInstruction; override;
    destructor Destroy; override;
  end;

  { TDropThreadInstruction }

  TDropThreadInstruction = class(TInstruction)
    DropIP, ResumeIP: integer;
    PlayersToDrop, PlayersToResume: TPlayers;
    constructor Create(ADropIP, AResumeIP: integer; APlayersToDrop, APlayersToResume: TPlayers);
    function Duplicate: TInstruction; override;
  end;

  { TWaitForPlayersInstruction }

  TWaitForPlayersInstruction = class(TInstruction)
    Players: TPlayers;
    constructor Create(APlayers: TPlayers);
    function Duplicate: TInstruction; override;
  end;

  { TReturnInstruction }

  TReturnInstruction = class(TInstruction)
    constructor Create;
    function Duplicate: TInstruction; override;
  end;

  { TDoAsInstruction }

  TDoAsInstruction = class(TInstruction)
    Players: TPlayers;
    constructor Create(APlayers: TPlayers);
    function Duplicate: TInstruction; override;
  end;

  { TEndDoAsInstruction }

  TEndDoAsInstruction = class(TInstruction)
    Players: TPlayers;
    constructor Create(APlayers: TPlayers);
    function Duplicate: TInstruction; override;
  end;

  { TWaitForPresenceDefinedInstruction }

  TWaitForPresenceDefinedInstruction = class(TInstruction)
    constructor Create;
    function Duplicate: TInstruction; override;
  end;

  { TIfInstruction }

  TIfInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition);
    constructor Create(AConditions: TConditionList);
    function Duplicate: TInstruction; override;
  end;

  { TElseIfInstruction }

  TElseIfInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition);
    constructor Create(AConditions: TConditionList);
    function Duplicate: TInstruction; override;
  end;

  { TFastIfInstruction }

  TFastIfInstruction = class(TInstruction)
    Conditions: TConditionList;
    Instructions: TInstructionList;
    constructor Create(AConditions: TConditionList; AInstructions: TInstructionList);
    constructor Create(AConditions: TConditionList; AInstructions: array of TInstruction);
    constructor Create(AConditions: array of TCondition; AInstructions: TInstructionList);
    constructor Create(AConditions: array of TCondition; AInstructions: array of TInstruction);
    destructor Destroy; override;
    function Duplicate: TInstruction; override;
  end;

  { TWaitConditionInstruction }

  TWaitConditionInstruction = class(TInstruction)
    Conditions: TConditionList;
    IP: integer;
    destructor Destroy; override;
    constructor Create(AConditions: TConditionList; AIP: Integer);
    constructor Create(ACondition: TCondition; AIP: Integer);
    function Duplicate: TInstruction; override;
  end;

  { TElseInstruction }

  TElseInstruction = class(TInstruction)
    constructor Create;
    function Duplicate: TInstruction; override;
  end;

  { TSplitInstruction }

  TSplitInstruction = class(TInstruction)
    ResumeIP, EndIP: integer;
    ChangePlayers: TPlayers;
    constructor Create(AResumeIP, AEndIP: integer; AChangePlayers: TPlayers = []);
    function Duplicate: TInstruction; override;
  end;

  { TEndIfInstruction }

  TEndIfInstruction = class(TInstruction)
    constructor Create;
    function Duplicate: TInstruction; override;
  end;

  { TChangeIPInstruction }

  TChangeIPInstruction = class(TInstruction)
    IP: integer;
    Preserve: Integer;
    constructor Create(AIP: Integer; APreserve: Integer);
    function Duplicate: TInstruction; override;
  end;

  { TWhileInstruction }

  TWhileInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: TConditionList);
    function Duplicate: TInstruction; override;
  end;

  { TEndWhileInstruction }

  TEndWhileInstruction = class(TInstruction)
    constructor Create;
    function Duplicate: TInstruction; override;
  end;

  /////////////////////////////////////////////////////////////////////////////////////////////

type
  { TNotCondition }

  TNotCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean); override;
    function Priority: integer; override;
    function ToBasic(AUseVariables: boolean): string; override;
    function Duplicate: TCondition; override;
  end;

  { TOrCondition }

  TOrCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean); override;
    function Priority: integer; override;
    function ToBasic(AUseVariables: boolean): string; override;
    function Duplicate: TCondition; override;
  end;

  { TAndCondition }

  TAndCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: TStarcraftUnit; {%H-}AFirst: boolean); override;
    function Priority: integer; override;
    function ToBasic(AUseVariables: boolean): string; override;
    function Duplicate: TCondition; override;
  end;

type
  TCreateSetIntegerInstructionProc = function (APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TSetIntegerMode; AValue: integer): TInstruction;
  TCreateIntegerConditionProc = function (APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TIntegerConditionMode; AValue: integer): TCondition;

var
  CreateSetIntegerInstruction: TCreateSetIntegerInstructionProc;
  CreateIntegerCondition: TCreateIntegerConditionProc;

implementation

procedure FreeParameterValues(var AParams: ArrayOfParameterValue);
var
  i: Integer;
begin
  for i := 0 to high(AParams) do
  begin
    AParams[i].Expression.Free;
    AParams[i].Condition.Free;
  end;
  AParams := nil;
end;

{ TElseIfInstruction }

destructor TElseIfInstruction.Destroy;
begin
  Conditions.FreeAll;
  inherited Destroy;
end;

constructor TElseIfInstruction.Create(ACondition: TCondition);
begin
  Conditions := TConditionList.Create;
  Conditions.Add(ACondition);
end;

constructor TElseIfInstruction.Create(AConditions: TConditionList);
begin
  Conditions := AConditions;
end;

function TElseIfInstruction.Duplicate: TInstruction;
begin
  Result:=TElseIfInstruction.Create(Conditions.Duplicate);
end;

{ TRandomizeIntegerInstruction }

constructor TRandomizeIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ARange: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Range := ARange;
  if Range < 1 then raise exception.Create('Invalid range');
end;

function TRandomizeIntegerInstruction.Duplicate: TInstruction;
begin
  result := TRandomizeIntegerInstruction.Create(Player,UnitType,Range);
end;

{ TOrCondition }

destructor TOrCondition.Destroy;
begin
  Conditions.FreeAll;
  inherited Destroy;
end;

constructor TOrCondition.Create(AConditions: array of TCondition);
var
  i: Integer;
begin
  Conditions := TConditionList.Create;
  for i := 0 to high(AConditions) do
    Conditions.Add(AConditions[i]);
end;

constructor TOrCondition.Create(AConditions: TConditionList);
begin
  Conditions := AConditions;
end;

function TOrCondition.IsArithmetic: boolean;
var
  i: Integer;
begin
  for i := 0 to Conditions.Count-1 do
    if Conditions[i].IsArithmetic then exit(true);
  exit(false);
end;

function TOrCondition.IsComputed: boolean;
begin
  Result:=true;
end;

procedure TOrCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean);
var
  i: Integer;
begin
  if Conditions.Count > 0 then
  begin
    if not AFirst then
      AProg.Add(TIfInstruction.Create(CreateIntegerCondition(APlayer,AUnitType,icmAtLeast,1)));
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
    for i := 0 to Conditions.Count-1 do
    begin
      if not Conditions[i].IsComputed then
      begin
        AProg.Add(TFastIfInstruction.Create(Conditions[i].Duplicate,[CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1)]));
      end else
      if Conditions[i] is TAndCondition then
      begin
        AProg.Add(TFastIfInstruction.Create(TAndCondition(Conditions[i]).Conditions.Duplicate,[CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1)]));
      end else
      begin
        AProg.Add(TIfInstruction.Create(Conditions[i].Duplicate));
        AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1));
        AProg.Add(TEndIfInstruction.Create);
      end;
    end;
    if not AFirst then
      AProg.Add(TEndIfInstruction.Create);
  end else
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
end;

function TOrCondition.Priority: integer;
begin
  result := -10;
end;

function TOrCondition.ToBasic(AUseVariables: boolean): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to Conditions.Count-1 do
  begin
    if i > 0 then result += ' Or ';
    if Conditions[0].Priority < self.Priority then
      result += '('+Conditions[i].ToBasic(AUseVariables)+')' else
      result += Conditions[i].ToBasic(AUseVariables);
  end;
end;

function TOrCondition.Duplicate: TCondition;
begin
  result := TOrCondition.Create(Conditions.Duplicate);
end;

{ TAndCondition }

destructor TAndCondition.Destroy;
begin
  Conditions.FreeAll;
  inherited Destroy;
end;

constructor TAndCondition.Create(AConditions: array of TCondition);
var
  i: Integer;
begin
  Conditions := TConditionList.Create;
  for i := 0 to high(AConditions) do
    Conditions.Add(AConditions[i]);
end;

constructor TAndCondition.Create(AConditions: TConditionList);
begin
  Conditions := AConditions;
end;

function TAndCondition.IsArithmetic: boolean;
var
  i: Integer;
begin
  for i := 0 to Conditions.Count-1 do
    if Conditions[i].IsArithmetic then exit(true);
  exit(false);
end;

function TAndCondition.IsComputed: boolean;
begin
  Result:= true;
end;

procedure TAndCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean);
begin
  raise exception.Create('Not handled');
end;

function TAndCondition.Priority: integer;
begin
  result := -5;
end;

function TAndCondition.ToBasic(AUseVariables: boolean): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to Conditions.Count-1 do
  begin
    if i > 0 then result += ' And ';
    if Conditions[i].Priority < self.Priority then
      result += '('+Conditions[i].ToBasic(AUseVariables)+')' else
      result += Conditions[i].ToBasic(AUseVariables);
  end;
end;

function TAndCondition.Duplicate: TCondition;
begin
  result := TAndCondition.Create(Conditions.Duplicate);
end;

{ TPrintForAnyPlayerInstruction }

constructor TPrintForAnyPlayerInstruction.Create(AMsg: integer);
begin
  Msg := AMsg;
end;

function TPrintForAnyPlayerInstruction.Duplicate: TInstruction;
begin
  result := TPrintForAnyPlayerInstruction.Create(Msg);
end;

{ TWaitForPresenceDefinedInstruction }

constructor TWaitForPresenceDefinedInstruction.Create;
begin
  //
end;

function TWaitForPresenceDefinedInstruction.Duplicate: TInstruction;
begin
  result := TWaitForPresenceDefinedInstruction.Create;
end;

{ TInstructionList }

procedure TInstructionList.FreeAll;
var i: integer;
begin
  if self = nil then exit;

  for i := 0 to Count-1 do
    Items[i].Free;
  Free;
end;

function TInstructionList.Duplicate: TInstructionList;
var
  i: Integer;
begin
  result := TInstructionList.Create;
  for i := 0 to Count-1 do
    result.Add(Items[i].Duplicate);
end;

{ TWaitForPlayersInstruction }

constructor TWaitForPlayersInstruction.Create(APlayers: TPlayers);
begin
  Players:= APlayers;
end;

function TWaitForPlayersInstruction.Duplicate: TInstruction;
begin
  result := TWaitForPlayersInstruction.Create(Players);
end;

{ TEndDoAsInstruction }

constructor TEndDoAsInstruction.Create(APlayers: TPlayers);
begin
  Players:= APlayers;
end;

function TEndDoAsInstruction.Duplicate: TInstruction;
begin
  result := TEndDoAsInstruction.Create(Players);
end;

{ TDoAsInstruction }

constructor TDoAsInstruction.Create(APlayers: TPlayers);
begin
  Players:= APlayers;
end;

function TDoAsInstruction.Duplicate: TInstruction;
begin
  result := TDoAsInstruction.Create(Players);
end;

{ TFastIfInstruction }

constructor TFastIfInstruction.Create(AConditions: TConditionList;
  AInstructions: TInstructionList);
begin
  Conditions := AConditions;
  Instructions := AInstructions;
end;

constructor TFastIfInstruction.Create(AConditions: TConditionList;
  AInstructions: array of TInstruction);
var
  i: Integer;
begin
  Conditions := AConditions;
  Instructions := TInstructionList.Create;
  for i := 0 to high(AInstructions) do Instructions.Add(AInstructions[i]);
end;

constructor TFastIfInstruction.Create(AConditions: array of TCondition;
  AInstructions: TInstructionList);
var
  i: Integer;
begin
  Conditions := TConditionList.Create;
  for i := 0 to high(AConditions) do Conditions.Add(AConditions[i]);
  Instructions := AInstructions;
end;

constructor TFastIfInstruction.Create(AConditions: array of TCondition;
  AInstructions: array of TInstruction);
var
  i: Integer;
begin
  Conditions := TConditionList.Create;
  for i := 0 to high(AConditions) do Conditions.Add(AConditions[i]);
  Instructions := TInstructionList.Create;
  for i := 0 to high(AInstructions) do Instructions.Add(AInstructions[i]);
end;

destructor TFastIfInstruction.Destroy;
var
  i: Integer;
begin
  Conditions.FreeAll;
  for i := 0 to Instructions.Count-1 do
    Instructions[i].Free;
  Instructions.Free;
  inherited Destroy;
end;

function TFastIfInstruction.Duplicate: TInstruction;
begin
  result := TFastIfInstruction.Create(Conditions.Duplicate,Instructions.Duplicate);
end;

{ TNotCondition }

destructor TNotCondition.Destroy;
var
  i: Integer;
begin
  for i := 0 to Conditions.Count-1 do
    Conditions[i].Free;
  Conditions.Free;
  inherited Destroy;
end;

constructor TNotCondition.Create(AConditions: array of TCondition);
var
  i: Integer;
begin
  Conditions := TConditionList.Create;
  for i := 0 to high(AConditions) do
    Conditions.Add(AConditions[i]);
end;

constructor TNotCondition.Create(AConditions: TConditionList);
begin
  Conditions := AConditions;
end;

function TNotCondition.IsArithmetic: boolean;
var
  i: Integer;
begin
  for i := 0 to Conditions.Count-1 do
    if Conditions[i].IsArithmetic then exit(true);
  exit(false);
end;

function TNotCondition.IsComputed: boolean;
begin
  Result:= true;
end;

procedure TNotCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean);
begin
  if AFirst then AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1));
  if not Conditions.IsComputed then
  begin
    AProg.Add(TFastIfInstruction.Create(Conditions.Duplicate,[CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0)]));
  end else
  if (Conditions.Count = 1) and (Conditions[0] is TAndCondition) then
  begin
    AProg.Add(TFastIfInstruction.Create(TAndCondition(Conditions[0]).Conditions.Duplicate,[CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0)]));
  end else
  begin
    AProg.Add(TIfInstruction.Create(Conditions.Duplicate));
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

function TNotCondition.Priority: integer;
begin
  result := 0;
end;

function TNotCondition.ToBasic(AUseVariables: boolean): string;
begin
  if (Conditions.Count <> 1) or
    (Conditions[0].Priority < self.Priority) then
    result := 'Not ('+Conditions.ToBasic(AUseVariables)+')'
    else result := 'Not '+Conditions.ToBasic(AUseVariables);
end;

function TNotCondition.Duplicate: TCondition;
begin
  result := TNotCondition.Create(Conditions.Duplicate);
end;

{ TSplitInstruction }

constructor TSplitInstruction.Create(AResumeIP, AEndIP: integer;
  AChangePlayers: TPlayers);
begin
  ResumeIP:= AResumeIP;
  EndIP := AEndIP;
  ChangePlayers := AChangePlayers;
end;

function TSplitInstruction.Duplicate: TInstruction;
begin
  result := TSplitInstruction.Create(ResumeIP,EndIP,ChangePlayers);
end;

{ TTransferIntegerInstruction }

constructor TTransferIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; AAction: TIntegerTransfer; AShift: integer);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Action:= AAction;
  Value := 0;
  Shift := AShift;
  if (Shift<>0) and not (AAction in[itAddAccumulator,itCopyAccumulator]) then raise exception.Create('Shift not valid for this action');
  if (Shift < 0) or (Shift > 23) then raise exception.Create('Shift value out of range');
  if AAction = itRandomizeAccumulator then raise exception.Create('Randomize can only be done with a constant range');
  if AAction = itLimitAccumulator then raise exception.Create('Limit can only be done with a constant range');
end;

constructor TTransferIntegerInstruction.Create(AValue: integer;
  AAction: TIntegerTransfer);
begin
  if AAction in [itAddAccumulator,itSubtractAccumulator,itCopyAccumulator] then
    raise exception.Create('Cannot copy into a constant');
  Player:= plNone;
  UnitType:= suConst;
  Action:= AAction;
  Value := AValue;
  if Value < 0 then
  begin
    case Action of
    itAddIntoAccumulator: begin
                            Action := itSubtractIntoAccumulator;
                            Value := -Value;
                          end;
    itSubtractIntoAccumulator: begin
                                 Action := itAddIntoAccumulator;
                                 Value := -Value;
                               end;
    itCopyIntoAccumulator, itLimitAccumulator: Value := 0;
    else raise exception.Create('Case not handled');
    end;
  end;
end;

function TTransferIntegerInstruction.Duplicate: TInstruction;
begin
  if UnitType = suConst then
    result := TTransferIntegerInstruction.Create(Value,Action)
  else
    result := TTransferIntegerInstruction.Create(Player,UnitType,Action,Shift);
end;

{ TConditionList }

function TConditionList.IsComputed: boolean;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Items[i].IsComputed then exit(true);
  exit(false);
end;

function TConditionList.IsArithmetic: boolean;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Items[i].IsArithmetic then exit(true);
  exit(false);
end;

procedure TConditionList.Compute(AProg: TInstructionList; APlayer: TPlayer;
  AUnitType: TStarcraftUnit);
var
  i: Integer;
begin
  if Count = 0 then
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1))
  else
  begin
    Items[0].AddToProgAsAndVar(AProg, APlayer, AUnitType, True);
    for i := 1 to Count-1 do
      Items[i].AddToProgAsAndVar(AProg, APlayer, AUnitType, False);
  end;
end;

procedure TConditionList.FreeAll;
var i: integer;
begin
  if self = nil then exit;

  for i := 0 to Count-1 do
    Items[i].Free;
  Free;
end;

function TConditionList.ToBasic(AUseVariables: boolean): string;
var
  i: Integer;
begin
  if Count = 0 then result := 'True'
  else if Count = 1 then
  begin
    result := Items[0].ToBasic(AUseVariables);
  end else
  begin
    result := '';
    for i := 0 to Count-1 do
    begin
      if i > 0 then result += ' And ';
      if Items[i].Priority < -5 then
        result += '(' + Items[i].ToBasic(AUseVariables) + ')'
        else result += Items[i].ToBasic(AUseVariables);
    end;
  end;
end;

function TConditionList.Duplicate: TConditionList;
var
  i: Integer;
begin
  result := TConditionList.Create;
  for i := 0 to Count-1 do
    result.Add(Items[i].Duplicate);
end;

{ TWaitConditionInstruction }

destructor TWaitConditionInstruction.Destroy;
begin
  Conditions.FreeAll;
  inherited Destroy;
end;

constructor TWaitConditionInstruction.Create(AConditions: TConditionList; AIP: Integer);
begin
  Conditions := AConditions;
  IP := AIP;
end;

constructor TWaitConditionInstruction.Create(ACondition: TCondition;
  AIP: Integer);
begin
  Conditions := TConditionList.Create;
  Conditions.Add(ACondition);
  IP := AIP;
end;

function TWaitConditionInstruction.Duplicate: TInstruction;
begin
  result := TWaitConditionInstruction.Create(Conditions.Duplicate, IP);
end;

{ TChangeIPInstruction }

constructor TChangeIPInstruction.Create(AIP: Integer; APreserve: Integer);
begin
  IP := AIP;
  Preserve := APreserve;
end;

function TChangeIPInstruction.Duplicate: TInstruction;
begin
  result := TChangeIPInstruction.Create(IP,Preserve);
end;

{ TEndIfInstruction }

constructor TEndIfInstruction.Create;
begin
  //nothing
end;

function TEndIfInstruction.Duplicate: TInstruction;
begin
  result := TEndIfInstruction.Create;
end;

{ TElseInstruction }

constructor TElseInstruction.Create;
begin
  //nothing
end;

function TElseInstruction.Duplicate: TInstruction;
begin
  result := TElseInstruction.Create;
end;

{ TIfInstruction }

destructor TIfInstruction.Destroy;
begin
  Conditions.FreeAll;
  inherited Destroy;
end;

constructor TIfInstruction.Create(ACondition: TCondition);
begin
  Conditions := TConditionList.Create;
  Conditions.Add(ACondition);
end;

constructor TIfInstruction.Create(AConditions: TConditionList);
begin
  Conditions := AConditions;
end;

function TIfInstruction.Duplicate: TInstruction;
begin
  result := TIfInstruction.Create(Conditions.Duplicate);
end;

{ TWhileInstruction }

destructor TWhileInstruction.Destroy;
begin
  Conditions.FreeAll;
  inherited Destroy;
end;

constructor TWhileInstruction.Create(AConditions: TConditionList);
begin
  Conditions := AConditions;
end;

function TWhileInstruction.Duplicate: TInstruction;
begin
  result := TWhileInstruction.Create(Conditions.Duplicate);
end;

{ TEndWhileInstruction }

constructor TEndWhileInstruction.Create;
begin
 //nothing
end;

function TEndWhileInstruction.Duplicate: TInstruction;
begin
  result := TEndWhileInstruction.Create;
end;

{ TReturnInstruction }

constructor TReturnInstruction.Create;
begin
  //nothing
end;

function TReturnInstruction.Duplicate: TInstruction;
begin
  result := TReturnInstruction.Create;
end;

{ TDropThreadInstruction }

constructor TDropThreadInstruction.Create(ADropIP, AResumeIP: integer;
  APlayersToDrop, APlayersToResume: TPlayers);
begin
  DropIP:= ADropIP;
  ResumeIP:= AResumeIP;
  PlayersToDrop:= APlayersToDrop;
  PlayersToResume:= APlayersToResume;
end;

function TDropThreadInstruction.Duplicate: TInstruction;
begin
  result := TDropThreadInstruction.Create(DropIP,ResumeIP,PlayersToDrop,PlayersToResume);
end;

{ TCallInstruction }

constructor TCallInstruction.Create(AScope: integer; AName: string; AParamValues: ArrayOfParameterValue; AReturnType: string = 'Void');
var
  i: Integer;
begin
  Scope := AScope;
  Name := AName;
  setlength(ParamValues, length(AParamValues));
  for i := 0 to high(AParamValues) do
    ParamValues[i] := AParamValues[i];
  ReturnType:= AReturnType;
end;

function TCallInstruction.Duplicate: TInstruction;
begin
  result := TCallInstruction.Create(Scope, Name, DuplicateParameterValues(ParamValues), ReturnType);
end;

destructor TCallInstruction.Destroy;
begin
  FreeParameterValues(ParamValues);
  inherited Destroy;
end;

{ TCondition }

function TCondition.IsArithmetic: boolean;
begin
  result := false;
end;

function TCondition.IsComputed: boolean;
begin
  result := IsArithmetic;
end;

procedure TCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit; AFirst: boolean);
begin
  if not self.IsComputed and AFirst then
  begin
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0) );
    AProg.Add(TFastIfInstruction.Create([self.Duplicate],
       [CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1)]) );
  end else
  begin
    AProg.Add(TIfInstruction.Create(self.Duplicate));
    if AFirst then AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
    AProg.Add(TElseInstruction.Create);
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

end.

