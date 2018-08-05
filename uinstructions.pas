unit uinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

type
  { TInstruction }

  TInstruction = class
    function ToStringAndFree: ansistring;
  end;

  { TEmptyInstruction }

  TEmptyInstruction = class(TInstruction)
    function ToString: ansistring; override;
  end;

  TInstructionList = specialize TFPGList<TInstruction>;

const
  SwitchToStr : array[TSwitchValue] of string = ('clear','set','randomize','toggle');

type
  { TCondition }

  TCondition = class
    function ToStringAndFree: ansistring;
  end;

  TCustomConditionList = specialize TFPGList<TCondition>;

  { TConditionList }

  TConditionList = class(TCustomConditionList)
    function ToString: ansistring; override;
    procedure FreeAll;
  end;

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TInstruction)
    Switch: integer;
    Value: TSwitchValue;
    constructor Create(ASwitch: integer; AValue: TSwitchValue);
    function ToString: ansistring; override;
  end;

type
  TSetIntegerMode = (simSetTo, simAdd, simSubtract, simRandomize);

  { TSetIntegerInstruction }

  TSetIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  TIntegerTransfer = (itCopyToAccumulator, itAddToAccumulator, itCopyFromAccumulator, itAddFromAccumulator);

  { TTransferIntegerInstruction }

  TTransferIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Action: TIntegerTransfer;
    constructor Create(APlayer: TPlayer; AUnitType: string; AAction: TIntegerTransfer);
    function ToString: ansistring; override;
  end;

  { TAddIntegerFromSwitchesInstruction }

  TAddIntegerFromSwitchesInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Switches: array of integer;
    constructor Create(APlayer: TPlayer; AUnitType: string; ASwitches: array of integer);
    function ToString: ansistring; override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TInstruction)
    Always: boolean;
    Text: string;
    Players: TPlayers;
    constructor Create(AAlways: boolean; AMessage: string; APlayers: TPlayers);
    function ToString: ansistring; override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToString: ansistring; override;
  end;

  { TCallInstruction }

  TCallInstruction = class(TInstruction)
    Name: string;
    Params: array of string;
    constructor Create(AName: string; AParams: array of string);
    constructor Create(AName: string; AParams: TStringList);
    function ToString: ansistring; override;
  end;

  { TJumpReturnInstruction }

  TJumpReturnInstruction = class(TInstruction)
    DestIP, ReturnIP: integer;
    constructor Create(ADestIP, AReturnIP: integer);
    function ToString: ansistring; override;
  end;

  { TReturnInstruction }

  TReturnInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TIfInstruction }

  TIfInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition);
    constructor Create(AConditions: TConditionList);
    function ToString: ansistring; override;
  end;

  { TWaitConditionInstruction }

  TWaitConditionInstruction = class(TInstruction)
    Conditions: TConditionList;
    IP: integer;
    destructor Destroy; override;
    constructor Create(AConditions: TConditionList; AIP: Integer);
    constructor Create(ACondition: TCondition; AIP: Integer);
    function ToString: ansistring; override;
  end;

  { TElseInstruction }

  TElseInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TSplitInstruction }

  TSplitInstruction = class(TInstruction)
    ResumeIP, EndIP: integer;
    constructor Create(AResumeIP, AEndIP: integer);
    function ToString: ansistring; override;
  end;

  { TEndIfInstruction }

  TEndIfInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TChangeIPInstruction }

  TChangeIPInstruction = class(TInstruction)
    IP: integer;
    Preserve: Integer;
    constructor Create(AIP: Integer; APreserve: Integer);
    function ToString: ansistring; override;
  end;

  { TWhileInstruction }

  TWhileInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: TConditionList);
    function ToString: ansistring; override;
  end;

  { TEndWhileInstruction }

  TEndWhileInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TCreateUnitInstruction }

  TCreateUnitInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    Properties: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; AProperties: integer = -1);
    function ToString: ansistring; override;
  end;

  TSetUnitProperty = (supLife, supShield, supEnergy, supResource, supHangarCount, supInvincible, supDoodadState);

  { TSetUnitPropertyInstruction }

  TSetUnitPropertyInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    UnitProperty: TSetUnitProperty;
    Value: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; AProperty: TSetUnitProperty; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TKillUnitInstruction }

  TKillUnitInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    DeathAnimation: boolean;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: string; ALocation: string = ''; ADeathAnimation: boolean = true);
    function ToString: ansistring; override;
  end;

  { TGiveUnitInstruction }

  TGiveUnitInstruction = class(TInstruction)
    Player, DestPlayer: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; ADestPlayer: TPlayer);
    function ToString: ansistring; override;
  end;

  { TTeleportUnitInstruction }

  TTeleportUnitInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    DestLocation: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; ADestLocation: string);
    function ToString: ansistring; override;
  end;

  { TOrderUnitInstruction }

  TOrderUnitInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    DestLocation: string;
    Order: string;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; ADestLocation: string; AOrder: string);
    function ToString: ansistring; override;
  end;

type

  { TAlwaysCondition }

  TAlwaysCondition = class(TCondition)
    function ToString: ansistring; override;
  end;

  { TNeverCondition }

  TNeverCondition = class(TCondition)
    function ToString: ansistring; override;
  end;

  { TNotCondition }

  TNotCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function ToString: ansistring; override;
  end;

  { TSwitchCondition }

  TSwitchCondition = class(TCondition)
    Switch: integer;
    Value: boolean;
    constructor Create(ASwitch: integer; AValue: boolean);
    function ToString: ansistring; override;
  end;

  TIntegerConditionMode = (icmAtLeast,icmAtMost,icmExactly);

const
  IntegerConditionModeToStr: array[TIntegerConditionMode] of string =
    ('At least', 'At most','Exactly');

type
  { TIntegerCondition }

  TIntegerCondition = class(TCondition)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TBringCondition }

  TBringCondition = class(TCondition)
    Player: TPlayer;
    UnitType, Location: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TKillCountCondition }

  TKillCountCondition = class(TCondition)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TOpponentCountCondition }

  TOpponentCountCondition = class(TCondition)
    Player: TPlayer;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AMode: TIntegerConditionMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TElapsedTimeCondition }

  TElapsedTimeCondition = class(TCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TCompareUnitCountCondition }

  TCompareUnitCountCondition = class(TCondition)
    UnitType, Location: string;
    Highest: boolean;
    constructor Create(AUnitType, ALocation: string; AHighest: boolean);
    function ToString: ansistring; override;
  end;

  { TCompareKillCountCondition }

  TCompareKillCountCondition = class(TCondition)
    UnitType: string;
    Highest: boolean;
    constructor Create(AUnitType: string; AHighest: boolean);
    function ToString: ansistring; override;
  end;

  { TCompareIntegerCondition }

  TCompareIntegerCondition = class(TCondition)
    ValueType: string;
    Highest: boolean;
    IsScore, IsResource: boolean;
    constructor Create(AValueType: string; AHighest: boolean);
    function ToString: ansistring; override;
  end;

function PlayerToStr(APlayer: TPlayer): string;

implementation

function AddQuotes(AText: string): string;
begin
  result := '"' + StringReplace(StringReplace(AText, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]) + '"';
end;

function PlayerToStr(APlayer: TPlayer): string;
begin
  case APlayer of
  plNone: result := 'None';
  plPlayer1..plPlayer12: result := 'Player ' + IntToStr(ord(APlayer) - ord(plPlayer1)+1);
  plCurrentPlayer: result := 'Current Player';
  plFoes: result := 'Foes';
  plAllies: result := 'Allies';
  plNeutralPlayers: result := 'Neutral players';
  plAllPlayers: result := 'All players';
  plForce1..plForce4: result := 'Force ' + IntToStr(ord(APlayer) - ord(plForce1)+1);
  plNonAlliedVictoryPlayers: result := 'Non Allied Victory Players';
  else result := 'Unknown';
  end;
end;

{ TCompareIntegerCondition }

constructor TCompareIntegerCondition.Create(AValueType: string; AHighest: boolean);
begin
  ValueType := AValueType;
  Highest:= AHighest;
  IsScore := (CompareText(copy(ValueType,length(ValueType)-5,6),' Score')=0);
  IsResource := (CompareText(ValueType,'ore')=0) or (CompareText(ValueType,'gas')=0) or (CompareText(ValueType,'ore and gas')=0);
  if not IsScore and not IsResource then raise exception.Create('Unknown value type');
end;

function TCompareIntegerCondition.ToString: ansistring;
begin
  if IsScore then
  begin
    if Highest then result := 'Highest' else result := 'Lowest';
    result += ' Score(' + copy(ValueType,1,length(ValueType)-6) + ')'
  end else
  if IsResource then
  begin
    if Highest then result := 'Most' else result := 'Least';
    result += ' Resources(' + ValueType + ')'
  end;
end;

{ TCompareKillCountCondition }

constructor TCompareKillCountCondition.Create(AUnitType: string;
  AHighest: boolean);
begin
  UnitType := AUnitType;
  Highest:= AHighest;
end;

function TCompareKillCountCondition.ToString: ansistring;
begin
  if Highest then result := 'Most' else result := 'Least';
  Result += ' Kills';
  result += '(' + AddQuotes(UnitType) + ')';
end;

{ TElapsedTimeCondition }

constructor TElapsedTimeCondition.Create(AMode: TIntegerConditionMode;
  AValue: integer);
begin
  Mode := AMode;
  Value := AValue;
end;

function TElapsedTimeCondition.ToString: ansistring;
begin
  Result:= 'Elapsed Time(' + IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

{ TOpponentCountCondition }

constructor TOpponentCountCondition.Create(APlayer: TPlayer;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  Mode := AMode;
  Value := AValue;
end;

function TOpponentCountCondition.ToString: ansistring;
begin
  Result:= 'Opponents("' + PlayerToStr(Player) + '", ' +
       IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

{ TKillCountCondition }

constructor TKillCountCondition.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TKillCountCondition.ToString: ansistring;
begin
  Result:= 'Kill("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' +
       IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

{ TCompareUnitCountCondition }

constructor TCompareUnitCountCondition.Create(AUnitType,
  ALocation: string; AHighest: boolean);
begin
  UnitType := AUnitType;
  Location:= ALocation;
  Highest:= AHighest;
end;

function TCompareUnitCountCondition.ToString: ansistring;
begin
  Result:= 'Command ';
  if Highest then result += 'the Most' else result += 'the Least';
  if Location <> '' then result += ' At(' + AddQuotes(UnitType) + ', ' +AddQuotes(Location)+')'
  else result += '(' + AddQuotes(UnitType) + ')';
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

function TNotCondition.ToString: ansistring;
begin
  Result:= '!' + Conditions.ToString;
end;

{ TOrderUnitInstruction }

constructor TOrderUnitInstruction.Create(APlayer: TPlayer; AUnitType,
  ALocation: string; ADestLocation: string; AOrder: string);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  DestLocation:= ADestLocation;
  Order := AOrder;
end;

function TOrderUnitInstruction.ToString: ansistring;
begin
  result := 'Order(' + AddQuotes(PlayerToStr(Player)) + ', '+AddQuotes(UnitType);
  result += ', '+AddQuotes(Location)+', '+AddQuotes(DestLocation)+', ' + Order + ')';
end;

{ TTeleportUnitInstruction }

constructor TTeleportUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer; AUnitType,
  ALocation: string; ADestLocation: string);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player:= APlayer;
  Quantity := AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  DestLocation:= ADestLocation;
end;

function TTeleportUnitInstruction.ToString: ansistring;
begin
  Result:= 'Move Unit(' + AddQuotes(PlayerToStr(Player)) + ', '+AddQuotes(UnitType)+', ';
  if Quantity = -1 then result +='All' else result += inttostr(Quantity);
  result += ', '+AddQuotes(Location)+', '+AddQuotes(DestLocation)+')';
end;

{ TSetUnitPropertyInstruction }

constructor TSetUnitPropertyInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType, ALocation: string;
  AProperty: TSetUnitProperty; AValue: integer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player := APlayer;
  Quantity := AQuantity;
  UnitType := AUnitType;
  Location := ALocation;
  UnitProperty := AProperty;
  Value := AValue;
  if (AProperty = supResource) and (CompareText(AUnitType, 'Any unit')<>0) then
    raise exception.Create('Setting resource amount cannot be applied to a specific unit');
  if (AProperty in[supInvincible,supDoodadState]) and (AQuantity <> -1) then
    raise exception.Create('This property can only be applied with quantity All');
  if (AProperty in [supLife, supShield, supEnergy]) and ((AValue < 0) or (AValue > 100)) then
    raise exception.Create('Property value out of bounds (0 to 100)');
end;

function TSetUnitPropertyInstruction.ToString: ansistring;
begin
  case UnitProperty of
  supLife: result := 'Modify Unit Hit Points';
  supShield: result := 'Modify Unit Shield Points';
  supEnergy: result := 'Modify Unit Energy';
  supResource:
    begin
      result := 'Modify Unit Resource Amount';
      result += '(' + AddQuotes(PlayerToStr(Player))+', '+inttostr(Value)+', ';
      result += inttostr(Quantity)+', '+AddQuotes(Location)+')';
      exit;
    end;
  supHangarCount: result := 'Modify Unit Hanger Count';
  supInvincible, supDoodadState:
    begin
      if UnitProperty = supInvincible then
        result := 'Set Invincibility'
      else
        result := 'Set Doodad State';

      result += '(' + AddQuotes(PlayerToStr(Player))+', '+AddQuotes(UnitType)+', '+AddQuotes(Location)+', ';
      case Value of
      0: result += 'disabled';
      1: result += 'enabled';
      else result += 'toggle';
      end;
      result += ')';
      exit;
    end
  else
    raise exception.Create('Case not handled');
  end;
  result += '(' + AddQuotes(PlayerToStr(Player))+', '+AddQuotes(UnitType)+', '+inttostr(Value)+', ';
  if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
  result += ', '+AddQuotes(Location)+')';
end;

{ TGiveUnitInstruction }

constructor TGiveUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType, ALocation: string; ADestPlayer: TPlayer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player := APlayer;
  Quantity := AQuantity;
  UnitType := AUnitType;
  Location := ALocation;
  DestPlayer := ADestPlayer;
end;

function TGiveUnitInstruction.ToString: ansistring;
begin
  result := 'Give Units to Player(' + AddQuotes(PlayerToStr(Player))+', '+ AddQuotes(PlayerToStr(DestPlayer))+ ', ' +
    AddQuotes(UnitType)+', ';
  if Quantity = -1 then result += 'All' else result += IntToStr(Quantity);
  result += ', ' + AddQuotes(Location) + ')';
end;

{ TKillUnitInstruction }

constructor TKillUnitInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType, ALocation: string; ADeathAnimation: boolean);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player:= APlayer;
  Quantity:= AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  DeathAnimation:= ADeathAnimation;
end;

function TKillUnitInstruction.ToString: ansistring;
begin
  if DeathAnimation then result := 'Kill Unit' else result := 'Remove Unit';
  if (Location<>'') or (Quantity <> -1) then
  begin
    Result += ' At Location';
    result += '("'+ PlayerToStr(Player)+'", ' + AddQuotes(UnitType)+', ';
    if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
    result += ', '+AddQuotes(Location) + ')';
  end
  else
    Result += '(' + AddQuotes(PlayerToStr(Player))+', '+AddQuotes(UnitType)+')';
end;

{ TSplitInstruction }

constructor TSplitInstruction.Create(AResumeIP, AEndIP: integer);
begin
  ResumeIP:= AResumeIP;
  EndIP := AEndIP;
end;

function TSplitInstruction.ToString: ansistring;
begin
  Result:= '}{';
end;

{ TBringCondition }

constructor TBringCondition.Create(APlayer: TPlayer; AUnitType, ALocation: string;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
  Location:= ALocation;
end;

function TBringCondition.ToString: ansistring;
begin
  if Location = '' then
    Result:= 'Command("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' +
         IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Bring("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' + AddQuotes(Location) + ', ' +
         IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

{ TCreateUnitInstruction }

constructor TCreateUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType, ALocation: string; AProperties: integer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player:= APlayer;
  Quantity:= AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  Properties:= AProperties;
end;

function TCreateUnitInstruction.ToString: ansistring;
begin
  Result:= 'Create Unit';

  if Properties >= 0 then result += ' with Properties';

  result += '("'+ PlayerToStr(Player)+'", ' + AddQuotes(UnitType)+', ';
  if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
  result += ', '+AddQuotes(Location);
  if Properties >=0 then result += ', ' + inttostr(Properties+1);

  result += ')';
end;

{ TTransferIntegerInstruction }

constructor TTransferIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: string; AAction: TIntegerTransfer);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Action:= AAction;
end;

function TTransferIntegerInstruction.ToString: ansistring;
var modeStr: string;
begin
  case Action of
  itAddToAccumulator: modeStr := 'Add to acc';
  itCopyFromAccumulator: modeStr := 'Copy from acc';
  else modeStr := 'Copy to acc';
  end;
  Result:= 'TransferInteger("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' + modeStr + ')';
end;

{ TConditionList }

function TConditionList.ToString: ansistring;
var i: integer;
begin
  result := '';
  for i := 0 to Count-1 do
  begin
    if i > 0 then result += ' And ';
    result += Items[i].ToString;
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

function TWaitConditionInstruction.ToString: ansistring;
begin
  Result:='Wait(' + Conditions.ToString + ')';
end;

{ TChangeIPInstruction }

constructor TChangeIPInstruction.Create(AIP: Integer; APreserve: Integer);
begin
  IP := AIP;
  Preserve := APreserve;
end;

function TChangeIPInstruction.ToString: ansistring;
begin
  Result:=inherited ToString;
end;

{ TNeverCondition }

function TNeverCondition.ToString: ansistring;
begin
  Result:= 'Never()';
end;

{ TEndIfInstruction }

constructor TEndIfInstruction.Create;
begin
  //nothing
end;

function TEndIfInstruction.ToString: ansistring;
begin
  Result:= '}';
end;

{ TElseInstruction }

constructor TElseInstruction.Create;
begin
  //nothing
end;

function TElseInstruction.ToString: ansistring;
begin
  Result:= '} Else {';
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

function TIfInstruction.ToString: ansistring;
begin
  Result:= 'If ('+ Conditions.ToString+'){';
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

function TWhileInstruction.ToString: ansistring;
begin
  Result:= 'While ('+ Conditions.ToString+')';
end;

{ TEndWhileInstruction }

constructor TEndWhileInstruction.Create;
begin
 //nothing
end;

function TEndWhileInstruction.ToString: ansistring;
begin
  Result:= '}';
end;

{ TReturnInstruction }

constructor TReturnInstruction.Create;
begin
  //nothing
end;

function TReturnInstruction.ToString: ansistring;
begin
  Result:= 'Return';
end;

{ TJumpReturnInstruction }

constructor TJumpReturnInstruction.Create(ADestIP, AReturnIP: integer);
begin
  DestIP:= ADestIP;
  ReturnIP:= AReturnIP;
end;

function TJumpReturnInstruction.ToString: ansistring;
begin
  result := '';
end;

{ TCallInstruction }

constructor TCallInstruction.Create(AName: string; AParams: array of string);
var
  i: Integer;
begin
  Name := AName;
  setlength(Params, length(AParams));
  for i := 0 to high(AParams) do
    Params[i] := AParams[i];
end;

constructor TCallInstruction.Create(AName: string; AParams: TStringList);
var
  i: Integer;
begin
  Name := AName;
  setlength(Params, AParams.Count);
  for i := 0 to AParams.Count-1 do
    Params[i] := AParams[i];
end;

function TCallInstruction.ToString: ansistring;
var
  i: Integer;
begin
  Result:= Name+'(';
  for i := 0 to high(Params) do
  begin
    if i > 0 then result += ', ';
    Result += Params[i];
  end;
  result += ')';
end;

{ TWaitInstruction }

constructor TWaitInstruction.Create(ADelayMs: integer);
begin
  DelayMs:= ADelayMs;
end;

function TWaitInstruction.ToString: ansistring;
begin
  Result:= 'Wait('+IntToStr(DelayMs)+')';
end;

{ TDisplayTextMessageInstruction }

constructor TDisplayTextMessageInstruction.Create(AAlways: boolean;
  AMessage: string; APlayers: TPlayers);
begin
  Always:= AAlways;
  Text:= AMessage;
  Players:= APlayers;
end;

function TDisplayTextMessageInstruction.ToString: ansistring;
begin
  Result := 'Display Text Message(' + BoolToStr(Always, 'Always Display', 'Don''t Always Display') +
            ', ' + AddQuotes(Text) + ')';
end;

{ TAddIntegerFromSwitchesInstruction }

constructor TAddIntegerFromSwitchesInstruction.Create(APlayer: TPlayer;
  AUnitType: string; ASwitches: array of integer);
var
  i: Integer;
begin
  Player := APlayer;
  UnitType:= AUnitType;
  setlength(Switches, length(ASwitches));
  for i := 0 to high(ASwitches) do
    Switches[i] := ASwitches[i];
end;

function TAddIntegerFromSwitchesInstruction.ToString: ansistring;
begin
  Result:= 'Set Deaths("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', Add, switches)'; //not a real instruction
end;

{ TCondition }

function TCondition.ToStringAndFree: ansistring;
begin
  result := ToString;
  Free;
end;

{ TInstruction }

function TInstruction.ToStringAndFree: ansistring;
begin
  result := ToString;
  Free;
end;

{ TIntegerCondition }

constructor TIntegerCondition.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TIntegerCondition.ToString: ansistring;
var
  modeStr: string;
begin
  modeStr := IntegerConditionModeToStr[Mode];
  if (CompareText(UnitType,'ore')=0) or (CompareText(UnitType,'gas')=0) or (CompareText(UnitType,'ore and gas')=0) then
      Result:= 'Accumulate("' + PlayerToStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + LowerCase(UnitType) + ')'
  else
  if CompareText(copy(UnitType,length(UnitType)-5,6),' Score')=0 then
    result := 'Score("' + PlayerToStr(Player) + '", ' + copy(UnitType,1,length(UnitType)-6) + ', ' + modeStr + ', ' + IntToStr(Value) + ')'
  else
  if CompareText(UnitType,'Countdown')=0 then
    result := 'Countdown Timer(' + modeStr + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Deaths("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' + modeStr + ', ' + IntToStr(Value) + ')';
end;

{ TAlwaysCondition }

function TAlwaysCondition.ToString: ansistring;
begin
  Result:= 'Always()';
end;

{ TSwitchCondition }

constructor TSwitchCondition.Create(ASwitch: integer; AValue: boolean);
begin
  Switch := ASwitch;
  Value := AValue;
end;

function TSwitchCondition.ToString: ansistring;
begin
  Result:= 'Switch("Switch' + IntToStr(Switch) + '", ' + BoolToStr(Value, 'set', 'not set') + ')';
end;

{ TSetIntegerInstruction }

constructor TSetIntegerInstruction.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TSetIntegerMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TSetIntegerInstruction.ToString: ansistring;
var
  modeStr: string;
begin
  case Mode of
  simSetTo: modeStr := 'Set To';
  simSubtract: modeStr := 'Subtract';
  simRandomize: modeStr := 'Randomize';
  else
    {simAdd: }modeStr := 'Add';
  end;
  if (CompareText(UnitType,'Ore')=0) or (CompareText(UnitType,'Gas')=0) or (CompareText(UnitType,'Ore And Gas')=0) then
    Result:= 'Set Resources("' + PlayerToStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + LowerCase(UnitType) + ')'
  else
  if CompareText(copy(UnitType,length(UnitType)-5,6),' Score')=0 then
    result := 'Set Score("' + PlayerToStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + copy(UnitType,1,length(UnitType)-6) + ')'
  else
  if CompareText(UnitType,'Countdown')= 0 then
    result := 'Set Countdown Timer(' + modeStr +', ' + IntToStr(Value) + ')'
  else
    Result:= 'Set Deaths("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' + modeStr + ', ' + IntToStr(Value) + ')';
end;

{ TEmptyInstruction }

function TEmptyInstruction.ToString: ansistring;
begin
  Result:= '';
end;

{ TSetSwitchInstruction }

constructor TSetSwitchInstruction.Create(ASwitch: integer;
  AValue: TSwitchValue);
begin
  Switch:= ASwitch;
  Value:= AValue;
end;

function TSetSwitchInstruction.ToString: ansistring;
begin
  Result:= 'Set Switch("Switch' + IntToStr(Switch) + '", ' + SwitchToStr[Value] + ')';
end;

end.

