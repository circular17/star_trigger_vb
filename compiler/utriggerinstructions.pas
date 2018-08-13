unit utriggerinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes;

type
  { TTriggerInstruction }

  TTriggerInstruction = class(TInstruction)
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual; abstract;
  end;

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TTriggerInstruction)
    Switch: integer;
    Value: TSwitchValue;
    constructor Create(ASwitch: integer; AValue: TSwitchValue);
    function ToTrigEdit: string; override;
  end;

  { TSetDeathInstruction }

  TSetDeathInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  { TSetResourceInstruction }

  TSetResourceInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Resource: TStarcraftResource;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AResource: TStarcraftResource; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  { TSetScoreInstruction }

  TSetScoreInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Score: TStarcraftScore;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AScore: TStarcraftScore; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  { TSetCountdownInstruction }

  TSetCountdownInstruction = class(TTriggerInstruction)
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TTriggerInstruction)
    Always: boolean;
    Text: string;
    constructor Create(AAlways: boolean; AMessage: string);
    function ToTrigEdit: string; override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TTriggerInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToTrigEdit: string; override;
  end;

  { TCreateUnitInstruction }

  TCreateUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    Properties: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; AProperties: integer = -1);
    function ToTrigEdit: string; override;
  end;

  TSetUnitProperty = (supLife, supShield, supEnergy, supResource, supHangarCount);

  { TSetUnitPropertyInstruction }

  TSetUnitPropertyInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    UnitProperty: TSetUnitProperty;
    Value: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; AProperty: TSetUnitProperty; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  TSetUnitFlag = (sufInvincible, sufDoodadState);
  TUnitFlagValue = (ufvEnable, ufvDisable, ufvToggle);

  { TSetUnitFlagInstruction }

  TSetUnitFlagInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    Flag: TSetUnitFlag;
    Value: TUnitFlagValue;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; AFlag: TSetUnitFlag; AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
  end;

  { TKillUnitInstruction }

  TKillUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    DeathAnimation: boolean;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: string; ALocation: string = ''; ADeathAnimation: boolean = true);
    function ToTrigEdit: string; override;
  end;

  { TGiveUnitInstruction }

  TGiveUnitInstruction = class(TTriggerInstruction)
    Player, DestPlayer: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; ADestPlayer: TPlayer);
    function ToTrigEdit: string; override;
  end;

  { TTeleportUnitInstruction }

  TTeleportUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    DestLocation: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; ADestLocation: string);
    function ToTrigEdit: string; override;
  end;

  { TMoveLocationInstruction }

  TMoveLocationInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    LocationToChange: string;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; ALocationToChange: string);
    function ToTrigEdit: string; override;
  end;

  TUnitOrder = (uoMove, uoPatrol, uoAttack);

  { TOrderUnitInstruction }

  TOrderUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    DestLocation: string;
    Order: TUnitOrder;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; ADestLocation: string; AOrder: TUnitOrder);
    function ToTrigEdit: string; override;
  end;

  { TPlayWAVInstruction }

  TPlayWAVInstruction = class(TTriggerInstruction)
    Filename: string;
    DurationMs: integer;
    constructor Create(AFilename: string; ADurationMs: integer);
    function ToTrigEdit: string; override;
  end;

  { TTalkingPortraitInstruction }

  TTalkingPortraitInstruction = class(TTriggerInstruction)
    UnitType: string;
    DurationMs: integer;
    constructor Create(AUnitType: string; ADurationMs: integer);
    function ToTrigEdit: string; override;
  end;

  { TRunAIScriptInstruction }

  TRunAIScriptInstruction = class(TTriggerInstruction)
    ScriptCode, Location: string;
    constructor Create(AScriptCode, ALocation: string);
    function ToTrigEdit: string; override;
  end;

  { TSetMissionObjectivesInstruction }

  TSetMissionObjectivesInstruction = class(TTriggerInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToTrigEdit: string; override;
  end;

  { TSetNextScenarioInstruction }

  TSetNextScenarioInstruction = class(TTriggerInstruction)
    Scenario: string;
    constructor Create(AScenario: string);
    function ToTrigEdit: string; override;
  end;

  { TCenterViewInstruction }

  TCenterViewInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
  end;

  { TMinimapPingInstruction }

  TMinimapPingInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
  end;

  { TLeaderBoardIncludeComputersInstruction }

  TLeaderBoardIncludeComputersInstruction = class(TTriggerInstruction)
    Value: TUnitFlagValue;
    constructor Create(AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardOreAndGasIconInstruction }

  TShowLeaderboardOreAndGasIconInstruction = class(TTriggerInstruction)
    Amount: integer;
    constructor Create(AAmount: integer);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardResourceInstruction }

  TShowLeaderboardResourceInstruction = class(TTriggerInstruction)
    Text: string;
    Resource: TStarcraftResource;
    Goal: integer;
    constructor Create(AText: string; AResource: TStarcraftResource; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardScoreInstruction }

  TShowLeaderboardScoreInstruction = class(TTriggerInstruction)
    Text: string;
    Score: TStarcraftScore;
    Goal: integer;
    constructor Create(AText: string; AScore: TStarcraftScore; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardKillCountInstruction }

  TShowLeaderboardKillCountInstruction = class(TTriggerInstruction)
    Text, UnitType: string;
    Goal: integer;
    constructor Create(AText,AUnitType: string; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardUnitCountInstruction }

  TShowLeaderboardUnitCountInstruction = class(TTriggerInstruction)
    Text, UnitType,Location: string;
    Goal: integer;
    constructor Create(AText,AUnitType,ALocation: string; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  TEndGameMode = (egDefeat, egDraw, egVictory);

  { TEndGameInstruction }

  TEndGameInstruction = class(TTriggerInstruction)
    Mode: TEndGameMode;
    constructor Create(AMode: TEndGameMode);
    function ToTrigEdit: string; override;
  end;

  { TUnitSpeechInstruction }

  TUnitSpeechInstruction = class(TTriggerInstruction)
    Active: boolean;
    constructor Create(AActive: Boolean);
    function ToTrigEdit: string; override;
  end;

  { TPauseGameInstruction }

  TPauseGameInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
  end;

  { TPauseCountdownInstruction }

  TPauseCountdownInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
  end;

  TAllianceStatus = (asEnnemy, asAlly, asAlliedVictory);

  { TSetAllianceStatus }

  TSetAllianceStatus = class(TTriggerInstruction)
    Player: TPlayer;
    Status: TAllianceStatus;
    constructor Create(APlayer: TPlayer; AStatus: TAllianceStatus);
    function ToTrigEdit: string; override;
  end;

function SetIntegerModeToStr(AMode: TSetIntegerMode): string;
function StarcraftResourceToStr(AResource: TStarcraftResource): string;
function StarcraftScoreToStr(AScore: TStarcraftScore): string;

const
  SwitchValueToStr : array[TSwitchValue] of string = ('clear','set','randomize','toggle');

implementation

function SetIntegerModeToStr(AMode: TSetIntegerMode): string;
begin
  case AMode of
  simSetTo: result := 'Set To';
  simSubtract: result := 'Subtract';
  else
    {simAdd: }result := 'Add';
  end;
end;

function StarcraftResourceToStr(AResource: TStarcraftResource): string;
begin
  case AResource of
  srGas: result := 'gas';
  srOreAndGas: result := 'ore and gas';
  else {srOre:} result := 'ore';
  end;
end;

function StarcraftScoreToStr(AScore: TStarcraftScore): string;
begin
  case AScore of
  ssUnitScore: result := 'Units';
  ssBuildingScore: result := 'Buildings';
  ssUnitAndBuildingScore: result := 'Units and buidlings';
  ssKillScore: result := 'Kills';
  ssRazingScore: result := 'Razings';
  ssKillAndRazingScore: result := 'Kills and razings';
  ssTotalScore: result := 'Total';
  ssCustomScore: result := 'Custom';
  end;
end;

function CreateSetIntegerInstructionImplementation(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer): TInstruction;
begin
  if AValue < 0 then
  begin
    if AMode = simSubtract then
    begin
      AValue := -AValue;
      AMode := simAdd;
    end else
    if AMode = simAdd then
    begin
      AValue := -AValue;
      AMode := simSubtract;
    end else
      AValue := 0;
  end;

  if CompareText(AUnitType,'Ore')=0 then
    result := TSetResourceInstruction.Create(APlayer,srOre,AMode,AValue) else
  if CompareText(AUnitType,'Gas')=0 then
    result := TSetResourceInstruction.Create(APlayer,srGas,AMode,AValue) else
  if CompareText(AUnitType,'Ore And Gas')=0 then
    result := TSetResourceInstruction.Create(APlayer,srOreAndGas,AMode,AValue) else
  if CompareText(copy(AUnitType,length(AUnitType)-5,6),' Score')=0 then
  begin
    if compareText(AUnitType, 'Units Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssUnitScore,AMode,AValue) else
    if compareText(AUnitType, 'Buildings Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssBuildingScore,AMode,AValue) else
    if compareText(AUnitType, 'Units and buildings Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssUnitAndBuildingScore,AMode,AValue) else
    if compareText(AUnitType, 'Kills Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssKillScore,AMode,AValue) else
    if compareText(AUnitType, 'Razings Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssRazingScore,AMode,AValue) else
    if compareText(AUnitType, 'Kills and razings Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssKillAndRazingScore,AMode,AValue) else
    if compareText(AUnitType, 'Custom Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssCustomScore,AMode,AValue) else
    if compareText(AUnitType, 'Total Score')=0 then
      result := TSetScoreInstruction.Create(APlayer,ssTotalScore,AMode,AValue) else
        raise exception.Create('Unknown score type "'+AUnitType+'"');
  end
  else
  if CompareText(AUnitType,'Countdown')= 0 then
    result := TSetCountdownInstruction.Create(AMode,AValue)
  else
    Result:= TSetDeathInstruction.Create(APlayer, AUnitType, AMode, AValue);
end;

{ TTriggerInstruction }

function TTriggerInstruction.ToTrigEditAndFree: string;
begin
  if self <> nil then
  begin
    result := ToTrigEdit;
    Free;
  end else
    result := '';
end;

{ TSetSwitchInstruction }

constructor TSetSwitchInstruction.Create(ASwitch: integer;
  AValue: TSwitchValue);
begin
  Switch:= ASwitch;
  Value:= AValue;
end;

function TSetSwitchInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Switch('+AddTrigEditQuotes(SwitchToStr(Switch))+ ', ' + SwitchValueToStr[Value] + ')';
end;

{ TSetDeathInstruction }

constructor TSetDeathInstruction.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TSetIntegerMode; AValue: integer);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Value:= AValue;
  Mode:= AMode;
end;

function TSetDeathInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Deaths("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ')';
end;

{ TSetResourceInstruction }

constructor TSetResourceInstruction.Create(APlayer: TPlayer; AResource: TStarcraftResource;
  AMode: TSetIntegerMode; AValue: integer);
begin
  Player:= APlayer;
  Resource:= AResource;
  Value:= AValue;
  Mode:= AMode;
end;

function TSetResourceInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Resources("' + PlayerToTrigEditStr(Player) + '", ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ', ' + StarcraftResourceToStr(Resource) + ')'
end;

{ TSetScoreInstruction }

constructor TSetScoreInstruction.Create(APlayer: TPlayer;
  AScore: TStarcraftScore; AMode: TSetIntegerMode; AValue: integer);
begin
  Player:= APlayer;
  Score:= AScore;
  Value:= AValue;
  Mode:= AMode;
end;

function TSetScoreInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Score("' + PlayerToTrigEditStr(Player) + '", ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ', ' + StarcraftScoreToStr(Score) + ')'
end;


{ TSetCountdownInstruction }

constructor TSetCountdownInstruction.Create(AMode: TSetIntegerMode;
  AValue: integer);
begin
  Mode := AMode;
  Value := AValue;
end;

function TSetCountdownInstruction.ToTrigEdit: string;
begin
  result := 'Set Countdown Timer(' + SetIntegerModeToStr(Mode) +', ' + IntToStr(Value) + ')'
end;

{ TDisplayTextMessageInstruction }

constructor TDisplayTextMessageInstruction.Create(AAlways: boolean;
  AMessage: string);
begin
  Always:= AAlways;
  Text:= AMessage;
end;

function TDisplayTextMessageInstruction.ToTrigEdit: string;
begin
  Result := 'Display Text Message(' + BoolToStr(Always, 'Always Display', 'Don''t Always Display') +
            ', ' + AddTrigEditQuotes(Text) + ')';
end;

{ TWaitInstruction }

constructor TWaitInstruction.Create(ADelayMs: integer);
begin
  DelayMs:= ADelayMs;
end;

function TWaitInstruction.ToTrigEdit: string;
begin
  Result:= 'Wait('+IntToStr(DelayMs)+')';
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

function TCreateUnitInstruction.ToTrigEdit: string;
begin
  Result:= 'Create Unit';
  if Properties >= 0 then result += ' with Properties';

  result += '("'+ PlayerToTrigEditStr(Player)+'", ' + AddTrigEditQuotes(UnitType)+', ';
  result += inttostr(Quantity) + ', '+AddTrigEditQuotes(Location);
  if Properties >=0 then result += ', ' + inttostr(Properties+1);

  result += ')';
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
  if (AProperty in [supLife, supShield, supEnergy]) and ((AValue < 0) or (AValue > 100)) then
    raise exception.Create('Property value out of bounds (0 to 100)')
  else if (AValue < 0) or (AValue > 65535) then
    raise exception.Create('Property value out of bounds (0 to 65535)')
end;

function TSetUnitPropertyInstruction.ToTrigEdit: string;
begin
  case UnitProperty of
  supLife: result := 'Modify Unit Hit Points';
  supShield: result := 'Modify Unit Shield Points';
  supEnergy: result := 'Modify Unit Energy';
  supResource:
    begin
      result := 'Modify Unit Resource Amount';
      result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+inttostr(Value)+', ';
      result += inttostr(Quantity)+', '+AddTrigEditQuotes(Location)+')';
      exit;
    end;
  supHangarCount: result := 'Modify Unit Hanger Count';
  else
    raise exception.Create('Case not handled');
  end;
  result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+AddTrigEditQuotes(UnitType)+', '+inttostr(Value)+', ';
  if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
  result += ', '+AddTrigEditQuotes(Location)+')';
end;

{ TSetUnitFlagInstruction }

constructor TSetUnitFlagInstruction.Create(APlayer: TPlayer; AUnitType,
  ALocation: string; AFlag: TSetUnitFlag; AValue: TUnitFlagValue);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Location := ALocation;
  Flag := AFlag;
  Value := AValue;
end;

function TSetUnitFlagInstruction.ToTrigEdit: string;
begin
  if Flag = sufInvincible then
    result := 'Set Invincibility'
  else
    result := 'Set Doodad State';

  result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+AddTrigEditQuotes(UnitType)+', '+AddTrigEditQuotes(Location)+', ';
  case Value of
  ufvDisable: result += 'disabled';
  ufvEnable: result += 'enabled';
  else result += 'toggle';
  end;
  result += ')';
end;

{ TKillUnitInstruction }

constructor TKillUnitInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType: string; ALocation: string; ADeathAnimation: boolean);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player:= APlayer;
  Quantity:= AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  DeathAnimation:= ADeathAnimation;
  If IsAnywhere(Location) then Location := AnywhereLocation;
end;

function TKillUnitInstruction.ToTrigEdit: string;
begin
  if DeathAnimation then result := 'Kill Unit' else result := 'Remove Unit';
  if not IsAnywhere(Location) or (Quantity <> -1) then
  begin
    Result += ' At Location';
    result += '("'+ PlayerToTrigEditStr(Player)+'", ' + AddTrigEditQuotes(UnitType)+', ';
    if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
    result += ', '+AddTrigEditQuotes(Location) + ')';
  end
  else
    Result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+AddTrigEditQuotes(UnitType)+')';
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

function TGiveUnitInstruction.ToTrigEdit: string;
begin
  result := 'Give Units to Player(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+ AddTrigEditQuotes(PlayerToTrigEditStr(DestPlayer))+ ', ' +
    AddTrigEditQuotes(UnitType)+', ';
  if Quantity = -1 then result += 'All' else result += IntToStr(Quantity);
  result += ', ' + AddTrigEditQuotes(Location) + ')';
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

function TTeleportUnitInstruction.ToTrigEdit: string;
begin
  Result:= 'Move Unit(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player)) + ', '+AddTrigEditQuotes(UnitType)+', ';
  if Quantity = -1 then result +='All' else result += inttostr(Quantity);
  result += ', '+AddTrigEditQuotes(Location)+', '+AddTrigEditQuotes(DestLocation)+')';
end;

{ TMoveLocationInstruction }

constructor TMoveLocationInstruction.Create(APlayer: TPlayer; AUnitType,
  ALocation: string; ALocationToChange: string);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  LocationToChange:= ALocationToChange;
end;

function TMoveLocationInstruction.ToTrigEdit: string;
begin
  Result:= 'Move Location(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player)) + ', '+AddTrigEditQuotes(UnitType)+', ';
  result += AddTrigEditQuotes(Location)+', '+AddTrigEditQuotes(LocationToChange)+')';
end;

{ TOrderUnitInstruction }

constructor TOrderUnitInstruction.Create(APlayer: TPlayer; AUnitType,
  ALocation: string; ADestLocation: string; AOrder: TUnitOrder);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  DestLocation:= ADestLocation;
  Order := AOrder;
end;

function TOrderUnitInstruction.ToTrigEdit: string;
begin
  result := 'Order(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player)) + ', '+AddTrigEditQuotes(UnitType);
  result += ', '+AddTrigEditQuotes(Location)+', '+AddTrigEditQuotes(DestLocation)+', ';
  case Order of
  uoPatrol: result += 'patrol';
  uoAttack: result += 'attack';
  else result += 'move';
  end;
  result += ')';
end;

{ TPlayWAVInstruction }

constructor TPlayWAVInstruction.Create(AFilename: string; ADurationMs: integer);
begin
  Filename:= AFilename;
  DurationMs:= ADurationMs;
end;

function TPlayWAVInstruction.ToTrigEdit: string;
begin
  Result:= 'Play WAV(' + AddTrigEditQuotes(Filename) + ', ' + inttostr(DurationMs) + ')';
end;

{ TTalkingPortraitInstruction }

constructor TTalkingPortraitInstruction.Create(AUnitType: string;
  ADurationMs: integer);
begin
  UnitType:= AUnitType;
  DurationMs:= ADurationMs;
end;

function TTalkingPortraitInstruction.ToTrigEdit: string;
begin
  Result:= 'Talking Portrait(' + AddTrigEditQuotes(UnitType) + ', ' + inttostr(DurationMs) + ')';
end;

{ TRunAIScriptInstruction }

constructor TRunAIScriptInstruction.Create(AScriptCode, ALocation: string);
begin
  ScriptCode:= AScriptCode;
  Location:= ALocation;
end;

function TRunAIScriptInstruction.ToTrigEdit: string;
begin
  Result := 'Run AI Script';
  if not IsAnywhere(Location) then Result += ' At Location';
  Result += '(' + AddTrigEditQuotes(ScriptCode);
  if not IsAnywhere(Location) then Result += ', ' + AddTrigEditQuotes(Location);
  result += ')';
end;

{ TSetMissionObjectivesInstruction }

constructor TSetMissionObjectivesInstruction.Create(AText: string);
begin
  Text := AText;
end;

function TSetMissionObjectivesInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Mission Objectives(' + AddTrigEditQuotes(Text) + ')';
end;

{ TSetNextScenarioInstruction }

constructor TSetNextScenarioInstruction.Create(AScenario: string);
begin
  Scenario:= AScenario;
end;

function TSetNextScenarioInstruction.ToTrigEdit: string;
begin
  Result:='Set Next Scenario(' + AddTrigEditQuotes(Scenario) + ')';
end;

{ TCenterViewInstruction }

constructor TCenterViewInstruction.Create(ALocation: string);
begin
  Location:= ALocation;
end;

function TCenterViewInstruction.ToTrigEdit: string;
begin
  Result:= 'Center View(' + AddTrigEditQuotes(Location) + ')';
end;

{ TMinimapPingInstruction }

constructor TMinimapPingInstruction.Create(ALocation: string);
begin
  Location:= ALocation;
end;

function TMinimapPingInstruction.ToTrigEdit: string;
begin
  Result:= 'Minimap Ping(' + AddTrigEditQuotes(Location) + ')';
end;

{ TLeaderBoardIncludeComputersInstruction }

constructor TLeaderBoardIncludeComputersInstruction.Create(AValue: TUnitFlagValue);
begin
  Value:= AValue;
end;

function TLeaderBoardIncludeComputersInstruction.ToTrigEdit: string;
begin
  Result:='Leaderboard Computer Players(';
  if Value = ufvDisable then result += 'disable'
  else if Value = ufvEnable then result += 'enable'
  else result += 'toggle';
  result += ')';
end;

{ TShowLeaderboardOreAndGasIconInstruction }

constructor TShowLeaderboardOreAndGasIconInstruction.Create(AAmount: integer);
begin
  Amount := AAmount;
end;

function TShowLeaderboardOreAndGasIconInstruction.ToTrigEdit: string;
begin
  Result:='Leaderboard Greed(' + intTostr(Amount)+')';
end;

{ TShowLeaderboardResourceInstruction }

constructor TShowLeaderboardResourceInstruction.Create(AText: string;
  AResource: TStarcraftResource; AGoal: integer);
begin
  Text:= AText;
  Resource:= AResource;
  Goal:= AGoal;
end;

function TShowLeaderboardResourceInstruction.ToTrigEdit: string;
begin
  Result:=inherited ToString;
  if Goal = -1 then
    result := 'Leader Board Resources'
  else
    result := 'Leaderboard Goal Resources';
  result += '(' + AddTrigEditQuotes(Text) + ', ' + StarcraftResourceToStr(Resource);
  if Goal <> -1 then result += ', ' + inttostr(Goal);
  result += ')';
end;

{ TShowLeaderboardScoreInstruction }

constructor TShowLeaderboardScoreInstruction.Create(AText: string;
  AScore: TStarcraftScore; AGoal: integer);
begin
  Text:= AText;
  Score:= AScore;
  Goal:= AGoal;
end;

function TShowLeaderboardScoreInstruction.ToTrigEdit: string;
begin
  if Goal = -1 then
    result := 'Leader Board Points'
  else
    result := 'Leaderboard Goal Points';
  result += '(' + AddTrigEditQuotes(Text) + ', ' + StarcraftScoreToStr(Score);
  if Goal <> -1 then result += ', ' + inttostr(Goal);
  result += ')';
end;

{ TShowLeaderboardKillCountInstruction }

constructor TShowLeaderboardKillCountInstruction.Create(AText,
  AUnitType: string; AGoal: integer);
begin
  Text:= AText;
  UnitType := AUnitType;
  Goal := AGoal;
end;

function TShowLeaderboardKillCountInstruction.ToTrigEdit: string;
begin
  if Goal <> -1 then
    result := 'Leaderboard Goal Kills'
  else
    result := 'Leader Board Kills';

  result += '(' + AddTrigEditQuotes(Text) + ', '+ AddTrigEditQuotes(UnitType);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);
  result += ')';
end;

{ TShowLeaderboardUnitCountInstruction }

constructor TShowLeaderboardUnitCountInstruction.Create(AText, AUnitType,
  ALocation: string; AGoal: integer);
begin
  Text:= AText;
  UnitType := AUnitType;
  Goal := AGoal;
  Location:= ALocation;
end;

function TShowLeaderboardUnitCountInstruction.ToTrigEdit: string;
begin
  if Goal <> -1 then
    result := 'Leaderboard Goal Control'
  else
    result := 'Leader Board Control';

  if not IsAnywhere(Location) then result += ' At Location';

  result += '(' + AddTrigEditQuotes(Text) + ', '+ AddTrigEditQuotes(UnitType);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);

  if not IsAnywhere(Location) then result += ', ' + AddTrigEditQuotes(Location);

  result += ')';
end;

{ TEndGameInstruction }

constructor TEndGameInstruction.Create(AMode: TEndGameMode);
begin
  Mode := AMode;
end;

function TEndGameInstruction.ToTrigEdit: string;
begin
  case Mode of
  egDefeat: Result:='Defeat()';
  egDraw: Result:='Draw()';
  egVictory: Result := 'Victory()';
  else
    raise exception.Create('Unhandled case');
  end;
end;

{ TUnitSpeechInstruction }

constructor TUnitSpeechInstruction.Create(AActive: Boolean);
begin
  Active := AActive;
end;

function TUnitSpeechInstruction.ToTrigEdit: string;
begin
  if not Active then
    result := 'Mute Unit Speech()'
  else
    result := 'Unmute Unit Speech()';
end;

{ TPauseGameInstruction }

constructor TPauseGameInstruction.Create(APaused: Boolean);
begin
  Paused := APaused;
end;

function TPauseGameInstruction.ToTrigEdit: string;
begin
  If Paused then
    Result := 'Pause Game()'
  else
    result := 'Unpause Game()';
end;

{ TPauseCountdownInstruction }

constructor TPauseCountdownInstruction.Create(APaused: Boolean);
begin
  Paused := APaused;
end;

function TPauseCountdownInstruction.ToTrigEdit: string;
begin
  If Paused then
    Result := 'Pause Timer()'
  else
    result := 'Unpause Timer()';
end;

{ TSetAllianceStatus }

constructor TSetAllianceStatus.Create(APlayer: TPlayer; AStatus: TAllianceStatus);
begin
  Player:= APlayer;
  Status:= AStatus;
end;

function TSetAllianceStatus.ToTrigEdit: string;
begin
  Result:='Set Alliance Status(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+ ', ';
  case Status of
  asEnnemy: result += 'Ennemy';
  asAlly: result += 'Ally';
  asAlliedVictory: result += 'AlliedVictory';
  end;
  result += ')';
end;

initialization

  CreateSetIntegerInstruction := @CreateSetIntegerInstructionImplementation;

end.

