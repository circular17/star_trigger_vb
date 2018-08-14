unit utriggerinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes;

const { Action types }
  atNone = 0;
  atVictory = 1;
  atDefeat = 2;
  atPreserveTrigger = 3;
  atWait = 4;
  atPauseGame = 5;
  atUnpauseGame = 6;
  atTransmission = 7;
  atPlayWAV = 8;
  atDisplayText = 9;
  atCenterView = 10;
  atCreateUnitWithProp = 11;
  atSetMissionObjectives = 12;
  atSetSwitch = 13;
  atSetCountdown = 14;
  atRunAIScript = 15;
  atRunAIScriptAt = 16;
  atLeaderboardControl = 17;
  atLeaderboardControlAt = 18;
  atLeaderboardResources = 19;
  atLeaderboardKills = 20;
  atLeaderboardScore = 21;
  atKillUnit = 22;
  atKillUnitAt = 23;
  atRemoveUnit = 24;
  atRemoveUnitAt = 25;
  atSetResource = 26;
  atSetScore = 27;
  atMinimapPing = 28;
  atTalkingPortrait = 29;
  atMuteUnitSpeech = 30;
  atUnmuteUnitSpeeh = 31;
  atLeaderboardIncludeComputers = 32;
  atLeaderboardGoalControl = 33;
  atLeaderboardGoalControlAt = 34;
  atLeaderboardGoalResources = 35;
  atLeaderboardGoalKills = 36;
  atLeaderboardGoalScore = 37;
  atMoveLocation = 38;
  atTeleportUnit = 39;
  atLeaderboardGreed = 40;
  atSetNextScenario = 41;
  atSetDoodadState = 42;
  atSetUnitInvincibility = 43;
  atCreateUnit = 44;
  atSetDeathCount = 45;
  atOrderUnit = 46;
  atComment = 47;
  atGiveUnit = 48;
  atSetUnitLife = 49;
  atSetUnitEnergy = 50;
  atSetUnitShield = 51;
  atSetUnitResourceAmount = 52;
  atSetUnitHangarCount = 53;
  atPauseCountdown = 54;
  atUnpauseCountdown = 55;
  atDraw = 56;
  atSetAlliance = 57;
  atDisableDebugMode = 58;
  atEnableDebugMode = 59;

type
  TUnitOrder = (uoMove, uoPatrol, uoAttack);
  TAllianceStatus = (asEnnemy, asAlly, asAlliedVictory);

  { TTriggerInstructionData }

  TTriggerInstructionData = object
  private
    function GetAlliance: TAllianceStatus;
    function GetAlwaysDisplay: boolean;
    function GetDestinationLocationBase0: Integer;
    function GetDestinationPlayer: TPlayer;
    function GetEnabled: boolean;
    function GetHasLocation: boolean;
    function GetIgnoreWaitOnce: boolean;
    function GetIntegerOperation: TSetIntegerMode;
    function GetLocationBase0: Integer;
    function GetPlayer: TPlayer;
    function GetResourceType: TStarcraftResource;
    function GetScoreType: TStarcraftScore;
    function GetScriptCode: string;
    function GetSwitch: integer;
    function GetSwitchValue: TSwitchValue;
    function GetUnitCount: integer;
    function GetUnitOrder: TUnitOrder;
    function GetUnitProperties: integer;
    function GetUnitPropertiesUsed: boolean;
    function GetUnitType: TStarcraftUnit;
    function GetUnitTypeUsed: boolean;
    procedure SetAlliance(AValue: TAllianceStatus);
    procedure SetAlwaysDisplay(AValue: boolean);
    procedure SetDestinationLocationBase0(AValue: Integer);
    procedure SetDestinationPlayer(AValue: TPlayer);
    procedure SetEnabled(AValue: boolean);
    procedure SetIgnoreWaitOnce(AValue: boolean);
    procedure SetIntegerOperation(AValue: TSetIntegerMode);
    procedure SetLocationBase0(AValue: Integer);
    procedure SetPlayer(AValue: TPlayer);
    procedure SetResourceType(AValue: TStarcraftResource);
    procedure SetScoreType(AValue: TStarcraftScore);
    procedure SetStringCode(AValue: string);
    procedure SetSwitch(AValue: integer);
    procedure SetSwitchValue(AValue: TSwitchValue);
    procedure SetUnitCount(AValue: integer);
    procedure SetUnitOrder(AValue: TUnitOrder);
    procedure SetUnitProperties(AValue: integer);
    procedure SetUnitPropertiesUsed(AValue: boolean);
    procedure SetUnitType(AValue: TStarcraftUnit);
    procedure SetUnitTypeUsed(AValue: boolean);
  public
    LocationBase1, StringIndex, WavStringIndex, Duration,
    PlayerBase0, GenericValue: LongWord;
    VariableOrAlliance: Word;
    ActionType, UnitCountOrState: byte;
    Flags: bitpacked array[0..7] of boolean;
    Reserved: array[1..3] of byte;
    property HasLocation: boolean read GetHasLocation;
    property LocationBase0: Integer read GetLocationBase0 write SetLocationBase0;
    property DestinationLocationBase0: Integer read GetDestinationLocationBase0 write SetDestinationLocationBase0;
    property Player: TPlayer read GetPlayer write SetPlayer;
    property DestinationPlayer: TPlayer read GetDestinationPlayer write SetDestinationPlayer;
    property UnitType: TStarcraftUnit read GetUnitType write SetUnitType;
    property ScoreType: TStarcraftScore read GetScoreType write SetScoreType;
    property ResourceType: TStarcraftResource read GetResourceType write SetResourceType;
    property Alliance: TAllianceStatus read GetAlliance write SetAlliance;
    property Switch: integer read GetSwitch write SetSwitch;
    property SwitchValue: TSwitchValue read GetSwitchValue write SetSwitchValue;
    property UnitOrder: TUnitOrder read GetUnitOrder write SetUnitOrder;
    property IntegerOperation: TSetIntegerMode read GetIntegerOperation write SetIntegerOperation;
    property IgnoreWaitOnce: boolean read GetIgnoreWaitOnce write SetIgnoreWaitOnce;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property AlwaysDisplay: boolean read GetAlwaysDisplay write SetAlwaysDisplay;
    property UnitPropertiesUsed: boolean read GetUnitPropertiesUsed write SetUnitPropertiesUsed;
    property UnitTypeUsed: boolean read GetUnitTypeUsed write SetUnitTypeUsed;
    property UnitCount: integer read GetUnitCount write SetUnitCount;
    property UnitProperties: integer read GetUnitProperties write SetUnitProperties;
    property ScriptCode: string read GetScriptCode write SetStringCode;
  end;

   {
  u32: Second group affected, destination location (1-based), CUWP #, number, AI script (4-byte string), switch (0-based #)
  u8: Flags
  Bit 0 - Ignore a wait/transmission once.
  Bit 1 - Enabled flag. If on, the trigger action/condition is disabled.
  Bit 2 - Always display flag.
  Bit 3 - Unit properties is used. Staredit uses this for *.trg files.
  Bit 4 - Unit type is used.
  Bit 5 - If on, the unit ID is used. Unnecessary.
  Bit 6-7 - Unknown/unused
  u24 (3 bytes): Used internally by starcraft (number of which action to process next, and maybe more?)     }

  { TTriggerInstruction }

  TTriggerInstruction = class(TInstruction)
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual; abstract;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); virtual; abstract;
  end;

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TTriggerInstruction)
    Switch: integer;
    Value: TSwitchValue;
    constructor Create(ASwitch: integer; AValue: TSwitchValue);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetDeathInstruction }

  TSetDeathInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetResourceInstruction }

  TSetResourceInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Resource: TStarcraftResource;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AResource: TStarcraftResource; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetScoreInstruction }

  TSetScoreInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Score: TStarcraftScore;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AScore: TStarcraftScore; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetCountdownInstruction }

  TSetCountdownInstruction = class(TTriggerInstruction)
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TTriggerInstruction)
    Always: boolean;
    Text: string;
    constructor Create(AAlways: boolean; AMessage: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TTriggerInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TCreateUnitInstruction }

  TCreateUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    Properties: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; AProperties: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  TSetUnitProperty = (supLife, supShield, supEnergy, supResource, supHangarCount);

  { TSetUnitPropertyInstruction }

  TSetUnitPropertyInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    UnitProperty: TSetUnitProperty;
    Value: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; AProperty: TSetUnitProperty; AValue: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  TSetUnitFlag = (sufInvincible, sufDoodadState);
  TUnitFlagValue = (ufvEnable, ufvDisable, ufvToggle);

  { TSetUnitFlagInstruction }

  TSetUnitFlagInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Location: string;
    Flag: TSetUnitFlag;
    Value: TUnitFlagValue;
    constructor Create(APlayer: TPlayer; AUnitType:TStarcraftUnit; ALocation: string; AFlag: TSetUnitFlag; AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TKillUnitInstruction }

  TKillUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    DeathAnimation: boolean;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string = ''; ADeathAnimation: boolean = true);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TGiveUnitInstruction }

  TGiveUnitInstruction = class(TTriggerInstruction)
    Player, DestPlayer: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; ADestPlayer: TPlayer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TTeleportUnitInstruction }

  TTeleportUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    DestLocation: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; ADestLocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TMoveLocationInstruction }

  TMoveLocationInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType:TStarcraftUnit;
    Location: string;
    LocationToChange: string;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string; ALocationToChange: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TOrderUnitInstruction }

  TOrderUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Location: string;
    DestLocation: string;
    Order: TUnitOrder;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string; ADestLocation: string; AOrder: TUnitOrder);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TPlayWAVInstruction }

  TPlayWAVInstruction = class(TTriggerInstruction)
    Filename: string;
    DurationMs: integer;
    constructor Create(AFilename: string; ADurationMs: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TTalkingPortraitInstruction }

  TTalkingPortraitInstruction = class(TTriggerInstruction)
    UnitType: TStarcraftUnit;
    DurationMs: integer;
    constructor Create(AUnitType: TStarcraftUnit; ADurationMs: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TRunAIScriptInstruction }

  TRunAIScriptInstruction = class(TTriggerInstruction)
    ScriptCode, Location: string;
    constructor Create(AScriptCode, ALocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetMissionObjectivesInstruction }

  TSetMissionObjectivesInstruction = class(TTriggerInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetNextScenarioInstruction }

  TSetNextScenarioInstruction = class(TTriggerInstruction)
    Scenario: string;
    constructor Create(AScenario: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TCenterViewInstruction }

  TCenterViewInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TMinimapPingInstruction }

  TMinimapPingInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TLeaderBoardIncludeComputersInstruction }

  TLeaderBoardIncludeComputersInstruction = class(TTriggerInstruction)
    Value: TUnitFlagValue;
    constructor Create(AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TShowLeaderboardOreAndGasIconInstruction }

  TShowLeaderboardOreAndGasIconInstruction = class(TTriggerInstruction)
    Amount: integer;
    constructor Create(AAmount: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TShowLeaderboardResourceInstruction }

  TShowLeaderboardResourceInstruction = class(TTriggerInstruction)
    Text: string;
    Resource: TStarcraftResource;
    Goal: integer;
    constructor Create(AText: string; AResource: TStarcraftResource; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TShowLeaderboardScoreInstruction }

  TShowLeaderboardScoreInstruction = class(TTriggerInstruction)
    Text: string;
    Score: TStarcraftScore;
    Goal: integer;
    constructor Create(AText: string; AScore: TStarcraftScore; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TShowLeaderboardKillCountInstruction }

  TShowLeaderboardKillCountInstruction = class(TTriggerInstruction)
    Text: string;
    UnitType: TStarcraftUnit;
    Goal: integer;
    constructor Create(AText: string; AUnitType: TStarcraftUnit; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TShowLeaderboardUnitCountInstruction }

  TShowLeaderboardUnitCountInstruction = class(TTriggerInstruction)
    Text: string;
    UnitType: TStarcraftUnit;
    Location: string;
    Goal: integer;
    constructor Create(AText: string; AUnitType:TStarcraftUnit; ALocation: string; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  TEndGameMode = (egDefeat, egDraw, egVictory);

  { TEndGameInstruction }

  TEndGameInstruction = class(TTriggerInstruction)
    Mode: TEndGameMode;
    constructor Create(AMode: TEndGameMode);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TUnitSpeechInstruction }

  TUnitSpeechInstruction = class(TTriggerInstruction)
    Active: boolean;
    constructor Create(AActive: Boolean);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TPauseGameInstruction }

  TPauseGameInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TPauseCountdownInstruction }

  TPauseCountdownInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

  { TSetAllianceStatus }

  TSetAllianceStatus = class(TTriggerInstruction)
    Player: TPlayer;
    Status: TAllianceStatus;
    constructor Create(APlayer: TPlayer; AStatus: TAllianceStatus);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
  end;

implementation

uses utrigedittypes, umapinfo;

function CreateSetIntegerInstructionImplementation(APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TSetIntegerMode; AValue: integer): TInstruction;
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

  if AUnitType = suResourceOre then
    result := TSetResourceInstruction.Create(APlayer,srOre,AMode,AValue) else
  if AUnitType = suResourceGas then
    result := TSetResourceInstruction.Create(APlayer,srGas,AMode,AValue) else
  if AUnitType = suResourceOreAndGas then
    result := TSetResourceInstruction.Create(APlayer,srOreAndGas,AMode,AValue) else
  if AUnitType = suScoreUnits then
    result := TSetScoreInstruction.Create(APlayer,ssUnitScore,AMode,AValue) else
  if AUnitType = suScoreBuildings then
    result := TSetScoreInstruction.Create(APlayer,ssBuildingScore,AMode,AValue) else
  if AUnitType = suScoreUnitsAndBuildings then
    result := TSetScoreInstruction.Create(APlayer,ssUnitAndBuildingScore,AMode,AValue) else
  if AUnitType = suScoreKills then
    result := TSetScoreInstruction.Create(APlayer,ssKillScore,AMode,AValue) else
  if AUnitType = suScoreRazings then
    result := TSetScoreInstruction.Create(APlayer,ssRazingScore,AMode,AValue) else
  if AUnitType = suScoreKillsAndRazings then
    result := TSetScoreInstruction.Create(APlayer,ssKillAndRazingScore,AMode,AValue) else
  if AUnitType = suScoreCustom then
    result := TSetScoreInstruction.Create(APlayer,ssCustomScore,AMode,AValue) else
  if AUnitType = suScoreTotal then
    result := TSetScoreInstruction.Create(APlayer,ssTotalScore,AMode,AValue) else
  if AUnitType = suCountdown then
    result := TSetCountdownInstruction.Create(AMode,AValue)
  else
    Result:= TSetDeathInstruction.Create(APlayer, AUnitType, AMode, AValue);
end;

{ TTriggerInstructionData }

function TTriggerInstructionData.GetAlliance: TAllianceStatus;
begin
  if VariableOrAlliance <= ord(high(TAllianceStatus)) then
    result := TAllianceStatus(VariableOrAlliance)
  else
    raise exception.Create('Value out of bounds');
end;

function TTriggerInstructionData.GetAlwaysDisplay: boolean;
begin
  result := Flags[2];
end;

function TTriggerInstructionData.GetDestinationLocationBase0: Integer;
begin
  result := GenericValue-1;
end;

function TTriggerInstructionData.GetDestinationPlayer: TPlayer;
begin
  result := TPlayer(GenericValue+ord(plPlayer1));
end;

function TTriggerInstructionData.GetEnabled: boolean;
begin
  result := not Flags[1];
end;

function TTriggerInstructionData.GetHasLocation: boolean;
begin
  result := LocationBase1 <> 0;
end;

function TTriggerInstructionData.GetIgnoreWaitOnce: boolean;
begin
  result := Flags[0];
end;

function TTriggerInstructionData.GetIntegerOperation: TSetIntegerMode;
begin
  case UnitCountOrState of
  7: result := simSetTo;
  8: result := simAdd;
  9: result := simSubtract;
  else
    raise exception.Create('Value out of bounds');
  end;
end;

function TTriggerInstructionData.GetLocationBase0: Integer;
begin
  result := LocationBase1-1;
end;

function TTriggerInstructionData.GetPlayer: TPlayer;
begin
  result := TPlayer(PlayerBase0+ord(plPlayer1));
end;

function TTriggerInstructionData.GetResourceType: TStarcraftResource;
begin
  if VariableOrAlliance <= ord(high(TStarcraftResource)) then
    result := TStarcraftResource(VariableOrAlliance)
  else
    raise exception.Create('Value out of bounds');
end;

function TTriggerInstructionData.GetScoreType: TStarcraftScore;
begin
  if VariableOrAlliance <= ord(high(TStarcraftScore)) then
    result := TStarcraftScore(VariableOrAlliance)
  else
    raise exception.Create('Value out of bounds');
end;

function TTriggerInstructionData.GetScriptCode: string;
var
  chars: array[1..4] of char absolute GenericValue;
begin
  result := chars[4]+chars[3]+chars[2]+chars[1];
end;

function TTriggerInstructionData.GetSwitch: integer;
begin
  result := GenericValue+1;
end;

function TTriggerInstructionData.GetSwitchValue: TSwitchValue;
begin
  case UnitCountOrState of
  4: result := svSet;
  5: result := svClear;
  6: result := svToggle;
  11: result := svRandomize;
  else raise exception.Create('Value out of bounds');
  end;
end;

function TTriggerInstructionData.GetUnitCount: integer;
begin
  if UnitCountOrState = 0 then exit(-1)
  else exit(UnitCountOrState);
end;

function TTriggerInstructionData.GetUnitOrder: TUnitOrder;
begin
  case UnitCountOrState of
  0: result := uoMove;
  1: result := uoPatrol;
  2: result := uoAttack;
  else raise exception.Create('Value out of bounds');
  end;
end;

function TTriggerInstructionData.GetUnitProperties: integer;
begin
  if UnitPropertiesUsed then
    result := GenericValue
  else
    result := -1;
end;

function TTriggerInstructionData.GetUnitPropertiesUsed: boolean;
begin
  result := Flags[3];
end;

function TTriggerInstructionData.GetUnitType: TStarcraftUnit;
begin
  result := TStarcraftUnit(VariableOrAlliance);
end;

function TTriggerInstructionData.GetUnitTypeUsed: boolean;
begin
  result := Flags[4];
end;

procedure TTriggerInstructionData.SetAlliance(AValue: TAllianceStatus);
begin
  VariableOrAlliance:= ord(AValue);
end;

procedure TTriggerInstructionData.SetAlwaysDisplay(AValue: boolean);
begin
  Flags[2] := AValue;
end;

procedure TTriggerInstructionData.SetDestinationLocationBase0(AValue: Integer);
begin
  GenericValue := AValue+1;
end;

procedure TTriggerInstructionData.SetDestinationPlayer(AValue: TPlayer);
begin
  GenericValue := ord(AValue) - ord(plPlayer1);
end;

procedure TTriggerInstructionData.SetEnabled(AValue: boolean);
begin
  Flags[1] := not AValue;
end;

procedure TTriggerInstructionData.SetIgnoreWaitOnce(AValue: boolean);
begin
  Flags[0] := AValue;
end;

procedure TTriggerInstructionData.SetIntegerOperation(AValue: TSetIntegerMode);
begin
  case AValue of
  simSetTo: UnitCountOrState:= 7;
  simAdd: UnitCountOrState:= 8;
  simSubtract: UnitCountOrState:= 9;
  else
    raise exception.Create('Case not handled');
  end;
end;

procedure TTriggerInstructionData.SetLocationBase0(AValue: Integer);
begin
  LocationBase1 := AValue+1;
end;

procedure TTriggerInstructionData.SetPlayer(AValue: TPlayer);
begin
  PlayerBase0 := ord(AValue) - ord(plPlayer1);
end;

procedure TTriggerInstructionData.SetResourceType(AValue: TStarcraftResource);
begin
  VariableOrAlliance:= ord(AValue);
end;

procedure TTriggerInstructionData.SetScoreType(AValue: TStarcraftScore);
begin
  VariableOrAlliance:= ord(AValue);
end;

procedure TTriggerInstructionData.SetStringCode(AValue: string);
var
  chars: array[1..4] of char absolute GenericValue;
begin
  if length(AValue) <> 4 then raise exception.Create('Code must be 4 chars long');
  chars[4] := AValue[1];
  chars[3] := AValue[2];
  chars[2] := AValue[3];
  chars[1] := AValue[4];
end;

procedure TTriggerInstructionData.SetSwitch(AValue: integer);
begin
  if (AValue < 1) or (AValue > 256) then
    raise exception.Create('Value out of bounds');
  GenericValue := AValue-1;
end;

procedure TTriggerInstructionData.SetSwitchValue(AValue: TSwitchValue);
begin
  case AValue of
  svSet: UnitCountOrState:= 4;
  svClear: UnitCountOrState:= 5;
  svToggle: UnitCountOrState:= 6;
  svRandomize: UnitCountOrState:= 11;
  else raise exception.Create('Case not handled');
  end;
end;

procedure TTriggerInstructionData.SetUnitCount(AValue: integer);
begin
  if AValue < 0 then
    UnitCountOrState:= 0 //all
  else if AValue = 0 then
    raise exception.Create('Zero value not allowed')
  else if GenericValue < 255 then
    raise exception.Create('Value out of bounds')
  else
    UnitCountOrState:= AValue;
end;

procedure TTriggerInstructionData.SetUnitOrder(AValue: TUnitOrder);
begin
  case AValue of
  uoMove: UnitCountOrState:= 0;
  uoPatrol: UnitCountOrState:= 1;
  uoAttack: UnitCountOrState:= 2;
  else raise exception.Create('Case not handled');
  end;
end;

procedure TTriggerInstructionData.SetUnitProperties(AValue: integer);
begin
  if AValue = -1 then
    UnitPropertiesUsed := false
  else
  begin
    GenericValue := GenericValue;
    UnitPropertiesUsed:= true;
  end;
end;

procedure TTriggerInstructionData.SetUnitPropertiesUsed(AValue: boolean);
begin
  Flags[3] := AValue;
end;

procedure TTriggerInstructionData.SetUnitType(AValue: TStarcraftUnit);
begin
  VariableOrAlliance := ord(AValue);
  UnitTypeUsed := true;
end;

procedure TTriggerInstructionData.SetUnitTypeUsed(AValue: boolean);
begin
  Flags[4] := AValue;
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
  Result:= 'Set Switch('+SwitchToTrigEditCode(Switch)+ ', ' + SwitchValueToStr[Value] + ')';
end;

procedure TSetSwitchInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType := atSetSwitch;
  AData.Switch:= Switch;
  AData.SwitchValue:= Value;
end;

{ TSetDeathInstruction }

constructor TSetDeathInstruction.Create(APlayer: TPlayer; AUnitType: TStarcraftUnit;
  AMode: TSetIntegerMode; AValue: integer);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Value:= AValue;
  Mode:= AMode;
end;

function TSetDeathInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Deaths("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ')';
end;

procedure TSetDeathInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atSetDeathCount;
  AData.Player:= Player;
  AData.UnitType := UnitType;
  AData.GenericValue := Value;
  AData.IntegerOperation:= Mode;
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

procedure TSetResourceInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atSetResource;
  AData.Player:= Player;
  AData.ResourceType := Resource;
  AData.GenericValue := Value;
  AData.IntegerOperation:= Mode;
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

procedure TSetScoreInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atSetScore;
  AData.Player:= Player;
  AData.ScoreType := Score;
  AData.GenericValue := Value;
  AData.IntegerOperation:= Mode;
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

procedure TSetCountdownInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atSetCountdown;
  AData.GenericValue := Value;
  AData.IntegerOperation:= Mode;
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

procedure TDisplayTextMessageInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType   := atDisplayText;
  AData.AlwaysDisplay:= Always;
  AData.StringIndex  := AllocateString(Text);
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

procedure TWaitInstruction.WriteTriggerData(var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atWait;
  AData.Duration := DelayMs;
end;

{ TCreateUnitInstruction }

constructor TCreateUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType: TStarcraftUnit; ALocation: string; AProperties: integer);
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

  result += '("'+ PlayerToTrigEditStr(Player)+'", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', ';
  result += inttostr(Quantity) + ', '+AddTrigEditQuotes(Location);
  if Properties >=0 then result += ', ' + inttostr(Properties+1);

  result += ')';
end;

procedure TCreateUnitInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Properties >= 0 then
  begin
    AData.ActionType := atCreateUnitWithProp;
    AData.UnitProperties:= Properties;
  end else
    AData.ActionType := atCreateUnit;

  AData.Player:= Player;
  AData.UnitType := UnitType;
  AData.LocationBase0:= LocationIndexOf(Location);
  AData.UnitCount := Quantity;
end;

{ TSetUnitPropertyInstruction }

constructor TSetUnitPropertyInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string;
  AProperty: TSetUnitProperty; AValue: integer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player := APlayer;
  Quantity := AQuantity;
  UnitType := AUnitType;
  Location := ALocation;
  UnitProperty := AProperty;
  Value := AValue;
  if (AProperty = supResource) and (AUnitType <> suAnyUnit) then
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
  result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', '+inttostr(Value)+', ';
  if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
  result += ', '+AddTrigEditQuotes(Location)+')';
end;

procedure TSetUnitPropertyInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  case UnitProperty of
  supLife: AData.ActionType:= atSetUnitLife;
  supShield: AData.ActionType:= atSetUnitShield;
  supEnergy: AData.ActionType:= atSetUnitEnergy;
  supResource: AData.ActionType:= atSetUnitResourceAmount;
  supHangarCount: AData.ActionType:= atSetUnitHangarCount;
  else
    raise exception.Create('Case not handled');
  end;
  AData.Player:= Player;
  AData.UnitCount := Quantity;
  AData.UnitType := UnitType;
  AData.LocationBase0:= LocationIndexOf(Location);
  AData.GenericValue := Value;
end;

{ TSetUnitFlagInstruction }

constructor TSetUnitFlagInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ALocation: string; AFlag: TSetUnitFlag;
  AValue: TUnitFlagValue);
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

  result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', '+AddTrigEditQuotes(Location)+', ';
  case Value of
  ufvDisable: result += 'disabled';
  ufvEnable: result += 'enabled';
  else result += 'toggle';
  end;
  result += ')';
end;

procedure TSetUnitFlagInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  case Flag of
  sufInvincible: AData.ActionType:= atSetUnitInvincibility;
  sufDoodadState: AData.ActionType := atSetDoodadState;
  else raise exception.Create('Case not handled');
  end;
  AData.Player := Player;
  AData.UnitType := UnitType;
  AData.LocationBase0:= LocationIndexOf(Location);
  case Value of
  ufvDisable: AData.SwitchValue:= svClear;
  ufvEnable: AData.SwitchValue:= svSet;
  ufvToggle: AData.SwitchValue:= svToggle;
  else raise exception.Create('Case not handled');
  end;
end;

{ TKillUnitInstruction }

constructor TKillUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType: TStarcraftUnit; ALocation: string; ADeathAnimation: boolean);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  Player:= APlayer;
  Quantity:= AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  DeathAnimation:= ADeathAnimation;
  If IsAnywhere(Location) then Location := GetAnywhereLocation;
end;

function TKillUnitInstruction.ToTrigEdit: string;
begin
  if DeathAnimation then result := 'Kill Unit' else result := 'Remove Unit';
  if not IsAnywhere(Location) or (Quantity <> -1) then
  begin
    Result += ' At Location';
    result += '("'+ PlayerToTrigEditStr(Player)+'", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', ';
    if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
    result += ', '+AddTrigEditQuotes(Location) + ')';
  end
  else
    Result += '(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player))+', '+AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+')';
end;

procedure TKillUnitInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if not IsAnywhere(Location) or (Quantity <> -1) then
  begin
    if DeathAnimation then AData.ActionType:= atKillUnitAt
    else AData.ActionType:= atRemoveUnitAt;
    AData.LocationBase0:= LocationIndexOf(Location);
    AData.UnitCount := Quantity;
  end else
  begin
    if DeathAnimation then AData.ActionType:= atKillUnit
    else AData.ActionType:= atRemoveUnit;
  end;
  AData.Player := Player;
  AData.UnitType := UnitType;
end;

{ TGiveUnitInstruction }

constructor TGiveUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType: TStarcraftUnit; ALocation: string; ADestPlayer: TPlayer);
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
    AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', ';
  if Quantity = -1 then result += 'All' else result += IntToStr(Quantity);
  result += ', ' + AddTrigEditQuotes(Location) + ')';
end;

procedure TGiveUnitInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atGiveUnit;
  AData.Player := Player;
  AData.UnitCount := Quantity;
  AData.UnitType := UnitType;
  AData.LocationBase0 := LocationIndexOf(Location);
  AData.DestinationPlayer := DestPlayer;
end;

{ TTeleportUnitInstruction }

constructor TTeleportUnitInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string;
  ADestLocation: string);
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
  Result:= 'Move Unit(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player)) + ', '+AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', ';
  if Quantity = -1 then result +='All' else result += inttostr(Quantity);
  result += ', '+AddTrigEditQuotes(Location)+', '+AddTrigEditQuotes(DestLocation)+')';
end;

procedure TTeleportUnitInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atTeleportUnit;
  AData.Player := Player;
  AData.UnitCount:= Quantity;
  AData.UnitType := UnitType;
  AData.LocationBase0 := LocationIndexOf(Location);
  AData.DestinationLocationBase0 := LocationIndexOf(DestLocation);
end;

{ TMoveLocationInstruction }

constructor TMoveLocationInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ALocation: string; ALocationToChange: string);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  LocationToChange:= ALocationToChange;
end;

function TMoveLocationInstruction.ToTrigEdit: string;
begin
  Result:= 'Move Location(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player)) + ', '+AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', ';
  result += AddTrigEditQuotes(Location)+', '+AddTrigEditQuotes(LocationToChange)+')';
end;

procedure TMoveLocationInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atMoveLocation;
  AData.Player := Player;
  AData.UnitType := UnitType;
  AData.LocationBase0:= LocationIndexOf(Location);
  AData.DestinationLocationBase0:= LocationIndexOf(LocationToChange);
end;

{ TOrderUnitInstruction }

constructor TOrderUnitInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ALocation: string; ADestLocation: string;
  AOrder: TUnitOrder);
begin
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  DestLocation:= ADestLocation;
  Order := AOrder;
end;

function TOrderUnitInstruction.ToTrigEdit: string;
begin
  result := 'Order(' + AddTrigEditQuotes(PlayerToTrigEditStr(Player)) + ', '+AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]);
  result += ', '+AddTrigEditQuotes(Location)+', '+AddTrigEditQuotes(DestLocation)+', ';
  case Order of
  uoPatrol: result += 'patrol';
  uoAttack: result += 'attack';
  else result += 'move';
  end;
  result += ')';
end;

procedure TOrderUnitInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atOrderUnit;
  AData.Player := Player;
  AData.UnitType := UnitType;
  AData.LocationBase0 := LocationIndexOf(Location);
  AData.DestinationLocationBase0:= LocationIndexOf(DestLocation);
  AData.UnitOrder := Order;
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

procedure TPlayWAVInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atPlayWAV;
  AData.WavStringIndex:= UseWavString(Filename);
  AData.Duration := DurationMs;
end;

{ TTalkingPortraitInstruction }

constructor TTalkingPortraitInstruction.Create(AUnitType: TStarcraftUnit;
  ADurationMs: integer);
begin
  UnitType:= AUnitType;
  DurationMs:= ADurationMs;
end;

function TTalkingPortraitInstruction.ToTrigEdit: string;
begin
  Result:= 'Talking Portrait(' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' + inttostr(DurationMs) + ')';
end;

procedure TTalkingPortraitInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atTalkingPortrait;
  AData.UnitType := UnitType;
  AData.Duration:= DurationMs;
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

procedure TRunAIScriptInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if IsAnywhere(Location) then
    AData.ActionType:= atRunAIScript
  else
  begin
    AData.ActionType := atRunAIScriptAt;
    AData.LocationBase0:= LocationIndexOf(Location);
  end;
  AData.ScriptCode := ScriptCode;
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

procedure TSetMissionObjectivesInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType := atSetMissionObjectives;
  AData.StringIndex := AllocateString(Text);
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

procedure TSetNextScenarioInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType := atSetNextScenario;
  AData.StringIndex := AllocateString(Scenario);
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

procedure TCenterViewInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType := atCenterView;
  AData.LocationBase0:= LocationIndexOf(Location);
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

procedure TMinimapPingInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atMinimapPing;
  AData.LocationBase0 := LocationIndexOf(Location);
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

procedure TLeaderBoardIncludeComputersInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atLeaderboardIncludeComputers;
  case Value of
  ufvDisable: AData.SwitchValue:= svClear;
  ufvEnable: AData.SwitchValue:= svSet;
  ufvToggle: AData.SwitchValue:= svToggle;
  else raise exception.Create('Unhandled case');
  end;
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

procedure TShowLeaderboardOreAndGasIconInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atLeaderboardGreed;
  AData.GenericValue:= Amount;
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
  if Goal = -1 then
    result := 'Leader Board Resources'
  else
    result := 'Leaderboard Goal Resources';
  result += '(' + AddTrigEditQuotes(Text) + ', ' + StarcraftResourceToStr(Resource);
  if Goal <> -1 then result += ', ' + inttostr(Goal);
  result += ')';
end;

procedure TShowLeaderboardResourceInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Goal = -1 then
    AData.ActionType:= atLeaderboardResources
  else
  begin
    AData.ActionType:= atLeaderboardGoalResources;
    AData.GenericValue := Goal;
  end;
  AData.ResourceType:= Resource;
  AData.StringIndex := AllocateString(Text);
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

procedure TShowLeaderboardScoreInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Goal = -1 then
    AData.ActionType:= atLeaderboardScore
  else
  begin
    AData.ActionType:= atLeaderboardGoalScore;
    AData.GenericValue := Goal;
  end;
  AData.ScoreType:= Score;
  AData.StringIndex := AllocateString(Text);
end;

{ TShowLeaderboardKillCountInstruction }

constructor TShowLeaderboardKillCountInstruction.Create(AText: string;
  AUnitType: TStarcraftUnit; AGoal: integer);
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

  result += '(' + AddTrigEditQuotes(Text) + ', '+ AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);
  result += ')';
end;

procedure TShowLeaderboardKillCountInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Goal = -1 then
    AData.ActionType:= atLeaderboardKills
  else
  begin
    AData.ActionType:= atLeaderboardGoalKills;
    AData.GenericValue := Goal;
  end;
  AData.UnitType:= UnitType;
  AData.StringIndex := AllocateString(Text);
end;

{ TShowLeaderboardUnitCountInstruction }

constructor TShowLeaderboardUnitCountInstruction.Create(AText: string;
  AUnitType: TStarcraftUnit; ALocation: string; AGoal: integer);
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

  result += '(' + AddTrigEditQuotes(Text) + ', '+ AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);

  if not IsAnywhere(Location) then result += ', ' + AddTrigEditQuotes(Location);

  result += ')';
end;

procedure TShowLeaderboardUnitCountInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Goal = -1 then
  begin
    if IsAnywhere(Location) then AData.ActionType:= atLeaderboardControl else
    begin
      AData.ActionType:= atLeaderboardControlAt;
      AData.LocationBase0 := LocationIndexOf(Location);
    end;
  end
  else
  begin
    if IsAnywhere(Location) then AData.ActionType:= atLeaderboardGoalControl else
    begin
      AData.ActionType:= atLeaderboardGoalControlAt;
      AData.LocationBase0 := LocationIndexOf(Location);
    end;
    AData.ActionType:= atLeaderboardGoalControl;
    AData.GenericValue := Goal;
  end;
  AData.UnitType:= UnitType;
  AData.StringIndex := AllocateString(Text);
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

procedure TEndGameInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  case Mode of
  egDefeat: AData.ActionType:= atDefeat;
  egDraw: AData.ActionType:= atDraw;
  egVictory: AData.ActionType:= atVictory;
  else raise exception.Create('Unhandled case');
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

procedure TUnitSpeechInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Active then AData.ActionType := atUnmuteUnitSpeeh
  else AData.ActionType:= atMuteUnitSpeech;
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

procedure TPauseGameInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Paused then AData.ActionType:= atPauseGame
  else AData.ActionType := atUnpauseGame;
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

procedure TPauseCountdownInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Paused then AData.ActionType:= atPauseCountdown
  else AData.ActionType := atUnpauseCountdown;
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

procedure TSetAllianceStatus.WriteTriggerData(var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atSetAlliance;
  AData.Alliance:= Status;
end;

initialization

  CreateSetIntegerInstruction := @CreateSetIntegerInstructionImplementation;

end.

