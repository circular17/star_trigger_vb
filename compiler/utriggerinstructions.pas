unit utriggerinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes, utriggerchunk;

type
  { TTriggerInstruction }

  TTriggerInstruction = class(TInstruction)
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual; abstract;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); virtual; abstract;
    function Duplicate: TTriggerInstruction; virtual; abstract;
  end;

  { TPreserveTriggerInstruction }

  TPreserveTriggerInstruction = class(TTriggerInstruction)
    constructor Create;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TTriggerInstruction)
    Switch: integer;
    Value: TSwitchValue;
    constructor Create(ASwitch: integer; AValue: TSwitchValue);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
  end;

  { TSetCountdownInstruction }

  TSetCountdownInstruction = class(TTriggerInstruction)
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TTriggerInstruction)
    Always: boolean;
    Text: string;
    constructor Create(AAlways: boolean; AText: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TTriggerInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
  end;

  { TPlayWAVInstruction }

  TPlayWAVInstruction = class(TTriggerInstruction)
    Filename: string;
    DurationMs: integer;
    constructor Create(AFilename: string; ADurationMs: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TTalkingPortraitInstruction }

  TTalkingPortraitInstruction = class(TTriggerInstruction)
    UnitType: TStarcraftUnit;
    DurationMs: integer;
    constructor Create(AUnitType: TStarcraftUnit; ADurationMs: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TRunAIScriptInstruction }

  TRunAIScriptInstruction = class(TTriggerInstruction)
    ScriptCode, Location: string;
    constructor Create(AScriptCode, ALocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TCommentInstruction }

  TCommentInstruction = class(TTriggerInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TSetMissionObjectivesInstruction }

  TSetMissionObjectivesInstruction = class(TTriggerInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TSetNextScenarioInstruction }

  TSetNextScenarioInstruction = class(TTriggerInstruction)
    Scenario: string;
    constructor Create(AScenario: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TCenterViewInstruction }

  TCenterViewInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TMinimapPingInstruction }

  TMinimapPingInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TLeaderBoardIncludeComputersInstruction }

  TLeaderBoardIncludeComputersInstruction = class(TTriggerInstruction)
    Value: TUnitFlagValue;
    constructor Create(AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TShowLeaderboardOreAndGasIconInstruction }

  TShowLeaderboardOreAndGasIconInstruction = class(TTriggerInstruction)
    Amount: integer;
    constructor Create(AAmount: integer);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TShowLeaderboardResourceInstruction }

  TShowLeaderboardResourceInstruction = class(TTriggerInstruction)
    Text: string;
    Resource: TStarcraftResource;
    Goal: integer;
    constructor Create(AText: string; AResource: TStarcraftResource; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TShowLeaderboardScoreInstruction }

  TShowLeaderboardScoreInstruction = class(TTriggerInstruction)
    Text: string;
    Score: TStarcraftScore;
    Goal: integer;
    constructor Create(AText: string; AScore: TStarcraftScore; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TShowLeaderboardKillCountInstruction }

  TShowLeaderboardKillCountInstruction = class(TTriggerInstruction)
    Text: string;
    UnitType: TStarcraftUnit;
    Goal: integer;
    constructor Create(AText: string; AUnitType: TStarcraftUnit; AGoal: integer = -1);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
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
    function Duplicate: TTriggerInstruction; override;
  end;

  TEndGameMode = (egDefeat, egDraw, egVictory);

  { TEndGameInstruction }

  TEndGameInstruction = class(TTriggerInstruction)
    Mode: TEndGameMode;
    constructor Create(AMode: TEndGameMode);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TUnitSpeechInstruction }

  TUnitSpeechInstruction = class(TTriggerInstruction)
    Active: boolean;
    constructor Create(AActive: Boolean);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TPauseGameInstruction }

  TPauseGameInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TPauseCountdownInstruction }

  TPauseCountdownInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
  end;

  { TSetAllianceStatus }

  TSetAllianceStatus = class(TTriggerInstruction)
    Player: TPlayer;
    Status: TAllianceStatus;
    constructor Create(APlayer: TPlayer; AStatus: TAllianceStatus);
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TTriggerInstruction; override;
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

{ TCommentInstruction }

constructor TCommentInstruction.Create(AText: string);
begin
  Text := AText;
end;

function TCommentInstruction.ToTrigEdit: string;
begin
  result := 'Comment('+ AddTrigEditQuotes(Text) + ')';
end;

procedure TCommentInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atComment;
  AData.StringIndex := MapInfo.MapStringAllocate(Text);
end;

function TCommentInstruction.Duplicate: TTriggerInstruction;
begin
  result := TCommentInstruction.Create(Text);
end;

{ TPreserveTriggerInstruction }

constructor TPreserveTriggerInstruction.Create;
begin
  //nothing
end;

function TPreserveTriggerInstruction.ToTrigEdit: string;
begin
  result := 'Preserve Trigger()';
end;

procedure TPreserveTriggerInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atPreserveTrigger;
end;

function TPreserveTriggerInstruction.Duplicate: TTriggerInstruction;
begin
  result := TPreserveTriggerInstruction.Create;
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

function TSetSwitchInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetSwitchInstruction.Create(Switch,Value);
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

function TSetDeathInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetDeathInstruction.Create(Player,UnitType,Mode,Value);
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

function TSetResourceInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetResourceInstruction.Create(Player,Resource,Mode,Value);
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

function TSetScoreInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetScoreInstruction.Create(Player,Score,Mode,Value);
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

function TSetCountdownInstruction.Duplicate: TTriggerInstruction;
begin
  result:= TSetCountdownInstruction.Create(Mode,Value);
end;

{ TDisplayTextMessageInstruction }

constructor TDisplayTextMessageInstruction.Create(AAlways: boolean;
  AText: string);
begin
  Always:= AAlways;
  Text:= AText;
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
  AData.StringIndex  := MapInfo.MapStringAllocate(Text);
end;

function TDisplayTextMessageInstruction.Duplicate: TTriggerInstruction;
begin
  result := TDisplayTextMessageInstruction.Create(Always,Text);
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

function TWaitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TWaitInstruction.Create(DelayMs);
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
  AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
  AData.UnitCount := Quantity;
end;

function TCreateUnitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TCreateUnitInstruction.Create(Player,Quantity,UnitType,Location,Properties);
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
  AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
  AData.GenericValue := Value;
end;

function TSetUnitPropertyInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetUnitPropertyInstruction.Create(Player,Quantity,UnitTYpe,Location,UnitProperty,Value);
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
  AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
  case Value of
  ufvDisable: AData.SwitchValue:= svClear;
  ufvEnable: AData.SwitchValue:= svSet;
  ufvToggle: AData.SwitchValue:= svToggle;
  else raise exception.Create('Case not handled');
  end;
end;

function TSetUnitFlagInstruction.Duplicate: TTriggerInstruction;
begin
  result:= TSetUnitFlagInstruction.Create(Player,UnitType,Location,Flag,Value);
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
  If MapInfo.IsAnywhere(Location) then Location := MapInfo.AnywhereLocationName;
end;

function TKillUnitInstruction.ToTrigEdit: string;
begin
  if DeathAnimation then result := 'Kill Unit' else result := 'Remove Unit';
  if not MapInfo.IsAnywhere(Location) or (Quantity <> -1) then
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
  if not MapInfo.IsAnywhere(Location) or (Quantity <> -1) then
  begin
    if DeathAnimation then AData.ActionType:= atKillUnitAt
    else AData.ActionType:= atRemoveUnitAt;
    AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
    AData.UnitCount := Quantity;
  end else
  begin
    if DeathAnimation then AData.ActionType:= atKillUnit
    else AData.ActionType:= atRemoveUnit;
  end;
  AData.Player := Player;
  AData.UnitType := UnitType;
end;

function TKillUnitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TKillUnitInstruction.Create(Player,Quantity,UnitType,Location,DeathAnimation);
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
  AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
  AData.DestinationPlayer := DestPlayer;
end;

function TGiveUnitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TGiveUnitInstruction.Create(Player,Quantity,UnitType,Location,DestPlayer);
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
  AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
  AData.DestinationLocationBase0 := MapInfo.LocationIndexOf(DestLocation);
end;

function TTeleportUnitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TTeleportUnitInstruction.Create(Player,Quantity,UnitTYpe,Location,DestLocation);
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
  AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
  AData.DestinationLocationBase0:= MapInfo.LocationIndexOf(LocationToChange);
end;

function TMoveLocationInstruction.Duplicate: TTriggerInstruction;
begin
  result := TMoveLocationInstruction.Create(Player,UnitType,Location,LocationToChange);
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
  AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
  AData.DestinationLocationBase0:= MapInfo.LocationIndexOf(DestLocation);
  AData.UnitOrder := Order;
end;

function TOrderUnitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TOrderUnitInstruction.Create(Player,UnitType,Location,DestLocation,Order);
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
  AData.WavStringIndex:= MapInfo.UseSoundFilename(Filename);
  AData.Duration := DurationMs;
end;

function TPlayWAVInstruction.Duplicate: TTriggerInstruction;
begin
  result := TPlayWAVInstruction.Create(Filename,DurationMs);
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

function TTalkingPortraitInstruction.Duplicate: TTriggerInstruction;
begin
  result := TTalkingPortraitInstruction.Create(UnitType,DurationMs);
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
  if not MapInfo.IsAnywhere(Location) then Result += ' At Location';
  Result += '(' + AddTrigEditQuotes(ScriptCode);
  if not MapInfo.IsAnywhere(Location) then Result += ', ' + AddTrigEditQuotes(Location);
  result += ')';
end;

procedure TRunAIScriptInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if MapInfo.IsAnywhere(Location) then
    AData.ActionType:= atRunAIScript
  else
  begin
    AData.ActionType := atRunAIScriptAt;
    AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
  end;
  AData.ScriptCode := ScriptCode;
end;

function TRunAIScriptInstruction.Duplicate: TTriggerInstruction;
begin
  result := TRunAIScriptInstruction.Create(ScriptCode,Location);
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
  AData.StringIndex := MapInfo.MapStringAllocate(Text);
end;

function TSetMissionObjectivesInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetMissionObjectivesInstruction.Create(Text);
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
  AData.StringIndex := MapInfo.MapStringAllocate(Scenario);
end;

function TSetNextScenarioInstruction.Duplicate: TTriggerInstruction;
begin
  result := TSetNextScenarioInstruction.Create(Scenario);
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
  AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
end;

function TCenterViewInstruction.Duplicate: TTriggerInstruction;
begin
  result := TCenterViewInstruction.Create(Location);
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
  AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
end;

function TMinimapPingInstruction.Duplicate: TTriggerInstruction;
begin
  result := TMinimapPingInstruction.Create(Location);
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

function TLeaderBoardIncludeComputersInstruction.Duplicate: TTriggerInstruction;
begin
  result := TLeaderBoardIncludeComputersInstruction.Create(Value);
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

function TShowLeaderboardOreAndGasIconInstruction.Duplicate: TTriggerInstruction;
begin
  result:= TShowLeaderboardOreAndGasIconInstruction.Create(Amount);
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
  AData.StringIndex := MapInfo.MapStringAllocate(Text);
end;

function TShowLeaderboardResourceInstruction.Duplicate: TTriggerInstruction;
begin
  result := TShowLeaderboardResourceInstruction.Create(Text,Resource,Goal);
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
  AData.StringIndex := MapInfo.MapStringAllocate(Text);
end;

function TShowLeaderboardScoreInstruction.Duplicate: TTriggerInstruction;
begin
  result := TShowLeaderboardScoreInstruction.Create(Text,Score,Goal);
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
  AData.StringIndex := MapInfo.MapStringAllocate(Text);
end;

function TShowLeaderboardKillCountInstruction.Duplicate: TTriggerInstruction;
begin
  result := TShowLeaderboardKillCountInstruction.Create(Text,UnitType,Goal);
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

  if not MapInfo.IsAnywhere(Location) then result += ' At Location';

  result += '(' + AddTrigEditQuotes(Text) + ', '+ AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);

  if not MapInfo.IsAnywhere(Location) then result += ', ' + AddTrigEditQuotes(Location);

  result += ')';
end;

procedure TShowLeaderboardUnitCountInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  if Goal = -1 then
  begin
    if MapInfo.IsAnywhere(Location) then AData.ActionType:= atLeaderboardControl else
    begin
      AData.ActionType:= atLeaderboardControlAt;
      AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
    end;
  end
  else
  begin
    if MapInfo.IsAnywhere(Location) then AData.ActionType:= atLeaderboardGoalControl else
    begin
      AData.ActionType:= atLeaderboardGoalControlAt;
      AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
    end;
    AData.ActionType:= atLeaderboardGoalControl;
    AData.GenericValue := Goal;
  end;
  AData.UnitType:= UnitType;
  AData.StringIndex := MapInfo.MapStringAllocate(Text);
end;

function TShowLeaderboardUnitCountInstruction.Duplicate: TTriggerInstruction;
begin
  result := TShowLeaderboardUnitCountInstruction.Create(Text,UnitType,Location,Goal);
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

function TEndGameInstruction.Duplicate: TTriggerInstruction;
begin
  result := TEndGameInstruction.Create(Mode);
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

function TUnitSpeechInstruction.Duplicate: TTriggerInstruction;
begin
  result := TUnitSpeechInstruction.Create(Active);
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

function TPauseGameInstruction.Duplicate: TTriggerInstruction;
begin
  result := TPauseGameInstruction.Create(Paused);
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

function TPauseCountdownInstruction.Duplicate: TTriggerInstruction;
begin
  result := TPauseCountdownInstruction.Create(Paused);
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

function TSetAllianceStatus.Duplicate: TTriggerInstruction;
begin
  result := TSetAllianceStatus.Create(Player,Status);
end;

initialization

  CreateSetIntegerInstruction := @CreateSetIntegerInstructionImplementation;

end.

