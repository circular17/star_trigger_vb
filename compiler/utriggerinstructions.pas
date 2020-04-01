unit utriggerinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes, utriggerchunk;

type
  { TTriggerInstruction }

  TTriggerInstruction = class(TInstruction)
    function ToTrigEditAndFree: string;
    function ToBasic: string; virtual;
    function ToTrigEdit: string; virtual; abstract;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); virtual; abstract;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; virtual;
  end;

  { TPreserveTriggerInstruction }

  TPreserveTriggerInstruction = class(TTriggerInstruction)
    constructor Create;
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TTriggerInstruction)
    Switch: integer;
    Value: TSwitchValue;
    constructor Create(ASwitch: integer; AValue: TSwitchValue);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetDeathInstruction }

  TSetDeathInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TSetIntegerMode; AValue: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetResourceInstruction }

  TSetResourceInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Resource: TStarcraftResource;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AResource: TStarcraftResource; AMode: TSetIntegerMode; AValue: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetScoreInstruction }

  TSetScoreInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Score: TStarcraftScore;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AScore: TStarcraftScore; AMode: TSetIntegerMode; AValue: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetCountdownInstruction }

  TSetCountdownInstruction = class(TTriggerInstruction)
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(AMode: TSetIntegerMode; AValue: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TTriggerInstruction)
    Always: boolean;
    Text: string;
    constructor Create(AAlways: boolean; AText: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TTriggerInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TCreateUnitInstruction }

  TCreateUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    Properties: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; AProperties: integer = -1);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
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
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
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
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TKillUnitInstruction }

  TKillUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    DeathAnimation: boolean;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string = ''; ADeathAnimation: boolean = true);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TGiveUnitInstruction }

  TGiveUnitInstruction = class(TTriggerInstruction)
    Player, DestPlayer: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; ADestPlayer: TPlayer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TTeleportUnitInstruction }

  TTeleportUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType: TStarcraftUnit;
    Location: string;
    DestLocation: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string; ADestLocation: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TMoveLocationInstruction }

  TMoveLocationInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType:TStarcraftUnit;
    Location: string;
    LocationToChange: string;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string; ALocationToChange: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TOrderUnitInstruction }

  TOrderUnitInstruction = class(TTriggerInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Location: string;
    DestLocation: string;
    Order: TUnitOrder;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string; ADestLocation: string; AOrder: TUnitOrder);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TPlayWAVInstruction }

  TPlayWAVInstruction = class(TTriggerInstruction)
    Filename: string;
    DurationMs: integer;
    constructor Create(AFilename: string; ADurationMs: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TTalkingPortraitInstruction }

  TTalkingPortraitInstruction = class(TTriggerInstruction)
    UnitType: TStarcraftUnit;
    DurationMs: integer;
    constructor Create(AUnitType: TStarcraftUnit; ADurationMs: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TRunAIScriptInstruction }

  TRunAIScriptInstruction = class(TTriggerInstruction)
    ScriptCode, Location: string;
    constructor Create(AScriptCode: string; ALocation: string = '');
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TCommentInstruction }

  TCommentInstruction = class(TTriggerInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetMissionObjectivesInstruction }

  TSetMissionObjectivesInstruction = class(TTriggerInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetNextScenarioInstruction }

  TSetNextScenarioInstruction = class(TTriggerInstruction)
    Scenario: string;
    constructor Create(AScenario: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TCenterViewInstruction }

  TCenterViewInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TMinimapPingInstruction }

  TMinimapPingInstruction = class(TTriggerInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TLeaderBoardIncludeComputersInstruction }

  TLeaderBoardIncludeComputersInstruction = class(TTriggerInstruction)
    Value: TUnitFlagValue;
    constructor Create(AValue: TUnitFlagValue);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TShowLeaderboardOreAndGasIconInstruction }

  TShowLeaderboardOreAndGasIconInstruction = class(TTriggerInstruction)
    Amount: integer;
    constructor Create(AAmount: integer);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TShowLeaderboardResourceInstruction }

  TShowLeaderboardResourceInstruction = class(TTriggerInstruction)
    Text: string;
    Resource: TStarcraftResource;
    Goal: integer;
    constructor Create(AText: string; AResource: TStarcraftResource; AGoal: integer = -1);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TShowLeaderboardScoreInstruction }

  TShowLeaderboardScoreInstruction = class(TTriggerInstruction)
    Text: string;
    Score: TStarcraftScore;
    Goal: integer;
    constructor Create(AText: string; AScore: TStarcraftScore; AGoal: integer = -1);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TShowLeaderboardKillCountInstruction }

  TShowLeaderboardKillCountInstruction = class(TTriggerInstruction)
    Text: string;
    UnitType: TStarcraftUnit;
    Goal: integer;
    constructor Create(AText: string; AUnitType: TStarcraftUnit; AGoal: integer = -1);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TShowLeaderboardUnitCountInstruction }

  TShowLeaderboardUnitCountInstruction = class(TTriggerInstruction)
    Text: string;
    UnitType: TStarcraftUnit;
    Location: string;
    Goal: integer;
    constructor Create(AText: string; AUnitType:TStarcraftUnit; ALocation: string; AGoal: integer = -1);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  TEndGameMode = (egDefeat, egDraw, egVictory);

  { TEndGameInstruction }

  TEndGameInstruction = class(TTriggerInstruction)
    Mode: TEndGameMode;
    constructor Create(AMode: TEndGameMode);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TUnitSpeechInstruction }

  TUnitSpeechInstruction = class(TTriggerInstruction)
    Active: boolean;
    constructor Create(AActive: Boolean);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TPauseGameInstruction }

  TPauseGameInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TPauseCountdownInstruction }

  TPauseCountdownInstruction = class(TTriggerInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

  { TSetAllianceStatus }

  TSetAllianceStatus = class(TTriggerInstruction)
    Player: TPlayer;
    Status: TAllianceStatus;
    constructor Create(APlayer: TPlayer; AStatus: TAllianceStatus);
    function ToBasic: string; override;
    function ToTrigEdit: string; override;
    procedure WriteTriggerData(var AData: TTriggerInstructionData); override;
    function Duplicate: TInstruction; override;
    class function LoadFromData(const AData: TTriggerInstructionData): TTriggerInstruction; override;
  end;

type
  TBasicSoundInfo = record
    ConstName: string;
    Filename: string;
    Duration: integer;
  end;

var
  BasicSoundInfoArray : array of TBasicSoundInfo;

implementation

uses utrigedittypes, umapinfo, uparsevb;

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

function TCommentInstruction.ToBasic: string;
var
  lines: TStringList;
  i: Integer;
begin
  result := '';
  lines := TStringList.Create;
  lines.Text := Text;
  for i := 0 to lines.Count-1 do
  begin
    if i > 0 then result += lineending;
    Result += '''' + lines[i];
  end;
  lines.Free;
end;

function TCommentInstruction.ToTrigEdit: string;
begin
  result := 'Comment('+ AddTrigEditQuotes(Text) + ')';
end;

procedure TCommentInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atComment;
  AData.StringIndex := MapInfo.TrigStringAllocate(Text);
end;

function TCommentInstruction.Duplicate: TInstruction;
begin
  result := TCommentInstruction.Create(Text);
end;

class function TCommentInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atComment then
    result := TCommentInstruction.Create(MapInfo.MapStringRead(AData.StringIndex))
  else
    result := nil;
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

function TPreserveTriggerInstruction.ToBasic: string;
begin
  result := '';
end;

procedure TPreserveTriggerInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atPreserveTrigger;
end;

function TPreserveTriggerInstruction.Duplicate: TInstruction;
begin
  result := TPreserveTriggerInstruction.Create;
end;

class function TPreserveTriggerInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atPreserveTrigger then
    result := TPreserveTriggerInstruction.Create
  else
    result := nil;
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

function TTriggerInstruction.ToBasic: string;
begin
  result := ToTrigEdit;
end;

class function TTriggerInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  result := TPreserveTriggerInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetSwitchInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetDeathInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetResourceInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetScoreInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetCountdownInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TDisplayTextMessageInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TWaitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCreateUnitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetUnitPropertyInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetUnitFlagInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TKillUnitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TGiveUnitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TTeleportUnitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TMoveLocationInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TOrderUnitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TPlayWAVInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TTalkingPortraitInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TRunAIScriptInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCommentInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetMissionObjectivesInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetNextScenarioInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCenterViewInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TMinimapPingInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TLeaderBoardIncludeComputersInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TShowLeaderboardOreAndGasIconInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TShowLeaderboardResourceInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TShowLeaderboardScoreInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TShowLeaderboardKillCountInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TShowLeaderboardUnitCountInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TEndGameInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TUnitSpeechInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TPauseGameInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TPauseCountdownInstruction.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSetAllianceStatus.LoadFromData(AData);
end;

{ TSetSwitchInstruction }

constructor TSetSwitchInstruction.Create(ASwitch: integer;
  AValue: TSwitchValue);
begin
  Switch:= ASwitch;
  Value:= AValue;
end;

function TSetSwitchInstruction.ToBasic: string;
begin
  result := 'Switch('+inttostr(Switch)+') = '+SwitchValueToBasic[Value];
  if Value = svToggle then result += ' Switch('+inttostr(Switch)+')';
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

function TSetSwitchInstruction.Duplicate: TInstruction;
begin
  result := TSetSwitchInstruction.Create(Switch,Value);
end;

class function TSetSwitchInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetSwitch then
    result := TSetSwitchInstruction.Create(AData.Switch,AData.SwitchValue)
  else
    result := nil;
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

function TSetDeathInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.DeathCount('+StarcraftUnitIdentifier[UnitType]+') '+SetIntModeToBasic[Mode]+' '+inttostr(Value);
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

function TSetDeathInstruction.Duplicate: TInstruction;
begin
  result := TSetDeathInstruction.Create(Player,UnitType,Mode,Value);
end;

class function TSetDeathInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetDeathCount then
    result := TSetDeathInstruction.Create(AData.Player,AData.UnitType,AData.IntegerOperation,AData.GenericValue)
  else
    result := nil;
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

function TSetResourceInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.'+StarcraftResourceToBasic[Resource]+' '+SetIntModeToBasic[Mode]+' '+inttostr(Value);
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

function TSetResourceInstruction.Duplicate: TInstruction;
begin
  result := TSetResourceInstruction.Create(Player,Resource,Mode,Value);
end;

class function TSetResourceInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetResource then
    result := TSetResourceInstruction.Create(AData.Player,AData.ResourceType,AData.IntegerOperation,AData.GenericValue)
  else
    result := nil;
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

function TSetScoreInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.'+StarcraftScoreToBasic[Score]+' '+SetIntModeToBasic[Mode]+' '+inttostr(Value);
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

function TSetScoreInstruction.Duplicate: TInstruction;
begin
  result := TSetScoreInstruction.Create(Player,Score,Mode,Value);
end;

class function TSetScoreInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetScore then
    result := TSetScoreInstruction.Create(AData.Player,AData.ScoreType,AData.IntegerOperation,AData.GenericValue)
  else
    result := nil;
end;

{ TSetCountdownInstruction }

constructor TSetCountdownInstruction.Create(AMode: TSetIntegerMode;
  AValue: integer);
begin
  Mode := AMode;
  Value := AValue;
end;

function TSetCountdownInstruction.ToBasic: string;
begin
  result := 'Countdown '+SetIntModeToBasic[Mode]+' '+inttostr(Value);
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

function TSetCountdownInstruction.Duplicate: TInstruction;
begin
  result:= TSetCountdownInstruction.Create(Mode,Value);
end;

class function TSetCountdownInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetCountdown then
    result := TSetCountdownInstruction.Create(AData.IntegerOperation,AData.GenericValue)
  else
    result := nil;
end;

{ TDisplayTextMessageInstruction }

constructor TDisplayTextMessageInstruction.Create(AAlways: boolean;
  AText: string);
begin
  Always:= AAlways;
  Text:= AText;
end;

function TDisplayTextMessageInstruction.ToBasic: string;
begin
  result := 'Print('+StrToBasic(Text);
  if not Always then result += ', False';
  result += ')';
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
  AData.StringIndex  := MapInfo.TrigStringAllocate(Text);
end;

function TDisplayTextMessageInstruction.Duplicate: TInstruction;
begin
  result := TDisplayTextMessageInstruction.Create(Always,Text);
end;

class function TDisplayTextMessageInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atDisplayText then
    result := TDisplayTextMessageInstruction.Create(AData.AlwaysDisplay, MapInfo.MapStringRead(AData.StringIndex))
  else
    result := nil;
end;

{ TWaitInstruction }

constructor TWaitInstruction.Create(ADelayMs: integer);
begin
  DelayMs:= ADelayMs;
end;

function TWaitInstruction.ToBasic: string;
begin
  Result:= 'Wait('+IntToStr(DelayMs)+')';
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

function TWaitInstruction.Duplicate: TInstruction;
begin
  result := TWaitInstruction.Create(DelayMs);
end;

class function TWaitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atWait then
    result := TWaitInstruction.Create(AData.Duration)
  else
    result := nil;
end;

{ TCreateUnitInstruction }

constructor TCreateUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType: TStarcraftUnit; ALocation: string; AProperties: integer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  if AQuantity > 255 then raise exception.Create('255 is the maximum quantity allowed');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Player:= APlayer;
  Quantity:= AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  Properties:= AProperties;
end;

function TCreateUnitInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.CreateUnit('+inttostr(Quantity)+', '+StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', ' + StrToBasic(Location);
  result += ')';
  if Properties >= 0 then result += ' With UnitProperties' + inttostr(Properties);
end;

function TCreateUnitInstruction.ToTrigEdit: string;
begin
  Result:= 'Create Unit';
  if Properties >= 0 then result += ' with Properties';

  result += '("'+ PlayerToTrigEditStr(Player)+'", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType])+', ';
  result += inttostr(Quantity) + ', '+AddTrigEditQuotes(Location);
  if Properties >= 0 then result += ', ' + inttostr(Properties);

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

function TCreateUnitInstruction.Duplicate: TInstruction;
begin
  result := TCreateUnitInstruction.Create(Player,Quantity,UnitType,Location,Properties);
end;

class function TCreateUnitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
var
  prop: Integer;
begin
  if AData.ActionType in[atCreateUnit,atCreateUnitWithProp] then
  begin
    if AData.ActionType = atCreateUnit then
      prop := -1
    else
      prop := AData.UnitProperties;

    result := TCreateUnitInstruction.Create(AData.Player,AData.UnitCount,
         AData.UnitType, MapInfo.LocationName[AData.LocationBase0], prop)
  end
  else
    result := nil;
end;

{ TSetUnitPropertyInstruction }

constructor TSetUnitPropertyInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string;
  AProperty: TSetUnitProperty; AValue: integer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  if AQuantity > 255 then raise exception.Create('255 is the maximum quantity allowed');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
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

function TSetUnitPropertyInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.Units(';
  if quantity <> -1 then result += inttostr(Quantity)+', ';
  result += StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
  result += ').';
  case UnitProperty of
  supLife: result += 'Life';
  supShield: result += 'Shield';
  supEnergy: result += 'Energy';
  supResource: result += 'Resource';
  supHangarCount: result += 'HangarCount';
  else
    raise exception.Create('Case not handled');
  end;
  result += ' = '+inttostr(Value);
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

function TSetUnitPropertyInstruction.Duplicate: TInstruction;
begin
  result := TSetUnitPropertyInstruction.Create(Player,Quantity,UnitTYpe,Location,UnitProperty,Value);
end;

class function TSetUnitPropertyInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
var prop: TSetUnitProperty;
begin
  case AData.ActionType of
  atSetUnitLife: prop := supLife;
  atSetUnitShield: prop := supShield;
  atSetUnitEnergy: prop := supEnergy;
  atSetUnitResourceAmount: prop := supResource;
  atSetUnitHangarCount: prop := supHangarCount;
  else exit(nil);
  end;
  result := TSetUnitPropertyInstruction.Create(AData.Player, AData.UnitCount,
      AData.UnitType, MapInfo.LocationName[AData.LocationBase0], prop, AData.GenericValue);
end;

{ TSetUnitFlagInstruction }

constructor TSetUnitFlagInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ALocation: string; AFlag: TSetUnitFlag;
  AValue: TUnitFlagValue);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Player := APlayer;
  UnitType := AUnitType;
  Location := ALocation;
  Flag := AFlag;
  Value := AValue;
end;

function TSetUnitFlagInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.Units('+StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
  result += ').';
  case Value of
  ufvDisable,ufvEnable:
  begin
    case Flag of
    sufInvincible: result += 'Invincible';
    sufDoodadState: result += 'DoodadState';
    else
      raise exception.Create('Case not handled');
    end;
    result += ' = ' + BoolToStr(Value=ufvEnable,'True','False');
  end;
  else
    begin
      case Flag of
      sufInvincible: result += 'ToggleInvincibility';
      sufDoodadState: result += 'ToggleDoodadState';
      else
        raise exception.Create('Case not handled');
      end;
    end;
  end;
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

function TSetUnitFlagInstruction.Duplicate: TInstruction;
begin
  result:= TSetUnitFlagInstruction.Create(Player,UnitType,Location,Flag,Value);
end;

class function TSetUnitFlagInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
var
  f: TSetUnitFlag;
  v: TUnitFlagValue;
begin
  case AData.ActionType of
  atSetUnitInvincibility: f := sufInvincible;
  atSetDoodadState: f := sufDoodadState;
  else exit(nil);
  end;
  case AData.SwitchValue of
  svClear: v := ufvDisable;
  svSet: v := ufvEnable;
  else v := ufvToggle;
  end;
  result := TSetUnitFlagInstruction.Create(AData.Player,AData.UnitType,
         MapInfo.LocationName[AData.LocationBase0], f, v);
end;

{ TKillUnitInstruction }

constructor TKillUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType: TStarcraftUnit; ALocation: string; ADeathAnimation: boolean);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  if AQuantity > 255 then raise exception.Create('255 is the maximum quantity allowed');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Player:= APlayer;
  Quantity:= AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  DeathAnimation:= ADeathAnimation;
  If MapInfo.IsAnywhere(Location) then Location := MapInfo.AnywhereLocationName;
end;

function TKillUnitInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.Units(';
  result += StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
  result += ').';
  if DeathAnimation then result += 'Kill' else result += 'Remove';
  if Quantity <> -1 then result += '('+inttostr(Quantity)+')';
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

function TKillUnitInstruction.Duplicate: TInstruction;
begin
  result := TKillUnitInstruction.Create(Player,Quantity,UnitType,Location,DeathAnimation);
end;

class function TKillUnitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
var
  loc: String;
  anim: Boolean;
  qty: Integer;
begin
  if AData.ActionType in[atKillUnit,atKillUnitAt,atRemoveUnit,atRemoveUnitAt] then
  begin
    if AData.ActionType in [atKillUnitAt,atRemoveUnitAt] then
    begin
      loc := MapInfo.LocationName[AData.LocationBase0];
      qty := AData.UnitCount;
    end
    else
    begin
      loc := MapInfo.AnywhereLocationName;
      qty := -1;
    end;
    anim := (AData.ActionType in [atKillUnit,atKillUnitAt]);
    result := TKillUnitInstruction.Create(AData.Player,qty,AData.UnitType,loc,anim);
  end else
    result := nil;
end;

{ TGiveUnitInstruction }

constructor TGiveUnitInstruction.Create(APlayer: TPlayer; AQuantity: integer;
  AUnitType: TStarcraftUnit; ALocation: string; ADestPlayer: TPlayer);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  if AQuantity > 255 then raise exception.Create('255 is the maximum quantity allowed');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Player := APlayer;
  Quantity := AQuantity;
  UnitType := AUnitType;
  Location := ALocation;
  DestPlayer := ADestPlayer;
end;

function TGiveUnitInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.Units(';
  result += StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
  result += ').Give('+PlayerIdentifiers[DestPlayer];
  if Quantity <> -1 then result += ', '+inttostr(Quantity);
  result += ')';
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

function TGiveUnitInstruction.Duplicate: TInstruction;
begin
  result := TGiveUnitInstruction.Create(Player,Quantity,UnitType,Location,DestPlayer);
end;

class function TGiveUnitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atGiveUnit then
    result := TGiveUnitInstruction.Create(AData.Player,AData.UnitCount,AData.UnitType,
           MapInfo.LocationName[AData.LocationBase0], AData.DestinationPlayer)
  else
    result := nil;
end;

{ TTeleportUnitInstruction }

constructor TTeleportUnitInstruction.Create(APlayer: TPlayer;
  AQuantity: integer; AUnitType: TStarcraftUnit; ALocation: string;
  ADestLocation: string);
begin
  if AQuantity = 0 then raise exception.Create('0 is not allowed as a quantity');
  if AQuantity > 255 then raise exception.Create('255 is the maximum quantity allowed');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ADestLocation)=-1) then raise exception.Create('Location not found');
  Player:= APlayer;
  Quantity := AQuantity;
  UnitType:= AUnitType;
  Location:= ALocation;
  DestLocation:= ADestLocation;
end;

function TTeleportUnitInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.Units(';
  if Quantity <> -1 then result += inttostr(Quantity)+', ';
  result += StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
  result += ').Teleport(';
  if MapInfo.IsAnywhere(DestLocation) then result += 'Anywhere'
  else result += StrToBasic(DestLocation);
  result += ')';
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

function TTeleportUnitInstruction.Duplicate: TInstruction;
begin
  result := TTeleportUnitInstruction.Create(Player,Quantity,UnitTYpe,Location,DestLocation);
end;

class function TTeleportUnitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atTeleportUnit then
    result := TTeleportUnitInstruction.Create(AData.Player,AData.UnitCount,AData.UnitType,
            MapInfo.LocationName[AData.LocationBase0],
            MapInfo.LocationName[AData.DestinationLocationBase0])
  else
    result := nil;
end;

{ TMoveLocationInstruction }

constructor TMoveLocationInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ALocation: string; ALocationToChange: string);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocationToChange)=-1) then raise exception.Create('Location not found');
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  LocationToChange:= ALocationToChange;
end;

function TMoveLocationInstruction.ToBasic: string;
begin
  if (UnitType = suUnusedCaveIn) and not MapInfo.IsAnywhere(Location) then
  begin
    result := 'Location('+StrToBasic(LocationToChange)+').CenterOn(';
    if MapInfo.IsAnywhere(LocationToChange) then result += 'Anywhere'
    else result += StrToBasic(Location);
    result += ')';
  end else
  begin
    result := PlayerIdentifiers[Player]+'.Units(';
    result += StarcraftUnitIdentifier[UnitType];
    if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
    result += ').AttractLocation(';
    if MapInfo.IsAnywhere(LocationToChange) then result += 'Anywhere'
    else result += StrToBasic(LocationToChange);
    result += ')';
  end;
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

function TMoveLocationInstruction.Duplicate: TInstruction;
begin
  result := TMoveLocationInstruction.Create(Player,UnitType,Location,LocationToChange);
end;

class function TMoveLocationInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atMoveLocation then
    result := TMoveLocationInstruction.Create(AData.Player,AData.UnitType,
            MapInfo.LocationName[AData.LocationBase0],
            MapInfo.LocationName[AData.DestinationLocationBase0])
  else
    result := nil;
end;

{ TOrderUnitInstruction }

constructor TOrderUnitInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ALocation: string; ADestLocation: string;
  AOrder: TUnitOrder);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ADestLocation)=-1) then raise exception.Create('Location not found');
  Player:= APlayer;
  UnitType:= AUnitType;
  Location:= ALocation;
  DestLocation:= ADestLocation;
  Order := AOrder;
end;

function TOrderUnitInstruction.ToBasic: string;
begin
  result := PlayerIdentifiers[Player]+'.Units(';
  result += StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', '+StrToBasic(Location);
  result += ').';
  case Order of
  uoPatrol: result += 'PatrolOrder';
  uoAttack: result += 'AttackOrder';
  else result += 'MoveOrder';
  end;
  result += '('+StrToBasic(DestLocation)+')';
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

function TOrderUnitInstruction.Duplicate: TInstruction;
begin
  result := TOrderUnitInstruction.Create(Player,UnitType,Location,DestLocation,Order);
end;

class function TOrderUnitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atOrderUnit then
    result := TOrderUnitInstruction.Create(AData.Player,AData.UnitType,
            MapInfo.LocationName[AData.LocationBase0],
            MapInfo.LocationName[AData.DestinationLocationBase0],
            AData.UnitOrder)
  else
    result := nil;
end;

{ TPlayWAVInstruction }

constructor TPlayWAVInstruction.Create(AFilename: string; ADurationMs: integer);
begin
  Filename:= AFilename;
  DurationMs:= ADurationMs;
end;

function TPlayWAVInstruction.ToBasic: string;
var
  i: Integer;
begin
  for i := 0 to high(BasicSoundInfoArray) do
    if (BasicSoundInfoArray[i].Filename = Filename) and
       (BasicSoundInfoArray[i].Duration = DurationMs) then
       exit(BasicSoundInfoArray[i].ConstName + '.Play');
  Result:= '{.Filename = ' + StrToBasic(Filename)+', .Duration = '+inttostr(DurationMs)+'}.Play';
end;

function TPlayWAVInstruction.ToTrigEdit: string;
begin
  Result:= 'Play WAV(' + AddTrigEditQuotes(Filename) + ', ' + inttostr(DurationMs) + ')';
end;

procedure TPlayWAVInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType:= atPlayWAV;
  if not MapInfo.SoundFilenameExists(Filename) then
    raise exception.Create('Sound is not defined in the map');
  AData.WavStringIndex:= MapInfo.TrigStringAllocate(Filename);
  AData.Duration := DurationMs;
end;

function TPlayWAVInstruction.Duplicate: TInstruction;
begin
  result := TPlayWAVInstruction.Create(Filename,DurationMs);
end;

class function TPlayWAVInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atPlayWAV then
    result := TPlayWAVInstruction.Create(MapInfo.MapStringRead(AData.WavStringIndex), AData.Duration)
  else
    result := nil;
end;

{ TTalkingPortraitInstruction }

constructor TTalkingPortraitInstruction.Create(AUnitType: TStarcraftUnit;
  ADurationMs: integer);
begin
  UnitType:= AUnitType;
  DurationMs:= ADurationMs;
end;

function TTalkingPortraitInstruction.ToBasic: string;
begin
  Result:= 'TalkingPortrait('+StarcraftUnitIdentifier[UnitType]+', '+inttostr(DurationMs)+')';
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

function TTalkingPortraitInstruction.Duplicate: TInstruction;
begin
  result := TTalkingPortraitInstruction.Create(UnitType,DurationMs);
end;

class function TTalkingPortraitInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atTalkingPortrait then
    result := TTalkingPortraitInstruction.Create(AData.UnitType,AData.Duration)
  else
    result := nil;
end;

{ TRunAIScriptInstruction }

constructor TRunAIScriptInstruction.Create(AScriptCode: string;
  ALocation: string);
begin
  if MapInfo.IsAnywhere(ALocation) then ALocation:= MapInfo.AnywhereLocationName;
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  ScriptCode:= AScriptCode;
  Location:= ALocation;
end;

function TRunAIScriptInstruction.ToBasic: string;
var
  i: Integer;
  found: Boolean;
begin
  Result:= 'RunAIScript(';
  found := false;
  for i := low(AIScripts) to high(AIScripts) do
    if AIScripts[i].Code = ScriptCode then
    begin
      result += 'AI.'+AIScripts[i].Identifier;
      found := true;
      break;
    end;
  if not found then result += StrToBasic(ScriptCode);
  if not MapInfo.IsAnywhere(Location) then Result += ', ' + StrToBasic(Location);
  result += ')';
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

function TRunAIScriptInstruction.Duplicate: TInstruction;
begin
  result := TRunAIScriptInstruction.Create(ScriptCode,Location);
end;

class function TRunAIScriptInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atRunAIScript then
    result := TRunAIScriptInstruction.Create(AData.ScriptCode)
  else
  if AData.ActionType = atRunAIScriptAt then
    result := TRunAIScriptInstruction.Create(AData.ScriptCode,
      MapInfo.LocationName[AData.LocationBase0])
  else
    result := nil;
end;

{ TSetMissionObjectivesInstruction }

constructor TSetMissionObjectivesInstruction.Create(AText: string);
begin
  Text := AText;
end;

function TSetMissionObjectivesInstruction.ToBasic: string;
begin
  Result:='MissionObjectives = '+StrToBasic(Text);
end;

function TSetMissionObjectivesInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Mission Objectives(' + AddTrigEditQuotes(Text) + ')';
end;

procedure TSetMissionObjectivesInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType := atSetMissionObjectives;
  AData.StringIndex := MapInfo.TrigStringAllocate(Text);
end;

function TSetMissionObjectivesInstruction.Duplicate: TInstruction;
begin
  result := TSetMissionObjectivesInstruction.Create(Text);
end;

class function TSetMissionObjectivesInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetMissionObjectives then
    result := TSetMissionObjectivesInstruction.Create(MapInfo.MapStringRead(AData.StringIndex))
  else
    result := nil;
end;

{ TSetNextScenarioInstruction }

constructor TSetNextScenarioInstruction.Create(AScenario: string);
begin
  Scenario:= AScenario;
end;

function TSetNextScenarioInstruction.ToBasic: string;
begin
  Result:='NextScenario = '+StrToBasic(Scenario);
end;

function TSetNextScenarioInstruction.ToTrigEdit: string;
begin
  Result:='Set Next Scenario(' + AddTrigEditQuotes(Scenario) + ')';
end;

procedure TSetNextScenarioInstruction.WriteTriggerData(
  var AData: TTriggerInstructionData);
begin
  AData.ActionType := atSetNextScenario;
  AData.StringIndex := MapInfo.TrigStringAllocate(Scenario);
end;

function TSetNextScenarioInstruction.Duplicate: TInstruction;
begin
  result := TSetNextScenarioInstruction.Create(Scenario);
end;

class function TSetNextScenarioInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetNextScenario then
    result := TSetNextScenarioInstruction.Create(MapInfo.MapStringRead(AData.StringIndex))
  else
    result := nil;
end;

{ TCenterViewInstruction }

constructor TCenterViewInstruction.Create(ALocation: string);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Location:= ALocation;
end;

function TCenterViewInstruction.ToBasic: string;
begin
  Result:= 'Me.CenterView(';
  if MapInfo.IsAnywhere(Location) then result += 'Anywhere'
  else result += StrToBasic(Location);
  result += ')';
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

function TCenterViewInstruction.Duplicate: TInstruction;
begin
  result := TCenterViewInstruction.Create(Location);
end;

class function TCenterViewInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atCenterView then
    result := TCenterViewInstruction.Create(MapInfo.LocationName[AData.LocationBase0])
  else
    result := nil;
end;

{ TMinimapPingInstruction }

constructor TMinimapPingInstruction.Create(ALocation: string);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Location:= ALocation;
end;

function TMinimapPingInstruction.ToBasic: string;
begin
  Result:= 'Me.MinimapPing(';
  if MapInfo.IsAnywhere(Location) then result += 'Anywhere'
  else result += StrToBasic(Location);
  result += ')';
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

function TMinimapPingInstruction.Duplicate: TInstruction;
begin
  result := TMinimapPingInstruction.Create(Location);
end;

class function TMinimapPingInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atMinimapPing then
    result := TMinimapPingInstruction.Create(MapInfo.LocationName[AData.LocationBase0])
  else
    result := nil;
end;

{ TLeaderBoardIncludeComputersInstruction }

constructor TLeaderBoardIncludeComputersInstruction.Create(AValue: TUnitFlagValue);
begin
  Value:= AValue;
end;

function TLeaderBoardIncludeComputersInstruction.ToBasic: string;
begin
  Result:='Me.Leaderboard.Computers';
  case Value of
  ufvDisable,ufvEnable: result += ' = ' + BoolToStr(Value=ufvEnable,'True','False');
  else result += '.ToggleComputers';
  end;
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

function TLeaderBoardIncludeComputersInstruction.Duplicate: TInstruction;
begin
  result := TLeaderBoardIncludeComputersInstruction.Create(Value);
end;

class function TLeaderBoardIncludeComputersInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atLeaderboardIncludeComputers then
  begin
    case AData.SwitchValue of
    svClear: result := TLeaderBoardIncludeComputersInstruction.Create(ufvDisable);
    svSet: result := TLeaderBoardIncludeComputersInstruction.Create(ufvEnable);
    else result := TLeaderBoardIncludeComputersInstruction.Create(ufvToggle);
    end;
  end else
    exit(nil);
end;

{ TShowLeaderboardOreAndGasIconInstruction }

constructor TShowLeaderboardOreAndGasIconInstruction.Create(AAmount: integer);
begin
  Amount := AAmount;
end;

function TShowLeaderboardOreAndGasIconInstruction.ToBasic: string;
begin
  Result:= 'Me.Leaderboard.Show(MineralsAndGas';
  if Amount > 0 then result += ', '+ inttostr(Amount);
  result += ')';
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

function TShowLeaderboardOreAndGasIconInstruction.Duplicate: TInstruction;
begin
  result:= TShowLeaderboardOreAndGasIconInstruction.Create(Amount);
end;

class function TShowLeaderboardOreAndGasIconInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atLeaderboardGreed then
    result := TShowLeaderboardOreAndGasIconInstruction.Create(AData.GenericValue)
  else
    result := nil;
end;

{ TShowLeaderboardResourceInstruction }

constructor TShowLeaderboardResourceInstruction.Create(AText: string;
  AResource: TStarcraftResource; AGoal: integer);
begin
  Text:= AText;
  Resource:= AResource;
  Goal:= AGoal;
end;

function TShowLeaderboardResourceInstruction.ToBasic: string;
begin
  Result:= 'Me.Leaderboard.Show(' + StrToBasic(Text)+', ';
  if Goal <> -1 then result += inttostr(Goal)+' - ';
  result += StarcraftResourceToBasic[Resource] + ')';
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
  AData.StringIndex := MapInfo.TrigStringAllocate(Text);
end;

function TShowLeaderboardResourceInstruction.Duplicate: TInstruction;
begin
  result := TShowLeaderboardResourceInstruction.Create(Text,Resource,Goal);
end;

class function TShowLeaderboardResourceInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atLeaderboardResources then
    result := TShowLeaderboardResourceInstruction.Create(MapInfo.MapStringRead(AData.StringIndex), AData.ResourceType)
  else if AData.ActionType = atLeaderboardGoalResources then
      result := TShowLeaderboardResourceInstruction.Create(MapInfo.MapStringRead(AData.StringIndex), AData.ResourceType, AData.GenericValue)
  else
    result := nil;
end;

{ TShowLeaderboardScoreInstruction }

constructor TShowLeaderboardScoreInstruction.Create(AText: string;
  AScore: TStarcraftScore; AGoal: integer);
begin
  Text:= AText;
  Score:= AScore;
  Goal:= AGoal;
end;

function TShowLeaderboardScoreInstruction.ToBasic: string;
begin
  Result:= 'Me.Leaderboard.Show(' + StrToBasic(Text)+', ';
  if Goal <> -1 then result += inttostr(Goal)+' - ';
  result += StarcraftScoreToBasic[Score] + ')';
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
  AData.StringIndex := MapInfo.TrigStringAllocate(Text);
end;

function TShowLeaderboardScoreInstruction.Duplicate: TInstruction;
begin
  result := TShowLeaderboardScoreInstruction.Create(Text,Score,Goal);
end;

class function TShowLeaderboardScoreInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atLeaderboardScore then
    result := TShowLeaderboardScoreInstruction.Create(MapInfo.MapStringRead(AData.StringIndex), AData.ScoreType)
  else if AData.ActionType = atLeaderboardGoalScore then
      result := TShowLeaderboardScoreInstruction.Create(MapInfo.MapStringRead(AData.StringIndex), AData.ScoreType, AData.GenericValue)
  else
    result := nil;
end;

{ TShowLeaderboardKillCountInstruction }

constructor TShowLeaderboardKillCountInstruction.Create(AText: string;
  AUnitType: TStarcraftUnit; AGoal: integer);
begin
  Text:= AText;
  UnitType := AUnitType;
  Goal := AGoal;
end;

function TShowLeaderboardKillCountInstruction.ToBasic: string;
begin
  Result:= 'Me.Leaderboard.Show(' + StrToBasic(Text)+', ';
  if Goal <> -1 then result += inttostr(Goal)+' - ';
  result += 'KillCount(' + StarcraftUnitIdentifier[UnitType] + '))';
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
  AData.StringIndex := MapInfo.TrigStringAllocate(Text);
end;

function TShowLeaderboardKillCountInstruction.Duplicate: TInstruction;
begin
  result := TShowLeaderboardKillCountInstruction.Create(Text,UnitType,Goal);
end;

class function TShowLeaderboardKillCountInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atLeaderboardKills then
    result := TShowLeaderboardKillCountInstruction.Create(MapInfo.MapStringRead(AData.StringIndex), AData.UnitType)
  else if AData.ActionType = atLeaderboardGoalKills then
      result := TShowLeaderboardKillCountInstruction.Create(MapInfo.MapStringRead(AData.StringIndex), AData.UnitType, AData.GenericValue)
  else
    result := nil;
end;

{ TShowLeaderboardUnitCountInstruction }

constructor TShowLeaderboardUnitCountInstruction.Create(AText: string;
  AUnitType: TStarcraftUnit; ALocation: string; AGoal: integer);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Text:= AText;
  UnitType := AUnitType;
  Goal := AGoal;
  Location:= ALocation;
end;

function TShowLeaderboardUnitCountInstruction.ToBasic: string;
begin
  Result:= 'Me.Leaderboard.Show(' + StrToBasic(Text)+', ';
  if Goal <> -1 then result += inttostr(Goal)+' - ';
  result += 'UnitCount(' + StarcraftUnitIdentifier[UnitType];
  if not MapInfo.IsAnywhere(Location) then result += ', ' + StrToBasic(Location);
  result += '))';
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
  AData.StringIndex := MapInfo.TrigStringAllocate(Text);
end;

function TShowLeaderboardUnitCountInstruction.Duplicate: TInstruction;
begin
  result := TShowLeaderboardUnitCountInstruction.Create(Text,UnitType,Location,Goal);
end;

class function TShowLeaderboardUnitCountInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
var
  loc: String;
  g: Integer;
begin
  if AData.ActionType in[atLeaderboardControl,atLeaderboardControlAt,atLeaderboardGoalControl,atLeaderboardGoalControlAt] then
  begin
    if AData.ActionType in [atLeaderboardControlAt,atLeaderboardGoalControlAt] then
      loc := MapInfo.LocationName[AData.LocationBase0]
    else loc := MapInfo.AnywhereLocationName;
    if AData.ActionType in [atLeaderboardGoalControl,atLeaderboardGoalControlAt] then
      g := AData.GenericValue
    else g := -1;
    result := TShowLeaderboardUnitCountInstruction.Create(MapInfo.MapStringRead(AData.StringIndex),
      AData.UnitType, loc, g);
  end else
    result := nil;
end;

{ TEndGameInstruction }

constructor TEndGameInstruction.Create(AMode: TEndGameMode);
begin
  Mode := AMode;
end;

function TEndGameInstruction.ToBasic: string;
begin
  case Mode of
  egDefeat: Result:='Defeat()';
  egDraw: Result:='Draw()';
  egVictory: Result := 'Victory()';
  else
    raise exception.Create('Unhandled case');
  end;
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

function TEndGameInstruction.Duplicate: TInstruction;
begin
  result := TEndGameInstruction.Create(Mode);
end;

class function TEndGameInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  case AData.ActionType of
  atDefeat: result := TEndGameInstruction.Create(egDefeat);
  atDraw: result := TEndGameInstruction.Create(egDraw);
  atVictory: result := TEndGameInstruction.Create(egVictory);
  else exit(nil);
  end;
end;

{ TUnitSpeechInstruction }

constructor TUnitSpeechInstruction.Create(AActive: Boolean);
begin
  Active := AActive;
end;

function TUnitSpeechInstruction.ToBasic: string;
begin
  Result:= 'UnitSpeech = ' + BoolToStr(Active);
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

function TUnitSpeechInstruction.Duplicate: TInstruction;
begin
  result := TUnitSpeechInstruction.Create(Active);
end;

class function TUnitSpeechInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  case AData.ActionType of
  atUnmuteUnitSpeeh: result := TUnitSpeechInstruction.Create(true);
  atMuteUnitSpeech: result := TUnitSpeechInstruction.Create(false);
  else result := nil;
  end;
end;

{ TPauseGameInstruction }

constructor TPauseGameInstruction.Create(APaused: Boolean);
begin
  Paused := APaused;
end;

function TPauseGameInstruction.ToBasic: string;
begin
  Result:= 'GamePaused = ' + BoolToStr(Paused);
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

function TPauseGameInstruction.Duplicate: TInstruction;
begin
  result := TPauseGameInstruction.Create(Paused);
end;

class function TPauseGameInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  case AData.ActionType of
  atPauseGame: result := TPauseGameInstruction.Create(true);
  atUnpauseGame: result := TPauseGameInstruction.Create(false);
  else result := nil;
  end;
end;

{ TPauseCountdownInstruction }

constructor TPauseCountdownInstruction.Create(APaused: Boolean);
begin
  Paused := APaused;
end;

function TPauseCountdownInstruction.ToBasic: string;
begin
  Result:= 'CountdownPaused = ' + BoolToStr(Paused);
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

function TPauseCountdownInstruction.Duplicate: TInstruction;
begin
  result := TPauseCountdownInstruction.Create(Paused);
end;

class function TPauseCountdownInstruction.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  case AData.ActionType of
  atPauseCountdown: result := TPauseCountdownInstruction.Create(true);
  atUnpauseCountdown: result := TPauseCountdownInstruction.Create(false);
  else result := nil;
  end;
end;

{ TSetAllianceStatus }

constructor TSetAllianceStatus.Create(APlayer: TPlayer; AStatus: TAllianceStatus);
begin
  Player:= APlayer;
  Status:= AStatus;
end;

function TSetAllianceStatus.ToBasic: string;
begin
  Result:= 'Me.Alliance.';
  case Status of
  asEnnemy: result += 'Ennemy';
  asAlly: result += 'Ally';
  asAlliedVictory: result += 'AlliedVictory';
  end;
  result += '(' + PlayerIdentifiers[Player]+')';
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
  AData.Player := Player;
  AData.Alliance:= Status;
end;

function TSetAllianceStatus.Duplicate: TInstruction;
begin
  result := TSetAllianceStatus.Create(Player,Status);
end;

class function TSetAllianceStatus.LoadFromData(
  const AData: TTriggerInstructionData): TTriggerInstruction;
begin
  if AData.ActionType = atSetAlliance then
    result := TSetAllianceStatus.Create(AData.Player, AData.Alliance)
  else
    result := nil;
end;

initialization

  CreateSetIntegerInstruction := @CreateSetIntegerInstructionImplementation;

end.

