unit uinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

var
  AnywhereLocation : string = 'Anywhere';
  Force : array[1..4] of string = ('Force 1', 'Force 2', 'Force 3', 'Force 4');
  SwitchNames : array[1..256] of string;

function PlayerToStr(APlayer: TPlayer): string;
function IsAnywhere(ALocation: string): boolean;
function SwitchToStr(ASwitch: integer): string;

type
  { TInstruction }

  TInstruction = class
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual;
  end;

  { TEmptyInstruction }

  TEmptyInstruction = class(TInstruction)
  end;

  TCustomInstructionList = specialize TFPGList<TInstruction>;

  { TInstructionList }

  TInstructionList = class(TCustomInstructionList)
    procedure FreeAll;
  end;

const
  SwitchValueToStr : array[TSwitchValue] of string = ('clear','set','randomize','toggle');

type
  { TCondition }

  TCondition = class
    function ToTrigEditAndFree: ansistring;
    function ToTrigEdit: string; virtual;
    function IsArithmetic: boolean; virtual;
    function IsComputed: boolean; virtual;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); virtual;
    function Duplicate: TCondition; virtual; abstract;
  end;

  TCustomConditionList = specialize TFPGList<TCondition>;

  { TConditionList }

  TConditionList = class(TCustomConditionList)
    function IsComputed: boolean;
    function IsArithmetic: boolean;
    procedure Compute(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string);
    procedure FreeAll;
    function Duplicate: TConditionList;
  end;

type
  TSetIntegerMode = (simSetTo, simAdd, simSubtract);

function SetIntegerModeToStr(AMode: TSetIntegerMode): string;

type
  { TSetIntegerInstruction }

  TSetIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer);
    function ConvertToSpecificInstruction: TInstruction;
  end;

  { TRandomizeIntegerInstruction }

  TRandomizeIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Range: integer;
    constructor Create(APlayer: TPlayer; AUnitType: string; ARange: integer);
  end;

  TIntegerTransfer = (itCopyIntoAccumulator, itAddIntoAccumulator, itCopyAccumulator, itAddAccumulator,
                      itSubtractIntoAccumulator, itSubtractAccumulator, itRandomizeAccumulator, itLimitAccumulator);

  { TTransferIntegerInstruction }

  TTransferIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Action: TIntegerTransfer;
    Value: integer;
    Shift: integer;
    constructor Create(APlayer: TPlayer; AUnitType: string; AAction: TIntegerTransfer; AShift: integer = 0);
    constructor Create(AValue: integer; AAction: TIntegerTransfer);
  end;

  { TPrintForAnyPlayerInstruction }

  TPrintForAnyPlayerInstruction = class(TInstruction)
    Msg: integer;
    constructor Create(AMsg: integer);
  end;

  { TCallInstruction }

  TCallInstruction = class(TInstruction)
    Name: string;
    Params: array of string;
    ReturnType: string;
    constructor Create(AName: string; AParams: array of string; AReturnType: string = 'Void');
    constructor Create(AName: string; AParams: TStringList; AReturnType: string = 'Void');
  end;

  { TDropThreadInstruction }

  TDropThreadInstruction = class(TInstruction)
    DropIP, ResumeIP: integer;
    PlayersToDrop, PlayersToResume: TPlayers;
    constructor Create(ADropIP, AResumeIP: integer; APlayersToDrop, APlayersToResume: TPlayers);
  end;

  { TWaitForPlayersInstruction }

  TWaitForPlayersInstruction = class(TInstruction)
    Players: TPlayers;
    AwaitPresenceDefined: boolean;
    constructor Create(APlayers: TPlayers; AAwaitPresenceDefined: boolean);
  end;

  { TReturnInstruction }

  TReturnInstruction = class(TInstruction)
    constructor Create;
  end;

  { TDoAsInstruction }

  TDoAsInstruction = class(TInstruction)
    Players: TPlayers;
    constructor Create(APlayers: TPlayers);
  end;

  { TEndDoAsInstruction }

  TEndDoAsInstruction = class(TInstruction)
    Players: TPlayers;
    constructor Create(APlayers: TPlayers);
  end;

  { TWaitForPresenceDefinedInstruction }

  TWaitForPresenceDefinedInstruction = class(TInstruction)
    constructor Create;
  end;

  { TIfInstruction }

  TIfInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition);
    constructor Create(AConditions: TConditionList);
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
  end;

  { TWaitConditionInstruction }

  TWaitConditionInstruction = class(TInstruction)
    Conditions: TConditionList;
    IP: integer;
    destructor Destroy; override;
    constructor Create(AConditions: TConditionList; AIP: Integer);
    constructor Create(ACondition: TCondition; AIP: Integer);
  end;

  { TElseInstruction }

  TElseInstruction = class(TInstruction)
    constructor Create;
  end;

  { TSplitInstruction }

  TSplitInstruction = class(TInstruction)
    ResumeIP, EndIP: integer;
    ChangePlayers: TPlayers;
    constructor Create(AResumeIP, AEndIP: integer; AChangePlayers: TPlayers = []);
  end;

  { TEndIfInstruction }

  TEndIfInstruction = class(TInstruction)
    constructor Create;
  end;

  { TChangeIPInstruction }

  TChangeIPInstruction = class(TInstruction)
    IP: integer;
    Preserve: Integer;
    constructor Create(AIP: Integer; APreserve: Integer);
  end;

  { TWhileInstruction }

  TWhileInstruction = class(TInstruction)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: TConditionList);
  end;

  { TEndWhileInstruction }

  TEndWhileInstruction = class(TInstruction)
    constructor Create;
  end;

  /////////////////////////////////////////////////////////////////////////////////////////////

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TInstruction)
    Switch: integer;
    Value: TSwitchValue;
    constructor Create(ASwitch: integer; AValue: TSwitchValue);
    function ToTrigEdit: string; override;
  end;

  { TSetDeathInstruction }

  TSetDeathInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  TStarcraftResource = (srOre, srGas, srOreAndGas);

function StarcraftResourceToStr(AResource: TStarcraftResource): string;

type
  { TSetResourceInstruction }

  TSetResourceInstruction = class(TInstruction)
    Player: TPlayer;
    Resource: TStarcraftResource;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AResource: TStarcraftResource; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  TStarcraftScore = (ssUnitScore, ssBuildingScore, ssUnitAndBuildingScore,
                     ssKillScore, ssRazingScore, ssKillAndRazingScore,
                     ssTotalScore, ssCustomScore);

function StarcraftScoreToStr(AScore: TStarcraftScore): string;

type
  { TSetScoreInstruction }

  TSetScoreInstruction = class(TInstruction)
    Player: TPlayer;
    Score: TStarcraftScore;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AScore: TStarcraftScore; AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  { TSetCountdownInstruction }

  TSetCountdownInstruction = class(TInstruction)
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(AMode: TSetIntegerMode; AValue: integer);
    function ToTrigEdit: string; override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TInstruction)
    Always: boolean;
    Text: string;
    constructor Create(AAlways: boolean; AMessage: string);
    function ToTrigEdit: string; override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToTrigEdit: string; override;
  end;

  { TCreateUnitInstruction }

  TCreateUnitInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    Properties: integer;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; AProperties: integer = -1);
    function ToTrigEdit: string; override;
  end;

  TSetUnitProperty = (supLife, supShield, supEnergy, supResource, supHangarCount);

  { TSetUnitPropertyInstruction }

  TSetUnitPropertyInstruction = class(TInstruction)
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

  TSetUnitFlagInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    Flag: TSetUnitFlag;
    Value: TUnitFlagValue;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; AFlag: TSetUnitFlag; AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
  end;

  { TKillUnitInstruction }

  TKillUnitInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    DeathAnimation: boolean;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType: string; ALocation: string = ''; ADeathAnimation: boolean = true);
    function ToTrigEdit: string; override;
  end;

  { TGiveUnitInstruction }

  TGiveUnitInstruction = class(TInstruction)
    Player, DestPlayer: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; ADestPlayer: TPlayer);
    function ToTrigEdit: string; override;
  end;

  { TTeleportUnitInstruction }

  TTeleportUnitInstruction = class(TInstruction)
    Player: TPlayer;
    Quantity: integer;
    UnitType, Location: string;
    DestLocation: string;
    constructor Create(APlayer: TPlayer; AQuantity: integer; AUnitType, ALocation: string; ADestLocation: string);
    function ToTrigEdit: string; override;
  end;

  { TMoveLocationInstruction }

  TMoveLocationInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    LocationToChange: string;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; ALocationToChange: string);
    function ToTrigEdit: string; override;
  end;

  TUnitOrder = (uoMove, uoPatrol, uoAttack);

  { TOrderUnitInstruction }

  TOrderUnitInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType, Location: string;
    DestLocation: string;
    Order: TUnitOrder;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; ADestLocation: string; AOrder: TUnitOrder);
    function ToTrigEdit: string; override;
  end;

  { TPlayWAVInstruction }

  TPlayWAVInstruction = class(TInstruction)
    Filename: string;
    DurationMs: integer;
    constructor Create(AFilename: string; ADurationMs: integer);
    function ToTrigEdit: string; override;
  end;

  { TTalkingPortraitInstruction }

  TTalkingPortraitInstruction = class(TInstruction)
    UnitType: string;
    DurationMs: integer;
    constructor Create(AUnitType: string; ADurationMs: integer);
    function ToTrigEdit: string; override;
  end;

  { TRunAIScriptInstruction }

  TRunAIScriptInstruction = class(TInstruction)
    ScriptCode, Location: string;
    constructor Create(AScriptCode, ALocation: string);
    function ToTrigEdit: string; override;
  end;

  { TSetMissionObjectivesInstruction }

  TSetMissionObjectivesInstruction = class(TInstruction)
    Text: string;
    constructor Create(AText: string);
    function ToTrigEdit: string; override;
  end;

  { TSetNextScenarioInstruction }

  TSetNextScenarioInstruction = class(TInstruction)
    Scenario: string;
    constructor Create(AScenario: string);
    function ToTrigEdit: string; override;
  end;

  { TCenterViewInstruction }

  TCenterViewInstruction = class(TInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
  end;

  { TMinimapPingInstruction }

  TMinimapPingInstruction = class(TInstruction)
    Location: string;
    constructor Create(ALocation: string);
    function ToTrigEdit: string; override;
  end;

  { TLeaderBoardIncludeComputersInstruction }

  TLeaderBoardIncludeComputersInstruction = class(TInstruction)
    Value: TUnitFlagValue;
    constructor Create(AValue: TUnitFlagValue);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardOreAndGasIconInstruction }

  TShowLeaderboardOreAndGasIconInstruction = class(TInstruction)
    Amount: integer;
    constructor Create(AAmount: integer);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardResourceInstruction }

  TShowLeaderboardResourceInstruction = class(TInstruction)
    Text: string;
    Resource: TStarcraftResource;
    Goal: integer;
    constructor Create(AText: string; AResource: TStarcraftResource; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardScoreInstruction }

  TShowLeaderboardScoreInstruction = class(TInstruction)
    Text: string;
    Score: TStarcraftScore;
    Goal: integer;
    constructor Create(AText: string; AScore: TStarcraftScore; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardKillCountInstruction }

  TShowLeaderboardKillCountInstruction = class(TInstruction)
    Text, UnitType: string;
    Goal: integer;
    constructor Create(AText,AUnitType: string; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  { TShowLeaderboardUnitCountInstruction }

  TShowLeaderboardUnitCountInstruction = class(TInstruction)
    Text, UnitType,Location: string;
    Goal: integer;
    constructor Create(AText,AUnitType,ALocation: string; AGoal: integer = -1);
    function ToTrigEdit: string; override;
  end;

  TEndGameMode = (egDefeat, egDraw, egVictory);

  { TEndGameInstruction }

  TEndGameInstruction = class(TInstruction)
    Mode: TEndGameMode;
    constructor Create(AMode: TEndGameMode);
    function ToTrigEdit: string; override;
  end;

  { TUnitSpeechInstruction }

  TUnitSpeechInstruction = class(TInstruction)
    Active: boolean;
    constructor Create(AActive: Boolean);
    function ToTrigEdit: string; override;
  end;

  { TPauseGameInstruction }

  TPauseGameInstruction = class(TInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
  end;

  { TPauseCountdownInstruction }

  TPauseCountdownInstruction = class(TInstruction)
    Paused: boolean;
    constructor Create(APaused: Boolean);
    function ToTrigEdit: string; override;
  end;

  TAllianceStatus = (asEnnemy, asAlly, asAlliedVictory);

  { TSetAllianceStatus }

  TSetAllianceStatus = class(TInstruction)
    Player: TPlayer;
    Status: TAllianceStatus;
    constructor Create(APlayer: TPlayer; AStatus: TAllianceStatus);
    function ToTrigEdit: string; override;
  end;

type

  { TAlwaysCondition }

  TAlwaysCondition = class(TCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: string); override;
    function Duplicate: TCondition; override;
  end;

  { TNeverCondition }

  TNeverCondition = class(TCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); override;
    function Duplicate: TCondition; override;
  end;

  { TNotCondition }

  TNotCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function ToTrigEdit: string; override;
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); override;
    function Duplicate: TCondition; override;
  end;

  { TOrCondition }

  TOrCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function ToTrigEdit: string; override;
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); override;
    function Duplicate: TCondition; override;
  end;

  { TAndCondition }

  TAndCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function ToTrigEdit: string; override;
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: string); override;
    function Duplicate: TCondition; override;
  end;

  { TSwitchCondition }

  TSwitchCondition = class(TCondition)
    Switch: integer;
    Value: boolean;
    constructor Create(ASwitch: integer; AValue: boolean);
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); override;
    function Duplicate: TCondition; override;
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
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TBringCondition }

  TBringCondition = class(TCondition)
    Player: TPlayer;
    UnitType, Location: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TKillCountCondition }

  TKillCountCondition = class(TCondition)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TOpponentCountCondition }

  TOpponentCountCondition = class(TCondition)
    Player: TPlayer;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TElapsedTimeCondition }

  TElapsedTimeCondition = class(TCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TCompareUnitCountCondition }

  TCompareUnitCountCondition = class(TCondition)
    UnitType, Location: string;
    Highest: boolean;
    constructor Create(AUnitType, ALocation: string; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TCompareKillCountCondition }

  TCompareKillCountCondition = class(TCondition)
    UnitType: string;
    Highest: boolean;
    constructor Create(AUnitType: string; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

  { TCompareIntegerCondition }

  TCompareIntegerCondition = class(TCondition)
    ValueType: string;
    Highest: boolean;
    IsScore, IsResource: boolean;
    constructor Create(AValueType: string; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TCondition; override;
  end;

implementation

function AddQuotes(AText: string): string;
var
  i: Integer;
begin
  result := '"' + StringReplace(StringReplace(AText, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]) + '"';
  for i := length(result) downto 1 do
    if result[i] in [#0..#31] then
    begin
      if result[i] = #13 then insert('\r',result,i+1)
      else if result[i] = #10 then insert('\n',result,i+1)
      else insert('\x0' +IntToHex(ord(result[i]),2), result,i+1);
      delete(result,i,1);
    end;
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
  plForce1..plForce4: result := Force[ord(APlayer)-ord(plForce1)+1];
  plNonAlliedVictoryPlayers: result := 'Non Allied Victory Players';
  else result := 'Unknown';
  end;
end;

function IsAnywhere(ALocation: string): boolean;
begin
  result := (ALocation = '') or (CompareText(ALocation, AnywhereLocation)=0);
end;

function SwitchToStr(ASwitch: integer): string;
begin
  if (ASwitch < low(SwitchNames)) or (ASwitch > high(SwitchNames)) then
    raise exception.Create('Index out of bounds');
  if SwitchNames[ASwitch] = '' then result := 'Switch'+IntToStr(ASwitch)
  else result := SwitchNames[ASwitch];
end;

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
  result += '(' + AddQuotes(Text) + ', ' + StarcraftScoreToStr(Score);
  if Goal <> -1 then result += ', ' + inttostr(Goal);
  result += ')';
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
  result += '(' + AddQuotes(Text) + ', ' + StarcraftResourceToStr(Resource);
  if Goal <> -1 then result += ', ' + inttostr(Goal);
  result += ')';
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

  result += '(' + AddQuotes(PlayerToStr(Player))+', '+AddQuotes(UnitType)+', '+AddQuotes(Location)+', ';
  case Value of
  ufvDisable: result += 'disabled';
  ufvEnable: result += 'enabled';
  else result += 'toggle';
  end;
  result += ')';
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
  Result:= 'Set Score("' + PlayerToStr(Player) + '", ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ', ' + StarcraftScoreToStr(Score) + ')'
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
  Result:= 'Set Resources("' + PlayerToStr(Player) + '", ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ', ' + StarcraftResourceToStr(Resource) + ')'
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
  Result:= 'Set Deaths("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' + SetIntegerModeToStr(Mode) + ', ' + IntToStr(Value) + ')';
end;

{ TRandomizeIntegerInstruction }

constructor TRandomizeIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: string; ARange: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Range := ARange;
  if Range < 1 then raise exception.Create('Invalid range');
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

function TAndCondition.ToTrigEdit: string;
begin
  Result:= 'And condition';
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
  APlayer: TPlayer; AUnitType: string);
begin
  raise exception.Create('Not handled');
end;

function TAndCondition.Duplicate: TCondition;
begin
  result := TAndCondition.Create(Conditions.Duplicate);
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

function TOrCondition.ToTrigEdit: string;
begin
  Result:='Or Condition';
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
  APlayer: TPlayer; AUnitType: string);
var
  i: Integer;
begin
  if Conditions.Count > 0 then
  begin
    AProg.Add(TIfInstruction.Create(TIntegerCondition.Create(APlayer,AUnitType,icmAtLeast,1)));
    AProg.Add(TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,0));
    for i := 0 to Conditions.Count-1 do
    begin
      if not Conditions[i].IsComputed then
      begin
        AProg.Add(TFastIfInstruction.Create(Conditions[i],[TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,1)]));
      end else
      begin
        AProg.Add(TIfInstruction.Create(Conditions[i].Duplicate));
        AProg.Add(TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,1));
        AProg.Add(TEndIfInstruction.Create);
      end;
    end;
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

function TOrCondition.Duplicate: TCondition;
begin
  result := TOrCondition.Create(Conditions.Duplicate);
end;

{ TPrintForAnyPlayerInstruction }

constructor TPrintForAnyPlayerInstruction.Create(AMsg: integer);
begin
  Msg := AMsg;
end;

{ TWaitForPresenceDefinedInstruction }

constructor TWaitForPresenceDefinedInstruction.Create;
begin
  //
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

{ TWaitForPlayersInstruction }

constructor TWaitForPlayersInstruction.Create(APlayers: TPlayers;
  AAwaitPresenceDefined: boolean);
begin
  Players:= APlayers;
  AwaitPresenceDefined:= AAwaitPresenceDefined;
end;

{ TEndDoAsInstruction }

constructor TEndDoAsInstruction.Create(APlayers: TPlayers);
begin
  Players:= APlayers;
end;

{ TDoAsInstruction }

constructor TDoAsInstruction.Create(APlayers: TPlayers);
begin
  Players:= APlayers;
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
  Result:= 'Move Location(' + AddQuotes(PlayerToStr(Player)) + ', '+AddQuotes(UnitType)+', ';
  result += AddQuotes(Location)+', '+AddQuotes(LocationToChange)+')';
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

  result += '(' + AddQuotes(Text) + ', '+ AddQuotes(UnitType);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);

  if not IsAnywhere(Location) then result += ', ' + AddQuotes(Location);

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

  result += '(' + AddQuotes(Text) + ', '+ AddQuotes(UnitType);

  if Goal <> -1 then
    result += ', ' + IntToStr(Goal);
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

{ TTalkingPortraitInstruction }

constructor TTalkingPortraitInstruction.Create(AUnitType: string;
  ADurationMs: integer);
begin
  UnitType:= AUnitType;
  DurationMs:= ADurationMs;
end;

function TTalkingPortraitInstruction.ToTrigEdit: string;
begin
  Result:= 'Talking Portrait(' + AddQuotes(UnitType) + ', ' + inttostr(DurationMs) + ')';
end;

{ TSetAllianceStatus }

constructor TSetAllianceStatus.Create(APlayer: TPlayer; AStatus: TAllianceStatus);
begin
  Player:= APlayer;
  Status:= AStatus;
end;

function TSetAllianceStatus.ToTrigEdit: string;
begin
  Result:='Set Alliance Status(' + AddQuotes(PlayerToStr(Player))+ ', ';
  case Status of
  asEnnemy: result += 'Ennemy';
  asAlly: result += 'Ally';
  asAlliedVictory: result += 'AlliedVictory';
  end;
  result += ')';
end;

{ TSetNextScenarioInstruction }

constructor TSetNextScenarioInstruction.Create(AScenario: string);
begin
  Scenario:= AScenario;
end;

function TSetNextScenarioInstruction.ToTrigEdit: string;
begin
  Result:='Set Next Scenario(' + AddQuotes(Scenario) + ')';
end;

{ TSetMissionObjectivesInstruction }

constructor TSetMissionObjectivesInstruction.Create(AText: string);
begin
  Text := AText;
end;

function TSetMissionObjectivesInstruction.ToTrigEdit: string;
begin
  Result:= 'Set Mission Objectives(' + AddQuotes(Text) + ')';
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
  Result += '(' + AddQuotes(ScriptCode);
  if not IsAnywhere(Location) then Result += ', ' + AddQuotes(Location);
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
  Result:= 'Play WAV(' + AddQuotes(Filename) + ', ' + inttostr(DurationMs) + ')';
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

{ TMinimapPingInstruction }

constructor TMinimapPingInstruction.Create(ALocation: string);
begin
  Location:= ALocation;
end;

function TMinimapPingInstruction.ToTrigEdit: string;
begin
  Result:= 'Minimap Ping(' + AddQuotes(Location) + ')';
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

{ TCenterViewInstruction }

constructor TCenterViewInstruction.Create(ALocation: string);
begin
  Location:= ALocation;
end;

function TCenterViewInstruction.ToTrigEdit: string;
begin
  Result:= 'Center View(' + AddQuotes(Location) + ')';
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

function TCompareIntegerCondition.ToTrigEdit: string;
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

function TCompareIntegerCondition.Duplicate: TCondition;
begin
  result := TCompareIntegerCondition.Create(ValueType,Highest);
end;

{ TCompareKillCountCondition }

constructor TCompareKillCountCondition.Create(AUnitType: string;
  AHighest: boolean);
begin
  UnitType := AUnitType;
  Highest:= AHighest;
end;

function TCompareKillCountCondition.ToTrigEdit: string;
begin
  if Highest then result := 'Most' else result := 'Least';
  Result += ' Kills';
  result += '(' + AddQuotes(UnitType) + ')';
end;

function TCompareKillCountCondition.Duplicate: TCondition;
begin
  result := TCompareKillCountCondition.Create(UnitType,Highest);
end;

{ TElapsedTimeCondition }

constructor TElapsedTimeCondition.Create(AMode: TIntegerConditionMode;
  AValue: integer);
begin
  Mode := AMode;
  Value := AValue;
end;

function TElapsedTimeCondition.ToTrigEdit: string;
begin
  Result:= 'Elapsed Time(' + IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TElapsedTimeCondition.Duplicate: TCondition;
begin
  result := TElapsedTimeCondition.Create(Mode,Value);
end;

{ TOpponentCountCondition }

constructor TOpponentCountCondition.Create(APlayer: TPlayer;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  Mode := AMode;
  Value := AValue;
end;

function TOpponentCountCondition.ToTrigEdit: string;
begin
  Result:= 'Opponents("' + PlayerToStr(Player) + '", ' +
       IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TOpponentCountCondition.Duplicate: TCondition;
begin
  result := TOpponentCountCondition.Create(Player,Mode,Value);
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

function TKillCountCondition.ToTrigEdit: string;
begin
  Result:= 'Kill("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' +
       IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TKillCountCondition.Duplicate: TCondition;
begin
  result := TKillCountCondition.Create(Player,UnitType,Mode,Value);
end;

{ TCompareUnitCountCondition }

constructor TCompareUnitCountCondition.Create(AUnitType,
  ALocation: string; AHighest: boolean);
begin
  UnitType := AUnitType;
  Location:= ALocation;
  Highest:= AHighest;
end;

function TCompareUnitCountCondition.ToTrigEdit: string;
begin
  Result:= 'Command ';
  if Highest then result += 'the Most' else result += 'the Least';
  if not IsAnywhere(Location) then result += ' At(' + AddQuotes(UnitType) + ', ' +AddQuotes(Location)+')'
  else result += '(' + AddQuotes(UnitType) + ')';
end;

function TCompareUnitCountCondition.Duplicate: TCondition;
begin
  result := TCompareUnitCountCondition.Create(UnitType,Location,Highest);
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

function TNotCondition.ToTrigEdit: string;
begin
  Result:= '!' + Conditions.ToString;
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
  APlayer: TPlayer; AUnitType: string);
begin
  if not Conditions.IsComputed then
  begin
    AProg.Add(TFastIfInstruction.Create(Conditions,[TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,0)]));
  end else
  begin
    AProg.Add(TIfInstruction.Create(Conditions.Duplicate));
    AProg.Add(TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,0));
    AProg.Add(TEndIfInstruction.Create);
  end;
end;

function TNotCondition.Duplicate: TCondition;
begin
  result := TNotCondition.Create(Conditions.Duplicate);
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
  result := 'Order(' + AddQuotes(PlayerToStr(Player)) + ', '+AddQuotes(UnitType);
  result += ', '+AddQuotes(Location)+', '+AddQuotes(DestLocation)+', ';
  case Order of
  uoPatrol: result += 'patrol';
  uoAttack: result += 'attack';
  else result += 'move';
  end;
  result += ')';
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
      result += '(' + AddQuotes(PlayerToStr(Player))+', '+inttostr(Value)+', ';
      result += inttostr(Quantity)+', '+AddQuotes(Location)+')';
      exit;
    end;
  supHangarCount: result := 'Modify Unit Hanger Count';
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

function TGiveUnitInstruction.ToTrigEdit: string;
begin
  result := 'Give Units to Player(' + AddQuotes(PlayerToStr(Player))+', '+ AddQuotes(PlayerToStr(DestPlayer))+ ', ' +
    AddQuotes(UnitType)+', ';
  if Quantity = -1 then result += 'All' else result += IntToStr(Quantity);
  result += ', ' + AddQuotes(Location) + ')';
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
    result += '("'+ PlayerToStr(Player)+'", ' + AddQuotes(UnitType)+', ';
    if Quantity = -1 then result += 'All' else result += inttostr(Quantity);
    result += ', '+AddQuotes(Location) + ')';
  end
  else
    Result += '(' + AddQuotes(PlayerToStr(Player))+', '+AddQuotes(UnitType)+')';
end;

{ TSplitInstruction }

constructor TSplitInstruction.Create(AResumeIP, AEndIP: integer;
  AChangePlayers: TPlayers);
begin
  ResumeIP:= AResumeIP;
  EndIP := AEndIP;
  ChangePlayers := AChangePlayers;
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

function TBringCondition.ToTrigEdit: string;
begin
  if IsAnywhere(Location) then
    Result:= 'Command("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' +
         IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Bring("' + PlayerToStr(Player) + '", ' + AddQuotes(UnitType) + ', ' + AddQuotes(Location) + ', ' +
         IntegerConditionModeToStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TBringCondition.Duplicate: TCondition;
begin
  result := TBringCondition.Create(Player,UnitType,Location,Mode,Value);
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

  result += '("'+ PlayerToStr(Player)+'", ' + AddQuotes(UnitType)+', ';
  result += inttostr(Quantity) + ', '+AddQuotes(Location);
  if Properties >=0 then result += ', ' + inttostr(Properties+1);

  result += ')';
end;

{ TTransferIntegerInstruction }

constructor TTransferIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: string; AAction: TIntegerTransfer; AShift: integer);
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
  UnitType:= 'Const';
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
  AUnitType: string);
var
  i: Integer;
begin
  AProg.Add(TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,1));
  for i := 0 to Count-1 do
    Items[i].AddToProgAsAndVar(AProg, APlayer, AUnitType);
end;

procedure TConditionList.FreeAll;
var i: integer;
begin
  if self = nil then exit;

  for i := 0 to Count-1 do
    Items[i].Free;
  Free;
end;

function TConditionList.Duplicate: TConditionList;
var
  i: Integer;
begin
  result := TConditionList.Create;
  for i := 0 to Count-1 do
    result.Add(Items[i]);
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

{ TChangeIPInstruction }

constructor TChangeIPInstruction.Create(AIP: Integer; APreserve: Integer);
begin
  IP := AIP;
  Preserve := APreserve;
end;

{ TNeverCondition }

function TNeverCondition.ToTrigEdit: string;
begin
  Result:= 'Never()';
end;

procedure TNeverCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: string);
begin
  AProg.Add(TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,0));
end;

function TNeverCondition.Duplicate: TCondition;
begin
  result := TNeverCondition.Create;
end;

{ TEndIfInstruction }

constructor TEndIfInstruction.Create;
begin
  //nothing
end;

{ TElseInstruction }

constructor TElseInstruction.Create;
begin
  //nothing
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

{ TEndWhileInstruction }

constructor TEndWhileInstruction.Create;
begin
 //nothing
end;

{ TReturnInstruction }

constructor TReturnInstruction.Create;
begin
  //nothing
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

{ TCallInstruction }

constructor TCallInstruction.Create(AName: string; AParams: array of string; AReturnType: string = 'Void');
var
  i: Integer;
begin
  Name := AName;
  setlength(Params, length(AParams));
  for i := 0 to high(AParams) do
    Params[i] := AParams[i];
  ReturnType:= AReturnType;
end;

constructor TCallInstruction.Create(AName: string; AParams: TStringList; AReturnType: string = 'Void');
var
  i: Integer;
begin
  Name := AName;
  setlength(Params, AParams.Count);
  for i := 0 to AParams.Count-1 do
    Params[i] := AParams[i];
  ReturnType:= AReturnType;
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
            ', ' + AddQuotes(Text) + ')';
end;

{ TCondition }

function TCondition.ToTrigEditAndFree: ansistring;
begin
  result := ToTrigEdit;
  Free;
end;

function TCondition.ToTrigEdit: string;
begin
  result := '?';
  raise exception.Create('Condition not translatable to TrigEdit code');
end;

function TCondition.IsArithmetic: boolean;
begin
  result := false;
end;

function TCondition.IsComputed: boolean;
begin
  result := IsArithmetic;
end;

procedure TCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: string);
begin
  AProg.Add(TIfInstruction.Create(self.Duplicate));
  AProg.Add(TElseInstruction.Create);
  AProg.Add(TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,0));
  AProg.Add(TEndIfInstruction.Create);
end;

{ TInstruction }

function TInstruction.ToTrigEditAndFree: string;
begin
  result := ToTrigEdit;
  Free;
end;

function TInstruction.ToTrigEdit: string;
begin
  result := '?';
  raise exception.Create('Instruction not translatable to TrigEdit code');
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

function TIntegerCondition.ToTrigEdit: string;
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

function TIntegerCondition.Duplicate: TCondition;
begin
  result := TIntegerCondition.Create(Player,UnitType,Mode,Value);
end;

{ TAlwaysCondition }

function TAlwaysCondition.ToTrigEdit: string;
begin
  Result:= 'Always()';
end;

procedure TAlwaysCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: string);
begin
  //nothing
end;

function TAlwaysCondition.Duplicate: TCondition;
begin
  result := TAlwaysCondition.Create;
end;

{ TSwitchCondition }

constructor TSwitchCondition.Create(ASwitch: integer; AValue: boolean);
begin
  Switch := ASwitch;
  Value := AValue;
end;

function TSwitchCondition.ToTrigEdit: string;
begin
  Result:= 'Switch("Switch' + IntToStr(Switch) + '", ' + BoolToStr(Value, 'set', 'not set') + ')';
end;

procedure TSwitchCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: string);
begin
  AProg.Add(TFastIfInstruction.Create([TSwitchCondition.Create(Switch,not Value)], [TSetIntegerInstruction.Create(APlayer,AUnitType,simSetTo,0)]));
end;

function TSwitchCondition.Duplicate: TCondition;
begin
  result := TSwitchCondition.Create(Switch,Value);
end;

{ TSetIntegerInstruction }

constructor TSetIntegerInstruction.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TSetIntegerMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
  if Value < 0 then
  begin
    if Mode = simSubtract then
    begin
      Value := -Value;
      Mode := simAdd;
    end else
    if Mode = simAdd then
    begin
      Value := -Value;
      Mode := simSubtract;
    end else
      Value := 0;
  end;
end;

function TSetIntegerInstruction.ConvertToSpecificInstruction: TInstruction;
begin
  if CompareText(UnitType,'Ore')=0 then
    result := TSetResourceInstruction.Create(Player,srOre,Mode,Value) else
  if CompareText(UnitType,'Gas')=0 then
    result := TSetResourceInstruction.Create(Player,srGas,Mode,Value) else
  if CompareText(UnitType,'Ore And Gas')=0 then
    result := TSetResourceInstruction.Create(Player,srOreAndGas,Mode,Value) else
  if CompareText(copy(UnitType,length(UnitType)-5,6),' Score')=0 then
  begin
    if compareText(UnitType, 'Units Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssUnitScore,Mode,Value) else
    if compareText(UnitType, 'Buildings Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssBuildingScore,Mode,Value) else
    if compareText(UnitType, 'Units and buildings Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssUnitAndBuildingScore,Mode,Value) else
    if compareText(UnitType, 'Kills Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssKillScore,Mode,Value) else
    if compareText(UnitType, 'Razings Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssRazingScore,Mode,Value) else
    if compareText(UnitType, 'Kills and razings Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssKillAndRazingScore,Mode,Value) else
    if compareText(UnitType, 'Custom Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssCustomScore,Mode,Value) else
    if compareText(UnitType, 'Total Score')=0 then
      result := TSetScoreInstruction.Create(Player,ssTotalScore,Mode,Value) else
        raise exception.Create('Unknown score type "'+UnitType+'"');
  end
  else
  if CompareText(UnitType,'Countdown')= 0 then
    result := TSetCountdownInstruction.Create(Mode,Value)
  else
    Result:= TSetDeathInstruction.Create(Player, UnitType, Mode, Value);
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
  Result:= 'Set Switch('+AddQuotes(SwitchToStr(Switch))+ ', ' + SwitchValueToStr[Value] + ')';
end;

end.

