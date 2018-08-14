unit utriggerchunk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes;

const
  //Condition types
  ctNone = 0;
  ctCountdown = 1;
  ctCommand = 2;
  ctBring = 3;
  ctAccumulate = 4;
  ctKillCount = 5;
  ctCommandTheMost = 6;
  ctCommandTheMostAt = 7;
  ctMostKills = 8;
  ctHighestScore = 9;
  ctMostResources = 10;
  ctSwitch = 11;
  ctElapsedTime = 12;
  ctMissiongBriefing = 13;
  ctOpponentCount = 14;
  ctDeathCount = 15;
  ctCommandTheLeast = 16;
  ctCommandTheLeastAt = 17;
  ctLeastKills = 18;
  ctLowestScore = 19;
  ctLeastResources = 20;
  ctScore = 21;
  ctAlways = 22;
  ctNever = 23;

type
  { TTriggerConditionData }

  TTriggerConditionData = object
  private
    function GetAlwaysDisplay: boolean;
    function GetEnabled: boolean;
    function GetExpectedSwitchValue: boolean;
    function GetHasLocation: boolean;
    function GetIntegerComparison: TIntegerConditionMode;
    function GetLocationBase0: Integer;
    function GetPlayer: TPlayer;
    function GetResourceType: TStarcraftResource;
    function GetScoreType: TStarcraftScore;
    function GetSwitch: integer;
    function GetUnitPropUsed: boolean;
    function GetUnitType: TStarcraftUnit;
    function GetUnitTypeUsed: boolean;
    procedure SetAlwaysDisplay(AValue: boolean);
    procedure SetEnabled(AValue: boolean);
    procedure SetExpectedSwitchValue(AValue: boolean);
    procedure SetIntegerComparison(AValue: TIntegerConditionMode);
    procedure SetLocationBase0(AValue: Integer);
    procedure SetPlayer(AValue: TPlayer);
    procedure SetResourceType(AValue: TStarcraftResource);
    procedure SetScoreType(AValue: TStarcraftScore);
    procedure SetSwitch(AValue: integer);
    procedure SetUnitPropUsed(AValue: boolean);
    procedure SetUnitType(AValue: TStarcraftUnit);
    procedure SetUnitTypeUsed(AValue: boolean);
  public
    LocationBase1, PlayerBase0, Quantity: LongWord;
    UnitTypeW: Word;
    Comparison, ConditionType, NonUnitVariable: byte;
    Flags: bitpacked array[0..7] of boolean;
    Reserved: Word;
    property HasLocation: boolean read GetHasLocation;
    property LocationBase0: Integer read GetLocationBase0 write SetLocationBase0;
    property Player: TPlayer read GetPlayer write SetPlayer;
    property UnitType: TStarcraftUnit read GetUnitType write SetUnitType;
    property IntegerComparison: TIntegerConditionMode read GetIntegerComparison write SetIntegerComparison;
    property ExpectedSwitchValue: boolean read GetExpectedSwitchValue write SetExpectedSwitchValue;
    property ScoreType: TStarcraftScore read GetScoreType write SetScoreType;
    property ResourceType: TStarcraftResource read GetResourceType write SetResourceType;
    property Switch: integer read GetSwitch write SetSwitch;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property AlwaysDisplay: boolean read GetAlwaysDisplay write SetAlwaysDisplay;
    property UnitPropUsed: boolean read GetUnitPropUsed write SetUnitPropUsed;
    property UnitTypeUsed: boolean read GetUnitTypeUsed write SetUnitTypeUsed;
  end;

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

  { TTriggerData }

  TTriggerData = object
  private
    function GetAllConditionsMet: boolean;
    function GetHasPausedGame: boolean;
    function GetIgnore: boolean;
    function GetIgnoreDefeatDrawActions: boolean;
    function GetIgnoreOnceWaitAndOtherActions: boolean;
    function GetPlayers: TPlayers;
    function GetPreserveTrigger: boolean;
    function GetWaitSkippingDisabledOnce: boolean;
    procedure SetAllConditionsMet(AValue: boolean);
    procedure SetHasPausedGame(AValue: boolean);
    procedure SetIgnore(AValue: boolean);
    procedure SetIgnoreDefeatDrawActions(AValue: boolean);
    procedure SetIgnoreOnceWaitAndOtherActions(AValue: boolean);
    procedure SetPlayers(AValue: TPlayers);
    procedure SetPreserveTrigger(AValue: boolean);
    procedure SetWaitSkippingDisabledOnce(AValue: boolean);
  public
    Conditions: array[0..15] of TTriggerConditionData;
    Actions: array[0..63] of TTriggerInstructionData;
    Flags: bitpacked array[0..7] of Boolean;
    Reserved: array[1..3] of byte;
    PlayerFlags: array[0..27] of byte;
    procedure Clear;
    property AllConditionsMet: boolean read GetAllConditionsMet write SetAllConditionsMet;
    property IgnoreDefeatDrawActions: boolean read GetIgnoreDefeatDrawActions write SetIgnoreDefeatDrawActions;
    property PreserveTrigger: boolean read GetPreserveTrigger write SetPreserveTrigger;
    property Ignore: boolean read GetIgnore write SetIgnore;
    property IgnoreOnceWaitAndOtherActions: boolean read GetIgnoreOnceWaitAndOtherActions write SetIgnoreOnceWaitAndOtherActions;
    property HasPausedGame: boolean read GetHasPausedGame write SetHasPausedGame;
    property WaitSkippingDisabledOnce: boolean read GetWaitSkippingDisabledOnce write SetWaitSkippingDisabledOnce;
    property Players: TPlayers read GetPlayers write SetPlayers;
  end;

implementation

{ TTriggerData }

function TTriggerData.GetAllConditionsMet: boolean;
begin
  result := Flags[0];
end;

function TTriggerData.GetHasPausedGame: boolean;
begin
  result := Flags[5];
end;

function TTriggerData.GetIgnore: boolean;
begin
  result := Flags[3];
end;

function TTriggerData.GetIgnoreDefeatDrawActions: boolean;
begin
  result := Flags[1];
end;

function TTriggerData.GetIgnoreOnceWaitAndOtherActions: boolean;
begin
  result := Flags[4];
end;

function TTriggerData.GetPlayers: TPlayers;
var
  pl: TPlayer;
begin
  result := [];
  for pl := plPlayer1 to high(TPlayer) do
    if PlayerFlags[ord(pl)-ord(plPlayer1)] = 1 then
      Include(result, pl);
end;

function TTriggerData.GetPreserveTrigger: boolean;
begin
  result := Flags[2];
end;

function TTriggerData.GetWaitSkippingDisabledOnce: boolean;
begin
  result := Flags[6];
end;

procedure TTriggerData.SetAllConditionsMet(AValue: boolean);
begin
  Flags[0] := AValue;
end;

procedure TTriggerData.SetHasPausedGame(AValue: boolean);
begin
  Flags[5] := AValue;
end;

procedure TTriggerData.SetIgnore(AValue: boolean);
begin
  Flags[3] := AValue;
end;

procedure TTriggerData.SetIgnoreDefeatDrawActions(AValue: boolean);
begin
  Flags[1] := AValue;
end;

procedure TTriggerData.SetIgnoreOnceWaitAndOtherActions(AValue: boolean);
begin
  Flags[4] := AValue;
end;

procedure TTriggerData.SetPlayers(AValue: TPlayers);
var
  pl: TPlayer;
begin
  for pl := plPlayer1 to high(TPlayer) do
    if pl in AValue then
      PlayerFlags[ord(pl)-ord(plPlayer1)] := 1
    else
      PlayerFlags[ord(pl)-ord(plPlayer1)] := 0;
end;

procedure TTriggerData.SetPreserveTrigger(AValue: boolean);
begin
  Flags[2] := AValue;
end;

procedure TTriggerData.SetWaitSkippingDisabledOnce(AValue: boolean);
begin
  Flags[6] := AValue;
end;

procedure TTriggerData.Clear;
begin
  fillchar(self, sizeof(self), 0);
end;

{ TTriggerConditionData }

function TTriggerConditionData.GetAlwaysDisplay: boolean;
begin
  result := Flags[2];
end;

function TTriggerConditionData.GetEnabled: boolean;
begin
  result := not Flags[1];
end;

function TTriggerConditionData.GetExpectedSwitchValue: boolean;
begin
  case Comparison of
  2: result := true;
  3: result := false;
  else raise exception.Create('Not an expected boolean value');
  end;
end;

function TTriggerConditionData.GetHasLocation: boolean;
begin
  result := LocationBase1 <> 0;
end;

function TTriggerConditionData.GetIntegerComparison: TIntegerConditionMode;
begin
  case Comparison of
  0: result := icmAtLeast;
  1: result := icmAtMost;
  10: result := icmExactly;
  else raise exception.Create('Not an integer comparison');
  end;
end;

function TTriggerConditionData.GetLocationBase0: Integer;
begin
  result := LocationBase1-1;
end;

function TTriggerConditionData.GetPlayer: TPlayer;
begin
  result := TPlayer(PlayerBase0+ord(plPlayer1));
end;

function TTriggerConditionData.GetResourceType: TStarcraftResource;
begin
  if NonUnitVariable <= ord(high(TStarcraftResource)) then
    result := TStarcraftResource(NonUnitVariable)
  else
    raise exception.Create('Value out of bounds');
end;

function TTriggerConditionData.GetScoreType: TStarcraftScore;
begin
  if NonUnitVariable <= ord(high(TStarcraftScore)) then
    result := TStarcraftScore(NonUnitVariable)
  else
    raise exception.Create('Value out of bounds');
end;

function TTriggerConditionData.GetSwitch: integer;
begin
  result := NonUnitVariable+1;
end;

function TTriggerConditionData.GetUnitPropUsed: boolean;
begin
  result := Flags[3];
end;

function TTriggerConditionData.GetUnitType: TStarcraftUnit;
begin
  result := TStarcraftUnit(UnitTypeW);
end;

function TTriggerConditionData.GetUnitTypeUsed: boolean;
begin
  result := Flags[4];
end;

procedure TTriggerConditionData.SetAlwaysDisplay(AValue: boolean);
begin
  Flags[2] := AValue;
end;

procedure TTriggerConditionData.SetEnabled(AValue: boolean);
begin
  Flags[1] := not AValue;
end;

procedure TTriggerConditionData.SetExpectedSwitchValue(AValue: boolean);
begin
  if AValue then Comparison := 2 else Comparison := 3;
end;

procedure TTriggerConditionData.SetIntegerComparison(
  AValue: TIntegerConditionMode);
begin
  case AValue of
  icmAtLeast: Comparison := 0;
  icmAtMost: Comparison:= 1;
  icmExactly: Comparison:= 10;
  else raise exception.Create('Case not handled');
  end;
end;

procedure TTriggerConditionData.SetLocationBase0(AValue: Integer);
begin
  LocationBase1 := AValue+1;
end;

procedure TTriggerConditionData.SetPlayer(AValue: TPlayer);
begin
  PlayerBase0 := Ord(AValue)-Ord(plPlayer1);
end;

procedure TTriggerConditionData.SetResourceType(AValue: TStarcraftResource);
begin
  NonUnitVariable:= ord(AValue);
end;

procedure TTriggerConditionData.SetScoreType(AValue: TStarcraftScore);
begin
  NonUnitVariable:= ord(AValue);
end;

procedure TTriggerConditionData.SetSwitch(AValue: integer);
begin
  if (AValue < 1) or (AValue > 256) then
    raise exception.Create('Value out of bounds');
  NonUnitVariable:= AValue-1;
end;

procedure TTriggerConditionData.SetUnitPropUsed(AValue: boolean);
begin
  Flags[3] := AValue;
end;

procedure TTriggerConditionData.SetUnitType(AValue: TStarcraftUnit);
begin
  UnitTypeW := Ord(AValue);
  UnitTypeUsed := true;
end;

procedure TTriggerConditionData.SetUnitTypeUsed(AValue: boolean);
begin
  Flags[4] := AValue;
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

end.

