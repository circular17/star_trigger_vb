unit utriggerconditions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes;

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

  { TTriggerCondition }

  TTriggerCondition = class(TCondition)
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual; abstract;
    procedure WriteTriggerData(var AData: TTriggerConditionData); virtual; abstract;
  end;

  { TAlwaysCondition }

  TAlwaysCondition = class(TTriggerCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: TStarcraftUnit); override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TNeverCondition }

  TNeverCondition = class(TTriggerCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit); override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TSwitchCondition }

  TSwitchCondition = class(TTriggerCondition)
    Switch: integer;
    Value: boolean;
    constructor Create(ASwitch: integer; AValue: boolean);
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit); override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TBringCondition }

  TBringCondition = class(TTriggerCondition)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Location: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TKillCountCondition }

  TKillCountCondition = class(TTriggerCondition)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TOpponentCountCondition }

  TOpponentCountCondition = class(TTriggerCondition)
    Player: TPlayer;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TElapsedTimeCondition }

  TElapsedTimeCondition = class(TTriggerCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TCompareUnitCountCondition }

  TCompareUnitCountCondition = class(TTriggerCondition)
    UnitType: TStarcraftUnit;
    Location: string;
    Highest: boolean;
    constructor Create(AUnitType: TStarcraftUnit; ALocation: string; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TCompareKillCountCondition }

  TCompareKillCountCondition = class(TTriggerCondition)
    UnitType: TStarcraftUnit;
    Highest: boolean;
    constructor Create(AUnitType: TStarcraftUnit; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TDeathCountCondition }

  TDeathCountCondition = class(TTriggerCondition)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TResourceCondition }

  TResourceCondition = class(TTriggerCondition)
    Player: TPlayer;
    Resource: TStarcraftResource;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AResource: TStarcraftResource; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TScoreCondition }

  TScoreCondition = class(TTriggerCondition)
    Player: TPlayer;
    Score: TStarcraftScore;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AScore: TStarcraftScore; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TCountdownCondition }

  TCountdownCondition = class(TTriggerCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TCompareResourceCondition }

  TCompareResourceCondition = class(TTriggerCondition)
    Resource: TStarcraftResource;
    Highest: boolean;
    constructor Create(AResource: TStarcraftResource; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

  { TCompareScoreCondition }

  TCompareScoreCondition = class(TTriggerCondition)
    Score: TStarcraftScore;
    Highest: boolean;
    constructor Create(AScore: TStarcraftScore; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
  end;

function CreateCompareIntegerCondition(AUnitType: TStarcraftUnit; AHighest: boolean): TCondition;

implementation

uses utriggerinstructions, utrigedittypes, umapinfo;

function CreateIntegerConditionImplementation(APlayer: TPlayer; AUnitType: TStarcraftUnit;
  AMode: TIntegerConditionMode; AValue: integer): TCondition;
begin
  if AUnitType = suResourceOre then
    result := TResourceCondition.Create(APlayer,srOre,AMode,AValue) else
  if AUnitType = suResourceGas then
    result := TResourceCondition.Create(APlayer,srGas,AMode,AValue) else
  if AUnitType = suResourceOreAndGas then
    result := TResourceCondition.Create(APlayer,srOreAndGas,AMode,AValue) else
  if AUnitType = suScoreUnits then
    result := TScoreCondition.Create(APlayer,ssUnitScore,AMode,AValue) else
  if AUnitType = suScoreBuildings then
    result := TScoreCondition.Create(APlayer,ssBuildingScore,AMode,AValue) else
  if AUnitType = suScoreUnitsAndBuildings then
    result := TScoreCondition.Create(APlayer,ssUnitAndBuildingScore,AMode,AValue) else
  if AUnitType = suScoreKills then
    result := TScoreCondition.Create(APlayer,ssKillScore,AMode,AValue) else
  if AUnitType = suScoreRazings then
    result := TScoreCondition.Create(APlayer,ssRazingScore,AMode,AValue) else
  if AUnitType = suScoreKillsAndRazings then
    result := TScoreCondition.Create(APlayer,ssKillAndRazingScore,AMode,AValue) else
  if AUnitType = suScoreCustom then
    result := TScoreCondition.Create(APlayer,ssCustomScore,AMode,AValue) else
  if AUnitType = suScoreTotal then
    result := TScoreCondition.Create(APlayer,ssTotalScore,AMode,AValue) else
  if AUnitType = suCountdown then
    result := TCountdownCondition.Create(AMode,AValue)
  else
    Result:= TDeathCountCondition.Create(APlayer, AUnitType, AMode, AValue);
end;

function CreateCompareIntegerCondition(AUnitType: TStarcraftUnit;
  AHighest: boolean): TCondition;
begin
  if AUnitType = suResourceOre then
    result := TCompareResourceCondition.Create(srOre,AHighest) else
  if AUnitType = suResourceGas then
    result := TCompareResourceCondition.Create(srGas,AHighest) else
  if AUnitType = suResourceOreAndGas then
    result := TCompareResourceCondition.Create(srOreAndGas,AHighest) else
  if AUnitType = suScoreUnits then
    result := TCompareScoreCondition.Create(ssUnitScore,AHighest) else
  if AUnitType = suScoreBuildings then
    result := TCompareScoreCondition.Create(ssBuildingScore,AHighest) else
  if AUnitType = suScoreUnitsAndBuildings then
    result := TCompareScoreCondition.Create(ssUnitAndBuildingScore,AHighest) else
  if AUnitType = suScoreKills then
    result := TCompareScoreCondition.Create(ssKillScore,AHighest) else
  if AUnitType = suScoreRazings then
    result := TCompareScoreCondition.Create(ssRazingScore,AHighest) else
  if AUnitType = suScoreKillsAndRazings then
    result := TCompareScoreCondition.Create(ssKillAndRazingScore,AHighest) else
  if AUnitType = suScoreCustom then
    result := TCompareScoreCondition.Create(ssCustomScore,AHighest) else
  if AUnitType = suScoreTotal then
    result := TCompareScoreCondition.Create(ssTotalScore,AHighest) else
    raise exception.Create('This variable cannot be compared between players');
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

{ TTriggerCondition }

function TTriggerCondition.ToTrigEditAndFree: string;
begin
  if self <> nil then
  begin
    result := ToTrigEdit;
    Free;
  end else
    result := '';
end;

{ TScoreCondition }

constructor TScoreCondition.Create(APlayer: TPlayer; AScore: TStarcraftScore;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  Score := AScore;
  Mode := AMode;
  Value := AValue;
end;

function TScoreCondition.ToTrigEdit: string;
begin
  result := 'Score("' + PlayerToTrigEditStr(Player) + '", ' + StarcraftScoreToStr(Score) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode]+ ', ' + IntToStr(Value) + ')'
end;

function TScoreCondition.Duplicate: TTriggerCondition;
begin
  result := TScoreCondition.Create(Player,Score,Mode,Value);
end;

procedure TScoreCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctScore;
  AData.Player:= Player;
  AData.ScoreType:= Score;
  AData.IntegerComparison := Mode;
  Adata.Quantity := Value;
end;

{ TResourceCondition }

constructor TResourceCondition.Create(APlayer: TPlayer;
  AResource: TStarcraftResource; AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  Resource := AResource;
  Mode := AMode;
  Value := AValue;
end;

function TResourceCondition.ToTrigEdit: string;
begin
  Result:= 'Accumulate("' + PlayerToTrigEditStr(Player) + '", ' + IntegerConditionModeToTrigEditStr[Mode] + ', ' +
          IntToStr(Value) + ', ' + StarcraftResourceToStr(Resource) + ')'
end;

function TResourceCondition.Duplicate: TTriggerCondition;
begin
  result := TResourceCondition.Create(Player,Resource,Mode,Value);
end;

procedure TResourceCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctAccumulate;
  AData.Player:= Player;
  AData.ResourceType:= Resource;
  AData.IntegerComparison := Mode;
  Adata.Quantity := Value;
end;

{ TCountdownCondition }

constructor TCountdownCondition.Create(AMode: TIntegerConditionMode;
  AValue: integer);
begin
  Mode := AMode;
  Value := AValue;
end;

function TCountdownCondition.ToTrigEdit: string;
begin
  result := 'Countdown Timer(' + IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')'
end;

function TCountdownCondition.Duplicate: TTriggerCondition;
begin
  result := TCountdownCondition.Create(Mode,Value);
end;

procedure TCountdownCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctCountdown;
  AData.IntegerComparison:= Mode;
  AData.Quantity:= Value;
end;

{ TDeathCountCondition }

constructor TDeathCountCondition.Create(APlayer: TPlayer; AUnitType: TStarcraftUnit;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TDeathCountCondition.ToTrigEdit: string;
begin
  Result:= 'Deaths("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' + IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TDeathCountCondition.Duplicate: TTriggerCondition;
begin
  result := TDeathCountCondition.Create(Player,UnitType,Mode,Value);
end;

procedure TDeathCountCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctDeathCount;
  AData.Player:= Player;
  AData.UnitType:= UnitType;
  AData.IntegerComparison:= Mode;
  AData.Quantity:= Value;
end;

{ TCompareScoreCondition }

constructor TCompareScoreCondition.Create(AScore: TStarcraftScore;
  AHighest: boolean);
begin
  Score := AScore;
  Highest:= AHighest;
end;

function TCompareScoreCondition.ToTrigEdit: string;
begin
  if Highest then result := 'Highest' else result := 'Lowest';
  result += ' Score(' + StarcraftScoreToStr(Score) + ')'
end;

function TCompareScoreCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareScoreCondition.Create(Score,Highest);
end;

procedure TCompareScoreCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  if Highest then AData.ConditionType:= ctHighestScore
  else AData.ConditionType:= ctLowestScore;
  AData.ScoreType:= Score;
end;

{ TCompareResourceCondition }

constructor TCompareResourceCondition.Create(AResource: TStarcraftResource;
  AHighest: boolean);
begin
  Resource:= AResource;
  Highest:= AHighest;
end;

function TCompareResourceCondition.ToTrigEdit: string;
begin
  if Highest then result := 'Most' else result := 'Least';
  result += ' Resources(' + StarcraftResourceToStr(Resource) + ')'
end;

function TCompareResourceCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareResourceCondition.Create(Resource,Highest);
end;

procedure TCompareResourceCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  if Highest then AData.ConditionType:= ctMostResources
  else AData.ConditionType:= ctLeastResources;
  AData.ResourceType:= Resource;
end;

{ TAlwaysCondition }

function TAlwaysCondition.ToTrigEdit: string;
begin
  Result:= 'Always()';
end;

procedure TAlwaysCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
begin
  //nothing
end;

function TAlwaysCondition.Duplicate: TTriggerCondition;
begin
  result := TAlwaysCondition.Create;
end;

procedure TAlwaysCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctAlways;
end;

{ TNeverCondition }

function TNeverCondition.ToTrigEdit: string;
begin
  Result:= 'Never()';
end;

procedure TNeverCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
begin
  AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
end;

function TNeverCondition.Duplicate: TTriggerCondition;
begin
  result := TNeverCondition.Create;
end;

procedure TNeverCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctNever;
end;

{ TSwitchCondition }

constructor TSwitchCondition.Create(ASwitch: integer; AValue: boolean);
begin
  Switch := ASwitch;
  Value := AValue;
end;

function TSwitchCondition.ToTrigEdit: string;
begin
  Result:= 'Switch('+SwitchToTrigEditCode(Switch)+ ', ' + BoolToStr(Value, 'set', 'not set') + ')';
end;

procedure TSwitchCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
begin
  AProg.Add(TFastIfInstruction.Create([TSwitchCondition.Create(Switch,not Value)], [CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0)]));
end;

function TSwitchCondition.Duplicate: TTriggerCondition;
begin
  result := TSwitchCondition.Create(Switch,Value);
end;

procedure TSwitchCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctSwitch;
  AData.Switch := Switch;
  AData.ExpectedSwitchValue:= Value;
end;

{ TBringCondition }

constructor TBringCondition.Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string;
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
    Result:= 'Command("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Bring("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' + AddTrigEditQuotes(Location) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TBringCondition.Duplicate: TTriggerCondition;
begin
  result := TBringCondition.Create(Player,UnitType,Location,Mode,Value);
end;

procedure TBringCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  if IsAnywhere(Location) then
    AData.ConditionType:= ctCommand
  else
  begin
    AData.ConditionType:= ctBring;
    AData.LocationBase0:= LocationIndexOf(Location);
  end;
  AData.Player := Player;
  AData.UnitType:= UnitType;
  AData.IntegerComparison:= Mode;
  AData.Quantity := Value;
end;

{ TKillCountCondition }

constructor TKillCountCondition.Create(APlayer: TPlayer; AUnitType: TStarcraftUnit;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TKillCountCondition.ToTrigEdit: string;
begin
  Result:= 'Kill("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' +
       IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TKillCountCondition.Duplicate: TTriggerCondition;
begin
  result := TKillCountCondition.Create(Player,UnitType,Mode,Value);
end;

procedure TKillCountCondition.WriteTriggerData(var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctKillCount;
  AData.Player := Player;
  AData.UnitType:= UnitType;
  AData.IntegerComparison:= Mode;
  AData.Quantity := Value;
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
  Result:= 'Opponents("' + PlayerToTrigEditStr(Player) + '", ' +
       IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TOpponentCountCondition.Duplicate: TTriggerCondition;
begin
  result := TOpponentCountCondition.Create(Player,Mode,Value);
end;

procedure TOpponentCountCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctOpponentCount;
  AData.Player := Player;
  AData.IntegerComparison:= Mode;
  AData.Quantity := Value;
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
  Result:= 'Elapsed Time(' + IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TElapsedTimeCondition.Duplicate: TTriggerCondition;
begin
  result := TElapsedTimeCondition.Create(Mode,Value);
end;

procedure TElapsedTimeCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  AData.ConditionType:= ctElapsedTime;
  AData.IntegerComparison:= Mode;
  AData.Quantity := Value;
end;

{ TCompareUnitCountCondition }

constructor TCompareUnitCountCondition.Create(AUnitType: TStarcraftUnit;
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
  if not IsAnywhere(Location) then result += ' At(' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' +AddTrigEditQuotes(Location)+')'
  else result += '(' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ')';
end;

function TCompareUnitCountCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareUnitCountCondition.Create(UnitType,Location,Highest);
end;

procedure TCompareUnitCountCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  if IsAnywhere(Location) then
  begin
    if Highest then AData.ConditionType:= ctCommandTheMost
    else AData.ConditionType:= ctCommandTheLeast;
  end else
  begin
    if Highest then AData.ConditionType:= ctCommandTheMostAt
    else AData.ConditionType:= ctCommandTheLeastAt;
    AData.LocationBase0 := LocationIndexOf(Location);
  end;
  AData.UnitType:= UnitType;
end;

{ TCompareKillCountCondition }

constructor TCompareKillCountCondition.Create(AUnitType: TStarcraftUnit;
  AHighest: boolean);
begin
  UnitType := AUnitType;
  Highest:= AHighest;
end;

function TCompareKillCountCondition.ToTrigEdit: string;
begin
  if Highest then result := 'Most' else result := 'Least';
  Result += ' Kills';
  result += '(' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ')';
end;

function TCompareKillCountCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareKillCountCondition.Create(UnitType,Highest);
end;

procedure TCompareKillCountCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  if Highest then AData.ConditionType:= ctMostKills
  else AData.ConditionType:= ctLeastKills;
  AData.UnitType:= UnitType;
end;

initialization

  CreateIntegerCondition := @CreateIntegerConditionImplementation;

end.

