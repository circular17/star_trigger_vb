unit utriggerconditions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes, utriggerchunk;

type
  { TTriggerCondition }

  TTriggerCondition = class(TCondition)
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual; abstract;
    procedure WriteTriggerData(var AData: TTriggerConditionData); virtual; abstract;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; virtual;
  end;

  { TAlwaysCondition }

  TAlwaysCondition = class(TTriggerCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: TStarcraftUnit); override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
  end;

  { TNeverCondition }

  TNeverCondition = class(TTriggerCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit); override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
  end;

  { TElapsedTimeCondition }

  TElapsedTimeCondition = class(TTriggerCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
  end;

  { TCompareKillCountCondition }

  TCompareKillCountCondition = class(TTriggerCondition)
    UnitType: TStarcraftUnit;
    Highest: boolean;
    constructor Create(AUnitType: TStarcraftUnit; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
  end;

  { TCountdownCondition }

  TCountdownCondition = class(TTriggerCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
  end;

  { TCompareResourceCondition }

  TCompareResourceCondition = class(TTriggerCondition)
    Resource: TStarcraftResource;
    Highest: boolean;
    constructor Create(AResource: TStarcraftResource; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
  end;

  { TCompareScoreCondition }

  TCompareScoreCondition = class(TTriggerCondition)
    Score: TStarcraftScore;
    Highest: boolean;
    constructor Create(AScore: TStarcraftScore; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
    procedure WriteTriggerData(var AData: TTriggerConditionData); override;
    class function LoadFromData(const AData: TTriggerConditionData): TTriggerCondition; override;
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

class function TTriggerCondition.LoadFromData(const AData: TTriggerConditionData
  ): TTriggerCondition;
begin
  result := TAlwaysCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TNeverCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TSwitchCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TBringCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TKillCountCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TOpponentCountCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TElapsedTimeCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCompareUnitCountCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCompareKillCountCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TDeathCountCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TResourceCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TScoreCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCountdownCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCompareResourceCondition.LoadFromData(AData);
  if Assigned(result) then exit;
  result := TCompareScoreCondition.LoadFromData(AData);
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

class function TScoreCondition.LoadFromData(const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctScore then
    result := TScoreCondition.Create(AData.Player, AData.ScoreType, AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
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

class function TResourceCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctAccumulate then
    result := TResourceCondition.Create(AData.Player, AData.ResourceType, AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
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

class function TCountdownCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctCountdown then
    result := TCountdownCondition.Create(AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
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

class function TDeathCountCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctDeathCount then
    result := TDeathCountCondition.Create(AData.Player, AData.UnitType, AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
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

class function TCompareScoreCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  case AData.ConditionType of
  ctHighestScore: result := TCompareScoreCondition.Create(AData.ScoreType, true);
  ctLowestScore: result := TCompareScoreCondition.Create(AData.ScoreType, false);
  else result := nil;
  end;
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

class function TCompareResourceCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  case AData.ConditionType of
  ctMostResources: result := TCompareResourceCondition.Create(AData.ResourceType,true);
  ctLeastResources: result := TCompareResourceCondition.Create(AData.ResourceType,false);
  else result := nil;
  end;
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

class function TAlwaysCondition.LoadFromData(const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctAlways then
    result := TAlwaysCondition.Create
  else
    result := nil;
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

class function TNeverCondition.LoadFromData(const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctNever then
    result := TNeverCondition.Create
  else
    result := nil;
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

class function TSwitchCondition.LoadFromData(const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctSwitch then
    result := TSwitchCondition.Create(AData.Switch, AData.ExpectedSwitchValue)
  else
    result := nil;
end;

{ TBringCondition }

constructor TBringCondition.Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ALocation: string;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
  Location:= ALocation;
end;

function TBringCondition.ToTrigEdit: string;
begin
  if MapInfo.IsAnywhere(Location) then
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
  if MapInfo.IsAnywhere(Location) then
    AData.ConditionType:= ctCommand
  else
  begin
    AData.ConditionType:= ctBring;
    AData.LocationBase0:= MapInfo.LocationIndexOf(Location);
  end;
  AData.Player := Player;
  AData.UnitType:= UnitType;
  AData.IntegerComparison:= Mode;
  AData.Quantity := Value;
end;

class function TBringCondition.LoadFromData(const AData: TTriggerConditionData): TTriggerCondition;
var
  loc: String;
begin
  if AData.ConditionType in[ctCommand,ctBring] then
  begin
    if AData.ConditionType = ctBring then
      loc := MapInfo.LocationName[AData.LocationBase0]
    else
      loc := MapInfo.AnywhereLocationName;

    result := TBringCondition.Create(AData.Player, AData.UnitType, loc, AData.IntegerComparison, AData.Quantity);
  end
  else result := nil;
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

class function TKillCountCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctKillCount then
    result := TKillCountCondition.Create(AData.Player, AData.UnitType, AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
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

class function TOpponentCountCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctOpponentCount then
    result := TOpponentCountCondition.Create(AData.Player, AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
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

class function TElapsedTimeCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  if AData.ConditionType = ctElapsedTime then
    result := TElapsedTimeCondition.Create(AData.IntegerComparison, AData.Quantity)
  else
    result := nil;
end;

{ TCompareUnitCountCondition }

constructor TCompareUnitCountCondition.Create(AUnitType: TStarcraftUnit;
  ALocation: string; AHighest: boolean);
begin
  if MapInfo.StrictLocations and (MapInfo.LocationIndexOf(ALocation)=-1) then raise exception.Create('Location not found');
  UnitType := AUnitType;
  Location:= ALocation;
  Highest:= AHighest;
end;

function TCompareUnitCountCondition.ToTrigEdit: string;
begin
  Result:= 'Command ';
  if Highest then result += 'the Most' else result += 'the Least';
  if not MapInfo.IsAnywhere(Location) then result += ' At(' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ', ' +AddTrigEditQuotes(Location)+')'
  else result += '(' + AddTrigEditQuotes(StarcraftUnitTrigEditNames[UnitType]) + ')';
end;

function TCompareUnitCountCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareUnitCountCondition.Create(UnitType,Location,Highest);
end;

procedure TCompareUnitCountCondition.WriteTriggerData(
  var AData: TTriggerConditionData);
begin
  if MapInfo.IsAnywhere(Location) then
  begin
    if Highest then AData.ConditionType:= ctCommandTheMost
    else AData.ConditionType:= ctCommandTheLeast;
  end else
  begin
    if Highest then AData.ConditionType:= ctCommandTheMostAt
    else AData.ConditionType:= ctCommandTheLeastAt;
    AData.LocationBase0 := MapInfo.LocationIndexOf(Location);
  end;
  AData.UnitType:= UnitType;
end;

class function TCompareUnitCountCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  case AData.ConditionType of
  ctCommandTheMost: result := TCompareUnitCountCondition.Create(AData.UnitType, MapInfo.AnywhereLocationName, true);
  ctCommandTheLeast: result := TCompareUnitCountCondition.Create(AData.UnitType, MapInfo.AnywhereLocationName, false);
  ctCommandTheMostAt: result := TCompareUnitCountCondition.Create(AData.UnitType, MapInfo.LocationName[AData.LocationBase0], true);
  ctCommandTheLeastAt: result := TCompareUnitCountCondition.Create(AData.UnitType, MapInfo.LocationName[AData.LocationBase0], false);
  else result := nil;
  end;
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

class function TCompareKillCountCondition.LoadFromData(
  const AData: TTriggerConditionData): TTriggerCondition;
begin
  case AData.ConditionType of
  ctMostKills: result := TCompareKillCountCondition.Create(AData.UnitType, true);
  ctLeastKills: result := TCompareKillCountCondition.Create(AData.UnitType, false);
  else result := nil;
  end;
end;

initialization

  CreateIntegerCondition := @CreateIntegerConditionImplementation;

end.

