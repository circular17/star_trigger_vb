unit utriggerconditions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, usctypes;

type

  { TTriggerCondition }

  TTriggerCondition = class(TCondition)
    function ToTrigEditAndFree: string;
    function ToTrigEdit: string; virtual; abstract;
  end;

  { TAlwaysCondition }

  TAlwaysCondition = class(TTriggerCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: string); override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TNeverCondition }

  TNeverCondition = class(TTriggerCondition)
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TSwitchCondition }

  TSwitchCondition = class(TTriggerCondition)
    Switch: integer;
    Value: boolean;
    constructor Create(ASwitch: integer; AValue: boolean);
    function ToTrigEdit: string; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: string); override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TBringCondition }

  TBringCondition = class(TTriggerCondition)
    Player: TPlayer;
    UnitType, Location: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType, ALocation: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TKillCountCondition }

  TKillCountCondition = class(TTriggerCondition)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TOpponentCountCondition }

  TOpponentCountCondition = class(TTriggerCondition)
    Player: TPlayer;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TElapsedTimeCondition }

  TElapsedTimeCondition = class(TTriggerCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TCompareUnitCountCondition }

  TCompareUnitCountCondition = class(TTriggerCondition)
    UnitType, Location: string;
    Highest: boolean;
    constructor Create(AUnitType, ALocation: string; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TCompareKillCountCondition }

  TCompareKillCountCondition = class(TTriggerCondition)
    UnitType: string;
    Highest: boolean;
    constructor Create(AUnitType: string; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TDeathCountCondition }

  TDeathCountCondition = class(TTriggerCondition)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
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
  end;

  { TCountdownCondition }

  TCountdownCondition = class(TTriggerCondition)
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(AMode: TIntegerConditionMode; AValue: integer);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TCompareResourceCondition }

  TCompareResourceCondition = class(TTriggerCondition)
    Resource: TStarcraftResource;
    Highest: boolean;
    constructor Create(AResource: TStarcraftResource; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

  { TCompareScoreCondition }

  TCompareScoreCondition = class(TTriggerCondition)
    Score: TStarcraftScore;
    Highest: boolean;
    constructor Create(AScore: TStarcraftScore; AHighest: boolean);
    function ToTrigEdit: string; override;
    function Duplicate: TTriggerCondition; override;
  end;

const
  IntegerConditionModeToTrigEditStr: array[TIntegerConditionMode] of string =
    ('At least', 'At most','Exactly');

function CreateCompareIntegerCondition(AUnitType: string; AHighest: boolean): TCondition;

implementation

uses utriggerinstructions;

function CreateIntegerConditionImplementation(APlayer: TPlayer; AUnitType: string;
  AMode: TIntegerConditionMode; AValue: integer): TCondition;
begin
  if CompareText(AUnitType,'Ore')=0 then
    result := TResourceCondition.Create(APlayer,srOre,AMode,AValue) else
  if CompareText(AUnitType,'Gas')=0 then
    result := TResourceCondition.Create(APlayer,srGas,AMode,AValue) else
  if CompareText(AUnitType,'Ore And Gas')=0 then
    result := TResourceCondition.Create(APlayer,srOreAndGas,AMode,AValue) else
  if CompareText(copy(AUnitType,length(AUnitType)-5,6),' Score')=0 then
  begin
    if compareText(AUnitType, 'Units Score')=0 then
      result := TScoreCondition.Create(APlayer,ssUnitScore,AMode,AValue) else
    if compareText(AUnitType, 'Buildings Score')=0 then
      result := TScoreCondition.Create(APlayer,ssBuildingScore,AMode,AValue) else
    if compareText(AUnitType, 'Units and buildings Score')=0 then
      result := TScoreCondition.Create(APlayer,ssUnitAndBuildingScore,AMode,AValue) else
    if compareText(AUnitType, 'Kills Score')=0 then
      result := TScoreCondition.Create(APlayer,ssKillScore,AMode,AValue) else
    if compareText(AUnitType, 'Razings Score')=0 then
      result := TScoreCondition.Create(APlayer,ssRazingScore,AMode,AValue) else
    if compareText(AUnitType, 'Kills and razings Score')=0 then
      result := TScoreCondition.Create(APlayer,ssKillAndRazingScore,AMode,AValue) else
    if compareText(AUnitType, 'Custom Score')=0 then
      result := TScoreCondition.Create(APlayer,ssCustomScore,AMode,AValue) else
    if compareText(AUnitType, 'Total Score')=0 then
      result := TScoreCondition.Create(APlayer,ssTotalScore,AMode,AValue) else
        raise exception.Create('Unknown score type "'+AUnitType+'"');
  end
  else
  if CompareText(AUnitType,'Countdown')= 0 then
    result := TCountdownCondition.Create(AMode,AValue)
  else
    Result:= TDeathCountCondition.Create(APlayer, AUnitType, AMode, AValue);
end;

function CreateCompareIntegerCondition(AUnitType: string;
  AHighest: boolean): TCondition;
begin
  if CompareText(AUnitType,'Ore')=0 then
    result := TCompareResourceCondition.Create(srOre,AHighest) else
  if CompareText(AUnitType,'Gas')=0 then
    result := TCompareResourceCondition.Create(srGas,AHighest) else
  if CompareText(AUnitType,'Ore And Gas')=0 then
    result := TCompareResourceCondition.Create(srOreAndGas,AHighest) else
  if CompareText(copy(AUnitType,length(AUnitType)-5,6),' Score')=0 then
  begin
    if compareText(AUnitType, 'Units Score')=0 then
      result := TCompareScoreCondition.Create(ssUnitScore,AHighest) else
    if compareText(AUnitType, 'Buildings Score')=0 then
      result := TCompareScoreCondition.Create(ssBuildingScore,AHighest) else
    if compareText(AUnitType, 'Units and buildings Score')=0 then
      result := TCompareScoreCondition.Create(ssUnitAndBuildingScore,AHighest) else
    if compareText(AUnitType, 'Kills Score')=0 then
      result := TCompareScoreCondition.Create(ssKillScore,AHighest) else
    if compareText(AUnitType, 'Razings Score')=0 then
      result := TCompareScoreCondition.Create(ssRazingScore,AHighest) else
    if compareText(AUnitType, 'Kills and razings Score')=0 then
      result := TCompareScoreCondition.Create(ssKillAndRazingScore,AHighest) else
    if compareText(AUnitType, 'Custom Score')=0 then
      result := TCompareScoreCondition.Create(ssCustomScore,AHighest) else
    if compareText(AUnitType, 'Total Score')=0 then
      result := TCompareScoreCondition.Create(ssTotalScore,AHighest) else
        raise exception.Create('Unknown score type "'+AUnitType+'"');
  end
  else
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

{ TDeathCountCondition }

constructor TDeathCountCondition.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TIntegerConditionMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TDeathCountCondition.ToTrigEdit: string;
begin
  Result:= 'Deaths("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' + IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TDeathCountCondition.Duplicate: TTriggerCondition;
begin
  result := TDeathCountCondition.Create(Player,UnitType,Mode,Value);
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

function TAlwaysCondition.Duplicate: TTriggerCondition;
begin
  result := TAlwaysCondition.Create;
end;

{ TNeverCondition }

function TNeverCondition.ToTrigEdit: string;
begin
  Result:= 'Never()';
end;

procedure TNeverCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: string);
begin
  AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
end;

function TNeverCondition.Duplicate: TTriggerCondition;
begin
  result := TNeverCondition.Create;
end;

{ TSwitchCondition }

constructor TSwitchCondition.Create(ASwitch: integer; AValue: boolean);
begin
  Switch := ASwitch;
  Value := AValue;
end;

function TSwitchCondition.ToTrigEdit: string;
begin
  Result:= 'Switch('+AddTrigEditQuotes(SwitchToStr(Switch))+ ', ' + BoolToStr(Value, 'set', 'not set') + ')';
end;

procedure TSwitchCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: string);
begin
  AProg.Add(TFastIfInstruction.Create([TSwitchCondition.Create(Switch,not Value)], [CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0)]));
end;

function TSwitchCondition.Duplicate: TTriggerCondition;
begin
  result := TSwitchCondition.Create(Switch,Value);
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
    Result:= 'Command("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Bring("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' + AddTrigEditQuotes(Location) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TBringCondition.Duplicate: TTriggerCondition;
begin
  result := TBringCondition.Create(Player,UnitType,Location,Mode,Value);
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
  Result:= 'Kill("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' +
       IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TKillCountCondition.Duplicate: TTriggerCondition;
begin
  result := TKillCountCondition.Create(Player,UnitType,Mode,Value);
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
  if not IsAnywhere(Location) then result += ' At(' + AddTrigEditQuotes(UnitType) + ', ' +AddTrigEditQuotes(Location)+')'
  else result += '(' + AddTrigEditQuotes(UnitType) + ')';
end;

function TCompareUnitCountCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareUnitCountCondition.Create(UnitType,Location,Highest);
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
  result += '(' + AddTrigEditQuotes(UnitType) + ')';
end;

function TCompareKillCountCondition.Duplicate: TTriggerCondition;
begin
  result := TCompareKillCountCondition.Create(UnitType,Highest);
end;

initialization

  CreateIntegerCondition := @CreateIntegerConditionImplementation;

end.

