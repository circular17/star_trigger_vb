unit uinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

var
  AnywhereLocation : string = 'Anywhere';
  Force : array[1..4] of string = ('Force 1', 'Force 2', 'Force 3', 'Force 4');
  SwitchNames : array[1..256] of string;

function AddTrigEditQuotes(AText: string): string;
function PlayerToTrigEditStr(APlayer: TPlayer): string;
function IsAnywhere(ALocation: string): boolean;
function SwitchToStr(ASwitch: integer): string;

type
  { TInstruction }

  TInstruction = class
  end;

  { TEmptyInstruction }

  TEmptyInstruction = class(TInstruction)
  end;

  TCustomInstructionList = specialize TFPGList<TInstruction>;

  { TInstructionList }

  TInstructionList = class(TCustomInstructionList)
    procedure FreeAll;
  end;

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
  { TSetIntegerInstruction }

  TSetIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer);
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
  IntegerConditionModeToTrigEditStr: array[TIntegerConditionMode] of string =
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

function AddTrigEditQuotes(AText: string): string;
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

function PlayerToTrigEditStr(APlayer: TPlayer): string;
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
  result += '(' + AddTrigEditQuotes(UnitType) + ')';
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
  Result:= 'Elapsed Time(' + IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
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
  Result:= 'Opponents("' + PlayerToTrigEditStr(Player) + '", ' +
       IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
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
  Result:= 'Kill("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' +
       IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
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
  if not IsAnywhere(Location) then result += ' At(' + AddTrigEditQuotes(UnitType) + ', ' +AddTrigEditQuotes(Location)+')'
  else result += '(' + AddTrigEditQuotes(UnitType) + ')';
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
    Result:= 'Command("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Bring("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' + AddTrigEditQuotes(Location) + ', ' +
         IntegerConditionModeToTrigEditStr[Mode] + ', ' + IntToStr(Value) + ')';
end;

function TBringCondition.Duplicate: TCondition;
begin
  result := TBringCondition.Create(Player,UnitType,Location,Mode,Value);
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
  modeStr := IntegerConditionModeToTrigEditStr[Mode];
  if (CompareText(UnitType,'ore')=0) or (CompareText(UnitType,'gas')=0) or (CompareText(UnitType,'ore and gas')=0) then
      Result:= 'Accumulate("' + PlayerToTrigEditStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + LowerCase(UnitType) + ')'
  else
  if CompareText(copy(UnitType,length(UnitType)-5,6),' Score')=0 then
    result := 'Score("' + PlayerToTrigEditStr(Player) + '", ' + copy(UnitType,1,length(UnitType)-6) + ', ' + modeStr + ', ' + IntToStr(Value) + ')'
  else
  if CompareText(UnitType,'Countdown')=0 then
    result := 'Countdown Timer(' + modeStr + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Deaths("' + PlayerToTrigEditStr(Player) + '", ' + AddTrigEditQuotes(UnitType) + ', ' + modeStr + ', ' + IntToStr(Value) + ')';
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

end.

