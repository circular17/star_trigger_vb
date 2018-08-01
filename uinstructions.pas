unit uinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  { TInstruction }

  TInstruction = class
    function ToStringAndFree: ansistring;
  end;

  { TEmptyInstruction }

  TEmptyInstruction = class(TInstruction)
    function ToString: ansistring; override;
  end;

  TInstructionList = specialize TFPGList<TInstruction>;

type
  TSwitchValue = (svClear, svSet, svRandomize, svToggle);

const
  BoolToSwitch : array[Boolean] of TSwitchValue = (svClear, svSet);
  SwitchToStr : array[TSwitchValue] of string = ('clear','set','randomize','toggle');

type
  TCondition = class;

  { TSetSwitchInstruction }

  TSetSwitchInstruction = class(TInstruction)
    SwitchName: string;
    Value: TSwitchValue;
    constructor Create(ASwitchName: string; AValue: TSwitchValue);
    function ToString: ansistring; override;
  end;

  TPlayer = (plNone, plPlayer1, plPlayer2, plPlayer3, plPlayer4,
             plPlayer5, plPlayer6, plPlayer7, plPlayer8,
             plPlayer9, plPlayer10, plPlayer11, plPlayer12,
             plUnknown13, plCurrentPlayer, plFoes, plAllies,
             plNeutralPlayers, plAllPlayers,
             plForce1, plForce2, plForce3, plForce4,
             plUnknown23, plUnknown24, plUnknown25, plUnknown26,
             plNonAlliedVictoryPlayers);
  TPlayers = set of TPlayer;
  TSetIntegerMode = (simSetTo, simAdd, simSubtract, simRandomize);

  { TSetIntInstruction }

  TSetIntInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TSetIntegerMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TSetIntegerMode; AValue: integer);
    function ToString: ansistring; override;
  end;

  { TAddDeathFromSwitchInstruction }

  TAddDeathFromSwitchInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: string;
    Switches: array of string;
    constructor Create(APlayer: TPlayer; AUnitType: string; ASwitches: array of string);
    function ToString: ansistring; override;
  end;

  { TDisplayTextMessageInstruction }

  TDisplayTextMessageInstruction = class(TInstruction)
    Always: boolean;
    Text: string;
    Players: TPlayers;
    constructor Create(AAlways: boolean; AMessage: string; APlayers: TPlayers);
    function ToString: ansistring; override;
  end;

  { TWaitInstruction }

  TWaitInstruction = class(TInstruction)
    DelayMs: integer;
    constructor Create(ADelayMs: integer);
    function ToString: ansistring; override;
  end;

  { TCallInstruction }

  TCallInstruction = class(TInstruction)
    Name: string;
    Params: array of string;
    constructor Create(AName: string; AParams: array of string);
    constructor Create(AName: string; AParams: TStringList);
    function ToString: ansistring; override;
  end;

  { TPushInstruction }

  TPushInstruction = class(TInstruction)
    Value: integer;
    SwitchName: string;
    NextIP: integer;
    constructor Create(AValue: integer; ASwitchName: string; ANextIP: integer);
    function ToString: ansistring; override;
  end;

  { TJumpReturnInstruction }

  TJumpReturnInstruction = class(TInstruction)
    DestIP, ReturnIP: integer;
    constructor Create(ADestIP, AReturnIP: integer);
    function ToString: ansistring; override;
  end;

  { TReturnInstruction }

  TReturnInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TIfInstruction }

  TIfInstruction = class(TInstruction)
    Condition: TCondition;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition);
    function ToString: ansistring; override;
  end;

  { TWaitConditionInstruction }

  TWaitConditionInstruction = class(TInstruction)
    Condition: TCondition;
    IP: integer;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition; AIP: Integer);
    function ToString: ansistring; override;
  end;

  { TElseInstruction }

  TElseInstruction = class(TInstruction)
    ThenElseIP, EndIP: integer;
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TEndIfInstruction }

  TEndIfInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

  { TChangeIPInstruction }

  TChangeIPInstruction = class(TInstruction)
    IP: integer;
    Preserve: boolean;
    constructor Create(AIP: Integer; APreserve: boolean);
    function ToString: ansistring; override;
  end;

  { TWhileInstruction }

  TWhileInstruction = class(TInstruction)
    Condition: TCondition;
    destructor Destroy; override;
    constructor Create(ACondition: TCondition);
    function ToString: ansistring; override;
  end;

  { TEndWhileInstruction }

  TEndWhileInstruction = class(TInstruction)
    constructor Create;
    function ToString: ansistring; override;
  end;

function PlayerToStr(APlayer: TPlayer): string;
function IntToPlayer(APlayer: integer): TPlayer;

type

  { TCondition }

  TCondition = class
    function ToStringAndFree: ansistring;
  end;

  TConditionList = specialize TFPGList<TCondition>;

  { TAlwaysCondition }

  TAlwaysCondition = class(TCondition)
    function ToString: ansistring; override;
  end;

  { TNeverCondition }

  TNeverCondition = class(TCondition)
    function ToString: ansistring; override;
  end;

  { TSwitchCondition }

  TSwitchCondition = class(TCondition)
    SwitchName: string;
    Value: boolean;
    constructor Create(ASwitchName: string; AValue: boolean);
    function ToString: ansistring; override;
  end;

  TIntegerConditionMode = (dcmAtLeast,dcmAtMost,dcmExactly,dcmNotEqualTo);

  { TIntegerCondition }

  TIntegerCondition = class(TCondition)
    Player: TPlayer;
    UnitType: string;
    Value: integer;
    Mode: TIntegerConditionMode;
    constructor Create(APlayer: TPlayer; AUnitType: string; AMode: TIntegerConditionMode; AValue: integer);
    function ToString: ansistring; override;
  end;

implementation

{ TWaitConditionInstruction }

destructor TWaitConditionInstruction.Destroy;
begin
  Condition.Free;
  inherited Destroy;
end;

constructor TWaitConditionInstruction.Create(ACondition: TCondition; AIP: Integer);
begin
  Condition := ACondition;
  IP := AIP;
end;

function TWaitConditionInstruction.ToString: ansistring;
begin
  Result:='Wait(' + Condition.ToString + ')';
end;

{ TChangeIPInstruction }

constructor TChangeIPInstruction.Create(AIP: Integer; APreserve: boolean);
begin
  IP := AIP;
  Preserve := APreserve;
end;

function TChangeIPInstruction.ToString: ansistring;
begin
  Result:=inherited ToString;
end;

{ TNeverCondition }

function TNeverCondition.ToString: ansistring;
begin
  Result:= 'Never()';
end;

{ TEndIfInstruction }

constructor TEndIfInstruction.Create;
begin
  //nothing
end;

function TEndIfInstruction.ToString: ansistring;
begin
  Result:= '}';
end;

{ TElseInstruction }

constructor TElseInstruction.Create;
begin
  EndIP := -1;
end;

function TElseInstruction.ToString: ansistring;
begin
  Result:= '} Else {';
end;

{ TIfInstruction }

destructor TIfInstruction.Destroy;
begin
  Condition.Free;
  inherited Destroy;
end;

constructor TIfInstruction.Create(ACondition: TCondition);
begin
  Condition := ACondition;
end;

function TIfInstruction.ToString: ansistring;
begin
  Result:= 'If ('+ Condition.ToString+'){';
end;

{ TWhileInstruction }

destructor TWhileInstruction.Destroy;
begin
  Condition.Free;
  inherited Destroy;
end;

constructor TWhileInstruction.Create(ACondition: TCondition);
begin
  Condition := ACondition;
end;

function TWhileInstruction.ToString: ansistring;
begin
  Result:= 'While ('+ Condition.ToString+')';
end;

{ TEndWhileInstruction }

constructor TEndWhileInstruction.Create;
begin
 //nothing
end;

function TEndWhileInstruction.ToString: ansistring;
begin
  Result:= '}';
end;

{ TPushInstruction }

constructor TPushInstruction.Create(AValue: integer; ASwitchName: string; ANextIP: integer);
begin
  Value := AValue;
  SwitchName:= ASwitchName;
  NextIP:= ANextIP;
end;

function TPushInstruction.ToString: ansistring;
begin
  Result:= 'Push('+inttostr(Value)+')';
end;

{ TReturnInstruction }

constructor TReturnInstruction.Create;
begin
  //nothing
end;

function TReturnInstruction.ToString: ansistring;
begin
  Result:= 'Return';
end;

{ TJumpReturnInstruction }

constructor TJumpReturnInstruction.Create(ADestIP, AReturnIP: integer);
begin
  DestIP:= ADestIP;
  ReturnIP:= AReturnIP;
end;

function TJumpReturnInstruction.ToString: ansistring;
begin
  result := '';
end;

{ TCallInstruction }

constructor TCallInstruction.Create(AName: string; AParams: array of string);
var
  i: Integer;
begin
  Name := AName;
  setlength(Params, length(AParams));
  for i := 0 to high(AParams) do
    Params[i] := AParams[i];
end;

constructor TCallInstruction.Create(AName: string; AParams: TStringList);
var
  i: Integer;
begin
  Name := AName;
  setlength(Params, AParams.Count);
  for i := 0 to AParams.Count-1 do
    Params[i] := AParams[i];
end;

function TCallInstruction.ToString: ansistring;
var
  i: Integer;
begin
  Result:= Name+'(';
  for i := 0 to high(Params) do
  begin
    if i > 0 then result += ', ';
    Result += Params[i];
  end;
  result += ')';
end;

{ TWaitInstruction }

constructor TWaitInstruction.Create(ADelayMs: integer);
begin
  DelayMs:= ADelayMs;
end;

function TWaitInstruction.ToString: ansistring;
begin
  Result:= 'Wait('+IntToStr(DelayMs)+')';
end;

{ TDisplayTextMessageInstruction }

constructor TDisplayTextMessageInstruction.Create(AAlways: boolean;
  AMessage: string; APlayers: TPlayers);
begin
  Always:= AAlways;
  Text:= AMessage;
  Players:= APlayers;
end;

function TDisplayTextMessageInstruction.ToString: ansistring;
begin
  Result := 'Display Text Message(' + BoolToStr(Always, 'Always Display', 'Don''t Always Display') +
            ', "' + StringReplace(StringReplace(Text, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]) + '")';
end;

{ TAddDeathFromSwitchInstruction }

constructor TAddDeathFromSwitchInstruction.Create(APlayer: TPlayer;
  AUnitType: string; ASwitches: array of string);
var
  i: Integer;
begin
  Player := APlayer;
  UnitType:= AUnitType;
  setlength(Switches, length(ASwitches));
  for i := 0 to high(ASwitches) do
    Switches[i] := ASwitches[i];
end;

function TAddDeathFromSwitchInstruction.ToString: ansistring;
begin
  Result:= 'Set Deaths("' + PlayerToStr(Player) + '", "' + UnitType + '", Add, switches)'; //not a real instruction
end;

{ TCondition }

function TCondition.ToStringAndFree: ansistring;
begin
  result := ToString;
  Free;
end;

{ TInstruction }

function TInstruction.ToStringAndFree: ansistring;
begin
  result := ToString;
  Free;
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

function TIntegerCondition.ToString: ansistring;
var
  modeStr: string;
begin
  case Mode of
  dcmAtLeast: modeStr := 'At least';
  dcmAtMost: modeStr := 'At most';
  dcmNotEqualTo: modeStr := 'Not equal to';
  else
    {dcmExactly: }modeStr := 'Exactly';
  end;
  if (CompareText(UnitType,'Ore')=0) or (CompareText(UnitType,'Gas')=0) or (CompareText(UnitType,'Ore And Gas')=0) then
      Result:= 'Accumulate("' + PlayerToStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + LowerCase(UnitType) + ')'
  else
  if (CompareText(UnitType,'Units Score')=0) or (CompareText(UnitType,'Buildings Score')=0)
  or (CompareText(UnitType,'Kills Score')=0) or (CompareText(UnitType,'Razings Score')=0)
  or (CompareText(UnitType,'Custom Score')=0) then
    result := 'Score("' + PlayerToStr(Player) + '", ' + copy(UnitType,1,length(UnitType)-6) + ', ' + modeStr + ', ' + IntToStr(Value) + ')'
  else
    Result:= 'Deaths("' + PlayerToStr(Player) + '", "' + UnitType + '", ' + modeStr + ', ' + IntToStr(Value) + ')';
end;

{ TAlwaysCondition }

function TAlwaysCondition.ToString: ansistring;
begin
  Result:= 'Always()';
end;

{ TSwitchCondition }

constructor TSwitchCondition.Create(ASwitchName: string; AValue: boolean);
begin
  SwitchName := ASwitchName;
  Value := AValue;
end;

function TSwitchCondition.ToString: ansistring;
begin
  Result:= 'Switch("' + SwitchName + '", ' + BoolToStr(Value, 'set', 'not set') + ')';
end;

{ TSetIntInstruction }

constructor TSetIntInstruction.Create(APlayer: TPlayer; AUnitType: string;
  AMode: TSetIntegerMode; AValue: integer);
begin
  Player := APlayer;
  UnitType := AUnitType;
  Mode := AMode;
  Value := AValue;
end;

function TSetIntInstruction.ToString: ansistring;
var
  modeStr: string;
begin
  case Mode of
  simSetTo: modeStr := 'Set To';
  simSubtract: modeStr := 'Subtract';
  simRandomize: modeStr := 'Randomize';
  else
    {simAdd: }modeStr := 'Add';
  end;
  if (CompareText(UnitType,'Ore')=0) or (CompareText(UnitType,'Gas')=0) or (CompareText(UnitType,'Ore And Gas')=0) then
    Result:= 'Set Resources("' + PlayerToStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + LowerCase(UnitType) + ')'
  else
  if (CompareText(UnitType,'Units Score')=0) or (CompareText(UnitType,'Buildings Score')=0)
  or (CompareText(UnitType,'Kills Score')=0) or (CompareText(UnitType,'Razings Score')=0)
  or (CompareText(UnitType,'Custom Score')=0) then
    result := 'Set Score("' + PlayerToStr(Player) + '", ' + modeStr + ', ' + IntToStr(Value) + ', ' + copy(UnitType,1,length(UnitType)-6) + ')'
  else
    Result:= 'Set Deaths("' + PlayerToStr(Player) + '", "' + UnitType + '", ' + modeStr + ', ' + IntToStr(Value) + ')';
end;

{ TEmptyInstruction }

function TEmptyInstruction.ToString: ansistring;
begin
  Result:= '';
end;

{ TSetSwitchInstruction }

constructor TSetSwitchInstruction.Create(ASwitchName: string;
  AValue: TSwitchValue);
begin
  SwitchName:= ASwitchName;
  Value:= AValue;
end;

function TSetSwitchInstruction.ToString: ansistring;
begin
  Result:= 'Set Switch("' + SwitchName+ '", ' + SwitchToStr[Value] + ')';
end;

function PlayerToStr(APlayer: TPlayer): string;
begin
  case APlayer of
  plPlayer1..plPlayer12: result := 'Player ' + IntToStr(ord(APlayer) - ord(plPlayer1)+1);
  plCurrentPlayer: result := 'Current Player';
  plFoes: result := 'Foes';
  plAllies: result := 'Allies';
  plNeutralPlayers: result := 'Neutral players';
  plAllPlayers: result := 'All players';
  plForce1..plForce4: result := 'Force ' + IntToStr(ord(APlayer) - ord(plForce1)+1);
  plNonAlliedVictoryPlayers: result := 'Non Allied Victory Players';
  else result := 'unknown/unused';
  end;
end;

function IntToPlayer(APlayer: integer): TPlayer;
begin
  result := TPlayer(ord(plPlayer1)+APlayer-1);
end;

end.

