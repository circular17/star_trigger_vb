unit uinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

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
    function IsArithmetic: boolean; virtual;
    function IsComputed: boolean; virtual;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit); virtual;
    function Duplicate: TCondition; virtual; abstract;
  end;

  TCustomConditionList = specialize TFPGList<TCondition>;

  { TConditionList }

  TConditionList = class(TCustomConditionList)
    function IsComputed: boolean;
    function IsArithmetic: boolean;
    procedure Compute(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit);
    procedure FreeAll;
    function Duplicate: TConditionList;
  end;

  { TRandomizeIntegerInstruction }

  TRandomizeIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Range: integer;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; ARange: integer);
  end;

  TIntegerTransfer = (itCopyIntoAccumulator, itAddIntoAccumulator, itCopyAccumulator, itAddAccumulator,
                      itSubtractIntoAccumulator, itSubtractAccumulator, itRandomizeAccumulator, itLimitAccumulator);

  { TTransferIntegerInstruction }

  TTransferIntegerInstruction = class(TInstruction)
    Player: TPlayer;
    UnitType: TStarcraftUnit;
    Action: TIntegerTransfer;
    Value: integer;
    Shift: integer;
    constructor Create(APlayer: TPlayer; AUnitType: TStarcraftUnit; AAction: TIntegerTransfer; AShift: integer = 0);
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
  { TNotCondition }

  TNotCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit); override;
    function Duplicate: TCondition; override;
  end;

  { TOrCondition }

  TOrCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar(AProg: TInstructionList; APlayer: TPlayer; AUnitType: TStarcraftUnit); override;
    function Duplicate: TCondition; override;
  end;

  { TAndCondition }

  TAndCondition = class(TCondition)
    Conditions: TConditionList;
    destructor Destroy; override;
    constructor Create(AConditions: array of TCondition);
    constructor Create(AConditions: TConditionList);
    function IsArithmetic: boolean; override;
    function IsComputed: boolean; override;
    procedure AddToProgAsAndVar({%H-}AProg: TInstructionList; {%H-}APlayer: TPlayer; {%H-}AUnitType: TStarcraftUnit); override;
    function Duplicate: TCondition; override;
  end;

type
  TCreateSetIntegerInstructionProc = function (APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TSetIntegerMode; AValue: integer): TInstruction;
  TCreateIntegerConditionProc = function (APlayer: TPlayer; AUnitType: TStarcraftUnit; AMode: TIntegerConditionMode; AValue: integer): TCondition;

var
  CreateSetIntegerInstruction: TCreateSetIntegerInstructionProc;
  CreateIntegerCondition: TCreateIntegerConditionProc;

implementation

{ TRandomizeIntegerInstruction }

constructor TRandomizeIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; ARange: integer);
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
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
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
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
var
  i: Integer;
begin
  if Conditions.Count > 0 then
  begin
    AProg.Add(TIfInstruction.Create(CreateIntegerCondition(APlayer,AUnitType,icmAtLeast,1)));
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
    for i := 0 to Conditions.Count-1 do
    begin
      if not Conditions[i].IsComputed then
      begin
        AProg.Add(TFastIfInstruction.Create(Conditions[i],[CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1)]));
      end else
      begin
        AProg.Add(TIfInstruction.Create(Conditions[i].Duplicate));
        AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1));
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
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
begin
  if not Conditions.IsComputed then
  begin
    AProg.Add(TFastIfInstruction.Create(Conditions,[CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0)]));
  end else
  begin
    AProg.Add(TIfInstruction.Create(Conditions.Duplicate));
    AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
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

{ TTransferIntegerInstruction }

constructor TTransferIntegerInstruction.Create(APlayer: TPlayer;
  AUnitType: TStarcraftUnit; AAction: TIntegerTransfer; AShift: integer);
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
  UnitType:= suConst;
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
  AUnitType: TStarcraftUnit);
var
  i: Integer;
begin
  AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,1));
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

function TCondition.IsArithmetic: boolean;
begin
  result := false;
end;

function TCondition.IsComputed: boolean;
begin
  result := IsArithmetic;
end;

procedure TCondition.AddToProgAsAndVar(AProg: TInstructionList;
  APlayer: TPlayer; AUnitType: TStarcraftUnit);
begin
  AProg.Add(TIfInstruction.Create(self.Duplicate));
  AProg.Add(TElseInstruction.Create);
  AProg.Add(CreateSetIntegerInstruction(APlayer,AUnitType,simSetTo,0));
  AProg.Add(TEndIfInstruction.Create);
end;

end.

