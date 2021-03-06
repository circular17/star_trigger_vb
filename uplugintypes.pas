unit uplugintypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uscmdrafttypes, umapinfo, utriggercode, utriggerchunk, usctypes,
  utriggerconditions, utriggerinstructions, uunitpropchunk;

const
  PluginMenu = 'BroodBasic program';
  PluginMenuSection = 'TRIG';

type

  { TPluginContext }

  TPluginContext = object
  private
    function GetTrigger(AIndex: integer): PTriggerData;
    function GetTriggerCount: integer;
    function GetUnitProperties(AIndex: integer): TUnitPropertiesData;
    function GetUnitPropertiesUsed(AIndex: integer): boolean;
    procedure SetUnitProperties(AIndex: integer; const AValue: TUnitPropertiesData);
    procedure SetUnitPropertiesUsed(AIndex: integer; AValue: boolean);
  public
    Section: TMenuSection;
    EngineData: PEngineData;
    Triggers, MissionBriefing, SwitchRenaming,
    UnitPropertiesList, UnitPropUsage: PChunkData;
    AllocRam: TAllocRamProc;
    DeAllocRam: TDeAllocRamProc;
    ReAllocRam: TReAllocRamProc;
    procedure ClearTriggers;
    procedure AddCompiledTriggers;
    procedure UpdateCompiledUnitProperties;
    function RetrieveStoredTriggers: TTriggerList;
    function RetrieveStoredProgram: string;
    function UseWavFilename(AFilename: string): integer;
    function GetLocationName(AIndex: integer): string;
    function GetLocationIndex(AName: string): integer;
    function LocationExists(AIndex: integer): boolean;
    function GetForceName(AForce: integer): string;
    function GetStandardUnitName(AUnitType: TStarcraftUnit): string;
    function GetCustomUnitName(AUnitType: TStarcraftUnit): string;
    function GetWavFilename(AIndex: integer): string;
    function GetWavIndex(AFilename: string): integer;
    function GetSwitchName(AIndex: integer): string;
    function GetSwitchIndex(AName: string): integer;
    property TriggerCount: integer read GetTriggerCount;
    property Trigger[AIndex: integer]: PTriggerData read GetTrigger;
    property UnitProperties[AIndex: integer]: TUnitPropertiesData read GetUnitProperties write SetUnitProperties;
    property UnitPropertiesUsed[AIndex: integer]: boolean read GetUnitPropertiesUsed write SetUnitPropertiesUsed;
  end;

  { TPluginMapInfo }

  TPluginMapInfo = class(TCustomMapInfo)
  protected
    FContext: TPluginContext;
    function GetAnywhereLocationName: string; override;
    function GetForceName(AIndexBase1: integer): string; override;
    function GetLocationName(AIndex: integer): string; override;
    function GetWavName(AIndex: integer): string; override;
    function GetSwitchName(AIndexBase1: integer): string; override;
    function GetProgramMapEmbedded: boolean; override;
    function GetCustomUnitName(AUnitType: TStarcraftUnit): string; override;
    function GetStandardUnitName(AUnitType: TStarcraftUnit): string; override;
  public
    constructor Create(const AContext: TPluginContext);
    function RetrieveStoredProgram: string; override;
    procedure UpdateTriggers; override;
    procedure UpdateUnitProperties; override;
    function StrictLocations: boolean; override;
    function LocationIndexOf(ALocation:string): integer; override;
    function TrigStringAllocate(AText: string): integer; override;
    procedure TrigStringRelease(AIndex: integer); override;
    function MapStringRead(AIndex: integer): string; override;
    function MapStringIndexOf(AText: string): integer; override;
    function SoundFilenameExists(AFilename: string): boolean; override;
    function LocationExists(AIndex: integer): boolean; override;
  end;

implementation

uses uparsevb;

{ TPluginContext }

function TPluginContext.GetTrigger(AIndex: integer): PTriggerData;
begin
  result := PTriggerData(Triggers^.Data)+AIndex;
end;

function TPluginContext.GetTriggerCount: integer;
begin
  result := Triggers^.Size div sizeof(TTriggerData);
end;

function TPluginContext.GetUnitProperties(AIndex: integer): TUnitPropertiesData;
begin
  if (AIndex < MIN_UNIT_PROPERTIES) or
     (dword(AIndex-MIN_UNIT_PROPERTIES) >= UnitPropertiesList^.Size div sizeof(TUnitPropertiesData)) then
     raise exception.Create('Unit properties chunk out of bounds');
  result := (PUnitPropertiesData(UnitPropertiesList^.Data)+(AIndex-MIN_UNIT_PROPERTIES))^;
end;

function TPluginContext.GetUnitPropertiesUsed(AIndex: integer): boolean;
begin
  if (AIndex < MIN_UNIT_PROPERTIES) or
     (dword(AIndex-MIN_UNIT_PROPERTIES) >= UnitPropUsage^.Size) then exit(false);
  result := (PByte(UnitPropUsage^.Data)+(AIndex-MIN_UNIT_PROPERTIES))^ <> 0;
end;

procedure TPluginContext.SetUnitProperties(AIndex: integer;
  const AValue: TUnitPropertiesData);
begin
  if (AIndex < MIN_UNIT_PROPERTIES) or
     (dword(AIndex-MIN_UNIT_PROPERTIES) >= UnitPropertiesList^.Size div sizeof(TUnitPropertiesData)) then
     raise exception.Create('Unit properties chunk out of bounds');
  (PUnitPropertiesData(UnitPropertiesList^.Data)+(AIndex-MIN_UNIT_PROPERTIES))^ := AValue;
end;

procedure TPluginContext.SetUnitPropertiesUsed(AIndex: integer; AValue: boolean);
begin
  if (AIndex < MIN_UNIT_PROPERTIES) or
     (dword(AIndex-MIN_UNIT_PROPERTIES) >= UnitPropUsage^.Size) then raise exception.Create('Unit properties chunk out of bounds');
  (PByte(UnitPropUsage^.Data)+AIndex-MIN_UNIT_PROPERTIES)^ := byte(AValue);
end;

procedure TPluginContext.ClearTriggers;
var
  i, a: Integer;
  t: PTriggerData;
begin
  for i := 0 to TriggerCount-1 do
  begin
    t := Trigger[i];
    for a := low(t^.Actions) to high(t^.Actions) do
    begin
      EngineData^.TrigReleaseString(t^.Actions[a].StringIndex);
      t^.Actions[a].StringIndex := 0;
      EngineData^.TrigReleaseString(t^.Actions[a].WavStringIndex);
      t^.Actions[a].WavStringIndex := 0;
    end;
  end;
  DeAllocRam(Triggers^.Data);
  Triggers^.Data := nil;
  Triggers^.Size := 0;
end;

procedure TPluginContext.AddCompiledTriggers;
var
  t: PTriggerData;
  from, i, j, totalCount: Integer;
  pl: TPlayer;
begin
  if CompiledTriggersMultiCount = 0 then exit;
  from := TriggerCount;
  totalCount := from + CompiledTriggersMultiCount;
  if totalCount > MAX_TRIGGER_COUNT then
    raise exception.Create('Too many triggers');

  Triggers^.Size := totalCount*sizeof(TTriggerData);
  Triggers^.Data := ReAllocRam(Triggers^.Data, Triggers^.Size);
  j := from;
  for i := 0 to CompiledTriggers.Count-1 do
  begin
    if CompiledTriggers[i].IsMultithread then
    begin
      for pl := succ(plNone) to high(TPlayer) do
        if pl in CompiledTriggers[i].Players then
        begin
          t := Trigger[j];
          fillchar(t^, sizeof(TTriggerData), 0);
          CompiledTriggers[i].WriteTriggerDataFor(pl, t^);
          inc(j);
        end;
    end else
    begin
      t := Trigger[j];
      fillchar(t^, sizeof(TTriggerData), 0);
      CompiledTriggers[i].WriteTriggerData(t^);
      inc(j);
    end;
  end;
end;

procedure TPluginContext.UpdateCompiledUnitProperties;
var
  i: Integer;
begin
  for i := MIN_UNIT_PROPERTIES to MAX_UNIT_PROPERTIES do
  begin
    UnitPropertiesUsed[i] := CompiledUnitProperties[i].Used;
    if CompiledUnitProperties[i].Used then
      UnitProperties[i] := CompiledUnitProperties[i].Data;
  end;
end;

function TPluginContext.RetrieveStoredTriggers: TTriggerList;
var
  loadedTriggers: TTriggerList;
  t: TTrigger;
  i: Integer;
begin
  loadedTriggers := TTriggerList.Create;
  try
    for i := 0 to TriggerCount-1 do
    begin
      t := TTrigger.Create;
      t.ReadTriggerData(Trigger[i]^);
      loadedTriggers.Add(t);
    end;
    result := loadedTriggers;
  except
    on ex: exception do
    begin
      loadedTriggers.Free;
      raise exception.Create('Unable to retrieve triggers. '+ex.Message);
    end;
  end;
end;

function TPluginContext.RetrieveStoredProgram: string;
var trigs: TTriggerList;
  i,j: Integer;

  procedure AddSoundInfo(AFilename: string; ADuration: integer);
  var
    k: Integer;
  begin
    for k := 0 to high(BasicSoundInfoArray) do
      if (BasicSoundInfoArray[k].Filename = AFilename) and
        (BasicSoundInfoArray[k].Duration = ADuration) then exit;

    setlength(BasicSoundInfoArray, length(BasicSoundInfoArray)+1);
    with BasicSoundInfoArray[high(BasicSoundInfoArray)] do
    begin
      ConstName:= 'Sound' + inttostr(length(BasicSoundInfoArray));
      Filename:= AFilename;
      Duration:= ADuration;
      result += 'Const '+ConstName+' As Sound = {.Filename = '+
        StrToBasic(Filename)+', .Duration = '+inttostr(Duration)+'}' + LineEnding;
    end;
  end;

begin
  trigs:= RetrieveStoredTriggers;
  for i := 0 to trigs.Count-1 do
    if (trigs[i].Players = [plNeutralPlayers]) and
      (trigs[i].ConditionCount = 1) and
      (trigs[i].Condition[0] is TNeverCondition) and
      (trigs[i].ActionCount = 2) and
      (trigs[i].Action[0] is TCommentInstruction) and
      (trigs[i].Action[1] is TCommentInstruction) and
      (TCommentInstruction(trigs[i].Action[0]).Text = 'BroodBasic source code') then
    begin
      result := TCommentInstruction(Trigs[i].Action[1]).Text;
      trigs.Free;
      exit;
    end;

  try
    result := '';
    BasicSoundInfoArray := nil;
    for i := 0 to trigs.count-1 do
      for j := 0 to trigs[i].ActionCount-1 do
        if trigs[i].Action[j] is TPlayWAVInstruction then
        with TPlayWAVInstruction(trigs[i].Action[j]) do
          AddSoundInfo(Filename, DurationMs);
    for i := MIN_UNIT_PROPERTIES to MAX_UNIT_PROPERTIES do
      if UnitPropertiesUsed[i] then
      with UnitProperties[i] do
      begin
        result += 'Const UnitProperties' + inttostr(i) + ' As UnitProperties = {';
        if CloakValid and IsCloaked then result += '.Cloaked = True, ';
        if BurrowValid and IsBurrowed then result += '.Burrowed = True, ';
        if InTransitValid and IsInTransit then result += '.Lifted = True, ';
        if HallucinateValid and IsHallucinated then result += '.Hallucinated = True, ';
        if InvincibleValid and IsInvincible then result += '.Invincible = True, ';
        if HitPointsValid and (HitPoints <> 100) then result += '.Life = ' + inttostr(HitPoints) + ', ';
        if ShieldValid and (Shield <> 100) then result += '.Shield = ' + inttostr(Shield) + ', ';
        if EnergyValid and (Energy <> 25) then result += '.Energy = ' + inttostr(Energy) + ', ';
        if ResourceAmountValid and (ResourceAmount <> 0) then result += '.Resource = ' + inttostr(ResourceAmount) + ', ';
        if HangarCountValid and (HangarCount <> 0) then result += '.HangarCount = ' + inttostr(HangarCount) + ', ';
        if copy(result, length(result)-1, 2) = ', ' then delete(result, length(result)-1, 2);
        result += '}' + LineEnding;
      end;
    if result <> '' then result += LineEnding;
    for i := 0 to trigs.Count-1 do
      result += trigs[i].ToBasic+LineEnding+LineEnding;
    trigs.Free;
  finally
    BasicSoundInfoArray := nil;
  end;
end;

function TPluginContext.UseWavFilename(AFilename: string): integer;
begin
  if EngineData^.GetWavIndex(AFilename) = -1 then
    raise exception.Create('Sound is not part of the listed ones for this map');
  result := EngineData^.TrigAllocateString(AFilename);
end;

function TPluginContext.GetLocationName(AIndex: integer): string;
begin
  result := EngineData^.GetLocationName(AIndex);
end;

function TPluginContext.GetLocationIndex(AName: string): integer;
begin
  result := EngineData^.GetLocationIndex(AName);
end;

function TPluginContext.LocationExists(AIndex: integer): boolean;
begin
  result := EngineData^.LocationExists(AIndex);
end;

function TPluginContext.GetForceName(AForce: integer): string;
begin
  result := EngineData^.GetForceName(AForce);
end;

function TPluginContext.GetStandardUnitName(AUnitType: TStarcraftUnit): string;
begin
  result := EngineData^.GetStandardUnitName(ord(AUnitType));
end;

function TPluginContext.GetCustomUnitName(AUnitType: TStarcraftUnit): string;
begin
  result := EngineData^.GetCustomUnitName(ord(AUnitType));
end;

function TPluginContext.GetWavFilename(AIndex: integer): string;
begin
  result := EngineData^.GetWavFilename(AIndex);
end;

function TPluginContext.GetWavIndex(AFilename: string): integer;
begin
  result := EngineData^.GetWavIndex(AFilename);
end;

function TPluginContext.GetSwitchName(AIndex: integer): string;
begin
  result := EngineData^.MapStrings^.GetString(PSwitchRenamingData(SwitchRenaming^.Data)^[AIndex]);
end;

function TPluginContext.GetSwitchIndex(AName: string): integer;
var
  idxStr, i: Integer;
begin
  idxStr := EngineData^.MapStrings^.IndexOf(AName);
  if idxStr = -1 then exit(-1);
  for i := MIN_SWITCH to MAX_SWITCH do
    if PSwitchRenamingData(SwitchRenaming^.Data)^[i] = idxStr then exit(i);
  exit(-1);
end;

{ TPluginMapInfo }

function TPluginMapInfo.GetAnywhereLocationName: string;
begin
  result := GetLocationName(AnywhereLocationIndex);
end;

function TPluginMapInfo.GetForceName(AIndexBase1: integer): string;
begin
  result := FContext.GetForceName(AIndexBase1);
end;

function TPluginMapInfo.GetLocationName(AIndex: integer): string;
begin
  result := FContext.GetLocationName(AIndex);
end;

function TPluginMapInfo.GetWavName(AIndex: integer): string;
begin
  result := FContext.GetWavFilename(AIndex);
end;

function TPluginMapInfo.GetSwitchName(AIndexBase1: integer): string;
begin
  result := FContext.GetSwitchName(AIndexBase1);
  if result = '' then result := 'Switch'+inttostr(AIndexBase1);
end;

function TPluginMapInfo.GetProgramMapEmbedded: boolean;
begin
  result := true;
end;

function TPluginMapInfo.GetCustomUnitName(AUnitType: TStarcraftUnit): string;
begin
  result := FContext.GetCustomUnitName(AUnitType);
end;

function TPluginMapInfo.GetStandardUnitName(AUnitType: TStarcraftUnit): string;
begin
  result := FContext.GetStandardUnitName(AUnitType);
end;

constructor TPluginMapInfo.Create(const AContext: TPluginContext);
begin
  FContext := AContext;
end;

function TPluginMapInfo.RetrieveStoredProgram: string;
begin
  result := FContext.RetrieveStoredProgram;
end;

procedure TPluginMapInfo.UpdateTriggers;
begin
  FContext.ClearTriggers;
  FContext.AddCompiledTriggers;
end;

procedure TPluginMapInfo.UpdateUnitProperties;
begin
  FContext.UpdateCompiledUnitProperties;
end;

function TPluginMapInfo.StrictLocations: boolean;
begin
  result := true;
end;

function TPluginMapInfo.LocationIndexOf(ALocation: string): integer;
begin
  result := FContext.GetLocationIndex(ALocation);
end;

function TPluginMapInfo.TrigStringAllocate(AText: string): integer;
begin
  result := FContext.EngineData^.TrigAllocateString(AText);
end;

procedure TPluginMapInfo.TrigStringRelease(AIndex: integer);
begin
  FContext.EngineData^.TrigReleaseString(AIndex);
end;

function TPluginMapInfo.MapStringRead(AIndex: integer): string;
begin
  result := FContext.EngineData^.MapStrings^.GetString(AIndex);
end;

function TPluginMapInfo.MapStringIndexOf(AText: string): integer;
begin
  result := FContext.EngineData^.MapStrings^.IndexOf(AText);
end;

function TPluginMapInfo.SoundFilenameExists(AFilename: string): boolean;
begin
  result := FContext.GetWavIndex(AFilename)<>-1;
end;

function TPluginMapInfo.LocationExists(AIndex: integer): boolean;
begin
  result := FContext.LocationExists(AIndex);
end;

end.
