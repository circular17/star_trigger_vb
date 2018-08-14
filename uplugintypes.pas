unit uplugintypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uscmdrafttypes, umapinfo;

const
  PluginMenu = 'BroodBasic program';
  PluginMenuSection = 'TRIG';

type

  { TPluginContext }

  TPluginContext = object
    Section: TMenuSection;
    EngineData: PEngineData;
    Triggers, MissionBriefing, SwitchRenaming,
    UnitProperties, UnitPropUsage: PChunkData;

    function GetLocationName(AIndex: integer): string;
    function GetLocationIndex(AName: string): integer;
    function LocationExists(AIndex: integer): boolean;
    function GetForceName(AForce: integer): string;
    function GetStandardUnitName(AIndex: integer): string;
    function GetCustomUnitName(AIndex: integer): string;
    function GetWavFilename(AIndex: integer): string;
    function GetWavIndex(AFilename: string): integer;
    function GetSwitchName(AIndex: integer): string;
    function GetSwitchIndex(AName: string): integer;
  end;

  { TPluginMapInfo }

  TPluginMapInfo = class(TCustomMapInfo)
  protected
    FContext: TPluginContext;
    function GetAnywhereLocationName: string; override;
    function GetForceName(AIndexBase1: integer): string; override;
    function GetLocationName(AIndex: integer): string; override;
    function GetSwitchName(AIndexBase1: integer): string; override;
  public
    constructor Create(const AContext: TPluginContext);
    function IsAnywhere(ALocation: string): boolean; override;
    function LocationIndexOf(ALocation:string): integer; override;
    function MapStringAllocate(AText: string): integer; override;
    function MapStringRead(AIndex: integer): string; override;
    procedure MapStringRelease(AIndex: integer); override;
    function MapStringIndexOf(AText: string): integer; override;
    function SoundFilenameExists(AFilename: string): boolean; override;
    function UseSoundFilename(AFilename: string): integer; override;
    function LocationExists(AIndex: integer): boolean; override;
  end;

implementation

{ TPluginContext }

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

function TPluginContext.GetStandardUnitName(AIndex: integer): string;
begin
  result := EngineData^.GetStandardUnitName(AIndex);
end;

function TPluginContext.GetCustomUnitName(AIndex: integer): string;
begin
  result := EngineData^.GetCustomUnitName(AIndex);
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

function TPluginMapInfo.GetSwitchName(AIndexBase1: integer): string;
begin
  result := FContext.GetSwitchName(AIndexBase1);
  if result = '' then result := 'Switch'+inttostr(AIndexBase1);
end;

constructor TPluginMapInfo.Create(const AContext: TPluginContext);
begin
  FContext := AContext;
end;

function TPluginMapInfo.IsAnywhere(ALocation: string): boolean;
begin
  result := CompareText(ALocation, AnywhereLocationName)=0;
end;

function TPluginMapInfo.LocationIndexOf(ALocation: string): integer;
begin
  result := FContext.GetLocationIndex(ALocation);
end;

function TPluginMapInfo.MapStringAllocate(AText: string): integer;
begin
  result := FContext.EngineData^.MapStrings^.AllocateString(AText, SectionCodeToLongWord('TRIG'));
end;

function TPluginMapInfo.MapStringRead(AIndex: integer): string;
begin
  result := FContext.EngineData^.MapStrings^.GetString(AIndex);
end;

procedure TPluginMapInfo.MapStringRelease(AIndex: integer);
begin
  with FContext.EngineData^.MapStrings^ do
    ReleaseString(AIndex, SectionCodeToLongWord('TRIG'));
end;

function TPluginMapInfo.MapStringIndexOf(AText: string): integer;
begin
  result := FContext.EngineData^.MapStrings^.IndexOf(AText);
end;

function TPluginMapInfo.SoundFilenameExists(AFilename: string): boolean;
begin
  result := FContext.GetWavIndex(AFilename)<>-1;
end;

function TPluginMapInfo.UseSoundFilename(AFilename: string): integer;
begin
  if not SoundFilenameExists(AFilename) then
    raise exception.Create('Sound is not listed');
  result := MapStringAllocate(AFilename);
end;

function TPluginMapInfo.LocationExists(AIndex: integer): boolean;
begin
  result := FContext.LocationExists(AIndex);
end;

end.
