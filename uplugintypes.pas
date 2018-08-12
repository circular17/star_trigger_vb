unit uplugintypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uscmdrafttypes;

const
  PluginMenu = 'StarBasic progam';

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

end.

