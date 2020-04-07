unit umapinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes;

const
  AnywhereLocationIndex = 63;
  LocationMinIndex = 0;
  LocationMaxIndex = 255;
  WavMinIndex = 0;
  WavMaxIndex = 511;

type

  { TCustomMapInfo }

  TCustomMapInfo = class
  protected
    function GetProgramMapEmbedded: boolean; virtual; abstract;
    function GetAnywhereLocationName: string; virtual; abstract;
    function GetForceName(AIndexBase1: integer): string; virtual; abstract;
    function GetLocationName(AIndex: integer): string; virtual; abstract;
    function GetWavName(AIndex: integer): string; virtual; abstract;
    function GetSwitchName(AIndexBase1: integer): string; virtual; abstract;
    function GetCustomUnitName(AUnitType: TStarcraftUnit): string; virtual; abstract;
    function GetStandardUnitName(AUnitType: TStarcraftUnit): string; virtual; abstract;
  public
    function StrictLocations: boolean; virtual; abstract;
    function RetrieveStoredProgram: string; virtual; abstract;
    procedure UpdateTriggers; virtual; abstract;
    procedure UpdateUnitProperties; virtual; abstract;
    function IsAnywhere(ALocation: string): boolean;
    function LocationIndexOf(ALocation:string): integer; virtual; abstract;
    function LocationExists(AIndex: integer): boolean; virtual; abstract;
    function MapStringRead(AIndex: integer): string; virtual; abstract;
    function MapStringIndexOf(AText: string): integer; virtual; abstract;
    function TrigStringAllocate(AText: string): integer; virtual; abstract;
    procedure TrigStringRelease(AIndex: integer); virtual; abstract;
    function SoundFilenameExists(AFilename: string): boolean; virtual; abstract;
    property ForceName[AIndexBase1: integer]: string read GetForceName;
    property SwitchName[AIndexBase1: integer]: string read GetSwitchName;
    property LocationName[AIndex: integer]: string read GetLocationName;
    property WavName[AIndex: integer]: string read GetWavName;
    property StandardUnitName[AUnitType: TStarcraftUnit]: string read GetStandardUnitName;
    property CustomUnitName[AUnitType: TStarcraftUnit]: string read GetCustomUnitName;
    property AnywhereLocationName: string read GetAnywhereLocationName;
    property ProgramMapEmbedded: boolean read GetProgramMapEmbedded;
  end;

  { TDefaultMapInfo }

  TDefaultMapInfo = class(TCustomMapInfo)
  protected
    function GetProgramMapEmbedded: boolean; override;
    function GetAnywhereLocationName: string; override;
    function GetForceName(AIndexBase1: integer): string; override;
    function GetLocationName(AIndex: integer): string; override;
    function GetWavName({%H-}AIndex: integer): string; override;
    function GetSwitchName(AIndexBase1: integer): string; override;
    function GetStandardUnitName(AUnitType: TStarcraftUnit): string; override;
    function GetCustomUnitName(AUnitType: TStarcraftUnit): string; override;
  public
    function StrictLocations: boolean; override;
    function RetrieveStoredProgram: string; override;
    procedure UpdateTriggers; override;
    procedure UpdateUnitProperties; override;
    function LocationExists(AIndex: integer): boolean; override;
    function LocationIndexOf(ALocation:string): integer; override;
    function TrigStringAllocate({%H-}AText: string): integer; override;
    procedure TrigStringRelease({%H-}AIndex: integer); override;
    function MapStringRead({%H-}AIndex: integer): string; override;
    function MapStringIndexOf({%H-}AText: string): integer; override;
    function SoundFilenameExists({%H-}AFilename: string): boolean; override;
  end;

var
  MapInfo: TCustomMapInfo;

implementation

{ TCustomMapInfo }

function TCustomMapInfo.IsAnywhere(ALocation: string): boolean;
begin
  result := (CompareText(ALocation, AnywhereLocationName)=0) or (ALocation = '');
end;

{ TDefaultMapInfo }

function TDefaultMapInfo.GetProgramMapEmbedded: boolean;
begin
  result := false;
end;

function TDefaultMapInfo.GetAnywhereLocationName: string;
begin
  result := 'Anywhere';
end;

function TDefaultMapInfo.GetForceName(AIndexBase1: integer): string;
begin
  result := 'Force ' + inttostr(AIndexBase1);
end;

function TDefaultMapInfo.GetLocationName(AIndex: integer): string;
begin
  if AIndex = AnywhereLocationIndex then result := GetAnywhereLocationName
  else if AIndex < AnywhereLocationIndex then result := 'Location '+inttostr(AIndex+1)
  else if AIndex > AnywhereLocationIndex then result := 'Location '+inttostr(AIndex);
end;

function TDefaultMapInfo.GetWavName(AIndex: integer): string;
begin
  result := '';
end;

function TDefaultMapInfo.GetSwitchName(AIndexBase1: integer): string;
begin
  result := 'Switch'+inttostr(AIndexBase1);
end;

function TDefaultMapInfo.GetStandardUnitName(AUnitType: TStarcraftUnit): string;
begin
  result := '';
end;

function TDefaultMapInfo.GetCustomUnitName(AUnitType: TStarcraftUnit): string;
begin
  result := '';
end;

function TDefaultMapInfo.StrictLocations: boolean;
begin
  result := false;
end;

function TDefaultMapInfo.RetrieveStoredProgram: string;
begin
  result := '';
  raise exception.Create('Not implemented');
end;

procedure TDefaultMapInfo.UpdateTriggers;
begin
  raise exception.Create('Not implemented');
end;

procedure TDefaultMapInfo.UpdateUnitProperties;
begin
  raise exception.Create('Not implemented');
end;

function TDefaultMapInfo.LocationExists(AIndex: integer): boolean;
begin
  result := (AIndex >= 0) and (AIndex <= 255);
end;

function TDefaultMapInfo.LocationIndexOf(ALocation: string): integer;
var
  i: Integer;
begin
  for i := 0 to 255 do
    if CompareText(GetLocationName(i),ALocation)=0 then exit(i);
  exit(-1);
end;

function TDefaultMapInfo.TrigStringAllocate(AText: string): integer;
begin
  result := 0;
  raise exception.Create('Not implemented');
end;

procedure TDefaultMapInfo.TrigStringRelease(AIndex: integer);
begin
  raise exception.Create('Not implemented');
end;

function TDefaultMapInfo.MapStringRead(AIndex: integer): string;
begin
  result := '';
  raise exception.Create('Not implemented');
end;

function TDefaultMapInfo.MapStringIndexOf(AText: string): integer;
begin
  result := 0;
  raise exception.Create('Not implemented');
end;

function TDefaultMapInfo.SoundFilenameExists(AFilename: string): boolean;
begin
  result := false;
  raise exception.Create('Not implemented');
end;

end.

