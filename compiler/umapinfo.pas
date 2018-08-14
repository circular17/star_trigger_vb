unit umapinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  AnywhereLocationIndex = 63;

var
  Force : array[1..4] of string = ('Force 1', 'Force 2', 'Force 3', 'Force 4');
  SwitchNames : array[1..256] of string;
  LocationNames: array[0..255] of string;

function IsAnywhere(ALocation: string): boolean;
function GetSwitchName(ASwitch: integer): string;
function GetAnywhereLocation: string;
procedure SetAnywhereLocation(AName: string);
function LocationIndexOf(AName: string): integer;

implementation

function IsAnywhere(ALocation: string): boolean;
begin
  result := (ALocation = '') or (CompareText(ALocation, GetAnywhereLocation)=0);
end;

function GetSwitchName(ASwitch: integer): string;
begin
  if (ASwitch < low(SwitchNames)) or (ASwitch > high(SwitchNames)) then
    raise exception.Create('Index out of bounds');
  if SwitchNames[ASwitch] = '' then result := 'Switch'+IntToStr(ASwitch)
  else result := SwitchNames[ASwitch];
end;

function GetAnywhereLocation: string;
begin
  result := LocationNames[AnywhereLocationIndex];
end;

procedure SetAnywhereLocation(AName: string);
begin
  LocationNames[AnywhereLocationIndex] := AName;
end;

function LocationIndexOf(AName: string): integer;
var
  i: Integer;
begin
  for i := low(LocationNames) to high(LocationNames) do
    if LocationNames[i] = AName then exit(i);
  exit(-1);
end;

initialization

  SetAnywhereLocation('Anywhere');

end.

