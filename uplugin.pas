unit uplugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uscmdrafttypes, uplugintypes;

function InitMyPlugin(RequestedSections: PRequestedMenuSections): boolean;

function GetMyPluginMenu(Section: TMenuSection): string;

function RunMyPlugin(const Context: TPluginContext): boolean;

implementation

uses Forms, Interfaces, Windows, umainform, uinstructions;

function InitMyPlugin(RequestedSections: PRequestedMenuSections): boolean;
begin
  RequestedSections^[0] := 'GIRT';
  Application.Initialize;
  result := true;
end;

function GetMyPluginMenu(Section: TMenuSection): string;
begin
  if Section = 'GIRT' then
    result := PluginMenu
  else
    result := '?';
end;

function RunMyPlugin(const Context: TPluginContext): boolean;

{  procedure DumpChunk(AChunk: PChunkData; AName: string);
  var
    dump: file;
  begin
    assignfile(dump,'c:\scmdraft2\'+AName+'.dat');
    rewrite(dump,1);
    BlockWrite(dump, AChunk^.Data^, AChunk^.Size);
    closefile(dump);
  end;}

var
  fMain: TFMain;
  i: Integer;
begin
  if Context.Section = 'GIRT' then
  begin
{    DumpChunk(Context.Triggers, 'triggers');
    DumpChunk(Context.UnitProperties, 'unitprop');
    DumpChunk(Context.UnitPropUsage, 'unitpropuse');}

    fMain := TFMain.Create(nil);
    try
      fMain.Position := poDefault;
      fMain.ClearLocations;
      AnywhereLocation := Context.GetLocationName(ANYWHERE_LOCATION);
      fMain.AddLocation('Anywhere', False);
      for i := MIN_LOCATION to MAX_LOCATION do
        if (i <> ANYWHERE_LOCATION) and Context.LocationExists(i) then
          fMain.AddLocation(Context.GetLocationName(i), True);
      fMain.ShowModal;
    except
      on ex: Exception do
        MessageBox(0, pchar(ex.Message), 'Error', 0);
    end;
    fMain.Free;
    result := true;
  end
  else
    result := false;
end;

end.

