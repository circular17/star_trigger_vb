unit uplugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uscmdrafttypes, uplugintypes;

function InitMyPlugin(RequestedSections: PRequestedMenuSections): boolean;

function GetMyPluginMenu(Section: TMenuSection): string;

function RunMyPlugin(const AContext: TPluginContext): boolean;

implementation

uses Forms, Interfaces, Windows, umainform, umapinfo;

function InitMyPlugin(RequestedSections: PRequestedMenuSections): boolean;
begin
  RequestedSections^[0] := SectionCodeToLongWord(PluginMenuSection);
  Application.Initialize;
  result := true;
end;

function GetMyPluginMenu(Section: TMenuSection): string;
begin
  if Section = SectionCodeToLongWord(PluginMenuSection) then
    result := PluginMenu
  else
    result := '?';
end;

function RunMyPlugin(const AContext: TPluginContext): boolean;

{  procedure DumpChunk(AChunk: PChunkData; AName: string);
  var
    dump: file;
  begin
    assignfile(dump,'c:\scmdraft2\'+AName+'.dat');
    rewrite(dump,1);
    BlockWrite(dump, AChunk^.Data^, AChunk^.Size);
    closefile(dump);
  end; }

var
  fMain: TFMain;
begin
  if AContext.Section = SectionCodeToLongWord(PluginMenuSection) then
  begin
 {   DumpChunk(AContext.Triggers, 'triggers');
    DumpChunk(AContext.UnitPropertiesList, 'unitprop');
    DumpChunk(AContext.UnitPropUsage, 'unitpropuse');   }

    MapInfo := TPluginMapInfo.Create(AContext);
    try
      fMain := TFMain.Create(nil);
      try
        fMain.Position := poDefault;
        fMain.ShowModal;
      except
        on ex: Exception do
          MessageBox(0, pchar(ex.Message), 'Error', 0);
      end;
      fMain.Free;
    except
      on ex: Exception do
        MessageBox(0, pchar(ex.Message), 'Error', 0);
    end;

    FreeAndNil(MapInfo);
    result := true;
  end
  else
    result := false;
end;

end.

