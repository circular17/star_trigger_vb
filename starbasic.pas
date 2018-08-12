library starbasic;

{$mode objfpc}{$H+}

uses
  Classes, uplugin, uscmdrafttypes, umainform, Windows, uplugintypes;

var
  hSCMD2MainWindow: HWND;
  {%H-}hSCMD2Instance: THandle;
  scmd2_malloc: TAllocRamProc;
  scmd2_free: TDeAllocRamProc;
  scmd2_realloc: TReAllocRamProc;

function GetPluginVersion: DWord; stdcall;
begin
  result := 2;
end;

function InitPlugin( MainWindow: HWND; Instance: THandle;
         AllocMem: TAllocRamProc; DeleteMem: TDeAllocRamProc;
         ResizeMem: TReAllocRamProc; RequestedSections: PRequestedMenuSections): LongBool; stdcall;
begin
  hSCMD2MainWindow := MainWindow;
  hSCMD2Instance := Instance;

  scmd2_malloc := AllocMem;
  scmd2_free := DeleteMem;
  scmd2_realloc := ResizeMem;

  result := InitMyPlugin(RequestedSections);
end;

function PluginGetMenuString(Section: TMenuSection; MenuStr: PChar; StrLen: Word): LongBool; stdcall;
var
  s: String;
begin
  s := GetMyPluginMenu(Section);
  if length(s) > StrLen then exit(false) else
    begin
      fillchar(MenuStr^, StrLen, 0);
      if s <> '' then
        move(s[1], MenuStr^, length(s));
      exit(true);
    end;
end;

function RunPlugin(EngineData: PEngineData; CurSection: TMenuSection;
                   Triggers, MissionBriefing, SwitchRenaming,
                   UnitProperties, UnitPropUsage: PChunkData): LongBool; stdcall;
var
  Context: TPluginContext;
begin
  if (Triggers = nil) or (MissionBriefing = nil) or (SwitchRenaming = nil) or (UnitProperties = nil) or (UnitPropUsage = nil) then exit(false);

  EnableWindow(hSCMD2MainWindow, False);
  Context.Section := CurSection;
  Context.EngineData := EngineData;
  Context.Triggers := Triggers;
  Context.MissionBriefing := MissionBriefing;
  Context.SwitchRenaming := SwitchRenaming;
  Context.UnitProperties := UnitProperties;
  Context.UnitPropUsage := UnitPropUsage;
  result := RunMyPlugin(Context);
  EnableWindow(hSCMD2MainWindow, true);
end;

exports GetPluginVersion, InitPlugin, PluginGetMenuString, RunPlugin;

begin
end.

