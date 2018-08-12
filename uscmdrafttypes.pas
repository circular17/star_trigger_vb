unit uscmdrafttypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMenuSection = array[0..3] of char;
  TRequestedMenuSections = array[0..7] of TMenuSection;
  PRequestedMenuSections = ^TRequestedMenuSections;

  TAllocRamProc = function(Size: DWord): Pointer; stdcall;
  TDeAllocRamProc = procedure(Ram: Pointer); stdcall;
  TReAllocRamProc = function(Ram: Pointer; Size: DWord): Pointer; stdcall;

const
  MIN_LOCATION = 0;
  MAX_LOCATION = 255;
  ANYWHERE_LOCATION = 63;
  MIN_FORCE = 1;
  MAX_FORCE = 4;
  MIN_UNIT_TYPE = 0;
  MAX_UNIT_TYPE = 227;
  MIN_WAV = 0;
  MAX_WAV = 511;
  MIN_SWITCH = 1;
  MAX_SWITCH = 256;

type
  TLocationNode = packed record
    x0,y0,x1,y1: DWord;
    NameIndex,Elevation: Word;
    Used: byte;
  end;
  TLocationNodes = packed array[MIN_LOCATION..MAX_LOCATION] of TLocationNode;
  PLocationNodes = ^TLocationNodes;

  TUnitNames = array[MIN_UNIT_TYPE..MAX_UNIT_TYPE] of PChar;
  PStandardUnitNames = ^TUnitNames;

  TForceNames = array[MIN_FORCE..MAX_FORCE] of LongInt;
  PForceNames = ^TForceNames;
  TCustomUnitNames = array[MIN_UNIT_TYPE..MAX_UNIT_TYPE] of LongInt;
  PCustomUnitNames = ^TCustomUnitNames;

  TWavFilenames = array[MIN_WAV..MAX_WAV] of LongInt;
  PWavFilenames = ^TWavFilenames;

  TSwitchRenamingData = array[MIN_SWITCH..MAX_SWITCH] of LongInt;
  PSwitchRenamingData = ^TSwitchRenamingData;

  PSCStringList_VirtualCalls = ^TSCStringList_VirtualCalls;
  TSCStringList_VirtualCalls = record
        filler1: array[1..3] of DWord; FindString_RawIndexProc: pointer;
        GetStringProc, UnknownProc1, AddSCMD2StringProc, DereferenceProc : pointer;
        DerefAndAddStringProc, SetSCMD2Text, UnknownProc2, GetTotalStringNum : Pointer;
        UnknownProc3, UnknownProc4, UnknownProc5, BackupStringsProc: Pointer;
        RestoreBackupProc, ClearBackupProc: Pointer;
      end;

  { TSCStringList }

  PSCStringList = ^TSCStringList;
  TSCStringList = object
    VirtualCalls: PSCStringList_VirtualCalls;
    function GetString(AIndex: LongInt): string;
    function IndexOf(AText: string): integer;
    function GetCapacity: integer;
  end;

  PEngineData = ^TEngineData;

  { TEngineData }

  TEngineData = object
    Size: Word;
    StatTxt, MapStrings: PSCStringList;
    MapInternalStrings: Pointer;
    MapLocations: PLocationNodes;
    WavFilenames: PWavFilenames;
    ActionLog: Pointer;
    ActionLogLevel: Word;
    DataInterface: pointer;
    CurSelLocation: PLongInt;
    CustomUnitNames: PCustomUnitNames;
    ForceNames: PForceNames;
    StandardUnitNames: PStandardUnitNames;
    function GetLocationName(AIndex: integer): string;
    function GetLocationIndex(AName: string): integer;
    function LocationExists(AIndex: integer): boolean;
    function GetForceName(AForce: integer): string;
    function GetStandardUnitName(AIndex: integer): string;
    function GetCustomUnitName(AIndex: integer): string;
    function GetWavFilename(AIndex: integer): string;
    function GetWavIndex(AFilename: string): integer;
  end;

type
  PChunkData = ^TChunkData;
  TChunkData = record
    Size: DWord;
    Data: Pointer;
  end;

implementation

{ TEngineData }

function TEngineData.GetLocationName(AIndex: integer): string;
begin
  with MapLocations^[AIndex] do
    result := MapStrings^.GetString(NameIndex)
end;

function TEngineData.GetLocationIndex(AName: string): integer;
var
  i, idxStr: Integer;
begin
  idxStr := MapStrings^.IndexOf(AName);
  if idxStr > 0 then
  begin
    for i := MIN_LOCATION to MAX_LOCATION do
      if MapLocations^[i].NameIndex = idxStr then exit(i);
  end;
  exit(-1);
end;

function TEngineData.LocationExists(AIndex: integer): boolean;
begin
  result := MapLocations^[AIndex].NameIndex<>0;
end;

function TEngineData.GetForceName(AForce: integer): string;
begin
  result := MapStrings^.GetString(ForceNames^[AForce]);
end;

function TEngineData.GetStandardUnitName(AIndex: integer): string;
begin
  result := StandardUnitNames^[AIndex];
end;

function TEngineData.GetCustomUnitName(AIndex: integer): string;
begin
  result := MapStrings^.GetString(CustomUnitNames^[AIndex]);
end;

function TEngineData.GetWavFilename(AIndex: integer): string;
begin
  result := MapStrings^.GetString(WavFilenames^[AIndex]);
end;

function TEngineData.GetWavIndex(AFilename: string): integer;
var
  idxStr, i: Integer;
begin
  idxStr := MapStrings^.IndexOf(AFilename);
  for i := MIN_WAV to MAX_WAV do
    if WavFilenames^[i] = idxStr then exit(i);
  exit(-1);
end;

{$asmmode intel}

{ TSCStringList }

function TSCStringList.GetString(AIndex: LongInt): string;
var p: PChar;
begin
  if AIndex = 0 then exit('');
  asm
    mov eax, AIndex
    mov ecx, self
    push eax
    mov eax, [ecx + TSCStringList.VirtualCalls]
    mov eax, [eax + TSCStringList_VirtualCalls.GetStringProc]
    call eax
    mov p, eax
  end;
  result := p;
end;

function TSCStringList.IndexOf(AText: string): integer;
begin
  if AText = '' then exit(0);
  asm
    mov eax, AText
    mov ecx, self
    push eax
    mov eax, [ecx + TSCStringList.VirtualCalls]
    mov eax, [eax + TSCStringList_VirtualCalls.FindString_RawIndexProc]
    call eax
    mov result, eax
  end;
  if result >= 0 then inc(result);
end;

function TSCStringList.GetCapacity: integer;
begin
  asm
    mov ecx, self
    mov eax, [ecx + TSCStringList.VirtualCalls]
    mov eax, [eax + TSCStringList_VirtualCalls.GetTotalStringNum]
    call eax
    mov result, eax
  end;
end;


end.

