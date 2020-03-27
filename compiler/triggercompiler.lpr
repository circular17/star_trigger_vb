program triggercompiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  usctypes, ureadprog, umapinfo, uwritetriggers, utrigeditoutput;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
const testPath = 'test'+PathDelim;
  outputPath = 'output'+PathDelim;
var
  ErrorMsg: String;
  MainThread: TPlayer;
  success: Boolean;
  i, lastScope: Integer;
  search: TRawByteSearchRec;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  MapInfo := TDefaultMapInfo.Create;
  try
    if FindFirst(testPath+'*.vb', faAnyFile, search) = 0 then
    repeat
      writeln('Reading ',search.name);
      success := ureadprog.ReadProg(testPath+search.Name, MainThread, lastScope);
      for i := 0 to ReadProgErrors.Count-1 do
        writeln('Error: ', ReadProgErrors[i]);
      for i := 0 to ReadProgWarnings.Count-1 do
        writeln('Warning: ', ReadProgWarnings[i]);

      if success then
      begin
        writeln('Writing output');
        if MainThread = plNone then MainThread := plPlayer8;

        CreateTriggers(MainThread);
        if not DirectoryExists(outputPath) then
          CreateDir(outputPath);
        WriteTrigEditCode(outputPath+ChangeFileExt(search.name,'.trigger'));
        WriteTrigEditUnitProperties(outputPath+ChangeFileExt(search.name,'.properties'));
      end else
        break;
    until FindNext(search) <> 0;
    FindClose(search);
  finally
    FreeAndNil(MapInfo);
  end;

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

