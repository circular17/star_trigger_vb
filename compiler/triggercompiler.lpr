program triggercompiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ureadprog, uwritetriggers, uinstructions,
  utriggercode, uarithmetic, usctypes, uparsevb, uvariables, uexpressions,
  uparseconditions, utriggerinstructions, utriggerconditions, utrigedittypes,
  umapinfo, utriggerchunk
  { you can add units after this };

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
var
  ErrorMsg: String;
  MainThread: TPlayer;
  success: Boolean;
  i: Integer;
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
    success := ureadprog.ReadProg('prog.vb', MainThread);
    for i := 0 to ReadProgErrors.Count-1 do
      writeln(ReadProgErrors[i]);

    if success then
    begin
      if MainThread = plNone then MainThread := plPlayer8;
      uwritetriggers.WriteTriggers('prog.trigger', MainThread);
      uwritetriggers.WriteUnitProperties('prog.property');
    end;
  finally
    MapInfo.Free;
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

