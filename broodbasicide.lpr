program broodbasicide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  umainform,
  umapinfo;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  MapInfo := TDefaultMapInfo.Create;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
  MapInfo.Free;
end.

