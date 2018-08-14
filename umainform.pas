unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterVB, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls;

type

  { TFMain }

  TFMain = class(TForm)
    Compile: TAction;
    FileSaveAs: TAction;
    FileSave: TAction;
    FileOpen: TAction;
    CancelChanges: TAction;
    Errors: TLabel;
    Label1: TLabel;
    ListBox_Locations: TListBox;
    ListBox_Errors: TListBox;
    OpenDialog1: TOpenDialog;
    Panel_Locations: TPanel;
    Panel_Utils: TPanel;
    Panel_Code: TPanel;
    Panel_Errors: TPanel;
    SaveDialog1: TSaveDialog;
    CompileDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    SynVBSyn1: TSynVBSyn;
    Timer1: TTimer;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ValidateChanges: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure CancelChangesExecute(Sender: TObject);
    procedure CancelChangesUpdate(Sender: TObject);
    procedure CompileExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox_ErrorsDblClick(Sender: TObject);
    procedure ListBox_LocationsDblClick(Sender: TObject);
    procedure SynCompletion1SearchPosition(var APosition: integer);
    procedure SynEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ValidateChangesExecute(Sender: TObject);
    procedure ValidateChangesUpdate(Sender: TObject);
  private
    function GetModified: boolean;
    procedure SetCurFilename(AValue: string);
    procedure SetModified(AValue: boolean);
    procedure UpdateTitleBar;
    procedure UpdateErrors;

  public
    AllCompletion: TStringList;
    ErrorsToUpdate, HasErrors: boolean;
    FCurFilename: string;
    procedure ClearLocations;
    procedure UpdateAutoCompleteList;
    procedure AddLocation(AName: string; AIsText: boolean);
    property CurFilename: string read FCurFilename write SetCurFilename;
    property Modified: boolean read GetModified write SetModified;
  end;

var
  FMain: TFMain;

implementation

uses ureadprog, uparsevb, uvariables, usctypes, umapinfo, uwritetriggers,
  utrigeditoutput;

{$R *.lfm}

{ TFMain }
procedure TFMain.ValidateChangesExecute(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TFMain.ValidateChangesUpdate(Sender: TObject);
begin
  ValidateChanges.Visible := MapInfo.ProgramMapEmbedded;
end;

function TFMain.GetModified: boolean;
begin
  result := SynEdit1.Modified;
end;

procedure TFMain.SetCurFilename(AValue: string);
begin
  if FCurFilename=AValue then Exit;
  FCurFilename:=AValue;
  UpdateTitleBar;
end;

procedure TFMain.SetModified(AValue: boolean);
begin
  if SynEdit1.Modified <> AValue then
  begin
    SynEdit1.Modified := AValue;
    UpdateTitleBar;
  end;
end;

procedure TFMain.UpdateTitleBar;
var modif: string;
begin
  if Modified then modif := '*' else modif := '';
  if CurFilename = '' then
    Caption := 'BroodBasic program'+modif
  else
    Caption := 'BroodBasic - ' +CurFilename+modif;
end;

procedure TFMain.UpdateErrors;
var
  i: Integer;
  MainThread: TPlayer;
  success: Boolean;
begin
  ErrorsToUpdate := false;

  //full program for validation
  success := ureadprog.ReadProg(SynEdit1.Lines, MainThread);
  if success then
  begin
    if MainThread = plNone then MainThread := plPlayer8;
    try
      if MapInfo.ProgramMapEmbedded then
        uwritetriggers.CreateTriggers(MainThread, SynEdit1.Text)
      else
        uwritetriggers.CreateTriggers(MainThread, '')
    except on ex:exception do
      ReadProgErrors.Add(ex.Message);
    end;
  end;

  //update error list
  ListBox_Errors.Items.BeginUpdate;
  for i := 0 to ReadProgErrors.Count-1 do
    if ListBox_Errors.Items.IndexOf(ReadProgErrors[i])=-1 then
      ListBox_Errors.Items.Add(ReadProgErrors[i]);
  for i := ListBox_Errors.Items.Count-1 downto 0 do
    if ReadProgErrors.IndexOf(ListBox_Errors.Items[i])=-1 then
      ListBox_Errors.Items.Delete(i);
  ListBox_Errors.Items.EndUpdate;

  HasErrors := ReadProgErrors.Count>0;
end;

procedure TFMain.ClearLocations;
begin
  ListBox_Locations.Items.Clear;
end;

procedure TFMain.UpdateAutoCompleteList;
var
  programUpToCursor: TStringList;
  i: Integer;
  MainThread, pl: TPlayer;
begin
  //partial program for autocompletion
  programUpToCursor:= TStringList.Create;
  for i := 1 to SynEdit1.CaretY do
    if i <= SynEdit1.Lines.Count then
      programUpToCursor.Add(SynEdit1.Lines[i-1]);
  ureadprog.ReadProg(programUpToCursor, MainThread);
  programUpToCursor.Free;

  AllCompletion.Clear;
  for i := 0 to High(ImplementedReservedWords) do
    AllCompletion.Add(ImplementedReservedWords[i]);
  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Scope = GlobalScope) or (IntVars[i].Scope = LastScope) then
      AllCompletion.Add(IntVars[i].Name);
  for i := 0 to BoolArrayCount-1 do
    if (BoolVars[i].Scope = GlobalScope) or (BoolVars[i].Scope = LastScope) then
      AllCompletion.Add(BoolVars[i].Name);
  for i := 0 to IntArrayCount-1 do
    if (IntArrays[i].Scope = GlobalScope) or (IntArrays[i].Scope = LastScope) then
      AllCompletion.Add(IntArrays[i].Name);
  for i := 0 to BoolArrayCount-1 do
    if (BoolArrays[i].Scope = GlobalScope) or (BoolArrays[i].Scope = LastScope) then
      AllCompletion.Add(BoolArrays[i].Name);
  for i := 0 to UnitPropCount-1 do
    if (UnitPropVars[i].Scope = GlobalScope) or (UnitPropVars[i].Scope = LastScope) then
      AllCompletion.Add(UnitPropVars[i].Name);
  for i := 0 to SoundCount-1 do
    if (SoundVars[i].Scope = GlobalScope) or (SoundVars[i].Scope = GlobalScope) then
      AllCompletion.Add(SoundVars[i].Name);
  for i := 0 to StringCount-1 do
    if (StringVars[i].Scope = GlobalScope) or (StringVars[i].Scope = GlobalScope) then
      AllCompletion.Add(StringVars[i].Name);
  for pl := low(TPlayer) to high(TPlayer) do
    if PlayerIdentifiers[pl]<>'' then
      AllCompletion.Add(PlayerIdentifiers[pl]);
end;

procedure TFMain.AddLocation(AName: string; AIsText: boolean);
begin
  if AIsText then AName := '"'+StringReplace(AName,'"','""',[rfReplaceAll])+'"';
  ListBox_Locations.Items.Add(AName);
end;

procedure TFMain.CancelChangesExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFMain.CancelChangesUpdate(Sender: TObject);
begin
  CancelChanges.Visible := MapInfo.ProgramMapEmbedded;
end;

procedure TFMain.CompileExecute(Sender: TObject);
begin
  If ErrorsToUpdate then UpdateErrors;
  if HasErrors then
    ShowMessage('There are errors in the program so that triggers cannot be generated')
  else
  begin
    if not MapInfo.ProgramMapEmbedded then
    begin
      if CurFilename = '' then
      begin
        ShowMessage('Please save your file first');
        exit;
      end else
        FileSave.Execute;

      if CompileDialog1.InitialDir = '' then
        CompileDialog1.InitialDir:= ExtractFilePath(CurFilename);
      CompileDialog1.FileName := ChangeFileExt(ExtractFilename(CurFilename),'.trigger');
      if CompileDialog1.Execute then
      begin
        try
          WriteTrigEditCode(CompileDialog1.FileName);
          WriteTrigEditUnitProperties(ChangeFileExt(CompileDialog1.FileName,'.properties'));
          CompileDialog1.InitialDir := ExtractFilePath(CompileDialog1.FileName);
        except
          on ex:exception do
            ShowMessage(ex.Message);
        end;
      end;
    end;
  end;
end;

procedure TFMain.FileOpenExecute(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(CurFilename);
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then
  begin
    try
      SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
      CurFilename := OpenDialog1.FileName;
      Modified := false;
      ErrorsToUpdate:= true;
    except
      on ex: Exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TFMain.FileSaveAsExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(CurFilename);
  SaveDialog1.FileName := ExtractFileName(CurFilename);
  if SaveDialog1.Execute then
  begin
    try
      SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
      CurFilename := SaveDialog1.FileName;
      Modified := false;
    except
      on ex: Exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TFMain.FileSaveExecute(Sender: TObject);
begin
  If CurFilename = '' then FileSaveAs.Execute else
    begin
      try
        SynEdit1.Lines.SaveToFile(Curfilename);
        Modified := false;
      except
        on ex: Exception do
          ShowMessage(ex.Message);
      end;
    end;
end;

procedure TFMain.FileSaveUpdate(Sender: TObject);
begin
  FileSave.Enabled := (CurFilename <> '') and Modified;
end;

procedure TFMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AllCompletion := TStringList.Create;

  AddLocation('Anywhere', False);
  for i := LocationMinIndex to LocationMaxIndex do
    if (i <> AnywhereLocationIndex) and MapInfo.LocationExists(i) then
      fMain.AddLocation(MapInfo.LocationName[i], True);
  Modified := false;

  ErrorsToUpdate:= true;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  AllCompletion.Free;
end;

procedure TFMain.ListBox_ErrorsDblClick(Sender: TObject);
var lineNumber,errPos: integer;
  err: String;
begin
  if ListBox_Errors.ItemIndex <> -1 then
  begin
    err := ListBox_Errors.Items[ListBox_Errors.ItemIndex];
    if copy(err,1,5) = 'Line ' then
    begin
      val(copy(err,6,pos(':',err)-6), lineNumber, errPos);
      if errPos = 0 then
      begin
        SynEdit1.CaretY := lineNumber;
        SynEdit1.SetFocus;
      end;
    end;
  end;
end;

procedure TFMain.ListBox_LocationsDblClick(Sender: TObject);
begin
  if ListBox_Locations.ItemIndex <> -1 then
  begin
    SynEdit1.InsertTextAtCaret(ListBox_Locations.Items[ListBox_Locations.ItemIndex]);
    SynEdit1.SetFocus;
  end;
end;

procedure TFMain.SynCompletion1SearchPosition(var APosition: integer);
var
  i: Integer;
  cur: String;
begin
  UpdateAutocompleteList;

  cur := SynCompletion1.CurrentString;
  SynCompletion1.ItemList.Clear;
  for i := 0 to AllCompletion.Count-1 do
    if CompareText(cur, copy(AllCompletion[i],1,length(cur)))= 0 then
      SynCompletion1.ItemList.Add(AllCompletion[i]);
  if AllCompletion.Count >0 then APosition:= 0
    else APosition:= -1;
end;

procedure TFMain.SynEdit1Change(Sender: TObject);
begin
  ErrorsToUpdate:= true;
end;

procedure TFMain.Timer1Timer(Sender: TObject);
begin
  if ErrorsToUpdate then
  begin
    UpdateErrors;
    UpdateTitleBar;
  end;
end;

end.

