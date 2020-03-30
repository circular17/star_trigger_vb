unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterVB, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls, Menus;

type

  { TFMain }

  TFMain = class(TForm)
    EditPaste: TAction;
    EditCopy: TAction;
    EditCut: TAction;
    Compile: TAction;
    FileSaveAs: TAction;
    FileSave: TAction;
    FileOpen: TAction;
    Errors: TLabel;
    Label1: TLabel;
    ListBox_Locations: TListBox;
    ListBox_Errors: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel_Locations: TPanel;
    Panel_Utils: TPanel;
    Panel_Code: TPanel;
    Panel_Errors: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    CompileDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    SynVBSyn1: TSynVBSyn;
    Timer1: TTimer;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    procedure CompileExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ListBox_ErrorsDblClick(Sender: TObject);
    procedure ListBox_LocationsDblClick(Sender: TObject);
    procedure SynCompletion1Execute(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function GetModified: boolean;
    procedure SetCurFilename(AValue: string);
    procedure SetModified(AValue: boolean);
    procedure UpdateTitleBar;
    procedure UpdateErrors;
    procedure TryOpenFile(AFilename: string);

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
  LastScope: integer;
begin
  ErrorsToUpdate := false;

  //full program for validation
  success := ureadprog.ReadProg(SynEdit1.Lines, MainThread, LastScope);

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

procedure TFMain.TryOpenFile(AFilename: string);
begin
  try
    SynEdit1.Lines.LoadFromFile(AFilename);
    CurFilename := AFilename;
    OpenDialog1.InitialDir := ExtractFilePath(AFilename);
    Modified := false;
    ErrorsToUpdate:= true;
  except
    on ex: Exception do
      ShowMessage(ex.Message);
  end;
end;

procedure TFMain.ClearLocations;
begin
  ListBox_Locations.Items.Clear;
end;

procedure TFMain.UpdateAutoCompleteList;
  procedure AddCompletion(AWords: array of string);
  var
    i: Integer;
  begin
    for i := 0 to high(AWords) do
      if AllCompletion.IndexOf(AWords[i])=-1 then
        AllCompletion.Add(AWords[i]);
  end;
var
  programUpToCursor: TStringList;
  i, LastScope: Integer;
  MainThread, pl: TPlayer;
  u: TStarcraftUnit;
  prevLine, lineUpToCursor: TStringList;
  isBeginLine: boolean;
  lastLineStr: String;

begin
  //partial program for autocompletion
  programUpToCursor:= TStringList.Create;
  for i := 1 to SynEdit1.CaretY-1 do
    if i <= SynEdit1.Lines.Count then
      programUpToCursor.Add(SynEdit1.Lines[i-1]);
  if SynEdit1.CaretY <= SynEdit1.Lines.Count then
  begin
    lastLineStr := copy(SynEdit1.Lines[SynEdit1.CaretY-1],1,SynEdit1.CaretX-1);
    if length(lastLineStr) = SynEdit1.CaretX-1 then
    while (lastLineStr <> '') and (lastLineStr[length(lastLineStr)] in ['a'..'z','A'..'Z','0'..'9','_']) and
      ((length(lastLineStr) = 1) or (lastLineStr[length(lastLineStr)] in ['a'..'z','A'..'Z','0'..'9','_'])) do
      delete(lastLineStr, length(lastLineStr), 1);
    programUpToCursor.Add(lastLineStr);
  end;
  ureadprog.ReadProg(programUpToCursor, MainThread, LastScope);
  programUpToCursor.Free;

  lineUpToCursor := nil;
  prevLine := nil;
  try
    if SynEdit1.CaretY <= SynEdit1.Lines.Count then
      lineUpToCursor := ParseLine(copy(SynEdit1.Lines[SynEdit1.CaretY-1],1,SynEdit1.CaretX-1))
    else
      lineUpToCursor := TStringList.Create;
    if (SynEdit1.CaretY-1 >= 0) and (SynEdit1.CaretY-1 <= SynEdit1.Lines.Count) then
      prevLine := ParseLine(SynEdit1.Lines[SynEdit1.CaretY-2])
    else
      prevLine := TStringList.Create;
  except
    if lineUpToCursor = nil then lineUpToCursor := TStringList.Create;
    if prevLine = nil then prevLine := TStringList.Create;
  end;
  if (lineUpToCursor.Count > 0) and IsValidVariableName(lineUpToCursor[lineUpToCursor.Count-1]) then
    lineUpToCursor.delete(lineUpToCursor.Count-1);
  isBeginLine:= (lineUpToCursor.Count = 0) and not ((prevLine.Count > 0) and IsTokenOverEndOfLine(prevLine[prevLine.Count-1]));

  AllCompletion.Clear;
  AllCompletion.AddStrings(uparsevb.ParseCompletionList);

  prevLine.Free;
  lineUpToCursor.Free;
  AllCompletion.Sort;
end;

procedure TFMain.AddLocation(AName: string; AIsText: boolean);
begin
  if AIsText then AName := '"'+StringReplace(AName,'"','""',[rfReplaceAll])+'"';
  ListBox_Locations.Items.Add(AName);
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
        ShowMessage('Please save your source code first');
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
    end else
    begin
      try
        if CurFilename <> '' then FileSave.Execute;
        MapInfo.UpdateTriggers;
        if CurFilename = '' then Modified:= false; //stored in map
        ShowMessage('Triggers have been updated. The source code has been stored in the map so that it will be retrieved next time you open this editor.');
        Close;
      except
        on ex:Exception do
          ShowMessage(ex.Message);
      end;
    end;
  end;
end;

procedure TFMain.EditCopyExecute(Sender: TObject);
begin
  SynEdit1.CopyToClipboard;
end;

procedure TFMain.EditCutExecute(Sender: TObject);
begin
  SynEdit1.CutToClipboard;
end;

procedure TFMain.EditPasteExecute(Sender: TObject);
begin
  SynEdit1.PasteFromClipboard;
end;

procedure TFMain.FileOpenExecute(Sender: TObject);
begin
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then TryOpenFile(OpenDialog1.FileName);
end;

procedure TFMain.FileSaveAsExecute(Sender: TObject);
begin
  if CurFilename <> '' then SaveDialog1.InitialDir := ExtractFilePath(CurFilename);
  SaveDialog1.FileName := ExtractFileName(CurFilename);
  if SaveDialog1.Execute then
  begin
    try
      SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
      CurFilename := SaveDialog1.FileName;
      SaveDialog1.InitialDir := ExtractFilePath(CurFilename);
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

procedure TFMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Modified then
  begin
    if MessageDlg('Closing','There are unsaved modifications. Would you like to close anyway?', mtConfirmation, mbYesNo, 0)<>mrYes then
      CanClose := false;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AllCompletion := TStringList.Create;

  ListBox_Locations.Items.BeginUpdate;
  AddLocation('Anywhere', False);
  for i := LocationMinIndex to LocationMaxIndex do
    if (i <> AnywhereLocationIndex) and MapInfo.LocationExists(i) then
      AddLocation(MapInfo.LocationName[i], True);
  ListBox_Locations.Items.EndUpdate;

  if MapInfo.ProgramMapEmbedded then
    SynEdit1.Text := MapInfo.RetrieveStoredProgram;

  SynEdit1.Font.Size := 12;

  Modified := false;

  ErrorsToUpdate:= false; //true;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  AllCompletion.Free;
end;

procedure TFMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  TryOpenFile(Filenames[0]);
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

procedure TFMain.SynCompletion1Execute(Sender: TObject);
var
  i: Integer;
  cur: String;
begin
  UpdateAutocompleteList;

  cur := LowerCase(SynCompletion1.CurrentString);
  SynCompletion1.ItemList.Clear;

  if length(cur) > 0 then cur[1] := upcase(cur[1]);
  for i := 0 to AllCompletion.Count-1 do
    if (CompareText(cur, copy(AllCompletion[i],1,length(cur)))= 0) or
      (pos(cur, AllCompletion[i])<>0) then
      SynCompletion1.ItemList.Add(AllCompletion[i]);
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

