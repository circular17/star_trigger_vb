unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterVB, SynCompletion,
  SynEditTypes, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ActnList, ExtCtrls, Menus, LCLType, usearch;

type

  { TFMain }

  TFMain = class(TForm)
    FileQuit: TAction;
    EditSearchPrevious: TAction;
    EditSearchNext: TAction;
    EditSearch: TAction;
    EditSearchReplace: TAction;
    EditUncomment: TAction;
    EditComment: TAction;
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
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure CompileExecute(Sender: TObject);
    procedure EditCommentExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditSearchExecute(Sender: TObject);
    procedure EditSearchNextExecute(Sender: TObject);
    procedure EditSearchPreviousExecute(Sender: TObject);
    procedure EditSearchReplaceExecute(Sender: TObject);
    procedure EditUncommentExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
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
    FPrevCaret: TPoint;
    FSearch : TFSearch;
    FSearchDefined : boolean;
    FCurFilename: string;
    FAllCompletion: TStringList;
    FHasErrors: boolean;
    FErrorsToUpdate: boolean;
    function GetModified: boolean;
    procedure SetCurFilename(AValue: string);
    procedure SetModified(AValue: boolean);
    procedure UpdateTitleBar;
    procedure UpdateErrors;
    procedure TryOpenFile(AFilename: string);
    procedure DoSearch(APrevious: boolean);

  public
    procedure ClearLocations;
    procedure UpdateAutoCompleteList;
    procedure AddLocation(AName: string; AIsText: boolean);
    procedure InvalidateErrors;
    property CurFilename: string read FCurFilename write SetCurFilename;
    property Modified: boolean read GetModified write SetModified;
    property HasErrors: boolean read FHasErrors;
  end;

var
  FMain: TFMain;

implementation

uses ureadprog, uparsevb, uvariables, usctypes, umapinfo, uwritetriggers,
  utrigeditoutput, math;

{$R *.lfm}

const
  DefaultMainThread = plPlayer8;

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
  FErrorsToUpdate := false;

  //full program for validation
  success := ureadprog.ReadProg(SynEdit1.Lines, MainThread, LastScope, DefaultMainThread);

  if success then
  begin
    try
      if MapInfo.ProgramMapEmbedded then
        uwritetriggers.CreateTriggers(MainThread, SynEdit1.Text)
      else
        uwritetriggers.CreateTriggers(MainThread, '');
      uvariables.CompileUnitProperties;
    except on ex:exception do
      ReadProgErrors.Add(ex.Message);
    end;
  end;

  //update error list
  ListBox_Errors.Items.BeginUpdate;
  for i := 0 to ReadProgErrors.Count-1 do
    if ListBox_Errors.Items.IndexOf(ReadProgErrors[i])=-1 then
      ListBox_Errors.Items.Add(ReadProgErrors[i]);
  for i := 0 to ReadProgWarnings.Count-1 do
    if ListBox_Errors.Items.IndexOf(ReadProgWarnings[i])=-1 then
      ListBox_Errors.Items.Add(ReadProgWarnings[i]);
  for i := ListBox_Errors.Items.Count-1 downto 0 do
    if (ReadProgErrors.IndexOf(ListBox_Errors.Items[i])=-1) and
       (ReadProgWarnings.IndexOf(ListBox_Errors.Items[i])=-1) then
      ListBox_Errors.Items.Delete(i);
  ListBox_Errors.Items.EndUpdate;

  FHasErrors := ReadProgErrors.Count>0;
end;

procedure TFMain.TryOpenFile(AFilename: string);
begin
  try
    SynEdit1.Lines.LoadFromFile(AFilename);
    CurFilename := AFilename;
    OpenDialog1.InitialDir := ExtractFilePath(AFilename);
    Modified := false;
    InvalidateErrors;
  except
    on ex: Exception do
      ShowMessage(ex.Message);
  end;
end;

procedure TFMain.DoSearch(APrevious: boolean);
var
  options: TSynSearchOptions;
begin
  if not FSearchDefined then exit;
  options := [];
  if FSearch.CheckBox_CaseSensitive.Checked then options += [ssoMatchCase];
  if FSearch.CheckBox_WholeWord.Checked then options += [ssoWholeWord];
  if FSearch.CheckBox_RegExp.Checked then options += [ssoRegExpr];
  if FSearch.CheckBox_Backwards.Checked xor APrevious then options += [ssoBackwards];
  if FSearch.CheckBox_Replace.Checked then options += [ssoReplace];
  if ssoBackwards in options then
    SynEdit1.LogicalCaretXY := SynEdit1.BlockBegin
    else SynEdit1.LogicalCaretXY := SynEdit1.BlockEnd;
  SynEdit1.SearchReplace(FSearch.Edit_SearchFor.Text, FSearch.Edit_ReplaceBy.Text, options);
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
      if FAllCompletion.IndexOf(AWords[i])=-1 then
        FAllCompletion.Add(AWords[i]);
  end;
var
  programUpToCursor: TStringList;
  i, LastScope: Integer;
  MainThread: TPlayer;
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
  try
    ureadprog.ReadProg(programUpToCursor, MainThread, LastScope, DefaultMainThread);
  except
    //ignore
  end;
  programUpToCursor.Free;

  FAllCompletion.Clear;
  FAllCompletion.AddStrings(uparsevb.ParseCompletionList);
  FAllCompletion.Sort;
end;

procedure TFMain.AddLocation(AName: string; AIsText: boolean);
begin
  if AIsText then AName := '"'+StringReplace(AName,'"','""',[rfReplaceAll])+'"';
  ListBox_Locations.Items.Add(AName);
end;

procedure TFMain.InvalidateErrors;
begin
  FErrorsToUpdate:= true;
end;

procedure TFMain.CompileExecute(Sender: TObject);
begin
  If FErrorsToUpdate then UpdateErrors;
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
        MapInfo.UpdateUnitProperties;
        if CurFilename = '' then Modified:= false; //stored in map
        Close;
      except
        on ex:Exception do
          ShowMessage(ex.Message);
      end;
    end;
  end;
end;

procedure TFMain.EditCommentExecute(Sender: TObject);
  procedure ApplyOnRow(AIndex: integer);
  var
    s: String;
  begin
    s := SynEdit1.Lines[AIndex-1];
    if trim(s) <> '' then
      SynEdit1.Lines[AIndex-1] := '''' + s;
  end;
var
  i: Integer;
begin
  if not SynEdit1.SelAvail then
    ApplyOnRow(SynEdit1.CaretY)
  else
    for i := min(SynEdit1.BlockBegin.y,SynEdit1.BlockEnd.y) to
             max(SynEdit1.BlockBegin.y,SynEdit1.BlockEnd.y) do
      ApplyOnRow(i);
  InvalidateErrors;
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

procedure TFMain.EditSearchExecute(Sender: TObject);
begin
  if not Assigned(FSearch) then
    FSearch := TFSearch.Create(self);
  FSearch.CheckBox_Replace.Checked := false;
  if FSearch.ShowModal = mrOK then
  begin
    FSearchDefined := true;
    EditSearchNext.Execute;
  end;
end;

procedure TFMain.EditSearchNextExecute(Sender: TObject);
begin
  DoSearch(false);
end;

procedure TFMain.EditSearchPreviousExecute(Sender: TObject);
begin
  DoSearch(true);
end;

procedure TFMain.EditSearchReplaceExecute(Sender: TObject);
begin
  if not Assigned(FSearch) then
    FSearch := TFSearch.Create(self);
  FSearch.CheckBox_Replace.Checked := true;
  if FSearch.ShowModal = mrOK then
  begin
    FSearchDefined := true;
    EditSearchNext.Execute;
  end;
end;

procedure TFMain.EditUncommentExecute(Sender: TObject);
  procedure ApplyOnRow(AIndex: integer);
  var
    s: String;
    p: Integer;
  begin
    s := SynEdit1.Lines[AIndex-1];
    p := 1;
    while (p <= length(s)) and (s[p] in[' ',#9]) do inc(p);
    if (p <= length(s)) and (s[p] = '''') then
    begin
      delete(s, p, 1);
      SynEdit1.Lines[AIndex-1] := s;
    end;
  end;
var
  i: Integer;
begin
  if not SynEdit1.SelAvail then
    ApplyOnRow(SynEdit1.CaretY)
  else
    for i := min(SynEdit1.BlockBegin.y,SynEdit1.BlockEnd.y) to
             max(SynEdit1.BlockBegin.y,SynEdit1.BlockEnd.y) do
      ApplyOnRow(i);
  InvalidateErrors;
end;

procedure TFMain.FileOpenExecute(Sender: TObject);
begin
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then TryOpenFile(OpenDialog1.FileName);
end;

procedure TFMain.FileQuitExecute(Sender: TObject);
begin
  Close;
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
  FAllCompletion := TStringList.Create;

  ListBox_Locations.Items.BeginUpdate;
  AddLocation('Anywhere', False);
  for i := LocationMinIndex to LocationMaxIndex do
    if (i <> AnywhereLocationIndex) and MapInfo.LocationExists(i) then
      AddLocation(MapInfo.LocationName[i], True);
  ListBox_Locations.Items.EndUpdate;

  SynEdit1.Font.Size := 12;
  Modified := false;

  if MapInfo.ProgramMapEmbedded then
    SynEdit1.Text := MapInfo.RetrieveStoredProgram;

  UpdateErrors;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FAllCompletion.Free;
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
  for i := 0 to FAllCompletion.Count-1 do
    if (CompareText(cur, copy(FAllCompletion[i],1,length(cur)))= 0) or
      (pos(cur, FAllCompletion[i])<>0) then
      SynCompletion1.ItemList.Add(FAllCompletion[i]);
  if ((SynCompletion1.Position < 0) or (SynCompletion1.Position >= SynCompletion1.ItemList.Count))
     and (SynCompletion1.ItemList.Count > 0) then
    SynCompletion1.Position := 0;
end;

procedure TFMain.SynEdit1Change(Sender: TObject);
const IdOrNumber : set of char = ['A'..'Z','a'..'z','_','0'..'9'];
  function IsIdOrNumber(AText: string; APos: integer): boolean;
  begin
    if (APos < 1) or (APos > length(AText)) then exit(false)
    else exit(AText[APos] in IdOrNumber);
  end;

  function FixCase(var AText: string; AIndexEnd: integer; out ADeltaX: integer): boolean;
  var
    indexStart, i: Integer;
    w: string;
    inStr: boolean;
  begin
    ADeltaX := 0;
    if not IsIdOrNumber(AText, AIndexEnd) then exit(false);

    inStr := false;
    for i := 1 to AIndexEnd do
      if AText[i] = '"' then inStr := not inStr
      else if AText[i] = '''' then exit(false);
    if inStr then exit(false);

    indexStart := AIndexEnd;
    while (indexStart > 1) and IsIdOrNumber(AText, indexStart-1) do
      dec(indexStart);
    w := copy(AText, indexStart, AIndexEnd-indexStart+1);
    if CompareText(w, 'EndIf')=0 then
    begin
      AText := Copy(AText, 1, indexStart-1) + 'End If' +
          Copy(AText, AIndexEnd+1, length(AText)-AIndexEnd);
      ADeltaX := 1;
      exit(true);
    end;
    for i := low(ImplementedReservedWords) to high(ImplementedReservedWords) do
       if CompareText(w, ImplementedReservedWords[i])=0 then
       begin
         AText := Copy(AText, 1, indexStart-1) + ImplementedReservedWords[i]
            + Copy(AText, AIndexEnd+1, length(AText)-AIndexEnd);
         exit(true);
       end;
    result := false;
  end;

var
  s: String;
  newCaret: TPoint;
  deltaX: integer;
begin
  newCaret := SynEdit1.LogicalCaretXY;
  if (((newCaret.X > FPrevCaret.X) and (newCaret.Y = FPrevCaret.Y)) or
     (newCaret.Y > FPrevCaret.Y)) and
     (FPrevCaret.Y >= 1) and (FPrevCaret.Y <= SynEdit1.Lines.Count) then
  begin
    s := SynEdit1.Lines[FPrevCaret.Y-1];
    if not IsIdOrNumber(s, FPrevCaret.X) and FixCase(s, FPrevCaret.X-1, deltaX) then
    begin
      SynEdit1.Lines[FPrevCaret.Y-1] := s;
      if FPrevCaret.Y = newCaret.Y then
      begin
        Synedit1.CaretX:= Synedit1.CaretX + deltaX;
        newCaret := SynEdit1.LogicalCaretXY;
      end;
    end;
  end;
  FPrevCaret := newCaret;
  InvalidateErrors;
end;

procedure TFMain.Timer1Timer(Sender: TObject);
begin
  if FErrorsToUpdate then
  begin
    UpdateErrors;
    UpdateTitleBar;
  end;
end;

end.

