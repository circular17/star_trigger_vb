unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterVB, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls;

type

  { TFMain }

  TFMain = class(TForm)
    CancelChanges: TAction;
    Errors: TLabel;
    Label1: TLabel;
    ListBox_Locations: TListBox;
    ListBox_Errors: TListBox;
    Panel_Locations: TPanel;
    Panel_Utils: TPanel;
    Panel_Code: TPanel;
    Panel_Errors: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    SynVBSyn1: TSynVBSyn;
    Timer1: TTimer;
    ToolButton2: TToolButton;
    ValidateChanges: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure CancelChangesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox_ErrorsDblClick(Sender: TObject);
    procedure ListBox_LocationsDblClick(Sender: TObject);
    procedure SynCompletion1SearchPosition(var APosition: integer);
    procedure SynEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ValidateChangesExecute(Sender: TObject);
  private

  public
    AllCompletion: TStringList;
    ErrorsToUpdate: boolean;
    procedure ClearLocations;
    procedure UpdateAutoCompleteList;
    procedure AddLocation(AName: string; AIsText: boolean);
  end;

var
  FMain: TFMain;

implementation

uses ureadprog, uparsevb, uvariables, usctypes, umapinfo, uwritetriggers;

{$R *.lfm}

{ TFMain }
procedure TFMain.ValidateChangesExecute(Sender: TObject);
begin
  ModalResult:= mrOK;
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

procedure TFMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AllCompletion := TStringList.Create;

  AddLocation('Anywhere', False);
  for i := LocationMinIndex to LocationMaxIndex do
    if (i <> AnywhereLocationIndex) and MapInfo.LocationExists(i) then
      fMain.AddLocation(MapInfo.LocationName[i], True);

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
var
  MainThread: TPlayer;
  i: Integer;
  success: Boolean;
begin
  if ErrorsToUpdate then
  begin
    ErrorsToUpdate := false;

    //full program for validation
    success := ureadprog.ReadProg(SynEdit1.Lines, MainThread);
    if success then
    begin
      try
        uwritetriggers.CreateTriggers(MainThread);
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

    if success then
    begin
      if MainThread = plNone then MainThread := plPlayer8;
    end;
  end;
      {uwritetriggers.WriteTriggers('prog.trigger', MainThread);
    uwritetriggers.WriteUnitProperties('prog.property');}
end;

end.

