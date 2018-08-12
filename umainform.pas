unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterVB, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls, usctypes;

type

  { TFMain }

  TFMain = class(TForm)
    CancelChanges: TAction;
    ListBox_Errors: TListBox;
    SynCompletion1: TSynCompletion;
    SynVBSyn1: TSynVBSyn;
    Timer1: TTimer;
    ToolButton2: TToolButton;
    ValidateChanges: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    SynEdit1: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure CancelChangesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynCompletion1SearchPosition(var APosition: integer);
    procedure Timer1Timer(Sender: TObject);
    procedure ValidateChangesExecute(Sender: TObject);
  private

  public
    AllCompletion: TStringList;
  end;

var
  FMain: TFMain;

implementation

uses ureadprog, uparsevb, uvariables;

{$R *.lfm}

{ TFMain }
procedure TFMain.ValidateChangesExecute(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TFMain.CancelChangesExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  AllCompletion := TStringList.Create;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  AllCompletion.Free;
end;

procedure TFMain.SynCompletion1SearchPosition(var APosition: integer);
var
  i: Integer;
  cur: String;
begin
  cur := SynCompletion1.CurrentString;
  SynCompletion1.ItemList.Clear;
  for i := 0 to AllCompletion.Count-1 do
    if CompareText(cur, copy(AllCompletion[i],1,length(cur)))= 0 then
      SynCompletion1.ItemList.Add(AllCompletion[i]);
  if AllCompletion.Count >0 then APosition:= 0
    else APosition:= -1;
end;

procedure TFMain.Timer1Timer(Sender: TObject);
var
  MainThread, pl: TPlayer;
  i: Integer;
  success: Boolean;
  programUpToCursor: TStringList;
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

  //full program for validation
  success := ureadprog.ReadProg(SynEdit1.Lines, MainThread);

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
    {uwritetriggers.WriteTriggers('prog.trigger', MainThread);
    uwritetriggers.WriteUnitProperties('prog.property');}
end;

end.

