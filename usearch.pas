unit usearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFSearch }

  TFSearch = class(TForm)
    Button_Ok: TButton;
    Button_Cancel: TButton;
    CheckBox_Replace: TCheckBox;
    CheckBox_CaseSensitive: TCheckBox;
    CheckBox_WholeWord: TCheckBox;
    CheckBox_RegExp: TCheckBox;
    CheckBox_Backwards: TCheckBox;
    Edit_ReplaceBy: TEdit;
    Edit_SearchFor: TEdit;
    Label1: TLabel;
    procedure CheckBox_ReplaceChange(Sender: TObject);
    procedure Edit_SearchForChange(Sender: TObject);
  private

  public

  end;

var
  FSearch: TFSearch;

implementation

{$R *.lfm}

{ TFSearch }

procedure TFSearch.CheckBox_ReplaceChange(Sender: TObject);
begin
  Edit_ReplaceBy.Enabled := CheckBox_Replace.Checked;
end;

procedure TFSearch.Edit_SearchForChange(Sender: TObject);
begin
  Button_Ok.Enabled := Edit_SearchFor.Text <> '';;
end;

end.

