unit FilterDetailsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, unitNNTPFilters;

type
  TdlgFilterDetails = class(TForm)
    dlgFilterDetails: TLabel;
    edFilter: TEdit;
    rgColumn: TRadioGroup;
    rgOperator: TRadioGroup;
    Label1: TLabel;
    edFilterTExt: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure UpdateActions; override;
  public
    Filter : TNNTPFilter;
    { Public declarations }
  end;

var
  dlgFilterDetails: TdlgFilterDetails;

implementation

{$R *.dfm}

procedure TdlgFilterDetails.FormShow(Sender: TObject);
begin
  rgColumn.ItemIndex := Integer (filter.Column);
  rgOperator.ItemIndex := Integer (filter.Operator);
  edFilterText.Text := filter.StrVal;
  edFilter.Text := filter.Name;
  edFilter.Enabled := filter.Name = ''
end;

procedure TdlgFilterDetails.UpdateActions;
var
  enable : boolean;
  filter : TNNTPFilter;
begin
  enable := (edFilter.Text <> '') and (edFilterText.Text <> '');
  if enable then
  begin
    filter := AllFilters.FindFilter (edFilter.Text);
    if (filter <> nil) and (filter <> self.Filter) then
      enable := False
  end;
  btnOK.Enabled := enable;
end;

procedure TdlgFilterDetails.btnOKClick(Sender: TObject);
begin
  filter.Name := edFilter.Text;
  filter.Column := TNNTPFilterColumn (rgColumn.ItemIndex);
  filter.Operator := TNNTPFilterOperator (rgOperator.ItemIndex);
  filter.StrVal := edFilterText.Text;
end;

end.
