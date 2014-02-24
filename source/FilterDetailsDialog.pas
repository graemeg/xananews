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
    Filter: TNNTPFilter;
    { Public declarations }
  end;

var
  dlgFilterDetails: TdlgFilterDetails;

implementation

{$R *.dfm}

procedure TdlgFilterDetails.FormShow(Sender: TObject);
begin
  rgColumn.ItemIndex := Integer(Filter.Column);
  rgOperator.ItemIndex := Integer(Filter.Operator);
  edFilterTExt.Text := Filter.StrVal;
  edFilter.Text := Filter.Name;
  edFilter.Enabled := Filter.Name = ''
end;

procedure TdlgFilterDetails.UpdateActions;
var
  enable: boolean;
  Filter: TNNTPFilter;
begin
  enable := (edFilter.Text <> '') and (edFilterTExt.Text <> '');
  if enable then
  begin
    Filter := AllFilters.FindFilter(edFilter.Text);
    if (Filter <> nil) and (Filter <> self.Filter) then
      enable := False
  end;
  btnOK.Enabled := enable;
end;

procedure TdlgFilterDetails.btnOKClick(Sender: TObject);
begin
  Filter.Name := edFilter.Text;
  Filter.Column := TNNTPFilterColumn(rgColumn.ItemIndex);
  Filter.Operator := TNNTPFilterOperator(rgOperator.ItemIndex);
  Filter.StrVal := edFilterTExt.Text;
end;

end.
