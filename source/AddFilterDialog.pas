unit AddFilterDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, unitNNTPFilters;

type
  TdlgAddFilter = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    stBlurb: TLabel;
    Label2: TLabel;
    edFilterName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure edFilterNameChange(Sender: TObject);
  private
    { Private declarations }
  public
    Filter : TNNTPFilter;
    { Public declarations }
  end;

var
  dlgAddFilter: TdlgAddFilter;

implementation

{$R *.dfm}

procedure TdlgAddFilter.FormShow(Sender: TObject);
begin
  stBlurb.Caption := Format (stBlurb.Caption, [filter.Text]);
  edFilterName.Text := filter.Text
end;

procedure TdlgAddFilter.edFilterNameChange(Sender: TObject);
begin
  OKBtn.Enabled := edFilterName.Text <> ''
end;

end.
