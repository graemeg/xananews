unit PropertyPageDefaultsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TfmPropertyPageDefaults = class(TfmPropertyPage)
    Bevel2: TBevel;
    btnReset: TButton;
    procedure btnResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  protected
    procedure RestoreParentSettings; virtual;
    function CanRestoreParentSettings : boolean; virtual;
  public
  end;

var
  fmPropertyPageDefaults: TfmPropertyPageDefaults;

implementation

{$R *.dfm}

procedure TfmPropertyPageDefaults.btnResetClick(Sender: TObject);
begin
  RestoreParentSettings;
end;

function TfmPropertyPageDefaults.CanRestoreParentSettings: boolean;
begin
  result := True
end;

procedure TfmPropertyPageDefaults.RestoreParentSettings;
begin
// Stub
end;

procedure TfmPropertyPageDefaults.FormShow(Sender: TObject);
begin
  inherited;

  if not CanRestoreParentSettings then
  begin
    btnReset.Visible := False
  end
end;

end.
