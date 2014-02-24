unit PropertyPageAccountGeneralForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls, unitNNTPServices;

type
  TPropertyPageAccountGeneralData = class (TPropertyPageData)
  private
    fAccountName : string;
    fMarkOnLeave : boolean;
    fScanKeyPhrases : boolean;
    fSecret : boolean;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageAccountGeneral = class(TfmPropertyPage)
    Label7: TLabel;
    edAccountName: TEdit;
    cbMarkOnLeave: TCheckBox;
    cbScanKeyPhrases: TCheckBox;
    cbSecret: TCheckBox;
    Bevel6: TBevel;
    procedure controlClick(Sender: TObject);
    procedure edAccountNameChange(Sender: TObject);
  private
    fData : TPropertyPageAccountGeneralData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageAccountGeneral: TfmPropertyPageAccountGeneral;

implementation

{$R *.dfm}

{ TfmPropertyPageAccountGeneral }

class function TfmPropertyPageAccountGeneral.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageAccountGeneralData
end;

procedure TfmPropertyPageAccountGeneral.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageAccountGeneralData;

  edAccountName.Text := fData.fAccountName;
  cbMarkOnLeave.Checked := fData.fMarkOnLeave;
  cbScanKeyPhrases.Checked := fData.fScanKeyPhrases;
  cbSecret.Checked := fData.fSecret;
end;

procedure TfmPropertyPageAccountGeneral.UpdateData;
begin
  if Populating then Exit;
  fData.fAccountName := edAccountName.Text;
  fData.fMarkOnLeave := cbMarkOnLeave.Checked;
  fData.fScanKeyPhrases := cbScanKeyPhrases.Checked;
  fData.fSecret := cbSecret.Checked
end;

{ TPropertyPageAccountGeneralData }

function TPropertyPageAccountGeneralData.Apply : boolean;
var
  account : TNNTPAccount;
begin
  result := True;
  account := TNNTPAccount (Param);

  account.Secret := fSecret;
  account.ScanKeyPhrases := fScanKeyPhrases;
  account.MarkOnLeave := fMarkOnLeave;
  if fAccountName <> '' then
    account.AccountName := fAccountName
end;

procedure TPropertyPageAccountGeneralData.Initialize;
var
  account : TNNTPAccount;
begin
  account := TNNTPAccount (Param);

  fSecret := account.Secret;
  fScanKeyPhrases := account.ScanKeyPhrases;
  fMarkOnLeave := account.MarkOnLeave;
  fAccountName := account.AccountName;
end;

procedure TfmPropertyPageAccountGeneral.controlClick(Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageAccountGeneral.edAccountNameChange(
  Sender: TObject);
begin
  UpdateData
end;

end.
