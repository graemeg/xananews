unit PropertyPageMailAccountPreferencesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMailAccountPreferencesData = class (TPropertyPageData)
  private
    fIdentityName : string;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageMailAccountPreferences = class(TfmPropertyPage)
    Label3: TLabel;
    cbIdentities: TComboBox;
    btnNewIdentity: TButton;
    procedure cbIdentitiesChange(Sender: TObject);
    procedure btnNewIdentityClick(Sender: TObject);
  private
    fData : TPropertyPageMailAccountPreferencesData;
    procedure UpdateData;
  public
    procedure PopulateControls (AData : TPropertyPageData); override;
    class function GetDataClass : TPropertyPageDataClass; override;
  end;

var
  fmPropertyPageMailAccountPreferences: TfmPropertyPageMailAccountPreferences;

implementation

uses unitMailServices, unitNNTPServices, unitCharsetMap, IdentityDialog;

{$R *.dfm}

{ TfmPropertyPageMailAccountPreferences }

class function TfmPropertyPageMailAccountPreferences.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMailAccountPreferencesData
end;

procedure TfmPropertyPageMailAccountPreferences.PopulateControls(
  AData: TPropertyPageData);
var
  i, idx : Integer;
begin
  inherited;
  fData := AData as TPropertyPageMailAccountPreferencesData;


  cbIdentities.Clear;
  idx := -1;
  for i := 0 to NNTPAccounts.Identities.Count - 1 do
  begin
    cbIdentities.Items.Add(NNTPAccounts.Identities [i].Name);
    if NNTPAccounts.Identities [i].Name = fData.fIdentityName then
      idx := i
  end;

  if idx > -1 then
    cbIdentities.ItemIndex := idx

end;

procedure TfmPropertyPageMailAccountPreferences.UpdateData;
begin
  if Populating then Exit;
  fData.fIdentityName := cbIdentities.Text;
end;

{ TPropertyPageMailAccountPreferencesData }

function TPropertyPageMailAccountPreferencesData.Apply : boolean;
var
  acct : TMailAccount;
begin
  result := True;
  acct := TMailAccount (Param);
  acct.SetIdentityName(fIdentityName);
end;

procedure TPropertyPageMailAccountPreferencesData.Initialize;
var
  acct : TMailAccount;
begin
  acct := TMailAccount (Param);
  fIdentityName := acct.Identity.Name;
end;

procedure TfmPropertyPageMailAccountPreferences.cbIdentitiesChange(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageMailAccountPreferences.btnNewIdentityClick(
  Sender: TObject);
var
  st : string;
begin
  if DoAddIdentityDialog (self, st) then
  begin
    cbIdentities.Items.Add(st);
    cbIdentities.Text := st
  end
end;

end.
