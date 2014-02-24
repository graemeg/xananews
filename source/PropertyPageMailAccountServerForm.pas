unit PropertyPageMailAccountServerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMailAccountServerData = class (TPropertyPageData)
  private
    fServerName : string;
    fServerAccountName : string;
    fServerAccountPassword : string;
    fRetypePassword : string;
    fForm : TForm;
    fPopulated : boolean;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
    procedure Error; override;
  end;

  TfmPropertyPageMailAccountServer = class(TfmPropertyPage)
    edServerAccountName: TEdit;
    stServerAccountName: TLabel;
    stServerAccountPassword: TLabel;
    edServerAccountPassword: TEdit;
    Label9: TLabel;
    edServerName: TEdit;
    stRetypePassword: TLabel;
    edRetypePassword: TEdit;
    stPasswordError: TLabel;
    procedure edServerAccountPasswordChange(Sender: TObject);
    procedure edServerNameChange(Sender: TObject);
  private
    fData : TPropertyPageMailAccountServerData;
    procedure UpdateData;
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageMailAccountServer: TfmPropertyPageMailAccountServer;

implementation

uses unitMailServices, unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageMailAccountServer }

class function TfmPropertyPageMailAccountServer.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMailAccountServerData;
end;

procedure TfmPropertyPageMailAccountServer.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageMailAccountServerData;

  edServername.Text := fData.fServerName;
  edServerAccountName.Text := fData.fServerAccountName;
  if XNOptions.PlainTextPasswords then
  begin
    edServerAccountPassword.Text := fData.fServerAccountPassword;
    edRetypePassword.Text := fData.fRetypePassword;
    edRetypePassword.Visible := False;
    stRetypePassword.Visible := False
  end
  else
  begin
    edServerAccountPassword.PasswordChar := '*';
    if fData.fPopulated then
    begin
      edServerAccountPassword.Text := fData.fServerAccountPassword;
      edRetypePassword.Text := fData.fRetypePassword
    end
    else
    begin
      if fData.fServerAccountPassword <> '' then
      begin
        edServerAccountPassword.Text := '~{~{~{~{';
        edRetypePassword.Text := '~{~{~{~{'
      end
    end;
    edRetypePassword.Visible := True;
    stRetypePassword.Visible := True;
  end
end;

procedure TfmPropertyPageMailAccountServer.UpdateData;
begin
  if Populating then Exit;
  fData.fServerName := edServerName.Text;
  fData.fServerAccountName := edServerAccountName.Text;
  if edServerAccountPassword.Text <> '~{~{~{~{' then
    fData.fServerAccountPassword := edServerAccountPassword.Text;
  if edRetypePassword.Text <> '~{~{~{~{' then
    fData.fRetypePassword := edRetypePassword.Text;
end;

procedure TfmPropertyPageMailAccountServer.UpdateActions;
var
  passReq : boolean;
  passOk : boolean;
begin
  passReq := not XNOptions.PlainTextPasswords;
  if passReq then
    passOk := edServerAccountPassword.Text = edRetypePassword.Text
  else
    passOk := True;
  stPasswordError.Visible := not passOk;
end;

{ TPropertyPageMailAccountServerData }

function TPropertyPageMailAccountServerData.Apply : boolean;
var
  acct : TMailAccount;
begin
  result := (fServerAccountPassword = fRetypePassword) or XNOptions.PlainTextPasswords;
  if result then
  begin
    acct := TMailAccount (Param);
    acct.ServerSettings.ServerName := fServerName;
    acct.ServerSettings.ServerAccountName := fServerAccountName;
    acct.ServerSettings.ServerPassword := fServerAccountPassword
  end;
end;

procedure TPropertyPageMailAccountServerData.Error;
begin
  ShowMessage ('Passwords must match');

  if Assigned (fForm) then
    TfmPropertyPageMailAccountServer (fForm).edServerAccountPassword.SetFocus
end;

procedure TPropertyPageMailAccountServerData.Initialize;
var
  acct : TMailAccount;
begin
  acct := TMailAccount (Param);

  fServerName := acct.ServerSettings.ServerName;
  fServerAccountName := acct.ServerSettings.ServerAccountName;
  fServerAccountPassword := acct.ServerSettings.ServerPassword;
  fRetypePassword := fServerAccountPassword;
end;

procedure TfmPropertyPageMailAccountServer.edServerNameChange(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageMailAccountServer.edServerAccountPasswordChange(
  Sender: TObject);
begin
  UpdateData;
  fData.fPopulated := True;
end;

end.
