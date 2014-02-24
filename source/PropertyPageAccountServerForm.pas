unit PropertyPageAccountServerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageAccountServerData = class (TPropertyPageData)
  private
    fServerName : string;
    fLogonRequired : boolean;
    fServerAccountName : string;
    fServerPassword : string;
    fRetypePassword : string;
    fAlwaysAuthenticate : boolean;
    fLastGreeting : string;
    fForm : TForm;
    fPopulated : boolean;

  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
    procedure Error; override;
  end;

  TfmPropertyPageAccountServer = class(TfmPropertyPage)
    Label9: TLabel;
    edServerName: TEdit;
    cbLogonRequired: TCheckBox;
    edServerAccountName: TEdit;
    edServerAccountPassword: TEdit;
    cbAlwaysAuthenticate: TCheckBox;
    Label2: TLabel;
    stServerAccountName: TLabel;
    stServerAccountPassword: TLabel;
    stGreeting: TMemo;
    stRetypePassword: TLabel;
    edRetypePassword: TEdit;
    stPasswordError: TLabel;
    procedure edServerAccountPasswordChange(Sender: TObject);
    procedure cbLogonRequiredClick(Sender: TObject);
    procedure edServerNameChange(Sender: TObject);
  private
    fData : TPropertyPageAccountServerData;
    procedure UpdateData;
    procedure EnableLogonOptions (logonEnabled : boolean);
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageAccountServer: TfmPropertyPageAccountServer;

implementation

uses unitNNTPServices, unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageAccountServer }

class function TfmPropertyPageAccountServer.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageAccountServerData;
end;

procedure TfmPropertyPageAccountServer.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageAccountServerData;
  fData.fForm := self;

  edServerName.Text := fData.fServerName;
  cbLogonRequired.Checked := fData.fLogonRequired;
  edServerAccountName.Text := fData.fServerAccountName;

  if XNOptions.PlainTextPasswords then
  begin
    edServerAccountPassword.Text := fData.fServerPassword;
    edRetypePassword.Text := fData.fRetypePassword;
    edRetypePassword.Visible := False;
    stRetypePassword.Visible := False
  end
  else
  begin
    edServerAccountPassword.PasswordChar := '*';
    if fData.fPopulated then
    begin
      edServerAccountPassword.Text := fData.fServerPassword;
      edRetypePassword.Text := fData.fRetypePassword
    end
    else
    begin
      if fData.fServerPassword <> '' then
      begin
        edServerAccountPassword.Text := '~{~{~{~{';
        edRetypePassword.Text := '~{~{~{~{'
      end
    end;
    edRetypePassword.Visible := True;
    stRetypePassword.Visible := True;
  end;

  cbAlwaysAuthenticate.Checked := fData.fAlwaysAuthenticate;
  stGreeting.Text := fData.fLastGreeting;
  EnableLogonOptions (cbLogonRequired.Checked);
end;

procedure TfmPropertyPageAccountServer.UpdateData;
var
  logonEnabled : boolean;
begin
  if Populating then Exit;
  fData.fServerName := edServerName.Text;
  logonEnabled := cbLogonRequired.Checked;
  fData.fLogonRequired := logonEnabled;
  fData.fServerAccountName := edServerAccountName.Text;
  if edServerAccountPassword.Text <> '~{~{~{~{' then
    fData.fServerPassword := edServerAccountPassword.Text;

  if edRetypePassword.Text <> '~{~{~{~{' then
    fData.fRetypePassword := edRetypePassword.Text;
  fData.fAlwaysAuthenticate := cbAlwaysAuthenticate.Checked;
  EnableLogonOptions (logonEnabled);
end;

procedure TfmPropertyPageAccountServer.EnableLogonOptions(
  logonEnabled: boolean);
begin
  edServerAccountName.Enabled := logonEnabled;
  edServerAccountPassword.Enabled := logonEnabled;
  edRetypePassword.Enabled := logonEnabled and not XNOptions.PlainTextPasswords;
  cbAlwaysAuthenticate.Enabled := logonEnabled;
  stServerAccountName.Enabled := logonEnabled;
  stServerAccountPassword.Enabled := logonEnabled;
  stRetypePassword.Enabled := logonEnabled;
end;

procedure TfmPropertyPageAccountServer.UpdateActions;
var
  passReq : boolean;
  passOk : boolean;
begin
  passReq := cbLogonRequired.Checked and not XNOptions.PlainTextPasswords;
  if passReq then
    passOk := edServerAccountPassword.Text = edRetypePassword.Text
  else
    passOk := True;
  stPasswordError.Visible := not passOk;
end;

{ TPropertyPageAccountServerData }

function TPropertyPageAccountServerData.Apply : boolean;
var
  account : TNNTPAccount;
begin
  result := (fServerPassword = fRetypePassword) or XNOptions.PlainTextPasswords;
  if result then
  begin
    account := TNNTPAccount (Param);

    account.NNTPServerSettings.ServerName := fServerName;
    if fLogonRequired then
    begin
      account.NNTPServerSettings.ServerLogonRequired := True;
      account.NNTPServerSettings.ServerAccountName := fServerAccountName;
      account.NNTPServerSettings.ServerPassword := fServerPassword;
      account.NNTPServerSettings.AlwaysAuthenticate := fAlwaysAuthenticate
    end
    else
    begin
      account.NNTPServerSettings.ServerLogonRequired := False;
      account.NNTPServerSettings.ServerAccountName := '';
      account.NNTPServerSettings.ServerPassword := '';
      account.NNTPServerSettings.AlwaysAuthenticate := False
    end
  end
end;

procedure TPropertyPageAccountServerData.Error;
begin
  ShowMessage ('Passwords must match');

  if Assigned (fForm) then
    TfmPropertyPageAccountServer (fForm).edServerAccountPassword.SetFocus
end;

procedure TPropertyPageAccountServerData.Initialize;
var
  account : TNNTPAccount;
begin
  account := TNNTPAccount (Param);

  fServerName := account.NNTPServerSettings.ServerName;
  fLogonRequired := account.NNTPServerSettings.ServerLogonRequired;
  fServerAccountName := account.NNTPServerSettings.ServerAccountName;
  fServerPassword := account.NNTPServerSettings.ServerPassword;
  fRetypePassword := fServerPassword;
  fAlwaysAuthenticate := account.NNTPServerSettings.AlwaysAuthenticate;
  fLastGreeting := account.Greeting;
end;

procedure TfmPropertyPageAccountServer.cbLogonRequiredClick(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageAccountServer.edServerNameChange(Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageAccountServer.edServerAccountPasswordChange(
  Sender: TObject);
begin
  UpdateData;
  fData.fPopulated := True;
end;

end.
