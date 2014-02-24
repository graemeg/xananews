unit PropertyPageMailAccountConnectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMailAccountConnectionData = class (TPropertyPageData)
  private
    fConnectTimeout: Integer;
    fReadTimeout: Integer;
    fServerTimeout : Integer;
    fSMTPPort : Integer;
    fSSL : boolean;
    fSSLPort : Integer;
    fRASConnection : string;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageMailAccountConnection = class(TfmPropertyPage)
    rbDialDefault: TRadioButton;
    rbDontDial: TRadioButton;
    rbAlwaysDial: TRadioButton;
    cbRasEntries: TComboBox;
    Bevel3: TBevel;
    edServerTimeout: TEdit;
    edSMTPPort: TEdit;
    Label13: TLabel;
    Label12: TLabel;
    Bevel6: TBevel;
    cbSSL: TCheckBox;
    edSSLPort: TEdit;
    Label15: TLabel;
    Label1: TLabel;
    lbReadTimeout: TLabel;
    edReadTimeout: TEdit;
    lbConnectTimeout: TLabel;
    edConnectTimeout: TEdit;
    Label28: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ControlClick(Sender: TObject);
    procedure ControlChange(Sender: TObject);
    procedure cbRasEntriesChange(Sender: TObject);
  private
    fData : TPropertyPageMailAccountConnectionData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageMailAccountConnection: TfmPropertyPageMailAccountConnection;

implementation

uses NewsGlobals, unitMailServices;

{$R *.dfm}

{ TPropertyPageMailAccountConnectionData }

function TPropertyPageMailAccountConnectionData.Apply : boolean;
var
  acct : TMailAccount;
begin
  result := True;
  acct := TMailAccount (Param);

  acct.ServerSettings.ConnectTimeout := fConnectTimeout;
  acct.ServerSettings.ReadTimeout := fReadTimeout;
  acct.ServerSettings.ServerTimeout := fServerTimeout;
  acct.ServerSettings.ServerPort := fSMTPPort;
  acct.ServerSettings.SSLRequired := fSSL;
  acct.ServerSettings.SSLPort := fSSLPort;
  acct.ServerSettings.RASConnection := fRASConnection;
end;

procedure TPropertyPageMailAccountConnectionData.Initialize;
var
  acct : TMailAccount;
begin
  acct := TMailAccount (Param);
  fConnectTimeout := acct.ServerSettings.ConnectTimeout;
  fReadTimeout := acct.ServerSettings.ReadTimeout;
  fServerTimeout := acct.ServerSettings.ServerTimeout;
  fSMTPPort := acct.ServerSettings.ServerPort;
  fSSL := acct.ServerSettings.SSLRequired;
  fSSLPort := acct.ServerSettings.SSLPort;
  fRASConnection := acct.ServerSettings.RASConnection;
end;

{ TfmPropertyPageMailAccountConnection }

class function TfmPropertyPageMailAccountConnection.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMailAccountConnectionData
end;

procedure TfmPropertyPageMailAccountConnection.PopulateControls(
  AData: TPropertyPageData);
var
  i, idx : Integer;
begin
  inherited;
  fData := AData as TPropertyPageMailAccountConnectionData;

  cbRasEntries.Clear;
  idx := -1;
  for i := 0 to Length (RasEntries) - 1 do
  begin
    cbRasEntries.Items.Add(RasEntries [i].szEntryName);
    if SameText (RasEntries [i].szEntryName, fData.fRasConnection) then
      idx := i
  end;

  edConnectTimeout.Text := IntToStr(fData.fConnectTimeout);
  edReadTimeout.Text := IntToStr(fData.fReadTimeout);
  edServerTimeout.Text := IntToStr (fData.fServerTimeout);
  edSMTPPort.Text := IntToStr (fData.fSMTPPort);
  cbSSL.Checked := fData.fSSL;
  edSSLPort.Text := IntToStr (fData.fSSLPort);
  if fData.fRASConnection = '' then
  begin
    rbDialDefault.Checked := True;
    cbRasEntries.Text := ''
  end
  else
    if fData.fRASConnection = '~' then
    begin
      rbDontDial.Checked := True;
      cbRasEntries.Text := ''
    end
    else
    begin
      rbAlwaysDial.Checked := True;
      if idx <> -1 then
        cbRasEntries.ItemIndex := idx
    end;
  edSSLPort.Enabled := cbSSL.Checked;
end;

procedure TfmPropertyPageMailAccountConnection.UpdateData;
begin
  if Populating then Exit;

  fData.fConnectTimeout := StrToIntDef(edConnectTimeout.Text, 60);
  fData.fReadTimeout := StrToIntDef(edReadTimeout.Text, 60);
  fData.fServerTimeout := StrToIntDef (edServerTimeout.Text, 60);
  fData.fSMTPPort := StrToIntDef (edSMTPPort.Text, 25);
  fData.fSSL := cbSSL.Checked;
  fData.fSSLPort := StrToIntDef (edSSLPort.Text, 465);
  if rbDialDefault.Checked then
    fData.fRASConnection := ''
  else
    if rbDontDial.Checked then
      fData.fRASConnection := '~'
    else
      fData.fRASConnection := cbRasEntries.Text;
end;

procedure TfmPropertyPageMailAccountConnection.ControlClick(
  Sender: TObject);
begin
  UpdateData;

  if Sender = cbSSL then
    edSSLPort.Enabled := cbSSL.Checked;
end;

procedure TfmPropertyPageMailAccountConnection.ControlChange(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageMailAccountConnection.cbRasEntriesChange(
  Sender: TObject);
begin
  if Populating then Exit;
  rbAlwaysDial.Checked := True;
  UpdateData
end;

end.
