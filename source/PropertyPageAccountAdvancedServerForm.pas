unit PropertyPageAccountAdvancedServerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls, ComCtrls;

type
  TPropertyPageAccountAdvancedServerData = class (TPropertyPageData)
  private
    fConnectTimeout: Integer;
    fReadTimeout: Integer;
    fServerTimeout : Integer;
    fUseXOver : boolean;
    fUsePipelining : boolean;
    fNNTPPort : Integer;
    fSSL : boolean;
    fSSLPort : Integer;
    fPipelineSize : Integer;
    fRASConnection : string;
    fMaxConnections : Integer;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageAccountAdvancedServer = class(TfmPropertyPage)
    Label13: TLabel;
    edServerTimeout: TEdit;
    Label28: TLabel;
    cbUseXOVER: TCheckBox;
    cbUsePipelining: TCheckBox;
    Label12: TLabel;
    edNNTPPort: TEdit;
    cbSSL: TCheckBox;
    Label15: TLabel;
    edSSLPort: TEdit;
    Label3: TLabel;
    edPipelineSize: TEdit;
    Label1: TLabel;
    Bevel6: TBevel;
    Bevel2: TBevel;
    rbDialDefault: TRadioButton;
    rbDontDial: TRadioButton;
    rbAlwaysDial: TRadioButton;
    cbRasEntries: TComboBox;
    Bevel3: TBevel;
    Label2: TLabel;
    edMaxConnections: TEdit;
    udMaxConnections: TUpDown;
    Label4: TLabel;
    lbConnectTimeout: TLabel;
    Label6: TLabel;
    edConnectTimeout: TEdit;
    lbReadTimeout: TLabel;
    Label8: TLabel;
    edReadTimeout: TEdit;
    procedure ControlClick(Sender: TObject);
    procedure ControlChange(Sender: TObject);
    procedure cbRasEntriesChange(Sender: TObject);
  private
    fData : TPropertyPageAccountAdvancedServerData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageAccountAdvancedServer: TfmPropertyPageAccountAdvancedServer;

implementation

uses NewsGlobals, unitNNTPServices;

{$R *.dfm}

{ TfmPropertyPageAccountAdvancedServer }

class function TfmPropertyPageAccountAdvancedServer.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageAccountAdvancedServerData
end;

procedure TfmPropertyPageAccountAdvancedServer.PopulateControls(
  AData: TPropertyPageData);
var
  i, idx : Integer;
begin
  Inherited;
  fData := AData as TPropertyPageAccountAdvancedServerData;

  cbRasEntries.Clear;
  idx := -1;
  for i := 0 to Length (RasEntries) - 1 do
  begin
    cbRasEntries.Items.Add(RasEntries [i].szEntryName);
    if SameText (RasEntries [i].szEntryName, fData.fRASConnection) then
      idx := i
  end;

  edConnectTimeout.Text := IntToStr(fData.fConnectTimeout);
  edReadTimeout.Text := IntToStr(fData.fReadTimeout);
  edServerTimeout.Text := IntToStr (fData.fServerTimeout);
  cbUseXOVER.Checked := fData.fUseXOver;
  edNNTPPort.Text := IntToStr (fData.fNNTPPort);
  cbUsePipelining.Checked := fData.fUsePipelining;
  cbSSL.Checked := fData.fSSL;
  edSSLPort.Text := IntToStr (fData.fSSLPort);
  edPipelineSize.Text := IntToStr (fData.fPipelineSize);
  udMaxConnections.Position := fData.fMaxConnections;
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
end;

procedure TfmPropertyPageAccountAdvancedServer.UpdateData;
begin
  if Populating then Exit;

  if rbDialDefault.Checked then
    fData.fRASConnection := ''
  else
    if rbDontDial.Checked then
      fData.fRASConnection := '~'
    else
      fData.fRASConnection := cbRasEntries.Text;

  fData.fConnectTimeout := StrToIntDef(edConnectTimeout.Text, 60);
  fData.fReadTimeout := StrToIntDef(edReadTimeout.Text, 60);
  fData.fServerTimeout := StrToIntDef (edServerTimeout.Text, 60);
  fData.fUseXOver := cbUseXOver.Checked;
  fData.fUsePipelining := cbUsePipelining.Checked;
  fData.fNNTPPort := StrToIntDef (edNNTPPort.Text, 119);
  fData.fSSL := cbSSL.Checked;
  fData.fSSLPort := StrToIntDef (edSSLPort.Text, 563);
  fData.fPipelineSize := StrToIntDef (edPipelineSize.Text, 1024);
  fData.fMaxConnections := udMaxConnections.Position;
end;

{ TPropertyPageAccountAdvancedServerData }

function TPropertyPageAccountAdvancedServerData.Apply : boolean;
var
  account : TNNTPAccount;
begin
  result := True;
  account := TNNTPAccount (Param);

  account.NNTPServerSettings.RASConnection := fRASConnection;
  account.NNTPServerSettings.ConnectTimeout := fConnectTimeout;
  account.NNTPServerSettings.ReadTimeout := fReadTimeout;
  account.NNTPServerSettings.ServerTimeout := fServerTimeout;
  account.NoXNews := not fUseXOver;
  account.UsePipelining := fUsePipelining;
  account.NNTPServerSettings.ServerPort := fNNTPPort;
  account.NNTPServerSettings.SSLRequired := fSSL;
  account.NNTPServerSettings.SSLPort := fSSLPort;
  account.NNTPServerSettings.PipelineSize := fPipelineSize;
  account.NNTPServerSettings.MaxConnections := fMaxConnections;
end;

procedure TPropertyPageAccountAdvancedServerData.Initialize;
var
  account : TNNTPAccount;
begin
  account := TNNTPAccount (Param);

  fRASConnection := account.NNTPServerSettings.RASConnection;
  fConnectTimeout := account.NNTPServerSettings.ConnectTimeout;
  fReadTimeout := account.NNTPServerSettings.ReadTimeout;
  fServerTimeout := account.NNTPServerSettings.ServerTimeout;
  fUseXOver := not account.NoXNews;
  fUsePipelining := Account.UsePipelining;
  fNNTPPort := account.NNTPServerSettings.ServerPort;
  fSSL := account.NNTPServerSettings.SSLRequired;
  fSSLPort := account.NNTPServerSettings.SSLPort;
  fPipelineSize := account.NNTPServerSettings.PipelineSize;
  fMaxConnections := account.NNTPServerSettings.MaxConnections;
end;

procedure TfmPropertyPageAccountAdvancedServer.ControlClick(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageAccountAdvancedServer.ControlChange(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageAccountAdvancedServer.cbRasEntriesChange(
  Sender: TObject);
begin
  if fPopulating then Exit;
  rbAlwaysDial.Checked := True;
  UpdateData
end;

end.
