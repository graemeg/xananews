unit PropertyPagePreferencesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageDefaultsForm, PropertyPageForm, StdCtrls, ExtCtrls, unitSettings, unitBatches;

type
  TPropertyPagePreferencesData = class (TPropertyPageData)
  private
    function GetNNTPSettings: TNNTPSettings;
  private
    fSettings : TDisplaySettings;
    fPerformDefaultAction : TPerformDefaultAction;
    fActName : string;
    fObject : TObject;
    fSignatureOverride : string;
    property NNTPSettings : TNNTPSettings read GetNNTPSettings;
  protected
    procedure Initialize; override;
  public
    procedure InitObject (obj : TObject);
    function Apply : boolean; override;
    destructor Destroy; override;
  end;

  TfmPropertyPagePreferences = class(TfmPropertyPageDefaults)
    cbDefMessageCharset: TComboBox;
    Label6: TLabel;
    Label5: TLabel;
    edPlaySound: TEdit;
    btnPlaySound: TButton;
    Label1: TLabel;
    cbPurgeToFolder: TComboBox;
    Label2: TLabel;
    lblDefaultAction: TLabel;
    btnChangeDefaultAction: TButton;
    Label4: TLabel;
    edTruncateFrom: TEdit;
    Label3: TLabel;
    rbPerformNever: TRadioButton;
    rbPerformOncePerSession: TRadioButton;
    rbPerformAlways: TRadioButton;
    OpenDialog1: TOpenDialog;
    Label7: TLabel;
    mmoSignature: TMemo;
    procedure mmoSignatureChange(Sender: TObject);
    procedure btnPlaySoundClick(Sender: TObject);
    procedure rbPerformNeverClick(Sender: TObject);
    procedure edTruncateFromChange(Sender: TObject);
    procedure cbDefMessageCharsetChange(Sender: TObject);
    procedure rbAscendingClick(Sender: TObject);
    procedure btnChangeDefaultActionClick(Sender: TObject);
  private
    fData : TPropertyPagePreferencesData;
    procedure InitializeControls (settings : TDisplaySettings);
    procedure UpdateData;
  protected
    function CanRestoreParentSettings : boolean; override;
    procedure RestoreParentSettings; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPagePreferences: TfmPropertyPagePreferences;

implementation

uses unitCharsetMap, NewsGlobals, unitSavedArticles, unitNNTPServices, MessagesDialog;

{$R *.dfm}

{ TfmPropertyPagePreferences }

function TfmPropertyPagePreferences.CanRestoreParentSettings: boolean;
var
  settings : TDisplaySettings;
begin
  settings := TDisplaySettings (fData.Param);
  result := Assigned (settings.Parent);
end;

class function TfmPropertyPagePreferences.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPagePreferencesData;
end;

procedure TfmPropertyPagePreferences.InitializeControls(
  settings: TDisplaySettings);
var
  i, idx : Integer;
  st : string;
begin
  GetCharsetNames (cbDefMessageCharset.Items);
  st := CodePageToCharsetName (settings.DefaultCodePage);

  idx := -1;
  for i := 0 to cbDefMessageCharset.Items.Count - 1 do
    if cbDefMessageCharset.Items [i] = st then
      idx := i;

  if idx > -1 then
    cbDefMessageCharset.ItemIndex := idx;

  cbPurgeToFolder.Clear;
  cbPurgeToFolder.Items.Add('--' + rstNone + '--');
  idx := 0;

  for i := 0 to gArticleFolders.Count - 1 do
  begin
    cbPurgeToFolder.Items.Add(gArticleFolders.Folder [i].Name);
    if gArticleFolders.Folder [i].Name = settings.PurgeFolder then
      idx := i + 1
  end;

  cbPurgeToFolder.ItemIndex := idx;

  edTruncateFrom.Text := settings.TruncateFrom;
  edPlaySound.Text := settings.SoundFile;

  lblDefaultAction.Caption := fData.fActName;

  case fData.fPerformDefaultAction of
    paNever : rbPerformNever.Checked := True;
    paAlways : rbPerformAlways.Checked := True;
    paSession : rbPerformOnceperSession.Checked := True
  end;

  mmoSignature.Text := fData.fSignatureOverride;
end;

procedure TfmPropertyPagePreferences.PopulateControls(
  AData: TPropertyPageData);
var
  settings : TDisplaySettings;
begin
  inherited;
  fData := AData as TPropertyPagePreferencesData;
  settings := fData.fSettings;

  InitializeControls (settings);
end;

procedure TfmPropertyPagePreferences.RestoreParentSettings;
var
  nsettings : TNNTPSettings;
begin
  fData.fSettings.Assign(fData.fSettings.Parent);
  Populating := True;
  nsettings := fData.NNTPSettings;
  if Assigned (nsettings) then
  begin
    nsettings.DefaultAction := Nil;
    if Assigned (nsettings.DefaultAction) then
      fData.fActName := nsettings.DefaultAction.ActionText
    else
      fData.fActName := 'Get all new messages';

    fData.fPerformDefaultAction := nsettings.PerformDefaultAction;
    fData.fSignatureOverride := nsettings.SignatureOverride;
  end;

  InitializeControls (fData.fSettings);
  Populating := False;
end;

procedure TfmPropertyPagePreferences.UpdateData;
begin
  if Populating then Exit;

  fData.fSettings.DefaultCodepage := CharsetNameToCodePage (cbDefMessageCharset.Text);
  if cbPurgeToFolder.ItemIndex = 0 then
    fData.fSettings.PurgeFolder := ''
  else
    fData.fSettings.PurgeFolder := cbPurgeToFolder.Text;

  fData.fSettings.TruncateFrom := edTruncateFrom.Text;
  fData.fSettings.SoundFile := edPlaySound.Text;
  fData.fSignatureOverride := mmoSignature.Text;
end;

{ TPropertyPagePreferencesData }

function TPropertyPagePreferencesData.Apply : boolean;
var
  settings : TDisplaySettings;
  nsettings : TNNTPSettings;
  act : TBatchAction;
begin
  result := True;
  settings := TDisplaySettings (Param);

  settings.DefaultCodepage := fSettings.DefaultCodepage;
  settings.PurgeFolder := fSettings.PurgeFolder;
  settings.TruncateFrom := fSettings.TruncateFrom;
  settings.SoundFile := fSettings.SoundFile;
  nsettings := NNTPSettings;
  if Assigned (nSettings) then
  begin
    act := TBatchAction.Create;
    try
      act.ActionText := fActName;
      nSettings.DefaultAction := act
    finally
      act.Free
    end;

    nsettings.PerformDefaultAction := fPerformDefaultAction;
    nsettings.SignatureOverride := fSignatureOverride;
  end
end;


destructor TPropertyPagePreferencesData.Destroy;
begin
  fSettings.Free;

  inherited;
end;

function TPropertyPagePreferencesData.GetNNTPSettings: TNNTPSettings;
begin
  result:= Nil;
  if fObject is TNNTPAccounts then
    result := TNNTPAccounts (fObject).NNTPSettings
  else
    if fObject is TNNTPAccount then
      result := TNNTPAccount (fObject).NNTPSettings
    else
      if fObject is TSubscribedGroup then
        result := TSubscribedGroup (fObject).NNTPSettings
end;

procedure TPropertyPagePreferencesData.Initialize;
var
  settings : TDisplaySettings;
  nsettings : TNNTPSettings;

begin
  settings := TDisplaySettings (Param);
  fSettings := TDisplaySettings.Create(settings.Parent);
  fSettings.Assign(settings);

  nSettings := NNTPSettings;
  if Assigned (nSettings) then
  begin
    if Assigned (nSettings.DefaultAction) then
      fActName := NNTPSettings.DefaultAction.ActionText
    else
      fActName := 'Get all new messages';
    fPerformDefaultAction := nsettings.PerformDefaultAction;
    fSignatureOverride := nSettings.SignatureOverride;
  end
end;

procedure TfmPropertyPagePreferences.edTruncateFromChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPagePreferences.cbDefMessageCharsetChange(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPagePreferences.rbAscendingClick(Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPagePreferences.btnChangeDefaultActionClick(
  Sender: TObject);
begin
  with lblDefaultAction do
  begin
    Caption := DoDefaultActionDialog (Caption);
    fData.fActName := Caption
  end
end;

procedure TPropertyPagePreferencesData.InitObject(obj: TObject);
begin
  fObject := obj
end;

procedure TfmPropertyPagePreferences.rbPerformNeverClick(Sender: TObject);
begin
  if Populating then Exit;
  if rbPerformOncePerSession.Checked then
    fData.fPerformDefaultAction := paSession
  else
    if rbPerformAlways.Checked then
      fData.fPerformDefaultAction := paAlways
    else
      fData.fPerformDefaultAction := paNever
end;

procedure TfmPropertyPagePreferences.btnPlaySoundClick(Sender: TObject);
begin
  OpenDialog1.FileName := edPlaySound.Text;
  if OpenDialog1.Execute then
    edPlaySound.Text := OpenDialog1.FileName;
end;

procedure TfmPropertyPagePreferences.mmoSignatureChange(Sender: TObject);
begin
  UpdateData;
end;

end.
