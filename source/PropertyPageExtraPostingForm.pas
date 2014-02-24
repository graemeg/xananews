unit PropertyPageExtraPostingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageDefaultsForm, StdCtrls, ExtCtrls, PropertyPageForm,
  PropertyPagePosting, unitSettings;

type
  TPropertyPageExtraPostingData = class (TPropertyPageData)
  private
    fSettings : TNNTPSettings;
  protected
    procedure Initialize; override;
  public
    destructor Destroy; override;
    function Apply : boolean; override;
  end;

  TfmPropertyPageExtraPosting = class(TfmPropertyPageDefaults)
    cbNoArchive: TCheckBox;
    cbGenerateDateHeaders: TCheckBox;
    cbGenerateMessageIDs: TCheckBox;
    Label1: TLabel;
    edMessageIDStub: TEdit;
    edDomain: TEdit;
    Label2: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    Label38: TLabel;
    lbXHeaders: TListBox;
    btnModifyXHeaders: TButton;
    Label4: TLabel;
    cbIdentities: TComboBox;
    btnNewIdentity: TButton;
    cbGenerateApprovedHeaders: TCheckBox;
    Label6: TLabel;
    procedure btnModifyXHeadersClick(Sender: TObject);
    procedure controlClick(Sender: TObject);
    procedure edMessageIDStubChange(Sender: TObject);
    procedure btnNewIdentityClick(Sender: TObject);
    procedure cbIdentitiesChange(Sender: TObject);
  private
    fData : TPropertyPageExtraPostingData;
    procedure InitializeControls (settings : TNNTPSettings);
    procedure UpdateData;
  protected
    function CanRestoreParentSettings : boolean; override;
    procedure RestoreParentSettings; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageExtraPosting: TfmPropertyPageExtraPosting;

implementation

uses AdvancedHeadersDialog, unitNNTPServices, IdentityDialog;

{$R *.dfm}

{ TfmPropertyPageExtraPosting }

function TfmPropertyPageExtraPosting.CanRestoreParentSettings: boolean;
var
  settings : TNNTPSettings;
begin
  settings := TNNTPSettings (fData.Param);
  result := Assigned (settings.Parent);
end;

class function TfmPropertyPageExtraPosting.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageExtraPostingData;
end;

procedure TfmPropertyPageExtraPosting.InitializeControls(
  settings: TNNTPSettings);
var
  i, idx : Integer;
begin
  idx := -1;
  for i := 0 to NNTPAccounts.Identities.Count - 1 do
  begin
    cbIdentities.Items.Add(NNTPAccounts.Identities [i].Name);
    if NNTPAccounts.Identities [i].Name = settings.Identity.Name then
      idx := i
  end;

  if idx > -1 then
    cbIdentities.ItemIndex := idx;

  lbxHeaders.Items.Text := settings.AdvancedHeaders;
  cbNoArchive.Checked := settings.NoArchive;
  cbGenerateDateHeaders.Checked := settings.GenerateDateHeaders;
  cbGenerateApprovedHeaders.Checked := settings.GenerateApprovedHeaders;
  cbGenerateMessageIDs.Checked := settings.GenerateMessageIDs;
  edMessageIDStub.Text := settings.MessageIDStub;
  edDomain.Text := settings.MessageIDDomain;
  if edDomain.Text = '' then
    edDomain.Text := '<Auto>';
  btnReset.Visible := Assigned (settings.Parent);
end;

procedure TfmPropertyPageExtraPosting.PopulateControls(
  AData: TPropertyPageData);
var
  settings : TNNTPSettings;
begin
  inherited;
  fData := AData as TPropertyPageExtraPostingData;
  settings := fData.fSettings;
  InitializeControls (settings);
end;

procedure TfmPropertyPageExtraPosting.RestoreParentSettings;
begin
  fData.fSettings.Assign(fData.fSettings.Parent);
  Populating := True;
  InitializeControls (fData.fSettings);
  Populating := False;
end;

procedure TfmPropertyPageExtraPosting.UpdateData;
var
  st : string;
begin
  if Populating then Exit;
  fData.fSettings.NoArchive := cbNoArchive.Checked;
  fData.fSettings.GenerateDateHeaders := cbGenerateDateHeaders.Checked;
  fData.fSettings.GenerateMessageIDs := cbGenerateMessageIDs.Checked;
  fData.fSettings.MessageIDStub := edMessageIDStub.Text;
  st := edDomain.Text;
  if st = '<Auto>' then
    st := '';
  fData.fSettings.MessageIDDomain := st;
  fData.fSettings.GenerateApprovedHeaders := cbGenerateApprovedHeaders.Checked;
end;

{ TPropertyPageExtraPostingData }

function TPropertyPageExtraPostingData.Apply : boolean;
var
  settings : TNNTPSettings;
begin
  result := True;
  settings := TNNTPSettings (Param);

  settings.SetIdentityName(fSettings.Identity.Name);
  settings.NoArchive := fSettings.NoArchive;
  settings.GenerateDateHeaders := fSettings.GenerateDateHeaders;
  settings.GenerateMessageIDs := fSettings.GenerateMessageIDs;
  settings.MessageIDStub := fSettings.MessageIDStub;
  settings.MessageIDDomain := fSettings.MessageIDDomain;
  settings.AdvancedHeaders := fSettings.AdvancedHeaders;
  settings.GenerateApprovedHeaders := fSettings.GenerateApprovedHeaders;
end;

destructor TPropertyPageExtraPostingData.Destroy;
begin
  fSettings.Free;

  inherited;
end;

procedure TPropertyPageExtraPostingData.Initialize;
var
  settings : TNNTPSettings;
begin
  settings := TNNTPSettings (Param);
  fSettings := TNNTPSettings.Create(settings.Parent);
  fSettings.Assign(settings);
end;

procedure TfmPropertyPageExtraPosting.btnModifyXHeadersClick(
  Sender: TObject);
var
  dlg : TdlgAdvancedHeaders;
begin
  dlg := TdlgAdvancedHeaders.Create (nil);
  try
    dlg.mmoAdvancedHeaders.Text := fData.fSettings.AdvancedHeaders;
    if dlg.ShowModal = mrOK then
    begin
      fData.fSettings.AdvancedHeaders := dlg.mmoAdvancedHeaders.Text;
      lbxHeaders.Items.Text := fData.fSettings.AdvancedHeaders;
    end
  finally
    dlg.Free
  end
end;

procedure TfmPropertyPageExtraPosting.controlClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPageExtraPosting.edMessageIDStubChange(
  Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPageExtraPosting.btnNewIdentityClick(Sender: TObject);
var
  st : string;
begin
  if DoAddIdentityDialog (self, st) then
  begin
    cbIdentities.Items.Add(st);
    cbIdentities.Text := st
  end
end;

procedure TfmPropertyPageExtraPosting.cbIdentitiesChange(Sender: TObject);
begin
  if cbIdentities.ItemIndex >= 0 then
    fData.fSettings.SetIdentityName(cbIdentities.Items [cbIdentities.ItemIndex]);
end;

end.
