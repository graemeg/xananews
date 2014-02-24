unit NewUserWizardForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, GifImg;

type
  TfrmNewUserWizard = class(TForm)
    PageControl1: TPageControl;
    tsUserDetails: TTabSheet;
    tsServerDetails: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    edYourName: TEdit;
    edYourEMail: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edAccountName: TEdit;
    edServerName: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    tsServerLogon: TTabSheet;
    Label13: TLabel;
    Label14: TLabel;
    sb1: TLabel;
    edServerUserName: TEdit;
    sb2: TLabel;
    edServerPassword: TEdit;
    sb3: TLabel;
    sb4: TLabel;
    cbServerLogon: TCheckBox;
    tsConnection: TTabSheet;
    Label19: TLabel;
    Label20: TLabel;
    tsFinished: TTabSheet;
    cbAlwaysConnectUsing: TCheckBox;
    cbRasEntries: TComboBox;
    Label21: TLabel;
    Label22: TLabel;
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure cbAlwaysConnectUsingClick(Sender: TObject);
    procedure cbServerLogonClick(Sender: TObject);
  private
    function CurrentPageOK: boolean;
  protected
    procedure UpdateActions; override;
  public
  end;

var
  frmNewUserWizard: TfrmNewUserWizard;

implementation

uses
  NewsGlobals;

{$R *.dfm}

procedure TfrmNewUserWizard.FormShow(Sender: TObject);
var
  i: Integer;
  g: TGIFImage;
  rs: TResourceStream;
begin
  g := nil;
  try
    rs := TResourceStream.Create(HInstance, Application.Title, 'GIF');
    try
      if rs.Size > 0 then
      begin
        g := TGIFImage.Create;
        g.LoadFromStream(rs);
        Image1.Picture.Assign(g);
      end;
    finally
      g.Free;
      rs.Free;
    end;
  except
  end;

  if g = nil then
    if Assigned(Application.Icon) then
      Image1.Picture.Icon := Application.Icon;

  LoadRASEntries;
  PageControl1.ActivePageIndex := 0;
  PageControl1Change(nil);
  for i := 0 to Length(RasEntries) - 1 do
    cbRasEntries.Items.Add(RasEntries[i].szEntryName);
end;

procedure TfrmNewUserWizard.btnBackClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePageIndex > 0 then
    begin
      ActivePageIndex := ActivePageIndex - 1;
      PageControl1Change(nil);
    end;
end;

procedure TfrmNewUserWizard.btnNextClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePageIndex < PageCount - 1 then
    begin
      ActivePageIndex := ActivePageIndex + 1;
      PageControl1Change(nil);
    end;
end;

procedure TfrmNewUserWizard.UpdateActions;
var
  activePage: Integer;
begin
  activePage := PageControl1.ActivePageIndex;
  btnNext.Enabled := CurrentPageOK and (activePage < PageControl1.PageCount);
  btnBack.Enabled := activePage > 0;

  if activePage = PageControl1.PageCount - 1 then
  begin
    btnNext.Caption := rstOK;
    btnNext.Default := True;
    btnNext.ModalResult := mrOK;
  end
  else
  begin
    btnNext.Caption := rstNext;
    btnNext.Default := False;
    btnNext.ModalResult := mrNone;
  end;
end;

function TfrmNewUserWizard.CurrentPageOK: boolean;
begin
  Result := True;
  case PageControl1.ActivePageIndex of
    0: Result := edYourName.Text <> '';
    1: Result := (edServerName.Text <> '') and (edAccountName.Text <> '');
    2: Result := (not cbServerLogon.Checked) or
        ((edServerUserName.Text <> '') and (edServerPassword.Text <> ''));
    3: Result := (not cbAlwaysConnectUsing.Checked) or (cbRasEntries.Text <> '');
  end;
end;

procedure TfrmNewUserWizard.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: ActiveControl := edYourName;
    1: ActiveControl := edAccountName;
    2: ActiveControl := cbServerLogon;
    3: ActiveControl := cbAlwaysConnectUsing;
  end;
end;

procedure TfrmNewUserWizard.cbAlwaysConnectUsingClick(Sender: TObject);
begin
  cbRasEntries.Enabled := cbAlwaysConnectUsing.Checked;
end;

procedure TfrmNewUserWizard.cbServerLogonClick(Sender: TObject);
var
  enable: boolean;
begin
  enable := cbServerLogon.Checked;

  sb1.Enabled := enable;
  sb2.Enabled := enable;
  sb3.Enabled := enable;
  sb4.Enabled := enable;

  edServerUserName.Enabled := enable;
  edServerPassword.Enabled := enable;
end;

end.
