unit AddAccountWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, GifImg, StdCtrls;

type
  TfmAddAccountWizard = class(TForm)
    Image1: TImage;
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edAccountName: TEdit;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edServerName: TEdit;
    TabSheet3: TTabSheet;
    Label9: TLabel;
    Label10: TLabel;
    cbLogonRequired: TCheckBox;
    pnlLogOnDetails: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    edUserName: TEdit;
    edPassword: TEdit;
    TabSheet4: TTabSheet;
    Label13: TLabel;
    Label14: TLabel;
    stRetypePassword: TLabel;
    edRetypePassword: TEdit;
    stPasswordError: TLabel;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function CurrentPageOK : boolean;
  protected
    procedure UpdateActions; override;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAddAccountWizard: TfmAddAccountWizard;

implementation

uses NewsGlobals, unitNNTPServices, unitNewsReaderOptions;

{$R *.dfm}

procedure TfmAddAccountWizard.FormShow(Sender: TObject);
var
  g : TGIFImage;
  rs : TResourceStream;
begin
  if XNOptions.PlaintextPasswords then
  begin
    edRetypePassword.Visible := False;
    stRetypePassword.Visible := False
  end
  else
    edPassword.PasswordChar := '*';
  g := Nil;
  try
    rs := TResourceStream.Create(HInstance, Application.Title, 'GIF');
    try
      if rs.Size > 0 then
      begin
        g := TGifImage.Create;
        g.LoadFromStream(rs);
        Image1.Picture.Assign(g)
      end
    finally
      g.Free;
      rs.Free
    end;
  except
  end;


  if g = Nil then
    if Assigned (Application.Icon) then
      Image1.Picture.Icon := Application.Icon;

  PageControl1.ActivePageIndex := 0;
  PageControl1Change (nil);
end;

procedure TfmAddAccountWizard.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0 : ActiveControl := edAccountName;
    1 : ActiveControl := edServerName;
    2 : ActiveControl := cbLogonRequired;
  end
end;

procedure TfmAddAccountWizard.btnBackClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePageIndex > 0 then
    begin
      ActivePageIndex := ActivePageIndex - 1;
      PageControl1Change (nil)
    end
end;

procedure TfmAddAccountWizard.btnNextClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePageIndex < PageCount - 1 then
    begin
      ActivePageIndex := ActivePageIndex + 1;
      PageControl1Change (nil)
    end
end;

procedure TfmAddAccountWizard.UpdateActions;
var
  activePage : Integer;
  ok : boolean;
begin
  activePage := PageControl1.ActivePageIndex;
  btnNext.Enabled := CurrentPageOK and (activePage < PageControl1.PageCount);
  btnBack.Enabled := activePage > 0;

  if activePage = PageControl1.PageCount -1 then
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

  if (activePage = 2) and cbLogonRequired.Checked and not XNOptions.PlainTextPasswords then
  begin
    ok := edPassword.Text = edRetypePassword.Text;
    btnNext.Enabled := ok;
    stPasswordError.Visible := not ok;
  end
  else
    stPasswordError.Visible := False;

  pnllogonDetails.Visible := cbLogonRequired.Checked
end;

function TfmAddAccountWizard.CurrentPageOK: boolean;
var
  i : Integer;
  st : string;
begin
  result := True;
  case PageControl1.ActivePageIndex of
    0 : begin
          st := Trim (edAccountName.Text);
          result := st <> '';
          if result then
            for i := 0 to NNTPAccounts.Count - 1 do
              if SameText (NNTPAccounts [i].AccountName, st) then
              begin
                result := False;
                break
              end
        end;
    1 : result := edServerName.Text <> '';
  end
end;

end.
