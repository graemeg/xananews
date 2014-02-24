unit IdentityDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ExtDlgs, unitIdentities, Menus;

type
  TdlgIdentity = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    edUserName: TEdit;
    Label3: TLabel;
    edOrganization: TEdit;
    Label4: TLabel;
    edEMailAddress: TEdit;
    Label5: TLabel;
    edReplyAddress: TEdit;
    Label14: TLabel;
    mmoSignature: TMemo;
    Panel1: TPanel;
    imgXFace: TImage;
    Label37: TLabel;
    btnLoadXFace: TButton;
    btnClearXFace: TButton;
    Label6: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edSigFile: TEdit;
    pomXFace: TPopupMenu;
    actXFaceCopy: TMenuItem;
    actXFaceCopyAsText: TMenuItem;
    actXFacePaste: TMenuItem;
    btnSigFile: TButton;
    OpenDialog1: TOpenDialog;
    Label11: TLabel;
    procedure btnLoadXFaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edReplyAddressEnter(Sender: TObject);
    procedure btnClearXFaceClick(Sender: TObject);
    procedure actXFaceCopyClick(Sender: TObject);
    procedure actXFaceCopyAsTextClick(Sender: TObject);
    procedure actXFacePasteClick(Sender: TObject);
    procedure btnSigFileClick(Sender: TObject);
  private
    fXFace: string;
    fIdentity: TIdentity;
    fDefaultIdentity: Boolean;

    procedure DisplayXFace;
    procedure LoadXFace(pic: TPicture);
  protected
    procedure UpdateActions; override;
  public
    property Identity: TIdentity read fIdentity write fIdentity;
    property DefaultIdentity: Boolean read fDefaultIdentity write fDefaultIdentity;
  end;

var
  dlgIdentity: TdlgIdentity;

function DoAddIdentityDialog(Owner: TComponent; var name: string): Boolean;

implementation

{$R *.dfm}

uses
  Registry, GIFImg, XnXFace, NewsGlobals, unitNNTPServices, ClipBrd;

procedure TdlgIdentity.btnLoadXFaceClick(Sender: TObject);
var
  pic: TPicture;
begin
  if OpenPictureDialog1.Execute then
  begin
    pic := TPicture.Create;
    try
      pic.LoadFromFile(OpenPictureDialog1.FileName);
      LoadXFace(pic);
    finally
      pic.Free;
    end;
  end;
end;

procedure TdlgIdentity.DisplayXFace;
var
  bmp: TBitmap;
  res: Integer;
begin
  bmp := TBitmap.Create;
  try
    bmp.Width := 48;
    bmp.Height := 48;
    bmp.PixelFormat := pf1Bit;
    if fXFace <> '' then
      res := XFaceToBitmap(fXFace, bmp)
    else
      res := 0;

    if res = 0 then
      imgXFace.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TdlgIdentity.FormShow(Sender: TObject);
var
  Owner, organization: string;

  procedure GetRegistrationInformation(var Owner, organization: string);
  var
    product: string;
  begin
    with TRegistry.Create(KEY_READ) do // Checked
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          product := 'Windows NT'
        else
          product := 'Windows';

        OpenKey(Format('Software\Microsoft\%s\CurrentVersion', [product]), False);
        Owner := ReadString('RegisteredOwner');
        organization := ReadString('RegisteredOrganization');
        Free;
      except
        Free;
      end;
  end;

begin
  AdjustFormConstraints(self);
  if Identity.Name = '' then
  begin
    if DefaultIdentity then
      Identity.ChangeName(rstDefaultIdentity);
    GetRegistrationInformation(Owner, organization);
    Identity.UserName := Owner;
    Identity.Organization := organization
  end;

  edName.Text := Identity.Name;
  Caption := Identity.Name;
  edUserName.Text := Identity.UserName;
  edEMailAddress.Text := Identity.EMailAddress;
  edReplyAddress.Text := Identity.ReplyAddress;
  edOrganization.Text := Identity.Organization;
  mmoSignature.Text := Identity.Signature;
  edSigFile.Text := Identity.SigFile;
  fXFace := Identity.XFace;
  DisplayXFace;
end;

procedure TdlgIdentity.btnOKClick(Sender: TObject);
begin
  Identity.ChangeName(edName.Text);
  Identity.UserName := edUserName.Text;
  Identity.EMailAddress := edEMailAddress.Text;
  Identity.ReplyAddress := edReplyAddress.Text;
  Identity.Organization := edOrganization.Text;
  Identity.Signature := mmoSignature.Text;
  Identity.SigFile := edSigFile.Text;
  Identity.XFace := fXFace;
end;

procedure TdlgIdentity.edReplyAddressEnter(Sender: TObject);
begin
  if edReplyAddress.Text = '' then
    edReplyAddress.Text := edEMailAddress.Text;
end;

function DoAddIdentityDialog(Owner: TComponent; var name: string): Boolean;
var
  dlg: TdlgIdentity;
  freeId: Boolean;
begin
  Result := False;
  freeId := False;
  dlg := TdlgIdentity.Create(Owner);
  try
    dlg.Identity := TIdentity.Create;
    freeId := True;
    if (dlg.ShowModal = idOK) and (dlg.Identity.Name <> '') then
      if not Assigned(NNTPAccounts.Identities.Find(dlg.Identity.Name)) then
      begin
        NNTPAccounts.Identities.Add(dlg.Identity);
        freeId := False;
        name := dlg.Identity.Name;
        Result := True;
      end;
  finally
    if freeId then
      dlg.Identity.Free;
    dlg.Free;
  end;
end;

procedure TdlgIdentity.btnClearXFaceClick(Sender: TObject);
begin
  fXFace := '';
  DisplayXFace;
end;

procedure TdlgIdentity.UpdateActions;
var
  enable: Boolean;
  i: Integer;
begin
  enable := (edName.Text <> '') and (edUserName.Text <> '');

  if enable then
    for i := 0 to NNTPAccounts.Identities.Count - 1 do
      if (NNTPAccounts.Identities[i] <> Identity) and
        (CompareText(NNTPAccounts.Identities[i].Name, edName.Text) = 0) then
      begin
        enable := False;
        Break;
      end;

  btnOK.Enabled := enable;
  actXFaceCopy.Enabled := fXFace <> '';
  actXFaceCopyAsText.Enabled := fXFace <> '';
end;

procedure TdlgIdentity.actXFaceCopyClick(Sender: TObject);
begin
  Clipboard.Assign(imgXFace.Picture);
end;

procedure TdlgIdentity.actXFaceCopyAsTextClick(Sender: TObject);
begin
  Clipboard.AsText := fXFace;
end;

procedure TdlgIdentity.LoadXFace(pic: TPicture);
var
  bmp, bmp1, tmp: TBitmap;
  bmp1Created: Boolean;
  XFace: string;
begin
  bmp := nil;
  bmp1 := nil;
  bmp1Created := False;
  try
    if (pic.Graphic is TBitmap) then
      bmp1 := pic.Bitmap;

    if not Assigned(bmp1) or (bmp1.PixelFormat <> pf1Bit) then
    begin
      tmp := TBitmap.Create;
      try
        tmp.Width := pic.Graphic.Width;
        tmp.Height := pic.Graphic.Height;
        tmp.PixelFormat := pf24Bit;
        tmp.Canvas.Draw(0, 0, pic.Graphic);
        bmp1 := ReduceColors(tmp, rmMonochrome, dmNearest, 1, 0);
        bmp1Created := True;
      finally
        tmp.Free;
      end;
    end;

    bmp := TBitmap.Create;
    bmp.Width := 48;
    bmp.Height := 48;
    bmp.PixelFormat := bmp1.PixelFormat;

    if (bmp1.Width < bmp.Width) and (bmp1.Height < bmp.Height) then
      bmp.Canvas.Draw((bmp.Width - bmp1.Width + 1) div 2, (bmp.Height - bmp1.Height + 1) div 2, bmp1)
    else
      bmp.Canvas.StretchDraw(Rect(0, 0, 48, 48), bmp1);

    if BitmapToXFace(bmp, XFace) = 0 then
    begin
      fXFace := Trim(XFace);
      fXFace := StringReplace(fXFace, #13#10' ', '', [rfReplaceAll]);
      DisplayXFace;
    end;
  finally
    bmp.Free;
    if bmp1Created then
      bmp1.Free;
  end;
end;

procedure TdlgIdentity.actXFacePasteClick(Sender: TObject);
var
  pic: TPicture;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    fXFace := Clipboard.AsText;
    DisplayXFace
  end
  else
  begin
    pic := TPicture.Create;
    try
      pic.Assign(Clipboard);
      LoadXFace(pic)
    finally
      pic.Free;
    end;
  end;
end;

procedure TdlgIdentity.btnSigFileClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := Identity.GetFullyQualifiedSigFilename(edSigFile.Text);
  OpenDialog1.InitialDir := ExtractFilePath(FileName);
  OpenDialog1.FileName := ExtractFilename(FileName);
  if OpenDialog1.Execute then
    edSigFile.Text := OpenDialog1.FileName;
end;

end.
