unit ReplyByMailForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, unitNNTPServices, unitMailServices, StdCtrls, ComCtrls, ExtCtrls, MAPI, unitNNTPThreadManager,
  cmpCWRichEdit, unitSettings, PostFrame, unitIdentities, ConTnrs, NewsGlobals, unitNewsThread,
  cmpPersistentPosition, unitExSettings, Menus;

// TODO: email (MAPI) vs Unicode!?

type
  TfmReplyByMail = class(TForm)
    edSubject: TEdit;
    edBCC: TEdit;
    edCC: TEdit;
    edTo: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    PersistentPosition1: TPersistentPosition;
    fmePost1: TfmePost;
    procedure PersistentPosition1GetSettingsFile(Owner: TObject;
      var fileName: string);
    procedure PersistentPosition1GetSettingsClass(Owner: TObject;
      var SettingsClass: TExSettingsClass);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure WMPostAndClose (var msg : TMessage); message WM_POSTANDCLOSE;
    procedure WMSetCodePage (var msg : TMessage); message WM_SETCODEPAGE;
    procedure WmSetup (var msg : TMessage); message WM_SETUP;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    fReplyToArticle: TArticleBase;
    fInitialText: string;
    fGroup: TSubscribedGroup;
    fPostingSettings : TPostingSettings;
    fAttachments : TObjectList;
    fIsReply : boolean;
    fOrigMessageID : string;
    fArticleContainer: TServerAccount;
    fEMailerRequest: TEMailerRequest;

    procedure SendSMTPMail (settings : TSMTPServerSettings; const st : string; codePage : Integer; AUseOutbasket : boolean);
  protected
    procedure CreateParams (var params : TCreateParams); override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    property ReplyToArticle : TArticleBase read fReplyToArticle write fReplyToArticle;
    property ArticleContainer : TServerAccount read fArticleContainer write fArticleContainer;
    property Group : TSubscribedGroup read fGroup write fGroup;
    property InitialText : string read fInitialText write fInitialText;
    property EMailerRequest : TEMailerRequest read fEMailerRequest write fEMailerRequest;
  end;

var
  fmReplyByMail: TfmReplyByMail;

implementation

uses unitNewsreaderOptions, unitCharsetMap;

{$R *.dfm}

const
  EM_AUTOURLDETECT = WM_USER + 91;


{ TfmReplyByMail }

constructor TfmReplyByMail.Create(AOwner: TComponent);
begin
  inherited;
  fmReplyByMail := Self;
  fAttachments := TObjectList.Create;
end;

destructor TfmReplyByMail.Destroy;
begin
  fmReplyByMail := nil;
  fPostingSettings.Free;
  fAttachments.Free;

  inherited;
end;

procedure TfmReplyByMail.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree
end;

procedure TfmReplyByMail.FormShow(Sender: TObject);
var
  sub, ato : string;
  codePageOverride : Integer;
begin
  AdjustFormConstraints (self);
  if Assigned (EMailerRequest) then
  begin
    ArticleContainer := EMailerRequest.ArticleContainer;
    // TODO: Maybe make similar changes like in NNTP?
//    fInitialText := StringToWideString (EMailerRequest.Msg, EMailerRequest.CodePage);
    fInitialText := EMailerRequest.Msg;

    edSubject.Text := EMailerRequest.MSubject;
    edTo.Text := EMailerRequest.MTo;
    edCC.Text := EMailerRequest.MCC;
    edBCC.Text := EMailerRequest.MBCC;
    codePageOverride := EMailerRequest.CodePage;
  end
  else
    codePageOverride := -1;

  fPostingSettings := TPostingSettings.Create(nil);
  fPostingSettings.Assign(ArticleContainer.PostingSettings);

  fmePost1.Initialize(fInitialText, fPostingSettings, ArticleContainer.Identity, ReplyToArticle, EMailerRequest, fAttachments, codePageOverride, '');

  fIsReply := Assigned (ReplyToArticle);

  if fIsReply then
  begin
    ato := ReplyToArticle.Header ['Reply-To'];
    if ato = '' then
      ato := ReplyToArticle.From;
    sub := ReplyToArticle.Subject;
    Caption := 'Reply by mail to ' + ato + ' - ' + sub;

    fOrigMessageID := ReplyToArticle.MessageId;

    while UpperCase (Copy (sub, 1, 3)) = 'RE:' do
      sub := Trim (Copy (sub, 4, MaxInt));

    edSubject.Text := 'Re: ' + sub;

    ActiveControl := fmePost1.mmoMessage;

    edTo.Text := ato;
    PostMessage (Handle, WM_SETUP, 1, 0);
  end
end;

procedure TfmReplyByMail.PersistentPosition1GetSettingsClass(Owner: TObject;
  var SettingsClass: TExSettingsClass);
begin
  SettingsClass := gExSettingsClass;
end;

procedure TfmReplyByMail.PersistentPosition1GetSettingsFile(Owner: TObject;
  var fileName: string);
begin
  fileName := gExSettingsFile;
end;

procedure TfmReplyByMail.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  params.ExStyle   := params.ExStyle or WS_EX_APPWINDOW;
  params.WndParent := Application.Handle;
end;

procedure TfmReplyByMail.SendSMTPMail (settings : TSMTPServerSettings; const st : string; codePage : Integer; AUseOutbasket : boolean);
var
  replyTo : string;
begin
  if Assigned (fReplyToArticle) then
    if fReplyToArticle.References <> '' then
      replyTo := fReplyToArticle.References + ' ' + fReplyToArticle.MessageId
    else
      replyTo := fReplyToArticle.MessageId;

  if Assigned (EMailerRequest) then
  begin
    EMailerRequest.MTo := edTo.Text;
    EMailerRequest.MCC := edCC.Text;
    EMailerRequest.MBCC := edBCC.Text;
    EMailerRequest.MSubject := edSubject.Text;
    EMailerRequest.Msg := st;
    EMailerRequest.CodePage := codepage;
    EMailerRequest.Attachments.Free;
    EMailerRequest.Attachments := fAttachments;
  end
  else
    ThreadManager.SendSMTPMail(ArticleContainer, settings,
      edTo.Text, edCC.Text, edBCC.Text, edSubject.Text, replyTo, st,
      fAttachments, codePage, AUseOutbasket);

  fAttachments := Nil;  // The EMailerRequest now owns the attachments - so make
                        // sure we don't free them here.
end;

procedure TfmReplyByMail.WMPostAndClose(var msg: TMessage);
var
  rv : HRESULT;
  mmsg : TMAPIMessage;
  attach : array of TMapiFileDesc;
  recips : array of TMapiRecipDesc;
  st, raddr : string;
  taskWindows : pointer;
  ActiveWindow : HWND;
  FocusState : TFocusState;
  oldCursor : TCursor;
  useMAPI : boolean;
  codePage : Integer;
  toName, toEMail : string;
  mailAccount : TMailAccount;
  I: Integer;
begin
  st := PChar (msg.WParam);
  codePage := msg.LParam;

  useMAPI := False;

  mailAccount := Nil;
  if ArticleContainer is TSubscribedGroup then
  begin
    useMapi := TSubscribedGroup (ArticleContainer).Owner.MailAccountName = 'MAPI';
    if not useMapi then
      mailAccount := MailAccounts.FindMailAccount(TSubscribedGroup (ArticleContainer).Owner.MailAccountName)
    else
      mailAccount := Nil
  end
  else
    if ArticleContainer is TMailAccount then
      mailAccount := TMailAccount (ArticleContainer);

  if (not useMAPI) and Assigned (mailAccount) then
  begin
    SendSMTPMail (mailAccount.ServerSettings as TSMTPServerSettings, st, codepage, mailAccount.PostingSettings.DelayPosting);
    Close;
    Exit
  end;

  if useMAPI then
  begin
    FillChar (mmsg, SizeOf (mmsg), 0);
    mmsg.lpszSubject := PAnsiChar(AnsiString(edSubject.Text));
    mmsg.lpszNoteText := PAnsiChar(AnsiString(st));
    mmsg.lpOriginator := nil;

    if edCC.Text <> '' then
      mmsg.nRecipCount := 2
    else
      mmsg.nRecipCount := 1;

    SetLength (recips, mmsg.nRecipCount);
    mmsg.lpRecips := @recips [0];

    DecodeFromEMail(RawByteString(edTo.Text), toName, toEMail, -1);
    FillChar (recips [0], mmsg.nRecipCount * sizeof (TMapiRecipDesc), 0);
    recips [0].ulRecipClass := MAPI_TO;
    recips [0].lpszName := PAnsiChar(AnsiString(toName));
    raddr := 'SMTP:' + toEMail;
    recips [0].lpszAddress := PAnsiChar(AnsiString(raddr));

    SetLength(attach, fAttachments.Count);
    FillChar(attach[0], fAttachments.Count * SizeOf(TMapiFileDesc), 0);
    mmsg.lpFiles := @attach[0];
    mmsg.nFileCount := fAttachments.Count;
    for I := 0 to fAttachments.Count - 1 do
    begin
      attach[I].nPosition := $FFFFFFFF;
      attach[I].lpszPathName := PAnsiChar(AnsiString(TAttachment(fAttachments[I]).PathName));
    end;

    ActiveWindow := GetActiveWindow;
    FocusState := SaveFocusState;
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    taskWindows := DisableTaskWindows (0);
    try
      rv := MAPISendMail (0, 0, mmsg, MAPI_LOGON_UI, 0);
    finally
      EnableTaskWindows (taskWindows);
      SetActiveWindow(ActiveWindow);
      RestoreFocusState(FocusState);
      Screen.Cursor := oldCursor;
    end;
    case rv of
      0 : Close;
      MAPI_E_USER_ABORT :;
      else
      begin
        MessageBeep (rv)
      end
    end
  end
end;

procedure TfmReplyByMail.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if ssCtrl in Shift then
      fmePost1.btnOK.Click;
end;

procedure TfmReplyByMail.FormResize(Sender: TObject);
begin
  fmePost1.DoResize
end;

procedure TfmReplyByMail.WMSetCodePage(var msg: TMessage);
var
  charset : TFontCharset;
begin
  charset := CodePageToCharset (msg.WParam);
  self.Font.Charset := charset;
end;

procedure TfmReplyByMail.WmSetup(var msg: TMessage);
begin
  case msg.WParam of
    0 : edSubject.SetFocus;
    1 :
      begin
        fmePost1.mmoMessage.SetFocus;
        SendMessage (fmePost1.mmoMessage.Handle, EM_SCROLLCARET, 0, 0);
      end
  end;

  fmePost1.TabStop := False
end;

end.
