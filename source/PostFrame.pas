unit PostFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, Buttons, Contnrs, cmpCWRichEdit, cmpCWSpellChecker,
  cmpSpellChecker, cmpRuler, unitIdentities, unitSettings, unitNNTPServices,
  unitNewsThread, NewsGlobals;

type
  TfmePost = class(TFrame)
    ScrollBox1: TScrollBox;
    Ruler1: TXNRuler;
    mmoMessage: TExRichEdit;
    cbCheckSpelling: TCheckBox;
    cbCharset: TComboBox;
    PopupMenu1: TPopupMenu;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    N1: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuPasteQuote: TMenuItem;
    mnuDelete: TMenuItem;
    ROT13SelectedText1: TMenuItem;
    N2: TMenuItem;
    mnuSelectAll: TMenuItem;
    CWSpellChecker1: TCWSpellChecker;
    btnCancel: TButton;
    btnOK: TButton;
    btnAttachments: TButton;
    btnAdvanced: TButton;
    btnSpell: TBitBtn;
    ReverseSelectedText1: TMenuItem;
    mnuPasteSelected: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    cbIdentity: TComboBox;
    lbISpellLanguage: TLabel;
    cbISpellLanguage: TComboBox;
    procedure btnAttachmentsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSpellClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuPasteQuoteClick(Sender: TObject);
    procedure mnuPasteSelectedClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure ROT13SelectedText1Click(Sender: TObject);
    procedure cbCharsetChange(Sender: TObject);
    procedure cbIdentityChange(Sender: TObject);
    procedure mmoMessageFontChange(Sender: TObject);
    procedure mmoMessageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReverseSelectedText1Click(Sender: TObject);
  private
    fInitialText: string;
    fInitialIdentity: string;
    fInitialP: Integer;
    fCodePage: Integer;
    fIsReply: Boolean;
    fIsExistingMessage: Boolean;
    fOrigReferences: string;
    fOrigMessageID: string;
    fSpellInstalled: Boolean;
    fCheckSpelling: Boolean;
    fPostingSettings: TPostingSettings;
    fResizing: Boolean;
    fAttachments: TObjectList;
    fCloseOK: Boolean;
    fSignatureOverride: string;

    procedure PostAndClose;
    function GetAttachment(idx: Integer): TAttachment;
    function GetAttachmentCount: Integer;
    procedure PasteQuote(const quote: string);
    procedure ApplySignature(Identity: TIdentity; const signatureOverride: string);
  public
    procedure AfterConstruction; override;
    function CanClose(ShowDlg: Boolean = True): Boolean;
    procedure Initialize(const InitialText: string; PostingSettings: TPostingSettings; identity: TIdentity; ReplyTOArticle: TArticleBase; Request: TObject; attachments: TObjectList; codePageOverride: Integer; const signatureOverride: string);
    procedure UpdateActions(okOK: Boolean);
    procedure DoResize;
    property AttachmentCount: Integer read GetAttachmentCount;
    property Attachment[idx: Integer]: TAttachment read GetAttachment;
    procedure AddAttachment(const fileName: string);
    procedure RemoveAttachment(idx: Integer);
    property CodePage: Integer read fCodePage;
    property CloseOK: Boolean read fCloseOK;
  end;

implementation

uses
  unitNewsreaderOptions, unitCharsetMap, ClipBrd,
  AttachmentsDialog, MainForm, SpellCheckerForm, unitSearchString;

{$R *.dfm}

procedure TfmePost.btnOKClick(Sender: TObject);
var
  okToPost: Boolean;
  skipFirstLine: Boolean;
begin
  if not CanClose then Exit;

  okToPost := True;
  if (mmoMessage.Text = '') and (AttachmentCount = 0) then
    if MessageBox(Handle, 'Are you sure you want to post an empty message', PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION) <> IDYES then
      okToPost := False;

  if okToPost then
  begin
    if fSpellInstalled then
    begin
      CWSpellChecker1.LanguageIdx := Integer(cbISpellLanguage.Items.Objects[cbISpellLanguage.ItemIndex]);
      CWSpellChecker1.QuoteChars := '>|';
    end;
    skipFirstLine := fIsReply and
                     (fPostingSettings.PostingStyle <> psTop) and
                     ((fPostingSettings.QuoteHeader <> '') or (fPostingSettings.QuoteSalutation <> ''));
    if not cbCheckSpelling.Checked or (CWSpellChecker1.CheckAndShowModal(Owner as TCustomForm, skipFirstLine) = mrOK) then
      PostAndClose
    else
      if Assigned(mmoMessage) then
        mmoMessage.SetFocus;
  end;
end;

procedure TfmePost.DoResize;
var
  w, h, cxvs: Integer;
begin
  if fResizing or not Assigned(fPostingSettings) then
    Exit;

  if mmoMessage.WordWrap and (fPostingSettings.MaxPostLineLength > 0) then
  begin
    cxvs := GetSystemMetrics(SM_CXVSCROLL);
    w := (fPostingSettings.MaxPostLineLength + 1) * mmoMessage.AveCharWidth + cxvs;
    if w < ScrollBox1.ClientWidth then
      w := ScrollBox1.ClientWidth;
    ScrollBox1.HorzScrollBar.Range := w;
  end
  else
    w := ScrollBox1.ClientWidth;

  h := ScrollBox1.ClientHeight;
  if Ruler1.Visible then
    Dec(h, Ruler1.Height);

  fResizing := True;
  try
    mmoMessage.Width := w;
    mmoMessage.Height := h;
    Ruler1.Width := mmoMessage.Width;
    mmoMessage.Invalidate;
    if mmoMessage.WordWrap and (fPostingSettings.MaxPostLineLength > 0) then
      mmoMessage.SetupRightMargin;
  finally
    fResizing := False;
  end;
end;

procedure TfmePost.Initialize(const InitialText: string; PostingSettings: TPostingSettings;identity: TIdentity; ReplyTOArticle: TArticleBase; Request: TObject; attachments: TObjectList; codePageOverride: Integer; const signatureOverride: string);
var
  sub: string;
  i, idx: Integer;
  att: TObjectList;
begin
  fSignatureOverride := SignatureOverride;
  mmoMessage.RawPaste := True;
  fCloseOK := True;
  fPostingSettings := PostingSettings;
  fAttachments := Attachments;
  fInitialIdentity := Identity.Name;
  for i := 0 to NNTPAccounts.Identities.Count - 1 do
    cbIdentity.Items.Add(NNTPAccounts.Identities.Identity[i].Name);

  idx := cbIdentity.Items.IndexOf(fInitialIdentity);
  if idx <> -1 then
    cbIdentity.ItemIndex := idx;

  mmoMessage.Color := XNOptions.Appearance[apMessageEditor].ApplyFontAndGetColor(mmoMessage.Font);
  fInitialP := 0;
  fCodePage := PostingSettings.DefaultCodePage;
  GetCharsetNames(cbCharset.Items);
  fIsReply := Assigned(ReplyToArticle);

  if fIsReply then
  begin
    sub := ReplyToArticle.Subject;
    Caption := 'Reply to article from ' + ReplyToArticle.FromName + ' - ' + sub;
    sub := Trim(sub);

    fOrigReferences := ReplyToArticle.References;
    fOrigMessageID := ReplyToArticle.MessageId;

    while UpperCase(Copy(sub, 1, 3)) = 'RE:' do
      sub := Trim(Copy(sub, 4, MaxInt));

    if PostingSettings.PostingStyle = psTop then
      mmoMessage.Text := #13#10#13#10 + InitialText
    else
      mmoMessage.Text := InitialText;
    fInitialP := mmoMessage.GetTextLen;
    fInitialText := mmoMessage.Text;

    if Assigned(ReplyToArticle.Msg) then
      fCodePage := ReplyToArticle.Msg.Codepage;
  end;
  if codePageOverride <> -1 then
    fCodePage := codePageOverride;
  mmoMessage.CodePage := fCodePage;

  idx := cbCharset.Items.IndexOf(CodePagetoCharsetName(fCodePage));
  cbCharset.ItemIndex := idx;

  if not Assigned(Request) then
  begin
    if not fIsReply then
      fInitialText := InitialText;
    ApplySignature(Identity, fSignatureOverride);
  end
  else
  begin
    fIsExistingMessage := True;
    att := nil;
    if Request is TPosterRequest then
    begin
      att := TPosterRequest(Request).Attachments;
      mmoMessage.Text := InitialText;
    end
    else
      if Request is TEmailerRequest then
      begin
        att := TEMailerRequest(Request).Attachments;
        mmoMessage.Text := TEmailerRequest(Request).Msg;
      end;

    if Assigned(att) then
      for i := 0 to att.Count - 1 do
        Attachments.Add(TAttachment.Create(TAttachment(att[i]).PathName));
  end;

  mmoMessage.RightMargin := PostingSettings.MaxPostLineLength;

  fSpellInstalled := DefaultISpellLanguage <> -1;
  fCheckSpelling := fSpellInstalled and XNOptions.CheckSpelling;

  if fSpellInstalled then
  begin
    cbISpellLanguage.Sorted := False;
    for i := 0 to TSpellChecker.LanguageCount - 1 do
      cbISpellLanguage.Items.AddObject(TSpellChecker.Language(i).name, TObject(i));
    cbISpellLanguage.Sorted := True;

    for i := 0 to cbISpellLanguage.Items.Count - 1 do
      if Integer(cbISpellLanguage.Items.Objects[i]) = fPostingSettings.DefaultSpellLanguage then
      begin
        cbISpellLanguage.ItemIndex := i;
        Break;
      end;
  end;

  cbCheckSpelling.Checked := fCheckSpelling;
  lbISpellLanguage.Enabled := fCheckSpelling;
  lbISpellLanguage.Visible := fSpellInstalled;
  cbISpellLanguage.Enabled := fCheckSpelling;
  cbISpellLanguage.Visible := fSpellInstalled;

  SendMessage(mmoMessage.Handle, CM_FONTCHANGED, 0, 0);

  mmoMessage.ClearUndoBuffer;

  SendMessage(Parent.Handle, WM_SETCODEPAGE, fCodePage, 0);
end;

procedure TfmePost.PostAndClose;
var
  st: string;
  s: TStringList;
begin
  fCloseOK := True;

  st := mmoMessage.Text;
  if (fPostingSettings.MaxPostLineLength > 0) and (fPostingSettings.TextPartStyle <> tpQuotedPrintable) and (fPostingSettings.TextPartStyle <> tpFlowed) then
  begin
    s := TStringList.Create;
    try
      s.Text := st;
      WrapStrings(s, fPostingSettings.MaxPostLineLength, tpNNTP, false, false);
      st := s.Text;
    finally
      s.Free;
    end;
  end;

  SendMessage(Parent.Handle, WM_POSTANDCLOSE, WPARAM(PChar(st)), fCodePage);
end;

procedure TfmePost.UpdateActions(okOK: Boolean);
begin
  btnOK.Enabled := okOK and (mmoMessage.GetTextLen <> 0);

  mnuUndo.Enabled := mmoMessage.CanUndo;
  mnuRedo.Enabled := mmoMessage.CanRedo;
  mnuCut.Enabled := mmoMessage.SelLength > 0;
  mnuCopy.Enabled := mmoMessage.SelLength > 0;
  mnuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
  mnuPasteQuote.Enabled := mnuPaste.Enabled;
  mnuDelete.Enabled := mmoMessage.SelLength > 0;
  mnuPasteSelected.Enabled := fmMain.MessageScrollBox1.SelLength > 0;

  btnSpell.Enabled := fSpellInstalled;
  cbCheckSpelling.Enabled := fSpellInstalled;
  cbISpellLanguage.Enabled := fSpellInstalled;
end;

procedure TfmePost.mnuUndoClick(Sender: TObject);
begin
  mmoMessage.Undo;
end;

procedure TfmePost.mnuCutClick(Sender: TObject);
begin
  mmoMessage.CutToClipboard;
end;

procedure TfmePost.mnuCopyClick(Sender: TObject);
begin
  mmoMessage.CopyToClipboard;
end;

procedure TfmePost.mnuPasteClick(Sender: TObject);
begin
  mmoMessage.PasteTextFromClipboard;
end;

procedure TfmePost.mnuSelectAllClick(Sender: TObject);
begin
  mmoMessage.SelectAll;
end;

procedure TfmePost.btnSpellClick(Sender: TObject);
var
  skipFirstLine: Boolean;
begin
  CWSpellChecker1.LanguageIdx := Integer(cbISpellLanguage.Items.Objects[cbISpellLanguage.ItemIndex]);
  CWSpellChecker1.QuoteChars := '>|';
  skipFirstLine := fIsReply and
                   (fPostingSettings.PostingStyle <> psTop) and
                   ((fPostingSettings.QuoteHeader <> '') or (fPostingSettings.QuoteSalutation <> ''));
  CWSpellChecker1.CheckAndShowModal(Owner as TCustomForm, skipFirstLine);
end;

procedure TfmePost.mnuPasteQuoteClick(Sender: TObject);
begin
  PasteQuote(Clipboard.AsText);
end;

procedure TfmePost.ROT13SelectedText1Click(Sender: TObject);
begin
  if mmoMessage.SelLength > 0 then
    mmoMessage.SelText := WideROT13(mmoMessage.SelText);
end;

procedure TfmePost.cbCharsetChange(Sender: TObject);
begin
  fCodePage := CharsetNameToCodePage(cbCharset.Text);
  mmoMessage.Font.Charset := CodePageToCharset(fCodePage);
  mmoMessage.CodePage := fCodePage;
  SendMessage(Parent.Handle, WM_SETCODEPAGE, fCodePage, 0);
end;

procedure TfmePost.mmoMessageFontChange(Sender: TObject);
begin
  if not Assigned(fPostingSettings) then Exit;
  Ruler1.Visible := mmoMessage.FixedFont;
  if Ruler1.Visible then
  begin
    mmoMessage.Top := Ruler1.Height;
    Ruler1.SmallTickSpacing := mmoMessage.AveCharWidth;
  end
  else
    mmoMessage.Top := 0;

  if (fPostingSettings.TextPartStyle = tpQuotedPrintable) or (fPostingSettings.TextPartStyle = tpFlowed) then
  begin
    mmoMessage.WordWrap := True;
    mmoMessage.HideScrollBars := True;
    mmoMessage.ScrollBars := ssVertical;
  end
  else
    if (fPostingSettings.MaxPostLineLength = 0) then
    begin
      mmoMessage.WordWrap := False;
      mmoMessage.ScrollBars := ssBoth;
      mmoMessage.HideScrollBars := True;
    end
    else
    begin
      mmoMessage.WordWrap := True;
      mmoMessage.HideScrollBars := False;
      mmoMessage.ScrollBars := ssVertical;
    end;
end;

procedure TfmePost.mnuRedoClick(Sender: TObject);
begin
  mmoMessage.Redo;
end;

procedure TfmePost.btnCancelClick(Sender: TObject);
begin
  if not CanClose then Exit;

  SendMessage(Parent.Handle, WM_CLOSE, 0, 0);
end;

procedure TfmePost.btnAttachmentsClick(Sender: TObject);
var
  dlg: TdlgAttachments;
begin
  dlg := TdlgAttachments.Create(Owner);
  try
    dlg.PopupParent := Owner as TCustomForm;
    dlg.PopupMode := pmExplicit;
    dlg.PostFrame := Self;
    if dlg.ShowModal = mrOK then
    begin
    end
  finally
    dlg.Free;
  end;
end;

procedure TfmePost.AddAttachment(const fileName: string);
begin
  fAttachments.Add(TAttachment.Create(fileName));
end;

function TfmePost.GetAttachment(idx: Integer): TAttachment;
begin
  Result := TAttachment(fAttachments[idx]);
end;

function TfmePost.GetAttachmentCount: Integer;
begin
  if Assigned(fAttachments) then
    Result := fAttachments.Count
  else
    Result := 0;
end;

function TfmePost.CanClose(ShowDlg: Boolean = True): Boolean;
var
  Component: TComponent;
  I: Integer;
  S: string;
begin
  Result := not Assigned(fmSpellChecker) or (fmSpellChecker.ModalResult <> 0);
  if Result then
  begin
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      Component := Owner.Components[I];
      if Component is TCustomForm then
      begin
        S := TCustomForm(Component).Caption;
        Result := False;
        Break;
      end;
    end;
  end
  else
    S := fmSpellChecker.Caption;

  if not Result and ShowDlg then
    MessageBox(Handle, PChar('[' + S + '] dialog is still open!'),
      PChar(Application.Title), MB_OK or MB_ICONWARNING);
end;

procedure TfmePost.RemoveAttachment(idx: Integer);
begin
  fAttachments.Delete(idx);
end;

procedure TfmePost.mmoMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    btnCancel.Click;

  fCloseOK := False;
end;

procedure TfmePost.ReverseSelectedText1Click(Sender: TObject);
begin
  if mmoMessage.SelLength > 0 then
    mmoMessage.SelText := WideReverseString(mmoMessage.SelText);
end;

procedure TfmePost.mnuPasteSelectedClick(Sender: TObject);
var
  ws: string;
begin
  fmMain.MessageScrollBox1.GetSelectedText(ws);
  if ws <> '' then
    PasteQuote(ws);
end;

procedure TfmePost.PasteQuote(const quote: string);
var
  s1: TStrings;
  wrap: Boolean;
  NewText: string;
begin
  s1 := TStringList.Create;
  try
    s1.Text := quote;
    wrap := (fPostingSettings.TextPartStyle <> tpQuotedPrintable) and (fPostingSettings.TextPartStyle <> tpFlowed) and  (fPostingSettings.MaxPostLineLength <> 0);
    FixQuotes(s1, wrap, fPostingSettings.MaxPostLineLength, fPostingSettings.QuoteLineMarker, False, XNOptions.StrictSigSep);
    NewText := s1.Text;
    mmoMessage.SelText := NewText;
    if (fPostingSettings.PostingStyle = psBottom) then
      mmoMessage.SelStart := mmoMessage.SelStart + Length(NewText) - 2;
  finally
    s1.Free;
  end;
end;

procedure TfmePost.cbIdentityChange(Sender: TObject);
var
  st: string;
  idx: Integer;
begin
  if not fCloseOK then
    if MessageBox(handle, 'Changing identity will loose any changes you have made to this message.  Are you sure', 'XanaNews', MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION) = IDNo then
    begin
      idx := cbIdentity.Items.IndexOf(fInitialIdentity);
      cbIdentity.ItemIndex := idx;
      mmoMessage.SetFocus;
      Exit;
    end;
  st := cbIdentity.Text;
  SendMessage(Parent.Handle, WM_SETIDENTITY, WPARAM(PChar(st)), 0);
  if not fIsExistingMessage then
    ApplySignature(NNTPAccounts.Identities.Find(st), fSignatureOverride);
  fCloseOK := True;
  fInitialIdentity := st;
  mmoMessage.SetFocus;
end;

procedure TfmePost.AfterConstruction;
begin
  inherited AfterConstruction;
  CWSpellChecker1.SettingsClass := gExSettingsClass;
  CWSpellChecker1.SettingsFilename := gExSettingsFile;
end;

procedure TfmePost.ApplySignature(Identity: TIdentity; const signatureOverride: string);
var
  wst, sig: string;
  p: Integer;
begin
  if Identity = nil then Exit;
  wst := fInitialText;
  p := fInitialP;
  sig := Identity.ChooseSignature(signatureOverride);
  sig := StringReplace(sig, '%author%', Identity.UserName, [rfReplaceAll, rfIgnoreCase]);
// QEC-20041217-18:00  Added system Date-Time capability to signature.
  sig := StringReplace(sig, '%DateTime%', DateTimeToStr(Now), [rfReplaceAll, rfIgnoreCase]);
  sig := StringReplace(sig, '%ver%', ProductVersion, [rfReplaceAll, rfIgnoreCase]);

  if Trim(sig) <> '' then
  begin
    if (wst <> '') and (Copy(wst, Length(wst) - 1, 2) <> #13#10) then
    begin
      wst := wst + #13#10;
      Inc(p);
    end;

    if fPostingSettings.PostingStyle = psBottom then
    begin
      if wst <> '' then
      begin
        wst := wst + #13#10;
        Inc(p, 2);
      end;

      sig := StringReplace(sig, '%author%', Identity.UserName, [rfReplaceAll, rfIgnoreCase]);
        mmoMessage.Text := wst + #13#10#13#10'-- '#13#10 + sig;
    end
    else
      mmoMessage.Text := #13#10'-- '#13#10 + sig + wst;
  end
  else
    if wst <> '' then
    begin
      if fPostingSettings.PostingStyle = psTop then
        mmoMessage.Text := wst
      else
        mmoMessage.Text := wst + #13#10;
      Inc(p, 2);
    end;
  if (p > 0) and (fPostingSettings.PostingStyle = psBottom) then
    mmoMessage.SelStart := p - 1
  else
    mmoMessage.SelStart := 0;
end;

end.
