{*======================================================================*
 | PostMessageForm                                                      |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright (c) Colin Wilson 2005  All Rights Reserved                 |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      20/06/2005  CPWW  Original                                  |
 *======================================================================*}
unit PostMessageForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, unitNNTPServices, unitNNTPThreads, unitNewsThread,
  ConTnrs, Menus, cmpCWRichEdit,
  cmpRuler, unitSettings, unitIdentities, PostFrame, NewsGlobals,
  cmpPersistentPosition, unitExSettings;

type
  TfmPostMessage = class(TForm)
    Label2: TLabel;
    cbGroup: TComboBox;
    cbFollowUpTo: TComboBox;
    Label3: TLabel;
    Label1: TLabel;
    edSubject: TExRichEdit;
    PersistentPosition1: TPersistentPosition;
    btnCrossPost: TButton;
    btnFollowUp: TButton;
    fmePost1: TfmePost;
    procedure PersistentPosition1GetSettingsFile(Owner: TObject;
      var fileName: string);
    procedure PersistentPosition1GetSettingsClass(Owner: TObject;
      var SettingsClass: TExSettingsClass);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mmoMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure btnCrossPostClick(Sender: TObject);
    procedure TfmePost1btnOKClick(Sender: TObject);
    procedure btnFollowUpClick(Sender: TObject);
    procedure fmePost1btnAdvancedClick(Sender: TObject);
  private
    fHeader : TStringList;
    fPosterRequest : TPosterRequest;
    fAttachments : TObjectList;

    fOrigMessageID : string;
    fOrigReferences : string;
    fIsReply : boolean;
    fPostingSettings : TPostingSettings;
    fNNTPSettings : TNNTPSettings;
    fIdentity : TIdentity;
    fDefaultPostingSettings: TPostingSettings;
    fReplyToArticle : TArticleBase;
    fAccount: TNNTPAccount;
    fInitialText: WideString;
    fGroupName: string;
    fCodePageOverride : Integer;
    fSubject: string;

    procedure MakeHeaderStringList;
    procedure ValidateHeaderStringList;
    { Private declarations }

    procedure LoadPreviousGroups;
    procedure SaveFollowUp;
    procedure WMSetup (var msg : TMessage); message WM_SETUP;
    procedure WMPostAndClose (var msg : TMessage); message WM_POSTANDCLOSE;
    procedure WMSetCodePage (var msg : TMessage); message WM_SETCODEPAGE;
    procedure WMSetIdentity (var msg : TMessage); message WM_SETIDENTITY;
    function CheckCrossPosts(const groups: string): string;

  protected
    procedure UpdateActions; override;
    procedure CreateParams (var params : TCreateParams); override;
  public
    property NNTPSettings : TNNTPSettings read fNNTPSettings write fNNTPSettings;
    property DefaultPostingSettings : TPostingSettings read fDefaultPostingSettings write fDefaultPostingSettings;
    property ReplyToArticle : TArticleBase read fReplyToArticle write fReplyToArticle;
    property Account : TNNTPAccount read fAccount write fAccount;
    property GroupName : string read fGroupName write fGroupName;
    property InitialText : WideString read fInitialText write fInitialText;
    property PosterRequest : TPosterRequest read fPosterRequest write fPosterRequest;
    property Subject: string read fSubject write fSubject;
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  end;

var
  fmPostMessage: TfmPostMessage;

implementation

uses unitNewsreaderOptions, unitNNTPThreadManager, AdvancedHeadersDialog,
  AttachmentsDialog, unitCharsetMap, clipbrd, unitSearchString, PostToGroupsForm,
  IdGlobal, CheckCrosspostDialog;

{$R *.dfm}

const
  EM_AUTOURLDETECT = WM_USER + 91;

procedure TfmPostMessage.FormShow(Sender: TObject);
var
  sub, st : string;
  i, codePageOverride : Integer;
  groupName : string;
  group : TSubscribedGroup;
begin
  AdjustFormConstraints (self);
  codePageOverride := -1;
  if Assigned (fPosterRequest) then
  begin
    fHeader := TStringList.Create;
    fHeader.CaseSensitive := False;
    fHeader.Assign(fPosterRequest.Hdr);
    fInitialText := StringToWideString (fPosterRequest.Msg, fPosterRequest.Codepage);

    codePageOverride := fPosterRequest.Codepage;

    groupName := fHeader.Values ['Newsgroups'];
    edSubject.Text := StringToWideString (fHeader.Values ['Subject'], fPosterRequest.Codepage);
    cbGroup.Text := groupName;
    cbFollowupTo.Text := fHeader.Values ['Followup-To'];

    groupName := SplitString (',', groupName);

    group := Nil;
      for i := 0 to Account.SubscribedGroupCount - 1 do
        if CompareText (groupName, Account.SubscribedGroups [i].Name) = 0 then
        begin
          group := Account.SubscribedGroups [i];
          break
        end;

    if Assigned (group) then
    begin
      groupName := group.Name;
      DefaultPostingSettings := group.PostingSettings;
      NNTPSettings := group.NNTPSettings
    end
    else
    begin
      DefaultPostingSettings := Account.PostingSettings;
      NNTPSettings := Account.NNTPSettings
    end
  end;

  fPostingSettings := TPostingSettings.Create(nil);
  fPostingSettings.Assign(DefaultPostingSettings);
  fIdentity := NNTPSettings.Identity;

  fmePost1.Initialize(fInitialText, fPostingSettings, fIdentity, ReplyToArticle, fPosterRequest, fAttachments, codePageOverride, NNTPSettings.SignatureOverride);

  fIsReply := Assigned (ReplyToArticle);
  LoadPreviousGroups;

  if fIsReply then
  begin
    groupName := ReplyToArticle.Owner.Name;
    sub := DecodeSubject(ReplyToArticle.subject, ReplyToArticle.CodePage);
    Caption := 'Reply to article from ' +
               StringToGDIString(ReplyToArticle.FromName, ReplyToArticle.CodePage) + ' - ' +
               StringToGDIString(sub, ReplyToArticle.CodePage);
    sub := Trim (sub);

    fOrigReferences := ReplyToArticle.References;
    fOrigMessageID := ReplyToArticle.MessageId;

    while UpperCase (Copy (sub, 1, 3)) = 'RE:' do
      sub := Trim (Copy (sub, 4, MaxInt));

    edSubject.Text := StringToWideString ('Re: ' + sub, ReplyToArticle.CodePage);

    if Assigned (ReplyToArticle.Msg) then
    begin
      for i := 0 to ReplyToArticle.Msg.Header.Count - 1 do
      begin
        st := ReplyToArticle.Msg.Header [i];
        if CompareText (Copy (st, 1, 12), 'Followup-To:') = 0 then
        begin
          groupName := Trim (Copy (st, 13, MaxInt));
          if SameText (groupName, 'poster') then
            groupName := ReplyToArticle.Owner.Name;

          break
        end;
        if CompareText (Copy (st, 1, 11), 'Newsgroups:') = 0 then
          groupName := Trim (Copy (st, 12, MaxInt));

      end
    end;

    cbFollowupTo.Text := CheckCrossPosts (groupName);
    cbGroup.Text := groupName;
    PostMessage (Handle, WM_SETUP, 1, 0);
  end
  else
  begin
    if fGroupName <> '' then
    begin
      cbGroup.Text := fGroupName;
      if fSubject <> '' then
        edSubject.Text := fSubject;
      PostMessage (Handle, WM_SETUP, 0, 0);
    end
    else
      PostMessage (Handle, WM_SETUP, 2, 0);
  end
end;

procedure TfmPostMessage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action := caFree;

  if not fmePost1.CloseOK then
    if (fmePost1.mmoMessage.Text <> '') or (fmePost1.AttachmentCount > 0) then
      if MessageBox (Handle, 'Are you sure you want to cancel this message', PChar (Application.Title), MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION) <> IDYES then
        action := caNone 
end;

destructor TfmPostMessage.Destroy;
begin
  fmPostMessage := nil;
  fHeader.Free;
  fAttachments.Free;
  fPostingSettings.Free;
  inherited;
end;

constructor TfmPostMessage.Create(AOwner: TComponent);
begin
  inherited;
  fAttachments := TObjectList.Create;
  fmPostMessage := Self;
  fCodePageOverride := -1;
end;

function TrimReferences (st : string; n : Integer) : string;
var
  refCount : Integer;
  s, r, firstRef: string;
begin
  s := '';
  refCount := 0;
  repeat
    r := SplitString (' ', st);
    if r <> '' then
    begin
      if (Copy (r, 1, 1) = '<') and (Copy (r, Length (r), 1) = '>') then
      begin
        Inc (refCount);
        if s = '' then
          s := r
        else
          s := s + ' ' + r
      end
    end
  until r = '';

  if refCount > n then
  begin
    firstRef := ExtractString (' ', s);
    while (refCount > n) or (Length (firstRef + ' ' + s) > 998) do
    begin
      ExtractString (' ', s);
      Dec (refCount);
      if s = '' then break;
    end;

    if s = '' then
      s := firstRef
    else
      s := firstRef + ' ' + s;
  end;

  result := s
end;

procedure TfmPostMessage.MakeHeaderStringList;
var
  i, idx : Integer;
  s : TStringList;
  st, n : string;


  procedure AddHeader (const hdrName, value : string);
  begin
    if fHeader.IndexOfName(hdrName) = -1 then
      fHeader.Add(hdrName + '=' + value);
  end;

begin
  if not Assigned (fHeader) then
  begin
    fHeader := TStringList.Create;
    fHeader.CaseSensitive := False;
    fHeader.Add('From="' + fIdentity.UserName + '" <' + fIdentity.EMailAddress + '>');
    fHeader.Add('Subject=' + WideStringToString (edSubject.Text, fmePost1.CodePage));
    fHeader.Add('Newsgroups=' + cbGroup.Text);
    if (fIdentity.ReplyAddress <> '') and (fIdentity.ReplyAddress <> fIdentity.EMailAddress) then
      fHeader.Add('Reply-To="' + fIdentity.UserName + '" <' + fIdentity.ReplyAddress + '>');
    if cbFollowUpTo.Text <> '' then
      fHeader.Add ('Followup-To=' + cbFollowupTo.Text);

    if fIsReply then
      fHeader.Add('References=' + TrimReferences (fOrigReferences, 19) + ' ' + fOrigMessageID);

    if fIdentity.Organization <> '' then
      fHeader.Add ('Organization=' + fIdentity.Organization);

    if NNTPSettings.GenerateDateHeaders then
      fHeader.Add('Date=' + DateTimeToInternetStr (Now));

    if NNTPSettings.GenerateMessageIDs then
    begin
      if (NNTPSettings.MessageIDDomain = '') or (NNTPSettings.MessageIDDomain = '<Auto>') then
        st := LowerCase (Account.NNTPServerSettings.ServerName)
      else
        st := NNTPSettings.MessageIDDomain;
      fHeader.Add('Message-ID='+GenerateMessageID ('xn', NNTPSettings.MessageIDStub, st))
    end;

    AddHeader ('User-Agent', ThreadManager.NewsAgent);

    if NNTPSettings.AdvancedHeaders <> '' then
    begin
      s := TStringList.Create;
      try
        s.Text := NNTPSettings.AdvancedHeaders;
        for i := 0 to s.Count - 1 do
        begin
          st := s [i];
          if st <> '' then
          begin
            n := SplitString (':', st);
            if n <> '' then
              AddHeader (n, st)
          end
        end
      finally
        s.Free
      end
    end;

    if NNTPSettings.NoArchive then
      AddHeader ('X-No-Archive', 'yes');

    if fIdentity.XFace <> '' then
      AddHeader ('X-Face', fIdentity.XFace);
  end
  else
  begin
    fHeader.Values ['Subject'] := WideStringToString (edSubject.Text, fmePost1.CodePage);
    fHeader.Values ['Newsgroups'] := cbGroup.Text;
    if cbFollowupTo.Text = '' then
    begin
     idx := fHeader.IndexOfName('Followup-To');
     if idx >= 0 then
       fHeader.Delete (idx)
    end
    else
      fHeader.Values ['Followup-To'] := cbFollowupTo.Text
  end
end;

procedure TfmPostMessage.ValidateHeaderStringList;
begin
  MakeHeaderStringList;

  if fIsReply then
    if fHeader.Values ['References'] = '' then
      fHeader.Add('References=' + fOrigReferences + ' ' + fOrigMessageID);

  if fHeader.Values ['Newsgroups'] = '' then
    fHeader.Insert (0, 'Newsgroups=' + cbGroup.Text);

  if fHeader.Values ['Subject'] = '' then
    fHeader.Insert(0, 'Subject=' + WideStringToString (edSubject.Text, fmePost1.CodePage));

  if fHeader.Values ['From'] = '' then
    fHeader.Insert(0, 'From="' + fIdentity.UserName + '" <' + fIdentity.EMailAddress + '>');

  if (fIdentity.ReplyAddress <> '') and (fIdentity.EMailAddress <> fIdentity.ReplyAddress) then
    if fHeader.Values ['Reply-To'] = '' then
      fHeader.Insert(0, 'Reply-To="' + fIdentity.UserName + '" <' + fIdentity.ReplyAddress + '>');
end;

procedure TfmPostMessage.UpdateActions;
var
  s, st : string;
  followupCount : Integer;
  followupPoster : boolean;
  allowed : boolean;
begin
  s := cbFollowupTo.Text;

  followupCount := 0;
  followupPoster := False;
  repeat
    st := SplitString (',', s);
    if st <> '' then
    begin
      Inc (followupCount);
      if SameText (st, 'poster') then
        followupPoster := True;
    end
  until st = '';

  allowed := (followupPoster = False) or (followupCount = 1);

  fmePost1.UpdateActions(allowed and (cbGroup.Text <> '') and (edSubject.Text <> ''));
end;

procedure TfmPostMessage.LoadPreviousGroups;
var
  reg : TExSettings;
  s : string;
  i : Integer;
begin
  s := '';
  reg := CreateExSettings;
  try
    reg.Section := 'Accounts\' + Account.AccountName;
    if reg.Open (true) then
    begin
      if reg.HasValue ('Last Follow Up') then
        s := reg.StringValue ['Last Follow Up']
    end;

    if GroupName <> '' then
      cbGroup.Items.Add(GroupName);

    if s <> '' then
      cbFollowupTo.Items.Add (s);

    if not SameText (s, 'poster') then
      cbFollowupTo.Items.Add('poster');

    for i := 0 to Account.SubscribedGroupCount - 1 do
    begin
      if Account.SubscribedGroups [i].Name <> GroupName then
        cbGroup.Items.Add(Account.SubscribedGroups [i].Name);

      if Account.SubscribedGroups [i].Name <> s then
        cbFollowupTo.Items.Add(Account.SubscribedGroups [i].Name);
    end
  finally
    reg.Free;
  end
end;

procedure TfmPostMessage.SaveFollowUp;
var
  reg : TExSettings;
begin
  if cbFollowupTo.Text <> '' then
  begin
    reg := CreateExSettings;
    try
      reg.Section := 'Accounts\' + Account.AccountName;
      reg.StringValue ['Last Follow Up'] := cbFollowupTo.Text;
    finally
      reg.Free
    end
  end
end;

procedure TfmPostMessage.mmoMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;

  inherited
end;

procedure TfmPostMessage.PersistentPosition1GetSettingsClass(Owner: TObject;
  var SettingsClass: TExSettingsClass);
begin
  SettingsClass := gExSettingsClass;
end;

procedure TfmPostMessage.PersistentPosition1GetSettingsFile(Owner: TObject;
  var fileName: string);
begin
 fileName := gExSettingsFile
end;

procedure TfmPostMessage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if ssCtrl in Shift then
      fmePost1.btnOK.Click;
end;

procedure TfmPostMessage.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  params.ExStyle   := params.ExStyle or WS_EX_APPWINDOW;
  params.WndParent := Application.Handle;
end;

procedure TfmPostMessage.FormResize(Sender: TObject);
begin
  fmePost1.DoResize
end;

procedure TfmPostMessage.WMPostAndClose(var msg: TMessage);
var
  codePage : Integer;
  st : string;
begin
  ValidateHeaderStringList;
  st := PChar (msg.WParam);
  codePage := msg.LParam;

  if Assigned (fPosterRequest) then
  begin
    fPosterRequest.Hdr.Assign(fHeader);
    fPosterRequest.Msg := st;
    fPosterRequest.Reset;
    fPosterRequest.Owner.Paused := False;
    fPosterRequest.Attachments.Free;
    fPosterRequest.Attachments := fAttachments;
    fAttachments := Nil
  end
  else
  begin
    ThreadManager.PostMessage (Account, fHeader, st, fAttachments, codepage, fPostingSettings.TextPartStyle);
    fAttachments := Nil
  end;
  SaveFollowUp;
  Close
end;

procedure TfmPostMessage.WMSetCodePage(var msg: TMessage);
var
  charset : TFontCharset;
begin
  charset := CodePageToCharset (msg.WParam);
  self.Font.Charset := charset;
end;

procedure TfmPostMessage.btnCrossPostClick(Sender: TObject);
var
  dlg : TfmPostToGroups;
begin
  dlg := TfmPostToGroups.Create (nil);
  try
    dlg.Account := Account;
    dlg.Groups := cbGroup.Text;
    if dlg.ShowModal = mrOK then
      cbGroup.Text := dlg.Groups
  finally
    dlg.Free
  end
end;

procedure TfmPostMessage.TfmePost1btnOKClick(Sender: TObject);
var
  firstGroup : string;
  okToPost : boolean;
  dlg : TdlgCheckCrosspost;
begin
  okToPost := True;
  if cbFollowupTo.Text = '' then
  begin
    firstGroup := CheckCrossPosts (cbGroup.Text);
    if firstGroup <> '' then
    begin
      dlg := TdlgCheckCrosspost.Create(nil);
      try
        if dlg.ShowModal <> idOK then
          okToPost := false;

        if okToPost then
          if dlg.rbFollowup.Checked then
          begin
            cbFollowupTo.Text := firstGroup;
            MakeHeaderStringList
          end
          else
            if dlg.rbFirstGroupOnly.Checked then
            begin
              cbGroup.Text := firstGroup;
              MakeHeaderStringList
            end;
      finally
        dlg.Free
      end
    end
  end;

  if okToPost then
    fmePost1.btnOKClick(Sender);
end;

procedure TfmPostMessage.btnFollowUpClick(Sender: TObject);
var
  dlg : TfmPostToGroups;
begin
  dlg := TfmPostToGroups.Create (nil);
  try
    dlg.Account := Account;
    dlg.Groups := cbFollowUpTo.Text;
    dlg.IsFollowUp := True;
    if dlg.ShowModal = mrOK then
      cbFollowUpTo.Text := dlg.Groups
  finally
    dlg.Free
  end
end;

function TfmPostMessage.CheckCrossPosts(const groups: string): string;
var
  first, st : string;

  crossPostCount : Integer;

begin
  st := groups;

  first := SplitString (',', st);
  crossPostCount := 0;

  if first <> '' then
  repeat
    Inc (crossPostCount)
  until SplitString (',', st) = '';

  if (Options.CheckCrossposts > 0) and (crossPostCount > Options.CheckCrossposts) then
    result := first
  else
    result := ''
end;

procedure TfmPostMessage.fmePost1btnAdvancedClick(Sender: TObject);
var
  dlg : TdlgAdvancedheaders;
  i : Integer;

begin
  dlg := TdlgAdvancedHeaders.Create(Self);
  try
    dlg.PopupParent := Self;
    dlg.PopupMode := pmExplicit;
    MakeHeaderStringList;
    dlg.mmoAdvancedHeaders.Lines.BeginUpdate;
    try
      dlg.mmoAdvancedHeaders.Lines.Clear;
      for i := 0 to fHeader.Count - 1 do
        dlg.mmoAdvancedHeaders.Lines.Add (StringReplace (fHeader [i], '=', ':', []));

      if dlg.ShowModal = mrOK then
      begin
        fHeader.Clear;
        for i := 0 to dlg.mmoAdvancedHeaders.Lines.Count - 1 do
          fHeader.Add(StringReplace (dlg.mmoAdvancedHeaders.Lines [i], ':', '=', []));

        ValidateHeaderStringList;

        edSubject.Text := StringToWideString (fHeader.Values ['Subject'], fmePost1.CodePage);
        cbGroup.Text := fHeader.Values ['Newsgroups'];
        cbFollowupTo.Text := fHeader.Values ['Followup-To'];
      end;

    finally
      dlg.mmoAdvancedHeaders.Lines.EndUpdate
    end
  finally
    dlg.Free
  end
end;

procedure TfmPostMessage.WMSetIdentity(var msg: TMessage);
var
  idName : string;
  id : TIdentity;

  procedure ReplaceHeader (const name, value : string);
  var
    idx : Integer;
  begin
    if Assigned (fHeader) then
    begin
      idx := fHeader.IndexOfName(name);
      if idx <> -1 then
        if value = '' then
          fHeader.Delete(idx)
        else
          fHeader.ValueFromIndex [idx] := value
      else
        if value <> '' then
          fHeader.Add(name + '=' + value);
    end
  end;

begin
  idName := PChar (msg.WParam);
  id := NNTPAccounts.Identities.Find(idName);

  if id <> Nil then
  begin
    fIdentity := id;

    ReplaceHeader ('From', '');
    ReplaceHeader ('Reply-To', '');
    ReplaceHeader ('Organization', id.Organization);
    ReplaceHeader ('X-Face', id.XFace);
    ValidateHeaderStringList
  end
end;


{*----------------------------------------------------------------------*
 | procedure WMSetup                                                    |
 |                                                                      |
 | Message handler for WM_SETUP - which is posted during FormShow.      |
 |                                                                      |
 | This works around some bugs.                                         |
 |                                                                      |
 |  1.  If the focus is moved from cbGroup during SetForm, then the     |
 |      text in cbGroup is still highlightedm which is confusing.       |
 |      Setting it here instead cures this.                             |
 |                                                                      |
 |  2.  For some reasong the EM_SCROLLCARET that the VCL sends when     |
 |      setting the memo's selection isn't obeyed - so repeat it here.  |
 *----------------------------------------------------------------------*}
procedure TfmPostMessage.WMSetup(var msg: TMessage);
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
