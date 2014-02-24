unit unitNewsReaderOptions;

interface

uses
  Windows, Classes, SysUtils, Forms, Graphics, StrUtils, Contnrs,
  cmpMessageScrollBox, NewsGlobals, unitExSettings;

type
// nb - these enums must match the 'Items' in the Appearances list view in design mode.
  TAppearanceEnum = (
    apMessageHeaders,
      apMessagesToMe,
      apMessagesFromMe,
      apXanaNewsMessages,
      apDormantMessages,
      apReplies,
      apIgnoredMessages,
      apChildlessMessages,
      apInterestingMessages,
    apMessagePane,
      apHeadersInMessagePane,
      apSignaturesInMessagePane,
      apLevel1Quotes,
      apLevel2Quotes,
      apLevel3Quotes,
    apMessageEditor,
    apSubscribedGroups,
    apMainForm,
      apToolBar,
      apMessageDetailsPanel,
    apMenu
  );

  TAppearanceSettings = class
  private
    fFontName: string;
    fFontColor: TColor;
    fFontStyle: TFontStyles;
    fFontSize: Integer;
    fBackgroundColor: TColor;
    fAlternateBGColor: TColor;
    procedure Load(reg: TExSettings);
    procedure Save(reg: TExSettings);
  public
    property FontName: string read fFontName write fFontName;
    property FontColor: TColor read fFontColor write fFontColor;
    property FontStyle: TFontStyles read fFontStyle write fFontStyle;
    property FontSize: Integer read fFontSize write fFontSize;
    property BackgroundColor: TColor read fBackgroundColor write fBackgroundColor;
    property AlternateBGColor: TColor read fAlternateBGColor write fAlternateBGColor;

    function ApplyFontAndGetColor(font: TFont; FixedFont: string = ''): TColor;
    procedure Init(font: TFont; bkColor: TColor);

    procedure Assign(source: TAppearanceSettings);
    function Equals(Obj: TObject): Boolean; override;
  end;

  TIntArray = array of Integer;
  TViewMode = (vmNormal, vmRawText, vmRaw, vmImages);
  TNewsreaderOptions = class
  private
    fAppKey: string;
    fReg: TExSettings;

    fAppearances: array[TAppearanceEnum] of TAppearanceSettings;
    fAutoDisconnectOnIdle: Boolean;
    fAutoDisconnectOnExit: Boolean;
    fPanelLeft: Integer;
    fArticlesColumnPCs: TIntArray;
    fArticlesColumnPositions: TIntArray;
    fArticlesHeight: Integer;
    fBookmarkColumnPCs: TIntArray;
    fBookmarkColumnPositions: TIntArray;
    fBookmarkHeight: Integer;
    fShowBookmark: Boolean;
    fShowHeader: TShowHeader;
    fAutoExpandThread: Boolean;
    fHideFolderIcons: Boolean;
    fAutoExpandAll: Boolean;
    fAutoMarkAsRead: Boolean;
    fAutoMarkSeconds: Integer;
    fAutoCentralizeMessage: Boolean;
    fQueuedRequestsHeight: Integer;
    fViewMode: TViewMode;
    fTreeColumn: Integer;
    fUnreadFontStyle: TFontStyles;
    fFirstLineAsSubject: Boolean;
    fUnreadNewsgroupsFontStyle: TFontStyles;
    fTextWindowSizeK: Integer;
    fShowDetailsBar: Boolean;
    fCheckSpelling: Boolean;
    fShowInSystemTray: Boolean;
    fEnterGetMessages: Boolean;
    fEnterGoToNextGroup: Boolean;
    fQuoteSelectedText: Boolean;
    fGroupsWithMessagesFontStyle: TFontStyles;
    fAutoDownloadOnClick: Boolean;
    fKeywordColors: array[0..7] of TColorRef;
    fKeyPhrase: array[0..7] of string;
    fKeyPattern: array[0..7] of string;
    fHideColumnFlags: DWORD;
    fWrapLines: Integer;
    fEnterLoadsMessage: Boolean;
    fNoXFaces: Boolean;
    fLoaded: Boolean;
    fShowCustomHeaders: TStringList;
    fHideReadMessages: Boolean;
    fHideIgnoredMessages: Boolean;
    fNoHTML: Boolean;
    fShowMessageCount: Boolean;
    fAutoExpandGroupTree: Boolean;
    fAutoContractGroupTree: Boolean;
    fMagicUser: Boolean;
    fTrimGroupNames: Integer;
    fPanelLeftHeight: Integer;
    fPanelLeftSplitter: Integer;
    fPanelLeftLeft: Integer;
    fPanelLeftTop: Integer;
    fMainToolbarLeft: Integer;
    fMenuToolbarLeft: Integer;
    fMainToolbarTop: Integer;
    fMenuToolbarTop: Integer;
    fAutoCrosspostDetect: Boolean;
    fCheckCrossposts: Integer;
    fDontHighlightXanaNewsUsers: Boolean;
    fHighlightSelectedText: Boolean;
    fAutoRemoveFromBin: Integer;
    fDeservesMedal: Boolean;
    fShowTooltips: Boolean;
    fShowInterestingMarkers: Integer;
    fSearchInternetURLStub: string;
    fTextInternetURLStub: string;
    fStrictSigSep: Boolean;
    fPlainTextPasswords: Boolean;
    fDefaultBozoAction: TBozoAction;
    fAutofitImages: Boolean;
    fUseVistaExplorerTheme: Boolean;
    fISpellDirectory: string;

    function OpenRegistry(const subKey: string; readOnly: Boolean): Boolean;
    procedure CloseRegistry;
    function GetAppearance(idx: TAppearanceEnum): TAppearanceSettings;
    function GetKeywordColors(idx: Integer): TColor;
    procedure SetKeywordColors(idx: Integer; const Value: TColor);
    function GetKeyPhrase(idx: Integer): string;
    procedure SetKeyPhrase(idx: Integer; const Value: string);
    function GetKeyPattern(idx: Integer): string;
    function GetHideColumn(idx: Integer): Boolean;
    procedure SetHideColumn(idx: Integer; const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property AppKey: string read fAppKey write fAppKey;
    procedure Save;
    procedure Load;
    procedure Reload;

    procedure LoadKeyboardShortcuts;
    procedure SaveKeyboardShortcuts;

    property Appearance[idx: TAppearanceEnum]: TAppearanceSettings read GetAppearance;

    property AutoDisconnectOnIdle: Boolean read fAutoDisconnectOnIdle write fAutoDisconnectOnIdle;
    property AutoDisconnectOnExit: Boolean read fAutoDisconnectOnExit write fAutoDisconnectOnExit;
    property AutoExpandThread: Boolean read fAutoExpandThread write fAutoExpandThread;
    property AutoExpandAll: Boolean read fAutoExpandAll write fAutoExpandAll;
    property AutoExpandGroupTree: Boolean read fAutoExpandGroupTree write fAutOExpandGroupTree;
    property AutoContractGroupTree: Boolean read fAutoContractGroupTree write fAutoContractGroupTree;
    property StrictSigSep: Boolean read fStrictSigSep write fStrictSigSep;
    property AutofitImages: Boolean read fAutofitImages write fAutofitImages;
    property UseVistaExplorerTheme: Boolean read fUseVistaExplorerTheme write fUseVistaExplorerTheme;
    property HideFolderIcons: Boolean read fHideFolderIcons write fHideFolderIcons;
    property AutoCentralizeMessage: Boolean read fAutoCentralizeMessage write fAutoCentralizeMessage;
    property AutoMarkAsRead: Boolean read fAutoMarkAsRead write fAutoMarkAsRead;
    property AutoMarkSeconds: integer read fAutoMarkSeconds write fAutoMarkSeconds;
    property AutoDownloadOnClick: Boolean read fAutoDownloadOnClick write fAutoDownloadOnClick;
    property FirstLineAsSubject: Boolean read fFirstLineAsSubject write fFirstLineAsSubject;
    property DontHighlightXanaNewsUsers: Boolean read fDontHighlightXanaNewsUsers write fDontHighlightXanaNewsUsers;
    property HighlightSelectedText: Boolean read fHighlightSelectedText write fHighlightSelectedText;

    property PanelLeft: Integer read fPanelLeft write fPanelLeft; // Width

    property PanelLeftLeft: Integer read fPanelLeftLeft write fPanelLeftLeft;
    property PanelLeftTop: Integer read fPanelLeftTop write fPanelLeftTop;
    property PanelLeftHeight: Integer read fPanelLeftHeight write fPanelLeftHeight;

    property MainToolbarLeft: Integer read fMainToolbarLeft write fMainToolbarLeft;
    property MenuToolbarLeft: Integer read fMenuToolbarLeft write fMenuToolbarLeft;
    property MainToolbarTop: Integer read fMainToolbarTop write fMainToolbarTop;
    property MenuToolbarTop: Integer read fMenuToolbarTop write fMenuToolbarTop;

    property PanelLeftSplitter: Integer read fPanelLeftSplitter write fPanelLeftSplitter;

    property ArticlesColumnPCs: TIntArray read fArticlesColumnPCs;
    property ArticlesColumnPositions: TIntArray read fArticlesColumnPositions;
    property ArticlesHeight: Integer read fArticlesHeight write fArticlesHeight;
    property BookmarkColumnPCs: TIntArray read fBookmarkColumnPCs;
    property BookmarkColumnPositions: TIntArray read fBookmarkColumnPositions;
    property BookmarkHeight: Integer read fBookmarkHeight write fBookmarkHeight;
    property ShowBookmark: Boolean read fShowBookmark write fShowBookmark;
    property ShowHeader: TShowHeader read fShowHeader write fShowHeader;
    property ViewMode: TViewMode read fViewMode write fViewMode;
    property TextWindowSizeK: Integer read fTextWindowSizeK write fTextWindowSizeK;
    property QueuedRequestsHeight: Integer read fQueuedRequestsHeight write fQueuedRequestsHeight;

    property TreeColumn: Integer read fTreeColumn write fTreeColumn;
    property UnreadFontStyle: TFontStyles read fUnreadFontStyle write fUnreadFontStyle;
    property UnreadNewsgroupsFontStyle: TFontStyles read fUnreadNewsgroupsFontStyle write fUnreadNewsgroupsFontStyle;
    property GroupsWithMessagesFontStyle: TFontStyles read fGroupsWithMessagesFontStyle write fGroupsWithMessagesFontStyle;

    property AutoCrosspostDetect: Boolean read fAutoCrosspostDetect write fAutoCrosspostDetect;
    property ShowTooltips: Boolean read fShowTooltips write fShowTooltips;
    property CheckCrossposts: Integer read fCheckCrossposts write fCheckCrossposts;
    property AutoRemoveFromBin: Integer read fAutoRemoveFromBin write fAutOremoveFromBin;
    property ShowDetailsBar: Boolean read fShowDetailsBar write fShowDetailsBar;
    property ShowInSystemTray: Boolean read fShowInSystemTray write fShowInSystemTray;
    property CheckSpelling: Boolean read fCheckSpelling write fCheckSpelling;
    property SearchInternetURLStub: string read fSearchInternetURLStub write fSearchInternetURLStub;
    property TextInternetURLStub: string read fTextInternetURLStub write fTextInternetURLStub;
    property ISpellDirectory: string read fISpellDirectory write fISpellDirectory;

    property EnterGoToNextGroup: Boolean read fEnterGoToNextGroup write fEnterGoToNextGroup;
    property EnterGetMessages: Boolean read fEnterGetMessages write fEnterGetMessages;
    property ShowMessageCount: Boolean read fShowMessageCount write fShowMessageCount;
    property EnterLoadsMessage: Boolean read fEnterLoadsMessage write fEnterLoadsMessage;
    property ShowInterestingMarkers: Integer read fShowInterestingMarkers write fShowInterestingMarkers;

    property QuoteSelectedText: Boolean read fQuoteSelectedText write fQuoteSelectedText;
    property KeywordColors[idx: Integer]: TColor read GetKeywordColors write SetKeywordColors;
    property KeyPhrase[idx: Integer]: string read GetKeyPhrase write SetKeyPhrase;
    property KeyPattern[idx: Integer]: string read GetKeyPattern;
    property HideColumn[idx: Integer]: Boolean read GetHideColumn write SetHideColumn;

    property WrapLines: Integer read fWrapLines write fWrapLines;
    property NoXFaces: Boolean read fNoXFaces write fNoXFaces;
    property NoHTML: Boolean read fNoHTML write fNoHTML;

    property HideReadMessages: Boolean read fHideReadMessages write fHidereadMessages;
    property HideIgnoredMessages: Boolean read fHideIgnoredMessages write fHideIgnoredMessages;

    property ShowCustomHeaders: TStringList read fShowCustomHeaders;

    property MagicUser: Boolean read fMagicUser;
    property TrimGroupNames: Integer read fTrimGroupNames write fTrimGroupNames;
    property DeservesMedal: Boolean read fDeservesMedal write fDeservesMedal;
    property PlainTextPasswords: Boolean read fPlainTextPasswords write fPlainTextPasswords;
    property DefaultBozoAction: TBozoAction read fDefaultBozoAction write fDefaultBozoAction;
  end;

var
  XNOptions: TNewsreaderOptions = nil;
  gDefaultWindowSizeK: Integer;


implementation

uses
  {$if CompilerVersion >= 24.0} // 24.0 = Delphi XE3
    System.UITypes,
  {$ifend}
  unitSearchString, unitNNTPServices;

const
  cISPELL_DIRECTORY = 'ISpell Directory';

  AppearanceKeyNames: array[TAppearanceEnum] of string = (
    'Articles Tree',
    'Messages To Me',
    'My Messages',
    'XanaNews Messages',
    'Dormant Messages',
    'Replies to my Messages',
    'Ignored Messages',
    'Messages Without Replies',
    'Interesting Messages',
    'Article',
    'Message Pane Headers',
    'Message Pane Signatures',
    'Message Pane Level1 Quotes',
    'Message Pane Level2 Quotes',
    'Message Pane Level3 Quotes',
    'Message Editor',
    'Newsgroup Tree',
    'Main Form',
    'Toolbar',
    'Message Details Panel',
    'Menu');

  AppearancesFontColorDefaults: array[TAppearanceEnum] of TColor = (
    clBlack,            // Articles Tree
    clBlue,             // Messages to Me
    clLime,             // My Messages
    clGreen,            // Xananews Messages
    clGray,             // Dormant Messages
    clFuchsia,          // Replies
    clYellow,           // Ignored
    clBlack,            // Childless
    clBlack,            // Interesting
    clBlack,            // Articles
    clGray,             // Message pane headers
    clGray,
    clTeal,
    clOlive,
    clPurple,
    clBlack,
    clBlack,
    clBlack,
    clBlack,
    clBlack,
    clBlack);

  ShowCustomHeadersDefault = 'From:1,Subject:1,Date:0,Message-Id:0,Lines:0,Path:0,Newsgroups:0,Organization:0,NNTP-Posting-Host:0,User-Agent:1,XRef:0,MIME-Version:0,Content-Type:0,X-Trace:0,X-Complaints-To:0';

{ TNewsreaderOptions }

function CountChar(const st: string; ch: char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(st) do
    if st[i] = ch then
      Inc(Result);
end;


procedure TNewsreaderOptions.CloseRegistry;
begin
  FreeAndNil(fReg);
end;

constructor TNewsreaderOptions.Create;
var
  i: TAppearanceEnum;
  n: Integer;
begin
  fShowCustomHeaders := TStringList.Create;
  fShowCustomHeaders.CaseSensitive := False;
  fShowCustomHeaders.NameValueSeparator := ':';
  fShowCustomHeaders.CommaText := ShowCustomHeadersDefault;
  fAutoDisconnectOnIdle := False;
  fAutoDisconnectOnExit := True;
  fAutoCentralizeMessage := True;
  fAutofitImages := True;
  fUseVistaExplorerTheme := (Win32MajorVersion >= 6);
  fHideFolderIcons := True;
  fShowTooltips := True;
  fPlainTextPasswords := False;
  fDefaultBozoAction := baIgnore;
  fShowDetailsBar := True;
  fCheckSpelling := True;
  fAutoExpandThread := True;
  fQuoteSelectedText := True;
  fEnterLoadsMessage := True;
  fTextWindowSizeK := gDefaultWindowSizeK;
  fCheckCrossposts := 3;
  fAutoRemoveFromBin := 30;
// TODO: Change the default or not?
//   The following would be the new-link:
//     fSearchInternetURLStub := 'http://groups.google.com/groups?threadm=%qid%';
//   ... although the old way also still works:
//     fSearchInternetURLStub := 'http://groups-beta.google.com/groups?threadm=%qid%';
//   ... or maybe just use the actual search function instead (?):
//     fSearchInternetURLStub := 'http://groups.google.com/groups/search?as_umsgid=%qid%';
  fSearchInternetURLStub := 'http://groups.google.com/groups?threadm=%qid%';
  fTextInternetURLStub := 'http://www.google.com/search?hl=en&q="%qtext%"';

  for i := Low(TAppearanceEnum) to High(TAppearanceEnum) do
    fAppearances[i] := TAppearanceSettings.Create;

  fShowHeader := shShort;
  SetLength(fArticlesColumnPCs, 5);
  SetLength(fArticlesColumnPositions, 6);
  SetLength(fBookmarkColumnPCs, 5);
  SetLength(fBookmarkColumnPositions, 6);
  fTreeColumn := 2;
  fFirstLineAsSubject := True;
  fUnreadFontStyle := [fsBold];
  fUnreadnewsgroupsFontStyle := [fsBold];
  fGroupsWithMessagesFontStyle := [fsBold];
  fEnterGetMessages := True;
  fShowMessageCount := True;
  fEnterGoToNextGroup := True;
  fAutoMarkAsRead := True;

  fKeywordColors[0] := clRed;
  fKeywordColors[1] := clLime;
  fKeywordColors[2] := clYellow;
  fKeywordColors[3] := clBlue;
  fKeywordColors[4] := clFuchsia;
  fKeywordColors[5] := clAqua;
  fKeywordColors[6] := clOlive;
  fKeywordColors[7] := clMaroon;

  for n := 0 to 7 do
    fKeyPattern[n] := '~';

  fAppKey := 'Software\Woozle\XanaNews';
end;

destructor TNewsreaderOptions.Destroy;
var
  i: TAppearanceEnum;
begin
  for i := Low(TAppearanceEnum) to High(TAppearanceEnum) do
    fAppearances[i].Free;

  fShowCustomHeaders.Free;
  CloseRegistry;

  inherited Destroy;
end;

procedure TNewsreaderOptions.Load;
var
  w: Integer;
  st, s: string;
  i: TAppearanceEnum;
begin
  if fLoaded then Exit;
  fArticlesColumnPCs[0] := 5;    // Flags
  fArticlesColumnPCs[1] := 10;   // Number
  fArticlesColumnPCs[2] := 45;   // Subject
  fArticlesColumnPCs[3] := 20;   // Author
  fArticlesColumnPCs[4] := 10;   // Date
                                 // remainder = lines

  fArticlesColumnPositions[0] := 0;
  fArticlesColumnPositions[1] := 1;
  fArticlesColumnPositions[2] := 2;
  fArticlesColumnPositions[3] := 3;
  fArticlesColumnPositions[4] := 4;
  fArticlesColumnPositions[5] := 5;

  fBookmarkColumnPCs[0] := 25;   // Account/Group
  fBookmarkColumnPCs[1] := 23;   // Subject
  fBookmarkColumnPCs[2] := 25;   // From
  fBookmarkColumnPCs[3] := 10;   // Date
  fBookmarkColumnPCs[4] := 7;    // Lines
                                 // remainder = bookmarked Date
  fBookmarkColumnPositions[0] := 0;
  fBookmarkColumnPositions[1] := 1;
  fBookmarkColumnPositions[2] := 2;
  fBookmarkColumnPositions[3] := 3;
  fBookmarkColumnPositions[4] := 4;
  fBookmarkColumnPositions[5] := 5;

  try
    if OpenRegistry('Directories', True) then
    begin
      fISpellDirectory := fReg.GetStringValue(cISPELL_DIRECTORY, fISpellDirectory);
    end;

    if OpenRegistry('General', True) then
    begin
      fShowInSystemTray   := fReg.GetBooleanValue('Show In System Tray', fShowInSystemTray);
      fSearchInternetURLStub := fReg.GetStringValue('Search Internet URL Stub', fSearchInternetURLStub);
      fTextInternetURLStub := fReg.GetStringValue('Text Internet URL Stub', fTextInternetURLStub);
      fAutoCrosspostDetect:= fReg.GetBooleanValue('Auto Crosspost Detect', fAutoCrosspostDetect);
      fShowTooltips       := fReg.GetBooleanValue('Show Tooltips', fShowTooltips);
      fPlainTextPasswords := fReg.GetBooleanValue('Plain Text Passwords', fPlainTextPasswords);
      fDefaultBozoAction := TBozoAction(fReg.GetIntegerValue('Default Bozo Action', Integer(fDefaultBozoAction)));
      fCheckCrossposts    := fReg.GetIntegerValue('Check Crossposts', fCheckCrossposts);
      fAutoRemoveFromBin  := fReg.GetIntegerValue('Auto Remove From Bin', fAutoRemoveFromBin);
      fQuoteSelectedText  := fReg.GetBooleanValue('Quote Selected Text', fQuoteSelectedText);
      fEnterGetMessages   := fReg.GetBooleanValue('Enter Gets Messages', fEnterGetMessages);
      fShowMessageCount   := fReg.GetBooleanValue('Show Message Count', fShowMessageCount);
      fEnterGoToNextGroup := fReg.GetBooleanValue('Enter Goto Next Group', fEnterGoToNextGroup);
      fEnterLoadsMessage  := fReg.GetBooleanValue('Enter Loads Message', fEnterLoadsMessage);
      fAutoExpandGroupTree:= fReg.GetBooleanValue('Auto Expand Group Tree', fAutoExpandGroupTree);
      fAutoContractGroupTree:= fReg.GetBooleanValue('Auto Contract Group Tree', fAutoContractGroupTree);
      fStrictSigSep := fReg.GetBooleanValue('Strict Signature Separator', fStrictSigSep);
      fAutofitImages := fReg.GetBooleanValue('Autofit Images', fAutofitImages);
      fUseVistaExplorerTheme:= fReg.GetBooleanValue('Use Vista Explorer Theme', fUseVistaExplorerTheme);
      fGroupsWithMessagesFontStyle := TFontStyles(Byte(fReg.GetIntegerValue('Groups With Messages Font Style', Byte(fGroupsWithMessagesFontStyle))));
      fMagicUser:= fReg.GetBooleanValue('Magic User', False);
      fDeservesMedal := fReg.GetBooleanValue('Deserves Medal', False);
      fTrimGroupNames := fReg.GetIntegerValue('Trim Group Names', fTrimGroupNames);
      fShowInterestingMarkers := fReg.GetIntegerValue('Show Interesting Markers', fShowInterestingMarkers);

      for w := 0 to 7 do
      begin
        fKeywordColors[w] := fReg.GetIntegerValue('Keyword Color ' + IntToStr(w), fKeywordColors[w]);
        fKeyphrase[w] := fReg.GetStringValue('Keyphrase ' + IntToStr(w), '');
      end;
    end;

    if OpenRegistry('Position', True) then
    begin
      fPanelLeft            := fReg.GetIntegerValue('Panel Left', fPanelLeft);
      fPanelLeftSplitter    := fReg.GetIntegerValue('Panel Left Splitter', fPanelLeftSplitter);
      fPanelLeftHeight      := fReg.GetIntegerValue('Panel Left Height', fPanelLeftHeight);
      fPanelLeftTop         := fReg.GetIntegerValue('Panel Left Top', fPanelLeftTop);
      fPanelLeftLeft        := fReg.GetIntegerValue('Panel Left Left', fPanelLeftLeft);
      fMenuToolbarLeft      := fReg.GetIntegerValue('Menu Bar Left', fMenuToolbarLeft);
      fMenuToolbarTop       := fReg.GetIntegerValue('Menu Bar Top', fMenuToolbarTop);
      fMainToolbarLeft      := fReg.GetIntegerValue('Main Bar Left', fMainToolbarLeft);
      fMainToolbarTop       := fReg.GetIntegerValue('Main Bar Top', fMainToolbarTop);
      fArticlesHeight       := fReg.GetIntegerValue('Articles Height', fArticlesHeight);
      fBookmarkHeight       := fReg.GetIntegerValue('Bookmark Height', fBookmarkHeight);
      fShowBookmark         := fReg.GetBooleanValue('Show Bookmark', fShowBookmark);
      fShowHeader           := TShowHeader(fReg.GetIntegerValue('Show Header', Integer(fShowHeader)));
      fQueuedRequestsHeight := fReg.GetIntegerValue('Queued Requests Height', fQueuedRequestsHeight);

      if fArticlesHeight = 0 then
        fArticlesHeight := 1;

      if fPanelLeft = 0 then
        fPanelLeft := 1;

      if fBookmarkHeight = 0 then
        fBookmarkHeight := 1;

      if fQueuedRequestsHeight = 0 then
        fQueuedRequestsHeight := 1;

      st := fReg.GetStringValue('Articles Column PCs', '');
      w := 0;
      repeat
        s := SplitString(',', st);
        if s <> '' then
        begin
          fArticlesColumnPCs[w] := StrToInt(s);
          Inc(w);
        end;
      until s = '';

      st := fReg.GetStringValue('Articles Column Positions', '');
      w := 0;
      repeat
        s := SplitString(',', st);
        if s <> '' then
        begin
          fArticlesColumnPositions[w] := StrToInt(s);
          Inc(w);
        end;
      until s = '';

      st := fReg.GetStringValue('Bookmark Column PCs', '');
      w := 0;
      repeat
        s := SplitString(',', st);
        if s <> '' then
        begin
          fBookmarkColumnPCs[w] := StrToInt(s);
          Inc(w);
        end;
      until s = '';

      st := fReg.GetStringValue('Bookmark Column Positions', '');
      w := 0;
      repeat
        s := SplitString(',', st);
        if s <> '' then
        begin
          fBookmarkColumnPositions[w] := StrToInt(s);
          Inc(w);
        end;
      until s = '';
    end;

    for i := Low(TAppearanceEnum) to High(TAppearanceEnum) do
    case i of
      apMessageHeaders,
      apMessagePane,
      apMessageEditor,
      apSubscribedGroups,
      apMainForm,
      apMenu:
        if OpenRegistry('Appearance\' + AppearanceKeyNames[i], True) then
          fAppearances[i].Load(fReg);

      Succ(apMessageHeaders)..
      Pred(apMessagePane):
        if OpenRegistry('Appearance\' + AppearanceKeyNames[i], True) then
          fAppearances[i].Load(fReg)
        else
        begin
          fAppearances[i].Assign(fAppearances[apMessageHeaders]);
          fAppearances[i].fFontColor := AppearancesFontColorDefaults[i];
        end;

      Succ(apMessagePane)..
      Pred(apMessageEditor):
        if OpenRegistry('Appearance\' + AppearanceKeyNames[i], True) then
          fAppearances[i].Load(fReg)
        else
        begin
          fAppearances[i].Assign(fAppearances[apMessagePane]);
          fAppearances[i].fFontColor := AppearancesFontColorDefaults[i];
        end;

      Succ(apMainForm)..
      Pred(apMenu):
        if OpenRegistry('Appearance\' + AppearanceKeyNames[i], True) then
          fAppearances[i].Load(fReg)
        else
        begin
          fAppearances[i].Assign(fAppearances[apMainForm]);
          fAppearances[i].fFontColor := AppearancesFontColorDefaults[i];
        end;
    end;

    if OpenRegistry('Connection', True) then
    begin
      fAutoDisconnectOnIdle  := fReg.GetBooleanValue('Auto Disconnect On Idle', fAutoDisconnectOnIdle);
      fAutoDisconnectOnExit  := fReg.GetBooleanValue('Auto Disconnect On Exit', fAutoDisconnectOnExit);
    end;

    if OpenRegistry('Message Tree', True) then
    begin
      fAutoExpandThread          := fReg.GetBooleanValue('Auto Expand Thread', fAutoExpandThread);
      fAutoExpandAll             := fReg.GetBooleanValue('Auto Expand All',    fAutoExpandAll);
      fHideFolderIcons           := fReg.GetBooleanValue('Hide Folder Icons',  fHideFolderIcons);
      fAutoCentralizeMessage     := fReg.GetBooleanValue('Auto Centralize Message', fAutoCentralizeMessage);
      fAutoMarkAsRead            := fReg.GetBooleanValue('Auto Mark As Read', fAutoMarkAsRead);
      fAutoMarkSeconds           := fReg.GetIntegerValue('Auto Mark Seconds', 0);
      fAutoDownloadOnClick       := fReg.GetBooleanValue('Auto Download On Click', fAutoDownloadOnClick);
      fFirstLineAsSubject        := fReg.GetBooleanValue('First Line As Subject', fFirstLineAsSubject);
      fDontHighlightXanaNewsUsers:= fReg.GetBooleanValue('Dont Highlight Xananews Users', fDontHighlightXanaNewsUsers);
      fHighlightSelectedText     := fReg.GetBooleanValue('Highlight Selected Text', fHighlightSelectedText);
      fTreeColumn                := fReg.GetIntegerValue('Tree Column', fTreeColumn);
      fUnreadFontStyle           := TFontStyles(Byte(fReg.GetIntegerValue('Unread Font Style', Byte(fUnreadFontStyle))));
      fUnreadNewsgroupsFontStyle := TFontStyles(Byte(fReg.GetIntegerValue('Unread Newsgroups Font Style', Byte(fUnreadNewsgroupsFontStyle))));
      fHideColumnFlags           := fReg.GetIntegerValue('Hide Column Flags', 0);
      fHideReadMessages          := fReg.GetBooleanValue('Hide Read Messages', fHideReadMessages);
      fHideIgnoredMessages       := fReg.GetBooleanValue('Hide Ignored Messages', fHideIgnoredMessages);
    end;

    if OpenRegistry('Message Pane', True) then
    begin
      fTextWindowSizeK := fReg.GetIntegerValue('Text Window Size K', fTextWindowSizeK);
      fShowDetailsBar := fReg.GetBooleanValue('Show Details Bar', fShowDetailsBar);
      fCheckSpelling := fReg.GetBooleanValue('Check Spelling', fCheckSpelling);
      fWrapLines := fReg.GetIntegerValue('Wrap Lines', fWrapLines);
      fNoXFaces := fReg.GetBooleanValue('No XFaces', fNoXFaces);
      fNoHTML := fReg.GetBooleanValue('No HTML', fNoHTML);
      fShowCustomHeaders.CommaText := fReg.GetStringValue('Show Custom Headers', ShowCustomHeadersDefault);
    end;

    fLoaded := True;
  finally
    CloseRegistry;
  end;
end;

function TNewsreaderOptions.OpenRegistry(const subKey: string; readOnly: Boolean): Boolean;
begin
  if fReg = nil then
    fReg := CreateExSettings;

  if readOnly then
  begin
    fReg.Section := '';
    Result := fReg.HasSection(subKey);
  end
  else
    Result := True;

  fReg.Section := subKey;
end;

procedure TNewsreaderOptions.Reload;
begin
  fLoaded := False;
  Load;
end;

procedure TNewsreaderOptions.Save;
var
  i: Integer;
  e: TAppearanceEnum;
  st: string;
  fs: TFontStyles;
  defColor: TColor;
begin
  try
    OpenRegistry('Position', False);
    fReg.SetIntegerValue('Panel Left', fPanelLeft, 0);
    fReg.SetIntegerValue('Panel Left Splitter', fPanelLeftSplitter, 0);
    fReg.SetIntegerValue('Panel Left Height', fPanelLeftHeight, 0);
    fReg.SetIntegerValue('Panel Left Left', fPanelLeftLeft, 0);
    fReg.SetIntegerValue('Panel Left Top', fPanelLeftTop, 0);
    fReg.SetIntegerValue('Menu Bar Left', fMenuToolbarLeft, 9);
    fReg.SetIntegerValue('Menu Bar Top', fMenuToolbarTop, 0);
    fReg.SetIntegerValue('Main Bar Left', fMainToolbarLeft, 9);
    fReg.SetIntegerValue('Main Bar Top', fMainToolbarTop, 23);
    fReg.SetIntegerValue('Articles Height', fArticlesHeight, 0);
    fReg.SetIntegerValue('Bookmark Height', fBookmarkHeight, 0);
    fReg.SetBooleanValue('Show Bookmark', fShowBookmark, False);
    fReg.SetIntegerValue('Queued Requests Height', fQueuedRequestsHeight, 0);
    fReg.SetIntegerValue('Show Header', Integer(fShowHeader), Integer(shShort));

    st := '';
    for i := 0 to Length(fArticlesColumnPCs) - 1 do
    begin
      st := st + IntToStr(fArticlesColumnPCs[i]);
      if i < Length(fArticlesColumnPCs) - 1 then
        st := st + ',';
    end;
    fReg.SetStringValue('Articles Column PCs', st, '');

    st := '';
    for i := 0 to Length(fArticlesColumnPositions) - 1 do
    begin
      st := st + IntToStr(fArticlesColumnPositions[i]);
      if i < Length(fArticlesColumnPositions) - 1 then
        st := st + ',';
    end;
    fReg.SetStringValue('Articles Column Positions', st, '');

    st := '';
    for i := 0 to Length(fBookmarkColumnPCs) - 1 do
    begin
      st := st + IntToStr(fBookmarkColumnPCs[i]);
      if i < Length(fBookmarkColumnPCs) - 1 then
        st := st + ',';
    end;
    fReg.SetStringValue('Bookmark Column PCs', st, '');

    st := '';
    for i := 0 to Length(fBookmarkColumnPositions) - 1 do
    begin
      st := st + IntToStr(fBookmarkColumnPositions[i]);
      if i < Length(fBookmarkColumnPositions) - 1 then
        st := st + ',';
    end;
    fReg.SetStringValue('Bookmark Column Positions', st, '');

    for e := Low(TAppearanceEnum) to High(TAppearanceEnum) do
    begin
      OpenRegistry('Appearance\' + AppearanceKeyNames[e], False);
      fAppearances[e].Save(fReg);
    end;

    OpenRegistry('Connection', False);
    fReg.SetBooleanValue('Auto Disconnect On Idle', fAutoDisconnectOnIdle, False);
    fReg.SetBooleanValue('Auto Disconnect On Exit', fAutoDisconnectOnExit, True);

    OpenRegistry('Message Tree', False);
    fReg.SetBooleanValue('Auto Expand Thread', fAutoExpandThread, True);
    fReg.SetBooleanValue('Auto Expand All', fAutoExpandAll, False);
    fReg.SetBooleanValue('Hide Folder Icons', fHideFolderIcons, True);
    fReg.SetBooleanValue('Auto Centralize Message', fAutoCentralizeMessage, True);
    fReg.SetBooleanValue('Auto Mark As Read', fAutoMarkAsRead, True);
    fReg.SetIntegerValue('Auto Mark Seconds', fAutoMarkSeconds, 0);
    fReg.SetBooleanValue('Auto Download On Click', fAutoDownloadOnClick, False);
    fReg.SetBooleanValue('First Line As Subject', fFirstLineAsSubject, True);
    fReg.SetBooleanValue('Dont Highlight XanaNews Users', fDontHighlightXanaNewsUsers, False);
    fReg.SetBooleanValue('Highlight Selected Text', fHighlightSelectedText, False);
    fReg.SetIntegerValue('Tree Column', fTreeColumn, 2);
    fReg.SetIntegerValue('Hide Column Flags', fHideColumnFlags, 0);
    fs := [fsBold];
    fReg.SetIntegerValue('Unread Font Style', Byte(fUnreadFontStyle), Byte(fs));
    fReg.SetIntegerValue('Unread Newsgroups Font Style', Byte(fUnreadNewsgroupsFontStyle), Byte(fs));
    fReg.SetBooleanValue('Hide Read Messages', fHideReadMessages, False);
    fReg.SetBooleanValue('Hide Ignored Messages', fHideIgnoredMessages, False);

    OpenRegistry('Message Pane', False);
    fReg.SetIntegerValue('Text Window Size K', fTextWindowSizeK, gDefaultWindowSizeK);
    fReg.SetBooleanValue('Check Spelling', fCheckSpelling, True);
    fReg.SetBooleanValue('Show Details Bar', fShowDetailsBar, True);
    fReg.SetIntegerValue('Wrap Lines', fWrapLines, 0);
    fReg.SetBooleanValue('No XFaces', fNoXfaces, False);
    fReg.SetBooleanValue('No HTML', fNoHTML, False);
    fReg.SetStringValue('Show Custom Headers', fShowCustomHeaders.CommaText, ShowCustomHeadersDefault);

    OpenRegistry('Directories', False);
    fReg.SetStringValue(cISPELL_DIRECTORY, fISpellDirectory, '');

    OpenRegistry('General', False);
    fReg.SetBooleanValue('Show In System Tray', fShowInSystemTray, False);
    fReg.SetStringValue('Search Internet URL Stub', fSearchInternetURLStub, '');
    fReg.SetStringValue('Text Internet URL Stub', fTextInternetURLStub, '');
    fReg.SetBooleanValue('Auto Crosspost Detect', fAutoCrosspostDetect, False);
    fReg.SetBooleanValue('Show Tooltips', fShowTooltips, True);
    fReg.SetBooleanValue('Plain Text Passwords', fPlainTextPasswords, False);
    fReg.SetIntegerValue('Default Bozo Action', Integer(fDefaultBozoAction), Integer(baIgnore));
    fReg.SetIntegerValue('Check Crossposts', fCheckCrossposts, 3);
    fReg.SetIntegerValue('Auto Remove From Bin', fAutoRemoveFromBin, 30);
    fReg.SetBooleanValue('Quote Selected Text', fQuoteSelectedText, True);
    fReg.SetBooleanValue('Enter Gets Messages', fEnterGetMessages, True);
    fReg.SetBooleanValue('Show Message Count', fShowMessageCount, True);
    fReg.SetBooleanValue('Enter Goto Next Group', fEnterGoToNextGroup, True);
    fReg.SetBooleanValue('Enter Loads Message', fEnterLoadsMessage, True);
    fReg.SetIntegerValue('Groups With Messages Font Style', Byte(fGroupsWithMessagesFontStyle), Byte(fs));
    fReg.SetBooleanValue('Auto Expand Group Tree', fAutoExpandGroupTree, False);
    fReg.SetBooleanValue('Auto Contract Group Tree', fAutoContractGroupTree, False);
    fReg.SetBooleanValue('Strict Signature Separator', fStrictSigSep, False);
    fReg.SetIntegerValue('Trim Group Names', fTrimGroupNames, 0);
    fReg.SetIntegerValue('Show Interesting Markers', fShowInterestingMarkers, 0);
    fReg.SetBooleanValue('Deserves Medal', fDeservesMedal, False);
    fReg.SetBooleanValue('Autofit Images', fAutofitImages, True);
    fReg.SetBooleanValue('Use Vista Explorer Theme', fUseVistaExplorerTheme, True);

    for i := 0 to 7 do
    begin
      fReg.SetStringValue('Keyphrase ' + IntToStr(i), fKeyphrase[i], '');
      defColor := clBlack;
      case i of
        0: defColor := clRed;
        1: defColor := clLime;
        2: defColor := clYellow;
        3: defColor := clBlue;
        4: defColor := clFuchsia;
        5: defColor := clAqua;
        6: defColor := clOlive;
        7: defColor := clMaroon;
      end;

      fReg.SetIntegerValue('Keyword Color ' + IntToStr(i), fKeywordColors[i], defColor);
    end
  finally
    CloseRegistry;
  end;
end;

function TNewsreaderOptions.GetAppearance(idx: TAppearanceEnum): TAppearanceSettings;
begin
  Result := fAppearances[idx];
end;

function TNewsreaderOptions.GetKeywordColors(idx: Integer): TColor;
begin
  Result := fKeywordColors[idx];
end;

procedure TNewsreaderOptions.SetKeywordColors(idx: Integer;
  const Value: TColor);
begin
  fKeywordColors[idx] := Value;
end;

function TNewsreaderOptions.GetKeyPhrase(idx: Integer): string;
begin
  Result := fKeyPhrase[idx];
end;

procedure TNewsreaderOptions.SetKeyPhrase(idx: Integer; const Value: string);
var
  n: Integer;
begin
  if fKeyPhrase[idx] <> Value then
  begin
    fKeyPattern[idx] := '~';
    if fKeyPhrase[idx] = '' then
      n := -1
    else
      n := idx;
    fKeyPhrase[idx] := Value;

    NNTPAccounts.ResetKeyPhraseFlags(n);
  end;
end;

function TNewsreaderOptions.GetKeyPattern(idx: Integer): string;
begin
  if fKeyPattern[idx] = '~' then
    fKeyPattern[idx] := fKeyPhrase[idx];

  Result := fKeyPattern[idx];
end;

function TNewsreaderOptions.GetHideColumn(idx: Integer): Boolean;
begin
  Result := (fHideColumnFlags and (1 shl idx)) <> 0;
end;

procedure TNewsreaderOptions.SetHideColumn(idx: Integer; const Value: Boolean);
var
  mask: DWORD;
begin
  mask := 1 shl idx;

  if value then
    fHideColumnFlags := fHideColumnFlags or mask
  else
    fHideColumnFlags := fHideColumnFlags and not mask;
end;

procedure TNewsreaderOptions.LoadKeyboardShortcuts;
var
  i: Integer;
  def: TActionDefault;
begin
  if OpenRegistry('Keyboard Shortcuts', True) then
  try
    for i := 0 to gDefaultActions.Count - 1 do
    begin
      def := TActionDefault(gDefaultActions[i]);

      def.Action.ShortCut := fReg.GetIntegerValue(def.Action.Name, def.Shortcut);
    end;
  finally
    CloseRegistry;
  end;
end;

procedure TNewsreaderOptions.SaveKeyboardShortcuts;
var
  i: Integer;
  def: TActionDefault;
begin
  if OpenRegistry('Keyboard Shortcuts', False) then
  try
    for i := 0 to gDefaultActions.Count - 1 do
    begin
      def := TActionDefault(gDefaultActions[i]);

      fReg.SetIntegerValue(def.Action.Name, def.Action.ShortCut, def.Shortcut);
    end;
  finally
    CloseRegistry;
  end;
end;

{ TAppearanceSettings }

function TAppearanceSettings.ApplyFontAndGetColor(font: TFont; FixedFont: string = ''): TColor;
begin
  Result := BackgroundColor;
  if FixedFont <> '' then
    font.Name := FixedFont
  else
    font.Name := FontName;
  font.Color := FontColor;
  font.Style := FontStyle;
  font.Size := FontSize;
end;

procedure TAppearanceSettings.Assign(source: TAppearanceSettings);
begin
  fFontName := source.fFontName;
  fFontColor := source.fFontColor;
  fFontStyle := source.fFontStyle;
  fBackgroundColor := source.fBackgroundColor;
  fAlternateBGColor := source.fAlternateBGColor;
  fFontSize := source.fFontSize;
end;

function TAppearanceSettings.Equals(Obj: TObject): Boolean;
var
  source: TAppearanceSettings;
begin
  if Obj is TAppearanceSettings then
  begin
    source := TAppearanceSettings(Obj);
    Result := (fFontColor = source.fFontColor) and
              (fFontSize = source.fFontSize) and
              (fFontStyle = source.fFontStyle) and
              (fBackgroundColor = source.fBackgroundColor) and
              (fAlternateBGColor = source.fAlternateBGColor) and
              (fFontName = source.fFontName);
  end
  else
    Result := inherited Equals(Obj);
end;

function CalcAlternateColor(Background: TColor): TColor;
begin
  Result := ColorToRGB(Background);
  if Win32MajorVersion >= 6 then // Vista or above
    Result := Windows.RGB(GetRValue(Result) * 97 div 100, GetGValue(Result) * 98 div 100, GetBValue(Result * 99 div 100))
  else
    Result := Windows.RGB(GetRValue(Result) * 95 div 100, GetGValue(Result) * 95 div 100, GetBValue(Result * 95 div 100));
end;

procedure TAppearanceSettings.Init(font: TFont; bkColor: TColor);
begin
  fFontName := font.Name;
  fFontColor := font.Color;
  fFontStyle := font.Style;
  fFontSize := font.Size;
  fBackgroundColor := bkColor;
  fAlternateBGColor := CalcAlternateColor(fBackgroundColor);
end;

procedure TAppearanceSettings.Load(reg: TExSettings);
begin
  fFontName  := reg.GetStringValue('Font Name', fFontName);
  fFontColor := reg.GetIntegerValue('Font Color', fFontColor);
  fFontStyle := TFontStyles(Byte(reg.GetIntegerValue('Font Style', Byte(fFontStyle))));
  fFontSize  := reg.GetIntegerValue('Font Size', fFontSize);
  fBackgroundColor := reg.GetIntegerValue('Background Color', fBackgroundColor);
  fAlternateBGColor := reg.GetIntegerValue('Alternate Background Color', CalcAlternateColor(fBackgroundColor));
end;

procedure TAppearanceSettings.Save(reg: TExSettings);
begin
  reg.SetStringValue('Font Name', FontName);
  reg.SetIntegerValue('Font Color', FontColor);
  reg.SetIntegerValue('Font Style', Byte(FontStyle));
  reg.SetIntegerValue('Font Size', FontSize);
  reg.SetIntegerValue('Background Color', BackgroundColor);
  reg.SetIntegerValue('Alternate Background Color', AlternateBGColor, CalcAlternateColor(BackgroundColor));
end;

initialization
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    gDefaultWindowSizeK := 1024 * 1024
  else
    gDefaultWindowSizeK := 8;
  XNOptions := TNewsReaderOptions.Create;
finalization
  FreeAndNil(XNOptions);
end.
