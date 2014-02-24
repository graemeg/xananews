(*======================================================================*
 | unitNNTPServices unit for NewsReader3                                |
 |                                                                      |
 | NNTP Services for News Reader 3                                      |
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
 | Copyright © Colin Wilson 2002-2004  All Rights Reserved              |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      24/07/2001  CPWW    Original                                |
 |                                                                      |
 |          11/10/2001  CPWW    Continued development                   |
 |          17/01/2002  CPWW    Improved threading                      |
 |          23/01/2003  CPWW    Big restructure for e-mail support      |
 |          10/02/2004  Leonel  Cross-post mark as read                 |
 *======================================================================*)

unit unitNNTPServices;

interface

uses
  Windows, Classes, SysUtils, Forms, Dialogs, Controls, Contnrs, StrUtils, idGlobal,
  unitMessages, unitMessageMime, unitNNTPFilters, NewsGlobals, unitObjectCache,
  unitIdentities, unitSearchString, unitSettings, unitBatches, unitExSettings,
  unitStreamTextReader, Shfolder, ShellAPI, SyncObjs, XnClasses, XnRawByteStrings;

//-----------------------------------------------------------------------
// Constant definitions
const
  fgRead                  = $1;           // TArticle flags values.
  fgDeleted               = $2;
  fgMine                  = $4;
  fgXanaNews              = $8;
  fgInteresting           = $10;
  fgScannedKeyphrases     = $20;
  fgScannedAttachment     = $40;
  fgCancelled             = $80;
  fgReply                 = $100;
  fgIgnore                = $200;
  fgNew                   = $400;
  fgNotOnServer           = $800;

  fgKeyPhrase0            = $01000;
  fgKeyPhrase1            = $02000;
  fgKeyPhrase2            = $04000;
  fgKeyPhrase3            = $08000;
  fgKeyPhrase4            = $10000;
  fgKeyPhrase5            = $20000;
  fgKeyPhrase6            = $40000;
  fgKeyPhrase7            = $80000;

  fgKeyPhraseMask         = $FF000;

  fgTemp                  = $00100000;

  fgBozo                  = $01000000;
  fgScannedBozo           = $02000000;
  fgUnknownCodePage       = $04000000;
  fgHasAttachment         = $08000000;

  fgSpamNoPostingHost     = $10000000;
  fgSpamNoXTrace          = $20000000;
  fgSpamSharesPostingHost = $40000000;
  fgSpamSharesName        = $80000000;

type
  TNNTPAccounts = class;
  TNNTPAccount = class;
  TSubscribedGroup = class;
  TArticleContainer = class;


  TMultipartFlags = (mfNotMultipart, mfPartialMultipart, mfCompleteMultipart);

  //-----------------------------------------------------------------------
  // TArticleBase class.  Base class for TArticle &  TFolderArticle

  TArticleBase = class
  private
    fOwner: TArticleContainer;
    fFromNameDecoded: Boolean;

    fFromName: string;
    fFromEMail: string;

    fChild: TArticleBase;            // Thread child
    fParent: TArticleBase;           // Thread parent
    fSibling: TArticleBase;
    fBlocked: Boolean;

    fIsMultiPart: Boolean;
    fPartCount: Integer;
    fPartDecoded: Boolean;
    fPartNo: Integer;
    fPartSubject: RawByteString;

    function GetFromEmail: string;
    function GetFromName: string;
    function GetHasAttachment: Boolean;
    function GetIsDeleted: Boolean;
    function GetIsDormant: Boolean;
    function GetIsInteresting: Boolean;
    function GetIsMine: Boolean;
    function GetIsOdd: Boolean;
    function GetIsXanaNews: Boolean;
    procedure SetIsInteresting(const Value: Boolean); virtual;
    procedure SetIsMine(const Value: Boolean);
    procedure SetIsRead(const Value: Boolean); virtual;
    function GetIsCancelled: Boolean;
    procedure SetIsCancelled(const Value: Boolean);
    function GetIsFromBozo: Boolean;
    function GetIsReply: Boolean;
    procedure SetIsReply(const Value: Boolean);
    function GetIsIgnore: Boolean;
    procedure SetIsIgnore(const Value: Boolean);
    function GetIsNotOnServer: Boolean;
    procedure SetIsNotOnServer(const Value: Boolean);
    function GetSubjectIsReply: Boolean;
    function GetSimplifiedSubject: RawByteString;
    function GetMessagesInThread: Integer;
    function GetUnreadMessagesInThread: Integer;
    function GetHasNoReplies: Boolean;
    function GetIsFromMe: Boolean;
    function GetMessageID: string;
    function GetSubject: string;
    function GetFrom: string;
    function GetReferences: string;
    procedure SetSubject(const Value: string);
  protected
    fMsg: TmvMessage;
    fMessageID: RawByteString;
    fBytes: Integer;
    fReferences: RawByteString;
    fLines: Integer;
    fFrom: RawByteString;
    fSubject: RawByteString;
    fDate: TDateTime;
    fTo: string;
    fMessageOffset: Int64;        // Offset of message body in messages.dat
    fMultipartFlags: TMultipartFlags;
    fFlags: DWORD;                // See fgXXXX constants above
    fArticleNo: Int64;          // 0 = Dummy article
    fNewestMessage: TDateTime;

    function GetIsRead: Boolean; virtual;
    function GetHeader(const name: string): string; virtual;
    procedure DecodeFromName;
    function DecodedMultipartSubject: RawByteString;
    function GetMsg: TmvMessage; virtual; abstract;
    procedure SetMsg(const Value: TmvMessage); virtual;
    function GetIndex: Integer; virtual;
    function GetInterestingMessageLine: string; virtual;
    function GetPostingHost: string; virtual;
    function GetCodePage: Integer; virtual;
    procedure SetCodePage(const Value: Integer); virtual;
    function PeekAtMsgHdr(const hdr: string): string; virtual;
    function PeekAtMsgHdrFromFile(const hdr: RawByteString): RawByteString;
    procedure SetFlag(flag: DWORD; value: Boolean);
    function GetNext: TArticleBase; virtual;
    function GetMsgFromFile: TmvMessage;
    function GetCodePageFromFile: Integer;
    procedure SetIsDeleted(const Value: Boolean); virtual;
    function GetUniqueID: RawByteString; virtual;
    function GetCrosspostedTo: Integer;
  public
    fCodePage: Integer;
    constructor Create(AOwner: TArticleContainer); virtual;
    procedure Assign(article: TArticleBase); virtual;
    procedure Initialize(articleNo: Int64; header: TAnsiStrings);
    function HasMsg: Boolean;
    function MsgDownloading: Boolean;
    procedure ReleaseMsg;

    property Subject: string read GetSubject write SetSubject;
    property SubjectIsReply: Boolean read GetSubjectIsReply;
    property SimplifiedSubject: RawByteString read GetSimplifiedSubject;
    property From: string read GetFrom;
    property Date: TDateTime read fDate;
    property MessageId: string read GetMessageID;
    property UniqueID: RawByteString read GetUniqueID;
    property References: string read GetReferences;
    property _To: string read fTo;
    property Bytes: Integer read fBytes;
    property Lines: Integer read fLines;
    property ArticleNo: Int64 read fArticleNo;

    property Msg: TmvMessage read GetMsg write SetMsg;
    property InterestingMessageLine: string read GetInterestingMessageLine;
    property PostingHost: string read GetPostingHost;
    property FromName: string read GetFromName;
    property FromEmail: string read GetFromEmail;
    property CodePage: Integer read GetCodePage write SetCodePage;
    property Flags: DWORD read fFlags;
    property MultipartFlags: TMultipartFlags read fMultipartFlags;
    property HasAttachment: Boolean read GetHasAttachment;

    property Owner: TArticleContainer read fOwner;
    property Child: TArticleBase read fChild;
    property Parent: TArticleBase read fParent;
    property Sibling: TArticleBase read fSibling;
    property Next: TArticleBase read GetNext;

    property IsDeleted: Boolean read GetIsDeleted write SetIsDeleted;
    property IsNotOnServer: Boolean read GetIsNotOnServer write SetIsNotOnServer;
    property IsCancelled: Boolean read GetIsCancelled write SetIsCancelled;
    property IsRead: Boolean read GetIsRead write SetIsRead;
    property IsIgnore: Boolean read GetIsIgnore write SetIsIgnore;
    property IsMine: Boolean read GetIsMine write SetIsMine;
    property IsReply: Boolean read GetIsReply write SetIsReply;
    property IsXanaNews: Boolean read GetIsXanaNews;
    property IsOdd: Boolean read GetIsOdd;
    property IsDormant: Boolean read GetIsDormant;
    property IsInteresting: Boolean read GetIsInteresting write SetIsInteresting;
    property IsFromBozo: Boolean read GetIsFromBozo;
    property HasNoReplies: Boolean read GetHasNoReplies;
    property Index: Integer read GetIndex;
    property Header[const name: string]: string read GetHeader;
    property CrosspostedTo: Integer read GetCrosspostedTo;
    property MessagesInThread: Integer read GetMessagesInThread;
    property UnreadMessagesInThread: Integer read GetUnreadMessagesInThread;
    property IsFromMe: Boolean read GetIsFromMe;

    property RawFrom: RawByteString read fFrom;
    property RawMessageID: RawByteString read fMessageId write fMessageId;
    property RawReferences: RawByteString read fReferences write fReferences;
    property RawSubject: RawByteString read fSubject;
  end;

  //-----------------------------------------------------------------------
  // TArticle class

  TArticle = class(TArticleBase)
  private
    fPostingHost: string;

    fInterestingMessageLine: string;
    fTempExtraHeaders: string;

    function GetKeyPhraseNo: Integer;
    function GetHasKeyPhrase(idx: Integer): Boolean;
    function GetAccount: TNNTPAccount;
    function GetSubscribedGroup: TSubscribedGroup;
    procedure SetCrossPostsFlag(flag: DWORD; value: Boolean);
    function GetHasAnyKeyPhrase: Boolean;
    function MatchesKeyPhrase(st: string; searcher: TStringSearcher): Boolean;
  protected
    function GetMsg: TmvMessage; override;
    procedure SetMsg(const Value: TmvMessage); override;
    function GetInterestingMessageLine: string; override;
    function GetPostingHost: string; override;
    function GetCodePage: Integer; override;
    procedure SetCodePage(const Value: Integer); override;
    function PeekAtMsgHdr(const hdr: string): string; override;
    procedure SetIsRead(const Value: Boolean); override;
    function GetHeader(const name: string): string; override;
  public
    constructor Create(AOwner: TArticleContainer); override;
    destructor Destroy; override;
    procedure ChangeArticleNo(newArticleNo: Int64);
    procedure FixArticleNo;

    procedure RemoveMessage;
    procedure SaveMessageBody;
    procedure RawMarkAsRead;

    property SubscribedGroup: TSubscribedGroup read GetSubscribedGroup;
    property Account: TNNTPAccount read GetAccount;

    property KeyPhraseNo: Integer read GetKeyPhraseNo;
    property HasKeyPhrase[idx: Integer]: Boolean read GetHasKeyPhrase;
    property HasAnyKeyPhrase: Boolean read GetHasAnyKeyPhrase;
    property TempExtraHeaders: string read fTempExtraHeaders;
  end;

  //-----------------------------------------------------------------------
  // Used in both NNTPAccounts and subscribed groups, because you
  // are allowed to set filters for both individual subscribed groups, and
  // whole accounts
  TFiltersCtnr = class
  private
    fOwner: TObject;
    fFilters: TNNTPFilters;
    fParent: TFiltersCtnr;
    function GetHasFilters: Boolean;
    function GetCount: Integer;
  public
    constructor Create(AOwner: TObject; AParent: TFiltersCtnr);
    destructor Destroy; override;
    property Filters: TNNTPFilters read fFilters;
    procedure EnableFilter(filter: TNNTPFilter; enable: Boolean);
    function FilterEnabled(filter: TNNTPFilter): Boolean;
    procedure LoadFilters(reg: TExSettings; displayFilters: Boolean);
    procedure SaveFilters(reg: TExSettings; displayFilters: Boolean);
    procedure AssignFilters(ctnr: TFiltersCtnr);
    function BlockArticle(article: TArticleBase): Boolean;
    procedure Clear;
    property Parent: TFiltersCtnr read fParent;
    property HasFilters: Boolean read GetHasFilters;
    property Count: Integer read GetCount;
  end;

  TServerAccount = class
  private
    fName: string;
  protected
    function GetIdentity: TIdentity; virtual;
    function GetPostingSettings: TPostingSettings; virtual;
    function GetServerSettings: TServerSettings; virtual; abstract;
    procedure SetName(const Value: string); virtual;
  public
    constructor Create(const AName: string);
    property Name: string read fName write SetName;
    property Identity: TIdentity read GetIdentity;
    property ServerSettings: TServerSettings read GetServerSettings;
    property PostingSettings: TPostingSettings read GetPostingSettings;
  end;

  //--------------------------------------------------------------------------
  // Base class for things that contain articles - TSubscribedGroup & TArticleFolder

  TArticleContainer = class(TServerAccount)
  private
    fFiltersCtnr: TFiltersCtnr;
    fDisplayFiltersCtnr: TFiltersCtnr;

    fSortBuf: TList;
    fDisplayFiltered: Boolean;
    fHideReadMessages: Boolean;
    fHideIgnoredMessages: Boolean;

    fSecret: Boolean;
    fHideMessagesNotToMe: Boolean;

    function FindChild(article, child: TArticleBase): Boolean;
    procedure PruneDummyArticles(article: TArticleBase);
    procedure GatherSubjects(article: TArticleBase);
    procedure SortSiblings(var root: TArticleBase);
    procedure ThreadArticles;
    function GetFirstArticle: TArticleBase;

    procedure SetThreadOrder(const Value: TThreadOrder);
    procedure SetThreadSortDirection(const Value: TThreadSortDirection);
    procedure SetThreadSortOrder(const Value: TThreadSortOrder);


    function GetUnreadArticleToMeCount: Integer;
    function GetUnreadReplyCount: Integer;
    function GetUnreadXanaNewsArticleCount: Integer;
    function GetInterestingArticleCount: Integer;
    procedure SetHideReadMessages(const Value: Boolean);
    function GetUnreadInterestingArticleCount: Integer;
    procedure SetHideIgnoredMessages(const Value: Boolean);
    procedure SetHideMessagesNotToMe(const Value: Boolean);
    function GetGroupMultipartMessages: Boolean;
    procedure SetGroupMultipartMessages(const Value: Boolean);
  protected
    fThreads: TList;                     // List of TArticleBase thread roots
    fUnreadArticleCount: Integer;
    fUnreadArticleToMeCount: Integer;
    fUnreadReplyCount: Integer;
    fUnloadedArticleCount: Integer;
    fUnreadXananewsArticleCount: Integer;
    fInterestingArticleCount: Integer;
    fUnreadInterestingArticleCount: Integer;
    fCursorArticleId: RawByteString;
    fFlagsDirty: Boolean;
    fMessageFile: TFileStream;
    fAdjustedArticleCount: Integer;

    fThreadOrder: TThreadOrder;           // Threaded or Chronological
    fThreadSortOrder: TThreadSortOrder;   // Sort by author, subject, date, lines
    fThreadSortDirection: TThreadSortDirection;

    fCurrentThreadOrder: TThreadOrder;
    fCurrentThreadSortOrder: TThreadSortOrder;
    fCurrentThreadSortDirection: TThreadSortDirection;
    fLockCount: Integer;

    procedure ClearThreadInfo;
    function GetDisplaySettings: TDisplaySettings; virtual;
    function GetMessagebaseSize: Int64; virtual;

    function GetArticleBase(idx: Integer): TArticleBase; virtual; abstract;
    function GetArticleCount: Integer; virtual; abstract;              // Object list of TArticle
    function GetThreadCount: Integer; virtual;
    function GetThreads(idx: Integer): TArticleBase; virtual;
    procedure GroupArticles; virtual;
    function GetLoaded: Boolean; virtual; abstract;
    function GetUnreadArticleCount: Integer; virtual; abstract;
    function GetAdjustedArticleCount: Integer; virtual;

    procedure RawSortArticles; virtual;
    procedure SortArticles; virtual;
    procedure SortThreads; virtual;
    procedure SetSecret(const Value: Boolean); virtual;

    function GetNext: TArticleContainer; virtual; abstract;
    procedure SaveArticleCounts(reg: TExSettings=nil); virtual;
    procedure LoadArticleCounts(reg: TExSettings); virtual;

  public
    fFocused: Boolean;
    fNeedsPurge: Boolean;
    fSearching: Boolean;
    fSorting: Boolean;

    constructor Create(const AName: string; AFiltersParent, ADisplayFiltersParent: TFiltersCtnr);
    destructor Destroy; override;

    procedure BeginLock;
    procedure EndLock;
    function Locked: Boolean;

    function MarkOnLeave: Boolean; virtual;
    function FindMsgID(const msgID: RawByteString): TArticleBase;
    function FindUniqueID(const msgID: RawByteString): TArticleBase; virtual;
    function FindMyXRef(const xref: string): TArticleBase;

    function FindArticleNo(cno: Int64): TArticleBase; virtual;
    function LoadGetArticleCount: Integer;
    procedure LoadArticles; virtual; abstract;
    procedure CloseMessageFile; virtual;
    procedure OpenMessageFile; virtual;
    property Loaded: Boolean read GetLoaded;
    procedure ReSortArticles;
    procedure ResetSortFlags;
    procedure ResetBozoFlags;
    procedure LeaveGroup(clearMessages: Boolean = True); virtual;

    procedure RawAddArticle(article: TArticleBase); virtual; abstract;
    procedure RawDeleteArticle(cno: Integer); virtual; abstract;
    procedure RawInsertArticle(index: Integer; article: TArticleBase); virtual;
    procedure RawRemoveArticle(article: TArticleBase); virtual; abstract;

    function IndexOf(article: TArticleBase): Integer; virtual;

    property ArticleCount: Integer read GetArticleCount;
    property AdjustedArticleCount: Integer read GetAdjustedArticleCount;
    property ArticleBase[idx: Integer]: TArticleBase read GetArticleBase;
    property CursorArticleID: RawByteString read fCursorArticleId write fCursorArticleId;
    property FlagsDirty: Boolean read fFlagsDirty;
    property FiltersCtnr: TFiltersCtnr read fFiltersCtnr;
    property DisplayFiltersCtnr: TFiltersCtnr read fDisplayFiltersCtnr;
    property FirstArticle: TArticleBase read GetFirstArticle;

    property MessageFile: TFileStream read fMessageFile;
    property DisplaySettings: TDisplaySettings read GetDisplaySettings;
    property GroupMultipartMessages: Boolean read GetGroupMultipartMessages write SetGroupMultipartMessages;
    property ThreadCount: Integer read GetThreadCount;
    property ThreadOrder: TThreadOrder read fThreadOrder write SetThreadOrder;
    property Threads[idx: Integer]: TArticleBase read GetThreads;
    property ThreadSortOrder: TThreadSortOrder read fThreadSortOrder write SetThreadSortOrder;
    property ThreadSortDirection: TThreadSortDirection read fThreadSortDirection write SetThreadSortDirection;
    property UnreadArticleCount: Integer read GetUnreadArticleCount;
    property InterestingArticleCount: Integer read GetInterestingArticleCount;
    property UnreadInterestingArticleCount: Integer read GetUnreadInterestingArticleCount;
    property UnreadArticleToMeCount: Integer read GetUnreadArticleToMeCount;
    property UnreadReplyCount: Integer read GetUnreadReplyCount;
    property UnreadXanaNewsArticleCount: Integer read GetUnreadXanaNewsArticleCount;
    property Next: TArticleContainer read GetNext;
    property HideReadMessages: Boolean read fHideReadMessages write SetHideReadMessages;
    property HideMessagesNotToMe: Boolean read fHideMessagesNotToMe write SetHideMessagesNotToMe;
    property HideIgnoredMessages: Boolean read fHideIgnoredMessages write SetHideIgnoredMessages;
    property MessagebaseSize: Int64 read GetMessagebaseSize;
    property Secret: Boolean read fSecret write SetSecret;
  end;

  //-----------------------------------------------------------------------
  // TArticleObjectContainer - base class for things that store articles as
  // a TObjectList - eg. news messages, but not archived messages
  TArticleObjectContainer = class(TArticleContainer)
  protected
    fArticles: TObjectList;
    fArticlesLoaded: Boolean;
    procedure RawClearArticles;
    procedure RawSortArticles; override;
    function GetLoaded: Boolean; override;
    procedure UnloadArticles; virtual;
  public
    constructor Create(const AName: string; AFiltersParent, ADisplayFiltersParent: TFiltersCtnr);
    destructor Destroy; override;
    function FindArticleNo(cno: Int64): TArticleBase; override;
    function GetArticleBase(idx: Integer): TArticleBase; override;
    function GetArticleCount: Integer; override;
    function GetThreadCount: Integer; override;
    function GetThreads(idx: Integer): TArticleBase; override;
    function IndexOf(article: TArticleBase): Integer; override;
    procedure PurgeArticles(all, reset: Boolean; const folderName: string);

    procedure RawAddArticle(article: TArticleBase); override;
    procedure RawDeleteArticle(cno: Integer); override;
    procedure RawInsertArticle(index: Integer; article: TArticleBase); override;
    procedure RawRemoveArticle(article: TArticleBase); override;

    procedure SaveArticles(recreateMessageFile: Boolean); virtual; abstract;
  //  property ArticlesLoaded: Boolean read fArticlesLoaded;
  end;

  //-----------------------------------------------------------------------
  // TSubscribedGroup class
  TSubscribedGroup = class(TArticleObjectContainer)
  private
    fOwner: TNNTPAccount;
    fHighWaterMark: Int64;
    fNNTPSettings: TNNTPSettings;
    fPostingSettings: TPostingSettings;
    fDisplaySettings: TDisplaySettings;
    fNickname: string;
    fNeedsUpdate: Boolean;
    fActionPerformedThisSession: Boolean;
    fSortIdx: Integer;

    function GetLastArticle: Int64;
    function GetLowestArticleNo: Int64;
    function GetArticle(idx: Integer): TArticle;
    function GetHighestArticleNo: Int64;
    procedure SetSortIdx(const Value: Integer);
    procedure QuickLoadHeaderStats;
  protected
    function GetServerSettings: TServerSettings; override;
    function GetPostingSettings: TPostingSettings; override;
    function GetDisplaySettings: TDisplaySettings; override;
    function GetIdentity: TIdentity; override;
    procedure GroupArticles; override;
    function GetNext: TArticleContainer; override;
    function GetMessagebaseSize: Int64; override;
    procedure SetSecret(const Value: Boolean); override;
    procedure SaveArticleCounts(reg: TExSettings=nil); override;
    procedure LoadArticleCounts(reg: TExSettings); override;
    function GetUnreadArticleCount: Integer; override;
    procedure UnloadArticles; override;

  public
    fRawLastArticle: Int64;

    constructor Create(AOwner: TNNTPAccount; const AGroupName: string);
    destructor Destroy; override;
    procedure LoadArticles; override;
    procedure LoadUnreadArticleCount;

    procedure AddRawHeaders(headers: TAnsiStringList; XOVERFmt: TStringList); overload;
    procedure AddRawHeaders(headers: TTextFileReader); overload;
    function AddArticle(articleNo: Int64; header: TAnsiStrings; body: TStream; isNew: Boolean): TArticle;
    procedure SaveArticles(recreateMessageFile: Boolean); override;
    procedure LeaveGroup(clearMessages: Boolean = True); override;          // Save articles, and release memory
    function MarkOnLeave: Boolean; override;
    function CreateGroupRegistry(access: DWORD): TExSettings;
    procedure CloseMessageFile; override;
    procedure OpenMessageFile; override;
    function TSGetLastArticle: Int64;
    procedure WriteSettings(reg: TExSettings = nil);

    property Articles[idx: Integer]: TArticle read GetArticle;
    property SortIdx: Integer read fSortIdx write SetSortIdx;
    property Owner: TNNTPAccount read fOwner;
    property LastArticle: Int64 read GetLastArticle;
    property LowestArticleNo: Int64 read GetLowestArticleNo;
    property HighestArticleNo: Int64 read GetHighestArticleNo;
    property NNTPSettings: TNNTPSettings read fNNTPSettings;
    property Nickname: string read fNickname write fNickname;
    property NeedsUpdate: Boolean read fNeedsUpdate write fNeedsUpdate;
    property HighWaterMark: Int64 read fHighWaterMark write fHighWaterMark;
    property ActionPerformedThisSession: Boolean read fActionPerformedThisSession write fActionPerformedThisSession;
  end;

  //-----------------------------------------------------------------------
  // TNNTPAccount class
  TNNTPAccount = class
  private
    fAccountName: string;

    fSubscribedGroups: TStringList;
    fOwner: TNNTPAccounts;
    fCapabilities: TStringList;
    fXOverFMT: TStringList;
    fNoXNews: Boolean;

    fMarkOnLeave: Boolean;
    fScanKeyPhrases: Boolean;
    fLastCheckForNewGroups: TDateTime;
    fRefreshedGroupsList: Boolean;
    fFiltersCtnr: TFiltersCtnr;
    fDisplayFiltersCtnr: TFiltersCtnr;
    fNNTPSettings: TNNTPSettings;

    fPostingSettings: TPostingSettings;
    fDisplaySettings: TDisplaySettings;
    fNNTPServerSettings: TNNTPServerSettings;
    fMailAccountName: string;
    fHasNewGroups: Boolean;
    fUsePipelining: Boolean;
    fGreeting: string;
    fSecret: Boolean;
    fSecretGroupCount: Integer;
    fPostingAccountName: string;
    fSortIdx: Integer;
    fSortGroupsByName: Boolean;

    procedure LoadSubscribedGroups(rootReg: TExSettings);
    procedure SaveSubscribedGroups(rootReg: TExSettings);
    function GetSubscribedGroup(idx: Integer): TSubscribedGroup;
    function GetSubscribedGroupCount: Integer;
    function GetCapabilities: TStringList;
    procedure SetCapabilities(const Value: TStringList);
    function GetXOverFMT: TStringList;
    procedure SetXOverFMT(const Value: TStringList);
    procedure ResetKeyPhraseFlags(idx: Integer);
    procedure SetScanKeyPhrases(const Value: Boolean);
    function GetNext: TNNTPAccount;
    procedure SetAccountName(const Value: string);
    function GetFileName: string;
    procedure SetGreeting(const Value: string);
    procedure SetSecret(const Value: Boolean);
    procedure SetSortIdx(const Value: Integer);
    procedure SortGroups;
    procedure ChangeGroupSortIdx(grp: TSubscribedGroup; idx: Integer);
    procedure SetSortGroupsByName(const Value: Boolean);
  public
    constructor Create(AOwner: TNNTPAccounts);
    destructor Destroy; override;

    function IsSubscribedTo(const groupName: string): Boolean;
    function SubscribeTo(const groupName: string; save: Boolean = True): TSubscribedGroup;
    procedure UnsubscribeTo(const groupName: string; save: Boolean = True);
    function FindSubscribedGroup(const groupName: string): TSubscribedGroup;
    procedure CopySettingsFrom(account: TNNTPAccount);
    function CreateAccountRegistry(access: DWORD): TExSettings;

    property AccountName: string read fAccountName write SetAccountName;
    property NoXNews: Boolean read fNoXNews write fNoXNews;
    property UsePipelining: Boolean read fUsePipelining write fUsePipelining;
    property MarkOnLeave: Boolean read fMarkOnLeave write fMarkOnLeave;
    property Secret: Boolean read fSecret write SetSecret;

    property Owner: TNNTPAccounts read fOwner;
    property SubscribedGroupCount: Integer read GetSubscribedGroupCount;
    property SubscribedGroups[idx: Integer]: TSubscribedGroup read GetSubscribedGroup;

    property Capabilities: TStringList read GetCapabilities write SetCapabilities;
    property XOverFMT: TStringList read GetXOverFMT write SetXOverFMT;

    property ScanKeyPhrases: Boolean read fScanKeyPhrases write SetScanKeyPhrases;
    property Next: TNNTPAccount read GetNext;
    property LastCheckForNewGroups: TDateTime read fLastCheckForNewGroups write fLastCheckForNewGroups;
    property RefreshedGroupsList: Boolean read fRefreshedGroupsList write fRefreshedGroupsList;
    property FileName: string read GetFileName;
    property SortIdx: Integer read fSortIdx write SetSortIdx;

    property NNTPSettings: TNNTPSettings read fNNTPSettings;
    property PostingSettings: TPostingSettings read fPostingSettings;
    property DisplaySettings: TDisplaySettings read fDisplaySettings;
    property FiltersCtnr: TFiltersCtnr read fFiltersCtnr;
    property DisplayFiltersCtnr: TFiltersCtnr read fDisplayFiltersCtnr;
    property NNTPServerSettings: TNNTPServerSettings read fNNTPServerSettings;
    property MailAccountName: string read fMailAccountName write fMailAccountName;
    property PostingAccountName: string read fPostingAccountName write fPostingAccountName;
    property HasNewGroups: Boolean read fHasNewGroups write fHasNewGroups;
    property Greeting: string read fGreeting write SetGreeting;
    property SortGroupsByName: Boolean read fSortGroupsByName write SetSortGroupsByName;
  end;

  TArticleStack = class;

  TNextArticleOption = (naMarkAsRead,
                        naUnreadOnly, naToMeOnly, naReplyOnly, naInterestingOnly,
                        naKeyword0, naKeyword1, naKeyword2, naKeyword3,
                        naKeyword4, naKeyword5, nakeyword6, naKeyword7,
                        naCanLeaveGroup, naCanWrap, naIncludeFirstArticle, naCircularAccounts, naTempFirstArticle, naAnyKeyword, naNoReplies);

  TMatchBozoFlag = (fgName, fgEMail, fgKeyphrase);
  TMatchBozoFlags = set of TMatchBozoFlag;

  TBozo = class
  private
    function GetSearcher: TStringSearcher;
  private
    fName: string;
    fEMail: string;
    fBozodDate: TDateTime;
    fSearcher: TStringSearcher;
    fKeyPhrase: string;
    fFlags: TMatchBozoFlags;
    fAction: TBozoAction;
    property Searcher: TStringSearcher read GetSearcher;
  public
    constructor Create(const AName, AEMail: string; ABozodDate: TDateTime; AAction: TBozoAction);
    constructor CreateNew;
    destructor Destroy; override;
    procedure Assign(b: TBozo);
    function MatchesArticle(art: TArticle): Boolean;

    property Name: string read fName write fName;
    property EMail: string read fEMail write fEMail;
    property BozodDate: TDateTime read fBozodDate write fBozodDate;
    property Flags: TMatchBozoFlags read fFlags write fFlags;
    property KeyPhrase: string read fKeyPhrase write fKeyPhrase;
    property Action: TBozoAction read fAction write fAction;
  end;

  TNextArticleOptions = set of TNextArticleOption;

  //-----------------------------------------------------------------------
  // TNNTPAccounts class
  //
  // Persists list of TNNTPAccount and their subscribed groups in registry

  TNNTPAccounts = class
  private
    fShowSecrets: Boolean;
    function GetKeyPhraseSearcher(idx: Integer): TStringSearcher;
    procedure SetDoVersionCheck(const Value: Integer);
    procedure SetHideDormantConnections(const Value: Boolean);
    procedure SetShowSecrets(const Value: Boolean);
    function GetBozo(i: Integer): TBozo;
    function GetBozoCount: Integer;
    procedure ResetAllBozoFlags;
  private
    fAccounts: TStringList;
    fBatches: TObjectList;
    fLastFoundGroup: TArticleContainer;
    fLastServerAccount: string;
    fLastServer: string;
    fIdentities: TIdentities;
    fKeyPhraseSearchers: array[0..8] of TStringSearcher;
    fNNTPSettings: TNNTPSettings;
    fPostingSettings: TPostingSettings;
    fDisplaySettings: TDisplaySettings;
    fDoVersionCheck: Integer;
    fHideDormantConnections: Boolean;
    fBozos: TObjectList;
    fFiltersCtnr: TFiltersCtnr;
    fDisplayFiltersCtnr: TFiltersCtnr;
    fSecretAccountCount: Integer;

    function GetCount: Integer;
    function GetItems(idx: Integer): TNNTPAccount;
    function GetBatchesCount: Integer;
    function GetBatches(idx: Integer): TNNTPBatch;
    function GetFirstArticle: TArticle;
    procedure ConfirmMessagebaseRoot(rootReg: TExSettings);
    procedure ChangeAccountSortIdx(acct: TNNTPAccount; idx: Integer);

    property KeyPhraseSearcher[idx: Integer]: TStringSearcher read GetKeyPhraseSearcher;
  public
    fNewUserFlag: Boolean;
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromRegistry;
    procedure SaveToRegistry(saveGroupsForAccount: TNNTPAccount = nil);

    procedure Add(account: TNNTPAccount);
    procedure Delete(account: TNNTPAccount);
    procedure Rename(account: TNNTPAccount; const oldName: string);
    procedure UnloadOldContainers(exceptFor: TArticleObjectContainer);

    procedure AddBatch(batch: TNNTPBatch);
    procedure DeleteBatch(idx: Integer);
    function IndexOfBatch(batch: TNNTPBatch): Integer;
    function FindMsgID(const accountName, groupName: string; MsgID: RawByteString): TArticleBase;
    function FindXRef(const XRef: string): TArticleBase;
    function NextArticle(options: TNextArticleOptions; firstArticle: TArticleBase): TArticleBase;
    function GetServerAccountName(const serverName, groupName: string): string;
    function FindServerAccount(const serverName: string; portNo: Integer; sslPort: Integer = -1): TNNTPAccount;
    function FindArticleContainer(const accountName, groupName: string): TArticleContainer;

    procedure PurgeOldArticles;
    procedure SaveDirtyArticles;

    procedure LoadBozoList;
    procedure SaveBozoList;
    procedure AddBozoMatching(art: TArticleBase);
    function ArticleIsFromBozo(article: TArticle; var action: TBozoAction): Boolean;
    procedure RemoveBozoMatching(art: TArticle);
    procedure RemoveOldBozos;
    procedure ReplaceBozos(newBozos: TObjectList);

    procedure ResetKeyPhraseFlags(idx: Integer);
    procedure PerfCue(freq: Integer; const st: string);
    procedure SortAccounts;

    property Count: Integer read GetCount;
    property Items[idx: Integer]: TNNTPAccount read GetItems; default;

    property BatchesCount: Integer read GetBatchesCount;
    property Batches[idx: Integer]: TNNTPBatch read GetBatches;
    property FirstArticle: TArticle read GetFirstArticle;
    property Identities: TIdentities read fIdentities;
    property PostingSettings: TPostingSettings read fPostingSettings;
    property DisplaySettings: TDisplaySettings read fDisplaySettings;
    property NNTPSettings: TNNTPSettings read fNNTPSettings;
    property DoVersionCheck: Integer read fDoVersionCheck write SetDoVersionCheck;
    property HideDormantConnections: Boolean read fHideDormantConnections write SetHideDormantConnections;
    property FiltersCtnr: TFiltersCtnr read fFiltersCtnr;
    property DisplayFiltersCtnr: TFiltersCtnr read fDisplayFiltersCtnr;
    property ShowSecrets: Boolean read fShowSecrets write SetShowSecrets;
    property BozoCount: Integer read GetBozoCount;
    property Bozo[i: Integer]: TBozo read GetBozo;
  end;

  //-----------------------------------------------------------------------
  // TNNTPMessageCacheController class
  //
  // Controls how many messages are cached in memory
  //
  // Keeps a list of recent articles.  When an article drops of the list, it's
  // 'Msg' is free-and-nilled.  Next time the article is referenced, it's message
  // will be reloaded from disk.

  TNNTPMessageCacheController = class(TObjectCache)
  private
    fAlwaysRemove: Boolean;
  protected
    function CanRemove(Obj: TObject): Boolean; override;
  public
    property AlwaysRemove: Boolean read fAlwaysRemove write fAlwaysRemove;
  end;

  //=======================================================================
  // Article stack class for 'go to previous article'

  TArticleStack = class
  private
    fList: TStringList;
    function GetCapacity: Integer;
    function GetIsEmpty: Boolean;

  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    procedure Push(article: TArticleBase);
    function Pop: TArticleBase;
    function Peek: TArticleBase;
    procedure Clear;
    property Capactity: Integer read GetCapacity;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TDontUnloadCache = class(TObjectCache)
  private
    fAlwaysRemove: Boolean;
  protected
    function CanRemove(Obj: TObject): Boolean; override;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Clear; override;
    property AlwaysRemove: Boolean read fAlwaysRemove write fAlwaysRemove;
  end;

function DecodeMultipartSubject(var s: RawByteString; var n, x: Integer; fixIt, blankIt: Boolean): Boolean;

//-----------------------------------------------------------------------
// Global variables

var
  NNTPAccounts: TNNTPAccounts;         // The list of accounts
  gXanaNewsDir: string;                // The root directory
  gMessageBaseRoot: string;
  gBestMessageBaseLocation: string;
  gKeyName: string;                    // Root registry key
  MessageCacheController: TNNTPMessageCacheController;
  DontUnloadCache: TDontUnloadCache;   // Most recently access groups
  LSSync: TCriticalSection;            // Sync header file access
                               // nb.  Header file access is always
                               //      done from the main thread *except*
                               //      when getting the last article number.
                               //      - see TSGetLastArticle.  Therefore
                               //      all header file accesses must be
                               //      controlled by this crit sec.

const
  cFolders = 'Folders';

implementation

uses
  {$if CompilerVersion >= 24.0} // 24.0 = Delphi XE3
    System.Types,
  {$ifend}
  IdNNTPX, unitArticleHash, unitNewsReaderOptions, unitCharsetMap,
  unitSavedArticles, unitMailServices, unitNewUserWizard, unitLog,
  unitCheckVersion, unitNNTPThreadManager, DateUtils, IdGlobalProtocols;


function ForceRenameFile(const Source, Dest: string): Boolean;
begin
  Result := MoveFileEx(PChar(Source), PChar(Dest), MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED);
  if not Result then
  begin
    Sleep(100);
    DeleteFile(Dest);
    Result := RenameFile(Source, Dest);
  end;
end;

function CompareThreads(p1, p2: pointer): Integer;
var
  a1, a2: TArticleBase;
  order: TThreadSortOrder;

  function CompareAuthor(a1, a2: string): Integer;
  begin
    if (Length(a1) <> 0) and (Length(a2) <> 0) then
    begin
      if a1[1] in ['''', '"'] then
        Delete(a1, 1, 1);
      if a2[1] in ['''', '"'] then
        Delete(a2, 1, 1);
    end;
    Result := CompareText(a1, a2);
  end;

  procedure Val(const s: string; var v, code: Integer);
  begin
    v := StrToIntDef(s, -1);
    if v = -1 then
      code := 1;
  end;

  function CompareIPAddress(a1, a2: string): Integer;
  var
    s1, s2: string;
    i1, i2, c1, c2: Integer;
  begin
    repeat
      if (a1 = '') or (a2 = '') then
        Result := CompareText(a1, a2)
      else
      begin
        s1 := SplitString('.', a1);
        s2 := SplitString('.', a2);

        Val(s1, i1, c1);
        Val(s2, i2, c2);

        if (c1 = 0) and (c2 = 0) then
          Result := i1 - i2
        else
          Result := CompareText(s1, s2);
      end;
    until (Result <> 0) or ((a1 = '') and (a2 = ''));
  end;

  function GetThreadNewestMessage(a: TArticleBase): TDateTime;
  var
    dt: TDateTime;

    function GTNM(a: TArticleBase): TDateTime;
    var
      dt: TDateTime;
    begin
      Result := -1;
      while a <> nil do
      begin
        dt := a.fDate;
        if dt > Result then
          Result := dt;

        dt := GTNM(a.fChild);
        if dt > Result then
          Result := dt;

        a := a.fSibling;
      end;
    end;

  begin
    dt := a.fDate;

    Result := GTNM(a.fChild);
    if dt > Result then
      Result := dt;
  end;

begin { CompareThreads }
  Result := 0;
  a1 := TArticleBase(p1);
  a2 := TArticleBase(p2);

  order := a1.Owner.ThreadSortOrder;

  case order of
    soDate: if a1.fDate < a2.fDate then
               Result := -1
             else
               if a1.fDate > a2.fDate then
                 Result := 1;
    soSubject:
     begin
       Result := RawCompareText(a1.DecodedMultipartSubject, a2.DecodedMultipartSubject);
       if Result = 0 then
         Result := a1.fPartNo - a2.fPartNo;
     end;
    soLines: if a1.fLines < a2.fLines then
                Result := -1
              else
                if a1.fLines > a2.fLines then
                  Result := 1;
    soAuthor: Result := CompareAuthor(a1.FromName, a2.FromName);
    soMessageNo: if a1.fArticleNo < a2.fArticleNo then
                    Result := -1
                  else
                    if a1.fArticleNo > a2.fArticleNo then
                      Result := 1;
    soPostingHost: Result := CompareIPAddress(a1.PostingHost, a2.PostingHost);

    soNewestMessage:
      begin
        if a1.fNewestMessage = -1 then
          a1.fNewestMessage := GetThreadNewestMessage(a1);

        if a2.fNewestMessage = -1 then
          a2.fNewestMessage := GetThreadNewestMessage(a2);

        if a1.fNewestMessage < a2.fNewestMessage then
          Result := -1
        else
          if a1.fNewestMessage > a2.fNewestMessage then
            Result := 1;
      end;
  end;

  if Result = 0 then
    if a1.fArticleNo < a2.fArticleNo then
      Result := -1
    else
      if a1.fArticleNo > a2.fArticleNo then
        Result := 1;

  if a1.Owner.ThreadSortDirection = sdDescending then
    Result := -Result
end;

procedure SetThreadFlags(root: TArticleBase; flags: Cardinal);
begin
  while root <> nil do
  begin
    root.fFlags := root.fFlags or flags;
    SetThreadFlags(root.Child, flags);
    root := root.Sibling;
  end;
end;

function GetNodeArticleCount(root: TArticleBase): Integer;
var
  p: TArticleBase;
begin
  Result := 1;

  p := root.Child;
  while p <> nil do
  begin
    Result := Result + GetNodeArticleCount(p);
    p := p.Sibling;
  end;
end;

{ TNNTPAccounts }

function SortBatchesFunction(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TNNTPBatch(Item1).BatchName, TNNTPBatch(Item2).BatchName);
end;

procedure TNNTPAccounts.Add(account: TNNTPAccount);
begin
  account.fOwner := Self;
  account.fSortIdx := Count;
  fAccounts.AddObject(account.AccountName, account);
end;

procedure TNNTPAccounts.AddBatch(batch: TNNTPBatch);
begin
  fBatches.Add(batch);
  fBatches.Sort(SortBatchesFunction);
end;

procedure TNNTPAccounts.AddBozoMatching(art: TArticleBase);
var
  nm, em: string;
begin
  nm := art.FromName;
  em := art.FromEmail;

  if (nm <> '') or (em <> '') then
  begin
    fBozos.Add(TBozo.Create(art.FromName, art.FromEMail, Now, XNOptions.DefaultBozoAction));
    SaveBozoList;
  end;
end;

function TNNTPAccounts.ArticleIsFromBozo(article: TArticle; var action: TBozoAction): Boolean;
var
  i: Integer;
  b: TBozo;
begin
  Result := False;
  if article <> nil then
    for i := 0 to BozoCount - 1 do
    begin
      b := Bozo[i];
      if b.MatchesArticle(article) then
      begin
        action := b.Action;
        Result := True;
        Break;
      end;
    end;
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.ConfirmMessagebaseRoot                       |
 |                                                                      |
 | Sort out old & new message base locations.  The function fixes up    |
 | the gMessagebaseRoot variable which was initialized in the           |
 | initialization section.                                              |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.ConfirmMessagebaseRoot(rootReg: TExSettings);
begin
  if rootReg.HasValue('Messagebase Directory') then
    gMessagebaseRoot := ExpandFileName(rootReg.GetStringValue('Messagebase Directory', gMessagebaseRoot))
  else
  begin     // But handle legacy entries too.
    if not rootReg.GetBooleanValue('Messagebase Directory Confirmed', False) then
      gMessagebaseRoot := gXanaNewsDir
    else
      if rootReg.GetBooleanValue('Use old directory structure', False) then
        gMessagebaseRoot := ExpandFileName(gXanaNewsDir);
    rootReg.SetStringValue('Messagebase Directory', gMessagebaseRoot, '');
  end;
end;

constructor TNNTPAccounts.Create;
begin
  fSecretAccountCount := -1;
  fAccounts := TStringList.Create;
  fBatches := TObjectList.Create;
  fIdentities := TIdentities.Create;
  fNNTPSettings := TNNTPSettings.Create(nil);
  fPostingSettings := TPostingSettings.Create(nil);
  fBozos := TObjectList.Create;
  fFiltersCtnr := TFiltersCtnr.Create(Self, nil);
  fDisplayFiltersCtnr := TFiltersCtnr.Create(Self, nil);
end;

procedure TNNTPAccounts.Delete(account: TNNTPAccount);
var
  idx: Integer;
begin
  idx := fAccounts.IndexOfObject(account);
  if idx >= 0 then
  begin
    account.Free;
    fAccounts.Delete(idx);
    for idx := 0 to Count - 1 do
      Items[idx].fSortIdx := idx;
  end;
end;

procedure TNNTPAccounts.DeleteBatch(idx: Integer);
begin
  fBatches.Delete(idx);
end;

destructor TNNTPAccounts.Destroy;
var
  i: Integer;
begin
  fSecretAccountCount := 0;
  for i := 0 to Count - 1 do
    Items[i].Free;

  for i := 0 to 7 do
    fKeyPhraseSearchers[i].Free;

  fAccounts.Free;
  fBatches.Free;
  fIdentities.Free;
  fNNTPSettings.Free;
  fPostingSettings.Free;
  fDisplaySettings.Free;
  fBozos.Free;
  fFiltersCtnr.Free;
  fDisplayFiltersCtnr.Free;
  inherited Destroy;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.FindArticleContainer                          |
 |                                                                      |
 | Find a group by account/group name.  If the account name is not      |
 | specified then search all accounts.  If the group name is not        |
 | specified then return the first group in the account                 |
 |                                                                      |
 | Parameters:                                                          |
 |   const accountName          Name of the account to find or blank    |
 |   groupName: string          Name of the group to find or blank      |
 |                                                                      |
 | The function returns the TSubscribedGroup that matches the details   |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.FindArticleContainer(const accountName, groupName: string): TArticleContainer;
var
  i, j: Integer;
  account: TNNTPAccount;
  grp: TSubscribedGroup;
begin
  // Cache last Result for speed.
  if Assigned(fLastFoundGroup) and (SameText(fLastFoundGroup.Name, groupName)) then
  begin
    if ((accountName = cFolders) and (fLastFoundGroup is TArticleFolder)) or
       ((fLastFoundGroup is TSubscribedGroup) and (TSubscribedGroup(fLastFoundGroup).Owner.AccountName = accountName)) then
    begin
      Result := fLastFoundGroup;
      Exit;
    end;
  end;

  Result := nil;        // Does the account name indicate archived message folders?
  if accountName = cFolders then
    Result := gArticleFolders.FindFolder(groupName)
  else
    for i := 0 to Count - 1 do
    begin
      account := Items[i];

      // Find the matching entry
      if (accountName = '') or (CompareText(accountName, account.AccountName) = 0) then
      begin
        for j := 0 to account.SubscribedGroupCount - 1 do
        begin
          grp := account.SubscribedGroups[j];
          if (groupName = '') or (CompareText(groupName, grp.Name) = 0) then
          begin
            Result := grp;
            Break;
          end;
        end;

        if Assigned(Result) or (accountName <> '') then
          Break;
      end;
    end;

  if Assigned(Result) then
    fLastFoundGroup := Result;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.FindMsgID                                     |
 |                                                                      |
 | Find a message by account/group/id.  Account & Group can be blank    |
 |                                                                      |
 | Parameters:                                                          |
 |   const accountName, groupName, MsgID: string                        |
 |                                                                      |
 | The function returns the TArticle that matches the group             |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.FindMsgID(const accountName, groupName: string;
  MsgID: RawByteString): TArticleBase;
var
  grp: TArticleContainer;
  acct: TNNTPAccount;
  i, j: Integer;
begin
  Result := nil;
                        // Find the account/group first.  If the group is not
                        // specified the first group in the account will be returned.
                        // if the account's not specified the first group in the first
                        // account will be returned.
  grp := FindArticleContainer(accountName, groupName);

  if groupName <> '' then
  begin                 // They specifically asked for this group, so fail if it's
                        // not found in it.
    if Assigned(grp) then
      Result := TArticle(grp.FindMsgID(MsgID));
  end
  else
    if (grp <> nil) and (grp is TSubscribedGroup) then
    begin
      acct := TSubscribedGroup(grp).Owner;
      i := NNTPAccounts.fAccounts.IndexOf(acct.AccountName);

      while (i < NNTPAccounts.fAccounts.Count) and not Assigned(Result) do
      begin
        acct := TNNTPAccount(NNTPAccounts.fAccounts.Objects[i]);
        j := 0;
        if fShowSecrets or not acct.Secret then
          while (j < acct.SubscribedGroupCount) and not Assigned(Result) do
          begin
            grp := acct.SubscribedGroups[j];
            Result := TArticle(grp.FindMsgID(MsgID));
            Inc(j);
          end;

        Inc(i);
        if AccountName <> '' then
                        // They specifically asked for this account, so fail if it's
                        // not there.
          Break;
      end;
    end;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.FindServerAccount                             |
 |                                                                      |
 | Find an account by server name/port number.                          |
 |                                                                      |
 | Parameters:                                                          |
 |   const serverName: string; portNo: Integer                          |
 |                                                                      |
 | The function returns TNNTPAccount                                    |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.FindServerAccount(
  const serverName: string; portNo: Integer; sslPort: Integer): TNNTPAccount;
var
  i: Integer;
  acct: TNNTPAccount;
begin
  Result := nil;

  for i := 0 to count - 1 do
  begin
    acct := Items[i];
    if sslPort <> -1 then
    begin
      if acct.NNTPServerSettings.SSLRequired and
         (acct.NNTPServerSettings.SSLPort = sslPort) and
         SameText(acct.NNTPServerSettings.ServerName, serverName) then
      begin
        Result := acct;
        Break;
      end;
    end
    else
      if (acct.NNTPServerSettings.ServerPort = portNo) and
         SameText(acct.NNTPServerSettings.ServerName, serverName) then
      begin
        Result := acct;
        Break;
      end;
  end;
end;

function TNNTPAccounts.FindXRef(const XRef: string): TArticleBase;
var
  xr, accountName, serverName, groupName: string;
  artNo: Int64;
  ctnr: TArticleContainer;
begin
  Result := nil;
  xr := XRef;
  serverName := SplitString(' ', xr);
  groupName := SplitString(':', xr);
  accountName := GetServerAccountName(serverName, groupName);
  artNo := StrToIntDef(xr, -1);

  if artNo <> -1 then
  begin
    ctnr := FindArticleContainer(accountName, groupName);
    if Assigned(ctnr) then
      Result := ctnr.FindArticleNo(artNo);
  end;
end;

function TNNTPAccounts.GetBatches(idx: Integer): TNNTPBatch;
begin
  Result := TNNTPBatch(fBatches[idx]);
end;

function TNNTPAccounts.GetBatchesCount: Integer;
begin
  Result := fBatches.Count;
end;

function TNNTPAccounts.GetBozoCount: Integer;
begin
  Result := fBozos.Count;
end;

function TNNTPAccounts.GetBozo(i: Integer): TBozo;
begin
  Result := TBozo(fBozos[i]);
end;

function TNNTPAccounts.GetCount: Integer;
var
  i: Integer;
begin
  if fSecretAccountCount = -1 then
  begin
    fSecretAccountCount := 0;

    for i := 0 to fAccounts.Count - 1 do
    begin
      if not fShowSecrets and TNNTPAccount(fAccounts.Objects[i]).Secret then
        Inc(fSecretAccountCount);
      TNNTPAccount(fAccounts.Objects[i]).fSecretGroupCount := -1;
    end;
  end;

  Result := fAccounts.Count - fSecretAccountCount;
end;

function TNNTPAccounts.GetFirstArticle: TArticle;
var
  i, j: Integer;
  grp: TSubscribedGroup;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    for j := 0 to Items[i].SubscribedGroupCount - 1 do
    begin
      grp := Items[i].SubscribedGroups[j];
      if grp.LoadGetArticleCount > 0 then
      begin
        Result := grp.Articles[0];
        Exit;
      end;
    end;
end;

function TNNTPAccounts.GetItems(idx: Integer): TNNTPAccount;
var
  i: Integer;
begin
  if idx < Count then
    if fSecretAccountCount > 0 then
    begin
      Result := nil;
      for i := 0 to fAccounts.Count - 1 do
        if not TNNTPAccount(fAccounts.Objects[i]).Secret then
          if idx > 0 then
            Dec(idx)
          else
          begin
            Result := TNNTPAccount(fAccounts.Objects[i]);
            Break;
          end;
    end
    else
      Result := fAccounts.Objects[idx] as TNNTPAccount
  else
    Result := nil;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.GetKeyPhraseSearcher                          |
 |                                                                      |
 | Get method for the KeyPhraseSearcher property.  This caches a        |
 | searcher used for searching for keyphrase 'idx'                      |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer       // The keyphrase no to get the searcher for     |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetKeyPhraseSearcher(idx: Integer): TStringSearcher;
begin
  if not Assigned(fKeyPhraseSearchers[idx]) then
    fKeyPhraseSearchers[idx] := TGoogleLikeStringSearcher.Create('', False);

  Result := fKeyPhraseSearchers[idx];
end;

function TNNTPAccounts.GetServerAccountName(const serverName, groupName: string): string;
var
  i, port: Integer;
  sn, st: string;
begin
  if serverName = fLastServer then
    Result := fLastServerAccount
  else
  begin
    fLastServer := '';
    fLastServerAccount := '';
    port := 119;
    i := Pos('(', serverName);
    if i > 0 then
    begin
      sn := Copy(serverName, 1, i - 1);
      st := Copy(serverName, i + 1, MaxInt);
      i := Pos(st, ')');
      if i > 0 then
        st := Copy(st, 1, i - 1);

      port := StrToIntDef(st, -1);
    end
    else
      sn := ServerName;
    Result := sn;
    for i := 0 to Count - 1 do
      if (CompareText(Items[i].NNTPServerSettings.ServerName, sn) = 0) and
         ((port = -1) or (port = Items[i].NNTPServerSettings.ServerPort)) and
         ((groupName = '') or (groupName = '~XNS') or (Items[i].FindSubscribedGroup(groupName) <> nil)) then
      begin
        Result := Items[i].AccountName;
        fLastServerAccount := Result;
        fLastServer := serverName;
        Break;
      end;
  end;
end;

function TNNTPAccounts.IndexOfBatch(batch: TNNTPBatch): Integer;
begin
  Result := fBatches.IndexOf(batch);
end;

procedure TNNTPAccounts.LoadBozoList;
var
  sl: TStringList;
  i: Integer;
  st: string;
  name, email, flagss: string;
  dt: TDateTime;
  bozo: TBozo;

  function StrToBozoFlags(const st: string): TMatchBozoFlags;
  var
    i: Integer;
    f: TMatchBozoFlag;
  begin
    i := 1;
    Result := [];
    for f := Low(TMatchBozoFlag) to High(TMatchBozoFlag) do
      if i <= Length(st) then
      begin
        if st[i] = '1' then
          Include(Result, f);
        Inc(i);
      end
      else
        Break;
  end;

begin
  if FileExists(gMessageBaseRoot + '\bozos.txt') then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(gMessageBaseRoot + '\bozos.txt');

      for i := 0 to sl.Count - 1 do
      begin
        st := sl[i];
        name := SplitString(':', st);
        email := SplitString(':', st);
        dt := StrToDateTimeDef(SplitString('~', st), Now);

        bozo := TBozo.Create(name, email, dt, baIgnore);
        fBozos.Add(bozo);

        if st <> '' then
        begin
          flagss := SplitString('~', st);
          bozo.fFlags := StrToBozoFlags(flagss);
          if st <> '' then
          begin
            flagss := SplitString('~', st);
            bozo.fAction := TBozoAction(StrToIntDef(flagss, 0));
            bozo.fKeyPhrase := st;
          end;
        end;
      end;
    finally
      sl.Free;
    end;
  end
  else
    fBozos.Clear;
end;

procedure TNNTPAccounts.LoadFromRegistry;
var
  reg, reg1, reg2: TExSettings;
  keyNames, keyNames1: TStringList;
  i, j, p: Integer;
  account: TNNTPAccount;
  batch: TNNTPBatch;
  actionName: string;
  Action: TBatchAction;
begin
  keyNames := nil;
  keyNames1 := nil;
  reg1 := nil;
  reg2 := nil;
  reg := CreateExSettings;
  try
    if not reg.HasSection('') then
    begin
      fNewUserFlag := True;
      if not DoNewUserConfig(gKeyName) then
        Halt;
    end
    else
      ConfirmMessagebaseRoot(reg);

    InitializeFolders(reg);
    fDisplaySettings := TDisplaySettings.Create(nil);
    LoadBozoList;
    fIdentities.Load(reg);

    fDoVersionCheck := reg.GetIntegerValue('HTTP Permission Required', 0);
    fHideDormantConnections := reg.GetBooleanValue('Hide Dormant Connections', True);

                                // Load the global settings
    if reg.HasSection('Globals') then
    begin
      reg.Section := 'Globals';
      try
        fNNTPSettings.ReadSettings(reg);
        fPostingSettings.ReadSettings(reg);
        fDisplaySettings.ReadSettings(reg);
        FiltersCtnr.LoadFilters(reg, False);
        DisplayFiltersCtnr.LoadFilters(reg, True);
      finally
        reg.Section := '';
      end;
    end
    else
    begin
      fNNTPSettings.SetIdentityName(NNTPAccounts.Identities.DefaultIdentity.Name);
      fDisplaySettings.DefaultCodepage := DefaultCodePage;
      fPostingSettings.QuoteHeader := '%author% wrote:';
      fPostingSettings.QuoteLineMarker := '> ';
      fPostingSettings.MaxPostLines := 5000;
      fPostingSettings.MaxPostLineLength := DefaultMaxLineLength;
      fPostingSettings.TextPartStyle := tpNNTP;
      fPostingSettings.DefaultCodePage := DefaultCodePage;
    end;

    if reg.HasSection('Accounts') then
    begin
      reg.Section := 'Accounts';

      try
        keyNames := TStringList.Create;
        reg1 := CreateChildSettings(reg);
        reg.GetSectionNames(keyNames);

        for i := 0 to keyNames.Count - 1 do
        begin                     // For each account...
          reg1.Section := keyNames[i];

          account := TNNTPAccount.Create(Self);
          try
            account.fAccountName := reg1.GetStringValue('Account Name', keyNames[i]);
            account.fSortIdx := reg1.GetIntegerValue('Sort Index', i);
            account.NNTPServerSettings.Id := account.fAccountName;
            Account.MarkOnLeave       := reg1.GetBooleanValue('Mark On Leave', False);
            account.fScanKeyPhrases   := reg1.GetBooleanValue('Scan Key Phrases', account.fScanKeyPhrases);

            account.NoXNews           := reg1.GetBooleanValue('No XNEWS', account.NOXNews);
            account.UsePipelining     := reg1.GetBooleanValue('Pipelining', account.UsePipelining);
            account.fGreeting         := reg1.GetStringValue( 'Greeting', '');
            account.fSecret           := reg1.GetBooleanValue('Secret', False);
            account.fSortGroupsByName := reg1.GetBooleanValue('SortGroupsByName', False);

            account.FiltersCtnr.LoadFilters(reg1, False);
            account.DisplayFiltersCtnr.LoadFilters(reg1, True);
            account.NNTPSettings.ReadSettings(reg1);
            account.PostingSettings.ReadSettings(reg1);
            account.DisplaySettings.ReadSettings(reg1);
            account.NNTPServerSettings.ReadSettings(reg1);
            account.fMailAccountName := reg1.GetStringValue('Mail Account Name', 'MAPI');
            account.fPostingAccountName := reg1.GetStringValue('Posting Account Name', '');
            account.HasNewGroups := reg1.GetBooleanValue('Has New Groups', False);
            fAccounts.AddObject(account.AccountName, account);

            account.LoadSubscribedGroups(reg1);
          except
            account.Free;
          end;
        end;
      finally
        reg.Section := '';
      end;
    end;

    SortAccounts;

    if reg.HasSection('Batches') then
    begin
      reg.Section := 'Batches';

      FreeAndNil(keyNames);
      FreeAndNil(reg1);
      keyNames := TStringList.Create;
      reg.GetSectionNames(keyNames);
      reg1 := CreateChildSettings(reg);
      keyNames1 := TStringList.Create;

      for i := 0 to keyNames.Count - 1 do
      begin
        reg1.Section := keyNames[i];

        batch := TNNTPBatch.Create(keyNames[i]);
        batch.BatchName := keyNames[i];
        batch.Scheduled := reg1.GetBooleanValue('Scheduled', False);
        if batch.Scheduled then
          p := 10
        else
          p := 0;
        batch.RunEvery := reg1.GetIntegerValue('Schedule Minutes', p);
        batch.RunAtStart := reg1.GetBooleanValue('Run At Start', False);

        fBatches.Add(batch);
        reg1.GetSectionNames(keyNames1);

        FreeAndNil(reg2);
        reg2 := CreateChildSettings(reg1);

        for j := 0 to keyNames1.Count - 1 do
        begin
          actionName := keyNames1[j];

          reg2.Section := actionName;

          actionName := reg2.GetStringValue('Action Name', actionName);
          p := Pos('_', actionName);
          if p <> 0 then
          begin
            Action := TBatchAction.Create;
            try
              Action.AccountName := Copy(actionName, 1, p - 1);
              Action.GroupName := Copy(actionName, p + 1, MaxInt);
              Action.LoadFromRegistry(reg2);
              batch.fBatchActions.Add(action);
            except
              Action.Free;
              raise;
            end;
          end;
        end;
      end;
      fBatches.Sort(SortBatchesFunction);
    end;
  finally
    reg2.Free;
    reg1.Free;
    keyNames.Free;
    keyNames1.Free;
    reg.Free;
  end;

  if Assigned(gGetVersionThread) then
    if gGetVersionThread.Suspended then
      gGetVersionThread.Start;
  if Assigned(gNetworkMonitorThread) then
    if gNetworkMonitorThread.Suspended then
      gNetworkMonitorThread.Start;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.NextArticle: TArticleBase                     |
 |                                                                      |
 | Returns the next article from a specified article or 'nil'           |
 | Depending on the options specified, the next article can be in a     |
 | different group, or even a different account.                        |
 |                                                                      |
 | Parameters:                                                          |
 |   options: TNextArticleOptions;      // The search options           |
 |   firstArticle: TArticleBase         // The specified article        |
 |                                                                      |
 | The options can be a combination of:                                 |
 |   naMarkAsRead               // Mark the 'first article' as read     |
 |   naUnreadOnly,              // The next article must be unread      |
 |   naToMeOnly,                // The next article must be 'to me'     |
 |   naInterestingOnly,         // The next article must be interesting |
 |   naKeyword0..7,             // The next article must match keyword n|
 |   naCanLeaveGroup,           // If no match is found in this group   |
 |                              // proceed to the next group            |
 |   naCanWrap,                 // If there are ealier articles in the  |
 |                              // group that match, wrap back to them  |
 |   naIncludeFirstArticle,     // Include the first article in the scan|
 |   naCircularAccounts;        // If the next article isn't found in   |
 |                              // the last account, wrap back to the   |
 |                              // first one.                           |
 |                                                                      |
 | The function returns the next article - or nil.                      |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.NextArticle(options: TNextArticleOptions; firstArticle: TArticleBase): TArticleBase;
var
  first, fg: Boolean;
  article, saveArticle: TArticleBase;
  tart: TArticle;
  group, startGroup: TArticleContainer;
  account, startAccount: TNNTPAccount;
  oldSearching: Boolean;
  canWrapInAccount, canWrapInGroup: Boolean;

  // Return True if article is nil, or all of the conditions succeed or are unused
  // The loops (below) will then terminate.
  function EndScan: Boolean;
  begin
    Result := not Assigned(Article) or
              ((not (naUnreadOnly in Options)) or not article.IsRead) and
              ((not (naToMeOnly in Options)) or article.IsMine) and
              ((not (naReplyOnly in Options)) or article.IsReply) and
              ((not (naInterestingOnly in Options)) or article.IsInteresting) and
              ((not (naNoReplies in Options)) or article.HasNoReplies);


    if Assigned(tart) then
      Result := Result and ((not (naAnyKeyword in Options)) or tart.HasAnyKeyPhrase);

    if Assigned(tart) then
      Result := Result and
              ((not (naKeyword0 in Options)) or tart.HasKeyPhrase[0]) and
              ((not (naKeyword1 in Options)) or tart.HasKeyPhrase[1]) and
              ((not (naKeyword2 in Options)) or tart.HasKeyPhrase[2]) and
              ((not (naKeyword3 in Options)) or tart.HasKeyPhrase[3]) and
              ((not (naKeyword4 in Options)) or tart.HasKeyPhrase[4]) and
              ((not (naKeyword5 in Options)) or tart.HasKeyPhrase[5]) and
              ((not (naKeyword6 in Options)) or tart.HasKeyPhrase[6]) and
              ((not (naKeyword7 in Options)) or tart.HasKeyPhrase[7]);
  end;

begin { NextArticle }
  Result := nil;
  first := naIncludeFirstArticle in Options;
  article := firstArticle;
  startGroup := nil;
  oldSearching := False;
  canWrapInAccount := naCanWrap in Options;
  canWrapInGroup := canWrapInAccount;

  try
    if Assigned(article) then
    begin
      saveArticle := article;     // Save the current article so we can push it
                                  // later if we succeed.

      if article is TArticle then
      begin
        tart := TArticle(article);
        startAccount := tart.Account;
        startGroup := tart.Owner;
        account := startAccount;
        oldSearching := startGroup.fSearching;
        startGroup.fSearching := True;
      end
      else
      begin
        startAccount := nil;
        account := nil;
        startGroup := nil;
        tart := nil
      end;

      if naMarkAsRead in Options then     // Mark article as read if required
        article.IsRead := True;

      repeat
        repeat
          if first then           // If 'firstArticle' was specified, include this
            first := False        // article in the scan.
          else
          begin
            article := TArticle(article.Next);
            if Assigned(account) then
              tart := TArticle(article);
          end ;
        until EndScan;

        if not Assigned(article) and canWrapInGroup then
        begin                     // We didn't find a match, but wrap back to the
                                  // beginning if required...
          canWrapInGroup := False;  // ... but only do it once!
          article := saveArticle.Owner.FirstArticle;
          if Assigned(account) then
            tart := TArticle(article);
        end
        else
          Break;
      until EndScan;

      if not Assigned(article) and (naCanLeaveGroup in Options) then
      begin                       // Not found in this group - go to the next
                                  // group if required
        article := nil;
        group := saveArticle.Owner;
        if group is TSubscribedGroup then
        begin
          account := TSubscribedGroup(group).Owner;
          fg := False;
        end
        else
        begin
          account := nil;
          fg := True;
        end;

        while not Assigned(article) and Assigned(group) do
        begin
          if fg then
          begin
            fg := False;
            account := NNTPAccounts.Items[0];
            startAccount := account;
            if not Assigned(account) then Break;
            if account.SubscribedGroupCount = 0 then
              group := nil
            else
              group := account.SubscribedGroups[0];
            startGroup := group;
          end
          else
          begin
            group := Group.Next;
            if canWrapInAccount and (account = startAccount) and (group = nil) then
            begin
              canWrapInAccount := False;
              group := account.SubscribedGroups[0];
            end;

            if group = startGroup then
              group := nil;
          end;

          while (group = nil) and (account <> nil) do
          begin                   // Try the next account if no next group
            account := account.Next;
            if (not Assigned(account)) and (naCircularAccounts in Options) and (NNTPAccounts.Count > 0) then
              account := NNTPAccounts.Items[0];

            if account = startAccount then
            begin
              account := nil;
              Break;
            end;

            if Assigned(account) and (account.SubscribedGroupCount > 0) then
              group := account.SubscribedGroups[0];
          end;

          if (group <> nil) and
             ((not (naUnreadOnly in Options)) or (Group.UnreadArticleCount > 0)) and
             ((not (naReplyOnly in Options)) or (Group.UnreadReplyCount > 0)) and
             ((not (naInterestingOnly in Options)) or (Group.InterestingArticleCount > 0)) and
             ((not (naToMeOnly in Options)) or (Group.UnreadArticleToMeCount > 0)) then
          begin                   // Searh this group for a match
            article := TArticle(group.FirstArticle);
            if Assigned(account) then
              tart := TArticle(article);
            while not EndScan do
            begin
              article := TArticle(article.Next);
              if Assigned(account) then
                tart := TArticle(article);
            end;
          end;
        end;
      end;

      if Assigned(article) then  // We found a matching article.
        Result := article
      else
        if (naIncludeFirstArticle in Options) and not (naTempFirstArticle in Options) then
          Result := firstArticle;
    end;
  finally
    if Assigned(startGroup) then
      startGroup.fSearching := oldSearching;
  end;
end;

procedure TNNTPAccounts.PerfCue(freq: Integer; const st: string);
begin
  if gAudiblePerformanceCues then
  begin
    Windows.Beep(freq, 20);
    LogMessage('PerfCue ' + IntToStr(freq) + ' ' + st);
  end
end;

procedure TNNTPAccounts.PurgeOldArticles;
var
  i, j: Integer;
  acct: TNNTPAccount;
  grp: TSubscribedGroup;
begin
  for i := 0 to Count - 1 do
  begin
    acct := Items[i];
    for j := 0 to acct.SubscribedGroupCount - 1 do
    begin
      grp := acct.SubscribedGroups[j];
      grp.PurgeArticles(False, False, grp.DisplaySettings.PurgeFolder);
      grp.CloseMessageFile;
    end;
  end;

  gArticleFolders.Tidy;
end;

procedure TNNTPAccounts.RemoveBozoMatching(art: TArticle);
var
  i: Integer;
  b: TBozo;
begin
  i := 0;
  while i < BozoCount do
  begin
    b := Bozo[i];
    if b.MatchesArticle(art) then
      fBozos.Delete(i)
    else
      Inc(i);
  end;
  SaveBozoList;
end;

procedure TNNTPAccounts.RemoveOldBozos;
var
  i: Integer;
  t: TDateTime;
  changed: Boolean;
begin
  if XNOptions.AutoRemoveFromBin <= 0 then
    Exit;
  i := 0;
  t := Trunc(Now);
  changed := False;
  t := IncDay(t, -XNOptions.AutoRemoveFromBin);
  while i < fBozos.Count do
  begin
    if TBozo(fBozos[i]).BozodDate < t then
    begin
      fBozos.Delete(i);
      changed := True;
    end
    else
      Inc(i);
  end;
  if changed then
    SaveBozoList;
end;

procedure TNNTPAccounts.Rename(account: TNNTPAccount; const oldName: string);
var
  reg, reg1: TExSettings;
  i, j: Integer;
  batch: TNNTPBatch;
  action: TBatchAction;
begin
  // Note: The account has already been renamed.
  //       Adjust the folders & settings to reflect this.
  reg1 := nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Accounts';
    reg.RenameSection(FixFileNameString(oldName), FixFileNameString(account.AccountName));

    try
      if DirectoryExists(gMessageBaseRoot + '\' + FixFileNameString(oldName)) then
        if not RenameFile(gMessageBaseRoot + '\' + FixFileNameString(oldName), gMessageBaseRoot + '\' + FixFileNameString(account.AccountName)) then
          raise Exception.Create('Cannot rename directory');

      reg1 := CreateChildSettings(reg, FixFileNameString(account.AccountName));
      reg1.SetStringValue('Account Name', account.AccountName, FixFileNameString(account.AccountName));

      for i := 0 to NNTPAccounts.BatchesCount - 1 do
      begin
        batch := NNTPAccounts.Batches[i];

        for j := 0 to batch.ActionCount - 1 do
        begin
          action := batch.Actions[j];
          if CompareText(action.AccountName, oldName) = 0 then
            action.AccountName := account.AccountName;
        end;
      end;

      SaveToRegistry(account);
    except
      reg.RenameSection(FixFileNameString(account.AccountName), FixFileNameString(oldName));
      raise;
    end
  finally
    reg1.Free;
    reg.Free;
  end;
end;

procedure TNNTPAccounts.ResetKeyPhraseFlags(idx: Integer);
var
  i: Integer;
  account: TNNTPAccount;
begin
  for i := 0 to Count - 1 do
  begin
    account := Items[i];
    account.ResetKeyPhraseFlags(idx);
  end;
end;

procedure TNNTPAccounts.SaveBozoList;
var
  sl: TStringList;
  i: Integer;
  st: string;
  bozo: TBozo;

  function BozoFlagsToStr(flags: TMatchBozoFlags): string;
  var
    fg: TMatchBozoFlag;
    i: Integer;
  begin
    Result := '000';
    i := 1;
    for fg := Low(TMatchBozoFlag) to High(TMatchBozoFlag) do
    begin
      if fg in flags then
        Result[i] := '1';
      Inc(i);
    end;
  end;

begin
  if fBozos.Count > 0 then
  begin
    sl := TStringList.Create;
    try
      for i := 0 to fBozos.Count - 1 do
      begin
        bozo := TBozo(fBozos[i]);
        st := StringReplace(bozo.Name, ':', '.', [rfreplaceAll]) + ':' +
              StringReplace(bozo.EMail, ':', '.', [rfreplaceAll]) + ':' +
              DateTimeToStr(bozo.BozodDate);

        if (bozo.fFlags <> [fgName, fgEMail]) or (bozo.fAction <> baIgnore) then
        begin
          st := st + '~' + BozoFlagsToStr(bozo.fFlags);
          st := st + '~' + IntToStr(Integer(bozo.fAction));
          if fgKeyphrase in bozo.fFlags then
            st := st + '~' + StringReplace(bozo.fKeyPhrase, '~', '-', [rfReplaceAll]);
        end;
        sl.Add(st)
      end;

      sl.SaveToFile(gMessageBaseRoot + '\bozos.txt')
    finally
      sl.Free;
    end;
  end
  else
    DeleteFile(gMessageBaseRoot + '\bozos.txt');
end;

procedure TNNTPAccounts.SaveDirtyArticles;
var
  i, j: Integer;
  acct: TNNTPAccount;
  grp: TSubscribedGroup;
begin
  for i := 0 to Count - 1 do
  begin
    acct := Items[i];

    for j := 0 to acct.SubscribedGroupCount - 1 do
    begin
      grp := acct.SubscribedGroups[j];
      if grp.FlagsDirty then
        grp.SaveArticles(False);
    end;
  end;
end;

procedure TNNTPAccounts.SaveToRegistry(saveGroupsForAccount: TNNTPAccount = nil);
var
  reg, reg1, reg2: TExSettings;
  i, j, p: Integer;
  account: TNNTPAccount;
  batch: TNNTPBatch;
  keyNames, keyNames1: TStringList;
  Action: TBatchAction;
  actionName, accName: string;
begin
  reg1 := nil;
  reg2 := nil;
  keyNames := nil;
  keyNames1 := nil;

  fIdentities.Save;

  SortAccounts;

  reg := CreateExSettings;
  try
    reg.Section := 'Globals';

    fNNTPSettings.WriteSettings(reg);
    fPostingSettings.WriteSettings(reg);
    fDisplaySettings.WriteSettings(reg);
    FiltersCtnr.SaveFilters(reg, False);
    DisplayFiltersCtnr.SaveFilters(reg, True);
  finally
    FreeAndNil(reg);
  end;

  try
    keyNames := TStringList.Create;
    reg := CreateExSettings;
    reg.Section := 'Accounts';
    reg.GetSectionNames(keyNames);

    reg1 := CreateChildSettings(reg);
    try
      for i := 0 to fAccounts.Count - 1 do
      begin
        account := TNNTPAccount(fAccounts.Objects[i]);
        accName := FixFileNameString(account.AccountName);

        reg1.Section := accName;

        p := keyNames.IndexOf(accName);
        if p >= 0 then
          keyNames.Delete(p);

        reg1.SetStringValue('Account Name', account.AccountName, accName);
        reg1.SetIntegerValue('Sort Index', account.fSortIdx, -1);

        reg1.SetStringValue('Identity', account.NNTPSettings.Identity.Name, NNTPAccounts.Identities.DefaultIdentity.Name);

        reg1.SetBooleanValue('Mark On Leave', account.MarkOnLeave, False);
        reg1.SetBooleanValue('No XNEWS', account.NoXNews, False);
        reg1.SetBooleanValue('Pipelining', account.UsePipelining, True);
        reg1.SetStringValue('Mail Account Name', account.fMailAccountName, 'MAPI');
        reg1.SetStringValue('Posting Account Name', account.PostingAccountName, '');
        reg1.SetBooleanValue('Has New Groups', account.HasNewGroups, False);
        reg1.SetStringValue('Greeting', account.Greeting, '');
        reg1.SetBooleanValue('Secret', account.Secret, False);
        reg1.SetBooleanValue('SortGroupsByName', account.SortGroupsByName, False);

        reg1.SetBooleanValue('Scan Key Phrases', account.ScanKeyPhrases, False);

        reg1.DeleteValue('SMTPMail');   // Tidy up registry for existing users
        reg1.DeleteSection('SMTP Server');  // Remove later!

        account.FiltersCtnr.SaveFilters(reg1, False);
        account.DisplayFiltersCtnr.SaveFilters(reg1, True);
        account.NNTPSettings.WriteSettings(reg1);
        account.PostingSettings.WriteSettings(reg1);
        account.DisplaySettings.WriteSettings(reg1);
        account.NNTPServerSettings.WriteSettings(reg1);

        if account = saveGroupsForAccount then
          account.SaveSubscribedGroups(reg1);
      end;
    finally
      FreeAndNil(reg1);
    end;

    if Assigned(keyNames) then
      for i := 0 to keyNames.Count - 1 do
      begin
        actionName := keyNames[i];
        reg.DeleteSection(keyNames[i]);
        PurgeDirectory(gMessageBaseRoot + '\' + actionName);
      end;

    if fBatches.Count > 0 then
    begin
      FreeAndNil(keyNames);
      keyNames := TStringList.Create;
      keyNames1 := TStringList.Create;
      reg.Close;
      reg.Section := 'Batches';
      reg.GetSectionNames(keyNames);
      reg1 := CreateChildSettings(reg);

      for i := 0 to fBatches.Count - 1 do
      begin
        batch := TNNTPBatch(fBatches[i]);
        p := keyNames.IndexOf(batch.BatchName);

        if p >= 0 then
          keyNames.Delete(p);

        reg1.Section := batch.BatchName;
        if batch.Scheduled then
          p := 10
        else
          p := 0;
        reg1.SetIntegerValue('Schedule Minutes', batch.RunEvery, p);
        reg1.SetBooleanValue('Scheduled', batch.Scheduled, False);
        reg1.SetBooleanValue('Run At Start', batch.RunAtStart, False);

        FreeAndNil(reg2);
        reg1.GetSectionNames(keyNames1);
        reg2 := CreateChildSettings(reg1);

        for j := 0 to batch.fBatchActions.Count - 1 do
        begin
          Action := TBatchAction(batch.fBatchActions[j]);
          actionName := Action.AccountName + '_' + Action.GroupName;

          p := keyNames1.IndexOf(FixFileNameString(actionName));

          if p >= 0 then
            keyNames1.Delete(p);

          reg2.Section := FixFileNameString(actionName);
          reg2.SetStringValue('Action name', actionName, FixFileNAmeString(actionName));
          Action.SaveToRegistry(reg2);
        end;

        for j := 0 to keyNames1.Count - 1 do
          reg1.DeleteSection(keyNames1[j]);
      end;

      for i := 0 to keyNames.Count - 1 do
        reg.DeleteSection(keyNames[i]);
    end
    else
    begin
      if not Assigned(reg) then
        reg := CreateExSettings;

      reg.DeleteSection('Batches');
    end;
  finally
    reg2.Free;
    reg1.Free;
    reg.Free;
    keyNames.Free;
    keyNames1.Free;
  end;
end;

{ TNNTPAccount }

procedure TNNTPAccount.ChangeGroupSortIdx(grp: TSubscribedGroup; idx: Integer);
var
  i: Integer;
  g: TSubscribedGroup;
begin
  if idx = grp.fSortIdx then Exit;

  i := SubscribedGroupCount - 1;
  while i >= 0 do
  begin
    g := SubscribedGroups[i];
    if g = grp then
      g.fSortIdx := idx
    else
      if g.fSortIdx <> -1 then
        if g.fSortIdx >= idx then
          Inc(g.fSortIdx);
    Dec(i);
  end;

  SortGroups;

  for i := 0 to SubscribedGroupCount - 1 do
    SubscribedGroups[i].fSortIdx := i;
end;

procedure TNNTPAccount.CopySettingsFrom(account: TNNTPAccount);
begin
  Self.fNoXNews := account.NoXNews;
  Self.fMarkOnLeave := account.MarkOnLeave;
  Self.fScanKeyPhrases := account.ScanKeyPhrases;
  Self.fFiltersCtnr.AssignFilters(account.FiltersCtnr);
  Self.fDisplayFiltersCtnr.AssignFilters(account.DisplayFiltersCtnr);
  Self.fNNTPSettings.Assign(account.NNTPSettings);
  Self.fPostingSettings.Assign(account.PostingSettings);
  Self.fDisplaySettings.Assign(account.DisplaySettings);
  Self.fNNTPServerSettings.Assign(account.NNTPServerSettings);
  Self.fMailAccountName := account.fMailAccountName;
  Self.fPostingAccountName := account.fPostingAccountName;
  Self.fGreeting := account.fGreeting;
  Self.fSecret := account.fSecret;
end;

constructor TNNTPAccount.Create(AOwner: TNNTPAccounts);
begin
  inherited Create;
  fOwner := AOwner;
  fSubscribedGroups := TStringList.Create;
  fFiltersCtnr := TFiltersCtnr.Create(Self, AOwner.FiltersCtnr);
  fDisplayFiltersCtnr := TFiltersCtnr.Create(Self, AOwner.DisplayFiltersCtnr);
  fNNTPSettings := TNNTPSettings.Create(Owner.fNNTPSettings);
  fPostingSettings := TPostingSettings.Create(Owner.fPostingSettings);
  fDisplaySettings := TDisplaySettings.Create(Owner.fDisplaySettings);
  fNNTPServerSettings := TNNTPServerSettings.Create(nil);
  fUsePipelining := True;
  fSecretGroupCount := -1;
end;

function TNNTPAccount.CreateAccountRegistry(access: DWORD): TExSettings;
begin
  Result := CreateExSettings;
  Result.Section := 'Accounts\' + FixFileNameString(AccountName);
  if not Result.Open((access and KEY_WRITE) = 0) then
    FreeAndNil(Result);
end;

destructor TNNTPAccount.Destroy;
var
  i: Integer;
begin
  ThreadManager.ClearGettersForAccount(Self);
  DontUnloadCache.Clear;
  if Assigned(NNTPAccounts) then
  begin
    NNTPAccounts.fLastFoundGroup := nil;
    NNTPAccounts.fLastServerAccount := '';
    NNTPAccounts.fLastServer := ''
  end;
  fSecretGroupCount := 0;
  for i := 0 to SubscribedGroupCount - 1 do
    SubscribedGroups[i].Free;
  fSubscribedGroups.Free;
  fCapabilities.Free;
  fXOverFMT.Free;               // Will exist if SetXOverFMT has been called.
                                // (contains the XOVER Format list)
  fFiltersCtnr.Free;
  fDisplayFiltersCtnr.Free;
  fNNTPSettings.Free;
  fPostingSettings.Free;
  fDisplaySettings.Free;
  fNNTPServerSettings.Free;
  inherited Destroy;
end;

function TNNTPAccount.FindSubscribedGroup(const groupName: string): TSubscribedGroup;
var
  idx: Integer;
begin
  idx := fSubscribedGroups.IndexOf(groupName);
  if idx >= 0 then
  begin
    Result := TSubscribedGroup(fSubscribedGroups.Objects[idx]);
    if (not NNTPAccounts.fShowSecrets) and Result.fSecret then
      Result := nil;
  end
  else
    Result := nil;
end;

function TNNTPAccount.GetCapabilities: TStringList;
begin
  Result := fCapabilities;
end;

function TNNTPAccount.GetFileName: string;
begin
  Result := gMessageBaseRoot + '\' + FixFileNameString(AccountName) + '\newsgroups.dat';
end;

function TNNTPAccount.GetNext: TNNTPAccount;
var
  i, idx: Integer;
begin
  Result := nil;
  if NNTPAccounts.fSecretAccountCount > 0 then
  begin
    idx := -1;

    for i := 0 to NNTPAccounts.Count - 1 do
      if NNTPAccounts[i].AccountName = AccountName then
      begin
        idx := i;
        Break;
      end;
  end
  else
    idx := NNTPAccounts.fAccounts.IndexOf(AccountName);
  if idx >= 0 then
  begin
    Inc(idx);
    if idx < NNTPAccounts.Count then
      Result := NNTPAccounts.Items[idx];
  end;
end;

function TNNTPAccount.GetSubscribedGroup(idx: Integer): TSubscribedGroup;
var
  i: Integer;
begin
  if fSecretGroupCount > 0 then
  begin
    Result := nil;
    for i := 0 to fSubscribedGroups.Count - 1 do
      if not TSubscribedGroup(fSubscribedGroups.Objects[i]).fSecret then
        if idx = 0 then
        begin
          Result := TSubscribedGroup(fSubscribedGroups.Objects[i]);
          Break;
        end
        else
          Dec(idx);
  end
  else
    Result := fSubscribedGroups.Objects[idx] as TSubscribedGroup;
end;

function TNNTPAccount.GetSubscribedGroupCount: Integer;
var
  i: Integer;
begin
  if fSecretGroupCount = -1 then
  begin
    fSecretGroupCount := 0;
    for i := 0 to fSubscribedGroups.Count - 1 do
      if not NNTPAccounts.fShowSecrets and TSubscribedGroup(fSubscribedGroups.Objects[i]).Secret then
        Inc(fSecretGroupCount);
  end;

  Result := fSubscribedGroups.Count - fSecretGroupCount;
end;

function TNNTPAccount.GetXOverFMT: TStringList;
begin
  Result := fXOVERFmt;
end;

function TNNTPAccount.IsSubscribedTo(const groupName: string): Boolean;
begin
  Result := fSubscribedGroups.IndexOf(groupName) >= 0
end;

procedure TNNTPAccount.LoadSubscribedGroups(rootReg: TExSettings);
var
  reg, reg1: TExSettings;
  keyNames: TStringList;
  i: Integer;
  group: TSubscribedGroup;
begin
  keyNames := nil;
  reg1 := nil;

  if rootReg.HasSection('Subscribed Groups') then
  begin

    reg := CreateChildSettings(rootReg, 'Subscribed Groups');
    try
      reg1 := CreateChildSettings(reg);
      keyNames := TStringList.Create;

      reg.GetSectionNames(keyNames);

      for i := 0 to keyNames.Count - 1 do
      begin
        group := TSubscribedGroup.Create(Self, keyNames[i]);
        reg1.Section := keyNames[i];
        group.fHighWaterMark := reg1.GetInteger64Value('Last Article', 0);
        group.fSortIdx := reg1.GetIntegerValue('Sort Index', -1);
        group.FiltersCtnr.LoadFilters(reg1, False);
        group.DisplayFiltersCtnr.LoadFilters(reg1, True);
        group.NNTPSettings.ReadSettings(reg1);
        group.PostingSettings.ReadSettings(reg1);
        group.DisplaySettings.ReadSettings(reg1);
        group.Nickname := reg1.GetStringValue('Nickname', '');
        group.fSecret := reg1.GetBooleanValue('Secret', False);
        group.LoadArticleCounts(reg1);
        fSubscribedGroups.AddObject(keyNames[i], group);
      end;

      SortGroups;
    finally
      keyNames.Free;
      reg1.Free;
      reg.Free;
    end;
  end;
end;

procedure TNNTPAccount.ResetKeyPhraseFlags(idx: Integer);
var
  j, k: Integer;
  grp: TSubscribedGroup;
  article: TArticle;
begin
  if ScanKeyPhrases then
    for j := 0 to SubscribedGroupCount - 1 do
    begin
      grp := SubscribedGroups[j];

      for k := 0 to grp.LoadGetArticleCount - 1 do
      begin
        article := grp.Articles[k];
        if (article.fFlags and fgScannedKeyPhrases) <> 0 then
          article.fFlags := article.fFlags and not (fgScannedKeyPhrases or fgKeyPhraseMask);
      end;
      grp.fFlagsDirty := True;
    end;
end;

procedure TNNTPAccount.SaveSubscribedGroups(rootReg: TExSettings);
var
  reg, reg1: TExSettings;
  i: Integer;
  group: TSubscribedGroup;
begin
  reg1 := nil;
  reg := CreateChildSettings(rootReg, 'Subscribed Groups');
  try
    reg1 := CreateChildSettings(reg);

    for i := 0 to fSubscribedGroups.Count - 1 do
    begin
      group := TSubscribedGroup(fSubscribedGroups.Objects[i]);
      reg1.Section := group.Name;

      reg1.SetInteger64Value('Last Article', group.fHighWaterMark, 0);
      reg1.SetBooleanValue('Secret', group.Secret, False);
      reg1.SetIntegerValue('Sort Index', group.fSortIdx, -1);

      group.WriteSettings(reg1);
    end;
  finally
    reg1.Free;
    reg.Free;
  end;
end;

procedure TNNTPAccount.SetAccountName(const Value: string);
var
  oldName: string;
begin
  if value <> AccountName then
  begin
    if AccountName = '' then
      fAccountName := Value
    else
    begin
      ThreadManager.ClearGettersForAccount(Self);
      DontUnloadCache.Clear;
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        oldName := fAccountName;
        fAccountName := Value;
        try
          NNTPAccounts.Rename(Self, oldName);
        except
          fAccountName := oldName;
          raise;
        end;
      finally
        SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGED, 0, 0);
      end;
    end;
  end;
end;

procedure TNNTPAccount.SetCapabilities(const Value: TStringList);
begin
  if not Assigned(fCapabilities) then
    fCapabilities := TStringList.Create;

  fCapabilities.Assign(Value);
end;

procedure TNNTPAccount.SetGreeting(const Value: string);
var
  reg: TExSettings;
begin
  if value <> fGreeting then
  begin
    fGreeting := Value;
    reg := CreateAccountRegistry(KEY_READ or KEY_WRITE);
    try
      reg.SetStringValue('Greeting', fGreeting, '')
    finally
      reg.Free;
    end;
  end;
end;

procedure TNNTPAccount.SetScanKeyPhrases(const Value: Boolean);
begin
  if fScanKeyPhrases <> Value then
  begin
    fScanKeyPhrases := Value;
    if Value then
      Self.ResetKeyPhraseFlags(-1);
  end;
end;

procedure TNNTPAccount.SetSecret(const Value: Boolean);
begin
  fSecret := Value;
  fOwner.fSecretAccountCount := -1;
end;

procedure TNNTPAccount.SetSortGroupsByName(const Value: Boolean);
begin
  if FSortGroupsByName <> Value then
  begin
    SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
    try
      FSortGroupsByName := Value;
      SortGroups;
    finally
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGED, 0, 0);
    end;
  end;
end;

procedure TNNTPAccount.SetSortIdx(const Value: Integer);
begin
  Owner.ChangeAccountSortIdx(Self, Value);
end;

procedure TNNTPAccount.SetXOverFMT(const Value: TStringList);
begin
  if not Assigned(fXOverFMT) then
    fXOverFMT := TStringList.Create;

  fXOverFMT.Assign(Value);
end;

function OldCompareGroups(List: TStringList; Index1, Index2: Integer): Integer;
var
  grp1, grp2: TSubscribedGroup;
  st1, st2: string;
begin
  grp1 := TSubscribedGroup(List.Objects[Index1]);
  grp2 := TSubscribedGroup(List.Objects[Index2]);

  if grp1.Nickname = '' then
    st1 := grp1.Name
  else
    st1 := grp1.Nickname;

  if grp2.Nickname = '' then
    st2 := grp2.Name
  else
    st2 := grp2.Nickname;

  Result := CompareText(st1, st2);
end;

function CompareGroups(List: TStringList; Index1, Index2: Integer): Integer;
var
  grp1, grp2: TSubscribedGroup;
begin
  grp1 := TSubscribedGroup(List.Objects[Index1]);
  grp2 := TSubscribedGroup(List.Objects[Index2]);

  Result := grp1.fSortIdx - grp2.fSortIdx;
end;

procedure TNNTPAccount.SortGroups;
var
  i: Integer;
begin
  if fSortGroupsByName then
    fSubscribedGroups.CustomSort(OldCompareGroups)
  else
    fSubscribedGroups.CustomSort(CompareGroups);

  for i := 0 to fSubscribedGroups.Count - 1 do
    TSubscribedGroup(fSubscribedGroups.Objects[i]).fSortIdx := i;
end;

function TNNTPAccount.SubscribeTo(const groupName: string; save: Boolean = True): TSubscribedGroup;
var
  idx: Integer;
  reg: TExSettings;
  grp: TSubscribedGroup;
begin
  idx := fSubscribedGroups.IndexOf(groupName);

  if idx = -1 then
  begin
    grp := TSubscribedGroup.Create(Self, groupName);
    grp.fSortIdx := fSubscribedGroups.Count;
    idx := fSubscribedGroups.AddObject(groupName, grp);
    SortGroups;
    CreateDir(FixFileNameString(AccountName) + '\' + FixFileNameString(groupName));
    reg := grp.CreateGroupRegistry(KEY_READ or KEY_WRITE);
    try
      reg.SetInteger64Value('Last Article', 0);
    finally
      reg.Free;
    end;
    if save then
      Owner.SaveToRegistry(Self);
  end;

  Result := TSubscribedGroup(fSubscribedGroups.Objects[idx])
end;

procedure TNNTPAccount.UnsubscribeTo(const groupName: string; save: Boolean = True);
var
  idx: Integer;
  reg: TExSettings;
  gn: string;
begin
  idx := fSubscribedGroups.IndexOf(groupName);

  if idx >= 0 then
  begin
    gn := FixFileNameString(groupName);
    fSubscribedGroups.Objects[idx].Free;
    fSubscribedGroups.Delete(idx);
    PurgeDirectory(gMessageBaseRoot + '\' + FixFileNameString(AccountName) + '\' + gn);

    reg := CreateExSettings;
    try
      reg.Section := 'Accounts' + '\' + FixFileNameString(AccountName) + '\Subscribed Groups';
      reg.DeleteSection(gn);
    finally
      reg.Free;
    end;

    for idx := 0 to fSubscribedGroups.Count - 1 do
      TSubscribedGroup(fSubscribedGroups.Objects[idx]).fSortIdx := idx;

    if save then
      Owner.SaveToRegistry(Self);
  end
end;

{ TSubscribedGroup }

function TSubscribedGroup.AddArticle(articleNo: Int64; header: TAnsiStrings; body: TStream; isNew: Boolean): TArticle;
var
  article: TArticle;
  filterIt, bozo: Boolean;
  action: TBozoAction;

  function GetLineCount(body: TStream): Integer;
  var
    P: PAnsiChar;
    L: Integer;
  begin
    Result := 0;
    if body is TMemoryStream then
    begin
      P := TMemoryStream(body).Memory;
      L := TMemoryStream(body).Size;
      while L > 1 do
      begin
        if P^ = #13 then
        begin
          Dec(L);
          Inc(P);
          if P^ = #10 then
            Inc(Result);
        end;
        Dec(L);
        Inc(P);
      end;
    end
  end;

begin
  if not Loaded then
    LoadArticles;

  Result := nil;
  article := TArticle.Create(Self);
  try
    article.Initialize(articleNo, header);

    article.fMsg := TmvMessage.Create(article);
    article.fMsg.Header := header;
    article.fMsg.AddData(body);

    if isNew then
    begin
      article.fFlags := article.fFlags or fgNew;
      if Assigned(body) then
      begin
        if article.fBytes = 0 then
          article.fBytes := body.Size;
        if article.fLines = 0 then
          article.fLines := GetLineCount(body);
      end;
    end;

    filterIt := filtersCtnr.BlockArticle(article);
    bozo := False;
    if not filterIt then
    begin
      bozo := NNTPAccounts.ArticleIsFromBozo(article, action);
      if bozo and (action = baDontDownload) then
        filterIt := True;
    end;

    if not filterIt then
    begin
      if bozo then article.IsRead := True;
      article.SaveMessageBody;

      RawAddArticle(article);
      fUnreadArticleCount := -1;
      fUnloadedArticleCount := -1;
      fInterestingArticleCount := -1;
      fUnreadInterestingArticleCount := -1;
      MessageCacheController.Add(article);

      if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) then
        fThreads.Add(article);

      Result := article;
    end
    else
    begin
      if article.fArticleNo > HighWaterMark then
        HighWaterMark := article.fArticleNo
      else
        if article.fArticleNo < HighWaterMark then
          HighWaterMark := 0;
      article.Free;
    end;
  except
    article.Free;
    raise;
  end;
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.AddRawHeaders                                       |
 |                                                                      |
 | Add a list of the headers (in XOVER format) to the articles list.    |
 | overwrite the existing articles if the 'replace' flag is set.        |
 |                                                                      |
 | nb.  This is not intended to be thread safe.                         |
 |                                                                      |
 | It's called after downloading article headers.                       |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   headers: TStringList      Headers to add                           |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.AddRawHeaders(headers: TAnsiStringList; XOVERFmt: TStringList);
var
  i, n: Integer;
  articleNo: Int64;
  subject: RawByteString;
  from: RawByteString;
  date: TDateTime;
  MessageID: RawByteString;
  references: RawByteString;
  Bytes: Cardinal;
  lines: Cardinal;
  exd, val, hdr, st: string;
  article: TArticle;
  filterIt, bozo: Boolean;
  action: TBozoAction;
begin
  if not fArticlesLoaded then
    LoadArticles;

  fArticles.Capacity := fArticles.Capacity + headers.Count;

  for i := 0 to headers.Count - 1 do
  begin
    if Headers[I] = '' then
      Continue;

    ParseXOVER(headers[i], articleNo, subject, from, date, MessageID, references, bytes, lines, exd);

    article := TArticle.Create(Self);
    try
      article.fArticleNo := articleNo;
      article.fMessageID := RawTrim(MessageID);
      article.fBytes := bytes;
      article.fLines := lines;
      article.fReferences := RawTrim(references);
      article.fFrom := from;
      article.fSubject := subject;
      article.fDate := date;
      article.fMessageOffset := -1;
      article.fFlags := fgNew;

      n := 7;

      if Assigned(XOverFMT) then
        while (n < XOverFMT.Count) and (exd <> '') do
        begin
          val := Fetch(exd, #9);
          st  := XOverFMT[n];
          hdr := Fetch(st, ':');
          Inc(n);
          if SameText(st, 'full') then
            article.fTempExtraHeaders := article.fTempExtraHeaders + #9 + val
          else
            article.fTempExtraHeaders := article.fTempExtraHeaders + #9 + hdr + ':' + val;
        end;

      if article.fTempExtraHeaders <> '' then
        Delete(article.fTempExtraHeaders, 1, 1);


      filterIt := filtersCtnr.BlockArticle(article);
      bozo := False;
      if not filterIt then
      begin
        bozo := NNTPAccounts.ArticleIsFromBozo(article, action);
        if bozo and (action = baDontDownload) then
          filterIt := True;
      end;

      if not filterIt then
      begin
        if bozo then article.IsRead := True;
        RawAddArticle(article);
      end
      else
      begin
        if article.fArticleNo > HighWaterMark then
          HighWaterMark := article.fArticleNo
        else
          if article.fArticleNo < HighWaterMark then
            HighWaterMark := 0;
        article.Free;
      end;
    except
      article.Free;
      raise;
    end;
  end;

  NNTPAccounts.PerfCue(660, 'Added Article Headers');
  ReSortArticles;
  NNTPAccounts.PerfCue(880, 'Sorted articles');
  fArticlesLoaded := True;
end;

procedure TSubscribedGroup.AddRawHeaders(headers: TTextFileReader);
var
  linesTotal: Integer;
  linesDeleted: Integer;
  P: PAnsiChar;
  article: TArticle;
  lastGoodDate: TDateTime;
  raw: RawByteString;
  st: string;

  function NextItem(var S: RawByteString): PAnsiChar;
  begin
    Result := P;
    if Result <> nil then
    begin
      while not (Result^ in [#0, #9]) do
        Inc(Result);

      SetString(S, P, Result - P);
      if Result^ <> #0 then
        Inc(Result);
    end
    else
      S := '';
  end;

  function NextItemStr: RawByteString;
  begin
    P := NextItem(Result);
  end;

begin
  if not fArticlesLoaded then
    LoadArticles;

  UpdateGlobalOffsetFromUTC;

  linesTotal := 0;
  linesDeleted := 0;
  lastGoodDate := 0;
  while headers.ReadLn(raw) do
  begin
    Inc(linesTotal);
    P := PAnsiChar(raw);

    article := TArticle.Create(Self);
    try
      article.fArticleNo := RawStrToInt64(NextItemStr);
      P := NextItem(article.fSubject);
      P := NextItem(article.fFrom);
      try
        article.fDate := FixedGMTToLocalDateTime(NextItemStr);
        lastGoodDate := article.fDate;
      except
        article.fDate := lastGoodDate;
      end;
      P := NextItem(article.fMessageID);
      P := NextItem(article.fReferences);
      article.fBytes := RawStrToIntDef(NextItemStr, 0);
      article.fLines := RawStrToIntDef(NextItemStr, 0);
      article.fFlags := RawStrToIntDef(NextItemStr, 0) and $08FFFFFF;
      article.fMessageOffset := RawStrToInt64Def(NextItemStr, -1);

      if article.IsDeleted then
        article.Owner.fNeedsPurge := True;
    except
      FreeAndNil(article);
      Inc(linesDeleted);
    end;

    if Assigned(article) then
      RawAddArticle(article);
  end;

  if linesDeleted > 1 then
  begin
    st := 'The file containing the main article header data is corrupted (articles.dat)!'#13#13 +
          'Account:'#9 + Owner.AccountName + #13 +
          'Group:'#9 + Name + #13 +
          'Posts:'#9 + IntTostr(linesDeleted) + ' out of ' + IntToStr(linesTotal) + ' are damaged!'#13#13 +
          'Delete the damaged lines?'#13 +
          '- Yes'#9'means the damaged lines will be removed.'#13 +
          '- No'#9'means an error will be raised which gives you'#13 +
          #9'the *opportunity* to close/terminate XanaNews and fix'#13 +
          #9'the problem by hand (or to make a backup first)';
    if MessageBox(0, PChar(st), PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2 or MB_ICONEXCLAMATION) <> IDYES then
    begin
      fArticlesLoaded := False;
      raise Exception.Create('Articles.dat file corrupted!');
    end;
  end;

  if linesDeleted > 0 then
  begin
    fFlagsDirty := True;
    st := 'Articles.dat file of Group: "' + Name +
      '" from Account "' + Owner.fAccountName + '" was damaged, ' +
      IntToStr(linesDeleted) + ' of ' + IntToStr(linesTotal) +
      ' lines/posts were deleted';
    LogMessage(st, True);
  end;

  NNTPAccounts.PerfCue(660, 'Added Article Headers');
  ReSortArticles;
  NNTPAccounts.PerfCue(880, 'Sorted articles');
  fArticlesLoaded := True;
end;

procedure TSubscribedGroup.CloseMessageFile;
begin
  inherited CloseMessageFile;
  FreeAndNil(fMessageFile);
end;

constructor TSubscribedGroup.Create(AOwner: TNNTPAccount; const AGroupName: string);
begin
  inherited Create(AGroupName, AOwner.FiltersCtnr, AOwner.DisplayFiltersCtnr);
  fOwner := AOwner;
  fNNTPSettings := TNNTPSettings.Create(fOwner.NNTPSettings);
  fPostingSettings := TPostingSettings.Create(fOwner.PostingSettings);
  fDisplaySettings := TDisplaySettings.Create(fOwner.DisplaySettings);
end;

function TSubscribedGroup.CreateGroupRegistry(access: DWORD): TExSettings;
begin
  Result := CreateExSettings;
  Result.Section := 'Accounts\' + FixFileNameString(fOwner.AccountName) + '\Subscribed Groups\' + FixFileNameString(Name);

  if not Result.Open((access and KEY_WRITE) = 0) then
    FreeAndNil(Result);
end;

destructor TSubscribedGroup.Destroy;
begin
  if Assigned(NNTPAccounts) then
    NNTPAccounts.fLastFoundGroup := nil;
  fNNTPSettings.Free;
  fPostingSettings.Free;
  fDisplaySettings.Free;
  inherited Destroy;
end;

function TSubscribedGroup.GetLastArticle: Int64;
var
  i, ct: Integer;
begin
  Result := 0;

  if not fArticlesLoaded then
  begin
    if fRawLastArticle = 0 then
    begin
      fUnreadArticleCount := -1;
      GetUnreadArticleCount;
    end;
    Result := fRawLastArticle;
  end
  else
  begin
    ct := fArticles.Count;
    for i := 0 to ct - 1 do
      if (articles[i].fArticleNo > Result) and (articles[i].fArticleNo <> 99999999) then
        Result := articles[i].fArticleNo;
    fRawLastArticle := Result;
  end;

  if fHighWaterMark > Result then
    Result := fHighWaterMark;
end;

procedure TSubscribedGroup.WriteSettings(reg: TExSettings);
var
  regCreated: Boolean;
begin
  regCreated := False;

  if reg = nil then
  begin
    reg := CreateGroupRegistry(KEY_READ or KEY_WRITE);
    regCreated := True;
  end;

  if Assigned(reg) then
  try
    FiltersCtnr.SaveFilters(reg, False);
    DisplayFiltersCtnr.SaveFilters(reg, True);
    NNTPSettings.WriteSettings(reg);
    PostingSettings.WriteSettings(reg);
    DisplaySettings.WriteSettings(reg);
    reg.SetStringValue('Identity', NNTPSettings.Identity.Name, Owner.NNTPSettings.Identity.Name);
    reg.SetStringValue('Nickname', fNickname, '');
  finally
    if regCreated then
      reg.Free;
  end;
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.LeaveGroup                                          |
 |                                                                      |
 | Another group has been selected in the UI.  Leave this group - save  |
 | it's articles, and release the memory for the messages.              |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.LeaveGroup(clearMessages: Boolean);
begin
  ClearThreadInfo;
  NNTPAccounts.PerfCue(880, 'Leaving group');

  if FlagsDirty then
    SaveArticles(False);
  NNTPAccounts.PerfCue(660, 'Saved Articles');
  fFocused := False;

  // Unload all messages...
  if clearMessages then
  begin
    MessageCacheController.Clear;
    UnloadArticles;
    NNTPAccounts.PerfCue(440, 'Unloaded articles');
  end;

  WriteSettings();

  ResetSortFlags;
end;

procedure TSubscribedGroup.LoadArticles;
var
  reader: TTextFileReader;
  fileName: string;
begin
  OpenMessageFile;
  if not fArticlesLoaded then
  begin
    LogMessage('Loading ' + name);
    LSSync.Enter;
    try
      fArticlesLoaded := True;
      fThreadSortDirection := DisplaySettings.ThreadSortDirection;
      fThreadSortOrder := DisplaySettings.ThreadSortOrder;
      fThreadOrder := DisplaySettings.ThreadOrder;
      fHideReadMessages := XNOptions.HideReadMessages;
      fHideMessagesNotToMe := False;
      fHideIgnoredMessages := XNOptions.HideIgnoredMessages;

      fileName := gMessageBaseRoot + '\' + FixFileNameString(Owner.AccountName) + '\' + FixFileNameString(Name) + '\articles.dat';
      if FileExists(fileName) then
      begin
        NNTPAccounts.PerfCue(440, 'Loading Article headers');
        reader := TTextFileReader.Create(fileName);
        try
          NNTPAccounts.PerfCue(550, 'Adding article headers');
          RawClearArticles;
          AddRawHeaders(reader);
        finally
          reader.Free;
        end;
      end;
    finally
      LSSync.Leave;
    end;
    if XNOptions.AutoCrosspostDetect then
      DontUnloadCache.MaxDepth := 3
    else
      DontUnloadCache.MaxDepth := 2;
    DontUnloadCache.Add(Self);
    NNTPAccounts.UnloadOldContainers(Self);
  end
  else
    SortArticles;
end;

procedure TSubscribedGroup.OpenMessageFile;
var
  msgFileName: string;
begin
  if not Assigned(fMessageFile) then
  begin
    msgFileName := gMessageBaseRoot + '\' + FixFileNameString(Owner.AccountName) + '\' + FixFileNameString(Name) + '\messages.dat';
    if FileExists(msgFileName) then
      fMessageFile := TFileStream.Create(msgFileName, fmOpenReadWrite or fmShareDenyWrite)
    else
    begin
      ForceDirectories(ExtractFilePath(msgFileName));
      fMessageFile := TFileStream.Create(msgFileName, fmCreate);
    end;
  end;
end;

procedure TSubscribedGroup.SaveArticles(recreateMessageFile: Boolean);
var
  fileName, newFileName, newMsgFileName, msgFileName: string;
  w: TTextFileWriter;
  i, j: Integer;
  article: Tarticle;
  st: string;
  raw: RawByteString;
  ms: TBufferedFileWriter;
  hasMessage, tmpMessage: Boolean;
  hLen: Word;
  accName: string;
  gName: string;
begin
  LSSync.Enter;
  try
    SaveArticleCounts;
    fUnreadArticleCount := -1;
    fUnloadedArticleCount := -1;
    fInterestingArticleCount := -1;
    fUnreadInterestingArticleCount := -1;

    accName := FixFileNameString(Owner.AccountName);
    gName := FixFileNameString(Name);

    newFileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\articles.new';
    fileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\articles.dat';
    newMsgFileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\messages.new';
    msgFileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\messages.dat';

    ms := nil;
    ForceDirectories(ExtractFilePath(msgFileName));
    w := TTextFileWriter.Create(newFileName);
    try
      if recreateMessageFile then
      begin
        OpenMessageFile;
        ms := TBufferedFileWriter.Create(newMsgFileName);
      end;

      for i := 0 to LoadGetArticleCount - 1 do
      begin
        article := TArticle(fArticles[i]);

        if article.ArticleNo <> 0 then
        begin
          if recreateMessageFile then
          begin
            tmpMessage := False;
            if not Assigned(article.fMsg) and (article.fMessageOffset <> -1) then
              tmpMessage := True;

              // nb. use article.Msg, not article.fMsg to force loading of temp message.
              //
              // Also, must test fMessageOffset <> -1.  We don't want to save downloading messages.
            hasMessage := assigned(article.Msg) and (article.fMessageOffset <> -1) and (article.fMsg.RawData.Size > 0);
          end
          else
          begin
            tmpMessage := False;
            hasMessage := article.fMessageOffset <> -1;
          end;

          if recreateMessageFile and hasMessage then
            article.fMessageOffset := ms.Position;

          st := Format('%d'#9'%s'#9'%s'#9'%s'#9'%s'#9'%s'#9'%d'#9'%d'#9'%d', [
            article.fArticleNo,
            article.fSubject,
            article.fFrom,
            RawByteString(SafeDateTimeToInternetStr(article.fDate, True)),
            article.fMessageID,
            article.fReferences,
            article.Bytes,
            article.fLines,
            article.fFlags
            ]);

          if hasMessage then // Add extra 'message offset' field to header string
            if recreateMessageFile then
              st := st + #9 + IntToStr(ms.Position)
            else
              st := st + #9 + IntToStr(article.fMessageOffset);

          w.WriteLn(st);

          if hasMessage and recreateMessageFile then
          begin
            raw := RawByteString('X-Msg:' + IntToHex(article.fMsg.RawData.Size, 8));
            ms.Write(raw[1], Length(raw));

            for j := 0 to article.fMsg.Header.Count - 1 do
            begin
              raw := article.fMsg.Header[j];
              hLen := Length(raw);
              ms.Write(hLen, SizeOf(hLen));
              ms.Write(raw[1], Length(raw));
            end;

            hLen := 0;
            ms.Write(hLen, SizeOf(hLen));

            article.fMsg.RawData.Seek(0, soBeginning);
            ms.Write(article.fMsg.RawData.Memory^, article.fMsg.RawData.Size);
          end;

          if tmpMessage then
            FreeAndNil(article.fMsg);
        end;
      end;

    finally
      w.Free;
      ms.Free;
      CloseMessageFile;

      ForceRenameFile(newFileName, fileName);

      if recreateMessageFile then
        ForceRenameFile(newMsgFileName, msgFileName);
    end;
    fFlagsDirty := False;
  finally
    LSSync.Leave;
  end;
end;

function TSubscribedGroup.GetMessagebaseSize: Int64;
var
  fileName: string;
  f: TFileStream;
begin
  OpenMessageFile;
  Result := fMessageFile.Size;
  fileName := gMessageBaseRoot + '\' + FixFileNameString(Owner.AccountName) + '\' + FixFileNameString(Name) + '\articles.dat';
  LSSync.Enter;
  try
    if FileExists(fileName) then
    begin
      f := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
      try
        Inc(Result, f.Size);
      finally
        f.Free;
      end;
    end;
  finally
    LSSync.Leave;
  end;
end;

procedure TSubscribedGroup.SetSecret(const Value: Boolean);
begin
  inherited;
  fOwner.fSecretGroupCount := -1;
end;

function TSubscribedGroup.GetHighestArticleNo: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to LoadGetArticleCount - 1 do
    if (articles[i].fArticleNo <> 99999999) and (articles[i].fArticleNo > Result) then
      Result := articles[i].fArticleNo;
end;

procedure TSubscribedGroup.SetSortIdx(const Value: Integer);
begin
  Owner.SortGroupsByName := False;
  Owner.ChangeGroupSortIdx(Self, Value);
end;

procedure TSubscribedGroup.SaveArticleCounts(reg: TExSettings);
var
  NeedsRegistry: Boolean;
begin
  NeedsRegistry := reg = nil;
  try
    if NeedsRegistry then
      reg := CreateGroupRegistry(KEY_READ or KEY_WRITE);

    if reg = nil then Exit;

    reg.SetInteger64Value('Last Article', HighWaterMark, 0);
    if ArticleCount > 0 then
    begin
      reg.SetIntegerValue('Unloaded Article Count', ArticleCount, 0);
      reg.SetIntegerValue('Unread Article Count', UnreadArticleCount, 0);
      reg.SetIntegerValue('Unread Article To Me Count', UnreadArticleToMeCount, 0);
      reg.SetIntegerValue('Unread Xananews Article Count', UnreadXananewsArticleCount, 0);
      reg.SetIntegerValue('Unread Reply Count', UnreadReplyCount, 0);
      reg.SetIntegerValue('Interesting Article Count', InterestingArticleCount, 0);
      reg.SetIntegerValue('Unread Interesting Article Count', UnreadInterestingArticleCount, 0);
    end
    else
    begin
      reg.DeleteValue('Unloaded Article Count');
      reg.DeleteValue('Unread Article Count');
      reg.DeleteValue('Unread Article To Me Count');
      reg.DeleteValue('Unread Xananews Article Count');
      reg.DeleteValue('Unread Reply Count');
      reg.DeleteValue('Interesting Article Count');
      reg.DeleteValue('Unread Interesting Article Count');
    end;
  finally
    if NeedsRegistry then
      reg.Free;
  end;
end;

procedure TSubscribedGroup.LoadArticleCounts(reg: TExSettings);
begin
  fUnloadedArticleCount := reg.GetIntegerValue('Unloaded Article Count', -1);
  fAdjustedArticleCount := fUnloadedArticleCount;
  if fUnloadedArticleCount <> -1 then
  begin
    fUnreadArticleToMeCount := reg.GetIntegerValue('Unread Article To Me Count', 0);
    fUnreadArticleCount := reg.GetIntegerValue('Unread Article Count', 0);
    fUnreadXananewsArticleCount := reg.GetIntegerValue('Unread Xananews Article Count', 0);
    fUnreadReplyCount := reg.GetIntegerValue('Unread Reply Count', 0);
    fInterestingArticleCount := reg.GetIntegerValue('Interesting Article Count', 0);
    fUnreadInterestingArticleCount := reg.GetIntegerValue('Unread Interesting Article Count', 0);
  end;
end;

procedure TSubscribedGroup.LoadUnreadArticleCount;
begin
  fUnreadArticleCount := -1;
  GetUnreadArticleCount;
end;

function TSubscribedGroup.TSGetLastArticle: Int64;
begin
  LSSync.Enter;
  try
    if Loaded then
      Result := LastArticle
    else
    begin
      QuickLoadHeaderStats;
      Result := fRawLastArticle;
    end;
  finally
    LSSync.Leave;
  end;
  if fHighWaterMark > Result then
    Result := fHighWaterMark;
end;

procedure TSubscribedGroup.UnloadArticles;
begin
  if Loaded then
  begin
    LSSync.Enter;
    try
      inherited UnloadArticles;
    finally
      LSSync.Leave;
    end;
  end;
end;

{ TArticle }

procedure TArticle.ChangeArticleNo(newArticleNo: Int64);
begin
  fArticleNo := newArticleNo;
end;

procedure TArticle.FixArticleNo;
var
  xref, st: string;
  p: Integer;
begin
  xref := Header['Xref'];
  if xref <> '' then
  begin
    st := ' ' + SubscribedGroup.Name + ':';
    p := Pos(st, xref);
    if p > 0 then
    begin
      st := Copy(xref, p + Length(st), MaxInt);
      p := Pos(' ', st);
      if p > 0 then
        st := Copy(st, 1, p - 1);
      fArticleNo := StrToInt64Def(st, 0);
    end
    else
      fArticleNo := 99999999;
  end;
end;

constructor TArticle.Create(AOwner: TArticleContainer);
begin
  inherited Create(AOwner);
  fCodePage := -1;
end;

destructor TArticle.Destroy;
begin
  MessageCacheController.AlwaysRemove := True;
  try
    MessageCacheController.Remove(Self);
  finally
    MessageCacheController.AlwaysRemove := False;
  end;
  FreeAndNil(fMsg);
  inherited Destroy;
end;

function TArticle.GetAccount: TNNTPAccount;
begin
  Result := SubscribedGroup.Owner;
end;

function TArticle.GetCodePage: Integer;
begin
  Result := GetCodePageFromFile;
end;

function TArticle.GetHasKeyPhrase(idx: Integer): Boolean;
begin
  GetKeyPhraseNo;
  Result := (fFlags and (fgKeyPhrase0 shl idx)) <> 0;
end;

(*----------------------------------------------------------------------*
 | TArticleBase.GetInterestingMessageLine                               |
 |                                                                      |
 | Get the first intersting line of the message, so that the message    |
 | subject doesn't just say 're:something', 'fred wrote:' or contain a  |
 | quite from the previous message.                                     |
 |                                                                      |
 | This has some limitations.  Because of the fixed max length on       |
 | lines.  Maybe provide an option to turn this off, though it seems to |
 | work well enough for most messages.                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   None                                                               |
 |                                                                      |
 | The function returns string                                          |
 *----------------------------------------------------------------------*)
function TArticle.GetInterestingMessageLine: string;
var
  i, len: Integer;
  hLen: word;
  st: string;
  raw: RawByteString;
  r: TAnsiStrings;
  m: TMemoryStream;
  rCreated: Boolean;
begin
  if fInterestingMessageLine <> '' then
  begin
    Result := fInterestingMessageLine;
    Exit;
  end;

  rCreated := False;
  r := nil;
  m := nil;
  try
    if Assigned(fMsg) then
      r := fMsg.TextPart
    else
      if (fMessageOffset <> -1) and Assigned(Owner.fMessageFile) then
      begin               // Load (some of) the message from 'messages.dat'
                          // I did this to prevent huge messages being loaded/
                          // decoded when simply displaying the headers for non-
                          // selected messages.

        Owner.fMessageFile.Seek(fMessageOffset, soBeginning);

        SetLength(raw, 5);         // Read XMsg: prefix
        Owner.fMessageFile.Read(raw[1], 5);

        if raw = 'X-Msg' then      // New style - contains extra header lines
        begin
          SetLength(raw, 9);
          Owner.fMessageFile.Read(raw[1], 9);
          raw[1] := '$';
          len := RawStrToInt(raw);

          Owner.fMessageFile.Read(hLen, SizeOf(hLen));

          while hLen > 0 do
          begin
            SetLength(raw, hLen);
            Owner.fMessageFile.Read(raw[1], hLen);
            Owner.fMessageFile.Read(hLen, SizeOf(hLen));
          end;

          r := TAnsiStringList.Create;
          rCreated := True;
          m := TMemoryStream.Create;
          if len > 2048 then len := 2048;
          m.CopyFrom(Owner.fMessageFile, len);
          m.Seek(0, soBeginning);
          r.LoadFromStream(m);
        end;
      end;

    st := '';
    if Assigned(r) then
    begin
      i := 0;
      while i < r.Count do
      begin
        st := AnsiStringToWideString(r[i], CodePage);

        if st <> '' then
        begin
          if AnsiContainsText(st, 'in article') or AnsiContainsText(st, 'wrote in message') or
             AnsiContainsText(st, 'wrote:') or AnsiContainsText(st, 'said:') then
          begin
            st := '';
            repeat
              Inc(i);
            until (i >= r.Count) or (Copy(r[i], 1, 1) = '>');

            repeat
              Inc(i);
            until (i >= r.Count) or (r[i] = '');

          end
          else
            if st[1] = '>' then
            begin
              st := '';
              repeat
                Inc(i)
              until (i >= r.Count) or (r[i] = '');
            end
            else
              if Copy(st, 1, 2) = '<<' then
              begin
                st := '';
                repeat
                  Inc(i)
                until (i >= r.Count) or AnsiContainsText(string(r[i]), '>>');
              end;
        end
        else
          Inc(i);

        if st <> '' then
        begin
          fInterestingMessageLine := TrimEx(st);
          Break;
        end;
      end;
    end;
  finally
    m.Free;
    if rCreated then
      r.Free;
  end;

  st := Trim(st);
  if (st = '') or (st = '-- ') then
    Result := Subject
  else
    Result := st;
end;

function TArticle.MatchesKeyPhrase(st: string; searcher: TStringSearcher): Boolean;
var
  thing: Integer;
  st1: string;
  tp: TAnsiStrings;
begin
  thing := 0;

  if Pos(':', st) > 0 then
    if AnsiStartsText('Subject:', st) then
      thing := 1
    else
      if AnsiStartsText('Author:', st) then
        thing := 2
      else
        if AnsiStartsText('From:', st) then
          thing := 3
        else
          thing := 4;

  if thing > 0 then
  begin
    st1 := SplitString(':', st);
    case thing of
      1: st1 := Subject;
      2: st1 := FromName;
      3: st1 := From;
      4: begin
            st1 := GetHeader(st1);
            if st1 = '' then
              thing := -1;
          end;
    end;
  end
  else
  begin
    if not Assigned(Msg) then
      thing := -1
    else
    begin
      tp := Msg.TextPart;
      if Assigned(tp) then
        st1 := string(tp.Text)
      else
        thing := -1;
    end;
  end;

  if thing >= 0 then
  begin
    searcher.Parse(st);
    Result := searcher.Matches(st1);
  end
  else
    Result := False;
end;

function TArticle.GetKeyPhraseNo: Integer;
var
  i: Integer;
begin
  if (fFlags and fgScannedKeyPhrases) <> 0 then
    if (fFlags and fgKeyphraseMask) <> 0 then
    begin
      Result := 0;
      i := fgKeyPhrase0;
      while (fFlags and i) = 0 do
      begin
        i := i shl 1;
        Inc(Result);
      end;
    end
    else
      Result := -1
  else
    if TSubscribedGroup(Owner).Owner.ScanKeyPhrases and HasMsg then
    begin
      Result := -1;

      for i := 0 to 7 do
      begin
        if XNOptions.KeyPhrase[i] <> '' then
        begin
          if MatchesKeyPhrase(XNOptions.KeyPhrase[i], NNTPAccounts.KeyPhraseSearcher[i]) then
          begin
            if Result = -1 then
              Result := i;
            fFlags := fFlags or (fgKeyPhrase0 shl i);
          end;
        end;
      end;
      fFlags := fFlags or fgScannedKeyPhrases;
      Owner.fFlagsDirty := True;
    end
    else
      Result := -1;
end;

function TArticle.GetMsg: TmvMessage;
begin
  Result := GetMsgFromFile;
end;

function TArticle.GetPostingHost: string;
const
  snntp = 'NNTP-Posting-Host';
  sxtrc = 'X-Trace';
  sphst = 'X-Posting-Host';
begin
  if ((fFlags and (fgSpamNoPostingHost or fgSpamNoXTrace)) = 0) and (fPostingHost = '') then
  begin
    fPostingHost := Header[snntp];
    if fPostingHost = '' then
    begin
      fFlags := fFlags or fgSpamNoPostingHost;
      fPostingHost := Header[sxtrc];
      if fPostingHost = '' then
        fFlags := fFlags or fgSpamNoXTrace;
      if fPostingHost = '' then
        fPostingHost := Header[sphst];
    end;
  end;
  Result := fPostingHost;
end;

function TArticle.GetSubscribedGroup: TSubscribedGroup;
begin
  Result := TSubscribedGroup(Owner);
end;

function TArticle.PeekAtMsgHdr(const hdr: string): string;
begin
  Result := string(PeekAtMsgHDrFromFile(RawByteString(hdr)));
end;

procedure TArticle.RemoveMessage;
begin
  FreeAndNil(fMsg);
  fMessageOffset := -1;
end;

(*----------------------------------------------------------------------*
 | TArticle.SaveMessageBody                                             |
 |                                                                      |
 | Save a message's body in the 'messages.dat' file for the group.      |
 |                                                                      |
 | The format is:                                                       |
 |                                                                      |
 |   'X-Msg:'    6 char header                                          |
 |   xxxxxxxx    8 char hex string containg message length              |
 |   nn          word length of first extra header                      |
 |   char (nn)   nn char first extra header string                      |
 |   nn          word length of second extra header string              |
 |   char (nn)   nn char second extra header string                     |
 |   ...                                                                |
 |   nn          word 0                                                 |
 |               Then follows the message - length xxxxxxxx             |
 *----------------------------------------------------------------------*)
procedure TArticle.SaveMessageBody;
var
  st: RawByteString;
  i: Integer;
  hLen: Word;
begin
  fOwner.OpenMessageFile;
  fOwner.fMessageFile.Seek(0, soEnd);
  fMessageOffset := fOwner.fMessageFile.Position;
  st := RawByteString('X-Msg:' + IntToHex(fMsg.RawData.Size, 8));
  fOwner.fMessageFile.Write(st[1], Length(st));

  for i := 0 to fMsg.Header.Count - 1 do
  begin
    st := fMsg.Header[i];
    hLen := Length(st);
    fOwner.fMessageFile.Write(hLen, SizeOf(hLen));
    fOwner.fMessageFile.Write(st[1], Length(st));
  end;

  hLen := 0;
  fOwner.fMessageFile.Write(hLen, SizeOf(hLen));

  fMsg.RawData.Seek(0, soBeginning);
  fMsg.RawData.SaveToStream(fOwner.fMessageFile);

  // Signal that the articles.dat file needs to be updated.
  fOwner.fFlagsDirty := True;
  fFlags := fFlags and not fgScannedAttachment;
end;

procedure TArticle.SetCodePage(const Value: Integer);
begin
  inherited SetCodePage(Value);
  if Assigned(fMsg) then
    fMsg.Codepage := Value;
end;

procedure TSubscribedGroup.GroupArticles;
var
  i: Integer;
  art, root, ap: TArticle;
  subject, lastSubject: RawByteString;
  filters: TFiltersCtnr;

  procedure CheckCompleteMultipart(root: TArticle);
  var
    c: Integer;
    p: TArticle;
  begin
    if root.fPartCount <= 1 then
      root.fMultipartFlags := mfNotMultipart
    else
    begin
      root.fMultipartFlags := mfCompleteMultipart;
      if root.fPartNo = 1 then
      begin
        p := TArticle(root.fChild);
        c := 2;
        while (p <> nil) and (p.fPartNo <= c) do
        begin
          if p.fPartNo = c then
            Inc(c);
          p := TArticle(p.fSibling);
        end;

        if (p <> nil) or (c - 1 <> root.fPartCount) then
          root.fMultipartFlags := mfPartialMultipart;
      end
      else
        root.fMultipartFlags := mfPartialMultipart;
    end;
  end;

begin { GroupArticles }
  fThreads.Clear;
  fArticles.Sort(CompareThreads);

  i := 0;
  root := nil;
  lastSubject := '';

  filters := DisplayFiltersCtnr;

  while i < ArticleCount do
  begin
    art := TArticle(fArticles[i]);
    art.fSibling := nil;

    if (HideMessagesNotToMe and not art.IsMine) or (hideReadMessages and art.IsRead) or (hideIgnoredMessages and art.IsIgnore) or (Assigned(filters) and filters.HasFilters and filters.BlockArticle(art)) then
    begin
      Inc(i);
      Continue;
    end;

    subject := art.DecodedMultipartSubject;
    if art.fIsMultiPart then
    begin
      art.fSibling := nil;
      art.fChild := nil;
      if (subject <> lastSubject) or (root = nil) then
      begin
        if Assigned(root) then
          CheckCompleteMultipart(root);
        lastSubject := subject;
        root := art;
        fThreads.Add(art);
      end
      else
        if root.Child <> nil then
        begin
          ap := TArticle(root.Child);
          while ap.Sibling <> nil do
            ap := TArticle(ap.Sibling);
          ap.fSibling := art;
        end
        else
          root.fChild := art;
    end
    else
    begin
      art.fSibling := nil;
      art.fChild := nil;
      if Assigned(root) then
        CheckCompleteMultipart(root);
      root := nil;
      fThreads.Add(art);
    end;

    Inc(i);
  end;

  if root <> nil then
    CheckCompleteMultipart(root);
end;

function TSubscribedGroup.GetLowestArticleNo: Int64;
var
  i: Integer;
begin
  Result := High(Int64);

  for i := 0 to LoadGetArticleCount - 1 do
    if (articles[i].fArticleNo > 0) and (articles[i].fArticleNo < Result) then
      Result := articles[i].fArticleNo;

  if Result = High(Int64) then
    Result := 0;
end;

function TSubscribedGroup.GetNext: TArticleContainer;
var
  i, idx: Integer;
begin
  Result := nil;
  if Owner.fSecretGroupCount > 0 then
  begin
    idx := -1;

    for i := 0 to Owner.SubscribedGroupCount - 1 do
      if Owner.SubscribedGroups[i].Name = Name then
      begin
        idx := i;
        Break;
      end;
  end
  else
    idx := Owner.fSubscribedGroups.IndexOf(Name);
  if idx >= 0 then
  begin
    Inc(idx);
    if idx < Owner.SubscribedGroupCount then
      Result := Owner.SubscribedGroups[idx];
  end;
end;

function TSubscribedGroup.GetArticle(idx: Integer): TArticle;
begin
  Result := TArticle(ArticleBase[idx]);
end;

function TSubscribedGroup.MarkOnLeave: Boolean;
begin
  Result := Owner.MarkOnLeave;
end;

function TSubscribedGroup.GetServerSettings: TServerSettings;
begin
  Result := Owner.fNNTPServerSettings;
end;

function TSubscribedGroup.GetIdentity: TIdentity;
begin
  Result := NNTPSettings.Identity;
end;

function TSubscribedGroup.GetUnreadArticleCount: Integer;
var
  i: Integer;
  l: Int64;
  art: TArticleBase;
begin
  Result := 0;

  if fUnreadArticleCount = -1 then
  begin
    fUnreadArticleToMeCount := 0;
    fUnreadXananewsArticleCount := 0;
    fUnloadedArticleCount := 0;
    fUnreadReplyCount := 0;
    fInterestingArticleCount := 0;
    fUnreadInterestingArticleCount := 0;
    fAdjustedArticleCount := 0;
    fRawLastArticle := 0;
    if Loaded then              // Check each article
    begin
      fUnloadedArticleCount := ArticleCount;
      for i := 0 to ArticleCount - 1 do
      begin
        art := ArticleBase[i];
        if not art.IsRead then
        begin
          Inc(Result);
          if art.IsMine then
            Inc(fUnreadArticleToMeCount);
          if art.IsReply then
            Inc(fUnreadReplyCount);
          if art.IsXanaNews then
            Inc(fUnreadXananewsArticleCount);
        end;
        if art.IsInteresting then
        begin
          Inc(fInterestingArticleCount);
          if not art.IsRead then
            Inc(fUnreadInterestingArticleCount);
        end;
        l := art.ArticleNo;
        if l <> 0 then
          Inc(fAdjustedArticleCount);
        if (l > fRawLastArticle) and (l <> 99999999) then
          fRawLastArticle := l;
      end;
      fUnreadArticleCount := Result;
    end
    else
    begin
      LSSync.Enter;
      try
        QuickLoadHeaderStats;
      finally
        LSSync.Leave;
      end;
    end;

    fAdjustedArticleCount := fUnloadedArticleCount;
  end
  else
    Result := fUnreadArticleCount;
end;

procedure TSubscribedGroup.QuickLoadHeaderStats;
var
  st, fileName: string;
  pc, pc1: PChar;
  reader: TTextFileReader;
  n, l, fgs: Integer;
  raw: RawByteString;
begin
  // Already LSSync-ed when this is called
  fileName := gMessageBaseRoot + '\' + FixFileNameString(Owner.AccountName) + '\' + FixFileNameString(Name) + '\articles.dat';
  if FileExists(fileName) then
    reader := TTextFileReader.Create(fileName)
  else
    reader := nil;
  fUnreadArticleCount := 0;
  if Assigned(reader) then
  try
    fUnloadedArticleCount := 0;
    fRawLastArticle := 0;
    while reader.ReadLn(raw) do
    try
      st := string(raw);
      Inc(fUnloadedArticleCount);
      n := 0;
      pc := PChar(st);
      l := 0;
      while n < 8 do
      begin
        pc1 := pc;
        pc := StrScan(pc, #9);

        if pc <> nil then
        begin
          if n = 0 then
          begin
            pc^ := #0;
            l := StrToIntDef(pc1, 0);
          end;

          Inc(pc);
          Inc(n);
        end
        else
          Break;
      end;

      if (l > fRawLastArticle) and (l <> 99999999) then
        fRawLastArticle := l;

      if Assigned(pc) then
      begin
        pc1 := StrScan(pc, #9);
        if Assigned(pc1) then
          pc1^ := #0;
      end;

      if Assigned(pc) then
        fgs := IndyStrToInt(pc)
      else
        fgs := 0;

      if (fgs and fgRead) = 0 then
      begin
        Inc(fUnreadArticleCount);
        if (fgs and fgMine) <> 0 then
          Inc(fUnreadArticleToMeCount);
        if (fgs and fgXanaNews) <> 0 then
          Inc(fUnreadXananewsArticleCount);
        if (fgs and fgReply) <> 0 then
          Inc(fUnreadReplyCount);
      end;
      if (fgs and fgInteresting) <> 0 then
      begin
        Inc(fInterestingArticleCount);
        if (fgs and fgRead) = 0 then
          Inc(fUnreadInterestingArticleCount);
      end;
    except
    end;
  finally
    reader.Free;
  end;
end;

function TSubscribedGroup.GetPostingSettings: TPostingSettings;
begin
  Result := fPostingSettings;
end;

function TSubscribedGroup.GetDisplaySettings: TDisplaySettings;
begin
  Result := fDisplaySettings;
end;

procedure TArticle.SetCrossPostsFlag(flag: DWORD; value: Boolean);
var
  xref, st, group: string;
  artNo: Int64;
  xCtnr: TArticleContainer;
  art: TArticleBase;
begin
  xref := Header['Xref'];
  st := SplitString(' ', xref);        // First entry *may* be the server name
                                        // if so, discard it.

  if Pos(':', st) = 0 then             // Get the first XRef
    st := SplitString(' ', xref);

  while st <> '' do
  begin
    group := SplitString(':', st);     // Split XRef into group & article no
    artNo := StrToInt64Def(st, 0);

    if artNo = 0 then Break;
                                        // Check group isn't *this* group
    if not SameText(group, SubscribedGroup.Name) then
    begin
                                        // Find the subscribed group - in this account!
      xCtnr := nntpAccounts.FindArticleContainer(Account.AccountName, group);

      if Assigned(xCtnr) then
      begin
        art := xCtnr.FindArticleNo(artNo);
        if Assigned(art) then
        begin
          art.SetFlag(flag, value);
          art.Owner.fUnreadArticleCount := -1;
          art.Owner.fUnloadedArticleCount := -1;
        end;
      end;
    end;
    st := SplitString(' ', xref);  // Get the server
  end;
end;


procedure TArticle.SetIsRead(const Value: Boolean);
begin
  inherited;
  if XNOptions.AutoCrosspostDetect then
    SetCrossPostsFlag(fgRead, Value);
end;

function TArticle.GetHeader(const name: string): string;
var
  s, t, h: string;
begin
  Result := inherited GetHeader(name);
  if (Result = '') and (fTempExtraHeaders <> '') then
  begin
    t := fTempExtraHeaders;
    while t <> '' do
    begin
      s := Fetch(t, #9);
      h := Fetch(s, ':');
      if SameText(name, h) then
      begin
        Result := s;
        Break;
      end;
    end;
  end;
end;

function TArticle.GetHasAnyKeyPhrase: Boolean;
begin
  Result := GetKeyPhraseNo <> -1;
end;

procedure TArticle.RawMarkAsRead;
begin
  SetFlag(fgRead, True);
  Owner.fUnreadArticleCount := -1;
  Owner.fUnloadedArticleCount := -1;
end;

{ TFiltersCtnr }

procedure TFiltersCtnr.AssignFilters(ctnr: TFiltersCtnr);
var
  i: Integer;
begin
  if Assigned(ctnr.fFilters) then
    if Assigned(fFilters) then
      fFilters.Clear
    else
      fFilters := TNNTPFilters.Create(False)
  else
    if Assigned(fFilters) then
      FreeAndNil(fFilters);

  if Assigned(ctnr.fFilters) then
    for i := 0 to ctnr.fFilters.Count - 1 do
      fFilters.AddObject(ctnr.Filters.Strings[i], ctnr.Filters.Objects[i]);

  fParent := ctnr.fParent;
end;

function TFiltersCtnr.BlockArticle(article: TArticleBase): Boolean;
var
  n: TFiltersCtnr;
  i: Integer;
  filter: TNNTPFilter;
begin
  n := Self;
  Result := False;
  // Clear filter tags.
  for i := 0 to AllFilters.Count - 1 do
    TNNTPFilter(AllFilters[i]).Tag := 0;

  if Assigned(n.Filters) then
    for i := 0 to n.Filters.Count - 1 do
      n.Filters[i].Tag := 0;

  while not Result and (n <> nil) do
  begin
    if Assigned(n.Filters) then
      for i := 0 to n.Filters.Count - 1 do
      begin
        filter := n.Filters[i];
        if filter.Tag = 0 then
        begin
          filter.Tag := 1;      // Set tag on the filter.  This indicates
                                // that this filter has been processed
          if FilterEnabled(filter) and filter.Matches(article) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;

    if not Result then
      n := n.fParent;
  end;
end;

procedure TFiltersCtnr.Clear;
begin
  FreeAndNil(fFilters);
end;

constructor TFiltersCtnr.Create(AOwner: TObject; AParent: TFiltersCtnr);
begin
  fOwner := AOwner;
  fParent := AParent
end;

destructor TFiltersCtnr.Destroy;
begin
  fFilters.Free;
  inherited Destroy;
end;

procedure TFiltersCtnr.EnableFilter(filter: TNNTPFilter; enable: Boolean);
var
  i: Integer;
begin
  if not Assigned(filter) then Exit;
  if enable <> FilterEnabled(filter) then
  begin
    if Assigned(fFilters) then
      i := fFilters.IndexOfFilter(filter)
    else
      i := -1;

    if (i = -1) and not Assigned(fFilters) then
      fFilters := TNNTPFilters.Create(False);

    case enable of
      True:                                    // Enabling...
        if i <> -1 then
          fFilters.Delete(i)                   // Existing filter, - so it must say 'off'.  Delete it.
        else
          fFilters.AddObject('', filter);       // No existing filter.  Add one.

      False:                                    // Disabling...
        if i <> -1 then
          fFilters.Delete(i)                    // Existing filter.  Delete it.
        else
          fFilters.AddObject('off', filter);    // No existing filter so it must be enabled on a higher
                                                // level.  Add an 'off' at this level.
    end;

    if fFilters.Count = 0 then
      FreeAndNil(fFilters);
  end;
end;

function TFiltersCtnr.FilterEnabled(filter: TNNTPFilter): Boolean;
var
  n: TFiltersCtnr;
  i: Integer;
begin
  n := Self;
  Result := False;
  if filter = nil then
    Exit;

  while n <> nil do
  begin
    if Assigned(n.Filters) then
      i := n.Filters.IndexOfFilter(filter)
    else
      i := -1;
    if i <> -1 then
    begin
      if filter.Dormant then
        n.fFilters.Delete(i)
      else
      begin
        Result := n.Filters.Strings[i] <> 'off';
        Break;
      end;
    end
    else
      n := n.fParent;
  end;
end;

function TFiltersCtnr.GetCount: Integer;
begin
  if Assigned(fFilters) then
    Result := fFilters.Count
  else
    Result := 0;
end;

function TFiltersCtnr.GetHasFilters: Boolean;
var
  n: TFiltersCtnr;
begin
  n := Self;
  Result := False;
  while (not Result) and Assigned(n) do
  begin
    Result := n.Count > 0;
    n := n.Parent;
  end
end;

procedure TFiltersCtnr.LoadFilters(reg: TExSettings; displayFilters: Boolean);
var
  reg1: TExSettings;
  valueNames: TStrings;
  i: Integer;
  nm: string;
  filter: TNNTPFilter;
  keyname: string;
begin
  FreeAndNil(fFilters);
  valueNames := nil;

  if displayFilters then
    keyname := 'Display Filters'
  else
    keyName := 'Active Filters';

  if reg.HasSection(keyName) then
  begin

    reg1 := CreateChildSettings(reg, keyName);
    try
      valueNames := TStringList.Create;


      reg1.GetValueNames(valueNames);

      if valueNames.Count > 0 then
      begin
        fFilters := TNNTPFilters.Create(False);

        for i := 0 to valueNames.Count - 1 do
        begin
          nm := valueNames[i];
          filter := AllFilters.FindFilter(nm);
          if filter = nil then
            reg1.DeleteValue(nm)
          else
            fFilters.AddObject(reg1.StringValue[nm], filter);
        end;
      end;

    finally
      valueNames.Free;
      reg1.Free;
    end;
  end;
end;

procedure TFiltersCtnr.SaveFilters(reg: TExSettings; displayFilters: Boolean);
var
  reg1: TExSettings;
  valueNames: TStrings;
  i, idx: Integer;
  filter: TNNTPFilter;
  keyName: string;
begin
  valueNames := nil;
  if displayFilters then
    keyName := 'Display Filters'
  else
    keyName := 'Active Filters';
  reg1 := CreateChildSettings(reg, keyName);
  try
    valueNames := TStringList.Create;

    i := 0;
    while i < Count do
      if fFilters[i].Dormant then
        fFilters.Delete(i)
      else
        Inc(i);

    if Count = 0 then
      reg1.DeleteSection(keyName)
    else
    begin
      reg1.GetValueNames(valueNames);

      for i := 0 to fFilters.Count - 1 do
      begin
        filter := fFilters[i];
        if filter.Temporary then
          Continue;

        idx := valueNames.IndexOf(filter.Name);
        if idx = -1 then
          reg1.SetStringValue(filter.Name, fFilters.Strings[i], '~')
        else
          valueNames.Delete(idx);
      end;

      for i := 0 to valueNames.Count - 1 do
        reg1.DeleteValue(valueNames[i]);
    end;

  finally
    reg1.Free;
    valueNames.Free;
  end;
end;

{ TArticleStack }

procedure TArticleStack.Clear;
var
  oldCapacity: Integer;
begin
  oldCapacity := fList.Capacity;
  fList.Clear;
  fList.Capacity := oldCapacity;
end;

constructor TArticleStack.Create(ACapacity: Integer);
begin
  fList := TStringList.Create;
  fList.Duplicates := dupIgnore;
  fList.Capacity := ACapacity;
end;

destructor TArticleStack.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TArticleStack.GetCapacity: Integer;
begin
  Result := fList.Capacity;
end;

function TArticleStack.GetIsEmpty: Boolean;
begin
  Result := fList.Count = 0;
end;

function TArticleStack.Peek: TArticleBase;
var
  MsgID, group, account: string;
begin
  if fList.Count > 0 then
  begin
    account := fList[0];
    MsgID := SplitString('=', account);
    group := SplitString('=', account);

    Result := NNTPAccounts.FindMsgID(account, group, RawByteString(MsgID));
  end
  else
    Result := nil;
end;

function TArticleStack.Pop: TArticleBase;
begin
  Result := Peek;
  if fList.Count > 0 then
    fList.Delete(0);
end;

procedure TArticleStack.Push(article: TArticleBase);
var
  accName, st: string;
begin
  if article = nil then
    Exit;

  if fList.Count = fList.Capacity then
    fList.Delete(fList.Count - 1);

  accName := '';

  try
    if article is TArticle then
      accName := TArticle(article).Account.AccountName
    else
      if article is TFolderArticle then
        accName := cFolders;

    if accName <> '' then
    begin
      st := string(article.MessageId) + '=' + article.Owner.Name + '=' + accName;
      if fList.IndexOf(st) = -1 then
        fList.Insert(0, st);
    end;
  except
  end;
end;

{ TArticleContainer }

procedure TArticleContainer.BeginLock;
begin
  Inc(fLockCount);
end;

procedure TArticleContainer.ClearThreadInfo;
var
  i: Integer;
  article: TArticleBase;
begin
  if fThreads.Count > 0 then
    fThreads.Clear;
  i := 0;
  while i < LoadGetArticleCount do
  begin
    article := ArticleBase[i];
    if article.ArticleNo = 0 then
      RawDeleteArticle(i)
    else
    begin
      article.fChild := nil;
      article.fParent := nil;
      article.fSibling := nil;
      Inc(i);
    end;
  end;
end;

procedure TArticleContainer.CloseMessageFile;
begin
end;

constructor TArticleContainer.Create(const AName: string; AFiltersParent, ADisplayFiltersParent: TFiltersCtnr);
begin
  inherited Create(AName);
  fUnreadArticleCount := -1;
  fUnloadedArticleCount := -1;
  fFiltersCtnr := TFiltersCtnr.Create(Self, AFiltersParent);
  fDisplayFiltersCtnr := TFiltersCtnr.Create(Self, ADisplayFiltersParent);
  fThreads := TList.Create;
  fSortBuf := TList.Create;
  fThreadSortOrder := soMessageNo;
end;

procedure TArticle.SetMsg(const Value: TmvMessage);
begin
  fMsg := Value;
end;

destructor TArticleContainer.Destroy;
begin
  fFiltersCtnr.Free;
  fDisplayFiltersCtnr.Free;
  fThreads.Free;
  fSortBuf.Free;
  CloseMessageFile;
  DontUnloadCache.AlwaysRemove := True;
  try
    DontUnloadCache.Remove(Self);
  finally
    DontUnloadCache.AlwaysRemove := False;
  end;
  inherited Destroy;
end;

procedure TArticleContainer.EndLock;
begin
  Dec(fLockCount);
  if fLockCount < 0 then
    fLockCount := 0;
end;

function TArticleContainer.FindArticleNo(cno: Int64): TArticleBase;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to LoadGetArticleCount - 1 do
    if ArticleBase[i].ArticleNo = cno then
    begin
      Result := ArticleBase[i];
      Break;
    end;
end;

function TArticleContainer.FindChild(article, child: TArticleBase): Boolean;
var
  p: TArticleBase;
begin
  p := article.Child;

  Result := False;

  while p <> nil do
  begin
    Result := child = p;
    if Result then Break;

    Result := FindChild(p, child);
    if Result then Break;
    p := p.Sibling;
  end;
end;

function TArticleContainer.FindMsgID(const msgID: RawByteString): TArticleBase;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to LoadGetArticleCount - 1 do
    if ArticleBase[i].RawMessageId = msgID then
    begin
      Result := ArticleBase[i];
      Break;
    end;
end;

function TArticleContainer.FindUniqueID(const msgID: RawByteString): TArticleBase;
begin
  Result := FindMsgID(msgID);
end;

function TArticleContainer.FindMyXRef(const xref: string): TArticleBase;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to LoadGetArticleCount - 1 do
    if ArticleBase[i].IsMine and (ArticleBase[i].Header['X-Ref'] = xref) then
    begin
      Result := ArticleBase[i];
      Break;
    end;
end;

procedure TArticleContainer.GatherSubjects(article: TArticleBase);
var
  count: Integer;
  c, t, old, prev, rest, tail, newc: TArticleBase;
  subject_table: PHashTable;
  subj: RawByteString;
  hash: DWORD;
begin
  subject_table := AllocHashTable;
  try
    c := article.Child;
    count := 0;

    // Add each root article's simplified subject to the subject table
    while c <> nil do
    begin
      if c.ArticleNo = 0 then
      begin
        t := c.Child;
        if t <> nil then
          while t.fSibling <> nil do
            t := t.fSibling;
      end
      else
        t := c;

      if t <> nil then
        subj := t.SimplifiedSubject
      else
        subj := '';

      if subj <> '' then
      begin
        hash := HashOf(subj);
        old := FindHashSubject(subject_table, hash, subj);

        // Put it in the table if...
        if (old = nil) or                         // ... it's not already there
           ((c.ArticleNo = 0) and (old.ArticleNo <> 0)) or
                                                  // ... this is a dummy and the previous one wasn't
           ((old.ArticleNo <> 0) and (old.SubjectIsReply) and (c.ArticleNo <> 0) and not (c.SubjectIsReply)) then
                                                  // ... this is not a 're' reply and the previous one was.
        begin
          AddHash(subject_table, hash, c);
          Inc(count);
        end;
      end;

      c := c.Sibling
    end;

    if count = 0 then
      Exit;     // Nothing to do.

    prev := nil;
    c := article.Child;
    rest := c.Sibling;

    // The subj_table is now populated with one entry for each subject which
    // occurs in the root set.  Now iterate over the root set, and gather
    // together the difference.

    while c <> nil do
    begin
      if c.ArticleNo <> 0 then
        t := c
      else
      begin
        t := c.Child;
        if t <> nil then
          while t.fSibling <> nil do
            t := t.fSibling;
      end;

      if t <> nil then
        subj := t.SimplifiedSubject
      else
        subj := '';

      if subj <> '' then        // Let empty subjects dangle
      begin
        hash := HashOf(subj);
                                // Unless somethings gone wrong,
                                // the 'Find' will work
        old := FindHashSubject(subject_table, hash, subj);

        if (old <> nil) and (old <> c) then
        begin
          if prev = nil then
            article.fChild := c.fSibling
          else
            prev.fSibling := c.fSibling;

          c.fSibling := nil;

          if (old.ArticleNo = 0) and (c.ArticleNo = 0) then
          begin
            tail := old.fChild;
            while (tail <> nil) and (tail.fSibling <> nil) do
              tail := tail.fSibling;
            tail.fSibling := c.fChild;

            tail := c.fChild;

            while tail <> nil do
            begin
              tail.fParent := old;
              tail := tail.fSibling
            end;

            c.fSibling := nil
          end
          else
            if (old.ArticleNo = 0) or ((c.ArticleNo <> 0) and c.SubjectIsReply and not old.SubjectIsReply) then
            begin
              c.fParent := old;
              c.fSibling := old.fChild;
              old.fChild := c;
            end
            else
            begin
              newc := TArticle.Create(Self);
              newc.Assign(old);
              newc.fArticleNo := old.fArticleNo;
              newc.fMessageOffset := old.fMessageOffset;
              newc.fFlags := old.fFlags;

              newc.fChild := old.fChild;
              tail := newc.fChild;
              while tail <> nil do
              begin
                tail.fParent := newc;
                tail := tail.fSibling;
              end;

              // "new" one is inserted at the place of the "old" one, the later will
              // be deleted when the file is saved. This is done to keep the order
              // of the posts in the file the same.
              RawInsertArticle(IndexOf(old), newc);
              old.fArticleNo := 0;
              old.fMessageOffset := -1;
              old.fChild := nil;

              c.fParent := old;
              newc.fParent := old;
              old.fChild := c;
              c.fSibling := newc;
            end;
          c := prev;
        end;
      end;

      prev := c;
      c := rest;
      if rest <> nil then
        rest := rest.Sibling
      else
        rest := nil;
    end;
  finally
    FreeHashTable(subject_table);
  end;
end;

function TArticleContainer.GetAdjustedArticleCount: Integer;
begin
  GetUnreadArticleCount;
  Result := fAdjustedArticleCount;
end;

function TArticleContainer.GetDisplaySettings: TDisplaySettings;
begin
  Result := NNTPAccounts.DisplaySettings
end;

function TArticleContainer.GetFirstArticle: TArticleBase;
begin
  if LoadGetArticleCount = 0 then
    Result := nil
  else
    if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) then
      if ThreadCount > 0 then
        Result := Threads[0]
      else
        Result := nil
    else
      Result := ArticleBase[0];
end;

function TArticleContainer.GetGroupMultipartMessages: Boolean;
begin
  Result := (fThreadOrder = toChronological) and (fThreadSortOrder = soSubject);
end;

function TArticleContainer.GetInterestingArticleCount: Integer;
begin
  GetUnreadArticleCount;
  Result := fInterestingArticleCount;
end;

function TArticleContainer.GetMessagebaseSize: Int64;
begin
  Result := 0;
end;

function TArticleContainer.GetThreadCount: Integer;
begin
  Result := ArticleCount;
end;

function TArticleContainer.GetThreads(idx: Integer): TArticleBase;
begin
  Result := ArticleBase[idx];
end;

function TArticleContainer.GetUnreadArticleToMeCount: Integer;
begin
  GetUnreadArticleCount;
  Result := fUnreadArticleToMeCount;
end;

function TArticleContainer.GetUnreadInterestingArticleCount: Integer;
begin
  GetUnreadArticleCount;
  Result := fUnreadInterestingArticleCount;
end;

function TArticleContainer.GetUnreadReplyCount: Integer;
begin
  GetUnreadArticleCount;
  Result := fUnreadReplyCount;
end;

function TArticleContainer.GetUnreadXanaNewsArticleCount: Integer;
begin
  GetUnreadArticleCount;
  Result := fUnreadXanaNewsArticleCount;
end;

procedure TArticleContainer.GroupArticles;
begin
// stub
end;

function TArticleContainer.IndexOf(article: TArticleBase): Integer;
begin
  Result := 0;
end;

procedure TArticleContainer.LeaveGroup(clearMessages: Boolean);
begin
  fFocused := False;
end;

procedure TArticleContainer.LoadArticleCounts(reg: TExSettings);
begin
// stub
end;

function TArticleContainer.LoadGetArticleCount: Integer;
begin
  if not Loaded then
    LoadArticles;
  Result := ArticleCount;
end;

function TArticleContainer.Locked: Boolean;
begin
  Result := fLockCount <> 0;
end;

function TArticleContainer.MarkOnLeave: Boolean;
begin
  Result := False;
end;

procedure TArticleContainer.OpenMessageFile;
begin
end;

procedure TArticleContainer.PruneDummyArticles(article: TArticleBase);
var
  prev, next, p, tail, kids: TArticleBase;
begin
  prev := nil;
  p := article.Child;
  next := p.Sibling;

  while p <> nil do
  begin
    if (p.ArticleNo = 0) and (p.Child <> nil) then
      p.fFlags := p.Flags or (p.Child.Flags and (fgInteresting or fgIgnore));

    if (p.ArticleNo = 0) and (p.Child = nil) then
    begin
      if prev = nil then
        article.fChild := p.fSibling
      else
        prev.fSibling := p.fSibling;

      RawRemoveArticle(p);
      p := prev;
    end
    else
      if (p.ArticleNo = 0) and (p.Child <> nil) and ((p.Parent <> nil) or (p.Child.Sibling = nil)) then
      begin
        kids := p.Child;
        if prev = nil then
          article.fChild := kids
        else
          prev.fSibling := kids;

        tail := kids;
        while tail.Sibling <> nil do
        begin
          tail.fParent := p.fParent;
          tail := tail.fSibling;
        end;

        tail.fParent := p.fParent;
        tail.fSibling := p.fSibling;

        next := kids;
        RawRemoveArticle(p);
        p := prev;
      end
      else
        if p.Child <> nil then
          PruneDummyArticles(p);

    prev := p;
    p := next;
    if p = nil then
      next := nil
    else
      next := p.fSibling;
  end;
end;

procedure TArticleContainer.RawInsertArticle(index: Integer; article: TArticleBase);
begin
// stub
end;

procedure TArticleContainer.RawSortArticles;
begin
// stub
end;

procedure TArticleContainer.ResetBozoFlags;
var
  i: Integer;
  art: TArticleBase;
begin
  for i := 0 to LoadGetArticleCount - 1 do
  begin
    art := ArticleBase[i];
    art.fFlags := art.fFlags and not fgScannedBozo;
  end;
end;

procedure TArticleContainer.ResetSortFlags;
begin
  fCurrentThreadOrder := toChronological;
  fCurrentThreadSortOrder := soMessageNo;
  fCurrentThreadSortDirection := sdAscending;
end;

procedure TArticleContainer.ReSortArticles;
begin
  ResetSortFlags;
  ClearThreadInfo;
  SortArticles;
end;

procedure TArticleContainer.SaveArticleCounts(reg: TExSettings);
begin
// stub
end;

procedure TArticleContainer.SetGroupMultipartMessages(const Value: Boolean);
begin
  // for the moment only setting it to True does something.
  if GroupMultipartMessages <> Value then
    if Value then
    begin
      fThreadOrder := toChronological;
      fThreadSortOrder := soSubject;
      fThreadSortDirection := sdAscending;
      SortArticles;
    end;
end;

procedure TArticleContainer.SetHideIgnoredMessages(const Value: Boolean);
begin
  if fHideIgnoredMessages <> Value then
  begin
    fHideIgnoredMessages := Value;
    ResortArticles;
  end;
end;

procedure TArticleContainer.SetHideMessagesNotToMe(const Value: Boolean);
begin
  if fHideMessagesNotToMe <> Value then
  begin
    fHideMessagesNotToMe := Value;
    ResortArticles;
  end;
end;

procedure TArticleContainer.SetHideReadMessages(const Value: Boolean);
begin
  if fHideReadMessages <> Value then
  begin
    fHideReadMessages := Value;
    ResortArticles;
  end;
end;

procedure TArticleContainer.SetSecret(const Value: Boolean);
begin
  fSecret := Value;
end;

procedure TArticleContainer.SetThreadOrder(const Value: TThreadOrder);
begin
  if value <> fThreadOrder then
  begin
    fThreadOrder := value;
    SortArticles;
  end;
end;

procedure TArticleContainer.SetThreadSortDirection(const Value: TThreadSortDirection);
begin
  if fThreadSortDirection <> Value then
  begin
    fThreadSortDirection := Value;
    SortArticles;
  end;
end;

procedure TArticleContainer.SetThreadSortOrder(const Value: TThreadSortOrder);
begin
  if fThreadSortOrder <> Value then
  begin
    fThreadSortOrder := Value;
    SortArticles;
  end;
end;

procedure TArticleContainer.SortArticles;
var
  i, idx: Integer;
  l, n: TArticleBase;
  spamAddresses: TStringList;
  spamNames: TStringList;
  lastPostingHost: string;
  lastAuthor: string;
  filters: TFiltersCtnr;
begin
  if (fSorting) or
     ((fCurrentThreadOrder = fThreadOrder) and
      (fCurrentThreadSortOrder = fThreadSortOrder) and
      (fCurrentThreadSortDirection = fThreadSortDirection)) then
     Exit;

  fDisplayFiltered := False;
  fSorting := True;
  BeginLock;
  try
    if ThreadOrder = toThreaded then
    begin
      ThreadArticles;
      SortThreads;
    end
    else
    begin
      if ThreadSortOrder = soSubject then
      begin
        GroupArticles;
        SortThreads;
      end
      else
      begin
        ClearThreadInfo;
        RawSortArticles;

        if Self is TSubscribedGroup then
          filters := TSubscribedGroup(Self).DisplayFiltersCtnr
        else
          filters := nil;

        fDisplayFiltered := Assigned(filters) and filters.HasFilters;

        if (ThreadSortOrder = soPostingHost) and (ArticleCount > 0) then
        begin
          spamAddresses := nil;
          spamNames := nil;
          try
            spamAddresses := TStringList.Create;
            spamAddresses.Sorted := True;
            spamAddresses.Duplicates := dupIgnore;
            lastAuthor := '';
            lastPostingHost := '';

            spamNames := TStringList.Create;
            spamNames.Sorted := True;
            spamNames.Duplicates := dupIgnore;

            l := ArticleBase[0];

            if HideMessagesNotToMe or HideReadMessages or HideIgnoredMessages or fDisplayFiltered then
              fThreads.Add(l);

            for i := 1 to ArticleCount - 1 do
            begin
              n := ArticleBase[i];

              if (HideMessagesNotToMe and not n.IsMine) or (HideReadMessages and n.IsRead) or (HideIgnoredMessages and n.IsIgnore) or (fDisplayFiltered and filters.BlockArticle(n)) then
                Continue;

              if HideMessagesNotToMe or HideReadMessages or HideIgnoredMessages or fDisplayFiltered then
                fThreads.Add(n);

              l.fSibling := n;
              l.fMultipartFlags := mfNotMultipart;
              l := n;

              if (lastPostingHost = n.PostingHost) and (CompareText(Copy(lastAuthor, 1, 5), Copy(n.FromName, 1, 5)) <> 0) then
              begin
                spamAddresses.Add(lastPostingHost);
                spamNames.Add(lastAuthor);
              end;
              lastPostingHost := n.PostingHost;
              lastAuthor := n.FromName;
            end;

            l.fSibling := nil;
            l.fMultipartFlags := mfNotMultipart;

            if (spamAddresses <> nil) and (spamAddresses.Count > 0) then
            begin
              for i := 0 to ArticleCount - 1 do
              begin
                n := ArticleBase[i];
                if spamAddresses.Find(n.PostingHost, idx) then
                  n.fFlags := n.fFlags or fgSpamSharesPostingHost;
                if spamNames.Find(n.FromName, idx) then
                  n.fFlags := n.fFlags or fgSpamSharesName;
              end;
            end;

          finally
            spamAddresses.Free;
            spamNames.Free;
          end
        end
        else
        begin
          l := nil;
          for i := 0 to ArticleCount - 1 do
          begin
            n := ArticleBase[i];
            if (HideMessagesNotToMe and not n.IsMine) or (HideReadMessages and n.IsRead) or (HideIgnoredMessages and n.IsIgnore) or (fDisplayFiltered and filters.BlockArticle(n)) then
              Continue;

            if HideMessagesNotToMe or HideReadMessages or HideIgnoredMessages or fDisplayFiltered then
              fThreads.Add(n);

            if l <> nil then
              l.fSibling := n;
            l := n;
          end;
        end;
      end;
    end;

    fCurrentThreadOrder := fThreadOrder;
    fCurrentThreadSortOrder := fThreadSortOrder;
    fCurrentThreadSortDirection := fThreadSortDirection;
  finally
    fSorting := False;
    EndLock;
  end;
end;

procedure TArticleContainer.SortSiblings(var root: TArticleBase);
var
  p: TArticleBase;
  i, ct: Integer;
begin
  if (root <> nil) and (root.Sibling <> nil) then
  begin
    fSortBuf.Clear;

    p := root;
    while p <> nil do
    begin
      fSortBuf.Add(p);
      p := p.Sibling
    end;

    fSortBuf.Sort(CompareThreads);

    root := TArticle(fSortBuf.List[0]);
    p := root;
    ct := fSortBuf.Count;
    i := 1;

    while i < ct do
    begin
      p.fSibling := TArticle(fSortBuf.List[i]);
      p := p.fSibling;
      Inc(i);
    end;

    p.fSibling := nil;

    p := root;
    while p <> nil do
    begin
      SortSiblings(p.fChild);
      p := p.Sibling;
    end;
  end
  else
    if (root <> nil) then
      SortSiblings(root.fChild);
end;

procedure TArticleContainer.SortThreads;
var
  i, c: Integer;
  l, n: TArticle;
  oldThreadSortOrder: TThreadSortOrder;
  oldThreadSortDirection: TThreadSortDirection;
begin
  fThreads.Sort(CompareThreads);

  if (fThreads.Count > 0) and not GroupMultipartMessages then
  begin
    l := TArticle(fThreads[0]);

    oldThreadSortOrder := threadSortOrder;
    oldThreadSortDirection := threadSortDirection;
    try
      threadSortOrder := soDate;
      threadSortDirection := sdAscending;

      c := fThreads.Count;
      for i := 1 to c do
      begin
        SortSiblings(l.fChild);
        if i < c then
          n := TArticle(fThreads.List[i])
        else
          n := nil;
        l.fSibling := n;
        l := n;
      end;
    finally
      fThreadSortOrder := oldThreadSortOrder;
      fThreadSortDirection := oldThreadSortDirection;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TArticleContainer.ThreadArticles                           |
 |                                                                      |
 | Thread articles using Jamie Zawinski's algorithm (modified)          |
 |                                                                      |
 | Because speed is of the essence there's a lot of duplicated inline   |
 | code.  Sorry!                                                        |
 *----------------------------------------------------------------------*)
procedure TArticleContainer.ThreadArticles;
var
  i, j: Integer;
  ps, pe, pss: PAnsiChar;
  len: Integer;
  ref: RawByteString;
  article, refArticle, prevArticle, prev, rest, tempRoot: TArticleBase;
  id_table: PHashTable;
  hash: DWORD;
  hideReadMessages: Boolean;
  hideIgnoredMessages: Boolean;
  mine: Boolean;
  filters: TFiltersCtnr;
  id: TIdentity;
  id_not_table: PHashTable;
  reDo: Boolean;
  Identities: TList;
  articleFromName: string;
  articleFromEmail: string;
begin
  reDo := True;
  while reDo do
  begin
    reDo := False;
    fThreads.Clear;
    hideReadMessages := Self.HideReadMessages;
    hideMessagesNotToMe := Self.HideMessagesNotToMe;
    hideIgnoredMessages := Self.HideIgnoredMessages;

    if Self is TSubscribedGroup then
      filters := TSubscribedGroup(Self).DisplayFiltersCtnr
    else
      filters := nil;

    if not filters.HasFilters then
      filters := nil;

    Identities := nil;
    id_not_table := AllocHashTable;            // Create a separate table for ignored messages
    try
      id_table := AllocHashTable;
      try
        Identities := TList.Create;
        // initialize identities local cache
        for j := 0 to NNTPAccounts.Identities.Count - 1 do
          Identities.Add(NNTPAccounts.Identities[j]);

        i := 0;
        while i < ArticleCount do
        begin
          article := ArticleBase[i];
          article.fNewestMessage := -1;

          article.fParent := nil;             // Must get rid of old thread pointers
          article.fSibling := nil;            // otherwise we *really* bugger up!
          article.fChild := nil;

          if (hideMessagesNotToMe and not article.IsMine) or
             (hideReadMessages and article.IsRead) or
             (hideIgnoredMessages and article.IsIgnore) or
             (Assigned(filters) and filters.BlockArticle(article)) then
          begin                                 // Ignore this message
            article.fBlocked := True;
                                                // Add it to hash of ignored msgs
            hash := HashOf(article.fMessageID);
            if FindHashMessage(id_not_table, hash, article.fMessageId) = nil then
              Addhash(id_not_table, hash, article);

            Inc(i);
            Continue;
          end;

          article.fBlocked := False;
          article.fMultipartFlags := mfNotMultipart;
          hash := HashOf(article.fMessageID);

          if FindHashMessage(id_table, hash, article.fMessageID) = nil then
          begin
            Addhash(id_table, hash, article);   // There shouldn't be duplicates, but
                                                // it doesn't matter if there are - they
                                                // will 'point to' the same article.
            mine := False;
            articleFromName := article.FromName;
            articleFromEmail := article.FromEmail;
            for j := 0 to Identities.Count - 1 do
            begin
              id := TIdentity(Identities.List[j]);
              mine := (articleFromName = id.UserName) and (articleFromEmail = id.EMailAddress);
              if mine then
                Break;
            end;

            article.IsMine := mine;
            article.IsReply := False;
            if mine then
              article.fFlags := article.fFlags or fgTemp
            else
              article.fFlags := article.fFlags and not fgTemp;
            Inc(i);
          end
          else
            RawDeleteArticle(i);
        end;

                                            // Go thru the articles.
        for i := 0 to ArticleCount - 1 do
        begin
          article := ArticleBase[i];

          if article.fBlocked then          // It's been removed by
            Continue;                       // Hide Read Messages, etc.

          ps := PAnsiChar(article.fReferences);

          prevArticle := nil;

          // Go thru each entry in 'References'

          // 1.17.5.8.  RFC 1036 says that References must be space
          // separated, but RFC 2822 suggests that the space is optional.
          // So handle either.

          while ps^ <> #0 do
          begin
            pe := StrScan(ps, '>');
            if pe = nil then
              pe := StrScan(ps, ' ')
            else
              Inc(pe);

            // pe^ is now the character after the '>' - which may be a space
            // a nul, or the start of the next id
            if (pe = nil) or (pe^ = #0) then
            begin
              ref := ps;
              len := Length(ref);
              Inc(ps, len);            // ... so ps points to the nul
            end
            else
            begin
              len := pe - ps;
              SetString(ref, ps, len); // Move the ID into ref

              while pe^ = ' ' do       // Skip spaces before the next id
                Inc(pe);
              ps := pe;
            end;

            // For each reference in 'references'
            if (len > 1) and (ref[1] = '<') and (ref[len] = '>') then
            begin
              hash := HashOf(ref);

              refArticle := FindHashMessage(id_table, hash, ref);

              // Referenced article no longer exists Create a dummy.
              if refArticle = nil then
              begin
                refArticle := TArticle.Create(Self);
                refArticle.fMessageID := ref;
                refArticle.fArticleNo := 0; // ie.  It's a dummy article
                refArticle.fSubject := article.fSubject;
                refArticle.fDate := article.fDate;
                refArticle.fMessageOffset := -1;
                refArticle.fFlags := fgRead;
                RawAddArticle(refArticle);
                AddHash(id_table, hash, refArticle);
              end;

              // Now slavishly follow Zawinski's Java threading code.
              if Assigned(prevArticle) and
                 (refArticle.Parent = nil) and
                 (refArticle <> prevArticle) and
                 not FindChild(refArticle, prevArticle) then
              begin
                refArticle.fParent := prevArticle;
                refArticle.fSibling := prevArticle.fChild;
                prevArticle.fChild := refArticle;
              end;
              prevArticle := refArticle;
            end;
          end;

          // Set the parent of this message to be the last element in References
          if (prevArticle <> nil) and ((prevArticle = article) or FindChild(article, prevArticle)) then
            prevArticle := nil;

          if (article.Parent <> nil) then
          begin
            prev := nil;
            rest := article.Parent.Child;

            while rest <> nil do
            begin
              if rest = article then
                Break;
              prev := rest;
              rest := rest.Sibling
            end;

            if rest = nil then
              raise Exception.Create('didn''t find ' + article.MessageID + ' in parent ' + article.parent.MessageID);

            if prev = nil then
              article.Parent.fChild := article.Sibling
            else
              prev.fSibling := article.sibling;

            article.fSibling := nil;
            article.fParent := nil;
          end;

          if prevArticle <> nil then
          begin
            article.fParent := prevArticle;
            article.fSibling := prevArticle.Child;
            prevArticle.fChild := article;
          end;

          if article.IsMine then
          begin
            while article.fParent <> nil do
            begin
              article.IsMine := True;
              article := article.fParent;
            end;

            article.IsMine := True;
          end;
        end;     // End 'for each article

      finally
        FreeHashTable(id_table);
      end;
                                    // Build a dummy article containing all the other
                                    // articles as children - used for pruning and gathering
      tempRoot := TArticle.Create(nil);
      try
        prevArticle := nil;

        for i := 0 to ArticleCount - 1 do
        begin
          article := ArticleBase[i];

          if article.fBlocked then  // ie. doesn't show because of
            Continue;               // Hide read messages, etc.

          if article.fParent = nil then // Add it as a child to tempRoot
          begin
            article.fFlags := article.fFlags and not fgNew;
            if prevArticle = nil then
              tempRoot.fChild := article
            else
              prevArticle.fSibling := article;

            prevArticle := article;
          end
          else
          begin
            if (article.Parent.fFlags and fgTemp) <> 0 then
              article.IsReply := True;
            if (article.fFlags and fgNew) <> 0 then
            begin
              article.fFlags := article.fFlags and not fgNew;

              // Propagate Interesting/Ignore from parent to new children

                                            // But although it doesn't have a parent
                                            // that may just be because the parent is
                                            // hidden.  So look up it's references in
                                            // the ignored messages table and check if
                                            // propogate its interesting and ignore
                                            // flags

              pss := PAnsiChar(article.fReferences);
              pe := pss + Length(article.fReferences) - 1;

              // Go thru each entry in 'References' - backwards this time because
              // we want to check the most recent one first.

              // 1.17.5.8.  RFC 1036 says that References must be space
              // separated, but RFC 2822 suggests that the space is optional.
              // So handle either.

              while (DWORD(pe) > DWORD(pss)) and (pe^ = '>') do
              begin
                ps := pe;
                if DWORD(ps) > DWORD(pss) then
                  repeat
                    Dec(ps);
                  until (ps^ = '<') or (DWORD(ps) < DWORD(pss));
                if DWORD(ps) < DWORD(pss) then
                  Break;

                len := pe - ps + 1;
                SetString(ref, ps, len);     // Move the ID into ref

                pe := ps;
                Dec(pe);
                while (DWORD(pe) > DWORD(pss)) and (pe^ <> '>') do
                  Dec(pe);

                if (len <> 0) and (ref[1] = '<') and (ref[len] = '>') then
                begin
                  if Assigned(article.Parent) and
                     (article.Parent.fMessageId = ref) and
                     (article.Parent.ArticleNo <> 0) then
                    refArticle := article.Parent
                  else
                  begin
                    hash := HashOf(ref);
                    refArticle := FindHashMessage(id_not_table, hash, ref);
                  end;

                  if refArticle <> nil then
                  begin
                    article.fFlags := article.fFlags or (refArticle.fFlags and (fgInteresting or fgIgnore));
                    if (article.fFlags and fgIgnore) <> 0 then
                    begin
                      reDo := True;
                      article.fFlags := article.fFlags or fgRead;
                    end;
                    Break;                      // Only look at previous parent
                  end;
                end;
              end;
            end;
          end;
        end;

        if tempRoot.Child <> nil then
        begin
          PruneDummyArticles(tempRoot);       // Use Zawinski pruning.
          if DisplaySettings.GatherSubjects then
            GatherSubjects(tempRoot);         // Use Zawinski gathering
        end;
                                              // Move all the root articles into the fThreads list
        article := tempRoot.fChild;           // clear their sibling references too.
        while article <> nil do
        begin
          fThreads.Add(article);
          prevArticle := article;
          article := article.fSibling;
          prevArticle.fSibling := nil;
          prevArticle.fParent := nil;
        end;
      finally
        tempRoot.Free;
      end;

      // All messages in threads that contain a message from the current user should
      // be marked.  At the moment only the root thread message is guaranteed to be
      for i := 0 to fThreads.Count - 1 do
      begin
        article := TArticle(fThreads[i]);
        if (article.fFlags and fgMine) <> 0 then
          SetThreadFlags(article.Child, fgMine)
      end
    finally
      FreeHashTable(id_not_table);
      Identities.Free;
    end;
  end;
end;

{ TArticleBase }

procedure TArticleBase.Assign(article: TArticleBase);
begin
  Self.fMessageID := article.fMessageId;
  Self.fBytes := article.fBytes;
  Self.fLines := article.fLines;
  Self.fReferences := article.fReferences;
  Self.fFrom := article.fFrom;
  Self.fSubject := article.fSubject;
  Self.fDate := article.fDate;
  Self.fTo := article.fTo;
end;

constructor TArticleBase.Create(AOwner: TArticleContainer);
begin
  fOwner := AOwner;
  fCodePage := CP_USASCII;
  fMessageOffset := -1;
  fNewestMessage := -1;
end;

procedure TArticleBase.DecodeFromName;
begin
  if not fFromNameDecoded then
  begin
    NewsGlobals.DecodeFromEMail(fFrom, fFromName, fFromEMail, fCodePage);
    fFromNameDecoded := (fCodePage <> -1);
  end;
end;

(*----------------------------------------------------------------------*
 | procedure DecodeMultipartSubject                                     |
 |                                                                      |
 | Return True if the subject line has (eg.) (3/22) or [3/22] within it |
 | indicating that it's part of a multipart. If so, return 'n' = 3 and  |
 | 'x' = 22.                                                            |
 |                                                                      |
 | If 'fixit' is specified, adjust the subject so that it says '03/22'  |
 | instead of '3/22'                                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   var s: string      The subject line to decode                      |
 |   var n: Integer     Return the part number                          |
 |   var x: Integer     Return the total number of parts in the set     |
 |   fixIt: Boolean     Adjust the subject string 's' so that the       |
 |                      n/x bit is justified so that it sorts correctly |
 |                                                                      |
 | The function returns True if the subject indicates that it's part of |
 | a multipart.                                                         |
 *----------------------------------------------------------------------*)
function DecodeMultipartSubject(var s: RawByteString; var n, x: Integer; fixIt, blankIt: Boolean): Boolean;
var
  p, p1, p2: PAnsiChar;
  l, l1, l2: Integer;
  s1: RawByteString;
begin
  Result := False;
  l := Length(s);
  if l > 4 then                   // Can't be less than 5 charcters - eg. [1/3]
  begin
    p := PAnsiChar(s) + l - 1;    // Start at the last character
    p1 := nil;

    while (p <> PAnsiChar(s)) do  // Find the final ']' or ')'
      if (p^ = ')') or (p^ = ']') then
      begin
        p1 := p;
        Break
      end
      else
        Dec(p);

    while (p <> PAnsiChar(s)) do  // Now find the matching ']' or ')'
      if ((p1^ = ')') and (p^ = '(')) or ((p1^ = ']') and (p^ = '[')) then
        Break
      else
        Dec(p);

    if (p1 <> nil) and ((p^ = '[') or (p^ = '(')) then
    begin
      l := p1 - p - 1;
                                // 'l' now contains the length of the bit within
                                // the brackets.  It must be at least 3
      if l > 2 then
      begin
        Inc(p);                 // p now points to the first character in the n/x string
        Dec(p1);                // and p1 points to the last character
        p2 := p;
        repeat                  // Find the '/' in the middle.
          Inc(p2);
          if (p2 = p1) or (p2^ = '/') then Break;
        until False;

        if p2 <> p1 then
        begin                   // We found the '/'

          l2 := p1 - p2;                        // l2 contains the length of the 'x' number
          l1 := p2 - p;                         // l1 contains the length of the 'n' number

          if l2 >= l1 then                      // Reallity check!
          begin
            n := RawStrToIntDef(Copy(p, 1, l1), -1);
            x := RawStrToIntDef(Copy(p2 + 1, 1, l2), -1);

                                                // Are bothe numbers valid?
            if (n <> -1) and (x <> -1) then
            begin
              Result := True;                   // We'e got a multipart!

              if blankIt then                   // Replace the 'n' and 'x' with
                                                // a string of '?' the same length
                                                // as 'x' (for comparing)
              begin
                l1 := p - PAnsiChar(s);
                s := Copy(s, 1, l1) + RawStringOfChar('?', l2) + '/' + RawStringOfChar('?', l2) + (p1 + 1);
              end
              else
                if fixIt and (l1 <> l2) then    // Pad 'n' with zeros so it's the
                begin                           // same length as 'x'
                  l1 := p - PAnsiChar(s);
                  s1 := RawIntToStr(n);
                  l := Length(s1);
                  if l < l2 then
                    s1 := RawStringOfChar('0', l2 - l) + s1;
                                                  // s1 now contains the 'n' string,
                                                  // padded to the same length as the
                                                  // 'x' string.

                  // Re-assemble the subject string with the padded 'n' string
                  s := Copy(s, 1, l1) + s1 + p2;
                end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TArticleBase.DecodedMultipartSubject: RawByteString;
begin
  if not fPartDecoded then
  begin
    fPartSubject := fSubject;
    fIsMultiPart := DecodeMultipartSubject(fPartSubject, fPartNo, fPartCount, False, True);
    fPartDecoded := True;
  end;
  Result := fPartSubject;
end;

function TArticleBase.GetCodePage: Integer;
begin
  Result := fCodePage;
end;

function TArticleBase.GetCodePageFromFile: Integer;
var
  st, st1: string;
begin
  if (fCodePage = -1) or ((Assigned(fMsg) and ((fFlags and fgUnknownCodePage) <> 0))) then
  begin
    if Assigned(fMsg) then
      fCodePage := Msg.Codepage
    else
    begin
      fFlags := Flags or fgUnknownCodePage;
      st := PeekAtMsgHdr('content-type');
      if st <> '' then
      begin
        st1 := SplitString('charset', st);
        if st <> '' then
        begin
          st1 := SplitString('=', st);
          if st <> '' then
          begin
            st1 := SplitString(';', st);
            fCodePage := MimeCharsetNameToCodePage(AnsiDequotedStr(st1, '"'));
          end;
        end;
      end;

      if fCodePage = -1 then
      begin
        if Copy(FromName, 1, 1) > #127 then
        begin
          st := ExtractFileExt(FromEMail);
          fCodePage := URLSuffixToCodePage(st);
        end
        else
          fCodePage := CP_USASCII;
      end;
    end;

    // In case the post does not have a codepage/charset defined at all,
    // change it to the user' preference when also the From and Subject
    // does not provide any clue about what the codepage might be.
    if fCodePage = CP_USASCII then
    begin
      st := HeaderCharset(string(fFrom));
      if st = '' then
        st := HeaderCharset(string(fSubject));

      if st <> '' then
        CodePage := MimeCharsetNameToCodePage(st)
      else
        CodePage := Owner.DisplaySettings.DefaultCodepage;
    end;
  end;

  Result := fCodePage;
end;

function TArticleBase.GetCrosspostedTo: Integer;
var
  s: string;
  ch: char;
begin
  s := GetHeader('Newsgroups');
  if s = '' then
  begin
    ch := ' ';
    s := GetHeader('Xref');
  end
  else
    ch := ',';

  if s = '' then
    Result := 1
  else
  begin
    Result := 0;
    while SplitString(ch, s) <> '' do
      Inc(Result);

    if ch = ' ' then
      Dec(Result);

    if Result <= 0 then
      Result := 1;
  end;
end;

function TArticleBase.GetFrom: string;
begin
  Result := string(fFrom); // !!!!!!
end;

function TArticleBase.GetFromEmail: string;
begin
  if not fFromNameDecoded then
    DecodeFromName;

  Result := fFromEmail;
end;

function TArticleBase.GetFromName: string;
begin
  if not fFromNameDecoded then
    DecodeFromName;

  Result := fFromName;
end;

function TArticleBase.GetHasAttachment: Boolean;
var
  gotHasAttachment: Boolean;
  i: Integer;
  mps: TmvMessageParts;
  mp: TmvMessagePart;
  mh: TMimeHeader;
begin
  gotHasAttachment := (flags and fgScannedAttachment) <> 0;
  if HasMsg and not gotHasAttachment then
  begin
    if Assigned(Msg) then
    begin
      fFlags := fFlags or fgScannedAttachment;
      mps := fMsg.MessageParts;
      for i := 0 to mps.Count - 1 do
      begin
        mp := mps[i];
        if mp.Complete then
        begin
          case mp.DecodeType of
            ttText, ttQuotedPrintable:
              if mp is TmvMimeMessagePart then
              begin
                mh := TmvMimeMessagePart(mp).MimeHeader;
                if Assigned(mh) and SameText(mh.ContentDisposition, 'attachment') then
                  fFlags := fFlags or fgHasAttachment;
              end;

            // ttBase64: it is only an attachment when inside one of the message parts.
            ttBase64:
              if i > 0 then
                fFlags := fFlags or fgHasAttachment;

            // ttUUEncode, ttYEnc: can be (are) attachments directly in the body
            ttUUEncode, ttYEnc:
              fFlags := fFlags or fgHasAttachment;
          end;
        end
        else
          if (fFlags and fgHasAttachment) = 0 then
            fFlags := fFlags and not (fgScannedAttachment);
      end;
      Owner.fFlagsDirty := True;
    end;
  end;

  Result := (flags and fgHasAttachment) <> 0;
end;

function TArticleBase.GetHasNoReplies: Boolean;
begin
  Result := (fChild = nil) and (fParent = nil) and (owner.ThreadOrder = toThreaded);
end;

function TArticleBase.GetHeader(const name: string): string;
var
  i: Integer;
  s: string;
begin
  if Assigned(fMsg) then
  begin
    Result := '';
    for i := 0 to Msg.Header.Count - 1 do
    begin
      s := string(Msg.Header[i]);
      if SameText(name, SplitString(':', s)) then
      begin
        Result := s;
        Break;
      end;
    end;
  end
  else
    Result := PeekAtMsgHdr(name);
end;

function TArticleBase.GetIndex: Integer;
var
  base: TArticleBase;
  n: Integer;
begin
  n := 0;
  base := Owner.GetFirstArticle;
  while Assigned(base) and not base.Equals(Self) do
  begin
    Inc(n);
    base := base.GetNext;
  end;

  if Assigned(base) then
    Result := n
  else
    Result := -1;
end;

function TArticleBase.GetInterestingMessageLine: string;
begin
  Result := Subject;
end;

function TArticleBase.GetIsCancelled: Boolean;
begin
  Result := (fFlags and fgCancelled) <> 0;
end;

function TArticleBase.GetIsDeleted: Boolean;
begin
  Result := (fFlags and fgDeleted) <> 0;
end;

function TArticleBase.GetIsDormant: Boolean;
var
  p: TArticleBase;
begin
  Result := IsRead;
  if Result and Assigned(child) then
  begin
    p := child;
    repeat
      Result := p.IsDormant;
      if not Result then Break;
      p := p.Sibling;
    until p = nil;
  end
end;

function TArticleBase.GetIsFromBozo: Boolean;
var
  art: TArticle;
  action: TBozoAction;
  p: TArticleBase;
  pp: TArticleBase;
  doWholeThread: Boolean;
begin
  action := baIgnore;
  if (fFlags and fgScannedBozo) <> 0 then
    Result := (fFlags and fgBozo) <> 0
  else
  begin
    if Self is TArticle then
      art := TArticle(Self)
    else
      art := nil;
    if NNTPAccounts.ArticleIsFromBozo(art, action) then
    begin
      fFlags := fFlags or fgBozo;
      Result := True;
    end
    else
    begin
      fFlags := fFlags and not fgBozo;
      Result := False;
    end;
    fFlags := fFlags or fgScannedBozo;
    if Result then
    begin
      p := Self;
      pp := p;
      doWholeThread := (action = baIgnoreThread) or (action = baMarkAsReadThread);
      while p <> nil do
      begin
        if (action = baIgnore) or (action = baIgnoreThread) and not p.IsFromMe then
          p.IsIgnore := True
        else
          if (action = baMarkAsRead) or (action = baMarkAsReadThread) then
            p.IsRead := True;

        if doWholeThread then
        begin
          if (p.Child <> nil) and not p.IsFromMe then
            p := p.Child
          else
            if p.Sibling <> nil then
              if p <> pp then
                p := p.Sibling
              else
                p := nil
            else
            begin
              p := p.Parent;
              if (p = nil) or (p = pp) then Break;
              p := p.Sibling;
            end;
        end
        else
          p := nil;
      end;
    end;
  end;
end;

function TArticleBase.GetIsFromMe: Boolean;
begin
  Result := FromName = Owner.Identity.UserName;
end;

function TArticleBase.GetIsIgnore: Boolean;
begin
  Result := fFlags and fgIgnore <> 0;
end;

function TArticleBase.GetIsInteresting: Boolean;
begin
  Result := fFlags and fgInteresting <> 0;
end;

function TArticleBase.GetIsMine: Boolean;
begin
  Result := (fFlags and fgMine) <> 0;
end;

function TArticleBase.GetIsNotOnServer: Boolean;
begin
  Result := ((fFlags and fgNotOnServer) <> 0);
end;

function TArticleBase.GetIsOdd: Boolean;
var
  p: TArticleBase;
begin
  p := Self;
  while p.Parent <> nil do
    p := p.Parent;

  if (p.fOwner.fThreadOrder = toThreaded) or (p.fOwner.fThreadSortOrder = soSubject) or (p.fOwner.HideReadMessages) or (p.fOwner.HideIgnoredMessages) or (p.fOwner.fDisplayFiltered) or (p.fOwner.fHideMessagesNotToMe) then
    Result := Odd(p.Owner.fThreads.IndexOf(p))
  else
    Result := Odd(p.Owner.IndexOf(p));
end;

function TArticleBase.GetIsRead: Boolean;
begin
  Result := ((fFlags and fgRead) <> 0) or IsFromBozo;
end;

function TArticleBase.GetIsReply: Boolean;
begin
  Result := (fFlags and fgReply) <> 0;
end;

function TArticleBase.GetIsXanaNews: Boolean;
begin
  Result := (fFlags and fgXanaNews) <> 0;
end;

function TArticleBase.GetMessageID: string;
begin
  Result := string(fMessageId);
end;

function TArticleBase.GetMessagesInThread: Integer;
var
  p: TArticlebase;
begin
  p := Self;
  while Assigned(p) and Assigned(p.Parent) do
    p := p.Parent;

  Result := 0;

  while Assigned(p) do
  begin
    Inc(Result);
    p := p.Next;
    if (p = nil) or (p.Parent = nil) then
      Break;
  end;
end;

function TArticleBase.GetMsgFromFile: TmvMessage;
var
  st: RawByteString;
  len: DWORD;
  hLen: Word;
  cp: Integer;
begin
  if not Assigned(fMsg) and (fMessageOffset <> -1) then
  begin
    if Assigned(Owner.fMessageFile) then
    begin               // Load the message from 'messages.dat'
      Owner.fMessageFile.Seek(fMessageOffset, soBeginning);

      SetLength(st, 5);        // Read XMsg: prefix
      Owner.fMessageFile.Read(st[1], 5);

      if st = 'X-Msg' then     // New style - contains extra header lines
      begin
        SetLength(st, 9);
        Owner.fMessageFile.Read(st[1], 9);
        st[1] := '$';
        len := StrToInt(string(st));

        Owner.fMessageFile.Read(hLen, SizeOf(hLen));

        cp := fCodePage;
        fMsg := TmvMessage.Create(Self);

        while hLen > 0 do
        begin
          SetLength(st, hLen);
          Owner.fMessageFile.Read(st[1], hLen);
          fMsg.Header.Add(st);

          Owner.fMessageFile.Read(hLen, SizeOf(hLen));
        end;

        if len <> 0 then
          fMsg.RawData.CopyFrom(Owner.fMessageFile, len);

        // Needs to be after reading the body, since the body can
        // contain multipart MIME headers specifying the charset.
        if fMsg.Codepage = CP_USASCII then
          fMsg.CodePage := cp;

        MessageCacheController.Add(Self);
      end;
    end;
  end;

  Result := fMsg;

  if Assigned(Result) then
    Result.PartialMessage := MultipartFlags <> mfNotMultipart;
end;

function TArticleBase.GetNext: TArticleBase;
var
  idx: Integer;
begin
  if Assigned(Child) then
    Result := Child
  else
    if Assigned(Sibling) then
      Result := Sibling
    else
    begin
      Result := Parent;
      if Assigned(Result) then
        while Assigned(Result) do
        begin
          if Assigned(Result.sibling) then
          begin
            Result := Result.sibling;
            Break;
          end
          else
            Result := Result.parent
        end
      else
        if Owner.ThreadOrder = toChronological then
        begin
          idx := Owner.IndexOf(Self);
          if (idx >= 0) and (idx < Owner.LoadGetArticleCount - 1) then
            Result := Owner.ArticleBase[idx + 1];
        end;
    end;
end;

function TArticleBase.GetPostingHost: string;
begin
// stub
end;

function TArticleBase.GetReferences: string;
begin
  Result := string(fReferences);
end;

function TArticleBase.GetSimplifiedSubject: RawByteString;
var
  p: Integer;
  t: Boolean;
begin
  t := False;
  Result := fSubject;
  while (RawSameText(Copy(Result, 1, 2), 're')) do
  begin
    t := True;
    p := RawPos(':', Result);
    if p = 0 then
      Break;

    if p > 3 then
      if (Result[3] <> '[') or (Result[p - 1] <> ']') then
        Break;

    Delete(Result, 1, p);
  end;
  if t then
    Result := RawTrim(Result);
end;

function TArticleBase.GetSubject: string;
begin
  Result := DecodeSubject(fSubject, CodePage);
end;

function TArticleBase.GetSubjectIsReply: Boolean;
var
  s: string;
  p: Integer;
begin
  s := Subject;
  Result := SameText(Copy(s, 1, 2), 're');

  if Result then
  begin
    p := Pos(':', s);
    Result := p <> 0;

    if Result then
      if p > 3 then
        Result := (s[3] = '[') and (s[p - 1] = ']');
  end;
end;

function TArticleBase.GetUniqueID: RawByteString;
begin
  Result := RawMessageID;
end;

function TArticleBase.GetUnreadMessagesInThread: Integer;
var
  p: TArticlebase;
begin
  p := Self;
  while Assigned(p) and Assigned(p.Parent) do
    p := p.Parent;

  Result := 0;

  while Assigned(p) do
  begin
    if not p.IsRead then
      Inc(Result);
    p := p.Next;
    if (p = nil) or (p.Parent = nil) then
      Break;
  end;
end;

function TArticleBase.HasMsg: Boolean;
begin
  Result := Assigned(fMsg) or (fMessageOffset <> -1);
end;

(*----------------------------------------------------------------------*
 | procedure TArticleBase.Initialize ()                                 |
 |                                                                      |
 | Initialize article fields from header strings.  The header strings   |
 | that are used to initialize article fields are removed from the      |
 | header strings.  The resulting string list therefore contains        |
 | only 'extra' header strings.                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   articleNo: Int64;                                                  |
 |   header: TStrings        // nb in/out parameter                     |
 *----------------------------------------------------------------------*)
procedure TArticleBase.Initialize(articleNo: Int64; header: TAnsiStrings);
var
  hdrs: TAnsiStringList;
  i: Integer;
  dt: RawByteString;

  function ProcessHeaderLine(const valueName: RawByteString): RawByteString;
  var
    idx: Integer;
  begin
    idx := hdrs.IndexOfName(valueName);
    if idx >= 0 then
    begin
      Result := RawTrim(hdrs.Values[valueName]);
      header.Delete(idx);
      hdrs.Delete(idx);
    end
    else
      Result := '';
  end;

begin
  hdrs := TAnsiStringList.Create;
  try
    hdrs.NameValueSeparator := ':';
    for i := 0 to header.Count - 1 do
      hdrs.Add(header[i]);

    fArticleNo := articleNo;

    fMessageID := ProcessHeaderLine('Message-ID');
    fCodePage := RawStrToIntDef(ProcessHeaderLine('CodePage'), -1);
    fBytes := RawStrToIntDef(ProcessHeaderLine('Bytes'), 0);
    fLines := RawStrToIntDef(ProcessHeaderLine('Lines'), 0);
    fReferences := ProcessHeaderLine('References');
    fFrom := ProcessHeaderLine('From');
    fSubject := ProcessHeaderLine('Subject');
    dt := ProcessHeaderLine('Date');
    fDate := RawGMTToLocalDateTime(string(dt));
    if StrInternetToDateTime(string(dt)) <> fDate then
      header.Add('X-XanaOrigDate:' + dt);

    if RawPos('XanaNews', hdrs.Values['X-Newsreader']) > 0 then
      fFlags := fFlags or fgXanaNews;

    if RawPos('XanaNews', hdrs.Values['User-Agent']) > 0 then
      fFlags := fFlags or fgXanaNews;
  finally
    hdrs.Free;
  end;
end;

function TArticleBase.MsgDownloading: Boolean;
begin
  Result := Assigned(fMsg) and ((fMessageOffset = -1) or fMsg.Updating);
end;

function TArticleBase.PeekAtMsgHdr(const hdr: string): string;
begin
  Result := '';
end;

function TArticleBase.PeekAtMsgHdrFromFile(const hdr: RawByteString): RawByteString;
var
  raw: RawByteString;
  hLen: Word;
begin
  Result := '';
  if (fMessageOffset <> -1) and Assigned(Owner.fMessageFile) then
  begin                 // Load (some of) the message from 'messages.datx'
                        // I did this to prevent huge messages being loaded/
                        // decoded when simply displaying the headers for non-
                        // selected messages.

    Owner.fMessageFile.Seek(fMessageOffset, soBeginning);

    SetLength(raw, 5);         // Read XMsg: prefix
    Owner.fMessageFile.Read(raw[1], 5);

    if raw = 'X-Msg' then      // New style - contains extra header lines
    begin
      SetLength(raw, 9);
      Owner.fMessageFile.Read(raw[1], 9);
      Owner.fMessageFile.Read(hLen, SizeOf(hLen));

      while hLen > 0 do
      begin
        SetLength(raw, hLen);
        Owner.fMessageFile.read(raw[1], hLen);

        if RawCompareText(RawSplitString(':', raw), hdr) = 0 then
        begin
          Result := RawTrim(raw);
          Exit;
        end;
        Owner.fMessageFile.Read(hLen, SizeOf(hLen));
      end;
    end;
  end;
end;

procedure TArticleBase.ReleaseMsg;
begin
  if not MsgDownloading then
    FreeAndNil(fMsg);
end;

procedure TArticleBase.SetCodePage(const Value: Integer);
begin
  if fCodePage <> Value then
  begin
    fCodePage := Value;
    // When the CodePage has changed the fromName needs to be re-decoded using
    // the current/actual CodePage settings.
    fFromNameDecoded := False;
  end;
end;

procedure TArticleBase.SetFlag(flag: DWORD; value: Boolean);
begin
  if ((fFlags and flag) <> 0) <> value then
  begin
    if value then
      fFlags := fFlags or flag
    else
      fFlags := fFlags and not flag;
    if (flag <> fgMine) and (flag <> fgReply) then
      Owner.fFlagsDirty := True;
  end;
end;

procedure TArticleBase.SetIsCancelled(const Value: Boolean);
begin
  SetFlag(fgCancelled, value);
end;

procedure TArticleBase.SetIsDeleted(const Value: Boolean);
begin
  SetFlag(fgDeleted, value);
  if value then
    Owner.fNeedsPurge := True;
end;

procedure TArticleBase.SetIsIgnore(const Value: Boolean);
begin
  SetFlag(fgIgnore, Value);
  if Value then
  begin
    fFlags := fFlags or fgRead;
    fFlags := fFlags and not fgInteresting;
  end;
end;

procedure TArticleBase.SetIsInteresting(const Value: Boolean);
begin
  SetFlag(fgInteresting, Value);
  if Value then
    fFlags := fFlags and not fgIgnore;
end;

procedure TArticleBase.SetIsMine(const Value: Boolean);
begin
  SetFlag(fgMine, value);
end;

procedure TArticleBase.SetIsNotOnServer(const Value: Boolean);
begin
  SetFlag(fgNotOnServer, Value);
end;

procedure TArticleBase.SetIsRead(const Value: Boolean);
begin
  SetFlag(fgRead, value);
  Owner.fUnreadArticleCount := -1;
  Owner.fUnloadedArticleCount := -1;
end;

procedure TArticleBase.SetIsReply(const Value: Boolean);
begin
  SetFlag(fgReply, value);
end;

procedure TArticleBase.SetMsg(const Value: TmvMessage);
begin
// stub
end;

procedure TArticleBase.SetSubject(const Value: string);
begin
  fSubject := WideStringToAnsiString(Value, CodePage);
end;

{ TNNTPMessageCacheController }

function TNNTPMessageCacheController.CanRemove(Obj: TObject): Boolean;
var
  article: TArticleBase;
begin
  if fAlwaysRemove then
  begin
    Result := True;
    Exit;
  end;

  try
    if (Obj <> nil) and (Obj is TArticleBase) then
    begin
      article := TArticleBase(Obj);

      Result := not Assigned(article.fMsg);
      if not Result then
      begin
                                    // Don't delete if the article's message is being
                                    // downloaded (fMsg exists but fMessageOffset is still -1)
                                    // or is being displayed.
        if not article.MsgDownloading and not article.fMsg.BeingDisplayed then
        begin
          Result := True;
          FreeAndNil(article.fMsg);
        end;
      end;
    end
    else
      Result := True;
  except
    Result := True;
  end;
end;

{ TArticleObjectContainer }

constructor TArticleObjectContainer.Create(const AName: string; AFiltersParent, ADisplayFiltersParent: TFiltersCtnr);
begin
  inherited Create(AName, AFiltersParent, ADisplayFiltersParent);
  fArticles := TObjectList.Create;
end;

destructor TArticleObjectContainer.Destroy;
begin
  fArticles.Free;
  inherited Destroy;
end;

function TArticleObjectContainer.FindArticleNo(cno: Int64): TArticleBase;
begin
  if not fArticlesLoaded then
    LoadArticles;

  Result := inherited FindArticleNo(cno);
end;

function TArticleObjectContainer.GetArticleBase(idx: Integer): TArticleBase;
begin
  if not fArticlesLoaded then
    LoadArticles;
  if idx < fArticles.Count then
    Result := TArticleBase(fArticles.List[idx])
  else
    Result := nil;
end;

function TArticleObjectContainer.GetArticleCount: Integer;
begin
  if not fArticlesLoaded then
  begin
    if fUnloadedArticleCount = -1 then
      GetUnreadArticleCount;
    Result := fUnloadedArticleCount
  end
  else
    Result := fArticles.Count;
end;

function TArticleObjectContainer.GetLoaded: Boolean;
begin
  Result := fArticlesLoaded;
end;

function TArticleObjectContainer.GetThreadCount: Integer;
begin
  if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) or hideReadMessages or HideMessagesNotToMe or hideIgnoredMessages or fDisplayFiltered then
  begin
    LoadArticles;
    Result := fThreads.Count;
  end
  else
    Result := inherited GetThreadCount;
end;

function TArticleObjectContainer.GetThreads(idx: Integer): TArticleBase;
begin
  LoadArticles;
  if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) or hideReadMessages or HideMessagesNotToMe or hideIgnoredMessages or fDisplayFiltered then
    Result := TArticleBase(fThreads[idx])
  else
    Result := TArticleBase(fArticles[idx]);
end;

function TArticleObjectContainer.IndexOf(article: TArticleBase): Integer;
begin
  Result := fArticles.IndexOf(article)
end;

(*----------------------------------------------------------------------*
 | procedure TArticleObjectContainer.PurgeArticles                      |
 |                                                                      |
 | Remove articles marked as 'read' - and put them in an archived       |
 | message folder.                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   all,                               Purge all message, not just     |
 |                                      'read' ones.                    |
 |                                                                      |
 |   reset: Boolean                     Reset the High Water Mark       |
 |                                                                      |
 |   const folderName: string           Folder name to purge to.        |
 *----------------------------------------------------------------------*)
procedure TArticleObjectContainer.PurgeArticles(all, reset: Boolean; const folderName: string);
var
  i: Int64;
  article: TarticleBase;
  recreateMessages: Boolean;
  lastArt, lastCurrentArticle: Int64;
  reg: TExSettings;
  folder: TArticleFolder;
  newArticles, tmp, deletedArticles: TObjectList;
begin
  if not fNeedsPurge and not all then Exit;
  i := 0;
  recreateMessages := False;
  lastArt := 0;
  lastCurrentArticle := 0;
  if folderName = '' then
    folder := nil
  else
    folder := gArticleFolders.FindFolder(folderName);

  MessageCacheController.Clear;
  MessageCacheController.AlwaysRemove := True;

  newArticles := TObjectList.Create;
  deletedArticles := TObjectList.Create;

  BeginLock;
  try // finally
    newArticles.OwnsObjects := False;
    deletedArticles.OwnsObjects := False;
    if ArticleCount < 100 then
      newArticles.Capacity := 100
    else
      newArticles.Capacity := fArticles.Count;

    try // except

      if all then   // Delete all articles.  Don't copy them into the purged folder
      begin
        recreateMessages := True;
        fArticlesLoaded := True;
        if Self is TSubscribedGroup then
          with TSubscribedGroup(Self) do
            if reset then
              fHighWaterMark := 0
            else
              fHighWaterMark := LastArticle;
      end
      else
      begin
        LoadArticles;

                    // Go thru the articles looking for deleted ones.
                    //
                    // Put undeleted articles in 'newArticles' and
                    // deleted ones in 'deletedArticles'

        while i < fArticles.Count do
        begin
          article := TArticle(fArticles[i]);
                                            // Keep track of 'highest ever' article no
          if (article.ArticleNo > lastArt) and (article.ArticleNo <> 99999999) then
            lastArt := article.ArticleNo;
                                            // Keep track of highest current article no
          if not (article.IsDeleted) and (article.ArticleNo > lastCurrentArticle) then
            lastCurrentArticle := Article.ArticleNo;

          if (Assigned(article.fMsg) and (article.fMessageOffset = -1)) or not article.IsDeleted then
            newArticles.Add(article)
          else
          begin
            if not recreateMessages then  // This is the first deleted article
            begin
              if Assigned(folder) then
                folder.BeginAdd;

              recreateMessages := True;
              SetTempStatusMessage('Purging ' + Name, 0, 0);
            end;

            if Assigned(folder) then
            begin
              if not Assigned(article.fMsg) then
                OpenMessageFile;

              folder.AddArticle(article);
            end;

            deletedArticles.Add(article);
          end;
          Inc(i);
        end;
      end;
    except
      if all then
        ShowMessage('Error purging all messages from ' + Name)
      else
        ShowMessageFmt('Error purging message %d from %d messages in %s', [i, fArticles.Count, Name]);
      raise
    end;

    MessageCacheController.Clear;
    MessageCacheController.AlwaysRemove := False;

    // All messages are still owned by fArticles at this stage.  However, if we're
    // not deleting 'all, then 'deletedArticles' contains the deleted ones,
    // and newArticles contains the undeleted ones.

    if recreateMessages then // Where any messages deleted ?
    begin
      tmp := fArticles;
      fArticles := newArticles;
      fArticles.OwnsObjects := True;
      newArticles:= tmp;

      if not all then
      begin
        newArticles.OwnsObjects := False;
        deletedArticles.OwnsObjects := True;
      end;

                  // Save the articles and their messages.
      SaveArticles(True);

                    // Reset current sort flags, so sorting will occur next
      ResetSortFlags;

      LoadArticles;     // Re-opens the message file, and sorts the articles.  This
                        // may remove duplicates - which is vital before calling
                        // GetLastArticle otherwise potential 'Index Out of Bounds'

      SetTempStatusMessage('', 0, 0);
    end;

    if Loaded and (Self is TSubscribedGroup) then with TSubscribedGroup(Self) do
    begin
      i := GetLastArticle;    // nb.  Get's current high water mark or highest
                              // article no - whichever's higher

      if lastArt > i then     // If highest ever article is even higher, use that
        fHighWaterMark := lastArt;

      if fHighWaterMark <= lastCurrentArticle then
        fHighWaterMark := 0;

      reg := CreateGroupRegistry(KEY_READ or KEY_WRITE);
      if Assigned(reg) then
      try
        reg.SetInteger64Value('Last Article', fHighWatermark, 0);
      finally
        reg.Free;
      end;
    end;

  finally
    try
      if Assigned(folder) and recreateMessages then
        folder.EndAdd;
    except
    end;

    try
      newArticles.Free;         // Owns the old articles if 'all'
      deletedArticles.Free;     // Owns the deleted articles if not 'all'.
    except
    end;

    EndLock;
    fNeedsPurge := False;
    MessageCacheController.AlwaysRemove := False;
  end;
end;

procedure TArticleObjectContainer.RawAddArticle(article: TArticleBase);
begin
  fArticles.Add(article);
end;

procedure TArticleObjectContainer.RawClearArticles;
begin
  fArticles.Clear;
  fArticlesLoaded := True;       // Yes - True!
end;

procedure TArticleObjectContainer.RawDeleteArticle(cno: Integer);
begin
  fArticles.Delete(cno);
end;

procedure TArticleObjectContainer.RawInsertArticle(index: Integer; article: TArticleBase);
begin
  fArticles.Insert(index, article);
end;

procedure TArticleObjectContainer.RawRemoveArticle(article: TArticleBase);
begin
  fArticles.Remove(article);
end;

procedure TArticleObjectContainer.RawSortArticles;
begin
  fArticles.Sort(CompareThreads);
end;

procedure TArticleObjectContainer.UnloadArticles;
var
  i: Integer;
  article: TArticleBase;
  dontFree: Boolean;
begin
  if not fArticlesLoaded then Exit;
  if DontUnloadCache.IndexOf(Self) >= 0 then Exit;
  if Locked then Exit;
  if fSearching then Exit;
  if fSorting then Exit;
  if fFocused then Exit;                // Container is focused in the UI
                                        // An article body is being downloaded
  if ThreadManager.GettingArticle(Self, nil) then Exit;

  LogMessage('Unloading ' + name);

  if fFlagsDirty then                   // Save articles (headers) if the flags
    SaveArticles(False);                // need saving

  MessageCacheController.Clear;
  dontFree := False;
                                        // Free the message bodies, and do a final
                                        // check that a message isn't being downloaded
  for i := 0 to fArticles.Count - 1 do
  begin
    article := TArticle(fArticles[i]);
    if not article.MsgDownloading then
      article.ReleaseMsg
    else
      dontFree := True;
  end;

  if not dontFree then                  // Nothing's being downloaded
  begin
    if Self is TSubscribedGroup then
      TSubscribedGroup(Self).fRawLastArticle := TSubscribedGroup(Self).LastArticle;
    fArticlesLoaded := False;
    fArticles.Clear;                    // Clear the article headers.
  end;
end;

procedure TNNTPAccounts.SetDoVersionCheck(const Value: Integer);
var
  reg: TExSettings;
begin
  if fDoVersionCheck <> Value then
  begin
    fDoVersionCheck := Value;
    reg := CreateExSettings;
    try
      reg.SetIntegerValue('HTTP Permission Required', Value, 0);
    finally
      reg.Free;
    end;
  end;
end;

procedure TNNTPAccounts.SetHideDormantConnections(const Value: Boolean);
var
  reg: TExSettings;
begin
  if fHideDormantConnections <> Value then
  begin
    fHideDormantConnections := Value;
    reg := CreateExSettings;
    try
      reg.SetBooleanValue('Hide Dormant Connections', Value, True);
    finally
      reg.Free;
    end;
  end;
end;

procedure TNNTPAccounts.SetShowSecrets(const Value: Boolean);
begin
  fShowSecrets := Value;
  fSecretAccountCount := -1;
end;

procedure TNNTPAccounts.UnloadOldContainers(exceptFor: TArticleObjectContainer);
var
  i, j: Integer;
  grp: TArticleObjectContainer;
begin
  for i := 0 to Count - 1 do
    for j := 0 to Items[i].SubscribedGroupCount - 1 do
    begin
      grp := Items[i].SubscribedGroups[j];
      if grp <> exceptFor then
        grp.UnloadArticles;
    end;
end;


{ TServerAccount }

constructor TServerAccount.Create(const AName: string);
begin
  fName := AName;
end;

function TServerAccount.GetIdentity: TIdentity;
begin
  Result := NNTPAccounts.Identities.DefaultIdentity;
end;

function TServerAccount.GetPostingSettings: TPostingSettings;
begin
  Result := NNTPAccounts.PostingSettings;
end;

procedure TServerAccount.SetName(const Value: string);
begin
  fName := Value;
end;

{ TBozo }

procedure TBozo.Assign(b: TBozo);
begin
  Self.fName := b.fName;
  Self.fEMail := b.fEMail;
  Self.fBozodDate := b.fBozodDate;
  Self.fFlags := b.fFlags;
  Self.fSearcher := nil;
  Self.fKeyphrase := b.fKeyphrase;
  Self.fAction := b.fAction;
end;

constructor TBozo.Create(const AName, AEMail: string; ABozodDate: TDateTime; AAction: TBozoAction);
begin
  if AName <> '' then
    fName := AName
  else
    fName := AEMail;
  fEMail := AEMail;
  fBozodDate := ABozodDate;
  if AName <> '' then
    fFlags := [fgName]
  else
    fFlags := [];
  if AEMail <> '' then
    Include(fFlags, fgEMail);
  fAction := AAction;
end;

procedure TNNTPAccounts.ReplaceBozos(newBozos: TObjectList);
var
  i: Integer;
  b, b1: TBozo;
begin
  fBozos.Clear;

  for i := 0 to newBozos.Count - 1 do
  begin
    b := TBozo(newBozos[i]);
    b1 := TBozo.CreateNew;
    b1.Assign(b);
    fBozos.Add(b1);
  end;

  SaveBozoList;
  ResetAllBozoFlags;
end;

procedure TNNTPAccounts.ResetAllBozoFlags;
var
  i, j: Integer;
  acc: TNNTPAccount;
  grp: TSubscribedGroup;
begin
  for i := 0 to Count - 1 do
  begin
    acc := Items[i];
    for j := 0 to acc.SubscribedGroupCount - 1 do
    begin
      grp := acc.SubscribedGroups[j];
      if grp.Loaded then
        grp.ResetBozoFlags;
    end;
  end;
end;

constructor TBozo.CreateNew;
begin
end;

destructor TBozo.Destroy;
begin
  fSearcher.Free;
  inherited Destroy;
end;

function TBozo.GetSearcher: TStringSearcher;
begin
  if fSearcher = nil then
    fSearcher := TGoogleLikeStringSearcher.Create('', False);

  Result := fSearcher;
end;

function TBozo.MatchesArticle(art: TArticle): Boolean;
begin
  if fgName in Flags then
    Result := WildContains(art.FromName, Name)
  else
    Result := True;

  if Result and (fgEMail in Flags) then
    Result := Result and WildContains(art.FromEmail, EMail);

  if Result and (fgKeyphrase in Flags) and (fKeyphrase <> '') then
    Result := Result and art.MatchesKeyPhrase(fKeyphrase, Searcher);
end;

procedure TNNTPAccounts.ChangeAccountSortIdx(acct: TNNTPAccount; idx: Integer);
var
  i: Integer;
  a: TNNTPAccount;
begin
  if idx = acct.fSortIdx then Exit;

  i := Count - 1;
  while i >= 0 do
  begin
    a := Items[i];
    if a = acct then
      a.fSortIdx := idx
    else
      if a.fSortIdx <> -1 then
        if a.fSortIdx >= idx then
          Inc(a.fSortIdx);
    Dec(i)
  end;

  SortAccounts;

  for i := 0 to Count - 1 do
    Items[i].fSortIdx := i;
end;

function CompareAccounts(List: TStringList; Index1, Index2: Integer): Integer;
var
  acc1, acc2: TNNTPAccount;
begin
  acc1 := TNNTPAccount(List.Objects[Index1]);
  acc2 := TNNTPAccount(List.Objects[Index2]);

  Result := acc1.fSortIdx - acc2.fSortIdx;
end;

procedure TNNTPAccounts.SortAccounts;
var
  idx: Integer;
begin
  fAccounts.CustomSort(CompareAccounts);
  for idx := 0 to fAccounts.Count - 1 do
    TNNTPAccount(fAccounts.Objects[idx]).fSortIdx := idx;
end;

{ TDontUnloadCache }

function TDontUnloadCache.CanRemove(Obj: TObject): Boolean;
var
  group: TSubscribedGroup;
begin
  if fAlwaysRemove then
  begin
    Result := True;
    Exit;
  end;

  try
    if (Obj <> nil) and (Obj is TSubscribedGroup) then
    begin
      group := TSubscribedGroup(Obj);
                                        // An article body is being downloaded
      Result := not ThreadManager.GettingArticle(group, nil);
    end
    else
      Result := True;
  except
    Result := True;
  end;
end;

procedure TDontUnloadCache.Clear;
begin
  fAlwaysRemove := True;
  try
    inherited Clear;
  finally
    fAlwaysRemove := False;
  end;
end;

procedure TDontUnloadCache.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if not ReOrdering and (TObject(Ptr) is TSubscribedGroup) then
    if Action = lnDeleted then
      TSubscribedGroup(Ptr).CloseMessageFile;
  inherited;
end;

initialization
  LSSync := TCriticalSection.Create;
  MessageCacheController := TNNTPMessageCacheController.Create(5, False); // Cache the last 5 message bodies

  DontUnloadCache := TDontUnloadCache.Create(3, False);    // Cache the last 3 newsgrops

  gXanaNewsDir := ExtractFilePath(paramStr(0));
  if gXanaNewsDir[Length(gXanaNewsDir)] = '\' then
    gXanaNewsDir := Copy(gXanaNewsDir, 1, Length(gXanaNewsDir) - 1);

  SetLength(gMessageBaseRoot, MAX_PATH);
  if SUCCEEDED(SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, PChar(gMessageBaseRoot))) then
  begin
    gMessageBaseRoot := PChar(gMessageBaseRoot);
    gMessageBaseRoot := gMessageBaseRoot + '\Woozle\XanaNews';
  end
  else
    gMessageBaseRoot := gXanaNewsDir;

  gBestmessageBaseLocation := gMessageBaseRoot;
  gKeyName := '\software\Woozle\XanaNews';
finalization
  MessageCacheController.Free;
  DontUnloadCache.Free;
  FreeAndNil(LSSync);
end.
