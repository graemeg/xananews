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

uses Windows, Classes, SysUtils, Forms, Dialogs, Controls, Contnrs, idGlobal, unitMessages,
     StrUtils, unitNNTPFilters, NewsGlobals, unitObjectCache, unitIdentities,
     unitSearchString, unitSettings, unitBatches, unitExSettings, unitStreamTextReader, Shfolder, ShellAPI, SyncObjs;

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

  fgKeyPhrase0            = $1000;
  fgKeyPhrase1            = $2000;
  fgKeyPhrase2            = $4000;
  fgKeyPhrase3            = $8000;
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

//-----------------------------------------------------------------------
// Type definitions

TNNTPAccounts = class;
TNNTPAccount = class;
TSubscribedGroup = class;
TArticleContainer = class;


TMultipartFlags = (mfNotMultipart, mfPartialMultipart, mfCompleteMultipart);

//-----------------------------------------------------------------------
// TArticleBase class.  Base class for TArticle &  TFolderArticle

TArticleBase = class
private
  fOwner : TArticleContainer;
  fFromNameDecoded : boolean;

  fFromName : string;
  fFromEMail : string;

  fChild : TArticleBase;            // Thread child
  fParent : TArticleBase;           // Thread parent
  fSibling : TArticleBase;
  fBlocked : boolean;

  function GetFromEmail: string;
  function GetFromName: string;
  function GetHasAttachment: boolean;
  function GetIsDeleted: boolean;
  function GetIsDormant: boolean;
  function GetIsInteresting: boolean;
  function GetIsMine: boolean;
  function GetIsOdd: boolean;
  function GetIsXanaNews: boolean;
  procedure SetIsInteresting(const Value: boolean); virtual;
  procedure SetIsMine(const Value: boolean);
  procedure SetIsRead(const Value: boolean); virtual;
  function GetIsCancelled: boolean;
  procedure SetIsCancelled(const Value: boolean);
  function GetIsFromBozo: boolean;
  function GetIsReply: boolean;
  procedure SetIsReply(const Value: boolean);
  function GetIsIgnore: boolean;
  procedure SetIsIgnore(const Value: boolean);
  function DecodeMultipartSubject (var s : string; var n, x : Integer; fixIt, blankIt : boolean) : boolean;
  function GetIsNotOnServer: boolean;
  procedure SetIsNotOnServer(const Value: boolean);
  function GetSubjectIsReply: boolean;
  function GetSimplifiedSubject: string;
  function GetMessagesInThread: Integer;
  function GetUnreadMessagesInThread: Integer;
  function GetHasNoReplies: boolean;
  function GetIsFromMe: Boolean;
protected
  fMsg : TmvMessage;
  fBytes: Integer;
  fReferences: string;
  fLines: Integer;
  fFrom: string;
  fSubject: string;
  fDate: TDateTime;
  fTo : string;
  fMessageOffset : DWORD;       // Offset of message body in messages.dat
  fMultipartFlags: TMultipartFlags;
  fFlags : DWORD;             // See fgXXXX constants above
  fArticleNo: Integer;          // 0 = Dummy article
  fNewestMessage : TDateTime;

  function GetIsRead: boolean; virtual;
  function GetHeader(const name: string): string; virtual;
  procedure DecodeFromName;
  function GetMsg: TmvMessage; virtual; abstract;
  procedure SetMsg(const Value: TmvMessage); virtual;
  function GetIndex : Integer; virtual;
  function GetInterestingMessageLine: string; virtual;
  function GetPostingHost: string; virtual;
  function GetCodePage: Integer; virtual;
  procedure SetCodePage(const Value: Integer); virtual;
  function PeekAtMsgHdr (const hdr : string) : string; virtual;
  procedure SetFlag (flag : DWORD; value : boolean);
  function GetNext: TArticleBase; virtual;
  function GetMsgFromFile : TmvMessage;
  function GetCodePageFromFile : Integer;
  function PeekAtMsgHdrFromFile (const hdr : string) : string;
  procedure SetIsDeleted(const Value: boolean); virtual;
  function GetUniqueID: string; virtual;
  function GetCrosspostedTo: Integer;
public
  fCodePage : Integer;
  fMessageID: string;
  constructor Create (AOwner : TArticleContainer); virtual;
  procedure Assign (article : TArticleBase); virtual;
  function Equals (art : TArticleBase) : boolean; virtual;
  procedure Initialize (articleNo : Integer; header : TStrings);
  function HasMsg : boolean;
  function MsgDownloading : boolean;
  procedure ReleaseMsg;

  property Subject : string read fSubject write fSubject;
  property SubjectIsReply : boolean read GetSubjectIsReply;
  property SimplifiedSubject : string read GetSimplifiedSubject;
  property From : string read fFrom;
  property Date : TDateTime read fDate;
  property MessageId : string read fMessageID;
  property UniqueID : string read GetUniqueID;
  property References : string read fReferences;
  property _To : string read fTo;
  property Bytes : Integer read fBytes;
  property Lines : Integer read fLines;
  property ArticleNo : Integer read fArticleNo;

  property Msg : TmvMessage read GetMsg write SetMsg;
  property InterestingMessageLine : string read GetInterestingMessageLine;
  property PostingHost : string read GetPostingHost;
  property FromName : string read GetFromName;
  property FromEmail : string read GetFromEmail;
  property CodePage : Integer read GetCodePage write SetCodePage;
  property Flags : DWORD read fFlags;
  property MultipartFlags : TMultipartFlags read fMultipartFlags;
  property HasAttachment : boolean read GetHasAttachment;

  property Owner : TArticleContainer read fOwner;
  property Child : TArticleBase read fChild;
  property Parent : TArticleBase read fParent;
  property Sibling : TArticleBase read fSibling;
  property Next : TArticleBase read GetNext;

  property IsDeleted : boolean read GetIsDeleted write SetIsDeleted;
  property IsNotOnServer : boolean read GetIsNotOnServer write SetIsNotOnServer;
  property IsCancelled : boolean read GetIsCancelled write SetIsCancelled;
  property IsRead : boolean read GetIsRead write SetIsRead;
  property IsIgnore : boolean read GetIsIgnore write SetIsIgnore;
  property IsMine : boolean read GetIsMine write SetIsMine;
  property IsReply : boolean read GetIsReply write SetIsReply;
  property IsXanaNews : boolean read GetIsXanaNews;
  property IsOdd : boolean read GetIsOdd;
  property IsDormant : boolean read GetIsDormant;
  property IsInteresting : boolean read GetIsInteresting write SetIsInteresting;
  property IsFromBozo : boolean read GetIsFromBozo;
  property HasNoReplies : boolean read GetHasNoReplies;
  property Index : Integer read GetIndex;
  property Header [const name : string] : string read GetHeader;
  property CrosspostedTo : Integer read GetCrosspostedTo;
  property MessagesInThread : Integer read GetMessagesInThread;
  property UnreadMessagesInThread : Integer read GetUnreadMessagesInThread;
  property IsFromMe: Boolean read GetIsFromMe;
end;

//-----------------------------------------------------------------------
// TArticle class

TArticle = class (TArticleBase)
private
  fPartNo : Integer;

  fPostingHost : string;

  fInterestingMessageLine : string;
  fTempExtraHeaders : string;

  function GetKeyPhraseNo: Integer;
  function GetHasKeyPhrase(idx: Integer): boolean;
  function GetAccount: TNNTPAccount;
  function GetSubscribedGroup: TSubscribedGroup;
  procedure SetCrossPostsFlag(flag: DWORD; value: boolean);
  function GetHasAnyKeyPhrase: boolean;
  function MatchesKeyPhrase(st: string; searcher: TStringSearcher): boolean;
protected
  function GetMsg: TmvMessage; override;
  procedure SetMsg(const Value: TmvMessage); override;
  function GetInterestingMessageLine: string; override;
  function GetPostingHost: string; override;
  function GetCodePage: Integer; override;
  procedure SetCodePage(const Value: Integer); override;
  function PeekAtMsgHdr(const hdr: string): string; override;
  procedure SetIsRead(const Value: boolean); override;
  function GetHeader(const name: string): string; override;
public
  constructor Create (AOwner : TArticleContainer); override;
  destructor Destroy; override;
  procedure ChangeArticleNo (newArticleNo : Integer);
  procedure FixArticleNo;

  procedure RemoveMessage;
  procedure SaveMessageBody;
  procedure RawMarkAsRead;

  property SubscribedGroup : TSubscribedGroup read GetSubscribedGroup;
  property Account : TNNTPAccount read GetAccount;

  property KeyPhraseNo : Integer read GetKeyPhraseNo;
  property HasKeyPhrase [idx : Integer] : boolean read GetHasKeyPhrase;
  property HasAnyKeyPhrase : boolean read GetHasAnyKeyPhrase;
  property TempExtraHeaders : string read fTempExtraHeaders;
end;

//-----------------------------------------------------------------------
// Used in both NNTPAccounts and subscribed groups, because you
// are allowed to set filters for both individual subscribed groups, and
// whole accounts
TFiltersCtnr = class
private
  fOwner : TObject;
  fFilters : TNNTPFilters;
  fParent : TFiltersCtnr;
  function GetHasFilters: boolean;
  function GetCount: Integer;
public
  constructor Create (AOwner : TObject; AParent : TFiltersCtnr);
  destructor Destroy; override;
  property Filters : TNNTPFilters read fFilters;
  procedure EnableFilter (filter : TNNTPFilter; enable : boolean);
  function FilterEnabled(filter: TNNTPFilter): boolean;
  procedure LoadFilters (reg : TExSettings; displayFilters : boolean);
  procedure SaveFilters (reg : TExSettings; displayFilters : boolean);
  procedure AssignFilters (ctnr : TFiltersCtnr);
  function BlockArticle (article : TArticleBase) : boolean;
  procedure Clear;
  property Parent : TFiltersCtnr read fParent;
  property HasFilters : boolean read GetHasFilters;
  property Count : Integer read GetCount;
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
  constructor Create (const AName : string);
  property Name : string read fName write SetName;
  property Identity : TIdentity read GetIdentity;
  property ServerSettings : TServerSettings read GetServerSettings;
  property PostingSettings : TPostingSettings read GetPostingSettings;
end;

//--------------------------------------------------------------------------
// Base class for things that contain articles - TSubscribedGroup & TArticleFolder

TArticleContainer = class (TServerAccount)
private
  fFiltersCtnr: TFiltersCtnr;
  fDisplayFiltersCtnr : TFiltersCtnr;

  fSortBuf : TList;
  fDisplayFiltered : boolean;
  fHideReadMessages : boolean;
  fHideIgnoredMessages : boolean;

  fSecret : boolean;
  fHideMessagesNotToMe: boolean;

  function FindChild (article, child : TArticleBase) : boolean;
  procedure PruneDummyArticles (article : TArticleBase);
  procedure GatherSubjects (article : TArticleBase);
  procedure SortSiblings (var root : TArticleBase);
  procedure ThreadArticles;
  function GetFirstArticle: TArticleBase;

  procedure SetThreadOrder(const Value: TThreadOrder);
  procedure SetThreadSortDirection(const Value: TThreadSortDirection);
  procedure SetThreadSortOrder(const Value: TThreadSortOrder);


  function GetUnreadArticleToMeCount: Integer;
  function GetUnreadReplyCount: Integer;
  function GetUnreadXanaNewsArticleCount: Integer;
  function GetInterestingArticleCount : Integer;
  procedure SetHideReadMessages(const Value: boolean);
  function GetUnreadInterestingArticleCount: Integer;
  procedure SetHideIgnoredMessages(const Value: boolean);
  procedure SetHideMessagesNotToMe(const Value: boolean);
protected
  fThreads : TList;                     // List of TArticleBase thread roots
  fUnreadArticleCount : Integer;
  fUnreadArticleToMeCount : Integer;
  fUnreadReplyCount : Integer;
  fUnloadedArticleCount : Integer;
  fUnreadXananewsArticleCount : Integer;
  fInterestingArticleCount : Integer;
  fUnreadInterestingArticleCount : Integer;
  fCursorArticleId: string;
  fFlagsDirty : boolean;
  fMessageFile : TFileStream;
  fAdjustedArticleCount : Integer;

  fThreadOrder: TThreadOrder;           // Threaded or Chronological
  fThreadSortOrder: TThreadSortOrder;   // Sort by author, subject, date, lines
  fThreadSortDirection : TThreadSortDirection;

  fCurrentThreadOrder : TThreadOrder;
  fCurrentThreadSortOrder : TThreadSortOrder;
  fCurrentThreadSortDirection : TThreadSortDirection;
  fLockCount : Integer;

  procedure ClearThreadInfo;
  function GetDisplaySettings: TDisplaySettings; virtual;
  function GetMessagebaseSize: Int64; virtual;

  function GetArticleBase(idx: Integer): TArticleBase; virtual; abstract;
  function GetArticleCount: Integer; virtual; abstract;              // Object list of TArticle
  function GetThreadCount : Integer; virtual;
  function GetThreads(idx: Integer): TArticleBase; virtual;
  procedure GroupArticles; virtual;
  function GetLoaded: boolean; virtual; abstract;
  function GetUnreadArticleCount: Integer; virtual; abstract;
  function GetAdjustedArticleCount: Integer; virtual;

  procedure RawSortArticles; virtual;
  procedure SortArticles; virtual;
  procedure SortThreads; virtual;
  procedure SetSecret(const Value: boolean); virtual;

  function GetNext : TArticleContainer; virtual; abstract;
  procedure SaveArticleCounts (reg : TExSettings=nil); virtual;
  procedure LoadArticleCounts (reg : TExSettings); virtual;

public
  fFocused : boolean;
  fNeedsPurge : boolean;
  fSearching : boolean;
  fSorting : boolean;

  constructor Create (const AName : string; AFiltersParent, ADisplayFiltersParent : TFiltersCtnr);
  destructor Destroy; override;

  procedure BeginLock;
  procedure EndLock;
  function Locked : boolean;

  function MarkOnLeave : boolean; virtual;
  function FindMsgID (const msgID : string) : TArticleBase;
  function FindUniqueID (const msgID : string) : TArticleBase; virtual;

  function FindArticleNo (cno : Integer) : TArticleBase; virtual;
  function LoadGetArticleCount : Integer;
  procedure LoadArticles; virtual; abstract;
  procedure OpenMessageFile; virtual;
  property Loaded : boolean read GetLoaded;
  procedure ReSortArticles;
  procedure ResetSortFlags;
  procedure ResetBozoFlags;
  procedure LeaveGroup(clearMessages : boolean = True); virtual;

  procedure RawDeleteArticle (cno : Integer); virtual; abstract;
  procedure RawRemoveArticle (article : TArticleBase); virtual; abstract;
  procedure RawAddArticle (article : TarticleBase); virtual; abstract;

  function IndexOf (article : TArticleBase) : Integer; virtual;

  property ArticleCount : Integer read GetArticleCount;
  property AdjustedArticleCount : Integer read GetAdjustedArticleCount;
  property ArticleBase [idx : Integer] : TArticleBase read GetArticleBase;
  property CursorArticleID : string read fCursorArticleId write fCursorArticleId;
  property FlagsDirty : boolean read fFlagsDirty;
  property FiltersCtnr : TFiltersCtnr read fFiltersCtnr;
  property DisplayFiltersCtnr : TFiltersCtnr read fDisplayFiltersCtnr;
  property FirstArticle : TArticleBase read GetFirstArticle;

  property MessageFile : TFileStream read fMessageFile;
  property DisplaySettings : TDisplaySettings read GetDisplaySettings;
  property ThreadCount : Integer read GetThreadCount;
  property ThreadOrder : TThreadOrder read fThreadOrder write SetThreadOrder;
  property Threads [idx : Integer] : TArticleBase read GetThreads;
  property ThreadSortOrder : TThreadSortOrder read fThreadSortOrder write SetThreadSortOrder;
  property ThreadSortDirection : TThreadSortDirection read fThreadSortDirection write SetThreadSortDirection;
  property UnreadArticleCount : Integer read GetUnreadArticleCount;
  property InterestingArticleCount : Integer read GetInterestingArticleCount;
  property UnreadInterestingArticleCount : Integer read GetUnreadInterestingArticleCount;
  property UnreadArticleToMeCount : Integer read GetUnreadArticleToMeCount;
  property UnreadReplyCount : Integer read GetUnreadReplyCount;
  property UnreadXanaNewsArticleCount : Integer read GetUnreadXanaNewsArticleCount;
  property Next : TArticleContainer read GetNext;
  property HideReadMessages : boolean read fHideReadMessages write SetHideReadMessages;
  property HideMessagesNotToMe : boolean read fHideMessagesNotToMe write SetHideMessagesNotToMe;
  property HideIgnoredMessages : boolean read fHideIgnoredMessages write SetHideIgnoredMessages;
  property MessagebaseSize : Int64 read GetMessagebaseSize;
  property Secret : boolean read fSecret write SetSecret;
end;

//-----------------------------------------------------------------------
// TArticleObjectContainer - base class for things that store articles as
// a TObjectList - eg. news messages, but not archived messages
TArticleObjectContainer = class (TArticleContainer)
protected
  fArticles : TObjectList;
  fArticlesLoaded : boolean;
  procedure RawClearArticles;
  procedure RawSortArticles; override;
  function GetLoaded: boolean; override;
  procedure UnloadArticles; virtual;
public
  constructor Create (const AName : string; AFiltersParent, ADisplayFiltersParent : TFiltersCtnr);
  destructor Destroy; override;
  function FindArticleNo (cno : Integer) : TArticleBase; override;
  function GetArticleBase(idx: Integer): TArticleBase; override;
  function GetArticleCount: Integer; override;
  function GetThreadCount: Integer; override;
  function GetThreads(idx: Integer) : TArticleBase; override;
  function IndexOf (article : TArticleBase) : Integer; override;
  procedure PurgeArticles (all, reset : boolean; const folderName : string);

  procedure RawAddArticle (article : TArticleBase); override;
  procedure RawDeleteArticle (cno : Integer); override;
  procedure RawRemoveArticle (article : TArticleBase); override;

  procedure SaveArticles (recreateMessageFile : boolean); virtual; abstract;
//  property ArticlesLoaded : boolean read fArticlesLoaded;
end;

//-----------------------------------------------------------------------
// TSubscribedGroup class
TSubscribedGroup = class (TArticleObjectContainer)
private
  fOwner : TNNTPAccount;
  fHighWaterMark : Integer;
  fNNTPSettings: TNNTPSettings;
  fPostingSettings: TPostingSettings;
  fDisplaySettings: TDisplaySettings;
  fNickname : string;
  fNeedsUpdate: boolean;
  fActionPerformedThisSession: boolean;
  fSortIdx: Integer;

  function GetLastArticle: Integer;
  function GetLowestArticleNo : Integer;
  function GetArticle(idx: Integer): TArticle;
  function GetHighestArticleNo: Integer;
  procedure SetSortIdx(const Value: Integer);
  procedure QuickLoadHeaderStats;
protected
  function GetServerSettings : TServerSettings; override;
  function GetPostingSettings : TPostingSettings; override;
  function GetDisplaySettings: TDisplaySettings; override;
  function GetIdentity: TIdentity; override;
  procedure GroupArticles; override;
  function GetNext : TArticleContainer; override;
  function GetMessagebaseSize: Int64; override;
  procedure SetSecret(const Value: boolean); override;
  procedure SaveArticleCounts (reg : TExSettings=nil); override;
  procedure LoadArticleCounts (reg : TExSettings); override;
  function GetUnreadArticleCount: Integer; override;
  procedure UnloadArticles; override;

public
  fRawLastArticle : Integer;

  constructor Create (AOwner : TNNTPAccount; const AGroupName : string);
  destructor Destroy; override;
  procedure LoadArticles; override;
  procedure LoadUnreadArticleCount;

  procedure AddRawHeaders (headers : TStringList; XOVERFmt : TStringList); overload;
  procedure AddRawHeaders (headers : TTextFileReader); overload;
  function AddArticle (articleNo : Integer; header : TStrings; body : TStream; isNew : boolean) : TArticle;
  procedure SaveArticles (recreateMessageFile : boolean); override;
  procedure LeaveGroup(clearMessages : boolean = True); override;          // Save articles, and release memory
  function MarkOnLeave : boolean; override;
  function CreateGroupRegistry (access : DWORD): TExSettings;
  procedure OpenMessageFile; override;
  function TSGetLastArticle : Integer;

  property Articles [idx : Integer] : TArticle read GetArticle;
  property SortIdx : Integer read fSortIdx write SetSortIdx;
  property Owner : TNNTPAccount read fOwner;
  property LastArticle : Integer read GetLastArticle;
  property LowestArticleNo : Integer read GetLowestArticleNo;
  property HighestArticleNo : Integer read GetHighestArticleNo;
  property NNTPSettings : TNNTPSettings read fNNTPSettings;
  property Nickname : string read fNickname write fNickname;
  property NeedsUpdate : boolean read fNeedsUpdate write fNeedsUpdate;
  property HighWaterMark : Integer read fHighWaterMark write fHighWaterMark;
  property ActionPerformedThisSession : boolean read fActionPerformedThisSession write fActionPerformedThisSession;
end;

//-----------------------------------------------------------------------
// TNNTPAccount class
TNNTPAccount = class
private
  fAccountName: string;

  fSubscribedGroups : TStringList;
  fOwner : TNNTPAccounts;
  fXOverFMT : TStringList;
  fNoXNews: boolean;

  fMarkOnLeave: boolean;
  fScanKeyPhrases: boolean;
  fCheckedNewGroups : boolean;
  fFiltersCtnr: TFiltersCtnr;
  fDisplayFiltersCtnr : TFiltersCtnr;
  fNNTPSettings: TNNTPSettings;

  fPostingSettings: TPostingSettings;
  fDisplaySettings: TDisplaySettings;
  fNNTPServerSettings : TNNTPServerSettings;
  fMailAccountName: string;
  fHasNewGroups: boolean;
  fUsePipelining: boolean;
  fGreeting: string;
  fSecret : boolean;
  fSecretGroupCount : Integer;
  fPostingAccountName: string;
  fSortIdx: Integer;
  fSortGroupsByName: Boolean;

  procedure LoadSubscribedGroups (rootReg : TExSettings);
  procedure SaveSubscribedGroups (rootReg : TExSettings);
  function GetSubscribedGroup(idx: Integer): TSubscribedGroup;
  function GetSubscribedGroupCount: Integer;
  function GetXOverFMT: TStringList;
  procedure ResetKeyPhraseFlags (idx : Integer);
  procedure SetScanKeyPhrases(const Value: boolean);
  function GetNext: TNNTPAccount;
  procedure SetAccountName(const Value: string);
  function GetFileName: string;
  procedure SetGreeting(const Value: string);
  procedure SetSecret(const Value: boolean);
  procedure SetSortIdx(const Value: Integer);
  procedure SortGroups;
  procedure ChangeGroupSortIdx (grp : TSubscribedGroup; idx : Integer);
  procedure SetSortGroupsByName(const Value: Boolean);
public
  constructor Create (AOwner : TNNTPAccounts);
  destructor Destroy; override;

  procedure SetXOverFMT (s : TStringList);

  function IsSubscribedTo (const groupName : string) : boolean;
  function SubscribeTo (const groupName : string) : TSubscribedGroup;
  procedure UnsubscribeTo (const groupName : string);
  function FindSubscribedGroup (const groupName : string) : TSubscribedGroup;
  procedure CopySettingsFrom (account : TNNTPAccount);
  function CreateAccountRegistry (access : DWORD): TExSettings;

  property AccountName : string read fAccountName write SetAccountName;
  property NoXNews : boolean read fNoXNews write fNoXNews;
  property UsePipelining : boolean read fUsePipelining write fUsePipelining;
  property MarkOnLeave : boolean read fMarkOnLeave write fMarkOnLeave;
  property Secret : boolean read fSecret write SetSecret;

  property Owner : TNNTPAccounts read fOwner;
  property SubscribedGroupCount : Integer read GetSubscribedGroupCount;
  property SubscribedGroups [idx : Integer] : TSubscribedGroup read GetSubscribedGroup;
  property XOverFMT : TStringList read GetXOverFMT write SetXOverFMT;

  property ScanKeyPhrases : boolean read fScanKeyPhrases write SetScanKeyPhrases;
  property Next : TNNTPAccount read GetNext;
  property CheckedNewGroups : boolean read fCheckedNewGroups write fCheckedNewGroups;
  property FileName : string read GetFileName;
  property SortIdx : Integer read fSortIdx write SetSortIdx;

  property NNTPSettings : TNNTPSettings read fNNTPSettings;
  property PostingSettings : TPostingSettings read fPostingSettings;
  property DisplaySettings : TDisplaySettings read fDisplaySettings;
  property FiltersCtnr : TFiltersCtnr read fFiltersCtnr;
  property DisplayFiltersCtnr : TFiltersCtnr read fDisplayFiltersCtnr;
  property NNTPServerSettings : TNNTPServerSettings read fNNTPServerSettings;
  property MailAccountName : string read fMailAccountName write fMailAccountName;
  property PostingAccountName : string read fPostingAccountName write fPostingAccountName;
  property HasNewGroups : boolean read fHasNewGroups write fHasNewGroups;
  property Greeting : string read fGreeting write SetGreeting;
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
  fName : string;
  fEMail : string;
  fBozodDate : TDateTime;
  fSearcher : TStringSearcher;
  fKeyPhrase : string;
  fFlags : TMatchBozoFlags;
  fAction : TBozoAction;
  property Searcher : TStringSearcher read GetSearcher;
public
  constructor Create (const AName, AEMail : string; ABozodDate : TDateTime; AAction : TBozoAction);
  constructor CreateNew;
  destructor Destroy; override;
  procedure Assign (b : TBozo);
  function MatchesArticle (art : TArticle) : boolean;

  property Name : string read fName write fName;
  property EMail : string read fEMail write fEMail;
  property BozodDate : TDateTime read fBozodDate write fBozodDate;
  property Flags : TMatchBozoFlags read fFlags write fFlags;
  property KeyPhrase : string read fKeyPhrase write fKeyPhrase;
  property Action : TBozoAction read fAction write fAction;
end;

TNextArticleOptions = set of TNextArticleOption;

//-----------------------------------------------------------------------
// TNNTPAccounts class
//
// Persists list of TNNTPAccount and their subscribed groups in registry

TNNTPAccounts = class
private
  fShowSecrets: boolean;
  function GetKeyPhraseSearcher(idx: Integer): TStringSearcher;
  procedure SetDoVersionCheck(const Value: Integer);
  procedure SetHideDormantConnections(const Value: boolean);
  procedure SetShowSecrets(const Value: boolean);
  function GetBozo(i : Integer): TBozo;
  function GetBozoCount: Integer;
  procedure ResetAllBozoFlags;
private
  fAccounts : TStringList;
  fBatches : TObjectList;
  fLastFoundGroup : TArticleContainer;
  fLastServerAccount : string;
  fLastServer : string;
  fIdentities : TIdentities;
  fKeyPhraseSearchers : array [0..8] of TStringSearcher;
  fNNTPSettings : TNNTPSettings;
  fPostingSettings : TPostingSettings;
  fDisplaySettings : TDisplaySettings;
  fDoVersionCheck : Integer;
  fHideDormantConnections: boolean;
  fBozos : TObjectList;
  fFiltersCtnr : TFiltersCtnr;
  fDisplayFiltersCtnr: TFiltersCtnr;
  fSecretAccountCount : Integer;

  function GetCount: Integer;
  function GetItems(idx: Integer): TNNTPAccount;
  function GetBatchesCount: Integer;
  function GetBatches(idx: Integer): TNNTPBatch;
  function GetFirstArticle: TArticle;
  procedure ConfirmMessagebaseRoot (rootReg : TExSettings);
  procedure ChangeAccountSortIdx (acct : TNNTPAccount; idx : Integer);

  property KeyPhraseSearcher [idx : Integer] : TStringSearcher read GetKeyPhraseSearcher;
public
  fNewUserFlag : boolean;
  constructor Create;
  destructor Destroy; override;

  procedure LoadFromRegistry;
  procedure SaveToRegistry (saveGroupsForAccount : TNNTPAccount = Nil);

  procedure Add (account : TNNTPAccount);
  procedure Delete (account : TNNTPAccount);
  procedure Rename (account : TNNTPAccount; const oldName : string);
  procedure UnloadOldContainers (exceptFor : TArticleObjectContainer);

  procedure AddBatch (batch : TNNTPBatch);
  procedure DeleteBatch (idx : Integer);
  function IndexOfBatch (batch : TNNTPBatch) : Integer;
  function FindMsgID (const accountName, groupName, MsgID : string) : TArticleBase;
  function FindXRef (const XRef : string) : TArticleBase;
  function NextArticle (options : TNextArticleOptions; firstArticle : TArticleBase) : TArticleBase;
  function GetServerAccountName (const serverName, groupName : string) : string;
  function FindServerAccount (const serverName : string; portNo : Integer; sslPort : Integer = -1) : TNNTPAccount;
  function FindArticleContainer (const accountName, groupName : string) : TArticleContainer;

  procedure PurgeOldArticles;
  procedure SaveDirtyArticles;

  procedure LoadBozoList;
  procedure SaveBozoList;
  procedure AddBozoMatching (art : TArticleBase);
  function ArticleIsFromBozo (article : TArticle; var action : TBozoAction) : boolean;
  procedure RemoveBozoMatching(art : TArticle);
  procedure RemoveOldBozos;
  procedure ReplaceBozos (newBozos : TObjectList);

  procedure ResetKeyPhraseFlags (idx : Integer);
  procedure PerfCue (freq : Integer; const st : string);
  procedure SortAccounts;

  property Count : Integer read GetCount;
  property Items [idx : Integer] : TNNTPAccount read GetItems; default;

  property BatchesCount : Integer read GetBatchesCount;
  property Batches [idx : Integer] : TNNTPBatch read GetBatches;
  property FirstArticle : TArticle read GetFirstArticle;
  property Identities : TIdentities read fIdentities;
  property PostingSettings : TPostingSettings read fPostingSettings;
  property DisplaySettings : TDisplaySettings read fDisplaySettings;
  property NNTPSettings : TNNTPSettings read fNNTPSettings;
  property DoVersionCheck : Integer read fDoVersionCheck write SetDoVersionCheck;
  property HideDormantConnections : boolean read fHideDormantConnections write SetHideDormantConnections;
  property FiltersCtnr : TFiltersCtnr read fFiltersCtnr;
  property DisplayFiltersCtnr : TFiltersCtnr read fDisplayFiltersCtnr;
  property ShowSecrets : boolean read fShowSecrets write SetShowSecrets;
  property BozoCount : Integer read GetBozoCount;
  property Bozo [i : Integer] : TBozo read GetBozo;
end;

//-----------------------------------------------------------------------
// TNNTPMessageCacheController class
//
// Controls how many messages are cached in memory
//
// Keeps a list of recent articles.  When an article drops of the list, it's
// 'Msg' is free-and-niled.  Next time the article is referenced, it's message
// will be reloaded from disk.

TNNTPMessageCacheController = class (TObjectCache)
private
  fAlwaysRemove: boolean;
protected
  function CanRemove (Obj : TObject) : boolean; override;
public
  property AlwaysRemove : boolean read fAlwaysRemove write fAlwaysRemove;
end;

//=======================================================================
// Article stack class for 'go to previous article'

TArticleStack = class
private
  fList : TStringList;
  function GetCapacity: Integer;
  function GetIsEmpty: boolean;

public
  constructor Create (ACapacity : Integer);
  destructor Destroy; override;
  procedure Push (article : TArticleBase);
  function Pop : TArticleBase;
  function Peek : TArticleBase;
  procedure Clear;
  property Capactity : Integer read GetCapacity;
  property IsEmpty : boolean read GetIsEmpty;
end;

//-----------------------------------------------------------------------
// Global variables

var
  NNTPAccounts : TNNTPAccounts;         // The list of accounts
  gXanaNewsDir : string;                // The root directory
  gMessageBaseRoot : string;
  gBestMessageBaseLocation : string;
  gKeyName : string;                    // Root registry key
  MessageCacheController : TNNTPMessageCacheController;
  DontUnloadCache : TObjectCache;       // Most recently access groups
  LSSync : TCriticalSection;            // Sync header file access
                               // nb.  Header file access is always
                               //      done from the main thread *except*
                               //      when getting the last article number.
                               //      - see TSGetLastArticle.  Therefore
                               //      all header file accesses must be
                               //      controlled by this crit sec.

const
  cFolders = 'Folders';

implementation

uses IdNNTPX, unitArticleHash, unitNewsReaderOptions, unitCharsetMap,
     unitSavedArticles, unitMailServices, unitNewUserWizard, unitLog, unitCheckVersion, unitNNTPThreadManager, DateUtils;


(*----------------------------------------------------------------------*
 | function ForceRenameFile : Boolean                                   |
 |                                                                      |
 | Renames a file                                                       |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   Source: string             File that should be renamed             |
 |   Dest  : string             Destination file name                   |
 |                                                                      |
 | The function returns true if the file was successfully renamed       |
 *----------------------------------------------------------------------*)
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

(*----------------------------------------------------------------------*
 | function CompareThreads : Integer                                    |
 |                                                                      |
 | Sort callback function.  Compare two articles.                       |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   p1 : pointer               First article to compare                |
 |   p2 : pointer               Second article to compare               |
 |                                                                      |
 | The function returns <0 if p1 < p2; 0 if p1 = p2; >0 if p1 > p2      |
 *----------------------------------------------------------------------*)
function CompareThreads (p1, p2 : pointer) : Integer;
var
  a1, a2 : TArticleBase;
  order : TThreadSortOrder;

  function CompareAuthor (a1, a2 : string) : Integer;
  begin
    if (Length (a1) <> 0) and (Length (a2) <> 0) then
    begin
      if a1 [1] in ['''', '"'] then
        Delete (a1, 1, 1);
      if a2 [1] in ['''', '"'] then
        Delete (a2, 1, 1);
    end;
    Result := CompareText (a1, a2);
  end;

  procedure Val (const s : string; var v, code : Integer);
  begin
    v := StrToIntDef (s, -1);
    if v = -1 then
      code := 1
  end;

  function CompareSubject (s1, s2 : string) : Integer;
  var
    n, x : Integer;
  begin
    a1.DecodeMultipartSubject (s1, n, x, True, False);
    a2.DecodeMultipartSubject (s2, n, x, True, False);
    result := CompareText (s1, s2);
  end;

  function CompareIPAddress (a1, a2 : string) : Integer;
  var
    s1, s2 : string;
    i1, i2, c1, c2 : Integer;
  begin
    repeat
      if (a1 = '') or (a2 = '') then
        result := CompareText (a1, a2)
      else
      begin
        s1 := SplitString ('.', a1);
        s2 := SplitString ('.', a2);

        Val (s1, i1, c1);
        Val (s2, i2, c2);

        if (c1 = 0) and (c2 = 0) then
          result := i1 - i2
        else
          result := CompareText (s1, s2)
      end
    until (result <> 0) or ((a1 = '') and (a2 = ''))

  end;

  function GetThreadNewestMessage (a : TArticleBase) : TDateTime;
  var
    dt : TDateTime;

    function GTNM (a : TArticleBase) : TDateTime;
    var
      dt : TDateTime;
    begin
      result := -1;
      while a <> Nil do
      begin
        dt := a.fDate;
        if dt > result then
          result := dt;

        dt := GTNM (a.fChild);
        if dt > result then
          result := dt;

        a := a.fSibling
      end
    end;

  begin
    dt := a.fDate;

    result := GTNM (a.fChild);
    if dt > result then
      result := dt;
  end;

begin { CompareThreads }
  result := 0;
  a1 := TArticleBase (p1);
  a2 := TArticleBase (p2);

  order := a1.Owner.ThreadSortOrder;

  case order of
    soDate : if a1.fDate < a2.fDate then
               result := -1
             else
               if a1.fDate > a2.fDate then
                 result := 1;
    soSubject : result := CompareSubject (a1.fSubject, a2.fSubject);
    soLines : if a1.fLines < a2.fLines then
                result := -1
              else
                if a1.fLines > a2.fLines then
                  result := 1;
    soAuthor : result := CompareAuthor (a1.FromName, a2.FromName);
    soMessageNo : if a1.fArticleNo < a2.fArticleNo then
                    result := -1
                  else
                    if a1.fArticleNo > a2.fArticleNo then
                      result := 1;
    soPostingHost : result := CompareIPAddress (a1.PostingHost, a2.PostingHost);

    soNewestMessage :
      begin
        if a1.fNewestMessage = -1 then
          a1.fNewestMessage := GetThreadNewestMessage (a1);

        if a2.fNewestMessage = -1 then
          a2.fNewestMessage := GetThreadNewestMessage (a2);

        if a1.fNewestMessage < a2.fNewestMessage then
          result := -1
        else
          if a1.fNewestMessage > a2.fNewestMessage then
            result := 1
      end
  end;

  if result = 0 then
    if a1.fArticleNo < a2.fArticleNo then
      result := -1
    else
      if a1.fArticleNo > a2.fArticleNo then
        result := 1;

  if a1.Owner.ThreadSortDirection = sdDescending then
    Result := -result
end;

(*----------------------------------------------------------------------*
 | procedure SetThreadFlags                                             |
 |                                                                      |
 | Set particular flags in all articles in a thread                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   root : TArticle            Root article of the thread.             |
 *----------------------------------------------------------------------*)
procedure SetThreadFlags (root : TArticleBase; flags : Cardinal);
begin
  while root <> nil do
  begin
    root.fFlags := root.fFlags or flags;
    SetThreadFlags (root.Child, flags);
    root := root.Sibling;
  end
end;

(*----------------------------------------------------------------------*
 | function GetNodeArticleCount : Integer                               |
 |                                                                      |
 | *** Diagnostic Function ***  Return the total count of articles in   |
 | a thread.                                                            |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   root : TArticle            Root of the thread to analyze           |
 |                                                                      |
 | The function returns the number of articles in the thread.           |
 *----------------------------------------------------------------------*)
function GetNodeArticleCount(root: TArticleBase): Integer;
var
  p : TArticleBase;
begin
  result := 1;

  p := root.Child;
  while p <> Nil do
  begin
    result := result + GetNodeArticleCount (p);
    p := p.Sibling
  end
end;

{ TNNTPAccounts }

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.Add                                          |
 |                                                                      |
 | Add an account to the Accounts list                                  |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   account : TNNTPAccount     The account to add.                     |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.Add(account: TNNTPAccount);
begin
  account.fOwner := self;
  account.fSortIdx := Count;
  fAccounts.AddObject(account.AccountName, account)
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.AddBatch                                     |
 |                                                                      |
 | Add a batch to the batches list                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   batch: TNNTPBatch          The batch to add                        |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.AddBatch(batch: TNNTPBatch);
begin
  fBatches.Add(batch)
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.AddBozoMatching                              |
 |                                                                      |
 | Add someone to the Bozo list                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   art : TArticleBase                                                 |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.AddBozoMatching(art : TArticleBase);
var
  nm, em : string;
begin
  nm := art.FromName;
  em := art.FromEmail;

  if (nm <> '') or (em <> '') then
  begin
    fBozos.Add(TBozo.Create(art.FromName, art.FromEMail, Now, Options.DefaultBozoAction));
    SaveBozoList
  end
end;

function TNNTPAccounts.ArticleIsFromBozo(article: TArticle; var action : TBozoAction): boolean;
var
  i : Integer;
  b : TBozo;
begin
  result := False;
  if article <> Nil then
    for i := 0 to BozoCount - 1 do
    begin
      b := Bozo [i];
      if b.MatchesArticle(article) then
      begin
        action := b.Action;
        result := True;
        break
      end
    end
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.ConfirmMessagebaseRoot                       |
 |                                                                      |
 | Sort out old & new message base locations.  The function fixes up    |
 | the gMessagebaseRoot variable which was initialized in the           |
 | initialization section.                                              |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.ConfirmMessagebaseRoot (rootReg : TExSettings);
begin
  if rootReg.HasValue('Messagebase Directory') then
    gMessagebaseRoot := ExpandFileName(rootReg.GetStringValue('Messagebase Directory', gMessagebaseRoot))
  else
  begin
            // But handle legacy entries too.

    if not rootReg.GetBooleanValue ('Messagebase Directory Confirmed', False) then
      gMessagebaseRoot := gXanaNewsDir
    else
      if rootReg.GetBooleanValue ('Use old directory structure', False) then
        gMessagebaseRoot := ExpandFileName(gXanaNewsDir);
    rootReg.SetStringValue('Messagebase Directory', gMessagebaseRoot, '')
  end
end;

(*----------------------------------------------------------------------*
 | constructor TNNTPAccounts.Create                                     |
 |                                                                      |
 | Constructor for TNNTPAccounts.  Create the accounts list.            |
 *----------------------------------------------------------------------*)
constructor TNNTPAccounts.Create;
begin
  fSecretAccountCount := -1;
  fAccounts := TStringList.Create;
  fBatches := TObjectList.Create;
  fIdentities := TIdentities.Create;
  fNNTPSettings := TNNTPSettings.Create (nil);
  fPostingSettings := TPostingSettings.Create (nil);
  fBozos := TObjectList.Create;
  fFiltersCtnr := TFiltersCtnr.Create(self, nil);
  fDisplayFiltersCtnr := TFiltersCtnr.Create(self, nil);
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.Delete                                       |
 |                                                                      |
 | Delete an account from the accounts list.                            |
 |                                                                      |
 | Parameters:                                                          |
 |   account: TNNTPAccount      The account to delete                   |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.Delete(account: TNNTPAccount);
var
 idx : Integer;
begin
  idx := fAccounts.IndexOfObject(account);
  if idx >= 0 then
  begin
    fAccounts.Delete(idx);
    for idx := 0 to Count - 1 do
      Items [idx].fSortIdx := idx
  end
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.DeleteBatch                                  |
 |                                                                      |
 | Delete a batch from the batches list                                 |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer       The index of the batch to delete                |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.DeleteBatch(idx: Integer);
begin
  fBatches.Delete(idx);
end;

(*----------------------------------------------------------------------*
 | destructor TNNTPAccounts.Destroy                                     |
 |                                                                      |
 | Destructor for TNNTPAccounts.  Clear the accounts list.              |
 *----------------------------------------------------------------------*)
destructor TNNTPAccounts.Destroy;
var
  i : Integer;
begin
  fSecretAccountCount := 0;
  for i := 0 to Count - 1 do
    Items [i].Free;

  for i := 0 to 7 do
    fKeyPhraseSearchers [i].Free;

  fAccounts.Free;
  fBatches.Free;
  fIdentities.Free;
  fNNTPSettings.Free;
  fPostingSettings.Free;
  fDisplaySettings.Free;
  fBozos.Free;
  fFiltersCtnr.Free;
  fDisplayFiltersCtnr.Free;
  inherited
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
 |   groupName : string         Name of the group to find or blank      |
 |                                                                      |
 | The function returns the TSubscribedGroup that matches the details   |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.FindArticleContainer (const accountName, groupName : string): TArticleContainer;
var
  i, j : Integer;
  account : TNNTPAccount;
  grp : TSubscribedGroup;

begin
        // Cache last result for speed.
  if Assigned (fLastFoundGroup) and (SameText (fLastFoundGroup.Name, groupName)) then
  begin
    if ((accountName = cFolders) and (fLastFoundGroup is TArticleFolder)) or
       ((fLastFoundGroup is TSubscribedGroup) and (TSubscribedGroup (fLastFoundGroup).Owner.AccountName = accountName)) then
    begin
      result := fLastFoundGroup;
      exit
    end
  end;

  result := Nil;        // Does the account name indicate archived message folders?
  if accountName = cFolders then
    result := gArticleFolders.FindFolder (groupName)
  else
    for i := 0 to Count - 1 do
    begin
      account := Items [i];

      // Find the matching entry

      if (accountName = '') or (CompareText (accountName, account.AccountName) = 0) then
      begin
        for j := 0 to account.SubscribedGroupCount - 1 do
        begin
          grp := account.SubscribedGroups [j];

          if (groupName = '') or (CompareText (groupName, grp.Name) = 0) then
          begin
            result := grp;
            break
          end
        end;

        if Assigned (result) or (accountName <> '') then
          break
      end
    end;

  if Assigned (result) then
    fLastFoundGroup := result
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
function TNNTPAccounts.FindMsgID(const accountName, groupName,
  MsgID: string): TArticleBase;
var
  grp : TArticleContainer;
  acct : TNNTPAccount;
  i, j : Integer;

begin
  result := Nil;
                        // Find the account/group first.  If the group is not
                        // specified the first group in the account will be returned.
                        // if the account's not specified the first group in the first
                        // account will be returned.
  grp := FindArticleContainer (accountName, groupName);

  if groupName <> '' then

  begin                 // They specifically asked for this group, so fail if it's
                        // not found in it.

    if Assigned (grp) then
      result := TArticle (grp.FindMsgID(MsgID))
  end
  else
    if (grp <> Nil) and (grp is TSubscribedGroup) then
    begin
      acct := TSubscribedGroup (grp).Owner;
      i := NNTPAccounts.fAccounts.IndexOf(acct.AccountName);

      while (i < NNTPAccounts.fAccounts.Count) and not Assigned (result) do
      begin
        acct := TNNTPAccount (NNTPAccounts.fAccounts.Objects [i]);
        j := 0;
        if fShowSecrets or not acct.Secret then
          while (j < acct.SubscribedGroupCount) and not Assigned (result) do
          begin
            grp := acct.SubscribedGroups [j];
            result := TArticle (grp.FindMsgID(MsgID));
            Inc (j)
          end;

        Inc (i);
        if AccountName <> '' then
                        // They specifically asked for this account, so fail if it's
                        // not there.
          break
      end
    end
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.FindServerAccount                             |
 |                                                                      |
 | Find an account by server name/port number.                          |
 |                                                                      |
 | Parameters:                                                          |
 |   const serverName: string; portNo : Integer                         |
 |                                                                      |
 | The function returns TNNTPAccount                                    |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.FindServerAccount(
  const serverName: string; portNo : Integer; sslPort : Integer): TNNTPAccount;
var
  i : Integer;
  acct : TNNTPAccount;
begin
  result := nil;

  for i := 0 to count - 1 do
  begin
    acct := Items [i];
    if sslPort <> -1 then
    begin
      if acct.NNTPServerSettings.SSLRequired and (acct.NNTPServerSettings.SSLPort = sslPort) and SameText (acct.NNTPServerSettings.ServerName, serverName) then
      begin
        result := acct;
        break
      end
    end
    else
      if (acct.NNTPServerSettings.ServerPort = portNo) and SameText (acct.NNTPServerSettings.ServerName, serverName) then
      begin
        result := acct;
        break
      end
  end
end;

function TNNTPAccounts.FindXRef(const XRef: string): TArticleBase;
var
  xr, accountName, serverName, groupName : string;
  artNo : Integer;
  ctnr : TArticleContainer;
begin
  result := Nil;
  xr := XRef;
  serverName := SplitString (' ', xr);
  groupName := SplitString (':', xr);
  accountName := GetServerAccountName (serverName, groupName);
  artNo := StrToIntDef (xr, -1);

  if artNo <> -1 then
  begin
    ctnr := FindArticleContainer (accountName, groupName);
    if Assigned (ctnr) then
      result := ctnr.FindArticleNo(artNo)
  end
end;

(*----------------------------------------------------------------------*
 | TNNTPAccounts.GetBatches                                             |
 |                                                                      |
 | 'Get' method for 'Batches' property                                  |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetBatches(idx: Integer): TNNTPBatch;
begin
  Result := TNNTPBatch (fBatches [idx])
end;

(*----------------------------------------------------------------------*
 | TNNTPAccounts.GetBatchesCount                                        |
 |                                                                      |
 | 'Get' method for 'BatchesCount' property                             |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetBatchesCount: Integer;
begin
  Result := fBatches.Count
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.GetCount : Integer                            |
 |                                                                      |
 | 'Get' method for Count property                                      |
 |                                                                      |
 | The function returns the number of accounts in the list              |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetBozoCount: Integer;
begin
  result := fBozos.Count
end;

function TNNTPAccounts.GetBozo(i : Integer) : TBozo;
begin
  result := TBozo (fBozos [i])
end;

function TNNTPAccounts.GetCount: Integer;
var
  i : Integer;
begin
  if fSecretAccountCount = -1 then
  begin
    fSecretAccountCount := 0;

    for i := 0 to fAccounts.Count - 1 do
    begin
      if not fShowSecrets and TNNTPAccount (fAccounts.Objects [i]).Secret then
        Inc (fSecretAccountCount);
      TNNTPAccount (fAccounts.Objects [i]).fSecretGroupCount := -1
    end
  end;

  result := fAccounts.Count - fSecretAccountCount;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.GetFirstArticle                               |
 |                                                                      |
 | Get the first article in the first subscribed group that has one     |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetFirstArticle: TArticle;
var
  i, j : Integer;
  grp : TSubscribedGroup;
begin
  result := Nil;
  for i := 0 to Count - 1 do
    for j := 0 to Items [i].SubscribedGroupCount - 1 do
    begin
      grp := Items [i].SubscribedGroups [j];
      if grp.LoadGetArticleCount > 0 then
      begin
        result := grp.Articles [0];
        exit
      end
    end
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.GetItems : TNNTPAccount                       |
 |                                                                      |
 | 'Get' Method for Items property                                      |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   idx : Integer              The account to get                      |
 |                                                                      |
 | The function returns the account at position 'idx'                   |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetItems(idx: Integer): TNNTPAccount;
var
  i : Integer;
begin
  if idx < Count then
    if fSecretAccountCount > 0 then
    begin
      result := Nil;
      for i := 0 to fAccounts.Count - 1 do
        if not TNNTPAccount (fAccounts.Objects [i]).Secret then
          if idx > 0 then
            Dec (idx)
          else
          begin
            result := TNNTPAccount (fAccounts.Objects [i]);
            break
          end
    end
    else
      result := fAccounts.Objects [idx] as TNNTPAccount
  else
    result := Nil
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
  if not Assigned (fKeyPhraseSearchers [idx]) then
    fKeyPhraseSearchers [idx] := TGoogleLikeStringSearcher.Create ('', False);

  result := fKeyPhraseSearchers [idx];
end;

(*----------------------------------------------------------------------*
 | TNNTPAccounts.GetServerAccountName                                   |
 |                                                                      |
 | Get the account name for the specified server name.                  |
 |                                                                      |
 | Parameters:                                                          |
 |   const serverName: string   // The server name to find              |
 |                                                                      |
 | The function returns the account name of the first account that uses |
 | the specified server                                                 |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.GetServerAccountName(const serverName, groupName: string): string;
var
  i, port : Integer;
  sn, st : string;
begin
  if serverName = fLastServer then
    result := fLastServerAccount
  else
  begin
    fLastServer := '';
    fLastServerAccount := '';
    port := 119;
    i := Pos ('(', serverName);
    if i > 0 then
    begin
      sn := Copy (serverName, 1, i - 1);
      st := Copy (serverName, i + 1, MaxInt);
      i := Pos (st, ')');
      if i > 0 then
        st := Copy (st, 1, i - 1);

      port := StrToIntDef (st, -1)
    end
    else
      sn := ServerName;
    result := sn;
    for i := 0 to Count - 1 do
      if (CompareText (Items [i].NNTPServerSettings.ServerName, sn) = 0) and
         ((port = -1) or (port = Items [i].NNTPServerSettings.ServerPort)) and
         ((groupName = '') or (groupName = '~XNS') or (Items [i].FindSubscribedGroup (groupName) <> Nil)) then
      begin
        result := Items [i].AccountName;
        fLastServerAccount := result;
        fLastServer := serverName;
        break
      end
  end
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.IndexOfBatch                                  |
 |                                                                      |
 | Returns the index of the specified batch in the batches container    |
 |                                                                      |
 | Parameters:                                                          |
 |   batch: TNNTPBatch          // The batch to find                    |
 |                                                                      |
 | The function returns the batches index                               |
 *----------------------------------------------------------------------*)
function TNNTPAccounts.IndexOfBatch(batch: TNNTPBatch): Integer;
begin
  Result := fBatches.IndexOf(batch)
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.LoadBozoList                                 |
 |                                                                      |
 | Load the bozos list.                                                 |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.LoadBozoList;
var
  sl : TStringList;
  i : Integer;
  st : string;
  name, email, flagss : string;
  dt : TDateTime;
  bozo : TBozo;

  function StrToBozoFlags (const st : string) : TMatchBozoFlags;
  var
    i : Integer;
    f : TMatchBozoFlag;
  begin
    i := 1;
    result := [];
    for f := Low (TMatchBozoFlag) to High (TMatchBozoFlag) do
      if i <= Length (st) then
      begin
        if st [i] = '1' then
          Include (result, f);
        Inc (i)
      end
      else
        break
  end;

begin
  if FileExists (gMessageBaseRoot + '\bozos.txt') then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(gMessageBaseRoot + '\bozos.txt');

      for i := 0 to sl.Count - 1 do
      begin
        st := sl [i];
        name := SplitString (':', st);
        email := SplitString (':', st);
        dt := StrToDateTimeDef (SplitString ('~', st), Now);

        bozo := TBozo.Create(name, email, dt, baIgnore);
        fBozos.Add(bozo);

        if st <> '' then
        begin
          flagss := SplitString ('~', st);
          bozo.fFlags := StrToBozoFlags (flagss);
          if st <> '' then
          begin
            flagss := SplitString ('~', st);
            bozo.fAction := TBozoAction (StrToIntDef (flagss, 0));
            bozo.fKeyPhrase := st
          end
        end
      end
    finally
      sl.Free
    end
  end
  else
    fBozos.Clear;
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.LoadFromRegistry                             |
 |                                                                      |
 | Load the accounts & their subscribed groups from the registry        |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const keyName : string     The registry key in HKCU to load from   |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.LoadFromRegistry;
var
  reg, reg1, reg2 : TExSettings;
  keyNames, keyNames1 : TStringList;
  i, j, p : Integer;

  account : TNNTPAccount;
  batch : TNNTPBatch;
  actionName : string;
  Action : TBatchAction;

begin
  keyNames := Nil;
  keyNames1 := nil;
  reg1 := Nil;
  reg2 := nil;
  reg := CreateExSettings;
  try
    if not reg.HasSection ('') then
    begin
      fNewUserFlag := True;
      if not DoNewUserConfig (gKeyName) then
        Halt
    end
    else
      ConfirmMessagebaseRoot (reg);

    InitializeFolders (reg);
    fDisplaySettings := TDisplaySettings.Create (nil);
    LoadBozoList;
    fIdentities.Load (reg);

    fDoVersionCheck := reg.GetIntegerValue ('HTTP Permission Required', 0);
    fHideDormantConnections := reg.GetBooleanValue('Hide Dormant Connections', True);

                                // Load the global settings
    if reg.HasSection ('Globals') then
    begin
      reg.Section := 'Globals';
      try
        fNNTPSettings.ReadSettings(reg);
        fPostingSettings.ReadSettings(reg);
        fDisplaySettings.ReadSettings(reg);
        FiltersCtnr.LoadFilters(reg, false);
        DisplayFiltersCtnr.LoadFilters(reg, True)
      finally
        reg.Section := ''
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
      fPostingSettings.DefaultCodePage := DefaultCodePage
    end;

    if reg.HasSection ('Accounts') then
    begin
      reg.Section := 'Accounts';

      try
        keyNames := TStringList.Create;
        reg1 := CreateChildSettings (reg);
        reg.GetSectionNames(keyNames);

        for i := 0 to keyNames.Count - 1 do

        begin                     // For each account...

          reg1.Section := keyNames [i];

          account := TNNTPAccount.Create (self);
          try
            account.fAccountName := reg1.GetStringValue ('Account Name', keyNames [i]);
            account.fSortIdx := reg1.GetIntegerValue ('Sort Index', i);
            account.NNTPServerSettings.Id := account.fAccountName;
            Account.MarkOnLeave       := reg1.GetBooleanValue('Mark On Leave', False);
            account.fScanKeyPhrases   := reg1.GetBooleanValue('Scan Key Phrases', account.fScanKeyPhrases);

            account.NoXNews           := reg1.GetBooleanValue('No XNEWS', account.NOXNews);
            account.UsePipelining     := reg1.GetBooleanValue('Pipelining', account.UsePipelining);
            account.fGreeting         := reg1.GetStringValue ('Greeting', '');
            account.fSecret           := reg1.GetBooleanValue('Secret', False);
            account.fSortGroupsByName := reg1.GetBooleanValue('SortGroupsByName', False);

            account.FiltersCtnr.LoadFilters(reg1, false);
            account.DisplayFiltersCtnr.LoadFilters(reg1, true);
            account.NNTPSettings.ReadSettings(reg1);
            account.PostingSettings.ReadSettings(reg1);
            account.DisplaySettings.ReadSettings(reg1);
            account.NNTPServerSettings.ReadSettings(reg1);
            account.fMailAccountName := reg1.GetStringValue ('Mail Account Name', 'MAPI');
            account.fPostingAccountName := reg1.GetStringValue ('Posting Account Name', '');
            account.HasNewGroups := reg1.GetBooleanValue ('Has New Groups', False);
            fAccounts.AddObject(account.AccountName, account);

            account.LoadSubscribedGroups (reg1);

          except
            account.Free
          end
        end
      finally
        reg.Section := ''
      end
    end;

    SortAccounts;

    if reg.HasSection ('Batches') then
    begin
      reg.Section := 'Batches';

      FreeAndNil (keyNames);
      FreeAndNil (reg1);
      keyNames := TStringList.Create;
      reg.GetSectionNames(keyNames);
      reg1 := CreateChildSettings (reg);
      keyNames1 := TStringList.Create;

      for i := 0 to keyNames.Count - 1 do
      begin
        reg1.Section := keyNames [i];

        batch := TNNTPBatch.Create(keyNames [i]);
        batch.BatchName := keyNames [i];
        batch.Scheduled := reg1.GetBooleanValue('Scheduled', False);
        if batch.Scheduled then
          p := 10
        else
          p := 0;
        batch.RunEvery := reg1.GetIntegerValue ('Schedule Minutes', p);
        batch.RunAtStart := reg1.GetBooleanValue('Run At Start', false);

        fBatches.Add(batch);
        reg1.GetSectionNames(keyNames1);

        FreeAndNil (reg2);
        reg2 := CreateChildSettings (reg1);

        for j := 0 to keyNames1.Count - 1 do
        begin
          actionName := keyNames1 [j];

          reg2.Section := actionName;

          actionName := reg2.GetStringValue ('Action Name', actionName);
          p := Pos ('_', actionName);
          if p <> 0 then
          begin
            Action := TBatchAction.Create;
            try
              Action.AccountName := Copy (actionName, 1, p - 1);
              Action.GroupName := Copy (actionName, p + 1, MaxInt);
              Action.LoadFromRegistry (reg2);
              batch.fBatchActions.Add(action)
            except
              Action.Free;
              raise
            end
          end
        end
      end
    end
  finally
    reg1.Free;
    reg2.Free;
    keyNames.Free;
    keyNames1.Free;
    reg.Free;
  end;

  if Assigned (gGetVersionThread) then
    gGetVersionThread.Resume
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccounts.NextArticle : TArticleBase                    |
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
  first, fg : boolean;
  article, saveArticle : TArticleBase;
  tart : TArticle;
  group, startGroup : TArticleContainer;
  account, startAccount : TNNTPAccount;
  oldSearching : boolean;
  canWrapInAccount, canWrapInGroup : boolean;

  // Return true if article is Nil, or all of the conditions succeed or are unused
  // The loops (below) will then terminate.
  function EndScan : boolean;
  begin
    result := not Assigned (Article) or
              ((not (naUnreadOnly in Options)) or not article.IsRead) and
              ((not (naToMeOnly in Options)) or article.IsMine) and
              ((not (naReplyOnly in Options)) or article.IsReply) and
              ((not (naInterestingOnly in Options)) or article.IsInteresting) and
              ((not (naNoReplies in Options)) or article.HasNoReplies);


    if Assigned (tart) then
      result := result and ((not (naAnyKeyword in Options)) or tart.HasAnyKeyPhrase);

    if Assigned (tart) then
      result := result and
              ((not (naKeyword0 in Options)) or tart.HasKeyPhrase [0]) and
              ((not (naKeyword1 in Options)) or tart.HasKeyPhrase [1]) and
              ((not (naKeyword2 in Options)) or tart.HasKeyPhrase [2]) and
              ((not (naKeyword3 in Options)) or tart.HasKeyPhrase [3]) and
              ((not (naKeyword4 in Options)) or tart.HasKeyPhrase [4]) and
              ((not (naKeyword5 in Options)) or tart.HasKeyPhrase [5]) and
              ((not (naKeyword6 in Options)) or tart.HasKeyPhrase [6]) and
              ((not (naKeyword7 in Options)) or tart.HasKeyPhrase [7]);

  end;

begin { NextArticle }
  result := Nil;
  first := naIncludeFirstArticle in Options;
  article := firstArticle;
  startGroup := Nil;
  oldSearching := false;
  canWrapInAccount := naCanWrap in Options;
  canWrapInGroup := canWrapInAccount;

  try
    if Assigned (article) then
    begin
      saveArticle := article;     // Save the current article so we can push it
                                  // later if we succeed.

      if article is TArticle then
      begin
        tart := TArticle (article);
        startAccount := tart.Account;
        startGroup := tart.Owner;
        account := startAccount;
        oldSearching := startGroup.fSearching;
        startGroup.fSearching := True;
      end
      else
      begin
        startAccount := Nil;
        account := Nil;
        startGroup := nil;
        tart := Nil
      end;

      if naMarkAsRead in Options then     // Mark article as read if required
        article.IsRead := True;

      repeat
        repeat
          if first then           // If 'firstArticle' was specified, include this
            first := False        // article in the scan.
          else
          begin
            article := TArticle (article.Next);
            if Assigned (account) then
              tart := TArticle (article)
          end
        until EndScan;

        if not Assigned (article) and canWrapInGroup then
        begin                     // We didn't find a match, but wrap back to the
                                  // beginning if required...
          canWrapInGroup := False;  // ... but only do it once!
          article := saveArticle.Owner.FirstArticle;
          if Assigned (account) then
            tart := TArticle (article)
        end
        else
          break
      until EndScan;

      if not Assigned (article) and (naCanLeaveGroup in Options) then

      begin                       // Not found in this group - go to the next
                                  // group if required

        article := Nil;
        group := saveArticle.Owner;
        if group is TSubscribedGroup then
        begin
          account := TSubscribedGroup (group).Owner;
          fg := False
        end
        else
        begin
          account := Nil;
          fg := True
        end;

        while not Assigned (article) and Assigned (group) do
        begin
          if fg then
          begin
            fg := False;
            account := NNTPAccounts.Items [0];
            startAccount := account;
            if not Assigned (account) then break;
            if account.SubscribedGroupCount = 0 then
              group := Nil
            else
              group := account.SubscribedGroups [0];
            startGroup := group;
          end
          else
          begin
            group := Group.Next;
            if canWrapInAccount and (account = startAccount) and (group = Nil) then
            begin
              canWrapInAccount := False;
              group := account.SubscribedGroups [0]
            end;

            if group = startGroup then
              group := Nil
          end;

          while (group = Nil) and (account <> Nil) do
          begin                   // Try the next account if no next group
            account := account.Next;
            if (not Assigned (account)) and (naCircularAccounts in Options) and (NNTPAccounts.Count > 0) then
              account := NNTPAccounts.Items [0];

            if account = startAccount then
            begin
              account := Nil;
              break
            end;

            if Assigned (account) and (account.SubscribedGroupCount > 0) then
              group := account.SubscribedGroups [0]
          end;

          if (group <> Nil) and
             ((not (naUnreadOnly in Options)) or (Group.UnreadArticleCount > 0)) and
             ((not (naReplyOnly in Options)) or (Group.UnreadReplyCount > 0)) and
             ((not (naInterestingOnly in Options)) or (Group.InterestingArticleCount > 0)) and
             ((not (naToMeOnly in Options)) or (Group.UnreadArticleToMeCount > 0)) then
          begin                   // Searh this group for a match
            article := TArticle (group.FirstArticle);
            if Assigned (account) then
              tart := TArticle (article);
            while not EndScan do
            begin
              article := TArticle (article.Next);
              if Assigned (account) then
                tart := TArticle (article);
            end
          end
        end
      end;

      if Assigned (article) then  // We found a matching article.
        result := article
      else
        if (naIncludeFirstArticle in Options) and not (naTempFirstArticle in Options) then
          result := firstArticle;
    end
  finally
    if Assigned (startGroup) then
      startGroup.fSearching := oldSearching
  end
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.PerfCue                                      |
 |                                                                      |
 | Beep & log - for debugging, etc.                                     |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.PerfCue(freq: Integer; const st: string);
begin
  if gAudiblePerformanceCues then
  begin
    Windows.Beep(freq, 20);

    LogMessage ('PerfCue ' + IntToStr (freq) + ' ' + st)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.PurgeOldArticles                             |
 |                                                                      |
 | Purge deleted articles from all accounts/groups                      |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.PurgeOldArticles;
var
  i, j : Integer;
  acct : TNNTPAccount;
  grp : TSubscribedGroup;
begin
  for i := 0 to Count - 1 do
  begin
    acct := Items [i];

    for j := 0 to acct.SubscribedGroupCount - 1 do
    begin
      grp := acct.SubscribedGroups [j];
      grp.PurgeArticles (False, False, grp.DisplaySettings.PurgeFolder);
    end
  end;

  gArticleFolders.Tidy;
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.RemoveBozo                                   |
 |                                                                      |
 | Remove a bozo from the bozo list.                                    |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.RemoveBozoMatching(art : TArticle);
var
  i : Integer;
  b : TBozo;
begin
  i := 0;
  while i < BozoCount do
  begin
    b := Bozo [i];
    if b.MatchesArticle(art) then
      fBozos.Delete(i)
    else
      Inc (i)
  end;
  SaveBozoList
end;

procedure TNNTPAccounts.RemoveOldBozos;
var
  i : Integer;
  t : TDateTime;
  changed : boolean;
begin
  if Options.AutoRemoveFromBin <= 0 then
    Exit;
  i := 0;
  t := Trunc (Now);
  changed := False;
  t := IncDay (t, -Options.AutoRemoveFromBin);
  while i < fBozos.Count do
  begin
    if TBozo (fBozos [i]).BozodDate < t then
    begin
      fBozos.Delete(i);
      changed := True
    end
    else
      Inc (i)
  end;
  if changed then
    SaveBozoList
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.Rename                                       |
 |                                                                      |
 | The account has already been renamed.  Adjust the folders & settings |
 | to reflect this.                                                     |
 |                                                                      |
 | Parameters:                                                          |
 |   account: TNNTPAccount;                                             |
 |   const oldName: string                                              |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.Rename(account: TNNTPAccount;
  const oldName: string);
var
  reg, reg1 : TExSettings;
  i, j : Integer;
  batch : TNNTPBatch;
  action : TBatchAction;
begin
  reg1 := Nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Accounts';
    reg.RenameSection(FixFileNameString (oldName), FixFileNameString (account.AccountName));

    try
      if DirectoryExists (gMessageBaseRoot + '\' + FixFileNameString (oldName)) then
        if not RenameFile (gMessageBaseRoot + '\' + FixFileNameString (oldName), gMessageBaseRoot + '\' + FixFileNameString (account.AccountName)) then
          raise Exception.Create ('Cannot rename directory');

      reg1 := CreateChildSettings (reg, FixFileNameString (account.AccountName));
      reg1.SetStringValue('Account Name', account.AccountName, FixFileNameString (account.AccountName));

      for i := 0 to NNTPAccounts.BatchesCount - 1 do
      begin
        batch := NNTPAccounts.Batches [i];

        for j := 0 to batch.ActionCount - 1 do
        begin
          action := batch.Actions [j];
          if CompareText (action.AccountName, oldName) = 0 then
            action.AccountName := account.AccountName
        end
      end;

      SaveToRegistry (account);
    except
      reg.RenameSection(FixFileNameString (account.AccountName), FixFileNameString (oldName));
      raise
    end
  finally
    reg1.Free;
    reg.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.ResetKeyPhraseFlags                          |
 |                                                                      |
 | Reset the KeyPhrase flags for all accounts.                          |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.ResetKeyPhraseFlags(idx: Integer);
var
  i : Integer;
  account : TNNTPAccount;
begin
  for i := 0 to Count - 1 do
  begin
    account := Items [i];
    account.ResetKeyPhraseFlags (idx)
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.SaveBozoList                                 |
 |                                                                      |
 | Save the bozo list.                                                  |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.SaveBozoList;
var
  sl : TStringList;
  i : Integer;
  st : string;
  bozo : TBozo;

  function BozoFlagsToStr (flags : TMatchBozoFlags) : string;
  var
    fg : TMatchBozoFlag;
    i : Integer;
  begin
    result := '000';
    i := 1;
    for fg := Low (TMatchBozoFlag) to High (TMatchBozoFlag) do
    begin
      if fg in flags then
        result [i] := '1';
      Inc (i)
    end
  end;

begin
  if fBozos.Count > 0 then
  begin
    sl := TStringList.Create;
    try
      for i := 0 to fBozos.Count - 1 do
      begin
        bozo := TBozo (fBozos [i]);
        st := StringReplace (bozo.Name, ':', '.', [rfreplaceAll]) + ':' +
              StringReplace (bozo.EMail, ':', '.', [rfreplaceAll]) + ':' +
              DateTimeToStr (bozo.BozodDate);

        if (bozo.fFlags <> [fgName, fgEMail]) or (bozo.fAction <> baIgnore) then
        begin
          st := st + '~' + BozoFlagsToStr (bozo.fFlags);
          st := st + '~' + IntToStr (Integer (bozo.fAction));
          if fgKeyphrase in bozo.fFlags then
            st := st + '~' + StringReplace (bozo.fKeyPhrase, '~', '-', [rfReplaceAll]);
        end;
        sl.Add(st)
      end;

      sl.SaveToFile (gMessageBaseRoot + '\bozos.txt')
    finally
      sl.Free
    end
  end
  else
    DeleteFile (gMessageBaseRoot + '\bozos.txt')
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccounts.SaveDirtyArticles                            |
 |                                                                      |
 | Save articles if their flags have changed.                           |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.SaveDirtyArticles;
var
  i, j : Integer;
  acct : TNNTPAccount;
  grp : TSubscribedGroup;
begin
  for i := 0 to Count - 1 do
  begin
    acct := Items [i];

    for j := 0 to acct.SubscribedGroupCount - 1 do
    begin
      grp := acct.SubscribedGroups [j];
      if grp.FlagsDirty then
        grp.SaveArticles(false);
    end
  end
end;

(*----------------------------------------------------------------------*
 | TNNTPAccounts.SaveToRegistry                                         |
 |                                                                      |
 | Save the details to the registry in the same place they were loaded  |
 | from.                                                                |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.SaveToRegistry (saveGroupsForAccount : TNNTPAccount = Nil);
var
  reg, reg1, reg2 : TExSettings;
  i, j, p : Integer;

  account : TNNTPAccount;
  batch : TNNTPBatch;
  keyNames, keyNames1 : TStringList;
  Action : TBatchAction;
  actionName, accName : string;
begin
  reg1 := Nil;
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
    FiltersCtnr.SaveFilters(reg, false);
    DisplayFiltersCtnr.SaveFilters(reg, true);
  finally
    FreeAndNil (reg)
  end;

  try
    keyNames := TStringList.Create;
    reg := CreateExSettings;
    reg.Section := 'Accounts';
    reg.GetSectionNames(keyNames);

    reg1 := CreateChildSettings (reg);
    try
      for i := 0 to fAccounts.Count - 1 do
      begin
        account := TNNTPAccount (fAccounts.Objects [i]);
        accName := FixFileNameString (account.AccountName);

        reg1.Section := accName;

        p := keyNames.IndexOf(accName);
        if p >= 0 then
          keyNames.Delete (p);

        reg1.SetStringValue('Account Name', account.AccountName, accName);
        reg1.SetIntegerValue('Sort Index', account.fSortIdx, -1);

        reg1.SetStringValue ('Identity', account.NNTPSettings.Identity.Name, NNTPAccounts.Identities.DefaultIdentity.Name);

        reg1.SetBooleanValue('Mark On Leave',  account.MarkOnLeave, False);
        reg1.SetBooleanValue('No XNEWS',       account.NoXNews, False);
        reg1.SetBooleanValue('Pipelining',     account.UsePipelining, True);
        reg1.SetStringValue('Mail Account Name', account.fMailAccountName, 'MAPI');
        reg1.SetStringValue('Posting Account Name', account.PostingAccountName, '');
        reg1.SetBooleanValue('Has New Groups', account.HasNewGroups, False);
        reg1.SetStringValue('Greeting',       account.Greeting, '');
        reg1.SetBooleanValue('Secret',         account.Secret, False);
        reg1.SetBooleanValue('SortGroupsByName', account.SortGroupsByName, False);

        reg1.SetBooleanValue ('Scan Key Phrases',     account.ScanKeyPhrases, False);

        reg1.DeleteValue('SMTPMail');   // Tidy up registry for existing users
        reg1.DeleteSection ('SMTP Server');  // Remove later!

        account.FiltersCtnr.SaveFilters(reg1, false);
        account.DisplayFiltersCtnr.SaveFilters(reg1, true);
        account.NNTPSettings.WriteSettings(reg1);
        account.PostingSettings.WriteSettings(reg1);
        account.DisplaySettings.WriteSettings(reg1);
        account.NNTPServerSettings.WriteSettings(reg1);

        if account = saveGroupsForAccount then
          account.SaveSubscribedGroups(reg1);
      end;
    finally
      FreeAndNil (reg1);
    end;

    if Assigned (keyNames) then
      for i:= 0 to keyNames.Count - 1 do
      begin
        actionName := keyNames [i];
        reg.DeleteSection (keyNames [i]);
        PurgeDirectory (gMessageBaseRoot + '\' + actionName)
      end;

    if fBatches.Count > 0 then
    begin
      FreeAndNil (keyNames);
      keyNames := TStringList.Create;
      keyNames1 := TStringList.Create;
      reg.Close;
      reg.Section := 'Batches';
      reg.GetSectionNames(keyNames);
      reg1 := CreateChildSettings (reg);

      for i := 0 to fBatches.Count - 1 do
      begin
        batch := TNNTPBatch (fBatches [i]);
        p := keyNames.IndexOf(batch.BatchName);

        if p >= 0 then
          keyNames.Delete(p);

        reg1.Section := batch.BatchName;
        if batch.Scheduled then
          p := 10
        else
          p := 0;
        reg1.SetIntegerValue ('Schedule Minutes', batch.RunEvery, p);
        reg1.SetBooleanValue ('Scheduled', batch.Scheduled, False);
        reg1.SetBooleanValue ('Run At Start', batch.RunAtStart, False);

        FreeAndNil (reg2);
        reg1.GetSectionNames(keyNames1);
        reg2 := CreateChildSettings (reg1);

        for j := 0 to batch.fBatchActions.Count - 1 do
        begin
          Action := TBatchAction (batch.fBatchActions [j]);
          actionName := Action.AccountName + '_' + Action.GroupName;

          p := keyNames1.IndexOf(FixFileNameString (actionName));

          if p >= 0 then
            keyNames1.Delete(p);

          reg2.Section := FixFileNameString (actionName);
          reg2.SetStringValue('Action name', actionName, FixFileNAmeString (actionName));
          Action.SaveToRegistry (reg2);
        end;

        for j := 0 to keyNames1.Count - 1 do
          reg1.DeleteSection (keyNames1 [j])
      end;

      for i := 0 to keyNames.Count - 1 do
        reg.DeleteSection(keyNames [i])

    end
    else
    begin
      if not Assigned (reg) then
        reg := CreateExSettings;

      reg.DeleteSection ('Batches')
    end;
  finally
    reg1.Free;
    reg.Free;
    keyNames.Free;
    keyNames1.Free;
    reg2.Free;
  end
end;

{ TNNTPAccount }

procedure TNNTPAccount.ChangeGroupSortIdx(grp : TSubscribedGroup; idx: Integer);
var
  i : Integer;
  g : TSubscribedGroup;
begin
  if idx = grp.fSortIdx then Exit;

  i := SubscribedGroupCount - 1;
  while i >= 0 do
  begin
    g := SubscribedGroups [i];
    if g = grp then
      g.fSortIdx := idx
    else
      if g.fSortIdx <> -1 then
        if g.fSortIdx >= idx then
          Inc (g.fSortIdx);

    Dec (i)
  end;

  SortGroups;

  for i := 0 to SubscribedGroupCount - 1 do
    SubscribedGroups [i].fSortIdx := i;
end;

procedure TNNTPAccount.CopySettingsFrom(account: TNNTPAccount);
begin
  self.fNoXNews := account.NoXNews;
  self.fMarkOnLeave := account.MarkOnLeave;
  self.fScanKeyPhrases := account.ScanKeyPhrases;
  self.fFiltersCtnr.AssignFilters(account.FiltersCtnr);
  self.fDisplayFiltersCtnr.AssignFilters(account.DisplayFiltersCtnr);
  self.fNNTPSettings.Assign(account.NNTPSettings);
  self.fPostingSettings.Assign(account.PostingSettings);
  self.fDisplaySettings.Assign(account.DisplaySettings);
  self.fNNTPServerSettings.Assign(account.NNTPServerSettings);
  self.fMailAccountName := account.fMailAccountName;
  self.fPostingAccountName := account.fPostingAccountName;
  self.fGreeting := account.fGreeting;
  self.fSecret := account.fSecret;
end;

(*----------------------------------------------------------------------*
 | constructor TNNTPAccount.Create                                      |
 |                                                                      |
 | Constructor for TNNTPAccount.  Initialize, and create the list of    |
 | subscribed groups.                                                   |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   AOwner : TNNTPAccounts     The owning account list                 |
 *----------------------------------------------------------------------*)
constructor TNNTPAccount.Create (AOwner : TNNTPAccounts);
begin
  inherited Create;
  fOwner := AOwner;
  fSubscribedGroups := TStringList.Create;
  fFiltersCtnr := TFiltersCtnr.Create (self, AOwner.FiltersCtnr);
  fDisplayFiltersCtnr := TFiltersCtnr.Create(self, AOwner.DisplayFiltersCtnr);
  fNNTPSettings := TNNTPSettings.Create (Owner.fNNTPSettings);
  fPostingSettings := TPostingSettings.Create(Owner.fPostingSettings);
  fDisplaySettings := TDisplaySettings.Create(Owner.fDisplaySettings);
  fNNTPServerSettings := TNNTPServerSettings.Create (nil);
  fUsePipelining := True;
  fSecretGroupCount := -1;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccount.CreateAccountRegistry                          |
 |                                                                      |
 | Open/Create the registry for this account                            |
 *----------------------------------------------------------------------*)
function TNNTPAccount.CreateAccountRegistry(access: DWORD): TExSettings;
begin
  result := CreateExSettings;
  result.Section := 'Accounts\' + FixFileNameString (AccountName);
  if not result.Open ((access and KEY_WRITE) = 0) then
    FreeAndNil (result);
end;

(*----------------------------------------------------------------------*
 | destructor TNNTPAccount.Destroy                                      |
 |                                                                      |
 | Destructor for TNNTPAccount.  Destroy the list of subscribed groups. |
 *----------------------------------------------------------------------*)
destructor TNNTPAccount.Destroy;
var
  i : Integer;
begin
  if Assigned (NNTPAccounts) then
  begin
    NNTPAccounts.fLastFoundGroup := Nil;
    NNTPAccounts.fLastServerAccount := '';
    NNTPAccounts.fLastServer := ''
  end;
  fSecretGroupCount := 0;
  for i := 0 to SubscribedGroupCount - 1 do
    SubscribedGroups [i].Free;
  fSubscribedGroups.Free;
  fXOverFMT.Free;               // Will exist if SetXOverFMT has been called.
                                // (contains the XOVER Format list)
  fFiltersCtnr.Free;
  fDisplayFiltersCtnr.Free;
  fNNTPSettings.Free;
  fPostingSettings.Free;
  fDisplaySettings.Free;
  fNNTPServerSettings.Free;
  inherited;
end;


(*----------------------------------------------------------------------*
 | function TNNTPAccount.FindSubscribedGroup                            |
 |                                                                      |
 | Find a subscribed group in this account.                             |
 *----------------------------------------------------------------------*)
function TNNTPAccount.FindSubscribedGroup(
  const groupName: string): TSubscribedGroup;
var
  idx : Integer;
begin
  idx := fSubscribedGroups.IndexOf(groupName);
  if idx >= 0 then
  begin
    result := TSubscribedGroup (fSubscribedGroups.Objects [idx]);
    if (not NNTPAccounts.fShowSecrets) and result.fSecret then
      result := Nil
  end
  else
    result := Nil
end;

function TNNTPAccount.GetFileName: string;
begin
  result := gMessageBaseRoot + '\' + FixFileNameString (AccountName) + '\newsgroups.dat'
end;

function TNNTPAccount.GetNext: TNNTPAccount;
var
  i, idx : Integer;
begin
  result := Nil;
  if NNTPAccounts.fSecretAccountCount > 0 then
  begin
    idx := -1;

    for i := 0 to NNTPAccounts.Count - 1 do
      if NNTPAccounts [i].AccountName = AccountName then
      begin
        idx := i;
        break
      end
  end
  else
    idx := NNTPAccounts.fAccounts.IndexOf(AccountName);
  if idx >= 0 then
  begin
    Inc (idx);
    if idx < NNTPAccounts.Count then
      result := NNTPAccounts.Items [idx]
  end
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccount.GetSubscribedGroup : TSubscribedGroup          |
 |                                                                      |
 | 'Get' method for SubscribedGroup property.  Returns the group at     |
 | position 'idx'                                                       |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   idx : Integer              The index of the group to get           |
 |                                                                      |
 | The function returns the group at position 'idx'                     |
 *----------------------------------------------------------------------*)
function TNNTPAccount.GetSubscribedGroup(idx: Integer): TSubscribedGroup;
var
  i : Integer;
begin
  if fSecretGroupCount > 0 then
  begin
    result := Nil;
    for i := 0 to fSubscribedGroups.Count - 1 do
      if not TSubscribedGroup (fSubscribedGroups.Objects [i]).fSecret then
        if idx = 0 then
        begin
          result := TSubscribedGroup (fSubscribedGroups.Objects [i]);
          break
        end
        else
          Dec (idx)
  end
  else
    result := fSubscribedGroups.Objects [idx] as TSubscribedGroup
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccount.GetSubscribedGroupCount : Integer              |
 |                                                                      |
 | 'Get method for SubscribedGroupCount property.  Returns the number   |
 | of subscribed groups                                                 |
 |                                                                      |
 | The function returns the number of subscribed groups.                |
 *----------------------------------------------------------------------*)

function TNNTPAccount.GetSubscribedGroupCount: Integer;
var
  i : Integer;
begin
  if fSecretGroupCount = -1 then
  begin
    fSecretGroupCount := 0;
    for i := 0 to fSubscribedGroups.Count - 1 do
      if not NNTPAccounts.fShowSecrets and TSubscribedGroup (fSubscribedGroups.Objects [i]).Secret then
        Inc (fSecretGroupCount)
  end;

  result := fSubscribedGroups.Count - fSecretGroupCount;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccount.GetXOverFMT                                    |
 |                                                                      |
 | 'Get' method for XOverFMT property.  Returns the XOVER format list   |
 |                                                                      |
 | The function returns the XOVER format string list                    |
 *----------------------------------------------------------------------*)
function TNNTPAccount.GetXOverFMT: TStringList;
begin
  result := fXOVERFmt;
end;

(*----------------------------------------------------------------------*
 | function TNNTPAccount.IsSubscribedTo                                 |
 |                                                                      |
 | Returns true if the account is subscribed to the specified group     |
 *----------------------------------------------------------------------*)
function TNNTPAccount.IsSubscribedTo(const groupName: string): boolean;
begin
  result := fSubscribedGroups.IndexOf (groupName) >= 0
end;

(*----------------------------------------------------------------------*
 | procedure TNNTPAccount.LoadSubscribedGroups                          |
 |                                                                      |
 | (private)  Loads the subscribed group details from the registry key  |
 | at rootKey                                                           |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   rootKey : HKEY             The root key of the subscribed group    |
 |                              information held in the registry        |
 *----------------------------------------------------------------------*)
procedure TNNTPAccount.LoadSubscribedGroups(rootReg : TExSettings);
var
  reg, reg1 : TExSettings;
  keyNames : TStringList;
  i : Integer;
  group : TSubscribedGroup;
begin
   keyNames := Nil;
   reg1 := Nil;

   if rootReg.HasSection ('Subscribed Groups') then
   begin

     reg := CreateChildSettings (rootReg, 'Subscribed Groups');
     try
       reg1 := CreateChildSettings (reg);
       keyNames := TStringList.Create;

       reg.GetSectionNames(keyNames);

       for i := 0 to keyNames.Count - 1 do
       begin
         group := TSubscribedGroup.Create(self, keyNames [i]);
         reg1.Section := keyNames [i];
         group.fHighWaterMark := reg1.GetIntegerValue ('Last Article', 0);
         group.fSortIdx := reg1.GetIntegerValue ('Sort Index', -1);
         group.FiltersCtnr.LoadFilters (reg1, false);
         group.DisplayFiltersCtnr.LoadFilters (reg1, true);
         group.NNTPSettings.ReadSettings(reg1);
         group.PostingSettings.ReadSettings(reg1);
         group.DisplaySettings.ReadSettings(reg1);
         group.Nickname := reg1.GetStringValue('Nickname', '');
         group.fSecret := reg1.GetBooleanValue('Secret', False);
         group.LoadArticleCounts(reg1);
         fSubscribedGroups.AddObject(keyNames [i], group)
       end;

       SortGroups;
     finally
       keyNames.Free;
       reg1.Free;
       reg.Free
     end
   end;
end;

procedure TNNTPAccount.ResetKeyPhraseFlags(idx: Integer);
var
  j, k : Integer;
  grp : TSubscribedGroup;
  article : TArticle;
begin
  if ScanKeyPhrases then
    for j := 0 to SubscribedGroupCount - 1 do
    begin
      grp := SubscribedGroups [j];

      for k := 0 to grp.LoadGetArticleCount - 1 do
      begin
        article := grp.Articles [k];
        if (article.fFlags and fgScannedKeyPhrases) <> 0 then
          article.fFlags := article.fFlags and not (fgScannedKeyPhrases or fgKeyPhraseMask)
      end;
      grp.fFlagsDirty := True
    end
end;

procedure TNNTPAccount.SaveSubscribedGroups(rootReg : TExSettings);
var
  reg, reg1 : TExSettings;
  i : Integer;
  group : TSubscribedGroup;
begin
   reg1 := Nil;
   reg := CreateChildSettings (rootReg, 'Subscribed Groups');
   try
     reg1 := CreateChildSettings (reg);

     for i := 0 to fSubscribedGroups.Count - 1 do
     begin
       group := TSubscribedGroup (fSubscribedGroups.Objects [i]);
       reg1.Section := group.Name;

       reg1.SetIntegerValue ('Last Article', group.fHighWaterMark, 0);
       reg1.SetStringValue('Nickname', group.Nickname, '');
       reg1.SetBooleanValue('Secret', group.Secret, false);
       reg1.SetIntegerValue('Sort Index', group.fSortIdx, -1);

       group.FiltersCtnr.SaveFilters (reg1, false);
       group.DisplayFiltersCtnr.SaveFilters (reg1, true);
       group.NNTPSettings.WriteSettings(reg1);
       group.PostingSettings.WriteSettings(reg1);
       group.DisplaySettings.WriteSettings(reg1);
     end
   finally
     reg1.Free;
     reg.Free
   end
end;

procedure TNNTPAccount.SetAccountName(const Value: string);
var
  oldName : string;
begin
  if value <> AccountName then
  begin
    if AccountName = '' then
      fAccountName := Value
    else
      begin
        oldName := fAccountName;
        fAccountName := Value;
        NNTPAccounts.Rename(self, oldName);
      end
    end
end;

procedure TNNTPAccount.SetGreeting(const Value: string);
var
  reg : TExSettings;
begin
  if value <> fGreeting then
  begin
    fGreeting := Value;
    reg := CreateAccountRegistry (KEY_READ or KEY_WRITE);
    try
      reg.SetStringValue('Greeting', fGreeting, '')
    finally
      reg.Free
    end
  end
end;

procedure TNNTPAccount.SetScanKeyPhrases(const Value: boolean);
begin
  if fScanKeyPhrases <> Value then
  begin
    fScanKeyPhrases := Value;
    if Value then
      self.ResetKeyPhraseFlags (-1)
  end
end;

(*----------------------------------------------------------------------*
 | TNNTPAccount.SetXOverFMT                                             |
 |                                                                      |
 | 'Set' method for XOverFMT property, which gives a list of the XOVER  |
 | format strings.                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   s : TStringList            The XOVER format strings.               |
 *----------------------------------------------------------------------*)
procedure TNNTPAccount.SetSecret(const Value: boolean);
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
  Owner.ChangeAccountSortIdx (self, Value);
end;

procedure TNNTPAccount.SetXOverFMT(s: TStringList);
begin
  if not Assigned (fXOverFMT) then
    fXOverFMT := TStringList.Create;

  fXOverFMT.Assign(s);
end;

function OldCompareGroups (List: TStringList; Index1, Index2: Integer) : Integer;
var
  grp1, grp2 : TSubscribedGroup;
  st1, st2 : string;
begin
  grp1 := TSubscribedGroup (List.Objects [Index1]);
  grp2 := TSubscribedGroup (List.Objects [Index2]);

  if grp1.Nickname = '' then
    st1 := grp1.Name
  else
    st1 := grp1.Nickname;

  if grp2.Nickname = '' then
    st2 := grp2.Name
  else
    st2 := grp2.Nickname;

  result := CompareText (st1, st2);
end;

function CompareGroups (List: TStringList; Index1, Index2: Integer) : Integer;
var
  grp1, grp2 : TSubscribedGroup;
begin
  grp1 := TSubscribedGroup (List.Objects [Index1]);
  grp2 := TSubscribedGroup (List.Objects [Index2]);

  result := grp1.fSortIdx - grp2.fSortIdx
end;

procedure TNNTPAccount.SortGroups;
var
  i : Integer;
begin
  if fSortGroupsByName then
    fSubscribedGroups.CustomSort(OldCompareGroups)
  else
    fSubscribedGroups.CustomSort(CompareGroups);

  for i := 0 to fSubscribedGroups.Count - 1 do
    TSubscribedGroup(fSubscribedGroups.Objects[i]).fSortIdx := i;
end;

(*----------------------------------------------------------------------*
 | TNNTPAccount.SubscribeTo                                             |
 |                                                                      |
 | Subscribe to a group.  Add 'subscribed group' details to the         |
 | subscribed groups list                                               |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const groupName : string   The group to subscribe to               |
 *----------------------------------------------------------------------*)
function TNNTPAccount.SubscribeTo(const groupName: string) : TSubscribedGroup;
var
  idx : Integer;
  reg : TExSettings;
  grp : TSubscribedGroup;
begin
  idx := fSubscribedGroups.IndexOf (groupName);

  if idx = -1 then
  begin
    grp := TSubscribedGroup.Create(self, groupName);
    grp.fSortIdx := fSubscribedGroups.Count;
    idx := fSubscribedGroups.AddObject (groupName, grp);
    SortGroups;
    CreateDir (FixFileNameString (AccountName) + '\' + FixFileNameString (groupName));
    reg := grp.CreateGroupRegistry (KEY_READ or KEY_WRITE);
    try
      reg.SetIntegerValue ('Last Article', 0)
    finally
      reg.Free;
    end;
    Owner.SaveToRegistry(Self);
  end;

  result := TSubscribedGroup (fSubscribedGroups.Objects [idx])
end;

(*----------------------------------------------------------------------*
 | TNNTPAccount.UnsubscribeTo                                           |
 |                                                                      |
 | Unsubscribe to a group.  Delete it's details from the 'Subscribed    |
 | Groups' list, and delete all the data held for the group.            |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const groupName : string   The group to unsubscribe from.          |
 *----------------------------------------------------------------------*)
procedure TNNTPAccount.UnsubscribeTo(const groupName: string);
var
  idx : Integer;
  reg : TExSettings;
  gn : string;
begin
  idx := fSubscribedGroups.IndexOf (groupName);

  if idx >= 0 then
  begin
    gn := FixFileNameString (groupName);
    fSubscribedGroups.Objects [idx].Free;
    fSubscribedGroups.Delete(idx);
    PurgeDirectory (gMessageBaseRoot + '\' + FixFileNameString (AccountName) + '\' + gn);

    reg := CreateExSettings;
    try
      reg.Section := 'Accounts' + '\' + FixFileNameString (AccountName) + '\Subscribed Groups';
      reg.DeleteSection(gn);
    finally
      reg.Free
    end;

    for idx := 0 to fSubscribedGroups.Count - 1 do
      TSubscribedGroup (fSubscribedGroups.Objects [idx]).fSortIdx := idx;

    Owner.SaveToRegistry(Self);
  end
end;

{ TSubscribedGroup }

function TSubscribedGroup.AddArticle (articleNo : Integer; header : TStrings; body : TStream; isNew : boolean) : TArticle;
var
  article : TArticle;
  filterIt, bozo : Boolean;
  action : TBozoAction;
begin
  if not Loaded then
    LoadArticles;

  article := TArticle.Create (self);
  try
    article.Initialize(articleNo, header);

    article.fMsg := TmvMessage.Create (article);
    article.fMsg.Header := header;
    article.fMsg.AddData(body);

    if isNew then
      article.fFlags := article.fFlags or fgNew;

    filterIt := filtersCtnr.BlockArticle(article);
    bozo := False;
    if not filterIt then
    begin
      bozo := NNTPAccounts.ArticleIsFromBozo (article, action);
      if bozo and (action = baDontDownload) then
        filterIt := True
    end;

    if not filterIt then
    begin
      if bozo then article.IsRead := True;
      article.SaveMessageBody;

      RawAddArticle (article);
      fUnreadArticleCount := -1;
      fUnloadedArticleCount := -1;
      fInterestingArticleCount := -1;
      fUnreadInterestingArticleCount := -1;
      MessageCacheController.Add(article);

      if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) then
        fThreads.Add(article);
    end
    else
      FreeAndNil (article)
  except
    FreeAndNil (article);
    raise
  end;

  result := article
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.AddRawHeaders                                       |
 |                                                                      |
 | Add a list of the headers (in XOVER format) to the articles list.    |
 | overwrite the existing articles if the 'replace' flag is set.        |
 |                                                                      |
 | nb.  This is not intended to be thread safe.                         |
 |                                                                      |
 | It's called during LoadArticles, and after downloading article       |
 | headers.                                                             |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   headers : TStringList      Headers to add                          |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.AddRawHeaders(headers: TStringList; XOVERFmt : TStringList);
var
  i, n : Integer;

  articleNo : cardinal;
  subject : string;
  from : string;
  date : TDateTime;
  MessageID : string;
  references : string;
  Bytes : Cardinal;
  lines : Cardinal;
  exd, val, hdr : string;

  article : TArticle;

begin
  if not fArticlesLoaded then
    LoadArticles;

  fArticles.Capacity := fArticles.Capacity + headers.Count;

  for i := 0 to headers.Count - 1 do
  begin
    ParseXOVER (headers [i], articleNo, subject, from, date, MessageID, references, bytes, lines, exd);
    article := TArticle.Create (self);

    article.fArticleNo := articleNo;
    article.fMessageID := Trim (MessageID);
    article.fBytes := bytes;
    article.fLines := lines;
    article.fReferences := Trim (references);
    article.fFrom := from;
    article.fSubject := subject;
    article.fDate := date;
    article.fMessageOffset := $ffffffff;
    article.fFlags := fgNew;

    n := 7;

    if Assigned (XOverFMT) then while (n < XOverFMT.Count) and (exd <> '') do
    begin
      val := Fetch (exd, #9);
      hdr := XOverFMT [n];
      hdr := Fetch (hdr, ':');
      Inc (n);
      article.fTempExtraHeaders := article.fTempExtraHeaders + #9 + hdr + ':' + val;
    end;

    if article.fTempExtraHeaders <> '' then
      Delete (article.fTempExtraHeaders, 1, 1);

    if not filtersCtnr.BlockArticle (article) then
      RawAddArticle (article)
  end;


  NNTPAccounts.PerfCue (660, 'Added Article Headers');
  ReSortArticles;
  NNTPAccounts.PerfCue (880, 'Sorted articles');
  fArticlesLoaded := True;
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.Create                                              |
 |                                                                      |
 | Constructor for TSubscribedGroup                                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   AOwner : TNNTPAccount      The owning account                      |
 |   const AGroupName : string  The group name                          |
 *----------------------------------------------------------------------*)
constructor TSubscribedGroup.Create(AOwner: TNNTPAccount; const AGroupName: string);
begin
  inherited Create (AGroupName, AOwner.FiltersCtnr, AOwner.DisplayFiltersCtnr);
  fOwner := AOwner;
  fNNTPSettings := TNNTPSettings.Create (fOwner.NNTPSettings);
  fPostingSettings := TPostingSettings.Create (fOwner.PostingSettings);
  fDisplaySettings := TDisplaySettings.Create (fOwner.DisplaySettings);
end;

function TSubscribedGroup.CreateGroupRegistry (access : DWORD) : TExSettings;
begin
  result := CreateExSettings;
  result.Section := 'Accounts\' + FixFileNameString (fOwner.AccountName) + '\Subscribed Groups\' + FixFileNameString (Name);

  if not result.Open((access and KEY_WRITE) = 0) then
    FreeAndNil (result)
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.Destroy                                             |
 |                                                                      |
 | Destructor for TSubscribedGroup                                      |
 *----------------------------------------------------------------------*)
destructor TSubscribedGroup.Destroy;
begin
  if Assigned (NNTPAccounts) then
    NNTPAccounts.fLastFoundGroup := Nil;
  fNNTPSettings.Free;
  fPostingSettings.Free;
  fDisplaySettings.Free;
  inherited;
end;

function TSubscribedGroup.GetLastArticle: Integer;
var
  i, ct : Integer;
begin
  result := 0;

  if not fArticlesLoaded then
  begin
    if fRawLastArticle = 0 then
    begin
      fUnreadArticleCount := -1;
      GetUnreadArticleCount;
    end;
    result := fRawLastArticle;
  end
  else
    begin
      ct := fArticles.Count;
      for i := 0 to ct - 1 do
        if (articles [i].fArticleNo > result) and (articles [i].fArticleNo <> 99999999) then
          result := articles [i].fArticleNo;
      fRawLastArticle := result
    end;

  if fHighWaterMark > result then
    result := fHighWaterMark
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.LeaveGroup                                          |
 |                                                                      |
 | Another group has been selected in the UI.  Leave this group - save  |
 | it's articles, and release the memory for the messages.              |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.LeaveGroup(clearMessages : boolean);
var
  reg : TExSettings;
begin
  ClearThreadInfo;
  NNTPAccounts.PerfCue (880, 'Leaving group');

  if FlagsDirty then
    SaveArticles (False);
  NNTPAccounts.PerfCue (660, 'Saved Articles');
  fFocused := False;


  // Unload all messages...
  if clearMessages then
  begin
    MessageCacheController.Clear;
    UnloadArticles;
    NNTPAccounts.PerfCue (440, 'Unloaded articles');
  end;


  reg := CreateGroupRegistry (KEY_READ or KEY_WRITE);
  if Assigned (reg) then
  try
    FiltersCtnr.SaveFilters (reg, false);
    DisplayFiltersCtnr.SaveFilters (reg, true);
    NNTPSettings.WriteSettings(reg);
    PostingSettings.WriteSettings(reg);
    DisplaySettings.WriteSettings(reg);
    reg.SetStringValue('Identity', NNTPSettings.Identity.Name, Owner.NNTPSettings.Identity.Name);
    reg.SetStringValue ('Nickname', fNickname, '');
  finally
    reg.Free
  end;

  ResetSortFlags;
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.LoadArticles                                        |
 |                                                                      |
 | Load the article headers for the group if they haven't already been  |
 | loaded.                                                              |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.LoadArticles;
var
  reader : TTextFileReader;
  fileName : string;

begin

  OpenMessageFile;
  if not fArticlesLoaded then
  begin
    LogMessage ('Loading ' + name);
    LSSync.Enter;
    try
      fArticlesLoaded := True;
      fThreadSortDirection := DisplaySettings.ThreadSortDirection;
      fThreadSortOrder := DisplaySettings.ThreadSortOrder;
      fThreadOrder := DisplaySettings.ThreadOrder;
      fHideReadMessages := Options.HideReadMessages;
      fHideMessagesNotToMe := False;
      fHideIgnoredMessages := Options.HideIgnoredMessages;

      fileName := gMessageBaseRoot + '\' + FixFileNameString (Owner.AccountName) + '\' + FixFileNameString (Name) + '\articles.dat';
      if FileExists (fileName) then
      begin
        NNTPAccounts.PerfCue (440, 'Loading Article headers');
        reader := TTextFileReader.Create(fileName);
        try
          NNTPAccounts.PerfCue (550, 'Adding article headers');
          RawClearArticles;
          AddRawHeaders (reader);
        finally
          reader.Free
        end
      end
    finally
      LSSync.Leave
    end;
    if  Options.AutoCrosspostDetect then
      DontUnloadCache.Capacity := 3
    else
      DontUnloadCache.Capacity := 2;
    DontUnloadCache.Add(self);
    NNTPAccounts.UnloadOldContainers (self);
  end
  else
    SortArticles;
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.OpenMessageFile                                     |
 |                                                                      |
 | (private)  Ensure that the message body file 'messages.dat' is open  |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.OpenMessageFile;
var
  msgFileName : string;
begin
  if not Assigned (fMessageFile) then
  begin
    msgFileName := gMessageBaseRoot + '\' + FixFileNameString (Owner.AccountName) + '\' + FixFileNameString (Name) + '\messages.dat';
    if FileExists (msgFileName) then
      fMessageFile := TFileStream.Create(msgFileName, fmOpenReadWrite)
    else
    begin
      ForceDirectories (ExtractFilePath (msgFileName));
      fMessageFile := TFileStream.Create(msgFileName, fmCreate)
    end
  end
end;

(*----------------------------------------------------------------------*
 | TSubscribedGroup.SaveArticles                                        |
 |                                                                      |
 | Save the articles headers and message bodies                         |
 *----------------------------------------------------------------------*)
procedure TSubscribedGroup.SaveArticles (recreateMessageFile : boolean);
var
  fileName, newFileName, newMsgFileName, msgFileName : string;
  w : TTextFileWriter;
  i, j : Integer;
  article : Tarticle;
  st : string;
//  ms : TFileStream;
  ms : TBufferedFileWriter;
  hasMessage, tmpMessage : boolean;
  hLen : Word;
  accName : string;
  gName : string;
begin
  LSSync.Enter;
  try
    SaveArticleCounts;
    fUnreadArticleCount := -1;
    fUnloadedArticleCount := -1;
    fInterestingArticleCount := -1;
    fUnreadInterestingArticleCount := -1;

    accName := FixFileNameString (Owner.AccountName);
    gName := FixFileNameString (Name);

    newFileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\articles.new';
    fileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\articles.dat';
    newMsgFileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\messages.new';
    msgFileName := gMessageBaseRoot + '\' + accName + '\' + gName + '\messages.dat';

    ms := Nil;
    ForceDirectories (ExtractFilePath (msgFileName));
    w := TTextFileWriter.Create (newFileName);
    try
      if recreateMessageFile then
      begin
        OpenMessageFile;
        ms := TBufferedFileWriter.Create(newMsgFileName);
      end;

      for i := 0 to LoadGetArticleCount - 1 do
      begin
        article := TArticle (fArticles [i]);

        if article.ArticleNo <> 0 then
        begin
          if recreateMessageFile then
          begin
            tmpMessage := False;
            if not Assigned (article.fMsg) and (article.fMessageOffset <> $ffffffff) then
              tmpMessage := True;

              // nb. use article.Msg, not article.fMsg to force loading of temp message.
              //
              // Also, must test fMessageOffset <> $ffffffff.  We don't want to save downloading messages.
            hasMessage := assigned (article.Msg) and (article.fMessageOffset <> $ffffffff) and (article.fMsg.RawData.Size > 0);
          end
          else
          begin
            tmpMessage := False;
            hasMessage := article.fMessageOffset <> $ffffffff
          end;

          if recreateMessageFile and hasMessage then
            article.fMessageOffset := ms.Position;

          st := Format ('%d'#9'%s'#9'%s'#9'%s'#9'%s'#9'%s'#9'%d'#9'%d'#9'%d', [
            article.fArticleNo,
            article.fSubject,
            article.fFrom,
            SafeDateTimeToInternetStr (article.fDate, True),
            article.fMessageID,
            article.fReferences,
            article.Bytes,
            article.fLines,
            article.fFlags
            ]);

          if hasMessage then // Add extra 'message offset' field to header string
            if recreateMessageFile then
              st := st + #9 + IntToStr (ms.Position)
            else
              st := st + #9 + IntToStr (article.fMessageOffset);

          w.WriteLn (st);

          if hasMessage and recreateMessageFile then
          begin
            st := 'X-Msg:'+ IntToHex (article.fMsg.RawData.Size, 8);
            ms.Write(st [1], Length (st));

            for j := 0 to article.fMsg.Header.Count - 1 do
            begin
              st := article.fMsg.Header [j];
              hLen := Length (st);
              ms.Write(hLen, SizeOf (hLen));
              ms.Write(st [1], Length (st));
            end;

            hLen := 0;
            ms.Write(hLen, SizeOf (hLen));

            article.fMsg.RawData.Seek(0, soFromBeginning);
            ms.Write(article.fMsg.RawData.Memory^, article.fMsg.RawData.Size);
          end;

          if tmpMessage then
            FreeAndNil (article.fMsg)
        end
      end;

    finally
      w.Free;
      ms.Free;
      FreeAndNil (fMessageFile);

      ForceRenameFile(newFileName, fileName);

      if recreateMessageFile then
        ForceRenameFile(newMsgFileName, msgFileName);
    end;
    fFlagsDirty := False;
  finally
    LSSync.Leave
  end
end;

function TSubscribedGroup.GetMessagebaseSize: Int64;
var
  fileName : string;
  f : TFileStream;
begin
  OpenMessageFile;
  result := fMessageFile.Size;
  fileName := gMessageBaseRoot + '\' + FixFileNameString (Owner.AccountName) + '\' + FixFileNameString (Name) + '\articles.dat';
  LSSync.Enter;
  try
    if FileExists (fileName) then
    begin
      f := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
      try
        Inc (result, f.Size)
      finally
        f.Free
      end
    end
  finally
    LSSync.Leave
  end
end;

procedure TSubscribedGroup.SetSecret(const Value: boolean);
begin
  inherited;

  fOwner.fSecretGroupCount := -1;
end;

function TSubscribedGroup.GetHighestArticleNo: Integer;
var
  i : Integer;
begin
  result := 0;

  for i := 0 to LoadGetArticleCount - 1 do
    if (articles [i].fArticleNo <> 99999999) and (articles [i].fArticleNo > result) then
      result := articles [i].fArticleNo;
end;

procedure TSubscribedGroup.SetSortIdx(const Value: Integer);
begin
  Owner.SortGroupsByName := False;
  Owner.ChangeGroupSortIdx (self, Value);
end;

procedure TSubscribedGroup.SaveArticleCounts(reg : TExSettings);
var
  NeedsRegistry : boolean;
begin
  NeedsRegistry := reg = Nil;
  try
    if NeedsRegistry then
      reg := CreateGroupRegistry (KEY_READ or KEY_WRITE);

    if reg = Nil then Exit;

    if ArticleCount > 0 then
    begin
      reg.SetIntegerValue ('Unloaded Article Count', ArticleCount, 0);
      reg.SetIntegerValue ('Unread Article Count', UnreadArticleCount, 0);
      reg.SetIntegerValue ('Unread Article To Me Count', UnreadArticleToMeCount, 0);
      reg.SetIntegerValue ('Unread Xananews Article Count', UnreadXananewsArticleCount, 0);
      reg.SetIntegerValue ('Unread Reply Count', UnreadReplyCount, 0);
      reg.SetIntegerValue ('Interesting Article Count', InterestingArticleCount, 0);
      reg.SetIntegerValue ('Unread Interesting Article Count', UnreadInterestingArticleCount, 0);
    end
    else
    begin
      reg.DeleteValue('Unloaded Article Count');
      reg.DeleteValue('Unread Article Count');
      reg.DeleteValue ('Unread Article To Me Count');
      reg.DeleteValue ('Unread Xananews Article Count');
      reg.DeleteValue ('Unread Reply Count');
      reg.DeleteValue ('Interesting Article Count');
      reg.DeleteValue ('Unread Interesting Article Count');
    end
  finally
    if NeedsRegistry then
      reg.Free
  end
end;

procedure TSubscribedGroup.LoadArticleCounts(reg : TExSettings);
begin
  fUnloadedArticleCount := reg.GetIntegerValue ('Unloaded Article Count', -1);
  fAdjustedArticleCount := fUnloadedArticleCount;
  if fUnloadedArticleCount <> -1 then
  begin
    fUnreadArticleToMeCount := reg.GetIntegerValue ('Unread Article To Me Count', 0);
    fUnreadArticleCount := reg.GetIntegerValue ('Unread Article Count', 0);
    fUnreadXananewsArticleCount := reg.GetIntegerValue ('Unread Xananews Article Count', 0);
    fUnreadReplyCount := reg.GetIntegerValue ('Unread Reply Count', 0);
    fInterestingArticleCount := reg.GetIntegerValue ('Interesting Article Count', 0);
    fUnreadInterestingArticleCount := reg.GetIntegerValue ('Unread Interesting Article Count', 0);
  end
end;

procedure TSubscribedGroup.LoadUnreadArticleCount;
begin
  fUnreadArticleCount := -1;
  GetUnreadArticleCount;
end;

function TSubscribedGroup.TSGetLastArticle: Integer;
begin
  LSSync.Enter;
  try
    if Loaded then
      result := LastArticle
    else
    begin
      QuickLoadHeaderStats;
      result := fRawLastArticle;
    end
  finally
    LSSync.Leave;
  end;
  if fHighWaterMark > result then
    result := fHighWaterMark
end;

procedure TSubscribedGroup.UnloadArticles;
begin
  if Loaded then
  begin
    LSSync.Enter;
    try
      inherited UnloadArticles
    finally
      LSSync.Leave
    end
  end
end;

{ TArticle }

procedure TArticle.ChangeArticleNo(newArticleNo: Integer);
begin
  fArticleNo := newArticleNo;
end;

procedure TArticle.FixArticleNo;
var
  xref, st : string;
  p : Integer;
begin
  xref := Header ['Xref'];
  if xref <> '' then
  begin
    st := ' ' + SubscribedGroup.Name + ':';
    p := Pos (st, xref);
    if p > 0 then
    begin
      st := Copy (xref, p + Length (st), MaxInt);
      p := Pos (' ', st);
      if p > 0 then
        st := Copy (st, 1, p - 1);
      fArticleNo := StrToIntDef (st, 0);
    end
    else
      fArticleNo := 99999999
  end
end;

(*----------------------------------------------------------------------*
 | constructor TArticle.Create                                          |
 |                                                                      |
 | Constructor for TArticle                                             |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   AOwner : TSubscribedGroup          The owning group                |
 *----------------------------------------------------------------------*)
constructor TArticle.Create(AOwner: TArticleContainer);
begin
  inherited Create (AOwner);
  fCodePage := -1;
end;

(*----------------------------------------------------------------------*
 | destructor TArticle.Destroy                                          |
 |                                                                      |
 | Destructor for TArticle                                              |
 *----------------------------------------------------------------------*)
destructor TArticle.Destroy;
begin
  MessageCacheController.AlwaysRemove := True;
  try
    MessageCacheController.Remove (self);
  finally
    MessageCacheController.AlwaysRemove := False
  end;
  FreeAndNil (fMsg);

  inherited;
end;

function TArticle.GetAccount: TNNTPAccount;
begin
  result := SubscribedGroup.Owner
end;

function TArticle.GetCodePage: Integer;
begin
  result := GetCodePageFromFile
end;

function TArticle.GetHasKeyPhrase(idx: Integer): boolean;
begin
  GetKeyPhraseNo;
  result := (fFlags and (fgKeyPhrase0 shl idx)) <> 0
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
  i, len : Integer;
  hLen : word;
  st : string;
  r : TStrings;
  m : TMemoryStream;
  rCreated : boolean;
begin
  if fInterestingMessageLine <> '' then
  begin
    result := fInterestingMessageLine;
    exit
  end;

  rCreated := False;
  r := nil;
  m := nil;
  try
    if Assigned (fMsg) then
      r := fMsg.TextPart
    else
      if (fMessageOffset <> $ffffffff) and Assigned (Owner.fMessageFile) then

      begin               // Load (some of) the message from 'messages.dat'
                          // I did this to prevent huge messages being loaded/
                          // decoded when simply displaying the headers for non-
                          // selected messages.

        Owner.fMessageFile.Seek (fMessageOffset, soFromBeginning);

        SetLength (st, 5);        // Read XMsg: prefix
        Owner.fMessageFile.Read(st [1], 5);

        if st = 'X-Msg' then      // New style - contains extra header lines
        begin
          SetLength (st, 9);
          Owner.fMessageFile.Read(st [1], 9);
          st [1] := '$';
          len := StrToInt (st);

          Owner.fMessageFile.Read(hLen, SizeOf (hLen));

          while hLen > 0 do
          begin
            SetLength (st, hLen);
            Owner.fMessageFile.read (st [1], hLen);
            Owner.fMessageFile.Read(hLen, SizeOf (hLen));
          end;

          r := TStringList.Create;
          rCreated := True;
          m := TMemoryStream.Create;
          if len > 2048 then len := 2048;
          m.CopyFrom(Owner.fMessageFile, len);
          m.Seek(0, soFromBeginning);
          r.LoadFromStream(m)
        end
      end;

    st := '';
    if Assigned (r) then
    begin
      i := 0;
      while i < r.Count do
      begin
        st := r [i];

        if st <> '' then
        begin
          if AnsiContainsText (st, 'in article') or AnsiContainsText (st, 'wrote in message') or AnsiContainsText (st, 'wrote:') or AnsiContainsText (st, 'said:') then
          begin
            st := '';
            repeat
              Inc (i);
            until (i >= r.Count) or (Copy (r [i], 1, 1) = '>');

            repeat
              Inc (i);
            until (i  >= r.Count) or (r [i] = '');

          end
          else
            if st [1] = '>' then
            begin
              st := '';
              repeat
                Inc (i)
              until (i >= r.Count) or (r [i] = '')
            end
            else
            if Copy (st, 1, 2) = '<<' then
            begin
              st := '';
              repeat
                Inc (i)
              until (i >= r.Count) or AnsiContainsText (r [i], '>>')
            end
        end
        else
          Inc (i);

        if st <> '' then
        begin
          fInterestingMessageLine := TrimEx (st);
          break
        end
      end
    end
  finally
    m.Free;
    if rCreated then
      r.Free
  end;

  st := Trim (st);
  if (st = '') or (st = '-- ') then
    result := DecodeSubject(Subject, CodePage)
  else
    result := st
end;

function TArticle.MatchesKeyPhrase (st : string; searcher: TStringSearcher) : boolean;
var
  thing : Integer;
  st1 : string;
  tp : TStrings;
begin
  thing := 0;

  if Pos (':', st) > 0 then
    if AnsiStartsText ('Subject:', st) then
      thing := 1
    else
      if AnsiStartsText ('Author:', st) then
        thing := 2
      else
        if AnsiStartsText ('From:', st) then
          thing := 3
        else
          thing := 4;

  if thing > 0 then
  begin
    st1 := SplitString (':', st);
    case thing of
      1 : st1 := Subject;
      2 : st1 := FromName;
      3 : st1 := From;
      4 : begin
            st1 := GetHeader (st1);
            if st1 = '' then
              thing := -1
          end;
    end
  end
  else
  begin
    if not Assigned (Msg) then
      thing := -1
    else
    begin

      tp := Msg.TextPart;
      if Assigned (tp) then
        st1 := tp.Text
      else
        thing := -1
    end
  end;

  if thing >= 0 then
  begin
    searcher.Parse(st);
    result := searcher.Matches(st1);
  end
  else
    result := false;
end;

function TArticle.GetKeyPhraseNo: Integer;
var
  i : Integer;
begin
  if (fFlags and fgScannedKeyPhrases) <> 0 then
    if (fFlags and fgKeyphraseMask) <> 0 then
    begin
      result := 0;
      i := fgKeyPhrase0;
      while (fFlags and i) = 0 do
      begin
        i := i shl 1;
        Inc (result)
      end
    end
    else
      result := -1
  else
    if TSubscribedGroup (Owner).Owner.ScanKeyPhrases and HasMsg then
    begin
      result := -1;

      for i := 0 to 7 do
      begin
        if Options.KeyPhrase [i] <> '' then
        begin
          if MatchesKeyPhrase (Options.KeyPhrase [i], NNTPAccounts.KeyPhraseSearcher [i]) then
          begin
            if result = -1 then
              result := i;
            fFlags := fFlags or (fgKeyPhrase0 shl i)
          end
        end
      end;
      fFlags := fFlags or fgScannedKeyPhrases;
      Owner.fFlagsDirty := True
    end
    else
      result := -1
end;

(*----------------------------------------------------------------------*
 | function TArticle.GetMsg : TmvMessage                                |
 |                                                                      |
 | Get the message body for the article                                 |
 |                                                                      |
 | The function returns the message body                                |
 *----------------------------------------------------------------------*)
function TArticle.GetMsg: TmvMessage;
begin
  result := GetMsgFromFile
end;


function TArticle.GetPostingHost: string;
const
  snntp = 'NNTP-Posting-Host';
  sxtrc = 'X-Trace';
begin
  if ((fFlags and (fgSpamNoPostingHost or fgSpamNoXTrace)) = 0) and (fPostingHost = '') then
  begin
    fPostingHost := Header [snntp];
    if fPostingHost = '' then
    begin
      fFlags := fFlags or fgSpamNoPostingHost;
      fPostingHost := Header [sxtrc];
      if fPostingHost = '' then
        fFlags := fFlags or fgSpamNoXTrace
    end
  end;
  result := fPostingHost
end;

function TArticle.GetSubscribedGroup: TSubscribedGroup;
begin
  result := TSubscribedGroup (Owner);
end;

function TArticle.PeekAtMsgHdr(const hdr: string): string;
begin
  result := PeekAtMsgHDrFromFile (hdr)
end;

(*----------------------------------------------------------------------*
 | TArticle.RemoveMessage                                               |
 |                                                                      |
 | Remove an article's message.  eg. when the message download fails    |
 | half way through.                                                    |
 *----------------------------------------------------------------------*)
procedure TArticle.RemoveMessage;
begin
  FreeAndNil (fMsg);
  fMessageOffset := $ffffffff;
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
  st : string;
  i : Integer;
  hLen : Word;
begin
  fOwner.OpenMessageFile;
  fOwner.fMessageFile.Seek(0, soFromEnd);
  fMessageOffset := fOwner.fMessageFile.Position;
  st := 'X-Msg:'+ IntToHex (fMsg.RawData.Size, 8);
  fOwner.fMessageFile.Write(st [1], Length (st));

  for i := 0 to fMsg.Header.Count - 1 do
  begin
    st := fMsg.Header [i];
    hLen := Length (st);
    fOwner.fMessageFile.Write(hLen, SizeOf (hLen));
    fOwner.fMessageFile.Write(st [1], Length (st))
  end;

  hLen := 0;
  fOwner.fMessageFile.Write(hLen, SizeOf (hLen));

  fMsg.RawData.Seek(0, soFromBeginning);
  fMsg.RawData.SaveToStream(fOwner.fMessageFile);
end;

procedure TArticle.SetCodePage(const Value: Integer);
begin
  inherited SetCodePage(Value);

  if Assigned (fMsg) then
    fMsg.Codepage := Value
end;

procedure TSubscribedGroup.GroupArticles;
var
  i : Integer;
  art, root,ap : TArticle;
  subject, lastSubject : string;
  n, count, rootCount : Integer;
  filters : TFiltersCtnr;
  multipart : boolean;

  procedure CheckCompleteMultipart (root : TArticle);
  var
    c : Integer;
    p : TArticle;
  begin
    if rootCount <= 1 then
      root.fMultipartFlags := mfNotMultipart
    else
    begin
      root.fMultipartFlags := mfCompleteMultipart;
      if root.fPartNo = 1 then
      begin
        p := TArticle (root.fChild);
        c := 2;
        while (p <> Nil) and (p.fPartNo <= c) do
        begin
          if p.fPartNo = c then
            Inc (c);
          p := TArticle (p.Sibling)
        end;

        if (p <> nil) or (c - 1 <> rootCount) then
          root.fMultipartFlags := mfPartialMultipart
      end
      else
        root.fMultipartFlags := mfPartialMultipart
    end
  end;

begin { GroupArticles }
  fThreads.Clear;
  fArticles.Sort(CompareThreads);

  i := 0;
  root := Nil;
  lastSubject := '';

  filters := DisplayFiltersCtnr;

  while i < ArticleCount do
  begin
    art := TArticle (fArticles [i]);
    art.fSibling := Nil;

    if (HideMessagesNotToMe and not art.IsMine) or (hideReadMessages and art.IsRead) or (hideIgnoredMessages and art.IsIgnore) or (Assigned (filters) and filters.HasFilters and filters.BlockArticle (art)) then
    begin
      Inc (i);
      Continue
    end;

    subject := Trim (art.Subject);
    multipart := art.DecodeMultipartSubject (subject, n, count, False, True);
    if multipart then
    begin
      art.fSibling := Nil;
      art.fChild := Nil;
      art.fPartNo := n;
      if (subject <> lastSubject) or (root = Nil) then
      begin
        if Assigned (root) then
          CheckCompleteMultipart (root);
        lastSubject := subject;
        root := art;
        rootCount := count;
        fThreads.Add (art)
      end
      else
        if root.Child <> Nil then
        begin
          ap := TArticle (root.Child);
          while ap.Sibling <> Nil do
            ap := TArticle (ap.Sibling);
          ap.fSibling := art
        end
        else
          root.fChild := art;

    end
    else
    begin
      art.fSibling := Nil;
      art.fChild := Nil;
      if Assigned (root) then
        CheckCompleteMultipart (root);
      root := Nil;
      fThreads.Add(art)
    end;

    Inc (i)
  end;

  if root <> Nil then
    CheckCompleteMultipart (root);
end;

function TSubscribedGroup.GetLowestArticleNo: Integer;
var
  i : Integer;
begin
  result := MaxInt;

  for i := 0 to LoadGetArticleCount - 1 do
    if (articles [i].fArticleNo > 0) and (articles [i].fArticleNo < result) then
      result := articles [i].fArticleNo;

  if result = MaxInt then
    result := 0;
end;

function TSubscribedGroup.GetNext: TArticleContainer;
var
  i, idx : Integer;
begin
  result := nil;
  if Owner.fSecretGroupCount > 0 then
  begin
    idx := -1;

    for i := 0 to Owner.SubscribedGroupCount - 1 do
      if Owner.SubscribedGroups [i].Name = Name then
      begin
        idx := i;
        break
      end
  end
  else
    idx := Owner.fSubscribedGroups.IndexOf(Name);
  if idx >= 0 then
  begin
    Inc (idx);
    if idx < Owner.SubscribedGroupCount then
      result := Owner.SubscribedGroups [idx]
  end;
end;

function TSubscribedGroup.GetArticle (idx : Integer) : TArticle;
begin
  result := TArticle (ArticleBase [idx]);
end;

function TSubscribedGroup.MarkOnLeave: boolean;
begin
  result := Owner.MarkOnLeave
end;

function TSubscribedGroup.GetServerSettings: TServerSettings;
begin
  result := Owner.fNNTPServerSettings
end;

function TSubscribedGroup.GetIdentity: TIdentity;
begin
  result := NNTPSettings.Identity
end;

function TSubscribedGroup.GetUnreadArticleCount: Integer;
var
  i, l : Integer;
  art : TArticleBase;

begin
  result := 0;

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
        art := ArticleBase [i];
        if not art.IsRead then
        begin
          Inc (result);
          if art.IsMine then
            Inc (fUnreadArticleToMeCount);
          if art.IsReply then
            Inc (fUnreadReplyCount);
          if art.IsXanaNews then
            Inc (fUnreadXananewsArticleCount);
        end;
        if art.IsInteresting then
        begin
          Inc (fInterestingArticleCount);
          if not art.IsRead then
            Inc (fUnreadInterestingArticleCount)
        end;
        l := art.ArticleNo;
        if l <> 0 then
          Inc (fAdjustedArticleCount);
        if (l > fRawLastArticle) and (l <> 99999999) then
            fRawLastArticle := l;
      end;
      fUnreadArticleCount := result;
    end
    else
    begin
      LSSync.Enter;
      try
        QuickLoadHeaderStats
      finally
        LSSync.Leave
      end
    end;

    fAdjustedArticleCount := fUnloadedArticleCount
  end
  else
    result := fUnreadArticleCount
end;

procedure TSubscribedGroup.QuickLoadHeaderStats;
var
  st, fileName : string;
  pc, pc1 : PChar;
  reader : TTextFileReader;
  n, l, fgs : Integer;
begin
  // Already LSSync-ed when this is called
  fileName := gMessageBaseRoot + '\' + FixFileNameString (Owner.AccountName) + '\' + FixFileNameString (Name) + '\articles.dat';
  if FileExists (fileName) then
    reader := TTextFileReader.Create(fileName)
  else
    reader := Nil;
  fUnreadArticleCount := 0;
  if Assigned (reader) then
  try
    fUnloadedArticleCount := 0;
    fRawLastArticle := 0;
    while reader.ReadLn(st) do
    try
      Inc (fUnloadedArticleCount);
      n := 0;
      pc := PChar (st);
      l := 0;
      while n < 8 do
      begin
        pc1 := pc;
        pc := StrScan (pc, #9);

        if pc <> Nil then
        begin
          if n = 0 then
          begin
            pc^ := #0;
            l := StrToIntDef (pc1, 0);
          end;

          Inc (pc);
          Inc (n)
        end
        else
          break
      end;

      if (l > fRawLastArticle) and (l <> 99999999) then
        fRawLastArticle := l;

      if Assigned (pc) then
      begin
        pc1 := StrScan (pc, #9);
        if Assigned (pc1) then
          pc1^ := #0
      end;

      if Assigned (pc) then
        fgs := StrToCard (pc)
      else
        fgs := 0;

      if (fgs and fgRead) = 0 then
      begin
        Inc (funreadArticleCount);
        if (fgs and fgMine) <> 0 then
          Inc (fUnreadArticleToMeCount);
        if (fgs and fgXanaNews) <> 0 then
          Inc (fUnreadXananewsArticleCount);
        if (fgs and fgReply) <> 0 then
          Inc (fUnreadReplyCount);
      end;
      if (fgs and fgInteresting) <> 0 then
      begin
        Inc (fInterestingArticleCount);
        if (fgs and fgRead) = 0 then
          Inc (fUnreadInterestingArticleCount)
      end
    except
    end
  finally
    reader.Free
  end
end;

function TSubscribedGroup.GetPostingSettings: TPostingSettings;
begin
  result := fPostingSettings
end;

function TSubscribedGroup.GetDisplaySettings: TDisplaySettings;
begin
  result := fDisplaySettings
end;

procedure TSubscribedGroup.AddRawHeaders(headers: TTextFileReader);
var
  articleNo : cardinal;
  subject : string;
  from : string;
  date : TDateTime;
  MessageID : string;
  references : string;
  Bytes : Cardinal;
  lines : Cardinal;
  exd, s, st : string;

  article : TArticle;

begin
  if not fArticlesLoaded then
    LoadArticles;

  while headers.ReadLn (st) do
  begin
    ParseXOVER (st, articleNo, subject, from, date, MessageID, references, bytes, lines, exd);
    article := TArticle.Create (self);

    article.fArticleNo := articleNo;
    article.fMessageID := Trim (MessageID);
    article.fBytes := bytes;
    article.fLines := lines;
    article.fReferences := Trim (references);
    article.fFrom := from;
    article.fSubject := subject;
    article.fDate := date;

    s := Fetch (exd, #9);
    article.fFlags := StrToCard (s) and $08ffffff;
    if article.IsDeleted then
      article.Owner.fNeedsPurge := True;

    s := Fetch (exd, #9);
    article.fMessageOffset := Cardinal (StrToIntDef (s, -1));

    RawAddArticle (article)
  end;


  NNTPAccounts.PerfCue (660, 'Added Article Headers');
  ReSortArticles;
  NNTPAccounts.PerfCue (880, 'Sorted articles');
  fArticlesLoaded := True;
end;

procedure TArticle.SetCrossPostsFlag(flag: DWORD; value: boolean);
var
  xref, st, group : string;
  artNo : Integer;
  xCtnr : TArticleContainer;
  art : TArticleBase;
begin
  xref := Header ['Xref'];
  st := SplitString (' ', xref);        // First entry *may* be the server name
                                        // if so, discard it.

  if Pos (':', st) = 0 then             // Get the first XRef
    st := SplitString (' ', xref);

  while st <> '' do
  begin
    group := SplitString (':', st);     // Split XRef into group & article no
    artNo := StrToIntDef (st, 0);

    if artNo = 0 then Break;
                                        // Check group isn't *this* group
    if not SameText (group, SubscribedGroup.Name) then
    begin
                                        // Find the subscribed group - in this account!
      xCtnr := nntpAccounts.FindArticleContainer(Account.AccountName, group);

      if Assigned (xCtnr) then
      begin
        art := xCtnr.FindArticleNo(artNo);
        if Assigned (art) then
        begin
          art.SetFlag(flag, value);
          art.Owner.fUnreadArticleCount := -1;
          art.Owner.fUnloadedArticleCount := -1;
        end
      end
    end;
    st := SplitString (' ', xref);  // Get the server
  end
end;


procedure TArticle.SetIsRead(const Value: boolean);
begin
  inherited;

  if Options.AutoCrosspostDetect then
    SetCrossPostsFlag (fgRead, Value)
end;

function TArticle.GetHeader(const name: string): string;
var
  s, t, h : string;
begin
  result := inherited GetHeader (name);
  if (result = '') and (fTempExtraHeaders <> '') then
  begin
    t := fTempExtraHeaders;
    while t <> '' do
    begin
      s := Fetch (t, #9);
      h := Fetch (s, ':');
      if SameText (name, h) then
      begin
        result := s;
        break
      end
    end
  end
end;

function TArticle.GetHasAnyKeyPhrase: boolean;
begin
  result := GetKeyPhraseNo <> -1;
end;

procedure TArticle.RawMarkAsRead;
begin
  SetFlag (fgRead, true);
  Owner.fUnreadArticleCount := -1;
  Owner.fUnloadedArticleCount := -1;
end;

{ TFiltersCtnr }

procedure TFiltersCtnr.AssignFilters(ctnr: TFiltersCtnr);
var
  i : Integer;
begin
  if Assigned (ctnr.fFilters) then
    if Assigned (fFilters) then
      fFilters.Clear
    else
      fFilters := TNNTPFilters.Create (false)
  else
    if Assigned (fFilters) then
      FreeAndNil (fFilters);

  if Assigned (ctnr.fFilters) then
    for i := 0 to ctnr.fFilters.Count - 1 do
      fFilters.AddObject(ctnr.Filters.Strings [i], ctnr.Filters.Objects [i]);

  fParent := ctnr.fParent;
end;

function TFiltersCtnr.BlockArticle(article: TArticleBase): boolean;
var
  n : TFiltersCtnr;
  i : Integer;
  filter : TNNTPFilter;
begin
  n := self;
  result := False;
  // Clear filter tags.
  for i := 0 to AllFilters.Count - 1 do
    TNNTPFilter (AllFilters [i]).Tag := 0;

  if Assigned (n.Filters) then
    for i := 0 to n.Filters.Count - 1 do
      n.Filters [i].Tag := 0;

  while not result and (n <> Nil) do
  begin
    if Assigned (n.Filters) then
    begin
      for i := 0 to n.Filters.Count - 1 do
      begin
        filter := n.Filters [i];
        if filter.Tag = 0 then
        begin
          filter.Tag := 1;      // Set tag on the filter.  This indicates
                                // that this filter has been processed
          if FilterEnabled (filter) and filter.Matches(article) then
          begin
            result := True;
            break
          end
        end
      end
    end;
    if not result then n := n.fParent
  end
end;

procedure TFiltersCtnr.Clear;
begin
  FreeAndNil (fFilters);
end;

constructor TFiltersCtnr.Create (AOwner : TObject; AParent : TFiltersCtnr);
begin
  fOwner := AOwner;
  fParent := AParent
end;

destructor TFiltersCtnr.Destroy;
begin
  fFilters.Free;

  inherited;
end;

procedure TFiltersCtnr.EnableFilter(filter: TNNTPFilter;
  enable: boolean);
var
  i : Integer;
begin
  if not Assigned (filter) then Exit;
  if enable <> FilterEnabled (filter) then
  begin
    if Assigned (fFilters) then
      i := fFilters.IndexOfFilter(filter)
    else
      i := -1;

    if (i = -1) and not Assigned (fFilters) then
      fFilters := TNNTPFilters.Create (false);

    case enable of
      true :                                    // Enabling...
        if i <> -1 then
          fFilters.Delete (i)                   // Existing filter, - so it must say 'off'.  Delete it.
        else
          fFilters.AddObject('', filter);       // No existing filter.  Add one.

      false:                                    // Disabling...
        if i <> -1 then
          fFilters.Delete(i)                    // Existing filter.  Delete it.
        else
          fFilters.AddObject('off', filter)     // No existing filter so it must be enabled on a higher
                                                // level.  Add an 'off' at this level.
    end;

    if fFilters.Count = 0 then
      FreeAndNil (fFilters)
  end
end;

function TFiltersCtnr.FilterEnabled (filter : TNNTPFilter) : boolean;
var
  n : TFiltersCtnr;
  i : Integer;
begin
  n := self;
  result := False;
  if filter = Nil then
    Exit;

  while n <> Nil do
  begin
    if Assigned (n.Filters) then
      i := n.Filters.IndexOfFilter(filter)
    else
      i := -1;
    if i <> -1 then
    begin
      if filter.Dormant then
        n.fFilters.Delete(i)
      else
      begin
        result := n.Filters.Strings [i] <> 'off';
        break
      end
    end
    else
      n := n.fParent
  end
end;

function TFiltersCtnr.GetCount: Integer;
begin
  if Assigned (fFilters) then
    result := fFilters.Count
  else
    result := 0
end;

function TFiltersCtnr.GetHasFilters: boolean;
var
  n : TFiltersCtnr;
begin
  n := self;
  result := False;
  while (not result) and Assigned (n) do
  begin
    result := n.Count > 0;
    n := n.Parent
  end
end;

procedure TFiltersCtnr.LoadFilters(reg : TExSettings; displayFilters : boolean);
var
  reg1 : TExSettings;
  valueNames : TStrings;
  i : Integer;
  nm : string;
  filter : TNNTPFilter;
  keyname : string;
begin
  FreeAndNil (fFilters);
  valueNames := Nil;

  if displayFilters then
    keyname := 'Display Filters'
  else
    keyName := 'Active Filters';

  if reg.HasSection (keyName) then
  begin

    reg1 := CreateChildSettings (reg, keyName);
    try
      valueNames := TStringList.Create;


      reg1.GetValueNames(valueNames);

      if valueNames.Count > 0 then
      begin
        fFilters := TNNTPFilters.Create (false);

        for i := 0 to valueNames.Count - 1 do
        begin
          nm := valueNames [i];
          filter := AllFilters.FindFilter (nm);
          if filter = Nil then
            reg1.DeleteValue(nm)
          else
            fFilters.AddObject (reg1.StringValue [nm], filter)
        end
      end

    finally
      valueNames.Free;
      reg1.Free
    end
  end
end;

procedure TFiltersCtnr.SaveFilters(reg : TExSettings; displayFilters : boolean);
var
  reg1 : TExSettings;
  valueNames : TStrings;
  i, idx : Integer;
  filter : TNNTPFilter;
  keyName : string;
begin
  valueNames := Nil;
  if displayFilters then
    keyName := 'Display Filters'
  else
    keyName := 'Active Filters';
  reg1 := CreateChildSettings (reg, keyName);
  try
    valueNames := TStringList.Create;

    i := 0;
    while i < Count do
      if fFilters [i].Dormant then
        fFilters.Delete(i)
      else
        Inc (i);

    if Count = 0 then
      reg1.DeleteSection (keyName)
    else
    begin
      reg1.GetValueNames(valueNames);

      for i := 0 to fFilters.Count - 1 do
      begin
        filter := fFilters [i];
        if filter.Temporary then
          continue;


        idx := valueNames.IndexOf(filter.Name);

        if idx = -1 then
          reg1.SetStringValue (filter.Name, fFilters.Strings [i], '~')
        else
          valueNames.Delete(idx)
      end;

      for i := 0 to valueNames.Count - 1 do
        reg1.DeleteValue(valueNames [i])
    end;

  finally
    reg1.Free;
    valueNames.Free
  end
end;

{ TArticleStack }

procedure TArticleStack.Clear;
var
  oldCapacity : Integer;
begin
  oldCapacity := fList.Capacity;
  fList.Clear;
  fList.Capacity := oldCapacity
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
  inherited;
end;

function TArticleStack.GetCapacity: Integer;
begin
  result := fList.Capacity
end;

function TArticleStack.GetIsEmpty: boolean;
begin
  result := fList.Count = 0
end;

function TArticleStack.Peek: TArticleBase;
var
  MsgID, group, account : string;
begin
  if fList.Count > 0 then
  begin
    account := fList [0];
    MsgID := SplitString ('=', account);
    group := SplitString ('=', account);

    result := NNTPAccounts.FindMsgID(account, group, MsgID)
  end
  else
    result := Nil;
end;

function TArticleStack.Pop: TArticleBase;
begin
  result := Peek;
  if fList.Count > 0 then
    fList.Delete (0)
end;

procedure TArticleStack.Push(article: TArticleBase);
var
  accName, st : string;
begin
  if article = Nil then
    Exit;

  if fList.Count = fList.Capacity then
    fList.Delete (fList.Count - 1);

  accName := '';

  try
    if article is TArticle then
      accName := TArticle (article).Account.AccountName
    else
      if article is TFolderArticle then
        accName := cFolders;

    if accName <> '' then
    begin
      st := article.MessageId+'='+article.Owner.Name + '=' + accName;
      if fList.IndexOf(st) = -1 then
        fList.Insert(0, st);
    end
  except
  end
end;

{ TArticleContainer }

procedure TArticleContainer.BeginLock;
begin
  Inc (fLockCount);
end;

procedure TArticleContainer.ClearThreadInfo;
var
  i : Integer;
  article : TArticleBase;
begin
  if fThreads.Count > 0 then
    fThreads.Clear;
  i := 0;
  while i < LoadGetArticleCount do
  begin
    article := ArticleBase [i];
    if article.ArticleNo = 0 then
      RawDeleteArticle (i)
    else
    begin
      article.fChild := Nil;
      article.fParent := Nil;
      article.fSibling := Nil;

      Inc (i)
    end
  end
end;

constructor TArticleContainer.Create(const AName: string; AFiltersParent, ADisplayFiltersParent : TFiltersCtnr);
begin
  inherited Create (AName);
  fUnreadArticleCount := -1;
  fUnloadedArticleCount := -1;
  fFiltersCtnr := TFiltersCtnr.Create (self, AFiltersParent);
  fDisplayFiltersCtnr := TFiltersCtnr.Create(self, ADisplayFiltersParent);
  fThreads := TList.Create;
  fSortBuf := TList.Create;
  fThreadSortOrder := soMessageNo;
end;

procedure TArticle.SetMsg(const Value: TmvMessage);
begin
  fMsg := Value
end;

destructor TArticleContainer.Destroy;
begin
  fFiltersCtnr.Free;
  fDisplayFiltersCtnr.Free;
  fMessageFile.Free;
  fThreads.Free;
  fSortBuf.Free;

  inherited;
end;

procedure TArticleContainer.EndLock;
begin
  Dec (fLockCount);
  if fLockCount < 0 then
    fLockCount := 0;
end;

function TArticleContainer.FindArticleNo(cno: Integer): TArticleBase;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to LoadGetArticleCount - 1 do
    if ArticleBase [i].ArticleNo = cno then
    begin
      result := ArticleBase [i];
      break
    end
end;

function TArticleContainer.FindChild(article,
  child: TArticleBase): boolean;
var
  p : TArticleBase;
begin
  p := article.Child;

  result := False;

  while p <> Nil do
  begin
    result := child = p;
    if result then break;

    result := FindChild (p, child);
    if result then break;
    p := p.Sibling
  end
end;

function TArticleContainer.FindMsgID(const msgID: string): TArticleBase;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to LoadGetArticleCount - 1 do
    if ArticleBase [i].MessageId = msgID then
    begin
      result := ArticleBase [i];
      break
    end
end;

function TArticleContainer.FindUniqueID(const msgID: string): TArticleBase;
begin
  result := FindMsgID (msgID);
end;

procedure TArticleContainer.GatherSubjects (article : TArticleBase);
var
  count : Integer;
  c, t, old, prev, rest, tail, newc : TArticleBase;
  subject_table : PPhashItem;
  subj : string;
  hash : DWORD;
begin
  subject_table := AllocHashTable;
  try
    c := article.Child;
    count := 0;

    // Add each root article's simplified subject to the subject table
    while c <> Nil do
    begin
      if c.ArticleNo = 0 then
      begin
        t := c.Child;
        if t <> Nil then
          while t.fSibling <> Nil do
            t := t.fSibling
      end
      else
        t := c;

      if t <> Nil then
        subj := t.SimplifiedSubject
      else
        subj := '';

      if subj <> '' then
      begin
        hash := HashOf (subj);
        old := FindHashSubject (subject_table, hash, subj);

        // Put it in the table if...
        if (old = Nil) or                         // ... it's not already there
           ((c.ArticleNo = 0) and (old.ArticleNo <> 0)) or
                                                  // ... this is a dummy and the previous one wasn't
           ((old.ArticleNo <> 0) and (old.SubjectIsReply) and (c.ArticleNo <> 0) and not (c.SubjectIsReply)) then
                                                  // ... this is not a 're' reply and the previous one was.
        begin
          AddHash (subject_table, hash, c);
          Inc (count)
        end
      end;

      c := c.Sibling
    end;

    if count = 0 then
      Exit;     // Nothing to do.

    prev := Nil;
    c := article.Child;
    rest := c.Sibling;

// The subj_table is now populated with one entry for each subject which
// occurs in the root set.  Now iterate over the root set, and gather
// together the difference.

    while c <> Nil do
    begin
      if c.ArticleNo <> 0 then
        t := c
      else
      begin
        t := c.Child;
        if t <> Nil then
          while t.fSibling <> Nil do
            t := t.fSibling
      end;

      if t <> Nil then
        subj := t.SimplifiedSubject
      else
        subj := '';

      if subj <> '' then        // Let empty subjects dangle
      begin
        hash := HashOf (subj);
                                // Unless somethings gone wrong,
                                // the 'Find' will work
        old := FindHashSubject (subject_table, hash, subj);

        if (old <> Nil) and (old <> c) then
        begin
          if prev = Nil then
            article.fChild := c.fSibling
          else
            prev.fSibling := c.fSibling;

          c.fSibling := Nil;

          if (old.ArticleNo = 0) and (c.ArticleNo = 0) then
          begin
            tail := old.fChild;
            while (tail <> Nil) and (tail.fSibling <> Nil) do
              tail := tail.fSibling;
            tail.fSibling := c.fChild;

            tail := c.fChild;

            while tail <> Nil do
            begin
              tail.fParent := old;
              tail := tail.fSibling
            end;

            c.fSibling := Nil
          end
          else
          if (old.ArticleNo = 0) or ((c.ArticleNo <> 0) and c.SubjectIsReply and not old.SubjectIsReply) then
          begin
            c.fParent := old;
            c.fSibling := old.fChild;
            old.fChild := c
          end
          else
          begin
            newc := TArticle.Create(self);
            newc.fArticleNo := old.fArticleNo;
            newc.fMessageOffset := old.fMessageOffset;
            newc.Assign(old);

            newc.fChild := old.fChild;
            tail := newc.fChild;
            while tail <> Nil do
            begin
              tail.fParent := newc;
              tail := tail.fSibling
            end;

            old.fArticleNo := 0;
            old.fMessageOffset := $ffffffff;
            old.fChild := Nil;
            c.fParent := old;
            newc.fParent := old;
            old.fChild := c;
            c.fSibling := newc
          end;
          c := prev
        end
      end;

      prev := c;
      c := rest;
      if rest <> Nil then
        rest := rest.Sibling
      else
        rest := Nil
    end
  finally
    FreeHashTable (subject_table);
  end
end;

function TArticleContainer.GetAdjustedArticleCount: Integer;
begin
  GetUnreadArticleCount;
  result := fAdjustedArticleCount;
end;

function TArticleContainer.GetDisplaySettings: TDisplaySettings;
begin
  result := NNTPAccounts.DisplaySettings
end;

function TArticleContainer.GetFirstArticle: TArticleBase;
begin
  if LoadGetArticleCount = 0 then
    Result := nil
  else
    if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) then
      if ThreadCount > 0 then
        Result := Threads [0]
      else
        result := Nil
    else
      Result := ArticleBase [0]
end;


function TArticleContainer.GetInterestingArticleCount: Integer;
begin
  GetUnreadArticleCount;
  result := fInterestingArticleCount;
end;

function TArticleContainer.GetMessagebaseSize: Int64;
begin
  result := 0;
end;

function TArticleContainer.GetThreadCount: Integer;
begin
  result := ArticleCount
end;

function TArticleContainer.GetThreads(idx: Integer): TArticleBase;
begin
  result := ArticleBase [idx]
end;

function TArticleContainer.GetUnreadArticleToMeCount: Integer;
begin
  GetUnreadArticleCount;
  result := fUnreadArticleToMeCount
end;

function TArticleContainer.GetUnreadInterestingArticleCount: Integer;
begin
  GetUnreadArticleCount;
  result := fUnreadInterestingArticleCount;
end;

function TArticleContainer.GetUnreadReplyCount: Integer;
begin
  GetUnreadArticleCount;
  result := fUnreadReplyCount;
end;

function TArticleContainer.GetUnreadXanaNewsArticleCount: Integer;
begin
  GetUnreadArticleCount;
  result := fUnreadXanaNewsArticleCount
end;

procedure TArticleContainer.GroupArticles;
begin
// stub
end;

function TArticleContainer.IndexOf(article: TArticleBase): Integer;
begin
  result := 0;

end;

procedure TArticleContainer.LeaveGroup(clearMessages: boolean);
begin
   fFocused := False
end;

procedure TArticleContainer.LoadArticleCounts(reg : TExSettings);
begin
// stub
end;

function TArticleContainer.LoadGetArticleCount: Integer;
begin
  if not Loaded then
    LoadArticles;
  result := ArticleCount
end;

function TArticleContainer.Locked: boolean;
begin
  result := fLockCount <> 0
end;

function TArticleContainer.MarkOnLeave: boolean;
begin
  result := False
end;

procedure TArticleContainer.OpenMessageFile;
begin
end;

procedure TArticleContainer.PruneDummyArticles(article: TArticleBase);
var
  prev, next, p, tail, kids : TArticleBase;
begin
  prev := Nil;
  p := article.Child;
  next := p.Sibling;

  while p <> Nil do
  begin
    if (p.ArticleNo = 0) and (p.Child <> Nil) then
      p.fFlags := p.Flags or (p.Child.Flags and (fgInteresting or fgIgnore));

    if (p.ArticleNo = 0) and (p.Child = Nil) then
    begin
      if prev = Nil then
        article.fChild := p.fSibling
      else
        prev.fSibling := p.fSibling;

      RawRemoveArticle (p);
      p := prev
    end
    else
    if (p.ArticleNo = 0) and (p.Child <> Nil) and ((p.Parent <> Nil) or (p.Child.Sibling = Nil)) then
    begin
      kids := p.Child;
      if prev = Nil then
        article.fChild := kids
      else
        prev.fSibling := kids;

      tail := kids;
      while tail.Sibling <> Nil do
      begin
        tail.fParent := p.fParent;
        tail := tail.fSibling
      end;

      tail.fParent := p.fParent;
      tail.fSibling := p.fSibling;

      next := kids;
      RawRemoveArticle (p);
      p := prev
    end
    else
    if p.Child <> Nil then
      PruneDummyArticles (p);

    prev := p;
    p := next;
    if p = nil then
      next := Nil
    else
      next := p.fSibling
  end
end;

procedure TArticleContainer.RawSortArticles;
begin
// stub
end;

procedure TArticleContainer.ResetBozoFlags;
var
  i : Integer;
  art : TArticleBase;
begin
  for i := 0 to LoadGetArticleCount -1 do
  begin
    art := ArticleBase [i];
    art.fFlags := art.fFlags and not fgScannedBozo
  end
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

procedure TArticleContainer.SaveArticleCounts(reg : TExSettings);
begin
// stub
end;

procedure TArticleContainer.SetHideIgnoredMessages(const Value: boolean);
begin
  if fHideIgnoredMessages <> Value then
  begin
    fHideIgnoredMessages := Value;
    ResortArticles
  end
end;

procedure TArticleContainer.SetHideMessagesNotToMe(const Value: boolean);
begin
  if fHideMessagesNotToMe <> Value then
  begin
    fHideMessagesNotToMe := Value;
    ResortArticles
  end
end;

procedure TArticleContainer.SetHideReadMessages(const Value: boolean);
begin
  if fHideReadMessages <> Value then
  begin
    fHideReadMessages := Value;
    ResortArticles
  end
end;

procedure TArticleContainer.SetSecret(const Value: boolean);
begin
  fSecret := Value;
end;

procedure TArticleContainer.SetThreadOrder(const Value: TThreadOrder);
begin
  if value <> fThreadOrder then
  begin
    fThreadOrder := value;
    SortArticles
  end
end;

procedure TArticleContainer.SetThreadSortDirection(
  const Value: TThreadSortDirection);
begin
  if fThreadSortDirection <> Value then
  begin
    fThreadSortDirection := Value;
    SortArticles
  end
end;

procedure TArticleContainer.SetThreadSortOrder(
  const Value: TThreadSortOrder);
begin
  if fThreadSortOrder <> Value then
  begin
    fThreadSortOrder := Value;
    SortArticles
  end
end;

procedure TArticleContainer.SortArticles;
var
  i, idx : Integer;
  l, n : TArticleBase;
  spamAddresses : TStringList;
  spamNames : TStringList;
  lastPostingHost : string;
  lastAuthor : string;
  filters : TFiltersCtnr;
begin
  if (fSorting) or
     ((fCurrentThreadOrder = fThreadOrder) and
     (fCurrentThreadSortOrder = fThreadSortOrder) and
     (fCurrentThreadSortDirection = fThreadSortDirection)) then
     exit;

  fDisplayFiltered := False;
  fSorting := True;
  BeginLock;
  try
    if ThreadOrder = toThreaded then
    begin
      ThreadArticles;
      SortThreads
    end
    else
    begin
      if ThreadSortOrder = soSubject then
      begin
        GroupArticles;
        SortThreads
      end
      else
      begin
        ClearThreadInfo;
        RawSortArticles;

        if self is TSubscribedGroup then
          filters := TSubscribedGroup (self).DisplayFiltersCtnr
        else
          filters := Nil;

        fDisplayFiltered := Assigned (filters) and filters.HasFilters;

        if (ThreadSortOrder = soPostingHost) and (ArticleCount > 0) then
        begin
          spamAddresses := Nil;
          spamNames := Nil;
          try
            spamAddresses := TStringList.Create;
            spamAddresses.Sorted := True;
            spamAddresses.Duplicates := dupIgnore;
            lastAuthor := '';
            lastPostingHost := '';

            spamNames := TStringList.Create;
            spamNames.Sorted := True;
            spamNames.Duplicates := dupIgnore;

            l := ArticleBase [0];

            if HideMessagesNotToMe or hideReadMessages or hideIgnoredMessages or fDisplayFiltered then
              fThreads.Add(l);

            for i := 1 to ArticleCount - 1 do
            begin
              n := ArticleBase [i];

              if (HideMessagesNotToMe and not n.IsMine) or (hideReadMessages and n.IsRead) or (hideIgnoredMessages and n.IsIgnore) or (fDisplayFiltered and filters.BlockArticle (n)) then
                Continue;

              if HideMessagesNotToMe or hideReadMessages or hideIgnoredMessages or fDisplayFiltered then
                fThreads.Add(n);

              l.fSibling := n;
              l.fMultipartFlags := mfNotMultipart;
              l := n;
              if spamAddresses <> Nil then
              begin
                if (lastPostingHost = n.PostingHost) and (CompareText (Copy (lastAuthor, 1, 5), Copy (n.FromName, 1, 5)) <> 0) then
                begin
                  spamNames.Add(lastAuthor);
                  spamAddresses.Add(lastPostingHost)
                end;
                lastPostingHost := n.PostingHost;
                lastAuthor := n.FromName
              end
            end;

            l.fSibling := nil;
            l.fMultipartFlags := mfNotMultipart;

            if (spamAddresses <> Nil) and (spamAddresses.Count > 0) then
            begin
              for i := 0 to ArticleCount - 1 do
              begin
                n := ArticleBase [i];
                if spamAddresses.Find(n.PostingHost, idx) then
                  n.fFlags := n.fFlags or fgSpamSharesPostingHost;
                if spamNames.Find (n.FromName, idx) then
                  n.fFlags := n.fFlags or fgSpamSharesName
              end
            end;

          finally
            spamAddresses.Free;
            spamNames.Free;
          end
        end
        else
        begin
          l := Nil;
          for i := 0 to ArticleCount - 1 do
          begin
            n := ArticleBase [i];
            if (HideMessagesNotToMe and not n.IsMine) or (hideReadMessages and n.IsRead) or (hideIgnoredMessages and n.IsIgnore) or (fDisplayFiltered and filters.BlockArticle (n)) then
              Continue;

            if HideMessagesNotToMe or hideReadMessages or HideIgnoredMessages or fDisplayFiltered then
              fThreads.Add(n);

            if l <> Nil then
              l.fSibling := n;
            l := n
          end
        end
      end
    end;

    fCurrentThreadOrder := fThreadOrder;
    fCurrentThreadSortOrder := fThreadSortOrder;
    fCurrentThreadSortDirection := fThreadSortDirection
  finally
    fSorting := False;
    EndLock
  end
end;

procedure TArticleContainer.SortSiblings(var root: TArticleBase);
var
  p : TArticleBase;
  i, ct : Integer;
begin
  if (root <> nil) and (root.Sibling <> nil) then
  begin
    fSortBuf.Clear;

    p := root;
    while p <> nil do
    begin
      fSortBuf.Add (p);
      p := p.Sibling
    end;

    fSortBuf.Sort(CompareThreads);

    root := TArticle (fSortBuf.List^ [0]);
    p := root;
    ct := fSortBuf.Count;
    i := 1;

    while i < ct do
    begin
      p.fSibling := TArticle (fSortBuf.List^ [i]);
      p := p.fSibling;
      Inc (i)
    end;

    p.fSibling := nil;

    p := root;
    while p <> nil do
    begin
      SortSiblings (p.fChild);
      p := p.Sibling
    end
  end
  else
    if (root <> Nil) then
      SortSiblings (root.fChild);
end;

procedure TArticleContainer.SortThreads;
var
  i, c : Integer;
  l, n : TArticle;
  oldThreadSortOrder : TThreadSortOrder;
  oldThreadSortDirection : TThreadSortDirection;
begin
  fThreads.Sort (CompareThreads);

  if (fThreads.Count > 0) and not ((ThreadOrder = toChronological) and (ThreadSortOrder = soSubject)) then
  begin
    l := TArticle (fThreads [0]);

    oldThreadSortOrder := threadSortOrder;
    oldThreadSortDirection := threadSortDirection;
    try
      threadSortOrder := soDate;
      threadSortDirection := sdAscending;

      c := fThreads.Count;
      for i := 1 to c do
      begin
        SortSiblings (l.fChild);
        if i < c then
          n := TArticle (fThreads.List^ [i])
        else
          n := Nil;
        l.fSibling := n;
        l := n
      end;
    finally
      fThreadSortOrder := oldThreadSortOrder;
      fThreadSortDirection := oldThreadSortDirection
    end
  end
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
  i, j : Integer;
  ps, pe, pss : PChar;
  len : Integer;
  ref : string;
  article, refArticle, prevArticle, prev, rest, tempRoot : TArticleBase;

  id_table : PPhashItem;
  hash : DWORD;
  hideReadMessages : boolean;
  hideIgnoredMessages : boolean;
  mine : boolean;
  filters : TFiltersCtnr;
  id : TIdentity;
  id_not_table : PPhashItem;
  reDo : boolean;

  Identities: TList;
  articleFromName : string;
  articleFromEmail : string;
begin
  reDo := True;
  while reDo do
  begin
    reDo := False;
    fThreads.Clear;
    hideReadMessages := self.HideReadMessages;
    hideMessagesNotToMe := self.HideMessagesNotToMe;
    hideIgnoredMessages := self.HideIgnoredMessages;

    if self is TSubscribedGroup then
      filters := TSubscribedGroup (self).DisplayFiltersCtnr
    else
      filters := Nil;

    if not filters.HasFilters then
      filters := Nil;

    Identities := nil;
    id_not_table := AllocHashTable;            // Create a separate table for ignored messages
    try
      id_table := AllocHashTable;
      try
        Identities := TList.Create;
        // initialize identities local cache
        for j := 0 to NNTPAccounts.Identities.Count - 1 do
          Identities.Add(NNTPAccounts.Identities [j]);

        i := 0;
        while i < ArticleCount do
        begin
          article := ArticleBase [i];
          article.fNewestMessage := -1;

          article.fParent := Nil;             // Must get rid of old thread pointers
          article.fSibling := Nil;            // otherwise we *really* bugger up!
          article.fChild := Nil;

          if (HideMessagesNotToMe and not article.IsMine) or (hideReadMessages and article.IsRead) or (hideIgnoredMessages and article.IsIgnore) or (Assigned (filters) and filters.BlockArticle (article)) then

          begin                                 // Ignore this message

            article.fBlocked := True;
                                                // Add it to hash of ignored msgs
            hash := HashOf (article.fMessageID);
            if FindHashMessage (id_not_table, hash, article.MessageId) = Nil then
              Addhash (id_not_table, hash, article);

            Inc (i);
            continue
          end;

          article.fBlocked := False;
          article.fMultipartFlags := mfNotMultipart;
          hash := HashOf (article.fMessageID);

          if FindHashMessage (id_table, hash, article.fMessageID) = Nil then
          begin
            Addhash (id_table, hash, article);  // There shouldn't be duplicates, but
                                                // it doesn't matter if there are - they
                                                // will 'point to' the same article.
            mine := False;
            articleFromName := article.FromName;
            articleFromEmail := article.FromEmail;
            for j := 0 to Identities.Count - 1 do
            begin
              id := TIdentity(Identities.List^ [j]);
              mine := (articleFromName = id.UserName) and (articleFromEmail = id.EMailAddress);
              if mine then
                break
            end;

            article.IsMine := mine;
            article.IsReply := false;
            if mine then
              article.fFlags := article.fFlags or fgTemp
            else
              article.fFlags := article.fFlags and not fgTemp;
            Inc (i)
          end
          else
            RawDeleteArticle (i)
        end;

                                            // Go thru the articles.
        for i := 0 to ArticleCount - 1 do
        begin
          article := ArticleBase [i];

          if article.fBlocked then          // It's been removed by
            Continue;                       // Hide Read Messages, etc.

          ps := PChar (article.references);

          prevArticle := Nil;

          // Go thru each entry in 'References'

          // 1.17.5.8.  RFC 1036 says that References must be space
          // separated, but RFC 2822 suggests that the space is optional.
          // So handle either.

          while ps^ <> #0 do
          begin
            pe := strscan (ps, '>');
            if pe = Nil then
              pe := strscan (ps, ' ')
            else
              Inc (pe);

                                          // pe^ is now the character after
                                          // the '>' - which may be a space
                                          // a nul, or the start of the next
                                          // id
            if (pe = Nil) or (pe^ = #0) then
            begin
              ref := ps;
              len := Length (ref);
              Inc (ps, len) // ... so ps points to the nul
            end
            else
            begin
                                          // Move the ID into ref
              len := Integer (pe) - Integer (ps);
              SetString(ref,  ps, len);

                                          // Skip spaces before the next id
              while pe^ = ' ' do
                Inc (pe);
              ps := pe
            end;

                                              // For each reference in 'references'

            if (len <> 0) and (ref [1] = '<') and (ref [len] = '>') then
            begin
              hash := HashOf (ref);

              refArticle := FindHashMessage (id_table, hash, ref);

              if refArticle = Nil then      // Referenced article no longer exists
              begin                         // Create a dummy.

                refArticle := TArticle.Create (self);
                refArticle.fMessageID := ref;
                refArticle.fArticleNo := 0; // ie.  It's a dummy article
                refArticle.fSubject := article.fSubject;
                refArticle.fDate := article.fDate;
                refArticle.fMessageOffset := $ffffffff;
                refArticle.fFlags := fgRead;
                RawAddArticle (refArticle);
                AddHash (id_table, hash, refArticle);
              end;

                                            // Now slavishly follow Zawinski's Java threading code.
              if Assigned (prevArticle) and (refArticle.Parent = Nil) and (refArticle <> prevArticle) and not FindChild (refArticle, prevArticle) then
              begin
                refArticle.fParent := prevArticle;
                refArticle.fSibling := prevArticle.fChild;
                prevArticle.fChild := refArticle
              end;
              prevArticle := refArticle;
            end
          end;

          // Set the parent of this message to be the last element in References

          if (prevArticle <> Nil) and ((prevArticle = article) or FindChild (article, prevArticle)) then
            prevArticle := Nil;

          if (article.Parent <> Nil) then
          begin
            prev := Nil;
            rest := article.Parent.Child;

            while rest <> Nil do
            begin
              if rest = article then
                break;
              prev := rest;
              rest := rest.Sibling
            end;

            if rest = Nil then
              raise Exception.Create ('didn''t find ' + article.MessageID + ' in parent ' + article.parent.MessageID);

            if prev = Nil then
              article.Parent.fChild := article.Sibling
            else
              prev.fSibling := article.sibling;

            article.fSibling := Nil;
            article.fParent := Nil
          end;

          if prevArticle <> Nil then
          begin
            article.fParent := prevArticle;
            article.fSibling := prevArticle.Child;
            prevArticle.fChild := article
          end;

          if article.IsMine then
          begin
            while article.fParent <> Nil do
            begin
              article.IsMine := True;
              article := article.fParent
            end;

            article.IsMine := True
          end
        end;     // End 'for each article

      finally
        FreeHashTable (id_table)
      end;
                                    // Build a dummy article containing all the other
                                    // articles as children - used for pruning and gathering
      tempRoot := TArticle.Create(nil);
      try
        prevArticle := Nil;

        for i := 0 to ArticleCount - 1 do
        begin
          article := ArticleBase [i];

          if article.fBlocked then  // ie. doesn't show because of
            Continue;               // Hide read messages, etc.

          if article.fParent = Nil then // Add it as a child to tempRoot
          begin
            article.fFlags := article.fFlags and not fgNew;
            if prevArticle = Nil then
              tempRoot.fChild := article
            else
              prevArticle.fSibling := article;

            prevArticle := article
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

              pss := PChar (article.references);
              pe := pss + Length (article.References) - 1;

              // Go thru each entry in 'References' - backwards this time because
              // we want to check the most recent one first.

              // 1.17.5.8.  RFC 1036 says that References must be space
              // separated, but RFC 2822 suggests that the space is optional.
              // So handle either.

              while (DWORD (pe) > DWORD (pss)) and (pe^ = '>') do
              begin
                ps := pe;
                if DWORD (ps) > DWORD (pss) then
                repeat
                  Dec (ps)
                until (ps^ = '<') or (DWORD (ps) < DWORD (pss));
                if DWORD (ps) < DWORD (pss) then
                  break;

                len := DWORD (pe) - DWORD (ps) + 1;

                                              // Move the ID into ref
                SetString(ref, ps, len);

                pe := ps;
                Dec (pe);
                while (DWORD (pe) > DWORD (pss)) and (pe^ <> '>') do
                  Dec (pe);

                if (len <> 0) and (ref [1] = '<') and (ref [len] = '>') then
                begin
                  if Assigned (article.Parent) and (article.Parent.MessageId = ref) and (article.Parent.ArticleNo <> 0) then
                    refArticle := article.Parent
                  else
                  begin
                    hash := HashOf (ref);
                    refArticle := FindHashMessage (id_not_table, hash, ref)
                  end;

                  if refArticle <> Nil then
                  begin
                    article.fFlags := article.fFlags or (refArticle.fFlags and (fgInteresting or fgIgnore));
                    if (article.fFlags and fgIgnore) <> 0 then
                    begin
                      reDo := True;
                      article.fFlags := article.fFlags or fgRead
                    end;
                    break                       // Only look at previous parent
                  end
                end
              end;
            end
          end
        end;

        if tempRoot.Child <> Nil then
        begin
          PruneDummyArticles (tempRoot);      // Use Zawinski pruning.
          if DisplaySettings.GatherSubjects then
            GatherSubjects (tempRoot);        // Use Zawinski gathering
        end;
                                              // Move all the root articles into the fThreads list
        article := tempRoot.fChild;           // clear their sibling references too.
        while article <> Nil do
        begin
          fThreads.Add(article);
          prevArticle := article;
          article := article.fSibling;
          prevArticle.fSibling := Nil;
          prevArticle.fParent := Nil;
        end
      finally
        tempRoot.Free
      end;
                                              // All messages in threads that contain
                                              // a message from the current user should
                                              // be marked.  At the moment only the
                                              // root thread message is guaranteed to be
      for i := 0 to fThreads.Count - 1 do
      begin
        article := TArticle (fThreads [i]);
        if (article.fFlags and fgMine) <> 0 then
          SetThreadFlags (article.Child, fgMine)
      end
    finally
      FreeHashTable (id_not_table);
      Identities.Free;
    end
  end
end;

{ TArticleBase }

procedure TArticleBase.Assign(article : TArticleBase);
begin
  self.fMessageID := article.MessageId;
  self.fBytes := article.Bytes;
  self.fLines := article.Lines;
  self.fReferences := article.References;
  self.fFrom := article.From;
  self.fSubject := article.Subject;
  self.fDate := article.Date;
  self.fTo := article.fTo;
end;

constructor TArticleBase.Create (AOwner : TArticleContainer);
begin
  fOwner := AOwner;
  fCodePage := CP_USASCII;
  fMessageOffset := $ffffffff;
  fNewestMessage := -1;
end;

procedure TArticleBase.DecodeFromName;
begin
  if not fFromNameDecoded then
  begin
    NewsGlobals.DecodeFromEMail(fFrom, fFromName, fFromEMail, fCodePage);
    fFromNameDecoded := (fCodePage <> -1);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TArticleBase.DecodeMultipartSubject                        |
 |                                                                      |
 | Return true if the subject line has (eg.) (3/22) or [3/22] within it |
 | indicating that it's part of a multipart. If so, return 'n' = 3 and  |
 | 'x' = 22.                                                            |
 |                                                                      |
 | If 'fixit' is specified, adjust the subject so that it says '03/22'  |
 | instead of '3/22'                                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   var s: string      The subject line to decode                      |
 |   var n : Integer    Return the part number                          |
 |   var x : Integer    Return the total number of parts in the set     |
 |   fixIt : boolean    Adjust the subject string 's' so that the       |
 |                      n/x bit is justified so that it sorts correctly |
 |                                                                      |
 | The function returns true if the subject indicates that it's part of |
 | a multipart.                                                         |
 *----------------------------------------------------------------------*)
function TArticleBase.DecodeMultipartSubject(var s: string; var n,
  x: Integer; fixIt, blankIt : boolean): boolean;

var
  p, p1, p2 : PChar;
  l, l1, l2 : Integer;
  s1 : string;
begin
  result := False;
  l := Length (s);
  if l > 4 then                  // Can't be less than 5 charcters - eg. [1/3]
  begin
    p := PChar (s) + l - 1;      // Start at the last character
    p1 := Nil;

    while (p <> PChar (s)) do    // Find the final ']' or ')'
      if (p^ = ')') or (p^ = ']') then
      begin
        p1 := p;
        break
      end
      else
        Dec (p);

    while (p <> PChar (s)) do   // Now find the matching ']' or ')'
      if ((p1^ = ')') and (p^ = '(')) or ((p1^ = ']') and (p^ = '[')) then
        break
      else
        Dec (p);

    if (p1 <> Nil) and ((p^ = '[') or (p^ = '(')) then
    begin
      l := Integer (p1) - Integer (p) - 1;
                                // 'l' now contains the length of the bit within
                                // the brackets.  It must be at least 3
      if l > 2 then
      begin
        Inc (p);                // p now points to the first character in the n/x string
        Dec (p1);               // and p1 points to the last character
        p2 := p;
        repeat                  // Find the '/' in the middle.
          Inc (p2);
          if (p2 = p1) or (p2^ = '/') then break;
        until False;

        if p2 <> p1 then
        begin                   // We found the '/'

          l2 := Integer (p1) - Integer (p2);    // l2 contains the length of the 'x' number
          l1 := Integer (p2) - Integer (p);     // l contains the length of the 'n' number

          if l2 >= l1 then                      // Reallity check!
          begin
            n := StrToIntDef (Copy (p, 1, l1), -1);
            x := StrToIntDef (Copy (p2 + 1, 1, l2), -1);

                                                // Are bothe numbers valid?
            if (n <> -1) and (x <> -1) then
            begin
              result := True;                   // We'e got a multipart!

              if blankIt then                   // Replace the 'n' and 'x' with
                                                // a string of '?' the same length
                                                // as 'x' (for comparing)
              begin
                l1 := p - PChar (s);
                s := Copy (s, 1, l1) + StringOfChar ('?', l2) + '/' + StringOfChar ('?', l2) + (p1 + 1)
              end
              else
                if fixIt and (l1 <> l2) then    // Pad 'n' with zeros so it's the
                begin                           // same length as 'x'
                  l1 := p - PChar (s);
                  s1 := IntToStr (n);
                  l := Length (s1);
                  if l < l2 then
                    s1 := StringOfChar ('0', l2 - l) + s1;
                                                  // s1 now contains the 'n' string,
                                                  // padded to the same length as the
                                                  // 'x' string.

                  // Re-assemble the subject string with the padded 'n' string
                  s := Copy (s, 1, l1) + s1 + p2;
                end
            end
          end
        end
      end
    end
  end
end;

function TArticleBase.Equals(art: TArticleBase): boolean;
begin
  result := self = art
end;

function TArticleBase.GetCodePage: Integer;
begin
  result := fCodePage;
end;

function TArticleBase.GetCodePageFromFile: Integer;
var
  st, st1 : string;
begin
  if (fCodePage = -1) or ((Assigned (fMsg) and ((fFlags and fgUnknownCodePage) <> 0))) then
  begin
    if Assigned (fMsg) then
      fCodePage := Msg.Codepage
    else
    begin
      fFlags := Flags or fgUnknownCodePage;
      st := PeekAtMsgHdr ('content-type');
      if st <> '' then
      begin
        st1 := SplitString ('charset', st);
        if st <> '' then
        begin
          st1 := SplitString ('=', st);
          if st <> '' then
          begin
            st1 := SplitString (';', st);
            fCodePage := MimeCharsetNameToCodePage (AnsiDequotedStr (st1, '"'))
          end
        end
      end;

      if fCodePage = -1 then
      begin
        if Copy (FromName, 1, 1) > #127 then
        begin
          st := ExtractFileExt (FromEMail);
          fCodePage := URLSuffixToCodePage (st)
        end
        else
          fCodePage := CP_USASCII;
      end
    end
  end;

  result := fCodePage;
end;

function TArticleBase.GetCrosspostedTo: Integer;
var
  s : string;
  ch : char;
begin
  s := GetHeader ('Newsgroups');
  if s = '' then
  begin
    ch := ' ';
    s := GetHeader ('Xref');
  end
  else
    ch := ',';

  if s = '' then
    result := 1
  else
  begin
    result := 0;
    while SplitString (ch, s) <> '' do
      Inc (result);

    if ch = ' ' then
      Dec (result);

    if result <= 0 then
      result := 1
  end
end;

function TArticleBase.GetFromEmail: string;
begin
  if not fFromNameDecoded then
    DecodeFromName;

  result := fFromEmail;
end;

function TArticleBase.GetFromName: string;
begin
  if not fFromNameDecoded then
    DecodeFromName;

  result := fFromName;
end;

function TArticleBase.GetHasAttachment: boolean;
var
  gotHasAttachment : boolean;
  i : Integer;
  mps : TmvMessageParts;
  mp : TmvMessagePart;
begin
  gotHasAttachment := (flags and fgScannedAttachment) <> 0;
  if HasMsg and not gotHasattachment then
  begin
    if Assigned (Msg) then
    begin
      fFlags := fFlags or fgScannedAttachment;
      mps := fMsg.MessageParts;
      for i := 0 to mps.Count - 1 do
      begin
        mp := mps [i];
        if mp.Complete then
        begin
          if mp.DecodeType in [ttBase64, ttUUEncode, ttYEnc] then
            fFlags := fFlags or fgHasAttachment
        end
        else
          if (fFlags and fgHasAttachment) = 0 then
            fFlags := fFlags and not (fgScannedAttachment)
      end;
      Owner.fFlagsDirty := True;
    end;
  end;

  result := (flags and fgHasAttachment) <> 0;
end;

function TArticleBase.GetHasNoReplies: boolean;
begin
  result := (fChild = Nil) and (fParent = Nil) and (owner.ThreadOrder = toThreaded);
end;

function TArticleBase.GetHeader(const name: string): string;
var
  i : Integer;
  s : string;
begin
  if Assigned (fMsg) then
  begin
    result := '';
    for i := 0 to Msg.Header.Count - 1 do
    begin
      s := Msg.Header [i];
      if CompareText (name, SplitString (':', s)) = 0 then
      begin
        result := s;
        break
      end
    end
  end
  else
    result := PeekAtMsgHdr (name)
end;

function TArticleBase.GetIndex: Integer;
var
  base : TArticleBase;
  n : Integer;
begin
  n := 0;
  base := Owner.GetFirstArticle;
  while Assigned (base) and not base.Equals (self) do
  begin
    Inc (n);
    base := base.GetNext
  end;

  if Assigned (base) then
    result := n
  else
    result := -1
end;

function TArticleBase.GetInterestingMessageLine: string;
begin
  result := fSubject
end;

function TArticleBase.GetIsCancelled: boolean;
begin
  result := (fFlags and fgCancelled) <> 0;
end;

function TArticleBase.GetIsDeleted: boolean;
begin
  result := (fFlags and fgDeleted) <> 0;
end;

function TArticleBase.GetIsDormant: boolean;
var
  p : TArticleBase;
begin
  result := IsRead;
  if result and Assigned (child) then
  begin
    p := child;
    repeat
      result := p.IsDormant;
      if not result then break;
      p := p.Sibling
    until p = Nil
  end
end;

function TArticleBase.GetIsFromBozo: boolean;
var
  art : TArticle;
  action : TBozoAction;
  p : TArticleBase;
  pp : TArticleBase;
  doWholeThread : boolean;
begin
  action := baIgnore;
  if (fFlags and fgScannedBozo) <> 0 then
    result := (fFlags and fgBozo) <> 0
  else
  begin
    if self is TArticle then
      art := TArticle (self)
    else
      art := Nil;
    if NNTPAccounts.ArticleIsFromBozo(art, action) then
    begin
      fFlags := fFlags or fgBozo;
      result := True
    end
    else
    begin
      fFlags := fFlags and not fgBozo;
      result := False
    end;
    fFlags := fFlags or fgScannedBozo;
    if result then
    begin
      p := self;
      pp := p;
      doWholeThread := (action = baIgnoreThread) or (action = baMarkAsReadThread);
      while p <> Nil do
      begin
        if (action = baIgnore) or (action = baIgnoreThread) and not p.IsFromMe then
          p.IsIgnore := True
        else
          if (action = baMarkAsRead) or (action = baMarkAsReadThread) then
            p.IsRead := True;

        if doWholeThread then
        begin
          if (p.Child <> Nil) and not p.IsFromMe then
            p := p.Child
          else
            if p.Sibling <> Nil then
              if p <> pp then
                p := p.Sibling
              else
                p := Nil
            else
            begin
              p := p.Parent;
              if (p = Nil) or (p = pp) then break;
              p := p.Sibling
            end
        end
        else
          p := Nil
      end
    end
  end;
end;

function TArticleBase.GetIsFromMe: Boolean;
begin
  Result := FromName = Owner.Identity.UserName;
end;

function TArticleBase.GetIsIgnore: boolean;
begin
  result := fFlags and fgIgnore <> 0
end;

function TArticleBase.GetIsInteresting: boolean;
begin
  result := fFlags and fgInteresting <> 0
end;

function TArticleBase.GetIsMine: boolean;
begin
  result := (fFlags and fgMine) <> 0;
end;

function TArticleBase.GetIsNotOnServer: boolean;
begin
  result := ((fFlags and fgNotOnServer) <> 0);
end;

function TArticleBase.GetIsOdd: boolean;
var
  p : TArticleBase;
begin
  p := self;
  while p.Parent <> Nil do
    p := p.Parent;

  if (p.fOwner.fThreadOrder = toThreaded) or (p.fOwner.fThreadSortOrder = soSubject) or (p.fOwner.HideReadMessages) or (p.fOwner.HideIgnoredMessages) or (p.fOwner.fDisplayFiltered) or (p.fOwner.fHideMessagesNotToMe) then
    result := Odd (p.Owner.fThreads.IndexOf(p))
  else
    result := Odd (p.Owner.IndexOf(p))
end;

function TArticleBase.GetIsRead: boolean;
begin
  result := ((fFlags and fgRead) <> 0) or IsFromBozo;
end;

function TArticleBase.GetIsReply: boolean;
begin
  result := (fFlags and fgReply) <> 0;
end;

function TArticleBase.GetIsXanaNews: boolean;
begin
  Result := (fFlags and fgXanaNews) <> 0
end;

function TArticleBase.GetMessagesInThread: Integer;
var
  p : TArticlebase;
begin
  p := self;
  while Assigned (p) and Assigned (p.Parent) do
    p := p.Parent;

  result := 0;

  while Assigned (p) do
  begin
    Inc (result);
    p := p.Next;
    if (p = Nil) or (p.Parent = Nil) then
      break
  end
end;

function TArticleBase.GetMsgFromFile: TmvMessage;
var
  st : string;
  len : DWORD;
  hLen : Word;
  cp : Integer;
begin
  if not Assigned (fMsg) and (fMessageOffset <> $ffffffff) then
  begin
//    Owner.OpenMessageFile;
    if Assigned (Owner.fMessageFile) then

    begin               // Load the message from 'messages.dat'

      Owner.fMessageFile.Seek (fMessageOffset, soFromBeginning);

      SetLength (st, 5);        // Read XMsg: prefix
      Owner.fMessageFile.Read(st [1], 5);

      if st = 'X-Msg' then      // New style - contains extra header lines
      begin
        SetLength (st, 9);
        Owner.fMessageFile.Read(st [1], 9);
        st [1] := '$';
        len := StrToInt (st);

        Owner.fMessageFile.Read(hLen, SizeOf (hLen));

        cp := fCodePage;
        fMsg := TmvMessage.Create (Self);

        while hLen > 0 do
        begin
          SetLength (st, hLen);
          Owner.fMessageFile.read (st [1], hLen);
          fMsg.Header.Add (st);
          Owner.fMessageFile.Read(hLen, SizeOf (hLen));
        end;

        if fMsg.Codepage = CP_USASCII then
          fMsg.CodePage := cp;

        if len <> 0 then
          fMsg.RawData.CopyFrom(Owner.fMessageFile, len);
        MessageCacheController.Add(Self)
      end
    end
  end;

  Result := fMsg;

  if Assigned (result) then
    result.PartialMessage := MultipartFlags <> mfNotMultipart;
end;

function TArticleBase.GetNext: TArticleBase;
var
  idx : Integer;
begin
  if Assigned (Child) then
    Result := Child
  else
    if Assigned (Sibling) then
      Result := Sibling
    else
    begin
      Result := Parent;
      if Assigned (result) then
      while Assigned (Result) do
      begin
        if Assigned (Result.sibling) then
        begin
          Result := Result.sibling;
          break
        end
        else
          Result := Result.parent
      end
      else
        if Owner.ThreadOrder = toChronological then
        begin
          idx := Owner.IndexOf(self);
          if (idx >= 0) and (idx < Owner.LoadGetArticleCount -1) then
            result := Owner.ArticleBase [idx+1]
        end
    end
end;

function TArticleBase.GetPostingHost: string;
begin
// stub
end;

(*----------------------------------------------------------------------*
 | function TArticleBase.HasMsg : boolean                               |
 |                                                                      |
 | The function returns true if the article has a message body.         |
 *----------------------------------------------------------------------*)
function TArticleBase.GetSimplifiedSubject: string;
var
  p : Integer;
  t : boolean;
begin
  t := False;
  result := Subject;
  while (SameText (Copy (result, 1, 2), 're')) do
  begin
    t := True;
    p := Pos (':', result);
    if p = 0 then
      break;

    if p > 3 then
      if (result [3] <> '[') or (result [p - 1] <> ']') then
        break;

    Delete (result, 1, p)
  end;
  if t then
    result := Trim (result);
end;

function TArticleBase.GetSubjectIsReply: boolean;
var
  s : string;
  p : Integer;
begin
  s := Subject;
  result := SameText (Copy (s, 1, 2), 're');

  if result then
  begin
    p := Pos (':', s);
    result := p <> 0;

    if result then
      if p > 3 then
        result := (s [3] = '[') and (s [p - 1] = ']')
  end
end;

function TArticleBase.GetUniqueID: string;
begin
  result := MessageID;
end;

function TArticleBase.GetUnreadMessagesInThread: Integer;
var
  p : TArticlebase;
begin
  p := self;
  while Assigned (p) and Assigned (p.Parent) do
    p := p.Parent;

  result := 0;

  while Assigned (p) do
  begin
    if not p.IsRead then
      Inc (result);
    p := p.Next;
    if (p = Nil) or (p.Parent = Nil) then
      break
  end
end;

function TArticleBase.HasMsg: boolean;
begin
  result := Assigned (fMsg) or (fMessageOffset <> $ffffffff);
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
 |   articleNo : Integer;                                               |
 |   header : TStrings       // nb in/out parameter                     |
 *----------------------------------------------------------------------*)

procedure TArticleBase.Initialize(articleNo: Integer; header: TStrings);
var
  hdrs : TStringList;
  i : Integer;
  dt : string;

  function ProcessHeaderLine (const valueName : string) : string;
  var
    idx : Integer;
  begin
    idx := hdrs.IndexOfName(valueName);
    if idx >= 0 then
    begin
      Result := Trim (hdrs.Values [valueName]);
      header.Delete(idx);
      hdrs.Delete (idx)
    end
    else
      Result := '';
  end;

begin
  hdrs := TStringList.Create;
  try
    hdrs.NameValueSeparator := ':';
    fCodePage := -1;
    for i := 0 to header.Count - 1 do
      hdrs.Add(header [i]);

    fArticleNo := articleNo;

    fMessageID := ProcessHeaderLine ('Message-ID');
    fBytes := StrToIntDef (ProcessHeaderLine ('Bytes'), 0);
    fLines := StrToIntDef (ProcessHeaderLine ('Lines'), 0);
    fReferences := ProcessHeaderLine ('References');
    fFrom := ProcessHeaderLine ('From');
    fSubject := ProcessHeaderLine ('Subject');
    dt := ProcessHeaderLine ('Date');
    fDate := GMTToLocalDateTime (dt);
    if StrInternetToDateTime (dt) <> fDate then
      header.Add('X-XanaOrigDate:'+dt);


    if Pos ('XanaNews', hdrs.Values ['X-Newsreader']) > 0 then
      fFlags := fFlags or fgXanaNews;

    if Pos ('XanaNews', hdrs.Values ['User-Agent']) > 0 then
      fFlags := fFlags or fgXanaNews;
  finally
    hdrs.Free
  end
end;

function TArticleBase.MsgDownloading: boolean;
begin
  result := Assigned (fMsg) and (fMessageOffset = $ffffffff)
end;

function TArticleBase.PeekAtMsgHdr(const hdr: string): string;
begin
 result := '';
end;

function TArticleBase.PeekAtMsgHdrFromFile(const hdr: string): string;
var
  st : string;
  hLen : word;
begin
  result := '';
  if (fMessageOffset <> $ffffffff) and Assigned (Owner.fMessageFile) then
  begin               // Load (some of) the message from 'messages.datx'
                        // I did this to prevent huge messages being loaded/
                        // decoded when simply displaying the headers for non-
                        // selected messages.

    Owner.fMessageFile.Seek (fMessageOffset, soFromBeginning);

    SetLength (st, 5);        // Read XMsg: prefix
    Owner.fMessageFile.Read(st [1], 5);

    if st = 'X-Msg' then      // New style - contains extra header lines
    begin
      SetLength (st, 9);
      Owner.fMessageFile.Read(st [1], 9);
      Owner.fMessageFile.Read(hLen, SizeOf (hLen));

      while hLen > 0 do
      begin
        SetLength (st, hLen);
        Owner.fMessageFile.read (st [1], hLen);

        if CompareText (SplitString (':', st), hdr) = 0 then
        begin
          result := Trim (st);
          exit
        end;
        Owner.fMessageFile.Read(hLen, SizeOf (hLen));
      end
    end
  end
end;

procedure TArticleBase.ReleaseMsg;
begin
  if not MsgDownloading then
    FreeAndNil (fMsg)
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

procedure TArticleBase.SetFlag(flag: DWORD; value: boolean);
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

procedure TArticleBase.SetIsCancelled(const Value: boolean);
begin
  SetFlag (fgCancelled, value);
end;

procedure TArticleBase.SetIsDeleted(const Value: boolean);
begin
  SetFlag (fgDeleted, value);
  if value then
    Owner.fNeedsPurge := True;
end;

procedure TArticleBase.SetIsIgnore(const Value: boolean);
var
  flg : Integer;
begin
  if Value then
    flg := fgIgnore or fgRead
  else
    flg := fgIgnore;

  SetFlag (flg, Value);

  if value then
    fFlags := fFlags and not fgInteresting;
end;

procedure TArticleBase.SetIsInteresting(const Value: boolean);
begin
  SetFlag (fgInteresting, value);

  if value then
    fFlags := fFlags and not fgIgnore;
end;

procedure TArticleBase.SetIsMine(const Value: boolean);
begin
  SetFlag (fgMine, value)
end;

procedure TArticleBase.SetIsNotOnServer(const Value: boolean);
begin
  SetFlag (fgNotOnServer, Value);
end;

procedure TArticleBase.SetIsRead(const Value: boolean);
begin
  SetFlag (fgRead, value);
  Owner.fUnreadArticleCount := -1;
  Owner.fUnloadedArticleCount := -1;
end;

procedure TArticleBase.SetIsReply(const Value: boolean);
begin
  SetFlag (fgReply, value)
end;

procedure TArticleBase.SetMsg(const Value: TmvMessage);
begin
// stub
end;

{ TNNTPMessageCacheController }

function TNNTPMessageCacheController.CanRemove(Obj: TObject): boolean;
var
  article : TArticleBase;
begin
  if fAlwaysRemove then
  begin
    result := True;
    exit
  end;
  try
    if (Obj <> Nil) and (Obj is TArticleBase) then
    begin
      article := TArticleBase (Obj);

      result := not Assigned (article.fMsg);
      if not result then
      begin
                                    // Don't delete if the article's message is being
                                    // downloaded (fMsg exists but fMessageOffset is still $ffffffff(
                                    // or is being displayed.
        if (article.fMessageOffset <> $ffffffff) and not article.fMsg.BeingDisplayed then
        begin
          result := True;
          FreeAndNil (article.fMsg)
        end
      end
    end
    else
      result := True
  except
    result := True
  end
end;

{ TArticleObjectContainer }

constructor TArticleObjectContainer.Create(const AName: string; AFiltersParent, ADisplayFiltersParent : TFiltersCtnr);
begin
  inherited Create (AName, AFiltersParent, ADisplayFiltersParent);
  fArticles := TObjectList.Create;

end;

destructor TArticleObjectContainer.Destroy;
begin
  fArticles.Free;

  inherited;
end;

function TArticleObjectContainer.FindArticleNo(cno: Integer): TArticleBase;
begin
  if not fArticlesLoaded then
    LoadArticles;

  result := inherited FindArticleNo (cno);
end;

function TArticleObjectContainer.GetArticleBase(
  idx: Integer): TArticleBase;
begin
  if not fArticlesLoaded then
    LoadArticles;
  if idx < fArticles.Count then
    result := TArticleBase (fArticles.List^ [idx])
  else
    result := Nil
end;

function TArticleObjectContainer.GetArticleCount: Integer;
begin
  if not fArticlesLoaded then
  begin
    if fUnloadedArticleCount = -1 then
      GetUnreadArticleCount;
    result := fUnloadedArticleCount
  end
  else
    result := fArticles.Count
end;

function TArticleObjectContainer.GetLoaded: boolean;
begin
  result := fArticlesLoaded
end;

function TArticleObjectContainer.GetThreadCount: Integer;
begin
  if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) or hideReadMessages or HideMessagesNotToMe or hideIgnoredMessages or fDisplayFiltered then
  begin
    LoadArticles;
    result := fThreads.Count
  end
  else
    result := inherited GetThreadCount
end;

(*----------------------------------------------------------------------*
 | function TArticleObjectContainer.GetThreads                          |
 |                                                                      |
 | Get a particular root article                                        |
 *----------------------------------------------------------------------*)
function TArticleObjectContainer.GetThreads(idx: Integer): TArticleBase;
begin
  LoadArticles;
  if (ThreadOrder = toThreaded) or (ThreadSortOrder = soSubject) or hideReadMessages or HideMessagesNotToMe or hideIgnoredMessages or fDisplayFiltered then
    result := TArticleBase (fThreads [idx])
  else
    result := TArticleBase (fArticles [idx]);
end;

(*----------------------------------------------------------------------*
 | function TArticleObjectContainer.IndexOf                             |
 |                                                                      |
 | Return the index of an article in the container                      |
 *----------------------------------------------------------------------*)
function TArticleObjectContainer.IndexOf(article: TArticleBase): Integer;
begin
  result := fArticles.IndexOf(article)
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
 |   reset: boolean                     Reset the High Water Mark       |
 |                                                                      |
 |   const folderName: string           Folder name to purge to.        |
 *----------------------------------------------------------------------*)

procedure TArticleObjectContainer.PurgeArticles(all, reset: boolean;
  const folderName: string);
var
  i : Integer;
  article : TarticleBase;
  recreateMessages : boolean;
  lastArt, lastCurrentArticle : Integer;
  reg : TExSettings;
  folder : TArticleFolder;
  newArticles, tmp, deletedArticles : TObjectList;

begin
  if not fNeedsPurge and not all then Exit;
  i := 0;
  recreateMessages := False;
  lastArt := 0;
  lastCurrentArticle := 0;
  if folderName = '' then
    folder := Nil
  else
    folder := gArticleFolders.FindFolder (folderName);

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
        if self is TSubscribedGroup then with TSubscribedGroup (self) do
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
          article := TArticle (fArticles [i]);
                                            // Keep track of 'highest ever' article no
          if (article.ArticleNo > lastArt) and (article.ArticleNo <> 99999999) then
            lastArt := article.ArticleNo;
                                            // Keep track of highest current article no
          if not (article.IsDeleted) and (article.ArticleNo > lastCurrentArticle) then
            lastCurrentArticle := Article.ArticleNo;

          if (Assigned (article.fMsg) and (article.fMessageOffset = $ffffffff)) or not article.IsDeleted then
            newArticles.Add(article)
          else
          begin
            if not recreateMessages then  // This is the first deleted article
            begin
              if Assigned (folder) then
                folder.BeginAdd;

              recreateMessages := True;
              SetTempStatusMessage ('Purging ' + Name, 0, 0)
            end;

            if Assigned (folder) then
            begin
              if not Assigned (article.fMsg) then
                OpenMessageFile;

              folder.AddArticle(article)
            end;

            deletedArticles.Add(article)
          end;
          Inc (i)
        end
      end
    except
      if all then
        ShowMessage ('Error purging all messages from ' + Name)
      else
        ShowMessageFmt ('Error purging message %d from %d messages in %s', [i, fArticles.Count, Name]);

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
        deletedArticles.OwnsObjects := True
      end;

                  // Save the articles and their messages.
      SaveArticles (True);

                    // Reset current sort flags, so sorting will occur next
      ResetSortFlags;

      LoadArticles;     // Re-opens the message file, and sorts the articles.  This
                        // may remove duplicates - which is vital before calling
                        // GetLastArticle otherwise potential 'Index Out of Bounds'

      SetTempStatusMessage ('', 0, 0);
    end;

    if Loaded and (self is TSubscribedGroup) then with TSubscribedGroup (self) do
    begin
      i := GetLastArticle;      // nb.  Get's current high water mark or highest
                                // article no - whichever's higher

      if lastArt > i then       // If highest ever article is even higher, use that
        fHighWaterMark := lastArt;

      if fHighWaterMark <= lastCurrentArticle then
        fHighWaterMark := 0;

      reg := CreateGroupRegistry (KEY_READ or KEY_WRITE);
      if Assigned (reg) then
      try
        reg.SetIntegerValue('Last Article', fHighWatermark, 0);
      finally
        reg.Free
      end
    end

  finally
    try
      if Assigned (folder) and recreateMessages then
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
    MessageCacheController.AlwaysRemove := False
  end
end;
(*----------------------------------------------------------------------*
 | procedure TArticleObjectContainer.RawAddArticle                      |
 *----------------------------------------------------------------------*)
procedure TArticleObjectContainer.RawAddArticle(article: TArticleBase);
begin
  fArticles.Add(article)
end;

(*----------------------------------------------------------------------*
 | procedure TArticleObjectContainer.RawClearArticles                   |
 |                                                                      |
 | Delete all articles.                                                 |
 *----------------------------------------------------------------------*)
procedure TArticleObjectContainer.RawClearArticles;
begin
  fArticles.Clear;
  fArticlesLoaded := True       // Yes - True!
end;

procedure TArticleObjectContainer.RawDeleteArticle(cno: Integer);
begin
  fArticles.Delete(cno);
end;

(*----------------------------------------------------------------------*
 | procedure TArticleObjectContainer.RawRemoveArticle                   |
 |                                                                      |
 | Remove a particular article                                          |
 *----------------------------------------------------------------------*)
procedure TArticleObjectContainer.RawRemoveArticle(article: TArticleBase);
begin
  fArticles.Remove(article)
end;

(*----------------------------------------------------------------------*
 | procedure TArticleObjectContainer.RawSortArticles                    |
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TArticleObjectContainer.RawSortArticles;
begin
  fArticles.Sort(CompareThreads);
end;

(*----------------------------------------------------------------------*
 | procedure TArticleObjectContainer.UnloadArticles                     |
 |                                                                      |
 | Unload the articles - as long as it's safe to do so.                 |
 *----------------------------------------------------------------------*)
procedure TArticleObjectContainer.UnloadArticles;
var
  i : Integer;
  article : TArticleBase;
  dontFree : boolean;
begin
  if not fArticlesLoaded then Exit;
  if DontUnloadCache.IndexOfObject(self) >= 0 then Exit;
  if Locked then Exit;
  if fSearching then Exit;
  if fSorting then Exit;
  if fFocused then Exit;                // Container is focused in the UI
                                        // An article body is being downloaded
  if ThreadManager.GettingArticle (self, Nil) then Exit;

  LogMessage ('Unloading ' + name);

  if fFlagsDirty then                   // Save articles (headers) if the flags
    SaveArticles (False);               // need saving

  MessageCacheController.Clear;
  dontFree := False;
                                        // Free the message bodies, and do a final
                                        // check that a message isn't being downloaded
  for i := 0 to fArticles.Count - 1 do
  begin
    article := TArticle (fArticles [i]);
    if not article.MsgDownloading then
      article.ReleaseMsg
    else
      dontFree := True
  end;

  if not dontFree then                  // Nothing's being downloaded
  begin
    if self is TSubscribedGroup then
      TSubscribedGroup (self).fRawLastArticle := TSubscribedGroup (self).LastArticle;
    fArticlesLoaded := False;
    fArticles.Clear                     // Clear the article headers.
  end
end;

procedure TNNTPAccounts.SetDoVersionCheck(const Value: Integer);
var
  reg : TExSettings;
begin
  if fDoVersionCheck <> Value then
  begin
    fDoVersionCheck := Value;
    reg := CreateExSettings;
    try
      reg.SetIntegerValue ('HTTP Permission Required', Value, 0);
    finally
      reg.Free
    end
  end
end;

procedure TNNTPAccounts.SetHideDormantConnections(const Value: boolean);
var
  reg : TExSettings;
begin
  if fHideDormantConnections <> Value then
  begin
    fHideDormantConnections := Value;
    reg := CreateExSettings;
    try
      reg.SetBooleanValue ('Hide Dormant Connections', Value, True);
    finally
      reg.Free
    end
  end
end;

procedure TNNTPAccounts.SetShowSecrets(const Value: boolean);
begin
  fShowSecrets := Value;
  fSecretAccountCount := -1;
end;

(*----------------------------------------------------------------------*
 | TNNTPAccounts.UnloadOldContainers                                    |
 |                                                                      |
 | Unload articles for all containers except for the specified one.     |
 *----------------------------------------------------------------------*)
procedure TNNTPAccounts.UnloadOldContainers (exceptFor : TArticleObjectContainer);
var
  i, j : Integer;
  grp : TArticleObjectContainer;
begin
  for i := 0 to Count - 1 do
    for j := 0 to Items [i].SubscribedGroupCount - 1 do
    begin
      grp := Items [i].SubscribedGroups [j];
      if grp <> exceptFor then
        grp.UnloadArticles
    end
end;


{ TServerAccount }

constructor TServerAccount.Create(const AName: string);
begin
  fName := AName;

end;

function TServerAccount.GetIdentity: TIdentity;
begin
  result := NNTPAccounts.Identities.DefaultIdentity;
end;

function TServerAccount.GetPostingSettings: TPostingSettings;
begin
  result := NNTPAccounts.PostingSettings;
end;

procedure TServerAccount.SetName(const Value: string);
begin
  fName := Value;
end;

{ TBozo }

procedure TBozo.Assign(b: TBozo);
begin
  self.fName := b.fName;
  self.fEMail := b.fEMail;
  self.fBozodDate := b.fBozodDate;
  self.fFlags := b.fFlags;
  self.fSearcher := Nil;
  self.fKeyphrase := b.fKeyphrase;
  self.fAction := b.fAction;
end;

constructor TBozo.Create(const AName, AEMail: string; ABozodDate: TDateTime; AAction : TBozoAction);
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
    Include (fFlags, fgEMail);
  fAction := AAction;
end;

procedure TNNTPAccounts.ReplaceBozos(newBozos: TObjectList);
var
  i : Integer;
  b, b1 : TBozo;
begin
  fBozos.Clear;

  for i := 0 to newBozos.Count - 1 do
  begin
    b := TBozo (newBozos [i]);
    b1 := TBozo.CreateNew;
    b1.Assign(b);
    fBozos.Add(b1)
  end;

  SaveBozoList;
  ResetAllBozoFlags
end;

procedure TNNTPAccounts.ResetAllBozoFlags;
var
  i, j : Integer;
  acc : TNNTPAccount;
  grp : TSubscribedGroup;
begin
  for i := 0 to Count - 1 do
  begin
    acc := Items [i];
    for j := 0 to acc.SubscribedGroupCount - 1 do
    begin
      grp := acc.SubscribedGroups [j];
      if grp.Loaded then
        grp.ResetBozoFlags
    end
  end
end;

constructor TBozo.CreateNew;
begin
end;

destructor TBozo.Destroy;
begin
  fSearcher.Free;

  inherited;
end;

function TBozo.GetSearcher: TStringSearcher;
begin
  if fSearcher = Nil then
    fSearcher := TGoogleLikeStringSearcher.Create('', False);

  result := fSearcher;
end;

function TBozo.MatchesArticle(art: TArticle): boolean;
begin
  if fgName in Flags then
    result := WildContains (art.FromName, Name)
  else
    result := True;

  if result and (fgEMail in Flags) then
    result := result and WildContains (art.FromEmail, EMail);

  if result and (fgKeyphrase in Flags) and (fKeyphrase <> '') then
    result := result and art.MatchesKeyPhrase(fKeyphrase, Searcher);
end;

procedure TNNTPAccounts.ChangeAccountSortIdx(acct: TNNTPAccount; idx: Integer);
var
  i : Integer;
  a : TNNTPAccount;
begin
  if idx = acct.fSortIdx then Exit;

  i := Count - 1;
  while i >= 0 do
  begin
    a := Items [i];
    if a = acct then
      a.fSortIdx := idx
    else
      if a.fSortIdx <> -1 then
        if a.fSortIdx >= idx then
          Inc (a.fSortIdx);

    Dec (i)
  end;

  SortAccounts;

  for i := 0 to Count - 1 do
    Items [i].fSortIdx := i
end;

function CompareAccounts (List: TStringList; Index1, Index2: Integer) : Integer;
var
  acc1, acc2 : TNNTPAccount;
begin
  acc1 := TNNTPAccount (List.Objects [Index1]);
  acc2 := TNNTPAccount (List.Objects [Index2]);

  result := acc1.fSortIdx - acc2.fSortIdx
end;

procedure TNNTPAccounts.SortAccounts;
var
  idx : Integer;
begin
  fAccounts.CustomSort(CompareAccounts);
  for idx := 0 to fAccounts.Count - 1 do
    TNNTPAccount (fAccounts.Objects [idx]).fSortIdx := idx
end;

initialization
  LSSync := TCriticalSection.Create;
  MessageCacheController := TNNTPMessageCacheController.Create (5, False); // Cache the last 5 message bodies

  DontUnloadCache := TObjectCache.Create(3, False);    // Cache the last 3 newsgrops

  gXanaNewsDir := ExtractFilePath (paramStr (0));
  if gXanaNewsDir [Length (gXanaNewsDir)] = '\' then
    gXanaNewsDir := Copy (gXanaNewsDir, 1, Length (gXanaNewsDir) - 1);

  SetLength (gMessageBaseRoot, MAX_PATH);
  if SUCCEEDED (SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, PChar (gMessageBaseRoot))) then
  begin
    gMessageBaseRoot := PChar (gMessageBaseRoot);
    gMessageBaseRoot := gMessageBaseRoot + '\Woozle\XanaNews'
  end
  else
    gMessageBaseRoot := gXanaNewsDir;
  gBestmessageBaseLocation := gMessageBaseRoot;
  gKeyName := '\software\Woozle\XanaNews';
finalization
  MessageCacheController.Free;
  DontUnloadCache.Free;
  FreeAndNil (LSSync);
end.
