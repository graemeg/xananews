(*======================================================================*
 | unitNewsThread                                                       |
 |                                                                      |
 | TNewsThread base class                                               |
 | TTCPGetter classes.                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitNewsThread;

interface

uses
  Windows, Classes, Graphics, SysUtils, unitNNTPServices, unitMailServices,
  SyncObjs, IdTCPClient, Contnrs, unitSettings, unitIdentities,
  IdMessage, NewsGlobals, IdNNTPX, IdSSLOpenSSL, XnClasses, XnRawByteStrings;

type
  TNNTPThreadState = (
    tsDormant, // Thread has nothing to do just sits and waits, might disconnect (Invisible or Green smudge).
    tsPending, // Has a job to do waiting for the thread to resume               (Red)
    tsBusy,    //                                                                (Green)
    tsDone     // Just finished it's job, processing data, (next is tsDormat)    (Wisp image)
  );

  TTCPGetter = class;

  //-----------------------------------------------------------------------
  // Base class for news reading threads.  Overridden by TNNTPThread
  // and TDNewsThread
  TTCPThread = class(TThread)
  private
    fTrigger: TEvent;
    fState: TNNTPThreadState;
    fGetter: TTCPGetter;
    fSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    fUISync: TCriticalSection;
    function GetLastResponse: string;
// TODO:    function VerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth: Integer): Boolean;

  protected
    fSettings: TServerSettings;
    fLastError: string;
    fClient: TIdTCPClientCustom;

    procedure DoWork; virtual; abstract;
    procedure NotifyError;

  public
    constructor Create(AGetter: TTCPGetter; ASettings: TServerSettings); virtual;
    destructor Destroy; override;
    procedure GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer); virtual;

    procedure UILock;
    procedure UIUnlock;

    property Trigger: TEvent read fTrigger;
    property State: TNNTPThreadState read fState write fState;
    property Client: TIdTCPClientCustom read fClient;
    property LastResponse: string read GetLastResponse;
    property Getter: TTCPGetter read fGetter;
    property SSLHandler: TIdSSLIOHandlerSocketOpenSSL read fSSLHandler;
  end;

  TTCPThreadClass = class of TTCPThread;

  //-----------------------------------------------------------------------
  // Base class for getters
  TTCPGetter = class
  private
    fPaused: Boolean;
    fTerminating: Boolean;
    function GetState: TNNTPThreadState;
    procedure SetState(const Value: TNNTPThreadState);
    function GetOutstandingRequestText(idx: Integer): string; virtual;
    function GetOutstandingRequestCount: Integer; virtual;
    function GetStatusBarMessage(group: TServerAccount): string; virtual;
    function GetGetterRootText: string; virtual;
    procedure SetPaused(const Value: Boolean);
    function GetSettings: TServerSettings;
  protected
    fThread: TTCPThread;
    constructor Create(cls: TTCPThreadClass; ASettings: TServerSettings);
    function GetIsDoing(obj: TObject): Boolean; virtual;
    function GetGroup(idx: Integer): TServerAccount; virtual;
  public
    procedure BeforeDestruction; override;

    procedure Disconnect(Done: Boolean = False);
    function Connected: Boolean;
    procedure Resume; virtual;
    property Paused: Boolean read fPaused write SetPaused;
    property Terminating: Boolean read fTerminating;

    // After the threads DoWork has finished:
    //
    // 1.  WorkDone is called.  The default method hangs up the connection, so save the data, and call inherited
    // 2.  Next, NotifyUI is called in the context of the main thread.
    // 3.  Next 'ClearWork' is called.  Get rid of temporary data.
    //
    // nb. ClearWork is also called *before* DoWork - so it should *only* reset temporary data.

    procedure ClearWork; virtual;
    procedure WorkDone; virtual;
    procedure NotifyUI; virtual;

    procedure Clear; virtual;
    procedure DeleteRequest(idx: Integer); virtual;

    property Settings: TServerSettings read GetSettings;
    property State: TNNTPThreadState read GetState write SetState;
    property OutstandingRequestCount: Integer read GetOutstandingRequestCount;
    property OutstandingRequestText[idx: Integer]: string read GetOutstandingRequestText;
    property StatusBarMessage[group: TServerAccount]: string read GetStatusBarMessage;
    property IsDoing[obj: TObject]: Boolean read GetIsDoing;
    property GetterRootText: string read GetGetterRootText;
    procedure GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer);
    property Group[idx: Integer]: TServerAccount read GetGroup;
  end;

  TTCPGetterClass = class of TTCPGetter;

  TNewsGetter = class(TTCPGetter)
  private
    procedure SetAccount(const Value: TNNTPAccount);
    function GetAccount: TNNTPAccount;
  protected
    function GetIsDoing(obj: TObject): Boolean; override;
  public
    constructor Create(cls: TTCPThreadClass; ANNTPAccount: TNNTPAccount);
    procedure Resume; override;
    property Account: TNNTPAccount read GetAccount write SetAccount;
  end;

  TMultiNewsGetter = class(TNewsGetter)
  private
    fRequests: TObjectList;
    fSync: TCriticalSection;
    fLocked: Boolean;
    fLockThread: Integer;
    function GetCount: Integer;
  public
    constructor Create(cls: TTCPThreadClass; ANNTPAccount: TNNTPAccount);
    destructor Destroy; override;
    function LockList: TObjectList;
    procedure UnlockList;
    property Count: Integer read GetCount;
    property Locked: Boolean read fLocked;
  end;

  //-----------------------------------------------------------------------
  // Getter for newsgroup lists
  TNewsgroupGetter = class(TNewsGetter)
    fNewsgroups: TStringList;
    function GetOutstandingRequestText(idx: Integer): string; override;
    function GetStatusBarMessage(group: TServerAccount): string; override;
    function GetGetterRootText: string; override;
  public
    constructor Create(ANNTPAccount: TNNTPAccount);
    destructor Destroy; override;
    procedure ClearWork; override;        //
    procedure WorkDone; override;         // Called when work has finished. (Individual thread)
    procedure NotifyUI; override;         // Called when work has finished. (Main thread)
  end;

  TArticlesGetterRequest = class
  private
    fGroup: TSubscribedGroup;
    fFromArticle: Int64;
    fArticleCount: Int64;
    fFull: Boolean;
    fAbandon: Boolean;
    fBatchRef: Integer;
    fRetry: Boolean;
    fSince: TDateTime;
  public
    constructor Create(AGroup: TSubscribedGroup; AFromArticle, AArticleCount: Int64; full: Boolean; ABatchRef: Integer; ASince: TDateTime);
    property Group: TSubscribedGroup read fGroup;
    property Full: Boolean read fFull;
    property FromArticle: Int64 read fFromArticle;
    property ArticleCount: Int64 read fArticleCount;
    property Abandon: Boolean read fAbandon write fAbandon;
    property BatchRef: Integer read fBatchRef;
    property Retry: Boolean read fRetry write fRetry;
    property Since: TDateTime read fSince;
  end;

  //-----------------------------------------------------------------------
  // Getter for bulk article/header downloading
  TArticlesGetter = class(TMultiNewsGetter)
  private
    fHeader: TAnsiStrings;
    fBody: TMemoryStream;
    fArticles: TAnsiStringList;

    function GetOutstandingRequestCount: Integer; override;
    function GetOutstandingRequestText(idx: Integer): string; override;
    function GetStatusBarMessage(group: TServerAccount): string; override;
    function GetGetterRootText: string; override;

  protected
    function GetIsDoing(obj: TObject): Boolean; override;
    function GetGroup(idx: Integer): TServerAccount; override;

  public
    CurrentFull: Boolean;
    CurrentUpdateAll: Boolean;
    CurrentArticleNo: Int64;
    CurrentMax: Int64;
    CurrentGroup: TSubscribedGroup;
    XOverFMT: TStringList;

    constructor Create(ANNTPAccount: TNNTPAccount);
    destructor Destroy; override;
    procedure Clear; override;
    procedure DeleteRequest(idx: Integer); override;
    procedure AddGroupToList(group: TSubscribedGroup; fromArticle, articleCount: Int64; full: Boolean; ABatchRef: Integer; ASince: TDateTime);
    procedure ClearWork; override;

    procedure SaveCurrentArticle;
    procedure UpdateArticles;
    procedure UpdateHeaders;

    property Articles: TAnsiStringList read fArticles;
    property Header: TAnsiStrings read fHeader;
    property Body: TMemoryStream read fBody;
  end;

  TArticleGetterRequest = class
  private
    fGroup: TSubscribedGroup;
    fArticle: TArticle;
    fNeedsFullRefresh: Boolean;
  public
    constructor Create(group: TSubscribedGroup; article: TArticle; needsFullRefresh: Boolean);
    property Group: TSubscribedGroup read fGroup;
    property Article: TArticle read fArticle;
    property NeedsFullRefresh: Boolean read fNeedsFullRefresh;
  end;

  //-----------------------------------------------------------------------
  // Getter for individual article downloading
  TArticleGetter = class(TMultiNewsGetter)
  private
    function GetOutstandingRequestCount: Integer; override;
    function GetOutstandingRequestText(idx: Integer): string; override;
    function GetStatusBarMessage(group: TServerAccount): string; override;
    function GetGetterRootText: string; override;
  protected
    function GetIsDoing(obj: TObject): Boolean; override;
    function GetGroup(idx: Integer): TServerAccount; override;
  public
    CurrentArticle: TArticle;
    CurrentGroup: TSubscribedGroup;
    PipelineGroup: TSubscribedGroup;
    PipelinePos: Integer;

    constructor Create(ANNTPAccount: TNNTPAccount);

    procedure Clear; override;
    procedure DeleteRequest(idx: Integer); override;
    procedure ClearArticle;
    procedure GotArticle;
    procedure FailArticle;
    procedure StartArticle;
    procedure UpdateArticles;
    procedure AddArticleToList(group: TSubscribedGroup; article: TArticle; needsFullRefresh: Boolean);
  end;

  TAttachment = class
  private
    fSize: Integer;
    fDate: TDateTime;
    fDirectory: string;
    fFileName: string;
    fInline: Boolean;
    function GetPathName: string;
  public
    constructor Create(APathName: string);
    property FileName: string read fFileName;
    property Directory: string read fDirectory;
    property Date: TDateTime read fDate;
    property Size: Integer read fSize;
    property IsInline: Boolean read fInline;
    property PathName: string read GetPathName;
  end;

  TPoster = class;

  TPosterRequest = class
  private
    fOwner: TPoster;
    fHdr: TAnsiStrings;
    fMsg: RawByteString;
    fGroups: RawByteString;
    fSubject: RawByteString;
    fAttachments: TObjectList;
    fCodepage: Integer;
    fTextPartStyle: TTextPartStyle;
    function GetGroups: RawByteString;
    function GetSubject: RawByteString;
    procedure GetGroupAndSubject;
    function MIMEHeaderRequired(): Boolean;
  public
    constructor Create(AOwner: TPoster; AHdr: TAnsiStrings; const AMsg: RawByteString; attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle);
    destructor Destroy; override;
    procedure Reset;

    procedure CreateEncodedHeader(var hdr: TAnsiStrings; var hdrCreated: Boolean; var multipartBoundary: RawByteString);
    procedure CreateEncodedMessage(var msg: TAnsiStrings; const multipartBoundary: RawByteString);
    procedure AddAttachments(msg: TAnsiStrings; const multipartBoundary: RawByteString);

    property Hdr: TAnsiStrings read fHdr;
    property Msg: RawByteString read fMsg write fMsg;

    property Groups: RawByteString read GetGroups;
    property Subject: RawByteString read GetSubject;

    property Owner: TPoster read fOwner;
    property Attachments: TObjectList read fAttachments write fAttachments;

    property Codepage: Integer read fCodepage;
  end;

  //-----------------------------------------------------------------------
  // Getter for posting messages
  TPoster = class(TMultiNewsGetter)
  private
    fUseOutbasket: Boolean;
    function GetOutstandingRequestCount: Integer; override;
    function GetOutstandingRequestText(idx: Integer): string; override;
    function GetGetterRootText: string; override;
    function GetStatusBarMessage(group: TServerAccount): string; override;
  protected
  public
    constructor Create(AAccount: TNNTPAccount);

    procedure WorkDone; override;

    procedure Resume; override;
    procedure ResumeOutbasket;
    procedure Clear; override;
    procedure DeleteRequest(idx: Integer); override;
    procedure AddPostToList(hdr: TAnsiStrings; const msg: RawByteString; attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle);
    property UseOutbasket: Boolean read fUseOutbasket write fUseOutbasket;
  end;

  TEmailer = class;

  TEMailerRequest = class
  private
    fMsg: string;
    fAttachments: TObjectList;
    fOwner: TEMailer;
    fCodePage: Integer;
    fCC: string;
    fBCC: string;
    fTo: string;
    fSubject: string;
    fArticleContainer: TServerAccount;
    fReplyTo: string;
    function GetMailAccount: TMailAccount;
  public
    constructor Create(AArticleContainer: TServerAccount; AOwner: TEMailer; const ATo, ACC, ABCC, ASubject, AReplyTo, AMsg: string; attachments: TObjectList; ACodePage: Integer);
    destructor Destroy; override;

    property MTo: string read fTo write fTo;
    property MCC: string read fCC write fCC;
    property MBCC: string read fBCC write fBCC;
    property MSubject: string read fSubject write fSubject;
    property MReplyTo: string read fReplyTo;
    property Msg: string read fMsg write fMsg;

    property Owner: TEMailer read fOwner;
    property Attachments: TObjectList read fAttachments write fAttachments;
    property CodePage: Integer read fCodePage write fCodePage;
    property ArticleContainer: TServerAccount read fArticleContainer;
    property MailAccount: TMailAccount read GetMailAccount;
  end;

  TEMailer = class(TTCPGetter)
  private
    fMessages: TObjectList;
    fCurrentMessage: TidMessage;
    fUseOutbasket: Boolean;
    fOrigUseOutbasket: Boolean;
    fSync: TCriticalSection;
    fLocked: Boolean;
    fLockThread: Integer;

    function GetCount: Integer;
    function GetOutstandingRequestCount: Integer; override;
    function GetOutstandingRequestText(idx: Integer): string; override;
    function GetGetterRootText: string; override;
    function GetStatusBarMessage(group: TServerAccount): string; override;
  protected
    function GetGroup(idx: Integer): TServerAccount; override;
  public
    constructor Create(settings: TSMTPServerSettings; AUseOutbasket: Boolean);
    destructor Destroy; override;
    procedure AddMessageToList(AArticleContainer: TServerAccount; const sTo, sCC, sBCC, sSubject, sReplyTo, msg: string; attachments: TObjectList; ACodePage: Integer);
    procedure DeleteRequest(idx: Integer); override;
    function LockList: TObjectList;
    procedure UnlockList;
    procedure Resume; override;
    procedure ResumeOutbasket;
    procedure WorkDone; override;
    property Count: Integer read GetCount;
    property CurrentMessage: TidMessage read fCurrentMessage write fCurrentMessage;
  //  procedure SaveOutgoingPost;
    property UseOutbasket: Boolean read fUseOutbasket;
    property OrigUseOutbasket: Boolean read fOrigUseOutbasket;
  end;


implementation

uses
  idCoder, idCoderMIME, IdSMTP, idHeaderList,
  unitNNTPThreadManager, unitNewsReaderOptions, unitNNTPThreads,
  unitCharsetMap, unitSearchString, unitLog, unitRFC2646Coder, XnCoderUUE,
  XnCoderQuotedPrintable;

{ TTCPThread }

(*----------------------------------------------------------------------*
 | TTCPThread.Create                                                    |
 |                                                                      |
 | Each 'getter' has a worker thread associated with it.  During their  |
 | constructor the create the worker thread.                            |
 |                                                                      |
 | Parameters:                                                          |
 |   AGetter: TTCPGetter;               The owning Getter               |
 |   ANNTPAccount: TNNTPAccount         The account to work on.         |
 *----------------------------------------------------------------------*)
constructor TTCPThread.Create(AGetter: TTCPGetter; ASettings: TServerSettings);
begin
  inherited Create(False);
  fUISync := TCriticalSection.Create;
  fGetter := AGetter;
  fSettings := ASettings;
  fTrigger := TEvent.Create(nil, False, False, '');
  fSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

// << // TODO: enable certificate verification (also see TTCPThread..VerifyPeer)
//  fSSLHandler.SSLOptions.VerifyMode := [sslvrfPeer];
  fSSLHandler.SSLOptions.Mode := sslmClient;
  fSSLHandler.SSLOptions.Method := sslvSSLv23;
//  fSSLHandler.SSLOptions.RootCertFile := '.\mozilla-root-certs.crt';
//  fSSLHandler.SSLOptions.VerifyDepth := 9;
//  fSSLHandler.OnVerifyPeer := VerifyPeer;
// >>
end;

destructor TTCPThread.Destroy;
begin
  try
    Terminate;
    fTrigger.SetEvent;
    fClient.Disconnect;
    WaitFor;
    fUISync.Free;
    fSSLHandler.Free;
    fClient.Free;
    fTrigger.Free;
  except
    Windows.Beep(440, 10);
  end;
  inherited Destroy;
end;

(*----------------------------------------------------------------------*
 | TTCPThread.GetLastResponse                                           |
 |                                                                      |
 | Get the last response from the NNTP or HTTP client.                  |
 |                                                                      |
 | The function returns the last response string.                       |
 *----------------------------------------------------------------------*)
function TTCPThread.GetLastResponse: string;
begin
  if Client.LastCmdResult.Text.Count > 0 then
    Result := Client.LastCmdResult.Text[0]
  else
    Result := '';
end;

// << TODO: add dialog when certificate is invalid
//function TTCPThread.VerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth: Integer): Boolean;
//begin
//  Result := True; // AOk;
//end;
// >>

procedure TTCPThread.GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer);
begin
  min := 0;
  max := 0;
  pos := 0;
end;

procedure TTCPThread.UILock;
begin
  fUISync.Enter;
end;

(*----------------------------------------------------------------------*
 | TTCPThread.NotifyError                                               |
 |                                                                      |
 | Called within the context of the main thread when an exception       |
 | occurs in the NNTP or HTTP client                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   None.       The error details are in fLastError                    |
 *----------------------------------------------------------------------*)
procedure TTCPThread.NotifyError;
begin
  if Assigned(ThreadManager.OnNotifyError) then
    ThreadManager.OnNotifyError(ThreadManager, fLastError);
end;


{ TTCPGetter }

procedure TTCPGetter.BeforeDestruction;
begin
  fThread.Free;
  inherited;
end;

procedure TTCPGetter.Clear;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.ClearWork                                                 |
 |                                                                      |
 | Called both before and after doing the work to reset/clear temporary |
 | data.                                                                |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.ClearWork;
begin
  // Stub
end;

function TTCPGetter.Connected: Boolean;
begin
  if (GetCurrentThreadId <> fThread.ThreadID) and (fThread.Client is TIdNNTPX) then
    Result := TIdNNTPX(fThread.Client).IsConnected
  else
    Result := fThread.Client.Connected;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.Create                                                    |
 |                                                                      |
 | Protected constructor for TTCPGetter.  Create worker thread of the   |
 | correct class.                                                       |
 *----------------------------------------------------------------------*)
constructor TTCPGetter.Create(cls: TTCPThreadClass; ASettings: TServerSettings);
begin
  fThread := cls.Create(Self, ASettings);
end;

procedure TTCPGetter.DeleteRequest(idx: Integer);
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.Disconnect                                                |
 |                                                                      |
 | Disconnect the TCP connection in the worker thread.  If it's busy    |
 | this will probably cause an exception, which is handled by the       |
 | worker thread.                                                       |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.Disconnect(Done: Boolean = False);
begin
  if Done then
    fTerminating := True;
  fThread.Client.Disconnect;
end;

function TTCPGetter.GetGetterRootText: string;
begin
  Result := 'Unknown';
end;

function TTCPGetter.GetGroup(idx: Integer): TServerAccount;
begin
  Result := nil;  // Stub
end;

function TTCPGetter.GetIsDoing(obj: TObject): Boolean;
begin
  Result := False;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetOutstandingRequestCount                                |
 |                                                                      |
 | Stub get method for the OutstandingRequestCount property.  This is   |
 | overridden by descendant classes.                                    |
 |                                                                      |
 | The function returns the Integer OutstandingRequestCount             |
 *----------------------------------------------------------------------*)
function TTCPGetter.GetOutstandingRequestCount: Integer;
begin
  Result := 0;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetOutstandingRequestText                                 |
 |                                                                      |
 | Stub get method for the OutstandingRequestText property.  This is    |
 | overridden by descendant classes.                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer       The idx of the request to get the text for.     |
 |                                                                      |
 | The function returns the string OutstandingRequestText               |
 *----------------------------------------------------------------------*)
function TTCPGetter.GetOutstandingRequestText(idx: Integer): string;
begin
  Result := ''; // Stub
end;

procedure TTCPGetter.GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer);
begin
  fThread.GetProgressNumbers(group, min, max, pos);
end;

function TTCPGetter.GetSettings: TServerSettings;
begin
  Result := fThread.fSettings;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetState                                                  |
 |                                                                      |
 | Return the worker thread state - dormant, pending, busy, etc.        |
 |                                                                      |
 |                                                                      |
 | The function returns the TNNTPThreadState                            |
 *----------------------------------------------------------------------*)
function TTCPGetter.GetState: TNNTPThreadState;
begin
  Result := fThread.State;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetStatusBarMessage                                       |
 |                                                                      |
 | Base get method for StatusBarMessage.  If the thread is busy, the    |
 | derived class will probably want to supply additional details.       |
 | otherwise it usually leaves it to this base function.                |
 |                                                                      |
 | Parameters:                                                          |
 |   group: TSubscribedGroup    Group for which status is requested.    |
 |                              may not be relevant - eg for the        |
 |                              Newsgroup getter.                       |
 |                                                                      |
 | The function returns the string StatusBarMessage                     |
 *----------------------------------------------------------------------*)
function TTCPGetter.GetStatusBarMessage(group: TServerAccount): string;
begin
  case State of
    tsDormant: Result := rstOK;
    tsPending: if fThread.fLastError <> '' then
                 Result := fThread.fLastError
               else
                 Result := rstQueued;
    tsBusy   : if ThreadManager.CurrentConnectoid = '~' then
                 Result := Format(rstDialling, [Settings.RASConnection])
               else
                 if Connected then
                   Result := Format(rstConnectedTo, [Settings.ServerName])
                 else
                   Result := Format(rstConnectingTo, [Settings.ServerName]);
    tsDone   : Result := rstDone;
  end;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.NotifyUI                                                  |
 |                                                                      |
 | Called after work has finished in the context of the main thread.    |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.NotifyUI;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.Resume                                                    |
 |                                                                      |
 | Trigger the worker thread to start working.                          |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.Resume;
begin
  if not Paused then
    fThread.Trigger.SetEvent;
end;

procedure TTCPGetter.SetPaused(const Value: Boolean);
begin
  if value <> fPaused then
  begin
    fPaused := Value;
    if not fPaused then
      Resume
    else
      repeat
        Disconnect;

        if State = tsBusy then
        begin
          Sleep(500);
          CheckSynchronize;
        end;
      until State <> tsBusy;
  end;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.SetState                                                  |
 |                                                                      |
 | Set the worker thread's state.                                       |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.SetState(const Value: TNNTPThreadState);
begin
  fThread.State := Value;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.WorkDone                                                  |
 |                                                                      |
 | Called when work has finished - before NotifyUI.  Not thread safe.   |
 |                                                                      |
 | Override this to save data.                                          |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.WorkDone;
begin
  if XNOptions.AutoDisconnectOnIdle then
    ThreadManager.DoAutoDisconnect;
end;

{ TNewsGetter }

constructor TNewsGetter.Create(cls: TTCPThreadClass; ANNTPAccount: TNNTPAccount);
begin
  inherited Create(cls, ANNTPAccount.NNTPServerSettings);
  Account := ANNTPAccount;
end;

function TNewsGetter.GetAccount: TNNTPAccount;
begin
  if fThread is TNNTPThread then
    Result := TNNTPThread(fThread).NNTPAccount
  else
    Result := nil;
end;

function TNewsGetter.GetIsDoing(obj: TObject): Boolean;
begin
  Result := obj = Account;
end;

procedure TNewsGetter.Resume;
var
  ok: Boolean;
begin
  ok := (Account.NNTPServerSettings.MaxConnections = 0) or
     (ThreadManager.CountActiveGettersForAccount(Account) < Account.NNTPServerSettings.MaxConnections);

  if not ok then
  begin
    threadManager.ClearDormantConnections(Account);

    ok := ThreadManager.CountActiveGettersForAccount(Account) < Account.NNTPServerSettings.MaxConnections;
  end;

  if ok then
    inherited Resume;
end;

procedure TNewsGetter.SetAccount(const Value: TNNTPAccount);
begin
  if fThread is TNNTPThread then
    TNNTPThread(fThread).NNTPAccount := Value;
end;

{ TMultiNewsGetter }

constructor TMultiNewsGetter.Create(cls: TTCPThreadClass;
  ANNTPAccount: TNNTPAccount);
begin
  inherited Create(cls, ANNTPAccount);
  fRequests := TObjectList.Create;
  fSync := TCriticalSection.Create;
end;

destructor TMultiNewsGetter.Destroy;
begin
  fRequests.Free;
  fSync.Free;
  inherited Destroy;
end;

function TMultiNewsGetter.GetCount: Integer;
begin
  Result := fRequests.Count;
end;

function TMultiNewsGetter.LockList: TObjectList;
begin
  if fLocked and (fLockThread = Integer(GetCurrentThreadID)) then
    raise Exception.Create('MultiNewsGetter already locked in this thread');
  fSync.Enter;
  fLocked := True;
  fLockThread := GetCurrentThreadID;
  Result := fRequests;
end;

procedure TMultiNewsGetter.UnlockList;
begin
  fLocked := False;
  fLockThread := 0;
  fSync.Leave;
end;

procedure TTCPThread.UIUnlock;
begin
  fUISync.Leave;
end;

{ TNewsgroupGetter }

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.ClearWork                                           |
 |                                                                      |
 | Called by the worker thread before and after it does work.           |
 *----------------------------------------------------------------------*)
procedure TNewsgroupGetter.ClearWork;
begin
  fNewsgroups.Clear;
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.Create                                              |
 |                                                                      |
 | Constructor for TNewsgroupGetter.  Create a string list to hold the  |
 | newsgroup list.                                                      |
 *----------------------------------------------------------------------*)
constructor TNewsgroupGetter.Create(ANNTPAccount: TNNTPAccount);
begin
  inherited Create(TNNTPNewsgroupThread, ANNTPAccount);
  fNewsgroups := TStringList.Create;
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.Destroy                                             |
 |                                                                      |
 | Destructor for TNewsgroupGetter.  Destroy the list of newsgroups.    |
 *----------------------------------------------------------------------*)
destructor TNewsgroupGetter.Destroy;
begin
  fNewsgroups.Free;
  inherited Destroy;
end;

function TNewsgroupGetter.GetGetterRootText: string;
begin
  if Assigned(Account) then
    Result := rstGetNewsgroupsFor + Account.AccountName
  else
    Result := inherited GetGetterRootText;
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.GetOutstandingRequestText                           |
 |                                                                      |
 | A Newsgroup Getter can only contain one outstanding request.  Return |
 | the name of the account for which the list is being retrieved.       |
 *----------------------------------------------------------------------*)
function TNewsgroupGetter.GetOutstandingRequestText(idx: Integer): string;
begin
  try
    Result := rstGetNewsgroupsFor + Account.AccountName;
  except
    Result := '';
  end
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.GetStatusBarMessage                                 |
 |                                                                      |
 | Return a status bar message showing the account that's being listed. |
 *----------------------------------------------------------------------*)
function TNewsgroupGetter.GetStatusBarMessage(group: TServerAccount): string;
begin
  if Connected then
    Result := rstGettingNewsgroupsFor + Account.AccountName
  else
    Result := inherited GetStatusBarMessage(group);
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.NotifyUI                                            |
 |                                                                      |
 | Call the ThreadManager's OnNewsgroupChanged event once the newsgroup |
 | list has been retrieved.                                             |
 *----------------------------------------------------------------------*)
procedure TNewsgroupGetter.NotifyUI;
begin
  if Assigned(ThreadManager.OnNewsgroupsChanged) then
    ThreadManager.OnNewsgroupsChanged(ThreadManager, Account);
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.WorkDone                                            |
 |                                                                      |
 | As soon as work is done, save the newsgroup list for the account.    |
 *----------------------------------------------------------------------*)
procedure TNewsgroupGetter.WorkDone;
var
  fName: string;
begin
  inherited;
  fName := gMessageBaseRoot + '\' + FixFileNameString(Account.AccountName);
  CreateDir(fName);
  fNewsgroups.SaveToFile(fName + '\Newsgroups.dat');
  Account.RefreshedGroupsList := True;
end;

{ TArticlesGetter }

(*----------------------------------------------------------------------*
 | TArticlesGetter.AddGroupToList                                       |
 |                                                                      |
 | Called by the ThreadManager to add an addional group to a servers    |
 | Articles getter.                                                     |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.AddGroupToList(group: TSubscribedGroup;
  fromArticle, articleCount: Int64; full: Boolean; ABatchRef: Integer; ASince: TDateTime);
begin
  LockList;
  try
    fRequests.Add(TArticlesGetterRequest.Create(group, fromArticle, articleCount, full, ABatchRef, ASince));
  finally
    UnlockList;
  end;
end;

procedure TArticlesGetter.Clear;
begin
  if state <> tsBusy then
  begin
    LockList;
    try
      fRequests.Clear;
    finally
      UnlockList;
    end;
    State := tsDormant;
    fPaused := False;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.ClearWork                                            |
 |                                                                      |
 | Called before and after work is done.  Clear the list of articles    |
 | and temporary string lists                                           |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.ClearWork;
begin
  fArticles.Clear;
  fHeader.Clear;
  fBody.Clear;
end;

(*----------------------------------------------------------------------*
 | constructor TArticlesGetter.Create                                   |
 |                                                                      |
 | Create the TArticlesGetter                                           |
 *----------------------------------------------------------------------*)
constructor TArticlesGetter.Create(ANNTPAccount: TNNTPAccount);
begin
  inherited Create(TNNTPArticlesThread, ANNTPAccount);
  fHeader := TAnsiStringList.Create;    // Temporary string list for getting headers
  fBody := TMemoryStream.Create;        // Temporary string list for getting bodies
  fArticles := TAnsiStringList.Create;  // Temporary string list containg article XOVER headers.
end;

procedure TArticlesGetter.DeleteRequest(idx: Integer);
begin
  if (State <> tsBusy) or (idx > 0) then
  begin
    LockList;
    try
      if idx < Count then
        fRequests.Delete(idx);
    finally
      UnlockList;
    end;
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False;
  end;
end;

destructor TArticlesGetter.Destroy;
begin
  fHeader.Free;
  fBody.Free;
  fArticles.Free;

  inherited Destroy;
end;

function TArticlesGetter.GetGetterRootText: string;
begin
  if Assigned(Account) then
    Result := rstGetArticleListFor + account.AccountName
  else
    Result := inherited GetGetterRootText;
end;

function TArticlesGetter.GetGroup(idx: Integer): TServerAccount;
begin
  try
    LockList;
    try
      if idx < fRequests.Count then
        Result := TArticlesGetterRequest(fRequests[idx]).Group
      else
        Result := nil;
    finally
      UnlockList;
    end;
  except
    Result := nil;
  end;
end;

function TArticlesGetter.GetIsDoing(obj: TObject): Boolean;
var
  i: Integer;
begin
  Result := inherited GetIsDoing(obj);

  if not Result then
  begin
    i := 0;
    LockList;
    try
      while (not Result) and (i < fRequests.Count) do
        if obj = TArticlesGetterRequest(fRequests[i]).fGroup then
          Result := True
        else
          Inc(i);
    finally
      UnlockList;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.GetOutstandingRequestCount                           |
 |                                                                      |
 | Get number of outstanding requests for this server's articles getter |
 *----------------------------------------------------------------------*)
function TArticlesGetter.GetOutstandingRequestCount: Integer;
begin
  Result := Count;
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.GetOutstandingRequestText                            |
 |                                                                      |
 | Get the text for outstanding request no. idx.                        |
 *----------------------------------------------------------------------*)
function TArticlesGetter.GetOutstandingRequestText(idx: Integer): string;
var
  getterRequest: TArticlesGetterRequest;
begin
  LockList;
  try
    if idx < Count then
    begin
      getterRequest := TArticlesGetterRequest(fRequests[idx]);
      Result := getterRequest.fGroup.Name;
    end
    else
      Result := '';
  finally
    UnlockList;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.GetStatusBarMessage                                  |
 |                                                                      |
 | Get status bar message                                               |
 *----------------------------------------------------------------------*)
function TArticlesGetter.GetStatusBarMessage(group: TServerAccount): string;
var
  req: TArticlesGetterRequest;
  gn: string;
  gp: TArticleContainer;
begin
  Result := inherited GetStatusBarMessage(group);
  gp := group as TArticleContainer;

  if Connected then
  begin
    LockList;
    try
      if fRequests.Count > 0 then
      begin
        req := TArticlesGetterRequest(fRequests[0]);
        gn := req.fGroup.Name;
      end;
    finally
      UnlockList;
    end;

    if group = nil then         // eg. the server is selected, not a group
    begin
      if gn <> '' then
        Result := Format(rstGettingArticles, [gn]);
    end
    else
      case State of
        tsPending: if ThreadManager.GettingArticleList(gp) then
                     Result := Format(rstQueuedPending, [gn])
                   else
                     Result := '';
        tsBusy: if CurrentFull and Assigned(CurrentGroup) and (CurrentGroup = group) then
                  if CurrentArticleNo = -1 then
                    Result := rstAboutToGetArticle
                  else
                    Result := Format(rstGettingFullArticle, [CurrentArticleNo, CurrentMax - CurrentArticleNo + 1])
                else
                  if gn = group.Name then
                    Result := Format(rstGettingArticles, [gn])
                  else
                    if ThreadManager.GettingArticleList(gp) then
                      Result := Format(rstQueuedBusy, [gn])
                    else
                      Result := ''
      end;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.SaveCurrentArticle                                   |
 |                                                                      |
 | Called by the worker thread in the context of the main thread, once  |
 | a full article has been retrieved.                                   |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.SaveCurrentArticle;
begin
  CurrentGroup.AddArticle(CurrentArticleNo, Header, Body, True);
  LogMessage(CurrentGroup.Name + ' - Saved ' + IntToStr(CurrentArticleNo));
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.UpdateArticles                                       |
 |                                                                      |
 | Called by the worker thread, in the context of the main thread once  |
 | all articles for a group has been retrieved.  This is not the        |
 | same as NotifyUI!  There may be other requests outstanding.          |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.UpdateArticles;
begin
  CurrentGroup.ReSortArticles;
  CurrentGroup.SaveArticles(False);

  if Assigned(ThreadManager.OnArticlesChanged) then
    ThreadManager.OnArticlesChanged(ThreadManager, CurrentGroup);
  LogMessage(CurrentGroup.Name + ' - Updated');
end;

procedure TArticlesGetter.UpdateHeaders;
begin
  if CurrentUpdateAll then    // Zap old articles if required.
  begin
    if ThreadManager.StopArticleDownloads(CurrentGroup) then;
(*      ClearSynchronizedMethods *) ;  // Don't think calling this within a sync handler is very safe (!)
    CurrentGroup.PurgeArticles(True, True, '')
  end;

  CurrentGroup.AddRawHeaders(fArticles, XOverFMT);
  CurrentGroup.SaveArticles(False);

  if Assigned(ThreadManager.OnArticlesChanged) then
    ThreadManager.OnArticlesChanged(ThreadManager, CurrentGroup);
  LogMessage(CurrentGroup.Name + ' - Updated');
end;

{ TArticleGetter }

(*----------------------------------------------------------------------*
 | TArticleGetter.AddArticleToList                                      |
 |                                                                      |
 | Add article to list of outstanding requests.                         |
 *----------------------------------------------------------------------*)
procedure TArticleGetter.AddArticleToList(group: TSubscribedGroup;
  article: TArticle; needsFullRefresh: Boolean);
var
  request: TArticleGetterRequest;
  i: Integer;
  dup: Boolean;
begin
  LockList;
  try
    dup := False;
    for i := 0 to fRequests.Count - 1 do
      if TArticleGetterRequest(fRequests[i]).fArticle = article then
      begin
        dup := True;
        Break;
      end;

    if not dup then
    begin
      request := TArticleGetterRequest.Create(group, article, needsFullRefresh);
      fRequests.Add(request);
    end;
  finally
    UnlockList;
  end;
end;

procedure TArticleGetter.Clear;
begin
  if state <> tsBusy then
  begin
    LockList;
    try
      fRequests.Clear;
    finally
      UnlockList;
    end;

    state := tsDormant;
    fPaused := False;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.ClearArticle                                          |
 |                                                                      |
 | Called in the context of the main thread, before an article's body   |
 | is downloaded, so we can notify the UI.  Otherwise we might zap the  |
 | message (in this thread) that's currently being displayed (in the    |
 | main thread)                                                         |
 *----------------------------------------------------------------------*)
procedure TArticleGetter.ClearArticle;
begin
  CurrentArticle.IsRead := False;
  if Assigned(ThreadManager.OnClearArticle) then
    ThreadManager.OnClearArticle(ThreadManager, CurrentArticle);

  CurrentArticle.Msg.Clear;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.Create                                                |
 |                                                                      |
 | Constructor for TArticleGetter.                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   cls: TNewsThreadClass; ANNTPAccount: TNNTPAccount                  |
 |                                                                      |
 | The function returns None                                            |
 *----------------------------------------------------------------------*)
constructor TArticleGetter.Create(ANNTPAccount: TNNTPAccount);
begin
  inherited Create(TNNTPArticleThread, ANNTPAccount);
end;

procedure TArticleGetter.DeleteRequest(idx: Integer);
begin
  if (State <> tsBusy) or (idx > 0) then
  begin
    LockList;
    try
      if idx < Count then
      begin
        fRequests.Delete(idx);
        Dec(PipelinePos);
      end;
    finally
      UnlockList;
    end;
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.FailArticle                                           |
 |                                                                      |
 | Called in the context of the main thread when an article body        |
 | download fails.  Maybe the (partial) message is being displayed, so  |
 | tell the UI that the message has failed (and will be removed!)       |
 *----------------------------------------------------------------------*)
procedure TArticleGetter.FailArticle;
begin
  if Assigned(CurrentArticle.Msg) then
    CurrentArticle.Msg.EndUpdate;
  if Assigned(ThreadManager.OnArticleFailed) then
    ThreadManager.OnArticleFailed(ThreadManager, CurrentArticle);
  CurrentArticle.RemoveMessage;
end;

function TArticleGetter.GetGetterRootText: string;
begin
  if Assigned(Account) then
    Result := rstGetArticleBodyFrom + account.AccountName
  else
    Result := inherited GetGetterRootText;
end;

function TArticleGetter.GetGroup(idx: Integer): TServerAccount;
begin
  try
    LockList;
    try
      if idx < fRequests.Count then
        Result := TArticleGetterRequest(fRequests[idx]).Group
      else
        Result := nil;
    finally
      UnlockList;
    end;
  except
    REsult := nil;
  end;
end;

function TArticleGetter.GetIsDoing(obj: TObject): Boolean;
var
  i: Integer;
begin
  Result := inherited GetIsDoing(obj);

  if not Result then
  begin
    i := 0;
    LockList;
    try
      while (not Result) and (i < fRequests.Count) do
        if (TArticleGetterRequest(fRequests[i]).fGroup = obj) or
           (TArticleGetterRequest(fRequests[i]).fArticle = obj) then
          Result := True
        else
          Inc(i);
    finally
      UnlockList;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GetOutstandingRequestCount                            |
 |                                                                      |
 | Return the number of message bodies lined up for retrieval           |
 *----------------------------------------------------------------------*)
function TArticleGetter.GetOutstandingRequestCount: Integer;
begin
  Result := Count;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GetOutstandingRequestText                             |
 |                                                                      |
 | Get the text for a message lined up for retrieval.                   |
 *----------------------------------------------------------------------*)
function TArticleGetter.GetOutstandingRequestText(idx: Integer): string;
var
  getterRequest: TArticleGetterRequest;
  requests: TObjectList;
begin
  requests := LockList;
  try
    if idx < requests.Count then
    begin
      getterRequest := TArticleGetterRequest(requests[idx]);
      Result := IntToStr(getterRequest.fArticle.ArticleNo) + ' ' + rstFrom + ' ' + getterRequest.fGroup.Name;
    end
    else
      Result := '';
  finally
    UnlockList;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GetStatusBarMessage                                   |
 |                                                                      |
 | Return the status-bar message text.                                  |
 *----------------------------------------------------------------------*)
function TArticleGetter.GetStatusBarMessage(group: TServerAccount): string;
var
  req: TArticleGetterRequest;
  gn: string;
  an: TArticle;
  gp: TArticleContainer;
begin
  Result := inherited GetStatusBarMessage(group);
  gp := group as TArticleContainer;

  if Connected then
  begin
    an := nil;

    if State = tsPending then
    begin
      LockList;
      try
        if Count > 0 then
        begin
          req := TArticleGetterRequest(fRequests[0]);
          gn := req.fGroup.Name;
          an := req.fArticle;
        end;
      finally
        UnlockList;
      end;
    end;

    fThread.UILock;
    try
      if group = nil then
        case State of
          tsPending: Result := 'Queued.  ' + gn + ' is next';
          tsBusy: if Assigned(CurrentArticle) then
                    Result := 'Getting article ' + IntToStr(CurrentArticle.ArticleNo) + ' for ' + CurrentGroup.Name;
        end
      else
        case State of
          tsPending: if ThreadManager.GettingArticle(gp, nil) then
                       if Assigned(an) then
                         Result := 'Group ' + group.Name + ' Article ' + IntToStr(an.ArticleNo) + ' queued'
                       else
                         Result := 'Group ' + group.Name + ' queued';
          tsBusy: if Assigned(CurrentArticle) then
                    if CurrentGroup = group then
                      Result := 'Getting article ' + IntToStr(CurrentArticle.ArticleNo) + ' from group ' + CurrentGroup.Name
                    else
                      Result := 'Queued'
                    else
                      Result := '';
        end;
    finally
      fThread.UIUnlock;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GotArticle                                            |
 |                                                                      |
 | Called in the context of the main thread when a message has been     |
 | successfully downloaded.  Save the message, and update the UI.       |
 *----------------------------------------------------------------------*)
procedure TArticleGetter.GotArticle;
begin
  if Assigned(CurrentArticle.Msg) then
  begin
    CurrentArticle.Msg.EndUpdate;
    CurrentArticle.Initialize(CurrentArticle.ArticleNo, CurrentArticle.Msg.Header);
    CurrentArticle.SaveMessageBody;
  end;
  if Assigned(ThreadManager.OnArticleChanged) then
    ThreadManager.OnArticleChanged(ThreadManager, CurrentArticle);
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.StartArticle                                          |
 |                                                                      |
 | Called in the context of the main thread when the body for an        |
 | article is about to be retrieved.  The worker thread is guaranteed   |
 | either call FailArticle or GotArticle next.                          |
 *----------------------------------------------------------------------*)
procedure TArticleGetter.StartArticle;
begin
  CurrentArticle.Msg.BeginUpdate;
  if Assigned(ThreadManager.OnStartArticle) then
    ThreadManager.OnStartArticle(ThreadManager, CurrentArticle);
end;

procedure TArticleGetter.UpdateArticles;
begin
  CurrentGroup.ReSortArticles;
  CurrentGroup.SaveArticles(False);

  if Assigned(ThreadManager.OnArticlesChanged) then
    ThreadManager.OnArticlesChanged(ThreadManager, CurrentGroup);
  LogMessage(CurrentGroup.Name + ' - Updated');
end;

{ TArticleGetterRequest }

constructor TArticleGetterRequest.Create(group: TSubscribedGroup;
  article: TArticle; needsFullRefresh: Boolean);
begin
  fGroup := group;
  fArticle := article;
  fNeedsFullRefresh := needsFullRefresh;
end;

{ TArticlesGetterRequest }

constructor TArticlesGetterRequest.Create(AGroup: TSubscribedGroup;
  AFromArticle, AArticleCount: Int64; full: Boolean; ABatchRef: Integer; ASince: TDateTime);
begin
  fGroup := AGroup;
  fFromArticle := AFromArticle;
  fArticleCount := AArticleCount;
  fFull := full;
  fBatchRef := ABatchRef;
  fSince := ASince;
end;

{ TPoster }

procedure TPoster.AddPostToList(hdr: TAnsiStrings; const msg: RawByteString;
  attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle);
begin
  LockList;
  try
    fRequests.Add(TPosterRequest.Create(Self, hdr, msg, attachments, ACodepage, ATextPartStyle));
  finally
    Unlocklist;
  end;
end;

procedure TPoster.Clear;
begin
  if state <> tsBusy then
  begin
    LockList;
    try
      fRequests.Clear
    finally
      UnlockList;
    end;
    state := tsDormant;
    fPaused := False;
  end;
end;

constructor TPoster.Create(AAccount: TNNTPAccount);
begin
  inherited Create(TNNTPPoster, AAccount);
  fUseOutbasket := AAccount.PostingSettings.DelayPosting;
end;

procedure TPoster.DeleteRequest(idx: Integer);
begin
  if (State <> tsBusy) or (idx > 0) then
  begin
    LockList;
    try
      if (idx >= 0) and (idx < fRequests.Count) then
        fRequests.Delete(idx);
    finally
      UnlockList;
    end;
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False;
  end;
end;


{ TPost }

procedure TPosterRequest.AddAttachments(msg: TAnsiStrings;
  const multipartBoundary: RawByteString);
var
  i, chunkLen: Integer;
  attachment: TAttachment;
  f: TFileStream;
  encoder: TidEncoder;
  s: string;
  contentType: RawByteString;

  function GetMIMEContentType(const fileName: string): RawByteString;
  var
    ext: string;
  begin
    ext := UpperCase(ExtractFileExt(fileName));

    if ext = '.JPG' then
      Result := 'Image/JPEG'
    else
      if ext = '.GIF' then
        Result := 'Image/GIF'
      else
        if ext = '.TIF' then
          Result := 'Image/TIFF'
        else
          if ext = '.PNG' then
            Result := 'Image/PNG'
          else
            Result := 'Application/Octet-Stream';
  end;

  function FixFileName(const fileName: string): RawByteString;
  var
    i: Integer;
    needsQuotes, cantQuote: Boolean;
  begin
    // Tidy up filename.
    //
    // 1.  If it contains whitespace then 'quote' the string
    //     if possible
    //
    // 2.  Convert illegal characters to '_'
    //
    // 3.  If the string already contains '"', then don't quote it

// !!!!!! filenames, check RFC?
    Result := RawByteString(fileName);

    needsQuotes := False;
    cantQuote := False;
    for i := 1 to Length(Result) do
      case Result[i] of

        #0..#8, #11..#12, #14..#31:  // Non Ascii
          Result[i] := '_';

                                      // Whitespace
        #9, #10, #13, ' ': needsQuotes := True;
        '"': cantQuote := true;

                                      // Illegal characters
        ':', '\', '/', '>', '<':
          Result[i] := '_';
      end;

    if needsQuotes and not cantQuote then
      Result := '"' + Result + '"';
  end;

begin
  if not Assigned(Attachments) then
    Exit;

  for i := 0 to Attachments.Count - 1 do
  begin
    attachment := TAttachment(attachments[i]);

    if FileExists(attachment.Pathname) then
    begin
      encoder := nil;
      f := TFileStream.Create(attachment.PathName, fmOpenRead or fmShareDenyNone);
      try
        if multipartBoundary <> '' then
        begin
          encoder := TidEncoderMIME.Create(nil);
          msg.Add('--' + multipartBoundary);
          contentType := GetMIMEContentType(attachment.FileName);
          msg.Add('Content-Type: ' + contentType + '; name=' + FixFilename(attachment.FileName));
          msg.Add('Content-Transfer-Encoding: base64');
          msg.Add('Content-Disposition: attachment; filename=' + FixFilename(attachment.FileName));
          msg.Add('');
          chunkLen := 57;
        end
        else
        begin
          msg.Add('');
          msg.Add('begin 444 ' + FixFileName(attachment.FileName));
          encoder := TXnEncoderUUE.Create(nil);
          chunkLen := 45;
        end;

        while f.Position < f.Size do
        begin
          s := encoder.Encode(f, chunkLen);
          // TODO: needs to be improved
          msg.Add(RawByteString(s));
        end;

        if multipartBoundary <> '' then
          msg.Add('') // do we need this??
        else
          msg.Add('end');
      finally
        f.Free;
        encoder.Free;
      end;
    end;
  end;

  if multipartBoundary <> '' then
    msg.Add('--' + multipartBoundary + '--');
end;

constructor TPosterRequest.Create(AOwner: TPoster; AHdr: TAnsiStrings; const AMsg: RawByteString;
  attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle);
begin
  fOwner := AOwner;
  fTextPartStyle := ATextPartStyle;
  fHdr := TAnsiStringList.Create;
  fHdr.Assign(AHdr);
  fMsg := AMsg;
  fAttachments := attachments;
  fCodepage := ACodepage;
end;

procedure TPosterRequest.CreateEncodedHeader(var hdr: TAnsiStrings;
  var hdrCreated: Boolean; var multipartBoundary: RawByteString);
var
  s, s1: RawByteString;
  mimeCharsetName: RawByteString;
  i: Integer;
  bt8: Boolean;

  function EncodeFace(S: RawByteString): RawByteString;
  const
    SPACES: set of AnsiChar = [' ', #9, #10, #13, '''', '"'];    {Do not Localize}
    MaxEncLen = 76;
  var
    I, L, Q: Integer;
  begin
    Result := 'Face=';
    I := 6;
    Q := 5;
    L := Length(S);

    // Skip spaces at the beginning of the original Face data.
    while (I <= L) and (S[I] in Spaces) do
      Inc(I);

    while I < L do
    begin
      if Q > MaxEncLen then
      begin
        Q := 2;
        Result := Result + #13#10' ';
      end;
      Result := Result + S[I];
      Inc(I);
      Result := Result + S[I];
      Inc(I);
      Inc(Q, 2);
    end;
  end;

  function GenerateMultipartBoundary: RawByteString;
  begin
    Result := 'Xananews.1.2.3';
  end;

begin
  multipartBoundary := '';
  hdrCreated := MimeHeaderRequired();

  if hdrCreated then
    hdr := TAnsiStringList.Create
  else
    hdr := Self.Hdr;

  try
    if hdrCreated then
    begin
      mimeCharsetName := RawCodepageToMIMECharsetName(fCodepage);
      if mimeCharsetName <> '' then
        for i := 0 to Self.hdr.Count - 1 do
        begin
          s := Self.Hdr[i];
          if RawPos('Face', s) = 1 then
            hdr.Add(EncodeFace(s))
          else
            if RawPos('X-Face', s) = 1 then
              hdr.Add(s)
            else
            begin
              s1 := RawSplitString('=', s);
              if RawPos('Newsgroups', s1) = 1 then
                hdr.Add(s1 + '=' + s)
              else
                hdr.Add(s1 + '=' + EncodeHeader(s, fCodePage, RawPos('From', s1) = 1, Length(s1) + 2));
            end;
        end
      else
        hdr.Assign(Self.Hdr);

      hdr.Add('MIME-Version=1.0');
      if not Assigned(attachments) or (attachments.Count = 0) then
      begin
        s := 'Content-Type=text/plain';

        if mimeCharsetName <> '' then
          s := s + '; charset=' + mimeCharsetName;

        if fTextPartStyle = tpFlowed then
          s := s + '; format=flowed';

        hdr.Add(s);

        if fTextPartStyle = tpQuotedPrintable then
          hdr.Add('Content-Transfer-Encoding=quoted-printable')
        else
        begin
          bt8 := False;
          for i := 1 to Length(msg) do
            if not (Msg[i] in [#9, #10, #13, ' '..'~']) then
            begin
              bt8 := True;
              Break;
            end;

          if bt8 then
            hdr.Add('Content-Transfer-Encoding=8bit')
          else
            hdr.Add('Content-Transfer-Encoding=7bit');
        end;
      end
      else
      begin
        multipartBoundary := GenerateMultipartBoundary;
        hdr.Add('Content-Type=multipart/mixed; boundary="' + multipartBoundary + '"');
      end;
    end;
  except
    if hdrCreated then
      hdr.Free;
    raise;
  end;
end;

procedure TPosterRequest.CreateEncodedMessage(var msg: TAnsiStrings; const multipartBoundary: RawByteString);
var
  bt8: Boolean;
  i: Integer;
  s: RawByteString;
  mimeCharsetName: RawByteString;

  procedure AddMessageString(msg: TAnsiStrings; const st: RawByteString);
  var
    m: TAnsiStrings;
    coder: TidEncoder;
  begin
    m := TAnsiStringList.Create;
    try
      m.Text := st;
      if fTextPartStyle = tpQuotedPrintable then
      begin
        coder := TXnEncoderQuotedPrintable.Create(nil);
        TXnEncoderQuotedPrintable(coder).AddEOL := True;
        try
          TXnEncoderQuotedPrintable(coder).EncodeStrings(m);
        finally
          coder.Free;
        end;
      end
      else
        if fTextPartStyle = tpFlowed then
        begin
          coder := TRFC2646Encoder.Create(nil);
          try
            TRFC2646Encoder(coder).EncodeStrings(m);
          finally
            coder.Free;
          end;
        end;
      msg.AddAnsiStrings(m);
    finally
      m.Free;
    end;
  end;

begin
  msg := TAnsiStringList.Create;

  try
    if multipartBoundary <> '' then
    begin
      msg.Add('This is a multipart message in MIME format');
      msg.Add('');
      msg.Add('--' + multipartBoundary);

      mimeCharsetName := RawCodepageToMIMECharsetName(fCodepage);

      s := 'Content-Type: text/plain';
      if mimeCharsetName <> '' then
        s := s + '; charset=' + mimeCharsetName;
      if fTextPartStyle = tpFlowed then
        s := s + '; format=flowed';
      msg.Add(s);

      if fTextPartStyle = tpQuotedPrintable then
        msg.Add('Content-Transfer-Encoding: quoted-printable')
      else
      begin
        bt8 := False;
        for i := 1 to Length(Self.Msg) do
          if not (Self.Msg[i] in [#9, #10, #13, ' '..'~']) then
          begin
            bt8 := True;
            Break;
          end;

        if bt8 then
          msg.Add('Content-Transfer-Encoding: 8bit')
        else
          msg.Add('Content-Transfer-Encoding: 7bit');
      end;
      msg.Add('Content-Disposition: inline');
      msg.Add('');

      AddMessageString(msg, Self.Msg);

      msg.Add('');
      if Attachments.Count = 0 then
        msg.Add('--' + multipartBoundary + '--');
    end
    else
      AddMessageString(msg, Self.Msg);
  except
    msg.Free;
    raise;
  end;
end;

destructor TPosterRequest.Destroy;
begin
  fHdr.Free;
  fAttachments.Free;
  inherited Destroy;
end;

function TPoster.GetGetterRootText: string;
begin
  Result := 'Post message(s) to ' + Settings.ServerName;
end;

function TPoster.GetOutstandingRequestCount: Integer;
begin
  Result := Count;
end;

function TPoster.GetOutstandingRequestText(idx: Integer): string;
var
  post: TPosterRequest;
begin
  LockList;
  try
    if idx < fRequests.Count then
    begin
      post := TPosterRequest(fRequests[idx]);
      Result := string(post.Groups + ':');
      Result := Result + AnsiStringToWideString(post.Subject, post.Codepage);
    end
    else
      Result := '';
  finally
    UnlockList;
  end;
end;

function TPoster.GetStatusBarMessage(group: TServerAccount): string;
var
  req: TPosterRequest;
  s: string;
begin
  Result := inherited GetStatusBarMessage(group);

  if Connected then
  begin
    s := '';
    LockList;
    try
      if Count > 0 then
      begin
        req := TPosterRequest(fRequests[0]);
        s := string(req.Groups + ':');
        s := s + AnsiStringToWideString(req.Subject, req.Codepage);
      end;
    finally
      UnlockList;
    end;

    if s <> '' then
      case State of
        tsPending: Result := 'Queued ' + s;
        tsBusy: Result := 'Posting ' + s;
      end;
  end;
end;

procedure TPosterRequest.GetGroupAndSubject;
var
  i: Integer;
  s, s1: RawByteString;
begin
  if (fGroups = '') or (fSubject = '') then
  begin
    for i := 0 to fHdr.Count - 1 do
    begin
      s := fHdr[i];
      s1 := RawSplitString('=', s);
      if RawCompareText(s1, 'Newsgroups') = 0 then
        fGroups := RawTrim(s);

      if RawCompareText(s1, 'Subject') = 0 then
        fSubject := RawTrim(s);

      if (fSubject <> '') and (fGroups <> '') then
        Break;
    end;
  end;
end;

function TPosterRequest.GetGroups: RawByteString;
begin
  GetGroupAndSubject;
  Result := fGroups;
end;

function TPosterRequest.GetSubject: RawByteString;
begin
  GetGroupAndSubject;
  Result := fSubject;
end;

function TPosterRequest.MIMEHeaderRequired(): Boolean;
var
  i: Integer;
begin
  Result := (fCodepage <> CP_USASCII) or (fTextPartStyle <> tpNNTP);
  if not Result and Assigned(attachments) then
    for i := 0 to Attachments.Count - 1 do
      if not TAttachment(Attachments[i]).fInline then
      begin
        Result := True;
        Break;
      end;
end;

procedure TPosterRequest.Reset;
begin
  fGroups := '';
  fSubject := '';
end;

procedure TPoster.Resume;
begin
  if fUseOutbasket then
    fUseOutbasket := Account.PostingSettings.DelayPosting;

  if not fUseOutbasket then
    inherited Resume;
end;

procedure TPoster.ResumeOutbasket;
begin
  fUseOutbasket := False;
  Resume;
end;

procedure TPoster.WorkDone;
begin
  inherited WorkDone;
  fUseOutbasket := Account.PostingSettings.DelayPosting;
end;

{ TAttachment }

constructor TAttachment.Create(APathName: string);
var
  f: TSearchRec;
begin
  if FindFirst(APathName, -1, f) = 0 then
  begin
    fDirectory := ExtractFilePath(APathName);
    fFilename := f.Name;
    fSize := f.Size;
    {$if CompilerVersion >= 22.0} // 22.0 = Delphi XE
      fDate := f.TimeStamp;
    {$else}
      fDate := FileDateToDateTime(f.Time);
    {$ifend}
    fInline := True;
    FindClose(f);
  end
  else
    raise Exception.Create('File ' + APathName + ' not found');
end;

function TAttachment.GetPathName: string;
begin
  Result := fDirectory + fFileName;
end;


{ TEMailer }

procedure TEMailer.AddMessageToList(AArticleContainer: TServerAccount;
  const sTo, sCC, sBCC, sSubject, sReplyTo, msg: string;
  attachments: TObjectList; ACodePage: Integer);
begin
  LockList;
  try
    fMessages.Add(TEMailerRequest.Create(AArticleContainer, Self,
      sTo, sCC, sBCC, sSubject, sReplyTo, msg, attachments, ACodePage))
  finally
    UnlockList;
  end;
end;

constructor TEMailer.Create(settings: TSMTPServerSettings; AUseOutbasket: Boolean);
begin
  inherited Create(TSMTPMailer, settings);
  fMessages := TObjectList.Create;
  fSync := TCriticalSection.Create;
  fOrigUseOutbasket := AUseOutbasket;
  fUseOutbasket := AUseOutbasket;
end;

destructor TEMailer.Destroy;
begin
  fMessages.Free;
  fSync.Free;
  inherited Destroy;
end;

procedure TEMailer.DeleteRequest(idx: Integer);
begin
  if (State <> tsBusy) or (idx > 0) then
  begin
    LockList;
    try
      if (idx >= 0) and (idx < fMessages.Count) then
        fMessages.Delete(idx);
    finally
      UnlockList;
    end;
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False;
  end;
end;

function TEMailer.GetCount: Integer;
begin
  Result := fMessages.Count;
end;

function TEMailer.GetGetterRootText: string;
begin
  Result := 'Mail message(s) to ' + Settings.ServerName;
end;

function TEMailer.GetGroup(idx: Integer): TServerAccount;
begin
  try
    LockList;
    try
      if idx < fMessages.Count then
        Result := TEMailerRequest(fMessages[idx]).fArticleContainer
      else
        Result := nil;
    finally
      UnlockList;
    end;
  except
    Result := nil;
  end;
end;

function TEMailer.GetOutstandingRequestCount: Integer;
begin
  Result := Count;
end;

function TEMailer.GetOutstandingRequestText(idx: Integer): string;
var
  msg: TEMailerRequest;
begin
  LockList;
  try
    if idx < Count then
    begin
      msg := TEMailerRequest(fMessages[idx]);
      Result := msg.MTo + ':' + msg.MSubject;
    end
    else
      Result := '';
  finally
    UnlockList;
  end;
end;

function TEMailer.GetStatusBarMessage(group: TServerAccount): string;
var
  req: TEMailerRequest;
  s: string;
begin
  Result := inherited GetStatusBarMessage(group);

  if fThread.Client.Connected then
  begin
    s := '';
    LockList;
    try
      if Count > 0 then
      begin
        req := TEMailerRequest(fMessages[0]);
        s := req.MTo + ':' + req.MSubject;
      end;
    finally
      UnLockList;
    end;

    if s <> '' then
      case State of
        tsPending: Result := 'Queued ' + s;
        tsBusy: Result := 'Posting ' + s;
      end;
  end;
end;

function TEMailer.LockList: TObjectList;
begin
  if fLocked and (fLockThread = Integer(GetCurrentThreadID)) then
    raise Exception.Create('EMailer already locked in this thread');
  fSync.Enter;
  fLocked := True;
  fLockThread := GetCurrentThreadID;
  Result := fMessages;
end;

procedure TEMailer.Resume;
begin
  if not fUseOutbasket then
    inherited;
end;

procedure TEMailer.ResumeOutbasket;
begin
  fUseOutbasket := False;
  Resume;
end;

procedure TEMailer.UnlockList;
begin
  fLocked := False;
  fLockThread := 0;
  fSync.Leave;
end;

procedure TEMailer.WorkDone;
begin
  inherited;
  fUseOutbasket := fOrigUseOutbasket;
end;

{ TEMailerRequest }

constructor TEMailerRequest.Create(AArticleContainer: TServerAccount; AOwner: TEmailer;
  const ATo, ACC, ABCC, ASubject, AReplyTo, Amsg: string;
  attachments: TObjectList; ACodePage: Integer);
begin
  fArticleContainer := AArticleContainer;
  fOwner := AOwner;
  fTo := ATo;
  fCC := ACC;
  fBCC := ABCC;
  fSubject := ASubject;
  fMsg := AMsg;
  fAttachments := attachments;
  fCodePage := ACodePage;
  fReplyTo := AReplyTo;
end;

destructor TEMailerRequest.Destroy;
begin
  fAttachments.Free;
  inherited Destroy;
end;

function TEMailerRequest.GetMailAccount: TMailAccount;
begin
  Result := nil;
  if ArticleContainer is TSubscribedGroup then
    Result := MailAccounts.FindMailAccount(TSubscribedGroup(ArticleContainer).Owner.MailAccountName)
  else
    if ArticleContainer is TMailAccount then
      Result := TMailAccount(ArticleContainer);
end;

end.
