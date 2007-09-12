(*======================================================================*
 | unitNewsThread                                                       |
 |                                                                      |
 | TNewsThread base class                                               |
 | TTCPGetter classes.                                                 |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitNewsThread;

interface

uses Windows, Classes, Graphics, SysUtils, unitNNTPServices, unitMailServices, SyncObjs, IdTCPClient, ConTnrs, IdSSLOpenSSL, unitSettings, unitIdentities, IdMessage, NewsGlobals;

type
TNNTPThreadState = (tsDormant, tsPending, tsBusy, tsDone);

TTCPGetter = class;

//-----------------------------------------------------------------------
// Base class for news reading threads.  Overridden by TNNTPThread
// and TDNewsThread
TTCPThread = class (TThread)
private
  fTrigger : TEvent;
  fState : TNNTPThreadState;
  fGetter : TTCPGetter;
  fSSLHandler: TIdSSLIOHandlerSocket;
  fUISync : TCriticalSection;
  function GetLastResponse: string;

protected
  fSettings : TServerSettings;
  fLastError : string;
  fClient : TidTCPClient;

  procedure DoWork; virtual; abstract;
  procedure NotifyError;

public
  constructor Create (AGetter : TTCPGetter; ASettings : TServerSettings); virtual;
  destructor Destroy; override;
  procedure GetProgressNumbers (group : TServerAccount; var min, max, pos : Integer); virtual;

  procedure UILock;
  procedure UIUnlock;

  property Trigger : TEvent read fTrigger;
  property State : TNNTPThreadState read fState write fState;
  property Client : TidTCPClient read fClient;
  property LastResponse : string read GetLastResponse;
  property Getter : TTCPGetter read fGetter;
  property SSLHandler : TidSSLIOHandlerSocket read fSSLHandler;
end;

TTCPThreadClass = class of TTCPThread;

//-----------------------------------------------------------------------
// Base class for getters
TTCPGetter = class
private
  fPaused : boolean;
  fTerminating : boolean;
  function GetState: TNNTPThreadState;
  procedure SetState(const Value: TNNTPThreadState);
  function GetOutstandingRequestText (idx : Integer) : WideString; virtual;
  function GetOutstandingRequestCount: Integer; virtual;
  function GetStatusBarMessage(group: TServerAccount): WideString; virtual;
  function GetGetterRootText : WideString; virtual;
  procedure SetPaused(const Value: boolean);
  function GetSettings: TServerSettings;
protected
  fThread : TTCPThread;
  constructor Create (cls : TTCPThreadClass; ASettings : TServerSettings);
  function GetIsDoing(obj : TObject) : boolean; virtual;
  function GetGroup(idx: Integer): TServerAccount; virtual;
  property Terminating : boolean read fTerminating;
public
  destructor Destroy; override;

  procedure Disconnect;
  function Connected : boolean;
  procedure Resume; virtual;
  property Paused : boolean read fPaused write SetPaused;


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
  procedure DeleteRequest (idx : Integer); virtual;

  property Settings : TServerSettings read GetSettings;
  property State : TNNTPThreadState read GetState write SetState;
  property OutstandingRequestCount : Integer read GetOutstandingRequestCount;
  property OutstandingRequestText [idx : Integer] : WideString read GetOutstandingRequestText;
  property StatusBarMessage [group : TServerAccount] : WideString read GetStatusBarMessage;
  property IsDoing [obj : TObject] : boolean read GetIsDoing;
  property GetterRootText : WideString read GetGetterRootText;
  procedure GetProgressNumbers (group : TServerAccount; var min, max, pos : Integer);
  property Group [idx : Integer] : TServerAccount read GetGroup;
end;

TTCPGetterClass = class of TTCPGetter;

TNewsGetter = class (TTCPGetter)
private
  procedure SetAccount(const Value: TNNTPAccount);
  function GetAccount: TNNTPAccount;
protected
  function GetIsDoing(obj : TObject) : boolean; override;
public
  constructor Create (cls : TTCPThreadClass; ANNTPAccount : TNNTPAccount);
  procedure Resume; override;
  property Account : TNNTPAccount read GetAccount write SetAccount;
end;

TMultiNewsGetter = class (TNewsGetter)
private
  fRequests : TObjectList;
  fSync : TCriticalSection;
  fLocked : boolean;
  fLockThread : Integer;
  function GetCount: Integer;
public
  constructor Create  (cls : TTCPThreadClass; ANNTPAccount : TNNTPAccount);
  destructor Destroy; override;
  function LockList : TObjectList;
  procedure UnlockList;
  property Count : Integer read GetCount;
  property Locked : boolean read fLocked;
end;

//-----------------------------------------------------------------------
// Getter for newsgroup lists
TNewsgroupGetter = class (TNewsGetter)
  fNewsgroups : TStringList;
  function GetOutstandingRequestText (idx : Integer) : WideString; override;
  function GetStatusBarMessage(group: TServerAccount): WideString; override;
  function GetGetterRootText : WideString; override;
public
  constructor Create (ANNTPAccount : TNNTPAccount);
  destructor Destroy; override;

  procedure ClearWork; override;        //
  procedure WorkDone; override;         // Called when work has finished. (Individual thread)
  procedure NotifyUI; override;         // Called when work has finished. (Main thread)
end;

TArticlesGetterRequest = class
private
  fGroup : TSubscribedGroup;
  fFromArticle : Integer;
  fArticleCount : Integer;
  fFull : boolean;
  fAbandon: boolean;
  fBatchRef : Integer;
  fSince : TDateTime;
public
  constructor Create (AGroup : TSubscribedGroup; AFromArticle, AArticleCount : Integer; full : boolean; ABatchRef : Integer; ASince : TDateTime);
  property Group : TSubscribedGroup read fGroup;
  property Full : Boolean  read fFull;
  property FromArticle : Integer  read fFromArticle;
  property ArticleCount : Integer read fArticleCount;
  property Abandon : boolean read fAbandon write fAbandon;
  property BatchRef : Integer read fBatchRef;
  property Since : TDateTime read fSince;
end;

//-----------------------------------------------------------------------
// Getter for bulk article/header downloading
TArticlesGetter = class (TMultiNewsGetter)
private
  fHeader : TStrings;
  fBody : TMemoryStream;
  fArticles : TStringList;

  function GetOutstandingRequestCount: Integer; override;
  function GetOutstandingRequestText (idx : Integer) : WideString; override;
  function GetStatusBarMessage(group: TServerAccount): WideString; override;
  function GetGetterRootText : WideString; override;

protected
  function GetIsDoing(obj : TObject): boolean; override;
  function GetGroup(idx: Integer): TServerAccount; override;

public
  CurrentFull : Boolean;
  CurrentUpdateAll : boolean;
  CurrentArticleNo : Integer;
  CurrentMax : Integer;
  CurrentGroup : TSubscribedGroup;
  XOverFMT : TStringList;

  constructor Create (ANNTPAccount : TNNTPAccount);
  destructor Destroy; override;
  procedure Clear; override;
  procedure DeleteRequest (idx : Integer); override;
  procedure AddGroupToList (group : TSubscribedGroup; fromArticle, articleCount : Integer; full : boolean; ABatchRef : Integer; ASince : TDateTime);
  procedure ClearWork; override;

  procedure SaveCurrentArticle;
  procedure UpdateArticles;
  procedure UpdateHeaders;

  property Articles : TStringList read fArticles;
  property Header : TStrings read fHeader;
  property Body : TMemoryStream read fBody;
end;

TArticleGetterRequest = class
private
  fGroup : TSubscribedGroup;
  fArticle : TArticle;
  fNeedsFullRefresh : boolean;
public
  constructor Create (group : TSubscribedGroup; article : TArticle; needsFullRefresh : boolean);
  property Group : TSubscribedGroup read fGroup;
  property Article : TArticle read fArticle;
  property NeedsFullRefresh : boolean read fNeedsFullRefresh;
end;

//-----------------------------------------------------------------------
// Getter for individual article downloading
TArticleGetter = class (TMultiNewsGetter)
private
  function GetOutstandingRequestCount: Integer; override;
  function GetOutstandingRequestText (idx : Integer) : WideString; override;
  function GetStatusBarMessage(group: TServerAccount): WideString; override;
  function GetGetterRootText : WideString; override;
protected
  function GetIsDoing(obj : TObject): boolean; override;
  function GetGroup(idx: Integer): TServerAccount; override;
public
  CurrentArticle : TArticle;
  CurrentGroup : TSubscribedGroup;
  PipelineGroup : TSubscribedGroup;
  PipelinePos : Integer;

  constructor Create (ANNTPAccount : TNNTPAccount);

  procedure Clear; override;
  procedure DeleteRequest (idx : Integer); override;
  procedure ClearArticle;
  procedure GotArticle;
  procedure FailArticle;
  procedure StartArticle;
  procedure Update;

  procedure AddArticleToList (group : TSubscribedGroup; article : TArticle; needsFullRefresh : boolean = false);
end;

TAttachment = class
private
  fSize: Integer;
  fDate: Integer;
  fDirectory: string;
  fFileName: string;
  fInline : boolean;

  function GetPathName: string;
public
  constructor Create (APathName : string);
  property FileName : string read fFileName;
  property Directory : string read fDirectory;
  property Date : Integer read fDate;
  property Size : Integer read fSize;
  property IsInline : boolean read fInline;

  property PathName : string read GetPathName;
end;

TPoster = class;

TPosterRequest = class
private
  fOwner : TPoster;
  fHdr : TStrings;
  fMsg : string;

  fGroups : string;
  fSubject : string;
  fAttachments: TObjectList;
  fCodepage : Integer;
  fTextPartStyle : TTextPartStyle;
  function GetGroups: string;
  function GetSubject: string;
  procedure GetGroupAndSubject;
  function MIMEHeaderRequired (Ahdr : TStrings): boolean;

public
  constructor Create (AOwner : TPoster; AHdr : TStrings; const AMsg : string; attachments : TObjectList; ACodepage : Integer; ATextPartStyle : TTextPartStyle);
  destructor Destroy; override;
  procedure Reset;

  procedure CreateEncodedHeader (var hdr : TStrings; var hdrCreated : boolean; var multipartBoundary : string);
  procedure CreateEncodedMessage (var msg : TStrings; const multipartBoundary : string);
  procedure AddAttachments (msg : TStrings; const multipartBoundary : string);

  property Hdr : TStrings read fHdr;
  property Msg : string read fMsg write fMsg;

  property Groups : string read GetGroups;
  property Subject : string read GetSubject;

  property Owner : TPoster read fOwner;
  property Attachments : TObjectList read fAttachments write fAttachments;

  property Codepage : Integer read fCodepage;
end;

//-----------------------------------------------------------------------
// Getter for posting messages
TPoster = class (TMultiNewsGetter)
private
  fUseOutbasket : boolean;
  function GetOutstandingRequestCount: Integer; override;
  function GetOutstandingRequestText (idx : Integer) : WideString; override;
  function GetGetterRootText : WideString; override;
  function GetStatusBarMessage(group: TServerAccount): WideString; override;
protected
public
  constructor Create (AAccount : TNNTPAccount);

  procedure WorkDone; override;

  procedure Resume; override;
  procedure ResumeOutbasket;
  procedure Clear; override;
  procedure DeleteRequest (idx : Integer); override;
  procedure AddPostToList (hdr : TStrings; const msg : string; attachments : TObjectList; ACodepage : Integer; ATextPartStyle : TTextPartStyle);
  property UseOutbasket : boolean read fUseOutbasket;
end;

TEmailer = class;

TEMailerRequest = class
private
  fMsg: string;
  fAttachments: TObjectList;
  fOwner: TEMailer;
  fCodePage : Integer;
  fCC: string;
  fBCC: string;
  fTo: string;
  fSubject: string;
  fArticleContainer : TServerAccount;
  fReplyTo: string;
public
  constructor Create (AArticleContainer : TServerAccount; AOwner : TEMailer; const ATo, ACC, ABCC, ASubject, AReplyTo, AMsg  : string; attachments : TObjectList; ACodePage : Integer);
  destructor Destroy; override;

  property MTo : string read fTo write fTo;
  property MCC : string read fCC write fCC;
  property MBCC : string read fBCC write fBCC;
  property MSubject : string read fSubject write fSubject;
  property Msg : string read fMsg write fMsg;
  property MReplyTo : string read fReplyTo;

  property Owner : TEMailer read fOwner;
  property Attachments : TObjectList read fAttachments write fAttachments;
  property CodePage : Integer read fCodePage write fCodePage;
  property ArticleContainer : TServerAccount read fArticleContainer;
end;

TEMailer = class (TTCPGetter)
private
  fMessages : TObjectList;
  fCurrentMessage : TidMessage;
  fUseOutbasket: boolean;
  fOrigUseOutbasket : boolean;

  function GetOutstandingRequestCount: Integer; override;
  function GetOutstandingRequestText (idx : Integer) : WideString; override;
  function GetGetterRootText : WideString; override;
  function GetStatusBarMessage(group: TServerAccount): WideString; override;
protected
  function GetGroup(idx: Integer): TServerAccount; override;

public
  constructor Create (settings : TSMTPServerSettings; AUseOutbasket : boolean);
  destructor Destroy; override;
  procedure AddMessageToList (AArticleContainer : TServerAccount; const sTo, sCC, sBCC, sSubject, sReplyTo, msg : string; attachments : TObjectList; ACodePage : Integer);
  procedure Resume; override;
  procedure ResumeOutbasket;
  property Messages : TObjectList read fMessages;
  procedure WorkDone; override;
  property CurrentMessage : TidMessage read fCurrentMessage write fCurrentMessage;

//  procedure SaveOutgoingPost;
  property UseOutbasket : boolean read fUseOutbasket;
  property OrigUseOutbasket : boolean read fOrigUseOutbasket;

end;


implementation

uses unitNNTPThreadManager, unitNewsReaderOptions, unitNNTPThreads,
     idCoder, idCoderUUE, idCoderMIME, unitCharsetMap, unitSearchString,
     IdSMTP, idHeaderList, unitLog, unitRFC2646Coder;

{ TTCPThread }

(*----------------------------------------------------------------------*
 | TTCPThread.Create                                                   |
 |                                                                      |
 | Each 'getter' has a worker thread associated with it.  During their  |
 | constructor the create the worker thread.                            |
 |                                                                      |
 | Parameters:                                                          |
 |   AGetter : TTCPGetter;             The owning Getter               |
 |   ANNTPAccount: TNNTPAccount         The account to work on.         |
 *----------------------------------------------------------------------*)
constructor TTCPThread.Create(AGetter : TTCPGetter; ASettings : TServerSettings);
begin
  inherited Create (True);      // Create suspended.
  fUISync := TCriticalSection.Create;
  fGetter := AGetter;
  fSettings := ASettings;
  fTrigger := TEvent.Create(Nil, False, False, '');
  fSSLHandler := TIdSSLIOHandlerSocket.Create(nil);
  fSSLHandler.SSLOptions.Mode := sslmClient;
  fSSLHandler.SSLOptions.Method := sslvSSLv23;
  Resume;
end;

(*----------------------------------------------------------------------*
 | TTCPThread.Destroy                                                  |
 |                                                                      |
 | Destructor for TTCPThread worker thread.                            |
 *----------------------------------------------------------------------*)
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
    Windows.Beep (440, 10);
  end;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TTCPThread.GetLastResponse                                          |
 |                                                                      |
 | Get the last response from the NNTP or HTTP client.                  |
 |                                                                      |
 | The function returns the last response string.                       |
 *----------------------------------------------------------------------*)
function TTCPThread.GetLastResponse: string;
begin
  if Client.LastCmdResult.Text.Count > 0 then
    Result := Client.LastCmdResult.Text [0]
  else
    Result := '';
end;

(*----------------------------------------------------------------------*
 | TTCPThread.NotifyError                                              |
 |                                                                      |
 | Called within the context of the main thread when an exception       |
 | occurs in the NNTP or HTTP client                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   None.       The error details are in fLastError                    |
 *----------------------------------------------------------------------*)
procedure TTCPThread.GetProgressNumbers(group : TServerAccount; var min, max, pos: Integer);
begin
  min := 0;
  max := 0;
  pos := 0;
end;

procedure TTCPThread.UILock;
begin
  fUISync.Enter;
end;

procedure TTCPThread.NotifyError;
begin
  if Assigned (ThreadManager.OnNotifyError) then
    ThreadManager.OnNotifyError (ThreadManager, fLastError);
end;


{ TTCPGetter }

(*----------------------------------------------------------------------*
 | TTCPGetter.ClearWork                                                |
 |                                                                      |
 | Called both before and after doing the work to reset/clear temporary |
 | data.                                                                |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.Clear;
begin
// stub
end;

procedure TTCPGetter.ClearWork;
begin
end; // Stub

(*----------------------------------------------------------------------*
 | TTCPGetter.Create                                                   |
 |                                                                      |
 | Protected constructor for TTCPGetter.  Create worker thread of the  |
 | correct class.                                                       |
 *----------------------------------------------------------------------*)
function TTCPGetter.Connected: boolean;
begin
  result := fThread.Client.Connected
end;

constructor TTCPGetter.Create(cls: TTCPThreadClass; ASettings : TServerSettings);
begin
  fThread := cls.Create(Self, ASettings);
end;

procedure TTCPGetter.DeleteRequest(idx: Integer);
begin
// stub
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.Destroy                                                  |
 |                                                                      |
 | Destructor for TTCPGetter.  Destroy the worker thread.              |
 *----------------------------------------------------------------------*)
destructor TTCPGetter.Destroy;
begin
  fThread.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.Disconnect                                               |
 |                                                                      |
 | Disconnect the TCP connection in the worker thread.  If it's busy    |
 | this will probably cause an exception, which is handled by the       |
 | worker thread.                                                       |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.Disconnect;
begin
  fTerminating := True;
  fThread.Client.Disconnect;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetOutstandingRequestCount                               |
 |                                                                      |
 | Stub get method for the OutstandingRequestCount property.  This is   |
 | overridden by descendant classes.                                    |
 |                                                                      |
 | The function returns the Integer OutstandingRequestCount             |
 *----------------------------------------------------------------------*)
function TTCPGetter.GetGetterRootText: WideString;
begin
  Result := ''; // stub
end;

function TTCPGetter.GetGroup(idx: Integer): TServerAccount;
begin
  result := Nil;  // Stub
end;

function TTCPGetter.GetIsDoing(obj : TObject): boolean;
begin
  result := False
end;

function TTCPGetter.GetOutstandingRequestCount: Integer;
begin
  Result := 0;
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetOutstandingRequestText                                |
 |                                                                      |
 | Stub get method for the OutstandingRequestText property.  This is    |
 | overridden by descendant classes.                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer       The idx of the request to get the text for.     |
 |                                                                      |
 | The function returns the string OutstandingRequestText               |
 *----------------------------------------------------------------------*)
function TTCPGetter.GetOutstandingRequestText(idx: Integer): WideString;
begin
  Result := '' // Stub
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetState                                                 |
 |                                                                      |
 | Return the worker thread state - dormant, pending, busy, etc.        |
 |                                                                      |
 |                                                                      |
 | The function returns the TNNTPThreadState                            |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.GetProgressNumbers(group : TServerAccount; var min, max, pos: Integer);
begin
  fThread.GetProgressNumbers (group, min, max, pos);
end;

function TTCPGetter.GetSettings: TServerSettings;
begin
  result := fThread.fSettings
end;

function TTCPGetter.GetState: TNNTPThreadState;
begin
  Result := fThread.State
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.GetStatusBarMessage                                      |
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
function TTCPGetter.GetStatusBarMessage(group: TServerAccount): WideString;
begin
  case State of
    tsDormant : result := rstOK;
    tsPending : if fThread.fLastError <> '' then
                  result := fThread.fLastError
                else
                  result := rstQueued;
    tsBusy    : if ThreadManager.CurrentConnectoid = '~' then
                  result := Format (rstDialling, [Settings.RASConnection])
                else
                  if fThread.Client.Connected then
                    result := Format (rstConnectedTo, [Settings.ServerName])
                  else
                    result := Format (rstConnectingTo, [Settings.ServerName]);
    tsDone    : result := rstDone
  end
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.NotifyUI                                                 |
 |                                                                      |
 | Called after work has finished in the context of the main thread.    |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.NotifyUI;
begin
 // stub
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.Resume                                                   |
 |                                                                      |
 | Trigger the worker thread to start working.                          |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.Resume;
begin
  if not Paused then
    fThread.Trigger.SetEvent
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.SetState                                                 |
 |                                                                      |
 | Set the worker thread's state.                                       |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.SetPaused(const Value: boolean);
begin
  if value <> fPaused then
  begin
    fPaused := Value;
    if not fPaused then
      Resume
    else
    repeat
      try
        Disconnect
      except
      end;

      if State = tsBusy then
      begin
        Sleep (500);
        CheckSynchronize
      end
    until State <> tsBusy
  end
end;

procedure TTCPGetter.SetState(const Value: TNNTPThreadState);
begin
  fThread.State := Value
end;

(*----------------------------------------------------------------------*
 | TTCPGetter.WorkDone                                                 |
 |                                                                      |
 | Called when work has finished - before NotifyUI.  Not thread safe.   |
 |                                                                      |
 | Override this to save data.                                          |
 *----------------------------------------------------------------------*)
procedure TTCPGetter.WorkDone;
begin
  if Options.AutoDisconnectOnIdle then
    ThreadManager.DoAutoDisconnect
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
  inherited Create (TNNTPNewsgroupThread, ANNTPAccount);
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

  inherited;
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.GetOutstandingRequestText                           |
 |                                                                      |
 | A Newsgroup Getter can only contain one outstanding request.  Return |
 | the name of the account for which the list is being retrieved.       |
 *----------------------------------------------------------------------*)
function TNewsgroupGetter.GetGetterRootText: WideString;
begin
  result := rstGetNewsgroupsFor + Account.AccountName
end;

function TNewsgroupGetter.GetOutstandingRequestText(idx: Integer): WideString;
begin
  try
    result := rstGetNewsgroupsFor + Account.AccountName
  except
  end
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.GetStatusBarMessage                                 |
 |                                                                      |
 | Return a status bar message showing the account that's being listed. |
 *----------------------------------------------------------------------*)
function TNewsgroupGetter.GetStatusBarMessage(
  group: TServerAccount): WideString;
begin
  if fThread.Client.Connected then
    Result := rstGettingNewsgroupsFor + Account.AccountName
  else
    Result := inherited GetStatusBarMessage (group);
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.NotifyUI                                            |
 |                                                                      |
 | Call the ThreadManager's OnNewsgroupChanged event once the newsgroup |
 | list has been retrieved.                                             |
 *----------------------------------------------------------------------*)
procedure TNewsgroupGetter.NotifyUI;
begin
  if Assigned (ThreadManager.OnNewsgroupsChanged) then
    ThreadManager.OnNewsgroupsChanged (ThreadManager, Account);
end;

(*----------------------------------------------------------------------*
 | TNewsgroupGetter.WorkDone                                            |
 |                                                                      |
 | As soon as work is done, save the newsgroup list for the account.    |
 *----------------------------------------------------------------------*)
procedure TNewsgroupGetter.WorkDone;
var
  fName : string;
begin
  inherited;
  fName := gMessageBaseRoot + '\' + FixFileNameString (Account.AccountName);
  CreateDir (fName);
  fNewsgroups.SaveToFile (fName + '\Newsgroups.dat')
end;

{ TArticlesGetter }

(*----------------------------------------------------------------------*
 | TArticlesGetter.AddGroupToList                                       |
 |                                                                      |
 | Called by the ThreadManager to add an addional group to a servers    |
 | Articles getter.                                                     |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.AddGroupToList(group: TSubscribedGroup;
  fromArticle, articleCount: Integer; full: boolean; ABatchRef : Integer; ASince : TDateTime);
begin
  LockList;
  try
    fRequests.Add (TArticlesGetterRequest.Create (group, fromArticle, articleCount, full, ABatchRef, ASince))
  finally
    UnlockList
  end
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.ClearWork                                            |
 |                                                                      |
 | Called before and after work is done.  Clear the list of articles    |
 | and temporary string lists                                           |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.Clear;
begin
  if state <> tsBusy then
  begin
    LockList;
    try
      fRequests.Clear;
    finally
      UnlockList
    end;
    State := tsDormant;
    fPaused := False
  end
end;

procedure TArticlesGetter.ClearWork;
begin
  fArticles.Clear;
  fHeader.Clear;
  fBody.Clear
end;

(*----------------------------------------------------------------------*
 | constructor TArticlesGetter.Create                                   |
 |                                                                      |
 | Create the TArticlesGetter                                           |
 *----------------------------------------------------------------------*)
constructor TArticlesGetter.Create(ANNTPAccount: TNNTPAccount);
begin
  inherited Create (TNNTPArticlesThread, ANNTPAccount);
  fHeader := TStringList.Create;        // Temporary string list for getting headers
  fBody := TMemoryStream.Create;        // Temporary string list for getting bodies
  fArticles := TStringList.Create;      // Temporary string list containg article XOVER headers.
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.Destroy                                              |
 |                                                                      |
 | Destructor for TArticlesGetter                                       |
 *----------------------------------------------------------------------*)
procedure TArticlesGetter.DeleteRequest(idx: Integer);
begin
  if (State <> tsBusy) or (idx > 0) then
  begin
    LockList;
    try
      if idx < Count then
        fRequests.Delete(idx);
    finally
      UnlockList
    end
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False
  end
end;

destructor TArticlesGetter.Destroy;
begin
  fHeader.Free;
  fBody.Free;
  fArticles.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.GetOutstandingRequestCount                           |
 |                                                                      |
 | Get number of outstanding requests for this server's articles getter |
 *----------------------------------------------------------------------*)
function TArticlesGetter.GetGetterRootText: WideString;
begin
  result := rstGetArticleListFor + account.AccountName
end;

function TArticlesGetter.GetGroup(idx: Integer): TServerAccount;
begin
  try
    LockList;
    try
      if idx < fRequests.Count then
        result := TArticlesGetterRequest (fRequests [idx]).Group
      else
        result := Nil
    finally
      UnlockList
    end
  except
    result := Nil
  end
end;

function TArticlesGetter.GetIsDoing(obj: TObject): boolean;
var
  i : Integer;
begin
  result := inherited GetIsDoing (obj);

  if not result then
  begin
    i := 0;
    LockList;
    try
      while (not result) and (i < fRequests.Count) do
        if obj = TArticlesGetterRequest (fRequests [i]).fGroup then
          result := True
        else
          Inc (i)
    finally
      UnlockList
    end
  end
end;

function TArticlesGetter.GetOutstandingRequestCount: Integer;
begin
  Result := Count
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.GetOutstandingRequestText                            |
 |                                                                      |
 | Get the text for outstanding request no. idx.                        |
 *----------------------------------------------------------------------*)
function TArticlesGetter.GetOutstandingRequestText(idx: Integer): WideString;
var
  getterRequest : TArticlesGetterRequest;
begin
  LockList;
  try
    if idx < Count then
    begin
      getterRequest := TArticlesGetterRequest (fRequests [idx]);
      Result := getterRequest.fGroup.Name
    end
  finally
    UnlockList
  end
end;

(*----------------------------------------------------------------------*
 | TArticlesGetter.GetStatusBarMessage                                  |
 |                                                                      |
 | Get status bar message                                               |
 *----------------------------------------------------------------------*)
function TArticlesGetter.GetStatusBarMessage(
  group: TServerAccount): WideString;
var
  req : TArticlesGetterRequest;
  gn : string;
  gp : TArticleContainer;
begin
  result := inherited GetStatusBarMessage (group);
  gp := group as TArticleContainer;

  if fThread.Client.Connected then
  begin
    LockList;
    try
      if fRequests.Count > 0 then
      begin
        req := TArticlesGetterRequest (fRequests [0]);
        gn := req.fGroup.Name
      end
    finally
      UnlockList
    end;


    if group = nil then         // eg. the server is selected, not a group
    begin
      if gn <> '' then
        result := Format (rstGettingArticles, [gn])
    end
    else
    case State of
      tsPending : if ThreadManager.GettingArticleList (gp) then
                    result := format (rstQueuedPending, [gn])
                  else
                    result := '';
      tsBusy : if CurrentFull and Assigned (CurrentGroup) and (CurrentGroup = group) then
                 if CurrentArticleNo = -1 then
                   result := rstAboutToGetArticle
                 else
                   result := Format (rstGettingFullArticle, [CurrentArticleNo, CurrentMax - CurrentArticleNo + 1])
               else
                 if gn = group.Name then
                   result := Format (rstGettingArticles, [gn])
                 else
                   if ThreadManager.GettingArticleList(gp) then
                     result := Format (rstQueuedBusy, [gn])
                   else
                     result := ''
    end
  end

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
  LogMessage (CurrentGroup.Name + ' - Saved ' + IntToStr (CurrentArticleNo));
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

  CurrentGroup.SaveArticles (False);

  if Assigned (ThreadManager.OnArticlesChanged) then
    ThreadManager.OnArticlesChanged (ThreadManager, CurrentGroup);
  LogMessage (CurrentGroup.Name + ' - Updated');
end;

procedure TArticlesGetter.UpdateHeaders;
begin
  if CurrentUpdateAll then    // Zap old articles if required.
  begin
    if ThreadManager.StopArticleDownloads(CurrentGroup) then;
(*      ClearSynchronizedMethods *) ;  // Don't think calling this within a sync handler is very safe (!)
    CurrentGroup.PurgeArticles(True, True, '')
  end;

  CurrentGroup.AddRawHeaders (fArticles, XOverFMT);
  CurrentGroup.SaveArticles (False);

  if Assigned (ThreadManager.OnArticlesChanged) then
    ThreadManager.OnArticlesChanged (ThreadManager, CurrentGroup);
  LogMessage (CurrentGroup.Name + ' - Updated');
end;

{ TArticleGetter }

(*----------------------------------------------------------------------*
 | TArticleGetter.AddArticleToList                                      |
 |                                                                      |
 | Add article to list of outstanding requests.                         |
 *----------------------------------------------------------------------*)

procedure TArticleGetter.AddArticleToList(group: TSubscribedGroup;
  article: TArticle; needsFullRefresh : boolean);
var
  request : TArticleGetterRequest;
  i : Integer;
  dup : boolean;
begin
  LockList;
  try
    dup := False;
    for i := 0 to fRequests.Count - 1 do
      if TArticleGetterRequest (fRequests [i]).fArticle = article then
      begin
        dup := True;
        break
      end;

    if not dup then
    begin
      request := TArticleGetterRequest.Create (group, article, needsFullRefresh);
      fRequests.Add (request)
    end
  finally
    UnlockList
  end
end;

procedure TArticleGetter.Clear;
begin
  if state <> tsBusy then
  begin
    LockList;
    try
      fRequests.Clear;
    finally
      UnlockList
    end;

    state := tsDormant;
    fPaused := False
  end
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
  if Assigned (ThreadManager.OnClearArticle) then
    ThreadManager.OnClearArticle (ThreadManager, CurrentArticle);

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
  inherited Create (TNNTPArticleThread, ANNTPAccount);
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
        Dec (PipelinePos)
      end
    finally
      UnlockList
    end
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False
  end
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
  CurrentArticle.Msg.EndUpdate;
  if Assigned (ThreadManager.OnArticleFailed) then
    ThreadManager.OnArticleFailed (ThreadManager, CurrentArticle);
  CurrentArticle.RemoveMessage;
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GetOutstandingRequestCount                            |
 |                                                                      |
 | Return the number of message bodies lined up for retrieval           |
 *----------------------------------------------------------------------*)
function TArticleGetter.GetGetterRootText: WideString;
begin
  result := rstGetArticleBodyFrom + account.AccountName
end;

function TArticleGetter.GetGroup(idx: Integer): TServerAccount;
begin
  try
    LockList;
    if idx < fRequests.Count then
      result := TArticleGetterRequest (fRequests [idx]).Group
    else
      result := Nil
  finally
    UnlockList
  end
end;

function TArticleGetter.GetIsDoing(obj: TObject): boolean;
var
  i : Integer;
begin
  result := inherited GetIsDoing (obj);

  if not result then
  begin
    i := 0;
    LockList;
    try
      while (not result) and (i < fRequests.Count) do
        if (TArticleGetterRequest (fRequests [i]).fGroup = obj) or
           (TArticleGetterRequest (fRequests [i]).fArticle = obj) then
          result := True
        else
          Inc (i)
    finally
      UnlockList
    end
  end
end;

function TArticleGetter.GetOutstandingRequestCount: Integer;
begin
  Result := Count
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GetOutstandingRequestText                             |
 |                                                                      |
 | Get the text for a message lined up for retrieval.                   |
 *----------------------------------------------------------------------*)
function TArticleGetter.GetOutstandingRequestText(idx: Integer): WideString;
var
  getterRequest : TArticleGetterRequest;
  requests : TObjectList;
begin
  requests := LockList;
  try
    if idx < requests.Count then
    begin
      getterRequest := TArticleGetterRequest (requests [idx]);
      Result := IntToStr (getterRequest.fArticle.ArticleNo) + ' ' + rstFrom + ' ' + getterRequest.fGroup.Name
    end
  finally
    UnlockList
  end
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GetStatusBarMessage                                   |
 |                                                                      |
 | Return the status-bar message text.                                  |
 *----------------------------------------------------------------------*)
function TArticleGetter.GetStatusBarMessage(
  group: TServerAccount): WideString;
var
  req : TArticleGetterRequest;
  gn : string;
  an : TArticle;
  gp : TArticleContainer;
begin
  result := inherited GetStatusBarMessage (group);
  gp := group as TArticleContainer;

  if fThread.Client.Connected then
  begin
    an := Nil;

    if State = tsPending then
    begin
      LockList;
      try
        if Count > 0 then
        begin
          req := TArticleGetterRequest (fRequests [0]);
          gn := req.fGroup.Name;
          an := req.fArticle
        end
      finally
        UnlockList
      end
    end;

    fThread.UILock;
    try
      if group = nil then
      case State of
        tsPending : result := 'Queued.  ' + gn + ' is next';
        tsBusy : if Assigned (CurrentArticle) then
                   result := 'Getting article ' + IntToStr (CurrentArticle.ArticleNo) + ' for ' + CurrentGroup.Name
      end
      else
      case State of
        tsPending : if ThreadManager.GettingArticle (gp, nil) then
                      if Assigned (an) then
                        result := 'Group ' + group.Name + ' Article ' + IntToStr (an.ArticleNo) + ' queued'
                      else
                        result := 'Group ' + group.Name + ' queued';


        tsBusy : if Assigned (CurrentArticle) then
                   if CurrentGroup = group then
                     result := 'Getting article ' + IntToStr (CurrentArticle.ArticleNo) + ' from group ' + CurrentGroup.Name
                   else
                     result := 'Queued'
                   else
                     result := ''
      end
    finally
      fThread.UIUnlock
    end
  end
end;

(*----------------------------------------------------------------------*
 | TArticleGetter.GotArticle                                            |
 |                                                                      |
 | Called in the context of the main thread when a message has been     |
 | successfully downloaded.  Save the message, and update the UI.       |
 *----------------------------------------------------------------------*)
procedure TArticleGetter.GotArticle;
begin
  CurrentArticle.Msg.EndUpdate;
  CurrentArticle.Initialize(CurrentArticle.ArticleNo,CurrentArticle.Msg.Header);
  CurrentArticle.SaveMessageBody;
  if Assigned (ThreadManager.OnArticleChanged) then
    ThreadManager.OnArticleChanged (ThreadManager, CurrentArticle);
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
  if Assigned (ThreadManager.OnStartArticle) then
   ThreadManager.OnStartArticle (ThreadManager, CurrentArticle)
end;

{ TArticleGetterRequest }

constructor TArticleGetterRequest.Create(group: TSubscribedGroup;
  article: TArticle; needsFullRefresh : boolean);
begin
  fGroup := group;
  fArticle := article;
  fNeedsFullRefresh := needsFullRefresh
end;

{ TArticlesGetterRequest }

constructor TArticlesGetterRequest.Create(AGroup: TSubscribedGroup;
  AFromArticle, AArticleCount: Integer; full: boolean; ABatchRef : Integer; ASince : TDateTime);
begin
  fGroup := AGroup;
  fFromArticle := AFromArticle;
  fArticleCount := AArticleCount;
  fFull := full;
  fBatchRef := ABatchRef;
  fSince := ASince;
end;

{ TPoster }

procedure TPoster.AddPostToList(hdr : TStrings; const msg : string; attachments : TObjectList; ACodepage : Integer; ATextPartStyle : TTextPartStyle);
begin
  LockList;
  try
    fRequests.Add (TPosterRequest.Create (self, hdr, msg, attachments, ACodepage, ATextPartStyle))
  finally
    Unlocklist
  end
end;

procedure TPoster.Clear;
begin
  if state <> tsBusy then
  begin
    LockList;
    try
      fRequests.Clear
    finally
      UnlockList
    end;
    state := tsDormant;
    fPaused := False
  end
end;

constructor TPoster.Create(AAccount : TNNTPAccount);
begin
  inherited Create (TNNTPPoster, AAccount);
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
      UnlockList
    end
  end;
  if Count = 0 then
  begin
    State := tsDormant;
    fPaused := False
  end
end;


{ TPost }

procedure TPosterRequest.AddAttachments(msg: TStrings;
  const multipartBoundary: string);
var
  i, chunkLen : Integer;
  attachment : TAttachment;
  f : TFileStream;
  encoder : TidEncoder;
  s, contentType : string;

  function GetMIMEContentType (const fileName : string) : string;
  var
    ext : string;
  begin
    ext := UpperCase (ExtractFileExt (fileName));

    if ext = '.JPG' then
      result := 'Image/JPEG'
    else
      if ext = '.GIF' then
        result := 'Image/GIF'
      else
        if ext = '.TIF' then
          result := 'Image/TIFF'
        else
          result := 'Application/Octet-Stream'
  end;

  function FixFileName (const fileName : string) : string;
  var
    i : Integer;
    needsQuotes, cantQuote : boolean;
  begin
    // Tidy up filename.
    //
    // 1.  If it contains whitespace then 'quote' the string
    //     if possible
    //
    // 2.  Convert illegal characters to '_'
    //
    // 3.  If the string already contains '"', then don't quote it
    result := fileName;

    needsQuotes := False;
    cantQuote := False;
    for i := 1 to Length (result) do
      case result [i] of

        #0..#8, #11..#12, #14..#31 :  // Non Ascii
          result [i] := '_';

                                      // Whitespace
        #9, #10, #13, ' ' : needsQuotes := True;
        '"' : cantQuote := true;

                                      // Illegal characters
        ':', '\', '/', '>', '<' :
          result [i] := '_';
      end;

    if needsQuotes and not cantQuote then
      result := '"' + result + '"';
  end;

begin
  if not Assigned (Attachments) then
    Exit;
  for i := 0 to Attachments.Count - 1 do
  begin
    attachment := TAttachment (attachments [i]);

    if FileExists (attachment.Pathname) then
    begin
      encoder := Nil;
      f := TFileStream.Create (attachment.PathName, fmOpenRead or fmShareDenyNone);
      try
        if multipartBoundary <> '' then
        begin
          encoder := TidEncoderMIME.Create(nil);
          msg.Add ('--' + multipartBoundary);
          contentType := GetMIMEContentType (attachment.FileName);
          msg.Add ('Content-Type: ' + contentType + '; name=' + FixFilename (attachment.FileName));
          msg.Add ('Content-Transfer-Encoding: base64');
          msg.Add ('');
          chunkLen := 57;
        end
        else
        begin
          msg.Add ('');
          msg.Add ('begin 444 ' + FixFileName (attachment.FileName));
          encoder := TidEncoderUUE.Create(nil);
          chunkLen := 45;
        end;

        while f.Position < f.Size do
        begin
          s := encoder.Encode(f, chunkLen);
          msg.Add (s)
        end;

        if multipartBoundary <> '' then
          msg.Add ('') // do we need this??
        else
          msg.Add('end')
      finally
        f.Free;
        encoder.Free;
      end
    end
  end;

  if multipartBoundary <> '' then
    msg.Add ('--' + multipartBoundary + '--');
end;

constructor TPosterRequest.Create(AOwner : TPoster; AHdr : TStrings; const AMsg: string; attachments : TObjectList; ACodepage : Integer; ATextPartStyle : TTextPartStyle);
begin
  fOwner := AOwner;
  fTextPartStyle := ATextPartStyle;
  fHdr := TStringList.Create;
  fHdr.Assign(AHdr);
  fMsg := AMsg;
  fAttachments := attachments;
  fCodepage := ACodepage;
end;

procedure TPosterRequest.CreateEncodedHeader(var hdr : TStrings;
  var hdrCreated : boolean; var multipartBoundary : string);
var
  s, s1 : string;
  mimeCharsetName : string;
  i : Integer;
  bt8 : boolean;

  function GenerateMultipartBoundary : string;
  begin
    result := 'Xananews.1.2.3';
  end;

begin
  multipartBoundary := '';
  hdrCreated := MimeHeaderRequired (self.hdr);

  if hdrCreated then
    hdr := TStringList.Create
  else
    hdr := self.Hdr;

  try
    if hdrCreated then
    begin
      mimeCharsetName := CodepageToMIMECharsetName (fCodepage);
      if mimeCharsetName <> '' then
        for i := 0 to self.hdr.Count - 1 do
        begin
          s := self.Hdr [i];
          if Pos ('X-Face', s) = 1 then
            hdr.Add(s)
          else
          begin
            s1 := SplitString ('=', s);
            if Pos ('Newsgroups', s1) = 1 then
              hdr.Add (s1 + '=' + s)
            else
              hdr.Add (s1 + '=' + EncodeHeader (s, CodePage, Pos ('From', s1) = 1));

          end
        end
      else
        hdr.Assign(self.Hdr);
      hdr.Add('MIME-Version=1.0');
      if not Assigned (attachments) or (attachments.Count = 0) then
      begin
        s := 'Content-Type=text/plain';

        if mimeCharsetName <> '' then
          s := s + '; charset=' + mimeCharsetName;

        if fTextPartStyle = tpFlowed then
          s := s + '; format=flowed';

        hdr.Add (s);

        if fTextPartStyle = tpQuotedPrintable then
          hdr.Add ('Content-Transfer-Encoding=quoted-printable')
        else
        begin
          bt8 := False;
          for i := 1 to Length (msg) do
            if not (msg [i] in [#9, #10, #13, ' '..'~']) then
            begin
              bt8 := True;
              break
            end;

          if bt8 then
            hdr.Add ('Content-Transfer-Encoding=8bit')
        end
      end
      else
      begin
        multipartBoundary := GenerateMultipartBoundary;
        hdr.Add('Content-Type=Multipart/Mixed; boundary=' + multipartBoundary)
      end;

    end

  except
    if hdrCreated then
      hdr.Free;
    raise
  end
end;

procedure TPosterRequest.CreateEncodedMessage(var msg: TStrings; const multipartBoundary: string);

  procedure AddMessageString (msg : TStrings; const st : string);
  var
    m : TStrings;
    coder : TRFC2646Encoder;
  begin
    m := TStringList.Create;
    try
      m.Text := st;
      if fTextPartStyle = tpQuotedPrintable then
        WrapStrings (m, 75, fTextPartStyle, false, false)
      else
        if fTextPartStyle = tpFlowed then
        begin
          coder := TRFC2646Encoder.Create(nil);
          try
            coder.EncodeStrings (m)
          finally
            coder.Free
          end
        end;
      msg.AddStrings(m);
    finally
      m.Free
    end
  end;

begin
  msg := TStringList.Create;

  try
    if multipartBoundary <> '' then
    begin
      msg.Add('This is a multipart message in MIME format');
      msg.Add ('');  // Do we need this ??
      msg.Add ('--' + multipartBoundary);
      if fTextPartStyle = tpQuotedPrintable then
        msg.Add ('Content-Transfer-Encoding: quoted-printable');
      msg.Add ('');

      AddMessageString (msg, self.Msg);

      msg.Add ('');
      if Attachments.Count = 0 then
        msg.Add ('--' + multipartBoundary + '--')
    end
    else
      AddMessageString (msg, self.Msg);

(* Done in IdNNTPX
    for i := 0 to msg.Count - 1 do
      if msg [i] = '.' then
        msg [i] := '..';
*)
  except
    msg.Free;
    raise
  end
end;

destructor TPosterRequest.Destroy;
begin
  fHdr.Free;
  fAttachments.Free;

  inherited;
end;

function TPoster.GetGetterRootText: WideString;
begin
  result := 'Post message(s) to ' + Settings.ServerName;
end;

function TPoster.GetOutstandingRequestCount: Integer;
begin
  Result := Count
end;

function TPoster.GetOutstandingRequestText(idx: Integer): WideString;
var
  post : TPosterRequest;
begin
  try
    LockList;
    if idx < fRequests.Count then
    begin
      post := TPosterRequest (fRequests [idx]);
      Result := post.Groups + ':';
      Result := Result + StringToWideString (post.Subject, post.Codepage)
    end
  finally
    UnlockList
  end
end;

function TPoster.GetStatusBarMessage(group: TServerAccount): WideString;
var
  req : TPosterRequest;
  s : WideString;
begin
  result := inherited GetStatusBarMessage (group);

  if fThread.Client.Connected then
  begin
    s := '';
    LockList;
    try
      if Count > 0 then
      begin
        req := TPosterRequest (fRequests [0]);
        s := req.Groups + ':';
        s := s + StringToWideString (req.Subject, req.Codepage)
      end
    finally
      UnlockList
    end;

    if s <> '' then
    case State of
      tsPending : result := 'Queued ' + s;
      tsBusy : result := 'Posting ' + s;
    end
  end
end;

procedure TPosterRequest.GetGroupAndSubject;
var
  i : Integer;
  s, s1 : string;
begin
  if (fGroups = '') or (fSubject = '') then
  begin
    for i := 0 to fHdr.Count - 1 do
    begin
      s := fHdr [i];
      s1 := SplitString ('=', s);
      if CompareText (s1, 'Newsgroups') = 0 then
        fGroups := Trim (s);

      if CompareText (s1, 'Subject') = 0 then
        fSubject := Trim (s);

      if (fSubject <> '') and (fGroups <> '') then
        break
    end
  end
end;

function TPosterRequest.GetGroups: string;
begin
  GetGroupAndSubject;
  result := fGroups;
end;

function TPosterRequest.GetSubject: string;
begin
  GetGroupAndSubject;
  result := fSubject;
end;

function TPosterRequest.MIMEHeaderRequired (Ahdr : TStrings): boolean;
var
  i : Integer;
begin
  result := (fCodepage <> CP_USASCII) or (fTextPartStyle <> tpNNTP);
  if not result and Assigned (attachments) then
  begin
    for i := 0 to Attachments.Count - 1 do
      if not TAttachment (Attachments [i]).fInline then
      begin
        result := True;
        break
      end;

(*
    if not result then
      for i := 1 to Length (msg) do
        if not (msg [i] in [' '..'~']) then
        begin
          result := True;
          break
        end
*)
  end
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
    inherited;
end;

procedure TPoster.ResumeOutbasket;
begin
  fUseOutbasket := False;
  Resume
end;

procedure TPoster.WorkDone;
begin
  inherited;
  fUseOutbasket := Account.PostingSettings.DelayPosting;
end;

{ TAttachment }

constructor TAttachment.Create(APathName: string);
var
  f : TSearchRec;
begin
  if FindFirst (APathName, -1, f) = 0 then
  begin
    fDirectory := ExtractFilePath (APathName);
    fFilename := f.Name;
    fSize := f.Size;
    fDate := f.Time;
    fInline := True;
    FindClose (f)
  end
  else
    raise Exception.Create ('File ' + APathName + ' not found');
end;

function TAttachment.GetPathName: string;
begin
  result := fDirectory + fFileName;
end;

{ TEMailer }

procedure TEMailer.AddMessageToList(AArticleContainer : TServerAccount; const sTo, sCC, sBCC, sSubject, sReplyTo, msg : string;
  attachments: TObjectList; ACodePage : Integer);
begin
  fMessages.Add (TEMailerRequest.Create (AArticleContainer, self, sTo, sCC, sBCC, sSubject, sReplyTo, msg, attachments, ACodePage))
end;

constructor TEMailer.Create(settings : TSMTPServerSettings; AUseOutbasket : boolean);
begin
  inherited Create (TSMTPMailer, settings);
  fMessages := TObjectList.Create;
  fOrigUseOutbasket := AUseOutbasket;
  fUseOutbasket := AUseOutbasket
end;

destructor TEMailer.Destroy;
begin
  fMessages.Free;

  inherited;
end;

function TEMailer.GetGetterRootText: WideString;
begin
  result := 'Mail message(s) to ' + Settings.ServerName;
end;

function TEMailer.GetGroup(idx: Integer): TServerAccount;
begin
  result := TEMailerRequest (Messages [idx]).fArticleContainer
end;

function TEMailer.GetOutstandingRequestCount: Integer;
begin
  result := fMessages.Count
end;

function TEMailer.GetOutstandingRequestText(idx: Integer): WideString;
var
  msg : TEMailerRequest;
begin
  try
    msg := TEMailerRequest (fMessages [idx]);
    Result := msg.MTo + ':' + msg.MSubject
  except
  end
end;

function TEMailer.GetStatusBarMessage(group: TServerAccount): WideString;
var
  req : TEMailerRequest;
  s : string;
begin
  result := inherited GetStatusBarMessage (group);

  if fThread.Client.Connected then
  begin
    s := '';
    if fMessages.Count > 0 then
    begin
      req := TEMailerRequest (fMessages[0]);
      s := req.MTo + ':' + req.MSubject
    end;

    if s <> '' then
    case State of
      tsPending : result := 'Queued ' + s;
      tsBusy : result := 'Posting ' + s;
    end
  end
end;

procedure TEMailer.Resume;
begin
  if not fUseOutbasket then
    inherited;
end;

procedure TEMailer.ResumeOutbasket;
begin
  fUseOutbasket := False;
  Resume
end;

(*
procedure TEMailer.SaveOutgoingPost;
var
  request : TEmailerRequest;
  mailAccount :TMailAccount;
  hdrs : TIdHeaderList;
  body : TStringStream;
begin
  request := TEMailerRequest (Messages [0]);

  hdrs := Nil;
  body := Nil;
  try
    mailAccount := Nil;
    if request.ArticleContainer is TMailAccount then
      mailAccount := TMailAccount (request.ArticleContainer)
    else
      if request.ArticleContainer is TSubscribedGroup then
        mailAccount := MailAccounts.FindMailAccount(TSubscribedGroup (request.ArticleContainer).Owner.MailAccountName);

    if Assigned (mailAccount) then
    begin
      hdrs := CurrentMessage.GenerateHeader;
      body := TStringStream.Create(CurrentMessage.Body.Text);

        FixHeaders (hdrs);

      mailAccount.AddMessage(hdrs, body)
    end
  finally
    hdrs.Free;
    Body.Free
  end
end;
*)
procedure TEMailer.WorkDone;
begin
  inherited;
  fUseOutbasket := fOrigUseOutbasket;
end;

{ TEMailerRequest }

constructor TEMailerRequest.Create(AArticleContainer : TServerAccount; AOwner: TEmailer; const ATo, ACC, ABCC, ASubject, AReplyTo, Amsg : string; attachments: TObjectList; ACodePage : Integer);
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

  inherited;
end;

{ TNewsGetter }

constructor TNewsGetter.Create(cls : TTCPThreadClass; ANNTPAccount: TNNTPAccount);
begin
  inherited Create (cls, ANNTPAccount.NNTPServerSettings);
  Account := ANNTPAccount;
end;

function TNewsGetter.GetAccount: TNNTPAccount;
begin
  if fThread is TNNTPThread then
    result := TNNTPThread (fThread).NNTPAccount
  else
    result := Nil
end;

function TNewsGetter.GetIsDoing(obj: TObject): boolean;
begin
  result := obj = Account
end;

procedure TNewsGetter.Resume;
var
  ok : boolean;
begin
  ok := (Account.NNTPServerSettings.MaxConnections = 0) or
     (ThreadManager.CountActiveGettersForAccount(Account) < Account.NNTPServerSettings.MaxConnections);

  if not ok then
  begin
    threadManager.ClearDormantConnections (Account);

    ok := ThreadManager.CountActiveGettersForAccount(Account) < Account.NNTPServerSettings.MaxConnections
  end;

  if ok then
    inherited;
end;

procedure TNewsGetter.SetAccount(const Value: TNNTPAccount);
begin
  if fThread is TNNTPThread then
    TNNTPThread (fThread).NNTPAccount := Value
end;

{ TMultiNewsGetter }

constructor TMultiNewsGetter.Create(cls: TTCPThreadClass;
  ANNTPAccount: TNNTPAccount);
begin
  inherited Create (cls, ANNTPAccount);
  fRequests := TObjectList.Create;
  fSync := TCriticalSection.Create;

end;

destructor TMultiNewsGetter.Destroy;
begin
  fRequests.Free;
  fSync.Free;

  inherited;
end;

function TMultiNewsGetter.GetCount: Integer;
begin
  result := fRequests.Count
end;

function TMultiNewsGetter.LockList: TObjectList;
begin
  if fLocked and (fLockThread = Integer (GetCurrentThreadID)) then
    raise Exception.Create('MultiNewsGetter already locked in this thread');
  fSync.Enter;
  fLocked := True;
  fLockThread := GetCurrentThreadID;
  result := fRequests
end;

procedure TMultiNewsGetter.UnlockList;
begin
  fLocked := False;
  fLockThread := 0;
  fSync.Leave;
end;

procedure TTCPThread.UIUnlock;
begin
  fUISync.Leave
end;

procedure TArticleGetter.Update;
begin
  CurrentGroup.ReSortArticles;
  CurrentGroup.SaveArticles (False);
  if Assigned (ThreadManager.OnArticlesChanged) then
    ThreadManager.OnArticlesChanged (ThreadManager, CurrentGroup);
end;

end.
