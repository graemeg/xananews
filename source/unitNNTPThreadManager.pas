unit unitNNTPThreadManager;

interface

uses
  Windows, Classes, Graphics, SysUtils, ConTnrs, SyncObjs,
  unitCharsetMap, unitNNTPServices, unitMailServices, unitNewsThread, unitSettings,
  NewsGlobals, XnClasses, XnRawByteStrings;

type
  TThreadManagerState = (tmDormant, tmPending, tmBusy);

  TOnArticlesNotify = procedure(Sender: TObject; group: TArticleContainer) of object;
  TOnArticleNotify = procedure(Sender: TObject; article: TArticleBase) of object;
  TOnAccountNotify = procedure(Sender: TObject; account: TNNTPAccount) of object;
  TOnNotifyError = procedure(Sender: TObject; const error: string) of object;
  TOnNotifyNewGroups = procedure(Sender: TObject; account: TNNTPAccount) of object;

  TNNTPThreadManager = class
  private
    fNewsAgent: string;
    fGetterList: TObjectList;
    fCurrentConnectoid: string;
    fConnection: Integer;
    fWeConnected: Boolean;
    fOnArticlesChanged: TOnArticlesNotify;
    fOnArticleChanged: TOnArticleNotify;
    fOnClearArticle: TOnArticleNotify;
    fOnArticleFailed: TOnArticleNotify;
    fOnStartArticle: TOnArticleNotify;
    fOnNewsgroupsChanged: TOnAccountNotify;
    fOnNotifyError: TOnNotifyError;
    fAllThreadsPaused: Boolean;
    fOnNotifyNewGroups: TOnNotifyNewGroups;
    fGetterSync: TCriticalSection;
    fConnectToSync: TCriticalSection;
    fLocked: Boolean;

    function LockFindGetter(cls: TTCPGetterClass; settings: TServerSettings): TTCPGetter;
    function GetConnected: Boolean;
    function GetThreadManagerState(obj: TObject): TThreadManagerState;
    function GetStatusBarMessage(settings: TServerSettings; group: TSubscribedGroup): string;
    function GetQueuedRequestCount: Integer;
    function GetQueuedRequestText(idx: Integer): string;
    procedure SetAllThreadsPaused(const Value: Boolean);
    function GetActiveGetter(idx: Integer): TTCPGetter;
    function GetActiveGetterCount: Integer;
    function GetNoOutbasketEntries: Integer;
    function ReadCurrentConnectoid: string;

  public
    constructor Create(const ANewsAgent: string);
    destructor Destroy; override;
    procedure DoAutoDisconnect;
    function ConnectToInternet(settings: TServerSettings): Boolean;
    function CountActiveGettersForAccount(acc: TNNTPAccount): Integer;
    procedure ClearDormantConnections(acc: TNNTPAccount);
    procedure GetNewsgroups(account: TNNTPAccount);
    procedure GetArticles(account: TNNTPAccount; group: TSubscribedGroup; fromArticle, articleCount: Int64; full: Boolean; ABatchRef: Integer; ASince: TDateTime);
    procedure GetParentArticles(article: TArticle);
    procedure GetArticleBody(account: TNNTPAccount; group: TSubscribedGroup; article: TArticle);
    procedure GetArticleThreadBodies(account: TNNTPAccount; group: TSubscribedGroup; article: TArticle);
    procedure JogThreads;
    procedure DisconnectAll(Done: Boolean);
    procedure PostMessage(account: TNNTPAccount; hdr: TAnsiStrings; const msg: RawByteString; attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle); overload;
    procedure PostMessage(account: TNNTPAccount; const hdr, msg: string; attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle); overload;
    procedure SendSMTPMail(articleContainer: TServerAccount; settings: TSMTPServerSettings; const sTo, sCC, sBCC, sSubject, sReplyTo, msg: string; attachments: TObjectList; ACodePage: Integer; AUseOutbasket: Boolean);
    property NewsAgent: string read fNewsAgent;
    procedure GetProgressNumbers(settings: TServerSettings; group: TSubscribedGroup; var min, max, pos: Integer);

    procedure ClearGetters;
    procedure ClearGettersForAccount(account: TNNTPAccount);
  //  property GetterCount: Integer read GetGetterCount;
  //  property Getters [idx: Integer]: TTCPGetter read GetGetters;

    function LockGetterList: TObjectList;
    procedure UnlockGetterList;

    property ActiveGetterCount: Integer read GetActiveGetterCount;
    property ActiveGetters[idx: Integer]: TTCPGetter read GetActiveGetter;

    function GettingNewsgroupList(account: TNNTPAccount): Boolean;
    function GettingArticleList(group: TArticleContainer; Resume: Boolean = False): Boolean;
    function GettingArticle(group: TArticleContainer; article: TArticleBase): Boolean;

    function StopArticleDownloads(group: TSubscribedGroup): Boolean;
    procedure Cancel(settings: TServerSettings; container: TArticleContainer; article: TArticleBase);
    procedure GetCurrentConnectoid;
    procedure ResumeOutbasketEntries;

    property Connected: Boolean read GetConnected;
    property CurrentConnectoid: string read ReadCurrentConnectoid;
    property ThreadManagerState[obj: TObject]: TThreadManagerState read GetThreadManagerState;

    property StatusBarMessage[account: TServerSettings; group: TSubscribedGroup]: string read GetStatusBarMessage;

    property QueuedRequestCount: Integer read GetQueuedRequestCount;
    property QueuedRequestText[idx: Integer]: string read GetQueuedRequestText;

    property AllThreadsPaused: Boolean read fAllThreadsPaused write SetAllThreadsPaused;
    property NoOutbasketEnties: Integer read GetNoOutbasketEntries;

    property OnArticlesChanged: TOnArticlesNotify read fOnArticlesChanged write fOnArticlesChanged;
    property OnArticleChanged: TOnArticleNotify read fOnArticleChanged write fOnArticleChanged;
    property OnClearArticle: TOnArticleNotify read fOnClearArticle write fOnClearArticle;
    property OnArticleFailed: TOnArticleNotify read fOnArticleFailed write fOnArticleFailed;
    property OnStartArticle: TOnArticleNotify read fOnStartArticle write fOnStartArticle;
    property OnNewsgroupsChanged: TOnAccountNotify read fOnNewsgroupsChanged write fOnNewsgroupsChanged;
    property OnNotifyError: TOnNotifyError read fOnNotifyError write fOnNotifyError;
    property OnNotifyNewGroups: TOnNotifyNewGroups read fOnNotifyNewGroups write fOnNotifyNewGroups;
  end;

  TNetworkMonitorThread = class(TXnThread)
  private
    FOverlap: OVERLAPPED;
    FHandle: THandle;
  protected
    procedure Execute; override;
  public
    procedure CancelMonitor;
    constructor Create;
    destructor Destroy; override;
  end;

  ENNTPThreadManager = class(Exception)
  end;

var
  ThreadManager: TNNTPThreadManager;

type
  TfnInternetGetConnectedStateExA = function(var dwFlags: DWORD; lpszConnectionName: PChar; dwNameLen, dwReserved: DWORD): BOOL; stdcall;
  TfnInternetGetConnectedStateExW = function(var dwFlags: DWORD; lpszConnectionName: PWideChar; dwNameLen, dwReserved: DWORD): BOOL; stdcall;
  TNotifyAddrChange = function(Handle: PHandle; overlapped: POVERLAPPED): DWORD; stdcall;

var
  InternetGetConnectedStateEx: TfnInternetGetConnectedStateExA = nil;
  NotifyAddrChange: TNotifyAddrChange = nil;
  gNetworkMonitorThread: TNetworkMonitorThread = nil;

function InternetTimeToSystemTime(lpszTime: PChar; var pst: SYSTEMTIME; dwReserved: DWORD): BOOL; stdcall;

implementation

uses WinINet, unitNewsReaderOptions, unitCheckVersion, unitSearchString, unitSavedArticles, IdWinsock2;

const
  wininetdll = 'wininet.dll';
  iphlpapilib = 'iphlpapi.dll';
var
  hWinINet: THandle;
  hiphlpapi: THandle;

function InternetTimeToSystemTime; external wininetdll name 'InternetTimeToSystemTimeA';

{ TNNTPThreadManager }

procedure TNNTPThreadManager.Cancel(settings: TServerSettings; container: TArticleContainer; article: TArticleBase);
var
  i, j: Integer;
  getter: TTCPGetter;
  deleteIt: Boolean;
  stopIt: Boolean;
  articleGetter: TArticleGetter;
  articlesGetter: TArticlesGetter;
  getterRequest: TArticleGetterRequest;
  articlesGetterRequest: TArticlesGetterRequest;
  requests: TObjectList;
  getters: TObjectList;
begin
  i := 0;
  getters := LockGetterList;
  try
    while i < getters.Count do
    begin
      getter := TTCPGetter(getters[i]);

      deleteIt := False;
      stopIt := False;
      if getter.Settings = settings then
      begin
        if getter is TNewsgroupGetter then
          deleteIt := True
        else
          if getter is TArticlesGetter then
          begin
            articlesGetter := TArticlesGetter(getter);
            requests := articlesGetter.LockList;
            try
              j := 0;
              while j < requests.Count do
              begin
                articlesGetterRequest := TArticlesGetterRequest(requests[j]);

                if (container = nil) or (articlesGetterRequest.Group = container) then
                  if (articlesGetter.CurrentGroup <> nil) and (((container = nil) and (j = 0)) or (articlesGetter.CurrentGroup = container)) then
                  begin
                    StopIt := True;
                    articlesGetterRequest.Abandon := True;
                    Inc(j);
                  end
                  else
                    requests.Delete(j)
                else
                  Inc(j);
              end;

              deleteIt := requests.Count = 0;
            finally
              articlesGetter.UnlockList;
            end;
          end
          else
            if getter is TArticleGetter then
            begin
              articleGetter := TArticleGetter(getter);
              requests := articleGetter.LockList;
              try
                j := 0;
                while j < requests.Count do
                begin
                  getterRequest := TArticleGetterRequest(requests[j]);

                  if (container = nil) or
                     ((getterRequest.Group = container) and
                      ((getterRequest.Article = article) or (article = nil))) then
                    requests.Delete(j)
                  else
                    Inc(j);
                end;

                if requests.Count = 0 then
                  deleteIt := True
                else
                  stopIt := (article = articleGetter.CurrentArticle) and
                    (articleGetter.CurrentGroup = container);
              finally
                articleGetter.UnlockList;
              end;
            end;
      end;

      if stopIt then
        getter.Disconnect;

      if deleteIt then
        fGetterList.Delete(i)
      else
        Inc(i);
    end;
  finally
    UnlockGetterList;
  end;

  ClearSynchronizedMethods;
end;

function TNNTPThreadManager.ConnectToInternet(Settings: TServerSettings): Boolean;
var
  rv: DWORD;
begin
  if settings.RASConnection <> '~' then
  begin
    fConnectToSync.Enter;
    try
      // If not connected to internet, or connected to another ISP
      if (fCurrentConnectoid = '') or
         ((fCurrentConnectoid <> settings.RASConnection) and
          (settings.RASConnection <> '')) then
      begin
        if fConnection <> 0 then
          InternetHangup(fConnection, 0)
        else
          if (fCurrentConnectoid <> '*') then
            InternetAutoDialHangup(0);

        fCurrentConnectoid := '~';
        fConnection := 0;
        fWeConnected := False;
        try
          if settings.RASConnection = '' then
          begin
            if not InternetAutoDial(0, 0) then
              RaiseLastOSError;
          end
          else
          begin
            rv := InternetDial(0, PChar(settings.RasConnection), 0, @fConnection, 0);

            if rv <> 0 then
            begin
              SetLastError(rv);
              RaiseLastOSError;
            end
            else
            begin
              fCurrentConnectoid := settings.RASConnection;
              JogThreads;
            end;
          end;

          fWeConnected := True;
        finally
          GetCurrentConnectoid;
        end;
      end;
    finally
      fConnectToSync.Leave;
    end;
    Result := fCurrentConnectoid <> '';
  end
  else
    Result := True;

  if Result then
    if Assigned(gGetVersionThread) then
      gGetVersionThread.DiallupTrigger
end;

constructor TNNTPThreadManager.Create(const ANewsAgent: string);
begin
  fNewsAgent := ANewsAgent;
  fGetterList := TObjectList.Create;
  fGetterSync := TCriticalSection.Create;
  fConnectToSync := TCriticalSection.Create;
  GetCurrentConnectoid;
  gGetVersionThread := TGetVersionThread.Create;
  gNetworkMonitorThread := TNetworkMonitorThread.Create;
end;

destructor TNNTPThreadManager.Destroy;
begin
  LockGetterList;
  try
    FreeAndNil(fGetterList);
  finally
    UnlockGetterList;
  end;

  if Assigned(gGetVersionThread) then
  try
    gGetVersionThread.Terminate;
  except
  end;
  if Assigned(gNetworkMonitorThread) then
  begin
    gNetworkMonitorThread.Terminate;
    gNetworkMonitorThread.CancelMonitor;
    gNetworkMonitorThread.WaitFor;
    FreeAndNil(gNetworkMonitorThread);
  end;
  if Assigned(XNOptions) and (XNOptions.AutoDisconnectOnIdle or XNOptions.AutoDisconnectOnExit) then
    DoAutoDisconnect;

  LockGetterList;
  UnlockGetterList;

  FreeAndNil(fGetterSync);
  FreeAndNil(fConnectToSync);
  inherited Destroy;
end;

procedure TNNTPThreadManager.DoAutoDisconnect;
var
  i: Integer;
  getter: TTCPGetter;
  canDisconnect: Boolean;
  getters: TObjectList;
begin
  if fWeConnected then  // Hang up MODEM connection
  begin
    canDisconnect := True;
    getters := LockGetterList;
    try
      if Assigned(getters) then
        for i := 0 to getters.Count - 1 do
        begin
          getter := TTCPGetter(getters[i]);
          if ((getter.Settings.RASConnection = '') or (getter.Settings.RASConnection = fCurrentConnectoid)) and
             not (getter.State in [tsDormant, tsDone]) then
          begin
            if not ((Getter is TPoster) and TPoster(Getter).UseOutbasket) then
            begin
              canDisconnect := False;
              Break;
            end;
          end;
        end;
    finally
      UnlockGetterList;
    end;

    if canDisconnect then
    begin
      if (fConnection <> 0) or ((fCurrentConnectoid <> '') and (fCurrentConnectoid <> '~')) then
        if fConnection = 0 then
          InternetAutoDialHangup(0)
        else
          InternetHangup(fConnection, 0);

      fConnection := 0;
      fWeConnected := False;
      fCurrentConnectoid := '';
      GetCurrentConnectoid;     // We may be still connected via LAN
      if Assigned(getters) then
        JogThreads;
    end;
  end;
end;

procedure TNNTPThreadManager.GetArticleBody(account: TNNTPAccount;
  group: TSubscribedGroup; article: TArticle);
var
  getter: TArticleGetter;
begin
  if article.ArticleNo = 0 then
    Exit;
  getter := TArticleGetter(LockFindGetter(TArticleGetter, account.NNTPServerSettings));
  try
    if getter = nil then
    begin
      getter := TArticleGetter.Create(account);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    // Note: last parm has to be False.
    // Otherwise it will go wrong when somebody uses "download headers only"
    //   -and-
    // uses "download body on click".
    //   -and-
    // has auto mark as read set to 0 half seconds.
    //
    // The then just downloaded post will be hidden straight away in
    // TfmMain.DoOnArticleChanged() followed by TfmMain.DoOnArticlesChanged()
    getter.AddArticleToList(group, article, False);

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

procedure TNNTPThreadManager.GetArticles(account: TNNTPAccount;
  group: TSubscribedGroup; fromArticle, articleCount: Int64;
  full: Boolean; ABatchRef: Integer; ASince: TDateTime);
var
  getter: TArticlesGetter;
begin
  getter := TArticlesGetter(LockFindGetter(TArticlesGetter, account.NNTPServerSettings));
  try
    if getter = nil then
    begin
      getter := TArticlesGetter.Create(account);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    getter.AddGroupToList(group, fromArticle, articleCount, full, ABatchRef, ASince);

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

{*----------------------------------------------------------------------*
 | procedure GetArticleThreadBodies                                     |
 |                                                                      |
 | Wind to the root message for the given article - then get all        |
 | message bodies for messages that don't already have them in the      |
 | thread.                                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   account: TNNTPAccount; group: TSubscribedGroup; article: TArticle
 |                                                                      |
 | The function returns None
 *----------------------------------------------------------------------*}
procedure TNNTPThreadManager.GetArticleThreadBodies(account: TNNTPAccount;
  group: TSubscribedGroup; article: TArticle);
var
  getter: TArticleGetter;

  procedure GetThreadBodies(article: TArticle);
  begin
    if not article.HasMsg then
      getter.AddArticleToList(group, article, False); // Note: last parm has to be False

    article := TArticle(article.Child);

    while Assigned(article) do
    begin
      GetThreadBodies(article);
      article := TArticle(article.Sibling);
    end;
  end;

begin
  getter := TArticleGetter(LockFindGetter(TArticleGetter, account.NNTPServerSettings));
  try
    if getter = nil then
    begin
      getter := TArticleGetter.Create(account);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    while article.Parent <> nil do
      article := TArticle(article.Parent);

    GetThreadBodies(article);

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

function TNNTPThreadManager.GetConnected: Boolean;
begin
  Result := ReadCurrentConnectoid <> '';
end;

procedure TNNTPThreadManager.GetCurrentConnectoid;
var
  connectoidFlags: DWORD;
begin
  fConnectToSync.Enter;
  try
    if Assigned(InternetGetConnectedStateEx) then
    begin
      fCurrentConnectoid := '';
      SetLength(fCurrentConnectoid, 256);
      connectoidFlags := DWORD(@InternetGetConnectedStateEx);
      if InternetGetConnectedStateEx(connectoidFlags, PChar(fCurrentConnectoid), 256, 0) then
        if (ConnectoidFlags and INTERNET_CONNECTION_LAN) <> 0 then
          fCurrentConnectoid := '*'
        else
          fCurrentConnectoid := PChar(fCurrentConnectoid)
      else
        fCurrentConnectoid := '';
    end
    else
      if InternetGetConnectedState(@connectoidFlags, 0) then
        if (ConnectoidFlags and INTERNET_CONNECTION_LAN) <> 0 then
          fCurrentConnectoid := '*'
        else
          fCurrentConnectoid := PChar(fCurrentConnectoid);
  finally
    fConnectToSync.Leave;
  end;
end;

procedure TNNTPThreadManager.GetNewsgroups(account: TNNTPAccount);
var
  getter: TNewsgroupGetter;
begin
  getter := TNewsgroupGetter(LockFindGetter(TNewsgroupGetter, account.NNTPServerSettings));
  try
    if getter = nil then
    begin
      getter := TNewsgroupGetter.Create(account);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

function TNNTPThreadManager.GetQueuedRequestCount: Integer;
var
  i: Integer;
  getter: TTCPGetter;
  getters: TObjectList;
  l: Boolean;
begin
  Result := 0;
  l := fLocked;
  if not l then
    getters := LockGetterList
  else
    getters := fGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(getters[i]);
      if getter.State in [tsPending, tsBusy] then
        Result := Result + getter.OutstandingRequestCount;
    end;
  finally
    if not l then
      UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GetQueuedRequestText(idx: Integer): string;
var
  i, n: Integer;
  getter: TTCPGetter;
  getters: TObjectList;
begin
  Result := '';
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(getters[i]);
      if getter.State in [tsPending, tsBusy] then
      begin
        n := getter.OutstandingRequestCount;
        if idx < n then
        begin
          Result := getter.OutstandingRequestText[idx];
          Exit;
        end
        else
          Dec(idx, n);
      end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GetStatusBarMessage(settings: TServerSettings; group: TSubscribedGroup): string;
var
  i: Integer;
  getter: TTCPGetter;
  getters: TObjectList;
begin
  Result := '';
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(getters[i]);

      if (getter.Settings = settings) and (getter.State in [tsPending, tsBusy]) then
      begin
        Result := getter.StatusBarMessage[group];
        Break;
      end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GetThreadManagerState(obj: TObject): TThreadManagerState;
var
  i: Integer;
  getter: TTCPGetter;
  getters: TObjectList;
begin
  Result := tmDormant;
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(Getters[i]);

      if (getter.State = tsPending) and (getter.IsDoing[obj]) then
      begin
        Result := tmPending;
        Break;
      end;

      if (getter.State = tsBusy) and (getter.IsDoing[obj]) then
      begin
        Result := tmBusy;
        Break;
      end;
    end
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GettingArticle(group: TArticleContainer;
  article: TArticleBase): Boolean;
var
  getter: TArticleGetter;
  i: Integer;
  requests: TObjectList;
  request: TArticleGetterRequest;
begin
  Result := False;
  getter := TArticleGetter(LockFindGetter(TArticleGetter, group.ServerSettings));
  try

    if Assigned(getter) then
    begin
      requests := getter.LockList;
      try
        for i := 0 to requests.Count - 1 do
        begin
          request := TArticleGetterRequest(requests[i]);

          if (request.Group = group) and ((article = nil) or (request.Article = article)) then
          begin
            Result := True;
            Break;
          end;
        end;
      finally
        getter.UnlockList;
      end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GettingArticleList(
  group: TArticleContainer; Resume: Boolean = False): Boolean;
var
  getter: TArticlesGetter;
  i: Integer;
  requests: TObjectList;
  request: TArticlesGetterRequest;
begin
  Result := False;
  getter := TArticlesGetter(LockFindGetter(TArticlesGetter, group.ServerSettings));
  try
    if Assigned(getter) then
    begin
      requests := getter.LockList;
      try
        for i := 0 to requests.Count - 1 do
        begin
          request := TArticlesGetterRequest(requests[i]);

          if request.Group = group then
          begin
            Result := True;
            if Resume and (getter.State in [tsDormant, tsPending]) then
              getter.Resume;
            Break;
          end;
        end;
      finally
        getter.UnlockList;
      end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GettingNewsgroupList(account: TNNTPAccount): Boolean;
var
  getter: TTCPGetter;
begin
  getter := LockFindGetter(TNewsgroupGetter, account.NNTPServerSettings);
  try
    Result := Assigned(getter) and (getter.State in [tsBusy, tsPending]);
  finally
    UnlockGetterList;
  end;
end;

procedure TNNTPThreadManager.JogThreads;
var
  i: Integer;
  connectoid: string;
  lanOverride: Boolean;
  getters: TObjectList;
begin
  if gAppTerminating then Exit;
  GetCurrentConnectoid;
  connectoid := ReadCurrentConnectoid;

  lanOverride := connectoid = '*';

  getters := LockGetterList;
  try
    if lanOverride then
      for i := 0 to getters.Count - 1 do
        if TTCPGetter(getters[i]).State = tsBusy then
          lanOverride := False;

    if lanOverride then
      connectoid := '';

    if connectoid = '' then
      for i := 0 to getters.Count - 1 do
        if (TTCPGetter(getters[i]).State <> tsDormant) and (TTCPGetter(getters[i]).Settings.RASConnection <> '') and (TTCPGetter(getters[i]).Settings.RASConnection <> '~') then
        begin
          connectoid := TTCPGetter(getters[i]).Settings.RASConnection;
          Break;
        end;

    for i := 0 to getters.Count - 1 do
      if (TTCPGetter(getters[i]).Settings.RASConnection = connectoid) and (TTCPGetter(getters[i]).State = tsPending) then
        TTCPGetter(getters[i]).Resume;
  finally
    UnlockGetterList;
  end;

  Sleep(0);

  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
      if ((TTCPGetter(getters[i]).Settings.RASConnection = '') or (TTCPGetter(getters[i]).Settings.RASConnection = '~')) and (TTCPGetter(getters[i]).State = tsPending) then
        TTCPGetter(getters[i]).Resume;
  finally
    UnlockGetterList;
  end;

  Sleep(0);
end;

function TNNTPThreadManager.LockFindGetter(cls: TTCPGetterClass;
  settings: TServerSettings): TTCPGetter;
var
  i: Integer;
  getter: TTCPGetter;
begin
  Result := nil;
  LockGetterList;
  for i := 0 to fGetterList.Count - 1 do
  begin
    getter := TTCPGetter(fGetterList[i]);

    if (getter is cls) and (getter.Settings.Equals(settings)) then
    begin
      Result := getter;
      Break;
    end;
  end;
end;

procedure TNNTPThreadManager.PostMessage(account: TNNTPAccount; hdr: TAnsiStrings; const msg: RawByteString;
  attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle);
var
  getter: TPoster;
  sentMessages: TSentMessages;
  nsettings: TNNTPServerSettings;
  i: Integer;
  naccount: TNNTPAccount;
begin
  if Account.PostingSettings.ArchivePostedMessages then
  begin
    sentMessages := TSentMessages(gArticleFolders.FindFolder('Posted Messages'));
    if Assigned(sentMessages) then
      sentMessages.AddMessage(account, hdr, msg, attachments, ACodePage, AtextPartStyle);
  end;

  nsettings := nil;
  naccount := nil;
  if account.PostingAccountName <> '' then
    for i := 0 to NNTPAccounts.Count - 1 do
      if NNTPAccounts[i].AccountName = account.PostingAccountName then
      begin
        naccount := NNTPAccounts[i];
        nsettings := naccount.NNTPServerSettings;
        Break;
      end;

  if nsettings = nil then
    nsettings := account.NNTPServerSettings;

  if naccount = nil then
    naccount := account;

  getter := TPoster(LockFindGetter(TPoster, nsettings));
  try
    if getter = nil then
    begin
      getter := TPoster.Create(naccount);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    getter.AddPostToList(hdr, msg, attachments, ACodepage, ATextPartStyle);

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

procedure TNNTPThreadManager.PostMessage(account: TNNTPAccount; const hdr, msg: string;
  attachments: TObjectList; ACodepage: Integer; ATextPartStyle: TTextPartStyle);
var
  h: TAnsiStrings;
  m: RawByteString;
begin
  h := TAnsiStringList.Create;
  try
    h.Text := WideStringToAnsiString(hdr, ACodePage);
    m := WideStringToAnsiString(msg, ACodePage);
    PostMessage(account, h, m, attachments, ACodepage, ATextPartStyle);
  finally
    h.Free;
  end;
end;

function TNNTPThreadManager.StopArticleDownloads(group: TSubscribedGroup): Boolean;
var
  getter: TTCPGetter;
  getters: TObjectList;
  i: Integer;
begin
  i := 0;
  Result := False;
  getters := LockGetterList;
  try
    while i < getters.Count do
    begin
      getter := TTCPGetter(getters[i]);
      if (getter is TArticleGetter) and
         (TArticleGetter(getter).CurrentGroup = group) and
         (getter.State in [tsBusy, tsDone]) then
      begin
        Result := True;
        getter.Disconnect;
        getter.State := tsPending;
        getter.Clear;
      end;
      Inc(i);
    end
  finally
    UnlockGetterList;
  end;
end;

procedure TNNTPThreadManager.DisconnectAll(Done: Boolean);
var
  i: Integer;
  getters: TObjectList;
begin
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    try
      TTCPGetter(getters[i]).Disconnect(Done);
    except
    end;
  finally
    UnlockGetterList;
  end;
end;

procedure TNNTPThreadManager.SetAllThreadsPaused(const Value: Boolean);
var
  i: Integer;
  getters: TObjectList;
begin
  if fAllThreadsPaused <> Value then
  begin
    fAllThreadsPaused := Value;
    getters := LockGetterList;
    try
      for i := 0 to getters.Count - 1 do
        TTCPGetter(getters[i]).Paused := Value;
    finally
      UnlockGetterList;
    end;
  end;
end;

procedure TNNTPThreadManager.SendSMTPMail(articleContainer: TServerAccount;
  settings: TSMTPServerSettings; const sTo, sCC, sBCC, sSubject, sReplyTo, msg: string;
  attachments: TObjectList; ACodePage: Integer; AUseOutbasket: Boolean);
var
  getter: TEMailer;
begin
  getter := TEMailer(LockFindGetter(TEMailer, settings));
  try
    if getter = nil then
    begin
      getter := TEMailer.Create(settings, AUseOutbasket);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    getter.AddMessageToList(articleContainer, sTo, sCC, sBCC, sSubject, sReplyTo, msg, attachments, ACodePage);

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

procedure TNNTPThreadManager.GetProgressNumbers(settings: TServerSettings; group: TSubscribedGroup; var min, max, pos: Integer);
var
  i: Integer;
  getter: TTCPGetter;
  getters: TObjectList;
begin
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(getters[i]);

      if (getter.Settings = settings) and (getter.State in [tsPending, tsBusy]) then
      begin
        getter.GetProgressNumbers(group, min, max, pos);
        Break;
      end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GetActiveGetter(idx: Integer): TTCPGetter;
var
  i: Integer;
  getters: TObjectList;
  l: Boolean;
begin
  i := 0;
  Result := nil;
  l := fLocked;
  if not l then
    getters := LockGetterList
  else
    getters := fGetterList;
  try
    while i < getters.Count do
    begin
      if TTCPGetter(getters[i]).State <> tsDormant then
        if idx = 0 then
        begin
          Result := TTCPGetter(getters[i]);
          Break;
        end
        else
          Dec(idx);
      Inc(i);
    end;
  finally
    if not l then
      UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GetActiveGetterCount: Integer;
var
  i: Integer;
  getters: TObjectList;
  l: Boolean;
begin
  Result := 0;

  l := fLocked;
  if not l then
    getters := LockGetterList
  else
    getters := fGetterList;

  try
    for i := 0 to getters.Count - 1 do
      if TTCPGetter(getters[i]).State <> tsDormant then
        Inc(Result);
  finally
    if not l then
      UnlockGetterList;
  end;
end;

function TNNTPThreadManager.GetNoOutbasketEntries: Integer;
var
  i: Integer;
  getter: TTCPGetter;
  poster: TPoster;
  emailer: TEmailer;
  getters: TObjectList;
begin
  Result := 0;
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(getters[i]);
      if getter is TPoster then
      begin
        poster := TPoster(getter);
        if poster.Account.PostingSettings.DelayPosting and (poster.Count > 0) then
          Inc(Result);
      end
      else
        if getter is TEmailer then
        begin
          emailer := TEMailer(getter);
          if emailer.OrigUseOutbasket and (emailer.Count > 0) then
            Inc(Result);
        end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.ReadCurrentConnectoid: string;
begin
  fConnectToSync.Enter;
  try
    Result := fCurrentConnectoid;
  finally
    fConnectToSync.Leave;
  end;
end;

procedure TNNTPThreadManager.ResumeOutbasketEntries;
var
  i: Integer;
  getter: TTCPGetter;
  poster: TPoster;
  emailer: TEmailer;
  getters: TObjectList;
begin
  getters := LockGetterList;
  try
    for i := 0 to getters.Count - 1 do
    begin
      getter := TTCPGetter(getters[i]);
      if getter is TPoster then
      begin
        poster := TPoster(getter);
        if poster.Account.PostingSettings.DelayPosting and (poster.Count > 0) then
          poster.ResumeOutbasket;
      end
      else
        if getter is TEMailer then
        begin
          emailer := TEMailer(getter);
          if emailer.OrigUseOutbasket and (emailer.Count > 0) then
            emailer.ResumeOutbasket;
        end;
    end;
  finally
    UnlockGetterList;
  end;
end;

procedure TNNTPThreadManager.ClearGetters;
begin
  LockGetterList;
  try
    fGetterList.Clear
  finally
    UnlockGetterList;
  end;
end;

procedure TNNTPThreadManager.ClearGettersForAccount(account: TNNTPAccount);
var
  I: Integer;
  getter: TTCPGetter;
begin
  LockGetterList;
  try
    for I := fGetterList.Count - 1 downto 0 do
    begin
      getter := TTCPGetter(fGetterList[I]);
      if getter is TNewsGetter then
        if TNewsGetter(getter).Account = account then
        begin
          getter.Disconnect(True);
          fGetterList.Delete(I);
        end;
    end;
  finally
    UnlockGetterList;
  end;
end;

function TNNTPThreadManager.LockGetterList: TObjectList;
begin
  fGetterSync.Enter;
  fLocked := True;
  Result := fGetterList;
end;

procedure TNNTPThreadManager.UnlockGetterList;
begin
  fLocked := False;
  fGetterSync.Leave;
end;

procedure TNNTPThreadManager.GetParentArticles(article: TArticle);
var
  getter: TArticleGetter;
  group: TSubscribedGroup;
  account: TNNTPAccount;
  ref, id: RawByteString;
  art: TArticle;
  isNewArticle: Boolean;
begin
  if article.References = '' then
    Exit;

  group := TSubscribedGroup(article.Owner);
  account := group.Owner;
  getter := TArticleGetter(LockFindGetter(TArticleGetter, account.NNTPServerSettings));
  try
    if getter = nil then
    begin
      getter := TArticleGetter.Create(account);
      fGetterList.Add(getter);
      getter.Paused := AllThreadsPaused;
    end;

    ref := Article.RawReferences;
    repeat
      id := RawSplitString(' ', ref);
      art := group.FindMsgID(id) as TArticle;

      isNewArticle := False;
      if art = nil then
      begin
        art := TArticle.Create(group);
        art.RawMessageID := id;
        group.RawAddArticle(art);
        isNewArticle := True;
      end;

      if not art.HasMsg then
        getter.AddArticleToList(group, art, IsNewArticle);
    until ref = '';

    if getter.State <> tsBusy then
      getter.State := tsPending;
  finally
    UnlockGetterList;
  end;

  JogThreads;
end;

function TNNTPThreadManager.CountActiveGettersForAccount(acc: TNNTPAccount): Integer;
var
  i: Integer;
  getter: TNewsGetter;
  l: Boolean;
  getters: TObjectList;
begin
  Result := 0;

  l := fLocked;
  if not l then
    getters := LockGetterList
  else
    getters := fGetterList;

  try
    for i := 0 to getters.Count - 1 do
      if getters[i] is TNewsGetter then
      begin
        getter := TNewsGetter(getters[i]);
        if (getter.Account = acc) and getter.Connected then
          Inc(Result);
      end;
  finally
    if not l then
      UnlockGetterList;
  end;
end;

procedure TNNTPThreadManager.ClearDormantConnections(acc: TNNTPAccount);
var
  i: Integer;
  getter: TNewsGetter;
  l: Boolean;
  getters: TObjectList;
begin
  l := fLocked;
  if not l then
    getters := LockGetterList
  else
    getters := fGetterList;

  try
    for i := 0 to getters.Count - 1 do
      if getters[i] is TNewsGetter then
      begin
        getter := TNewsGetter(getters[i]);
        if (getter.Account = acc) and getter.Connected and (getter.State = tsDormant) then
          getter.Disconnect;
      end;
  finally
    if not l then
      UnlockGetterList;
  end;
end;

{ TNetworkMonitor }

constructor TNetworkMonitorThread.Create;
begin
  inherited Create(True);
  FOverlap.hEvent := WSACreateEvent;
end;

destructor TNetworkMonitorThread.Destroy;
begin
  inherited Destroy;
  WSACloseEvent(FOverlap.hEvent);
end;

procedure TNetworkMonitorThread.CancelMonitor;
begin
  SetEvent(FOverlap.hEvent);
  inherited;
end;

procedure TNetworkMonitorThread.Execute;
begin
  while not Terminated do
  begin
    // this blocks until an IP change occurs
    NotifyAddrChange(@FHandle, @FOverlap);
    if WaitForSingleObject(FOverlap.hEvent, INFINITE) = WAIT_OBJECT_0 then
      ThreadManager.JogThreads;
  end;
end;

initialization
  hWinINet := LoadLibrary(wininetdll);
  if hWinINet <> 0 then
    InternetGetConnectedStateEx := GetProcAddress(hWinINet, 'InternetGetConnectedStateExA');
  hiphlpapi := LoadLibrary(iphlpapilib);
  if hiphlpapi <> 0 then
    NotifyAddrChange := GetProcAddress(hiphlpapi, 'NotifyAddrChange');
  InitializeWinSock;
finalization
  if hWinINet <> 0 then
    FreeLibrary(hWinINet);
  if hWinINet <> 0 then
    FreeLibrary(hiphlpapi);
end.
