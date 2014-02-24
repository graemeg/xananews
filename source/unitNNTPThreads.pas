(*======================================================================*
 | unitNNTPThreads                                                      |
 |                                                                      |
 | NNTP Threads for News reader 3                                       |
 |                                                                      |
 | nb.  fCurrentConnectoid works like this...                           |
 |                                                                      |
 | = ''   Not connected                                                 |
 | = '*'  Connected to LAN                                              |
 | = '~'  Connecting                                                    |
 | = 'nnn'  nnn = ISP                                                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      25/07/2001  CPWW  Original                                  |
 | 1.1      30/07/2002  SG    SSL support (use SSLSupport define)       |
 | 1.17.3.2 23/03/2005  CPWW  Implemented thread timeout & Max          |
 |                            Connections                               |
 *======================================================================*)

unit unitNNTPThreads;

interface

uses
  Windows, Classes, SysUtils, Forms, SyncObjs, IdNNTPX, Contnrs, {WinINet,}
  unitNNTPServices, IdTCPClient, unitNewsThread, IdSSLOpenSSL, IdSMTP, IdMessage,
  unitSettings, XnClasses, XnRawByteStrings;

type
  //---------------------------------------------------
  // TNNTPThread - base class for NNTP worker threads
  TNNTPThread = class(TTCPThread)
  private
    fNNTPAccount: TNNTPAccount;
    fGreet: string;
    function GetCapabilities: TStringList;
    function GetClient: TidNNTPX;
    procedure DoNotifyNewGroups;
    function GetSettings: TNNTPServerSettings;
    procedure SetGreeting;
  protected
    procedure CheckCapabilities;
    procedure CheckNewGroups;
    procedure Execute; override;
  public
    constructor Create(AGetter: TTCPGetter; ASettings: TServerSettings); override;
    destructor Destroy; override;
    property NNTPAccount: TNNTPAccount read fNNTPAccount write fNNTPAccount;
    property NNTP: TidNNTPX read GetClient;
    property ServerSettings: TNNTPServerSettings read GetSettings;
  end;

  TSMTPThread = class(TTCPThread)
  private
    function GetClient: TidSMTP;
    function GetSettings: TSMTPServerSettings;

  protected
    procedure Execute; override;
  public
    constructor Create(AGetter: TTCPGetter; ASettings: TServerSettings); override;
    destructor Destroy; override;
    property SMTP: TidSMTP read GetClient;
    property Settings: TSMTPServerSettings read GetSettings;
  end;

  TNNTPThreadClass = class of TNNTPThread;

  TNNTPNewsgroupThread = class(TNNTPThread)
  protected
    procedure DoWork; override;
  end;

  TNNTPArticlesThread = class(TNNTPThread)
  private
    fExpectedArticles: Integer;
    fIsXOver: Boolean;
    fCurrentArticleNo: Int64;     // For status bar - 0..fExpectedArticles
    fCurrentGetter: TArticlesGetter;
    procedure DoPipeLineCommandStartEvent(cmd: TPipelineCommand; var headrs: TAnsiStrings; var body: TStream);
    procedure DoPipeLineCommandEndEvent(cmd: TPipelineCommand);
    procedure DoPipeLineCommandCancelEvent(cmd: TPipelineCommand; startCalled: Boolean);
  protected
    procedure DoWork; override;
  public
    procedure GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer); override;
  end;

  TNNTPArticleThread = class(TNNTPThread)
  private
    fFailedArticle: TArticle;
    fUICurrentArticle: TArticle;
    procedure DoArticleFailed;
    procedure DoPipeLineCommandStartEvent(cmd: TPipelineCommand; var headrs: TAnsiStrings; var body: TStream);
    procedure DoPipeLineCommandEndEvent(cmd: TPipelineCommand);
    procedure DoPipeLineCommandCancelEvent(cmd: TPipelineCommand; startCalled: Boolean);
  protected
    procedure DoWork; override;
  public
    procedure GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer); override;
  end;

  TNNTPPoster = class(TNNTPThread)
  protected
    procedure DoWork; override;
  end;

  TSMTPMailer = class(TSMTPThread)
  protected
    procedure DoWork; override;
  end;

implementation

uses
  DateUtils,
  IdException, unitNNTPThreadManager, unitNewsReaderOptions, unitMessages,
  unitCharsetMap, unitMailServices, unitNewsgroups, unitLog, unitQpcTimer,
  unitSearchString,
  NewsGlobals,
  IdGlobal, IdReplyRFC, IdStack, IdGlobalProtocols, IdExceptionCore,
  IdAttachmentFile, IdExplicitTLSClientServerBase;

{ TNNTPThread }

function GetExtendedExceptionInfo(const E: Exception): string;
begin
  Result := 'Class: ' + E.ClassName;
  if (e is EIdReplyRFCError) then
    Result := Result + ', ErrorCode: ' + IntToStr(EIdReplyRFCError(E).ErrorCode);
  if (e is EIdSocketError) then
    Result := Result + ', LastError: ' + IntToStr(EIdSocketError(E).LastError);
  Result := Result + ', Message: ' + E.Message;
end;

procedure TNNTPThread.CheckCapabilities;
begin
  try
    GetCapabilities();
  except
    on E: Exception do
    begin
      LogMessage('CAPABILITIES: ' + GetExtendedExceptionInfo(E));
    end;
  end;
end;

procedure TNNTPThread.CheckNewGroups;
var
  FileName: string;
  dt: TDateTime;
  groups: TNewsgroupsStringList;
  newGroups: TStringList;
  i: Integer;
  st: string;
  group: string;
  modified: Boolean;
begin
  if HoursBetween(Now, NNTPAccount.LastCheckForNewGroups) < 6 then Exit;

  newGroups := nil;
  groups := nil;
  try
    FileName := gMessageBaseRoot + '\' + NNTPAccount.AccountName + '\newsgroups.dat';
    if FileAge(fileName, dt) then
    begin

      newGroups := TStringList.Create;

      try
        NNTP.GetNewGroupsList(dt, True, '', newGroups);

        if newGroups.Count > 0 then
        begin
          groups := TNewsgroupsStringList.Create;
          groups.LoadFromFile(fileName);

          groups.Sorted := True;
          groups.Duplicates := dupIgnore;

          modified := False;

          // Check incoming line(s) should be:
          //    group last first p
          // where <group> is the name of the newsgroup, <last> is the number of
          // the last known article currently in that newsgroup, <first> is the
          // number of the first article currently in the newsgroup, and <p> is
          // either 'y' or 'n' indicating whether posting to this newsgroup is
          // allowed ('y') or prohibited ('n').

          for i := 0 to newGroups.Count - 1 do
          begin
            st := newGroups[i];
            if Length(st) > 0 then
            begin
              group := SplitString(' ', st);
              if (StrToInt64Def(SplitString(' ', st), -1) <> -1) and
                 (StrToInt64Def(SplitString(' ', st), -1) <> -1) then
              begin
                // Received a valid new group, add it to the list if it didn't exist yet.
                if groups.IndexOf(group) = -1 then
                begin
                  groups.Add(newGroups[i] + ' *');
                  modified := True;
                end;
              end
              else
              begin
                st := 'NEWGROUPS: Invalid new group recieved for account: "' +
                      NNTPAccount.AccountName + '" - ' + newGroups[i];
                LogMessage(st, True);
              end;
            end;
          end;

          if modified then
          begin
            groups.SaveToFile(fileName);
            Synchronize(DoNotifyNewGroups);
          end;
        end;

      except
        on E: Exception do
        begin
          st := 'NEWGROUPS: Error checking for new groups for account: "' +
                NNTPAccount.AccountName + '" - ' + E.Message;
          LogMessage(st, True);
        end;
      end;
    end;
  finally
    newGroups.Free;
    groups.Free;
    NNTPAccount.LastCheckForNewGroups := Now;
  end;
end;

constructor TNNTPThread.Create(AGetter: TTCPGetter; ASettings: TServerSettings);
begin
  inherited Create(AGetter, ASettings);
  fClient := TidNNTPX.Create(nil);
end;

destructor TNNTPThread.Destroy;
begin
  inherited Destroy;
end;

procedure TNNTPThread.DoNotifyNewGroups;
begin
  if Assigned(ThreadManager.OnNotifyNewGroups) then
    ThreadManager.OnNotifyNewGroups(Self, NNTPAccount);
end;

procedure TNNTPThread.Execute;
var
  st: string;
  sTimeOut, tm: DWORD;
  ServerFault: Boolean;
  eActive: Boolean;
  eTimer: Int64;
begin
  eActive := False;
  eTimer := 0;

  if Assigned(NNTPAccount) then
  begin
    st := 'NNTP Getter Thread for ' + NNTPAccount.AccountName;
    sTimeOut := NNTPAccount.NNTPServerSettings.ServerTimeout;
  end
  else
  begin
    sTimeOut := 0;
    st := 'NNTP Getter Thread for ???';
  end;

  SendMessage(Application.MainForm.Handle, WM_NAMETHREAD, ThreadID, LPARAM(PChar(st)));

  while not Terminated do
  begin

    while not Terminated do
    begin
      if (State = tsDormant) and NNTP.Connected and (sTimeOut <> 0) then
        tm := 1000 * sTimeOut
      else
        if eActive then
          tm := 1000
        else
          tm := INFINITE;

      case Trigger.WaitFor(tm) of
        wrTimeout:
          begin
            if not eActive or qpcTimerDone(eTimer) then
            begin
              eActive := False;
              NNTP.Disconnect;
            end;
            Continue;
          end;

        wrSignaled:
          if eActive and not Getter.Terminating and not qpcTimerDone(eTimer) then
            Continue
          else
            Break;
      end;
    end;

    if Getter.Terminating then
      Exit;

    State := tsBusy;

    // Minimal wait time after an exception before retrying is 30 seconds.
    eActive := False;
    eTimer  := qpcStartTimer(30.0);

    if not Terminated then
    try
      if not NNTP.Connected then     // Connect!
      begin
        if ThreadManager.ConnectToInternet(ServerSettings) then
        begin
          NNTP.Host := ServerSettings.ServerName;
          NNTP.UserName := ServerSettings.ServerAccountName;
          NNTP.Password := ServerSettings.ServerPassword;
          NNTP.ConnectTimeout := 1000 * ServerSettings.ConnectTimeout;
          NNTP.ReadTimeout := 1000 * ServerSettings.ReadTimeout;
          if ServerSettings.SSLrequired then
          begin
            NNTP.IOHandler := SSLHandler;
            NNTP.Port := ServerSettings.SSLPort;
          end
          else
            NNTP.Port := ServerSettings.ServerPort;
          NNTP.PipelineSize := ServerSettings.PipelineSize;
          NNTP.Mode := mtReader;
          NNTP.NewsAgent := ThreadManager.NewsAgent;
          NNTP.Connect;
          NNTP.IOHandler.DefStringEncoding := Indy8BitEncoding;
          NNTP.IOHandler.MaxLineAction := maSplit;
          fGreet := NNTP.Greeting.Text.Text;
          Synchronize(SetGreeting);
          if ServerSettings.AlwaysAuthenticate then
            NNTP.Authenticate;
        end;
      end;

      if not Terminated and NNTP.Connected then
      begin
// << CAPABILITIES, the world is not ready for this yet.
//    - most servers respond with        500 Command not recognized
//    - news.components4developers.com   ReadTimeout
//    - news.soft-gems.net               400 Command not recognized
//    - Only recent builds (nov 2009) of INN 2.5.2 and 2.6.0 support this.
//        CheckCapabilities();
// >>
        CheckNewGroups();

        Getter.ClearWork;
        DoWork;
        State := tsDone;
        Getter.WorkDone;
        Synchronize(Getter.NotifyUI);
        Getter.ClearWork;

        if (Getter is TMultiNewsGetter) and (TMultiNewsGetter(Getter).Count > 0) then
          State := tsPending
        else
          State := tsDormant;
      end
      else
        State := tsDormant;

      if not gAppTerminating then
        Synchronize(ThreadManager.JogThreads);
    except
      on E: Exception do
      begin
        eActive := True;

        fLastError := E.Message;
        State := tsPending;

        st := 'Error in thread "' + Getter.GetterRootText + '" - ';
        st := st + GetExtendedExceptionInfo(E);
        LogMessage(st, True);

        if not gAppTerminating then
          Synchronize(NotifyError);

        // When the exception was caused by a server fault no need to
        // notify the peer, just close the IOHandler.  Notifying the peer
        // would just reraise the same exception and the thread would die.
        ServerFault := NNTP.IsServerException(E);
        try
          NNTP.Disconnect(not ServerFault);
        except
          on E: Exception do
          begin
            st := 'Error in thread "' + Getter.GetterRootText + '" while disconnecting after an exception occurred - ';
            st := st + GetExtendedExceptionInfo(E);
            LogMessage(st, True);
          end;
        end;

        if not gAppTerminating then
          try
            ThreadManager.GetCurrentConnectoid;
          except
          end;

        try
          if Getter is TMultiNewsGetter and (TMultiNewsGetter(Getter).Count = 0) then
            State := tsDormant
          else
            State := tsPending;
        except
          State := tsPending;
        end;

        if not gAppTerminating then
        begin
          if E is EIdConnClosedGracefully then
            Synchronize(ThreadManager.JogThreads);

          // Retry "immediately" with 400 & 503 & 10054 errors or ReadTimeout,
          // otherwise wait for the user to retrigger.
          // nb.  400 = Server disconnected (eg. timeout)    (=EIdReplyRFCError)
          //      503 = Server Fault                         (=EIdReplyRFCError)
          //    10054 = Connection reset by peer             (=EIdSocketError)
          if ServerFault or (E is EIdReadTimeout) then
          begin
            if Getter is TPoster then
            begin
              // If it concerns a posting thread, pause it, to prevent
              // (possible) dublicate posts to end up on the server.
              TPoster(Getter).Paused := True;
              TPoster(Getter).UseOutbasket := True;
            end
            else
            begin
              Sleep(250);
              Synchronize(ThreadManager.JogThreads);
            end;
          end;
        end;

        if not (E is EIdException) then
          fLastError := '';
      end;
    end;
  end;
end;

function TNNTPThread.GetCapabilities: TStringList;
begin
  Result := NNTPAccount.Capabilities;

  if not Assigned(Result) then
  begin
    Result := TStringList.Create;
    try
      try
        NNTP.GetCapabilities(Result);
      except
        on e: EIdReplyRFCError do
          case e.ErrorCode of
            500, 501: ;                 // Capabilities not supported.  Not an
                                        // error - just a lousy news server.
            else
              raise;
          end
        else
          raise;
      end;

      NNTPAccount.Capabilities := Result;
    finally
      Result.Free;
    end;
  end;

  Result := NNTPAccount.Capabilities;
end;


function TNNTPThread.GetClient: TidNNTPX;
begin
  Result := TidNNTPx(Client);
end;

function TNNTPThread.GetSettings: TNNTPServerSettings;
begin
  Result := TNNTPServerSettings(fSettings);
end;

procedure TNNTPThread.SetGreeting;
begin
  fNNTPAccount.Greeting := fGreet;
end;

{ TNNTPNewsgroupThread }

procedure TNNTPNewsgroupThread.DoWork;
begin
  NNTP.GetNewsgroupList(TNewsgroupGetter(getter).fNewsgroups);
end;

{ TNNTPArticlesThread }

procedure TNNTPArticlesThread.DoPipeLineCommandCancelEvent(
  cmd: TPipelineCommand; startCalled: Boolean);
begin
  Inc(fCurrentArticleNo);
end;

procedure TNNTPArticlesThread.DoPipeLineCommandEndEvent(cmd: TPipelineCommand);
var
  gtr: TArticlesGetter;
  st: RawByteString;
  i: Integer;
begin
  gtr := TArticlesGetter(cmd.Param);
  if gtr.CurrentFull then
  begin
    LogMessage(gtr.CurrentGroup.Name + ' - Sync ' + IntToStr(gtr.CurrentArticleNo));
    Synchronize(gtr.SaveCurrentArticle);
  end
  else
  begin
                    // Wanted header only - but can't do XOVER (server doesn't support it).
                    //
                    // Build a string containing headers in XOVER fmt

    for i := 0 to gtr.header.Count - 1 do
      gtr.Header[i] := RawStringReplace(gtr.Header[i], ':', '=', []);

    st := RawIntToStr(cmd.ArticleNo) + #9 +
          RawTrim(gtr.Header.Values['Subject']) + #9 +
          RawTrim(gtr.Header.Values['From']) + #9 +
          RawTrim(gtr.Header.Values['Date']) + #9 +
          RawTrim(gtr.Header.Values['Message-ID']) + #9 +
          RawTrim(gtr.Header.Values['References']) + #9 +
          RawTrim(gtr.Header.Values['Bytes']) + #9 +
          RawTrim(gtr.Header.Values['Lines']) + #9 +
          RawTrim(gtr.Header.Values['Xref']);

    TArticlesGetter(getter).Articles.Add(st);
  end;
  Inc(fCurrentArticleNo);
end;

procedure TNNTPArticlesThread.DoPipeLineCommandStartEvent(
  cmd: TPipelineCommand; var headrs: TAnsiStrings; var body: TStream);
var
  gtr: TArticlesGetter;
begin
  gtr := TArticlesGetter(cmd.Param);
  gtr.CurrentArticleNo := cmd.ArticleNo;
  headrs := gtr.Header;
  body := gtr.Body;
end;

(*----------------------------------------------------------------------*
 | TNNTPArticlesThread.DoWork                                           |
 |                                                                      |
 | Get article range.  Headers only or headers & bodies.                |
 *----------------------------------------------------------------------*)
procedure TNNTPArticlesThread.DoWork;
var
  XOverFMT: TStringList;
  fromArticle, articleCount, dest: Int64;
  request: TArticlesGetterRequest;
  gtr: TArticlesGetter;
  requests: TObjectList;
  needsRetry: Boolean;
  ok: Boolean;
  I: Integer;
  st: string;

  function GetXOverFMT: TStringList;
  begin
    Result := NNTPAccount.XOverFMT;

    if not Assigned(Result) and not NNTPAccount.NoXNews then       // Get overview.fmt list
    begin
      Result := TStringList.Create;
      try
        try
          NNTP.GetOverviewFMT(Result);
        except
          on e: EIdReplyRFCError do
            case e.ErrorCode of
              500, 501: ;                 // overview.fmt not supported.  Not an
                                          // error - just a lousy news server.  We'll
                                          // have to get headers one by one.
              else
                raise;
            end
          else
            raise;
        end;

        NNTPAccount.XOverFMT := Result;
      finally
        Result.Free;
      end;
    end;

    Result := NNTPAccount.XOverFMT;
  end;

  function GetFirstArticleNoSince(date: TDateTime): Int64;
  var
    nearest: Int64;
    head: TAnsiStrings;
    dtst: RawByteString;
    dt: TDateTime;

    function CmpDate(d1, d2: TDateTime): Integer;
    var
      diff: TDateTime;
    begin
      diff := d2 - d1;

      if Abs(diff) < 0.000000001 then
        Result := 0
      else
        if diff > 0 then
          Result := 1
        else
          Result := -1;
    end;

    function NNTPSearch(s, e: Int64): Integer;
    var
      n: Int64;
    begin
      if e >= s then
      begin
        nearest := s + (e - s) div 2;
        n := nearest;
        while not NNTP.GetHeader(nearest + 1, '', head) do // while article missing;
        begin
          Inc(nearest);
          if nearest = e then
          begin
            Result := -2;
            Exit;
          end;
        end;

        dtst := RawTrim(head.Values['Date']);
        if dtst <> '' then
          dt := GMTToLocalDateTime(string(dtst))
        else
          dt := 0.0;

        if CmpDate(dt, 0.0) = 0 then
        begin
          Result := -2;
          Exit;
        end;

        case CmpDate(dt, date) of
          -1: Result := NNTPSearch(s, n - 1);
           1: Result := NNTPSearch(nearest + 1, e);
        else
          Result := nearest;
        end;
      end
      else
        Result := -1;
    end;

  begin
    head := TAnsiStringList.Create;
    try
      head.NameValueSeparator := ':';
      Result := NNTPSearch(NNTP.MsgLow - 1, NNTP.MsgHigh - 1);
      if Result = -2 then Exit; // Internal error.

      if Result = -1 then
      begin
        Result := nearest;
        if Result > 0 then
          Dec(Result);
      end
    finally
      head.Free;
    end;
  end;

  procedure GetXOverHeaders(fromArticle, articleCount: Int64);
  begin
    LogMessage(gtr.CurrentGroup.Name + ' XOVER');
    try
      if articleCount = 0 then
        NNTP.SendXOVER(IntToStr(fromArticle) + '-', gtr.Articles)
      else
        NNTP.SendXOVER(IntToStr(fromArticle) + '-' + IntToStr(fromArticle + articleCount - 1), gtr.Articles);
    finally
      gtr.XOverFMT := XOverFmt;
      Synchronize(gtr.UpdateHeaders);
    end;
  end;

  procedure GetPipelineArticles(fromArticle, dest: Int64);
  var
    msgNo: Int64;
  begin
    LogMessage(gtr.CurrentGroup.Name + ' Pipeline get ' + IntToStr(fromArticle) + '-' + IntToStr(dest));
    NNTP.PipelineCommandStartEvent := DoPipelineCommandStartEvent;
    NNTP.PipelineCommandEndEvent := DoPipelineCommandEndEvent;
    NNTP.PipelineCommandAbortEvent := DoPipelineCommandCancelEvent;
    if fromArticle <= dest then
    try
      NNTP.BeginPipeline;
      try
        msgNo := fromArticle;
        while msgNo <= dest do
        begin
          if gtr.CurrentFull then
            NNTP.PipelineGetArticle(msgNo, '', LPARAM(gtr))
          else
            NNTP.PipelineGetHeader(msgNo, '', LPARAM(gtr));
          Inc(msgNo);
        end;
      finally
        NNTP.EndPipeline;
      end
    finally
      if gtr.CurrentFull then
        Synchronize(gtr.UpdateArticles)
      else
      begin
        gtr.XOverFMT := XOverFMT;
        Synchronize(gtr.UpdateHeaders);
      end;
    end;
  end;

  procedure GetArticles(fromArticle, dest: Int64);
  var
    msgNo: Int64;
  begin
    LogMessage(gtr.CurrentGroup.Name + ' Get ' + IntToStr(fromArticle) + '-' + IntToStr(dest));
    try
      msgNo := fromArticle;
      while msgNo <= dest do
      begin
        gtr.CurrentArticleNo := msgNo;

        gtr.Header.Clear;
        gtr.Body.Clear;
        if NNTP.GetArticle(msgNo, '', gtr.Header, gtr.Body) then
        begin
          LogMessage(gtr.CurrentGroup.Name + ' - Sync ' + IntToStr(gtr.CurrentArticleNo));
          Synchronize(gtr.SaveCurrentArticle)
        end
        else
          LogMessage(gtr.CurrentGroup.Name + ' - Get Article ' + IntToStr(gtr.CurrentArticleNo) +
            ' failed. Server response: ' + NNTP.LastCmdResult.Code);

        Inc(fCurrentArticleNo);
        Inc(msgNo);
      end;
    finally
      Synchronize(gtr.UpdateArticles);
    end;
  end;

  procedure GetHeaders(fromArticle, dest: Int64);
  var
    i: Integer;
    msgNo: Int64;
    st: RawByteString;
  begin
    LogMessage(gtr.CurrentGroup.Name + ' Get ' + IntToStr(fromArticle) + '-' + IntToStr(dest));
    try
      msgNo := fromArticle;
      while msgNo <= dest do
      begin
        gtr.CurrentArticleNo := msgNo;

        gtr.Header.Clear;
        if NNTP.GetHeader(msgNo, '', gtr.Header) then
        begin
          for i := 0 to gtr.header.Count - 1 do
            gtr.Header[i] := RawStringReplace(gtr.Header[i], ':', '=', []);

          st := RawIntToStr(msgNo) + #9 +
                RawTrim(gtr.Header.Values['Subject']) + #9 +
                RawTrim(gtr.Header.Values['From']) + #9 +
                RawTrim(gtr.Header.Values['Date']) + #9 +
                RawTrim(gtr.Header.Values['Message-ID']) + #9 +
                RawTrim(gtr.Header.Values['References']) + #9 +
                RawTrim(gtr.Header.Values['Bytes']) + #9 +
                RawTrim(gtr.Header.Values['Lines']) + #9 +
                RawTrim(gtr.Header.Values['Xref']);

          TArticlesGetter(getter).Articles.Add(st);
        end;
        Inc(fCurrentArticleNo);
        Inc(msgNo);
      end;
    finally
      gtr.XOverFMT := XOverFMT;
      Synchronize(gtr.UpdateHeaders);
    end;
  end;

begin
  gtr := TArticlesGetter(getter);
  gtr.CurrentArticleNo := -1;

  XOverFMT := GetXOVERFmt;

  requests := gtr.LockList;

  for I := 0 to requests.Count - 1 do
    TArticlesGetterRequest(requests[I]).Retry := False;

  try
    while requests.Count > 0 do
    begin
      ok := True;
      needsRetry := False;
      request := TArticlesGetterRequest(requests[0]);
      try
        try
          UILock;
          try
            fCurrentGetter := nil;

            gtr.CurrentArticleNo := -1;
            gtr.CurrentGroup := request.Group;
            gtr.CurrentFull := request.Full;
            gtr.CurrentUpdateAll := request.FromArticle = 0;

            fromArticle  := request.FromArticle;
            articleCount := request.ArticleCount;

            fCurrentGetter := gtr;
          finally
            UIUnlock;
          end;
        finally
          gtr.UnlockList;
        end;

        // Danger!!! Can't call 'LastArticle' if the articles arent already
        // loaded because it's not thread safe.
        if fromArticle = -2 then
          fromArticle := gtr.CurrentGroup.TSGetLastArticle + 1;

        gtr.Articles.Clear;

        // Select the group on the server
        try
          NNTP.SelectGroup(gtr.CurrentGroup.Name);
        except
          on E: Exception do
          begin
            st := 'GROUP: Error selecting group in thread "' +
                  gtr.CurrentGroup.Name + '" - ' + E.Message;
            LogMessage(st, True);

            if request.Retry then
            begin
              needsRetry := True;
              raise;
            end
            else
            begin
              ok := False;
              request.Retry := True;
            end;
          end;
        end;

        if ok then
        begin
          if fromArticle = -3 then
          begin
            fromArticle := GetFirstArticleNoSince(request.Since);
            if fromArticle < 0 then
              fromArticle := 0;
            articleCount := 0;
          end;

          // Ensure the fromArticle and articleCount are valid for the group.
          if fromArticle = 0 then
            if articleCount > 0 then
            begin
              fromArticle := NNTP.MsgHigh - articleCount + 1;
              if fromArticle < NNTP.MsgLow then
                fromArticle := NNTP.MsgLow;
            end;

          if fromArticle > NNTP.MsgHigh + 1 then
            fromArticle := NNTP.MsgHigh + 1;

          if fromArticle < NNTP.MsgLow then
            fromArticle := NNTP.MsgLow;

          if articleCount > (NNTP.MsgHigh - NNTP.MsgLow) + 1 then
            articleCount := (NNTP.MsgHigh - NNTP.MsgLow) + 1;

          if (fromArticle = 0) or (fromArticle > NNTP.MsgHigh) then
            Continue;

          gtr.CurrentMax := NNTP.MsgHigh;

          if articleCount = 0 then
            articleCount := NNTP.MsgHigh - fromArticle + 1;

          fExpectedArticles := articleCount - 1;
          fCurrentArticleNo := 0;

          try
            // If we've been asked for headers only, and if the server supports it,
            // use XOVER to get the headers. It's quick.
            gtr.CurrentGroup.BeginLock;
            if Assigned(XOverFMT) and (XOverFMT.Count > 0) and not gtr.CurrentFull then
            begin
              fIsXOver := True;
              LogMessage(gtr.CurrentGroup.Name + ' - XOVER ' + IntToStr(fromArticle) + '-' + IntToStr(articleCount));
              GetXOverHeaders(fromArticle, articleCount);
            end
            else
            begin
              // XOver not supported, or we need full articles (not just headers)
              fIsXOver := False;
              if articleCount = 0 then
                dest := NNTP.MsgHigh
              else
              begin
                dest := fromArticle + articleCount - 1;
                if dest > NNTP.MsgHigh then
                  dest := NNTP.MsgHigh;
              end;

              if NNTPaccount.UsePipelining then
              begin
                LogMessage(gtr.CurrentGroup.Name + ' - Pipeline get articles ' + IntToStr(fromArticle) + '-' + IntToStr(dest));
                GetPipelineArticles(fromArticle, dest);
              end
              else
                if gtr.CurrentFull then
                begin
                  LogMessage(gtr.CurrentGroup.Name + ' - Get articles ' + IntToStr(fromArticle) + '-' + IntToStr(dest));
                  GetArticles(fromArticle, dest);
                end
                else
                begin
                  LogMessage(gtr.CurrentGroup.Name + ' - Get headers ' + IntToStr(fromArticle) + '-' + IntToStr(dest));
                  GetHeaders(fromArticle, dest);
                end;
            end;
          except
            LogMessage('Exception in get articles or headers - ' + gtr.CurrentGroup.Name);
            needsRetry := True;
            raise;
          end;
        end;

      finally
        gtr.CurrentGroup.EndLock;
        requests := gtr.LockList;
        if request.Retry then
        begin
          if requests.Count > 0 then
            requests.Move(0, requests.Count - 1);
        end
        else
          if not needsRetry then
            if (requests.Count > 0) and (requests[0] = request) then
              requests.Delete(0);
      end;
    end;
  finally
    UILock;
    fCurrentGetter := nil;
    UIUnlock;
    gtr.UnlockList;
  end;
end;

procedure TNNTPArticlesThread.GetProgressNumbers(group: TServerAccount; var min, max,
  pos: Integer);
begin
  min := 0;
  UILock;
  try
    if Assigned(fCurrentGetter) and ((group = nil) or (fCurrentGetter.CurrentGroup = group)) then
    begin
      max := fExpectedArticles;
      if fIsXOver then
        pos := fCurrentGetter.Articles.Count
      else
        pos := fCurrentArticleNo;
    end
    else
    begin
      max := 0;
      pos := 0;
    end;
  finally
    UIUnlock;
  end;
end;

{ TNNTPArticleThread }

procedure TNNTPArticleThread.DoArticleFailed;
begin
  if Assigned(ThreadManager.OnArticleFailed) then
    ThreadManager.OnArticleFailed(ThreadManager, fFailedArticle);
end;

procedure TNNTPArticleThread.DoPipeLineCommandCancelEvent(
  cmd: TPipelineCommand; startCalled: Boolean);
var
  gtr: TArticleGetter;
  requests: TObjectList;
begin
  if cmd.IsGet then
  begin
    gtr := TArticleGetter(getter);
    requests := gtr.LockList;
    try
      requests.Delete(0);
      Dec(gtr.PipelinePos);
    finally
      gtr.UnlockList;
    end;
    UILock;
    fUICurrentArticle := nil;
    UIUnlock;
    if startCalled then
      Synchronize(gtr.FailArticle)
    else
    begin
      fFailedArticle := TArticle(cmd.Param);
      Synchronize(DoArticleFailed);
      if Assigned(fFailedArticle) then
        fFailedArticle.IsNotOnServer := True;
    end;
  end;
end;

procedure TNNTPArticleThread.DoPipeLineCommandEndEvent(cmd: TPipelineCommand);
var
  gtr: TArticleGetter;
  artNo: Int64;
  requests: TObjectList;
begin
  if cmd.isGet then
  begin
    gtr := TArticleGetter(getter);
    requests := gtr.LockList;
    try
      requests.Delete(0);
      Dec(gtr.PipelinePos);
    finally
      gtr.UnlockList;
    end;

    try
      if gtr.CurrentArticle.ArticleNo = 0 then
      begin
        artNo := cmd.ArticleNo;
        if artNo = 0 then
          gtr.CurrentArticle.FixArticleNo
        else
          gtr.CurrentArticle.ChangeArticleNo(artNo);
      end;
      Synchronize(gtr.GotArticle);
    except
      Windows.Beep(440, 10);
      raise;
    end;
  end;
end;

procedure TNNTPArticleThread.DoPipeLineCommandStartEvent(
  cmd: TPipelineCommand; var headrs: TAnsiStrings; var body: TStream);
var
  gtr: TArticleGetter;
begin
  gtr := TArticleGetter(getter);
  if cmd.Command = gmGroup then
  begin
    if gtr.CurrentGroup.NeedsUpdate then
      Synchronize(gtr.UpdateArticles);
    gtr.CurrentGroup := TSubscribedGroup(cmd.Param);
  end
  else
    if cmd.isGet then
    begin
      UILock;
      try
        gtr.CurrentArticle := TArticle(cmd.Param);
        gtr.CurrentArticle.IsNotOnServer := False;

        if Assigned(gtr.CurrentArticle.Msg) then
        begin
          UIUnlock;
          Synchronize(gtr.ClearArticle);
        end
        else
        begin
          gtr.CurrentArticle.Msg := TmvMessage.Create(gtr.CurrentArticle);
          MessageCacheController.Add(gtr.CurrentArticle);
          UIUnlock;
        end
      except
        UIUnlock;
        raise;
      end;

      Synchronize(gtr.StartArticle);
      headrs := gtr.CurrentArticle.Msg.Header;
      body := gtr.CurrentArticle.Msg.RawData;
      UILock;
      fUICurrentArticle := gtr.CurrentArticle;
      UIUnlock;
    end;
end;

procedure TNNTPArticleThread.DoWork;
var
  request: TArticleGetterRequest;
  requests: TObjectList;
  ok: Boolean;
  gtr: TArticleGetter;
  article: TArticle;
  group: TSubscribedGroup;
begin
  gtr := TArticleGetter(getter);
  gtr.CurrentGroup := nil;

  if (gtr.Account.UsePipelining) and (gtr.Count >= 1) then
  begin
    NNTP.PipelineCommandStartEvent := DoPipelineCommandStartEvent;
    NNTP.PipelineCommandEndEvent := DoPipelineCommandEndEvent;
    NNTP.PipelineCommandAbortEvent := DoPipelineCommandCancelEvent;

    // nb.  It is important to select the first group outside pipelining because
    //      authentication may be required.

    requests := gtr.LockList;
    try
      request := TArticleGetterRequest(requests[0]);
      gtr.CurrentGroup := request.Group;
      gtr.PipelineGroup := request.Group;
    finally
      gtr.UnlockList;
    end;

    NNTP.SelectGroup(gtr.CurrentGroup.Name);

    requests := gtr.LockList;
    try
      repeat
        NNTP.BeginPipeline;
        try
          try
            gtr.PipelinePos := 0;
            while gtr.PipelinePos < gtr.Count do
            begin
              try
                try
                  request := TArticleGetterRequest(requests[gtr.PipelinePos]);
                  Inc(gtr.PipelinePos);
                  article := request.Article;
                  group := request.Group;
                  group.NeedsUpdate := group.NeedsUpdate or request.NeedsFullRefresh;
                finally
                  gtr.UnlockList;
                end;

                if gtr.PipelineGroup <> group then
                begin
                  NNTP.PipelineGroup(group.Name, LPARAM(group));
                  gtr.PipelineGroup := group;
                end;

                NNTP.PipelineGetArticle(article.ArticleNo, article.MessageId, LPARAM(Article));
              finally
                requests := gtr.LockList;
              end;
            end;
          finally
            gtr.UnlockList;
            NNTP.EndPipeline;
            requests := gtr.LockList;
          end;
        except
          if gtr.Locked then
            gtr.UnlockList;
          NNTP.CancelPipeline;
          gtr.LockList;
          raise;
        end;
      until requests.Count = 0;
    finally
      gtr.UnlockList;
      if Assigned(gtr.CurrentGroup) and (gtr.CurrentGroup.NeedsUpdate) then
        Synchronize(gtr.UpdateArticles);
    end;
    Exit;
  end;

  requests := gtr.LockList;
  try
    while requests.Count > 0 do
    try
      try
        UILock;
        fUICurrentArticle := nil;
        UIUnlock;
        request := TArticleGetterRequest(requests[0]);
        article := request.Article;
        gtr.CurrentArticle := article;
        UILock;
        fUICurrentArticle := gtr.CurrentArticle;
        UIUnlock;
        group := request.Group;
        group.NeedsUpdate := group.NeedsUpdate or request.NeedsFullRefresh;
        requests.Delete(0);
      finally
        gtr.UnlockList;
      end;

      if gtr.CurrentGroup <> group then
      begin
        NNTP.SelectGroup(Group.Name);
        gtr.CurrentGroup := Group;
      end;


      if Assigned(gtr.CurrentArticle.Msg) then
        Synchronize(gtr.ClearArticle)
      else
      begin
        gtr.CurrentArticle.Msg := TmvMessage.Create(gtr.CurrentArticle);
        MessageCacheController.Add(gtr.CurrentArticle);
      end;

      Synchronize(gtr.StartArticle);
                  // After calling StartArticle *** MUST *** Call GotArticle or FailArticle
      try
        ok := NNTP.GetArticle(gtr.CurrentArticle.ArticleNo, gtr.CurrentArticle.MessageId, gtr.CurrentArticle.Msg.Header, gtr.CurrentArticle.Msg.RawData);

        // nb - do additional processing to strip out standard headers!!

        if ok then
        begin
          if gtr.CurrentArticle.ArticleNo = 0 then
            if NNTP.MsgNo = 0 then
              gtr.CurrentArticle.FixArticleNo
            else
              gtr.CurrentArticle.ChangeArticleNo(NNTP.MsgNo);
          Synchronize(gtr.GotArticle);
        end
        else
          Synchronize(gtr.FailArticle);
      except
        try
          Synchronize(gtr.FailArticle);
        except
        end;
        raise;
      end
    finally
      requests := gtr.LockList;
    end;
  finally
    UILock;
    fUICurrentArticle := nil;
    UIUnlock;
    gtr.UnlockList;
    if Assigned(gtr.CurrentGroup) and (gtr.CurrentGroup.NeedsUpdate) then
      Synchronize(gtr.UpdateArticles);
  end;
end;

procedure TNNTPArticleThread.GetProgressNumbers(group: TServerAccount; var min, max, pos: Integer);
var
  i, s: Integer;
  p: PAnsiChar;
begin
  min := 0;
  UILock;
  try
    if Assigned(fUICurrentArticle) and
       ((group = nil) or (fUICurrentArticle.Owner = group)) and
       Assigned(fUICurrentArticle.Msg) then
    begin
      if fUICurrentArticle.Bytes > 0 then
      begin
        max := fUICurrentArticle.Bytes;
        pos := fUICurrentArticle.Msg.RawData.Size;
      end
      else
      begin
        max := fUICurrentArticle.Lines;
        pos := 1;
        fUICurrentArticle.Msg.RawData.Lock;
        try
          s := fUICurrentArticle.Msg.RawData.Size;
          p := fUICurrentArticle.Msg.RawData.Memory;
          for i := 0 to s - 1 do
          begin
            if p^ = #13 then Inc(pos);
            Inc(p);
          end;
        finally
          fUICurrentArticle.Msg.RawData.Unlock;
        end;
      end;
    end
    else
    begin
      max := 0;
      pos := 0;
    end;
  finally
    UIUnlock;
  end;
end;

{ TNNTPPoster }

procedure TNNTPPoster.DoWork;
var
  post: TPosterRequest;
  ok: Boolean;
  gtr: TPoster;
  hdr, msg: TAnsiStrings;
  hdrCreated: Boolean;
  multipartBoundary: RawByteString;
  maxLines: Integer;
  requests: TObjectList;

  procedure PostSplitMessage;
  var
    subject:RawByteString;
    st: string;
    n, m, c, x, i: Integer;
    tmsg: TAnsiStrings;
    generateMessageID: Boolean;
  begin
    subject := hdr.Values['Subject'];

    // If a message Id has already been allocated then we must
    // generate a separate message Id for each message part.
    generateMessageID := hdr.Values['Message-ID'] <> '';

    n := 0;
    x := msg.Count div maxLines;
    tmsg := TAnsiStringList.Create;
    try
      c := 0;
      while n < msg.Count do
      begin
        m := n + maxLines;
        if m > msg.Count then
          m := msg.Count;

        tmsg.Clear;
        for i := n to m - 1 do
          tmsg.Add(msg[i]);

        hdr.Values['Subject'] := RawByteString(Format('%s [%d/%d]', [subject, c + 1, x + 1]));
        if (c > 0) and generateMessageID then
        begin
          if (gtr.Account.NNTPSettings.MessageIDDomain = '') or
             (gtr.Account.NNTPSettings.MessageIDDomain = '<Auto>') then
            st := LowerCase(gtr.Account.NNTPServerSettings.ServerName)
          else
            st := gtr.Account.NNTPSettings.MessageIDDomain;
          hdr.Values['Message-ID'] := NewsGlobals.GenerateMessageID('xn', gtr.Account.NNTPSettings.MessageIDStub, st);
        end;

        gtr.UnlockList;
        try
          NNTP.Send(hdr, tmsg);
        finally
          requests := gtr.LockList;
        end;

        Inc(c);
        n := m;
      end;
    finally
      tmsg.Free;
    end;
  end;

begin
  post := nil;
  gtr := TPoster(getter);
  ok := False;
  requests := gtr.LockList;
  try
    while requests.Count > 0 do
    try
      ok := False;
      post := TPosterRequest(requests[0]);
      post.CreateEncodedHeader(hdr, hdrCreated, multipartBoundary);
      try
        post.CreateEncodedMessage(msg, multipartBoundary);
        try
          post.AddAttachments(msg, multipartBoundary);
          maxLines := gtr.Account.PostingSettings.MaxPostLines;
          if (msg.Count > maxLines) and (maxLines >= 100) then
            PostSplitMessage
          else
          begin
            gtr.UnlockList;
            try
              NNTP.Send(hdr, msg);
            finally
              requests := gtr.LockList;
            end;
          end;

          ok := True;
        finally
          msg.Free;
        end;
      finally
        if hdrCreated then
          hdr.Free;
      end;
    finally
      if (requests.Count > 0) and (post = requests[0]) and ok then
        requests.Delete(0)
      else
        LogMessage(Format('Failed posting %s. Count: %d. OK: %s', [post.Subject, requests.Count, BoolToStr(ok, True)]), True);
    end;
  finally
    gtr.UnlockList;
  end;
end;

{ TSMTPMailer }

procedure TSMTPMailer.DoWork;
var
  msg: TidMessage;
  gtr: TEmailer;
  email: TEmailerRequest;
  ok: Boolean;
  i: Integer;
  att: TAttachment;
  account: TMailAccount;
  messages: TObjectList;
begin
  gtr := TEmailer(getter);
  messages := gtr.LockList;
  try
    while messages.Count > 0 do
    begin
      ok := False;
      email := TEMailerRequest(messages[0]);

      msg := TidMessage.Create(nil);
      try
        msg.Recipients.Add.Text := email.MTo;
        if email.MCC <> '' then
          msg.CCList.Add.Text := email.MCC;
        if email.MBCC <> '' then
          msg.BccList.Add.Text := email.MBCC;

        msg.Subject := email.MSubject;

        account := email.MailAccount;
        if Assigned(account) then
        begin
          msg.From.Address := account.Identity.EMailAddress;
          msg.From.Name := account.Identity.UserName;
          if account.Identity.ReplyAddress <> '' then
            if msg.From.Address <> account.Identity.ReplyAddress then
              with msg.ReplyTo.Add do
              begin
                Name := account.Identity.UserName;
                Address := account.Identity.ReplyAddress;
              end;
        end
        else
        begin
          msg.From.Address := email.ArticleContainer.Identity.EMailAddress;
          msg.From.Name := email.ArticleContainer.Identity.UserName;
          if email.ArticleContainer.Identity.ReplyAddress <> '' then
            if msg.From.Address <> email.ArticleContainer.Identity.ReplyAddress then
              with msg.ReplyTo.Add do
              begin
                Name := email.ArticleContainer.Identity.UserName;
                Address := email.ArticleContainer.Identity.ReplyAddress;
              end;
        end;

        msg.Body.Text := email.Msg;

        if email.CodePage <> CP_USASCII then
          msg.CharSet := CodePageToMIMECharsetName(email.CodePage);

        if Assigned(email.Attachments) then
          for I := 0 to email.Attachments.Count - 1 do
          begin
            att := TAttachment(email.Attachments[I]);
            TidAttachmentFile.Create(msg.MessageParts, att.PathName);
          end;

        msg.MsgId := string(GenerateMessageID('XN', '', SMTP.Host));
        msg.ExtraHeaders.Values['Message-Id'] := msg.MsgId;

        msg.References := email.MReplyTo;

        SMTP.MailAgent := ThreadManager.NewsAgent;
        gtr.UnlockList;
        try
          SMTP.Send(msg);
        finally
          messages := gtr.LockList;
        end;

        gtr.CurrentMessage := msg;

        ok := True;
      finally
        msg.Free;
        if (messages.Count > 0) and (email = messages[0]) and ok then
          messages.Delete(0);
      end;
    end;
  finally
    gtr.UnlockList;
  end;
end;

{ TSMTPThread }

constructor TSMTPThread.Create(AGetter: TTCPGetter; ASettings: TServerSettings);
begin
  inherited Create(AGetter, ASettings);
  fClient := TidSMTP.Create(nil);
end;

destructor TSMTPThread.Destroy;
begin
  // TODO: Shouldn't this one be removed (as with NNTP)?
  SMTP.IOHandler := nil;
  inherited Destroy;
end;

procedure TSMTPThread.Execute;
var
  st: string;
  timeout, tm: DWORD;
begin
  if Assigned(Settings) then
  begin
    st := 'SMTP mail Getter Thread for ' + Settings.ServerName;
    timeout := Settings.ServerTimeout;
  end
  else
  begin
    st := 'SMTP mail Getter Thread for ???';
    timeout := 0;
  end;

  SendMessage(Application.MainForm.Handle, WM_NAMETHREAD, ThreadID, LPARAM(PChar(st)));
  while not Terminated do
  begin
    if (State = tsDormant) and SMTP.Connected then
      if timeout = 0 then
        tm := INFINITE
      else
        tm := timeout * 1000
    else
      tm := INFINITE;
    if Trigger.WaitFor(tm) = wrTimeout then
    begin
      SMTP.Disconnect;
      Continue;
    end;

    State := tsBusy;

    if not Terminated then
    try
      if not SMTP.Connected then     // Connect!
      begin
        if ThreadManager.ConnectToInternet(Settings) then
        begin
          SMTP.Host := Settings.ServerName;
          SMTP.Username := Settings.ServerAccountName;
          SMTP.Password := Settings.ServerPassword;
          SMTP.ConnectTimeout := 1000 * Settings.ConnectTimeout;
          SMTP.ReadTimeout := 1000 * Settings.ReadTimeout;
          if (Settings.ServerAccountName <> '') or (Settings.ServerPassword <> '') then
            SMTP.AuthType := satDefault
          else
            SMTP.AuthType := satNone;
          if Settings.SSLrequired then
          begin
            SSLHandler.SSLOptions.Method := sslvTLSv1;
            SMTP.IOHandler := SSLHandler;
            SMTP.Port := Settings.SSLPort;
            SMTP.UseTLS := utUseExplicitTLS;
          end
          else
          begin
            SMTP.UseTLS := utNoTLSSupport;
            SMTP.Port := Settings.ServerPort;
          end;
          SMTP.Connect;
          SMTP.IOHandler.DefStringEncoding := Indy8BitEncoding;
          SMTP.Authenticate;
        end;
      end;

      if not Terminated and SMTP.Connected then
      begin
        Getter.ClearWork;
        DoWork;
        State := tsDone;
        Getter.WorkDone;
        Synchronize(Getter.NotifyUI);
        Getter.ClearWork;
      end;

      State := tsDormant;
    except
      on e: Exception do
      begin
        fLastError := e.Message;
        if not gAppTerminating then
          Synchronize(NotifyError);
        try
          SMTP.Disconnect;
        except
        end;
        try
          ThreadManager.GetCurrentConnectoid
        except
        end;
        State := tsPending;

        if e is EIdReplyRFCError then
          if EIdReplyRFCError(e).ErrorCode = 503 then      // Retry immediately with
                                                           // 503 errors - otherwise wait
                                                           // for the user to retrigger
            if not gAppTerminating then
              Synchronize(ThreadManager.JogThreads);

        if e is EidSocketError then
          if (EidSocketError(e).LastError = 0) or (EidSocketError(e).LastError = 10054) then
            if not gAppTerminating then
              Synchronize(ThreadManager.JogThreads);

        if not (e is EIdException) then
          fLastError := '';
      end;
    end;
  end;
end;

function TSMTPThread.GetClient: TidSMTP;
begin
  Result := TidSMTP(Client);
end;

function TSMTPThread.GetSettings: TSMTPServerSettings;
begin
  Result := TSMTPServerSettings(fSettings);
end;

end.
