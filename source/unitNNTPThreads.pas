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

uses Windows, Classes, SysUtils, Forms, syncobjs, IdNNTPX, ConTnrs, WinINet, unitNNTPServices,IdTCPClient, unitNewsThread, IdSSLOpenSSL, IdSMTP, IdMessage, unitSettings;

type
//---------------------------------------------------
// TNNTPThread - base class for NNTP worker threads

TNNTPThread = class (TTCPThread)
private
  fNNTPAccount: TNNTPAccount;
  fGreet : string;

  function GetClient: TidNNTPX;
  procedure CheckNewGroups;
  procedure DoNotifyNewGroups;
  function GetSettings: TNNTPServerSettings;
  procedure SetGreeting;

protected
  procedure Execute; override;
public
  constructor Create (AGetter : TTCPGetter; ASettings : TServerSettings); override;
  destructor Destroy; override;
  property NNTPAccount : TNNTPAccount read fNNTPAccount write fNNTPAccount;
  property NNTP : TidNNTPX read GetClient;
  property ServerSettings : TNNTPServerSettings read GetSettings;
end;

TSMTPThread = class (TTCPThread)
private
  function GetClient: TidSMTP;
  function GetSettings: TSMTPServerSettings;

protected
   procedure Execute; override;
public
  constructor Create (AGetter : TTCPGetter; ASettings : TServerSettings); override;
  destructor Destroy; override;
  property SMTP : TidSMTP read GetClient;
  property Settings : TSMTPServerSettings read GetSettings;
end;

TNNTPThreadClass = class of TNNTPThread;

TNNTPNewsgroupThread = class (TNNTPThread)
protected
  procedure DoWork; override;
end;

TNNTPArticlesThread = class (TNNTPThread)
private
  fExpectedArticles : Integer;
  fIsXOver : boolean;
  fCurrentArticleNo : Integer;     // For status bar - 0..fExpectedArticles
  fCurrentGetter : TArticlesGetter;
  procedure DoPipeLineCommandStartEvent (cmd : TPipelineCommand; var headrs : TStrings; var body : TStream);
  procedure DoPipeLineCommandEndEvent (cmd : TPipelineCommand);
  procedure DoPipeLineCommandCancelEvent (cmd : TPipelineCommand; startCalled : boolean);
protected
  procedure DoWork; override;
public
  procedure GetProgressNumbers (group : TServerAccount; var min, max, pos : Integer); override;
end;

TNNTPArticleThread = class (TNNTPThread)
private
  fUICurrentArticle : TArticle;
  procedure DoPipeLineCommandStartEvent (cmd : TPipelineCommand; var headrs : TStrings; var body : TStream);
  procedure DoPipeLineCommandEndEvent (cmd : TPipelineCommand);
  procedure DoPipeLineCommandCancelEvent (cmd : TPipelineCommand; startCalled : boolean);
protected
  procedure DoWork; override;
public
  procedure GetProgressNumbers (group : TServerAccount; var min, max, pos : Integer); override;
end;

TNNTPPoster = class (TNNTPThread)
protected
  procedure DoWork; override;
end;

TSMTPMailer = class (TSMTPThread)
protected
  procedure DoWork; override;
end;

implementation

uses IdException, unitNNTPThreadManager, unitNewsReaderOptions, unitMessages, unitCharsetMap, unitMailServices, unitLog, NewsGlobals, IdGlobal;

{ TNNTPThread }

procedure TNNTPThread.CheckNewGroups;
var
  FileName : string;
  dt : TDateTime;
  groups, newGroups : TStringList;
  i : Integer;
begin
  if NNTPAccount.CheckedNewGroups then Exit;

  newGroups := Nil;
  groups := Nil;
  try
    FileName := gMessageBaseRoot + '\' + NNTPAccount.AccountName + '\newsgroups.dat';
    if FileAge (fileName, dt) then
    begin

      newGroups := TStringList.Create;

      try
        NNTP.GetNewGroupsList(dt, True, '', newGroups);
      except
      end;

      if newGroups.Count > 0 then
      begin
        groups := TStringList.Create;
        groups.LoadFromFile(fileName);

        groups.Sorted := True;
        groups.Duplicates := dupIgnore;

        for i := 0 to newGroups.Count - 1 do
          if groups.IndexOf(newGroups [i]) = -1 then
            groups.Add(newGroups [i] + ' *');

        groups.SaveToFile(fileName);

        Synchronize (DoNotifyNewGroups)
      end;
      NNTPAccount.CheckedNewGroups := True;
    end
  finally
    newGroups.Free;
    groups.Free
  end
end;

constructor TNNTPThread.Create(AGetter : TTCPGetter; ASettings : TServerSettings);
begin
  inherited Create (AGetter, ASettings);
  fClient := TidNNTPX.Create (Nil);
  TidNNTPX (fClient).ReadLnDelay := gReadLnDelay;
end;

destructor TNNTPThread.Destroy;
begin
  NNTP.IOHandler := nil;
  inherited;
end;

procedure TNNTPThread.DoNotifyNewGroups;
begin
  if Assigned (ThreadManager.OnNotifyNewGroups) then
    ThreadManager.OnNotifyNewGroups (self, NNTPAccount)
end;

(*----------------------------------------------------------------------*
 | TNNTPThread.Execute                                                  |
 |                                                                      |
 | Execute handler for an NNTP thread.  Wait to be triggered, connect   |
 | and do the work.                                                     |
 *----------------------------------------------------------------------*)
procedure TNNTPThread.Execute;
var
  st : string;
  oldLogFlag : boolean;
  timeout, tm : DWORD;
begin
  if Assigned (NNTPAccount) then
  begin
    st := 'NNTP Getter Thread for ' + NNTPAccount.AccountName;
    timeout := NNTPAccount.NNTPServerSettings.ServerTimeout
  end
  else
  begin
    timeout := 0;
    st := 'NNTP Getter Thread for ???';
  end;

  SendMessage (Application.MainForm.Handle, WM_NAMETHREAD, ThreadID, Integer (PChar (st)));
  while not Terminated do
  try
    if (State = tsDormant) and NNTP.Connected then
      if timeout = 0 then
        tm := INFINITE
      else
        tm := timeout * 1000
    else
      tm := INFINITE;
    if Trigger.WaitFor(tm) = wrTimeout then
    begin
      NNTP.Disconnect;
      continue
    end;

    State := tsBusy;

    if not Terminated then
    try
      if not NNTP.Connected then     // Connect!
      begin
        if ThreadManager.ConnectToInternet (ServerSettings) then
        begin
          NNTP.Host := ServerSettings.ServerName;
          NNTP.UserName := ServerSettings.ServerAccountName;
          NNTP.Password := ServerSettings.ServerPassword;
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
          fGreet := NNTP.Greeting.Text.Text;
          Synchronize (SetGreeting);
          if ServerSettings.AlwaysAuthenticate then
            NNTP.Authenticate;
        end
      end;

      if not Terminated and NNTP.Connected then
      begin
        if not NNTPAccount.CheckedNewGroups then
          CheckNewGroups;

        Getter.ClearWork;
        DoWork;
        State := tsDone;
        Getter.WorkDone;
        Synchronize (Getter.NotifyUI);
        Getter.ClearWork
      end;

      State := tsDormant;
      if not gAppTerminating then
        Synchronize (ThreadManager.JogThreads);
    except
      on e : Exception do
      begin
        fLastError := e.Message;
        State := tsPending;

        try
          st := 'Error in thread "' + Getter.GetterRootText + '" - ' + fLastError
        except
          st := 'Error in rogue getter - ' + fLastError
        end;

        oldLogFlag := gLogFlag;
        gLogFlag := True;
        try
          try
            LogMessage (st);
          except
          end
        finally
          gLogFlag := oldLogFlag
        end;

        if not gAppTerminating then
          Synchronize (NotifyError);

        try
          NNTP.DisconnectSocket;
        except
        end;

        if not gAppTerminating then
        try
          ThreadManager.GetCurrentConnectoid
        except
        end;

        try
          if Getter is TMultiNewsGetter and (TMultiNewsGetter (Getter).Count = 0) then
            State := tsDone
          else
            State := tsPending;
        except
          State := tsPending
        end;

        if not gAppTerminating then
        begin
          if e is EIdConnClosedGracefully then
            Synchronize (ThreadManager.JogThreads);

          if e is EIdProtocolReplyError then
            if (EIdProtocolReplyError (e).ReplyErrorCode = 503) or (EIdProtocolReplyError (e).ReplyErrorCode = 400) then
                                                                        // Retry immediately with
                                                                        // 503 & 400 errors - otherwise wait
                                                                        // for the user to retrigger
                                                                        // nb.  503 = Server Fault
                                                                        //      400 = Server disconnected (eg. timeout)
                Synchronize (ThreadManager.JogThreads);

          if e is EidSocketError then
            if (EidSocketError (e).LastError = 0) or (EidSocketError (e).LastError = 10054) then
              Synchronize (ThreadManager.JogThreads)
        end;

        if not (e is EIdException) then
          fLastError := '';
      end
    end
  except
  end
end;

function TNNTPThread.GetClient: TidNNTPX;
begin
  Result := TidNNTPx (Client);
end;

function TNNTPThread.GetSettings: TNNTPServerSettings;
begin
  result := TNNTPServerSettings (fSettings);
end;

procedure TNNTPThread.SetGreeting;
begin
  fNNTPAccount.Greeting := fGreet;
end;

{ TNNTPNewsgroupThread }

procedure TNNTPNewsgroupThread.DoWork;
begin
  NNTP.GetNewsgroupList(TNewsgroupGetter (getter).fNewsgroups);
end;

{ TNNTPArticlesThread }

(*----------------------------------------------------------------------*
 | TNNTPArticlesThread.DoWork                                           |
 |                                                                      |
 | Get article range.  Headers only or headers & bodies.                |
 *----------------------------------------------------------------------*)

procedure TNNTPArticlesThread.DoPipeLineCommandCancelEvent(
  cmd: TPipelineCommand; startCalled : boolean);
begin
  Inc (fCurrentArticleNo)
end;

procedure TNNTPArticlesThread.DoPipeLineCommandEndEvent(
  cmd: TPipelineCommand);
var
  gtr : TArticlesGetter;
  st : string;
  i : Integer;
begin
  gtr := TArticlesGetter (cmd.Param);
  if gtr.CurrentFull then
  begin
    LogMessage (gtr.CurrentGroup.Name + ' - Sync ' + IntToStr (gtr.CurrentArticleNo));
    Synchronize (gtr.SaveCurrentArticle)
  end
  else
  begin
                    // Wanted header only - but can't do XOVER (server doesn't support it).
                    //
                    // Build a string containing headers in XOVER fmt

    for i := 0 to gtr.header.Count - 1 do
      gtr.Header [i] := StringReplace (gtr.Header [i], ':', '=', []);

    st := IntToStr (cmd.ArticleNo) + #9 +
          Trim (gtr.Header.Values ['Subject']) + #9 +
          Trim (gtr.Header.Values ['From']) + #9 +
          Trim (gtr.Header.Values ['Date']) + #9 +
          Trim (gtr.Header.Values ['Message-ID']) + #9 +
          Trim (gtr.Header.Values ['References']) + #9 +
          Trim (gtr.Header.Values ['Bytes']) + #9 +
          Trim (gtr.Header.Values ['Lines']) + #9 +
          Trim (gtr.Header.Values ['Xref']);

    TArticlesGetter (getter).Articles.Add(st)
  end;
  Inc (fCurrentArticleNo)
end;

procedure TNNTPArticlesThread.DoPipeLineCommandStartEvent(
  cmd: TPipelineCommand; var headrs: TStrings; var body: TStream);
var
  gtr : TArticlesGetter;
begin
  gtr := TArticlesGetter (cmd.Param);
  gtr.CurrentArticleNo := cmd.ArticleNo;
  headrs := gtr.Header;
  body := gtr.Body
end;

procedure TNNTPArticlesThread.DoWork;
var
  XOverFMT : TStringList;
  fromArticle, articleCount, dest: Integer;
  request : TArticlesGetterRequest;
  gtr : TArticlesGetter;
  requests : TObjectList;
  needsRetry : boolean;

  function GetXOverFMT : TStringList;
  begin
    result := NNTPAccount.XOverFMT;

    if not Assigned (result) and not NNTPAccount.NoXNews then       // Get overview.fmt list
    begin                                 //
      result := TStringList.Create;
      try
        try
          NNTP.GetOverviewFMT(result);
        except
          on e : EIdProtocolReplyError do
            case e.ReplyErrorCode of
              500, 501 :;                 // overview.fmt not supported.  Not an
                                          // error - just a lousy news server.  We'll
                                          // have to get headers one by one.
              else
                raise
            end
          else
            raise;
        end;

        NNTPAccount.XOverFMT := result
      finally
        result.Free
      end
    end;

    result := NNTPAccount.XOverFMT;
  end;
  function GetFirstArticleNoSince (date : TDateTime) : Integer;
  var
    nearest : Integer;
    head : TStrings;
    dtst : string;
    dt : TDateTime;

    function CmpDate (d1, d2 : TDateTime) : Integer;
    var
      diff : TDateTime;
    begin
      diff := d2 - d1;

      if Abs (diff) < 0.000000001 then
        result := 0
      else
        if diff > 0 then
          result := 1
        else
          result := -1
    end;

    function NNTPSearch (s, e : Integer) : Integer;
    var
      n : Integer;
    begin
      if e >= s then
      begin
        nearest := s + (e - s) div 2;
        n := nearest;
        while not NNTP.GetHeader(nearest+1, '', head) do // while article missing;
        begin
          Inc (nearest);
          if nearest = e then
          begin
            result := -2;
            exit
          end
        end;

        dtst := Trim (head.Values ['Date']);
        if dtst <> '' then
          dt :=  GMTToLocalDateTime (dtst)
        else
          dt := 0.0;

        if CmpDate (dt, 0.0) = 0 then
        begin
          result := -2;
          exit;
        end;

        case CmpDate (dt, date) of
          -1 : result := NNTPSearch (s, n - 1);
           1 : result := NNTPSearch (nearest + 1, e);
           else
             result := nearest
        end
      end
      else
        result := -1;
    end;

  begin
    head := TStringList.Create;
    try
      head.NameValueSeparator := ':';
      result := NNTPSearch (NNTP.MsgLow - 1, NNTP.MsgHigh - 1);
      if result = -2 then Exit; // Internal error.

      if result = -1 then
      begin
        result := nearest;
        if result > 0 then
          Dec (result)
      end
    finally
      head.Free
    end
  end;

  procedure GetXOverHeaders (fromArticle, articleCount : Integer);
  begin
    LogMessage (gtr.CurrentGroup.Name + ' XOVER');
    try
      if articleCount = 0 then
        NNTP.SendXOVER(IntToStr (fromArticle) + '-', gtr.Articles)
      else
        NNTP.SendXOVER(IntToStr (fromArticle) + '-' + IntToStr (fromArticle + articleCount - 1), gtr.Articles);
    finally
      gtr.XOverFMT := XOverFmt;
      Synchronize (gtr.UpdateHeaders)
    end
  end;

  procedure GetPipelineArticles (fromArticle, dest : Integer);
  var
    msgNo : Integer;
  begin
    LogMessage (gtr.CurrentGroup.Name + ' Pipeline get ' + IntToStr (fromArticle) + '-' + IntToStr (dest));
    NNTP.PipelineCommandStartEvent := DoPipelineCommandStartEvent;
    NNTP.PipelineCommandEndEvent := DoPipelineCommandEndEvent;
    NNTP.PipelineCommandAbortEvent := DoPipelineCommandCancelEvent;
    if fromArticle <= dest then
    try

      NNTP.BeginPipeline;
      try
        for msgNo := fromArticle to dest do
        begin
          if gtr.CurrentFull then
            NNTP.PipelineGetArticle(msgNo, '', Integer (gtr))
          else
            NNTP.PipelineGetHeader(msgNo, '', Integer (gtr))
        end;
      finally
        NNTP.EndPipeline
      end
    finally
      if gtr.CurrentFull then
        Synchronize (gtr.UpdateArticles)
      else
      begin
        gtr.XOverFMT := XOverFMT;
        Synchronize (gtr.UpdateHeaders)
      end
    end
  end;

  procedure GetArticles (fromArticle, dest : Integer);
  var
    msgNo : Integer;

  begin
    LogMessage (gtr.CurrentGroup.Name + ' Get ' + IntToStr (fromArticle) + '-' + IntToStr (dest));
    try
      for msgNo := fromArticle to dest do
      begin
        gtr.CurrentArticleNo := msgNo;

        gtr.Header.Clear;
        gtr.Body.Clear;
        if  NNTP.GetArticle (msgNo, '', gtr.Header, gtr.Body) then
        begin
          LogMessage (gtr.CurrentGroup.Name + ' - Sync ' + IntToStr (gtr.CurrentArticleNo));
          Synchronize (gtr.SaveCurrentArticle)
        end;

        Inc (fCurrentArticleNo)
      end
    finally
      Synchronize (gtr.UpdateArticles)
    end
  end;

  procedure GetHeaders (fromArticle, dest : Integer);
  var
    msgNo, i : Integer;
    st : string;
  begin
    LogMessage (gtr.CurrentGroup.Name + ' Get ' + IntToStr (fromArticle) + '-' + IntToStr (dest));
    try
      for msgNo := fromArticle to dest do
      begin
        gtr.CurrentArticleNo := msgNo;

        gtr.Header.Clear;
        if NNTP.GetHeader(msgNo, '', gtr.Header) then
        begin
          for i := 0 to gtr.header.Count - 1 do
            gtr.Header [i] := StringReplace (gtr.Header [i], ':', '=', []);

          st := IntToStr (msgNo) + #9 +
                Trim (gtr.Header.Values ['Subject']) + #9 +
                Trim (gtr.Header.Values ['From']) + #9 +
                Trim (gtr.Header.Values ['Date']) + #9 +
                Trim (gtr.Header.Values ['Message-ID']) + #9 +
                Trim (gtr.Header.Values ['References']) + #9 +
                Trim (gtr.Header.Values ['Bytes']) + #9 +
                Trim (gtr.Header.Values ['Lines']) + #9 +
                Trim (gtr.Header.Values ['Xref']);

          TArticlesGetter (getter).Articles.Add(st)
        end;
        Inc (fCurrentArticleNo)
      end
    finally
      gtr.XOverFMT := XOverFMT;
      Synchronize (gtr.UpdateHeaders)
    end
  end;

begin
  gtr := TArticlesGetter (getter);
  gtr.CurrentArticleNo := -1;

  XOverFMT := GetXOVERFmt;

  requests := gtr.LockList;
  try
    while requests.Count > 0 do         // Group list will contain 1 or more
                                        // TArticleGetterRequest requests
    begin
      needsRetry := False;
      request := TArticlesGetterRequest (requests [0]);
      try
        try
          UILock;
          try
            fCurrentGetter := Nil;

            gtr.CurrentArticleNo := -1;
            gtr.CurrentGroup := request.Group;
            gtr.CurrentFull := request.Full;
            gtr.CurrentUpdateAll := request.FromArticle = 0;

            fromArticle := request.FromArticle;

            articleCount := request.ArticleCount;

            fCurrentGetter := gtr;
          finally
            UIUnlock
          end
        finally
          gtr.UnlockList
        end;

        // Danger!!! Can't call 'LastArticle' if the articles arent already
        // loaded because it's not thread safe.

        if fromArticle = -2 then
          fromArticle := gtr.CurrentGroup.TSGetLastArticle + 1;
                                            // Select the group on the server
        gtr.Articles.Clear;

        try
          NNTP.SelectGroup(gtr.CurrentGroup.Name);
        except
          try
            LogMessage ('Exception in SelectGroup - ' + gtr.CurrentGroup.Name);
          except
          end;
          needsRetry := True;
          raise
        end;

        if fromArticle = -3 then
        begin
          fromArticle := GetFirstArticleNoSince (request.Since);
          if fromArticle < 0 then
            fromArticle := 0;
          articleCount := 0;
        end;

                                            // Ensure the fromArticle and articleCount
                                            // are valid for the group.
        if fromArticle = 0 then
          if articleCount > 0 then
          begin
            fromArticle := Integer (NNTP.MsgHigh) - articleCount + 1;
            if fromArticle < Integer (NNTP.MsgLow) then
             fromArticle := Integer (NNTP.MsgLow)
          end;

        if fromArticle > Integer (NNTP.MsgHigh) + 1 then
          fromArticle := NNTP.MsgHigh + 1;

        if fromArticle < Integer (NNTP.MsgLow) then
          fromArticle := NNTP.MsgLow;

        if articleCount > (Integer (NNTP.MsgHigh) - Integer (NNTP.MsgLow)) + 1 then
          articleCount := (Integer (NNTP.MsgHigh) - Integer (NNTP.MsgLow)) + 1;

        if (fromArticle = 0) or (fromArticle > Integer (NNTP.MsgHigh)) then
          Continue;

                                            // If we've been asked for headers only,
                                            // and if the server supports it, use XOVER
                                            // to get the headers.  It's quick.
        gtr.CurrentMax := NNTP.MsgHigh;

        if articleCount = 0 then
          fExpectedArticles := Integer (NNTP.MsgHigh) - fromArticle
        else
          fExpectedArticles := articleCount;
        fCurrentArticleNo := 0;

        try
          gtr.CurrentGroup.BeginLock;
          if Assigned (XOverFMT) and (XOverFMT.Count > 0) and not gtr.CurrentFull then
          begin
            fIsXOver := True;
            LogMessage (gtr.CurrentGroup.Name + ' - XOVER ' + IntToStr (fromArticle) + '-' + IntToStr (articleCount));
            GetXOverHeaders (fromArticle, articleCount)
          end
          else

          begin                               // XOver not supported, or we need full
                                              // articles (not just headers)

            fIsXOver := False;
            if articleCount = 0 then
              dest := NNTP.MsgHigh
            else
            begin
              dest := fromArticle + articleCount - 1;
              if dest > Integer (NNTP.MsgHigh) then
                dest := NNTP.MsgHigh
            end;

            if NNTPaccount.UsePipelining then
            begin
              LogMessage (gtr.CurrentGroup.Name + ' - Pipeline get articles ' + IntToStr (fromArticle) + '-' + IntToStr (dest));
              GetPipelineArticles (fromArticle, dest)
            end
            else
              if gtr.CurrentFull then
              begin
                LogMessage (gtr.CurrentGroup.Name + ' - Get articles ' + IntToStr (fromArticle) + '-' + IntToStr (dest));
                GetArticles (fromArticle, dest)
              end
              else
              begin
                LogMessage (gtr.CurrentGroup.Name + ' - Get headers ' + IntToStr (fromArticle) + '-' + IntToStr (dest));
                GetHeaders (fromArticle, dest)
              end
          end
        except
          try
            LogMessage ('Exception in get articles or headers - ' + gtr.CurrentGroup.Name);
          except
          end;
          needsRetry := True;
          raise
        end;


      finally
        gtr.CurrentGroup.EndLock;
        requests := gtr.LockList;
        if not needsRetry then
          if (requests.Count > 0) and (requests [0] = request) then
            requests.Delete(0);
      end
    end
  finally
    UILock;
    fCurrentGetter := Nil;
    UIUnlock;
    gtr.UnlockList;
  end
end;

{ TNNTPArticleThread }

procedure TNNTPArticleThread.DoPipeLineCommandCancelEvent(
  cmd: TPipelineCommand; startCalled : boolean);
var
  gtr : TArticleGetter;
  requests : TObjectList;
  art : TArticle;

begin
  if cmd.IsGet then
  begin
    gtr := TArticleGetter (getter);
    requests := gtr.LockList;
    try
      requests.Delete (0);
      Dec (gtr.PipelinePos);
    finally
      gtr.UnlockList
    end;
    UILock;
    fUICurrentArticle := Nil;
    UIUnlock;
    if startCalled then
      synchronize (gtr.FailArticle)
    else
    begin
      art := TArticle (cmd.Param);

      if Assigned (ThreadManager.OnArticleFailed) then
        ThreadManager.OnArticleFailed (ThreadManager, art);

      if Assigned (art) then
        art.IsNotOnServer := True
    end
  end
end;

procedure TNNTPArticleThread.DoPipeLineCommandEndEvent(
  cmd: TPipelineCommand);
var
  gtr : TArticleGetter;
  artNo : Integer;
  requests : TObjectList;
begin
  if cmd.isGet then
  begin

    gtr := TArticleGetter (getter);
    requests := gtr.LockList;
    try
      requests.Delete(0);
      Dec (gtr.PipelinePos);
    finally
      gtr.UnlockList
    end;

    try
      if gtr.CurrentArticle.ArticleNo = 0 then
      begin
        artNo := cmd.ArticleNo;
        if artNo = 0 then
          gtr.CurrentArticle.FixArticleNo
        else
          gtr.CurrentArticle.ChangeArticleNo (artNo)
      end;
      synchronize (gtr.GotArticle)
    except
      Windows.Beep (440, 10);
      raise;
    end
  end
end;

procedure TNNTPArticleThread.DoPipeLineCommandStartEvent(
  cmd: TPipelineCommand; var headrs: TStrings; var body: TStream);
var
  gtr : TArticleGetter;
begin
  gtr := TArticleGetter (getter);
  if cmd.Command = gmGroup then
  begin
    if gtr.CurrentGroup.NeedsUpdate then
      Synchronize (gtr.Update);
    gtr.CurrentGroup := TSubscribedGroup (cmd.Param)
  end
  else
    if cmd.isGet then
    begin
      UILock;
      try
        gtr.CurrentArticle := TArticle (cmd.Param);
        gtr.CurrentArticle.IsNotOnServer := False;

        if Assigned (gtr.CurrentArticle.Msg) then
        begin
          UIUnlock;
          synchronize (gtr.ClearArticle)
        end
        else
        begin
          gtr.CurrentArticle.Msg := TmvMessage.Create (gtr.CurrentArticle);
          MessageCacheController.Add(gtr.CurrentArticle);
          UIUnlock
        end
      except
        UIUnlock;
        raise
      end;

      synchronize (gtr.StartArticle);
      headrs := gtr.CurrentArticle.Msg.Header;
      body := gtr.CurrentArticle.Msg.RawData;
      UILock;
      fUICurrentArticle := gtr.CurrentArticle;
      UIUnlock;
    end
end;

procedure TNNTPArticleThread.DoWork;
var
  request : TArticleGetterRequest;
  requests : TObjectList;
  ok : boolean;
  gtr : TArticleGetter;
  article : TArticle;
  group : TSubscribedGroup;


begin
  gtr := TArticleGetter (getter);
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
      request := TArticleGetterRequest (requests [0]);
      gtr.CurrentGroup := request.Group;
      gtr.PipelineGroup := request.Group;
    finally
      gtr.UnlockList
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
                  request := TArticleGetterRequest (requests [gtr.PipelinePos]);
                  Inc (gtr.PipelinePos);
                  article := request.Article;
                  group := request.Group;
                  group.NeedsUpdate := request.NeedsFullRefresh;
                finally
                  gtr.UnlockList
                end;

                if gtr.PipelineGroup <> group then
                begin
                  NNTP.PipelineGroup (group.Name, Integer (group));
                  gtr.PipelineGroup := group
                end;

                NNTP.PipelineGetArticle(article.ArticleNo, article.MessageId, Integer (Article));
              finally
                requests := gtr.LockList
              end
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
          raise
        end
      until requests.Count = 0
    finally
      gtr.UnlockList;
      if Assigned (gtr.CurrentGroup) and (gtr.CurrentGroup.NeedsUpdate) then
        Synchronize (gtr.Update);
    end;
    exit;
  end;

  requests := gtr.LockList;
  try
    while requests.Count > 0 do
    try
      try
        UILock;
        fUICurrentArticle := Nil;
        UIUnlock;
        request := TArticleGetterRequest (requests [0]);
        article := request.Article;
        gtr.CurrentArticle := article;
        UILock;
        fUICurrentArticle := gtr.CurrentArticle;
        UIUnlock;
        group := request.Group;
        group.NeedsUpdate := request.NeedsFullRefresh;
        requests.Delete(0);
      finally
        gtr.UnlockList
      end;

      if gtr.CurrentGroup <> group then
      begin
        NNTP.SelectGroup(Group.Name);
        gtr.CurrentGroup := Group
      end;


      if Assigned (gtr.CurrentArticle.Msg) then
        synchronize (gtr.ClearArticle)
      else
      begin
        gtr.CurrentArticle.Msg := TmvMessage.Create (gtr.CurrentArticle);
        MessageCacheController.Add(gtr.CurrentArticle);
      end;

      synchronize (gtr.StartArticle);
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
              gtr.CurrentArticle.ChangeArticleNo (NNTP.MsgNo);
          synchronize (gtr.GotArticle)
        end
        else
          synchronize (gtr.FailArticle);
      except
        try
          synchronize (gtr.FailArticle);
        except
        end;
        raise
      end
    finally
      requests := gtr.LockList
    end;
  finally
    UILock;
    fUICurrentArticle := Nil;
    UIUnlock;
    gtr.UnlockList;
    if Assigned (gtr.CurrentGroup) and (gtr.CurrentGroup.NeedsUpdate) then
      Synchronize (gtr.Update);

  end
end;

procedure TNNTPArticleThread.GetProgressNumbers(group : TServerAccount; var min, max, pos: Integer);
var
  i, s : Integer;
  p : PChar;
begin
  min := 0;
  UILock;
  try
    if Assigned (fUICurrentArticle) and
       ((group = Nil) or (fUICurrentArticle.Owner = group)) and
       Assigned (fUICurrentArticle.Msg) then
    begin
      if fUICurrentArticle.Bytes > 0 then
      begin
        max := fUICurrentArticle.Bytes;
        pos := fUICurrentArticle.Msg.RawData.Size
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
            if p^ = #13 then Inc (pos);
            Inc (p);
          end
        finally
          fUICurrentArticle.Msg.RawData.Unlock
        end
      end
    end
    else
    begin
      max := 0;
      pos := 0
    end
  finally
    UIUnlock
  end
end;

{ TNNTPPoster }

procedure TNNTPPoster.DoWork;
var
  post : TPosterRequest;
  ok : boolean;
  gtr : TPoster;
  hdr, msg : TStrings;
  hdrCreated : boolean;
  multipartBoundary : string;
  maxLines : Integer;
  requests : TObjectList;

  procedure PostSplitMessage;
  var
    subject, st : string;
    n, m, c, x, i : Integer;
    tmsg : TStrings;
    generateMessageID : boolean;
  begin
    subject := hdr.Values ['Subject'];

    // If a message Id has already been allocated then we must
    // generate a separate message Id for each message part.
    generateMessageID := hdr.Values ['Message-ID'] <> '';

    n := 0;
    x := msg.Count div maxLines;
    tmsg := TStringList.Create;
    try
      c := 0;
      while n < msg.Count do
      begin
        m := n + maxLines;
        if m > msg.Count then
          m := msg.Count;

        tmsg.Clear;
        for i := n to m - 1 do
          tmsg.Add(msg [i]);

        hdr.Values ['Subject'] := Format ('%s [%d/%d]', [subject, c + 1, x + 1]);
        if (c > 0) and generateMessageID then
        begin
          if (gtr.Account.NNTPSettings.MessageIDDomain = '') or
             (gtr.Account.NNTPSettings.MessageIDDomain = '<Auto>') then
            st := LowerCase (gtr.Account.NNTPServerSettings.ServerName)
          else
            st := gtr.Account.NNTPSettings.MessageIDDomain;
          hdr.Values ['Message-ID'] := NewsGlobals.GenerateMessageID ('xn',gtr.Account.NNTPSettings.MessageIDStub, st)
        end;
        NNTP.Send (hdr, tmsg);

        Inc (c);
        n := m
      end
    finally
      tmsg.Free
    end
  end;

begin
  gtr := TPoster (getter);
  requests := gtr.LockList;
  post := Nil;
  ok := False;
  try
    while requests.Count > 0 do
    try
      try
        try
          ok := False;
          post := TPosterRequest (requests [0]);
          post.CreateEncodedHeader(hdr, hdrCreated, multipartBoundary);
          maxLines := gtr.Account.PostingSettings.MaxPostLines;
          post.CreateEncodedMessage(msg, multipartBoundary);
          post.AddAttachments (msg, multipartBoundary);
        finally
          gtr.UnlockList
        end;

        if (msg.Count > maxLines) and (maxLines >= 100) then
          PostSplitMessage
        else
          NNTP.Send (hdr, msg);
        ok := True
      finally
        msg.Free;

        if hdrCreated then
          hdr.Free;
      end
    finally
      requests := gtr.LockList;
      if (requests.Count > 0) and (post = requests [0]) and ok then
        requests.Delete(0);
    end
  finally
    gtr.UnlockList
  end
end;

{ TSMTPThread }

constructor TSMTPThread.Create(AGetter: TTCPGetter; ASettings : TServerSettings);
begin
  inherited Create (AGetter, ASettings);
  fClient := TidSMTP.Create (Nil);
end;

destructor TSMTPThread.Destroy;
begin
  SMTP.IOHandler := nil;
  inherited;
end;

procedure TSMTPThread.Execute;
var
  st : string;
begin
  if Assigned (settings) then
    st := 'SMTP mail Getter Thread for ' + Settings.ServerName
  else
    st := 'SMTP mail Getter Thread for ???';

  SendMessage (Application.MainForm.Handle, WM_NAMETHREAD, ThreadID, Integer (PChar (st)));
  while not Terminated do
  begin
    Trigger.WaitFor(INFINITE);

    State := tsBusy;

    try
      if not Terminated then
      try
        if not SMTP.Connected then     // Connect!
        begin
          if ThreadManager.ConnectToInternet (Settings) then
          begin
            SMTP.Host := Settings.ServerName;
            SMTP.Username := Settings.ServerAccountName;
            SMTP.Password := Settings.ServerPassword;
            if (Settings.ServerAccountName <> '') or (settings.ServerPassword <> '') then
              SMTP.AuthenticationType := atLogin;
            if Settings.SSLrequired then
            begin
              SMTP.IOHandler := SSLHandler;
              SMTP.Port := Settings.SSLPort;
            end
            else
              SMTP.Port := Settings.ServerPort;
            SMTP.Connect;
            if Settings.SSLRequired then
            begin
              SMTP.SendCmd('STARTTLS', 220);
            end;
            if (SMTP.AuthenticationType <> atNone) then
              SMTP.Authenticate;
          end
        end;

        if not Terminated and SMTP.Connected then
        begin
          Getter.ClearWork;
          DoWork;
          State := tsDone;
          Getter.WorkDone;
          Synchronize (Getter.NotifyUI);
          Getter.ClearWork
        end;

        State := tsDormant
      except
        on e : Exception do
        begin
          fLastError := e.Message;
          if not gAppTerminating then
            Synchronize (NotifyError);
          try
            SMTP.Disconnect;
          except
          end;
          try
            ThreadManager.GetCurrentConnectoid
          except
          end;
          State := tsPending;

          if e is EIdProtocolReplyError then
            if EIdProtocolReplyError (e).ReplyErrorCode = 503 then      // Retry immediately with
                                                                        // 503 errors - otherwise wait
                                                                        // for the user to retrigger
              if not gAppTerminating then
                Synchronize (ThreadManager.JogThreads);

          if e is EidSocketError then
            if (EidSocketError (e).LastError = 0) or (EidSocketError (e).LastError = 10054) then
              if not gAppTerminating then
                Synchronize (ThreadManager.JogThreads);

          if not (e is EIdException) then
            fLastError := '';
        end
      end
    finally
    end
  end
end;

function TSMTPThread.GetClient: TidSMTP;
begin
  Result := TidSMTP (Client);
end;

function TSMTPThread.GetSettings: TSMTPServerSettings;
begin
  result := TSMTPServerSettings (fSettings)
end;

{ TSMTPMailer }

procedure TSMTPMailer.DoWork;
var
  msg : TidMessage;
  gtr : TEmailer;
  request : TEmailerRequest;
  ok : boolean;
  i : Integer;
  att : TAttachment;

begin
  gtr := TEmailer (getter);
  while gtr.Messages.Count > 0 do
  begin
    ok := False;
    request := TEMailerRequest (gtr.Messages [0]);

    msg := TidMessage.Create (nil);
    try
      msg.Recipients.Add.Text := request.MTo;
      if request.MCC <> '' then
        msg.CCList.Add.Text := request.MCC;
      if request.MBCC <> '' then
        msg.BccList.Add.Text := request.MBCC;

      msg.Subject := request.MSubject;

      msg.From.Address := request.ArticleContainer.Identity.EMailAddress;
      msg.From.Name := request.ArticleContainer.Identity.UserName;

      if request.ArticleContainer.Identity.EMailAddress <> request.ArticleContainer.Identity.ReplyAddress then
        with msg.ReplyTo.Add do
        begin
          Name := request.ArticleContainer.Identity.UserName;
          Address := request.ArticleContainer.Identity.ReplyAddress
        end;

      msg.Body.Text := request.Msg;

      if request.CodePage <> CP_USASCII then
      begin
        msg.ContentType := 'text/plain';
        msg.CharSet := CodePageToMIMECharsetName (request.CodePage)
      end;

      if request.Attachments.Count > 0 then
        for i := 0 to request.Attachments.Count - 1 do
        begin
          att := TAttachment (request.Attachments [i]);
          TidAttachment.Create(msg.MessageParts, att.Directory + att.FileName);
        end;

      msg.MsgId := GenerateMessageID ('XN', '', SMTP.Host);
      msg.ExtraHeaders.Values ['Message-Id'] := msg.MsgId;

      msg.References := request.MReplyTo;

      SMTP.MailAgent := ThreadManager.NewsAgent;
      SMTP.Send (msg);

      gtr.CurrentMessage := msg;

      ok := True
    finally
      msg.Free;

      if ok then
        gtr.Messages.Delete (0);
    end
  end
end;

procedure TNNTPArticlesThread.GetProgressNumbers(group : TServerAccount; var min, max,
  pos: Integer);
begin
  min := 0;
  UILock;
  try
    if Assigned (fCurrentGetter) and ((group = nil) or (fCurrentGetter.CurrentGroup = group)) then
    begin
      max := fExpectedArticles;
      if fIsXOver then
        pos := fCurrentGetter.Articles.Count
      else
        pos := fCurrentArticleNo
    end
    else
    begin
      max := 0;
      pos := 0
    end
  finally
    UIUnlock
  end
end;

end.
