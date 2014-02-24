unit IdNNTPX;

interface

uses
  Windows, Classes, SysUtils, IdTCPClient, IdGlobal, IdException,
  IdAssignedNumbers, IdIOHandlerSocket, ConTnrs, IdReplyRFC,
  XnClasses, XnRawByteStrings;

type
  TModeType = (mtStream, mtIHAVE, mtReader);
  TConnectionResult = (crCanPost, crNoPost, crAuthRequired, crTempUnavailable);
  TModeSetResult = (mrCanStream, mrNoStream, mrCanIHAVE, mrNoIHAVE, mrCanPost, mrNoPost);
  TEventNewNewsList = procedure(const AMsgID: string; var ACanContinue: Boolean) of object;
  TEventNewsgroupList = procedure(const ANewsgroup: string; const ALow, AHigh: Int64;
    const AType: string; var ACanContinue: Boolean) of object;
  TNewsTransportEvent = procedure(AMsg: TStringList) of object;

  TGetMessageCommand = (gmHeader, gmBody, gmArticle, gmGroup);
  TPipelineState = (psIdle, psFilling, psProcessing);

  TPipeLineCommand = class
  private
    fArticleNo: Int64;
    fStr: string;
    fCommand: TGetMessageCommand;
    fParam: LPARAM;
    function GetIsGet: Boolean;
  public
    constructor Create(AArticleNo: Int64; const AMsgID: string; ACommand: TGetMessageCommand; AParam: LPARAM);
    constructor CreateGroupCommand(const AGroupName: string; AParam: LPARAM);
    property ArticleNo: Int64 read fArticleNo;
    property Command: TGetMessageCommand read fCommand;
    property Param: LPARAM read fParam;
    property MsgID: string read fStr;
    property IsGet: Boolean read GetIsGet;
  end;

  TPipeLineCommandStartEvent = procedure(cmd: TPipelineCommand; var headrs: TAnsiStrings; var body: TStream) of object;
  TPipeLineCommandEvent = procedure(cmd: TPipelineCommand) of object;
  TPipeLineCommandAbortEvent = procedure(cmd: TPipelineCommand; startCalled: Boolean) of object;

  TidNNTPX = class(TIdTCPClient)
  private
    FPipelineSize: Integer;
    FPipeLine: TObjectList;
    FPipeLineCmd: string;
    FPipelineState: TPipelineState;
    FlMsgNo: Int64;
    FlMsgCount: Int64;
    FlMsgLow: Int64;
    FlMsgHigh: Int64;
    fsMsgID: string;
    fGroupOK: Boolean;
    fConectionResult: TConnectionResult;
    fModeResult: TModeSetResult;
    FbSetMode: Boolean;
    FNewsAgent: string;
    FOnNewNewsList: TEventNewNewsList;
    FOnNewsgroupList: TEventNewsgroupList;
    FOnNewGroupsList: TEventNewsGroupList;
    fModeType: TModeType;
    fOnSendIHAVE: TNewsTransportEvent;
    fOnSendCheck: TNewsTransportEvent;
    fOnSendTakethis: TNewsTransportEvent;
    FPipelineCommandEndEvent: TPipelineCommandEvent;
    FPipelineCommandStartEvent: TPipelineCommandStartEvent;
    FPipelineCommandAbortEvent: TPipelineCommandAbortEvent;
    FIsConnected: Boolean;
    function Capture(const AList: TStrings): Integer;
    function ConvertDateTimeDist(ADate: TDateTime; AGMT: Boolean; const ADistributions: string): string;
    function Get(const ACmd: string; const AMsgNo: Int64; const AMsgID: string; AHdr: TAnsiStrings; ABody: TStream): Boolean;
    procedure ReceiveBody(const AStream: TStream; const ADelim: string = '');
    function  ReceiveHeader(AMsg: TAnsiStrings; const ADelim: RawByteString = ''): Boolean;
    procedure ReceiveHeaders(AMsg: TAnsiStrings);
    procedure setConnectionResult(const Value: TConnectionResult);
    procedure SetModeResult(const Value: TModeSetResult);
    procedure SetModeType(const Value: TModeType);
    function SetArticle(const ACmd: string; const AMsgNo: Int64; const AMsgID: string): Boolean;
    procedure AddPipelineGetCommand(AArticleNo: Int64; const AMsgID: string; ACommand: TGetMessageCommand; AParam: LPARAM);
    procedure AddPipelineGroupCommand(const AGroupName: string; AParam: LPARAM);
    procedure ProcessPipeline;
  protected
    procedure DoOnConnected; override;
    procedure DoOnDisconnected; override;
  public
    procedure InitComponent; override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure DisconnectNotifyPeer; override;
    function Connected: Boolean; override;
    procedure Disconnect(ANotifyPeer: Boolean); override;
    function GetBody(const AMsgNo: Int64; const AMsgID: string; AMsg: TStream): Boolean;
    function GetHeader(const AMsgNo: Int64; const AMsgID: string; AHdr: TAnsiStrings): Boolean;
    function GetArticle(const AMsgNo: Int64; const AMsgID: string; AHdr: TAnsiStrings; ABody: TStream): Boolean;

    procedure BeginPipeline;
    procedure PipelineGetArticle(AMsgNo: Int64; const AMsgId: string; AParam: LPARAM);
    procedure PipelineGetBody(AMsgNo: Int64; const AMsgID: string; AParam: LPARAM);
    procedure PipelineGetHeader(AMsgNo: Int64; const AMsgID: string; AParam: LPARAM);
    procedure PipelineGroup(const groupName: string; AParam: LPARAM);
    procedure EndPipeline;
    procedure CancelPipeline;

    procedure GetNewsgroupList(const AList: TStrings);
    procedure GetNewGroupsList(const ADate: TDateTime; const AGMT: Boolean; const ADistributions: string; const AList: TStrings);
    procedure GetCapabilities(const AList: TstringList);
    procedure GetOverviewFMT(const AList: TStringList);
    function GetServerDateTime: TDateTime;
    function SelectArticle(const AMsgNo: Int64): Boolean;
    procedure SelectGroup(const AGroup: string);
    function SendCmd(AOut: string; const AResponse: array of SmallInt;
      AEncoding: IIdTextEncoding = nil): SmallInt; override;
    procedure SendXOVER(const AParam: string; AResponse: TAnsiStrings);
    procedure Send(header, msg: TAnsiStrings);
    procedure Authenticate;

    function IsServerException(E: Exception): Boolean;

    property MsgID: string read fsMsgID;
    property MsgNo: Int64 read FlMsgNo;
    property MsgHigh: Int64 read FlMsgHigh;
    property MsgLow: Int64 read FlMsgLow;
    property GreetingResult: TConnectionResult read fConectionResult write setConnectionResult;
    property ModeResult: TModeSetResult read fModeResult write SetModeResult;
    property MsgCount: Int64 read flMsgCount write flMsgCount;
    property PipelineSize: Integer read fPipelineSize write fPipelineSize;
    property IsConnected: Boolean read FIsConnected;
  published
    property NewsAgent: string read FNewsAgent write FNewsAgent;
    property Mode: TModeType read fModeType write SetModeType default mtReader;
    property Password;
    property Username;
    property SetMode: Boolean read FbSetMode write FbSetMode default True;
    property OnSendCheck: TNewsTransportEvent read fOnSendCheck write fOnSendCheck;
    property OnSendIHAVE: TNewsTransportEvent read fOnSendIHAVE write fOnSendIHAVE;
    property OnSendTakeThis: TNewsTransportEvent read fOnSendTakethis write fOnSendTakethis;
    property OnNewsgroupList: TEventNewsgroupList read FOnNewsgroupList write FOnNewsgroupList;
    property OnNewGroupsList: TEventNewsGroupList read FOnNewGroupsList write FOnNewGroupsList;
    property PipelineCommandStartEvent: TPipelineCommandStartEvent read FPipelineCommandStartEvent write FPipelineCommandStartEvent;
    property PipelineCommandEndEvent: TPipelineCommandEvent read FPipelineCommandEndEvent write FPipelineCommandEndEvent;
    property PipelineCommandAbortEvent: TPipelineCommandAbortEvent read FPipelineCommandAbortEvent write FPipelineCommandAbortEvent;
    property OnNewNewsList: TEventNewNewsList read FOnNewNewsList write FOnNewNewsList;
    property Port default IdPORT_NNTP;
  end;

  EIdNNTPConnectionRefused = class(EIdReplyRFCError);
  EIdNNTPAuthenticationRequired = class(EIdException);

procedure ParseXOVER(const Aline: RawByteString; var AArticleIndex: Int64;
  var ASubject,
      AFrom: RawByteString;
  var ADate: TDateTime;
  var AMsgId,
      AReferences: RawByteString;
  var AByteCount,
      ALineCount: Cardinal;
  var AExtraData: string);

procedure ParseNewsGroup(ALine: string; var ANewsGroup: string;
  var AHi, ALo: Int64; var AStatus: string; var isNew: Boolean);

implementation

uses
  IdResourceStrings, IdComponent, NewsGlobals, unitSearchString, unitLog,
  IdGlobalProtocols, IdResourceStringsProtocols, IdStack;

var
  lastGoodDate: TDateTime = 0;

procedure ParseXOVER(const Aline: RawByteString; var AArticleIndex: Int64;
  var ASubject,
      AFrom: RawByteString;
  var ADate: TDateTime;
  var AMsgId,
      AReferences: RawByteString;
  var AByteCount,
      ALineCount: Cardinal;
  var AExtraData: string);

  function NextItem(P: PAnsiChar; var S: RawByteString): PAnsiChar;
  begin
    Result := P;
    if Result <> nil then
    begin
      while Result^ <> #0 do
      begin
        if Result^ = #9 then
          Break;
        Inc(Result);
      end;

      SetString(S, P, Result - P);
      if Result^ <> #0 then
      begin
        Inc(Result);
        {Strip backspace and tab junk sequences which occur after a tab separator so they don't throw off any code}
        if (Result^ = #8) and (Result[1] = #9) then
          Inc(Result, 2);
      end;
    end
    else
      S := '';
  end;

  function NextItemStr(var P: PAnsiChar): RawByteString;
  begin
    P := NextItem(P, Result);
  end;

var
  P: PAnsiChar;
begin
  P := PAnsiChar(Aline);

  {Article Index}
  AArticleIndex := RawStrToInt64(NextItemStr(P));
  {Subject}
  P := NextItem(P, ASubject);
  {From}
  P := NextItem(P, AFrom);
  {Date}
  try
    ADate := RawGMTToLocalDateTime(string(NextItemStr(P)));
    lastGoodDate := ADate;
  except
    ADate := LastGoodDate;
  end;
  {Message ID}
  P := NextItem(P, AMsgId);
  {References}
  P := NextItem(P, AReferences);
  {Byte Count}
  AByteCount := RawStrToIntDef(NextItemStr(P), 0);
  {Line Count}
  ALineCount := RawStrToIntDef(NextItemStr(P), 0);
  {Extra data}
  AExtraData := string(P);
  if (AExtraData <> '') and (Pos(#9#8#9, AExtraData) > 0) then
    AExtraData := StringReplace(AExtraData, #9#8#9, #9, [rfReplaceAll]);
end;

procedure ParseNewsGroup(ALine: string; var ANewsGroup: string; var AHi, ALo: Int64;
  var AStatus: string; var isNew: Boolean);
begin
  isNew := False;
  if ALine <> '' then
  begin
    if ALine[1] = '"' then
    begin
      ANewsGroup := AnsiDequotedStr(ALine, '"');
      Delete(ALine, 1, Length(ANewsGroup) + 2);
      ALine := TrimLeft(Aline);
    end
    else
      ANewsgroup := Fetch(ALine, ' ');
    AHi := IndyStrToInt64(Fetch(Aline, ' '), 0);
    ALo := IndyStrToInt64(Fetch(ALine, ' '), 0);
    AStatus := Fetch(ALine, ' ');

    if LowerCase(Trim(ALine)) = '*' then
      isNew := True;
  end;
end;

{ TidNNTPX }

procedure TidNNTPX.Authenticate;
begin
  LogMessage('[tx] AuthInfo User <username>');
  inherited SendCmd('AuthInfo User ' + Username, [281, 381]);
  LogMessage('[rx] ' + IntToStr(LastCmdResult.NumericCode) + ' ' + LastCmdResult.Text.Text, False, False);

  if LastCmdResult.NumericCode <> 281 then
  begin
    LogMessage('[tx] AuthInfo Pass <password>');
    inherited SendCmd('AuthInfo Pass ' + Password, [281]);
    LogMessage('[rx] ' + IntToStr(LastCmdResult.NumericCode) + ' ' + LastCmdResult.Text.Text, False, False);
  end;
end;

procedure TidNNTPX.Connect;
begin
  inherited Connect;
  if IOHandler is TidIOHandlerSocket then
    TidIOHandlerSOcket(IOHandler).UseNagle := False;
  try
    GetResponse([]);
    // Here lets check to see what condition we are in after being greeted by
    // the server. The application utilizing NNTPWinshoe should check the value
    // of GreetingResult to determine if further action is warranted.

    Greeting.Assign(LastCmdResult);

    case LastCmdResult.NumericCode of
      200: GreetingResult := crCanPost;
      201: GreetingResult := crNoPost;
      400: GreetingResult := crTempUnavailable;
      // This should never happen because the server should immediately close
      // the connection but just in case ....
      // Kudzu: Changed this to an exception, otherwise it produces non-standard usage by the
      // users code
      502: raise EIdNNTPConnectionRefused.CreateError(502, RSNNTPConnectionRefused);
    end;
    // here we call Setmode on the value stored in mode to make sure we can
    // use the mode we have selected
    case mode of
      mtStream:
        begin
          SendCmd('MODE STREAM');
          if LastCmdResult.NumericCode <> 203 then
            ModeResult := mrNoStream
          else
            ModeResult := mrCanStream;
        end;
      mtReader:
        begin
          // We should get the same info we got in the greeting
          // Result but we set mode to reader anyway since the
          // server may want to do some internal reconfiguration
          // if it knows that a reader has connected
          SendCmd('MODE READER');
          if LastCmdResult.NumericCode <> 200 then
            ModeResult := mrNoPost
          else
            ModeResult := mrCanPost;
        end;
    end;
  except
    Disconnect;
    raise;
  end;
end;

function TidNNTPX.Connected: Boolean;
begin
  try
    Result := inherited Connected;
  except
    on E: Exception do
      if (E is EIdConnClosedGracefully) or
         ((E is EIdSocketError) and (EIdSocketError(E).LastError = 10054)) then
        Result := False
      else
        raise;
  end;
end;

procedure TidNNTPX.InitComponent;
begin
  inherited;
  Mode := mtReader;
  Port := IdPORT_NNTP;
  SetMode := True;
  PipelineSize := 1024;
end;

function TidNNTPX.IsServerException(E: Exception): Boolean;
begin
  Result := E is EIdConnClosedGracefully;

  if not Result and (e is EIdReplyRFCError) then
    Result := (EIdReplyRFCError(e).ErrorCode = 400) or
              (EIdReplyRFCError(e).ErrorCode = 503);

  if not Result and (e is EIdSocketError) then
    Result := (EIdSocketError(e).LastError = 0) or
              (EIdSocketError(e).LastError = 10054) or
              (EIdSocketError(e).LastError = 10057);
end;

procedure TidNNTPX.Disconnect(ANotifyPeer: Boolean);
begin
  try
    inherited Disconnect(ANotifyPeer);
    if Assigned(IOHandler) then
      IOHandler.InputBuffer.Clear;
  except
    on E: Exception do
    begin
      if not IsServerException(E) then
        raise;
    end;
  end;
  Assert(not Connected);
end;

procedure TidNNTPX.DisconnectNotifyPeer;
begin
  try
    try
      if Connected then
        IOHandler.WriteLn('QUIT');
    except
      on E: Exception do
      begin
        if not IsServerException(E) then
          raise;
      end;
    end;
  finally
    inherited DisconnectNotifyPeer;
  end;
end;

procedure TidNNTPX.DoOnConnected;
begin
  FIsConnected := True;
  inherited;
end;

procedure TidNNTPX.DoOnDisConnected;
begin
  FIsConnected := False;
  inherited;
end;

function TidNNTPX.Get(const ACmd: string; const AMsgNo: Int64;
  const AMsgID: string; AHdr: TAnsiStrings; ABody: TStream): Boolean;
var
  LContinue: Boolean;
begin
  Result := SetArticle(ACmd, AMsgNo, AMsgID);
  if Result then
  begin
    if Assigned(AHdr) then
      AHdr.Clear;
    if AnsiSameText(ACmd, 'HEAD') then
    begin
      if LastCmdResult.NumericCode in [220, 221] then
        ReceiveHeader(AHdr, '.')
    end
    else
    begin
      if LastCmdResult.NumericCode in [220, 221] then
        LContinue := ReceiveHeader(AHdr, '')
      else
        LContinue := True;

      if LContinue and (LastCmdResult.NumericCode in [220, 222]) then
        ReceiveBody(ABody, '.');
    end;
  end;
end;

function TidNNTPX.GetArticle(const AMsgNo: Int64; const AMsgID: string;
  AHdr: TAnsiStrings; ABody: TStream): Boolean;
begin
  Result := Get('Article', AMsgNo, AMsgID, AHdr, ABody);
end;

function TidNNTPX.GetBody(const AMsgNo: Int64; const AMsgID: string;
  AMsg: TStream): Boolean;
begin
  Result := Get('Body', AMsgNo, AMsgID, nil, AMsg);
end;

function TidNNTPX.GetHeader(const AMsgNo: Int64; const AMsgID: string;
  AHdr: TAnsiStrings): Boolean;
begin
  Result := Get('Head', AMsgNo, AMsgID, AHdr, nil);
end;

procedure TidNNTPX.GetNewsgroupList(const AList: TStrings);
begin
  SendCmd('LIST', 215);
  Capture(AList);
end;

procedure TidNNTPX.GetCapabilities(const AList: TStringList);
begin
  SendCmd('CAPABILITIES', 101);
  Capture(AList);
end;

procedure TidNNTPX.GetOverviewFMT(const AList: TStringList);
begin
  SendCmd('LIST OVERVIEW.FMT', 215);
  Capture(AList);
end;

function TidNNTPX.GetServerDateTime: TDateTime;
begin
  SendCmd('DATE', 111);
  Result := FTPMLSToGMTDateTime(LastCmdResult.Text[0]);
end;

procedure TidNNTPX.ReceiveBody(const AStream: TStream; const ADelim: string);
var
  I: Integer;
  sl: TStringList;
begin
  IOHandler.Capture(AStream, ADelim);
  if gLogFlag then
  begin
    sl := TStringList.Create;
    try
      AStream.Position := 0;
      sl.LoadFromStream(AStream);
      for I := 0 to sl.Count - 1 do
        LogMessage('[rx] ' + sl[I], False);
    finally
      sl.Free;
    end;
  end;
end;

function TidNNTPX.ReceiveHeader(AMsg: TAnsiStrings; const ADelim: RawByteString): Boolean;
var
  L: Integer;
  S: string;
  Bytes: TIdBytes;
  NewLine: RawByteString;
begin
  Result := True;

  BeginWork(wmRead);
  try
    repeat
      S := IOHandler.ReadLn();
      LogMessage('[rx] ' + S);
      Bytes := ToBytes(S, Indy8BitEncoding);
      L := Length(Bytes);
      if L > 0 then
        SetString(NewLine, PAnsiChar(@Bytes[0]), L)
      else
        NewLine := '';

      if NewLine = ADelim then
        Break
      else if Copy(NewLine, 1, 2) = '..' then
        Delete(NewLine, 1, 1)
      else if NewLine = '.' then
      begin
        Result := False;
        Break;
      end;
      if Assigned(AMsg) then
        AMsg.Add(NewLine);
    until False;

    if Assigned(AMsg) then
      FixHeaders(AMsg);
  finally
    EndWork(wmRead);
  end;
end;

procedure TidNNTPX.ReceiveHeaders(AMsg: TAnsiStrings);
var
  L: Integer;
  S: string;
  Bytes: TIdBytes;
  NewLine: RawByteString;
begin
  BeginWork(wmRead);
  try
    repeat
      S := IOHandler.ReadLn();
      LogMessage('[rx] ' + S);
      Bytes := ToBytes(S, Indy8BitEncoding);
      L := Length(Bytes);
      if L > 0 then
        SetString(NewLine, PAnsiChar(@Bytes[0]), L)
      else
        NewLine := '';

      if NewLine = '.' then
        Break
      else if Copy(NewLine, 1, 2) = '..' then
        Delete(NewLine, 1, 1);

      if Assigned(AMsg) then
        AMsg.Add(NewLine);
    until False;

  finally
    EndWork(wmRead);
  end;
end;

function TidNNTPX.SelectArticle(const AMsgNo: Int64): Boolean;
begin
  Result := SetArticle('STAT', AMsgNo, '');
end;

procedure TidNNTPX.SelectGroup(const AGroup: string);
var
  s: string;
  group: string;
begin
  SendCmd('GROUP ' + AGroup, [211]);
  s := LastCmdResult.Text[0];
  FlMsgCount := IndyStrToInt64(Fetch(s));
  FlMsgLow := IndyStrToInt64(Fetch(s));
  FlMsgHigh := IndyStrToInt64(Fetch(s));
  group := Fetch(s);

  if (group <> '') and not SameText(AGroup, group) then
    raise Exception.Create('Selected group: ' + group +
      ' does not match with the requested group: ' + AGroup);

  LogMessage('Select group ' + AGroup +
    '. Messages available ' + IntToStr(FlMsgLow) + ' - ' + IntToStr(FlMsgHigh));
end;

procedure TidNNTPX.Send(header, msg: TAnsiStrings);
var
  i: Integer;
  name, val: RawByteString;
  S: RawByteString;
begin
  SendCmd('POST', 340);
  BeginWork(wmWrite);
  try
    for i := 0 to header.Count - 1 do
    begin
      name := header.Names[i];
      val := header.ValueFromIndex[i];
      if name <> '' then
      begin
        S := name + ': ' + val + EOL;
        LogMessage('[tx] ' + string(S), False, False);
        IOHandler.Write(ToBytes(S));
      end;
    end;

    LogMessage('[tx]');
    IOHandler.WriteLn;

    for i := 0 to msg.Count - 1 do
    begin
      if Copy(msg[i], 1, 1) = '.' then
        S := '.' + msg[i] + EOL
      else
        S := msg[i] + EOL;
      LogMessage('[tx] ' + string(S), False, False);
      IOHandler.Write(ToBytes(S));
    end;
  finally
    EndWork(wmWrite);
  end;
  SendCmd('.', 240);
end;

function TidNNTPX.SendCmd(AOut: string; const AResponse: array of SmallInt;
  AEncoding: IIdTextEncoding = nil): SmallInt;
begin
  // NOTE: Responses must be passed as arrays so that the proper inherited SendCmd is called
  // and a stack overflow is not caused.
  LogMessage('[tx] ' + AOut);
  Result := inherited SendCmd(AOut, [], AEncoding);
  LogMessage('[rx] ' + IntToStr(LastCmdResult.NumericCode) + ' ' + LastCmdResult.Text.Text, False, False);

  if (Result = 480) or (Result = 450) then
  begin
    if UserName <> '' then
    begin
      LogMessage('[tx] AuthInfo User <username>');
      Result := inherited SendCmd('AuthInfo User ' + Username, [281, 381]);
      LogMessage('[rx] ' + IntToStr(LastCmdResult.NumericCode) + ' ' + LastCmdResult.Text.Text, False, False);

      if Result = 381 then
      begin
        if Password <> '' then
        begin
          LogMessage('[tx] AuthInfo Pass <password>');
          inherited SendCmd('AuthInfo Pass ' + Password, [281]);
          LogMessage('[rx] ' + IntToStr(LastCmdResult.NumericCode) + ' ' + LastCmdResult.Text.Text, False, False);
        end
        else
          raise EIdNNTPAuthenticationRequired.Create('Authentication failed, use a valid non-empty password');
      end;

      LogMessage('[tx] ' + AOut);
      Result := inherited SendCmd(AOut, AResponse, AEncoding);
      LogMessage('[rx] ' + IntToStr(LastCmdResult.NumericCode) + ' ' + LastCmdResult.Text.Text, False, False);
    end
    else
      raise EIdNNTPAuthenticationRequired.Create('Authentication failed, use a valid non-empty username');
  end
  else
    Result := CheckResponse(Result, AResponse);
end;

procedure TidNNTPX.SendXOVER(const AParam: string; AResponse: TAnsiStrings);
begin
  SendCmd('XOVER ' + AParam, 224);
  ReceiveHeaders(AResponse);
end;

function TidNNTPX.SetArticle(const ACmd: string; const AMsgNo: Int64;
  const AMsgID: string): Boolean;
var
  s: string;
begin
  if AMsgNo >= 1 then
    SendCmd(ACmd + ' ' + IntToStr(AMsgNo))
  else if AMsgID <> '' then
    SendCmd(ACmd + ' ' + AMsgID)
  else // Retrieve / Set currently selected atricle
    SendCmd(ACmd);

  if LastCmdResult.NumericCode in [220, 221, 222, 223] then
  begin
    if AMsgID = '' then
    begin
      s := Trim(LastCmdResult.Text[0]);
      flMsgNo := IndyStrToInt64(Fetch(s, ' '));
      fsMsgID := s;
    end
    else
      if AMsgNo < 1 then
      begin
        s := Trim(LastCmdResult.Text[0]);
        flMsgNo := IndyStrToInt64(Fetch(s, ' '));
      end;

    Result := True;
  end
  else
    if (LastCmdResult.NumericCode = 421) or (LastCmdResult.NumericCode = 422) or
       (LastCmdResult.NumericCode = 423) or (LastCmdResult.NumericCode = 430) or
       (LastCmdResult.NumericCode = 503) then
    begin
      // 421 no next article in this group
      // 422 no previous article in this group
      // 423 no such article number in this group
      // 430 no such article found
      // 503 program fault - command not performed
      Result := False;
    end
    else
      raise EIdReplyRFCError.Create(LastCmdResult.Text[0]);
end;

procedure TidNNTPX.setConnectionResult(const Value: TConnectionResult);
begin
  fConectionResult := Value;
end;

procedure TidNNTPX.SetModeResult(const Value: TModeSetResult);
begin
  fModeResult := Value;
end;

procedure TidNNTPX.SetModeType(const Value: TModeType);
begin
  fModeType := Value;
end;

procedure TIdNNTPX.GetNewGroupsList(const ADate: TDateTime; const AGMT: Boolean;
  const ADistributions: string; const AList: TStrings);
begin
  SendCmd('NEWGROUPS ' + ConvertDateTimeDist(ADate - OffsetFromUTC, AGMT, ADistributions), 231);
  Capture(AList);
end;

function TidNNTPX.ConvertDateTimeDist(ADate: TDateTime; AGMT: Boolean;
  const ADistributions: string): string;
begin
  Result := FormatDateTime('yymmdd hhnnss', ADate);
  if AGMT then
    Result := Result + ' GMT';
  if Length(ADistributions) > 0 then
    Result := ' <' + ADistributions + '>';
end;

(*----------------------------------------------------------------------*
 | TidNNTPX.BeginPipeline                                               |
 |                                                                      |
 | Start pipelining.  This will fail if it's already pipelining         |
 *----------------------------------------------------------------------*)
procedure TidNNTPX.BeginPipeline;
begin
  if fPipelineState = psIdle then
  begin
    fGroupOK := True;
    fPipelineCmd := '';
    if not Assigned(fPipeLine) then
      fPipeLine := TObjectList.Create
    else
      fPipeLine.Clear;

    fPipelineState := psFilling;
  end;
end;

destructor TidNNTPX.Destroy;
begin
  fPipeLine.Free;
  inherited Destroy;
end;

(*----------------------------------------------------------------------*
 | TidNNTPX.EndPipeline                                                 |
 |                                                                      |
 | Send the pipelined commands, and process the results.                |
 *----------------------------------------------------------------------*)
procedure TidNNTPX.EndPipeline;
begin
  if fPipelineState = psFilling then
  try
    ProcessPipeLine;
  finally
    fPipeLineState := psIdle;
  end;
end;

procedure TidNNTPX.PipelineGetArticle(AMsgNo: Int64; const AMsgID: string; AParam: LPARAM);
begin
  AddPipelineGetCommand(AMsgNo, AMsgID, gmArticle, AParam);
end;

procedure TidNNTPX.PipelineGetBody(AMsgNo: Int64; const AMsgID: string; AParam: LPARAM);
begin
  AddPipelineGetCommand(AmsgNo, AMsgID, gmBody, AParam);
end;

procedure TidNNTPX.PipelineGetHeader(AMsgNo: Int64; const AMsgID: string; AParam: LPARAM);
begin
  AddPipelineGetCommand(AmsgNo, AMsgID, gmheader, AParam);
end;

procedure TidNNTPX.PipelineGroup(const groupName: string; AParam: LPARAM);
begin
  AddPipelineGroupCommand(groupName, AParam);
end;

procedure TidNNTPX.CancelPipeline;
begin
  try
    fPipelineState := psIdle;
    if Assigned(fPipeLine) then
      fPipeLine.Clear;
  except
  end;
end;

function TidNNTPX.Capture(const AList: TStrings): Integer;
var
  I: Integer;
begin
  IOHandler.Capture(AList);

  if gLogFlag then
    for I := 0 to AList.Count - 1 do
      LogMessage('[rx] ' + AList[i]);

  Result := AList.Count;
end;

procedure TidNNTPX.AddPipelineGetCommand(AArticleNo: Int64;
  const AMsgID: string; ACommand: TGetMessageCommand; AParam: LPARAM);
var
  st, str: string;
begin
  if fPipelineState = psFilling then
  begin
    st := '';

    if AArticleNo = 0 then
      str := AMsgID
    else
      str := IntToStr(AArticleNo);

    case ACommand of
      gmHeader: st := 'HEAD';
      gmBody: st := 'BODY';
      gmArticle: st := 'ARTICLE';
    end;

    st := st + ' ' + str + #13#10;

    if Length(fPipelineCmd) + Length(st) > PipelineSize then
      ProcessPipeLine;
    fPipeLine.Add(TPipeLineCommand.Create(AArticleNo, AMsgID, ACommand, AParam));
    fPipelineCmd := fPipelineCmd + st;
  end;
end;

procedure TidNNTPX.AddPipelineGroupCommand(const AGroupName: string; AParam: LPARAM);
var
  st: string;
begin
  if fPipelineState = psFilling then
  begin
    st := 'GROUP ' + AGroupName + #13#10;

    if Length(fPipelineCmd) + Length(st) > PipelineSize then
      ProcessPipeLine;

    fPipeLine.Add(TPipeLineCommand.CreateGroupCommand(AGroupName, AParam));
    fPipelineCmd := fPipelineCmd + st;
  end;
end;

procedure TidNNTPX.ProcessPipeline;
  //----------------------------------------------------------
  // Do a buffer full of NNTP commands
  procedure ProcessNNTPCommand(const cmd: string; startIdx, endIdx: Integer);
  var
    st, str: string;
    pipelineCommand: TPipeLineCommand;
    resp: Integer;
    respOK: Boolean;
    header: TAnsiStrings;
    body: TStream;
    seCalled, eeCalled: Boolean;
    artNo: Int64;
    msgID: string;

    function ValidResponse(const str: string): Boolean;
    begin
      Result := (Length(str) > 4) and (str[4] in [' ', #9]);
      if Result then
        Result := (str[1] in ['0'..'9']) and
                  (str[2] in ['0'..'9']) and
                  (str[3] in ['0'..'9']);
    end;

  begin
    LogMessage('[tx] ' + cmd, False, False);
    IOHandler.Write(cmd);              // Send the buffer of commands.

                        // Must get a reply for each one
    while startIdx <= endIdx do
    begin
      pipelineCommand := TPipeLineCommand(fPipeLine[startIdx]);

      repeat
        str := IOHandler.ReadLn;       // Get the reply
        LogMessage('[rx] ' + str);
        if not ValidResponse(str) then
        begin
          if str <> '.' then                    // Work around for Jive forums.
          begin
            repeat
              str := IOHandler.ReadLn;
              LogMessage('[rx] ' + str);
            until str = '.';
          end;

          str := '';
        end;
      until str <> '';                          // Version 1.15.7.5 onwards -
                                                // Ignore blank lines before response
      str := StringReplace(str, #9, ' ', [rfReplaceAll]);
      st := str;

      resp := StrToIntDef(SplitString(' ', str), -1);
      if resp = -1 then
        raise EIdException.Create('Invalid response in pipeline.  ' + st);

                        // Check whether the expected response was received.
      case pipeLineCommand.Command of
        gmHeader : respOK := resp in [220..223];       // Don't be too pedantic.
        gmBody:    respOK := resp in [220..223];       // There are lots of
        gmArticle: respOK := resp in [220..223];       // weird servers out there!
        gmGroup:
          begin
            respOK := resp = 211;
            fGroupOK := respOK; // Set flag to say that the last group change
          end                   // failed.  If so, then ignore all message replies
        else                    // until the next successful group change
          respOK := False;
      end;

      seCalled := False;
      eeCalled := False;
      header := nil;
      body := nil;
      try
        if respOK and fGroupOK then
        begin
          if pipelineCommand.IsGet then
          begin                                 // Check 22x reply matches what we
                                                // expected.
            artNo := StrToInt64Def(SplitString(' ', str), -1);
            msgID := SplitString(' ', str);

            if msgID <> '' then                 // Fix for buggy AToZed server.
            begin
              if Copy(msgId, 1, 1) <> '<' then
                msgID := '<' + msgID;

              if Copy(msgId, Length(msgID), 1) <> '>' then
                msgID := msgID + '>';
            end;

                // nb.  Most servers return '0' for the article no if eg.
                //      HEAD <msgID> was requested - so this bit is of limited
                //     usefulness.  For these servers you should fix up the
                //     article no in PipelineCommandEndEvent
            if pipelineCommand.fArticleNo = 0 then
              pipelineCommand.fArticleNo := artNo;

            if pipelineCommand.fStr = '' then
              pipelineCommand.fStr := msgID;
          end
          else
            artNo := 0;

          if Assigned(PipelineCommandStartEvent) then // Send event to say that the
          begin                                       // command was successfully started
            PipelineCommandStartEvent(pipelineCommand, header, body);
            seCalled := True;                         // Set a flag so we can clear up

            if pipelineCommand.IsGet then
              if (pipelineCommand.fArticleNo <> artNo) or
                 (pipelineCommand.fStr <> msgID) then
                 raise EidException.Create(Format('Pipeline out of sequence %d %d %s %s',
                   [pipelineCommand.fArticleNo, artNo, pipelineCommand.fStr, msgID]));

            if Assigned(header) then                  // Initialize the buffers passed to us
              header.Clear;

            if Assigned(body) then
              body.Size := 0;
          end
          else
          begin
            header := nil;
            body := nil;
          end;
        end;

        if respOK then                           // We *must* get the response text
          case pipelineCommand.Command of
            gmBody:
              ReceiveBody(body, '.');            // nb. Capture works if body is nil -
                                                 // it just discards the Result which
                                                 // is what we want.
            gmHeader:
              ReceiveHeader(header, '.');
            gmArticle:
              if ReceiveHeader(header, '') then
                ReceiveBody(body, '.');
          end
        else
          if not seCalled and Assigned(PipelineCommandAbortEvent) then
          begin
            PipelineCommandAbortEvent(pipelineCommand, seCalled);
            eeCalled := True;
          end;

                                                // If we called the start event we
                                                // *must* call either end event or cancel event
        if seCalled and Assigned(PipelineCommandEndEvent) then
        begin
          PipelineCommandEndEvent(pipelineCommand);
          eeCalled := True;
        end;

      except
        if not eeCalled and Assigned(PipelineCommandAbortEvent) then
        try
          PipelineCommandAbortEvent(pipelineCommand, seCalled);
        except
        end;
        raise;
      end;

      Inc(startIdx);
    end;
  end;

begin
  if fPipelineState = psFilling then
  begin
    fPipelineState := psProcessing;
    try
      ProcessNNTPCommand(fPipelineCmd, 0, fPipeLine.Count - 1);
    finally
      fPipeLine.Clear;
      fPipelineCmd := '';
      fPipelineState := psFilling;
    end;
  end;
end;


{ TPipeLineCommand }

constructor TPipeLineCommand.Create(AArticleNo: Int64; const AMsgID: string;
  ACommand: TGetMessageCommand; AParam: LPARAM);
begin
  fArticleNo := AArticleNo;
  fStr := AMsgID;
  fCommand := ACommand;
  fParam := AParam;
end;

constructor TPipeLineCommand.CreateGroupCommand(const AGroupName: string; AParam: LPARAM);
begin
  fStr := AGroupName;
  fCommand := gmGroup;
  fParam := AParam;
end;

function TPipeLineCommand.GetIsGet: Boolean;
begin
  Result := fCommand in [gmHeader, gmBody, gmArticle];
end;

end.
