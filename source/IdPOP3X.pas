unit IdPOP3X;

{*
  POP 3 (Post Office Protocol Version 3)

  11-10-2001 - J. Peter Mugaas
    Added suggested code from Andrew P.Rybin that does the following:
    -APOP Authentication Support
    -unrecognized text header now displayed in exception message
    -GetUIDL method

  2001-AUG-31 DSiders
    Changed TIdPOP3.Connect to use ATimeout when calling
    inherited Connect.

  2000-SEPT-28 SG
    Added GetUIDL as from code by

  2000-MAY-10 HH
    Added RetrieveMailBoxSize and renamed RetrieveSize to RetrieveMsgSize.
    Finished Connect.

  2000-MARCH-03 HH
    Converted to Indy

*}

interface

uses
  Classes,
  IdAssignedNumbers,
  IdGlobal,
  IdTCPClient;

const
  DEF_APOP = False;

type
  TidPOP3X = class(TIdTCPClient)
  protected
    FAPOP : Boolean;
  public
    function CheckMessages: longint;
    procedure Connect(const ATimeout: Integer = IdTimeoutDefault); override;
    constructor Create(AOwner: TComponent); override;
    function Delete(const MsgNum: Integer): Boolean;
    procedure Disconnect; override;
    function GetResponse(const AAllowedResponses: array of SmallInt): SmallInt;
      override;
    procedure KeepAlive;
    function Reset: Boolean;
//    function Retrieve(const MsgNum: Integer; AMsg: TIdMessage): Boolean;
//    function RetrieveHeader(const MsgNum: Integer; AMsg: TIdMessage): Boolean;
    function RetrieveMsgSize(const MsgNum: Integer): Integer;
    function RetrieveMailBoxSize: integer;
    function RetrieveRaw(const MsgNum: Integer; const Dest: TStrings): boolean;
    function UIDL(const ADest: TStrings; const AMsgNum: Integer = -1): Boolean;
  published
    property APOP: Boolean read FAPOP write FAPOP default DEF_APOP;
    property Password;
    property Username;
    property Port default IdPORT_POP3;
  end;

const
  wsOk = 1;
  wsErr = 2;

implementation

uses
  IdException,
  IdHash,
  IdHashMessageDigest,
  IdTCPConnection,
  IdResourceStrings,
  SysUtils;

{ TidPOP3X }

function TidPOP3X.CheckMessages: longint;
var
  s: string;
begin
  Result := 0;
  SendCmd('STAT', wsOk);    {Do not Localize}
  // Only gets here if exception is not raised
  s := LastCmdResult.Text[0];
  if Length(s) > 0 then begin
    Result := StrToInt(Copy(s, 1, IndyPos(' ', s) - 1));    {Do not Localize}
  end;
end;

procedure TidPOP3X.Connect(const ATimeout: Integer = IdTimeoutDefault);
var
  S: String;
  i: Integer;
begin
  inherited Connect(ATimeout); // ds 2001-AUG-31
  try
    GetResponse([wsOk]);
    if FAPOP then
    begin //APR
        S:=LastCmdResult.Text[0]; //read response
        i:=Pos('<',S);    {Do not Localize}
        if i>0 then begin
           S:=Copy(S,i,MaxInt); //?: System.Delete(S,1,i-1);
           i:=Pos('>',S);    {Do not Localize}
           if i>0 then
           begin
             S:=Copy(S,1,i)
           end
           else begin
             S:='';    {Do not Localize}
           end;
        end//if
        else begin
          S:=''; //no time-stamp    {Do not Localize}
        end;

        if Length(S) > 0 then
        begin
          with TIdHashMessageDigest5.Create do
          try
            S:=LowerCase(TIdHash128.AsHex(HashValue(S+Password)));
          finally
            Free;
          end;//try

          SendCmd('APOP '+Username+' '+S, wsOk);    {Do not Localize}
        end
        else begin
          raise EIdException.Create(RSPOP3ServerDoNotSupportAPOP);
        end;
    end
    else begin //classic method
      SendCmd('USER ' + Username, wsOk);    {Do not Localize}
      SendCmd('PASS ' + Password, wsOk);    {Do not Localize}
    end;//if APOP
  except
    Disconnect;
    raise;
  end;
end;

constructor TidPOP3X.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := IdPORT_POP3;
  APOP := DEF_APOP;
end;

function TidPOP3X.Delete(const MsgNum: Integer): Boolean;
begin
  SendCmd('DELE ' + IntToStr(MsgNum), wsOk);    {Do not Localize}
  Result := LastCmdResult.NumericCode = wsOk;
end;

procedure TidPOP3X.Disconnect;
begin
  try
    if Connected then begin
      WriteLn('QUIT');    {Do not Localize}
    end;
  finally
    inherited Disconnect;
  end;
end;

procedure TidPOP3X.KeepAlive;
begin
  SendCmd('NOOP', wsOk);    {Do not Localize}
end;

function TidPOP3X.Reset: Boolean;
begin
  SendCmd('RSET', wsOK);    {Do not Localize}
  Result := LastCmdResult.NumericCode = wsOK;
end;


function TidPOP3X.RetrieveRaw(const MsgNum: Integer; const Dest: TStrings):
  boolean;
begin
  result := SendCmd('RETR ' + IntToStr(MsgNum)) = wsOk;    {Do not Localize}
  if result then
  begin
    Capture(Dest);
    result := true;
  end;
end;

(*
function TidPOP3X.Retrieve(const MsgNum: Integer; AMsg: TIdMessage): Boolean;
begin
  if SendCmd('RETR ' + IntToStr(MsgNum)) = wsOk then    {Do not Localize}
  begin
    // This is because of a bug in Exchange? with empty messages. See comment in ReceiveHeader
    if Length(ReceiveHeader(AMsg)) = 0 then begin
      // Only retreive the body if we do not already have a full RFC
      ReceiveBody(AMsg);
    end;
  end;
  // Will only hit here if ok and NO exception, or IF is not executed
  Result := LastCmdResult.NumericCode = wsOk;
end;
*)

(*
function TidPOP3X.RetrieveHeader(const MsgNum: Integer; AMsg: TIdMessage): Boolean;
begin
//  Result := False;
  SendCmd('TOP ' + IntToStr(MsgNum) + ' 0', wsOk);    {Do not Localize}
  // Only gets here if no exception is raised
  ReceiveHeader(AMsg,'.');
  Result := True;
end;
*)

function TidPOP3X.RetrieveMailBoxSize: integer;
var
  CurrentLine: string;
begin
  // Returns the size of the mailbox. Issues a LIST command and then
  // sums up each message size. The message sizes are returned in the format
  // 1 1400 2 405 3 100 etc....
  // With this routine, we prevent the user having to call REtrieveSize for
  // each message to get the mailbox size
  Result := 0;
  try
    SendCmd('LIST', wsOk);    {Do not Localize}
    CurrentLine := ReadLn;
    while (CurrentLine <> '.') and (CurrentLine <> '') do    {Do not Localize}
    begin
      CurrentLine := Copy(CurrentLine, IndyPos(' ', CurrentLine) + 1,    {Do not Localize}
        Length(CurrentLine) - IndyPos(' ', CurrentLine) + 1);    {Do not Localize}
      Result := Result + StrToIntDef(CurrentLine, 0);
      CurrentLine := ReadLn;
    end;
  except
    Result := -1;
  end;
end;

function TidPOP3X.RetrieveMsgSize(const MsgNum: Integer): Integer;
var
  s: string;
begin
  Result := -1;
  // Returns the size of the message. if an error ocurrs, returns -1.
  SendCmd('LIST ' + IntToStr(MsgNum), wsOk);    {Do not Localize}
  s := LastCmdResult.Text[0];
  if Length(s) > 0 then  begin
    Result := StrToIntDef(Copy(s, IndyPos(' ', s) + 1,    {Do not Localize}
     Length(s) - IndyPos(' ', s) + 1), -1);    {Do not Localize}
  end;
end;

function TidPOP3X.UIDL(const ADest: TStrings; const AMsgNum: Integer = -1): Boolean;
Begin
  if AMsgNum >= 0 then begin
    Result:=SendCmd('UIDL '+IntToStr(AMsgNum))=wsOk;    {Do not Localize}
    if Result then
    begin
      ADest.Assign(LastCmdResult.Text);
    end;
  end
  else begin
    Result:=SendCmd('UIDL')=wsOk;    {Do not Localize}
    if Result then
    begin
      Capture(ADest);
    end;
  end;
End;//TidPOP3X.GetUIDL

function TidPOP3X.GetResponse(const AAllowedResponses: array of SmallInt): SmallInt;
begin
  GetInternalResponse;
  if AnsiSameText(LastCmdResult.TextCode, '+OK') then begin    {Do not Localize}
    LastCmdResult.NumericCode := wsOK;
  end else if AnsiSameText(LastCmdResult.TextCode, '-ERR') then begin    {Do not Localize}
    LastCmdResult.NumericCode := wsErr;
  end else begin
    raise EIdException.Create(Format(RSPOP3UnrecognizedPOP3ResponseHeader, [LastCmdResult.Text.Text]));
  end;
  Result := CheckResponse(LastCmdResult.NumericCode, AAllowedResponses);
end;

end.

