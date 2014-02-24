unit unitCIDMIMEHandler;

interface

uses
  Windows, Classes, SysUtils, ComObj, ActiveX, URLMon, unitMessages;

const
  Class_CIDMimeFilter: TGUID = '{638A4DB2-B381-42B2-9EE9-58229EF72967}';

type
  TCIDMimeFilter = class(TComObject, IInternetProtocol)
  private
    fUrl: PWideChar;
    fData: TMemoryStream;
  protected
    // IInternetProtocol Methods
    function Start(szUrl: PWideChar; OIProtSink: IInternetProtocolSink;
      OIBindInfo: IInternetBindInfo; grfPI, dwReserved: DWORD): HResult; stdcall;
    function Continue(const ProtocolData: TProtocolData): HResult; stdcall;
    function Abort(hrReason: HResult; dwOptions: DWORD): HResult; stdcall;
    function Terminate(dwOptions: DWORD): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult; stdcall;
    function Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD;
      out libNewPosition: ULARGE_INTEGER): HResult; stdcall;
    function LockRequest(dwOptions: DWORD): HResult; stdcall;
    function UnlockRequest: HResult; stdcall;
  public
    destructor Destroy; override;
  end;

procedure InitializeCIDMIMEHandler;
procedure FreeCIDMIMEHandler;

implementation

uses
  ComServ;

var
  gInternetSession: IInternetSession;
  gFactory: IClassFactory;

procedure InitializeCIDMIMEHandler;
begin
  OleCheck(CoInternetGetSession(0, gInternetSession, 0));
  if gInternetSession <> nil then
  begin
    CoGetClassObject(Class_CIDMimeFilter, CLSCTX_SERVER, nil, IClassFactory, gFactory);
    if gFactory <> nil then
    begin
      OleCheck(gInternetSession.RegisterMimeFilter(gFactory, Class_CIDMimeFilter, 'cid'));
      OleCheck(gInternetSession.RegisterNameSpace(gFactory, CLASS_CIDMimeFilter, 'cid', 0, nil, 0));
    end
  end
end;

procedure FreeCIDMIMEHandler;
(**)(*)  // Magic comment.  To toggle, insert a space
         // before the first)
var
  rc: Integer;
(**)
begin
  if Assigned(gInternetSession) then
  begin
    gInternetSession.UnregisterNameSpace(gFactory, 'cid');
    gInternetSession.UnregisterMimeFilter(gFactory, 'cid')
  end;
  gFactory := nil;
(**)(*)
  rc := gInternetSession._AddRef - 1;
  gInternetSession._Release;
  MessageBeep(rc);
(**)

  gInternetSession := nil
end;

{ TCIDMimeFilter }

function TCIDMimeFilter.Abort(hrReason: HResult; dwOptions: DWORD): HResult;
begin
  Result := S_OK;
end;

function TCIDMimeFilter.Continue(const ProtocolData: TProtocolData): HResult;
begin
  Result := S_OK;
end;

destructor TCIDMimeFilter.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

function TCIDMimeFilter.LockRequest(dwOptions: DWORD): HResult;
begin
  Result := S_OK;
end;

function TCIDMimeFilter.Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult;
var
  data: PAnsiChar;
begin
  data := PAnsiChar(fData.Memory);
  Inc(data, fData.Position);

  cbRead := cb;
  if cbRead > (fData.Size - fData.Position) then
    cbRead := fData.Size - fData.Position;

  if cbRead > 0 then
  begin
    Move(data^, pv^, cbRead);
    fData.Position := fData.Position + cbRead
  end;

  if fData.Position < fData.Size then
    Result := S_OK
  else
    Result := S_FALSE
end;

function TCIDMimeFilter.Resume: HResult;
begin
  Result := S_OK;
end;

function TCIDMimeFilter.Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD;
  out libNewPosition: ULARGE_INTEGER): HResult;
begin
  Result := S_OK;
end;

function TCIDMimeFilter.Start(szUrl: PWideChar;
  OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo; grfPI,
  dwReserved: DWORD): HResult;
var
  sink: IInternetProtocolSink;
  cid: string;
  mp: TmvMessagePart;
  w, wc: string;
begin
  fURL := szURL;

  Result := S_FALSE;
  if Assigned(gCurrentMessage) then
  begin
    sink := OIProtSink as IInternetProtocolSink;

    cid := Copy(fURL, 5, MaxInt);

    mp := gCurrentMessage.FindMessagePartFromCID('<' + cid + '>');

    if Assigned(mp) then
    begin
      fData := TMemoryStream.Create;
      mp.GetData(fData);
      fData.Seek(0, soBeginning);
      w := mp.MIMEContentType;
      wc := mp.FileName;
      sink.ReportData(BSCF_DATAFULLYAVAILABLE or BSCF_LASTDATANOTIFICATION, 0, 1969);
      sink.ReportProgress(BINDSTATUS_MIMETYPEAVAILABLE, PWideChar(w));
      sink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE, PWideChar(wc));
      sink.ReportResult(S_OK, 0, nil);
      Result := S_OK;
    end;
  end;
end;

function TCIDMimeFilter.Suspend: HResult;
begin
  Result := S_OK;
end;

function TCIDMimeFilter.Terminate(dwOptions: DWORD): HResult;
begin
  Result := S_OK;
end;

function TCIDMimeFilter.UnlockRequest: HResult;
begin
  Result := S_OK;
end;

initialization
  TComObjectFactory.Create(ComServer, TCIDMimeFilter, Class_CIDMimeFilter,
    'CIDMimeFilter', 'CIDMimeFilter', ciMultiInstance, tmApartment);
end.
