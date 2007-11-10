{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10337: IdSSLOpenSSL.pas 
{
{   Rev 1.3    2004-05-18 21:33:10  Mattias
{ Fixed unload bug
}
{
{   Rev 1.2    2004-05-07 16:33:46  Mattias
{ Minor fix properly releasing locking structure
}
{
{   Rev 1.1    2004-05-07 10:10:44  Mattias
{ Implemented OpenSSL locking callbacks for thread safity
}
{
{   Rev 1.0    2002.11.12 10:52:32 PM  czhower
}
unit IdSSLOpenSSL;
{
  Author: Gregor Ibic (gregor.ibic@intelicom.si)
  Copyright: (c) Gregor Ibic, Intelicom d.o.o and Indy Working Group.
}
interface

uses
  Classes,
  IdException,
  IdStackConsts,
  IdSocketHandle,
  IdSSLOpenSSLHeaders,
  IdComponent,
  IdIOHandler,
  IdGlobal,
  IdTCPServer,
  IdThread,
  IdTCPConnection,
  IdIntercept, SysUtils,
  IdIOHandlerSocket,
  IdServerIOHandler,
  IdSocks;

type
  TIdX509 = class;

  TIdSSLVersion = (sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1);
  TIdSSLMode = (sslmUnassigned, sslmClient, sslmServer, sslmBoth);
  TIdSSLVerifyMode = (sslvrfPeer, sslvrfFailIfNoPeerCert, sslvrfClientOnce);
  TIdSSLVerifyModeSet = set of TIdSSLVerifyMode;
  TIdSSLCtxMode = (sslCtxClient, sslCtxServer);
  TIdSSLAction = (sslRead, sslWrite);

  TULong = packed record
    case Byte of
      0: (B1,B2,B3,B4: Byte);
      1: (W1,W2: Word);
      2: (L1: Longint);
      3: (C1: Cardinal);
  end;

  TEVP_MD = record
    Length: Integer;
    MD: Array[0..OPENSSL_EVP_MAX_MD_SIZE-1] of Char;
  end;

  TByteArray = record
    Length: Integer;
    Data: PChar;
  End;

  TIdSSLIOHandlerSocket = class;
  TIdSSLCipher = class;

  TCallbackEvent  = procedure(Msg: String) of object;
  TPasswordEvent  = procedure(var Password: String) of object;
  TVerifyPeerEvent  = function(Certificate: TIdX509): Boolean of object;
  TIOHandlerNotify = procedure(ASender: TIdSSLIOHandlerSocket) of object;

  TIdSSLOptions = class(TPersistent)
  protected
    fsRootCertFile, fsCertFile, fsKeyFile: TFileName;
    fMethod: TIdSSLVersion;
    fMode: TIdSSLMode;

    fVerifyDepth: Integer;
    fVerifyMode: TIdSSLVerifyModeSet;
    //fVerifyFile,
    fVerifyDirs, fCipherList: String;
    procedure AssignTo(ASource: TPersistent); override;
  published
    property RootCertFile: TFileName read fsRootCertFile write fsRootCertFile;
    property CertFile: TFileName read fsCertFile write fsCertFile;
    property KeyFile: TFileName read fsKeyFile write fsKeyFile;
    property Method: TIdSSLVersion read fMethod write fMethod;
    property Mode: TIdSSLMode read fMode write fMode;
    property VerifyMode: TIdSSLVerifyModeSet read fVerifyMode write fVerifyMode;
    property VerifyDepth: Integer read fVerifyDepth write fVerifyDepth;
//    property VerifyFile: String read fVerifyFile write fVerifyFile;
    property VerifyDirs: String read fVerifyDirs write fVerifyDirs;
    property CipherList: String read fCipherList write fCipherList;
  public
    // procedure Assign(ASource: TPersistent); override;
  end;

  TIdSSLContext = class(TObject)
  protected
    fMethod: TIdSSLVersion;
    fMode: TIdSSLMode;
    fsRootCertFile, fsCertFile, fsKeyFile: String;
    fVerifyDepth: Integer;
    fVerifyMode: TIdSSLVerifyModeSet;
//    fVerifyFile: String;
    fVerifyDirs: String;
    fCipherList: String;
    fContext: PSSL_CTX;
    fStatusInfoOn: Boolean;
//    fPasswordRoutineOn: Boolean;
    fVerifyOn: Boolean;
    fSessionId: Integer;
    fCtxMode: TIdSSLCtxMode;
    procedure DestroyContext;
    function SetSSLMethod: PSSL_METHOD;
    procedure SetVerifyMode(Mode: TIdSSLVerifyModeSet; CheckRoutine: Boolean);
    function GetVerifyMode: TIdSSLVerifyModeSet;
    procedure InitContext(CtxMode: TIdSSLCtxMode);
  public
    Parent: TObject;
    constructor Create;
    destructor Destroy; override;
    function LoadRootCert: Boolean;
    function LoadCert: Boolean;
    function LoadKey: Boolean;
    property StatusInfoOn: Boolean read fStatusInfoOn write fStatusInfoOn;
//    property PasswordRoutineOn: Boolean read fPasswordRoutineOn write fPasswordRoutineOn;
    property VerifyOn: Boolean read fVerifyOn write fVerifyOn;
  published
    property Method: TIdSSLVersion read fMethod write fMethod;
    property Mode: TIdSSLMode read fMode write fMode;
    property RootCertFile: String read fsRootCertFile write fsRootCertFile;
    property CertFile: String read fsCertFile write fsCertFile;
    property KeyFile: String read fsKeyFile write fsKeyFile;
//    property VerifyMode: TIdSSLVerifyModeSet read GetVerifyMode write SetVerifyMode;
    property VerifyMode: TIdSSLVerifyModeSet read fVerifyMode write fVerifyMode;
    property VerifyDepth: Integer read fVerifyDepth write fVerifyDepth;
  end;

  TIdSSLSocket = class(TObject)
  private
    fPeerCert: TIdX509;
    //fCipherList: String;
    fSSLCipher: TIdSSLCipher;
    fParent: TObject;
    fSSLContext: TIdSSLContext;
    function GetPeerCert: TIdX509;
    function GetSSLError(retCode: Integer): Integer;
    function GetSSLCipher: TIdSSLCipher;
  public
    fSSL: PSSL;
    //
    constructor Create(Parent: TObject);
    procedure Accept(const pHandle: TIdStackSocketHandle; fSSLContext: TIdSSLContext);
    procedure Connect(const pHandle: TIdStackSocketHandle; fSSLContext: TIdSSLContext);
    function Send(var ABuf; ALen: integer): integer;
    function Recv(var ABuf; ALen: integer): integer;
    destructor Destroy; override;
    function GetSessionID: TByteArray;
    function GetSessionIDAsString:String;
    procedure SetCipherList(CipherList: String);
    //
    property PeerCert: TIdX509 read GetPeerCert;
    property Cipher: TIdSSLCipher read GetSSLCipher;
  end;

  TIdSSLIOHandlerSocket = class(TIdIOHandlerSocket)
  private
    fSSLContext: TIdSSLContext;
    fxSSLOptions: TIdSSLOptions;
    fSSLSocket: TIdSSLSocket;
    fIsPeer: Boolean;
    //fPeerCert: TIdX509;
    fOnStatusInfo: TCallbackEvent;
    fOnGetPassword: TPasswordEvent;
    fOnVerifyPeer: TVerifyPeerEvent;
    fSSLLayerClosed: Boolean;
    fOnBeforeConnect: TIOHandlerNotify;
    // function GetPeerCert: TIdX509;
    //procedure CreateSSLContext(axMode: TIdSSLMode);
    fPassThrough: Boolean;
    //
    procedure SetPassThrough(const Value: Boolean);
    procedure Init;
  protected
    procedure DoBeforeConnect(ASender: TIdSSLIOHandlerSocket); virtual;
    procedure DoStatusInfo(Msg: String); virtual;
    procedure DoGetPassword(var Password: String); virtual;
    function DoVerifyPeer(Certificate: TIdX509): Boolean; virtual;
    function RecvEnc(var ABuf; ALen: integer): integer; virtual;
    function SendEnc(var ABuf; ALen: integer): integer; virtual;
    procedure OpenEncodedConnection; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterAccept; override;
    procedure ConnectClient(const AHost: string; const APort: Integer; const ABoundIP: string;
     const ABoundPort: Integer; const ABoundPortMin: Integer; const ABoundPortMax: Integer;
     const ATimeout: Integer = IdTimeoutDefault); override;
    procedure Close; override;
    procedure Open; override;

    function Recv(var ABuf; ALen: integer): integer; override;
    function Send(var ABuf; ALen: integer): integer; override;

    property SSLSocket: TIdSSLSocket read fSSLSocket write fSSLSocket;
    property PassThrough: Boolean read fPassThrough write SetPassThrough;

    property OnBeforeConnect: TIOHandlerNotify read fOnBeforeConnect write fOnBeforeConnect;
  published
    property SSLOptions: TIdSSLOptions read fxSSLOptions write fxSSLOptions;
    property OnStatusInfo: TCallbackEvent read fOnStatusInfo write fOnStatusInfo;
    property OnGetPassword: TPasswordEvent read fOnGetPassword write fOnGetPassword;
    property OnVerifyPeer: TVerifyPeerEvent read fOnVerifyPeer write fOnVerifyPeer;
  end;

  TIdServerIOHandlerSSL = class(TIdServerIOHandler)
  private
    fSSLContext: TIdSSLContext;
    fxSSLOptions: TIdSSLOptions;
//    fPeerCert: TIdX509;
//    function GetPeerCert: TIdX509;
    fIsInitialized: Boolean;
    fOnStatusInfo: TCallbackEvent;
    fOnGetPassword: TPasswordEvent;
    fOnVerifyPeer: TVerifyPeerEvent;
    //procedure CreateSSLContext(axMode: TIdSSLMode);
    //procedure CreateSSLContext;
  protected
    procedure DoStatusInfo(Msg: String); virtual;
    procedure DoGetPassword(var Password: String); virtual;
    function DoVerifyPeer(Certificate: TIdX509): Boolean; virtual;
  public
    procedure Init; override;
    function Accept(ASocket: TIdStackSocketHandle; AThread: TIdThread = nil): TIdIOHandler; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SSLOptions: TIdSSLOptions read fxSSLOptions write fxSSLOptions;
    property OnStatusInfo: TCallbackEvent read fOnStatusInfo write fOnStatusInfo;
    property OnGetPassword: TPasswordEvent read fOnGetPassword write fOnGetPassword;
    property OnVerifyPeer: TVerifyPeerEvent read fOnVerifyPeer write fOnVerifyPeer;
  end;

  TIdX509Name = class(TObject)
  private
    fX509Name: PX509_NAME;
    function CertInOneLine: String;
    function GetHash: TULong;
    function GetHashAsString: String;
  public
    constructor Create(aX509Name: PX509_NAME);
    //
    property Hash: TULong read GetHash;
    property HashAsString: string read GetHashAsString;
    property OneLine: string read CertInOneLine;
  end;

  TIdX509 = class(TObject)
  protected
    FX509    : PX509;
    FSubject : TIdX509Name;
    FIssuer  : TIdX509Name;
    function RSubject:TIdX509Name;
    function RIssuer:TIdX509Name;
    function RnotBefore:TDateTime;
    function RnotAfter:TDateTime;
    function RFingerprint:TEVP_MD;
    function RFingerprintAsString:String;
  public
    Constructor Create(aX509: PX509); virtual;
    Destructor Destroy; override;
    //
    property Fingerprint: TEVP_MD read RFingerprint;
    property FingerprintAsString: String read RFingerprintAsString;
    property Subject: TIdX509Name read RSubject;
    property Issuer: TIdX509Name read RIssuer;
    property notBefore: TDateTime read RnotBefore;
    property notAfter: TDateTime read RnotAfter;
  end;

  TIdSSLCipher = class(TObject)
  private
    FSSLSocket: TIdSSLSocket;
    function GetDescription: String;
    function GetName: String;
    function GetBits: Integer;
    function GetVersion: String;
  public
    constructor Create(AOwner: TIdSSLSocket);
    destructor Destroy; override;
  published
    property Description: String read GetDescription;
    property Name: String read GetName;
    property Bits: Integer read GetBits;
    property Version: String read GetVersion;
  end;


  type
    EIdOpenSSLError = class(EIdException);
    EIdOpenSSLLoadError = class(EIdOpenSSLError);
    EIdOSSLCouldNotLoadSSLLibrary = class(EIdOpenSSLLoadError);
    EIdOSSLModeNotSet = class(EIdOpenSSLError);
    EIdOSSLGetMethodError = class(EIdOpenSSLError);
    EIdOSSLCreatingContextError = class(EIdOpenSSLError);
    EIdOSSLLoadingRootCertError = class(EIdOpenSSLLoadError);
    EIdOSSLLoadingCertError = class(EIdOpenSSLLoadError);
    EIdOSSLLoadingKeyError = class(EIdOpenSSLLoadError);
    EIdOSSLSettingCipherError = class(EIdOpenSSLError);
    EIdOSSLDataBindingError = class(EIdOpenSSLError);
    EIdOSSLAcceptError = class(EIdOpenSSLError);
    EIdOSSLConnectError = class(EIdOpenSSLError);

function LogicalAnd(A, B: Integer): Boolean;
procedure InfoCallback(sslSocket: PSSL; where: Integer; ret: Integer); cdecl;
function PasswordCallback(buf:PChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
function VerifyCallback(Ok: Integer; ctx: PX509_STORE_CTX):Integer; cdecl;

implementation

uses
  IdResourceStrings, SyncObjs;

var
  DLLLoadCount: Integer = 0;
  LockInfoCB: TCriticalSection;
  LockPassCB: TCriticalSection;
  LockVerifyCB: TCriticalSection;
  CallbackLockList: TThreadList;
  OpenSSLLibrarySection: TCriticalSection;

//////////////////////////////////////////////////////////////
// SSL SUPPORT FUNCTIONS
//////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
// SSL CALLBACK ROUTINES
//////////////////////////////////////////////////////////////

function PasswordCallback(buf:PChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var
  Password: String;
  IdSSLContext: TIdSSLContext;
begin
  LockPassCB.Enter;
  try
    Password := '';    {Do not Localize}

    IdSSLContext := TIdSSLContext(userdata);

    if (IdSSLContext.Parent is TIdSSLIOHandlerSocket) then begin
      TIdSSLIOHandlerSocket(IdSSLContext.Parent).DoGetPassword(Password);
    end;

    if (IdSSLContext.Parent is TIdServerIOHandlerSSL) then begin
      TIdServerIOHandlerSSL(IdSSLContext.Parent).DoGetPassword(Password);
    end;

    size := Length(Password);
    StrLCopy(buf, PChar(Password + #0), size + 1);
    Result := size;
  finally
    LockPassCB.Leave;
  end;
end;

procedure InfoCallback(sslSocket: PSSL; where: Integer; ret: Integer); cdecl;
var
  IdSSLSocket: TIdSSLSocket;
  StatusStr : String;
begin
  LockInfoCB.Enter;
  try
    IdSSLSocket := TIdSSLSocket(IdSslGetAppData(sslSocket));

    StatusStr := Format(RSOSSLStatusString, [StrPas(IdSslStateStringLong(sslSocket))]);

    if (IdSSLSocket.fParent is TIdSSLIOHandlerSocket) then begin
      TIdSSLIOHandlerSocket(IdSSLSocket.fParent).DoStatusInfo(StatusStr);
    end;

    if (IdSSLSocket.fParent is TIdServerIOHandlerSSL) then begin
      TIdServerIOHandlerSSL(IdSSLSocket.fParent).DoStatusInfo(StatusStr);
    end;
  finally
    LockInfoCB.Leave;
  end;

end;

{function RSACallback(sslSocket: PSSL; e: Integer; KeyLength: Integer):PRSA; cdecl;
const
  RSA: PRSA = nil;
var
  SSLSocket: TSSLWSocket;
  IdSSLSocket: TIdSSLSocket;
begin
  IdSSLSocket := TIdSSLSocket(IdSslGetAppData(sslSocket));

  if Assigned(IdSSLSocket) then begin
    IdSSLSocket.TriggerSSLRSACallback(KeyLength);
  end;

  if not Assigned(RSA) then begin
    RSA := f_RSA_generate_key(KeyLength, RSA_F4, @RSAProgressCallback, ssl);
  end;
  Result := RSA;
end;}

function AddMins (const DT: TDateTime; const Mins: Extended): TDateTime;
begin
  Result := DT + Mins / (60 * 24)
end;

function AddHrs (const DT: TDateTime; const Hrs: Extended): TDateTime;
begin
  Result := DT + Hrs / 24.0
end;

{function GetLocalTZBias: LongInt;
var
	TZ : TTimeZoneInformation;
begin
	case GetTimeZoneInformation (TZ) of
		TIME_ZONE_ID_STANDARD: Result := TZ.Bias + TZ.StandardBias;
		TIME_ZONE_ID_DAYLIGHT: Result := TZ.Bias + TZ.DaylightBias;
	else
		Result := TZ.Bias;
	end;
end;}

function GetLocalTime (const DT: TDateTime): TDateTime;
begin
  Result := DT - TimeZoneBias{ / (24 * 60)};
end;

procedure SslLockingCallback(mode, n : integer; Afile : PChar; line : integer) cdecl;
var
  Lock : TCriticalSection;
begin
  with CallbackLockList.LockList do
  try
    Lock := TCriticalSection(Items[n]);
  finally
    CallbackLockList.UnlockList;
  end;

  if (mode and OPENSSL_CRYPTO_LOCK) > 0 then
    Lock.Acquire
  else
    Lock.Release;
end;

procedure PrepareOpenSSLLocking;
var
  i, cnt : integer;
begin
  with CallbackLockList.LockList do
  try
    cnt := IdSslCryptoNumLocks;
    for i := 0 to cnt-1 do
      Add(TCriticalSection.Create);
  finally
    CallbackLockList.UnlockList;
  end;
end;

function _GetThreadID : integer cdecl;
begin
  Result := GetCurrentThreadHandle;
end;

function LoadOpenSLLibrary: Boolean;
begin
  if not IdSSLOpenSSLHeaders.Load then begin
    Result := False;
    Exit;
  end;

  InitializeRandom;
  // IdSslRandScreen;
  IdSslLoadErrorStrings;

  // Successful loading if true
  result := IdSslAddSslAlgorithms > 0;

  // Create locking structures, we need them for callback routines
  // they are probably not thread safe
  LockInfoCB := TCriticalSection.Create;
  LockPassCB := TCriticalSection.Create;
  LockVerifyCB := TCriticalSection.Create;

  // Handle internal OpenSSL locking
  CallbackLockList := TThreadList.Create;
  IdSslSetLockingCallback(SslLockingCallback);
  PrepareOpenSSLLocking;
  
  IdSslSetIdCallback(_GetThreadID);
end;

procedure UnLoadOpenSLLibrary;
var
  i : integer;
begin
  FreeAndNil(LockInfoCB);
  FreeAndNil(LockPassCB);
  FreeAndNil(LockVerifyCB);

  if Assigned(CallbackLockList) then
  begin
    with CallbackLockList.LockList do
      try
        for i := 0 to Count-1 do
          TObject(Items[i]).Free;

        Clear;
      finally
        CallbackLockList.UnlockList;
      end;
    FreeAndNil(CallbackLockList);
  end;

  IdSSLOpenSSLHeaders.Unload;
end;

function UTCTime2DateTime(UCTTime: PASN1_UTCTIME):TDateTime;
var
  year  : Word;
  month : Word;
  day   : Word;
  hour  : Word;
  min   : Word;
  sec   : Word;
  tz_h  : Integer;
  tz_m  : Integer;
begin
  Result := 0;
  if IdSslUCTTimeDecode(UCTTime, year, month, day, hour, min, sec, tz_h, tz_m) > 0 Then Begin
    Result := EncodeDate(year, month, day) + EncodeTime(hour, min, sec, 0);
    AddMins(Result, tz_m);
    AddHrs(Result, tz_h);
    Result := GetLocalTime(Result);
  end;
end;

function TranslateInternalVerifyToSLL(Mode: TIdSSLVerifyModeSet): Integer;
begin
  Result := OPENSSL_SSL_VERIFY_NONE;
  if sslvrfPeer in Mode then Result := Result or OPENSSL_SSL_VERIFY_PEER;
  if sslvrfFailIfNoPeerCert in Mode then Result:= Result or OPENSSL_SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
  if sslvrfClientOnce in Mode then Result:= Result or OPENSSL_SSL_VERIFY_CLIENT_ONCE;
end;

{function TranslateSLLVerifyToInternal(Mode: Integer): TIdSSLVerifyModeSet;
begin
  Result := [];
  if LogicalAnd(Mode, OPENSSL_SSL_VERIFY_PEER) then Result := Result + [sslvrfPeer];
  if LogicalAnd(Mode, OPENSSL_SSL_VERIFY_FAIL_IF_NO_PEER_CERT) then Result := Result + [sslvrfFailIfNoPeerCert];
  if LogicalAnd(Mode, OPENSSL_SSL_VERIFY_CLIENT_ONCE) then Result := Result + [sslvrfClientOnce];
end;}

function LogicalAnd(A, B: Integer): Boolean;
begin
  Result := (A and B) = B;
end;

function VerifyCallback(Ok: Integer; ctx: PX509_STORE_CTX): Integer; cdecl;
var
  hcert: PX509;
  Certificate: TIdX509;
  hSSL: PSSL;
  IdSSLSocket: TIdSSLSocket;
  // str: String;
  VerifiedOK: Boolean;
  Depth: Integer;
  // Error: Integer;
begin
  LockVerifyCB.Enter;
  try
    VerifiedOK := True;
    try
      hcert := IdSslX509StoreCtxGetCurrentCert(ctx);
      hSSL := IdSslX509StoreCtxGetAppData(ctx);
      Certificate := TIdX509.Create(hcert);

      if hSSL <> nil then begin
        IdSSLSocket := TIdSSLSocket(IdSslGetAppData(hSSL));
      end
      else begin
        Result := Ok;
        exit;
      end;

      //Error :=
      IdSslX509StoreCtxGetError(ctx);
      Depth := IdSslX509StoreCtxGetErrorDepth(ctx);
    //  str := Format('Certificate: %s', [Certificate.Subject.OneLine]);    {Do not Localize}
    //  str := IdSSLSocket.GetSessionIDAsString;
    //  ShowMessage(str);

      if (IdSSLSocket.fParent is TIdSSLIOHandlerSocket) then begin
        VerifiedOK := TIdSSLIOHandlerSocket(IdSSLSocket.fParent).DoVerifyPeer(Certificate);
      end;

      if (IdSSLSocket.fParent is TIdServerIOHandlerSSL) then begin
        VerifiedOK := TIdServerIOHandlerSSL(IdSSLSocket.fParent).DoVerifyPeer(Certificate);
      end;

      if not ((Ok>0) and (IdSSLSocket.fSSLContext.VerifyDepth>=Depth)) then begin
        Ok := 0;
        {if Error = OPENSSL_X509_V_OK then begin
          Error := OPENSSL_X509_V_ERR_CERT_CHAIN_TOO_LONG;
        end;}
      end;
      FreeAndNil(Certificate);
    except
    end;
    if VerifiedOK and (Ok > 0) then begin
      Result := 1;
    end
    else begin
      Result := 0;
    end;

  //  Result := Ok; // testing
  finally
    LockVerifyCB.Leave;
  end;
end;

//////////////////////////////////////////////////////
//   TIdSSLOptions
///////////////////////////////////////////////////////

procedure TIdSSLOptions.AssignTo(ASource: TPersistent);
begin
  if ASource is TIdSSLOptions then
    with TIdSSLOptions(ASource) do begin
      RootCertFile := Self.RootCertFile;
      CertFile := Self.CertFile;
      KeyFile := Self.KeyFile;
      Method := Self.Method;
      Mode := Self.Mode;
      VerifyMode := Self.VerifyMode;
      VerifyDepth := Self.VerifyDepth;
      VerifyDirs := Self.VerifyDirs;
      CipherList := Self.CipherList;
    end
  else
    inherited AssignTo(ASource);
end;

///////////////////////////////////////////////////////
//   TIdServerIOHandlerSSL
///////////////////////////////////////////////////////

{ TIdServerIOHandlerSSL }

constructor TIdServerIOHandlerSSL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIsInitialized := False;
  fxSSLOptions := TIdSSLOptions.Create;
end;

destructor TIdServerIOHandlerSSL.Destroy;
begin
  if fSSLContext <> nil then begin
    FreeAndNil(fSSLContext);
  end;

  FreeAndNil(fxSSLOptions);
  inherited Destroy;
end;

procedure TIdServerIOHandlerSSL.Init;
begin
  // CreateSSLContext(SSLOptions.fMode);
  // CreateSSLContext;
  fSSLContext := TIdSSLContext.Create;
  with fSSLContext do begin
    Parent := self;
    RootCertFile := SSLOptions.RootCertFile;
    CertFile := SSLOptions.CertFile;
    KeyFile := SSLOptions.KeyFile;

    fVerifyDepth := SSLOptions.fVerifyDepth;
    fVerifyMode := SSLOptions.fVerifyMode;
    // fVerifyFile := SSLOptions.fVerifyFile;
    fVerifyDirs := SSLOptions.fVerifyDirs;
    fCipherList := SSLOptions.fCipherList;

    if Assigned(fOnVerifyPeer) then begin
      VerifyOn := True;
    end
    else begin
      VerifyOn := False;
    end;

    if Assigned(fOnStatusInfo) then begin
      StatusInfoOn := True;
    end
    else begin
      StatusInfoOn := False;
    end;

    {if Assigned(fOnGetPassword) then begin
      PasswordRoutineOn := True;
    end
    else begin
      PasswordRoutineOn := False;
    end;}

    fMethod :=  SSLOptions.Method;
    fMode := SSLOptions.Mode;
    fSSLContext.InitContext(sslCtxServer);
  end;

  fIsInitialized := True;
end;

function TIdServerIOHandlerSSL.Accept(ASocket: TIdStackSocketHandle; AThread: TIdThread = nil): TIdIOHandler;
var
  tmpIdCIOpenSSL: TIdSSLIOHandlerSocket;
begin
  if not fIsInitialized then begin
    Init;
  end;

  tmpIdCIOpenSSL := TIdSSLIOHandlerSocket.Create(nil); // Was self
  tmpIdCIOpenSSL.fIsPeer := True;
  tmpIdCIOpenSSL.Open;
  if tmpIdCIOpenSSL.Binding.Accept(ASocket) then begin
    tmpIdCIOpenSSL.fxSSLOptions.Assign(fxSSLOptions);
    tmpIdCIOpenSSL.fSSLSocket := TIdSSLSocket.Create(self);
    tmpIdCIOpenSSL.fSSLContext := fSSLContext;
    result := tmpIdCIOpenSSL;
  end
  else begin
    result := nil;
    FreeAndNil(tmpIdCIOpenSSL);
  end;
end;

procedure TIdServerIOHandlerSSL.DoStatusInfo(Msg: String);
begin
  if Assigned(fOnStatusInfo) then
    fOnStatusInfo(Msg);
end;

procedure TIdServerIOHandlerSSL.DoGetPassword(var Password: String);
begin
  if Assigned(fOnGetPassword) then
    fOnGetPassword(Password);
end;

function TIdServerIOHandlerSSL.DoVerifyPeer(Certificate: TIdX509): Boolean;
begin
  Result := True;
  if Assigned(fOnVerifyPeer) then
    Result := fOnVerifyPeer(Certificate);
end;

///////////////////////////////////////////////////////
//   TIdSSLIOHandlerSocket
///////////////////////////////////////////////////////

{ TIdSSLIOHandlerSocket }

constructor TIdSSLIOHandlerSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIsPeer := False;
  fxSSLOptions := TIdSSLOptions.Create;
  fSSLLayerClosed := True;
end;

destructor TIdSSLIOHandlerSocket.Destroy;
begin
  FreeAndNil(fxSSLOptions); //Added
  FreeAndNil(fSSLSocket);
  // FreeAndNil(fSSLContext);
  if not fIsPeer then begin
    FreeAndNil(fSSLContext);
  end;
  inherited Destroy;
end;

procedure TIdSSLIOHandlerSocket.ConnectClient(const AHost: string; const APort: Integer; const ABoundIP: string;
     const ABoundPort: Integer; const ABoundPortMin: Integer; const ABoundPortMax: Integer;
     const ATimeout: Integer = IdTimeoutDefault);
begin
  inherited ConnectClient(AHost, APort, ABoundIP, ABoundPort, ABoundPortMin, ABoundPortMax, ATimeout);

  DoBeforeConnect(self);

  // CreateSSLContext(sslmClient);
  // CreateSSLContext(SSLOptions.fMode);

  try
    Init;
  except
    on EIdOSSLCouldNotLoadSSLLibrary do begin
      if not PassThrough then raise;
    end;
  end;

  if not PassThrough then begin
    OpenEncodedConnection;
  end;
end;

procedure TIdSSLIOHandlerSocket.Close;
begin
  FreeAndNil(fSSLSocket);
  if not fIsPeer then begin
    FreeAndNil(fSSLContext);
  end;

  inherited Close;
end;

procedure TIdSSLIOHandlerSocket.Open;
begin
  inherited Open;
end;

function TIdSSLIOHandlerSocket.Recv(var ABuf; ALen: integer): integer;
begin
  if fPassThrough then begin
    result := inherited Recv(ABuf, ALen);
  end
  else begin
    result := RecvEnc(ABuf, ALen);
  end;
end;

function TIdSSLIOHandlerSocket.Send(var ABuf; ALen: integer): integer;
begin
  if fPassThrough then begin
    result := inherited Send(ABuf, ALen);
  end
  else begin
    result := SendEnc(ABuf, ALen);
  end;
end;

procedure TIdSSLIOHandlerSocket.SetPassThrough(const Value: Boolean);
begin
  if not Value then begin
    if Connected then begin
      if Assigned(fSSLContext) then begin
        OpenEncodedConnection;
      end
      else begin
        raise EIdOSSLCouldNotLoadSSLLibrary.Create(RSOSSLCouldNotLoadSSLLibrary);
      end;
    end;
  end;
  fPassThrough := Value;
end;

function TIdSSLIOHandlerSocket.RecvEnc(var ABuf; ALen: integer): integer;
begin
  Result := fSSLSocket.Recv(ABuf, ALen);
end;

function TIdSSLIOHandlerSocket.SendEnc(var ABuf; ALen: integer): integer;
begin
  Result := fSSLSocket.Send(ABuf, ALen);
end;

procedure TIdSSLIOHandlerSocket.AfterAccept;
begin
  try
    inherited AfterAccept;
    fSSLSocket.Accept(Binding.Handle, fSSLContext);
  except
    Close;
    raise;
  end;
end;

procedure TIdSSLIOHandlerSocket.Init;
begin
  fSSLContext := TIdSSLContext.Create;
  with fSSLContext do begin
    Parent := self;
    RootCertFile := SSLOptions.RootCertFile;
    CertFile := SSLOptions.CertFile;
    KeyFile := SSLOptions.KeyFile;

    fVerifyDepth := SSLOptions.fVerifyDepth;
    fVerifyMode := SSLOptions.fVerifyMode;
    // fVerifyFile := SSLOptions.fVerifyFile;
    fVerifyDirs := SSLOptions.fVerifyDirs;
    fCipherList := SSLOptions.fCipherList;

    if Assigned(fOnVerifyPeer) then begin
      VerifyOn := True;
    end
    else begin
      VerifyOn := False;
    end;

    if Assigned(fOnStatusInfo) then begin
      StatusInfoOn := True;
    end
    else begin
      StatusInfoOn := False;
    end;

    {if Assigned(fOnGetPassword) then begin
      PasswordRoutineOn := True;
    end
    else begin
      PasswordRoutineOn := False;
    end;}


    fMethod :=  SSLOptions.Method;
    fMode := SSLOptions.Mode;
    fSSLContext.InitContext(sslCtxClient);
  end;


  {fSSLContext := TIdSSLContext.Create;
  with fSSLContext do begin
    Parent := self;
    RootCertFile := SSLOptions.RootCertFile;
    CertFile := SSLOptions.CertFile;
    KeyFile := SSLOptions.KeyFile;

    if Assigned(fOnStatusInfo) then begin
      StatusInfoOn := True;
    end
    else begin
      StatusInfoOn := False;
    end;

    if Assigned(fOnVerifyPeer) then begin
      VerifyOn := True;
    end
    else begin
      VerifyOn := False;
    end;

    // Must set mode after above props are set
    Method :=  SSLOptions.Method;
    Mode := axMode;
  end;}
end;

//}
{function TIdSSLIOHandlerSocket.GetPeerCert: TIdX509;
begin
  if fSSLContext <> nil then begin
    Result := fSSLSocket.PeerCert;
  end
  else begin
    Result := nil;
  end;
end;}

procedure TIdSSLIOHandlerSocket.DoStatusInfo(Msg: String);
begin
  if Assigned(fOnStatusInfo) then
    fOnStatusInfo(Msg);
end;

procedure TIdSSLIOHandlerSocket.DoGetPassword(var Password: String);
begin
  if Assigned(fOnGetPassword) then
    fOnGetPassword(Password);
end;

function TIdSSLIOHandlerSocket.DoVerifyPeer(Certificate: TIdX509): Boolean;
begin
  Result := True;
  if Assigned(fOnVerifyPeer) then
    Result := fOnVerifyPeer(Certificate);
end;

procedure TIdSSLIOHandlerSocket.OpenEncodedConnection;
begin
  if not Assigned(fSSLSocket) then
  begin
    fSSLSocket := TIdSSLSocket.Create(self);
    fSSLSocket.fSSLContext := fSSLContext;
    fSSLSocket.Connect(Binding.Handle, fSSLContext);
  end;
end;

procedure TIdSSLIOHandlerSocket.DoBeforeConnect(ASender: TIdSSLIOHandlerSocket);
begin
  if Assigned(OnBeforeConnect) then begin
    OnBeforeConnect(Self);
  end;
end;

{ TIdSSLContext }

constructor TIdSSLContext.Create;
begin
  inherited Create;

  OpenSSLLibrarySection.Enter;
  try
    if DLLLoadCount <= 0 then begin
      if not IdSSLOpenSSL.LoadOpenSLLibrary then begin
        raise EIdOSSLCouldNotLoadSSLLibrary.Create(RSOSSLCouldNotLoadSSLLibrary);
      end;
    end;
    Inc(DLLLoadCount);
  finally
    OpenSSLLibrarySection.Leave;
  end;

  fVerifyMode := [];
  fMode := sslmUnassigned;
  fSessionId := 1;
end;

destructor TIdSSLContext.Destroy;
begin
  DestroyContext;
  inherited Destroy;
end;

procedure TIdSSLContext.DestroyContext;
begin
  if fContext <> nil then begin
    IdSslCtxFree(fContext);
    fContext := nil;
  end;
end;

procedure TIdSSLContext.InitContext(CtxMode: TIdSSLCtxMode);
var
  SSLMethod: PSSL_METHOD;
  error: Integer;
  pCipherList, pRootCertFile: PChar;
//  pCAname: PSTACK_X509_NAME;
begin
  // Destroy the context first
  DestroyContext;

  if fMode = sslmUnassigned then begin
    if CtxMode = sslCtxServer then begin
      fMode := sslmServer;
    end
    else begin
      fMode := sslmClient;
    end
  end;

  // get SSL method function (SSL2, SSL23, SSL3, TLS)
  SSLMethod := SetSSLMethod;

  // create new SSL context
  fContext := IdSslCtxNew(SSLMethod);
  if fContext = nil then begin
    raise EIdOSSLCreatingContextError.Create(RSSSLCreatingContextError);
  end;

  // assign a password lookup routine
//  if PasswordRoutineOn then begin
    IdSslCtxSetDefaultPasswdCb(fContext, @PasswordCallback);
    IdSslCtxSetDefaultPasswdCbUserdata(fContext, self);
//  end;

  IdSSLCtxSetDefaultVerifyPaths(fContext);

  // load key and certificate files
  if RootCertFile <> '' then begin    {Do not Localize}
    if not LoadRootCert then begin
      raise EIdOSSLLoadingRootCertError.Create(RSSSLLoadingRootCertError);
    end;
  end;

  if CertFile <> '' then begin    {Do not Localize}
    if not LoadCert then begin
      raise EIdOSSLLoadingCertError.Create(RSSSLLoadingCertError);
    end;
  end;

  if KeyFile <> '' then begin    {Do not Localize}
    if not LoadKey then begin
      raise EIdOSSLLoadingKeyError.Create(RSSSLLoadingKeyError);
    end;
  end;

  if StatusInfoOn then begin
    IdSslCtxSetInfoCallback(fContext, PFunction(@InfoCallback));
  end;

//    f_SSL_CTX_set_tmp_rsa_callback(hSSLContext, @RSACallback);

  if fCipherList <> '' then begin    {Do not Localize}
    pCipherList := StrNew(PChar(fCipherList));
    error := IdSslCtxSetCipherList(fContext, pCipherList);
    StrDispose(pCipherList);
  end
  else begin
    error := IdSslCtxSetCipherList(fContext, OPENSSL_SSL_DEFAULT_CIPHER_LIST);
  end;
  if error <= 0 then begin
    raise EIdOSSLSettingCipherError.Create(RSSSLSettingCipherError);
  end;

  if fVerifyMode <> [] then begin
    SetVerifyMode(fVerifyMode, VerifyOn);
  end;

  if CtxMode = sslCtxServer then begin
    IdSSLCtxSetSessionIdContext(fContext, PChar(@fSessionId), SizeOf(fSessionId));
  end;

  // CA list
  if RootCertFile <> '' then begin    {Do not Localize}
    pRootCertFile := StrNew(PChar(RootCertFile));
    IdSSLCtxSetClientCAList(fContext, IdSSLLoadClientCAFile(pRootCertFile));
    StrDispose(pRootCertFile);
  end

end;

procedure TIdSSLContext.SetVerifyMode(Mode: TIdSSLVerifyModeSet; CheckRoutine: Boolean);
begin
  if fContext<>nil then begin
//    IdSSLCtxSetDefaultVerifyPaths(fContext);
    if CheckRoutine then begin
      IdSslCtxSetVerify(fContext, TranslateInternalVerifyToSLL(Mode), PFunction(@VerifyCallback));
    end
    else begin
      IdSslCtxSetVerify(fContext, TranslateInternalVerifyToSLL(Mode), nil);
    end;
    IdSslCtxSetVerifyDepth(fContext, fVerifyDepth);
  end;
end;

function TIdSSLContext.GetVerifyMode: TIdSSLVerifyModeSet;
begin
  Result := fVerifyMode;
end;
{
function TIdSSLContext.LoadVerifyLocations(FileName: String; Dirs: String): Boolean;
var
  pFileName, pDirs : PChar;
begin
  Result := False;

  pFileName := nil;
  pDirs := nil;
  if FileName <> '' then begin
    pFileName := StrNew(PChar(FileName));
  end;
  if Dirs <> '' then begin  
    pDirs := StrNew(PChar(Dirs));
  end;

  If (pDirs<>nil) or (pFileName<>nil) Then begin
    If IdSslCtxLoadVerifyLocations(fContext, pFileName, pDirs)<=0 Then Begin
      raise EIdOSSLCouldNotLoadSSLLibrary.Create(RSOSSLCouldNotLoadSSLLibrary);
      exit;
    End;
  end;
  StrDispose(pFileName);
  StrDispose(pDirs);
  Result:=True;
End;
}
function TIdSSLContext.SetSSLMethod: PSSL_METHOD;
begin
  if fMode = sslmUnassigned then begin
  	raise EIdOSSLModeNotSet.create(RSOSSLModeNotSet);
  end;
  case fMethod of
    sslvSSLv2:
      case fMode of
        sslmServer : Result := IdSslMethodServerV2;
        sslmClient : Result := IdSslMethodClientV2;
        sslmBoth   : Result := IdSslMethodV2;
      else
        Result := IdSslMethodV2;
      end;

    sslvSSLv23:
      case fMode of
        sslmServer : Result := IdSslMethodServerV23;
        sslmClient : Result := IdSslMethodClientV23;
        sslmBoth   : Result := IdSslMethodV23;
      else
        Result := IdSslMethodV23;
      end;

    sslvSSLv3:
      case fMode of
        sslmServer : Result := IdSslMethodServerV3;
        sslmClient : Result := IdSslMethodClientV3;
        sslmBoth   : Result := IdSslMethodV3;
      else
        Result := IdSslMethodV3;
      end;

    sslvTLSv1:
      case fMode of
        sslmServer : Result := IdSslMethodServerTLSV1;
        sslmClient : Result := IdSslMethodClientTLSV1;
        sslmBoth   : Result := IdSslMethodTLSV1;
      else
        Result := IdSslMethodTLSV1;
      end;
  else
    raise EIdOSSLGetMethodError.Create(RSSSLGetMethodError);
  end;
end;

function TIdSSLContext.LoadRootCert: Boolean;
var
  pStr: PChar;
  error: Integer;
//  pDirs : PChar;
begin
  pStr := StrNew(PChar(RootCertFile));
{  if fVerifyDirs <> '' then begin    
    pDirs := StrNew(PChar(fVerifyDirs));
    error := IdSslCtxLoadVerifyLocations(
                   fContext,
                   pStr,
                   pDirs);
    StrDispose(pDirs);
  end
  else begin
}
    error := IdSslCtxLoadVerifyLocations(
                   fContext,
                   pStr,
                   nil);
{  end;}
  if error <= 0 then begin
    Result := False
  end else begin
    Result := True;
  end;

  StrDispose(pStr);
end;

function TIdSSLContext.LoadCert: Boolean;
var
  pStr: PChar;
  error: Integer;
begin
  pStr := StrNew(PChar(CertFile));
  error := IdSslCtxUseCertificateFile(
                 fContext,
                 pStr,
                 OPENSSL_SSL_FILETYPE_PEM);
  if error <= 0 then
    Result := False
  else
    Result := True;

  StrDispose(pStr);
end;

function TIdSSLContext.LoadKey: Boolean;
var
  pStr: PChar;
  error: Integer;
begin
  Result := True;

  pStr := StrNew(PChar(fsKeyFile));
  error := IdSslCtxUsePrivateKeyFile(
                 fContext,
                 pStr,
                 OPENSSL_SSL_FILETYPE_PEM);

  if error <= 0 then begin
    Result := False;
  end else begin
    error := IdSslCtxCheckPrivateKeyFile(fContext);
    if error <= 0 then begin
      Result := False;
    end;
  end;

  StrDispose(pStr);
end;


//////////////////////////////////////////////////////////////

{ TIdSSLSocket }

constructor TIdSSLSocket.Create(Parent: TObject);
begin
  inherited Create;
  fParent := Parent;
  fSSLContext := nil;
end;

destructor TIdSSLSocket.Destroy;
begin
  if fSSL <> nil then begin
    //IdSslSetShutdown(fSSL, OPENSSL_SSL_SENT_SHUTDOWN);
    IdSslShutdown(fSSL);
    IdSslFree(fSSL);
    fSSL := nil;
  end;
  if fSSLCipher <> nil then begin
    FreeAndNil(fSSLCipher);
  end;
  inherited Destroy;
end;

function TIdSSLSocket.GetSSLError(retCode: Integer): Integer;
begin
  // COMMENT!!!
  // I found out that SSL layer should not interpret errors, cause they will pop up
  // on the socket layer. Only thing that the SSL layer should consider is key
  // or protocol renegotiation. This is done by loop in read and write

  Result := IdSslGetError(fSSL, retCode);
  case Result of
    OPENSSL_SSL_ERROR_NONE:
      Result := OPENSSL_SSL_ERROR_NONE;
    OPENSSL_SSL_ERROR_WANT_WRITE:
      Result := OPENSSL_SSL_ERROR_WANT_WRITE;
    OPENSSL_SSL_ERROR_WANT_READ:
      Result := OPENSSL_SSL_ERROR_WANT_READ;
    OPENSSL_SSL_ERROR_ZERO_RETURN:
      Result := OPENSSL_SSL_ERROR_ZERO_RETURN;
      //Result := OPENSSL_SSL_ERROR_NONE;
      {  
      // ssl layer has been disconnected, it is not necessary that also
      // socked has been closed
      case Mode of
        sslemClient: begin
          case Action of
            sslWrite: begin
              if retCode = 0 then begin
                Result := 0;
              end
              else begin
                raise EIdException.Create(RSOSSLConnectionDropped);
              end;
            end;
        end;
      end;}

        //raise EIdException.Create(RSOSSLConnectionDropped);
      // X509_LOOKUP event is not really an error, just an event
    // OPENSSL_SSL_ERROR_WANT_X509_LOOKUP:
        // raise EIdException.Create(RSOSSLCertificateLookup);
    OPENSSL_SSL_ERROR_SYSCALL:
      Result := OPENSSL_SSL_ERROR_SYSCALL;
      // Result := OPENSSL_SSL_ERROR_NONE;

        {//raise EIdException.Create(RSOSSLInternal);
        if (retCode <> 0) or (DataLen <> 0) then begin
          raise EIdException.Create(RSOSSLConnectionDropped);
        end
        else begin
          Result := 0;
        end;}

    OPENSSL_SSL_ERROR_SSL:
      // raise EIdException.Create(RSOSSLInternal);
      Result := OPENSSL_SSL_ERROR_SSL;
      // Result := OPENSSL_SSL_ERROR_NONE;
  end;
end;

procedure TIdSSLSocket.Accept(const pHandle: TIdStackSocketHandle; fSSLContext: TIdSSLContext);
var
  err: Integer;
  StatusStr: String;
begin
  fSSL := IdSslNew(fSSLContext.fContext);
  if fSSL = nil then exit;

  if IdSslSetAppData(fSSL, self) <= 0 then begin
    raise EIdOSSLDataBindingError.Create(RSSSLDataBindingError);
    exit;
  end;

  self.fSSLContext := fSSLContext;
  IdSslSetFd(fSSL, pHandle);
  err := IdSslAccept(fSSL);
  if err <= 0 then begin
    // err := GetSSLError(err);

    {if err <= -1 then
      raise EIdOSSLAcceptError.Create(RSSSLAcceptError)
    else}
    raise EIdOSSLAcceptError.Create(RSSSLAcceptError);
  end;

  StatusStr := 'Cipher: name = ' + Cipher.Name + '; ' +    {Do not Localize}
               'description = ' + Cipher.Description + '; ' +    {Do not Localize}
               'bits = ' + IntToStr(Cipher.Bits) + '; ' +    {Do not Localize}
               'version = ' + Cipher.Version + '; ';    {Do not Localize}

  if (fParent is TIdServerIOHandlerSSL) then begin
    (fParent as TIdServerIOHandlerSSL).DoStatusInfo(StatusStr);
  end;

end;

procedure TIdSSLSocket.Connect(const pHandle: TIdStackSocketHandle; fSSLContext: TIdSSLContext);
var
  error: Integer;
  StatusStr: String;
begin
  fSSL := IdSslNew(fSSLContext.fContext);
  if fSSL = nil then exit;

  if IdSslSetAppData(fSSL, self) <= 0 then begin
    raise EIdOSSLDataBindingError.Create(RSSSLDataBindingError);
    exit;
  end;

  IdSslSetFd(fSSL, pHandle);
  error := IdSslConnect(fSSL);
  if error <= 0 then begin
//    error2 := IdSslGetError(fSSL, error);
    raise EIdOSSLConnectError.Create(RSSSLConnectError);
  end;

  StatusStr := 'Cipher: name = ' + Cipher.Name + '; ' +    {Do not Localize}
               'description = ' + Cipher.Description + '; ' +    {Do not Localize}
               'bits = ' + IntToStr(Cipher.Bits) + '; ' +    {Do not Localize}
               'version = ' + Cipher.Version + '; ';    {Do not Localize}

  if (fParent is TIdSSLIOHandlerSocket) then begin
    (fParent as TIdSSLIOHandlerSocket).DoStatusInfo(StatusStr);
  end;

end;

function TIdSSLSocket.Recv(var ABuf; ALen: integer): integer;
var
  err: Integer;
begin
  Result := IdSslRead(fSSL, @ABuf, ALen);
  err := GetSSLError(Result);
  if (err = OPENSSL_SSL_ERROR_WANT_READ) or (err = OPENSSL_SSL_ERROR_WANT_WRITE) then begin
    Result := IdSslRead(fSSL, @ABuf, ALen);
  end;
end;

function TIdSSLSocket.Send(var ABuf; ALen: integer): integer;
var
  err: Integer;
begin
  Result := IdSslWrite(fSSL, @ABuf, ALen);
  err := GetSSLError(Result);
  if (err = OPENSSL_SSL_ERROR_WANT_READ) or (err = OPENSSL_SSL_ERROR_WANT_WRITE) then begin
    Result := IdSslWrite(fSSL, @ABuf, ALen);
  end;
end;

function TIdSSLSocket.GetPeerCert: TIdX509;
var
  X509: PX509;
begin
  if fPeerCert = nil then begin
    X509 := IdSslGetPeerCertificate(fSSL);
    if X509 <> nil then begin
      fPeerCert := TIdX509.Create(X509);
    end;
  end;
  Result := fPeerCert;
end;

function TIdSSLSocket.GetSSLCipher: TIdSSLCipher;
begin
  if (fSSLCipher = nil) and (fSSL<>nil) then begin
    fSSLCipher := TIdSSLCipher.Create(self);
  end;
  Result := fSSLCipher;
end;


function TIdSSLSocket.GetSessionID: TByteArray;
var
  pSession: PSSL_SESSION;
  tmpArray: TByteArray;
begin
  Result.Length := 0;
  FillChar(tmpArray, SizeOf(TByteArray), 0);
  if fSSL<>nil then begin
    pSession := IdSslGetSession(fSSL);
    if pSession <> nil then begin
      IdSslSessionGetId(pSession, @tmpArray.Data, @tmpArray.Length);
      Result := tmpArray;
    end;
  end;
end;

function  TIdSSLSocket.GetSessionIDAsString:String;
var
  Data: TByteArray;
  i: Integer;
begin
  Result := '';    {Do not Localize}
  Data := GetSessionID;
  for i := 0 to Data.Length-1 do begin
    Result := Result+Format('%.2x', [Byte(Data.Data[I])]);{do not localize}
  end;
end;

procedure TIdSSLSocket.SetCipherList(CipherList: String);
//var
//  tmpPStr: PChar;
begin
{
  fCipherList := CipherList;
  fCipherList_Ch:=True;
  aCipherList:=aCipherList+#0;
  If hSSL<>nil Then f_SSL_set_cipher_list(hSSL, @aCipherList[1]);
}
end;

///////////////////////////////////////////////////////////////
//  X509 Certificate
///////////////////////////////////////////////////////////////

{ TIdX509Name }

function TIdX509Name.CertInOneLine: String;
var
  OneLine: Array[0..2048] of Char;
begin
  if FX509Name = nil then begin
    Result := '';    {Do not Localize}
  end
  else begin
    Result := StrPas(IdSslX509NameOneline(FX509Name, PChar(@OneLine), sizeof(OneLine)));
  end;
end;

function TIdX509Name.GetHash: TULong;
begin
  if FX509Name = nil then begin
    FillChar(Result, SizeOf(Result), 0)
  end
  else begin
    Result.C1 := IdSslX509NameHash(FX509Name);
  end;
end;

function TIdX509Name.GetHashAsString: String;
begin
  Result := Format('%.8x', [Hash.L1]); {do not localize}
end;

constructor TIdX509Name.Create(aX509Name: PX509_NAME);
begin
  Inherited Create;

  FX509Name := aX509Name;
end;


///////////////////////////////////////////////////////////////
//  X509 Certificate
///////////////////////////////////////////////////////////////

{ TIdX509 }

constructor TIdX509.Create(aX509: PX509);
begin
  inherited Create;

  FX509 := aX509;
  FSubject := nil;
  FIssuer := nil;
end;

destructor TIdX509.Destroy;
begin
  if Assigned(FSubject) then FSubject.Destroy;
  if Assigned(FIssuer) then FIssuer.Destroy;

  inherited Destroy;
end;

function TIdX509.RSubject: TIdX509Name;
var
  x509_name: PX509_NAME;
Begin
  if not Assigned(FSubject) then begin
    if FX509<>nil then
      x509_name := IdSslX509GetSubjectName(FX509)
    else
      x509_name := nil;
    FSubject := TIdX509Name.Create(x509_name);
  end;
  Result := FSubject;
end;

function TIdX509.RIssuer: TIdX509Name;
var
  x509_name: PX509_NAME;
begin
  if not Assigned(FIssuer) then begin
    if FX509<>nil then
      x509_name := IdSslX509GetIssuerName(FX509)
    else
      x509_name := nil;
    FIssuer := TIdX509Name.Create(x509_name);
  End;
  Result := FIssuer;
end;

function TIdX509.RFingerprint: TEVP_MD;
begin
  IdSslX509Digest(FX509, IdSslEvpMd5, PChar(@Result.MD), @Result.Length);
end;

function TIdX509.RFingerprintAsString: String;
var
  I: Integer;
  EVP_MD: TEVP_MD;
begin
  Result := '';
  EVP_MD := Fingerprint;
  for I := 0 to EVP_MD.Length - 1 do begin
    if I <> 0 then Result := Result + ':';    {Do not Localize}
    Result := Result + Format('%.2x', [Byte(EVP_MD.MD[I])]);  {do not localize}
  end;
end;

function TIdX509.RnotBefore:TDateTime;
begin
  if FX509=nil then
    Result := 0
  else
    Result := UTCTime2DateTime(IdSslX509GetNotBefore(FX509));
end;


function TIdX509.RnotAfter:TDateTime;
begin
  if FX509=nil then
    Result := 0
  else
    Result := UTCTime2DateTime(IdSslX509GetNotAfter(FX509));
end;

///////////////////////////////////////////////////////////////
//  TIdSSLCipher
///////////////////////////////////////////////////////////////
constructor TIdSSLCipher.Create(AOwner: TIdSSLSocket);
begin
  inherited Create;

  FSSLSocket := AOwner;
end;

destructor TIdSSLCipher.Destroy;
begin
  inherited Destroy;
end;

function TIdSSLCipher.GetDescription;
var
  Buf: Array[0..1024] of Char;
begin
  Result := StrPas(IdSSLCipherDescription(IdSSLGetCurrentCipher(FSSLSocket.fSSL), @Buf[0], SizeOf(Buf)-1));
end;

function TIdSSLCipher.GetName:String;
begin
  Result := StrPas(IdSSLCipherGetName(IdSSLGetCurrentCipher(FSSLSocket.fSSL)));
end;

function TIdSSLCipher.GetBits:Integer;
begin
  IdSSLCipherGetBits(IdSSLGetCurrentCipher(FSSLSocket.fSSL), @Result);
end;

function TIdSSLCipher.GetVersion:String;
begin
  Result := StrPas(IdSSLCipherGetVersion(IdSSLGetCurrentCipher(FSSLSocket.fSSL)));
end;

initialization
  // Let's load the library    {Do not Localize}
  //if DLLLoadCount <= 0 then begin
   {
  	if not LoadOpenSLLibrary then begin
    	raise EIdException.Create(RSOSSLCouldNotLoadSSLLibrary);
    end;
   }
  //end;
  //Inc(DLLLoadCount);
  OpenSSLLibrarySection := TCriticalSection.Create;


finalization
  // if DLLLoadCount = 0 then begin
  UnLoadOpenSLLibrary;
  OpenSSLLibrarySection.Free;
  // end;
end.
