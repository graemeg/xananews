{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSSynaSock Unit                                  }
{
  LICENSE

  Copyright (c) 2004, Henrick Wibell Hellström, StreamSec
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of StreamSec nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
{*******************************************************}
{$I ver.inc}
unit TlsSynaSock;

interface
{$IFDEF SHA1_AND_MD5}

uses
  SysUtils, Classes, SyncObjs, SynSock, BlckSock, Tls, TlsInternalServer;

type
  // TLS wrapper classes:
  TCustomTLSSynSock = class(TAbstractTLSSocket)
  private
    FSocket: TBlockSocket;
    FPort: Integer;
    FHost: string;
    FService: string;
    FAddress: string;
    FRemoteHost: string;
    FRemoteAddress: string;
    FRemotePort: Integer;
    procedure SetAddress(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetService(const Value: string);
    function GetRemoteHost: string;
    function GetRemoteAddress: string;
    function GetRemotePort: Integer;
  protected
    procedure DoRead; virtual;
    procedure RawClose; override;
    procedure RawConnect; override;
    function RawReceive: TStream; override;
    procedure RawSend(Strm: TCustomMemoryStream); override;
  public
    constructor Create(ASocket: TBlockSocket); virtual;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
    property RemoteHost: string read GetRemoteHost;
    property RemoteAddress: string read GetRemoteAddress;
    property RemotePort: Integer read GetRemotePort;
  end;

  TNewSynConnectEvent = procedure (Sender: TObject; Socket: TCustomTLSSynSock) of object;

  TTLSSynSockSlave = class(TCustomTLSSynSock)
  private
    FSlaveSocket: TBlockSocket;
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Receive; override;
    procedure Send; override;
    procedure DoConnect; virtual;
  public
    constructor CreateSocket(ASocket: TSocket);
    destructor Destroy; override;
  end;

  TBlockTLSSynSock = class(TTLSSynSockSlave)
  private
    FOnConnect: TNotifyEvent;
    procedure SetOnConnect(const Value: TNotifyEvent);
  protected
    procedure DoConnect; override;
  public
    property OnConnect: TNotifyEvent read FOnConnect write SetOnConnect;
  end;

  // TSynSocketStream is used internally as a substitute for
  // ScktComp.TWinSocketStream
  TSynSocketStream = class(TStream)
  private
    FSocket: TBlockSocket;
    FTimeout: Longint;
  public
    constructor Create(ASocket: TBlockSocket; TimeOut: Longint);
    function WaitForData(Timeout: Longint): Boolean;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property TimeOut: Longint read FTimeout write FTimeout;
  end;

  // Synapse descendant. It encapsulates a TTLSSynSockSlave for SSL/TLS I/O,
  // which in turn encapsulates a TTCPBlockSocket for TCP/IP I/O.
  TSsTCPBlockSocket = class(TTCPBlockSocket)
  private
    FTLSServer: TCustomTLSInternalServer;
    FTLSSocket: TTLSSynSockSlave;
    FBlockingRead: Boolean;
    FURIToCheck: string;     
    FSessionID: TSessionID;
    FCipherSuite: TCipherSuites;
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    procedure SetBlockingRead(const Value: Boolean);
    procedure SetURIToCheck(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseSocket; override;
    function WaitingData: Integer; override;
    procedure Connect(IP, Port: string); override;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvPacket(Timeout: Integer): string; override;
    property TLSSocket: TTLSSynSockSlave read FTLSSocket;
    property URIToCheck: string read FURIToCheck write SetURIToCheck;
  published
    property BlockingRead: Boolean read FBlockingRead write SetBlockingRead;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

  TSynaServerSocket = class; // forward declaration
  TTLSSynaServer = class; // forward declaration
  TTLSDaemon = class; // forward declaration

  // TTLSServerClientThread is to be used in a fashion similar to the
  // ScktComp.TServerClientThread class. Override ClientExecute to support
  // high level protocols such as HTTP (or manage the high level protocol in
  // the OnNewConnect event).
  TTLSServerClientThread = class(TThread)
  private
    FSock: TBlockTLSSynSock;
    FEvent: TSimpleEvent;
    FServSock: TSynaServerSocket;
    FKeepInCache: Boolean;
    FException: Exception;
    procedure DoHandleException;
    function GetServerSocket: TTCPBlockSocket;
    procedure SetKeepInCache(const Value: Boolean);
  protected
    function BeginConnect: Boolean;
    procedure ClientExecute; virtual;
    function EndConnect: Boolean;
    procedure Execute; override;
    procedure HandleException; virtual;
    procedure Reactivate(ASocket: TBlockTLSSynSock);
  public
    constructor Create(ASocket: TBlockTLSSynSock; AServSock: TSynaServerSocket);
    destructor Destroy; override;
    property ServerSocket: TTCPBlockSocket read GetServerSocket;
    property KeepInCache: Boolean read FKeepInCache write SetKeepInCache;
    property Sock: TBlockTLSSynSock read FSock;
  end;

  // TSynaServerSocket is to be used as a listenting socket and will manage
  // client sockets and client threads.
  // Override GetNewSocket and/or GetNewThread to create instances of other
  // classes than the default.
  TSynaServerSocket = class(TTCPBlockSocket)
  private
    FListLock: TCriticalSection;
    FActiveConnections: TList;
    FConnectionPool: TList;
    FActiveConnectionCount: Integer;
    FServer: TTLSSynaServer;
    FTLSServer: TCustomTLSInternalServer;
    FThreadCacheSize: Integer;
    FDaemon: TTLSDaemon;
    function GetActiveConnections(index: Integer): TBlockTLSSynSock;
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    procedure SetThreadCacheSize(const Value: Integer);
  protected
    procedure AddClient(AClient: TBlockTLSSynSock);
    procedure AddClientThread(AClientThread: TTLSServerClientThread);
    procedure ClientTerminate(Sender: TObject);
    function GetNewSocket(ASocket: TSocket): TBlockTLSSynSock; dynamic;
    function GetNewThread(AClient: TBlockTLSSynSock): TTLSServerClientThread; dynamic;
    procedure RemoveClient(AClient: TBlockTLSSynSock);
  public
    constructor Create(AServer: TTLSSynaServer);
    destructor Destroy; override;
    procedure CloseSocket; override;
    procedure Listen; override;
    property ActiveConnectionCount: Integer read FActiveConnectionCount;
    property Connections[index: Integer]: TBlockTLSSynSock read GetActiveConnections;
    property ThreadCacheSize: Integer read FThreadCacheSize write SetThreadCacheSize;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

  // TTLSDaemon is just a listener thread.
  TTLSDaemon = class(TThread)
  private
    FSock: TSynaServerSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TTLSSynaServer);
  end;

  // TTLSSynaServer is a server component for TCP/IP with SSL/TLS.
  // Override CreateListenerSocket to create a decendant of another class.
  TTLSSynaServer = class(TComponent)
  private
    FSock: TSynaServerSocket;
    FDaemon: TTLSDaemon;
    FOnNewConnect: TNewSynConnectEvent;
    FTLSServer: TCustomTLSInternalServer;
    FBindTo: string;
    FPort: Word;
    procedure SetOnNewConnect(const Value: TNewSynConnectEvent);
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    procedure SetBindTo(const Value: string);
    procedure SetPort(const Value: Word);
  protected
    procedure ClientConnect(AClient: TObject);
    procedure CreateListenerSocket; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; dynamic;
    procedure Stop; dynamic;
    property ServerSocket: TSynaServerSocket read FSock;
  published
    property BindTo: string read FBindTo write SetBindTo;
    property Port: Word read FPort write SetPort;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
    property OnNewConnect: TNewSynConnectEvent read FOnNewConnect write SetOnNewConnect;
  end;

var
  // The GlobalTLSInternalServer is used by the classes in HTTPSend etc:
  GlobalTLSInternalServer: TCustomTLSInternalServer absolute GlobalServer;

implementation
              
uses       
{$IFNDEF D6UP}
  Forms, Windows,
{$ENDIF}
  TlsConst;

type
  TTLSInternalServerHack = class(TCustomTLSInternalServer);

{ TSsTCPBlockSocket }

procedure TSsTCPBlockSocket.CloseSocket;
begin
  if Assigned(FTLSSocket) then begin
    FTLSSocket.Disconnect;
    FTLSSocket.Free;
    FTLSSocket := nil;
  end;
  inherited CloseSocket;
end;

procedure TSsTCPBlockSocket.Connect(IP, Port: string);
var
  RemIP: string;
  x: Integer;
begin
  if (IP <> '') and (Port <> '') then
    CloseSocket;
  if FSocket = INVALID_SOCKET then
    inherited Connect(IP, Port);
  if LastError = 0 then begin
    RemIP := GetRemoteSinIP;
    if (RemIP = '') or (RemIP = '0.0.0.0') then begin
      x := FLastError;
      CloseSocket;
      FLastError := x;
    end;
    if (FSocket <> INVALID_SOCKET) and Assigned(TLSServer) then begin
      FTLSSocket.Free;
      FTLSSocket := TTLSSynSockSlave.CreateSocket(FSocket);
      FTLSSocket.SessionID := FSessionID;
      FTLSSocket.CipherSuite := FCipherSuite;
      FTLSSocket.TLSServer := TLSServer;
      FTLSSocket.IPToCheck := RemIP;
      if (IP <> '') then
        FTLSSocket.DNSNameToCheck := ResolveIPToName(IP);
      FTLSSocket.URIToCheck := URIToCheck;
      FTLSSocket.Connect;
      if FTLSSocket.ErrorCode <> 0 then begin
        SetLength(FSessionID,0);
        SetLength(FCipherSuite,0);
        FLastError := WSASYSNOTREADY;
        FLastErrorDesc := TlsConst.AlertMsg(FTLSSocket.ErrorCode);
      end else begin
        FSessionID := FTLSSocket.SessionID;
        FCipherSuite := FTLSSocket.CipherSuite;
      end;
    end;
  end;
end;

constructor TSsTCPBlockSocket.Create;
begin
  inherited Create;
  Self.BlockingRead := True;
  TLSServer := GlobalTLSInternalServer;
end;

destructor TSsTCPBlockSocket.Destroy;
begin
  TLSServer := nil;
  inherited Destroy;
end;

function TSsTCPBlockSocket.RecvBuffer(Buffer: Pointer;
  Length: Integer): Integer;
begin
  if Assigned(FTLSSocket) then begin
    repeat
      Result := FTLSSocket.ReceiveLength;
    until (Result >= Length) or
          (FTLSSocket.ErrorCode <> 0) or
          (FTLSSocket.TLSServer = nil) or
          not FBlockingRead;
    if (FTLSSocket.ErrorCode <> 0) or (FTLSSocket.TLSServer = nil) then begin
      FLastError := WSASYSNOTREADY;
      FLastErrorDesc := TlsConst.AlertMsg(FTLSSocket.ErrorCode);
      Result := 0;
    end else begin
      if Result > Length then
        Result := Length;
      Result := FTLSSocket.ReceiveBuf(Buffer^,Result);
    end;
  end else
    Result := inherited RecvBuffer(Buffer,Length);
end;

function TSsTCPBlockSocket.RecvPacket(Timeout: Integer): string;
var
  x: integer;
begin
  Result := inherited RecvPacket(TimeOut);
  if LastError = WSAECONNRESET then begin
    FLastError := 0;
    //not drain CPU on large downloads...
    Sleep(0);
    x := WaitingData;
    if x > 0 then begin
      SetLength(Result, x);
      x := RecvBuffer(Pointer(Result), x);
      if x >= 0 then
        SetLength(Result, x);
    end else begin
      if CanRead(Timeout) then begin
        if Assigned(FTLSSocket) then
          x := FTLSSocket.FSlaveSocket.WaitingData
        else
          x := WaitingData;
        if x = 0 then
          FLastError := WSAECONNRESET;
        x := WaitingData;
        if x > 0 then begin
          SetLength(Result, x);
          x := RecvBuffer(Pointer(Result), x);
          if x >= 0 then
            SetLength(Result, x);
        end;
      end else
        FLastError := WSAETIMEDOUT;
    end;
  end;
  ExceptCheck;
end;

function TSsTCPBlockSocket.SendBuffer(Buffer: Pointer;
  Length: Integer): Integer;
begin
  if Assigned(FTLSSocket) then begin
    FTLSSocket.SendBuf(Buffer^,Length);
    Result := Length;
    if FTLSSocket.ErrorCode <> 0 then begin
      FLastError := WSASYSNOTREADY;        
      FLastErrorDesc := TlsConst.AlertMsg(FTLSSocket.ErrorCode);
    end;
  end else
    Result := inherited SendBuffer(Buffer,Length);
end;

procedure TSsTCPBlockSocket.SetBlockingRead(const Value: Boolean);
begin
  FBlockingRead := Value;
end;

procedure TSsTCPBlockSocket.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  if Value <> FTLSServer then begin
    FTLSServer := Value;
    if Assigned(FTLSSocket) then
      FTLSSocket.TLSServer := Value;
  end;
end;

procedure TSsTCPBlockSocket.SetURIToCheck(const Value: string);
begin
  FURIToCheck := Value;
end;

function TSsTCPBlockSocket.WaitingData: Integer;
begin
  if Assigned(FTLSSocket) then
    Result := FTLSSocket.ReceiveLength
  else
    Result := inherited WaitingData;
end;

{ TCustomTLSSynSock }

constructor TCustomTLSSynSock.Create(ASocket: TBlockSocket);
begin
  inherited Create;
  FSocket := ASocket;
end;

procedure TCustomTLSSynSock.DoRead;
begin
  // Implemented in descendant
end;

function TCustomTLSSynSock.GetRemoteAddress: string;
begin
  Result := FRemoteAddress;
  if Result = '' then begin
    Result := FSocket.GetRemoteSinIP;
    FRemoteAddress := Result;
  end;
end;

function TCustomTLSSynSock.GetRemoteHost: string;
begin
  Result := FRemoteHost;
  if Result = '' then begin
    Result := FSocket.ResolveIPToName(GetRemoteAddress);
    FRemoteHost := Result;
  end;
end;

function TCustomTLSSynSock.GetRemotePort: Integer;
begin
  Result := FRemotePort;
  if Result = 0 then begin
    Result := FSocket.GetRemoteSinPort;
    FRemotePort := Result;
  end;
end;

procedure TCustomTLSSynSock.RawClose;
begin
  FSocket.CloseSocket;
end;

procedure TCustomTLSSynSock.RawConnect;
var
  IPList: TStringList;
begin
  if not Connected then begin
    if FAddress = '' then begin
      IPList := TStringList.Create;
      try
        FSocket.ResolveNameToIP(FHost,IPList);
        if (IPList.Count > 0) and (IPList[0] <> '0.0.0.0') then
          FAddress := IPList[0];
      finally
        IPList.Free;
      end;
    end;
    if FAddress <> '' then begin
      if FPort = 0 then
        FPort := SynSock.ntohs(FSocket.ResolvePort(FService));
      FSocket.Connect(FAddress,IntToStr(FPort));
      InternalSetConnected(True);
    end;
  end;
end;

function TCustomTLSSynSock.RawReceive: TStream;
var
  TimeOut: Cardinal;
begin
  if not Encrypted then
    TimeOut := 20000
  else
    TimeOut := 0;
  if FSocket.Socket = INVALID_SOCKET then begin
    Result := nil;
    InternalSetConnected(False);
    InternalDisconnect;
  end else if (FSocket.WaitingData > 0) or FSocket.CanRead(TimeOut) then begin
    if FSocket.WaitingData = 0 then begin
      Result := nil;
      InternalSetConnected(False);
      InternalDisconnect;
    end else
      Result := TSynSocketStream.Create(FSocket,60000)
  end else begin
    if not Encrypted then
      InternalSetConnected(False);
    Result := nil;
  end;
end;

procedure TCustomTLSSynSock.RawSend(Strm: TCustomMemoryStream);
begin
  FSocket.SendBuffer(Strm.Memory,Strm.Size);
end;

procedure TCustomTLSSynSock.SetAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TCustomTLSSynSock.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TCustomTLSSynSock.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TCustomTLSSynSock.SetService(const Value: string);
begin
  FService := Value;
end;

{ TSynSocketStream }

constructor TSynSocketStream.Create(ASocket: TBlockSocket;
  TimeOut: Integer);
begin
  if ASocket.NonBlockMode then
    raise Exception.Create('Socket must be blocking');
  FSocket := ASocket;
  FTimeOut := TimeOut;
  FSocket.SetSendTimeout(TimeOut);
  FSocket.SetRecvTimeout(TimeOut);
  inherited Create;
end;

function TSynSocketStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FSocket.RecvBuffer(@Buffer,Count);
end;

function TSynSocketStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := 0;
end;

function TSynSocketStream.WaitForData(Timeout: Integer): Boolean;
begin
  Result := FSocket.CanRead(TimeOut);
end;

function TSynSocketStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FSocket.SendBuffer(@Buffer,Count);
end;

{ TTLSSynSockSlave }

procedure TTLSSynSockSlave.Connect;
begin
  InternalConnect;
  while Connected and not Encrypted do
    InternalRead;
end;

constructor TTLSSynSockSlave.CreateSocket(ASocket: TSocket);
begin
  FSlaveSocket := TBlockSocket.Create;
  FSlaveSocket.Socket := ASocket;
  InternalSetConnected(ASocket <> INVALID_SOCKET);
  Create(FSlaveSocket);
end;

destructor TTLSSynSockSlave.Destroy;
begin
  FSlaveSocket.Free;
  inherited;
end;

procedure TTLSSynSockSlave.Disconnect;
begin
  InternalDisconnect;
end;

procedure TTLSSynSockSlave.DoConnect;
begin
  GetRemoteHost;
  InternalSetConnected(FSocket.Socket <> INVALID_SOCKET);
  while Connected and not Encrypted do
    Receive;
end;

procedure TTLSSynSockSlave.Receive;
begin
  InternalRead;
end;

procedure TTLSSynSockSlave.Send;
begin
  InternalSend;
end;

{ TBlockTLSSynSock }

procedure TBlockTLSSynSock.DoConnect;
begin
  inherited;
  if Connected and Encrypted then
    if Assigned(FOnConnect) then
      FOnConnect(Self);
end;

procedure TBlockTLSSynSock.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
end;

{ TTLSServerClientThread }

function TTLSServerClientThread.BeginConnect: Boolean;
begin
  if FEvent.WaitFor(Cardinal(-1)) = wrSignaled then
    FEvent.ResetEvent;
  Result := not Terminated;
end;

procedure TTLSServerClientThread.ClientExecute;
begin
  try
    FSock.DoConnect;
    FSock.InternalDisconnect;
  except
    HandleException;
    FSock.InternalSetConnected(False);
  end;
end;

constructor TTLSServerClientThread.Create(ASocket: TBlockTLSSynSock;
  AServSock: TSynaServerSocket);
begin
  FEvent := TSimpleEvent.Create;
  if Assigned(ASocket) then
    FEvent.SetEvent;
  FSock := ASocket;
  FServSock := AServSock;
  OnTerminate := AServSock.ClientTerminate;
  AServSock.AddClientThread(Self);
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TTLSServerClientThread.Destroy;
begin
  FSock.Free;
  FEvent.Free;
  inherited;
end;

procedure TTLSServerClientThread.DoHandleException;
begin
  if FException is Exception then begin
  {$IFDEF D6UP}
    if Assigned(ApplicationShowException) then
      ApplicationShowException(FException);
  {$ELSE}
    Application.ShowException(FException);
  {$ENDIF}
  end else
    SysUtils.ShowException(FException, nil);
end;

function TTLSServerClientThread.EndConnect: Boolean;
begin
  FServSock.RemoveClient(FSock);
  FSock.Free;
  FSock := nil;
  Result := Terminated or not KeepInCache;
end;

procedure TTLSServerClientThread.Execute;
begin
  try
    while True do begin
      if BeginConnect then ClientExecute;
      if EndConnect then Break;
    end;
  except
    HandleException;
    KeepInCache := False;
  end;
end;

function TTLSServerClientThread.GetServerSocket: TTCPBlockSocket;
begin
  Result := FServSock;
end;

procedure TTLSServerClientThread.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    if not (FException is EAbort) then
      Synchronize(DoHandleException);
  finally
    FException := nil;
  end;
end;

procedure TTLSServerClientThread.Reactivate(ASocket: TBlockTLSSynSock);
begin
  FSock := ASocket;
  FEvent.SetEvent;
end;

procedure TTLSServerClientThread.SetKeepInCache(const Value: Boolean);
begin
  FKeepInCache := Value;
end;

{ TTLSDaemon }

constructor TTLSDaemon.Create(AServer: TTLSSynaServer);
begin
  FSock := AServer.FSock;
  AServer.FDaemon := Self;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TTLSDaemon.Execute;
var
  HSock: TSocket;
  Client: TBlockTLSSynSock;
begin
  repeat
    if FSock.CanRead(1000) then begin
      HSock := FSock.Accept;
      if FSock.LastError = 0 then begin
        Client := FSock.GetNewSocket(HSock);
        FSock.GetNewThread(Client);
      end;
    end;
  until Terminated;
end;

{ TTLSSynaServer }

procedure TTLSSynaServer.ClientConnect(AClient: TObject);
begin
  if Assigned(FOnNewConnect) then
    FOnNewConnect(Self,AClient as TCustomTLSSynSock);
end;

constructor TTLSSynaServer.Create(AOwner: TComponent);
begin
  inherited;
  CreateListenerSocket;
  TLSServer := GlobalTLSInternalServer;
end;

procedure TTLSSynaServer.CreateListenerSocket;
begin
  // Override this method to create an instance of another class:
  FSock := TSynaServerSocket.Create(Self);
end;

destructor TTLSSynaServer.Destroy;
begin
  FSock.Free;
  inherited;
end;

procedure TTLSSynaServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTLSServer then begin
      FSock.TLSServer := nil;
      FTLSServer := nil;
    end;
  end;
end;

procedure TTLSSynaServer.SetBindTo(const Value: string);
begin
  FBindTo := Value;
end;

procedure TTLSSynaServer.SetOnNewConnect(const Value: TNewSynConnectEvent);
begin
  FOnNewConnect := Value;
end;

procedure TTLSSynaServer.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TTLSSynaServer.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  FTLSServer := Value;
  FSock.TLSServer := Value;
end;

procedure TTLSSynaServer.Start;
begin
  FSock.Bind(FBindTo,IntToStr(FPort));
  FSock.Listen;
end;

procedure TTLSSynaServer.Stop;
begin
  FSock.CloseSocket;
end;

{ TSynaServerSocket }

procedure TSynaServerSocket.AddClient(AClient: TBlockTLSSynSock);
begin
  FListLock.Acquire;
  try
    FActiveConnections.Add(AClient);
  finally
    FListLock.Release;
  end;
end;

procedure TSynaServerSocket.AddClientThread(
  AClientThread: TTLSServerClientThread);
begin
  FListLock.Acquire;
  try
    AClientThread.KeepInCache := FConnectionPool.Count < ThreadCacheSize;
    FConnectionPool.Add(AClientThread);
  finally
    FListLock.Release;
  end;
end;

procedure TSynaServerSocket.ClientTerminate(Sender: TObject);
begin
  FListLock.Acquire;
  try
    FActiveConnections.Remove(TTLSServerClientThread(Sender).FSock);
    FConnectionPool.Remove(Sender);
  finally
    FListLock.Release;
  end;
end;

procedure TSynaServerSocket.CloseSocket;
var
  I:  Integer;
begin
  if Assigned(FDaemon) then begin
    FDaemon.FreeOnTerminate := False;
    FDaemon.Terminate;
    FDaemon.WaitFor;
    FDaemon := nil;
  end;
  FServer.FDaemon := nil;
  inherited;             
  FListLock.Acquire;
  try
    for I := 0 to FActiveConnections.Count - 1 do
      TBlockTLSSynSock(FActiveConnections[I]).Close;
  finally
    FListLock.Release;
  end;
end;

constructor TSynaServerSocket.Create(AServer: TTLSSynaServer);
begin
  inherited Create;
  FActiveConnections := TList.Create;
  FConnectionPool := TList.Create;
  FListLock := TCriticalSection.Create;
  FServer := AServer;
  FServer.FSock := Self;
end;

destructor TSynaServerSocket.Destroy;
begin
  inherited;
  FListLock.Free;
  FActiveConnections.Free;
  FConnectionPool.Free;
end;

function TSynaServerSocket.GetActiveConnections(
  index: Integer): TBlockTLSSynSock;
begin
  FListLock.Acquire;
  try
    Result := FActiveConnections[index];
  finally
    FListLock.Release;
  end;
end;

function TSynaServerSocket.GetNewSocket(
  ASocket: TSocket): TBlockTLSSynSock;
begin
  Result := TBlockTLSSynSock.CreateSocket(ASocket);
  Result.TLSServer := TLSServer;
  Result.OnConnect := FServer.ClientConnect;
  AddClient(Result);
end;

function TSynaServerSocket.GetNewThread(
  AClient: TBlockTLSSynSock): TTLSServerClientThread;
var
  I: Integer;
begin
  FListLock.Acquire;
  try
    Result := nil;
    for I := 0 to FConnectionPool.Count - 1 do begin
      Result := FConnectionPool[I];
      if Result.FSock = nil then begin
        Result.Reactivate(AClient);
        Break;
      end;
      Result := nil;
    end;
  finally
    FListLock.Release;
  end;
  if Result = nil then
    Result := TTLSServerClientThread.Create(AClient,Self);
end;

procedure TSynaServerSocket.Listen;
begin
  inherited;
  FDaemon := TTLSDaemon.Create(FServer);
end;

procedure TSynaServerSocket.RemoveClient(AClient: TBlockTLSSynSock);
begin
  FListLock.Acquire;
  try
    FActiveConnections.Remove(AClient);
  finally
    FListLock.Release;
  end;
end;

procedure TSynaServerSocket.SetThreadCacheSize(const Value: Integer);
var
  Start, I: Integer;
begin
  if Value <> FThreadCacheSize then begin
    if Value < FThreadCacheSize then
      Start := Value
    else
      Start := FThreadCacheSize;
    FThreadCacheSize := Value;
    FListLock.Acquire;
    try
      for I := 0 to FConnectionPool.Count - 1 do
        TTLSServerClientThread(FConnectionPool[I]).KeepInCache := I < Start;
    finally
      FListLock.Release;
    end;
  end;
end;

procedure TSynaServerSocket.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  FTLSServer := Value;
end;
                     
{$ELSE  SHA1_AND_MD5}
implementation
{$ENDIF SHA1_AND_MD5}
end.
