{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsTLSIdIntercept Unit                             }
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
unit SsTLSIdIntercept;

interface

uses
  SysUtils, Classes, TLSInternalServer, IdStackConsts, IdGlobal, IdStack,
  IdThread, IdSocketHandle, IdBaseComponent, IdComponent, IdTCPServer,
  IdIntercept, IdSSLIntercept;

type
  // TLS wrapper classes:
  TCustomTLSIdSock = class(TAbstractTLSSocket)
  private
    FSocket: TIdSocketHandle;
    FPort: Integer;
    FHost: string;
    FService: string;
    FAddress: string;
    FRemoteHost: string;
    FRemoteAddress: string;
    FRemotePort: Integer;
    FBindMaxPort: Integer;
    FBindMinPort: Integer;
    FBindPort: Integer;
    FBindAddress: string;
    FTimeOut: Cardinal;
    procedure SetAddress(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetService(const Value: string);
    function GetRemoteHost: string;
    function GetRemoteAddress: string;
    function GetRemotePort: Integer;
    procedure SetBindAddress(const Value: string);
    procedure SetBindMaxPort(const Value: Integer);
    procedure SetBindMinPort(const Value: Integer);
    procedure SetBindPort(const Value: Integer);
    procedure SetTimeOut(const Value: Cardinal);
  protected
    procedure DoRead; virtual;
    procedure RawClose; override;
    procedure RawConnect; override;
    function RawReceive: TStream; override;
    procedure RawSend(Strm: TCustomMemoryStream); override;
  public
    constructor Create(ASocket: TIdSocketHandle); virtual;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
    property BindAddress: string read FBindAddress write SetBindAddress;
    property BindPort: Integer read FBindPort write SetBindPort;
    property BindMinPort: Integer read FBindMinPort write SetBindMinPort;
    property BindMaxPort: Integer read FBindMaxPort write SetBindMaxPort;
    property RemoteHost: string read GetRemoteHost;
    property RemoteAddress: string read GetRemoteAddress;
    property RemotePort: Integer read GetRemotePort;
    property TimeOut: Cardinal read FTimeOut write SetTimeOut;
  end;

  TNewSynConnectEvent = procedure (Sender: TObject; Socket: TCustomTLSIdSock) of object;

  TTLSIdSockSlave = class(TCustomTLSIdSock)
  private
    FSlaveSocket: TIdSocketHandle;
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Receive; override;
    procedure Send; override;
    procedure DoConnect; virtual;
  public
    constructor CreateSocket(ASocket: TIdStackSocketHandle);
    destructor Destroy; override;
  end;

  TBlockTLSIdSock = class(TTLSIdSockSlave)
  private
    FOnConnect: TNotifyEvent;
    procedure SetOnConnect(const Value: TNotifyEvent);
  protected
    procedure DoConnect; override;
  public
    property OnConnect: TNotifyEvent read FOnConnect write SetOnConnect;
  end;

  // TIdSocketStream is used internally as a substitute for
  // ScktComp.TWinSocketStream
  TIdSocketStream = class(TStream)
  private
    FSocket: TIdSocketHandle;
    FTimeout: Longint;
    FReadable: Boolean;
  public
    constructor Create(ASocket: TIdSocketHandle; TimeOut: Longint);
    function WaitForData(Timeout: Longint): Boolean;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property TimeOut: Longint read FTimeout write FTimeout;
  end;

  TSsTLSIdConnectionIntercept = class(TIdSSLConnectionIntercept)
  private
    FTLSServer: TCustomTLSInternalServer;  
    FTLSSocket: TTLSIdSockSlave;
    function GetPassThrough: Boolean;
    procedure SetPassThrough(const Value: Boolean);
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
                                       
    procedure Connect(ABinding: TIdSocketHandle); override;
    procedure Disconnect; override;

    function Recv(var ABuf; ALen: integer): integer; override;
    function Send(var ABuf; ALen: integer): integer; override;
    
    property PassThrough: Boolean read GetPassThrough write SetPassThrough;
  published
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

  TSsTLSIdServerIntercept = class(TIdSSLServerIntercept)
  private
    FTLSServer: TCustomTLSInternalServer;
    FIsInitialized: Boolean;
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Init; override;
    function Accept(ABinding: TIdSocketHandle): TIdConnectionIntercept;  override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

implementation

{ TSsTLSIdConnectionIntercept }

procedure TSsTLSIdConnectionIntercept.Connect(ABinding: TIdSocketHandle);
begin
  if Assigned(TLSServer) and
     not (Assigned(FTLSSocket) and FTLSSocket.Connected) then begin
    FTLSSocket.Free;
    FTLSSocket := TTLSIdSockSlave.CreateSocket(ABinding.Handle);
    FTLSSocket.TLSServer := TLSServer;
    FTLSSocket.Address := ABinding.PeerIP;
    FTLSSocket.Port := ABinding.PeerPort;
    FTLSSocket.Connect;
  end;
end;

constructor TSsTLSIdConnectionIntercept.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSsTLSIdConnectionIntercept.Destroy;
begin
  TLSServer := nil;
  inherited;
end;

procedure TSsTLSIdConnectionIntercept.Disconnect;
begin
  if Assigned(FTLSSocket) then begin
    FTLSSocket.Disconnect;
    FTLSSocket.Free;
    FTLSSocket := nil;
  end;
end;

function TSsTLSIdConnectionIntercept.GetPassThrough: Boolean;
begin
  Result := not Assigned(FTLSServer);
end;

function TSsTLSIdConnectionIntercept.Recv(var ABuf; ALen: integer): integer;
var
  DLen: Integer;
begin
  if PassThrough then
    Result := inherited Recv(ABuf,ALen)
  else begin
    Result := 0;
    DLen := 0;
    while FTLSSocket.Connected and (DLen = 0) do begin
      DLen := FTLSSocket.ReceiveLength;
      if DLen > ALen then DLen := ALen;
      if DLen > 0 then
        Result := FTLSSocket.ReceiveBuf(ABuf,DLen);
    end;
  end;
end;

function TSsTLSIdConnectionIntercept.Send(var ABuf; ALen: integer): integer;
begin
  if PassThrough then
    Result := inherited Send(ABuf,ALen)
  else begin
    Result := ALen;
    FTLSSocket.SendBuf(ABuf,ALen);
  end;
end;

procedure TSsTLSIdConnectionIntercept.SetPassThrough(const Value: Boolean);
begin
  if Value then
    TLSServer := nil;
end;

procedure TSsTLSIdConnectionIntercept.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  FTLSServer := Value;
  if Value = nil then
    Disconnect;
end;

{ TCustomTLSIdSock }

constructor TCustomTLSIdSock.Create(ASocket: TIdSocketHandle);
begin
  inherited Create;
  FSocket := ASocket;
  FBindAddress := '0.0.0.0';
  FBindMaxPort := 65535;
  FTimeOut := 20000;
end;

procedure TCustomTLSIdSock.DoRead;
begin
  // Implemented in descendant
end;

function TCustomTLSIdSock.GetRemoteAddress: string;
begin
  Result := FRemoteAddress;
  if Result = '' then begin
    Result := FSocket.PeerIP;
    FRemoteAddress := Result;
  end;
end;

function TCustomTLSIdSock.GetRemoteHost: string;
begin
  Result := FRemoteHost;
  if Result = '' then begin
    Result := FSocket.PeerIP;
    FRemoteHost := Result;
  end;
end;

function TCustomTLSIdSock.GetRemotePort: Integer;
begin
  Result := FRemotePort;
  if Result = 0 then begin
    Result := FSocket.PeerPort;
    FRemotePort := Result;
  end;
end;

procedure TCustomTLSIdSock.RawClose;
begin
  FSocket.CloseSocket;
end;

procedure TCustomTLSIdSock.RawConnect;
begin
  if not FSocket.HandleAllocated then begin
    if FHost = '' then
      FHost := FAddress;
    if FPort = 0 then
      FPort := GStack.WSGetServByName(FService);
    FSocket.IP := FBindAddress;
    FSocket.Port := FBindPort;
    FSocket.Bind;
    if GStack.IsIP(FHost) then
      FAddress := FHost
    else
      FAddress := GStack.ResolveHost(FHost);
    FSocket.SetPeer(FAddress,FPort);
    GStack.CheckForSocketError(FSocket.Connect);
  end;
  InternalSetConnected(FSocket.HandleAllocated);
end;

function TCustomTLSIdSock.RawReceive: TStream;
begin
  Result := TIdSocketStream.Create(FSocket,FTimeOut);
end;

procedure TCustomTLSIdSock.RawSend(Strm: TCustomMemoryStream);
begin
  FSocket.Send(Strm.Memory^,Strm.Size,0);
end;

procedure TCustomTLSIdSock.SetAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TCustomTLSIdSock.SetBindAddress(const Value: string);
begin
  FBindAddress := Value;
end;

procedure TCustomTLSIdSock.SetBindMaxPort(const Value: Integer);
begin
  FBindMaxPort := Value;
end;

procedure TCustomTLSIdSock.SetBindMinPort(const Value: Integer);
begin
  FBindMinPort := Value;
end;

procedure TCustomTLSIdSock.SetBindPort(const Value: Integer);
begin
  FBindPort := Value;
end;

procedure TCustomTLSIdSock.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TCustomTLSIdSock.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TCustomTLSIdSock.SetService(const Value: string);
begin
  FService := Value;
end;

procedure TCustomTLSIdSock.SetTimeOut(const Value: Cardinal);
begin
  FTimeOut := Value;
end;

{ TTLSIdSockSlave }

procedure TTLSIdSockSlave.Connect;
begin
  InternalConnect;
  while Connected and not Encrypted do
    InternalRead;
end;

type
  TIdSocketHandleHack = class(TIdSocketHandle);

constructor TTLSIdSockSlave.CreateSocket(ASocket: TIdStackSocketHandle);
begin
  FSlaveSocket := TIdSocketHandle.Create(nil);
  with TIdSocketHandleHack(FSlaveSocket) do begin
    FHandle := ASocket;
    FHandleAllocated := ASocket <> Id_INVALID_SOCKET;
  end;
  InternalSetConnected(ASocket <> Id_INVALID_SOCKET);
  Create(FSlaveSocket);
end;

destructor TTLSIdSockSlave.Destroy;
begin
  FSlaveSocket.Free;
  inherited;
end;

procedure TTLSIdSockSlave.Disconnect;
begin
  InternalDisconnect;
end;

procedure TTLSIdSockSlave.DoConnect;
begin
  GetRemoteHost;
  InternalSetConnected(FSocket.Handle <> Id_INVALID_SOCKET);
  while Connected and not Encrypted do
    Receive;
end;

procedure TTLSIdSockSlave.Receive;
begin
  InternalRead;
end;

procedure TTLSIdSockSlave.Send;
begin
  InternalSend;
end;

{ TBlockTLSIdSock }

procedure TBlockTLSIdSock.DoConnect;
begin
  inherited;
  if Connected and Encrypted then
    if Assigned(FOnConnect) then
      FOnConnect(Self);
end;

procedure TBlockTLSIdSock.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
end;

{ TIdSocketStream }

constructor TIdSocketStream.Create(ASocket: TIdSocketHandle;
  TimeOut: Integer);
begin
  FSocket := ASocket;
  FTimeOut := TimeOut;
end;

function TIdSocketStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Count > 0 then begin
    if WaitForData(FTimeOut) then
      Result := FSocket.Recv(Buffer,Count,0)
    else
      Result := 0;
    FReadable := False;
  end else
    Result := 0;
end;

function TIdSocketStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := 0;
end;

function TIdSocketStream.WaitForData(Timeout: Integer): Boolean;
begin
  Result := FReadable;
  if not Result then begin
    Result := FSocket.Readable(Timeout);
    FReadable := Result;
  end;
end;

function TIdSocketStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FSocket.Send(Pointer(@Buffer)^,Count,0);
end;

{ TSsTLSIdServerIntercept }

function TSsTLSIdServerIntercept.Accept(ABinding: TIdSocketHandle): TIdConnectionIntercept;
var
  tmpIdCISsTLS: TSsTLSIdConnectionIntercept;
begin
  if not FIsInitialized then begin
    Init;
  end;

  tmpIdCISsTLS := TSsTLSIdConnectionIntercept.Create(Self);
  tmpIdCISsTLS.TLSServer := FTLSServer;
  tmpIdCISsTLS.FTLSSocket.Free;
  tmpIdCISsTLS.FTLSSocket := TTLSIdSockSlave.CreateSocket(ABinding.Handle);
  tmpIdCISsTLS.FTLSSocket.TLSServer := TLSServer;
  tmpIdCISsTLS.FTLSSocket.Address := ABinding.PeerIP;
  tmpIdCISsTLS.FTLSSocket.Port := ABinding.PeerPort;
  tmpIdCISsTLS.FTLSSocket.DoConnect;
  Result := tmpIdCISsTLS;
end;

constructor TSsTLSIdServerIntercept.Create(AOwner: TComponent);
begin
  inherited;
  FIsInitialized := False;
end;

destructor TSsTLSIdServerIntercept.Destroy;
begin

  inherited;
end;

procedure TSsTLSIdServerIntercept.Init;
begin
  if FTLSServer = nil then
    raise Exception.Create('TSsTLSIdServerIOHandler.Init: No TLSServer assigned');
  if FTLSServer.PublicKeyAlgorithms = [] then
    raise Exception.Create('TSsTLSIdServerIOHandler.Init: No Server Certificate');
  FTLSServer.TLSSetupServer;
  FIsInitialized := True;
end;

procedure TSsTLSIdServerIntercept.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTLSServer then
      TLSServer := nil;
  end;
end;

procedure TSsTLSIdServerIntercept.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  if Value <> FTLSServer then begin
    FTLSServer := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self)
    else
      FIsInitialized := False;
  end;
end;

end.
