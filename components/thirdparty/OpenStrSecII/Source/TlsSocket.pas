{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSSocket Unit                                    }
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
unit TlsSocket;

interface
      
{$IFDEF SHA1_AND_MD5}
uses
  SysUtils, Classes, SyncObjs, WinSock, ScktComp, TlsInternalServer;

type
  TCustomTLSSocket = class(TAbstractTLSSocket)
  private
    FSocket: TCustomWinSocket;
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
    constructor Create(ASocket: TCustomWinSocket); virtual;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
    property RemoteHost: string read GetRemoteHost;
    property RemoteAddress: string read GetRemoteAddress;
    property RemotePort: Integer read GetRemotePort;
  end;        

  TNewConnectEvent = procedure (Sender: TObject; Socket: TCustomTLSSocket) of object;

  TBlockingTLSSocket = class(TCustomTLSSocket)
  private
    FOnConnect: TNotifyEvent;
    procedure SetOnConnect(const Value: TNotifyEvent);
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Receive; override;
    procedure Send; override;
    procedure DoConnect;
  public
    property OnConnect: TNotifyEvent read FOnConnect write SetOnConnect;
  end;

  TTLSServerClientThread = class(TServerClientThread)
  protected
    procedure ClientExecute; override;
  end;

  TTLSServerClientWinSocket = class(TServerClientWinSocket)
  private
    FTLSSocket: TBlockingTLSSocket;
    procedure SetTLSSocket(const Value: TBlockingTLSSocket);
  public
    destructor Destroy; override;
    property TLSSocket: TBlockingTLSSocket read FTLSSocket write SetTLSSocket;
  end;

  TCustomTLSServerSocket = class(TComponent)
  private
    FTLSServer: TCustomTLSInternalServer;
    FServerSocket: TServerSocket;
    FOnNewConnect: TNewConnectEvent;
    FActive: Boolean;
    FPort: Integer;
    FService: string;
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    procedure SetOnNewConnect(const Value: TNewConnectEvent);
    function GetOnAccept: TSocketNotifyEvent;
    function GetOnListen: TSocketNotifyEvent;
    procedure SetOnAccept(const Value: TSocketNotifyEvent);
    procedure SetOnListen(const Value: TSocketNotifyEvent);
    function GetActive: Boolean;
    function GetPort: Integer;
    function GetService: string;
    procedure SetActive(const Value: Boolean);
    procedure SetPort(const Value: Integer);
    procedure SetService(const Value: string);
    function GetTLSSocketCount: Integer;
    function GetTLSSockets(index: Integer): TCustomTLSSocket;
  protected
    property Active: Boolean read GetActive write SetActive;
    property Port: Integer read GetPort write SetPort;
    property Service: string read GetService write SetService;
    property OnNewConnect: TNewConnectEvent read FOnNewConnect write SetOnNewConnect;
    procedure DoGetSocket(Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket);
    procedure DoGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);
    procedure DoNewConnect(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TLSSocketCount: Integer read GetTLSSocketCount;
    property TLSSockets[index: Integer]: TCustomTLSSocket read GetTLSSockets;
  published
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
    property OnListen: TSocketNotifyEvent read GetOnListen write SetOnListen;
    property OnAccept: TSocketNotifyEvent read GetOnAccept write SetOnAccept;
  end;

  TTLSServerSocket = class(TCustomTLSServerSocket)
  published
    property Active;
    property Port;
    property Service;
    property OnNewConnect;
  end;

  TCustomTLSClientSocket = class(TComponent)
  private
    FTLSSocket: TCustomTLSSocket;
    FTLSServer: TCustomTLSInternalServer;
    FPort: Integer;
    FHost: string;
    FAddress: string;
    FService: string;
    FOnRead: TNotifyEvent;
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetAddress(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetService(const Value: string);
    procedure SetOnRead(const Value: TNotifyEvent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Active: Boolean read GetActive write SetActive;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
    property OnRead: TNotifyEvent read FOnRead write SetOnRead;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TLSSocket: TCustomTLSSocket read FTLSSocket;
  published
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

  TTLSClientSocket = class(TCustomTLSClientSocket)
  public
    property Active;
  published
    property Address;
    property Host;
    property Port;
    property Service;
    property OnRead;
  end;

implementation

uses
  Windows, Messages;

{ TCustomTLSSocket }

constructor TCustomTLSSocket.Create(ASocket: TCustomWinSocket);
begin       
  inherited Create;
  FSocket := ASocket;
end;

procedure TCustomTLSSocket.DoRead;
begin
  // Implemented in descendant
end;

function TCustomTLSSocket.GetRemoteAddress: string;
begin
  Result := FRemoteAddress;
  if Result = '' then begin
    Result := FSocket.RemoteAddress;
    FRemoteAddress := Result;
  end;
end;

function TCustomTLSSocket.GetRemoteHost: string;
begin
  Result := FRemoteHost;
  if Result = '' then begin
    Result := FSocket.RemoteHost;
    FRemoteHost := Result;
  end;
end;

function TCustomTLSSocket.GetRemotePort: Integer;
begin
  Result := FRemotePort;
  if Result = 0 then begin
    Result := FSocket.RemotePort;
    FRemotePort := Result;
  end;
end;

procedure TCustomTLSSocket.RawClose;
begin
  FSocket.Close;
end;

procedure TCustomTLSSocket.RawConnect;
begin
  FSocket.Open(FHost, FAddress, FService, FPort{$IFDEF D5UP}, True {$ENDIF});
  DoSleep(1);
  InternalSetConnected(True);
end;

function TCustomTLSSocket.RawReceive: TStream;
begin
  if not FSocket.Connected then begin
    Result := nil;
    InternalSetConnected(False);
    InternalDisconnect;
  end else begin
    Result := TWinSocketStream.Create(FSocket,60000);
    if not TWinSocketStream(Result).WaitForData(60000) then begin
      Result.Free;
      Result := nil;
    end;
    if not FSocket.Connected then begin
      InternalSetConnected(False);
      InternalDisconnect;
    end;
  end;
end;

procedure TCustomTLSSocket.RawSend(Strm: TCustomMemoryStream);
var
  SockStrm: TWinSocketStream;
begin
  if not Connected then begin
    InternalSetConnected(False);
    InternalDisconnect;
  end else if Assigned(FSocket) then begin
    SockStrm := TWinSocketStream.Create(FSocket,60000);
    try
      SockStrm.Write(Strm.Memory^,Strm.Size);
    finally
      SockStrm.Free;
    end;
  end;
end;

procedure TCustomTLSSocket.SetAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TCustomTLSSocket.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TCustomTLSSocket.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TCustomTLSSocket.SetService(const Value: string);
begin
  FService := Value;
end;

{ TBlockingTLSSocket }

procedure TBlockingTLSSocket.Connect;
begin
  InternalConnect;
  while Connected and not Encrypted do
    InternalRead;
end;

procedure TBlockingTLSSocket.Disconnect;
begin
  InternalDisconnect;
end;

procedure TBlockingTLSSocket.DoConnect;
begin
  GetRemoteHost;
  InternalSetConnected(FSocket.Connected);
  while Connected and not Encrypted do
    Receive;
  if Connected and Encrypted then
    if Assigned(FOnConnect) then
      FOnConnect(Self);
end;

procedure TBlockingTLSSocket.Receive;
begin
  InternalRead;
end;

procedure TBlockingTLSSocket.Send;
begin
  InternalSend;
end;

procedure TBlockingTLSSocket.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
end;

{ TTLSServerClientThread }

procedure TTLSServerClientThread.ClientExecute;
begin
  try
    TTLSServerClientWinSocket(ClientSocket).TLSSocket.DoConnect;
    TTLSServerClientWinSocket(ClientSocket).TLSSocket.InternalDisconnect;
  except
    HandleException;
    TTLSServerClientWinSocket(ClientSocket).TLSSocket.InternalSetConnected(False);
  end;
end;

{ TTLSServerClientWinSocket }

destructor TTLSServerClientWinSocket.Destroy;
begin
  FTLSSocket.Free;
  inherited;
end;

procedure TTLSServerClientWinSocket.SetTLSSocket(
  const Value: TBlockingTLSSocket);
begin
  FTLSSocket := Value;
end;

{ TCustomTLSServerSocket }

constructor TCustomTLSServerSocket.Create(AOwner: TComponent);
begin
  inherited;
  FServerSocket := TServerSocket.Create(Self);
  FServerSocket.OnGetSocket := DoGetSocket;
  FServerSocket.OnGetThread := DoGetThread;
  FServerSocket.ServerType := stThreadBlocking;
end;

destructor TCustomTLSServerSocket.Destroy;
begin
  FServerSocket.Free;
  FServerSocket := nil;
  inherited;
end;

procedure TCustomTLSServerSocket.DoGetSocket(Sender: TObject; Socket: TSocket;
  var ClientSocket: TServerClientWinSocket);
begin
  ClientSocket := TTLSServerClientWinSocket.Create(Socket,Sender as TServerWinSocket);
  TTLSServerClientWinSocket(ClientSocket).TLSSocket :=
    TBlockingTLSSocket.Create(ClientSocket);
  TTLSServerClientWinSocket(ClientSocket).TLSSocket.TLSServer := FTLSServer;
  TTLSServerClientWinSocket(ClientSocket).TLSSocket.OnConnect := DoNewConnect;
end;

procedure TCustomTLSServerSocket.DoGetThread(Sender: TObject;
  ClientSocket: TServerClientWinSocket;
  var SocketThread: TServerClientThread);
begin
  SocketThread := TTLSServerClientThread.Create(False,ClientSocket);
end;

procedure TCustomTLSServerSocket.DoNewConnect(Sender: TObject);
begin
  if Assigned(FOnNewConnect) then
    FOnNewConnect(Self,Sender as TCustomTLSSocket);
end;

function TCustomTLSServerSocket.GetActive: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FActive
  else
    Result := Assigned(FServerSocket) and FServerSocket.Active;
end;

function TCustomTLSServerSocket.GetOnAccept: TSocketNotifyEvent;
begin
  Result := FServerSocket.OnAccept;
end;

function TCustomTLSServerSocket.GetOnListen: TSocketNotifyEvent;
begin
  Result := FServerSocket.OnListen;
end;

function TCustomTLSServerSocket.GetPort: Integer;
begin                                  
  if csDesigning in ComponentState then
    Result := FPort
  else
    Result := FServerSocket.Port;
end;

function TCustomTLSServerSocket.GetService: string;
begin
  if csDesigning in ComponentState then
    Result := FService
  else
    Result := FServerSocket.Service;
end;

function TCustomTLSServerSocket.GetTLSSocketCount: Integer;
begin
  Result := FServerSocket.Socket.ActiveConnections;
end;

function TCustomTLSServerSocket.GetTLSSockets(
  index: Integer): TCustomTLSSocket;
begin
  Result := TTLSServerClientWinSocket(FServerSocket.Socket.Connections[index]).TLSSocket;
end;

procedure TCustomTLSServerSocket.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTLSServer then
      TLSServer := nil;
  end;
end;

procedure TCustomTLSServerSocket.SetActive(const Value: Boolean);
begin
  if csDesigning in ComponentState then
    FActive := Value
  else if Assigned(FServerSocket) then
    FServerSocket.Active := Value and Assigned(FTLSServer);
end;

procedure TCustomTLSServerSocket.SetOnAccept(const Value: TSocketNotifyEvent);
begin
  FServerSocket.OnAccept := Value;
end;

procedure TCustomTLSServerSocket.SetOnListen(const Value: TSocketNotifyEvent);
begin
  FServerSocket.OnListen := Value;
end;

procedure TCustomTLSServerSocket.SetOnNewConnect(const Value: TNewConnectEvent);
begin
  FOnNewConnect := Value;
end;

procedure TCustomTLSServerSocket.SetPort(const Value: Integer);
begin
  if csDesigning in ComponentState then
    FPort := Value
  else
    FServerSocket.Port := Value;
end;

procedure TCustomTLSServerSocket.SetService(const Value: string);
begin
  if csDesigning in ComponentState then
    FService := Value
  else
    FServerSocket.Service := Value;
end;

procedure TCustomTLSServerSocket.SetTLSServer(const Value: TCustomTLSInternalServer);
begin
  FTLSServer := Value;
  if not (csDestroying in ComponentState) then begin
    if Value = nil then
      Active := False
    else begin
      FreeNotification(Value);
      Value.FreeNotification(Self);
    end;
  end;
end;

type
  TReadThread = class(TThread)
  protected
    FTLSSocket: TBlockingTLSSocket;
    procedure Execute; override;
  public
    constructor Create(ATLSSocket: TBlockingTLSSocket);
  end;

  TBlockingTLSClientSocket = class(TBlockingTLSSocket)
  private
    FInternalLock: TCriticalSection;
    FBlocking: Boolean;
    FReadThread: TReadThread;
    FOnRead: TNotifyEvent;
    procedure SetOnRead(const Value: TNotifyEvent);
  protected
    procedure DoRead; override;
    procedure InternalLock; override;
    procedure InternalUnlock; override;
    procedure Connect; override;
    procedure Disconnect; override;
  public
    constructor Create(ASocket: TCustomWinSocket); override;
    destructor Destroy; override;
    property OnRead: TNotifyEvent read FOnRead write SetOnRead;
  end;

{ TCustomTLSClientSocket }

constructor TCustomTLSClientSocket.Create(AOwner: TComponent);
begin
  inherited;
  SetActive(False);
end;

destructor TCustomTLSClientSocket.Destroy;
begin
  SetActive(False);
  FTLSSocket.Free;
  inherited;
end;

function TCustomTLSClientSocket.GetActive: Boolean;
begin
  Result := FTLSSocket.Connected;
end;

procedure TCustomTLSClientSocket.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTLSServer then
      TLSServer := nil;
  end;
end;

procedure TCustomTLSClientSocket.SetActive(const Value: Boolean);
var
  Socket: TCustomWinSocket;
begin
  if (FTLSSocket = nil) or
     (Value <> FTLSSocket.Connected) then begin
    if Value and Assigned(FTLSSocket) then begin
      FTLSSocket.Port := Port;
      FTLSSocket.Address := Address;
      FTLSSocket.Host := Host;
      FTLSSocket.Service := Service;
      FTLSSocket.TLSServer := TLSServer;
      if FTLSSocket is TBlockingTLSClientSocket then
        TBlockingTLSClientSocket(FTLSSocket).FBlocking := Assigned(FOnRead);
      FTLSSocket.Connect;
    end else begin
      if Assigned(FTLSSocket) then begin
        Socket := FTLSSocket.FSocket;
        FTLSSocket.FSocket := nil;
        Socket.Free;
        FTLSSocket.FSocket := TCustomWinSocket.Create(INVALID_SOCKET);
      end else begin
        FTLSSocket := TBlockingTLSSocket.Create(TCustomWinSocket.Create(INVALID_SOCKET));
      end;
      FTLSSocket.FSocket.ASyncStyles := [];
    end;
  end;
end;

procedure TCustomTLSClientSocket.SetAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TCustomTLSClientSocket.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TCustomTLSClientSocket.SetOnRead(const Value: TNotifyEvent);
begin
  FOnRead := Value;
end;

procedure TCustomTLSClientSocket.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TCustomTLSClientSocket.SetService(const Value: string);
begin
  FService := Value;
end;

procedure TCustomTLSClientSocket.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  FTLSServer := Value;
  if Assigned(Value) then Value.FreeNotification(Self);
end;

{ TBlockingTLSClientSocket }

procedure TBlockingTLSClientSocket.Connect;
begin
  FRemoteHost := '';
  FRemoteAddress := '';
  FRemotePort := 0;
  inherited;
  if FBlocking then FReadThread := TReadThread.Create(Self);
end;

constructor TBlockingTLSClientSocket.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(ASocket);
  FInternalLock := TCriticalSection.Create;
  FBlocking := True;
end;

destructor TBlockingTLSClientSocket.Destroy;
begin
  FInternalLock.Free;
  FSocket.Free;
  inherited;
end;

procedure TBlockingTLSClientSocket.Disconnect;
begin
  inherited;
  FReadThread := nil;
end;

procedure TBlockingTLSClientSocket.DoRead;
begin
  if Assigned(FOnRead) then FOnRead(Self);
end;

procedure TBlockingTLSClientSocket.InternalLock;
begin
  FInternalLock.Acquire;
end;

procedure TBlockingTLSClientSocket.InternalUnlock;
begin
  FInternalLock.Release;
end;

procedure TBlockingTLSClientSocket.SetOnRead(const Value: TNotifyEvent);
begin
  FOnRead := Value;
end;

{ TReadThread }

constructor TReadThread.Create(ATLSSocket: TBlockingTLSSocket);
begin
  inherited Create(True);
  FTLSSocket := ATLSSocket;
  FreeOnTerminate := True;
  Resume;
end;

procedure TReadThread.Execute;
begin
  try
    while FTLSSocket.Connected do begin
      if FTLSSocket.ReceiveLength > 0 then
        Synchronize(FTLSSocket.DoRead);
    end;
  except
  end;
end;

{$ELSE}
implementation
{$ENDIF}
end.
