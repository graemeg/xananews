{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsTLSIdSocket Unit                                }
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
{$Q-,R-}
{$I ver.inc}
unit SsTLSIdSocket;

interface

uses
  SysUtils, Classes, SyncObjs, TLSInternalServer,
  IdStackConsts, IdStack, IdSocketHandle;

const
  SsTLSIdDefaultTimeOut = 10000;

type
  // TLS wrapper classes:
  TCustomTLSIdSock = class(TAbstractTLSSocket)
  private
    FSocket: TIdSocketHandle;
    FInternalLock: TCriticalSection;
    FCloseLock: TCriticalSection;
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
    FThreadLocked: Boolean;
    FFewClientsMuchData: Boolean;
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
    procedure SetThreadLocked(const Value: Boolean);
    procedure SetFewClientsMuchData(const Value: Boolean);
  protected
    function CheckRecvResult(ASize: Integer): Boolean; virtual;
    procedure DoRead; virtual;
    procedure InternalLock; override;
    procedure InternalUnlock; override;
    procedure RawClose; override;
    procedure RawConnect; override;
    function RawReceive: TStream; override;
    procedure RawSend(Strm: TCustomMemoryStream); override;
  public
    constructor Create(ASocket: TIdSocketHandle); virtual;
    destructor Destroy; override;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
    property BindAddress: string read FBindAddress write SetBindAddress;
    property BindPort: Integer read FBindPort write SetBindPort;
    property BindMinPort: Integer read FBindMinPort write SetBindMinPort;
    property BindMaxPort: Integer read FBindMaxPort write SetBindMaxPort;
    property FewClientsMuchData: Boolean read FFewClientsMuchData write SetFewClientsMuchData;
    property RemoteHost: string read GetRemoteHost;
    property RemoteAddress: string read GetRemoteAddress;
    property RemotePort: Integer read GetRemotePort;
    // Set ThreadLocked := True when using the instance in a Free Thread Model.
    // Most Indy9 server components use am Apartment Thread Model, so False
    // is the default.
    property ThreadLocked: Boolean read FThreadLocked write SetThreadLocked;
    property TimeOut: Cardinal read FTimeOut write SetTimeOut;
  end;

  TNewSynConnectEvent = procedure (Sender: TObject; Socket: TCustomTLSIdSock) of object;

  TTLSIdSockSlave = class(TCustomTLSIdSock)
  protected
    FSlaveSocket: TIdSocketHandle;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Receive; override;
    procedure Send; override;
    procedure DoConnect; virtual;
  public
    constructor CreateSocket(ASocket: TIdStackSocketHandle);
    destructor Destroy; override;
    property TLSServer;
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
  protected
    FSocket: TIdSocketHandle;
    FTimeout: Longint;
    FReadable: Boolean;
    procedure HandleException(E: Exception);
  public
    constructor Create(ASocket: TIdSocketHandle; TimeOut: Longint);
    destructor Destroy; override;
    function WaitForData(Timeout: Longint): Boolean;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Socket: TIdSocketHandle read FSocket write FSocket;
    property TimeOut: Longint read FTimeout write FTimeout;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, TlsUtils,
  {$ENDIF}
  IdException;

type
  TIdSocketHandleHack = class(TIdSocketHandle)
  private
    FOwner: TTLSIdSockSlave;
    FStream: TIdSocketStream;
  public
    constructor CreateTLS(AOwner: TTLSIdSockSlave);
    destructor Destroy; override;
  end;

{ TCustomTLSIdSock }
function TCustomTLSIdSock.CheckRecvResult(ASize: Integer): Boolean;
begin
  if ASize < 0 then begin
    Result := False;
    InternalSetConnected(False);
    TLSServer := nil;
  end else
    Result := True;
end;

constructor TCustomTLSIdSock.Create(ASocket: TIdSocketHandle);
begin
  inherited Create;
  FSocket := ASocket;
  FBindAddress := '0.0.0.0';
  FBindMaxPort := 65535;
  FTimeOut := SsTLSIdDefaultTimeOut;
  FInternalLock := TCriticalSection.Create;
  FCloseLock := TCriticalSection.Create;
end;

destructor TCustomTLSIdSock.Destroy;
begin
  FCloseLock.Free;
  FInternalLock.Free;
  inherited;
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

procedure TCustomTLSIdSock.InternalLock;
begin
  if ThreadLocked then
    FInternalLock.Acquire;
end;

procedure TCustomTLSIdSock.InternalUnlock;
begin
  if ThreadLocked then
    FInternalLock.Release;
end;

procedure TCustomTLSIdSock.RawClose;
var
  vStream: TIdSocketStream;
begin
  FCloseLock.Acquire;
  try
    if Assigned(FSocket) then begin
      if FSocket is TIdSocketHandleHack then begin
        vStream := TIdSocketHandleHack(FSocket).FStream;
        TIdSocketHandleHack(FSocket).FStream := nil;
        if Assigned(vStream) then
          vStream.FSocket := nil;
      end;
      FSocket.CloseSocket;
    end;
  finally
    FCloseLock.Release;
  end;
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
    FSocket.ClientPortMin := FBindMinPort;
    FSocket.ClientPortMax := FBindMaxPort;
    FSocket.AllocateSocket;
    FSocket.Bind;
    if GStack.IsIP(FHost) then
      FAddress := FHost
    else
      FAddress := GStack.ResolveHost(FHost);
    FSocket.SetPeer(FAddress,FPort);
    try
      GStack.CheckForSocketError(FSocket.Connect);
      InternalSetConnected(True);
    except
      InternalSetConnected(False);
      raise;
    end;
  end;
end;

function TCustomTLSIdSock.RawReceive: TStream;
var
  OK: Boolean;
  SS: TIdSocketStream;
  MS: TMemoryStream;
  Size: Integer;
begin
  Result := nil;
  try
    OK := Assigned(FSocket) and FSocket.Readable(0);
  except
    on EIdConnClosedGracefully do begin
      InternalSetConnected(False);
      OK := False;
    end;
  else
    raise;
  end;
  if OK then begin
    CheckAbort;
    if FFewClientsMuchData then begin
      SS := TIdSocketStream.Create(FSocket,FTimeOut);
      SS.FReadable := True;
      Result := SS;
    end else begin
      Result := TMemoryStream.Create;
      try
        MS := TMemoryStream(Result);
        MS.SetSize($4000);
        try
          Size := FSocket.Recv(MS.Memory^,$4000,0);
        except
          Size := 0;
        end;
        CheckAbort;
        if CheckRecvResult(Size) and (Size > 0) then begin
          MS.SetSize(Size);
          Result := MS;
        end else begin
          Result := nil;
          MS.Free;
        end;
      except
        MS := TMemoryStream(Result);
        Result := nil;
        MS.Free;
      end;
    end;
  end;
end;

procedure TCustomTLSIdSock.RawSend(Strm: TCustomMemoryStream);
var
  Res: Integer;
begin
  if FSocket = nil then
    Res := -1
  else try
    Res := FSocket.Send(Strm.Memory^,Strm.Size,0);
    GStack.CheckForSocketError(Res);
  except
    Res := -1;
  end;
  if Res <> Strm.Size then
    InternalSetConnected(False);            
  CheckAbort;
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

procedure TCustomTLSIdSock.SetFewClientsMuchData(const Value: Boolean);
begin
  FFewClientsMuchData := Value;
  if Value then
    SleepInterval := 1
  else
    SleepInterval := 50;
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

procedure TCustomTLSIdSock.SetThreadLocked(const Value: Boolean);
begin
  FThreadLocked := Value;
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

constructor TTLSIdSockSlave.CreateSocket(ASocket: TIdStackSocketHandle);
begin
  FSlaveSocket := TIdSocketHandleHack.CreateTLS(Self);
  with TIdSocketHandleHack(FSlaveSocket) do begin
    FHandle := ASocket;
    FHandleAllocated := ASocket <> Id_INVALID_SOCKET;
  end;
  InternalSetConnected(ASocket <> Id_INVALID_SOCKET);
  Create(FSlaveSocket);
end;

destructor TTLSIdSockSlave.Destroy;
begin
  Close;
  TIdSocketHandleHack(FSlaveSocket).FOwner := nil;
  FSlaveSocket.Free;
  inherited;
end;

procedure TTLSIdSockSlave.Disconnect;
begin
  InternalDisconnect;
end;

procedure TTLSIdSockSlave.DoConnect;
var
  Ticks: Cardinal;
begin
  GetRemoteHost;
  InternalSetConnected(FSocket.Handle <> Id_INVALID_SOCKET);
  Ticks := GetTickCount;
  while Connected and not Encrypted do begin
    Receive;
    if GetTickCount - Ticks > ConnectTimeOut then begin
      RawClose;         
      InternalSetConnected(False);
      TLSServer := nil;
    end;
  end;
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
  if ASocket is TIdSocketHandleHack then
    TIdSocketHandleHack(ASocket).FStream := Self;
  if TimeOut <= 0 then TimeOut := 20000;
  FTimeOut := TimeOut;
end;

destructor TIdSocketStream.Destroy;
begin
  if FSocket is TIdSocketHandleHack then
    TIdSocketHandleHack(FSocket).FStream := nil;
  inherited;
end;

procedure TIdSocketStream.HandleException(E: Exception);
begin
  if FSocket is TIdSocketHandleHack then
    TIdSocketHandleHack(FSocket).FOwner.InternalSetConnected(False)
  else if not (E is EIdConnClosedGracefully) then
    raise E;
end;

function TIdSocketStream.Read(var Buffer; Count: Integer): Longint;
begin
  try
    if Count > 0 then begin
      if WaitForData(FTimeOut) and Assigned(FSocket) then
        Result := FSocket.Recv(Buffer,Count,0)
      else
        Result := 0;
      if FSocket = nil then
        raise Exception.Create('TIdSocketStream.Read: Socket destroyed');
      FReadable := False;
    end else
      Result := 0;
  except
    on E: Exception do begin
      HandleException(E);
      Result := 0;
    end;
  else
    raise;
  end;
end;

function TIdSocketStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := 0;
end;

function TIdSocketStream.WaitForData(Timeout: Integer): Boolean;
begin
  Result := FReadable;
  if not Result then begin
    try
      while Assigned(FSocket) and (Timeout >= 0) and not Result do begin
        Result := FSocket.Readable(0);
        if not Result then begin
          Dec(Timeout,10);
          if Timeout > 0 then
            Sleep(10);
        end;
      end;
    except
      on E: Exception do begin
        HandleException(E);
        Result := False;
      end;
    else
      raise;
    end;
    FReadable := Result;
  end;
end;

function TIdSocketStream.Write(const Buffer; Count: Integer): Longint;
begin
  try
    Result := FSocket.Send(Pointer(@Buffer)^,Count,0);
  except
    on E: Exception do begin
      HandleException(E);
      Result := 0;
    end;
  else
    raise;
  end;
end;

{ TIdSocketHandleHack }

constructor TIdSocketHandleHack.CreateTLS(AOwner: TTLSIdSockSlave);
begin
  inherited Create(nil);
  FOwner := AOwner;
end;

destructor TIdSocketHandleHack.Destroy;
begin
  if Assigned(FStream) then
    FStream.FSocket := nil;
  if Assigned(FOwner) then begin
    FOwner.Lock;
    try
      FOwner.FSlaveSocket := nil;
      FOwner.Close;
    finally
      FOwner.Unlock;
    end;
  end;
  inherited;
end;

end.
