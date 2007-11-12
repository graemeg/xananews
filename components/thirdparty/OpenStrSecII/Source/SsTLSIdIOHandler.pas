{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsTLSIdIOHandler Unit                             }
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
unit SsTLSIdIOHandler;

interface

uses
  SysUtils, Classes, SyncObjs, TLSInternalServer,
  SsTLSIdSocket,
  IdStackConsts, IdGlobal, IdStack, IdThread, IdSocketHandle, IdBaseComponent,
  IdComponent, IdIOHandler, IdIOHandlerSocket,
  {$IFDEF USE_INDY_HTTP}
  IdSSLOpenSSL,
  {$ENDIF}
  IdServerIOHandler;

type
  {$IFDEF USE_INDY_HTTP}
  TSsTLSIdIOHandlerSocket = class(TIdSSLIOHandlerSocket)
  {$ELSE}
  TSsTLSIdIOHandlerSocket = class(TIdIOHandlerSocket)
  {$ENDIF}
  private
    FFewClientsMuchData: Boolean;
    FConnectTimeOut: Cardinal;
    FTimeOut: Cardinal;
    FTLSServer: TCustomTLSInternalServer;
    FTLSSocket: TTLSIdSockSlave; 
    FThreadLocked: Boolean;
    procedure SetConnectTimeOut(const Value: Cardinal);
    procedure SetFewClientsMuchData(const Value: Boolean);
    procedure SetThreadLocked(const Value: Boolean);
    procedure SetTimeOut(const Value: Cardinal);
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
  protected
  {$IFDEF USE_INDY_HTTP}
    procedure OpenEncodedConnection; override;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterAccept; override;
    procedure ConnectClient(const AHost: string; const APort: Integer; const ABoundIP: string;
     const ABoundPort: Integer; const ABoundPortMin: Integer; const ABoundPortMax: Integer;
     const ATimeout: Integer = IdTimeoutDefault); override;
    procedure Close; override;
    procedure Open; override;

    function Readable(AMSec: integer = IdTimeoutDefault): boolean; override;
    function Recv(var ABuf; ALen: integer): integer; override;
    function Send(var ABuf; ALen: integer): integer; override;

    property TLSSocket: TTLSIdSockSlave read FTLSSocket;
  published
    property ConnectTimeOut: Cardinal read FConnectTimeOut write SetConnectTimeOut default SsTLSIdDefaultTimeOut;
    property FewClientsMuchData: Boolean read FFewClientsMuchData write SetFewClientsMuchData default False;
    property ReadTimeOut: Cardinal read FTimeOut write SetTimeOut default SsTLSIdDefaultTimeOut;
    property ThreadLocked: Boolean read FThreadLocked write SetThreadLocked default False;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

  TSsTLSIdServerIOHandler = class(TIdServerIOHandler)
  private
    FTLSServer: TCustomTLSInternalServer;
    FIsInitialized: Boolean;
    FComponentsLock: TCriticalSection;
    FTimeOut: Cardinal;
    FFewClientsMuchData: Boolean;
    FConnectTimeOut: Cardinal;
    FThreadLocked: Boolean;
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    procedure SetTimeOut(const Value: Cardinal);
    procedure SetFewClientsMuchData(const Value: Boolean);
    procedure SetConnectTimeOut(const Value: Cardinal);
    procedure SetThreadLocked(const Value: Boolean);
  protected
    procedure ComponentsLock;
    procedure ComponentsUnlock;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override;
    function Accept(ASocket: TIdStackSocketHandle; AThread: TIdThread = nil): TIdIOHandler; override;
    procedure CloseAll;
  published
    property ConnectTimeOut: Cardinal read FConnectTimeOut write SetConnectTimeOut default SsTLSIdDefaultTimeOut;
    property FewClientsMuchData: Boolean read FFewClientsMuchData write SetFewClientsMuchData default False;
    property ReadTimeOut: Cardinal read FTimeOut write SetTimeOut default SsTLSIdDefaultTimeOut;
    property ThreadLocked: Boolean read FThreadLocked write SetThreadLocked default True;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write SetTLSServer;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  MpArith, IdException;

type
  TTLSIdSockSlaveHack = class(TTLSIdSockSlave);

{ TSsTLSIdIOHandlerSocket }

procedure TSsTLSIdIOHandlerSocket.AfterAccept;
var
  vTLSSocket: TTLSIdSockSlave;
begin
  if Assigned(FTLSServer) then begin
    try
      vTLSSocket := FTLSSocket;
      FTLSSocket := nil;
      vTLSSocket.Release;
    except
    end;
    vTLSSocket := nil;
    try
      vTLSSocket := TTLSIdSockSlave.CreateSocket(Binding.Handle);
      vTLSSocket.ThreadLocked := ThreadLocked;
      vTLSSocket.TLSServer := TLSServer;
      vTLSSocket.TimeOut := ReadTimeOut;
      vTLSSocket.ConnectTimeOut := ConnectTimeOut;
      vTLSSocket.FewClientsMuchData := FewClientsMuchData;
      FTLSSocket := vTLSSocket;
      TTLSIdSockSlaveHack(FTLSSocket).DoConnect;
      if not FTLSSocket.Connected then
        Close;
    except
      on E: Exception do begin
        FTLSSocket := nil;
        vTLSSocket.Release;
        Close;
      end;
    end;
  end else
    raise Exception.Create('IO Handler Socket accepted too soon');
end;

procedure TSsTLSIdIOHandlerSocket.Close;
begin
  if Assigned(FTLSSocket) then
    FTLSSocket.Close;
  inherited Close;
end;

procedure TSsTLSIdIOHandlerSocket.ConnectClient(const AHost: string;
  const APort: Integer; const ABoundIP: string; const ABoundPort,
  ABoundPortMin, ABoundPortMax, ATimeout: Integer);
begin
{$IFDEF USE_INDY_HTTP}
  Passthrough := True;
{$ENDIF}
  inherited ConnectClient(AHost,APort,ABoundIP,ABoundPort,ABoundPortMin,ABoundPortMax,ATimeOut);
  if Assigned(TLSServer) and Binding.HandleAllocated then begin
{$IFDEF USE_INDY_HTTP}
    Passthrough := True;
{$ENDIF}
    FTLSSocket.Release;
    FTLSSocket := TTLSIdSockSlave.CreateSocket(Binding.Handle);
    FTLSSocket.TLSServer := TLSServer;
    FTLSSocket.ConnectTimeOut := ConnectTimeOut;
    FTLSSocket.TimeOut := ReadTimeOut;
    FTLSSocket.FewClientsMuchData := FewClientsMuchData;
    FTLSSocket.IPToCheck := Binding.PeerIP;
    FTLSSocket.DNSNameToCheck := GStack.WSGetHostByAddr(Binding.IP);
    with GStack.WSGetServByPort(APort) do begin
      if Count > 0 then
        FTLSSocket.URIToCheck := Strings[0] + '://' + AHost + '/';
      Free;
    end;
    TTLSIdSockSlaveHack(FTLSSocket).InternalSetConnected(True);
    FTLSSocket.Open;
    if not FTLSSocket.Connected then
      Close;
  end;
end;

constructor TSsTLSIdIOHandlerSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeOut := SsTLSIdDefaultTimeOut;
  FConnectTimeOut := SsTLSIdDefaultTimeOut;
  LockMPPoolAllocations(True);
end;

destructor TSsTLSIdIOHandlerSocket.Destroy;
var
  vOwner: TSsTLSIdServerIOHandler;
begin
  TLSServer := nil;
  FTLSSocket.Free;
  FTLSSocket := nil;
  LockMPPoolAllocations(False);
  if Owner is TSsTLSIdServerIOHandler then begin
    vOwner := TSsTLSIdServerIOHandler(Owner);
    vOwner.ComponentsLock;
    try
      inherited;
    finally
      vOwner.ComponentsUnlock;
    end;
  end else
    inherited;
end;

procedure TSsTLSIdIOHandlerSocket.Open;
begin
  inherited Open;
end;
                   
{$IFDEF USE_INDY_HTTP}
procedure TSsTLSIdIOHandlerSocket.OpenEncodedConnection;
begin
  // Not done here.
end;
{$ENDIF}

function TSsTLSIdIOHandlerSocket.Readable(AMSec: integer): boolean;
var
  Ticks: Integer;
begin
  if not Assigned(FTLSServer) then
    Result := inherited Readable(AMSec)
  else if Assigned(FTLSSocket) then begin
    if AMSec = IdTimeoutInfinite then
      AMSec := MaxInt
    else if AMSec = IdTimeOutDefault then
      AMSec := ReadTimeOut;
    try
      repeat
        Ticks := Integer(GetTickCount);
        Result := FTLSSocket.ReceiveLength > 0;
        AMSec := AMSec - (Integer(GetTickCount) - Ticks);
      until Result or (AMSec <= 0) or (FTLSSocket = nil) or not FTLSSocket.Connected;
    except
      on EIdConnClosedGracefully do
        Result := False;
    else
      raise
    end;
  end else
    Result := False;
end;

function TSsTLSIdIOHandlerSocket.Recv(var ABuf; ALen: integer): integer;
var
  DLen: Integer;
  Ticks: Cardinal;
begin
  Result := 0;
  if not Assigned(FTLSServer) then
    Result := inherited Recv(ABuf,ALen)
  else if Assigned(FTLSSocket) and (ALen > 0) then try
    DLen := FTLSSocket.ReceiveLength;
    if DLen > ALen then DLen := ALen;
    if DLen > 0 then
      Result := FTLSSocket.ReceiveBuf(ABuf,DLen)
    else begin
      Ticks := GetTickCount;
      while Assigned(FTLSSocket) and FTLSSocket.Connected and (DLen = 0) do begin
        DLen := FTLSSocket.ReceiveLength;
        if DLen > ALen then DLen := ALen;
        if DLen > 0 then
          Result := FTLSSocket.ReceiveBuf(ABuf,DLen)
        else if GetTickCount > Ticks + FTLSSocket.TimeOut then
          DLen := -1;
      end;
    end;
  except
    on EIdConnClosedGracefully do {nothing} ;
  else
    raise;
  end else
    Result := 0;
end;

function TSsTLSIdIOHandlerSocket.Send(var ABuf; ALen: integer): integer;
begin
  if not Assigned(FTLSServer) then
    Result := inherited Send(ABuf,ALen)
  else if Assigned(FTLSSocket) then begin
    Result := ALen;
    FTLSSocket.SendBuf(ABuf,ALen);
  end else
    Result := 0;
end;

procedure TSsTLSIdIOHandlerSocket.SetConnectTimeOut(const Value: Cardinal);
begin
  FConnectTimeOut := Value;
end;

procedure TSsTLSIdIOHandlerSocket.SetFewClientsMuchData(
  const Value: Boolean);
begin
  FFewClientsMuchData := Value;
end;

procedure TSsTLSIdIOHandlerSocket.SetThreadLocked(const Value: Boolean);
begin
  FThreadLocked := Value;
end;

procedure TSsTLSIdIOHandlerSocket.SetTimeOut(const Value: Cardinal);
begin
  FTimeOut := Value;
end;

procedure TSsTLSIdIOHandlerSocket.SetTLSServer(
  const Value: TCustomTLSInternalServer);
begin
  FTLSServer := Value;
  if Value = nil then
    Close;
end;

{ TSsTLSIdServerIOHandler }

function TSsTLSIdServerIOHandler.Accept(ASocket: TIdStackSocketHandle;
  AThread: TIdThread): TIdIOHandler;
var
  tmpIdCISsTLS: TSsTLSIdIOHandlerSocket;
begin
  if not FIsInitialized then begin
    Init;
  end;

  FComponentsLock.Acquire;
  try
    tmpIdCISsTLS := TSsTLSIdIOHandlerSocket.Create(Self);
  finally
    FComponentsLock.Release;
  end;
  tmpIdCISsTLS.ReadTimeOut := ReadTimeOut;
  tmpIdCISsTLS.FewClientsMuchData := FewClientsMuchData;
  tmpIdCISsTLS.ConnectTimeOut := ConnectTimeOut;
  tmpIdCISsTLS.ThreadLocked := ThreadLocked;
  tmpIdCISsTLS.Open;
  if tmpIdCISsTLS.Binding.Accept(ASocket) then begin
    tmpIdCISsTLS.TLSServer := FTLSServer;
    Result := tmpIdCISsTLS;
  end else begin
    Result := nil;
    tmpIdCISsTLS.Free;
  end;
end;

procedure TSsTLSIdServerIOHandler.CloseAll;
var
  I: Integer;
begin
  ComponentsLock;
  try
    for I := ComponentCount - 1 downto 0 do
      if Components[I] is TSsTLSIdIOHandlerSocket then
        TSsTLSIdIOHandlerSocket(Components[I]).Close;
  finally
    ComponentsUnlock;
  end;
end;

procedure TSsTLSIdServerIOHandler.ComponentsLock;
begin
  if Assigned(FComponentsLock) then
    FComponentsLock.Acquire;
end;

procedure TSsTLSIdServerIOHandler.ComponentsUnlock;
begin
  if Assigned(FComponentsLock) then
    FComponentsLock.Release;
end;

constructor TSsTLSIdServerIOHandler.Create(AOwner: TComponent);
begin
  inherited;
  FIsInitialized := False;
  FComponentsLock := TCriticalSection.Create;
  FTimeOut := SsTLSIdDefaultTimeOut;
  FConnectTimeOut := SsTLSIdDefaultTimeOut;
  FFewClientsMuchData := False;
  FThreadLocked := True;
end;

destructor TSsTLSIdServerIOHandler.Destroy;
begin
  CloseAll;
  FComponentsLock.Free;
  inherited;
end;

procedure TSsTLSIdServerIOHandler.Init;
begin
  if FTLSServer = nil then
    raise Exception.Create('TSsTLSIdServerIOHandler.Init: No TLSServer assigned');
  FTLSServer.CheckLoaded;
  if FTLSServer.PublicKeyAlgorithms = [] then
    raise Exception.Create('TSsTLSIdServerIOHandler.Init: No Server Certificate');
  FTLSServer.TLSSetupServer;
  FIsInitialized := True;
end;

procedure TSsTLSIdServerIOHandler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if Operation = opRemove then begin
    if AComponent = FTLSServer then
      TLSServer := nil;
  end;
end;

procedure TSsTLSIdServerIOHandler.SetConnectTimeOut(const Value: Cardinal);
begin
  FConnectTimeOut := Value;
end;

procedure TSsTLSIdServerIOHandler.SetFewClientsMuchData(
  const Value: Boolean);
begin
  FFewClientsMuchData := Value;
end;

procedure TSsTLSIdServerIOHandler.SetThreadLocked(const Value: Boolean);
begin
  FThreadLocked := Value;
end;

procedure TSsTLSIdServerIOHandler.SetTimeOut(const Value: Cardinal);
begin
  FTimeOut := Value;
end;

procedure TSsTLSIdServerIOHandler.SetTLSServer(
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
