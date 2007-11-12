{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSClass Unit                                     }
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
unit TlsClass;

interface             
{$IFDEF SHA1_AND_MD5}

uses
  Classes, Asn1, Tls, MpX509;

type
  TAlertEvent = procedure (Sender: TObject;
                           var Fatal: Boolean;
                           AlertCode: Integer) of object;
  TSelectCompressionEvent = procedure (Sender: TObject;
                                       const CompMethods: array of TCompressionMethod;
                                       var CompMethod: TCompressionMethod) of object;
  {$HPPEMIT 'typedef void __fastcall (__closure *TRenegotiateEvent)(System::TObject* Sender, const Tls::TSessionID &SessionID,  System::ShortString  &MasterSecret, const int MasterSecret_Size);'}
  {$NODEFINE TRenegotiateEvent}
  TRenegotiateEvent = procedure (Sender: TObject;
                                 const SessionID: TSessionID;
                                 var MasterSecret: ShortString) of object;

  TRecordType = (rtUnknown,rtChangeCipherSpec,rtAlert,rtHandshake,rtData);

  TCompressEvent = procedure (Sender: TObject;
                              Src: PTLSPlainText; PTLen: Integer;
                              Method: Byte;
                              var Dst: PTLSCompressed; var CLen: Integer;
                              var Error: Integer) of object;

  TDecompressEvent = procedure (Sender: TObject;
                                Src: PTLSCompressed; CLen: Integer;
                                Method: Byte;
                                var Dst: PTLSPlainText; var PTLen: Integer;
                                var Error: Integer) of object;

  TGetCertificateEvent = procedure (Sender: TObject;
                                    KEA: TKeyExchangeAlgorithm;
                                    SignAlg: TSignatureAlgorithm;
                                    var H: PTLSHandshake;
                                    var Len: Integer;
                                    var C: PASN1Struct;
                                    var Found: Boolean) of object;

  TGetCertReqEvent = procedure (Sender: TObject;
                                KEA: TKeyExchangeAlgorithm;
                                var H: PTLSHandshake;
                                var Len: Integer;
                                var Found: Boolean) of object;

  TCustomTLS_ContentLayer = class
  private
    FRefCount: Integer;
    FUserData: Pointer;
    FLastAlertCode: Integer;
    FUnmanaged: Boolean;
    FAcceptHelloRequest: Boolean;
    FIPToCheck: string;
    FURIToCheck: string;
    FDNSNameToCheck: string;
    procedure SetAcceptedCiphers(const Value: TCipherSuites);
    procedure SetCACerts(const Value: TX509TrustedCertificates);
    procedure SetCipherPreference(const Value: TCipherSuites);
    procedure SetCollectedCerts(const Value: TX509TrustedCertificates);
    procedure SetCompressionPreference(const Value: TCompressionMethods);
    procedure SetDHEphemeralKeySize(const Value: Integer);
    procedure SetDHEphemeralQSize(const Value: Integer);
    procedure SetECDHEphemeralKeySize(const Value: Integer);
    procedure SetMyCerts(const Value: TX509TrustedCertificates);
    procedure SetOnIncomingAlert(const Value: TAlertEvent);
    procedure SetOnOutgoingAlert(const Value: TAlertEvent);
    procedure SetOnRenegotiate(const Value: TRenegotiateEvent);
    procedure SetOnSelectCompression(const Value: TSelectCompressionEvent);
    procedure SetRequestClientAuth(const Value: Boolean);
    procedure SetRequireClientAuth(const Value: Boolean);
    procedure SetUserData(const Value: Pointer);
    procedure SetOnGetCertificate(const Value: TGetCertificateEvent);
    procedure SetOnGetCertReq(const Value: TGetCertReqEvent);
    procedure SetAcceptHelloRequest(const Value: Boolean);
    procedure SetDNSNameToCheck(const Value: string);
    procedure SetIPToCheck(const Value: string);
    procedure SetURIToCheck(const Value: string);
  protected
    FOnChangeCipherSpec: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnCompress: TCompressEvent;
    FOnDecompress: TDecompressEvent;
    FRequireClientAuth: Boolean;
    FRequestClientAuth: Boolean;
    FActive: Boolean;
    FECDHEphemeralKeySize: Integer;
    FDHEphemeralKeySize: Integer;
    FDHEphemeralQSize: Integer;   
    FOnGetCertificate: TGetCertificateEvent;
    FOnGetCertReq: TGetCertReqEvent;
    FOnIncomingAlert: TAlertEvent;
    FOnOutgoingAlert: TAlertEvent;
    FAcceptedCiphers: TCipherSuites;
    FCipherPreference: TCipherSuites;
    FCompressionPreference: TCompressionMethods;
    FOnRenegotiate: TRenegotiateEvent;
    FOnSelectCompression: TSelectCompressionEvent;
    FCollectedCerts: TX509TrustedCertificates;
    FCACerts: TX509TrustedCertificates;
    FMyCerts: TX509TrustedCertificates;
    FSessionID: TSessionID;
    procedure DoDestroy;
    function GetClientCertificate: PASN1Struct; virtual;
    function GetContext: PTLSSecurityParams; virtual;
    function GetEncrypted: Boolean; virtual;
    function GetServerCertificate: PASN1Struct; virtual;
    procedure SetLastAlertCode(const Value: Integer);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property RefCount: Integer read FRefCount;
  public
    constructor Create;
    destructor Destroy; override;
    function Accept(Src, Response: TStream): Integer; virtual; abstract;
    function Close(Response: TStream): Integer; virtual; abstract;
    function Connect(Response: TStream): Integer; virtual; abstract;
    function DecodeData(Src, Data, Response: TStream): Integer; overload; virtual; abstract;
    function DecodeData(const Src: TTLSEncrypted;
                        var Data: TPlainTextFragment;
                        var Response: TTLSEncrypted): Integer; overload;
    function EncodeData(Src, Response: TStream): Integer; overload; virtual; abstract;
    procedure Release;
    property Active: Boolean read FActive;
    property LastAlertCode: Integer read FLastAlertCode;
    property Unmanaged: Boolean read FUnmanaged write FUnmanaged;
    property AcceptedCiphers: TCipherSuites read FAcceptedCiphers write SetAcceptedCiphers;
    property AcceptHelloRequest: Boolean read FAcceptHelloRequest write SetAcceptHelloRequest;
    property CipherPreference: TCipherSuites read FCipherPreference write SetCipherPreference;
    property ClientCertificate: PASN1Struct read GetClientCertificate;
    property CompressionPreference: TCompressionMethods read FCompressionPreference write SetCompressionPreference;
    property Context: PTLSSecurityParams read GetContext;
    property DHEphemeralKeySize: Integer read FDHEphemeralKeySize write SetDHEphemeralKeySize;
    property DHEphemeralQSize: Integer read FDHEphemeralQSize write SetDHEphemeralQSize;
    property ECDHEphemeralKeySize: Integer read FECDHEphemeralKeySize write SetECDHEphemeralKeySize;
    property Encrypted: Boolean read GetEncrypted;
    property CACerts: TX509TrustedCertificates read FCACerts write SetCACerts;
    property CollectedCerts: TX509TrustedCertificates read FCollectedCerts write SetCollectedCerts;
    property MyCerts: TX509TrustedCertificates read FMyCerts write SetMyCerts;
    property RequestClientAuth: Boolean read FRequestClientAuth write SetRequestClientAuth;
    property RequireClientAuth: Boolean read FRequireClientAuth write SetRequireClientAuth;
    property ServerCertificate: PASN1Struct read GetServerCertificate;
    property URIToCheck: string read FURIToCheck write SetURIToCheck;
    property IPToCheck: string read FIPToCheck write SetIPToCheck;
    property DNSNameToCheck: string read FDNSNameToCheck write SetDNSNameToCheck;
    property SessionID: TSessionID read FSessionID write FSessionID;
    property UserData: Pointer read FUserData write SetUserData;
    property OnChangeCipherSpec: TNotifyEvent read FOnChangeCipherSpec write FOnChangeCipherSpec;
    property OnCompress: TCompressEvent read FOnCompress write FOnCompress;
    property OnDecompress: TDecompressEvent read FOnDecompress write FOnDecompress;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnGetCertificate: TGetCertificateEvent read FOnGetCertificate write SetOnGetCertificate;
    property OnGetCertReq: TGetCertReqEvent read FOnGetCertReq write SetOnGetCertReq;
    property OnIncomingAlert: TAlertEvent read FOnIncomingAlert write SetOnIncomingAlert;
    property OnOutgoingAlert: TAlertEvent read FOnOutgoingAlert write SetOnOutgoingAlert;
    property OnRenegotiate: TRenegotiateEvent read FOnRenegotiate write SetOnRenegotiate;
    property OnSelectCompression: TSelectCompressionEvent read FOnSelectCompression write SetOnSelectCompression;
  end;

function TLSLayerInstanceCount: Integer;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  ReadStrm, SysUtils;

var
  GTLSLayerInstanceCount: Integer;

function TLSLayerInstanceCount: Integer;
begin
  Result := GTLSLayerInstanceCount;
end;

type
  TBufferStream = class(TCustomMemoryStream)
  private
    FCapacity: Integer;
  public
    constructor Create(var Buf; Capacity: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TCustomTLS_ContentLayer }

constructor TCustomTLS_ContentLayer.Create;
begin            
  InterlockedIncrement(GTLSLayerInstanceCount);
  FRefCount := 1;
end;

function TCustomTLS_ContentLayer.DecodeData(const Src: TTLSEncrypted;
  var Data: TPlainTextFragment; var Response: TTLSEncrypted): Integer;
var
  SrcStream: TReadStream;
  DataStream, ResponseStream: TBufferStream;
begin
  SrcStream := TReadStream.Create(Src,SizeOf(Src));
  try              
    DataStream := TBufferStream.Create(Data,SizeOf(Data));
    try
      ResponseStream := TBufferStream.Create(Response,SizeOf(Response));
      try
        Result := DecodeData(SrcStream,DataStream,ResponseStream);
      finally
        ResponseStream.Free;
      end;
    finally
      DataStream.Free;
    end;
  finally
    SrcStream.Free;
  end;
end;

destructor TCustomTLS_ContentLayer.Destroy;
begin
  inherited;
  InterlockedDecrement(GTLSLayerInstanceCount);
end;

procedure TCustomTLS_ContentLayer.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FOnDestroy := nil;
end;

function TCustomTLS_ContentLayer.GetClientCertificate: PASN1Struct;
begin
  Result := nil;
end;

function TCustomTLS_ContentLayer.GetContext: PTLSSecurityParams;
begin
  Result := nil;
end;

function TCustomTLS_ContentLayer.GetEncrypted: Boolean;
begin
  Result := False;
end;

function TCustomTLS_ContentLayer.GetServerCertificate: PASN1Struct;
begin
  Result := nil;
end;

procedure TCustomTLS_ContentLayer.Release;
begin
  if Self <> nil then begin
    DoDestroy;
    _Release;
  end;
end;

procedure TCustomTLS_ContentLayer.SetAcceptedCiphers(
  const Value: TCipherSuites);
begin
  FAcceptedCiphers := Value;
end;

procedure TCustomTLS_ContentLayer.SetAcceptHelloRequest(
  const Value: Boolean);
begin
  FAcceptHelloRequest := Value;
end;

procedure TCustomTLS_ContentLayer.SetCACerts(
  const Value: TX509TrustedCertificates);
begin
  FCACerts := Value;
end;

procedure TCustomTLS_ContentLayer.SetCipherPreference(
  const Value: TCipherSuites);
begin
  FCipherPreference := Value;
end;

procedure TCustomTLS_ContentLayer.SetCollectedCerts(
  const Value: TX509TrustedCertificates);
begin
  FCollectedCerts := Value;
end;

procedure TCustomTLS_ContentLayer.SetCompressionPreference(
  const Value: TCompressionMethods);
begin
  FCompressionPreference := Value;
end;

procedure TCustomTLS_ContentLayer.SetDHEphemeralKeySize(
  const Value: Integer);
begin
  FDHEphemeralKeySize := Value;
end;

procedure TCustomTLS_ContentLayer.SetDHEphemeralQSize(
  const Value: Integer);
begin
  FDHEphemeralQSize := Value;
end;

procedure TCustomTLS_ContentLayer.SetDNSNameToCheck(const Value: string);
begin
  FDNSNameToCheck := Value;
end;

procedure TCustomTLS_ContentLayer.SetECDHEphemeralKeySize(
  const Value: Integer);
begin
  FECDHEphemeralKeySize := Value;
end;

procedure TCustomTLS_ContentLayer.SetIPToCheck(const Value: string);
begin
  FIPToCheck := Value;
end;

procedure TCustomTLS_ContentLayer.SetLastAlertCode(const Value: Integer);
begin
  FLastAlertCode := Value;
end;

procedure TCustomTLS_ContentLayer.SetMyCerts(
  const Value: TX509TrustedCertificates);
begin
  FMyCerts := Value;
end;

procedure TCustomTLS_ContentLayer.SetOnGetCertificate(
  const Value: TGetCertificateEvent);
begin
  FOnGetCertificate := Value;
end;

procedure TCustomTLS_ContentLayer.SetOnGetCertReq(
  const Value: TGetCertReqEvent);
begin
  FOnGetCertReq := Value;
end;

procedure TCustomTLS_ContentLayer.SetOnIncomingAlert(
  const Value: TAlertEvent);
begin
  FOnIncomingAlert := Value;
end;

procedure TCustomTLS_ContentLayer.SetOnOutgoingAlert(
  const Value: TAlertEvent);
begin
  FOnOutgoingAlert := Value;
end;

procedure TCustomTLS_ContentLayer.SetOnRenegotiate(
  const Value: TRenegotiateEvent);
begin
  FOnRenegotiate := Value;
end;

procedure TCustomTLS_ContentLayer.SetOnSelectCompression(
  const Value: TSelectCompressionEvent);
begin
  FOnSelectCompression := Value;
end;

procedure TCustomTLS_ContentLayer.SetRequestClientAuth(
  const Value: Boolean);
begin
  FRequestClientAuth := Value;
end;

procedure TCustomTLS_ContentLayer.SetRequireClientAuth(
  const Value: Boolean);
begin
  FRequireClientAuth := Value;
end;

procedure TCustomTLS_ContentLayer.SetURIToCheck(const Value: string);
begin
  FURIToCheck := Value;
end;

procedure TCustomTLS_ContentLayer.SetUserData(const Value: Pointer);
begin
  FUserData := Value;
end;

function TCustomTLS_ContentLayer._AddRef: Integer;
begin
  if FRefCount <= 0 then
    raise Exception.Create('TCustomTLS_ContentLayer._AddRef: Already released');
  Result := InterlockedIncrement(FRefCount);
end;

function TCustomTLS_ContentLayer._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TBufferStream }

constructor TBufferStream.Create(var Buf; Capacity: Integer);
begin
  inherited Create;
  SetPointer(@Buf,0);
  FCapacity := Capacity;
end;

function TBufferStream.Write(const Buffer; Count: Integer): Longint;
var
  OldPos, Pos: Longint;
begin
  OldPos := Position;
  if (OldPos >= 0) and (Count >= 0) then
  begin
    Pos := OldPos + Count;
    if Pos > 0 then
    begin
      if Pos > Size then
      begin
        if Pos > FCapacity then
          raise Exception.Create('TBufferStream: Cannot write beyond end of buffer.');
        SetPointer(Memory,Pos);
      end;
      System.Move(Buffer, Pointer(Longint(Memory) + OldPos)^, Count);
      Position := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;
                               
{$ELSE  SHA1_AND_MD5}
implementation          
{$ENDIF SHA1_AND_MD5}
end.
