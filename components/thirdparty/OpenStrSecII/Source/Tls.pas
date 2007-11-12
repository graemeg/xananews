{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLS Unit                                          }
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
unit Tls;

interface
     
{$IFDEF SHA1_AND_MD5}
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  Classes, SecUtils, TlsConst, MpArithTypes, Asn1, MpDL, MpIF, MpEC,
  X509Base, TlsUtils;

type
  TCompressionMethod = Byte;
  TCompressionMethods = array of TCompressionMethod;

  TSessionID = string[32];

  TProtocolVersion = packed record
    major, minor: Byte;
  end;

  TPlainTextFragment = packed array [0..MaxPlainTextLength-1] of Byte;

  TPlainTextLength = 0..MaxPlainTextLength;
  PTLSPlainText = ^TTLSPlainText;
  TTLSPlainText = packed record
    contentType: Byte;
    version: TProtocolVersion;
    length: TPlainTextLength;
    fragment: TPlainTextFragment;
  end;

  TCompressedLength = 0..MaxCompressedLength;
  PTLSCompressed = ^TTLSCompressed;
  TTLSCompressed = packed record
    contentType: Byte;
    version: TProtocolVersion;
    length: TCompressedLength;
    fragment: packed array [0..MaxCompressedLength-1] of Byte;
  end;

  TEncryptedLength = 0..MaxCipherTextLength;
  PTLSEncrypted = ^TTLSEncrypted;
  TTLSEncrypted = packed record
    contentType: Byte;
    version: TProtocolVersion;
    length: TEncryptedLength;
    fragment: packed array [0..MaxCipherTextLength-1] of Byte;
  end;

  TCipherSuite = array [0..1] of Byte;
  TCipherSuites = array of TCipherSuite;

  TConnectionEnd = (ceServer,ceClient);
  TBulkCipherAlgorithm = (bcaNull,bcaRC2_40,bcaRC4,bcaRC4_40,bcaRC4_56,bcaDES,bca3DES,
                          bcaDES40,bcaDES56,bcaAES128,bcaAES256,
                          // Application specific:
                          bcaAES192,
                          bcaAES128_CTR,bcaAES192_CTR,bcaAES256_CTR,
                          bcaTwoFish128,bcaTwoFish192,bcaTwoFish256,
                          bcaTwoFish128_CTR,bcaTwoFish192_CTR,bcaTwoFish256_CTR,
                          bcaBlowFish128,bcaBlowFish128_CTR,
                          bcaHelix);
  TCipherType = (ctStream,ctBlock);

  {$IFNDEF BCB}
  TRandomString = TLSUtils.TRandomString;
  {$ENDIF}
  TMasterSecret = packed array [0..47] of Byte;

  PTLSSecurityParams = ^TTLSSecurityParams;
  TTLSSecurityParams = record
    entity:                TConnectionEnd;
    bulk_cipher_algorithm: TBulkCipherAlgorithm;
    cipher_type:           TCipherType;
    key_size:              Word;
    key_material_length:   Word;
    is_exportable:         Boolean;
    mac_algorithm:         THashAlgorithm;
    compression_algorithm: TCompressionMethod;
    master_secret:         TMasterSecret;
    client_random:         TRandomString;
    server_random:         TRandomString;
  end;

  TTLSKeys = record
    hLen: Byte;
    client_write_MAC_secret,
    server_write_MAC_secret: packed array [0..63] of Byte;
    kLen: Byte;
    client_write_key,
    server_write_key: packed array [0..31] of Byte;
    bLen: Byte;
    client_write_iv,
    server_write_iv: packed array [0..31] of Char;
  end;

  TTLSAlert = packed record
    alertLevel,
    alertDescription: Byte;
  end;

  {$IFNDEF BCB}
  TRandom = TLSUtils.TRandom;
  {$ENDIF}

  TInternalClientHello = record
    client_version:      TProtocolVersion;
    random:              TRandom;
    session_id:          TSessionID;
    cipher_suite_count:  Word;
    cipher_suites:       TCipherSuites;
    compression_count:   Byte;
    compression_methods: TCompressionMethods;
  end;

  PClientHello = ^TClientHello;
  TClientHello = packed record
    client_version:      TProtocolVersion;
    random:              TRandom;
    data:                packed array [0..$FFFFFF] of Byte;
  end;

  TInternalServerHello = record
    server_version:      TProtocolVersion;
    random:              TRandom;
    session_id:          string[32];
    cipher_suite:        TCipherSuite;
    compression_method:  TCompressionMethod;
  end;

  PServerHello = ^TServerHello;
  TServerHello = packed record
    server_version:      TProtocolVersion;
    random:              TRandom;
    data:                packed array [0..33 + 2 + 1] of Byte;
  end;

  PCertificate = ^TCertificate;
  TCertificate = packed record
    certificate_list:    packed array [0..$FFFFFF] of Byte;
  end;

  TKeyExchangeAlgorithm = (keaNull,keaRSA,keaDH,keaDHE,keaRSA_Export,
                           keaDHE_Export,keaRSA1024,keaDHE1024,
                           keaECDH, keaECDHE);
  TServerRSAParams = record
    rsa_modulus:  PMPInteger;
    rsa_exponent: PMPInteger;
  end;
  TServerDHParams = record
    dh_p:  PMPInteger;
    dh_g:  PMPInteger;
    dh_Ys: PMPInteger;
  end;
  TECCurveType = (ctReserved,ctExplicitPrime,ctExplicitChar2,ctNamedCurve);
  TECParameters = record
    curve_type: TECCurveType;
    primeCurve: TECurve;
    curveName:  Byte; // Set to zero if explicit prime curve
  end;
  TServerECDHParams = record
    curve_params: TECParameters;
    public_key:   TECPoint;
  end;

  TSignatureAlgorithm = (saAnon,saRSA,saDSS,saECDSA);
  TSignatureInput = record
    case SA: TSignatureAlgorithm of
      saAnon: ();
      saRSA:  (rsaInput:
        packed record
          md5_hash: packed array [0..15] of Byte;
          sha_hash: packed array [0..19] of Byte;
        end);
      saDSS:  (sha_hash: packed array [0..19] of Byte);
      saECDSA:(sha2_hash: packed array [0..63] of Byte);
  end;

  TSignature = record
    signature: ISecretKey;
    input:     TSignatureInput;
  end;

  TInternalServerKeyExchange = record
    Signed: TSignature;
    case KEA: TKeyExchangeAlgorithm of
      keaRSA,
      keaRSA_Export,
      keaRSA1024:  (rsaParams:  TServerRSAParams);
      keaDHE,
      keaDHE_Export,
      keaDHE1024:  (dhParams:   TServerDHParams);
      keaECDH:     (ecdhParams: TServerECDHParams);
  end;

  PServerKeyExchange = ^TServerKeyExchange;
  TServerKeyExchange = packed record
    data:    packed array [0..$FFFFFF] of Byte;
  end;

  TCertificateTypes = array of Byte;
  TX501Names = array of TX501Name;
  TInternalCertificateRequest = packed record
    certificate_types:       TCertificateTypes;
    certificate_authorities: TX501Names;
  end;

  PCertificateRequest = ^TCertificateRequest;
  TCertificateRequest = packed record
    data:    packed array [0..$FFFFFF] of Byte;
  end;

  TPreMasterSecret = packed record
    version: TProtocolVersion;
    random:  packed array [0..45] of Byte;
  end;
  TPublicValueEncoding = (pveImplicit,pveExplicit);
  TClientDHPublic = record
    case TPublicValueEncoding of
      pveImplicit: ();
      pveExplicit: (dh_Yc: PMPInteger);
  end;

  TInternalClientKeyExchange = record
    rsaEncryptedPreMasterSecret: array of Byte;
    clientDHPublic: TClientDHPublic;
  end;

  PClientKeyExchange = ^TClientKeyExchange;
  TClientKeyExchange = packed record
    data:    packed array [0..$FFFFFF] of Byte;
  end;

  TInternalCertificateVerify = record
    signature: TSignature;
  end;

  PCertificateVerify = ^TCertificateVerify;
  TCertificateVerify = packed record
    data:    packed array [0..$FFFFFF] of Byte;
  end;

  TFinished = packed record
    verify_data: packed array [0..35] of Byte;
  end;


  PTLSHandshake = ^TTLSHandshake;
  TTLSHandshake = packed record
    msg_type: Byte;
    length:   array [0..2] of Byte;
    case Byte of
      hello_request:       ();
      client_hello:        (client_hello:        TClientHello);
      server_hello:        (server_hello:        TServerHello);
      certificate:         (certificate:         TCertificate);
      server_key_exchange: (server_key_exchange: TServerKeyExchange);
      certificate_request: (certificate_request: TCertificateRequest);
      server_hello_done:   ();
      certificate_verify:  (certificate_verify:  TCertificateVerify);
      client_key_exchange: (client_key_exchange: TClientKeyExchange);
      finished:            (finished:            TFinished);
  end;

  PSSL2ClientHello = ^TSSL2ClientHello;
  TSSL2CipherSpec = packed array [0..2] of Byte;
  TSSL2ClientHello = packed record
    msg_length:         packed array [0..1] of Byte;
    msg_type:           Byte;
    version:            TProtocolVersion;
    cipher_spec_length: Word;
    session_id_length:  Word;
    challenge_length:   Word;
    data:               packed array [0..$3FFFF] of Byte;
  end;

  TTLSHandshakes = array of PTLSHandshake;

  {$IFNDEF BCB}
  TGMTUnixTime = TLSUtils.TGMTUnixTime;
  {$ENDIF}

function CopyTLSHandshake(Src: PTLSHandshake; var Dst: PTLSHandshake): Integer;

function RecordLayerEncodeNext(Fragments: TStream;
                               const Version: TProtocolVersion;
                               contentType: Byte;
                               var Dst: PTLSPlainText): Integer;  
function RecordLayerDecodeNext(Src: PTLSPlainText; Len: Word;
                               const Version: TProtocolVersion;
                               var Error: Integer;
                               var contentType: Byte;
                               ChangeCipherSpec,
                               Alert,
                               Handshake,
                               ApplicationData: TStream): Integer;

function TLS_EncodeCipherSuite(CipherAlg:       TBulkCipherAlgorithm;
                               HashAlg:         THashAlgorithm;
                               KeyExchangeAlg:  TKeyExchangeAlgorithm;
                               SignAlg:         TSignatureAlgorithm;
                               var CipherSuite: TCipherSuite): Boolean;

function TLS_DecodeCipherSuite(const CipherSuite:  TCipherSuite;
                               var CipherAlg:      TBulkCipherAlgorithm;
                               var HashAlg:        THashAlgorithm;
                               var KeyExchangeAlg: TKeyExchangeAlgorithm;
                               var SignAlg:        TSignatureAlgorithm): Boolean;

function TLS_EncodeXCipherSuite(CipherAlg:       TBulkCipherAlgorithm;
                                HashAlg:         THashAlgorithm;
                                KeyExchangeAlg:  TKeyExchangeAlgorithm;
                                SignAlg:         TSignatureAlgorithm;
                                var CipherSuite: TCipherSuite): Boolean;

function TLS_DecodeXCipherSuite(const CipherSuite:  TCipherSuite;
                                var CipherAlg:      TBulkCipherAlgorithm;
                                var HashAlg:        THashAlgorithm;
                                var KeyExchangeAlg: TKeyExchangeAlgorithm;
                                var SignAlg:        TSignatureAlgorithm): Boolean;

{$IFDEF MSWINDOWS}
procedure UnixTimeToSystemTime(UnixTime: TGMTUnixTime; var SystemTime: TSystemTime);
function SystemTimeToUnixTime(const SystemTime: TSystemTime): TGMTUnixTime;
{$ENDIF}
function UnixTime: TGMTUnixTime;

procedure TLS_NewRandom(var Random: TRandom);

function TLS_EncodeClientHello(const Src: TInternalClientHello;
                               var Dst: PTLSHandshake): Integer;
function TLS_DecodeClientHello(const Src: PClientHello;
                               const Len: Integer;
                               var Dst: TInternalClientHello): Boolean;
function TLS_DecodeClientHelloSSL2(const Src: PSSL2ClientHello;
                                   const Len: Integer;
                                   var Dst: TInternalClientHello): Boolean;

function TLS_EncodeServerHello(const Src: TInternalServerHello;
                               var Dst: PTLSHandshake): Integer;
function TLS_DecodeServerHello(const Src: PServerHello;
                               const Len: Integer;
                               var Dst: TInternalServerHello): Boolean;

function TLS_EncodeCertificate(const RCList: TASN1Struct;
                               var Dst: PTLSHandshake): Integer;
function TLS_DecodeCertificateLength(Certificate: PCertificate;
                                     Len: Integer): Integer;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: TIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: TDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: TECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: IMPIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: IMPDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: IMPECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     var Dst: PTLSHandshake): Integer; overload;

function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     KEA: TKeyExchangeAlgorithm;
                                     const SignKey: TIFPublicKey;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     KEA: TKeyExchangeAlgorithm;
                                     const SignKey: TDLPublicKey;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     KEA: TKeyExchangeAlgorithm;
                                     const SignKey: TECPublicKey;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     KEA: TKeyExchangeAlgorithm;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;

function TLS_EncodeCertificateRequest(const Src: TInternalCertificateRequest;
                                      var Dst: PTLSHandshake): Integer;
function TLS_DecodeCertificateRequest(Src: PCertificateRequest;
                                      Len: Integer;
                                      var Dst: TInternalCertificateRequest): Boolean;

function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     const DLPriv: TDLPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     const ECPriv: TECPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     ECPriv: IMPECPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     IFPriv: TIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     IFPriv: IMPIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     DLPriv: TDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     ECPriv: TECPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     ECPriv: IMPECPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: TIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: TDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const ECDSASignKey: TECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     RSASignKey: IMPIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     DSSSignKey: IMPDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     ECDSASignKey: IMPECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function TLS_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: TIFPublicKey): Boolean; overload;
function TLS_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: TDLPublicKey): Boolean; overload;  
function TLS_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const ECDSASignKey: TECPublicKey): Boolean; overload;

procedure TLS_PRF(const Secret; SecretLen: Integer;
                  ALabel: string;
                  Seed: string;
                  var Key; KeyLen: Integer);

procedure TLS_KDF(var Params: TTLSSecurityParams;
                  var Key: TTLSKeys); overload;
procedure TLS_KDF(const Secret; SecretLen: Integer;
                  ALabel: string;
                  Seed: string;
                  HLen, KLen, EKLen, BLen: Integer;
                  Exportable: Boolean;
                  var Key: TTLSKeys); overload;

function TLS_Verify(const Params: TTLSSecurityParams;
                    const SSL2ClientHello: PSSL2ClientHello;
                    const Handshakes: TTLSHandshakes;
                    Entity: TConnectionEnd;
                    var Verify: PTLSHandshake): Integer;

implementation

uses
  {$IFDEF LINUX}DateUtils,{$ENDIF}
  SysUtils, MpArith, MpYarrow, ReadStrm, Pkix, MpEC_NISTCurves,
  MpEC_X9_62Curves, MpECArith;
                             
function CopyTLSHandshake(Src: PTLSHandshake; var Dst: PTLSHandshake): Integer;
begin
  Result := Src.length[0]*65536 +
            Src.length[1]*256 +
            Src.length[2] +
            4;
  ReallocMem(Dst,Result);
  Move(Src^,Dst^,Result);
end;

function RecordLayerEncodeNext(Fragments: TStream;
                               const Version: TProtocolVersion;
                               contentType: Byte;
                               var Dst: PTLSPlainText): Integer;
var
  Len: Integer;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length shl 8 +
           Dst^.length shr 8;
    if Len > 0 then
      ProtectClear(Dst^.fragment,Len);
  end;

  Len := Fragments.Size - Fragments.Position;
  if Len = 0 then begin
    if Assigned(Dst) then
      FreeMem(Dst);
    Dst := nil;
    Result := 0;
  end else begin
    if Len > 16384 then
      Len := 16384;
    if contentType = application_data then
      ReallocMem(Dst,Len + 5 + 80)
    else                             
      ReallocMem(Dst,Len + 5);
    Dst^.contentType := contentType;
    Dst^.version.major := Version.major;
    Dst^.version.minor := Version.minor;
    Dst^.length := Len shl 8 + Len shr 8;
    Result := Fragments.Read(Dst^.fragment,Len) + 5;
  end;
end;

function RecordLayerDecodeNext(Src: PTLSPlainText; Len: Word;
                               const Version: TProtocolVersion;
                               var Error: Integer;
                               var contentType: Byte;
                               ChangeCipherSpec,
                               Alert,
                               Handshake,
                               ApplicationData: TStream): Integer;
var
  ELen: Word;
  Strm: TStream;
begin
  Result := -1;
  Error := -1;
  contentType := Src.contentType;
  if not (contentType in [20..23]) then begin
    Error := decode_error;
    Result := 0;
    Exit;
  end;
  if (Src.version.major <> Version.major) or
     (Src.version.minor <> Version.minor) then begin
    Error := protocol_version;
    Exit;
  end;
  ELen := Src.length shr 8 + Src.length shl 8;
  if ELen > Len then begin
    Error := decode_error;
    Exit;
  end;
  case contentType of
    20: Strm := ChangeCipherSpec;
    21: Strm := Alert;
    22: Strm := Handshake;
    23: Strm := ApplicationData;
  else
    Strm := nil;
  end;
  if Assigned(Strm) then
    Result := Strm.Write(Src.fragment,ELen)
  else
    Error := unexpected_message;
end;

function TLS_EncodeCipherSuite(CipherAlg:       TBulkCipherAlgorithm;
                               HashAlg:         THashAlgorithm;
                               KeyExchangeAlg:  TKeyExchangeAlgorithm;
                               SignAlg:         TSignatureAlgorithm;
                               var CipherSuite: TCipherSuite): Boolean;
begin
  Result := True;
  CipherSuite[0] := 0;
 case KeyExchangeAlg of
    keaRSA_Export:
      if SignAlg = saRSA then begin
        case CipherAlg of
          bcaRC2_40:
            begin
              Result := HashAlg = haMD5;
              if Result then
                CipherSuite[1] := $06;
            end;
          bcaRC4_40:
            begin
              Result := HashAlg = haMD5;
              if Result then
                CipherSuite[1] := $03;
            end;
          bcaDES40:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $08;
            end;
        else
          Result := False;
        end;
      end else
        Result := False;
    keaRSA:
      if SignAlg = saRSA then begin
        case CipherAlg of
          bcaNull:
            begin
              case HashAlg of
                haSHA1: CipherSuite[1] := $02;
                haMD5:  CipherSuite[1] := $01;
              else
                Result := False;
              end;
            end;
          bcaRC4:
            begin
              case HashAlg of
                haSHA1: CipherSuite[1] := $05;
                haMD5:  CipherSuite[1] := $04;
              else
                Result := False;
              end;
            end;
          bcaDES:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $09;
            end;
          bca3DES:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $0A;
            end;
          bcaAES128:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $2F;
            end;
          bcaAES256:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $35;
            end;
        else
          Result := False;
        end;
      end else
        Result := False;
    keaDH:
      begin
        case CipherAlg of
          bcaRC4:
            begin
              Result := (HashAlg = haMD5) and (SignAlg = saAnon);
              if Result then
                CipherSuite[1] := $18;
            end;
          bcaRC4_40:
            begin
              Result := (HashAlg = haMD5) and (SignAlg = saAnon);
              if Result then
                CipherSuite[1] := $17;
            end;
          bcaDES:
            begin        
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA:  CipherSuite[1] := $0F;
                  saDSS:  CipherSuite[1] := $0C;
                  saAnon: CipherSuite[1] := $1A;
                else
                  Result := False;
                end;
              end;
            end;
          bca3DES:
            begin        
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA:  CipherSuite[1] := $10;
                  saDSS:  CipherSuite[1] := $0D;
                  saAnon: CipherSuite[1] := $1B;
                else
                  Result := False;
                end;
              end;
            end;
          bcaDES40:
            begin
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA:  CipherSuite[1] := $0E;
                  saDSS:  CipherSuite[1] := $0B;
                  saAnon: CipherSuite[1] := $19;
                else
                  Result := False;
                end;
              end;
            end;
          bcaAES128:
            begin
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA:  CipherSuite[1] := $31;
                  saDSS:  CipherSuite[1] := $30;
                  saAnon: CipherSuite[1] := $34;
                else
                  Result := False;
                end;
              end;
            end;
          bcaAES256:
            begin
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA:  CipherSuite[1] := $37;
                  saDSS:  CipherSuite[1] := $36;
                  saAnon: CipherSuite[1] := $3A;
                else
                  Result := False;
                end;
              end;
            end;
        else
          Result := False;
        end;
      end;
    keaDHE_Export:       
      begin
        case CipherAlg of  
          bcaDES40:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                case SignAlg of
                  saRSA: CipherSuite[1] := $14;
                  saDSS: CipherSuite[1] := $11;
                else
                  Result := False;
                end;
            end;
        else
          Result := False;
        end;
      end;
    keaDHE:
      begin
        case CipherAlg of
          bcaRC4:
            begin
              Result := (HashAlg = haSHA1) and (SignAlg = saDSS);
              if Result then
                CipherSuite[1] := $66;
            end;
          bcaDES:
            begin
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA: CipherSuite[1] := $15;
                  saDSS: CipherSuite[1] := $12;
                else
                  Result := False;
                end;
              end;
            end;
          bca3DES:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                case SignAlg of
                  saRSA: CipherSuite[1] := $16;
                  saDSS: CipherSuite[1] := $13;
                else
                  Result := False;
                end;
            end;
          bcaAES128:
            begin
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA: CipherSuite[1] := $33;
                  saDSS: CipherSuite[1] := $32;
                else
                  Result := False;
                end;
              end;
            end;
          bcaAES256:
            begin
              Result := HashAlg = haSHA1;
              if Result then begin
                case SignAlg of
                  saRSA: CipherSuite[1] := $39;
                  saDSS: CipherSuite[1] := $38;
                else
                  Result := False;
                end;
              end;
            end;
        else
          Result := False;
        end;
      end;
    keaRSA1024:
      begin
        case CipherAlg of
          bcaRC4_56:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $64;
            end;
          bcaDES56:
            begin
              Result := HashAlg = haSHA1;
              if Result then
                CipherSuite[1] := $62;
            end;
        else
          Result := False;
        end;
      end;
    keaDHE1024:
      begin
        case CipherAlg of
          bcaRC4_56:
            begin
              Result := (HashAlg = haSHA1) and (SignAlg = saDSS);
              if Result then
                CipherSuite[1] := $65;
            end;
          bcaDES56:
            begin
              Result := (HashAlg = haSHA1) and (SignAlg = saDSS);
              if Result then
                CipherSuite[1] := $63;
            end;
        else
          Result := False;
        end;
      end;
    keaECDH:
      begin
        Result := (HashAlg = haSHA1) and (SignAlg in [saAnon,saRSA,saECDSA]);
        if Result then case CipherAlg of
          bcaRC4:
            case SignAlg of
              saECDSA: CipherSuite[1] := $48;
              saRSA:   CipherSuite[1] := $4E;
              saAnon:  CipherSuite[1] := $56;
            end;
          bcaDES:
            case SignAlg of
              saECDSA: CipherSuite[1] := $49;
              saRSA:   CipherSuite[1] := $4F;
              saAnon:  CipherSuite[1] := $57;
            end;
          bca3DES:
            case SignAlg of
              saECDSA: CipherSuite[1] := $4A;
              saRSA:   CipherSuite[1] := $50;
              saAnon:  CipherSuite[1] := $58;
            end;
          bcaAES128:
            case SignAlg of
              saECDSA: CipherSuite[1] := $4B;
              saRSA:   CipherSuite[1] := $51;
              saAnon:  Result := False;
            end;
          bcaAES256:
            case SignAlg of
              saECDSA: CipherSuite[1] := $4C;
              saRSA:   CipherSuite[1] := $52;
              saAnon:  Result := False;
            end;
        else
          Result := False;
        end;
      end;
  else
    Result := False;
  end;
end;

function TLS_EncodeXCipherSuite(CipherAlg:       TBulkCipherAlgorithm;
                                HashAlg:         THashAlgorithm;
                                KeyExchangeAlg:  TKeyExchangeAlgorithm;
                                SignAlg:         TSignatureAlgorithm;
                                var CipherSuite: TCipherSuite): Boolean;
var
  X: Byte;
begin
  Result := True;
  case CipherAlg of
    bcaAES128:          X := 0;
    bcaAES192:          X := 1;
    bcaAES256:          X := 2;
    bcaTwoFish128:      X := 3;
    bcaTwoFish192:      X := 4;
    bcaTwoFish256:      X := 5;
    bcaBlowFish128:     X := 6;
    bcaAES128_CTR:      X := 7;
    bcaAES192_CTR:      X := 8;
    bcaAES256_CTR:      X := 9;
    bcaTwoFish128_CTR:  X := 10;
    bcaTwoFish192_CTR:  X := 11;
    bcaTwoFish256_CTR:  X := 12;
    bcaBlowFish128_CTR: X := 13;
  else
    X := 0;
    Result := False;
  end;
  case HashAlg of
    haSHA1:             X := X*14;
    {$IFDEF RIPEMD160}
    haRipeMD160:        X := X*14 + 1;
    {$ENDIF}
    {$IFDEF SHA256}
    haSHA256:
      if ((X mod 7) mod 3) = 0 then
        X := X*14 + 2
      else
        Result := False;
    {$ENDIF}            
    {$IFDEF SHA512}
    haSHA384:
      if ((X mod 7) mod 3) = 1 then
        X := X*14 + 2
      else
        Result := False;
    haSHA512:
      if ((X mod 7) mod 3) = 2 then
        X := X*14 + 2
      else
        Result := False;
    {$ENDIF}
  else
    Result := False;
  end;
  case SignAlg of
    saRSA:   X := X*3;
    saDSS:   X := X*3 + 1;
    saECDSA: X := X*3 + 2;
  else
    Result := False;
  end;
  case KeyExchangeAlg of
    keaDHE:   X := X*3;
    keaECDHE: X := X*3 + 1;
  else
    Result := False;
  end;
  CipherSuite[0] := $FF;
  CipherSuite[1] := X;
end;

function TLS_DecodeCipherSuite(const CipherSuite:  TCipherSuite;
                               var CipherAlg:      TBulkCipherAlgorithm;
                               var HashAlg:        THashAlgorithm;
                               var KeyExchangeAlg: TKeyExchangeAlgorithm;
                               var SignAlg:        TSignatureAlgorithm): Boolean;
begin
  Result := True;
  if CipherSuite[0] = 0 then begin
    case CipherSuite[1] of
      0:        KeyExchangeAlg := keaNull;
      $01..$02,
      $04..$05,
      $07,
      $09..$0A,
      $2F,$35:  KeyExchangeAlg := keaRSA;
      $03,$06,
      $08:      KeyExchangeAlg := keaRSA_Export;
      $0B..$10,
      $17..$1B,
      $30..$31,
      $34,
      $36..$37,
      $3A:      KeyExchangeAlg := keaDH;
      $12..$13,
      $15..$16,
      $32..$33,
      $38..$39,
      $66:      KeyExchangeAlg := keaDHE;
      $11,$14:  KeyExchangeAlg := keaDHE_Export;
      $63,$65:  KeyExchangeAlg := keaDHE1024;
      $62,$64:  KeyExchangeAlg := keaRSA1024;
      $48..$4C,
      $4E..$52,
      $56..$58: KeyExchangeAlg := keaECDH;
    else
      Result := False;
    end;
    if Result then begin
      case CipherSuite[1] of
        0:        SignAlg := saAnon;
        $01..$0A,
        $0E..$10,
        $14..$16,
        $2F,$31,
        $33,$35,
        $37,$39,
        $4D..$54,
        $62,$64:  SignAlg := saRSA;
        $0B..$0D,
        $11..$13,
        $30,$32,
        $36,$38,
        $63,$65,
        $66:      SignAlg := saDSS;
        $17..$1B,
        $34,$3A,
        $55..$5A: SignAlg := saAnon;
        $47..$4C: SignAlg := saECDSA;
      else
        Result := False;
      end;
      if Result then begin
        case CipherSuite[1] of
          $01,$03,
          $04,$06,
          $17,$18: HashAlg := haMD5;
        else
          HashAlg := haSHA1;
        end;
        case CipherSuite[1] of
          $00..$02: CipherAlg := bcaNull;
          $06:      CipherAlg := bcaRC2_40;
          $03,$17:  CipherAlg := bcaRC4_40;
          $04..$05,
          $48,$4E,
          $56,
          $18,$66:  CipherAlg := bcaRC4;
          $08,$0B,
          $0E,$11,
          $14,$19:  CipherAlg := bcaDES40;
          $09,$0C,
          $0F,$12,
          $49,$4F,
          $57,
          $15,$1A:  CipherAlg := bcaDES;
          $62..$63: CipherAlg := bcaDES56;
          $0A,$0D,
          $10,$13,
          $16,$1B,
          $4A,$50,
          $58:      CipherAlg := bca3DES;
          $2F..$34,
          $4B,$51:  CipherAlg := bcaAES128;
          $35..$3A,
          $4C,$52:  CipherAlg := bcaAES256;
          $64..$65: CipherAlg := bcaRC4_56;
        else
          Result := False;
        end;
      end;
    end;
  end else
    Result := False;
end;

function TLS_DecodeXCipherSuite(const CipherSuite:  TCipherSuite;
                                var CipherAlg:      TBulkCipherAlgorithm;
                                var HashAlg:        THashAlgorithm;
                                var KeyExchangeAlg: TKeyExchangeAlgorithm;
                                var SignAlg:        TSignatureAlgorithm): Boolean;
var
  X: Byte;
begin
  Result := (CipherSuite[0] = 255) and (CipherSuite[1] < 2 * 3 * 3 * 2 * 7);
  if Result then begin
    X := CipherSuite[1];
    // Key agreement
    case X and 1 of
      0: KeyExchangeAlg := keaDHE;
      1: KeyExchangeAlg := keaECDHE;
    end;
    X := X shr 1;
    // Signature
    case X mod 3 of
      0: SignAlg := saRSA;
      1: SignAlg := saDSS;
      2: SignAlg := saECDSA;
    end;
    X := X div 3;
    // Hash
    case X mod 3 of
      0: HashAlg := haSHA1;
      1: {$IFDEF RIPEMD160}HashAlg := haRipeMD160;{$ELSE}Result := False;{$ENDIF}
      2: {$IFDEF SHA256}   HashAlg := haSHA256;{$ELSE}HashAlg := haReserved3;{$ENDIF}
    end;
    X := X div 3;
    // Cipher
    if X < 7 then
      case X mod 7 of
        0: CipherAlg := bcaAES128;
        1: CipherAlg := bcaAES192;
        2: CipherAlg := bcaAES256;
        3: CipherAlg := bcaTwoFish128;
        4: CipherAlg := bcaTwoFish192;
        5: CipherAlg := bcaTwoFish256;
        6: CipherAlg := bcaBlowFish128;
      end
    else
      case X mod 7 of
        0: CipherAlg := bcaAES128_CTR;
        1: CipherAlg := bcaAES192_CTR;
        2: CipherAlg := bcaAES256_CTR;
        3: CipherAlg := bcaTwoFish128_CTR;
        4: CipherAlg := bcaTwoFish192_CTR;
        5: CipherAlg := bcaTwoFish256_CTR;
        6: CipherAlg := bcaBlowFish128_CTR;
      end;

    if HashAlg = {$IFDEF SHA256}haSHA256{$ELSE}haReserved3{$ENDIF} then
      case X mod 7 of
        0,3,6: {$IFDEF SHA256}HashAlg := haSHA256;{$ELSE}Result := False;{$ENDIF}
        1,4:   {$IFDEF SHA512}HashAlg := haSHA384;{$ELSE}Result := False;{$ENDIF}
        2,5:   {$IFDEF SHA512}HashAlg := haSHA512;{$ELSE}Result := False;{$ENDIF}
      end;
  end else if CipherSuite[0] = 255 then begin
    Result := True;
    CipherAlg := bcaHelix;
    case CipherSuite[1] of
      252:
        begin
          KeyExchangeAlg := keaDHE;
          SignAlg := saRSA;
        end;
      253:
        begin
          KeyExchangeAlg := keaDHE;
          SignAlg := saDSS;
        end;
      254:
        begin
          KeyExchangeAlg := keaECDHE;
          SignAlg := saECDSA;
        end;
    else
      Result := False;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure UnixTimeToSystemTime(UnixTime: TGMTUnixTime; var SystemTime: TSystemTime);
begin
  TLSUtils.UnixTimeToSystemTime(UnixTime,SystemTime);
end;

function SystemTimeToUnixTime(const SystemTime: TSystemTime): TGMTUnixTime;
begin
  Result := TLSUtils.SystemTimeToUnixTime(SystemTime);
end;
{$ENDIF}

function UnixTime: TGMTUnixTime;
begin
  Result := TLSUtils.UnixTime;
end;

procedure TLS_NewRandom(var Random: TRandom);
var
  LRandom: TLSUtils.TRandom absolute Random;
begin
  TLSUtils.TLS_NewRandom(LRandom);
end;

function TLS_EncodeClientHello(const Src: TInternalClientHello;
                               var Dst: PTLSHandshake): Integer;
var
  Len, L: Integer;
  W: Word;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  Len := 4 +
         SizeOf(Src.client_version) + SizeOf(Src.random) +
         1 + Length(Src.session_id) +
         2 + 2*Src.cipher_suite_count +
         1 + Src.compression_count;
  ReallocMem(Dst,Len);
  Result := Len;

  Dst^.msg_type := client_hello;
  Dst^.length[0] := (Len - 4) shr 16;
  Dst^.length[1] := (Len - 4) shr 8 and $FF;
  Dst^.length[2] := (Len - 4) and $FF;

  Dst^.client_hello.client_version.major := 3;
  Dst^.client_hello.client_version.minor := 1;
  Move(Src.random,Dst^.client_hello.random,SizeOf(Src.random));

  L := 1 + Length(Src.session_id);
  Move(Src.session_id,Dst^.client_hello.Data,L);

  W := 2*Src.cipher_suite_count;
  W := (W shr 8) or (W shl 8);
  Move(W,Dst^.client_hello.Data[L],2);

  L := L + 2;
  Move(Src.cipher_suites[0],Dst^.client_hello.Data[L],2*Src.cipher_suite_count);

  L := L + 2*Src.cipher_suite_count;
  Dst^.client_hello.Data[L] := Src.compression_count;
  Move(Src.compression_methods[0],Dst^.client_hello.Data[L+1],Src.compression_count);
end;

function TLS_DecodeClientHello(const Src: PClientHello;
                               const Len: Integer;
                               var Dst: TInternalClientHello): Boolean;
var
  L: Integer;
  W: Word;
begin
  Result := (Src.client_version.major >= 3);
  if not Result then Exit;
  Dst.client_version := Src.client_version;

  Dst.random := Src.random;

  Result := Src.Data[0] <= 32;
  if not Result then
    Exit;

  Move(Src.Data,Dst.Session_id,Src.Data[0] + 1);

  L := 1 + Length(Dst.session_id);
  Move(Src^.Data[L],W,2);
  W := (W shr 8) or (W shl 8);

  Result := W < (Len + 2 - L - 32 - 2);
  if not Result then
    Exit;

  Dst.cipher_suite_count := W shr 1;
  SetLength(Dst.cipher_suites,Dst.cipher_suite_count);

  L := L + 2;
  Move(Src^.Data[L],Dst.cipher_suites[0],2*Dst.cipher_suite_count);

  L := L + 2*Dst.cipher_suite_count;

  Result := Src^.Data[L] <= (Len + 1 - L - 32 - 2);
  if not Result then
    Exit;

  Dst.compression_count := Src^.Data[L];
  SetLength(Dst.compression_methods,Dst.compression_count);
  Move(Src^.Data[L+1],Dst.compression_methods[0],Dst.compression_count);
end;

function TLS_DecodeClientHelloSSL2(const Src: PSSL2ClientHello;
                                   const Len: Integer;
                                   var Dst: TInternalClientHello): Boolean; 
var
  L, CSL, ChL, I, J: Integer;
  W: Word;
begin
  Result := Len >= 11;
  if not Result then Exit;
  Result := (Src.msg_length[0] and $80) > 0;
  if not Result then Exit;
  L := (Src.msg_length[0] and $7F) shl 8 + Src.msg_length[1];
  Result := L + 2 <= Len;
  if not Result then Exit;

  W := Src.session_id_length;
  Result := W = 0;
  if not Result then Exit;
  Dst.session_id := '';

  Result := Src.msg_type = 1;
  if not Result then Exit;

  Result := (Src.version.major >= 3);
  if not Result then Exit;
  Dst.client_version := Src.version;

  W := Src.cipher_spec_length;
  CSL := (W shr 8) + Word(W shl 8);
  W := Src.challenge_length;
  ChL := (W shr 8) + Word(W shl 8);
  Result := (L = (9 + CSL + ChL)) and
            (ChL >= 16);
  if not Result then Exit;

  Dst.cipher_suite_count := CSL div 3;
  SetLength(Dst.cipher_suites,Dst.cipher_suite_count);
  J := 0;
  for I := 0 to Dst.cipher_suite_count - 1 do
    if Src.data[I*3] = 0 then begin
      Dst.cipher_suites[J,0] := Src.data[I*3+1];
      Dst.cipher_suites[J,1] := Src.data[I*3+2];
      Inc(J);
    end;
  Dst.cipher_suite_count := J;
  SetLength(Dst.cipher_suites,J);

  Dst.compression_count := 1;
  SetLength(Dst.compression_methods,1);
  Dst.compression_methods[0] := cmNull;

  if ChL > 32 then
    Move(Src.data[CSL + ChL - 32],Dst.random,32)
  else begin
    if ChL < 32 then
      FillChar(Dst.random,32-ChL,0);
    Move(Src.data[CSL],Dst.random.AsString[32 - ChL],ChL);
  end;
end;

function TLS_EncodeServerHello(const Src: TInternalServerHello;
                               var Dst: PTLSHandshake): Integer;
var
  Len, L: Integer;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shl 16 + Dst^.length[1] shl 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  Len := 37 + 1 + Length(Src.session_id);
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := server_hello;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Dst^.server_hello.server_version := Src.server_version;
  Dst^.server_hello.random := Src.random;
  L := 1 + Length(Src.session_id);
  Move(Src.session_id,Dst^.server_hello.data,L);
  Move(Src.cipher_suite,Dst^.server_hello.data[L],2);
  Move(Src.compression_method,Dst^.server_hello.data[L+2],1);
end;

function TLS_DecodeServerHello(const Src: PServerHello;
                               const Len: Integer;
                               var Dst: TInternalServerHello): Boolean;
var
  L: Integer;
begin
  Result := (Src.server_version.major >= 3);
  if not Result then Exit;
  Dst.server_version := Src.server_version;

  Move(Src.random,Dst.random,SizeOf(Src.random));

  L := Src.Data[0] + 1;
  Result := (L <= 33) and (L <= Len - 33);
  if not Result then
    Exit;
  Move(Src^.Data[0],Dst.session_id[0],L);
  Move(Src^.Data[L],Dst.cipher_suite,2);
  Move(Src^.Data[L+2],Dst.compression_method,1);
end;

function TLS_EncodeCertificate(const RCList: TASN1Struct;
                               var Dst: PTLSHandshake): Integer;
var
  MS, MSC: TSecureMemoryStream;
  C: PASN1Struct;
  I, ELen, Len: Integer;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 +
           Dst^.length[1] shr 8 +
           Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  MS := TSecureMemoryStream.Create;
  try
    if Assigned(RCList) then begin
      MSC := TSecureMemoryStream.Create;
      try
        RCList.ResetStreaming;
        for I := 0 to RCList.ItemCount - 1 do begin
          C := RCList.Items[I];
          C^.SaveToStream(MSC,fmtDER);
          Len := MSC.Size;
          ELen := TlsUtils.ByteSwap(LongWord(Len)) shr 8;
          MS.Write(ELen,3);
          MS.CopyFrom(MSC,0);
          MSC.Size := 0;
        end;
      finally
        MSC.Free;
      end;
    end;
    Len := 3 + MS.Size;
    ELen := TlsUtils.ByteSwap(LongWord(Len-3)) shr 8;

    ReallocMem(Dst,Len + 4);
    Result := Len + 4;

    Dst^.msg_type := certificate;
    Dst^.length[0] := Len shr 16;
    Dst^.length[1] := (Len shr 8) and $FF;
    Dst^.length[2] := Len and $FF;

    Move(ELen,Dst^.certificate,3);
    MS.Position := 0;
    MS.Read(Dst^.certificate.certificate_list[3],MS.Size);
  finally
    MS.Free;
  end;
end;

function TLS_DecodeCertificateLength(Certificate: PCertificate;
                                     Len: Integer): Integer;
var
  ELen: LongWord;
begin
  if Len < 3 then
    Result := -1
  else begin
    ELen := 0;
    Move(Certificate^,ELen,3);
    ELen := TlsUtils.ByteSwap(ELen) shr 8;
    if ELen <= Cardinal(Len - 3) then
      Result := ELen
    else
      Result := -1;
  end;
end;

function InternalEncodeServerKeaRSA(var Src: TInternalServerKeyExchange;
                                    SignLen: Integer;
                                    var L: Integer;
                                    var Dst: PTLSHandshake): Integer;
var
  Len: Integer;
  W: Word;
begin
  Len := (MPMSB(Src.rsaParams.rsa_modulus) + 7) shr 3 + 2 +
         (MPMSB(Src.rsaParams.rsa_exponent) + 7) shr 3 + 2;
  Len := Len + SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len;

  W := (MPMSB(Src.rsaParams.rsa_modulus) + 7) shr 3;
  Dst^.server_key_exchange.data[0] := Byte(W shr 8);
  Dst^.server_key_exchange.data[1] := Byte(W);
  UMPIntToBase256(Src.rsaParams.rsa_modulus,Dst^.server_key_exchange.Data[2],W);
  L := 2 + W;

  W := (MPMSB(Src.rsaParams.rsa_exponent) + 7) shr 3;
  Dst^.server_key_exchange.data[L] := Byte(W shr 8);
  Dst^.server_key_exchange.data[L + 1] := Byte(W);
  UMPIntToBase256(Src.rsaParams.rsa_exponent,Dst^.server_key_exchange.Data[L + 2],W);
  L := L + W + 2;
end;

function InternalEncodeServerKeaDHE(var Src: TInternalServerKeyExchange;
                                    SignLen: Integer;
                                    var L: Integer;
                                    var Dst: PTLSHandshake): Integer;
var
  Len: Integer;
  W: Word;
begin
  Len := (MPMSB(Src.dhParams.dh_p) + 7) shr 3 + 2 +
         (MPMSB(Src.dhParams.dh_g) + 7) shr 3 + 2 +
         (MPMSB(Src.dhParams.dh_Ys) + 7) shr 3 + 2;
  Len := Len + SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len;

  W := (MPMSB(Src.dhParams.dh_p) + 7) shr 3;
  Dst^.server_key_exchange.data[0] := Byte(W shr 8);
  Dst^.server_key_exchange.data[1] := Byte(W);
  UMPIntToBase256(Src.dhParams.dh_p,Dst^.server_key_exchange.Data[2],W);
  L := 2 + W;

  W := (MPMSB(Src.dhParams.dh_g) + 7) shr 3;
  Dst^.server_key_exchange.data[L] := Byte(W shr 8);
  Dst^.server_key_exchange.data[L + 1] := Byte(W);
  UMPIntToBase256(Src.dhParams.dh_g,Dst^.server_key_exchange.Data[L + 2],W);
  L := L + W + 2;

  W := (MPMSB(Src.dhParams.dh_Ys) + 7) shr 3;
  Dst^.server_key_exchange.data[L] := Byte(W shr 8);
  Dst^.server_key_exchange.data[L + 1] := Byte(W);
  UMPIntToBase256(Src.dhParams.dh_Ys,Dst^.server_key_exchange.Data[L + 2],W);
  L := L + W + 2;
end;

function InternalEncodeServerKeaECDHE(var Src: TInternalServerKeyExchange;
                                      SignLen: Integer;
                                      var L: Integer;
                                      var Dst: PTLSHandshake): Integer;
var
  Len: Integer;
  W: Word;
  G, Publ: ShortString;
begin
  if Src.ecdhParams.curve_params.curve_type = ctNamedCurve then begin
    case Src.ecdhParams.curve_params.curveName of
      19: ECurveP192(Src.ecdhParams.curve_params.primeCurve);
      21: ECurveP224(Src.ecdhParams.curve_params.primeCurve);
      23: ECurveP256(Src.ecdhParams.curve_params.primeCurve);
      24: ECurveP384(Src.ecdhParams.curve_params.primeCurve);
      25: ECurveP521(Src.ecdhParams.curve_params.primeCurve);
    end;
  end;
  with TASN1Struct.Create do try
    Tag := V_ASN1_OCTET_STRING;
    EditContent(Src.ecdhParams.curve_params.primeCurve.G,
                Src.ecdhParams.curve_params.primeCurve,
                cUncompressed);
    G := ContentAsOctetString;
  finally
    Free;
  end;
  with TASN1Struct.Create do try
    Tag := V_ASN1_OCTET_STRING;
    EditContent(Src.ecdhParams.public_key,
                Src.ecdhParams.curve_params.primeCurve,
                cUncompressed);
    Publ:= ContentAsOctetString;
  finally
    Free;
  end;
  if Src.ecdhParams.curve_params.curve_type = ctExplicitPrime then begin
    Len := Length(Publ) + 1 +
           1 +
           (MPMSB(Src.ecdhParams.curve_params.primeCurve.Q) + 7) shr 3 + 1 +
           (MPMSB(Src.ecdhParams.curve_params.primeCurve.A) + 7) shr 3 + 1 +
           (MPMSB(Src.ecdhParams.curve_params.primeCurve.B) + 7) shr 3 + 1 +
           1 +
           Length(G) + 1 +
           (MPMSB(Src.ecdhParams.curve_params.primeCurve.R) + 7) shr 3 + 1 +
           (MPMSB(Src.ecdhParams.curve_params.primeCurve.K) + 7) shr 3 + 1;

    Len := Len + SignLen + 2;
    ReallocMem(Dst,Len + 4);

    Dst^.server_key_exchange.data[0] := 1;

    W := (MPMSB(Src.ecdhParams.curve_params.primeCurve.Q) + 7) shr 3;
    Dst^.server_key_exchange.data[1] := Byte(W);
    UMPIntToBase256(Src.ecdhParams.curve_params.primeCurve.Q,
                    Dst^.server_key_exchange.Data[1],W);
    L := 2 + W;

    W := (MPMSB(Src.ecdhParams.curve_params.primeCurve.A) + 7) shr 3;
    Dst^.server_key_exchange.data[L] := Byte(W);
    UMPIntToBase256(Src.ecdhParams.curve_params.primeCurve.A,
                    Dst^.server_key_exchange.Data[L + 1],W);
    L := L + W + 1;

    W := (MPMSB(Src.ecdhParams.curve_params.primeCurve.B) + 7) shr 3;
    Dst^.server_key_exchange.data[L] := Byte(W);
    UMPIntToBase256(Src.ecdhParams.curve_params.primeCurve.B,
                    Dst^.server_key_exchange.Data[L + 1],W);
    L := L + W + 1;

    Dst^.server_key_exchange.data[L] := 0;
    L := L + 1;

    Move(G,Dst^.server_key_exchange.Data[L],Length(G) + 1);
    L := L + Length(G) + 1;

    W := (MPMSB(Src.ecdhParams.curve_params.primeCurve.R) + 7) shr 3;
    Dst^.server_key_exchange.data[L] := Byte(W);
    UMPIntToBase256(Src.ecdhParams.curve_params.primeCurve.R,
                    Dst^.server_key_exchange.Data[L + 1],W);
    L := L + W + 1;

    W := (MPMSB(Src.ecdhParams.curve_params.primeCurve.K) + 7) shr 3;
    Dst^.server_key_exchange.data[L] := Byte(W);
    UMPIntToBase256(Src.ecdhParams.curve_params.primeCurve.K,
                    Dst^.server_key_exchange.Data[L + 1],W);
    L := L + W + 1;

  end else if Src.ecdhParams.curve_params.curve_type = ctNamedCurve then begin
    Len := Length(Publ) + 3;

    Len := Len + SignLen + 2;
    ReallocMem(Dst,Len + 4);

    Dst^.server_key_exchange.data[0] := 3;
    Dst^.server_key_exchange.data[1] := Src.ecdhParams.curve_params.curveName;
    L := 2;
  end else begin
    L := 0;
    Len := 0;
  end;
  Result := Len;


  Move(Publ,Dst^.server_key_exchange.Data[L],Length(Publ) + 1);
  L := L + Length(Publ) + 1;
end;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: TIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  SignBSize: Integer;
  SignLen, L, Len: Integer;
  H: THash;
begin
  Assert(Src.Signed.input.SA = saRSA);

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  case SignKey.ReprType of
    0: SignBSize := MPMSB(SignKey.zN);
    1: SignBSize := MPMSB(SignKey.oP) + MPMSB(SignKey.oQ);
    2: SignBSize := MPMSB(SignKey.tP) + MPMSB(SignKey.tQ);
  else
    SignBSize := 0;
  end;
  SignLen := (SignBSize + 7) shr 3;
  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,SignLen,L,Dst);
    keaDHE,
    keaDHE_Export,
    keaDHE1024:
      Len := InternalEncodeServerKeaDHE(Src,SignLen,L,Dst);
    keaECDHE:
      Len := InternalEncodeServerKeaECDHE(Src,SignLen,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  H := TMD5.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.rsaInput.md5_hash);
  finally
    H.Free;
  end;
  H := TSHA1.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.rsaInput.sha_hash);
  finally
    H.Free;
  end;

  Src.Signed.signature := TSecretKey.Create('');
  Src.Signed.signature.SetLength(SignLen);
  IFSSASignatureGeneration(SignKey,
                           Src.Signed.input.rsaInput,36,
                           haMD5,haSHA1, // <- These parameters are not used
                           seEMSA_TLS,
                           False,
                           Src.Signed.signature.KeyBytes[0],SignLen);
  Dst^.server_key_exchange.Data[L] := Byte(SignLen shr 8);
  Dst^.server_key_exchange.Data[L+1] := Byte(SignLen);
  Move(Src.Signed.signature.KeyBytes[0],Dst^.server_key_exchange.Data[L+2],SignLen);
end;              

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: IMPIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  SignLen, ActSignLen, L, Len: Integer;
  H: THash;
begin
  Assert(Src.Signed.input.SA = saRSA);

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SignLen := SignKey.SignatureLength;
  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,SignLen,L,Dst);
    keaDHE,
    keaDHE_Export,
    keaDHE1024:
      Len := InternalEncodeServerKeaDHE(Src,SignLen,L,Dst);
    keaECDHE:
      Len := InternalEncodeServerKeaECDHE(Src,SignLen,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  H := TMD5.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.rsaInput.md5_hash);
  finally
    H.Free;
  end;
  H := TSHA1.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.rsaInput.sha_hash);
  finally
    H.Free;
  end;

  Src.Signed.signature := TSecretKey.Create('');
  Src.Signed.signature.SetLength(SignLen);
  ActSignLen := SignKey.SignDig(Src.Signed.input.rsaInput,36,Src.Signed.signature.KeyBytes[0],SignLen);
  if ActSignLen < SignLen then begin
    Move(Src.Signed.signature.KeyBytes[0],
         Src.Signed.signature.KeyBytes[SignLen - ActSignLen],
         ActSignLen);
    FillChar(Src.Signed.signature.KeyBytes[0],SignLen - ActSignLen,0);
  end;
  Dst^.server_key_exchange.Data[L] := Byte(SignLen shr 8);
  Dst^.server_key_exchange.Data[L+1] := Byte(SignLen);
  Move(Src.Signed.signature.KeyBytes[0],Dst^.server_key_exchange.Data[L+2],SignLen);
end;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: TDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  QBSize: Integer;
  SignLen, L, Len: Integer;
  H: THash;
begin
  Assert(Src.Signed.input.SA = saDSS);

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  QBSize := MPMSB(SignKey.Params.Q);
  SignLen := (QBSize + 7) shr 3;
  if SignLen < 128 then
    SignLen := 2*SignLen + 4
  else
    SignLen := 2*SignLen + 8;
  if SignLen < 128 then
    SignLen := SignLen + 2
  else
    SignLen := SignLen + 4;

  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,SignLen,L,Dst);
    keaDHE,
    keaDHE_Export,
    keaDHE1024:
      Len := InternalEncodeServerKeaDHE(Src,SignLen,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  H := TSHA1.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.sha_hash);
  finally
    H.Free;
  end;

  Src.Signed.signature := TSecretKey.Create('');
  Src.Signed.signature.SetLength(SignLen);
  DLSSASignatureGeneration(SignKey,
                           Src.Signed.input.sha_hash,20,
                           haSHA1,False, // <- These parameters are not used
                           True,
                           Src.Signed.signature.KeyBytes[0],SignLen);
  Dst^.server_key_exchange.Data[L] := Byte(SignLen shr 8);
  Dst^.server_key_exchange.Data[L+1] := Byte(SignLen);
  Move(Src.Signed.signature.KeyBytes[0],Dst^.server_key_exchange.Data[L+2],SignLen);
end;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: IMPDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer;
var
  SignLen, L, Len: Integer;
  H: THash;
begin
  Assert(Src.Signed.input.SA = saDSS);

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SignKey.DEREncodeSignature := True;
  SignKey.SignatureDigestAlg := haSHA1;
  SignLen := SignKey.SignatureLength;

  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,SignLen,L,Dst);
    keaDHE,
    keaDHE_Export,
    keaDHE1024:
      Len := InternalEncodeServerKeaDHE(Src,SignLen,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  H := TSHA1.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.sha_hash);
  finally
    H.Free;
  end;

  Src.Signed.signature := TSecretKey.Create('');
  Src.Signed.signature.SetLength(SignLen);
  SignKey.SignDig(Src.Signed.input.sha_hash,20,Src.Signed.signature.KeyBytes[0],SignLen);
  Dst^.server_key_exchange.Data[L] := Byte(SignLen shr 8);
  Dst^.server_key_exchange.Data[L+1] := Byte(SignLen);
  Move(Src.Signed.signature.KeyBytes[0],Dst^.server_key_exchange.Data[L+2],SignLen);
end;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: TECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  QBSize: Integer;
  SignLen, L, Len: Integer;
  H: THash;
begin
  Assert(Src.Signed.input.SA = saECDSA);

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  QBSize := MPMSB(SignKey.Params.Q);
  SignLen := (QBSize + 7) shr 3;
  if SignLen < 128 then
    SignLen := 2*SignLen + 4
  else
    SignLen := 2*SignLen + 8;
  if SignLen < 128 then
    SignLen := SignLen + 2
  else
    SignLen := SignLen + 4;

  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,SignLen,L,Dst);
    keaDHE,
    keaDHE_Export,
    keaDHE1024:
      Len := InternalEncodeServerKeaDHE(Src,SignLen,L,Dst);
    keaECDH, keaECDHE:
      Len := InternalEncodeServerKeaECDHE(Src,SignLen,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  H := TSHA1.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.sha2_hash);
  finally
    H.Free;
  end;

  Src.Signed.signature := TSecretKey.Create('');
  Src.Signed.signature.SetLength(SignLen);
  ECSSASignatureGeneration(SignKey,
                           Src.Signed.input.sha2_hash,20,
                           haSHA1,False, // <- These parameters are not used
                           True,
                           Src.Signed.signature.KeyBytes[0],SignLen);
  Dst^.server_key_exchange.Data[L] := Byte(SignLen shr 8);
  Dst^.server_key_exchange.Data[L+1] := Byte(SignLen);
  Move(Src.Signed.signature.KeyBytes[0],Dst^.server_key_exchange.Data[L+2],SignLen);
end;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     const SignKey: IMPECPrivateKey;
                                     var Dst: PTLSHandshake): Integer;
var
  SignLen, L, Len: Integer;
  HC: THashClass;
  H: THash;
begin
  Assert(Src.Signed.input.SA = saECDSA);

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SignKey.DEREncodeSignature := True;
  SignKey.SignatureDigestAlg := haSHA1;
  SignLen := SignKey.SignatureLength;

  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,SignLen,L,Dst);
    keaDHE,
    keaDHE_Export,
    keaDHE1024:
      Len := InternalEncodeServerKeaDHE(Src,SignLen,L,Dst);
    keaECDH, keaECDHE:
      Len := InternalEncodeServerKeaECDHE(Src,SignLen,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  HC := FindHashClass(SignKey.SignatureDigestAlg);
  H := HC.Create(ClientRandom,32);
  try
    H.HashData(ServerRandom,32);
    H.HashData(Dst^.server_key_exchange.Data[0],L);
    H.Done(@Src.Signed.input.sha2_hash);
  finally
    H.Free;
  end;

  Src.Signed.signature := TSecretKey.Create('');
  Src.Signed.signature.SetLength(SignLen);
  SignKey.SignDig(Src.Signed.input.sha2_hash,HC.DigestSize,Src.Signed.signature.KeyBytes[0],SignLen);
  Dst^.server_key_exchange.Data[L] := Byte(SignLen shr 8);
  Dst^.server_key_exchange.Data[L+1] := Byte(SignLen);
  Move(Src.Signed.signature.KeyBytes[0],Dst^.server_key_exchange.Data[L+2],SignLen);
end;

function TLS_EncodeServerKeyExchange(var Src: TInternalServerKeyExchange;
                                     var Dst: PTLSHandshake): Integer;
var
  L, Len: Integer;
begin
//  Assert((Src.Signed.input.SA = saAnon) or
//         (Src.KEA = keaECDH));

  if Assigned(Dst) then begin
    Len := Dst^.length[0] shl 16 + Dst^.length[1] shl 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  case Src.KEA of
    keaRSA,
    keaRSA_Export,
    keaRSA1024:
      Len := InternalEncodeServerKeaRSA(Src,0,L,Dst);
    keaDH:
      Len := InternalEncodeServerKeaDHE(Src,0,L,Dst);
    keaECDH:
      Len := InternalEncodeServerKeaECDHE(Src,0,L,Dst);
  else
    L := 0;
    Len := 0;
  end;

  Result := Len + 4;

  Dst^.msg_type := server_key_exchange;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Dst^.server_key_exchange.data[L] := 0;
  Dst^.server_key_exchange.data[L+1] := 0;
end;

function InternalDecodeServerKeaCheck(Src: PServerKeyExchange;
                                      Len: Integer;
                                      KEA: TKeyExchangeAlgorithm;
                                      var L: Integer;
                                      var Dst: TInternalServerKeyExchange): Boolean;
var
  ELen: Word;
  B: Byte;
  SS: TStringStream;
begin
  Result := Len >= 2;
  if not Result then Exit;

  L := 0;
  if KEA in [keaRSA,keaRSA_Export,keaRSA1024] then begin
    Dst.KEA := KEA;
    ELen := Src.data[0] shl 8 + Src.data[1];
    Result := Len >= ELen + 2;
    if Result then begin
      L := 2 + ELen;
      ELen := Src.data[L] shl 8 + Src.data[L+1];
      Result := Len >= ELen + L + 2;
      if Result then
        L := L + 2 + ELen;
    end;
  end else if KEA in [keaDH,keaDHE,keaDHE_Export,keaDHE1024] then begin
    Dst.KEA := KEA;
    ELen := Src.data[0] shl 8 + Src.data[1];
    Result := Len >= ELen + 2;
    if Result then begin
      L := 2 + ELen;
      ELen := Src.data[L] shl 8 + Src.data[L+1];
      Result := Len >= ELen + L + 2;
      if Result then begin
        L := L + 2 + ELen;
        ELen := Src.data[L] shl 8 + Src.data[L+1];
        Result := Len >= ELen + L + 2;
        if Result then
          L := L + 2 + ELen;
      end;
    end;
  end else if KEA in [keaECDH,keaECDHE] then begin
    Dst.KEA := KEA;
    Result := Src.data[0] in [1,3];
    if Result then begin
      Dst.ecdhParams.curve_params.curve_type := TECCurveType(Src.data[0]);
      if Dst.ecdhParams.curve_params.curve_type = ctExplicitPrime then begin
        Dst.ecdhParams.curve_params.curveName := 0;
        ELen := Src.data[1];
        Result := Len >= ELen + 1;
        if Result then begin
          Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.Q,Src.Data[2],ELen,ELen*8);
          L := 1 + ELen;
          ELen := Src.data[L];
          Result := Len >= ELen + L + 1;
          if Result then begin
            Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.A,Src.Data[L + 1],ELen,ELen*8);
            L := L + 1 + ELen;
            ELen := Src.data[L];
            Result := Len >= ELen + L + 1;
            if Result then begin
              Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.B,Src.Data[L + 1],ELen,ELen*8);
              L := L + 1 + ELen;
              ELen := Src.data[L];
              Result := Len >= ELen + L + 1;
              if Result then begin
                {Seed}
                L := L + 1 + ELen;
                ELen := Src.data[L];
                Result := Len >= ELen + L + 1;
                if Result then begin
                  SS := TStringStream.Create('');
                  try
                    B := V_ASN1_OCTET_STRING;
                    SS.Write(B,1);
                    if ELen > 127 then begin
                      B := $81;
                      SS.Write(B,1);
                    end;
                    B := ELen;
                    SS.Write(B,1);
                    SS.Write(Src.Data[L + 1],ELen);
                    SS.Position := 0;
                    with TASN1Struct.Create do try
                      LoadFromStream(SS,fmtDER);
                      Result := ContentAsECPoint(Dst.ecdhParams.curve_params.primeCurve.G,
                                                 Dst.ecdhParams.curve_params.primeCurve);
                    finally
                      Free;
                    end;
                  finally
                    SS.Free;
                  end;
                  if Result then begin
                    L := L + 1 + ELen;
                    ELen := Src.data[L];
                    Result := Len >= ELen + L + 1;
                    if Result then begin
                      Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.R,Src.Data[L + 1],ELen,ELen*8);
                      L := L + 1 + ELen;
                      ELen := Src.data[L];
                      Result := Len >= ELen + L + 1;
                      if Result then begin
                        Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.K,Src.Data[L + 1],ELen,ELen*8);
                        L := L + 1 + ELen;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end else if Dst.ecdhParams.curve_params.curve_type = ctNamedCurve then begin
        Dst.ecdhParams.curve_params.curveName := Src.data[1];
        case Src.data[1] of
          19: ECurveP192(Dst.ecdhParams.curve_params.primeCurve);
          21: ECurveP224(Dst.ecdhParams.curve_params.primeCurve);
          23: ECurveP256(Dst.ecdhParams.curve_params.primeCurve);
          24: ECurveP384(Dst.ecdhParams.curve_params.primeCurve);
          25: ECurveP521(Dst.ecdhParams.curve_params.primeCurve);
        else
          Result := False;
        end;
        L := 2;
      end;
      if Result then begin
        ELen := Src.data[L];
        Result := Len >= ELen + L + 1;
        if Result then begin
          SS := TStringStream.Create('');
          try
            B := V_ASN1_OCTET_STRING;
            SS.Write(B,1);
            if ELen > 127 then begin
              B := $81;
              SS.Write(B,1);
            end;
            B := ELen;
            SS.Write(B,1);
            SS.Write(Src.Data[L + 1],ELen);
            SS.Position := 0;
            with TASN1Struct.Create do try
              LoadFromStream(SS,fmtDER);
              Result := ContentAsECPoint(Dst.ecdhParams.public_key,
                                         Dst.ecdhParams.curve_params.primeCurve);
            finally
              Free;
            end;
            L := L + 1 + ELen;
          finally
            SS.Free;
          end;
        end;
        ECDealloc(Dst.ecdhParams.curve_params.primeCurve);
        ECDealloc(Dst.ecdhParams.public_key);
      end;
    end;
  end else
    Result := False;
end;

procedure InternalDecodeServerKea(Src: PServerKeyExchange;
                                  KEA: TKeyExchangeAlgorithm;
                                  var Dst: TInternalServerKeyExchange);
var
  L: Integer;
  ELen: Word;
  B: Byte;
  SS: TStringStream;
begin
  if KEA in [keaRSA,keaRSA_Export,keaRSA1024] then begin
    ELen := Src.data[0] shl 8 + Src.data[1];
    Base256ToUMPInt(Dst.rsaParams.rsa_modulus,Src.Data[2],ELen,ELen*8);
    L := 2 + ELen;
    ELen := Src.data[L] shl 8 + Src.data[L+1];
    Base256ToUMPInt(Dst.rsaParams.rsa_exponent,Src.Data[L+2],ELen,ELen*8);
  end else if KEA in [keaDHE,keaDHE_Export,keaDHE1024] then begin
    ELen := Src.data[0] shl 8 + Src.data[1];
    Base256ToUMPInt(Dst.dhParams.dh_P,Src.Data[2],ELen,ELen*8);
    L := 2 + ELen;
    ELen := Src.data[L] shl 8 + Src.data[L+1];
    Base256ToUMPInt(Dst.dhParams.dh_G,Src.Data[L+2],ELen,ELen*8);
    L := L + 2 + ELen;
    ELen := Src.data[L] shl 8 + Src.data[L+1];
    Base256ToUMPInt(Dst.dhParams.dh_Ys,Src.Data[L+2],ELen,ELen*8);
  end else if KEA in [keaECDH,keaECDHE] then begin
    Dst.KEA := KEA;
    Dst.ecdhParams.curve_params.curve_type := TECCurveType(Src.data[0]);
    if Dst.ecdhParams.curve_params.curve_type = ctExplicitPrime then begin
      Dst.ecdhParams.curve_params.curveName := 0;
      ELen := Src.data[1];
      Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.Q,Src.Data[2],ELen,ELen*8);
      L := 1 + ELen;
      ELen := Src.data[L];
      Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.A,Src.Data[L + 1],ELen,ELen*8);
      L := L + 1 + ELen;
      ELen := Src.data[L];
      Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.B,Src.Data[L + 1],ELen,ELen*8);
      L := L + 1 + ELen;
      ELen := Src.data[L];
      {Seed}
      L := L + 1 + ELen;
      ELen := Src.data[L];
      SS := TStringStream.Create('');
      try
        B := V_ASN1_OCTET_STRING;
        SS.Write(B,1);
        if ELen > 127 then begin
          B := $81;
          SS.Write(B,1);
        end;
        B := ELen;
        SS.Write(B,1);
        SS.Write(Src.Data[L + 1],ELen);
        SS.Position := 0;
        with TASN1Struct.Create do try
          LoadFromStream(SS,fmtDER);
          ContentAsECPoint(Dst.ecdhParams.curve_params.primeCurve.G,
                           Dst.ecdhParams.curve_params.primeCurve);
        finally
          Free;
        end;
      finally
        SS.Free;
      end;
      L := L + 1 + ELen;
      ELen := Src.data[L];
      Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.R,Src.Data[L + 1],ELen,ELen*8);
      L := L + 1 + ELen;
      ELen := Src.data[L];
      Base256ToUMPInt(Dst.ecdhParams.curve_params.primeCurve.K,Src.Data[L + 1],ELen,ELen*8);
      L := L + 1 + ELen;
    end else if Dst.ecdhParams.curve_params.curve_type = ctNamedCurve then begin
      Dst.ecdhParams.curve_params.curveName := Src.data[1];
      case Src.data[1] of
        19: ECurveP192(Dst.ecdhParams.curve_params.primeCurve);
        21: ECurveP224(Dst.ecdhParams.curve_params.primeCurve);
        23: ECurveP256(Dst.ecdhParams.curve_params.primeCurve);
        24: ECurveP384(Dst.ecdhParams.curve_params.primeCurve);
        25: ECurveP521(Dst.ecdhParams.curve_params.primeCurve);
      end;
      L := 2;
    end else
      L := 0; // Happy compiler
    ELen := Src.data[L];
    SS := TStringStream.Create('');
    try
      B := V_ASN1_OCTET_STRING;
      SS.Write(B,1);
      if ELen > 127 then begin
        B := $81;
        SS.Write(B,1);
      end;
      B := ELen;
      SS.Write(B,1);
      SS.Write(Src.Data[L + 1],ELen);
      SS.Position := 0;
      with TASN1Struct.Create do try
        LoadFromStream(SS,fmtDER);
        ContentAsECPoint(Dst.ecdhParams.public_key,
                         Dst.ecdhParams.curve_params.primeCurve);
      finally
        Free;
      end;
    finally
      SS.Free;
    end;
  end;
end;

function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     KEA: TKeyExchangeAlgorithm;
                                     const SignKey: TIFPublicKey;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
var
  L, SignLen: Integer;
  H: THash;
begin
  Result := InternalDecodeServerKeaCheck(Src,Len,KEA,L,Dst);
  if Result then begin
    H := TMD5.Create(ClientRandom,32);
    try
      H.HashData(ServerRandom,32);
      H.HashData(Src^,L);
      H.Done(@Dst.Signed.input.rsaInput.md5_hash);
    finally
      H.Free;
    end;
    H := TSHA1.Create(ClientRandom,32);
    try
      H.HashData(ServerRandom,32);
      H.HashData(Src^,L);
      H.Done(@Dst.Signed.input.rsaInput.sha_hash);
    finally
      H.Free;
    end;

    SignLen := Src^.Data[L] shl 8 + Src^.Data[L+1];

    Result := (Len >= SignLen + L + 2) and
              (SignLen = (MPMSB(SignKey.N) + 7) shr 3);

    if Result then
      Result := IFSSASignatureVerification(SignKey,
                                           Dst.Signed.input.rsaInput,36,
                                           Src^.Data[L+2],SignLen,
                                           haMD5,haSHA1, // <- These parameters are not used
                                           seEMSA_TLS,
                                           False)
    else
      Result := False;
    if Result then
      InternalDecodeServerKea(Src,KEA,Dst);
  end;
end;

function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     KEA: TKeyExchangeAlgorithm;
                                     const SignKey: TDLPublicKey;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
var
  L, SignLen: Integer;
  H: THash;
begin
  Result := InternalDecodeServerKeaCheck(Src,Len,KEA,L,Dst);
  if Result then begin
    H := TSHA1.Create(ClientRandom,32);
    try
      H.HashData(ServerRandom,32);
      H.HashData(Src^.Data[0],L);
      H.Done(@Dst.Signed.input.rsaInput.sha_hash);
    finally
      H.Free;
    end;

    SignLen := Src^.Data[L] shl 8 + Src^.Data[L+1];

    Result := (Len >= SignLen + L + 2) and
              (SignLen = (MPMSB(SignKey.Params.Q) + 7) shr 3);

    if Result then
      Result := DLSSASignatureVerification(SignKey,
                                           Dst.Signed.input.sha_hash,20,
                                           Src^.Data[L+2],SignLen,
                                           True,haSHA1, // <- These parameters are not used
                                           False);
    if Result then
      InternalDecodeServerKea(Src,KEA,Dst);
  end;
end;

function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     const ClientRandom, ServerRandom: TRandomString;
                                     KEA: TKeyExchangeAlgorithm;
                                     const SignKey: TECPublicKey;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
var
  L, SignLen: Integer;
  H: THash;
begin
  Result := InternalDecodeServerKeaCheck(Src,Len,KEA,L,Dst);
  if Result then begin
    {TODO: Add support for SHA-2}
    H := TSHA1.Create(ClientRandom,32);
    try
      H.HashData(ServerRandom,32);
      H.HashData(Src^.Data[0],L);
      H.Done(@Dst.Signed.input.rsaInput.sha_hash);
    finally
      H.Free;
    end;

    SignLen := Src^.Data[L] shl 8 + Src^.Data[L+1];

    Result := (Len >= SignLen + L + 2) and
              (SignLen = (MPMSB(SignKey.Params.Q) + 7) shr 3);

    if Result then
      Result := ECSSASignatureVerification(SignKey,
                                           Dst.Signed.input.sha_hash,20,
                                           Src^.Data[L+2],SignLen,
                                           True,haSHA1, // <- These parameters are not used
                                           False);
    if Result then
      InternalDecodeServerKea(Src,KEA,Dst);
  end;
end;

function TLS_DecodeServerKeyExchange(Src: PServerKeyExchange;
                                     Len: Integer;
                                     KEA: TKeyExchangeAlgorithm;
                                     var Dst: TInternalServerKeyExchange): Boolean; overload;
var
  L, SignLen: Integer;
begin
  Result := InternalDecodeServerKeaCheck(Src,Len,KEA,L,Dst);
  if Result then begin
    SignLen := Src^.Data[L] shl 8 + Src^.Data[L+1];

    Result := (Len >= SignLen + L + 2) and
              (SignLen = 0);
    if Result then
      InternalDecodeServerKea(Src,KEA,Dst);
  end;
end;

function TLS_EncodeCertificateRequest(const Src: TInternalCertificateRequest;
                                      var Dst: PTLSHandshake): Integer;
var
  Len, L, I: Integer;
  MS, MSN: TSecureMemoryStream;
  F: TASN1Struct;
  W: Word;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  MS := TSecureMemoryStream.Create;
  try
    F := nil;
    try
      MSN := TSecureMemoryStream.Create;
      try
        for I := 0 to Length(Src.certificate_authorities) - 1 do begin
          TranslateName(Src.certificate_authorities[I],F);
          F.SaveToStream(MSN,fmtDER);
          W := MSN.Size;
          W := Byte(W shr 8) or Word(W shl 8);
          MS.Write(W,2);
          MS.CopyFrom(MSN,0);
          MSN.Size := 0;
        end;
      finally
        MSN.Free;
      end;
    finally
      F.Free;
    end;
    L := Length(Src.certificate_types);
    W := MS.Size;
    Len := 4 + 1 + L + 2 + W;
    ReallocMem(Dst,Len);
    MS.Position := 0;
    MS.Read(Dst^.certificate_request.data[L+3],W);
  finally
    MS.Free;
  end;
  Dst.certificate_request.data[L+1] := W shr 8;
  Dst.certificate_request.data[L+2] := Byte(W);

  Dst.certificate_request.data[0] := L;
  Move(Src.certificate_types[0],Dst^.certificate_request.data[1],L);

  Result := Len;

  Dst^.msg_type := certificate_request;
  Dst^.length[0] := (Len - 4) shr 16;
  Dst^.length[1] := (Len - 4) shr 8 and $FF;
  Dst^.length[2] := (Len - 4) and $FF;
end;

function TLS_DecodeCertificateRequest(Src: PCertificateRequest;
                                      Len: Integer;
                                      var Dst: TInternalCertificateRequest): Boolean;
var
  I, L: Integer;
  W: Word;
  RS: TReadStream;
  F: TASN1Struct;
begin
  Result := Len >= 7;
  if not Result then Exit;
  L := Src^.data[0];
  Result := Len >= L + 6;
  if not Result then Exit;
  SetLength(Dst.certificate_types,L);
  Move(Src^.data[1],Dst.certificate_types[0],L);
  L := L + 1;
  W := Src^.data[L] shl 8 or Src^.data[L+1];
  Result := Len >= L + 2 + W;
  if not Result then Exit;
  RS := TReadStream.Create(Src^.Data[L+2],W);
  try
    L := W;
    I := 0;
    repeat
      RS.Read(W,2);
      W := W shr 8 or W shl 8;
      F := TASN1Struct.Create;
      try
        try
          Result := F.LoadFromStream(RS,fmtDER) = W;
        except
          Result := False;
        end;
        if Result then begin
          SetLength(Dst.certificate_authorities,I+1);
          Result := InterpretName(F,Dst.certificate_authorities[I]) = E_OK;
          Inc(I);
        end;
      finally
        F.Free;
      end;
    until (RS.Position = L) or not Result;
  finally
    RS.Free;
  end;
end;

function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer;
var
  Len: Integer;
  PMS: ^TPreMasterSecret;
  IFPubl: TIFPublicKey;
  DLPubl: TDLPublicKey;
  DLPriv: TDLPrivateKey;
  Y: PMPInteger;
  P: string;
  ECPubl: TECPublicKey;
  ECPriv: TECPrivateKey;
  W: TECPoint;
  Seed: string[64];
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end else
    Len := -4;

  case ServerKey.KeyAlgorithmType of
    katIF:
      begin
        GetMem(PMS,48);
        try
          PMS^.version.major := 3;
          PMS^.version.minor := 1;
          RawRandom(PMS^.random,46*8);
          try
            SetLength(Seed,64);
            try
              Move(Ctx.client_random,Seed[1],32);
              Move(Ctx.server_random,Seed[33],32);
              TLS_PRF(PMS^,48,
                      'master secret',
                      Seed,
                      Ctx.master_secret,48);
            finally
              ProtectClear(Seed,65);
            end;
            FillChar(IFPubl,SizeOf(IFPubl),0);
            try
              ServerKey.AsIFKey(IFPubl);
              Len := MPMSB(IFPubl.N) shr 3;
              ReallocMem(Dst,Len + 6);
              Result := Len + 6;
              IFESEncryption(IFPubl,PMS^,SizeOf(PMS^),
                             nil^,0,haSHA1,haSHA1,
                             eeEME_PKCS1_v1_5,
                             Dst^.client_key_exchange.data[2],Len);
              Dst^.client_key_exchange.data[0] := Len shr 8;
              Dst^.client_key_exchange.data[1] := Byte(Len);
            finally
              DisposeIFPublicKey(IFPubl);
            end;
          finally
            ProtectClear(PMS^,SizeOf(PMS^));
          end;
        finally
          FreeMem(PMS);
        end;
      end;
    katDL:
      begin
        FillChar(DLPubl,SizeOf(DLPubl),0);
        Y := nil;
        FillChar(DLPriv,SizeOf(DLPriv),0);
        try
          ServerKey.AsDLKey(DLPubl);
          DLKeys(DLPubl.Params,DLPriv,Y);
          Len := (MPMSB(DLPubl.Params.P) + 7) shr 3;
          ReallocMem(Dst,Len + 6);
          Result := Len + 6;
          Dst^.client_key_exchange.data[0] := (Len) shr 8;
          Dst^.client_key_exchange.data[1] := Byte(Len);
          UMPIntToBase256(Y,Dst^.client_key_exchange.data[2],Len);
          MPExpMod(DLPubl.Y,DLPriv.X,DLPubl.Params.P,Y);
          SetLength(P,Len);
          try
            UMPIntToBase256(Y,Pointer(P)^,Len);
            SetLength(Seed,64);
            try
              Move(Ctx.client_random,Seed[1],32);
              Move(Ctx.server_random,Seed[33],32);
              TLS_PRF(Pointer(P)^,Len,
                      'master secret',
                      Seed,
                      Ctx.master_secret,48);
            finally
              ProtectClear(Seed,65);
            end;
          finally
            ProtectClear(Pointer(P)^,Len);
          end;
        finally
          DisposeDLPublicKey(DLPubl);
          DisposeDLPrivateKey(DLPriv);
          MPDealloc(Y);
        end;
      end;
    katEC:
      begin
        FillChar(ECPubl,SizeOf(ECPubl),0);
        FillChar(W,SizeOf(W),0);
        FillChar(ECPriv,SizeOf(ECPriv),0);
        try
          ServerKey.AsECKey(ECPubl);
          ECKeys(ECPubl.Params,ECPriv,W);
          with TASN1Struct.Create do begin
            Tag := V_ASN1_OCTET_STRING;
            EditContent(W,ECPubl.Params,cCompressed);
            P := ContentAsOctetString;
            Free;
          end;
          Len := Length(P);
          ReallocMem(Dst,Len + 5);
          Result := Len + 5;
          Dst^.client_key_exchange.data[0] := Len;
          Move(Pointer(P)^,Dst^.client_key_exchange.data[1],Len);
          ProtectClear(Pointer(P)^,Len);
          SetLength(P,20);
          try
            ECKAS_DH1(ECPriv,ECPubl.W,nil^,0,False,haSHA1,kdKDF1,Pointer(P)^,20);
            SetLength(Seed,64);
            try
              Move(Ctx.client_random,Seed[1],32);
              Move(Ctx.server_random,Seed[33],32);
              TLS_PRF(Pointer(P)^,20,
                      'master secret',
                      Seed,
                      Ctx.master_secret,48);
            finally
              ProtectClear(Seed,65);
            end;
          finally
            ProtectClear(Pointer(P)^,20);
          end;
        finally
          DisposeECPublicKey(ECPubl);
          DisposeECPrivateKey(ECPriv);
          ECDealloc(W);
        end;
      end;
  else
    Result := Len + 4;
  end;
  if Assigned(Dst) then begin
    Dst^.msg_type := client_key_exchange;
    Dst^.length[0] := (Result - 4) shr 16;
    Dst^.length[1] := Byte((Result - 4) shr 8);
    Dst^.length[2] := Byte(Result - 4);
  end;
end;

function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     const DLPriv: TDLPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload; 
var
  Len: Integer;
  DLPubl: TDLPublicKey;
  Y: PMPInteger;
  P: string;
  Seed: string[64];
begin             
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  FillChar(DLPubl,SizeOf(DLPubl),0);
  Y := nil;
  try
    DLPublicKey(DLPriv,DLPubl);
    MPCopy2(DLPubl.Y,Y);
    ServerKey.AsDLKey(DLPubl);
    ReallocMem(Dst,6);
    Result := 6;
    Dst^.client_key_exchange.data[0] := 0;
    Dst^.client_key_exchange.data[1] := 0;
    MPExpMod(DLPubl.Y,DLPriv.X,DLPubl.Params.P,Y);
    Len := (MPMSB(DLPubl.Params.P) + 7) shr 3;
    SetLength(P,Len);
    try
      UMPIntToBase256(Y,Pointer(P)^,Len);
      SetLength(Seed,64);
      try
        Move(Ctx.client_random,Seed[1],32);
        Move(Ctx.server_random,Seed[33],32);
        TLS_PRF(Pointer(P)^,Len,
                'master secret',
                Seed,
                Ctx.master_secret,48);
      finally
        ProtectClear(Seed,65);
      end;
    finally
      ProtectClear(Pointer(P)^,Len);
    end;
  finally
    DisposeDLPublicKey(DLPubl);
    MPDealloc(Y);
  end;

  Dst^.msg_type := client_key_exchange;
  Dst^.length[0] := (Result - 4) shr 16;
  Dst^.length[1] := Byte((Result - 4) shr 8);
  Dst^.length[2] := Byte(Result - 4);
end;

function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  Len: Integer;
  DLPubl: TDLPublicKey;
  P: ISecretKey;
  Seed: string[64];
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  FillChar(DLPubl,SizeOf(DLPubl),0);
  try
    ReallocMem(Dst,6);
    Result := 6;
    Dst^.client_key_exchange.data[0] := 0;
    Dst^.client_key_exchange.data[1] := 0;
    ServerKey.AsDLKey(DLPubl);
    Len := (MPMSB(DLPubl.Params.P) + 7) shr 3;
    P := TSecretKey.Create('');
    P.SetLength(Len);
    DLPriv.KDF := kdNone;
    DLPriv.DecryptKeyAgreement(DLPubl,P.Key^,Len);
    SetLength(Seed,64);
    try
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      TLS_PRF(P.Key^,Len,
              'master secret',
              Seed,
              Ctx.master_secret,48);
    finally
      ProtectClear(Seed,65);
    end;
  finally
    DisposeDLPublicKey(DLPubl);
  end;

  Dst^.msg_type := client_key_exchange;
  Dst^.length[0] := (Result - 4) shr 16;
  Dst^.length[1] := Byte((Result - 4) shr 8);
  Dst^.length[2] := Byte(Result - 4);
end;

function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     const ECPriv: TECPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  Len, L: Integer;
  P: string;
  ECPubl: TECPublicKey;
  W: TECPoint;
  Seed: string[64];
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  FillChar(ECPubl,SizeOf(ECPubl),0);
  FillChar(W,SizeOf(W),0);
  try
    ECPublicKey(ECPriv,ECPubl);
    ECCopy2(ECPubl.W,W);
    ServerKey.AsECKey(ECPubl);
    ReallocMem(Dst,5);
    Result := 5;
    Dst^.client_key_exchange.data[0] := 0;
    // draft-ietf-tls-ecc-03
    L := MPMSB(ECPubl.Params.Q) shr 3;
    if L <= 24 then
      L := 20;
    SetLength(P,L);
    try
      if L = 20 then
        ECKAS_DH1(ECPriv,ECPubl.W,nil^,0,False,haSHA1,kdKDF1,Pointer(P)^,L)
      else
        ECKAS_DH1(ECPriv,ECPubl.W,nil^,0,False,haSHA1,kdNone,Pointer(P)^,L);
      SetLength(Seed,64);
      try
        Move(Ctx.client_random,Seed[1],32);
        Move(Ctx.server_random,Seed[33],32);
        TLS_PRF(Pointer(P)^,20,
                'master secret',
                Seed,
                Ctx.master_secret,48);
      finally
        ProtectClear(Seed,65);
      end;
    finally
      ProtectClear(Pointer(P)^,L);
    end;
  finally
    DisposeECPublicKey(ECPubl);
    ECDealloc(W);
  end;

  Dst^.msg_type := client_key_exchange;
  Dst^.length[0] := (Result - 4) shr 16;
  Dst^.length[1] := Byte((Result - 4) shr 8);
  Dst^.length[2] := Byte(Result - 4);
end;

function TLS_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     ECPriv: IMPECPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  Len, L: Integer;
  P: ISecretKey;
  ECPubl: TECPublicKey;
  Seed: string[64];
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len);
  end;

  FillChar(ECPubl,SizeOf(ECPubl),0);
  try
    ServerKey.AsECKey(ECPubl);
    ReallocMem(Dst,5);
    Result := 5;
    Dst^.client_key_exchange.data[0] := 0;
    P := TSecretKey.Create('');
    // draft-ietf-tls-ecc-03
    L := MPMSB(ECPubl.Params.Q) shr 3;
    if L <= 24 then begin
      L := 20;
      ECPriv.KDF := kdKDF1;
    end else
      ECPriv.KDF := kdNone;
    P.SetLength(L);
    ECPriv.DecryptKeyAgreement(ECPubl,P.Key^,L);
    SetLength(Seed,64);
    try
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      TLS_PRF(P.Key^,L,
              'master secret',
              Seed,
              Ctx.master_secret,48);
    finally
      ProtectClear(Seed,65);
    end;
  finally
    DisposeECPublicKey(ECPubl);
  end;

  Dst^.msg_type := client_key_exchange;
  Dst^.length[0] := (Result - 4) shr 16;
  Dst^.length[1] := Byte((Result - 4) shr 8);
  Dst^.length[2] := Byte(Result - 4);
end;

function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     IFPriv: TIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L, PMSLen: Integer;
  PMS: TPreMasterSecret;
  Seed: string[64];
begin
  Result := Len >= 2;
  if not Result then Exit;
  L := Src^.data[0] shl 8 + Src^.data[1];
  Result := Len >= L + 2;
  if not Result then Exit;
  PMSLen := 48;
  Result := IFESDecryption(IFPriv,
                           Src^.data[2],L,nil^,0,haSHA1,haSHA1,
                           eeEME_PKCS1_v1_5,
                           PMS,PMSLen);
  Result := Result and (PMSLen = 48);
  if Result then try
    Result := (PMS.version.major > 3) or
              ((PMS.version.major = 3) and (PMS.version.minor >= 1));
    if Result then try
      SetLength(Seed,64);
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      TLS_PRF(PMS,48,'master secret',Seed,Ctx.master_secret,48);
    finally
      ProtectClear(Seed,65);
    end;
  finally
    ProtectClear(PMS,48);
  end else try
    // This prevents the Daniel Bleichenbacher attack:
    RawRandom(PMS,48*8);                  
    SetLength(Seed,64);
    Move(Ctx.client_random,Seed[1],32);
    Move(Ctx.server_random,Seed[33],32);
    TLS_PRF(PMS,48,'master secret',Seed,Ctx.master_secret,48);
    Result := True;
  finally
    ProtectClear(PMS,48);
  end;
end;

function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     IFPriv: IMPIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L, PMSLen: Integer;
  PMS: TPreMasterSecret;
  Seed: string[64];
begin
  Result := Len >= 2;
  if not Result then Exit;
  L := Src^.data[0] shl 8 + Src^.data[1];
  Result := Len >= L + 2;
  if not Result then Exit;
  PMSLen := 48;
  Result := IFPriv.DecryptKeyTransport(Src^.data[2],L,PMS,PMSLen);
  Result := Result and (PMSLen = 48);
  if Result then try
    Result := (PMS.version.major > 3) or
              ((PMS.version.major = 3) and (PMS.version.minor >= 1));
    if Result then try
      SetLength(Seed,64);
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      TLS_PRF(PMS,48,'master secret',Seed,Ctx.master_secret,48);
    finally
      ProtectClear(Seed,65);
    end;
  finally
    ProtectClear(PMS,48);
  end else try
    // This prevents the Daniel Bleichenbacher attack:
    RawRandom(PMS,48*8);                  
    SetLength(Seed,64);
    Move(Ctx.client_random,Seed[1],32);
    Move(Ctx.server_random,Seed[33],32);
    TLS_PRF(PMS,48,'master secret',Seed,Ctx.master_secret,48);
    Result := True;
  finally
    ProtectClear(PMS,48);
  end;
end;

function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     DLPriv: TDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L: Integer;
  DLPubl: TDLPublicKey;
  Y: PMPInteger;
  P: string;
  Seed: string[64];
begin
  Result := Len >= 2;
  if not Result then Exit;
  L := Src^.data[0] shl 8 + Src^.data[1];
  Result := Len >= L + 2;
  if not Result then Exit;
  FillChar(DLPubl,SizeOf(DLPubl),0);
  try
    if (L = 0) and Assigned(ClientCert) then begin
      Result := keyAgreement in ExtractKeyUsage(ClientCert);
      if Result then
        Result := ExtractSubjectDHPublicKey(ClientCert,DLPubl) = E_OK;
    end else if L > 0 then begin
      L := Src^.data[0] shl 8 + Src^.data[1];
      Result := Len >= L + 2;
      if Result then try
        Base256ToUMPInt(DLPubl.Y,Src^.data[2],L,MPMSB(DLPriv.Params.P));
      except
        Result := False;
      end;
    end else
      Result := False;
    if Result then begin
      Y := nil;
      try
        L := (MPMSB(DLPriv.Params.P) + 7) shr 3;
        MPExpMod(DLPubl.Y,DLPriv.X,DLPriv.Params.P,Y);
        SetLength(P,L);
        try
          UMPIntToBase256(Y,Pointer(P)^,L);
          SetLength(Seed,64);
          try
            Move(Ctx.client_random,Seed[1],32);
            Move(Ctx.server_random,Seed[33],32);
            TLS_PRF(Pointer(P)^,L,
                    'master secret',
                    Seed,
                    Ctx.master_secret,48);
          finally
            ProtectClear(Seed,65);
          end;
        finally
          ProtectClear(Pointer(P)^,L);
        end;
      finally
        MPDealloc(Y);
      end;
    end;
  finally
    DisposeDLPublicKey(DLPubl);
  end;
end;                      

function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L: Integer;
  DLPubl: TDLPublicKey;
  MPDLPubl: TMPDLPublicKey;
  Y: PMPInteger;
  P: ISecretKey;
  Seed: string[64];
begin
  Result := Len >= 2;
  if not Result then Exit;
  L := Src^.data[0] shl 8 + Src^.data[1];
  Result := Len >= L + 2;
  if not Result then Exit;
  FillChar(DLPubl,SizeOf(DLPubl),0);
  try
    if (L = 0) and Assigned(ClientCert) then begin
      Result := keyAgreement in ExtractKeyUsage(ClientCert);
      if Result then
        Result := ExtractSubjectDHPublicKey(ClientCert,DLPubl) = E_OK;
    end else if L > 0 then begin
      L := Src^.data[0] shl 8 + Src^.data[1];
      Result := Len >= L + 2;
      if Result then try
        Base256ToUMPInt(DLPubl.Y,Src^.data[2],L,MPMSB(DLPriv.SystemParams.P));
      except
        Result := False;
      end;
    end else
      Result := False;
    if Result then begin
      Y := nil;
      try
        L := (MPMSB(DLPriv.SystemParams.P) + 7) shr 3;
        P := TSecretKey.Create('');
        P.SetLength(L);
        CopyDLSystemParams(DLPubl.Params,DLPriv.SystemParams^);
        MPDLPubl := TMPDLPublicKey.CreateFromRecord(dhPublicNumber,DLPubl,nil);
        try
          DLPriv.KDF := kdNone;
          Result := DLPriv.DecryptKeyAgreement(MPDLPubl,P.Key^,L);
        finally
          MPDLPubl.Free;
        end;
        SetLength(Seed,64);
        try
          Move(Ctx.client_random,Seed[1],32);
          Move(Ctx.server_random,Seed[33],32);
          TLS_PRF(P.Key^,L,
                  'master secret',
                  Seed,
                  Ctx.master_secret,48);
        finally
          ProtectClear(Seed,65);
        end;
      finally
        MPDealloc(Y);
      end;
    end;
  finally
    DisposeDLPublicKey(DLPubl);
  end;
end;

function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     ECPriv: TECPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L: Integer;
  ECPubl: TECPublicKey;
  Y: PMPInteger;
  P: string;
  Seed: string[64];
begin
  Result := Len >= 1;
  if not Result then Exit;
  L := Src^.data[0];
  Result := Len >= L + 1;
  if not Result then Exit;
  FillChar(ECPubl,SizeOf(ECPubl),0);
  try
    if (L = 0) and Assigned(ClientCert) then begin
      Result := keyAgreement in ExtractKeyUsage(ClientCert);
      if Result then
        Result := ExtractSubjectECPublicKey(ClientCert,ECPubl) = E_OK;
    end else if L > 0 then begin
      L := Src^.data[0];
      Result := Len >= L + 1;
      if Result then try
        with TASN1Struct.Create do begin
          Tag := V_ASN1_OCTET_STRING;
          SetContent(Src^.data[1],L);
          Result := ContentASECPoint(ECPubl.W,ECPriv.Params);
          Free;
        end;
      except
        Result := False;
      end;
    end else
      Result := False;
    if Result then begin
      Y := nil;
      try
        // draft-ietf-tls-ecc-03
        L := MPMSB(ECPubl.Params.Q) shr 3;
        if L <= 24 then begin
          L := 20;
          SetLength(P,L);
          ECKAS_DH1(ECPriv,ECPubl.W,nil^,0,False,haSHA1,kdKDF1,Pointer(P)^,L)
        end else begin
          SetLength(P,L);
          ECKAS_DH1(ECPriv,ECPubl.W,nil^,0,False,haSHA1,kdNone,Pointer(P)^,L);
        end;
        SetLength(Seed,64);
        try
          Move(Ctx.client_random,Seed[1],32);
          Move(Ctx.server_random,Seed[33],32);
          TLS_PRF(Pointer(P)^,L,
                  'master secret',
                  Seed,
                  Ctx.master_secret,48);
        finally
          ProtectClear(Seed,65);
        end;
      finally
        MPDealloc(Y);
      end;
    end;
  finally
    DisposeECPublicKey(ECPubl);
  end;
end;

function TLS_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     ECPriv: IMPECPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L: Integer;
  ECPubl: TECPublicKey;
  MPECPubl: TMPECPublicKey;
  Y: PMPInteger;
  P: ISecretKey;
  Seed: string[64];
begin
  Result := Len >= 1;
  if not Result then Exit;
  L := Src^.data[0];
  Result := Len >= L + 1;
  if not Result then Exit;
  FillChar(ECPubl,SizeOf(ECPubl),0);
  try
    if (L = 0) and Assigned(ClientCert) then begin
      Result := keyAgreement in ExtractKeyUsage(ClientCert);
      if Result then
        Result := ExtractSubjectECPublicKey(ClientCert,ECPubl) = E_OK;
    end else if L > 0 then begin
      L := Src^.data[0];
      Result := Len >= L + 1;
      if Result then try
        with TASN1Struct.Create do begin
          Tag := V_ASN1_OCTET_STRING;
          SetContent(Src^.data[1],L);
          Result := ContentAsECPoint(ECPubl.W,ECPriv.SystemParams^);
          Free;
        end;
      except
        Result := False;
      end;
    end else
      Result := False;
    if Result then begin
      Y := nil;
      try
        L := MPMSB(ECPubl.Params.Q) shr 3;
        P := TSecretKey.Create('');
        MPECPubl := TMPECPublicKey.CreateFromRecord(id_ecPublicKey,ECPubl,nil);
        try
          // draft-ietf-tls-ecc-03
          if L > 24 then
            ECPriv.KDF := kdNone
          else begin
            L := 20;
            ECPriv.KDF := kdKDF1;
          end;
          P.SetLength(L);
          ECPriv.KAHashAlg := haSHA1;
          ECPriv.DecryptKeyAgreement(MPECPubl,P.Key^,L);
        finally
          MPECPubl.Free;
        end;
        SetLength(Seed,64);
        try
          Move(Ctx.client_random,Seed[1],32);
          Move(Ctx.server_random,Seed[33],32);
          TLS_PRF(P.Key^,L,
                  'master secret',
                  Seed,
                  Ctx.master_secret,48);
        finally
          ProtectClear(Seed,65);
        end;
      finally
        MPDealloc(Y);
      end;
    end;
  finally
    DisposeECPublicKey(ECPubl);
  end;
end;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: TIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  I, L, Len, SignBSize, SignLen: Integer;
  Signed: TSignature;
  MD5:  TMD5;
  SHA1: TSHA1;
  H: PTLSHandshake;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SHA1 := TSHA1.Create(nil^,0);
  try
    MD5 := TMD5.Create(nil^,0);
    try
      if Assigned(SSL2ClientHello) then begin
        L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
             SSL2ClientHello.msg_length[1];
        SHA1.HashData(SSL2ClientHello^.msg_type,L);
        MD5.HashData(SSL2ClientHello^.msg_type,L);
      end;
      for I := 0 to Length(Handshakes)-1 do begin
        H := Handshakes[I];
        L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        SHA1.HashData(H^,L + 4);
        MD5.HashData(H^,L + 4);
      end;
      SHA1.Done(@Signed.input.rsaInput.sha_hash);
      MD5.Done(@Signed.input.rsaInput.md5_hash);
    finally
      MD5.Free;
    end;
  finally
    SHA1.Free;
  end;

  case RSASignKey.ReprType of
    0: SignBSize := MPMSB(RSASignKey.zN);
    1: SignBSize := MPMSB(RSASignKey.oP) + MPMSB(RSASignKey.oQ);
    2: SignBSize := MPMSB(RSASignKey.tP) + MPMSB(RSASignKey.tQ);
  else
    SignBSize := 0;
  end;
  SignLen := (SignBSize + 7) shr 3;

  Len := SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := certificate_verify;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Signed.signature := TSecretKey.Create('');
  Signed.signature.SetLength(SignLen);
  SignLen := IFSSASignatureGeneration(RSASignKey,
                                     Signed.input.rsaInput,36,
                                     haSHA1, // <- These parameters are not used
                                     haSHA1, // <- These parameters are not used
                                     seEMSA_TLS,False,
                                     Signed.signature.KeyBytes[0],SignLen);
  if SignLen < Len - 2 then begin
    Move(Signed.signature.KeyBytes[0],
         Dst^.certificate_verify.Data[Len-SignLen],
         SignLen);
    FillChar(Dst^.certificate_verify.Data[2],Len-2-SignLen,#0);
    SignLen := Len - 2;
  end else
    Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
end;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     RSASignKey: IMPIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  I, L, Len, SignLen: Integer;
  Signed: TSignature;
  MD5:  TMD5;
  SHA1: TSHA1;
  H: PTLSHandshake;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SHA1 := TSHA1.Create(nil^,0);
  try
    MD5 := TMD5.Create(nil^,0);
    try
      if Assigned(SSL2ClientHello) then begin
        L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
             SSL2ClientHello.msg_length[1];
        SHA1.HashData(SSL2ClientHello^.msg_type,L);
        MD5.HashData(SSL2ClientHello^.msg_type,L);
      end;
      for I := 0 to Length(Handshakes)-1 do begin
        H := Handshakes[I];
        L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        SHA1.HashData(H^,L + 4);
        MD5.HashData(H^,L + 4);
      end;
      SHA1.Done(@Signed.input.rsaInput.sha_hash);
      MD5.Done(@Signed.input.rsaInput.md5_hash);
    finally
      MD5.Free;
    end;
  finally
    SHA1.Free;
  end;

  SignLen := RSASignKey.SignatureLength;

  Len := SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := certificate_verify;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;
                                 
  Signed.signature := TSecretKey.Create('');
  Signed.signature.SetLength(SignLen);
  RSASignKey.SignDig(Signed.input.rsaInput,36,Signed.signature.KeyBytes[0],SignLen);
  if SignLen < Len - 2 then begin
    Move(Signed.signature.KeyBytes[0],
         Dst^.certificate_verify.Data[Len-SignLen],
         SignLen);
    FillChar(Dst^.certificate_verify.Data[2],Len-2-SignLen,#0);
    SignLen := Len - 2;
  end else
    Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: TDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  I, L, Len, QBSize, SignLen: Integer;
  Signed: TSignature;
  SHA1: THash;
  H: PTLSHandshake;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SHA1 := TSHA1.Create(nil^,0);
  try
    if Assigned(SSL2ClientHello) then begin
      L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
           SSL2ClientHello.msg_length[1];
      SHA1.HashData(SSL2ClientHello^.msg_type,L);
    end;
    for I := 0 to Length(Handshakes)-1 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SHA1.Done(@Signed.input.sha_hash);
  finally
    SHA1.Free;
  end;

  QBSize := MPMSB(DSSSignKey.Params.Q);
  SignLen := (QBSize + 7) shr 3;
  if SignLen < 128 then
    SignLen := 2*SignLen + 4
  else
    SignLen := 2*SignLen + 8;
  if SignLen < 128 then
    SignLen := SignLen + 2
  else
    SignLen := SignLen + 4;

  Len := SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := certificate_verify;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Signed.signature := TSecretKey.Create('');
  Signed.signature.SetLength(SignLen);
  DLSSASignatureGeneration(DSSSignKey,
                           Signed.input.sha_hash,20,
                           haSHA1,False, // <- These parameters are not used
                           True,
                           Signed.signature.KeyBytes[0],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     DSSSignKey: IMPDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer;
var
  I, L, Len, SignLen: Integer;
  Signed: TSignature;
  SHA1: THash;
  H: PTLSHandshake;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SHA1 := TSHA1.Create(nil^,0);
  try
    if Assigned(SSL2ClientHello) then begin
      L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
           SSL2ClientHello.msg_length[1];
      SHA1.HashData(SSL2ClientHello^.msg_type,L);
    end;
    for I := 0 to Length(Handshakes)-1 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SHA1.Done(@Signed.input.sha_hash);
  finally
    SHA1.Free;
  end;
                                      
  DSSSignKey.DEREncodeSignature := True;
  DSSSignKey.SignatureDigestAlg := haSHA1;
  SignLen := DSSSignKey.SignatureLength;

  Len := SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := certificate_verify;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Signed.signature := TSecretKey.Create('');
  Signed.signature.SetLength(SignLen);
  DSSSignKey.SignDig(Signed.input.sha_hash,20,Signed.signature.KeyBytes[0],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const ECDSASignKey: TECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  I, L, Len, QBSize, SignLen: Integer;
  Signed: TSignature;
  SHA1: THash;
  H: PTLSHandshake;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SHA1 := TSHA1.Create(nil^,0);
  try
    if Assigned(SSL2ClientHello) then begin
      L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
           SSL2ClientHello.msg_length[1];
      SHA1.HashData(SSL2ClientHello^.msg_type,L);
    end;
    for I := 0 to Length(Handshakes)-1 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SHA1.Done(@Signed.input.sha_hash);
  finally
    SHA1.Free;
  end;

  QBSize := MPMSB(ECDSASignKey.Params.Q);
  SignLen := (QBSize + 7) shr 3;
  if SignLen < 128 then
    SignLen := 2*SignLen + 4
  else
    SignLen := 2*SignLen + 8;
  if SignLen < 128 then
    SignLen := SignLen + 2
  else
    SignLen := SignLen + 4;

  Len := SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := certificate_verify;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Signed.signature := TSecretKey.Create('');
  Signed.signature.SetLength(SignLen);
  ECSSASignatureGeneration(ECDSASignKey,
                           Signed.input.sha_hash,20,
                           haSHA1,False, // <- These parameters are not used
                           True,
                           Signed.signature.KeyBytes[0],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function TLS_EncodeCertificateVerify(const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     ECDSASignKey: IMPECPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
var
  I, L, Len, SignLen: Integer;
  Signed: TSignature;
  SHA1: THash;
  H: PTLSHandshake;
begin
  if Assigned(Dst) then begin
    Len := Dst^.length[0] shr 16 + Dst^.length[1] shr 8 + Dst^.length[2];
    if Len > 0 then
      ProtectClear(Dst^,Len + 4);
  end;

  SHA1 := TSHA1.Create(nil^,0);
  try
    if Assigned(SSL2ClientHello) then begin
      L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
           SSL2ClientHello.msg_length[1];
      SHA1.HashData(SSL2ClientHello^.msg_type,L);
    end;
    for I := 0 to Length(Handshakes)-1 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SHA1.Done(@Signed.input.sha_hash);
  finally
    SHA1.Free;
  end;

  ECDSASignKey.DEREncodeSignature := True;
  ECDSASignKey.SignatureDigestAlg := haSHA1;
  SignLen := ECDSASignKey.SignatureLength;

  Len := SignLen + 2;
  ReallocMem(Dst,Len + 4);
  Result := Len + 4;

  Dst^.msg_type := certificate_verify;
  Dst^.length[0] := Len shr 16;
  Dst^.length[1] := (Len shr 8) and $FF;
  Dst^.length[2] := Len and $FF;

  Signed.signature := TSecretKey.Create('');
  Signed.signature.SetLength(SignLen);

  ECDSASignKey.SignDig(Signed.input.sha_hash,20,Signed.signature.KeyBytes[0],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function TLS_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: TIFPublicKey): Boolean; overload;
var
  I, L, SignLen: Integer;
  SHA1, MD5: THash;
  H: PTLSHandshake;
  Signed: TSignature;
begin
  SHA1 := TSHA1.Create(nil^,0);
  try
    MD5 := TMD5.Create(nil^,0);
    try
      if Assigned(SSL2ClientHello) then begin
        L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
             SSL2ClientHello.msg_length[1];
        SHA1.HashData(SSL2ClientHello^.msg_type,L);
        MD5.HashData(SSL2ClientHello^.msg_type,L);
      end;
      for I := 0 to Length(Handshakes)-2 do begin
        H := Handshakes[I];
        L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        MD5.HashData(H^,L + 4);
        SHA1.HashData(H^,L + 4);
      end;
      SHA1.Done(@Signed.input.rsaInput.sha_hash);
      MD5.Done(@Signed.input.rsaInput.md5_hash);
    finally
      MD5.Free;
    end;
  finally
    SHA1.Free;
  end;

  SignLen := Src^.Data[0] shl 8 + Src^.Data[1];

  Result := (Len >= SignLen + 2) and
            (SignLen <= (MPMSB(RSASignKey.N) + 7) shr 3);

  if Result then
    Result := IFSSASignatureVerification(RSASignKey,
                                         Signed.input.rsaInput,36,
                                         Src^.Data[2],SignLen,
                                         haMD5,haSHA1, // <- These parameters are not used
                                         seEMSA_TLS,
                                         False);
end;

function TLS_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: TDLPublicKey): Boolean; overload;
var
  I, L, SignLen: Integer;
  SHA1: THash;
  H: PTLSHandshake;
  Signed: TSignature;
begin
  SHA1 := TSHA1.Create(nil^,0);
  try
    if Assigned(SSL2ClientHello) then begin
      L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
           SSL2ClientHello.msg_length[1];
      SHA1.HashData(SSL2ClientHello^.msg_type,L);
    end;
    for I := 0 to Length(Handshakes)-2 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SHA1.Done(@Signed.input.sha_hash);
  finally
    SHA1.Free;
  end;

  SignLen := Src^.Data[0] shl 8 + Src^.Data[1];

  Result := (Len >= SignLen + 2) and
            (SignLen = (MPMSB(DSSSignKey.Params.Q) + 7) shr 3);

  if Result then
    Result := DLSSASignatureVerification(DSSSignKey,
                                         Signed.input.sha_hash,20,
                                         Src.Data[2],SignLen,
                                         True,haSHA1, // <- These parameters are not used
                                         False);
end;

function TLS_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const ECDSASignKey: TECPublicKey): Boolean; overload;
var
  I, L, SignLen: Integer;
  SHA1: THash;
  H: PTLSHandshake;
  Signed: TSignature;
begin
  SHA1 := TSHA1.Create(nil^,0);
  try
    if Assigned(SSL2ClientHello) then begin
      L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
           SSL2ClientHello.msg_length[1];
      SHA1.HashData(SSL2ClientHello^.msg_type,L);
    end;
    for I := 0 to Length(Handshakes)-2 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SHA1.Done(@Signed.input.sha_hash);
  finally
    SHA1.Free;
  end;

  SignLen := Src^.Data[0] shl 8 + Src^.Data[1];

  Result := (Len >= SignLen + 2) and
            (SignLen = (MPMSB(ECDSASignKey.Params.Q) + 7) shr 3);

  if Result then
    Result := ECSSASignatureVerification(ECDSASignKey,
                                         Signed.input.sha_hash,20,
                                         Src.Data[2],SignLen,
                                         True,haSHA1);
end;
{
procedure P_Hash(const Secret; SecretLen: Integer;
                 Seed: string;
                 HashAlgorithm: THashAlgorithm;
                 var Key; KeyLen: Integer);
var
  HC: THashClass;
  A, SA: string;
  HLen: Integer;
  P: PChar;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    HLen := HC.DigestSize
  else
    HLen := 0;
  SetLength(A,HLen);
  HMac(Secret,SecretLen,Pointer(Seed)^,Length(Seed),HashAlgorithm,Pointer(A)^,HLen);
  P := @Key;
  SetLength(SA,HLen + Length(Seed));
  Move(Pointer(Seed)^,SA[HLen + 1],Length(Seed));
  while KeyLen > 0 do begin
    Move(Pointer(A)^,Pointer(SA)^,HLen);
    if KeyLen >= HLen then
      HMac(Secret,SecretLen,Pointer(SA)^,Length(SA),HashAlgorithm,P^,HLen)
    else
      HMac(Secret,SecretLen,Pointer(SA)^,Length(SA),HashAlgorithm,P^,KeyLen);
    P := P + HLen;
    KeyLen := KeyLen - HLen;
    if KeyLen > 0 then
      HMac(Secret,SecretLen,Pointer(SA)^,HLen,HashAlgorithm,Pointer(A)^,HLen);
  end;
  ProtectClear(Pointer(A)^,Length(A));
  ProtectClear(Pointer(SA)^,Length(SA));
end;
}   
procedure P_Hash(Secret: ISecretKey;
                 Seed: string;
                 HashAlgorithm: THashAlgorithm;
                 KeyLen: Integer; var Key: ISecretKey);
var
  HC: THashClass;
  A, SA, P: ISecretKey;
  HLen: Integer;
  Idx: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    HLen := HC.DigestSize
  else
    HLen := 0;
  A := nil;
  HMac(Secret,Pointer(Seed)^,Length(Seed),HashAlgorithm,HLen,A);
  Key := TSecretKey.Create('');
  Key.SetLength(KeyLen);
  SA := TSecretKey.Create('');
  SA.SetLength(HLen + Length(Seed));
  SA.SetKeyStrAt(Seed,HLen);
  Idx := 0;
  while KeyLen > 0 do begin
    SA.SetKeyAt(A,0);
    HMac(Secret,SA.Key^,SA.KeyLen,HashAlgorithm,HLen,P);
    Key.SetTruncKeyAt(P,Idx);
    Inc(Idx,HLen);
    KeyLen := KeyLen - HLen;
    if KeyLen > 0 then
      HMac(Secret,SA.Key^,HLen,HashAlgorithm,HLen,A);
  end;
end;
{
procedure TLS_PRF(const Secret; SecretLen: Integer;
                  ALabel: string;
                  Seed: string;
                  var Key; KeyLen: Integer);
var
  S, Key2: PChar;
  P: PChar;
  I, L: Integer;
  Sd: string;
begin
  L := (SecretLen + 1) shr 1;
  S := @Secret;
  S := S + SecretLen - L;
  SetLength(Sd,Length(ALabel) + Length(Seed));
  try
    Move(Pointer(ALabel)^,Pointer(Sd)^,Length(ALabel));
    if Seed <> '' then
      Move(Pointer(Seed)^,Sd[1 + Length(ALabel)],Length(Seed));
    P_Hash(S^,L,Sd,haSHA1,Key,KeyLen);
    GetMem(Key2,KeyLen);
    try
      P_Hash(Secret,L,Sd,haMD5,Key2^,KeyLen);
      P := @Key;
      for I := 0 to KeyLen - 1 do
        P[I] := Char(Byte(P[I]) xor Byte(Key2[I]));
      ProtectClear(Pointer(Key2)^,KeyLen);
    finally
      FreeMem(Key2);
    end;
  finally
    ProtectClear(Pointer(Sd)^,Length(Sd));
  end;
end;
}
procedure TLS_PRF(const Secret; SecretLen: Integer;
                  ALabel: string;
                  Seed: string;
                  var Key; KeyLen: Integer);
var
  S1, S2, Key1, Key2: ISecretKey;
  P: PChar;
  I, L: Integer;
  Sd: string;
begin
  if SecretLen < 0 then SecretLen := 0;
  if KeyLen < 0 then KeyLen := 0;
  L := (SecretLen + 1) shr 1;

  S1 := TSecretKey.Create('');
  S1.SetLength(L);
  S1.SetKey(@Secret,L,0);

  S2 := TSecretKey.Create('');
  S2.SetLength(L);
  S2.SetKey(PChar(@Secret) + SecretLen - L,L,0);

  SetLength(Sd,Length(ALabel) + Length(Seed));
  try
    Move(Pointer(ALabel)^,Pointer(Sd)^,Length(ALabel));
    if Seed <> '' then
      Move(Pointer(Seed)^,Sd[1 + Length(ALabel)],Length(Seed));

    P_Hash(S2,Sd,haSHA1,KeyLen,Key2);
    P_Hash(S1,Sd,haMD5,KeyLen,Key1);
    P := @Key;
    for I := 0 to KeyLen - 1 do
      P[I] := Char(Key1.KeyBytes[I] xor Key2.KeyBytes[I]);
  finally
    ProtectClear(Pointer(Sd)^,Length(Sd));
  end;
end;

procedure TLS_KDF(var Params: TTLSSecurityParams;
                  var Key: TTLSKeys);
var
  HC: THashClass;
  HLen:  Integer;
  KLen:  Integer;
  EKLen: Integer;
  BLen:  Integer;
  Rnds:  string[64];
begin
  HC := FindHashClass(Params.mac_algorithm);
  if Assigned(HC) then
    HLen := HC.DigestSize
  else
    HLen := 0;

  KLen := 0;
  EKLen := 0;
  BLen := 0;
  case Params.bulk_cipher_algorithm of
    bcaRC4:
      begin
        KLen := 16;
        BLen := 0;
        Params.cipher_type := ctStream;
        Params.is_exportable := False;
      end;
    bcaDES:
      begin
        KLen := 8;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := False;
      end;
    bcaDES56:
      begin
        KLen := 7;
        EKLen := 8;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := True;
      end;
    bca3DES:
      begin
        KLen := 24;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := False;
      end;
    bcaRC4_40:
      begin
        KLen := 5;
        EKLen := 16;
        BLen := 0;
        Params.cipher_type := ctStream;
        Params.is_exportable := True;
      end;
    bcaRC4_56:
      begin
        KLen := 7;
        EKLen := 16;
        BLen := 0;
        Params.cipher_type := ctStream;
        Params.is_exportable := True;
      end;
    bcaDES40:
      begin
        KLen := 5;
        EKLen := 8;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := True;
      end;
    bcaAES128,
    bcaTwoFish128:
      begin
        KLen := 16;
        BLen := 16;
        Params.cipher_type := ctBlock;
        Params.is_exportable := False;
      end;
    bcaAES192,
    bcaTwoFish192:
      begin
        KLen := 24;
        BLen := 16;
        Params.cipher_type := ctBlock;
        Params.is_exportable := False;
      end;
    bcaAES256,
    bcaTwoFish256:
      begin
        KLen := 32;
        BLen := 16;
        Params.cipher_type := ctBlock;
        Params.is_exportable := False;
      end;
    bcaAES128_CTR,
    bcaTwoFish128_CTR:
      begin
        KLen := 16;
        BLen := 16;
        Params.cipher_type := ctStream;
        Params.is_exportable := False;
      end;
    bcaAES192_CTR,
    bcaTwoFish192_CTR:
      begin
        KLen := 24;
        BLen := 16;
        Params.cipher_type := ctStream;
        Params.is_exportable := False;
      end;
    bcaAES256_CTR,
    bcaTwoFish256_CTR:
      begin
        KLen := 32;
        BLen := 16;
        Params.cipher_type := ctStream;
        Params.is_exportable := False;
      end;
    bcaBlowFish128:
      begin
        KLen := 16;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := False;
      end;
    bcaBlowFish128_CTR:
      begin
        KLen := 16;
        BLen := 8;
        Params.cipher_type := ctStream;
        Params.is_exportable := False;
      end;
    bcaHelix:
      begin
        HLen := 0;
        KLen := 32;
        BLen := 0;
        Params.cipher_type := ctStream;
        Params.is_exportable := False;
      end;
  end;

  Params.key_size := KLen;

  SetLength(Rnds,64);
  try
    Move(Params.server_random,Rnds[1],32);
    Move(Params.client_random,Rnds[33],32);
    TLS_KDF(Params.master_secret,48,
            'key expansion',
            Rnds,
            HLen,KLen,EKLen,BLen,Params.is_exportable,
            Key);
  finally
    ProtectClear(Rnds,65);
  end;
end;

procedure TLS_KDF(const Secret; SecretLen: Integer;
                  ALabel: string;
                  Seed: string;
                  HLen, KLen, EKLen, BLen: Integer;
                  Exportable: Boolean;
                  var Key: TTLSKeys);
var
  KeyBlock: string;
begin
  KeyBlock := '';
  try
    if Exportable then begin
      SetLength(KeyBlock,HLen*2 + KLen*2);
      TLS_PRF(Secret,SecretLen,ALabel,Seed,Pointer(KeyBlock)^,Length(KeyBlock));

      Move(KeyBlock[1],Key.client_write_MAC_secret,HLen);
      Move(KeyBlock[1 + HLen],Key.server_write_MAC_secret,HLen);

      TLS_PRF(KeyBlock[1 + 2*HLen],KLen,'client write key',Seed,Key.client_write_key,EKLen);
      TLS_PRF(KeyBlock[1 + 2*HLen + KLen],KLen,'server write key',Seed,Key.server_write_key,EKLen);

      ProtectClear(Pointer(KeyBlock)^,Length(KeyBlock));

      if BLen > 0 then begin
        SetLength(KeyBlock,BLen*2);
        TLS_PRF(nil^,0,'IV block',Seed,Pointer(KeyBlock)^,Length(KeyBlock));
        Move(KeyBlock[1],Key.client_write_iv,BLen);
        Move(KeyBlock[1 + BLen],Key.server_write_iv,BLen);
      end;

      Key.kLen := EKLen;
    end else begin
      SetLength(KeyBlock,HLen*2 + KLen*2 + BLen*2);
      TLS_PRF(Secret,SecretLen,ALabel,Seed,Pointer(KeyBlock)^,Length(KeyBlock));

      Move(KeyBlock[1],Key.client_write_MAC_secret,HLen);
      Move(KeyBlock[1 + HLen],Key.server_write_MAC_secret,HLen);
      Move(KeyBlock[1 + 2*HLen],Key.client_write_key,KLen);
      Move(KeyBlock[1 + 2*HLen + KLen],Key.server_write_key,KLen);
      if BLen > 0 then begin
        Move(KeyBlock[1 + 2*HLen + 2*KLen],Key.client_write_iv,BLen);
        Move(KeyBlock[1 + 2*HLen + 2*KLen + BLen],Key.server_write_iv,BLen);
      end;

      Key.kLen := KLen;
    end;
    Key.hLen := HLen;
    Key.bLen := BLen;
  finally
    ProtectClear(Pointer(KeyBlock)^,Length(KeyBlock));
  end;
end;

function TLS_Verify(const Params: TTLSSecurityParams;
                    const SSL2ClientHello: PSSL2ClientHello;
                    const Handshakes: TTLSHandshakes;
                    Entity: TConnectionEnd;
                    var Verify: PTLSHandshake): Integer;
var
  MD5:  TMD5;
  SHA1: TSHA1;
  H: PTLSHandshake;
  I, L: Integer;
  Hash: string[36];
begin
  MD5 := TMD5.Create(nil^,0);
  try
    SHA1 := TSHA1.Create(nil^,0);
    try
      if Assigned(SSL2ClientHello) then begin
        L := (SSL2ClientHello.msg_length[0] and $7F) shl 8 +
             SSL2ClientHello.msg_length[1];
        SHA1.HashData(SSL2ClientHello^.msg_type,L);
        MD5.HashData(SSL2ClientHello^.msg_type,L);
      end;
      for I := 0 to Length(Handshakes) - 1 do begin
        H := Handshakes[I];
        L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        MD5.HashData(H^,L + 4);
        SHA1.HashData(H^,L + 4);
      end;
      SetLength(Hash,36);
      MD5.Done(@Hash[1]);
      SHA1.Done(@Hash[17]);

      if Assigned(Verify) then
        ReallocMem(Verify,16)
      else
        GetMem(Verify,16);
      Verify.msg_type := finished;
      Verify.length[0] := 0;
      Verify.length[1] := 0;
      Verify.length[2] := 12;

      case Entity of
        ceServer:
          TLS_PRF(Params.master_secret,48,
                  'server finished',
                  Hash,
                  Verify.finished.verify_data,12);
        ceClient:
          TLS_PRF(Params.master_secret,48,
                  'client finished',
                  Hash,
                  Verify.finished.verify_data,12);
      end;
      Result := 16;
    finally
      SHA1.Free;
    end;
  finally
    MD5.Free;
  end;
end;

{$ELSE  SHA1_AND_MD5}
implementation
{$ENDIF SHA1_AND_MD5}
end.
