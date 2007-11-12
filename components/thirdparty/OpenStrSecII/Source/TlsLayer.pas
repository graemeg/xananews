{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSLayer Unit                                     }
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
unit TlsLayer;

interface
{$IFDEF SHA1_AND_MD5}

uses
  Classes, SecUtils, Asn1, MpPK, MpIF, MpDL, MpEC, Tls, TlsClass, MpX509,
  StreamSecII;

type
  TTLS_RecordLayer = class
  private
    FVersion: TProtocolVersion;
    FContext: TTLSSecurityParams;
    FPendCtx: TTLSSecurityParams;
    FCCSSent: Boolean;
    FCCSRcvd: Boolean;
    FIAmServer: Boolean;
    FSeqNoIn: Int64;
    FSeqNoOut: Int64;
    FKeys: TTLSKeys;
    FHashIn: THash;
    FHashOut: THash;
    FCipherIn: TCipher;
    FCipherOut: TCipher;
    FOnCompress: TCompressEvent;
    FOnDecompress: TDecompressEvent;
    FCipherText: TTLSEncrypted;
    FCTPos: Integer;
    FOnChangeCipherSpec: TNotifyEvent;
    procedure ChangeCipherSpec(Incoming: Boolean);
    procedure Compress(Src: PTLSPlainText; PTLen: Integer;
                       var Dst: PTLSCompressed; var CLen: Integer;
                       var Error: Integer);
    procedure Decompress(Src: PTLSCompressed; CLen: Integer;
                         var Dst: PTLSPlainText; var PTLen: Integer;
                         var Error: Integer);
    procedure Decrypt(Src: PTLSEncrypted; CTLen: Integer;
                      var Dst: PTLSCompressed; var CLen: Integer;
                      var Error: Integer);
    procedure Encrypt(Src: PTLSCompressed; CLen: Integer;
                      var Dst: PTLSEncrypted; var CTLen: Integer;
                      var Error: Integer);
    procedure SetOnChangeCipherSpec(const Value: TNotifyEvent);
  public
    constructor Create(AsServer: Boolean);
    destructor Destroy; override;
    function LoadFragmentFromStream(Src, CCS, Alert, Handshake, Data: TStream;
                                    var Error: Integer;
                                    var RecordType: TRecordType): Integer;
    function SaveFragmentToStream(Src, Dst: TStream;
                                  var Error: Integer;
                                  RecordType: TRecordType): Integer;
    property OnCompress: TCompressEvent read FOnCompress write FOnCompress;
    property OnDecompress: TDecompressEvent read FOnDecompress write FOnDecompress;
    property OnChangeCipherSpec: TNotifyEvent read FOnChangeCipherSpec write SetOnChangeCipherSpec;
  end;

  TTLS_ContentLayer = class;

  TTLS_HandshakeProtocol = class
  private
    FOwner: TTLS_ContentLayer;
    FVersion: TProtocolVersion;
    FPendCtx: TTLSSecurityParams;
    FIAmServer: Boolean;
    FPrevHandshake: Byte;
    FHandshakes: TTLSHandshakes;
    FSSL2ClientHello: PSSL2ClientHello;
    FKEA: TKeyExchangeAlgorithm;
    FSignAlg: TSignatureAlgorithm;
    FServerCert: PASN1Struct;
    FClientCert: PASN1Struct;
    FKEAIFPriv: IMPIFPrivateKey;
    FKEADLPriv: IMPDLPrivateKey;
    FKEAECPriv: IMPECPrivateKey;
    FKEAIFPubl: TIFPublicKey;
    FKEADLPubl: TDLPublicKey;
    FKEAECPubl: TECPublicKey;
    FClientCertMsg: PTLSHandshake;
    FClientCertMsgLen: Integer;
    procedure ClearHandshakes;
    function EncodeFinished(Response: TStream): Integer;
    function EncodeServerHelloReneg(var H: PTLSHandshake; var Len: Integer; const SID: ShortString): Boolean;
    function EncodeServerCertificate(var H: PTLSHandshake; var Len: Integer): Boolean;
    function EncodeServerKeyExch(var H: PTLSHandshake; var Len: Integer): Boolean;
    function EncodeServerCertReq(var H: PTLSHandshake; var Len: Integer): Boolean;
    function HandleHelloRequest(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleClientHello(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleServerHello(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleServerCert(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleServerKeyExch(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleServerCertReq(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleServerHelloDone(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleClientCert(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleClientKeyExch(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function InternalHandleClientKeyExch(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleClientCertVer(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleClientFinished(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    function HandleServerFinished(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
  public
    constructor Create(AsServer: Boolean; AOwner: TTLS_ContentLayer);
    destructor Destroy; override;
    function Add(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
  end;

  TTLS_ContentLayer = class(TCustomTLS_ContentLayer)
  private
    FRecordLayer: TTLS_RecordLayer;
    FCCS: TStream;
    FAlert: TStream;
    FHandshake: TStream;
    FHandshakeReadPos: Integer;
    FHandshakes: TTLS_HandshakeProtocol;
    FPrivateKeyRing: TStreamSecII;
    function EncodeAlert(Error: Integer; Dst: TStream; IsFatal: Boolean = True): Integer;
    function EncodeChangeCipherSpec(const Ctx: TTLSSecurityParams; Dst: TStream): Integer;
    procedure HandleAlert;
    function HandleHandshake(Response: TStream): Integer;
    function HandleSSL2ClientHello(Response: TStream): Integer;
    procedure SetPrivateKeyRing(const Value: TStreamSecII);
  protected
    procedure DoChangeCipherSpec(Sender: TObject);
    procedure DoCompress(Sender: TObject;
                         Src: PTLSPlainText; PTLen: Integer;
                         Method: Byte;
                         var Dst: PTLSCompressed; var CLen: Integer;
                         var Error: Integer);
    procedure DoDecompress(Sender: TObject;
                           Src: PTLSCompressed; CLen: Integer;
                           Method: Byte;
                           var Dst: PTLSPlainText; var PTLen: Integer;
                           var Error: Integer);
    function GetClientCertificate: PASN1Struct; override;
    function GetContext: PTLSSecurityParams; override;
    function GetEncrypted: Boolean; override;
    function GetServerCertificate: PASN1Struct; override;
  protected
    // *** TTLS_HandshakeProtocol Thread support ***
    // The methods placed here relies on Synchronize calls for thread safety.
    FKEA: TKeyExchangeAlgorithm;
    FSignAlg: TSignatureAlgorithm;
    FH: PTLSHandshake;
    FLen: Integer;
    FC: PASN1Struct;
    FFound: Boolean;
    FCertStatus: TCertStatusCodes;
    FResult: Integer;
    FResponse: TStream;
    function GetMyServerCertificate(KEA: TKeyExchangeAlgorithm;
                              SignAlg: TSignatureAlgorithm;
                              var H: PTLSHandshake;
                              var Len: Integer;
                              var C: PASN1Struct): Boolean;
    function GetServerKeyExch(KEA: TKeyExchangeAlgorithm;
                              SignAlg: TSignatureAlgorithm;
                              var H: PTLSHandshake;
                              var Len: Integer): Boolean;
    function HandleCert(H: PTLSHandshake; Len: Integer;
                        var Cert: PASN1Struct;
                        var Status: TCertStatusCodes): Boolean;
    function HandleClientKeyExch(var H: PTLSHandshake; var Len: Integer; Response: TStream): Integer;
    procedure InternalGetMyServerCertificate;
    procedure InternalGetServerKeyExch;
    procedure InternalHandleCert;
    procedure InternalHandleClientKeyExch;
  public
    constructor Create(AsServer: Boolean);
    destructor Destroy; override;
    function Accept(Src, Response: TStream): Integer; override;
    function Close(Response: TStream): Integer; override;
    function Connect(Response: TStream): Integer; override;
    function DecodeData(Src, Data, Response: TStream): Integer; override;
    function EncodeData(Src, Response: TStream): Integer; override;
    property PrivateKeyRing: TStreamSecII read FPrivateKeyRing write SetPrivateKeyRing;
  end;

implementation

uses
  SysUtils, MpArith, TlsConst, SsDes, SsArc4, SsRijndael, SsTwoFish, SsBlowFish,
  ReadStrm, MpYarrow, Pkix, X509Base, MpECArith, MpEC_NISTCurves, Ssl3, SsHelix,
  SsARC2;

{ TTLS_RecordLayer }

procedure TTLS_RecordLayer.ChangeCipherSpec(Incoming: Boolean);
var
  Keys: TTLSKeys;
  CC: TCipherClass;
  HC: THashClass;
begin
  FillChar(Keys,SizeOf(Keys),0);
  try
    if not (FCCSRcvd or FCCSSent) then begin
      Move(FPendCtx,FContext,SizeOf(FPendCtx));
      ProtectClear(FPendCtx,SizeOf(FPendCtx));
      if (FVersion.major = 3) and (FVersion.minor = 0) then
        SSL_KDF(FContext,Keys)
      else if (FVersion.major = 3) and (FVersion.minor = 1) then
        TLS_KDF(FContext,Keys);
    end else
      Move(FKeys,Keys,SizeOf(Keys));
    ProtectClear(FKeys,SizeOf(FKeys));
    case FContext.bulk_cipher_algorithm of
      bcaNull:   CC := nil;
      bcaRC2_40: CC := TARC2_CBC;
      bcaRC4,
      bcaRC4_40,
      bcaRC4_56: CC := TARC4;
      bcaDES,
      bcaDES40,
      bcaDES56,
      bca3DES:   CC := T3DES_CBC;
      bcaAES128,
      bcaAES192,
      bcaAES256: CC := TRijndael_CBC;
      bcaAES128_CTR,
      bcaAES192_CTR,
      bcaAES256_CTR: CC := TRijndael_CTR;
      bcaTwoFish128,
      bcaTwoFish192,
      bcaTwoFish256: CC := TTwoFish_CBC;
      bcaTwoFish128_CTR,
      bcaTwoFish192_CTR,
      bcaTwoFish256_CTR: CC := TTwoFish_CTR;
      bcaBlowFish128: CC := TBlowFish_CBC;
      bcaBlowFish128_CTR: CC := TBlowFish_CTR;
      bcaHelix: CC := THelix;
    else
      CC := nil;
    end;
    if FContext.bulk_cipher_algorithm = bcaHelix then
      HC := nil
    else case FContext.mac_algorithm of
      haMD5:  HC := TMD5;
      haSHA1: HC := TSHA1;
      {$IFDEF RIPEMD160}
      haRipeMD160: HC := TRipeMD160;
      {$ENDIF}
      {$IFDEF SHA256}
      haSHA256: HC := TSHA256;
      {$ENDIF}
      {$IFDEF SHA512}
      haSHA384: HC := TSHA384;
      haSHA512: HC := TSHA512;
      {$ENDIF}
    else
      HC := nil;
    end;
    if Assigned(HC) then begin
      if Incoming then begin
        FHashIn.Free;
        FHashIn := HC.Create(nil^,0);
      end else begin
        FHashOut.Free;
        FHashOut := HC.Create(nil^,0);
      end;
    end else begin
      if Incoming then begin
        FHashIn.Free;
        FHashIn := nil;
      end else begin
        FHashOut.Free;
        FHashOut := nil;
      end;
    end;
    if Incoming then begin
      FSeqNoIn := 0;
      FCCSRcvd := True;
      if Assigned(CC) then begin
        if FIAmServer then begin
          FCipherIn := CC.Create(Keys.client_write_key,Keys.kLen,0);
          if Keys.bLen > 1 then
            FCipherIn.SetVectorBuf(Keys.client_write_iv,Keys.bLen);
        end else begin
          FCipherIn := CC.Create(Keys.server_write_key,Keys.kLen,0);
          if Keys.bLen > 1 then
            FCipherIn.SetVectorBuf(Keys.server_write_iv,Keys.bLen);
        end;
      end;
    end else begin
      FSeqNoOut := 0;
      FCCSSent := True;
      if Assigned(CC) then begin
        if not FIAmServer then begin
          FCipherOut := CC.Create(Keys.client_write_key,Keys.kLen,0);
          if Keys.bLen > 1 then
            FCipherOut.SetVectorBuf(Keys.client_write_iv,Keys.bLen);
        end else begin
          FCipherOut := CC.Create(Keys.server_write_key,Keys.kLen,0);
          if Keys.bLen > 1 then
            FCipherOut.SetVectorBuf(Keys.server_write_iv,Keys.bLen);
        end;
      end;
    end;
    if FCCSSent and FCCSRcvd then begin
      FKeys.hLen := Keys.hLen;
      FKeys.client_write_MAC_secret := Keys.client_write_MAC_secret;
      FKeys.server_write_MAC_secret := Keys.server_write_MAC_secret;
      FKeys.kLen := Keys.kLen;
      FKeys.bLen := Keys.bLen;
    end else
      Move(Keys,FKeys,SizeOf(Keys));
  finally
    ProtectClear(Keys,SizeOf(Keys));
  end;
  if FCCSSent and FCCSRcvd and Assigned(FOnChangeCipherSpec) then
    FOnChangeCipherSpec(Self);
end;

procedure TTLS_RecordLayer.Compress(Src: PTLSPlainText; PTLen: Integer;
  var Dst: PTLSCompressed; var CLen: Integer; var Error: Integer);
begin
  if FContext.compression_algorithm <> cmNull then begin
    if Assigned(FOnCompress) then
      FOnCompress(Self,Src,PTLen,FContext.compression_algorithm,Dst,CLen,Error)
    else
      Error := internal_error;
  end;
end;

constructor TTLS_RecordLayer.Create(AsServer: Boolean);
begin
  FIAmServer := AsServer;
  FVersion.major := 3;
  FVersion.minor := 1;
  // This will prevent an attacker from establishing an
  // encrypted connection with a null key in case a
  // change cipher spec message is sent after an unsuccessful
  // handshake.
  RawRandom(FPendCtx.master_secret,48*8);
end;

procedure TTLS_RecordLayer.Decompress(Src: PTLSCompressed; CLen: Integer;
  var Dst: PTLSPlainText; var PTLen, Error: Integer);
begin
  if FContext.compression_algorithm <> cmNull then begin
    if Assigned(FOnDecompress) then
      FOnDecompress(Self,Src,CLen,FContext.compression_algorithm,Dst,PTLen,Error)
    else
      Error := internal_error;
  end;
end;

procedure TTLS_RecordLayer.Decrypt(Src: PTLSEncrypted; CTLen: Integer;
  var Dst: PTLSCompressed; var CLen, Error: Integer);
var
  PadLen: Integer;
  I: Integer;
  S: Int64;
  M: string;
begin
  if Assigned(FCipherIn) or Assigned(FHashIn) then begin
    if FCipherIn is TCipherMAC then begin
      if (CTLen shr 8 + Word(CTLen shl 8) <> Src.length) or
         (CTLen < 32) then
        Error := decode_error
      else begin
        CLen := CTLen - 32;
        Dst := Pointer(Src);
        Dst^.length := CLen shr 8 + CLen shl 8;

        FCipherIn.SetVectorBuf(Src.fragment[CLen],16);
        S := ByteSwap(FSeqNoIn);
        FCipherIn.Encrypt(S,8);

        TCipherMac(FCipherIn).EncryptToNil(Src^,5);
        FCipherIn.Decrypt(Src.fragment,CLen);
        M := TCipherMac(FCipherIn).Mac;
        if not CompareMem(@Src^.fragment[CLen+16],Pointer(M),16) then
          Error := bad_record_mac
        else
          Inc(FSeqNoIn);
      end;
    end else if Assigned(FCipherIn) xor Assigned(FHashIn) then
      Error := internal_error
    else begin
    {HH Changed 2003-feb-27:
     If the padding is invalid, the mac calculation is executed anyway. This
     will prevent attacks that use small difference in the response time of
     the peer in order to tell if the failure was caused by invalid padding
     or a bad mac. This attack shouldn't be a concern, since the bad_record_mac
     alert is fatal by default. We are just playing it safe. }
      if (CTLen shr 8 + Word(CTLen shl 8) <> Src.length) or
         (CTLen < FKeys.hLen) then
        Error := decode_error
      else begin
        FCipherIn.Decrypt(Src^.fragment,CTLen);
        if FCipherIn.Mode = cmCBC then begin
          PadLen := Src^.fragment[CTLen-1] + 1;
          if PadLen > CTLen then begin
            Error := bad_record_mac; {cf. comment in TLSConst.pas}
            PadLen := 0;
          end else
            for I := CTLen - PadLen to CTLen-1 do
              if Src^.fragment[I] <> Byte(PadLen - 1) then begin
                Error := bad_record_mac; {cf. comment in TLSConst.pas}
                PadLen := 0;
              end;
        end else
          PadLen := 0;
        CLen := CTLen - FKeys.hLen - PadLen;
        Dst := Pointer(Src);
//        ReallocMem(Dst,5 + CLen);
//        Move(Src^,Dst^,5 + CLen);
        Dst^.length := CLen shr 8 + CLen shl 8;

        M := StringOfChar(#0,FKeys.hLen);
        if (FVersion.major = 3) and (FVersion.minor = 0) then begin
          if FIAmServer then begin
            SSL3MacBegin(FKeys.client_write_MAC_secret,FKeys.hLen,FHashIn,False);
            S := ByteSwap(FSeqNoIn);
            FHashIn.HashData(S,8);
            FHashIn.HashData(Dst^.contentType,1);
            FHashIn.HashData(Dst^.length,2);
            FHashIn.HashData(Dst^.fragment,CLen);
            SSL3MacEnd(FKeys.client_write_MAC_secret,FKeys.hLen,FHashIn,
                       Pointer(M)^,FKeys.hLen);
          end else begin
            SSL3MacBegin(FKeys.server_write_MAC_secret,FKeys.hLen,FHashIn,False);
            S := ByteSwap(FSeqNoIn);
            FHashIn.HashData(S,8);
            FHashIn.HashData(Dst^.contentType,1);
            FHashIn.HashData(Dst^.length,2);
            FHashIn.HashData(Dst^.fragment,CLen);
            SSL3MacEnd(FKeys.server_write_MAC_secret,FKeys.hLen,FHashIn,
                       Pointer(M)^,FKeys.hLen);
          end;
        end else if (FVersion.major = 3) and (FVersion.minor = 1) then begin
          if FIAmServer then begin
            HMacBegin(FKeys.client_write_MAC_secret,FKeys.hLen,FHashIn);
            S := ByteSwap(FSeqNoIn);
            FHashIn.HashData(S,8);
            FHashIn.HashData(Dst^,5 + CLen);
            HMacEnd(FKeys.client_write_MAC_secret,FKeys.hLen,FHashIn,
                    Pointer(M)^,FKeys.hLen);
          end else begin
            HMacBegin(FKeys.server_write_MAC_secret,FKeys.hLen,FHashIn);
            S := ByteSwap(FSeqNoIn);
            FHashIn.HashData(S,8);
            FHashIn.HashData(Dst^,5 + CLen);
            HMacEnd(FKeys.server_write_MAC_secret,FKeys.hLen,FHashIn,
                            Pointer(M)^,FKeys.hLen);
          end;
        end;
        if not CompareMem(@Src^.fragment[CLen],Pointer(M),FKeys.hLen) then
          Error := bad_record_mac
        else
          Inc(FSeqNoIn);
      end;
    end;
  end;
end;

destructor TTLS_RecordLayer.Destroy;
begin
  ProtectClear(FContext,SizeOf(FContext));
  ProtectClear(FPendCtx,SizeOf(FPendCtx));
  ProtectClear(FKeys,SizeOf(FKeys));
  FCipherIn.Free;
  FCipherOut.Free;
  FHashOut.Free;
  FHashIn.Free;
  inherited;
end;

procedure TTLS_RecordLayer.Encrypt(Src: PTLSCompressed; CLen: Integer;
  var Dst: PTLSEncrypted; var CTLen: Integer; var Error: Integer);
var
  PadLen: Integer;
  I: Integer;
  S: Int64;
begin
  if Assigned(FCipherOut) or Assigned(FHashOut) then begin
    if FCipherOut is TCipherMAC then begin
      CTLen := CLen + 16 + 16;
      if LongInt(Dst) <> LongInt(Src) then begin
        ReallocMem(Dst,CTLen + 5);
        Move(Src^,Dst^,5 + CLen);
      end;
      RawRandom(Dst^.fragment[CLen],128);
      FCipherOut.SetVectorBuf(Dst^.fragment[CLen],16);
      S := ByteSwap(FSeqNoOut);
      Inc(FSeqNoOut);
      FCipherOut.Encrypt(S,8);
      TCipherMac(FCipherOut).EncryptToNil(Dst^,5);
      FCipherOut.Encrypt(Dst.fragment,CLen);
      TCipherMac(FCipherOut).GetMacBuf(Dst^.fragment[CLen+16],16);
      Dst^.length := CTLen shl 8 + CTLen shr 8;
    end else if Assigned(FCipherOut) and (FHashOut = nil) then
      Error := internal_error
    else begin
      if Assigned(FCipherOut) and (FCipherOut.Mode = cmCBC) then begin
//        if (FVersion.major = 3) and (FVersion.minor = 0) then
          PadLen := FKeys.bLen - ((CLen + FKeys.hLen) mod FKeys.bLen)
//        else
//          PadLen := 256 - ((CLen + FKeys.hLen) and $FF);
      end else
        PadLen := 0;
      CTLen := CLen + FKeys.hLen + PadLen;
      if LongInt(Dst) <> LongInt(Src) then begin
        ReallocMem(Dst,CTLen + 5);
        Move(Src^,Dst^,5 + CLen);
        FillChar(Dst^.fragment[CLen],FKeys.hLen,0);
      end;
      if (FVersion.major = 3) and (FVersion.minor = 0) then begin
        if FIAmServer then begin
          SSL3MacBegin(FKeys.server_write_MAC_secret,FKeys.hLen,FHashOut,False);
          S := ByteSwap(FSeqNoOut);
          FHashOut.HashData(S,8);
          FHashOut.HashData(Src^.contentType,1);
          FHashOut.HashData(Src^.length,2);
          FHashOut.HashData(Src^.fragment,CLen);
          SSL3MacEnd(FKeys.server_write_MAC_secret,FKeys.hLen,FHashOut,
                     Dst^.fragment[CLen],FKeys.hLen);
        end else begin
          SSL3MacBegin(FKeys.client_write_MAC_secret,FKeys.hLen,FHashOut,False);
          S := ByteSwap(FSeqNoOut);
          FHashOut.HashData(S,8);
          FHashOut.HashData(Src^.contentType,1);
          FHashOut.HashData(Src^.length,2);
          FHashOut.HashData(Src^.fragment,CLen);
          SSL3MacEnd(FKeys.client_write_MAC_secret,FKeys.hLen,FHashOut,
                     Dst^.fragment[CLen],FKeys.hLen);
        end;
      end else if (FVersion.major = 3) and (FVersion.minor = 1) then begin
        if FIAmServer then begin
          HMacBegin(FKeys.server_write_MAC_secret,FKeys.hLen,FHashOut);
          S := ByteSwap(FSeqNoOut);
          FHashOut.HashData(S,8);
          FHashOut.HashData(Dst^,5 + CLen);
          HMacEnd(FKeys.server_write_MAC_secret,FKeys.hLen,FHashOut,
                  Dst^.fragment[CLen],FKeys.hLen);
        end else begin
          HMacBegin(FKeys.client_write_MAC_secret,FKeys.hLen,FHashOut);
          S := ByteSwap(FSeqNoOut);
          FHashOut.HashData(S,8);
          FHashOut.HashData(Dst^,5 + CLen);
          HMacEnd(FKeys.client_write_MAC_secret,FKeys.hLen,FHashOut,
                  Dst^.fragment[CLen],FKeys.hLen);
        end;
      end;
      Inc(FSeqNoOut);
      for I := CLen + FKeys.hLen to CTLen-1 do
        Dst^.fragment[I] := Byte(PadLen - 1);
      FCipherOut.Encrypt(Dst^.fragment,CTLen);
      Dst^.length := CTLen shl 8 + CTLen shr 8;
    end;
  end;
end;

function TTLS_RecordLayer.LoadFragmentFromStream(Src, CCS, Alert, Handshake, Data: TStream;
  var Error: Integer; var RecordType: TRecordType): Integer;
var
  PTCount, CCount, CTCount: Integer;
  PT: PTLSPlainText;
  C: PTLSCompressed;
  CT: PTLSEncrypted;
  Buf: Pointer;
  CntType: Byte;
  W, L, HL: Word;
  vVersion: TProtocolVersion;
begin
  Result := 0;
  Error := -1;

  CT := @FCipherText;

  if RecordType = rtHandshake then begin
    W := 0;
    L := Src.Read(W,2);
    if (L = 2) and (W and $0080 > 0) then begin
      HL := W and $FF7F;
      HL := HL shr 8 or Word(HL shl 8);
      Result := Handshake.Write(W,2);
      if HL > 0 then
        Result := Result + Handshake.CopyFrom(Src,HL); // <- Read all or fail
      Error := -2;
      Exit;
    end;
    Buf := Ptr(LongInt(CT) + FCTPos);
    Move(W,Buf^,L);
    FCTPos :=  FCTPos + L;
//    Buf := Ptr(LongInt(CT) + FCTPos);
  end;
  if FCTPos < 5 then begin
    Buf := Ptr(LongInt(CT) + FCTPos);
    FCTPos := Src.Read(Buf^,5 - FCTPos) + FCTPos;
  end;
  Buf := Ptr(LongInt(CT) + FCTPos);
  if FCTPos = 0 then
    Error := 0
  else if FCTPos >= 5 then
  try
    CTCount := Word(CT^.length shl 8) + CT^.length shr 8;
    if CTCount > MaxCipherTextLength then begin
      Error := decode_error;
      FCTPos := 0;
    end else if (Assigned(FCipherIn) and
                 ((CT^.version.major <> FVersion.major) or
                  (CT^.version.minor <> FVersion.minor))) or
                (CT^.version.major <> 3) or
                (CT^.version.minor > 1)
    then begin
      Error := protocol_version;
      FCTPos := 0;
    end else begin
      FCTPos := Src.Read(Buf^,5 + CTCount - FCTPos) + FCTPos;
      if FCTPos = 5 + CTCount then begin
        FCTPos := 0;
        CCount := 0;
        C := nil;
        try
          Decrypt(CT,CTCount,C,CCount,Error);
          if Error < 0 then begin
            PTCount := 0;
            PT := nil;
            try
              if Assigned(C) then
                Decompress(C,CCount,PT,PTCount,Error);
              if Assigned(FCipherIn) then
                vVersion := FVersion;
              if Assigned(PT) then begin
                if FCipherIn = nil then
                  vVersion := PT^.version;
                Result := RecordLayerDecodeNext(PT,PTCount,
                                                vVersion,Error,CntType,
                                                CCS,Alert,Handshake,Data)
              end else if Assigned(C) then begin
                if FCipherIn = nil then
                  vVersion := PTLSPlainText(C)^.version;
                Result := RecordLayerDecodeNext(PTLSPlainText(C),CCount,
                                                vVersion,Error,CntType,
                                                CCS,Alert,Handshake,Data)
              end else begin
                if FCipherIn = nil then
                  vVersion := PTLSPlainText(CT)^.version;
                Result := RecordLayerDecodeNext(PTLSPlainText(CT),CTCount,
                                                vVersion,Error,CntType,
                                                CCS,Alert,Handshake,Data);
              end;
              if Error < 0 then
                case CntType of
                  change_cipher_spec: RecordType := rtChangeCipherSpec;
                  TLSConst.alert:     RecordType := rtAlert;
                  TLSConst.handshake: RecordType := rtHandshake;
                  application_data:
                    if Assigned(FCipherIn) then
                      RecordType := rtData
                    else
                      RecordType := rtUnknown;
                else
                  RecordType := rtUnknown;
                end;
            finally
              if Assigned(PT) then begin
                ProtectClear(PT^,5 + PTCount);
                FreeMem(PT);
              end;
            end;
          end;
        finally
          if Assigned(C) then begin
            ProtectClear(C^,5 + CCount);
//            FreeMem(C);
          end;
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TTLS_RecordLayer.SaveFragmentToStream(Src, Dst: TStream;
  var Error: Integer; RecordType: TRecordType): Integer;
var
  PTCount, CCount, CTCount: Integer;
  PT: PTLSPlainText;
  C: PTLSCompressed;
  CT: PTLSEncrypted;
begin
  Error := -1;
  Result := 0;
  PTCount := 0;
  PT := nil;
  try
    case RecordType of
      rtChangeCipherSpec: PTCount := RecordLayerEncodeNext(Src,FVersion,change_cipher_spec,PT) - 5;
      rtAlert:            PTCount := RecordLayerEncodeNext(Src,FVersion,alert,PT) - 5;
      rtHandshake:        PTCount := RecordLayerEncodeNext(Src,FVersion,handshake,PT) - 5;
      rtData:             PTCount := RecordLayerEncodeNext(Src,FVersion,application_data,PT) - 5;
    else
      Error := internal_error;
    end;
    if Error < 0 then begin
      C := nil;
      try
        CCount := 0;
        Compress(PT,PTCount,C,CCount,Error);
        if Error < 0 then begin
          CT := nil;
          try
            CTCount := 0;
            if Assigned(C) then
              Encrypt(C,CCount,CT,CTCount,Error)
            else if RecordType = rtData then
              Encrypt(PTLSCompressed(PT),PTCount,PTLSEncrypted(PT),PTCount,Error)
            else
              Encrypt(PTLSCompressed(PT),PTCount,CT,CTCount,Error);
            if Error < 0 then begin
              if Assigned(CT) then
                Result := Dst.Write(CT^,CTCount + 5)
              else if Assigned(C) then
                Result := Dst.Write(C^,CCount + 5)
              else
                Result := Dst.Write(PT^,PTCount + 5);
            end;
          finally
            if Assigned(CT) then begin
              ProtectClear(CT^,5 + CTCount);
              FreeMem(CT);
            end;
          end;
        end;
      finally
        if Assigned(C) then begin
          ProtectClear(C^,5 + CCount);
          FreeMem(C);
        end;
      end;
    end;
  finally
    ProtectClear(PT^,5 + PTCount);
    FreeMem(PT);
  end;
  if (Error < 0) and (RecordType = rtChangeCipherSpec) then
    ChangeCipherSpec(False);
end;

procedure TTLS_RecordLayer.SetOnChangeCipherSpec(
  const Value: TNotifyEvent);
begin
  FOnChangeCipherSpec := Value;
end;

{ TTLS_ContentLayer }

function TTLS_ContentLayer.Accept(Src, Response: TStream): Integer;
var
  Handshake: PTLSHandshake;
  MS: TSecureMemoryStream;
  RT: TRecordType;
  Error: Integer;
begin       
  Result := -1;
  _AddRef;
  try
    if not FRecordLayer.FIAmServer then
      Result := -1
    else try
      if Assigned(Src) then begin
        RT := rtHandshake;
        FRecordLayer.LoadFragmentFromStream(Src,nil,FAlert,FHandshake,nil,Error,RT);
        if Error = -2 then begin
          if FHandshakes = nil then
            FHandshakes := TTLS_HandshakeProtocol.Create(True,Self)
          else
            FHandshakes.ClearHandshakes;
          Result := HandleSSL2ClientHello(Response);
        end else if Error >= 0 then begin
          Result := EncodeAlert(Error,Response);
        end else begin
          if RT = rtAlert then begin
            Result := 0;
            HandleAlert;
          end else if RT = rtHandshake then begin
            if FHandshakes = nil then
              FHandshakes := TTLS_HandshakeProtocol.Create(True,Self)
            else
              FHandshakes.ClearHandshakes;
            Result := HandleHandshake(Response);
          end else
            raise Exception.Create('TTLS_ContentLayer.Accept: Unrecoverable error');
        end;
      end else begin
        GetMem(Handshake,4);
        try
          Handshake^.msg_type := hello_request;
          Handshake.length[0] := 0;
          Handshake.length[1] := 0;
          Handshake.length[2] := 0;
          MS := TSecureMemoryStream.Create;
          try
            Result := FRecordLayer.SaveFragmentToStream(MS,Response,Error,rtHandshake);
            if Error >= 0 then
              Result := Result + EncodeAlert(Error,Response);
          finally
            MS.Free;
          end;
        finally
          FreeMem(Handshake);
        end;
      end;
      if Error < 0 then
        FActive := True
      else
        FActive := False;
    except
      raise;
    end;
  finally
    _Release;
  end;
end;

function TTLS_ContentLayer.Close(Response: TStream): Integer;
begin
  _AddRef;
  try
    Result := EncodeAlert(close_notify,Response);
  finally
    _Release;
  end;
end;

function TTLS_ContentLayer.Connect(Response: TStream): Integer;
var
  Handshake: PTLSHandshake;
  Len: Integer;
begin
  _AddRef;
  try
    if FRecordLayer.FIAmServer then
      Result := -1
    else begin
      GetMem(Handshake,4);
      try
        Handshake^.msg_type := hello_request;
        Handshake.length[0] := 0;
        Handshake.length[1] := 0;
        Handshake.length[2] := 0;
        if FHandshakes = nil then
          FHandshakes := TTLS_HandshakeProtocol.Create(False,Self)
        else
          FHandshakes.ClearHandshakes;
        FActive := True;
        Result := FHandshakes.Add(Handshake,Len,Response);
      finally
        FreeMem(Handshake);
      end;
    end;
  finally
    _Release;
  end;
end;

constructor TTLS_ContentLayer.Create(AsServer: Boolean);
begin
  FRecordLayer := TTLS_RecordLayer.Create(AsServer);
  FRecordLayer.OnChangeCipherSpec := DoChangeCipherSpec;
  FRecordLayer.OnCompress := DoCompress;
  FRecordLayer.OnDecompress := DoDecompress;
  FCCS := TSecureMemoryStream.Create;
  FHandshake := TSecureMemoryStream.Create;
  FAlert := TSecureMemoryStream.Create;
  inherited Create;
end;

function TTLS_ContentLayer.DecodeData(Src, Data,
  Response: TStream): Integer;
var
  PLen, Error: Integer;
  RT: TRecordType;
begin
  _AddRef;
  try
    Result := 0;
    repeat
      RT := rtUnknown;
      PLen := FRecordLayer.LoadFragmentFromStream(Src,FCCS,FAlert,FHandshake,Data,Error,RT);
      if Error >= 0 then
        Result := EncodeAlert(Error,Response)
      else if PLen = 0 then
        if (Src.Position < Src.Size) and FActive then
          Continue;
      if FActive then
        case RT of
          rtData:
            Result := Result + PLen;
          rtChangeCipherSpec:
            begin
              FRecordLayer.FPendCtx := FHandshakes.FPendCtx;
              FRecordLayer.ChangeCipherSpec(True);
            end;
          rtAlert:
            HandleAlert;
          rtHandshake:
            Result := Result + HandleHandshake(Response);
        end;
    until (Src.Position = Src.Size) or not FActive;
  finally
    _Release;
  end;
end;

destructor TTLS_ContentLayer.Destroy;
begin
  if RefCount > 0 then
    raise Exception.Create('TTLS_ContentLayer.Destroy: Object is still in use');
  DoDestroy;
  FRecordLayer.Free;
  FCCS.Free;
  FAlert.Free;
  FHandshakes.Free;
  FHandshake.Free;
  inherited;
end;

procedure TTLS_ContentLayer.DoChangeCipherSpec(Sender: TObject);
begin
  if Assigned(FOnChangeCipherSpec) then
    FOnChangeCipherSpec(Self);
end;

procedure TTLS_ContentLayer.DoCompress(Sender: TObject;
  Src: PTLSPlainText; PTLen: Integer; Method: Byte;
  var Dst: PTLSCompressed; var CLen, Error: Integer);
begin
  if Assigned(FOnCompress) then
    FOnCompress(Self,Src,PTLen,Method,Dst,CLen,Error);
end;

procedure TTLS_ContentLayer.DoDecompress(Sender: TObject;
  Src: PTLSCompressed; CLen: Integer; Method: Byte; var Dst: PTLSPlainText;
  var PTLen, Error: Integer);
begin
  if Assigned(FOnDecompress) then
    FOnDecompress(Self,Src,CLen,Method,Dst,PTLen,Error);
end;

function TTLS_ContentLayer.EncodeAlert(Error: Integer; Dst: TStream;
  IsFatal: Boolean): Integer;
var
  Alert: TTLSAlert;
  RS: TReadStream;
begin
  if Assigned(FOnOutgoingAlert) then
    FOnOutgoingAlert(Self,IsFatal,Error);
  if Error < 0 then
    Result := 0 // ignore
  else begin
    if not (Error and $FF) in [user_canceled,no_renegotiation,certificate_unknown,
                               certificate_expired,certificate_revoked,
                               unsupported_certificate,bad_certificate] then
      IsFatal := True;
    if IsFatal then begin
      FActive := False;
      Alert.alertLevel := fatal;
    end else
      Alert.alertLevel := warning;
    Alert.alertDescription := Error and $FF;
    SetLastAlertCode(Error);
    RS := TReadStream.Create(Alert,2);
    try
      Result := FRecordLayer.SaveFragmentToStream(RS,Dst,Error,rtAlert);
      // Subsequent errors are ignored here.
    finally
      RS.Free;
    end;
  end;
end;

function TTLS_ContentLayer.EncodeChangeCipherSpec(
  const Ctx: TTLSSecurityParams; Dst: TStream): Integer;
var
  Error: Integer;
  RS: TReadStream;
  B: Byte;
begin
  B := 1;
  RS := TReadStream.Create(B,1);
  try
    FRecordLayer.FPendCtx := FHandshakes.FPendCtx;
    Result := FRecordLayer.SaveFragmentToStream(RS,Dst,Error,rtChangeCipherSpec);
    if Error >= 0 then
      Result := Result + EncodeAlert(Error,Dst);
    if FActive then begin
      Result := Result + FHandshakes.EncodeFinished(Dst);
    end;
  finally
    RS.Free;
  end;
end;

function TTLS_ContentLayer.EncodeData(Src, Response: TStream): Integer;
var
  Error: Integer;
begin
  _AddRef;
  try
    if FRecordLayer.FCipherOut = nil then
      Result := -1
    else begin
      Result := FRecordLayer.SaveFragmentToStream(Src,Response,Error,rtData);
      if Error >= 0 then
        Result := Result + EncodeAlert(Error,Response);
    end;
  finally
    _Release;
  end;
end;

function TTLS_ContentLayer.GetClientCertificate: PASN1Struct;
begin
  Result := FHandshakes.FClientCert;
end;

function TTLS_ContentLayer.GetContext: PTLSSecurityParams;
begin
  Result := @FRecordLayer.FContext;
end;

function TTLS_ContentLayer.GetEncrypted: Boolean;
begin
  Result := Assigned(FRecordLayer.FCipherIn) and
            Assigned(FRecordLayer.FCipherOut);
end;

function TTLS_ContentLayer.GetMyServerCertificate(
  KEA: TKeyExchangeAlgorithm; SignAlg: TSignatureAlgorithm;
  var H: PTLSHandshake; var Len: Integer; var C: PASN1Struct): Boolean;
begin
  FKEA := KEA;
  FSignAlg := SignAlg;
  FH := H;
  FLen := Len;
  FC := C;
  FFound := False;
  InternalGetMyServerCertificate;
  H := FH;
  Len := FLen;
  C := FC;
  Result := FFound;
end;

function TTLS_ContentLayer.GetServerCertificate: PASN1Struct;
begin
  Result := FHandshakes.FServerCert;
end;

function TTLS_ContentLayer.GetServerKeyExch(KEA: TKeyExchangeAlgorithm;
  SignAlg: TSignatureAlgorithm; var H: PTLSHandshake;
  var Len: Integer): Boolean;
begin
  FKEA := KEA;
  FSignAlg := SignAlg;
  FH := H;
  FLen := Len;
  FFound := False;
  InternalGetServerKeyExch;
  H := FH;
  Len := FLen;
  Result := FFound;
end;

procedure TTLS_ContentLayer.HandleAlert;
var
  A: TTLSAlert;
  IsFatal: Boolean;
begin
  FAlert.Position := 0;
  FAlert.Read(A,2);
  FAlert.Size := 0;
  IsFatal := A.alertLevel = fatal;
  if IsFatal then
    FActive := False;
  SetLastAlertCode(A.alertDescription);
  if Assigned(FOnIncomingAlert) then
    FOnIncomingAlert(Self,IsFatal,A.alertDescription);
end;

function TTLS_ContentLayer.HandleHandshake(Response: TStream): Integer;
var
  Len: Integer;
  H: PTLSHandshake;
begin
  Result := 0;
  FHandshake.Position := FHandshakeReadPos;
  try
    GetMem(H,4);
    try
      while FHandshake.Read(H^,4) = 4 do begin
        if H^.msg_type and $80 > 0 then begin
          Result := EncodeAlert(decode_error,Response);
          Break;
        end;
        Len := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        ReallocMem(H,Len+4);
        if FHandShake.Read(Ptr(LongInt(H) + 4)^,Len) = Len then begin
          FHandshakeReadPos := FHandshake.Position;

          Result := FHandshakes.Add(H,Len,Response);
          if not FActive then
            Break;

          H := nil;
          GetMem(H,4);
        end;
      end;
    finally
      FreeMem(H);
    end;
  finally
    FHandshake.Seek(0,soFromEnd);
  end;
end;

function TTLS_ContentLayer.HandleCert(H: PTLSHandshake; Len: Integer;
  var Cert: PASN1Struct;
  var Status: TCertStatusCodes): Boolean;
var
  CertsIntf: IThreadIdentifier;
begin
  FH := H;
  FLen := Len;
  FC := Cert;
  FCertStatus := Status;
  FFound := False;
  CertsIntf := FCollectedCerts;
  InternalHandleCert;
  Cert := FC;
  Status := FCertStatus;
  Result := FFound;
end;      

function TTLS_ContentLayer.HandleClientKeyExch(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
//var
//  PrivateKeyRingIntf: IThreadIdentifier;
begin
  { The InternalHandleClientKeyExch method involves a lot of (implicit) calls to
    MPRealloc. It is important the method is executed in the same thread as the
    TStreamSecII component. }
//  PrivateKeyRingIntf := FPrivateKeyRing;
//  if PrivateKeyRingIntf.CheckThreadID then
    Result := FHandshakes.InternalHandleClientKeyExch(H,Len,Response)
{  else begin
    FH := H;
    FLen := Len;
    FResponse := Response;
    Synchronize(InternalHandleClientKeyExch);
    Result := FResult;
  end;}
end;

function TTLS_ContentLayer.HandleSSL2ClientHello(
  Response: TStream): Integer;   
var
  Len: Integer;
  H: PSSL2ClientHello;
  PH: PTLSHandshake;
begin
  FHandshake.Position := 0;
  Len := FHandshake.Size;
  FHandshakeReadPos := Len;
  GetMem(FHandshakes.FSSL2ClientHello,Len);
  H := FHandshakes.FSSL2ClientHello;
  FHandshake.Read(H^,Len);

  PH := nil;
  Result := FHandshakes.HandleClientHello(PH,Len,Response);

  FHandshake.Seek(0,soFromEnd);
end;

procedure TTLS_ContentLayer.InternalGetMyServerCertificate;
var
  C: PASN1Struct;
  OK: Boolean;
  Idx, KS: Integer;

  function FindCert(const SignatureAlgorithms: array of string;
                    const PKAlgorithms: array of string;
                    const KeyUsage: TKeyUsage;
                    var Idx: Integer;
                    var Cert: PASN1Struct): Boolean;
  begin
    if (URIToCheck = '') and (IPToCheck = '') and (DNSNameToCheck = '') then
      Result := FMyCerts.FindCert(SignatureAlgorithms,PKAlgorithms,KeyUsage,Idx,Cert)
    else
      Result := FMyCerts.FindServerCert(SignatureAlgorithms,PKAlgorithms,KeyUsage,
                                        URIToCheck,IPToCheck,DNSNameToCheck,
                                        Idx,Cert);
  end;

begin
  Idx := 0;
  C := nil;
  OK := False;
  if Assigned(OnGetCertificate) then begin
    OnGetCertificate(Self,FKEA,FSignAlg,FH,FLen,C,OK);
    if OK then begin
      FFound := True;
      FC := C;
      Exit;
    end;
  end;
  if FSignAlg = saRSA then begin
    if FKEA = keaRSA then begin
      OK := FindCert([{$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF}
                      sha1WithRSAEncryption],
                     [rsaEncryption],[KeyEncipherment],Idx,C)
    end else if FKEA = keaRSA_Export then begin
      repeat
        OK := FindCert([{$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF}
                        sha1WithRSAEncryption],
                       [rsaEncryption],[KeyEncipherment],Idx,C);
        if OK then begin
          OK := ExtractSubjectPublicKeySize(C^,KS) = E_OK;
          if OK and (KS <= 512) then
            Break;
        end;
      until not OK;
    end else if FKEA = keaRSA1024 then
      repeat
        OK := FindCert([{$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF}
                        sha1WithRSAEncryption],
                       [rsaEncryption],[KeyEncipherment],Idx,C);
        if OK then begin
          OK := ExtractSubjectPublicKeySize(C^,KS) = E_OK;
          if OK and (KS > 512) and (KS <= 1024) then
            Break;
        end;
      until not OK
    else if FKEA = keaDH then begin
      OK := FindCert([{$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF}
                        sha1WithRSAEncryption],
                     [dhPublicNumber],[KeyAgreement],Idx,C);
    end else if FKEA in [keaDHE,keaDHE_Export,keaDHE1024,keaECDHE] then begin
      OK := FindCert([],[rsaEncryption],[digitalSignature],Idx,C);
    end else if FKEA = keaECDH then begin
      OK := FindCert([{$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF}
                        sha1WithRSAEncryption],
                     [id_ecPublicKey],[KeyAgreement],Idx,C);
    end;
  end else if FSignAlg = saDSS then begin
    if FKEA = keaRSA then begin
      OK := FindCert([id_dsa_with_sha1],[rsaEncryption],[KeyEncipherment],Idx,C);
    end else if FKEA = keaRSA_Export then begin
      repeat
        OK := FindCert([id_dsa_with_sha1],
                       [rsaEncryption],[KeyEncipherment],Idx,C);
        if OK then begin
          OK := ExtractSubjectPublicKeySize(C^,KS) = E_OK;
          if OK and (KS <= 512) then
            Break;
        end;
      until not OK;
    end else if FKEA = keaRSA1024 then
      repeat
        OK := FindCert([id_dsa_with_sha1],
                       [rsaEncryption],[KeyEncipherment],Idx,C);
        if OK then begin
          OK := ExtractSubjectPublicKeySize(C^,KS) = E_OK;
          if OK and (KS > 512) and (KS <= 1024) then
            Break;
        end;
      until not OK
    else if FKEA = keaDH then
      OK := FindCert([id_dsa_with_sha1],[dhPublicNumber],[KeyAgreement],Idx,C)
    else if FKEA in [keaDHE,keaDHE_Export,keaDHE1024,keaECDHE] then
      OK := FindCert([],[id_dsa],[digitalSignature],Idx,C);
  end else if FSignAlg = saECDSA then begin
    if FKEA = keaECDH then
      OK := FindCert([ecdsa_with_sha1],[id_ecPublicKey],[KeyAgreement],Idx,C)
    else if FKEA = keaECDHE then                                             
      OK := FindCert([ecdsa_with_sha1],[id_ecPublicKey],[digitalSignature],Idx,C);
  end;
  FFound := OK or (FSignAlg = saAnon);
  if OK then begin
    FC := C;
    FMyCerts.ExportChainToTLS(FH,FLen,Idx);
  end;
end;

procedure TTLS_ContentLayer.InternalGetServerKeyExch;
var
  Res, KS: Integer;
  PKI: string;
  Index: Integer;
  Dummy: Boolean;
  ISKE: TInternalServerKeyExchange;
  Obj: TObject;
  KEAPriv, Priv: IMPPrivateKey;
  IFPriv: IMPIFPrivateKey;
  IFPubl: TIFPublicKey;
  DLPriv: IMPDLPrivateKey;
  DLPubl: TDLPublicKey;
  ECPriv: IMPECPrivateKey;
  ECPubl: TECPublicKey;
  Curve: ObjectIdentifier;
begin
  FillChar(ISKE,SizeOf(ISKE),0);
  FFound := False;
  if Assigned(FHandshakes.FServerCert) then
    Res := ExtractSubjectPublicKeySize(FHandshakes.FServerCert^,KS)
  else
    Res := E_NOT_SUPPORTED;
  if (FKEA = keaRSA_Export) and
     (Res = E_OK) and (KS > 512) then begin
    PKI := 'RSA key 512';
    Index := 0;
    Obj := FPrivateKeyRing.FindCreatePrivateKey(Pointer(PKI),Length(PKI),Dummy,Index);
    Supports(Obj,IMPPrivateKey,KEAPriv);
    if Assigned(KEAPriv) then begin
      FHandshakes.FKEAIFPriv := TMPIFPrivateKey(KEAPriv.Clone);
      FFound := True;
      FillChar(IFPubl,SizeOf(IFPubl),0);
      try
        FHandshakes.FKEAIFPriv.RSAPublicKey(IFPubl);
        Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
        if Assigned(Priv) then begin
          Priv.QueryInterface(IMPIFPrivateKey,IFPriv);
          ISKE.Signed.input.SA := FSignAlg;
          ISKE.KEA := FKEA;
          ISKE.rsaParams.rsa_modulus := IFPubl.N;
          ISKE.rsaParams.rsa_exponent := IFPubl.E;
          FLen := TLS_EncodeServerKeyExchange(ISKE,
                                              FHandshakes.FPendCtx.client_random,
                                              FHandshakes.FPendCtx.server_random,
                                              IFPriv,FH);
        end;
      finally
        DisposeIFPublicKey(IFPubl);
      end;
    end;
  end else if (FKEA = keaRSA1024) and
              (Res = E_OK) and (KS > 1024) then begin
    PKI := 'RSA key 1024';
    Index := 0;             
    Obj := FPrivateKeyRing.FindCreatePrivateKey(Pointer(PKI),Length(PKI),Dummy,Index);
    Supports(Obj,IMPPrivateKey,KEAPriv);
    if Assigned(KEAPriv) then begin
      FHandshakes.FKEAIFPriv := TMPIFPrivateKey(KEAPriv.Clone);
      FFound := True;
      FillChar(IFPubl,SizeOf(IFPubl),0);
      try
        FHandshakes.FKEAIFPriv.RSAPublicKey(IFPubl);
        Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
        if Assigned(Priv) then begin
          Priv.QueryInterface(IMPIFPrivateKey,IFPriv);
          ISKE.Signed.input.SA := FSignAlg;
          ISKE.KEA := FKEA;
          ISKE.rsaParams.rsa_modulus := IFPubl.N;
          ISKE.rsaParams.rsa_exponent := IFPubl.E;
          FLen := TLS_EncodeServerKeyExchange(ISKE,
                                              FHandshakes.FPendCtx.client_random,
                                              FHandshakes.FPendCtx.server_random,
                                              IFPriv,FH);
        end;
      finally
        DisposeIFPublicKey(IFPubl);
      end;
    end;
  end else if (FKEA in [keaDHE,keaDHE_Export,keaDHE1024]) or
              ((FKEA = keaDH) and (FSignAlg = saAnon)) then begin
    case FKEA of
      keaDHE_Export,
      keaDHE1024:    PKI := 'DH key 1024 160';
      keaDH,
      keaDHE:        PKI := Format('DH key %d %d',
                                   [FDHEphemeralKeySize,
                                    FDHEphemeralQSize]);
    else
      PKI := '';
    end;
    Index := 0;
    Obj := FPrivateKeyRing.FindCreatePrivateKey(Pointer(PKI),Length(PKI),Dummy,Index);
    Supports(Obj,IMPPrivateKey,KEAPriv);
    if Assigned(KEAPriv) then begin
      FHandshakes.FKEADLPriv := TMPDLPrivateKey(KEAPriv.Clone);
      FFound := True;
      FillChar(DLPubl,SizeOf(DLPubl),0);
      try
        FHandshakes.FKEADLPriv.DLPublicKey(DLPubl);
        ISKE.Signed.input.SA := FSignAlg;
        ISKE.KEA := FKEA;
        ISKE.dhParams.dh_p := DLPubl.Params.P;
        ISKE.dhParams.dh_g := DLPubl.Params.G;
        ISKE.dhParams.dh_Ys := DLPubl.Y;
        if FSignAlg = saRSA then begin
          Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
          if Assigned(Priv) then begin
            Priv.QueryInterface(IMPIFPrivateKey,IFPriv);
            FLen := TLS_EncodeServerKeyExchange(ISKE,
                                                FHandshakes.FPendCtx.client_random,
                                                FHandshakes.FPendCtx.server_random,
                                                IFPriv,FH);
          end else
            FFound := False;
        end else if FSignAlg = saDSS then begin
          Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
          if Assigned(Priv) then begin
            Priv.QueryInterface(IMPDLPrivateKey,DLPriv);
            FLen := TLS_EncodeServerKeyExchange(ISKE,
                                                FHandshakes.FPendCtx.client_random,
                                                FHandshakes.FPendCtx.server_random,
                                                DLPriv,FH);
          end else
            FFound := False;
        end else if FSignAlg = saECDSA then begin
          Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
          if Assigned(Priv) then begin
            Priv.QueryInterface(IMPECPrivateKey,ECPriv);
            FLen := TLS_EncodeServerKeyExchange(ISKE,
                                                FHandshakes.FPendCtx.client_random,
                                                FHandshakes.FPendCtx.server_random,
                                                ECPriv,FH);
          end else
            FFound := False;
        end else if FSignAlg = saAnon then
          FLen := TLS_EncodeServerKeyExchange(ISKE,FH)
        else
          FFound := False;
        ISKE.dhParams.dh_p := nil;
        ISKE.dhParams.dh_g := nil;
        ISKE.dhParams.dh_Ys := nil;
        ISKE.Signed.signature.SetLength(0);
        ISKE.Signed.signature := nil;
      finally
        if Assigned(ISKE.Signed.signature) then
          ISKE.Signed.signature := nil;
        DisposeDLPublicKey(DLPubl);
      end;
    end;
  end else if ((FKEA = keaECDH) and (FSignAlg = saAnon)) or (FKEA = keaECDHE) then begin
    FFound := True;
    if FECDHEphemeralKeySize = 0 then begin
      if FHandshakes.FPendCtx.bulk_cipher_algorithm = bcaDES then
        FECDHEphemeralKeySize := 192
      else if FHandshakes.FPendCtx.bulk_cipher_algorithm in [bca3DES,bcaRC4] then
        FECDHEphemeralKeySize := 224
      else
        case FHandshakes.FPendCtx.bulk_cipher_algorithm of
          bcaAES128, bcaTwoFish128, bcaBlowFish128,
          bcaAES128_CTR, bcaTwoFish128_CTR, bcaBlowFish128_CTR:
            FECDHEphemeralKeySize := 256;
          bcaAES192, bcaTwoFish192,
          bcaAES192_CTR, bcaTwoFish192_CTR:
            FECDHEphemeralKeySize := 384;
        else
          FECDHEphemeralKeySize := 521;
        end;
    end;
    case FECDHEphemeralKeySize of
      192: Curve := prime192v1;
      224: Curve := prime224;
      256: Curve := prime256v1;
      384: Curve := prime384;
      521: Curve := prime521;
    else
      FFound := False;
    end;
    if FFound then begin
      FHandshakes.FKEAECPriv := TMPECPrivateKey.CreateNew(id_ecPublicKey,
                                                          Curve,
                                                          nil,nil,haSHA1,nil);
      FillChar(ECPubl,SizeOf(ECPubl),0);
      try
        FHandshakes.FKEAECPriv.ECPublicKey(ECPubl);
        ISKE.KEA := FKEA;
        ISKE.Signed.input.SA := FSignAlg;
        ISKE.ecdhParams.curve_params.curve_type := ctNamedCurve;
        case FECDHEphemeralKeySize of
          192: ISKE.ecdhParams.curve_params.curveName := 19;
          224: ISKE.ecdhParams.curve_params.curveName := 21;
          256: ISKE.ecdhParams.curve_params.curveName := 23;
          384: ISKE.ecdhParams.curve_params.curveName := 24;
          521: ISKE.ecdhParams.curve_params.curveName := 25;
        end;
        ECCopy2(ECPubl.W,ISKE.ecdhParams.public_key);
        if FSignAlg = saAnon then
          FLen := TLS_EncodeServerKeyExchange(ISKE,FH)
        else begin
          if FSignAlg = saRSA then begin
            Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
            if Assigned(Priv) then begin         
              Priv.QueryInterface(IMPIFPrivateKey,IFPriv);
              FLen := TLS_EncodeServerKeyExchange(ISKE,
                                                  FHandshakes.FPendCtx.client_random,
                                                  FHandshakes.FPendCtx.server_random,
                                                  IFPriv,FH);
            end else
              FFound := False;
          end else if FSignAlg = saDSS then begin
            Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
            if Assigned(Priv) then begin                  
              Priv.QueryInterface(IMPDLPrivateKey,DLPriv);
              FLen := TLS_EncodeServerKeyExchange(ISKE,
                                                  FHandshakes.FPendCtx.client_random,
                                                  FHandshakes.FPendCtx.server_random,
                                                  DLPriv,FH);
            end else
              FFound := False;
          end else if FSignAlg = saECDSA then begin
            Priv := FPrivateKeyRing.FindCreatePrivateKeyTLS(FHandshakes.FServerCert^);
            if Assigned(Priv) then begin
              Priv.QueryInterface(IMPECPrivateKey,ECPriv);
              FLen := TLS_EncodeServerKeyExchange(ISKE,
                                                  FHandshakes.FPendCtx.client_random,
                                                  FHandshakes.FPendCtx.server_random,
                                                  ECPriv,FH);
            end else
              FFound := False;
          end else
            FFound := False;
        end;
        ECDealloc(ISKE.ecdhParams.curve_params.primeCurve);
        ECDealloc(ISKE.ecdhParams.public_key);
      finally
        DisposeECPublicKey(ECPubl);
      end;
    end;
  end else
    FFound := True; // NULL message
end;

procedure TTLS_ContentLayer.InternalHandleCert;
begin
  if Assigned(FCollectedCerts) then
    FFound := FCollectedCerts.ImportChainFromTLS(@FH.Certificate,FLen,FC,FCertStatus);
end;

procedure TTLS_ContentLayer.SetPrivateKeyRing(const Value: TStreamSecII);
begin
  FPrivateKeyRing := Value;
end;

procedure TTLS_ContentLayer.InternalHandleClientKeyExch;
begin
  FResult := FHandshakes.InternalHandleClientKeyExch(FH,FLen,FResponse);
end;

{ TTLS_HandshakeProtocol }

function TTLS_HandshakeProtocol.Add(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;

  function OutOfOrder(MsgType: Byte): Boolean;
  begin
    Result :=
      (FIAmServer and
       (MsgType in [hello_request,
                    server_hello,
                    server_key_exchange,
                    certificate_request,
                    server_hello_done])) or
      (not FIAmServer and
       (MsgType in [client_hello,
                    client_key_exchange,
                    certificate_verify])) or
      ((MsgType = finished) and not FOwner.FRecordLayer.FCCSRcvd);
    if Result and FIAmServer and (MsgType = hello_request) then begin
    // Changed 2003-Mar-04: If AcceptHelloRequest then server sessions may be
    // automatically transformed to client sessions if the first message is a
    // Hello Request instead of a Client Hello.
      Result := FOwner.AcceptHelloRequest and not FOwner.Encrypted;
      if Result then begin
        FIAmServer := False;
        FOwner.FRecordLayer.FIAmServer := False;
      end;
    end else if not Result then begin
      if FIAmServer then
        Result := ((MsgType = client_hello) and (FPrevHandshake >= MsgType)) or
                  ((MsgType = certificate) and (FPrevHandshake <> server_hello_done)) or
                  ((MsgType = client_key_exchange) and
                   not (FPrevHandshake in [server_hello_done,certificate])) or
                  ((MsgType = certificate_verify) and (FPrevHandshake <> client_key_exchange)) or
                  ((MsgType = finished) and not (FPrevHandshake in [finished,
                                                                    certificate_verify,
                                                                    client_key_exchange]))
      else
        Result := ((MsgType = hello_request) and (FPrevHandshake > 0)) or
                  ((MsgType = server_hello) and (FPrevHandshake <> client_hello)) or
                  ((MsgType = certificate) and (FPrevHandshake <> server_hello)) or
                  ((MsgType = server_key_exchange) and not (FPrevHandshake in [server_hello,
                                                                               certificate])) or
                  ((MsgType = certificate_request) and not (FPrevHandshake in [certificate,
                                                                               server_key_exchange])) or
                  ((MsgType = server_hello_done) and not (FPrevHandshake in [server_hello,
                                                                             certificate,
                                                                             server_key_exchange,
                                                                             certificate_request])) or
                  ((MsgType = finished) and not (FOwner.FRecordLayer.FCCSRcvd and
                                                 (FPrevHandshake = server_hello)) and
                                            not (FPrevHandshake in [finished,
                                                                    certificate,
                                                                    certificate_verify,
                                                                    client_key_exchange]));

    end;
  end;

begin
  Result := -1;
  if Assigned(H) and OutOfOrder(H^.msg_type) then begin
    Result := FOwner.EncodeAlert(unexpected_message,Response);
    if not FOwner.FActive then
      Exit;
  end else begin
    if Assigned(H) then begin
      SetLength(FHandshakes,Length(FHandshakes)+1);
      FHandshakes[Length(FHandshakes)-1] := H;
      FPrevHandshake := H^.msg_type;
    end;

    case H^.msg_type of
      hello_request:       Result := HandleHelloRequest(H,Len,Response);
      client_hello:        Result := HandleClientHello(H,Len,Response);
      server_hello:        Result := HandleServerHello(H,Len,Response);
      certificate:         if FIAmServer then
                             Result := HandleClientCert(H,Len,Response)
                           else
                             Result := HandleServerCert(H,Len,Response);
      server_key_exchange: Result := HandleServerKeyExch(H,Len,Response);
      certificate_request: Result := HandleServerCertReq(H,Len,Response);
      server_hello_done:   Result := HandleServerHelloDone(H,Len,Response);
      client_key_exchange: Result := HandleClientKeyExch(H,Len,Response);
      certificate_verify:  Result := HandleClientCertVer(H,Len,Response);
      finished:            if FIAmServer then
                             Result := HandleClientFinished(H,Len,Response)
                           else
                             Result := HandleServerFinished(H,Len,Response);
    else
      Exit;
    end;
    H := nil;
  end;
end;

procedure TTLS_HandshakeProtocol.ClearHandshakes;
var
  I: Integer;
begin
  FPrevHandshake := 0;
  if Assigned(FSSL2ClientHello) then begin
    FreeMem(FSSL2ClientHello);
    FSSL2ClientHello := nil;
  end;
  for I := 0 to Length(FHandshakes) - 1 do
    FreeMem(FHandshakes[I]);
  SetLength(FHandshakes,0);
  FClientCertMsg := nil;
  FClientCertMsgLen := 0;
  FKEADLPriv := nil;
  FKEAECPriv := nil;
  FKEAIFPriv := nil;
  DisposeDLPublicKey(FKEADLPubl);
  DisposeECPublicKey(FKEAECPubl);
  DisposeIFPublicKey(FKEAIFPubl);
  FOwner.FHandshake.Size := 0;
end;

constructor TTLS_HandshakeProtocol.Create(AsServer: Boolean; AOwner: TTLS_ContentLayer);
begin
  FIAmServer := AsServer;
  FOwner := AOwner;
end;

destructor TTLS_HandshakeProtocol.Destroy;
begin
  ProtectClear(FPendCtx,SizeOf(FPendCtx));
  ClearHandshakes;
  inherited;
end;

function TTLS_HandshakeProtocol.EncodeFinished(Response: TStream): Integer;
var
  H: PTLSHandshake;
  MS: TSecureMemoryStream;
  Len, Error: Integer;
begin
  H := nil;
  if (FVersion.major = 3) and (FVersion.minor = 0) then begin
    if FIAmServer then
      Len := SSL_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceServer,H)
    else
      Len := SSL_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceClient,H);
  end else if (FVersion.major = 3) and (FVersion.minor = 1) then begin
    if FIAmServer then
      Len := TLS_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceServer,H)
    else
      Len := TLS_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceClient,H);
  end else begin
    Assert(False);
    Len := 0;
  end;
  SetLength(FHandshakes,Length(FHandshakes)+1);
  FHandshakes[Length(FHandshakes)-1] := H;
  FPrevHandshake := H^.msg_type;
  MS := TSecureMemoryStream.Create;
  try
    MS.Write(H^,Len);
    MS.Position := 0;
    Result := FOwner.FRecordLayer.SaveFragmentToStream(MS,Response,Error,rtHandshake);
    if Error >= 0 then
      Result := Result + FOwner.EncodeAlert(Error,Response);
  finally
    MS.Free;
  end;
end;

function TTLS_HandshakeProtocol.EncodeServerCertificate(
  var H: PTLSHandshake; var Len: Integer): Boolean;
var
  C: PASN1Struct;
begin
  C := nil;
  Result := FOwner.GetMyServerCertificate(FKEA,FSignAlg,H,Len,C);
  if Result then
    FServerCert := C;
end;

function TTLS_HandshakeProtocol.EncodeServerCertReq(var H: PTLSHandshake;
  var Len: Integer): Boolean;
var
  ICR: TInternalCertificateRequest;
  I: Integer;
  Cert: TASN1Struct;
  OK: Boolean;
begin
  Result := True;
  if not FOwner.RequestClientAuth then
    Len := 0
  else begin
    if Assigned(FOwner.FOnGetCertReq) then begin
      OK := False;
      FOwner.FOnGetCertReq(Self,FKEA,H,Len,OK);
      if OK then Exit;
    end;
    if FOwner.RequestClientAuth and (FSignAlg <> saAnon) then begin
      if FKEA = keaDH then begin
        SetLength(ICR.certificate_types,4);
        ICR.certificate_types[0] := 1;
        ICR.certificate_types[1] := 2;
        ICR.certificate_types[2] := 3;
        ICR.certificate_types[3] := 4;
      end else if FKEA = keaECDH then begin
        SetLength(ICR.certificate_types,3);
        ICR.certificate_types[0] := 5;
        ICR.certificate_types[1] := 6;
        ICR.certificate_types[2] := 7;
      end else if (FKEA = keaECDHE) and (FSignAlg = saECDSA) then begin
        SetLength(ICR.certificate_types,1);
        ICR.certificate_types[0] := 5;
      end else begin
        SetLength(ICR.certificate_types,2);
        ICR.certificate_types[0] := 1;
        ICR.certificate_types[1] := 2;
      end;
      SetLength(ICR.certificate_authorities,FOwner.FCACerts.Count);
      for I := 0 to FOwner.FCACerts.Count - 1 do begin
        Cert := FOwner.FCACerts.Certs[I];
        ExtractSubject(Cert,ICR.certificate_authorities[I]);
      end;
      Len := TLS_EncodeCertificateRequest(ICR,H);
    end;
  end;
end;

function TTLS_HandshakeProtocol.EncodeServerHelloReneg(
  var H: PTLSHandshake; var Len: Integer; const SID: ShortString): Boolean;
var
  MasterSecret: ShortString;
begin
  Result := False;
  if (SID <> '') and (SID = FOwner.FSessionID) then begin
    Result := FOwner.Encrypted and
      (FPendCtx.bulk_cipher_algorithm = FOwner.FRecordLayer.FContext.bulk_cipher_algorithm) and
      (FPendCtx.mac_algorithm = FOwner.FRecordLayer.FContext.mac_algorithm) and
      (FPendCtx.compression_algorithm = FOwner.FRecordLayer.FContext.compression_algorithm);
    if Result then
      Move(FOwner.FRecordLayer.FContext.master_secret,FPendCtx.master_secret,48);
  end else if Assigned(FOwner.FOnRenegotiate) then begin
    MasterSecret := '';
    FOwner.FOnRenegotiate(FOwner,SID,MasterSecret);
    Result := Length(MasterSecret) = 48;
    if Result then begin
      Move(MasterSecret[1],FPendCtx.master_secret,48);
      ProtectClear(MasterSecret[1],48);
    end;
  end;
end;

function TTLS_HandshakeProtocol.EncodeServerKeyExch(var H: PTLSHandshake;
  var Len: Integer): Boolean;
begin
  Result := FOwner.GetServerKeyExch(FKEA,FSignAlg,H,Len);
end;

function TTLS_HandshakeProtocol.HandleClientCert(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  Status: TCertStatusCodes;
  Res: Boolean;
begin
  Result := 0;
  try
    Res := FOwner.HandleCert(H,Len,FClientCert,Status);
    if not Res then
      if (Status <> []) or FOwner.FRequireClientAuth then
        Result := FOwner.EncodeAlert(bad_certificate,Response);
  except
    Result := FOwner.EncodeAlert(internal_error,Response);
  end;
end;

function TTLS_HandshakeProtocol.HandleClientCertVer(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  PK: TX509PublicKey;
  IFPubl: TIFPublicKey;
  DLPubl: TDLPublicKey;
  ECPubl: TECPublicKey;
begin
  Result := 0;
  if not Assigned(FClientCert) then begin
    Result := FOwner.EncodeAlert(unexpected_message,Response);
    Exit;
  end;
  PK := nil;
  try
    if ExtractSubjectPublicKey(FClientCert^,PK) = E_OK then begin
      case PK.KeyAlgorithmType of
        katDL:
          begin
            FillChar(DLPubl,SizeOf(DLPubl),0);
            try
              PK.AsDLKey(DLPubl);
              if ((FVersion.minor > 0) and not TLS_DecodeCertificateVerify(@H^.certificate_verify,Len,FSSL2ClientHello,FHandshakes,DLPubl)) or
                 ((Fversion.minor = 0) and not SSL_DecodeCertificateVerify(@H^.certificate_verify,Len,FPendCtx.master_secret,FSSL2ClientHello,FHandshakes,DLPubl)) then
                Result := FOwner.EncodeAlert(decrypt_error,Response);
            finally
              DisposeDLPublicKey(DLPubl);
            end;
          end;
        katIF:
          begin
            FillChar(IFPubl,SizeOf(IFPubl),0);
            try
              PK.AsIFKey(IFPubl);
              if ((FVersion.minor > 0) and not TLS_DecodeCertificateVerify(@H^.certificate_verify,Len,FSSL2ClientHello,FHandshakes,IFPubl)) or
                 ((FVersion.minor = 0) and not SSL_DecodeCertificateVerify(@H^.certificate_verify,Len,FPendCtx.master_secret,FSSL2ClientHello,FHandshakes,IFPubl)) then
                Result := FOwner.EncodeAlert(decrypt_error,Response);
            finally
              DisposeIFPublicKey(IFPubl);
            end;
          end;
        katEC:
          begin
            FillChar(ECPubl,SizeOf(ECPubl),0);
            try
              PK.AsECKey(ECPubl);
              if not TLS_DecodeCertificateVerify(@H^.certificate_verify,Len,FSSL2ClientHello,FHandshakes,ECPubl) then
                Result := FOwner.EncodeAlert(decrypt_error,Response);
            finally
              DisposeECPublicKey(ECPubl);
            end;
          end;
      else
        Result := FOwner.EncodeAlert(unsupported_certificate,Response);
      end
    end else
      Result := FOwner.EncodeAlert(certificate_unknown,Response);
  finally
    PK.Free;
  end;
end;

function TTLS_HandshakeProtocol.HandleClientFinished(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  tH: PTLSHandshake;
begin
  Result := 0;
  if Len < 12 then begin
    Result := FOwner.EncodeAlert(illegal_parameter,Response);
    Exit;
  end;
  tH := nil;
  try
    SetLength(FHandshakes,Length(FHandshakes)-1);
    if (FVersion.major = 3) and (FVersion.minor = 0) then begin
      SSL_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceClient,tH);
      if not CompareMem(tH,H,40) then
        Result := Result + FOwner.EncodeAlert(decrypt_error,Response);
    end else if (FVersion.major = 3) and (FVersion.minor = 1) then begin
      TLS_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceClient,tH);
      if not CompareMem(tH,H,16) then
        Result := Result + FOwner.EncodeAlert(decrypt_error,Response);
    end;
    SetLength(FHandshakes,Length(FHandshakes)+1);
    FHandshakes[Length(FHandshakes)-1] := H;
    H := nil;

  finally
    ProtectClear(tH^,4 + 12);
    FreeMem(tH);
  end;

  if FOwner.FActive and not FOwner.FRecordLayer.FCCSSent then
    Result := Result + FOwner.EncodeChangeCipherSpec(FPendCtx,Response);

  if FOwner.FRecordLayer.FCCSRcvd and FOwner.FRecordLayer.FCCSSent then begin
    FOwner.FRecordLayer.FCCSSent := False;
    FOwner.FRecordLayer.FCCSRcvd := False;
  end;
end;

function TTLS_HandshakeProtocol.HandleClientHello(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  I, J, Error: Integer;
  T: LongWord;
  ICH: TInternalClientHello;
  ISH: TInternalServerHello;
  OK: Boolean;
  MS: TSecureMemoryStream;
begin
  if (H = nil) and Assigned(FSSL2ClientHello) then begin
    if not TLS_DecodeClientHelloSSL2(FSSL2ClientHello,Len,ICH) then begin
      Result := FOwner.EncodeAlert(illegal_parameter or client_hello_format,Response);
      if not FOwner.FActive then
        Exit;
    end;
  end else if not TLS_DecodeClientHello(@H^.client_hello,Len,ICH) then begin
    Result := FOwner.EncodeAlert(illegal_parameter or client_hello_format,Response);
    if not FOwner.FActive then
      Exit;
  end;
  if (ICH.client_version.major = 3) and (ICH.client_version.minor = 0) then
    ISH.server_version := ICH.client_version
  else begin
    ISH.server_version.major := 3;
    ISH.server_version.minor := 1;
  end;
  FVersion := ISH.server_version;
  FPendCtx.client_random := ICH.random.AsString;
  ISH.compression_method := cmNull;
  if Assigned(FOwner.FOnSelectCompression) and (ICH.compression_count > 1) then
    FOwner.FOnSelectCompression(FOwner,ICH.compression_methods,ISH.compression_method);
  OK := (ISH.compression_method = cmNull) and (ICH.compression_count = 0);
  for I := 0 to ICH.compression_count - 1 do begin
    OK := ICH.compression_methods[I] = ISH.compression_method;
    if OK then
      Break;
  end;
  if not OK then begin
    Result := FOwner.EncodeAlert(handshake_failure or unknown_compression,Response);
    if not FOwner.FActive then
      Exit;
  end else begin
    OK := False;
    for I := 0 to ICH.cipher_suite_count - 1 do begin
      for J := 0 to Length(FOwner.FAcceptedCiphers) - 1 do begin
        OK := (ICH.cipher_suites[I,0] = FOwner.FAcceptedCiphers[J,0]) and
              (ICH.cipher_suites[I,1] = FOwner.FAcceptedCiphers[J,1]) and
              ((FVersion.minor > 0) or (ICH.cipher_suites[I,1] <= $1B));
        if OK then begin
          ISH.cipher_suite := FOwner.FAcceptedCiphers[J];
          Break;
        end;
      end;
      if OK then
        Break;
    end;
    if not OK then begin
      Result := FOwner.EncodeAlert(handshake_failure or no_common_cipher_suite,Response);
      if not FOwner.FActive then
        Exit;
    end else begin
      OK := TLS_DecodeCipherSuite(ISH.cipher_suite,
                                  FPendCtx.bulk_cipher_algorithm,
                                  FPendCtx.mac_algorithm,
                                  FKEA,
                                  FSignAlg);
      if not OK then
        OK := TLS_DecodeXCipherSuite(ISH.cipher_suite,
                                     FPendCtx.bulk_cipher_algorithm,
                                     FPendCtx.mac_algorithm,
                                     FKEA,
                                     FSignAlg);
      if not OK then begin
        Result := FOwner.EncodeAlert(handshake_failure or cipher_suite_unknown,Response);
        if not FOwner.FActive then
          Exit;
      end else begin
        if ICH.session_id <> '' then begin
          if EncodeServerHelloReneg(H,Len,ICH.session_id) then begin
            MS := TSecureMemoryStream.Create;
            try
              T := ByteSwap(UnixTime);
              FOwner.FSessionID := ICH.session_id;
              ISH.session_id := ICH.session_id;
              TLS_NewRandom(ISH.random);
              FPendCtx.server_random := ISH.random.AsString;
              H := nil;
              Len := TLS_EncodeServerHello(ISH,H);
              SetLength(FHandshakes,Length(FHandshakes)+1);
              FHandshakes[Length(FHandshakes)-1] := H;
              FPrevHandshake := H^.msg_type;           
              MS.Write(H^,Len);
              Result := 0;

              FOwner.FRecordLayer.FVersion := FVersion;
              MS.Position := 0;
              repeat
                Result := Result + FOwner.FRecordLayer.SaveFragmentToStream(MS,Response,Error,rtHandshake);
                if Error >= 0 then begin
                  Result := Result + FOwner.EncodeAlert(Error,Response);
                  if not FOwner.FActive then
                    Break;
                end;
              until MS.Position = MS.Size;
            finally
              MS.Free;
            end;
            FOwner.FActive := Error < 0;
            if FOwner.FActive then
              Result := Result + FOwner.EncodeChangeCipherSpec(FPendCtx,Response);
            Exit;
          end;
        end;

        T := ByteSwap(UnixTime);
        SetLength(ISH.session_id,32);
        HMac(nil^,0,T,4,haSHA256,ISH.session_id[1],32);
        FOwner.FSessionID := ISH.session_id;
        TLS_NewRandom(ISH.random);
        FPendCtx.server_random := ISH.random.AsString;
        H := nil;
        Len := TLS_EncodeServerHello(ISH,H);
        SetLength(FHandshakes,Length(FHandshakes)+1);
        FHandshakes[Length(FHandshakes)-1] := H;
        FPrevHandshake := H^.msg_type;
        MS := TSecureMemoryStream.Create;
        try
          MS.Write(H^,Len);

          H := nil;
          Len := 0;
          if not EncodeServerCertificate(H,Len) then
            Result := FOwner.EncodeAlert(handshake_failure or server_cert_not_found,Response)
          else begin
            if Len > 0 then begin
              SetLength(FHandshakes,Length(FHandshakes)+1);
              FHandshakes[Length(FHandshakes)-1] := H;
              FPrevHandshake := H^.msg_type;
              MS.Write(H^,Len);
            end;

            H := nil;
            Len := 0;
            if not EncodeServerKeyExch(H,Len) then
              Result := FOwner.EncodeAlert(handshake_failure or server_key_not_found,Response)
            else begin
              if Len > 0 then begin
                SetLength(FHandshakes,Length(FHandshakes)+1);
                FHandshakes[Length(FHandshakes)-1] := H;
                FPrevHandshake := H^.msg_type;
                MS.Write(H^,Len);
              end;

              H := nil;
              Len := 0;
              if not EncodeServerCertReq(H,Len) then
                Result := FOwner.EncodeAlert(handshake_failure,Response)
              else begin
                if Len > 0 then begin
                  SetLength(FHandshakes,Length(FHandshakes)+1);
                  FHandshakes[Length(FHandshakes)-1] := H;
                  FPrevHandshake := H^.msg_type;
                  MS.Write(H^,Len);
                end;

                H := nil;
                ReallocMem(H,4);
                H.msg_type := server_hello_done;
                H.length[0] := 0;
                H.length[1] := 0;
                H.length[2] := 0;
                SetLength(FHandshakes,Length(FHandshakes)+1);
                FHandshakes[Length(FHandshakes)-1] := H;
                FPrevHandshake := H^.msg_type;
                MS.Write(H^,4);

                FOwner.FRecordLayer.FVersion := FVersion;
                Result := 0;
                MS.Position := 0;
                repeat
                  Result := Result + FOwner.FRecordLayer.SaveFragmentToStream(MS,Response,Error,rtHandshake);
                  if Error >= 0 then begin
                    Result := Result + FOwner.EncodeAlert(Error,Response);
                    Break;
                  end;
                until MS.Position = MS.Size;
              end;
            end;
          end;
        finally
          MS.Free;
        end;
      end;
    end;
  end;
end;      

function TTLS_HandshakeProtocol.HandleClientKeyExch(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
begin
//  Result := FOwner.HandleClientKeyExch(H,Len,Response);
  Result := InternalHandleClientKeyExch(H,Len,Response);
end;

function TTLS_HandshakeProtocol.HandleHelloRequest(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  ICH: TInternalClientHello;
  MasterSecret: ShortString;
  Error: Integer;
  MS: TSecureMemoryStream;
begin
  ClearHandshakes;
  H := nil;
  TLS_NewRandom(ICH.random);
  Move(ICH.random,FPendCtx.client_random,SizeOf(ICH.random));
  ICH.client_version.major := 3;
  ICH.client_version.minor := 1;
  if FOwner.FSessionID <> '' then begin
//    SetLength(ICH.cipher_suites,1);
//    TLS_EncodeCipherSuite(FOwner.FRecordLayer.FContext.bulk_cipher_algorithm,
//                          FOwner.FRecordLayer.FContext.mac_algorithm,
//                          FKEA,FSignAlg,
//                          ICH.cipher_suites[0]);
    ICH.session_id := FOwner.SessionID;
//    Move(FOwner.FRecordLayer.FContext.master_secret,FPendCtx.master_secret,48);
  end else begin
    ICH.session_id := '';
  end;
  ICH.cipher_suites := FOwner.FCipherPreference;
  ICH.cipher_suite_count := Length(FOwner.FCipherPreference);
  ICH.compression_methods := FOwner.FCompressionPreference;
  ICH.compression_count := Length(FOwner.FCompressionPreference);

  if Assigned(FOwner.FOnRenegotiate) then
    FOwner.FOnRenegotiate(FOwner,ICH.session_id,MasterSecret);
  if Length(MasterSecret) = 48 then begin
    ICH.session_id := FOwner.SessionID;
    Move(MasterSecret[1],FPendCtx.master_secret,48);
    ProtectClear(MasterSecret[1],48);
  end else
    FOwner.SessionID := '';
  Len := TLS_EncodeClientHello(ICH,H);

  SetLength(FHandshakes,1);
  FHandshakes[0] := H;
  FPrevHandshake := H^.msg_type;

  MS := TSecureMemoryStream.Create;
  try
    MS.Write(H^,Len);
    MS.Position := 0;
    Result := FOwner.FRecordLayer.SaveFragmentToStream(MS,Response,Error,rtHandshake);
    if Error >= 0 then
      Result := Result + FOwner.EncodeAlert(Error,Response);
  finally
    MS.Free;
  end;
end;

function TTLS_HandshakeProtocol.HandleServerCert(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  Status: TCertStatusCodes;
  Ext: TX509Extension;
  I: Integer;
  KeyUsage: TKeyUsage;
  CheckCert: PASN1Struct;
  Res: Integer;
  NetscapeKeyUsage: TNetscapeKeyUsage;
begin
  Status := [];
  if not FOwner.HandleCert(H,Len,
                           FServerCert,
                           Status) then begin
    if crcInvalidSignature in Status then
      Result := FOwner.EncodeAlert(bad_certificate,Response)
    else if crcExpired in Status then
      Result := FOwner.EncodeAlert(certificate_expired,Response)
    else if crcRevoked in Status then
      Result := FOwner.EncodeAlert(certificate_revoked,Response)
    else if crcCANotTrusted in Status then
      Result := FOwner.EncodeAlert(unknown_ca,Response)
    else
      Result := FOwner.EncodeAlert(certificate_unknown,Response);
  end else if FSignAlg <> saAnon then begin
    Result := 0;
    if FIAmServer then
      CheckCert := FClientCert
    else
      CheckCert := FServerCert;
    if Assigned(CheckCert) then begin
      if FOwner.IPToCheck <> '' then
        if not X509Base.CheckIP(CheckCert^,FOwner.IPToCheck) then
          Result := FOwner.EncodeAlert(certificate_unknown or server_cert_ip,Response,False);
      if FOwner.URIToCheck <> '' then
        if not X509Base.CheckURI(CheckCert^,FOwner.URIToCheck) then
          Result := Result + FOwner.EncodeAlert(certificate_unknown or server_cert_uri,Response,False);
      if FOwner.DNSNameToCheck <> '' then
        if not X509Base.CheckDNSName(CheckCert^,FOwner.DNSNameToCheck) then
          Result := Result + FOwner.EncodeAlert(certificate_unknown or server_cert_name,Response,False);
    end;
    if (Status <> [crcTrusted]) and FOwner.Active then begin
      Ext := nil;
      try
        Res := ExtractNamedExtension(FServerCert^,id_ce_extKeyUsage,Ext);
        if Res = E_OK then begin
          if Ext.extnValue.Constructed and (Ext.extnValue.Tag = V_ASN1_SEQUENCE) then begin
            Res := -1;
            for I := 0 to Ext.extnValue.ItemCount - 1 do
              if Ext.extnValue.Items[I]^.ContentAsOID = id_kp_serverAuth then begin
                Res := E_OK;
                Break;
              end else if Ext.extnValue.Items[I]^.ContentAsOID = netscape + '.4.1' then
                Res := MaxInt;
            if Res < E_OK then
              Result := Result + FOwner.EncodeAlert(bad_certificate or wrong_netscape_key_usage,Response);
          end else
            Result := Result + FOwner.EncodeAlert(bad_certificate or extension_format,Response);
        end;
        if (Res = E_OK) or (Res = MaxInt) then begin
          if Res = MaxInt then
            Res := -1;
          if ExtractNamedExtension(FServerCert^,netscape_cert_type,Ext) = E_OK then begin
            Res := -1;
            if (Ext.extnValue.Tag = V_ASN1_BIT_STRING) and
               (not Ext.extnValue.Constructed) and
               (Ext.extnValue.Length = 2) then begin
              Move(Ext.extnValue.Content[1],NetscapeKeyUsage,1);
              if nkuSslServer in NetscapeKeyUsage then
                Res := E_OK;
            end;
          end;
          if Res < 0 then
            Result := Result + FOwner.EncodeAlert(bad_certificate or wrong_netscape_cert_type,Response);
        end;
        if (FVersion.minor > 0) and (Res <> E_NOT_SUPPORTED) and (Res <> E_OK) then
          Result := Result + FOwner.EncodeAlert(bad_certificate,Response);
        try
          KeyUsage := ExtractKeyUsage(FServerCert^);
          if FKEA in [keaRSA,keaRSA_Export,keaRSA1024] then begin
            if not (keyEncipherment in KeyUsage) then
              Result := Result + FOwner.EncodeAlert(unsupported_certificate or wrong_key_usage,Response);
          end else if FKEA in [keaDH,keaECDH] then begin
            if not (keyAgreement in KeyUsage) then
              Result := Result + FOwner.EncodeAlert(unsupported_certificate or wrong_key_usage,Response);
          end else begin
            if not (digitalSignature in KeyUsage) then
              Result := Result + FOwner.EncodeAlert(unsupported_certificate or wrong_key_usage,Response);
          end;
        except
          Result := Result + FOwner.EncodeAlert(bad_certificate,Response);
        end;
        if not FOwner.Active then begin
          FOwner.CollectedCerts.RemoveCertificate(FServerCert^);
          FServerCert := nil;
        end;
      finally
        Ext.Free;
      end;
    end;
  end else
    Result := 0;
end;

function TTLS_HandshakeProtocol.HandleServerCertReq(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  ICR: TInternalCertificateRequest;
  I: Integer;
  OK: Boolean;
begin
  Result := 0;
  if FSignAlg = saAnon then
    Result := FOwner.EncodeAlert(unexpected_message,Response)
  else if TLS_DecodeCertificateRequest(@H^.certificate_request,Len,ICR) then begin
    OK := False;
    if FKEA in [keaDHE,keaDHE_Export,keaDHE1024] then begin
      OK := True;
      for I := Low(ICR.certificate_types) to High(ICR.certificate_types) do
        OK := OK and (ICR.certificate_types[I] in [1,2]);
    end else if FKEA = keaDH then begin
      OK := True;
      for I := Low(ICR.certificate_types) to High(ICR.certificate_types) do
        OK := OK and (ICR.certificate_types[I] in [1,2,3,4]);
    end else if FKEA in [keaRSA,keaRSA_Export,keaRSA1024] then begin
      OK := True;
      for I := Low(ICR.certificate_types) to High(ICR.certificate_types) do
        OK := OK and (ICR.certificate_types[I] in [1,2]);
    end else if FKEA = keaECDH then begin
      OK := True;
      for I := Low(ICR.certificate_types) to High(ICR.certificate_types) do
        OK := OK and (ICR.certificate_types[I] in [5,6,7]);
    end else if (FKEA = keaECDHE) and (FSignAlg in [saRSA,saDSS]) then begin
      OK := True;
      for I := Low(ICR.certificate_types) to High(ICR.certificate_types) do
        OK := OK and (ICR.certificate_types[I] in [1,2,5]);
    end else if FKEA = keaECDHE then begin
      OK := True;
      for I := Low(ICR.certificate_types) to High(ICR.certificate_types) do
        OK := OK and (ICR.certificate_types[I] in [1,5]);
    end;
    if not OK then
      Result := FOwner.EncodeAlert(handshake_failure or client_cert_not_found,Response)
    else
      if not FOwner.FMyCerts.ExportChainToTLS(FClientCertMsg,FClientCertMsgLen,
                                              FClientCert,
                                              ICR.certificate_types,
                                              ICR.certificate_authorities) then
        FClientCert := nil;
  end else
    Result := FOwner.EncodeAlert(decode_error,Response);
end;

function TTLS_HandshakeProtocol.HandleServerFinished(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  tH: PTLSHandshake;
begin
  if Len < 12 then begin
    Result := FOwner.EncodeAlert(illegal_parameter,Response);
    Exit;
  end;
  tH := nil;
  try
    SetLength(FHandshakes,Length(FHandshakes)-1);
    if (FVersion.major = 3) and (FVersion.minor = 0) then
      SSL_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceServer,tH)
    else if (FVersion.major = 3) and (FVersion.minor = 1) then
      TLS_Verify(FPendCtx,FSSL2ClientHello,FHandshakes,ceServer,tH);
    if CompareMem(tH,H,16) then
      Result := 0
    else
      Result := FOwner.EncodeAlert(decrypt_error,Response);
    SetLength(FHandshakes,Length(FHandshakes)+1);
    FHandshakes[Length(FHandshakes)-1] := H;
  finally
    ProtectClear(tH^,4 + 12);
    FreeMem(tH);
  end;

  if FOwner.FActive and
     FOwner.FRecordLayer.FCCSRcvd and
     not FOwner.FRecordLayer.FCCSSent then
    Result := Result + FOwner.EncodeChangeCipherSpec(FPendCtx,Response);

  if FOwner.FRecordLayer.FCCSRcvd and FOwner.FRecordLayer.FCCSSent then begin
    FOwner.FRecordLayer.FCCSSent := False;
    FOwner.FRecordLayer.FCCSRcvd := False;
  end;     
end;

function TTLS_HandshakeProtocol.HandleServerHello(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  ISH: TInternalServerHello;
begin
  if not TLS_DecodeServerHello(@H^.server_hello,Len,ISH) then
    Result := FOwner.EncodeAlert(decode_error,Response)
  else begin
    Result := 0;
    FVersion := ISH.server_version;
    FPendCtx.entity := ceClient;
    Move(ISH.random,FPendCtx.server_random,SizeOf(ISH.random));
    FPendCtx.compression_algorithm := ISH.compression_method;
    FOwner.FSessionID := ISH.session_id;
    if not TLS_DecodeCipherSuite(ISH.cipher_suite,
                                 FPendCtx.bulk_cipher_algorithm,
                                 FPendCtx.mac_algorithm,
                                 FKEA,FSignAlg) then
      TLS_DecodeXCipherSuite(ISH.cipher_suite,
                             FPendCtx.bulk_cipher_algorithm,
                             FPendCtx.mac_algorithm,
                             FKEA,FSignAlg);
    SetLength(FOwner.FCipherPreference,1);
    FOwner.FCipherPreference[0] := ISH.cipher_suite;
    SetLength(FOwner.FCompressionPreference,1);
    FOwner.FCompressionPreference[0] := ISH.compression_method;
  end;
end;

function TTLS_HandshakeProtocol.HandleServerHelloDone(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  MS: TSecureMemoryStream;
  PK, PKE: TX509PublicKey;
  Error: Integer;
  KEAPriv: IMPPrivateKey;
begin
  Result := 0;
  H := nil;
  MS := TSecureMemoryStream.Create;
  try
    if Assigned(FClientCertMsg) then begin
      MS.Write(FClientCertMsg^,FClientCertMsgLen);

      SetLength(FHandshakes,Length(FHandshakes)+1);
      FHandshakes[Length(FHandshakes)-1] := FClientCertMsg;
      FPrevHandshake := FClientCertMsg^.msg_type;
      H := nil;

      PK := nil;
      try
        if ExtractSubjectPublicKey(FClientCert^,PK) = E_OK then begin
          if FKEA in [keaRSA,keaRSA_Export,keaRSA1024] then begin
            if Assigned(FKEAIFPubl.N) then begin
              PKE := TX509PublicKey.CreateFromIFKey(FKEAIFPubl,False,haSHA1,haSHA1,'');
              try
                if (FVersion.major = 3) and (FVersion.minor = 0) then
                  Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
                else if (FVersion.major = 3) and (FVersion.minor = 1) then
                  Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
              finally
                PKE.Free;
              end;
            end else begin
              PKE := nil;
              try
                if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then begin
                  if (FVersion.major = 3) and (FVersion.minor = 0) then
                    Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
                  else if (FVersion.major = 3) and (FVersion.minor = 1) then
                    Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
                end;
              finally
                PKE.Free;
              end;
            end;

            if H = nil then
              Result := FOwner.EncodeAlert(internal_error,Response)
            else begin
              MS.Write(H^,Len);
              SetLength(FHandshakes,Length(FHandshakes)+1);
              FHandshakes[Length(FHandshakes)-1] := H;
              FPrevHandshake := H^.msg_type;
              H := nil;

              if PK.KeyAlgorithmType = katIF then begin
                KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FClientCert^);
                if Assigned(KEAPriv) then begin
                  KEAPriv.QueryInterface(IMPIFPrivateKey,FKEAIFPriv);
                  if FVersion.minor = 0 then
                    Len := SSL_EncodeCertificateVerify(FPendCtx.master_secret,nil,FHandshakes,FKEAIFPriv,H)
                  else
                    Len := TLS_EncodeCertificateVerify(nil,FHandshakes,FKEAIFPriv,H)
                end;
              end else if PK.KeyAlgorithmType = katDL then begin
                KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FClientCert^);
                if Assigned(KEAPriv) then begin
                  KEAPriv.QueryInterface(IMPDLPrivateKey,FKEADLPriv);
                  if FVersion.minor = 0 then
                    Len := SSL_EncodeCertificateVerify(FPendCtx.master_secret,nil,FHandshakes,FKEADLPriv,H)
                  else
                    Len := TLS_EncodeCertificateVerify(nil,FHandshakes,FKEADLPriv,H)
                end;
              end;

              if Assigned(H) then begin
                MS.Write(H^,Len);
                SetLength(FHandshakes,Length(FHandshakes)+1);
                FHandshakes[Length(FHandshakes)-1] := H;
                FPrevHandshake := H^.msg_type;
                H := nil;
              end;
            end;
          end else if FKEA <> keaECDH then begin
            if (DigitalSignature in ExtractKeyUsage(FClientCert^)) then begin
              if Assigned(FKEADLPubl.Y) then begin
                PKE := TX509PublicKey.CreateFromDLKey(FKEADLPubl,False);
                try
                  if (FVersion.major = 3) and (FVersion.minor = 0) then
                    Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
                  else if (FVersion.major = 3) and (FVersion.minor = 1) then
                    Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
                finally
                  PKE.Free;
                end;
              end else begin
                PKE := nil;
                try
                  if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then begin
                    if (FVersion.major = 3) and (FVersion.minor = 0) then
                      Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
                    else if (FVersion.major = 3) and (FVersion.minor = 1) then
                      Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
                  end;
                finally
                  PKE.Free;
                end;
              end;

              if H = nil then
                Result := FOwner.EncodeAlert(internal_error,Response)
              else begin
                MS.Write(H^,Len);
                SetLength(FHandshakes,Length(FHandshakes)+1);
                FHandshakes[Length(FHandshakes)-1] := H;
                FPrevHandshake := H^.msg_type;
                H := nil;

                if PK.KeyAlgorithmType = katIF then begin
                  KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FClientCert^);
                  if Assigned(KEAPriv) then begin            
                    KEAPriv.QueryInterface(IMPIFPrivateKey,FKEAIFPriv);
                    if FVersion.minor = 0 then
                      Len := SSL_EncodeCertificateVerify(FPendCtx.master_secret,nil,FHandshakes,FKEAIFPriv,H)
                    else
                      Len := TLS_EncodeCertificateVerify(nil,FHandshakes,FKEAIFPriv,H)
                  end;
                end else if PK.KeyAlgorithmType = katDL then begin
                  KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FClientCert^);
                  if Assigned(KEAPriv) then begin            
                    KEAPriv.QueryInterface(IMPDLPrivateKey,FKEADLPriv);
                    if FVersion.minor = 0 then
                      Len := SSL_EncodeCertificateVerify(FPendCtx.master_secret,nil,FHandshakes,FKEADLPriv,H)
                    else
                      Len := TLS_EncodeCertificateVerify(nil,FHandshakes,FKEADLPriv,H)
                    end;
                end;

                if Assigned(H) then begin
                  MS.Write(H^,Len);
                  SetLength(FHandshakes,Length(FHandshakes)+1);
                  FHandshakes[Length(FHandshakes)-1] := H;
                  FPrevHandshake := H^.msg_type;
                  H := nil;
                end;
              end;
            end else begin
              KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FClientCert^);

              if Assigned(KEAPriv) then begin            
                KEAPriv.QueryInterface(IMPDLPrivateKey,FKEADLPriv);
                if Assigned(FKEADLPubl.Y) then begin
                  PKE := TX509PublicKey.CreateFromDLKey(FKEADLPubl,False);
                  try
                    if (FVersion.major = 3) and (FVersion.minor = 0) then
                      Len := SSL_EncodeClientKeyExchange(PKE,FKEADLPriv,FPendCtx,H)
                    else if (FVersion.major = 3) and (FVersion.minor = 1) then
                      Len := TLS_EncodeClientKeyExchange(PKE,FKEADLPriv,FPendCtx,H);
                  finally
                    PKE.Free;
                  end;
                end else begin
                  PKE := nil;
                  try
                    if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then begin
                      if (FVersion.major = 3) and (FVersion.minor = 0) then
                        Len := SSL_EncodeClientKeyExchange(PKE,FKEADLPriv,FPendCtx,H)
                      else if (FVersion.major = 3) and (FVersion.minor = 1) then
                        Len := TLS_EncodeClientKeyExchange(PKE,FKEADLPriv,FPendCtx,H);
                    end;
                  finally
                    PKE.Free;
                  end;
                end;
              end;

              if Assigned(H) then begin
                MS.Write(H^,Len);
                SetLength(FHandshakes,Length(FHandshakes)+1);
                FHandshakes[Length(FHandshakes)-1] := H;
                FPrevHandshake := H^.msg_type;
                H := nil;
              end else
                Result := FOwner.EncodeAlert(internal_error,Response);

            end;
          end else begin
            KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FClientCert^);

            if Assigned(KEAPriv) then begin            
              KEAPriv.QueryInterface(IMPECPrivateKey,FKEAECPriv);
              if Assigned(FKEAECPubl.W.X) then begin
                PKE := TX509PublicKey.CreateFromECKey(FKEAECPubl);
                try
                  Len := TLS_EncodeClientKeyExchange(PKE,FKEAECPriv,FPendCtx,H);
                finally
                  PKE.Free;
                end;
              end else begin
                PKE := nil;
                try
                  if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then
                    Len := TLS_EncodeClientKeyExchange(PKE,FKEAECPriv,FPendCtx,H);
                finally
                  PKE.Free;
                end;
              end;

              if Assigned(H) then begin
                MS.Write(H^,Len);
                SetLength(FHandshakes,Length(FHandshakes)+1);
                FHandshakes[Length(FHandshakes)-1] := H;
                FPrevHandshake := H^.msg_type;
                H := nil;
              end else
                Result := FOwner.EncodeAlert(internal_error,Response);

            end;
          end;
        end else
          Result := FOwner.EncodeAlert(internal_error,Response);
      finally
        PK.Free;
      end;
    end else begin
      if FKEA in [keaRSA,keaRSA_Export,keaRSA1024] then begin
        if Assigned(FKEAIFPubl.N) then begin
          PKE := TX509PublicKey.CreateFromIFKey(FKEAIFPubl,False,haSHA1,haSHA1,'');
          try
            if (FVersion.major = 3) and (FVersion.minor = 0) then
              Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
            else if (FVersion.major = 3) and (FVersion.minor = 1) then
              Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
          finally
            PKE.Free;
          end;
        end else if Assigned(FServerCert) then begin
          PKE := nil;
          try
            if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then begin
              if (FVersion.major = 3) and (FVersion.minor = 0) then
                Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
              else if (FVersion.major = 3) and (FVersion.minor = 1) then
                Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
            end;
          finally
            PKE.Free;
          end;
        end else
          H := nil;

        if H = nil then
          Result := FOwner.EncodeAlert(internal_error,Response)
        else begin
          MS.Write(H^,Len);
          SetLength(FHandshakes,Length(FHandshakes)+1);
          FHandshakes[Length(FHandshakes)-1] := H;
          FPrevHandshake := H^.msg_type;
          H := nil;
        end;
      end else if FKEA <> keaECDH then begin
        if Assigned(FKEADLPubl.Y) then begin
          PKE := TX509PublicKey.CreateFromDLKey(FKEADLPubl,False);
          try
            if (FVersion.major = 3) and (FVersion.minor = 0) then
              Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
            else if (FVersion.major = 3) and (FVersion.minor = 1) then
              Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
          finally
            PKE.Free;
          end;
        end else if Assigned(FServerCert) then begin
          PKE := nil;
          try
            if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then begin
              if (FVersion.major = 3) and (FVersion.minor = 0) then
                Len := SSL_EncodeClientKeyExchange(PKE,FPendCtx,H)
              else if (FVersion.major = 3) and (FVersion.minor = 1) then
                Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
            end;
          finally
            PKE.Free;
          end;
        end else
          H := nil;

        if H = nil then
          Result := FOwner.EncodeAlert(internal_error,Response)
        else begin
          MS.Write(H^,Len);
          SetLength(FHandshakes,Length(FHandshakes)+1);
          FHandshakes[Length(FHandshakes)-1] := H;
          FPrevHandshake := H^.msg_type;
          H := nil;
        end;
      end else begin
        if Assigned(FKEAECPubl.W.X) then begin
          PKE := TX509PublicKey.CreateFromECKey(FKEAECPubl);
          try
            Len := TLS_EncodeClientKeyExchange(PKE,FPendCtx,H);
          finally
            PKE.Free;
          end;
        end else begin
          PKE := nil;
          try
            if ExtractSubjectPublicKey(FServerCert^,PKE) = E_OK then
              Len := TLS_EncodeClientKeyExchange(PKE,FKEAECPriv,FPendCtx,H);
          finally
            PKE.Free;
          end;

          if Assigned(H) then begin
            MS.Write(H^,Len);
            SetLength(FHandshakes,Length(FHandshakes)+1);
            FHandshakes[Length(FHandshakes)-1] := H;
            FPrevHandshake := H^.msg_type;
            H := nil;
          end else
            Result := FOwner.EncodeAlert(internal_error,Response);

        end;
      end;
    end;

    if FOwner.FActive then begin
      FOwner.FRecordLayer.FVersion := FVersion;
      Result := 0;
      MS.Position := 0;
      repeat
        Result := Result + FOwner.FRecordLayer.SaveFragmentToStream(MS,Response,Error,rtHandshake);
        if Error >= 0 then begin
          Result := Result + FOwner.EncodeAlert(Error,Response);
          Break;
        end;
      until MS.Position = MS.Size;

      if FOwner.FActive then
        Result := Result + FOwner.EncodeChangeCipherSpec(FPendCtx,Response);

    end;
  finally
    KEAPriv := nil;
    FKEADLPriv := nil;
    FKEAECPriv := nil;
    FKEAIFPriv := nil;
    DisposeDLPublicKey(FKEADLPubl);
    DisposeECPublicKey(FKEAECPubl);
    DisposeIFPublicKey(FKEAIFPubl);
    MS.Free;
  end;
end;

function TTLS_HandshakeProtocol.HandleServerKeyExch(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  ISKE: TInternalServerKeyExchange;
  IFPubl: TIFPublicKey;
  DLPubl: TDLPublicKey;
begin
  FillChar(ISKE,SizeOf(ISKE),0);
  Result := 0;
  if FServerCert = nil then begin
    if not TLS_DecodeServerKeyExchange(@H^.server_key_exchange,Len,FKEA,ISKE) then
      Result := FOwner.EncodeAlert(decrypt_error,Response)
    else begin
      if FKEA = keaECDH then begin
        ECCopy2(ISKE.ecdhParams.public_key,FKEAECPubl.W);
        ECDealloc(ISKE.ecdhParams.public_key);
        ECCopy2(ISKE.ecdhParams.curve_params.primeCurve,FKEAECPubl.Params);
        ECDealloc(ISKE.ecdhParams.curve_params.primeCurve);
      end else if FKEA in [keaDH,keaDHE,keaDHE_Export,keaDHE1024] then begin
        MPDealloc(FKEADLPubl.Y);
        FKEADLPubl.Y := ISKE.dhParams.dh_Ys;
        MPDealloc(FKEADLPubl.Params.P);
        FKEADLPubl.Params.P := ISKE.dhParams.dh_P;
        MPDealloc(FKEADLPubl.Params.G);
        FKEADLPubl.Params.G := ISKE.dhParams.dh_G;
        MPDec2(FKEADLPubl.Params.P,FKEADLPubl.Params.Q);
      end else if FKEA in [keaRSA_Export,keaRSA1024] then begin
        DisposeIFPublicKey(FKEAIFPubl);
        FKEAIFPubl.N := ISKE.rsaParams.rsa_modulus;
        FKEAIFPubl.E := ISKE.rsaParams.rsa_exponent;
      end;
    end;
  end else if FSignAlg = saRSA then begin
    FillChar(IFPubl,SizeOf(IFPubl),0);
    try
      if ExtractSubjectRSAPublicKey(FServerCert^,IFPubl) <> E_OK then begin
        Result := FOwner.EncodeAlert(bad_certificate,Response);
      end else if not TLS_DecodeServerKeyExchange(@H^.server_key_exchange,Len,
                                                  FPendCtx.client_random,
                                                  FPendCtx.server_random,
                                                  FKEA,IFPubl,ISKE) then
        Result := FOwner.EncodeAlert(handshake_failure,Response)
      else begin                                           
        if FKEA = keaECDHE then begin
          ECCopy2(ISKE.ecdhParams.public_key,FKEAECPubl.W);
          ECDealloc(ISKE.ecdhParams.public_key);
          ECCopy2(ISKE.ecdhParams.curve_params.primeCurve,FKEAECPubl.Params);
          ECDealloc(ISKE.ecdhParams.curve_params.primeCurve);
        end else if FKEA in [keaDHE,keaDHE_Export,keaDHE1024] then begin
          MPDealloc(FKEADLPubl.Y);
          FKEADLPubl.Y := ISKE.dhParams.dh_Ys;
          MPDealloc(FKEADLPubl.Params.P);
          FKEADLPubl.Params.P := ISKE.dhParams.dh_P;
          MPDealloc(FKEADLPubl.Params.G);
          FKEADLPubl.Params.G := ISKE.dhParams.dh_G;
          MPDec2(FKEADLPubl.Params.P,FKEADLPubl.Params.Q);
        end else if FKEA in [keaRSA_Export,keaRSA1024] then begin
          DisposeIFPublicKey(FKEAIFPubl);
          FKEAIFPubl.N := ISKE.rsaParams.rsa_modulus;
          FKEAIFPubl.E := ISKE.rsaParams.rsa_exponent;
        end else
          Result := FOwner.EncodeAlert(handshake_failure,Response)
      end;
    finally
      DisposeIFPublicKey(IFPubl);
    end;
  end else if FSignAlg = saDSS then begin
    FillChar(DLPubl,SizeOf(DLPubl),0);
    try
      if ExtractSubjectDSAPublicKey(FServerCert^,DLPubl) <> E_OK then begin
        Result := FOwner.EncodeAlert(bad_certificate,Response);
      end else if not TLS_DecodeServerKeyExchange(@H^.server_key_exchange,Len-4,
                                                  FPendCtx.client_random,
                                                  FPendCtx.server_random,
                                                  FKEA,DLPubl,ISKE) then
        Result := FOwner.EncodeAlert(handshake_failure,Response)
      else begin
        if FKEA = keaECDHE then begin
          ECCopy2(ISKE.ecdhParams.public_key,FKEAECPubl.W);
          ECDealloc(ISKE.ecdhParams.public_key);
          ECCopy2(ISKE.ecdhParams.curve_params.primeCurve,FKEAECPubl.Params);
          ECDealloc(ISKE.ecdhParams.curve_params.primeCurve);
        end else if FKEA in [keaDHE,keaDHE_Export,keaDHE1024] then begin
          MPDealloc(FKEADLPubl.Y);
          FKEADLPubl.Y := ISKE.dhParams.dh_Ys;
          MPDealloc(FKEADLPubl.Params.P);
          FKEADLPubl.Params.P := ISKE.dhParams.dh_P;
          MPDealloc(FKEADLPubl.Params.G);
          FKEADLPubl.Params.G := ISKE.dhParams.dh_G;
          MPDec2(FKEADLPubl.Params.P,FKEADLPubl.Params.Q);
        end else
          Result := FOwner.EncodeAlert(handshake_failure,Response)
      end;
    finally
      DisposeDLPublicKey(DLPubl);
    end;
  end else
    Result := FOwner.EncodeAlert(unexpected_message,Response)
end;

function TTLS_HandshakeProtocol.InternalHandleClientKeyExch(var H: PTLSHandshake;
  var Len: Integer; Response: TStream): Integer;
var
  KEAPriv: TMPPrivateKey;
begin
  Result := 0;
  if FOwner.FRequireClientAuth and (FClientCert = nil) then begin
    Result := FOwner.EncodeAlert(handshake_failure,Response);
  end else if (FKEA in [keaDH,keaECDH]) and Assigned(FClientCert) and
     (KeyAgreement in ExtractKeyUsage(FClientCert^)) then begin
    if FServerCert = nil then
      KEAPriv := nil
    else
      KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FServerCert^);
    if Assigned(KEAPriv) then begin
      if KEAPriv is TMPDLPrivateKeyTLS then
        FKEADLPriv := TMPDLPrivateKeyTLS(KEAPriv)
      else if KEAPriv is TMPECPrivateKey then
        FKEAECPriv := TMPECPrivateKey(KEAPriv);
    end;
    if (FKEA = keaDH) and
       not (Assigned(KEAPriv) and (KEAPriv is TMPDLPrivateKeyTLS) and
            (((FVersion.minor = 0) and
              (SSL_DecodeClientKeyExchange(@H^.client_key_exchange,Len,FClientCert^,FKEADLPriv,FPendCtx))) or
             TLS_DecodeClientKeyExchange(@H^.client_key_exchange,Len,FClientCert^,FKEADLPriv,FPendCtx))) then
      Result := FOwner.EncodeAlert(illegal_parameter or client_key_exch_format,Response)
    else if FKEA = keaECDH then begin
      if not (Assigned(KEAPriv) and (KEAPriv is TMPECPrivateKey) and
              TLS_DecodeClientKeyExchange(@H^.client_key_exchange,Len,FClientCert^,FKEAECPriv,FPendCtx)) then
        Result := FOwner.EncodeAlert(illegal_parameter or client_key_exch_format,Response);
    end else
      Result := FOwner.EncodeAlert(certificate_unknown,Response)
  end else if FKEA in [keaDH,keaDHE,keaDHE_Export,keaDHE1024] then begin
    if (FKEA = keaDH) and Assigned(FServerCert) then begin
      KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FServerCert^);
      if Assigned(KEAPriv) and (KEAPriv is TMPDLPrivateKeyTLS) then
        FKEADLPriv := TMPDLPrivateKeyTLS(KEAPriv);
    end;
    if not Assigned(FKEADLPriv) then
      Result := FOwner.EncodeAlert(internal_error or client_key_exch_format,Response)
    else if not (((FVersion.minor = 0) and
                  SSL_DecodeClientKeyExchange(@H^.client_key_exchange,Len,nil,FKEADLPriv,FPendCtx)) or
                 TLS_DecodeClientKeyExchange(@H^.client_key_exchange,Len,nil,FKEADLPriv,FPendCtx)) then
      Result := FOwner.EncodeAlert(illegal_parameter or client_key_exch_format,Response);
  end else if FKEA = keaECDH then begin
    if not Assigned(FKEAECPriv) then
      Result := FOwner.EncodeAlert(internal_error or client_key_exch_format,Response)
    else if not TLS_DecodeClientKeyExchange(@H^.client_key_exchange,Len,nil,FKEAECPriv,FPendCtx) then
      Result := FOwner.EncodeAlert(illegal_parameter or client_key_exch_format,Response);
  end else begin
    if (FKEA = keaRSA) or (FKEAIFPriv = nil) then begin
      KEAPriv := FOwner.FPrivateKeyRing.FindCreatePrivateKeyTLS(FServerCert^);
      if Assigned(KEAPriv) and (KEAPriv is TMPIFPrivateKey) then
        FKEAIFPriv := TMPIFPrivateKey(KEAPriv);
    end;
    if not (Assigned(FKEAIFPriv) and
            (((FVersion.minor = 0) and
              SSL_DecodeClientKeyExchange(@H^.client_key_exchange,Len,FKEAIFPriv,FPendCtx)) or
             TLS_DecodeClientKeyExchange(@H^.client_key_exchange,Len,FKEAIFPriv,FPendCtx))) then
      Result := FOwner.EncodeAlert(illegal_parameter or client_key_exch_format,Response);
  end;
  FKEADLPriv := nil;
  FKEAECPriv := nil;
  FKEAIFPriv := nil;
  DisposeDLPublicKey(FKEADLPubl);
  DisposeECPublicKey(FKEAECPubl);
  DisposeIFPublicKey(FKEAIFPubl);
end;

{$ELSE  SHA1_AND_MD5}
implementation
{$ENDIF SHA1_AND_MD5}
end.
