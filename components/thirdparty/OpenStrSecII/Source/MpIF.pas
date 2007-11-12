{$B-,O+,Q-,R-}
{$I ver.inc}
{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPIF Unit                                         }
{     Implementation of Integer Factorization schemes   }
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
unit MpIF;

interface

uses
  MpArithTypes, SecUtils, SysUtils, Asn1, MpPK;

type
  TIFScheme = (ifRSA1, ifRSA2,ifRW);  
  TSignEncoding = ({$IFDEF SHA1_OR_RIPEMD160}
                   seEMSA2,      // As defined in P1363
                   {$ELSE  SHA1_OR_RIPEMD160}
                   seReserved0,
                   {$ENDIF SHA1_OR_RIPEMD160}
                   seEMSA3,      // PKCS#1v1.5
                   seEMSA4,      // PKCS#1v2.1 (RSASSA-PSS)
                   {$IFDEF SHA1_AND_MD5}
                   seEMSA_TLS    // TLS (PKCS#1v1.5 with no hash identifier and
                                 //      two digest values)
                   {$ELSE  SHA1_AND_MD5}
                   seReserved3
                   {$ENDIF SHA1_AND_MD5}
                   );
  TEncryptEncoding = (eeEME1,           // PKCS#1v2.1 (RSAES-OAEP)
                      eeEME_PKCS1_v1_5, // PKCS#1v1.5
                      eeNotSupported);  // (Internal, don't use this value)

  TIFPublicKey = record
    Scheme: TIFScheme;
    N, E: PMPInteger;
  end;

  TExternalIFPrivateKey = class
  protected                                 
    function GetIdentifier: string; virtual;
    function GetKeySize: Integer; virtual;
    function IFPrivateKeyOperation(I: PMPInteger;
                                   var J: PMPInteger): Boolean; virtual;
    function SignMsg(const Msg; MsgCount: Integer;
                     HashAlgorithm: THashAlgorithm;
                     MGFHashAlgorithm: THashAlgorithm;
                     Encoding: TSignEncoding;
                     var Sign; var SignCount: Integer): Boolean; virtual;
    function SignDigest(const Digest; DigestCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        MGFHashAlgorithm: THashAlgorithm;
                        Encoding: TSignEncoding;
                        NullMsg: Boolean;
                        var Sign; var SignCount: Integer): Boolean; virtual;
    function IFESDecryption(const EMsg; EMsgCount: Integer;
                            const P; PCount: Integer;
                            HashAlgorithm: THashAlgorithm;
                            MGFHashAlgorithm: THashAlgorithm;
                            EncryptEncoding: TEncryptEncoding;
                            var Msg; var MsgLen: Integer;
                            var Success: Boolean): Boolean; virtual;
    function Validate(const APubl: TIFPublicKey): Boolean; virtual;
  public
    constructor Create; virtual;
    function ImportKey(const PKIdentifier; PKILen: Integer): Boolean; virtual;
    property Identifier: string read GetIdentifier;
  end;

  TExternalIFPrivateKeyClass = class of TExternalIFPrivateKey;

  EBitAlignment = class(Exception);

  TIFPrivateKey = record
    Scheme: TIFScheme;
    case ReprType: Byte of
      0: (zN, zD: PMPInteger);
      1: (oP, oQ, oD1, oD2, oC: PMPInteger);
      2: (tP, tQ, tD: PMPInteger);
      3: (ExternalKey: TExternalIFPrivateKey);
  end;

  IIFPublicKeyInfo = interface(IPKPublicKeyInfo)
    ['{BBDA86BA-A3F4-4BB2-A642-C263A2F5BD1F}']
    function ExtractSubjectRSAPublicKey(var RSAKey: TIFPublicKey): Boolean;
    function ExtractSubjectRSAES_OAEPPublicKey(var RSAKey: TIFPublicKey;
                                               var HA: THashAlgorithm;
                                               var MGFHA: THashAlgorithm;
                                               var P: string): Boolean;
    procedure ImposeSubjectRSAPublicKey(const RSAKey: TIFPublicKey);
    procedure ImposeSubjectRSAES_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                               HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               MGFHA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               P: OctetString = '');
  end;

  IMPIFPrivateKey = interface(IMPPrivateKey)
  ['{DB4663AC-A68E-428B-8D54-AAAE91117DA9}']
    function GetEncryptHashAlg: THashAlgorithm;
    function GetExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
    function GetMGFHashAlg: THashAlgorithm;
    function GetP: OctetString;
    function RSAPublicKey(var APublKey: TIFPublicKey): Boolean;
    procedure SetEncryptHashAlg(const Value: THashAlgorithm);
    procedure SetMGFHashAlg(const Value: THashAlgorithm);
    procedure SetExternalIFPrivateKeyClass(
      const Value: TExternalIFPrivateKeyClass);
    procedure SetP(const Value: OctetString);
    function ValidatePublicKey(const Publ: TIFPublicKey): Boolean;
    property EncryptHashAlg: THashAlgorithm read GetEncryptHashAlg
                                            write SetEncryptHashAlg;
    property ExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass
      read GetExternalIFPrivateKeyClass
      write SetExternalIFPrivateKeyClass;
    property MGFHashAlg: THashAlgorithm read GetMGFHashAlg
                                        write SetMGFHashAlg;
    property P: OctetString read GetP write SetP;
  end;

  TMPIFPrivateKey = class(TMPPrivateKey,IMPIFPrivateKey)
  private
    FKey: TIFPrivateKey;
    FMGFHashAlg: THashAlgorithm;
    FExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
    FModulusBitSize: Integer;
    FEncryptHashAlg: THashAlgorithm;
    FP: OctetString;
    FCreating: Boolean;
  protected
    function DecodeASNKey(K: TASN1Struct): Boolean; override;
    procedure DisposeKey; override;
    function EncodeASNKey(K: TASN1Struct): Boolean; override;
    function EncryptEncoding: TEncryptEncoding; virtual;
    function GetAlgorithm: ObjectIdentifier; override;
    function GetEncryptHashAlg: THashAlgorithm;
    function GetExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
    function GetMGFHashAlg: THashAlgorithm;
    function GetP: OctetString;
    function GetSignatureLength: Integer; override;
    function GetSignatureSize: Integer; override;
    procedure InternalDecryptKeyTransport(const EMsg; MsgLen: Integer;
                                          var Key; KeyLen: Integer;
                                          out Result: Boolean); override;
    procedure InternalSignBuf(const Msg; MsgLen: Integer;
                              var Sign; var SignLen: Integer); override;
    procedure InternalSignDig(const Digest; DigestLen: Integer;
                              var Sign; var SignLen: Integer); override;
    procedure InternalSignSigned(Signed: IUnknown;
                                 CACert: TASN1Struct;
                                 var OK: Boolean); override;
    procedure SetEncryptHashAlg(const Value: THashAlgorithm);
    procedure SetMGFHashAlg(const Value: THashAlgorithm);
    procedure SetPublKey(const Value: IPKPublicKeyInfo); override;
    procedure SetExternalIFPrivateKeyClass(
      const Value: TExternalIFPrivateKeyClass);
    procedure SetP(const Value: OctetString);
    function SignEncoding: TSignEncoding; virtual;
  public
    constructor CreateNew(const AAlgorithm: ObjectIdentifier;
                          ABitSize: Integer;
                          AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
                          ASignatureDigestAlg: THashAlgorithm;
                          APublKey: IIFPublicKeyInfo;
                          Exportable: Boolean = False);
    function ExportToRSAPrivateKey(var RSAPrivateKey: TASN1Struct;
                                   OverrideLock: Boolean): Boolean;
    function ExportToPRIVATEKEYBLOB(out Priv: Pointer;
                                    out Len: Integer;
                                    Alg: LongWord;
                                    OverrideLock: Boolean): Boolean;
    function RSAPublicKey(var APublKey: TIFPublicKey): Boolean;
    function ValidatePublicKey(const Publ: TIFPublicKey): Boolean;
    property EncryptHashAlg: THashAlgorithm read GetEncryptHashAlg write SetEncryptHashAlg;
    property ExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass
      read GetExternalIFPrivateKeyClass
      write SetExternalIFPrivateKeyClass;
    property MGFHashAlg: THashAlgorithm read GetMGFHashAlg
                                        write SetMGFHashAlg;
    property P: OctetString read GetP write SetP;
  end;

  TMPIFPrivateKeyTLS = class(TMPIFPrivateKey)
  protected
    function SignEncoding: TSignEncoding; override;
  end;

  TMPIFPublicKey = class(TMPPublicKey)
  private
    FKey: TIFPublicKey;
    FAlgorithm: ObjectIdentifier;
    FMGFHashAlg: THashAlgorithm;
    FP: OctetString;
    procedure SetMGFHashAlg(const Value: THashAlgorithm);
    procedure SetP(const Value: OctetString);
  protected
    function DecodeKey: Boolean; override;
    procedure DisposeKey; override;
    procedure EncodeKey; override;        
    function GetSignatureLength: Integer; override;
    procedure InternalEncryptKeyTransport(const Key; KeyLen: Integer;
                                          var EMsg; MsgLen: Integer;
                                          out Result: Boolean); override;
    function InternalVerBuf(const Msg; MsgLen: Integer;
                            const Sign; SignLen: Integer): Boolean; override;
    function InternalVerDig(const Digest; DigestLen: Integer;
                            const Sign; SignLen: Integer): Boolean; override;
    function SignEncoding: TSignEncoding;
  public
    constructor CreateFromRecord(const AAlgorithm: ObjectIdentifier;
                                 const IFPublKey: TIFPublicKey;
                                 AKeyIntf: IIFPublicKeyInfo);
    constructor CreateRSAES_OAEPFromRecord(const IFPublKey: TIFPublicKey;
                                           HA, MGFHA: THashAlgorithm;
                                           const P: OctetString;
                                           AKeyIntf: IIFPublicKeyInfo);
    constructor CreateRWFromRecord(const IFPublKey: TIFPublicKey;
                                   HA, MGFHA: THashAlgorithm;
                                   AKeyIntf: IIFPublicKeyInfo);
    property MGFHashAlg: THashAlgorithm read FMGFHashAlg
                                        write SetMGFHashAlg;
    property P: OctetString read FP write SetP;
  end;
  
procedure DisposeIFPrivateKey(var AKey: TIFPrivateKey);
procedure DisposeIFPublicKey(var AKey: TIFPublicKey);

function GetEncodingRangeEME1(AKeyBits: Cardinal;
                              AHashAlgorithm: THashAlgorithm;
                              var MaxBytes: Integer): Boolean;   
function GetEncodingRangeEME_PKCS1_v1_5(AKeyBits: Cardinal;
                                        var MaxBytes: Integer): Boolean;

procedure IFRSAKeys(ABitSize: Cardinal;
                    AReprType: Byte;
                    AStrongPrimes: Boolean;
                    var APriv: TIFPrivateKey;
                    var APubl: TIFPublicKey;
                    Exportable: Boolean = False);
procedure IFRSAPublicKey(const APriv: TIFPrivateKey;
                         var APubl: TIFPublicKey;
                         Compability: Boolean = False);
function KeyIdentifierFromPrivateKey(const APriv: TIFPrivateKey): OctetString;

procedure IFRWKeys(ABitSize: Cardinal;
                   AReprType: Byte;
                   AStrongPrimes: Boolean;
                   var APriv: TIFPrivateKey;
                   var APubl: TIFPublicKey);

function ValidateIFPrivateKey(APriv: TIFPrivateKey; APubl: TIFPublicKey): Boolean;

// Usage:
//   AKey.Scheme = ifRSA1, Encoding = seEMSA3: RSA-PKCS-v1.5
//   AKey.Scheme = ifRSA1, Encoding = seEMSA4: RSA-PSS
function IFSSASignatureGeneration(const AKey: TIFPrivateKey;
                                  const Digest; DigestCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  MGFHashAlgorithm: THashAlgorithm;
                                  Encoding: TSignEncoding;
                                  NullMsg: Boolean;
                                  var Sign; SignCount: Integer): Integer; overload;

function IFSSASignatureGeneration(const AKey: TIFPrivateKey;
                                  const Msg; MsgCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  MGFHashAlgorithm: THashAlgorithm;
                                  Encoding: TSignEncoding;
                                  var Sign; SignCount: Integer): Integer; overload;

function IFSSASignatureVerification(const AKey: TIFPublicKey;
                                    const Digest; DigestCount: Integer;
                                    const Sign; SignCount: Integer;
                                    HashAlgorithm: THashAlgorithm;
                                    MGFHashAlgorithm: THashAlgorithm;
                                    Encoding: TSignEncoding;
                                    NullMsg: Boolean): Boolean; overload;

function IFSSASignatureVerification(const AKey: TIFPublicKey;
                                    const Msg; MsgCount: Integer;
                                    const Sign; SignCount: Integer;
                                    HashAlgorithm: THashAlgorithm;
                                    MGFHashAlgorithm: THashAlgorithm;
                                    Encoding: TSignEncoding): Boolean; overload;

// RSA-OAEP
procedure IFESEncryption(const AKey: TIFPublicKey;
                         const Msg; MsgCount: Integer;
                         const P; PCount: Integer;
                         HashAlgorithm: THashAlgorithm;
                         MGFHashAlgorithm: THashAlgorithm;
                         EncryptEncoding: TEncryptEncoding;
                         var EMsg; var EMsgLen: Integer); overload;
procedure IFESEncryption(const AKey: TIFPublicKey;
                         const Msg; MsgCount: Integer;
                         HashAlgorithm: THashAlgorithm;
                         EncryptEncoding: TEncryptEncoding;
                         var EMsg; var EMsgLen: Integer); overload;

function IFESDecryption(const AKey: TIFPrivateKey;
                        const EMsg; EMsgCount: Integer;
                        const P; PCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        MGFHashAlgorithm: THashAlgorithm;
                        EncryptEncoding: TEncryptEncoding;
                        var Msg; var MsgLen: Integer): Boolean; overload;
function IFESDecryption(const AKey: TIFPrivateKey;
                        const EMsg; EMsgCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        EncryptEncoding: TEncryptEncoding;
                        var Msg; var MsgLen: Integer): Boolean; overload;
function IFESDecryption(const AKey: TIFPrivateKey;
                        const EMsg; EMsgCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        EncryptEncoding: TEncryptEncoding;
                        var Msg: ISecretKey): Boolean; overload;

function SignatureAlgEMSA3(HA: THashAlgorithm): ObjectIdentifier;
function ExtractHashAlgEMSA3(OID: ObjectIdentifier;
                             var HA: THashAlgorithm): Boolean;

implementation

uses
  MpArith, MpYarrow, MpKaratsuba, Pkix, Pkix_Cert, MpIFConversion, X509Base;

function SignatureAlgEMSA3(HA: THashAlgorithm): ObjectIdentifier;
begin
{$IFDEF MD2}
  if HA = haMD2 then
    Result := md2WithRSAEncryption
  else
{$ENDIF MD2}
{$IFNDEF DISABLEMD5SIGN}
{$IFDEF MD5}
  if HA = haMD5 then
    Result := md5WithRSAEncryption
  else
{$ENDIF MD5}
{$ENDIF DISABLEMD5SIGN}
{$IFDEF SHA1}
  if HA = haSHA1 then
    Result := sha1WithRSAEncryption
  else
{$ENDIF SHA1}
{$IFDEF SHA256}
  if HA = haSHA256 then
    Result := sha256WithRSAEncryption
  else
{$ENDIF SHA256}
{$IFDEF SHA512}
  if HA = haSHA384 then
    Result := sha384WithRSAEncryption
  else if HA = haSHA512 then
    Result := sha512WithRSAEncryption
  else
{$ENDIF SHA512}
    raise Exception.Create('Algorithm not supported');
end;

function ExtractHashAlgEMSA3(OID: ObjectIdentifier; var HA: THashAlgorithm): Boolean;
begin
  Result := True;
  if (OID = md2WithRSAEncryption) or
     (OID = '1.3.14.3.2.24') then
{$IFDEF MD2}
    HA := haMD2
{$ELSE  MD2}
    Result := False
{$ENDIF MD2}
  else if (OID = md5WithRSAEncryption) or
          (OID = '1.3.14.3.2.25') then
{$IFNDEF DISABLEMD5SIGN}
{$IFDEF MD5}
    HA := haMD5
{$ELSE  MD5}
    Result := False
{$ENDIF MD5}
{$ENDIF DISABLEMD5SIGN}
  else if (OID = sha1WithRSAEncryption) or
          (OID = '1.3.14.3.2.29') then
{$IFDEF SHA1}
    HA := haSHA1
{$ELSE  SHA1}
    Result := False
{$ENDIF SHA1}
  else if OID = sha256WithRSAEncryption then
{$IFDEF SHA256}
    HA := haSHA256
{$ELSE  SHA256}
    Result := False
{$ENDIF SHA256}
  else if OID = sha384WithRSAEncryption then
{$IFDEF SHA512}
    HA := haSHA384
{$ELSE  SHA512}
    Result := False
{$ENDIF SHA512}
  else if OID = sha512WithRSAEncryption then
{$IFDEF SHA512}
    HA := haSHA512
{$ELSE  SHA512}
    Result := False
{$ENDIF SHA512}
  else
    Result := False;
end;

function IsSHA1orRipeMD160(HA: THashAlgorithm): Boolean;
begin
{$IFDEF SHA1}
  Result := HA = haSHA1;
{$ELSE  SHA1}
  Result := False;
{$ENDIF SHA1}
{$IFDEF RIPEMD160}
  Result := Result or (HA = haRipeMD160);
{$ENDIF}
end;

procedure DisposeIFPrivateKey(var AKey: TIFPrivateKey);
begin
  case AKey.ReprType of
    0: begin
         MPDealloc(AKey.zN);
         MPDealloc(AKey.zD);
       end;
    1: begin
         MPDealloc(AKey.oP);
         MPDealloc(AKey.oQ);
         MPDealloc(AKey.oD1);
         MPDealloc(AKey.oD2);
         MPDealloc(AKey.oC);
       end;
    2: begin
         MPDealloc(AKey.tP);
         MPDealloc(AKey.tQ);
         MPDealloc(AKey.tD);
       end;
    3: AKey.ExternalKey.Free;
  end;
  FillChar(AKey,SizeOf(AKey),0);
end;

procedure DisposeIFPublicKey(var AKey: TIFPublicKey);
begin
  MPDealloc(AKey.N);
  MPDealloc(AKey.E);
  FillChar(AKey,SizeOf(AKey),0);
end;

function ValidateIFPrivateKey(APriv: TIFPrivateKey; APubl: TIFPublicKey): Boolean;
var
  X, Y, Z, N, L, W: PMPInteger;
begin
  case APriv.ReprType of
    0: begin
         Result := (MPCmpOffset(APriv.zN,APubl.N) = 0);
         Result := Result and
                   (MPCmpOffset(APriv.zD,APriv.zN) < 0);
         Result := Result and
                   (APriv.Scheme = APubl.Scheme) and
                   (((APriv.Scheme in [ifRSA1,ifRSA2]) and
                     ((MPMSB(APubl.E) > 2) or
                      (APubl.E^.Data[APubl.E^.Size - 1] = 3))) or
                    ((APriv.Scheme = ifRW) and
                     (MPMSB(APubl.E) > 1)));
         if Result then begin
           X := nil;
           try
             repeat
               MPRawRandom(X,MPMSB(APriv.zN)-1);
             until MPMSB(X) > 0;
             Y := nil;
             try
               MPKaratsubaExpMod(X,APriv.zD,APriv.zN,Y);
               Z := nil;
               try
                 MPKaratsubaExpMod(Y,APubl.E,APubl.N,Z);
                 Result := MPCmpOffset(X,Z) = 0;
               finally
                 MPDealloc(Z);
               end;
             finally
               MPDealloc(Y);
             end;
           finally
             MPDealloc(X);
           end;
         end;
       end;
    1: begin
         N := nil;
         try
           MPMul2(APriv.oP,APriv.oQ,N);
           Result := (MPCmpOffset(N,APubl.N) = 0) and
                     (MPMSB(APriv.oP) = (MPMSB(N) + 1) shr 1);
         finally
           MPDealloc(N);
         end;
         if Result then begin
           X := nil;
           try
             Y := nil;
             try
               MPMulMod2(APriv.oC,APriv.oQ,APriv.oP,Y);
               Result := MPMSB(Y) = 1;
               if Result and (APriv.Scheme = ifRW) then begin
                 Result := (MPModByInt(APriv.oP,8) = 3) and
                           (MPModByInt(APriv.oQ,8) = 7);
                 if Result then begin
                   MPDec2(APriv.oP,X);
                   MPDec2(APriv.oQ,Y);
                   Z := nil;
                   try
                     MPBinaryGCD(X,Y,Z);
                     L := nil;
                     try
                       W := nil;
                       try
                         MPMul2(X,Y,W);
                         MPDiv(W,Z,L);
                         MPShr(L,1);
                         MPInvMod(APubl.E,L,Z);
                         MPMod2(Z,X,W);
                         Result := MPCmpOffset(W,APriv.oD1) = 0;
                         if Result then begin
                           MPMod2(Z,Y,W);
                           Result := MPCmpOffset(W,APriv.oD2) = 0;
                         end;
                       finally
                         MPDealloc(W);
                       end;
                     finally
                       MPDealloc(L);
                     end;
                   finally
                     MPDealloc(Z);
                   end;
                 end;
               end;
             finally
               MPDealloc(Y);
             end;
           finally
             MPDealloc(X);
           end;
         end;
       end;
    2: begin  
         N := nil;
         try
           MPMul2(APriv.tP,APriv.tQ,N);
           Result := (MPCmpOffset(N,APubl.N) = 0) and
                     (MPMSB(APriv.oP) = (MPMSB(N) + 1) shr 1);
         finally
           MPDealloc(N);
         end;
         if APriv.Scheme = ifRW then
           Result := Result and
                     (MPModByInt(APriv.tP,8) = 3) and
                     (MPModByInt(APriv.tP,8) = 7);
         if Result then begin
           X := nil;
           try
             MPDec2(APriv.tP,X);
             Y := nil;
             try
               MPDec2(APriv.tQ,Y);
               Z := nil;
               try
                 MPBinaryGCD(X,Y,Z);
                 W := nil;
                 try
                   MPMul2(X,Y,W);
                   L := nil;
                   try
                     MPDiv(W,Z,L);
                     if APriv.Scheme = ifRW then
                       MPShr(L,1);
                     MPInvMod(APubl.E,L,Z);
                     Result := MPCmpOffset(Z,APriv.tD) = 0;
                   finally
                     MPDealloc(L);
                   end;
                 finally
                   MPDealloc(W);
                 end;
               finally
                 MPDealloc(Z);
               end;
             finally
               MPDealloc(Y);
             end;
           finally
             MPDealloc(X);
           end;
         end;
       end;
    3: Result := APriv.ExternalKey.Validate(APubl);
  else
    Result := False;
  end;
end;

procedure IFRSAKeys(ABitSize: Cardinal;
                    AReprType: Byte;
                    AStrongPrimes: Boolean;
                    var APriv: TIFPrivateKey;
                    var APubl: TIFPublicKey;
                    Exportable: Boolean);
var
  P, Q, N, X, Y, M, G, L, D: PMPInteger;
  Params: TStrongPrimeParams;
begin
  Assert(AReprType < 3);
  APriv.ReprType := AReprType;
  APriv.Scheme := ifRSA1;
  APubl.Scheme := ifRSA1;

  if APubl.E = nil then
    APubl.E := IntToMPInt($10001);

  P := nil;
  try
    Params.PMin := nil;
    Params.PUBound := nil;
    Params.F := APubl.E;
    Params.Mod8 := 255;
    if AStrongPrimes then begin
      Params.LT := ABitSize div 6;
      Params.LR := ABitSize div 5;
      Params.LS := ABitSize div 5;
      MPPrime(P,(ABitSize + 1) shr 1,paStrongPrime,@Params);
    end else
      MPPrime(P,(ABitSize + 1) shr 1,paStandardFast,@Params);
    if AReprType = 1 then
      MPCopy2(P,APriv.oP)
    else if AReprType = 2 then
      MPCopy2(P,APriv.tP);
    Q := nil;
    try
      X := IntToMPInt(1);
      try
        MPShl(X,ABitSize-1);
        MPDiv(X,P,Params.PMin);
        MPInc(Params.PMin);
        if Exportable then begin
          if MPMSB(Params.PMin) < Integer(ABitSize shr 1) then begin
            MPDealloc(Params.PMin);
            Params.PMin := IntToMPInt(1);
            MPShl(Params.PMin,(ABitSize shr 1) - 1);
          end;
        end;
      finally
        MPDealloc(X);
      end;
      X := IntToMPInt(1);
      try
        MPShl(X,ABitSize);
        MPDiv(X,P,Params.PUBound);
        MPInc(Params.PUBound);
        if Exportable then begin
          if MPMSB(Params.PUBound) > Integer(ABitSize shr 1) then begin
            MPDealloc(Params.PUBound);
            Params.PUBound := IntToMPInt(1);
            MPShl(Params.PUBound,(ABitSize shr 1));
            Assert(MPMSB(Params.PUBound) = Integer(ABitSize shr 1) + 1,
                   IntToStr(MPMSB(Params.PUBound)));
            MPDec(Params.PUBound);
            Assert(MPMSB(Params.PUBound) = Integer(ABitSize shr 1),
                   IntToStr(MPMSB(Params.PUBound)));
          end;
        end;
      finally
        MPDealloc(X);
      end;
      Params.F := APubl.E;
      Params.Mod8 := 255;
      if AStrongPrimes then begin
        Params.LT := ABitSize div 6;
        Params.LR := ABitSize div 5;
        Params.LS := ABitSize div 5;
        MPPrime(Q,0,paStrongPrime,@Params);
      end else
        MPPrime(Q,0,paStandardFast,@Params);
      MPDealloc(Params.PMin);
      MPDealloc(Params.PUBound);
      if AReprType = 1 then
        MPCopy2(Q,APriv.oQ)
      else if AReprType = 2 then
        MPCopy2(Q,APriv.tQ);

      N := nil;
      try
        MPMul2(P,Q,N);
        MPRealloc(N,(MPMSB(N) + 31) shr 5);
        if AReprType = 0 then
          MPCopy2(N,APriv.zN);
        MPCopy2(N,APubl.N);
        X := nil;
        try
          MPDec2(P,X);
          Y := nil;
          try
            MPDec2(Q,Y);
            M := nil;
            try
              MPMul2(X,Y,M);
              G := nil;
              try
                MPBinaryGCD(X,Y,G);
                L := nil;
                try
                  MPDiv(M,G,L);
                  D := nil;
                  try
                    MPInvMod(APubl.E,L,D);
                    if AReprType = 0 then
                      MPCopy2(D,APriv.zD)
                    else if AReprType = 2 then
                      MPCopy2(D,APriv.tD)
                    else begin
                      MPCopy2(D,aPriv.oD1);
                      MPMod(aPriv.oD1,X);

                      MPCopy2(D,aPriv.oD2);
                      MPMod(aPriv.oD2,Y);

                      MPInvMod(Q,P,aPriv.oC);

                      MPCopy2(Q,M);
                      MPMulMod(M,aPriv.oC,P);
                      Assert(MPMSB(M) = 1);
                    end;
                  finally
                    MPDealloc(D);
                  end;
                finally
                  MPDealloc(L);
                end;
              finally
                MPDealloc(G);
              end;
            finally
              MPDealloc(M);
            end;
          finally
            MPDealloc(Y);
          end;
        finally
          MPDealloc(X);
        end;
      finally
        MPDealloc(N);
      end;
    finally
      MPDealloc(Q);
    end;
  finally
    MPDealloc(P);
  end;
end;

procedure IFRSAPublicKey(const APriv: TIFPrivateKey;
                         var APubl: TIFPublicKey;
                         Compability: Boolean);
var
  N, M, D, X, Y, G, L, D1, P, Q: PMPInteger;
begin
  Assert(APriv.ReprType in [1,2]);

  N := nil;
  try
    case APriv.ReprType of
      1:
        begin
          P := APriv.oP;
          Q := APriv.oQ;
        end;
      2:
        begin
          P := APriv.tP;
          Q := APriv.tQ;
        end;
    else
      P := nil;
      Q := nil;
    end;
    MPMul2(P,Q,N);
    MPRealloc(N,(MPMSB(N) + 31) shr 5);
    MPCopy2(N,APubl.N);
    X := nil;
    try
      MPDec2(P,X);
      Y := nil;
      try
        MPDec2(Q,Y);
        M := nil;
        try
          MPMul2(X,Y,M);
          G := nil;
          try
            L := nil;
            try
              if APriv.ReprType = 1 then begin
                D := nil;
                try
                  repeat
                    MPBinaryGCD(X,Y,G);
                    if MPMSB(G) = 1 then Break;
                    MPCopy2(X,M);
                    MPDiv(M,G,X);
                  until False;
                  D1 := nil;
                  try
                    MPMod2(APriv.oD1,X,D1);
                    MPGarnerCRT(D,[X,Y],[D1,APriv.oD2]);
                    MPMul2(X,Y,L);
                    MPInvMod(D,L,APubl.E);
                  finally
                    MPDealloc(D1);
                  end;
                finally
                  MPDealloc(D);
                end;
              end else begin
                MPBinaryGCD(X,Y,G);
                if not Compability then begin
                  MPDiv(M,G,L);
                  MPInvMod(APriv.tD,L,APubl.E);
                end else
                  MPInvMod(APriv.tD,M,APubl.E);
              end;
            finally
              MPDealloc(L);
            end;
          finally
            MPDealloc(G);
          end;
        finally
          MPDealloc(M);
        end;
      finally
        MPDealloc(Y);
      end;
    finally
      MPDealloc(X);
    end;
  finally
    MPDealloc(N);
  end;
end;

function KeyIdentifierFromPrivateKey(const APriv: TIFPrivateKey): OctetString;
var
  Publ: TIFPublicKey;
  PublKeyInfo: TX509PublicKey;
  V: TASN1Struct;
begin
  FillChar(Publ,SizeOf(Publ),0);
  try
    IFRSAPublicKey(APriv,Publ);
    PublKeyInfo := TX509PublicKey.CreateFromIFKey(Publ,False,haSHA1,haSHA1,'');
    try
      V := nil;
      try
        TranslatePublicKey(PublKeyInfo,V);
        Result := DigestString(haSHA1,V.Items[1].ContentAsOctetString);
      finally
        V.Free;
      end;
    finally
      PublKeyInfo.Free;
    end;
{
    PublKeyInfo := TSubjectPublicKeyInfo.Create(nil,nil);
    try
      PublKeyInfo.ImposeSubjectRSAPublicKey(Publ);
      Result := PublKeyInfo.PublicKeyIdentifier;
    finally
      PublKeyInfo.Free;
    end;
}
  finally
    DisposeIFPublicKey(Publ);
  end;
end;

procedure IFRWKeys(ABitSize: Cardinal;
                   AReprType: Byte;
                   AStrongPrimes: Boolean;
                   var APriv: TIFPrivateKey;
                   var APubl: TIFPublicKey);
var
  P, Q, N, X, Y, M, G, L, D: PMPInteger;
  Params: TStrongPrimeParams;
begin
  Assert(AReprType < 3);
  Assert((APubl.E = nil) or (APubl.E^.Data[APubl.E^.Size - 1] and 1 = 0));
  APriv.ReprType := AReprType;
  APriv.Scheme := ifRW;
  APubl.Scheme := ifRW;

  if APubl.E = nil then
    APubl.E := IntToMPInt(2);

  P := nil;
  try
    Params.PMin := nil;
    Params.PUBound := nil;
    Params.F := MPCopy(APubl.E);
    MPShr(Params.F,1);
    Params.Mod8 := 3;
    if AStrongPrimes then begin
      Params.LT := ABitSize div 6;
      Params.LR := ABitSize div 5;
      Params.LS := ABitSize div 5;
      MPPrime(P,(ABitSize + 1) shr 1,paStrongPrime,@Params);
    end else
      MPPrime(P,(ABitSize + 1) shr 1,paStandard,@Params);
    if AReprType = 1 then
      MPCopy2(P,APriv.oP)
    else if AReprType = 2 then
      MPCopy2(P,APriv.tP);
    Q := nil;
    try
      X := IntToMPInt(1);
      try
        MPShl(X,ABitSize-1);
        MPDiv(X,P,Params.PMin);
        MPInc(Params.PMin);
      finally
        MPDealloc(X);
      end;
      X := IntToMPInt(1);
      try
        MPShl(X,ABitSize);
        MPDiv(X,P,Params.PMin);
        MPInc(Params.PUBound);
      finally
        MPDealloc(X);
      end;
      Params.F := APubl.E;
      Params.Mod8 := 7;
      if AStrongPrimes then begin
        Params.LT := ABitSize div 3;
        Params.LR := 2*ABitSize div 5;
        Params.LS := 2*ABitSize div 5;
        MPPrime(Q,0,paStrongPrime,@Params);
      end else
        MPPrime(Q,0,paStandard,@Params);
      MPDealloc(Params.PMin);
      MPDealloc(Params.PUBound);
      MPDealloc(Params.F);
      if AReprType = 1 then
        MPCopy2(Q,APriv.oQ)
      else if AReprType = 2 then
        MPCopy2(Q,APriv.tQ);

      N := nil;
      try
        MPMul2(P,Q,N);
        MPRealloc(N,(MPMSB(N) + 31) shr 5);
        if AReprType = 0 then
          MPCopy2(N,APriv.zN);
        MPCopy2(N,APubl.N);
        X := nil;
        try
          MPDec2(P,X);
          Y := nil;
          try
            MPDec2(Q,Y);
            M := nil;
            try
              MPMul2(X,Y,M);
              G := nil;
              try
                MPBinaryGCD(X,Y,G);
                L := nil;
                try
                  MPDiv(M,G,L);
                  MPShr(M,1);
                  D := nil;
                  try
                    MPInvMod(APubl.E,L,D);
                    if AReprType = 0 then
                      MPCopy2(D,APriv.zD)
                    else if AReprType = 2 then
                      MPCopy2(D,APriv.tD)
                    else begin
                      MPCopy2(D,aPriv.oD1);
                      MPDiv(aPriv.oD1,X,M);
                      MPCopy2(D,aPriv.oD2);
                      MPDiv(aPriv.oD2,Y,M);
                      MPInvMod(Q,P,aPriv.oC);
                    end;
                  finally
                    MPDealloc(D);
                  end;
                finally
                  MPDealloc(L);
                end;
              finally
                MPDealloc(G);
              end;
            finally
              MPDealloc(M);
            end;
          finally
            MPDealloc(Y);
          end;
        finally
          MPDealloc(X);
        end;
      finally
        MPDealloc(N);
      end;
    finally
      MPDealloc(Q);
    end;
  finally
    MPDealloc(P);
  end;
end;

procedure IFPrivateKeyOperation(const APriv: TIFPrivateKey; I: PMPInteger;
                                var J: PMPInteger);
var
  N, J1, J2, H, X, D, C: PMPInteger;
begin
  case APriv.ReprType of
    0: begin
         Assert(MPCmpOffset(APriv.zN,I) > 0,'Input must be smaller than the modulus');
{$IFDEF DEMO}
         if {MPMSB(APriv.zN) > 1024} False then
           raise Exception.Create('This application was built using StreamSec StrSecII components'#13#10 +
                                  'Copyright 2002 (c) StreamSec'#13#10 +
                                  'You MUST purchase the registred version to use private keys larger than 1024 bits.');
{$ENDIF}
         MPKaratsubaExpMod(I,APriv.zD,APriv.zN,J);
       end;
    1: begin
         N := nil;
         try
           MPMul2(APriv.oP,APriv.oQ,N);
{$IFDEF DEMO}
           if {MPMSB(N) > 1024} False then
             raise Exception.Create('This application was built using StreamSec StrSecII components'#13#10 +
                                    'Copyright 2002 (c) StreamSec'#13#10 +
                                    'You MUST purchase the registred version to use private keys larger than 1024 bits.');
{$ENDIF}
           if not MPCmpOffset(N,I) > 0 then
             Exit;
         finally
           MPDealloc(N);
         end;
         J1 := nil;
         try
           MPKaratsubaExpMod(I,APriv.oD1,APriv.oP,J1);
           J2 := nil;
           try
             MPKaratsubaExpMod(I,APriv.oD2,APriv.oQ,J2);
             H := nil;
             try
               MPSub2(J1,J2,H);
               if H^.Sign = -1 then
                 MPAdd(H,APriv.oP);
               MPMulMod(H,APriv.oC,APriv.oP);
               MPMul(H,APriv.oQ);
               MPAdd2(H,J2,J);
             finally
               MPDealloc(H);
             end;
           finally
             MPDealloc(J2);
           end;
         finally
           MPDealloc(J1);
         end;
       end;
    2: begin
         N := nil;
         try
           MPMul2(APriv.tP,APriv.tQ,N);
{$IFDEF DEMO}
           if {MPMSB(N) > 1024} False then
             raise Exception.Create('This application was built using StreamSec StrSecII components'#13#10 +
                                    'Copyright 2002 (c) StreamSec'#13#10 +
                                    'You MUST purchase the registred version to use private keys larger than 1024 bits.');
{$ENDIF}
           if not MPCmpOffset(N,I) > 0 then
             Exit;
         finally
           MPDealloc(N);
         end;
         X := nil;
         try
           MPDec2(APriv.tP,X);
           D := MPCopy(APriv.tD);
           try
             MPMod(D,X);
             J1 := nil;
             try
               MPKaratsubaExpMod(I,D,APriv.tP,J1);
               J2 := nil;
               try
                 MPDec2(APriv.tQ,X);
                 MPCopy2(APriv.tD,D);
                 MPMod(D,X);
                 MPKaratsubaExpMod(I,D,APriv.tQ,J2);
                 C := nil;
                 try
                   MPInvMod(APriv.tQ,APriv.tP,C);
                   H := nil;
                   try
                     MPSub2(J1,J2,H);
                     if H^.Sign = -1 then
                       MPAdd(H,APriv.tP);
                     MPMulMod(H,C,APriv.tP);
                     MPMul(H,APriv.tQ);
                     MPAdd2(H,J2,J);
                   finally
                     MPDealloc(H);
                   end;
                 finally
                   MPDealloc(C);
                 end;
               finally
                 MPDealloc(J2);
               end;
             finally
               MPDealloc(J1);
             end;
           finally
             MPDealloc(D);
           end;
         finally
           MPDealloc(X);
         end;
       end;
    3: if not APriv.ExternalKey.IFPrivateKeyOperation(I,J) then
         raise Exception.Create('Private key operation failed');
  end;
end;

procedure IFEP_RSA(const APubl: TIFPublicKey; F: PMPInteger; var G: PMPInteger);
begin
  Assert(APubl.Scheme in [ifRSA1,ifRSA2],'Not a RSA key');
  Assert((F^.Sign = 1) and (MPCmpOffset(F,APubl.N) < 0));
  MPExpMod(F,APubl.E,APubl.N,G);
end;

procedure IFDP_RSA(const APriv: TIFPrivateKey; G: PMPInteger; var F: PMPInteger);
begin
  Assert(APriv.Scheme in [ifRSA1,ifRSA2],'Not a RSA key');
  IFPrivateKeyOperation(APriv,G,F);
end;     

function ByteSwap(Value: Cardinal): Cardinal;
asm
  bswap EAX
end;

procedure MGF1(const Z; zLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var O; oLen: Integer);
var
  H: THash;
  HC: THashClass;
  Counter, cThreshold, CB: LongWord;
  OB: Pointer;
  HB: packed array [0..63] of Byte;
begin
  Assert((SizeOf(Integer) = 4) or (zLen >= (oLen shr 32)),'Output too large for MGF1');
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    cThreshold := Cardinal((oLen + HC.DigestSize - 1) div HC.DigestSize)
  else
    raise Exception.Create('MGF1: Unsupported hash algorithm');
  OB := @O;
  for Counter := 1 to cThreshold do begin
    if Assigned(HC) then
      H := HC.Create(Z,zLen)
    else
      raise Exception.Create('MGF1: Unsupported hash algorithm');
    try
      CB := ByteSwap(Counter - 1);
      H.HashData(CB,4);
      H.Done(@HB);
      if oLen >= H.DigestSize then
        Move(HB,OB^,H.DigestSize)
      else
        Move(HB,OB^,oLen);
      Dec(oLen,H.DigestSize);
      Inc(LongInt(OB),H.DigestSize);
    finally
      H.Free;
    end;
  end;
end;

function GetEncodingRangeEME1(AKeyBits: Cardinal;
                              AHashAlgorithm: THashAlgorithm;
                              var MaxBytes: Integer): Boolean;
var
  HC: THashClass;
  hLen, oLen: Cardinal;
begin
  HC := FindHashClass(AHashAlgorithm);
  if Assigned(HC) then
    hLen := HC.DigestSize
  else
    hLen := 0;
  if hLen < 20 then
  // The caller is way out of line. Raise an exception:
    raise Exception.Create('EME1Encoding: Unsupported hash algorithm');

  // Is the digest size of the hash too large for the key?
  oLen := (AKeyBits-1) shr 3;
  Result := oLen > 2*hLen;
  // If not, calculate the maximum size of a plain text block and return true
  if Result then
    MaxBytes := oLen - 2*hLen - 1;
end;

procedure EME1Encoding(const M; MLen: Integer;
                       const P; PLen: Integer;
                       HashAlgorithm: THashAlgorithm;
                       MGFHashAlgorithm: THashAlgorithm;
                       FBitSize: Cardinal;
                       var F: PMPInteger);
var
  HC: THashClass;
  H: THash;
  hLen, oLen, zeroLen, I: Cardinal;
  Seed, T, mask: PChar;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    hLen := HC.DigestSize
  else
    hLen := 0;
  if hLen < 20 then
    raise Exception.Create('EME1Encoding: Unsupported hash algorithm');
  oLen := FBitSize shr 3;
  Assert((Cardinal(MLen) <= oLen - 2*hLen - 1) and (oLen > 2*hLen));
  zeroLen := oLen - Cardinal(MLen) - 2*hLen - 1;
  GetMem(T,oLen);
  try
    FillChar(T^,oLen,0);
    try
      T[zeroLen + 2*hLen] := #1;
      Move(M,T[zeroLen + 2*hLen + 1],MLen);
      if Assigned(HC) then
        H := HC.Create(P,PLen)
      else
        raise Exception.Create('EME1Encoding: Unsupported hash algorithm');
      try
        H.Done(T + hLen);
      finally
        H.Free;
      end;
      GetMem(Seed,hLen);
      try
        RawRandom(Seed^,hLen);
        try
          GetMem(Mask,oLen - hLen);
          try
            MGF1(Seed^,hLen,MGFHashAlgorithm,Mask^,oLen - hLen);
            try
              for I := 0 to oLen - hLen - 1 do
                T[I + hLen] := Char(Byte(T[I + hLen]) xor Byte(Mask[I]));
            finally
              ProtectClear(Mask^,oLen - hLen);
            end;
          finally
            FreeMem(Mask);
          end;
          MGF1(T[hLen],oLen - hLen,HashAlgorithm,T^,hLen);
          for I := 0 to hLen - 1 do
            T[I] := Char(Byte(T[I]) xor Byte(Seed[I]));
        finally
          ProtectClear(Seed^,hLen);
        end;
      finally
        FreeMem(Seed);
      end;
      Base256ToUMPInt(F,T^,oLen,FBitSize);
    finally
      ProtectClear(T^,oLen);
    end;
  finally
    FreeMem(T);
  end;
end;

function EME1Decoding(F: PMPInteger;
                      var   M; var MLen: Integer;
                      const P; PLen: Integer;
                      HashAlgorithm: THashAlgorithm;
                      MGFHashAlgorithm: THashAlgorithm;
                      FBitSize: Cardinal): Boolean;
var
  HC: THashClass;
  H: THash;
  hLen, oLen, zeroLen, I: Cardinal;
  Seed, T, mask: PChar;
begin
  Result := MPMSB(F) <= Integer(FBitSize);
  if not Result then Exit;
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    hLen := HC.DigestSize
  else
    hLen := 0;
  if hLen < 20 then
    raise Exception.Create('EME1Decoding: Unsupported hash algorithm');
  oLen := FBitSize shr 3;
  Result := (oLen > 2*hLen);
  if not Result then Exit;
  GetMem(T,oLen);
  try
    UMPIntToBase256(F,T^,oLen);
    try
      GetMem(Seed,hLen);
      try
        try
          MGF1(T[hLen],oLen - hLen,HashAlgorithm,Seed^,hLen);
          for I := 0 to hLen - 1 do
            Seed[I] := Char(Byte(T[I]) xor Byte(Seed[I]));
          GetMem(Mask,oLen - hLen);
          try
            try
              MGF1(Seed^,hLen,MGFHashAlgorithm,Mask^,oLen - hLen);
              for I := 0 to oLen - hLen - 1 do
                T[I + hLen] := Char(Byte(T[I + hLen]) xor Byte(Mask[I]));
            finally
              ProtectClear(Mask^,oLen - hLen);
            end;
          finally
            FreeMem(Mask);
          end;
          if Assigned(HC) then
            H := HC.Create(P,PLen)
          else
            raise Exception.Create('EME1Decoding: Unsupported hash algorithm');
          try
            H.Done(Seed);
          finally
            H.Free;
          end;
          Result := True;
          for I := 0 to hLen - 1 do
            Result := Result and (Seed[I] = T[I + hLen]);
        finally
          ProtectClear(Seed^,hLen);
        end;
      finally
        FreeMem(Seed);
      end;
      if Result then begin
        zeroLen := 0;
        while T[zeroLen + 2*hLen] = #0 do
          Inc(zeroLen);
        Result := Result and (T[2*hLen + zeroLen] = #1);
        if Result then begin
          Assert(MLen >= Integer(oLen - zeroLen - 2*hLen - 1));
          MLen := oLen - zeroLen - 2*hLen - 1;
          Move(T[zeroLen + 2*hLen + 1],M,MLen);
        end;
      end;
    finally
      ProtectClear(T^,oLen);
    end;
  finally
    FreeMem(T);
  end;
end;

function GetEncodingRangeEME_PKCS1_v1_5(AKeyBits: Cardinal;
                                        var MaxBytes: Integer): Boolean;
var
  oLen: Cardinal;
begin
  Result := AKeyBits >= 12*8;
  oLen := (AKeyBits-1) shr 3;
  MaxBytes := oLen - 11;
end;

procedure EME_PKCS1_v1_5_Encoding(const M; MLen: Integer;
                                  FBitSize: Cardinal;
                                  var F: PMPInteger);
var
  PM: PChar;
  EM: PChar;
  oLen, pLen, tLen, I, J: Integer;
begin
  oLen := FBitSize shr 3;
  Assert(mLen + 10 <= OLen,'Message too large');
  GetMem(EM,oLen);
  try
    pLen := oLen-mLen-2;
    tLen := oLen + ((oLen + 255) shr 7);
    try
      GetMem(PM,tLen);
      try
        try
          J := 0;
          repeat
            I := 0;
            RawRandom(PM^,tLen shl 3);
            while (I < tLen) and (J < pLen) do begin
              if PM[I] <> #0 then begin
                EM[J+1] := PM[I];
                Inc(J);
              end;
              Inc(I);
            end;
          until J = pLen;
        finally
          ProtectClear(PM^,tLen);
        end;
      finally
        FreeMem(PM);
      end;
      EM[0] := #2;
      EM[pLen+1] := #0;
      Move(M,EM[pLen+2],MLen);
      Base256ToUMPInt(F,EM^,oLen,FBitSize);
    finally
      ProtectClear(EM^,oLen);
    end;
  finally
    FreeMem(EM);
  end;
end;

function EME_PKCS1_v1_5_Decoding(F: PMPInteger;
                                 var M; var MLen: Integer;
                                 FBitSize: Cardinal): Boolean;
var
  EM: PChar;
  oLen, I: Integer;
begin
  oLen := FBitSize shr 3;
  Result := (MPMSB(F) <= oLen*8) and (oLen >= 10);
  if not Result then Exit;
  GetMem(EM,oLen);
  try
    try
      UMPIntToBase256(F,EM^,oLen);
      Result := EM[0] = #2;
      if Result then begin
        I := 1;
        while (I < oLen) and (EM[I] <> #0) do
          Inc(I);
        Result := I > 8;
        if Result then begin
          Assert(MLen >= oLen - I - 1,'Buffer too small');
          MLen := oLen - I - 1;
          Move(EM[I+1],M,MLen);
        end;
      end;
    finally
      ProtectClear(EM^,oLen);
    end;
  finally
    FreeMem(EM);
  end;
end;

procedure IFESEncryption(const AKey: TIFPublicKey;
                         const Msg; MsgCount: Integer;
                         const P; PCount: Integer;
                         HashAlgorithm: THashAlgorithm;
                         MGFHashAlgorithm: THashAlgorithm;
                         EncryptEncoding: TEncryptEncoding;
                         var EMsg; var EMsgLen: Integer);
var
  FBitSize, ELen: Cardinal;
  F, G: PMPInteger;
begin
  Assert(AKey.Scheme in [ifRSA1,ifRSA2],'Not a RSA key');
  FBitSize := MPMSB(AKey.N) - 1;
  F := nil;
  try
    case EncryptEncoding of
      eeEME1:           EME1Encoding(Msg,MsgCount,P,PCount,HashAlgorithm,MGFHashAlgorithm,FBitSize,F);
      eeEME_PKCS1_v1_5: EME_PKCS1_v1_5_Encoding(Msg,MsgCount,FBitSize,F);
    end;
    G := nil;
    try
      IFEP_RSA(AKey,F,G);
      ELen := (MPMSB(G) + 7) shr 3;
      if Integer(ELen) > EMsgLen then
        EMsgLen := 0
      else begin
        EMsgLen := ELen;
        UMPIntToBase256(G,EMsg,EMsgLen);
      end;
    finally
      MPDealloc(G);
    end;
  finally
    MPDealloc(F);
  end;
end;

procedure IFESEncryption(const AKey: TIFPublicKey;
                         const Msg; MsgCount: Integer;
                         HashAlgorithm: THashAlgorithm;
                         EncryptEncoding: TEncryptEncoding;
                         var EMsg; var EMsgLen: Integer);
begin
  IFESEncryption(AKey,
                 Msg,MsgCount,
                 nil^,0,
                 HashAlgorithm,
                 Low(THashAlgorithm),
                 EncryptEncoding,
                 EMsg,EMsgLen);
end;

function IFESDecryption(const AKey: TIFPrivateKey;
                        const EMsg; EMsgCount: Integer;
                        const P; PCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        MGFHashAlgorithm: THashAlgorithm;
                        EncryptEncoding: TEncryptEncoding;
                        var Msg; var MsgLen: Integer): Boolean;
var
  FBitSize: Cardinal;
  N, F, G: PMPInteger;
begin
  Assert(AKey.Scheme in [ifRSA1,ifRSA2],'Not a RSA key');

  if (AKey.ReprType = 3) and Assigned(AKey.ExternalKey) then begin
    Result := False;
    if AKey.ExternalKey.IFESDecryption(EMsg,EMsgCount,
                                       P,PCount,
                                       HashAlgorithm,
                                       MGFHashAlgorithm,
                                       EncryptEncoding,
                                       Msg,MsgLen,Result) then
      Exit;
  end;

  if AKey.ReprType = 3 then
    FBitSize := AKey.ExternalKey.GetKeySize - 1
  else if AKey.ReprType = 0 then
    FBitSize := MPMSB(AKey.zN) - 1
  else begin
    N := nil;
    try
      if AKey.ReprType = 1 then
        MPMul2(AKey.oP,AKey.oQ,N)
      else if AKey.ReprType = 2 then
        MPMul2(AKey.tP,AKey.tQ,N);
      FBitSize := MPMSB(N) - 1;
    finally
      MPDealloc(N);
    end;
  end;
  G := nil;
  try
    Base256ToUMPInt(G,EMsg,EMsgCount,FBitSize + 1);
    F := nil;
    try
      IFDP_RSA(AKey,G,F);
      Result := Assigned(F);
      if Result then
        case EncryptEncoding of
          eeEME1:           Result := EME1Decoding(F,Msg,MsgLen,P,PCount,HashAlgorithm,MGFHashAlgorithm,FBitSize);
          eeEME_PKCS1_v1_5: Result := EME_PKCS1_v1_5_Decoding(F,Msg,MsgLen,FBitSize);
        else
          Result := False;
        end;
    finally
      MPDealloc(F);
    end;
  finally
    MPDealloc(G);
  end;
end;

function IFESDecryption(const AKey: TIFPrivateKey;
                        const EMsg; EMsgCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        EncryptEncoding: TEncryptEncoding;
                        var Msg: ISecretKey): Boolean; overload;
var
  Tmp: ISecretKey;
  MsgLen: Integer;
begin
  Tmp := TSecretKey.Create('');
  Tmp.SetLength(EMsgCount);
  MsgLen := Tmp.KeyLen;
  Result := IFESDecryption(AKey,
                           EMsg,EMsgCount,
                           nil^,0,
                           HashAlgorithm,
                           Low(THashAlgorithm),
                           EncryptEncoding,
                           Tmp.Key^,MsgLen);
  if Result then begin
    if Msg = nil then
      Msg := TSecretKey.Create('');
    Msg.SetKey(Tmp.Key,MsgLen,0);
  end else
    Msg := nil;
end;

function IFESDecryption(const AKey: TIFPrivateKey;
                        const EMsg; EMsgCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        EncryptEncoding: TEncryptEncoding;
                        var Msg; var MsgLen: Integer): Boolean;
begin
  Result := IFESDecryption(AKey,
                           EMsg,EMsgCount,
                           nil^,0,
                           HashAlgorithm,
                           Low(THashAlgorithm),
                           EncryptEncoding,
                           Msg,MsgLen);
end;

procedure IFSP_RSA1(const APriv: TIFPrivateKey; F: PMPInteger; var S: PMPInteger);
begin
  Assert(APriv.Scheme = ifRSA1,'Not a RSA1 key');
  IFPrivateKeyOperation(APriv,F,S);
end;

function IFVP_RSA1(const APubl: TIFPublicKey; S: PMPInteger; var F: PMPInteger): Boolean;
begin
  Assert(APubl.Scheme = ifRSA1,'Not a RSA1 key');
  Result := (S^.Sign = 1) and (MPCmpOffset(S,APubl.N) < 0);
  if Result then
    MPExpMod(S,APubl.E,APubl.N,F);
end;

procedure IFSP_RSA2(const APriv: TIFPrivateKey; F: PMPInteger; var S: PMPInteger);
var
  T, N: PMPInteger;
begin
  Assert(APriv.Scheme = ifRSA2,'Not a RSA2 key');
  Assert(MPModByInt(F,16) = 12,'IFSP_RSA2: Message must be congruent to 12 (mod 16)');
  T := nil;
  try
    IFPrivateKeyOperation(APriv,F,T);
    N := nil;
    try
      case APriv.ReprType of
        0: MPCopy2(APriv.zN,N);
        1: MPMul2(APriv.oP,APriv.oQ,N);
        2: MPMul2(APriv.tP,APriv.tQ,N);
      end;
      MPSub(N,T);
      if MPCmpOffset(T,N) > 0 then
        MPCopy2(N,S)
      else
        MPCopy2(T,S);
    finally
      MPDealloc(N);
    end;
  finally
    MPDealloc(T);
  end;
end;

function IFVP_RSA2(const APubl: TIFPublicKey; S: PMPInteger; var F: PMPInteger): Boolean;
var
  vS, T: PMPInteger;
begin
  Assert(APubl.Scheme = ifRSA2,'Not a RSA2 key');
  vS := MPCopy(S);
  try
    MPShl(vS,1);
    Result := (S^.Sign = 1) and (MPCmpOffset(vS,APubl.N) < 0);
  finally
    MPDealloc(vS);
  end;
  if Result then begin
    T := nil;
    try
      MPExpMod(S,APubl.E,APubl.N,T);
      if MPModByInt(T,16) = 12 then
        MPCopy2(T,F)
      else begin
        MPSub2(APubl.N,T,F);
        Result := MPModByInt(F,16) = 12;
      end;
    finally
      MPDealloc(T);
    end;
  end;
end;

procedure IFSP_RW(const APriv: TIFPrivateKey; F: PMPInteger; var S: PMPInteger);
var
  U, T, N: PMPInteger;
  J: Integer;
begin
  Assert(APriv.Scheme = ifRW,'Not a RW key');
  Assert(MPModByInt(F,16) = 12,'IFSP_RW: Message must be congruent to 12 (mod 16)');
  U := MPCopy(F);
  try
    case APriv.ReprType of
      0: J := MPJacobiSymbol(U,APriv.zN);
      1: J := MPJacobiSymbol(U,APriv.oP)*MPJacobiSymbol(U,APriv.oQ);
      2: J := MPJacobiSymbol(U,APriv.tP)*MPJacobiSymbol(U,APriv.tQ);
    else
      J := 0; // Keeps the compiler happy;
      Assert(False);
    end;
    if J <> 1 then
      MPShr(U,1);
    T := nil;
    try
      IFPrivateKeyOperation(APriv,U,T);
      N := nil;
      try
        case APriv.ReprType of
          0: MPCopy2(APriv.zN,N);
          1: MPMul2(APriv.oP,APriv.oQ,N);
          2: MPMul2(APriv.tP,APriv.tQ,N);
        end;
        MPSub(N,T);
        if MPCmpOffset(T,N) > 0 then
          MPCopy2(N,S)
        else
          MPCopy2(T,S);
      finally
        MPDealloc(N);
      end;
    finally
      MPDealloc(T);
    end;
  finally
    MPDealloc(U);
  end;
end;           

function IFVP_RW(const APubl: TIFPublicKey; S: PMPInteger; var F: PMPInteger): Boolean;
var
  vS, T1,T2: PMPInteger;
begin
  Assert(APubl.Scheme = ifRW,'Not a RW key');
  vS := MPCopy(S);
  try
    MPShl(vS,1);
    Result := (S^.Sign = 1) and (MPCmpOffset(vS,APubl.N) < 0);
  finally
    MPDealloc(vS);
  end;
  if Result then begin
    T1 := nil;
    try
      MPExpMod(S,APubl.E,APubl.N,T1);
      if T1^.Data[T1^.Size - 1] and $F = 12 then
        MPCopy2(T1,F)
      else if T1^.Data[T1^.Size - 1] and $7 = 6 then begin
        MPAdd(T1,T1);
        MPCopy2(T1,F);
      end else begin
        T2 := nil;
        try
          MPSub2(APubl.N,T1,T2);
          if T2^.Data[T2^.Size - 1] and $F = 12 then
            MPCopy2(T2,F)
          else if T2^.Data[T2^.Size - 1] and $7 = 6 then begin
            MPAdd(T2,T2);
            MPCopy2(T2,F);
          end else
            Result := False;
        finally
          MPDealloc(T2);
        end;
      end;
    finally
      MPDealloc(T1);
    end;
  end;
end;

{$IFDEF SHA1_OR_RIPEMD160}
procedure EMSA2(const Digest; DigestCount: Integer;
                HashAlgorithm: THashAlgorithm;
                NullMsg: Boolean;
                FBitSize: Cardinal;
                var F: PMPInteger);
var
  T: string;
  I, J: Integer;
begin
  Assert(IsSHA1orRipeMD160(HashAlgorithm) and (DigestCount = 20));
  Assert((FBitSize >= 191) and (FBitSize and 7 = 7));
  SetLength(T,(FBitSize + 1) shr 3);
  if NullMsg then
    T[1] := #$4B
  else
    T[1] := #$6B;
  J := ((FBitSize + 1) shr 3) - 24;
  for I := 2 to 1 + J do
    T[I] := #$BB;
  T[J + 2] := #$BA;
  Move(Digest,T[J + 3],DigestCount);
{$IFDEF SHA1}
  if HashAlgorithm = haSHA1 then
    T[J + 23] := #$33
  else
{$ENDIF SHA1}
{$IFDEF RIPEMD160}
  if HashAlgorithm = haRipeMD160 then
    T[J + 23] := #$31;
{$ENDIF RIPEMD160}
  T[J + 24] := #$CC;
  Base256ToMPInt(F,T);
  ProtectClear(T[1],Length(T));
end;

function EMSA2Verification(F: PMPInteger;
                           const Digest; DigestCount: Integer;
                           HashAlgorithm: THashAlgorithm;
                           NullMsg: Boolean;
                           FBitSize: Cardinal): Boolean;
var
  T: string;
  I, J: Integer;
begin
  Assert(IsSHA1orRipeMD160(HashAlgorithm) and (DigestCount = 20));
  Assert((FBitSize >= 191) and (FBitSize and 7 = 7));
  T := StringOfChar(#0,(FBitSize + 1) shr 3);
  MPIntToBase256(F,T[1],Length(T));
  Result := (NullMsg and (T[1] = #$4B)) or ((T[1] = #$6B) and not NullMsg);
  if Result then begin
    J := ((FBitSize + 1) shr 3) - 24;
    for I := 2 to 1 + J do
      Result := Result and (T[I] = #$BB);
    if Result then begin
      case T[Length(T) - 1] of
        {$IFDEF SHA1}     #$33: Result := HashAlgorithm = haSHA1;{$ENDIF}
        {$IFDEF RIPEMD160}#$31: Result := HashAlgorithm = haRipeMD160;{$ENDIF}
      else
        Result := False;
      end;
      if Result then begin
        Result := T[Length(T)] = #$CC;
        if Result then
          Result := CompareMem(@Digest,@T[J + 3],20);
      end;
    end;
  end;
  ProtectClear(T[1],Length(T));
end;
{$ENDIF SHA1_OR_RIPEMD160}

procedure EMSA3(const Digest; DigestCount: Integer;
                HashAlgorithm: THashAlgorithm;
                NullMsg: Boolean;
                FBitSize: Cardinal;
                var F: PMPInteger);
var
  HC: THashClass;
  T: string;
  HashIDLen,I, J: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then begin
    Assert(DigestCount = HC.DigestSize);
    HashIDLen := HC.DERPrefixLen;
  end else
    HashIDLen := 0;
  Assert(FBitSize >= Cardinal(80 + 8*DigestCount + 8*HashIDLen));
  SetLength(T,FBitSize shr 3);
  T[1] := #01;
  J := LongInt(FBitSize shr 3) - HashIDLen - DigestCount - 2;
  for I := 2 to 1 + J do
    T[I] := #$FF;
  T[J + 2] := #0;
  if Assigned(HC) then
    Move(HC.DERPrefix^,T[J + 3],HashIDLen);
  Move(Digest,T[J + 3 + HashIDLen],DigestCount);
  Base256ToMPInt(F,T);
  ProtectClear(T[1],Length(T));
end;

function EMSA3Verification(F: PMPInteger;
                           const Digest; DigestCount: Integer;
                           HashAlgorithm: THashAlgorithm;
                           NullMsg: Boolean;
                           FBitSize: Cardinal): Boolean;
var
  HC: THashClass;
  T: string;
  HashIDLen,I, J: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then begin
    // Check HashID:
    Assert(DigestCount = HC.DigestSize);
    HashIDLen := HC.DERPrefixLen;
    Result := FBitSize >= Cardinal(80 + 8*DigestCount + 8*HashIDLen);
    if Result then begin
      SetLength(T,FBitSize shr 3);
      MPIntToBase256(F,T[1],Length(T));
      Result := T[1] = #01;
      J := LongInt(FBitSize shr 3) - HashIDLen - DigestCount - 2;
      for I := 2 to 1 + J do
        Result := Result and (T[I] = #$FF);
      Result := Result and (T[J + 2] = #0);
      if Assigned(HC) then
        Result := Result and CompareMem(HC.DERPrefix,@T[J + 3],HashIDLen);
      if Result then
        Result := CompareMem(@Digest,@T[J + 3 + HashIDLen],DigestCount);
      ProtectClear(T[1],Length(T));
    end;
  end else begin
    // Ignore HashID:
    Result := FBitSize >= Cardinal(80 + 8*DigestCount);
    if Result then begin
      SetLength(T,FBitSize shr 3);
      MPIntToBase256(F,T[1],Length(T));
      Result := T[1] = #01;
      J := LongInt(FBitSize shr 3) - DigestCount - 2;
      for I := 2 to 2 + J do
        case T[I] of
          #$FF:
            Result := True;
          #$00:
            begin
              Result := True;
              Break;
            end;
        else
          Result := False;
          Break;
        end;
      if Result then
        Result := CompareMem(@Digest,@T[J + 3],DigestCount);
      ProtectClear(T[1],Length(T));
    end;
  end;
end;

procedure EMSA_TLS(const Digest; DigestCount: Integer;
                   FBitSize: Cardinal;
                   var F: PMPInteger);
var
  T: string;
  I, J: Integer;
begin
  Assert(DigestCount = 36);
  Assert(FBitSize >= Cardinal(80 + 8*DigestCount));
  SetLength(T,FBitSize shr 3);
  T[1] := #01;
  J := LongInt(FBitSize shr 3) - DigestCount - 2;
  for I := 2 to 1 + J do
    T[I] := #$FF;
  T[J + 2] := #0;
  Move(Digest,T[J + 3],DigestCount);
  Base256ToMPInt(F,T);
  ProtectClear(T[1],Length(T));
end;

function EMSA_TLSVerification(F: PMPInteger;
                              const Digest; DigestCount: Integer;
                              FBitSize: Cardinal): Boolean;
var
  T: string;
  I, J: Integer;
begin
  Assert(DigestCount = 36);
  Result := FBitSize >= Cardinal(80 + 8*DigestCount);
  if Result then begin
    SetLength(T,FBitSize shr 3);
    MPIntToBase256(F,T[1],Length(T));
    Result := T[1] = #01;
    J := LongInt(FBitSize shr 3) - DigestCount - 2;
    for I := 2 to 1 + J do
      Result := Result and (T[I] = #$FF);
    Result := Result and (T[J + 2] = #0);
    if Result then
      Result := CompareMem(@Digest,@T[J + 3],DigestCount)
    else
      Result := False;
    ProtectClear(T[1],Length(T));
  end;
end;

procedure EMSR3(const M1; M1Len: Integer;
                const H2; H2Len: Integer;
                HashAlgorithm: THashAlgorithm;
                MGFHashAlgorithm: THashAlgorithm;
                HashIden: Boolean;
                SaltLen: Cardinal;
                FBitSize: Cardinal;
                var F: PMPInteger);
var
  HC: THashClass;
  H: THash;
  HB: packed array [0..63] of Byte;
  hLen, oLen, tbits, C, zeroLen, I: Cardinal;
  Salt, T, mask: string;
begin
  Assert(IsSHA1orRipeMD160(HashAlgorithm) or not HashIden);
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    hLen := HC.DigestSize
  else
    raise Exception.Create('EMSR3: Unsupported hash algorithm');
  Assert(M1Len < 1 shl 29);
  Assert((HashIden and
          (Cardinal(M1Len*8) + hLen*8 + SaltLen*8 + 16 + 1 <= FBitSize)) or
         (Cardinal(M1Len*8) + hLen*8 + SaltLen*8 + 8 + 1 <= FBitSize));
  oLen := (FBitSize + 7) shr 3;
  tbits := 8*oLen - FBitSize;
  SetLength(Salt,SaltLen);
  if SaltLen > 0 then
    RawRandom(Salt[1],SaltLen*8);
  C := 0;
  if Assigned(HC) then
    H := HC.Create(C,4)
  else
    raise Exception.Create('EMSR3: Unsupported hash algorithm');
  try
    C := ByteSwap(M1Len*8);
    H.HashData(C,4);
    H.HashData(M1,M1Len);
    H.HashData(H2,H2Len);
    if SaltLen > 0 then
      H.HashData(Salt[1],SaltLen);
    H.Done(@HB);
    if HashIden then
      zeroLen := oLen - Cardinal(M1Len) - SaltLen - hLen - 2
    else
      zeroLen := oLen - Cardinal(M1Len) - SaltLen - hLen - 1;
    T := StringOfChar(#0,oLen);
    T[zeroLen] := #1;
    if M1Len > 0 then
      Move(M1,T[1 + zeroLen],M1Len);
    if SaltLen > 0 then begin
      Move(Salt[1],T[1 + zeroLen + Cardinal(M1Len)],SaltLen);
      ProtectClear(Salt[1],Length(Salt));
    end;
    SetLength(mask,zeroLen + Cardinal(M1Len) + SaltLen);
    MGF1(HB,H.DigestSize,MGFHashAlgorithm,mask[1],Length(mask));
    for I := 1 to Length(mask) do
      T[I] := Char(Byte(T[I]) xor Byte(mask[I]));
    if tbits > 0 then
      T[1] := Char(Byte(T[1]) and ((1 shl (8 - tbits)) - 1));
    Move(HB,T[1 + zeroLen + Cardinal(M1Len) + SaltLen],H.DigestSize);
    if HashIden then begin
      case HashAlgorithm of
        {$IFDEF SHA1}     haSHA1:      T[Length(T) - 1] := #$33;
        {$ELSE  SHA1}     haReserved0: raise Exception.Create('EMSR3: Unsupported hash algorithm');
        {$ENDIF}
        {$IFDEF RIPEMD160}haRipeMD160: T[Length(T) - 1] := #$31;{$ENDIF}
      end;
      T[Length(T)] := #$cc;
    end else
      T[Length(T)] := #$bc;
    Base256ToMPInt(F,T,FBitSize);
    ProtectClear(T[1],Length(T));
  finally
    H.Free;
  end;
end;

function EMSR3Verification(F: PMPInteger;
                           var M1; var M1Len: Integer;
                           const H2; H2Len: Integer;
                           HashAlgorithm: THashAlgorithm;
                           MGFHashAlgorithm: THashAlgorithm;
                           HashIden: Boolean;
                           SaltLen: Cardinal;
                           FBitSize: Cardinal): Boolean;
var
  HC: THashClass;
  H: THash;
  HB: packed array [0..63] of Byte;
  hLen, oLen, tbits, C, I, u: Cardinal;
  T, mask: string;
begin
  Assert(IsSHA1orRipeMD160(HashAlgorithm) or not HashIden);
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    hLen := HC.DigestSize
  else
    raise Exception.Create('EMSR3Verification: Unsupported hash algorithm');
  Result := (HashIden and
             (FBitSize >= hLen*8 + SaltLen*8 + 16 + 1)) or
            ((FBitSize >= hLen*8 + SaltLen*8 + 8 + 1) and not HashIden);
  if Result then begin
    oLen := (FBitSize + 7) shr 3;
    tbits := 8*oLen - FBitSize;
    Result := MPMSB(F) <= Integer(FBitSize);
    if Result then begin
      T := MPIntToBase256(F,oLen);
      if HashIden then begin
        Result := T[oLen] = #$CC;
        if Result then
          case T[oLen - 1] of
            #0: Result := False; // Keep the compiler happy
            {$IFDEF SHA1}     #$33: Result := HashAlgorithm = haSHA1;{$ENDIF}
            {$IFDEF RIPEMD160}#$31: Result := HashAlgorithm = haRipeMD160;{$ENDIF}
          else
            Result := False;
          end;
      end else
        Result := T[oLen] = #$BC;
      if Result then begin
        if HashIden then
          u := 2
        else
          u := 1;
        Move(T[1 + oLen - hLen - u],HB,hLen);
        SetLength(mask,oLen - hLen - u);
        MGF1(HB,hLen,MGFHashAlgorithm,mask[1],Length(mask));
        for I := 1 to Length(mask) do
          T[I] := Char(Byte(T[I]) xor Byte(mask[I]));
        if tbits > 0 then
          T[1] := Char(Byte(T[1]) and ((1 shl (8 - tbits)) - 1));
        I := 1;
        while (T[I] = #0) and (I <= oLen - saltLen - hLen - u) do
          Inc(I);
        Result := (I <= oLen - saltLen - hLen - u);
        if Result then begin
          if T[I] = #1 then begin
            if Cardinal(M1Len) < oLen - saltLen - hLen - u - I then
              raise Exception.Create('EMSR3Verification: Data buffer too small');
            M1Len := oLen - saltLen - hLen - u - I;
            Move(T[I+1],M1,M1Len);
          end else
            Result := False;
          if Result then begin
            C := 0;
            if Assigned(HC) then
              H := HC.Create(C,4)
            else
              raise Exception.Create('EMSR3Verification: Unsupported hash algorithm');
            try
              C := ByteSwap(M1Len*8);
              H.HashData(C,4);
              H.HashData(M1,M1Len);
              H.HashData(H2,H2Len);
              if SaltLen > 0 then
                H.HashData(T[1 + oLen - saltLen - hLen - u],SaltLen);
              H.Done(@HB);
            finally
              H.Free;
            end;
            Result := CompareMem(@T[1 + oLen - hLen - u],@HB,hLen);
          end;
        end;
      end;
    end;
  end;
end;

procedure EMSA4(const Digest; DigestCount: Integer;
                HashAlgorithm: THashAlgorithm;
                MGFHashAlgorithm: THashAlgorithm;
                NullMsg: Boolean;
                FBitSize: Cardinal;
                var F: PMPInteger);
var
  HC: THashClass;
  Dummy, saltLen: Cardinal;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    saltLen := HC.DigestSize
  else
    raise Exception.Create('EMSA4: Unsupported hash algorithm');
  EMSR3(Dummy,0,
        Digest,DigestCount,
        HashAlgorithm,
        MGFHashAlgorithm,
        IsSHA1orRipeMD160(HashAlgorithm),
        saltLen,FBitSize,F);
end;

function EMSA4Verification(F: PMPInteger;
                           const Digest; DigestCount: Integer;
                           HashAlgorithm: THashAlgorithm;
                           MGFHashAlgorithm: THashAlgorithm;
                           NullMsg: Boolean;
                           FBitSize: Cardinal): Boolean;
var
  HC: THashClass;
  DummyLen: Integer;
  saltLen: Cardinal;
  Dummy: PMPInteger;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    saltLen := HC.DigestSize
  else
    raise Exception.Create('EMSA4Verification: Unsupported hash algorithm');
  Dummy := nil;
  try
    MPRealloc(Dummy,F^.Size);
    DummyLen := Dummy^.Size*4;
    Result := EMSR3Verification(F,
                                Dummy^.Data,DummyLen,
                                Digest,DigestCount,
                                HashAlgorithm,
                                MGFHashAlgorithm,
                                IsSHA1orRipeMD160(HashAlgorithm),
                                saltLen,FBitSize);
    Result := Result and (DummyLen = 0);
  finally
    MPDealloc(Dummy);
  end;
end;

function IFSSASignatureGeneration(const AKey: TIFPrivateKey;
                                  const Digest; DigestCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  MGFHashAlgorithm: THashAlgorithm;
                                  Encoding: TSignEncoding;
                                  NullMsg: Boolean;
                                  var Sign; SignCount: Integer): Integer;
var
  FBitSize: Cardinal;
  N, F, S: PMPInteger;
  Done: Boolean;
begin
{$IFDEF SHA1_OR_RIPEMD160}
  Assert(IsSHA1orRipeMD160(HashAlgorithm) or not (Encoding = seEMSA2));         
  Assert((Encoding in [seEMSA2,seEMSA4]) or not (AKey.Scheme in [ifRSA2,ifRW]));
{$ELSE  SHA1_OR_RIPEMD160}
  Assert((Encoding in [seEMSA4]) or not (AKey.Scheme in [ifRSA2,ifRW]));
{$ENDIF SHA1_OR_RIPEMD160}
{$IFDEF DISABLEMD5SIGN}
{$IFDEF MD5}
  Assert(HashAlgorithm <> haMD5);
{$ENDIF MD5}
{$ENDIF DISABLEMD5SIGN}

  if AKey.ReprType = 3 then begin
    Result := SignCount;
    Done := AKey.ExternalKey.SignDigest(Digest,DigestCount,
                                        HashAlgorithm,
                                        MGFHashAlgorithm,
                                        Encoding,
                                        NullMsg,
                                        Sign,Result);
  end else
    Done := False;

  if not Done then begin
    if AKey.ReprType = 3 then
      FBitSize := AKey.ExternalKey.GetKeySize - 1
    else if AKey.ReprType = 0 then
      FBitSize := MPMSB(AKey.zN) - 1
    else begin
      N := nil;
      try
        if AKey.ReprType = 1 then
          MPMul2(AKey.oP,AKey.oQ,N)
        else if AKey.ReprType = 2 then
          MPMul2(AKey.tP,AKey.tQ,N);
        FBitSize := MPMSB(N) - 1;
      finally
        MPDealloc(N);
      end;
    end;
    F := nil;
    try
      case Encoding of
{$IFDEF SHA1_OR_RIPEMD160}
        seEMSA2:    EMSA2(Digest,DigestCount,HashAlgorithm,NullMsg,FBitSize,F);
{$ENDIF SHA1_OR_RIPEMD160}
        seEMSA3:    EMSA3(Digest,DigestCount,HashAlgorithm,NullMsg,FBitSize,F);
        seEMSA4:    EMSA4(Digest,DigestCount,HashAlgorithm,MGFHashAlgorithm,NullMsg,FBitSize,F);
{$IFDEF SHA1_AND_MD5}
        seEMSA_TLS: EMSA_TLS(Digest,DigestCount,FBitSize,F);
{$ENDIF SHA1_AND_MD5}
      end;
      S := nil;
      try
        case AKey.Scheme of
          ifRSA1: IFSP_RSA1(AKey,F,S);
          ifRSA2: IFSP_RSA2(AKey,F,S);
          ifRW:   IFSP_RW(AKey,F,S);
        end;
        Result := ((MPMSB(S) - 1) shr 3) + 1;
        if Result > SignCount then
          raise Exception.Create(Format('Insufficient buffer: %d - %d',[Result,SignCount]));
        MPIntToBase256(S,Sign,Result);
      finally
        MPDealloc(S);
      end;
    finally
      MPDealloc(F);
    end;
  end;
end;

function IFSSASignatureGeneration(const AKey: TIFPrivateKey;
                                  const Msg; MsgCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  MGFHashAlgorithm: THashAlgorithm;
                                  Encoding: TSignEncoding;
                                  var Sign; SignCount: Integer): Integer;
var
  HC: THashClass;
  H: THash;
  D: string;
  Done: Boolean;
begin
{$IFDEF SHA1_AND_MD5}
  if Encoding = seEMSA_TLS then begin
    SetLength(D,36);
    try
      HC := TMD5;
      H := HC.Create(Msg,MsgCount);
      try
        H.Done(Pointer(D));
      finally
        H.Free;
      end;
      HC := TSHA1;
      H := HC.Create(Msg,MsgCount);
      try
        H.Done(@D[17]);
      finally
        H.Free;
      end;
      Result := IFSSASignatureGeneration(AKey,
                                         Pointer(D)^,36,
                                         HashAlgorithm,
                                         MGFHashAlgorithm,
                                         Encoding,
                                         MsgCount = 0,
                                         Sign,SignCount);
    finally
      ProtectClear(Pointer(D)^,Length(D));
    end;
  end else begin
{$ENDIF SHA1_AND_MD5}
    if AKey.ReprType = 3 then begin
      Result := SignCount;
      Done := AKey.ExternalKey.SignMsg(Msg,MsgCount,
                                       HashAlgorithm,
                                       MGFHashAlgorithm,
                                       Encoding,
                                       Sign,Result);
    end else
      Done := False;

    if not Done then begin
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(Msg,MsgCount)
      else
        raise Exception.Create('IFSSASignatureGeneration: Unsupported hash algorithm');
      try
        SetLength(D,H.DigestSize);
        H.Done(Pointer(D));
        Result := IFSSASignatureGeneration(AKey,
                                           Pointer(D)^,Length(D),
                                           H.Algorithm,
                                           MGFHashAlgorithm,
                                           Encoding,
                                           MsgCount = 0,
                                           Sign,SignCount);
      finally
        H.Free;
      end;
    end;
{$IFDEF SHA1_AND_MD5}
  end;
{$ENDIF SHA1_AND_MD5}
end;

function IFSSASignatureVerification(const AKey: TIFPublicKey;
                                    const Digest; DigestCount: Integer;
                                    const Sign; SignCount: Integer;
                                    HashAlgorithm: THashAlgorithm;
                                    MGFHashAlgorithm: THashAlgorithm;
                                    Encoding: TSignEncoding;
                                    NullMsg: Boolean): Boolean;
var
  FBitSize: Cardinal;
  F, S: PMPInteger;
begin
{$IFDEF SHA1_OR_RIPEMD160}
  Assert(IsSHA1orRipeMD160(HashAlgorithm) or not (Encoding = seEMSA2));         
  Assert((Encoding in [seEMSA2,seEMSA4]) or not (AKey.Scheme in [ifRSA2,ifRW]));
{$ELSE  SHA1_OR_RIPEMD160}
  Assert((Encoding in [seEMSA4]) or not (AKey.Scheme in [ifRSA2,ifRW]));
{$ENDIF SHA1_OR_RIPEMD160}
{$IFDEF DISABLEMD5SIGN}
{$IFDEF MD5}
  Assert(HashAlgorithm <> haMD5);
{$ENDIF MD5}
{$ENDIF DISABLEMD5SIGN}
  FBitSize := MPMSB(AKey.N) - 1;
  S := nil;
  try
    Base256ToUMPInt(S,Sign,SignCount,0);
    Result := MPMSB(S) <= Integer(FBitSize) + 1;
    if Result then begin
      F := nil;
      try
        case AKey.Scheme of
          ifRSA1: Result := IFVP_RSA1(AKey,S,F);
          ifRSA2: Result := IFVP_RSA2(AKey,S,F);
          ifRW:   Result := IFVP_RW(AKey,S,F);
        end;
        Result := Result and (MPMSB(F) <= Integer(FBitSize));
        if Result then begin
          case Encoding of
{$IFDEF SHA1_OR_RIPEMD160}
            seEMSA2: Result := EMSA2Verification(F,Digest,DigestCount,HashAlgorithm,NullMsg,FBitSize);
{$ENDIF SHA1_OR_RIPEMD160}
            seEMSA3: Result := EMSA3Verification(F,Digest,DigestCount,HashAlgorithm,NullMsg,FBitSize);
            seEMSA4: Result := EMSA4Verification(F,
                                                 Digest,DigestCount,
                                                 HashAlgorithm,
                                                 MGFHashAlgorithm,
                                                 NullMsg,
                                                 FBitSize);
{$IFDEF SHA1_AND_MD5}
            seEMSA_TLS: Result := EMSA_TLSVerification(F,Digest,DigestCount,FBitSize);
{$ENDIF SHA1_AND_MD5}
          end;
        end;
      finally
        MPDealloc(F);
      end;
    end;
  finally
    MPDealloc(S);
  end;
end;

function IFSSASignatureVerification(const AKey: TIFPublicKey;
                                    const Msg; MsgCount: Integer;
                                    const Sign; SignCount: Integer;
                                    HashAlgorithm: THashAlgorithm;
                                    MGFHashAlgorithm: THashAlgorithm;
                                    Encoding: TSignEncoding): Boolean;
var
  HC: THashClass;
  H: THash;
  D: string;
begin
{$IFDEF SHA1_AND_MD5}
  if Encoding = seEMSA_TLS then begin
    SetLength(D,36);
    try
      HC := TMD5;
      H := HC.Create(Msg,MsgCount);
      try
        H.Done(Pointer(D));
      finally
        H.Free;
      end;
      HC := TSHA1;
      H := HC.Create(Msg,MsgCount);
      try
        H.Done(@D[17]);
      finally
        H.Free;
      end;
      Result := IFSSASignatureVerification(AKey,
                                           Pointer(D)^,36,
                                           Sign,SignCount,
                                           HashAlgorithm,
                                           MGFHashAlgorithm,
                                           Encoding,
                                           MsgCount = 0);
    finally
      ProtectClear(Pointer(D)^,Length(D));
    end;
  end else begin
{$ENDIF SHA1_AND_MD5}
    HC := FindHashClass(HashAlgorithm);
    if Assigned(HC) then
      H := HC.Create(Msg,MsgCount)
    else
      raise Exception.Create('IFSSASignatureVerification: Unsupported hash algorithm');
    try
      SetLength(D,H.DigestSize);
      H.Done(Pointer(D));
      Result := IFSSASignatureVerification(AKey,
                                           Pointer(D)^,Length(D),
                                           Sign,SignCount,
                                           H.Algorithm,
                                           MGFHashAlgorithm,
                                           Encoding,
                                           MsgCount = 0);
    finally
      H.Free;
    end;             
{$IFDEF SHA1_AND_MD5}
  end;
{$ENDIF SHA1_AND_MD5}
end;

{ TExternalIFPrivateKey }

constructor TExternalIFPrivateKey.Create;
begin

end;

function TExternalIFPrivateKey.GetKeySize: Integer;
begin
  Result := -1;
end;

function TExternalIFPrivateKey.IFESDecryption(const EMsg; EMsgCount: Integer;
                                              const P; PCount: Integer;
                                              HashAlgorithm,
                                              MGFHashAlgorithm: THashAlgorithm;
                                              EncryptEncoding: TEncryptEncoding;
                                              var Msg; var MsgLen: Integer;
                                              var Success: Boolean): Boolean;
begin
  Result := False;
end;

function TExternalIFPrivateKey.IFPrivateKeyOperation(I: PMPInteger;
  var J: PMPInteger): Boolean;
begin
  Result := False;
end;

function TExternalIFPrivateKey.SignDigest(const Digest;
  DigestCount: Integer; HashAlgorithm, MGFHashAlgorithm: THashAlgorithm;
  Encoding: TSignEncoding; NullMsg: Boolean; var Sign;
  var SignCount: Integer): Boolean;
begin
  Result := False;
end;

function TExternalIFPrivateKey.SignMsg(const Msg;
  MsgCount: Integer; HashAlgorithm, MGFHashAlgorithm: THashAlgorithm;
  Encoding: TSignEncoding; var Sign; var SignCount: Integer): Boolean;
begin
  Result := False;
end;

function TExternalIFPrivateKey.ImportKey(const PKIdentifier;
  PKILen: Integer): Boolean;
begin
  Result := False;
end;

function TExternalIFPrivateKey.Validate(
  const APubl: TIFPublicKey): Boolean;
begin
  Result := False;
end;

function TExternalIFPrivateKey.GetIdentifier: string;
begin
  Result := '';
end;

{ TMPIFPrivateKey }

constructor TMPIFPrivateKey.CreateNew(const AAlgorithm: ObjectIdentifier;
  ABitSize: Integer; AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
  ASignatureDigestAlg: THashAlgorithm; APublKey: IIFPublicKeyInfo;
  Exportable: Boolean);
var
  IFPubl: TIFPublicKey;
begin
  FCreating := True;
  FillChar(IFPubl,SizeOf(IFPubl),0);
  try
    if AAlgorithm = id_rw then
      IFRWKeys(ABitSize,1,False,FKey,IFPubl)
    else
      IFRSAKeys(ABitSize,1,False,FKey,IFPubl,Exportable);
    Create(AKeyStruct,AKeyHandler);
    if Assigned(APublKey) then begin
      if AAlgorithm = rsaEncryption then
        APublKey.ImposeSubjectRSAPublicKey(IFPubl)
      else
        APublKey.ImposeSubjectRSAES_OAEPPublicKey(IFPubl,ASignatureDigestAlg,ASignatureDigestAlg);
    end;
    PublKey := APublKey;
    FModulusBitSize := MPMSB(IFPubl.N);
  finally
    DisposeIFPublicKey(IFPubl);
  end;
  SignatureDigestAlg := ASignatureDigestAlg;
  if Assigned(AKeyHandler) then
    if not EncodeNewKey then
      raise Exception.Create('TMPIFPrivateKey.CreateNew: Could not encode key');
  FCreating := False;
end;

function TMPIFPrivateKey.DecodeASNKey(K: TASN1Struct): Boolean;
var
  S: string;
  T: Cardinal;
  N: PMPInteger;
begin
  Result := True;
  T := K.Tag;
  if K.Cls <> V_ASN1_CONTEXT_SPECIFIC then
    Result := False
  else if T = 0 then begin
    FKey.ReprType := 0;
    K.Items[0]^.ContentAsUMPInt(FKey.zN);
    K.Items[1]^.ContentAsUMPInt(FKey.zD);
    FModulusBitSize := MPMSB(FKey.zN);
  end else if T = 1 then begin
    FKey.ReprType := 1;
    K.Items[0]^.ContentAsUMPInt(FKey.oP);
    K.Items[1]^.ContentAsUMPInt(FKey.oQ);
    K.Items[2]^.ContentAsUMPInt(FKey.oD1);
    K.Items[3]^.ContentAsUMPInt(FKey.oD2);
    K.Items[4]^.ContentAsUMPInt(FKey.oC);
    N := nil;
    try
      MPMul2(FKey.oP,FKey.oQ,N);
      FModulusBitSize := MPMSB(N);
    finally
      MPDealloc(N);
    end;
  end else if T = 2 then begin
    FKey.ReprType := 2;
    K.Items[0]^.ContentAsUMPInt(FKey.tP);
    K.Items[1]^.ContentAsUMPInt(FKey.tQ);
    K.Items[2]^.ContentAsUMPInt(FKey.tD);
    N := nil;
    try
      MPMul2(FKey.tP,FKey.tQ,N);
      FModulusBitSize := MPMSB(N);
    finally
      MPDealloc(N);
    end;
  end else if T = 3 then begin
    FKey.ReprType := 3;
    Result := Assigned(FExternalIFPrivateKeyClass);
    if Result then begin
      S := Identifier;
      FKey.ExternalKey := FExternalIFPrivateKeyClass.Create;
      Result := FKey.ExternalKey.ImportKey(Pointer(S)^,Length(S));
      FModulusBitSize := FKey.ExternalKey.GetKeySize;
    end;
  end else
    Result := False;
  if Result then begin
    if Algorithm = id_rw then
      FKey.Scheme := ifRW
    else
      FKey.Scheme := ifRSA1;
  end;
end;

procedure TMPIFPrivateKey.DisposeKey;
begin
  try
    inherited DisposeKey;
    DisposeIFPrivateKey(FKey);
  except
    on E: Exception do
      raise Exception.Create('TMPIFPrivateKey.DisposeKey'#13#10 +
                             E.Message); 
  end;
end;

function TMPIFPrivateKey.EncodeASNKey(K: TASN1Struct): Boolean;
var
  S: string;
  A: PASN1Struct;
begin
  Result := True;
  if K.ItemCount > 0 then begin
    K.SelectChoice(FKey.ReprType);
    if FKey.ReprType = 0 then begin
      K.EditField('n',FKey.zN,True);
      K.EditField('d',FKey.zD,True);
    end else if FKey.ReprType = 1 then begin
      K.EditField('p',FKey.oP,True);
      K.EditField('q',FKey.oQ,True);
      K.EditField('d1',FKey.oD1,True);
      K.EditField('d2',FKey.oD2,True);
      K.EditField('c',FKey.oC,True);
    end else if FKey.ReprType = 2 then begin
      K.EditField('p',FKey.tP,True);
      K.EditField('q',FKey.tQ,True);
      K.EditField('d',FKey.tD,True);
    end else if (FKey.ReprType = 3) and Assigned(FKey.ExternalKey) then begin
      A := KeyStruct.FindField('identifier');
      if A = nil then
        A := KeyStruct.FindField('/privateKey/identifier');
      if Assigned(A) then begin
        S := FKey.ExternalKey.Identifier;
        A.SetContent(Pointer(S)^,Length(S));
      end;
      K.SetContent(Pointer(S)^,Length(S));
    end else
      Result := False;
  end else begin                     
    K.Cls := V_ASN1_CONTEXT_SPECIFIC;
    K.Tag := FKey.ReprType;
    K.Implicit := True;
    K.ImplicitTag := V_ASN1_SEQUENCE;
    K.Constructed := True;
    if FKey.ReprType = 0 then begin
      A := K.AddField('n','INTEGER',nil);
      A.EditContent(FKey.zN,True);
      A := K.AddField('d','INTEGER',nil);
      A.EditContent(FKey.zD,True);
    end else if FKey.ReprType = 1 then begin
      A := K.AddField('p','INTEGER',nil);
      A.EditContent(FKey.oP,True);
      A := K.AddField('q','INTEGER',nil);
      A.EditContent(FKey.oQ,True);
      A := K.AddField('d1','INTEGER',nil);
      A.EditContent(FKey.oD1,True);
      A := K.AddField('d2','INTEGER',nil);
      A.EditContent(FKey.oD2,True);
      A := K.AddField('c','INTEGER',nil);
      A.EditContent(FKey.oC,True);
    end else if FKey.ReprType = 2 then begin
      A := K.AddField('p','INTEGER',nil);
      A.EditContent(FKey.tP,True);
      A := K.AddField('q','INTEGER',nil);
      A.EditContent(FKey.tQ,True);
      A := K.AddField('d','INTEGER',nil);
      A.EditContent(FKey.tD,True);
    end else if (FKey.ReprType = 3) and Assigned(FKey.ExternalKey) then begin
      K.Tag := V_ASN1_OCTET_STRING;
      K.Constructed := False;
      A := KeyStruct.FindField('identifier');
      if A = nil then
        A := KeyStruct.FindField('/privateKey/identifier');
      if Assigned(A) then begin
        S := FKey.ExternalKey.Identifier;
        A.SetContent(Pointer(S)^,Length(S));
      end;
      K.SetContent(Pointer(S)^,Length(S));
    end else
      Result := False;
  end;
end;

function TMPIFPrivateKey.EncryptEncoding: TEncryptEncoding;
var
  OID: string;
begin
  OID := Algorithm;
  if OID = id_rw then
    Result := eeNotSupported
  else if OID = rsaEncryption then
    Result := eeEME_PKCS1_v1_5
  else
    Result := eeEME1;
end;

function TMPIFPrivateKey.ExportToPRIVATEKEYBLOB(out Priv: Pointer;
  out Len: Integer; Alg: LongWord; OverrideLock: Boolean): Boolean;
var
  K: PRSAKeyBlob;
begin
  LockKey;
  try
    if not OverrideLock then
      UnlockKey;
    K := nil;
    if KeyInitialized then
      Result := RSAPrivateKeyToPRIVATEKEYBLOB(FKey,Alg,K,Len)
    else
      Result := False;
    Priv := K;
  finally
    if OverrideLock then
      UnlockKey;
  end;
end;

function TMPIFPrivateKey.ExportToRSAPrivateKey(
  var RSAPrivateKey: TASN1Struct; OverrideLock: Boolean): Boolean;
begin
  LockKey;
  try
    if not OverrideLock then
      UnlockKey;
    if KeyInitialized then
      Result := RSAPrivateKeyToASN1Struct(FKey,RSAPrivateKey)
    else
      Result := False;
  finally
    if OverrideLock then
      UnlockKey;
  end;
end;

function TMPIFPrivateKey.GetAlgorithm: ObjectIdentifier;
begin
  if KeyInitialized and (FKey.Scheme = ifRW) then
    Result := id_rw
  else
    Result := inherited GetAlgorithm;
end;

function TMPIFPrivateKey.GetEncryptHashAlg: THashAlgorithm;
begin
  Result := FEncryptHashAlg;
end;

function TMPIFPrivateKey.GetExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
begin
  Result := FExternalIFPrivateKeyClass;
end;

function TMPIFPrivateKey.GetMGFHashAlg: THashAlgorithm;
begin
  Result := FMGFHashAlg;
end;

function TMPIFPrivateKey.GetP: OctetString;
begin
  Result := FP;
end;

function TMPIFPrivateKey.GetSignatureLength: Integer;
begin
  Result := (GetSignatureSize + 7) shr 3;
end;

function TMPIFPrivateKey.GetSignatureSize: Integer;
var
  IFPublIntf: IIFPublicKeyInfo;
  IFPubl: TIFPublicKey;
  HA, MGFHA: THashAlgorithm;
  P: string;
  Done: Boolean;
begin
  Result := FModulusBitSize;
  if Result = 0 then begin
    Done := False;
    if Assigned(PublKey) then begin
      Done := PublKey.QueryInterface(IIFPublicKeyInfo,IFPublIntf) = 0;
      if Done then begin
        FillChar(IFPubl,SizeOf(IFPubl),0);
        try
          if Algorithm = rsaEncryption then
            Done := IFPublIntf.ExtractSubjectRSAPublicKey(IFPubl)
          else
            Done := IFPublIntf.ExtractSubjectRSAES_OAEPPublicKey(IFPubl,HA,MGFHA,P);
          if Done then
            Result := MPMSB(IFPubl.N);
        finally
          DisposeIFPublicKey(IFPubl);
        end;
      end;
    end;
    if not Done then begin
      LockKey;
      UnlockKey;
      if FModulusBitSize > 0 then
        Result := FModulusBitSize
      else
        Result := inherited GetSignatureLength
    end else
      FModulusBitSize := Result
  end;
end;

procedure TMPIFPrivateKey.InternalDecryptKeyTransport(const EMsg;
  MsgLen: Integer; var Key; KeyLen: Integer; out Result: Boolean);
var
  EE: TEncryptEncoding;
  KLen: Integer;
begin
  EE := EncryptEncoding;
  Result := EE <> eeNotSupported;
  if Result then begin
    KLen := KeyLen;
    Result := IFESDecryption(FKey,EMsg,MsgLen,EncryptHashAlg,EE,Key,KLen);
    Result := Result and (KeyLen = KLen);
  end;
end;

procedure TMPIFPrivateKey.InternalSignBuf(const Msg; MsgLen: Integer;
  var Sign; var SignLen: Integer);
begin
  SignLen := IFSSASignatureGeneration(FKey,
                                      Msg,MsgLen,
                                      SignatureDigestAlg,
                                      MGFHashAlg,
                                      SignEncoding,
                                      Sign,SignLen);
end;

procedure TMPIFPrivateKey.InternalSignDig(const Digest; DigestLen: Integer;
  var Sign; var SignLen: Integer);
begin
  SignLen := IFSSASignatureGeneration(FKey,
                                      Digest,DigestLen,
                                      SignatureDigestAlg,
                                      MGFHashAlg,
                                      SignEncoding,
                                      False,
                                      Sign,SignLen);
end;

procedure TMPIFPrivateKey.InternalSignSigned(Signed: IUnknown;
  CACert: TASN1Struct; var OK: Boolean);
var
  S: ISigned;
begin
  OK := Signed.QueryInterface(ISigned,S) = 0;
  if OK then begin
    if Algorithm = rsaEncryption then begin
      if Assigned(CACert) then
        OK := S.RSASign(FKey,SignatureDigestAlg,CACert)
      else
        OK := S.RSASign(FKey,SignatureDigestAlg)
    end else if (Algorithm = id_rsaes_oaep) or (Algorithm = id_rsassa_pss) then begin
      if Assigned(CACert) then
        OK := S.RSASSA_PSSSign(FKey,CACert)
      else
        OK := S.RSASSA_PSSSign(FKey)
    end else
      OK := False;
  end;
end;

function TMPIFPrivateKey.RSAPublicKey(var APublKey: TIFPublicKey): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      IFRSAPublicKey(FKey,APublKey);
  finally
    UnlockKey;
  end;
end;

procedure TMPIFPrivateKey.SetEncryptHashAlg(const Value: THashAlgorithm);
begin
  FEncryptHashAlg := Value;
end;

procedure TMPIFPrivateKey.SetExternalIFPrivateKeyClass(
  const Value: TExternalIFPrivateKeyClass);
begin
  FExternalIFPrivateKeyClass := Value;
end;

procedure TMPIFPrivateKey.SetMGFHashAlg(const Value: THashAlgorithm);
begin
  FMGFHashAlg := Value;
end;

procedure TMPIFPrivateKey.SetP(const Value: OctetString);
begin
  FP := Value;
end;

procedure TMPIFPrivateKey.SetPublKey(const Value: IPKPublicKeyInfo);
var
  Publ: IIFPublicKeyInfo;
  IFPubl: TIFPublicKey;
  Res: Boolean;
begin
  if Assigned(Value) and
     (Value.QueryInterface(IIFPublicKeyInfo,Publ) = 0) then begin
    FillChar(IFPubl,SizeOf(IFPubl),0);
    try
      if Publ.AlgorithmIdentifier = '' then begin
        LockKey;
        try
          if KeyInitialized then begin
            if (FKey.Scheme in [ifRSA1,ifRSA2]) and
               (FKey.ReprType in [1,2]) then begin
              IFRSAPublicKey(FKey,IFPubl);
              if GetAlgorithm = rsaEncryption then
                Publ.ImposeSubjectRSAPublicKey(IFPubl)
              else
                Publ.ImposeSubjectRSAES_OAEPPublicKey(IFPubl,
                                                      FEncryptHashAlg,
                                                      FMGFHashAlg,
                                                      FP);
            end else
              Publ := nil;
          end else
            Publ := nil;
        finally
          UnlockKey;
        end;
      end else begin
        if Publ.AlgorithmIdentifier = rsaEncryption then
          Res := Publ.ExtractSubjectRSAPublicKey(IFPubl)
        else begin
          Res := Publ.ExtractSubjectRSAES_OAEPPublicKey(IFPubl,
                                                        FEncryptHashAlg,
                                                        FMGFHashAlg,
                                                        FP);
          SignatureDigestAlg := FEncryptHashAlg;
        end;
        if Res and not FCreating then begin
          LockKey;
          try
            if not (KeyInitialized and ValidateIFPrivateKey(FKey,IFPubl)) then
              Publ := nil;
          finally
            UnlockKey;
          end;
        end else if not Res then
          Publ := nil;
      end;
    finally
      DisposeIFPublicKey(IFPubl);
    end;
    inherited SetPublKey(Publ);
  end else
    inherited SetPublKey(nil);
end;

function TMPIFPrivateKey.SignEncoding: TSignEncoding;
begin
  if Algorithm = rsaEncryption then
    Result := seEMSA3
  else
    Result := seEMSA4;
end;

function TMPIFPrivateKey.ValidatePublicKey(
  const Publ: TIFPublicKey): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      Result := ValidateIFPrivateKey(FKey,Publ);
  finally
    UnlockKey;
  end;
end;

{ TMPIFPublicKey }

constructor TMPIFPublicKey.CreateFromRecord(
  const AAlgorithm: ObjectIdentifier; const IFPublKey: TIFPublicKey;
  AKeyIntf: IIFPublicKeyInfo);
begin
  Create(AKeyIntf);
  MPCopy2(IFPublKey.N,FKey.N);
  MPCopy2(IFPublKey.E,FKey.E);
  FAlgorithm := AAlgorithm;
  EncodeKey;
end;

constructor TMPIFPublicKey.CreateRSAES_OAEPFromRecord(
  const IFPublKey: TIFPublicKey; HA, MGFHA: THashAlgorithm;
  const P: OctetString; AKeyIntf: IIFPublicKeyInfo);
begin
  SignatureDigestAlg := HA;
  MGFHashAlg := MGFHA;
  FP := P;
  CreateFromRecord(id_RSAES_OAEP,IFPublKey,AKeyIntf);
end;

constructor TMPIFPublicKey.CreateRWFromRecord(
  const IFPublKey: TIFPublicKey; HA, MGFHA: THashAlgorithm;
  AKeyIntf: IIFPublicKeyInfo);
begin
  SignatureDigestAlg := HA;
  MGFHashAlg := MGFHA;
  CreateFromRecord(id_rw,IFPublKey,AKeyIntf);
end;

function TMPIFPublicKey.DecodeKey: Boolean;
var
  PublInfo: IIFPublicKeyInfo;
  HA: THashAlgorithm;
begin
  Result := KeyIntf.QueryInterface(IIFPublicKeyInfo,PublInfo) = 0;
  if Result then begin
    if PublInfo.AlgorithmIdentifier = rsaEncryption then
      Result := PublInfo.ExtractSubjectRSAPublicKey(FKey)
    else begin
      Result := PublInfo.ExtractSubjectRSAES_OAEPPublicKey(FKey,HA,FMGFHashAlg,FP);
      SignatureDigestAlg := HA;
    end;
  end;
end;

procedure TMPIFPublicKey.DisposeKey;
begin
  inherited;
  DisposeIFPublicKey(FKey);
end;

procedure TMPIFPublicKey.EncodeKey;
var
  PublInfo: IIFPublicKeyInfo;
begin
  Assert(KeyIntf.QueryInterface(IIFPublicKeyInfo,PublInfo) = 0);
  if FAlgorithm = rsaEncryption then
    PublInfo.ImposeSubjectRSAPublicKey(FKey)
  else
    PublInfo.ImposeSubjectRSAES_OAEPPublicKey(FKey,SignatureDigestAlg,MGFHashAlg,P);
end;

function TMPIFPublicKey.GetSignatureLength: Integer;
begin
  if FKey.N = nil then
    DecodeKey;
  Result := (MPMSB(FKey.N) + 7) shr 3;
end;

procedure TMPIFPublicKey.InternalEncryptKeyTransport(const Key;
  KeyLen: Integer; var EMsg; MsgLen: Integer; out Result: Boolean);
begin
  if Algorithm = rsaEncryption then
    IFESEncryption(FKey,Key,KeyLen,SignatureDigestAlg,eeEME_PKCS1_v1_5,EMsg,MsgLen)
  else
    IFESEncryption(FKey,
                   Key,KeyLen,
                   Pointer(FP)^,Length(FP),
                   SignatureDigestAlg,MGFHashAlg,eeEME1,
                   EMsg,MsgLen);
  Result := MsgLen > 0;
end;

function TMPIFPublicKey.InternalVerBuf(const Msg; MsgLen: Integer;
  const Sign; SignLen: Integer): Boolean;
begin
  Result := IFSSASignatureVerification(FKey,
                                       Msg,MsgLen,
                                       Sign,SignLen,
                                       SignatureDigestAlg,
                                       MGFHashAlg,
                                       SignEncoding);
end;

function TMPIFPublicKey.InternalVerDig(const Digest; DigestLen: Integer;
  const Sign; SignLen: Integer): Boolean;
begin
  Result := IFSSASignatureVerification(FKey,
                                       Digest,DigestLen,
                                       Sign,SignLen,
                                       SignatureDigestAlg,
                                       MGFHashAlg,
                                       SignEncoding,
                                       False);
end;

procedure TMPIFPublicKey.SetMGFHashAlg(const Value: THashAlgorithm);
begin
  FMGFHashAlg := Value;
end;

procedure TMPIFPublicKey.SetP(const Value: OctetString);
begin
  FP := Value;
end;

function TMPIFPublicKey.SignEncoding: TSignEncoding;
begin
  if Algorithm = rsaEncryption then
    Result := seEMSA3
  else
    Result := seEMSA4;
end;

{ TMPIFPrivateKeyTLS }

function TMPIFPrivateKeyTLS.SignEncoding: TSignEncoding;
begin
{$IFDEF SHA1_AND_MD5}
  Result := seEMSA_TLS;
{$ELSE  SHA1_AND_MD5}
  Result := seReserved3;
{$ENDIF SHA1_AND_MD5}
end;

end.
