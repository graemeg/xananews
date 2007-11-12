{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPDL Unit                                         }
{     Implementation of Discrete Logarithm schemes      }
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
{$B-,O+,Q-,R-}
{$I ver.inc}
unit MpDL;

interface

uses
  MpArithTypes, SecUtils, Asn1, MpPK;

type
  TKeyDerivation = (kdKDF1,kdKDF2,kdX9_42ASN1DER,kdX9_42Concatenation,kdNone);

  PDLSystemParams = ^TDLSystemParams;
  TDLSystemParams = record
    P: PMPInteger;
    Q: PMPInteger;
    G: PMPInteger;
    J: PMPInteger;
  end;

  TDLKeyAgreementScheme = (dlkasDH1,dlkasDH2,dlkasMQV);
  TDLSignatureScheme = (dlssaDSA,dlssaNR,dlSSADSA_P1363);

  TDLPrivateKey = record
    KAScheme: TDLKeyAgreementScheme;
    SignScheme: TDLSignatureScheme;
    Params: TDLSystemParams;
    X: PMPInteger;
  end;

  TDLPublicKey = record   
    KAScheme: TDLKeyAgreementScheme;
    SignScheme: TDLSignatureScheme;
    Params: TDLSystemParams;
    Y: PMPInteger;
  end;

  TDataBuf = record
    Data: Pointer;
    Size: Integer;
  end;

  PX9_42MacTagParams = ^TX9_42MacTagParams;
  TX9_42MacTagParams = record
    AlgorithmID: TDataBuf;
    Counter: LongWord;
    IdA: TDataBuf;
    IdB: TDataBuf;
    YA, YB, TA, TB: PMPInteger;
    MacTag: TDataBuf;
  end;

  IDLPublicKeyInfo = interface(IPKPublicKeyInfo)
    ['{E07CAB32-2E83-4FD5-A0E8-C2BC7AAD3BD4}']
    function ExtractSubjectDHPublicKey(var DHKey: TDLPublicKey): Boolean;
    function ExtractSubjectDSAPublicKey(var DSAKey: TDLPublicKey): Boolean;
    function ExtractSubjectNRPublicKey(var NRKey: TDLPublicKey): Boolean;
    procedure ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
    procedure ImposeSubjectDSAPublicKey(const DSAKey: TDLPublicKey);
    procedure ImposeSubjectNRPublicKey(const NRKey: TDLPublicKey);
  end;

  IMPKAPrivateKey = interface(IMPPrivateKey)
    ['{47FB3CF8-A873-42AD-9A45-A6584B018DA8}']
    function GetCoFactor: Boolean;
    function GetDEREncodeSignature: Boolean;
    function GetKAHashAlg: THashAlgorithm;
    function GetKDF: TKeyDerivation;
    function GetP: OctetString;
    procedure SetCoFactor(const Value: Boolean);
    procedure SetDEREncodeSignature(const Value: Boolean);
    procedure SetKAHashAlg(const Value: THashAlgorithm);
    procedure SetKDF(const Value: TKeyDerivation);
    procedure SetP(const Value: OctetString);
    property CoFactor: Boolean read GetCoFactor write SetCoFactor;
    property DEREncodeSignature: Boolean read GetDEREncodeSignature write SetDEREncodeSignature;
    property KAHashAlg: THashAlgorithm read GetKAHashAlg write SetKAHashAlg;
    property KDF: TKeyDerivation read GetKDF write SetKDF;
    property P: OctetString read GetP write SetP;
  end;

  TMPKAPrivateKey = class(TMPPrivateKey,IMPKAPrivateKey)
  private
    FDEREncodeSignature: Boolean;
    FP: OctetString;
    FKAHashAlg: THashAlgorithm;
    FKDF: TKeyDerivation;
    FCoFactor: Boolean;
  protected
    function GetCoFactor: Boolean;
    function GetDEREncodeSignature: Boolean;
    function GetKAHashAlg: THashAlgorithm;
    function GetKDF: TKeyDerivation;
    function GetP: OctetString;
    procedure SetCoFactor(const Value: Boolean);
    procedure SetDEREncodeSignature(const Value: Boolean);
    procedure SetKAHashAlg(const Value: THashAlgorithm);
    procedure SetKDF(const Value: TKeyDerivation);
    procedure SetP(const Value: OctetString);
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); override;
    property CoFactor: Boolean read GetCoFactor write SetCoFactor;
    property DEREncodeSignature: Boolean read GetDEREncodeSignature write SetDEREncodeSignature;
    property KAHashAlg: THashAlgorithm read GetKAHashAlg write SetKAHashAlg;
    property KDF: TKeyDerivation read GetKDF write SetKDF;
    property P: OctetString read GetP write SetP;
  end;

  IMPDLPrivateKey = interface(IMPKAPrivateKey)
    ['{9E657209-13A6-41B6-A6DF-128BF57A4563}']
    function DecryptKeyAgreement(const PublKey: TDLPublicKey;
                                 var Secret; SLen: Integer): Boolean; overload;
    function DLPublicKey(var APublKey: TDLPublicKey): Boolean;
    function GetSystemParams: PDLSystemParams;
    property SystemParams: PDLSystemParams read GetSystemParams;
  end;

  TMPDLPrivateKey = class(TMPKAPrivateKey,IMPDLPrivateKey)
  private
    FKey: TDLPrivateKey;
    FSystemParams: TDLSystemParams;
    FCreating: Boolean;
  protected
    function DecodeASNKey(K: TASN1Struct): Boolean; override;
    procedure DisposeKey; override;
    function EncodeASNKey(K: TASN1Struct): Boolean; override;
    function GetAlgorithm: ObjectIdentifier; override;
    function GetSignatureLength: Integer; override;
    function GetSystemParams: PDLSystemParams;
    procedure InternalDecryptKeyAgreement(PublKey: TMPPublicKey;
                                          var Secret; SLen: Integer;
                                          out Result: Boolean); override;
    procedure InternalDecryptKeyAgreementRec(const PublKey: TDLPublicKey;
                                             var Secret; SLen: Integer;
                                             out Result: Boolean);
    procedure InternalDecryptKeyAgreementIntf(PublKey: IPKPublicKeyInfo;
                                              var Secret; SLen: Integer;
                                              out Result: Boolean); override;
    procedure InternalSignBuf(const Msg; MsgLen: Integer;
                              var Sign; var SignLen: Integer); override;
    procedure InternalSignDig(const Digest; DigestLen: Integer;
                              var Sign; var SignLen: Integer); override;
    procedure InternalSignSigned(Signed: IUnknown;
                                 CACert: TASN1Struct;
                                 var OK: Boolean); override;
    procedure SetPublKey(const Value: IPKPublicKeyInfo); override;
    property Creating: Boolean read FCreating write FCreating;
  public
    constructor CreateNew(const AAlgorithm: ObjectIdentifier;
                          APBitSize, AQBitSize: Integer;
                          AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
                          ASignatureDigestAlg: THashAlgorithm;
                          APublKey: IDLPublicKeyInfo);
    constructor CreateNewParams(const AAlgorithm: ObjectIdentifier;
                                AParams: TDLSystemParams;
                                AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
                                APublKey: IDLPublicKeyInfo);
    destructor Destroy; override;
    function DecryptKeyAgreement(const PublKey: TDLPublicKey;
                                 var Secret; SLen: Integer): Boolean; overload;
    function DLPublicKey(var APublKey: TDLPublicKey): Boolean;
    property SystemParams: PDLSystemParams read GetSystemParams;
  end;

  TMPDLPrivateKeyTLS = class(TMPDLPrivateKey)
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); override;
  end;

  TMPDLPublicKey = class(TMPPublicKey)
  private
    FKey: TDLPublicKey;
    FDEREncodeSignature: Boolean;
    FAlgorithm: ObjectIdentifier;
    function GetSystemParams: PDLSystemParams;
    procedure SetDEREncodeSignature(const Value: Boolean);
  protected
    function DecodeKey: Boolean; override;
    procedure DisposeKey; override;
    procedure EncodeKey; override;
    function InternalVerBuf(const Msg; MsgLen: Integer;
                            const Sign; SignLen: Integer): Boolean; override;
    function InternalVerDig(const Digest; DigestLen: Integer;
                            const Sign; SignLen: Integer): Boolean; override;
  public
    constructor CreateFromRecord(const AAlgorithm: ObjectIdentifier;
                                 const DLPublKey: TDLPublicKey;
                                 AKeyIntf: IDLPublicKeyInfo);
    property DEREncodeSignature: Boolean read FDEREncodeSignature write SetDEREncodeSignature;
    property SystemParams: PDLSystemParams read GetSystemParams;
  end;

procedure DisposeDLPrivateKey(var AKey: TDLPrivateKey);
procedure DisposeDLPublicKey(var AKey: TDLPublicKey);
procedure DisposeDLSystemParams(var APrm: TDLSystemParams);

procedure DLKeys(ABitSize: Cardinal;
                 AQBitSize: Cardinal;
                 var APriv: TDLPrivateKey;
                 var APubl: TDLPublicKey); overload;
procedure DLKeys(const Params: TDLSystemParams;
                 var APriv: PMPInteger;
                 var APubl: PMPInteger); overload;
procedure DLKeys(const Params: TDLSystemParams;
                 var APriv: TDLPrivateKey;
                 var APubl: PMPInteger); overload;
procedure DLPublicKey(const APriv: TDLPrivateKey;
                      var APubl: TDLPublicKey);


procedure CopyDLSystemParams(var Dest: TDLSystemParams;
                             const Src: TDLSystemParams);

function ValidateDLPublicKey(APubl: TDLPublicKey): Boolean;
function ValidateDLPrivateKey(APriv: TDLPrivateKey; APubl: TDLPublicKey): Boolean;


// Key Agreement Schemes.
function DLKAS_DH1(const APriv: TDLPrivateKey;
                   WPr: PMPInteger;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams = nil): Boolean;

function DLKAS_DH2(const APriv0: TDLPrivateKey;
                   const APriv1: TDLPrivateKey;
                   WPr: PMPInteger;
                   VPr: PMPInteger;
                   const P; pLen: Integer;
                   CoFactor0, CoFactor1: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams = nil): Boolean;

function DLKAS_MQV(const APriv0: TDLPrivateKey;
                   APriv1: PMPInteger;
                   APubl1: PMPInteger;
                   WPr: PMPInteger;
                   VPr: PMPInteger;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams = nil): Boolean;

// Signature Schemes:
//  DEREncode = True:  Sign will be a DER encoded Dss-Sig-Value structure.
//  DEREncode = False: Sign will be the concatenation of r and s.
function DLSSASignatureGeneration(const AKey: TDLPrivateKey;
                                  const Digest; DigestCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  NullMsg: Boolean;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer; overload;

function DLSSASignatureGeneration(const AKey: TDLPrivateKey;
                                  const Msg; MsgCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer; overload;

function DLSSASignatureVerification(const AKey: TDLPublicKey;
                                    const Digest; DigestCount: Integer;
                                    const Sign; SignCount: Integer;
                                    DEREncode: Boolean;
                                    HashAlgorithm: THashAlgorithm;
                                    NullMsg: Boolean): Boolean; overload;

function DLSSASignatureVerification(const AKey: TDLPublicKey;
                                    const Msg; MsgCount: Integer;
                                    const Sign; SignCount: Integer;
                                    DEREncode: Boolean;
                                    HashAlgorithm: THashAlgorithm): Boolean; overload;

// Integrated Encryption Schemes:
procedure DLIESEncryption(const AKey: TDLPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          var EphemeralKey: PMPInteger;
                          var CipherText; CTCount: Integer;
                          var MAC; MACCount: Integer); overload;

procedure DLIESEncryption(const AKey: TDLPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          CipherClass: TCipherClass;
                          KeyLen: Integer;
                          var EphemeralKey: PMPInteger;
                          var CipherText; var CTCount: Integer;
                          var MAC; MACCount: Integer); overload;

function DLIESDecryption(const AKey: TDLPrivateKey;
                         const EphemeralKey: PMPInteger;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         var Msg; MsgCount: Integer): Boolean; overload;

function DLIESDecryption(const AKey: TDLPrivateKey;
                         const EphemeralKey: PMPInteger;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         CipherClass: TCipherClass;
                         KeyLen: Integer;
                         var Msg; var MsgCount: Integer): Boolean; overload;   
                          

// Key derivation functions
procedure KDF1(const Z; zLen: Integer;
               const P; pLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var K; kLen: Integer);

procedure KDF2(const Z; zLen: Integer;
               const P; pLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var K; kLen: Integer); overload;
procedure KDF2(const Z; zLen: Integer;
               const P; pLen: Integer;
               HashAlgorithm: THashAlgorithm;
               kLen: Integer;
               var K: ISecretKey); overload;
procedure KDF2(Z: ISecretKey;
               P: ISecretKey;
               HashAlgorithm: THashAlgorithm;
               kLen: Integer;
               var K: ISecretKey); overload;

procedure KDFX9_42ASN1DER(const Z; zLen: Integer;
                          const P; pLen: Integer;
                          HashAlgorithm: THashAlgorithm;
                          var K; kLen: Integer); overload;
procedure KDFX9_42ASN1DER(Z: ISecretKey;
                          P: ISecretKey;
                          HashAlgorithm: THashAlgorithm;
                          kLen: Integer;
                          var K: ISecretKey); overload;
procedure KDFX9_42Concatenation(const Z; zLen: Integer;
                                const P; pLen: Integer;
                                HashAlgorithm: THashAlgorithm;
                                var K; kLen: Integer); overload;
procedure KDFX9_42Concatenation(Z: ISecretKey;
                                P: ISecretKey;
                                HashAlgorithm: THashAlgorithm;
                                kLen: Integer;
                                var K: ISecretKey); overload;
// Supporting functions
procedure EMSA1(const Digest; DigestCount: Integer;
                HashAlgorithm: THashAlgorithm;
                FBitSize: Cardinal;
                var F: PMPInteger);
function EMSA1Verification(F: PMPInteger;
                           const Digest; DigestCount: Integer;
                           HashAlgorithm: THashAlgorithm;
                           FBitSize: Cardinal): Boolean;
{$IFDEF SHA1}
procedure X9_42MacTag(const Z; zLen: Integer;
                      MacTag: PX9_42MacTagParams);
{$ENDIF SHA1}

var
  SubGroupValidation: Boolean = False;

implementation

uses
  MpArith, SysUtils, MpKaratsuba, Pkix, Pkix_Cert;

procedure CopyDLSystemParams(var Dest: TDLSystemParams;
                             const Src: TDLSystemParams);
begin
  MPCopy2(Src.P,Dest.P);
  MPCopy2(Src.Q,Dest.Q);
  MPCopy2(Src.G,Dest.G);
  if Assigned(Src.J) then
    MPCopy2(Src.J,Dest.J)
  else begin
    MPDealloc(Dest.J);
    Dest.J := nil;
  end;
end;

procedure DisposeDLSystemParams(var APrm: TDLSystemParams);
begin
  MPDealloc(APrm.P);
  MPDealloc(APrm.Q);
  MPDealloc(APrm.G);
  MPDealloc(APrm.J);
  APrm.P := nil;    
  APrm.Q := nil;
  APrm.G := nil;
  APrm.J := nil;
end;

procedure DisposeDLPrivateKey(var AKey: TDLPrivateKey);
begin
  DisposeDLSystemParams(AKey.Params);
  MPDealloc(AKey.X);
  AKey.X := nil;
end;

procedure DisposeDLPublicKey(var AKey: TDLPublicKey);
begin
  DisposeDLSystemParams(AKey.Params);
  MPDealloc(AKey.Y);
  AKey.Y := nil;
end;

function CompareDomainParameters(const Prm0: TDLSystemParams;
                                 const Prm1: TDLSystemParams): Boolean;
begin
  Result := (MPCmpOffset(Prm0.P,Prm1.P) = 0) and
            ((MPCmpOffset(Prm0.Q,Prm1.Q) = 0) or
             (MPMSB(Prm0.Q) = 0) or
             (MPMSB(Prm1.Q) = 0)) and
            ((MPCmpOffset(Prm0.J,Prm1.J) = 0) or
             (MPMSB(Prm0.J) = 0) or
             (MPMSB(Prm1.J) = 0)) and
            (MPCmpOffset(Prm0.G,Prm1.G) = 0);
end;

function ValidateDLPublicKey(APubl: TDLPublicKey): Boolean;
var
  JQ, V, P: PMPInteger;
begin
  Result := (MPMSB(APubl.Params.P) > 2) and MPMillerRabin(APubl.Params.P,4);
  if not Result then Exit;

  Result := (MPMSB(APubl.Params.Q) > 2) and MPMillerRabin(APubl.Params.Q,4);
  if not Result then Exit;

  Result := (MPMSB(APubl.Params.G) > 2) and
            (MPCmpOffset(APubl.Params.G,APubl.Params.P) < 0);
  if not Result then Exit;

  V := nil;
  MPExpMod(APubl.Params.G,APubl.Params.Q,APubl.Params.P,V);
  Result := MPMSB(V) = 1;
  MPDealloc(V);
  if not Result then Exit;

  if APubl.Params.J <> nil then begin
    JQ := nil;
    MPMul2(APubl.Params.Q,APubl.Params.J,JQ);
    MPInc(JQ);
    Result := MPCmpOffset(JQ,APubl.Params.P) = 0;
    MPDealloc(JQ);
    if not Result then Exit;
    P := MPCopy(APubl.Params.J);
    V := nil;
    MPDiv(P,APubl.Params.Q,V);
    Result := MPMSB(P) > 0;
    MPDealloc(V);
    MPDealloc(P);
  end else begin
    JQ := nil;
    MPMul2(APubl.Params.Q,APubl.Params.Q,JQ);
    P := MPCopy(APubl.Params.P);
    V := nil;
    MPDiv(P,JQ,V);
    Result := MPMSB(P) > 1;
    MPDealloc(V);
    MPDealloc(P);
    MPDealloc(JQ);
  end;
  if not Result then Exit;

  Result := MPMSB(APubl.Y) > 1;
  if not Result then Exit;
  V := nil;
  MPKaratsubaExpMod(APubl.Y,APubl.Params.Q,APubl.Params.P,V);
  Result := MPMSB(V) = 1;
  MPDealloc(V);
end;

function ValidateDLPrivateKey(APriv: TDLPrivateKey; APubl: TDLPublicKey): Boolean;
var
  Y: PMPInteger;
begin
  Result := CompareDomainParameters(APriv.Params,APubl.Params) and
            ValidateDLPublicKey(APubl);
  if Result then begin
    Y := nil;
    MPKaratsubaExpMod(APriv.Params.G,APriv.X,APriv.Params.P,Y);
    Result := MPCmpOffset(Y,APubl.Y) = 0;
    MPDealloc(Y);
  end;
end;

procedure DLKeys(ABitSize: Cardinal;
                 AQBitSize: Cardinal;
                 var APriv: TDLPrivateKey;
                 var APubl: TDLPublicKey);
var
  SGParams: TSubGroupParams;
  T: PMPInteger;
begin
  FillChar(SGParams,SizeOf(SGParams),0);
    SGParams.l := AQBitSize;
  repeat
    MPDealloc(SGParams.c);
    MPDealloc(SGParams.q);
    MPPrime(APriv.Params.P,ABitSize,paSubGroup,@SGParams);
    T := MPCopy(APriv.Params.P);
    MPDiv(T,SGParams.q,APriv.Params.J);
    MPDealloc(T);
    MPPow2Mod(APriv.Params.J,APriv.Params.P,APriv.Params.G);
  until MPMSB(APriv.Params.G) > 1;
  MPCopy2(SGParams.q,APriv.Params.Q);
  MPDealloc(SGParams.c);
  MPDealloc(SGParams.q);
  MPCopy2(APriv.Params.P,APubl.Params.P);
  MPCopy2(APriv.Params.Q,APubl.Params.Q);
  MPCopy2(APriv.Params.G,APubl.Params.G);
  MPCopy2(APriv.Params.J,APubl.Params.J);
  DLKeys(APriv.Params,APriv.X,APubl.Y);
end;

procedure DLKeys(const Params: TDLSystemParams;
                 var APriv: PMPInteger;
                 var APubl: PMPInteger);
begin
  MPRandomBound(APriv,nil,Params.Q);
  MPKaratsubaExpMod(Params.G,APriv,Params.P,APubl);
end;

procedure DLKeys(const Params: TDLSystemParams;
                 var APriv: TDLPrivateKey;
                 var APubl: PMPInteger); overload;
begin
  DLKeys(Params,APriv.X,APubl);
  CopyDLSystemParams(APriv.Params,Params);
end;

procedure DLPublicKey(const APriv: TDLPrivateKey;
                       var APubl: TDLPublicKey);
begin
  CopyDLSystemParams(APubl.Params,APriv.Params);
  MPKaratsubaExpMod(APriv.Params.G,APriv.X,APriv.Params.P,APubl.Y);
end;

procedure DLSVDP_DH(const APriv: TDLPrivateKey;
                    const APubl: PMPInteger;
                    var Z: PMPInteger);
begin
  MPKaratsubaExpMod(APubl,APriv.X,APriv.Params.P,Z);
end;

function DLSVDP_DHC(const APriv: TDLPrivateKey;
                    const APubl: PMPInteger;
                    var Z: PMPInteger): Boolean;
var
  J, JInv, T: PMPInteger;
  M: Integer;
begin
  if APriv.Params.J = nil then begin
    T := MPCopy(APriv.Params.P);
    J := nil;
    MPDiv(T,APriv.Params.Q,J);
    Result := MPMSB(T) = 1;
    MPDealloc(T);
    if not Result then Exit;
  end else
    J := MPCopy(APriv.Params.J);
  JInv := nil;
  MPBinaryInvMod(J,APriv.Params.Q,JInv);
  T := nil;
  MPMulMod2(APriv.X,JInv,APriv.Params.Q,T);
  MPDealloc(JInv);
  MPMul(T,J);
  MPDealloc(J);
  MPKaratsubaExpMod(APubl,T,APriv.Params.P,Z);
  M := MPMSB(Z);
  Result := M > 1;
  MPDealloc(T);
end;

procedure DLSVDP_MQV(const Params: TDLSystemParams;
                     const S: PMPInteger;
                     const U: PMPInteger;
                     const V: PMPInteger;
                     const WPr: PMPInteger;
                     const VPr: PMPInteger;
                     var Z: PMPInteger);
var
  T, TPr, PowH, E: PMPInteger;
  H: Integer;
begin
  H := (MPMSB(Params.Q) + 1) shr 1;
  PowH := IntToMPInt(1);
  MPShl(PowH,H);

  T := MPCopy(V);
  MPMask(T,H);
  MPAdd(T,PowH);

  TPr := MPCopy(VPr);
  MPMask(TPr,H);
  MPAdd(TPr,PowH);

  MPDealloc(PowH);

  E := MPCopy(S);
  MPMulMod(E,T,Params.Q);
  MPAdd(E,U);
  if MPCmpOffset(E,Params.Q) > 0 then
    MPSub(E,Params.Q);

  MPKaratsubaExpMod(WPr,TPr,Params.P,T);
  MPMulMod(T,VPr,Params.P);
  MPKaratsubaExpMod(T,E,Params.P,Z);

  MPDealloc(T);
  MPDealloc(TPr);
  MPDealloc(E);
end;

function DLSVDP_MQVC(const Params: TDLSystemParams;
                     const S: PMPInteger;
                     const U: PMPInteger;
                     const V: PMPInteger;
                     const WPr: PMPInteger;
                     const VPr: PMPInteger;
                     var Z: PMPInteger): Boolean;
var
  J, JInv, T, TPr, PowH, E: PMPInteger;
  H: Integer;
begin
  if Params.J = nil then begin
    T := MPCopy(Params.P);
    J := nil;
    MPDiv(T,Params.Q,J);
    Result := MPMSB(T) = 1;
    MPDealloc(T);
    if not Result then Exit;
  end else
    J := MPCopy(Params.J);

  H := (MPMSB(Params.Q) + 1) shr 1;
  PowH := IntToMPInt(1);
  MPShl(PowH,H);

  T := MPCopy(V);
  MPMask(T,H);
  MPAdd(T,PowH);

  TPr := MPCopy(VPr);
  MPMask(TPr,H);
  MPAdd(TPr,PowH);

  MPDealloc(PowH);

  E := MPCopy(S);
  MPMulMod(E,T,Params.Q);
  MPAdd(E,U);
  MPMod(E,Params.Q);

  JInv := nil;
  MPBinaryInvMod(J,Params.Q,JInv);
  MPMulMod(E,JInv,Params.Q);
  MPMul(E,J);
  MPDealloc(J);
  MPDealloc(JInv);

  MPKaratsubaExpMod(WPr,TPr,Params.P,T);
  MPMulMod(T,VPr,Params.P);
  MPKaratsubaExpMod(T,E,Params.P,Z);

  Result := MPMSB(Z) > 1;

  MPDealloc(T);
  MPDealloc(TPr);
  MPDealloc(E);
end;

procedure DLSP_NR(const APriv: TDLPrivateKey; F: PMPInteger; var R, S: PMPInteger);
var
  U: PMPInteger;
begin
  U := nil;
  DLKeys(APriv.Params,U,R);
  MPAdd(R,F);
  MPMod(R,APriv.Params.Q);

  MPMulMod2(R,APriv.X,APriv.Params.Q,S);
  S^.Sign := -1;
  MPAdd(S,U);
  if S^.Sign < 0 then
    MPAdd(S,APriv.Params.Q);
  MPDealloc(U);
end;

function DLVP_NR(const APubl: TDLPublicKey; R, S: PMPInteger; var F: PMPInteger): Boolean;
var
  T, T0: PMPInteger;
begin
  Result := (MPMSB(R) > 0) and (MPCmpOffset(R,APubl.Params.Q) < 0) and
                               (MPCmpOffset(S,APubl.Params.Q) < 0) and
            (R^.Sign > 0) and (S^.Sign >= 0);
  if not Result then Exit;
  T := nil;
  MPKaratsubaExpMod(APubl.Params.G,S,APubl.Params.P,T);
  T0 := nil;
  MPKaratsubaExpMod(APubl.Y,R,APubl.Params.P,T0);
  MPMulMod(T,T0,APubl.Params.P);
  MPDealloc(T0);
  MPMod(T,APubl.Params.Q);
  MPCopy2(R,F);
  MPSub(F,T);
  MPDealloc(T);
  if F^.Sign < 0 then
    MPAdd(F,APubl.Params.Q);
end;

procedure DLSP_DSA(const APriv: TDLPrivateKey; F: PMPInteger; var R, S: PMPInteger);
var
  U, UInv: PMPInteger;
begin
  U := nil;
  DLKeys(APriv.Params,U,R);
  MPMod(R,APriv.Params.Q);

  MPMulMod2(R,APriv.X,APriv.Params.Q,S);
  MPAdd(S,F);
  MPMod(S,APriv.Params.Q);
  UInv := nil;
  MPInvMod(U,APriv.Params.Q,UInv);
  MPDealloc(U);
  MPMulMod(S,UInv,APriv.Params.Q);
  MPDealloc(UInv);
end;

function DLVP_DSA(const APubl: TDLPublicKey; R, S, F: PMPInteger): Boolean;
var
  H, H1, H2: PMPInteger;
  T, T0: PMPInteger;
begin
  Result := (MPMSB(R) > 0) and (MPCmpOffset(R,APubl.Params.Q) < 0) and
            (MPMSB(S) > 0) and (MPCmpOffset(S,APubl.Params.Q) < 0) and
            (R^.Sign > 0) and (S^.Sign > 0);
  if not Result then Exit;
  H := nil;
  H1 := nil;
  H2 := nil;
  MPInvMod(S,APubl.Params.Q,H);
  MPMulMod2(H,F,APubl.Params.Q,H1);
  MPMulMod2(H,R,APubl.Params.Q,H2);

  T := nil;
  MPKaratsubaExpMod(APubl.Params.G,H1,APubl.Params.P,T);
  T0 := nil;
  MPKaratsubaExpMod(APubl.Y,H2,APubl.Params.P,T0);
  MPMulMod(T,T0,APubl.Params.P);
  MPMod(T,APubl.Params.Q);
  Result := MPCmpOffset(T,R) = 0;

  MPDealloc(T0);
  MPDealloc(T);
  MPDealloc(H2);
  MPDealloc(H1);
  MPDealloc(H);
end;

procedure KDF1(const Z; zLen: Integer;
               const P; pLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var K; kLen: Integer);
var
  HC: THashClass;
  H: THash;
  D: PChar;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    H := HC.Create(Z,zLen)
  else
    raise Exception.Create('KDF1: Unsupported hash algorithm');
  try
    H.HashData(P,pLen);
    GetMem(D,H.DigestSize);
    try
      H.Done(D);
      Move(D^,K,kLen);
      ProtectClear(D^,H.DigestSize);
    finally
      FreeMem(D);
    end;
  finally
    H.Free;
  end;
end;    

function ByteSwap(Value: Cardinal): Cardinal;
asm
  bswap EAX
end;                                 

procedure KDF2(const Z; zLen: Integer;
               const P; pLen: Integer;
               HashAlgorithm: THashAlgorithm;
               kLen: Integer;
               var K: ISecretKey);
begin
  K := TSecretKey.Create('');
  K.SetLength(kLen);
  KDF2(Z,zLen,P,pLen,HashAlgorithm,K.Key^,K.KeyLen);
end;


procedure KDF2(Z: ISecretKey;
               P: ISecretKey;
               HashAlgorithm: THashAlgorithm;
               kLen: Integer;
               var K: ISecretKey);
begin
  K := TSecretKey.Create('');
  K.SetLength(kLen);
  KDF2(Z.Key^,Z.KeyLen,P.Key^,P.KeyLen,HashAlgorithm,K.Key^,K.KeyLen);
end;

procedure KDF2(const Z; zLen: Integer;
               const P; pLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var K; kLen: Integer);
var
  D, PB: PChar;
  hLen: Integer;
  Count, LW: LongWord;
  HC: THashClass;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    hLen := HC.DigestSize
  else
    raise Exception.Create('KDF2: Unsupported hash algorithm');
  GetMem(PB,pLen + 4);
  try
    Move(P,(PB + 4)^,pLen);
    D := @K;
    Count := 1;
    while kLen > 0 do begin
      LW := ByteSwap(Count);
      Move(LW,PB^,4);
      if hLen <= kLen then
        KDF1(Z,zLen,PB^,pLen + 4,HashAlgorithm,D^,hLen)
      else
        KDF1(Z,zLen,PB^,pLen + 4,HashAlgorithm,D^,kLen);
      Dec(kLen,hLen);
      D := D + hLen;
      Inc(Count);
    end;
  finally
    FreeMem(PB);
  end;
end;

procedure KDFX9_42ASN1DER(const Z; zLen: Integer;
                          const P; pLen: Integer;
                          HashAlgorithm: THashAlgorithm;
                          var K; kLen: Integer);
var
  pZ, pP: PChar;
  I: Integer;
begin
  pP := @P;
  Assert(pLen >= 4);
  Assert((pP[0] = #$30) and (Byte(pP[1]) <= pLen - 2) and
         (pP[2] = #$30) and (Byte(pP[3]) <= pLen - 10) and
         (pP[4] = #6) and (Byte(pP[5]) < pLen - 10) and
         (pP[6 + Byte(pP[5])] = #4) and (Byte(pP[7 + Byte(pP[5])]) = 4));
  I := 12 + Byte(pP[5]);
  GetMem(pZ,zLen + I - 4);
  try
    Move(Z,pZ^,zLen);
    try
      Move(P,pZ[zLen],I-4);
      KDF2(pZ^,zLen + I - 4,pP[I],pLen - I,HashAlgorithm,K,kLen);
    finally
      ProtectClear(pZ^,zLen + I - 4);
    end;
  finally
    FreeMem(pZ);
  end;
end;

procedure KDFX9_42ASN1DER(Z: ISecretKey;
                          P: ISecretKey;
                          HashAlgorithm: THashAlgorithm;
                          kLen: Integer;
                          var K: ISecretKey);
begin
  if K = nil then K := TSecretKey.Create('');
  K.SetLength(kLen);
  KDFX9_42ASN1DER(Z.Key^,Z.KeyLen,
                  P.Key^,P.KeyLen,
                  HashAlgorithm,
                  K.Key^,kLen);
end;

procedure KDFX9_42Concatenation(const Z; zLen: Integer;
                                const P; pLen: Integer;
                                HashAlgorithm: THashAlgorithm;
                                var K; kLen: Integer);
var
  pZ, pP: PChar;
  I: Integer;
begin
  pP := @P;
  Assert(pLen >= 4);
  Assert((pP[0] = #6) and (Byte(pP[1]) < pLen - 6));
  I := 6 + Byte(pP[1]);
  GetMem(pZ,zLen + I - 4);
  try
    Move(Z,pZ^,zLen);
    try
      Move(P,pZ[zLen],I-4);
      KDF2(pZ^,zLen + I - 4,pP[I],pLen - I,HashAlgorithm,K,kLen);
    finally
      ProtectClear(pZ^,zLen + I - 4);
    end;
  finally
    FreeMem(pZ);
  end;
end;

procedure KDFX9_42Concatenation(Z: ISecretKey;
                                P: ISecretKey;
                                HashAlgorithm: THashAlgorithm;
                                kLen: Integer;
                                var K: ISecretKey);
begin
  if K = nil then K := TSecretKey.Create('');
  K.SetLength(kLen);
  KDFX9_42Concatenation(Z.Key^,Z.KeyLen,
                        P.Key^,P.KeyLen,
                        HashAlgorithm,
                        K.Key^,kLen);
end;

{$IFDEF SHA1}
procedure X9_42MacTag(const Z; zLen: Integer;
                      MacTag: PX9_42MacTagParams);
const
  Text1: string = 'This is an implementation validation message from DUT to KMVS to validate DUT’s implementation.';
var
  P, K: PChar;
  SizeYA, SizeYB, SizeTA, SizeTB, Size, Idx, Len: Integer;
  LW: LongWord;
begin
  if Assigned(MacTag.YA) then
    SizeYA := (MPMSB(MacTag.YA) + 7) shr 3
  else
    SizeYA := 1;
  if Assigned(MacTag.YB) then
    SizeYB := (MPMSB(MacTag.YB) + 7) shr 3
  else
    SizeYB := 1;
  if Assigned(MacTag.TA) then
    SizeTA := (MPMSB(MacTag.TA) + 7) shr 3
  else
    SizeTA := 1;
  if Assigned(MacTag.TB) then
    SizeTB := (MPMSB(MacTag.TB) + 7) shr 3
  else
    SizeTB := 1;
  Size := MacTag.AlgorithmID.Size + 4 + MacTag.IdA.Size + MacTag.IdB.Size +
          SizeYA + SizeYB + SizeTA + SizeTB + Length(Text1);
  GetMem(P,Size);
  try
    FillChar(P^,Size,0);
    Len := MacTag.AlgorithmID.Size;
    Move(MacTag.AlgorithmID.Data^,P[0],Len);
    Idx := Len;
    LW := ByteSwap(MacTag.Counter);
    Move(LW,P[Idx],4);
    Idx := Idx + 4;
    Len := MacTag.IdA.Size;
    Move(MacTag.IdA.Data^,P[Idx],Len);
    Idx := Idx + Len;
    Len := MacTag.IdB.Size;
    Move(MacTag.IdB.Data^,P[Idx],Len);
    Idx := Idx + Len;
    if Assigned(MacTag.YA) then
      UMPIntToBase256(MacTag.YA,P[Idx],SizeYA);
    Idx := Idx + SizeYA;
    if Assigned(MacTag.YB) then
      UMPIntToBase256(MacTag.YB,P[Idx],SizeYB);
    Idx := Idx + SizeYB;
    if Assigned(MacTag.TA) then
      UMPIntToBase256(MacTag.TA,P[Idx],SizeTA);
    Idx := Idx + SizeTA;
    if Assigned(MacTag.TB) then
      UMPIntToBase256(MacTag.TB,P[Idx],SizeTB);
    Idx := Idx + SizeTB;
    Move(Text1[1],P[Idx],Length(Text1));

    GetMem(K,64);
    try
      FillChar(K[0],44,0);
      Len := MacTag.AlgorithmID.Size + 4;
      KDFX9_42Concatenation(Z,zLen,P^,Len,haSHA1,K[44],20);
      HMAC(K^,64,P[Len],Size - Len,haSHA1,MacTag.MacTag.Data,MacTag.MacTag.Size);
      ProtectClear(K[0],64);
    finally
      FreeMem(K);
    end;
  finally
    FreeMem(P);
  end;
end;
{$ENDIF SHA1}

function KDF(KeyDerivation: TKeyDerivation;
             const Z; zLen: Integer;
             const P; pLen: Integer;
             HashAlgorithm: THashAlgorithm;
             var K; kLen: Integer;
             MacTag: PX9_42MacTagParams = nil): Boolean;
begin
  Result := True;
  case KeyDerivation of
    kdKDF1:               KDF1(Z,zLen,P,pLen,HashAlgorithm,K,kLen);
    kdKDF2:               KDF2(Z,zLen,P,pLen,HashAlgorithm,K,kLen);
    kdX9_42ASN1DER:       KDFX9_42ASN1DER(Z,zLen,P,pLen,HashAlgorithm,K,kLen);
    kdX9_42Concatenation: KDFX9_42Concatenation(Z,zLen,P,pLen,HashAlgorithm,K,kLen);
    kdNone:               if kLen = zLen then
                            Move(Z,K,zLen)
                          else
                            Result := False;
  end;
{$IFDEF SHA1}
  if Assigned(MacTag) then
    X9_42MacTag(Z,zLen,MacTag);
{$ENDIF SHA1}
end;

function DLKAS_DH1(const APriv: TDLPrivateKey;
                   WPr: PMPInteger;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams): Boolean;
var
  V: PMPInteger;
  Z: PChar;
  zLen: Integer;
begin
  Result := MPMSB(WPr) > 1;
  if not Result then Exit;
  V := nil;
  if SubGroupValidation or not CoFactor then begin
    MPKaratsubaExpMod(WPr,APriv.Params.Q,APriv.Params.P,V);
    Result := MPMSB(V) = 1;
    if not Result then begin
      MPDealloc(V);
      Exit;
    end;
  end;
  if CoFactor then
    Result := DLSVDP_DHC(APriv,WPr,V)
  else begin
    Result := True;
    DLSVDP_DH(APriv,WPr,V);
  end;
  if Result then begin
    zLen := (MPMSB(APriv.Params.P) + 7) shr 3;
    GetMem(Z,zLen);
    try
      UMPIntToBase256(V,Z^,zLen);
      Result := KDF(KeyDerivation,Z^,zLen,P,pLen,HashAlgorithm,K,kLen,MacTag);
    finally
      ProtectClear(Z^,zLen);
      FreeMem(Z);
    end;
  end;
  MPDealloc(V);
end;

function DLKAS_DH2(const APriv0: TDLPrivateKey;
                   const APriv1: TDLPrivateKey;
                   WPr: PMPInteger;
                   VPr: PMPInteger;
                   const P; pLen: Integer;
                   CoFactor0, CoFactor1: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams): Boolean;
var
  V0, V1: PMPInteger;
  Z: PChar;
  z0Len, z1Len: Integer;
begin
  Result := (MPMSB(WPr) > 1) and (MPMSB(vPr) > 1);
  if not Result then Exit;
  V0 := nil;
  { -- WPr is normally a static key and doesn't have to be verified
       each time.

  MPKaratsubaExpMod(WPr,APriv0.Params.Q,APriv0.Params.P,V0);
  Result := MPMSB(V0) = 1;
  if not Result then begin
    MPDealloc(V0);
    Exit;
  end;
  }
  if CoFactor0 then
    Result := DLSVDP_DHC(APriv0,WPr,V0)
  else begin
    Result := True;
    DLSVDP_DH(APriv0,WPr,V0);
  end;
  if not Result then begin
    MPDealloc(V0);
    Exit;
  end;
  V1 := nil;
  if not CoFactor1 then begin
    if SubGroupValidation then begin
      if WPr <> VPr then begin
        MPKaratsubaExpMod(VPr,APriv1.Params.Q,APriv1.Params.P,V1);
        Result := MPMSB(V1) = 1;
        if not Result then begin
          MPDealloc(V0);
          MPDealloc(V1);
          Exit;
        end;
      end;
    end;
    DLSVDP_DH(APriv1,VPr,V1);
  end else
    Result := DLSVDP_DHC(APriv1,VPr,V1);
  if Result then begin
    z0Len := ((MPMSB(APriv0.Params.P) + 7) shr 3);
    z1Len := ((MPMSB(APriv1.Params.P) + 7) shr 3);
    GetMem(Z,z0Len + z1Len);
    try
      MPIntToBase256(V0,Z^,z0Len);
      MPIntToBase256(V1,(Z + z0Len)^,z1Len);
      Result := KDF(KeyDerivation,Z^,z0Len + z1Len,P,pLen,HashAlgorithm,K,kLen,MacTag);
    finally
      FreeMem(Z);
    end;
  end;
  MPDealloc(V0);
  MPDealloc(V1);
end;

function DLKAS_MQV(const APriv0: TDLPrivateKey;
                   APriv1: PMPInteger;
                   APubl1: PMPInteger;
                   WPr: PMPInteger;
                   VPr: PMPInteger;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams): Boolean;
var
  V: PMPInteger;
  Z: PChar;
  zLen: Integer;
begin
  Result := (MPMSB(WPr) > 1) and (MPMSB(vPr) > 1);
  if not Result then Exit;
  V := nil;
  { -- WPr is normally a static key and doesn't have to be verified
       each time.
  MPKaratsubaExpMod(WPr,APriv0.Params.Q,APriv0.Params.P,V);
  Result := MPMSB(V) = 1;
  if not Result then begin
    MPDealloc(V);
    Exit;
  end;
  }
  if SubGroupValidation and (WPr <> VPr) and not CoFactor then begin
    MPKaratsubaExpMod(VPr,APriv0.Params.Q,APriv0.Params.P,V);
    Result := MPMSB(V) = 1;
    if not Result then begin
      MPDealloc(V);
      Exit;
    end;
  end;
  if CoFactor then
    Result := DLSVDP_MQVC(APriv0.Params,APriv0.X,APriv1,APubl1,WPr,VPr,V)
  else begin
    Result := True;
    DLSVDP_MQV(APriv0.Params,APriv0.X,APriv1,APubl1,WPr,VPr,V);
  end;
  if Result then begin
    zLen := ((MPMSB(APriv0.Params.P) + 7) shr 3);
    GetMem(Z,zLen);
    try
      MPIntToBase256(V,Z^,zLen);
      Result := KDF(KeyDerivation,Z^,zLen,P,pLen,HashAlgorithm,K,kLen,MacTag);
    finally
      FreeMem(Z);
    end;
  end;
  MPDealloc(V);
end;

procedure EMSA1(const Digest; DigestCount: Integer;
                HashAlgorithm: THashAlgorithm;
                FBitSize: Cardinal;
                var F: PMPInteger);
var
  HC: THashClass;
  hBits: Cardinal;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    hBits := HC.DigestSize * 8
  else
    raise Exception.Create('EMSA1: Unsupported hash algorithm');
  Base256ToUMPInt(F,Digest,DigestCount,0);
  if hBits > FBitSize then
    MPShr(F,hBits - FBitSize);
end;

function EMSA1Verification(F: PMPInteger;
                           const Digest; DigestCount: Integer;
                           HashAlgorithm: THashAlgorithm;
                           FBitSize: Cardinal): Boolean;
var
  FPr: PMPInteger;
begin
  FPr := nil;
  EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,FPr);
  Result := MPCmpOffset(F,FPr) = 0;
  MPDealloc(FPr);
end;

function DLSSASignatureGeneration(const AKey: TDLPrivateKey;
                                  const Digest; DigestCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  NullMsg: Boolean;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer;
var
  qLen, sigLen, rLen, sLen, FBitSize: Cardinal;
  F, R, S: PMPInteger;
  P: PChar;
begin
  qLen := (MPMSB(AKey.Params.Q) + 7) shr 3;
  F := nil;
  R := nil;
  S := nil;
  case AKey.SignScheme of
    dlssaDSA: begin
                FBitSize := MPMSB(AKey.Params.Q);
                Base256ToUMPInt(F,Digest,DigestCount,FBitSize);
                DLSP_DSA(AKey,F,R,S);
              end;
    dlssaDSA_P1363:
              begin
                FBitSize := MPMSB(AKey.Params.Q);
                EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,F);
                DLSP_DSA(AKey,F,R,S);
              end;
    dlssaNR:  begin
                FBitSize := MPMSB(AKey.Params.Q) - 1;
                EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,F);
                DLSP_NR(AKey,F,R,S);
              end;
  else
    raise Exception.Create('DLSSASignatureGeneration: Unsupported scheme');
  end;
  P := @Sign;
  if DEREncode then begin
    rLen := (MPMSB(R) + 8) shr 3;                 
    sLen := (MPMSB(S) + 8) shr 3;
    sigLen := rLen + sLen + 6;
    Assert(Integer(sigLen) <= SignCount);
    P[0] := #$30;
    P[1] := Char(sigLen - 2);
    P[2] := #2;
    P[3] := Char(rLen);
    MPIntToBase256(R,P[4],rLen);
    P[4+rLen] := #2;
    P[5+rLen] := Char(sLen);
    MPIntToBase256(S,P[6+rLen],sLen);
  end else begin
    sigLen := qLen*2;
    Assert(Integer(sigLen) <= SignCount);
    UMPIntToBase256(R,P[0],qLen);
    UMPIntToBase256(S,P[qLen],qLen);
  end;
  Result := sigLen;
  MPDealloc(S);
  MPDealloc(R);
  MPDealloc(F);
end;

function DLSSASignatureGeneration(const AKey: TDLPrivateKey;
                                  const Msg; MsgCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer;
var
  HC: THashClass;
  H: THash;
  D: PChar;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    H := HC.Create(Msg,MsgCount)
  else
    raise Exception.Create('DLSSASignatureGeneration: Unsupported hash algorithm');
  try
    GetMem(D,H.DigestSize);
    try
      H.Done(D);
      try
        Result := DLSSASignatureGeneration(AKey,D^,H.DigestSize,H.Algorithm,MsgCount = 0,DEREncode,Sign,SignCount);
      finally
        ProtectClear(D^,H.DigestSize);
      end;
    finally
      FreeMem(D);
    end;
  finally
    H.Free;
  end;
end;

function DLSSASignatureVerification(const AKey: TDLPublicKey;
                                    const Digest; DigestCount: Integer;
                                    const Sign; SignCount: Integer;
                                    DEREncode: Boolean;
                                    HashAlgorithm: THashAlgorithm;
                                    NullMsg: Boolean): Boolean;
var
  qLen, sLen, FBitSize: Cardinal;
  F, R, S: PMPInteger;
  P: PChar;
begin
  qLen := (MPMSB(AKey.Params.Q) + 7) shr 3;
  if DEREncode then begin
    sLen := qLen*2 + 8;
    Result := (Integer(sLen) >= SignCount);
  end else begin
    sLen := qLen*2;
    Result := (Integer(sLen) = SignCount);
  end;
  if Result then begin
    F := nil;
    R := nil;
    S := nil;
    try
      P := @Sign;
      if DEREncode then begin
        Result := (P[0] = #$30) and (Byte(P[1]) <= sLen-2) and
                  (P[2] = #2) and (Byte(P[3]) <= qLen+1) and
                  (P[4+Byte(P[3])] = #2) and (Byte(P[5+Byte(P[3])]) <= qLen+1);
        if Result then begin
          Base256ToUMPInt(R,P[4],Byte(P[3]),qLen*8);
          Base256ToUMPInt(S,P[6+Byte(P[3])],Byte(P[5+Byte(P[3])]),qLen*8);
        end;
      end else begin
        Base256ToUMPInt(R,P[0],qLen,0);
        Base256ToUMPInt(S,P[qLen],qLen,0);
      end;
      if Result then begin
        case AKey.SignScheme of
          dlssaDSA: begin
                      FBitSize := MPMSB(AKey.Params.Q);
                      Base256ToUMPInt(F,Digest,DigestCount,FBitSize);
                      Result := DLVP_DSA(AKey,R,S,F);
                    end;
          dlssaDSA_P1363:
                    begin
                      FBitSize := MPMSB(AKey.Params.Q);
                      EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,F);
                      Result := DLVP_DSA(AKey,R,S,F);
                    end;
          dlssaNR:  begin
                      Result := DLVP_NR(AKey,R,S,F);
                      if Result then begin
                        FBitSize := MPMSB(AKey.Params.Q) - 1;
                        Result := EMSA1Verification(F,Digest,DigestCount,HashAlgorithm,FBitSize);
                      end;
                    end;
        else
          raise Exception.Create('DLSSASignatureVerification: Unsupported scheme');
        end;
      end;
    finally
      MPDealloc(S);
      MPDealloc(R);
      MPDealloc(F);
    end;
  end;
end;

function DLSSASignatureVerification(const AKey: TDLPublicKey;
                                    const Msg; MsgCount: Integer;
                                    const Sign; SignCount: Integer;
                                    DEREncode: Boolean;
                                    HashAlgorithm: THashAlgorithm): Boolean;
var
  HC: THashClass;
  H: THash;
  D: PChar;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then
    H := HC.Create(Msg,MsgCount)
  else
    raise Exception.Create('DLSSASignatureGeneration: Unsupported hash algorithm');
  try
    GetMem(D,H.DigestSize);
    try
      H.Done(D);
      try
        Result := DLSSASignatureVerification(AKey,D^,H.DigestSize,Sign,SignCount,DEREncode,H.Algorithm,MsgCount = 0);
      finally
        ProtectClear(D^,H.DigestSize);
      end;
    finally
      FreeMem(D);
    end;
  finally
    H.Free;
  end;
end;

procedure DLIESEncryption(const AKey: TDLPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          var EphemeralKey: PMPInteger;
                          var CipherText; CTCount: Integer;
                          var MAC; MACCount: Integer);
var
  X, iZ: PMPInteger;
  Z, K, M, C: PChar;
  bLen, zLen, I: Integer;
  HC: THashClass;
  H: THash;
begin
  Assert(CTCount >= MsgCount);   
{$IFDEF SHA512}
  if HashAlgorithm in [haSHA384,haSHA512] then
    bLen := 128
  else        
{$ENDIF SHA512}
    bLen := 64;
  X := nil;
  DLKeys(AKey.Params,X,EphemeralKey);
  iZ := nil;
  MPKaratsubaExpMod(AKey.Y,X,AKey.Params.P,iZ);
  MPDealloc(X);

  zLen := (MPMSB(AKey.Params.P) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(iZ,Z^,zLen);
    MPDealloc(iZ);
    GetMem(K,MsgCount + bLen);
    try
      KDF2(Z^,zLen,P1,P1Len,HashAlgorithm,K^,MsgCount + bLen);
      M := @Msg;
      C := @CipherText;
      for I := 0 to MsgCount - 1 do
        C[I] := Char(Byte(M[I]) xor Byte(K[I]));
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(C^,0)
      else
        raise Exception.Create('DLIESEncryption: Unsupported hash algorithm');
      try
        HMACBegin(K[MsgCount],bLen,H);
        H.HashData(C^,MsgCount);
        H.HashData(P2,P2Len);
        HMACEnd(K[MsgCount],bLen,H,Mac,MacCount);
      finally
        H.Free;
      end;
      ProtectClear(K^,MsgCount + bLen);
    finally
      FreeMem(K);
    end;
    ProtectClear(Z^,zLen);
  finally
    FreeMem(Z);
  end;
end;

procedure DLIESEncryption(const AKey: TDLPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          CipherClass: TCipherClass;
                          KeyLen: Integer;
                          var EphemeralKey: PMPInteger;
                          var CipherText; var CTCount: Integer;
                          var MAC; MACCount: Integer);
var
  C: TCipher;
  X, iZ: PMPInteger;
  Z, K, CT: PChar;
  BlockSize, bLen, zLen, tLen, I: Integer;
  HC: THashClass;
  H: THash;
begin
  BlockSize := CipherClass.BlockSize;
  tLen := BlockSize - (MsgCount mod BlockSize);
  Assert(CTCount >= MsgCount + tLen);
  CTCount := MsgCount + tLen;  
{$IFDEF SHA512}
  if HashAlgorithm in [haSHA384,haSHA512] then
    bLen := 128
  else        
{$ENDIF SHA512}
    bLen := 64;
  X := nil;
  DLKeys(AKey.Params,X,EphemeralKey);
  iZ := nil;
  MPKaratsubaExpMod(AKey.Y,X,AKey.Params.P,iZ);
  zLen := (MPMSB(AKey.Params.P) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(iZ,Z^,zLen);
    GetMem(K,KeyLen + bLen);
    try
      KDF2(Z^,zLen,P1,P1Len,HashAlgorithm,K^,KeyLen + bLen);
      Move(Msg,CipherText,MsgCount);
      CT := @CipherText;
      for I := MsgCount to MsgCount + tLen - 1 do
        CT[I] := Char(tLen);
      {$WARNINGS OFF}
      C := CipherClass.Create(K^,KeyLen,0);
      {$WARNINGS ON}
      try
        C.Encrypt(CipherText,CTCount);
      finally
        C.Free;
      end;
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(C,0)
      else
        raise Exception.Create('DLIESEncryption: Unsupported hash algorithm');
      try
        HMACBegin(K[KeyLen],bLen,H);
        H.HashData(CipherText,CTCount);
        H.HashData(P2,P2Len);
        HMACEnd(K[KeyLen],bLen,H,Mac,MacCount);
      finally
        H.Free;
      end;
      ProtectClear(K^,KeyLen + bLen);
    finally
      FreeMem(K);
    end;
    ProtectClear(Z^,zLen);
  finally
    FreeMem(Z);
  end;
  MPDealloc(iZ);
  MPDealloc(X);
end;

function DLIESDecryption(const AKey: TDLPrivateKey;
                         const EphemeralKey: PMPInteger;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         var Msg; MsgCount: Integer): Boolean; overload;
var
  iZ: PMPInteger;
  Z, K, M, C, vMac: PChar;
  bLen, zLen, I: Integer;
  HC: THashClass;
  H: THash;
begin
  Assert(CTCount <= MsgCount);
  Result := MPMSB(EphemeralKey) > 1;
  if not Result then Exit; 
{$IFDEF SHA512}
  if HashAlgorithm in [haSHA384,haSHA512] then
    bLen := 128
  else
{$ENDIF SHA512}
    bLen := 64;
  iZ := nil;
  MPKaratsubaExpMod(EphemeralKey,AKey.X,AKey.Params.P,iZ);
  Result := MPMSB(iZ) > 1;
  if not Result then begin
    MPDealloc(iZ);
    Exit;
  end;

  zLen := (MPMSB(AKey.Params.P) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(iZ,Z^,zLen);
    MPDealloc(iZ);
    GetMem(K,MsgCount + bLen);
    try
      KDF2(Z^,zLen,P1,P1Len,HashAlgorithm,K^,MsgCount + bLen);
      M := @Msg;
      C := @CipherText;
      for I := 0 to MsgCount - 1 do
        M[I] := Char(Byte(C[I]) xor Byte(K[I]));
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(nil^,0)
      else
        raise Exception.Create('DLIESEncryption: Unsupported hash algorithm');
      try
        HMACBegin(K[MsgCount],bLen,H);
        H.HashData(C^,MsgCount);
        H.HashData(P2,P2Len);

        GetMem(vMac,MacCount);
        try
          HMACEnd(K[MsgCount],bLen,H,vMac^,MacCount);
          Result := CompareMem(@Mac,vMac,MacCount);
          ProtectClear(vMac^,MacCount);
        finally
          FreeMem(vMac);
        end;
      finally
        H.Free;
      end;
      ProtectClear(K^,MsgCount + bLen);
    finally
      FreeMem(K);
    end;
    ProtectClear(Z^,zLen);
  finally
    FreeMem(Z);
  end;
end;

function DLIESDecryption(const AKey: TDLPrivateKey;
                         const EphemeralKey: PMPInteger;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         CipherClass: TCipherClass;
                         KeyLen: Integer;
                         var Msg; var MsgCount: Integer): Boolean; overload;
var
  C: TCipher;
  iZ: PMPInteger;
  Z, K, vMac, MT: PChar;
  bLen, zLen, tLen, I: Integer;
  HC: THashClass;
  H: THash;
begin
  Assert(CTCount <= MsgCount);
  Result := MPMSB(EphemeralKey) > 1;
  if not Result then Exit; 
{$IFDEF SHA512}
  if HashAlgorithm in [haSHA384,haSHA512] then
    bLen := 128
  else
{$ENDIF SHA512}
    bLen := 64;
  iZ := nil;
  MPKaratsubaExpMod(EphemeralKey,AKey.X,AKey.Params.P,iZ);
  Result := MPMSB(iZ) > 1;
  if not Result then begin
    MPDealloc(iZ);
    Exit;
  end;

  zLen := (MPMSB(AKey.Params.P) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(iZ,Z^,zLen);
    GetMem(K,KeyLen + bLen);
    try
      KDF2(Z^,zLen,P1,P1Len,HashAlgorithm,K^,KeyLen + bLen);
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(nil^,0)
      else
        raise Exception.Create('DLIESDecryption: Unsupported hash algorithm');
      try
        HMACBegin(K[KeyLen],bLen,H);
        H.HashData(CipherText,CTCount);
        H.HashData(P2,P2Len);

        GetMem(vMac,MacCount);
        try
          HMACEnd(K[KeyLen],bLen,H,vMac^,MacCount);
          Result := CompareMem(@Mac,vMac,MacCount);
          ProtectClear(vMac^,MacCount);
        finally
          FreeMem(vMac);
        end;
      finally
        H.Free;
      end;
      if Result then begin
        Move(CipherText,Msg,CTCount);
        {$WARNINGS OFF}
        C := CipherClass.Create(K^,KeyLen,0);
        {$WARNINGS ON}
        try
          C.Decrypt(Msg,CTCount);
        finally
          C.Free;
        end;
        MT := @Msg;
        tLen := Byte(MT[CTCount - 1]);
        Result := (tLen <= CipherClass.BlockSize) and (tLen > 0);
        if Result then begin
          MsgCount := CTCount - tLen;
          for I := MsgCount to CTCount - 1 do
            Result := Result and (MT[I] = Char(tLen))
        end;
      end;
      ProtectClear(K^,KeyLen + bLen);
    finally
      FreeMem(K);
    end;
    ProtectClear(Z^,zLen);
  finally
    FreeMem(Z);
  end;
  MPDealloc(iZ);
end;

{ TMPDLPrivateKey }

constructor TMPDLPrivateKey.CreateNew(const AAlgorithm: ObjectIdentifier;
  APBitSize, AQBitSize: Integer; AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler; ASignatureDigestAlg: THashAlgorithm;
  APublKey: IDLPublicKeyInfo);
var
  DLPubl: TDLPublicKey;
begin
  Creating := True;
  FillChar(DLPubl,SizeOf(DLPubl),0);
  try
    DLKeys(APBitSize,AQBitSize,FKey,DLPubl);
    Create(AKeyStruct,AKeyHandler);
    if Assigned(APublKey) then begin
      if AAlgorithm = id_dsa then
        APublKey.ImposeSubjectDSAPublicKey(DLPubl)
      else if AAlgorithm = id_nr then
        APublKey.ImposeSubjectNRPublicKey(DLPubl)
      else
        APublKey.ImposeSubjectDHPublicKey(DLPubl);
    end;
    if AAlgorithm = id_nr then
      FKey.SignScheme := dlssaNR;
    PublKey := APublKey;
    CopyDLSystemParams(FSystemParams,DLPubl.Params);
  finally
    DisposeDLPublicKey(DLPubl);
  end;
  SignatureDigestAlg := ASignatureDigestAlg;
  if Assigned(KeyHandler) then
    if not EncodeNewKey then
      raise Exception.Create('TMPDLPrivateKey.CreateNew: Could not encode key');
  Creating := False;
end;

constructor TMPDLPrivateKey.CreateNewParams(
  const AAlgorithm: ObjectIdentifier; AParams: TDLSystemParams;
  AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
  APublKey: IDLPublicKeyInfo);
var
  DLPubl: TDLPublicKey;
begin
  Creating := True;
  FillChar(DLPubl,SizeOf(DLPubl),0);
  try
    DLKeys(AParams,FKey,DLPubl.Y);
    Create(AKeyStruct,AKeyHandler);
    if Assigned(APublKey) then begin
      CopyDLSystemParams(DLPubl.Params,AParams);
      if AAlgorithm = id_dsa then
        APublKey.ImposeSubjectDSAPublicKey(DLPubl)
      else if AAlgorithm = id_nr then
        APublKey.ImposeSubjectNRPublicKey(DLPubl)
      else
        APublKey.ImposeSubjectDHPublicKey(DLPubl);
    end;
    if AAlgorithm = id_nr then
      FKey.SignScheme := dlssaNR;
    PublKey := APublKey;
    CopyDLSystemParams(FSystemParams,AParams);
  finally
    DisposeDLPublicKey(DLPubl);
  end;
  if Assigned(AKeyHandler) then
    if not EncodeNewKey then
      raise Exception.Create('TMPDLPrivateKey.CreateNewParams: Could not encode key');
  Creating := True;
end;

function TMPDLPrivateKey.DecodeASNKey(K: TASN1Struct): Boolean;
var
  OID: string;
  P, A: PASN1Struct;
begin
  Result := True;
  OID := Algorithm;
  if (OID = id_dsa) or (OID = id_nr) then begin
    K.Items[0]^.ContentAsUMPInt(FKey.X);
    P := K.Items[1];
    P^.Items[0]^.ContentAsUMPInt(FKey.params.P);
    P^.Items[1]^.ContentAsUMPInt(FKey.params.Q);
    P^.Items[2]^.ContentAsUMPInt(FKey.params.G);
    MPDealloc(FKey.params.J);
    FKey.Params.J := nil;
    if OID = id_dsa then
      FKey.SignScheme := dlssaDSA
    else
      FKey.SignScheme := dlssaNR;
  end else if OID = dhPublicNumber then begin  
    K.Items[0]^.ContentAsUMPInt(FKey.X);
    P := K.Items[1];
    P^.Items[0]^.ContentAsUMPInt(FKey.params.P);
    P^.Items[1]^.ContentAsUMPInt(FKey.params.G);
    P^.Items[2]^.ContentAsUMPInt(FKey.params.Q);
    if (P^.ItemCount > 3) then begin
      A := P^.Items[3];
      if A^.Length > 0 then
        A^.ContentAsUMPInt(FKey.params.J);
    end;
  end else
    Result := False;
  if Result then
    CopyDLSystemParams(FSystemParams,FKey.Params);
end;

function TMPDLPrivateKey.DecryptKeyAgreement(const PublKey: TDLPublicKey;
  var Secret; SLen: Integer): Boolean;
begin
  LockKey;
  try
    if KeyInitialized then
      InternalDecryptKeyAgreementRec(PublKey,Secret,SLen,Result)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

destructor TMPDLPrivateKey.Destroy;
begin
  DisposeDLSystemParams(FSystemParams);
  inherited Destroy;
end;

procedure TMPDLPrivateKey.DisposeKey;
begin
  inherited;
  DisposeDLPrivateKey(FKey);
end;

function TMPDLPrivateKey.DLPublicKey(var APublKey: TDLPublicKey): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      MPDL.DLPublicKey(FKey,APublKey);
  finally
    UnlockKey;
  end;
end;

function TMPDLPrivateKey.EncodeASNKey(K: TASN1Struct): Boolean;
var
  A, P: PASN1Struct;
begin
  Result := True;
  if K.ItemCount > 0 then begin
    if (Algorithm = id_dsa) or (Algorithm = id_nr) then begin
      K.EditField('privateKey',FKey.X,True);
      K.EditField('/params/p',FKey.Params.P,True);
      K.EditField('/params/q',FKey.Params.Q,True);
      K.EditField('/params/g',FKey.Params.G,True);
    end else if Algorithm = dhPublicNumber then begin
      K.EditField('privateKey',FKey.X,True);
      K.EditField('/params/p',FKey.Params.P,True);
      K.EditField('/params/g',FKey.Params.G,True);
      K.EditField('/params/q',FKey.Params.Q,True);
      K.EditField('/params/j',FKey.Params.J,True);
    end else
      Result := False;
  end else begin
    K.Constructed := True;
    A := K.AddField('privateKey','OCTET STRING',nil);
    A^.EditContent(FKey.X,True);
    A := K.AddField('params','SEQUENCE',nil);
    if (Algorithm = id_dsa) or (Algorithm = id_nr) then begin
      P := A^.AddField('p','INTEGER',nil);
      P^.EditContent(FKey.Params.P,True);
      P := A^.AddField('q','INTEGER',nil);
      P^.EditContent(FKey.Params.Q,True);
      P := A^.AddField('g','INTEGER',nil);
      P^.EditContent(FKey.Params.G,True);
    end else if Algorithm = dhPublicNumber then begin
      P := A^.AddField('p','INTEGER',nil);
      P^.EditContent(FKey.Params.P,True);
      P := A^.AddField('g','INTEGER',nil);
      P^.EditContent(FKey.Params.G,True);
      P := A^.AddField('q','INTEGER',nil);
      P^.EditContent(FKey.Params.Q,True);
      if Assigned(FKey.Params.J) then begin
        P := A^.AddField('j','INTEGER',nil);
        P^.EditContent(FKey.Params.J,True);
      end;
    end else
      Result := False;
  end;
  K.CalculateLength;
end;

function TMPDLPrivateKey.GetAlgorithm: ObjectIdentifier;
begin
  if KeyInitialized and (FKey.SignScheme = dlssaNR) then
    Result := id_nr
  else
    Result := inherited GetAlgorithm;
end;

function TMPDLPrivateKey.GetSignatureLength: Integer;
var
  DLPublIntf: IDLPublicKeyInfo;
  DLPubl: TDLPublicKey;
  Done: Boolean;

  function MSBToSignSize(Q: PMPInteger): Integer;
  begin
    Result := (MPMSB(Q) + 7) shr 3;
    if FDEREncodeSignature then
      Result := Result*2 + 6
    else
      Result := Result*2;
  end;

begin
  if Assigned(FSystemParams.Q) then
    Result := MSBToSignSize(FSystemParams.Q)
  else begin
    Result := 0;
    Done := False;
    if Assigned(PublKey) then begin
      Done := PublKey.QueryInterface(IDLPublicKeyInfo,DLPublIntf) = 0;
      if Done then begin
        FillChar(DLPubl,SizeOf(DLPubl),0);
        try
          if Algorithm = id_dsa then
            DLPublIntf.ExtractSubjectDSAPublicKey(DLPubl)
          else if Algorithm = id_nr then
            DLPublIntf.ExtractSubjectNRPublicKey(DLPubl)
          else
            DLPublIntf.ExtractSubjectDHPublicKey(DLPubl);
          CopyDLSystemParams(FSystemParams,DLPubl.Params);
          Result := MSBToSignSize(DLPubl.Params.Q);
        finally
          DisposeDLPublicKey(DLPubl);
        end;
      end;
    end;
    if not Done then begin
      LockKey;
      UnlockKey;
      if Assigned(FSystemParams.Q) then
        Result := MSBToSignSize(FSystemParams.Q)
      else
        Result := inherited GetSignatureLength;
    end;
  end;
end;

function TMPDLPrivateKey.GetSystemParams: PDLSystemParams;
begin
  if FSystemParams.P = nil then begin
    LockKey;
    UnlockKey;
  end;
  Result := @FSystemParams;
end;

procedure TMPDLPrivateKey.InternalDecryptKeyAgreement(
  PublKey: TMPPublicKey; var Secret; SLen: Integer; out Result: Boolean);
var
  Publ: TMPDLPublicKey;
begin
  Result := (PublKey is TMPDLPublicKey);
  if Result and (Algorithm = dhPublicNumber) then begin
    Publ := TMPDLPublicKey(PublKey);
    Publ.LockKey;
    try
      InternalDecryptKeyAgreementRec(Publ.FKey,Secret,SLen,Result);
    finally
      Publ.UnlockKey;
    end;
  end;
end;

procedure TMPDLPrivateKey.InternalDecryptKeyAgreementIntf(
  PublKey: IPKPublicKeyInfo; var Secret; SLen: Integer;
  out Result: Boolean);
var
  DLPublIntf: IDLPublicKeyInfo;
  DLPubl: TDLPublicKey;
begin
  Result := PublKey.QueryInterface(IDLPublicKeyInfo,DLPublIntf) = 0;
  if Result and (Algorithm = dhPublicNumber) then begin
    FillChar(DLPubl,SizeOf(DLPubl),0);
    try
      DLPublIntf.ExtractSubjectDHPublicKey(DLPubl);
      InternalDecryptKeyAgreementRec(DLPubl,Secret,SLen,Result);
    finally
      DisposeDLPublicKey(DLPubl);
    end;
  end;
end;

procedure TMPDLPrivateKey.InternalDecryptKeyAgreementRec(
  const PublKey: TDLPublicKey; var Secret; SLen: Integer;
  out Result: Boolean);
begin
  Result := CompareDomainParameters(FKey.Params,PublKey.Params);
  if Result then
    Result := DLKAS_DH1(FKey,PublKey.Y,
                        Pointer(FP)^,Length(FP),
                        CoFactor,KAHashAlg,KDF,
                        Secret,SLen);
end;

procedure TMPDLPrivateKey.InternalSignBuf(const Msg; MsgLen: Integer;
  var Sign; var SignLen: Integer);
begin
  if (Algorithm = id_dsa) or (Algorithm = id_nr) then
    SignLen := DLSSASignatureGeneration(FKey,
                                        Msg,MsgLen,
                                        SignatureDigestAlg,
                                        DEREncodeSignature,
                                        Sign,SignLen)
  else
    SignLen := 0;
end;

procedure TMPDLPrivateKey.InternalSignDig(const Digest; DigestLen: Integer;
  var Sign; var SignLen: Integer);
begin
  if (Algorithm = id_dsa) or (Algorithm = id_nr) then
    SignLen := DLSSASignatureGeneration(FKey,
                                        Digest,DigestLen,
                                        SignatureDigestAlg,
                                        False,
                                        DEREncodeSignature,
                                        Sign,SignLen)
  else
    SignLen := 0;
end;

procedure TMPDLPrivateKey.InternalSignSigned(Signed: IUnknown;
  CACert: TASN1Struct; var OK: Boolean);
var
  S: ISigned;
begin
  OK := Signed.QueryInterface(ISigned,S) = 0;
  if OK then begin
    if (Algorithm = id_dsa) or (Algorithm = id_nr) then
      OK := S.DSASign(FKey,CACert)
    else
      OK := False;
  end;
end;

procedure TMPDLPrivateKey.SetPublKey(const Value: IPKPublicKeyInfo);
var
  Publ: IDLPublicKeyInfo;
  OID: string;
  DLPubl: TDLPublicKey;
  Res: Boolean;
begin
  if Value.QueryInterface(IDLPublicKeyInfo,Publ) = 0 then begin
    FillChar(DLPubl,SizeOf(DLPubl),0);
    try
      OID := Publ.AlgorithmIdentifier;
      if OID = '' then begin       
        LockKey;
        try
          if KeyInitialized then begin
            MPDL.DLPublicKey(FKey,DLPubl);
            OID := GetAlgorithm;
            if OID = dhPublicNumber then
              Publ.ImposeSubjectDHPublicKey(DLPubl)
            else if OID = id_nr then
              Publ.ImposeSubjectNRPublicKey(DLPubl)
            else if OID = id_dsa then
              Publ.ImposeSubjectDSAPublicKey(DLPubl)
            else
              Publ := nil;
          end else
            Publ := nil;
        finally
          UnlockKey;
        end;
      end else begin
        if OID = dhPublicNumber then
          Res := Publ.ExtractSubjectDHPublicKey(DLPubl)
        else if OID = id_nr then
          Res := Publ.ExtractSubjectNRPublicKey(DLPubl)
        else
          Res := Publ.ExtractSubjectDSAPublicKey(DLPubl);
        if Res and not Creating then begin
          LockKey;
          try
            if not (KeyInitialized and ValidateDLPrivateKey(FKey,DLPubl)) then
              Publ := nil;
          finally
            UnlockKey;
          end;
        end else if not Res then
          Publ := nil;
      end;
    finally
      DisposeDLPublicKey(DLPubl);
    end;
    inherited SetPublKey(Publ);
  end else
    inherited SetPublKey(nil);
end;

{ TMPDLPublicKey }

constructor TMPDLPublicKey.CreateFromRecord(
  const AAlgorithm: ObjectIdentifier; const DLPublKey: TDLPublicKey;
  AKeyIntf: IDLPublicKeyInfo);
begin
  Create(AKeyIntf);
  MPCopy2(DLPublKey.Y,FKey.Y);
  CopyDLSystemParams(FKey.Params,DLPublKey.Params);
  FAlgorithm := AAlgorithm;
  EncodeKey;
end;

function TMPDLPublicKey.DecodeKey: Boolean;
var
  PublInfo: IDLPublicKeyInfo;
begin
  Result := (KeyIntf = nil) and Assigned(FKey.Y);
  if not Result then begin
    Result := (KeyIntf.QueryInterface(IDLPublicKeyInfo,PublInfo) = 0);
    if Result then begin
      if PublInfo.AlgorithmIdentifier = id_dsa then
        Result := PublInfo.ExtractSubjectDSAPublicKey(FKey)
      else
        Result := PublInfo.ExtractSubjectDHPublicKey(FKey);
    end;
  end;
end;

procedure TMPDLPublicKey.DisposeKey;
begin
  inherited;
  DisposeDLPublicKey(FKey);
end;

procedure TMPDLPublicKey.EncodeKey;
var
  PublInfo: IDLPublicKeyInfo;
begin
  if Assigned(KeyIntf) and
     (KeyIntf.QueryInterface(IDLPublicKeyInfo,PublInfo) = 0) then begin
    if FAlgorithm = id_dsa then
      PublInfo.ImposeSubjectDSAPublicKey(FKey)
    else
      PublInfo.ImposeSubjectDHPublicKey(FKey);
  end;
end;

function TMPDLPublicKey.GetSystemParams: PDLSystemParams;
begin
  Result := @FKey.Params;
end;

function TMPDLPublicKey.InternalVerBuf(const Msg; MsgLen: Integer;
  const Sign; SignLen: Integer): Boolean;
begin
  Result := DLSSASignatureVerification(FKey,
                                       Msg,MsgLen,
                                       Sign,SignLen,
                                       DEREncodeSignature,
                                       SignatureDigestAlg);
end;

function TMPDLPublicKey.InternalVerDig(const Digest; DigestLen: Integer;
  const Sign; SignLen: Integer): Boolean;
begin
  Result := DLSSASignatureVerification(FKey,
                                       Digest,DigestLen,
                                       Sign,SignLen,
                                       DEREncodeSignature,
                                       SignatureDigestAlg,
                                       False);
end;

procedure TMPDLPublicKey.SetDEREncodeSignature(const Value: Boolean);
begin
  FDEREncodeSignature := Value;
end;

{ TMPKAPrivateKey }

constructor TMPKAPrivateKey.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
begin
  FDEREncodeSignature := True;
  inherited Create(AKeyStruct,AKeyHandler);
end;

function TMPKAPrivateKey.GetCoFactor: Boolean;
begin
  Result := FCoFactor;
end;

function TMPKAPrivateKey.GetDEREncodeSignature: Boolean;
begin
  Result := FDEREncodeSignature;
end;

function TMPKAPrivateKey.GetKAHashAlg: THashAlgorithm;
begin
  Result := FKAHashAlg;
end;

function TMPKAPrivateKey.GetKDF: TKeyDerivation;
begin
  Result := FKDF;
end;

function TMPKAPrivateKey.GetP: OctetString;
begin
  Result := FP;
end;

procedure TMPKAPrivateKey.SetCoFactor(const Value: Boolean);
begin
  FCoFactor := Value;
end;

procedure TMPKAPrivateKey.SetDEREncodeSignature(const Value: Boolean);
begin
  FDEREncodeSignature := Value;
end;

procedure TMPKAPrivateKey.SetKAHashAlg(const Value: THashAlgorithm);
begin
  FKAHashAlg := Value;
end;

procedure TMPKAPrivateKey.SetKDF(const Value: TKeyDerivation);
begin
  FKDF := Value;
end;

procedure TMPKAPrivateKey.SetP(const Value: OctetString);
begin
  FP := Value;
end;

{ TMPIFPrivateKeyTLS }

constructor TMPDLPrivateKeyTLS.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
begin
  inherited;
  KDF := kdNone;
  CoFactor := True;
end;

end.
