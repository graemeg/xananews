{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPEC Unit                                         }
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
unit MpEC;

interface

uses
  MpArithTypes, SecUtils, Asn1, MpPK, MpDL;

type
  TECKeyAgreementScheme = (eckasDH1,eckasDH2,eckasMQV);
  TECSignatureScheme = (ecssaDSA,ecssaNR);

  TECPrivateKey = record
    KAScheme: TECKeyAgreementScheme;
    SignScheme: TECSignatureScheme;
    Params: TECurve;
    S: PMPInteger;
  end;

  TECPublicKey = record
    KAScheme: TECKeyAgreementScheme;
    SignScheme: TECSignatureScheme;
    Params: TECurve;
    W: TECPoint;
  end;

  IECPublicKeyInfo = interface(IPKPublicKeyInfo)
    ['{6DB51720-34CB-43CA-892B-9B25AD39D943}']
    function ExtractSubjectECPublicKey(var ECKey: TECPublicKey): Boolean;
    procedure ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
  end;

  IMPECPrivateKey = interface(IMPKAPrivateKey)
    ['{1B121772-0F4A-4971-A3BB-735D532656A7}']
    function GetSystemParams: PECurve;                  
    function DecryptKeyAgreement(const PublKey: TECPublicKey;
                                 var Secret; SLen: Integer): Boolean; overload;
    function ECPublicKey(var APublKey: TECPublicKey): Boolean;
    property SystemParams: PECurve read GetSystemParams;
  end;

  TMPECPrivateKey = class(TMPKAPrivateKey,IMPECPrivateKey)
  private
    FKey: TECPrivateKey;
    FSystemParams: TECurve;
    FCreating: Boolean;
  protected
    function DecodeASNKey(K: TASN1Struct): Boolean; override;
    procedure DisposeKey; override;
    function EncodeASNKey(K: TASN1Struct): Boolean; override;
    function GetAlgorithm: ObjectIdentifier; override;
    function GetSignatureLength: Integer; override;
    function GetSystemParams: PECurve;
    procedure InternalDecryptKeyAgreement(PublKey: TMPPublicKey;
                                          var Secret; SLen: Integer;
                                          out Result: Boolean); override;
    procedure InternalDecryptKeyAgreementIntf(PublKey: IPKPublicKeyInfo;
                                              var Secret; SLen: Integer;
                                              out Result: Boolean); override;
    procedure InternalDecryptKeyAgreementRec(PublKey: TECPublicKey;
                                             var Secret; SLen: Integer;
                                             out Result: Boolean);
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
                          const ACurve: ObjectIdentifier;
                          AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
                          ASignatureDigestAlg: THashAlgorithm;
                          APublKey: IECPublicKeyInfo);
    constructor CreateNewParams(const AAlgorithm: ObjectIdentifier;
                                AParams: TECurve;
                                AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
                                APublKey: IECPublicKeyInfo);
    destructor Destroy; override;       
    function DecryptKeyAgreement(const PublKey: TECPublicKey;
                                 var Secret; SLen: Integer): Boolean; overload;
    function ECPublicKey(var APublKey: TECPublicKey): Boolean;
    property SystemParams: PECurve read GetSystemParams;
  end;

  TMPECPublicKey = class(TMPPublicKey)
  private
    FKey: TECPublicKey;
    FDEREncodeSignature: Boolean;
    FAlgorithm: ObjectIdentifier;
    function GetSystemParams: PECurve;
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
                                 const ECPublKey: TECPublicKey;
                                 AKeyIntf: IECPublicKeyInfo);
    property DEREncodeSignature: Boolean read FDEREncodeSignature write SetDEREncodeSignature;
    property Curve: PECurve read GetSystemParams;
  end;
                                                                              
function OIDToCurve(const OID: ObjectIdentifier; var Curve: TECurve): Boolean;  
function CurveToOID(const Curve: TECurve): ObjectIdentifier;

procedure DisposeECPrivateKey(var AKey: TECPrivateKey);
procedure DisposeECPublicKey(var AKey: TECPublicKey);

procedure ECKeys(const Params: TECurve;
                 var APriv: TECPrivateKey;
                 var APubl: TECPublicKey); overload;
procedure ECKeys(const Params: TECurve;
                 var APriv: TECPrivateKey;
                 var APubl: TECPoint); overload;
procedure ECKeys(const Params: TECurve;
                 var APriv: PMPInteger;
                 var APubl: TECPoint); overload;

procedure ECPublicKey(const APriv: TECPrivateKey;
                      var APubl: TECPublicKey);

function ValidateECPrivateKey(APriv: TECPrivateKey; APubl: TECPublicKey): Boolean;

// Key Agreement Schemes.
function ECKAS_DH1(const APriv: TECPrivateKey;
                   const WPr: TECPoint;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams = nil): Boolean;  
function ECKAS_DH2(const APriv0: TECPrivateKey;
                   const APriv1: TECPrivateKey;
                   const WPr: TECPoint;
                   const VPr: TECPoint;
                   const P; pLen: Integer;
                   CoFactor0, CoFactor1: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams = nil): Boolean;
function ECKAS_MQV(const APriv0: TECPrivateKey;
                   APriv1: PMPInteger;
                   const APubl1: TECPoint;
                   const WPr: TECPoint;
                   const VPr: TECPoint;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams = nil): Boolean;
                     
// Signature Schemes:
//  DEREncode = True:  Sign will be a DER encoded Dss-Sig-Value structure.
//  DEREncode = False: Sign will be the concatenation of r and s.
function ECSSASignatureGeneration(const AKey: TECPrivateKey;
                                  const Digest; DigestCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  NullMsg: Boolean;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer; overload;

function ECSSASignatureGeneration(const AKey: TECPrivateKey;
                                  const Msg; MsgCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer; overload;

function ECSSASignatureVerification(const AKey: TECPublicKey;
                                    const Digest; DigestCount: Integer;
                                    const Sign; SignCount: Integer;
                                    DEREncode: Boolean;
                                    HashAlgorithm: THashAlgorithm;
                                    NullMsg: Boolean): Boolean; overload;

function ECSSASignatureVerification(const AKey: TECPublicKey;
                                    const Msg; MsgCount: Integer;
                                    const Sign; SignCount: Integer;
                                    DEREncode: Boolean;
                                    HashAlgorithm: THashAlgorithm): Boolean; overload;
                             
// Integrated Encryption Schemes:
procedure ECIESEncryption(const AKey: TECPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          var EphemeralKey: TECPoint;
                          var CipherText; CTCount: Integer;
                          var MAC; MACCount: Integer); overload;

procedure ECIESEncryption(const AKey: TECPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          CipherClass: TCipherClass;
                          KeyLen: Integer;
                          var EphemeralKey: TECPoint;
                          var CipherText; var CTCount: Integer;
                          var MAC; MACCount: Integer); overload;

function ECIESDecryption(const AKey: TECPrivateKey;
                         const EphemeralKey: TECPoint;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         var Msg; MsgCount: Integer): Boolean; overload;

function ECIESDecryption(const AKey: TECPrivateKey;
                         const EphemeralKey: TECPoint;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         CipherClass: TCipherClass;
                         KeyLen: Integer;
                         var Msg; var MsgCount: Integer): Boolean; overload;


implementation

uses
  SysUtils, MPArith, MpECArith, MpEC_NISTCurves, MpEC_X9_62Curves, Pkix,
  Pkix_Cert;

function OIDToCurve(const OID: ObjectIdentifier; var Curve: TECurve): Boolean;
begin
  Result := True;
  if OID = prime192v1 then
    ECurveP192_1(Curve)
  else if OID = prime192v2 then
    ECurveP192_2(Curve)
  else if OID = prime192v3 then
    ECurveP192_3(Curve)
  else if OID = prime224 then
    ECurveP224(Curve)
  else if OID = prime239v1 then
    ECurveP239_1(Curve)
  else if OID = prime239v2 then
    ECurveP239_2(Curve)
  else if OID = prime239v3 then
    ECurveP239_3(Curve)
  else if OID = prime256v1 then
    ECurveP256_1(Curve)
  else if OID = prime384 then
    ECurveP384(Curve)
  else if OID = prime521 then
    ECurveP521(Curve)
  else
    Result := False;
end;

function CurveToOID(const Curve: TECurve): ObjectIdentifier;
begin
  Result := '';
  if TryIsECurveP192_1(Curve) then
    Result := prime192v1
  else if TryIsECurveP192_2(Curve) then
    Result := prime192v2
  else if TryIsECurveP192_3(Curve) then
    Result := prime192v3
  else if TryIsECurveP239_1(Curve) then
    Result := prime239v1
  else if TryIsECurveP239_2(Curve) then
    Result := prime239v2
  else if TryIsECurveP239_3(Curve) then
    Result := prime239v3
  else if TryIsECurveP256_1(Curve) then
    Result := prime256v1
  else if TryIsECurveP224(Curve) then
    Result := GetRegistredOID('prime224')
  else if TryIsECurveP384(Curve) then
    Result := GetRegistredOID('prime384')
  else if TryIsECurveP521(Curve) then
    Result := GetRegistredOID('prime521');
end;

procedure DisposeECPrivateKey(var AKey: TECPrivateKey);
begin
  ECDealloc(AKey.Params);
  MPDealloc(AKey.S);
  AKey.S := nil;
end;

procedure DisposeECPublicKey(var AKey: TECPublicKey);
begin
  ECDealloc(AKey.Params);
  ECDealloc(AKey.W);
end;
            
function ValidateECPrivateKey(APriv: TECPrivateKey; APubl: TECPublicKey): Boolean;
var
  W: TECPoint;
begin
  Result := ECCompare(APriv.Params,APubl.Params);
  if Result then begin
    FillChar(W,SizeOf(W),0);
    ECScalarMulW(APriv.Params.G,APriv.S,APriv.Params,W);
    Result := (MPCmpOffset(W.X,APubl.W.X) = 0) and
              (MPCmpOffset(W.Y,APubl.W.Y) = 0);
    ECDealloc(W);
  end;
end;

procedure ECKeys(const Params: TECurve;
                 var APriv: TECPrivateKey;
                 var APubl: TECPublicKey);
begin
  MPRandomBound(APriv.S,nil,Params.R);
  ECScalarMulW(Params.G,APriv.S,Params,APubl.W);
  ECCopy2(Params,APriv.Params);
  ECCopy2(Params,APubl.Params);
end;

procedure ECKeys(const Params: TECurve;
                 var APriv: TECPrivateKey;
                 var APubl: TECPoint);
begin                                 
  MPRandomBound(APriv.S,nil,Params.R);
  ECScalarMulW(Params.G,APriv.S,Params,APubl);
  ECCopy2(Params,APriv.Params);
end;

procedure ECKeys(const Params: TECurve;
                 var APriv: PMPInteger;
                 var APubl: TECPoint);
begin
  MPRandomBound(APriv,nil,Params.R);
  ECScalarMulW(Params.G,APriv,Params,APubl);
end;

procedure ECPublicKey(const APriv: TECPrivateKey;
                      var APubl: TECPublicKey);
begin
  ECScalarMulW(APriv.Params.G,APriv.S,APriv.Params,APubl.W);
  ECCopy2(APriv.Params,APubl.Params);
end;

procedure ECSVDP_DH(const APriv: TECPrivateKey;
                    const APubl: TECPoint;
                    var Z: PMPInteger);
var
  X: TECPoint;
begin
  FillChar(X,SizeOf(X),0);
  ECScalarMulW(APubl,APriv.S,APriv.Params,X);
  MPCopy2(X.X,Z);
  ECDealloc(X);
end;

function ECSVDP_DHC(const APriv: TECPrivateKey;
                    const APubl: TECPoint;
                    var Z: PMPInteger): Boolean;
var
  X: TECPoint;
  J, JInv, T: PMPInteger;
begin
  if MPMSB(APriv.Params.K) = 1 then begin
    Result := True;
    ECSVDP_DH(APriv,APubl,Z);
  end else begin
    J := MPCopy(APriv.Params.K);
    FillChar(X,SizeOf(X),0);
    JInv := nil;
    MPBinaryInvMod(J,APriv.Params.R,JInv);
    T := nil;
    MPMulMod2(APriv.S,JInv,APriv.Params.R,T);
    MPDealloc(JInv);
    MPMul(T,J);
    MPDealloc(J);
    ECScalarMulW(APubl,T,APriv.Params,X);
    Result := not ECIsInf(X,APriv.Params);
    MPCopy2(X.X,Z);
    ECDealloc(X);
    MPDealloc(T);
  end;
end;

procedure ECSVDP_MQV(const Params: TECurve;
                     const S: PMPInteger;
                     const U: PMPInteger;
                     const V: TECPoint;
                     const WPr: TECPoint;
                     const VPr: TECPoint;
                     var Z: PMPInteger);
var
  T, TPr, PowH, E: PMPInteger;
  H: Integer;
  P: TECPoint;
begin
  H := (MPMSB(Params.R) + 1) shr 1;
  PowH := IntToMPInt(1);
  MPShl(PowH,H);

  T := MPCopy(V.X);
  MPMask(T,H);
  MPAdd(T,PowH);

  TPr := MPCopy(VPr.X);
  MPMask(TPr,H);
  MPAdd(TPr,PowH);

  MPDealloc(PowH);

  E := MPCopy(S);
  MPMulMod(E,T,Params.R);
  MPAdd(E,U);
  if MPCmpOffset(E,Params.R) > 0 then
    MPSub(E,Params.R);

  FillChar(P,SizeOf(P),0);
  ECScalarMulW(WPr,TPr,Params,P);
  ECFullAddition(P,VPr,Params);
  ECScalarMulW(P,E,Params,P);

  MPCopy2(P.X,Z);

  ECDealloc(P);
  MPDealloc(T);
  MPDealloc(TPr);
  MPDealloc(E);
end;

function ECSVDP_MQVC(const Params: TECurve;
                     const S: PMPInteger;
                     const U: PMPInteger;
                     const V: TECPoint;
                     const WPr: TECPoint;
                     const VPr: TECPoint;
                     var Z: PMPInteger): Boolean;
var
  T, TPr, PowH, E: PMPInteger;
  H: Integer;
  P: TECPoint;
begin
  if MPMSB(Params.K) = 1 then begin
    Result := True;
    ECSVDP_MQV(Params,S,U,V,WPr,VPr,Z)
  end else begin
    H := (MPMSB(Params.R) + 1) shr 1;
    PowH := IntToMPInt(1);
    MPShl(PowH,H);

    T := MPCopy(V.X);
    MPMask(T,H);
    MPAdd(T,PowH);

    TPr := MPCopy(VPr.X);
    MPMask(TPr,H);
    MPAdd(TPr,PowH);

    MPDealloc(PowH);

    E := MPCopy(S);
    MPMulMod(E,T,Params.R);
    MPAdd(E,U);
    if MPCmpOffset(E,Params.R) > 0 then
      MPSub(E,Params.R);

    MPDivMod(E,Params.K,Params.R);
    MPMul(E,Params.K);

    FillChar(P,SizeOf(P),0);
    ECScalarMulW(WPr,TPr,Params,P);
    ECFullAddition(P,VPr,Params);
    ECScalarMulW(P,E,Params,P);

    Result := not ECIsInf(P,Params);

    MPCopy2(P.X,Z);

    ECDealloc(P);
    MPDealloc(T);
    MPDealloc(TPr);
    MPDealloc(E);
  end;
end;

procedure ECSP_NR(const APriv: TECPrivateKey; F: PMPInteger; var R, S: PMPInteger);
var
  U: PMPInteger;
  V: TECPoint;
begin
  FillChar(V,SizeOf(V),0);
  U := nil;
  ECKeys(APriv.Params,U,V);
  MPCopy2(V.X,R);
  ECDealloc(V);
  MPAdd(R,F);
  MPMod(R,APriv.Params.R);

  MPMulMod2(R,APriv.S,APriv.Params.R,S);
  S^.Sign := -1;
  MPAdd(S,U);
  if S^.Sign < 0 then
    MPAdd(S,APriv.Params.R);
  MPDealloc(U);
end;

function ECVP_NR(const APubl: TECPublicKey; R, S: PMPInteger; var F: PMPInteger): Boolean;
var
  T, T0: TECPoint;
begin
  Result := (MPMSB(R) > 0) and (MPCmpOffset(R,APubl.Params.R) < 0) and
                               (MPCmpOffset(S,APubl.Params.R) < 0) and
            (R^.Sign > 0) and (S^.Sign >= 0);
  if not Result then Exit;
  FillChar(T,SizeOf(T),0);
  ECScalarMulW(APubl.Params.G,S,APubl.Params,T);
  FillChar(T0,SizeOf(T0),0);
  ECScalarMulW(APubl.W,R,APubl.Params,T0);
  ECFullAddition(T,T0,APubl.Params);
  ECDealloc(T0);
  Result := not ECIsInf(T,APubl.Params);
  if Result then begin
    MPMod(T.X,APubl.Params.R);
    MPCopy2(R,F);
    MPSub(F,T.X);
    if F^.Sign < 0 then
      MPAdd(F,APubl.Params.R);
  end;
  ECDealloc(T);
end;

procedure ECSP_DSA(const APriv: TECPrivateKey; F: PMPInteger; var R, S: PMPInteger);
var
  U: PMPInteger;
  V: TECPoint;
begin
  FillChar(V,SizeOf(V),0);
  U := nil;
  ECKeys(APriv.Params,U,V);
  MPMod2(V.X,APriv.Params.R,R);
  ECDealloc(V);

  MPMulMod2(R,APriv.S,APriv.Params.R,S);
  MPAdd(S,F);
  MPMod(S,APriv.Params.R);
  MPDivMod(S,U,APriv.Params.R);
  MPDealloc(U);
end;

function ECVP_DSA(const APubl: TECPublicKey; R, S, F: PMPInteger): Boolean;
var
  H, H1, H2: PMPInteger;
  T, T0: TECPoint;
begin
  Result := (MPMSB(R) > 0) and (MPCmpOffset(R,APubl.Params.R) < 0) and
            (MPMSB(S) > 0) and (MPCmpOffset(S,APubl.Params.R) < 0) and
            (R^.Sign > 0) and (S^.Sign > 0);
  if not Result then Exit;
  H := nil;
  H1 := nil;
  H2 := nil;
  MPBinaryInvMod(S,APubl.Params.R,H);
  MPMulMod2(H,F,APubl.Params.R,H1);
  MPMulMod2(H,R,APubl.Params.R,H2);

  FillChar(T,SizeOf(T),0);
  ECScalarMulW(APubl.Params.G,H1,APubl.Params,T);
  FillChar(T0,SizeOf(T0),0);
  ECScalarMulW(APubl.W,H2,APubl.Params,T0);
  ECFullAddition(T,T0,APubl.Params);
  Result := not ECIsInf(T,APubl.Params);
  if Result then begin
    MPMod(T.X,APubl.Params.R);
    Result := MPCmpOffset(T.X,R) = 0;
  end;

  ECDealloc(T0);
  ECDealloc(T);
  MPDealloc(H2);
  MPDealloc(H1);
  MPDealloc(H);
end;

function ECKAS_DH1(const APriv: TECPrivateKey;
                   const WPr: TECPoint;
                   const P; pLen: Integer;
                   CoFactor: Boolean;
                   HashAlgorithm: THashAlgorithm;
                   KeyDerivation: TKeyDerivation;
                   var K; kLen: Integer;
                   MacTag: PX9_42MacTagParams): Boolean;
var
  V: TECPoint;
  iZ: PMPInteger;
  Z: PChar;
  zLen: Integer;
begin
  Result := ECOnCurve(WPr,APriv.Params);
  if not Result then Exit;
  FillChar(V,SizeOf(V),0);
  iZ := nil;
  if CoFactor then
    Result := ECSVDP_DHC(APriv,WPr,iZ)
  else begin
    Result := True;
    ECSVDP_DH(APriv,WPr,iZ);
  end;
  if Result then begin
    zLen := (MPMSB(iZ) + 7) shr 3;
    GetMem(Z,zLen);
    try
      MPIntToBase256(iZ,Z^,zLen);
      case KeyDerivation of
        kdKDF1:               KDF1(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdKDF2:               KDF2(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdX9_42ASN1DER:       KDFX9_42ASN1DER(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdX9_42Concatenation: KDFX9_42Concatenation(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdNone:               Result := False;
      end;
{$IFDEF SHA1}
      if Assigned(MacTag) then
        X9_42MacTag(Z^,zLen,MacTag);
{$ENDIF SHA1}
    finally
      FreeMem(Z);
    end;
  end;
  MPDealloc(iZ);
end;

function ECKAS_DH2(const APriv0: TECPrivateKey;
                   const APriv1: TECPrivateKey;
                   const WPr: TECPoint;
                   const VPr: TECPoint;
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
  Result := ECOnCurve(WPr,APriv0.Params) and ECOnCurve(vPr,APriv1.Params);
  if not Result then Exit;
  V0 := nil;
  if CoFactor0 then
    Result := ECSVDP_DHC(APriv0,WPr,V0)
  else begin
    Result := True;
    ECSVDP_DH(APriv0,WPr,V0);
  end;
  if not Result then begin
    MPDealloc(V0);
    Exit;
  end;
  V1 := nil;
  if not CoFactor1 then
    ECSVDP_DH(APriv1,VPr,V1)
  else
    Result := ECSVDP_DHC(APriv1,VPr,V1);
  if Result then begin
    z0Len := ((MPMSB(V0) + 7) shr 3);
    z1Len := ((MPMSB(V1) + 7) shr 3);
    GetMem(Z,z0Len + z1Len);
    try
      MPIntToBase256(V0,Z^,z0Len);
      MPIntToBase256(V1,(Z + z0Len)^,z1Len);
      case KeyDerivation of
        kdKDF1:               KDF1(Z^,z0Len + z1Len,P,pLen,HashAlgorithm,K,kLen);
        kdKDF2:               KDF2(Z^,z0Len + z1Len,P,pLen,HashAlgorithm,K,kLen);
        kdX9_42ASN1DER:       KDFX9_42ASN1DER(Z^,z0Len + z1Len,P,pLen,HashAlgorithm,K,kLen);
        kdX9_42Concatenation: KDFX9_42Concatenation(Z^,z0Len + z1Len,P,pLen,HashAlgorithm,K,kLen);
        kdNone:               Result := False;
      end;
{$IFDEF SHA1}
      if Assigned(MacTag) then
        X9_42MacTag(Z^,z0Len + z1Len,MacTag);
{$ENDIF SHA1}
    finally
      FreeMem(Z);
    end;
  end;
  MPDealloc(V0);
  MPDealloc(V1);
end;

function ECKAS_MQV(const APriv0: TECPrivateKey;
                   APriv1: PMPInteger;
                   const APubl1: TECPoint;
                   const WPr: TECPoint;
                   const VPr: TECPoint;
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
  Result := ECOnCurve(WPr,APriv0.Params) and ECOnCurve(vPr,APriv0.Params);
  if not Result then Exit;
  V := nil;
  if CoFactor then
    Result := ECSVDP_MQVC(APriv0.Params,APriv0.S,APriv1,APubl1,WPr,VPr,V)
  else begin
    Result := True;
    ECSVDP_MQV(APriv0.Params,APriv0.S,APriv1,APubl1,WPr,VPr,V);
  end;
  if Result then begin
    zLen := ((MPMSB(V) + 7) shr 3);
    GetMem(Z,zLen);
    try
      MPIntToBase256(V,Z^,zLen);
      case KeyDerivation of
        kdKDF1:               KDF1(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdKDF2:               KDF2(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdX9_42ASN1DER:       KDFX9_42ASN1DER(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdX9_42Concatenation: KDFX9_42Concatenation(Z^,zLen,P,pLen,HashAlgorithm,K,kLen);
        kdNone:               Result := False;
      end;
{$IFDEF SHA1}
      if Assigned(MacTag) then
        X9_42MacTag(Z^,zLen,MacTag);
{$ENDIF SHA1}
    finally
      FreeMem(Z);
    end;
  end;
  MPDealloc(V);
end;

function ECSSASignatureGeneration(const AKey: TECPrivateKey;
                                  const Digest; DigestCount: Integer;
                                  HashAlgorithm: THashAlgorithm;
                                  NullMsg: Boolean;
                                  DEREncode: Boolean;
                                  var Sign; SignCount: Integer): Integer;
var
  qLen, sigLen, sLen, rLen, FBitSize: Cardinal;
  F, R, S: PMPInteger;
  P: PChar;
begin
  qLen := (MPMSB(AKey.Params.R) + 7) shr 3;
  F := nil;
  R := nil;
  S := nil;
  case AKey.SignScheme of
    ecssaDSA: begin
                FBitSize := MPMSB(AKey.Params.R);
                EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,F);
                ECSP_DSA(AKey,F,R,S);
              end;
    ecssaNR:  begin
                FBitSize := MPMSB(AKey.Params.R) - 1;
                EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,F);
                ECSP_NR(AKey,F,R,S);
              end;
  else
    raise Exception.Create('ECSSASignatureGeneration: Unsupported scheme');
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

function ECSSASignatureGeneration(const AKey: TECPrivateKey;
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
    raise Exception.Create('ECSSASignatureGeneration: Unsupported hash algorithm');
  try
    GetMem(D,H.DigestSize);
    try
      H.Done(D);
      try
        Result := ECSSASignatureGeneration(AKey,D^,H.DigestSize,H.Algorithm,MsgCount = 0,DEREncode,Sign,SignCount);
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

function ECSSASignatureVerification(const AKey: TECPublicKey;
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
  qLen := (MPMSB(AKey.Params.R) + 7) shr 3;
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
          ecssaDSA: begin
                      FBitSize := MPMSB(AKey.Params.R);
                      EMSA1(Digest,DigestCount,HashAlgorithm,FBitSize,F);
                      Result := ECVP_DSA(AKey,R,S,F);
                    end;
          ecssaNR:  begin
                      Result := ECVP_NR(AKey,R,S,F);
                      if Result then begin
                        FBitSize := MPMSB(AKey.Params.R) - 1;
                        Result := EMSA1Verification(F,Digest,DigestCount,HashAlgorithm,FBitSize);
                      end;
                    end;
        else
          raise Exception.Create('ECSSASignatureVerification: Unsupported scheme');
        end;
      end;
    finally
      MPDealloc(S);
      MPDealloc(R);
      MPDealloc(F);
    end;
  end;
end;

function ECSSASignatureVerification(const AKey: TECPublicKey;
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
    raise Exception.Create('ECSSASignatureGeneration: Unsupported hash algorithm');
  try
    GetMem(D,H.DigestSize);
    try
      H.Done(D);
      try
        Result := ECSSASignatureVerification(AKey,D^,H.DigestSize,Sign,SignCount,DEREncode,H.Algorithm,MsgCount = 0);
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

procedure ECIESEncryption(const AKey: TECPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          var EphemeralKey: TECPoint;
                          var CipherText; CTCount: Integer;
                          var MAC; MACCount: Integer);
var
  X: PMPInteger;
  eZ: TECPoint;
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
  ECKeys(AKey.Params,X,EphemeralKey);
  FillChar(eZ,SizeOf(eZ),0);
  ECScalarMulW(AKey.W,X,AKey.Params,eZ);
  MPDealloc(X);

  zLen := (MPMSB(AKey.Params.Q) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(eZ.X,Z^,zLen);
    ECDealloc(eZ);
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
        raise Exception.Create('ECIESEncryption: Unsupported hash algorithm');
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

procedure ECIESEncryption(const AKey: TECPublicKey;
                          const Msg; MsgCount: Integer;
                          const P1; P1Len: Integer;
                          const P2; P2Len: Integer;
                          HashAlgorithm: THashAlgorithm;
                          CipherClass: TCipherClass;
                          KeyLen: Integer;
                          var EphemeralKey: TECPoint;
                          var CipherText; var CTCount: Integer;
                          var MAC; MACCount: Integer);
var
  C: TCipher;
  X: PMPInteger;
  eZ: TECPoint;
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
  ECKeys(AKey.Params,X,EphemeralKey);
  FillChar(eZ,SizeOf(eZ),0);
  ECScalarMulW(AKey.W,X,AKey.Params,eZ);
  zLen := (MPMSB(AKey.Params.Q) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(eZ.X,Z^,zLen);
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
        raise Exception.Create('ECIESEncryption: Unsupported hash algorithm');
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
  ECDealloc(eZ);
  MPDealloc(X);
end;

function ECIESDecryption(const AKey: TECPrivateKey;
                         const EphemeralKey: TECPoint;
                         const CipherText; CTCount: Integer;
                         const MAC; MACCount: Integer;
                         const P1; P1Len: Integer;
                         const P2; P2Len: Integer;
                         HashAlgorithm: THashAlgorithm;
                         var Msg; MsgCount: Integer): Boolean; overload;
var
  eZ: TECPoint;
  Z, K, M, C, vMac: PChar;
  bLen, zLen, I: Integer;
  HC: THashClass;
  H: THash;
begin
  Assert(CTCount <= MsgCount);
  Result := ECOnCurve(EphemeralKey,AKey.Params);
  if not Result then Exit;   
{$IFDEF SHA512}
  if HashAlgorithm in [haSHA384,haSHA512] then
    bLen := 128
  else
{$ENDIF SHA512}
    bLen := 64;
  FillChar(eZ,SizeOf(eZ),0);
  ECScalarMulW(EphemeralKey,AKey.S,AKey.Params,eZ);
  Result := not ECIsInf(eZ,AKey.Params);
  if not Result then begin
    ECDealloc(eZ);
    Exit;
  end;

  zLen := (MPMSB(AKey.Params.Q) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(eZ.X,Z^,zLen);
    ECDealloc(eZ);
    GetMem(K,MsgCount + bLen);
    try
      KDF2(Z^,zLen,P1,P1Len,HashAlgorithm,K^,MsgCount + bLen);
      M := @Msg;
      C := @CipherText;
      for I := 0 to MsgCount - 1 do
        M[I] := Char(Byte(C[I]) xor Byte(K[I]));
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(C^,0)
      else
        raise Exception.Create('ECIESEncryption: Unsupported hash algorithm');
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

function ECIESDecryption(const AKey: TECPrivateKey;
                         const EphemeralKey: TECPoint;
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
  eZ: TECPoint;
  Z, K, vMac, MT: PChar;
  bLen, zLen, tLen, I: Integer;
  HC: THashClass;
  H: THash;
begin
  Assert(CTCount <= MsgCount);
  Result := ECOnCurve(EphemeralKey,AKey.Params);
  if not Result then Exit;       
{$IFDEF SHA512}
  if HashAlgorithm in [haSHA384,haSHA512] then
    bLen := 128
  else        
{$ENDIF SHA512}
    bLen := 64;
  FilLChar(eZ,SizeOf(eZ),0);
  ECScalarMulW(EphemeralKey,AKey.S,AKey.Params,eZ);
  Result := not ECIsInf(eZ,AKey.Params);
  if not Result then begin
    ECDealloc(eZ);
    Exit;
  end;

  zLen := (MPMSB(AKey.Params.Q) + 7) shr 3;
  GetMem(Z,zLen);
  try
    UMPIntToBase256(eZ.X,Z^,zLen);
    GetMem(K,KeyLen + bLen);
    try
      KDF2(Z^,zLen,P1,P1Len,HashAlgorithm,K^,KeyLen + bLen);
      HC := FindHashClass(HashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(C,0)
      else
        raise Exception.Create('ECIESEncryption: Unsupported hash algorithm');
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
  ECDealloc(eZ);
end;

{ TMPECPrivateKey }

constructor TMPECPrivateKey.CreateNew(const AAlgorithm,
  ACurve: ObjectIdentifier; AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
  ASignatureDigestAlg: THashAlgorithm; APublKey: IECPublicKeyInfo);
var
  ECPubl: TECPublicKey;
  Res: Boolean;
begin
  Creating := True;
  FillChar(ECPubl,SizeOf(ECPubl),0);
  try
    Res := OIDToCurve(ACurve,ECPubl.Params);
    Assert(Res);
    ECKeys(ECPubl.Params,FKey,ECPubl.W);
    Create(AKeyStruct,AKeyHandler);
    if AAlgorithm = id_ecnr then begin
      FKey.SignScheme := ecssaNR;
      ECPubl.SignScheme := ecssaNR;
    end;
    if Assigned(APublKey) then
      APublKey.ImposeSubjectECPublicKey(ECPubl);
    PublKey := APublKey;
    ECCopy2(ECPubl.Params,FSystemParams);
  finally
    DisposeECPublicKey(ECPubl);
  end;
  SignatureDigestAlg := ASignatureDigestAlg;
  if Assigned(KeyHandler) then
    if not EncodeNewKey then
      raise Exception.Create('TMPECPrivateKey.CreateNew: Could not encode key');
  Creating := False;
end;

constructor TMPECPrivateKey.CreateNewParams(
  const AAlgorithm: ObjectIdentifier; AParams: TECurve;
  AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler;
  APublKey: IECPublicKeyInfo);
var
  ECPubl: TECPublicKey;
begin
  Creating := True;
  FillChar(ECPubl,SizeOf(ECPubl),0);
  try
    ECKeys(AParams,FKey,ECPubl);
    Create(AKeyStruct,AKeyHandler);
    if AAlgorithm = id_ecnr then begin
      FKey.SignScheme := ecssaNR;
      ECPubl.SignScheme := ecssaNR;
    end;
    if Assigned(APublKey) then
      APublKey.ImposeSubjectECPublicKey(ECPubl);
    PublKey := APublKey;
    ECCopy2(AParams,FSystemParams);
  finally
    DisposeECPublicKey(ECPubl);
  end;
  if Assigned(AKeyHandler) then
    if not EncodeNewKey then
      raise Exception.Create('TMPECPrivateKey.CreateNewParams: Could not encode key');
  Creating := False;
end;

function TMPECPrivateKey.DecodeASNKey(K: TASN1Struct): Boolean;
var
  OID: string;
  P: PASN1Struct;
  T: Cardinal;
begin
  P := K.Items[1];
  T := P^.Tag;
  if T = V_ASN1_SEQUENCE then begin
    OID := P^.Items[1]^.Items[0]^.ContentAsOID;
    if OID = prime_field then begin
      Result := True;
      K.Items[0]^.ContentAsUMPInt(FKey.S);
      P^.Items[1]^.Items[1]^.ContentAsUMPInt(FKey.params.Q);
      P^.Items[2]^.Items[0]^.ContentAsUMPInt(FKey.params.A);
      P^.Items[2]^.Items[1]^.ContentAsUMPInt(FKey.params.B);
      P^.Items[4]^.ContentAsUMPInt(FKey.params.R);
      if P^.ItemCount > 5 then
        P^.Items[5]^.ContentAsUMPInt(FKey.Params.K)
      else begin
        MPDealloc(FKey.Params.K);
        FKey.Params.K := IntToMPInt(1);
      end;
      P^.Items[3]^.ContentAsECPoint(FKey.params.G,FKey.Params);
    end else
      Result := False;
  end else if T = V_ASN1_OBJECT then begin
    OID := P^.ContentAsOID;
    Result := OIDToCurve(OID,FKey.Params);
    if Result then
      K.Items[0]^.ContentAsUMPInt(FKey.S);
  end else
    Result := False;
  if Result then begin
    OID := Algorithm;
    if OID = id_ecnr then
      FKey.SignScheme := ecssaNR
    else
      FKey.SignScheme := ecssaDSA;
    ECCopy2(FKey.Params,FSystemParams);
  end;
end;

function TMPECPrivateKey.DecryptKeyAgreement(const PublKey: TECPublicKey;
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

destructor TMPECPrivateKey.Destroy;
begin
  ECDealloc(FSystemParams);
  inherited;
end;

procedure TMPECPrivateKey.DisposeKey;
begin
  inherited;
  DisposeECPrivateKey(FKey);
end;

function TMPECPrivateKey.ECPublicKey(var APublKey: TECPublicKey): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      MPEC.ECPublicKey(FKey,APublKey);
  finally
    UnlockKey;
  end;
end;

function TMPECPrivateKey.EncodeASNKey(K: TASN1Struct): Boolean;
var
  A, P, Q: PASN1Struct;
  OID: string;
begin
  Result := True;
  if K.ItemCount > 0 then begin
    K.EditField('privateKey',FKey.S,True);

    OID := CurveToOID(FKey.Params);
    if OID <> '' then begin
      K.FindField('params')^.SelectChoice(1);
      K.EditField('params',OID);
    end else begin
      K.FindField('params')^.SelectChoice(0);
      K.EditField('/params/version',Integer(1));
      K.EditField('/params/fieldID/fieldType',prime_field);
      K.EditField('/params/fieldID/parameters',FKey.params.Q,True);
      K.EditField('/params/curve/a',FKey.params.A,True);
      K.EditField('/params/curve/b',FKey.params.B,True);
      K.EditField('/params/base',FKey.params.G,FKey.params,cHybrid);
      K.EditField('/params/order',FKey.params.R,True);
      K.EditField('/params/coFactor',FKey.params.K,True);
    end;
  end else begin
    K.Tag := V_ASN1_SEQUENCE;
    K.Constructed := True;

    A := K.AddField('privateKey','OCTET STRING',nil);
    A^.EditContent(FKey.S,True);

    OID := CurveToOID(FKey.Params);
    if OID <> '' then begin
      A := K.AddField('params','OBJECT',nil);
      A.EditContent(OID);
    end else begin
      A := K.AddField('params','SEQUENCE',nil);
      A^.AddField('version','INTEGER','1',True);
      P := A^.AddField('fieldID','SEQUENCE',nil);
      P^.AddField('fieldType','OBJECT',prime_field,True);
      Q := P^.AddField('parameters','INTEGER',nil);
      Q^.EditContent(FKey.params.Q,True);
      P := A^.AddField('curve','SEQUENCE',nil);
      Q := P^.AddField('a','OCTET STRING',nil);
      Q^.EditContent(FKey.params.A,True);
      Q := P^.AddField('b','OCTET STRING',nil);
      Q^.EditContent(FKey.params.B,True);
      P := A^.AddField('base','OCTET STRING',nil);
      P^.EditContent(FKey.params.G,FKey.params,cHybrid);
      P := A^.AddField('order','INTEGER',nil);
      P^.EditContent(FKey.params.R,True);
      P := A^.AddField('coFactor','INTEGER',nil);
      P^.EditContent(FKey.params.K,True);
    end;
  end;
  K.CalculateLength;
end;

function TMPECPrivateKey.GetAlgorithm: ObjectIdentifier;
begin
  if KeyInitialized and (FKey.SignScheme = ecssaNR) then
    Result := id_ecnr
  else
    Result := inherited GetAlgorithm;
end;

function TMPECPrivateKey.GetSignatureLength: Integer;
var
  ECPublIntf: IECPublicKeyInfo;
  ECPubl: TECPublicKey;
  Done: Boolean;

  function MSBToSignSize(Q: PMPInteger): Integer;
  begin
    Result := (MPMSB(Q) + 7) shr 3;
    if DEREncodeSignature then
      Result := Result*2 + 6
    else
      Result := Result*2;
  end;

begin
  if Assigned(FSystemParams.R) then
    Result := MSBToSignSize(FSystemParams.R)
  else begin
    Result := 0;
    Done := False;
    if Assigned(PublKey) then begin
      Done := PublKey.QueryInterface(IECPublicKeyInfo,ECPublIntf) = 0;
      if Done then begin
        FillChar(ECPubl,SizeOf(ECPubl),0);
        try
          ECPublIntf.ExtractSubjectECPublicKey(ECPubl);
          ECCopy2(ECPubl.Params,FSystemParams);
          Result := MSBToSignSize(ECPubl.Params.R);
        finally
          DisposeECPublicKey(ECPubl);
        end;
      end;
    end;
    if not Done then begin
      LockKey;
      UnlockKey;
      if Assigned(FSystemParams.R) then
        Result := MSBToSignSize(FSystemParams.R)
      else
        Result := inherited GetSignatureLength;
    end;
  end;
end;

function TMPECPrivateKey.GetSystemParams: PECurve;
begin
  if FSystemParams.Q = nil then begin
    LockKey;
    UnlockKey;
  end;
  Result := @FSystemParams;
end;

procedure TMPECPrivateKey.InternalDecryptKeyAgreement(
  PublKey: TMPPublicKey; var Secret; SLen: Integer; out Result: Boolean);
var
  Publ: TMPECPublicKey;
begin
  Result := (PublKey is TMPECPublicKey);
  if Result and (Algorithm = id_ecPublicKey) then begin
    Publ := TMPECPublicKey(PublKey);
    Publ.LockKey;
    try
      InternalDecryptKeyAgreementRec(Publ.FKey,Secret,SLen,Result);
    finally
      Publ.UnlockKey;
    end;
  end;
end;

procedure TMPECPrivateKey.InternalDecryptKeyAgreementIntf(
  PublKey: IPKPublicKeyInfo; var Secret; SLen: Integer;
  out Result: Boolean);
var
  ECPublIntf: IECPublicKeyInfo;
  ECPubl: TECPublicKey;
begin
  Result := PublKey.QueryInterface(IECPublicKeyInfo,ECPublIntf) = 0;
  if Result and (Algorithm = id_ecPublicKey) then begin
    FillChar(ECPubl,SizeOf(ECPubl),0);
    try
      ECPublIntf.ExtractSubjectECPublicKey(ECPubl);
      InternalDecryptKeyAgreementRec(ECPubl,Secret,SLen,Result);
    finally
      DisposeECPublicKey(ECPubl);
    end;
  end;
end;

procedure TMPECPrivateKey.InternalDecryptKeyAgreementRec(
  PublKey: TECPublicKey; var Secret; SLen: Integer; out Result: Boolean);
begin
  Result := ECCompare(FKey.Params,PublKey.Params);
  if Result then begin
    P := GetP;
    Result := ECKAS_DH1(FKey,PublKey.W,
                        Pointer(P)^,Length(P),
                        CoFactor,KAHashAlg,KDF,
                        Secret,SLen);
  end;
end;

procedure TMPECPrivateKey.InternalSignBuf(const Msg; MsgLen: Integer;
  var Sign; var SignLen: Integer);
begin
  SignLen := ECSSASignatureGeneration(FKey,
                                      Msg,MsgLen,
                                      SignatureDigestAlg,
                                      DEREncodeSignature,
                                      Sign,SignLen)
end;

procedure TMPECPrivateKey.InternalSignDig(const Digest; DigestLen: Integer;
  var Sign; var SignLen: Integer);
begin
  SignLen := ECSSASignatureGeneration(FKey,
                                      Digest,DigestLen,
                                      SignatureDigestAlg,
                                      False,
                                      DEREncodeSignature,
                                      Sign,SignLen)
end;

procedure TMPECPrivateKey.InternalSignSigned(Signed: IUnknown;   
  CACert: TASN1Struct; var OK: Boolean);
var
  S: ISigned;
begin
  OK := Signed.QueryInterface(ISigned,S) = 0;
  if OK then begin
    if Algorithm = id_ecPublicKey then
      OK := S.ECDSASign(FKey,CACert)
    else
      OK := False;
  end;
end;

procedure TMPECPrivateKey.SetPublKey(const Value: IPKPublicKeyInfo);
var
  Publ: IECPublicKeyInfo;
  ECPubl: TECPublicKey;
  Res: Boolean;
begin
  if Assigned(Value) and (Value.QueryInterface(IECPublicKeyInfo,Publ) = 0) then begin
    FillChar(ECPubl,SizeOf(ECPubl),0);
    try
      if Publ.AlgorithmIdentifier = '' then begin
        LockKey;
        try
          if KeyInitialized then begin
            MPEC.ECPublicKey(FKey,ECPubl);
            Publ.ImposeSubjectECPublicKey(ECPubl);
          end else
            Publ := nil;
        finally
          UnlockKey;
        end;
      end else begin
        Res := Publ.ExtractSubjectECPublicKey(ECPubl);
        if Res and not Creating then begin
          LockKey;
          try
            if not (KeyInitialized and ValidateECPrivateKey(FKey,ECPubl)) then
              Publ := nil;
          finally
            UnlockKey;
          end;
        end else if not Res then
          Publ := nil;
      end;
    finally
      DisposeECPublicKey(ECPubl);
    end;              
    inherited SetPublKey(Publ);
  end else
    inherited SetPublKey(nil);
end;

{ TMPECPublicKey }

constructor TMPECPublicKey.CreateFromRecord(
  const AAlgorithm: ObjectIdentifier; const ECPublKey: TECPublicKey;
  AKeyIntf: IECPublicKeyInfo);
begin
  Create(AKeyIntf);
  ECCopy2(ECPublKey.W,FKey.W);
  ECCopy2(ECPublKey.Params,FKey.Params);
  FKey.KAScheme := ECPublKey.KAScheme;
  FKey.SignScheme := ECPublKey.SignScheme;
  FAlgorithm := AAlgorithm;
  EncodeKey;
end;

function TMPECPublicKey.DecodeKey: Boolean;
var
  PublInfo: IECPublicKeyInfo;
begin
  Result := (KeyIntf = nil) and Assigned(FKey.W.X) and Assigned(FKey.W.Y);
  if not Result then begin
    Result := KeyIntf.QueryInterface(IECPublicKeyInfo,PublInfo) = 0;
    if Result then
      Result := PublInfo.ExtractSubjectECPublicKey(FKey);
  end;
end;

procedure TMPECPublicKey.DisposeKey;
begin
  inherited;
  DisposeECPublicKey(FKey);
end;

procedure TMPECPublicKey.EncodeKey;
var
  PublInfo: IECPublicKeyInfo;
  Res: Integer;
begin
  if Assigned(KeyIntf) then begin
    Res := KeyIntf.QueryInterface(IECPublicKeyInfo,PublInfo);
    Assert(Res = 0);
    PublInfo.ImposeSubjectECPublicKey(FKey);
  end;
end;

function TMPECPublicKey.GetSystemParams: PECurve;
begin
  Result := @FKey.Params;
end;

function TMPECPublicKey.InternalVerBuf(const Msg; MsgLen: Integer;
  const Sign; SignLen: Integer): Boolean;
begin
  Result := ECSSASignatureVerification(FKey,
                                       Msg,MsgLen,
                                       Sign,SignLen,
                                       DEREncodeSignature,
                                       SignatureDigestAlg);
end;

function TMPECPublicKey.InternalVerDig(const Digest; DigestLen: Integer;
  const Sign; SignLen: Integer): Boolean;
begin
  Result := ECSSASignatureVerification(FKey,
                                       Digest,DigestLen,
                                       Sign,SignLen,
                                       DEREncodeSignature,
                                       SignatureDigestAlg,
                                       False);
end;

procedure TMPECPublicKey.SetDEREncodeSignature(const Value: Boolean);
begin
  FDEREncodeSignature := Value;
end;

end.
