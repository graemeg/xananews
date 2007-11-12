{$I ver.inc}
{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPPK Unit                                         }
{     Public and Private Key Base Classes               }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
{  This unit contains two class hierarchies:            }
{                                                       }
{    TKeyHandler                                        }
{      TEncryptedKeyHandler                             }
{        TSignedEncryptedKeyHandler                     }
{  and                                                  }
{    TMPPK                                              }
{      TMPPrivateKey                                    }
{      TMPPublicKey                                     }
{                                                       }
{  The TKeyHandler classes are used for in-memory key   }
{  management and key protection. Instances of these    }
{  classes are typically created and maintained by a    }
{  TStreamSecII component.                              }

unit MpPK;

interface

uses
  SecUtils, SyncObjs, Asn1;

type
  TMPSecretKey = class;
  TMPPrivateKey = class;
  TMPPublicKey = class;

  IMPPK = interface(IKey)
    ['{30F2B8CD-87D6-4D81-9F96-7DC21018E8A5}']
    function GetPKI: string;
    function GetSignatureDigestAlg: THashAlgorithm;
    function GetSignatureLength: Integer;
    procedure SetSignatureDigestAlg(const Value: THashAlgorithm);
    property PublicKeyIdentifier: string read GetPKI;
    property SignatureDigestAlg: THashAlgorithm read GetSignatureDigestAlg
                                                write SetSignatureDigestAlg;
    property SignatureLength: Integer read GetSignatureLength;
  end;

  IPKPublicKeyInfo = interface
    ['{5B39A13A-9EF3-407B-819E-341C426C65BD}']
    function AlgorithmIdentifier: string;
    function PublicKeyIdentifier: string;
  end;

  IMPPrivateKey = interface(IMPPK)
    ['{F040FE0E-35E5-4888-895A-6117C83D3E1D}']
    function Clone: TMPSecretKey;
    function DecryptKeyAgreement(PublKey: TMPPublicKey;
                                 var Secret; SLen: Integer): Boolean; overload;
    function DecryptKeyAgreement(PublKey: IPKPublicKeyInfo;
                                 var Secret; SLen: Integer): Boolean; overload;
    function DecryptKeyTransport(const EMsg; MsgLen: Integer;
                                 var Key; KeyLen: Integer): Boolean;
    function GetPublKey: IPKPublicKeyInfo;
    function GetTLS: Boolean;
    procedure SetPublKey(const Value: IPKPublicKeyInfo);
    procedure SetTLS(const Value: Boolean);
    function SignBuf(const Msg; MsgLen: Integer;
                     var Sign; SignLen: Integer): Integer;
    function SignDig(const Digest; DigestLen: Integer;
                     var Sign; SignLen: Integer;
                     HashAlg: THashAlgorithm = haDefault): Integer;
    property PublKey: IPKPublicKeyInfo read GetPublKey write SetPublKey;
    function SignSigned(Signed: IUnknown; CACert: TASN1Struct = nil): Boolean;
    property TLS: Boolean read GetTLS write SetTLS;
  end;

  IMPPublicKey = interface(IMPPK)
    ['{08CBE561-B45E-490D-8063-B8BDDCA5089D}']
    function EncryptKeyTransport(const Key; KeyLen: Integer;
                                 var EMsg; MsgLen: Integer): Boolean;
    function VerBuf(const Msg; MsgLen: Integer;
                    const Sign; SignLen: Integer): Boolean;
    function VerDig(const Digest; DigestLen: Integer;
                    const Sign; SignLen: Integer): Boolean;
  end;

  IKeyHandler = interface
    ['{87F5C465-8601-456B-BB1D-A7F92D6934C4}']
    function DecodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean;
    function EncodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean;
    procedure InitStruct(F: TASN1Struct);
    procedure Lock(Key: TMPSecretKey);
    procedure Unlock(Key: TMPSecretKey);
  end;

  TKeyHandler = class(TInterfacedObject,IKeyHandler)
  private
    FLock: TCriticalSection;
  protected
    procedure InitStruct(F: TASN1Struct); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function DecodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean; virtual;
    function EncodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean; virtual;
    procedure Lock(Key: TMPSecretKey);
    procedure Unlock(Key: TMPSecretKey); dynamic;
  end;

  TEncryptedKeyHandler = class(TKeyHandler)
  private
    FCipher: TCipher;
  protected
    function DecryptStruct(P: TASN1Struct; var Key: TASN1Struct): Boolean;
    procedure EncryptStruct(P: TASN1Struct; Key: TASN1Struct);
    procedure InitStruct(F: TASN1Struct); override;
  public
    constructor Create(const ALabel, APassword: string);
    constructor CreateIntf(const ALabel: string; APassword: ISecretKey); overload;
    constructor CreateIntf(AKey: ISecretKey); overload;
    destructor Destroy; override;
    function DecodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean; override;
    function EncodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean; override;
    procedure Unlock(Key: TMPSecretKey); override;
  end;

  ISignedEncryptedKeyHandler = interface(IKeyHandler)
    function GetSignKey: IMPPrivateKey;
    function GetVerKey: IMPPublicKey;
    procedure SetSignKey(const Value: IMPPrivateKey);
    procedure SetVerKey(const Value: IMPPublicKey);
    property SignaturePrivateKey: IMPPrivateKey read GetSignKey write SetSignKey;
    property SignaturePublicKey: IMPPublicKey read GetVerKey write SetVerKey;
  end;

  TSignedEncryptedKeyHandler = class(TEncryptedKeyHandler,
                                     ISignedEncryptedKeyHandler)
  private
    FSignKey: IMPPrivateKey;
    FVerKey: IMPPublicKey;
  protected
    function GetSignKey: IMPPrivateKey;
    function GetVerKey: IMPPublicKey;
    procedure InitStruct(F: TASN1Struct); override;
    procedure SetSignKey(const Value: IMPPrivateKey);
    procedure SetVerKey(const Value: IMPPublicKey);
  public
    function DecodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean; override;
    function EncodePrivateKey(F: TASN1Struct;
                              Key: TMPSecretKey): Boolean; override;
    property SignaturePrivateKey: IMPPrivateKey read GetSignKey write SetSignKey;
    property SignaturePublicKey: IMPPublicKey read GetVerKey write SetVerKey;
  end;

  TMPPK = class(TInterfacedObject,IKey)
  private
    FKeyInitialized: Boolean;
    FSignatureDigestAlg: THashAlgorithm;
  protected
    procedure DisposeKey; virtual;
    function GetAlgorithm: ObjectIdentifier; virtual; abstract;
    function GetPKI: string; virtual; abstract;
    function GetSignatureDigestAlg: THashAlgorithm;
    function GetSignatureLength: Integer; virtual;
    function GetSignatureSize: Integer; virtual;
    function Instance: TObject;
    procedure SetSignatureDigestAlg(const Value: THashAlgorithm);
    property KeyInitialized: Boolean read FKeyInitialized;
    property SignatureDigestAlg: THashAlgorithm read GetSignatureDigestAlg
                                                write SetSignatureDigestAlg;
    property SignatureLength: Integer read GetSignatureLength;
    property SignatureSize: Integer read GetSignatureSize;
  public
    destructor Destroy; override;
    property Algorithm: ObjectIdentifier read GetAlgorithm;
    property PublicKeyIdentifier: string read GetPKI;
  end;

  TMPSecretKey = class(TMPPK)
  private
    FKeyStruct: TASN1Struct;
    FKeyHandler: IKeyHandler;
    FAlgorithm: ObjectIdentifier;
    FIdentifier: OctetString;
    FPKI: string;
  protected
    function DecodeASNKey(K: TASN1Struct): Boolean; virtual; abstract;
    function GetAlgorithm: ObjectIdentifier; override;
    function GetPKI: string; override;
    function EncodeASNKey(K: TASN1Struct): Boolean; virtual; abstract;
    function EncodeNewKey: Boolean;
    procedure LockKey;
    procedure SetAlgorithm(const Value: ObjectIdentifier); virtual;
    class function SupportsAssign(AClass: TClass): Boolean; virtual;
    procedure UnlockKey;
    property Identifier: OctetString read FIdentifier write FIdentifier;
    property KeyHandler: IKeyHandler read FKeyHandler;
    property KeyStruct: TASN1Struct read FKeyStruct;
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); virtual;
    destructor Destroy; override;
    function Assign(Source: TMPSecretKey): Boolean;
    function Clone: TMPSecretKey;
  end;

  TMPSecretKeyClass = class of TMPSecretKey;

  TMPPrivateKey = class(TMPSecretKey,IMPPrivateKey)
  private
    FPublKey: IPKPublicKeyInfo;
    FTLS: Boolean;
  protected
    function GetAlgorithm: ObjectIdentifier; override;
    function GetPKI: string; override;
    function GetPublKey: IPKPublicKeyInfo;
    function GetTLS: Boolean;
    procedure InternalDecryptKeyAgreement(PublKey: TMPPublicKey;
                                          var Secret; SLen: Integer;
                                          out Result: Boolean); virtual;
    procedure InternalDecryptKeyAgreementIntf(PublKey: IPKPublicKeyInfo;
                                              var Secret; SLen: Integer;
                                              out Result: Boolean); virtual;
    procedure InternalDecryptKeyTransport(const EMsg; MsgLen: Integer;
                                          var Key; KeyLen: Integer;
                                          out Result: Boolean); virtual;
    procedure InternalSignBuf(const Msg; MsgLen: Integer;
                              var Sign; var SignLen: Integer); virtual; abstract;
    procedure InternalSignDig(const Digest; DigestLen: Integer;
                              var Sign; var SignLen: Integer); virtual; abstract;
    procedure InternalSignSigned(Signed: IUnknown;
                                 CACert: TASN1Struct;
                                 var OK: Boolean); virtual; abstract;
    procedure SetPublKey(const Value: IPKPublicKeyInfo); virtual;
    procedure SetTLS(const Value: Boolean);
    property PublKey: IPKPublicKeyInfo read GetPublKey write SetPublKey;
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); override;
    destructor Destroy; override;
    function DecryptKeyAgreement(PublKey: TMPPublicKey;
                                 var Secret; SLen: Integer): Boolean; overload;
    function DecryptKeyAgreement(PublKey: IPKPublicKeyInfo;
                                 var Secret; SLen: Integer): Boolean; overload;
    function DecryptKeyTransport(const EMsg; MsgLen: Integer;
                                 var Key; KeyLen: Integer): Boolean;
    function SignBuf(const Msg; MsgLen: Integer;
                     var Sign; SignLen: Integer): Integer;
    function SignDig(const Digest; DigestLen: Integer;
                     var Sign; SignLen: Integer;
                     HashAlg: THashAlgorithm): Integer;
    function SignSigned(Signed: IUnknown; CACert: TASN1Struct = nil): Boolean;
    property SignatureDigestAlg;
    property SignatureLength;
    property SignatureSize;
    property TLS: Boolean read GetTLS write SetTLS;
  end;

  TMPPublicKey = class(TMPPK,IMPPublicKey)
  private
    FKeyIntf: IPKPublicKeyInfo;
    FCacheKey: Boolean;
    procedure SetCacheKey(const Value: Boolean);
  protected
    function DecodeKey: Boolean; virtual; abstract;
    procedure EncodeKey; virtual; abstract;
    function GetAlgorithm: ObjectIdentifier; override;
    function GetPKI: string; override;
    procedure InternalEncryptKeyTransport(const Key; KeyLen: Integer;
                                          var EMsg; MsgLen: Integer;
                                          out Result: Boolean); virtual;
    function InternalVerBuf(const Msg; MsgLen: Integer;
                            const Sign; SignLen: Integer): Boolean; virtual; abstract;
    function InternalVerDig(const Digest; DigestLen: Integer;
                            const Sign; SignLen: Integer): Boolean; virtual; abstract;
    procedure LockKey;
    procedure UnlockKey;
    property KeyIntf: IPKPublicKeyInfo read FKeyIntf;
  public
    constructor Create(AKeyIntf: IPKPublicKeyInfo);
    destructor Destroy; override;
    function EncryptKeyTransport(const Key; KeyLen: Integer;
                                 var EMsg; MsgLen: Integer): Boolean;
    function VerBuf(const Msg; MsgLen: Integer;
                    const Sign; SignLen: Integer): Boolean;
    function VerDig(const Digest; DigestLen: Integer;
                    const Sign; SignLen: Integer): Boolean;
    property CacheKey: Boolean read FCacheKey write SetCacheKey;
    property SignatureDigestAlg;
    property SignatureLength;
  end;

function CreateMPPublicKey(APubl: IPKPublicKeyInfo): TMPPublicKey;

implementation

uses
  SysUtils, Pkix, SsRijndael, MpIF, MpDL, MpEC;

function CreateMPPublicKey(APubl: IPKPublicKeyInfo): TMPPublicKey;
var
  OID: ObjectIdentifier;
begin
  OID := APubl.AlgorithmIdentifier;
  if (OID = rsaEncryption) or
     (OID = id_rsaes_oaep) or
     (OID = id_rsassa_pss) or
     (OID = id_rw) then
    Result := TMPIFPublicKey.Create(APubl)
  else if (OID = id_dsa) or
          (OID = id_nr) then
    Result := TMPDLPublicKey.Create(APubl)
  else if (OID = id_ecPublicKey) or
          (OID = id_ecnr) then
    Result := TMPECPublicKey.Create(APubl)
  else
    Result := nil;
end;

{ TMPPK }

destructor TMPPK.Destroy;
begin
  Assert(RefCount = 0,'TMPPK.Destroy: RefCount = ' + IntToStr(RefCount));
  DisposeKey;
  inherited;
end;

procedure TMPPK.DisposeKey;
begin
  FKeyInitialized := False;
end;

function TMPPK.GetSignatureDigestAlg: THashAlgorithm;
begin
  Result := FSignatureDigestAlg;
end;

function TMPPK.GetSignatureLength: Integer;
begin
  Result := 0;
end;

function TMPPK.GetSignatureSize: Integer;
begin
  Result := 0;
end;

function TMPPK.Instance: TObject;
begin
  Result := Self;
end;

procedure TMPPK.SetSignatureDigestAlg(const Value: THashAlgorithm);
begin
  FSignatureDigestAlg := Value;
end;

{ TMPPublicKey }

constructor TMPPublicKey.Create(AKeyIntf: IPKPublicKeyInfo);
begin
  FKeyIntf := AKeyIntf;
end;

destructor TMPPublicKey.Destroy;
begin
  FKeyIntf := nil;
  inherited;
end;

function TMPPublicKey.EncryptKeyTransport(const Key; KeyLen: Integer;
  var EMsg; MsgLen: Integer): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then
      InternalEncryptKeyTransport(Key,KeyLen,EMsg,MsgLen,Result)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

function TMPPublicKey.GetAlgorithm: ObjectIdentifier;
begin
  if Assigned(FKeyIntf) then
    Result := FKeyIntf.AlgorithmIdentifier
  else
    Result := inherited GetAlgorithm;
end;

function TMPPublicKey.GetPKI: string;
begin
  if Assigned(FKeyIntf) then
    Result := FKeyIntf.PublicKeyIdentifier
  else
    Result := inherited GetPKI;
end;

procedure TMPPublicKey.InternalEncryptKeyTransport(const Key;
  KeyLen: Integer; var EMsg; MsgLen: Integer; out Result: Boolean);
begin
  Result := False; // Default result = not supported
end;

procedure TMPPublicKey.LockKey;
begin
  if not FKeyInitialized then
    FKeyInitialized := DecodeKey
end;

procedure TMPPublicKey.SetCacheKey(const Value: Boolean);
begin
  FCacheKey := Value;
end;

procedure TMPPublicKey.UnlockKey;
begin
  if not FCacheKey then
    DisposeKey;
end;

function TMPPublicKey.VerBuf(const Msg; MsgLen: Integer; const Sign;
  SignLen: Integer): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then
      Result := InternalVerBuf(Msg,MsgLen,Sign,SignLen)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

function TMPPublicKey.VerDig(const Digest; DigestLen: Integer; const Sign;
  SignLen: Integer): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then
      Result := InternalVerDig(Digest,DigestLen,Sign,SignLen)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

{ TMPPrivateKey }

constructor TMPPrivateKey.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
begin
  FKeyHandler := AKeyHandler;
  if Assigned(AKeyStruct) then begin
    Assert(Assigned(FKeyHandler));
    FKeyStruct := AKeyStruct;
  end else begin
    FKeyStruct := TASN1Struct.Create;
    if FKeyHandler = nil then
      FKeyHandler := TKeyHandler.Create;
    FKeyHandler.InitStruct(FKeyStruct);
  end;
  FKeyStruct._AddRef;
end;

function TMPPrivateKey.DecryptKeyAgreement(PublKey: IPKPublicKeyInfo;
  var Secret; SLen: Integer): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then
      InternalDecryptKeyAgreementIntf(PublKey,Secret,SLen,Result)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

function TMPPrivateKey.DecryptKeyAgreement(PublKey: TMPPublicKey;
  var Secret; SLen: Integer): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then
      InternalDecryptKeyAgreement(PublKey,Secret,SLen,Result)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

function TMPPrivateKey.DecryptKeyTransport(const EMsg; MsgLen: Integer;
  var Key; KeyLen: Integer): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then
      InternalDecryptKeyTransport(EMsg,MsgLen,Key,KeyLen,Result)
    else
      Result := False;
  finally
    UnlockKey;
  end;
end;

destructor TMPPrivateKey.Destroy;
begin
  FPublKey := nil;
  inherited Destroy;
end;

function TMPPrivateKey.GetAlgorithm: ObjectIdentifier;
begin
  if (FAlgorithm = '') and Assigned(FPublKey) then
    FAlgorithm := FPublKey.AlgorithmIdentifier;
  Result := FAlgorithm;
end;

function TMPPrivateKey.GetPKI: string;
begin
  if (FPKI = '') and Assigned(FPublKey) then
    FPKI := FPublKey.PublicKeyIdentifier;
  Result := FPKI;
end;

function TMPPrivateKey.GetPublKey: IPKPublicKeyInfo;
begin
  Result := FPublKey;
end;

function TMPPrivateKey.GetTLS: Boolean;
begin
  Result := FTLS;
end;

procedure TMPPrivateKey.InternalDecryptKeyAgreement(PublKey: TMPPublicKey;
  var Secret; SLen: Integer; out Result: Boolean);
begin
  Result := False; // Default result = not supported
end;

procedure TMPPrivateKey.InternalDecryptKeyAgreementIntf(
  PublKey: IPKPublicKeyInfo; var Secret; SLen: Integer;
  out Result: Boolean);
begin
  Result := False; // Default result = not supported
end;

procedure TMPPrivateKey.InternalDecryptKeyTransport(const EMsg;
  MsgLen: Integer; var Key; KeyLen: Integer; out Result: Boolean);
begin
  Result := False; // Default result = not supported
end;

procedure TMPPrivateKey.SetPublKey(const Value: IPKPublicKeyInfo);
begin
  FPublKey := Value;
end;

procedure TMPPrivateKey.SetTLS(const Value: Boolean);
begin
  FTLS := Value;
end;

function TMPPrivateKey.SignBuf(const Msg; MsgLen: Integer; var Sign;
  SignLen: Integer): Integer;
begin
  LockKey;
  try
    if FKeyInitialized then begin
      Result := SignLen;
      InternalSignBuf(Msg,MsgLen,Sign,Result);
    end else
      Result := -1;
  finally
    UnlockKey;
  end;
end;

function TMPPrivateKey.SignDig(const Digest; DigestLen: Integer; var Sign;
  SignLen: Integer; HashAlg: THashAlgorithm): Integer;
var
  IntSignAlg: THashAlgorithm;
begin
  LockKey;
  try
    if FKeyInitialized then begin
      IntSignAlg := SignatureDigestAlg;
      try
        if HashAlg <> haDefault then
          SignatureDigestAlg := HashAlg;
        Result := SignLen;
        InternalSignDig(Digest,DigestLen,Sign,Result);
      finally
        SignatureDigestAlg := IntSignAlg;
      end;
    end else
      Result := -1;
  finally
    UnlockKey;
  end;
end;

function TMPPrivateKey.SignSigned(Signed: IUnknown;
  CACert: TASN1Struct): Boolean;
begin
  LockKey;
  try
    if FKeyInitialized then begin
      Result := True;
      InternalSignSigned(Signed,CACert,Result);
    end else
      Result := False;
  finally
    UnlockKey;
  end;
end;

{ TKeyHandler }

constructor TKeyHandler.Create;
begin
  FLock := TCriticalSection.Create;
end;

function TKeyHandler.DecodePrivateKey(F: TASN1Struct;
  Key: TMPSecretKey): Boolean;
var
  A, K: PASN1Struct;
begin
  A := F.FindField('/privateKey//privateKey');
  if A = nil then begin
    A := F.FindField('/privateKey/');
    Result := Assigned(A);
    if Result then begin
      Key.FAlgorithm := F.FindField('privateKeyType')^.ContentAsOID;
      K := F.FindField('identifier');
      Result := Assigned(K);
      if Result then begin
        Key.FPKI := K^.ContentAsOctetString;
        Key.FIdentifier := Key.FPKI;
        Result := Key.DecodeASNKey(A^);
      end;
    end;
  end else begin
    Key.FAlgorithm := F.FindField('privateKeyType')^.ContentAsOID;
    K := F.FindField('/privateKey//publicKeyIdentifier');
    Result := Assigned(K);
    if Result then begin
      Key.FPKI := K^.ContentAsOctetString;
      K := F.FindField('identifier');
      if Assigned(K) then
        Key.FIdentifier := K^.ContentAsOctetString;
      K := F.FindField('/privateKey//signatureDigestAlg');
      if Assigned(K) and (K^.Length > 0) then
         OIDToHashAlgorithm(K^.ContentAsOID,Key.FSignatureDigestAlg);
      Result := Key.DecodeASNKey(A^);
    end;
  end;
end;

destructor TKeyHandler.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TKeyHandler.EncodePrivateKey(F: TASN1Struct; Key: TMPSecretKey): Boolean;
var
  A, K: PASN1Struct;
  PKI, Iden: string;
begin
  PKI := Key.PublicKeyIdentifier;
  if PKI <> '' then begin
    K := F.FindField('/privateKey//publicKeyIdentifier');
    if K = nil then
      K := F.FindField('identifier');
    K^.SetContent(Pointer(PKI)^,Length(PKI));
  end;
  F.EditField('/privateKey//signatureDigestAlg',
              HashAlgorithmToOID(Key.FSignatureDigestAlg));
  Iden := Key.Identifier;
  if Iden <> '' then begin
    K := F.FindField('identifier');
    K^.SetContent(Pointer(Iden)^,Length(Iden));
  end;
  if Key.Algorithm <> '' then
    F.EditField('privateKeyType',Key.Algorithm);
  A := F.FindField('/privateKey//privateKey');
  if A = nil then
    A := F.FindField('/privateKey/');
  Result := Assigned(A);
  if Result then
    Result := Key.EncodeASNKey(A^);
  F.CalculateLength;
end;

procedure TKeyHandler.InitStruct(F: TASN1Struct);
var
  A: PASN1Struct;
begin
  F.Tag := V_ASN1_SEQUENCE;
  F.Constructed := True;
  F.AddField('privateKeyType','OBJECT',nil);
  F.AddField('identifier','OCTET STRING OPTIONAL',nil);
  F.AddField('privateKeyAlg','OBJECT OPTIONAL',nil);
  A := F.AddField('privateKey','[0] SEQUENCE OPTIONAL',nil);
  A.AddField('publicKeyIdentifier','OCTET STRING',nil);
  A.AddField('privateKey','SEQUENCE',nil);
end;

procedure TKeyHandler.Lock(Key: TMPSecretKey);
begin
  FLock.Acquire;
  if not Key.FKeyInitialized then
    Key.FKeyInitialized := DecodePrivateKey(Key.FKeyStruct,Key);
end;

procedure TKeyHandler.Unlock(Key: TMPSecretKey);
begin
  FLock.Release;
end;

{ TEncryptedKeyHandler }

constructor TEncryptedKeyHandler.Create(const ALabel, APassword: string);
var
  Key: packed array [0..31] of Byte;
begin
  try
{$IFDEF SHA256}
    WeakPasswordToPrivKey256(APassword,ALabel,$10000,Key,SizeOf(Key));
{$ELSE  SHA256}
    raise Exception.Create('TEncryptedKeyHandler.Create: Action requires SHA-256');
{$ENDIF SHA256}
    FCipher := TAES_Wrap.Create(Key,SizeOf(Key),0);
  finally
    ProtectClear(Key,SizeOf(Key));
  end;
  inherited Create;
end;

constructor TEncryptedKeyHandler.CreateIntf(const ALabel: string;
  APassword: ISecretKey);
var
  Key: ISecretKey;
begin
{$IFDEF SHA256}
  WeakPasswordToPrivKey256(APassword.Key^,APassword.KeyLen,
                           Pointer(ALabel)^,Length(ALabel),
                           $10000,
                           32,Key);
{$ELSE  SHA256}
    raise Exception.Create('TEncryptedKeyHandler.CreateIntf: Action requires SHA-256');
{$ENDIF SHA256}
  CreateIntf(Key);
end;

constructor TEncryptedKeyHandler.CreateIntf(AKey: ISecretKey);
begin
  FCipher := TAES_Wrap.CreateIntf(AKey);
  inherited Create;
end;

function TEncryptedKeyHandler.DecodePrivateKey(F: TASN1Struct;
  Key: TMPSecretKey): Boolean;
var
  A: PASN1Struct;
  K: TASN1Struct;
begin
  A := F.FindField('encryptedKey');
  Result := Assigned(A);
  if Result then begin
    Key.FAlgorithm := F.FindField('privateKeyType')^.ContentAsOID;
    Key.FPKI := F.FindField('identifier')^.ContentAsOctetString;
    K := nil;
    try
      Result := DecryptStruct(A^,K);
      if Result then begin
        Result := Key.DecodeASNKey(K);
        if Result then begin
          A := F.FindField('signatureDigestAlg');
          if Assigned(A) and (A^.Length > 0) then
            OIDToHashAlgorithm(A^.ContentAsOID,Key.FSignatureDigestAlg);
        end;
      end;
    finally
      K.Free;
    end;
  end;
end;

function TEncryptedKeyHandler.DecryptStruct(P: TASN1Struct;
  var Key: TASN1Struct): Boolean;
var
  G: TASN1Struct;
begin
  Result := Assigned(P) and (P.Length > 8);
  if Result then begin
    G := TASN1Struct.Create;
    try
      G.Tag := V_ASN1_OCTET_STRING;
      G.Length := P.Length - 8;
      Move(P.Content[8],G.Content^,G.Length);
      FCipher.IVector := Copy(P.ContentAsOctetString,1,8);
      FCipher.Decrypt(G.Content^,G.Length);
      Result := FCipher.IVector = #$A6#$A6#$A6#$A6#$A6#$A6#$A6#$A6;
      if Result then
        G.ContentAsASN1Struct(Key);
    finally
      G.Free;
    end;
  end;
end;

destructor TEncryptedKeyHandler.Destroy;
begin
  FCipher.Free;
  inherited;
end;

function TEncryptedKeyHandler.EncodePrivateKey(F: TASN1Struct;
  Key: TMPSecretKey): Boolean;
var
  G: TASN1Struct;
  A: PASN1Struct;
  PKI: string;
begin
  G := TASN1Struct.Create;
  try
    G.Tag := V_ASN1_SEQUENCE;
    G.Constructed := True;
    Result := Key.EncodeASNKey(G);
    if Result then begin
      G.CalculateLength;
      A := F.FindField('encryptedKey');
      EncryptStruct(A^,G);

      if Key.Algorithm <> '' then
        F.EditField('privateKeyType',Key.Algorithm);
      F.EditField('signatureDigestAlg',HashAlgorithmToOID(Key.FSignatureDigestAlg));
      PKI := Key.PublicKeyIdentifier;
      if PKI <> '' then begin
        A := F.FindField('identifier');
        A.SetContent(Pointer(PKI)^,Length(PKI));
      end;
      F.CalculateLength
    end;
  finally
    G.Free;
  end;
end;

procedure TEncryptedKeyHandler.EncryptStruct(P: TASN1Struct;
  Key: TASN1Struct);
var
  G: TASN1Struct;
begin
  G := TASN1Struct.Create;
  try
    G.Tag := V_ASN1_OCTET_STRING;
    G.Constructed := False;
    G.EditContent(Key,8);
    FCipher.IVector := #$A6#$A6#$A6#$A6#$A6#$A6#$A6#$A6;
    FCipher.Encrypt(G.Content^,G.Length);
    P.Length := G.Length + 8;
    Move(G.Content^,P.Content[8],G.Length);
    FCipher.GetVectorBuf(P.Content^,8);
  finally
    G.Free;
  end;
end;

procedure TEncryptedKeyHandler.InitStruct(F: TASN1Struct);
begin
  F.Persistent := True;
  F.Tag := V_ASN1_SEQUENCE;
  F.Constructed := True;
  F.AddField('privateKeyType','OBJECT',nil);
  F.AddField('identifier','OCTET STRING OPTIONAL',nil);
  F.AddField('privateKeyAlg','OBJECT OPTIONAL',nil);
  F.AddField('encryptedKey','OCTET STRING',nil);
end;

procedure TEncryptedKeyHandler.Unlock(Key: TMPSecretKey);
begin
  Key.DisposeKey;
  inherited;
end;

{ TSignedEncryptedKeyHandler }

function TSignedEncryptedKeyHandler.DecodePrivateKey(F: TASN1Struct;
  Key: TMPSecretKey): Boolean;
var
  A, S: PASN1Struct;
  M: string;
begin
  A := F.FindField('privateKey');
  M := A^.ContentAsOctetString;
  S := F.FindField('signature');
  Result := FVerKey.VerBuf(Pointer(M)^,Length(M),S^.Content^,S^.Length);
  if Result then
    Result := inherited DecodePrivateKey(A^,Key);
end;

function TSignedEncryptedKeyHandler.EncodePrivateKey(F: TASN1Struct;
  Key: TMPSecretKey): Boolean;
var
  A, S: PASN1Struct;
  M: string;
begin
  Result := Assigned(FSignKey);
  if Result then begin
    A := F.FindField('privateKey');
    Result := inherited EncodePrivateKey(A^,Key);
    if Result then begin
      M := A^.ContentAsOctetString;
      S := F.FindField('signature');
      S^.Length := FSignKey.SignatureLength;
      FSignKey.SignBuf(Pointer(M)^,Length(M),S^.Content^,S^.Length);
    end;
  end;
end;

function TSignedEncryptedKeyHandler.GetSignKey: IMPPrivateKey;
begin
  Result := FSignKey;
end;

function TSignedEncryptedKeyHandler.GetVerKey: IMPPublicKey;
begin
  Result := FVerKey;
end;

procedure TSignedEncryptedKeyHandler.InitStruct(F: TASN1Struct);
var
  A: PASN1Struct;
begin
  F.Tag := V_ASN1_SEQUENCE;
  F.Constructed := True;
  A := F.AddField('privateKey','EncryptedKeyInfo',nil);
  inherited InitStruct(A^);
  F.AddField('signature','OCTET STRING',nil);
end;

procedure TSignedEncryptedKeyHandler.SetSignKey(
  const Value: IMPPrivateKey);
begin
  FSignKey := Value;
end;

procedure TSignedEncryptedKeyHandler.SetVerKey(const Value: IMPPublicKey);
begin
  FVerKey := Value;
end;

{ TMPSecretKey }

function TMPSecretKey.Assign(Source: TMPSecretKey): Boolean;
begin
  Result := Assigned(KeyHandler) and SupportsAssign(Source.ClassType);
  if Result then begin
    if Source.KeyHandler = KeyHandler then begin
      FKeyInitialized := True;
      try
        LockKey;
        try
          Result := FKeyHandler.EncodePrivateKey(KeyStruct,Source);
        finally
          UnlockKey;
        end;
      finally
        FKeyInitialized := False;
      end;
    end else begin
      Source.LockKey;
      try
        FKeyInitialized := True;
        try
          LockKey;
          try
            Result := FKeyHandler.EncodePrivateKey(KeyStruct,Source);
            KeyStruct.CalculateLength;
          finally
            UnlockKey;
          end;
        finally
          FKeyInitialized := False;
        end;
      finally
        Source.UnlockKey;
      end;
    end;
  end;
end;

function TMPSecretKey.Clone: TMPSecretKey;
begin
  Result := TMPSecretKeyClass(ClassType).Create(nil,nil);
  Result.Assign(Self);
end;

constructor TMPSecretKey.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
begin
  FKeyHandler := AKeyHandler;
  if Assigned(AKeyStruct) then begin
    Assert(Assigned(FKeyHandler));
    FKeyStruct := AKeyStruct;
  end else begin
    FKeyStruct := TASN1Struct.Create;
    if FKeyHandler = nil then
      FKeyHandler := TKeyHandler.Create;
    FKeyHandler.InitStruct(FKeyStruct);
  end;
  FKeyStruct._AddRef;
end;

destructor TMPSecretKey.Destroy;
var
  OK: Boolean;
begin
  if Assigned(FKeyStruct) then begin
    OK := FKeyStruct.Owner = nil;
    if FKeyStruct._Release > 0 then
      OK := True;
    Assert(OK);
  end;
  FKeyHandler := nil;
  inherited;
end;

function TMPSecretKey.EncodeNewKey: Boolean;
begin
  FKeyInitialized := True;
  LockKey;
  try
    Result := FKeyHandler.EncodePrivateKey(FKeyStruct,Self);
  finally
    UnlockKey;
  end;
end;

function TMPSecretKey.GetAlgorithm: ObjectIdentifier;
begin
  Result := FAlgorithm;
end;

function TMPSecretKey.GetPKI: string;
begin
  Result := FPKI;
end;

procedure TMPSecretKey.LockKey;
begin
  FKeyHandler.Lock(Self);
end;

procedure TMPSecretKey.SetAlgorithm(const Value: ObjectIdentifier);
begin
  FAlgorithm := Value;
end;

class function TMPSecretKey.SupportsAssign(AClass: TClass): Boolean;
begin
  Result := AClass = Self;
end;

procedure TMPSecretKey.UnlockKey;
begin
  FKeyHandler.Unlock(Self);
end;

end.
