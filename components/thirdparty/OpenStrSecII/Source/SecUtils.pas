{$Q-,R-}
{$I ver.inc}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
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

unit SecUtils;

interface

uses
  Classes;

// ASN.1 DER encoded digest info prefixes, including data type and length of the digest
const
  DERPrefixSHA1: array [0..14] of Byte =
    ($30,$21,$30,$09,$06,$05,$2b,$0e,$03,$02,$1a,$05,$00,$04,$14);
  DERPrefixRipeMD160: array [0..14] of Byte =
    ($30,$21,$30,$09,$06,$05,$2b,$24,$03,$02,$01,$05,$00,$04,$14);
  DERPrefixMD2: array [0..17] of Byte =
    ($30,$20,$30,$0c,$06,$08,$2a,$86,$48,$86,$f7,$0d,$02,$02,$05,$00,$04,$10);
  DERPrefixMD5: array [0..17] of Byte =
    ($30,$20,$30,$0c,$06,$08,$2a,$86,$48,$86,$f7,$0d,$02,$05,$05,$00,$04,$10);
  DERPrefixSHA256: array [0..18] of Byte =
    ($30,$31,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$01,$05,$00,$04,$20);
  DERPrefixSHA384: array [0..18] of Byte =
    ($30,$41,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$02,$05,$00,$04,$30);
  DERPrefixSHA512: array [0..18] of Byte =
    ($30,$51,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$03,$05,$00,$04,$40);

resourcestring
  S_ERR_BUF_TOO_SMALL = 'Procedure HexToBin: Buffer is too small.';
  S_ERR_CREATE_CLONE  = 'CreateClone not implemented for class %s with source %s';

type
  TCipherAlg = (caUnknown,caRijndael,caDES,caTwoFish,caARC4,caBlowFish,caSteak,
                caARC2,caHelix);

  TCipherMode = (cmECB,cmCFB,cmCBC,cmOFB,cmPCFB,cmPipedPCFB,cmABC,cmCTR,
                 cmAESWrap);

  ObjectIdentifier = AnsiString;
  OctetString = AnsiString;

  IKey = interface(IUnknown)
  ['{8F803463-C3F2-46B7-B729-D1592FDA562A}']
    function GetAlgorithm: ObjectIdentifier;
    function Instance: TObject;
    property Algorithm: ObjectIdentifier read GetAlgorithm;
  end;

  PByteArray = ^TByteArray;
  TByteArray = packed array [0..MaxInt-1] of Byte;

  ISecretKey = interface(IKey)
  ['{E1219A5A-3E76-4BA0-8B52-7D8396DF735F}']
    procedure AddTruncKeyAt(AKey: ISecretKey; APos: Integer; IncVal: Byte = 0);
    function GetKey: Pointer;
    function GetKeyBytes: PByteArray;
    function GetKeyLen: Integer;
    function GetVectorSize: Integer;
    procedure SetKey(AKey: Pointer; AKeyLen: Integer; AVectorSize: Integer);
    procedure SetKeyAt(AKey: ISecretKey; APos: Integer);     
    procedure SetKeyStr(const AKeyStr: string; ConvertToBMPStr: Boolean = False);
    procedure SetKeyStrAt(const AKeyStr: string; APos: Integer);
    procedure SetLength(AKeyLen: Integer);
    procedure SetTruncKeyAt(AKey: ISecretKey; APos: Integer);
    procedure SetVectorSize(const Value: Integer);
    procedure XORKeyAt(AKey: ISecretKey; APos: Integer);
    property Key: Pointer read GetKey;
    property KeyBytes: PByteArray read GetKeyBytes;
    property KeyLen: Integer read GetKeyLen;
    property VectorSize: Integer read GetVectorSize write SetVectorSize;
  end;

  TSecretKey = class(TInterfacedObject,ISecretKey)
  private
    FAlgorithm: ObjectIdentifier;
    FKey: Pointer;
    FKeyLen: Integer;
    FVectorSize: Integer;
  protected
    procedure ClearKey;
    function GetAlgorithm: ObjectIdentifier;
    function GetKey: Pointer;
    function GetKeyBytes: PByteArray;
    function GetKeyLen: Integer;
    function GetVectorSize: Integer;      
    function Instance: TObject;        
    procedure SetVectorSize(const Value: Integer);
  public
    constructor Create(AAlgorithm: ObjectIdentifier);
    constructor CreateStr(AValue: PAnsiChar; Len: Integer = -1);
    constructor CreateBMPStr(AValue: PWideChar; Len: Integer = -1);
    destructor Destroy; override;             
    procedure AddTruncKeyAt(AKey: ISecretKey; APos: Integer; IncVal: Byte = 0);
    procedure SetKey(AKey: Pointer; AKeyLen: Integer; AVectorSize: Integer);
    procedure SetKeyAt(AKey: ISecretKey; APos: Integer);
    procedure SetTruncKeyAt(AKey: ISecretKey; APos: Integer);
    procedure SetKeyStr(const AKeyStr: string; ConvertToBMPStr: Boolean);
    procedure SetKeyStrAt(const AKeyStr: string; APos: Integer);
    procedure SetLength(AKeyLen: Integer);                
    procedure XORKeyAt(AKey: ISecretKey; APos: Integer);
    property Algorithm: ObjectIdentifier read GetAlgorithm;
    property Key: Pointer read GetKey;             
    property KeyBytes: PByteArray read GetKeyBytes;
    property KeyLen: Integer read GetKeyLen;
    property VectorSize: Integer read GetVectorSize write SetVectorSize;
  end;
{
  ECB:  ci := E(pi)
        pi := D(ci)
        each pi, ci is one full block.

  CFB:  ci := pi xor E(c(i-n)|...|c(i-1))
        pi := ci xor E(c(i-n)|...|c(i-1))
        each pi, ci is one byte.

  CBC:  ci := E(pi xor c(i-1))
        pi := D(ci) xor c(i-1)
        each ci, pi is one full block,
        if the last block m is incomplete, then
        cm := pm xor E(c(m-1))
        pm := cm xor E(c(m-1))

  OFB:  hi := E(h(i-1))
        ci := pi xor hi
        pi := ci xor hi

  PCFB: vi := E(v(i-1)/256 + c(i-1)*2^(8*(n-1)))
        ci := pi xor (vi mod 256)
        pi := ci xor (vi mod 256)
        each vi is one full block, each ci, pi is one byte.

  PipedPCFB:
        vi := E(c(i-1) + v(i-1))
        ci := pi xor E(v(i-1) + vi)
        pi := ci xor E(v(i-1) + vi)
        each vi, ci, pi is one full block,
        if the last block m is incomplete, then
        vm := E(c(m-1) + v(m-1))
        cm := pm xor E(v(m-1) + vm)
        pm := cm xor E(v(m-1) + vm)

  ABC:  hi := pi xor h(i-1), ci := E(c(i-1) xor hi) xor h(i-1)
        hi := D(ci xor h(i-1)) xor c(i-1), pi := hi xor h(i-1)
        each hi, pi, ci is one full block,
        if the last block m is incomplete, then
        hm := E(c(i-1) xor h(i-1))
        cm := pm xor hm
        pm := cm xor hm

  CTR:  ctri := ctri + 1
        ci := pi xor E(ctri)
        pi := ci xor E(ctri)
        each ctri, pi, ci is one full block.
}

  {TCipher is the base class for all ciphers}
  TCipher = class
  protected
    procedure CleanUp; virtual; abstract;
    function GetVector: OctetString; virtual; abstract;
    procedure SetVector(const Value: OctetString); virtual; abstract;
    procedure Nag(var Count: Integer);
    procedure VirtualLock; virtual;
    procedure VirtualUnlock; virtual;
  public
    constructor CreateClone(ACipher: TCipher); virtual;
    constructor CreateIntf(ASecretKey: ISecretKey); virtual;
    constructor Create(const AKey; Count, VectorSize: Integer); virtual;
    destructor Destroy; override;
    class function BlockSize: Integer; virtual;
    class function Mode: TCipherMode; virtual;
    class function Algorithm: TCipherAlg; virtual;
    class function AlgorithmName: PChar; virtual;
    class function OID(KeySize: Integer): PChar; virtual;
    class function KeyedIVOID(KeySize: Integer): PChar; virtual;
    function Clone: TCipher;
    procedure Decrypt(var Buf; Count: Integer); virtual;
    procedure DecryptIntf(Buf: ISecretKey);
    procedure DecryptPtr(Buf: Pointer; Count: Integer);
    function DecryptStr(const Buf: OctetString): string;
    procedure DecryptToBuf(const Src; var Dst; Count: Integer); virtual;
    procedure DecryptToPtr(Src, Dst: Pointer; Count: Integer);
    procedure Encrypt(var Buf; Count: Integer); virtual;
    procedure EncryptIntf(Buf: ISecretKey);
    procedure EncryptPtr(Buf: Pointer; Count: Integer);
    function EncryptStr(const Buf: string): OctetString;
    procedure EncryptToBuf(const Src; var Dst; Count: Integer); virtual;
    procedure EncryptToPtr(Src, Dst: Pointer; Count: Integer);
    procedure GetVectorBuf(var IV; Count: Integer); virtual;
    procedure SetUp(const AKey; Count, VectorSize: Integer); virtual; abstract;
    procedure SetUpIntf(ASecretKey: ISecretKey);
    procedure SetVectorBuf(const IV; Count: Integer); virtual;
    property IVector: OctetString read GetVector write SetVector;
  end;

  TCipherClass = class of TCipher;

  TCipherMAC = class(TCipher)
  protected
    function GetMac: OctetString; virtual; abstract;
  public
    class function MacLen: Integer; virtual;
    procedure EncryptToNil(const Buf; Count: Integer); virtual;
    procedure GetMacBuf(var Mac; Count: Integer); virtual;
    property Mac: OctetString read GetMac;
  end;

  {The TBlockCipher class introduces a TestSecureSetUp method for secure setup
   when the key material is an unprocessed passphrase, the output from an
   assymetric key exchange etc. The behavior of Create and TestSecureSetup are
   controlled by the VectorSize parameter:

   if VectorSize = BlockVectorSize then
     The first BlockVectorSize * BlockSize bytes of AKey are copied to the
     IVector. The remaining bytes of AKey are passed to SetUp. (An exception is
     raised if less than MinKeySize bytes remain of AKey.)

   else if VectorSize = 0 then
     AKey is passed to SetUp and IVector is cleared.

   else if VectorSize = -1 then
     The key material in AKey is pre-processed. IVector is set, and MinKeySize
     bytes of key data are passed to SetUp.

   else if (-VectorSize >= MinKeySize) and (-VectorSize <= MaxKeySize) then
     The key material in AKey is pre-processed. IVector is NOT set, and
     -VectorSize bytes of key data are passed to SetUp.

   else if VectorSize < 0 then
     The key material in AKey is pre-processed. IVector is set, and MaxKeySize
     bytes of key data are passed to SetUp.

   else
     An exception is raised.}

  TBlockCipher = class(TCipher)
  private
    FModeRatio: Integer;
  protected
    FIV: PByteArray;
    FFBIndex: Integer;
    function AllocBuffer: PByteArray;
    class function CreateAnother(const AKey; Count, VectorSize: Integer): TBlockCipher;
    procedure CleanUp; override;
    procedure DeallocBuffer(var T: PByteArray);
    procedure Decrypt_ABC(var Buf; Count: Integer);
    procedure Decrypt_CBC(var Buf; Count: Integer);
    procedure Decrypt_CFB(var Buf; Count: Integer);
    procedure DecryptBlock(var Buf); virtual;
    procedure DecryptBlockToDst(var Dst; const Src); virtual;
    procedure Encrypt_ABC(var Buf; Count: Integer);
    procedure Encrypt_CBC(var Buf; Count: Integer);
    procedure Encrypt_CFB(var Buf; Count: Integer);
    procedure Encrypt_CTR(var Buf; Count: Integer);
    procedure Encrypt_OFB(var Buf; Count: Integer);
    procedure EncryptBlock(var Buf); virtual;
    procedure EncryptBlockToDst(var Dst; const Src); virtual;
    function GetVector: OctetString; override;
    procedure SetModeRatio(Value: Integer); virtual;
    procedure SetVector(const Value: OctetString); override;
    property ModeRatio: Integer read FModeRatio write SetModeRatio;
    property FBIndex: Integer read FFBIndex;
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    constructor CreateIntf(ASecretKey: ISecretKey); override;
    class function BlockVectorSize: Integer; virtual;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    procedure GetVectorBuf(var IV; Count: Integer); override;
    class function MaxKeySize: Integer; virtual;
    class function MinKeySize: Integer; virtual;
    procedure SetVectorBuf(const IV; Count: Integer); override;
    procedure TestSecureSetUp(const AKey; Count, VectorSize: Integer); virtual;
  end;

  TBlockCipherClass = class of TBlockCipher;

  THashAlgorithm = ({$IFDEF SHA1}     haSHA1     {$ELSE}haReserved0{$ENDIF},
                    {$IFDEF MD5}      haMD5      {$ELSE}haReserved1{$ENDIF},
                    {$IFDEF RIPEMD160}haRipeMD160{$ELSE}haReserved2{$ENDIF},
                    {$IFDEF SHA256}   haSHA256   {$ELSE}haReserved3{$ENDIF},
                    {$IFDEF SHA512}   haSHA384   {$ELSE}haReserved4{$ENDIF},
                    {$IFDEF SHA512}   haSHA512   {$ELSE}haReserved5{$ENDIF},
                    {$IFDEF MD2}      haMD2      {$ELSE}haReserved6{$ENDIF},
                    {$IFDEF SHA256}   haSHA224   {$ELSE}haReserved7{$ENDIF},
                    haNull,
                    haDefault);

  {THash is the abstract base class for hash algorithms.}
  THash = class
  protected
    function GetDigest: OctetString; virtual; abstract;
    procedure Calc; virtual; abstract;
    procedure CleanUp; virtual; abstract;
    procedure Init; virtual; abstract;
  public
    constructor Create(const Data; Count: Int64); virtual;
    destructor Destroy; override;
    class function Algorithm: THashAlgorithm; virtual;
    class function CyclesOfCompress: Int64;
    class function DERPrefix: Pointer; virtual;
    class function DERPrefixLen: Integer; virtual;
    class function DigestSize: Integer; virtual;
    procedure Done(Digest: Pointer); virtual; abstract;
    procedure GetContext(Context: Pointer); virtual; abstract;
    procedure HashData(const Data; Count: Int64); virtual; abstract;
    procedure SetUp; virtual;
    procedure SetUpContext(Context: Pointer); virtual; abstract;
    property Digest: OctetString read GetDigest;
  end;

  THashClass = class of THash;

  TBaseHash = class(THash)
  protected
    FCount: Int64;
    FDataBuffer: packed array [0..127] of Byte;
    FDigest: packed array [0..63] of Byte;
    procedure CleanUp; override;
    function GetDigest: OctetString; override;
  public
    procedure GetContext(Context: Pointer); override;
    procedure HashData(const Data; Count: Int64); override;
    procedure SetUpContext(Context: Pointer); override;
    property Digest;
  end;

  TBaseSHA = class(TBaseHash)
  public
    procedure Done(Digest: Pointer); override;
  end;

{$IFDEF SHA1}
  {TSHA1 is a fast assembler implementation of the Secure Hash Algorithm 1.
   It is declared in this unit because it is used by TBlockCipher.}
  TSHA1 = class(TBaseSHA)
  protected
    procedure Calc; override;
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;
{$ENDIF SHA1}

  TBaseMD = class(TBaseHash)
  public
    procedure Done(Digest: Pointer); override;
  end;

{$IFDEF MD5}
  TMD5 = class(TBaseMD)
  protected
    procedure Calc; override;
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;      
{$ENDIF MD5}
       
{$IFDEF RIPEMD160}
  TRipeMD160 = class(TBaseMD)
  protected
    procedure Calc; override;
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;           
{$ENDIF RIPEMD160}
  
{$IFDEF SHA256}
  TSHA256 = class(TBaseSHA)
  protected
    procedure Calc; override;
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;      
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;         

  TSHA224 = class(TSHA256)
  protected
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;
{$ENDIF SHA256}
       
{$IFDEF SHA512}
  TSHA512 = class(TBaseHash)
  protected
    procedure Calc; override;
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
    procedure Done(Digest: Pointer); override;
    procedure HashData(const Data; Count: Int64); override;
  end;

  TSHA384 = class(TSHA512)
  protected
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override; 
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;        
{$ENDIF SHA512}
       
{$IFDEF MD2}
  TMD2 = class(THash)
  protected
    FCount: Byte;
    FCheckSum: array [0..15] of Byte;
    FDataBuffer: packed array [0..47] of Byte;
    procedure Calc; override;
    procedure CleanUp; override;
    function GetDigest: OctetString; override;
    procedure Init; override;
  public
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
    procedure Done(Digest: Pointer); override;
    procedure GetContext(Context: Pointer); override;
    procedure HashData(const Data; Count: Int64); override;
    procedure SetUpContext(Context: Pointer); override;
    property Digest;
  end;     
{$ENDIF MD2}

  {TPRGTest is a reference implementation used by the PRG Test application.
   Do NOT use it for security purposes.}
  TPRGTest = class(TCipher)
  protected
    procedure CleanUp; override;
    function GetVector: OctetString; override;
    procedure SetVector(const Value: OctetString); override;
  public
    class function Mode: TCipherMode; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
  end;


{$IFDEF SHA1}
procedure WeakPasswordToPrivKey(const Password; PWLen: Integer;
                                Iter: Integer;
                                var Key; KeyLen: Integer); overload;
procedure WeakPasswordToPrivKey(const Password; PWLen: Integer;
                                const Nonce; NonceLen: Integer;
                                Iter: Integer;
                                var Key; KeyLen: Integer); overload;
procedure WeakPasswordToPrivKey(const Password: string;
                                const Nonce: string;
                                Iter: Integer;
                                var Key; KeyLen: Integer); overload;
procedure WeakPasswordToPrivKey(const Password; PWLen: Integer;
                                const Nonce; NonceLen: Integer;
                                Iter: Integer;
                                KeyLen: Integer;
                                var Key: ISecretKey); overload;
procedure WeakPasswordToPrivKey(Password: ISecretKey;
                                Nonce: ISecretKey;
                                Iter: Integer;
                                KeyLen: Integer;
                                var Key: ISecretKey); overload;
procedure WeakPasswordToPrivKeyStr(Password: PChar;
                                   Nonce: PChar;
                                   Iter: Integer;
                                   Key: PChar; KeyLen: Integer);
procedure WeakPasswordKeySplit(Password: ISecretKey;
                               Nonce: ISecretKey;
                               Iter: Integer;
                               Label1, Label2: ISecretKey;
                               KeyLen1, KeyLen2: Integer;
                               var Key1, Key2: ISecretKey);
{$ENDIF SHA1}

{$IFDEF SHA256}
procedure WeakPasswordToPrivKey256(const Password; PWLen: Integer;
                                   Iter: Integer;
                                   var Key; KeyLen: Integer); overload;
procedure WeakPasswordToPrivKey256(const Password; PWLen: Integer;
                                   const Nonce; NonceLen: Integer;
                                   Iter: Integer;
                                   var Key; KeyLen: Integer); overload;
procedure WeakPasswordToPrivKey256(const Password: string;
                                   const Nonce: string;
                                   Iter: Integer;
                                   var Key; KeyLen: Integer); overload;
procedure WeakPasswordToPrivKey256(const Password; PWLen: Integer;
                                   const Nonce; NonceLen: Integer;
                                   Iter: Integer;
                                   KeyLen: Integer;
                                   var Key: ISecretKey); overload;
procedure WeakPasswordToPrivKey256(Password: ISecretKey;
                                   Nonce: ISecretKey;
                                   Iter: Integer;
                                   KeyLen: Integer;
                                   var Key: ISecretKey); overload;
procedure WeakPasswordToPrivKey256Str(Password: PChar;
                                      Nonce: PChar;
                                      Iter: Integer;
                                      Key: PChar; KeyLen: Integer);
procedure WeakPasswordKeySplit256(Password: ISecretKey;
                                  Nonce: ISecretKey;
                                  Iter: Integer;
                                  Label1, Label2: ISecretKey;
                                  KeyLen1, KeyLen2: Integer;
                                  var Key1, Key2: ISecretKey);
{$ENDIF SHA256}

procedure WeakPasswordKDFv2(const Password; PWLen: Integer;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            var Key; KeyLen: Integer); overload;
procedure WeakPasswordKDFv2(const Password; PWLen: Integer;
                            const Nonce; NonceLen: Integer;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            var Key; KeyLen: Integer); overload;
procedure WeakPasswordKDFv2(const Password: string;
                            const Nonce: string;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            var Key; KeyLen: Integer); overload;
procedure WeakPasswordKDFv2(const Password; PWLen: Integer;
                            const Nonce; NonceLen: Integer;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            KeyLen: Integer;
                            var Key: ISecretKey); overload;
procedure WeakPasswordKDFv2(Password: ISecretKey;
                            Nonce: ISecretKey;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            KeyLen: Integer;
                            var Key: ISecretKey); overload;
procedure WeakPasswordKDFv2Str(Password: PChar;
                               Nonce: PChar;
                               Iter: Integer;
                               HashAlg: THashAlgorithm;
                               Key: PChar; KeyLen: Integer);

procedure HMACBegin(const K; KLen: Integer; H: THash);
procedure HMACEnd(const K; KLen: Integer; H: THash; var Mac; MacLen: Integer);
procedure HMAC(const K; KLen: Integer;
               const M; MLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var Mac; MacLen: Integer); overload;
procedure HMAC(K: ISecretKey;
               const M; MLen: Integer;
               HashAlgorithm: THashAlgorithm;
               MacLen: Integer; var Mac: ISecretKey); overload;

procedure CBCMACBegin(const K; KLen: Integer;
                      var IV; IVLen: Integer;
                      C: TBlockCipher);
procedure CBCMACEnd(C: TBlockCipher;
                    var IV; IVLen: Integer;
                    const M; MLen: Integer;
                    var Mac; MacLen: Integer);
procedure CBCMAC(const K; KLen: Integer;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 var Mac; MacLen: Integer); overload;
procedure CBCMAC(const K; KLen: Integer;
                 const IV; IVLen: Integer;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 var Mac; MacLen: Integer); overload;
procedure CBCMAC(K: ISecretKey;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 MacLen: Integer; var Mac: ISecretKey); overload;
procedure CBCMAC(K: ISecretKey;
                 IV: ISecretKey;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 MacLen: Integer; var Mac: ISecretKey); overload;

procedure ProtectClear(var ABuf; Count: Integer);

procedure XORBlock128(var Dest; const Source);
procedure AddBlock128(var Dest; const Source);
procedure XORBlock64(var Dest; const Source);
procedure AddBlock64(var Dest; const Source);

procedure XORBytes(var Dest; const Source; Count: Integer);
                                           
procedure IncBlockByInt(var Dst; const Src; LWCount: Integer; Value: LongInt);

// Data conversion routines:

function BinToHex(const ABuf; Count: Integer): string;
procedure HexToBin(const Value: string; var ABuf; var Count: Integer);

function GetTrimmedHex(Strings: TStrings): string;
function TrimHex(const Value: string): string;

// Cipher class registration routines:

procedure RegisterCipherClass(AClass: TCipherClass);
procedure RegisterCipherClasses(const AClasses: array of TCipherClass);

function FindCipherClass(Alg: TCipherAlg;
                         Mode: TCipherMode): TCipherClass; overload;
function FindCipherClass(const AlgName: string;
                         Mode: TCipherMode): TCipherClass; overload;
function FindCipherClass(const AlgOID: ObjectIdentifier; // Alg + Mode + Keysize
                         var KeySize: Integer;           // Keysize (excl IV)
                         var KeyedIV: Boolean): TCipherClass; overload;
                                              
function CreateCipher(const Key; KeyLength: Integer;
                      CipherClass: TCipherClass;
                      KeyedIV: Boolean;
                      Param: string): TCipher;
function CreateCipher_v2(const Key; KeyLength: Integer;
                         CipherClass: TCipherClass;
                         KeyedIV: Boolean;
                         Param: string;
                         MACKey: ISecretKey = nil): TCipher;

// Hash class registration routines:

function FindHashClass(Alg: THashAlgorithm): THashClass;

// Basic utilities:

function EncryptString(const AlgName: string; Mode: TCipherMode;
                       const Key, PlainText: string): OctetString; overload;
function DecryptString(const AlgName: string; Mode: TCipherMode;
                       const Key: string;
                       const CipherText: OctetString): string; overload;
function EncryptString(Alg: TCipherAlg; Mode: TCipherMode;
                       const Key, PlainText: string): OctetString; overload;
function DecryptString(Alg: TCipherAlg; Mode: TCipherMode;
                       const Key: string;
                       const CipherText: OctetString): string; overload;
function EncryptString(CipherClass: TCipherClass;
                       const Key, PlainText: string): OctetString; overload;
function DecryptString(CipherClass: TCipherClass;
                       const Key: string;
                       const CipherText: OctetString): string; overload;
function EncryptString(CipherClass: TCipherClass;
                       const Key, IV, PlainText: string): OctetString; overload;
function DecryptString(CipherClass: TCipherClass;
                       const Key, IV: string;
                       const CipherText: OctetString): string; overload;

procedure EncryptToPtr(const AlgName: string; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       PlainText: Pointer;
                       TextLen: Integer;
                       CipherText: Pointer); overload;
procedure DecryptToPtr(const AlgName: string; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       CipherText: Pointer;
                       TextLen: Integer;
                       PlainText: Pointer); overload;
procedure EncryptToPtr(Alg: TCipherAlg; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       PlainText: Pointer;
                       TextLen: Integer;
                       CipherText: Pointer); overload;
procedure DecryptToPtr(Alg: TCipherAlg; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       CipherText: Pointer;
                       TextLen: Integer;
                       PlainText: Pointer); overload;
procedure EncryptToPtr(CipherClass: TCipherClass;
                       Key: Pointer; KeyLen: Integer;
                       PlainText: Pointer;
                       TextLen: Integer;
                       CipherText: Pointer); overload;
procedure DecryptToPtr(CipherClass: TCipherClass;
                       Key: Pointer; KeyLen: Integer;
                       CipherText: Pointer;
                       TextLen: Integer;
                       PlainText: Pointer); overload;

function DigestString(HashAlgorithm: THashAlgorithm;
                      const Text: string): OctetString; overload;
procedure DigestString(HashAlgorithm: THashAlgorithm;
                       const Text: string;
                       Digest: Pointer; var DigLen: Integer); overload;
function DigestStream(HashAlgorithm: THashAlgorithm;
                      Stream: TStream): OctetString; overload;
procedure DigestStream(HashAlgorithm: THashAlgorithm;
                       Stream: TStream;
                       Digest: Pointer; var DigLen: Integer); overload;
function DigestFile(HashAlgorithm: THashAlgorithm;
                    const FileName: string): OctetString; overload;
procedure DigestFile(HashAlgorithm: THashAlgorithm;
                     const FileName: string;
                     Digest: Pointer; var DigLen: Integer); overload;

function HMACString(HashAlgorithm: THashAlgorithm;
                    const Key: string;
                    MacLen: Integer;
                    const Text: string): string; overload;
procedure HMACString(HashAlgorithm: THashAlgorithm;
                     Key: Pointer; KeyLen: Integer;
                     const Text: string;
                     Mac: Pointer; MacLen: Integer); overload;
function HMACStream(HashAlgorithm: THashAlgorithm;
                    const Key: string;
                    MacLen: Integer;
                    Stream: TStream): string; overload;
procedure HMACStream(HashAlgorithm: THashAlgorithm;
                     Key: Pointer; KeyLen: Integer;
                     Stream: TStream;
                     Mac: Pointer; MacLen: Integer); overload;
function HMACFile(HashAlgorithm: THashAlgorithm;
                  const Key: string;
                  MacLen: Integer;
                  const FileName: string): string; overload;
procedure HMACFile(HashAlgorithm: THashAlgorithm;
                   Key: Pointer; KeyLen: Integer;
                   const FileName: string;
                   Mac: Pointer; MacLen: Integer); overload;

procedure GetSMIMECapabilities(AList: TStrings);

implementation

uses
  SysUtils,
{$IFDEF MSWINDOWS}
{$IFDEF LINUX}
  Compiler Error
{$ENDIF}
  Windows
{$ENDIF}
{$IFDEF LINUX}
  Libc
{$ENDIF};

var
  CipherClassList: TList;

procedure RegisterCipherClass(AClass: TCipherClass);
begin
  if CipherClassList = nil then
    CipherClassList := TList.Create;
  CipherClassList.Add(AClass);
end;

procedure RegisterCipherClasses(const AClasses: array of TCipherClass);
var
  I: Integer;
begin
  if CipherClassList = nil then
    CipherClassList := TList.Create;
  for I := Low(AClasses) to High(AClasses) do
    RegisterCipherClass(AClasses[I]);
end;

function FindCipherClass(Alg: TCipherAlg; Mode: TCipherMode): TCipherClass;
var
  I: Integer;
begin
  if Assigned(CipherClassList) then
    for I := 0 to CipherClassList.Count - 1 do begin
      Result := CipherClassList[I];
      if (Result.Mode = Mode) and (Result.Algorithm = Alg) then
        Exit;
    end;
  Result := nil;
end;

function FindCipherClass(const AlgName: string; Mode: TCipherMode): TCipherClass; overload;
var
  I: Integer;
begin
  if Assigned(CipherClassList) then
    for I := 0 to CipherClassList.Count - 1 do begin
      Result := CipherClassList[I];
      if (Result.Mode = Mode) and
         (StrIComp(Result.AlgorithmName,PChar(AlgName)) = 0) then
        Exit;
    end;
  Result := nil;
end;

function FindCipherClass(const AlgOID: ObjectIdentifier;
                         var KeySize: Integer;
                         var KeyedIV: Boolean): TCipherClass; overload;
var
  I, KS: Integer;
  BC: TBlockCipherClass;
  OID: string;
begin
  if Assigned(CipherClassList) then
    for I := 0 to CipherClassList.Count - 1 do begin
      Result := CipherClassList[I];
      if Result.InheritsFrom(TBlockCipher) then begin
        BC := TBlockCipherClass(Result);
        if (KeySize >= BC.MinKeySize) and (KeySize <= BC.MaxKeySize) then begin
          if KeyedIV then begin
            OID := Result.KeyedIVOID(KeySize);
            if StrComp(PChar(OID),PChar(AlgOID)) = 0 then
              Exit;
          end else begin
            OID := Result.OID(KeySize);
            if StrComp(PChar(OID),PChar(AlgOID)) = 0 then
              Exit;
          end;
        end;
        for KS := BC.MinKeySize shr 3 to BC.MaxKeySize shr 3 do begin
          if KS = 0 then Continue;
          OID := Result.OID(KS*8);
          if StrComp(PChar(OID),PChar(AlgOID)) = 0 then begin
            KeySize := KS*8;
            KeyedIV := False;
            Exit;
          end else begin
            OID := Result.KeyedIVOID(KS*8);
            if StrComp(PChar(OID),PChar(AlgOID)) = 0 then begin
              KeySize := KS*8;
              KeyedIV := True;
              Exit;
            end;
          end;
        end;
      end else if StrComp(Result.OID(0),PChar(AlgOID)) = 0 then
        Exit;
    end;
  Result := nil;
end;

function FindHashClass(Alg: THashAlgorithm): THashClass;
begin
  case Alg of
    {$IFDEF SHA1}     haSHA1:      Result := TSHA1;      {$ENDIF}
    {$IFDEF SHA256}   haSHA224:    Result := TSHA224;    {$ENDIF}
    {$IFDEF SHA256}   haSHA256:    Result := TSHA256;    {$ENDIF}
    {$IFDEF SHA512}   haSHA384:    Result := TSHA384;    {$ENDIF}
    {$IFDEF SHA512}   haSHA512:    Result := TSHA512;    {$ENDIF}
    {$IFDEF RIPEMD160}haRipeMD160: Result := TRipeMD160; {$ENDIF}
    {$IFDEF MD5}      haMD5:       Result := TMD5;       {$ENDIF}
    {$IFDEF MD2}      haMD2:       Result := TMD2;       {$ENDIF}
  else
    Result := nil;
  end;
end;

procedure GetSMIMECapabilities(AList: TStrings);
var
  I, KeyLen: Integer;
  CC: TCipherClass;
  BCC: TBlockCipherClass;
begin
  for I := 0 to CipherClassList.Count - 1 do begin
    CC := CipherClassList[I];
    if CC.InheritsFrom(TBlockCipher) and (CC.Mode = cmCBC) then begin
      BCC := TBlockCipherClass(CC);
      KeyLen := BCC.MinKeySize;
      if KeyLen < 8 then KeyLen := 8;
      repeat
        if BCC.OID(KeyLen) <> nil then
          AList.AddObject(BCC.OID(KeyLen),TObject(KeyLen));
        Inc(KeyLen,8);
      until KeyLen > BCC.MaxKeySize;
    end;
  end;
end;

function CreateCipher(const Key; KeyLength: Integer;
                      CipherClass: TCipherClass;
                      KeyedIV: Boolean;
                      Param: string): TCipher;
{$IFDEF SHA256}
var
  K: ISecretKey;
{$ENDIF SHA256}
begin
  if not KeyedIV then begin
    Result := CipherClass.Create(Key,KeyLength,0);
    if Param <> '' then
      Result.IVector := Param;
  end else if Param <> '' then begin
{$IFDEF SHA256}
    WeakPasswordToPrivKey256(Key,KeyLength,Pointer(Param)^,Length(Param),1,KeyLength,K);
    Result := CipherClass.Create(K.Key^,K.KeyLen,
                                 TBlockCipherClass(CipherClass).BlockVectorSize);
{$ELSE  SHA256}
    raise Exception.Create('CreateCipher: Action requires SHA-256');
{$ENDIF SHA256}
  end else
    Result := CipherClass.Create(Key,KeyLength,
                                 TBlockCipherClass(CipherClass).BlockVectorSize);
end;

function CreateCipher_v2(const Key; KeyLength: Integer;
                         CipherClass: TCipherClass;
                         KeyedIV: Boolean;
                         Param: string;
                         MACKey: ISecretKey = nil): TCipher;
var
  P: string;
  K: ISecretKey;
begin
  if KeyedIV then
    P := Param
  else
    P := '';
  if Assigned(MACKey) then begin
{$IFDEF SHA256}
    WeakPasswordKDFv2(Key,KeyLength,Pointer(P)^,Length(P),1,haSHA256,KeyLength*2,K);
    MACKey.SetKey(@K.KeyBytes[KeyLength],KeyLength,0);
{$ELSE  SHA256}
    raise Exception.Create('CreateCipher_v2: Action requires SHA-256');
{$ENDIF SHA256}
  end else if P <> '' then
{$IFDEF SHA256}
    WeakPasswordKDFv2(Key,KeyLength,Pointer(P)^,Length(P),1,haSHA256,KeyLength,K)
{$ELSE  SHA256}
    raise Exception.Create('CreateCipher_v2: Action requires SHA-256')
{$ENDIF SHA256}
  else
    K := TSecretKey.CreateStr(@Key,KeyLength);
  if not KeyedIV then begin
    Result := CipherClass.Create(K.Key^,KeyLength,0);
    if Param <> '' then
      Result.IVector := Param;
  end else
    Result := CipherClass.Create(K.Key^,KeyLength,
                                 TBlockCipherClass(CipherClass).BlockVectorSize);
end;

function EncryptString(CipherClass: TCipherClass;
                       const Key, IV, PlainText: string): OctetString;
var
  C: TCipher;
begin
  if Assigned(CipherClass) then begin
    C := CipherClass.Create(Pointer(Key)^,Length(Key),0);
    try
      C.IVector := IV;
      Result := C.EncryptStr(PlainText);
    finally
      C.Free;
    end;
  end else
    Result := '';
end;

function EncryptString(CipherClass: TCipherClass;
                       const Key, PlainText: string): OctetString;
var
  C: TCipher;
begin
  if Assigned(CipherClass) then begin
    C := CipherClass.Create(Pointer(Key)^,Length(Key),0);
    try
      Result := C.EncryptStr(PlainText);
    finally
      C.Free;
    end;
  end else
    Result := '';
end;

function DecryptString(CipherClass: TCipherClass;
                       const Key, IV: string;
                       const CipherText: OctetString): string;
var
  C: TCipher;
begin
  if Assigned(CipherClass) then begin
    C := CipherClass.Create(Pointer(Key)^,Length(Key),0);
    try
      C.IVector := IV;
      Result := C.DecryptStr(CipherText);
    finally
      C.Free;
    end;
  end else
    Result := '';
end;

function DecryptString(CipherClass: TCipherClass;
                       const Key: string;
                       const CipherText: OctetString): string;
var
  C: TCipher;
begin
  if Assigned(CipherClass) then begin
    C := CipherClass.Create(Pointer(Key)^,Length(Key),0);
    try
      Result := C.DecryptStr(CipherText);
    finally
      C.Free;
    end;
  end else
    Result := '';
end;

function EncryptString(const AlgName: string; Mode: TCipherMode;
                       const Key, PlainText: string): OctetString;
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(AlgName,Mode);
  Result := EncryptString(CC,Key,PlainText);
end;

function DecryptString(const AlgName: string; Mode: TCipherMode;
                       const Key: string;
                       const CipherText: OctetString): string;
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(AlgName,Mode);
  Result := DecryptString(CC,Key,CipherText);
end;

function EncryptString(Alg: TCipherAlg; Mode: TCipherMode;
                       const Key, PlainText: string): OctetString;
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(Alg,Mode);
  Result := EncryptString(CC,Key,PlainText);
end;

function DecryptString(Alg: TCipherAlg; Mode: TCipherMode;
                       const Key: string;
                       const CipherText: OctetString): string;
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(Alg,Mode);
  Result := DecryptString(CC,Key,CipherText);
end;

procedure EncryptToPtr(CipherClass: TCipherClass;
                       Key: Pointer; KeyLen: Integer;
                       PlainText: Pointer;
                       TextLen: Integer;
                       CipherText: Pointer);
var
  C: TCipher;
begin
  if Assigned(CipherClass) then begin
    C := CipherClass.Create(Key^,KeyLen,0);
    try
      C.EncryptToPtr(PlainText,CipherText,TextLen);
    finally
      C.Free;
    end;
  end else
    raise Exception.Create('Cipher class not found');
end;

procedure DecryptToPtr(CipherClass: TCipherClass;
                       Key: Pointer; KeyLen: Integer;
                       CipherText: Pointer;
                       TextLen: Integer;
                       PlainText: Pointer);
var
  C: TCipher;
begin
  if Assigned(CipherClass) then begin
    C := CipherClass.Create(Key^,KeyLen,0);
    try
      C.DecryptToPtr(CipherText,PlainText,TextLen);
    finally
      C.Free;
    end;
  end else
    raise Exception.Create('Cipher class not found');
end;

procedure EncryptToPtr(const AlgName: string; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       PlainText: Pointer;
                       TextLen: Integer;
                       CipherText: Pointer);
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(AlgName,Mode);
  EncryptToPtr(CC,Key,KeyLen,PlainText,TextLen,CipherText);
end;

procedure DecryptToPtr(const AlgName: string; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       CipherText: Pointer;
                       TextLen: Integer;
                       PlainText: Pointer);
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(AlgName,Mode);
  DecryptToPtr(CC,Key,KeyLen,CipherText,TextLen,PlainText);
end;

procedure EncryptToPtr(Alg: TCipherAlg; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       PlainText: Pointer;
                       TextLen: Integer;
                       CipherText: Pointer);
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(Alg,Mode);
  EncryptToPtr(CC,Key,KeyLen,PlainText,TextLen,CipherText);
end;

procedure DecryptToPtr(Alg: TCipherAlg; Mode: TCipherMode;
                       Key: Pointer; KeyLen: Integer;
                       CipherText: Pointer;
                       TextLen: Integer;
                       PlainText: Pointer);
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(Alg,Mode);
  DecryptToPtr(CC,Key,KeyLen,CipherText,TextLen,PlainText);
end;

function DigestString(HashAlgorithm: THashAlgorithm; const Text: string): OctetString;
var
  HC: THashClass;
  H: THash;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then begin
    H := HC.Create(Pointer(Text)^,Length(Text));
    try
      H.Done(nil);
      Result := H.Digest;
    finally
      H.Free;
    end;
  end else
    Result := '';
end;

procedure DigestString(HashAlgorithm: THashAlgorithm;
                       const Text: string;
                       Digest: Pointer; var DigLen: Integer);
var
  HC: THashClass;
  H: THash;
begin
  HC := FindHashClass(HashAlgorithm);
  if Assigned(HC) then begin
    H := HC.Create(Pointer(Text)^,Length(Text));
    try
      if DigLen < H.DigestSize then
        raise Exception.Create('Digest buffer too small');
      H.Done(Digest);
      DigLen := H.DigestSize;
    finally
      H.Free;
    end;
  end else
    raise Exception.Create('Hash class not found');
end;

function DigestStream(HashAlgorithm: THashAlgorithm;
                      Stream: TStream): OctetString;
var
  HC: THashClass;
  H: THash;
  B: Pointer;
  BLen: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if HC = nil then
    Result := ''
  else begin
    H := HC.Create(nil^,0);
    try
      GetMem(B,$1000);
      try
        repeat
          BLen := Stream.Read(B^,$1000);
          H.HashData(B^,BLen);
        until BLen = 0;
      finally
        FreeMem(B);
      end;
      H.Done(nil);
      Result := H.Digest;
    finally
      H.Free;
    end;
  end;
end;

procedure DigestStream(HashAlgorithm: THashAlgorithm;
                       Stream: TStream;
                       Digest: Pointer; var DigLen: Integer);
var
  HC: THashClass;
  H: THash;
  B: Pointer;
  BLen: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if HC = nil then
    raise Exception.Create('Hash class not found');
  H := HC.Create(nil^,0);
  try
    if DigLen < H.DigestSize then
      raise Exception.Create('Digest buffer too small');
    GetMem(B,$1000);
    try
      repeat
        BLen := Stream.Read(B^,$1000);
        if BLen > 0 then
          H.HashData(B^,BLen);
      until BLen <= 0;
    finally
      FreeMem(B);
    end;
    H.Done(Digest);
    DigLen := H.DigestSize;
  finally
    H.Free;
  end;
end;

function DigestFile(HashAlgorithm: THashAlgorithm;
                    const FileName: string): OctetString;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmOpenRead);
  try
    Result := DigestStream(HashAlgorithm,FS);
  finally
    FS.Free;
  end;
end;

procedure DigestFile(HashAlgorithm: THashAlgorithm;
                     const FileName: string;
                     Digest: Pointer; var DigLen: Integer);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmOpenRead);
  try
    DigestStream(HashAlgorithm,FS,Digest,DigLen);
  finally
    FS.Free;
  end;
end;

function HMACString(HashAlgorithm: THashAlgorithm;
                    const Key: string;
                    MacLen: Integer;
                    const Text: string): string;
begin
  SetLength(Result,MacLen);
  HMac(Pointer(Key)^,Length(Key),
       Pointer(Text)^,Length(Text),
       HashAlgorithm,
       Pointer(Result)^,MacLen);
end;

procedure HMACString(HashAlgorithm: THashAlgorithm;
                     Key: Pointer; KeyLen: Integer;
                     const Text: string;
                     Mac: Pointer; MacLen: Integer);
begin
  HMac(Key^,KeyLen,
       Pointer(Text)^,Length(Text),
       HashAlgorithm,
       Mac^,MacLen);
end;

function HMACStream(HashAlgorithm: THashAlgorithm;
                    const Key: string;
                    MacLen: Integer;
                    Stream: TStream): string;
var
  HC: THashClass;
  H: THash;
  B: Pointer;
  BLen: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if HC = nil then
    Result := ''
  else begin
    H := HC.Create(nil^,0);
    try
      HMacBegin(Pointer(Key)^,Length(Key),H);
      GetMem(B,$1000);
      try
        repeat
          BLen := Stream.Read(B^,$1000);
          H.HashData(B^,BLen);
        until BLen = 0;
      finally
        FreeMem(B);
      end;
      SetLength(Result,MacLen);
      HMacEnd(Pointer(Key)^,Length(Key),H,Pointer(Result)^,MacLen);
    finally
      H.Free;
    end;
  end;
end;

procedure HMACStream(HashAlgorithm: THashAlgorithm;
                     Key: Pointer; KeyLen: Integer;
                     Stream: TStream;
                     Mac: Pointer; MacLen: Integer);
var
  HC: THashClass;
  H: THash;
  B: Pointer;
  BLen: Integer;
begin
  HC := FindHashClass(HashAlgorithm);
  if HC = nil then
    raise Exception.Create('Hash class not found');
  H := HC.Create(nil^,0);
  try
    HMacBegin(Key^,KeyLen,H);
    GetMem(B,$1000);
    try
      repeat
        BLen := Stream.Read(B^,$1000);
        H.HashData(B^,BLen);
      until BLen = 0;
    finally
      FreeMem(B);
    end;
    HMacEnd(Key^,KeyLen,H,Mac^,MacLen);
  finally
    H.Free;
  end;
end;

function HMACFile(HashAlgorithm: THashAlgorithm;
                  const Key: string;
                  MacLen: Integer;
                  const FileName: string): string;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmOpenRead);
  try
    Result := HMACStream(HashAlgorithm,Key,MacLen,FS);
  finally
    FS.Free;
  end;
end;

procedure HMACFile(HashAlgorithm: THashAlgorithm;
                   Key: Pointer; KeyLen: Integer;
                   const FileName: string;
                   Mac: Pointer; MacLen: Integer);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmOpenRead);
  try
    HMACStream(HashAlgorithm,Key,KeyLen,FS,Mac,MacLen);
  finally
    FS.Free;
  end;
end;

procedure CBCMACBegin(const K; KLen: Integer;       
                      var IV; IVLen: Integer;
                      C: TBlockCipher);
begin
  C.SetUp(K,KLen,0);
  FillChar(IV,IVLen,0);
end;

procedure CBCMACUpdate(C: TBlockCipher;
                       var IV; IVLen: Integer;
                       const M; MLen: Integer);
var
  P: Pointer;
  I: Integer;
begin
  Assert(IVLen = C.BlockSize);
  Assert(MLen mod IVLen = 0);
  P := @M;
  for I := 0 to Pred(MLen div IVLen) do begin
    XORBytes(IV,P^,IVLen);
    C.EncryptBlock(IV);
    P := Ptr(LongInt(P) + IVLen);
  end;
end;

procedure CBCMACEnd(C: TBlockCipher;
                    var IV; IVLen: Integer;
                    const M; MLen: Integer;
                    var Mac; MacLen: Integer);
var
  P: Pointer;
  TruncMLen: Integer;
begin
  Assert(MacLen <= IVLen);
  TruncMLen := (MLen div IVLen) * IVLen;
  CBCMACUpdate(C,IV,IVLen,M,TruncMLen);
  if TruncMLen < MLen then begin
    P := Ptr(LongInt(@M) + TruncMLen);
    XORBytes(IV,P^,MLen-TruncMLen);
    C.EncryptBlock(IV);
  end;
  Move(IV,Mac,MacLen);
end;

procedure CBCMAC(const K; KLen: Integer;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 var Mac; MacLen: Integer);
begin
  CBCMAC(K,KLen,nil^,0,M,MLen,CipherAlgorithm,Mac,MacLen);
end;

procedure CBCMAC(const K; KLen: Integer;
                 const IV; IVLen: Integer;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 var Mac; MacLen: Integer);
var
  CC: TCipherClass;
  C: TCipher;
  pIV: Pointer;
begin
  CC := FindCipherClass(CipherAlgorithm,cmCBC);
  Assert(Assigned(CC));
  Assert((IVLen = 0) or (IVLen = CC.BlockSize));
  C := CC.Create(K,KLen,0);
  try
    if IVLen = 0 then
      GetMem(pIV,C.BlockSize)
    else
      pIV := @IV;
    try
      if IVLen = 0 then
        FillChar(pIV^,C.BlockSize,0);
      CBCMACEnd(C as TBlockCipher,pIV^,C.BlockSize,M,MLen,Mac,MacLen);
    finally
      if IVLen = 0 then
        FreeMem(pIV);
    end;
  finally
    C.Free;
  end;
end;

procedure CBCMAC(K: ISecretKey;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 MacLen: Integer; var Mac: ISecretKey);
var
  IV: ISecretKey;
begin
  IV := nil;
  CBCMAC(K,IV,M,MLen,CipherAlgorithm,MacLen,Mac);
end;

procedure CBCMAC(K: ISecretKey;
                 IV: ISecretKey;
                 const M; MLen: Integer;
                 CipherAlgorithm: TCipherAlg;
                 MacLen: Integer; var Mac: ISecretKey);
var
  CC: TCipherClass;
  C: TCipher;
begin
  CC := FindCipherClass(CipherAlgorithm,cmCBC);
  Assert(Assigned(CC));
  Assert((IV = nil) or (IV.KeyLen = CC.BlockSize));
  C := CC.CreateIntf(K);
  try
    if IV = nil then begin
      IV := TSecretKey.Create('');
      IV.SetLength(CC.BlockSize);
      FillChar(IV.Key^,IV.KeyLen,0);
    end;
    Mac := TSecretKey.Create('');
    Mac.SetLength(MacLen);
    CBCMACEnd(C as TBlockCipher,IV.Key^,C.BlockSize,M,MLen,Mac.Key^,MacLen);
  finally
    C.Free;
  end;
end;

{$IFDEF SHA1}
procedure WeakPasswordToPrivKey(const Password; PWLen: Integer;
                                Iter: Integer;
                                var Key; KeyLen: Integer);
begin
  WeakPasswordToPrivKey(Password,PWLen,Password,0,Iter,Key,KeyLen);
end;

procedure WeakPasswordToPrivKey(const Password: string;
                                const Nonce: string;
                                Iter: Integer;
                                var Key; KeyLen: Integer);
begin
  WeakPasswordToPrivKey(Pointer(Password)^,Length(Password),
                        Pointer(Nonce)^,Length(Nonce),
                        Iter,Key,KeyLen);
end;

procedure WeakPasswordToPrivKeyStr(Password: PChar;
                                   Nonce: PChar;
                                   Iter: Integer;
                                   Key: PChar; KeyLen: Integer);
begin
  WeakPasswordToPrivKey(Password^,StrLen(Password),
                        Nonce^,StrLen(Nonce),
                        Iter,
                        Key^,KeyLen);
end;
                                                     
procedure WeakPasswordToPrivKey(const Password; PWLen: Integer;
                                const Nonce; NonceLen: Integer;
                                Iter: Integer;
                                var Key; KeyLen: Integer);
var
  K: ISecretKey;
begin
  K := TSecretKey.Create('');
  WeakPasswordToPrivKey(Password,PWLen,Nonce,NonceLen,Iter,KeyLen,K);
  Move(K.Key^,Key,KeyLen);
end;

procedure WeakPasswordToPrivKey(Password: ISecretKey;
                                Nonce: ISecretKey;
                                Iter: Integer;
                                KeyLen: Integer;
                                var Key: ISecretKey);
begin
  WeakPasswordToPrivKey(Password.Key^,Password.KeyLen,
                        Nonce.Key^,Nonce.KeyLen,
                        Iter,
                        KeyLen,Key);
end;

procedure WeakPasswordToPrivKey(const Password; PWLen: Integer;
                                const Nonce; NonceLen: Integer;
                                Iter: Integer;
                                KeyLen: Integer;
                                var Key: ISecretKey);
var
  P: PChar;
  H: THash;
  I, J, K: Integer;
  B: packed array [0..63] of Byte;
begin
  GetMem(P,KeyLen + 20);
  try
    H := TSHA1.Create(Password,0);
    try
      try
        K := 0;
        J := 0;
        while J < KeyLen do begin
          H.SetUp;
          H.HashData(Nonce,NonceLen);
          H.HashData(Password,PWLen);
          B[3] := K and $FF;
          B[2] := (K shr 8) and $FF;
          B[1] := (K shr 16) and $FF;
          B[0] := K shr 24;
          H.HashData(B,4);
          H.Done(P + J);
          Inc(J,20);
          Inc(K);
        end;
        for I := 1 to Iter - 1 do begin
          J := 0;
          while J < KeyLen do begin
            H.SetUp;
            H.HashData((P + J)^,20);
            H.HashData(Nonce,NonceLen);
            H.HashData(Password,PWLen);
            B[3] := K and $FF;
            B[2] := (K shr 8) and $FF;
            B[1] := (K shr 16) and $FF;
            B[0] := K shr 24;
            H.HashData(B,4);
            H.Done(P + J);
            Inc(J,20);
            Inc(K);
          end;
        end;
        H.Done(P);
        if KeyLen > 20 then try
          FillChar(B,64,0);
          if KeyLen > 64 then
            Move(P^,B,64)
          else
            Move(P^,B,KeyLen);
          for I := 0 to 63 do
            B[I] := B[I] xor $36;
          J := 0;
          while J < KeyLen do begin
            H.SetUp;
            H.HashData(B,64);
            H.HashData((P + J)^,20);
            H.HashData(Nonce,NonceLen);
            H.HashData(Password,PWLen);
            H.Done(P + J);
            Inc(J,20);
          end;
          for I := 0 to 63 do
            B[I] := B[I] xor $36 xor $5C;
          J := 0;
          while J < KeyLen do begin
            H.SetUp;
            H.HashData(B,64);
            H.HashData((P + J)^,20);
            H.Done(P + J);
            Inc(J,20);
          end;
        finally
          ProtectClear(B,SizeOf(B));
        end;
        Key := TSecretKey.Create('');
        Key.SetKey(P,KeyLen,0);
      finally
        ProtectClear(P^,KeyLen + 20);
      end;
    finally
      H.Free;
    end;
  finally
    FreeMem(P);
  end;
end;

procedure WeakPasswordKeySplit(Password: ISecretKey;
                               Nonce: ISecretKey;
                               Iter: Integer;
                               Label1, Label2: ISecretKey;
                               KeyLen1, KeyLen2: Integer;
                               var Key1, Key2: ISecretKey);
var
  MasterKey: ISecretKey;
begin
  if KeyLen1 > KeyLen2 then
    WeakPasswordToPrivKey(Password,Nonce,Iter,KeyLen1,MasterKey)
  else
    WeakPasswordToPrivKey(Password,Nonce,Iter,KeyLen2,MasterKey);
  WeakPasswordToPrivKey(MasterKey,Label1,1,KeyLen1,Key1);
  WeakPasswordToPrivKey(MasterKey,Label2,1,KeyLen2,Key2);
end;
{$ENDIF SHA1}

{$IFDEF SHA256}
procedure WeakPasswordToPrivKey256(const Password; PWLen: Integer;
                                   Iter: Integer;
                                   var Key; KeyLen: Integer);
begin
  WeakPasswordToPrivKey256(Password,PWLen,Pointer(nil)^,0,Iter,Key,KeyLen);
end;

procedure WeakPasswordToPrivKey256(const Password: string;
                                   const Nonce: string;
                                   Iter: Integer;
                                   var Key; KeyLen: Integer);
begin
  WeakPasswordToPrivKey256(Pointer(Password)^,Length(Password),
                           Pointer(Nonce)^,Length(Nonce),
                           Iter,Key,KeyLen);
end;

procedure WeakPasswordToPrivKey256Str(Password: PChar;
                                      Nonce: PChar;
                                      Iter: Integer;
                                      Key: PChar; KeyLen: Integer);
begin
  WeakPasswordToPrivKey256(Password^,StrLen(Password),
                           Nonce^,StrLen(Nonce),
                           Iter,
                           Key^,KeyLen);
end;

procedure WeakPasswordToPrivKey256(const Password; PWLen: Integer;
                                   const Nonce; NonceLen: Integer;
                                   Iter: Integer;
                                   var Key; KeyLen: Integer);
var
  K: ISecretKey;
begin
  K := TSecretKey.Create('');
  WeakPasswordToPrivKey256(Password,PWLen,Nonce,NonceLen,Iter,KeyLen,K);
  Move(K.Key^,Key,KeyLen);
end;

procedure WeakPasswordToPrivKey256(Password: ISecretKey;
                                   Nonce: ISecretKey;
                                   Iter: Integer;
                                   KeyLen: Integer;
                                   var Key: ISecretKey);
begin   
  WeakPasswordToPrivKey256(Password.Key^,Password.KeyLen,
                           Nonce.Key^,Nonce.KeyLen,
                           Iter,
                           KeyLen,Key);
end;

procedure WeakPasswordToPrivKey256(const Password; PWLen: Integer;
                                   const Nonce; NonceLen: Integer;
                                   Iter: Integer;
                                   KeyLen: Integer;
                                   var Key: ISecretKey);
var
  P: PChar;
  H: TSHA256;
  I, J, K: Integer;
  B: packed array [0..63] of Byte;
begin
  GetMem(P,KeyLen + 32);
  try
    H := TSHA256.Create(Password,0);
    try
      try
        K := 0;
        J := 0;
        while J < KeyLen do begin
          H.SetUp;
          H.HashData(Nonce,NonceLen);
          H.HashData(Password,PWLen);
          B[3] := K and $FF;
          B[2] := (K shr 8) and $FF;
          B[1] := (K shr 16) and $FF;
          B[0] := K shr 24;
          H.HashData(B,4);
          H.Done(P + J);
          Inc(J,32);
          Inc(K);
        end;
        for I := 1 to Iter - 1 do begin
          J := 0;
          while J < KeyLen do begin
            H.SetUp;
            H.HashData((P + J)^,32);
            H.HashData(Nonce,NonceLen);
            H.HashData(Password,PWLen);
            B[3] := K and $FF;
            B[2] := (K shr 8) and $FF;
            B[1] := (K shr 16) and $FF;
            B[0] := K shr 24;
            H.HashData(B,4);
            H.Done(P + J);
            Inc(J,32);
            Inc(K);
          end;
        end;
        H.Done(P);
        if KeyLen > 32 then try
          FillChar(B,64,0);
          if KeyLen > 64 then
            Move(P^,B,64)
          else
            Move(P^,B,KeyLen);
          for I := 0 to 63 do
            B[I] := B[I] xor $36;
          J := 0;
          while J < KeyLen do begin
            H.SetUp;
            H.HashData(B,64);
            H.HashData((P + J)^,32);
            H.HashData(Nonce,NonceLen);
            H.HashData(Password,PWLen);
            H.Done(P + J);
            Inc(J,32);
          end;
          for I := 0 to 63 do
            B[I] := B[I] xor $36 xor $5C;
          J := 0;
          while J < KeyLen do begin
            H.SetUp;
            H.HashData(B,64);
            H.HashData((P + J)^,32);
            H.Done(P + J);
            Inc(J,32);
          end;
        finally
          ProtectClear(B,SizeOf(B));
        end;
        Key := TSecretKey.Create('');
        Key.SetKey(P,KeyLen,0);
      finally
        ProtectClear(P^,KeyLen + 32);
      end;
    finally
      H.Free;
    end;
  finally
    FreeMem(P);
  end;
end;

procedure WeakPasswordKeySplit256(Password: ISecretKey;
                                  Nonce: ISecretKey;
                                  Iter: Integer;
                                  Label1, Label2: ISecretKey;
                                  KeyLen1, KeyLen2: Integer;
                                  var Key1, Key2: ISecretKey);
var
  MasterKey: ISecretKey;
begin
  if KeyLen1 > KeyLen2 then
    WeakPasswordToPrivKey256(Password,Nonce,Iter,KeyLen1,MasterKey)
  else
    WeakPasswordToPrivKey256(Password,Nonce,Iter,KeyLen2,MasterKey);
  WeakPasswordToPrivKey256(MasterKey,Label1,1,KeyLen1,Key1);
  WeakPasswordToPrivKey256(MasterKey,Label2,1,KeyLen2,Key2);
end;           
{$ENDIF SHA256}

procedure WeakPasswordKDFv2(const Password; PWLen: Integer;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            var Key; KeyLen: Integer);
begin
  WeakPasswordKDFv2(Password,PWLen,nil^,0,Iter,HashAlg,Key,KeyLen);
end;

procedure WeakPasswordKDFv2(const Password; PWLen: Integer;
                            const Nonce; NonceLen: Integer;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            var Key; KeyLen: Integer);
var
  P: ISecretKey;
  HC: THashClass;
  H: THash;
  I, J, Counter, KSize: Integer;
  C: packed array [0..3] of Byte;
  BS, BE: packed array [0..127] of Byte;
  D: ISecretKey;
begin
  HC := FindHashClass(HashAlg);
  if HC = nil then
    raise Exception.Create('WeakPasswordKDFv2: Algorithm not supported');

  P := TSecretKey.Create('');
  P.SetLength(KeyLen + HC.DigestSize);

  H := HC.Create(nil^,0);
  try
{$IFDEF SHA512}
    if H.Algorithm in [haSHA384,haSHA512] then
      KSize := 128
    else
{$ENDIF SHA512}
{$IFDEF MD2}
    if H.Algorithm = haMD2 then
      KSize := 16
    else
{$ENDIF MD2}
      KSize := 64;
    FillChar(BS,KSize,0);
    D := TSecretKey.Create('');
    D.SetLength(HC.DigestSize);
    Counter := 0;
    J := 0;
    try
      if PWLen > KSize then begin
        H.HashData(Password,PWLen);
        H.Done(@BS);
      end else
        Move(Password,BS,PWLen);
      Move(BS,BE,KSize);
      for I := 0 to KSize-1 do
        BS[I] := BS[I] xor $36;
      for I := 0 to KSize-1 do
        BE[I] := BE[I] xor $5C;

      while J < KeyLen do begin
        H.SetUp;
        H.HashData(BS,KSize);
        H.HashData(Nonce,NonceLen);
        C[3] := Counter and $FF;
        C[2] := (Counter shr 8) and $FF;
        C[1] := (Counter shr 16) and $FF;
        C[0] := Counter shr 24;
        H.HashData(C,4);
        H.Done(D.Key);
        H.SetUp;
        H.HashData(BE,KSize);
        H.HashData(D.Key^,D.KeyLen);
        H.Done(@P.KeyBytes^[J]);
        Inc(J,HC.DigestSize);
        Inc(Counter);
      end;
      for I := 1 to Iter - 1 do begin
        J := 0;
        while J < KeyLen do begin
          H.SetUp;
          H.HashData(BS,KSize);
          H.HashData(P.KeyBytes[J],H.DigestSize);
          H.HashData(Nonce,NonceLen);
          C[3] := Counter and $FF;
          C[2] := (Counter shr 8) and $FF;
          C[1] := (Counter shr 16) and $FF;
          C[0] := Counter shr 24;
          H.HashData(C,4);
          H.Done(D.Key);
          H.SetUp;
          H.HashData(BE,KSize);
          H.HashData(D.Key^,D.KeyLen);
          H.Done(D.Key);
          P.XORKeyAt(D,J);
          Inc(J,H.DigestSize);
          Inc(Counter);
        end;
      end;
      H.SetUp;
      H.HashData(P.Key^,KeyLen);
      H.Done(D.Key);

      FillChar(BS,KSize,0);
      Move(D.Key^,BS,D.KeyLen);
      Move(BS,BE,KSize);
      for I := 0 to KSize - 1 do
        BS[I] := BS[I] xor $36;
      for I := 0 to KSize - 1 do
        BE[I] := BE[I] xor $5C;
      J := 0;
      while J < KeyLen do begin
        H.SetUp;
        H.HashData(BS,KSize);
        H.HashData(P.KeyBytes[J],H.DigestSize);
        H.HashData(Nonce,NonceLen);
        H.HashData(Password,PWLen);
        H.Done(D.Key);
        H.SetUp;
        H.HashData(BE,KSize);
        H.HashData(D.Key^,H.DigestSize);
        H.Done(@P.KeyBytes[J]);
        Inc(J,H.DigestSize);
      end;
    finally
      ProtectClear(BS,SizeOf(BS));
      ProtectClear(BE,SizeOf(BE));
    end;
    Move(P.Key^,Key,KeyLen);
  finally
    H.Free;
  end;
end;

procedure WeakPasswordKDFv2(const Password: string;
                            const Nonce: string;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            var Key; KeyLen: Integer);
begin
  WeakPasswordKDFv2(Pointer(Password)^,Length(Password),
                    Pointer(Nonce)^,Length(Nonce),
                    Iter,
                    HashAlg,
                    Key,KeyLen);
end;

procedure WeakPasswordKDFv2(const Password; PWLen: Integer;
                            const Nonce; NonceLen: Integer;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            KeyLen: Integer;
                            var Key: ISecretKey);
begin
  Key := TSecretKey.Create('');
  Key.SetLength(KeyLen);
  WeakPasswordKDFv2(Password,PWLen,Nonce,NonceLen,Iter,HashAlg,Key.Key^,Key.KeyLen);
end;

procedure WeakPasswordKDFv2(Password: ISecretKey;
                            Nonce: ISecretKey;
                            Iter: Integer;
                            HashAlg: THashAlgorithm;
                            KeyLen: Integer;
                            var Key: ISecretKey);
begin
  Key := TSecretKey.Create('');
  Key.SetLength(KeyLen);
  WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                    Nonce.Key^,Nonce.KeyLen,
                    Iter,
                    HashAlg,
                    Key.Key^,KeyLen);
end;

procedure WeakPasswordKDFv2Str(Password: PChar;
                               Nonce: PChar;
                               Iter: Integer;
                               HashAlg: THashAlgorithm;
                               Key: PChar; KeyLen: Integer);
begin
  WeakPasswordKDFv2(Password^,StrLen(Password),
                    Nonce^,StrLen(Nonce),
                    Iter,
                    HashAlg,
                    Key^,KeyLen);
end;

procedure HMACBegin(const K; KLen: Integer; H: THash);
var
  K0: packed array [0..127] of Byte;
  I, KSize: Integer;
begin
  FillChar(K0,SizeOf(K0),0);
  try             
{$IFDEF SHA512}
    if H.Algorithm in [haSHA384,haSHA512] then
      KSize := 128
    else
{$ENDIF SHA512}
{$IFDEF MD2}
    if H.Algorithm = haMD2 then
      KSize := 16
    else
{$ENDIF MD2}
      KSize := 64;
    if KLen <= KSize then
      Move(K,K0,KLen)
    else begin
      H.SetUp;
      H.HashData(K,KLen);
      H.Done(@K0);
    end;
    for I := 0 to KSize - 1 do
      K0[I] := K0[I] xor $36;
    H.SetUp;
    H.HashData(K0,KSize);
  finally
    ProtectClear(K0,SizeOf(K0));
  end;
end;

procedure HMACEnd(const K; KLen: Integer; H: THash; var Mac; MacLen: Integer);
var
  K0: packed array [0..191] of Byte;
  I, KSize: Integer;
begin
  FillChar(K0,SizeOf(K0),0);
  try                 
{$IFDEF SHA512}
    if H.Algorithm in [haSHA384,haSHA512] then
      KSize := 128
    else
{$ENDIF SHA512}
{$IFDEF MD2}
    if H.Algorithm = haMD2 then
      KSize := 16
    else
{$ENDIF MD2}
      KSize := 64;
    H.Done(@K0[KSize]);
    if KLen <= KSize then
      Move(K,K0,KLen)
    else begin
      H.SetUp;
      H.HashData(K,KLen);
      H.Done(@K0);
    end;
    for I := 0 to KSize - 1 do
      K0[I] := K0[I] xor $5C;
    H.SetUp;
    H.HashData(K0,KSize + H.DigestSize);
    H.Done(@K0);
    if MacLen < H.DigestSize then
      Move(K0,Mac,MacLen)
    else begin
      FillChar(Mac,MacLen,0);
      Move(K0,Mac,H.DigestSize);
    end;
  finally
    ProtectClear(K0,SizeOf(K0));
  end;
end;

procedure HMAC(const K; KLen: Integer;
               const M; MLen: Integer;
               HashAlgorithm: THashAlgorithm;
               var Mac; MacLen: Integer);
var
  HC: THashClass;
  H: THash;
begin
  HC := FindHashClass(HashAlgorithm);
  if HC = nil then
    raise Exception.Create('Algorithm not supported');
  H := HC.Create(nil^,0);
  try
    HMACBegin(K,KLen,H);
    H.HashData(M,MLen);
    HMACEnd(K,KLen,H,Mac,MacLen);
  finally
    H.Free;
  end;
end;

procedure HMAC(K: ISecretKey;
               const M; MLen: Integer;
               HashAlgorithm: THashAlgorithm;
               MacLen: Integer; var Mac: ISecretKey);
var
  HC: THashClass;
  H: THash;
begin
  HC := FindHashClass(HashAlgorithm);
  if HC = nil then
    raise Exception.Create('Algorithm not supported');
  H := HC.Create(nil^,0);
  try
    HMACBegin(K.Key^,K.KeyLen,H);
    H.HashData(M,MLen);
    if Mac = nil then Mac := TSecretKey.Create('');
    Mac.SetLength(MacLen);
    HMACEnd(K.Key^,K.KeyLen,H,Mac.Key^,MacLen);
  finally
    H.Free;
  end;
end;

function TrimHex(const Value: string): string;
var
  P: PChar;
  S: string;
begin
  P := Pointer(Value);
  S := '';
  while (P <> nil) and (P^ <> #0) do begin
    if P^ = #32 then begin
      while Length(S) > 1 do begin
        Result := Result + Copy(S,Length(S)-1,2);
        Delete(S,Length(S)-1,2);
      end;
      if Length(S) = 1 then
        Result := Result + '0' + S;
    end else
      S := S + P^;
    Inc(P);
  end;
end;

function GetTrimmedHex(Strings: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Strings.Count - 1 do
    Result := Result + TrimHex(Strings[I]);
end;

procedure ProtectClear(var ABuf; Count: Integer);
begin
  if Count <= 0 then Exit;
  FillChar(ABuf,Count,$FF);
  FillChar(ABuf,Count,$AA);
  FillChar(ABuf,Count,$55);
  FillChar(ABuf,Count,$00);
end;

function BinToHex(const ABuf; Count: Integer): string;
var
  P: ^Byte;
begin
  Result := '';
  P := @ABuf;
  while Count > 0 do begin
    Result := Result + IntToHex(P^,2);
    Inc(LongInt(P));
    Dec(Count);
  end;
end;

procedure HexToBin(const Value: string; var ABuf; var Count: Integer);
var
  P: ^Byte;
  J: Integer;
begin
  Trim(Value);
  if Count < (Length(Value) div 2) then
    raise Exception.Create(S_ERR_BUF_TOO_SMALL);
  FillChar(ABuf,Count,0);
  P := @ABuf;
  J := 1;
  Count := 0;
  while J < Length(Value) do begin
    while (J < Length(Value)) and (Value[J] = ' ') do
      Inc(J);
    P^ := StrToInt('$'+Copy(Value,J,2));
    Inc(LongInt(P));
    Inc(Count);
    Inc(J,2);
  end;
end;

procedure XORBytes(var Dest; const Source; Count: Integer);
asm
  push EBX
  push EDI

  mov EDI,ECX
  shr EDI,2
  jz @@2

  lea EDX,[EDX + EDI*4]
  lea EAX,[EAX + EDI*4]
  neg EDI
@@1:
  mov EBX,dword [EDX + EDI*4]
  xor dword [EAX + EDI*4],EBX
  inc EDI
  jnz @@1
@@2:
  and ECX,3
  jz @@4
@@3:
  mov BL,byte [EDX]
  xor byte [EAX],BL
  inc EAX
  inc EDX
  dec ECX
  jnz @@3
@@4:
  pop EDI
  pop EBX
end;

procedure XORBytesBackup(var Dest; const Source; var Backup; Count: Integer);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov EDI,ECX

  mov ECX,Count
  mov ESI,ECX
  shr ESI,2
  jz @@2

  lea EDI,[EDI + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:
  mov EBX,dword [EAX + ESI*4]
  mov EBP,dword [EDX + ESI*4]
  mov dword [EDI + ESI*4],EBX
  xor dword [EAX + ESI*4],EBP
  inc ESI
  jnz @@1
@@2:
  and ECX,3
  jz @@4
@@3:
  mov BL,byte [EAX + ECX - 1]
  mov BH,byte [EDX + ECX - 1]
  mov byte [EDI + ECX - 1],BL
  xor byte [EAX + ECX - 1],BH
  dec ECX
  jnz @@3
@@4:
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure XORBytes2Backup(var Dest; const Src1; var Src2; Count: Integer);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov ESI,Count
  push ESI
  shr ESI,2
  jz @@2

  lea ECX,[ECX + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:
  mov EBP,dword [EAX + ESI*4]
  mov EBX,dword [EDX + ESI*4]
  xor EBX,dword [ECX + ESI*4]
  mov dword [ECX + ESI*4],EBP
  mov dword [EAX + ESI*4],EBX
  inc ESI
  jnz @@1
@@2:
  pop ESI
  and ESI,3
  jz @@4
@@3:
  mov BL,byte [EAX + ESI - 1]
  mov BH,byte [EDX + ESI - 1]
  xor BH,byte [ECX + ESI - 1]
  mov byte [ECX + ESI - 1],BL
  mov byte [EAX + ESI - 1],BH
  dec ESI
  jnz @@3
@@4:
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure XORBytesABCDcr(var MsgText; var H; const PrevH; var PrevC; Count: Integer);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov EDI,PrevC

  mov ESI,Count
  push ESI
  shr ESI,2
  jz @@2

  lea ECX,[ECX + ESI*4]
  lea EDI,[EDI + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:                         
  mov EBP,dword [EAX + ESI*4]
  mov EBX,dword [EDI + ESI*4]
  xor EBX,dword [EDX + ESI*4]
  mov dword [EDX + ESI*4],EBX
  xor EBX,dword [ECX + ESI*4]
  mov dword [EDI + ESI*4],EBP
  mov dword [EAX + ESI*4],EBX
  inc ESI
  jnz @@1
@@2:
  pop ESI
  and ESI,3
  jz @@4
@@3:
  mov BL,byte [EAX + ESI - 1]
  mov BH,byte [EDX + ESI - 1]
  xor BH,byte [EDI + ESI - 1]
  mov byte [EDX + ESI - 1],BH
  xor BH,byte [ECX + ESI - 1]
  mov byte [EDI + ESI - 1],BL
  mov byte [EAX + ESI - 1],BH
  dec ESI
  jnz @@3
@@4:
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure XORBytesCopy(var Dest; const Source; var Copy; Count: Integer);
asm
  push EBX
  push EDI
  push ESI

  mov EDI,ECX

  mov ECX,Count
  mov ESI,ECX
  shr ESI,2
  jz @@2

  lea EDI,[EDI + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:
  mov EBX,dword [EDX + ESI*4]
  xor EBX,dword [EAX + ESI*4]
  mov dword [EDI + ESI*4],EBX
  mov dword [EAX + ESI*4],EBX
  inc ESI
  jnz @@1
@@2:
  and ECX,3
  jz @@4
@@3:
  mov BL,byte [EDX + ECX - 1]
  xor BL,byte [EAX + ECX - 1]
  mov byte [EDI + ECX - 1],BL
  mov byte [EAX + ECX - 1],BL
  dec ECX
  jnz @@3
@@4:
  pop ESI
  pop EDI
  pop EBX
end;

procedure XORBlock(var Dest; const Source; LWCount: Integer);
asm
  push EBX
  push EDI
  shr ECX,1
  lea EDI,[EAX + ECX*8]
  lea EDX,[EDX + ECX*8]
  neg ECX
@@Main:
  mov EBX,dword [EDX + ECX*8]
  mov EAX,dword [EDX + ECX*8 + 4]
  xor dword [EDI + ECX*8],EBX
  xor dword [EDI + ECX*8 + 4],EAX
  inc ECX
  jnz @@Main
  pop EDI
  pop EBX
end;

procedure XORBlock128(var Dest; const Source);
asm
  mov ECX,[EDX]
  xor [EAX],ECX
  mov ECX,[EDX + 4]
  xor [EAX + 4],ECX
  mov ECX,[EDX + 8]
  xor [EAX + 8],ECX
  mov ECX,[EDX + 12]
  xor [EAX + 12],ECX
end;           

procedure XORBlock64(var Dest; const Source);
asm
  mov ECX,[EDX]
  xor [EAX],ECX
  mov ECX,[EDX + 4]
  xor [EAX + 4],ECX
end;

procedure AddBlock128(var Dest; const Source);
asm
  mov ECX,[EDX]
  add [EAX],ECX
  mov ECX,[EDX + 4]
  adc [EAX + 4],ECX
  mov ECX,[EDX + 8]
  adc [EAX + 8],ECX
  mov ECX,[EDX + 12]
  adc [EAX + 12],ECX
end;

procedure AddBlock64(var Dest; const Source);
asm
  mov ECX,[EDX]
  add [EAX],ECX
  mov ECX,[EDX + 4]
  adc [EAX + 4],ECX
end;               

procedure IncBlock(var Buf; LWCount: Integer);
asm
  push EBX
  lea EAX,[EAX + EDX*4 - 4]
  mov ECX,1
  neg EDX
@@Main:
  mov EBX,dword [EAX]
  bswap EBX
  add EBX,ECX
  mov CL,CH
  setc CL
  bswap EBX
  mov dword [EAX],EBX
  sub EAX,4
  inc EDX
  jnz @@Main
  pop EBX
end;

procedure IncBlockByInt(var Dst; const Src; LWCount: Integer; Value: LongInt);
asm
  push EBX
  push ESI
  lea EAX,[EAX + ECX*4 - 4]
  lea ESI,[EDX + ECX*4 - 4]
  mov EDX,Value
  neg ECX
@@Main:
  mov EBX,dword [ESI]
  bswap EBX
  add EBX,EDX
  mov EDX,0
  setc DL
  bswap EBX
  mov dword [EAX],EBX
  sub EAX,4
  inc ECX
  jnz @@Main
  pop ESI
  pop EBX
end;

{ TCipher }

class function TCipher.Algorithm: TCipherAlg;
begin
  Result := caUnknown;
end;

class function TCipher.AlgorithmName: PChar;
begin
  Result := nil;
end;

class function TCipher.BlockSize: Integer;
begin
  Result := -1;
end;

class function TCipher.Mode: TCipherMode;
begin
  Result := cmECB;
end;

constructor TCipher.Create(const AKey; Count, VectorSize: Integer);
begin
  VirtualLock;
  SetUp(AKey,Count,VectorSize);
end;

constructor TCipher.CreateIntf(ASecretKey: ISecretKey);
var
  vOID: string;
begin
  vOID := ASecretKey.Algorithm;
  if vOID = '' then
    Create(ASecretKey.Key^,ASecretKey.KeyLen,ASecretKey.VectorSize)
  else if OID(ASecretKey.KeyLen) = vOID then
    Create(ASecretKey.Key^,ASecretKey.KeyLen,0)
  else if KeyedIVOID(ASecretKey.KeyLen - BlockSize * ASecretKey.VectorSize) =
          vOID then
    Create(ASecretKey.Key^,ASecretKey.KeyLen,ASecretKey.VectorSize)
  else
    Assert(False,'TCipher.CreateIntf: Algorithm mismatch');
end;

procedure TCipher.Decrypt(var Buf; Count: Integer);
begin
  DecryptToBuf(Buf,Buf,Count);
end;

procedure TCipher.DecryptIntf(Buf: ISecretKey);
begin
  Decrypt(Buf.Key^,Buf.KeyLen);
end;

procedure TCipher.DecryptPtr(Buf: Pointer; Count: Integer);
begin
  Decrypt(Buf^,Count);
end;

function TCipher.DecryptStr(const Buf: OctetString): string;
var
  Tmp: ISecretKey;
  PadLen: Byte;
  I: Integer;
begin
  if Buf = '' then
    Result := ''
  else if (BlockSize = 1) or
          (Mode in [cmCFB,cmOFB,cmCTR,cmPCFB]) or
          ((Length(Buf) mod BlockSize) > 0) then begin
    Result := Buf;
    UniqueString(Result);
    Decrypt(Pointer(Result)^,Length(Result));
  end else begin
    Tmp := TSecretKey.CreateStr(PChar(Buf),Length(Buf));
    Decrypt(Tmp.Key^,Tmp.KeyLen);
    PadLen := Tmp.KeyBytes^[Tmp.KeyLen - 1];
    if (PadLen > 0) and (PadLen <= BlockSize) then begin
      for I := Tmp.KeyLen - 1 downto Tmp.KeyLen - PadLen do
        if Tmp.KeyBytes^[I] <> PadLen then begin
          PadLen := 0;
          Break;
        end
    end else
      PadLen := 0;
    SetLength(Result,Tmp.KeyLen - PadLen);
    Move(Tmp.Key^,Pointer(Result)^,Tmp.KeyLen - PadLen);
  end;
end;

procedure TCipher.DecryptToBuf(const Src; var Dst; Count: Integer);
begin
  Move(Src,Dst,Count);
  Decrypt(Dst,Count);
end;

procedure TCipher.DecryptToPtr(Src, Dst: Pointer; Count: Integer);
begin
  Move(Src^,Dst^,Count);
  Decrypt(Dst^,Count);
end;

destructor TCipher.Destroy;
begin
  CleanUp;
  VirtualUnlock;
  inherited;
end;

procedure TCipher.Encrypt(var Buf; Count: Integer);
begin
  EncryptToBuf(Buf,Buf,Count);
end;

procedure TCipher.EncryptIntf(Buf: ISecretKey);
begin
  Encrypt(Buf.Key^,Buf.KeyLen);
end;

procedure TCipher.EncryptPtr(Buf: Pointer; Count: Integer);
begin
  Encrypt(Buf^,Count);
end;

function TCipher.EncryptStr(const Buf: string): OctetString;
var
  PadLen: Integer;
begin
  if (BlockSize > 1) and not (Mode in [cmCFB,cmOFB,cmCTR,cmPCFB]) then begin
    PadLen := BlockSize - (Length(Buf) mod BlockSize);
    SetLength(Result,Length(Buf) + PadLen);
    Move(Pointer(Buf)^,Pointer(Result)^,Length(Buf));
    FillChar(Result[Length(Result) - PadLen + 1],PadLen,Byte(PadLen));
    Encrypt(Pointer(Result)^,Length(Result));
  end else begin
    Result := Buf;
//    UniqueString(Result);
    if Buf <> '' then
      Encrypt(Result[1],Length(Result));
  end;
end;

procedure TCipher.EncryptToBuf(const Src; var Dst; Count: Integer);
begin
  Move(Src,Dst,Count);
  Encrypt(Dst,Count);
end;

procedure TCipher.EncryptToPtr(Src, Dst: Pointer; Count: Integer);
begin
  Move(Src^,Dst^,Count);
  Encrypt(Dst^,Count);
end;

procedure TCipher.GetVectorBuf(var IV; Count: Integer);
var
  Str: string;
begin
  Str := IVector;
  if Length(Str) < Count then
    Move(Pointer(Str)^,IV,Length(Str))
  else
    Move(Pointer(Str)^,IV,Count);
  ProtectClear(Pointer(Str)^,Length(Str));
end;

class function TCipher.KeyedIVOID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

procedure TCipher.Nag(var Count: Integer);
begin
{$IFDEF DEMO}
  if False {Count > 16} then begin
    Count := 16;
    MessageBox(0,'This application was built using StreamSec StrSecII components'#13#10 +
                  'Copyright 2002 (c) StreamSec'#13#10 +
                  'You MUST purchase the registred version to use keys larger than 128 bits.',
                 'StreamSec',MB_OK);
  end;
{$ENDIF}
end;

class function TCipher.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

procedure TCipher.SetVectorBuf(const IV; Count: Integer);
var
  Str: string;
begin
  SetLength(Str,Count);
  Move(IV,Pointer(Str)^,Count);
  IVector := Str;
  ProtectClear(Pointer(Str)^,Count);
end;

procedure TCipher.VirtualLock;
begin
  {$IFDEF WIN32}
  Windows.VirtualLock(Self,InstanceSize);
  {$ENDIF}
end;

procedure TCipher.VirtualUnlock;
begin
  {$IFDEF WIN32}
  Windows.VirtualUnlock(Self,InstanceSize);
  {$ENDIF}
end;

constructor TCipher.CreateClone(ACipher: TCipher);
begin
  raise Exception.Create(Format(S_ERR_CREATE_CLONE,[ClassName,ACipher.ClassName]));
end;

procedure TCipher.SetUpIntf(ASecretKey: ISecretKey);
var
  vOID: string;
begin
  vOID := ASecretKey.Algorithm;
  if vOID = '' then
    SetUp(ASecretKey.Key^,ASecretKey.KeyLen,ASecretKey.VectorSize)
  else if OID(ASecretKey.KeyLen) = vOID then
    SetUp(ASecretKey.Key^,ASecretKey.KeyLen,0)
  else if KeyedIVOID(ASecretKey.KeyLen - BlockSize * ASecretKey.VectorSize) =
          vOID then
    SetUp(ASecretKey.Key^,ASecretKey.KeyLen,ASecretKey.VectorSize)
  else
    Assert(False,'TCipher.SetUpIntf: Algorithm mismatch');
end;

function TCipher.Clone: TCipher;
begin
  Result := TCipherClass(ClassType).CreateClone(Self);
end;

{ TPRGTest }

procedure TPRGTest.CleanUp;
begin
  // Nothing to do.
end;

procedure TPRGTest.Decrypt(var Buf; Count: Integer);
begin
  // Nothing to do.
end;

procedure TPRGTest.Encrypt(var Buf; Count: Integer);
var
  P: ^Byte;
  I: Integer;
begin
  P := @Buf;
  for I := 0 to Count-1 do begin
    P^ := Random(256);
    Inc(LongInt(P));
  end;
end;

function TPRGTest.GetVector: OctetString;
begin
  Result := '';
end;

class function TPRGTest.Mode: TCipherMode;
begin
  Result := cmECB;
end;

procedure TPRGTest.SetUp(const AKey; Count, VectorSize: Integer);
begin
  // Nothing to do.
end;

procedure TPRGTest.SetVector(const Value: OctetString);
begin
  // Nothing to do.
end;

{ TBlockCipher }

function TBlockCipher.AllocBuffer: PByteArray;
begin
  GetMem(Result,BlockSize);
end;

class function TBlockCipher.BlockVectorSize: Integer;
begin
  Result := -1;
end;

procedure TBlockCipher.CleanUp;
begin
  ProtectClear(FIV^,BlockSize * BlockVectorSize);
end;

constructor TBlockCipher.Create(const AKey; Count, VectorSize: Integer);
begin
  TestSecureSetUp(AKey,Count,VectorSize);
end;

class function TBlockCipher.CreateAnother(const AKey; Count,
  VectorSize: Integer): TBlockCipher;
begin
  Result := Self.Create(AKey,Count,VectorSize);
end;

constructor TBlockCipher.CreateIntf(ASecretKey: ISecretKey);
begin
  if (ASecretKey.VectorSize <= 0) or (ASecretKey.VectorSize = BlockVectorSize) then
    inherited CreateIntf(ASecretKey)
  else
    Assert(False,'TBlockCipher.CreateIntf: Wrong VectorSize');
end;

procedure TBlockCipher.DeallocBuffer(var T: PByteArray);
begin
  FreeMem(T);
end;

procedure TBlockCipher.Decrypt(var Buf; Count: Integer);
var
  I, BlockLen, C: Integer;
  PT: PByteArray;
begin
  case Mode of
    cmECB:
      begin
        BlockLen := BlockSize;
        PT := Addr(Buf);
        C := Count div BlockLen;
        for I := 0 to C-1 do begin
          DecryptBlock(PT[0]);
          PT := Addr(PT[BlockLen]);
        end;
      end;
    cmCBC:
      Decrypt_CBC(Buf,Count);
    cmCFB:
      Decrypt_CFB(Buf,Count);
    cmCTR:
      Encrypt_CTR(Buf,Count); // Encrypt_CTR = Decrypt_CTR
    cmOFB:
      Encrypt_OFB(Buf,Count); // Encrypt_OFB = Decrypt_OFB
    cmABC:
      Decrypt_ABC(Buf,Count);
  else
    Assert(False,'Cipher mode not supported');
  end;
end;

procedure TBlockCipher.DecryptBlock(var Buf);
begin
  DecryptBlockToDst(Buf,Buf);
end;

procedure TBlockCipher.DecryptBlockToDst(var Dst; const Src);
begin
  Move(Src,Dst,BlockSize);
  DecryptBlock(Dst);
end;

procedure TBlockCipher.Decrypt_ABC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  CT, PT, HT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  CT := Addr(FIV[BlockLen]);
  HT := Addr(FIV[BlockLen*2]);
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    // A[i] := D(C[i] xor H[i-1])
    XORBytesBackup(FIV[0],PT[0],HT[0],BlockLen);
    DecryptBlock(FIV[0]);
    // H[i] := A[i] xor C[i-1]
    // P[i] := H[i] xor H[i-1]
    XORBytesABCDcr(PT[0],FIV[0],HT[0],CT[0],BlockLen);
    PT := Addr(PT[BlockLen]);
  end;          
  C := Count mod BlockLen;
  if C > 0 then begin
    XORBlock(FIV[0],CT[0],BlockLWLen);
    EncryptBlock(FIV[0]);
    for I := 0 to C - 1 do
      PT[I] := PT[I] xor FIV[I];
  end;
end;

procedure TBlockCipher.Decrypt_CBC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  CT, PT, PrevCT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  CT := AllocBuffer;
  try
    C := Count div (2*BlockLen);
    PrevCT := Addr(FIV[0]);
    for I := 0 to C - 1 do begin
      Move(PT[0],CT[0],BlockLen);
      DecryptBlock(PT[0]);
      XORBlock(PT[0],PrevCT[0],BlockLWLen);      
      PT := Addr(PT[BlockLen]);
      Move(PT[0],PrevCT[0],BlockLen);
      DecryptBlock(PT[0]);
      XORBlock(PT[0],CT[0],BlockLWLen);
      PT := Addr(PT[BlockLen]);
    end;
    C := Count mod (2*BlockLen);
    if C >= BlockLen then begin
      Move(PT[0],CT[0],BlockLen);
      DecryptBlock(PT[0]);
      XORBlock(PT[0],PrevCT[0],BlockLWLen);
      Move(CT[0],FIV[0],BlockLen);     
      PT := Addr(PT[BlockLen]);
      C := C - BlockLen;
    end;
    if C > 0 then begin
      EncryptBlock(FIV[0]);
      for I := 0 to C - 1 do
        PT[I] := PT[I] xor FIV[I];
    end;
  finally
    DeallocBuffer(CT);
  end;
end;

procedure TBlockCipher.Decrypt_CFB(var Buf; Count: Integer);
var
  I, K, BlockLen, C: Integer;
  PT, CT: PByteArray;
begin
  BlockLen := BlockSize;
  PT := Addr(Buf);
  CT := AllocBuffer;
  try
    if ModeRatio = BlockSize then begin
      if FBIndex > 0 then begin
        C := BlockLen;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        XORBytesBackup(PT[0],FIV[FBIndex],FIV[FBIndex],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod BlockLen;
      end;
      C := Count div BlockLen;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesBackUp(PT[0],CT[0],FIV[0],BlockLen);
        PT := Addr(PT[BlockLen]);
        FFBIndex := 0;
      end;
      C := Count mod BlockLen;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesBackup(PT[0],CT[0],FIV[0],C);
        Move(CT[C],FIV[C],BlockLen - C);
        FFBIndex := C;
      end;
    end else begin
      if FBIndex > 0 then begin
        C := ModeRatio;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        K := BlockLen - ModeRatio + FBIndex;
        XORBytesBackup(PT[0],FIV[K],FIV[K],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod ModeRatio;
      end;
      C := Count div ModeRatio;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesBackUp(PT[0],CT[0],FIV[BlockLen - ModeRatio],ModeRatio);
        PT := Addr(PT[ModeRatio]);
        FFBIndex := 0;
      end;
      C := Count mod ModeRatio;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesBackUp(PT[0],CT[0],FIV[BlockLen - ModeRatio],C);
        Move(CT[C],FIV[BlockLen - ModeRatio + C],ModeRatio - C);
        FFBIndex := C;
      end;
    end;
  finally
    DeallocBuffer(CT);
  end;
end;

procedure TBlockCipher.Encrypt(var Buf; Count: Integer);
var
  I, BlockLen, C: Integer;
  PT: PByteArray;
begin
  case Mode of
    cmECB:
      begin
        BlockLen := BlockSize;
        PT := Addr(Buf);
        C := Count div BlockLen;
        for I := 0 to C-1 do begin
          EncryptBlock(PT[0]);
          PT := Addr(PT[BlockLen]);
        end;
      end;
    cmCBC:
      Encrypt_CBC(Buf,Count);
    cmCFB:
      Encrypt_CFB(Buf,Count);
    cmCTR:
      Encrypt_CTR(Buf,Count);
    cmOFB:
      Encrypt_OFB(Buf,Count);
    cmABC:
      Encrypt_ABC(Buf,Count);
  else
    Assert(False,'Cipher mode not supported');
  end;
end;

procedure TBlockCipher.EncryptBlock(var Buf);
begin
  EncryptBlockToDst(Buf,Buf);
end;

procedure TBlockCipher.EncryptBlockToDst(var Dst; const Src);
begin
  Move(Src,Dst,BlockSize);
  EncryptBlock(Dst);
end;

procedure TBlockCipher.Encrypt_ABC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  CT, PT, HT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  CT := Addr(FIV[BlockLen]);
  HT := Addr(FIV[BlockLen*2]);
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    // H[i] := P[i] xor H[i-1]
    XORBytesBackup(FIV[0],PT[0],HT[0],BlockLen);
    // C[i] := E(C[i-1] xor H[i]) xor H[i-1]
    XORBytes(CT[0],FIV[0],BlockLen);
    EncryptBlock(CT[0]);
    XORBytesCopy(CT[0],HT[0],PT[0],BlockLen);
    PT := Addr(PT[BlockLen]);
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    XORBlock(FIV[0],CT[0],BlockLWLen);
    EncryptBlock(FIV[0]);
    for I := 0 to C - 1 do
      PT[I] := PT[I] xor FIV[I];
  end;
end;

procedure TBlockCipher.Encrypt_CBC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  PT, PrevCT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  C := Count div BlockLen;
  PrevCT := Addr(FIV[0]);
  for I := 0 to C - 1 do begin
    XorBlock(PT[0],PrevCT[0],BlockLWLen);
    EncryptBlock(PT[0]);
    PrevCT := PT;
    PT := Addr(PT[BlockLen]);
  end;
  Move(PrevCT[0],FIV[0],BlockLen);
  C := Count mod BlockLen;
  if C > 0 then begin
    EncryptBlock(FIV[0]);
    for I := 0 to C - 1 do
      PT[I] := PT[I] xor FIV[I];
  end;
end;

procedure TBlockCipher.Encrypt_CFB(var Buf; Count: Integer);
var
  I, K, BlockLen, C: Integer;
  PT, CT: PByteArray;
begin
  BlockLen := BlockSize;
  PT := Addr(Buf);
  CT := AllocBuffer;
  try
    if ModeRatio = BlockSize then begin
      if FBIndex > 0 then begin
        C := BlockLen;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        XORBytesCopy(PT[0],FIV[FBIndex],FIV[FBIndex],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod BlockLen;
      end;
      C := Count div BlockLen;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesCopy(PT[0],CT[0],FIV[0],BlockLen);
        PT := Addr(PT[BlockLen]);
        FFBIndex := 0;
      end;
      C := Count mod BlockLen;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesCopy(PT[0],CT[0],FIV[0],C);
        Move(CT[C],FIV[C],BlockLen - C);
        FFBIndex := C;
      end;
    end else begin
      if FBIndex > 0 then begin
        C := ModeRatio;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        K := BlockLen - ModeRatio + FBIndex;
        XORBytesCopy(PT[0],FIV[K],FIV[K],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod ModeRatio;
      end;
      C := Count div ModeRatio;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesCopy(PT[0],CT[0],FIV[BlockLen - ModeRatio],ModeRatio);
        PT := Ptr(Integer(PT) + ModeRatio);
        FFBIndex := 0;
      end;
      C := Count mod ModeRatio;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesCopy(PT[0],CT[0],FIV[BlockLen - ModeRatio],C);
        Move(CT[C],FIV[BlockLen - ModeRatio + C],ModeRatio - C);
        FFBIndex := C;
      end;
    end;
  finally
    DeallocBuffer(CT);
  end;
end;

procedure TBlockCipher.Encrypt_CTR(var Buf; Count: Integer);
var
  I, BlockLen, BlockLWLen, C: Integer;
  PT, CT: PByteArray;
begin
  // IMPORTANT: Classes that use this method MUST allocate two blocks for FIV,
  // despite that BlockVectorSize MUST return 1 for CTR classes.
  BlockLen := ModeRatio;
  if BlockLen = 0 then
    BlockLen := BlockSize;
  BlockLWLen := BlockSize shr 2;
  PT := Addr(Buf);
  CT := Addr(FIV[BlockSize]);
  if FBIndex > 0 then begin
    C := BlockLen;
    if C > Count + FBIndex then
      C := Count + FBIndex;
    XORBytes(PT[0],CT[FBIndex],C - FBIndex);
    PT := Addr(PT[C - FBIndex]);
    Count := Count - C + FBIndex;
    FFBIndex := C mod BlockLen;
  end;
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    IncBlock(FIV[0],BlockLWLen);
    EncryptBlockToDst(CT[0],FIV[0]);
    XORBytes(PT[0],CT[0],BlockLen);
    PT := Addr(PT[BlockLen]);
    FFBIndex := 0;
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    IncBlock(FIV[0],BlockLWLen);
    EncryptBlockToDst(CT[0],FIV[0]);
    XORBytes(PT[0],CT[0],C);
    FFBIndex := C;
  end;
end;

procedure TBlockCipher.Encrypt_OFB(var Buf; Count: Integer);
var
  I, BlockLen, C: Integer;
  PT: PByteArray;
begin
  BlockLen := ModeRatio;
  PT := Addr(Buf);
  if FBIndex > 0 then begin
    C := BlockLen - FBIndex;
    if C > Count then
      C := Count;
    XORBytes(PT[0],FIV[FBIndex],C);
    PT := Addr(PT[C]);
    Count := Count - C;
    FFBIndex := (C + FBIndex) mod BlockLen;
  end;
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    EncryptBlock(FIV[0]);
    XORBytes(PT[0],FIV[0],BlockLen);
    PT := Addr(PT[BlockLen]);
    FFBIndex := 0;
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    EncryptBlock(FIV[0]);
    XORBytes(PT[0],FIV[0],C);
    FFBIndex := C;
  end;
end;

function TBlockCipher.GetVector: OctetString;
begin
  SetLength(Result,BlockSize * BlockVectorSize);
  Move(FIV^,Result[1],Length(Result));
end;

procedure TBlockCipher.GetVectorBuf(var IV; Count: Integer);
begin
  if Count > BlockSize * BlockVectorSize then
    Move(FIV^,IV,BlockSize * BlockVectorSize)
  else
    Move(FIV^,IV,Count);
end;

class function TBlockCipher.MaxKeySize: Integer;
begin
  Result := -1;
end;

class function TBlockCipher.MinKeySize: Integer;
begin
  Result := -1;
end;

procedure TBlockCipher.SetModeRatio(Value: Integer);
begin
  if (Value < 1) or (Value >= BlockSize) then begin
    if Mode = cmPCFB then
      Value := BlockSize shr 1
    else
      Value := BlockSize;
  end;
  FModeRatio := Value;
end;

procedure TBlockCipher.SetVector(const Value: OctetString);
begin
  if Length(Value) > BlockSize * BlockVectorSize then
    Move(Pointer(Value)^,FIV^,BlockSize * BlockVectorSize)
  else if Length(Value) = BlockSize * BlockVectorSize then
    Move(Pointer(Value)^,FIV^,Length(Value))
  else begin
    ProtectClear(FIV^,BlockSize * BlockVectorSize);
    Move(Pointer(Value)^,FIV^,Length(Value));
  end;
  FFBIndex := 0;
end;

procedure TBlockCipher.SetVectorBuf(const IV; Count: Integer);
begin
  if Count > BlockSize * BlockVectorSize then
    Move(IV,FIV^,BlockSize * BlockVectorSize)
  else if Count = BlockSize * BlockVectorSize then
    Move(IV,FIV^,Count)
  else begin
    ProtectClear(FIV^,BlockSize * BlockVectorSize);
    Move(IV,FIV^,Count);
  end;          
  FFBIndex := 0;
end;

procedure TBlockCipher.TestSecureSetUp(const AKey; Count, VectorSize: Integer);
{$IFDEF SHA1}
var
  D, K: string;
  I, J: Integer;
  SetIV: Boolean;
  H: TSHA1;
  KG: TBlockCipher;
{$ENDIF SHA1}
begin
  if VectorSize < 0 then begin
{$IFDEF SHA1}
    // Secure SetUp:
    // The actual key (and IV) are generated from AKey using a combination of
    // SHA1 to produce a sequence of hash values, and the
    // cipher in CTR mode with the hash values as key:
    //
    // D0 <- H(0|AKey), Di <- H(D(i-1)|AKey), D <- D0|D1|...
    // K <- X|E_D(n-1)|...|E_D(2)|E_D(1),
    // where X is E_D(n) if the length of IV|K is divisible by BlockSize,
    // otherwise X is E_D(n+1|Y1)|Y0, where Y1|Y0 = E_D(n).
    //
    // IMPORTANT: If AKey is a passphrase, it ought to be salted by the
    // application, i.e. pass Salt|Passphrase as AKey to the constructor or
    // this method.
    I := MaxKeySize;
    if (I mod 20) <> 0 then
      I := I + 20 - (I mod 20);
    K := StringOfChar(#0,I);
    D := StringOfChar(#0,I);
    while (I > 0) do begin
      H := TSHA1.Create(D[1],20);
      try
        H.HashData(AKey,Count);
        H.Done(Pointer(D));
        Dec(I,20);
        Move(D[1],K[I + 1],20);
      finally
        H.Free;
      end;
    end;
    ProtectClear(D[1],Length(D));
    KG := CreateAnother(K[1],MaxKeySize,0);
    try
      ProtectClear(K[1],Length(K));
      if VectorSize = -1 then begin
        {generate MinKeySize}
        I := MinKeySize + BlockVectorSize * BlockSize;
        SetIV := True;
      end else if (-VectorSize >= MinKeySize) and (-VectorSize <= MaxKeySize) then begin
        {generate custom key size, set IV to binary zero}
        I := -VectorSize;
        SetIV := False;
      end else begin
        {generate MaxKeySize}
        I := MaxKeySize + BlockVectorSize * BlockSize;
        SetIV := True;
      end;
      if I < BlockSize then
        I := BlockSize;
      K := StringOfChar(#0,I);
      J := 1;
      repeat
        Dec(I,BlockSize);
        if I < 0 then
          I := 0;
        K[I + 1] := Char(J);
        Inc(J);
        KG.EncryptBlock(K[I + 1]);
      until I = 0;
    finally
      KG.Free;
    end;
    if SetIV then begin
      SetUp(K[1 + BlockVectorSize * BlockSize],Length(K) - BlockVectorSize * BlockSize,0);
      Move(K[1],FIV^,BlockVectorSize * BlockSize);
    end else
      SetUp(K[1],Length(K),0);
    ProtectClear(K[1],Length(K));
{$ELSE  SHA1}
    raise Exception.Create('The implementation made an attempt to use a feature that requires SHA-1');
{$ENDIF SHA1}
  end else if VectorSize > 0 then begin
    if VectorSize <> BlockVectorSize then
      raise Exception.Create(Format('The vector for %s is %d blocks. Cannot initialize with a %d block vector.',
                                    [ClassName,BlockVectorSize,VectorSize]))
    else if VectorSize * BlockSize >= Count then
      raise Exception.Create(Format('The block size for %s is %d bytes and the key is %d bytes. Cannot initialize with a %d block vector.',
                                    [ClassName,BlockSize,Count,VectorSize]))
    else if VectorSize * BlockSize + MinKeySize > Count then
      raise Exception.Create(Format('The minimum key and IV size for %s is %d bytes.',[ClassName,VectorSize*BlockSize + MinKeySize]))
    else begin
      SetUp(Ptr(LongInt(@AKey) + VectorSize * BlockSize)^,Count - VectorSize * BlockSize,0);
      Move(AKey,FIV^,VectorSize * BlockSize);
    end;
  end else
    SetUp(AKey,Count,0);
end;

{ THash }

{$WARNINGS OFF}
class function THash.Algorithm: THashAlgorithm;
begin
  // Undefined
end;
{$WARNINGS ON}

constructor THash.Create(const Data; Count: Int64);
begin
  SetUp;
  HashData(Data,Count);
end;

class function THash.CyclesOfCompress: Int64;
var
  N: Int64;
  H: THash;
begin
  Result := 0;
  H := Self.Create(Self,0);
  try
    H.Calc;
    asm
      {$IFDEF D6UP}
      rdtsc
      {$ELSE}
      db $0F, $31
      {$ENDIF}
      mov dword ptr [n],eax
      mov dword ptr [n+4],edx
      cld
      dw $9090,$9090,$9090,$9090
    end;
    H.Calc;
    asm       
      {$IFDEF D6UP}
      rdtsc   
      rdtsc
      {$ELSE}
      db $0F, $31
      db $0F, $31
      {$ENDIF}
      sub eax,dword ptr [n]
      sbb edx,dword ptr [n+4]
      mov dword ptr [result],eax
      mov dword ptr [result+4],edx
    end;
    Result := Result - 72;
  finally
    H.Free;
  end;
end;

class function THash.DERPrefix: Pointer;
begin
  Result := nil;
end;

class function THash.DERPrefixLen: Integer;
begin
  Result := 0;
end;

destructor THash.Destroy;
begin
  CleanUp;
  inherited;
end;

class function THash.DigestSize: Integer;
begin
  Result := -1;
end;

procedure THash.SetUp;
begin
  CleanUp;
  Init;
end;

{ TBaseHash }

procedure TBaseHash.CleanUp;
begin
  FCount := 0;
  ProtectClear(FDigest,SizeOf(FDigest));
  ProtectClear(FDataBuffer,SizeOf(FDataBuffer));
end;

procedure TBaseHash.GetContext(Context: Pointer);
begin
  Move(FDigest,Context^,DigestSize);
end;

function TBaseHash.GetDigest: OctetString;
begin
  SetLength(Result,DigestSize);
  Move(FDigest,Result[1],DigestSize);
end;

procedure TBaseHash.HashData(const Data; Count: Int64);
var
  I: Integer;
  P: Pointer;
begin
  P := @Data;
  I := FCount and $3F;
  if I > 0 then begin
    if Count >= $40 - I then begin
      Move(P^,FDataBuffer[I],$40 - I);
      Calc;
      Inc(LongInt(P),$40 - I);
      Inc(FCount,$40 - I);
      Dec(Count,$40 - I);
    end else begin
      Move(P^,FDataBuffer[I],Count);
      Inc(FCount,Count);
      Count := 0;
    end;
  end;
  while Count >= $40 do begin
    Move(P^,FDataBuffer,$40);
    Calc;
    Inc(LongInt(P),$40);
    Inc(FCount,$40);
    Dec(Count,$40);
  end;
  if Count > 0 then begin
    Move(P^,FDataBuffer,Count);
    Inc(FCount,Count);
  end;
end;

procedure TBaseHash.SetUpContext(Context: Pointer);
begin
  Move(Context^,FDigest,DigestSize);
end;

{ TBaseSHA }

procedure TBaseSHA.Done(Digest: Pointer);
var
  I: Integer;
  S: Int64;
begin
  I := (FCount and $3F);
  FDataBuffer[I] := $80;
  Inc(I);
  if I > $38 then begin
    FillChar(FDataBuffer[I],$40 - I,0);
    Calc;
    I := 0;
  end;
  FillChar(FDataBuffer[I],$40 - I,0);
  S := FCount * 8;
  for I := $3F downto $38 do begin
    FDataBuffer[I] := S and $FF;
    S := S shr 8;
  end;
  Calc;
  if Assigned(Digest) then
    Move(FDigest,Digest^,DigestSize);
  FCount := 0;
end;
      
{$IFDEF SHA1}
{ TSHA1 }

class function TSHA1.Algorithm: THashAlgorithm;
begin
  Result := haSHA1;
end;

procedure TSHA1.Calc;

  procedure SHAFillBuffer;
  asm
    lea EDI,[EDI - 16 * 4 + 64 * 4]
    mov ECX,64
    neg ECX
  @@FillBuffer:
    mov EDX,[EDI + ECX*4 +  0 * 4]
    xor EDX,[EDI + ECX*4 +  2 * 4]
    xor EDX,[EDI + ECX*4 +  8 * 4]
    xor EDX,[EDI + ECX*4 + 13 * 4]
    rol EDX,1
    mov [EDI + ECX*4 + 16 * 4],EDX
    inc ECX
    jnz @@FillBuffer
  end;

asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX

// Expand data buffer with endian conversion:
  sub ESP,320
  mov EDI,ESP
  mov ECX,-16
  lea ESI,dword ptr [EAX].TSHA1.FDataBuffer
  lea ESI,[ESI+64]
  lea EDI,[EDI+64]
@@CopyToBuffer:
  mov EDX,[ESI+ECX*4]
  bswap EDX
  mov [EDI+ECX*4],EDX
  inc ECX
  jnz @@CopyToBuffer

  call SHAFillBuffer

// Copy FDigest to state with endian conversion:
  lea ESI,dword ptr [EAX].TSHA1.FDigest
  mov EAX,[ESI]
  mov EBX,[ESI + 4]
  mov ECX,[ESI + 8]
  mov EDX,[ESI + 12]
  mov EDI,[ESI + 16]
  bswap EAX
  bswap EBX
  bswap ECX
  bswap EDX
  bswap EDI

// Compress:
  {$I SHA1Compress1.inc}
  {$I SHA1Compress2.inc}
  {$I SHA1Compress3.inc}
  {$I SHA1Compress4.inc}

// Copy state to FDigest with endian conversion:
  mov EBP,EAX
  mov EAX,[ESP + 320]
  lea ESI,dword ptr [EAX].TSHA1.FDigest
  mov EAX,[ESI]
  bswap EAX
  add EAX,EBP
  bswap EAX
  mov [ESI],EAX
  mov EAX,[ESI + 4]
  bswap EAX
  add EAX,EBX
  bswap EAX
  mov [ESI + 4],EAX
  mov EBX,[ESI + 8]
  bswap EBX
  add EBX,ECX
  bswap EBX
  mov [ESI + 8],EBX
  mov EAX,[ESI + 12]
  mov ECX,[ESI + 16]
  bswap EAX
  bswap ECX
  add EAX,EDX
  add ECX,EDI
  bswap EAX
  bswap ECX
  mov [ESI + 12],EAX
  mov [ESI + 16],ECX

  mov ECX,81
  mov EDI,ESP
  xor EAX,EAX
  rep stosd
  mov ESP,EDI

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

class function TSHA1.DERPrefix: Pointer;
begin
  Result := @DERPrefixSHA1;
end;

class function TSHA1.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixSHA1);
end;

class function TSHA1.DigestSize: Integer;
begin
  Result := 20;
end;

procedure TSHA1.Init;
asm
  // Initialize FDigest.
  // NOTE: These values might differ from implementations made by other
  // manufacturers due to the order in which endian conversion is applied.
  lea EDX,dword ptr [EAX].TSHA1.FDigest
  mov dword [EDX     ],$01234567;
  mov dword [EDX +  4],$89ABCDEF;
  mov dword [EDX +  8],$FEDCBA98;
  mov dword [EDX + 12],$76543210;
  mov dword [EDX + 16],$F0E1D2C3;
end;   
{$ENDIF SHA1}

{ TBaseMD }

procedure TBaseMD.Done(Digest: Pointer);
var
  I: Integer;
  S: Int64;
begin
  I := (FCount and $3F);
  FDataBuffer[I] := $80;
  Inc(I);
  if I > $38 then begin
    FillChar(FDataBuffer[I],$40 - I,0);
    Calc;
    I := 0;
  end;
  FillChar(FDataBuffer[I],$40 - I,0);
  S := FCount * 8;
  Move(S,FDataBuffer[$38],8);
  Calc;
  if Assigned(Digest) then
    Move(FDigest,Digest^,DigestSize);
  FCount := 0;
end;

{$IFDEF MD5}
{ TMD5 }

class function TMD5.Algorithm: THashAlgorithm;
begin
  Result := haMD5;
end;

procedure TMD5.Calc;
asm
  push EBX
  push ESI
  push EDI
  push EBP

  lea EBP,dword ptr [EAX].TBaseHash.FDataBuffer

// Copy FDigest to state:
  push EAX
  lea ESI,dword ptr [EAX].TBaseHash.FDigest
  mov EAX,[ESI]
  mov EBX,[ESI + 4]
  mov ECX,[ESI + 8]
  mov EDX,[ESI + 12]

// Compress:
  {$I MD5CompressFF.inc}
  {$I MD5CompressGG.inc}
  {$I MD5CompressHH.inc}
  {$I MD5CompressII.inc}

// Add state to FDigest:
  mov EBP,EAX
  pop EAX
  lea ESI,dword ptr [EAX].TBaseHash.FDigest
  mov EAX,[ESI]
  add EAX,EBP
  mov [ESI],EAX
  mov EAX,[ESI + 4]
  add EAX,EBX
  mov [ESI + 4],EAX
  mov EAX,[ESI + 8]
  add EAX,ECX
  mov [ESI + 8],EAX
  mov EAX,[ESI + 12]
  add EAX,EDX
  mov [ESI + 12],EAX

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

class function TMD5.DERPrefix: Pointer;
begin
  Result := @DERPrefixMD5;
end;

class function TMD5.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixMD5);
end;

class function TMD5.DigestSize: Integer;
begin
  Result := 16;
end;

procedure TMD5.Init;
asm
  // Initialize FDigest.
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX     ],$67452301;
  mov dword [EDX +  4],$EFCDAB89;
  mov dword [EDX +  8],$98BADCFE;
  mov dword [EDX + 12],$10325476;
end;        
{$ENDIF MD5}

{$IFDEF RIPEMD160}
{ TRipeMD160 }

class function TRipeMD160.Algorithm: THashAlgorithm;
begin
  Result := haRipeMD160;
end;

procedure TRipeMD160.Calc;
asm
  push EBX
  push ESI
  push EDI
  push EBP

  lea EBP,dword ptr [EAX].TBaseHash.FDataBuffer

// Copy FDigest to state:
  lea ESI,dword ptr [EAX].TBaseHash.FDigest
  push ESI
  mov EAX,[ESI]
  mov EBX,[ESI + 4]
  mov ECX,[ESI + 8]
  mov EDX,[ESI + 12]
  mov EDI,[ESI + 16]

// Compress:
  {$I RipeMD160Compress1.inc}
  {$I RipeMD160Compress2.inc}
  {$I RipeMD160Compress3.inc}
  {$I RipeMD160Compress4.inc}
  {$I RipeMD160Compress5.inc}

// Save and reset intermediary state:
  sub ESP,20
  mov [ESP],EAX
  mov [ESP + 4],EBX
  mov [ESP + 8],ECX
  mov [ESP + 12],EDX
  mov [ESP + 16],EDI
  mov ESI,[ESP + 20]
  mov EAX,[ESI]
  mov EBX,[ESI + 4]
  mov ECX,[ESI + 8]
  mov EDX,[ESI + 12]
  mov EDI,[ESI + 16]

// Compress parallel:
  {$I RipeMD160Compress6.inc}
  {$I RipeMD160Compress7.inc}
  {$I RipeMD160Compress8.inc}
  {$I RipeMD160Compress9.inc}
  {$I RipeMD160Compress10.inc}

  add EDX,[ESP + 8]
  add EDI,[ESP + 12]
  add EAX,[ESP + 16]
  add EBX,[ESP]
  add ECX,[ESP + 4]

  mov ESI,[ESP + 20]
  add EDX,[ESI + 4]
  add EDI,[ESI + 8]
  add EAX,[ESI + 12]
  add EBX,[ESI + 16]
  add ECX,[ESI]

  mov [ESI],EDX
  mov [ESI + 4],EDI
  mov [ESI + 8],EAX
  mov [ESI + 12],EBX
  mov [ESI + 16],ECX
  // Burn intermediary values:
  mov dword [ESP],0
  mov dword [ESP + 4],0
  mov dword [ESP + 8],0
  mov dword [ESP + 12],0
  mov dword [ESP + 16],0
  add ESP,24

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

class function TRipeMD160.DERPrefix: Pointer;
begin
  Result := @DERPrefixRipeMD160;
end;

class function TRipeMD160.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixRipeMD160);
end;

class function TRipeMD160.DigestSize: Integer;
begin
  Result := 20;
end;

procedure TRipeMD160.Init;
asm
  // Initialize FDigest.
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX     ],$67452301;
  mov dword [EDX +  4],$EFCDAB89;
  mov dword [EDX +  8],$98BADCFE;
  mov dword [EDX + 12],$10325476;
  mov dword [EDX + 16],$C3D2E1F0;
end;              
{$ENDIF RIPEMD160}

{$IFDEF SHA256}
{ TSHA256 }

class function TSHA256.Algorithm: THashAlgorithm;
begin
  Result := haSHA256;
end;

procedure TSHA256.Calc;
asm
  push EBX
  push ESI
  push EDI
  push EBP

  lea EBP,dword ptr [EAX].TSHA256.FDataBuffer
// Copy FDigest to state with endian conversion:
  lea ESI,dword ptr [EAX].TSHA256.FDigest
  push ESI
  sub ESP,32
  mov ECX,8
@@InitState:
  mov EAX,[ESI + ECX*4 - 4]
  bswap EAX
  mov [ESP + ECX*4 - 4],EAX
  loop @@InitState

// Expand data buffer:
  sub ESP,256
  mov ECX,16
@@InitBuffer:
  mov EAX,[EBP + ECX*4 - 4]
  bswap EAX
  mov [ESP + ECX*4 - 4],EAX
  loop @@InitBuffer
  mov ECX,16
@@ExpandBuffer:
  mov EAX,[ESP + ECX*4 - 8]
  mov EDX,EAX
  mov EBX,EAX
  ror EDX,17
  ror EBX,19
  xor EDX,EBX
  shr EAX,10
  xor EDX,EAX
  add EDX,[ESP + ECX*4 - 28]
  mov EDI,[ESP + ECX*4 - 60]
  mov EAX,EDI
  mov EBX,EDI
  ror EAX,7
  ror EBX,18
  xor EAX,EBX
  shr EDI,3
  xor EDI,EAX
  add EDX,EDI
  add EDX,[ESP + ECX*4 - 64]
  mov [ESP + ECX*4],EDX
  inc ECX
  cmp ECX,64
  jl @@ExpandBuffer

// Compress:
  {$I SHA256Compress.inc}

  mov ECX,64
@@ZeroizeW:
  mov dword ptr [ESP + ECX*4 - 4],0
  loop @@ZeroizeW
  add ESP,256
// Copy state to FDigest with endian conversion:
  mov ESI,[ESP + 32]
  mov ECX,8
@@AddState:
  mov EAX,[ESI + ECX*4 - 4]
  bswap EAX
  add EAX,[ESP + ECX*4 - 4]
  bswap EAX
  mov [ESI + ECX*4 - 4],EAX
  loop @@AddState
  mov ECX,9
@@ZeroizeState:
  mov dword ptr [ESP + ECX*4 - 4],0
  loop @@ZeroizeState
  add ESP,36

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

class function TSHA256.DERPrefix: Pointer;
begin
  Result := @DERPrefixSHA256;
end;

class function TSHA256.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixSHA256);
end;

class function TSHA256.DigestSize: Integer;
begin
  Result := 32;
end;

procedure TSHA256.Init;
asm
  // Initialize FDigest.
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX     ],$67E6096A;
  mov dword [EDX +  4],$85AE67BB;
  mov dword [EDX +  8],$72F36E3C;
  mov dword [EDX + 12],$3AF54FA5;
  mov dword [EDX + 16],$7F520E51;
  mov dword [EDX + 20],$8C68059B;
  mov dword [EDX + 24],$ABD9831F;
  mov dword [EDX + 28],$19CDE05B;
end;
{$ENDIF SHA256}
               
{$IFDEF SHA512}
{ TSHA512 }

class function TSHA512.Algorithm: THashAlgorithm;
begin
  Result := haSHA512;
end;

procedure TSHA512.Calc;
asm
  push EBX
  push ESI
  push EDI
  push EBP

  lea EBP,dword ptr [EAX].TSHA512.FDataBuffer
// Copy FDigest to state with endian conversion:
  lea ESI,dword ptr [EAX].TSHA512.FDigest
  push ESI
  sub ESP,64
  mov ECX,8
@@InitState:
  mov EAX,[ESI + ECX*8 - 4]
  bswap EAX
  mov [ESP + ECX*8 - 8],EAX
  mov EAX,[ESI + ECX*8 - 8]
  bswap EAX
  mov [ESP + ECX*8 - 4],EAX
  loop @@InitState

// Expand data buffer:
  sub ESP,640
  mov ECX,16
@@InitBuffer:
  mov EAX,[EBP + ECX*8 - 4]
  bswap EAX
  mov [ESP + ECX*8 - 8],EAX
  mov EAX,[EBP + ECX*8 - 8]
  bswap EAX
  mov [ESP + ECX*8 - 4],EAX
  loop @@InitBuffer
  mov ECX,16
@@ExpandBuffer:
  mov EDI,[ESP + ECX*8 - 16]
  mov ESI,[ESP + ECX*8 - 12]
  mov EBX,EDI
  shrd EBX,ESI,19
  mov EAX,EBX
  mov EBX,ESI
  shrd EBX,EDI,19
  mov EDX,EBX
  mov EBX,EDI
  shld EBX,ESI,3
  xor EAX,EBX
  mov EBX,ESI
  shld EBX,EDI,3
  xor EDX,EBX
  shrd EDI,ESI,6
  xor EAX,EDI
  shr ESI,6
  xor EDX,ESI
  mov [ESP + ECX*8    ],EAX
  mov [ESP + ECX*8 + 4],EDX
  mov EAX,[ESP + ECX*8 - 56]
  mov EDX,[ESP + ECX*8 - 52]
  add [ESP + ECX*8    ],EAX
  adc [ESP + ECX*8 + 4],EDX
  mov EDI,[ESP + ECX*8 - 120]
  mov ESI,[ESP + ECX*8 - 116]
  mov EBX,EDI
  shrd EBX,ESI,1
  mov EAX,EBX
  mov EBX,ESI
  shrd EBX,EDI,1
  mov EDX,EBX
  mov EBX,EDI
  shrd EBX,ESI,8
  xor EAX,EBX
  mov EBX,ESI
  shrd EBX,EDI,8
  xor EDX,EBX
  shrd EDI,ESI,7
  xor EAX,EDI
  shr ESI,7
  xor EDX,ESI
  add [ESP + ECX*8    ],EAX
  adc [ESP + ECX*8 + 4],EDX
  mov EAX,[ESP + ECX*8 - 128]
  mov EDX,[ESP + ECX*8 - 124]
  add [ESP + ECX*8    ],EAX
  adc [ESP + ECX*8 + 4],EDX
  inc ECX
  cmp ECX,80
  jl @@ExpandBuffer

// Compress:
  {$I SHA512Compress.inc}
             
@@ZeroizeW:
  xor EAX,EAX
  mov EDI,ESP
  mov ECX,160
  rep stosd

  add ESP,640

// Copy state to FDigest with endian conversion:

  mov ESI,[ESP + 64]
  mov ECX,8
@@AddState:
  mov EAX,[ESI + ECX*8 - 4]
  mov EDX,[ESI + ECX*8 - 8]
  bswap EAX
  bswap EDX
  add EAX,[ESP + ECX*8 - 8]
  adc EDX,[ESP + ECX*8 - 4]
  bswap EAX
  bswap EDX
  mov [ESI + ECX*8 - 4],EAX
  mov [ESI + ECX*8 - 8],EDX
  loop @@AddState

@@ZeroizeState:
  xor EAX,EAX
  mov EDI,ESP
  mov ECX,17
  rep stosd

  add ESP,68

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

class function TSHA512.DERPrefix: Pointer;
begin
  Result := @DERPrefixSHA512;
end;

class function TSHA512.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixSHA512);
end;

class function TSHA512.DigestSize: Integer;
begin
  Result := 64;
end;

procedure TSHA512.Done(Digest: Pointer);
var
  I: Integer;
  S: Int64;
begin
  I := (FCount and $7F);
  FDataBuffer[I] := $80;
  Inc(I);
  if I > $70 then begin
    FillChar(FDataBuffer[I],$80 - I,0);
    Calc;
    I := 0;
  end;
  FillChar(FDataBuffer[I],$80 - I,0);
  S := FCount * 8;
  for I := $7F downto $70 do begin
    FDataBuffer[I] := S and $FF;
    S := S shr 8;
  end;
  Calc;
  if Assigned(Digest) then
    Move(FDigest,Digest^,DigestSize);
  FCount := 0;
end;

procedure TSHA512.HashData(const Data; Count: Int64);
var
  I: Integer;
  P: Pointer;
begin
  P := @Data;
  I := FCount and $7F;
  if I > 0 then begin
    if Count >= $80 - I then begin
      Move(P^,FDataBuffer[I],$80 - I);
      Calc;
      Inc(LongInt(P),$80 - I);
      Inc(FCount,$80 - I);
      Dec(Count,$80 - I);
    end else begin
      Move(P^,FDataBuffer[I],Count);
      Inc(FCount,Count);
      Count := 0;
    end;
  end;
  while Count >= $80 do begin
    Move(P^,FDataBuffer,$80);
    Calc;
    Inc(LongInt(P),$80);
    Inc(FCount,$80);
    Dec(Count,$80);
  end;
  if Count > 0 then begin
    Move(P^,FDataBuffer,Count);
    Inc(FCount,Count);
  end;
end;

procedure TSHA512.Init;
asm
//1 = 6a09e667f3bcc908
//2 = bb67ae8584caa73b
//3 = 3c6ef372fe94f82b
//4 = a54ff53a5f1d36f1
//5 = 510e527fade682d1
//6 = 9b05688c2b3e6c1f
//7 = 1f83d9abfb41bd6b
//8 = 5be0cd19137e2179
  // Initialize FDigest.
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX +  4],$08C9BCF3;
  mov dword [EDX     ],$67E6096A;
  mov dword [EDX + 12],$3BA7CA84;
  mov dword [EDX +  8],$85AE67BB;
  mov dword [EDX + 20],$2BF894FE;
  mov dword [EDX + 16],$72F36E3C;
  mov dword [EDX + 28],$F1361D5F;
  mov dword [EDX + 24],$3AF54FA5;
  mov dword [EDX + 36],$D182E6AD;
  mov dword [EDX + 32],$7F520E51;
  mov dword [EDX + 44],$1F6C3E2B;
  mov dword [EDX + 40],$8C68059B;
  mov dword [EDX + 52],$6BBD41FB;
  mov dword [EDX + 48],$ABD9831F;
  mov dword [EDX + 60],$79217E13;
  mov dword [EDX + 56],$19CDE05B;
end;

{ TSHA384 }

class function TSHA384.Algorithm: THashAlgorithm;
begin
  Result := haSHA384;
end;

class function TSHA384.DERPrefix: Pointer;
begin
  Result := @DERPrefixSHA384;
end;

class function TSHA384.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixSHA384);
end;

class function TSHA384.DigestSize: Integer;
begin
  Result := 48;
end;

procedure TSHA384.Init;
asm
//1 = cbbb9d5dc1059ed8
//2 = 629a292a367cd507
//3 = 9159015a3070dd17
//4 = 152fecd8f70e5939
//5 = 67332667ffc00b31
//6 = 8eb44a8768581511
//7 = db0c2e0d64f98fa7
//8 = 47b5481dbefa4fa4 
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX +  4],$D89E05C1;
  mov dword [EDX     ],$5D9DBBCB;
  mov dword [EDX + 12],$07D57C36;
  mov dword [EDX +  8],$2A299A62;
  mov dword [EDX + 20],$17DD7030;
  mov dword [EDX + 16],$5A015991;
  mov dword [EDX + 28],$39590EF7;
  mov dword [EDX + 24],$D8EC2F15;
  mov dword [EDX + 36],$310BC0FF;
  mov dword [EDX + 32],$67263367;
  mov dword [EDX + 44],$11155868;
  mov dword [EDX + 40],$874AB48E;
  mov dword [EDX + 52],$A78FF964;
  mov dword [EDX + 48],$0D2E0CDB;
  mov dword [EDX + 60],$A44FFABE;
  mov dword [EDX + 56],$1D48B547;
end;          
{$ENDIF SHA512}

{$IFDEF MD2}
{ TMD2 }

class function TMD2.Algorithm: THashAlgorithm;
begin
  Result := haMD2;
end;

const
  S: array [0..255] of Byte = (
    41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6,
    19, 98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188,
    76, 130, 202, 30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24,
    138, 23, 229, 18, 190, 78, 196, 214, 218, 158, 222, 73, 160, 251,
    245, 142, 187, 47, 238, 122, 169, 104, 121, 145, 21, 178, 7, 63,
    148, 194, 16, 137, 11, 34, 95, 33, 128, 127, 93, 154, 90, 144, 50,
    39, 53, 62, 204, 231, 191, 247, 151, 3, 255, 25, 48, 179, 72, 165,
    181, 209, 215, 94, 146, 42, 172, 86, 170, 198, 79, 184, 56, 210,
    150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241, 69, 157,
    112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2, 27,
    96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
    85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197,
    234, 38, 44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65,
    129, 77, 82, 106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123,
    8, 12, 189, 177, 74, 120, 136, 149, 139, 227, 99, 232, 109, 233,
    203, 213, 254, 59, 0, 29, 57, 242, 239, 183, 14, 102, 88, 208, 228,
    166, 119, 114, 248, 235, 117, 75, 10, 49, 68, 80, 180, 143, 237,
    31, 26, 219, 153, 141, 51, 159, 17, 131, 20);

procedure TMD2.Calc;
{$IFDEF MD2}
var
  I, J: Integer;
  L, T: Byte;
{$ENDIF MD2}
begin
  {$IFDEF MD2}
  // Calc checksum:
  L := FCheckSum[15];
  for I := 0 to 15 do begin
    L := FCheckSum[I] xor S[FDataBuffer[16 + I] xor L];
    FCheckSum[I] := L;
  end;

  // Update buffer:
  for I := 0 to 3 do
    PLongWord(@FDataBuffer[32 + I*4])^ := PLongWord(@FDataBuffer[I*4])^ xor
                                          PLongWord(@FDataBuffer[16 + I*4])^;
  T := 0;
  for I := 0 to 17 do begin
    for J := 0 to 47 do begin
      T := FDataBuffer[J] xor S[T];
      FDataBuffer[J] := T;
    end;
    T := (T + I) and $FF;
  end;
  {$ELSE}
  Assert(False);
  {$ENDIF}
end;

procedure TMD2.CleanUp;
begin
  ProtectClear(FDataBuffer,SizeOf(FDataBuffer));
  ProtectClear(FCheckSum,SizeOf(FCheckSum));
  FCount := 0;
end;

class function TMD2.DERPrefix: Pointer;
begin
  Result := @DERPrefixMD2;
end;

class function TMD2.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixMD2);
end;

class function TMD2.DigestSize: Integer;
begin
  Result := 16;
end;

procedure TMD2.Done(Digest: Pointer);
var
  Index: Integer;
  Pad: Byte;
begin
  Index := FCount;
  Pad := 16 - Index;
  FillChar(FDataBuffer[16+Index],Pad,Pad);
  Calc;
  Move(FCheckSum,FDataBuffer[16],16);
  Calc;
  if Assigned(Digest) then
    Move(FDataBuffer,Digest^,16);
end;

procedure TMD2.GetContext(Context: Pointer);
begin
  Assert(False,'Not supported');
end;

function TMD2.GetDigest: OctetString;
begin
  SetLength(Result,16);
  Move(FDataBuffer,Pointer(Result)^,16);
end;

procedure TMD2.HashData(const Data; Count: Int64);
var
  Index: Integer;
  P: PChar;
begin
  Index := FCount;
  FCount := (Index + Count) and $F;

  P := @Data;
  if Index + Count >= 16 then begin
    Move(P^,FDataBuffer[16 + Index],16 - Index);
    Calc;
    P := P + 16 - Index;
    Count := Count - 16 + Index;
    Index := 0;
    while Count >= 16 do begin
      Move(P^,FDataBuffer[16],16);
      Calc;
      Inc(P,16);
      Dec(Count,16);
    end;
  end;
  if Count > 0 then
    Move(P^,FDataBuffer[16 + Index],Count);
end;

procedure TMD2.Init;
begin
  CleanUp;
end;

procedure TMD2.SetUpContext(Context: Pointer);
begin
  Assert(False,'Not supported');
end;        
{$ENDIF MD2}

{ TSecretKey }

procedure TSecretKey.AddTruncKeyAt(AKey: ISecretKey; APos: Integer; IncVal: Byte);
var
  I: Integer;
  W: Word;
  D, S: PByteArray;
begin
  if Assigned(AKey) then begin
    if (APos < 0) or (FKeyLen < AKey.KeyLen + APos) then
      raise Exception.Create('TSecretKey.AddTruncKeyAt: Access Violation');
    D := @GetKeyBytes^[APos];
    S := AKey.KeyBytes;
    W := IncVal;
    for I := AKey.KeyLen - 1 downto 0 do begin
      W := D^[I] + S^[I] + W;
      D^[I] := Lo(W);
      W := Hi(W);
    end;
  end;
end;

procedure TSecretKey.ClearKey;
begin
  if Assigned(FKey) then begin
    ProtectClear(FKey^,FKeyLen);
    {$IFDEF WIN32}
//    VirtualUnlock(FKey,FKeyLen);
    {$ENDIF}
    FreeMem(FKey);
  end;
  FKey := nil;
  FKeyLen := 0;
  FVectorSize := 0;
end;

constructor TSecretKey.Create(AAlgorithm: ObjectIdentifier);
begin
  FAlgorithm := AAlgorithm;
end;

constructor TSecretKey.CreateBMPStr(AValue: PWideChar; Len: Integer);
var
  I: Integer;
begin
  if Len < 0 then
    repeat
      Inc(Len);
    until AValue[Len] = #0;
  SetLength((Len+1)*2);
  for I := 0 to Len do begin
    PByteArray(FKey)[I*2] := Word(AValue[I]) shr 8;
    PByteArray(FKey)[I*2+1] := Word(AValue[I]) and $FF;
  end;
end;

constructor TSecretKey.CreateStr(AValue: PChar; Len: Integer);
begin
  if Len < 0 then
    Len := StrLen(AValue);
  SetKey(AValue,Len,0);
end;

destructor TSecretKey.Destroy;
begin
  ClearKey;
  inherited Destroy;
end;

function TSecretKey.GetAlgorithm: ObjectIdentifier;
begin
  Result := FAlgorithm;
end;

function TSecretKey.GetKey: Pointer;
begin
  Result := FKey;
end;

function TSecretKey.GetKeyBytes: PByteArray;
begin
  Result := FKey;
end;

function TSecretKey.GetKeyLen: Integer;
begin
  Result := FKeyLen;
end;

function TSecretKey.GetVectorSize: Integer;
begin
  Result := FVectorSize;
end;

function TSecretKey.Instance: TObject;
begin
  Result := Self;
end;

procedure TSecretKey.SetKey(AKey: Pointer; AKeyLen, AVectorSize: Integer);
begin
  SetLength(AKeyLen);
  Move(AKey^,FKey^,FKeyLen);
  FVectorSize := AVectorSize;
end;

procedure TSecretKey.SetKeyAt(AKey: ISecretKey; APos: Integer);
begin
  if Assigned(AKey) then begin
    if (APos < 0) or (FKeyLen < AKey.KeyLen + APos) then
      raise Exception.Create('TSecretKey.SetKeyAt: Access Violation');
    Move(AKey.Key^,GetKeyBytes^[APos],AKey.KeyLen);
  end;
end;

procedure TSecretKey.SetKeyStr(const AKeyStr: string; ConvertToBMPStr: Boolean);
var
  Len, I: Integer;
begin
  if ConvertToBMPStr then begin
    Len := Length(AKeyStr);
    SetLength((Len+1)*2);
    for I := 0 to Len-1 do begin
      PByteArray(FKey)[I*2] := 0;
      PByteArray(FKey)[I*2+1] := Byte(AKeyStr[I+1]);
    end;
    PByteArray(FKey)[Len*2] := 0;
    PByteArray(FKey)[Len*2+1] := 0;
  end else
    SetKey(Pointer(AKeyStr),Length(AKeyStr),0);
end;

procedure TSecretKey.SetKeyStrAt(const AKeyStr: string; APos: Integer);
begin
  if (APos < 0) or (FKeyLen < Length(AKeyStr) + APos) then
    raise Exception.Create('TSecretKey.SetKeyStrAt: Access Violation');
  Move(Pointer(AKeyStr)^,GetKeyBytes^[APos],Length(AKeyStr));
end;

procedure TSecretKey.SetLength(AKeyLen: Integer);
begin
  ClearKey;
  FKeyLen := AKeyLen;
  GetMem(FKey,FKeyLen);
  {$IFDEF WIN32}
//  VirtualLock(FKey,FKeyLen);
  {$ENDIF}
end;

procedure TSecretKey.SetTruncKeyAt(AKey: ISecretKey; APos: Integer);
begin
  if Assigned(AKey) then
    if FKeyLen > APos then begin
      if FKeyLen >= AKey.KeyLen + APos then
        Move(AKey.Key^,GetKeyBytes^[APos],AKey.KeyLen)
      else
        Move(AKey.Key^,GetKeyBytes^[APos],FKeyLen - APos)
    end;
end;

procedure TSecretKey.SetVectorSize(const Value: Integer);
begin
  FVectorSize := Value;
end;

procedure TSecretKey.XORKeyAt(AKey: ISecretKey; APos: Integer);
var
  KL: Integer;
  S, D: PLongWord;
begin
  if Assigned(AKey) then begin
    if (APos < 0) or (FKeyLen < AKey.KeyLen + APos) then
      raise Exception.Create('TSecretKey.XORKeyAt: Access Violation');
    D := @GetKeyBytes^[APos];
    S := AKey.Key;
    KL := AKey.KeyLen;
    XORBytes(D^,S^,KL);
  end;
end;

{ TCipherMAC }

procedure TCipherMAC.EncryptToNil(const Buf; Count: Integer);
var
  B: array of Byte;
begin
  SetLength(B,Count);
  EncryptToBuf(Buf,Pointer(B)^,Count);
end;

procedure TCipherMAC.GetMacBuf(var Mac; Count: Integer);
var
  Str: string;
begin
  Str := GetMac;
  if Length(Str) < Count then
    Move(Pointer(Str)^,Mac,Length(Str))
  else
    Move(Pointer(Str)^,Mac,Count);
  ProtectClear(Pointer(Str)^,Length(Str));
end;

class function TCipherMAC.MacLen: Integer;
begin
  Result := 0;
end;

{$IFDEF SHA256}
{ TSHA224 }

class function TSHA224.Algorithm: THashAlgorithm;
begin
  Result := haSHA224;
end;

class function TSHA224.DERPrefix: Pointer;
begin
  Result := nil; // Yet to be defined
end;

class function TSHA224.DERPrefixLen: Integer;
begin
  Result := 0;  // Yet to be defined
end;

class function TSHA224.DigestSize: Integer;
begin
  Result := 28;
end;

procedure TSHA224.Init;
asm
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX     ],$D89E05C1
  mov dword [EDX +  4],$07D57C36
  mov dword [EDX +  8],$17DD7030
  mov dword [EDX + 12],$39590EF7
  mov dword [EDX + 16],$310BC0FF
  mov dword [EDX + 20],$11155868
  mov dword [EDX + 24],$A78FF964
  mov dword [EDX + 28],$A44FFABE
end;
{$ENDIF SHA256}

initialization
{$IFDEF DEMO}
  MessageBox(0,'You are using a trial version of StreamSec StrSecII components'#13#10 +
               'Please goto www.streamsec.com and purchase your single developer license'#13#10 +
                'Copyright 2002-2003 (c) StreamSec',
               'StreamSec',MB_OK);
{$ENDIF}
finalization
  CipherClassList.Free;
end.






