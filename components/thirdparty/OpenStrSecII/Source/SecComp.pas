{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SecComp Unit                                      }
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
unit SecComp;

interface

uses
  Controls,
  SysUtils, Classes, SyncObjs, SecUtils, Kdf
  {$IFDEF ARCFOUR}, SsArc4 {$ENDIF}
  {$IFDEF RIJNDAEL}, SsRijndael {$ENDIF}
  {$IFDEF TWOFISH}, SsTwoFish {$ENDIF}
  {$IFDEF DES}, SsDes {$ENDIF}           
  {$IFDEF ARCTWO}, SsArc2 {$ENDIF}
  {$IFDEF BLOWFISH}, SsBlowFish {$ENDIF};

resourcestring
  S_ERR_KeyNotSet         = '%s: Key not set';
  S_ERR_WrongDirection    = '%s: Wrong direction';
  S_ERR_NoInplace         = '%s: Unable to perform inplace operation';
  S_ERR_NoIV              = '%s: Initialization Vector not set';
  S_ERR_OnlyInplace       = '%s: Only able to perform inplace operation';

type
  ESecComp = class(Exception);

  TCipherDirection = (cdAuto,cdDecrypt,cdEncrypt);
  TStringFormat = (sfBinary,sfHex,sfBase64);

  TSsKeyRingComponent = class(TComponent)
  public                    
    function AddPrivateAESKey(const PrivKey; PrivKeyLen: Integer;
                              IsLongTermKey: Boolean;
                              KeyIdentifier: string = ''): Integer; virtual; abstract;
    function AddPrivate3DESKey(const PrivKey; PrivKeyLen: Integer;
                               IsLongTermKey: Boolean;
                               KeyIdentifier: string = ''): Integer; virtual; abstract;
    function AddPrivateARC4Key(const PrivKey; PrivKeyLen: Integer;
                               IsLongTermKey: Boolean;
                               KeyIdentifier: string = ''): Integer; virtual; abstract;
    function AddPrivateTwoFishKey(const PrivKey; PrivKeyLen: Integer;
                                  IsLongTermKey: Boolean;
                                  KeyIdentifier: string = ''): Integer; virtual; abstract;
    function AddPrivateBlowFishKey(const PrivKey; PrivKeyLen: Integer;
                                   IsLongTermKey: Boolean;
                                   KeyIdentifier: string = ''): Integer; virtual; abstract;
    function FindCreateCipher(const KeyIdentifier: OctetString;
                              Alg: TCipherAlg; Mode: TCipherMode;
                              var KeyedIV: Boolean;
                              var Param: OctetString;
                              MACKey: ISecretKey = nil): TCipher; virtual; abstract;
  end;

  TSsCustomSecComponent = class(TComponent)
  private
    FLock: TCriticalSection;
  protected
    procedure CleanUp; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  TSsHash = class(TSsCustomSecComponent)
  private
    FHash: THash;
    FHMACKey: ISecretKey;
    FHashAlgorithm: THashAlgorithm;
    FDigest: string;
    procedure SetHashAlgorithm(const Value: THashAlgorithm);
  protected
    procedure CleanUp; override;
    function GetDigest: string;
  public
    procedure Done(Digest: Pointer; DigestLen: Integer = -1);
    procedure GetContext(Context: Pointer);
    procedure HashData(const AData; Count: Int64);
    procedure HashIntf(AData: ISecretKey);
    procedure HashPtr(AData: Pointer; Count: Int64);
    procedure HashStr(const AData: string);
    procedure HashStream(AData: TStream; Count: Integer);
    procedure SetKeyBuf(const AKey; AKeyLen: Integer);
    procedure SetKeyIntf(AKey: ISecretKey);
    procedure SetKeyPtr(AKey: Pointer; AKeyLen: Integer);
    procedure SetKeyStr(const AKey: string; AFormat: TStringFormat = sfBinary);
    procedure SetUp;
    procedure SetUpContext(Context: Pointer);
    property Digest: string read GetDigest;
  published
    property HashAlgorithm: THashAlgorithm read FHashAlgorithm write SetHashAlgorithm;
  end;

  TSsCustomCipherComponent = class(TSsCustomSecComponent)
  private
    FCipher: TCipher;
    FMustSetIV: Boolean;
    FKeyIdentifier: OctetString;
    FAlgorithm: TCipherAlg;
    FMode: TCipherMode;
    FDirection: TCipherDirection;
    FInternalDirection: TCipherDirection;
    FKeyedIV: Boolean;
    FDestinationStream: TStream;
    FInputBlock: ISecretKey;
    FBufferSize: Integer;
    FPadding: Boolean;
    FKeyRing: TSsKeyRingComponent;
    FParam: OctetString;
    FMAC: TSsHash;
    FPBKDF: TPBKDF;
    FPBKDFClass: TPBKDFClass;
    function GetIVector: OctetString;
    function GetMode: TCipherMode;
    procedure SetAlgorithm(const Value: TCipherAlg);
    procedure SetDestinationStream(const Value: TStream);
    procedure SetDirection(const Value: TCipherDirection);
    procedure SetInternalDirection(const Value: TCipherDirection);
    procedure SetIVector(const Value: OctetString);
    procedure SetKeyedIV(const Value: Boolean);
    procedure SetKeyIdentifier(const Value: OctetString);
    procedure SetMode(const Value: TCipherMode);
    procedure SetBufferSize(const Value: Integer);
    procedure SetPadding(const Value: Boolean);
    procedure SetKeyRing(const Value: TSsKeyRingComponent);
    procedure SetParam(const Value: OctetString);
    procedure SetMAC(const Value: TSsHash);
    procedure SetPBKDFClass(const Value: TPBKDFClass);
  protected
    procedure CheckKeyNotSet(const MethodName: string; Inplace: Boolean);
    procedure CheckInplaceCleanUp(ABufLen: Integer);
    procedure DoCipherText(const ABuf; ABufLen: Integer);
    function GetCipherClass: TCipherClass;
    function GetVectorSize: Integer; dynamic;        
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;
    property Algorithm: TCipherAlg read FAlgorithm write SetAlgorithm;
    property Cipher: TCipher read FCipher;
    property InternalDirection: TCipherDirection read FInternalDirection
                                                 write SetInternalDirection;
    property KeyIdentifier: OctetString read FKeyIdentifier write SetKeyIdentifier;
    property KeyRing: TSsKeyRingComponent read FKeyRing write SetKeyRing;
    property Mode: TCipherMode read GetMode write SetMode;
    property Padding: Boolean read FPadding write SetPadding;
    property Param: OctetString read FParam write SetParam;
  public                      
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;                
    procedure CleanUp; override;
    procedure DecryptToDst(const ABuf; ABufLen: Integer);
    procedure DecryptPtrToDst(ABuf: Pointer; ABufLen: Integer);
    function DecryptStreamToDst(ABuf: TStream; Count: Integer): Integer;
    procedure DecryptStrToDst(const ABuf: string);
    procedure Decrypt(var ABuf; ABufLen: Integer);
    procedure DecryptIntf(ABuf: ISecretKey);
    procedure DecryptPtr(ABuf: Pointer; ABufLen: Integer);
    function DecryptStr(const ABuf: string): string;
    function DoneToDst: Boolean;
    procedure EncryptToDst(const ABuf; ABufLen: Integer);
    procedure EncryptPtrToDst(ABuf: Pointer; ABufLen: Integer);
    procedure EncryptStrToDst(const ABuf: string);
    procedure Encrypt(var ABuf; ABufLen: Integer);
    procedure EncryptIntf(ABuf: ISecretKey);
    procedure EncryptPtr(ABuf: Pointer; ABufLen: Integer);
    function EncryptStreamToDst(ABuf: TStream; Count: Integer): Integer;
    function EncryptStr(const ABuf: string): string;
    procedure SetKeyBuf(const AKey; AKeyLen: Integer);
    function SetKeyFromKeyRing: Boolean;
    procedure SetKeyIntf(AKey: ISecretKey);
    procedure SetKeyPassword(APassword: ISecretKey; AKeyLen: Integer);
    procedure SetKeyPtr(AKey: Pointer; AKeyLen: Integer);
    function SetKeyRandom(AKeyLen: Integer): Boolean;
    procedure SetKeyStr(const AKey: string; AFormat: TStringFormat = sfBinary);
    property DestinationStream: TStream read FDestinationStream write SetDestinationStream;
    property IVector: OctetString read GetIVector write SetIVector;
    property PBKDF: TPBKDF read FPBKDF;
    property PBKDFClass: TPBKDFClass read FPBKDFClass write SetPBKDFClass;
  published
    property BufferSize: Integer read FBufferSize write SetBufferSize default 0;
    property Direction: TCipherDirection read FDirection write SetDirection default cdAuto;
    property KeyedIV: Boolean read FKeyedIV write SetKeyedIV default False;
    property MAC: TSsHash read FMAC write SetMAC;
  end;

  TSsCipherCompClass = class of TSsCustomCipherComponent;

  TSsAES = class(TSsCustomCipherComponent)
  public
    constructor Create(AOwner: TComponent); override;
    property Param;
  published
    property KeyIdentifier;
    property KeyRing;
    property Mode default cmCTR;
    property Padding default False;
  end;

  TSsRC4 = class(TSsCustomCipherComponent)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property KeyIdentifier;
    property KeyRing;
  end;

  TSsDES = class(TSsCustomCipherComponent)
  public
    constructor Create(AOwner: TComponent); override;
    property Param;
  published
    property KeyIdentifier;
    property KeyRing;
    property Mode default cmCTR;
    property Padding default False;
  end;

  TSsTwoFish = class(TSsCustomCipherComponent)
  public
    constructor Create(AOwner: TComponent); override;
    property Param;
  published
    property KeyIdentifier;
    property KeyRing;
    property Mode default cmCTR;
    property Padding default False;
  end;

  TSsBlowFish = class(TSsCustomCipherComponent)
  public
    constructor Create(AOwner: TComponent); override;
    property Param;
  published
    property KeyIdentifier;
    property KeyRing;
    property Mode default cmCTR;
    property Padding default False;
  end;

implementation

uses
  SsBase64, ReadStrm, MpYarrow;

{ TSsCustomSecComponent }

constructor TSsCustomSecComponent.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

destructor TSsCustomSecComponent.Destroy;
begin
  FLock.Free;
  CleanUp;
  inherited;
end;

procedure TSsCustomSecComponent.Lock;
begin
  FLock.Acquire;
end;

procedure TSsCustomSecComponent.Unlock;
begin
  FLock.Release;
end;

{ TSsCustomCipherComponent }

procedure TSsCustomCipherComponent.CheckInplaceCleanUp(ABufLen: Integer);
begin
  if ((ABufLen mod FCipher.BlockSize) > 0) and
     (Mode <> cmECB) and (GetCipherClass.InheritsFrom(TBlockCipher)) then
    FMustSetIV := True;
end;

procedure TSsCustomCipherComponent.CheckKeyNotSet(
  const MethodName: string; Inplace: Boolean);
begin
  if not Assigned(FCipher) then
    raise ESecComp.Create(Format(S_ERR_KeyNotSet,[MethodName]));
  if Inplace then begin
    if Assigned(FDestinationStream) then
      raise ESecComp.Create(Format(S_ERR_NoInplace,[MethodName]));
    if FMustSetIV then                                            
      raise ESecComp.Create(Format(S_ERR_NoIV,[MethodName]));
  end else
    if not Assigned(FDestinationStream) then
      raise ESecComp.Create(Format(S_ERR_OnlyInplace,[MethodName]));
end;

procedure TSsCustomCipherComponent.CleanUp;
var
  C: TCipher;
begin
  C := FCipher;
  FCipher := nil;
  C.Free;
  FInputBlock := nil;
  SetInternalDirection(Direction);
end;

constructor TSsCustomCipherComponent.Create(AOwner: TComponent);
begin
  inherited;
  SetPBKDFClass(TWPKDF2);
end;

procedure TSsCustomCipherComponent.Decrypt(var ABuf;
  ABufLen: Integer);
begin
  CheckKeyNotSet('Decrypt',True);
  DoCipherText(ABuf,ABufLen);
  FCipher.Decrypt(ABuf,ABufLen);
  CheckInplaceCleanUp(ABufLen);
end;

procedure TSsCustomCipherComponent.DecryptIntf(ABuf: ISecretKey);
begin
  CheckKeyNotSet('DecryptIntf',True);
  DoCipherText(ABuf.Key^,ABuf.KeyLen);
  FCipher.Decrypt(ABuf.Key^,ABuf.KeyLen);
  CheckInplaceCleanUp(ABuf.KeyLen);
end;

procedure TSsCustomCipherComponent.DecryptPtr(ABuf: Pointer;
  ABufLen: Integer);
begin
  CheckKeyNotSet('DecryptPtr',True);
  DoCipherText(ABuf^,ABufLen);
  FCipher.Decrypt(ABuf^,ABufLen);
  CheckInplaceCleanUp(ABufLen);
end;

procedure TSsCustomCipherComponent.DecryptPtrToDst(ABuf: Pointer;
  ABufLen: Integer);
begin
  DecryptToDst(ABuf^,ABufLen);
end;

function TSsCustomCipherComponent.DecryptStr(const ABuf: string): string;
begin
  CheckKeyNotSet('DecryptStr',True);
  DoCipherText(Pointer(ABuf)^,Length(ABuf));
  Result := FCipher.DecryptStr(ABuf);
  CheckInplaceCleanUp(Length(Result));
end;

function TSsCustomCipherComponent.DecryptStreamToDst(ABuf: TStream;
  Count: Integer): Integer;
var
  B: ISecretKey;
  L: Integer;
begin
  if Count = 0 then
    ABuf.Position := 0;
  if Count <= 0 then
    Count := ABuf.Size - ABuf.Position;
  B := TSecretKey.Create('');
  if BufferSize = 0 then
    B.SetLength(Count)
  else
    B.SetLength(BufferSize);
  Result := Count;
  while Count > 0 do begin
    L := ABuf.Read(B.Key^,B.KeyLen);
    DecryptToDst(B.Key^,L);
    Dec(Count,L);
  end;
end;

procedure TSsCustomCipherComponent.DecryptStrToDst(const ABuf: string);
begin
  DecryptToDst(Pointer(ABuf)^,Length(ABuf));
end;

procedure TSsCustomCipherComponent.DecryptToDst(const ABuf; ABufLen: Integer);
var
  B: ISecretKey;
  NewLen,DLen: Integer;
begin
  CheckKeyNotSet('DecryptToDst',False);
  if InternalDirection = cdAuto then
    InternalDirection := cdDecrypt
  else if InternalDirection = cdEncrypt then
    raise ESecComp.Create(Format(S_ERR_WrongDirection,['DecryptToDst']));
  DoCipherText(ABuf,ABufLen);
  if Assigned(FInputBlock) then begin
    B := TSecretKey.Create('');
    NewLen := FInputBlock.KeyLen + ABufLen;
    DLen := NewLen mod FCipher.BlockSize;
    if DLen = 0 then DLen := FCipher.BlockSize;
    NewLen := NewLen - DLen;
    if NewLen > 0 then begin
      B.SetLength(NewLen);
      B.SetKeyAt(FInputBlock,0);
      Move(ABuf,B.KeyBytes[FInputBlock.KeyLen],ABufLen - DLen);
      FCipher.DecryptIntf(B);
      FDestinationStream.Write(B.Key^,NewLen);
      FInputBlock := TSecretKey.Create('');
      FInputBlock.SetKey(Ptr(LongInt(@ABuf) + ABufLen - DLen),DLen,0);
    end else begin
      NewLen := FInputBlock.KeyLen + ABufLen;
      B.SetLength(NewLen);
      B.SetKeyAt(FInputBlock,0);
      Move(ABuf,B.KeyBytes[FInputBlock.KeyLen],ABufLen);
      FInputBlock := B;
    end;
  end else begin
    B := TSecretKey.Create('');
    DLen := ABufLen mod FCipher.BlockSize;
    if DLen = 0 then DLen := FCipher.BlockSize;
    NewLen := ABufLen - DLen;
    if NewLen > 0 then begin
      B.SetKey(@ABuf,NewLen,0);
      FCipher.DecryptIntf(B);
      FDestinationStream.Write(B.Key^,NewLen);
    end;
    FInputBlock := TSecretKey.Create('');
    FInputBlock.SetKey(Ptr(LongInt(@ABuf) + NewLen),DLen,0);
  end;
end;

destructor TSsCustomCipherComponent.Destroy;
var
  Intf: ISecretKey;
begin
  Intf := FPBKDF;
  Intf._Release;
  inherited;
end;

procedure TSsCustomCipherComponent.DoCipherText(const ABuf;
  ABufLen: Integer);
begin
  if Assigned(FMAC) then
    FMAC.HashData(ABuf,ABufLen);
end;

function TSsCustomCipherComponent.DoneToDst: Boolean;
var
  B: ISecretKey;
  I: Integer;
  C: Byte;
begin
  CheckKeyNotSet('DoneToDst',False);
  Result := False;
  if InternalDirection = cdEncrypt then begin
    Result := True;
    if not FPadding then begin
      B := FInputBlock;
      FInputBlock := nil;
    end else begin
      B := TSecretKey.Create('');
      B.SetLength(FCipher.BlockSize);
      B.SetKeyAt(FInputBlock,0);
      C := FCipher.BlockSize - FInputBlock.KeyLen;
      for I := FInputBlock.KeyLen to B.KeyLen - 1 do
        B.KeyBytes[I] := C;
    end;
    FCipher.EncryptIntf(B);
    DoCipherText(B.Key^,B.KeyLen);
    FDestinationStream.Write(B.Key^,B.KeyLen);
    CleanUp;
  end else if InternalDirection = cdDecrypt then begin
    B := FInputBlock;
    FInputBlock := nil;
    Result := B.KeyLen > 0;
    if Result then begin
      FCipher.DecryptIntf(B);
      if not FPadding then begin
        Result := True;
        FDestinationStream.Write(B.Key^,B.KeyLen);
      end else begin
        Result := (B.KeyLen mod FCipher.BlockSize) = 0;
        if Result then begin
          C := B.KeyBytes[B.KeyLen-1];
          Result := (C > 0) and (C <= FCipher.BlockSize);
          if Result then
            for I := B.KeyLen - C to B.KeyLen - 1 do
              Result := Result and (B.KeyBytes[I] = C);
          if Result then
            FDestinationStream.Write(B.Key^,B.KeyLen - C)
          else
            FDestinationStream.Write(B.Key^,B.KeyLen);
        end;
      end;
    end;
    CleanUp;
  end;
  if Assigned(FMAC) then
    FMAC.Done(nil);
end;

procedure TSsCustomCipherComponent.Encrypt(var ABuf;
  ABufLen: Integer);
begin
  CheckKeyNotSet('Encrypt',True);
  FCipher.Encrypt(ABuf,ABufLen);
  DoCipherText(ABuf,ABufLen);
  CheckInplaceCleanUp(ABufLen);
end;

procedure TSsCustomCipherComponent.EncryptIntf(ABuf: ISecretKey);
begin
  CheckKeyNotSet('EncryptIntf',True);
  FCipher.Encrypt(ABuf.Key^,ABuf.KeyLen);
  DoCipherText(ABuf.Key^,ABuf.KeyLen);
  CheckInplaceCleanUp(ABuf.KeyLen);
end;

procedure TSsCustomCipherComponent.EncryptPtr(ABuf: Pointer;
  ABufLen: Integer);
begin
  CheckKeyNotSet('EncryptPtr',True);
  FCipher.Encrypt(ABuf^,ABufLen);
  DoCipherText(ABuf^,ABufLen);
  CheckInplaceCleanUp(ABufLen);
end;

procedure TSsCustomCipherComponent.EncryptPtrToDst(ABuf: Pointer;
  ABufLen: Integer);
begin
  EncryptToDst(ABuf^,ABufLen);
end;

function TSsCustomCipherComponent.EncryptStr(const ABuf: string): string;
begin
  CheckKeyNotSet('EncryptStr',True);
  Result := FCipher.EncryptStr(ABuf);
  DoCipherText(Pointer(ABuf)^,Length(ABuf));
  CheckInplaceCleanUp(Length(ABuf));
end;

function TSsCustomCipherComponent.EncryptStreamToDst(ABuf: TStream;
  Count: Integer): Integer;
var
  B: ISecretKey;
  L: Integer;
begin
  if Count = 0 then
    ABuf.Position := 0;
  if Count <= 0 then
    Count := ABuf.Size - ABuf.Position;
  B := TSecretKey.Create('');
  if BufferSize = 0 then
    B.SetLength(Count)
  else
    B.SetLength(BufferSize);
  Result := Count;
  while Count > 0 do begin
    L := ABuf.Read(B.Key^,B.KeyLen);
    EncryptToDst(B.Key^,L);
    Dec(Count,L);
  end;
end;

procedure TSsCustomCipherComponent.EncryptStrToDst(const ABuf: string);
begin
  EncryptToDst(Pointer(ABuf)^,Length(ABuf));
end;

procedure TSsCustomCipherComponent.EncryptToDst(const ABuf; ABufLen: Integer);
var
  B: ISecretKey;
  NewLen,DLen: Integer;
begin
  CheckKeyNotSet('EncryptToDst',False);
  if InternalDirection = cdAuto then
    InternalDirection := cdEncrypt
  else if InternalDirection = cdDecrypt then
    raise ESecComp.Create(Format(S_ERR_WrongDirection,['EncryptToDst']));
  if Assigned(FInputBlock) then begin
    B := TSecretKey.Create('');
    NewLen := FInputBlock.KeyLen + ABufLen;
    DLen := NewLen mod FCipher.BlockSize;
    NewLen := NewLen - DLen;
    B.SetLength(NewLen);
    B.SetKeyAt(FInputBlock,0);
    Move(ABuf,B.KeyBytes[FInputBlock.KeyLen],ABufLen - DLen);
    FCipher.EncryptIntf(B);
    DoCipherText(B.Key^,NewLen);
    FDestinationStream.Write(B.Key^,NewLen);
    if DLen = 0 then
      FInputBlock := nil
    else begin
      FInputBlock := TSecretKey.Create('');
      FInputBlock.SetKey(Ptr(LongInt(@ABuf) + ABufLen - DLen),DLen,0);
    end;
  end else begin
    B := TSecretKey.Create('');
    DLen := ABufLen mod FCipher.BlockSize;
    NewLen := ABufLen - DLen;
    B.SetKey(@ABuf,NewLen,0);
    FCipher.EncryptIntf(B);
    DoCipherText(B.Key^,NewLen);
    FDestinationStream.Write(B.Key^,NewLen);
    if DLen = 0 then
      FInputBlock := nil
    else begin
      FInputBlock := TSecretKey.Create('');
      FInputBlock.SetKey(Ptr(LongInt(@ABuf) + NewLen),DLen,0);
    end;
  end;
end;

function TSsCustomCipherComponent.GetCipherClass: TCipherClass;
begin
  Result := SecUtils.FindCipherClass(FAlgorithm,FMode);
end;

function TSsCustomCipherComponent.GetIVector: OctetString;
begin
  if Assigned(FCipher) then
    Result := FCipher.IVector
  else
    Result := '';
end;


function TSsCustomCipherComponent.GetMode: TCipherMode;
begin
  if FCipher = nil then
    Result := FMode
  else
    Result := FCipher.Mode;
end;

function TSsCustomCipherComponent.GetVectorSize: Integer;
var
  CC: TCipherClass;
begin
  CC := GetCipherClass;
  if Assigned(CC) and CC.InheritsFrom(TBlockCipher) then
    Result := TBlockCipherClass(CC).BlockVectorSize
  else
    Result := 0;
end;

procedure TSsCustomCipherComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FKeyRing then
      FKeyRing := nil;
    if AComponent = FMAC then
      FMAC := nil;
  end;
end;

procedure TSsCustomCipherComponent.SetAlgorithm(const Value: TCipherAlg);
begin
  if FCipher = nil then
    FAlgorithm := Value;
end;

procedure TSsCustomCipherComponent.SetBufferSize(const Value: Integer);
begin
  FBufferSize := Value;
end;

procedure TSsCustomCipherComponent.SetDestinationStream(const Value: TStream);
begin
  FDestinationStream := Value;
end;

procedure TSsCustomCipherComponent.SetDirection(
  const Value: TCipherDirection);
begin
  if FCipher = nil then
    FDirection := Value;
end;

procedure TSsCustomCipherComponent.SetInternalDirection(
  const Value: TCipherDirection);
begin
  FInternalDirection := Value;
end;

procedure TSsCustomCipherComponent.SetIVector(const Value: OctetString);
begin
  if Assigned(FCipher) then begin
    FCipher.IVector := Value;
    FMustSetIV := False;
  end;
end;

procedure TSsCustomCipherComponent.SetKeyBuf(const AKey; AKeyLen: Integer);
begin
  CleanUp;
  if KeyedIV then
    FCipher := GetCipherClass.Create(AKey,AKeyLen,GetVectorSize)
  else
    FCipher := GetCipherClass.Create(AKey,AKeyLen,0);
end;

procedure TSsCustomCipherComponent.SetKeyedIV(const Value: Boolean);
begin
  if FCipher = nil then begin
    FKeyedIV := Value;
    FMustSetIV := not Value;
  end;
end;

function TSsCustomCipherComponent.SetKeyFromKeyRing: Boolean;
var
  MACKey: ISecretKey;
begin
  CleanUp;
  Result := Assigned(FKeyRing) and (FKeyIdentifier <> '');
  if Result then begin
    if Assigned(FMAC) then begin
      MACKey := TSecretKey.Create('');
      FCipher := FKeyRing.FindCreateCipher(KeyIdentifier,FAlgorithm,FMode,FKeyedIV,FParam,MACKey);
      FMAC.SetKeyIntf(MACKey);
    end else
      FCipher := FKeyRing.FindCreateCipher(KeyIdentifier,FAlgorithm,FMode,FKeyedIV,FParam);
  end;
end;

procedure TSsCustomCipherComponent.SetKeyIdentifier(
  const Value: OctetString);
begin
  if FCipher = nil then
    FKeyIdentifier := Value;
end;

procedure TSsCustomCipherComponent.SetKeyIntf(AKey: ISecretKey);
begin
  CleanUp;
  FCipher := GetCipherClass.CreateIntf(AKey);
end;

procedure TSsCustomCipherComponent.SetKeyPassword(APassword: ISecretKey;
  AKeyLen: Integer);
var
  P: string;
  Intf: ISecretKey;
begin
  Assert(Assigned(PBKDF),
         'You must assign a TPBKDF ancestor instance to property PBKDF');
  if Param = '' then begin
    SetLength(FParam,AKeyLen);
    RawRandom(Pointer(FParam)^,AKeyLen*8);
    P := FParam;
  end else
    P := FParam;
  if Assigned(FMAC) then begin
    PBKDF.DeriveKey(APassword,
                    TSecretKey.CreateStr(PChar(P),Length(P)),
                    AKeyLen * 2);
    SetKeyBuf(PBKDF.KeyBytes[0],AKeyLen);
    FMAC.SetKeyBuf(PBKDF.KeyBytes[AKeyLen],AKeyLen);
    Intf := FPBKDF;
    Intf._Release;
  end else begin
    PBKDF.DeriveKey(APassword,
                    TSecretKey.CreateStr(PChar(P),Length(P)),
                    AKeyLen);
    SetKeyIntf(PBKDF);
    Intf := FPBKDF;
    Intf._Release;
  end;
  FPBKDF := FPBKDFClass.Create('');
  Intf := FPBKDF;
  Intf._AddRef;
end;

procedure TSsCustomCipherComponent.SetKeyPtr(AKey: Pointer;
  AKeyLen: Integer);
begin
  CleanUp;
  if KeyedIV then
    FCipher := GetCipherClass.Create(AKey^,AKeyLen,GetVectorSize)
  else
    FCipher := GetCipherClass.Create(AKey^,AKeyLen,0);
end;

function TSsCustomCipherComponent.SetKeyRandom(AKeyLen: Integer): Boolean;
var             
  CC: TCipherClass;
  K: ISecretKey;
begin
  CC := GetCipherClass;
  Result := Assigned(CC) and Assigned(FKeyRing) and (FKeyIdentifier <> '');
  if Result then begin
    RawRandomIntf(K,AKeyLen*8);
    case CC.Algorithm of
      caRijndael:
        Result := FKeyRing.AddPrivateAESKey(K.Key^,K.KeyLen,True,KeyIdentifier) >= 0;
      caTwoFish:
        Result := FKeyRing.AddPrivateTwoFishKey(K.Key^,K.KeyLen,True,KeyIdentifier) >= 0;
      caBlowFish:
        Result := FKeyRing.AddPrivateBlowFishKey(K.Key^,K.KeyLen,True,KeyIdentifier) >= 0;
      caARC4:
        Result := FKeyRing.AddPrivateARC4Key(K.Key^,K.KeyLen,True,KeyIdentifier) >= 0;
      caDES:
        Result := FKeyRing.AddPrivate3DESKey(K.Key^,K.KeyLen,True,KeyIdentifier) >= 0;
    else
      Result := False;
    end;
    if Result then
      Result := SetKeyFromKeyRing;
  end;
end;

procedure TSsCustomCipherComponent.SetKeyRing(
  const Value: TSsKeyRingComponent);
begin
  FKeyRing := Value;
end;

procedure TSsCustomCipherComponent.SetKeyStr(const AKey: string;
  AFormat: TStringFormat = sfBinary);
var
  K: string;
  C: Integer;
begin
  case AFormat of
    sfBinary:
      SetKeyPtr(Pointer(AKey),Length(AKey));
    sfHex:
      begin
        C := (Length(AKey) + 1) shr 1;
        SetLength(K,C);
        HexToBin(AKey,Pointer(K)^,C);
        SetKeyPtr(Pointer(K),C);
        ProtectClear(Pointer(K)^,Length(K));
      end;
    sfBase64:
      begin
        K := MIME64ToStr(AKey);
        SetKeyPtr(Pointer(K),Length(K));
        ProtectClear(Pointer(K)^,Length(K));
      end;
  end;
end;

procedure TSsCustomCipherComponent.SetMAC(const Value: TSsHash);
begin
  FMAC := Value;
end;

procedure TSsCustomCipherComponent.SetMode(const Value: TCipherMode);
begin
  if FCipher = nil then begin
    FMode := Value;
    FPadding := Value in [cmECB,cmCBC,cmABC,cmPipedPCFB];
  end;
end;

procedure TSsCustomCipherComponent.SetName(const NewName: TComponentName);
var
  DoSetNewKI: Boolean;
  NewKI: OctetString;
begin
  DoSetNewKI := FKeyIdentifier = Name;
  if DoSetNewKI then
    NewKI := NewName;
  inherited;
  if DoSetNewKI then
    SetKeyIdentifier(NewKI);
end;

procedure TSsCustomCipherComponent.SetPadding(const Value: Boolean);
begin
  FPadding := Value;
end;

procedure TSsCustomCipherComponent.SetParam(const Value: OctetString);
begin
  FParam := Value;
end;

procedure TSsCustomCipherComponent.SetPBKDFClass(const Value: TPBKDFClass);
var
  Intf: ISecretKey;
begin
  if Value <> FPBKDFClass then begin
    Assert(Assigned(Value));
    if Assigned(FPBKDF) then begin
      Intf := FPBKDF;
      Intf._Release;
    end;
    FPBKDFClass := Value;
    FPBKDF := Value.Create('');
    Intf := FPBKDF;
    Intf._AddRef;
  end;
end;

{ TSsHash }

procedure TSsHash.CleanUp;
begin
  FHMACKey := nil;
  ProtectClear(Pointer(FDigest)^,Length(FDigest));
  FDigest := '';
  FHash.Free;
end;

procedure TSsHash.Done(Digest: Pointer; DigestLen: Integer);
begin
  if not Assigned(FHash) then SetUp;
  if DigestLen > FHash.DigestSize then
    DigestLen := FHash.DigestSize;
  if Assigned(FHMACKey) then begin
    if DigestLen < 0 then DigestLen := FHash.DigestSize;
    SetLength(FDigest,DigestLen);
    HMACEnd(FHMACKey.Key^,FHMACKey.KeyLen,FHash,Pointer(FDigest)^,DigestLen);
  end else begin
    FHash.Done(nil);
    FDigest := FHash.Digest;
  end;
  if Assigned(Digest) then
    Move(Pointer(FDigest)^,Digest^,DigestLen);
end;

procedure TSsHash.GetContext(Context: Pointer);
begin
  if not Assigned(FHash) then SetUp;
  FHash.GetContext(Context);
end;

function TSsHash.GetDigest: string;
begin
  if Assigned(FHash) and (FDigest = '') then
    Result := FHash.Digest
  else
    Result := FDigest;
end;

procedure TSsHash.HashData(const AData; Count: Int64);
begin
  FHash.HashData(AData,Count);
end;

procedure TSsHash.HashIntf(AData: ISecretKey);
begin
  FHash.HashData(AData.Key^,AData.KeyLen);
end;

procedure TSsHash.HashPtr(AData: Pointer; Count: Int64);
begin
  FHash.HashData(AData^,Count);
end;

procedure TSsHash.HashStr(const AData: string);
begin
  FHash.HashData(Pointer(AData)^,Length(AData));
end;

procedure TSsHash.HashStream(AData: TStream; Count: Integer);
var
  B: ISecretKey;
begin
  B := TSecretKey.Create('');
  if Count = 0 then
    AData.Position := 0;
  if Count <= 0 then
    Count := AData.Size - AData.Position;
  B.SetLength(Count);
  AData.Read(B.Key^,Count);
  FHash.HashData(B.Key^,Count);
end;

procedure TSsHash.SetHashAlgorithm(const Value: THashAlgorithm);
begin
  FHashAlgorithm := Value;
end;

procedure TSsHash.SetKeyBuf(const AKey; AKeyLen: Integer);
begin
  if AKeyLen = 0 then
    SetKeyIntf(nil)
  else
    SetKeyIntf(TSecretKey.CreateStr(@AKey,AKeyLen));
end;

procedure TSsHash.SetKeyIntf(AKey: ISecretKey);
begin
  CleanUp;
  FHMACKey := AKey;
  FHash := FindHashClass(FHashAlgorithm).Create(nil^,0);
  if Assigned(AKey) then
    HMACBegin(AKey.Key^,AKey.KeyLen,FHash);
end;

procedure TSsHash.SetKeyPtr(AKey: Pointer; AKeyLen: Integer);
begin
  if AKeyLen = 0 then
    SetKeyIntf(nil)
  else
    SetKeyIntf(TSecretKey.CreateStr(AKey,AKeyLen));
end;

procedure TSsHash.SetKeyStr(const AKey: string; AFormat: TStringFormat);
var
  K: string;
  C: Integer;
begin
  if AKey = '' then
    SetKeyIntf(nil)
  else
    case AFormat of
      sfBinary:
        SetKeyIntf(TSecretKey.CreateStr(PChar(AKey),Length(AKey)));
      sfHEX:
        begin
          C := (Length(AKey) + 1) shr 1;
          SetLength(K,C);
          HexToBin(AKey,Pointer(K)^,C);
          SetKeyIntf(TSecretKey.CreateStr(PChar(K),C));
          ProtectClear(Pointer(K)^,Length(K));
        end;
      sfBase64:
        begin
          K := MIME64ToStr(AKey);
          SetKeyIntf(TSecretKey.CreateStr(PChar(K),Length(K)));
          ProtectClear(Pointer(K)^,Length(K));
        end;
    end;
end;

procedure TSsHash.SetUp;
begin
  CleanUp;
  FHash := FindHashClass(FHashAlgorithm).Create(nil^,0);
  if Assigned(FHMACKey) then
    HMACBegin(FHMACKey.Key^,FHMACKey.KeyLen,FHash);
end;

procedure TSsHash.SetUpContext(Context: Pointer);
begin
  if not Assigned(FHash) then SetUp;
  FHash.SetUpContext(Context);
end;

{ TSsAES }

constructor TSsAES.Create(AOwner: TComponent);
begin
  inherited;
  Algorithm := caRijndael;
  Mode := cmCTR;
end;

{ TSsRC4 }

constructor TSsRC4.Create(AOwner: TComponent);
begin
  inherited;
  Algorithm := caARC4;
  Mode := cmOFB;
end;

{ TSsDES }

constructor TSsDES.Create(AOwner: TComponent);
begin
  inherited;
  Algorithm := caDES;     
  Mode := cmCTR;
end;

{ TSsTwoFish }

constructor TSsTwoFish.Create(AOwner: TComponent);
begin
  inherited;
  Algorithm := caTwoFish;
  Mode := cmCTR;
end;

{ TSsBlowFish }

constructor TSsBlowFish.Create(AOwner: TComponent);
begin
  inherited;
  Algorithm := caBlowFish;
  Mode := cmCTR;
end;

end.
