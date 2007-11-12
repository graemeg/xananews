{$I ver.inc}
{$B-,O+,Q-,R-}
{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     CAPIExtKey Unit                                   }
{     CryptoAPI RSA support                             }
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
unit CAPIExtKey {$IFDEF D6UP}platform{$ENDIF};

interface

uses
  Windows, SysUtils, SecUtils, MpArithTypes, MpIF;

const
  DLLName = 'advapi32.dll';

type
  // The purpose of the class TCAPIIFPrivateKey is to serve as a wrapper for
  // RSA keys stored in hardware CSPs.

  TCAPIIFPrivateKey = class(TExternalIFPrivateKey)
  private
    FStoreHandle: DWORD;
    FKeyHandle: DWORD;
    FImpKeyHandle: DWORD;
    FProviderName: string;
    FContainerName: string;
    FProvType: LongInt;
    FProvFlags: LongInt;
    FKeySpec: LongInt;
    FRaiseError: Boolean;
    FKeySize: LongInt;
  protected
    function Check(ARes: BOOL): Boolean;
    procedure RaiseError(ARes: BOOL);
    function GetIdentifier: string; override;
    // These methods are used by the routines in MPIF.
    function GetKeySize: Integer; override;
    function IFPrivateKeyOperation(I: PMPInteger;
                                   var J: PMPInteger): Boolean; override;
    function SignMsg(const Msg; MsgCount: Integer;
                     HashAlgorithm: THashAlgorithm;
                     MGFHashAlgorithm: THashAlgorithm;
                     Encoding: TSignEncoding;
                     var Sign; var SignCount: Integer): Boolean; override;
    function SignDigest(const Digest; DigestCount: Integer;
                        HashAlgorithm: THashAlgorithm;
                        MGFHashAlgorithm: THashAlgorithm;
                        Encoding: TSignEncoding;
                        NullMsg: Boolean;
                        var Sign; var SignCount: Integer): Boolean; override;
    function IFESDecryption(const EMsg; EMsgCount: Integer;
                            const P; PCount: Integer;
                            HashAlgorithm: THashAlgorithm;
                            MGFHashAlgorithm: THashAlgorithm;
                            EncryptEncoding: TEncryptEncoding;
                            var Msg; var MsgLen: Integer;
                            var Success: Boolean): Boolean; override;
    function Validate(const APubl: TIFPublicKey): Boolean; override;
  public
    constructor Create(ARaiseError: Boolean = False); reintroduce;
    destructor Destroy; override;
    // CryptAcquireContext wrapper:
    function AcquireContext(const Container, Provider: string;
                            ProvType: LongInt;
                            Flags: LongInt): Boolean;
    // CryptGetUserKey wrapper:
    function GetUserKey(KeySpec: LongInt): Boolean;
    // ImportKey is called by TStreamSecII when the key is returned from a
    // private key ring. This implementation does NOT call the CAPI routine with
    // the same name. Instead it calls AcquireContext and GetUserKey.
    function ImportKey(const PKIdentifier; PKILen: Integer): Boolean; override;
    // NOTE: The Retr... and Stor... methods are added for completeness only.
    function RetrPrivKey(var PrivKey: TIFPrivateKey): Boolean;
    function RetrPublKey(var PublKey: TIFPublicKey): Boolean;
    function StorPrivKey(const PrivKey: TIFPrivateKey): Boolean;
    function StorPublKey(const PublKey: TIFPublicKey): Boolean;
    // Use Identifier to identify the key in a TStreamSecII private key ring.
    // Identifier equals the comma separated text of the CAPI parameters.
    property Identifier: string read GetIdentifier;
  end;

function CryptAcquireContext(var hProv    :DWORD;
                             pszContainer :PAnsiChar;
                             pszProvider  :PAnsiChar;
                             dwProvType   :LongInt;
                             dwFlags      :LongInt) :BOOL; stdcall;
function CryptReleaseContext(hProv   :DWORD;
                             dwFlags :LongInt) :BOOL; stdcall;
function CryptGenKey(hProv     :DWORD;
                     Algid     :DWORD;
                     dwFlags   :LongInt;
                     var hKey  :DWORD) :BOOL; stdcall;
function CryptExportKey(hKey          :DWORD;
                        hExpKey       :DWORD;
                        dwBlobType    :LongInt;
                        dwFlags       :LongInt;
                        pbData        :PBYTE;
                        var dwDataLen :LongInt) :BOOL; stdcall;
function CryptImportKey(hProv     :DWORD;
                        pbData    :PBYTE;
                        dwDataLen :LongInt;
                        hPubKey   :DWORD;
                        dwFlags   :LongInt;
                        var phKey :DWORD) :BOOL; stdcall;
function CryptGetUserKey(hProv        :DWORD;
                         dwKeySpec    :LongInt;
                         var hUserKey :DWORD) :BOOL; stdcall;
function CryptGetKeyParam(hKey           :DWORD;
                          dwParam        :LongInt;
                          pbData         :PBYTE;
                          var pdwDataLen :LongInt;
                          dwFlags        :LongInt) :BOOL; stdcall;
function CryptDestroyKey(hKey :DWORD) :BOOL; stdcall;
function CryptDecrypt(hKey       :DWORD;
                      hHash      :DWORD;
                      Final      :Bool;
                      dwFlags    :LongInt;
                      pbData     :PBYTE;
                      var dwDataLen :LongInt) :BOOL; stdcall;
function CryptCreateHash(hProv     :DWORD;
                         Algid     :DWORD;
                         hKey      :DWORD;
                         dwFlags   :LongInt;
                         var hHash :DWORD) :BOOL; stdcall;
function CryptHashData(hHash     :DWORD;
                       pbData    :PBYTE;
                       dwDataLen :LongInt;
                       dwFlags   :LongInt) :BOOL; stdcall;
function CryptDestroyHash(hHash :DWORD) :BOOL; stdcall;
function CryptSignHash(hHash        :DWORD;
                       dwKeySpec    :LongInt;
                       sDescription :PAnsiChar;
                       dwFlags      :LongInt;
                       pbSignature  :PBYTE;
                       var dwSigLen :LongInt) :BOOL; stdcall;

function CryptGenRandom(hProv       :DWORD;
                        dwLen       :DWORD;
                        pbBuffer    :PBYTE) :BOOL; stdcall;


implementation

uses
  Classes, MpArith, MpIFConversion;

function CryptAcquireContext; external DLLName name 'CryptAcquireContextA';
function CryptReleaseContext; external DLLName name 'CryptReleaseContext';
function CryptGenKey; external DLLName name 'CryptGenKey';
function CryptExportKey; external DLLName name 'CryptExportKey';
function CryptImportKey; external DLLName name 'CryptImportKey';
function CryptGetUserKey; external DLLName name 'CryptGetUserKey';
function CryptGetKeyParam; external DLLName name 'CryptGetKeyParam';
function CryptDestroyKey; external DLLName name 'CryptDestroyKey';
function CryptDecrypt; external DLLName name 'CryptDecrypt';
function CryptCreateHash; external DLLName name 'CryptCreateHash';
function CryptHashData; external DLLName name 'CryptHashData';
function CryptDestroyHash; external DLLName name 'CryptDestroyHash';
function CryptSignHash; external DLLName name 'CryptSignHashA';     
function CryptGenRandom; external DLLName name 'CryptGenRandom';

{ TCAPIIFPrivateKey }

function TCAPIIFPrivateKey.AcquireContext(const Container,
  Provider: string; ProvType, Flags: Integer): Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if FKeyHandle <> 0 then
    CryptDestroyKey(FKeyHandle);
  FKeyHandle := 0;
  if FStoreHandle <> 0 then
    CryptReleaseContext(FStoreHandle,0);
  Res := CryptAcquireContext(FStoreHandle,PChar(Container),PChar(Provider),ProvType,Flags);
  if Res then begin
    FProviderName := Provider;
    FContainerName := Container;
    FProvType := ProvType;
    FProvFlags := Flags;
    Result := True;
  end else begin
    FStoreHandle := 0;
    RaiseError(Res);
  end;
end;

function TCAPIIFPrivateKey.Check(ARes: BOOL): Boolean;
begin
  Result := ARes;
  RaiseError(ARes);
end;

constructor TCAPIIFPrivateKey.Create(ARaiseError: Boolean);
begin
  inherited Create;
  FRaiseError := ARaiseError;
end;

destructor TCAPIIFPrivateKey.Destroy;
begin
  FRaiseError := False;
  if FKeyHandle <> 0 then
    CryptDestroyKey(FKeyHandle);
  if FImpKeyHandle <> 0 then
    CryptDestroyKey(FImpKeyHandle);
  if FStoreHandle <> 0 then
    CryptReleaseContext(FStoreHandle,0);
  inherited;
end;

function TCAPIIFPrivateKey.GetIdentifier: string;
var
  SL: TStringList;
begin
  if (FStoreHandle = 0) or (FKeyHandle = 0) then
    Result := ''
  else begin
    SL := TStringList.Create;
    try
      SL.Add(FProviderName);
      SL.Add(FContainerName);
      SL.Add(IntToStr(FProvType));
      SL.Add(IntToStr(FProvFlags));
      SL.Add(IntToStr(FKeySpec));
      Result := SL.CommaText;
    finally
      SL.Free;
    end;
  end;
end;

const
  KP_BLOCKLEN            = 8;

function TCAPIIFPrivateKey.GetKeySize: Integer;
var
  Len: LongInt;
  Res: BOOL;
begin
  Result := FKeySize;
  if Result <= 0 then begin
    Len := SizeOf(Result);
    // This will fail for signature only keys:
    Res := CryptGetKeyParam(FKeyHandle,KP_BLOCKLEN,@Result,Len,0);
    if not Res then
      Result := 0
    else
      FKeySize := Result;
  end;
end;

function TCAPIIFPrivateKey.GetUserKey(KeySpec: Integer): Boolean;
begin
  if (KeySpec = FKeySpec) and (FKeyHandle <> 0) and (FStoreHandle <> 0) then begin
    Result := True;
    Exit;
  end;
  if FKeyHandle <> 0 then
    Check(CryptDestroyKey(FKeyHandle));
  FKeyHandle := 0;
  FKeySpec := 0;
  Result := Check(CryptGetUserKey(FStoreHandle,KeySpec,FKeyHandle));
  if Result then
    FKeySpec := KeySpec;
end;

function TCAPIIFPrivateKey.IFESDecryption(const EMsg; EMsgCount: Integer;
  const P; PCount: Integer; HashAlgorithm,
  MGFHashAlgorithm: THashAlgorithm; EncryptEncoding: TEncryptEncoding;
  var Msg; var MsgLen: Integer; var Success: Boolean): Boolean;
var
  B, S, E: PByte;
begin
  Result := True;
  GetMem(B,EMsgCount);
  try
    S := @EMsg;
    E := Ptr(LongInt(B) + EMsgCount - 1);
    while LongInt(B) <= LongInt(E) do begin
      E^ := S^;
      Dec(E);
      Inc(S);
    end;
    Success := Check(CryptDecrypt(FKeyHandle,0,True,0,B,EMsgCount));
    if Success then begin
      if EMsgCount > MsgLen then
        Success := False
      else begin
        MsgLen := EMsgCount;
        Move(B^,Msg,MsgLen);
      end;
    end;
  finally
    FreeMem(B);
  end;
end;

function TCAPIIFPrivateKey.IFPrivateKeyOperation(I: PMPInteger;
  var J: PMPInteger): Boolean;
begin
  Result := inherited IFPrivateKeyOperation(I,J);
end;

function TCAPIIFPrivateKey.ImportKey(const PKIdentifier;
  PKILen: Integer): Boolean;
var
  Id: string;
  SL: TStringList;
  ProvType, ProvFlags, KeySpec: LongInt;
  Code: Integer;
begin
  SL := TStringList.Create;
  try
    SetLength(Id,PKILen);
    Move(PKIdentifier,Pointer(Id)^,PKILen);
    SL.CommaText := Id;
    Result := SL.Count >= 5;
    if Result then begin
      Val(SL[2],ProvType,Code);
      Result := Code = 0;
      if Result then begin
        Val(SL[3],ProvFlags,Code);
        Result := Code = 0;
        if Result then begin
          Val(SL[4],KeySpec,Code);
          Result := Code = 0;
          if Result then begin
            Result := AcquireContext(SL[1],SL[0],ProvType,ProvFlags);
            if Result then
              Result := GetUserKey(KeySpec);
          end;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TCAPIIFPrivateKey.RaiseError(ARes: BOOL);
begin
  if FRaiseError then
    Win32Check(ARes);
end;

function TCAPIIFPrivateKey.RetrPrivKey(
  var PrivKey: TIFPrivateKey): Boolean;
var
  KeyLen, Len: Integer;
  Key: PRSAKeyBlob;
begin
  KeyLen := GetKeySize shr 3;
  Result := KeyLen > 0;
  if Result then begin
    Result := False;
    Len := 5000;
    GetMem(Key,Len);
    try
      if Check(CryptExportKey(FKeyHandle,0,PRIVATEKEYBLOB,0,Pointer(Key),Len)) then try
        Result := PRIVATEKEYBLOBToRSAPrivateKey(Key,Len,PrivKey);
      finally
        ProtectClear(Key^,Len);
      end;
    finally
      FreeMem(Key);
    end;
  end;
end;

function TCAPIIFPrivateKey.RetrPublKey(var PublKey: TIFPublicKey): Boolean;
var
  KeyLen, Len, I, J, PSize: Integer;
  Key: PRSAKeyBlob;
begin
  KeyLen := GetKeySize shr 3;
  Result := KeyLen > 0;
  if not Result then begin
    Result := Check(CryptExportKey(FKeyHandle,0,PUBLICKEYBLOB,0,nil,Len));
    if Result then
      KeyLen := Len - 20;
  end;
  if Result then begin
    Result := False;
    Len := 20 + KeyLen;
    GetMem(Key,Len);
    try
      if Check(CryptExportKey(FKeyHandle,0,PUBLICKEYBLOB,0,Pointer(Key),Len)) then try
        DisposeIFPublicKey(PublKey);

        PublKey.E := IntToMPInt(Key^.pubExp);

        J := 0;
        PSize := (KeyLen + 3) shr 2;
        MPRealloc(PublKey.N,PSize);
        for I := PSize - 1 downto 1 do begin
          Move(Key^.modulus[J],PublKey.N^.Data[I],4);
          Inc(J,4);
        end;
        PublKey.N^.Data[0] := 0;
        PublKey.N^.Sign := 1;
        Move(Key^.modulus[J],PublKey.N^.Data[0],KeyLen - J);

        FKeySize := KeyLen shl 3;

        Result := True;
      finally
        ProtectClear(Key^,Len);
      end;
    finally
      FreeMem(Key);
    end;
  end;
end;

function TCAPIIFPrivateKey.SignDigest(const Digest; DigestCount: Integer;
  HashAlgorithm, MGFHashAlgorithm: THashAlgorithm; Encoding: TSignEncoding;
  NullMsg: Boolean; var Sign; var SignCount: Integer): Boolean;
begin
  Result := inherited SignDigest(Digest,DigestCount,HashAlgorithm,MGFHashAlgorithm,Encoding,NullMsg,Sign,SignCount);
end;

const
  ALG_CLASS_HASH            = (4 shl 13);
  ALG_TYPE_ANY              = 0;
  ALG_SID_MD2               = 1;
  ALG_SID_MD5               = 3;
  ALG_SID_SHA               = 4;
  ALG_SID_RIPEMD160         = 7;
  CALG_SHA      = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA);
  CALG_MD2      = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD2);
  CALG_MD5      = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD5);
  CALG_RIPEMD160= (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_RIPEMD160);

function TCAPIIFPrivateKey.SignMsg(const Msg; MsgCount: Integer;
  HashAlgorithm, MGFHashAlgorithm: THashAlgorithm; Encoding: TSignEncoding;
  var Sign; var SignCount: Integer): Boolean;
var
  Hash: LongWord;
  Tmp: Byte;
  S, E: PByte;
begin
  case HashAlgorithm of
 {$IFDEF SHA1}
   haSHA1:      Result := CryptCreateHash(FStoreHandle,CALG_SHA,0,0,Hash);
 {$ENDIF}
 {$IFDEF MD2}
   haMD2:       Result := CryptCreateHash(FStoreHandle,CALG_MD2,0,0,Hash);
 {$ENDIF}
 {$IFDEF MD5}
   haMD5:       Result := CryptCreateHash(FStoreHandle,CALG_MD5,0,0,Hash);
 {$ENDIF}
 {$IFDEF RIPEMD160}
   haRipeMD160: Result := CryptCreateHash(FStoreHandle,CALG_RIPEMD160,0,0,Hash);
 {$ENDIF}
  else
    Result := False;
  end;
  if Result then begin
    Result := Check(CryptHashData(Hash,@Msg,MsgCount,0));
    if Result then begin
      Result := Check(CryptSignHash(Hash,FKeySpec,nil,0,@Sign,SignCount));
      if Result then begin
        S := @Sign;
        E := Ptr(LongInt(S) + SignCount - 1);
        while LongInt(S) < LongInt(E) do begin
          Tmp := E^;
          E^ := S^;
          S^ := Tmp;
          Dec(E);
          Inc(S);
        end;
      end;
    end;
    Check(CryptDestroyHash(Hash));
  end;
end;

function TCAPIIFPrivateKey.StorPrivKey(
  const PrivKey: TIFPrivateKey): Boolean;
var
  Len: Integer;
  Key: PRSAKeyBlob;
begin
  // IMPORTANT:
  // This method will only work as expected if:
  //
  // 1. PrivKey.ReprType = 1;
  //
  // 2. PrivKey.oP, PrivKey.oQ, PrivKey.oD1, PrivKey.oD2 and PrivKey.oC all
  //    have the same bit length and all have minimum PMPInteger.Size.
  //
  // 3. The public exponent corresponding to PrivKey has a bit length <= 32.

  Result := PrivKey.ReprType = 1;
  if not Result then Exit;

  if FKeyHandle <> 0 then
    CryptDestroyKey(FKeyHandle);
  FKeyHandle := 0;

  Result := RSAPrivateKeyToPRIVATEKEYBLOB(PrivKey,Key,Len);
  if Result then try
    Result := Check(CryptImportKey(FStoreHandle,Pointer(Key),Len,0,0,FKeyHandle));
  finally
    ProtectClear(Key^,Len);
    FreeMem(Key);
  end;
end;

function TCAPIIFPrivateKey.StorPublKey(
  const PublKey: TIFPublicKey): Boolean;
var
  KeyLen, Len, I, J, PSize: Integer;
  Key: PRSAKeyBlob;
begin
  KeyLen := (MPMSB(PublKey.N) + 7) shr 8;
  Result := KeyLen > 0;
  if Result then begin
    Len := 20 + KeyLen;
    GetMem(Key,Len);
    try
      Key^.bType := PUBLICKEYBLOB;
      Key^.bVersion := 2;
      Key^.reserved := 0;
      Key^.aiKeyAlg := CALG_RSA_KEYX;
      Key^.magic.A := 'RSA1';
      Key^.bitLen := MPMSB(PublKey.N);
      try
        Key^.pubExp := PublKey.E^.Data[PublKey.E^.Size - 1];

        J := 0;
        PSize := (KeyLen + 3) shr 2;
        for I := PSize - 1 downto 1 do begin
          Move(PublKey.N^.Data[I],Key^.modulus[J],4);
          Inc(J,4);
        end;
        Move(PublKey.N^.Data[0],Key^.modulus[J],KeyLen - J);

        Result := Check(CryptImportKey(FStoreHandle,Pointer(Key),Len,0,0,FImpKeyHandle));
      finally
        ProtectClear(Key^,Len);
      end;
    finally
      FreeMem(Key);
    end;
  end;
end;

function TCAPIIFPrivateKey.Validate(const APubl: TIFPublicKey): Boolean;
// This is faster, but doesn't check the CSP implementation:
var
  Publ: TIFPublicKey;
begin

  FillChar(Publ,SizeOf(Publ),0);
  try
    Result := RetrPublKey(Publ);
    if Result then
      Result := (MPCmpOffset(Publ.N,APubl.N) = 0) and
                (MPCmpOffset(Publ.E,APubl.E) = 0);
  finally
    DisposeIFPublicKey(Publ);
  end;
{
var
  Msg, EMsg: string;
  MsgLen, EMsgLen: Integer;
  Success: Boolean;
begin
  MsgLen := 20;
  Msg := StringOfChar('a',20);
  EMsgLen := GetKeySize shr 3;
  SetLength(EMsg,EMsgLen);
  IFESEncryption(APubl,Pointer(Msg)^,MsgLen,nil^,0,haSHA1,haSHA1,eeEME_PKCS1_V1_5,Pointer(EMsg)^,EMSgLen);
  Result := IFESDecryption(Pointer(EMsg)^,EMsgLen,nil^,0,haSHA1,haSHA1,eeEME_PKCS1_V1_5,Pointer(Msg)^,MsgLen,Success);
  Result := Result and Success;
  if Result then
    Result := Msg = StringOfChar('a',20);
}
end;

end.
