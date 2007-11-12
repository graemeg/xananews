{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SSL3 Unit                                         }
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
unit Ssl3;

interface
         
{$IFDEF SHA1_AND_MD5}
uses
  SecUtils, MpIF, MpDL, Asn1, X509Base, 
{$IFDEF BCB}
  TlsUtils,
{$ENDIF}
  Tls;

procedure SSL3MACBegin(const K; KLen: Integer; H: THash; Finished: Boolean);
procedure SSL3MACEnd(const K; KLen: Integer; H: THash; var Mac; MacLen: Integer);

function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: TIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: TDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: IMPIFPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: IMPDLPrivateKey;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: TIFPublicKey): Boolean; overload;
function SSL_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: TDLPublicKey): Boolean; overload;

procedure SSL_PRF(const Secret; SecretLen: Integer;
                  Seed: string;
                  var Key; KeyLen: Integer);
procedure SSL_KDF(const Secret; SecretLen: Integer;
                  ClientRandom: TRandomString;
                  ServerRandom: TRandomString;
                  HLen, KLen, EKLen, BLen: Integer;
                  Exportable: Boolean;
                  var Key: TTLSKeys); overload;
procedure SSL_KDF(var Params: TTLSSecurityParams;
                  var Key: TTLSKeys); overload;

function SSL_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     const DLPriv: TDLPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams;
                                     var Dst: PTLSHandshake): Integer; overload;
function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     const IFPriv: TIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     IFPriv: IMPIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     const DLPriv: TDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;

function SSL_Verify(const Params: TTLSSecurityParams;
                    const SSL2ClientHello: PSSL2ClientHello;
                    const Handshakes: TTLSHandshakes;
                    Entity: TConnectionEnd;
                    var Verify: PTLSHandshake): Integer;

implementation

uses
  TlsConst, MpArithTypes, MpArith, Pkix, MpYarrow;

procedure SSL3MACBegin(const K; KLen: Integer; H: THash; Finished: Boolean);
var
  Pad: packed array [0..47] of Byte;
  PadSize: Integer;
begin
  PadSize := 0;
  if H.Algorithm = haSHA1 then
    PadSize := 40
  else if H.Algorithm = haMD5 then
    PadSize := 48
  else
    Assert(False);
  FillChar(Pad,PadSize,$36);
  if not Finished then
    H.SetUp;
  H.HashData(K,KLen);
  H.HashData(Pad,PadSize);
end;

procedure SSL3MACEnd(const K; KLen: Integer; H: THash; var Mac; MacLen: Integer);
var
  Pad: packed array [0..67] of Byte;
  PadSize: Integer;
begin
  try
    PadSize := 0;
    if H.Algorithm = haSHA1 then
      PadSize := 40
    else if H.Algorithm = haMD5 then
      PadSize := 48
    else
      Assert(False);
    FillChar(Pad,PadSize,$5C);
    H.Done(@Pad[PadSize]);
    H.SetUp;
    H.HashData(K,KLen);
    H.HashData(Pad,PadSize + H.DigestSize);
    H.Done(@Pad);
    if MacLen < H.DigestSize then
      Move(Pad,Mac,MacLen)
    else begin
      FillChar(Mac,MacLen,0);
      Move(Pad,Mac,H.DigestSize);
    end;
  finally
    ProtectClear(Pad,SizeOf(Pad));
  end;
end;

function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
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
        SHA1.HashData(SSL2ClientHello^,L+2);
        MD5.HashData(SSL2ClientHello^,L+2);
      end;
      for I := 0 to Length(Handshakes)-1 do begin
        H := Handshakes[I];
        L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        SHA1.HashData(H^,L + 4);
        MD5.HashData(H^,L + 4);
      end;
      SSL3MacBegin(MasterSecret,48,SHA1,True);
      SSL3MacBegin(MasterSecret,48,MD5,True);
      SSL3MacEnd(MasterSecret,48,SHA1,Signed.input.rsaInput.sha_hash,20);
      SSL3MacEnd(MasterSecret,48,MD5,Signed.input.rsaInput.md5_hash,16);
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
  IFSSASignatureGeneration(RSASignKey,
                           Signed.input.rsaInput,36,
                           haSHA1,haSHA1, // <- These parameters are not used
                           seEMSA_TLS,False,
                           Signed.signature.KeyBytes[0],SignLen);
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const RSASignKey: IMPIFPrivateKey;
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
        SHA1.HashData(SSL2ClientHello^,L+2);
        MD5.HashData(SSL2ClientHello^,L+2);
      end;
      for I := 0 to Length(Handshakes)-1 do begin
        H := Handshakes[I];
        L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
        SHA1.HashData(H^,L + 4);
        MD5.HashData(H^,L + 4);
      end;
      SSL3MacBegin(MasterSecret,48,SHA1,True);
      SSL3MacBegin(MasterSecret,48,MD5,True);
      SSL3MacEnd(MasterSecret,48,SHA1,Signed.input.rsaInput.sha_hash,20);
      SSL3MacEnd(MasterSecret,48,MD5,Signed.input.rsaInput.md5_hash,16);
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
  Dst^.certificate_verify.Data[0] := Byte(SignLen shr 8);
  Dst^.certificate_verify.Data[1] := Byte(SignLen);
  Move(Signed.signature.KeyBytes[0],Dst^.certificate_verify.Data[2],SignLen);
end;

function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
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
      SHA1.HashData(SSL2ClientHello^,L+2);
    end;
    for I := 0 to Length(Handshakes)-1 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SSL3MacBegin(MasterSecret,48,SHA1,True);
    SSL3MacEnd(MasterSecret,48,SHA1,Signed.input.sha_hash,20);
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

function SSL_EncodeCertificateVerify(const MasterSecret: TMasterSecret;
                                     const SSL2ClientHello: PSSL2ClientHello;
                                     const Handshakes: TTLSHandshakes;
                                     const DSSSignKey: IMPDLPrivateKey;
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
    SSL3MacBegin(MasterSecret,48,SHA1,True);
    SSL3MacEnd(MasterSecret,48,SHA1,Signed.input.sha_hash,20);
  finally
    SHA1.Free;
  end;

  DSSSignKey.SignatureDigestAlg := haSHA1;
  DSSSignKey.DEREncodeSignature := True;
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

function SSL_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const MasterSecret: TMasterSecret;
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
      SSL3MacBegin(MasterSecret,48,SHA1,True);
      SSL3MacBegin(MasterSecret,48,MD5,True);
      SSL3MacEnd(MasterSecret,48,SHA1,Signed.input.rsaInput.sha_hash,20);
      SSL3MacEnd(MasterSecret,48,MD5,Signed.input.rsaInput.md5_hash,16);
    finally
      MD5.Free;
    end;
  finally
    SHA1.Free;
  end;

  SignLen := Src^.Data[0] shl 8 + Src^.Data[1];

  Result := (Len >= SignLen + 2) and
            (SignLen = (MPMSB(RSASignKey.N) + 7) shr 3);

  if Result then
    Result := IFSSASignatureVerification(RSASignKey,
                                         Signed.input.rsaInput,36,
                                         Src^.Data[2],SignLen,
                                         haMD5,haSHA1, // <- These parameters are not used
                                         seEMSA_TLS,
                                         False);
end;

function SSL_DecodeCertificateVerify(Src: PCertificateVerify;
                                     Len: Integer;
                                     const MasterSecret: TMasterSecret;
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
      SHA1.HashData(SSL2ClientHello^,L+2);
    end;
    for I := 0 to Length(Handshakes)-2 do begin
      H := Handshakes[I];
      L := H^.length[0]*65536 + H^.length[1]*256 + H^.length[2];
      SHA1.HashData(H^,L + 4);
    end;
    SSL3MacBegin(MasterSecret,48,SHA1,True);
    SSL3MacEnd(MasterSecret,48,SHA1,Signed.input.sha_hash,20);
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


procedure SSL_PRF(const Secret; SecretLen: Integer;
                  Seed: string;
                  var Key; KeyLen: Integer);
var
  MD5:  TMD5;
  SHA1: TSHA1;
  vLabel: string;
  I: Integer;
  C: Char;
  Hash: packed array [0..19] of Byte;
  K: PChar;
begin
  MD5 := TMD5.Create(nil^,0);
  try
    SHA1 := TSHA1.Create(nil^,0);
    try
      C := 'A';
      K := @Key;
      for I := 1 to (KeyLen + 15) shr 4 do begin
        vLabel := StringOfChar(C,I);
        SHA1.SetUp;
        SHA1.HashData(Pointer(vLabel)^,I);
        SHA1.HashData(Secret,SecretLen);
        SHA1.HashData(Pointer(Seed)^,Length(Seed));
        SHA1.Done(@Hash);
        MD5.SetUp;
        MD5.HashData(Secret,SecretLen);
        MD5.HashData(Hash,20);
        MD5.Done(@Hash);
        if KeyLen >= 16 then
          Move(Hash,K^,16)
        else
          Move(Hash,K^,KeyLen);
        K := K + 16;
        Dec(KeyLen,16);
        Inc(C);
      end;
    finally
      SHA1.Free;
    end;
  finally
    MD5.Free;
  end;
end;    

procedure SSL_KDF(const Secret; SecretLen: Integer;
                  ClientRandom: TRandomString;
                  ServerRandom: TRandomString;
                  HLen, KLen, EKLen, BLen: Integer;
                  Exportable: Boolean;
                  var Key: TTLSKeys);
var
  Seed: string;
  KeyBlock: string;
  MD5: TMD5;
  Hash: packed array [0..15] of Byte;
begin
  if Exportable then begin
    SetLength(KeyBlock,HLen*2 + KLen*2);
    SetLength(Seed,64);
    Move(ServerRandom,Seed[1],32);
    Move(ClientRandom,Seed[33],32);
    SSL_PRF(Secret,SecretLen,
            Seed,
            Pointer(KeyBlock)^,Length(KeyBlock));

    Move(KeyBlock[1],Key.client_write_MAC_secret,HLen);
    Move(KeyBlock[1 + HLen],Key.server_write_MAC_secret,HLen);

    MD5 := TMD5.Create(nil^,0);
    try
      MD5.HashData(KeyBlock[1 + 2*HLen],KLen);
      MD5.HashData(ClientRandom,32);
      MD5.HashData(ServerRandom,32);
      MD5.Done(@Hash);
      Move(Hash,Key.client_write_key,EKLen);

      MD5.SetUp;
      MD5.HashData(KeyBlock[1 + 2*HLen + KLen],KLen);
      MD5.HashData(ServerRandom,32);
      MD5.HashData(ClientRandom,32);
      MD5.Done(@Hash);
      Move(Hash,Key.server_write_key,EKLen);

      if BLen > 0 then begin
        MD5.SetUp;
        MD5.HashData(ClientRandom,32);
        MD5.HashData(ServerRandom,32);
        MD5.Done(@Hash);
        Move(Hash,Key.client_write_iv,BLen);

        MD5.SetUp;
        MD5.HashData(ServerRandom,32);
        MD5.HashData(ClientRandom,32);
        MD5.Done(@Hash);
        Move(Hash,Key.server_write_iv,BLen);
      end;
    finally
      MD5.Free
    end;

    ProtectClear(Pointer(KeyBlock)^,Length(KeyBlock));

    Key.kLen := EKLen;
  end else begin
    SetLength(KeyBlock,HLen*2 + KLen*2 + BLen*2);
    SetLength(Seed,64);
    Move(ServerRandom,Seed[1],32);
    Move(ClientRandom,Seed[33],32);
    SSL_PRF(Secret,SecretLen,
            Seed,
            Pointer(KeyBlock)^,Length(KeyBlock));

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
end;

procedure SSL_KDF(var Params: TTLSSecurityParams;
                  var Key: TTLSKeys);
var
  HLen:  Integer;
  KLen:  Integer;
  EKLen: Integer;
  BLen:  Integer;
begin
  case Params.mac_algorithm of
    haMD5:       HLen := 16;
    haSHA1:      HLen := 20;
  else
    HLen := 0;
  end;

  KLen := 0;
  EKLen := 0;
  BLen := 0;
  case Params.bulk_cipher_algorithm of
    bcaRC2_40:
      begin
        KLen := 5;
        EKLen := 16;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := True;
      end;
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
    bcaDES40:
      begin
        KLen := 5;
        EKLen := 8;
        BLen := 8;
        Params.cipher_type := ctBlock;
        Params.is_exportable := True;
      end;
  end;

  Params.key_size := KLen;

  SSL_KDF(Params.master_secret,48,
          Params.client_random,Params.server_random,
          HLen,KLen,EKLen,BLen,Params.is_exportable,
          Key);
end;

function SSL_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
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
          SetLength(Seed,64);
          RawRandom(PMS^.random,46*8);
          try
            Move(Ctx.client_random,Seed[1],32);
            Move(Ctx.server_random,Seed[33],32);
            SSL_PRF(PMS^,48,
                    Seed,
                    Ctx.master_secret,48);
            FillChar(IFPubl,SizeOf(IFPubl),0);
            try
              ServerKey.AsIFKey(IFPubl);
              Len := (MPMSB(IFPubl.N) + 7) shr 3;
            {$IFDEF HOW_SSL3_SHOULD_BE_DONE} // (but usually isn't)
              ReallocMem(Dst,Len + 6);
              Result := Len + 6;
              Dst^.client_key_exchange.data[0] := Len shr 8;
              Dst^.client_key_exchange.data[1] := Byte(Len);
              IFESEncryption(IFPubl,PMS^,48,
                             nil^,0,haSHA1,haSHA1,
                             eeEME_PKCS1_v1_5,
                             Dst^.client_key_exchange.data[2],Len);
            {$ELSE}
              ReallocMem(Dst,Len + 4);
              Result := Len + 4;
              IFESEncryption(IFPubl,PMS^,48,
                             nil^,0,haSHA1,haSHA1,
                             eeEME_PKCS1_v1_5,
                             Dst^.client_key_exchange.data[0],Len);
            {$ENDIF}
            finally
              DisposeIFPublicKey(IFPubl);
              ProtectClear(Seed,65);
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
              SSL_PRF(Pointer(P)^,Len,
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

function SSL_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
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
        SSL_PRF(Pointer(P)^,Len,
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

function SSL_EncodeClientKeyExchange(ServerKey: TX509PublicKey;
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
    ServerKey.AsDLKey(DLPubl);
    ReallocMem(Dst,6);
    Result := 6;
    Dst^.client_key_exchange.data[0] := 0;
    Dst^.client_key_exchange.data[1] := 0;
    Len := (MPMSB(DLPubl.Params.P) + 7) shr 3;
    P := TSecretKey.Create('');
    P.SetLength(Len);
    DLPriv.KDF := kdNone;
    DLPriv.DecryptKeyAgreement(DLPubl,P.Key^,Len);
    SetLength(Seed,64);
    try
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      SSL_PRF(P.Key^,Len,
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

function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     const IFPriv: TIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean;
var
  L, PMSLen: Integer;
  PMS: TPreMasterSecret;
  Seed: string[64];
begin
  PMSLen := 48;
  Result := Len >= 2;
  if not Result then Exit;
  L := Src^.data[0] shl 8 + Src^.data[1];
  Result := Len >= L + 2;
  if not Result then begin
    Result := IFESDecryption(IFPriv,
                             Src^.data[0],Len,nil^,0,haSHA1,haSHA1,
                             eeEME_PKCS1_v1_5,
                             PMS,PMSLen);
    if not Result then begin
      PMSLen := 48;
      Result := IFESDecryption(IFPriv,
                               Src^.data[2],L,nil^,0,haSHA1,haSHA1,
                               eeEME_PKCS1_v1_5,
                               PMS,PMSLen);
    end;
  end else begin
    Result := IFESDecryption(IFPriv,
                             Src^.data[2],L,nil^,0,haSHA1,haSHA1,
                             eeEME_PKCS1_v1_5,
                             PMS,PMSLen);
    if not Result then begin
      PMSLen := 48;
      Result := IFESDecryption(IFPriv,
                               Src^.data[0],Len,nil^,0,haSHA1,haSHA1,
                               eeEME_PKCS1_v1_5,
                               PMS,PMSLen);
    end;
  end;
  Result := Result and (PMSLen = 48);
  if Result then try
    Result := (PMS.version.major = 3) and (PMS.version.minor = 0);
    if Result then try
      SetLength(Seed,64);
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      SSL_PRF(PMS,48,Seed,Ctx.master_secret,48);
    finally
      ProtectClear(Seed,SizeOf(Seed));
    end;
  finally
    ProtectClear(PMS,48);
  end;
end;

function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     IFPriv: IMPIFPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
var
  L, PMSLen: Integer;
  PMS: TPreMasterSecret;
  Seed: string[64];
begin
  PMSLen := 48;
  Result := Len >= 2;
  if not Result then Exit;
  IFPriv.EncryptHashAlg := haSHA1;
  L := Src^.data[0] shl 8 + Src^.data[1];
  Result := Len >= L + 2;
  if not Result then begin
    L := Len;
    Result := IFPriv.DecryptKeyTransport(Src^.data[0],L,PMS,PMSLen);
  end else
    Result := IFPriv.DecryptKeyTransport(Src^.data[2],L,PMS,PMSLen);
  Result := Result and (PMSLen = 48);
  if Result then try
    Result := (PMS.version.major = 3) and (PMS.version.minor = 0);
    if Result then try
      SetLength(Seed,64);
      Move(Ctx.client_random,Seed[1],32);
      Move(Ctx.server_random,Seed[33],32);
      SSL_PRF(PMS,48,Seed,Ctx.master_secret,48);
    finally
      ProtectClear(Seed,SizeOf(Seed));
    end;
  finally
    ProtectClear(PMS,48);
  end;
end;

function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     const DLPriv: TDLPrivateKey;
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
            SSL_PRF(Pointer(P)^,L,
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

function SSL_DecodeClientKeyExchange(Src: PClientKeyExchange;
                                     Len: Integer;
                                     ClientCert: TASN1Struct;
                                     DLPriv: IMPDLPrivateKey;
                                     var Ctx: TTLSSecurityParams): Boolean; overload;
var
  L: Integer;
  DLPubl: TDLPublicKey;
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
      L := (MPMSB(DLPriv.SystemParams.P) + 7) shr 3;
      P := TSecretKey.Create('');
      P.SetLength(L);
      DLPriv.DecryptKeyAgreement(DLPubl,P.Key^,L);
      SetLength(Seed,64);
      try
        Move(Ctx.client_random,Seed[1],32);
        Move(Ctx.server_random,Seed[33],32);
        SSL_PRF(P.Key^,L,
                Seed,
                Ctx.master_secret,48);
      finally
        ProtectClear(Seed,65);
      end;
    end;
  finally
    DisposeDLPublicKey(DLPubl);
  end;
end;

function SSL_Verify(const Params: TTLSSecurityParams;
                    const SSL2ClientHello: PSSL2ClientHello;
                    const Handshakes: TTLSHandshakes;
                    Entity: TConnectionEnd;
                    var Verify: PTLSHandshake): Integer;
const
  clientSender: LongWord = $544E4C43;
  serverSender: LongWord = $52565253;
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
      case Entity of
        ceServer:
          begin
            SHA1.HashData(serverSender,4);
            MD5.HashData(serverSender,4);
          end;
        ceClient:
          begin
            SHA1.HashData(clientSender,4);
            MD5.HashData(clientSender,4);
          end;
      end;
      SSL3MacBegin(Params.master_secret,48,SHA1,True);
      SSL3MacBegin(Params.master_secret,48,MD5,True);
      SetLength(Hash,36);
      try
        SSL3MacEnd(Params.master_secret,48,SHA1,Hash[17],20);
        SSL3MacEnd(Params.master_secret,48,MD5,Hash[1],16);

        ReallocMem(Verify,40);
        Verify.msg_type := finished;
        Verify.length[0] := 0;
        Verify.length[1] := 0;
        Verify.length[2] := 36;

        Move(Hash[1],Verify.finished,36);
      finally
        ProtectClear(Hash,37);
      end;
      Result := 40;
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
