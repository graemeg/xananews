{$I ver.inc}
{$B-,O+,Q-,R-}
{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MpIFConversion Unit                               }
{     CryptoAPI RSA and OpenSSL support                 }
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
unit MpIFConversion;

interface

uses       
  SecUtils, Asn1, MpIF;

const
  PUBLICKEYBLOB          = $6;
  PRIVATEKEYBLOB         = $7;

const
  ALG_CLASS_SIGNATURE    = (1 shl 13);
  ALG_CLASS_KEY_EXCHANGE = (5 shl 13);
  ALG_TYPE_RSA           = (2 shl 9);
  ALG_SID_RSA_ANY        = 0;
  CALG_RSA_SIGN = (ALG_CLASS_SIGNATURE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);
  CALG_RSA_KEYX = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);

type
  DWORD = LongWord;

  TMagic = record
    case Byte of
      0: (D: DWORD);
      1: (A: array [0..3] of Char);
  end;
  TRSAKeyBlob = packed record
    bType:    Byte;
    bVersion: Byte;
    reserved: Word;
    aiKeyAlg: DWORD;
    magic:    TMagic;
    bitLen:   DWORD;
    pubExp:   DWORD;
    case Byte of
      0: (modulus: array [0..65535] of Byte);
      1: (privkey: array [0..65535] of Byte);
  end;
  PRSAKeyBlob = ^TRSAKeyBlob;

function RSAPrivateKeyToPRIVATEKEYBLOB(const PrivKey: TIFPrivateKey;
  out Key: PRSAKeyBlob; out Len: Integer): Boolean; overload;
function RSAPrivateKeyToPRIVATEKEYBLOB(const PrivKey: TIFPrivateKey;
  KeyAlg: DWORD; out Key: PRSAKeyBlob; out Len: Integer): Boolean; overload;
function PRIVATEKEYBLOBToRSAPrivateKey(Key: PRSAKeyBlob;
  Len: Integer; var PrivKey: TIFPrivateKey): Boolean;

function LoadRSAPrivateKeyFromPVKFile(const FileName: string;
  Password: ISecretKey; var PrivKey: TIFPrivateKey): Boolean;
function SavePRIVATEKEYBLOBToPVKFile(const FileName: string;
  Password: ISecretKey; PrivKey: PRSAKeyBlob; Len: Integer): Boolean;
function SaveRSAPrivateKeyToPVKFile(const FileName: string;
  Password: ISecretKey; const PrivKey: TIFPrivateKey; KeyAlg: DWORD): Boolean;

function RSAPrivateKeyToASN1Struct(const PrivKey: TIFPrivateKey;
  var RSAPrivateKey: TASN1Struct): Boolean;
function ASN1StructToRSAPrivateKey(RSAPrivateKey: TASN1Struct;
  var PrivKey: TIFPrivateKey): Boolean;

function LoadRSAPrivateKeyFromKEYFile(const FileName: string;
  var PrivKey: TIFPrivateKey): Boolean;

implementation

uses
  SysUtils, Classes, SsArc4, MpArithTypes, MpArith, MpYarrow, SsBase64;

function RSAPrivateKeyToPRIVATEKEYBLOB(const PrivKey: TIFPrivateKey;
  out Key: PRSAKeyBlob; out Len: Integer): Boolean;
begin
  Result := RSAPrivateKeyToPRIVATEKEYBLOB(PrivKey,CALG_RSA_KEYX,Key,Len);
end;

function RSAPrivateKeyToPRIVATEKEYBLOB(const PrivKey: TIFPrivateKey;
  KeyAlg: DWORD; out Key: PRSAKeyBlob; out Len: Integer): Boolean;
var
  KeyLen, PSize, I, J: Integer;
  N, D, E, X, PM, QM, G, M, D1: PMPInteger;
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

  N := nil;
  try
    MPMul2(PrivKey.oP,PrivKey.oQ,N);
    KeyLen := (MPMSB(N) + 7) shr 3;

    Len := 20 + KeyLen * 5;
    GetMem(Key,Len);
    if Result then try
      Key^.bType := PRIVATEKEYBLOB;
      Key^.bVersion := 2;
      Key^.reserved := 0;
      Key^.aiKeyAlg := KeyAlg;
      Key^.magic.A := 'RSA2';
      try
        Key^.bitLen := MPMSB(N);

        X := nil;
        try
          MPDec2(PrivKey.oP,X);
          E := nil;
          try
            MPInvMod(PrivKey.oD1,X,E);
            Key^.pubExp := E^.Data[E^.Size - 1];
          finally
            MPDealloc(E);
          end;
        finally
          MPDealloc(X);
        end;

        J := 0;
        PSize := (KeyLen + 3) shr 2;
        for I := PSize - 1 downto 0 do begin
          Move(N^.Data[I],Key^.modulus[J],4);
          Inc(J,4);
        end;

        PSize := (PSize + 1) shr 1;

        for I := PSize - 1 downto 0 do begin
          Move(PrivKey.oP^.Data[I],Key^.privKey[J],4);
          Inc(J,4);
        end;

        for I := PSize - 1 downto 0 do begin
          Move(PrivKey.oQ^.Data[I],Key^.privKey[J],4);
          Inc(J,4);
        end;

        for I := PSize - 1 downto 0 do begin
          Move(PrivKey.oD1^.Data[I],Key^.privKey[J],4);
          Inc(J,4);
        end;

        for I := PSize - 1 downto 0 do begin
          Move(PrivKey.oD2^.Data[I],Key^.privKey[J],4);
          Inc(J,4);
        end;

        for I := PSize - 1 downto 0 do begin
          Move(PrivKey.oC^.Data[I],Key^.privKey[J],4);
          Inc(J,4);
        end;

        PM := nil;
        QM := nil;
        G := nil;
        M := nil;
        D1 := nil;
        D := nil;
        try
          MpDec2(PrivKey.oP,PM);
          MpDec2(PrivKey.oQ,QM);
          repeat
            MPBinaryGCD(PM,QM,G);
            if MPMSB(G) = 1 then Break;
            MPCopy2(PM,M);
            MPDiv(M,G,PM);
          until False;
          MPMod2(PrivKey.oD1,X,D1);
          MPGarnerCRT(D,[PM,QM],[D1,PrivKey.oD2]);
          PSize := (KeyLen + 3) shr 2;
          MPRealloc(D,PSize);
          for I := PSize - 1 downto 0 do begin
            Move(D^.Data[I],Key^.privKey[J],4);
            Inc(J,4);
          end;
        finally
          MPDealloc(D);
          MPDealloc(D1);
          MPDealloc(M);
          MPDealloc(G);
          MPDealloc(QM);
          MPDealloc(PM);
        end;

        Len := 20 + J;
        Result := True;
      except
        ProtectClear(Key^,Len);
        raise;
      end;
    except
      FreeMem(Key);
      raise;
    end;
  finally
    MPDealloc(N);
  end;
end;

function PRIVATEKEYBLOBToRSAPrivateKey(Key: PRSAKeyBlob;
  Len: Integer; var PrivKey: TIFPrivateKey): Boolean;
var
  KeyLen, PSize, DPSize, I, J: Integer;
begin
  KeyLen := Key.bitLen shr 3;

  DisposeIFPrivateKey(PrivKey);
  PrivKey.ReprType := 1;
  PSize := (((KeyLen + 3) shr 2) + 1) shr 1;
  DPSize := ((KeyLen + 1) shr 1) and 3;
  if DPSize = 0 then
    DPSize := 4;

  J := KeyLen;
  MPRealloc(PrivKey.oP,PSize);
  for I := PSize - 1 downto 1 do begin
    Move(Key^.privKey[J],PrivKey.oP^.Data[I],4);
    Inc(J,4);
  end;
  PrivKey.oP^.Data[0] := 0;
  Move(Key^.privKey[J],PrivKey.oP^.Data[0],DPSize);
  Inc(J,DPSize);
  PrivKey.oP^.Sign := 1;

  MPRealloc(PrivKey.oQ,PSize);
  for I := PSize - 1 downto 1 do begin
    Move(Key^.privKey[J],PrivKey.oQ^.Data[I],4);
    Inc(J,4);
  end;
  PrivKey.oQ^.Data[0] := 0;
  Move(Key^.privKey[J],PrivKey.oQ^.Data[0],DPSize);
  Inc(J,DPSize);
  PrivKey.oQ^.Sign := 1;

  MPRealloc(PrivKey.oD1,PSize);
  for I := PSize - 1 downto 1 do begin
    Move(Key^.privKey[J],PrivKey.oD1^.Data[I],4);
    Inc(J,4);
  end;
  PrivKey.oD1^.Data[0] := 0;
  Move(Key^.privKey[J],PrivKey.oD1^.Data[0],DPSize);
  Inc(J,DPSize);
  PrivKey.oD1^.Sign := 1;

        MPRealloc(PrivKey.oD2,PSize);
        for I := PSize - 1 downto 1 do begin
          Move(Key^.privKey[J],PrivKey.oD2^.Data[I],4);
          Inc(J,4);
        end;
        PrivKey.oD2^.Data[0] := 0;
        Move(Key^.privKey[J],PrivKey.oD2^.Data[0],DPSize);
        Inc(J,DPSize);
        PrivKey.oD2^.Sign := 1;

        MPRealloc(PrivKey.oC,PSize);
        for I := PSize - 1 downto 1 do begin
          Move(Key^.privKey[J],PrivKey.oC^.Data[I],4);
          Inc(J,4);
        end;
        PrivKey.oC^.Data[0] := 0;
        Move(Key^.privKey[J],PrivKey.oC^.Data[0],DPSize);
//        Inc(J,DPSize); // Allow future extension
        PrivKey.oC^.Sign := 1;

        Result := True;
end;

const
  PVKMagic = $b0b5f11e;

function LoadRSAPrivateKeyFromPVKFile(const FileName: string;
  Password: ISecretKey; var PrivKey: TIFPrivateKey): Boolean;
var
  FS: TFileStream;
  D, SaltLen, Len: DWORD;
  K: PRSAKeyBlob;
  Encrypted: Boolean;
  H: TSHA1;
  Salt: array [0..32] of Byte;
  Key: array [0..19] of Byte;
begin
  Result := FileExists(FileName);
  if Result then begin
    FS := TFileStream.Create(FileName,fmOpenRead);
    try
      FS.Read(D,4);
      Result := (D = PVKMagic);  // magic
      if Result then begin
        FS.Read(D,4); // reserved
        FS.Read(D,4); // key type - ignore
        FS.Read(D,4); // encrypted
        FS.Read(SaltLen,4);       
        Encrypted := (D <> 0) or (SaltLen > 0);
        Result := SaltLen <= SizeOf(Salt);
        if Result then begin
          FS.Read(Len,4);
          GetMem(K,Len);
          try
            if Encrypted then begin
              if Assigned(Password) then begin
                FS.Read(Salt,SaltLen);
                FS.Read(K^,Len);
                H := TSHA1.Create(Salt,SaltLen);
                try
                  H.HashData(Password.Key^,Password.KeyLen);
                  H.Done(@Key);
                finally
                  H.Free;
                end;
                DecryptToPtr(TARC4,@Key,16,@K.magic,Len-8,@K.magic);
                if K^.magic.A <> 'RSA2' then begin
                  EncryptToPtr(TARC4,@Key,16,@K.magic,Len-8,@K.magic);
                  FillChar(Key[5],11,0);
                  DecryptToPtr(TARC4,@Key,16,@K.magic,Len-8,@K.magic);
                end;
              end else
                Result := False;
            end else
              FS.Read(K^,Len);
            Result := (K^.magic.A = 'RSA2');
            if Result then
              Result := PRIVATEKEYBLOBToRSAPrivateKey(K,Len,PrivKey);
          finally
            ProtectClear(K^,Len);
            FreeMem(K);
          end;
        end;
      end;
    finally
      FS.Free;
    end;
  end;
end;

function SavePRIVATEKEYBLOBToPVKFile(const FileName: string;
  Password: ISecretKey; PrivKey: PRSAKeyBlob; Len: Integer): Boolean;
var
  FS: TFileStream;
  D: DWORD;
  H: TSHA1;
  Salt: array [0..15] of Byte;
  Key: array [0..19] of Byte;
begin
    FS := TFileStream.Create(FileName,fmCreate);
    try
      D := PVKMagic;
      FS.Write(D,4);
      D := 0; // reserved
      FS.Write(D,4);
      case PrivKey.aiKeyAlg of
        CALG_RSA_KEYX: D := 1;
        CALG_RSA_SIGN: D := 2;
      else
        raise Exception.Create('Unknown key algorithm');
      end;
      FS.Write(D,4);
      if Assigned(Password) then begin
        D := 1;
        FS.Write(D,4); // encrypted
        D := SizeOf(Salt);
        FS.Write(D,4); // saltlen
        D := Len;
        FS.Write(D,4); // keylen
        RawRandom(Salt,128);
        FS.Write(Salt,SizeOf(Salt));
        H := TSHA1.Create(Salt,SizeOf(Salt));
        try
          H.HashData(Password.Key^,Password.KeyLen);
          H.Done(@Key);
          try
            EncryptToPtr(TARC4,@Key,16,@PrivKey.magic,Len-8,@PrivKey.magic);
          finally
            ProtectClear(Key,SizeOf(Key));
          end;
        finally
          H.Free;
        end;
        FS.Write(PrivKey^,Len);
      end else begin
        D := 0;
        FS.Write(D,4); // encrypted
        FS.Write(D,4); // saltlen
        D := Len;
        FS.Write(D,4); // keylen
        FS.Write(PrivKey^,Len);
      end;
    finally
      FS.Free;
    end;
  Result := True;
end;

function SaveRSAPrivateKeyToPVKFile(const FileName: string;
  Password: ISecretKey; const PrivKey: TIFPrivateKey; KeyAlg: DWORD): Boolean;
var
  K: PRSAKeyBlob;
  Len: Integer;
begin
  Result := RSAPrivateKeyToPRIVATEKEYBLOB(PrivKey,KeyAlg,K,Len);
  if Result then try
    Result := SavePRIVATEKEYBLOBToPVKFile(FileName,Password,K,Len);
  finally
    ProtectClear(K^,Len);
    FreeMem(K);
  end;
end;

function RSAPrivateKeyToASN1Struct(const PrivKey: TIFPrivateKey;
  var RSAPrivateKey: TASN1Struct): Boolean;
var
  Publ: TIFPublicKey;
  Priv: TIFPrivateKey;
  X, Y: PMPInteger;
begin
  Result := PrivKey.ReprType = 1;
  if Result then begin
    Result := MPMSB(PrivKey.oP) = MPMSB(PrivKey.oQ);
    if Result then begin
      NewComposeASN1Struct(RSAPrivateKey,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      RSAPrivateKey.Persistent := True;
      RSAPrivateKey.AddField('version','INTEGER',nil).EditContent(Integer(0));
      FillChar(Publ,SizeOf(Publ),0);
      try
        IFRSAPublicKey(PrivKey,Publ);
        RSAPrivateKey.AddField('modulus','INTEGER',nil).EditContent(Publ.N,False);
        RSAPrivateKey.AddField('publicExponent','INTEGER',nil).EditContent(Publ.E,False);
        FillChar(Priv,SizeOf(Priv),0);
        try
          Priv.ReprType := 2;
          Priv.tP := MPCopy(PrivKey.oP);
          Priv.tQ := MPCopy(PrivKey.oQ);
          Priv.tD := MPCopy(Publ.E);
          IFRSAPublicKey(Priv,Publ,True);
          RSAPrivateKey.AddField('privateExponent','INTEGER',nil).EditContent(Publ.E,False);

          RSAPrivateKey.AddField('prime1','INTEGER',nil).EditContent(PrivKey.oP,False);
          RSAPrivateKey.AddField('prime2','INTEGER',nil).EditContent(PrivKey.oQ,False);

          Priv.ReprType := 1;
          X := nil;
          Y := nil;
          try
            MPDec2(PrivKey.oP,X);
            MPMod2(Publ.E,X,Priv.oD1);
            RSAPrivateKey.AddField('exponent1','INTEGER',nil).EditContent(Priv.oD1,False);
            MPDec2(PrivKey.oQ,Y);
            MPMod2(Publ.E,Y,Priv.oD2);
            RSAPrivateKey.AddField('exponent2','INTEGER',nil).EditContent(Priv.oD2,False);
            RSAPrivateKey.AddField('coefficient','INTEGER',nil).EditContent(PrivKey.oC,False);
          finally
            MPDealloc(X);
            MPDealloc(Y);
          end;
        finally
          DisposeIFPrivateKey(Priv);
        end;
      finally
        DisposeIFPublicKey(Publ);
      end;
    end;
  end;
end;

function ASN1StructToRSAPrivateKey(RSAPrivateKey: TASN1Struct;
  var PrivKey: TIFPrivateKey): Boolean;
var
  Publ: TIFPublicKey;
  Priv: TIFPrivateKey;
  D: PMPInteger;
begin
  Result := Assigned(RSAPrivateKey) and RSAPrivateKey.Constructed and
            (RSAPrivateKey.ItemCount = 9) and
            (RSAPrivateKey.Items[0].ContentAsInteger = 0);
  if Result then begin
    FillChar(Publ,SizeOf(Publ),0);
    FillChar(Priv,SizeOf(Priv),0);
    try
      RSAPrivateKey.Items[1].ContentAsMPInt(Publ.N);
      RSAPrivateKey.Items[2].ContentAsMPInt(Publ.E);
      D := nil;
      try
        RSAPrivateKey.Items[3].ContentAsMPInt(D);
        RSAPrivateKey.Items[4].ContentAsMPInt(Priv.oP);
        RSAPrivateKey.Items[5].ContentAsMPInt(Priv.oQ);
        RSAPrivateKey.Items[6].ContentAsMPInt(Priv.oD1);
        RSAPrivateKey.Items[7].ContentAsMPInt(Priv.oD2);
        RSAPrivateKey.Items[8].ContentAsMPInt(Priv.oC);
        Priv.ReprType := 1;
        Result := MPMillerRabin(Priv.oP,2) and MPMillerRabin(Priv.oQ,2) and
                  ValidateIFPrivateKey(Priv,Publ);
        if Result then begin
          PrivKey.ReprType := 1;
          MPCopy2(Priv.oP,PrivKey.oP);
          MPCopy2(Priv.oQ,PrivKey.oQ);
          MPCopy2(Priv.oD1,PrivKey.oD1);
          MPCopy2(Priv.oD2,PrivKey.oD2);
          MPCopy2(Priv.oC,PrivKey.oC);
        end;
      finally
        MPDealloc(D);
      end;
    finally
      DisposeIFPrivateKey(Priv);
      DisposeIFPublicKey(Publ);
    end;
  end;
end;

function LoadRSAPrivateKeyFromKEYFile(const FileName: string;
  var PrivKey: TIFPrivateKey): Boolean;
var
  MS: TMemoryStream;
  S, EndDelim, BeginDelim: string;
  SS: TStringStream;
  F: TASN1Struct;
begin
  try
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile(FileName);
      SetLength(S,MS.Size);
      MS.Read(Pointer(S)^,Length(S));
    finally
      MS.Free;
    end;
    F := TASN1Struct.Create;
    try
      SS := TStringStream.Create(SsBase64.MIME64ToStrEx(BeginDelim,EndDelim,S));
      try
        F.LoadFromStream(SS,fmtDER);
      finally
        SS.Free;
      end;
      Result := ASN1StructToRSAPrivateKey(F,PrivKey);
    finally
      F.Free;
    end;
  except
    Result := False;
  end;
end;

end.
