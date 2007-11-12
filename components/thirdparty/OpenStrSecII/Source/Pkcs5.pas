{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKCS5 Unit                                        }
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
{                                                       }
{     "RSA Security Inc.                                }
{      Public-Key Cryptography Standards (PKCS)"        }
{                                                       }
{*******************************************************}
{$I ver.inc}
unit Pkcs5;

interface

uses
  SecUtils, Kdf;

type
  TPBKDF1 = class(TPBKDF,IPBKDF)
  protected
    procedure SetHashClass(const Value: THashClass); override;
  public
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); override;
  end;

  TPBKDF2 = class(TPBKDF,IPBKDF)
  protected
    procedure F(P, S: ISecretKey; C, I: Cardinal; var Res: ISecretKey);
    procedure PRF(P, S: ISecretKey; var Res: ISecretKey); virtual; abstract;
  public
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); override;
  end;

  TPBKDF2_HMAC = class(TPBKDF2,IPBKDF)
  protected
    procedure PRF(P, S: ISecretKey; var Res: ISecretKey); override;
  end;

implementation

{ TPBKDF1 }

procedure TPBKDF1.DeriveKey(Password, Salt: ISecretKey; Iter,
  DKLen: Integer);
var
  H: THash;
  Dig: ISecretKey;
begin
  ClearKey;
  Dig := TSecretKey.Create('');
  Dig.SetLength(HashClass.DigestSize);
  H := HashClass.Create(Password.Key^,Password.KeyLen);
  try
    H.HashData(Salt.Key^,Salt.KeyLen);
    H.Done(Dig.Key);
    Dec(Iter);
    while Iter > 0 do begin
      H.SetUp;
      H.HashData(Dig.Key^,Dig.KeyLen);
      H.Done(Dig.Key);
      Dec(Iter);
    end;
    SetKey(Dig.Key,DKLen,VectorSize);
  finally
    H.Free;
  end;
end;

procedure TPBKDF1.SetHashClass(const Value: THashClass);
{$IFOPT C+}
var
  Check: Boolean;
{$ENDIF}
begin
{$IFOPT C+}
{$IFDEF SHA1}
  Check := Value = TSHA1;
{$ELSE}
  Check := False;
{$ENDIF}
{$IFDEF MD2}
  Check := Check or (Value = TMD2);
{$ENDIF}
{$IFDEF MD5}
  Check := Check or (Value = TMD5);
{$ENDIF}
  Assert(Check);
{$ENDIF}
  inherited;
end;

{ TPBKDF2 }

procedure TPBKDF2.DeriveKey(Password, Salt: ISecretKey; Iter,
  DKLen: Integer);
var
  T: ISecretKey;
  I, P: Integer;
begin
  ClearKey;
  SetLength(DKLen);
  I := 1;
  P := 0;
  while DKLen > P do begin
    F(Password,Salt,Iter,I,T);
    SetTruncKeyAt(T,P);
    Inc(P,T.KeyLen);
    Inc(I);
  end;
end;

procedure TPBKDF2.F(P, S: ISecretKey; C, I: Cardinal; var Res: ISecretKey);
type
  PLongWord = ^LongWord;
var
  U: ISecretKey;
  J: Integer;
begin
  U := TSecretKey.Create('');
  U.SetLength(S.KeyLen + 4);
  U.SetKeyAt(S,0);
  U.KeyBytes[S.KeyLen] := I shr 24;
  U.KeyBytes[S.KeyLen + 1] := (I shr 16) and $FF;
  U.KeyBytes[S.KeyLen + 2] := (I shr 8) and $FF;
  U.KeyBytes[S.KeyLen + 3] := I and $FF;
  PRF(P,U,U);
  if Res = nil then Res := TSecretKey.Create('');
  Res.SetLength(U.KeyLen);
  Res.SetKeyAt(U,0);
  Dec(C);
  while C > 0 do begin
    PRF(P,U,U);
    for J := 0 to (U.KeyLen shr 2) - 1 do
      PLongWord(@Res.KeyBytes^[J*4])^ := PLongWord(@Res.KeyBytes^[J*4])^ xor
                                         PLongWord(@U.KeyBytes^[J*4])^;
    J := U.KeyLen;
    while (J and $3) <> 0 do begin
      Dec(J);
      Res.KeyBytes[J] := Res.KeyBytes[J] xor U.KeyBytes[J];
    end;            
    Dec(C);
  end;
end;

{ TPBKDF2_HMAC }

procedure TPBKDF2_HMAC.PRF(P, S: ISecretKey; var Res: ISecretKey);
begin
  HMac(P,S.Key^,S.KeyLen,HashClass.Algorithm,HashClass.DigestSize,Res);
end;

end.
