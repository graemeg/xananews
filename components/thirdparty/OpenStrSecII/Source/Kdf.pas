{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     KDF Unit                                          }
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
unit Kdf;

interface

uses
  SecUtils;

type
  IKDF = interface(ISecretKey)
  ['{1A1D739D-B06F-4DDF-8A02-C440768F5739}']
    function GetHashClass: THashClass;
    procedure DeriveKey(KeyMaterial, Param: ISecretKey;
                        DKLen: Integer); overload;  
    procedure SetHashClass(const Value: THashClass);
    property HashClass: THashClass read GetHashClass write SetHashClass;
  end;

  TKDF = class(TSecretKey,IKDF)
  private
    FHashClass: THashClass;
  protected                
    function GetHashClass: THashClass; virtual;
    procedure SetHashClass(const Value: THashClass); virtual;
  public
    constructor Create(AAlgorithm: ObjectIdentifier);
    procedure DeriveKey(KeyMaterial, Param: ISecretKey;
                        DKLen: Integer); overload; virtual; abstract;
    property HashClass: THashClass read FHashClass write SetHashClass;
  end;

  IPBKDF = interface(IKDF)
  ['{EAAA8477-B0A8-4E9D-828B-9CE91CA1B483}']
    function GetIter: Integer;
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); overload;
    procedure SetIter(const Value: Integer);
    property Iter: Integer read GetIter write SetIter;
  end;

  TPBKDF = class(TKDF,IPBKDF)
  private
    FIter: Integer;
  protected
    function GetIter: Integer;
    procedure SetIter(const Value: Integer);
  public
    constructor Create(AAlgorithm: ObjectIdentifier);
    procedure DeriveKey(KeyMaterial, Param: ISecretKey;
                        DKLen: Integer); override;
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); overload; virtual; abstract;
    property Iter: Integer read FIter write SetIter;
  end;

  TPBKDFClass = class of TPBKDF;

  TKDF1 = class(TKDF,IKDF)
  protected
    procedure SetHashClass(const Value: THashClass); override;
    procedure F(KeyMaterial, Param: ISecretKey; var DK; DKLen: Integer);
  public
    procedure DeriveKey(KeyMaterial, Param: ISecretKey;
                        DKLen: Integer); override;
  end;

  TKDF2 = class(TKDF1,IKDF)
  public
    procedure DeriveKey(KeyMaterial, Param: ISecretKey;
                        DKLen: Integer); override;
  end;

  TWPKDF = class(TPBKDF,IPBKDF)
  protected
    procedure SetHashClass(const Value: THashClass); override;
  public
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); override;
  end;

  TWPKDF2 = class(TPBKDF,IPBKDF)
  public
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); override;
  end;

implementation

uses
  SysUtils;

function ByteSwap(Value: Cardinal): Cardinal;
asm
  bswap EAX
end;

{ TKDF }

constructor TKDF.Create(AAlgorithm: ObjectIdentifier);
begin
  inherited;                  
{$IFDEF SHA1}
  FHashClass := TSHA1;          
{$ENDIF SHA1}
end;

function TKDF.GetHashClass: THashClass;
begin
  Result := FHashClass;
end;

procedure TKDF.SetHashClass(const Value: THashClass);
begin
  FHashClass := Value;
end;

{ TPBKDF }

procedure TPBKDF.DeriveKey(KeyMaterial, Param: ISecretKey; DKLen: Integer);
begin
  DeriveKey(KeyMaterial,Param,Iter,DKLen);
end;

procedure TPBKDF.SetIter(const Value: Integer);
begin
  FIter := Value;
end;

constructor TPBKDF.Create(AAlgorithm: ObjectIdentifier);
begin
  inherited;
  FIter := 1;
end;

function TPBKDF.GetIter: Integer;
begin
  Result := FIter;
end;

{ TKDF1 }

procedure TKDF1.DeriveKey(KeyMaterial, Param: ISecretKey; DKLen: Integer);
begin
  SetLength(DKLen);
  F(KeyMaterial, Param, Key^, DKLen);
end;

procedure TKDF1.F(KeyMaterial, Param: ISecretKey; var DK; DKLen: Integer);
var
  H: THash;
  D: PChar;
begin
  H := FHashClass.Create(KeyMaterial.Key^,KeyMaterial.KeyLen);
  try
    H.HashData(Param.Key^,Param.KeyLen);
    GetMem(D,H.DigestSize);
    try
      H.Done(D);
      Move(D[0],DK,DKLen);
      ProtectClear(D[0],H.DigestSize);
    finally
      FreeMem(D);
    end;
  finally
    H.Free;
  end;
end;

procedure TKDF1.SetHashClass(const Value: THashClass);
begin
  Assert(Assigned(Value) and (Value.DigestSize >= 20));
  inherited;
end;

{ TKDF2 }

procedure TKDF2.DeriveKey(KeyMaterial, Param: ISecretKey; DKLen: Integer);
var
  D: Integer;
  PB: ISecretKey;
  hLen: Integer;
  Count, LW: LongWord;
begin
  hLen := FHashClass.DigestSize;
  PB := TSecretKey.Create('');
  PB.SetLength(Param.KeyLen + 4);
  PB.SetKeyAt(Param,4);
  D := 0;
  Count := 1;
  while DKLen > 0 do begin
    LW := ByteSwap(Count);
    Move(LW,PB.KeyBytes[0],4);
    if hLen <= DKLen then
      F(KeyMaterial,PB,GetKeyBytes[D],hLen)
    else
      F(KeyMaterial,PB,GetKeyBytes[D],DKLen);
    Dec(DKLen,hLen);
    Inc(D,hLen);
    Inc(Count);
  end;
end;

{ TWPKDF }

procedure TWPKDF.DeriveKey(Password, Salt: ISecretKey; Iter,
  DKLen: Integer);
begin                                     
{$IFDEF SHA1}
  if HashClass = TSHA1 then begin
    SetLength(DKLen);
    WeakPasswordToPrivKey(Password.Key^,Password.KeyLen,
                          Salt.Key^,Salt.KeyLen,
                          Iter,
                          Key^,DKLen)
  end else          
{$ENDIF SHA1}
{$IFDEF SHA256}
  if HashClass = TSHA256 then begin
    SetLength(DKLen);
    WeakPasswordToPrivKey256(Password.Key^,Password.KeyLen,
                             Salt.Key^,Salt.KeyLen,
                             Iter,
                             Key^,DKLen)
  end else          
{$ENDIF SHA256}
    raise Exception.Create('TWPKDF.DeriveKey: Unsupported algorithm');
end;

procedure TWPKDF.SetHashClass(const Value: THashClass);
{$IFOPT C+}
var
  OK: Boolean;
{$ENDIF}
begin   
{$IFOPT C+}
{$IFDEF SHA1}
  OK := Value = TSHA1;
{$ELSE  SHA1}
  OK := False;
{$ENDIF}
{$IFDEF SHA1}
  OK := OK or (Value = TSHA256);
{$ENDIF}
  Assert(OK);
{$ENDIF C+}
  inherited;
end;

{ TWPKDF2 }

procedure TWPKDF2.DeriveKey(Password, Salt: ISecretKey; Iter,
  DKLen: Integer);
begin
  SetLength(DKLen);
  WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                    Salt.Key^,Salt.KeyLen,
                    Iter,HashClass.Algorithm,
                    Key^,DKLen);
end;

end.
