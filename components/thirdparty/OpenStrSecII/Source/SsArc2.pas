{$Q-,R-}
{$I ver.inc}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ARC2 Unit                                         }
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
unit SsArc2;

interface
{$IFDEF ARCTWO}
uses
  SecUtils;

const
  PiTable: array [0..255] of Byte =
    ($d9,$78,$f9,$c4,$19,$dd,$b5,$ed,$28,$e9,$fd,$79,$4a,$a0,$d8,$9d,
     $c6,$7e,$37,$83,$2b,$76,$53,$8e,$62,$4c,$64,$88,$44,$8b,$fb,$a2,
     $17,$9a,$59,$f5,$87,$b3,$4f,$13,$61,$45,$6d,$8d,$09,$81,$7d,$32,
     $bd,$8f,$40,$eb,$86,$b7,$7b,$0b,$f0,$95,$21,$22,$5c,$6b,$4e,$82,
     $54,$d6,$65,$93,$ce,$60,$b2,$1c,$73,$56,$c0,$14,$a7,$8c,$f1,$dc,
     $12,$75,$ca,$1f,$3b,$be,$e4,$d1,$42,$3d,$d4,$30,$a3,$3c,$b6,$26,
     $6f,$bf,$0e,$da,$46,$69,$07,$57,$27,$f2,$1d,$9b,$bc,$94,$43,$03,
     $f8,$11,$c7,$f6,$90,$ef,$3e,$e7,$06,$c3,$d5,$2f,$c8,$66,$1e,$d7,
     $08,$e8,$ea,$de,$80,$52,$ee,$f7,$84,$aa,$72,$ac,$35,$4d,$6a,$2a,
     $96,$1a,$d2,$71,$5a,$15,$49,$74,$4b,$9f,$d0,$5e,$04,$18,$a4,$ec,
     $c2,$e0,$41,$6e,$0f,$51,$cb,$cc,$24,$91,$af,$50,$a1,$f4,$70,$39,
     $99,$7c,$3a,$85,$23,$b8,$b4,$7a,$fc,$02,$36,$5b,$25,$55,$97,$31,
     $2d,$5d,$fa,$98,$e3,$8a,$92,$ae,$05,$df,$29,$10,$67,$6c,$ba,$c9,
     $d3,$00,$e6,$cf,$e1,$9e,$a8,$2c,$63,$16,$01,$3f,$58,$e2,$89,$a9,
     $0d,$38,$34,$1b,$ab,$33,$ff,$b0,$bb,$48,$0c,$5f,$b9,$b1,$cd,$2e,
     $c5,$f3,$db,$47,$e5,$a5,$9c,$77,$0a,$a6,$20,$68,$fe,$7f,$c1,$ad);
  EKBTable: array [0..255] of Byte =
    ($bd,$56,$ea,$f2,$a2,$f1,$ac,$2a,$b0,$93,$d1,$9c,$1b,$33,$fd,$d0,
     $30,$04,$b6,$dc,$7d,$df,$32,$4b,$f7,$cb,$45,$9b,$31,$bb,$21,$5a,
     $41,$9f,$e1,$d9,$4a,$4d,$9e,$da,$a0,$68,$2c,$c3,$27,$5f,$80,$36,
     $3e,$ee,$fb,$95,$1a,$fe,$ce,$a8,$34,$a9,$13,$f0,$a6,$3f,$d8,$0c,
     $78,$24,$af,$23,$52,$c1,$67,$17,$f5,$66,$90,$e7,$e8,$07,$b8,$60,
     $48,$e6,$1e,$53,$f3,$92,$a4,$72,$8c,$08,$15,$6e,$86,$00,$84,$fa,
     $f4,$7f,$8a,$42,$19,$f6,$db,$cd,$14,$8d,$50,$12,$ba,$3c,$06,$4e,
     $ec,$b3,$35,$11,$a1,$88,$8e,$2b,$94,$99,$b7,$71,$74,$d3,$e4,$bf,
     $3a,$de,$96,$0e,$bc,$0a,$ed,$77,$fc,$37,$6b,$03,$79,$89,$62,$c6,
     $d7,$c0,$d2,$7c,$6a,$8b,$22,$a3,$5b,$05,$5d,$02,$75,$d5,$61,$e3,
     $18,$8f,$55,$51,$ad,$1f,$0b,$5e,$85,$e5,$c2,$57,$63,$ca,$3d,$6c,
     $b4,$c5,$cc,$70,$b2,$91,$59,$0d,$47,$20,$c8,$4f,$58,$e0,$01,$e2,
     $16,$38,$c4,$6f,$3b,$0f,$65,$46,$be,$7e,$2d,$7b,$82,$f9,$40,$b5,
     $1d,$73,$f8,$eb,$26,$c7,$87,$97,$25,$54,$b1,$28,$aa,$98,$9d,$a5,
     $64,$6d,$7a,$d4,$10,$81,$44,$ef,$49,$d6,$ae,$2e,$dd,$76,$5c,$2f,
     $a7,$1c,$c9,$09,$69,$9a,$83,$cf,$29,$39,$b9,$e9,$4c,$ff,$43,$ab);
type
  PK = ^TK;
  TK = packed array [0..63] of Word;
  PL = ^TL;
  TL = packed array [0..127] of Byte;
  TKeyBuffer = record
    case Byte of
      0: (K: TK);
      1: (L: TL);
  end;

  TARC2_ECB = class(TBlockCipher)
  private
    FEffectiveKeySize: Integer;
    procedure SetEffectiveKeySize(const Value: Integer);
    function GetEKBParameter: Integer;
    procedure SetEKBParameter(const Value: Integer);
  protected
    FKeyBuffer: TKeyBuffer;
    IV: packed array [0..23] of Byte;
    procedure CleanUp; override;
    procedure DecryptBlock(var Buf); override;
    procedure EncryptBlock(var Buf); override;
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function BlockSize: Integer; override;
    class function Mode: TCipherMode; override;
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PChar; override;
    class function BlockVectorSize: Integer; override;
    class function MaxKeySize: Integer; override;
    class function MinKeySize: Integer; override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
    property EffectiveKeySize: Integer read FEffectiveKeySize write SetEffectiveKeySize;
    property EKBParameter: Integer read GetEKBParameter write SetEKBParameter;
  end;

  TARC2_CBC = class(TARC2_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
  end;

  TARC2_ABC = class(TARC2_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
  end;                     

  TARC2_CFB = class(TARC2_ECB)
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    property ModeRatio;
  end;                      

  TARC2_OFB = class(TARC2_CFB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
  end;                      

  TARC2_CTR = class(TARC2_CFB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
  end;
  
implementation

uses
  SysUtils;

{ TARC2_ECB }

class function TARC2_ECB.Algorithm: TCipherAlg;
begin
  Result := caARC2;
end;

class function TARC2_ECB.AlgorithmName: PChar;
begin
  Result := 'ARC2';
end;

class function TARC2_ECB.BlockSize: Integer;
begin
  Result := 8;
end;

class function TARC2_ECB.BlockVectorSize: Integer;
begin
  Result := 0;
end;

procedure TARC2_ECB.CleanUp;
begin
  ProtectClear(FKeyBuffer,SizeOf(FKeyBuffer));
  ProtectClear(IV,SizeOf(IV));
end;

constructor TARC2_ECB.Create(const AKey; Count, VectorSize: Integer);
begin
  FIV := @IV;
  if Count > 0 then
    inherited;
end;

type
  TBlock = array [0..3] of Word;

procedure TARC2_ECB.DecryptBlock(var Buf);
var
  R: ^TBlock;
  K: PK;
  I, J: Integer;
  Tmp: Cardinal;
begin
  R := @Buf;
  K := @FKeyBuffer;
  // Initialize J to 63:
  J := 63;
  // Perform five r-mixing rounds:
  for I := 0 to 4 do begin
    Tmp := R[3]; Tmp := Tmp shl 11; R[3] := Tmp or (Tmp shr 16);
    R[3] := R[3] - K[j] - (R[2] and R[1]) - ((not R[2]) and R[0]);
    Dec(j);
    Tmp := R[2]; Tmp := Tmp shl 13; R[2] := Tmp or (Tmp shr 16);
    R[2] := R[2] - K[j] - (R[1] and R[0]) - ((not R[1]) and R[3]);
    Dec(j);
    Tmp := R[1]; Tmp := Tmp shl 14; R[1] := Tmp or (Tmp shr 16);
    R[1] := R[1] - K[j] - (R[0] and R[3]) - ((not R[0]) and R[2]);
    Dec(j);
    Tmp := R[0]; Tmp := Tmp shl 15; R[0] := Tmp or (Tmp shr 16);
    R[0] := R[0] - K[j] - (R[3] and R[2]) - ((not R[3]) and R[1]);
    Dec(j);
  end;
  // Perform one r-mashing round:
  R^[3] := R^[3] - K^[R^[2] and $3F];
  R^[2] := R^[2] - K^[R^[1] and $3F];
  R^[1] := R^[1] - K^[R^[0] and $3F];
  R^[0] := R^[0] - K^[R^[3] and $3F];
  // Perform six r-mixing rounds:
  for I := 0 to 5 do begin
    Tmp := R[3]; Tmp := Tmp shl 11; R[3] := Tmp or (Tmp shr 16);
    R[3] := R[3] - K[j] - (R[2] and R[1]) - ((not R[2]) and R[0]);
    Dec(j);
    Tmp := R[2]; Tmp := Tmp shl 13; R[2] := Tmp or (Tmp shr 16);
    R[2] := R[2] - K[j] - (R[1] and R[0]) - ((not R[1]) and R[3]);
    Dec(j);
    Tmp := R[1]; Tmp := Tmp shl 14; R[1] := Tmp or (Tmp shr 16);
    R[1] := R[1] - K[j] - (R[0] and R[3]) - ((not R[0]) and R[2]);
    Dec(j);
    Tmp := R[0]; Tmp := Tmp shl 15; R[0] := Tmp or (Tmp shr 16);
    R[0] := R[0] - K[j] - (R[3] and R[2]) - ((not R[3]) and R[1]);
    Dec(j);
  end;
  // Perform one r-mashing round:
  R^[3] := R^[3] - K^[R^[2] and $3F];
  R^[2] := R^[2] - K^[R^[1] and $3F];
  R^[1] := R^[1] - K^[R^[0] and $3F];
  R^[0] := R^[0] - K^[R^[3] and $3F];
  // Perform five r-mixing rounds:
  for I := 0 to 4 do begin
    Tmp := R[3]; Tmp := Tmp shl 11; R[3] := Tmp or (Tmp shr 16);
    R[3] := R[3] - K[j] - (R[2] and R[1]) - ((not R[2]) and R[0]);
    Dec(j);
    Tmp := R[2]; Tmp := Tmp shl 13; R[2] := Tmp or (Tmp shr 16);
    R[2] := R[2] - K[j] - (R[1] and R[0]) - ((not R[1]) and R[3]);
    Dec(j);
    Tmp := R[1]; Tmp := Tmp shl 14; R[1] := Tmp or (Tmp shr 16);
    R[1] := R[1] - K[j] - (R[0] and R[3]) - ((not R[0]) and R[2]);
    Dec(j);
    Tmp := R[0]; Tmp := Tmp shl 15; R[0] := Tmp or (Tmp shr 16);
    R[0] := R[0] - K[j] - (R[3] and R[2]) - ((not R[3]) and R[1]);
    Dec(j);
  end;
end;

procedure TARC2_ECB.EncryptBlock(var Buf);
var
  R: ^TBlock;
  K: PK;
  I, J: Integer;
  Tmp: Cardinal;
begin
  R := @Buf;

  K := @FKeyBuffer;
  // Initialize J to zero:
  J := 0;
  // Perform five mixing rounds:
  for I := 0 to 4 do begin
    Tmp := R[0] + K^[j] + (R^[3] and R^[2]) + ((not R^[3]) and R^[1]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp + Tmp; R^[0] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[1] + K^[j] + (R^[0] and R^[3]) + ((not R^[0]) and R^[2]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 2; R^[1] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[2] + K^[j] + (R^[1] and R^[0]) + ((not R^[1]) and R^[3]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 3; R^[2] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[3] + K^[j] + (R^[2] and R^[1]) + ((not R^[2]) and R^[0]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 5; R^[3] := (Tmp or (Tmp shr 16)) and $FFFF;
  end;
  // Perform one mashing round:
  R^[0] := Word(R^[0] + K^[R^[3] and $3F]);
  R^[1] := Word(R^[1] + K^[R^[0] and $3F]);
  R^[2] := Word(R^[2] + K^[R^[1] and $3F]);
  R^[3] := Word(R^[3] + K^[R^[2] and $3F]);
  // Perform six mixing rounds:
  for I := 0 to 5 do begin
    Tmp := R[0] + K^[j] + (R^[3] and R^[2]) + ((not R^[3]) and R^[1]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp + Tmp; R^[0] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[1] + K^[j] + (R^[0] and R^[3]) + ((not R^[0]) and R^[2]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 2; R^[1] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[2] + K^[j] + (R^[1] and R^[0]) + ((not R^[1]) and R^[3]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 3; R^[2] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[3] + K^[j] + (R^[2] and R^[1]) + ((not R^[2]) and R^[0]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 5; R^[3] := (Tmp or (Tmp shr 16)) and $FFFF;
  end;
  // Perform one mashing round:
  R^[0] := Word(R^[0] + K^[R^[3] and $3F]);
  R^[1] := Word(R^[1] + K^[R^[0] and $3F]);
  R^[2] := Word(R^[2] + K^[R^[1] and $3F]);
  R^[3] := Word(R^[3] + K^[R^[2] and $3F]);
  // Perform five mixing rounds:
  for I := 0 to 4 do begin
    Tmp := R[0] + K^[j] + (R^[3] and R^[2]) + ((not R^[3]) and R^[1]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp + Tmp; R^[0] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[1] + K^[j] + (R^[0] and R^[3]) + ((not R^[0]) and R^[2]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 2; R^[1] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[2] + K^[j] + (R^[1] and R^[0]) + ((not R^[1]) and R^[3]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 3; R^[2] := (Tmp or (Tmp shr 16)) and $FFFF;
    Tmp := R[3] + K^[j] + (R^[2] and R^[1]) + ((not R^[2]) and R^[0]);
    Inc(J);
    Tmp := Word(Tmp); Tmp := Tmp shl 5; R^[3] := (Tmp or (Tmp shr 16)) and $FFFF;
  end;
end;

function TARC2_ECB.GetEKBParameter: Integer;
begin
  if FEffectiveKeySize < 256 then
    Result := EKBTable[FEffectiveKeySize]
  else
    Result := FEffectiveKeySize;
end;

class function TARC2_ECB.MaxKeySize: Integer;
begin
  Result := 128;
end;

class function TARC2_ECB.MinKeySize: Integer;
begin
  Result := 1;
end;

class function TARC2_ECB.Mode: TCipherMode;
begin
  Result := cmECB;
end;

procedure TARC2_ECB.SetEffectiveKeySize(const Value: Integer);
begin
  if (Value < 1) or (Value > 1024) then
    raise Exception.Create(Format('ARC2: Invalid key bit size - %d',[Value]));
  FEffectiveKeySize := Value;
end;

procedure TARC2_ECB.SetEKBParameter(const Value: Integer);
var
  EKB: Integer;
begin
  if Value > 255 then
    SetEffectiveKeySize(Value)
  else if Value < 0 then
    raise Exception.Create(Format('ARC2: Invalid key bit size - %d',[Value]))
  else begin
    EKB := 0;
    while (EKB < 256) and (Value <> EKBTable[EKB]) do
      Inc(EKB);
    SetEffectiveKeySize(EKB);
  end;
end;

procedure TARC2_ECB.SetUp(const AKey; Count, VectorSize: Integer);
var
  T1, T8: Integer;
  TM: Byte;
  I : Integer;
  L: PL;
begin
  Nag(Count);
  CleanUp;
  if (Count < 1) or (Count > 128) then
    raise Exception.Create(Format('ARC2: Invalid key size - %d',[Count]));

  T1 := FEffectiveKeySize;
  if T1 = 0 then
    T1 := Count * 8;
  T8 := (T1 + 7) shr 3;
  TM := (1 shl (T1 and $7)) - 1;
  if TM = 0 then
    TM := $FF;

  Move(AKey,FKeyBuffer,Count);
  L := @FKeyBuffer;
  for I := Count to 127 do
    L^[I] := PiTable[(L^[I-1] + L^[I-Count]) and $FF];
  L^[128-T8] := PiTable[L^[128-T8] and TM];
  for I := 127 - T8 downto 0 do
    L^[I] := PiTable[L^[I+1] xor L^[I+T8]];
end;

{ TARC2_CBC }

class function TARC2_CBC.BlockVectorSize: Integer;
begin
  Result := 1;
end;

class function TARC2_CBC.Mode: TCipherMode;
begin
  Result := cmCBC;
end;

class function TARC2_CBC.OID(KeySize: Integer): PChar;
begin
  Result := '1.2.840.113549.3.2';
end;

{ TARC2_ABC }

class function TARC2_ABC.BlockVectorSize: Integer;
begin
  Result := 2;
end;

class function TARC2_ABC.Mode: TCipherMode;
begin
  Result := cmABC;
end;

class function TARC2_ABC.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TARC2_CFB }

class function TARC2_CFB.BlockVectorSize: Integer;
begin
  Result := 1;
end;

constructor TARC2_CFB.Create(const AKey; Count, VectorSize: Integer);
begin
  inherited;
  ModeRatio := 8;
end;

class function TARC2_CFB.Mode: TCipherMode;
begin
  Result := cmCFB;
end;

class function TARC2_CFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TARC2_OFB }

class function TARC2_OFB.BlockVectorSize: Integer;
begin
  Result := 1;
end;

class function TARC2_OFB.Mode: TCipherMode;
begin
  Result := cmOFB;
end;

class function TARC2_OFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TARC2_CTR }

class function TARC2_CTR.BlockVectorSize: Integer;
begin
  Result := 1;
end;

class function TARC2_CTR.Mode: TCipherMode;
begin
  Result := cmCTR;
end;

class function TARC2_CTR.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;
     
{$IFDEF ARCTWO}
initialization
  {$IFDEF ECB}       RegisterCipherClass( TARC2_ECB );       {$ENDIF}
  {$IFDEF ABC}       RegisterCipherClass( TARC2_ABC );       {$ENDIF}
  {$IFDEF CBC}       RegisterCipherClass( TARC2_CBC );       {$ENDIF}
  {$IFDEF CFB}       RegisterCipherClass( TARC2_CFB );       {$ENDIF}
  {$IFDEF CTR}       RegisterCipherClass( TARC2_CTR );       {$ENDIF}
  {$IFDEF OFB}       RegisterCipherClass( TARC2_OFB );       {$ENDIF}
{$ENDIF}
{$ELSE  ARCTWO}
implementation
{$ENDIF}
end.
