{$Q-,R-}
{$I ver.inc}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     DES Unit                                          }
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
unit SsDes;

interface

uses
  SecUtils;

type
  TBlockProc = procedure (var Dst; const Src) of object;

  T3DES_ECB = class(TBlockCipher)
  protected
    IV: packed array [0..23] of byte;
    FSubKeys: packed array [0..191] of LongWord;
    FEncrBlock: TBlockProc;
    FDecrBlock: TBlockProc;
    procedure CleanUp; override;
    procedure DecryptBlock(var Buf); override;
    procedure DecryptBlockToDst(var Dst; const Src); override;
    procedure EncryptBlock(var Buf); override;
    procedure EncryptBlockToDst(var Dst; const Src); override;
    procedure DecryptBlock1(var Dst; const Src);
    procedure EncryptBlock1(var Dst; const Src);
    procedure DecryptBlock3(var Dst; const Src);
    procedure EncryptBlock3(var Dst; const Src);
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    constructor CreateClone(ACipher: TCipher); override;
    class function BlockSize: Integer; override;
    class function Mode: TCipherMode; override;
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PChar; override;
    class function BlockVectorSize: Integer; override;
    class function MaxKeySize: Integer; override;
    class function MinKeySize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
  end;

  T3DES_CFB = class(T3DES_ECB)
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
    property ModeRatio;
  end;

  T3DES_OFB = class(T3DES_CFB)
  public
    class function Mode: TCipherMode; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  T3DES_PCFB = class(T3DES_CFB)
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  T3DES_PipedPCFB = class(T3DES_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  T3DES_ABC = class(T3DES_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  T3DES_CTR = class(T3DES_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  T3DES_CBC = class(T3DES_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

function CheckDESKeyParity(const Key; Len: Integer): Boolean;
procedure DESKeyParity(var Key; Len: Integer);

function IsDESWeakKey(var Key; Len: Integer): Boolean;
                    
{$IFDEF SHA1}
function TrDESKeyWrapDecrypt(const InBuf; InBufLen: Integer;
                             const Key; KeyLen: Integer;
                             var OutBuf; OutBufLen: Integer): Integer; overload;
function TrDESKeyWrapEncrypt(const InBuf; InBufLen: Integer;
                             const Key; KeyLen: Integer;
                             var OutBuf; OutBufLen: Integer): Integer; overload;

function TrDESKeyWrapDecrypt(InBuf: ISecretKey;
                             Key: ISecretKey;
                             var OutBuf: ISecretKey): Boolean; overload;
function TrDESKeyWrapEncrypt(InBuf: ISecretKey;
                             Key: ISecretKey;
                             var OutBuf: ISecretKey): Boolean; overload;
{$ENDIF SHA1}

implementation

uses
  SysUtils, MpYarrow;

function CheckDESKeyParity(const Key; Len: Integer): Boolean;
var
  P: ^Byte;
  Bit: Byte;
  Parity: Byte;
begin
  P := @Key;
  Result := True;
  while Result and (Len > 0) do begin
    Parity := 1;
    Bit := $80;
    repeat
      if Bit and P^ > 0 then
        Parity := Parity xor 1;
      Bit := Bit shr 1;
    until Bit = 1;
    Result := (P^ and $1) = Parity;
    Dec(Len);
    Inc(P);
  end;
end;

procedure DESKeyParity(var Key; Len: Integer);
var
  P: ^Byte;
  Bit: Byte;
  Parity: Boolean;
begin
  P := @Key;
  while Len > 0 do begin
    Parity := True;
    Bit := $80;
    repeat
      Parity := Parity xor (Bit and P^ > 0);
      Bit := Bit shr 1;
    until Bit = 1;
    if Parity then
      P^ := (P^ and $FE) or 1
    else
      P^ := P^ and $FE;
    Dec(Len);
    Inc(P);
  end;
end;

function IsDESWeakKey(var Key; Len: Integer): Boolean;
const
  WeakKeys: array [0..15,0..7] of Byte = (
    ($01,$01,$01,$01,$01,$01,$01,$01),
    ($1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F),
    ($E0,$E0,$E0,$E0,$F1,$F1,$F1,$F1),
    ($FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE),

    ($E0,$FE,$E0,$FE,$F1,$FE,$F1,$FE),
    ($FE,$E0,$FE,$E0,$FE,$F1,$FE,$F1),
    ($1F,$FE,$1F,$FE,$0E,$FE,$0E,$FE),
    ($FE,$1F,$FE,$1F,$FE,$0E,$FE,$0E),
    ($01,$FE,$01,$FE,$01,$FE,$01,$FE),
    ($FE,$01,$FE,$01,$FE,$01,$FE,$01),
    ($1F,$E0,$1F,$E0,$1F,$E0,$F1,$E0),
    ($E0,$1F,$E0,$1F,$E0,$1F,$E0,$1F),
    ($01,$E0,$01,$E0,$01,$E0,$01,$E0),
    ($E0,$01,$E0,$01,$E0,$01,$E0,$01),
    ($01,$1F,$01,$1F,$01,$0E,$01,$0E),
    ($1F,$01,$1F,$01,$0E,$01,$0E,$01)
  );
var
  I: Integer;
  P: Pointer;
begin
  DESKeyParity(Key,Len);
  P := @Key;
  Result := False;
  while (Len >= 8) and not Result do begin
    for I := Low(WeakKeys) to High(WeakKeys) do begin
      Result := CompareMem(P,@WeakKeys[I],8);
      if Result then Break;
    end;
    Len := Len - 8;
    P := Ptr(LongInt(P) + 8);
  end;
end;

{$IFDEF SHA1}
const
  IV1: packed array [0..7] of Byte = ($4a,$dd,$a2,$2c,$79,$e8,$21,$05);

function TrDESKeyWrapDecrypt(const InBuf; InBufLen: Integer;
                             const Key; KeyLen: Integer;
                             var OutBuf; OutBufLen: Integer): Integer;
var
  C: T3DES_CBC;
  Temp: packed array [0..39] of Byte;
  Tmp: Byte;
  I: Integer;
  H: TSHA1;
  Dig: packed array [0..19] of Byte;
begin
  if (InBufLen <> 40) or (OutBufLen < 24) then
    Result := -1
  else begin
    Move(InBuf,Temp,40);
    try
      C := T3DES_CBC.Create(Key,KeyLen,0);
      try
        C.SetVectorBuf(IV1,8);
        C.Decrypt(Temp,40);
        for I := 0 to 19 do begin
          Tmp := Temp[I];
          Temp[I] := Temp[39 - I];
          Temp[39 - I] := Tmp;
        end;
        C.SetVectorBuf(Temp,8);
        C.Decrypt(Temp[8],32);
        H := TSHA1.Create(Temp[8],24);
        try
          H.Done(@Dig);
        finally
          H.Free;
        end;
        if CompareMem(@Dig,@Temp[32],8) then begin
          if CheckDESKeyParity(Temp[8],24) then begin
            Move(Temp[8],OutBuf,24);
            Result := 24;
          end else
            Result := 0;
        end else
          Result := 0;
      finally
        C.Free;
      end;
    finally
      ProtectClear(Temp,SizeOf(Temp));
    end;
  end;
end;

{$DEFINE KEYWRAPTEST}
function TrDESKeyWrapEncrypt(const InBuf; InBufLen: Integer;
                             const Key; KeyLen: Integer;
                             var OutBuf; OutBufLen: Integer): Integer;
{$IFDEF KEYWRAPTEST}
const
  ICV: packed array [0..7] of Byte = ($18,$1b,$7e,$96,$86,$e0,$4a,$4e);
  IV0: packed array [0..7] of Byte = ($5d,$d4,$cb,$fc,$96,$f5,$45,$3b);
{$ENDIF}
var
  C: T3DES_CBC;
  Temp: packed array [0..39] of Byte;
  Tmp: Byte;
  I: Integer;
  H: TSHA1;
  Dig: packed array [0..19] of Byte;
begin
  if (InBufLen <> 24) or (OutBufLen < 40) then
    Result := -1
  else begin
    Result := 40;
    try
      Move(InBuf,Temp[8],24);
      DesKeyParity(Temp[8],24);
      H := TSHA1.Create(Temp[8],24);
      try
        H.Done(@Dig);
      finally
        H.Free;
      end;
      {$IFDEF KEYWRAPTEST}
      Assert(CompareMem(@Dig,@ICV,8),'ICV Mismatch');
      {$ENDIF}
      Move(Dig,Temp[32],8);
      {$IFDEF KEYWRAPTEST}
      Move(IV0,Temp,8);
      {$ELSE}
      RawRandom(Temp,64);
      {$ENDIF}
      C := T3DES_CBC.Create(Key,KeyLen,0);
      try
        C.SetVectorBuf(Temp,8);
        C.Encrypt(Temp[8],32);
        for I := 0 to 19 do begin
          Tmp := Temp[I];
          Temp[I] := Temp[39 - I];
          Temp[39 - I] := Tmp;
        end;
        C.SetVectorBuf(IV1,8);
        C.Encrypt(Temp,40);
      finally
        C.Free;
      end;
      Move(Temp,OutBuf,40);
    finally
      ProtectClear(Temp,SizeOf(Temp));
    end;
  end;
end;

function TrDESKeyWrapDecrypt(InBuf: ISecretKey;
                             Key: ISecretKey;
                             var OutBuf: ISecretKey): Boolean;
begin
  if OutBuf = nil then
    OutBuf := TSecretKey.Create('');
  OutBuf.SetLength(24);
  Result := TrDESKeyWrapDecrypt(InBuf.Key^,InBuf.KeyLen,
                                Key.Key^,Key.KeyLen,
                                OutBuf.Key^,OutBuf.KeyLen) = 24;
end;

function TrDESKeyWrapEncrypt(InBuf: ISecretKey;
                             Key: ISecretKey;
                             var OutBuf: ISecretKey): Boolean;
begin
  if OutBuf = nil then
    OutBuf := TSecretKey.Create('');
  OutBuf.SetLength(40);
  Result := TrDESKeyWrapEncrypt(InBuf.Key^,InBuf.KeyLen,
                                Key.Key^,Key.KeyLen,
                                OutBuf.Key^,OutBuf.KeyLen) = 40;
end;         
{$ENDIF SHA1}

{ T3DES_ECB }

class function T3DES_ECB.Algorithm: TCipherAlg;
begin
  Result := caDES;
end;

class function T3DES_ECB.AlgorithmName: PChar;
begin
  Result := 'DES';
end;

class function T3DES_ECB.BlockSize: Integer;
begin
  Result := 8;
end;

class function T3DES_ECB.BlockVectorSize: Integer;
begin
  Result := 0;
end;

procedure T3DES_ECB.CleanUp;
begin
  ProtectClear(FSubKeys,Sizeof(FSubKeys));
  ProtectClear(IV,Sizeof(IV));
end;

constructor T3DES_ECB.Create(const AKey; Count, VectorSize: Integer);
begin    
  FIV := @IV;
  inherited;
end;

procedure _Encrypt(BlockIn, BlockOut: Pointer; SubKeys: Pointer);
{$IFDEF DES}
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov EBX,EAX
  mov EDI,EDX
  mov ESI,ECX

  mov EAX,[EBX]
  bswap EAX
  mov EDX,[EBX + 4]
  bswap EDX

  mov ECX,EAX
  shr ECX,4
  xor ECX,EDX
  and ECX,$0F0F0F0F
  xor EDX,ECX
  shl ECX,4
  xor EAX,ECX

  mov ECX,EAX
  shr ECX,16
  xor ECX,EDX
  xor DX,CX
  shl ECX,16
  xor EAX,ECX

  mov ECX,EDX
  shr ECX,2
  xor ECX,EAX
  and ECX,$33333333
  xor EAX,ECX
  shl ECX,2
  xor EDX,ECX

  mov ECX,EDX
  shr ECX,8
  xor ECX,EAX
  and ECX,$00FF00FF
  xor EAX,ECX
  shl ECX,8
  xor EDX,ECX

  rol EDX,1
  mov ECX,EAX
  xor ECX,EDX
  and ECX,$AAAAAAAA
  xor EDX,ECX
  xor EAX,ECX
  rol EAX,1

  mov EBP,8
@@1:
  mov EBX,EDX
  ror EBX,4
  xor EBX,[ESI]
  and EBX,$3F3F3F3F
  movzx ECX,BL
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*0].dword
  movzx ECX,BH
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*1].dword
  shr EBX,16
  movzx ECX,BL
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*2].dword
  movzx ECX,BH
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*3].dword
  mov EBX,EDX
  xor EBX,[ESI + 4]
  and EBX,$3F3F3F3F
  movzx ECX,BL
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*4].dword
  movzx ECX,BH
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*5].dword
  shr EBX,16
  movzx ECX,BL
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*6].dword
  movzx ECX,BH
  xor EAX,[OFFSET @DESBox + ECX*4 + 256*7].dword

  mov EBX,EAX
  ror EBX,4
  xor EBX,[ESI + 8]
  and EBX,$3F3F3F3F
  movzx ECX,BL
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*0].dword
  movzx ECX,BH
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*1].dword
  shr EBX,16
  movzx ECX,BL
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*2].dword
  movzx ECX,BH
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*3].dword
  mov EBX,EAX
  xor EBX,[ESI + 12]
  and EBX,$3F3F3F3F
  movzx ECX,BL
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*4].dword
  movzx ECX,BH
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*5].dword
  shr EBX,16
  movzx ECX,BL
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*6].dword
  movzx ECX,BH
  xor EDX,[OFFSET @DESBox + ECX*4 + 256*7].dword

  add ESI,16

  dec EBP
  jnz @@1

  ror EDX,1
  mov ECX,EAX
  xor ECX,EDX
  and ECX,$AAAAAAAA
  xor EDX,ECX
  xor EAX,ECX
  ror EAX,1

  mov ECX,EAX
  shr ECX,8
  xor ECX,EDX
  and ECX,$00FF00FF
  xor EDX,ECX
  shl ECX,8
  xor EAX,ECX

  mov ECX,EAX
  shr ECX,2
  xor ECX,EDX
  and ECX,$33333333
  xor EDX,ECX
  shl ECX,2
  xor EAX,ECX

  mov ECX,EDX
  shr ECX,16
  xor ECX,EAX
  xor AX,CX
  shl ECX,16
  xor EDX,ECX

  mov ECX,EDX
  shr ECX,4
  xor ECX,EAX
  and ECX,$0F0F0F0F
  xor EAX,ECX
  shl ECX,4
  xor EDX,ECX

  bswap EDX
  mov [EDI].dword,EDX
  bswap EAX
  mov [EDI + 4].dword,EAX

  pop EBP
  pop ESI
  pop EDI
  pop EBX
  ret

  DB 0,0,0
@DESBox:
  DD  $00200000,$04200002,$04000802,$00000000,$00000800,$04000802,$00200802,$04200800
  DD  $04200802,$00200000,$00000000,$04000002,$00000002,$04000000,$04200002,$00000802
  DD  $04000800,$00200802,$00200002,$04000800,$04000002,$04200000,$04200800,$00200002
  DD  $04200000,$00000800,$00000802,$04200802,$00200800,$00000002,$04000000,$00200800
  DD  $04000000,$00200800,$00200000,$04000802,$04000802,$04200002,$04200002,$00000002
  DD  $00200002,$04000000,$04000800,$00200000,$04200800,$00000802,$00200802,$04200800
  DD  $00000802,$04000002,$04200802,$04200000,$00200800,$00000000,$00000002,$04200802
  DD  $00000000,$00200802,$04200000,$00000800,$04000002,$04000800,$00000800,$00200002
  DD  $00000100,$02080100,$02080000,$42000100,$00080000,$00000100,$40000000,$02080000
  DD  $40080100,$00080000,$02000100,$40080100,$42000100,$42080000,$00080100,$40000000
  DD  $02000000,$40080000,$40080000,$00000000,$40000100,$42080100,$42080100,$02000100
  DD  $42080000,$40000100,$00000000,$42000000,$02080100,$02000000,$42000000,$00080100
  DD  $00080000,$42000100,$00000100,$02000000,$40000000,$02080000,$42000100,$40080100
  DD  $02000100,$40000000,$42080000,$02080100,$40080100,$00000100,$02000000,$42080000
  DD  $42080100,$00080100,$42000000,$42080100,$02080000,$00000000,$40080000,$42000000
  DD  $00080100,$02000100,$40000100,$00080000,$00000000,$40080000,$02080100,$40000100
  DD  $00000208,$08020200,$00000000,$08020008,$08000200,$00000000,$00020208,$08000200
  DD  $00020008,$08000008,$08000008,$00020000,$08020208,$00020008,$08020000,$00000208
  DD  $08000000,$00000008,$08020200,$00000200,$00020200,$08020000,$08020008,$00020208
  DD  $08000208,$00020200,$00020000,$08000208,$00000008,$08020208,$00000200,$08000000
  DD  $08020200,$08000000,$00020008,$00000208,$00020000,$08020200,$08000200,$00000000
  DD  $00000200,$00020008,$08020208,$08000200,$08000008,$00000200,$00000000,$08020008
  DD  $08000208,$00020000,$08000000,$08020208,$00000008,$00020208,$00020200,$08000008
  DD  $08020000,$08000208,$00000208,$08020000,$00020208,$00000008,$08020008,$00020200
  DD  $01010400,$00000000,$00010000,$01010404,$01010004,$00010404,$00000004,$00010000
  DD  $00000400,$01010400,$01010404,$00000400,$01000404,$01010004,$01000000,$00000004
  DD  $00000404,$01000400,$01000400,$00010400,$00010400,$01010000,$01010000,$01000404
  DD  $00010004,$01000004,$01000004,$00010004,$00000000,$00000404,$00010404,$01000000
  DD  $00010000,$01010404,$00000004,$01010000,$01010400,$01000000,$01000000,$00000400
  DD  $01010004,$00010000,$00010400,$01000004,$00000400,$00000004,$01000404,$00010404
  DD  $01010404,$00010004,$01010000,$01000404,$01000004,$00000404,$00010404,$01010400
  DD  $00000404,$01000400,$01000400,$00000000,$00010004,$00010400,$00000000,$01010004
  DD  $10001040,$00001000,$00040000,$10041040,$10000000,$10001040,$00000040,$10000000
  DD  $00040040,$10040000,$10041040,$00041000,$10041000,$00041040,$00001000,$00000040
  DD  $10040000,$10000040,$10001000,$00001040,$00041000,$00040040,$10040040,$10041000
  DD  $00001040,$00000000,$00000000,$10040040,$10000040,$10001000,$00041040,$00040000
  DD  $00041040,$00040000,$10041000,$00001000,$00000040,$10040040,$00001000,$00041040
  DD  $10001000,$00000040,$10000040,$10040000,$10040040,$10000000,$00040000,$10001040
  DD  $00000000,$10041040,$00040040,$10000040,$10040000,$10001000,$10001040,$00000000
  DD  $10041040,$00041000,$00041000,$00001040,$00001040,$00040040,$10000000,$10041000
  DD  $20000010,$20400000,$00004000,$20404010,$20400000,$00000010,$20404010,$00400000
  DD  $20004000,$00404010,$00400000,$20000010,$00400010,$20004000,$20000000,$00004010
  DD  $00000000,$00400010,$20004010,$00004000,$00404000,$20004010,$00000010,$20400010
  DD  $20400010,$00000000,$00404010,$20404000,$00004010,$00404000,$20404000,$20000000
  DD  $20004000,$00000010,$20400010,$00404000,$20404010,$00400000,$00004010,$20000010
  DD  $00400000,$20004000,$20000000,$00004010,$20000010,$20404010,$00404000,$20400000
  DD  $00404010,$20404000,$00000000,$20400010,$00000010,$00004000,$20400000,$00404010
  DD  $00004000,$00400010,$20004010,$00000000,$20404000,$20000000,$00400010,$20004010
  DD  $00802001,$00002081,$00002081,$00000080,$00802080,$00800081,$00800001,$00002001
  DD  $00000000,$00802000,$00802000,$00802081,$00000081,$00000000,$00800080,$00800001
  DD  $00000001,$00002000,$00800000,$00802001,$00000080,$00800000,$00002001,$00002080
  DD  $00800081,$00000001,$00002080,$00800080,$00002000,$00802080,$00802081,$00000081
  DD  $00800080,$00800001,$00802000,$00802081,$00000081,$00000000,$00000000,$00802000
  DD  $00002080,$00800080,$00800081,$00000001,$00802001,$00002081,$00002081,$00000080
  DD  $00802081,$00000081,$00000001,$00002000,$00800001,$00002001,$00802080,$00800081
  DD  $00002001,$00002080,$00800000,$00802001,$00000080,$00800000,$00002000,$00802080
  DD  $80108020,$80008000,$00008000,$00108020,$00100000,$00000020,$80100020,$80008020
  DD  $80000020,$80108020,$80108000,$80000000,$80008000,$00100000,$00000020,$80100020
  DD  $00108000,$00100020,$80008020,$00000000,$80000000,$00008000,$00108020,$80100000
  DD  $00100020,$80000020,$00000000,$00108000,$00008020,$80108000,$80100000,$00008020
  DD  $00000000,$00108020,$80100020,$00100000,$80008020,$80100000,$80108000,$00008000
  DD  $80100000,$80008000,$00000020,$80108020,$00108020,$00000020,$00008000,$80000000
  DD  $00008020,$80108000,$00100000,$80000020,$00100020,$80008020,$80000020,$00100020
  DD  $00108000,$00000000,$80008000,$00008020,$80000000,$80100020,$80108020,$00108000
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

constructor T3DES_ECB.CreateClone(ACipher: TCipher);
begin                      
  VirtualLock;
  if ACipher is T3DES_ECB then begin  
    FIV := @IV;      
    FFBIndex := T3DES_ECB(ACipher).FFBIndex;
    IV := T3DES_ECB(ACipher).IV;
    ModeRatio := T3DES_ECB(ACipher).ModeRatio;
    FSubKeys := T3DES_ECB(ACipher).FSubKeys;
    if TMethod(T3DES_ECB(ACipher).FEncrBlock).Code =
       @T3DES_ECB.EncryptBlock1 then begin
      FEncrBlock := EncryptBlock1;
      FDecrBlock := DecryptBlock1;
    end else begin
      FEncrBlock := EncryptBlock3;
      FDecrBlock := DecryptBlock3;
    end;
  end else
    inherited;
end;

procedure T3DES_ECB.DecryptBlock(var Buf);
begin
  FDecrBlock(Buf,Buf);
end;

procedure T3DES_ECB.DecryptBlock1(var Dst; const Src);
begin
  _Encrypt(@Src,@Dst,@FSubKeys[96]);
end;

procedure T3DES_ECB.DecryptBlock3(var Dst; const Src);
begin
  _Encrypt(@Src,@Dst,@FSubKeys[96]);
  _Encrypt(@Dst,@Dst,@FSubKeys[128]);
  _Encrypt(@Dst,@Dst,@FSubKeys[160]);
end;

procedure T3DES_ECB.DecryptBlockToDst(var Dst; const Src);
begin
  FDecrBlock(Dst,Src);
end;

procedure T3DES_ECB.EncryptBlock(var Buf);
begin
  FEncrBlock(Buf,Buf);
end;

procedure T3DES_ECB.EncryptBlock1(var Dst; const Src);
begin
  _Encrypt(@Src,@Dst,@FSubKeys);
end;

procedure T3DES_ECB.EncryptBlock3(var Dst; const Src);
begin
  _Encrypt(@Src,@Dst,@FSubKeys);
  _Encrypt(@Dst,@Dst,@FSubKeys[32]);
  _Encrypt(@Dst,@Dst,@FSubKeys[64]);
end;

procedure T3DES_ECB.EncryptBlockToDst(var Dst; const Src);
begin
  FEncrBlock(Dst,Src);
end;

class function T3DES_ECB.KeyedIVOID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

class function T3DES_ECB.MaxKeySize: Integer;
begin
  Result := 24;
end;

class function T3DES_ECB.MinKeySize: Integer;
begin
  Result := 8;
end;

class function T3DES_ECB.Mode: TCipherMode;
begin
  Result := cmECB;
end;

const
  PC1: packed array[0..55] of Byte =
   (63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22, 14, 6,
    61, 53, 45, 37, 29, 21, 13,  5, 60, 52, 44, 36,
    57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18, 10, 2,
    59, 51, 43, 35, 27, 19, 11,  3, 28, 20, 12,  4);
  Rols: packed array [0..15] of Byte =
   (1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1);


procedure MakeSubKeys(Key, SubKeys: Pointer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  mov ESI,EAX
  mov EDI,EDX

  xor EAX,EAX
  xor EDX,EDX

  xor EBX,EBX
@@1:
  movzx ECX,[PC1 + EBX].byte
  bt [ESI],ECX
  adc EAX,0
  ror EAX,1
  inc EBX
  cmp EBX,28
  jb @@1
  ror EAX,4
@@2:
  movzx ECX,[PC1 + EBX].byte
  bt [ESI],ECX
  adc EDX,0
  ror EDX,1
  inc EBX
  cmp EBX,56
  jb @@2
  ror EDX,4

  xor EBP,EBP
@@3:
  mov CL,[Rols + EBP].byte
  ror EAX,CL
  ror EDX,CL
  mov ECX,EAX
  and ECX,$F0000000
  shr ECX,4
  or  EAX,ECX
  and EAX,$0FFFFFFF
  mov ECX,EDX      
  and ECX,$F0000000
  shr ECX,4
  or  EDX,ECX
  and EDX,$0FFFFFFF

  xor EBX,EBX

  bt EAX,13
  adc EBX,0
  shl EBX,1

  bt EAX,16
  adc EBX,0
  shl EBX,1

  bt EAX,10
  adc EBX,0
  shl EBX,1

  bt EAX,23
  adc EBX,0
  shl EBX,1

  bt EAX,0
  adc EBX,0
  shl EBX,1

  bt EAX,4
  adc EBX,0
  shl EBX,3

  bt EAX,22
  adc EBX,0
  shl EBX,1

  bt EAX,18
  adc EBX,0
  shl EBX,1

  bt EAX,11
  adc EBX,0
  shl EBX,1

  bt EAX,3
  adc EBX,0
  shl EBX,1

  bt EAX,25
  adc EBX,0
  shl EBX,1

  bt EAX,7
  adc EBX,0
  shl EBX,3

  bt EDX,12
  adc EBX,0
  shl EBX,1

  bt EDX,23
  adc EBX,0
  shl EBX,1

  bt EDX,2
  adc EBX,0
  shl EBX,1

  bt EDX,8
  adc EBX,0
  shl EBX,1

  bt EDX,18
  adc EBX,0
  shl EBX,1

  bt EDX,26
  adc EBX,0
  shl EBX,3

  bt EDX,15
  adc EBX,0
  shl EBX,1

  bt EDX,20
  adc EBX,0
  shl EBX,1

  bt EDX,10
  adc EBX,0
  shl EBX,1

  bt EDX,27
  adc EBX,0
  shl EBX,1

  bt EDX,5
  adc EBX,0
  shl EBX,1

  bt EDX,24
  adc EBX,0

  mov [EDI].dword,EBX
  xor EBX,EBX

  bt EAX,2
  adc EBX,0
  shl EBX,1

  bt EAX,27
  adc EBX,0
  shl EBX,1

  bt EAX,14
  adc EBX,0
  shl EBX,1

  bt EAX,5
  adc EBX,0
  shl EBX,1

  bt EAX,20
  adc EBX,0
  shl EBX,1

  bt EAX,9
  adc EBX,0
  shl EBX,3

  bt EAX,15
  adc EBX,0
  shl EBX,1

  bt EAX,6
  adc EBX,0
  shl EBX,1

  bt EAX,26
  adc EBX,0
  shl EBX,1

  bt EAX,19
  adc EBX,0
  shl EBX,1

  bt EAX,12
  adc EBX,0
  shl EBX,1

  bt EAX,1
  adc EBX,0
  shl EBX,3

  bt EDX,1
  adc EBX,0
  shl EBX,1

  bt EDX,11
  adc EBX,0
  shl EBX,1

  bt EDX,22
  adc EBX,0
  shl EBX,1

  bt EDX,16
  adc EBX,0
  shl EBX,1

  bt EDX,4
  adc EBX,0
  shl EBX,1

  bt EDX,19
  adc EBX,0
  shl EBX,3

  bt EDX,17
  adc EBX,0
  shl EBX,1

  bt EDX,13
  adc EBX,0
  shl EBX,1

  bt EDX,21
  adc EBX,0
  shl EBX,1

  bt EDX,7
  adc EBX,0
  shl EBX,1

  bt EDX,0
  adc EBX,0
  shl EBX,1

  bt EDX,3
  adc EBX,0

  mov [EDI + 4].dword,EBX

  add EDI,8

  inc EBP
  cmp EBP,16
  jb @@3

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

class function T3DES_ECB.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.36.3.1.1.1';
    24: Result := '1.3.36.3.1.3.1.1';
  else
    Result := nil;
  end;
end;

procedure T3DES_ECB.SetUp(const AKey; Count, VectorSize: Integer);
var
  Key: packed array [0..5] of LongWord;
  I: Integer;
begin
  Nag(Count);
  CleanUp;
  if (Count<= 0) then
    raise Exception.Create(Format('3DES: Invalid key size - %d',[Count]));

  FillChar(Key,SizeOf(Key),0);
  try
    if Count <= MinKeySize then begin
      Move(AKey,Key,Count);
      MakeSubKeys(@Key,@FSubKeys);
      for I := 0 to 15 do begin
        FSubKeys[96 + 30 - I*2] := FSubKeys[I*2];
        FSubKeys[96 + 30 - I*2 + 1] := FSubKeys[I*2 + 1];
      end;
      FEncrBlock := EncryptBlock1;
      FDecrBlock := DecryptBlock1;
    end else if Count <= 16 then begin
      Move(AKey,Key,Count);
      MakeSubKeys(@Key,@FSubKeys);
      for I := 0 to 15 do begin
        FSubKeys[160 + 30 - I*2] := FSubKeys[I*2];
        FSubKeys[160 + 30 - I*2 + 1] := FSubKeys[I*2 + 1];
      end;
      MakeSubKeys(@Key[2],@FSubKeys[128]);
      for I := 0 to 15 do begin
        FSubKeys[32 + 30 - I*2] := FSubKeys[128 + I*2];
        FSubKeys[32 + 30 - I*2 + 1] := FSubKeys[128 + I*2 + 1];
      end;
      Move(FSubKeys,FSubKeys[64],128);
      Move(FSubKeys[160],FSubKeys[96],128);
      FEncrBlock := EncryptBlock3;
      FDecrBlock := DecryptBlock3;
    end else begin
      if Count <= MaxKeySize then
        Move(AKey,Key,Count)
      else
        Move(AKey,Key,MaxKeySize);
      MakeSubKeys(@Key,@FSubKeys);
      for I := 0 to 15 do begin
        FSubKeys[160 + 30 - I*2] := FSubKeys[I*2];
        FSubKeys[160 + 30 - I*2 + 1] := FSubKeys[I*2 + 1];
      end;
      MakeSubKeys(@Key[2],@FSubKeys[128]);
      for I := 0 to 15 do begin
        FSubKeys[32 + 30 - I*2] := FSubKeys[128 + I*2];
        FSubKeys[32 + 30 - I*2 + 1] := FSubKeys[128 + I*2 + 1];
      end;
      MakeSubKeys(@Key[4],@FSubKeys[64]);
      for I := 0 to 15 do begin
        FSubKeys[96 + 30 - I*2] := FSubKeys[64 + I*2];
        FSubKeys[96 + 30 - I*2 + 1] := FSubKeys[64 + I*2 + 1];
      end;
      FEncrBlock := EncryptBlock3;
      FDecrBlock := DecryptBlock3;
    end;
  finally
    ProtectClear(Key,SizeOf(Key));
  end;
end;

{ T3DES_CFB }

class function T3DES_CFB.BlockVectorSize: Integer;
begin
   Result := 1;
end;

constructor T3DES_CFB.Create(const AKey; Count, VectorSize: Integer);
begin
  ModeRatio := 8;
  inherited;
end;

class function T3DES_CFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.82';
    16: Result := '1.3.6.1.4.1.13085.1.83';
    24: Result := '1.3.6.1.4.1.13085.1.84';
  else
    Result := nil;
  end;
end;

class function T3DES_CFB.Mode: TCipherMode;
begin
  Result := cmCFB;
end;

class function T3DES_CFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ T3DES_OFB }

class function T3DES_OFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.79';
    16: Result := '1.3.6.1.4.1.13085.1.80';
    24: Result := '1.3.6.1.4.1.13085.1.81';
  else
    Result := nil;
  end;
end;

class function T3DES_OFB.Mode: TCipherMode;
begin
  Result := cmOFB;
end;

class function T3DES_OFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ T3DES_PCFB }

constructor T3DES_PCFB.Create(const AKey; Count, VectorSize: Integer);
begin
  inherited;
  ModeRatio := 1;
end;

procedure T3DES_PCFB.Decrypt(var Buf; Count: Integer);
var
  i, j, c: longint;
  CT: array[0..8] of byte;
begin
  c := Count div ModeRatio;
  for i:= 0 to c-1 do begin
    Move(PByteArray(@Buf)^[i*ModeRatio],CT,ModeRatio);
    FEncrBlock(IV,IV);
    for j := 0 to ModeRatio - 1 do
      PByteArray(@Buf)^[i*ModeRatio + j]:=
        PByteArray(@Buf)^[i*ModeRatio + j] xor IV[j];
    Move(IV[ModeRatio],IV[0],8 - ModeRatio);
    Move(CT,IV[8 - ModeRatio],ModeRatio);
  end;
  if (Count mod ModeRatio) <> 0 then begin
    Move(PByteArray(@Buf)^[c*ModeRatio],CT,Count mod ModeRatio);
    FEncrBlock(IV,IV);
    for j := 0 to (Count mod ModeRatio) - 1 do
      PByteArray(@Buf)^[c*ModeRatio + j]:=
        PByteArray(@Buf)^[c*ModeRatio + j] xor IV[j];
    Move(IV[Count mod ModeRatio],IV[0],8 - (Count mod ModeRatio));
    Move(CT,IV[8 - (Count mod ModeRatio)],(Count mod ModeRatio));
  end;
end;

procedure T3DES_PCFB.Encrypt(var Buf; Count: Integer);
var
  i, j, c: longint;
begin
  c := Count div ModeRatio;
  for i:= 0 to c-1 do begin
    FEncrBlock(IV,IV);
    for j := 0 to ModeRatio - 1 do
      PByteArray(@Buf)^[i*ModeRatio + j]:=
        PByteArray(@Buf)^[i*ModeRatio + j] xor IV[j];
    Move(IV[ModeRatio],IV[0],8 - ModeRatio);
    Move(PByteArray(@Buf)^[i*ModeRatio],IV[8 - ModeRatio],ModeRatio);
  end;
  if (Count mod ModeRatio) <> 0 then begin
    FEncrBlock(IV,IV);
    for j := 0 to (Count mod ModeRatio) - 1 do
      PByteArray(@Buf)^[c*ModeRatio + j]:=
        PByteArray(@Buf)^[c*ModeRatio + j] xor IV[j];
    Move(IV[Count mod ModeRatio],IV[0],8 - (Count mod ModeRatio));
    Move(PByteArray(@Buf)^[c*ModeRatio],IV[8 - (Count mod ModeRatio)],Count mod ModeRatio);
  end;
end;

class function T3DES_PCFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.67';
    16: Result := '1.3.6.1.4.1.13085.1.68';
    24: Result := '1.3.6.1.4.1.13085.1.69';
  else
    Result := nil;
  end;
end;

class function T3DES_PCFB.Mode: TCipherMode;
begin
  Result := cmPCFB;
end;

class function T3DES_PCFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ T3DES_PipedPCFB }

class function T3DES_PipedPCFB.BlockVectorSize: Integer;
begin
  Result := 2;
end;

procedure T3DES_PipedPCFB.Decrypt(var Buf; Count: Integer);
var
  BlockCount, I: longint;
begin
  BlockCount := Count div BlockSize;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[16],8);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock64(IV[16],IV[8]);
    FEncrBlock(IV[16],IV[16]);

    Move(Ptr(LongInt(@Buf) + I*8)^,IV[8],8);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock64(IV[0],IV[16]);
    FEncrBlock(IV[0],IV[0]);
    XORBlock64(Ptr(LongInt(@Buf) + I*8)^,IV[0]);

    Move(IV[16],IV[0],8);
  end;

  if BlockCount * 8 < Count then begin
    Move(IV[0],IV[16],8);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock64(IV[16],IV[8]);
    FEncrBlock(IV[16],IV[16]);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock64(IV[0],IV[16]);
    FEncrBlock(IV[0],IV[0]);
    for I := 0 to Count - BlockCount * 8 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*8)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*8)^) xor IV[I];
  end;
end;

procedure T3DES_PipedPCFB.Encrypt(var Buf; Count: Integer);
var
  BlockCount, I: longint;
begin
  BlockCount := Count div BlockSize;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[16],8);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock64(IV[16],IV[8]);
    FEncrBlock(IV[16],IV[16]);

    // C[i] := P[i] xor E(V[i] + V[i-1]);
    AddBlock64(IV[0],IV[16]);
    FEncrBlock(IV[0],IV[0]);
    XORBlock64(Ptr(LongInt(@Buf) + I*8)^,IV[0]);

    Move(Ptr(LongInt(@Buf) + I*8)^,IV[8],8);
    Move(IV[16],IV[0],8);
  end;

  if BlockCount * 8 < Count then begin
    Move(IV[0],IV[16],8);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock64(IV[16],IV[8]);
    FEncrBlock(IV[16],IV[16]);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[16]);
    FEncrBlock(IV[0],IV[0]);
    for I := 0 to Count - BlockCount * 8 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*8)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*8)^) xor IV[I];
  end;
end;

class function T3DES_PipedPCFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.70';
    16: Result := '1.3.6.1.4.1.13085.1.71';
    24: Result := '1.3.6.1.4.1.13085.1.72';
  else
    Result := nil;
  end;
end;

class function T3DES_PipedPCFB.Mode: TCipherMode;
begin
  Result := cmPipedPCFB;
end;

class function T3DES_PipedPCFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ T3DES_ABC }

class function T3DES_ABC.BlockVectorSize: Integer;
begin
  Result := 2;
end;

class function T3DES_ABC.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.64';
    16: Result := '1.3.6.1.4.1.13085.1.65';
    24: Result := '1.3.6.1.4.1.13085.1.66';
  else
    Result := nil;
  end;
end;

class function T3DES_ABC.Mode: TCipherMode;
begin
  Result := cmABC;
end;

class function T3DES_ABC.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ T3DES_CTR }

class function T3DES_CTR.BlockVectorSize: Integer;
begin
  Result := 1;
end;

procedure Inc64(var Buf);
asm
  mov ECX,[EAX]
  mov EDX,[EAX + 4]
  bswap ECX
  bswap EDX
  add EDX,1
  adc ECX,0
  bswap ECX
  bswap EDX
  mov [EAX],ECX
  mov [EAX + 4],EDX
end;

class function T3DES_CTR.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.61';
    16: Result := '1.3.6.1.4.1.13085.1.62';
    24: Result := '1.3.6.1.4.1.13085.1.63';
  else
    Result := nil;
  end;
end;

class function T3DES_CTR.Mode: TCipherMode;
begin
  Result := cmCTR;
end;

class function T3DES_CTR.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ T3DES_CBC }

class function T3DES_CBC.BlockVectorSize: Integer;
begin
  Result := 1;
end;

class function T3DES_CBC.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.6.1.4.1.13085.1.76';
    16: Result := '1.3.6.1.4.1.13085.1.77';
    24: Result := '1.3.6.1.4.1.13085.1.78';
  else
    Result := nil;
  end;
end;

class function T3DES_CBC.Mode: TCipherMode;
begin
  Result := cmCBC;
end;

class function T3DES_CBC.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    8:  Result := '1.3.14.3.2.7';
    24: Result := '1.2.840.113549.3.7';
  else
    Result := nil;
  end;
end;

{$IFDEF DES}
initialization
  {$IFDEF ECB}       RegisterCipherClass( T3DES_ECB );       {$ENDIF}
  {$IFDEF ABC}       RegisterCipherClass( T3DES_ABC );       {$ENDIF}
  {$IFDEF CBC}       RegisterCipherClass( T3DES_CBC );       {$ENDIF}
  {$IFDEF CFB}       RegisterCipherClass( T3DES_CFB );       {$ENDIF}
  {$IFDEF CTR}       RegisterCipherClass( T3DES_CTR );       {$ENDIF}
  {$IFDEF OFB}       RegisterCipherClass( T3DES_OFB );       {$ENDIF}
  {$IFDEF PCFB}      RegisterCipherClass( T3DES_PCFB );      {$ENDIF}
  {$IFDEF PIPEDPCFB} RegisterCipherClass( T3DES_PipedPCFB ); {$ENDIF}
{$ENDIF}
end.
