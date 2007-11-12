{$Q-,R-}
{$I ver.inc}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Helix Unit                                        }
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
unit SsHelix;

interface

uses
  SecUtils;

type
  TK = packed array [0..7] of LongWord;
  TZ = packed array [0..4] of LongWord;

  THelixState = packed record
    N: TK;
    K: TK;
    Z: TZ;
    LU: LongWord;
    I: Int64;
    LP: Byte;
  end;
  
  THelix = class(TCipherMAC)
  private
    FState: THelixState;
    FMac: OctetString;
  protected
    procedure CleanUp; override;
    function GetMac: OctetString; override;
    function GetVector: OctetString; override;
    procedure SetVector(const Value: OctetString); override;
  public
    constructor CreateClone(ACipher: TCipher); override;
    class function BlockSize: Integer; override;
    class function Mode: TCipherMode; override;
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PChar; override;
    class function MacLen: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    procedure EncryptToNil(const Buf; Count: Integer); override;
    procedure GetMacBuf(var Mac; Count: Integer); override;
    procedure GetVectorBuf(var IV; Count: Integer); override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
    procedure SetVectorBuf(const IV; Count: Integer); override;
    class procedure Test;
  end;

implementation

uses
  SysUtils;

function EncryptBlock(var State: THelixState; PlainText: LongWord): LongWord;
asm
  push EBX
  push EDI
  push ESI
  push EBP

  sub ESP,12

  mov ESI,EAX
  mov EBP,EDX

  mov EAX,[ESI+88].dword
  mov EDX,[ESI+92].dword
  add [ESI+88].dword,1
  adc [ESI+92].dword,0

  mov ECX,EAX
  and ECX,07h
  mov EBX,[ESI+ECX*4+32].dword
  mov [ESP+4].dword,EBX
  mov EBX,[ESI+ECX*4].dword
  add ECX,4
  and ECX,07h
  add EBX,[ESI+ECX*4+32].dword
  lea EBX,[EBX+EAX]

  test EAX,01h
  jz @@2
  test EAX,02h
  jz @@1

  shrd EAX,EDX,31
  add EBX,EAX

  jmp @@2
@@1:
  mov EAX,[ESI+84].dword
  lea EAX,[EAX*4]
  add EBX,EAX
@@2:
  mov [ESP+8].dword,EBX

  mov EAX,[ESI+64].dword
  mov EBX,[ESI+68].dword
  mov ECX,[ESI+72].dword
  mov EDX,[ESI+76].dword
  mov EDI,[ESI+80].dword

  mov [ESP].dword,EAX

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+4].dword,EDX
  xor EAX,[ESP+4].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  xor EDX,EBP
  add EAX,EDX
  xor EDX,EBP
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+8].dword,EDX
  xor EAX,[ESP+8].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  mov [ESI+64].dword,EAX
  mov [ESI+68].dword,EBX
  mov [ESI+72].dword,ECX
  mov [ESI+76].dword,EDX
  mov [ESI+80].dword,EDI

  mov EAX,[ESP].dword
  xor EAX,EBP

  add ESP,12

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;       

function DecryptBlock(var State: THelixState; PlainText, Mask: LongWord): LongWord;
asm
  push EBX
  push EDI
  push ESI
  push EBP

  sub ESP,12

  mov [ESP].dword,ECX

  mov ESI,EAX
  mov EBP,EDX

  mov EAX,[ESI+88].dword
  mov EDX,[ESI+92].dword
  add [ESI+88].dword,1
  adc [ESI+92].dword,0

  mov ECX,EAX
  and ECX,07h
  mov EBX,[ESI+ECX*4+32].dword
  mov [ESP+4].dword,EBX
  mov EBX,[ESI+ECX*4].dword
  add ECX,4
  and ECX,07h
  add EBX,[ESI+ECX*4+32].dword
  lea EBX,[EBX+EAX]

  test EAX,01h
  jz @@2
  test EAX,02h
  jz @@1

  shrd EAX,EDX,31
  add EBX,EAX

  jmp @@2
@@1:
  mov EAX,[ESI+84].dword
  lea EAX,[EAX*4]
  add EBX,EAX
@@2:
  mov [ESP+8].dword,EBX

  mov EAX,[ESI+64].dword
  mov EBX,[ESI+68].dword
  mov ECX,[ESI+72].dword
  mov EDX,[ESI+76].dword
  mov EDI,[ESI+80].dword

  xor EBP,EAX
  and EBP,[ESP].dword

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+4].dword,EDX
  xor EAX,[ESP+4].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  xor EDX,EBP
  add EAX,EDX
  xor EDX,EBP
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+8].dword,EDX
  xor EAX,[ESP+8].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  mov [ESI+64].dword,EAX
  mov [ESI+68].dword,EBX
  mov [ESI+72].dword,ECX
  mov [ESI+76].dword,EDX
  mov [ESI+80].dword,EDI

  mov EAX,EBP

  add ESP,12

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure EncryptBlocks(var State: THelixState; var Buf; BlockCount: LongWord);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  push ECX

  sub ESP,12

  mov ESI,EAX
  mov EBP,EDX

@@Main:
  mov EAX,[ESI+88].dword
  mov EDX,[ESI+92].dword
  add [ESI+88].dword,1
  adc [ESI+92].dword,0

  mov ECX,EAX
  and ECX,07h
  mov EBX,[ESI+ECX*4+32].dword
  mov [ESP+4].dword,EBX
  mov EBX,[ESI+ECX*4].dword
  add ECX,4
  and ECX,07h
  add EBX,[ESI+ECX*4+32].dword
  lea EBX,[EBX+EAX]

  test EAX,01h
  jz @@2
  test EAX,02h
  jz @@1

  shrd EAX,EDX,31
  add EBX,EAX

  jmp @@2
@@1:
  mov EAX,[ESI+84].dword
  lea EAX,[EAX*4]
  add EBX,EAX
@@2:
  mov [ESP+8].dword,EBX

  mov EAX,[EBP].dword
  mov [ESP].dword,EAX

  mov EAX,[ESI+64].dword
  mov EBX,[ESI+68].dword
  mov ECX,[ESI+72].dword
  mov EDX,[ESI+76].dword
  mov EDI,[ESI+80].dword

  xor [EBP].dword,EAX

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+4].dword,EDX
  xor EAX,[ESP+4].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  xor [ESP].dword,EDX
  add EAX,[ESP].dword
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+8].dword,EDX
  xor EAX,[ESP+8].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  mov [ESI+64].dword,EAX
  mov [ESI+68].dword,EBX
  mov [ESI+72].dword,ECX
  mov [ESI+76].dword,EDX
  mov [ESI+80].dword,EDI

  lea EBP,[EBP+4]

  dec [ESP+12].dword
  jnz @@Main

  add ESP,16

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure DecryptBlocks(var State: THelixState; var Buf; BlockCount: LongWord);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  push ECX

  sub ESP,12

  mov ESI,EAX
  mov EBP,EDX

@@Main:
  mov EAX,[ESI+88].dword
  mov EDX,[ESI+92].dword
  add [ESI+88].dword,1
  adc [ESI+92].dword,0

  mov ECX,EAX
  and ECX,07h
  mov EBX,[ESI+ECX*4+32].dword
  mov [ESP+4].dword,EBX
  mov EBX,[ESI+ECX*4].dword
  add ECX,4
  and ECX,07h
  add EBX,[ESI+ECX*4+32].dword
  lea EBX,[EBX+EAX]

  test EAX,01h
  jz @@2
  test EAX,02h
  jz @@1

  shrd EAX,EDX,31
  add EBX,EAX

  jmp @@2
@@1:
  mov EAX,[ESI+84].dword
  lea EAX,[EAX*4]
  add EBX,EAX
@@2:
  mov [ESP+8].dword,EBX

  mov EAX,[EBP].dword
  mov [ESP].dword,EAX

  mov EAX,[ESI+64].dword
  mov EBX,[ESI+68].dword
  mov ECX,[ESI+72].dword
  mov EDX,[ESI+76].dword
  mov EDI,[ESI+80].dword

  xor [EBP].dword,EAX
  xor [ESP].dword,EAX

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+4].dword,EDX
  xor EAX,[ESP+4].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  xor [ESP].dword,EDX
  add EAX,[ESP].dword
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  add [ESP+8].dword,EDX
  xor EAX,[ESP+8].dword
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  mov [ESI+64].dword,EAX
  mov [ESI+68].dword,EBX
  mov [ESI+72].dword,ECX
  mov [ESI+76].dword,EDX
  mov [ESI+80].dword,EDI

  lea EBP,[EBP+4]

  dec [ESP+12].dword
  jnz @@Main

  add ESP,16

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure F8(var K: TK; LU: LongWord);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov ECX,4
  push ECX

  mov ESI,EAX
  lea EBP,[EDX+64]

@@Main:
  mov EAX,[ESI   ].dword
  mov EBX,[ESI+ 4].dword
  mov ECX,[ESI+ 8].dword
  mov EDX,[ESI+12].dword
  mov EDI,EBP

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  xor EAX,EDX
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  xor EAX,EDX
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  xor [ESI+16].dword,EAX
  xor [ESI+20].dword,EBX
  xor [ESI+24].dword,ECX
  xor [ESI+28].dword,EDX
  mov EAX,[ESI+16].dword
  mov EBX,[ESI+20].dword
  mov ECX,[ESI+24].dword
  mov EDX,[ESI+28].dword
  mov EDI,EBP

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  xor EAX,EDX
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  add EAX,EDX
  rol EDX,15
  add EBX,EDI
  rol EDI,25
  xor ECX,EAX
  rol EAX,9
  xor EDX,EBX
  rol EBX,10
  add EDI,ECX
  rol ECX,17

  xor EAX,EDX
  rol EDX,30
  xor EBX,EDI
  rol EDI,13
  add ECX,EAX
  rol EAX,20
  add EDX,EBX
  rol EBX,11
  xor EDI,ECX
  rol ECX,5

  xor [ESI   ].dword,EAX
  xor [ESI+ 4].dword,EBX
  xor [ESI+ 8].dword,ECX
  xor [ESI+12].dword,EDX

  dec [ESP].dword
  jnz @@Main

  pop ECX

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

{ THelix }

class function THelix.Algorithm: TCipherAlg;
begin
  Result := caHelix;
end;

class function THelix.AlgorithmName: PChar;
begin
  Result := 'Helix';
end;

class function THelix.BlockSize: Integer;
begin
  Result := 4;
end;

procedure THelix.CleanUp;
begin
  ProtectClear(FState,SizeOf(FState));
  SetLength(FMac,0);
end;

constructor THelix.CreateClone(ACipher: TCipher);
begin
  VirtualLock;
  if ACipher is THelix then begin
    FState := THelix(ACipher).FState;
  end else
    inherited;
end;

procedure THelix.Decrypt(var Buf; Count: Integer);
var
  P, C: LongWord;
begin
  Assert(FState.LP = 0,'Cannot resume decryption after an incomplete block');
  DecryptBlocks(FState,Buf,Count shr 2);
  FState.LP := Count and 3;
  if FState.LP > 0 then begin
    C := 0;
    Move(Pointer(LongInt(@Buf) + Count - FState.LP)^,C,FState.LP);
    P := DecryptBlock(FState,C,$FFFFFFFF shr (32 - FState.LP*8));
    Move(P,Pointer(LongInt(@Buf) + Count - FState.LP)^,FState.LP);
  end;
end;

procedure THelix.Encrypt(var Buf; Count: Integer);
var
  P, C: LongWord;
begin
  Assert(FState.LP = 0,'Cannot resume encryption after an incomplete block');
  EncryptBlocks(FState,Buf,Count shr 2);
  FState.LP := Count and 3;
  if FState.LP > 0 then begin
    P := 0;
    Move(Pointer(LongInt(@Buf) + Count - FState.LP)^,P,FState.LP);
    C := EncryptBlock(FState,P);
    Move(C,Pointer(LongInt(@Buf) + Count - FState.LP)^,FState.LP);
  end;
end;

procedure THelix.EncryptToNil(const Buf; Count: Integer);
var
  B: string;
begin
  // This method is for MAC calculation of unecrypted header data
  Assert(FState.LP = 0,'Cannot resume encryption after an incomplete block');
  B := StringOfChar(#0,((Count + 3) shr 2) * 4);
  Move(Buf,Pointer(B)^,Count);
  EncryptBlocks(FState,Pointer(B)^,(Count + 3) shr 2);
end;

function THelix.GetMac: OctetString;
var
  I: Integer;
  T: array [0..11] of LongWord;
begin
  if FMac = '' then begin
    FState.Z[0] := FState.Z[0] xor $912d94f1;
    for I := 0 to 11 do
      T[I] := FState.LP;
    EncryptBlocks(FState,T,12);
    for I := 8 to 11 do
      T[I] := T[I] xor FState.LP;
    SetLength(FMac,16);
    Move(T[8],Pointer(FMac)^,16);
    FState.I := 0;
    FState.LP := $FF;
  end;
  Result := FMac;
end;

procedure THelix.GetMacBuf(var Mac; Count: Integer);
var
  I: Integer;
  T: array [0..11] of LongWord;
begin
  if FMac = '' then begin
    FState.Z[0] := FState.Z[0] xor $912d94f1;
    for I := 0 to 11 do
      T[I] := FState.LP;
    EncryptBlocks(FState,T,12);
    for I := 8 to 11 do
      T[I] := T[I] xor FState.LP;
    SetLength(FMac,16);
    Move(T[8],Pointer(FMac)^,16);
    FState.I := 0;
    FState.LP := $FF;
  end;
  if 16 > Count then
    Move(Pointer(FMac)^,Mac,Count)
  else
    Move(Pointer(FMac)^,Mac,16);
end;

function THelix.GetVector: OctetString;
begin
  SetLength(Result,16);
  Move(FState,Pointer(Result)^,16);
end;

procedure THelix.GetVectorBuf(var IV; Count: Integer);
begin
  if 16 > Count then
    Move(FState,IV,Count)
  else
    Move(FState,IV,16);
end;

class function THelix.MacLen: Integer;
begin
  Result := 16;
end;

class function THelix.Mode: TCipherMode;
begin
  Result := cmPCFB;
end;

procedure THelix.SetUp(const AKey; Count, VectorSize: Integer);
begin
  CleanUp;
  if Count > 32 then Count := 32;
  FState.LU := Count;
  Move(AKey,FState.K,Count);
  F8(FState.K,Count);
end;

procedure THelix.SetVector(const Value: OctetString);
var
  M: array [0..7] of LongWord;
begin
  if Length(Value) = 16 then begin
    Move(Pointer(Value)^,FState.N,16);
    FState.N[4] := 0-FState.N[0];
    FState.N[5] := 1-FState.N[1];
    FState.N[6] := 2-FState.N[2];
    FState.N[7] := 3-FState.N[3];
    FState.Z[0] := FState.K[3] xor FState.N[0];
    FState.Z[1] := FState.K[4] xor FState.N[1];
    FState.Z[2] := FState.K[5] xor FState.N[2];
    FState.Z[3] := FState.K[6] xor FState.N[3];
    FState.Z[4] := FState.K[7];
    FState.I := 0;
    FillChar(M,SizeOf(M),0);
    EncryptBlocks(FState,M,8);
    FState.LP := 0;
  end;
end;

procedure THelix.SetVectorBuf(const IV; Count: Integer);
var
  M: array [0..7] of LongWord;
begin
  if Count = 16 then begin
    Move(IV,FState.N,16);
    FState.N[4] := 0-FState.N[0];
    FState.N[5] := 1-FState.N[1];
    FState.N[6] := 2-FState.N[2];
    FState.N[7] := 3-FState.N[3];
    FState.Z[0] := FState.K[3] xor FState.N[0];
    FState.Z[1] := FState.K[4] xor FState.N[1];
    FState.Z[2] := FState.K[5] xor FState.N[2];
    FState.Z[3] := FState.K[6] xor FState.N[3];
    FState.Z[4] := FState.K[7];
    FState.I := 0;
    FillChar(M,SizeOf(M),0);
    EncryptBlocks(FState,M,8);
    FState.LP := 0;
  end;
end;

class procedure THelix.Test;
const
  Nonce1 = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
  WorkingKey1: array [0..31] of Byte =
    ($a9,$3b,$6e,$32,$bc,$23,$4f,$6c,$32,$6c,$0f,$82,$74,$ff,$a2,$41,
     $e3,$da,$57,$7d,$ef,$7c,$1b,$64,$af,$78,$7c,$38,$dc,$ef,$e3,$de);
  Plaintext1 = #0#0#0#0#0#0#0#0#0#0;
  Ciphertext1 = #$70#$44#$c9#$be#$48#$ae#$89#$22#$66#$e4;
  MAC1 = #$65#$be#$7a#$60#$fd#$3b#$8a#$5e#$31#$61#$80#$80#$56#$32#$d8#$10;

  InitialKey2: array [0..31] of Byte =
    ($00,$00,$00,$00,$01,$00,$00,$00,$02,$00,$00,$00,$03,$00,$00,$00,
     $04,$00,$00,$00,$05,$00,$00,$00,$06,$00,$00,$00,$07,$00,$00,$00);
  Nonce2 = #$00#$00#$00#$00#$01#$00#$00#$00#$02#$00#$00#$00#$03#$00#$00#$00;
  WorkingKey2: array [0..31] of Byte =
    ($6e,$e9,$a7,$6c,$bd,$0b,$f6,$20,$a6,$d9,$b7,$59,$49,$d3,$39,$95,
     $04,$f8,$4a,$d6,$83,$12,$f9,$06,$ed,$d1,$a6,$98,$9e,$c8,$9d,$45);
  Plaintext2 = #$00#$00#$00#$00#$01#$00#$00#$00#$02#$00#$00#$00#$03#$00#$00#$00 +
               #$04#$00#$00#$00#$05#$00#$00#$00#$06#$00#$00#$00#$07#$00#$00#$00;
  Ciphertext2 = #$7a#$72#$a7#$5b#$62#$50#$38#$0b#$69#$75#$1c#$d1#$28#$30#$8d#$9a +
                #$0c#$74#$46#$a3#$bf#$3f#$99#$e6#$65#$56#$b9#$c1#$18#$ca#$7d#$87;
  MAC2 = #$e4#$e5#$49#$01#$c5#$0b#$34#$e7#$80#$c0#$9c#$39#$b1#$09#$a1#$17;

  InitialKey3: array [0..4] of Byte = ($48,$65,$6c,$69,$78);
  Nonce3 = #$30#$31#$32#$33#$34#$35#$36#$37#$38#$39#$61#$62#$63#$64#$65#$66;
  WorkingKey3: array [0..31] of Byte =
    ($6c,$1e,$d7,$7a,$cb,$a3,$a1,$d2,$8f,$1c,$d6,$20,$6d,$f1,$15,$da,
     $f4,$03,$28,$4a,$73,$9b,$b6,$9f,$35,$7a,$85,$f5,$51,$32,$11,$39);
  Plaintext3 = #$48#$65#$6c#$6c#$6f#$2c#$20#$77#$6f#$72#$6c#$64#$21;
  Ciphertext3 = #$6c#$4c#$27#$b9#$7a#$82#$a0#$c5#$80#$2c#$23#$f2#$0d;
  MAC3 = #$6c#$82#$d1#$aa#$3b#$90#$5f#$12#$f1#$44#$3f#$a7#$f6#$a1#$01#$d2;
var
  C: THelix;
  CT, cCT, Mac, cMac: string;
begin
  C := THelix.Create(nil^,0,0);
  try
    if not CompareMem(@C.FState.K,@WorkingKey1,32) then
      raise Exception.Create('Mismatch in Working Key #1');
    C.IVector := Nonce1;
    CT := PlainText1;
    UniqueString(CT);
    C.Encrypt(Pointer(CT)^,Length(CT));
    cCT := CipherText1;
    if CompareStr(CT,cCT) <> 0 then
      raise Exception.Create(Format('Mismatch in Cipher Text #1'#13#10'%s'#13#10'%s',
                                    [BinToHex(Pointer(CT)^,Length(CT)),
                                     BinToHex(Pointer(cCT)^,Length(cCT))]));
    Mac := C.Mac;
    cMac := MAC1;
    if CompareStr(Mac,cMac) <> 0 then
      raise Exception.Create(Format('Mismatch in Mac #1'#13#10'%s'#13#10'%s',
                                    [BinToHex(Pointer(Mac)^,Length(Mac)),
                                     BinToHex(Pointer(cMac)^,Length(cMac))]));
  finally
    C.Free;
  end;

  C := THelix.Create(InitialKey2,SizeOf(InitialKey2),0);
  try
    if not CompareMem(@C.FState.K,@WorkingKey2,32) then
      raise Exception.Create('Mismatch in Working Key #2');
    C.IVector := Nonce2;
    CT := PlainText2;
    UniqueString(CT);
    C.Encrypt(Pointer(CT)^,Length(CT));
    cCT := CipherText2;
    if CompareStr(CT,cCT) <> 0 then
      raise Exception.Create(Format('Mismatch in Cipher Text #2'#13#10'%s'#13#10'%s',
                                    [BinToHex(Pointer(CT)^,Length(CT)),
                                     BinToHex(Pointer(cCT)^,Length(cCT))]));
    Mac := C.Mac;
    cMac := MAC2;
    if CompareStr(Mac,cMac) <> 0 then
      raise Exception.Create(Format('Mismatch in Mac #2'#13#10'%s'#13#10'%s',
                                    [BinToHex(Pointer(Mac)^,Length(Mac)),
                                     BinToHex(Pointer(cMac)^,Length(cMac))]));
  finally
    C.Free;
  end;

  C := THelix.Create(InitialKey3,SizeOf(InitialKey3),0);
  try
    if not CompareMem(@C.FState.K,@WorkingKey3,32) then
      raise Exception.Create('Mismatch in Working Key #3');
    C.IVector := Nonce3;
    CT := PlainText3;
    UniqueString(CT);
    C.Encrypt(Pointer(CT)^,Length(CT));
    cCT := CipherText3;
    if CompareStr(CT,cCT) <> 0 then
      raise Exception.Create(Format('Mismatch in Cipher Text #3'#13#10'%s'#13#10'%s',
                                    [BinToHex(Pointer(CT)^,Length(CT)),
                                     BinToHex(Pointer(cCT)^,Length(cCT))]));
    Mac := C.Mac;
    cMac := MAC3;
    if CompareStr(Mac,cMac) <> 0 then
      raise Exception.Create(Format('Mismatch in Mac #3'#13#10'%s'#13#10'%s',
                                    [BinToHex(Pointer(Mac)^,Length(Mac)),
                                     BinToHex(Pointer(cMac)^,Length(cMac))]));
  finally
    C.Free;
  end;
end;

{$IFDEF HELIX}
initialization
  RegisterCipherClass(THelix);
{$ENDIF}
end.
