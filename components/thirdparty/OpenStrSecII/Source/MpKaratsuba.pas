{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPKaratsuba Unit                                  }
{     Implementation of Karatsuba multiplication        }
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
unit MpKaratsuba;

interface

uses
  MpArithTypes;

function MPKaratsubaMul2(A, B: PMPInteger; var C: PMPInteger): Boolean;
function MPKaratsubaSqr2(A: PMPInteger; var R: PMPInteger): Boolean;
                                                             
procedure MPKaratsubaExpMod(G, E, M: PMPInteger; var R: PMPInteger);
procedure MPKaratsubaPow2Mod(E, M: PMPInteger; var R: PMPInteger);
procedure MPKaratsubaPowBMod(Base: Cardinal; E, M: PMPInteger; var R: PMPInteger);

implementation

uses
  MpArith;

procedure RawMul4(A, B, C: LongWord); stdcall;
var
  SumA: array [0..2] of LongWord;
  SumB: array [0..2] of LongWord;
  P2: array [0..4] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EBX,B
  mov EDI,C

  push EBP
  mov EBP,EBX
  xor EBX,EBX

  // --- Mul low ---
  mov EAX,dword [ESI]
  mov EDX,dword [EBP]
  mul EDX
  mov dword [EDI],EAX
  mov EBX,EDX

  xor ECX,ECX
  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBP]
  mul EDX
  add EAX,EBX
  adc EDX,0
  adc ECX,0
  mov dword [EDI + 4],EAX
  mov EBX,EDX

  mov EAX,dword [ESI]
  mov EDX,dword [EBP + 4]
  mul EDX
  add dword [EDI + 4],EAX
  adc EBX,EDX
  adc ECX,0

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBP + 4]
  mul EDX
  add EAX,EBX
  adc EDX,ECX
  mov dword [EDI + 8],EAX
  mov dword [EDI + 12],EDX

  // --- Mul high ---
  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBP + 8]
  mul EDX
  mov dword [EDI + 16],EAX
  mov EBX,EDX

  xor ECX,ECX
  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 8]
  mul EDX
  add EAX,EBX
  adc EDX,0
  adc ECX,0
  mov dword [EDI + 20],EAX
  mov EBX,EDX

  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBP + 12]
  mul EDX
  add dword [EDI + 20],EAX
  adc EBX,EDX
  adc ECX,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EAX,EBX
  adc EDX,ECX
  mov dword [EDI + 24],EAX
  mov dword [EDI + 28],EDX

  mov EBX,EBP
  pop EBP

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 8]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 12]
  mov dword [EDX + 4],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 8],EAX

  // --- Add B ---
  lea EDX,SumB
  mov EAX,dword [EBX]
  add EAX,dword [EBX + 8]
  mov dword [EDX],EAX
  mov EAX,dword [EBX + 4]
  adc EAX,dword [EBX + 12]
  mov dword [EDX + 4],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 8],EAX

  // --- Mul middle ---
  push EBP    
  push EDI
  lea ESI,SumA
  lea EDI,P2
  lea EBP,SumB

  xor EBX,EBX
  xor ECX,ECX
  mov EAX,dword [ESI]
  mov EDX,dword [EBP]
  mul EDX
  mov dword [EDI],EAX
  mov EBX,EDX

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBP]
  mul EDX
  add EAX,EBX
  adc EDX,0
  setc CL
  mov dword [EDI + 4],EAX
  mov EBX,EDX

  mov EAX,dword [ESI]
  mov EDX,dword [EBP + 4]
  mul EDX
  add dword [EDI + 4],EAX
  adc EBX,EDX
  adc ECX,0

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBP + 4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  xor EDX,EDX

  mov EAX,dword [ESI + 8]
  test EAX,EAX
  jz @@1
  add EBX,dword [EBP]
  adc ECX,dword [EBP + 4]
  adc EDX,0
@@1:
  mov EAX,dword [EBP + 8]
  test EAX,EAX
  jz @@2
  add EBX,dword [ESI]
  adc ECX,dword [ESI + 4]
  adc EDX,0
@@2:
  and EAX,dword [ESI + 8]
  add EDX,EAX

  mov dword [EDI + 8],EBX
  mov dword [EDI + 12],ECX
  mov dword [EDI + 16],EDX

  mov ECX,EDI
  pop EDI
  mov EBX,EBP
  pop EBP

  // --- Sub middle ---
  xor EDX,EDX
  mov EAX,dword [ECX]
  sub EAX,dword [EDI]
  sbb EDX,0
  sub EAX,dword [EDI + 16]
  sbb EDX,0
  mov dword [ECX],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 4]
  adc EDX,0
  sub EAX,dword [EDI + 4]
  sbb EDX,0
  sub EAX,dword [EDI + 20]
  sbb EDX,0
  mov dword [ECX + 4],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 8]
  adc EDX,0
  sub EAX,dword [EDI + 8]
  sbb EDX,0
  sub EAX,dword [EDI + 24]
  sbb EDX,0
  mov dword [ECX + 8],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 12]
  adc EDX,dword [ECX + 16]
  sub EAX,dword [EDI + 12]
  sbb EDX,0
  sub EAX,dword [EDI + 28]
  sbb EDX,0
  mov dword [ECX + 12],EAX
  mov dword [ECX + 16],EDX

  // --- Add middle ---
  mov EAX,dword [ECX]
  add dword [EDI + 8],EAX
  mov EAX,dword [ECX + 4]
  adc dword [EDI + 12],EAX
  mov EAX,dword [ECX + 8]
  adc dword [EDI + 16],EAX
  mov EAX,dword [ECX + 12]
  adc dword [EDI + 20],EAX
  mov EAX,dword [ECX + 16]
  adc dword [EDI + 24],EAX
  adc dword [EDI + 28],0

  mov ECX,11
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawSqr4(A, R: LongWord); stdcall;
var
  SumA: array [0..2] of LongWord;
  P2: array [0..4] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDI,R

  // --- Sqr low ---
  mov EAX,dword [ESI]
  mul EAX
  mov dword [EDI],EAX
  mov EBX,EDX

  xor ECX,ECX
  mov EAX,dword [ESI]
  mov EDX,dword [ESI + 4]
  mul EDX
  add EAX,EAX
  adc EDX,EDX
  adc ECX,0
  add EAX,EBX
  adc EDX,0
  adc ECX,0
  mov dword [EDI + 4],EAX
  mov EBX,EDX

  mov EAX,dword [ESI + 4]
  mul EAX
  add EAX,EBX
  adc EDX,ECX
  mov dword [EDI + 8],EAX
  mov dword [EDI + 12],EDX

  // --- Sqr high ---
  mov EAX,dword [ESI + 8]
  mul EAX
  mov dword [EDI + 16],EAX
  mov EBX,EDX

  xor ECX,ECX
  mov EAX,dword [ESI + 12]
  mov EDX,dword [ESI + 8]
  mul EDX
  add EAX,EAX
  adc EDX,EDX
  adc ECX,0
  add EAX,EBX
  adc EDX,0
  adc ECX,0
  mov dword [EDI + 20],EAX
  mov EBX,EDX

  mov EAX,dword [ESI + 12]
  mul EAX
  add EAX,EBX
  adc EDX,ECX
  mov dword [EDI + 24],EAX
  mov dword [EDI + 28],EDX

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 8]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 12]
  mov dword [EDX + 4],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 8],EAX

  // --- Sqr middle ---
  push EBP
  lea ESI,SumA
  lea EBP,P2

  mov EAX,dword [ESI]
  mul EAX
  mov dword [EBP],EAX
  mov EBX,EDX

  xor ECX,ECX
  mov EAX,dword [ESI + 4]
  mov EDX,dword [ESI]
  mul EDX
  add EAX,EAX
  adc EDX,EDX
  adc ECX,0
  add EAX,EBX
  adc EDX,0
  adc ECX,0
  mov dword [EBP + 4],EAX
  mov EBX,EDX

  mov EAX,dword [ESI + 4]
  mul EAX
  add EBX,EAX
  adc ECX,EDX

  xor EDX,EDX
  mov dword [EBP + 16],0
  mov EAX,dword [ESI + 8]
  test EAX,EAX
  jz @@1
  mov EAX,dword [ESI]
  mov ESI,dword [ESI + 4]
  add EAX,EAX
  adc ESI,ESI
  adc EDX,1
  add EBX,EAX
  adc ECX,ESI
  adc EDX,0
@@1:
  mov dword [EBP + 8],EBX
  mov dword [EBP + 12],ECX
  mov dword [EBP + 16],EDX
  mov EBX,EBP
  pop EBP

  // --- Sub middle ---
  xor EDX,EDX
  mov EAX,dword [EBX]
  sub EAX,dword [EDI]
  sbb EDX,0
  sub EAX,dword [EDI + 16]
  sbb EDX,0
  mov dword [EBX],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 4]
  adc EDX,0
  sub EAX,dword [EDI + 4]
  sbb EDX,0
  sub EAX,dword [EDI + 20]
  sbb EDX,0
  mov dword [EBX + 4],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 8]
  adc EDX,0
  sub EAX,dword [EDI + 8]
  sbb EDX,0
  sub EAX,dword [EDI + 24]
  sbb EDX,0
  mov dword [EBX + 8],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 12]
  adc EDX,dword [EBX + 16]
  sub EAX,dword [EDI + 12]
  sbb EDX,0
  sub EAX,dword [EDI + 28]
  sbb EDX,0
  mov dword [EBX + 12],EAX
  mov dword [EBX + 16],EDX

  // --- Add middle ---
  mov EAX,dword [EBX]
  add dword [EDI + 8],EAX
  mov EAX,dword [EBX + 4]
  adc dword [EDI + 12],EAX
  mov EAX,dword [EBX + 8]
  adc dword [EDI + 16],EAX
  mov EAX,dword [EBX + 12]
  adc dword [EDI + 20],EAX
  mov EAX,dword [EBX + 16]
  adc dword [EDI + 24],EAX
  adc dword [EDI + 28],0

  mov ECX,8
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;         

procedure RawMul6(A, B, C: LongWord); stdcall;
var
  SumA: array [0..3] of LongWord;
  SumB: array [0..3] of LongWord;
  P2: array [0..6] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EBX,B
  mov EDI,C

  // --- Mul low ---
  mov EAX,dword [ESI]
  mov EDX,dword [EBX]
  mul EDX
  mov dword [EDI],EAX
  mov dword [EDI + 4],EDX

  mov dword [EDI + 8],0
  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBX]
  mul EDX
  add dword [EDI + 4],EAX
  adc dword [EDI + 8],EDX

  xor ECX,ECX
  mov EAX,dword [ESI]
  mov EDX,dword [EBX + 4]
  mul EDX
  add dword [EDI + 4],EAX
  adc dword [EDI + 8],EDX
  adc ECX,0
  mov dword [EDI + 12],ECX

  xor ECX,ECX
  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBX]
  mul EDX
  add dword [EDI + 8],EAX
  adc dword [EDI + 12],EDX
  adc ECX,0

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBX + 4]
  mul EDX
  add dword [EDI + 8],EAX
  adc dword [EDI + 12],EDX
  adc ECX,0

  mov EAX,dword [ESI]
  mov EDX,dword [EBX + 8]
  mul EDX
  add dword [EDI + 8],EAX
  adc dword [EDI + 12],EDX
  adc ECX,0
  mov dword [EDI + 16],ECX

  xor ECX,ECX
  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBX + 4]
  mul EDX
  add dword [EDI + 12],EAX
  adc dword [EDI + 16],EDX
  adc ECX,0

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBX + 8]
  mul EDX
  add dword [EDI + 12],EAX
  adc dword [EDI + 16],EDX
  adc ECX,0
  mov dword [EDI + 20],ECX

  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBX + 8]
  mul EDX
  add dword [EDI + 16],EAX
  adc dword [EDI + 20],EDX

  // --- Mul high ---
  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBX + 12]
  mul EDX
  mov dword [EDI + 24],EAX
  mov dword [EDI + 28],EDX

  mov dword [EDI + 32],0
  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBX + 12]
  mul EDX
  add dword [EDI + 28],EAX
  adc dword [EDI + 32],EDX

  xor ECX,ECX
  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBX + 16]
  mul EDX
  add dword [EDI + 28],EAX
  adc dword [EDI + 32],EDX
  adc ECX,0
  mov dword [EDI + 36],ECX

  xor ECX,ECX
  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBX + 12]
  mul EDX
  add dword [EDI + 32],EAX
  adc dword [EDI + 36],EDX
  adc ECX,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBX + 16]
  mul EDX
  add dword [EDI + 32],EAX
  adc dword [EDI + 36],EDX
  adc ECX,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBX + 20]
  mul EDX
  add dword [EDI + 32],EAX
  adc dword [EDI + 36],EDX
  adc ECX,0
  mov dword [EDI + 40],ECX

  xor ECX,ECX
  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBX + 16]
  mul EDX
  add dword [EDI + 36],EAX
  adc dword [EDI + 40],EDX
  adc ECX,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBX + 20]
  mul EDX
  add dword [EDI + 36],EAX
  adc dword [EDI + 40],EDX
  adc ECX,0
  mov dword [EDI + 44],ECX

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBX + 20]
  mul EDX
  add dword [EDI + 40],EAX
  adc dword [EDI + 44],EDX

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 12]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 16]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ESI + 8]
  adc EAX,dword [ESI + 20]
  mov dword [EDX + 8],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 12],EAX

  // --- Add B ---
  lea EDX,SumB
  mov EAX,dword [EBX]
  add EAX,dword [EBX + 12]
  mov dword [EDX],EAX
  mov EAX,dword [EBX + 4]
  adc EAX,dword [EBX + 16]
  mov dword [EDX + 4],EAX
  mov EAX,dword [EBX + 8]
  adc EAX,dword [EBX + 20]
  mov dword [EDX + 8],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 12],EAX

  // --- Mul middle ---
  lea ESI,SumA
  lea EBX,SumB
  lea EDI,P2

  mov EAX,dword [ESI]
  mov EDX,dword [EBX]
  mul EDX
  mov dword [EDI],EAX
  mov dword [EDI + 4],EDX

  mov dword [EDI + 8],0
  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBX]
  mul EDX
  add dword [EDI + 4],EAX
  adc dword [EDI + 8],EDX

  xor ECX,ECX
  mov EAX,dword [ESI]
  mov EDX,dword [EBX + 4]
  mul EDX
  add dword [EDI + 4],EAX
  adc dword [EDI + 8],EDX
  adc ECX,0
  mov dword [EDI + 12],ECX

  xor ECX,ECX
  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBX]
  mul EDX
  add dword [EDI + 8],EAX
  adc dword [EDI + 12],EDX
  adc ECX,0

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBX + 4]
  mul EDX
  add dword [EDI + 8],EAX
  adc dword [EDI + 12],EDX
  adc ECX,0

  mov EAX,dword [ESI]
  mov EDX,dword [EBX + 8]
  mul EDX
  add dword [EDI + 8],EAX
  adc dword [EDI + 12],EDX
  adc ECX,0
  mov dword [EDI + 16],ECX

  xor ECX,ECX
  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBX + 4]
  mul EDX
  add dword [EDI + 12],EAX
  adc dword [EDI + 16],EDX
  adc ECX,0

  mov EAX,dword [ESI + 4]
  mov EDX,dword [EBX + 8]
  mul EDX
  add dword [EDI + 12],EAX
  adc dword [EDI + 16],EDX
  adc ECX,0
  mov dword [EDI + 20],ECX

  mov EAX,dword [ESI + 8]
  mov EDX,dword [EBX + 8]
  mul EDX
  add dword [EDI + 16],EAX
  adc dword [EDI + 20],EDX

  lea ECX,P2
  mov dword [ECX + 24],0
  mov EAX,dword [ESI + 12]
  test EAX,EAX
  jz @@1
  mov EAX,dword [EBX]
  add dword [ECX + 12],EAX
  mov EAX,dword [EBX + 4]
  adc dword [ECX + 16],EAX
  mov EAX,dword [EBX + 8]
  adc dword [ECX + 20],EAX
  adc dword [ECX + 24],0
@@1:
  mov EDX,dword [EBX + 12]
  test EDX,EDX
  jz @@2
  mov EAX,dword [ESI]
  add dword [ECX + 12],EAX
  mov EAX,dword [ESI + 4]
  adc dword [ECX + 16],EAX
  mov EAX,dword [ESI + 8]
  adc dword [ECX + 20],EAX
  adc dword [ECX + 24],0
@@2:
  mov EAX,dword [EBX + 12]
  and EAX,dword [ESI + 12]
  add dword [ECX + 24],EAX

  // --- Sub middle ---
  mov EDI,C
  xor EDX,EDX
  mov EAX,dword [ECX]
  sub EAX,dword [EDI]
  sbb EDX,0
  sub EAX,dword [EDI + 24]
  sbb EDX,0
  mov dword [ECX],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 4]
  adc EDX,0
  sub EAX,dword [EDI + 4]
  sbb EDX,0
  sub EAX,dword [EDI + 28]
  sbb EDX,0
  mov dword [ECX + 4],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 8]
  adc EDX,0
  sub EAX,dword [EDI + 8]
  sbb EDX,0
  sub EAX,dword [EDI + 32]
  sbb EDX,0
  mov dword [ECX + 8],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 12]
  adc EDX,0
  sub EAX,dword [EDI + 12]
  sbb EDX,0
  sub EAX,dword [EDI + 36]
  sbb EDX,0
  mov dword [ECX + 12],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 16]
  adc EDX,0
  sub EAX,dword [EDI + 16]
  sbb EDX,0
  sub EAX,dword [EDI + 40]
  sbb EDX,0
  mov dword [ECX + 16],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 20]
  adc EDX,dword [ECX + 24]
  sub EAX,dword [EDI + 20]
  sbb EDX,0
  sub EAX,dword [EDI + 44]
  sbb EDX,0
  mov dword [ECX + 20],EAX
  mov dword [ECX + 24],EDX

  // --- Add middle ---
  mov EAX,dword [ECX]
  add dword [EDI + 12],EAX
  mov EAX,dword [ECX + 4]
  adc dword [EDI + 16],EAX
  mov EAX,dword [ECX + 8]
  adc dword [EDI + 20],EAX
  mov EAX,dword [ECX + 12]
  adc dword [EDI + 24],EAX
  mov EAX,dword [ECX + 16]
  adc dword [EDI + 28],EAX
  mov EAX,dword [ECX + 20]
  adc dword [EDI + 32],EAX
  mov EAX,dword [ECX + 24]
  adc dword [EDI + 36],EAX
  adc dword [EDI + 40],0
  adc dword [EDI + 44],0

  mov ECX,15
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMul8(A, B, C: LongWord); stdcall;
var
  SumA, SumB: array [0..4] of LongWord;
  P2: array [0..8] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDX,B
  mov EDI,C

  // --- Mul low ---
  push EDI
  push EDX
  push ESI
  call RawMul4

  // --- Mul high ---
  lea EAX,[EDI + 32]
  push EAX
  mov EDX,B
  lea EAX,[EDX + 16]
  push EAX
  lea EAX,[ESI + 16]
  push EAX
  call RawMul4

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 16]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 20]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ESI + 8]
  adc EAX,dword [ESI + 24]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ESI + 12]
  adc EAX,dword [ESI + 28]
  mov dword [EDX + 12],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 16],EAX

  // --- Add B ---
  mov ECX,B
  lea EDX,SumB
  mov EAX,dword [ECX]
  add EAX,dword [ECX + 16]
  mov dword [EDX],EAX
  mov EAX,dword [ECX + 4]
  adc EAX,dword [ECX + 20]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ECX + 8]
  adc EAX,dword [ECX + 24]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ECX + 12]
  adc EAX,dword [ECX + 28]
  mov dword [EDX + 12],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 16],EAX

  // --- Mul middle ---
  lea ESI,SumA
  lea EDX,SumB
  lea ECX,P2

  push ECX
  push EDX
  push ESI
  call RawMul4

  lea ESI,SumA
  lea EBX,SumB
  lea ECX,P2

  mov dword [ECX + 32],0
  mov EAX,dword [ESI + 16]
  test EAX,EAX
  jz @@1
  mov EAX,dword [EBX]
  add dword [ECX + 16],EAX
  mov EAX,dword [EBX + 4]
  adc dword [ECX + 20],EAX
  mov EAX,dword [EBX + 8]
  adc dword [ECX + 24],EAX
  mov EAX,dword [EBX + 12]
  adc dword [ECX + 28],EAX
  adc dword [ECX + 32],0
@@1:
  mov EDX,dword [EBX + 16]
  test EDX,EDX
  jz @@2
  mov EAX,dword [ESI]
  add dword [ECX + 16],EAX
  mov EAX,dword [ESI + 4]
  adc dword [ECX + 20],EAX
  mov EAX,dword [ESI + 8]
  adc dword [ECX + 24],EAX
  mov EAX,dword [ESI + 12]
  adc dword [ECX + 28],EAX
  adc dword [ECX + 32],0
@@2:
  mov EAX,dword [EBX + 16]
  and EAX,dword [ESI + 16]
  add dword [ECX + 32],EAX

  // --- Sub middle ---
  xor EDX,EDX
  mov EAX,dword [ECX]
  sub EAX,dword [EDI]
  sbb EDX,0
  sub EAX,dword [EDI + 32]
  sbb EDX,0
  mov dword [ECX],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 4]
  adc EDX,0
  sub EAX,dword [EDI + 4]
  sbb EDX,0
  sub EAX,dword [EDI + 36]
  sbb EDX,0
  mov dword [ECX + 4],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 8]
  adc EDX,0
  sub EAX,dword [EDI + 8]
  sbb EDX,0
  sub EAX,dword [EDI + 40]
  sbb EDX,0
  mov dword [ECX + 8],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 12]
  adc EDX,0
  sub EAX,dword [EDI + 12]
  sbb EDX,0
  sub EAX,dword [EDI + 44]
  sbb EDX,0
  mov dword [ECX + 12],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 16]
  adc EDX,0
  sub EAX,dword [EDI + 16]
  sbb EDX,0
  sub EAX,dword [EDI + 48]
  sbb EDX,0
  mov dword [ECX + 16],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 20]
  adc EDX,0
  sub EAX,dword [EDI + 20]
  sbb EDX,0
  sub EAX,dword [EDI + 52]
  sbb EDX,0
  mov dword [ECX + 20],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 24]
  adc EDX,0
  sub EAX,dword [EDI + 24]
  sbb EDX,0
  sub EAX,dword [EDI + 56]
  sbb EDX,0
  mov dword [ECX + 24],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [ECX + 28]
  adc EDX,dword [ECX + 32]
  sub EAX,dword [EDI + 28]
  sbb EDX,0
  sub EAX,dword [EDI + 60]
  sbb EDX,0
  mov dword [ECX + 28],EAX
  mov dword [ECX + 32],EDX

  // --- Add middle ---
  mov EAX,dword [ECX]
  add dword [EDI + 16],EAX
  mov EAX,dword [ECX + 4]
  adc dword [EDI + 20],EAX
  mov EAX,dword [ECX + 8]
  adc dword [EDI + 24],EAX
  mov EAX,dword [ECX + 12]
  adc dword [EDI + 28],EAX
  mov EAX,dword [ECX + 16]
  adc dword [EDI + 32],EAX
  mov EAX,dword [ECX + 20]
  adc dword [EDI + 36],EAX
  mov EAX,dword [ECX + 24]
  adc dword [EDI + 40],EAX
  mov EAX,dword [ECX + 28]
  adc dword [EDI + 44],EAX
  mov EAX,dword [ECX + 32]
  adc dword [EDI + 48],EAX
  adc dword [EDI + 52],0
  adc dword [EDI + 56],0
  adc dword [EDI + 60],0

  mov ECX,19
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawSqr8(A, R: LongWord); stdcall;
var
  SumA: array [0..4] of LongWord;
  P2: array [0..8] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDI,R

  // --- Sqr low ---
  push EDI
  push ESI
  call RawSqr4

  // --- Sqr high ---
  lea EAX,[EDI + 32]
  push EAX
  lea EAX,[ESI + 16]
  push EAX
  call RawSqr4

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 16]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 20]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ESI + 8]
  adc EAX,dword [ESI + 24]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ESI + 12]
  adc EAX,dword [ESI + 28]
  mov dword [EDX + 12],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 16],EAX

  // --- Sqr middle ---
  lea ESI,SumA
  lea ECX,P2

  push ECX
  push ESI
  call RawSqr4

  lea ESI,SumA
  lea EBX,P2

  mov dword [EBX + 32],0
  mov EAX,dword [ESI + 16]
  test EAX,EAX
  jz @@1
  mov EAX,dword [ESI]
  add dword [ESI],EAX
  mov EAX,dword [ESI + 4]
  adc dword [ESI + 4],EAX
  mov EAX,dword [ESI + 8]
  adc dword [ESI + 8],EAX
  mov EAX,dword [ESI + 12]
  adc dword [ESI + 12],EAX
  adc dword [ESI + 16],0

  mov EAX,dword [ESI]
  add dword [EBX + 16],EAX
  mov EAX,dword [ESI + 4]
  adc dword [EBX + 20],EAX
  mov EAX,dword [ESI + 8]
  adc dword [EBX + 24],EAX
  mov EAX,dword [ESI + 12]
  adc dword [EBX + 28],EAX
  mov EAX,dword [ESI + 16]
  adc dword [EBX + 32],EAX
@@1:

  // --- Sub middle ---
  xor EDX,EDX
  mov EAX,dword [EBX]
  sub EAX,dword [EDI]
  sbb EDX,0
  sub EAX,dword [EDI + 32]
  sbb EDX,0
  mov dword [EBX],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 4]
  adc EDX,0
  sub EAX,dword [EDI + 4]
  sbb EDX,0
  sub EAX,dword [EDI + 36]
  sbb EDX,0
  mov dword [EBX + 4],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 8]
  adc EDX,0
  sub EAX,dword [EDI + 8]
  sbb EDX,0
  sub EAX,dword [EDI + 40]
  sbb EDX,0
  mov dword [EBX + 8],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 12]
  adc EDX,0
  sub EAX,dword [EDI + 12]
  sbb EDX,0
  sub EAX,dword [EDI + 44]
  sbb EDX,0
  mov dword [EBX + 12],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 16]
  adc EDX,0
  sub EAX,dword [EDI + 16]
  sbb EDX,0
  sub EAX,dword [EDI + 48]
  sbb EDX,0
  mov dword [EBX + 16],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 20]
  adc EDX,0
  sub EAX,dword [EDI + 20]
  sbb EDX,0
  sub EAX,dword [EDI + 52]
  sbb EDX,0
  mov dword [EBX + 20],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 24]
  adc EDX,0
  sub EAX,dword [EDI + 24]
  sbb EDX,0
  sub EAX,dword [EDI + 56]
  sbb EDX,0
  mov dword [EBX + 24],EAX
  mov EAX,EDX
  movsx EDX,DH
  add EAX,dword [EBX + 28]
  adc EDX,dword [EBX + 32]
  sub EAX,dword [EDI + 28]
  sbb EDX,0
  sub EAX,dword [EDI + 60]
  sbb EDX,0
  mov dword [EBX + 28],EAX
  mov dword [EBX + 32],EDX

  // --- Add middle ---
  mov EAX,dword [EBX]
  add dword [EDI + 16],EAX
  mov EAX,dword [EBX + 4]
  adc dword [EDI + 20],EAX
  mov EAX,dword [EBX + 8]
  adc dword [EDI + 24],EAX
  mov EAX,dword [EBX + 12]
  adc dword [EDI + 28],EAX
  mov EAX,dword [EBX + 16]
  adc dword [EDI + 32],EAX
  mov EAX,dword [EBX + 20]
  adc dword [EDI + 36],EAX
  mov EAX,dword [EBX + 24]
  adc dword [EDI + 40],EAX
  mov EAX,dword [EBX + 28]
  adc dword [EDI + 44],EAX
  mov EAX,dword [EBX + 32]
  adc dword [EDI + 48],EAX
  adc dword [EDI + 52],0
  adc dword [EDI + 56],0
  adc dword [EDI + 60],0

  mov ECX,14
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMul12(A, B, C: LongWord); stdcall;
var
  SumA, SumB: array [0..6] of LongWord;
  P2: array [0..12] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDX,B
  mov EDI,C

  // --- Mul low ---
  push EDI
  push EDX
  push ESI
  call RawMul6

  // --- Mul high ---
  lea EAX,[EDI + 48]
  push EAX          
  mov EDX,B
  lea EAX,[EDX + 24]
  push EAX
  lea EAX,[ESI + 24]
  push EAX
  call RawMul6

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 24]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 28]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ESI + 8]
  adc EAX,dword [ESI + 32]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ESI + 12]
  adc EAX,dword [ESI + 36]
  mov dword [EDX + 12],EAX
  mov EAX,dword [ESI + 16]
  adc EAX,dword [ESI + 40]
  mov dword [EDX + 16],EAX
  mov EAX,dword [ESI + 20]
  adc EAX,dword [ESI + 44]
  mov dword [EDX + 20],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 24],EAX

  // --- Add B ---
  lea EDX,SumB
  mov ECX,B
  mov EAX,dword [ECX]
  add EAX,dword [ECX + 24]
  mov dword [EDX],EAX
  mov EAX,dword [ECX + 4]
  adc EAX,dword [ECX + 28]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ECX + 8]
  adc EAX,dword [ECX + 32]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ECX + 12]
  adc EAX,dword [ECX + 36]
  mov dword [EDX + 12],EAX
  mov EAX,dword [ECX + 16]
  adc EAX,dword [ECX + 40]
  mov dword [EDX + 16],EAX
  mov EAX,dword [ECX + 20]
  adc EAX,dword [ECX + 44]
  mov dword [EDX + 20],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 24],EAX

  // --- Mul middle ---
  lea ESI,SumA
  lea EDX,SumB
  lea ECX,P2

  push ECX
  push EDX
  push ESI
  call RawMul6

  lea ESI,SumA
  lea EBX,SumB
  lea ECX,P2

  mov dword [ECX + 48],0
  mov EAX,dword [ESI + 24]
  test EAX,EAX
  jz @@1
  mov EAX,dword [EBX]
  add dword [ECX + 24],EAX
  mov EAX,dword [EBX + 4]
  adc dword [ECX + 28],EAX
  mov EAX,dword [EBX + 8]
  adc dword [ECX + 32],EAX
  mov EAX,dword [EBX + 12]
  adc dword [ECX + 36],EAX
  mov EAX,dword [EBX + 16]
  adc dword [ECX + 40],EAX
  mov EAX,dword [EBX + 20]
  adc dword [ECX + 44],EAX
  adc dword [ECX + 48],0
@@1:
  mov EDX,dword [EBX + 24]
  test EDX,EDX
  jz @@2
  mov EAX,dword [ESI]
  add dword [ECX + 24],EAX
  mov EAX,dword [ESI + 4]
  adc dword [ECX + 28],EAX
  mov EAX,dword [ESI + 8]
  adc dword [ECX + 32],EAX
  mov EAX,dword [ESI + 12]
  adc dword [ECX + 36],EAX
  mov EAX,dword [ESI + 16]
  adc dword [ECX + 40],EAX
  mov EAX,dword [ESI + 20]
  adc dword [ECX + 44],EAX
  adc dword [ECX + 48],0
@@2:
  mov EAX,dword [EBX + 24]
  and EAX,dword [ESI + 24]
  add dword [ECX + 48],EAX

  // --- Sub middle ---
  xor EDX,EDX
  mov EBX,ECX
  xor ECX,ECX
  xor EAX,EAX
  mov EDI,C
@@SubMiddle:
  add EAX,dword [EBX + ECX*4]
  adc EDX,0
  sub EAX,dword [EDI + ECX*4]
  sbb EDX,0
  sub EAX,dword [EDI + ECX*4 + 48]
  sbb EDX,0
  mov dword [EBX + ECX*4],EAX
  mov EAX,EDX
  movsx EDX,DH
  inc ECX
  cmp ECX,12
  jb @@SubMiddle
  add dword [EBX + 48],EAX

  // --- Add middle ---
  xor ECX,ECX
  clc
@@AddMiddle:
  mov EAX,dword [EBX]
  add dword [EDI + 24],EAX
  mov EAX,dword [EBX + 4]
  adc dword [EDI + 28],EAX
  mov EAX,dword [EBX + 8]
  adc dword [EDI + 32],EAX
  mov EAX,dword [EBX + 12]
  adc dword [EDI + 36],EAX
  mov EAX,dword [EBX + 16]
  adc dword [EDI + 40],EAX
  mov EAX,dword [EBX + 20]
  adc dword [EDI + 44],EAX
  mov EAX,dword [EBX + 24]
  adc dword [EDI + 48],EAX
  mov EAX,dword [EBX + 28]
  adc dword [EDI + 52],EAX
  mov EAX,dword [EBX + 32]
  adc dword [EDI + 56],EAX
  mov EAX,dword [EBX + 36]
  adc dword [EDI + 60],EAX
  mov EAX,dword [EBX + 40]
  adc dword [EDI + 64],EAX
  mov EAX,dword [EBX + 44]
  adc dword [EDI + 68],EAX
  mov EAX,dword [EBX + 48]
  adc dword [EDI + 72],EAX
  adc dword [EDI + 76],0
  adc dword [EDI + 80],0
  adc dword [EDI + 84],0
  adc dword [EDI + 88],0
  adc dword [EDI + 92],0

  mov ECX,27
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMul16(A, B, C: LongWord); stdcall;
var
  SumA, SumB: array [0..8] of LongWord;
  P2: array [0..16] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDX,B
  mov EDI,C

  // --- Mul low ---
  push EDI
  push EDX
  push ESI
  call RawMul8

  // --- Mul high ---
  lea EAX,[EDI + 64]
  push EAX
  mov EDX,B
  lea EAX,[EDX + 32]
  push EAX
  lea EAX,[ESI + 32]
  push EAX
  call RawMul8

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 32]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 36]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ESI + 8]
  adc EAX,dword [ESI + 40]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ESI + 12]
  adc EAX,dword [ESI + 44]
  mov dword [EDX + 12],EAX
  mov EAX,dword [ESI + 16]
  adc EAX,dword [ESI + 48]
  mov dword [EDX + 16],EAX
  mov EAX,dword [ESI + 20]
  adc EAX,dword [ESI + 52]
  mov dword [EDX + 20],EAX
  mov EAX,dword [ESI + 24]
  adc EAX,dword [ESI + 56]
  mov dword [EDX + 24],EAX
  mov EAX,dword [ESI + 28]
  adc EAX,dword [ESI + 60]
  mov dword [EDX + 28],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 32],EAX

  // --- Add B ---
  mov ECX,B
  lea EDX,SumB
  mov EAX,dword [ECX]
  add EAX,dword [ECX + 32]
  mov dword [EDX],EAX
  mov EAX,dword [ECX + 4]
  adc EAX,dword [ECX + 36]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ECX + 8]
  adc EAX,dword [ECX + 40]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ECX + 12]
  adc EAX,dword [ECX + 44]
  mov dword [EDX + 12],EAX
  mov EAX,dword [ECX + 16]
  adc EAX,dword [ECX + 48]
  mov dword [EDX + 16],EAX
  mov EAX,dword [ECX + 20]
  adc EAX,dword [ECX + 52]
  mov dword [EDX + 20],EAX
  mov EAX,dword [ECX + 24]
  adc EAX,dword [ECX + 56]
  mov dword [EDX + 24],EAX
  mov EAX,dword [ECX + 28]
  adc EAX,dword [ECX + 60]
  mov dword [EDX + 28],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 32],EAX

  // --- Mul middle ---
  lea ESI,SumA
  lea EDX,SumB
  lea ECX,P2

  push ECX
  push EDX
  push ESI
  call RawMul8

  lea ESI,SumA
  lea EBX,SumB
  lea ECX,P2

  mov dword [ECX + 64],0
  mov EAX,dword [ESI + 32]
  test EAX,EAX
  jz @@1
  mov EAX,dword [EBX]
  add dword [ECX + 32],EAX
  mov EAX,dword [EBX + 4]
  adc dword [ECX + 36],EAX
  mov EAX,dword [EBX + 8]
  adc dword [ECX + 40],EAX
  mov EAX,dword [EBX + 12]
  adc dword [ECX + 44],EAX
  mov EAX,dword [EBX + 16]
  adc dword [ECX + 48],EAX
  mov EAX,dword [EBX + 20]
  adc dword [ECX + 52],EAX
  mov EAX,dword [EBX + 24]
  adc dword [ECX + 56],EAX
  mov EAX,dword [EBX + 28]
  adc dword [ECX + 60],EAX
  adc dword [ECX + 64],0
@@1:
  mov EDX,dword [EBX + 32]
  test EDX,EDX
  jz @@2
  mov EAX,dword [ESI]
  add dword [ECX + 32],EAX
  mov EAX,dword [ESI + 4]
  adc dword [ECX + 36],EAX
  mov EAX,dword [ESI + 8]
  adc dword [ECX + 40],EAX
  mov EAX,dword [ESI + 12]
  adc dword [ECX + 44],EAX
  mov EAX,dword [ESI + 16]
  adc dword [ECX + 48],EAX
  mov EAX,dword [ESI + 20]
  adc dword [ECX + 52],EAX
  mov EAX,dword [ESI + 24]
  adc dword [ECX + 56],EAX
  mov EAX,dword [ESI + 28]
  adc dword [ECX + 60],EAX
  adc dword [ECX + 64],0
@@2:
  mov EAX,dword [EBX + 32]
  and EAX,dword [ESI + 32]
  add dword [ECX + 64],EAX

  // --- Sub middle ---
  xor EDX,EDX
  mov EBX,ECX
  xor ECX,ECX
  xor EAX,EAX
@@SubMiddle:
  add EAX,dword [EBX + ECX*4]
  adc EDX,0
  sub EAX,dword [EDI + ECX*4]
  sbb EDX,0
  sub EAX,dword [EDI + ECX*4 + 64]
  sbb EDX,0
  mov dword [EBX + ECX*4],EAX
  mov EAX,EDX
  movsx EDX,DH
  inc ECX
  cmp ECX,16
  jb @@SubMiddle
  add dword [EBX + 64],EAX

  // --- Add middle ---
  xor ECX,ECX
  clc
@@AddMiddle:
  mov EAX,dword [EBX]
  add dword [EDI + 32],EAX
  mov EAX,dword [EBX + 4]
  adc dword [EDI + 36],EAX
  mov EAX,dword [EBX + 8]
  adc dword [EDI + 40],EAX
  mov EAX,dword [EBX + 12]
  adc dword [EDI + 44],EAX
  mov EAX,dword [EBX + 16]
  adc dword [EDI + 48],EAX
  mov EAX,dword [EBX + 20]
  adc dword [EDI + 52],EAX
  mov EAX,dword [EBX + 24]
  adc dword [EDI + 56],EAX
  mov EAX,dword [EBX + 28]
  adc dword [EDI + 60],EAX
  mov EAX,dword [EBX + 32]
  adc dword [EDI + 64],EAX
  mov EAX,dword [EBX + 36]
  adc dword [EDI + 68],EAX
  mov EAX,dword [EBX + 40]
  adc dword [EDI + 72],EAX
  mov EAX,dword [EBX + 44]
  adc dword [EDI + 76],EAX
  mov EAX,dword [EBX + 48]
  adc dword [EDI + 80],EAX
  mov EAX,dword [EBX + 52]
  adc dword [EDI + 84],EAX
  mov EAX,dword [EBX + 56]
  adc dword [EDI + 88],EAX
  mov EAX,dword [EBX + 60]
  adc dword [EDI + 92],EAX
  mov EAX,dword [EBX + 64]
  adc dword [EDI + 96],EAX
  adc dword [EDI + 100],0
  adc dword [EDI + 104],0
  adc dword [EDI + 108],0
  adc dword [EDI + 112],0
  adc dword [EDI + 116],0
  adc dword [EDI + 120],0
  adc dword [EDI + 124],0

  mov ECX,35
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;                

procedure RawSqr16(A, R: LongWord); stdcall;
var
  SumA: array [0..8] of LongWord;
  P2: array [0..16] of LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDI,R

  // --- Sqr low ---
  push EDI
  push ESI
  call RawSqr8

  // --- Sqr high ---
  lea EAX,[EDI + 64]
  push EAX
  lea EAX,[ESI + 32]
  push EAX
  call RawSqr8

  // --- Add A ---
  lea EDX,SumA
  mov EAX,dword [ESI]
  add EAX,dword [ESI + 32]
  mov dword [EDX],EAX
  mov EAX,dword [ESI + 4]
  adc EAX,dword [ESI + 36]
  mov dword [EDX + 4],EAX
  mov EAX,dword [ESI + 8]
  adc EAX,dword [ESI + 40]
  mov dword [EDX + 8],EAX
  mov EAX,dword [ESI + 12]
  adc EAX,dword [ESI + 44]
  mov dword [EDX + 12],EAX
  mov EAX,dword [ESI + 16]
  adc EAX,dword [ESI + 48]
  mov dword [EDX + 16],EAX
  mov EAX,dword [ESI + 20]
  adc EAX,dword [ESI + 52]
  mov dword [EDX + 20],EAX
  mov EAX,dword [ESI + 24]
  adc EAX,dword [ESI + 56]
  mov dword [EDX + 24],EAX
  mov EAX,dword [ESI + 28]
  adc EAX,dword [ESI + 60]
  mov dword [EDX + 28],EAX
  mov EAX,0
  adc EAX,0
  mov dword [EDX + 32],EAX

  // --- Sqr middle ---
  lea ESI,SumA
  lea ECX,P2

  push ECX
  push ESI
  call RawSqr8

  lea ESI,SumA
  lea EBX,P2

  mov dword [EBX + 64],0
  mov EAX,dword [ESI + 32]
  test EAX,EAX
  jz @@1
  mov EAX,dword [ESI]
  add dword [ESI],EAX
  mov EAX,dword [ESI + 4]
  adc dword [ESI + 4],EAX
  mov EAX,dword [ESI + 8]
  adc dword [ESI + 8],EAX
  mov EAX,dword [ESI + 12]
  adc dword [ESI + 12],EAX
  mov EAX,dword [ESI + 16]
  adc dword [ESI + 16],EAX
  mov EAX,dword [ESI + 20]
  adc dword [ESI + 20],EAX
  mov EAX,dword [ESI + 24]
  adc dword [ESI + 24],EAX
  mov EAX,dword [ESI + 28]
  adc dword [ESI + 28],EAX
  adc dword [ESI + 32],0

  mov EAX,dword [ESI]
  add dword [EBX + 32],EAX
  mov EAX,dword [ESI + 4]
  adc dword [EBX + 36],EAX
  mov EAX,dword [ESI + 8]
  adc dword [EBX + 40],EAX
  mov EAX,dword [ESI + 12]
  adc dword [EBX + 44],EAX
  mov EAX,dword [ESI + 16]
  adc dword [EBX + 48],EAX
  mov EAX,dword [ESI + 20]
  adc dword [EBX + 52],EAX
  mov EAX,dword [ESI + 24]
  adc dword [EBX + 56],EAX
  mov EAX,dword [ESI + 28]
  adc dword [EBX + 60],EAX
  mov EAX,dword [ESI + 32]
  adc dword [EBX + 64],EAX
@@1:

  // --- Sub middle ---
  xor EDX,EDX
  xor ECX,ECX
  xor EAX,EAX
@@SubMiddle:
  add EAX,dword [EBX + ECX*4]
  adc EDX,0
  sub EAX,dword [EDI + ECX*4]
  sbb EDX,0
  sub EAX,dword [EDI + ECX*4 + 64]
  sbb EDX,0
  mov dword [EBX + ECX*4],EAX
  mov EAX,EDX
  movsx EDX,DH
  inc ECX
  cmp ECX,16
  jb @@SubMiddle
  add dword [EBX + 64],EAX

  // --- Add middle ---
  xor ECX,ECX
  clc
@@AddMiddle:
  mov EAX,dword [EBX]
  add dword [EDI + 32],EAX
  mov EAX,dword [EBX + 4]
  adc dword [EDI + 36],EAX
  mov EAX,dword [EBX + 8]
  adc dword [EDI + 40],EAX
  mov EAX,dword [EBX + 12]
  adc dword [EDI + 44],EAX
  mov EAX,dword [EBX + 16]
  adc dword [EDI + 48],EAX
  mov EAX,dword [EBX + 20]
  adc dword [EDI + 52],EAX
  mov EAX,dword [EBX + 24]
  adc dword [EDI + 56],EAX
  mov EAX,dword [EBX + 28]
  adc dword [EDI + 60],EAX
  mov EAX,dword [EBX + 32]
  adc dword [EDI + 64],EAX
  mov EAX,dword [EBX + 36]
  adc dword [EDI + 68],EAX
  mov EAX,dword [EBX + 40]
  adc dword [EDI + 72],EAX
  mov EAX,dword [EBX + 44]
  adc dword [EDI + 76],EAX
  mov EAX,dword [EBX + 48]
  adc dword [EDI + 80],EAX
  mov EAX,dword [EBX + 52]
  adc dword [EDI + 84],EAX
  mov EAX,dword [EBX + 56]
  adc dword [EDI + 88],EAX
  mov EAX,dword [EBX + 60]
  adc dword [EDI + 92],EAX
  mov EAX,dword [EBX + 64]
  adc dword [EDI + 96],EAX
  adc dword [EDI + 100],0
  adc dword [EDI + 104],0
  adc dword [EDI + 108],0
  adc dword [EDI + 112],0
  adc dword [EDI + 116],0
  adc dword [EDI + 120],0
  adc dword [EDI + 124],0

  mov ECX,26
  lea EDI,P2
  xor EAX,EAX
  rep stosd

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMul(A, B, C, D: LongWord); stdcall;
var
  OldESP: LongWord;
  SumA, SumATop: LongWord;
  SumB, SumBTop: LongWord;
  P2, P2Top: LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDX,B
  mov EDI,C

  // --- Mul low ---
  mov ECX,D
  cmp ECX,48
  ja @@Low48      
  push EDI
  push EDX
  push ESI
  call RawMul12
  jmp @@LowDoneMul
@@Low48:
  cmp ECX,64
  ja @@Low64
  push EDI
  push EDX
  push ESI
  call RawMul16
  jmp @@LowDoneMul
@@Low64:
  shr ECX,1
  push ECX
  push EDI
  push EDX
  push ESI
  call RawMul
@@LowDoneMul:

  // --- Mul high ---
  mov ECX,D
  mov EDX,B
  cmp ECX,48
  ja @@High48
  lea EAX,[EDI + ECX*2]
  push EAX
  lea EAX,[EDX + ECX]
  push EAX
  lea EAX,[ESI + ECX]
  push EAX
  call RawMul12
  jmp @@HighDoneMul
@@High48:
  cmp ECX,64
  ja @@High64
  lea EAX,[EDI + ECX*2]
  push EAX
  lea EAX,[EDX + ECX]
  push EAX
  lea EAX,[ESI + ECX]
  push EAX
  call RawMul16
  jmp @@HighDoneMul
@@High64:
  shr ECX,1
  push ECX
  lea EAX,[EDI + ECX*4]
  push EAX
  lea EAX,[EDX + ECX*2]
  push EAX
  lea EAX,[ESI + ECX*2]
  push EAX
  call RawMul
@@HighDoneMul:

  mov OldESP,ESP
  // --- Add A ---
  mov ECX,D
  mov EDX,ESP
  sub ESP,ECX
  mov SumA,ESP
  lea ESI,[ESI + ECX]
  lea EDI,[ESI + ECX]
  shr ECX,2
  neg ECX
  clc
@@AddA:
  mov EAX,dword [ESI + ECX*4]
  adc EAX,dword [EDI + ECX*4]
  mov dword [EDX + ECX*4],EAX
  inc ECX
  jnz @@AddA
  mov SumATop,0
  adc SumATop,0

  // --- Add B ---
  mov ECX,D
  mov EDX,ESP
  sub ESP,ECX
  mov SumB,ESP
  mov ESI,B
  lea ESI,[ESI + ECX]
  lea EDI,[ESI + ECX]
  shr ECX,2
  neg ECX
  clc
@@AddB:
  mov EAX,dword [ESI + ECX*4]
  adc EAX,dword [EDI + ECX*4]
  mov dword [EDX + ECX*4],EAX
  inc ECX
  jnz @@AddB
  mov SumBTop,0
  adc SumBTop,0
  mov EDI,C

  // --- Mul middle ---
  mov ECX,D
  add ECX,ECX
  sub ESP,ECX
  mov P2,ESP

  mov ESI,SumA
  mov EDX,SumB

  mov ECX,D  
  cmp ECX,48
  ja @@Mid48
  mov ECX,P2
  push ECX
  push EDX
  push ESI
  call RawMul12
  jmp @@MidDoneMul
@@Mid48:
  cmp ECX,64
  ja @@Mid64
  mov ECX,P2
  push ECX
  push EDX
  push ESI
  call RawMul16
  jmp @@MidDoneMul
@@Mid64:
  shr ECX,1
  push ECX
  mov ECX,P2
  push ECX
  push EDX
  push ESI
  call RawMul
@@MidDoneMul:

  mov P2Top,0
  mov EAX,SumBTop
  test EAX,EAX
  jz @@1
  mov ECX,D
  shr ECX,2
  mov EDX,P2
  lea EDX,[EDX + ECX*8]
  mov ESI,SumA
  lea ESI,[ESI + ECX*4]
  neg ECX
  clc
@@AddSumA:
  mov EAX,dword [ESI + ECX*4]
  adc dword [EDX + ECX*4],EAX
  inc ECX
  jnz @@AddSumA
  adc P2Top,0

@@1:
  mov EAX,SumATop
  test EAX,EAX
  jz @@2
  mov ECX,D
  shr ECX,2
  mov EDX,P2
  lea EDX,[EDX + ECX*8]
  mov ESI,SumB
  lea ESI,[ESI + ECX*4]
  neg ECX
  clc
@@AddSumB:
  mov EAX,dword [ESI + ECX*4]
  adc dword [EDX + ECX*4],EAX
  inc ECX
  jnz @@AddSumB
  adc P2Top,0

@@2:
  mov EAX,SumATop
  and EAX,SumBTop
  add P2Top,EAX

  // --- Sub middle ---
  mov ECX,D
  shr ECX,1
  mov ESI,C
  lea ESI,[ESI + ECX*4]
  lea EBX,[ESI + ECX*4]
  mov EDX,P2
  lea EDI,[EDX + ECX*4]
  xor EAX,EAX
  xor EDX,EDX
  neg ECX
@@SubMiddle:
  add EAX,dword [EDI + ECX*4]
  adc EDX,0
  sub EAX,dword [ESI + ECX*4]
  sbb EDX,0
  sub EAX,dword [EBX + ECX*4]
  sbb EDX,0
  mov dword [EDI + ECX*4],EAX
  mov EAX,EDX
  movsx EDX,DH
  inc ECX
  jnz @@SubMiddle
  add P2Top,EAX

  // --- Add middle ---
  mov ECX,D
  shr ECX,1
  mov EDI,C
  lea EDI,[EDI + ECX*2]
  lea EDI,[EDI + ECX*4]
  mov ESI,P2
  lea ESI,[ESI + ECX*4]
  mov EDX,ECX
  neg ECX
  clc
@@AddMiddle:
  mov EAX,dword [ESI + ECX*4]
  adc dword [EDI + ECX*4],EAX
  inc ECX
  jnz @@AddMiddle
  mov EAX,P2Top
  adc EAX,0
@@AddMiddleCarry:
  add dword [EDI + ECX*4],EAX
  mov EAX,0
  adc EAX,0
  jz @@AddMiddleCarryDone
  inc ECX
  cmp ECX,EDX
  jne @@AddMiddleCarry
@@AddMiddleCarryDone:

  xor EAX,EAX
@@ClearStack:
  mov dword [ESP],EAX
  add ESP,4
  cmp ESP,OldESP
  jne @@ClearStack

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawSqr(A, R, D: LongWord); stdcall;
var
  OldESP: LongWord;
  SumA, SumATop: LongWord;
  P2, P2Top: LongWord;
asm
  push EBX
  push EDI
  push ESI

  mov ESI,A
  mov EDI,R

  // --- Sqr low ---
  mov ECX,D
  cmp ECX,48
  ja @@Low48
  push EDI
  push ESI   
  push ESI
  call RawMul12
  jmp @@LowDoneSqr
@@Low48:
  cmp ECX,64
  ja @@Low64
  push EDI
  push ESI
  call RawSqr16
  jmp @@LowDoneSqr
@@Low64:
  shr ECX,1
  push ECX
  push EDI
  push ESI
  call RawSqr
@@LowDoneSqr:

  // --- Sqr high ---
  mov ECX,D    
  cmp ECX,48
  ja @@High48
  lea EAX,[EDI + ECX*2]
  push EAX
  lea EAX,[ESI + ECX]
  push EAX
  push EAX
  call RawMul12
  jmp @@HighDoneSqr
@@High48:
  cmp ECX,64
  ja @@High64
  lea EAX,[EDI + ECX*2]
  push EAX
  lea EAX,[ESI + ECX]
  push EAX
  call RawSqr16
  jmp @@HighDoneSqr
@@High64:
  shr ECX,1
  push ECX
  lea EAX,[EDI + ECX*4]
  push EAX
  lea EAX,[ESI + ECX*2]
  push EAX
  call RawSqr
@@HighDoneSqr:

  mov OldESP,ESP
  // --- Add A ---
  mov ECX,D
  mov EDX,ESP
  sub ESP,ECX
  mov SumA,ESP
  lea ESI,[ESI + ECX]
  lea EDI,[ESI + ECX]
  shr ECX,2
  neg ECX
  clc
@@AddA:
  mov EAX,dword [ESI + ECX*4]
  adc EAX,dword [EDI + ECX*4]
  mov dword [EDX + ECX*4],EAX
  inc ECX
  jnz @@AddA
  mov SumATop,0
  adc SumATop,0

  // --- Sqr middle ---
  mov ECX,D
  add ECX,ECX
  sub ESP,ECX
  mov P2,ESP

  mov ESI,SumA

  mov ECX,D
  cmp ECX,48
  ja @@Mid48
  mov ECX,P2
  push ECX
  push ESI   
  push ESI
  call RawMul12
  jmp @@MidDoneMul
@@Mid48:
  cmp ECX,64
  ja @@Mid64
  mov ECX,P2
  push ECX
  push ESI
  call RawSqr16
  jmp @@MidDoneMul
@@Mid64:
  shr ECX,1
  push ECX
  mov ECX,P2
  push ECX
  push ESI
  call RawSqr
@@MidDoneMul:

  mov P2Top,0
  mov EAX,SumATop
  test EAX,EAX
  jz @@1
  mov ECX,D
  shr ECX,2
  mov ESI,SumA
  lea ESI,[ESI + ECX*4]
  neg ECX
  clc
@@DblSumA:
  mov EAX,dword [ESI + ECX*4]
  adc dword [ESI + ECX*4],EAX
  inc ECX
  jnz @@DblSumA
  adc SumATop,0
  mov ECX,D
  shr ECX,2
  mov EDX,P2
  lea EDX,[EDX + ECX*8]
  neg ECX
  clc
@@AddSumA:
  mov EAX,dword [ESI + ECX*4]
  adc dword [EDX + ECX*4],EAX
  inc ECX
  jnz @@AddSumA
  mov EAX,SumATop
  adc P2Top,EAX
@@1:

  // --- Sub middle ---
  mov ECX,D
  shr ECX,1
  mov ESI,R
  lea ESI,[ESI + ECX*4]
  lea EBX,[ESI + ECX*4]
  mov EDI,P2
  lea EDI,[EDI + ECX*4]
  xor EAX,EAX
  xor EDX,EDX
  neg ECX
@@SubMiddle:
  add EAX,dword [EDI + ECX*4]
  adc EDX,0
  sub EAX,dword [ESI + ECX*4]
  sbb EDX,0
  sub EAX,dword [EBX + ECX*4]
  sbb EDX,0
  mov dword [EDI + ECX*4],EAX
  mov EAX,EDX
  movsx EDX,DH
  inc ECX
  jnz @@SubMiddle
  add P2Top,EAX

  // --- Add middle ---
  mov ECX,D
  shr ECX,1
  mov EDI,R
  lea EDI,[EDI + ECX*2]
  lea EDI,[EDI + ECX*4]
  mov ESI,P2
  lea ESI,[ESI + ECX*4]
  mov EDX,ECX
  neg ECX
  clc
@@AddMiddle:
  mov EAX,dword [ESI + ECX*4]
  adc dword [EDI + ECX*4],EAX
  inc ECX
  jnz @@AddMiddle
  mov EAX,P2Top
  adc EAX,0
@@AddMiddleCarry:
  add dword [EDI + ECX*4],EAX
  mov EAX,0
  adc EAX,0
  jz @@AddMiddleCarryDone
  inc ECX
  cmp ECX,EDX
  jne @@AddMiddleCarry
@@AddMiddleCarryDone:

  xor EAX,EAX
@@ClearStack:
  mov dword [ESP],EAX
  add ESP,4
  cmp ESP,OldESP
  jne @@ClearStack

  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMul17(A, B, C: Cardinal); stdcall;
asm
  push EDI
  push ESI
  push EBX

  mov EAX,C
  push EAX
  mov EAX,B
  push EAX
  mov EAX,A
  push EAX
  call RawMul16

  mov ESI,A
  mov ECX,B
  mov ECX,[ECX + 64]
  mov EDI,C

  xor EBX,EBX
  mov EAX,dword [ESI]
  mul ECX
  add dword [EDI + 64],EAX
  adc dword [EDI + 68],EDX
  adc EBX,0
  mov EAX,dword [ESI + 4]
  mul ECX
  add dword [EDI + 68],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 72],EDX
  adc EBX,0
  mov EAX,dword [ESI + 8]
  mul ECX
  add dword [EDI + 72],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 76],EDX
  adc EBX,0
  mov EAX,dword [ESI + 12]
  mul ECX
  add dword [EDI + 76],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 80],EDX
  adc EBX,0
  mov EAX,dword [ESI + 16]
  mul ECX
  add dword [EDI + 80],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 84],EDX
  adc EBX,0
  mov EAX,dword [ESI + 20]
  mul ECX
  add dword [EDI + 84],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 88],EDX
  adc EBX,0
  mov EAX,dword [ESI + 24]
  mul ECX
  add dword [EDI + 88],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 92],EDX
  adc EBX,0
  mov EAX,dword [ESI + 28]
  mul ECX
  add dword [EDI + 92],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 96],EDX
  adc EBX,0
  mov EAX,dword [ESI + 32]
  mul ECX
  add dword [EDI + 96],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 100],EDX
  adc EBX,0
  mov EAX,dword [ESI + 36]
  mul ECX
  add dword [EDI + 100],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 104],EDX
  adc EBX,0
  mov EAX,dword [ESI + 40]
  mul ECX
  add dword [EDI + 104],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 108],EDX
  adc EBX,0
  mov EAX,dword [ESI + 44]
  mul ECX
  add dword [EDI + 108],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 112],EDX
  adc EBX,0
  mov EAX,dword [ESI + 48]
  mul ECX
  add dword [EDI + 112],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 116],EDX
  adc EBX,0
  mov EAX,dword [ESI + 52]
  mul ECX
  add dword [EDI + 116],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 120],EDX
  adc EBX,0
  mov EAX,dword [ESI + 56]
  mul ECX
  add dword [EDI + 120],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 124],EDX
  adc EBX,0
  mov EAX,dword [ESI + 60]
  mul ECX
  add dword [EDI + 124],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  mov dword [EDI + 128],EDX
  mov dword [EDI + 132],EBX

  mov ESI,B
  mov ECX,A
  mov ECX,[ECX + 64]
  mov EDI,C

  xor EBX,EBX
  mov EAX,dword [ESI]
  mul ECX
  add dword [EDI + 64],EAX
  adc dword [EDI + 68],EDX
  adc EBX,0
  mov EAX,dword [ESI + 4]
  mul ECX
  add dword [EDI + 68],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 72],EDX
  adc EBX,0
  mov EAX,dword [ESI + 8]
  mul ECX
  add dword [EDI + 72],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 76],EDX
  adc EBX,0
  mov EAX,dword [ESI + 12]
  mul ECX
  add dword [EDI + 76],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 80],EDX
  adc EBX,0
  mov EAX,dword [ESI + 16]
  mul ECX
  add dword [EDI + 80],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 84],EDX
  adc EBX,0
  mov EAX,dword [ESI + 20]
  mul ECX
  add dword [EDI + 84],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 88],EDX
  adc EBX,0
  mov EAX,dword [ESI + 24]
  mul ECX
  add dword [EDI + 88],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 92],EDX
  adc EBX,0
  mov EAX,dword [ESI + 28]
  mul ECX
  add dword [EDI + 92],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 96],EDX
  adc EBX,0
  mov EAX,dword [ESI + 32]
  mul ECX
  add dword [EDI + 96],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 100],EDX
  adc EBX,0
  mov EAX,dword [ESI + 36]
  mul ECX
  add dword [EDI + 100],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 104],EDX
  adc EBX,0
  mov EAX,dword [ESI + 40]
  mul ECX
  add dword [EDI + 104],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 108],EDX
  adc EBX,0
  mov EAX,dword [ESI + 44]
  mul ECX
  add dword [EDI + 108],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 112],EDX
  adc EBX,0
  mov EAX,dword [ESI + 48]
  mul ECX
  add dword [EDI + 112],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 116],EDX
  adc EBX,0
  mov EAX,dword [ESI + 52]
  mul ECX
  add dword [EDI + 116],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 120],EDX
  adc EBX,0
  mov EAX,dword [ESI + 56]
  mul ECX
  add dword [EDI + 120],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 124],EDX
  adc EBX,0
  mov EAX,dword [ESI + 60]
  mul ECX
  add dword [EDI + 124],EAX
  adc EDX,EBX
  mov EBX,0
  adc EBX,0
  add dword [EDI + 128],EDX
  adc EBX,0
  add dword [EDI + 132],EBX

  mov EAX,A
  mov EAX,dword [EAX + 64]
  mov EDX,B
  mov EDX,dword [EDX + 64]
  mul EDX
  add dword [EDI + 128],EAX
  adc dword [EDI + 132],EDX

  pop EBX
  pop ESI
  pop EDI
end;

procedure RawSqr17(A, R: Cardinal); stdcall;
asm
  push EDI
  push ESI
  push EBX
  push EBP

  mov EAX,R
  push EAX
  mov EAX,A
  push EAX
  call RawSqr16

  mov ESI,A
  mov ECX,[ESI + 64]
  mov EDI,R

  xor EBX,EBX
  xor EBP,EBP
  mov EAX,dword [ESI]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBX,EBX
  add dword [EDI + 64],EAX
  adc dword [EDI + 68],EDX
  adc EBX,0
  mov EAX,dword [ESI + 4]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 68],EAX
  adc dword [EDI + 72],EDX
  adc EBX,0
  mov EAX,dword [ESI + 8]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 72],EAX
  adc dword [EDI + 76],EDX
  adc EBX,0
  mov EAX,dword [ESI + 12]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 76],EAX
  adc dword [EDI + 80],EDX
  adc EBX,0
  mov EAX,dword [ESI + 16]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 80],EAX
  adc dword [EDI + 84],EDX
  adc EBX,0
  mov EAX,dword [ESI + 20]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 84],EAX
  adc dword [EDI + 88],EDX
  adc EBX,0
  mov EAX,dword [ESI + 24]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 88],EAX
  adc dword [EDI + 92],EDX
  adc EBX,0
  mov EAX,dword [ESI + 28]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 92],EAX
  adc dword [EDI + 96],EDX
  adc EBX,0
  mov EAX,dword [ESI + 32]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 96],EAX
  adc dword [EDI + 100],EDX
  adc EBX,0
  mov EAX,dword [ESI + 36]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 100],EAX
  adc dword [EDI + 104],EDX
  adc EBX,0
  mov EAX,dword [ESI + 40]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 104],EAX
  adc dword [EDI + 108],EDX
  adc EBX,0
  mov EAX,dword [ESI + 44]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 108],EAX
  adc dword [EDI + 112],EDX
  adc EBX,0
  mov EAX,dword [ESI + 48]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 112],EAX
  adc dword [EDI + 116],EDX
  adc EBX,0
  mov EAX,dword [ESI + 52]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 116],EAX
  adc dword [EDI + 120],EDX
  adc EBX,0
  mov EAX,dword [ESI + 56]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 120],EAX
  adc dword [EDI + 124],EDX
  adc EBX,0
  mov EAX,dword [ESI + 60]
  mul ECX
  add EAX,EAX
  adc EDX,EDX
  adc EBP,EBP
  add EDX,EBX
  adc EBP,0
  mov EBX,EBP
  xor EBP,EBP
  add dword [EDI + 124],EAX
  adc dword [EDI + 128],EDX
  adc dword [EDI + 132],EBX

  mov EAX,dword [ESI + 64]
  mul EAX
  add dword [EDI + 128],EAX
  adc dword [EDI + 132],EDX
  pop EBP

  pop EBX
  pop ESI
  pop EDI
end;

procedure RawKaratsubaMul(A, B, C: PMPInteger);
asm
  push EBP
  push ESI
  push EDI

  mov EBP,EAX
  mov ESI,EDX
  mov EDI,ECX

  mov ECX,dword [EDI]
  mov EDX,ECX
  add EDX,EDX
  sub ESP,EDX
  xor EAX,EAX
@@A1:
  mov dword [ESP + EDX - 4],EAX
  sub EDX,4
  jnz @@A1
  mov ECX,dword [EBP]
  mov EDX,ECX
  add EDX,EDX
  lea EBP,[EBP + 8]
  sub EDX,dword [EDI]
  jle @@A
  lea EBP,[EBP + EDX*2]
  mov ECX,dword [EDI]
  shr ECX,1
@@A:
  mov EAX,dword [EBP]
  mov dword [ESP + ECX*4 - 4],EAX
  add EBP,4
  loop @@A
  mov EBP,ESP

  mov ECX,dword [EDI]
  mov EDX,ECX
  add EDX,EDX
  sub ESP,EDX
  xor EAX,EAX
@@B1:
  mov dword [ESP + EDX - 4],EAX
  sub EDX,4
  jnz @@B1
  mov ECX,dword [ESI]
  mov EDX,ECX
  add EDX,EDX
  lea ESI,[ESI + 8]
  sub EDX,dword [EDI]
  jle @@B
  lea ESI,[ESI + EDX*2]
  mov ECX,dword [EDI]
  shr ECX,1
@@B:
  mov EAX,dword [ESI]
  mov dword [ESP + ECX*4 - 4],EAX
  add ESI,4
  loop @@B
  mov ESI,ESP

  mov ECX,dword [EDI]
  mov EDX,ECX
  shl EDX,2
  sub ESP,EDX
  xor EAX,EAX
@@C1:
  mov dword [ESP + EDX - 4],EAX
  sub EDX,4
  jnz @@C1

  mov EAX,ESP
  cmp ECX,8
  ja @@8
  push EAX
  push ESI
  push EBP
  call RawMul4
  jmp @@DoneMul
@@8:
  cmp ECX,12
  ja @@12
  push EAX
  push ESI
  push EBP
  call RawMul6
  jmp @@DoneMul
@@12:
  cmp ECX,16
  ja @@16
  push EAX
  push ESI
  push EBP
  call RawMul8
  jmp @@DoneMul
@@16:
  cmp ECX,24
  ja @@24
  push EAX
  push ESI
  push EBP
  call RawMul12
  jmp @@DoneMul
@@24:
  cmp ECX,32
  ja @@32
  push EAX
  push ESI
  push EBP
  call RawMul16
  jmp @@DoneMul
@@32:
  cmp ECX,34
  ja @@34
  push EAX
  push ESI
  push EBP
  call RawMul17
  jmp @@DoneMul
@@34:
  push ECX
  push EAX
  push ESI
  push EBP
  call RawMul
  jmp @@DoneMul

@@DoneMul:
  mov ECX,dword [EDI]
  add ECX,ECX
  mov EBP,ECX

  mov ECX,dword [EDI]
  lea ESI,[ESP + ECX*4]
  lea EDI,[EDI + ECX*4 + 4]
  neg ECX
@@C:
  mov EAX,dword [ESI + ECX*4]
  mov dword [EDI],EAX
  sub EDI,4
  inc ECX
  jnz @@C

  mov ECX,EBP
  xor EAX,EAX
  mov EDI,ESP
  cld
  rep stosd
  mov ESP,EDI

  pop EDI
  pop ESI
  pop EBP
end;

procedure RawKaratsubaSqr(A, R: PMPInteger);
asm
  push EBP
  push ESI
  push EDI

  mov ESI,EAX
  mov EDI,EDX

  mov ECX,dword [EDI]
  mov EDX,ECX
  add EDX,EDX
  sub ESP,EDX
  xor EAX,EAX
@@A1:
  mov dword [ESP + EDX - 4],EAX
  sub EDX,4
  jnz @@A1
  mov ECX,dword [ESI]
  mov EDX,ECX
  add EDX,EDX
  lea ESI,[ESI + 8]
  sub EDX,dword [EDI]
  jle @@A
  lea ESI,[ESI + EDX*2]
  mov ECX,dword [EDI]
  shr ECX,1
@@A:
  mov EAX,dword [ESI]
  mov dword [ESP + ECX*4 - 4],EAX
  add ESI,4
  loop @@A
  mov ESI,ESP

  mov ECX,dword [EDI]
  mov EDX,ECX
  shl EDX,2
  sub ESP,EDX
  xor EAX,EAX
@@R1:
  mov dword [ESP + EDX - 4],EAX
  sub EDX,4
  jnz @@R1

  mov EAX,ESP
  cmp ECX,8
  ja @@8
  push EAX
  push ESI
  call RawSqr4
  jmp @@DoneMul
@@8:
  cmp ECX,16
  ja @@16
  push EAX
  push ESI
  call RawSqr8
  jmp @@DoneMul
@@16:
  cmp ECX,32
  ja @@32
  push EAX
  push ESI
  call RawSqr16
  jmp @@DoneMul
@@32:
  cmp ECX,34
  ja @@34
  push EAX
  push ESI
  call RawSqr17
  jmp @@DoneMul
@@34:
  push ECX
  push EAX
  push ESI
  call RawSqr
  jmp @@DoneMul

@@DoneMul:
  mov EBP,dword [EDI]
  mov ECX,EBP
  shr ECX,1
  add EBP,ECX

  mov ECX,dword [EDI]
  lea ESI,[ESP + ECX*4]
  lea EDI,[EDI + ECX*4 + 4]
  neg ECX
@@C:
  mov EAX,dword [ESI + ECX*4]
  mov dword [EDI],EAX
  sub EDI,4
  inc ECX
  jnz @@C

  mov ECX,EBP
  xor EAX,EAX
  mov EDI,ESP
  cld
  rep stosd
  mov ESP,EDI

  pop EDI
  pop ESI
  pop EBP
end;

function MPKaratsubaMul2(A, B: PMPInteger; var C: PMPInteger): Boolean;
var
  ASize, BSize, Size, Bit: Cardinal;
begin
  ASize := A^.Size;
  while (ASize > 1) and (A^.Data[A^.Size - ASize] = 0) do
    Dec(ASize);
  BSize := B^.Size;
  while (BSize > 1) and (B^.Data[B^.Size - BSize] = 0) do
    Dec(BSize);
  if (ASize - 1 <= BSize) and (ASize + 1 >= BSize) then begin
    Size := ASize;
    if Size < BSize then
      Size := BSize;
    if (Size <> 17) and Odd(Size) then
      Size := Size + 1;
    if Size < 4 then
      MPMul2(A,B,C)
    else begin
      if Size = 6 then
        Bit := 6
      else if Size = 12 then
        Bit := 12
      else if Size = 17 then
        Bit := 17
      else begin
        Bit := 4;
        while Bit < Size do
          Bit := Bit + Bit;
        if Size <= (Bit - Bit shr 2) then
          Bit := Bit - Bit shr 2;
      end;
      Size := Bit;
      MPRealloc(C,Size*2);
      RawKaratsubaMul(A,B,C);
      C^.Sign := A^.Sign * B^.Sign;
    end;
    Result := True;
  end else
    Result := False;
end;

function MPKaratsubaSqr2(A: PMPInteger; var R: PMPInteger): Boolean;
var
  Size, Bit: Cardinal;
begin
  Size := A^.Size;
  while (Size > 1) and (A^.Data[A^.Size - Size] = 0) do
    Dec(Size);
  if (Size <> 17) and Odd(Size) then
    Size := Size + 1;
  if Size < 4 then
    Result := False
  else begin
    Result := True;
    if Size = 17 then
      Bit := 17
    else begin
      Bit := 4;
      while Bit < Size do
        Bit := Bit + Bit;
    end;
    if (Size <> 17) and (Size <= Bit - Bit shr 2) then begin
      Size := Bit - Bit shr 2;
      MPRealloc(R,Size*2);
      RawKaratsubaMul(A,A,R);
    end else begin
      Size := Bit;
      MPRealloc(R,Size*2);
      RawKaratsubaSqr(A,R);
    end;
    R^.Sign := 1;
  end;
end;
                                                                    
type
  TLittleEndianInteger = array of LongWord;

procedure MPIntToLEI(X: PMPInteger; Size: Cardinal; var R: TLittleEndianInteger);
var
  I: Cardinal;
begin
  SetLength(R,Size);
  for I := 0 to X^.Size - 1 do begin
    if I >= Size then Break;
    R[I] := X^.Data[X^.Size - I - 1];
  end;
  for I := X^.Size to Size - 1 do
    R[I] := 0;
end;

procedure RawMontgomeryRed(X: Cardinal;
                           M: Cardinal;
                           Size: Cardinal;
                           MPrime: LongWord);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov EBX,MPrime
  lea EBP,[EDX + ECX*4]
  mov EDI,EAX
  push EBX
  push EBX

  push ECX
  push ECX
@@Outer:
  mov EBX,dword [ESP + 8]
  mov EAX,dword [EDI]
  mul EBX
  mov dword [ESP + 12],EAX

  mov ECX,dword [ESP]
  lea ESI,[ECX*4]
  lea EDI,[EDI + ESI]
  neg ECX
  xor ESI,ESI

  test EAX,EAX
  jz @@InnerDone
  xor EBX,EBX
@@InnerLow:
  mov EAX,dword [EBP + ECX*4]
  mov EDX,dword [ESP + 12]
  mul EDX
  add EAX,ESI
  adc EDX,EBX
  setc BL
  xor ESI,ESI
  add dword [EDI + ECX*4],EAX
  adc ESI,EDX
  adc BL,0
  inc ECX
  jnz @@InnerLow
  add dword [EDI],ESI
  adc BL,0
  add EBX,$FFFFFFFF
  jnc @@InnerDone
@@InnerHigh:
  adc dword [EDI + ECX*4 + 4],0
  inc ECX
  jc @@InnerHigh
@@InnerDone:
  mov ECX,dword [ESP]
  neg ECX
  inc ECX
  lea EDI,[EDI + ECX*4]

  dec dword [ESP + 4]
  jnz @@Outer

  mov ECX,dword [ESP]
  neg ECX
  lea EDI,[EDI + ECX*4]

  neg ECX
  lea ESI,[EDI + ECX*4]
  inc ECX
  cld
  rep movsd

  pop ECX
  pop ECX
  pop EBX
  pop EBX

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure RawSub(X, Y, Size: Cardinal);
asm
  push EBX
  lea EAX,[EAX + ECX*4]
  lea EDX,[EDX + ECX*4]
  neg ECX
  clc
@@1:
  mov EBX,dword [EDX + ECX*4]
  sbb dword [EAX + ECX*4],EBX
  inc ECX
  jnz @@1
  pop EBX
end;

procedure RawAdd(X, Y, Size: Cardinal);
asm
  push EBX
  lea EAX,[EAX + ECX*4]
  lea EDX,[EDX + ECX*4]
  neg ECX
  clc
@@1:
  mov EBX,dword [EDX + ECX*4]
  adc dword [EAX + ECX*4],EBX
  inc ECX
  jnz @@1
  pop EBX
end;

procedure RawMulByInt(X, Y, Size: Cardinal);
asm
  push EBX
  push ESI
  push EDI
  push EBP   
  dec ECX
  lea ESI,dword [EAX + ECX*4]
  mov EDI,EDX
  neg ECX
  xor EBX,EBX
  xor EBP,EBP
  clc
@@1:
  mov EAX,dword [ESI + ECX*4]
  mul EDI
  add EAX,EBP
  adc EDX,EBX
  setc BL
  mov dword [ESI + ECX*4],EAX
  mov EBP,EDX
  inc ECX
  jnz @@1
  mov dword [ESI + ECX*4],EBP
  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

procedure MontgomeryParams(M: PMPInteger;
                           var R, RQuad: TLittleEndianInteger;
                           var MPrime: LongWord;
                           MSize: Integer);
var
  X, Y: PMPInteger;
begin
  X := nil;
  Y := nil;
  MPMontgomeryParams(M,X,Y,MPrime);
  MPIntToLEI(X,MSize,R);
  MPIntToLEI(Y,MSize,RQuad);
  MPDealloc(X);
  MPDealloc(Y);
end;

procedure MPKaratsubaExpMod(G, E, M: PMPInteger; var R: PMPInteger);
var
  LW, Bit, EW, MPrime: LongWord;
  MSize, MTop: Cardinal;
  i, j, c, k, l: Cardinal;
  lG, lM, lR, G2: TLittleEndianInteger;
  WG: array of TLittleEndianInteger;
  RQuad, X, A: TLittleEndianInteger;

  procedure Mont(X,Y,M: TLittleEndianInteger; MPrime: LongWord; var R: TLittleEndianInteger);
  var
    I: Integer;
  begin
    if Cardinal(X) = Cardinal(Y) then
      case MSize of
        4: RawSqr4(Cardinal(X),Cardinal(A));
        6: RawMul6(Cardinal(X),Cardinal(X),Cardinal(A));
        8: RawSqr8(Cardinal(X),Cardinal(A));
        12: RawMul12(Cardinal(X),Cardinal(X),Cardinal(A));
        16: RawSqr16(Cardinal(X),Cardinal(A));
        17: RawSqr17(Cardinal(X),Cardinal(A));
      else
        RawSqr(Cardinal(X),Cardinal(A),MSize*2);
      end
    else
      case MSize of
        4: RawMul4(Cardinal(X),Cardinal(Y),Cardinal(A));
        6: RawMul6(Cardinal(X),Cardinal(Y),Cardinal(A));
        8: RawMul8(Cardinal(X),Cardinal(Y),Cardinal(A));
        12: RawMul12(Cardinal(X),Cardinal(Y),Cardinal(A));
        16: RawMul16(Cardinal(X),Cardinal(Y),Cardinal(A));
        17: RawMul17(Cardinal(X),Cardinal(Y),Cardinal(A));
      else
        RawMul(Cardinal(X),Cardinal(Y),Cardinal(A),MSize*2);
      end;
    RawMontgomeryRed(Cardinal(A),Cardinal(M),MTop,MPrime);
    if MTop < MSize - 1 then
      FillChar(A[MTop + 1],(MSize - MTop - 1)*4,0);
    if A[MTop] > 0 then
      RawSub(Cardinal(A),Cardinal(M),MSize)
    else
      for I := MSize-1 downto 0 do
        if A[I] <> M[I] then begin
          if A[I] > M[I] then
            RawSub(Cardinal(A),Cardinal(M),MSize);
          Break;
        end else if I = 0 then
          RawSub(Cardinal(A),Cardinal(M),MSize);
    Move(A[0],R[0],MSize*4);
    FillChar(A[0],Length(A)*4,0);
  end;

begin
  MTop := M^.Size;
  if MTop = 17 then
    MSize := 17
  else begin
    MSize := 4;
    while MSize < MTop do
      MSize := MSize + MSize;
   if MSize > 4 then
     if MTop <= MSize - (MSize shr 2) then
       MSize := MSize - (MSize shr 2);
  end;

  if G^.Size > MSize then begin
    G := MPCopy(G);
    MPMod(G,M);
    MPIntToLEI(G,MSize,lG);
    MPDealloc(G);
  end else
    MPIntToLEI(G,MSize,lG);
  MPIntToLEI(M,MSize,lM);
  SetLength(A,MSize*2 + 1);
  FillChar(A[0],MSize*8+4,0);

  c := MPMSB(E);
  case c of
    0..18:     k := 1;
    19..48:    k := 3;
    49..160:   k := 4;
    161..480:  k := 5;
    481..1344: k := 6;
    1345..3584:k := 7;
  else
    k := 8;
  end;

  MontgomeryParams(M,lR,RQuad,MPrime,MSize);
  SetLength(X,MSize);
  Mont(lG,RQuad,lM,MPrime,X);

  SetLength(WG,1 shl (k-1));
  SetLength(G2,MSize);
  Mont(X,X,lM,MPrime,G2);
  WG[0] := X;
  for i := 1 to (1 shl (k-1)) - 1 do begin
    SetLength(WG[i],MSize);
    Mont(WG[i-1],G2,lM,MPrime,WG[i]);
  end;
  FillChar(G2[0],Length(G2)*4,0);
  SetLength(G2,0);

  EW := 0;
  for i := 0 to E^.Size - 1 do begin
    LW := E^.Data[i];
    Bit := $80000000;
    for j := 31 downto 0 do begin
      EW := EW shl 1;
      if (Bit and LW) = Bit then
        EW := EW + 1
      else if EW = 0 then
        Mont(lR,lR,lM,MPrime,lR);
      if (EW shr (k-1)) = 1 then begin
        l := 0;
        while not Odd(EW) do begin
          Inc(l);
          EW := EW shr 1;
        end;
        c := k - l;
        while c > 0 do begin
          Mont(lR,lR,lM,MPrime,lR);
          Dec(c);
        end;
        Mont(lR,WG[EW shr 1],lM,MPrime,lR);
        while l > 0 do begin
          Mont(lR,lR,lM,MPrime,lR);
          Dec(l);
        end;
        EW := 0;
      end;
      Bit := Bit shr 1;
    end;
  end;
  if EW <> 0 then begin
    i := k;
    while (EW shr (i - 1)) = 0 do
      Dec(i);
    l := 0;
    while not Odd(EW) do begin
      Inc(l);
      EW := EW shr 1;
    end;
    c := i - l;
    while c > 0 do begin
      Mont(lR,lR,lM,MPrime,lR);
      Dec(c);
    end;
    Mont(lR,WG[EW shr 1],lM,MPrime,lR);
    while l > 0 do begin
      Mont(lR,lR,lM,MPrime,lR);
      Dec(l);
    end;
  end;
  FillChar(X[0],MSize*4,0);
  X[0] := 1;
  Mont(lR,X,lM,MPrime,lR);

  for i := 1 to (1 shl (k-1)) - 1 do
    FillChar(WG[i,0],Length(WG[i])*4,0);
  WG := nil;

  MPRealloc(R,M^.Size);
  for I := 0 to R^.Size - 1 do
    R^.Data[R^.Size - I - 1] := lR[I];
  R^.Sign := 1;
  FillChar(RQuad[0],Length(RQuad)*4,0);
  FillChar(lR[0],Length(lR)*4,0);
  FillChar(lM[0],MSize*4,0);
end;

procedure MPKaratsubaPow2Mod(E, M: PMPInteger; var R: PMPInteger);
var
  LW, MPrime: LongWord;
  MSize, MTop: Cardinal;
  i, j, k: Cardinal;
  lM, lR: TLittleEndianInteger;
  RQuad, X, A: TLittleEndianInteger;
  HasMult: Boolean;

  procedure Mont(X,Y,M: TLittleEndianInteger; MPrime: LongWord; var R: TLittleEndianInteger);
  var
    I: Integer;
  begin
    if Cardinal(X) = Cardinal(Y) then
      case MSize of
        4: RawSqr4(Cardinal(X),Cardinal(A));
        6: RawMul6(Cardinal(X),Cardinal(X),Cardinal(A));
        8: RawSqr8(Cardinal(X),Cardinal(A));
        12: RawMul12(Cardinal(X),Cardinal(X),Cardinal(A));
        16: RawSqr16(Cardinal(X),Cardinal(A));
        17: RawSqr17(Cardinal(X),Cardinal(A));
      else
        RawSqr(Cardinal(X),Cardinal(A),MSize*2);
      end
    else
      case MSize of
        4: RawMul4(Cardinal(X),Cardinal(Y),Cardinal(A));
        6: RawMul6(Cardinal(X),Cardinal(Y),Cardinal(A));
        8: RawMul8(Cardinal(X),Cardinal(Y),Cardinal(A));
        12: RawMul12(Cardinal(X),Cardinal(Y),Cardinal(A));
        16: RawMul16(Cardinal(X),Cardinal(Y),Cardinal(A));
        17: RawMul17(Cardinal(X),Cardinal(Y),Cardinal(A));
      else
        RawMul(Cardinal(X),Cardinal(Y),Cardinal(A),MSize*2);
      end;
    RawMontgomeryRed(Cardinal(A),Cardinal(M),MTop,MPrime);         
    if MTop < MSize - 1 then
      FillChar(A[MTop + 1],(MSize - MTop - 1)*4,0);
    if A[MTop] > 0 then
      RawSub(Cardinal(A),Cardinal(M),MSize)
    else
      for I := MSize-1 downto 0 do
        if A[I] <> M[I] then begin
          if A[I] > M[I] then
            RawSub(Cardinal(A),Cardinal(M),MSize);
          Break;
        end else if I = 0 then
          RawSub(Cardinal(A),Cardinal(M),MSize);
    Move(A[0],R[0],MSize*4);
    FillChar(A[0],Length(A)*4,0);
  end;

begin
  MTop := M^.Size;
  if MTop = 17 then
    MSize := 17
  else begin
    MSize := 4;
    while MSize < MTop do
        MSize := MSize + MSize;
   if MSize > 4 then
     if MTop <= MSize - (MSize shr 2) then
       MSize := MSize - (MSize shr 2);
  end;

  MPIntToLEI(M,MSize,lM);
  SetLength(A,MSize*2 + 1);
  FillChar(A[0],MSize*8+4,0);

  MontgomeryParams(M,lR,RQuad,MPrime,MSize + 1);

  HasMult := False;
  for i := 0 to E^.Size - 1 do begin
    LW := E^.Data[i];

    if (LW = 0) and not HasMult then Continue;
    HasMult := True;
    for j := 31 downto 0 do begin
      Mont(lR,lR,lM,MPrime,lR);
      if LW and $80000000 > 0 then begin
        RawAdd(Cardinal(lR),Cardinal(lR),MSize + 1);
        if lR[MSize] > 0 then
          RawSub(Cardinal(lR),Cardinal(lM),MSize)
        else
          for k := MSize-1 downto 0 do
            if lR[k] <> lM[k] then begin
              if lR[k] > lM[k] then
                RawSub(Cardinal(lR),Cardinal(lM),MSize);
              Break;
            end else if k = 0 then
              RawSub(Cardinal(lR),Cardinal(lM),MSize);
        lR[MSize] := 0;
      end;
      LW := LW shl 1;
    end;
  end;

  SetLength(X,MSize);
  FillChar(X[0],MSize*4,0);
  X[0] := 1;
  Mont(lR,X,lM,MPrime,lR);
  MPRealloc(R,M^.Size);
  for I := 0 to R^.Size - 1 do
    R^.Data[R^.Size - I - 1] := lR[I];
  R^.Sign := 1;

  FillChar(RQuad[0],Length(RQuad)*4,0);
  FillChar(lR[0],Length(lR)*4,0);
  FillChar(lM[0],MSize*4,0);
end;

procedure MPKaratsubaPowBMod(Base: Cardinal; E, M: PMPInteger; var R: PMPInteger);
var
  LW, MPrime: LongWord;
  MSize, MTop: Cardinal;
  i, j, k, highk: Cardinal;
  lM, lR: TLittleEndianInteger;
  RQuad, X, A: TLittleEndianInteger;
  Divs: array [0..31] of TLittleEndianInteger;
  HasMult: Boolean;

  procedure Mont(X,Y,M: TLittleEndianInteger; MPrime: LongWord; var R: TLittleEndianInteger);
  var
    I: Integer;
  begin
    if Cardinal(X) = Cardinal(Y) then
      case MSize of
        4: RawSqr4(Cardinal(X),Cardinal(A));
        6: RawMul6(Cardinal(X),Cardinal(X),Cardinal(A));
        8: RawSqr8(Cardinal(X),Cardinal(A));
        12: RawMul12(Cardinal(X),Cardinal(X),Cardinal(A));
        16: RawSqr16(Cardinal(X),Cardinal(A));
        17: RawSqr17(Cardinal(X),Cardinal(A));
      else
        RawSqr(Cardinal(X),Cardinal(A),MSize*2);
      end
    else
      case MSize of
        4: RawMul4(Cardinal(X),Cardinal(Y),Cardinal(A));
        6: RawMul6(Cardinal(X),Cardinal(Y),Cardinal(A));
        8: RawMul8(Cardinal(X),Cardinal(Y),Cardinal(A));
        12: RawMul12(Cardinal(X),Cardinal(Y),Cardinal(A));
        16: RawMul16(Cardinal(X),Cardinal(Y),Cardinal(A));
        17: RawMul17(Cardinal(X),Cardinal(Y),Cardinal(A));
      else
        RawMul(Cardinal(X),Cardinal(Y),Cardinal(A),MSize*2);
      end;
    RawMontgomeryRed(Cardinal(A),Cardinal(M),MTop,MPrime);
    if MTop < MSize - 1 then
      FillChar(A[MTop + 1],(MSize - MTop - 1)*4,0);
    if A[MTop] > 0 then
      RawSub(Cardinal(A),Cardinal(M),MSize)
    else
      for I := MSize-1 downto 0 do
        if A[I] <> M[I] then begin
          if A[I] > M[I] then
            RawSub(Cardinal(A),Cardinal(M),MSize);
          Break;
        end else if I = 0 then
          RawSub(Cardinal(A),Cardinal(M),MSize);
    Move(A[0],R[0],MSize*4);
    FillChar(A[0],Length(A)*4,0);
  end;

  procedure Reduction;
  var
    i, k: Cardinal;
  begin
    for i := highk downto 0 do begin
      for k := MSize downto 0 do
        if lR[k] <> Divs[i,k] then begin
          if lR[k] > Divs[i,k] then
            RawSub(Cardinal(lR),Cardinal(Divs[i]),MSize + 1);
          Break;
        end else if k = 0 then
          RawSub(Cardinal(lR),Cardinal(Divs[i]),MSize + 1);
    end;
  end;

begin
  MTop := M^.Size;
  if MTop = 17 then
    MSize := 17
  else begin
    MSize := 4;
    while MSize < MTop do
      MSize := MSize + MSize;
   if MSize > 4 then
     if MTop <= MSize - (MSize shr 2) then
       MSize := MSize - (MSize shr 2);
  end;

  MPIntToLEI(M,MSize,lM);
  SetLength(A,MSize*2 + 1);
  FillChar(A[0],MSize*8+4,0);

  MontgomeryParams(M,lR,RQuad,MPrime,MSize + 1);

  highk := 0;
  LW := 2;
  while LW < Base do begin
    Inc(highk);
    LW := LW + LW;
  end;
  FillChar(Divs,SizeOf(Divs),0);
  MPIntToLEI(M,MSize + 1,Divs[0]);
  for k := 1 to highk do begin
    SetLength(Divs[k],MSize + 1);
    for i := 0 to MSize do
      Divs[k,i] := Divs[k-1,i];
    RawAdd(Cardinal(Divs[k]),Cardinal(Divs[k]),MSize + 1);
  end;

  HasMult := False;
  for i := 0 to E^.Size - 1 do begin
    LW := E^.Data[i];

    if (LW = 0) and not HasMult then Continue;
    HasMult := True;
    for j := 31 downto 0 do begin
      Mont(lR,lR,lM,MPrime,lR);
      if LW and $80000000 > 0 then begin
        RawMulByInt(Cardinal(lR),Base,MSize + 1);
        Reduction;
      end;
      LW := LW shl 1;
    end;
  end;

  SetLength(X,MSize);
  FillChar(X[0],MSize*4,0);
  X[0] := 1;
  Mont(lR,X,lM,MPrime,lR);
  MPRealloc(R,M^.Size);
  for I := 0 to R^.Size - 1 do
    R^.Data[R^.Size - I - 1] := lR[I];
  R^.Sign := 1;

  FillChar(RQuad[0],Length(RQuad)*4,0);
  FillChar(lR[0],Length(lR)*4,0);
  FillChar(lM[0],MSize*4,0);
end;

end.
