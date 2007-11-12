{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPEC_NISTCurves Unit                              }
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
unit MpEC_NISTCurves;

interface

uses
  MpArithTypes;
               
procedure MPModP192(var X: PMPInteger; M: PMPInteger);
procedure MPModP224(var X: PMPInteger; M: PMPInteger);
procedure MPModP256(var X: PMPInteger; M: PMPInteger);
procedure MPModP384(var X: PMPInteger; M: PMPInteger);
procedure MPModP521(var X: PMPInteger; M: PMPInteger);
                                                
procedure MPMulModP192(X, Y, M: PMPInteger; var R: PMPInteger);  
procedure MPMulModP224(X, Y, M: PMPInteger; var R: PMPInteger);  
procedure MPMulModP256(X, Y, M: PMPInteger; var R: PMPInteger);  
//procedure MPMulModP521(X, Y, M: PMPInteger; var R: PMPInteger);

procedure ECurveP192(var C: TECurve);   
procedure ECurveP224(var C: TECurve);
procedure ECurveP256(var C: TECurve);
procedure ECurveP384(var C: TECurve);
procedure ECurveP521(var C: TECurve);

function IsECurveP192(var C: TECurve): Boolean;
function IsECurveP224(var C: TECurve): Boolean;
function IsECurveP256(var C: TECurve): Boolean;
function IsECurveP384(var C: TECurve): Boolean;
function IsECurveP521(var C: TECurve): Boolean;

function TryIsECurveP224(const C: TECurve): Boolean;
function TryIsECurveP384(const C: TECurve): Boolean;
function TryIsECurveP521(const C: TECurve): Boolean;

implementation

uses
  SecUtils, MpArith, MpECArith;

procedure RawMulP192(Src1, Src2, Dst: Pointer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  sub ESP,48

  mov ESI,EAX
  mov EBP,EDX
  mov EDI,ECX
  push EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 20]
  mul EDX
  mov dword [ESP + 48],EAX
  mov EBX,EDX

  xor ECX,ECX
  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 44],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 40],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 36],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  lea EAX,[ESP + 4]
  mov dword [EAX + 28],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 28],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 24],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 20],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 16],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 12],EBX
  mov EBX,ECX
  mov ECX,EDI

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX

  mov dword [ESP +  8],EBX
  mov dword [ESP +  4],ECX

  lea EAX,[ESP + 4]
  mov EDX,[ESP]

  xor EBX,EBX
  mov ECX,[EAX + 44].dword
  add ECX,[EAX + 20].dword
  adc EBX,0
  add ECX,[EAX + 4].dword
  adc EBX,0
  mov [EDX + 20].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 40].dword
  adc EBX,0
  add ECX,[EAX + 16].dword
  adc EBX,0
  add ECX,[EAX + 0].dword
  adc EBX,0
  mov [EDX + 16].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 36].dword
  adc EBX,0
  add ECX,[EAX + 20].dword
  adc EBX,0
  add ECX,[EAX + 12].dword
  adc EBX,0
  add ECX,[EAX + 4].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 32].dword
  adc EBX,0
  add ECX,[EAX + 16].dword
  adc EBX,0
  add ECX,[EAX + 8].dword
  adc EBX,0
  add ECX,[EAX + 0].dword
  adc EBX,0
  mov [EDX + 8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 28].dword
  adc EBX,0
  add ECX,[EAX + 12].dword
  adc EBX,0
  add ECX,[EAX + 4].dword
  adc EBX,0
  mov [EDX + 4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 24].dword
  adc EBX,0
  add ECX,[EAX + 8].dword
  adc EBX,0
  add ECX,[EAX + 0].dword
  adc EBX,0
  mov [EDX + 0].dword,ECX

  test EBX,EBX
  jz @@2
@@1:
  add [EDX + 20].dword,1
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,1
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  sbb EBX,1
  jnz @@1
@@2:
  xor EAX,EAX
  mov ECX,12
  lea EDI,[ESP+4]
  cld
  rep stosd

  add ESP,52

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

procedure RawModP192(Src: Pointer; Dst: Pointer);
asm
  push EBX

  xor EBX,EBX
  mov ECX,[EAX + 44].dword
  add ECX,[EAX + 20].dword
  adc EBX,0
  add ECX,[EAX + 4].dword
  adc EBX,0
  mov [EDX + 20].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 40].dword
  adc EBX,0
  add ECX,[EAX + 16].dword
  adc EBX,0
  add ECX,[EAX + 0].dword
  adc EBX,0
  mov [EDX + 16].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 36].dword
  adc EBX,0
  add ECX,[EAX + 20].dword
  adc EBX,0
  add ECX,[EAX + 12].dword
  adc EBX,0
  add ECX,[EAX + 4].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 32].dword
  adc EBX,0
  add ECX,[EAX + 16].dword
  adc EBX,0
  add ECX,[EAX + 8].dword
  adc EBX,0
  add ECX,[EAX + 0].dword
  adc EBX,0
  mov [EDX + 8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 28].dword
  adc EBX,0
  add ECX,[EAX + 12].dword
  adc EBX,0
  add ECX,[EAX + 4].dword
  adc EBX,0
  mov [EDX + 4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[EAX + 24].dword
  adc EBX,0
  add ECX,[EAX + 8].dword
  adc EBX,0
  add ECX,[EAX + 0].dword
  adc EBX,0
  mov [EDX + 0].dword,ECX

  test EBX,EBX
  jz @@2
@@1:
  add [EDX + 20].dword,1
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,1
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  sbb EBX,1
  jnz @@1
@@2:
  pop EBX
end;

procedure RawAddP192(Src: Pointer);
asm
  mov EDX,dword [EAX + 20]
  mov ECX,0FFFFFFFFh
  sub ECX,EDX
  mov dword [EAX + 20],ECX
  mov EDX,dword [EAX + 16]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 16],ECX
  mov EDX,dword [EAX + 12]
  mov ECX,0FFFFFFFEh
  sbb ECX,EDX
  mov dword [EAX + 12],ECX
  mov EDX,dword [EAX +  8]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX +  8],ECX
  mov EDX,dword [EAX +  4]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX +  4],ECX
  mov EDX,dword [EAX     ]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX     ],ECX
end;

procedure MPModP192(var X: PMPInteger; M: PMPInteger);
var
  L: Integer;
  R: PMPInteger;
begin
  if X^.Size < 6 then
    L := 160
  else if X^.Size <= 6 then
    L := 192
  else if (X^.Size <= 12) or
          ((X^.Size = 13) and (X^.Data[0] = 0)) or
          ((X^.Size = 14) and (X^.Data[0] = 0) and (X^.Data[1] = 0)) then
    L := 192*2
  else
    L := MPMSB(X);
  if L = 192 then
    while MPCmpOffset(X,M) >= 0 do
      MPSub(X,M)
  else if L > 192*2 then
    MPMod(X,M)
  else if L > 192 then begin
    MPRealloc(X,12);
    R := nil;
    MPRealloc(R,6);
    RawModP192(@X.Data,@R.Data);    
    R^.Sign := 1;
    if (X^.Sign < 0) and (MPMSB(R) > 0) then
      RawAddP192(@R^.Data);
    MPDealloc(X);
    X := R;
  end;
end;

procedure MPMulModP192(X, Y, M: PMPInteger; var R: PMPInteger);
var
  Sign: Integer;
  XD, YD: Pointer;
begin
  if ((X^.Size <= 6) or ((X^.Size = 7) and (X^.Data[0] = 0))) and
     ((Y^.Size <= 6) or ((Y^.Size = 7) and (Y^.Data[0] = 0))) then begin
    MPRealloc(X,6);
    MPRealloc(Y,6);
    XD := @X^.Data;
    YD := @Y^.Data;
    if (LongInt(R) <> LongInt(X)) and (LongInt(R) <> LongInt(Y)) then
      MPRealloc(R,6);
    if R^.Size = 6 then
      RawMulP192(XD,YD,@R^.Data)
    else begin
      RawMulP192(XD,YD,@R^.Data[1]);
      MPRealloc(R,6);
    end;
    if X.Sign = Y.Sign then
      Sign := 1
    else
      Sign := -1;
    R^.Sign := 1;
    if (Sign < 0) and (MPMSB(R) > 0) then
      RawAddP192(@R^.Data);
  end else begin
    MPMul2(X,Y,R);
    MPModP192(R,M);
  end;
end;

procedure RawMulP224(Src1, Src2, Dst: Pointer);      
const
  p0 = 52;
  p1 = 48;
  p2 = 44;
  p3 = 40;
  p4 = 36;
  p5 = 32;
  p6 = 28;
  p7 = 24;
  p8 = 20;
  p9 = 16;
  p10 = 12;
  p11 = 8;
  p12 = 4;
  p13 = 0;
asm
  push EBX
  push ESI
  push EDI
  push EBP

  sub ESP,56

  mov ESI,EAX
  mov EBP,EDX
  mov EDI,ECX
  push EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 24]
  mul EDX
  mov dword [ESP + 56],EAX
  mov EBX,EDX

  xor ECX,ECX
  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 52],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 48],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 44],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 40],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 36],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 32],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 28],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 24],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 20],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 16],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 12],EBX
  mov EBX,ECX
  mov ECX,EDI

  mov EAX,dword [ESI     ]
  mov EDX,dword [EBP     ]
  mul EDX
  add EBX,EAX
  adc ECX,EDX

  mov dword [ESP +  8],EBX
  mov dword [ESP +  4],ECX

  lea ESI,[ESP + 4]
  mov EDX,[ESP]

  mov ECX,[ESI + p0].dword
  mov [EDX + 24].dword,ECX
  mov ECX,[ESI + p1].dword
  mov [EDX + 20].dword,ECX
  mov ECX,[ESI + p2].dword
  mov [EDX + 16].dword,ECX

  xor EBX,EBX
  mov ECX,[ESI + p3].dword
  adc EBX,0
  add ECX,[ESI + p7].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p4].dword
  adc EBX,0
  add ECX,[ESI + p8].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  mov [EDX + 8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p5].dword
  adc EBX,0
  add ECX,[ESI + p9].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  mov [EDX + 4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p6].dword
  adc EBX,0
  add ECX,[ESI + p10].dword
  adc EBX,0
  mov [EDX + 0].dword,ECX

  // --

  xor EAX,EAX
  mov ECX,[ESI + p7].dword
  add ECX,[ESI + p11].dword
  adc EAX,0
  sub [EDX + 24].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p8].dword
  adc EAX,0
  add ECX,[ESI + p12].dword
  adc EAX,0
  sub [EDX + 20].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p9].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  sub [EDX + 16].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p10].dword
  adc EAX,0
  sub [EDX + 12].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p11].dword
  adc EAX,0
  sub [EDX + 8].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p12].dword
  adc EAX,0
  sub [EDX + 4].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p13].dword
  adc EAX,0
  sub [EDX + 0].dword,ECX
  adc EAX,0

  sub EBX,EAX

  cmp EBX,0
  jz @@3
  jl @@2
@@1:
  sub [EDX + 24].dword,1
  sbb [EDX + 20].dword,0
  sbb [EDX + 16].dword,0
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,0
  sbb EBX,1
  add [EDX + 12].dword,1
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  adc EBX,0
  test EBX,EBX
  jnz @@1
  jmp @@3
@@2:
  add [EDX + 24].dword,1
  adc [EDX + 20].dword,0
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  adc EBX,1
  sub [EDX + 12].dword,1
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,0
  sbb EBX,0
  test EBX,EBX
  jnz @@2
@@3:
  xor EAX,EAX
  mov ECX,14
  lea EDI,[ESP+4]
  cld
  rep stosd

  add ESP,60

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;

procedure RawModP224(Src: Pointer; Dst: Pointer);
const
  p0 = 52;
  p1 = 48;
  p2 = 44;
  p3 = 40;
  p4 = 36;
  p5 = 32;
  p6 = 28;
  p7 = 24;
  p8 = 20;
  p9 = 16;
  p10 = 12;
  p11 = 8;
  p12 = 4;
  p13 = 0;
asm
  push EBX
  push ESI

  mov ESI,EAX

  mov ECX,[ESI + p0].dword
  mov [EDX + 24].dword,ECX
  mov ECX,[ESI + p1].dword
  mov [EDX + 20].dword,ECX
  mov ECX,[ESI + p2].dword
  mov [EDX + 16].dword,ECX

  xor EBX,EBX
  mov ECX,[ESI + p3].dword
  adc EBX,0
  add ECX,[ESI + p7].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p4].dword
  adc EBX,0
  add ECX,[ESI + p8].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  mov [EDX + 8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p5].dword
  adc EBX,0
  add ECX,[ESI + p9].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  mov [EDX + 4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p6].dword
  adc EBX,0
  add ECX,[ESI + p10].dword
  adc EBX,0
  mov [EDX + 0].dword,ECX

  // --

  xor EAX,EAX
  mov ECX,[ESI + p7].dword
  add ECX,[ESI + p11].dword
  adc EAX,0
  sub [EDX + 24].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p8].dword
  adc EAX,0
  add ECX,[ESI + p12].dword
  adc EAX,0
  sub [EDX + 20].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p9].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  sub [EDX + 16].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p10].dword
  adc EAX,0
  sub [EDX + 12].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p11].dword
  adc EAX,0
  sub [EDX + 8].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p12].dword
  adc EAX,0
  sub [EDX + 4].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p13].dword
  adc EAX,0
  sub [EDX + 0].dword,ECX
  adc EAX,0

  sub EBX,EAX

  cmp EBX,0
  jz @@3
  jl @@2
@@1:
  sub [EDX + 24].dword,1
  sbb [EDX + 20].dword,0
  sbb [EDX + 16].dword,0
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,0
  sbb EBX,1
  add [EDX + 12].dword,1
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  adc EBX,0
  test EBX,EBX
  jnz @@1
  jmp @@3
@@2:
  add [EDX + 24].dword,1
  adc [EDX + 20].dword,0
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  adc EBX,1
  sub [EDX + 12].dword,1
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,0
  sbb EBX,0
  test EBX,EBX
  jnz @@2
@@3:
  pop ESI
  pop EBX
end;

procedure RawAddP224(Src: Pointer);
asm
  mov EDX,dword [EAX + 24]
  mov ECX,01h
  sub ECX,EDX
  mov dword [EAX + 24],ECX
  mov EDX,dword [EAX + 20]
  mov ECX,0h
  sbb ECX,EDX
  mov dword [EAX + 20],ECX
  mov EDX,dword [EAX + 16]
  mov ECX,0h
  sbb ECX,EDX
  mov dword [EAX + 16],ECX
  mov EDX,dword [EAX + 12]
  mov ECX,0FFFFFFFEh
  sbb ECX,EDX
  mov dword [EAX + 12],ECX
  mov EDX,dword [EAX +  8]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX +  8],ECX
  mov EDX,dword [EAX +  4]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX +  4],ECX
  mov EDX,dword [EAX     ]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX     ],ECX
end;

procedure MPModP224(var X: PMPInteger; M: PMPInteger);
var
  L: Integer;
  R: PMPInteger;
begin
  if X^.Size < 7 then
    L := 192
  else if X^.Size <= 7 then
    L := 224
  else if (X^.Size <= 14) or
          ((X^.Size = 15) and (X^.Data[0] = 0)) or
          ((X^.Size = 16) and (X^.Data[0] = 0) and (X^.Data[1] = 0)) then
    L := 224*2
  else
    L := MPMSB(X);
  if L = 224 then
    while MPCmpOffset(X,M) >= 0 do
      MPSub(X,M)
  else if L > 224*2 then
    MPMod(X,M)
  else if L > 224 then begin
    MPRealloc(X,14);
    R := nil;
    MPRealloc(R,7);
    RawModP224(@X.Data,@R.Data);
    R^.Sign := 1;
    if (X^.Sign < 0) and (MPMSB(R) > 0) then
      RawAddP224(@R^.Data);
    MPDealloc(X);
    X := R;
  end;
end;       

procedure MPMulModP224(X, Y, M: PMPInteger; var R: PMPInteger);
var
  Sign: Integer; 
  XD, YD: Pointer;
begin
  if ((X^.Size <= 7) or ((X^.Size = 8) and (X^.Data[0] = 0))) and
     ((Y^.Size <= 7) or ((Y^.Size = 8) and (Y^.Data[0] = 0))) then begin
    MPRealloc(X,7);
    MPRealloc(Y,7);
    XD := @X^.Data;
    YD := @Y^.Data;
    if (LongInt(R) <> LongInt(X)) and (LongInt(R) <> LongInt(Y)) then
      MPRealloc(R,7);
    if R^.Size = 7 then
      RawMulP224(XD,YD,@R^.Data)
    else begin
      RawMulP224(XD,YD,@R^.Data[1]);
      MPRealloc(R,7);
    end;
    if X.Sign = Y.Sign then
      Sign := 1
    else
      Sign := -1;
    R^.Sign := 1;
    if (Sign < 0) and (MPMSB(R) > 0) then
      RawAddP224(@R^.Data);
  end else begin
    MPMul2(X,Y,R);
    MPModP224(R,M);
  end;
end;

procedure RawMulP256(Src1, Src2, Dst: Pointer);
const
  p0 = 60;
  p1 = 56;
  p2 = 52;
  p3 = 48;
  p4 = 44;
  p5 = 40;
  p6 = 36;
  p7 = 32;
  p8 = 28;
  p9 = 24;
  p10 = 20;
  p11 = 16;
  p12 = 12;
  p13 = 8;
  p14 = 4;
  p15 = 0;
asm
  push EBX
  push EDI
  push ESI
  push EBP

  sub ESP,64 + 40 + 36

  mov ESI,EAX
  mov EBP,EDX
  mov EDI,ECX
  push EDI

// ------------------------
// Low Kataratsuba product:

  mov EAX,dword [ESI + 28]
  mov EDX,dword [EBP + 28]
  mul EDX
  mov dword [ESP + 64],EAX
  mov EBX,EDX

  xor ECX,ECX
  xor EDI,EDI

  mov EAX,dword [ESI + 28]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 28]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 60],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 28]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 28]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 56],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 28]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 28]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 52],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 24]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 24]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 48],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 20]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 20]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 44],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 16]
  mov EDX,dword [EBP + 16]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0
  mov dword [ESP + 40],EBX
  mov dword [ESP + 36],ECX

// ------------------------          
// High Kataratsuba product:

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP + 12]
  mul EDX
  mov dword [ESP + 32],EAX
  mov EBX,EDX

  xor ECX,ECX
  xor EDI,EDI

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 28],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 24],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI + 12]
  mov EDX,dword [EBP +  0]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  0]
  mov EDX,dword [EBP + 12]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 20],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  8]
  mov EDX,dword [EBP +  0]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  0]
  mov EDX,dword [EBP +  8]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 16],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  4]
  mov EDX,dword [EBP +  0]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESI +  0]
  mov EDX,dword [EBP +  4]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 12],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESI +  0]
  mov EDX,dword [EBP +  0]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  mov dword [ESP +  8],EBX
  mov dword [ESP +  4],ECX

// ------------------------
// Middle Kataratsuba terms:
  xor EBX,EBX
  mov ECX,4
  clc
@@1:
  mov EAX,dword [ESI + ECX*4 - 4]
  mov EDX,dword [ESI + ECX*4 + 12]
  adc EAX,EDX
  mov dword [ESP + ECX*4 + 68],EAX
  loop @@1
  adc EBX,0
  mov dword [ESP + ECX*4 + 68],EBX
  xor EBX,EBX
  mov ECX,4
  clc
@@2:
  mov EAX,dword [EBP + ECX*4 - 4]
  mov EDX,dword [EBP + ECX*4 + 12]
  adc EAX,EDX
  mov dword [ESP + ECX*4 + 88],EAX
  loop @@2
  adc EBX,0
  mov dword [ESP + ECX*4 + 88],EBX
// ------------------------
// Middle Kataratsuba product:

  mov EAX,dword [ESP +  84]
  mov EDX,dword [ESP + 104]
  mul EDX
  mov dword [ESP + 140],EAX
  mov EBX,EDX

  xor ECX,ECX
  xor EDI,EDI

  mov EAX,dword [ESP +  84]
  mov EDX,dword [ESP + 100]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  80]
  mov EDX,dword [ESP + 104]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 136],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESP +  84]
  mov EDX,dword [ESP +  96]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  80]
  mov EDX,dword [ESP + 100]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  76]
  mov EDX,dword [ESP + 104]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 132],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESP +  84]
  mov EDX,dword [ESP +  92]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  80]
  mov EDX,dword [ESP +  96]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  76]
  mov EDX,dword [ESP + 100]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  72]
  mov EDX,dword [ESP + 104]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 128],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESP +  80]
  mov EDX,dword [ESP +  92]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  76]
  mov EDX,dword [ESP +  96]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  72]
  mov EDX,dword [ESP + 100]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 124],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESP +  76]
  mov EDX,dword [ESP +  92]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov EAX,dword [ESP +  72]
  mov EDX,dword [ESP +  96]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 120],EBX
  mov EBX,ECX
  mov ECX,EDI

  xor EDI,EDI

  mov EAX,dword [ESP +  72]
  mov EDX,dword [ESP +  92]
  mul EDX
  add EBX,EAX
  adc ECX,EDX
  adc EDI,0

  mov dword [ESP + 116],EBX
  mov EBX,ECX
  mov ECX,EDI

  mov dword [ESP + 112],EBX
  mov dword [ESP + 108],ECX

// -------------------------
// Middle Karatsuba term adjustment
  mov EAX,dword [ESP + 88]
  test EAX,EAX
  jz @@NoLoCarry
  mov ECX,5
  clc
@@LoCarry:
  mov EAX,dword [ESP + ECX*4 + 64]
  adc dword [ESP + ECX*4 + 104],EAX
  loop @@LoCarry
  xor EAX,EAX
  mov dword [ESP + 88],EAX
@@NoLoCarry:
  mov EAX,dword [ESP + 68]
  test EAX,EAX
  jz @@NoHiCarry
  mov ECX,5
  clc
@@HiCarry:
  mov EAX,dword [ESP + ECX*4 + 84]
  adc dword [ESP + ECX*4 + 104],EAX
  loop @@HiCarry
@@NoHiCarry:

  xor EAX,EAX
  xor EBX,EBX
  mov ECX,8
@@3:
  add EAX,dword [ESP + ECX*4 + 108]
  adc EBX,0
  sub EAX,dword [ESP + ECX*4 +  0]
  sbb EBX,0
  sub EAX,dword [ESP + ECX*4 + 32]
  sbb EBX,0
  mov dword [ESP + ECX*4 + 108],EAX
  mov EAX,EBX
  movsx EBX,BH
  loop @@3
  add dword [ESP + 108],EAX
  adc EBX,0

  mov ECX,9
  clc
@@4:
  mov EAX,dword [ESP + ECX*4 + 108 - 4]
  adc dword [ESP + ECX*4 + 16 - 4],EAX
  loop @@4
  adc dword [ESP + 12],EBX
  adc dword [ESP +  8],EBX
  adc dword [ESP +  4],EBX
// -------------------------

  lea ESI,[ESP + 4]
  mov EDX,[ESP]

  xor EBX,EBX
  mov ECX,[ESI + p0].dword
  add ECX,[ESI + p8].dword
  adc EBX,0
  add ECX,[ESI + p9].dword
  adc EBX,0
  mov [EDX + 28].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p1].dword
  adc EBX,0
  add ECX,[ESI + p9].dword
  adc EBX,0
  add ECX,[ESI + p10].dword
  adc EBX,0
  mov [EDX + 24].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p2].dword
  adc EBX,0
  add ECX,[ESI + p10].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  mov [EDX + 20].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p3].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  mov [EDX + 16].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p4].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p5].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  mov [EDX + 8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p6].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  mov [EDX + 4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p7].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p8].dword
  adc EBX,0
  mov [EDX + 0].dword,ECX

  //---

  xor EAX,EAX
  mov ECX,[ESI + p11].dword
  adc EAX,0
  add ECX,[ESI + p12].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  add ECX,[ESI + p14].dword
  adc EAX,0
  sub [EDX + 28].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p12].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  add ECX,[ESI + p14].dword
  adc EAX,0
  add ECX,[ESI + p15].dword
  adc EAX,0
  sub [EDX + 24].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p13].dword
  adc EAX,0
  add ECX,[ESI + p14].dword
  adc EAX,0
  add ECX,[ESI + p15].dword
  adc EAX,0
  sub [EDX + 20].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p15].dword
  adc EAX,0
  add ECX,[ESI + p8].dword
  adc EAX,0
  add ECX,[ESI + p9].dword
  adc EAX,0
  sub [EDX + 16].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p9].dword
  adc EAX,0
  add ECX,[ESI + p10].dword
  adc EAX,0
  sub [EDX + 12].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p10].dword
  adc EAX,0
  add ECX,[ESI + p11].dword
  adc EAX,0
  sub [EDX + 8].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p8].dword
  adc EAX,0
  add ECX,[ESI + p9].dword
  adc EAX,0
  sub [EDX + 4].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p10].dword
  adc EAX,0
  add ECX,[ESI + p11].dword
  adc EAX,0
  add ECX,[ESI + p12].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  sub [EDX + 0].dword,ECX
  adc EAX,0

  sub EBX,EAX

  cmp EBX,0
  jz @@7
  jl @@6
@@5:
  sub [EDX + 16].dword,1
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,1
  sbb [EDX +  0].dword,0
  sbb EBX,1
  add [EDX + 28].dword,1
  adc [EDX + 24].dword,0
  adc [EDX + 20].dword,0
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,1
  adc EBX,0
  test EBX,EBX
  jnz @@5
  jmp @@7
@@6:
  add [EDX + 16].dword,1
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,1
  adc [EDX +  0].dword,0
  adc EBX,1
  sub [EDX + 28].dword,1
  sbb [EDX + 24].dword,0
  sbb [EDX + 20].dword,0
  sbb [EDX + 16].dword,0
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,1
  sbb EBX,0
  test EBX,EBX
  jnz @@6
@@7:
  xor EAX,EAX
  mov ECX,16 + 5 + 5 + 9
  lea EDI,[ESP+4]
  cld
  rep stosd

  add ESP,4 + 64 + 40 + 36

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure RawModP256(Src: Pointer; Dst: Pointer);
const
  p0 = 60;
  p1 = 56;
  p2 = 52;
  p3 = 48;
  p4 = 44;
  p5 = 40;
  p6 = 36;
  p7 = 32;
  p8 = 28;
  p9 = 24;
  p10 = 20;
  p11 = 16;
  p12 = 12;
  p13 = 8;
  p14 = 4;
  p15 = 0;
asm
  push EBX
  push ESI

  mov ESI,EAX

  xor EBX,EBX
  mov ECX,[ESI + p0].dword
  add ECX,[ESI + p8].dword
  adc EBX,0
  add ECX,[ESI + p9].dword
  adc EBX,0
  mov [EDX + 28].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p1].dword
  adc EBX,0
  add ECX,[ESI + p9].dword
  adc EBX,0
  add ECX,[ESI + p10].dword
  adc EBX,0
  mov [EDX + 24].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p2].dword
  adc EBX,0
  add ECX,[ESI + p10].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  mov [EDX + 20].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p3].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p11].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  mov [EDX + 16].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p4].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p12].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p5].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  mov [EDX + 8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p6].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p14].dword
  adc EBX,0
  add ECX,[ESI + p13].dword
  adc EBX,0
  mov [EDX + 4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + p7].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p15].dword
  adc EBX,0
  add ECX,[ESI + p8].dword
  adc EBX,0
  mov [EDX + 0].dword,ECX

  //---

  xor EAX,EAX
  mov ECX,[ESI + p11].dword
  adc EAX,0
  add ECX,[ESI + p12].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  add ECX,[ESI + p14].dword
  adc EAX,0
  sub [EDX + 28].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p12].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  add ECX,[ESI + p14].dword
  adc EAX,0
  add ECX,[ESI + p15].dword
  adc EAX,0
  sub [EDX + 24].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p13].dword
  adc EAX,0
  add ECX,[ESI + p14].dword
  adc EAX,0
  add ECX,[ESI + p15].dword
  adc EAX,0
  sub [EDX + 20].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p15].dword
  adc EAX,0
  add ECX,[ESI + p8].dword
  adc EAX,0
  add ECX,[ESI + p9].dword
  adc EAX,0
  sub [EDX + 16].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p9].dword
  adc EAX,0
  add ECX,[ESI + p10].dword
  adc EAX,0
  sub [EDX + 12].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p10].dword
  adc EAX,0
  add ECX,[ESI + p11].dword
  adc EAX,0
  sub [EDX + 8].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p8].dword
  adc EAX,0
  add ECX,[ESI + p9].dword
  adc EAX,0
  sub [EDX + 4].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + p10].dword
  adc EAX,0
  add ECX,[ESI + p11].dword
  adc EAX,0
  add ECX,[ESI + p12].dword
  adc EAX,0
  add ECX,[ESI + p13].dword
  adc EAX,0
  sub [EDX + 0].dword,ECX
  adc EAX,0

  sub EBX,EAX

  cmp EBX,0
  jz @@3
  jl @@2
@@1:
  sub [EDX + 16].dword,1
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,1
  sbb [EDX +  0].dword,0
  sbb EBX,1
  add [EDX + 28].dword,1
  adc [EDX + 24].dword,0
  adc [EDX + 20].dword,0
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,1
  adc EBX,0
  test EBX,EBX
  jnz @@1
  jmp @@3
@@2:
  add [EDX + 16].dword,1
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,1
  adc [EDX +  0].dword,0
  adc EBX,1
  sub [EDX + 28].dword,1
  sbb [EDX + 24].dword,0
  sbb [EDX + 20].dword,0
  sbb [EDX + 16].dword,0
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,1
  sbb EBX,0
  test EBX,EBX
  jnz @@2
@@3:
  pop ESI
  pop EBX
end;

procedure RawAddP256(Src: Pointer);
asm
  mov EDX,dword [EAX + 28]
  mov ECX,0FFFFFFFFh
  sub ECX,EDX
  mov dword [EAX + 28],ECX
  mov EDX,dword [EAX + 24]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 24],ECX
  mov EDX,dword [EAX + 20]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 20],ECX
  mov EDX,dword [EAX + 16]
  mov ECX,0h
  sbb ECX,EDX
  mov dword [EAX + 16],ECX
  mov EDX,dword [EAX + 12]
  mov ECX,0h
  sbb ECX,EDX
  mov dword [EAX + 12],ECX
  mov EDX,dword [EAX +  8]
  mov ECX,0h
  sbb ECX,EDX
  mov dword [EAX +  8],ECX
  mov EDX,dword [EAX +  4]
  mov ECX,01h
  sbb ECX,EDX
  mov dword [EAX +  4],ECX
  mov EDX,dword [EAX     ]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX     ],ECX
end;

procedure MPModP256(var X: PMPInteger; M: PMPInteger);
var
  L: Integer;
  R: PMPInteger;
begin
  if X^.Size < 8 then
    L := 224
  else if X^.Size <= 8 then
    L := 256
  else if (X^.Size <= 16) or
          ((X^.Size = 17) and (X^.Data[0] = 0)) or
          ((X^.Size = 18) and (X^.Data[0] = 0) and (X^.Data[1] = 0)) then
    L := 256*2
  else
    L := MPMSB(X);
  if L = 256 then
    while MPCmpOffset(X,M) >= 0 do
      MPSub(X,M)
  else if L > 256*2 then
    MPMod(X,M)
  else if L > 256 then begin
    MPRealloc(X,16);
    R := nil;
    MPRealloc(R,8);
    RawModP256(@X.Data,@R.Data);
    R^.Sign := 1;
    if (X^.Sign < 0) and (MPMSB(R) > 0) then
      RawAddP256(@R^.Data);
    MPDealloc(X);
    X := R;
  end;
end;

procedure MPMulModP256(X, Y, M: PMPInteger; var R: PMPInteger);
var
  Sign: Integer;
  XD, YD: Pointer;
begin
  if ((X^.Size <= 8) or ((X^.Size = 9) and (X^.Data[0] = 0))) and
     ((Y^.Size <= 8) or ((Y^.Size = 9) and (Y^.Data[0] = 0))) then begin
    MPRealloc(X,8);
    MPRealloc(Y,8);
    XD := @X^.Data;
    YD := @Y^.Data;
    if (LongInt(R) <> LongInt(X)) and (LongInt(R) <> LongInt(Y)) then
      MPRealloc(R,8);
    if R^.Size = 8 then
      RawMulP256(XD,YD,@R^.Data)
    else begin
      RawMulP256(XD,YD,@R^.Data[1]);
      MPRealloc(R,8);
    end;
    if X.Sign = Y.Sign then
      Sign := 1
    else
      Sign := -1;          
    R^.Sign := 1;
    if (Sign < 0) and (MPMSB(R) > 0) then
      RawAddP256(@R^.Data);
  end else begin
    MPMul2(X,Y,R);
    MPModP256(R,M);
  end;
end;

procedure RawModP384(Src: Pointer; Dst: Pointer);
const
  P0 = 92;
  P1 = 88;
  P2 = 84;
  P3 = 80;
  P4 = 76;
  P5 = 72;
  P6 = 68;
  P7 = 64;
  P8 = 60;
  P9 = 56;
  P10 = 52;
  P11 = 48;
  P12 = 44;
  P13 = 40;
  P14 = 36;
  P15 = 32;
  P16 = 28;
  P17 = 24;
  P18 = 20;
  P19 = 16;
  P20 = 12;
  P21 = 8;
  P22 = 4;
  P23 = 0;
asm
  push EBX
  push ESI

  mov ESI,EAX

  xor EBX,EBX
  mov ECX,[ESI + P0 ].dword
  add ECX,[ESI + P12].dword
  adc EBX,0
  add ECX,[ESI + P21].dword
  adc EBX,0
  add ECX,[ESI + P20].dword
  adc EBX,0
  mov [EDX + 44].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P1 ].dword
  adc EBX,0
  add ECX,[ESI + P13].dword
  adc EBX,0
  add ECX,[ESI + P22].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  mov [EDX + 40].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P2 ].dword
  adc EBX,0
  add ECX,[ESI + P14].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  mov [EDX + 36].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P3 ].dword
  adc EBX,0
  add ECX,[ESI + P15].dword
  adc EBX,0
  add ECX,[ESI + P12].dword
  adc EBX,0
  add ECX,[ESI + P20].dword
  adc EBX,0
  add ECX,[ESI + P21].dword
  adc EBX,0
  mov [EDX + 32].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P4 ].dword
  adc EBX,0
  add ECX,[ESI + P21].dword
  adc EBX,0
  add ECX,[ESI + P21].dword
  adc EBX,0
  add ECX,[ESI + P16].dword
  adc EBX,0
  add ECX,[ESI + P13].dword
  adc EBX,0
  add ECX,[ESI + P12].dword
  adc EBX,0
  add ECX,[ESI + P20].dword
  adc EBX,0
  add ECX,[ESI + P22].dword
  adc EBX,0
  mov [EDX + 28].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P5 ].dword
  adc EBX,0
  add ECX,[ESI + P22].dword
  adc EBX,0
  add ECX,[ESI + P22].dword
  adc EBX,0
  add ECX,[ESI + P17].dword
  adc EBX,0
  add ECX,[ESI + P14].dword
  adc EBX,0
  add ECX,[ESI + P13].dword
  adc EBX,0
  add ECX,[ESI + P21].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  mov [EDX + 24].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P6 ].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  add ECX,[ESI + P18].dword
  adc EBX,0
  add ECX,[ESI + P15].dword
  adc EBX,0
  add ECX,[ESI + P14].dword
  adc EBX,0
  add ECX,[ESI + P22].dword
  adc EBX,0
  mov [EDX + 20].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P7 ].dword
  adc EBX,0
  add ECX,[ESI + P19].dword
  adc EBX,0
  add ECX,[ESI + P16].dword
  adc EBX,0
  add ECX,[ESI + P15].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  mov [EDX + 16].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P8 ].dword
  adc EBX,0
  add ECX,[ESI + P20].dword
  adc EBX,0
  add ECX,[ESI + P17].dword
  adc EBX,0
  add ECX,[ESI + P16].dword
  adc EBX,0
  mov [EDX + 12].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P9 ].dword
  adc EBX,0
  add ECX,[ESI + P21].dword
  adc EBX,0
  add ECX,[ESI + P18].dword
  adc EBX,0
  add ECX,[ESI + P17].dword
  adc EBX,0
  mov [EDX +  8].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P10].dword
  adc EBX,0
  add ECX,[ESI + P22].dword
  adc EBX,0
  add ECX,[ESI + P19].dword
  adc EBX,0
  add ECX,[ESI + P18].dword
  adc EBX,0
  mov [EDX +  4].dword,ECX

  mov ECX,EBX
  xor EBX,EBX
  add ECX,[ESI + P11].dword
  adc EBX,0
  add ECX,[ESI + P23].dword
  adc EBX,0
  add ECX,[ESI + P20].dword
  adc EBX,0
  add ECX,[ESI + P19].dword
  adc EBX,0
  mov [EDX +  0].dword,ECX


  //---

  xor EAX,EAX
  mov ECX,[ESI + P23].dword
  sub [EDX + 44].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P12].dword
  adc EAX,0
  add ECX,[ESI + P20].dword
  adc EAX,0
  sub [EDX + 40].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P13].dword
  adc EAX,0
  add ECX,[ESI + P21].dword // <-- FIPS Errata
  adc EAX,0
  sub [EDX + 36].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P14].dword
  adc EAX,0
  add ECX,[ESI + P22].dword
  adc EAX,0
  add ECX,[ESI + P23].dword
  adc EAX,0
  sub [EDX + 32].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P15].dword
  adc EAX,0
  add ECX,[ESI + P23].dword
  adc EAX,0
  add ECX,[ESI + P23].dword
  adc EAX,0
  sub [EDX + 28].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P16].dword
  adc EAX,0
  sub [EDX + 24].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P17].dword
  adc EAX,0
  sub [EDX + 20].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P18].dword
  adc EAX,0
  sub [EDX + 16].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P19].dword
  adc EAX,0
  sub [EDX + 12].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P20].dword
  adc EAX,0
  sub [EDX +  8].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P21].dword
  adc EAX,0
  sub [EDX +  4].dword,ECX
  adc EAX,0

  mov ECX,EAX
  xor EAX,EAX
  add ECX,[ESI + P22].dword
  adc EAX,0
  sub [EDX +  0].dword,ECX
  adc EAX,0

  sub EBX,EAX

  cmp EBX,0
  jz @@3
  jl @@2
@@1:
  sub [EDX + 40].dword,1
  sbb [EDX + 36].dword,0
  sbb [EDX + 32].dword,0
  sbb [EDX + 28].dword,0
  sbb [EDX + 24].dword,0
  sbb [EDX + 20].dword,0
  sbb [EDX + 16].dword,0
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,0
  sbb EBX,1
  add [EDX + 44].dword,1
  adc [EDX + 40].dword,0
  adc [EDX + 36].dword,0
  adc [EDX + 32].dword,1
  adc [EDX + 28].dword,1
  adc [EDX + 24].dword,0
  adc [EDX + 20].dword,0
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  adc EBX,0
  test EBX,EBX
  jnz @@1
  jmp @@3
@@2:
  add [EDX + 40].dword,1
  adc [EDX + 36].dword,0
  adc [EDX + 32].dword,0
  adc [EDX + 28].dword,0
  adc [EDX + 24].dword,0
  adc [EDX + 20].dword,0
  adc [EDX + 16].dword,0
  adc [EDX + 12].dword,0
  adc [EDX +  8].dword,0
  adc [EDX +  4].dword,0
  adc [EDX +  0].dword,0
  adc EBX,1
  sub [EDX + 44].dword,1
  sbb [EDX + 40].dword,0
  sbb [EDX + 36].dword,0
  sbb [EDX + 32].dword,1
  sbb [EDX + 28].dword,1
  sbb [EDX + 24].dword,0
  sbb [EDX + 20].dword,0
  sbb [EDX + 16].dword,0
  sbb [EDX + 12].dword,0
  sbb [EDX +  8].dword,0
  sbb [EDX +  4].dword,0
  sbb [EDX +  0].dword,0
  sbb EBX,0
  test EBX,EBX
  jnz @@2
@@3:
  pop ESI
  pop EBX
end;

procedure MPModP384(var X: PMPInteger; M: PMPInteger);
var
  L: Integer;
  R: PMPInteger;
begin
  L := MPMSB(X);
  if L > 384*2 then
    MPMod(X,M)
  else begin
    MPRealloc(X,24);
    R := nil;
    MPRealloc(R,12);
    RawModP384(@X.Data,@R.Data);
    R^.Sign := X^.Sign;
    if (X^.Sign < 0) and (MPMSB(R) > 0) then
      MPAdd(R,M);
    MPDealloc(X);
    X := R;
  end;
end;

procedure RawModP521(Src: Pointer; Dst: Pointer);
asm
  push EBX
  push ESI
  push EDI

  mov ESI,EAX
  mov EDI,EDX
  xor EBX,EBX
  mov ECX,16
@@1:
  mov EAX,[ESI + ECX*4].dword
  mov EDX,[ESI + ECX*4 - 4].dword
  shrd EAX,EDX,9
  xor EDX,EDX
  add EAX,EBX
  adc EDX,0
  mov EBX,EDX
  add EAX,[ESI + ECX*4 + 64].dword
  adc EBX,0
  mov [EDI + ECX*4].dword,EAX
  loop @@1

  mov EAX,[ESI + 64].dword
  mov EDX,[ESI].dword
  shr EDX,9
  and EAX,$1FF
  add EAX,EDX
  add EAX,EBX

  mov EDX,EAX
  and EAX,$1FF
  mov [EDI].dword,EAX
  shr EDX,9
  test EDX,EDX
  jz @@3
@@2:
  mov ECX,16
  add [EDI + 64].dword,EDX
@@4:
  adc [EDI + ECX*4 - 4].dword,0
  loop @@4
  mov EDX,[EDI].dword
  shr EDX,9
  test EDX,EDX
  jnz @@2
@@3:
  pop EDI
  pop ESI
  pop EBX
end;          

procedure RawAddP521(Src: Pointer);
asm
  mov EDX,dword [EAX + 64]
  mov ECX,0FFFFFFFFh
  sub ECX,EDX
  mov dword [EAX + 64],ECX
  mov EDX,dword [EAX + 60]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 60],ECX
  mov EDX,dword [EAX + 56]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 56],ECX
  mov EDX,dword [EAX + 52]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 52],ECX
  mov EDX,dword [EAX + 48]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 48],ECX
  mov EDX,dword [EAX + 44]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 44],ECX
  mov EDX,dword [EAX + 40]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 40],ECX
  mov EDX,dword [EAX + 36]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 36],ECX
  mov EDX,dword [EAX + 32]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 32],ECX
  mov EDX,dword [EAX + 28]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 28],ECX
  mov EDX,dword [EAX + 24]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 24],ECX
  mov EDX,dword [EAX + 20]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 20],ECX
  mov EDX,dword [EAX + 16]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 16],ECX
  mov EDX,dword [EAX + 12]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX + 12],ECX
  mov EDX,dword [EAX +  8]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX +  8],ECX
  mov EDX,dword [EAX +  4]
  mov ECX,0FFFFFFFFh
  sbb ECX,EDX
  mov dword [EAX +  4],ECX
  mov EDX,dword [EAX     ]
  mov ECX,01FFh
  sbb ECX,EDX
  mov dword [EAX     ],ECX
end;

procedure MPModP521(var X: PMPInteger; M: PMPInteger);
var
  L: Integer;
  R: PMPInteger;
begin
  L := MPMSB(X);
  if (L > 521*2) or (X^.Sign < 0) then
    MPMod(X,M)
  else begin
    MPRealloc(X,33);
    R := nil;
    MPRealloc(R,17);
    RawModP521(@X.Data,@R.Data);
    R^.Sign := X^.Sign;
    if (X^.Sign < 0) and (MPMSB(R) > 0) then
      RawAddP521(@R^.Data);
    MPDealloc(X);
    X := R;
  end;
end;

procedure MPMulModP521(X, Y, M: PMPInteger; var R: PMPInteger);
begin
  MPMul2(X,Y,R);
  MPModP521(R,M);
end;

procedure ECurveP192(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFE,$FFFFFFFF,
                          $FFFFFFFF]);
  C.R := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$99DEF836,$146BC9B1,
                          $B4D22831]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  MPMod(C.A,C.Q);
  C.B := IntegersToMPInt([$64210519,$e59c80e7,$0fa7e9ab,$72243049,$feb8deec,
                          $c146b9b1]);
  C.G.X := IntegersToMPInt([$188da80e,$b03090f6,$7cbf20eb,$43a18800,$f4ff0afd,
                            $82ff1012]);
  C.G.Y := IntegersToMPInt([$07192b95,$ffc8da78,$631011ed,$6b24cdd5,$73f977a1,
                            $1e794811]);
  C.ModProc := MPModP192;
  C.MulModProc := MPMulModP192;
end;

procedure ECurveP224(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$00000000,
                          $00000000,$00000001]);
  C.R := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFF16A2,$E0B8F03E,
                          $13DD2945,$5C5C2A3D]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);  
  MPMod(C.A,C.Q);
  C.B := IntegersToMPInt([$b4050a85,$0c04b3ab,$f5413256,$5044b0b7,$d7bfd8ba,
                          $270b3943,$2355ffb4]);
  C.G.X := IntegersToMPInt([$b70e0cbd,$6bb4bf7f,$321390b9,$4a03c1d3,$56c21122,
                            $343280d6,$115c1d21]);
  C.G.Y := IntegersToMPInt([$bd376388,$b5f723fb,$4c22dfe6,$cd4375a0,$5a074764,
                            $44d58199,$85007e34]);
  C.ModProc := MPModP224;
  C.MulModProc := MPMulModP224;
end;

procedure ECurveP256(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$FFFFFFFF,$00000001,$00000000,$00000000,$00000000,
                          $FFFFFFFF,$FFFFFFFF,$FFFFFFFF]);
  C.R := IntegersToMPInt([$FFFFFFFF,$00000000,$FFFFFFFF,$FFFFFFFF,$BCE6FAAD,
                          $A7179E84,$F3B9CAC2,$FC632551]);  
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);  
  MPMod(C.A,C.Q);
  C.B := IntegersToMPInt([$5ac635d8,$aa3a93e7,$b3ebbd55,$769886bc,$651d06b0,
                          $cc53b0f6,$3bce3c3e,$27d2604b]);
  C.G.X := IntegersToMPInt([$6b17d1f2,$e12c4247,$f8bce6e5,$63a440f2,$77037d81,
                            $2deb33a0,$f4a13945,$d898c296]);
  C.G.Y := IntegersToMPInt([$4fe342e2,$fe1a7f9b,$8ee7eb4a,$7c0f9e16,$2bce3357,
                            $6b315ece,$cbb64068,$37bf51f5]);
  C.ModProc := MPModP256;
  C.MulModProc := MPMulModP256;
//  C.MulModProc := nil;
end;

procedure ECurveP384(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
                          $FFFFFFFF,$FFFFFFFF,$FFFFFFFE,$FFFFFFFF,$00000000,
                          $00000000,$FFFFFFFF]);
  C.R := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
                          $FFFFFFFF,$C7634D81,$F4372DDF,$581A0DB2,$48B0A77A,
                          $ECEC196A,$CCC52973]);     
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);  
  MPMod(C.A,C.Q);
  C.B := IntegersToMPInt([$b3312fa7,$e23ee7e4,$988e056b,$e3f82d19,$181d9c6e,
                          $fe814112,$0314088f,$5013875a,$c656398d,$8a2ed19d,
                          $2a85c8ed,$d3ec2aef]);
  C.G.X := IntegersToMPInt([$aa87ca22,$be8b0537,$8eb1c71e,$f320ad74,$6e1d3b62,
                            $8ba79b98,$59f741e0,$82542a38,$5502f25d,$bf55296c,
                            $3a545e38,$72760ab7]);
  C.G.Y := IntegersToMPInt([$3617de4a,$96262c6f,$5d9e98bf,$9292dc29,$f8f41dbd,
                            $289a147c,$e9da3113,$b5f0b8c0,$0a60b1ce,$1d7e819d,
                            $7a431d7c,$90ea0e5f]);
  C.ModProc := MPModP384; 
  C.MulModProc := nil;
end;

procedure ECurveP521(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$000001FF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
                          $FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
                          $FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
                          $FFFFFFFF,$FFFFFFFF]);
  C.R := IntegersToMPInt([$000001FF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
                          $FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFA,$51868783,
                          $BF2F966B,$7FCC0148,$F709A5D0,$3BB5C9B8,$899C47AE,
                          $BB6FB71E,$91386409]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  MPMod(C.A,C.Q);
  C.B := IntegersToMPInt([$00000051,$953eb961,$8e1c9a1f,$929a21a0,$b68540ee,
                          $a2da725b,$99b315f3,$b8b48991,$8ef109e1,$56193951,
                          $ec7e937b,$1652c0bd,$3bb1bf07,$3573df88,$3d2c34f1,
                          $ef451fd4,$6b503f00]);
  C.G.X := IntegersToMPInt([$c6,$858e06b7,$0404e9cd,$9e3ecb66,$2395b442,
                            $9c648139,$053fb521,$f828af60,$6b4d3dba,$a14b5e77,
                            $efe75928,$fe1dc127,$a2ffa8de,$3348b3c1,$856a429b,
                            $f97e7e31,$c2e5bd66]);
  C.G.Y := IntegersToMPInt([$118,$39296a78,$9a3bc004,$5c8a5fb4,$2c7d1bd9,
                            $98f54449,$579b4468,$17afbd17,$273e662c,$97ee7299,
                            $5ef42640,$c550b901,$3fad0761,$353c7086,$a272c240,
                            $88be9476,$9fd16650]);
  C.ModProc := MPModP521;
//  C.MulModProc := MPMulModP521;
  C.MulModProc := nil;
end;

function IsECurveP192(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      C.MulModProc := vC.MulModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP224(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP224(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP256(var C: TECurve): Boolean;    
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP256(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP384(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP384(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP521(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP521(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP224(const C: TECurve): Boolean;    
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP224(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP384(const C: TECurve): Boolean;  
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP384(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP521(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP521(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

end.
