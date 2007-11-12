{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPEC_X9_62Curves Unit                             }
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
unit MpEC_X9_62Curves;

interface

uses
  MpArithTypes;

procedure ECurveP192_1(var C: TECurve);
procedure ECurveP192_2(var C: TECurve);
procedure ECurveP192_3(var C: TECurve);
procedure ECurveP239_1(var C: TECurve);
procedure ECurveP239_2(var C: TECurve);
procedure ECurveP239_3(var C: TECurve);
procedure ECurveP256_1(var C: TECurve);

function IsECurveP192_1(var C: TECurve): Boolean;
function IsECurveP192_2(var C: TECurve): Boolean;
function IsECurveP192_3(var C: TECurve): Boolean;
function IsECurveP239_1(var C: TECurve): Boolean;
function IsECurveP239_2(var C: TECurve): Boolean;
function IsECurveP239_3(var C: TECurve): Boolean;
function IsECurveP256_1(var C: TECurve): Boolean;

function TryIsECurveP192_1(const C: TECurve): Boolean;
function TryIsECurveP192_2(const C: TECurve): Boolean;
function TryIsECurveP192_3(const C: TECurve): Boolean;
function TryIsECurveP239_1(const C: TECurve): Boolean;
function TryIsECurveP239_2(const C: TECurve): Boolean;
function TryIsECurveP239_3(const C: TECurve): Boolean;
function TryIsECurveP256_1(const C: TECurve): Boolean;

implementation

uses
  MpArith, MpECArith, MpEC_NISTCurves;

procedure ECurveP192_1(var C: TECurve);
begin
  ECurveP192(C);
end;

procedure ECurveP192_2(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFE,$FFFFFFFF,
                          $FFFFFFFF]);
  C.R := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFE,$5FB1A724,$DC804186,
                          $48D8DD31]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  C.B := IntegersToMPInt([$CC22D6DF,$B95C6B25,$E49C0D63,$64A4E598,$0C393AA2,
                          $1668D953]);
  C.G.X := IntegersToMPInt([$EEA2BAE7,$E1497842,$F2DE7769,$CFE9C989,$C072AD69,
                            $6F48034A]);
  C.G.Y := IntegersToMPInt([$6574D11D,$69B6EC7A,$672BB82A,$083DF2F2,$B0847DE9,
                            $70B2DE15]);
  C.ModProc := MPModP192;
end;

procedure ECurveP192_3(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFE,$FFFFFFFF,
                          $FFFFFFFF]);
  C.R := IntegersToMPInt([$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$7A62D031,$C83F4294,
                          $F640EC13]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  C.B := IntegersToMPInt([$22123DC2,$395A05CA,$A7423DAE,$CCC94760,$A7D46225,
                          $6BD56916]);
  C.G.X := IntegersToMPInt([$7D297781,$00C65A1D,$A1783716,$588DCE2B,$8B4AEE8E,
                            $228F1896]);
  C.G.Y := IntegersToMPInt([$38A90F22,$63733733,$4B49DCB6,$6A6DC8F9,$978ACA76,
                            $48A943B0]);
  C.ModProc := MPModP192;
end;

procedure ECurveP239_1(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$7FFF,$FFFFFFFF,$FFFFFFFF,$FFFF7FFF,$FFFFFFFF,
                          $80000000,$00007FFF,$FFFFFFFF]);
  C.R := IntegersToMPInt([$7FFF,$FFFFFFFF,$FFFFFFFF,$FFFF7FFF,$FF9E5E9A,
                          $9F5D9071,$FBD15226,$88909D0B]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  C.B := IntegersToMPInt([$6B01,$6C3BDCF1,$8941D0D6,$54921475,$CA71A9DB,
                          $2FB27D1D,$37796185,$C2942C0A]);
  C.G.X := IntegersToMPInt([$0FFA,$963CDCA8,$816CCC33,$B8642BED,$F905C3D3,
                            $58573D3F,$27FBBD3B,$3CB9AAAF]);
  C.G.Y := IntegersToMPInt([$7DEB,$E8E4E90A,$5DAE6E40,$54CA530B,$A04654B3,
                            $6818CE22,$6B39FCCB,$7B02F1AE]);
end;

procedure ECurveP239_2(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$7FFF,$FFFFFFFF,$FFFFFFFF,$FFFF7FFF,$FFFFFFFF,
                          $80000000,$00007FFF,$FFFFFFFF]);
  C.R := IntegersToMPInt([$7FFF,$FFFFFFFF,$FFFFFFFF,$FFFF8000,$00CFA7E8,
                          $594377D4,$14C03821,$BC582063]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  C.B := IntegersToMPInt([$617F,$AB683257,$6CBBFED5,$0D99F024,$9C3FEE58,
                          $B94BA003,$8C7AE84C,$8C832F2C]);
  C.G.X := IntegersToMPInt([$38AF,$09D98727,$705120C9,$21BB5E9E,$26296A3C,
                            $DCF2F357,$57A0EAFD,$87B830E7]);
  C.G.Y := IntegersToMPInt([$5B01,$25E4DBEA,$0EC7206D,$A0FC01D9,$B081329F,
                            $B555DE6E,$F460237D,$FF8BE4BA]);
end;

procedure ECurveP239_3(var C: TECurve);
begin
  ECDealloc(C);
  C.Q := IntegersToMPInt([$7FFF,$FFFFFFFF,$FFFFFFFF,$FFFF7FFF,$FFFFFFFF,
                          $80000000,$00007FFF,$FFFFFFFF]);
  C.R := IntegersToMPInt([$7FFF,$FFFFFFFF,$FFFFFFFF,$FFFF7FFF,$FF975DEB,
                          $41B3A605,$7C3C4321,$46526551]);
  C.K := IntToMPInt(1);
  C.A := IntToMPInt(-3);
  C.B := IntegersToMPInt([$2557,$05FA2A30,$6654B1F4,$CB03D6A7,$50A30C25,
                          $0102D498,$8717D9BA,$15AB6D3E]);
  C.G.X := IntegersToMPInt([$6768,$AE8E18BB,$92CFCF00,$5C949AA2,$C6D94853,
                            $D0E660BB,$F854B1C9,$505FE95A]);
  C.G.Y := IntegersToMPInt([$1607,$E6898F39,$0C06BC1D,$552BAD22,$6F3B6FCF,
                            $E48B6E81,$8499AF18,$E3ED6CF3]);
end;

procedure ECurveP256_1(var C: TECurve);
begin
  ECurveP256(C);
end;

function IsECurveP192_1(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192_1(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP192_2(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192_2(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP192_3(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192_3(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP239_1(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP239_1(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP239_2(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP239_2(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP239_3(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP239_3(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function IsECurveP256_1(var C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP256_1(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      C.ModProc := vC.ModProc;
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP192_1(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192_1(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP192_2(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192_2(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP192_3(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP192_3(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP239_1(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP239_1(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP239_2(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP239_2(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP239_3(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP239_3(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

function TryIsECurveP256_1(const C: TECurve): Boolean;
var
  vC: TECurve;
begin
  FillChar(vC,SizeOf(vC),0);
  try
    ECurveP256_1(vC);
    if MPCmpOffset(vC.Q,C.Q) = 0 then begin
      Result := ECCompare(C,vC);
    end else
      Result := False;
  finally
    ECDealloc(vC);
  end;
end;

end.
