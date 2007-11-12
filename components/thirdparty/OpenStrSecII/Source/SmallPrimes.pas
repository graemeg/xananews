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
}
{$I ver.inc}
{$IFDEF STATIC_PRIMES}
{$WEAKPACKAGEUNIT ON}
{$ENDIF}
unit SmallPrimes;

interface

{$IFDEF STATIC_PRIMES}
{$I SmallPrimes.inc}
{$ELSE}
const
  SPrimeMaxCount = 6541;
  SPrimeBound = $10000;
  SPrimeBoundBitSize = 16;
var
  Primes: array [0..SPrimeMaxCount-1] of LongWord;
  SPrimeCount: Cardinal = 0;

{$IFNDEF INIT_SECTIONS}
procedure GenPrimes;
{$ENDIF}
{$ENDIF}

implementation

{$IFNDEF STATIC_PRIMES}
procedure GenPrimes;
const
  SqrtPrimeCount = 171;
var
  P, Q, R: LongWord;
  PrimeSqr: array [0..SqrtPrimeCount-1] of LongWord;
  I, J: Integer;
  OK: Boolean;
  Res: array [0..SqrtPrimeCount-1] of LongWord;
begin
  Primes[0] := 3;
  PrimeSqr[0] := 9;
  P := 3;
  for I := 1 to SqrtPrimeCount-1 do begin
    repeat
      Inc(P,2);
      OK := True;
      for J := 0 to I-1 do begin
        if PrimeSqr[J] > P then Break;
        Q := Primes[J];
        OK := (P mod Q) > 0;
        if not OK then Break;
      end;
    until OK;
    Primes[I] := P;
    PrimeSqr[I] := P*P;
  end;
  for J := 0 to SqrtPrimeCount-1 do
    Res[J] := P mod Primes[J];
  SPrimeCount := SPrimeMaxCount;
  for I := SqrtPrimeCount to SPrimeMaxCount-1 do begin
    repeat
      Inc(P,2);
      OK := True;
      for J := 0 to SqrtPrimeCount-1 do begin
        Q := Primes[J];
        R := Res[J];
        Inc(R,2);
        if R >= Q then R := R - Q;
        Res[J] := R;
        if R = 0 then
          OK := False;
      end;
    until OK;
    Primes[I] := P;
    if P > SPrimeBound then begin
      SPrimeCount := I;
      Exit;
    end;
  end;
end;
{$ENDIF}

{$IFNDEF STATIC_PRIMES}
{$IFDEF INIT_SECTIONS}
initialization
  GenPrimes;
{$ENDIF}
{$ENDIF}
end.
