{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPECArith Unit                                    }
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
unit MpECArith;

interface

uses
  MpArithTypes;

procedure ECDealloc(var Point: TECPoint); overload;
procedure ECDealloc(var Curve: TECurve); overload;

procedure ECCopy2(const P0: TECPoint; var P1: TECPoint); overload;
procedure ECCopy2(const C0: TECurve; var C1: TECurve); overload;

function ECCompare(const C0: TECurve; const C1: TECurve): Boolean;

function ECIsInf(const P: TECPoint; const C: TECurve): Boolean;  
function ECIsInfProj(const P: TECPoint; const C: TECurve): Boolean;
                                                                 
function ECOnCurve(const P: TECPoint; const C: TECurve): Boolean;

function ECCompressedBit(const P: TECPoint): Boolean;
function ECDecompress(CompressedBit: Boolean; var P: TECPoint; C: TECurve): Boolean;
                                                    
procedure ECInf(var P: TECPoint; C: TECurve);        
procedure ECInfProj(var P: TECPoint; C: TECurve);

procedure ECAffineToProjective(var P: TECPoint; const C: TECurve);
procedure ECProjectiveToAffine(var P: TECPoint; const C: TECurve);
                    
procedure ECFullAddition(var P0: TECPoint; const P1: TECPoint; const C: TECurve); 
procedure ECFullSubtraction(var P0: TECPoint; const P1: TECPoint; const C: TECurve);

procedure ECDoubleProj(var P: TECPoint; const C: TECurve);  
procedure ECFullAdditionProj(var P0: TECPoint; const P1: TECPoint; const C: TECurve);  
procedure ECFullSubtractionProj(var P0: TECPoint; const P1: TECPoint; const C: TECurve);

procedure ECScalarMul(const P: TECPoint; N: PMPInteger; const C: TECurve; var S: TECPoint); 
procedure ECScalarMulW(const P: TECPoint; N: PMPInteger; const C: TECurve; var S: TECPoint);

implementation

uses
  MpArith, MpYarrow;

procedure ECDealloc(var Point: TECPoint);
var
  X, Y, Z: PMPInteger;
begin
  X := Point.X;
  Y := Point.Y;
  Z := Point.Z;
  Point.X := nil;
  Point.Y := nil;
  Point.Z := nil;
  MPDealloc(X);
  MPDealloc(Y);
  MPDealloc(Z);
end;

procedure ECDealloc(var Curve: TECurve);
var
  X, Y, Q, R, K: PMPInteger;
begin
  X := Curve.A;
  Y := Curve.B;
  Q := Curve.Q;
  R := Curve.R;
  K := Curve.K;
  Curve.A := nil;
  Curve.B := nil;
  Curve.Q := nil;
  Curve.R := nil;
  Curve.K := nil;
  Curve.ModProc := nil;
  Curve.MulModProc := nil;
  ECDealloc(Curve.G);
  MPDealloc(X);
  MPDealloc(Y);
  MPDealloc(Q);
  MPDealloc(R);
  MPDealloc(K);
end;

procedure ECCopy2(const P0: TECPoint; var P1: TECPoint);
begin
  MPCopy2(P0.X,P1.X);
  MPCopy2(P0.Y,P1.Y);
  if Assigned(P0.Z) then
    MPCopy2(P0.Z,P1.Z)
  else begin
    MPDealloc(P1.Z);
    P1.Z := nil;
  end;
end;

procedure ECCopy2(const C0: TECurve; var C1: TECurve);
begin
  MPCopy2(C0.Q,C1.Q);
  MPCopy2(C0.R,C1.R);
  MPCopy2(C0.K,C1.K);
  MPCopy2(C0.A,C1.A);
  MPCopy2(C0.B,C1.B);
  ECCopy2(C0.G,C1.G);
  C1.ModProc := C0.ModProc;
end;

function ECCompare(const C0: TECurve; const C1: TECurve): Boolean;
begin
  Result := (MPCmpOffset(C0.A,C1.A) = 0);
  Result := Result and (MPCmpOffset(C0.B,C1.B) = 0);
  Result := Result and (MPCmpOffset(C0.Q,C1.Q) = 0);
  Result := Result and (MPCmpOffset(C0.R,C1.R) = 0);
  Result := Result and Assigned(C0.G.X) and Assigned(C1.G.X) and
                       (MPCmpOffset(C0.G.X,C1.G.X) = 0);
  Result := Result and Assigned(C0.G.Y) and Assigned(C1.G.Y) and
                       (MPCmpOffset(C0.G.Y,C1.G.Y) = 0);
end;

function ECIsInf(const P: TECPoint; const C: TECurve): Boolean;
begin
  Result := MPMSB(P.X) = 0;
  if Result then begin
    if MPMSB(C.B) = 0 then
      Result := MPMSB(P.Y) = 1
    else
      Result := MPMSB(P.Y) = 0
  end;
end;

function ECOnCurve(const P: TECPoint; const C: TECurve): Boolean;
var
  A, B: PMPInteger;
  ModProc: TMPModProc;
begin
  Result := not ECIsInf(P,C);
  if Result then begin
    Result := (MPMSB(P.X) >= 0) and (MPCmpOffset(P.X,C.Q) < 0) and
              (MPMSB(P.Y) >= 0) and (MPCmpOffset(P.Y,C.Q) < 0) and
              (P.X^.Sign >= 0) and (P.Y^.Sign >= 0);
    if Result then begin
      ModProc := C.ModProc;
      if not Assigned(ModProc) then
        ModProc := MPMod;
      A := nil;
      B := nil;

      MPMul2(P.X,P.X,A); ModProc(A,C.Q);
      MPMul(A,P.X); ModProc(A,C.Q);

      MPMul2(P.X,C.A,B); ModProc(B,C.Q);
      MPAdd(A,B);
      MPAdd(A,C.B); ModProc(A,C.Q);

      MPMul2(P.Y,P.Y,B); ModProc(B,C.Q);

      Result := MPCmpOffset(A,B) = 0;

      MPDealloc(A);
      MPDealloc(B);
    end;
  end;
end;

function ECCompressedBit(const P: TECPoint): Boolean;
begin
  Result := Odd(P.Y^.Data[P.Y^.Size-1]);
end;

function ECDecompress(CompressedBit: Boolean; var P: TECPoint; C: TECurve): Boolean;
var
  A, B: PMPInteger;    
  ModProc: TMPModProc;
begin     
  ModProc := C.ModProc;
  if not Assigned(ModProc) then
    ModProc := MPMod;
  A := nil;
  B := nil;

  MPMul2(P.X,P.X,A); ModProc(A,C.Q);
  MPMul(A,P.X); ModProc(A,C.Q);

  MPMul2(P.X,C.A,B); ModProc(B,C.Q);
  MPAdd(A,B);
  MPAdd(A,C.B); ModProc(A,C.Q);

  Result := MPSqrtModPrime2(A,C.Q,B);
  if Result then begin
    if CompressedBit xor Odd(B^.Data[B^.Size-1]) then
      MPSub2(C.Q,B,P.Y)
    else
      MPCopy2(B,P.Y);
  end;

  MPDealloc(A);
  MPDealloc(B);
end;

function ECIsInfProj(const P: TECPoint; const C: TECurve): Boolean;
begin
  Result := MPMSB(P.Z) = 0;
end;

procedure ECInf(var P: TECPoint; C: TECurve);
begin
  MPRealloc(P.X,1);
  MPRealloc(P.Y,1);
  P.X^.Sign := 0;
  P.X^.Data[0] := 0;
  if MPMSB(C.B) = 0 then begin
    P.Y^.Sign := 0;
    P.Y^.Data[0] := 1;
  end else begin
    P.Y^.Sign := 0;
    P.Y^.Data[0] := 0;
  end;
end;

procedure ECInfProj(var P: TECPoint; C: TECurve);
begin
  MPRealloc(P.X,1);
  MPRealloc(P.Y,1);
  MPRealloc(P.Z,1);
  P.X^.Sign := 1;
  P.X^.Data[0] := 1;
  P.Y^.Sign := 1;
  P.Y^.Data[0] := 1;
  P.Z^.Sign := 0;
  P.Z^.Data[0] := 0;
end;

procedure ECAffineToProjective(var P: TECPoint; const C: TECurve);
begin
  if ECIsInf(P,C) then
    ECInfProj(P,C)
  else begin
    MPRealloc(P.Z,1);
    P.Z^.Data[0] := 1;
    P.Z^.Sign := 1;
  end;
end;

procedure ECProjectiveToAffine(var P: TECPoint; const C: TECurve);
var
  Z2: PMPInteger;
begin
  if ECIsInfProj(P,C) then
    ECInf(P,C)
  else begin
    Z2 := nil;
    try
      MPMulMod2(P.Z,P.Z,C.Q,Z2);
      MPDivMod(P.X,Z2,C.Q);
      MPMulMod(Z2,P.Z,C.Q);
      MPDivMod(P.Y,Z2,C.Q);
    finally
      MPDealloc(Z2);
    end;
    MPDealloc(P.Z);
    P.Z := nil;
  end;
end;

procedure ECFullAddition(var P0: TECPoint; const P1: TECPoint; const C: TECurve);
var
  L, D, X: PMPInteger;
begin
  if ECIsInf(P0,C) then
    ECCopy2(P1,P0)
  else if not ECIsInf(P1,C) then begin
    L := nil;
    try
      if MPCmpOffset(P0.X,P1.X) <> 0 then begin
        MPSub2(P0.Y,P1.Y,L);
        D := nil;
        try
          MPSub2(P0.X,P1.X,D);
          MPDivMod(L,D,C.Q);
        finally
          MPDealloc(D);
        end;
      end else begin
        if MPCmpOffset(P0.Y,P1.Y) <> 0 then
          ECInf(P0,C)
        else if MPMSB(P0.Y) = 0 then
          ECInf(P0,C)
        else begin
          MPMul2(P1.X,P1.X,L);
          MPMulByInt(L,3);
          MPAdd(L,C.A);
          D := nil;
          try
            MPAdd2(P1.Y,P1.Y,D);
            MPDivMod(L,D,C.Q);
          finally
            MPDealloc(D);
          end;
        end;
      end;
      if L <> nil then begin
        D := nil;
        try
          MPMul2(L,L,D);
          MPSub(D,P0.X);
          MPSub(D,P1.X);
          X := nil;
          MPMod2(D,C.Q,X);
          MPSub2(P1.X,X,D);
          MPDealloc(P0.X);
          P0.X := X;
          MPMul(D,L);
          MPSub(D,P1.Y);
          MPMod2(D,C.Q,P0.Y);
        finally
          MPDealloc(D);
        end;
      end;
    finally
      MPDealloc(L);
    end;
  end;
end;

procedure ECNeg(var P: TECPoint);
begin
  P.Y^.Sign := -P.Y^.Sign;
end;

procedure ECFullSubtraction(var P0: TECPoint; const P1: TECPoint; const C: TECurve);
begin       
  P1.Y^.Sign := -P1.Y^.Sign;
  ECFullAddition(P0,P1,C);
  P1.Y^.Sign := -P1.Y^.Sign;
end;

procedure ECDoubleProj(var P: TECPoint; const C: TECurve);
var
  T1,T2,T3,T4,T5,Q: PMPInteger;
  ModProc: TMPModProc;         
  MulModProc: TMPMulModProc;
begin
  Assert(Assigned(P.Z));
  if (MPMSB(P.Y) = 0) or (MPMSB(P.Z) = 0) then
    ECInfProj(P,C)
  else begin
    if Assigned(C.ModProc) then
      ModProc := C.ModProc
    else
      ModProc := MPMod;
    MulModProc := C.MulModProc;
    Q := C.Q;
    T1 := P.X;
    T2 := P.Y;
    T3 := P.Z;
    T4 := nil;
    T5 := nil;
    if Assigned(MulModProc) then begin
      if (C.A^.Sign = -1) and (C.A^.Size = 1) and (C.A^.Data[0] = 3) then begin
        MulModProc(T3,T3,Q,T4);
        MPSub2(T1,T4,T5); ModProc(T5,Q);
        MPAdd(T4,T1); ModProc(T4,Q);
        MulModProc(T5,T4,Q,T5);
        MPMulByInt2(T5,3,T4); ModProc(T4,Q);
      end else begin
        MPCopy2(C.A,T4);
        MulModProc(T3,T3,Q,T5);
        MulModProc(T5,T5,Q,T5);
        MulModProc(T5,T4,Q,T5);
        MulModProc(T1,T1,Q,T4);
        MPMulByInt(T4,3); ModProc(T4,Q);
        MPAdd(T4,T5); ModProc(T4,Q);
      end;
      MulModProc(T3,T2,Q,T3);
      MPAdd(T3,T3); ModProc(T3,Q);
      MulModProc(T2,T2,Q,T2);
      MulModProc(T1,T2,Q,T5);
      MPShl(T5,2); ModProc(T5,Q);
      MulModProc(T4,T4,Q,T1);
      MPSub(T1,T5); MPSub(T1,T5); ModProc(T1,Q);
      MulModProc(T2,T2,Q,T2);
      MPShl(T2,3); ModProc(T2,Q);
      MPSub(T5,T1); ModProc(T5,Q);
      MulModProc(T5,T4,Q,T5);
      MPSub2(T5,T2,T2);
    end else begin
      if (C.A^.Sign = -1) and (C.A^.Size = 1) and (C.A^.Data[0] = 3) then begin
        MPMul2(T3,T3,T4); ModProc(T4,Q);
        MPSub2(T1,T4,T5); ModProc(T5,Q);
        MPAdd(T4,T1); ModProc(T4,Q);
        MPMul(T5,T4); ModProc(T5,Q);
        MPMulByInt2(T5,3,T4); ModProc(T4,Q);
      end else begin
        MPCopy2(C.A,T4);
        MPMul2(T3,T3,T5); ModProc(T5,Q);
        MPMul(T5,T5); ModProc(T5,Q);
        MPMul(T5,T4); ModProc(T5,Q);
        MPMul2(T1,T1,T4); ModProc(T4,Q);
        MPMulByInt(T4,3); ModProc(T4,Q);
        MPAdd(T4,T5); ModProc(T4,Q);
      end;
      MPMul(T3,T2); ModProc(T3,Q);
      MPAdd(T3,T3); ModProc(T3,Q);
      MPMul(T2,T2); ModProc(T2,Q);
      MPMul2(T1,T2,T5); ModProc(T5,Q);
      MPShl(T5,2); ModProc(T5,Q);
      MPMul2(T4,T4,T1); ModProc(T1,Q);
      MPSub(T1,T5); MPSub(T1,T5); ModProc(T1,Q);
      MPMul(T2,T2); ModProc(T2,Q);
      MPShl(T2,3); ModProc(T2,Q);
      MPSub(T5,T1); ModProc(T5,Q);
      MPMul(T5,T4); ModProc(T5,Q);
      MPSub2(T5,T2,T2);
    end;
    P.X := T1;
    P.Y := T2;
    P.Z := T3;
    MPDealloc(T4);
    MPDealloc(T5);
  end;
end;

function ECAddProj(var P0: TECPoint; const P1: TECPoint; const C: TECurve): Boolean;
var
  T1,T2,T3,T4,T5,T6,T7,Q: PMPInteger;
  ModProc: TMPModProc;
  MulModProc: TMPMulModProc;
begin
  Result := True;
  if Assigned(C.ModProc) then
    ModProc := C.ModProc
  else
    ModProc := MPMod;
  MulModProc := C.MulModProc;
  T7 := nil;
  Q := C.Q;
  T1 := MPCopy(P0.X);                               // 1. T1 ¬ X0 = U0 (if Z1 = 1)
  T2 := MPCopy(P0.Y);                               // 2. T2 ¬ Y0 = S0 (if Z1 = 1)
  T3 := MPCopy(P0.Z);                               // 3. T3 ¬ Z0
  T4 := MPCopy(P1.X);                               // 4. T4 ¬ X1
  T5 := MPCopy(P1.Y);                               // 5. T5 ¬ Y1
  T6 := nil;
  if Assigned(MulModProc) then begin
    if (MPMSB(P1.Z) <> 1) or
       (P1.Z.Sign <> 1) then begin                    // 6. If Z1 ¹ 1 then
      T6 := P1.Z;                                     //   T6 ¬ Z1
      MulModProc(T6,T6,Q,T7);                         //   T7 ¬ T 6 2
      MulModProc(T1,T7,Q,T1);                         //   T1 ¬ T1 ´ T7 = U0 (if Z1 ¹ 1)
      MulModProc(T7,T6,Q,T7);                         //   T7 ¬ T6 ´ T7
      MulModProc(T2,T7,Q,T2);                         //   T2 ¬ T2 ´ T7 = S0 (if Z1 ¹ 1)
    end;
    MulModProc(T3,T3,Q,T7);                           // 7. T7 ¬ T 3 2
    MulModProc(T4,T7,Q,T4);                           // 8. T4 ¬ T4 ´ T7 = U1
    MulModProc(T7,T3,Q,T7);                           // 9. T7 ¬ T3 ´ T7
    MulModProc(T5,T7,Q,T5);                           // 10. T5 ¬ T5 ´ T7 = S1
    MPSub2(T1,T4,T4); ModProc(T4,Q);                  // 11. T4 ¬ T1 – T4 = W
    MPSub2(T2,T5,T5); ModProc(T5,Q);                  // 12. T5 ¬ T2 – T5 = R
    if MPMSB(T4) = 0 then begin                       // 13. If T4 = 0 then
      if MPMSB(T5) = 0 then                           // If T5 = 0 then output (0,0,0) and stop
        Result := False
      else                                            // else output (1, 1, 0) and stop
        ECInfProj(P0,C);
      MPDealloc(T1);
      MPDealloc(T2);
      MPDealloc(T3);
    end else begin
      MPAdd(T1,T1); MPSub(T1,T4); ModProc(T1,Q);      // 14. T1 ¬ 2 ´ T1 – T4 = T
      MPAdd(T2,T2); MPSub(T2,T5); ModProc(T2,Q);      // 15. T2 ¬ 2 ´ T2 – T5 = M
      if (MPMSB(P1.Z) <> 1) or
         (P1.Z^.Sign <> 1) then begin                 // 16. If Z1 ¹ 1 then
        MPMul(T3,T6); ModProc(T3,Q);                  // T3 ¬ T3 ´ T6
      end;
      MulModProc(T3,T4,Q,T3);                         // 17. T3 ¬ T3 ´ T4 = Z2
      MulModProc(T4,T4,Q,T7);                         // 18. T7 ¬ T 4 2
      MulModProc(T4,T7,Q,T4);                         // 19. T4 ¬ T4 ´ T7
      MulModProc(T7,T1,Q,T7);                         // 20. T7 ¬ T1 ´ T7
      MulModProc(T5,T5,Q,T1);                         // 21. T1 ¬ T 5 2
      MPSub(T1,T7); ModProc(T1,Q);                    // 22. T1 ¬ T1 – T7 = X2
      MPSub(T7,T1); MPSub(T7,T1); ModProc(T7,Q);      // 23. T7 ¬ T7 – 2 ´ T1 = V
      MulModProc(T5,T7,Q,T5);                         // 24. T5 ¬ T5 ´ T7
      MulModProc(T4,T2,Q,T4);                         // 25. T4 ¬ T2 ´ T4
      MPSub2(T5,T4,T2); ModProc(T2,Q);                // 26. T2 ¬ T5 – T4
      if Odd(T2^.Data[T2^.Size-1]) then               // 27. T2 ¬ T2 / 2 = Y2
        MPAdd(T2,Q);
      MPShr(T2,1);
      MPDealloc(P0.X); P0.X := T1;                    // 28. X2 ¬ T1
      MPDealloc(P0.Y); P0.Y := T2;                    // 29. Y2 ¬ T2
      MPDealloc(P0.Z); P0.Z := T3;                    // 30. Z2 ¬ T3
    end;
  end else begin
    if (MPMSB(P1.Z) <> 1) or
       (P1.Z.Sign <> 1) then begin                    // 6. If Z1 ¹ 1 then
      T6 := P1.Z;                                     //   T6 ¬ Z1
      MPMul2(T6,T6,T7); ModProc(T7,Q);                //   T7 ¬ T 6 2
      MPMul(T1,T7); ModProc(T1,Q);                    //   T1 ¬ T1 ´ T7 = U0 (if Z1 ¹ 1)
      MPMul(T7,T6); ModProc(T7,Q);                    //   T7 ¬ T6 ´ T7
      MPMul(T2,T7); ModProc(T2,Q);                    //   T2 ¬ T2 ´ T7 = S0 (if Z1 ¹ 1)
    end;
    MPMul2(T3,T3,T7); ModProc(T7,Q);                  // 7. T7 ¬ T 3 2
    MPMul(T4,T7); ModProc(T4,Q);                      // 8. T4 ¬ T4 ´ T7 = U1
    MPMul(T7,T3); ModProc(T7,Q);                      // 9. T7 ¬ T3 ´ T7
    MPMul(T5,T7); ModProc(T5,Q);                      // 10. T5 ¬ T5 ´ T7 = S1
    MPSub2(T1,T4,T4); ModProc(T4,Q);                  // 11. T4 ¬ T1 – T4 = W
    MPSub2(T2,T5,T5); ModProc(T5,Q);                  // 12. T5 ¬ T2 – T5 = R
    if MPMSB(T4) = 0 then begin                       // 13. If T4 = 0 then
      if MPMSB(T5) = 0 then                           // If T5 = 0 then output (0,0,0) and stop
        Result := False
      else                                            // else output (1, 1, 0) and stop
        ECInfProj(P0,C);
      MPDealloc(T1);
      MPDealloc(T2);
      MPDealloc(T3);
    end else begin
      MPAdd(T1,T1); MPSub(T1,T4); ModProc(T1,Q);      // 14. T1 ¬ 2 ´ T1 – T4 = T
      MPAdd(T2,T2); MPSub(T2,T5); ModProc(T2,Q);      // 15. T2 ¬ 2 ´ T2 – T5 = M
      if (MPMSB(P1.Z) <> 1) or
         (P1.Z^.Sign <> 1) then begin                 // 16. If Z1 ¹ 1 then
        MPMul(T3,T6); ModProc(T3,Q);                  // T3 ¬ T3 ´ T6
      end;
      MPMul(T3,T4); ModProc(T3,Q);                    // 17. T3 ¬ T3 ´ T4 = Z2
      MPMul2(T4,T4,T7); ModProc(T7,Q);                 // 18. T7 ¬ T 4 2
      MPMul(T4,T7); ModProc(T4,Q);                    // 19. T4 ¬ T4 ´ T7
      MPMul(T7,T1); ModProc(T7,Q);                    // 20. T7 ¬ T1 ´ T7
      MPMul2(T5,T5,T1); ModProc(T1,Q);                 // 21. T1 ¬ T 5 2
      MPSub(T1,T7); ModProc(T1,Q);                    // 22. T1 ¬ T1 – T7 = X2
      MPSub(T7,T1); MPSub(T7,T1); ModProc(T7,Q);      // 23. T7 ¬ T7 – 2 ´ T1 = V
      MPMul(T5,T7); ModProc(T5,Q);                    // 24. T5 ¬ T5 ´ T7
      MPMul(T4,T2); ModProc(T4,Q);                    // 25. T4 ¬ T2 ´ T4
      MPSub2(T5,T4,T2); ModProc(T2,Q);                // 26. T2 ¬ T5 – T4
      if Odd(T2^.Data[T2^.Size-1]) then               // 27. T2 ¬ T2 / 2 = Y2
        MPAdd(T2,Q);
      MPShr(T2,1);
      MPDealloc(P0.X); P0.X := T1;                    // 28. X2 ¬ T1
      MPDealloc(P0.Y); P0.Y := T2;                    // 29. Y2 ¬ T2
      MPDealloc(P0.Z); P0.Z := T3;                    // 30. Z2 ¬ T3
    end;
  end;
  MPDealloc(T4);
  MPDealloc(T5);
  MPDealloc(T7);
end;

procedure ECFullAdditionProj(var P0: TECPoint; const P1: TECPoint; const C: TECurve);
begin
  Assert(Assigned(P0.Z) and Assigned(P1.Z));
  if ECIsInfProj(P0,C) then
    ECCopy2(P1,P0)
  else if not ECIsInfProj(P1,C) then begin
    if not ECAddProj(P0,P1,C) then
      ECDoubleProj(P0,C);
  end;
end;      

procedure ECFullSubtractionProj(var P0: TECPoint; const P1: TECPoint; const C: TECurve);
begin 
  Assert(Assigned(P0.Z) and Assigned(P1.Z));
  P1.Y^.Sign := -P1.Y^.Sign;
  ECFullAdditionProj(P0,P1,C);
  P1.Y^.Sign := -P1.Y^.Sign;
end;

procedure ECScalarMul(const P: TECPoint; N: PMPInteger; const C: TECurve; var S: TECPoint);
var
  Q: TECPoint;
  K: PMPInteger;
  I, J: Integer;
  LWK, Bit: LongWord;
  HasDoubled: Boolean;
begin
  if MPMSB(N) = 0 then
    ECInf(S,C)
  else begin
    FillChar(Q,SizeOf(Q),0);
    ECCopy2(P,Q);
    K := nil;
    MPCopy2(N,K);
    if N.Sign < 0 then begin
      ECNeg(Q);
    end;
    ECInfProj(S,C);
    ECAffineToProjective(Q,C);
    HasDoubled := False;
    for I := 0 to K.Size - 1 do begin
      LWK := K^.Data[I];
      if (LWK = 0) and not HasDoubled then Continue;
      Bit := $80000000;
      for J := 31 downto 0 do begin
        if HasDoubled then
          ECDoubleProj(S,C);
        if Bit and LWK > 0 then begin
          HasDoubled := True;
          ECFullAdditionProj(S,Q,C);
        end;
        Bit := Bit shr 1;
      end;
    end;
    ECProjectiveToAffine(S,C);
    ECDealloc(Q);
    MPDealloc(K);
  end;
end;

procedure ECScalarMulW(const P: TECPoint; N: PMPInteger; const C: TECurve; var S: TECPoint);
var
  LW, Bit, EW: LongWord;
  i, j, bits, k, l: Integer;
  G2: TECPoint;
  WG: array of TECPoint;
begin
  bits := MPMSB(N);
  case bits of
    0..18:     k := 1;
    19..48:    k := 3;
    49..160:   k := 4;
    161..480:  k := 5;
    481..1344: k := 6;
    1345..3584:k := 7;
  else
    k := 8;
  end;                          
  SetLength(WG,1 shl (k-1));
  FillChar(WG[0],SizeOf(WG[0]),0);
  ECCopy2(P,WG[0]);
  ECAffineToProjective(WG[0],C);
  FillChar(G2,SizeOf(G2),0);
  ECCopy2(WG[0],G2);
  ECDoubleProj(G2,C);
  if k > 1 then
    for i := 1 to (1 shl (k-1)) - 1 do begin
      FillChar(WG[i],SizeOf(WG[i]),0);
      ECCopy2(WG[i-1],WG[i]);
      ECFullAdditionProj(WG[i],G2,C);
    end;
  ECDealloc(G2);

  ECInfProj(S,C);
  try
    EW := 0;
    for i := 0 to N^.Size - 1 do begin
      LW := N^.Data[i];
      Bit := $80000000;
      for j := 31 downto 0 do begin
        EW := EW shl 1;
        if (Bit and LW) = Bit then
          EW := EW + 1
        else if EW = 0 then
          ECDoubleProj(S,C);
        if (EW shr (k-1)) = 1 then begin
          l := 0;
          while not Odd(EW) do begin
            Inc(l);
            EW := EW shr 1;
          end;
          bits := k - l;
          while bits > 0 do begin
            ECDoubleProj(S,C);
            Dec(bits);
          end;
          ECFullAdditionProj(S,WG[EW shr 1],C);
          while l > 0 do begin
            ECDoubleProj(S,C);
            Dec(l);
          end;
          EW := 0;
        end;
        Bit := Bit shr 1;
      end;
    end;

    if EW <> 0 then begin
      i := k;
      while (EW shr (i-1)) = 0 do
        Dec(i);
      l := 0;
      while not Odd(EW) do begin
        Inc(l);
        EW := EW shr 1;
      end;
      bits := i - l;
      while bits > 0 do begin
        ECDoubleProj(S,C);
        Dec(bits);
      end;
      ECFullAdditionProj(S,WG[EW shr 1],C);
      while l > 0 do begin
        ECDoubleProj(S,C);
        Dec(l);
      end;
    end;
  finally
    for i := 0 to (1 shl (k-1)) - 1 do
      ECDealloc(WG[i]);
    WG := nil;
  end;
  ECProjectiveToAffine(S,C);
end;

{$WARNINGS OFF}
{$HINTS OFF}
procedure ECFindG(var C: TECurve);
var
  X, Alpha, Beta, T: PMPInteger;
  ModProc: TMPModProc;
  S: Byte;
begin
  ModProc := C.ModProc;
  if not Assigned(ModProc) then
    ModProc := MPMod;
  X := nil;
  Alpha := nil;
  Beta := nil;
  try
    repeat
      MPRandomBound(X,nil,C.Q);
      MPMul2(X,X,Alpha); ModProc(Alpha,C.Q);
      MPMul(Alpha,X); ModProc(Alpha,C.Q);
      T := nil;
      try
        MPMul2(X,C.A,T); ModProc(T,C.Q);
        MPAdd(Alpha,T); ModProc(Alpha,C.Q);
      finally
        MPDealloc(T);
      end;
      MPAdd(Alpha,C.B); ModProc(Alpha,C.Q);
    until MPSqrtModPrime2(Alpha,C.Q,Beta);
    S := 0;
    RawRandom(S,1);
    if S = 0 then
      Beta.Sign := -1
    else
      Beta.Sign := 1;
    MPCopy2(Beta,C.G.Y);
    MPCopy2(X,C.G.X);
  finally
    MPDealloc(X);
    MPDealloc(Alpha);
    MPDealloc(Beta);
  end;
end;

function ECTestCM(const E: TECurve; K: Cardinal; D: LongWord;
                  var W, V: PMPInteger): Boolean;
var
  Tmp, B2, C2, Delta, A, B, C, U0, U1, T: PMPInteger;
  Res: Integer;
  DeltaWasZeroLastTime: Boolean;
begin
  Res := E.Q^.Data[E.Q^.Size-1] and $7;
  if K = 1 then
    Result := D and $7 = 3
  else begin
    if Res = 3 then
      Result := (D and $7) in [2,3,7]
    else if Res = 5 then
      Result := Odd(D)
    else if Res = 7 then
      Result := (D and $7) in [3,6,7]
    else
      Result := True;
    if K < 4 then
      Result := Result and ((D and $7) <> 7);
  end;
  if Result then begin
    Tmp := IntToMPInt(-D);
    MPMod(Tmp,E.Q);
    B := nil;
    Result := MPSqrtModPrime2(Tmp,E.Q,B);
    if Result then begin
      A := MPCopy(E.Q);
      MPMul2(B,B,Tmp);
      C := IntToMPInt(D);
      MPAdd(Tmp,C);
      MPDiv(Tmp,E.Q,C);
      U0 := IntToMPInt(1);
      U1 := IntToMPInt(0);
      B2 := nil;
      C2 := nil;
      Delta := nil;
      DeltaWasZeroLastTime := False;
      MPAdd2(B,B,B2);
      B2.Sign := 1;
      while (MPCmpOffset(Tmp,A) > 0) or (MPCmpOffset(A,C) > 0) do begin
        B2.Sign := B.Sign;
        MPAdd(B2,C);
        MPAdd2(C,C,C2);
        MPDiv(B2,C2,Delta);
        if MPMSB(Delta) = 0 then begin
          if DeltaWasZeroLastTime then begin
            Result := False;
            Break;
          end else
            DeltaWasZeroLastTime := True;
        end;

        MPCopy2(U0,Tmp);
        MPMul(U0,Delta);
        MPAdd(U0,U1);
        MPCopy2(Tmp,U1);
        U1.Sign := -U1.Sign;

        MPCopy2(C,C2);
        
        MPMul2(B,Delta,Tmp);
        MPAdd(Tmp,Tmp);
        MPMul(C,Delta);
        MPMul(C,Delta);
        MPSub(C,Tmp);
        MPAdd(C,A);

        MPCopy2(Tmp,A);

        MPMul(Tmp,Delta);
        MPSub(Tmp,B);
        MPCopy2(Tmp,B);

        MPAdd2(B,B,B2);
        B2.Sign := 1;
      end;
      if Result then begin
        if (D = 11) and
           (MPMSB(A) = 2) and (A^.Sign = 1) and (A^.Data[0] = 3) then begin
          T := U0;
          U0 := U1;
          U1 := T;
          U1.Sign := -U1.Sign;

          T := C;
          C := A;
          A := T;
        end;
        if (D = 1) or (D = 3) then begin
          MPAdd2(U0,U0,W);
          MPAdd2(U1,U1,V);
        end else begin
          MPDealloc(V);
          V := nil;
          if MPMSB(A) = 1 then
            MPAdd2(U0,U0,W)
          else if (MPMSB(A) = 3) and
                  (A^.Sign = 1) and (A^.Data[A^.Size-1] = 4) then begin
            MPShl(U0,2);
            MPMul(U1,B);
            MPAdd2(U0,U1,W);
          end else
            Result := False;
        end;
      end;
      MPDealloc(Delta);
      MPDealloc(B2);
      MPDealloc(C2);
      MPDealloc(U0);
      MPDealloc(U1);
      MPDealloc(C);
      MPDealloc(A);
    end;
    MPDealloc(Tmp);
    MPDealloc(B);
  end;
end;

function ECMOVCondition(Q, R: PMPInteger): Boolean;
var
  m, B, I: Integer;
  T: PMPInteger;
begin
  m := MPMSB(Q);
  case m of
    0..142:   B := 6;
    143..165: B := 7;
    166..186: B := 8;
    187..206: B := 9;
    207..226: B := 10;
    227..244: B := 11;
    245..262: B := 12;
    263..280: B := 13;
    281..297: B := 14;
    298..313: B := 15;
    314..330: B := 16;
    331..346: B := 17;
    347..361: B := 18;
    362..376: B := 19;
    377..391: B := 20;
    392..406: B := 21;
    407..420: B := 22;
    421..434: B := 23;
    435..448: B := 24;
    449..462: B := 25;
    463..475: B := 26;
    476..488: B := 27;
    489..501: B := 28;
    502..512: B := 29;
  else
    B := 32;
  end;
  T := IntToMPInt(1);
  for I := 1 to B do begin
    MPMulMod(T,Q,R);
    Result := MPMSB(T) > 1;
    if not Result then Break;
  end;
end;
{$WARNINGS ON}
{$HINTS ON}

end.
