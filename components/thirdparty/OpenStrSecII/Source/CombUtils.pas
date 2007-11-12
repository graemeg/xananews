{$Q-,R-}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Combinatoric Utilities Unit                       }
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
unit CombUtils;

interface

type
  TByteDiv = 1..256;

  PByte = ^Byte;

  TSmallByteArray = array [0..255] of Byte;

  TByteSet = set of Byte;

  {TPermution creates a single cycle permution out of Buf, and reduces
   Buf by the values used to create the permutation.}

  TPermutation = class
  private
    FMap: TSmallByteArray;
    FSize: TByteDiv;
    function GetMap(index: Byte): Byte;
    function GetSize: Integer;
    procedure SetMap(index: Byte; const Value: Byte);
  protected
    procedure MakeCycle(var Buf; Count: Integer); virtual;
  public
    constructor Create(var Buf; Count: Integer; ASize: TByteDiv);
    destructor Destroy; override;
    function Check: Boolean; virtual;
    property Map[index: Byte]: Byte read GetMap write SetMap;
    property Size: Integer read GetSize;
  end;

  
  {TAnyPermution creates a permution out of Buf, and reduces
   Buf by the values used to create the permutation.}

  TAnyPermutation = class(TPermutation)
  protected
    procedure MakeCycle(var Buf; Count: Integer); override;
  public
    function Check: Boolean; override;
  end;


  {TCombination creates a selection of B element of [0..A-1] out of Buf, and
   reduces Buf by the values used to create the selection.}

  TCombination = class
  private
    FItems: TByteSet;
    FA: TByteDiv;
    FB: TByteDiv;
    function GetA: TByteDiv;
    function GetB: TByteDiv;
    procedure MakeSet(var Buf; Count: Integer);
    procedure SetItems(const Value: TByteSet);
  public
    constructor Create(var Buf; Count: Integer; A, B: TByteDiv);
    destructor Destroy; override;
    function Check: Boolean;
    property A: TByteDiv read GetA;
    property B: TByteDiv read GetB;
    property Items: TByteSet read FItems write SetItems;
  end;

// Arithmetic routines:

function DivModByByte(var Buf; Count: Integer; const Value: TByteDiv): Byte;

procedure AddMultByByte(var Buf; Count: Integer; const Fac: TByteDiv; const Term: Byte);

procedure RevertMakeCycle(var Buf; Count: Integer; Perm: TPermutation);

implementation

uses
  SysUtils, SecUtils;

procedure RevertMakeCycle(var Buf; Count: Integer; Perm: TPermutation);
var
  P: Pointer;
  I, N, B: Integer;
  M: Byte;
  S: TByteDiv;
  Taken: set of Byte;
  Cycle: TSmallByteArray;
begin
  S := Perm.Size;
  N := Perm.FMap[S-1];
  for I := 0 to S-2 do begin
    Cycle[I] := N;
    N := Perm.FMap[N];
  end;
  Cycle[S-1] := S-1;

  for I := 0 to S - 1 do Include(Taken,I);
  Exclude(Taken,Cycle[0]);

  P := @Buf;

  for I := 2 to S - 1 do begin
    N := Cycle[I-1];
    Exclude(Taken,N);
    M := 0;
    for B := 0 to N-1 do
      if not (B in Taken) then Inc(M);
    AddMultByByte(P^,Count,I,M);
  end;

  Exit;
  ProtectClear(Cycle,SizeOf(Cycle));
end;

function DivModByByte(var Buf; Count: Integer; const Value: TByteDiv): Byte;
var
  Carry, Q: LongInt;
  B: PByte;
  I: Integer;
begin
  if Value < 2 then begin
    Result := 0;
    Exit;
  end;
  Carry := 0;
  B := Ptr(LongInt(@Buf) + Count - 1);
  for I := Count downto 1 do begin
    Carry := ((Carry shl 8) or B^);
    Q := Carry div Value;
    B^ := Byte(Q);
    Dec(Carry,Q*Value);
    Dec(LongInt(B));
  end;
  Result := Byte(Carry);
end;

procedure AddMultByByte(var Buf; Count: Integer; const Fac: TByteDiv; const Term: Byte);
var
  Q: LongInt;
  B: PByte;
  I: Integer;
begin
  if Fac = 1 then Exit;
  if Term >= Fac then
    raise EIntOverflow.Create('Overflow in AddMultByByte');
  Q := Term;
  B := @Buf;
  for I := 1 to Count do begin
    Q := (Fac * B^) + Q;
    B^ := Byte(Q);
    Q := Q shr 8;
    Inc(LongInt(B));
  end;
  if Q > 0 then
    raise EIntOverflow.Create('Overflow in AddMultByByte');
end;

{ TPermutation }

function TPermutation.Check: Boolean;
var
  I, S: Integer;
  Taken: set of Byte;
  Cycle: TSmallByteArray;
  B: Byte;
begin
  Result := False;

  FillChar(Taken,SizeOf(Taken),0);
  S := FSize;
  Cycle[S-1] := S-1;
  B := S-1;
  Include(Taken,B);
  for I := 0 to S - 2 do begin
    B := FMap[B];
    if B in Taken then Exit;
    Include(Taken,B);
    Cycle[I] := B;
  end;
  Result := FMap[B] = S - 1;
end;

constructor TPermutation.Create(var Buf; Count: Integer; ASize: TByteDiv);
begin
  FSize := ASize;
  MakeCycle(Buf,Count);
end;

destructor TPermutation.Destroy;
begin
  ProtectClear(FMap,SizeOf(FMap));
  inherited;
end;

function TPermutation.GetMap(index: Byte): Byte;
begin
  Result := FMap[index];
end;

function TPermutation.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TPermutation.MakeCycle(var Buf; Count: Integer);
var
  P: Pointer;
  I, J: Integer;
  B, M, N, FirstAvail: Byte;
  S: TByteDiv;
  Taken: set of Byte;
  Cycle: TSmallByteArray;
begin
  {The binary content of Buf is divided by (Size-1)! and the residue is
   used to create a single cycle permutation of length Size.}
  FillChar(Taken,SizeOf(Taken),0);
  FirstAvail := 0;
  S := Size;
  P := @Buf;
  {There are (S-1)! different single cycle permutations of length S,
   so it is immaterial where the first value is positioned in the cycle.}
  Cycle[S-1] := S-1;
  Include(Taken,S-1);
  // Create cycle:
  for I := Size-1 downto 2 do begin
    B := I;
    M := DivModByByte(P^,Count,B);
    N := FirstAvail;
    for J := 1 to M do
      repeat
        N := (N + 1) mod S;
      until not (N in Taken);
    Include(Taken,N);
    Cycle[I-1] := N;
    // Setup FirstTaken for next round:
    while FirstAvail in Taken do
      FirstAvail := (FirstAvail + 1) mod S;
  end;
  Cycle[0] := FirstAvail;
  // Evaluate Cycle:
  { B holds the previous value in the cycle and hence the value at the
    position given by the current value. }
  B := S-1; // ...since Cycle[S-1] = S-1.
  for I := 0 to S-2 do begin
    N := Cycle[I];
    FMap[B] := N;
    B := N;
  end;
  FMap[B] := S-1;
  // Clean up local variables containing key data:
  ProtectClear(Cycle,SizeOf(Cycle));
end;

procedure TPermutation.SetMap(index: Byte; const Value: Byte);
begin
  FMap[index] := Value;
end;

{ TCombination }

function TCombination.Check: Boolean;
var
  I, Count: Integer;
begin
  Count := 0;
  for I := 0 to 255 do
    if I in FItems then begin
      Result := I < FA;
      if not Result then Exit;
      Inc(Count);
    end;
  Result := Count = FB;
end;

constructor TCombination.Create(var Buf; Count: Integer; A, B: TByteDiv);
begin
  if (B >= A) then
    raise EIntOverflow.Create('Overflow in TCombination');
  FA := A;
  FB := B;
  MakeSet(Buf,Count);
end;

destructor TCombination.Destroy;
begin
  ProtectClear(FItems,SizeOf(FItems));
  inherited;
end;

function TCombination.GetA: TByteDiv;
begin
  Result := FA;
end;

function TCombination.GetB: TByteDiv;
begin
  Result := FB;
end;

procedure TCombination.MakeSet(var Buf; Count: Integer);
var
  C, M: array [Byte] of Word;
  I, J, LowBound: Integer;
  Data: Pointer;
  B, N: Byte;
  FirstAvail: Word;
begin
  // Calculate A over B:
  {Extract the factors of A!/(A-B+1)!}
  LowBound := FA - FB + 1;
  for I := LowBound to FA do
    C[I - LowBound] := I;
  {Divide the factors of A!/(A-B+1)! by the factors of B!}
  for I := FB downto 2 do begin
    J := 0;
    {Find the lowest element of C which is divisible by I}
    while (C[J] mod I) > 0 do
      Inc(J);
    C[J] := C[J] div I;
  end;
  {C now contains the factors of A over B.}
  // Extract set:
  GetMem(Data,256);
  try
    // Extract Data from Buf:
    FillChar(Data^,256,0);
    for I := 0 to FB-1 do
      M[I] := DivModByByte(Buf,Count,C[I]);
    for I := FB-1 downto 0 do
      AddMultByByte(Data^,256,C[I],M[I]);
    // Augment Data with default permutation order:
    for I := FB downto 2 do
      AddMultByByte(Data^,256,I,0);
    // Extract set from Data:
    FillChar(FItems,SizeOf(FItems),0);
    FirstAvail := 0;
    for I := FA downto LowBound do begin
      B := DivModByByte(Data^,256,I);
      N := FirstAvail;
      for J := 1 to B do begin
        repeat
          Inc(N);
        until not (N in FItems);
      Include(FItems,N);
      if N = FirstAvail then
        repeat
          Inc(FirstAvail);
        until not (FirstAvail in FItems);
      end;
    end;
  finally
    ProtectClear(C,SizeOf(C));
    ProtectClear(M,SizeOf(M));
    ProtectClear(Data^,256);
    FreeMem(Data);
  end;
end;

procedure TCombination.SetItems(const Value: TByteSet);
begin
  FItems := Value;
end;

{ TAnyPermutation }

function TAnyPermutation.Check: Boolean;
var
  I, S: Integer;
  Taken: set of Byte;
begin
  FillChar(Taken,SizeOf(Taken),0);
  S := FSize;
  for I := 0 to S-1 do Include(Taken,FMap[I]);
  Result := (Taken = [0..S-1]);
end;

procedure TAnyPermutation.MakeCycle(var Buf; Count: Integer);
var
  I, J: Integer;
  B: Byte;
begin
  {The binary content of Buf is divided by Size! and the residue is
   used to create a permutation of length Size.}
  for I := 0 to Size-1 do FMap[I] := I;
  // Create cycle:
  for I := Size downto 2 do begin
    J := DivModByByte(Buf,Count,I);
    B := FMap[I-1];
    FMap[I-1] := FMap[J];
    FMap[J] := B;
  end;
end;

end.
