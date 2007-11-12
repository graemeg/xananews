{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Base64 Unit                                       }
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
unit SsBase64;

interface

{$IFDEF D4UP}
function MIME64ToBuf(const Src; SrcSize: Integer; var Dst): Integer; overload;
function MIME64ToBuf(Value: string; var Dst): Integer; overload;
{$ELSE}
function MIME64ToBuf(const Src; SrcSize: Integer; var Dst): Integer;
function MIME64StrToBuf(Value: string; var Dst): Integer;
{$ENDIF}
function MIME64ToStr(Value: string): string; overload;
function MIME64ToStr(BeginDelim, EndDelim, Value: string): string; overload;
function MIME64ToStrEx(var BeginDelim, EndDelim: string; Value: string): string;

{$IFDEF D4UP}
function BufToMIME64(const Src; SrcSize: Integer; var Dst; DstSize: Integer): Integer; overload;
function BufToMIME64(const Src; SrcSize: Integer): string; overload;
{$ELSE}
function BufToMIME64(const Src; SrcSize: Integer; var Dst; DstSize: Integer): Integer;
function BufToMIME64Str(const Src; SrcSize: Integer): string;
{$ENDIF}
function StrToMIME64(Value: string): string; overload;
function StrToMIME64(BeginDelim, EndDelim, Value: string): string; overload;

function StrLengthFromMIME64(Value: string): Integer;
function MIME64LengthFromStr64(Value: string): Integer;

implementation

uses
  SysUtils;

{$IFNDEF LONGWORD}
type
  LongWord = LongInt;
{$ENDIF}

function TableFind(Value: Char; Table: PChar; Len: Integer): Integer;
asm
      PUSH  EDI
      MOV   EDI,EDX
      REPNE SCASB
      MOV   EAX,0
      JNE   @@1
      MOV   EAX,EDI
      SUB   EAX,EDX
@@1:  DEC   EAX
      POP   EDI
end;

const
  CharTable: packed array [0..64] of Char =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

function StrLengthFromMIME64(Value: string): Integer;
var
  I, J, Len: Integer;
begin
  Len := 0;
  for J := 1 to Length(Value) do begin
    I := TableFind(Value[J],CharTable,65);
    if (I >= 0) and (I < 64) then
      Inc(Len);
  end;
  if Len mod 4 > 0 then
    Result := (Len div 4) * 3 + (Len mod 4) - 1
  else
    Result := (Len div 4) * 3;
end;

function MIME64LengthFromStr64(Value: string): Integer;
var
  Len: Integer;
begin
  Len := Length(Value);
  Result := ((Len + 2) div 3) * 4;
end;
                      
const
  InvCharTable: array [Char] of LongWord = (
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 062, 255, 255, 255, 063,
    052, 053, 054, 055, 056, 057, 058, 059,
    060, 061, 255, 255, 255, 255, 255, 255,
    255, 000, 001, 002, 003, 004, 005, 006,
    007, 008, 009, 010, 011, 012, 013, 014,
    015, 016, 017, 018, 019, 020, 021, 022,
    023, 024, 025, 255, 255, 255, 255, 255,
    255, 026, 027, 028, 029, 030, 031, 032,
    033, 034, 035, 036, 037, 038, 039, 040,
    041, 042, 043, 044, 045, 046, 047, 048,
    049, 050, 051, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);
                                                                              
function DecodeBuf(const Src; SrcSize: Integer; var Dst; var Tmp: LongWord; var TmpCount: Integer): Integer;
asm
  push EBX
  push ESI
  push EDI

  mov EBX,EDX

  mov EDI,ECX
  mov ESI,EAX

  push ECX

  test EBX,EBX
  jz @@Exit

  add EBX,ESI
  xor EAX,EAX
  mov ECX,4
@@1:
  cmp ESI,EBX
  je @@3
@@2:
  movzx EDX,[ESI].byte
  inc ESI
  mov EDX,[InvCharTable + EDX*4].dword
  cmp EDX,$FF
  je @@1
  shl EAX,6
  or EAX,EDX
  loop @@1

  bswap EAX
  shr EAX,8
  mov [EDI],EAX

  add EDI,3
  mov ECX,4

  cmp ESI,EBX
  jne @@2

@@3:
  mov EBX,[TmpCount]
  mov [EBX].DWord,ECX
  mov EBX,[Tmp]
  mov [EBX].DWord,EAX
@@Exit:
  sub EDI,[ESP]
  mov EAX,EDI
  add ESP,4

  pop EDI
  pop ESI
  pop EBX
end;

function MIME64ToBuf(const Src; SrcSize: Integer; var Dst): Integer;
{$IFDEF D4UP}overload;{$ENDIF}
var
  TmpCount: Integer;
  Tmp: LongWord;
begin
  TmpCount := 0;
  Result := DecodeBuf(Src,SrcSize,Dst,Tmp,TmpCount);
  if TmpCount = 2 then begin
    Tmp := Tmp shr 4;
    PChar(@Dst)[Result] := Char(Byte(Tmp));
    Inc(Result);
  end else if TmpCount = 1 then begin
    Tmp := Tmp shr 2;
    PChar(@Dst)[Result] := Char(Byte(Tmp shr 8));
    PChar(@Dst)[Result + 1] := Char(Byte(Tmp));
    Result := Result + 2;
  end;
end;

{$IFDEF D4UP}
function MIME64ToBuf(Value: string; var Dst): Integer; overload;
begin
  Result := MIME64ToBuf(Pointer(Value)^,Length(Value),Dst);
end;
{$ELSE}
function MIME64StrToBuf(Value: string; var Dst): Integer;
begin
  Result := MIME64ToBuf(Pointer(Value)^,Length(Value),Dst);
end;
{$ENDIF}

function MIME64ToStr(Value: string): string;
var
  Len: Integer;
begin
  Result := '';
  Len := Length(Value);
  if Len = 0 then Exit;
  SetLength(Result, ((Len + 3) shr 2) * 3);
  Len := MIME64ToBuf(Pointer(Value)^,Length(Value),Pointer(Result)^);
  SetLength(Result, Len);
end;

function MIME64ToStr(BeginDelim, EndDelim, Value: string): string;
var
  I: Integer;
begin
  I := Pos(BeginDelim,Value);
  if I > 0 then begin
    Value := Copy(Value,I + Length(BeginDelim),MaxInt);
    I := Pos(EndDelim,Value);
    if I > 0 then begin
      Value := Copy(Value,1,I-1);
      Result := MIME64ToStr(Value);
    end else
      Result := '';
  end else
    Result := '';
end;

function MIME64ToStrEx(var BeginDelim, EndDelim: string; Value: string): string;
var
  I, J: Integer;
  LineWithValidChars: Boolean;
begin
  I := Pos('-',Value);
  if I > 0 then begin
    J := I;
    while (J <= Length(Value)) and (Value[J] = '-') do
      Inc(J);
    while (J <= Length(Value)) and (Value[J] <> '-') do
      Inc(J);
    while (J <= Length(Value)) and (Value[J] = '-') do
      Inc(J);
    BeginDelim := Copy(Value,I,J-I);
    while (J <= Length(Value)) and (Value[J] in [#0..#32]) do
      Inc(J);
    I := J;
    repeat
      LineWithValidChars := True;
      while (J <= Length(Value)) and not (Value[J] in [#0..#32]) do begin
        LineWithValidChars := InvCharTable[Value[J]] <> 255;
        if not LineWithValidChars then Break;
        Inc(J);
      end;
      if not LineWithValidChars then begin
        while (J <= Length(Value)) and not (Value[J] in [#0..#32]) do
          Inc(J);                          
        while (J <= Length(Value)) and (Value[J] in [#0..#32]) do
          Inc(J);
        I := J;
      end;
    until (J > Length(Value)) or LineWithValidChars;
    Value := Copy(Value,I,MaxInt);
    I := Pos('-',Value);
    if I > 0 then begin
      J := I;
      while (J <= Length(Value)) and (Value[J] = '-') do
        Inc(J);
      while (J <= Length(Value)) and (Value[J] <> '-') do
        Inc(J);
      while (J <= Length(Value)) and (Value[J] = '-') do
        Inc(J);
      EndDelim := Copy(Value,I,J-I);
      Value := Copy(Value,1,I);
      Result := MIME64ToStr(Value);
    end else
      Result := '';
  end else
    Result := '';
end;

procedure EncodeBlocks(Src, Dst: Pointer; BlockCount: Integer);
asm
  push ESI
  push EDI
  push EBX
  push EBP

  mov ESI,EAX
  mov EDI,EDX
  mov EBP,ECX
@@1:
  mov EAX,[ESI]
  add ESI,4
  bswap EAX
  movzx EDX,AL

  shr EAX,8
  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]
  shl EBX,16
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]

  mov [EDI],EBX
  add EDI,4

  mov EAX,[ESI]
  add ESI,4
  bswap EAX
  xchg DX,AX
  ror EAX,16

  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]
  shl EBX,16
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]

  mov [EDI],EBX
  add EDI,4

  mov EAX,[ESI]
  add ESI,4
  mov EBX,EAX
  bswap EAX
  xchg EAX,EDX
  shl EAX,8
  mov AL,BL

  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]
  shl EBX,16
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]

  mov [EDI],EBX
  add EDI,4

  mov EAX,EDX

  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]
  shl EBX,16
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BH,byte ptr [CharTable + ECX]
  shr EAX,6
  movzx ECX,AL
  and ECX,$3F
  mov BL,byte ptr [CharTable + ECX]

  mov [EDI],EBX
  add EDI,4

  dec EBP
  jnz @@1

  pop EBP
  pop EBX
  pop EDI
  pop ESI
end;

function BufToMIME64(const Src; SrcSize: Integer; var Dst; DstSize: Integer): Integer;
{$IFDEF D4UP}overload;{$ENDIF}
var
  B: Cardinal;
  I,Len,BlockCount: Integer;
  D,T,V: PChar;
begin
  Result := 0;
  Len := SrcSize;
  if Len = 0 then Exit;

  BlockCount := Len div 12;
  if BlockCount > DstSize shr 4 then
    BlockCount := DstSize shr 4;

  D := PChar(@Dst);
  V := PChar(@Src);

  if BlockCount > 0 then
    EncodeBlocks(V,D,BlockCount);

  Result := BlockCount shl 4;
  D := D + Result;
  Len := Len - BlockCount * 12;
  V := V + BlockCount * 12;


  T := CharTable;
  while Len > 0 do begin
    B := 0;
    for I := 0 to 2 do begin
      B := B shl 8;
      if Len > 0 then begin
        B := B or Byte(V^);
        Inc(V);
      end;
      Dec(Len);
    end;
    if Result + 4 <= DstSize then begin
      for I := 3 downto 0 do begin
        if Len < 0 then begin
          D[I] := T[64];
          Inc(Len);
        end else
          D[I] := T[B and $3F];
        B := B shr 6;
      end;
      Inc(Result,4);
    end else begin
      for I := 3 downto 0 do begin
        if (Len < 0) and (Result + I < DstSize) then begin
          D[I] := T[64];
          Inc(Len);
        end else if Len >= 0 then
          D[I] := T[B and $3F];
        B := B shr 6;
      end;
      Result := DstSize;
    end;
    Inc(D, 4);
  end;
end;

{$IFDEF D4UP}
function BufToMIME64(const Src; SrcSize: Integer): string; overload;
var
  Len: Integer;
begin
  Len := ((SrcSize + 2) div 3) * 4;
  SetLength(Result,Len);
  BufToMIME64(Src,SrcSize,Pointer(Result)^,Len);
end;
{$ELSE}      
function BufToMIME64Str(const Src; SrcSize: Integer): string;
var
  Len: Integer;
begin
  Len := ((SrcSize + 2) div 3) * 4;
  SetLength(Result,Len);
  BufToMIME64(Src,SrcSize,Pointer(Result)^,Len);
end;
{$ENDIF}

function StrToMIME64(Value: string): string;
begin
{$IFDEF D4UP}
  Result := BufToMIME64(Pointer(Value)^,Length(Value));
{$ELSE}                                                
  Result := BufToMIME64Str(Pointer(Value)^,Length(Value));
{$ENDIF}
end;

function StrToMIME64(BeginDelim, EndDelim, Value: string): string; overload;
var
  L: Integer;
begin
  Result := StrToMIME64(Value);
  L := 1;
  while L + 76 <= Length(Result) do begin
    Insert(#13#10,Result,L + 76);
    L := L + 78;
  end;
  Result := BeginDelim + #13#10 +
            Result + #13#10 +
            EndDelim;
end;

end.




