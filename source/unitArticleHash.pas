(*======================================================================*
 | unitArticleHash unit for NewsReader3                                 |
 |                                                                      |
 | Low-level, fast hashing                                              |
 |                                                                      |
 | If you add an article to a hash-table, and no collision occurs (ie   |
 | it's position in the table is not already occupied), the reference   |
 | to the article itself is added.  If there's collision, a linked      |
 | list of THashItems as added instead.                                 |
 |                                                                      |
 | The algorithm sets the high bit of the reference if an article is    |
 | added, and clears the high bit of reference if a hash item is added. |
 |                                                                      |
 | All this takes advantage of the max 2Gb addressable range of         |
 | the current operating systems to ensure that the high bit can't be   |
 | part of the reference itself.                                        |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002.  All Rights Reserved                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      30/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitArticleHash;

interface

uses
  Windows, Classes, SysUtils, unitNNTPServices, XnRawByteStrings;

const
  HASHSIZE = 49157;                     // Prime numbers work best - makes a
                                        // Huge difference to the distribution

// TODO: check if this is really needed.
const
  MaxListSize = MaxInt div (SizeOf(Pointer) * 4);  //divisor: x86=16, x64=32

type
  PHashItem = TArticleBase;

  PPHashItems = ^TPHashItems;
  TPHashItems = array[0..MaxListSize - 1] of PHashItem;

  PHashTable = ^THashTable;
  THashTable = array[0..HASHSIZE - 1] of PPHashItems;

{$IFNDEF FPC}
type
  PtrInt = Integer;
  PtrUInt = Cardinal;
{$ENDIF}

function AllocHashTable: PHashTable;
function HashOf(const Key: RawByteString): Cardinal;
function FindHashMessage(table: PHashTable; hash: DWORD; const msg: RawByteString): TArticleBase;
function FindHashSubject(table: PHashTable; hash: DWORD; const subject: RawByteString): TArticleBase;
procedure AddHash(table: PHashTable; hash: DWORD; article: TArticleBase);
procedure ClearHash(table: PHashTable);
procedure FreeHashTable(table: PHashTable);

implementation

{$R-,Q-}

procedure AddHash(table: PHashTable; hash: DWORD; article: TArticleBase);
const
  Delta = 4; // must a power of two (2, 4, 8, 16, 32, etc)
var
  L: PtrUInt;
  P: ^PPHashItems;
begin
  P := @table^[hash];

  if P^ = nil then
  begin
    GetMem(P^, Delta * SizeOf(PHashItem));
    P^[0] := nil;
  end;
  // Note: P^[0] is used as a "Cardinal" to hold the count of items.
  L := PtrUInt(P^[0]);
  Inc(L);
  if (L and (Delta - 1)) = 0 then
    ReallocMem(P^, (L + Delta) * SizeOf(PHashItem));
  P^[0] := PHashItem(L);
  P^[L] := article;
end;


procedure ClearHash(table: PHashTable);
var
  I: Integer;
begin
  for I := 0 to HASHSIZE - 1 do
  begin
    FreeMem(table^[I]);
    table^[I] := nil;
  end;
end;


function FindHashMessage(table: PHashTable; hash: DWORD; const msg: RawByteString): TArticleBase;
var
  L: PtrUInt;
  P: PPHashItems;
begin
  P := table^[hash];

  // Note: P^[0] is used as a "Cardinal" to hold the count of items.
  if P <> nil then
    for L := PtrUInt(P^[0]) downto 1 do
    begin
      Result := P^[L];
      if RawCompareStr(msg, Result.RawMessageID) = 0 then
        Exit;
    end;

  Result := nil;
end;


function FindHashSubject(table: PHashTable; hash: DWORD; const subject: RawByteString): TArticleBase;
var
  L: PtrUInt;
  P: PPHashItems;
begin
  P := table^[hash];

  // Note: P^[0] is used as a "Cardinal" to hold the count of items.
  if P <> nil then
    for L := PtrUInt(P^[0]) downto 1 do
    begin
      Result := P^[L];
      if RawCompareStr(subject, Result.SimplifiedSubject) = 0 then
        Exit;
    end;

  Result := nil;
end;


(*----------------------------------------------------------------------*
 | function HashOf: Cardinal                                            |
 |                                                                      |
 | Calculate the hash value for a string.                               |
 |                                                                      |
 | nb.  It uses the Borland algorithm.  Fastest, though distribution is |
 | not quite as good as PJW.  Overall timings with this algo are        |
 | slightly faster than with PJW                                        |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const Key: string         The string to hash                       |
 |                                                                      |
 | The function returns the hash value for the string.                  |
 *----------------------------------------------------------------------*)
(*function HashOf(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(Key) - 1 do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key[I + 1]);

  Result := Result mod hashSize
end;*)

function HashOf(const Key: RawByteString): Cardinal;
{Note: this hash function is described in "The C Programming Language"
       by Brian Kernighan and Donald Ritchie, Prentice Hall}
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Key) do
    Result := (Result * 31) + Ord(Key[i]);
  Result := Result mod HASHSIZE;
end;

(*
// PJW algorithm.  Slightly better distribution, but slower.
function HashOf1(const key: string): Cardinal;
var
  l, i: Cardinal;
begin
  Result := 0;

  for l := 1 to Length(key) do
  begin
    Result := (Result shl 4) + Ord(Key [l]);

    i := Result and $f0000000;
    if i <> 0 then
      Result := (Result xor (i shr 24)) and not $f0000000;
  end;

  Result := Result mod hashSize;
end;
*)

function AllocHashTable: PHashTable;
begin
  Result := AllocMem(SizeOf(PPHashItems) * HASHSIZE);
end;

procedure FreeHashTable(table: PHashTable);
begin
  ClearHash(table);
  FreeMem(table);
end;

end.
