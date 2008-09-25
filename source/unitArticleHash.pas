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
 | Copyright © Colin Wilson 2002.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      30/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitArticleHash;

interface

uses Windows, Classes, SysUtils, unitNNTPServices, iniFiles;

type
PPHashItem = ^PHashItem;
PHashItem = ^THashItem;
THashItem = packed record
  Next: PHashItem;
  article : TArticleBase;
end;

function AllocHashTable : PPHashItem;
function HashOf(const Key: string): Cardinal;
function FindHashMessage (table : PPHashItem; hash : DWORD; const msg : string) : TArticleBase;
function FindHashSubject (table : PPHashItem; hash : DWORD; const subject : string) : TArticleBase;
procedure AddHash (table : PPHashItem; hash : DWORD; article : TArticleBase);
procedure ClearHash (table : PPHashItem);
procedure FreeHashTable (Table : PPHashItem);

implementation

const
  hashSize = 49157;                     // Prime numbers work best - makes a
                                        // Huge difference to the distribution
(*----------------------------------------------------------------------*
 | procedure AddHash                                                    |
 |                                                                      |
 | Add an article to the hash table at position 'hash'                  |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   table : PPHashItem         The hash table to add to.               |
 |   hash : DWORD               The position to add to.                 |
 |   article : TArticleBase     The article to add                      |
 *----------------------------------------------------------------------*)
//---------------------------------------------------------------------
// Actually Delphi does a pretty good job - but to show off, here's an
// optimised version.
procedure AddHash(table : PPHashItem; hash : DWORD; article : TArticleBase);
label
  collision, done, alloc;
asm
  push  edi
  push  esi
  push  ebx

  mov   esi, table
  mov   eax, hash
  mov   ebx, article
  shl   eax, 2
  add   esi, eax                        // esi = table [hash]

  mov   edi, [esi]                      // edi = table [hash]^
  test  edi, edi
  jnz   collision
                                        // empty slot
  mov   eax, ebx                        // table [hash]^ := article
  or    eax, $80000000
  mov   [esi], eax
  jmp   done

collision:
  test  edi, $80000000                  // Was it an article?
  jz    alloc

                                        // Replace it with a node pointing to
                                        // the article

  mov   eax, 8
  call  system.@GetMem                  // Create a node

  and   edi, $7fffffff                  // Point it to the articl
  mov   [eax], 0                        // node^.next := 0
  mov   [eax + 4], edi

  mov   edi, eax                        // Save this node

alloc:
  mov   eax, 8
  call  system.@GetMem                  // Create a node

  mov   [eax], edi                      // 'Next' points to old entry
  mov   [eax + 4], ebx                  // point to article
  mov   [esi], eax

done:

  pop   ebx
  pop   esi
  pop   edi
end;

(*
procedure AddHashX (table : PPHashItem; hash : DWORD; article : TArticleBase);
var
  pp : PPHashItem;
  p : PHashItem;
  a : TArticleBase;
begin
  pp := table;
  Inc (pp, hash);
  p := pp^;

  if p = Nil then               // First item at this position.  Add the article, + $80000000
    pp^ := PHashItem ($80000000 + DWORD (article))
  else
  begin                         // There's already something at this position

                                // If it's an article, replace it with a linked list of
                                // hash items.
    if (DWORD (p) and $80000000) <> 0 then
    begin
      a := TArticleBase (Integer (p) and $7fffffff);
      new (p);
      p^.Next := Nil;
      p^.article := a;
      pp^ := p;
    end;

    new (p);                    // Add to the linked list of hash items.

    p^.Next := pp^;
    p^.article := article;
    pp^ := p
  end;

end;
*)

(*----------------------------------------------------------------------*
 | procedure ClearHash                                                  |
 |                                                                      |
 | Clear a hash table.                                                  |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   table : PPHashItem         The table to clear                      |
 *----------------------------------------------------------------------*)
procedure ClearHash (table : PPHashItem);
var
  I: Integer;
  pp : PPHashITem;
  P, N: PHashItem;
begin
  pp := table;

  for I := 0 to hashSize - 1 do
  begin
    P := pp^;                           // For each item in the table...

    if (DWORD (p) and $80000000) = 0 then
      while P <> nil do                 // There's a linked list of hash items...
      begin                             // Dispose of each element.
        N := P^.Next;
        Dispose(P);
        P := N;
      end;
    pp^ := nil;

    Inc (pp)
  end;
end;

(*----------------------------------------------------------------------*
 | function FindHashMessage : TArticleBase                              |
 |                                                                      |
 | Find an article at position 'hash' in the table.                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   table : PPHashItem         The table to search.                    |
 |   hash : DWORD               The position to search                  |
 |   const msg : string         The MessageID to search for             |
 |                                                                      |
 | The function returns the 'found' article - or Nil                    |
 *----------------------------------------------------------------------*)
function FindHashMessageX (table : PPHashItem; hash : DWORD; const msg : string) : TArticleBase;
asm
  push  esi
  push  ebx
  push  edi

  mov   esi, table
  mov   eax, hash
  mov   ebx, msg                        // Msg saved in ebx
  shl   eax, 2
  add   esi, eax                        // esi = table [hash]
  mov   esi, [esi]                      // esi = table [hash]^

  test  esi, esi
  jz    @nothing                         // Nothing there!

  test  esi, $80000000
  jz    @list

  and   esi, $7fffffff
  mov   edx, [esi + $10]
  mov   eax, ebx
  call  system.@LStrCmp
  jnz   @nothing

  mov   eax, esi
  jmp   @done

@list:
  mov   edi, [esi + 4]
  mov   edx, [edi + $10]
  mov   eax, ebx
  call system.@LStrCmp
  jnz   @cycle
  mov   eax, edi
  jmp   @done

@cycle:
  mov   esi, [esi]
  test  esi, esi
  jz    @nothing
  jmp   @list

@nothing:
  xor   eax, eax

@done:
  pop   edi
  pop   ebx
  pop   esi
end;

function FindHashMessage (table : PPHashItem; hash : DWORD; const msg : string) : TArticleBase;
var
  pp : PPHashitem;
  p : PHashItem;
begin
  pp := table;
  Inc (pp, hash);
  p := pp^;

  if p <> Nil then
    if (DWORD (p) and $80000000) <> 0 then      // Position contains a single article.
    begin
      Result := TArticleBase (DWORD (p) and $7fffffff);
      if Result.MessageID = msg then                // Is it the right one?
        Exit;
    end
    else
    begin                                       // Position contains a linked list of hash items
      repeat
        Result := p^.article;                        // Find the article in it
        if Result.MessageID = msg then
          exit;
        p := p^.next
      until p = Nil;
    end;
  Result := nil;
end;

(*----------------------------------------------------------------------*
 | function FindHashSubject : TArticleBase                              |
 |                                                                      |
 | Find an article at position 'hash' in the table.                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   table : PPHashItem         The table to search.                    |
 |   hash : DWORD               The position to search                  |
 |   const subject : string     The subject to search for               |
 |                                                                      |
 | The function returns the 'found' article - or Nil                    |
 *----------------------------------------------------------------------*)

function FindHashSubject (table : PPHashItem; hash : DWORD; const subject : string) : TArticleBase;
var
  pp : PPHashitem;
  p : PHashItem;
  a : TArticleBase;
begin
  pp := table;
  Inc (pp, hash);
  p := pp^;

  result := Nil;
  if p <> Nil then
    if (DWORD (p) and $80000000) <> 0 then      // Position contains a single article.
    begin
      a := TArticleBase
       (DWORD (p) and $7fffffff);
      if a.SimplifiedSubject = subject then                // Is it the right one?
        result := a
    end
    else
    begin                                       // Position contains a linked list of hash items
      repeat
        a := p^.article;                        // Find the article in it
        if a.SimplifiedSubject = subject then
        begin
          result := a;
          exit
        end;
        p := p^.next
      until p = Nil;
    end
end;

(*----------------------------------------------------------------------*
 | function HashOf : Cardinal                                           |
 |                                                                      |
 | Calculate the hash value for a string.                               |
 |                                                                      |
 | nb.  It uses the Borland algorithm.  Fastest, though distribution is |
 | not quite as good as PJW.  Overall timings with this algo are        |
 | slightly faster than with PJW                                        |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const Key : string         The string to hash                      |
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

  result := result mod hashSize
end;*)

{$R-,Q-}
function HashOf(const Key : string) : Cardinal;
{Note: this hash function is described in "The C Programming Language"
       by Brian Kernighan and Donald Ritchie, Prentice Hall}
var
  i : integer;
begin
  Result := 0;
  for i := 1 to length(Key) do begin
    Result := (Result * 31) + Ord(Key[i]);
  end;
  Result := Result mod hashSize
end;

(*
// PJW algorithm.  Slightly better distribution, but slower.
function HashOf1 (const key : string) : Cardinal;
var
  l, i : Cardinal;
begin
  result := 0;

  for l := 1 to Length (key) do
  begin
    result := (result shl 4) + Ord (Key [l]);

    i := result and $f0000000;
    if i <> 0 then
      result := (result xor (i shr 24)) and not $f0000000;
  end;

  result := result mod hashSize;
end;
*)

(*----------------------------------------------------------------------*
 | function AllocHashTable                                              |
 |                                                                      |
 | Create a hash table.  Must free it with FreeHashTable                |
 |                                                                      |
 | Parameters:                                                          |
 |   None                                                               |
 |                                                                      |
 | The function returns PPHashItem the initialized empty hash table     |
 *----------------------------------------------------------------------*)
function AllocHashTable : PPHashItem;
begin
  Result := AllocMem (SizeOf (PHashItem) * hashSize);
end;

(*----------------------------------------------------------------------*
 | procedure FreeHashTable                                              |
 |                                                                      |
 | Free a hash table allocated with AllocHashTable                      |
 |                                                                      |
 | Parameters:                                                          |
 |   Table : PPHashItem         // The hash table to clear              |
 *----------------------------------------------------------------------*)
procedure FreeHashTable (Table : PPHashItem);
begin
  ClearHash (Table);
  FreeMem (Table)
end;

end.
