(*======================================================================*
 | unitStringSearcher                                                   |
 |                                                                      |
 | Useful classes and functions for searching strings                   |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)

unit unitSearchString;

interface

uses
  Windows, Classes, SysUtils, StrUtils;

type
  TStrArray = array of string;

  TStringSearcher = class(TObject)
  protected
    fSearchString: string;
    fOrWords: TStrArray;
    fNotWords: TStrArray;
    fAndWords: TStrArray;
    nOrWords: Integer;
    nAndWords: Integer;
    nNotWords: Integer;
    fCaseSensitive: Boolean;
  public
    constructor Create(const ASearchString: string; AcaseSensitive: Boolean);
    function Matches(AString: string): Boolean;
    procedure Parse(searchString: string); virtual; abstract;
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
  end;

  TGoogleLikeStringSearcher = class(TStringSearcher)
  public
    procedure Parse(searchString: string); override;
  end;

function DelimPos(const delims: string; const st: string; out delim: Char): Integer;
function DelimSplitString(const search: string; var s: string; out delim: Char): string;
function ExtractString(const search: string; var s: string): string;
function SplitString(const search: string; var s: string): string;
function SplitToken(var st: string): string;
function WildContains(const a, b: string): Boolean;
function SearchStringArray(arr: array of string; const st: string): Integer;
function StringArrayContains(arr: array of string; const st: string): Boolean;
function SearchQuotedString(const st: string; const delims: string; quote: Char = '"'; brk: Char = #0): Integer;
function SearchRQuotedString(const st: string; const delims: string; quote: Char = '"'; brk: Char = #0): Integer;


implementation

{ TStringSearcher }

constructor TStringSearcher.Create(const ASearchString: string; ACaseSensitive: Boolean);
begin
  fCaseSensitive := ACaseSensitive;
  fSearchString := ASearchString;
  Parse(ASearchString);
end;

function TStringSearcher.Matches(AString: string): Boolean;
type
  TMatch = (mYes, mNo, mMaybe);
var
  i: Integer;
  ok: TMatch;
begin
  if not fCaseSensitive then
    AString := UpperCase(AString);
  ok := mMaybe;

  for i := 0 to nOrWords - 1 do
    if WildContains(AString, fOrWords[i]) then
    begin
      ok := mYes;
      Break;
    end;

  if ok = mMaybe then
    for i := 0 to nAndWords - 1 do
      if not WildContains(AString, fAndWords[i]) then
      begin
        ok := mNo;
        Break;
      end;

  if ok = mMaybe then
    for i := 0 to nNotWords - 1 do
      if WildContains(AString, fNotWords[i]) then
      begin
        ok := mNo;
        Break;
      end;

  if ok = mMaybe then
    Result := (nAndWords > 0) or (nNotWords > 0)
  else
    Result := ok = mYes;
end;

{ TGoogleLikeStringSearcher }

procedure TGoogleLikeStringSearcher.Parse(searchString: string);
type
  tOP = (opAnd, opOr, opNot);
var
  l: Integer;
  s1: string;
  op: tOp;

  procedure AddToVarArray(var arr: TStrArray; const st: string; var n: Integer);
  begin
    if n = Length(arr) then
      SetLength(arr, n + 5);
    arr[n] := st;
    Inc(n);
  end;

begin
  if CompareText(fSearchString, searchString) = 0 then
    Exit;
  fSearchString := searchString;
  nAndWords := 0;
  nOrWords := 0;
  nNotWords := 0;
  if not fCaseSensitive then
    searchString := UpperCase(searchString);

  l := Length(searchString);
  op := opAnd;
  while l > 0 do
  begin
    case searchString[1] of
      '+':
        begin
          op := opAnd;
          Delete(searchString, 1, 1);
          l := Length(searchString);
        end;
      '-':
        begin
          op := opNot;
          Delete(searchString, 1, 1);
          l := Length(searchString);
        end;
    end;

    if l = 0 then Break;

    if searchString[1] = '"' then
    begin
      Delete(searchString, 1, 1);
      s1 := SplitString('"', searchString);
    end
    else
    begin
      s1 := SplitString(' ', searchString);
      if UpperCase(s1) = 'OR' then
      begin
        op := opOR;
        l := Length(searchString);
        Continue;
      end;
    end;

    if s1 <> '' then
      case op of
        opAnd: AddToVarArray(fAndWords, s1, nAndWords);
        opOr : AddToVarArray(fOrWords, s1, nOrWords);
        opNot: AddToVarArray(fNotWords, s1, nNotWords);
      end;

    op := opAnd;
    l := Length(searchString);
  end;
end;

function DelimPos(const delims: string; const st: string; out delim: Char): Integer;
var
  i, p: Integer;
begin
  if delims = '' then
  begin
    Result := 0;
    Exit;
  end;

  Result := MaxInt;
  for i := 1 to Length(delims) do
  begin
    p := Pos(delims[i], st);
    if (p > 0) and (p < Result) then
    begin
      delim := delims[i];
      Result := p;
    end;
  end;

  if Result = MaxInt then
    Result := 0;
end;

function DelimSplitString(const search: string; var s: string; out delim: Char): string;
var
  p: Integer;
begin
  p := DelimPos(search, s, delim);
  if p > 0 then
  begin
    Result := Trim(Copy(s, 1, p - 1));
    s := Trim(Copy(s, p + 1, maxInt));
  end
  else
  begin
    Result := Trim(s);
    s := ''
  end;
end;

(*----------------------------------------------------------------------*
 | function ExtractString: string                                       |
 |                                                                      |
 | Search for a substring.  If the substring is found, return the       |
 | characters up to the substring, and set the soure string to the      |
 | characters after it.  If it was not found, return an empty string    |
 | and leave the source string unchanged.                               |
 *----------------------------------------------------------------------*)
function ExtractString(const search: string; var s: string): string;
var
  p, l: Integer;
begin
  l := Length(search);
  p := Pos(search, s);
  if p > 0 then
  begin
    Result := Trim(Copy(s, 1, p - 1));
    s := Trim(Copy(s, p + l, MaxInt));
  end
  else
    Result := '';
end;

(*----------------------------------------------------------------------*
 | function SplitString: string                                         |
 |                                                                      |
 | Search for a substring.  If the substring is found, return the       |
 | characters up to the substring, and set the soure string to the      |
 | characters after it.  If it was not found, return the entire source  |
 | string, and set the source string to an empty string                 |
 *----------------------------------------------------------------------*)
function SplitString(const search: string; var s: string): string;
var
  p, l: Integer;
begin
  l := Length(search);
  p := Pos(search, s);
  if p > 0 then
  begin
    Result := Trim(Copy(s, 1, p - 1));
    s := Trim(Copy(s, p + l, MaxInt));
  end
  else
  begin
    Result := Trim(s);
    s := ''
  end;
end;

function SplitToken(var st: string): string;
var
  p, p1: Integer;
begin
  p := Pos(' ', st);
  p1 := Pos(#9, st);
  if p = 0 then p := MaxInt;
  if p1 = 0 then p1 := MaxInt;
  if p < p1 then
    Result := SplitString(' ', st)
  else
    Result := SplitString(#9, st);
end;

function WildContains(const a, b: string): Boolean;
var
  p, offs, l, l1: Integer;
begin
  l := Length(a);
  l1 := Length(b);

  if (l1 = 0) or (l = 0) then
  begin
    Result := False;
    Exit;
  end;

  if b[l1] = '*' then
  begin
    if l1 = 1 then
      Result := True
    else
      Result := AnsiContainsStr(a, Copy(b, 1, l1 - 1));
    Exit;
  end;

  if b[1] = '*' then
  begin
    Result := AnsiContainsStr(a, Copy(b, 2, l1 - 1));
    Exit;
  end;

  offs := 1;
  repeat
    p := PosEx(b, a, offs);
    offs := 0;
    if p > 0 then
    begin
      if (p > 1) and (a[p - 1] in ['A'..'Z', '0'..'9']) then
      begin
        offs := p + 1;
        p := 0;
        Continue;
      end;

      if ((p + l1) < l) and (a[p + l1] in ['A'..'Z', '0'..'9']) then
      begin
        offs := p + l1 + 1;
        p := 0;
        Continue;
      end;
    end;
  until (p <> 0) or (offs = 0);

  Result := p <> 0;
end;

function SearchStringArray(arr: array of string; const st: string): Integer;

  function bsearch(s, e: Integer): Integer;
  var
    m, c: Integer;
  begin
    if s <= e then
    begin
      m := s + (e - s) div 2;

      c := CompareText(st, arr[m]);

      if c = 0 then
        Result := m
      else
        if c > 0 then
          Result := bsearch(m + 1, e)
        else
          Result := bsearch(s, m - 1);
    end
    else
      Result := -1;
  end;

begin
  Result := bsearch(Low(arr), High(arr));
end;

function StringArrayContains(arr: array of string; const st: string): Boolean;
begin
  Result := SearchStringArray(arr, st) >= 0;
end;

function SearchQuotedString(const st: string; const delims: string; quote: Char; brk: Char): Integer;
var
  p: Integer;
  c: Char;
  inQuote: Boolean;
  l: Integer;
begin
  Result := 0;
  l := Length(st);
  inQuote := False;

  for p := 1 to l do
  begin
    c := st[p];

    if c = brk then
      Break
    else
      if c = quote then
        InQuote := not InQuote
      else
        if not InQuote and (Pos(c, delims) > 0) then
        begin
          Result := p;
          Break;
        end;
  end;
end;

function SearchRQuotedString(const st: string; const delims: string; quote: Char; brk: Char): Integer;
var
  p: Integer;
  c: Char;
  inQuote: Boolean;
  l: Integer;
begin
  Result := 0;
  l := Length(st);
  inQuote := False;

  for p := 1 to l do
  begin
    c := st[p];

    if c = brk then
      Break
    else
      if c = quote then
        InQuote := not InQuote
      else
        if not InQuote and (Pos(c, delims) > 0) then
          Result := p;
  end;
end;

end.

