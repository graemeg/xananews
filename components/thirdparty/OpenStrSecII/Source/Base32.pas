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
unit Base32;

interface

uses
  SysUtils;

const
  Base32Str = 'ABCDEFGHJKLMNPQRSTUVWXYZ23456789- '#9;
//Base32Str = 'ACEFGHJKLMNPQRTUVWXY1234567890+*- '#9;

function Base32ToStr(const Value: string): string;
function StrToBase32(const Value: string; Hyphens: Boolean = True): string;

implementation

function Base32ToStr(const Value: string): string;
var
  C: Integer;
  Res: Word;
  BitPos: Integer;
  I: Integer;
begin
  BitPos := 0;
  Result := '';
  Res := 0;
  for I := Length(Value) downto 1 do begin
    C := Pos(UpperCase(Value[I]),Base32Str) - 1;
    if C < 0 then raise Exception.Create('Illegal char.');
    if C > 31 then Continue;
    Res := Res or (C shl BitPos);
    Inc(BitPos,5);
    if BitPos >= 8 then begin
      Result := Char(Byte(Res and $FF)) + Result;
      Res := Res shr 8;
      Dec(BitPos,8);
    end;
  end;
  if (BitPos > 0) and (Res > 0) then
    Result := Char(Byte(Res and $FF)) + Result;
end;

function StrToBase32(const Value: string; Hyphens: Boolean = True): string;
var
  C, Len: Integer;
  Res: Word;
  BitPos: Integer;
  I: Integer;
begin
  BitPos := 0;
  Result := '';
  Res := 0;
  Len := 0;
  for I := Length(Value) downto 1 do begin
    C := Byte(Value[I]);
    Res := Res or (C shl BitPos);
    Inc(BitPos,8);
    while BitPos >= 5 do begin
      Result := Base32Str[(Res and $1F) + 1] + Result;
      Inc(Len);
      if Hyphens and ((Len mod 5) = 0) then Result := '-' + Result;
      Res := Res shr 5;
      Dec(BitPos,5);
    end;
  end;
  if (BitPos > 0) then
    Result := Base32Str[(Res and $1F) + 1] + Result
  else if (Length(Result) > 0) and (Result[1] = '-') then
    Delete(Result,1,1);
end;

end.
