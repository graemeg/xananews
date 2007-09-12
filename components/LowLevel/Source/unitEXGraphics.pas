(*======================================================================*
 | unitExGraphics unit                                                  |
 |                                                                      |
 | Simple graphics functions to convert HSL to RGB.  Provides 2-color   |
 | 256 color palettes                                                   |
 |                                                                      |
 | nb.                                                                  |
 |                                                                      |
 | The functions are deprecated.  You should use the equivalent         |
 | functions in the standard Delphi GraphUtil unit.                     |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 2.0      23/2/2000   CPWW  Original                                  |
 *======================================================================*)

unit unitEXGraphics;

interface

uses Windows, Sysutils, Classes, Graphics, clipbrd, commctrl, controls;

var
  SystemPalette256 : HPALETTE;  // 256 color 'web' palette.
  SystemPalette2 : HPALETTE;

implementation

function WebPalette: HPalette;
type
  TLogWebPalette	= packed record
    palVersion		: word;
    palNumEntries	: word;
    PalEntries		: array [0..5,0..5,0..5] of TPaletteEntry;
    MonoEntries         : array [0..23] of TPaletteEntry;
    StdEntries          : array [0..15] of TPaletteEntry;
  end;
var
  r, g, b		: byte;
  LogWebPalette		: TLogWebPalette;
  LogPalette		: TLogpalette absolute LogWebPalette; // Stupid typecast
begin
  with LogWebPalette do
  begin
    GetPaletteEntries (SystemPalette16, 0, 16, StdEntries);
    palVersion:= $0300;
    palNumEntries:= 256;

    g := 10;
    for r := 0 to 23 do
    begin
      MonoEntries [r].peRed := g;
      MonoEntries [r].peGreen := g;
      MonoEntries [r].peBlue := g;
      MonoEntries [r].peFlags := 0;
      Inc (g, 10)
    end;

    for r:=0 to 5 do
      for g:=0 to 5 do
        for b:=0 to 5 do
        begin
          with PalEntries[r,g,b] do
          begin
            peRed := 51 * r;
            peGreen := 51 * g;
            peBlue := 51 * b;
            peFlags := 0;
          end;
        end;
  end;
  Result := CreatePalette(Logpalette);
end;

(*----------------------------------------------------------------------------*
 | function Create2ColorPalette;                                              |
 |                                                                            |
 | Does what it says on the tin..                                             |
 *----------------------------------------------------------------------------*)
function Create2ColorPalette : HPALETTE;
const
  palColors2 : array [0..1] of TColor = ($000000, $ffffff);
var
  logPalette : PLogPalette;
  i, c : Integer;

begin
  GetMem (logPalette, sizeof (logPalette) + 2 * sizeof (PALETTEENTRY));

  try
    logPalette^.palVersion := $300;
    logPalette^.palNumEntries := 2;
{$R-}
    for i := 0 to 1 do
      with logPalette^.palPalEntry [i] do
      begin
        c := palColors2 [i];

        peRed := c and $ff;
        peGreen := c shr 8 and $ff;
        peBlue :=  c shr 16 and $ff
      end;
{$R+}
    result := CreatePalette (logPalette^);
  finally
    FreeMem (logPalette)
  end
end;

initialization
  SystemPalette256 := WebPalette;
  SystemPalette2 := Create2ColorPalette;
finalization
  DeleteObject (SystemPalette2);
  DeleteObject (SystemPalette256);
end.
