unit XFaceHelper;

(*
 *  Compface - 48x48x1 image compression and decompression
 *
 *  Copyright (c) James Ashton - Sydney University - June 1990.
 *
 *  Written 11th November 1989.
 *
 *  Delphi conversion Copyright (c) Nicholas Ring February 2012
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors on inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 *)

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Graphics;

type
  TXFaceHelper = class
  public
    class function CreateXFaceBitmap(const AFirstColour : TColor;
                                     const ASecondColour : TColor) : Graphics.TBitmap;
  end;

implementation

uses
  XFaceConstants,
  Windows;

{ TXFaceHelper }

class function TXFaceHelper.CreateXFaceBitmap(const AFirstColour : TColor;
                                              const ASecondColour : TColor) : Graphics.TBitmap;
var
  LMaxLogPalette : Windows.TMaxLogPalette;
  LRGBColour : Longint;
begin
  Result             := Graphics.TBitmap.Create;
  Result.Monochrome  := True;
  Result.Width       := XFaceConstants.cWIDTH;
  Result.Height      := XFaceConstants.cHEIGHT;
  Result.PixelFormat := pf1bit;

  LMaxLogPalette.palVersion    := $0300;  // "Magic Number" for Windows LogPalette
  LMaxLogPalette.palNumEntries := 2;

  LRGBColour := Graphics.ColorToRGB(AFirstColour);
  LMaxLogPalette.palPalEntry[0].peRed   := Windows.GetRValue(LRGBColour);
  LMaxLogPalette.palPalEntry[0].peGreen := Windows.GetGValue(LRGBColour);
  LMaxLogPalette.palPalEntry[0].peBlue  := Windows.GetBValue(LRGBColour);
  LMaxLogPalette.palPalEntry[0].peFlags := 0;

  LRGBColour := Graphics.ColorToRGB(ASecondColour);
  LMaxLogPalette.palPalEntry[1].peRed   := Windows.GetRValue(LRGBColour);
  LMaxLogPalette.palPalEntry[1].peGreen := Windows.GetGValue(LRGBColour);
  LMaxLogPalette.palPalEntry[1].peBlue  := Windows.GetBValue(LRGBColour);
  LMaxLogPalette.palPalEntry[1].peFlags := 0;

  Result.Palette := Windows.CreatePalette(PLogPalette(@LMaxLogPalette)^);
end;

end.
