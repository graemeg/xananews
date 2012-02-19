unit XFaceTransferInMonochromeBitmap;

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
  XFaceInterfaces,
  Graphics,
  SysUtils;

type

  { THexStrTransferIn }

  TXFaceTransferInMonochromeBitmap = class(TInterfacedObject, IXFaceTransferIn)
  private
    FBitmap : Graphics.TBitmap;
    FUnsetRGB : Longint;
  public
    constructor Create(const ABitmap : Graphics.TBitmap);
    destructor Destroy; override;

    procedure StartTransfer();
    function TransferIn(const AIndex : Cardinal) : Byte;
    procedure EndTransfer();
  end;

type
  EXFaceTransferInMonochromeBitmapBaseException = class(Exception);
  EXFaceTransferInMonochromeBitmapBitmapNotAssigned = class(EXFaceTransferInMonochromeBitmapBaseException);
  EXFaceTransferInMonochromeBitmapBitmapNotMonochrome = class(EXFaceTransferInMonochromeBitmapBaseException);
  EXFaceTransferInMonochromeBitmapWrongSize = class(EXFaceTransferInMonochromeBitmapBaseException);

implementation

uses
  XFaceConstants, Windows;

{ TXFaceTransferInBitmap }

constructor TXFaceTransferInMonochromeBitmap.Create(const ABitmap: Graphics.TBitmap);
begin
  inherited Create();
  FBitmap := Graphics.TBitmap.Create;
  FBitmap.Assign(ABitmap);
end;

destructor TXFaceTransferInMonochromeBitmap.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TXFaceTransferInMonochromeBitmap.EndTransfer();
begin
end;

procedure TXFaceTransferInMonochromeBitmap.StartTransfer();
var
  LMaxLogPalette : Windows.TMaxLogPalette;
  LResult        : Windows.UINT;
begin
  if FBitmap.Empty then
    raise EXFaceTransferInMonochromeBitmapBitmapNotAssigned.Create('Bitmap not assigned');

  if not FBitmap.Monochrome then
    raise EXFaceTransferInMonochromeBitmapBitmapNotMonochrome.Create('Bitmap not monochrome');

  if FBitmap.Width <> XFaceConstants.cWIDTH then
    raise EXFaceTransferInMonochromeBitmapWrongSize.Create(Format('Image not of correct width - %d', [FBitmap.Width]));

  if FBitmap.Height <> XFaceConstants.cHEIGHT then
    raise EXFaceTransferInMonochromeBitmapWrongSize.Create(Format('Image not of correct height - %d', [FBitmap.Height]));

  if FBitmap.PixelFormat <> pf1bit then
    raise EXFaceTransferInMonochromeBitmapWrongSize.Create(Format('Image not of correct depth - %d', [Ord(FBitmap.PixelFormat)]));

  LMaxLogPalette.palVersion := $0300;
  LMaxLogPalette.palNumEntries := 1;
  LResult := Windows.GetPaletteEntries(FBitmap.Palette, 0, 1, PLogPalette(@LMaxLogPalette)^.palPalEntry);
  if LResult = 0 then
    if Windows.GetLastError() <> 0 then
      SysUtils.RaiseLastOSError();

  FUnsetRGB := RGB(LMaxLogPalette.palPalEntry[0].peRed, LMaxLogPalette.palPalEntry[0].peGreen, LMaxLogPalette.palPalEntry[0].peBlue);
end;

function TXFaceTransferInMonochromeBitmap.TransferIn(const AIndex: Cardinal): Byte;
var
  LPixelIdx : Byte;
  LY : integer;
  LX : integer;
  LPixelColour : TColor;
  LPixelRGB : Longint;
begin
  Result := 0;
  LPixelIdx := 1 shl (XFaceConstants.cBITS_PER_WORD - 1);
  LY := (AIndex - 1) div XFaceConstants.cBYTES_PER_LINE;
  LX := ((AIndex - 1) mod XFaceConstants.cBYTES_PER_LINE) * XFaceConstants.cBITS_PER_WORD;
  while LPixelIdx <> 0 do
  begin
    LPixelColour := FBitmap.Canvas.Pixels[LX, LY];
    LPixelRGB := Graphics.ColorToRGB(LPixelColour);

    if LPixelRGB <> FUnsetRGB then
      Result := Result or LPixelIdx;

    Inc(LX, 1);
    LPixelIdx := LPixelIdx shr 1;
  end;
end;

end.

