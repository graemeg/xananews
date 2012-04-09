unit  XFaceTransferInGraphic;

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
  SysUtils,
  Types;

type

  { THexStrTransferIn }

  TXFaceTransferInGraphic = class(TInterfacedObject, IXFaceTransferIn)
  private
    FBitmap : Graphics.TBitmap;
    FUnsetRGB : Longint;
  public
    constructor Create(const AGraphic : Graphics.TGraphic); reintroduce; overload;
    constructor Create(const AGraphic : Graphics.TGraphic; const ADestPoint : TPoint); reintroduce; overload;
    destructor Destroy; override;

    procedure StartTransfer();
    function TransferIn(const AIndex : Cardinal) : Byte;
    procedure EndTransfer();
  end;

implementation

uses
  XFaceConstants,
  Windows,
  Math,
  XFaceHelper;

{ TXFaceTransferInBitmap }

constructor TXFaceTransferInGraphic.Create(const AGraphic: Graphics.TGraphic);
var
  LPoint : TPoint;
begin
  LPoint := Point((XFaceConstants.cWIDTH - AGraphic.Width) div 2,
                  (XFaceConstants.cHEIGHT - AGraphic.Height) div 2);
  Create(AGraphic, LPoint);
end;

constructor TXFaceTransferInGraphic.Create(const AGraphic: Graphics.TGraphic; const ADestPoint: TPoint);
var
  FUnsetColour : TColor;
begin
  inherited Create;

  FUnsetColour := clWhite;

  FBitmap := TXFaceHelper.CreateXFaceBitmap(FUnsetColour, clBlack);
  FBitmap.Canvas.Draw(ADestPoint.X, ADestPoint.Y, AGraphic);

  FUnsetRGB := Windows.RGB(Windows.GetRValue(Graphics.ColorToRGB(FUnsetColour)),
                           Windows.GetGValue(Graphics.ColorToRGB(FUnsetColour)),
                           Windows.GetBValue(Graphics.ColorToRGB(FUnsetColour)));
end;

destructor TXFaceTransferInGraphic.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TXFaceTransferInGraphic.EndTransfer();
begin
end;

procedure TXFaceTransferInGraphic.StartTransfer();
begin
end;

function TXFaceTransferInGraphic.TransferIn(const AIndex: Cardinal): Byte;
var
  LPixelIdx    : Byte;
  LY           : integer;
  LX           : integer;
  LPixelColour : TColor;
  LPixelRGB    : Longint;
begin
  Result := 0;

  LY := (AIndex - 1) div XFaceConstants.cBYTES_PER_LINE;
  LX := ((AIndex - 1) mod XFaceConstants.cBYTES_PER_LINE) * XFaceConstants.cBITS_PER_WORD;

  LPixelIdx := 1 shl (XFaceConstants.cBITS_PER_WORD - 1);
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

