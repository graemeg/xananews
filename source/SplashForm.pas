(*======================================================================*
 | SplashForm unit for NewsReader3                                      |
 |                                                                      |
 | Display the splash screen                                            |
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

unit SplashForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Jpeg, ExtCtrls, NewsGlobals;

type
  TfmSplash = class(TForm)
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSplash: TfmSplash;

implementation

{$R *.DFM}

procedure TfmSplash.FormCreate(Sender: TObject);
begin
  if DebugHook <> 0 then
    FormStyle := fsNormal;
end;

procedure TfmSplash.FormShow(Sender: TObject);
var
  strm : TResourceStream;
  jpeg : TJPegImage;
  bmpCanvas : TCanvas;
  h, x, y : Integer;
  st : string;
  bmp : TBitmap;
begin
  jpeg := Nil;
  strm := TResourceStream.Create(hInstance, 'SPLASH', 'JPEG');
  try
    jpeg := TJPegImage.Create;
    jpeg.LoadFromStream(strm);
    bmp := TBitmap.Create;
    try
      Image1.Picture.Graphic := bmp
    finally
      bmp.Free;
    end;
    Image1.Picture.Bitmap.Width := jpeg.Width;
    h := JPeg.Height;
    Image1.Picture.Bitmap.Height := h;
    Image1.Picture.Bitmap.PixelFormat := pf24Bit;
    bmpCanvas := Image1.Picture.Bitmap.Canvas;
    bmpCanvas.Draw(0, 0, jpeg);


    bmpCanvas.Font.Height := 18;
    bmpCanvas.Font.Name := 'Arial';
    bmpCanvas.Font.Style := [fsBold];
    SetBkMode (bmpCanvas.Handle, TRANSPARENT);
    st := 'Version ' + ProductVersion;
    x := jpeg.Width - bmpCanvas.TextWidth (st) - 10;
    y := h - 18;

//    bmpCanvas.Font.Color := clMaroon;
//    bmpCanvas.TextOut(x + 1, y + 1, st);
    bmpCanvas.Font.Color := clWhite;
    bmpCanvas.TextOut(x-1, y-1, st);
//    bmpCanvas.Font.Color := clRed;
//    bmpCanvas.TextOut(x, y, st);
  finally
    strm.Free;
    jpeg.Free;
  end
end;

end.
