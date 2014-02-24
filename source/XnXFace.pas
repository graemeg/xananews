unit XnXFace;

interface

uses
  Graphics,
  XFace,
  XFaceInterfaces,
  XFaceTransferInGraphic,
  XFaceTransferOutAscii,
  XFaceTransferOutBitmap,
  XFaceTransferOutHexString;

function XFaceToBitmap(const XFace: string; Bitmap: TBitmap): Integer;
function BitmapToXFace(Bitmap: TBitmap; var XFace: string): Integer;

implementation

function XFaceToBitmap(const XFace: string; Bitmap: TBitmap): Integer;
var
  LXFace: IXFace;
  LXFaceTransferOut: IXFaceTransferOut;
begin
  Result := 0;
  try
    LXFaceTransferOut := TXFaceTransferOutBitmap.Create();
    LXFace := TXFace.Create;
    LXFace.UncompressXFace(XFace, LXFaceTransferOut);

    Bitmap.Assign((LXFaceTransferOut as ITransferOutBitmapAccess).Bitmap);
  except
    Result := -1;
  end;
end;

function BitmapToXFace(Bitmap: TBitmap; var XFace: string): Integer;
var
  LXFace: IXFace;
  LXFaceTransferIn: IXFaceTransferIn;
begin
  Result := 0;
  try
    LXFaceTransferIn := TXFaceTransferInGraphic.Create(Bitmap);
    LXFace := TXFace.Create;
    XFace := LXFace.CompressXFace(LXFaceTransferIn);
  except
    Result := -1;
  end;
end;

end.
