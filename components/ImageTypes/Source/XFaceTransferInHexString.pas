unit XFaceTransferInHexString;

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
  XFaceInterfaces;

type

  { THexStrTransferIn }

  TXFaceTransferInHexString = class(TInterfacedObject, IXFaceTransferIn)
  private
    FHexString : string;
  public
    constructor Create(const AHexString : string);

    procedure StartTransfer();
    function TransferIn(const AIndex : Cardinal) : Byte;
    procedure EndTransfer();
  end;

implementation

uses
  SysUtils;

{ THexStrTransferIn }

constructor TXFaceTransferInHexString.Create(const AHexString : string);
begin
  inherited Create();
  FHexString := AHexString;
end;

procedure TXFaceTransferInHexString.StartTransfer();
begin
end;

function TXFaceTransferInHexString.TransferIn(const AIndex : Cardinal) : Byte;
begin
  Result := StrToIntDef('0x' + Copy(FHexString, (AIndex - 1) * 2 + 1, 2), 0);
end;

procedure TXFaceTransferInHexString.EndTransfer();
begin
end;

end.

