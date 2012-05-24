unit XFaceInterfaces;

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

type

  { IXFaceTransferIn }

  IXFaceTransferIn = interface(IInterface)
    ['{D737124A-9FE5-471B-8F41-3E5EF5A4A87C}']
    procedure StartTransfer();
    function TransferIn(const AIndex : Cardinal) : Byte;
    procedure EndTransfer();
  end;

type

  { IXFaceTransferOut }

  IXFaceTransferOut = interface(IInterface)
    ['{A3C62660-2E1C-4CB8-8322-096CC0FB67CB}']
    procedure StartTransfer();
    procedure TransferOut(const AByte : Byte; const AIndex : Cardinal);
    procedure EndTransfer();
  end;

implementation

end.

