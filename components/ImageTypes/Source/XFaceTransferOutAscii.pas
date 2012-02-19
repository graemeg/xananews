unit XFaceTransferOutAscii;

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
  XFaceTransferOutHexString;

type

  { TXFaceTransferOutAscii }

  TXFaceTransferOutAscii = class(TInterfacedObject, IXFaceTransferOut, ITransferOutStringAccess)
  private
    FHexString : string;
  public
    constructor Create();

    function AsString : string;

    procedure StartTransfer();
    procedure TransferOut(const AByte : Byte; const AIndex : Cardinal);
    procedure EndTransfer();
  end;


implementation

uses
  XFaceConstants,
  StrUtils;

{ TXFaceTransferOutAscii }

constructor TXFaceTransferOutAscii.Create();
begin
  inherited Create();
end;

function TXFaceTransferOutAscii.AsString : string;
begin
  Result := FHexString;
end;

procedure TXFaceTransferOutAscii.StartTransfer();
begin
  FHexString := '+' +
                StringOfChar('-', XFaceConstants.cWIDTH) +
                '+' +
                System.sLineBreak;
end;

procedure TXFaceTransferOutAscii.TransferOut(const AByte : Byte;
  const AIndex : Cardinal);
var
  LIdx : Byte;
begin
  LIdx := 128;

  if (((AIndex - 1) mod XFaceConstants.cBYTES_PER_LINE) = 0) then
    FHexString := FHexString + '|';

  while LIdx <> 0 do
  begin
    FHexString := FHexString + StrUtils.IfThen((AByte and Lidx) = 0, 'X', ' ');
    LIdx := Lidx shr 1;
  end;

  if ((AIndex mod XFaceConstants.cBYTES_PER_LINE) = 0) then
    FHexString := FHexString + '|' + System.sLineBreak;
end;

procedure TXFaceTransferOutAscii.EndTransfer();
begin
  FHexString := FHexString +
                '+' +
                StringOfChar('-', XFaceConstants.cWIDTH) +
                '+' +
                System.sLineBreak;
end;

end.

