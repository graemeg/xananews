unit XFaceTransferOutHexString;

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
  ITransferOutStringAccess = interface(IInterface)
    ['{B760773B-E066-42DA-8533-95184AED52A1}']
    function AsString : string;
  end;

type

  { TXFaceTransferOutHexString }

  TXFaceTransferOutHexString = class(TInterfacedObject, IXFaceTransferOut, ITransferOutStringAccess)
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
  SysUtils;

{ TXFaceTransferOutHexString }

constructor TXFaceTransferOutHexString.Create();
begin
  inherited Create();
end;

function TXFaceTransferOutHexString.AsString : string;
begin
  Result := FHexString;
end;

procedure TXFaceTransferOutHexString.StartTransfer();
begin
end;

procedure TXFaceTransferOutHexString.TransferOut(const AByte : Byte; const AIndex : Cardinal);
begin
  FHexString := FHexString + IntToHex(AByte, 2);
end;

procedure TXFaceTransferOutHexString.EndTransfer();
begin
end;

end.

