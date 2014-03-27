unit XFaceBitMap;

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
  XFaceBigInt,
  XFaceProbabilityBuffer,
  XFaceConstants;

type

  { IXFaceBitMap }
  (* internal face representation - 1 char/byte per pixel is faster *)
  IXFaceBitMap = interface(IInterface)
    ['{E3BF07EA-0A6C-4A7B-9261-1FC1CFC98A8D}']
    function GetF(const AIndex : Cardinal) : Byte;
    procedure SetF(const AIndex : Cardinal; const AValue : Byte);

    function AllWhite(const AOffset : integer; const AWidth : integer; const AHeight : integer) : Boolean;
    function AllBlack(const AOffset : integer; AWidth : integer; AHeight : integer) : Boolean;
    function Same(AOffset : integer; AWidth : integer; AHeight : integer) : Boolean;

    procedure PushGreys(const AOffset : integer; AWidth : integer; AHeight : integer; const AProbabilityBuffer : IXFaceProbabilityBuffer);
    procedure Compress(const AOffset : integer; AWidth : integer; AHeight : integer; ALevel : integer; const AProbabilityBuffer : IXFaceProbabilityBuffer);

    procedure PopGreys(const AOffset : integer; AWidth : integer; AHeight : integer; const AXFaceBigInt : IXFaceBigInt);
    procedure UnCompress(const AOffset : integer; AWidth : integer; AHeight : integer; ALevel : integer; const AXFaceBigInt : IXFaceBigInt);
    procedure Clear;

    property F[const AIndex : Cardinal] : Byte read GetF write SetF;
  end;

type

  { TXFaceBitMap }

  TXFaceBitMap = class(TInterfacedObject, IXFaceBitMap)
  private
    FF : array[0..XFaceConstants.cPIXELS - 1] of Byte;
    function GetF(const AIndex: Cardinal): Byte;
    procedure SetF(const AIndex: Cardinal; const AValue: Byte);
  public
    constructor Create(); overload;
    constructor Create(const AXFaceBitMap : IXFaceBitMap); overload;

    function AllWhite(const AOffset : integer; const AWidth : integer; const AHeight : integer) : Boolean;
    function AllBlack(const AOffset : integer; AWidth : integer; AHeight : integer) : Boolean;
    function Same(AOffset : integer; AWidth : integer; AHeight : integer) : Boolean;

    procedure Clear;

    procedure PopGreys(const AOffset : integer; AWidth : integer; AHeight : integer; const AXFaceBigInt : IXFaceBigInt);
    procedure UnCompress(const AOffset : integer; AWidth : integer; AHeight : integer; ALevel : integer; const AXFaceBigInt : IXFaceBigInt);

    procedure PushGreys(const AOffset : integer; AWidth : integer; AHeight : integer; const AProbabilityBuffer : IXFaceProbabilityBuffer);
    procedure Compress(const AOffset : integer; AWidth : integer; AHeight : integer; ALevel : integer; const AProbabilityBuffer : IXFaceProbabilityBuffer);

    property F[const AIndex : Cardinal] : Byte read GetF write SetF;
  end;


implementation

{ TXFaceBitMap }

function TXFaceBitMap.GetF(const AIndex : Cardinal) : Byte;
begin
  Result := FF[AIndex];
end;

procedure TXFaceBitMap.SetF(const AIndex : Cardinal; const AValue : Byte);
begin
  FF[AIndex] := AValue;
end;

constructor TXFaceBitMap.Create();
begin
  inherited Create();
  Clear;
end;

constructor TXFaceBitMap.Create(const AXFaceBitMap : IXFaceBitMap);
var
  lp : Integer;
begin
  Create();
  for lp := Low(FF) to High(FF) do
    FF[lp] := AXFaceBitMap.F[lp];
end;

function TXFaceBitMap.AllWhite(const AOffset : integer; const AWidth : integer;
  const AHeight : integer) : Boolean;
begin
  Result := (F[AOffset] = 0) and Same(AOffset, AWidth, AHeight);
end;

function TXFaceBitMap.AllBlack(const AOffset : integer; AWidth : integer;
  AHeight : integer) : Boolean;
begin
  if (AWidth > 3) then
  begin
    AWidth := AWidth div 2;
    AHeight := AHeight div 2;

    Result := AllBlack(AOffset, AWidth, AHeight) and
              AllBlack(AOffset + AWidth, AWidth, AHeight) and
              AllBlack(AOffset + XFaceConstants.cWIDTH * AHeight, AWidth, AHeight) and
              AllBlack(AOffset + XFaceConstants.cWIDTH * AHeight + AWidth, AWidth, AHeight);
  end
  else
    Result := (F[AOffset] <> 0) or
              (F[AOffset + 1] <> 0) or
              (F[AOffset + XFaceConstants.cWIDTH] <> 0) or
              (F[AOffset + XFaceConstants.cWIDTH + 1] <> 0);
end;

function TXFaceBitMap.Same(AOffset : integer; AWidth : integer; AHeight : integer) : Boolean;
var
  row: Integer;
  LValue: Integer;
  x: Integer;
begin
  Result := True;
  LValue := F[AOffset];
  while (AHeight > 0) do
  begin
    Dec(AHeight, 1);
    row := AOffset;
    x := AWidth;
    while (x > 0) do
    begin
      Dec(x, 1);
      if (F[row] <> LValue) then
      begin
        Result := False;
        Exit;
      end;
      Inc(row, 1);
    end;
    Inc(AOffset, XFaceConstants.cWIDTH);
  end;
end;

procedure TXFaceBitMap.PopGreys(const AOffset : integer; AWidth : integer;
  AHeight : integer; const AXFaceBigInt : IXFaceBigInt);
begin
  if (AWidth > 3) then
  begin
    AWidth := AWidth div 2;
    AHeight := AHeight div 2;
    PopGreys(AOffset, AWidth, AHeight, AXFaceBigInt);
    PopGreys(AOffset + AWidth, AWidth, AHeight, AXFaceBigInt);
    PopGreys(AOffset + XFaceConstants.cWIDTH * AHeight, AWidth, AHeight, AXFaceBigInt);
    PopGreys(AOffset + XFaceConstants.cWIDTH * AHeight + AWidth, AWidth, AHeight, AXFaceBigInt);
  end
  else
  begin
    AWidth := AXFaceBigInt.BigPop(XFaceConstants.cFREQUENCIES);

    if ((AWidth and 1) <> 0) then
      F[AOffset] := 1;
    if ((AWidth and 2) <> 0) then
      F[AOffset + 1] := 1;
    if ((AWidth and 4) <> 0) then
      F[AOffset + XFaceConstants.cWIDTH] := 1;
    if ((AWidth and 8) <> 0) then
      F[AOffset + XFaceConstants.cWIDTH + 1] := 1;
  end;
end;

procedure TXFaceBitMap.UnCompress(const AOffset : integer; AWidth : integer;
  AHeight : integer; ALevel : integer; const AXFaceBigInt : IXFaceBigInt);
begin
  case AXFaceBigInt.BigPop(XFaceConstants.cLEVELS[ALevel]) of
    XFaceConstants.cWHITE :
      exit;
    XFaceConstants.cBLACK :
      PopGreys(AOffset, AWidth, AHeight, AXFaceBigInt);
    else
      begin
        AWidth := AWidth div 2;
        AHeight := AHeight div 2;
        Inc(ALevel, 1);
        UnCompress(AOffset, AWidth, AHeight, ALevel, AXFaceBigInt);
        UnCompress(AOffset + AWidth, AWidth, AHeight, ALevel, AXFaceBigInt);
        UnCompress(AOffset + AHeight * XFaceConstants.cWIDTH, AWidth, AHeight, ALevel, AXFaceBigInt);
        UnCompress(AOffset + AWidth + AHeight * XFaceConstants.cWIDTH, AWidth, AHeight, ALevel, AXFaceBigInt);
      end;
  end;
end;

procedure TXFaceBitMap.PushGreys(const AOffset : integer; AWidth : integer;
  AHeight : integer; const AProbabilityBuffer : IXFaceProbabilityBuffer);
begin
  if (AWidth > 3) then
  begin
    AWidth := AWidth div 2;
    AHeight := AHeight div 2;
    PushGreys(AOffset, AWidth, AHeight, AProbabilityBuffer);
    PushGreys(AOffset + AWidth, AWidth, AHeight, AProbabilityBuffer);
    PushGreys(AOffset + XFaceConstants.cWIDTH * AHeight, AWidth, AHeight, AProbabilityBuffer);
    PushGreys(AOffset + XFaceConstants.cWIDTH * AHeight + AWidth, AWidth, AHeight, AProbabilityBuffer);
  end
  else
    AProbabilityBuffer.RevPush(XFaceConstants.cFREQUENCIES[F[AOffset] +
                                                           2 * F[AOffset + 1] +
                                                           4 * F[AOffset + XFaceConstants.cWIDTH] +
                                                           8 * F[AOffset + XFaceConstants.cWIDTH + 1]]);
end;

procedure TXFaceBitMap.Compress(const AOffset : integer; AWidth : integer;
  AHeight : integer; ALevel : integer;
  const AProbabilityBuffer : IXFaceProbabilityBuffer);
begin
  if (AllWhite(AOffset, AWidth, AHeight)) then
  begin
    AProbabilityBuffer.RevPush(XFaceConstants.cLEVELS[Alevel][XFaceConstants.cWHITE]);
    Exit;
  end;
  if (AllBLACK(AOffset, AWidth, AHeight)) then
  begin
    AProbabilityBuffer.RevPush(XFaceConstants.cLEVELS[Alevel][XFaceConstants.cBLACK]);
    PushGreys(AOffset, AWidth, AHeight, AProbabilityBuffer);
    exit;
  end;

  AProbabilityBuffer.RevPush(XFaceConstants.cLEVELS[Alevel][XFaceConstants.cGREY]);

  AWidth := AWidth div 2;
  AHeight := AHeight div 2;
  Inc(Alevel, 1);
  Compress(AOffset, AWidth, AHeight, Alevel, AProbabilityBuffer);
  Compress(AOffset + AWidth, AWidth, AHeight, Alevel, AProbabilityBuffer);
  Compress(AOffset + AHeight * XFaceConstants.cWIDTH, AWidth, AHeight, Alevel, AProbabilityBuffer);
  Compress(AOffset + AWidth + AHeight * XFaceConstants.cWIDTH, AWidth, AHeight, Alevel, AProbabilityBuffer);
end;

procedure TXFaceBitMap.Clear;
var
  lp : Integer;
begin
  for lp := Low(FF) to High(FF) do
    FF[lp] := 0;
end;

end.

