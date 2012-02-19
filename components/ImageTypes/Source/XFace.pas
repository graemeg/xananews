unit XFace;

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
  XFaceConstants,
  XFaceBigInt,
  XFaceBitMap,
  XFaceProbabilityBuffer,
  XFaceInterfaces;

type

  { IXFace }

  IXFace = interface(IInterface)
    ['{03DC306E-ABCB-4A16-92D8-D1D82297EB23}']
    procedure UncompressXFace(const AString : string; const AXFaceTransferOut : IXFaceTransferOut);
    function CompressXFace(const AXFaceTransferIn : IXFaceTransferIn) : string;
  end;


type

  { TXFace }

  TXFace = class(TInterfacedObject, IXFace)
  private
    procedure Gen(const LF1 : IXFaceBitMap; const LF : IXFaceBitMap);

    procedure UnGenFace(const AXFaceBitMap : IXFaceBitMap);
    procedure UnCompAll(const AString: string; const AXFaceBitMap : IXFaceBitMap);
    procedure ReadFace(const AXFaceTransferIn : IXFaceTransferIn; const AXFaceBitMap : IXFaceBitMap);

    procedure GenFace(const AXFaceBitMap : IXFaceBitMap);
    function CompAll(const AXFaceBitMap : IXFaceBitMap) : string;
    procedure WriteFace2(const AXFaceBitMap : IXFaceBitMap; const AXFaceTransferOut : IXFaceTransferOut);
  public
    procedure UncompressXFace(const AString : string; const AXFaceTransferOut : IXFaceTransferOut);

    function CompressXFace(const AXFaceTransferIn : IXFaceTransferIn) : string;
  end;


implementation

uses
  SysUtils;

{ TXFace }

function TXFace.CompAll(const AXFaceBitMap : IXFaceBitMap): string;
var
  LXFaceBigInt : IXFaceBigInt;
  LXFaceProbabilityBuffer : IXFaceProbabilityBuffer;
begin
  LXFaceProbabilityBuffer := TXFaceProbabilityBuffer.Create;

  AXFaceBitMap.Compress(0, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + 16, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + 32, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + XFaceConstants.cWIDTH * 16, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + XFaceConstants.cWIDTH * 16 + 16, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + XFaceConstants.cWIDTH * 16 + 32, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + XFaceConstants.cWIDTH * 32, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + XFaceConstants.cWIDTH * 32 + 16, 16, 16, 0, LXFaceProbabilityBuffer);
  AXFaceBitMap.Compress(0 + XFaceConstants.cWIDTH * 32 + 32, 16, 16, 0, LXFaceProbabilityBuffer);

  LXFaceBigInt := TXFaceBigInt.Create;
  LXFaceBigInt.BigClear();

  while LXFaceProbabilityBuffer.HasNext() do
  begin
    LXFaceBigInt.BigPush(LXFaceProbabilityBuffer.Pop());
  end;

  Result := LXFaceBigInt.BigWrite();
end;

function TXFace.CompressXFace(const AXFaceTransferIn : IXFaceTransferIn) : string;
var
  LXFaceBitMap : IXFaceBitMap;
begin
  LXFaceBitMap := TXFaceBitMap.Create;
  ReadFace(AXFaceTransferIn, LXFaceBitMap);
  GenFace(LXFaceBitMap);
  Result := CompAll(LXFaceBitMap);
end;

procedure TXFace.Gen(const LF1 : IXFaceBitMap; const LF : IXFaceBitMap);
var
  h: Integer;
  i: Integer;
  j: Integer;
  k: Integer;
  l: Integer;
  m: Integer;
begin
  for j := 0 to Pred(XFaceConstants.cHEIGHT) do
  begin
    for i := 0 to Pred(XFaceConstants.cWIDTH) do
    begin
      h := i + j * XFaceConstants.cHEIGHT;
      k := 0;
      for l := i - 2 to i + 2 do
      begin
        for m := j - 2 to j do
        begin
          if (l >= i) and (m = j) then
            continue;
          if (l > 0) and (l <= XFaceConstants.cWIDTH) and (m > 0) then
            if LF1.F[l + m * XFaceConstants.cWIDTH] <> 0 then
              k := k * 2 + 1
            else
              k := k * 2;
        end;
      end;

      case i of
        1 :
          begin
            case j of
              1 : LF.F[h] := LF.F[h] xor g_22[k];
              2 : LF.F[h] := LF.F[h] xor g_21[k];
              else
                LF.F[h] := LF.F[h] xor g_20[k];
            end;
          end;
        2 :
          begin
            case j of
              1 : LF.F[h] := LF.F[h] xor g_12[k];
              2 : LF.F[h] := LF.F[h] xor g_11[k];
              else
                LF.F[h] := LF.F[h] xor g_10[k];
            end;
          end;
        XFaceConstants.cWIDTH - 1 :
          begin
            case j of
              1 : LF.F[h] := LF.F[h] xor g_42[k];
              2 : LF.F[h] := LF.F[h] xor g_41[k];
              else
                LF.F[h] := LF.F[h] xor g_40[k];
            end;
          end;
        XFaceConstants.cWIDTH :
          begin
            case j of
              1 : LF.F[h] := LF.F[h] xor g_32[k];
              2 : LF.F[h] := LF.F[h] xor g_31[k];
              else
                LF.F[h] := LF.F[h] xor g_30[k];
            end;
          end;
        else
          begin
            case j of
              1 : LF.F[h] := LF.F[h] xor g_02[k];
              2 : LF.F[h] := LF.F[h] xor g_01[k];
              else
                LF.F[h] := LF.F[h] xor g_00[k];
            end;
          end;
      end;
    end;
  end;
end;

procedure TXFace.GenFace(const AXFaceBitMap : IXFaceBitMap);
var
  LXFaceBitMap : IXFaceBitMap;
begin
  LXFaceBitMap := TXFaceBitMap.Create(AXFaceBitMap);
  Gen(LXFaceBitMap, AXFaceBitMap);
end;

procedure TXFace.ReadFace(const AXFaceTransferIn : IXFaceTransferIn;
  const AXFaceBitMap : IXFaceBitMap);
var
  LIdx : Cardinal;
  LByte : Byte;
  c2: Integer;
  t2: Integer;
begin
  AXFaceTransferIn.StartTransfer();
  try
    t2 := 0;
    for LIdx := 1 to (XFaceConstants.cPIXELS div XFaceConstants.cBITS_PER_WORD) do
    begin
      LByte := AXFaceTransferIn.TransferIn(LIdx);
      c2 := 1 shl (XFaceConstants.cBITS_PER_WORD - 1);
      while c2 <> 0 do
      begin
        if (LByte and c2 <> 0) then
          AXFaceBitMap.F[t2] := 1
        else
          AXFaceBitMap.F[t2] := 0;
        c2 := c2 shr 1;
        Inc(t2, 1);
      end;
    end;
  finally
    AXFaceTransferIn.EndTransfer();
  end;
end;

procedure TXFace.UnCompAll(const AString: string; const AXFaceBitMap : IXFaceBitMap);
var
  LXFaceBigInt : IXFaceBigInt;
begin
  LXFaceBigInt := TXFaceBigInt.Create;
  LXFaceBigInt.BigClear();
  LXFaceBigInt.BigRead(AString);

  AXFaceBitMap.Clear;

  AXFaceBitMap.UnCompress(0, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + 16, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + 32, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + XFaceConstants.cWIDTH * 16, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + XFaceConstants.cWIDTH * 16 + 16, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + XFaceConstants.cWIDTH * 16 + 32, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + XFaceConstants.cWIDTH * 32, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + XFaceConstants.cWIDTH * 32 + 16, 16, 16, 0, LXFaceBigInt);
  AXFaceBitMap.UnCompress(0 + XFaceConstants.cWIDTH * 32 + 32, 16, 16, 0, LXFaceBigInt);
end;

procedure TXFace.UncompressXFace(const AString : string;
  const AXFaceTransferOut : IXFaceTransferOut);
var
  LXFaceBitMap : IXFaceBitMap;
begin
  LXFaceBitMap := TXFaceBitMap.Create;
  UnCompAll(AString, LXFaceBitMap);
  UnGenFace(LXFaceBitMap);
  WriteFace2(LXFaceBitMap, AXFaceTransferOut);
end;

procedure TXFace.UnGenFace(const AXFaceBitMap : IXFaceBitMap);
begin
  Gen(AXFaceBitMap, AXFaceBitMap);
end;

procedure TXFace.WriteFace2(const AXFaceBitMap : IXFaceBitMap; const AXFaceTransferOut : IXFaceTransferOut);
var
  bits: Integer;
  LDigits: Integer;
  LByte : Byte;
  s: Integer;
begin
  AXFaceTransferOut.StartTransfer();
  try
    bits := 0;
    LDigits := 0;
    LByte := 0;
    s := 0;
    while s < XFaceConstants.cPIXELS do
    begin
      if AXFaceBitMap.F[s] <> 0 then
        LByte := LByte * 2 + 1
      else
        LByte := LByte * 2;

      Inc(s, 1);
      inc(bits, 1);
      if (bits = XFaceConstants.cBITS_PER_WORD) then
      begin
        inc(LDigits, 1);
        AXFaceTransferOut.TransferOut(LByte, LDigits);
        bits := 0;
        LByte := 0;
      end;
    end;
  finally
    AXFaceTransferOut.EndTransfer();
  end;
end;

end.

