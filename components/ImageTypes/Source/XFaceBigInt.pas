unit XFaceBigInt;

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
  XFaceConstants;

type

  { IXFaceBigInt }

  IXFaceBigInt = interface(IInterface)
    function BigWrite() : string;
    function BigPop(const P: array of TProbability) : Integer;

    procedure BigClear();
    procedure BigRead(const AString : string);
    (* Multiply a by B storing the result in B *)
    procedure BigMul(a : Integer);
    (* Divide B by a storing the result in B and the remainder in the word
     * pointer to by r *)
    procedure BigDiv(a : Integer; out AReminder : Integer);
    (* Add to a to B storing the result in B *)
    procedure BigAdd(a : Integer);
    (* Print a BigInt in HexaDecimal *)
    procedure BigPrint();
    (* Subtract a from B storing the result in B *)
    procedure BigSub(a : Integer);
    procedure BigPush(const AProbability: TProbability);
  end;

type

  { TXFaceBigInt }

  TXFaceBigInt = class(TInterfacedObject, IXFaceBigInt)
  private
    FCount : Integer;
    FBigByte : array[0..XFaceConstants.cMAX_WORDS - 1] of Byte;
  public
    constructor Create();

    function BigWrite() : string;
    function BigPop(const P: array of TProbability) : Integer;

    procedure BigClear();
    procedure BigRead(const AString : string);
    procedure BigMul(AValue : Integer);
    procedure BigDiv(AValue : Integer; out ARemainder : Integer);
    procedure BigAdd(AValue : Integer);
    procedure BigPrint();
    procedure BigSub(AValue : Integer);
    procedure BigPush(const AProbability: TProbability);
  end;

implementation

uses
  SysUtils;

{ TXFaceBigInt }

constructor TXFaceBigInt.Create();
begin
  inherited Create();
  BigClear();
end;

procedure TXFaceBigInt.BigAdd(AValue : Integer);
var
  LCarry: Integer;
  LIdx: Integer;
begin
  AValue := AValue and XFaceConstants.cWORD_MASK;
  if (AValue = 0) then
    Exit;

  LIdx := 0;
  LCarry := AValue;
  while (LIdx < FCount) and (LCarry <> 0) do
  begin
    LCarry := LCarry + FBigByte[LIdx];
    FBigByte[LIdx] := LCarry and XFaceConstants.cWORD_MASK;
    LCarry := LCarry shr XFaceConstants.cBITS_PER_WORD;
    Inc(LIdx, 1);
  end;

  if (LIdx = FCount) and
     (LCarry <> 0) then
  begin
    if (FCount >= XFaceConstants.cMAX_WORDS) then
      raise EXFaceExcessException.Create('Bigint overflow in carry over in BigAdd');

    Inc(FCount);
    FBigByte[FCount - 1] := LCarry and XFaceConstants.cWORD_MASK;
  end;
end;

procedure TXFaceBigInt.BigClear();
var
  lp: Integer;
begin
  FCount := 0;
  for lp := Low(FBigByte) to High(FBigByte) do
    FBigByte[lp] := 0;
end;

procedure TXFaceBigInt.BigDiv(AValue : Integer; out ARemainder : Integer);
var
  LCarry: Integer;
  d: Integer;
  lp: Integer;
begin
  AValue := AValue and XFaceConstants.cWORD_MASK;
  if (AValue = 1) or
     (FCount = 0) then
  begin
    ARemainder := 0;
    Exit;
  end;

  if (AValue = 0) then (* treat this as a == cWORDCARRY *)
  begin (* and just shift everything right a WORD *)
    ARemainder := FBigByte[0];
    Dec(FCount, 1);
    for lp := 0 to FCount - 1 do
      FBigByte[lp] := FBigByte[lp + 1];
    FBigByte[FCount] := 0;
    Exit;
  end;

  LCarry := 0;
  for lp := (FCount - 1) downto 0 do
  begin
    LCarry := LCarry shl XFaceConstants.cBITS_PER_WORD;
    LCarry := LCarry + FBigByte[lp];
    d := LCarry div AValue;
    LCarry := LCarry mod AValue;
    FBigByte[lp] := d and XFaceConstants.cWORD_MASK;
  end;
  ARemainder := LCarry;

  if (FBigByte[FCount - 1] = 0) then
    Dec(FCount, 1);
end;

procedure TXFaceBigInt.BigMul(AValue : Integer);
var
  LCarry: Integer;
  lp: Integer;
begin
  AValue := AValue and XFaceConstants.cWORD_MASK;
  if (AValue = 1) or
     (FCount = 0) then
    Exit;

  if (AValue = 0) then (* treat this as a == cWORDCARRY *)
  begin (* and just shift everything left a WORD *)
    if (FCount >= XFaceConstants.cMAX_WORDS) then
      raise EXFaceExcessException.Create('Bigint overflow in shift left in BigMul');

    Inc(FCount);
    for lp := (FCount - 1) downto 1 do
      FBigByte[lp] := FBigByte[lp - 1];
    FBigByte[0] := 0;
    Exit;
  end;

  LCarry := 0;
  for lp := 0 to (FCount - 1) do
  begin
    LCarry := LCarry + FBigByte[lp] * AValue;
    FBigByte[lp] := LCarry and XFaceConstants.cWORD_MASK;
    LCarry := LCarry shr XFaceConstants.cBITS_PER_WORD;
  end;

  if (LCarry <> 0) then
  begin
    if (FCount >= XFaceConstants.cMAX_WORDS) then
      raise EXFaceExcessException.Create('Bigint overflow in carry over in BigMul');

    Inc(FCount);
    FBigByte[FCount - 1] := LCarry;
  end;
end;

function TXFaceBigInt.BigPop(const P : array of TProbability) : Integer;
var
  LIdx: Integer;
  LTmp : Integer;
begin
  BigDiv(0, LTmp);

  LIdx := 0;
  while (LTmp < p[LIdx].p_offset) or
        (LTmp >= Integer(p[LIdx].p_range + p[LIdx].p_offset)) do
  begin
    Inc(LIdx, 1);
  end;

  BigMul(p[LIdx].p_range);
  BigAdd(LTmp - p[LIdx].p_offset);
  Result := LIdx;
end;

procedure TXFaceBigInt.BigPrint();
var
  c: string;
  LCount: Integer;
  LIdx: Integer;
  w: Integer;
begin
  LCount := 0;
  LIdx := FCount;
  w := FCount;
  while (LIdx > 0) do
  begin
    Dec(LIdx, 1);
    Dec(w, 1);
    c := IntToHex(FBigByte[w], 2);
    Write(ErrOutput, c);
    Inc(LCount, 1);
    if (LCount >= 36) then
    begin
      Writeln(ErrOutput, '');
      LCount := 0;
    end;
  end;
  Writeln(ErrOutput, '');
end;

procedure TXFaceBigInt.BigPush(const AProbability : TProbability);
var
  LTmp : Integer;
begin
  BigDiv(AProbability.p_range, LTmp);
  BigMul(0);
  BigAdd(LTmp + AProbability.p_offset);
end;

procedure TXFaceBigInt.BigSub(AValue : Integer);
var
  LCarry: Integer;
  LIdx: Integer;
  w: Integer;
begin
  AValue := AValue and XFaceConstants.cWORD_MASK;
  if (AValue = 0) then
    Exit;
  LIdx := 1;
  w := FBigByte[0];
  LCarry := w - AValue;
  FBigByte[0] := (LCarry and XFaceConstants.cWORD_MASK);
  while ((LCarry and XFaceConstants.cWORD_CARRY) <> 0) do
  begin
    if (LIdx >= FCount) then
      raise EXFaceExcessException.Create('Bigint underflow in carry over in BigSub');

    LCarry := FBigByte[LIdx] - 1;
    FBigByte[LIdx] := (LCarry and XFaceConstants.cWORD_MASK);
    Inc(LIdx, 1);
  end;

  if (LIdx = FCount) and
     (FBigByte[LIdx - 1] = 0) and
     (LIdx > 0) then
  begin
    Dec(FCount, 1);
  end;
end;

function TXFaceBigInt.BigWrite() : string;
var
  LTmp: Integer;
begin
  Result := '';
  while (FCount > 0) do
  begin
    BigDiv(XFaceConstants.cNUM_PRINTS, LTmp);
    Result := Chr(XFaceConstants.cFIRST_PRINT + LTmp) + Result;
  end;
end;

procedure TXFaceBigInt.BigRead(const AString : string);
var
  lp: Integer;
  LChar : Byte;
begin
  for lp := 1 to Length(AString) do
  begin
    LChar := Ord(AString[lp]);
    if (LChar < XFaceConstants.cFIRST_PRINT) or
       (LChar > XFaceConstants.cLAST_PRINT) then
    begin
      Continue;
    end;
    BigMul(XFaceConstants.cNUM_PRINTS);
    BigAdd(LChar - XFaceConstants.cFIRST_PRINT);
  end;
end;

end.

