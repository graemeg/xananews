unit XFaceProbabilityBuffer;

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

  { IXFaceProbabilityBuffer }

  IXFaceProbabilityBuffer = interface(IInterface)
    function HasNext() : Boolean;
    function Pop() : TProbability;
    procedure RevPush(const P: TProbability);
  end;

   { TXFaceProbabilityBuffer }

   TXFaceProbabilityBuffer = class(TInterfacedObject, IXFaceProbabilityBuffer)
   private
     (* A stack of probability values *)
     FCount : Cardinal;
     FProbBuf : array[0..XFaceConstants.cPIXELS * 2 - 1] of TProbability;
   public
     constructor Create();

     function HasNext() : Boolean;
     function Pop() : TProbability;

     procedure RevPush(const AProbility : TProbability);
   end;

implementation

uses
  SysUtils;

{ IXFaceProbability }

constructor TXFaceProbabilityBuffer.Create();
var
  lp : Integer;
begin
  inherited Create();

  FCount := 0;
  for lp := Low(FProbBuf) to High(FProbBuf) do
  begin
    FProbBuf[lp].p_offset := 0;
    FProbBuf[lp].p_range := 0;
  end;
end;

function TXFaceProbabilityBuffer.HasNext() : Boolean;
begin
  Result := (FCount > 0);
end;

function TXFaceProbabilityBuffer.Pop() : TProbability;
begin
  Dec(FCount, 1);
  Result := FProbBuf[FCount];
end;

procedure TXFaceProbabilityBuffer.RevPush(const AProbility : TProbability);
begin
  if (FCount >= High(FProbBuf)) then
    raise EXFaceExcessException.Create('TXFace.RevPush overflow');

  FProbBuf[FCount] := AProbility;
  Inc(FCount, 1);
end;

end.

