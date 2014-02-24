unit XnCoderUUE;

// Replacement unit for Indy 10 IdCoderUUE.

interface

uses
  Windows, Classes, SysUtils, IdGlobal, IdCoder3to4;

type
  TXnDecoderUUE = class(TIdDecoder4to3)
  protected
    procedure InitComponent; override;
  public
    procedure Decode(ASrcStream: TStream; const ABytes: Integer = -1); override;
  end;

  TXnEncoderUUE = class(TIdEncoder3to4)
  private
    FBuffer: TIdBytes;
  protected
    procedure InitComponent; override;
  public
    procedure Encode(ASrcStream, ADestStream: TStream; const ABytes: Integer = -1); override;
  end;


implementation

uses
  IdStream, IdCoderUUE;

{ TXnDecoderUUE }

procedure TXnDecoderUUE.Decode(ASrcStream: TStream; const ABytes: Integer = -1);
var
  LBuffer: TIdBytes;
  LBufSize: Integer;
  LLength: Integer;
begin
  if ASrcStream.Size > 0 then
  begin
    TIdStreamHelper.ReadBytes(ASrcStream, LBuffer, 1);
    LLength := FDecodeTable[Ord(LBuffer[0])];

    LBufSize := IndyLength(ASrcStream, ABytes);
    if LBufSize > 0 then
    begin
      case (LLength mod 3) of
        0: begin
             LLength := (LLength div 3) * 4;
             SetLength(LBuffer, LLength);
             TIdStreamHelper.ReadBytes(ASrcStream, LBuffer, LBufSize);
           end;
        1: begin
             LLength := (LLength div 3) * 4 + 4;
             SetLength(LBuffer, LLength);
             TIdStreamHelper.ReadBytes(ASrcStream, LBuffer, LBufSize);
             LBuffer[LLength - 2] := Ord(FillChar);
             LBuffer[LLength - 1] := Ord(FillChar);
           end;
        2: begin
             LLength := (LLength div 3) * 4 + 4;
             SetLength(LBuffer, LLength);
             TIdStreamHelper.ReadBytes(ASrcStream, LBuffer, LBufSize);
             LBuffer[LLength - 1] := Ord(FillChar);
           end;
      end;
      LBuffer := InternalDecode(LBuffer);
      TIdStreamHelper.Write(FStream, LBuffer);
    end;
  end;
end;

procedure TXnDecoderUUE.InitComponent;
begin
  inherited InitComponent;
  FDecodeTable := GUUEDecodeTable;
  FFillChar := '~';  {Do not Localize}
end;


{ TXnEncoderUUE }

procedure TXnEncoderUUE.Encode(ASrcStream, ADestStream: TStream; const ABytes: Integer = -1);
var
  LBufSize: Integer;
begin
  LBufSize := IndyLength(ASrcStream, ABytes);
  if LBufSize > 0 then
  begin
    SetLength(FBuffer, LBufSize);
    FBuffer[0] := Ord(FCodingTable[Integer(LBufSize)+1]);
    TIdStreamHelper.Write(ADestStream, FBuffer, 1);

    TIdStreamHelper.ReadBytes(ASrcStream, FBuffer, LBufSize);
    FBuffer := InternalEncode(FBuffer);
    TIdStreamHelper.Write(ADestStream, FBuffer, Length(FBuffer));
  end;
end;

procedure TXnEncoderUUE.InitComponent;
begin
  inherited InitComponent;
  FCodingTable := ToBytes(GUUECodeTable);
  FFillChar := GUUECodeTable[1];
end;

end.
