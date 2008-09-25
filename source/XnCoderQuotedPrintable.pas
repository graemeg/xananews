unit XnCoderQuotedPrintable;

interface

uses
  Classes, IdCoderQuotedPrintable;

type
  TXnEncoderQuotedPrintable = class(TIdEncoderQuotedPrintable)
  public
    procedure Encode(ASrcStream, ADestStream: TStream; const ABytes: Integer = -1); override;
  end;

implementation

uses
  IdGlobal;

{ TXnEncoderQuotedPrintable }

procedure TXnEncoderQuotedPrintable.Encode(ASrcStream, ADestStream: TStream;
  const ABytes: Integer);
begin
  inherited Encode(ASrcStream, ADestStream, ABytes);
  // Remove the extra EOL from the encoded stream.
  if ADestStream.Size >= Length(EOL) then
    ADestStream.Size := ADestStream.Size - Length(EOL);
end;

end.
