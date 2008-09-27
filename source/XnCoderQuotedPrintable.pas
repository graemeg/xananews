unit XnCoderQuotedPrintable;

interface

uses
  Classes, IdCoder, IdCoderQuotedPrintable;

type
  TXnDecoderQuotedPrintable = class(TIdDecoder)
  public
    procedure Decode(ASrcStream: TStream; const ABytes: Integer = -1); override;
  end;

  TXnEncoderQuotedPrintable = class(TIdEncoderQuotedPrintable)
  public
    procedure Encode(ASrcStream, ADestStream: TStream; const ABytes: Integer = -1); override;
  end;

implementation

uses
  IdGlobal,
  IdStream;

{ TXnDecoderQuotedPrintable }

procedure TXnDecoderQuotedPrintable.Decode(ASrcStream: TStream; const ABytes: Integer = -1);
var
  LBuffer: TIdBytes;
  i : Integer;
  B, DecodedByte : Byte;
  LBufferLen: Integer;
  LBufferIndex: Integer;
  LPos: integer;

  procedure StripEOLChars;
  var
    j: Integer;
  begin
    for j := 1 to 2 do begin
      if (LBufferIndex >= LBufferLen) or (not ByteIsInEOL(LBuffer, LBufferIndex)) then begin
        Break;
      end;
      Inc(LBufferIndex);
    end;
  end;

  function TrimRightWhiteSpace(const ABuf: TIdBytes): TIdBytes;
  var
    LSaveBytes: TIdBytes;
    li, LLen: Integer;
  begin
    SetLength(LSaveBytes, 0);
    LLen := Length(ABuf);
    for li := Length(ABuf)-1 downto 0 do begin
      case ABuf[li] of
        9, 32: ;
        10, 13:
          begin
            //BGO: TODO: Change this
            InsertByte(LSaveBytes, ABuf[li], 0);
          end;
      else
        begin
          Break;
        end;
      end;
      Dec(LLen);
    end;
    SetLength(Result, LLen + Length(LSaveBytes));
    if LLen > 0 then begin
      CopyTIdBytes(ABuf, 0, Result, 0, LLen);
    end;
    if Length(LSaveBytes) > 0 then begin
      CopyTIdBytes(LSaveBytes, 0, Result, LLen, Length(LSaveBytes));
    end;
  end;

begin
  LBufferLen := IndyLength(ASrcStream, ABytes);
  if LBufferLen <= 0 then begin
    Exit;
  end;
  SetLength(LBuffer, LBufferLen);
  TIdStreamHelper.ReadBytes(ASrcStream, LBuffer, LBufferLen);
  { when decoding a Quoted-Printable body, any trailing
  white space on a line must be deleted, - RFC 1521}
  LBuffer := TrimRightWhiteSpace(LBuffer);
  LBufferLen := Length(LBuffer);
  LBufferIndex := 0;
  while LBufferIndex < LBufferLen do begin
    LPos := ByteIndex(Ord('='), LBuffer, LBufferIndex);
    if LPos = -1 then begin
      if Assigned(FStream) then begin
        TIdStreamHelper.Write(FStream, LBuffer, -1, LBufferIndex);
      end;
      Break;
    end;
    if Assigned(FStream) then begin
      TIdStreamHelper.Write(FStream, LBuffer, LPos-LBufferIndex, LBufferIndex);
    end;
    LBufferIndex := LPos+1;
    // process any following hexidecimal representation
    if LBufferIndex < LBufferLen then begin
      i := 0;
      DecodedByte := 0;
      while LBufferIndex < LBufferLen do begin
        B := LBuffer[LBufferIndex];
        case B of
          48..57: //0-9                                           {Do not Localize}
            DecodedByte := (DecodedByte shl 4) or (B - 48);       {Do not Localize}
          65..70: //A-F                                           {Do not Localize}
            DecodedByte := (DecodedByte shl 4) or (B - 65 + 10);  {Do not Localize}
          97..102://a-f                                           {Do not Localize}
            DecodedByte := (DecodedByte shl 4) or (B - 97 + 10);  {Do not Localize}
        else
          Break;
        end;
        Inc(i);
        Inc(LBufferIndex);
        if i > 1 then begin
          Break;
        end;
      end;
      if i > 0 then begin
        //if =20 + EOL, this is a hard line break after a space
        if (DecodedByte = 32) and (LBufferIndex < LBufferLen) and ByteIsInEOL(LBuffer, LBufferIndex) then begin
          if Assigned(FStream) then begin
            FStream.Write(DecodedByte, 1);
            FStream.Write(EOL, Length(EOL));
//            WriteStringToStream(FStream, Char(DecodedByte) + EOL);
          end;
          StripEOLChars;
        end else begin
          FStream.Write(DecodedByte, 1);
//          WriteStringToStream(FStream, Char(DecodedByte));
        end;
      end else begin
        //ignore soft line breaks -
        StripEOLChars;
      end;
    end;
  end;
end;


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
