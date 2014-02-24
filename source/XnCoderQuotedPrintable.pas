unit XnCoderQuotedPrintable;

// Replacement unit for Indy 10 IdCoderQuotedPrintable.

interface

uses
  Classes, IdCoder, IdCoderQuotedPrintable,
  unitStreamTextReader, XnClasses;

type
  TXnDecoderQuotedPrintable = class(TIdDecoder)
  public
    procedure Decode(ASrcStream: TStream; const ABytes: Integer = -1); override;
  end;

  TXnEncoderQuotedPrintable = class(TIdEncoderQuotedPrintable)
  private
    FAddEOL: Boolean;
  public
    procedure Encode(ASrcStream, ADestStream: TStream; const ABytes: Integer = -1); override;
    procedure EncodeStrings(strings: TAnsiStrings);
    property AddEOL: Boolean read FAddEOL write FAddEOL;
  end;

implementation

uses
  IdGlobal, IdStream, IdGlobalProtocols;

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
            DecodedByte := 13;
            FStream.Write(DecodedByte, 1);
            DecodedByte := 10;
            FStream.Write(DecodedByte, 1);
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

procedure TXnEncoderQuotedPrintable.Encode(ASrcStream, ADestStream: TStream; const ABytes: Integer = -1);
const
  SafeChars = [#33..#60, #62..#126];
  HalfSafeChars = [#32, TAB];
  // Rule #2, #3
var
  CurrentLine: ShortString;
  // this is a shortstring for performance reasons.
  // the lines may never get longer than 76, so even if I go a bit
  // further, they won't go longer than 80 or so
  SourceLine: RawByteString;
  CurrentPos: Integer;
  reader: TStreamTextReader;

  procedure WriteToString(const s: AnsiString);
  var
    SLen: Integer;
  begin
    SLen := Length(s);
    Move(s[1], CurrentLine[CurrentPos], SLen);
//    MoveChars(s, 1, CurrentLine, CurrentPos, SLen);
    Inc(CurrentPos, SLen);
  end;

  procedure FinishLine(AddCRLF: Boolean = True);
  var
    S: string;
  begin
    S := Copy(string(CurrentLine), 1, CurrentPos - 1);
    if AddCRLF then
      S := S + EOL;
    WriteStringToStream(ADestStream, S);
    CurrentPos := 1;
  end;

  function CharToHex(const AChar: AnsiChar): AnsiString;
  begin
    Result := '=' + AnsiString(ByteToHex(Ord(AChar))); {do not localize}
  end;

var
  I: Integer;
begin
  SetLength(CurrentLine, 255);

  reader := TStreamTextReader.Create(ASrcStream);
  try
    while reader.ReadLn(SourceLine) do
    begin
      CurrentPos := 1;
      for i := 1 to Length(SourceLine) do
      begin
        if not (SourceLine[i] in SafeChars) then
        begin
          if (SourceLine[i] in HalfSafeChars) then
          begin
            if i = Length(SourceLine) then
              WriteToString(CharToHex(SourceLine[i]))
            else
              WriteToString(SourceLine[i]);
          end
          else
            WriteToString(CharToHex(SourceLine[i]));
        end
        else
        begin
          if ((CurrentPos = 1) or (CurrentPos = 71)) and (SourceLine[i] = '.') then {do not localize}
            WriteToString(CharToHex(SourceLine[i]))
          else
            WriteToString(SourceLine[i]);
        end;
        if CurrentPos > 70 then
        begin
          WriteToString('=');  {Do not Localize}
          FinishLine;
        end;
      end;
      FinishLine(FAddEOL);
    end;
  finally
    reader.Free;
  end;
end;

procedure TXnEncoderQuotedPrintable.EncodeStrings(strings: TAnsiStrings);
var
  DecodedStream: TMemoryStream;
  EncodedStream: TMemoryStream;
begin
  DecodedStream := TMemoryStream.Create;
  try
    EncodedStream := TMemoryStream.Create;
    try
      strings.SaveToStream(DecodedStream);
      DecodedStream.Position := 0;
      Encode(DecodedStream, EncodedStream, DecodedStream.Size);
      EncodedStream.Position := 0;
      strings.LoadFromStream(EncodedStream);
    finally
      EncodedStream.Free;
    end;
  finally
    DecodedStream.Free;
  end;
end;

end.
