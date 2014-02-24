(*======================================================================*
 | unitMessageMime unit for NewsReader3                                 |
 |                                                                      |
 | Decode MIME messages                                                 |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitMessageMime;

interface

uses
  Windows, Classes, SysUtils, Graphics, unitMessages, XnClasses, XnRawByteStrings;

type
  TmvMimeMessagePart = class(TmvMessagePart)
  private
    fHeaderDecoded: Boolean;
    fHeader: TStringList;
    fImageClass: string;
    fMIMEHeaderStrings: TAnsiStrings;
    fForced: Boolean;
    fBody: TAnsiStrings;
    fGotBody: Boolean;
    fMIMEHeader: TMIMEHeader;
    fMultipartHeader: TMIMEHeader;
    procedure DecodeHeader;
    function GetMimeHeader: TMimeHeader;
  protected
    class function IsBoundary(const s: string; MIMEHeader: TMIMEHeader): Boolean; override;
    function IsBoundaryEnd(const st: string): Boolean; override;
    function ProcessHeaderLine(const st: RawByteString): Boolean; override;
    function GetGraphic: TGraphic; override;
    function GetBody: TAnsiStrings; override;
    function GetFileName: string; override;
    function GetDecodeType: TDecodeType; override;
    procedure InitMultipart(multipartHeader: TMIMEHeader); override;
    function MatchesCID(const cid: string): Boolean; override;
    function GetMIMEContentType: string; override;
    function GetIsHTMLMultipart: Boolean; override;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure GetData(s: TStream); override;
    property Header: TStringList read fHeader;
    property ImageClass: string read fImageClass;
    procedure InitForcedMessagePart(AMIMEHeader: TMIMEHEader);
    property MimeHeader: TMimeHeader read GetMimeHeader;
    property MultipartHeader: TMimeHeader read fMultipartHeader;
  end;

function GetMimeGraphicClass(const imageClass: string): TGraphicClass;
procedure DecodeFormatFlowed(const ins: TStream; outs: TAnsiStrings);

implementation

uses
  GIFImg, Jpeg, pngImage, NewsGlobals, idGlobal, idCoderUUE, idCoder, idCoderMIME,
  unitStreamTextReader, unitRFC2646Coder, XnCoderQuotedPrintable, XnCoderUUE;

function GetMimeGraphicClass(const imageClass: string): TGraphicClass;
begin
  // Get the graphic class needed to display a MIME content subtype       
  Result := nil;
  if CompareText(imageClass, 'jpeg') = 0 then
    Result := TJPegImage
  else
    if CompareText(imageClass, 'gif') = 0 then
      Result := TGIFImage
    else
      if CompareText(imageClass, 'bmp') = 0 then
        Result := TBitmap
      else
        if CompareText(imageClass, 'png') = 0 then
          Result := TPngImage;
end;

procedure DecodeQuotedPrintable(const ins: TStream; outs: TAnsiStrings);
var
  ch, ch1, ch2: AnsiChar;
  s: TMemoryStream;
  raw: RawByteString;
begin
  // Decode a 'quoted-printable' stream to a string list
  s := TMemoryStream.Create;
  try
    while ins.Read(ch, SizeOf(ch)) = SizeOf(ch) do
    begin
      if ch = '=' then
      begin
        ins.Read(ch1, SizeOf(ch1));
        ins.Read(ch2, SizeOf(ch2));

        // Line ending with =CRLF is a soft carriage return.  Strip it out
        if (ch1 in [#13, #10]) then
          Continue;

        // '=' means that the next two characters are hex digits for the
        // character.
        raw := AnsiChar('$') + ch1 + ch2;
        ch := AnsiChar(RawStrToIntDef(raw, 0));

        if ch = #0 then
          Continue;
      end;
      s.Write(ch, SizeOf(ch));
    end;

    s.Position := 0;
    outs.LoadFromStream(s);
  finally
    s.Free;
  end;
end;

procedure DecodeFormatFlowed(const ins: TStream; outs: TAnsiStrings);
var
  coder: TRFC2646Decoder;
  ms: TMemoryStream;
begin
  // Decode a 'format flowed' stream to a string list - See RFC 2646
  coder := nil;
  ms := TMemoryStream.Create;
  try
    coder := TRFC2646Decoder.Create(nil);
    coder.InsertSpaceAfterQuote := False;

    coder.DecodeBegin(ms);
    coder.Decode(ins, ins.Size);
    coder.DecodeEnd;

    ms.Position := 0;
    outs.LoadFromStream(ms);
  finally
    coder.Free;
    ms.Free;
  end;
end;

procedure DecodeBase64(const ins: TStream; outs: TAnsiStrings);
var
  Decoder: TidDecoder;
  Reader: TStreamTextReader;
  DecodedStream: TMemoryStream;
  EncodedStream: TMemoryStream;
begin
  DecodedStream := nil;
  EncodedStream := nil;
  Reader := nil;
  Decoder := TIdDecoderMIMELineByLine.Create(nil);
  try
    ins.Seek(0, soBeginning);
    Reader := TStreamTextReader.Create(ins);
    DecodedStream := TMemoryStream.Create;
    EncodedStream := TMemoryStream.Create;

    Decoder.DecodeBegin(DecodedStream);
    try
      while Reader.ReadLn(EncodedStream) do
        Decoder.Decode(EncodedStream);
    finally
      Decoder.DecodeEnd;
    end;

    DecodedStream.Seek(0, soBeginning);
    outs.LoadFromStream(DecodedStream);
  finally
    Decoder.Free;
    Reader.Free;
    DecodedStream.Free;
    EncodedStream.Free;
  end;
end;

{ TmvMimeMessagePart }

constructor TmvMimeMessagePart.Create(AOwner: TCollection);
begin
  inherited;
  fInLine := False;
end;

procedure TmvMimeMessagePart.DecodeHeader;
begin
  if not fHeaderDecoded and Assigned(fMIMEHeaderStrings) then
  begin
    FixHeaders(fMIMEHeaderStrings);
    fMIMEHeader := TMimeHeader.CreateFromHeaderStrings(fMIMEHeaderStrings, True)
  end;
  fHeaderDecoded := True;
end;

destructor TmvMimeMessagePart.Destroy;
begin
  fHeader.Free;
  fMIMEHeaderStrings.Free;
  fBody.Free;
  fMIMEHeader.Free;

  inherited;
end;

function TmvMimeMessagePart.GetBody: TAnsiStrings;
begin
  if not fGotBody then
  begin
    DecodeHeader;
    if fHeaderDecoded then
      if not Assigned(fMIMEHeader) or
        (SameText(fMIMEHeader.ContentType_Type, 'text') and not SameText(fMIMEHeader.ContentDisposition, 'attachment')) then
      begin
        if not Assigned(fBody) then
          fBody := TAnsiStringList.Create;

        if fData = nil then
        begin
          Result := fBody;
          Exit;
        end;

        try
          fData.Seek(0, soBeginning);
          if Assigned(fMIMEHeader) then
          begin
            if CompareText(fMIMEHeader.ContentTransferEncoding, 'Quoted-Printable') = 0 then
              DecodeQuotedPrintable(fData, fBody)
            else
              if CompareText(fMIMEHeader.ContentTransferEncoding, 'base64') = 0 then
                DecodeBase64(fData, fBody)
              else
                if Assigned(fMimeHeader.ContentType_Attributes) and (CompareText(MimeHeader.ContentType_Attributes.Values['format'], 'flowed') = 0) then
                  DecodeFormatFlowed(fData, fBody)
                else
                  fBody.LoadFromStream(fData);

            if (CompareText(fMIMEHeader.ContentType_Subtype, 'html') = 0) and (fBody.Count > 0) then
              if Pos('<HTML', UpperCase(string(fBody.Text))) = 0 then
              begin
                fBody.AnsiStrings[0] := '<HTML>' + fBody.AnsiStrings[0];
                fBody.AnsiStrings[fBody.Count - 1] := fBody.AnsiStrings[fBody.Count - 1] + '</HTML> ';
              end;
          end
          else
            fBody.LoadFromStream(fData);
        finally
          fData.Seek(0, soEnd);
        end;
      end;
    fGotBody := Complete;
  end;
  Result := fBody;
end;

procedure TmvMimeMessagePart.GetData(s: TStream);
var
  Decoder: TidDecoder;
  EncodedStream: TMemoryStream;
  Reader: TStreamTextReader;
begin
  DecodeHeader;

  if not Assigned(fData) then
    Exit;

  if fHeaderDecoded then
  begin
    fData.Seek(0, soBeginning);
    s.Size := 0;
    case DecodeType of
      ttText:
        s.CopyFrom(fData, fData.Size);

      ttQuotedPrintable:
        begin
          Decoder := TXnDecoderQuotedPrintable.Create(nil);
          try
            try
              Decoder.DecodeBegin(s);
              Decoder.Decode(fData);
              Decoder.DecodeEnd;
            except
            end;
          finally
            Decoder.Free;
          end;
        end
    else
      Decoder := nil;
      case DecodeType of
        ttUUEncode: Decoder := TXnDecoderUUE.Create(nil);
        ttBase64  : Decoder := TidDecoderMIME.Create(nil);
      end;

      EncodedStream := nil;
      Reader := nil;
      if Assigned(Decoder) then
      try
        EncodedStream := TMemoryStream.Create;
        Reader := TStreamTextReader.Create(fData);
        try
          Decoder.DecodeBegin(s);
          while Reader.ReadLn(EncodedStream) do
            Decoder.Decode(EncodedStream);
          Decoder.DecodeEnd;
        except
        end
      finally
        Decoder.Free;
        EncodedStream.Free;
        Reader.Free;
      end;
    end;
  end;
  fData.Seek(0, soEnd);
end;

function TmvMimeMessagePart.GetDecodeType: TDecodeType;
begin
  Result := ttText;
  if Assigned(fMIMEHeader) then
    Result := fMIMEHeader.DecodeType;
end;

function TmvMimeMessagePart.GetFileName: string;
begin
  Result := '';
  if Assigned(fMIMEHeader) then
    Result := fMIMEHeader.FileName;
end;

function TmvMimeMessagePart.GetGraphic: TGraphic;
var
  gc: TGraphicClass;
  ext: string;
begin
  if not fGotGraphic then
  begin
    DecodeHeader;
    if fHeaderDecoded then
    begin
      gc := nil;
      if Assigned(fMIMEHeader) and (CompareText(fMIMEHeader.ContentType_Type, 'Image') = 0) and (fMIMEHeader.ContentType_Subtype <> '') then
        if not Assigned(fGraphic) then
        begin
          gc := GetMimeGraphicClass(fMIMEHeader.ContentType_Subtype);
          if not Assigned(gc) then
          begin
            ext := ExtractFileExt(FileName);
            gc := GetGraphicClass(ext);
          end;
        end;
      DecodeGraphic(gc);
    end;
  end;
  Result := fGraphic;
end;

function TmvMimeMessagePart.GetIsHTMLMultipart: Boolean;
begin
  Result := CompareText(MIMEContentType, 'text/html') = 0;
end;

function TmvMimeMessagePart.GetMIMEContentType: string;
var
  hdr: TMimeHeader;
begin
  hdr := GetMimeHeader;
  if Assigned(hdr) then
    Result := hdr.ContentType_Type + '/' + hdr.ContentType_Subtype
  else
    Result := ''
end;

function TmvMimeMessagePart.GetMimeHeader: TMimeHeader;
begin
  DecodeHeader;
  Result := fMIMEHeader;
end;

procedure TmvMimeMessagePart.InitForcedMessagePart(AMIMEHeader: TMIMEHEader);
begin
  fMIMEHeader := TMIMEHeader.Create;
  fMIMEHeader.Assign(AMIMEHeader);
  fHeaderDecoded := True;
  fForced := True;
end;

procedure TmvMimeMessagePart.InitMultipart(multipartHeader: TMIMEHeader);
begin
  fMultipartHeader := multipartHeader;
end;

class function TmvMimeMessagePart.IsBoundary(const s: string; MIMEHeader: TMIMEHeader): Boolean;
begin
  Result := False;
  if Assigned(MIMEHeader) and (Length(s) > 2) and (s[1] = '-') and (s[2] = '-') then
    Result := CompareText(Trim(s), '--' + MIMEHeader.MultipartBoundary) = 0;
end;

function TmvMimeMessagePart.IsBoundaryEnd(const st: string): Boolean;
begin
  if fForced and (MIMEHeader.DecodeType <> ttText) and (MIMEHeader.DecodeType <> ttQuotedPrintable) then
    Result := st = ''
  else
  begin
    Result := False;
    if Assigned(fMultipartHeader) then
      Result := CompareText(Trim(st), '--' + fMultipartHeader.MultipartBoundary + '--') = 0;
  end;
end;

function TmvMimeMessagePart.MatchesCID(const cid: string): Boolean;
begin
  DecodeHeader;
  if fHeaderDecoded and Assigned(fMIMEHeader) then
    Result := CompareText(fMIMEHeader.ContentID, cid) = 0
  else
    Result := False;
end;

function TmvMimeMessagePart.ProcessHeaderLine(const st: RawByteString): Boolean;
begin
  if st = '' then
    Result := False
  else
  begin
    Result := True;
    if not Assigned(fMIMEHeaderStrings) then
      fMIMEHeaderStrings := TAnsiStringList.Create;
    fMIMEHeaderStrings.Add(st);
  end;
end;

initialization
  RegisterMessagePart(TmvMimeMessagePart);
end.
