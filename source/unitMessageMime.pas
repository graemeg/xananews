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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitMessageMime;

interface

uses Windows, Classes, SysUtils, graphics, unitMessages;

type

TmvMimeMessagePart = class (TmvMessagePart)
private
  fHeaderDecoded : boolean;
  fHeader : TStringList;
  fImageClass : string;
  fMIMEHeaderStrings : TStrings;
  fForced : boolean;
  fBody : TStrings;
  fGotBody : boolean;
  fMIMEHeader : TMIMEHeader;
  fMultipartHeader : TMIMEHeader;
  procedure DecodeHeader;
  function GetMimeHeader: TMimeHeader;

protected
  class function IsBoundary (const s : string; MIMEHeader : TMIMEHeader) : boolean; override;
  function IsBoundaryEnd (const st : string) : boolean; override;
  function ProcessHeaderLine (const st : string) : boolean; override;
  function GetGraphic: TGraphic; override;
  function GetBody : TStrings; override;
  function GetFileName : string; override;
  function GetDecodeType : TDecodeType; override;
  procedure InitMultipart (multipartHeader : TMIMEHeader); override;
  function MatchesCID (const cid : string) : boolean; override;
  function GetMIMEContentType : string; override;
  function GetIsHTMLMultipart: boolean; override;
public
  constructor Create (AOwner : TCollection); override;
  destructor Destroy; override;
  procedure GetData (s : TStream); override;
  property Header : TStringList read fHeader;
  property ImageClass : string read fImageClass;
  procedure InitForcedMessagePart (AMIMEHeader : TMIMEHEader);
  property MimeHeader : TMimeHeader read GetMimeHeader;
  property MultipartHeader : TMimeHeader read fMultipartHeader;
end;

function GetMimeGraphicClass (const imageClass : string) : TGraphicClass;
procedure DecodeFormatFlowed (const ins : TStream; outs : TStrings);

implementation

uses GIFImage, Jpeg, pngImage, NewsGlobals, idCoderUUE, idCoder, idCoderMIME, unitStreamTextReader, unitRFC2646Coder;


(*----------------------------------------------------------------------*
 | function GetMimeGraphicClass                                         |
 |                                                                      |
 | Get the graphic class needed to display a MIME content subtype       |
 |                                                                      |
 | Parameters:                                                          |
 |   const imageClass : string          The MIMD Content subtype        |
 |                                                                      |
 | The function returns the graphic class                               |
 *----------------------------------------------------------------------*)

function GetMimeGraphicClass (const imageClass : string) : TGraphicClass;
begin
  result := Nil;
  if CompareText (imageClass, 'jpeg') = 0 then
    result := TJPegImage
  else
    if CompareText (imageClass, 'gif') = 0 then
      result := TGIFImage
    else
      if CompareText (imageClass, 'bmp') = 0 then
        result := TBitmap
      else
        if CompareText (imageClass, 'png') = 0 then
          result := TPngObject
end;

(*----------------------------------------------------------------------*
 | procedure DecodeQuotedPrintable                                      |
 |                                                                      |
 | Decode a 'quoted-printable' stream to a string list                  |
 |                                                                      |
 | Parameters:                                                          |
 |   const ins : TStream        The stream to decode                    |
 |   outs : TStrings            The stringlist to populate              |
 *----------------------------------------------------------------------*)
procedure DecodeQuotedPrintable (const ins : TStream; outs : TStrings);
var
  ch, ch1, ch2 : char;
  st : string;
  s : TStringStream;
begin
  s := TStringStream.Create (st);
  try
    while ins.Read(ch, sizeof (ch)) = sizeof (ch) do
    begin
      if ch = '=' then
      begin
        ins.Read(ch1, sizeof (ch1));
        ins.Read(ch2, sizeof (ch2));

        // Line ending with =CRLF is a soft carriage return.  Strip it out
        if (ch1 in [#13,#10]) then
          Continue;

        // '=' means that the next two characters are hex digits for the
        // character.
        ch := Char (StrToIntDef ('$'+ch1+ch2, 0));

        if ch = #0 then
          Continue;
      end;
      s.Write(ch, sizeof (ch))
    end;

    outs.Text := s.DataString
  finally
    s.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure DecodeFormatFlowed                                         |
 |                                                                      |
 | Decode a 'format flowed' stream to a string list - See RFC 2646      |
 |                                                                      |
 | Second stab at this!                                                 |
 |                                                                      |
 | Parameters:                                                          |
 |   const ins : TStream        The stream to decode                    |
 |   outs : TStrings            The stringlist to populate              |
 *----------------------------------------------------------------------*)
procedure DecodeFormatFlowed (const ins : TStream; outs : TStrings);
var
  coder : TRFC2646Decoder;
  s : TStringStream;
  inst, st : string;
begin
  coder := Nil;
  s := TStringStream.Create(st);
  try
    coder := TRFC2646Decoder.Create(nil);
    coder.InsertSpaceAfterQuote := False;

    if ins is TMemoryStream then
      coder.DecodeBuffer(TMemoryStream (ins).Memory, ins.Size, s)
    else
    begin
      SetLength (inst, ins.Size);
      ins.Read(inst [1], ins.Size);
      coder.DecodeToStream(inst, s)
    end;

    outs.Text := s.DataString
  finally
    coder.Free;
    s.Free
  end
end;

procedure DecodeBase64 (const ins : TStream; outs : TStrings);
var
  decoder : TidDecoder;
  str : TStreamTextReader;
  s : TMemoryStream;
  st : string;
begin
  str := Nil;
  s := Nil;
  decoder := TidDecoderMIME.Create(nil);
  try
    ins.Seek (0, soFromBeginning);
    str := TStreamTextReader.Create(ins);
    s := TMemoryStream.Create;
    try
      while str.ReadLn(st) do
      begin
        if (Length (st) mod 4) <> 0 then
          st := Copy (st, 1, (Length (st) div 4) * 4);
        decoder.DecodeToStream(st, s)
      end;
    except
    end;

    s.Seek(0, soFromBeginning);
    outs.LoadFromStream(s);
  finally
    decoder.Free;
    str.Free;
    s.Free
  end
end;

{ TmvMimeMessagePart }

constructor TmvMimeMessagePart.Create(AOwner: TCollection);
begin
  inherited;
  fInLine := False
end;

procedure TmvMimeMessagePart.DecodeHeader;
begin
  if not fHeaderDecoded and Assigned (fMIMEHeaderStrings) then
  begin
    FixHeaders (fMIMEHeaderStrings);
    fMIMEHeader := TMimeHeader.CreateFromHeaderStrings(fMIMEHeaderStrings, True)
  end;
  fHeaderDecoded := True
end;

destructor TmvMimeMessagePart.Destroy;
begin
  fHeader.Free;
  fMIMEHeaderStrings.Free;
  fBody.Free;
  fMIMEHeader.Free;

  inherited;
end;

function TmvMimeMessagePart.GetBody: TStrings;
begin
  if not fGotBody then
  begin
    DecodeHeader;
    if fHeaderDecoded then
      if not Assigned (fMIMEHeader) or (CompareText (fMIMEHeader.ContentType_Type, 'text') = 0) then
      begin
        if not Assigned (fBody) then
          fBody := TStringList.Create;

        if fData = Nil then
        begin
          result := fBody;
          Exit;
        end;

        try
          fData.Seek(0, soFromBeginning);
          if Assigned (fMIMEHeader) then
          begin
            if CompareText (fMIMEHeader.ContentTransferEncoding, 'Quoted-Printable') = 0 then
              DecodeQuotedPrintable (fData, fBody)
            else
              if CompareText (fMIMEHeader.ContentTransferEncoding, 'base64') = 0 then
                DecodeBase64 (fData, fBody)
              else
                if Assigned (fMimeHeader.ContentType_Attributes) and (CompareText (MimeHeader.ContentType_Attributes.Values ['format'], 'flowed') = 0) then
                  DecodeFormatFlowed (fData, fBody)
                else
                  fBody.LoadFromStream(fData);

            if (CompareText (fMIMEHeader.ContentType_Subtype, 'html') = 0) and (fBody.Count > 0) then
              if Pos ('<HTML', UpperCase (fBody.Text)) = 0 then
              begin
                fBody.Strings [0] := '<HTML>' + fBody.Strings [0];
                fBody.Strings [fBody.Count - 1] := fBody.Strings [fBody.Count - 1] + '</HTML> '
              end
          end
          else
            fBody.LoadFromStream(fData)
        finally
          fData.Seek(0, soFromEnd)
        end
      end;
    fGotBody := Complete
  end;
  result := fBody;
end;

procedure TmvMimeMessagePart.GetData(s: TStream);
var
  decoder : TidDecoder;
  st : string;
  str : TStreamTextReader;
begin
  DecodeHeader;

  if not Assigned (fData) then
    Exit;

  if fHeaderDecoded then
  begin
    fData.Seek(0, soFromBeginning);
    s.Size := 0;
    if DecodeType <> ttText then
    begin
      decoder := Nil;
      case DecodeType of
        ttUUEncode : decoder := TidDecoderUUE.Create(nil);
        ttBase64   : decoder := TidDecoderMIME.Create(nil);
      end;

      str := Nil;
      if Assigned (decoder) then
      try
        fData.Seek (0, soFromBeginning);
        str := TStreamTextReader.Create(fData);
        try
          while str.ReadLn(st) do
            decoder.DecodeToStream(st, s);
        except
        end
      finally
        decoder.Free;
        str.Free
      end
    end
    else
      s.CopyFrom(fData, fData.Size)
  end;
  fData.Seek (0, soFromEnd);
end;

function TmvMimeMessagePart.GetDecodeType: TDecodeType;
begin
  result := ttText;
  if Assigned (fMIMEHeader) then
    result := fMIMEHeader.DecodeType
end;

function TmvMimeMessagePart.GetFileName: string;
begin
  result := '';
  if Assigned (fMIMEHeader) then
    result := fMIMEHeader.FileName;
end;

function TmvMimeMessagePart.GetGraphic: TGraphic;
var
  gc : TGraphicClass;
  ext : string;
begin
  if not fGotGraphic then
  begin
    DecodeHeader;
    if fHeaderDecoded then
    begin
      gc := Nil;
      if Assigned (fMIMEHeader) and (CompareText (fMIMEHeader.ContentType_Type, 'Image') = 0) and (fMIMEHeader.ContentType_Subtype <> '') then
      begin
        if not Assigned (fGraphic) then
        begin
          gc := GetMimeGraphicClass (fMIMEHeader.ContentType_Subtype);
          if not Assigned (gc) then
          begin
            ext := ExtractFileExt (FileName);
            gc := GetGraphicClass (ext)
          end
        end
      end;
      DecodeGraphic (gc)

    end
  end;
  result := fGraphic
end;

function TmvMimeMessagePart.GetIsHTMLMultipart: boolean;
begin
  result := CompareText (MIMEContentType, 'text/html') = 0
end;

function TmvMimeMessagePart.GetMIMEContentType: string;
var
  hdr : TMimeHeader;
begin
  hdr := GetMimeHeader;
  if Assigned (hdr) then
    result := hdr.ContentType_Type + '/' + hdr.ContentType_Subtype
  else
    result := ''
end;

function TmvMimeMessagePart.GetMimeHeader: TMimeHeader;
begin
  DecodeHeader;
  result := fMIMEHeader
end;

procedure TmvMimeMessagePart.InitForcedMessagePart(AMIMEHeader : TMIMEHEader);
begin
  fMIMEHeader := TMIMEHeader.Create;
  fMIMEHeader.Assign (AMIMEHeader);
  fHeaderDecoded := True;
  fForced := True
end;

procedure TmvMimeMessagePart.InitMultipart (multipartHeader : TMIMEHeader);
begin
  fMultipartHeader := multipartHeader;
end;

class function TmvMimeMessagePart.IsBoundary(const s: string; MIMEHeader : TMIMEHeader): boolean;
begin
  result := False;
  if Assigned (MIMEHeader) and (Length (s) > 2) and (s [1] = '-') and (s [2] = '-') then
    result := CompareText (Trim (s), '--' + MIMEHeader.MultipartBoundary) = 0
end;

function TmvMimeMessagePart.IsBoundaryEnd(const st: string): boolean;
begin
  if fForced and (MIMEHeader.DecodeType <> ttText) and (MIMEHeader.DecodeType <> ttQuotedPrintable) then
    result := st = ''
  else
  begin
    result := False;
    if Assigned (fMultipartHeader) then
      result := CompareText (Trim (st), '--' + fMultipartHeader.MultipartBoundary + '--') = 0;
  end
end;

function TmvMimeMessagePart.MatchesCID(const cid: string): boolean;
begin
  DecodeHeader;
  if fHeaderDecoded and Assigned (fMIMEHeader) then
    result := CompareText (fMIMEHeader.ContentID, cid) = 0
  else
    result := False
end;

function TmvMimeMessagePart.ProcessHeaderLine(const st: string) : boolean;
begin
  if st = '' then
    result := False
  else
  begin
    result := True;
    if not Assigned (fMIMEHeaderStrings) then
      fMIMEHeaderStrings := TStringList.Create;
    fMIMEHeaderStrings.Add(st);
  end
end;

initialization
  RegisterMessagePart (TmvMimeMessagePart);
end.
