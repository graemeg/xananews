(*======================================================================*
 | unitMessageNNTPBinary                                                |
 |                                                                      |
 | Decoder unit for UUEncoded NNTP binary attachments                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/08/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitMessageNNTPBinary;

interface

uses
  Windows, Classes, SysUtils, unitMessages, Graphics, XnClasses, XnRawByteStrings;

type
  TmvNNTPBinary = class(TmvMessagePart)
  private
    fFileName: string;
  protected
    class function IsBoundary(const st: string; MIMEHeader: TMIMEHeader): Boolean; override;
    function IsBoundaryEnd(const st: string): Boolean; override;
    function ProcessHeaderLine(const st: RawByteString): Boolean; override;
    function GetGraphic: TGraphic; override;
    function GetFileName: string; override;
    function GetDecodeType: TDecodeType; override;
  public
    procedure GetData(s: TStream); override;
  end;

implementation

uses
  NewsGlobals, idCoder, XnCoderUUE, unitStreamTextReader;

{ TmvNNTPBinary }

procedure TmvNNTPBinary.GetData(s: TStream);
var
  Decoder: TXnDecoderUUE;
  EncodedStream: TMemoryStream;
  Reader: TStreamTextReader;
  sz: Integer;
begin
  // Clear and fill the stream with decoded data.
  // nb - *must* leave fData at end of stream.
  sz := fData.Size;
  if sz > 0 then
  begin
    EncodedStream := nil;
    Reader := nil;
    Decoder := TXnDecoderUUE.Create(nil);
    try
      fData.Seek(0, soBeginning);
      EncodedStream := TMemoryStream.Create;
      Reader := TStreamTextReader.Create(fData);

      Decoder.DecodeBegin(s);
      while Reader.ReadLn(EncodedStream) do
        Decoder.Decode(EncodedStream);
      Decoder.DecodeEnd;

      fData.Seek(0, soEnd);
    finally
      Decoder.Free;
      EncodedStream.Free;
      Reader.Free;
    end;
  end;
end;

function TmvNNTPBinary.GetDecodeType: TDecodeType;
begin
  Result := ttUUEncode;
end;

function TmvNNTPBinary.GetFileName: string;
begin
  Result := fFileName
end;

function TmvNNTPBinary.GetGraphic: TGraphic;
var
  ext: string;
  gc: TGraphicClass;
begin
  if not fGotGraphic then
  begin
    if not Assigned(fGraphic) then
    begin
      ext := ExtractFileExt(FileName);
      gc := GetGraphicClass(ext);
    end
    else
      gc := nil;

    DecodeGraphic(gc);
  end;
  Result := fGraphic;
end;

class function TmvNNTPBinary.IsBoundary(const st: string; MIMEHeader: TMIMEHeader): Boolean;
begin
  Result := (Length(st) > 9) and
    (CompareText(Copy(st, 1, 6), 'begin ') = 0) and
    (st[7] in ['0'..'9']) and (st[8] in ['0'..'9']) and (st[9] in ['0'..'9']);
end;

function TmvNNTPBinary.IsBoundaryEnd(const st: string): Boolean;
begin
  Result := CompareText(Copy(st, 1, 3), 'end') = 0;
end;

function TmvNNTPBinary.ProcessHeaderLine(const st: RawByteString): Boolean;
var
  p: Integer;
  s: RawByteString;
begin
  p := RawPos(' ', st);
  if p > 0 then
  begin
    s := Copy(st, p + 1, MaxInt);
    p := RawPos(' ', s);
    if p > 0 then
      s := Copy(s, p + 1, MaxInt)
    else
      s := ''
  end
  else
    s := '';

  fFileName := string(s);
  Result := False;
end;

initialization
  RegisterMessagePart(TmvNNTPBinary);
end.
