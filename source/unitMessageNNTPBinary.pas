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

uses Windows, Classes, SysUtils, unitMessages, graphics;

type

//---------------------------------------------------------------------
// TmvNNTPBinary

TmvNNTPBinary = class (TmvMessagePart)
private
  fFileName : string;
protected
  class function IsBoundary (const st : string; MIMEHeader : TMIMEHeader) : boolean; override;
  function IsBoundaryEnd (const st : string) : boolean; override;
  function ProcessHeaderLine (const st : string) : boolean; override;
  function GetGraphic: TGraphic; override;
  function GetFileName : string; override;
  function GetDecodeType : TDecodeType; override;
public
  procedure GetData (s : TStream); override;
  destructor Destroy; override;
end;

implementation

uses NewsGlobals, idCoder, idCoderUUE, unitStreamTextReader;

{ TmvNNTPBinary }

(*----------------------------------------------------------------------*
 | TmvNNTPBinary.GetData                                                |
 |                                                                      |
 | Clear and fill the stream with decoded data.                         |
 |                                                                      |
 | nb - *must* leave fData at end of stream.                            |
 |                                                                      |
 | Parameters:                                                          |
 |   s : TStream        The stream to fill.                             |
 *----------------------------------------------------------------------*)

destructor TmvNNTPBinary.Destroy;
begin
  inherited;
end;

procedure TmvNNTPBinary.GetData(s: TStream);
var
  decoder : TidDecoder;
  str : TStreamTextReader;
  sz : Integer;
  st : string;
begin
  sz := fData.Size;
  if sz > 0 then
  begin
    str := Nil;
    decoder := TidDecoderUUE.Create(nil);
    try
      fData.Seek (0, soFromBeginning);
      str := TStreamTextReader.Create(fData);
      while str.ReadLn(st) do
        decoder.DecodeToStream(st, s);
      fData.Seek (0, soFromEnd);
    finally
      decoder.Free;
      str.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TmvNNTPBinary.GetGraphic                                             |
 |                                                                      |
 | Get the graphic                                                      |
 |                                                                      |
 | The function returns the graphic representation of the decoded data  |
 *----------------------------------------------------------------------*)
function TmvNNTPBinary.GetDecodeType: TDecodeType;
begin
  result := ttUUEncode;

end;

function TmvNNTPBinary.GetFileName: string;
begin
  result := fFileName
end;

function TmvNNTPBinary.GetGraphic: TGraphic;
var
  ext : string;
  gc : TGraphicClass;
begin
  if not fGotGraphic then
  begin
    if not Assigned (fGraphic) then
    begin
      ext := ExtractFileExt (FileName);
      gc := GetGraphicClass (ext)
    end
    else
      gc := nil;

    DecodeGraphic (gc);
  end;
  Result := fGraphic
end;

class function TmvNNTPBinary.IsBoundary(const st: string; MIMEHeader : TMIMEHeader): boolean;
begin
  result := (Length (st) > 9) and
            (CompareText (Copy (st, 1, 6), 'begin ') = 0) and
            (st [7] in ['0'..'9']) and (st [8] in ['0'..'9']) and (st [9] in ['0'..'9']);
end;

function TmvNNTPBinary.IsBoundaryEnd(const st: string): boolean;
begin
  result := CompareText (Copy (st, 1, 3), 'end') = 0;
end;

function TmvNNTPBinary.ProcessHeaderLine(const st: string) : boolean;
var
  p : Integer;
  s : string;
begin
  p := Pos (' ', st);
  if p > 0 then
  begin
    s := Copy (st, p + 1, MaxInt);
    p := Pos (' ', s);
    if p > 0 then
      s := Copy (s, p + 1, MaxInt)
    else
      s := ''
  end
  else
    s := '';

  fFileName := s;
  result := False
end;

initialization
  RegisterMessagePart (TmvNNTPBinary);
end.
