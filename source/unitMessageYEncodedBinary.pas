(*======================================================================*
 | unitMessageYEncodedBinary unit for NewsReader3                       |
 |                                                                      |
 | yEnc attachment Decoder                                              |
 |                                                                      |
 | Provides the bare minimum at the moment.                             |
 |                                                                      |
 | 1.  No multipart support                                             |
 | 2.  No CRC or other checking.                                        |
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
 | 1.0      02/05/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitMessageYEncodedBinary;

interface

uses Windows, Classes, SysUtils, unitMessages, graphics, dialogs, Jpeg;

type
TmvYEncodedBinary = class (TmvMessagePart)
private
  fGotBegin : boolean;
  fDecodePos : Integer;
  fFileName : string;
  procedure ParseHeaderLine (const st : string);
protected
  class function IsBoundary (const st : string; MIMEHeader : TMIMEHeader) : boolean; override;
  function IsBoundaryEnd (const st : string) : boolean; override;
  function ProcessHeaderLine (const st : string) : boolean; override;
  function GetGraphic: TGraphic; override;
  function GetFileName : string; override;
  function GetDecodeType : TDecodeType; override;
public
  procedure GetData (s : TStream); override;
end;

implementation

{ TmvYEncodedBinary }

(*----------------------------------------------------------------------*
 | procedure TmvYEncodedBinary.GetData                                  |
 |                                                                      |
 | Decode the data in fData into stream 's'                             |
 |                                                                      |
 | Parameters:                                                          |
 |   s: TStream          Receives the decoded data.                     |
 *----------------------------------------------------------------------*)
procedure TmvYEncodedBinary.GetData(s: TStream);
var
  ch : char;
  b : byte;
  pb : PByte;
begin
  if not Assigned (fData) then
    Exit;

  fDecodePos := 0;
  pb := fData.Memory;           // Seeing as fData is a memory stream, use the
                                // memory directly - for speed.
  while fDecodePos < fData.Size do
  begin
    ch := Char (pb^);

    if ch = '=' then
                                // If our (incomplete) chunk of encoded data ends
                                // with '=', finish.  We'll catch it next time, when we've
                                // got more data to work with.

      if fDecodePos = fData.Size - 1 then
        break
      else
      begin                     // Next character is munged.
        Inc (fDecodePos);
        Inc (pb);
        b := pb^ - 64- 42;     // Un-munge it
        ch := Char (b);
        Inc (fDecodePos);
        Inc (pb)
      end
    else
    begin                       // This is a probably a good character.
      Inc (fDecodePos);
      Inc (pb);
      if ch in [#10, #13] then  // ... but drop CR & LF.  They'll always be munged.
        continue;
      b := Byte (ch) - 42;      // Subtract 42?  The answer to the ultimate question!
      ch := Char (b)
    end;

    s.Write (ch, 1)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TmvYEncodedBinary.GetGraphic                               |
 |                                                                      |
 | I'm not sure why this isn't in the base class.  It's the same as     |
 | the uuencoded message part stuff.                                    |
 |                                                                      |
 | Get the graphic represntation of the attachment                      |
 *----------------------------------------------------------------------*)
function TmvYEncodedBinary.GetDecodeType: TDecodeType;
begin
  result := ttYEnc;

end;

function TmvYEncodedBinary.GetFileName: string;
begin
  result := fFileName
end;

function TmvYEncodedBinary.GetGraphic: TGraphic;
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

(*----------------------------------------------------------------------*
 | class procedure TmvYEncodedBinary.IsBoundary                         |
 |                                                                      |
 | Return 'True' if the line is the start line of a yEnc attachment     |
 |                                                                      |
 | Parameters:                                                          |
 |   const st: string;          The string to check                     |
 |   fIsMIME: boolean           Maybe we should check this, and return  |
 |                              false if it's set?                      |
 *----------------------------------------------------------------------*)
class function TmvYEncodedBinary.IsBoundary(const st: string; MIMEHeader : TMIMEHeader): boolean;
begin
  result := (Length (st) > 7) and
            (CompareText (Copy (st, 1, 7), '=ybegin') = 0) and
            (st [8] = ' ');
end;

(*----------------------------------------------------------------------*
 | procedure TmvYEncodedBinary.IsBoundaryEnd                            |
 |                                                                      |
 | Return 'True' if the line is the end line of a yEnc attachment       |
 |                                                                      |
 | The spec says check the CRC, bytes & lines to ensure that the        |
 | data is valid.  But let's take a more laissez-faire attitude.  There |
 | may be *something* thats OK!                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   const st: string           The string to check                     |
 *----------------------------------------------------------------------*)
function TmvYEncodedBinary.IsBoundaryEnd(const st: string): boolean;
begin
  result := (Length (st) > 5) and
            (CompareText (Copy (st, 1, 5), '=yend') = 0) and
            (st [6] = ' ');
end;

(*----------------------------------------------------------------------*
 | procedure TmvYEncodedBinary.ParseHeaderLine                          |
 |                                                                      |
 | The line may be the =ybegin one, or a a =ypart one.  Break it apart  |
 *----------------------------------------------------------------------*)
procedure TmvYEncodedBinary.ParseHeaderLine(const st: string);
var
  s1 : string;
  vName, vValue : string;
  inPart : boolean;
  p : Integer;
begin
  inPart := False;
  if not fGotBegin then // If we haven't already got =ybegin, nothing else will do.
    if CompareText (Copy (st, 1, 7), '=ybegin') <> 0 then
      Exit
    else
      fGotBegin := True
  else
                        // If we have already had =ybegin, then this should be =ypart
    if CompareText (Copy (st, 1, 6), '=ypart') <> 0 then
      Exit
    else
      inPart := True;

  s1 := st;
  p := Pos (' ', st);
  while p > 0 do
  begin
    s1 := Trim (Copy (s1, p + 1, MaxInt));
    p := Pos ('=', s1);
    if p > 0 then
    begin
      vName := Trim (Copy (s1, 1, p - 1));
      s1 := Trim (Copy (s1, p + 1, MaxInt));

                        // 'name' is always the last thing.  And the file-name may
                        // contain a quoted-string with spaces.  So make sure we don't
                        // continue.
      if CompareText (vName, 'name') = 0 then
      begin
        fFileName := Trim (s1);
        exit
      end
      else
      begin
        p := Pos (' ', s1);
        if p > 0 then
          vValue := Trim (Copy (s1, 1, p - 1))
        else
          vValue := Trim (s1)
      end;

      if (vName <> '') and (vValue <> '') then
      begin
        if InPart then
        // Save size, lines, crc, etc.
      end
      else
        break
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TmvYEncodedBinary.ProcessHeaderLine                        |
 |                                                                      |
 | The line may be =ybegin, =ypart, or the first line of data           |
 | If it's the first line of data, stuff it into the encoded data       |
 | buffer - otherwise parse it.                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   const st: string           The line to check                       |
 |                                                                      |
 | The function returns False if it was a data line.                    |
 *----------------------------------------------------------------------*)
function TmvYEncodedBinary.ProcessHeaderLine(const st: string): boolean;
begin
  result := Copy (st, 1, 2) = '=y';
  if not result then
  begin
    fData := TMemoryStream.Create;
    AddLine (st);
    exit
  end;

  ParseHeaderLine (st)
end;

initialization
  RegisterMessagePart (TmvYEncodedBinary);
end.
