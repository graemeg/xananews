unit IdCoderQuotedPrintableX;

{9-17-2001 - J. Peter Mugaas
  made the interpretation of =20 + EOL to mean a hard line break
  soft line breaks are now ignored.  It does not make much sense
  in plain text.  Soft breaks do not indicate the end of paragraphs unlike
  hard line breaks that do end paragraphs.
 3-24-2001 - J. Peter Mugaas
  Rewrote the Decoder according to a new design.
 3-25-2001 - J. Peter Mugaas
  Rewrote the Encoder according to the new design}

interface

uses
  Classes,
  IdCoder;

type
  TIdDecoderQuotedPrintable = class(TIdDecoder)
  public
    // If you get an error here about method not found in base class,
    // you need to change your search path to use Indy9 instead of Indy 10
    procedure DecodeToStream(AIn: string; ADest: TStream); override;
  end;

  TIdEncoderQuotedPrintable = class(TIdEncoder)
  private
    fMaxLineLen : Integer;
    fAtom: boolean;
    fLinePostamble: string;
    fLinePreamble: string;
  public
    constructor Create (AOwner : TComponent); override;
    function Encode(ASrcStream: TStream; const ABytes: integer = MaxInt): string; override;
    property MaxLineLen : Integer read fMaxLineLen write fMaxLineLen default 71;
    property Atom : boolean read fAtom write fAtom;
    property LinePreamble : string read fLinePreamble write fLinePreamble;
    property LinePostamble : string read fLinePostamble write fLinePostamble;
  end;

implementation

uses
  IdGlobal,
  SysUtils;

{ TIdDecoderQuotedPrintable }

procedure TIdDecoderQuotedPrintable.DecodeToStream(AIn: string; ADest: TStream);
var
  Buffer, Line, Hex : String;
  i : Integer;
  b : Byte;
const
  Numbers = '01234567890ABCDEF';  {Do not Localize}

  procedure StripEOLChars;
  var j : Integer;
  begin
    for j := 1 to 2 do
    begin
      if (Length(Buffer) > 0) and
         (Pos(Buffer[1],EOL) > 0) then
      begin
        Delete(Buffer,1,1);
      end
      else
      begin
        break;
      end;
    end;
  end;
  function TrimRightWhiteSpace(const Str : String) : String;
  var
    i : integer;
    LSaveStr : String;
  begin
    SetLength(LSaveStr,0);
    i := Length(Str);
    while (i > 0) and (Str[i] in [#9,#32]+[#10,#13]) do
    begin
      if Str[i] in [#10,#13] then
      begin
      Insert(Str[i],LSaveStr,1);
      end;
      dec(i);
    end;
    result := Copy(Str,1,i) + LSaveStr;
  end;

begin
  Line := '';     {Do not Localize}
  { when decoding a Quoted-Printable body, any trailing
  white space on a line must be deleted, - RFC 1521}
  Buffer := TrimRightWhiteSpace(AIn);
  while Length(Buffer) > 0 do
  begin
    Line :=  Line + Fetch(Buffer,'=');  {Do not Localize}
    // process any following hexidecimal represntation
    if Length(Buffer) > 0 then
    begin
      Hex := '';     {Do not Localize}
      for i := 0 to 1 do
      begin
        If IndyPos(UpperCase(Buffer[1]),Numbers) <> 0 then
        begin
          Hex := Hex + Copy(Buffer,1,1);
          Delete(Buffer,1,1);
        end
        else
        begin
          break;
        end;
      end;
      if (Length(Hex) > 0) then
      begin
        b := StrToInt('$'+Hex);  {Do not Localize}
        //if =20 + EOL, this is a hard line break after a space
        if (b = 32) and
          (Length(Buffer) > 0) and
          (Pos(Buffer[1],EOL) > 0) then
        begin
          Line := Line + Char(b) + EOL;
          StripEOLChars;
        end
        else
        begin
          Line := Line + Char(b);
        end;
      end
      else
      begin
        //ignore soft line breaks -
        StripEOLChars;
      end;
    end;
  end;
  if Length(Line) > 0 then
  begin
    ADest.Write(Line[1],Length(Line));
  end;
end;

{ TIdEncoderQuotedPrintable }

constructor TIdEncoderQuotedPrintable.Create(AOwner: TComponent);
begin
  inherited;
  fMaxLineLen := 71
end;

function TIdEncoderQuotedPrintable.Encode(ASrcStream: TStream; const ABytes: integer): string;
//TODO: Change this to be more efficient - dont read the whole data in ahead of time as it may
// be quite large
const BUF_SIZE = 8192;
var
  i, LDataSize, LBytesRead, LBufSize : Integer;
  Buffer : Array [1..BUF_SIZE] of char;
  Line : String;
  st : TStrings;
  s : String;

    Procedure NewLine;
    begin
      Line := Line + LinePostamble + '=';  {Do not Localize}
      st.Add(Line);
      Line := LinePreamble;    {Do not Localize}
    end;

    Function QPHex(c : Char) : String;
    begin
      Result := '='+ IntToHex(Ord(c),2);  {Do not Localize}
    end;
begin
  st := TStringList.Create;
  try
    Result := '';      {Do not Localize}
    Line := LinePreamble;
    LBytesRead := 0;

    LDataSize := ASrcStream.Size - ASrcStream.Position;
    if LDataSize > ABytes then
    begin
      LDataSize := ABytes;
    end;

    if (LDataSize > 0) then
    begin
      while LBytesRead < LDataSize do
      begin
        if (LDataSize - LBytesRead) > BUF_SIZE then
        begin
          LBufSize := BUF_SIZE
        end
        else
        begin
          LBufSize := LDataSize - LBytesRead;
        end;
        ASrcStream.Read(Buffer[1],LBufSize);
        LBytesRead := LBytesRead + LBufSize;
        For i := 1 to LBufSize do
        begin
          case Buffer[i] of
          {TODO: Strange problem. Temp var needed in between. Check.}
           ' ', TAB :          {Do not Localize}
            If ((i < Length(Buffer)) and (Buffer[i+1] in [#10,#13])) then
           // If (Length(Line) > 71) then
            begin
               //Modified by Dennies Chang.
               //Line := Line + QPHex(Buffer[i]);
               s := QPHex(Buffer[i]);
               Line := Line + s;
            end
            else
              if Atom then
                if buffer [i] = ' ' then
                  Line := Line + '_'
                else
                  Line := Line + QPHex (buffer [i])
              else
                Line := Line + Buffer[i];
            '=' :   {Do not Localize}
            begin
              //Modified by Dennies Chang.
              //Line := Line + QPHex(Buffer[i]);
              s := QPHex(Buffer[i]);
              Line := Line + s;
            end
            else
            begin
              if ((Buffer[i] >= #33 ) and (Buffer[i] <= #60 )) or ((Buffer[i] >= #62) and (Buffer[i] <= #126 )) then
              begin
                if Atom and not (buffer [i] in ['a'..'z', 'A'..'Z', '0'..'9']) then
                begin
                  s := QPHex(Buffer[i]);
                  Line := Line + s
                end
                else
                  Line := Line + Buffer[i];
              end
              else
              begin
                if Buffer[i] in [#10,#13] then
                begin
                  Line := Line + Buffer[i]
                end
                else
                begin
                  Line := Line + QPHex(Buffer[i]);
                end;
              end;  //...else
            end; //..else
          end; //case buffer[i] of
          if Length(Line) > MaxLineLen then
          begin
            NewLine;
          end;  //if Length(Line > 71 then
        end; //For i := 1 to LBufSize do
      end; //while LBytesRead < LDataSize do
    end; //if (LDataSize > 0) then    {This ensures that the remaining is added to the TStrings}
    if Length(Line) >0 then
    begin
      st.Add(Line + LinePostamble);
    end;
    Result := st.Text;
    //Delete an extra system EOL that was added by the TStrings itself
    //The EOL varies from system to system
    i := Length(sLineBreak);
    if (Length(Result)>i) then
    begin
      Delete(Result,Length(Result) - i+1,i);
    end;
  finally
    FreeAndNil(st);
  end;
end;

end.
