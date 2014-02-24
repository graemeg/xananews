unit unitRFC2646Coder;
// !!!!!!

interface

uses Windows, Classes, SysUtils, IdCoder, XnClasses, XnRawByteStrings;

type
  TRFC2646Decoder = class(TidDecoder)
  private
    fInsertSpaceAfterQuote: boolean;
  public
    procedure Decode(ASrcStream: TStream; const ABytes: Integer = -1); override;

    // InsertSpaceAfterQuote inserts a space between the last quote character
    // and the first character of the message line - so that it displays
    // 'better'.  RFC 2646 would treat this space as 'space stuffing' so
    // it's not possible for the actual message to provide this space.
    property InsertSpaceAfterQuote: boolean read fInsertSpaceAfterQuote write fInsertSpaceAfterQuote;
  end;

  TRFC2646Encoder = class(TidEncoder)
  private
    fMaxLineLength: Integer;
  public
    procedure InitComponent; override;
    procedure Encode(ASrcStream: TStream; ADestStream: TStream; const ABytes: Integer = -1); override;
    procedure EncodeStrings(strings: TAnsiStrings);

    property MaxLineLength: Integer read fMaxLineLength write fMaxLineLength;
  end;

implementation

uses
  IdGlobal;

{ TRFC2646Decoder }

procedure TRFC2646Decoder.Decode(ASrcStream: TStream; const ABytes: Integer = -1);
var
  sl: TAnsiStringList;
  i, l: Integer;
  st, qs: RawByteString;

  procedure AnalyzeQuotes;
  var
    i, l, p: Integer;
    st: RawByteString;
  begin
    for i := 0 to sl.Count - 1 do
    begin
      st := sl[i];
      l := Length(st);
      if l = 0 then Continue;

      p := 1;
      while (st[p] = '>') and (p < l) do
        Inc(p);

      if p > 1 then
        sl[i] := Copy(st, p, MaxInt);

      sl.Objects[i] := TObject(p - 1);
    end;
  end;

begin
  qs := RawStringOfChar('>', 80);
  sl := TAnsiStringList.Create;
  try
    sl.LoadFromStream(ASrcStream);
    AnalyzeQuotes;        // Count quote markers (into sl.Objects[i]) and
                          // remove them from the text
    i := 0;
    while i < sl.Count do
    begin
      st := sl[i];
      l := Length(st);

      if (l = 0) or (st = '-- ') then
      begin
        Inc(i);
        Continue;
      end;

      if st[1] = ' ' then // Remove space stuffing.  (We already took it
                          // into consideration in AnalyzeQuotes
      begin
        Delete(st, 1, 1);
        sl[i] := st;
        Dec(l);
      end;
                          // Wrap next line into this one - only if this
                          // line ends with ' ', and it has the same
                          // quote level.

      if (l > 0) and (st[l] = ' ') and
        (i + 1 < sl.Count) and (sl.Objects[i] = sl.Objects[i + 1]) then
      begin
        if sl[i + 1] = '' then
          sl[i] := RawTrimRight(st)
        else
          sl[i] := st + sl[i + 1];
        sl.Delete(i + 1);
      end
      else
        Inc(i);
    end;

                          // Add the quote markers back in.
    for i := 0 to sl.Count - 1 do
      if Integer(sl.Objects[i]) > 0 then
        if InsertSpaceAfterQuote then
          sl[i] := Copy(qs, 1, Integer(sl.Objects[i])) + ' ' + sl[i]
        else
          sl[i] := Copy(qs, 1, Integer(sl.Objects[i])) + sl[i];
    sl.SaveToStream(FStream);
  finally
    sl.Free;
  end;
end;

{ TRFC2646Encoder }

procedure TRFC2646Encoder.EncodeStrings(strings: TAnsiStrings);
var
  i, l, p, quoteDepth: Integer;
  st, st1, qs: RawByteString;

  procedure AnalyzeQuotes;
  var
    i, l, p, quoteDepth: Integer;
    st: RawByteString;
  begin
    for i := 0 to strings.Count - 1 do
    begin
      st := strings[i];
      l := Length(st);
      p := 1;

      quoteDepth := 0;

      while (p <= l) and (st[p] = '>') do
      begin
        Inc(quoteDepth);
        Inc(p);

        if (p <= l) and (st[p] = ' ') then
          Inc(p);
      end;

      st := Copy(st, p, MaxInt);
      if st <> strings[i] then
        strings[i] := st;
      strings.Objects[i] := TObject(quoteDepth);
    end;
  end;

begin
  AnalyzeQuotes;

  qs := RawStringOfChar('>', 80);
  i := 0;
  while i < strings.Count do
  begin
    st := strings[i];
    if st = '-- ' then  // Always skip signature separator
    begin
      Inc(i);
      Continue;
    end;

                        // Always strip of trailing spaces
    st := RawTrimRight(st);
    l := Length(st);
    quoteDepth := Integer(strings.Objects[i]);

                        // If a line starts with ' ' or '>' then
                        // space-stuff it.

    if (l > 0) and (st[1] in [' ', '>']) then
      strings[i] := ' ' + st
    else
      if l > (maxLineLength - quoteDepth) then
      begin
        p := MaxLineLength - 1;
        while (p > 1) and (st[p] <> ' ') do
          Dec(p);

        if (p = 1) then
        begin
          p := MaxLineLength;
          while (p < l) and (st[p] <> ' ') do
            Inc(p);

          if p = l then
            p := 0;
        end;

        if p > 1 then
        begin
          st1 := Copy(st, p + 1, MaxInt);
          st := Copy(st, 1, p);
          strings.InsertObject(i + 1, st1, TObject(quoteDepth));
          strings[i] := st;
        end
      end
      else
        if strings[i] <> st then
          strings[i] := st;

    Inc(i);
  end;

  for i := 0 to strings.Count - 1 do
  begin
    quoteDepth := Integer(strings.Objects[i]);

    if quoteDepth > 0 then
      strings[i] := Copy(qs, 1, quoteDepth) + strings[i];
  end;
end;

procedure TRFC2646Encoder.Encode(ASrcStream: TStream; ADestStream: TStream; const ABytes: Integer = -1);
begin

end;

procedure TRFC2646Encoder.InitComponent;
begin
  inherited;

  fMaxLineLength := 76
end;

end.
