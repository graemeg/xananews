{
  LICENSE

  Copyright (c) 2004, Henrick Wibell Hellström, StreamSec
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of StreamSec nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
}
{$I ver.inc}
unit FastHelpHTML;

interface
    
{$IFDEF MSWINDOWS}
uses
  StdCtrls, ComCtrls;
{$ENDIF}

{$IFDEF MSWINDOWS}
function HtmlToRichText(Buf: PChar; BufLen: Integer;
                        RichEdit: TRichEdit): Integer;
function RichTextToHtml(RichEdit: TRichEdit): string;
{$ENDIF}

function StrippedLength(Buf: PChar; BufLen: Integer): Integer;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows, Graphics, RichEdit,
  {$ENDIF}
  SysUtils;

function StrippedLength(Buf: PChar; BufLen: Integer): Integer;
var
  I: Integer;
begin
  I := 0;
  Result := 0;
  while I < BufLen do begin
    if Buf[I] = '<' then begin
      repeat
        Inc(I);
      until (I = BufLen) or (Buf[I] = '>');
    end else if not (Buf[I] in [#0..#32]) then
      Inc(Result);
    Inc(I);
  end;
end;
                  
{$IFDEF MSWINDOWS}
function HtmlToRichText(Buf: PChar; BufLen: Integer;
                        RichEdit: TRichEdit): Integer;
var
  L, I, J, Start: Integer;
  S, Lit, Arg: string;
  C: Char;
  WhiteSpacePrev: Boolean;
  Style: TFontStyles;
  Link: Boolean;
  FontName: string;
  FontSize: Integer;
  Pitch: TFontPitch;
  Color: TColor;

  procedure SetFormat;
  var
    Format: TCharFormat;
  begin
    RichEdit.SelAttributes.Pitch := Pitch;
    RichEdit.SelAttributes.Name := FontName;
    RichEdit.SelAttributes.Size := FontSize;
    RichEdit.SelAttributes.Color := Color;
    FillChar(Format, SizeOf(TCharFormat), 0);
    Format.cbSize := SizeOf(TCharFormat);
    with Format do begin
      dwMask := CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT or CFM_LINK;
      if fsBold in Style then dwEffects := dwEffects or CFE_BOLD;
      if fsItalic in Style then dwEffects := dwEffects or CFE_ITALIC;
      if fsUnderline in Style then dwEffects := dwEffects or CFE_UNDERLINE;
      if fsStrikeOut in Style then dwEffects := dwEffects or CFE_STRIKEOUT;
      if Link then
        dwEffects := dwEffects or CFE_LINK;
    end;
    SendMessage(RichEdit.Handle, EM_SETCHARFORMAT, SCF_SELECTION, LPARAM(@Format))
  end;

begin
  I := 0;
  L := 0;
  Result := 0;
  Pitch := RichEdit.SelAttributes.Pitch;
  FontName := RichEdit.SelAttributes.Name;
  FontSize := RichEdit.SelAttributes.Size;
  Color := RichEdit.SelAttributes.Color;
  Link := False;
  WhiteSpacePrev := True;
  Style := RichEdit.SelAttributes.Style;
  while I < BufLen do begin
    if Buf[I] = '&' then begin
      C := Buf[I];
      Buf[I] := #0;
      S := S + PChar(@Buf[L]);
      Buf[I] := C;
      L := I + 1;
      while Buf[I] <> ';' do begin
        if Buf[I] in [#0..#32] then
          Break;
        Inc(I);
      end;
      Buf[I] := #0;
      Lit := LowerCase(PChar(@Buf[L]));
      Buf[I] := ';';
      L := I + 1;
      if Lit = 'nbsp' then
        S := S + ' '
      else if Lit = 'gt' then
        S := S + '>'
      else if Lit = 'lt' then
        S := S + '<'
      else if Lit = 'amp' then
        S := S + '&';
      WhiteSpacePrev := False;
    end else if Buf[I] = '<' then begin
      C := Buf[I];
      Buf[I] := #0;
      S := S + PChar(@Buf[L]);
      Buf[I] := C;
      if S <> '' then begin
        Start := RichEdit.SelStart;
        RichEdit.SelText := S;
        RichEdit.SelStart := Start;
        RichEdit.SelLength := Length(S);
        SetFormat;
        RichEdit.SelStart := Start + Length(S);
        for J := 1 to Length(S) do
          if not (S[J] in [#0..#32]) then
            Inc(Result);
      end;
      S := '';
      L := I + 1;
      while (Buf[L] <> '>') and (L < BufLen) do
        Inc(L);
      if L = BufLen then
        raise Exception.Create('Illegal format');
      Buf[L] := #0;
      S := PChar(@Buf[I+1]);
      Buf[L] := '>';
      I := L;
      Inc(L);
      if S = 'body' then begin
        RichEdit.Clear;
        RichEdit.SelAttributes.Assign(RichEdit.DefAttributes);
      end else if S = '/body' then begin
        Exit;
      end else if (S = 'p') or (S = '/p') then begin
        RichEdit.SelText := #13#10;
        RichEdit.SelStart := Length(RichEdit.Text);
        RichEdit.Paragraph.LeftIndent := 0;
        RichEdit.Paragraph.FirstIndent := 0;
        RichEdit.SelStart := MaxInt;
      end else if (S = 'br') or (S = '/li') then begin
        RichEdit.SelText := #13#10;
        RichEdit.SelStart := Length(RichEdit.Text);
      end else if Copy(S,1,2) = 'a ' then begin
        Link := True;
//        RichEdit.SelAttributes.Color := clOlive;
      end else if S = '/a' then begin
        Link := False;
//        RichEdit.SelAttributes.Color := RichEdit.Font.Color;
      end else if Copy(S,1,1) = 'h' then begin
        RichEdit.SelText := #13#10;
        RichEdit.SelStart := Length(RichEdit.Text);
        RichEdit.Paragraph.LeftIndent := 0;
        RichEdit.Paragraph.FirstIndent := 0;
        RichEdit.SelStart := MaxInt;
        Style := Style + [fsBold];
      end else if Copy(S,1,2) = '/h' then begin
        RichEdit.SelText := #13#10;
        RichEdit.SelStart := Length(RichEdit.Text);
        RichEdit.Paragraph.LeftIndent := 0;
        RichEdit.Paragraph.FirstIndent := 0;
        RichEdit.SelStart := MaxInt;
        Style := Style - [fsBold];
      end else if S = 'b' then
        Style := Style + [fsBold]
      else if S = '/b' then
        Style := Style - [fsBold]
      else if S = 'i' then
        Style := Style + [fsItalic]
      else if S = '/i' then
        Style := Style - [fsItalic]
      else if S = 'u' then
        Style := Style + [fsUnderline]
      else if S = '/u' then
        Style := Style - [fsUnderline]
      else if S = 'ul' then begin
        RichEdit.Paragraph.LeftIndent := 0;
        RichEdit.SelText := #13#10;
        RichEdit.SelStart := Length(RichEdit.Text);
      end else if S = 'li' then begin
        RichEdit.Paragraph.LeftIndent := 20;
        RichEdit.Paragraph.FirstIndent := 0;
        RichEdit.Paragraph.Tab[0] := 0;
        RichEdit.Paragraph.Tab[1] := 20;
        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := '*'#9;
        Style := [];
      end else if S = '/ul' then begin
        RichEdit.SelText := #13#10;
      end else if Copy(S,1,5) = 'font ' then begin
        Delete(S,1,5);
        S := Trim(S);
        while S <> '' do begin
          J := Pos('=',S);
          if J = 0 then
            Break;
          Lit := LowerCase(Trim(Copy(S,1,J-1)));
          Arg := '';
          J := J + 1;
          while S[J] in [#0..#32] do
            Inc(J);
          if S[J] = '"' then begin
            Inc(J);
            while S[J] <> '"' do begin
              Arg := Arg + S[J];
              Inc(J);
            end;
          end else begin
            while not (S[J] in [#0..#32]) do begin
              Arg := Arg + S[J];
              Inc(J);
            end;
          end;
          Delete(S,1,J);
          if Lit = 'face' then begin
            FontName := Arg;
          end else if Lit = 'size' then begin
            FontSize := StrToIntDef(Arg,FontSize);
          end else if Lit = 'color' then begin
            Color := StrToInt('$' + Arg);
          end;
        end;
      end else if S = '/font' then begin
        Pitch := RichEdit.DefAttributes.Pitch;
        FontName := RichEdit.DefAttributes.Name;
        FontSize := RichEdit.DefAttributes.Size;
        Style := RichEdit.DefAttributes.Style;
        Color := RichEdit.DefAttributes.Color;
      end;
      WhiteSpacePrev := False;
      S := '';
    end else if Buf[I] in [#0..#32] then begin
      if (not WhiteSpacePrev) and ((S <> '') or (Buf[I] = #32)) then begin
        C := Buf[I];
        Buf[I] := #0;
        S := S + PChar(@Buf[L]) + ' ';
        Buf[I] := C;
        L := I + 1;
        WhiteSpacePrev := True;
      end else
        L := I + 1;
    end else
      WhiteSpacePrev := False;
    Inc(I);
  end;
  if L < BufLen then
    S := S + PChar(@Buf[L]);
  if S <> '' then begin
    for J := 1 to Length(S) do
      if not (S[J] in [#0..#32]) then
        Inc(Result);
    SetFormat;
    RichEdit.SelText := S + #13#10;
  end;
  RichEdit.SelStart := Length(RichEdit.Text);
end;

function RichTextToHtml(RichEdit: TRichEdit): string;
var
  I: Integer;
  Style, NewStyle: TFontStyles;
  C: Char;
  WhiteSpacePrev: Boolean;
  FontName: string;
  FontSize: Integer;
  Color: TColor;
  NewBullet: Boolean;
  S: string;
begin
  Result := '';
  Style := [];
  I := 1;
  NewBullet := True;
  WhiteSpacePrev := True;
  FontName := RichEdit.SelAttributes.Name;
  FontSize := RichEdit.SelAttributes.Size;
  Color := RichEdit.SelAttributes.Color;
  if (FontName <> RichEdit.DefAttributes.Name) or
     (FontSize <> RichEdit.DefAttributes.Size) or
     (Color <> RichEdit.DefAttributes.Color) then
    Result := Format('<font face="%s" size="%d" color="%x">',[FontName,FontSize,Color]);
  S := RichEdit.Text;
  while I <= Length(RichEdit.Text) do begin
    RichEdit.SelStart := I;
    if (RichEdit.SelAttributes.Name <> FontName) or
       (RichEdit.SelAttributes.Size <> FontSize) or
       (RichEdit.SelAttributes.Color <> Color) then begin
      if (FontName <> RichEdit.DefAttributes.Name) or
         (FontSize <> RichEdit.DefAttributes.Size) or
         (Color <> RichEdit.DefAttributes.Color) then
        Result := Result + '</font>';
      FontName := RichEdit.SelAttributes.Name;
      FontSize := RichEdit.SelAttributes.Size;
      Color := RichEdit.SelAttributes.Color;
      if (FontName <> RichEdit.DefAttributes.Name) or
         (FontSize <> RichEdit.DefAttributes.Size) or
         (Color <> RichEdit.DefAttributes.Color) then
        Result := Result + Format('<font face="%s" size="%d" color="%x">',[FontName,FontSize,Color]);
    end;
    NewStyle := RichEdit.SelAttributes.Style;
    if Style <> NewStyle then begin
      if (fsBold in Style) and not (fsBold in NewStyle) then
        Result := Result + '</b>'
      else if (fsBold in NewStyle) and not (fsBold in Style) then
        Result := Result + '<b>';
      if (fsItalic in Style) and not (fsItalic in NewStyle) then
        Result := Result + '</i>'
      else if (fsItalic in NewStyle) and not (fsItalic in Style) then
        Result := Result + '<i>';
      if (fsUnderline in Style) and not (fsUnderline in NewStyle) then
        Result := Result + '</u>'
      else if (fsUnderline in NewStyle) and not (fsUnderline in Style) then
        Result := Result + '<u>';
      WhiteSpacePrev := False;
    end;
    if Length(S) >= I then begin
      C := S[I];
      if (C = '*') and (Length(S) > I) and (S[I+1] = #9) then begin
        if NewBullet then
          Result := Result + '<ul><li>'#13#10
        else
          Result := Result + '</li>'#13#10'<li>';
        NewBullet := False;
        WhiteSpacePrev := False;
      end else begin
        if (C = #10) and (Length(S) > I) and NewBullet then begin
          if S[I+1] = #13 then begin
            Result := Result + '</p>'#13#10'<p>';
            Inc(I,2);
          end else if (S[I+1] = '*') and (Length(S) > I + 1) and (S[I+2] = #9) then begin
            // nothing
          end else
            Result := Result + '<br>'#13#10;
        end else if (C = #10) and (Length(S) > I) then begin
          if (S[I+1] = '*') and (Length(S) > I + 1) and (S[I+2] = #9) then begin
            // nothing
          end else begin
            Result := Result + '</li></ul>'#13#10;
            NewBullet := True;
          end;
        end;
        if C in [#0..#31] then
          WhiteSpacePrev := True
        else if (C = ' ') then begin
          if WhiteSpacePrev then
            Result := Result + '&nbsp;'
          else
            Result := Result + ' ';
          WhiteSpacePrev := True;
        end else begin
          if C = '<' then
            Result := Result + '&lt;'
          else if C = '>' then
            Result := Result + '&gt;'
          else if C = '&' then
            Result := Result + '&amp;'
          else
            Result := Result + C;
          WhiteSpacePrev := False;
        end;
      end;
    end;
    Style := NewStyle;
    Inc(I);
  end;
  if fsBold in Style then
    Result := Result + '</b>';
  if fsItalic in Style then
    Result := Result + '</i>';
  if fsUnderline in Style then
    Result := Result + '</u>';
  if not NewBullet then
    Result := Result + '</li></ul>';
  if (FontName <> RichEdit.DefAttributes.Name) or
     (FontSize <> RichEdit.DefAttributes.Size) or
     (Color <> RichEdit.DefAttributes.Color) then
    Result := Result + '</font>';
end;
{$ENDIF}

end.
