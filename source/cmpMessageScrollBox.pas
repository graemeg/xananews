(*======================================================================*
 | cmpMessageScrollBox unit for NewsReader3                             |
 |                                                                      |
 | Display a message and attachments                                    |
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
 | 1.0      20/03/2002  CPWW  Original                                  |
 *======================================================================*)

unit cmpMessageScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, StrUtils,
  ComCtrls, ExtCtrls, unitMessages, CommDlg, cmpMessageDisplay, cmpThemedScrollBox;

type

  TShowHeader = (shNone, shShort, shFull, shCustom);

  TMessageScrollBox = class(TThemedScrollBox)
  private
    fMsg: TmvMessage;
    fShowHeader: TShowHeader;
    fHeaderText: TStrings;
    fMessageDisplay: TMessageDisplay;
    fRawMode: Boolean;
    fRawMessage: Boolean;
    fParsing: Boolean;
    fImagesOnly: Boolean;
    fURLText: string;
    fFixedFont: string;
    fAutoFit: Boolean;
    fLastSize: Integer;
    procedure SetAutoFit(const Value: Boolean);
    procedure SetMsg(const Value: TmvMessage);

    procedure SetShowHeader(const Value: TShowHeader);
    procedure SetRawMode(const Value: Boolean);
    procedure SetRawMessage(const Value: Boolean);
    function GetLineHeight: Integer;
    function GetSelLength: Integer;

    procedure SetImagesOnly(const Value: Boolean);

    procedure DoOnURLClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; const url: string);
    procedure DoOnDblClick(Sender: TObject);
  protected
    procedure Resize; override;
    procedure PaintWindow(dc: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Msg: TmvMessage read fMsg write SetMsg;
    property ShowHeader: TShowHeader read fShowHeader write SetShowHeader;
    procedure ParseMessage;
    procedure Print;
    function GetAttachmentAt(x, y: Integer): TmvMessagePart;
    function GetFocusedAttachment: TmvMessagePart;
    procedure Refresh(erase, renew: Boolean);
    procedure PageDown;

    function GetSelectedText(var txt: string): Boolean;
    procedure SetSelectedText(const st: string);
    function GetText(var txt: string): Boolean;
    function GetHTML(var HTML: string; rawFragment: Boolean = False): Boolean;
    function FindText(const SearchStr: string; NewSearch: Boolean; Options: TStringSearchOptions): Boolean;
    property RawMessage: Boolean read fRawMessage write SetRawMessage;
    property ImagesOnly: Boolean read fImagesOnly write SetImagesOnly;
    property LineHeight: Integer read GetLineHeight;

    property SelLength: Integer read GetSelLength;
    property URLText: string read fURLText write fURLText;
    property FixedFont: string read fFixedFont write fFixedFont;
    property AutoFit: Boolean read fAutoFit write SetAutoFit;
  published
    property RawMode: Boolean read fRawMode write SetRawMode;
  end;

implementation

uses
  unitExSettings, unitMessageNNTPBinary, unitMessageMIME, IdGlobal, unitNNTPServices,
  unitMessageYEncodedBinary, unitHTMLStringsDisplayObject, unitMailServices,
  unitNewsStringsDisplayObject, unitNewsReaderOptions, NewsGlobals,
  unitSavedArticles, unitCharsetMap;

type
  THeaderDisplayObjectLink = class(TNewsStringsDisplayObjectLink)
  end;

{ TMessageScrollBox }

function TMessageScrollBox.GetAttachmentAt(x, y: Integer): TmvMessagePart;
var
  i, m: Integer;
  link: TDisplayObjectLink;

begin
  Result := nil;
  Inc(y, VertScrollBar.Position);
  Inc(x, HorzScrollBar.Position);

  i := 0;
  link := nil;
  while i < fMessageDisplay.ObjectCount do
  begin
    link := fMessageDisplay.Objects[i];
    if y < link.Height then
      Break;
    Dec(y, link.Height);
    Inc(i);
  end;

  if (i < fMessageDisplay.ObjectCount) and Assigned(link) then
    if (x >= link.Margin) and (x < link.Margin + link.Width) then
    begin
      m := 0;

      while (i > 0) and (m < msg.MessageParts.Count) do
      begin
        if i = 1 then
        begin
          Result := Msg.MessageParts[m];
          Break;
        end;

        Dec(i);
        Inc(m);
      end;
    end;

  if Assigned(Result) and not Result.Complete then
    Result := nil;
end;

function GetArticleHeaderText(Msg: TmvMessage; ShowHeader: TShowHeader; raw: Boolean): string;
var
  art: TArticleBase;
  headst: string;
  obj: TObject;
  i: Integer;
  sl: TStrings;
  st: string;
  dt: string;
  from: string;
  sub: string;
begin
  obj := Msg.Obj;
  if not Assigned(obj) then Exit;

  if raw then
    headst := ''
  else
    headst := #1;

  if (obj is TArticle) or (obj is TFolderArticle) then
  begin
    art := TArticleBase(obj);

    dt := art.Header['X-XanaOrigDate'];
    if dt = '' then
      dt := SafeDateTimeToInternetStr(art.Date, True);

    if raw then
    begin
      // "ISO 8859-1" (codepage 28591), treats codeunits $00-$FF as-is, and
      // seems to be just as widely supported as codepage 1252 on most systems.
      from := AnsiStringToWideString(art.RawFrom, 28591);
      sub  := AnsiStringToWideString(art.RawSubject, 28591);
    end
    else
    begin
      from := '"' + art.FromName + '" <' + art.FromEmail + '>';
      sub  := art.Subject;
    end;

    Result :=
      headst + 'From: ' + from + #13#10 +
      headst + 'Subject: ' + sub + #13#10 +
      headst + 'Date: ' + dt + #13#10 +
      headst + 'Message-ID: ' + art.MessageId + #13#10;

    if (ShowHeader in [shFull, shCustom]) and (art.References <> '') then
      Result := Result + headst + 'References: ' + art.References + #13#10;

    if art.Lines > 0 then
      Result := Result + headst + 'Lines: ' + IntToStr(art.Lines) + #13#10;
  end
  else
    Result := '';

  if not Msg.Updating then
    if ShowHeader in [shFull, shCustom] then
      for i := 0 to Msg.Header.Count - 1 do
        if Copy(msg.Header[i], 1, 14) <> 'X-XanaOrigDate' then
          if (obj is TArticleBase) and not raw then
            Result := Result + headst + AnsiStringToWideString(Msg.Header[i], TArticleBase(obj).CodePage) + #13#10
          else
            Result := Result + headst + AnsiStringToWideString(Msg.Header[i], 28591) + #13#10;


  if ShowHeader = shCustom then
  begin
    sl := TStringList.Create;
    try
      sl.NameValueSeparator := ':';
      sl.Text := Result;

      i := 0;
      while i < sl.Count do
      begin
        st := sl.Names[i];
        Delete(st, 1, Length(headst));
        if XNOptions.ShowCustomHeaders.Values[st] <> '1' then
          sl.Delete(i)
        else
          Inc(i);
      end;
      Result := sl.Text;
    finally
      sl.Free;
    end;
  end;
end;

procedure TMessageScrollBox.ParseMessage;
var
  i, c, ct: Integer;
  mp: TmvMessagePart;
  XFace: TBitmap;
  bo: TObject;
  nsd: TNewsStringsDisplayObjectLink;
  TextOnlyAttachement: Boolean;

  procedure AddOrReplaceObject(obj: TObject; XFace, header: Boolean; toa: Boolean = False);
  var
    tp: TDisplayObjectLinkClass;
    newObj: TDisplayObjectLink;

    procedure AddOrReplaceTextObj(objNo: Integer);
    var
      textObj: TNewsStringsDisplayObjectLink;
    begin
      textObj := TNewsStringsDisplayObjectLink(fMessageDisplay.Objects[objNo]);
      textObj.BeginUpdate;
      if textObj.TextObjectCount = ct then
        textObj.AddTextObject(obj)
      else
        textObj.SetTextObject(ct, obj);
      if Assigned(mp) then
        textObj.PercentDecoded := mp.PercentDecoded
      else
        textObj.PercentDecoded := 100;
      Inc(ct);
    end;

  begin
    tp := fMessageDisplay.GetObjectLinkClass(obj, XFace);

    if not header and fImagesOnly and (tp <> TGraphicDisplayObjectLink) then
      Exit;

    if (tp = TNewsStringsDisplayObjectLink) and ((TStrings(obj).Count = 0) or ((TStrings(obj).Count = 1) and (TStrings(obj)[0] = ''))) then
      Exit;

    if (c = fMessageDisplay.ObjectCount) then
    begin
      if not toa and (c > 0) and (tp = TNewsStringsDisplayObjectLink) and (fMessageDisplay.Objects[c - 1] is TNewsStringsDisplayObjectLink) then
        AddOrReplaceTextObj(c - 1)
      else
      begin
        newObj := fMessageDisplay.InsertObject(c, obj, Msg.Codepage, tp);
        newObj.MessagePart := mp;
        if Assigned(mp) then
          newObj.PercentDecoded := mp.PercentDecoded
        else
          newObj.PercentDecoded := 100;

        Inc(c);
        if tp = TNewsStringsDisplayObjectLink then
        begin
          with TNewsStringsDisplayObjectLink(newObj) do
          begin
            TruncateFrom := msg.TruncateFrom;
            StrictSigSeparator := msg.StrictSigSeparator;
            nsd := TNewsStringsDisplayObjectLink(newObj);
            XNOptions.Appearance[apHeadersInMessagePane].ApplyFontAndGetColor(nsd.RichEdit.HeaderFont, FixedFont);
            XNOptions.Appearance[apLevel1Quotes].ApplyFontAndGetColor(nsd.RichEdit.Level1QuoteFont, FixedFont);
            XNOptions.Appearance[apLevel2Quotes].ApplyFontAndGetColor(nsd.RichEdit.Level2QuoteFont, FixedFont);
            XNOptions.Appearance[apLevel3Quotes].ApplyFontAndGetColor(nsd.RichEdit.Level3QuoteFont, FixedFont);
            XNOptions.Appearance[apSignaturesInMessagePane].ApplyFontAndGetColor(nsd.RichEdit.SignatureFont, FixedFont);
            RightMargin := XNOptions.WrapLines;
          end;

          ct := 1;
        end
        else
          ct := 0;
      end;
    end
    else
      if fMessageDisplay.Objects[c] is TNewsStringsDisplayObjectLink then
      begin
        if tp = TNewsStringsDisplayObjectLink then
          AddOrReplaceTextObj(c)
        else
        begin
          Inc(c);
          AddOrReplaceObject(obj, False, False);
        end;
      end
      else
      begin
        if fMessageDisplay.Objects[c].Obj <> Obj then
          fMessageDisplay.Objects[c].Obj := Obj;
        if Assigned(mp) then
          fMessageDisplay.Objects[c].PercentDecoded := mp.PercentDecoded
        else
          fMessageDisplay.Objects[c].PercentDecoded := 100;
        Inc(c);
        ct := 0;
      end;
  end;

begin
  if not Assigned(Msg) then Exit;
  if fParsing then Exit;
  fParsing := True;

  gNoHTML := XNOptions.NoHTML;

  if FixedFont = '' then
    Font.Name := XNOptions.Appearance[apMessagePane].FontName
  else
    Font.Name := FixedFont;

  Msg.Lock;
  if Assigned(Msg.Obj) then
    fMessageDisplay.Subject := TArticleBase(Msg.Obj).Subject;

  fMessageDisplay.BeginUpdate;
  try
    fMessageDisplay.RawMode    := RawMode;
    fMessageDisplay.RawMessage := RawMessage;

    c := 0;
    ct := 0;
    mp := nil;

    if not XNOptions.NoXFaces then
    begin
      XFace := Msg.XFace;
      if Assigned(XFace) then
        AddOrReplaceObject(XFace, True, False);
    end;

    if (ShowHeader <> shNone) then
    begin
      if not Assigned(fHeaderText) then
        fHeaderText := TStringList.Create
      else
        fHeaderText.Clear;

      fHeaderText.Text := GetArticleHeaderText(Msg, ShowHeader, fMessageDisplay.RawMode);
      if fHeaderText.Count = 0 then
        fHeaderText.Add('-')
      else
        fHeaderText.Add('');
      AddOrReplaceObject(fHeaderText, False, True);
    end;

    bo := Msg.Body;
    if not Assigned(Msg.MIMEHeader) or not Msg.MIMEHeader.IsMultipart then
      if Assigned(bo) then
        AddOrReplaceObject(bo, False, False);

    i := 0;

    while i < Msg.AlternateMessagePartCount do
    begin
      mp := Msg.AlternateMessagePart[i];

      if Assigned(mp.Body) then
      begin
        TextOnlyAttachement := (mp.FileName <> '') and not SameText(mp.FileName, 'text/plain');
        AddOrReplaceObject(mp.Body, False, False, TextOnlyAttachement);
      end
      else
        if Assigned(mp.Graphic) then
          AddOrReplaceObject(mp.Graphic, False, False);

      Inc(i);
    end;
  finally
    try
      for i := 0 to fMessageDisplay.ObjectCount - 1 do
      try
        if fMessageDisplay.Objects[i] is TNewsStringsDisplayObjectLink then
          TNewsStringsDisplayObjectLink(fMessageDisplay.Objects[i]).EndUpdate;
      except
      end;
    finally
      fMessageDisplay.EndUpdate;
      fParsing := False;
      fMsg.Unlock;
    end;
  end;
end;

procedure TMessageScrollBox.Resize;
begin
  inherited;
  fMessageDisplay.Width := ClientWidth;
  fMessageDisplay.Invalidate;
end;

procedure TMessageScrollBox.SetMsg(const Value: TmvMessage);
begin
  if fParsing or fMessageDisplay.IsUpdating then
  begin
    Windows.Beep(440, 10);
    PostMessage(Application.MainForm.Handle, WM_RETRYSETMSG, 0, 0);
    Exit;       // This can't happen.  Unless fMessageDislay is parsing an HTML message -
                // where application.ProcessMessages is effectively called (yeugh!).
                //
                // eg.  You double-click a previously obtained HTML message.  The first click
                //      causes ParseMessage to be called for the existing message.  But the second
                //      click gets thru (because of the app.procmessages); calls 'SetMsg (nil)' because
                //      a new message is being downloaded - and 'Bang!'
  end;

  if Value <> fMsg then
  begin
    if Assigned(fMsg) then
      fMsg.BeingDisplayed := False;
    fMsg := Value;
    gCurrentMessage := fMsg;
    if Assigned(fMsg) then
    begin
      fMsg.RawMode := fRawMessage;
      fMsg.BeingDisplayed := True;
    end;
    VertScrollBar.Position := 0;
    HorzScrollBar.Position := 0;
    DisableAutoRange;
    try
      Refresh(False, True);
    finally
      EnableAutoRange;
    end;
  end;
end;

procedure TMessageScrollBox.Refresh(erase, renew: Boolean);
begin
  if renew then
  begin
    fMessageDisplay.Clear;
    fLastSize := -1;
  end;

  if Assigned(fMSg) then
  begin
    if fMsg.RawData.Size <> fLastSize then
    begin
      fLastSize := fMsg.RawData.Size;
      DisableAutoRange;
      try
        ParseMessage;
      finally
        EnableAutoRange;
      end;
    end;

    InvalidateRect(fMessageDisplay.Handle, nil, erase or renew);
  end;
end;

function TMessageScrollBox.GetSelectedText(var txt: string): Boolean;
begin
  Result := fMessageDisplay.GetSelectedText(txt);
end;

function TMessageScrollBox.FindText(const SearchStr: string; NewSearch: Boolean; Options: TStringSearchOptions): Boolean;
begin
  Result := fMessageDisplay.FindText(SearchStr, newSearch, Options);
end;

procedure TMessageScrollBox.SetShowHeader(const Value: TShowHeader);
begin
  if Value <> fShowHeader then
  begin
    fShowHeader := Value;
    VertScrollBar.Position := 0;
    HorzScrollBar.Position := 0;
    DisableAutoRange;
    try
      Refresh(False, True);
    finally
      EnableAutoRange;
    end;
  end;
end;

procedure TMessageScrollBox.PaintWindow(dc: HDC);
begin
end;

constructor TMessageScrollBox.Create(AOwner: TComponent);
var
  reg: TExSettings;
  TextWindowSizeK: Integer;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    fMessageDisplay := TMessageDisplay.Create(Owner);
    fMessageDisplay.AutoFit := AutoFit;

    TextWindowSizeK := gDefaultWindowSizeK;
    reg := CreateEXSettings;
    try
      reg.Section := 'Message Pane';
      if reg.Open(True) then
        TextWindowSizeK := reg.GetIntegerValue('Text Window Size K', gDefaultWindowSizeK);
    finally
      reg.Free;
    end;

    fMessageDisplay.CreateWindowSizeH := TextWindowSizeK * 1024;
    if TextWindowSizeK = 1024 * 1024 then
      fMessageDisplay.CreateWindowSizeW := fMessageDisplay.CreateWindowSizeH
    else
    begin
      fMessageDisplay.CreateWindowSizeW := 1024 * 3;
      if fMessageDisplay.CreateWindowSizeW > fMessageDisplay.CreateWindowSizeH then
        fMessageDisplay.CreateWindowSizeW := fMessageDisplay.CreateWindowSizeH;
    end;

    fMessageDisplay.Parent := Self;
    fMessageDisplay.Name := 'MessageDisplay';
    fMessageDisplay.TabStop := False;
    fMessageDisplay.PictureIndent := 10;
    fMessageDisplay.OnURLClick := DoOnURLClick;
    fMessageDisplay.OnDblClick := DoOnDblClick;
  end;
end;

destructor TMessageScrollBox.Destroy;
begin
  fHeaderText.Free;
  inherited Destroy;
end;

function TMessageScrollBox.GetFocusedAttachment: TmvMessagePart;
var
  i: Integer;
begin
  i := fMessageDisplay.FocusedObject;
  Result := nil;

  if (i >= 0) and (i < fMessageDisplay.ObjectCount) then
    Result := TmvMessagePart(fMessageDisplay.Objects[i].MessagePart);

  if Assigned(Result) and not Result.Complete then
    Result := nil;
end;

procedure TMessageScrollBox.SetRawMode(const Value: Boolean);
begin
  if Value <> fRawMode then
  begin
    fRawMode := Value;
    fMessageDisplay.RawMode := Value;
  end;
end;

procedure TMessageScrollBox.SetRawMessage(const Value: Boolean);
begin
  if Value <> fRawMessage then
  begin
    fRawMessage := Value;
    if Assigned(fMsg) then
    begin
      fMsg.RawMode := fRawMessage;
      VertScrollBar.Position := 0;
      HorzScrollBar.Position := 0;
      DisableAutoRange;
      try
        fMessageDisplay.RawMessage := fRawMessage;
        Refresh(False, True);
      finally
        EnableAutoRange;
      end;
    end;
  end;
end;

function TMessageScrollBox.GetLineHeight: Integer;
begin
  Result := fMessageDisplay.LineHeight;
end;

function TMessageScrollBox.GetText(var txt: string): Boolean;
begin
  txt := fMessageDisplay.Text;
  Result := txt <> ''
end;

function TMessageScrollBox.GetSelLength: Integer;
begin
  Result := fMessageDisplay.SelLength;
end;

procedure TMessageScrollBox.SetSelectedText(const st: string);
begin
  fMessageDisplay.SetSelectedText(st);
end;

procedure TMessageScrollBox.Print;
begin
  fMessageDisplay.Print;
end;

procedure TMessageScrollBox.SetAutoFit(const Value: Boolean);
begin
  if Value <> fAutoFit then
  begin
    fAutoFit := Value;
    fMessageDisplay.AutoFit := fAutoFit;
  end;
end;

procedure TMessageScrollBox.SetImagesOnly(const Value: Boolean);
begin
  if Value <> fImagesOnly then
  begin
    fImagesOnly := Value;
    Refresh(True, True);
  end;
end;

procedure TMessageScrollBox.PageDown;
begin
  fMessageDisplay.PageDown;
end;

procedure TMessageScrollBox.DoOnURLClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; const url: string);
begin
  if Button = mbRight then fURLText := url;
end;

function TMessageScrollBox.GetHTML(var HTML: string; rawFragment: Boolean = False): Boolean;
begin
  HTML := fMessageDisplay.GetHTML(rawFragment);
  Result := html <> '';
end;

procedure TMessageScrollBox.DoOnDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) and not (csDestroying in ComponentState) then
    OnDblClick(self);
end;

end.
