(*======================================================================*
 | unitNewsStringsDisplayObject unit for NewsReader3                    |
 |                                                                      |
 | Work with TMessageDisplay to decode and display the text part of     |
 | nntp-style messages                                                  |
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
 | 1.0      19/07/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitNewsStringsDisplayObject;

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, Forms, Types,
  cmpMessageDisplay, cmpNewsRichEdit, Dialogs, SyncObjs, ComCtrls, StrUtils, XnClasses;

type
  TNewsRichEditX = class(TNewsRichEdit)
  private
    fAutoSize: Boolean;
    fTimerScrollDelta: Integer;
    fRightMargin: Integer;
    fObjectLink: TDisplayObjectLink;
    fScrollingParent: TScrollingWinControl;
    procedure SetRightMargin(const Value: Integer);
    procedure ScrollIntoView;
  protected
    procedure SetAutoSize(Value: Boolean); override;
    procedure RequestSize(const R: TRect); override;
    procedure CreateParams(var params: TCreateParams); override;

    function FindScrollingParent: TScrollingWinControl;

    function LinesOnPage: Integer;
    function MoveCursor(lines: Integer; moveUp: Boolean; shiftState: TShiftState): Boolean;

    procedure WMLButtonDown(var msg: TwmLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var msg: TwmRButtonDown); message WM_RBUTTONDOWN;
    procedure WMKeyDown(var msg: TwmKeyDown); message WM_KEYDOWN;
    procedure WMMouseMove(var msg: TwmMouseMove); message WM_MOUSEMOVE;
    procedure WMTimer(var msg: TwmTimer); message WM_TIMER;
    procedure WMMouseWheel(var msg: TwmMouseWheel); message WM_MOUSEWHEEL;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoSize: Boolean read fAutoSize write SetAutoSize default True;
    property RightMargin: Integer read fRightMargin write SetRightMargin default 76;
  end;

  TNewsStringsDisplayObjectLink = class(TWinControlObjectLink)
  private
    fTextObjects: TList;
    fUpdating: Boolean;
    fLevel1QuotesFontColor: TColor;
    fFooterFontColor: TColor;
    fHeaderFontColor: TColor;
    fLevel2QuotesFontColor: TColor;
    fLevel3QuotesFontColor: TColor;
    fLastNoChunks: Integer;
    fLastChunkLen: Integer;
    fOwner: TMessageDisplay;
    function GetRichEdit: TNewsRichEditX;
    procedure LoadFromTextObjects;
    function GetTextObjectCount: Integer;
    function GetRightMargin: Integer;
    procedure SetRightMargin(const Value: Integer);
    procedure SetTruncateFrom(const Value: string);
    function GetTruncateFrom: string;
    procedure DoOnURLMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetStrictSigSeparator: Boolean;
    procedure SetStrictSigSeparator(const Value: Boolean);

  protected
    class function DisplaysObject(obj: TObject): Boolean; override;
    procedure GetSelectedText(var txt: string); override;
    procedure SetSelectedText(const txt: string); override;
    function GetSelLength: Integer; override;
    procedure GetText(var txt: string); override;
    procedure GetHTML(var txt: string; rawFragment: Boolean = False); override;
    procedure Refresh; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetHasText: Boolean; override;
    function FindText(const SearchStr: string; NewSearch: Boolean; Options: TStringSearchOptions; y: Integer): Boolean; override;
  public
    constructor Create(AOwner: TMessageDisplay; AObj: TObject; codepage: Integer); override;
    destructor Destroy; override;
    procedure SetTextObject(objNo: Integer; obj: TObject);
    procedure AddTextObject(obj: TObject);
    procedure Print; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property HeaderFontColor: TColor read fHeaderFontColor write fHeaderFontColor;
    property FooterFontColor: TColor read fFooterFontColor write fFooterFontColor;
    property Level1QuotesFontColor: TColor read fLevel1QuotesFontColor write fLevel1QuotesFontColor;
    property Level2QuotesFontColor: TColor read fLevel2QuotesFontColor write fLevel2QuotesFontColor;
    property Level3QuotesFontColor: TColor read fLevel3QuotesFontColor write fLevel3QuotesFontColor;

    property TextObjectCount: Integer read GetTextObjectCount;
    property RichEdit: TNewsRichEditX read GetRichEdit;

    property RightMargin: Integer read GetRightMargin write SetRightMargin;
    property TruncateFrom: string read GetTruncateFrom write SetTruncateFrom;
    property StrictSigSeparator: Boolean read GetStrictSigSeparator write SetStrictSigSeparator;
  end;

implementation

uses
  RichEdit, unitCharsetMap, unitRTF2HTML;

{ TNewsStringsDisplayObjectLink }

procedure TNewsStringsDisplayObjectLink.AddTextObject(obj: TObject);
begin
  fTextObjects.Add(obj);
  LoadFromTextObjects;
end;

procedure TNewsStringsDisplayObjectLink.BeginUpdate;
begin
  fUpdating := True;
end;

constructor TNewsStringsDisplayObjectLink.Create(AOwner: TMessageDisplay;
  AObj: TObject; codepage: Integer);
var
  ctrl: TNewsRichEditX;
  w, w1: Integer;
  tm: TTextMetric;
begin
  fOwner := AOwner;
  fTextObjects := TList.Create;
  fTextObjects.Add(AObj);
  ctrl := TNewsRichEditX.Create(AOwner.Owner);
  ctrl.Parent := AOwner;
  ctrl.CodePage := codepage;

  if AOwner.Parent is TScrollingWinControl then
    ctrl.fScrollingParent := TScrollingWinControl(AOwner.Parent);

  ctrl.fObjectLink := Self;
  inherited Create(AOwner, ctrl, codepage);
  BeginUpdate;
  ctrl.BorderStyle := bsNone;

  if Assigned(ctrl.fScrollingParent) then
    w := ctrl.fScrollingParent.Width - Margin * 2 - GetSystemMetrics(SM_CXVSCROLL)
  else
    w := AOwner.Width;

  GetTextMetrics(AOwner.Canvas.Handle, tm);
  w1 := tm.tmAveCharWidth * ctrl.RightMargin;
  if w1 > w then
    w := w1
  else
    ctrl.fRightMargin := 0;

  ctrl.AutoSize := True;
  ctrl.Width := w;
  ctrl.ReadOnly := True;
  ctrl.WordWrap := True;
  ctrl.ParentColor := True;
  ctrl.AutoURLDetect := True;
  ctrl.AutoURLExecute := True;
  ctrl.HideSelection := False;
  ctrl.Font.Assign(Font);
  ctrl.OnURLMouseDown := DoOnURLMouseDown;
  LoadFromTextObjects;
end;

destructor TNewsStringsDisplayObjectLink.Destroy;
begin
  Obj.Free;
  fTextObjects.Free;

  inherited Destroy;
end;

class function TNewsStringsDisplayObjectLink.DisplaysObject(
  obj: TObject): Boolean;
begin
  Result := (obj is TStrings) or (obj is TAnsiStrings);
end;

procedure TNewsStringsDisplayObjectLink.DoOnURLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fOwner.OnURLClick(Sender, Button, Shift, RichEdit.URLText);
end;

procedure TNewsStringsDisplayObjectLink.EndUpdate;
begin
  if fUpdating then
  begin
    fUpdating := False;
    LoadFromTextObjects;
  end;
end;

function TNewsStringsDisplayObjectLink.FindText(const SearchStr: string;
  NewSearch: Boolean; Options: TStringSearchOptions; y: Integer): Boolean;
var
  ops: TSearchTypes;
  StartPos: Integer;
begin
  RichEdit.SetFocus;
  ops := [];

  if soMatchCase in Options then
    ops := ops + [stMatchCase];

  if soWholeWord in Options then
    ops := ops + [stWholeWord];

  if newSearch then
    StartPos := 0
  else
    StartPos := RichEdit.SelStart + RichEdit.SelLength;

  StartPos := RichEdit.FindText(SearchStr, StartPos, MaxInt, ops);

  if StartPos >= 0 then
  begin
    RichEdit.SelStart := StartPos;
    RichEdit.SelLength := Length(SearchStr);
    Result := True;
  end
  else
    Result := False;
end;

function TNewsStringsDisplayObjectLink.GetHasText: Boolean;
begin
  Result := True;
end;

function TNewsStringsDisplayObjectLink.GetHeight: Integer;
begin
  try
    Result := inherited GetHeight;
  except
    Result := 0;
  end;
end;

procedure TNewsStringsDisplayObjectLink.GetHTML(var txt: string; rawFragment :Boolean = False);
var
  re: TNewsRichEditX;
  st: string;
begin
  re := GetRichEdit;
  re.StreamRTF := True;
  try
    st := GetRichEdit.Text;
    txt := RTF2HTML(st, rawFragment);
  finally
    re.StreamRTF := False;
  end;
end;

function TNewsStringsDisplayObjectLink.GetRichEdit: TNewsRichEditX;
begin
  Result := TNewsRichEditX(Obj);
end;

function TNewsStringsDisplayObjectLink.GetRightMargin: Integer;
begin
  Result := RichEdit.RightMargin;
end;

procedure TNewsStringsDisplayObjectLink.GetSelectedText(var txt: string);
begin
  Txt := GetRichEdit.SelText;
end;

function TNewsStringsDisplayObjectLink.GetSelLength: Integer;
begin
  Result := GetRichEdit.SelLength;
end;

function TNewsStringsDisplayObjectLink.GetStrictSigSeparator: Boolean;
begin
  Result := GetRichEdit.StrictSigSeparator;
end;

procedure TNewsStringsDisplayObjectLink.GetText(var txt: string);
begin
  txt := GetRichEdit.Text;
end;

function TNewsStringsDisplayObjectLink.GetTextObjectCount: Integer;
begin
  Result := fTextObjects.Count;
end;

function TNewsStringsDisplayObjectLink.GetTruncateFrom: string;
begin
  Result := GetRichEdit.TruncateFrom;
end;

function TNewsStringsDisplayObjectLink.GetWidth: Integer;
begin
  try
    Result := inherited GetWidth;
  except
    Result := 0;
  end;
end;

procedure TNewsStringsDisplayObjectLink.LoadFromTextObjects;
var
  ctrl: TNewsRichEditX;
  i: Integer;
  st, ws: string;
  cp, n, lastLen: Integer;
begin
  if fUpdating then Exit;
  ctrl := GetRichEdit;
  ctrl.RawText := Owner.RawMode;

  ws := '';
  n := 0;
  lastLen := 0;
  for i := 0 to fTextObjects.Count - 1 do
  begin
    Inc(n);
    if TObject(fTextObjects[i]) is TAnsiStrings then
    begin
      if Owner.RawMessage then
      begin
        // "ISO 8859-1" (codepage 28591), treats codeunits $00-$FF as-is, and
        // seems to be just as widely supported as codepage 1252 on most systems.
        cp := 28591;
      end
      else
      begin
        cp := CodePage;
        if cp = 1252 then
          cp := CP_ACP;
      end;

      st := AnsiStringToWideString(TAnsiStrings(fTextObjects[i]).Text, cp);
      st := StringReplace(st, 'url:', 'url: ', [rfReplaceAll, rfIgnoreCase]);
    end
    else
      st := StringReplace(TStrings(fTextObjects[i]).Text, 'url:', 'url: ', [rfReplaceAll, rfIgnoreCase]);

    lastLen := Length(st);
    ws := ws + st;
  end;

  if (n <> fLastNoChunks) or (lastLen <> fLastChunkLen) then
  begin // It's important to *only* set ctrl.Text when we have to - for
        // performances and flicker reasons.

    fLastNoChunks := n;
    fLastChunkLen := lastLen;
    ctrl.Text := ws;
  end;
end;

procedure TNewsStringsDisplayObjectLink.Print;
begin
  with GetRichEdit do
  begin
    PageRect := Self.PageRect;
    Print;
  end;
end;

procedure TNewsStringsDisplayObjectLink.Refresh;
begin
  LoadFromTextObjects;
end;

procedure TNewsStringsDisplayObjectLink.SetRightMargin(
  const Value: Integer);
begin
  RichEdit.RightMargin := Value;
end;

procedure TNewsStringsDisplayObjectLink.SetSelectedText(const txt: string);
begin
  GetRichEdit.SelText := txt;
end;

procedure TNewsStringsDisplayObjectLink.SetStrictSigSeparator(
  const Value: Boolean);
begin
  GetRichEdit.StrictSigSeparator := Value;
end;

procedure TNewsStringsDisplayObjectLink.SetTextObject(objNo: Integer;
  obj: TObject);
begin
  fTextObjects[objNo] := obj;
  LoadFromTextObjects;
end;

procedure TNewsStringsDisplayObjectLink.SetTruncateFrom(
  const Value: string);
begin
  GetRichEdit.TruncateFrom := Value;
end;

{ TNewsRichEditX }

constructor TNewsRichEditX.Create(AOwner: TComponent);
begin
  inherited;
  fAutoSize := True;
  fRightMargin := 76;
end;

procedure TNewsRichEditX.CreateParams(var params: TCreateParams);
begin
  inherited;

  params.WindowClass.style := params.WindowClass.style or CS_HREDRAW;
end;

destructor TNewsRichEditX.Destroy;
begin
  inherited Destroy;
end;

function TNewsRichEditX.FindScrollingParent: TScrollingWinControl;
var
  p: TWinControl;
begin
  p := Parent;
  while Assigned(p) and not (p is TScrollingWinControl) do
    p := p.Parent;

  Result := TScrollingWinControl(p);
end;


function TNewsRichEditX.LinesOnPage: Integer;
var
  sp: TScrollingWinControl;
  lineHeight: Integer;
begin
  Result := 0;
  sp := FindScrollingParent;
  if not Assigned(sp) then Exit;
  lineHeight := Abs(font.Height);

  Result := (sp.ClientHeight div lineHeight);
  if sp.ClientHeight mod lineHeight > 0 then
    Inc(Result);
end;

function TNewsRichEditX.MoveCursor(lines: Integer; moveUp: Boolean; shiftState: TShiftState): Boolean;
var
  line, pos, spos, l: Integer;
  sp: TScrollingWinControl;
begin
  sp := FindScrollingParent;

  if ssShift in shiftState then
    spos := SelStart + SelLength
  else
    spos := SelStart;

  line := SendMessage(handle, EM_LINEFROMCHAR, spos, 0);

  spos := spos - SendMessage(handle, EM_LINEINDEX, line, 0);

  if moveUp then
    if lines > line then lines := -line else lines := -lines
  else
    if line + lines >= lineCount then
      lines := lineCount - line - 1;

  if Assigned(sp) then
    sp.VertScrollBar.Position := sp.VertScrollBar.Position + lines * Abs(Font.Height);

  pos := SendMessage(handle, EM_LINEINDEX, line + lines, 0);
  l := SendMessage(handle, EM_LINELENGTH, line + lines, 0);

  if spos > l then
    spos := l;
  Inc(pos, spos);

  Result := True;
  if ssShift in shiftState then
    SelLength := pos - SelStart
  else
  begin
    SelStart := pos;
    Result := pos <> SelStart;
  end;
end;

procedure TNewsRichEditX.RequestSize(const R: TRect);
begin
  inherited RequestSize(R);

  if fAutoSize then
    BoundsRect := Rect(BoundsRect.Left, BoundsRect.Top, BoundsRect.Right, BoundsRect.Top + R.Bottom);
// TODO: re-check when a new Wine version arrives, last checked on Wine 1.1.36
//  Above is a fix for Wine (Linux) which returns strange numbers for the requested size.
//  orig-code was: BoundsRect := R;
end;

procedure TNewsRichEditX.ScrollIntoView;
var
  sp: TScrollingWinControl;
  pt: TPoint;
  deltaY, lineHeight: Integer;
begin
  sp := FindScrollingParent;
  if not Assigned(sp) then Exit;

  SendMessage(handle, EM_POSFROMCHAR, WPARAM(@pt), Self.SelStart + Self.SelLength);

  lineHeight := Abs(font.Height);
  if Top + pt.y + lineHeight >= sp.VertScrollBar.Position + sp.ClientHeight then
  begin
    DeltaY := (Top + pt.y + lineHeight) - (sp.VertScrollBar.Position + sp.ClientHeight);
    DeltaY := (DeltaY + lineHeight - 1) div lineHeight * lineHeight;
    sp.VertScrollBar.Position := sp.VertScrollBar.Position + DeltaY;
  end
  else
    if Top + pt.Y < sp.VertScrollBar.Position then
    begin
      DeltaY := (Top + pt.Y) - sp.VertScrollBar.Position;
      DeltaY := (DeltaY - lineHeight - 1) div lineHeight * lineHeight;
      if sp.VertScrollBar.Position + DeltaY < 0 then
        DeltaY := -sp.VertScrollBar.Position;
      sp.VertScrollBar.Position := sp.VertScrollBar.Position + DeltaY;
    end;
end;

procedure TNewsRichEditX.SetAutoSize(Value: Boolean);
begin
//  inherited;

  if value <> AutoSize then
  begin
    fAutoSize := Value;
    if Value then
      SendMessage(Handle, EM_REQUESTRESIZE, 0, 0);
  end;
end;

procedure TNewsRichEditX.SetRightMargin(const Value: Integer);
var
  tm: TTextMetric;
  canvas: TControlCanvas;
begin
  if value <> fRightMargin then
  begin
    fRightMargin := Value;

    if not (csDesigning in ComponentState) then
      if value = 0 then
      begin
        if Assigned(fScrollingParent) then
          Width := fScrollingParent.Width - fObjectLink.Margin * 2 - GetSystemMetrics(SM_CXVSCROLL)
        else
          Width := Parent.Width;
      end
      else
      begin
        canvas := TControlCanvas.Create;
        try
          canvas.Control := Self;
          canvas.Font.Assign(Font);
          GetTextMetrics(canvas.Handle, tm);
          Width := tm.tmAveCharWidth * fRightMargin;
        finally
          canvas.Free;
        end;
      end;
  end;
end;


procedure TNewsRichEditX.WMKeyDown(var msg: TwmKeyDown);
var
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(msg.KeyData);
  if (msg.CharCode = VK_NEXT) or ((msg.CharCode = VK_SPACE) and not (ssCtrl in ShiftState)) or (msg.CharCode = VK_PRIOR) then
  begin
    if (not MoveCursor(LinesOnPage - 1, msg.CharCode = VK_PRIOR, shiftState)) and (msg.CharCode = VK_SPACE) then
      inherited;
  end
  else
    inherited;

  if (msg.CharCode <> VK_SHIFT) and (msg.CharCode <> VK_CONTROL) then
    ScrollIntoView;
end;

procedure TNewsRichEditX.WMLButtonDown(var msg: TwmLButtonDown);
var
  sp: TScrollingWinControl;
  xPos, yPos: Integer;
begin
  if Assigned(fObjectLink) then
    fObjectLink.Owner.FocusObject(fObjectLink);
  if not Focused then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      xPos := sp.HorzScrollBar.Position;
      yPos := sp.VertScrollBar.Position;
      inherited;
      sp.HorzScrollBar.Position := xPos;
      sp.VertScrollBar.Position := yPos;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TNewsRichEditX.WMMouseMove(var msg: TwmMouseMove);
var
  p: TPoint;
  sp: TScrollingWinControl;
begin
  fTimerScrollDelta := 0;
  if (msg.Keys and MK_LBUTTON) <> 0 then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      p := Point(msg.xPos, msg.yPos);
      MapWindowPoints(Handle, sp.Handle, p, 1);
      if p.Y < sp.ClientRect.Top then
        fTimerScrollDelta := p.Y - sp.ClientRect.Top
      else
        if p.Y > sp.ClientRect.Bottom then
          fTimerScrollDelta := p.Y - sp.ClientRect.Bottom;
    end;
  end;
  inherited;
end;

procedure TNewsRichEditX.WMMouseWheel(var msg: TwmMouseWheel);
begin
  inherited;
end;

procedure TNewsRichEditX.WMRButtonDown(var msg: TwmRButtonDown);
var
  sp: TScrollingWinControl;
  xPos, yPos: Integer;
begin
  if Assigned(fObjectLink) then
    fObjectLink.Owner.FocusObject(fObjectLink);
  if not Focused then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      xPos := sp.HorzScrollBar.Position;
      yPos := sp.VertScrollBar.Position;
      SetFocus;
      inherited;
      sp.HorzScrollBar.Position := xPos;
      sp.VertScrollBar.Position := yPos;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TNewsRichEditX.WMTimer(var msg: TwmTimer);
var
  delta, ad: Integer;
  sp: TScrollingWinControl;
begin
  if fTimerScrollDelta <> 0 then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      delta := Self.Font.Height;
      ad := Abs(fTimerScrollDelta);
      if ad < delta then
        delta := delta div 2
      else
        if ad > 4 * delta then
          delta := delta * 2;

      if fTimerScrollDelta < 0 then
        delta := -delta;

      sp.VertScrollBar.Position := sp.VertScrollBar.Position - delta;
    end;
  end;
  inherited;
end;

initialization
  RegisterDisplayObjectLink(TNewsStringsDisplayObjectLink, 0);
end.
