unit unitHTMLStringsDisplayObject;

interface

uses
  Windows, Classes, SysUtils, Graphics, Forms, OleCtrls, SHDocVw, ComObj,
  ActiveX, ShlObj, MSHTML, ShellAPI, cmpMessageDisplay, XNClasses;

type
  THTMLStringsDisplayObjectLink = class(TWinControlObjectLink)
  private
    fOrigObj: TAnsiStrings;
    fRendering: Boolean;
    fNavigating: Boolean;
    fXanaLink: string;
    procedure DoOnDocumentComplete(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure DoOnBeforeNavigate2(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant;
                                  var Flags: OleVariant;
                                  var TargetFrameName: OleVariant;
                                  var PostData: OleVariant;
                                  var Headers: OleVariant;
                                  var Cancel: WordBool);

    procedure DoOnNewWindow2(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
    procedure RecalcHeight;
    procedure LoadFromString(const st: string);
  protected
    class function DisplaysObject(obj: TObject): Boolean; override;
    procedure SetHeight(const Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetObj(const Value: TObject); override;
    function GetBusy: Boolean; override;
  public
    constructor Create(AOwner: TMessageDisplay; AObj: TObject; codepage: Integer); override;
    destructor Destroy; override;
    procedure Stop; override;
  end;

implementation

uses
  cmpExWebBrowser;

resourcestring
  rstCantSetHeight = 'Can''t set height of this object';

{ THTMLStringsDisplayObjectLink }

constructor THTMLStringsDisplayObjectLink.Create(AOwner: TMessageDisplay;
  AObj: TObject; codepage: Integer);
var
  ctrl: TExWebBrowser;
  xanalink: string;
  p: Integer;
begin
  fOrigObj := AObj as TAnsiStrings;
  ctrl := TEXWebBrowser.Create(AOwner.Owner);
  inherited Create(AOwner, ctrl, codepage);

  ctrl.OnDocumentComplete := DoOnDocumentComplete;
  ctrl.UIProperties.EnableContextMenu := True;
  ctrl.Width := AOwner.Parent.Width - Margin * 2 - GetSystemMetrics(SM_CXVSCROLL);
  ctrl.Height := AOwner.Parent.Height;
  if (fOrigObj.Count > 0) and (Copy(fOrigObj[0], 1, 16) = '<HTML><XanaLink>') then
  begin
    xanaLink := string(Copy(fOrigObj[0], 17, MaxInt));
    p := Pos('</XanaLink>', xanaLink);
    if p > 0 then
      xanaLink := Copy(xanaLink, 1, p - 1);
    fXanaLink := xanaLink;
    ctrl.OnNewWindow2 := DoOnNewWindow2;
    ctrl.Navigate(xanaLink);
  end
  else
  begin
    ctrl.OnBeforeNavigate2 := DoOnBeforeNavigate2;
    ctrl.OnNewWindow2 := DoOnNewWindow2;
    ctrl.Offline := True;
    LoadFromString(string(fOrigObj.Text));
  end;
  fRendering := True;
end;

destructor THTMLStringsDisplayObjectLink.Destroy;
begin
  Obj.Free;
  inherited Destroy;
end;

class function THTMLStringsDisplayObjectLink.DisplaysObject(obj: TObject): Boolean;
var
  s: TAnsiStrings;
  i: Integer;
  st, st1: string;
begin
  Result := False;
  if obj is TAnsiStrings then
  begin
    s := TAnsiStrings(obj);
    for i := 0 to s.Count - 1 do        // Is it HTML ??
    begin
      st := Trim(string(s[i]));

      if st = '' then                   // Ignore blank lines
        Continue;

      if Copy(st, 1, 2) = '<!' then     // Ignore HTML Comments (eg. <!DOCTYPE
        Continue;

      st1 := Uppercase(Copy(st, 1, 5));
      if st1 = '<HTML' then
        Result := True;

      if st1 = '<BODY' then
        Result := True;

      if st1 = '<HEAD' then
        Result := True;

      Break;
    end;
  end;
end;

procedure THTMLStringsDisplayObjectLink.DoOnBeforeNavigate2(
  Sender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName,
  PostData, Headers: OleVariant; var Cancel: WordBool);
var
  urlStr: string;
begin
  urlStr := url;

  if urlStr <> 'about:blank' then
  begin
    cancel := True;
    ShellExecute(HWND_DESKTOP, 'open', PChar(urlStr), nil, nil, SW_NORMAL);
    ctrl.Invalidate;
  end;
end;

procedure THTMLStringsDisplayObjectLink.DoOnDocumentComplete(
  Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
var
  ctrl: TExWebBrowser;
  doc: IHTMLDocument2;
  elm: IHTMLElement2;
begin
  try
    ctrl := TExWebBrowser(obj);
    if Supports(ctrl.Document, IHTMLDocument2, doc) then
    begin
      try
        elm := doc.ActiveElement as IHTMLElement2; //body as IHTMLElement2;

// TODO: re-check when a new Wine version arrives, last checked on Wine 1.1.36
//  ActiveElement does not appear to be implemented in Wine (Linux).
      except
        on E: EOleException do
          if E.ErrorCode = E_NOTIMPL then
            elm := doc.Body as IHTMLElement2;
      end;

      if Assigned(elm) then
      begin
        ctrl.Width := elm.scrollWidth;
        ctrl.Height := elm.scrollHeight;
        Owner.RecalcBounds;
      end;
    end;
  finally
    fRendering := False;
  end;
end;

procedure THTMLStringsDisplayObjectLink.DoOnNewWindow2(Sender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
var
  ctrl: TExWebBrowser;
begin
  ctrl := TExWebBrowser(obj);
  cancel := not ctrl.UIProperties.OpenLinksInNewWindow;
end;

function THTMLStringsDisplayObjectLink.GetBusy: Boolean;
var
  b: TExWebBrowser;
begin
  try
    if Assigned(obj) then
    begin
      b := TExWebBrowser(obj);
      Result := b.Busy;

      if Result then
        if b.ReadyState = READYSTATE_INTERACTIVE then
          Result := False;
    end
    else
      Result := False;
  except
    Result := True;
  end;
end;

procedure THTMLStringsDisplayObjectLink.LoadFromString(const st: string);
begin
  if fNavigating then Exit;

  fNavigating := True;
  try
    TExWebBrowser(obj).LoadFromString(st);
  finally
    fNavigating := False;
  end;
end;

procedure THTMLStringsDisplayObjectLink.RecalcHeight;
var
  ctrl: TExWebBrowser;
  doc: IHTMLDocument2;
  elm: IHTMLElement2;
begin
  ctrl := TExWebBrowser(obj);
  if Supports(ctrl.Document, IHTMLDocument2, doc) then
  begin
    elm := doc.body as IHTMLElement2;

    if Assigned(elm) then
    begin
      ctrl.Height := elm.scrollHeight;
      Owner.RecalcBounds;
    end;
  end;
end;

procedure THTMLStringsDisplayObjectLink.SetHeight(const Value: Integer);
begin
end;

procedure THTMLStringsDisplayObjectLink.SetObj(const Value: TObject);
begin
  fOrigObj := Value as TAnsiStrings;
  Stop;
  if fRendering or fNavigating or Busy then
  begin
    Windows.Beep(440, 10);
    Exit
  end;
  ctrl.Width := Owner.MessageWidth;
  ctrl.Height := Owner.ClientHeight;
  fRendering := True;
  LoadFromString(string(fOrigObj.Text));
end;

procedure THTMLStringsDisplayObjectLink.SetWidth(const Value: Integer);
begin
  inherited SetWidth(Value);
  RecalcHeight;
end;

procedure THTMLStringsDisplayObjectLink.Stop;
var
  n: Integer;
  b: TExWebBrowser;
begin
  if Busy then
    if Assigned(obj) then
    begin
      b := TExWebBrowser(obj);
      try
        n := 0;
        while b.ReadyState < READYSTATE_COMPLETE do
        begin
          Sleep(100);
          if b.ReadyState = READYSTATE_INTERACTIVE then
            Inc(n);
          if n > 2 then
            Break;
        end;
        if b.ReadyState = READYSTATE_INTERACTIVE then
          b.Stop;
      except
      end;
      Sleep(200);
    end;
end;

initialization
  RegisterDisplayObjectLink(THTMLStringsDisplayObjectLink)
end.
