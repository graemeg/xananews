unit XnCaptionedDockTree;

interface

uses
  Windows, Controls, Graphics, Messages, SysUtils, CaptionedDockTree;

type
  TCaptionedDockTreeEx = class(TCaptionedDockTree)
{$if CompilerVersion < 22.0} // 22.0 = Delphi XE
  private
    FGrabberSize: Integer;
  protected
    function AdjustCaptionRect(const ARect: TRect): TRect; override;
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
  public
    constructor Create(DockSite: TWinControl); override;
{$else}
  public
{$ifend}
    class function GetParentFormState(const Control: TControl): TParentFormState; override;
  end;


implementation

uses
  Types, ExtCtrls, Forms, GraphUtil;

{$if CompilerVersion < 22.0} // 22.0 = Delphi XE
procedure TCaptionedDockTreeEx.AdjustDockRect(Control: TControl;
  var ARect: TRect);
begin
  if DockCaptionOrientation = dcoHorizontal then
    Inc(ARect.Top, FGrabberSize)
  else
    Inc(ARect.Left, FGrabberSize)
end;

function TCaptionedDockTreeEx.AdjustCaptionRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if DockCaptionOrientation = dcoHorizontal then
  begin
    Result.Bottom := Result.Top + FGrabberSize - 1;
    Result.Top := Result.Top + 1;
    Result.Right := Result.Right - 1; { Shrink the rect a little }
  end
  else
  begin
    Result.Right := Result.Left + FGrabberSize - 1;
    Result.Left := Result.Left + 1;
    Result.Bottom := Result.Bottom - 3;
  end;
end;

constructor TCaptionedDockTreeEx.Create(DockSite: TWinControl);
begin
  inherited;
  FGrabberSize := GetSystemMetrics(SM_CYMENUSIZE) + 2;
end;
{$ifend}

class function TCaptionedDockTreeEx.GetParentFormState(const Control: TControl): TParentFormState;
begin
  if Control is TCustomForm then
  begin
    Result.Caption := TCustomForm(Control).Caption;
    Result.Focused := (Screen.ActiveControl <> nil) and
      Screen.ActiveControl.Focused and
      (TWinControl(Control).ContainsControl(Screen.ActiveControl));
    if Control is TForm then
      Result.Icon := TForm(Control).Icon
    else
      Result.Icon := nil;
  end
  else
  begin
    Result.Caption := (Control as TPanel).Caption;
    Result.Focused := False;
    Result.Icon := nil;
  end;

  if Result.Focused then
  begin
    Result.StartColor := clActiveBorder;
    Result.EndColor := GetHighlightColor(clActiveBorder, 22);
    Result.FontColor := clCaptionText;
  end
  else
  begin
    Result.StartColor := GetShadowColor(clBtnFace, -25);
    Result.EndColor := GetHighlightColor(clBtnFace, 15);
    Result.FontColor := clBtnText;
  end;
end;

initialization
  DefaultDockTreeClass := TCaptionedDockTreeEx;
end.

