unit cmpRuler;

interface
{$IF CompilerVersion >= 24}
  {$LEGACYIFEND ON}
  {$define has_StyleElements}
  {$define HasSystemUITypes}
{$IFEND}
{$IF CompilerVersion >= 23}
   {$define UseVCLStyles}
{$IFEND}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TXNRulerOrientation = (ruHorizontal, ruVertical);
  TXNRuler = class(TCustomControl)
  private
    fSmallTickSpacing: Integer;
    fSmallTickLength: Integer;
    fSmallTicksPerLargeTick: Integer;
    fLargeTickLength: Integer;
    fDialogBox: HWND;
    procedure SetLargeTickLength(const Value: Integer);
    procedure SetOrientation(const Value: TXNRulerOrientation);
    procedure SetSmallTickLength(const Value: Integer);
    procedure SetSmallTickSpacing(const Value: Integer);
    procedure SetSmallTicksperLargeTick(const Value: Integer);
    function GetOrientation: TXNRulerOrientation;
    procedure SetDialogBox(const Value: HWND);
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create (AOwner : TComponent); override;
    property DialogBox : HWND read fDialogBox write SetDialogBox;
  published
    property Align;
    property Anchors;
    property BevelKind default bkTile;
    property BevelInner default bvLowered;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property Color;
    property Constraints;
    property ParentColor;

    property SmallTickSpacing : Integer read fSmallTickSpacing write SetSmallTickSpacing default 10;
    property SmallTicksPerLargeTick : Integer read fSmallTicksPerLargeTick write SetSmallTicksperLargeTick default 5;
    property SmallTickLength : Integer read fSmallTickLength write SetSmallTickLength default 5;
    property LargeTickLength : Integer read fLargeTickLength write SetLargeTickLength default 10;
    property Orientation : TXNRulerOrientation read GetOrientation write SetOrientation stored False;
  end;

implementation
uses
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
{$ifdef UseVCLStyles}
  Vcl.Themes,
{$endif}
  unitGUIUtils;

{ TXNRuler }

constructor TXNRuler.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Width := 180;
  Height := 40;
  BevelKind := bkTile;
  BevelInner := bvLowered;
  BevelOuter := bvLowered;
  fLargeTickLength := 10;
  fSmallTickLength := 5;
  fSmallTicksPerLargeTick := 5;
  fSmallTickSpacing := 10;
end;

function TXNRuler.GetOrientation: TXNRulerOrientation;
begin
  if Width > Height then
    result := ruHorizontal
  else
    result := ruVertical
end;

procedure TXNRuler.Loaded;
begin
  inherited;
end;

procedure TXNRuler.Paint;
var
  x, y : Integer;
  w, h : Integer;
  t : Integer;
  sm : Integer;
  r : TRect;
  offset : Integer;
begin
  Canvas.Brush.Color := ThemedColor(Color{$ifdef has_StyleElements},seClient in StyleElements{$endif});

  Canvas.Font := Font;
  Canvas.Font.Color := ThemedColor( Font.Color {$ifdef has_StyleElements},seFont in StyleElements{$endif});

  w := ClientWidth;
  h := ClientHeight;

  if fDialogBox <> 0 then
    sm := fSmallTickSpacing
  else
    sm := fSmallTickSpacing;

  y := 0;
  x := 0;
  offset := 0;
  t := 0;
  
  if Orientation = ruHorizontal then
  begin
    repeat
      Inc (offset, sm);
      if fDialogBox <> 0 then
      begin
        r := Rect (0, 0, offset, 10);
        MapDialogRect (fDialogBox, r);
        x := r.Right
      end
      else
        x := offset;
      Inc (t);
      if x < w then
      begin
        Canvas.MoveTo (x, y);
        if t = fSmallTicksPerLargeTick then
        begin
          Canvas.LineTo (x, y + fLargeTickLength);
          t := 0
        end
        else
          Canvas.LineTo (x, y + fSmallTickLength)
      end
    until x >= w
  end
  else
  begin
    repeat
      Inc (offset, sm);
      if fDialogBox <> 0 then
      begin
        r := Rect (0, 0, 10, offset);
        MapDialogRect (fDialogBox, r);
        y := r.Bottom
      end
      else
        y := offset;
        
      Inc (t);
      if y < h then
      begin
        Canvas.MoveTo (x, y);
        if t = fSmallTicksPerLargeTick then
        begin
          Canvas.LineTo (x + fLargeTickLength, y);
          t := 0
        end
        else
          Canvas.LineTo (x + fSmallTickLength, y)
      end
    until y >= h
  end
end;

procedure TXNRuler.SetDialogBox(const Value: HWND);
begin
  fDialogBox := Value;
  invalidate
end;

procedure TXNRuler.SetLargeTickLength(const Value: Integer);
begin
  if value <> fLargeTickLength then
  begin
    fLargeTickLength := Value;
    Invalidate
  end
end;

procedure TXNRuler.SetOrientation(const Value: TXNRulerOrientation);
var
  h : Integer;
begin
  if value <> Orientation then
  begin
    h := Height;
    Height := Width;
    Width := h;
    Invalidate
  end
end;

procedure TXNRuler.SetSmallTickLength(const Value: Integer);
begin
  if value <> fSmallTickLength then
  begin
    fSmallTickLength := Value;
    Invalidate
  end
end;

procedure TXNRuler.SetSmallTickSpacing(const Value: Integer);
begin
  if value <> fSmallTickSpacing then
  begin
    fSmallTickSpacing := Value;
    Invalidate
  end
end;

procedure TXNRuler.SetSmallTicksperLargeTick(const Value: Integer);
begin
  if value <> fSmallTicksPerLargeTick then
  begin
    fSmallTicksPerLargeTick := Value;
    Invalidate
  end
end;

end.
