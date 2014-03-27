unit unitGUIUtils;

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
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
{$ifdef UseVCLStyles}
  Vcl.Themes,
{$endif}
  Windows;

function ThemedColor(const AColor : {$ifdef HasSystemUITypes}System.UITypes.{$endif}TColor
  {$ifdef has_StyleElements};const AUseThemes : Boolean{$endif}
  ): TColor; {$ifdef UseInline} inline; {$endif} //overload;


implementation
uses Graphics;

  {$ifdef has_StyleElements}
function ThemedColor(const AColor : TColor; const AUseThemes : Boolean): TColor; {$ifdef UseInline} inline; {$endif} overload;
begin
  if AUseThemes and TStyleManager.IsCustomStyleActive then begin
    Result := StyleServices.GetSystemColor(AColor);
  end else begin
    Result := AColor;
  end;
  Result := ColorToRGB(Result);
end;
{$else}

function ThemedColor(const AColor : TColor): TColor;
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef UseVCLStyles}
  if TStyleManager.IsCustomStyleActive then begin
    Result := StyleServices.GetSystemColor(AColor);
  end else begin
    Result := AColor;
  end;
  Result := ColorToRGB(Result);
{$else}
  if AColor < 0 then
    Result := GetSysColor(AColor and $FFFFFF)
  else
    Result := AColor and $FFFFFF;
{$endif}
end;
{$endif}

end.
