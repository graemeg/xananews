unit PropertyPageColorFontForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls, ComCtrls, NewsGlobals,
  unitNewsReaderOptions, unitFontDetails;

type
  TPropertyPageColorFontData = class(TPropertyPageData)
  private
    fFontSize: Integer;
    fFontStyle: TFontStyles;
    fFontName: string;
    fFontColor: TColor;
    fBackgroundColor: TColor;
  protected
    function GetCaption: string; override;
    function GetHelpText: string; override;
  public
    function Apply: Boolean; override;
    procedure Initialize; override;
    procedure AssignFontData(const AName: string; ASize: Integer; AStyle: TFontStyles; ABkColor: TColor);
  end;

  TfmPropertyPageColorFont = class(TfmPropertyPage)
    Label6: TLabel;
    Bevel2: TBevel;
    rePreview: TRichEdit;
    lvFonts: TListView;
    lvSizes: TListView;
    gbFontEffects: TGroupBox;
    cbBold: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeout: TCheckBox;
    cbItalic: TCheckBox;
    gbFontColors: TGroupBox;
    lblFont: TLabel;
    clrFont: TColorBox;
    clrBackground: TColorBox;
    lblBackground: TLabel;
    procedure lvFontsData(Sender: TObject; Item: TListItem);
    procedure lvSizesData(Sender: TObject; Item: TListItem);
    procedure lvFontsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvFontsResize(Sender: TObject);
    procedure lvSizesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure cbBoldClick(Sender: TObject);
    procedure clrFontChange(Sender: TObject);
    procedure clrBackgroundChange(Sender: TObject);
  private
    fData: TPropertyPageColorFontData;
    function FontDetails(i: Integer): TFontDetails;
    procedure PopulateSizes(fontNo, fontSize: Integer);
    procedure PopulatePreview;
    procedure wmDelayedResize(var Msg: TMessage); message WM_DELAYEDRESIZE;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls(AData: TPropertyPageData); override;
  end;

var
  fmPropertyPageColorFont: TfmPropertyPageColorFont;

implementation

uses
  Contnrs;

{$R *.dfm}

var
  ColorFontPageNames: array[TAppearanceEnum] of string = (
    rstRegularMessages,
    rstMessagesToMe,
    rstMyMessages,
    rstXanaNewsMessages,
    rstDormantMessages,
    rstRepliesToMyMessages,
    rstIgnoredMessages,
    rstChildlessMessages,
    rstInterestingMessages,
    rstRegularText,
    rstHeaderText,
    rstSignatureText,
    rstLevel1QuoteText,
    rstLevel2QuoteText,
    rstLevel3QuoteText,
    rstMessageEditor,
    rstNewsgroupTree,
    rstBasicElements,
    rstToolBar,
    rstMessageDetailsPanel,
    rstMenu
    );

const
  sTilde = '~';
  sTilde1 = '~1';
  sTilde2 = '~2';
var
  ColorFontHelpText: array[TAppearanceEnum] of string = (
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde,
    sTilde1,
    sTilde1,
    sTilde2,
    sTilde2,
    sTilde2,
    sTilde2);

  { TfmPropertyPageColorFont }

function TfmPropertyPageColorFont.FontDetails(i: Integer): TFontDetails;
begin
  if (i >= 0) and (i < gFontDetails.Count) then
    Result := TFontDetails(gFontDetails.Objects[i])
  else
    Result := nil;
end;

class function TfmPropertyPageColorFont.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageColorFontData
end;

procedure TfmPropertyPageColorFont.lvFontsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  sel: TListItem;
begin
  if Populating then Exit;
  sel := lvFonts.ItemFocused;
  if Assigned(sel) and sel.Selected then
  begin
    fData.fFontName := sel.Caption;
    PopulateSizes(sel.Index, rePreview.Font.Size);
    PopulatePreview;
  end;
end;

procedure TfmPropertyPageColorFont.lvFontsData(Sender: TObject; Item: TListItem);
var
  st: string;
  details: TFontDetails;
begin
  details := FontDetails(Item.Index);
  if Assigned(details) then
  begin
    Item.Caption := details.Name;
    if details.Fixed then
      st := '*'
    else
      st := '';
    Item.SubItems.Add(st);
    if details.TrueType then
      st := '*'
    else
      st := '';
    Item.SubItems.Add(st);
  end;
end;

procedure TfmPropertyPageColorFont.lvFontsResize(Sender: TObject);
begin
  PostMessage(Handle, WM_DELAYEDRESIZE, 0, 0);
end;

procedure TfmPropertyPageColorFont.lvSizesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  sel: TListItem;
begin
  if Populating then Exit;
  sel := lvSizes.ItemFocused;
  if Assigned(sel) and (sel.Caption <> '')then
  begin
    fData.fFontSize := StrToInt(sel.Caption);
    PopulatePreview;
  end;
end;

procedure TfmPropertyPageColorFont.lvSizesData(Sender: TObject;
  Item: TListItem);
var
  details: TFontDetails;
begin
  details := FontDetails(lvFonts.ItemIndex);
  if Assigned(details) then
    Item.Caption := IntToStr(details.Size[Item.Index]);
end;

procedure TfmPropertyPageColorFont.PopulateControls(AData: TPropertyPageData);
var
  i, n: Integer;
  details: TFontDetails;
begin
  inherited;
  fData := AData as TPropertyPageColorFontData;
  EnumerateFonts;

  gbFontColors.Visible := not (TAppearanceEnum(fData.Param) in [apToolbar, apMenu]);
  lblBackground.Visible := not (TAppearanceEnum(fData.Param) in [apMainForm..apMenu]);
  clrBackground.Visible := lblBackground.Visible;

  lblBackground.Enabled := not (TAppearanceEnum(fData.Param) in [apMessagePane..apLevel3Quotes]);
  clrBackground.Enabled := lblBackground.Enabled;

  cbBold.Checked := fsBold in fData.fFontStyle;
  cbUnderline.Checked := fsUnderline in fData.fFontStyle;
  cbItalic.Checked := fsItalic in fData.fFontStyle;
  cbStrikeout.Checked := fsStrikeout in fData.fFontStyle;

  clrFont.HandleNeeded;
  clrFont.Selected := fData.fFontColor;
  clrBackground.HandleNeeded;
  clrBackground.Selected := fData.fBackgroundColor;

  lvFonts.Items.Count := gFontDetails.Count;

  for i := 0 to gFontDetails.Count - 1 do
  begin
    details := TFontDetails(gFontDetails.Objects[i]);

    if Assigned(details) then
      if SameText(details.Name, fData.fFontName) then
      begin
        lvFonts.ItemIndex := i;
        n := i + 3;
        if n >= gFontDetails.Count then
          n := gFontDetails.Count - 1;
        lvFonts.Items[n].MakeVisible(False);
        PopulateSizes(i, fData.fFontSize);
      end;
  end;

  PopulatePreview;
end;

procedure TfmPropertyPageColorFont.PopulatePreview;
begin
  rePreview.Color := 1;
  rePreview.Color := fData.fBackgroundColor;
  rePreview.Font.Name := fData.fFontname;
  rePreview.Font.Size := fData.fFontSize;
  rePreview.Font.Style := fData.fFontStyle;
  rePreview.Font.Color := fData.fFontColor;
end;

procedure TfmPropertyPageColorFont.PopulateSizes(fontNo, fontSize: Integer);
var
  details: TFontDetails;
  j, n: Integer;
  size: Integer;
begin
  if fontNo >= gFontDetails.Count then Exit;

  details := FontDetails(fontNo);

  if Assigned(details) then
  begin
    lvSizes.Items.Count := details.SizeCount;
    lvSizes.Invalidate;

    for j := 0 to details.SizeCount - 1 do
    begin
      size := details.Size[j];
      if size = fontSize then
      begin
        lvSizes.ItemIndex := j;
        n := j + 3;
        if n >= details.SizeCount then
          n := details.SizeCount - 1;
        lvSizes.Items[n].MakeVisible(False);
        Break;
      end;
    end;
  end;
end;

procedure TfmPropertyPageColorFont.wmDelayedResize(var Msg: TMessage);
var
  hasScrollBar: Boolean;
  offset: Integer;
begin
  hasScrollBar := (GetWindowLong(lvFonts.Handle, GWL_STYLE) and WS_VSCROLL) <> 0;
  if not hasScrollBar then
    offset := GetSystemMetrics(SM_CXVSCROLL)
  else
    offset := 0;

  lvFonts.Columns[0].Width := lvFonts.ClientWidth - lvFonts.Columns[1].Width - lvFonts.Columns[2].Width - 2 - offset;
end;

{ TPropertyPageColorFontData }

function TPropertyPageColorFontData.Apply: Boolean;
var
  appn: TAppearanceEnum;
  app: TAppearanceSettings;
begin
  Result := True;
  appn := TAppearanceEnum(Param);

  app := XNOptions.Appearance[appn];

  app.FontName := fFontName;
  app.FontSize := fFontSize;
  app.FontStyle := fFontStyle;
  app.FontColor := fFontColor;
  app.BackgroundColor := fBackgroundColor;
end;

procedure TPropertyPageColorFontData.AssignFontData(const AName: string;
  ASize: Integer; AStyle: TFontStyles; ABkColor: TColor);
begin
  fFontName := AName;
  fFontSize := ASize;
  fFontStyle := AStyle;
  fBackgroundColor := ABkColor;
end;

function TPropertyPageColorFontData.GetCaption: string;
var
  appn: TAppearanceEnum;
begin
  appn := TAppearanceEnum(Param);

  Result := ColorFontPageNames[appn];
end;

function TPropertyPageColorFontData.GetHelpText: string;
var
  appn: TAppearanceEnum;
begin
  appn := TAppearanceEnum(Param);

  Result := ColorFontHelpText[appn];
  if Result = sTilde then
    Result := Format(rstStandardHelp, [ColorFontPageNames[appn]])
  else
    if Result = sTilde1 then
      Result := Format(rstStandardHelp1, [ColorFontPageNames[appn]])
    else
      if Result = sTilde2 then
        Result := Format(rstStandardHelp2, [ColorFontPageNames[appn]])
end;

procedure TPropertyPageColorFontData.Initialize;
var
  appn: TAppearanceEnum;
  app: TAppearanceSettings;
begin
  appn := TAppearanceEnum(Param);

  app := XNOptions.Appearance[appn];

  fFontSize := app.FontSize;
  fFontName := app.FontName;
  fFontStyle := app.FontStyle;
  fFontColor := app.FontColor;
  fBackgroundColor := app.BackgroundColor;
end;

procedure TfmPropertyPageColorFont.cbBoldClick(Sender: TObject);
var
  cb: TCheckBox;
  style: TFontStyle;
begin
  if Populating or not (Sender is TCheckBox) then Exit;
  cb := TCheckbox(Sender);

  if Sender = cbBold then
    style := fsBold
  else
    if Sender = cbUnderline then
      style := fsUnderline
    else
      if Sender = cbStrikeout then
        style := fsStrikeout
      else
        style := fsItalic;

  if cb.Checked then
    with fData do fFontStyle := fFontStyle + [style]
  else
    with fData do fFontStyle := fFontStyle - [style];
  PopulatePreview;
end;

procedure TfmPropertyPageColorFont.clrFontChange(Sender: TObject);
begin
  fData.fFontColor := clrFont.Selected;
  PopulatePreview;
end;

procedure TfmPropertyPageColorFont.clrBackgroundChange(Sender: TObject);
begin
  fData.fBackgroundColor := clrBackground.Selected;
  PopulatePreview;
end;

end.

