unit PropertyPageFontForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls, ComCtrls, NewsGlobals,
  unitNewsReaderOptions, unitFontDetails;

type
  TPropertyPageFontData = class(TPropertyPageData)
  private
    fFontSize: Integer;
    fFontStyle: TFontStyles;
    fFontName: string;
    fBackgroundColor: TColor;
    fAlternateBGColor: TColor;
  protected
    procedure Initialize; override;
  public
    function Apply: Boolean; override;
    property FontName: string read fFontName;
    property FontSize: Integer read fFontSize;
    property FontStyle: TFontStyles read fFontStyle;
    property BackgroundColor: TColor read fBackgroundColor;
    property AlternateBGColor: TColor read fAlternateBGColor;
    property Param;
  end;

  TfmPropertyPageFont = class(TfmPropertyPage)
    lvFonts: TListView;
    lvSizes: TListView;
    gbFontEffects: TGroupBox;
    cbBold: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeout: TCheckBox;
    cbItalic: TCheckBox;
    Bevel2: TBevel;
    Label6: TLabel;
    rePreview: TRichEdit;
    gbFontColors: TGroupBox;
    lblBackground: TLabel;
    clrBackground: TColorBox;
    lblAlternateBG: TLabel;
    clrAlternateBG: TColorBox;
    procedure lvFontsData(Sender: TObject; Item: TListItem);
    procedure lvSizesData(Sender: TObject; Item: TListItem);
    procedure lvFontsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvSizesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure cbBoldClick(Sender: TObject);
    procedure clrBackgroundChange(Sender: TObject);
    procedure clrAlternateBGChange(Sender: TObject);
    procedure lvFontsResize(Sender: TObject);
  private
    fData: TPropertyPageFontData;
    function FontDetails(i: Integer): TFontDetails;
    procedure PopulateSizes(fontNo, fontSize: Integer);
    procedure PopulatePreview;
    procedure ApplyDataToChildForms;
    procedure wmDelayedResize(var Msg: TMessage); message WM_DELAYEDRESIZE;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls(AData: TPropertyPageData); override;
  end;

var
  fmPropertyPageFont: TfmPropertyPageFont;

implementation

uses
  PropertyBaseForm;

{$R *.dfm}

{ TfmPropertyPageFont }

function TfmPropertyPageFont.FontDetails(i: Integer): TFontDetails;
begin
  if (i >= 0) and (i < gFontDetails.Count) then
    Result := TFontDetails(gFontDetails.Objects[i])
  else
    Result := nil;
end;

class function TfmPropertyPageFont.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageFontData;
end;

procedure TfmPropertyPageFont.lvFontsChange(Sender: TObject;
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
    ApplyDataToChildForms;
  end;
end;

procedure TfmPropertyPageFont.lvFontsData(Sender: TObject; Item: TListItem);
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

procedure TfmPropertyPageFont.lvFontsResize(Sender: TObject);
begin
  PostMessage(Handle, WM_DELAYEDRESIZE, 0, 0);
end;

procedure TfmPropertyPageFont.lvSizesChange(Sender: TObject;
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
    ApplyDataToChildForms;
  end;
end;

procedure TfmPropertyPageFont.lvSizesData(Sender: TObject;
  Item: TListItem);
var
  details: TFontDetails;
begin
  details := FontDetails(lvFonts.ItemIndex);
  if Assigned(details) then
    Item.Caption := IntToStr(details.Size[Item.Index]);
end;

procedure TfmPropertyPageFont.PopulateControls(AData: TPropertyPageData);
var
  i, n: Integer;
  details: TFontDetails;
begin
  inherited;
  fData := AData as TPropertyPageFontData;
  EnumerateFonts;

  cbBold.Checked := fsBold in fData.fFontStyle;
  cbUnderline.Checked := fsUnderline in fData.fFontStyle;
  cbItalic.Checked := fsItalic in fData.fFontStyle;
  cbStrikeout.Checked := fsStrikeout in fData.fFontStyle;

  clrBackground.HandleNeeded;
  clrBackground.Selected := fData.fBackgroundColor;

  if TAppearanceEnum(fData.Param) = apMessageHeaders then
  begin
    lblAlternateBG.Visible := True;
    clrAlternateBG.Visible := True;
    clrAlternateBG.HandleNeeded;
    clrAlternateBG.Selected := fData.fAlternateBGColor;
  end
  else
  begin
    lblAlternateBG.Visible := False;
    clrAlternateBG.Visible := False;
  end;

  gbFontColors.Visible := not (TAppearanceEnum(fData.Param) in [apMainForm..apMenu]);
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

procedure TfmPropertyPageFont.PopulatePreview;
begin
  rePreview.Color := 1;
  rePreview.Color := fData.fBackgroundColor;
  rePreview.Font.Name := fData.fFontname;
  rePreview.Font.Size := fData.fFontSize;
  rePreview.Font.Style := fData.fFontStyle;
end;

procedure TfmPropertyPageFont.PopulateSizes(fontNo, fontSize: Integer);
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

procedure TfmPropertyPageFont.wmDelayedResize(var Msg: TMessage);
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

procedure TfmPropertyPageFont.ApplyDataToChildForms;
begin
  if Owner is TForm then
    SendMessage(TForm(Owner).Handle, WM_APPLYGLOBALFONTCHANGES, LPARAM(fData), 0);
end;

{ TPropertyPageFontData }

function TPropertyPageFontData.Apply: Boolean;
var
  appn: TAppearanceEnum;
  app: TAppearanceSettings;
begin
  Result := True;
  appn := TAppearanceEnum(Param);

  app := XNOptions.Appearance[appn];
  app.AlternateBGColor := fAlternateBGColor;
end;

procedure TPropertyPageFontData.Initialize;
var
  appn: TAppearanceEnum;
  app: TAppearanceSettings;
begin
  appn := TAppearanceEnum(Param);

  app := XNOptions.Appearance[appn];

  fFontSize := app.FontSize;
  fFontName := app.FontName;
  fFontStyle := app.FontStyle;
  fBackgroundColor := app.BackgroundColor;
  fAlternateBGColor := app.AlternateBGColor;
end;

procedure TfmPropertyPageFont.cbBoldClick(Sender: TObject);
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
  ApplyDataToChildForms;
end;

procedure TfmPropertyPageFont.clrAlternateBGChange(Sender: TObject);
begin
  if Populating then Exit;

  if clrAlternateBG.Visible then
    fData.fAlternateBGColor := clrAlternateBG.Selected;
end;

procedure TfmPropertyPageFont.clrBackgroundChange(Sender: TObject);
begin
  if Populating then Exit;
  fData.fBackgroundColor := clrBackground.Selected;
  PopulatePreview;
  ApplyDataToChildForms;
end;

end.
