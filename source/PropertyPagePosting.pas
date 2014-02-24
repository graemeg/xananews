unit PropertyPagePosting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageDefaultsForm, PropertyPageForm, StdCtrls, ExtCtrls,
  unitSettings;

type
  TPropertyPagePostingData = class(TPropertyPageData)
  protected
    fSettings: TPostingSettings;
    procedure Initialize; override;
  public
    destructor Destroy; override;
    function Apply: boolean; override;
  end;

  TfmPropertyPagePosting = class(TfmPropertyPageDefaults)
    cbUseOutbasket: TCheckBox;
    Label39: TLabel;
    edMaxPostLineLength: TEdit;
    Label37: TLabel;
    lblSplitPosts: TLabel;
    edMaxPostLines: TEdit;
    lblSplitPostsLines: TLabel;
    Label41: TLabel;
    cbDefaultCharset: TComboBox;
    lblDefaultISpellLanguage: TLabel;
    cbDefaultISpellLanguage: TComboBox;
    Label40: TLabel;
    rbTextPartNNTP: TRadioButton;
    rbTextPartMIME: TRadioButton;
    rbTextPartQuotedPrintable: TRadioButton;
    rbTextPartFlowed: TRadioButton;
    Label1: TLabel;
    cbArchivePostedMessages: TCheckBox;
    Panel2: TPanel;
    rbBottomPost: TRadioButton;
    rbTopPost: TRadioButton;
    procedure ControlClick(Sender: TObject);
    procedure edMaxPostLineLengthChange(Sender: TObject);
    procedure cbDefaultCharsetChange(Sender: TObject);
  private
    fData: TPropertyPagePostingData;
    fISpellInstalled: boolean;
    procedure InitializeControls(settings: TPostingSettings);
    procedure UpdateData;
  protected
    function CanRestoreParentSettings: boolean; override;
    procedure RestoreParentSettings; override;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls(AData: TPropertyPageData); override;
  end;

var
  fmPropertyPagePosting: TfmPropertyPagePosting;

implementation

{$R *.dfm}

uses unitCharsetMap, NewsGlobals, cmpSpellChecker;

{ TPropertyPagePostingData }

function TPropertyPagePostingData.Apply: boolean;
var
  settings: TPostingSettings;
begin
  Result := True;
  settings := TPostingSettings(Param);

  { nb. Can't just do settings.Assign (fSettings) because only some
    posting settings are handled by this form }

  settings.MaxPostLines := fSettings.MaxPostLines;
  settings.MaxPostLineLength := fSettings.MaxPostLineLength;
  settings.TextPartStyle := fSettings.TextPartStyle;
  settings.PostingStyle := fSettings.PostingStyle;
  settings.DefaultCodePage := fSettings.DefaultCodePage;
  settings.DefaultSpellLanguage := fSettings.DefaultSpellLanguage;
  settings.DelayPosting := fSettings.DelayPosting;
  settings.ArchivePostedMessages := fSettings.ArchivePostedMessages;
end;

destructor TPropertyPagePostingData.Destroy;
begin
  fSettings.Free;
  inherited Destroy;
end;

procedure TPropertyPagePostingData.Initialize;
var
  settings: TPostingSettings;
begin
  settings := TPostingSettings(Param);
  fSettings := TPostingSettings.Create(settings.Parent);
  fSettings.Assign(settings);
end;

{ TfmPropertyPagePosting }

class function TfmPropertyPagePosting.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPagePostingData;
end;

procedure TfmPropertyPagePosting.InitializeControls(settings: TPostingSettings);
var
  idx: Integer;
begin
  GetCharsetNames(cbDefaultCharset.Items);

  case settings.TextPartStyle of
    tpNNTP:
      rbTextPartNNTP.Checked := True;
    tpMIME:
      rbTextPartMIME.Checked := True;
    tpQuotedPrintable:
      rbTextPartQuotedPrintable.Checked := True;
    tpFlowed:
      rbTextPartFlowed.Checked := True;
  end;

  case settings.PostingStyle of
    psBottom: rbBottomPost.Checked := True;
    psTop:    rbTopPost.Checked := True;
  end;

  edMaxPostLines.Text := IntToStr(settings.MaxPostLines);
  edMaxPostLineLength.Text := IntToStr(settings.MaxPostLineLength);
  idx := cbDefaultCharset.Items.IndexOf(CodePageToCharsetName(settings.DefaultCodePage));
  if idx >= 0 then
    cbDefaultCharset.ItemIndex := idx;
  cbUseOutbasket.Checked := settings.DelayPosting;
  cbArchivePostedMessages.Checked := settings.ArchivePostedMessages;
end;

procedure TfmPropertyPagePosting.PopulateControls(AData: TPropertyPageData);
var
  i, idx: Integer;
  settings: TPostingSettings;
begin
  inherited;
  fISpellInstalled := DefaultISpellLanguage <> -1;
  fData := AData as TPropertyPagePostingData;

  settings := fData.fSettings;
  InitializeControls(settings);

  if settings.isGroup then // A few things aren't supported at group level
  begin
    cbArchivePostedMessages.Visible := False;
    edMaxPostLines.Visible := False;
    lblSplitPosts.Visible := False;
    lblSplitPostsLines.Visible := False;
  end;

  if not fISpellInstalled then
  begin
    lblDefaultISpellLanguage.Enabled := False;
    cbDefaultISpellLanguage.Enabled := False;
  end
  else
  begin
    cbDefaultISpellLanguage.Sorted := False;
    for i := 0 to TSpellChecker.LanguageCount - 1 do
      cbDefaultISpellLanguage.Items.AddObject(TSpellChecker.Language(i).name, TObject(i));
    cbDefaultISpellLanguage.Sorted := True;

    if (settings.DefaultSpellLanguage >= 0) and
       (settings.DefaultSpellLanguage < TSpellChecker.LanguageCount) then
      idx := settings.DefaultSpellLanguage
    else if TSpellChecker.LanguageCount > 0 then
      idx := DefaultISpellLanguage
    else
      idx := -1;

    if idx > -1 then
    begin
      for i := 0 to cbDefaultISpellLanguage.Items.Count - 1 do
        if Integer(cbDefaultISpellLanguage.Items.Objects[i]) = idx then
        begin
          cbDefaultISpellLanguage.ItemIndex := i;
          Break;
        end;
    end;
  end;
end;

procedure TfmPropertyPagePosting.UpdateData;
var
  settings: TPostingSettings;
begin
  if Populating then
    Exit;
  settings := fData.fSettings;
  if rbTextPartNNTP.Checked then
    settings.TextPartStyle := tpNNTP
  else if rbTextPartMIME.Checked then
    settings.TextPartStyle := tpMIME
  else if rbTextPartQuotedPrintable.Checked then
    settings.TextPartStyle := tpQuotedPrintable
  else
    settings.TextPartStyle := tpFlowed;

  if rbBottomPost.Checked then
    settings.PostingStyle := psBottom
  else
    settings.PostingStyle := psTop;

  settings.MaxPostLines := StrToIntDef(edMaxPostLines.Text, 5000);
  settings.MaxPostLineLength := StrToIntDef(edMaxPostLineLength.Text, DefaultMaxLineLength);
  settings.DefaultCodePage := CharsetNameToCodePage(cbDefaultCharset.Text);
  settings.DelayPosting := cbUseOutbasket.Checked;
  settings.ArchivePostedMessages := cbArchivePostedMessages.Checked;
  if fISpellInstalled then
    settings.DefaultSpellLanguage := Integer(cbDefaultISpellLanguage.Items.Objects[cbDefaultISpellLanguage.ItemIndex])
  else
    settings.DefaultSpellLanguage := -1;
end;

procedure TfmPropertyPagePosting.ControlClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPagePosting.edMaxPostLineLengthChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPagePosting.cbDefaultCharsetChange(Sender: TObject);
begin
  UpdateData;
end;

function TfmPropertyPagePosting.CanRestoreParentSettings: boolean;
begin
  Result := Assigned(fData.fSettings.Parent);
end;

procedure TfmPropertyPagePosting.RestoreParentSettings;
begin
  fData.fSettings.Assign(fData.fSettings.Parent);
  Populating := True;
  InitializeControls(fData.fSettings);
  Populating := False;
end;

end.
