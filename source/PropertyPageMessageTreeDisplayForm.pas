unit PropertyPageMessageTreeDisplayForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMessageTreeDisplayDetails = class (TPropertyPageData)
  private
    fUnreadFontStyle : TFontStyles;
    fFirstLineAsSubject : boolean;
    fHideReadMessages : boolean;
    fHideIgnoredMessages : boolean;
    fHideFolderIcons : boolean;
    fDontHighlightXanaNewsUsers : boolean;
    fTreeColumn : Integer;
    fHideAuthor : boolean;
    fHideFlags : boolean;
    fHideNumber : boolean;
    fHideDate : boolean;
    fHideLines : boolean;
    fHideSubject : boolean;
    fHighlightSelectedText : boolean;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageMessageTreeDisplay = class(TfmPropertyPage)
    Label3: TLabel;
    cbUnreadMessagesBold: TCheckBox;
    cbUnreadMessagesItalic: TCheckBox;
    cbUnreadMessagesUnderline: TCheckBox;
    cbFirstLineAsSubject: TCheckBox;
    cbHideReadMessages: TCheckBox;
    cbHideFolderIcons: TCheckBox;
    cbDontHighlightXanaNewsUsers: TCheckBox;
    Label2: TLabel;
    cbTreeColumn: TComboBox;
    cbHideAuthor: TCheckBox;
    cbHideFlags: TCheckBox;
    cbHideNumber: TCheckBox;
    cbHideDate: TCheckBox;
    cbHideLines: TCheckBox;
    cbHideSubject: TCheckBox;
    Label1: TLabel;
    cbHighlightText: TCheckBox;
    cbHideIgnoredMessages: TCheckBox;
    procedure ControlClick(Sender: TObject);
    procedure cbTreeColumnChange(Sender: TObject);
  private
    fData : TPropertyPageMessageTreeDisplayDetails;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageMessageTreeDisplay: TfmPropertyPageMessageTreeDisplay;

implementation

uses unitNNTPServices, unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageMessageTreeDisplay }

class function TfmPropertyPageMessageTreeDisplay.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMessageTreeDisplayDetails;
end;

procedure TfmPropertyPageMessageTreeDisplay.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;

  fData := AData as TPropertyPageMessageTreeDisplayDetails;
  cbUnreadMessagesBold.Checked      := fsBold      in fData.fUnreadFontStyle;
  cbUnreadMessagesItalic.Checked    := fsItalic    in fData.fUnreadFontStyle;
  cbUnreadMessagesUnderline.Checked := fsUnderline in fData.fUnreadFontStyle;

  cbFirstLineAsSubject.Checked         := fData.fFirstLineAsSubject;
  cbHideReadMessages.Checked           := fData.fHideReadMessages;
  cbHideIgnoredMessages.Checked        := fData.fHideIgnoredMessages;
  cbHideFolderIcons.Checked            := fData.fHideFolderIcons;
  cbDontHighlightXanaNewsUsers.Checked := fData.fDontHighlightXanaNewsUsers;
  cbTreeColumn.ItemIndex               := fData.fTreeColumn;
  cbHideAuthor.Checked                 := fData.fHideAuthor;
  cbHideFlags.Checked                  := fData.fHideFlags;
  cbHideNumber.Checked                 := fData.fHideNumber;
  cbHideDate.Checked                   := fData.fHideDate;
  cbHideLines.Checked                  := fData.fHideLines;
  cbHideSubject.Checked                := fData.fHideSubject;
  cbHighlightText.Checked              := fData.fHighlightSelectedText;
end;

procedure TfmPropertyPageMessageTreeDisplay.UpdateData;
begin
  if Populating then Exit;

  fData.fUnreadFontStyle := [];
  if cbUnreadMessagesBold.Checked      then Include (fData.fUnreadFontStyle, fsBold);
  if cbUnreadMessagesItalic.Checked    then Include (fData.fUnreadFontStyle, fsItalic);
  if cbUnreadMessagesUnderline.Checked then Include (fData.fUnreadFontStyle, fsUnderline);

  fData.fFirstLineAsSubject         := cbFirstLineAsSubject.Checked;
  fData.fHideReadMessages           := cbHideReadMessages.Checked;
  fData.fHideIgnoredMessages        := cbHideIgnoredMessages.Checked;
  fData.fHideFolderIcons            := cbHideFolderIcons.Checked;
  fData.fDontHighlightXanaNewsUsers := cbDontHighlightXanaNewsUsers.Checked;
  fData.fTreeColumn                 := cbTreeColumn.ItemIndex;
  fData.fHideAuthor                 := cbHideAuthor.Checked;
  fData.fHideFlags                  := cbHideFlags.Checked;
  fData.fHideNumber                 := cbHideNumber.Checked;
  fData.fHideDate                   := cbHideDate.Checked;
  fData.fHideLines                  := cbHideLines.Checked;
  fData.fHideSubject                := cbHideSubject.Checked;
  fData.fHighlightSelectedText      := cbHighlightText.Checked;
end;

{ TPropertyPageMessageTreeDisplayDetails }

function TPropertyPageMessageTreeDisplayDetails.Apply : boolean;
begin
  result := True;
  XNOptions.UnreadFontStyle := fUnreadFontStyle;
  XNOptions.FirstLineAsSubject := fFirstLineAsSubject;
  XNOptions.HideReadMessages := fHideReadMessages;
  XNOptions.HideIgnoredMessages := fHideIgnoredMessages;
  XNOptions.HideFolderIcons := fHideFolderIcons;
  XNOptions.DontHighlightXanaNewsUsers := fDontHighlightXanaNewsUsers;
  XNOptions.TreeColumn := fTreeColumn;
  XNOptions.HideColumn [0] := fHideFlags;
  XNOptions.HideColumn [1] := fHideNumber;
  XNOptions.HideColumn [2] := fHideSubject;
  XNOptions.HideColumn [3] := fHideAuthor;
  XNOptions.HideColumn [4] := fHideDate;
  XNOptions.HideColumn [5] := fHideLines;
  XNOptions.HighlightSelectedText := fHighlightSelectedText;
end;

procedure TPropertyPageMessageTreeDisplayDetails.Initialize;
begin
  fUnreadFontStyle            := XNOptions.UnreadFontStyle;
  fFirstLineAsSubject         := XNOptions.FirstLineAsSubject;
  fHideReadMessages           := XNOptions.HideReadMessages;
  fHideIgnoredMessages        := XNOptions.HideIgnoredMessages;
  fHideFolderIcons            := XNOptions.HideFolderIcons;
  fDontHighlightXanaNewsUsers := XNOptions.DontHighlightXanaNewsUsers;
  fTreeColumn                 := XNOptions.TreeColumn;
  fHideFlags                  := XNOptions.HideColumn [0];
  fHideNumber                 := XNOptions.HideColumn [1];
  fHideSubject                := XNOptions.HideColumn [2];
  fHideAuthor                 := XNOptions.HideColumn [3];
  fHideDate                   := XNOptions.HideColumn [4];
  fHideLines                  := XNOptions.HideColumn [5];
  fHighlightSelectedText      := XNOptions.HighlightSelectedText;
end;

procedure TfmPropertyPageMessageTreeDisplay.ControlClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPageMessageTreeDisplay.cbTreeColumnChange(
  Sender: TObject);
begin
  UpdateData;
end;

end.
