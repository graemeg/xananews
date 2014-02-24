unit PropertyPageGroupsTreeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageGroupsTreeData = class (TPropertyPageData)
    fShowMessageCount : boolean;
    fAutoExpandGroupTree : boolean;
    fAutoContractGroupTree : boolean;
    fUnreadFontStyle : TFontStyles;
    fTrimGroupNames : Integer;
    fShowInterestingMarkers : Integer;

  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageGroupsTree = class(TfmPropertyPage)
    cbShowMessageCount: TCheckBox;
    cbAutoExpandGroupTree: TCheckBox;
    cbUnreadNewsgroupsBold: TCheckBox;
    cbUnreadNewsgroupsItalic: TCheckBox;
    cbUnreadNewsgroupsUnderline: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    rbTrimNone: TRadioButton;
    rbTrimRelaxed: TRadioButton;
    rbTrimAggressive: TRadioButton;
    Label3: TLabel;
    Panel2: TPanel;
    rbMarkersNone: TRadioButton;
    rbMarkersUnread: TRadioButton;
    rbMarkersAll: TRadioButton;
    cbAutoContractGroupTree: TCheckBox;
    procedure ControlClick(Sender: TObject);
  private
    fData : TPropertyPageGroupsTreeData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageGroupsTree: TfmPropertyPageGroupsTree;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageGroupsTree }

class function TfmPropertyPageGroupsTree.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageGroupsTreeData;
end;

procedure TfmPropertyPageGroupsTree.PopulateControls (AData : TPropertyPageData);
begin
  inherited;

  fData := AData as TPropertyPageGroupsTreeData;

  cbShowMessageCount.Checked := fData.fShowMessageCount;
  cbAutoExpandGroupTree.Checked := fData.fAutoExpandGroupTree;
  cbAutoContractGroupTree.Checked := fData.fAutoContractGroupTree;

  cbUnreadNewsgroupsBold.Checked := fsBold in fData.fUnreadFontStyle;
  cbUnreadNewsgroupsItalic.Checked := fsItalic in fData.fUnreadFontStyle;
  cbUnreadNewsgroupsUnderline.Checked := fsUnderline in fData.fUnreadFontStyle;

  case fData.fTrimGroupNames of
    1 : rbTrimRelaxed.Checked := True;
    2 : rbTrimAggressive.Checked := True;
    else
      rbTrimNone.Checked := True
  end;

  case fData.fShowInterestingMarkers of
    1 : rbMarkersUnread.Checked := True;
    2 : rbMarkersAll.Checked := True;
    else
      rbMarkersNone.Checked := True
  end
end;

procedure TfmPropertyPageGroupsTree.UpdateData;
begin
  if Populating then Exit;
  fData.fShowMessageCount := cbShowMessageCount.Checked;
  fData.fAutoExpandGroupTree := cbAutoExpandGroupTree.Checked;
  fData.fAutoContractGroupTree := cbAutoContractGroupTree.Checked;
  fData.fUnreadFontStyle := [];
  if cbUnreadNewsgroupsBold.Checked then Include (fData.fUnreadFontStyle, fsBold);
  if cbUnreadNewsgroupsItalic.Checked then Include (fData.fUnreadFontStyle, fsItalic);
  if cbUnreadNewsgroupsUnderline.Checked then Include (fData.fUnreadFontStyle, fsUnderline);

  if rbTrimAggressive.Checked then
    fData.fTrimGroupNames := 2
  else
    if rbTrimRelaxed.Checked then
      fData.fTrimGroupNames := 1
    else
      fData.fTrimGroupNames := 0;

  if rbMarkersAll.Checked then
    fData.fShowInterestingMarkers := 2
  else
    if rbMarkersUnread.Checked then
      fData.fShowInterestingMarkers := 1
    else
      fData.fShowInterestingMarkers := 0
end;

{ TPropertyPageGroupsTreeData }

function TPropertyPageGroupsTreeData.Apply : boolean;
begin
  result := True;
  XNOptions.ShowMessageCount := fShowMessageCount;
  XNOptions.AutoExpandGroupTree := fAutoExpandGroupTree;
  XNOptions.AutoContractGroupTree := fAutoContractGroupTree;
  XNOptions.UnreadNewsgroupsFontStyle := fUnreadFontStyle;
  XNOptions.TrimGroupNames := fTrimGroupNames;
  XNOptions.ShowInterestingMarkers := fShowInterestingMarkers;
end;

procedure TPropertyPageGroupsTreeData.Initialize;
begin
  fShowMessageCount := XNOptions.ShowMessageCount;
  fAutoExpandGroupTree := XNOptions.AutoExpandGroupTree;
  fAutoContractGroupTree := XNOptions.AutoContractGroupTree;
  fUnreadFontStyle := XNOptions.UnreadNewsgroupsFontStyle;
  fTrimGroupNames := XNOptions.TrimGroupNames;
  fShowInterestingMarkers := XNOptions.ShowInterestingMarkers
end;

procedure TfmPropertyPageGroupsTree.ControlClick(
  Sender: TObject);
begin
  UpdateData
end;

end.
