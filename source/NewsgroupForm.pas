unit NewsgroupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyBaseForm, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls, unitNNTPServices, Menus;

type
  TfmNewsgroup = class(TfmPropertyBase)
  private
    fGrp: TSubscribedGroup;
  protected
    function SaveSettings: Boolean; override;
  public
    constructor CreateInit(AOwner: TComponent; grp: TSubscribedGroup);
  end;

var
  fmNewsgroup: TfmNewsgroup;

implementation

uses
  NewsGlobals,
  PropertyPagePosting,
  PropertyPageExtraPostingForm,
  PropertyPageQuotingForm,
  PropertyPageFiltersForm,
  PropertyPagePreferencesForm,
  PropertyPageNewsgroupGeneralForm,
  PropertyPageSortingForm;

{$R *.dfm}

{ TfmNewsgroup }

constructor TfmNewsgroup.CreateInit(AOwner: TComponent; grp: TSubscribedGroup);
var
  page, pg1: TPropertyPageDetails;
begin
  inherited Create(AOwner);
  fGrp := grp;

  AddPropertyPageDetails(TfmPropertyPageNewsgroupGeneral, nil, '', '', '', LPARAM(grp));

  page := AddPropertyPageDetails(TfmPropertyPageDummy, nil, rstDerivedSettings, rstDerivedNewsgroupSettingsHelp);

  pg1 := AddPropertyPageDetails(TfmPropertyPagePreferences, page, '', rstDerivedNewsgroupSettingsHelp, '', LPARAM(grp.DisplaySettings));
  TPropertyPagePreferencesData(pg1.Data).InitObject(grp);

  AddPropertyPageDetails(TfmPropertyPageSorting, page, '', rstDerivedNewsgroupSettingsHelp, '', LPARAM(grp.DisplaySettings));
  AddPropertyPageDetails(TfmPropertyPagePosting, page, '', rstDerivedNewsgroupSettingsHelp, '', LPARAM(grp.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageExtraPosting, page, '', rstDerivedNewsgroupSettingsHelp, '', LPARAM(grp.NNTPSettings));
  AddPropertyPageDetails(TfmPropertyPageQuoting, page, '', rstDerivedNewsgroupSettingsHelp, '', LPARAM(grp.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageFilters, page, '', '', '', LPARAM(grp));
end;

function TfmNewsgroup.SaveSettings: Boolean;
begin
  Result := inherited SaveSettings;

  fGrp.fSorting := True;
  try
    fGrp.ThreadOrder := fGrp.DisplaySettings.ThreadOrder;
    fGrp.ThreadSortOrder := fGrp.DisplaySettings.ThreadSortOrder;
    fGrp.ThreadSortDirection := fGrp.DisplaySettings.ThreadSortDirection;
  finally
    fGrp.fSorting := False;
  end;
end;

end.
