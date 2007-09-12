unit NewsgroupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyBaseForm, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls, unitNNTPServices, Menus;

type
  TfmNewsgroup = class(TfmPropertyBase)
  private
    fGrp : TSubscribedGroup;
  protected
    function SaveSettings : boolean; override;
  public
    constructor CreateInit (AOwner : TComponent; grp : TSubscribedGroup);
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

constructor TfmNewsgroup.CreateInit(AOwner: TComponent;
  grp: TSubscribedGroup);
var
  page, pg1 : TPropertyPageDetails;
begin
  inherited Create (AOwner);
  fGrp := grp;

  AddPropertyPageDetails (TfmPropertyPageNewsgroupGeneral, Nil, '', '', '', Integer (grp));

  page := AddPropertyPageDetails (TfmPropertyPageDummy, Nil, rstDerivedSettings, rstDerivedNewsgroupSettingsHelp);

  pg1 := AddPropertyPageDetails (TfmPropertyPagePreferences, page, '', rstDerivedNewsgroupSettingsHelp, '', Integer (grp.DisplaySettings));
  TPropertyPagePreferencesData (pg1.Data).InitObject(grp);

  AddPropertyPageDetails (TfmPropertyPageSorting, page, '', rstDerivedNewsgroupSettingsHelp, '', Integer (grp.DisplaySettings));
  AddPropertyPageDetails (TfmPropertyPagePosting, page, '', rstDerivedNewsgroupSettingsHelp, '', Integer (grp.PostingSettings));
  AddPropertyPageDetails (TfmPropertyPageExtraPosting, page, '', rstDerivedNewsgroupSettingsHelp, '', Integer (grp.NNTPSettings));
  AddPropertyPageDetails (TfmPropertyPageQuoting, page, '', rstDerivedNewsgroupSettingsHelp, '', Integer (grp.PostingSettings));
  AddPropertyPageDetails (TfmPropertyPageFilters, page, '', '', '', Integer (grp));
end;

function TfmNewsgroup.SaveSettings : boolean;
begin
  result := inherited SaveSettings;

  fGrp.fSorting := True;
  try
    fGrp.ThreadOrder := fGrp.DisplaySettings.ThreadOrder;
    fGrp.ThreadSortOrder := fGrp.DisplaySettings.ThreadSortOrder;
    fGrp.ThreadSortDirection := fGrp.DisplaySettings.ThreadSortDirection;
  finally
    fGrp.fSorting := False
  end
end;

end.
