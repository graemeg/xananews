unit AccountForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyBaseForm, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls, unitNNTPServices, Menus;

type
  TfmAccount = class(TfmPropertyBase)
    procedure FormShow(Sender: TObject);
  private
    fNewAccount : boolean;
  public
    constructor CreateInit (AOwner : TComponent; Acct : TNNTPAccount);
  end;

var
  fmAccount: TfmAccount;

implementation

uses
  NewsGlobals,
  PropertyPageForm,
  PropertyPageAccountGeneralForm,
  PropertyPagePosting,
  PropertyPageExtraPostingForm,
  PropertyPageAccountServerForm,
  PropertyPageAccountAdvancedServerForm,
  PropertyPageQuotingForm,
  PropertyPageFiltersForm,
  PropertyPagePreferencesForm,
  PropertyPageSortingForm,
  PropertyPagePostingServersForm;

{$R *.dfm}

{ TfmAccount }

constructor TfmAccount.CreateInit(AOwner: TComponent; acct : TNNTPAccount);
var
  page, pg1 : TPropertyPageDetails;
begin
  inherited Create (AOwner);
  fNewAccount := acct.AccountName = '';

  AddPropertyPageDetails (TfmPropertyPageAccountGeneral, nil, '', '', '', Integer (acct));
  AddPropertyPageDetails (TfmPropertyPageAccountServer,  nil, '', '', '', Integer (acct));
  AddPropertyPageDetails (TfmPropertyPageAccountAdvancedServer,  nil, '', '', '', Integer (acct));
  AddPropertyPageDetails (TfmPropertyPagePostingServers,  nil, '', '', '', Integer (acct));
  page := AddPropertyPageDetails (TfmPropertyPageDummy, Nil, rstDerivedSettings, rstDerivedAccountSettingsHelp);

  pg1 := AddPropertyPageDetails (TfmPropertyPagePreferences, page, '', rstDerivedAccountSettingsHelp, '', Integer (acct.DisplaySettings));
  TPropertyPagePreferencesData (pg1.Data).InitObject(acct);

  AddPropertyPageDetails (TfmPropertyPageSorting, page, '', rstDerivedAccountSettingsHelp, '', Integer (acct.DisplaySettings));
  AddPropertyPageDetails (TfmPropertyPagePosting, page, '', rstDerivedAccountSettingsHelp, '', Integer (acct.PostingSettings));
  AddPropertyPageDetails (TfmPropertyPageExtraPosting, page, '', rstDerivedAccountSettingsHelp, '', Integer (acct.NNTPSettings));
  AddPropertyPageDetails (TfmPropertyPageQuoting, page, '', rstDerivedAccountSettingsHelp, '', Integer (acct.PostingSettings));
  AddPropertyPageDetails (TfmPropertyPageFilters, page, '', '', '', Integer (acct));
end;

procedure TfmAccount.FormShow(Sender: TObject);
begin
  inherited;

  if fNewAccount then
    if (pnlOptions.ControlCount > 1) and (pnlOptions.Controls [1] is TWinControl) then
      ActiveControl := TWinControl (pnlOptions.Controls [1])
end;

end.
