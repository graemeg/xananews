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
    fNewAccount: Boolean;
  public
    constructor CreateInit(AOwner: TComponent; acct: TNNTPAccount);
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

constructor TfmAccount.CreateInit(AOwner: TComponent; acct: TNNTPAccount);
var
  page, pg1: TPropertyPageDetails;
begin
  inherited Create(AOwner);
  fNewAccount := acct.AccountName = '';

  AddPropertyPageDetails(TfmPropertyPageAccountGeneral, nil, '', '', '', LPARAM(acct));
  AddPropertyPageDetails(TfmPropertyPageAccountServer,  nil, '', '', '', LPARAM(acct));
  AddPropertyPageDetails(TfmPropertyPageAccountAdvancedServer,  nil, '', '', '', LPARAM(acct));
  AddPropertyPageDetails(TfmPropertyPagePostingServers,  nil, '', '', '', LPARAM(acct));
  page := AddPropertyPageDetails(TfmPropertyPageDummy, nil, rstDerivedSettings, rstDerivedAccountSettingsHelp);

  pg1 := AddPropertyPageDetails(TfmPropertyPagePreferences, page, '', rstDerivedAccountSettingsHelp, '', LPARAM(acct.DisplaySettings));
  TPropertyPagePreferencesData(pg1.Data).InitObject(acct);

  AddPropertyPageDetails(TfmPropertyPageSorting, page, '', rstDerivedAccountSettingsHelp, '', LPARAM(acct.DisplaySettings));
  AddPropertyPageDetails(TfmPropertyPagePosting, page, '', rstDerivedAccountSettingsHelp, '', LPARAM(acct.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageExtraPosting, page, '', rstDerivedAccountSettingsHelp, '', LPARAM(acct.NNTPSettings));
  AddPropertyPageDetails(TfmPropertyPageQuoting, page, '', rstDerivedAccountSettingsHelp, '', LPARAM(acct.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageFilters, page, '', '', '', LPARAM(acct));
end;

procedure TfmAccount.FormShow(Sender: TObject);
begin
  inherited;

  if fNewAccount then
    if (pnlOptions.ControlCount > 1) and (pnlOptions.Controls[1] is TWinControl) then
      ActiveControl := TWinControl (pnlOptions.Controls[1]);
end;

end.
