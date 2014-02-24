unit MailAccountForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyBaseForm, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls, unitMailServices, Menus;

type
  TfmMailAccount = class(TfmPropertyBase)
    procedure FormShow(Sender: TObject);
  private
   fNewAccount: Boolean;
  public
    constructor CreateInit(Owner: TComponent; account: TMailAccount);
    { Public declarations }
  end;

var
  fmMailAccount: TfmMailAccount;

implementation

uses
  NewsGlobals,
  PropertyPageMailAccountGeneralForm,
  PropertyPageMailAccountPreferencesForm,
  PropertyPageMailAccountServerForm,
  PropertyPageMailAccountConnectionForm,
  PropertyPagePosting,
  PropertyPageQuotingForm;

{$R *.dfm}

{ TfmMailAccount }

constructor TfmMailAccount.CreateInit(Owner: TComponent; account: TMailAccount);
var
  page: TPropertyPageDetails;
begin
  inherited Create(Owner);

  fNewAccount := account.Name = '';

  AddPropertyPageDetails(TfmPropertyPageMailAccountGeneral, nil, '', '', '', LPARAM(account));
  AddPropertyPageDetails(TfmPropertyPageMailAccountPreferences, nil, '', '', '', LPARAM(account));
  AddPropertyPageDetails(TfmPropertyPageMailAccountServer, nil, '', '', '', LPARAM(account));
  AddPropertyPageDetails(TfmPropertyPageMailAccountConnection, nil, '', '', '', LPARAM(account));

  page := AddPropertyPageDetails(TfmPropertyPageDummy, nil, rstDerivedSettings, rstDerivedMailSettingsHelp);
  AddPropertyPageDetails(TfmPropertyPagePosting, page, '', rstDerivedMailSettingsHelp, '', LPARAM(account.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageQuoting, page, '', rstDerivedMailSettingsHelp, '', LPARAM(account.PostingSettings));
end;

procedure TfmMailAccount.FormShow(Sender: TObject);
begin
  inherited;

  if fNewAccount then
    if (pnlOptions.ControlCount > 1) and (pnlOptions.Controls[1] is TWinControl) then
      ActiveControl := TWinControl(pnlOptions.Controls[1]);
end;

end.
