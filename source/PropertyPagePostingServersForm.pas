unit PropertyPagePostingServersForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPagePostingServersData = class (TPropertyPageData)
  private
    fMailAccount : string;
    fPostingAccount : string;
    fAccount : string;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPagePostingServers = class(TfmPropertyPage)
    Label4: TLabel;
    cbMailAccount: TComboBox;
    Label1: TLabel;
    cbPostAccount: TComboBox;
    procedure ControlChange(Sender: TObject);
  private
    fData : TPropertyPagePostingServersData;

    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPagePostingServers: TfmPropertyPagePostingServers;

implementation

uses NewsGlobals, unitNNTPServices, unitMailServices;

{$R *.dfm}

{ TfmPropertyPagePostingServers }

class function TfmPropertyPagePostingServers.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPagePostingServersData;
end;

procedure TfmPropertyPagePostingServers.PopulateControls(
  AData: TPropertyPageData);
var
  i, n, idx : Integer;
  st : string;
begin
  inherited;
  fData := AData as TPropertyPagePostingServersData;
  cbMailAccount.Items.Add(rstDefaultMailAccount);
  idx := 0;
  for i := 0 to MailAccounts.Count - 1 do
  begin
    st := MailAccounts.Items [i].Name;
    cbMailAccount.Items.Add(st);
    if SameText (st, fData.fMailAccount) then
      idx := i + 1
  end;

  cbMailAccount.ItemIndex := idx;

  cbPostAccount.Items.Add(rstThisAccount);
  idx := 0;
  n := 0;
  for i := 0 to NNTPAccounts.Count - 1 do
  begin
    st := NNTPAccounts.Items [i].AccountName;
    if not SameText (st, fData.fAccount) then
    begin
      cbPostAccount.Items.Add(st);
      Inc (n)
    end;
    if SameText (st, fData.fPostingAccount)then
      idx := n
  end;
  cbPostAccount.ItemIndex := idx
end;

procedure TfmPropertyPagePostingServers.UpdateData;
begin
  if cbMailAccount.ItemIndex >= 0 then
    fData.fMailAccount := cbMailAccount.Items [cbMailAccount.ItemIndex];

  if cbPostAccount.ItemIndex >= 0 then
  begin
    fData.fPostingAccount := cbPostAccount.Items [cbPostAccount.ItemIndex];
    if fData.fPostingAccount = rstThisAccount then
      fData.fPostingAccount := ''
  end
end;

{ TPropertyPagePostingServersData }

function TPropertyPagePostingServersData.Apply : boolean;
var
  acct : TNNTPAccount;
begin
  result := True;
  acct := TNNTPAccount (Param);
  acct.MailAccountName := fMailAccount;
  acct.PostingAccountName := fPostingAccount;
end;

procedure TPropertyPagePostingServersData.Initialize;
var
  acct : TNNTPAccount;
begin
  acct := TNNTPAccount (Param);
  fMailAccount := acct.MailAccountName;
  fPostingAccount := acct.PostingAccountName;
  fAccount := acct.AccountName;
end;

procedure TfmPropertyPagePostingServers.ControlChange(
  Sender: TObject);
begin
  UpdateData
end;

end.
