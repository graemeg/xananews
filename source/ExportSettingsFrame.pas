unit ExportSettingsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls;

type
  TfmeExportSettings = class(TFrame)
    cbExportSettings: TCheckBox;
    PopupMenu1: TPopupMenu;
    mnuSelAll: TMenuItem;
    mnuClearAll: TMenuItem;
    N1: TMenuItem;
    mnuSelAccount: TMenuItem;
    mnuClearAccount: TMenuItem;
    Panel1: TPanel;
    lvActions: TListView;
    procedure mnuSelAllClick(Sender: TObject);
    procedure mnuClearAllClick(Sender: TObject);
    procedure mnuSelAccountClick(Sender: TObject);
    procedure mnuClearAccountClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    procedure CheckAll (account, check : boolean);
    function GetExportSettings: boolean;
  public
    procedure Initialize;
    procedure GetGroups (groups : TStrings);

    property ExportSettings : boolean read GetExportSettings;

  end;

implementation

uses unitNNTPServices;

{$R *.dfm}

{ TfmeExportSettings }

procedure TfmeExportSettings.Initialize;
var
  i, j : Integer;
  account : TNNTPAccount;
  group : TSubscribedGroup;
  acName : string;
begin
  lvActions.Parent := Nil;      // Gid rid of weird 'Control Has No Parent Window' bug.
  lvActions.ParentWindow := Panel1.Handle;
  lvActions.Parent := Panel1;

  lvActions.Items.BeginUpdate;
  try
    for i := 0 to NNTPAccounts.Count - 1 do
    begin
      account := NNTPAccounts.Items [i];
      acName := account.AccountName;

      for j := 0 to account.SubscribedGroupCount - 1 do
      begin
        group := account.SubscribedGroups [j];

        with lvActions.Items.Add do
        begin
          data := group;
          Caption := account.AccountName;
          SubItems.Add(group.Name);
        end
      end
    end
  finally
    lvActions.Items.EndUpdate
  end
end;

procedure TfmeExportSettings.mnuSelAllClick(Sender: TObject);
begin
  CheckAll (False, True)
end;

procedure TfmeExportSettings.mnuClearAllClick(Sender: TObject);
begin
  CheckAll (False, False)
end;

procedure TfmeExportSettings.mnuSelAccountClick(Sender: TObject);
begin
  CheckAll (True, True)
end;

procedure TfmeExportSettings.mnuClearAccountClick(Sender: TObject);
begin
  CheckAll (True, False)
end;

procedure TfmeExportSettings.CheckAll(account, check: boolean);
var
  acc : TNNTPAccount;
  i : Integer;
  itm : TListItem;
begin
  if account and Assigned (lvActions.Selected) then
    acc := TSubscribedGroup (lvActions.Selected.Data).Owner
  else
    acc := Nil;

  for i := 0 to lvActions.Items.Count - 1 do
  begin
    itm := lvActions.Items [i];
    if not Assigned (acc) or (TSubscribedGroup (itm.data).Owner = acc) then
      itm.Checked := check
  end
end;

procedure TfmeExportSettings.PopupMenu1Popup(Sender: TObject);
begin
  mnuSelAccount.Enabled := Assigned (lvActions.Selected);
  mnuClearAccount.Enabled := Assigned (lvActions.Selected);
end;

function TfmeExportSettings.GetExportSettings: boolean;
begin
  result := cbExportSettings.Checked
end;

procedure TfmeExportSettings.GetGroups(groups: TStrings);
var
  i : Integer;
begin
  groups.BeginUpdate;
  try
    groups.Clear;

    for i := 0 to lvActions.Items.Count - 1 do
      if lvActions.Items [i].Checked then
        with TSubscribedGroup (lvActions.Items [i].Data) do
          groups.Add(Owner.AccountName + ':' + name)
  finally
    groups.EndUpdate
  end
end;

end.
