(*======================================================================*
 | AccountsDialog unit for NewsReader3                                  |
 |                                                                      |
 | Display list of Server Accounts                                      |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit AccountsDialog;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, AccountForm, unitNNTPServices, Dialogs;

type
  TdlgAccounts = class(TForm)
    CancelBtn: TButton;
    lvAccounts: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnProperties: TButton;
    btnClone: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lvAccountsDblClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
  private
    fMustReset: Boolean;
    fFirstAccount: Boolean;
    procedure UpdateListItem(idx: Integer; account: TNNTPAccount);
  protected
    procedure UpdateActions; override;
  public
    property MustReset: Boolean read fMustReset;
    property FirstAccount: Boolean read fFirstAccount write fFirstAccount;
    { Public declarations }
  end;

var
  dlgAccounts: TdlgAccounts;

implementation

{$R *.dfm}

uses NewsGlobals;

procedure TdlgAccounts.btnAddClick(Sender: TObject);
var
  dlg: TfmAccount;
  account: TNNTPAccount;
begin
  dlg := nil;
  account := TNNTPAccount.Create(NNTPAccounts);
  try
    dlg := TfmAccount.CreateInit(nil, account);
    if dlg.ShowModal = mrOK then
    begin
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        NNTPAccounts.Add(account);
        UpdateListItem(lvAccounts.Items.Count, account);
      finally
        SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGED, 0, 0);
      end;
    end
    else
      account.Free;
  finally
    dlg.Free;
  end;
end;

procedure TdlgAccounts.FormShow(Sender: TObject);
var
  i: Integer;
begin
  AdjustFormConstraints(self);
  for i := 0 to NNTPAccounts.Count - 1 do
    UpdateListItem(i, NNTPAccounts[i]);

  if fFirstAccount then
    btnAddClick(Self);

  if lvAccounts.Items.Count > 0 then
    lvAccounts.ItemIndex := 0;
end;

procedure TdlgAccounts.UpdateActions;
begin
  inherited;
  btnProperties.Enabled := lvAccounts.SelCount = 1;
  btnClone.Enabled := lvAccounts.SelCount = 1;
  btnRemove.Enabled := lvAccounts.SelCount > 0;
end;

procedure TdlgAccounts.btnPropertiesClick(Sender: TObject);
var
  dlg: TfmAccount;
  account: TNNTPAccount;
begin
  account := nil;
  if Assigned(lvAccounts.Selected) then
    account := NNTPAccounts[lvAccounts.Selected.Index];

  if Assigned(account) then
  begin
    dlg := TfmAccount.CreateInit(nil, account);
    try
      if dlg.ShowModal = mrOK then
      begin
        SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
        try
          UpdateListItem(lvAccounts.Selected.Index, account);
        finally
          SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGED, 0, 0);
        end;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TdlgAccounts.UpdateListItem(idx: Integer; account: TNNTPAccount);
var
  item: TListItem;
  st: string;
begin
  if account.NNTPServerSettings.RASConnection = '' then
    st := rstAnyAvailable
  else
    st := account.NNTPServerSettings.RASConnection;

  if idx < lvAccounts.Items.Count then
  begin
    item := lvAccounts.Items[idx];
    item.SubItems[0] := st;
  end
  else
  begin
    item := lvAccounts.Items.Add;
    item.SubItems.Add(st);
  end;

  Item.Caption := account.AccountName;
end;

procedure TdlgAccounts.btnRemoveClick(Sender: TObject);
var
  account: TNNTPAccount;
  msg: string;
  i: Integer;
begin
  if lvAccounts.SelCount > 0 then
  begin
    if lvAccounts.SelCount = 1 then
    begin
      account := NNTPAccounts[lvAccounts.Selected.Index];
      msg := Format(rstDeleteMessage, [account.AccountName]);
    end
    else
      msg := Format(rstDeleteMessage, [rstSelectedAccounts]);

    if MessageBox(handle, PChar(msg), PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2) = ID_YES then
    begin
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        i := 0;
        while i < NNTPAccounts.Count do
        begin
          if lvAccounts.Items[i].Selected then
          begin
            account := NNTPAccounts[i];
            NNTPAccounts.Delete(account);
            lvAccounts.Selected.Free;
          end
          else
            Inc(i);
        end;
      finally
        SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGED, 0, 0);
      end;
    end;
  end;
end;

procedure TdlgAccounts.lvAccountsDblClick(Sender: TObject);
begin
  btnPropertiesClick(nil);
end;

procedure TdlgAccounts.btnCloneClick(Sender: TObject);
var
  dlg: TfmAccount;
  account: TNNTPAccount;
begin
  if not Assigned(lvAccounts.Selected) then Exit;

  dlg := nil;
  account := TNNTPAccount.Create(NNTPAccounts);
  try
    account.CopySettingsFrom(NNTPAccounts[lvAccounts.Selected.Index]);
    dlg := TfmAccount.CreateInit(nil, account);
    if dlg.ShowModal = mrOK then
    begin
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        NNTPAccounts.Add(account);
        UpdateListItem(lvAccounts.Items.Count, account);
      finally
        SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGED, 0, 0);
      end;
    end
    else
      account.Free;
  finally
    dlg.Free;
  end;
end;

end.
