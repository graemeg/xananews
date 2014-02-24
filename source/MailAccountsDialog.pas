unit MailAccountsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, unitMailServices;

type
  TdlgMailAccounts = class(TForm)
    lvAccounts: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnProperties: TButton;
    btnClone: TButton;
    CancelBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure lvAccountsDblClick(Sender: TObject);
  private
    fFirstAccount: Boolean;
    fMustReset: Boolean;
    procedure UpdateListItem(idx: Integer; account: TMailAccount);
  protected
    procedure UpdateActions; override;
  public
    property FirstAccount: Boolean read fFirstAccount write fFirstAccount;
    property MustReset: Boolean read fMustReset write fMustReset;
  end;

var
  dlgMailAccounts: TdlgMailAccounts;

implementation

uses
  NewsGlobals, MailAccountForm;

{$R *.dfm}

procedure TdlgMailAccounts.FormShow(Sender: TObject);
var
  i: Integer;
begin
  AdjustFormConstraints(self);
  for i := 0 to MailAccounts.Count - 1 do
    UpdateListItem(i, MailAccounts[i]);

  if fFirstAccount then
    btnAddClick(self)
  else if lvAccounts.Items.Count > 0 then
    lvAccounts.ItemIndex := 0;
end;

procedure TdlgMailAccounts.UpdateListItem(idx: Integer; account: TMailAccount);
var
  item: TListItem;
  st: string;
begin
  if account.ServerSettings.RASConnection = '' then
    st := rstAnyAvailable
  else
    st := account.ServerSettings.RASConnection;

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

  if item.Caption <> account.Name then
  begin
    if item.Caption <> '' then
      try
        MailAccounts.Rename(account, item.Caption)
      except
        account.Name := item.Caption;
        raise;
      end;

    item.Caption := account.Name;
  end;
end;

procedure TdlgMailAccounts.btnAddClick(Sender: TObject);
var
  dlg: TfmMailAccount;
  account: TMailAccount;
begin
  dlg := nil;
  account := TMailAccount.Create(MailAccounts);
  try
    dlg := TfmMailAccount.CreateInit(nil, account);
    if dlg.ShowModal = mrOK then
    begin
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        MailAccounts.Add(account);
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

procedure TdlgMailAccounts.btnRemoveClick(Sender: TObject);
var
  account: TMailAccount;
  msg: string;
  i: Integer;
begin
  if lvAccounts.SelCount > 0 then
  begin
    if lvAccounts.SelCount = 1 then
    begin
      account := MailAccounts[lvAccounts.Selected.Index];
      msg := Format(rstDeleteMessage, [account.Name]);
    end
    else
      msg := Format(rstDeleteMessage, [rstSelectedAccounts]);

    if MessageBox(Handle, PChar(msg), PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2) = ID_YES then
    begin
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        i := 0;
        while i < MailAccounts.Count do
        begin
          if lvAccounts.Items[i].Selected then
          begin
            account := MailAccounts[i];
            MailAccounts.Delete(account);
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

procedure TdlgMailAccounts.btnPropertiesClick(Sender: TObject);
var
  dlg: TfmMailAccount;
  account: TMailAccount;
begin
  account := nil;
  if Assigned(lvAccounts.Selected) then
    account := MailAccounts[lvAccounts.Selected.Index];

  if Assigned(account) then
  begin
    dlg := TfmMailAccount.CreateInit(nil, account);
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

procedure TdlgMailAccounts.btnCloneClick(Sender: TObject);
var
  dlg: TfmMailAccount;
  account: TMailAccount;
begin
  if not Assigned(lvAccounts.Selected) then
    Exit;

  dlg := nil;
  account := TMailAccount.Create(MailAccounts);
  try
    account.CopySettingsFrom(MailAccounts[lvAccounts.Selected.Index]);
    dlg := TfmMailAccount.CreateInit(nil, account);
    if dlg.ShowModal = mrOK then
    begin
      SendMessage(Application.MainForm.Handle, WM_GROUPSCHANGING, 0, 0);
      try
        MailAccounts.Add(account);
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

procedure TdlgMailAccounts.lvAccountsDblClick(Sender: TObject);
begin
  btnPropertiesClick(btnProperties);
end;

procedure TdlgMailAccounts.UpdateActions;
begin
  inherited;
  btnProperties.Enabled := lvAccounts.SelCount = 1;
  btnClone.Enabled := lvAccounts.SelCount = 1;
  btnRemove.Enabled := lvAccounts.SelCount > 0;
end;

end.
