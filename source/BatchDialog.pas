(*======================================================================*
 | BatchDialog unit for NewsReader3                                     |
 |                                                                      |
 | Allow the user to set up a batch of message retrieval functions      |
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

unit BatchDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, unitNNTPServices, NewsGlobals, unitBatches, Menus;

type
  TdlgBatch = class(TForm)
    lvActions: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edBatchName: TEdit;
    btnAction: TButton;
    cbRunEvery: TCheckBox;
    edRunEvery: TEdit;
    Label3: TLabel;
    cbRunAtStartup: TCheckBox;
    PopupMenu1: TPopupMenu;
    SelectAll1: TMenuItem;
    SelectNone1: TMenuItem;
    SelectAllGroupsinAccount1: TMenuItem;
    N1: TMenuItem;
    UncheckAllGroupsinAccount1: TMenuItem;
    procedure UncheckAllGroupsinAccount1Click(Sender: TObject);
    procedure SelectAllGroupsinAccount1Click(Sender: TObject);
    procedure SelectNone1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure lvActionsColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormShow(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure lvActionsDblClick(Sender: TObject);
  private
    fUpdating: Boolean;
    procedure WmSetup(var msg: TMessage); message WM_SETUP;
    procedure WmNotify(var msg: TWMNotify); message WM_NOTIFY;
    procedure DoCheckChange(Item: TListItem; checked: Boolean);
    procedure SelectAll(const account: string; selected: Boolean);
  protected
    procedure UpdateActions; override;
  public
    Batch: TNNTPBatch;
  end;

var
  dlgBatch: TdlgBatch;

implementation

uses MessagesDialog, CommCtrl;

{$R *.dfm}

procedure TdlgBatch.FormShow(Sender: TObject);
begin
  AdjustFormConstraints(self);
  PostMessage(handle, WM_SETUP, 0, 0);
end;

procedure TdlgBatch.btnActionClick(Sender: TObject);
var
  Action: TBatchAction;
  dlg: TdlgGetMessages;
  i: Integer;
  txt: string;
  grp: TSubscribedGroup;
begin
  if Assigned(lvActions.Selected) then
  begin
    fUpdating := True;
    try
      for i := 0 to lvActions.Items.Count - 1 do
        if lvActions.Items[i].Selected then
          lvActions.Items[i].Checked := True;
    finally
      fUpdating := False;
    end;

    if lvActions.SelCount = 1 then
      grp := lvActions.Selected.Data
    else
      grp := nil;
    dlg := nil;
    Action := TBatchAction.Create;
    try
      action.ActionText := lvActions.Selected.SubItems[1];
      Application.CreateForm(TdlgGetMessages, dlg);
      dlg.group := grp;
      dlg.Action := Action;

      if dlg.ShowModal = mrOK then
      begin
        dlg.UpdateAct;
        txt := Action.ActionText;

        for i := 0 to lvActions.Items.Count - 1 do
          if lvActions.Items[i].Selected then
            lvActions.Items[i].SubItems[1] := txt;
      end;
    finally
      action.Free;
      dlg.Free;
    end;
  end;
end;

procedure TdlgBatch.lvActionsDblClick(Sender: TObject);
begin
  btnActionClick(nil);
end;

procedure TdlgBatch.UpdateActions;
var
  seld: Boolean;
begin
  seld := Assigned(lvActions.Selected);

  btnAction.Enabled := seld;
  btnOK.Enabled := edBatchName.Text <> '';
  edRunEvery.Enabled := cbRunEvery.Checked;

  SelectAllGroupsInAccount1.Enabled := seld;
  UnCheckAllGroupsInAccount1.Enabled := seld;
end;

procedure TdlgBatch.WmSetup(var msg: TMessage);
var
  i, j, k: Integer;
  account: TNNTPAccount;
  group: TSubscribedGroup;
  sel: Boolean;
  acName, grName, st: string;
begin
  edBatchName.Text := batch.BatchName;
  cbRunEvery.Checked := batch.Scheduled;
  cbRunAtStartup.Checked := batch.RunAtStart;
  edRunEvery.Text := IntToStr(batch.RunEvery);
  lvActions.Clear;
  fUpdating := True;
  lvActions.Items.BeginUpdate;
  try
    for i := 0 to NNTPAccounts.Count - 1 do
    begin
      account := NNTPAccounts.Items[i];
      acName := account.AccountName;

      for j := 0 to account.SubscribedGroupCount - 1 do
      begin
        group := account.SubscribedGroups[j];
        grName := acName + '_' + group.Name;
        k := batch.IndexOf(grName);

        if k >= 0 then
        begin
          sel := True;
          st := batch.Actions[k].ActionText;
        end
        else
        begin
          sel := False;
          st := 'Get all new messages';
        end;

        with lvActions.Items.Add do
        begin
          data := group;
          Caption := account.AccountName;
          SubItems.Add(group.Name);
          SubItems.Add(st);
          Checked := sel;
        end;
      end;
    end;
  finally
    lvActions.Items.EndUpdate;
    fUpdating := False;
  end;

  i := 0;
  while i < lvActions.Items.Count - 1 do
    if lvActions.Items[i].Checked then
    begin
      lvActions.Items[i].Selected := True;

      Inc(i, lvActions.VisibleRowCount - 1);
      if i >= lvActions.Items.Count then
        i := lvActions.Items.Count - 1;

      lvActions.Items[i].MakeVisible(False);
      Break;
    end
    else
      Inc(i);
end;

function SortFunc(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
begin
  case lParam of
    0: Result := CompareText(Item1.Caption, Item2.Caption);
  else
    Result := CompareText(Item1.SubItems[lParam - 1], Item2.SubItems[lParam - 1]);
  end;
end;

procedure TdlgBatch.lvActionsColumnClick(Sender: TObject; Column: TListColumn);
var
  itm: TListItem;
begin
  lvActions.CustomSort(@SortFunc, Column.Index);
  itm := lvActions.Selected;
  if Assigned(itm) then
    itm.MakeVisible(False);
end;

procedure TdlgBatch.WmNotify(var msg: TWMNotify);
var
  lvNMHdr: PNMListView;
  oldStateImgIdx, newStateImgIdx: Integer;
begin
  inherited;

  if msg.NMHdr^.hwndFrom = lvActions.Handle then
  begin
    lvNMHdr := PNMListView(msg.NMHdr);

    if lvNMhdr^.hdr.code = LVN_ITEMCHANGED then
      if lvNMHdr^.uChanged = LVIF_STATE then
      begin
        oldStateImgIdx := ((lvNMHdr^.uOldState and LVIS_STATEIMAGEMASK) shr 12);
        newStateImgIdx := ((lvNMHdr^.uNewState and LVIS_STATEIMAGEMASK) shr 12);

        if (oldStateImgIdx <> newStateImgIdx) and (oldStateImgIdx <> 0) and (newStateImgIdx <> 0) then
          DoCheckChange(lvActions.Items[lvNMHdr^.iItem], newStateImgIdx = 2);
      end;
  end;
end;

procedure TdlgBatch.DoCheckChange(Item: TListItem; checked: Boolean);
var
  i: Integer;
begin
  if fUpdating then Exit;
  fUpdating := True;
  try
    for i := 0 to lvActions.Items.Count - 1 do
      if lvActions.Items[i].Selected then
        lvActions.Items[i].Checked := checked;
  finally
    fUpdating := False;
  end;
end;

procedure TdlgBatch.SelectAll1Click(Sender: TObject);
begin
  SelectAll('', True);
end;

procedure TdlgBatch.SelectNone1Click(Sender: TObject);
begin
  SelectAll('', False);
end;

procedure TdlgBatch.SelectAllGroupsinAccount1Click(Sender: TObject);
var
  itm: TListItem;
begin
  itm := lvActions.Selected;
  if Assigned(itm) then
    SelectAll(itm.Caption, True);
end;

procedure TdlgBatch.SelectAll(const account: string; selected: Boolean);
var
  i: Integer;
begin
  fUpdating := True;
  try
    for i := 0 to lvActions.Items.Count - 1 do
      if (account = '') or (lvActions.Items[i].Caption = account) then
        lvActions.Items[i].Checked := selected;
  finally
    fUpdating := False;
  end
end;

procedure TdlgBatch.UncheckAllGroupsinAccount1Click(Sender: TObject);
var
  itm: TListItem;
begin
  itm := lvActions.Selected;
  if Assigned(itm) then
    SelectAll(itm.Caption, False);
end;

end.
