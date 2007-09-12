(*======================================================================*
 | NewsgroupsDialog unit for NewsReader3                                |
 |                                                                      |
 | Newsgroup list/subscribe/unsubscribe dialog.                         |
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
 | 1.0      06/11/2001  CPWW  Original                                  |
 *======================================================================*)

unit NewsgroupsDialog;

interface

uses Windows, Dialogs, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, idNNTPX, unitNNTPServices, ImgList;

type
  TdlgNewsgroups = class(TForm)
    lvNewsgroups: TListView;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    Label1: TLabel;
    edFilter: TEdit;
    btnSubscribe: TButton;
    ImageList1: TImageList;
    CancelBtn: TButton;
    cbShowSubscribedGroupsOnly: TCheckBox;
    cbNewGroupsOnly: TCheckBox;
    btnUnsubscribe: TButton;
    stSubscribedCounts: TLabel;
    btnCopy: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvNewsgroupsData(Sender: TObject; Item: TListItem);
    procedure edFilterChange(Sender: TObject);
    procedure btnSubscribeClick(Sender: TObject);
    procedure lvNewsgroupsDblClick(Sender: TObject);
    procedure cbShowSubscribedGroupsOnlyClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure btnUnsubscribeClick(Sender: TObject);
    procedure lvNewsgroupsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnCopyClick(Sender: TObject);
  private
    fAccount: TNNTPAccount;
    fRawData : TStringList;
    fUppercaseGroupNames : TStringList;
    fGroupNames : TStringList;
    fFilteredData : TStringList;
    fFiltered : boolean;
    fHasNewGroups : boolean;
    fOrigSubscribedCounts : string;

    procedure ShowGroupList;
    procedure UpdateSubscribedCounts;
  public
    property Account : TNNTPAccount read fAccount write fAccount;
    { Public declarations }
  end;

var
  dlgNewsgroups: TdlgNewsgroups;

implementation

uses NewsGlobals, unitSearchString, ClipBrd;

{$R *.dfm}

(*----------------------------------------------------------------------*
 | TdlgNewsgroups.FormShow                                              |
 |                                                                      |
 | OnShow handler for newsgroups dialog.  Initialize.                   |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgNewsgroups.FormShow(Sender: TObject);
var
  i : Integer;
  st, s : string;
  requiresSorting : boolean;
begin
  Caption := Format (rstNewsgroupsFor, [Account.AccountName]);
  fOrigSubscribedCounts := stSubscribedCounts.Caption;
  screen.Cursor := crHourglass;
  try
    fRawData.LoadFromFile(account.FileName);
    s := '';
    requiresSorting := False;
    for i := 0 to fRawData.Count - 1 do
    begin
      st := fRawData [i];
      if not fhasNewGroups then
        fHasNewGroups := Copy (st, Length (st), 1) = '*';

      if AnsiCompareText(st, s) < 0 then
        requiresSorting := True
      else
        s := st;
    end;
    if requiresSorting then
    begin
      fRawData.Sort;
      fRawData.SaveToFile(account.FileName);
    end;

    if not fHasNewGroups then
      cbNewGroupsOnly.Enabled := False;

    i := 0;
    while i < fRawData.Count do
    begin
      st := fRawData [i];
      if length (st) > 0 then
      begin
        if st [1] = '"' then
          s := AnsiDequotedStr (st, '"')
        else
          s := SplitString (' ', st);
        fUppercaseGroupNames.Add(Uppercase (s));
        fGroupNames.Add (s);
        Inc (i)
      end
      else
        fRawData.Delete (i)
    end
  finally
    screen.Cursor := crDefault
  end;
  UpdateSubscribedCounts;
  ShowGroupList
end;

(*----------------------------------------------------------------------*
 | TdlgNewsgroups.FormCreate                                            |
 |                                                                      |
 | OnCreate handler for form.  Create the string lists.                 |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgNewsgroups.FormCreate(Sender: TObject);
begin
  fRawData := TStringList.Create;
  fUppercaseGroupNames := TStringList.Create;
  fGroupNames := TStringList.Create;
  fFilteredData := TStringList.Create;
end;

(*----------------------------------------------------------------------*
 | TdlgNewsgroups.FormDestroy                                           |
 |                                                                      |
 | OnDestroy handler.  Tidy up.                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgNewsgroups.FormDestroy(Sender: TObject);
begin
  fRawData.Free;
  fUppercaseGroupNames.Free;
  fGroupNames.Free;
  fFilteredData.Free;
end;

(*----------------------------------------------------------------------*
 | TdlgNewsgroups.lvNewsgroupsData                                      |
 |                                                                      |
 | OnData handler for (virtual) list view.                              |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject;                                                   |
 |   Item: TListItem                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgNewsgroups.lvNewsgroupsData(Sender: TObject; Item: TListItem);
var
  st, stx : string;
  hi, lo : DWORD;
  isNew : boolean;
begin
  if fFiltered then             // Split newgroup line into it's constituents
    ParseNewsGroup (fFilteredData [item.Index], st, hi, lo, stx, isNew)
  else
    ParseNewsGroup (fRawData [item.Index], st, hi, lo, stx, isNew);

  item.caption := st;

  if account.IsSubscribedTo(st) then
    item.ImageIndex := 0
  else
    if isNew then
      item.ImageIndex := 1
    else
      item.ImageIndex := -1
end;

(*----------------------------------------------------------------------*
 | TdlgNewsgroups.edFilterChange                                        |
 |                                                                      |
 | OnChange handler for filter edit box.  Recalculate the filtered list.|
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgNewsgroups.edFilterChange(Sender: TObject);
begin
  ShowGroupList;
end;

(*----------------------------------------------------------------------*
 | TdlgNewsgroups.btnSubscribeClick                                     |
 |                                                                      |
 | OnClick handler for subscribe/unsubscribe button.                    |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgNewsgroups.btnSubscribeClick(Sender: TObject);
var
  groupName : string;
  itm : TListItem;
begin
  if Assigned (lvNewsgroups.Selected) then
  begin
    itm := lvNewsgroups.Selected;
    while Assigned (itm) do
    begin
      groupName := itm.Caption;

      if not account.IsSubscribedTo(groupName) then
      begin
        account.SubscribeTo (groupName);
        SendMessage (Application.MainForm.Handle, WM_UNSUBSCRIBE, 0, 0)
      end;

      itm := lvNewsgroups.GetNextItem(itm, sdAll, [isSelected])
    end;

    lvNewsgroups.Invalidate;
    lvNewsgroupsChange (nil, nil, TItemChange (0));
    UpdateSubscribedCounts;
  end
end;

procedure TdlgNewsgroups.lvNewsgroupsDblClick(Sender: TObject);
begin
  btnSubscribeClick (lvNewsgroups)
end;

procedure TdlgNewsgroups.cbShowSubscribedGroupsOnlyClick(Sender: TObject);
begin
  ShowGroupList
end;

procedure TdlgNewsgroups.ShowGroupList;
var
  i : Integer;
  filter : string;
  subscribedOnly : Boolean;
  newOnly : boolean;
  st : string;
begin
  filter := edFilter.Text;
  subscribedOnly := cbShowSubscribedGroupsOnly.Checked;
  newOnly := cbNewgroupsOnly.Checked;
  if (filter = '') and not subscribedOnly and not newOnly then
  begin
    fFiltered := False;
    lvNewsgroups.Items.Count := fRawData.Count
  end
  else
  begin
    fFilteredData.Clear;
    fFiltered := True;

    fFilteredData.BeginUpdate;
    try
      filter := UpperCase (filter);
      for i := 0 to fRawData.Count - 1 do
        if (Filter = '') or (AnsiPos (filter, fUppercaseGroupNames [i]) > 0) then
          if (not subscribedOnly) or (account.IsSubscribedTo(fGroupNames [i])) then
          begin
            st := fRawData [i];
            if not newOnly or (Copy (st, Length (st), 1) = '*') then
              fFilteredData.Add (st);
          end
    finally
      fFilteredData.EndUpdate
    end;

    lvNewsgroups.Items.Count := fFilteredData.Count;
  end;

  lvNewsgroups.Invalidate;
  lvNewsgroupsChange (nil, nil, TItemChange (0));
end;

procedure TdlgNewsgroups.CancelBtnClick(Sender: TObject);
var
  i  : Integer;
  dt : TDateTime;
  fileOk : boolean;
  st : string;
begin
  if fHasNewGroups then
  begin
    fileOk := FileAge (account.FileName, dt);
    fRawData.Sort;

    for i := 0 to fRawData.Count - 1 do
    begin
      st := fRawData [i];
      if Copy (st, Length (st), 1) = '*' then
        fRawData [i] := Trim (Copy (st, 1, Length (st) - 1))
    end;
    fRawData.SaveToFile(account.FileName);
    if fileOk then
      FileSetDate (account.FileName, DateTimeToFileDate (dt));
  end;
  if account.HasNewGroups then
  begin
    account.HasNewGroups := False;
    NNTPAccounts.SaveToRegistry(nil)
  end
end;

procedure TdlgNewsgroups.btnUnsubscribeClick(Sender: TObject);
var
  groupName : string;
  grp : TSubscribedGroup;
  itm : TListItem;
begin
  if Assigned (lvNewsgroups.Selected) then
  begin
    itm := lvNewsgroups.Selected;
    while Assigned (itm) do
    begin
      groupName := itm.Caption;

      if account.IsSubscribedTo(groupName) then
      begin
        if MessageDlg (Format (rstConfirmUnsubscribe, [groupName]), mtConfirmation, [mbYes, mbNo], 0) = idYes then
        begin
          grp := account.FindSubscribedGroup(groupName);
          if Assigned (grp) then
            SendMessage (Application.MainForm.Handle, WM_UNSUBSCRIBE, Integer (grp), 0);

          if cbShowSubscribedGroupsOnly.Checked then
            ShowGroupList
        end
      end;

      itm := lvNewsgroups.GetNextItem(itm, sdAll, [isSelected])
    end;

    lvNewsgroups.Invalidate;
    lvNewsgroupsChange (nil, nil, TItemChange (0));
    UpdateSubscribedCounts;
  end
end;

procedure TdlgNewsgroups.lvNewsgroupsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  canSubscribe : boolean;
  canUnsubscribe : boolean;
  itm : TListItem;
begin
  canSubscribe := False;
  canUnsubscribe := False;

  if lvNewsgroups.SelCount > 1 then
  begin
    itm := lvNewsgroups.Selected;
    while Assigned (itm) do
    begin
      if itm.Selected then
        if Account.IsSubscribedTo(itm.Caption) then
          canUnsubscribe := True
        else
          canSubscribe := True;

      itm := lvNewsgroups.GetNextItem(itm, sdAll, [isSelected]);
      if canSubscribe and canUnsubscribe then
        break
    end
  end
  else
    if lvNewsgroups.SelCount = 1 then
      if Account.IsSubscribedTo(lvNewsgroups.Selected.Caption) then
        canUnsubscribe := True
      else
        canSubscribe := True;

  btnSubscribe.Enabled := canSubscribe;
  btnUnsubscribe.Enabled := canUnsubscribe;
  btnCopy.Enabled := lvNewsgroups.SelCount > 0;
end;

procedure TdlgNewsgroups.UpdateSubscribedCounts;
begin
  stSubscribedCounts.Caption := Format (fOrigSubscribedCounts, [Account.SubscribedGroupCount, fRawData.Count]);
end;

procedure TdlgNewsgroups.btnCopyClick(Sender: TObject);
var
  st : string;
  itm : TListItem;
begin
  itm := lvNewsgroups.Selected;
  st := '';
  while Assigned (itm) do
  begin
    st := st + #13#10 + itm.Caption;
    itm := lvNewsgroups.GetNextItem(itm, sdAll, [isSelected])
  end;

  if st <> '' then
    Clipboard.AsText := Copy (st, 3, MaxInt);

  lvNewsgroups.SetFocus
end;

end.
