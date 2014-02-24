(* ======================================================================*
  | BatchesDialog unit for NewsReader3                                   |
  |                                                                      |
  | Allow the user to set up and run batches of message retrieval        |
  | operations.                                                          |
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
  *====================================================================== *)

unit BatchesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, unitNNTPServices, unitBatches;

type
  TdlgBatches = class(TForm)
    lvBatches: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnProperties: TButton;
    btnRun: TButton;
    btnClose: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fBatch: TNNTPBatch;
    function EditBatch(batch: TNNTPBatch): Boolean;
  protected
    procedure InitListView;
    procedure UpdateActions; override;
  public
    property batch: TNNTPBatch read fBatch;
  end;

var
  dlgBatches: TdlgBatches;

implementation

uses
  BatchDialog, NewsGlobals;

{$R *.dfm}

{ TdlgBatches }

function TdlgBatches.EditBatch(batch: TNNTPBatch): Boolean;
var
  dlg: TdlgBatch;
  oldNames: TStringList;
  i, p: Integer;
  Action: TBatchAction;
begin
  Result := False;
  Application.CreateForm(TdlgBatch, dlg);
  try
    dlg.batch := batch;
    if dlg.ShowModal = mrOK then
    begin
      batch.BatchName := dlg.edBatchName.Text;
      batch.RunEvery := StrToIntDef(dlg.edRunEvery.Text, 0);
      batch.Scheduled := dlg.cbRunEvery.Checked;
      batch.RunAtStart := dlg.cbRunAtStartup.Checked;
      batch.HasRunThisSession := True;

      oldNames := TStringList.Create;
      try
        for i := 0 to batch.ActionCount - 1 do
          oldNames.AddObject(batch.Actions[i].ActionName, batch.Actions[i]);

        for i := 0 to dlg.lvActions.Items.Count - 1 do
          if dlg.lvActions.Items[i].Checked then
          begin
            p := oldNames.IndexOf(dlg.lvActions.Items[i].Caption + '_' + dlg.lvActions.Items[i].SubItems[0]);

            if p >= 0 then
            begin
              Action := TBatchAction(oldNames.Objects[p]);
              oldNames.Delete(p);
            end
            else
            begin
              Action := TBatchAction.Create;
              Action.AccountName := dlg.lvActions.Items[i].Caption;
              Action.GroupName := dlg.lvActions.Items[i].SubItems[0];
              batch.AddAction(Action);
            end;

            Action.ActionText := dlg.lvActions.Items[i].SubItems[1];
          end;

        for i := 0 to oldNames.Count - 1 do
        begin
          p := batch.IndexOf(oldNames[i]);
          if p >= 0 then
            batch.DeleteAction(p);
        end;
      finally
        oldNames.Free;
      end;

      Result := True;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TdlgBatches.InitListView;
var
  i: Integer;
begin
  lvBatches.Clear;
  lvBatches.Items.BeginUpdate;
  try
    for i := 0 to NNTPAccounts.BatchesCount - 1 do
      with lvBatches.Items.Add do
      begin
        Caption := NNTPAccounts.Batches[i].BatchName;
        SubItems.Add(NNTPAccounts.Batches[i].BatchTime);
        if NNTPAccounts.Batches[i].RunAtStart then
          SubItems.Add('Yes')
        else
          SubItems.Add('No');
        data := NNTPAccounts.Batches[i];
      end;
  finally
    lvBatches.Items.EndUpdate;
  end;
end;

procedure TdlgBatches.UpdateActions;
var
  selected: Boolean;
begin
  selected := Assigned(lvBatches.selected);
  btnRemove.Enabled := selected;
  btnProperties.Enabled := selected;
  btnRun.Enabled := selected;
end;


procedure TdlgBatches.btnAddClick(Sender: TObject);
var
  batch: TNNTPBatch;
  idx: Integer;
begin
  batch := TNNTPBatch.Create('');
  try
    if EditBatch(batch) and (batch.BatchName <> '') then
    begin
      NNTPAccounts.AddBatch(batch);
      NNTPAccounts.SaveToRegistry;

      InitListView;

      idx := NNTPAccounts.IndexOfBatch(batch);
      if idx <> -1 then
      begin
        lvBatches.Items[idx].Selected := True;
        lvBatches.Items[idx].MakeVisible(False);
      end;
    end
    else
      batch.Free;
  except
    batch.Free;
    raise;
  end;
end;

procedure TdlgBatches.btnPropertiesClick(Sender: TObject);
var
  batch: TNNTPBatch;
begin
  if Assigned(lvBatches.selected) then
  begin
    batch := TNNTPBatch(lvBatches.selected.data);
    if EditBatch(batch) and (batch.BatchName <> '') then
    begin
      if (batch.Scheduled) and (batch.RunEvery <= 0) then
        batch.RunEvery := 10;
      NNTPAccounts.SaveToRegistry;
      lvBatches.selected.Caption := batch.BatchName;
      lvBatches.selected.SubItems[0] := batch.BatchTime;
      if batch.RunAtStart then
        lvBatches.selected.SubItems[1] := 'Yes'
      else
        lvBatches.selected.SubItems[1] := 'No';
    end;
  end;
end;

procedure TdlgBatches.btnRemoveClick(Sender: TObject);
var
  batch: TNNTPBatch;
  idx: Integer;
begin
  if Assigned(lvBatches.selected) then
  begin
    batch := TNNTPBatch(lvBatches.selected.data);
    idx := NNTPAccounts.IndexOfBatch(batch);

    if idx >= 0 then
    begin
      NNTPAccounts.DeleteBatch(idx);
      NNTPAccounts.SaveToRegistry;
      lvBatches.DeleteSelected;
    end;
  end;
end;

procedure TdlgBatches.btnRunClick(Sender: TObject);
begin
  if Assigned(lvBatches.Selected) then
    fBatch := TNNTPBatch(lvBatches.Selected.Data)
  else
    fBatch := nil;
end;

procedure TdlgBatches.FormShow(Sender: TObject);
begin
  AdjustFormConstraints(Self);

  InitListView;

  if lvBatches.Items.Count > 0 then
    lvBatches.Items[0].selected := True;
end;

end.
