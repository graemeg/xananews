(*======================================================================*
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
 *======================================================================*)

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
    procedure FormShow(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    fBatch: TNNTPBatch;
    function EditBatch (batch : TNNTPBatch) : Boolean;
  protected
    procedure UpdateActions; override;
    { Private declarations }
  public
    property Batch : TNNTPBatch read fBatch;
    { Public declarations }
  end;

var
  dlgBatches: TdlgBatches;

implementation

uses BatchDialog, NewsGlobals;

{$R *.dfm}

{ TdlgBatches }

procedure TdlgBatches.UpdateActions;
var
  selected : Boolean;
begin
  selected := Assigned (lvBatches.Selected);
  btnRemove.Enabled := selected;
  btnProperties.Enabled := selected;
  btnRun.Enabled := selected
end;

procedure TdlgBatches.btnAddClick(Sender: TObject);
var
  batch : TNNTPBatch;
begin
  batch := TNNTPBatch.Create ('');
  try
    if EditBatch (batch) and (batch.BatchName <> '') then
    begin
      NNTPAccounts.AddBatch (batch);
      NNTPAccounts.SaveToRegistry;
      with lvBatches.Items.Add do
      begin
        Caption := batch.BatchName;
        SubItems.Add(batch.BatchTime);
        if batch.RunAtStart then
          SubItems.Add('Yes')
        else
          SubItems.Add('No');
        data := batch
      end
    end
    else
      batch.Free
  except
    batch.Free;
    raise
  end
end;

function TdlgBatches.EditBatch(batch: TNNTPBatch) : Boolean;
var
  dlg : TdlgBatch;
  oldNames : TStringList;
  i, p : Integer;
  Action : TBatchAction;
begin
  Result := False;
  Application.CreateForm(TdlgBatch, dlg);
  try
    dlg.Batch := batch;
    if dlg.ShowModal = mrOK then
    begin
      batch.BatchName := dlg.edBatchName.Text;
      batch.RunEvery := StrToIntDef (dlg.edRunEvery.Text, 0);
      batch.Scheduled := dlg.cbRunEvery.Checked;
      batch.RunAtStart := dlg.cbRunAtStartup.Checked;
      batch.HasRunThisSession := True;

      oldNames := TStringList.Create;
      try
        for i := 0 to batch.ActionCount - 1 do
          oldNames.AddObject (batch.Actions [i].ActionName, batch.Actions [i]);

        for i := 0 to dlg.lvActions.Items.Count - 1 do
          if dlg.lvActions.Items [i].Checked then
          begin
            p := oldNames.IndexOf(dlg.lvActions.Items [i].Caption + '_' + dlg.lvActions.Items [i].SubItems [0]);

            if p >= 0 then
            begin
              Action := TBatchAction (oldNames.Objects [p]);
              oldNames.Delete(p)
            end
            else
            begin
              Action := TBatchAction.Create;
              Action.AccountName := dlg.lvActions.Items [i].Caption;
              Action.GroupName := dlg.lvActions.Items [i].SubItems [0];
              batch.AddAction (Action);
            end;

            Action.ActionText := dlg.lvActions.Items [i].SubItems [1];
          end;

        for i := 0 to oldNames.Count - 1 do
        begin
          p := batch.IndexOf (oldNames [i]);
          if p >= 0 then
            batch.DeleteAction (p)
        end
      finally
        oldNames.Free
      end;

      Result := True
    end
  finally
    dlg.Free
  end
end;

procedure TdlgBatches.FormShow(Sender: TObject);
var
  i : Integer;
begin
  AdjustFormConstraints (self);
  lvBatches.Items.BeginUpdate;
  try
    for i := 0 to NNTPAccounts.BatchesCount - 1 do
      with lvBatches.Items.Add do
      begin
        Caption := NNTPAccounts.Batches[i].BatchName;
        SubItems.Add(NNTPAccounts.Batches[i].BatchTime);
        if NNTPAccounts.Batches [i].RunAtStart then
          SubItems.Add('Yes')
        else
          SubItems.Add ('No');
        Data := NNTPAccounts.Batches [i]
      end;
  finally
    lvBatches.Items.EndUpdate
  end;

  if lvBatches.Items.Count > 0 then
    lvBatches.Items [0].Selected := True
end;

procedure TdlgBatches.btnRemoveClick(Sender: TObject);
var
  batch : TNNTPBatch;
  idx : Integer;
begin
  if Assigned (lvBatches.Selected) then
  begin
    batch := TNNTPBatch (lvBatches.Selected.Data);
    idx := NNTPAccounts.IndexOfBatch(batch);

    if idx >= 0 then
    begin
      NNTPAccounts.DeleteBatch(idx);
      NNTPAccounts.SaveToRegistry;
      lvBatches.DeleteSelected
    end
  end
end;

procedure TdlgBatches.btnPropertiesClick(Sender: TObject);
var
  batch : TNNTPBatch;
begin
  if Assigned (lvBatches.Selected) then
  begin
    batch := TNNTPBatch (lvBatches.Selected.Data);
    if EditBatch (batch) and (batch.BatchName <> '') then
    begin
      if (batch.Scheduled) and (batch.RunEvery <= 0) then
        batch.RunEvery := 10;
      NNTPAccounts.SaveToRegistry;
      lvBatches.Selected.Caption := batch.BatchName;
      lvBatches.Selected.SubItems [0] := batch.BatchTime;
      if batch.RunAtStart then
        lvBatches.Selected.SubItems [1] := 'Yes'
      else
        lvBatches.Selected.SubItems [1] := 'No'
    end
  end
end;

procedure TdlgBatches.btnRunClick(Sender: TObject);
begin
  if Assigned (lvBatches.Selected) then
    fBatch := TNNTPBatch (lvBatches.Selected.Data)
  else
    fBatch := nil;
end;

end.
