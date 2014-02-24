(*======================================================================*
 | MessagesDialog unit for NewsReader3                                  |
 |                                                                      |
 | Display a dialog displaying message retriavel options                |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit MessagesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unitNNTPServices, ExtCtrls, unitBatches, ComCtrls;

type
  TdlgGetMessages = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    rbMarkAsRead: TRadioButton;
    rbDelete: TRadioButton;
    Panel3: TPanel;
    Bevel1: TBevel;
    stOld: TLabel;
    rbAllMessages: TRadioButton;
    rbMessagesMoreThan: TRadioButton;
    edMoreThan: TEdit;
    udMoreThan: TUpDown;
    cbDaysWeeksMonths: TComboBox;
    Panel4: TPanel;
    stNextMessages: TLabel;
    stLastMessages: TLabel;
    rbGetAllNewMessages: TRadioButton;
    rbGetNextMessages: TRadioButton;
    edGetNextMessages: TEdit;
    rbGetAllMessages: TRadioButton;
    rbGetLastMessages: TRadioButton;
    edGetLastMessages: TEdit;
    cbNext: TCheckBox;
    cbFirst: TCheckBox;
    cbGetHeadersOnly: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure rbAddFromClick(Sender: TObject);
    procedure cbFirstClick(Sender: TObject);
    procedure cbNextClick(Sender: TObject);
    procedure rbGetNextMessagesClick(Sender: TObject);
    procedure rbGetLastMessagesClick(Sender: TObject);
    procedure rbGetAllNewMessagesClick(Sender: TObject);
    procedure rbGetAllMessagesClick(Sender: TObject);
  private
    fGroup: TSubscribedGroup;
    fShowRange: Boolean;
    fAccount: TNNTPAccount;
    fAction: TBatchAction;
    procedure EnableEditBoxes(next, last: Boolean);
    procedure EnableFirstSection(enable: Boolean);
    procedure EnableNextSection(enable: Boolean);
  public
    procedure UpdateAct;
    property Group:TSubscribedGroup read fGroup write fGroup;
    property Action: TBatchAction read fAction write fAction;
    property ShowRange: Boolean read fShowRange write fShowRange;
    property Account: TNNTPAccount read fAccount write fAccount;
    { Public declarations }
  end;

var
  dlgGetMessages: TdlgGetMessages;

function DoDefaultActionDialog(const defaultAction : string) : string;

implementation

uses
  unitNewsreaderOptions;

{$R *.dfm}

procedure TdlgGetMessages.EnableEditBoxes(next, last : Boolean);
begin
  edGetNextMessages.Enabled := next;
  edGetLastMessages.Enabled := last;
end;

procedure TdlgGetMessages.EnableFirstSection(enable : Boolean);
begin
  rbMarkAsRead.Enabled := enable;
  rbDelete.Enabled := enable;
  rbAllMessages.Enabled := enable;
  rbMessagesMoreThan.Enabled := enable;
  edMoreThan.Enabled := enable;
  udMoreThan.Enabled := enable;
  cbDaysWeeksMonths.Enabled := enable;
  stOld.Enabled := enable;
end;

procedure TdlgGetMessages.EnableNextSection(enable : Boolean);
begin
  rbGetAllNewMessages.Enabled := enable;
  rbGetAllMessages.Enabled := enable;
  rbGetNextMessages.Enabled := enable;
  rbGetLastMessages.Enabled := enable;
  cbGetHeadersOnly.Enabled := enable;
  stNextMessages.Enabled := enable;
  stLastMessages.Enabled := enable;

  if enable then
    EnableEditBoxes(rbGetNextMessages.Checked, rbGetLastMessages.Checked)
  else
    EnableEditBoxes(False, False);
end;

procedure TdlgGetMessages.FormShow(Sender: TObject);
var
  act: TbatchAction;
begin
  act := Action;
  if not Assigned(act) then
    if Assigned(group) then
    begin
      act := group.NNTPSettings.DefaultAction;
      Caption := Caption + ' - ' + group.Name;
    end
    else
      if Assigned(account) then
      begin
        act := account.NNTPSettings.DefaultAction;
        Caption := Caption + ' - All groups in ' + account.AccountName;
      end;

  if Assigned(act) then
  begin
    cbFirst.Checked := act.ManagementOption <> bmoNone;
    EnableFirstSection(cbFirst.Checked);
    case act.ManagementType of
      bmtMarkAsRead : rbMarkAsRead.Checked := True;
      bmtDelete     : rbDelete.Checked := True;
    end;

    if (act.ManagementOption = bmoAll) or (act.ManagementOption = bmoNone) then
      rbAllMessages.Checked := True
    else
      rbMessagesMoreThan.Checked := True;

    edMoreThan.Text := IntToStr(act.ManagementCount);

    if act.ManagementOption in [bmoDay, bmoWeek, bmoMonth] then
      cbDaysWeeksMonths.ItemIndex := Integer(act.ManagementOption) - Integer(bmoDay)
    else
      cbDaysWeeksMonths.ItemIndex := 0;

    cbNext.Checked := (act.ActionType <> batNone);
    EnableNextSection(cbNext.Checked);
    case act.ActionType of
      batAllNew : rbGetAllNewMessages.Checked := True;
      batAll    : rbGetAllMessages.Checked := True;
      batNextN  : rbGetNextMessages.Checked := True;
      batLastN  : rbGetLastMessages.Checked := True;
    end;

    cbGetHeadersOnly.Checked := act.HeadersOnly;

    edGetNextMessages.Text := IntToStr(act.MessageCount);
    edGetLastMessages.Text := IntToStr(act.MessageCount);
  end;
end;

procedure TdlgGetMessages.rbAddFromClick(Sender: TObject);
begin
  EnableEditBoxes(False, False);
end;

procedure TdlgGetMessages.UpdateAct;
var
  act: TBatchAction;
begin
  act := Action;

  if not Assigned(act) then
    if Assigned(group) then
      act := group.NNTPSettings.DefaultAction
    else
      if Assigned(account) then
        act := account.NNTPSettings.DefaultAction;

  Act.MessageCount := 0;
  Act.ManagementCount := 0;
  Act.ActionType := batNone;
  Act.ManagementType := bmtMarkAsRead;
  Act.ManagementOption := bmoNone;
  Act.HeadersOnly := False;

  if cbFirst.Checked then
  begin
    if rbAllMessages.Checked then
      Act.ManagementOption := bmoAll
    else
      Act.ManagementOption := TBatchManagementOption(cbDaysWeeksMonths.ItemIndex + Integer(bmoDay));

    if Act.ManagementOption in [bmoDay, bmoMonth, bmoWeek] then
      Act.ManagementCount := udMoreThan.Position;

    if rbDelete.Checked then
      Act.ManagementType := bmtDelete;
  end;

  if cbNext.Checked then
  begin
    if rbGetAllNewMessages.Checked then
      Act.ActionType := batAllNew
    else
      if rbGetAllMessages.Checked then
        Act.ActionType := batAll
      else
        if rbGetNextMessages.Checked then
        begin
          Act.ActionType := batNextN;
          Act.MessageCount := StrToIntDef(edGetNextMessages.Text, 300);
        end
        else
          if rbGetLastMessages.Checked then
          begin
            Act.ActionType := batLastN;
            Act.MessageCount := StrToIntDef(edGetLastMessages.Text, 300);
          end;
    Act.HeadersOnly := cbGetHeadersOnly.Checked;

    if ((Act.ActionType = batAll) or (Act.ActionType = batLastN)) then
    begin
      Act.ManagementType := bmtDelete;
      Act.ManagementOption := bmoAll;
    end;
  end;
end;

function DoDefaultActionDialog(const defaultAction: string): string;
var
  dlg : TdlgGetMessages;
  action : TBatchAction;
begin
  Result := defaultAction;
  dlg := nil;
  Action := TBatchAction.Create;
  try
    Action.ActionText := Result;

    Application.CreateForm(TdlgGetMessages, dlg);
    dlg.ShowRange := False;
    dlg.Action := Action;

    if dlg.ShowModal = mrOK then
    begin
      dlg.UpdateAct;
      Result := Action.ActionText;
    end;
  finally
    action.Free;
  end;
end;

procedure TdlgGetMessages.cbFirstClick(Sender: TObject);
begin
  EnableFirstSection(cbFirst.Checked);
end;

procedure TdlgGetMessages.cbNextClick(Sender: TObject);
begin
  EnableNextSection(cbNext.Checked);
end;

procedure TdlgGetMessages.rbGetNextMessagesClick(Sender: TObject);
begin
  EnableEditBoxes(True, False);
end;

procedure TdlgGetMessages.rbGetLastMessagesClick(Sender: TObject);
begin
  EnableEditBoxes(False, True);
end;

procedure TdlgGetMessages.rbGetAllNewMessagesClick(Sender: TObject);
begin
  EnableEditBoxes(False, False);
end;

procedure TdlgGetMessages.rbGetAllMessagesClick(Sender: TObject);
begin
  EnableEditBoxes(False, False);
end;

end.
