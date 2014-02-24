unit unitGetMessages1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unitBatches, unitNNTPServices, ComCtrls, ExtCtrls;

type
  TdlgGetMessages1 = class(TForm)
    rbGetAllNewMessages: TRadioButton;
    rbGetAllMessages: TRadioButton;
    rbGetNextMessages: TRadioButton;
    edGetNextMessages: TEdit;
    stNextMessages: TLabel;
    rbGetLastMessages: TRadioButton;
    edGetLastMessages: TEdit;
    stLastMessages: TLabel;
    edAddFrom: TEdit;
    stTo: TLabel;
    edAddTo: TEdit;
    rbAddFrom: TRadioButton;
    cbGetHeadersOnly: TCheckBox;
    rbGetOnlyNewMessages: TRadioButton;
    rbAddSince: TRadioButton;
    dtpSince: TDateTimePicker;
    btnOK: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    btnHelp: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAddFromClick(Sender: TObject);
    procedure rbGetNextMessagesClick(Sender: TObject);
    procedure rbGetLastMessagesClick(Sender: TObject);
    procedure rbGetAllNewMessagesClick(Sender: TObject);
    procedure rbGetAllMessagesClick(Sender: TObject);
    procedure rbAddSinceClick(Sender: TObject);
  private
    fAction: TBatchAction;
    fGroup: TSubscribedGroup;
    fAccount: TNNTPAccount;
    procedure EnableEditBoxes (next, last, fromto, since : boolean);
  public
    procedure UpdateAct;
    property Action : TBatchAction read fAction write fAction;
    property group :TSubscribedGroup read fGroup write fGroup;
    property Account : TNNTPAccount read fAccount write fAccount;
  end;

var
  dlgGetMessages1: TdlgGetMessages1;

implementation

{$R *.dfm}

{ TdlgGetMessages1 }

procedure TdlgGetMessages1.EnableEditBoxes(next, last, fromto, since: boolean);
begin
  edGetNextMessages.Enabled := next;
  edGetLastMessages.Enabled := last;
  edAddFrom.Enabled := fromto;
  edAddTo.Enabled := fromto;
  dtpSince.Enabled := since;
end;

procedure TdlgGetMessages1.FormShow(Sender: TObject);
var
  f, t : Int64;
  act : TbatchAction;
  mCount : Int64;
  dt : TDateTime;
  art : TArticleBase;
begin
  act := Nil;
  dt := 0;
  if Assigned (group) then
  begin
    act := group.NNTPSettings.DefaultAction;
    Caption := Caption + ' - ' + group.Name;
    mCount := group.HighestArticleNo;
    art := group.FindArticleNo(mCount);
    if art <> Nil then
      dt := art.Date;
  end
  else
    if Assigned (account) then
    begin
      act := account.NNTPSettings.DefaultAction;
      Caption := Caption + ' - All groups in ' + account.AccountName
    end;

  if dt = 0 then
    dt := Now - 7;

  dtpSince.Date := dt;

  if Assigned (act) then
    mCount := act.MessageCount
  else
    mCount := 0;

  if mCount = 0 then
    mCount := 300;
  if Assigned (act) then
  begin
    case act.ActionType of
      batAllNew : if (act.ManagementType = bmtDelete) and (act.ManagementOption = bmoAll) then
                    rbGetOnlyNewMessages.Checked := True
                  else
                    rbGetAllNewMessages.Checked := True;
      batAll    : rbGetAllMessages.Checked := True;
      batNextN  : rbGetNextMessages.Checked := True;
      batLastN  : rbGetLastMessages.Checked := True;
    end;

    cbGetHeadersOnly.Checked := act.HeadersOnly;

  end;
  edGetNextMessages.Text := IntToStr (mCount);
  edGetLastMessages.Text := IntToStr (mCount);

  if Assigned (group) then
  begin
    t := group.LowestArticleNo - 1;
    if t < 1 then t := 1;
    f := t - MCount;
    if f < 1 then f := 1;

    edAddFrom.Text := IntToStr (f);
    edAddTo.Text := IntToStr (t);
  end
  else
  begin
    rbAddFrom.Visible := False;
    edAddFrom.Visible := False;
    edAddTo.Visible := False;
    stTo.Visible := False;
  end
end;

procedure TdlgGetMessages1.rbAddFromClick(Sender: TObject);
begin
  EnableEditBoxes (False, False, True, false);
end;

procedure TdlgGetMessages1.UpdateAct;
var
  act : TBatchAction;
  onlyNew : boolean;
begin
  act := Action;
  onlyNew := False;

  if not Assigned (act) then
    if Assigned (group) then
      act := group.NNTPSettings.DefaultAction
    else
      if Assigned (account) then
        act := account.NNTPSettings.DefaultAction;

  Act.MessageCount := 0;
  Act.ManagementCount := 0;
  Act.ActionType := batNone;
  Act.ManagementType := bmtMarkAsRead;
  Act.ManagementOption := bmoNone;
  Act.HeadersOnly := False;

  if rbGetAllNewMessages.Checked then
    Act.ActionType := batAllNew
  else
    if rbGetAllMessages.Checked then
      Act.ActionType := batAll
    else
      if rbGetOnlyNewMessages.Checked then
      begin
        onlyNew := True;
        Act.ActionType := batAllNew
      end
      else
      if rbGetNextMessages.Checked then
      begin
        Act.ActionType := batNextN;
        Act.MessageCount := StrToIntDef (edGetNextMessages.Text, 300)
      end
      else
        if rbGetLastMessages.Checked then
        begin
          Act.ActionType := batLastN;
          Act.MessageCount := StrToIntDef (edGetLastMessages.Text, 300)
        end
        else
          if rbAddSince.Checked then
          begin
            Act.ActionType := batSince;
            Act.Since := Trunc (dtpSince.DateTime);
          end;

  if onlyNew or (Act.ActionType = batAll) or (Act.ActionType = batLastN) or (Act.ActionType = batSince) then
  begin
    Act.ManagementType := bmtDelete;
    Act.ManagementOption := bmoAll
  end;

  Act.HeadersOnly := cbGetHeadersOnly.Checked;
end;

procedure TdlgGetMessages1.rbGetNextMessagesClick(Sender: TObject);
begin
  EnableEditBoxes (True, False, False, False);
end;

procedure TdlgGetMessages1.rbGetLastMessagesClick(Sender: TObject);
begin
  EnableEditBoxes (False, True, False, False);
end;

procedure TdlgGetMessages1.rbGetAllNewMessagesClick(Sender: TObject);
begin
  EnableEditBoxes (false, false, false, False);
end;

procedure TdlgGetMessages1.rbGetAllMessagesClick(Sender: TObject);
begin
  EnableEditBoxes (false, false, false, False);
end;

procedure TdlgGetMessages1.rbAddSinceClick(Sender: TObject);
begin
  EnableEditBoxes (False, False, False, True);
end;

procedure TdlgGetMessages1.btnHelpClick(Sender: TObject);
begin
  Application.HelpKeyword(HelpKeyword)
end;

end.
