unit ExportCompressedMessagesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NewsGlobals, StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TdlgExportCompressedMessages = class(TForm)
    lvActions: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    cbExportToFile: TComboBox;
    cbExportSettings: TCheckBox;
    btnSelectFile: TButton;
    dlgExportCompressed: TSaveDialog;
    pnlStatus: TPanel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    PopupMenu1: TPopupMenu;
    mnuSelAll: TMenuItem;
    mnuClearAll: TMenuItem;
    N1: TMenuItem;
    mnuSelAccount: TMenuItem;
    mnuClearAccount: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure mnuSelAllClick(Sender: TObject);
    procedure mnuClearAllClick(Sender: TObject);
    procedure mnuSelAccountClick(Sender: TObject);
    procedure mnuClearAccountClick(Sender: TObject);
  private
    procedure WmSetup (var msg : TMessage); message WM_SETUP;
    procedure DoOnProgress (Sender : TObject; n : Integer);
  protected
    procedure UpdateActions; override;
  public
    { Public declarations }
  end;

var
  dlgExportCompressedMessages: TdlgExportCompressedMessages;

implementation

uses unitNNTPServices, unitXanaExporter;

{$R *.dfm}

procedure TdlgExportCompressedMessages.FormShow(Sender: TObject);
begin
  PostMessage (handle, WM_SETUP, 0, 0);
end;

procedure TdlgExportCompressedMessages.WmSetup(var msg: TMessage);
var
  i, j : Integer;
  account : TNNTPAccount;
  group : TSubscribedGroup;
  acName : string;
begin
  lvActions.Clear;
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

procedure TdlgExportCompressedMessages.btnOKClick(Sender: TObject);
var
  exporter : TXanaExporter;
  i : Integer;
  groups : TStrings;
  group : TSubscribedGroup;
begin
  exporter := Nil;
  groups := TStringList.Create;
  try
    for i := 0 to lvActions.Items.Count - 1 do
      if lvActions.Items [i].Checked then
      begin
        group := TSubscribedGroup (lvActions.Items [i].Data);
        groups.Add(group.Owner.AccountName + ':' + group.Name)
      end;

    btnCancel.Enabled := False;
    exporter := TXanaExporter.Create;
    exporter.OnProgress := DoOnProgress;
    pnlStatus.Visible := True;
    ProgressBar1.Max := groups.Count;
    exporter.Export (cbExportToFile.Text, groups, cbExportSettings.Checked)
  finally
    groups.Free;
    exporter.Free;
    progressBar1.Position := ProgressBar1.Max;
    Application.ProcessMessages;
    ShowMessage ('Archive ' + ExtractFileName (cbExportToFile.Text) + ' created OK');
    Close
  end
end;

procedure TdlgExportCompressedMessages.btnSelectFileClick(Sender: TObject);
begin
  if dlgExportCompressed.Execute then
    cbExportToFile.Text := dlgExportCompressed.FileName
end;

procedure TdlgExportCompressedMessages.UpdateActions;
var
  i : Integer;
  sel : boolean;
begin
  sel := cbExportSettings.Checked;
  if not sel then
    for i := 0 to lvActions.Items.Count - 1 do
      if lvActions.Items [i].Checked then
      begin
        sel := True;
        break
      end;

  btnOK.Enabled := sel  and (cbExportToFile.Text <> '');

  sel := Assigned (lvActions.Selected);
  mnuSelAccount.Enabled := sel;
  mnuClearAccount.Enabled := sel;

end;

procedure TdlgExportCompressedMessages.DoOnProgress(Sender: TObject;
  n: Integer);
begin
  ProgressBar1.Position := n;
  ProgressBar1.Repaint
end;

procedure TdlgExportCompressedMessages.mnuSelAllClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to lvActions.Items.Count - 1 do
    lvActions.Items [i].Checked := True
end;

procedure TdlgExportCompressedMessages.mnuClearAllClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to lvActions.Items.Count - 1 do
    lvActions.Items [i].Checked := False
end;

procedure TdlgExportCompressedMessages.mnuSelAccountClick(Sender: TObject);
var
  acc : TNNTPAccount;
  i :Integer;
begin
  if not Assigned (lvActions.Selected) then Exit;

  acc := TSubscribedGroup (lvActions.Selected.Data).Owner;

  for i := 0 to lvActions.Items.Count - 1 do
    if TSubscribedGroup (lvActions.Items [i].Data).Owner = acc then
      lvActions.Items [i].Checked := True
end;

procedure TdlgExportCompressedMessages.mnuClearAccountClick(
  Sender: TObject);
var
  acc : TNNTPAccount;
  i :Integer;
begin
  if not Assigned (lvActions.Selected) then Exit;

  acc := TSubscribedGroup (lvActions.Selected.Data).Owner;

  for i := 0 to lvActions.Items.Count - 1 do
    if TSubscribedGroup (lvActions.Items [i].Data).Owner = acc then
      lvActions.Items [i].Checked := False
end;

end.


