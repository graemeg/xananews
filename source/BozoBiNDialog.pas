unit BozoBiNDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TdlgBozoBin = class(TForm)
    lvBozos: TListView;
    btnAdd: TButton;
    btnProperties: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
  protected
    procedure UpdateActions; override;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgBozoBin: TdlgBozoBin;

implementation

uses unitNNTPServices, unitSearchString, BozoDetailsDialog;

{$R *.dfm}

{ TdlgBozoBin }

procedure TdlgBozoBin.UpdateActions;
begin
  btnProperties.Enabled := Assigned (lvBozos.Selected);
  btnDelete.Enabled := Assigned (lvBozos.Selected);
end;

procedure TdlgBozoBin.FormShow(Sender: TObject);
var
  i : Integer;
  st : string;
begin
  lvBozos.Items.BeginUpdate;
  try
    for i := 0 to NNTPAccounts.Bozos.Count - 1 do
      with lvBozos.Items.Add do
      begin
        st := NNTPAccounts.Bozos [i];
        Caption := SplitString (':', st);
        SubItems.Add(SplitString (':', st));
        SubItems.Add (st)
      end
  finally
    lvBozos.Items.EndUpdate
  end
end;

procedure TdlgBozoBin.btnDeleteClick(Sender: TObject);
begin
  if Assigned (lvBozos.Selected) then
    lvBozos.Selected.Delete
end;

procedure TdlgBozoBin.btnAddClick(Sender: TObject);
var
  dlg : TdlgBozoDetails;
begin
  dlg := TdlgBozoDetails.Create(nil);
  try
    dlg.dpDate.DateTime := Now;
    if dlg.ShowModal = mrOK then
      with lvBozos.Items.Add do
      begin
        Caption := dlg.edName.Text;
        SubItems.Add (dlg.edEMail.Text);
        SubItems.Add(DateToStr (dlg.dpDate.DateTime))
      end
  finally
    dlg.Free
  end
end;

procedure TdlgBozoBin.btnPropertiesClick(Sender: TObject);
var
  dlg : TdlgBozoDetails;
  itm : TListItem;
begin
  itm := lvBozos.Selected;
  if not Assigned (itm) then Exit;

  dlg := TdlgBozoDetails.Create(nil);
  try
    dlg.edName.Text := itm.Caption;
    dlg.edEMail.Text := itm.SubItems [0];
    dlg.dpDate.DateTime := StrToDate (itm.SubItems [1]);

    if dlg.ShowModal = mrOK then
      with itm do
      begin
        Caption := dlg.edName.Text;
        SubItems [0] := dlg.edEMail.Text;
        SubItems [1] := DateToStr (dlg.dpDate.DateTime)
      end
  finally
    dlg.Free
  end
end;

procedure TdlgBozoBin.btnOKClick(Sender: TObject);
var
  i : Integer;
  itm : TListItem;
begin
  NNTPAccounts.Bozos.Clear;
  NNTPAccounts.Bozos.Sorted := False;

  for i := 0 to lvBozos.Items.Count - 1 do
  begin
    itm := lvBozos.Items [i];
    NNTPAccounts.Bozos.Add(StringReplace (itm.Caption, ':', '.', [rfReplaceAll]) + ':' + StringReplace (itm.SubItems [0], ':', '.', [rfReplaceAll]) + ':' + itm.SubItems [1])
  end;

  NNTPAccounts.Bozos.Sorted := True;
  NNTPAccounts.SaveBozoList
end;

end.
