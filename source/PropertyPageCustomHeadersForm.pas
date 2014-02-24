unit PropertyPageCustomHeadersForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls, ComCtrls;

type
  TPropertyPageCustomHeadersData = class (TPropertyPageData)
  private
    fShowCustomHeaders : TStringList;
  protected
    procedure Initialize; override;
  public
    destructor Destroy; override;
    function Apply : boolean; override;
  end;

  TfmPropertyPageCustomHeaders = class(TfmPropertyPage)
    Label23: TLabel;
    lvShowCustomHeaders: TListView;
    btnAddShowCustomHeader: TButton;
    btnRemoveShowCustomHeader: TButton;
    procedure lvShowCustomHeadersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnAddShowCustomHeaderClick(Sender: TObject);
    procedure btnRemoveShowCustomHeaderClick(Sender: TObject);
  private
    fData : TPropertyPageCustomHeadersData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (Data : TPropertyPageData); override;
  end;

var
  fmPropertyPageCustomHeaders: TfmPropertyPageCustomHeaders;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageCustomHeaders }

class function TfmPropertyPageCustomHeaders.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageCustomHeadersData;
end;

procedure TfmPropertyPageCustomHeaders.PopulateControls(
  Data: TPropertyPageData);
var
  i : Integer;
  st : string;
begin
  inherited;

  fData := Data as TPropertyPageCustomHeadersData;

  lvShowCustomHeaders.Clear;
  lvShowCustomHeaders.Items.BeginUpdate;
  try
    for i := 0 to fData.fShowCustomHeaders.Count - 1 do
      with lvShowCustomHeaders.Items.Add do
      begin
        st := fData.fShowCustomHeaders.Names [i];
        Caption := st;
        if fData.fShowCustomHeaders.Values [st] = '1' then
          Checked := True
      end
  finally
    lvShowCustomHeaders.Items.EndUpdate
  end;
end;

procedure TfmPropertyPageCustomHeaders.UpdateData;
var
  i : Integer;
  itm : TListItem;
begin
  if Populating then Exit;
  if csDestroying in ComponentState then Exit;
  fData.fShowCustomHeaders.Clear;

  for i := 0 to lvShowCustomHeaders.Items.Count - 1 do
  begin
    itm := lvShowCustomHeaders.Items [i];
    if itm.Checked then
      fData.fShowCustomHeaders.Values [itm.Caption] := '1'
    else
      fData.fShowCustomHeaders.Values [itm.Caption] := '0'
  end
end;

{ TPropertyPageCustomHeadersData }

function TPropertyPageCustomHeadersData.Apply : boolean;
begin
  result := True;
  XNOptions.ShowCustomHeaders.Assign(fShowCustomHeaders);
end;

destructor TPropertyPageCustomHeadersData.Destroy;
begin
  fShowCustomHeaders.Free;

  inherited;
end;

procedure TPropertyPageCustomHeadersData.Initialize;
begin
  fShowCustomHeaders := TStringList.Create;
  fShowCustomHeaders.Assign(XNOptions.ShowCustomHeaders);
end;

procedure TfmPropertyPageCustomHeaders.btnAddShowCustomHeaderClick(
  Sender: TObject);
var
  st : string;
  item : TListItem;
  i : Integer;
begin
  st := InputBox (Application.Title, 'Show Custom Header', '');

  if st <> '' then
  begin
    st := StringReplace (st, ' ', '', [rfReplaceAll]);
    st := StringReplace (st, ',', '', [rfReplaceAll]);
    st := StringReplace (st, ':', '', [rfReplaceAll])
  end;

  if st <> '' then
  begin
    i := 0;
    while i < lvShowCustomHeaders.Items.Count do
      if CompareText (lvShowCustomHeaders.Items [i].Caption, st) = 0 then
        break
      else
        Inc (i);

    if i = lvShowCustomHeaders.Items.Count then
    begin
      item := lvShowCustomHeaders.Items.Add;
      item.Caption := st;
    end
    else
      item := lvShowCustomHeaders.Items [i];

    item.Checked := True;
    item.MakeVisible(False);
    item.Selected := True
  end;

  UpdateData;


  lvShowCustomHeaders.SetFocus
end;

procedure TfmPropertyPageCustomHeaders.btnRemoveShowCustomHeaderClick(
  Sender: TObject);
begin
  if Assigned (lvShowCustomHeaders.Selected) then
    lvShowCustomHeaders.Selected.Delete;
  lvShowCustomHeaders.SetFocus;
  UpdateData;
end;

procedure TfmPropertyPageCustomHeaders.lvShowCustomHeadersChange(
  Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  inherited;
  if Change = ctState then
    UpdateData
end;

end.
