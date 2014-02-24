unit PropertyPageBozoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls, ComCtrls, ConTnrs, unitNNTPServices, NewsGlobals;

type
  TPropertyPageBozoData = class (TPropertyPageData)
  private
    fRemoveFromBin : Integer;
    fBozos : TObjectList;
    fDefaultBozoAction : TBozoAction;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
    destructor Destroy; override;
  end;

  TfmPropertyPageBozos = class(TfmPropertyPage)
    lvBozos: TListView;
    btnAdd: TButton;
    btnProperties: TButton;
    btnDelete: TButton;
    Label1: TLabel;
    Bevel2: TBevel;
    Label32: TLabel;
    edRemoveFromBin: TEdit;
    Label34: TLabel;
    Label2: TLabel;
    cbDefaultBozoAction: TComboBox;
    procedure cbDefaultBozoActionChange(Sender: TObject);
    procedure lvBozosData(Sender: TObject; Item: TListItem);
    procedure edRemoveFromBinChange(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    fData : TPropertyPageBozoData;
    function GetSelectedBozo : TBozo;
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
    { Public declarations }
  end;

var
  fmPropertyPageBozos: TfmPropertyPageBozos;

implementation

uses unitNewsReaderOptions, unitSearchString, BozoDetailsDialog;

{$R *.dfm}

{ TfmPropertyPageBozos }

class function TfmPropertyPageBozos.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageBozoData;
end;

procedure TfmPropertyPageBozos.PopulateControls(AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageBozoData;
  cbDefaultBozoAction.ItemIndex := Integer (fData.fDefaultBozoAction);

  lvBozos.Items.BeginUpdate;
  try
    lvBozos.Items.Count := fData.fBozos.Count;
  finally
    lvBozos.Items.EndUpdate
  end;

  edRemoveFromBin.Text := IntToStr (fData.fRemoveFromBin);
end;

function TfmPropertyPageBozos.GetSelectedBozo: TBozo;
var
  idx : Integer;
begin
  idx := lvBozos.ItemIndex;

  if idx >= 0 then
    result := TBozo (fData.fBozos [idx])
  else
    result := Nil
end;

procedure TfmPropertyPageBozos.UpdateActions;
var
  sel : boolean;
begin
  sel := geTSelectedBozo <> Nil;
  btnDelete.Enabled := sel;
  btnProperties.Enabled := sel
end;

{ TPropertyPageBozoData }

function TPropertyPageBozoData.Apply : boolean;
begin
  result := True;
  XNOptions.AutoRemoveFromBin := fRemoveFromBin;
  XNOptions.DefaultBozoAction := fDefaultBozoAction;
  NNTPAccounts.ReplaceBozos(fBozos);
end;

destructor TPropertyPageBozoData.Destroy;
begin
  fBozos.Free;

  inherited;
end;

procedure TPropertyPageBozoData.Initialize;
var
  i : Integer;
  b1, b2 : TBozo;
begin
  fRemoveFromBin := XNOptions.AutoRemoveFromBin;
  fDefaultBozoAction := XNOptions.DefaultBozoAction;

  fBozos := TObjectList.Create;
  for i := 0 to NNTPAccounts.BozoCount - 1 do
  begin
    b1 := NNTPAccounts.Bozo [i];
    b2 := TBozo.CreateNew;
    b2.Assign(b1);
    fBozos.Add(b2)
  end
end;

procedure TfmPropertyPageBozos.edRemoveFromBinChange(Sender: TObject);
begin
  if Populating then Exit;

  fData.fRemoveFromBin := StrToIntDef (edRemoveFromBin.Text, 0);
end;

procedure TfmPropertyPageBozos.btnPropertiesClick(Sender: TObject);
var
  dlg : TdlgBozoDetails;
  bozo : TBozo;
  fgs : TMatchBozoFlags;
begin
  bozo := GetSelectedBozo;
  if not Assigned (bozo) then Exit;

  dlg := TdlgBozoDetails.Create(nil);
  try
    dlg.edName.Text := bozo.Name;
    dlg.edEMail.Text := bozo.EMail;
    dlg.edKeyphrase.Text := bozo.KeyPhrase;
    dlg.cbUseName.Checked := fgName in bozo.Flags;
    dlg.cbUseEMail.Checked := fgEMail in bozo.Flags;
    dlg.cbUseKeyphrase.Checked := fgKeyphrase in bozo.Flags;
    dlg.dpDate.Date := bozo.BozodDate;

    case bozo.Action of
      baIgnore           : dlg.rbIgnore.Checked := True;
      baMarkAsread       : dlg.rbMarkAsRead.Checked := True;
      baIgnoreThread     : dlg.rbIgnoreThread.Checked := True;
      baMarkAsReadThread : dlg.rbMarkAsReadThread.Checked := True
    end;

    if dlg.ShowModal = mrOK then
    begin
      bozo.Name := dlg.edName.Text;
      bozo.EMail := dlg.edEMail.Text;
      bozo.BozodDate :=dlg.dpDate.Date;
      bozo.KeyPhrase := dlg.edKeyphrase.Text;
      fgs := [];
      if dlg.cbUseName.Checked then Include (fgs, fgName);
      if dlg.cbUseEMail.Checked then Include (fgs, fgEMail);
      if dlg.cbUseKeyphrase.Checked then Include (fgs, fgKeyphrase);
      bozo.Flags := fgs;

      if dlg.rbMarkAsRead.Checked then
        bozo.Action := baMarkAsRead
      else
        if dlg.rbIgnoreThread.Checked then
          bozo.Action := baIgnoreThread
        else
          if dlg.rbMarkAsReadThread.Checked then
            bozo.Action := baMarkAsReadThread
          else
            if dlg.rbDontDownload.Checked then
              bozo.Action := baDontDownload
            else
              bozo.Action := baIgnore;

      lvBozos.Invalidate
    end;
  finally
    dlg.Free
  end
end;

procedure TfmPropertyPageBozos.btnDeleteClick(Sender: TObject);
var
  idx : Integer;
begin
  idx := lvBozos.ItemIndex;
  if idx >= 0 then
  begin
    fData.fBozos.Delete(idx);
    lvBozos.Items.Count := fData.fBozos.Count;
    lvBozos.Invalidate
  end
end;

procedure TfmPropertyPageBozos.btnAddClick(Sender: TObject);
var
  dlg : TdlgBozoDetails;
  bozo : TBozo;
begin
  dlg := TdlgBozoDetails.Create(nil);
  try
    dlg.dpDate.Date := Now;
    if dlg.ShowModal = mrOK then
    begin
      bozo := TBozo.Create(dlg.edName.Text,
                           dlg.edEMail.Text,
                           dlg.dpDate.Date, XNOptions.DefaultBozoAction);
      fData.fBozos.Add(bozo);
      lvBozos.Items.Count := fData.fBozos.Count
    end
  finally
    dlg.Free
  end
end;

procedure TfmPropertyPageBozos.lvBozosData(Sender: TObject; Item: TListItem);
var
  idx : Integer;
  bozo : TBozo;
  st : string;
begin
  idx := Item.Index;
  if (idx >= 0) and (idx < fData.fBozos.Count) then
  begin
    bozo := TBozo (fData.fBozos [idx]);
    item.Caption := bozo.Name;
    item.SubItems.Add(bozo.EMail);
    item.SubItems.Add (DateToStr (bozo.BozodDate));

    case bozo.Action of
      baMarkAsRead       : st := 'Mark as Read';
      baIgnoreThread     : st := 'Ignore Thread';
      baMarkAsReadThread : st := 'Mark Thread as Read';
      baDontDownload     : st := 'Don''t Download';
      else
        st := 'Ignore'
    end;

    item.SubItems.Add (st)
  end
end;

procedure TfmPropertyPageBozos.cbDefaultBozoActionChange(Sender: TObject);
begin
  fData.fDefaultBozoAction := TBozoAction (cbDefaultBozoAction.ItemIndex);

end;

end.
