unit PropertyPageFiltersForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, PropertyPageForm, PropertyPageDefaultsForm, VirtualTrees, StdCtrls, ExtCtrls, unitNNTPFilters,
  unitNNTPServices;

type
  TPropertyPageFiltersData = class (TPropertyPageData)
  private
    fCtnr : TFiltersCtnr;
    fDisplayCtnr : TFiltersCtnr;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
    destructor Destroy; override;
  end;

  TfmPropertyPageFilters = class(TfmPropertyPageDefaults)
    btnUpdate: TButton;
    btnDelete: TButton;
    btnAdd: TButton;
    vstFilters: TVirtualStringTree;
    procedure btnAddClick(Sender: TObject);
    procedure vstFiltersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstFiltersGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstFiltersAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure vstFiltersClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fData : TPropertyPageFiltersData;
    procedure Reinit_vstFilters;
    procedure Refresh_vstFilters;
    function GetSelectedFilter : TNNTPFilter;
    function GetNodeFilter (node : PVirtualNode) : TNNTPFilter;
  protected
    function CanRestoreParentSettings : boolean; override;
    procedure RestoreParentSettings; override;
    procedure UpdateActions; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageFilters: TfmPropertyPageFilters;

implementation

uses MainForm, FilterDetailsDialog;

{$R *.dfm}

type
  PObject = ^TObject;

procedure TfmPropertyPageFilters.btnAddClick(Sender: TObject);
var
  dlg : TdlgFilterDetails;
  filter : TNNTPFilter;
begin
  filter := Nil;
  dlg := TdlgFilterDetails.Create(nil);
  try
    filter := TNNTPFilter.Create;
    dlg.Filter := filter;

    if dlg.ShowModal = mrOK then
    begin
      filter.Save;
      AllFilters.AddFilter(filter);
      filter := Nil; // Don't delete in 'finally'
      Reinit_vstFilters;
    end
  finally
    filter.Free;
    dlg.Free
  end
end;

function TfmPropertyPageFilters.GetNodeFilter(
  node: PVirtualNode): TNNTPFilter;
var
  data : PObject;
begin
  result := Nil;
  if Assigned (node) then
  begin
    data := PObject (vstFilters.GetNodeData(node));

    if Assigned (data) and Assigned (data^) then
      result := TNNTPFilter (data^)
  end
end;

function TfmPropertyPageFilters.GetSelectedFilter: TNNTPFilter;
begin
  result := GetNodeFilter (vstFilters.GetFirstSelected);
end;

procedure TfmPropertyPageFilters.Refresh_vstFilters;
begin
  vstFilters.BeginUpdate;
  try
    vstFilters.ReinitNode (nil, True)
  finally
    vstFilters.EndUpdate
  end
end;

procedure TfmPropertyPageFilters.Reinit_vstFilters;
begin
  vstFilters.BeginUpdate;
  try
    vstFilters.RootNodeCount := AllFilters.Count;
    Refresh_vstFilters;
  finally
    vstFilters.EndUpdate
  end
end;

procedure TfmPropertyPageFilters.vstFiltersInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  p : PObject;
begin
  if ParentNode = Nil then
  begin
    p := vstFilters.GetNodeData(Node);
    if Assigned (p) then
      p^ := AllFilters [node.Index]
  end
end;

procedure TfmPropertyPageFilters.vstFiltersGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  filter : TNNTPFilter;
begin
  filter := GetNodeFilter (Node);
  if Assigned (filter) then
  case Column of
    0 : CellText := filter.Name;
    1 : CellText := filter.Text;
    else
      CellText := ''
  end
end;

procedure TfmPropertyPageFilters.vstFiltersAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  filter : TNNTPFilter;

  procedure PaintImage (checked : boolean);
  var
    state : DWORD;
    r : TRect;
  begin
    r := CellRect;
    InflateRect (r, -2, -2);
    state := DFCS_BUTTONCHECK;
    if checked then
      state := state or DFCS_CHECKED;
    DrawFrameControl (TargetCanvas.Handle, r, DFC_BUTTON, state);
  end;

begin
  filter := GetNodeFilter (Node);
  if Assigned (filter) then
  case Column of
    2 : PaintImage (fData.fCtnr.FilterEnabled(filter));
    3 : PaintImage (fData.fDisplayCtnr.FilterEnabled(filter))
  end
end;

procedure TfmPropertyPageFilters.vstFiltersClick(Sender: TObject);
var
  pt : TPoint;
  hitInfo : THitInfo;
  r : TRect;
  w : Integer;
  filter : TNNTPFilter;
  ctnr : TFiltersCtnr;
begin
  pt := Mouse.CursorPos;
  pt := vstFilters.ScreenToClient (pt);

  vstFilters.GetHitTestInfoAt(pt.X, pt.Y, True, hitInfo);

  if hitInfo.HitColumn in [2, 3] then
  begin
    filter := GetNodeFilter (hitInfo.HitNode);
    if Assigned (filter) then
    begin
      r := vstFilters.GetDisplayRect(hitInfo.HitNode, hitInfo.HitColumn, False);
      w := r.Bottom - r.Top;
      r.Left := r.Left + (r.right - r.left) div 2 - w div 2;
      r.Right := r.Left + w;
      InflateRect (r, -2, -2);

      if hitInfo.hitColumn = 2 then
        ctnr := fData.fCtnr
      else
        ctnr := fData.fDisplayCtnr;

      if PtInRect (r, pt) then
      begin
        ctnr.EnableFilter(filter, not ctnr.FilterEnabled(filter));
        vstFilters.Invalidate
      end
    end
  end
end;

{ TPropertyPageFiltersData }

function TPropertyPageFiltersData.Apply : boolean;
var
  obj : TObject;
  accts : TNNTPAccounts;
  cont : TArticleContainer;
  acct : TNNTPAccount;
begin
  result := True;
  obj := TObject (Param);
  if obj is TNNTPAccounts then
  begin
    accts := TNNTPAccounts (obj);
    accts.FiltersCtnr.AssignFilters(fCtnr);
    accts.DisplayFiltersCtnr.AssignFilters(fDisplayCtnr);
  end
  else
    if obj is TArticleContainer then
    begin
      cont := TArticleContainer (obj);
      cont.FiltersCtnr.AssignFilters(fCtnr);
      cont.DisplayFiltersCtnr.AssignFilters(fDisplayCtnr);
    end
    else
    if obj is TNNTPAccount then
    begin
      acct := TNNTPAccount (obj);
      acct.FiltersCtnr.AssignFilters(fCtnr);
      acct.DisplayFiltersCtnr.AssignFilters(fDisplayCtnr);
    end
    else
      Raise Exception.Create ('Object type does not support filters');
end;

destructor TPropertyPageFiltersData.Destroy;
begin
  fCtnr.Free;
  fDisplayCtnr.Free;

  inherited;
end;

procedure TPropertyPageFiltersData.Initialize;
var
  obj : TObject;
  accts : TNNTPAccounts;
  cont : TArticleContainer;
  acct : TNNTPAccount;
begin
  fCtnr := TFiltersCtnr.Create(nil, nil);
  fDisplayCtnr := TFiltersCtnr.Create(nil, nil);
  obj := TObject (Param);
  if obj is TNNTPAccounts then
  begin
    accts := TNNTPAccounts (obj);
    fCtnr.AssignFilters (accts.FiltersCtnr);
    fDisplayCtnr.AssignFilters (accts.DisplayFiltersCtnr);
  end
  else
    if obj is TArticleContainer then
    begin
      cont := TArticleContainer (obj);
      fCtnr.AssignFilters (cont.FiltersCtnr);
      fDisplayCtnr.AssignFilters (cont.DisplayFiltersCtnr);
    end
    else
    if obj is TNNTPAccount then
    begin
      acct := TNNTPAccount (obj);
      fCtnr.AssignFilters (acct.FiltersCtnr);
      fDisplayCtnr.AssignFilters (acct.DisplayFiltersCtnr)
    end
    else
      Raise Exception.Create ('Object type does not support filters');
end;

procedure TfmPropertyPageFilters.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageFiltersData;
  Reinit_vstFilters
end;

class function TfmPropertyPageFilters.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageFiltersData;
end;

procedure TfmPropertyPageFilters.btnDeleteClick(Sender: TObject);
var
  filter : TNNTPFilter;
  msg : string;
begin
  filter := GetSelectedFilter;
  if Assigned (filter) then
  begin

    msg := Format ('Are you sure you want to delete filter ''%s'' from all newsgroups and accounts', [filter.Name]);
    if MessageBox (0, PChar (msg), PChar (Application.Title), MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION) <> IDYES then
      exit;

    AllFilters.DeleteFilter(filter);
    AllFilters.Save;
    Reinit_vstFilters
  end
end;

procedure TfmPropertyPageFilters.btnUpdateClick(Sender: TObject);
var
  dlg : TdlgFilterDetails;
  filter : TNNTPFilter;
begin
  filter := GetSelectedFilter;
  if Assigned (filter) then
  begin

    dlg := TdlgFilterDetails.Create(nil);
    try
      dlg.Filter := filter;
      if dlg.ShowModal = mrOK then
      begin
        AllFilters.Save;
        Reinit_vstFilters;
      end
    finally
      dlg.Free
    end
  end
end;

function TfmPropertyPageFilters.CanRestoreParentSettings: boolean;
begin
  result := Assigned (fData.fCtnr.Parent);
end;

procedure TfmPropertyPageFilters.FormCreate(Sender: TObject);
begin
  inherited;
  vstFilters.NodeDataSize := SizeOf(Pointer);
end;

procedure TfmPropertyPageFilters.RestoreParentSettings;
begin
  fData.fCtnr.Clear;
  fData.fDisplayCtnr.Clear;
  Reinit_vstFilters
end;

procedure TfmPropertyPageFilters.UpdateActions;
var
  sel :boolean;
begin
  sel := GetSelectedFilter <> Nil;
  btnUpdate.Enabled := sel;
  btnDelete.Enabled := sel
end;

end.
