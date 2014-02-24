unit PropertyPageShortcutsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, Menus, VirtualTrees, ExtCtrls, ConTnrs, ActnList;

type
  TActionCategories = class;

  TActionCategory = class(TObjectList)
  private
    fCategoryName: string;
    fOwner: TActionCategories;
    function GetAction(idx: Integer): TCustomAction;
    function GetCategoryHasDuplicates: Boolean;
  public
    constructor Create(AOwner: TActionCategories; const ACategoryName: string);
    property Action[idx: Integer]: TCustomAction read GetAction;
    property CategoryName: string read fCategoryName;
    property CategoryHasDuplicates: Boolean read GetCategoryHasDuplicates;
  end;

  TActionCategories = class(TObjectList)
  private
    fHasDuplicates: Boolean;
    function GetCategory(idx: Integer): TActionCategory;
  public
    procedure AddAction(action: TCustomAction);
    function FindCategory(const name: string): TActionCategory;
    property Category[idx: Integer]: TActionCategory read GetCategory;
    procedure ScanForDuplicates;
    property HasDuplicates: Boolean read fHasDuplicates;
  end;

  TPropertyPageShortcutsData = class(TPropertyPageData)
  private
    fActionCategories: TActionCategories;
    fDefaultActions: TObjectList;
    fModified: Boolean;
  protected
    procedure Initialize; override;
  public
    procedure Cancel; override;
    destructor Destroy; override;
  end;

  TfmPropertyPageShortcuts = class(TfmPropertyPage)
    vstActions: TVirtualStringTree;
    btnEdit: TButton;
    btnRestoreDefaults: TButton;
    procedure vstActionsAfterItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
    procedure vstActionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstActionsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstActionsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstActionsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstActionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure vstActionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure btnEditClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fData: TPropertyPageShortcutsData;
    function GetNodeAction(node: PVirtualNode): TCustomAction;
    function GetSelectedAction: TCustomAction;
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls(AData: TPropertyPageData); override;
  end;

var
  fmPropertyPageShortcuts: TfmPropertyPageShortcuts;

implementation

uses
  {$if CompilerVersion >= 23.0} // 23.0 = Delphi XE2
    System.UITypes,
  {$ifend}
  MainForm, NewsGlobals;

{$R *.dfm}

type
  TComboEditLink = class;

  TVTComboBox = class(TCustomComboBox)
  private
    FLink: TComboEditLink;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Link: TComboEditLink); reintroduce;
  end;

  TComboEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FCombo: TVTComboBox;               // a normal custom combobox control
    FTree: TVirtualStringTree; // a back reference to the tree calling
    FNode: PVirtualNode;             // the node to be edited
    FColumn: TColumnIndex;           // the column of the node
    FAlignment: TAlignment;
    FTextBounds: TRect;              // smallest rectangle around the text
    FStopping: Boolean;              // set to True when the edit link requests stopping the edit action
    fEditObjectInstance: pointer;
    fOldEditWindowProc: TFNWndProc;

    procedure EditWindowProc(var msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

{ TPropertyPageShortcutsData }

procedure TPropertyPageShortcutsData.Cancel;
var
  i: Integer;
  def: TActionDefault;
begin
  for i := 0 to fDefaultActions.Count - 1 do
  begin
    def := TActionDefault(fDefaultActions[i]);
    def.Action.ShortCut := def.Shortcut;
  end;
end;

destructor TPropertyPageShortcutsData.Destroy;
begin
  fActionCategories.Free;
  fDefaultActions.Free;
  inherited Destroy;
end;

procedure TPropertyPageShortcutsData.Initialize;
var
  i: Integer;
  actionList: TActionList;
  def: TActionDefault;
begin
  fActionCategories := TActionCategories.Create;
  fDefaultActions := TObjectList.Create;

  actionList := fmMain.alMain;

  for i := 0 to actionList.ActionCount - 1 do
  begin
    fActionCategories.AddAction(TCustomAction(actionList[i]));
    fDefaultActions.Add(TActionDefault.Create(TCustomAction(actionList[i])));
  end;

  for i := 0 to gDefaultActions.Count - 1 do
  begin
    def := TActionDefault(gDefaultActions[i]);
    if def.Shortcut <> def.Action.ShortCut then
    begin
      fModified := True;
      Break;
    end;
  end;

  fActionCategories.ScanForDuplicates;
end;

{ TActionCategory }

constructor TActionCategory.Create(AOwner: TActionCategories;
  const ACategoryName: string);
begin
  fOwner := AOwner;
  fCategoryName := ACategoryName;
  OwnsObjects := False;
end;

function TActionCategory.GetAction(idx: Integer): TCustomAction;
begin
  Result := TCustomAction(Items[idx]);
end;

function TActionCategory.GetCategoryHasDuplicates: Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Count - 1 do
    if Action[i].Tag = 1 then
    begin
      Result := True;
      Break;
    end;
end;

{ TActionCategories }

procedure TActionCategories.AddAction(action: TCustomAction);
var
  category: TActionCategory;
begin
  category := FindCategory(action.Category);
  if category = nil then
  begin
    category := TActionCategory.Create(self, action.Category);
    Add(category);
  end;

  category.Add(action);
end;

function TActionCategories.FindCategory(const name: string): TActionCategory;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to count - 1 do
    if Category[i].CategoryName = name then
    begin
      Result := Category[i];
      Break;
    end;
end;

function TActionCategories.GetCategory(idx: Integer): TActionCategory;
begin
  Result := TActionCategory(Items[idx]);
end;

procedure TActionCategories.ScanForDuplicates;
var
  s: TStringList;
  st: string;
  i, j: Integer;
  cat: TActionCategory;
  act: TCustomAction;
begin
  s := TStringList.Create;
  s.Duplicates := dupAccept;
  try
    for i := 0 to Count - 1 do
    begin
      cat := Category[i];
      for j := 0 to cat.Count - 1 do
      begin
        act := cat.Action[j];
        act.Tag := 0;

        st := ShortcutToText(act.Shortcut);
        if st <> '' then
          s.AddObject(st, act);
      end;
    end;

    s.Sort;

    fHasDuplicates := False;
    for i := 1 to s.Count - 1 do
      if s[i] = s[i - 1] then
      begin
        fHasDuplicates := True;
        TCustomAction(s.Objects[i]).Tag := 1;
        TCustomAction(s.Objects[i - 1]).Tag := 1;
      end;

  finally
    s.Free;
  end;
end;


{ TfmPropertyPageShortcuts }

class function TfmPropertyPageShortcuts.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageShortcutsData;
end;

procedure TfmPropertyPageShortcuts.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageShortcutsData;
  vstActions.RootNodeCount := fData.fActionCategories.Count;
end;

procedure TfmPropertyPageShortcuts.vstActionsAfterItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect);
var
  color: TColor;
  rgb: DWORD;
begin
  if GetNodeAction(Node) = nil then
  begin
    color := TargetCanvas.Brush.Color;
    rgb := ColorToRGB(color);
    rgb := Windows.RGB(GetRValue(rgb) * 95 div 100, GetGValue(rgb) * 95 div 100, GetBValue(rgb * 95 div 100));

    TargetCanvas.Brush.Color := rgb;
    TargetCanvas.FillRect(ItemRect);
  end;
end;

procedure TfmPropertyPageShortcuts.vstActionsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  data: PObject;
  action: TCustomAction;
begin
  data := PObject(vstActions.GetNodeData(node));
  if Assigned(data) and Assigned(data^) then
    if data^ is TActionCategory then
      case Column of
        0: CellText := TActionCategory(data^).CategoryName;
        1: CellText := '';
      end
    else
      if data^ is TCustomAction then
      begin
        action := TCustomAction(data^);
        case Column of
          0: CellText := StringReplace(StringReplace(action.Caption, '...', '', [rfReplaceAll]), '&', '', [rfReplaceAll]);
          1: CellText := ShortcutToText(action.Shortcut);
        end;
      end;
end;

procedure TfmPropertyPageShortcuts.vstActionsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  data, parentData: PObject;
  category: TActionCategory;
begin
  data := PObject(vstActions.GetNodeData(node));
  if parentNode = nil then
  begin
    category := fData.fActionCategories.Category[node^.Index];
    data^ := category;
    if category.Count > 0 then
      InitialStates :=[ivsHasChildren];
  end
  else
  begin
    parentData := PObject(vstActions.GetNodeData(ParentNode));
    category := TActionCategory(parentData^);
    data^ := category[node^.Index];
  end;
end;

procedure TfmPropertyPageShortcuts.vstActionsInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  data: PObject;
begin
  data := PObject(vstActions.GetNodeData(node));
  if Assigned(data) and Assigned(data^) then
    if data^ is TActionCategory then
      ChildCount := TActionCategory(data^).Count;
end;

function TfmPropertyPageShortcuts.GetNodeAction(node: PVirtualNode): TCustomAction;
var
  data: PObject;
begin
  data := vstActions.GetNodeData(node);
  if Assigned(data) and Assigned(data^) and (data^ is TCustomAction) then
    Result := TCustomAction(data^)
  else
    Result := nil;
end;

function TfmPropertyPageShortcuts.GetSelectedAction: TCustomAction;
begin
  Result := getNodeAction(vstActions.GetFirstSelected);
end;

procedure TfmPropertyPageShortcuts.vstActionsPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PObject;
  highlightDuplicates: Boolean;
begin
  data := vstActions.GetNodeData(node);
  highlightDuplicates := False;
  if Assigned(data) and Assigned(data^) then
    if data^ is TActionCategory then
      highlightDuplicates := TActionCategory(data^).CategoryHasDuplicates
    else
      if data^ is TCustomAction then
        highlightDuplicates := TCustomAction(data^).Tag = 1;

  if highlightDuplicates then
    TargetCanvas.Font.Color := clRed
end;

procedure TfmPropertyPageShortcuts.vstActionsNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: string);
var
  act: TCustomAction;
  sc: TShortcut;
begin
  act := GetNodeAction(node);
  if Assigned(act) then
    if (NewText = '(None)') or (NewText = '') then
      act.ShortCut := 0
    else
    begin
      sc := TextToShortcut(NewText);
      if sc <> 0 then
        act.ShortCut := sc
    end;

  fData.fModified := True;
  fData.fActionCategories.ScanForDuplicates;
  vstActions.Invalidate;
end;

procedure TfmPropertyPageShortcuts.vstActionsCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
begin
  EditLink := TComboEditLink.Create;
end;

procedure TfmPropertyPageShortcuts.UpdateActions;
begin
  btnEdit.Enabled := GetSelectedAction <> nil;
  btnRestoreDefaults.Enabled := fData.fModified;
end;


{ TVTComboBox }

constructor TVTComboBox.Create(Link: TComboEditLink);
begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  FLink := Link;
end;

procedure TVTComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
  begin
    if tsUseThemes in FLink.FTree.TreeStates then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TVTComboBox.WMChar(var Message: TWMChar);
begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

procedure TVTComboBox.WMDestroy(var Message: TWMDestroy);
begin
  if not FLink.FStopping then
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        if Assigned(OnNewText) then
          OnNewText(FTree, FNode, FColumn, self.Text);
    end;

  inherited;
end;

procedure TVTComboBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TVTComboBox.WMKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    // pretend these keycodes were send to the tree
    VK_ESCAPE:
      FLink.FTree.Perform(Message.Msg, Message.CharCode, Message.KeyData);
    VK_RETURN:
      FLink.CancelEdit;
    VK_UP:
      begin
        Message.CharCode := VK_LEFT;
        inherited;
      end;
    VK_DOWN:
      begin
        Message.CharCode := VK_RIGHT;
        inherited;
      end;
  else
    inherited;
  end;
end;

{ TComboEditLink }

function TComboEditLink.BeginEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  begin
    fEditObjectInstance := Classes.MakeObjectInstance(EditWindowProc);
    {$IFDEF CPUX64}
      fOldEditWindowProc := Pointer(SetWindowLongPtr(FCombo.EditHandle, GWL_WNDPROC, LONG_PTR(fEditObjectInstance)));
    {$ELSE}
      fOldEditWindowProc := Pointer(SetWindowLong(FCombo.EditHandle, GWL_WNDPROC, LPARAM(fEditObjectInstance)));
    {$ENDIF}

    FCombo.Show;
//    F.SelectAll;
    FCombo.SetFocus;
  end;
end;

function TComboEditLink.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FCombo.Hide;
    FTree.CancelEditNode;
  end;
end;

constructor TComboEditLink.Create;
begin
  FCombo := TVTComboBox.Create(Self);
  with FCombo do
  begin
    Visible := False;
//    BorderStyle := bsSingle;
    AutoSize := False;
  end
end;

destructor TComboEditLink.Destroy;
begin
  {$IFDEF CPUX64}
    SetWindowLongPtr(FCombo.EditHandle, GWL_WNDPROC, LONG_PTR(fOldEditWindowProc));
  {$ELSE}
    SetWindowLong(FCombo.EditHandle, GWL_WNDPROC, LPARAM(fOldEditWindowProc));
  {$ENDIF}
  Classes.FreeObjectInstance(fEditObjectInstance);
  FCombo.Free;
  inherited;
end;

procedure TComboEditLink.EditWindowProc(var msg: TMessage);
var
  doDefault: Boolean;
begin
  doDefault := False;

  case msg.Msg of
    WM_GETDLGCODE:
      with Msg do
        Result := CallWindowProc(fOldEditWindowProc, FCombo.EditHandle, msg, wParam, lParam) or DLGC_WANTTAB;

    WM_KEYDOWN:
      case TwmKeyDown(msg).CharCode of
        VK_RETURN :
          CancelEdit;
      else
        doDefault := True;
      end
  else
    doDefault := True;
  end;

  if doDefault then
    with Msg do
      Result := CallWindowProc(fOldEditWindowProc, FCombo.EditHandle, msg, wParam, lParam);
end;

function TComboEditLink.EndEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    with FTree do
      if Assigned(OnNewText) then
        OnNewText(FTree, FNode, FColumn, FCombo.Text);
    FCombo.Hide;
  except
    FStopping := False;
    raise;
  end;
end;

function TComboEditLink.GetBounds: TRect;
begin
  Result := FCombo.BoundsRect;
end;

function TComboEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean;

type
  TCharSet = set of Char;

  procedure AddHotkeyRange(const s: string; minChar, maxChar: Char);
  var
    ch: Char;
  begin
    for ch := minChar to maxChar do
      FCombo.Items.Add(s + ch)
  end;

// retrieves the true text bounds from the owner tree

var
  Text: string;
  idx: Integer;

begin
  Result := Tree is TCustomVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    // initial size, font and text of the node
    Text := FTree.Text[Node, Column];
    fTextBounds := FTree.GetDisplayRect(Node, Column, True, True);
    FCombo.Parent := Tree;
    FCombo.Font := Tree.Font;
    FCombo.Font.Color := clBlack;
    FCombo.Items.BeginUpdate;
    try
      FCombo.Items.Add('(None)');
      AddHotkeyRange('Ctrl+', 'A', 'Z');
      AddHotkeyRange('Ctrl+Alt+', 'A', 'Z');
      AddHotkeyRange('F', '1', '9');
      AddHotkeyRange('F1', '0', '2');
      AddHotkeyRange('Ctrl+F', '1', '9');
      AddHotkeyRange('Ctrl+F1', '0', '2');
      AddHotkeyRange('Shift+F', '1', '9');
      AddHotkeyRange('Shift+F1', '0', '2');
      AddHotkeyRange('Shift+Ctrl+F', '1', '9');
      AddHotkeyRange('Shift+Ctrl+F1', '0', '2');
      FCombo.Items.Add('Ins');
      FCombo.Items.Add('Shift+Ins');
      FCombo.Items.Add('Ctrl+Ins');
      FCombo.Items.Add('Del');
      FCombo.Items.Add('Shift+Del');
      FCombo.Items.Add('Ctrl+Del');
      FCombo.Items.Add('Alt+BkSp');
      FCombo.Items.Add('Shif+Alt+BkSp');
      AddHotkeyRange('', 'A', 'Z');
    finally
      FCombo.Items.EndUpdate;
    end;

    idx := FCombo.Items.IndexOf(Text);
    if idx = -1 then
      FCombo.Caption := Text
    else
      FCombo.ItemIndex := idx;

    if Column <= NoColumn then
    begin
      FCombo.BidiMode := FTree.BidiMode;
      FAlignment := FTree.Alignment;
    end
    else
    begin
      FCombo.BidiMode := FTree.Header.Columns[Column].BidiMode;
      FAlignment := FTree.Header.Columns[Column].Alignment;
    end;

    if FCombo.BidiMode <> bdLeftToRight then
      ChangeBidiModeAlignment(FAlignment);
  end;
end;

procedure TComboEditLink.ProcessMessage(var Message: TMessage);
begin
  FCombo.WindowProc(Message);
end;

procedure TComboEditLink.SetBounds(R: TRect);
begin
  if not FStopping then
  begin
    with R do
    begin
      // Set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's left/right border.
      if Left < 0 then
        Left := 0;
      if Right - Left < 30 then
      begin
        if FAlignment = taRightJustify then
          Left := Right - 30
        else
          Right := Left + 30;
      end;
      if Right > FTree.ClientWidth then
        Right := FTree.ClientWidth;
      FCombo.BoundsRect := R;
    end;
  end;
end;

procedure TfmPropertyPageShortcuts.btnEditClick(Sender: TObject);
begin
  if GetSelectedAction <> nil then
    vstActions.EditNode(vstActions.GetFirstSelected, 1)
end;

procedure TfmPropertyPageShortcuts.btnRestoreDefaultsClick(
  Sender: TObject);
var
  i: Integer;
  def: TActionDefault;
begin
  for i := 0 to gDefaultActions.Count - 1 do
  begin
    def := TActionDefault(gDefaultActions[i]);
    def.Action.ShortCut := def.Shortcut;
    vstActions.BeginUpdate;
    try
      vstActions.ReinitNode(nil, True);
    finally
      vstActions.EndUpdate
    end
  end;

  fData.fActionCategories.ScanForDuplicates;
  vstActions.Invalidate;
  fData.fModified := False;
end;

procedure TfmPropertyPageShortcuts.FormCreate(Sender: TObject);
begin
  inherited;
  vstActions.NodeDataSize := SizeOf(Pointer);
end;

end.
