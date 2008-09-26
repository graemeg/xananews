unit cmpExplorerTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, ShlObj, ShFolder, ActiveX;

type
  TCustomExplorerTree = class;

  TFolderInfo = class
  private
    fOwner : TCustomExplorerTree;
    fPIDL : PItemIDList;            // Full ID List of folder
    fParentFolder : IShellFolder;   // 'IShellFolder' of parent
    fRIDL : PItemIDList;            // ID List relative to the parent
    fFolderAttributes : DWORD;
    function GetPath: string;

    procedure DoAfterCreate;
    function GetImageIndex: Integer;
    function GetHasChildren: boolean;
    function GetParentFolder: IShellFolder;
    function GetName: string;
    function FolderAttributes : DWORD;

    property ParentFolder : IShellFolder read GetParentFolder;
  public
    constructor Create (AOwner : TCustomExplorerTree; ACSIDL : DWORD); overload;
    constructor Create (AOwner : TCustomExplorerTree; const APath : string); overload;
    constructor Create (AOwner : TCustomExplorerTree; APIDL : PItemIDList); overload;

    destructor Destroy; override;

    property Owner : TCustomExplorerTree read fOwner;
    property Name : string read GetName;
    property HasChildren : boolean read GetHasChildren;
    property ImageIndex : Integer read GetImageIndex;
    property Path : string read GetPath;
  end;

  TExplorerTreeStandardFolders = (
    sfPathname,
    sfCustom,

    sfDesktop,

    sfComputer,
    sfComputerPrograms,
    sfComputerSystem,
    sfComputerWindows,

    sfUserAdminTools,
    sfUserDocuments,
    sfUserMusic,
    sfUserPictures,
    sfUserTemplates,
    sfUserVideos,

    sfCommonAdminTools,
    sfCommonDocuments,
    sfCommonMusic,
    sfCommonPictures,
    sfCommonTemplates,
    sfCommonVideo);

  TOleDropSource = class (TInterfacedObject, IDropSource, IDataObject)
  public
  { IDataObject }
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult;
      stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
      stdcall;

    { IDropSource }
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  end;

  TOleDragControlObject = class (TDragControlObjectEx)
  end;

  TCustomExplorerTree = class(TCustomTreeView)
  private
    fFileSystemOnly: boolean;
    fImages : TImageList;
    fShowImages: boolean;
    fFolderCount : Integer;
    fAutoPopupMenu: boolean;

    fRootFolder : TExplorerTreeStandardFolders;
    fRootFolderPath : string;
    fCustomRootFolderPIDL : PItemIDList;
    fOleDropSource: boolean;
    fLastMousePosition : TPoint;
    function GetNodePath(node: TTreeNode): string;
    function GetSelectedPath: string;

    procedure ClearCustomRootPIDL;
    function GetRootFolder: TExplorerTreeStandardFolders;
    procedure SetRootFolder(const Value: TExplorerTreeStandardFolders);
    function GetRootFolderPath: string;
    procedure SetRootFolderPath(const Value: string);
    procedure SetShowImages(const Value: boolean);
    procedure SetFileSystemOnly(const Value: boolean);
    function AddChildEntries (node : TTreeNode) : Integer;
    procedure AddRootEntries;
    procedure HandleContextMenu (mnuIntf : IContextMenu; p : TPoint);
    function HandleMenuCommand (const cmd : string) : boolean;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Clear (branch : TTreeNode = Nil);
    procedure GetImageIndex(Node: TTreeNode); override;
    procedure Delete(Node: TTreeNode); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Loaded; override;

  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Reset;
    procedure SetCustomRootFolder (value : PItemIDList);

    property AutoPopupMenu : boolean read fAutoPopupMenu write fAutoPopupMenu default True;
    property FileSystemOnly : boolean read fFileSystemOnly write SetFileSystemOnly default True;
    property FolderCount : Integer read fFolderCount;
    property RootFolder : TExplorerTreeStandardFolders read GetRootFolder write SetRootFolder default sfDesktop;
    property RootFolderPath : string read GetRootFolderPath write SetRootFolderPath;
    property ShowImages : boolean read fShowImages write SetShowImages default True;
    property Items;

    property SelectedPath : string read GetSelectedPath;
    property NodePath [node : TTreeNode] : string read GetNodePath;
    property OleDropSource : boolean read fOleDropSource write fOleDropSource;

  published
  end;

  TExplorerTree = class (TCustomExplorerTree)
  published
    property Align;
    property Anchors;
    property AutoExpand;

    property AutoPopupMenu;

    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;

    property FileSystemOnly;

    property Font;
    property HideSelection;
    property HotTrack;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;

    property RootFolder;
    property RootFolderPath;

    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowImages;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

//XP Only
//function Shell_GetImageLists (var imgList, smImgList : THandle) : BOOL; stdcall;

implementation

uses ShellAPI, ComObj, unitFolderUtils;

const
  CSIDL_MYMUSIC = $000d;
  CSIDL_MYVIDEO = $000e;
  CSIDL_COMMON_TEMPLATES = $002d;

  StandardFolderCSIDLS : array [Low (TExplorerTreeStandardFolders)..High (TExplorerTreeStandardFolders)] of DWORD = (
    $ffffffff,
    $ffffffff,

    CSIDL_DESKTOP,
    CSIDL_DRIVES,
    CSIDL_PROGRAM_FILES,
    CSIDL_SYSTEM,
    CSIDL_WINDOWS,

    CSIDL_ADMINTOOLS,
    CSIDL_PERSONAL,
    CSIDL_MYMUSIC,
    CSIDL_MYPICTURES,
    CSIDL_TEMPLATES,
    CSIDL_MYVIDEO,

    CSIDL_COMMON_ADMINTOOLS,
    CSIDL_COMMON_DOCUMENTS,
    CSIDL_COMMON_MUSIC,
    CSIDL_COMMON_PICTURES,
    CSIDL_COMMON_TEMPLATES,
    CSIDL_COMMON_VIDEO);


//function Shell_GetImageLists (var imgList, smImgList : THandle) : BOOL; stdcall; external 'shell32.dll';

{ TCustomExplorerTree }

(*--------------------------------------------------------------------------*
 | procedure TCustomExplorerTree.AddChildEntries                            |
 |                                                                          |
 | Enumerate and add the child folder entries for a node                    |
 *--------------------------------------------------------------------------*)
function TCustomExplorerTree.AddChildEntries(node: TTreeNode): Integer;
var
  enum : IEnumIDList;
  fi, cfi : TFolderInfo;
  ridl, pidl : PItemIDList;
  fetched : DWORD;
  itm : TTreeNode;
  fldr : IShellFolder;
  hasChildren : boolean;
begin
  result := 0;
  fi := TFolderInfo (node.Data);

  if IsDesktopFolder (fi.fPIDL) then
    OleCheck (fi.ParentFolder.EnumObjects(Handle, SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN, enum))
  else
  begin
    OleCheck (fi.ParentFolder.BindToObject(fi.fRIDL, Nil, IShellFolder, fldr));
    OleCheck (fldr.EnumObjects(Handle, SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN, enum));
  end;

  if enum <> Nil then
    enum.Reset;

  if enum <> nil then
  repeat
    enum.Next(1, ridl, fetched);
    if fetched = 0 then   // Work round 'Control Panel' horror where it
      ridl := Nil;        // always returns 'fetched=1' even when it's at
                          // the end of the list
    if ridl <> Nil then
    begin
      Inc (result);
      pidl := AppendPIDL (fi.fPIDL, ridl);
      CoTaskMemFree (ridl);
      cfi := TFolderInfo.Create(self, pidl);
      try
        if not FileSystemOnly or ((cfi.FolderAttributes and SFGAO_FILESYSANCESTOR) <> 0) then
        begin
          hasChildren := cfi.HasChildren;
          itm := Items.AddChildObject(node, cfi.Name, cfi);
          cfi := Nil;
          itm.HasChildren := hasChildren
        end
      finally
        cfi.Free
      end;
    end
  until ridl = Nil
end;

(*--------------------------------------------------------------------------*
 | procedure TCustomExplorerTree.AddRootEntries                             |
 |                                                                          |
 | Clear the tree and add the root entries.                                 |
 *--------------------------------------------------------------------------*)
procedure TCustomExplorerTree.AddRootEntries;
var
  item : TTreeNode;
  folder : TFolderInfo;
begin
  Items.BeginUpdate;
  try
    Clear;

    if RootFolder = sfPathname then
      folder := TFolderInfo.Create(self, fRootFolderPath)
    else
      if RootFolder = sfCustom then
        folder := TFolderInfo.Create(self, CopyPidl (fCustomRootFolderPIDL))
      else
        folder := TFolderInfo.Create(self, StandardFolderCSIDLs [RootFolder]);
    item := Items.AddObject(nil, folder.Name, folder);
    item.HasChildren := folder.HasChildren;
  finally
    Items.EndUpdate
  end
end;


(*--------------------------------------------------------------------------*
 | procedure TCustomExplorerTree.CanExpand                                  |
 |                                                                          |
 | Determine whether the node can *really* be expanded.  Add child nodes    |
 *--------------------------------------------------------------------------*)
function TCustomExplorerTree.CanExpand(Node: TTreeNode): Boolean;
begin
  if node.HasChildren then
  begin
    if node.Count > 0 then
      result := True
    else
    begin
      result := AddChildEntries (Node) <> 0;
      if not result then Node.HasChildren := False
    end
  end
  else
    result := False
end;

(*--------------------------------------------------------------------------*
 | procedure TCustomExplorerTree.Clear                                      |
 |                                                                          |
 | Clear - either the whoe tree or a node and its children                  |
 *--------------------------------------------------------------------------*)
procedure TCustomExplorerTree.Clear (branch : TTreeNode);
begin
  Items.BeginUpdate;
  try
    if branch = Nil then
      Items.Clear
    else
      items.Delete(branch);
  finally
    Items.EndUpdate
  end;
end;

procedure TCustomExplorerTree.ClearCustomRootPIDL;
begin
  if Assigned (fCustomRootFolderPIDL) then
  begin
    CoTaskMemFree (fCustomRootFolderPIDL);
    fCustomRootFolderPIDL := Nil;
  end
end;

constructor TCustomExplorerTree.Create(AOwner: TComponent);
begin
  inherited;

  fFileSystemOnly := True;
  fAutoPopupMenu := True;
  fRootFolder := sfDesktop;
  fShowImages := True;
  fOleDropSource := True;
end;

procedure TCustomExplorerTree.CreateWnd;
//var
//  il, smil : THandle;
begin
  inherited;
  fImages.Free;
//  if ShowImages and Shell_GetImageLists (il, smil) then
//  begin
//    fImages := TImageList.Create(nil);
//    fImages.Handle := smil;
//    fImages.ShareImages := True;
//    self.Images := fImages;
//  end;
  if not (csLoading in ComponentState) then
    AddRootEntries
end;

procedure TCustomExplorerTree.Delete(Node: TTreeNode);
var
  p : Pointer;
begin
  inherited;

  p := Node.Data;
  Node.Data := Nil;
  TFolderInfo (p).Free
end;

destructor TCustomExplorerTree.Destroy;
begin
  fImages.Free;
  ClearCustomRootPIDL;

  inherited;
end;

procedure TCustomExplorerTree.DestroyWnd;
begin
  Clear;
  inherited;
end;

procedure TCustomExplorerTree.GetImageIndex(Node: TTreeNode);
begin
  node.ImageIndex := TFolderInfo (node.Data).ImageIndex;
  node.SelectedIndex := node.ImageIndex
end;

function TCustomExplorerTree.GetNodePath(node: TTreeNode): string;
var
  folder : TFolderInfo;
begin
  if node <> Nil then
    folder := TFolderInfo (node.Data)
  else
    folder := Nil;
  if folder = Nil then Exit;

  result := folder.Path
end;

function TCustomExplorerTree.GetRootFolder: TExplorerTreeStandardFolders;
begin
  result := fRootFolder;
end;

function TCustomExplorerTree.GetRootFolderPath: string;
var
  info : TFolderInfo;
begin
  if (RootFolder = sfPathname) or (csDesigning in ComponentState) or not HandleAllocated then
    result := fRootFolderPath
  else
  begin
    info := TFolderInfo.Create(self, StandardFolderCSIDLs [RootFolder]);
    try
      result := info.Name
    finally
      info.Free
    end
  end
end;

function TCustomExplorerTree.GetSelectedPath: string;
begin
  result := NodePath [Selected];
end;

procedure TCustomExplorerTree.HandleContextMenu(mnuIntf: IContextMenu; p : TPoint);
var
  menu : HMENU;
  cmd : DWORD;
  verb : string;
  ici  : TCMInvokeCommandInfo;
begin
  menu := CreatePopupMenu;
  mnuIntf.QueryContextMenu(menu, 0, 1, MaxWord, CMF_EXPLORE or CMF_CANRENAME);
  MapWindowPoints (Handle, HWND_DESKTOP, p, 1);
  cmd := DWORD (TrackPopupMenu (menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or
        TPM_RETURNCMD or TPM_NONOTIFY, p.x, p.y, 0, Handle, Nil));
  if cmd = 0 then Exit;

  Dec (cmd);

  SetLength (verb, 256);
  if Succeeded (mnuIntf.GetCommandString(cmd, GCS_VERBA, Nil, PAnsiChar(AnsiString(verb)), 256)) then
  begin
    verb := PChar (verb);
    if not HandleMenuCommand (verb) then
    begin
      FillChar (ici, sizeof (ici), 0);
      ici.cbSize := sizeof (ici);
      ici.hwnd := Handle;
      ici.lpVerb := MakeIntResourceA(cmd);
      ici.nShow := SW_SHOWNORMAL;

      mnuIntf.InvokeCommand(ici)
    end
  end
end;

function TCustomExplorerTree.HandleMenuCommand(const cmd: string): boolean;
begin
  if SameText (cmd, 'rename') then
    result := True
  else
    result := False;
end;

procedure TCustomExplorerTree.Loaded;
begin
  inherited;
  AddRootEntries
end;

procedure TCustomExplorerTree.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  node : TTreeNode;
  Folder : TFolderInfo;
  menu : IContextMenu;
begin
  fLastMousePosition := Point (x, y);
  if (Button = mbRight) and AutoPopupMenu and (PopupMenu = Nil) then
  begin
    node := GetNodeAt(x, y);
    if node <> Nil then Folder := TFolderInfo (node.Data) else Folder := Nil;

    if Folder <> Nil then
      if Succeeded (Folder.fParentFolder.GetUIObjectOf(Handle, 1, Folder.fRidl, IID_IContextMenu, Nil, menu)) and (menu <> Nil) then
        HandleContextMenu (menu, Point (x, y));
  end;

  inherited;
end;

procedure TCustomExplorerTree.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TCustomExplorerTree.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited

end;

procedure TCustomExplorerTree.Reset;
begin
  if HandleAllocated then
    AddRootEntries
end;

procedure TCustomExplorerTree.SetCustomRootFolder(value: PItemIDList);
begin
  fCustomRootFolderPIDL := CopyPIDL (value);
  RootFolder := sfCustom;
end;

procedure TCustomExplorerTree.SetFileSystemOnly(const Value: boolean);
begin
  if Value <> fFileSystemOnly then
  begin
    fFileSystemOnly := Value;
    Reset
  end
end;

procedure TCustomExplorerTree.SetRootFolder(
  const Value: TExplorerTreeStandardFolders);
begin
  if Value <> fRootFolder  then
  begin
    fRootFolder := Value;
    if Value <> sfCustom then
      ClearCustomRootPIDL;
    if Value <> sfPathname then
      fRootFolderPath := '';
    Reset;
  end
end;

procedure TCustomExplorerTree.SetRootFolderPath(const Value: string);
begin
  if Value <> fRootFolderPath then
  begin
    fRootFolderPath := Value;
    if Value <> '' then
    begin
      fRootFolder := sfPathName;
      ClearCustomRootPIDL;
    end;
    Reset
  end
end;

procedure TCustomExplorerTree.SetShowImages(const Value: boolean);
begin
  if Value <> fShowImages then
  begin
    fShowImages := Value;
    RecreateWnd
  end
end;

procedure TCustomExplorerTree.WMLButtonDown(var Message: TWMLButtonDown);
var
  MousePos : TPoint;
  node : TTreeNode;
begin
  inherited;

  if (DragMode = dmManual) and OleDropSource then
  begin
    SetFocus;

    node := GetNodeAt (Message.XPos, Message.YPos);
    if node <> Nil then
    begin
      GetCursorPos(MousePos);
      with PointToSmallPoint(ScreenToClient(MousePos)) do
        Perform(WM_LBUTTONUP, 0, MakeLong(X, Y))
    end
  end
end;

{ TFolderInfo }

constructor TFolderInfo.Create(AOwner : TCustomExplorerTree; ACSIDL: DWORD);
begin
  fOwner := AOwner;
  OleCheck (ShGetSpecialFolderLocation (Owner.Handle, ACSIDL, fPidl));
  DoAfterCreate;
end;

constructor TFolderInfo.Create(AOwner : TCustomExplorerTree; const APath: string);
var
  sf : IShellFolder;
  eaten, attribs : DWORD;
begin
  fOwner := AOwner;
  OleCheck (ShGetDesktopFolder (sf));
  attribs := 0;
  OleCheck (sf.ParseDisplayName(Owner.Handle, Nil, PWideChar (WideString (APath)), eaten, fPIDL, attribs));
  DoAfterCreate;
end;

constructor TFolderInfo.Create(AOwner: TCustomExplorerTree; APIDL: PItemIDList);
begin
  fOwner := AOwner;
  fPIDL := APIDL;
  DoAfterCreate;
end;

destructor TFolderInfo.Destroy;
begin
  if Assigned (fOwner) then
     Dec (fOwner.fFolderCount);
  
  fParentFolder := Nil;
  if fPIDL <> Nil then
    CoTaskMemFree (fPIDL);

  inherited;
end;

procedure TFolderInfo.DoAfterCreate;
begin
  if Assigned (fOwner) then
    Inc (fOwner.fFolderCount);
end;

function TFolderInfo.FolderAttributes: DWORD;
begin
  if fFolderAttributes = 0 then
  begin
    fFolderAttributes := SFGAO_HASSUBFOLDER or SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR;
    OleCheck (ParentFolder.GetAttributesOf(1, fRIDL, fFolderAttributes));
  end;

  result := fFolderAttributes;
end;

function TFolderInfo.GetParentFolder: IShellFolder;
begin
  if fParentFolder = Nil then
    OleCheck (ShBindToParent (fPidl, IShellFolder, fParentFolder, fRidl));

  result := fParentFolder;
end;

function TFolderInfo.GetPath: string;
begin
  result := GetFolderName (ParentFolder, fRIDL, fnForParsing);
end;

function TFolderInfo.GetHasChildren: boolean;
begin
  result := (FolderAttributes and SFGAO_HASSUBFOLDER) <> 0
end;

function TFolderInfo.GetImageIndex: Integer;
var
  info : TShFileInfo;
begin
  if Owner.ShowImages then
  begin
    FillChar (info, sizeof (info), 0);
    ShGetFileInfo (PChar (fPIDL), 0, info, sizeof (info), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    result := info.iIcon;
  end
  else
    result := -1;
end;

function TFolderInfo.GetName: string;
begin
  result := GetFolderName (ParentFolder, fRIDL);
end;

{ TOleDropSource }

function TOleDropSource.DAdvise(const formatetc: TFormatEtc; advf: Integer;
  const advSink: IAdviseSink; out dwConnection: Integer): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.DUnadvise(dwConnection: Integer): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.EnumFormatEtc(dwDirection: Integer;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.GiveFeedback(dwEffect: Integer): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  result := E_NOTIMPL;
end;

function TOleDropSource.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  result := E_NOTIMPL;
end;

end.
