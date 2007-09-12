unit unitFolderInfo;

interface

uses Windows, Classes, SysUtils, ShlObj, ShFolder;

type
TFolderInfo = class
private
  fOwnerHWND : HWND;
  fPIDL : PItemIDList;            // Full ID List of folder
  fParentFolder : IShellFolder;   // 'IShellFolder' of parent
  fRIDL : PItemIDList;            // ID List relative to the parent
  fFolderAttributes : DWORD;
  function GetPath: string;

  function GetImageIndex: Integer;
  function GetHasChildren: boolean;
  function GetParentFolder: IShellFolder;
  function GetName: string;
  function FolderAttributes : DWORD;

  property ParentFolder : IShellFolder read GetParentFolder;
protected
  procedure DoAfterCreate; virtual;
  procedure DoBeforeDestroy; virtual;
public
  constructor Create (AOwnerHWND : HWND; ACSIDL : DWORD); overload;
  constructor Create (AOwnerHWND : HWND; const APath : string); overload;
  constructor Create (AOwnerHWND : HWND; APIDL : PItemIDList); overload;

  destructor Destroy; override;

  property OwnerHWND : HWND read fOwnerHWND;
  property Name : string read GetName;
  property HasChildren : boolean read GetHasChildren;
  property ImageIndex : Integer read GetImageIndex;
  property Path : string read GetPath;
end;


implementation

uses ActiveX, ComObj, ShellAPI, unitFolderUtils;

{ TFolderInfo }

constructor TFolderInfo.Create(AOwnerHWND: HWND; ACSIDL: DWORD);
begin
  fOwnerHWND := AOwnerHWND;
  OleCheck (ShGetSpecialFolderLocation (OwnerHWND, ACSIDL, fPidl));
  DoAfterCreate;
end;

constructor TFolderInfo.Create(AOwnerHWND: HWND; const APath: string);
var
  sf : IShellFolder;
  eaten, attribs : DWORD;
begin
  fOwnerHWND := AOwnerHWND;
  OleCheck (ShGetDesktopFolder (sf));
  attribs := 0;
  OleCheck (sf.ParseDisplayName(OwnerHWND, Nil, PWideChar (WideString (APath)), eaten, fPIDL, attribs));
  DoAfterCreate;
end;

constructor TFolderInfo.Create(AOwnerHWND: HWND; APIDL: PItemIDList);
begin
  fOwnerHWND := AOwnerHWND;
  fPIDL := APIDL;
  DoAfterCreate;
end;

destructor TFolderInfo.Destroy;
begin
  DoBeforeDestroy;

  fParentFolder := Nil;
  if fPIDL <> Nil then
    CoTaskMemFree (fPIDL);

  inherited;
end;

procedure TFolderInfo.DoAfterCreate;
begin
// Stub
end;

procedure TFolderInfo.DoBeforeDestroy;
begin
// Stub
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

function TFolderInfo.GetHasChildren: boolean;
begin
  result := (FolderAttributes and SFGAO_HASSUBFOLDER) <> 0
end;

function TFolderInfo.GetImageIndex: Integer;
var
  info : TShFileInfo;
begin
  FillChar (info, sizeof (info), 0);
  ShGetFileInfo (PChar (fPIDL), 0, info, sizeof (info), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  result := info.iIcon;
end;

function TFolderInfo.GetName: string;
begin
  result := GetFolderName (ParentFolder, fRIDL);
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

end.