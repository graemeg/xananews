unit unitFolderUtils;

interface

uses Windows, Classes, SysUtils, ShlObj, ShFolder, ActiveX;

type
  TFolderNameType = (fnNormal, fnInFolder, fnForEditing, fnForAddressBar, fnForParsing);

function SHBindToParent(pidl : PItemIdList; const riid : TIID; out ppv; var ppidlLast : PItemIdList) : Hresult; stdcall;
function GetFolderName (folder : IShellFolder; pidl : PItemIDList; tp : TFolderNameType = fnNormal) : WideString; overload;
function AppendPIDL(DestPIDL, SrcPIDL: PItemIDList): PItemIDList;
function IsDesktopFolder (pidl : PItemIDList) : boolean;
function CopyPIDL(APIDL: PItemIDList): PItemIDList;

implementation

uses Controls, ShellAPI;

function SHBindToParent(pidl : PItemIdList; const riid : TIID; out ppv; var ppidlLast : PItemIdList) : Hresult; stdcall; external 'shell32.dll';

function GetFolderName (folder : IShellFolder; pidl : PItemIDList; tp : TFolderNameType) : WideString; overload;
var
  t : DWORD;
  str : TStrRet;
begin
  case tp of
    fnInfolder      : t := SHGDN_INFOLDER;
    fnForEditing    : t := SHGDN_FOREDITING;
    fnForAddressBar : t := SHGDN_FORADDRESSBAR;
    fnForPARSING    : t := SHGDN_FORPARSING;
    else
      t := SHGDN_NORMAL;
  end;

  if Succeeded (folder.GetDisplayNameOf(pidl, t, str)) then
  case str.uType of
    STRRET_CSTR   : result := str.cStr;
    STRRET_WSTR   : begin
                      result := str.pOleStr;
                      CoTaskMemFree (str.pOleStr)
                    end;
    STRRET_OFFSET : result := PChar (pidl) + str.uOffset;
    else result := ''
  end
  else
    result := ''
end;

function GetFolderName (pidl : PItemIDList; tp : TFolderNameType = fnNormal) : WideString; overload;
var
  ridl : PItemIDList;
  folder : IShellFolder;
begin
  if Succeeded (ShBindToParent (pidl, IShellFolder, folder, ridl)) then
    result := GetFolderName (folder, ridl, tp);
end;

function IsDesktopFolder (pidl : PItemIDList) : boolean;
begin
  result := (pidl <> Nil) and (pidl^.mkid.cb = 0);
end;

function NextID(APIDL: PItemIDList): PItemIDList;
begin
  result := PItemIDList (PChar (APIDL) + APIDL^.mkid.cb);
end;

function PIDLSize(APIDL: PItemIDList): integer;
// Returns the total Memory in bytes the PIDL occupies.
begin
  Result := 0;
  if Assigned(APIDL) then
  begin
    Result := SizeOf( Word);  // add the null terminating last ItemID
    while APIDL.mkid.cb <> 0 do
    begin
      Result := Result + APIDL.mkid.cb;
      APIDL := NextID(APIDL);
    end;
  end;
end;


function AppendPIDL(DestPIDL, SrcPIDL: PItemIDList): PItemIDList;
// Returns the concatination of the two PIDLs. Neither passed PIDLs are
// freed so it is up to the caller to free them.
var
  DestPIDLSize, SrcPIDLSize: integer;
begin
  DestPIDLSize := 0;
  SrcPIDLSize := 0;
  // Appending a PIDL to the DesktopPIDL is invalid so don't allow it.
  if Assigned(DestPIDL) then
    if not IsDesktopFolder(DestPIDL) then
      DestPIDLSize := PIDLSize(DestPIDL) - SizeOf(DestPIDL^.mkid.cb);

  if Assigned(SrcPIDL) then
    SrcPIDLSize := PIDLSize(SrcPIDL);

  Result := CoTaskMemAlloc (DestPIDLSize + SrcPIDLSize);
  if Assigned(Result) then
  begin
    if Assigned(DestPIDL) then
      CopyMemory(Result, DestPIDL, DestPIDLSize);
    if Assigned(SrcPIDL) then
      CopyMemory(Pchar(Result) + DestPIDLSize, SrcPIDL, SrcPIDLSize);
  end;
end;

function CopyPIDL(APIDL: PItemIDList): PItemIDList;
var
  Size: integer;
begin
  if Assigned(APIDL) then
  begin
    Size := PIDLSize(APIDL);
    Result := CoTaskMemAlloc (Size);
    if Result <> nil then
      CopyMemory(Result, APIDL, Size);
  end else
    Result := nil
end;



end.
