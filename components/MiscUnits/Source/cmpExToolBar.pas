(*======================================================================*
 | cmpExToolBar                                                         |
 |                                                                      |
 | Extended Tool Bar control with support for OLE drag/drop              |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      26/11/2002  CPWW  Original                                  |
 *======================================================================*)

unit cmpExToolBar;

interface

uses
  Windows, SysUtils, Classes, Controls, ToolWin, ComCtrls, ActiveX, ShlObj;

type
  TOnDropPIDL = procedure (sender : TObject; parentPIDL, PIDL : PItemIDList) of object;
  TExToolBar = class(TToolBar, IDropTarget)
  private
    fDropTargetHelper : IDropTargetHelper;
    fDropDataObject : IDataObject;
    fOnDropPIDL: TOnDropPIDL;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function IDropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;

    function IDropTarget.DragOver=IDropTargetDragOver;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property OnDropPIDL : TOnDropPIDL read fOnDropPIDL write fOnDropPIDL;
  end;

TFolderNameType = (fnNormal, fnInFolder, fnForEditing, fnForAddressBar, fnForParsing);

function GetFolderName (folder : IShellFolder; pidl : PItemIDList; tp : TFolderNameType = fnNormal) : WideString; overload;
function GetFolderName (pidl : PItemIDList; tp : TFolderNameType = fnNormal) : WideString; overload;
function SHBindToParent(pidl : PItemIdList; const riid : TIID; out ppv; var ppidlLast : PItemIdList) : Hresult; stdcall;
function SHGetFolderLocation(hWndOwner: HWnd; csidl: Integer; hToken: THandle; dwReserved: DWord; out pidl: PItemIDList): HResult; stdcall;

implementation

uses ComObj;

var
  CF_SHELLIDLIST : DWORD = 0;

function SHBindToParent(pidl : PItemIdList; const riid : TIID; out ppv; var ppidlLast : PItemIdList) : Hresult; stdcall; external 'shell32.dll';
function SHGetFolderLocation(hWndOwner: HWnd; csidl: Integer; hToken: THandle; dwReserved: DWord; out pidl: PItemIDList): HResult; stdcall; external 'shell32.dll';

function GetFolderName (folder : IShellFolder; pidl : PItemIDList; tp : TFolderNameType) : WideString;
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
    STRRET_CSTR   : result := string(str.cStr);
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

function GetFolderName (pidl : PItemIDList; tp : TFolderNameType) : WideString;
var
  ridl : PItemIDList;
  folder : IShellFolder;
begin
  if Succeeded (ShBindToParent (pidl, IShellFolder, folder, ridl)) then
    result := GetFolderName (folder, ridl, tp);
end;

{ TExToolbar }

constructor TExToolbar.Create(AOwner: TComponent);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    CoCreateInstance (CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IID_IDropTargetHelper, fDropTargetHelper);
end;

procedure TExToolbar.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    RegisterDragDrop(Handle, Self);
end;

destructor TExToolbar.Destroy;
begin
  fDropTargetHelper := Nil;

  inherited;
end;

procedure TExToolbar.DestroyWnd;
begin
  if not (csDesigning in ComponentState) then
    RevokeDragDrop (Handle);
  inherited;
end;

function TExToolbar.DragEnter(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HResult;
begin
  if Assigned(fDropTargetHelper) then
    result := fDropTargetHelper.DragEnter(Handle, dataObj, Pt, dwEffect)
  else
    result := E_UNEXPECTED;

  if not Succeeded (result) then Exit;

  dwEffect := DROPEFFECT_NONE;

  fDropDataObject := dataObj;

  if Assigned (OnDropPIDL) then
    dwEffect := DROPEFFECT_LINK;
  
end;

function TExToolbar.DragLeave: HResult;
begin
  if Assigned(fDropTargetHelper) then
    result := fDropTargetHelper.DragLeave
  else
    result := E_UNEXPECTED;

  fDropDataObject := Nil;

  if not Succeeded (result) then Exit;
end;

function TExToolbar.IDropTargetDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
begin
  if Assigned(fDropTargetHelper) then
    result := fDropTargetHelper.DragOver(pt, dwEffect)
  else
    result := E_UNEXPECTED;

  if not Succeeded (result) then Exit;
end;

function TExToolbar.Drop(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HResult;
var
  formatEtc : TFormatETC;
  stgMedium: TStgMedium;
  p : PIDA;
  i : Integer;
  ppidl, pidl : PItemIDList;
begin
  if Assigned(fDropTargetHelper) then
    result := fDropTargetHelper.Drop (dataObj, pt, dwEffect)
  else
    result := E_UNEXPECTED;

  if not Succeeded (result) then Exit;
  if not Assigned (OnDropPIDL) then
    Exit;

  FillChar (formatEtc, sizeof (formatEtc), 0);
  formatEtc.cfFormat := CF_SHELLIDLIST;
  formatEtc.dwAspect := DVASPECT_CONTENT;
  formatEtc.lindex := -1;
  formatEtc.tymed := TYMED_HGLOBAL;
  if Succeeded (dataObj.QueryGetData (formatEtc)) then
  begin
    dataObj.GetData(formatETC, stgMedium);
    p := PIDA (GlobalLock (stgMedium.hGlobal));
    try
      ppidl := PItemIDList (PChar (p) + p^.aoffset [0]);
      for i:= 0 to p^.cidl - 1 do
      begin
        pidl := PItemIDList (PChar (p) + p^.aoffset [i+1]);
        OnDropPIDL (self, ppidl, pidl);
      end
    finally
      GlobalUnlock (stgMedium.hGlobal);
    end;
  end
end;

initialization
  OleInitialize (Nil);
  CF_SHELLIDLIST := RegisterClipboardFormat (CFSTR_SHELLIDLIST);
finalization
end.
