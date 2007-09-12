unit MapiX;

interface

uses Windows, MAPI;

function MapiLogonEx (ulUIParam: Cardinal; lpszProfileName: LPSTR;
  lpszPassword: LPSTR; flFlags: FLAGS; lplhSession: PLHANDLE) : HRESULT;

function SupportsMAPIX : boolean;

const
 MAPI_NO_MAIL = $00008000;  // Do not activate transports

 MAPI_SIMPLE_DEFAULT = (MAPI_LOGON_UI or MAPI_FORCE_DOWNLOAD or MAPI_ALLOW_OTHERS);

implementation

type
TFNMapiLogOnEx = function(ulUIParam: Cardinal; lpszProfileName: LPSTR;
  lpszPassword: LPSTR; flFlags: FLAGS; lplhSession: PLHANDLE): Cardinal stdcall;

TFNMapiInitialize = function(lpMapiInit : pointer) : HRESULT; stdcall;
TFNMapiUnInitialize = procedure; stdcall;

var
  MAPIChecked : boolean = False;
  MAPIModule: HModule = 0;
  LogOnEx: TFNMapiLogOnEx = nil;
  MapiInitialize : TFNMapiInitialize = nil;
  MapiUninitialize : TFNMapiUninitialize = nil;
  fInitialized : boolean = False;

const
  MAPIDLL = 'MAPI32.DLL';

procedure InitMapiX;
begin
  if not MAPIChecked then
  begin
    MAPIModule := LoadLibrary(MAPIDLL);
    if MAPIModule <> 0 then
    begin
      LogOnEx := getProcAddress (MAPIModule, 'MAPILogonEx');
      MapiInitialize := GetProcAddress (MAPIModule, 'MAPIInitialize');
      MapiUninitialize := GetProcAddress (MAPIModule, 'MAPIUninitialize');
      if (@MapiInitialize <> Nil) and (@LogonEx <> Nil) and (@MapiUninitialize <> Nil) then
        fInitialized := MapiInitialize (nil) = 0;
    end;
    MAPIChecked := True
  end
end;

function MapiLogonEx (ulUIParam: Cardinal; lpszProfileName: LPSTR;
  lpszPassword: LPSTR; flFlags: FLAGS; lplhSession: PLHANDLE) : HRESULT;
begin
  if SupportsMAPIX then
    result := LogOnEx (ulUIParam, lpszProfileName, lpszPassword, flFlags, lplhSession)
  else
    result := MAPI_E_FAILURE;
end;

function SupportsMAPIX : boolean;
begin
  InitMapiX;
  result := fInitialized
end;

initialization
finalization
  if fInitialized then
    MapiUninitialize
end.
