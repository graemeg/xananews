unit unitWow64;

interface

function IsWow64: Boolean;

implementation

uses
  Windows;

type
  LPFN_ISWOW64PROCESS = function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;

function IsWow64: Boolean;
var
  fnIsWow64Process: LPFN_ISWOW64PROCESS;
  bIsWow64: BOOL;
begin
  Result := False;
  fnIsWow64Process := LPFN_ISWOW64PROCESS(GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(fnIsWow64Process) then
  begin
    bIsWow64 := False;
    if fnIsWow64Process(GetCurrentProcess(), bIsWow64) then
      Result := bIsWow64;
  end;
end;

end.
