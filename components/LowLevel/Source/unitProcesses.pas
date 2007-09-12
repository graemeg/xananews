(*======================================================================*
 | unitProcesses                                                        |
 |                                                                      |
 | OS agnostic process enumeration classes.                             |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)
unit unitProcesses;

interface

uses Windows, Classes, SysUtils, psapi, tlhelp32, contnrs, DateUtils;


type

TProcessSnapshot = class;
TProcess = class;

TModule = class
private
  fOwner : TProcess;
  fFileName : string;
  fhModule : THandle;
  fVersionCache : string;
  fFileDateTime : TDateTime;
  fFileSize : DWORD;
  fObjectTag: TObject;
  function GetVersion: string;  // Module handle (in the context of the module's process)
  procedure GetFileDetails;
  function GetFileDateTime: TDateTime;
  function GetFileSize: DWORD;
  function GetFileName: string;
protected
  constructor Create (AOwner : TProcess; fInitDetails : PModuleEntry32); overload;
  constructor Create (AOwner : TProcess; hOwner, hModule : THandle); overload;
public
  property FileName : string read GetFileName;
  property Version : string read GetVersion;
  property FileDateTime : TDateTime read GetFileDateTime;
  property FileSize : DWORD read GetFileSize;
  property ObjectTag : TObject read fObjectTag write fObjectTag;
  property Owner : TProcess read fOwner;
end;

TProcess = class
private
  fParentProcessID: DWORD;
  fProcessID: DWORD;
  fEXEName: string;
  fBasePriority : Longint;
  fModules : TObjectList;
  fObjectTag: TObject;
  fOwner : TProcessSnapshot;

  constructor Create (AOwner : TProcessSnapshot; fInitDetails : PProcessEntry32); overload;
  constructor Create (AOwner : TProcessSnapshot; fPID : DWORD); overload;
  function GetModule(idx: Integer): TModule;
  function GetModuleCount: Integer;
  function GetBaseModule: TModule;
public
  destructor Destroy; override;
  property ProcessID : DWORD read fProcessID;
  property ParentProcessID : DWORD read fParentProcessID;
  property EXEName : string read fEXEName;
  property BasePriority : Longint read fBasePriority;

  property ModuleCount : Integer read GetModuleCount;
  property Module [idx : Integer] : TModule read GetModule; default;
  property BaseModule : TModule read GetBaseModule;
  property ObjectTag : TObject read fObjectTag write fObjectTag;
end;

TPSSortColumn = (psPID, psParentPID, psBasePriority, psName, psDate, psSize, psVersion);
TPSSortDirection = (psAscending, psDescending);
TProcessEnumerator = (peAuto, peToolHelp, pePSAPI);
TProcessSnapshot = class (TComponent)
private
  fProcesses : TObjectList;
  fProcessEnumerator: TProcessEnumerator;
  fSortColumn : TPSSortColumn;
  fSortDirection : TPSSortDirection;
  function GetProcess(idx: Integer): TProcess;
  function GetProcessCount: Integer;
  procedure SetProcessEnumerator(const Value: TProcessEnumerator);
  procedure LoadEnumerator;
protected
  procedure Loaded; override;
public
  constructor Create (AOwner : TComponent); override;
  destructor Destroy; override;

  procedure SortProcesses (column : TPSSortColumn; direction : TPSSortDirection);
  procedure TakeSnapshot;
  property ProcessCount : Integer read GetProcessCount;
  property Process [idx : Integer] : TProcess read GetProcess; default;
  property SortColumn : TPSSortColumn read fSortColumn;
  property SortDirection : TPSSortDirection read fSortDirection;
published
  property ProcessEnumerator : TProcessEnumerator read fProcessEnumerator write SetProcessEnumerator;
end;

EProcessSnapshot = class (exception);

procedure CheckToolHelp;
procedure CheckPSAPI;

implementation

resourcestring
  rstNoToolHelp   = 'Internal error.  No ToolHelp';
  rstNoPSAPI      = 'Internal error.  No PSAPI';
  rstCantSnapshot = 'Internal error.  Can''t enumerate processes on this computer';
  rstIdle         = 'Idle';
  rstSystem       = 'System';

const SE_DEBUG_NAME = 'SeDebugPrivilege';

type  // Support for NtQueryInformationProcess
  TProcessInfoClass = (
    ProcessBasicInformation,
    ProcessQuotaLimits,
    ProcessIoCounters,
    ProcessVmCounters,
    ProcessTimes,
    ProcessBasePriority,
    ProcessRaisePriority,
    ProcessDebugPort,
    ProcessExceptionPort,
    ProcessAccessToken,
    ProcessLdtInformation,
    ProcessLdtSize,
    ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers,          // Note: this is kernel mode only
    ProcessPooledUsageAndLimits,
    ProcessWorkingSetWatch,
    ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup,
    ProcessPriorityClass,
    ProcessWx86Information,
    ProcessHandleCount,
    ProcessAffinityMask,
    ProcessPriorityBoost,
    ProcessDeviceMap,
    ProcessSessionInformation,
    ProcessForegroundInformation,
    ProcessWow64Information,
    MaxProcessInfoClass);

  TProcessBasicInformation = record
    ExitStatus : Longint;
    PebBaseAddress : Pointer;
    AffinityMask : DWORD;
    BasePriority : Longint;
    UniqueProcessId : DWORD;
    InheritedFromUniqueProcessId : DWORD;
  end;

  TfnNtQueryInformationProcess = function (
    Handle : THandle;
    infoClass : TProcessInfoClass;
    processInformation : Pointer;
    processInformationLength : ULONG;
    returnLength : PULONG
  ) : DWORD; stdcall;

var
  gUsesToolHelp : boolean = False;
  gUsesPSAPI : boolean = False;
  gCheckedToolHelp : boolean = False;
  gCheckedPSAPI : boolean = False;
  gOldState : DWORD = $BadF00d;
  NTQueryInformationProcess : TfnNTQueryInformationProcess;

function EnableNTPrivilege (const privilege : string; state : Integer) : Integer;
var
  hToken : THandle;
  aluid : TLargeInteger;
  cbPrevTP : DWORD;
  tp, fPrevTP : PTokenPrivileges;
begin
  result := 0;
  if OpenProcessToken (GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  try
    LookupPrivilegeValue (Nil, PChar (privilege), aluid);

    cbPrevTP := SizeOf (TTokenPrivileges) + sizeof (TLUIDAndAttributes);

    GetMem (tp, cbPrevTP);
    GetMem (fPrevTP, cbPrevTP);
    try

      tp^.PrivilegeCount := 1;
      tp^.Privileges [0].Luid := aLuid;
      tp^.Privileges [0].Attributes := state;

      if not AdjustTokenPrivileges (hToken, False, tp^, cbPrevTP, fPrevTP^, cbPrevTP) then
        RaiseLastOSError;

      result := fPrevTP^.Privileges [0].Attributes;

    finally
     FreeMem (fPrevTP);
     FreeMem (tp);
    end
  finally
    CloseHandle (hToken);
  end
end;

function GetFileVersion (const fileName : string) : string;
var
  size, zero : DWORD;
  buffer, pBuffer: pointer;
  info : PVSFixedFileInfo;
begin
  result := '';
  size := GetFileVersionInfoSize (PChar (FileName), zero);
  if size > 0 then
  begin
    GetMem (buffer, size);
    if not GetFileVersionInfo (PChar (FileName), zero, size, buffer) then
      RaiseLastOSError;

    if not VerQueryValue (buffer, '\', pBuffer, size) then
      RaiseLastOSError;

    info := PVSFixedFileInfo (pBuffer);

    result := Format ('%d.%d.%d.%d', [HiWord (info^.dwProductVersionMS), LoWord (info^.dwProductVersionMS), HiWord (info^.dwProductVersionLS), LoWord (info^.dwProductVersionLS)])
  end
  else
    result := '-'
end;

function UsesToolHelp : boolean;
var
  KernelHandle : THandle;
begin
  if not gCheckedToolHelp then
  begin
    gCheckedToolHelp := True;
    KernelHandle := GetModuleHandle(kernel32);
    gUsesToolHelp := Assigned (GetProcAddress(KernelHandle, 'CreateToolhelp32Snapshot'))
  end;

  result := gUsesToolHelp
end;

function UsesPSAPI : boolean;
var
  hPSAPI : THandle;
  hNTDLL : THandle;
begin
  if not gCheckedPSAPI then
  begin
    gCheckedPSAPI := True;
    hPSAPI := LoadLibrary('PSAPI.dll');
    if hPSAPI <> 0 then
    begin
      FreeLibrary (hPSAPI);
      gUsesPSAPI := True;

      hNTDLL := GetModuleHandle ('ntdll.dll');
      if hNTDLL <> 0 then
      begin
        NTQueryInformationProcess := GetProcAddress (hNTDLL, 'ZwQueryInformationProcess');
        if not Assigned (NTQueryInformationProcess) then
          raise EProcessSnapshot.Create(rstCantSnapshot);
       end

    end
  end;
  result := gUsesPSAPI
end;

procedure CheckToolHelp;
begin
  if not UsesToolHelp then
    raise EProcessSnapshot.Create(rstNoToolhelp);
end;

procedure CheckPSAPI;
begin
  if not UsesPSAPI then
    raise EProcessSnapshot.Create (rstNoPSAPI)
end;

{ TProcess }

constructor TProcess.Create(AOwner: TProcessSnapshot;
  fInitDetails: PProcessEntry32);
var
  hSnapshot : THandle;
  moduleEntry : TModuleEntry32;
begin
  fOwner := AOwner;
  CheckToolHelp;
  fModules := TObjectList.Create;

  fProcessID := fInitDetails^.th32ProcessID;
  fParentProcessID := fInitDetails^.th32ParentProcessID;
  fEXEName := fInitDetails^.szExeFile;
  fBasePriority := fInitDetails^.pcPriClassBase;

  if fParentProcessID <> 0 then
  begin
    hSnapshot := TlHelp32.CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, fProcessID);
    if hSnapshot <> INVALID_HANDLE_VALUE then
    try
      moduleEntry.dwSize := sizeof (moduleEntry);

      if Module32First (hSnapshot, moduleentry) then
      begin
        fEXEName := moduleEntry.szExePath;
        repeat
          fModules.Add(TModule.Create(self, @moduleEntry))
        until not Module32Next (hSnapshot, moduleEntry)
      end
    finally
      Closehandle (hSnapshot)
    end
  end
end;

constructor TProcess.Create(AOwner: TProcessSnapshot; fPID: DWORD);
var
  hProcess : THandle;
  processInfo : TProcessBasicInformation;
  baseName : array [0..MAX_PATH] of char;
  buffer, p : PDWORD;
  bufLen : DWORD;
  i, modCount : Integer;
begin
  fOwner := AOwner;
  CheckPSAPI;
  fModules := TObjectList.Create;
  fProcessID := fPID;
  if fPID = 0 then
    fEXEName := rstIdle
  else
  begin
    hProcess := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, fPid);
    if hProcess <> 0 then
    try
      buffer := Nil;
      try
        ReallocMem (buffer, 1024*1024);
        if not psapi.EnumProcessModules(hProcess, buffer, 1024*1024, bufLen) then bufLen := 0;
        ReallocMem (buffer, bufLen);
        modCount := bufLen div sizeof (PDWORD);

        p := buffer;
        for i := 0 to modCount - 1 do
        begin
          fModules.Add(TModule.Create(self, hProcess, p^));
          Inc (p)
        end
      finally
        ReallocMem (buffer, 0)
      end;

      if psapi.GetModuleBaseName(hProcess, 0, baseName, sizeof (baseName)) > 0 then
        fEXEName := baseName
      else
        fEXEName := rstSystem;


      if NTQueryInformationProcess (hProcess, ProcessBasicInformation, @processInfo, SizeOf (processInfo), nil) = 0 then
      begin
        fParentProcessID := processInfo.InheritedFromUniqueProcessId;
        fBasePriority := processInfo.BasePriority
      end
    finally
      CloseHandle (hProcess)
    end
  end
end;

destructor TProcess.Destroy;
begin
  fModules.Free;

  inherited;
end;

function TProcess.GetBaseModule: TModule;
begin
  if ModuleCount > 0 then
    result := Module [0]
  else
    result := Nil
end;

function TProcess.GetModule(idx: Integer): TModule;
begin
  result := TModule (fModules [idx])
end;

function TProcess.GetModuleCount: Integer;
begin
  result := fModules.Count
end;

{ TProcessSnapshot }

constructor TProcessSnapshot.Create;
begin
  inherited Create (Aowner);
  fSortColumn := psPID;
  fProcesses := TObjectList.Create;
  if not (csDesigning in ComponentState) then
    LoadEnumerator;
end;

destructor TProcessSnapshot.Destroy;
begin
  fProcesses.Free;

  inherited;
end;

function TProcessSnapshot.GetProcess(idx: Integer): TProcess;
begin
  result := TProcess (fProcesses [idx]);
end;

function TProcessSnapshot.GetProcessCount: Integer;
begin
  result := fProcesses.Count
end;

procedure TProcessSnapshot.Loaded;
begin
  inherited;

  LoadEnumerator;
end;

procedure TProcessSnapshot.LoadEnumerator;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  gUsesToolHelp := False;
  gUsesPSAPI := False;
  gCheckedToolHelp := False;
  gCheckedPSAPI := False;

  case ProcessEnumerator of
    peAuto: if not UsesPSAPI and not UsesToolHelp then
              raise EProcessSnapshot.Create (rstCantSnapshot);
    pePSAPI    : CheckPSAPI;
    peToolHelp : CheckToolHelp;
  end;

  TakeSnapshot;
end;

procedure TProcessSnapshot.SetProcessEnumerator(
  const Value: TProcessEnumerator);
begin
  if Value <> fProcessEnumerator then
  begin
    fProcessEnumerator := Value;

    if not (csLoading in ComponentState) then
      LoadEnumerator;
  end
end;

function CompareProcesses (p1, p2 : Pointer) : Integer;
var
  pr1, pr2 : TProcess;
  m1, m2 : TModule;
  column : TPSSortColumn;
  direction : TPSSortDirection;
begin
  pr1 := TProcess (p1);
  pr2 := TProcess (p2);
  m1 := pr1.BaseModule;
  m2 := pr2.BaseModule;

  column := pr1.fOwner.fSortColumn;
  direction := pr1.fOwner.fSortDirection;

  result := 99;
  if (column in [psDate, psSize]) and ((m1 = Nil) or (m2 = Nil)) then
  begin
    if m1 = Nil then
      if m2 = Nil then
        result := 0
      else
        result := -1
    else
      if m2 = Nil then
        result := 1
  end;


  if result = 99 then
  case column of
    psPID : result := pr1.ProcessID - pr2.ProcessID;
    psParentPID : result := pr1.ParentProcessID - pr2.ParentProcessID;
    psBasePriority : result := pr1.BasePriority - pr2.BasePriority;
    psName : result := CompareText (pr1.EXEName, pr2.EXEName);
    psDate : result := CompareDateTime (m1.FileDateTime, m2.FileDateTime);
    psSize : result := m1.FileSize - m2.FileSize
    else
      result := 0
  end;

  if direction = psDescending then
    result := -result
end;

procedure TProcessSnapshot.SortProcesses (column : TPSSortColumn; direction : TPSSortDirection);
begin
  fSortColumn := column;
  fSortDirection := direction;
  fProcesses.Sort(CompareProcesses);
end;

procedure TProcessSnapshot.TakeSnapshot;
var
  buffer, p : PDWORD;
  bufLen : DWORD;
  i, count : Integer;
  hSnapshot : THandle;
  processEntry : TProcessEntry32;
begin
  if csDesigning in ComponentState then
    Exit;

  fProcesses.Clear;
  if gUsesToolhelp then
  begin
    hSnapshot := TlHelp32.CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnapshot <> INVALID_HANDLE_VALUE then
    try
      processEntry.dwSize := sizeof (processEntry);

      if Process32First (hSnapshot, processEntry) then
      repeat
        fProcesses.Add(TProcess.Create(self, @processEntry))
      until not Process32Next (hSnapshot, processEntry)
    finally
      Closehandle (hSnapshot)
    end
  end
  else
  begin
    buffer := Nil;
    try
      ReallocMem (buffer, 1024*1024);
      if not EnumProcesses (buffer, 1024*1024, bufLen) then bufLen := 0;
      ReallocMem (buffer, bufLen);
      count := bufLen div sizeof (DWORD);

      p := buffer;
      for i := 0 to count - 1 do
      begin
        fProcesses.Add(TProcess.Create(self, p^));
        Inc (p)
      end
    finally
      ReallocMem (buffer, 0)
    end
  end;

  SortProcesses (fSortColumn, fSortDirection);
end;

{ TModule }

constructor TModule.Create(AOwner: TProcess; fInitDetails: PModuleEntry32);
begin
  fVersionCache := '~';
  fOwner := AOwner;
  fFileName := fInitDetails.szExePath;
  fhModule := fInitDetails.hModule;
end;

constructor TModule.Create(AOwner: TProcess; hOwner, hModule: THandle);
var
  szfileName : array [0..MAX_PATH] of char;
begin
  fVersionCache := '~';
  fOwner := AOwner;
  fhModule := hModule;
  GetModuleFileNameEx (hOwner, hModule, szFileName, sizeof (szFileName));
  fFileName := szFilename
end;

function TModule.GetFileDateTime: TDateTime;
begin
  GetFileDetails;
  result := fFileDateTime;
end;

procedure TModule.GetFileDetails;
var
  f : TSearchRec;
begin
  if fVersionCache = '~' then
  begin
    fVersionCache := GetFileVersion (FileName);
    if FindFirst (FileName, faAnyFile, f) = 0 then
    try
      fFileDateTime := FileDateToDateTime (f.Time);
      fFileSize := f.Size
    finally
      FindClose (f)
    end
  end
end;

function TModule.GetFileName: string;
var
  sysDir : string;
begin
  result := fFileName;
  if Copy (result, 1, 4) = '\??\' then
    Delete (result, 1, 4);

  if Copy (result, 1, 11) = '\SystemRoot' then
  begin
    SetLength (sysDir, MAX_PATH);
    GetWindowsDirectory (PChar (sysDir), MAX_PATH);
    sysDir := PChar (sysDir);
    result := sysDir + Copy (result, 12, MaxInt)
  end
end;

function TModule.GetFileSize: DWORD;
begin
  GetFileDetails;
  result := fFileSize;
end;

function TModule.GetVersion: string;
begin
  GetFileDetails;
  result := fVersionCache;
end;

initialization
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
    gOldState := EnableNTPrivilege (SE_DEBUG_NAME, SE_PRIVILEGE_ENABLED);
finalization
  if gOldState <> $badf00d then
    EnableNTPrivilege (SE_DEBUG_NAME, gOldState)
end.