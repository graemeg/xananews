unit cmpRunOnce;

interface

uses
  Windows, Messages, SysUtils, classes, Forms;

const
  WM_PARAMS = WM_USER + $200;

type
  TOnOtherInstance = procedure(Sender: TObject; ParamCount: DWORD; ParamStr: array of string) of object;

  TRunOnce = class(TComponent)
  private
    fOtherWindowHandle: HWND;
    fUniqueMessage: DWORD;
    fParamsMessage: DWORD;
    fOldOwnerWindowProc: TFNWndProc;
    fObjectInstance: Pointer;
    fOnOtherInstance: TOnOtherInstance;
    fMutex: THandle;
    fName: string;
    function CheckOtherApp(hWnd: HWND): Boolean;
    procedure OwnerWindowProc(var msg: TMessage);
    procedure ProcessParameters(remoteMemHandle: THandle; remoteProcessID: DWORD);

  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnOtherInstance: TOnOtherInstance read fOnOtherInstance write fOnOtherInstance;
  end;

implementation

type
  PDWORD_PTR = ^DWORD_PTR;

function SendMessageTimeout(hWnd: HWND; msg: UINT; wParam: wParam;
  lParam: lParam; fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT;
  stdcall; external user32 name 'SendMessageTimeoutW';

{ TRunOnce }

function TRunOnce.CheckOtherApp(hWnd: HWND): Boolean;
var
  msgResult: DWORD;
begin
  Result := False;
  if hWnd <> TForm(Owner).Handle then
  begin
    if GetWindowLong(hWnd, GWL_USERDATA) = $BADF00D then
      if (SendMessageTimeout(hWnd, fUniqueMessage, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, 1000, @msgResult) <> 0) and (msgResult = fUniqueMessage) then
      begin
        fOtherWindowHandle := hWnd;
        Result := True;
      end;
  end;
end;

constructor TRunOnce.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRunOnce.Destroy;
begin
  if Assigned(fObjectInstance) then
    Classes.FreeObjectInstance(fObjectInstance);

  if fMutex <> 0 then
  begin
    ReleaseMutex(fMutex);
    CloseHandle(fMutex);
  end;
  inherited Destroy;
end;

function EnumWindowsProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
begin
  Result := not TRunOnce(lParam).CheckOtherApp(hWnd);
end;

procedure TRunOnce.OwnerWindowProc(var msg: TMessage);
begin
  with msg do
    if msg = fUniqueMessage then
      Result := fUniqueMessage
    else if msg = fParamsMessage then
      try
        ProcessParameters(wParam, lParam)
      except
        Application.HandleException(self)
      end
    else
      Result := CallWindowProc(fOldOwnerWindowProc, TForm(Owner).Handle, msg, wParam, lParam);
end;

procedure TRunOnce.Loaded;
var
  mapHandle: THandle;
  paramPtr, p: PChar;
  paramSize: DWORD;
  i: Integer;
begin
  inherited Loaded;

  if not(csDesigning in ComponentState) and (Owner is TForm) then
  begin
    fName := UpperCase(ExtractFileName(Application.Exename));
    fMutex := CreateMutex(nil, True, PChar(fName));
    if GetLastError <> 0 then
    begin
      CloseHandle(fMutex);
      fMutex := 0;
    end;
    fUniqueMessage := RegisterWindowMessage(PChar(fName));
    fParamsMessage := RegisterWindowMessage('WoozleRunOnce');

    fObjectInstance := Classes.MakeObjectInstance(OwnerWindowProc);
    {$IFDEF CPUX64}
      fOldOwnerWindowProc := TFNWndProc(SetWindowLongPtr(TForm(Owner).Handle, GWL_WNDPROC, LONG_PTR(fObjectInstance)));
    {$ELSE}
      fOldOwnerWindowProc := TFNWndProc(SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LPARAM(fObjectInstance)));
    {$ENDIF}
    if fMutex = 0 then
    begin
      Sleep(100);
      EnumWindows(@EnumWindowsProc, LPARAM(Self));

      if fOtherWindowHandle <> 0 then
      begin
        paramSize := 1;
        for i := 0 to ParamCount do
          Inc(paramSize, 1 + Length(ParamStr(i)));
        mapHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, 65536, nil);
        if mapHandle <> 0 then
          try
            paramPtr := MapViewOfFile(mapHandle, FILE_MAP_WRITE, 0, 0,
              paramSize * SizeOf(Char));
            if paramPtr <> nil then
              try
                p := paramPtr;
                for i := 0 to ParamCount do
                begin
                  lstrcpy(p, PChar(ParamStr(i)));
                  Inc(p, Length(ParamStr(i)) + 1);
                end;
                p^ := #0;
              finally
                UnmapViewOfFile(paramPtr);
              end
            else
              RaiseLastOSError;

            SendMessage(fOtherWindowHandle, fParamsMessage, mapHandle, GetCurrentProcessID);
          finally
            CloseHandle(mapHandle);
          end
        else
          RaiseLastOSError;

        SetForegroundWindow(fOtherWindowHandle);
      end;
      Halt;
    end
    else
      SetWindowLongPtr(TForm(Owner).Handle, GWL_USERDATA, $BADF00D)
  end
end;

procedure TRunOnce.ProcessParameters(remoteMemHandle: THandle; remoteProcessID: DWORD);
var
  memHandle: THandle;
  remoteProcessHandle: THandle;
  paramPtr: PChar;
  p: PChar;
  ParamCount: DWORD;
  params: array of string;
  i: Integer;
begin
  remoteProcessHandle := OpenProcess(PROCESS_DUP_HANDLE, False, remoteProcessID);
  if remoteProcessHandle <> 0 then
    try
      if DuplicateHandle(remoteProcessHandle, remoteMemHandle, GetCurrentProcess, @memHandle, FILE_MAP_READ, False, 0) then
        try
          paramPtr := MapViewOfFile(memHandle, FILE_MAP_READ, 0, 0, 65536);
          if paramPtr <> nil then
            try
              if Assigned(fOnOtherInstance) and not (csDestroying in ComponentState) then
              begin
                p := paramPtr;
                ParamCount := 0;
                while p^ <> #0 do
                begin
                  Inc(ParamCount);
                  Inc(p, lstrlen(p) + 1);
                end;
                SetLength(params, ParamCount);
                p := paramPtr;
                i := 0;
                while p^ <> #0 do
                begin
                  params[i] := p;
                  Inc(p, lstrlen(p) + 1);
                  Inc(i);
                end;

                OnOtherInstance(self, ParamCount - 1, params);
              end;
            finally
              UnmapViewOfFile(paramPtr);
            end
          else
            RaiseLastOSError;
        finally
          CloseHandle(memHandle);
        end
      else
        RaiseLastOSError;
    finally
      CloseHandle(remoteProcessHandle);
    end
  else
    RaiseLastOSError;
end;

end.
