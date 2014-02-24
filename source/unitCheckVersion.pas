unit unitCheckVersion;

interface

uses Windows, Dialogs, Classes, SysUtils, Forms, SyncObjs, XnClasses;

type
  TGetVersionThread = class(TXnThread)
  private
    fDiallupTrigger: TEvent;
    fPermissionGranted: boolean;
    procedure GetPermission;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DiallupTrigger;
    procedure Terminate;
  end;

var
  gGetVersionThread: TGetVersionThread;

implementation

uses WinINet, unitSearchString, NewsGlobals, unitNNTPServices, NewVersionDialog;

resourcestring
  rstBadVersionInfo = 'Unable to get XanaNews version information from the Internet';

function GetXanaNewsVersionFromInternet(var contributors : string) : string;
var
  handle: hInternet;
  hurl: hInternet;
  bytesRead: DWORD;
  Buffer: AnsiString;
begin
  hurl := nil;
  handle := InternetOpen('XanaNews', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    if handle = nil then RaiselastOSError;

    hurl := InternetOpenURL(handle, 'http://xananews.techtips.com.br/xananews.txt', nil, 0,
                                    INTERNET_FLAG_EXISTING_CONNECT or
                                    INTERNET_FLAG_NO_CACHE_WRITE or
                                    INTERNET_FLAG_RESYNCHRONIZE or
                                    INTERNET_FLAG_NO_UI or
                                    INTERNET_FLAG_PRAGMA_NOCACHE or
                                    INTERNET_FLAG_RELOAD
                                    , 0);
    if hurl = nil then RaiseLastOSError;

    SetLength(Buffer, 256);
    if InternetReadFile(hurl, PAnsiChar(Buffer), 256, bytesRead) then
    begin
      SetLength(Buffer, bytesRead);
      Result := Trim(string(Buffer));
      if CompareText(SplitString(':', Result), 'version') <> 0 then
        raise Exception.Create(rstBadVersionInfo);
    end;
    InternetCloseHandle(hurl);

    hurl := InternetOpenURL(handle, 'http://xananews.techtips.com.br/contributors.txt', nil, 0,
                                    INTERNET_FLAG_EXISTING_CONNECT or
                                    INTERNET_FLAG_NO_CACHE_WRITE or
                                    INTERNET_FLAG_RESYNCHRONIZE, 0);
    if hurl <> nil then
    begin
      SetLength(buffer, 131072);
      if InternetReadFile(hurl, PAnsiChar(buffer), 131072, bytesRead) then
      begin
        SetLength(buffer, bytesRead);
        contributors := Trim(string(buffer));
      end;
    end;
  finally
    if Assigned(hurl) then InternetCloseHandle(hurl);
    if Assigned(handle) then InternetCloseHandle(handle);
  end;
end;

{ TGetVersionThread }

constructor TGetVersionThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TGetVersionThread.Destroy;
begin
  fDiallupTrigger.Free;
  gGetVersionThread := nil;
  inherited Destroy;
end;

procedure TGetVersionThread.DiallupTrigger;
begin
  if Assigned(fDiallupTrigger) then
    fDiallupTrigger.SetEvent;
end;

procedure TGetVersionThread.Execute;
var
  retryCount: Integer;
  contributors: string;
  LastTimeChecked: TDateTime;
begin
  contributors := '~';
  retryCount := 3;
  LastTimeChecked := 0;
  Sleep(2000);
  if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
    SendMessage(Application.MainForm.Handle, WM_NAMETHREAD, ThreadID, LPARAM(PChar('Get Version Thread')));
  Synchronize(GetPermission);
  if not fPermissionGranted then
    SetLatestVersion('')
  else
  begin
    fDiallupTrigger := TEvent.Create(nil, False, False, '');
    while not Terminated do
    begin
      if (Now - LastTimeChecked) > 1/24 then
        try
          if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
            if SendMessage(Application.MainForm.Handle, WM_GETCONNECTED, 0, 0) <> 0 then
            begin
              SetLatestVersion(GetXanaNewsVersionFromInternet(contributors));
              SetDeserveMedals(contributors);
            end;
        except
        end;

      if GetLatestVersion <> '~' then
        retryCount := 0;

      if retryCount = 0 then
        LastTimeChecked := Now;

      if fDiallupTrigger.WaitFor(5 * 60 * 1000) = wrSignaled then
      begin       // They dialled up.  So try to find the version more
                  // agressively for a bit.
        if Terminated then Break;
        retryCount := 3;
      end
      else        // Timeout.
        if retryCount > 0 then
          Dec(retryCount);
    end;
  end;
end;

procedure TGetVersionThread.GetPermission;
var
  req: Integer;
  dlg: TfmNewVersionNotification;
begin
  if not Assigned(NNTPAccounts) then Exit;
  if NNTPAccounts.fNewUserFlag then
  begin
    fPermissionGranted := False;
    Exit;
  end;

  req := NNTPAccounts.DoVersionCheck;

  if req = 0 then
  begin
    dlg := TfmNewVersionNotification.Create(nil);
    try
      if dlg.ShowModal = idyes then
        req := 2
      else
        req := 1;

      if dlg.CheckBox1.Checked then
        NNTPAccounts.DoVersionCheck := req;
    finally
      dlg.Free;
    end;
  end;

  fPermissionGranted := req = 2;
end;

procedure TGetVersionThread.Terminate;
begin
  inherited Terminate;
  DiallupTrigger;
end;

end.
