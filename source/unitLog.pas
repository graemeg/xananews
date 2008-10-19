unit unitLog;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, XnClasses;

procedure LogMessage(msg: string; Force: Boolean = False);

implementation

uses
  NewsGlobals, unitNNTPServices;

var
  crit: TCriticalSection = nil;
  lf: TFileStream = nil;

procedure LogMessage(msg: string; Force: Boolean = False);
var
  dt: TDateTime;
  raw: MessageString;
begin
  if (Force or gLogFlag) and Assigned(crit) then
  begin
    crit.Enter;
    try
      if not Assigned(lf) then
      begin
        ForceDirectories(gMessageBaseRoot);
        if not FileAge(gMessageBaseRoot + '\log.txt', dt) then
          dt := -1;

        if Trunc(dt) <> Trunc(Now) then
        begin
          if dt <> -1 then
            RenameFile(gMessageBaseRoot + '\log.txt', gMessageBaseRoot + '\log-' + FormatDateTime('yyyymmdd', dt) + '.txt');
          lf := TFileStream.Create(gMessageBaseRoot + '\log.txt', fmCreate);
          FreeAndNil(lf);
        end;
        lf := TFileStream.Create(gMessageBaseRoot + '\log.txt', fmOpenReadWrite or fmShareDenyNone);
        lf.Seek(0, soEnd);
      end;

      msg := FormatDateTime('hh:nn:ss:zzzz', Now) + ' TID=' + IntToStr(GetCurrentThreadID) + '- ' + msg + #13#10;

      raw := MessageString(msg);
      lf.Write(raw[1], Length(raw));
    finally
      crit.Leave;
    end;
  end;
end;

initialization
  crit := TCriticalSection.Create;
finalization
  FreeAndNil(crit);
  lf.Free;
end.
