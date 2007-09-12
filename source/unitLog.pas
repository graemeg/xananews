unit unitLog;

interface

uses Windows, Classes, SysUtils, SyncObjs;

procedure LogMessage (msg : string);

implementation

uses NewsGlobals, unitNNTPServices;

var
  crit : TCriticalSection = Nil;
  lf : TFileStream = Nil;

procedure LogMessage (msg : string);
var
  dt : TDateTime;
begin
  if not gLogFlag then Exit;
  if not Assigned (crit) then Exit;
  crit.Enter;
  try
    if not Assigned (lf) then
    begin
      if not FileAge (gXanaNewsDir + '\log.txt', dt) then
        dt := -1;

      if Trunc (dt) <> Trunc (Now) then
      begin
        if dt <> -1 then
          RenameFile (gXanaNewsDir + '\log.txt', gXanaNewsDir + '\log-' + FormatDateTime ('dd-mm', dt) + '.txt');
        lf := TFileStream.Create(gXanaNewsDir + '\log.txt', fmCreate);
        FreeAndNil (lf)
      end;
      lf := TFileStream.Create (gXanaNewsDir + '\log.txt', fmOpenReadWrite or fmShareDenyNone);
      lf.Seek(0, soFromEnd)
    end;

    msg := FormatDateTime ('hh:nn:ss:zzzz', now) + ' TID=' + IntToStr (GetCurrentThreadID) + '- ' + msg + #13#10;
    lf.Write(msg [1], Length (msg))
  finally
    crit.Leave
  end
end;

initialization
  crit := TCriticalSection.Create;
finalization
  FreeAndNil (crit);
  lf.Free
end.
