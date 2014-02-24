unit unitLog;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, XnClasses;

procedure CloseLogFile;
procedure LogMessage(msg: string; Force: Boolean = False; AddCRLF: Boolean = True);

implementation

uses
  NewsGlobals, unitNNTPServices;

var
  crit: TCriticalSection = nil;
  lf: TFileStream = nil;

procedure CloseLogFile;
begin
  FreeAndNil(lf);
end;

procedure LogMessage(msg: string; Force: Boolean = False; AddCRLF: Boolean = True);
var
  dt: TDateTime;
  raw: RawByteString;
begin
  if (Force or gLogFlag) and Assigned(crit) then
  begin
    try
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
            // Create a new log file and directly free the reference to it with the "bad" access rights.
            TFileStream.Create(gMessageBaseRoot + '\log.txt', fmCreate).Free;
          end;
          lf := TFileStream.Create(gMessageBaseRoot + '\log.txt', fmOpenReadWrite or fmShareDenyNone);
          lf.Seek(0, soEnd);
        end;

        msg := FormatDateTime('yyyymmdd hh:nn:ss.zzz', Now) + ' TID=' + IntToStr(GetCurrentThreadID) + '- ' + msg;
        if AddCRLF then
          msg := msg +  #13#10;

        raw := RawByteString(msg);
        lf.Write(raw[1], Length(raw));
      finally
        crit.Leave;
      end;
    except
      // eat log errors.
    end;
  end;
end;

initialization
  crit := TCriticalSection.Create;
finalization
  FreeAndNil(crit);
  CloseLogFile;
end.
