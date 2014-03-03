unit unitQpcTimer;

interface

uses
  SysUtils, Windows;

function qpcStartTimer(delay: Extended): Int64;
function qpcTimerDone(timer: Int64): Boolean;
function qpcAdjustTimer(timvar: Int64; sec: Extended): Int64;

implementation

uses
  Forms;

var
  QPF: Int64;

// --- Setup a "timer", <delay> is in seconds
function qpcStartTimer(delay: Extended): Int64;
var
  QPC: Int64;
begin
  QueryPerformanceCounter(QPC);
  Result := QPC + Round(delay * QPF);
end;

// --- Checks if <timer> is expired
function qpcTimerDone(timer: Int64): Boolean;
var
  QPC: Int64;
begin
  QueryPerformanceCounter(QPC);
  Result := (timer = 0) or (QPC >= timer);
end;

// --- Adjusts timer <timvar> by <sec>
function qpcAdjustTimer(timvar: Int64; sec: Extended): Int64;
begin
  if qpcTimerDone(timvar) then
  begin
    if sec > 0 then
      Result := qpcStartTimer(sec)
    else
      Result := timvar;
  end
  else
    Result := timvar + Round(sec * QPF);
end;

initialization
  if not QueryPerformanceFrequency(QPF) then
  begin
    MessageBox(0, 'Hi-resolution timer not available!'#13+
                  'This application will not operate without this timer',
               PChar(Application.Title),
               MB_OK or MB_ICONERROR);
    Abort;
  end;
end.
