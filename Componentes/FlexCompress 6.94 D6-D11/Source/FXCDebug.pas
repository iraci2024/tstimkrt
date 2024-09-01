 //==============================================================================
 // Unit name: FXCDebug
 // Copyright 2003 ComponentAce.
 // Debug library, contains time measurement routines.
 //==============================================================================
unit FXCDebug;

{$I FXCVer.inc}

interface

uses Windows, SysUtils, Dialogs;

var
  DebugOff:     boolean = False;
  DebugStarted: boolean = False;

type
  TACRTestTime = record
    Name:      string;
    startTime: cardinal;
    stopTime:  cardinal;
    timeStarted: boolean;
    timeRestarted: boolean;
  end;


var
  startTime, stopTime: cardinal;
  timeStarted:   boolean = False; // if true - time is counting, else - pause
  timeRestarted: boolean = False; // if true - restart time counting

var
  logFileName: string;

const
  logName = 's:\FxcLog.txt';

// gets counted time in milliseconds (based on GetTickCount)
function aaGetTime: cardinal; overload;
function aaGetTime(var TimeRec: TACRTestTime): cardinal; overload;
// inits time counting
procedure aaInitTime; overload;
procedure aaInitTime(var TimeRec: TACRTestTime); overload;
// starts time counting from current time
procedure aaStartTime; overload;
procedure aaStartTime(var TimeRec: TACRTestTime); overload;
// stops time counting
procedure aaStopTime; overload;
procedure aaStopTime(var TimeRec: TACRTestTime); overload;
// shows time
procedure aaShowTime; overload;
procedure aaShowTime(var TimeRec: TACRTestTime); overload;
// write time to log
procedure aaWriteTime; overload;
procedure aaWriteTime(var TimeRec: TACRTestTime); overload;


 // writes string to log file
procedure aaWriteToLog(s: string); // writes string to log file
 // delete log
procedure EmptyLog;

implementation

 //-------------------------------- DEBUG ---------------------------------------
 // gets counted time in milliseconds (based on GetTickCount)
function aaGetTime: cardinal;
begin
  if (timeStarted) then
    Result := GetTickCount - startTime
  else
    Result := stopTime - startTime;
end; // aaGetTime

function aaGetTime(var TimeRec: TACRTestTime): cardinal;
begin
  if (TimeRec.timeStarted) then
    Result := GetTickCount - TimeRec.startTime
  else
    Result := TimeRec.stopTime - TimeRec.startTime;
end; // aaGetTime



// inits time counting
procedure aaInitTime;
begin
  timeRestarted := True;
  startTime     := 0;
  stopTime      := 0;
  timeStarted   := False;
end;


// inits time counting
procedure aaInitTime(var TimeRec: TACRTestTime);
begin
  TimeRec.timeRestarted := True;
  TimeRec.startTime     := 0;
  TimeRec.stopTime      := 0;
  TimeRec.timeStarted   := False;
end;


// starts time counting from current time
procedure aaStartTime;
begin
  if (timeRestarted) then
  begin
    startTime     := GetTickCount;
    timeRestarted := False;
    timeStarted   := True;
  end
  else
  if (not timeStarted) then
  begin
    startTime := startTime + GetTickCount - stopTime;
  end;
end;


// starts time counting from current time
procedure aaStartTime(var TimeRec: TACRTestTime);
begin
  if (TimeRec.timeRestarted) then
  begin
    TimeRec.startTime     := GetTickCount;
    TimeRec.timeRestarted := False;
    TimeRec.timeStarted   := True;
  end
  else
  if (not TimeRec.timeStarted) then
  begin
    TimeRec.startTime := TimeRec.startTime + GetTickCount - TimeRec.stopTime;
  end;
end;


// stops time counting
procedure aaStopTime;
begin
  timeStarted := False;
  stopTime    := GetTickCount;
end;


// stops time counting
procedure aaStopTime(var TimeRec: TACRTestTime);
begin
  TimeRec.timeStarted := False;
  TimeRec.stopTime    := GetTickCount;
end;


// shows time
procedure aaShowTime;
begin
  ShowMessage('time = ' + IntToStr(aaGetTime));
end;


// shows time
procedure aaShowTime(var TimeRec: TACRTestTime);
begin
  ShowMessage(TimeRec.Name + IntToStr(aaGetTime(TimeRec)));
end;


// write time to log
procedure aaWriteTime;
begin
  aaWriteToLog('time = ' + IntToStr(aaGetTime));
end;


procedure aaWriteTime(var TimeRec: TACRTestTime);
begin
  aaWriteToLog(TimeRec.Name + IntToStr(aaGetTime(TimeRec)));
end;


 //-------------------------------- DEBUG ---------------------------------------
 // writes string to log file

procedure aaWriteToLog(s: string);
var
  f: Text;
begin
{$IFNDEF DEBUG_LOG}
  Exit;
{$ENDIF}
  if (DebugOff) then
    Exit;

  Assign(f, logFileName);
  if (FileExists(logFileName)) then
    Append(f)
  else
    ReWrite(f);
  Writeln(f, s);
  Close(f);
end;


procedure EmptyLog;
var
  f: Text;
begin
  Assign(f, logFileName);
  ReWrite(f);
  Close(f);
end;

initialization

  // logFileName := GetCurrentDir+'\'+logName;
  logFileName := logName;
  // Delete Old Log !
  DeleteFile(logFileName);

end.
