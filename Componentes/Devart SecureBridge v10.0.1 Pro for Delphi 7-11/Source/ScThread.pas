
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScThread;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFDEF VER17P}
  Types,
{$ENDIF}
{$IFNDEF SBRIDGE}
  CRTypes, CRFunctions,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsUtils;
{$ELSE}
  TdsSSLConstsUni, TdsUtilsUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScFunctions,
  ScConsts, ScUtils;
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  TScTimer = class(TComponent)
  private
    FInterval: Cardinal;
    FWindowHandle: HWND;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;
{$ENDIF}

  TScIntervalThread = class(TThread)
  private
    FIntervalEvent: TEvent;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure UpdateThread;

  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  TScIntervalProcessor = class
  private
  {$IFDEF MSWINDOWS}
    FTimer: TScTimer;
  {$ENDIF}
    FIntervalThread: TScIntervalThread;

    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetOnTimer: TNotifyEvent;
    procedure SetOnTimer(Value: TNotifyEvent);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
  end;

  TScAsyncEventProcessor = class
  private
  {$IFDEF MSWINDOWS}
    FTimer: TScTimer;
  {$ENDIF}
    FEventHandlerThread: TThread;
    FDoTimerProcessing: boolean;
    FEventListLock: TCriticalSection;
    FHandleEvent: TEvent;
    FAddListEvent: TEvent;
    FEventList: array of TMethod;
    FProcessingEvent: TMethod;
    FProcessingThreadId: TThreadID;
    FEventCount: integer;

  protected
    procedure ProcessEvent;
    procedure HandleLeadEvent(NeedSync: boolean);
    procedure DoTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure HandleEventAsync(Event: TThreadMethod);
    procedure DisposeAsyncEvent(Event: TThreadMethod);
  end;

  TScAsyncEvent = class
  protected
    FEventsCallMode: TScEventCallMode;

    procedure InternalNotify; virtual; abstract;
    procedure Notify; overload;
  public
    destructor Destroy; override;
    procedure Notify(EventsCallMode: TScEventCallMode); overload;
  end;

procedure CheckIfAsyncEventProcessorStarted;
procedure HandleEventAsync(Event: TThreadMethod);
procedure DisposeAsyncEvent(Event: TThreadMethod);

var
  UseAsyncEventProcessor: boolean;

implementation

type
  TScEventHandlerThread = class(TThread)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScAsyncEventProcessor;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TScAsyncEventProcessor);
    destructor Destroy; override;
  end;

var
  AsyncEvents: TCRThreadList;
  AsyncEventProcessor: TScAsyncEventProcessor;
  LockCreate: TCriticalSection;

{ TScIntervalProcessor }

constructor TScIntervalProcessor.Create(AOwner: TComponent);
begin
  inherited Create;

{$IFDEF MSWINDOWS}
  if not IsConsole then
    FTimer := TScTimer.Create(AOwner)
  else
{$ENDIF}
    FIntervalThread := TScIntervalThread.Create(AOwner);
end;

destructor TScIntervalProcessor.Destroy;
begin
{$IFDEF MSWINDOWS}
  FTimer.Free;
{$ENDIF}
  FIntervalThread.Free;
  inherited;
end;

function TScIntervalProcessor.GetEnabled: Boolean;
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    Result := FTimer.Enabled
  else
{$ENDIF}
    Result := FIntervalThread.Enabled;
end;

procedure TScIntervalProcessor.SetEnabled(Value: Boolean);
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    FTimer.Enabled := Value
  else
{$ENDIF}
    FIntervalThread.Enabled := Value;
end;

function TScIntervalProcessor.GetInterval: Cardinal;
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    Result := FTimer.Interval
  else
{$ENDIF}
    Result := FIntervalThread.Interval;
end;

procedure TScIntervalProcessor.SetInterval(Value: Cardinal);
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    FTimer.Interval := Value
  else
{$ENDIF}
    FIntervalThread.Interval := Value;
end;

function TScIntervalProcessor.GetOnTimer: TNotifyEvent;
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    Result := FTimer.OnTimer
  else
{$ENDIF}
    Result := FIntervalThread.OnTimer;
end;

procedure TScIntervalProcessor.SetOnTimer(Value: TNotifyEvent);
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    FTimer.OnTimer := Value
  else
{$ENDIF}
    FIntervalThread.OnTimer := Value;
end;

{ TScIntervalThread }

constructor TScIntervalThread.Create(AOwner: TComponent);
begin
  FIntervalEvent := CreateEvent;
  FEnabled := True;
  FInterval := 1000;
  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TScIntervalThread.Destroy;
begin
  Terminate;
  FEnabled := False;
  FIntervalEvent.SetEvent;
  inherited;
  FIntervalEvent.Free;
end;

procedure TScIntervalThread.UpdateThread;
begin
  if (FInterval > 0) and FEnabled and Assigned(FOnTimer) then
    FIntervalEvent.SetEvent;
end;

procedure TScIntervalThread.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    UpdateThread;
  end;
end;

procedure TScIntervalThread.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then begin
    FInterval := Value;
    UpdateThread;
  end;
end;

procedure TScIntervalThread.SetOnTimer(Value: TNotifyEvent);
begin
  if @Value <> @FOnTimer then begin
    FOnTimer := Value;
    UpdateThread;
  end;
end;

procedure TScIntervalThread.Execute;
begin
  try
    while not Terminated do begin
      if (FInterval = 0) or not FEnabled or not Assigned(FOnTimer) then
        FIntervalEvent.WaitFor(INFINITE)
      else begin
        case FIntervalEvent.WaitFor(FInterval) of
          wrTimeout:
            if FEnabled and Assigned(FOnTimer) then
              FOnTimer(Self);
        end;
      end;

      FIntervalEvent.ResetEvent;
    end;
  except
    on E: Exception do
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(E)
      else
        ShowException(E, ExceptAddr);
  end;
end;

{$IFDEF MSWINDOWS}
function TimerWndProc(Window: HWND; Message: UINT; WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
var
  Timer: TScTimer;
begin
  if Message = WM_TIMER then begin
    Result := 1;
  {$IFDEF CPU64}
    Timer := TScTimer(GetWindowLongPtr(Window, GWL_USERDATA));
  {$ELSE}
    Timer := TScTimer(GetWindowLong(Window, GWL_USERDATA));
  {$ENDIF}
    try
      Timer.Timer;
    except
      ApplicationHandleException(Timer);
    end;
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

var
  CRTimerWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @TimerWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'CRTimerWindowClass');

{ TScTimer }

constructor TScTimer.Create(AOwner: TComponent);
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;

  // allocate timer window
  CRTimerWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, CRTimerWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> @TimerWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(CRTimerWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(CRTimerWindowClass);
  end;
  FWindowHandle := CreateWindowEx(WS_EX_TOOLWINDOW, CRTimerWindowClass.lpszClassName,
    '', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);

  // pass Self to window
{$IFDEF CPU64}
  SetWindowLongPtr(FWindowHandle, GWL_USERDATA, NativeInt(Pointer(Self)));
{$ELSE}
  SetWindowLong(FWindowHandle, GWL_USERDATA, Integer(Pointer(Self)));
{$ENDIF}
end;

destructor TScTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  DestroyWindow(FWindowHandle);

  inherited;
end;

procedure TScTimer.UpdateTimer;
begin
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers + #13#10 + SysErrorMessage(GetLastError));
end;

procedure TScTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TScTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TScTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TScTimer.Timer;
begin
  if FEnabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;
{$ENDIF}

procedure CheckIfAsyncEventProcessorStarted;
begin
  if AsyncEventProcessor <> nil then
    Exit;

  LockCreate.Acquire;
  try
    if AsyncEventProcessor = nil then
      AsyncEventProcessor := TScAsyncEventProcessor.Create;
  finally
    LockCreate.Release;
  end;
end;

procedure HandleEventAsync(Event: TThreadMethod);
begin
  if AsyncEventProcessor = nil then
    raise Exception.Create(SAsyncEventProcessorNotStarted);

  AsyncEventProcessor.HandleEventAsync(Event);
end;

procedure DisposeAsyncEvent(Event: TThreadMethod);
begin
  if AsyncEventProcessor <> nil then
    AsyncEventProcessor.DisposeAsyncEvent(Event);
end;

{ TScAsyncEventProcessor }

constructor TScAsyncEventProcessor.Create;
{$IFDEF MSWINDOWS}
const
  USER_TIMER_MINIMUM = $A;
var
  CanUseTimer: boolean;
{$ENDIF}
begin
  inherited;

  FEventListLock := TCriticalSection.Create;
  FHandleEvent := CreateEvent;
  FAddListEvent := CreateEvent;
  SetLength(FEventList, 32);
  FEventCount := 0;

{$IFDEF MSWINDOWS}
  if not IsConsole then
    CanUseTimer := True
  else
    CanUseTimer := False;

  if CanUseTimer and IsMainThread then begin
    FTimer := TScTimer.Create(nil);
    FTimer.Enabled := True;
    FTimer.Interval := USER_TIMER_MINIMUM;
    FTimer.OnTimer := DoTimer;
  end
  else
{$ENDIF}
    FEventHandlerThread := TScEventHandlerThread.Create(Self);
end;

destructor TScAsyncEventProcessor.Destroy;
begin
{$IFDEF MSWINDOWS}
  FTimer.Free;
{$ENDIF}
  FEventHandlerThread.Free;
  FEventListLock.Free;
  FHandleEvent.Free;
  FAddListEvent.Free;

  inherited;
end;

procedure TScAsyncEventProcessor.HandleEventAsync(Event: TThreadMethod);
var
  i: integer;
begin
  if not Assigned(Event) then
    Exit;

  FEventListLock.Acquire;
  try
    for i := 0 to FEventCount - 1 do begin
      if CompareMethods(FEventList[i], TMethod(Event)) then
        Exit;
    end;

    if FEventCount >= Length(FEventList) then
      SetLength(FEventList, FEventCount shl 1);

    FEventList[FEventCount].Code := TMethod(Event).Code;
    FEventList[FEventCount].Data := TMethod(Event).Data;
    Inc(FEventCount);
    FAddListEvent.SetEvent;
  finally
    FEventListLock.Release;
  end;
end;

procedure TScAsyncEventProcessor.DisposeAsyncEvent(Event: TThreadMethod);
var
  i: integer;
begin
  FEventListLock.Acquire;
  try
    i := 0;
    while i < FEventCount do begin
      if CompareMethods(FEventList[i], TMethod(Event)) then begin
        Dec(FEventCount);
        if i < FEventCount then
          Move(FEventList[i + 1], FEventList[i], (FEventCount - i) * SizeOf(TMethod));
        Break;
      end
      else
        Inc(i);
    end;

    if CompareMethods(FProcessingEvent, TMethod(Event)) then begin
      if (FProcessingThreadId = {$IFDEF DARWIN}nil{$ELSE}0{$ENDIF}) or (FProcessingThreadId <> GetCurrentThreadId) then
        if FHandleEvent.WaitFor(INFINITE) <> wrSignaled then
          raise Exception.Create('Timeout expired');
    end;
  finally
    FEventListLock.Release;
  end;
end;

procedure TScAsyncEventProcessor.HandleLeadEvent(NeedSync: boolean);
begin
  FEventListLock.Acquire;
  try
    if FEventCount > 0 then begin
      FProcessingEvent := FEventList[0];
      FProcessingThreadId := {$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
      Dec(FEventCount);
      Move(FEventList[1], FEventList[0], FEventCount * SizeOf(TMethod));

      FHandleEvent.ResetEvent;
    end
    else begin
      FAddListEvent.ResetEvent;
      Exit;
    end;
  finally
    FEventListLock.Release;
  end;

  try
    try
      if NeedSync then
        SynchronizeWithMainThread(ProcessEvent)
      else
        ProcessEvent;
    except
      on E: Exception do
        if not (E is EAbort) then
          if Assigned(ApplicationHandleException) then
            ApplicationHandleException(E)
          else
            ShowException(E, ExceptAddr);
    end;
  finally
    FProcessingEvent.Code := nil;
    FProcessingEvent.Data := nil;
    FProcessingThreadId := {$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
    FHandleEvent.SetEvent;
  end;
end;

procedure TScAsyncEventProcessor.ProcessEvent;
var
  Event: TThreadMethod;
begin
  Event := TThreadMethod(FProcessingEvent);
  if Assigned(Event) then begin
    FProcessingThreadId := GetCurrentThreadId;
    Event();
  end;
end;

procedure TScAsyncEventProcessor.DoTimer(Sender: TObject);
begin
  if FDoTimerProcessing then
    Exit; // On showing error message, or if any event handler call ProcessMessage
  FDoTimerProcessing := True;

  try
    while FEventCount > 0 do
      HandleLeadEvent(False);
  finally
    FDoTimerProcessing := False;
  end;
end;

{ TScEventHandlerThread }

constructor TScEventHandlerThread.Create(AOwner: TScAsyncEventProcessor);
begin
  FOwner := AOwner;
  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TScEventHandlerThread.Destroy;
begin
  Terminate;
  FOwner.FAddListEvent.SetEvent;
  inherited;
end;

procedure TScEventHandlerThread.Execute;
begin
  while not Terminated do begin
    FOwner.FAddListEvent.WaitFor(INFINITE);
    if not Terminated then
      try
        FOwner.HandleLeadEvent(True);
      except
      end;
  end;
end;

{ TScAsyncEvent }

destructor TScAsyncEvent.Destroy;
begin
  if FEventsCallMode = ecAsynchronous then
    DisposeAsyncEvent(Notify);

  inherited;
end;

procedure TScAsyncEvent.Notify;
begin
  try
    InternalNotify;
  finally
    if AsyncEvents <> nil then
      AsyncEvents.Remove(Self);
  {$IFNDEF NEXTGEN}
    Free;
  {$ENDIF}
  end;
end;

procedure TScAsyncEvent.Notify(EventsCallMode: TScEventCallMode);
begin
  FEventsCallMode := EventsCallMode;

  case EventsCallMode of
    ecDirectly: begin
      InternalNotify;
    {$IFNDEF NEXTGEN}
      Free;
    {$ENDIF}
    end;

    ecSynchronous: begin
      SynchronizeWithMainThread(InternalNotify);
    {$IFNDEF NEXTGEN}
      Free;
    {$ENDIF}
    end;

    ecAsynchronous: begin
      if AsyncEvents <> nil then
        AsyncEvents.Add(Self);
      HandleEventAsync(Notify);
    end;
  else
    Assert(False);
  end;
end;

initialization
  AsyncEvents := TCRThreadList.Create;
  AsyncEventProcessor := nil;
  LockCreate := TCriticalSection.Create;

finalization
  FreeAndNil(AsyncEvents);
  FreeAndNil(AsyncEventProcessor);
  LockCreate.Free;

end.
