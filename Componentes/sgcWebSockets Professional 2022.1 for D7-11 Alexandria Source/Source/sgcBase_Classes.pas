{ ***************************************************************************
  sgcBase component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcBase_Classes;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdThread{$ELSE}IdThread{$ENDIF},
  // sgcWebSocket
  sgcWebSocket_Types, sgcWebSocket_Helpers;


type
  TsgcComponent_Base = class(TComponent)
  private
    function GetVersion: String;
  protected
    function IsDesigning: Boolean; virtual;
    function IsDestroying: Boolean; virtual;
    function IsLoading: Boolean; virtual;
  public
    property Version: String read GetVersion;
  end;

type
  TsgcTimerOnException = procedure(Sender: TObject; E: Exception) of object;

  TsgcIdThread = class(TIdThread)
  private
    FDebugName: string;
    FThreadId: Cardinal;
  protected
    FIsExecuting: Boolean;
  protected
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True; ALoop: Boolean = True;
        {$IFDEF INDY10_2}const {$ENDIF}AName: string = ''); override;
  public
    function IsCurrentThread: Boolean;
    property DebugName: string read FDebugName write FDebugName;
    property IsExecuting: Boolean read FIsExecuting;
  end;

  TsgcTimer = class(TsgcIdThread)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FNotifyEvents: TwsNotifyEvent;
    FOnException: TsgcTimerOnException;
    FOnTimer: TNotifyEvent;
    {$IFDEF DEBUG}
    FSetThreadName: Boolean;
    {$ENDIF}
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure DoOnTimerEvent; virtual;
    procedure DoOnExceptionEvent(const E: Exception); virtual;
  protected
    procedure Run; override;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
  public
    property Interval: Integer read FInterval write FInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property NotifyEvents: TwsNotifyEvent read FNotifyEvents
      write FNotifyEvents;
  public
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  public
    property OnException: TsgcTimerOnException read FOnException
      write FOnException;
  End;

type
  TsgcThreadSafeBase = class
  private
    {$IFDEF MSWINDOWS}
    FCS: TRTLCriticalSection;
    {$ELSE}
    FCS: TCriticalSection;
    {$ENDIF}
  protected
    procedure DoEnterCS;
    procedure DoLeaveCS;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type
  TsgcThreadSafeComponent = class(TComponent)
  private
    {$IFDEF MSWINDOWS}
    FCS: TRTLCriticalSection;
    {$ELSE}
    FCS: TCriticalSection;
    {$ENDIF}
  protected
    procedure DoEnterCS;
    procedure DoLeaveCS;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  procedure sgcThreadFree({$IFDEF D10_4}const [ref] aThread:
      TsgcIdThread{$ELSE}var aThread{$ENDIF});

implementation

uses
  sgcBase_Helpers,
  {$IFDEF SGC_DEBUG}sgcTCP_Classes,{$ENDIF}
  sgcBase_Const;

procedure sgcThreadFree({$IFDEF D10_4}const [ref] aThread:
    TsgcIdThread{$ELSE}var aThread{$ENDIF});
var
  i: Integer;
begin
  if Assigned(TsgcIdThread(aThread)) then
  begin
    {$IFDEF SGC_DEBUG}
    DoLog(nil, nil, 'sgcThreadFree_Start');
    {$ENDIF}
    if not TsgcIdThread(aThread).IsCurrentThread and not TsgcIdThread(aThread).IsExecuting then
    begin
      if not TsgcIdThread(aThread).Terminated then
      begin
        TsgcIdThread(aThread).Terminate;
        i := 0;
        repeat
          sleep(1);
          inc(i);
        until
          TsgcIdThread(aThread).Terminated or (i > 1000);
      end;
      sgcFree(aThread);
    end
    else
    begin
      TsgcIdThread(aThread).FreeOnTerminate := True;
      TsgcIdThread(aThread).Terminate;
      {$IFDEF D10_4}
      TObject(Pointer(@aThread)^) := nil;
      {$ELSE}
      Pointer(aThread) := nil;
      {$ENDIF}
    end;
    {$IFDEF SGC_DEBUG}
    DoLog(nil, nil, 'sgcThreadFree_End');
    {$ENDIF}
  end;
end;

function TsgcComponent_Base.GetVersion: String;
begin
  Result := CS_VERSION;
end;

function TsgcComponent_Base.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TsgcComponent_Base.IsDestroying: Boolean;
begin
  result := csDestroying in ComponentState;
end;

function TsgcComponent_Base.IsLoading: Boolean;
begin
  result := csLoading in ComponentState;
end;

constructor TsgcTimer.Create;
begin
  {$IFDEF DEBUG}
  FSetThreadName := False;
  {$ENDIF}
  inherited Create(False);
  Interval := 60000;
  NotifyEvents := neNoSync;
end;

destructor TsgcTimer.Destroy;
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'Timer_Destroy_Start: ' + DebugName);
  {$ENDIF}
  inherited;
  {$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'Timer_Destroy_End: ' + DebugName);
  {$ENDIF}
end;

procedure TsgcTimer.DoOnExceptionEvent(const E: Exception);
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'OnOnExceptionEvent_Start: ' + DebugName + ' ' + E.message);
  {$ENDIF}
  if Assigned(FOnException) then
  begin
    FIsExecuting := True;
    Try
      FOnException(self, E);
    Finally
      FIsExecuting := False;
    End;
  end;
  {$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'OnOnExceptionEvent_End: ' + DebugName + ' ' + E.message);
  {$ENDIF}
end;

procedure TsgcTimer.DoOnTimerEvent;
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'OnTimerEvent_Start: ' + DebugName);
  {$ENDIF}
  if Assigned(FOnTimer) then
  begin
    Try
      FIsExecuting := True;
      Try
        FOnTimer(Self);
      Finally
        FIsExecuting := False;
      End;
    Except
      On E: Exception do
      begin
        {$IFDEF SGC_DEBUG}
        DoLog(self, nil, 'OnTimerEvent_Exception ' + DebugName + ' ' + E.message);
        {$ENDIF}
        if Assigned(FOnException) then
          DoOnExceptionEvent(E)
        else
          raise;
      end;
    End;
  end;
  {$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'OnTimerEvent_End: ' + DebugName);
  {$ENDIF}
end;

procedure TsgcTimer.Run;
var
  vInterval: Integer;
begin
  inherited;

  Try
    {$IFDEF DEBUG}
    if not FSetThreadName then
    begin
      FSetThreadName := True;
      sgcSetThreadName(ClassName);
    end;
    {$ENDIF}
    vInterval := Interval;
    while vInterval > 0 do
    begin
{$IFNDEF INDY10_2}
      sleep(100);
{$ELSE}
      IndySleep(100);
{$ENDIF}
      vInterval := vInterval - 100;

      if Terminated then
        exit;
    end;

    case NotifyEvents of
      neNoSync:
        DoOnTimerEvent;
      neAsynchronous:
        NotifyMethod(DoOnTimerEvent);
      neSynchronous:
        SynchronizeMethod(DoOnTimerEvent);
    end;
  Except
    On E: Exception do
      DoOnExceptionEvent(E);
  End;
end;

procedure TsgcTimer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if Enabled then
  begin
    {$IFDEF SGC_DEBUG}
    DoLog(self, nil, 'Timer_Start_Before: ' + DebugName);
    {$ENDIF}
    Start;
    {$IFDEF SGC_DEBUG}
    DoLog(self, nil, 'Timer_Start_After: ' + DebugName);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF SGC_DEBUG}
    DoLog(self, nil, 'Timer_Terminate_Before: ' + DebugName);
    {$ENDIF}
    Terminate;
    {$IFDEF SGC_DEBUG}
    DoLog(self, nil, 'Timer_Terminate_After: ' + DebugName);
    {$ENDIF}
  end;
end;

constructor TsgcIdThread.Create(ACreateSuspended: Boolean = True; ALoop:
    Boolean = True; {$IFDEF INDY10_2}const {$ENDIF}AName: string = '');
begin
  FThreadId := 0;
  inherited;
end;

function TsgcIdThread.IsCurrentThread: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := False;
  if FThreadID = GetCurrentThreadId then
    Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TsgcIdThread.Run;
begin
  {$IFDEF MSWINDOWS}
  if FThreadId = 0 then
    FThreadId := GetCurrentThreadId;
  {$ENDIF}

  inherited;
end;

constructor TsgcThreadSafeBase.Create;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  InitializeCriticalSection(FCS);
  {$ELSE}
  FCS := TCriticalSection.Create;
  {$ENDIF}
end;

destructor TsgcThreadSafeBase.Destroy;
begin
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(FCS);
  {$ELSE}
  if Assigned(FCS) then
    sgcFree(FCS);
  {$ENDIF}
  inherited;
end;

procedure TsgcThreadSafeBase.DoEnterCS;
begin
  {$IFDEF MSWINDOWS}
  EnterCriticalSection(FCS);
  {$ELSE}
  FCS.Enter;
  {$ENDIF}
end;

procedure TsgcThreadSafeBase.DoLeaveCS;
begin
  {$IFDEF MSWINDOWS}
  LeaveCriticalSection(FCS);
  {$ELSE}
  FCS.Leave;
  {$ENDIF}
end;

constructor TsgcThreadSafeComponent.Create(aOwner: TComponent);
begin
  inherited;
  {$IFDEF MSWINDOWS}
  InitializeCriticalSection(FCS);
  {$ELSE}
  FCS := TCriticalSection.Create;
  {$ENDIF}
end;

destructor TsgcThreadSafeComponent.Destroy;
begin
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(FCS);
  {$ELSE}
  if Assigned(FCS) then
    sgcFree(FCS);
  {$ENDIF}
  inherited;
end;

procedure TsgcThreadSafeComponent.DoEnterCS;
begin
  {$IFDEF MSWINDOWS}
  EnterCriticalSection(FCS);
  {$ELSE}
  FCS.Enter;
  {$ENDIF}
end;

procedure TsgcThreadSafeComponent.DoLeaveCS;
begin
  {$IFDEF MSWINDOWS}
  LeaveCriticalSection(FCS);
  {$ELSE}
  FCS.Leave;
  {$ENDIF}
end;

initialization

finalization

end.
