//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSignalRHubConnection;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Types, TypInfo, Variants, SyncObjs,
  ScTypes, ScFunctions, ScCLRClasses,
  ScThread, ScUtils, ScPipe,
  ScSignalRConsts, 
  ScSignalRMessages, ScSignalRProtocol, ScSignalRHttpConnection;

const
  DefaultHandshakeTimeout = 30;
  DefaultInvokeTimeout = 120;
  DefaultServerTimeout = 30; // Server ping rate is 15 sec, this is 2 times that
  DefaultKeepAliveInterval = 15;
  DefaultTickRateTicks = 1000;

type
  TScHubConnection = class;

  TScInvocationHandlerCallback = procedure(Sender: TObject; const Values: array of Variant) of object;
  TScHubAfterConnectEvent = procedure(Sender: TObject; const ConnectionId: string) of object;
  TScHubBeforeReconnectEvent = procedure(Sender: TObject; E: Exception) of object;
  TScHubAfterReconnectEvent = procedure(Sender: TObject; const ConnectionId: string) of object;
  TScHubAfterDisconnectEvent = procedure(Sender: TObject; E: Exception) of object;

  TScInvocationRequest = class
  private
    FLogger: TScLogger;
    FInvocationId: string;
    FResultType: TVarType;
    FResultClass: TClass;
    FInvocationCaller: TScInvocationCaller;
    FIsCompleted: boolean;
    FIsCanceled: boolean;

  public
    constructor Create(ALogger: TScLogger; const AResultType: TVarType; const AResultClass: TClass;
      const AInvocationId: string; AInvocationCaller: TScInvocationCaller); virtual;
    destructor Destroy; override;

    class function Invoke(ALogger: TScLogger; const AResultType: TVarType; const AResultClass: TClass;
      const AInvocationId: string; AInvocationCaller: TScInvocationCaller): TScInvocationRequest;
    class function Stream(ALogger: TScLogger; const AResultType: TVarType; const AResultClass: TClass;
      const AInvocationId: string; AInvocationCaller: TScInvocationCaller): TScInvocationRequest;

    procedure Fail(E: Exception); virtual; abstract;
    procedure Complete(Message: TScCompletionMessage); virtual; abstract;
    function StreamItem(Message: TScStreamItemMessage): boolean; virtual; abstract;
    procedure Cancel; virtual; abstract;

    property InvocationId: string read FInvocationId;
    property ResultType: TVarType read FResultType;
    property ResultClass: TClass read FResultClass;
    property IsCanceled: boolean read FIsCanceled;
  end;

  TScStreaming = class(TScInvocationRequest)
  public
    procedure Fail(E: Exception); override;
    procedure Complete(Message: TScCompletionMessage); override;
    function StreamItem(Message: TScStreamItemMessage): boolean; override;
    procedure Cancel; override;
  end;

  TScNonStreaming = class(TScInvocationRequest)
  public
    procedure Fail(E: Exception); override;
    procedure Complete(Message: TScCompletionMessage); override;
    function StreamItem(Message: TScStreamItemMessage): boolean; override;
    procedure Cancel; override;
  end;

  TScSyncInvocationCaller = class(TScInvocationCaller)
  private
    FCompletion: TEvent;
    FResult: Variant;

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteItem(var Item: Variant); override;
    procedure Complete(var Item: Variant); override;
    procedure Fail(E: Exception); override;
    function WaitFor(Timeout: cardinal): boolean;

    property Result: Variant read FResult;
  end;

  TScRetryContext = record
    PreviousRetryCount: Int64;
    ElapsedTime: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
    RetryReason: Exception;
  end;

  TScRetryPolicy = class
  public
    function NextRetryDelay(const RetryContext: TScRetryContext): {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}; virtual; abstract;
  end;

  TScDefaultRetryPolicy = class(TScRetryPolicy)
  private
    FRetryDelays: array of {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
  public
    constructor Create;

    function NextRetryDelay(const RetryContext: TScRetryContext): {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}; override;
  end;

  TScHubConnectionState = (
    hcsDisconnected, hcsConnected, hcsConnecting, hcsReconnecting
  );

  IScConnectionState = interface(IScInvocationBinder)
    function GetConnection: TScHttpConnection;
    function GetCloseException: Exception;
    procedure SetCloseException(E: Exception);
    function GetStopping: boolean;
    procedure SetStopping(Value: boolean);
    procedure SetReceiveTask(Value: TThread);

    function GetNextId: string;
    procedure AddInvocation(InvocationRequest: TScInvocationRequest);
    function TryGetInvocation(const InvocationId: string; out InvocationRequest: TScInvocationRequest): boolean;
    function TryRemoveInvocation(const InvocationId: string; out InvocationRequest: TScInvocationRequest): boolean;
    procedure CancelOutstandingInvocations(E: Exception);
    procedure Stop;

    procedure PreparePing;
    function GetTimerLoopProc: TNotifyEvent;
    procedure ResetSendPing;
    procedure ResetTimeout;
    procedure RunTimerActions(Sender: TObject);

    property Connection: TScHttpConnection read GetConnection;
    property CloseException: Exception read GetCloseException;
    property Stopping: boolean read GetStopping write SetStopping;
  end;

  // Represents all the transient state about a connection
  // This includes binding information because return type binding depends upon FPendingCalls
  TScConnectionState = class(TInterfacedObject, IScConnectionState)
  private
    FLogger: TScLogger;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FHubConnection: TScHubConnection;

    FLock: TCriticalSection;
    FPendingCalls: TStringList;
    FConnection: TScHttpConnection;
    FCloseException: Exception;
    FStopping: boolean;
    FReceiveTask: TThread;

    FNextInvocationId: integer;
    FLastActivationSendPing: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
    FLastActivationServerTimeout: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
    FHasInherentKeepAlive: boolean;

    function GetConnection: TScHttpConnection;
    function GetCloseException: Exception;
    procedure SetCloseException(E: Exception);
    function GetStopping: boolean;
    procedure SetStopping(Value: boolean);
    procedure SetReceiveTask(Value: TThread);

    procedure OnServerTimeout;

    function GetNextId: string;
    procedure AddInvocation(InvocationRequest: TScInvocationRequest);
    function TryGetInvocation(const InvocationId: string; out InvocationRequest: TScInvocationRequest): boolean;
    function TryRemoveInvocation(const InvocationId: string; out InvocationRequest: TScInvocationRequest): boolean;
    procedure CancelOutstandingInvocations(E: Exception);
    procedure Stop;

    procedure PreparePing;
    function GetTimerLoopProc: TNotifyEvent;
    procedure ResetSendPing;
    procedure ResetTimeout;
    procedure RunTimerActions(Sender: TObject);

    function GetParameterTypes(const MethodName: string; out ParamTypes: TExtTypeInfoArray;
      out AsArray: boolean): boolean;
    function GetReturnType(const InvocationId: string): TExtTypeInfo;
    function GetStreamItemType(const InvocationId: string): TExtTypeInfo;

  public
    constructor Create(AConnection: TScHttpConnection; AHubConnection: TScHubConnection; ALogger: TScLogger);
    destructor Destroy; override;
  end;

  TScInvocationHandler = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FParameterTypes: TExtTypeInfoArray;
    FAsArray: boolean;
    FCallback: TScInvocationHandlerCallback;
  public
    constructor Create(AOwner: TScHubConnection; const AParameterTypes: TExtTypeInfoArray;
      AAsArray: boolean; const ACallback: TScInvocationHandlerCallback);

    procedure Invoke(const Parameters: TVariantArray);
  end;

  TScReconnectingConnectionState = class
  private
    FLogger: TScLogger;
    FStopCts: TScCancellationToken;
    FConnectionLock: TSimpleSemaphore;
    FCurrentConnectionStateUnsynchronized: IScConnectionState;
    FOverallState: TScHubConnectionState;
    FReconnectTask: TThread;

    class function StateToStr(State: TScHubConnectionState): string;

  public
    constructor Create(ALogger: TScLogger);
    destructor Destroy; override;

    procedure ChangeState(ExpectedState, NewState: TScHubConnectionState);
    function TryChangeState(ExpectedState, NewState: TScHubConnectionState): boolean;
    procedure AssertInConnectionLock;
    procedure AssertConnectionValid;
    procedure WaitConnectionLock;
    function TryAcquireConnectionLock: boolean;
    function WaitForActiveConnection: IScConnectionState;
    procedure ReleaseConnectionLock;

    property StopCts: TScCancellationToken read FStopCts;
  end;

  TScOnHubClosedAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FEx: Exception;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScHubConnection; Ex: Exception);
    destructor Destroy; override;
  end;

  TScOnHubReconnectingAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FEx: Exception;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScHubConnection; Ex: Exception);
    destructor Destroy; override;
  end;

  TScOnHubReconnectedAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FConnectionId: string;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScHubConnection; const ConnectionId: string);
  end;

  TScHubHandleInvocationAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FHandler: TScInvocationHandler;
    FInvocationMessage: TScInvocationMessage;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScHubConnection; Handler: TScInvocationHandler; InvocationMessage: TScInvocationMessage);
  end;

  TScHubHandleInvocationStreamItemAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FInvocationRequest: TScInvocationRequest;
    FStreamItemMessage: TScStreamItemMessage;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScHubConnection; InvocationRequest: TScInvocationRequest; StreamItemMessage: TScStreamItemMessage);
  end;

  TScHubHandleInvocationCompletionAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScHubConnection;
    FInvocationRequest: TScInvocationRequest;
    FCompletionMessage: TScCompletionMessage;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScHubConnection; InvocationRequest: TScInvocationRequest; CompletionMessage: TScCompletionMessage);
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScHubConnection = class(TComponent)
  private
    FLogger: TScLogger;
    FReconnectPolicy: TScRetryPolicy;
    FHttpConnectionOptions: TScHttpConnectionOptions;
    FProtocol: TScHubProtocol;
    FState: TScReconnectingConnectionState;

    FEventsCallMode: TScEventCallMode;
    FBeforeConnect: TNotifyEvent;
    FAfterConnect: TScHubAfterConnectEvent;
    FBeforeReconnect: TScHubBeforeReconnectEvent;
    FAfterReconnect: TScHubAfterReconnectEvent;
    FAfterDisconnect: TScHubAfterDisconnectEvent;

    FHandshakeTimeout: integer;
    FHandshakeTimeoutTicks: cardinal;
    FInvokeTimeout: integer;
    FInvokeTimeoutTicks: cardinal;
    FServerTimeout: integer;
    FServerTimeoutTicks: cardinal;
    FKeepAliveInterval: integer;
    FKeepAliveIntervalTicks: cardinal;
    FTickRateTicks: cardinal;

    FHandlersLock: TCriticalSection;
    FHandlers: TStringList;

    function GetUrl: string;
    procedure SetUrl(const Value: string);

    procedure SetHandshakeTimeout(Value: integer);
    procedure SetInvokeTimeout(Value: integer);
    procedure SetServerTimeout(Value: integer);
    procedure SetKeepAliveInterval(Value: integer);
    procedure SetHttpConnectionOptions(Value: TScHttpConnectionOptions);
    procedure SetEventsCallMode(Value: TScEventCallMode);

    function GetConnectionId: string;
    function GetState: TScHubConnectionState;

    function GetHandler(const MethodName: string): TScInvocationHandler;
    procedure ProcessMessages(ConnectionState: IScConnectionState; HubMessage: TScHubMessage;
      out Close: boolean; out E: Exception);
    procedure DispatchInvocation(Invocation: TScInvocationMessage);
    procedure HandleInvocation(Handler: TScInvocationHandler; Invocation: TScInvocationMessage);
    procedure DispatchInvocationStreamItem(StreamItem: TScStreamItemMessage; InvocationRequest: TScInvocationRequest);
    procedure HandleInvocationStreamItem(InvocationRequest: TScInvocationRequest; StreamItem: TScStreamItemMessage);
    procedure DispatchInvocationCompletion(Completion: TScCompletionMessage; InvocationRequest: TScInvocationRequest);
    procedure HandleInvocationCompletion(InvocationRequest: TScInvocationRequest; Completion: TScCompletionMessage);

    procedure InternalStart(CancellationToken: TScCancellationToken);
    procedure Close(Connection: TScHttpConnection);

    function ArrayObjectsToVariant(const Args: array of TObject): TVariantArray;
    function ArrayConstsToVariant(const Args: array of Variant): TVariantArray;

    procedure InternalRegister(const MethodName: string; const ParameterTypes: TExtTypeInfoArray;
      AsArray: boolean; const Handler: TScInvocationHandlerCallback); overload;
    procedure InternalInvoke(const MethodName: string; const Args: TVariantArray; AsArray: boolean;
      const ReturnType: TVarType; const ReturnClass: TClass; out Res: Variant);
    procedure InternalSend(const MethodName: string; const Args: TVariantArray; AsArray: boolean);
    procedure Handshake(StartingConnectionState: IScConnectionState; CancellationToken: TScCancellationToken);
    procedure SendHubMessage(ConnectionState: IScConnectionState; HubMessage: TScHubMessage);

    procedure ReceiveLoop(ConnectionState: IScConnectionState);
    procedure HandleConnectionClose(ConnectionState: IScConnectionState);
    procedure CompleteClose(CloseException: Exception);
    procedure Reconnect(CloseException: Exception);
    function GetNextRetryDelay(PreviousRetryCount: integer; ElapsedTime: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
      RetryReason: Exception): {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};

    procedure DoBeforeReconnect(Ex: Exception);
    procedure DoAfterReconnect(const ConnectionId: string);
    procedure DoAfterDisconnect(Ex: Exception);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    procedure Init(const Url: string; const Transports: TScHttpTransportTypes;
      AHttpConnectionOptions: TScHttpConnectionOptions; AReconnectPolicy: TScRetryPolicy; ALogger: TScLogger);

    procedure CheckInactive;

  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const Url: string; const Transports: TScHttpTransportTypes = []; ALogger: TScLogger = nil); reintroduce; overload;
    constructor Create(const Url: string; const Transports: TScHttpTransportTypes;
      AHttpConnectionOptions: TScHttpConnectionOptions; AReconnectPolicy: TScRetryPolicy; ALogger: TScLogger); reintroduce; overload;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure Register(const MethodName: string; const Handler: TScInvocationHandlerCallback; const ParameterTypes: array of TVarType);
    procedure RegisterArr(const MethodName: string; const Handler: TScInvocationHandlerCallback; const ParameterType: TVarType);
    procedure RegisterObj(const MethodName: string; const Handler: TScInvocationHandlerCallback; const ParameterTypes: array of TClass);
    procedure Unregister(const MethodName: string);

    procedure InvokeObj(const MethodName: string; const Args: array of TObject; const ReturnType: TVarType; out Res: Variant); overload;
    procedure InvokeObj(const MethodName: string; const Args: array of TObject; const ReturnClass: TClass; out Res: TObject); overload;
    procedure InvokeObj(const MethodName: string; const Args: array of TObject); overload;
    procedure InvokeArr(const MethodName: string; const Args: array of Variant; const ReturnType: TVarType; out Res: Variant); overload;
    procedure InvokeArr(const MethodName: string; const Args: array of Variant; const ReturnClass: TClass; out Res: TObject); overload;
    procedure InvokeArr(const MethodName: string; const Args: array of Variant); overload;
    procedure Invoke(const MethodName: string; const Args: array of Variant; const ReturnType: TVarType; out Res: Variant); overload;
    procedure Invoke(const MethodName: string; const Args: array of Variant; const ReturnClass: TClass; out Res: TObject); overload;
    procedure Invoke(const MethodName: string; const Args: array of Variant); overload;

    procedure SendObj(const MethodName: string; const Args: array of TObject);
    procedure SendArr(const MethodName: string; const Args: array of Variant);
    procedure Send(const MethodName: string; const Args: array of Variant);

    property ConnectionId: string read GetConnectionId;
    property State: TScHubConnectionState read GetState;

    property Logger: TScLogger read FLogger write FLogger;
    property ReconnectPolicy: TScRetryPolicy read FReconnectPolicy;

  published
    property HandshakeTimeout: integer read FHandshakeTimeout write SetHandshakeTimeout default DefaultHandshakeTimeout;
    property InvokeTimeout: integer read FInvokeTimeout write SetInvokeTimeout default DefaultInvokeTimeout;
    property ServerTimeout: integer read FServerTimeout write SetServerTimeout default DefaultServerTimeout;
    property KeepAliveInterval: integer read FKeepAliveInterval write SetKeepAliveInterval default DefaultKeepAliveInterval;
    property Url: string read GetUrl write SetUrl;
    property HttpConnectionOptions: TScHttpConnectionOptions read FHttpConnectionOptions write SetHttpConnectionOptions;

    property EventsCallMode: TScEventCallMode read FEventsCallMode write SetEventsCallMode default ecDirectly;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TScHubAfterConnectEvent read FAfterConnect write FAfterConnect;
    property BeforeReconnect: TScHubBeforeReconnectEvent read FBeforeReconnect write FBeforeReconnect;
    property AfterReconnect: TScHubAfterReconnectEvent read FAfterReconnect write FAfterReconnect;
    property AfterDisconnect: TScHubAfterDisconnectEvent read FAfterDisconnect write FAfterDisconnect;
  end;

var
  DefaultReconnectPolicy: TScDefaultRetryPolicy;

implementation

type
  TScReceiveLoop = class(TThread)
  private
    FHubConnection: TScHubConnection;
    FConnectionState: IScConnectionState;
  protected
    procedure Execute; override;
  public
    constructor Create(AHubConnection: TScHubConnection; AConnectionState: IScConnectionState);
  end;

  TScReconnectThread = class(TThread)
  private
    FHubConnection: TScHubConnection;
    FCloseException: Exception;
  protected
    procedure Execute; override;
  public
    constructor Create(AHubConnection: TScHubConnection; ACloseException: Exception);
  end;

{ TScInvocationRequest }

constructor TScInvocationRequest.Create(ALogger: TScLogger; const AResultType: TVarType; const AResultClass: TClass;
  const AInvocationId: string; AInvocationCaller: TScInvocationCaller);
begin
  inherited Create;

  FLogger := ALogger;

  FResultType := AResultType;
  FResultClass := AResultClass;
  FInvocationId := AInvocationId;
  FInvocationCaller := AInvocationCaller;

  Log.LogDebug(FLogger, SInvocationRequest_InvocationCreated, [FInvocationId]);
end;

destructor TScInvocationRequest.Destroy;
begin
  Log.LogDebug(FLogger, SInvocationRequest_InvocationDisposed, [FInvocationId]);

  if not FIsCompleted then
    Cancel;

  inherited;
end;

class function TScInvocationRequest.Invoke(ALogger: TScLogger; const AResultType: TVarType; const AResultClass: TClass;
  const AInvocationId: string; AInvocationCaller: TScInvocationCaller): TScInvocationRequest;
begin
  Result := TScNonStreaming.Create(ALogger, AResultType, AResultClass, AInvocationId, AInvocationCaller);
end;

class function TScInvocationRequest.Stream(ALogger: TScLogger; const AResultType: TVarType; const AResultClass: TClass;
  const AInvocationId: string; AInvocationCaller: TScInvocationCaller): TScInvocationRequest;
begin
  Result := TScStreaming.Create(ALogger, AResultType, AResultClass, AInvocationId, AInvocationCaller);
end;

{ TScStreaming }

procedure TScStreaming.Fail(E: Exception);
begin
  FIsCompleted := True;

  Log.LogDebug(FLogger, SInvocationRequest_InvocationFailed, [FInvocationId]);
  FInvocationCaller.Fail(E);
end;

procedure TScStreaming.Complete(Message: TScCompletionMessage);
var
  E: Exception;
begin
  Log.LogDebug(FLogger, SInvocationRequest_InvocationCompleted, [FInvocationId]);

  FIsCompleted := True;

  if not (VarIsNull(Message.Result) or VarIsEmpty(Message.Result)) then begin
    Log.LogError(FLogger, SInvocationRequest_ReceivedUnexpectedComplete, [FInvocationId], nil);

    E := InvalidOperationException.Create(SServerProvidedCompletionResult);
    try
      FInvocationCaller.Fail(E);
    finally
      E.Free;
    end;
  end
  else
  if Message.Error <> '' then begin
    E := HubException.Create(Message.Error);
    try
      Fail(E);
    finally
      E.Free;
    end;
  end
  else
    FInvocationCaller.Complete(Message.Result);
end;

function TScStreaming.StreamItem(Message: TScStreamItemMessage): boolean;
begin
  FIsCompleted := True;

  try
    FInvocationCaller.WriteItem(Message.Item);
  except
    on E: Exception do
      Log.LogError(FLogger, SInvocationRequest_ErrorWritingStreamItem, [FInvocationId], E);
  end;

  Result := True;
end;

procedure TScStreaming.Cancel;
var
  E: Exception;
begin
  FIsCanceled := True;

  E := OperationCanceledException.Create(SOperationCanceled);
  try
    FInvocationCaller.Fail(E);
  finally
    E.Free;
  end;
end;

{ TScNonStreaming }

procedure TScNonStreaming.Fail(E: Exception);
begin
  FIsCompleted := True;

  Log.LogDebug(FLogger, SInvocationRequest_InvocationFailed, [FInvocationId]);
  FInvocationCaller.Fail(E);
end;

procedure TScNonStreaming.Complete(Message: TScCompletionMessage);
var
  E: Exception;
begin
  FIsCompleted := True;

  if Message.Error <> '' then begin
    E := HubException.Create(Message.Error);
    try
      Fail(E);
    finally
      E.Free;
    end;
  end
  else begin
    Log.LogDebug(FLogger, SInvocationRequest_InvocationCompleted, [FInvocationId]);
    FInvocationCaller.Complete(Message.Result);
  end;
end;

function TScNonStreaming.StreamItem(Message: TScStreamItemMessage): boolean;
var
  E: Exception;
begin
  FIsCompleted := True;

  Log.LogError(FLogger, SInvocationRequest_StreamItemOnNonStreamInvocation, [FInvocationId], nil);

  E := InvalidOperationException.Create(SStreamingMustBeInvokeWithStreamAsChannel);
  try
    FInvocationCaller.Fail(E);
  finally
    E.Free;
  end;

  Result := True;
end;

procedure TScNonStreaming.Cancel;
var
  E: Exception;
begin
  FIsCanceled := True;

  E := OperationCanceledException.Create(SOperationCanceled);
  try
    FInvocationCaller.Fail(E);
  finally
    E.Free;
  end;
end;

{ TScSyncInvocationCaller }

constructor TScSyncInvocationCaller.Create;
begin
  inherited;

  FResult := Unassigned;
  FCompletion := CreateEvent;
end;

destructor TScSyncInvocationCaller.Destroy;
begin
  FCompletion.SetEvent;
  FCompletion.Free;

  inherited;
end;

procedure TScSyncInvocationCaller.WriteItem(var Item: Variant);
begin
  inherited;

  FResult := Item;
  Item := Unassigned;

  FCompletion.SetEvent;
end;

procedure TScSyncInvocationCaller.Complete(var Item: Variant);
begin
  inherited;

  FResult := Item;
  Item := Unassigned;

  FCompletion.SetEvent;
end;

procedure TScSyncInvocationCaller.Fail(E: Exception);
begin
  inherited;

  TVarData(FResult).VType := varByRef;
  TVarData(FResult).VPointer := CloneException(E);

  FCompletion.SetEvent;
end;

function TScSyncInvocationCaller.WaitFor(Timeout: cardinal): boolean;
begin
  Result := FCompletion.WaitFor(Timeout) = wrSignaled;
end;

{ TScDefaultRetryPolicy }

constructor TScDefaultRetryPolicy.Create;
begin
  inherited;

  SetLength(FRetryDelays, 5);
  FRetryDelays[0] := 0;
  FRetryDelays[1] := 2 * 1000;
  FRetryDelays[2] := 10 * 1000;
  FRetryDelays[3] := 30 * 1000;
  FRetryDelays[4] := {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}(-1);
end;

function TScDefaultRetryPolicy.NextRetryDelay(const RetryContext: TScRetryContext): {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
begin
  if RetryContext.PreviousRetryCount >= Length(FRetryDelays) then
    Result := {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}(-1)
  else
    Result := FRetryDelays[RetryContext.PreviousRetryCount];
end;

{ TScReceiveLoop }

constructor TScReceiveLoop.Create(AHubConnection: TScHubConnection; AConnectionState: IScConnectionState);
begin
  FHubConnection := AHubConnection;
  FConnectionState := AConnectionState;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

procedure TScReceiveLoop.Execute;
begin
  try
    try
      FHubConnection.ReceiveLoop(FConnectionState);
    finally
      FConnectionState := nil;
    end;
  except
  end;
end;

{ TScReconnectThread }

constructor TScReconnectThread.Create(AHubConnection: TScHubConnection; ACloseException: Exception);
begin
  FHubConnection := AHubConnection;
  FCloseException := ACloseException;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

procedure TScReconnectThread.Execute;
begin
  try
    FHubConnection.Reconnect(FCloseException);
  except
  end;
end;

{ TScOnHubClosedAsyncEvent }

constructor TScOnHubClosedAsyncEvent.Create(Owner: TScHubConnection; Ex: Exception);
begin
  inherited Create;

  FOwner := Owner;
  FEx := Ex;
end;

destructor TScOnHubClosedAsyncEvent.Destroy;
begin
  FreeAndNil(FEx);

  inherited;
end;

procedure TScOnHubClosedAsyncEvent.InternalNotify;
begin
  FOwner.DoAfterDisconnect(FEx);
end;

{ TScOnHubReconnectingAsyncEvent }

constructor TScOnHubReconnectingAsyncEvent.Create(Owner: TScHubConnection; Ex: Exception);
begin
  inherited Create;

  FOwner := Owner;
  FEx := Ex;
end;

destructor TScOnHubReconnectingAsyncEvent.Destroy;
begin
  FreeAndNil(FEx);

  inherited;
end;

procedure TScOnHubReconnectingAsyncEvent.InternalNotify;
begin
  FOwner.DoBeforeReconnect(FEx);
end;

{ TScOnHubReconnectedAsyncEvent }

constructor TScOnHubReconnectedAsyncEvent.Create(Owner: TScHubConnection; const ConnectionId: string);
begin
  inherited Create;

  FOwner := Owner;
  FConnectionId := ConnectionId;
end;

procedure TScOnHubReconnectedAsyncEvent.InternalNotify;
begin
  FOwner.DoAfterReconnect(FConnectionId);
end;

{ TScHubHandleInvocationAsyncEvent }

constructor TScHubHandleInvocationAsyncEvent.Create(Owner: TScHubConnection;
  Handler: TScInvocationHandler; InvocationMessage: TScInvocationMessage);
begin
  inherited Create;

  FOwner := Owner;
  FHandler := Handler;
  FInvocationMessage := InvocationMessage;
end;

procedure TScHubHandleInvocationAsyncEvent.InternalNotify;
begin
  FOwner.HandleInvocation(FHandler, FInvocationMessage);
end;

{ TScHubHandleInvocationStreamItemAsyncEvent }

constructor TScHubHandleInvocationStreamItemAsyncEvent.Create(Owner: TScHubConnection;
  InvocationRequest: TScInvocationRequest; StreamItemMessage: TScStreamItemMessage);
begin
  inherited Create;

  FOwner := Owner;
  FInvocationRequest := InvocationRequest;
  FStreamItemMessage := StreamItemMessage;
end;

procedure TScHubHandleInvocationStreamItemAsyncEvent.InternalNotify;
begin
  FOwner.HandleInvocationStreamItem(FInvocationRequest, FStreamItemMessage);
end;

{ TScHubHandleInvocationCompletionAsyncEvent }

constructor TScHubHandleInvocationCompletionAsyncEvent.Create(Owner: TScHubConnection;
  InvocationRequest: TScInvocationRequest; CompletionMessage: TScCompletionMessage);
begin
  inherited Create;

  FOwner := Owner;
  FInvocationRequest := InvocationRequest;
  FCompletionMessage := CompletionMessage;
end;

procedure TScHubHandleInvocationCompletionAsyncEvent.InternalNotify;
begin
  FOwner.HandleInvocationCompletion(FInvocationRequest, FCompletionMessage);
end;

{ TScHubConnection }

constructor TScHubConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Init('', [], nil, DefaultReconnectPolicy, nil);
end;

constructor TScHubConnection.Create(const Url: string; const Transports: TScHttpTransportTypes = []; ALogger: TScLogger = nil);
begin
  inherited Create(nil);

  Init(Url, Transports, nil, DefaultReconnectPolicy, ALogger);
end;

constructor TScHubConnection.Create(const Url: string; const Transports: TScHttpTransportTypes;
  AHttpConnectionOptions: TScHttpConnectionOptions; AReconnectPolicy: TScRetryPolicy; ALogger: TScLogger);
begin
  inherited Create(nil);

  Init(Url, Transports, AHttpConnectionOptions, AReconnectPolicy, ALogger);
end;

procedure TScHubConnection.Init(const Url: string; const Transports: TScHttpTransportTypes;
  AHttpConnectionOptions: TScHttpConnectionOptions; AReconnectPolicy: TScRetryPolicy; ALogger: TScLogger);
begin
  FEventsCallMode := ecDirectly;

  FLogger := ALogger;
  FReconnectPolicy := AReconnectPolicy;

  FHttpConnectionOptions := TScHttpConnectionOptions.Create(Self);
  if Assigned(AHttpConnectionOptions) then
    FHttpConnectionOptions.Assign(AHttpConnectionOptions);

  if Url <> '' then
    FHttpConnectionOptions.Url := Url;
  if Transports <> [] then
    FHttpConnectionOptions.Transports := Transports;

  FHandlers := TStringList.Create;
  FHandlersLock := TCriticalSection.Create;

  FProtocol := TScJsonHubProtocol.Create;
  FState := TScReconnectingConnectionState.Create(FLogger);

  SetHandshakeTimeout(DefaultHandshakeTimeout);
  SetInvokeTimeout(DefaultInvokeTimeout);
  SetServerTimeout(DefaultServerTimeout);
  SetKeepAliveInterval(DefaultKeepAliveInterval);
  FTickRateTicks := DefaultTickRateTicks;
end;

destructor TScHubConnection.Destroy;
var
  i: integer;
begin
  Stop;

  FState.Free;

  for i := 0 to FHandlers.Count - 1 do
  {$IFNDEF NEXTGEN}
    FHandlers.Objects[i].Free;
  {$ELSE}
    FHandlers.Objects[i] := nil;
  {$ENDIF}

  FHandlers.Free;
  FHandlersLock.Free;

  FProtocol.Free;
  FreeAndNil(FHttpConnectionOptions);

  inherited;
end;

procedure TScHubConnection.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScHubConnection) then begin
    TScHubConnection(Dest).EventsCallMode := EventsCallMode;

    TScHubConnection(Dest).SetHandshakeTimeout(HandshakeTimeout);
    TScHubConnection(Dest).SetInvokeTimeout(InvokeTimeout);
    TScHubConnection(Dest).SetServerTimeout(ServerTimeout);
    TScHubConnection(Dest).SetKeepAliveInterval(KeepAliveInterval);

    TScHubConnection(Dest).FHttpConnectionOptions.Assign(FHttpConnectionOptions);
    TScHubConnection(Dest).FLogger := FLogger;
    TScHubConnection(Dest).FReconnectPolicy := FReconnectPolicy;
  end
  else
    inherited;
end;

procedure TScHubConnection.Notification(Component: TComponent; Operation: TOperation);
begin
  if (FHttpConnectionOptions <> nil) and (Component = FHttpConnectionOptions.SSLOptions.Storage) and (Operation = opRemove) then
    FHttpConnectionOptions.SSLOptions.Storage := nil;

  inherited;
end;

procedure TScHubConnection.CheckInactive;
begin
  if GetState <> hcsDisconnected then
    raise InvalidOperationException.CreateRes(@SHubConnectionAlreadyConnected);
end;

function TScHubConnection.GetUrl: string;
begin
  Result := FHttpConnectionOptions.Url;
end;

procedure TScHubConnection.SetUrl(const Value: string);
begin
  CheckInactive;
  FHttpConnectionOptions.Url := Value;
end;

procedure TScHubConnection.SetHandshakeTimeout(Value: integer);
begin
  CheckInactive;
  FHandshakeTimeout := Value;
  FHandshakeTimeoutTicks := FHandshakeTimeout * 1000;
end;

procedure TScHubConnection.SetInvokeTimeout(Value: integer);
begin
  FInvokeTimeout := Value;
  FInvokeTimeoutTicks := FInvokeTimeout * 1000;
end;

procedure TScHubConnection.SetServerTimeout(Value: integer);
begin
  CheckInactive;
  FServerTimeout := Value;
  FServerTimeoutTicks := FServerTimeout * 1000;
end;

procedure TScHubConnection.SetKeepAliveInterval(Value: integer);
begin
  CheckInactive;
  FKeepAliveInterval := Value;
  FKeepAliveIntervalTicks := FKeepAliveInterval * 1000;
end;

procedure TScHubConnection.SetHttpConnectionOptions(Value: TScHttpConnectionOptions);
begin
  if Value <> FHttpConnectionOptions then begin
    CheckInactive;
    FHttpConnectionOptions.Assign(Value);
  end;
end;

procedure TScHubConnection.SetEventsCallMode(Value: TScEventCallMode);
begin
  if Value <> FEventsCallMode then begin
    CheckInactive;
    FEventsCallMode := Value;
  end;
end;

function TScHubConnection.GetConnectionId: string;
var
  ConnectionState: IScConnectionState;
begin
  ConnectionState := FState.FCurrentConnectionStateUnsynchronized;
  if ConnectionState <> nil then
    Result := ConnectionState.Connection.ConnectionId
  else
    Result := '';
end;

function TScHubConnection.GetState: TScHubConnectionState;
begin
  Result := FState.FOverallState;
end;

procedure TScHubConnection.Start;
begin
  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  FState.WaitConnectionLock;
  try
    if not FState.TryChangeState(hcsDisconnected, hcsConnecting) then
      raise InvalidOperationException.Create(SHubConnectionCanNotBeStartedIfNotDisconnected);

    try
      // The StopCts is canceled at the start of Stop should be reset every time the connection finishes stopping.
      // If this token is currently canceled, it means that Start was called while Stop was still running.
      if FState.StopCts.IsCancellationRequested then
        raise InvalidOperationException.Create(SHubConnectionCanNotBeStartedWhileStop);

      if Assigned(BeforeConnect) then
        BeforeConnect(Self);

      InternalStart(FState.StopCts);

      FState.ChangeState(hcsConnecting, hcsConnected);
    except
      if FState.TryChangeState(hcsConnecting, hcsDisconnected) then
        FState.StopCts.ReInit;
      raise;
    end;
  finally
    FState.ReleaseConnectionLock;
  end;

  if Assigned(AfterConnect) then
    AfterConnect(Self, ConnectionId);
end;

procedure TScHubConnection.Stop;
var
  ConnectionState: IScConnectionState;
begin
  // Start acquires the connection lock for the duration of the handshake.
  // Reconnect also acquires the connection lock for reconnect attempts and handshakes.
  // Cancel the StopCts without acquiring the lock so we can short-circuit it.
  FState.StopCts.Cancel;

  // Potentially wait for Start to finish, and block a new Start from starting until we have finished stopping.
  FState.WaitConnectionLock;
  try
    ConnectionState := FState.FCurrentConnectionStateUnsynchronized;

    // Set the stopping flag so that any invocations after this get a useful error message instead of
    // silently failing or throwing an error about the pipe being completed.
    if ConnectionState <> nil then
      ConnectionState.Stopping := True;

    if FState.FReconnectTask <> nil then begin
      // Let the current reconnect attempts finish if necessary without the lock.
      // Otherwise, Reconnect will stall forever acquiring the lock.
      // It should never throw, even if the reconnect attempts fail.
      // The StopCts should prevent the HubConnection from restarting until it is reset.
      FState.ReleaseConnectionLock;
      try
        if FState.FReconnectTask <> nil then
          FState.FReconnectTask.WaitFor;
      finally
        FState.WaitConnectionLock;
      end;
      FreeAndNil(FState.FReconnectTask);
    end;
  finally
    FState.ReleaseConnectionLock;
  end;

  // Now stop the connection we captured
  if ConnectionState <> nil then
    ConnectionState.Stop;
end;

function TScHubConnection.GetHandler(const MethodName: string): TScInvocationHandler;
var
  Index: integer;
begin
  FHandlersLock.Enter;
  try
    Index := FHandlers.IndexOf(MethodName);
    if Index >= 0 then
      Result := TScInvocationHandler(FHandlers.Objects[Index])
    else
      Result := nil;
  finally
    FHandlersLock.Leave;
  end;
end;

procedure TScHubConnection.Register(const MethodName: string; const Handler: TScInvocationHandlerCallback;
  const ParameterTypes: array of TVarType);
var
  ExtParameterTypes: TExtTypeInfoArray;
  i: integer;
begin
  SetLength(ExtParameterTypes, Length(ParameterTypes));
  for i := 0 to Length(ParameterTypes) - 1 do begin
    ExtParameterTypes[i].VarType := ParameterTypes[i];
    ExtParameterTypes[i].ClassType := nil;
  end;

  InternalRegister(MethodName, ExtParameterTypes, False, Handler);
end;

procedure TScHubConnection.RegisterArr(const MethodName: string; const Handler: TScInvocationHandlerCallback;
  const ParameterType: TVarType);
var
  ExtParameterTypes: TExtTypeInfoArray;
begin
  SetLength(ExtParameterTypes, 1);
  ExtParameterTypes[0].VarType := ParameterType;
  ExtParameterTypes[0].ClassType := nil;

  InternalRegister(MethodName, ExtParameterTypes, True, Handler);
end;

procedure TScHubConnection.RegisterObj(const MethodName: string; const Handler: TScInvocationHandlerCallback;
  const ParameterTypes: array of TClass);
var
  ExtParameterTypes: TExtTypeInfoArray;
  i: integer;
begin
  SetLength(ExtParameterTypes, Length(ParameterTypes));
  for i := 0 to Length(ParameterTypes) - 1 do begin
    ExtParameterTypes[i].VarType := varByRef;

    if ParameterTypes[i] = nil then
      raise ArgumentException.CreateFmt(SInvalidInputArg, ['ParameterTypes']);
    ExtParameterTypes[i].ClassType := ParameterTypes[i];
  end;

  InternalRegister(MethodName, ExtParameterTypes, False, Handler);
end;

procedure TScHubConnection.InternalRegister(const MethodName: string; const ParameterTypes: TExtTypeInfoArray;
  AsArray: boolean; const Handler: TScInvocationHandlerCallback);
var
  InvocationHandler: TScInvocationHandler;
  Index: integer;
begin
  if not Assigned(Handler) then
    raise ArgumentException.CreateFmt(SInvalidInputArg, ['Handler']);

  Log.LogDebug(FLogger, SHubConnection_RegisteringHandler, [MethodName]);

  FHandlersLock.Enter;
  try
    Index := FHandlers.IndexOf(MethodName);

    if Index = -1 then begin
      InvocationHandler := TScInvocationHandler.Create(Self, ParameterTypes, AsArray, Handler);
      FHandlers.AddObject(MethodName, InvocationHandler);
    end
    else begin
      InvocationHandler := TScInvocationHandler(FHandlers.Objects[Index]);
      InvocationHandler.FParameterTypes := ParameterTypes;
      InvocationHandler.FAsArray := AsArray;
      InvocationHandler.FCallback := Handler;
    end;
  finally
    FHandlersLock.Leave;
  end;
end;

procedure TScHubConnection.Unregister(const MethodName: string);
var
  Index: integer;
begin
  FHandlersLock.Enter;
  try
    Log.LogDebug(FLogger, SHubConnection_RemovingHandlers, [MethodName]);

    Index := FHandlers.IndexOf(MethodName);
    if Index >= 0 then begin
    {$IFNDEF NEXTGEN}
      FHandlers.Objects[Index].Free;
    {$ELSE}
      FHandlers.Objects[Index] := nil;
    {$ENDIF}
      FHandlers.Delete(Index);
    end;
  finally
    FHandlersLock.Leave;
  end;
end;

function TScHubConnection.ArrayObjectsToVariant(const Args: array of TObject): TVariantArray;
var
  i: integer;
begin
  SetLength(Result, Length(Args));
  for i := 0 to Length(Args) - 1 do begin
    TVarData(Result[i]).VType := varByRef;
    TVarData(Result[i]).VPointer := Args[i];
  end;
end;

function TScHubConnection.ArrayConstsToVariant(const Args: array of Variant): TVariantArray;
var
  i: integer;
begin
  SetLength(Result, Length(Args));
  for i := 0 to Length(Args) - 1 do
    Result[i] := Args[i];
end;
(*
function TScHubConnection.ArrayConstsToVariant(const Args: array of const): TVariantArray;
var
  i: integer;
begin
  SetLength(Result, Length(Args));
  for i := 0 to Length(Args) - 1 do begin
    case Args[i].VType of
      vtInteger: begin
        TVarData(Result[i]).VType := varInteger;
        TVarData(Result[i]).VInteger := Args[i].VInteger;
      end;
      vtBoolean: begin
        TVarData(Result[i]).VType := varBoolean;
        TVarData(Result[i]).VBoolean := Args[i].VBoolean;
      end;
      vtChar: begin
        TVarData(Result[i]).VType := {$IFDEF VER12P}varUString{$ELSE}varOleStr{$ENDIF};
        TVarData(Result[i]).{$IFDEF NEXTGEN}VOleStr{$ELSE}{$IFDEF VER12P}VUString{$ELSE}VOleStr{$ENDIF}{$ENDIF} := {$IFNDEF NEXTGEN}@Args[i].VChar{$ELSE}Args[i].VPWideChar{$ENDIF};
      end;
      vtExtended: begin
        TVarData(Result[i]).VType := varDouble;
        TVarData(Result[i]).VDouble := Extended(Args[i].VExtended^);
      end;
    {$IFNDEF NEXTGEN}
      vtString: begin
        TVarData(Result[i]).VType := {$IFDEF VER12P}varUString{$ELSE}varString{$ENDIF};
        TVarData(Result[i]).{$IFDEF VER12P}VUString{$ELSE}VString{$ENDIF} := Args[i].VString;
      end;
    {$ENDIF}
      vtPointer: begin
        TVarData(Result[i]).VType := varByRef;
        TVarData(Result[i]).VPointer := Args[i].VPointer;
      end;
      vtPChar: begin
        TVarData(Result[i]).VType := {$IFDEF VER12P}varUString{$ELSE}varString{$ENDIF};
        TVarData(Result[i]).{$IFDEF NEXTGEN}VOleStr{$ELSE}{$IFDEF VER12P}VUString{$ELSE}VString{$ENDIF}{$ENDIF} := Args[i].{$IFNDEF NEXTGEN}VPChar{$ELSE}VPWideChar{$ENDIF};
      end;
      vtObject: begin
        TVarData(Result[i]).VType := varByRef;
        TVarData(Result[i]).VPointer := Args[i].VObject;
      end;
      vtClass: begin
        TVarData(Result[i]).VType := varByRef;
        TVarData(Result[i]).VPointer := Args[i].VClass;
      end;
      vtWideChar: begin
        TVarData(Result[i]).VType := varOleStr;
        TVarData(Result[i]).VOleStr := @Args[i].VWideChar;
      end;
      vtPWideChar: begin
        TVarData(Result[i]).VType := varOleStr;
        TVarData(Result[i]).VOleStr := Args[i].VPWideChar;
      end;
      vtAnsiString: begin
        TVarData(Result[i]).VType := {$IFDEF VER12P}varUString{$ELSE}varOleStr{$ENDIF};
        TVarData(Result[i]).{$IFDEF NEXTGEN}VOleStr{$ELSE}{$IFDEF VER12P}VUString{$ELSE}VOleStr{$ENDIF}{$ENDIF} := Args[i].{$IFNDEF NEXTGEN}VAnsiString{$ELSE}VPWideChar{$ENDIF};
      end;
      vtCurrency: begin
        TVarData(Result[i]).VType := varCurrency;
        TVarData(Result[i]).VCurrency := Args[i].VCurrency^;
      end;
      vtVariant: begin
        Result[i] := Args[i].VVariant^;
      end;
      vtInterface: begin
        TVarData(Result[i]).VType := varByRef;
        TVarData(Result[i]).VPointer := Args[i].VInterface;
      end;
      vtWideString: begin
        TVarData(Result[i]).VType := varOleStr;
        TVarData(Result[i]).VOleStr := Args[i].VWideString;
      end;
      vtInt64: begin
        TVarData(Result[i]).VType := varInt64;
        TVarData(Result[i]).VInt64 := Args[i].VInt64^;
      end;
    {$IFDEF VER12P}
      vtUnicodeString: begin
        TVarData(Result[i]).VType := varUString;
        TVarData(Result[i]).VUString := Args[i].VUnicodeString;
      end;
    {$ENDIF}
    else
      TVarData(Result[i]).VType := varAny;
      TVarData(Result[i]).VAny := Args[i].VPointer;
    end;
  end;
end;
*)
procedure TScHubConnection.InvokeObj(const MethodName: string; const Args: array of TObject;
  const ReturnType: TVarType; out Res: Variant);
var
  vArgs: TVariantArray;
begin
  vArgs := ArrayObjectsToVariant(Args);
  InternalInvoke(MethodName, vArgs, False, ReturnType, nil, Res);
end;

procedure TScHubConnection.InvokeObj(const MethodName: string; const Args: array of TObject;
  const ReturnClass: TClass; out Res: TObject);
var
  vArgs: TVariantArray;
  vRes: Variant;
begin
  if ReturnClass = nil then
    raise ArgumentException.CreateFmt(SInvalidInputArg, ['ReturnClass']);

  vArgs := ArrayObjectsToVariant(Args);
  InternalInvoke(MethodName, vArgs, False, varByRef, ReturnClass, vRes);

  if VarIsNull(vRes) or VarIsEmpty(vRes) then begin
    Res := nil;
    Exit;
  end;

  if TVarData(vRes).VType <> varByRef then
    raise InvalidOperationException.Create(SUnexpectedResultType);
  Res := TObject(TVarData(vRes).VPointer);
end;

procedure TScHubConnection.InvokeObj(const MethodName: string; const Args: array of TObject);
var
  vArgs: TVariantArray;
  Res: Variant;
begin
  vArgs := ArrayObjectsToVariant(Args);
  InternalInvoke(MethodName, vArgs, False, varUnknown, nil, Res);
end;

procedure TScHubConnection.InvokeArr(const MethodName: string; const Args: array of Variant;
  const ReturnType: TVarType; out Res: Variant);
var
  vArgs: TVariantArray;
begin
  vArgs := ArrayConstsToVariant(Args);
  InternalInvoke(MethodName, vArgs, True, ReturnType, nil, Res);
end;

procedure TScHubConnection.InvokeArr(const MethodName: string; const Args: array of Variant;
  const ReturnClass: TClass; out Res: TObject);
var
  vArgs: TVariantArray;
  vRes: Variant;
begin
  if ReturnClass = nil then
    raise ArgumentException.CreateFmt(SInvalidInputArg, ['ReturnClass']);

  vArgs := ArrayConstsToVariant(Args);
  InternalInvoke(MethodName, vArgs, True, varByRef, ReturnClass, vRes);

  if VarIsNull(vRes) or VarIsEmpty(vRes) then begin
    Res := nil;
    Exit;
  end;

  if TVarData(vRes).VType <> varByRef then
    raise InvalidOperationException.Create(SUnexpectedResultType);
  Res := TObject(TVarData(vRes).VPointer);
end;

procedure TScHubConnection.InvokeArr(const MethodName: string; const Args: array of Variant);
var
  vArgs: TVariantArray;
  Res: Variant;
begin
  vArgs := ArrayConstsToVariant(Args);
  InternalInvoke(MethodName, vArgs, True, varUnknown, nil, Res);
end;

procedure TScHubConnection.Invoke(const MethodName: string; const Args: array of Variant;
  const ReturnType: TVarType; out Res: Variant);
var
  vArgs: TVariantArray;
begin
  vArgs := ArrayConstsToVariant(Args);
  InternalInvoke(MethodName, vArgs, False, ReturnType, nil, Res);
end;

procedure TScHubConnection.Invoke(const MethodName: string; const Args: array of Variant;
  const ReturnClass: TClass; out Res: TObject);
var
  vArgs: TVariantArray;
  vRes: Variant;
begin
  if ReturnClass = nil then
    raise ArgumentException.CreateFmt(SInvalidInputArg, ['ReturnClass']);

  vArgs := ArrayConstsToVariant(Args);
  InternalInvoke(MethodName, vArgs, False, varByRef, ReturnClass, vRes);

  if VarIsNull(vRes) or VarIsEmpty(vRes) then begin
    Res := nil;
    Exit;
  end;

  if TVarData(vRes).VType <> varByRef then
    raise InvalidOperationException.Create(SUnexpectedResultType);
  Res := TObject(TVarData(vRes).VPointer);
end;

procedure TScHubConnection.Invoke(const MethodName: string; const Args: array of Variant);
var
  vArgs: TVariantArray;
  Res: Variant;
begin
  vArgs := ArrayConstsToVariant(Args);
  InternalInvoke(MethodName, vArgs, False, varUnknown, nil, Res);
end;

procedure TScHubConnection.SendObj(const MethodName: string; const Args: array of TObject);
var
  vArgs: TVariantArray;
begin
  vArgs := ArrayObjectsToVariant(Args);
  InternalSend(MethodName, vArgs, False);
end;

procedure TScHubConnection.SendArr(const MethodName: string; const Args: array of Variant);
var
  vArgs: TVariantArray;
begin
  vArgs := ArrayConstsToVariant(Args);
  InternalSend(MethodName, vArgs, True);
end;

procedure TScHubConnection.Send(const MethodName: string; const Args: array of Variant);
var
  vArgs: TVariantArray;
begin
  vArgs := ArrayConstsToVariant(Args);
  InternalSend(MethodName, vArgs, False);
end;

procedure TScHubConnection.InternalStart(CancellationToken: TScCancellationToken);
var
  StartingConnectionState: IScConnectionState;
  Connection: TScHttpConnection;
begin
  FState.AssertInConnectionLock;
  if FState.FCurrentConnectionStateUnsynchronized <> nil then
    raise InvalidOperationException.Create(SHubConnectionAlreadyConnected);

  CancellationToken.ThrowIfCancellationRequested;

  Log.LogDebug(FLogger, SHubConnection_Starting);

  Connection := TScHttpConnection.Create(FHttpConnectionOptions, FLogger);
  try
    Connection.Start(FProtocol.TransferFormat, CancellationToken);
  except
    Connection.Free;
    raise;
  end;

  StartingConnectionState := TScConnectionState.Create(Connection, Self, FLogger);

  // From here on, if an error occurs we need to shut down the connection because we still own it.
  try
    Log.LogInformation(FLogger, SHubConnection_HubProtocol, [FProtocol.Name, FProtocol.Version]);
    Handshake(StartingConnectionState, CancellationToken);
  except
    on E: Exception do begin
      Log.LogError(FLogger, SHubConnection_ErrorStartingConnection, E);

      // Can not have any invocations to cancel, we are in the lock.
      Close(StartingConnectionState.Connection);
      StartingConnectionState := nil;
      raise;
    end;
  end;

  // Set this at the end to avoid setting internal state until the connection is real
  FState.FCurrentConnectionStateUnsynchronized := StartingConnectionState;

  StartingConnectionState.PreparePing;
  StartingConnectionState.SetReceiveTask(TScReceiveLoop.Create(Self, StartingConnectionState));

  Log.LogInformation(FLogger, SHubConnection_Started);
end;

procedure TScHubConnection.Close(Connection: TScHttpConnection);
begin
  Connection.Dispose;
end;

procedure TScHubConnection.InternalInvoke(const MethodName: string; const Args: TVariantArray; AsArray: boolean;
  const ReturnType: TVarType; const ReturnClass: TClass; out Res: Variant);
var
  ConnectionState: IScConnectionState;
  InvocationCaller: TScSyncInvocationCaller;
  InvocationRequest, tmpInvocationRequest: TScInvocationRequest;
  InvocationMessage: TScInvocationMessage;
begin
  InvocationCaller := TScSyncInvocationCaller.Create;
  try
    ConnectionState := FState.WaitForActiveConnection;
    try
      InvocationRequest := TScInvocationRequest.Invoke(FLogger, ReturnType, ReturnClass, ConnectionState.GetNextId, InvocationCaller);

      try
        Log.LogDebug(FLogger, SHubConnection_RegisteringInvocation, [InvocationRequest.InvocationId]);
        ConnectionState.AddInvocation(InvocationRequest);
      except
        InvocationRequest.Free;
        raise;
      end;

      Log.LogDebug(FLogger, SHubConnection_PreparingBlockingInvocation, [InvocationRequest.InvocationId, MethodName, IntToStr(InvocationRequest.ResultType), Length(Args)]);
      InvocationMessage := TScInvocationMessage.Create(InvocationRequest.InvocationId, MethodName, Args, AsArray, False);

      try
        try
          Log.LogDebug(FLogger, SHubConnection_IssuingInvocation, [InvocationMessage.InvocationId, IntToStr(InvocationRequest.ResultType), MethodName, Length(Args)]);
          SendHubMessage(ConnectionState, InvocationMessage);
        except
          on E: Exception do begin
            Log.LogError(FLogger, SHubConnection_FailedToSendInvocation, [InvocationMessage.InvocationId], E);
            ConnectionState.TryRemoveInvocation(InvocationMessage.InvocationId, tmpInvocationRequest);
            try
              InvocationRequest.Fail(E);
            finally
              InvocationRequest.Free; // tmpInvocationRequest == InvocationRequest
            end;
          end;
        end;
      finally
        InvocationMessage.Free;
      end;
    finally
      FState.ReleaseConnectionLock;
    end;

    // Wait for this outside the lock, because it won't complete until the server responds.
    if not InvocationCaller.WaitFor(FInvokeTimeoutTicks) then
      raise HubException.CreateFmt(SServerTimeoutWithoutReceivingMessage, [FInvokeTimeoutTicks]);

    Res := InvocationCaller.Result;
  finally
    InvocationCaller.Free;
  end;
end;

procedure TScHubConnection.InternalSend(const MethodName: string; const Args: TVariantArray; AsArray: boolean);
var
  ConnectionState: IScConnectionState;
  InvocationMessage: TScInvocationMessage;
begin
  /// Invokes a hub method on the server using the specified method name and arguments.
  ConnectionState := FState.WaitForActiveConnection;
  try
    Log.LogDebug(FLogger, SHubConnection_PreparingNonBlockingInvocation, [MethodName, Length(Args)]);
    InvocationMessage := TScInvocationMessage.Create('', MethodName, Args, AsArray, False);
    try
      SendHubMessage(ConnectionState, InvocationMessage);
    finally
      InvocationMessage.Free;
    end;
  finally
    FState.ReleaseConnectionLock;
  end;
end;

procedure TScHubConnection.SendHubMessage(ConnectionState: IScConnectionState; HubMessage: TScHubMessage);
var
  OutputStream: TScPipeWriterStream;
begin
  FState.AssertConnectionValid;

  OutputStream := TScPipeWriterStream.Create(ConnectionState.Connection.Transport.Output);
  try
    FProtocol.WriteMessage(HubMessage, OutputStream);
  finally
    OutputStream.Free;
  end;

  Log.LogDebug(FLogger, SHubConnection_SendingMessage, [HubMessage.ClassName]);
  ConnectionState.Connection.Transport.Output.Flush;
  Log.LogDebug(FLogger, SHubConnection_MessageSent, [HubMessage.ClassName]);

  // We've sent a message, so don't ping for a while
  ConnectionState.ResetSendPing;
end;

procedure TScHubConnection.ProcessMessages(ConnectionState: IScConnectionState;
  HubMessage: TScHubMessage; out Close: boolean; out E: Exception);
var
  InvocationRequest: TScInvocationRequest;
  BindingFailure: TScInvocationBindingFailureMessage;
  Invocation: TScInvocationMessage;
  Completion: TScCompletionMessage;
  StreamItem: TScStreamItemMessage;
  CloseMessage: TScCloseMessage;
begin
  Log.LogDebug(FLogger, SHubConnection_ResettingKeepAliveTimer);
  ConnectionState.ResetTimeout;

  E := nil;
  Close := False;

  if HubMessage is TScInvocationBindingFailureMessage then begin
    BindingFailure := TScInvocationBindingFailureMessage(HubMessage);
    // The server can not receive a response, so we just drop the message and log
    Log.LogError(FLogger, SHubConnection_ArgumentBindingFailure, [BindingFailure.InvocationId, BindingFailure.Target], BindingFailure.BindingFailure);
  end
  else
  if HubMessage is TScInvocationMessage then begin
    Invocation := TScInvocationMessage(HubMessage);
    Log.LogDebug(FLogger, SHubConnection_ReceivedInvocation, [Invocation.InvocationId, Invocation.Target, Length(Invocation.Arguments)]);
    DispatchInvocation(Invocation);
  end
  else
  if HubMessage is TScCompletionMessage then begin
    Completion := TScCompletionMessage(HubMessage);
    if not ConnectionState.TryRemoveInvocation(Completion.InvocationId, InvocationRequest) then
      Log.LogWarning(FLogger, SHubConnection_DroppedCompletionMessage, [Completion.InvocationId])
    else begin
      try
        DispatchInvocationCompletion(Completion, InvocationRequest);
      finally
        InvocationRequest.Free;
      end;
    end;
  end
  else
  if HubMessage is TScStreamItemMessage then begin
    StreamItem := TScStreamItemMessage(HubMessage);
    if not ConnectionState.TryGetInvocation(StreamItem.InvocationId, InvocationRequest) then
      Log.LogWarning(FLogger, SHubConnection_DroppedStreamMessage, [StreamItem.InvocationId])
    else
      DispatchInvocationStreamItem(StreamItem, InvocationRequest);
  end
  else
  if HubMessage is TScCloseMessage then begin
    CloseMessage := TScCloseMessage(HubMessage);
    Close := True;

    if CloseMessage.Error = '' then begin
      Log.LogDebug(FLogger, SHubConnection_ReceivedClose);
    end
    else begin
      Log.LogError(FLogger, SHubConnection_ReceivedCloseWithError, [CloseMessage.Error], nil);
      E := HubException.CreateFmt(SServerClosedWithError, [CloseMessage.Error]);
    end;
  end
  else
  if HubMessage is TScPingMessage then begin
    Log.LogDebug(FLogger, SHubConnection_ReceivedPing);
    // timeout is reset above, on receiving any message
  end
  else
    raise InvalidOperationException.CreateFmt(SUnexpectedMessageType, [HubMessage.ClassName]);
end;

procedure TScHubConnection.DispatchInvocation(Invocation: TScInvocationMessage);
var
  InvocationHandler: TScInvocationHandler;
  HandleInvocationAsyncEvent: TScHubHandleInvocationAsyncEvent;
begin
  InvocationHandler := GetHandler(Invocation.Target);
  if InvocationHandler = nil then begin
    Log.LogWarning(FLogger, SHubConnection_MissingHandler, [Invocation.Target]);
    Exit;
  end;

  HandleInvocationAsyncEvent := TScHubHandleInvocationAsyncEvent.Create(Self, InvocationHandler, Invocation);
  HandleInvocationAsyncEvent.Notify(FEventsCallMode);
end;

procedure TScHubConnection.HandleInvocation(Handler: TScInvocationHandler; Invocation: TScInvocationMessage);
begin
  try
    Handler.Invoke(Invocation.Arguments);
  except
    on E: Exception do
      Log.LogError(FLogger, SHubConnection_ErrorInvokingClientSideMethod, [Invocation.Target], E);
  end;
end;

procedure TScHubConnection.DispatchInvocationStreamItem(StreamItem: TScStreamItemMessage; InvocationRequest: TScInvocationRequest);
var
  HandleInvocationStreamItemAsyncEvent: TScHubHandleInvocationStreamItemAsyncEvent;
begin
  Log.LogDebug(FLogger, SHubConnection_ReceivedStreamItem, [StreamItem.InvocationId]);

  if InvocationRequest.IsCanceled then
    Log.LogDebug(FLogger, SHubConnection_CancelingStreamItem, [InvocationRequest.InvocationId])
  else begin
    HandleInvocationStreamItemAsyncEvent := TScHubHandleInvocationStreamItemAsyncEvent.Create(Self, InvocationRequest, StreamItem);
    HandleInvocationStreamItemAsyncEvent.Notify(FEventsCallMode);
  end;
end;

procedure TScHubConnection.HandleInvocationStreamItem(InvocationRequest: TScInvocationRequest; StreamItem: TScStreamItemMessage);
begin
  if not InvocationRequest.StreamItem(StreamItem) then
    Log.LogWarning(FLogger, SHubConnection_ReceivedStreamItemAfterClose, [InvocationRequest.InvocationId]);
end;

procedure TScHubConnection.DispatchInvocationCompletion(Completion: TScCompletionMessage; InvocationRequest: TScInvocationRequest);
var
  HandleInvocationCompletionAsyncEvent: TScHubHandleInvocationCompletionAsyncEvent;
begin
  Log.LogDebug(FLogger, SHubConnection_ReceivedInvocationCompletion, [Completion.InvocationId]);

  if InvocationRequest.IsCanceled then
    Log.LogDebug(FLogger, SHubConnection_CancelingInvocationCompletion, [InvocationRequest.InvocationId])
  else begin
    HandleInvocationCompletionAsyncEvent := TScHubHandleInvocationCompletionAsyncEvent.Create(Self, InvocationRequest, Completion);
    HandleInvocationCompletionAsyncEvent.Notify(FEventsCallMode);
  end;
end;

procedure TScHubConnection.HandleInvocationCompletion(InvocationRequest: TScInvocationRequest; Completion: TScCompletionMessage);
begin
  InvocationRequest.Complete(Completion);
end;

procedure TScHubConnection.Handshake(StartingConnectionState: IScConnectionState; CancellationToken: TScCancellationToken);
var
  HandshakeRequest: TScHandshakeRequestMessage;
  SendHandshakeResult: TScFlushResult;
  Ex: Exception;
  ReadResult: TScReadResult;
  Message: TScHandshakeResponseMessage;
  Consumed, Examined: TScSequencePosition;
begin
  Log.LogDebug(FLogger, SHubConnection_SendingHubHandshake);

  HandshakeRequest := TScHandshakeRequestMessage.Create(FProtocol.Name, FProtocol.Version);
  try
    TScHandshakeProtocol.WriteRequestMessage(HandshakeRequest, StartingConnectionState.Connection.Transport.Output);
  finally
    HandshakeRequest.Free;
  end;

  SendHandshakeResult := StartingConnectionState.Connection.Transport.Output.Flush(INFINITE, CancellationToken);
  if SendHandshakeResult.IsCompleted then begin
    // The other side disconnected
    Ex := InvalidOperationException.Create(SServerDisconnectedBeforeStarted);
    Log.LogError(FLogger, SHubConnection_ErrorReceivingHandshakeResponse, Ex);
    raise Ex;
  end;

  try
    while True do begin
      Consumed := DefaultSequencePosition;
      Examined := DefaultSequencePosition;
      ReadResult := StartingConnectionState.Connection.Transport.Input.Read(FHandshakeTimeoutTicks, CancellationToken);
      try
        try
          if ReadResult.Buffer <> nil then begin
            Consumed := ReadResult.Buffer.StartPos;
            Examined := ReadResult.Buffer.EndPos;
          end;

          // Read first message out of the incoming data
          if (ReadResult.Buffer <> nil) and not ReadResult.Buffer.IsEmpty then begin
            Message := nil;
            try
              if TScHandshakeProtocol.TryParseResponseMessage(ReadResult.Buffer, Message) then begin
                // Adjust consumed and examined to point to the end of the handshake
                // response, this handles the case where invocations are sent in the same payload
                // as the negotiate response.
                Consumed := ReadResult.Buffer.StartPos;
                Examined := Consumed;

                if Message.Error <> '' then begin
                  Log.LogError(FLogger, SHubConnection_HandshakeServerError, [Message.Error], nil);
                  raise HubException.CreateFmt(SUnableToCompleteHandshake, [Message.Error]);
                end;

                Log.LogDebug(FLogger, SHubConnection_HandshakeComplete);
                Break;
              end;
            finally
              Message.Free;
            end;
          end;

          if ReadResult.IsCompleted then
            // Not enough data, and we won't be getting any more data.
            raise InvalidOperationException.Create(SServerDisconnectedBeforeSendingHandshakeResponse);
        finally
          StartingConnectionState.Connection.Transport.Input.AdvanceTo(Consumed, Examined);
        end;
      finally
        ReadResult.Buffer.Free;
      end;
    end;
  except
    on HubException do begin
      // This was already logged as a HandshakeServerError
      raise;
    end;

    on E1: InvalidDataException do begin
      Log.LogError(FLogger, SHubConnection_ErrorInvalidHandshakeResponse, E1);
      raise;
    end;

    on E2: OperationCanceledException do begin
      if not CancellationToken.IsCancellationRequested then
        Log.LogError(FLogger, SHubConnection_ErrorHandshakeTimedOut, [HandshakeTimeout], E2)
      else
        Log.LogError(FLogger, SHubConnection_ErrorHandshakeCanceled, E2);
      raise;
    end;

    on E3: Exception do begin
      Log.LogError(FLogger, SHubConnection_ErrorReceivingHandshakeResponse, E3);
      raise;
    end;
  end;
end;

procedure TScHubConnection.ReceiveLoop(ConnectionState: IScConnectionState);
var
  Timer: TScIntervalThread;
  ReadResult: TScReadResult;
  HubMessage: TScHubMessage;
  Close: boolean;
  Ex: Exception;
begin
  Log.LogDebug(FLogger, SHubConnection_ReceiveLoopStarting);

  // Performs periodic tasks - here sending pings and checking timeout.
  Timer := TScIntervalThread.Create(nil);
  try
    try
      Timer.Interval := FTickRateTicks;
      Timer.OnTimer := ConnectionState.GetTimerLoopProc;
      Timer.Enabled := True;

      try
        while True do begin
          ReadResult := ConnectionState.Connection.Transport.Input.Read;
          try
            try
              if ReadResult.IsCanceled then begin
                // We were canceled. Possibly because we were stopped gracefully
                Break;
              end
              else
              if (ReadResult.Buffer <> nil) and not ReadResult.Buffer.IsEmpty then begin
                Log.LogDebug(FLogger, SHubConnection_ProcessingMessage, [ReadResult.Buffer.Length]);

                Close := False;
                while FProtocol.TryParseMessage(ReadResult.Buffer, ConnectionState, HubMessage) do begin
                  try
                    // We have data, process it
                    ProcessMessages(ConnectionState, HubMessage, Close, Ex);
                    if Ex <> nil then
                      ConnectionState.SetCloseException(Ex);
                  finally
                    HubMessage.Free;
                  end;

                  if Close then begin
                    ConnectionState.Stopping := True;
                    Break;
                  end;
                end;

                // If we're closing stop everything
                if Close then
                  Break;
              end;

              if ReadResult.IsCompleted then begin
                if (ReadResult.Buffer <> nil) and not ReadResult.Buffer.IsEmpty then
                  raise InvalidDataException.Create(SConnectionTerminatedWhileReadingMessage);
                Break;
              end;
            finally
              // The buffer was sliced up to where it was consumed, so we can just advance to the start.
              // We mark examined as Buffer.EndPos so that if we didn't receive a full frame, we'll wait for more data
              // before yielding the read again.
              if ReadResult.Buffer <> nil then
                ConnectionState.Connection.Transport.Input.AdvanceTo(ReadResult.Buffer.StartPos, ReadResult.Buffer.EndPos);
            end;
          finally
            ReadResult.Buffer.Free;
          end;
        end;

      except
        on E: Exception do begin
          Log.LogError(FLogger, SHubConnection_ServerDisconnectedWithError, E);
          ConnectionState.SetCloseException(CloneException(E));
        end;
      end;
    finally
      Timer.Free;
    end;
  finally
    HandleConnectionClose(ConnectionState);
  end;
end;

procedure TScHubConnection.HandleConnectionClose(ConnectionState: IScConnectionState);
begin
  // Clear the ConnectionState field
  FState.WaitConnectionLock;
  try
    if FState.FCurrentConnectionStateUnsynchronized <> ConnectionState then
      raise InvalidOperationException.Create(SSomeoneClearedConnectionState);
    FState.FCurrentConnectionStateUnsynchronized := nil;

    try
      // Dispose the connection
      Close(ConnectionState.Connection);

      // Cancel any outstanding invocations within the connection lock
      ConnectionState.CancelOutstandingInvocations(ConnectionState.CloseException);
    finally
      if ConnectionState.Stopping or (FReconnectPolicy = nil) then begin
        if ConnectionState.CloseException <> nil then
          Log.LogError(FLogger, SHubConnection_ShutdownWithError, ConnectionState.CloseException)
        else
          Log.LogDebug(FLogger, SHubConnection_ShutdownConnection);

        FState.ChangeState(hcsConnected, hcsDisconnected);
        CompleteClose(ConnectionState.CloseException);
      end
      else begin
        if GetNextRetryDelay(0, 0, nil) = {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}(-1) then begin
          Log.LogWarning(FLogger, SHubConnection_FirstReconnectRetryDelayNull);
          FState.ChangeState(hcsConnected, hcsDisconnected);
          CompleteClose(ConnectionState.CloseException);
        end
        else begin
          FState.ChangeState(hcsConnected, hcsReconnecting);
          FreeAndNil(FState.FReconnectTask);
          FState.FReconnectTask := TScReconnectThread.Create(Self, ConnectionState.CloseException);
        end;
      end;
    end;
  finally
    FState.ReleaseConnectionLock;
  end;
end;

procedure TScHubConnection.CompleteClose(CloseException: Exception);
var
  OnClosedAsyncEvent: TScOnHubClosedAsyncEvent;
begin
  FState.AssertInConnectionLock;
  FState.StopCts.ReInit;

  if Assigned(FAfterDisconnect) then begin
    OnClosedAsyncEvent := TScOnHubClosedAsyncEvent.Create(Self, CloneException(CloseException));
    OnClosedAsyncEvent.Notify(FEventsCallMode);
  end;
end;

procedure TScHubConnection.Reconnect(CloseException: Exception);
var
  PreviousReconnectAttempts: integer;
  ReconnectStartTime, ElapsedTime: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
  NextRetryDelay: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
  RetryReason: Exception;
  OnReconnectingAsyncEvent: TScOnHubReconnectingAsyncEvent;
  OnReconnectedAsyncEvent: TScOnHubReconnectedAsyncEvent;
begin
  if CloseException <> nil then
    Log.LogError(FLogger, SHubConnection_ReconnectingWithError, CloseException)
  else
    Log.LogInformation(FLogger, SHubConnection_Reconnecting);

  if Assigned(FBeforeReconnect) then begin
    OnReconnectingAsyncEvent := TScOnHubReconnectingAsyncEvent.Create(Self, CloneException(CloseException));
    OnReconnectingAsyncEvent.Notify(FEventsCallMode);
  end;

  PreviousReconnectAttempts := 0;
  ReconnectStartTime := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
  RetryReason := CloneException(CloseException);
  try
    NextRetryDelay := GetNextRetryDelay(PreviousReconnectAttempts, 0, RetryReason);
    Inc(PreviousReconnectAttempts);

    while NextRetryDelay <> {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}(-1) do begin
      Log.LogDebug(FLogger, SHubConnection_AwaitingReconnectRetryDelay, [PreviousReconnectAttempts, NextRetryDelay]);

      try
        FState.StopCts.Delay(NextRetryDelay);
      except
        on E: OperationCanceledException do begin
          Log.LogDebug(FLogger, SHubConnection_ReconnectingStoppedDuringRetryDelay);

          FState.WaitConnectionLock;
          try
            FState.ChangeState(hcsReconnecting, hcsDisconnected);
            CompleteClose(OperationCanceledException.Create(SConnectionStoppedDuringReconnectDelay + #13#10 + E.Message));
          finally
            FState.ReleaseConnectionLock;
          end;
          Exit;
        end;
      end;

      FState.WaitConnectionLock;
      try
        try
          if FState.FCurrentConnectionStateUnsynchronized <> nil then
            raise InvalidOperationException.Create(SSomeoneSetConnectionState);

          InternalStart(FState.StopCts);

          Log.LogInformation(FLogger, SHubConnection_Reconnected, [PreviousReconnectAttempts, {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF} - ReconnectStartTime]);

          FState.ChangeState(hcsReconnecting, hcsConnected);
          if Assigned(FAfterReconnect) then begin
            OnReconnectedAsyncEvent := TScOnHubReconnectedAsyncEvent.Create(Self, ConnectionId);
            OnReconnectedAsyncEvent.Notify(FEventsCallMode);
          end;

          Exit;
        except
          on E: Exception do begin
            FreeAndNil(RetryReason);
            RetryReason := CloneException(E);

            Log.LogError(FLogger, SHubConnection_ReconnectAttemptFailed, E);

            if FState.StopCts.IsCancellationRequested then begin
              Log.LogDebug(FLogger, SHubConnection_ReconnectingStoppedDuringReconnectAttempt);
              FState.ChangeState(hcsReconnecting, hcsDisconnected);
              CompleteClose(OperationCanceledException.Create(SConnectionStoppedDuringReconnectAttempt + #13#10 + E.Message));
              Exit;
            end;
          end;
        end;
      finally
        FState.ReleaseConnectionLock;
      end;

      NextRetryDelay := GetNextRetryDelay(PreviousReconnectAttempts, {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF} - ReconnectStartTime, RetryReason);
      Inc(PreviousReconnectAttempts);
    end;
  finally
    RetryReason.Free;
  end;

  FState.WaitConnectionLock;
  try
    if FState.FCurrentConnectionStateUnsynchronized <> nil then
      raise InvalidOperationException.Create(SSomeoneSetConnectionState);

    ElapsedTime := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF} - ReconnectStartTime;
    Log.LogInformation(FLogger, SHubConnection_ReconnectAttemptsExhausted, [PreviousReconnectAttempts, ElapsedTime]);
    FState.ChangeState(hcsReconnecting, hcsDisconnected);
    CompleteClose(OperationCanceledException.CreateFmt(SReconnectRetriesExhausted, [PreviousReconnectAttempts, ElapsedTime]));
  finally
    FState.ReleaseConnectionLock;
  end;
end;

function TScHubConnection.GetNextRetryDelay(PreviousRetryCount: integer; ElapsedTime: {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
  RetryReason: Exception): {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF};
var
  RetryContext: TScRetryContext;
begin
  Assert(Assigned(FReconnectPolicy));

  try
    RetryContext.PreviousRetryCount := PreviousRetryCount;
    RetryContext.ElapsedTime := ElapsedTime;
    RetryContext.RetryReason := RetryReason;
    Result := FReconnectPolicy.NextRetryDelay(RetryContext);
  except
    on E: Exception do begin
      Log.LogError(FLogger, SHubConnection_ErrorDuringNextRetryDelay, E);
      Result := {$IFNDEF FPC}cardinal{$ELSE}UInt64{$ENDIF}(-1);
    end;
  end;
end;

procedure TScHubConnection.DoAfterDisconnect(Ex: Exception);
begin
  try
    Log.LogDebug(FLogger, SHubConnection_InvokingClosedEventHandler);
    if Assigned(FAfterDisconnect) then
      FAfterDisconnect(Self, Ex);
  except
    on E: Exception do
      Log.LogError(FLogger, SHubConnection_ErrorDuringClosedEvent, E);
  end;
end;

procedure TScHubConnection.DoBeforeReconnect(Ex: Exception);
begin
  try
    if Assigned(FBeforeReconnect) then
      FBeforeReconnect(Self, Ex);
  except
    on E: Exception do
      Log.LogError(FLogger, SHubConnection_ErrorDuringReconnectingEvent, E);
  end;
end;

procedure TScHubConnection.DoAfterReconnect(const ConnectionId: string);
begin
  try
    if Assigned(FAfterReconnect) then
      FAfterReconnect(Self, ConnectionId);
  except
    on E: Exception do
      Log.LogError(FLogger, SHubConnection_ErrorDuringReconnectedEvent, E);
  end;
end;

{ TScInvocationHandler }

constructor TScInvocationHandler.Create(AOwner: TScHubConnection; const AParameterTypes: TExtTypeInfoArray;
  AAsArray: boolean; const ACallback: TScInvocationHandlerCallback);
begin
  inherited Create;

  FOwner := AOwner;
  FParameterTypes := AParameterTypes;
  FAsArray := AAsArray;
  FCallback := ACallback;
end;

procedure TScInvocationHandler.Invoke(const Parameters: TVariantArray);
begin
  if Assigned(FCallback) then
    FCallback(FOwner, Parameters);
end;

{ TScConnectionState }

constructor TScConnectionState.Create(AConnection: TScHttpConnection; AHubConnection: TScHubConnection; ALogger: TScLogger);
begin
  inherited Create;

  FLogger := ALogger;
  FHubConnection := AHubConnection;
  FConnection := AConnection;

  FLock := TCriticalSection.Create;
  FPendingCalls := TStringList.Create;

  FNextInvocationId := 0;
  FHasInherentKeepAlive := FConnection.HasInherentKeepAlive;
end;

destructor TScConnectionState.Destroy;
var
  i: integer;
begin
  FReceiveTask.Free;

  for i := 0 to FPendingCalls.Count - 1 do
  {$IFNDEF NEXTGEN}
    FPendingCalls.Objects[i].Free;
  {$ELSE}
    FPendingCalls.Objects[i] := nil;
  {$ENDIF}

  FCloseException.Free;
  FConnection.Free;
  FLock.Free;
  FPendingCalls.Free;

  inherited;
end;

function TScConnectionState.GetConnection: TScHttpConnection;
begin
  Result := FConnection;
end;

function TScConnectionState.GetCloseException: Exception;
begin
  Result := FCloseException;
end;

procedure TScConnectionState.SetCloseException(E: Exception);
begin
  FreeAndNil(FCloseException);
  FCloseException := E;
end;

function TScConnectionState.GetStopping: boolean;
begin
  Result := FStopping;
end;

procedure TScConnectionState.SetStopping(Value: boolean);
begin
  FStopping := Value;
end;

procedure TScConnectionState.SetReceiveTask(Value: TThread);
begin
  Assert(FReceiveTask = nil);
  FReceiveTask := Value;
end;

function TScConnectionState.GetNextId: string;
begin
  Result := IntToStr(InterlockedIncrement(FNextInvocationId));
end;

procedure TScConnectionState.AddInvocation(InvocationRequest: TScInvocationRequest);
begin
  FLock.Enter;
  try
    if FPendingCalls.IndexOf(InvocationRequest.InvocationId) > -1 then begin
      Log.LogDebug(FLogger, SHubConnection_InvocationAlreadyInUse, [InvocationRequest.InvocationId]);
      raise InvalidOperationException.CreateFmt(SInvocationIdUsed, [InvocationRequest.InvocationId])
    end
    else
      FPendingCalls.AddObject(InvocationRequest.InvocationId, InvocationRequest);
  finally
    FLock.Leave;
  end;
end;

function TScConnectionState.TryGetInvocation(const InvocationId: string; out InvocationRequest: TScInvocationRequest): boolean;
var
  Index: integer;
begin
  FLock.Enter;
  try
    Index := FPendingCalls.IndexOf(InvocationId);
    Result := Index > -1;

    if Result then
      InvocationRequest := TScInvocationRequest(FPendingCalls.Objects[Index])
    else
      InvocationRequest := nil;
  finally
    FLock.Leave;
  end;
end;

function TScConnectionState.TryRemoveInvocation(const InvocationId: string; out InvocationRequest: TScInvocationRequest): boolean;
var
  Index: integer;
begin
  FLock.Enter;
  try
    Index := FPendingCalls.IndexOf(InvocationId);
    Result := Index > -1;

    if Result then begin
      InvocationRequest := TScInvocationRequest(FPendingCalls.Objects[Index]);
      FPendingCalls.Delete(Index);
    end
    else
      InvocationRequest := nil;
  finally
    FLock.Leave;
  end;
end;

procedure TScConnectionState.CancelOutstandingInvocations(E: Exception);
var
  OutstandingCall: TScInvocationRequest;
  i: integer;
begin
  Log.LogDebug(FLogger, SHubConnection_CancelingOutstandingInvocations);

  FLock.Enter;
  try
    for i := 0 to FPendingCalls.Count - 1 do begin
      OutstandingCall := TScInvocationRequest(FPendingCalls.Objects[i]);
      if OutstandingCall <> nil then begin
        Log.LogDebug(FLogger, SHubConnection_RemovingInvocation, [OutstandingCall.InvocationId]);
        try
          if E <> nil then
            OutstandingCall.Fail(E);
        finally
          OutstandingCall.Free;
        end;
      end;
    end;

    FPendingCalls.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TScConnectionState.Stop;
begin
  FLock.Enter;
  try
    Log.LogDebug(FLogger, SHubConnection_Stopping);

    // Complete our write pipe, which should cause everything to shut down
    Log.LogDebug(FLogger, SHubConnection_TerminatingReceiveLoop);
    FConnection.Transport.Input.CancelPendingRead;
  finally
    FLock.Leave;
  end;

  // Wait ServerTimeout for the server or transport to shut down.
  Log.LogDebug(FLogger, SHubConnection_WaitingForReceiveLoopToTerminate);
  Assert(FReceiveTask <> nil);
  FReceiveTask.WaitFor;

  Log.LogDebug(FLogger, SHubConnection_Stopped);
end;

procedure TScConnectionState.PreparePing;
begin
  FHubConnection.FState.AssertInConnectionLock;

  // Tell the server we intend to ping and we should be timed out if we stop
  FHubConnection.SendHubMessage(Self, TScPingMessage.Default);

  // initialize the timers
  ResetTimeout;
  ResetSendPing;
end;

procedure TScConnectionState.ResetSendPing;
begin
  FLastActivationSendPing := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
end;

procedure TScConnectionState.ResetTimeout;
begin
  FLastActivationServerTimeout := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
end;

procedure TScConnectionState.RunTimerActions(Sender: TObject);
begin
  if not FHasInherentKeepAlive and (GetTickInterval(FLastActivationServerTimeout, {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF}) >= FHubConnection.FServerTimeoutTicks) then
    OnServerTimeout;

  if (GetTickInterval(FLastActivationSendPing, {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF}) >= FHubConnection.FKeepAliveIntervalTicks) and
    not FStopping then
  begin
    if not FHubConnection.FState.TryAcquireConnectionLock then begin
      Log.LogDebug(FLogger, SHubConnection_UnableToAcquireConnectionLockForPing);
      Exit;
    end;

    Log.LogDebug(FLogger, SHubConnection_AcquiredConnectionLockForPing);
    try
      if FHubConnection.FState.FCurrentConnectionStateUnsynchronized <> nil then begin
        if FHubConnection.FState.FCurrentConnectionStateUnsynchronized <> IScConnectionState(Self) then
          raise InvalidOperationException.Create(SSomethingResetConnectionState);
        FHubConnection.SendHubMessage(Self, TScPingMessage.Default);
      end;
    finally
      FHubConnection.FState.ReleaseConnectionLock;
    end;
  end;
end;

function TScConnectionState.GetTimerLoopProc: TNotifyEvent;
begin
  Result := RunTimerActions;
end;

procedure TScConnectionState.OnServerTimeout;
begin
  if FCloseException = nil then
    SetCloseException(HubException.CreateFmt(SServerTimeoutWithoutReceivingMessage, [FHubConnection.FServerTimeoutTicks]));
  FConnection.Transport.Input.CancelPendingRead;
end;

function TScConnectionState.GetReturnType(const InvocationId: string): TExtTypeInfo;
var
  InvocationRequest: TScInvocationRequest;
begin
  if not TryGetInvocation(InvocationId, InvocationRequest) then begin
    Log.LogError(FLogger, SHubConnection_ReceivedUnexpectedResponse, [InvocationId], nil);
    Result.VarType := varUnknown;
    Result.ClassType := nil;
  end
  else begin
    Result.VarType := InvocationRequest.ResultType;
    Result.ClassType := InvocationRequest.ResultClass;
  end;
end;

function TScConnectionState.GetStreamItemType(const InvocationId: string): TExtTypeInfo;
var
  InvocationRequest: TScInvocationRequest;
begin
  if not TryGetInvocation(InvocationId, InvocationRequest) then begin
    Log.LogError(FLogger, SHubConnection_ReceivedUnexpectedResponse, [InvocationId], nil);
    Result.VarType := varUnknown;
    Result.ClassType := nil;
  end
  else begin
    Result.VarType := InvocationRequest.ResultType;
    Result.ClassType := InvocationRequest.ResultClass;
  end;
end;

function TScConnectionState.GetParameterTypes(const MethodName: string;
  out ParamTypes: TExtTypeInfoArray; out AsArray: boolean): boolean;
var
  InvocationHandler: TScInvocationHandler;
  Len: integer;
begin
  InvocationHandler := FHubConnection.GetHandler(MethodName);
  Result := InvocationHandler <> nil;

  if Result then begin
    AsArray := InvocationHandler.FAsArray;
    Len := Length(InvocationHandler.FParameterTypes);
    SetLength(ParamTypes, Len);
    if Len > 0 then
      Move(InvocationHandler.FParameterTypes[0], ParamTypes[0], SizeOf(TExtTypeInfo) * Len);
  end
  else begin
    Log.LogWarning(FLogger, SHubConnection_MissingHandler, [MethodName]);
    SetLength(ParamTypes, 0);
    AsArray := False;
  end;
end;

{ TScReconnectingConnectionState }

constructor TScReconnectingConnectionState.Create(ALogger: TScLogger);
begin
  inherited Create;

  FLogger := ALogger;

  FStopCts := TScCancellationToken.Create;
  FConnectionLock := TSimpleSemaphore.Create;
end;

destructor TScReconnectingConnectionState.Destroy;
begin
  FReconnectTask.Free;

  FCurrentConnectionStateUnsynchronized := nil;

  FStopCts.Free;
  FConnectionLock.Free;

  inherited;
end;

class function TScReconnectingConnectionState.StateToStr(State: TScHubConnectionState): string;
begin
  Result := GetEnumName(TypeInfo(TScHubConnectionState), integer(State));
end;

procedure TScReconnectingConnectionState.ChangeState(ExpectedState, NewState: TScHubConnectionState);
begin
  if not TryChangeState(ExpectedState, NewState) then begin
    Log.LogError(FLogger, SHubConnection_StateTransitionFailed, [StateToStr(ExpectedState), StateToStr(NewState), StateToStr(FOverallState)], nil);
    raise InvalidOperationException.CreateFmt(SHubConnectionFailedToTransition, [StateToStr(ExpectedState), StateToStr(NewState), StateToStr(FOverallState)]);
  end;
end;

function TScReconnectingConnectionState.TryChangeState(ExpectedState, NewState: TScHubConnectionState): boolean;
begin
  AssertInConnectionLock;

  Log.LogDebug(FLogger, SHubConnection_AttemptingStateTransition, [StateToStr(ExpectedState), StateToStr(NewState)]);

  if FOverallState <> ExpectedState then
    Result := False
  else begin
    FOverallState := NewState;
    Result := True;
  end;
end;

procedure TScReconnectingConnectionState.AssertInConnectionLock;
begin
  Assert(FConnectionLock.CurrentCount = 0, SNotInConnectionLock);
end;

procedure TScReconnectingConnectionState.AssertConnectionValid;
begin
  AssertInConnectionLock;
  Assert(FCurrentConnectionStateUnsynchronized <> nil, SNotHaveConnection);
end;

procedure TScReconnectingConnectionState.WaitConnectionLock;
begin
  Log.LogDebug(FLogger, SHubConnection_WaitingOnConnectionLock);
  FConnectionLock.WaitFor;
end;

function TScReconnectingConnectionState.TryAcquireConnectionLock: boolean;
begin
  Result := FConnectionLock.WaitFor(0);
end;

function TScReconnectingConnectionState.WaitForActiveConnection: IScConnectionState;
begin
  WaitConnectionLock;

  if (FCurrentConnectionStateUnsynchronized = nil) or FCurrentConnectionStateUnsynchronized.Stopping then
    raise InvalidOperationException.Create(SMethodCannotBeCalledIfConnectionNotActive);

  Result := FCurrentConnectionStateUnsynchronized;
end;

procedure TScReconnectingConnectionState.ReleaseConnectionLock;
begin
  Log.LogDebug(FLogger, SHubConnection_ReleasingConnectionLock);
  FConnectionLock.Release;
end;

initialization
  DefaultReconnectPolicy := TScDefaultRetryPolicy.Create;

finalization
  DefaultReconnectPolicy.Free;

end.
