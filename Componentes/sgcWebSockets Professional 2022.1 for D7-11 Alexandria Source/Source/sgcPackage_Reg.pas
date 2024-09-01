{ ***************************************************************************
  sgcPackage component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcPackage_Reg;

interface

{$I sgcVer.inc}
{$IFNDEF BCB}
{$IFNDEF LAZARUS}
{$IFDEF D2007}
{$IFDEF SGC_PACKAGE_REG}

uses
  Windows, Messages, SysUtils, Classes, ToolsAPI;

const
  CS_COMPUTER_LENGTH = 15; // Computer name length
  CS_EVENT_SIGNATURE = $EAB6F31C;

type
  TsgcInstanceEventType = (ieGlobalCreated,
    // Global notiifcation about instance creation
    ieGlobalDeleted, // Global notiifcation about instance deletion
    ieLocalRequest, // Local request for immeidate update
    ieLocalStatus, // "I'm still alive" local notification
    ieGlobalRequest, // Global request for immeidate update
    ieGlobalStatus // "I'm still alive" global notification
    );

  TsgcInstanceEvent = record
    Signature: DWORD; // $EAB6F31C
    Id: DWORD; // Unique (per-process) ID to suppress duplicates
    Event: TsgcInstanceEventType; // TYpe of event
    Computer: string[CS_COMPUTER_LENGTH + 1]; // Name of computer
    LocalIndex: Integer; // Used to form slot name
    InstanceCount: Integer; // The number of instances on computer
    PID: DWORD; // Process ID
  end;

  PsgcInstanceEvent = ^TsgcInstanceEvent;

  TsgcInstanceGlobalInfo = record
    Computer: string[CS_COMPUTER_LENGTH + 1]; // Name of computer
    LocalCount: Integer; // Count of instances on the computer
    LastRefresh: TDateTime; // Last time the activity of computer detected
  end;

  PsgcInstanceGlobalInfo = ^TsgcInstanceGlobalInfo;

  TsgcInstanceLocalInfo = record
    PID: DWORD; // Process ID
    LastRefresh: TDateTime; // Last time the activity of process detected
  end;

  PsgcInstanceLocalInfo = ^TsgcInstanceLocalInfo;

  TsgcInstanceCounterThread = class;

  TsgcInstanceCounter = class
  private
    { Private declarations }
    _Active: Boolean;
    _UpdateCount: Integer;
    _Interval: Integer;
    _InstanceName: string;
    _OnChange: TNotifyEvent;
    procedure RemoveExpiredInfo;
    procedure SetInterval(const Value: Integer);
    procedure SetInstanceName(const Value: string);
    // delete items older then Now-Interval*3
  private
    _ReceivedEvents: TStringList;
    _ThreadLocalInfo: TThreadList;
    _ThreadGlobalInfo: TThreadList;
    _LocalInfo: TList;
    _GlobalInfo: TList;
    _LockCount: Integer;
    procedure Lock;
    procedure Unlock;
  private
    _ThreadStop: THANDLE;
    _ThreadStopped: THANDLE;
    _Thread: TsgcInstanceCounterThread;
  protected
    { Protected declarations }
    procedure SetActive(Value: Boolean);
    procedure ApplyUpdates;
  public
    { Public declarations }
    function CountComputers: Integer;
    function CountInstances: Integer;
    procedure RequestUpdate;
  public
    { Published declarations }
    constructor Create { };
    destructor Destroy; override;
    property Active: Boolean read _Active write SetActive;
    procedure BeginUpdates;
    procedure EndUpdates;
    property InstanceName: string read _InstanceName write SetInstanceName;
    property Interval: Integer read _Interval write SetInterval default 10000;
    property OnChange: TNotifyEvent read _OnChange write _OnChange;
  end;

  TsgcInstanceCounterThread = class(TThread)
  private
    _Owner: TsgcInstanceCounter;
    _LocalInfo: TList;
    _GlobalInfo: TList;
    _IntervalTime: TDateTime;
    _LocalIndex: Integer;
    _Slot: THANDLE;
    _SlotName: string;
    _Changed: Boolean;
    _LockCount: Integer;
    procedure Changed;

    procedure Lock;
    procedure Unlock;

    procedure InitSlot;
    procedure DoneSlot;
    procedure UpdateSlot;

    function GetInstanceEvent(var Event: TsgcInstanceEvent): Boolean;
    procedure ProcessInstanceEvent(const Event: TsgcInstanceEvent);

    function FindLocal(const Event: TsgcInstanceEvent): PsgcInstanceLocalInfo;
    procedure PostLocal(const Event: TsgcInstanceEvent; LocalIndex: Integer);
    procedure PostLocalStatus(LocalIndex: Integer);
    procedure PostLocalRequest;

    function FindGlobal(const Event: TsgcInstanceEvent): PsgcInstanceGlobalInfo;
    procedure PostGlobal(const Event: TsgcInstanceEvent; Computer: string;
      LocalIndex: Integer);
    procedure PostGlobalStatus(Computer: string; LocalIndex: Integer);
    procedure PostGlobalRequest;

    procedure InitEvent(var Event: TsgcInstanceEvent);
    procedure DoPostEvent(const Event: TsgcInstanceEvent; SlotName: string);

    procedure ProcessLocalCreated(const Event: TsgcInstanceEvent);
    procedure ProcessLocalDeleted(const Event: TsgcInstanceEvent);
    procedure ProcessGlobalCreated(const Event: TsgcInstanceEvent);
    procedure ProcessGlobalDeleted(const Event: TsgcInstanceEvent);
    procedure ProcessLocalRequest(const Event: TsgcInstanceEvent);
    procedure ProcessLocalStatus(const Event: TsgcInstanceEvent);
    procedure ProcessGlobalRequest(const Event: TsgcInstanceEvent);
    procedure ProcessGlobalStatus(const Event: TsgcInstanceEvent);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TsgcInstanceCounter);
  end;

type
  TFuncCallback = procedure;

type
  TsgcCheckPackage = class
  private
    FInstanceCounter: TsgcInstanceCounter;
    FIsValid: Boolean;
    FInstanceCount: Integer;
    FCallBack: TFuncCallback;
    procedure Change(Sender: TObject);
    procedure SetCallBack(const Value: TFuncCallback);
  public
    property IsValid: Boolean read FIsValid;
    property InstanceCount: Integer read FInstanceCount;
    constructor Create(aPackage: string);
    destructor Destroy; override;
    property CallBack: TFuncCallback read FCallBack write SetCallBack;
  end;

  TIDEIOTAsgcWebSockets = class(TNotifierObject, IOTANotifier, IOTAIDENotifier,
    IOTAIDENotifier50)
  private
    FCheckPackage: TsgcCheckPackage;
  public
    // IOTAIDENotifier
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;

    // IOTAIDENotifier50
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);
      overload;
  public
    property CheckPackage: TsgcCheckPackage read FCheckPackage
      write FCheckPackage;
  end;

{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

{$IFNDEF BCB}
{$IFNDEF LAZARUS}
{$IFDEF D2007}
{$IFDEF SGC_PACKAGE_REG}
{$WARNINGS Off}

uses
  sgcBase_Helpers;

const
  CS_ERROR_MESSAGE =
    'VGhlIGNvbXBpbGF0aW9uIG9mIHRoZSBwcm9qZWN0IGhhcyBiZWVuIHN0b3BwZWQgYmVjYXVzZSBjdXJyZW50IHNnY1dlYlNvY2tldHMgbGljZW5zZSBpcyBiZWluZyB1c2VkIG9uIG1vcmUgY29tcHV0ZXJzIHNpbXVsdGFuZW91c2x5IHRoYW4gYWxsb3dlZC4=';

var
  _LastEventId: Integer;

procedure OnChangeEvent(Sender: TObject);
begin
  if TsgcInstanceCounter(Sender).CountInstances > 0 then
    raise Exception.Create('Error Message');
end;

function GetHash(Str: string): string;
  function ElfHash(const Value: string): Integer;
  var
    i, x: Integer;
  begin
    Result := 0;
    for i := 1 to Length(Value) do
    begin
      Result := (Result shl 4) + Ord(Value[i]);
      x := Result and $F0000000;
      if (x <> 0) then
        Result := Result xor (x shr 24);
      Result := Result and (not x);
    end;
  end;

begin
  Result := Format('%x', [ElfHash(Str)]);
  if Length(Result) > 8 then
    Result := copy(Result, 1, 8);
end;

function LastEventId: Integer;
begin
  Result := _LastEventId;
  INC(_LastEventId);
end;

function ComputerName: string;
var
  Buf: array [0 .. CS_COMPUTER_LENGTH] of Char;
  BufSize: DWORD;
begin
  BufSize := CS_COMPUTER_LENGTH + 1;
  GetComputerName(Buf, BufSize);
  Result := Buf;
end;

constructor TsgcInstanceCounter.Create;
begin
  inherited;
  _ReceivedEvents := TStringList.Create;
  _ThreadGlobalInfo := TThreadList.Create;
  _ThreadLocalInfo := TThreadList.Create;
  _Interval := 10000;
  _ThreadStop := CreateEvent(nil, True, False, nil);
  _ThreadStopped := CreateEvent(nil, True, False, nil);
end;

destructor TsgcInstanceCounter.Destroy;
begin
  _UpdateCount := 0;

  Active := False;

  RemoveExpiredInfo;

  _LocalInfo.Free;
  _GlobalInfo.Free;

  _ReceivedEvents.Free;

  CloseHandle(_ThreadStop);
  CloseHandle(_ThreadStopped);

  inherited;
end;

procedure TsgcInstanceCounter.Lock;
begin
  _GlobalInfo := _ThreadGlobalInfo.LockList;
  _LocalInfo := _ThreadLocalInfo.LockList;
  INC(_LockCount);
end;

procedure TsgcInstanceCounter.Unlock;
begin
  DEC(_LockCount);
  if _LockCount = 0 then
  begin
    _LocalInfo := nil;
    _GlobalInfo := nil;
  end;
  _ThreadLocalInfo.UnLockList;
  _ThreadGlobalInfo.UnLockList;
end;

procedure TsgcInstanceCounter.RemoveExpiredInfo;
var
  i: Integer;
  Expiration: TDateTime;
  PLI: PsgcInstanceLocalInfo;
  PGI: PsgcInstanceGlobalInfo;
begin
  Lock;
  try
    Expiration := Now - 3 * Interval / 24 / 3600 / 1000;

    for i := _LocalInfo.Count - 1 downto 0 do
    begin
      PLI := _LocalInfo[i];
      if Active and (PLI.LastRefresh > Expiration) then
        continue;
      FreeMem(PLI);
      _LocalInfo.Remove(PLI);
    end;

    for i := _GlobalInfo.Count - 1 downto 0 do
    begin
      PGI := _GlobalInfo[i];
      if Active and (PGI.LastRefresh > Expiration) then
        continue;
      FreeMem(PGI);
      _GlobalInfo.Remove(PGI);
    end;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounter.SetActive(Value: Boolean);
begin
  _Active := Value;
  if _UpdateCount > 0 then
    exit;

  if not Active then
  begin
    if _Thread = nil then
      exit;
    _Thread.Terminate;
    SetEvent(_ThreadStop);
    WaitForSingleObject(_ThreadStopped, INFINITE);
    _Thread := nil;
  end
  else
  begin
    if _Thread <> nil then
      exit;
    ResetEvent(_ThreadStop);
    ResetEvent(_ThreadStopped);
    _Thread := TsgcInstanceCounterThread.Create(Self);
  end;
end;

procedure TsgcInstanceCounter.SetInterval(const Value: Integer);
begin
  if Value < 1000 then
    raise Exception.Create('Interval can''t be smaller then 1000');
  _Interval := Value;
end;

function TsgcInstanceCounter.CountComputers: Integer;
begin
  Lock;
  try
    RemoveExpiredInfo;
    Result := _GlobalInfo.Count;
  finally
    Unlock;
  end;
end;

function TsgcInstanceCounter.CountInstances: Integer;
var
  i: Integer;
begin
  Result := 0;
  Lock;
  try
    RemoveExpiredInfo;
    for i := _GlobalInfo.Count - 1 downto 0 do
      Result := Result + PsgcInstanceGlobalInfo(_GlobalInfo[i]).LocalCount;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounter.SetInstanceName(const Value: string);
var
  i: Integer;
begin
  if Length(Value) > 8 then
    raise Exception.Create
      ('Instance name is too long (max length is 8 symbols)');

  for i := 1 to Length(Value) do
    if not(Value[i] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', '_']) then
      raise Exception.Create('Invalid symbol in InstanceName');

  _InstanceName := Value;
  ApplyUpdates;
end;

procedure TsgcInstanceCounter.ApplyUpdates;
begin
  if _UpdateCount > 0 then
    exit;
  if not Active then
    exit;
  Active := False;
  Active := True;
end;

procedure TsgcInstanceCounter.BeginUpdates;
begin
  INC(_UpdateCount);
end;

procedure TsgcInstanceCounter.EndUpdates;
begin
  DEC(_UpdateCount);
  if _UpdateCount = 0 then
    ApplyUpdates;
end;

procedure TsgcInstanceCounter.RequestUpdate;
begin
  if not Active then
    exit;
  if _UpdateCount > 0 then
    exit;
  if _Thread = nil then
    exit;
  _Thread.PostGlobalRequest;
end;

constructor TsgcInstanceCounterThread.Create(Owner: TsgcInstanceCounter);
begin
  inherited Create(True);
  _Owner := Owner;
  _IntervalTime := _Owner._Interval / 1000 / 3600 / 24;
  FreeOnTerminate := True;
  Resume;
end;

procedure TsgcInstanceCounterThread.Execute;
var
  Event: TsgcInstanceEvent;
  NextBroadcast: TDateTime;
  NextSlotUpdate: TDateTime;
begin
  InitSlot;
  NextBroadcast := Now + _IntervalTime;
  NextSlotUpdate := Now + 0.5 / 3600 / 24;

  while not Terminated do
  begin
    if Now > NextBroadcast then
    begin
      PostLocalStatus(-1);

      if _LocalIndex < 2 then
        PostGlobalStatus('', -1);

      NextBroadcast := Now + _IntervalTime;

      _Owner.RemoveExpiredInfo;
    end;

    if GetInstanceEvent(Event) then
      ProcessInstanceEvent(Event)
    else
      WaitForSingleObject(_Owner._ThreadStop, 100);

    if Now > NextSlotUpdate then // Attemp to decrease LocalIndex
    begin
      UpdateSlot;
      NextSlotUpdate := Now + 0.5 / 3600 / 24;
    end;
  end;

  DoneSlot;
  SetEvent(_Owner._ThreadStopped);
end;

function TsgcInstanceCounterThread.GetInstanceEvent
  (var Event: TsgcInstanceEvent): Boolean;
var
  Size: DWORD;
  Buf: PsgcInstanceEvent;
  Count: DWORD;
  S: string;
begin
  Result := False;
  Lock;
  try
    // Slot is not active
    if _Slot = INVALID_HANDLE_VALUE then
      exit;

    while True do
    begin
      // Check for new messages
      if not GetMailslotInfo(_Slot, nil, Size, nil, nil) then
        exit;

      if Size = MAILSLOT_NO_MESSAGE then
        exit;

      GetMem(Buf, Size);
      try
        if not ReadFile(_Slot, Buf^, Size, Count, nil) then
          exit;

        if Buf.Signature <> CS_EVENT_SIGNATURE then
          continue;
        if Count < SizeOf(Event) then
          continue;
        Event := Buf^;
        S := Event.Computer + IntToHex(Event.PID, 8) + IntToHex(Event.Id, 8);

        // Mailslot can get duplicated messages
        // Do not process duplicates twice
        with _Owner._ReceivedEvents do
        begin
          if IndexOf(S) >= 0 then
            continue;
          Add(S);
          while Count > 100 do
            Delete(0);
        end;

        Event := Buf^;
        Result := True;
        exit;
      finally
        FreeMem(Buf);
      end;
    end;

  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessInstanceEvent
  (const Event: TsgcInstanceEvent);
begin
  case Event.Event of
    ieGlobalCreated:
      ProcessGlobalCreated(Event);
    ieGlobalDeleted:
      ProcessGlobalDeleted(Event);
    ieLocalRequest:
      ProcessLocalRequest(Event);
    ieLocalStatus:
      ProcessLocalStatus(Event);
    ieGlobalRequest:
      ProcessGlobalRequest(Event);
    ieGlobalStatus:
      ProcessGlobalStatus(Event);
  end;
end;

procedure TsgcInstanceCounterThread.Lock;
begin
  _Owner.Lock;
  INC(_LockCount);
  _LocalInfo := _Owner._LocalInfo;
  _GlobalInfo := _Owner._GlobalInfo;
end;

procedure TsgcInstanceCounterThread.Unlock;
begin
  DEC(_LockCount);
  if _LockCount = 0 then
  begin
    _LocalInfo := nil;
    _GlobalInfo := nil;
  end;
  _Owner.Unlock;

  if _Changed and (_LockCount = 0) then
  begin
    Synchronize(Changed);
    _Changed := False;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessGlobalCreated
  (const Event: TsgcInstanceEvent);
var
  PIGI: PsgcInstanceGlobalInfo;
  SameComputer: Boolean;
  SameProcess: Boolean;
  StatusEvent: TsgcInstanceEvent;
  i: Integer;
begin
  Lock;
  try
    ProcessLocalCreated(Event);

    PIGI := FindGlobal(Event);
    if PIGI = nil then
    begin
      GetMem(PIGI, SizeOf(PIGI^));
      PIGI.Computer := Event.Computer;
      PIGI.LocalCount := 0;
      _GlobalInfo.Add(PIGI);
    end;
    INC(PIGI.LocalCount);
    _Changed := True;
    PIGI.LastRefresh := Now;

    if _LocalIndex > 1 then
      exit;
    SameComputer := Event.Computer = ComputerName;
    SameProcess := SameComputer and (Event.PID = GetCurrentProcessId);

    if SameProcess then
      exit;

    PostLocal(Event, -1);

    if not SameComputer then
    begin
      PostGlobalStatus(Event.Computer, Event.LocalIndex);
    end
    else
    begin
      StatusEvent.Signature := CS_EVENT_SIGNATURE;
      StatusEvent.Computer := ComputerName;
      StatusEvent.Id := LastEventId;
      StatusEvent.Event := ieGlobalStatus;
      StatusEvent.LocalIndex := Event.LocalIndex;
      StatusEvent.PID := Event.PID;

      for i := 0 to _GlobalInfo.Count - 1 do
      begin
        PIGI := _GlobalInfo[i];
        StatusEvent.InstanceCount := PIGI.LocalCount;
        PostLocal(StatusEvent, Event.LocalIndex);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessGlobalDeleted
  (const Event: TsgcInstanceEvent);
var
  PIGI: PsgcInstanceGlobalInfo;
begin
  Lock;
  try
    PIGI := FindGlobal(Event);
    if PIGI = nil then
      exit;

    DEC(PIGI.LocalCount);
    _Changed := True;

    PIGI.LastRefresh := Now;
    if PIGI.LocalCount <= 0 then
    begin
      _GlobalInfo.Remove(PIGI);
      FreeMem(PIGI);
    end;

    if _LocalIndex < 2 then
      PostLocal(Event, -1);

    ProcessLocalDeleted(Event);

  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessGlobalRequest
  (const Event: TsgcInstanceEvent);
begin
  Lock;
  try
    if _LocalIndex > 2 then
      exit;

    PostGlobalStatus(Event.Computer, Event.LocalIndex);

    PostLocalRequest;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessGlobalStatus
  (const Event: TsgcInstanceEvent);
var
  PIGI: PsgcInstanceGlobalInfo;
begin
  Lock;
  try
    PIGI := FindGlobal(Event);

    if PIGI <> nil then
    begin
      PIGI.LastRefresh := Now;
      _Changed := _Changed or (PIGI.LocalCount <> Event.InstanceCount);
      PIGI.LocalCount := Event.InstanceCount;
    end
    else
      ProcessGlobalCreated(Event);

    if _LocalIndex < 2 then
      PostLocal(Event, -1);
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessLocalCreated
  (const Event: TsgcInstanceEvent);
var
  PILI: PsgcInstanceLocalInfo;
begin
  if Event.Computer <> ComputerName then
    exit;

  Lock;
  try
    PILI := FindLocal(Event);
    if PILI <> nil then
      exit;

    GetMem(PILI, SizeOf(PILI^));
    PILI.PID := Event.PID;
    PILI.LastRefresh := Now;
    _LocalInfo.Add(PILI);
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessLocalDeleted
  (const Event: TsgcInstanceEvent);
var
  PILI: PsgcInstanceLocalInfo;
begin
  if Event.Computer <> ComputerName then
    exit;
  Lock;
  try
    PILI := FindLocal(Event);
    if PILI <> nil then
    begin
      _LocalInfo.Remove(PILI);
      FreeMem(PILI);
    end;
    if Event.LocalIndex < _LocalIndex then
      UpdateSlot;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessLocalRequest
  (const Event: TsgcInstanceEvent);
begin
  if Event.PID = GetCurrentProcessId then
    exit;

  Lock;
  try
    PostLocalStatus(Event.LocalIndex);
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.ProcessLocalStatus
  (const Event: TsgcInstanceEvent);
var
  PILI: PsgcInstanceLocalInfo;
begin
  Lock;
  try
    PILI := FindLocal(Event);

    if PILI <> nil then
      PILI.LastRefresh := Now
    else
      ProcessLocalCreated(Event);
  finally
    Unlock;
  end;
end;

function TsgcInstanceCounterThread.FindLocal(const Event: TsgcInstanceEvent)
  : PsgcInstanceLocalInfo;
var
  i: Integer;
begin
  if _LocalInfo = nil then
    raise Exception.Create('Internal error: no lock');

  for i := 0 to _LocalInfo.Count - 1 do
  begin
    Result := _LocalInfo[i];
    if Event.PID <> Result.PID then
      continue;
    exit;
  end;
  Result := nil;
end;

function TsgcInstanceCounterThread.FindGlobal(const Event: TsgcInstanceEvent)
  : PsgcInstanceGlobalInfo;
var
  i: Integer;
begin
  if _GlobalInfo = nil then
    raise Exception.Create('Internal error: no lock');

  for i := 0 to _GlobalInfo.Count - 1 do
  begin
    Result := _GlobalInfo[i];
    if Event.Computer <> Result.Computer then
      continue;
    exit;
  end;
  Result := nil;
end;

procedure TsgcInstanceCounterThread.DoPostEvent(const Event: TsgcInstanceEvent;
  SlotName: string);
var
  Slot: THANDLE;
  Count: DWORD;
begin
  Slot := CreateFile(PChar(SlotName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if Slot = INVALID_HANDLE_VALUE then
    exit;

  WriteFile(Slot, Event, SizeOf(Event), Count, nil);

  CloseHandle(Slot);
end;

procedure TsgcInstanceCounterThread.PostGlobalStatus(Computer: string;
  LocalIndex: Integer);
var
  Event: TsgcInstanceEvent;
begin
  InitEvent(Event);
  Event.Event := ieGlobalStatus;
  PostGlobal(Event, Computer, LocalIndex);
end;

procedure TsgcInstanceCounterThread.PostLocalStatus(LocalIndex: Integer);
var
  Event: TsgcInstanceEvent;
begin
  InitEvent(Event);
  Event.Event := ieLocalStatus;
  PostLocal(Event, LocalIndex);
  ProcessLocalStatus(Event);
end;

procedure TsgcInstanceCounterThread.PostGlobalRequest;
var
  Event: TsgcInstanceEvent;
begin
  InitEvent(Event);
  Event.Event := ieGlobalRequest;
  PostGlobal(Event, '', -1);
end;

procedure TsgcInstanceCounterThread.PostLocalRequest;
var
  Event: TsgcInstanceEvent;
begin
  InitEvent(Event);
  Event.Event := ieLocalRequest;
  PostLocal(Event, -1);
  ProcessLocalStatus(Event);
end;

procedure TsgcInstanceCounterThread.InitEvent(var Event: TsgcInstanceEvent);
begin
  Lock;
  try
    Event.Signature := CS_EVENT_SIGNATURE;
    Event.Computer := ComputerName;
    Event.Id := LastEventId;
    Event.LocalIndex := _LocalIndex;
    Event.InstanceCount := _LocalInfo.Count;
    Event.PID := GetCurrentProcessId;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.PostLocal(const Event: TsgcInstanceEvent;
  LocalIndex: Integer);
var
  i: Integer;
  SlotName: string;
begin
  if LocalIndex >= 0 then
  begin
    SlotName := '\\' + ComputerName + '\mailslot\' + _Owner.InstanceName + '.';
    DoPostEvent(Event, SlotName + IntToHex(LocalIndex, 3));
  end
  else
  begin
    SlotName := '\\' + ComputerName + '\mailslot\' + _Owner.InstanceName + '.';
    Lock;
    try
      for i := 0 to _LocalInfo.Count + 4 do
      begin
        if i <> _LocalIndex then
          DoPostEvent(Event, SlotName + IntToHex(i, 3));
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TsgcInstanceCounterThread.PostGlobal(const Event: TsgcInstanceEvent;
  Computer: string; LocalIndex: Integer);
var
  i: Integer;
  SlotName: string;
begin
  if Computer = '' then // Broadcast
  begin
    for i := 0 to 1 do
    begin
      SlotName := '\\*\mailslot\' + _Owner.InstanceName + '.' + IntToHex(i, 3);
      DoPostEvent(Event, SlotName);
    end;
  end
  else
  begin
    SlotName := '\\' + Computer + '\mailslot\' + _Owner.InstanceName + '.' +
      IntToHex(LocalIndex, 3);
    DoPostEvent(Event, SlotName);
  end;
end;

procedure TsgcInstanceCounterThread.InitSlot;
var
  i: Integer;
  Event: TsgcInstanceEvent;
begin
  Lock;
  try
    _LocalIndex := -1;
    for i := 0 to 100 do
    // Unlikely that more then 100 instances run on the same computer
    begin
      _SlotName := '\\.\mailslot\' + _Owner.InstanceName + '.' + IntToHex(i, 3);
      _Slot := CreateMailslot(PChar(_SlotName), 400, 0, nil);
      if _Slot = INVALID_HANDLE_VALUE then
        continue;

      // Successful initiation
      _LocalIndex := i;

      PostLocalRequest;

      InitEvent(Event);
      Event.Event := ieGlobalCreated;
      PostGlobal(Event, '', -1);

      break;
    end;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.DoneSlot;
var
  Event: TsgcInstanceEvent;
begin
  Lock;
  try
    if _Slot = INVALID_HANDLE_VALUE then
      exit;

    InitEvent(Event);
    Event.Event := ieGlobalDeleted;
    PostGlobal(Event, '', -1);

    CloseHandle(_Slot);
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.UpdateSlot;
var
  i: Integer;
  Slot: THANDLE;
  S: string;
begin
  Lock;
  try
    for i := 0 to _LocalIndex - 1 do
    begin
      S := '\\.\mailslot\' + _Owner.InstanceName + '.' + IntToHex(i, 3);
      Slot := CreateMailslot(PChar(S), 400, 0, nil);
      if Slot = INVALID_HANDLE_VALUE then
        continue;

      CloseHandle(_Slot);
      _SlotName := S;
      _Slot := Slot;
      _LocalIndex := i;
      break;
    end;
  finally
    Unlock;
  end;
end;

procedure TsgcInstanceCounterThread.Changed;
begin
  if Assigned(_Owner._OnChange) then
    _Owner._OnChange(_Owner);
end;

{ TIDEIOTACleverFilter }

procedure TIDEIOTAsgcWebSockets.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TIDEIOTAsgcWebSockets.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin

end;

procedure TIDEIOTAsgcWebSockets.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  Cancel := not(Assigned(CheckPackage) and CheckPackage.IsValid);
  if Cancel then
    (BorlandIDEServices as IOTAMessageServices)
      .AddTitleMessage(DecodeBase64(CS_ERROR_MESSAGE));
end;

procedure TIDEIOTAsgcWebSockets.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  Cancel := not(Assigned(CheckPackage) and CheckPackage.IsValid);
  if Cancel then
    (BorlandIDEServices as IOTAMessageServices)
      .AddTitleMessage((DecodeBase64(CS_ERROR_MESSAGE)));
end;

procedure TIDEIOTAsgcWebSockets.FileNotification
  (NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  Cancel := not(Assigned(CheckPackage) and CheckPackage.IsValid);
  if Cancel then
    (BorlandIDEServices as IOTAMessageServices)
      .AddTitleMessage((DecodeBase64(CS_ERROR_MESSAGE)));
end;

procedure TsgcCheckPackage.Change(Sender: TObject);
begin
  FIsValid := FInstanceCounter.CountComputers <= (10-8);
  FInstanceCount := FInstanceCounter.CountComputers;
  if Assigned(FCallBack) then
    FCallBack;
end;

constructor TsgcCheckPackage.Create(aPackage: string);
begin
  FInstanceCounter := nil;
  FIsValid := True;
  FInstanceCount := 0;
  FCallBack := nil;
  begin
    FInstanceCounter := TsgcInstanceCounter.Create;
    with FInstanceCounter do
    begin
      InstanceName := GetHash(aPackage);
      OnChange := Change;
      Interval := 5 * 1000;
      Active := True;
    end;
  end;
end;

destructor TsgcCheckPackage.Destroy;
begin
  if Assigned(FInstanceCounter) then
    FInstanceCounter.Free;
  inherited;
end;

procedure TsgcCheckPackage.SetCallBack(const Value: TFuncCallback);
begin
  FCallBack := Value;
end;

{$WARNINGS On}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
