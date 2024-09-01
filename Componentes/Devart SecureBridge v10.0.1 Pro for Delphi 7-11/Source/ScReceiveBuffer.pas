
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScReceiveBuffer;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLConsts;
{$ELSE}
  TdsUtilsUni, TdsSSLConstsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions, ScUtils, ScConsts;
{$ENDIF}

const
  RECV_CHUNK_SIZE = 64 * 1024;

type
  /// ReceiveBuffer class implements a temporary storage
  /// It writes data to the end of buffer and reads from beginning
  TReceiveBuffer = class
  private
    FReceiveBuffer: TList; // queue that contains written data
    FReadBuffer: IntPtr;  // current read buffer
    FWriteBuffer: IntPtr; // current not finished write buffer
    FReadPos: Integer;
    FWritePos: Integer;
    FChunkSize: Integer;
    FevDataAvailable: TEvent;
    FLockEvent: TCriticalSection;
    FLockRW: TCriticalSection; // Read/Write synchronization object
    FClosed: Boolean;
    FOnReceiveData: TNotifyEvent;

    function GetDataLength: Integer;
    procedure SetOnReceiveData(Value: TNotifyEvent);

  public
    constructor Create(ChunkSize: Integer);
    destructor Destroy; override;

    procedure CloseBuffer;
    function Read(const Data: TValueArr; Offset, Count: Integer): Integer; overload;
    function Read(Stream: TStream): Integer; overload;
    procedure Write(const Data: TValueArr; Offset, Count: Integer);
    function WaitForData(Count: Integer; MillisecondsTimeout: cardinal): boolean;
    function GetByte(const Index: Integer): Byte;
    function SearchFromIndex(const SearchByte: Byte; const Index: Integer): Integer;

    property DataLength: Integer read GetDataLength;
    property OnReceiveData: TNotifyEvent read FOnReceiveData write SetOnReceiveData;
  end;

  TDataBuffer = TReceiveBuffer;

implementation

uses
  Math;

constructor TReceiveBuffer.Create(ChunkSize: Integer);
begin
  inherited Create;

  FLockEvent := TCriticalSection.Create;
  FLockRW := TCriticalSection.Create;
  FReceiveBuffer := TList.Create;
  FevDataAvailable := CreateEvent;
  FReadPos := 0;
  FWritePos := 0;

  // add first chunk to the buffer
  FChunkSize := ChunkSize;
  FWriteBuffer := Marshal.AllocHGlobal(ChunkSize);
  FReadBuffer := FWriteBuffer;
  FReceiveBuffer.Insert(0, FWriteBuffer);
end;

destructor TReceiveBuffer.Destroy;
var
  DelBlock: IntPtr;
begin
  while FReceiveBuffer.Count > 0 do begin
    DelBlock := IntPtr(FReceiveBuffer.Last);
    FReceiveBuffer.Delete(FReceiveBuffer.Count - 1);
    Marshal.FreeHGlobal(DelBlock);
  end;

  FReceiveBuffer.Free;
  FevDataAvailable.Free;
  FLockEvent.Free;
  FLockRW.Free;
  inherited;
end;

procedure TReceiveBuffer.CloseBuffer;
begin
  lock(FLockRW);
  try
    FevDataAvailable.SetEvent;
    FClosed := True;
  finally
    unlock(FLockRW);
  end;
end;

/// Read data from Receive queue
/// Return count of bytes currently Read
function TReceiveBuffer.Read(const Data: TValueArr; Offset, Count: Integer): Integer;
var
  cnt: Integer;
  DelBlock: IntPtr;
begin
  Result := Count;
  while Count > 0 do begin
    if FReceiveBuffer.Count = 1 then begin
      lock(FLockRW);
      try
        if FReceiveBuffer.Count = 1 then
          cnt := Min(FWritePos - FReadPos, Count)
        else
          cnt := Min(FChunkSize - FReadPos, Count);

        if cnt <= 0 then
          break;
      finally
        unlock(FLockRW);
      end;
    end
    else
      cnt := Min(FChunkSize - FReadPos, Count);

    if Data <> nil then
      Move(PtrOffset(FReadBuffer, FReadPos)^, Data[Offset], cnt);

    Inc(FReadPos, cnt);
    Inc(Offset, cnt);
    Dec(Count, cnt);

    if FReadPos = FChunkSize then begin
      lock(FLockRW);
      try
        if FReceiveBuffer.Count > 1 then begin
          DelBlock := IntPtr(FReceiveBuffer.Last);
          FReceiveBuffer.Delete(FReceiveBuffer.Count - 1);
          Marshal.FreeHGlobal(DelBlock);

          FReadBuffer := IntPtr(FReceiveBuffer.Last);
          FReadPos := 0;
        end
        else begin
          if FWritePos = FChunkSize then begin
            FWritePos := 0;
            FReadPos := 0;
          end;
          break;
        end;
      finally
        unlock(FLockRW);
      end;
    end;
  end;

  Result := Result - Count;
end;

function TReceiveBuffer.Read(Stream: TStream): Integer;
var
  cnt: Integer;
  DelBlock: IntPtr;
begin
  Result := 0;

  while True do begin
    if FReceiveBuffer.Count = 1 then begin
      lock(FLockRW);
      try
        if FReceiveBuffer.Count = 1 then
          cnt := FWritePos - FReadPos
        else
          cnt := FChunkSize - FReadPos;
      finally
        unlock(FLockRW);
      end;
    end
    else
      cnt := FChunkSize - FReadPos;

    if cnt <= 0 then
      break;

    Stream.WriteBuffer(PtrOffset(FReadBuffer, FReadPos)^, cnt);
    Dec(FReadPos, cnt);
    Inc(Result, cnt);

    if FReadPos = FChunkSize then begin
      lock(FLockRW);
      try
        if FReceiveBuffer.Count > 1 then begin
          DelBlock := IntPtr(FReceiveBuffer.Last);
          FReceiveBuffer.Delete(FReceiveBuffer.Count - 1);
          Marshal.FreeHGlobal(DelBlock);

          FReadBuffer := IntPtr(FReceiveBuffer.Last);
          FReadPos := 0;
        end
        else begin
          if FWritePos = FChunkSize then begin
            FWritePos := 0;
            FReadPos := 0;
          end;
          break;
        end;
      finally
        unlock(FLockRW);
      end;
    end;
  end;
end;

procedure TReceiveBuffer.Write(const Data: TValueArr; Offset, Count: Integer);
var
  cnt: Integer;
begin
  if Count <= 0 then
    Exit;

  while Count > 0 do begin
    // check if we need more buffer
    if FWritePos = FChunkSize then begin
      lock(FLockRW);
      try
        if FWritePos = FChunkSize then begin
          FWriteBuffer := Marshal.AllocHGlobal(FChunkSize);
          FReceiveBuffer.Insert(0, FWriteBuffer);
          FWritePos := 0;
        end;
      finally
        unlock(FLockRW);
      end;
    end;

    cnt := Min(FChunkSize - FWritePos, Count);
    Move(Data[Offset], PtrOffset(FWriteBuffer, FWritePos)^, cnt);
    Inc(Offset, cnt);
    Dec(Count, cnt);

    lock(FLockRW);
    try
      Inc(FWritePos, cnt);
      FevDataAvailable.SetEvent;
    finally
      unlock(FLockRW);
    end;
  end;

  FLockEvent.Acquire;
  try
    if Assigned(FOnReceiveData) then
      FOnReceiveData(Self);
  finally
    FLockEvent.Release;
  end;
end;

function TReceiveBuffer.WaitForData(Count: Integer; MillisecondsTimeout: cardinal): boolean;
var
  tc: cardinal;
begin
  if MillisecondsTimeout = 0 then begin
    Result := DataLength >= Count;
    Exit;
  end;

  Result := False;
  tc := GetTickCount;

  while True do begin
    lock(FLockRW);
    try
      if DataLength >= Count then begin
        Result := True;
        Exit;
      end
      else
      if FClosed or (GetTickInterval(tc, GetTickCount) > MillisecondsTimeout) then
        Exit
      else
        FevDataAvailable.ResetEvent;
    finally
      unlock(FLockRW);
    end;

    if FevDataAvailable.WaitFor(MillisecondsTimeout) <> wrSignaled then
      Exit;
  end;
end;

function TReceiveBuffer.GetDataLength: Integer;
begin
  lock(FLockRW);
  try
    if FReceiveBuffer.Count = 0 then
      Result := 0
    else
      Result := (FReceiveBuffer.Count - 1) * FChunkSize + FWritePos - FReadPos;
  finally
    unlock(FLockRW);
  end;
end;

function TReceiveBuffer.GetByte(const Index: Integer): Byte;
var
  buf: IntPtr;
  n: Integer;
begin
  if (FReadPos + Index) < FChunkSize then
    Result := Marshal.ReadByte(FReadBuffer, FReadPos + Index)
  else begin
    if Index > DataLength then
      raise EScError.Create(seNotEnoughData);

    n := (Index + FReadPos) div FChunkSize;  //((Index - (FChunkSize - FReadPos)) div FChunkSize) + 1
    buf := FReceiveBuffer[FReceiveBuffer.Count - 1 - n];
    Result := Marshal.ReadByte(buf, Index + FReadPos - n * FChunkSize);
  end;
end;

function TReceiveBuffer.SearchFromIndex(const SearchByte: Byte; const Index: Integer): Integer;
var
  buf: IntPtr;
  StartPos, EndPos: Integer;
  i, j, n: Integer;
begin
  Result := -1;
  n := (Index + FReadPos) div FChunkSize;
  StartPos := Index + FReadPos - n * FChunkSize;

  for i := FReceiveBuffer.Count - 1 - n downto 0 do begin
    buf := IntPtr(FReceiveBuffer[i]);

    if i > 0 then
      EndPos := FChunkSize - 1
    else
      EndPos := FWritePos - 1;

    for j := StartPos to EndPos do
      if Marshal.ReadByte(buf, j) = SearchByte then begin
        Result := (FReceiveBuffer.Count - 1 - i) * FChunkSize + j - FReadPos;
        Exit;
      end;

    StartPos := 0;
  end;
end;

procedure TReceiveBuffer.SetOnReceiveData(Value: TNotifyEvent);
begin
  FLockEvent.Acquire; // to make sure, that FOnReceiveData will not be called more
  FOnReceiveData := Value;
  FLockEvent.Release;
end;

end.

