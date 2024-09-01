//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScPipe;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFNDEF SBRIDGE}
  CRTypes, CRFunctions, CLRClasses,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLConsts, TdsThread;
{$ELSE}
  TdsUtilsUni, TdsSSLConstsUni, TdsThreadUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScFunctions, ScCLRClasses,
  ScUtils, ScConsts, ScThread;
{$ENDIF}

const
  BuffersCountPerArraySize = 64;
  NumArraySizes = 4; // 4096, 16384, 32768, 65536
  InitialSegmentPoolSize = 16; // 65K
  MaxSegmentPoolSize = 256; // 1MB
  DefaultMinimumSegmentSize = 4096;

type
  TNotifyWithExceptionEvent = procedure(E: Exception) of object;
  TScOnCompleted = procedure (E: Exception) of object;

  TScPipe = class;

  TScMemoryRef = record
    Memory: PByteArray;
    Offset: integer;
    Length: integer;
  end;

  TScMemoryRefHelper = class
    class function PosOf(const Source: TScMemoryRef; const Value: Byte): integer; overload;
    class function PosOf(const Source: TScMemoryRef; Offset, Count: integer; const Value: Byte): integer; overload;
    class function Slice(const Source: TScMemoryRef; Offset, Count: integer): TScMemoryRef; overload;
    class function Slice(const Source: TScMemoryRef; Offset: integer): TScMemoryRef; overload;
    class procedure CopyTo(const Source: TScMemoryRef; const Destination: TScMemoryRef); overload;
    class procedure CopyTo(Source: Pointer; SrcCount: integer; const Destination: TScMemoryRef); overload;
    class procedure CopyTo(const Source: TScMemoryRef; SrcOffset: integer; Destination: Pointer; Count: integer); overload;
  end;

  TScMemoryPiece = record
    Memory: Pointer;
    Size: integer;
  end;

  TScMemoryPool = class
  private
    FMemoryPieces: array[0..NumArraySizes-1, 0..BuffersCountPerArraySize-1] of TScMemoryPiece;
    FMemoryPiecesCount: array[0..NumArraySizes-1] of integer;
    FLock: TCriticalSection;

    class function GetArraySizeIndex(Size: integer): integer;
    class function GetMaxSizeForArraySize(MinimumSize: integer): integer;

  public
    constructor Create;
    destructor Destroy; override;

    function Rent(MinimumSize: integer): TScMemoryPiece;
    procedure Return(const MemoryPiece: TScMemoryPiece);
  end;

  TScBufferSegment = class
  private
    FMemoryOwner: TScMemoryPiece;
    FMemory: TScMemoryRef;
    FAvailableMemory: TScMemoryRef;
    FNext: TScBufferSegment;
    FLength: integer;
    FStartingOffset: Int64;

    procedure SetLength(Value: integer);
    function GetWritableCount: integer;

  public
    destructor Destroy; override;

    procedure SetMemoryOwner(const MemoryPiece: TScMemoryPiece);
    procedure ResetMemory;
    procedure SetNext(Segment: TScBufferSegment);
    class function GetLength(StartSegment: TScBufferSegment; StartOffset: integer; EndSegment: TScBufferSegment; EndOffset: integer): Int64; overload;
    class function GetLength(StartPosition: Int64; EndSegment: TScBufferSegment; EndOffset: integer): Int64; overload;

    property Memory: TScMemoryRef read FMemory; // Memory is set on SetLength or ResetMemory
    property AvailableMemory: TScMemoryRef read FAvailableMemory;
    property Length: integer read FLength write SetLength;
    property Next: TScBufferSegment read FNext write FNext;
    property WritableCount: integer read GetWritableCount;

    // The sum of node length before current
    property StartingOffset: Int64 read FStartingOffset write FStartingOffset;
  end;

  TScBufferSegmentStack = class
  private
    FArray: array of TScBufferSegment;
    FCount: integer;
  public
    constructor Create(InitialCount: integer);
    destructor Destroy; override;

    function TryPop(out BufferSegment: TScBufferSegment): boolean;
    procedure Push(Item: TScBufferSegment);

    property Count: integer read FCount;
  end;

  TScSequencePosition = record
    Obj: TObject;
    Int: integer;
  end;

  TScReadOnlySequence = class
  private
    FStartObject: TObject;
    FStartInteger: integer;
    FEndObject: TObject;
    FEndInteger: integer;

    function GetLength: Int64;
    function GetFirstBuffer: TScMemoryRef;

    function GetStartPos: TScSequencePosition;
    function GetEndPos: TScSequencePosition;

    class function GetOffset(const Position: TScSequencePosition): integer;

    function GetEndPosition(StartSegment: TScBufferSegment; StartObject: TObject; StartOffset: integer;
      EndObject: TObject; EndOffset: integer; Count: Int64): TScSequencePosition;
    class function SeekMultiSegmentPosition(CurrentSegment: TScBufferSegment;
      EndObject: TObject; EndOffset: integer; Offset: Int64): TScSequencePosition;
    procedure BoundsCheck(const Position: TScSequencePosition; PosIsNotNull: boolean); overload;
    procedure BoundsCheck(SliceStartOffset: cardinal; SliceStartObject: TObject; SliceEndOffset: cardinal; SliceEndObject: TObject); overload;
    function InternalSlice(const StartPos, EndPos: TScSequencePosition): TScReadOnlySequence;
    function Seek(Offset: Int64): TScSequencePosition; overload;
    function Seek(const Start: TScSequencePosition; Offset: Int64): TScSequencePosition; overload;

  public
    constructor Create(StartSegment: TObject; StartOffset: integer; EndSegment: TObject; EndOffset: integer);

    function GetPosition(Offset: Int64): TScSequencePosition; overload;
    function GetPosition(Offset: Int64; const Origin: TScSequencePosition): TScSequencePosition; overload;
    function PositionOf(Value: Byte): TScSequencePosition;
    function TryGet(var Position: TScSequencePosition; out Memory: TScMemoryRef): boolean;
    function Slice(Start, Count: Int64): TScReadOnlySequence; overload;
    function Slice(Start: Int64; const EndPos: TScSequencePosition): TScReadOnlySequence; overload;
    function Slice(const Start: TScSequencePosition; Count: Int64): TScReadOnlySequence; overload;
    function Slice(const StartPos, EndPos: TScSequencePosition): TScReadOnlySequence; overload;
    function Slice(const Start: TScSequencePosition): TScReadOnlySequence; overload;
    function Slice(Start: Int64): TScReadOnlySequence; overload;

    function ToArray: TBytes;
    function IsEmpty: boolean;
    function IsSingleSegment: boolean;

    property Length: Int64 read GetLength;
    property First: TScMemoryRef read GetFirstBuffer;
    property StartPos: TScSequencePosition read GetStartPos;
    property EndPos: TScSequencePosition read GetEndPos;
  end;

  TScReadOnlyStream = class(TStream)
  private
    FReadOnlySequence: TScReadOnlySequence;
    FPosition: TScSequencePosition;
    FStartingOffset: Int64;
  protected
    function GetSize: Int64; {$IFDEF VER7P}override;{$ENDIF}{$IFDEF FPC}override;{$ENDIF}
  public
    constructor Create(ReadOnlySequence: TScReadOnlySequence);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TScExecMethodAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScPipe;
    FMethod: TThreadMethod;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScPipe; Method: TThreadMethod);
  end;

  TScExecWithExceptionAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScPipe;
    FEvent: TNotifyWithExceptionEvent;
    FException: Exception;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScPipe; Event: TNotifyWithExceptionEvent; Ex: Exception);
    destructor Destroy; override;
  end;

  TScFlushResult = record
    IsCanceled: boolean;
    IsCompleted: boolean;
  end;

  TScReadResult = record
    IsCanceled: boolean;
    IsCompleted: boolean;
    Buffer: TScReadOnlySequence;
  end;

  TScPipeReader = class
  private
    FPipe: TScPipe;
  public
    constructor Create(Pipe: TScPipe);

    function TryRead(var ReadResult: TScReadResult): boolean;
    function Read(MillisecondsTimeout: cardinal = INFINITE; CancellationToken: TScCancellationToken = nil): TScReadResult;
    function GetResult: TScReadResult;
    procedure AdvanceTo(const Consumed: TScSequencePosition); overload;
    procedure AdvanceTo(const Consumed, Examined: TScSequencePosition); overload;
    procedure CancelPendingRead;
    procedure Complete(E: Exception = nil);
    procedure SetOnReadResumed(Event: TThreadMethod);
    procedure SetOnWriterCompleted(Event: TScOnCompleted);
  end;

  TScPipeWriter = class
  private
    FPipe: TScPipe;
  public
    constructor Create(Pipe: TScPipe);

    function GetMemory(const SizeHint: integer = 0): TScMemoryRef;
    function Flush(MillisecondsTimeout: cardinal = INFINITE; CancellationToken: TScCancellationToken = nil): TScFlushResult;
    function Write(const Source: TScMemoryRef; MillisecondsTimeout: cardinal = INFINITE; CancellationToken: TScCancellationToken = nil): TScFlushResult;
    function GetResult: TScFlushResult;
    procedure Advance(const Count: integer);
    procedure CancelPendingFlush;
    procedure Complete(E: Exception = nil);
    procedure SetOnFlushResumed(Event: TThreadMethod);
    procedure SetOnReaderCompleted(Event: TScOnCompleted);
  end;

  TScPipeState = (sAwaiting, sCanceled, sComplited);
  TScPipeStates = set of TScPipeState;

  TScPipeStateOperation = (soReading, soTryReading, soWriting);
  TScPipeStateOperations = set of TScPipeStateOperation;

  TScPipeOptions = record
    MinimumSegmentSize: integer;
    PauseWriterThreshold: Int64;
    ResumeWriterThreshold: Int64;
  end;

  TScPipe = class
  private
    FDisposed: boolean;
    FEventsCallMode: TScEventCallMode;
    FLock: TCriticalSection;
    FMinimumSegmentSize: integer;
    FPauseWriterThreshold: Int64;
    FResumeWriterThreshold: Int64;

    FReaderCancellationToken: TScCancellationToken;
    FWriterCancellationToken: TScCancellationToken;
    FReaderState: TScPipeStates;
    FWriterState: TScPipeStates;
    FOnReaderResumed: TThreadMethod;
    FOnWriterResumed: TThreadMethod;
    FReaderAwaiter: TEvent;
    FWriterAwaiter: TEvent;
    FOnReaderCompleted: TNotifyWithExceptionEvent;
    FOnWriterCompleted: TNotifyWithExceptionEvent;
    FOnCompleteReaderException: Exception;
    FOnCompleteWriterException: Exception;
    FStateOperations: TScPipeStateOperations;

    FReader: TScPipeReader;
    FWriter: TScPipeWriter;

    FUnconsumedCount: Int64;
    FUnflushedCount: Int64;
    FLastExaminedOffset: Int64;
    FBufferSegmentPool: TScBufferSegmentStack;
    FReadHead: TScBufferSegment;
    FReadHeadOffset: integer;
    FReadTail: TScBufferSegment;
    FReadTailOffset: integer;
    FWritingHead: TScBufferSegment;
    FWritingMemory: TScMemoryRef;
    FWritingHeadBufferedCount: integer;

    function IsReaderAwaiting: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsWriterAwaiting: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsReaderCompleted: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsWriterCompleted: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure BeginRead;
    procedure BeginTryRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
    function IsWritingActive: boolean;
    function IsReadingActive: boolean;

    procedure ResetState;
    procedure CompletePipe;
    procedure AllocateWritingMemoryIfNeeded(SizeHint: integer);
    function AllocateSegment(SizeHint: integer): TScBufferSegment;
    procedure ReturnSegment(Segment: TScBufferSegment);
    procedure InternalAdvance(Count: integer);
    function Commit: boolean;
    procedure PrepareFlush(out ReaderWasResumed: boolean; out IsAwaiting: boolean;
      out FlushResult: TScFlushResult; CancellationToken: TScCancellationToken);
    procedure SetFlushResult(var FlushResult: TScFlushResult);
    procedure AdvanceReader(ConsumedSegment: TScBufferSegment; ConsumedOffset: integer;
      ExaminedSegment: TScBufferSegment; ExaminedOffset: integer); overload;
    procedure SetReadResult(out ReadResult: TScReadResult);

    procedure DoOnReaderResumed;
    procedure DoOnWriterResumed;
    procedure DoOnReaderCompleted;
    procedure DoOnWriterCompleted;
    procedure ExecAsync(Event: TNotifyWithExceptionEvent; Ex: Exception); overload;
    procedure ExecAsync(Event: TThreadMethod); overload;

  protected
    function GetMemory(SizeHint: integer): TScMemoryRef;
    procedure Advance(Count: integer);
    function Flush(CancellationToken: TScCancellationToken; out IsAwaiting: boolean): TScFlushResult;
    function Write(const Source: TScMemoryRef; CancellationToken: TScCancellationToken; out IsAwaiting: boolean): TScFlushResult;
    procedure SetOnFlushResumed(Event: TThreadMethod);
    function GetFlushResult: TScFlushResult;
    procedure CancelPendingFlush;
    procedure CompleteWriter(E: Exception);
    procedure SetOnWriterCompleted(Event: TScOnCompleted);

    procedure AdvanceReader(const Consumed: TScSequencePosition); overload;
    procedure AdvanceReader(const Consumed, Examined: TScSequencePosition); overload;
    function Read(CancellationToken: TScCancellationToken; out IsAwaiting: boolean): TScReadResult;
    procedure SetOnReadResumed(Event: TThreadMethod);
    function GetReadResult: TScReadResult;
    procedure CancelPendingRead;
    procedure CompleteReader(E: Exception);
    procedure SetOnReaderCompleted(Event: TScOnCompleted);

  public
    constructor Create(const PipeOptions: TScPipeOptions);
    destructor Destroy; override;

    procedure Reset;

    property Length: Int64 read FUnconsumedCount;

    property Reader: TScPipeReader read FReader;
    property Writer: TScPipeWriter read FWriter;

    property EventsCallMode: TScEventCallMode read FEventsCallMode write FEventsCallMode;
  end;

  TScPipeWriterStream = class(TStream)
  private
    FPipeWriter: TScPipeWriter;
  protected
    function GetSize: Int64; {$IFDEF VER7P}override;{$ENDIF}{$IFDEF FPC}override;{$ENDIF}
  public
    constructor Create(PipeWriter: TScPipeWriter);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TScDuplexPipe = class;

  TScDuplexPipePair = record
    Transport: TScDuplexPipe; // Application to Transport
    Application: TScDuplexPipe; // Transport to Application
  end;

  TScDuplexPipe = class
  private
    FInternalPipe: TScPipe;
    FInput: TScPipeReader;
    FOutput: TScPipeWriter;
  public
    constructor Create(InternalPipe: TScPipe; Input: TScPipeReader; Output: TScPipeWriter);
    destructor Destroy; override;

    class function CreateConnectionPair(const PipeOptions: TScPipeOptions): TScDuplexPipePair;

    property Input: TScPipeReader read FInput;
    property Output: TScPipeWriter read FOutput;
  end;

const
  DefaultMemoryRef: TScMemoryRef = (
    Memory: nil;
    Offset: 0;
    Length: 0);
  DefaultSequencePosition: TScSequencePosition = (
    Obj: nil;
    Int: 0);
  DefaultFlushResult: TScFlushResult = (
    IsCanceled: False;
    IsCompleted: False);
  DefaultReadResult: TScReadResult = (
    IsCanceled: False;
    IsCompleted: False;
    Buffer: nil);
  DefaultPipeOptions: TScPipeOptions = (
    MinimumSegmentSize: DefaultMinimumSegmentSize;
    PauseWriterThreshold: -1;
    ResumeWriterThreshold: -1);

implementation

uses
  Math;

resourcestring
  SWritingAfterCompleted = 'Writing is not allowed after writer was completed';
  SReadingAfterCompleted = 'Reading is not allowed after reader was completed';
  SInvalidExaminedOrConsumedPosition = 'The examined position must be greater than or equal to the consumed position';
  SInvalidExaminedPosition = 'The examined position cannot be less than the previously examined position';
  SAdvanceToInvalidCursor = 'The PipeReader has already advanced past the provided position';
  SReadingIsInProgress = 'Reading is already in progress';
  SConcurrentOperationsNotSupported = 'Concurrent reads or writes are not supported';
  SGetResultBeforeCompleted = 'Can not GetResult unless awaiter is completed';
  SReaderAndWriterHasToBeCompleted = 'Both reader and writer has to be completed to be able to reset the pipe';
  SNoReadingOperationToComplete = 'No reading operation to complete';
  SUnconsumedCountNegative = 'Unconsumed count is negative';
  SPositionOutOfRange = 'Position out of range';
  SEndPositionNotReached = 'End position was not reached during enumeration';

const
  ArraySizes: array[0..NumArraySizes-1] of integer =
    (4096, 16384, 32768, 65536);

var
  DefaultMemoryPool: TScMemoryPool;

{ TScMemoryPool }

constructor TScMemoryPool.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;
end;

destructor TScMemoryPool.Destroy;
var
  i, j: integer;
begin
  for i := Low(FMemoryPieces) to High(FMemoryPieces) do
    for j := Low(FMemoryPieces[i]) to High(FMemoryPieces[i]) do begin
      if FMemoryPieces[i, j].Memory <> nil then
        FreeMem(FMemoryPieces[i, j].Memory);
    end;

  FLock.Free;

  inherited;
end;

class function TScMemoryPool.GetArraySizeIndex(Size: integer): integer;
begin
  if Size = ArraySizes[0] then
    Result := 0
  else
  if Size = ArraySizes[1] then
    Result := 1
  else
  if Size = ArraySizes[2] then
    Result := 2
  else
  if Size = ArraySizes[3] then
    Result := 3
  else
    Result := -1;
end;

class function TScMemoryPool.GetMaxSizeForArraySize(MinimumSize: integer): integer;
begin
  if MinimumSize <= ArraySizes[0] then
    Result := ArraySizes[0]
  else
  if MinimumSize <= ArraySizes[1] then
    Result := ArraySizes[1]
  else
  if MinimumSize <= ArraySizes[2] then
    Result := ArraySizes[2]
  else
  if MinimumSize <= ArraySizes[3] then
    Result := ArraySizes[3]
  else
    Result := MinimumSize;
end;

function TScMemoryPool.Rent(MinimumSize: integer): TScMemoryPiece;
var
  ArraySizeIndex: integer;
  Count: integer;
begin
  MinimumSize := GetMaxSizeForArraySize(MinimumSize);
  ArraySizeIndex := GetArraySizeIndex(MinimumSize);

  if ArraySizeIndex >= 0 then begin
    FLock.Enter;
    try
      Count := FMemoryPiecesCount[ArraySizeIndex];
      if Count > 0 then begin
        Assert(FMemoryPieces[ArraySizeIndex, Count - 1].Memory <> nil);
        Result.Memory := FMemoryPieces[ArraySizeIndex, Count - 1].Memory;
        Result.Size := FMemoryPieces[ArraySizeIndex, Count - 1].Size;
        FMemoryPieces[ArraySizeIndex, Count - 1].Memory := nil;
        Dec(FMemoryPiecesCount[ArraySizeIndex]);
        Exit;
      end;
    finally
      FLock.Leave;
    end;
  end;

  GetMem(Result.Memory, MinimumSize);
  Result.Size := MinimumSize;
end;

procedure TScMemoryPool.Return(const MemoryPiece: TScMemoryPiece);
var
  ArraySizeIndex: integer;
  Count: integer;
begin
  if MemoryPiece.Memory = nil then
    Exit;

  ArraySizeIndex := GetArraySizeIndex(MemoryPiece.Size);

  if ArraySizeIndex >= 0 then begin
    FLock.Enter;
    try
      Count := FMemoryPiecesCount[ArraySizeIndex];
      if Count < Length(FMemoryPieces[ArraySizeIndex]) then begin
        Assert(FMemoryPieces[ArraySizeIndex, Count].Memory = nil);
        FMemoryPieces[ArraySizeIndex, Count].Memory := MemoryPiece.Memory;
        FMemoryPieces[ArraySizeIndex, Count].Size := MemoryPiece.Size;
        Inc(FMemoryPiecesCount[ArraySizeIndex]);
        Exit;
      end;
    finally
      FLock.Leave;
    end;
  end;

  FreeMem(MemoryPiece.Memory);
end;

{ TScMemoryRefHelper }

class function TScMemoryRefHelper.PosOf(const Source: TScMemoryRef; const Value: Byte): integer;
var
  i: integer;
begin
  Result := -1;
  if Source.Memory = nil then
    Exit;

  for i := Source.Offset to Source.Offset + Source.Length - 1 do begin
    if Source.Memory[i] = Value then begin
      Result := i - Source.Offset;
      Exit;
    end;
  end;
end;

class function TScMemoryRefHelper.PosOf(const Source: TScMemoryRef; Offset, Count: integer; const Value: Byte): integer;
var
  i: integer;
begin
  Result := -1;
  if Source.Memory = nil then
    Exit;

  Inc(Offset, Source.Offset);

  for i := Offset to Offset + Count - 1 do begin
    if Source.Memory[i] = Value then begin
      Result := i - Offset;
      Exit;
    end;
  end;
end;

class function TScMemoryRefHelper.Slice(const Source: TScMemoryRef; Offset, Count: integer): TScMemoryRef;
begin
  if (Offset > Source.Length) or (Count > Source.Length - Offset) then
    raise ArgumentException.Create('Offset');

  Result.Memory := Source.Memory;
  Result.Offset := Source.Offset + Offset;
  Result.Length := Count;
end;

class function TScMemoryRefHelper.Slice(const Source: TScMemoryRef; Offset: integer): TScMemoryRef;
begin
  if Offset > Source.Length then
    raise ArgumentException.Create('Offset');

  Result.Memory := Source.Memory;
  Result.Offset := Source.Offset + Offset;
  Result.Length := Source.Length - Offset;
end;

class procedure TScMemoryRefHelper.CopyTo(const Source: TScMemoryRef; const Destination: TScMemoryRef);
begin
  if (Source.Memory = nil) or (Destination.Memory = nil) then
    raise ArgumentException.Create('Memory');

  if Source.Length > Destination.Length then
    raise ArgumentException.Create('Destination too short');

  Move(Source.Memory[Source.Offset], Destination.Memory[Destination.Offset], Source.Length);
end;

class procedure TScMemoryRefHelper.CopyTo(Source: Pointer; SrcCount: integer; const Destination: TScMemoryRef);
begin
  if (Source = nil) or (Destination.Memory = nil) then
    raise ArgumentException.Create('Memory');

  if SrcCount > Destination.Length then
    raise ArgumentException.Create('Destination too short');

  Move(Source^, Destination.Memory[Destination.Offset], SrcCount);
end;

class procedure TScMemoryRefHelper.CopyTo(const Source: TScMemoryRef; SrcOffset: integer; Destination: Pointer; Count: integer);
begin
  if (Source.Memory = nil) or (Destination = nil) then
    raise ArgumentException.Create('Memory');

  Move(PtrOffset(Source.Memory, Source.Offset + SrcOffset)^, Destination^, Count);
end;

{ TScBufferSegment }

destructor TScBufferSegment.Destroy;
begin
  if (FMemoryOwner.Memory <> nil) and (DefaultMemoryPool <> nil) then
    DefaultMemoryPool.Return(FMemoryOwner);

  inherited;
end;

procedure TScBufferSegment.SetLength(Value: integer);
begin
  if Value > FAvailableMemory.Length then
    raise ArgumentException.Create;

  FLength := Value;
  FMemory := TScMemoryRefHelper.Slice(FAvailableMemory, 0, Value);
end;

procedure TScBufferSegment.SetMemoryOwner(const MemoryPiece: TScMemoryPiece);
begin
  FMemoryOwner := MemoryPiece;

  FAvailableMemory.Memory := MemoryPiece.Memory;
  FAvailableMemory.Offset := 0;
  FAvailableMemory.Length := MemoryPiece.Size;
end;

procedure TScBufferSegment.ResetMemory;
begin
  if DefaultMemoryPool <> nil then
    DefaultMemoryPool.Return(FMemoryOwner);
  FMemoryOwner.Memory := nil;

  FNext := nil;
  FStartingOffset := 0;
  FLength := 0;

  FMemory := DefaultMemoryRef;
  FAvailableMemory := DefaultMemoryRef;
end;

function TScBufferSegment.GetWritableCount: integer;
begin
  Result := FAvailableMemory.Length - FLength;
end;

procedure TScBufferSegment.SetNext(Segment: TScBufferSegment);
begin
  FNext := Segment;

  Segment := Self;
  while Segment.Next <> nil do begin
    Segment.Next.StartingOffset := Segment.StartingOffset + Segment.Length;
    Segment := Segment.Next;
  end;
end;

class function TScBufferSegment.GetLength(StartSegment: TScBufferSegment; StartOffset: integer;
  EndSegment: TScBufferSegment; EndOffset: integer): Int64;
begin
  Result := (EndSegment.StartingOffset + EndOffset) - (StartSegment.StartingOffset + StartOffset);
end;

class function TScBufferSegment.GetLength(StartPosition: Int64; EndSegment: TScBufferSegment; EndOffset: integer): Int64;
begin
  Result := EndSegment.StartingOffset + EndOffset - StartPosition;
end;

{ TScBufferSegmentStack }

constructor TScBufferSegmentStack.Create(InitialCount: integer);
begin
  inherited Create;

  if InitialCount < 4 then
    InitialCount := 4;

  SetLength(FArray, InitialCount);
  FCount := 0;
end;

destructor TScBufferSegmentStack.Destroy;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    FArray[i].Free;

  inherited;
end;

function TScBufferSegmentStack.TryPop(out BufferSegment: TScBufferSegment): boolean;
var
  Count: integer;
begin
  Count := FCount - 1;

  if cardinal(Count) >= cardinal(Length(FArray)) then begin // cardinal checks Count < 0
    BufferSegment := nil;
    Result := False;
    Exit;
  end;

  FCount := Count;
  BufferSegment := FArray[Count];
  FArray[Count] := nil;
  Result := True;
end;

procedure TScBufferSegmentStack.Push(Item: TScBufferSegment);
begin
  if cardinal(FCount) >= cardinal(Length(FArray)) then // cardinal checks Count < 0
    SetLength(FArray, 2 * Length(FArray));

  FArray[FCount] := Item;
  Inc(FCount);
end;

{ TScReadOnlySequence }

constructor TScReadOnlySequence.Create(StartSegment: TObject; StartOffset: integer; EndSegment: TObject; EndOffset: integer);
begin
  inherited Create;

  if (StartSegment = nil) or (EndSegment = nil) or (StartOffset < 0) or (EndOffset < 0) then
    raise ArgumentException.Create;

  FStartObject := StartSegment;
  FStartInteger := StartOffset;
  FEndObject := EndSegment;
  FEndInteger := EndOffset;
end;

class function TScReadOnlySequence.GetOffset(const Position: TScSequencePosition): integer;
begin
  Result := Position.Int;
end;

function TScReadOnlySequence.GetPosition(Offset: Int64): TScSequencePosition;
begin
  if Offset < 0 then
    raise ArgumentException.Create('Offset');

  Result := Seek(Offset);
end;

function TScReadOnlySequence.GetPosition(Offset: Int64; const Origin: TScSequencePosition): TScSequencePosition;
begin
  if Offset < 0 then
    raise ArgumentException.Create('Offset');

  Result := Seek(Origin, Offset);
end;

function TScReadOnlySequence.PositionOf(Value: Byte): TScSequencePosition;
var
  Pos: integer;
  CurrentSegment: TScBufferSegment;
begin
  Result := DefaultSequencePosition;

  if FStartObject = FEndObject then begin
    Pos := TScMemoryRefHelper.PosOf(TScBufferSegment(FStartObject).Memory, FStartInteger, FEndInteger - FStartInteger, Value);
    if Pos <> -1 then begin
      Result.Obj := FStartObject;
      Result.Int := FStartInteger + Pos;
    end;
  end
  else begin
    CurrentSegment := TScBufferSegment(FStartObject);
    Pos := TScMemoryRefHelper.PosOf(CurrentSegment.Memory, FStartInteger, CurrentSegment.Memory.Length - FStartInteger, Value);
    if Pos <> -1 then begin
      Result.Obj := CurrentSegment;
      Result.Int := FStartInteger + Pos;
      Exit;
    end;

    CurrentSegment := CurrentSegment.Next;
    while (CurrentSegment <> nil) and (CurrentSegment <> FEndObject) do begin
      Pos := TScMemoryRefHelper.PosOf(CurrentSegment.Memory, Value);
      if Pos <> -1 then begin
        Result.Obj := CurrentSegment;
        Result.Int := Pos;
        Exit;
      end;

      CurrentSegment := CurrentSegment.Next;
    end;

    if CurrentSegment <> nil then begin
      Pos := TScMemoryRefHelper.PosOf(CurrentSegment.Memory, 0, FEndInteger, Value);
      if Pos <> -1 then begin
        Result.Obj := CurrentSegment;
        Result.Int := Pos;
      end;
    end;
  end;
end;

function TScReadOnlySequence.Seek(Offset: Int64): TScSequencePosition;
begin
  Result := Seek(GetStartPos, Offset);
end;

function TScReadOnlySequence.Seek(const Start: TScSequencePosition; Offset: Int64): TScSequencePosition;
var
  StartObject: TObject;
  StartOffset, EndOffset: integer;
  StartSegment: TScBufferSegment;
  StartSegmentLength: integer;
begin
  StartObject := Start.Obj;
  StartOffset := GetOffset(Start);
  EndOffset := FEndInteger;

  if StartObject <> FEndObject then begin
    Assert(StartObject <> nil);
    StartSegment := TScBufferSegment(StartObject);
    StartSegmentLength := StartSegment.Memory.Length - StartOffset;

    if StartSegmentLength > Offset then begin
      Result.Obj := StartObject;
      Result.Int := StartOffset + integer(Offset);
    end
    else begin
      if StartSegmentLength < 0 then
        raise ArgumentException.Create(SPositionOutOfRange);

      Result := SeekMultiSegmentPosition(StartSegment.Next, FEndObject, EndOffset, Offset - StartSegmentLength);
    end;
  end
  else begin
    if EndOffset - StartOffset < Offset then
      raise ArgumentException.Create('Offset');

    Result.Obj := StartObject;
    Result.Int := StartOffset + integer(Offset);
  end;
end;

function TScReadOnlySequence.TryGet(var Position: TScSequencePosition; out Memory: TScMemoryRef): boolean;
var
  Next: TScSequencePosition;
  StartOffset: integer;
  StartSegment, NextSegment: TScBufferSegment;
begin
  if Position.Obj = nil then begin
    Memory := DefaultMemoryRef;
    Result := False;
    Exit;
  end;

  Next := DefaultSequencePosition;
  StartOffset := GetOffset(Position);
  StartSegment := TScBufferSegment(Position.Obj);

  if StartSegment <> FEndObject then begin
    NextSegment := StartSegment.Next;
    if NextSegment = nil then
      raise InvalidOperationException.Create(SEndPositionNotReached);

    Next.Obj := NextSegment;
    Memory := TScMemoryRefHelper.Slice(StartSegment.Memory, StartOffset);
  end
  else
    Memory := TScMemoryRefHelper.Slice(StartSegment.Memory, StartOffset, FEndInteger - StartOffset);

  Position := Next;
  Result := True;
end;

function TScReadOnlySequence.Slice(Start, Count: Int64): TScReadOnlySequence;
var
  BeginPos, EndPos: TScSequencePosition;
  StartOffset, EndOffset: integer;
  StartSegment: TScBufferSegment;
  StartSegmentLength: integer;
  BeginOffset: integer;
  BeginObject: TObject;
begin
  if (Start < 0) or (Count < 0) then
    raise ArgumentException.Create('Start, Count');

  StartOffset := FStartInteger;
  EndOffset := FEndInteger;

  if FStartObject <> FEndObject then begin
    if FStartObject = nil then
      raise ArgumentException.Create('StartObject');

    StartSegment := TScBufferSegment(FStartObject);
    StartSegmentLength := StartSegment.Memory.Length - StartOffset;

    if StartSegmentLength > Start then begin
      StartOffset := StartOffset + integer(Start);
      BeginPos.Obj := FStartObject;
      BeginPos.Int := StartOffset;
      EndPos := GetEndPosition(StartSegment, FStartObject, StartOffset, FEndObject, EndOffset, Count);
    end
    else begin
      if StartSegmentLength < 0 then
        raise ArgumentException.Create(SPositionOutOfRange);

      BeginPos := SeekMultiSegmentPosition(StartSegment.Next, FEndObject, EndOffset, Start - StartSegmentLength);
      BeginObject := BeginPos.Obj;
      BeginOffset := GetOffset(BeginPos);

      if BeginObject <> FEndObject then begin
        if BeginObject = nil then
          raise ArgumentException.Create('BeginObject');

        EndPos := GetEndPosition(TScBufferSegment(BeginObject), BeginObject, BeginOffset, FEndObject, EndOffset, Count);
      end
      else begin
        if EndOffset - BeginOffset < Count then
          raise ArgumentException.Create('Count');

        EndPos.Obj := BeginObject;
        EndPos.Int := BeginOffset + integer(Count);
      end;
    end
  end
  else begin
    if EndOffset - StartOffset < Start then
      raise ArgumentException.Create('Start');
    if EndOffset - StartOffset < Count then
      raise ArgumentException.Create('Count');

    StartOffset := StartOffset + integer(Start);
    BeginPos.Obj := FStartObject;
    BeginPos.Int := StartOffset;

    EndPos.Obj := FStartObject;
    EndPos.Int := StartOffset + integer(Count);
  end;

  Result := InternalSlice(BeginPos, EndPos);
end;

function TScReadOnlySequence.Slice(Start: Int64; const EndPos: TScSequencePosition): TScReadOnlySequence;
var
  SliceEndObject: TObject;
  StartOffset, EndOffset, SliceEndOffset: integer;
  StartSegment: TScBufferSegment;
  StartRange, SliceRange: Int64;
  StartSegmentLength: integer;
  BeginPos, SliceEndPos: TScSequencePosition;
begin
  if Start < 0 then
    raise ArgumentException.Create('Start');

  StartOffset := FStartInteger;
  EndOffset := FEndInteger;
  SliceEndObject := EndPos.Obj;
  SliceEndOffset := GetOffset(EndPos);

  if SliceEndOffset < 0 then
    raise ArgumentException.Create('EndPos');

  if SliceEndObject = nil then begin
    SliceEndObject := FStartObject;
    SliceEndOffset := StartOffset;
  end;

  if FStartObject = FEndObject then begin
    if not InRange(SliceEndOffset, StartOffset, EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);

    if SliceEndOffset - StartOffset < Start then
      raise ArgumentException.Create('Start');

    BeginPos.Obj := FStartObject;
    BeginPos.Int := StartOffset + integer(Start);
    SliceEndPos.Obj := SliceEndObject;
    SliceEndPos.Int := SliceEndOffset;
    Result := InternalSlice(BeginPos, SliceEndPos);
  end
  else begin
    StartSegment := TScBufferSegment(FStartObject);
    StartRange := StartSegment.StartingOffset + StartOffset;
    SliceRange := TScBufferSegment(SliceEndObject).StartingOffset + SliceEndOffset;

    if not InRange(SliceRange, StartRange, TScBufferSegment(FEndObject).StartingOffset + EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);

    if StartRange + Start > SliceRange then
      raise ArgumentException.Create('Start');

    StartSegmentLength := StartSegment.Memory.Length - StartOffset;

    if StartSegmentLength <= Start then begin
      if StartSegmentLength < 0 then
        raise ArgumentException.Create(SPositionOutOfRange);

      BeginPos := SeekMultiSegmentPosition(StartSegment.Next, SliceEndObject, SliceEndOffset, Start - StartSegmentLength);
      Result := InternalSlice(BeginPos, EndPos);
    end
    else begin
      BeginPos.Obj := FStartObject;
      BeginPos.Int := StartOffset + integer(Start);
      SliceEndPos.Obj := SliceEndObject;
      SliceEndPos.Int := SliceEndOffset;
      Result := InternalSlice(BeginPos, SliceEndPos);
    end;
  end;
end;

function TScReadOnlySequence.Slice(const Start: TScSequencePosition; Count: Int64): TScReadOnlySequence;
var
  SliceStartObject: TObject;
  StartOffset, EndOffset, SliceStartOffset: integer;
  SliceStartSegment: TScBufferSegment;
  StartRange, SliceRange, EndRange: Int64;
  StartSegmentLength: integer;
  StartPos, EndPos: TScSequencePosition;
begin
  if Count < 0 then
    raise ArgumentException.Create('Count');

  StartOffset := FStartInteger;
  EndOffset := FEndInteger;
  SliceStartObject := Start.Obj;
  SliceStartOffset := GetOffset(Start);

  if SliceStartOffset < 0 then
    raise ArgumentException.Create('Start');

  if SliceStartObject = nil then begin
    SliceStartObject := FStartObject;
    SliceStartOffset := StartOffset;
  end;

  if FStartObject = FEndObject then begin
    if not InRange(SliceStartOffset, StartOffset, EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);

    if EndOffset - SliceStartOffset < Count then
      raise ArgumentException.Create('Count');

    StartPos.Obj := SliceStartObject;
    StartPos.Int := SliceStartOffset;
    EndPos.Obj := SliceStartObject;
    EndPos.Int := SliceStartOffset + integer(Count);
    Result := InternalSlice(StartPos, EndPos);
  end
  else begin
    SliceStartSegment := TScBufferSegment(SliceStartObject);
    SliceRange := SliceStartSegment.StartingOffset + SliceStartOffset;
    StartRange := TScBufferSegment(FStartObject).StartingOffset + StartOffset;
    EndRange := TScBufferSegment(FEndObject).StartingOffset + EndOffset;

    if not InRange(SliceRange, StartRange, EndRange) then
      raise ArgumentException.Create(SPositionOutOfRange);

    if SliceRange + Count > EndRange then
      raise ArgumentException.Create('Count');

    StartSegmentLength := SliceStartSegment.Memory.Length - SliceStartOffset;

    if StartSegmentLength < Count then begin
      if StartSegmentLength < 0 then
        raise ArgumentException.Create(SPositionOutOfRange);

      EndPos := SeekMultiSegmentPosition(SliceStartSegment.Next, FEndObject, EndOffset, Count - StartSegmentLength);
      Result := InternalSlice(Start, EndPos);
    end
    else begin
      StartPos.Obj := SliceStartObject;
      StartPos.Int := SliceStartOffset;
      EndPos.Obj := SliceStartObject;
      EndPos.Int := SliceStartOffset + Count;
      Result := InternalSlice(StartPos, EndPos);
    end;
  end;
end;

function TScReadOnlySequence.Slice(const StartPos, EndPos: TScSequencePosition): TScReadOnlySequence;
begin
  BoundsCheck(cardinal(GetOffset(StartPos)), StartPos.Obj, cardinal(GetOffset(EndPos)), EndPos.Obj);
  Result := InternalSlice(StartPos, EndPos);
end;

function TScReadOnlySequence.Slice(const Start: TScSequencePosition): TScReadOnlySequence;
var
  StartIsNotNull: boolean;
begin
  StartIsNotNull := Start.Obj <> nil;
  BoundsCheck(Start, StartIsNotNull);
  if StartIsNotNull then
    Result := TScReadOnlySequence.Create(Start.Obj, Start.Int, FEndObject, FEndInteger)
  else
    Result := TScReadOnlySequence.Create(FStartObject, FStartInteger, FEndObject, FEndInteger);
end;

function TScReadOnlySequence.Slice(Start: Int64): TScReadOnlySequence;
var
  StartPos: TScSequencePosition;
begin
  if Start < 0 then
    raise ArgumentException.Create('Start');

  if Start = 0 then
    Result := TScReadOnlySequence.Create(FStartObject, FStartInteger, FEndObject, FEndInteger)
  else begin
    StartPos := Seek(Start);
    Result := TScReadOnlySequence.Create(StartPos.Obj, StartPos.Int, FEndObject, FEndInteger);
  end;
end;

function TScReadOnlySequence.InternalSlice(const StartPos, EndPos: TScSequencePosition): TScReadOnlySequence;
begin
  Result := TScReadOnlySequence.Create(StartPos.Obj, StartPos.Int, EndPos.Obj, EndPos.Int);
end;

function TScReadOnlySequence.GetEndPosition(StartSegment: TScBufferSegment; StartObject: TObject; StartOffset: integer;
  EndObject: TObject; EndOffset: integer; Count: Int64): TScSequencePosition;
var
  StartSegmentLength: integer;
begin
  StartSegmentLength := StartSegment.Memory.Length - StartOffset;

  if StartSegmentLength > Count then begin
    Result.Obj := StartObject;
    Result.Int := StartOffset + integer(Count);
  end
  else begin
    if StartSegmentLength < 0 then
      raise ArgumentException.Create(SPositionOutOfRange);

    Result := SeekMultiSegmentPosition(StartSegment.Next, EndObject, EndOffset, Count - StartSegmentLength);
  end;
end;

class function TScReadOnlySequence.SeekMultiSegmentPosition(CurrentSegment: TScBufferSegment;
  EndObject: TObject; EndOffset: integer; Offset: Int64): TScSequencePosition;
var
  MemoryLength: integer;
begin
  Assert(CurrentSegment <> nil);
  Assert(Offset >= 0);

  while (CurrentSegment <> nil) and (CurrentSegment <> EndObject) do begin
    MemoryLength := CurrentSegment.Memory.Length;

    if MemoryLength > Offset then begin
      Result.Obj := CurrentSegment;
      Result.Int := Offset;
      Exit;
    end;

    Offset := Offset - MemoryLength;
    CurrentSegment := CurrentSegment.Next;
  end;

  if (CurrentSegment = nil) or (EndOffset < Offset) then
    raise ArgumentException.Create;

  Result.Obj := CurrentSegment;
  Result.Int := Offset;
end;

procedure TScReadOnlySequence.BoundsCheck(const Position: TScSequencePosition; PosIsNotNull: boolean);
var
  StartOffset, EndOffset, SliceStartOffset: integer;
  StartingOffset: Int64;
begin
  SliceStartOffset := GetOffset(Position);
  StartOffset := FStartInteger;
  EndOffset := FEndInteger;

  if FStartObject = FEndObject then begin
    if not InRange(SliceStartOffset, StartOffset, EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);
  end
  else begin
    if PosIsNotNull then begin
      Assert(Position.Obj <> nil);
      StartingOffset := TScBufferSegment(Position.Obj).StartingOffset;
    end
    else
      StartingOffset := 0;

    Assert(FStartObject <> nil);
    Assert(FEndObject <> nil);
    if not InRange(StartingOffset + SliceStartOffset, TScBufferSegment(FStartObject).StartingOffset + StartOffset, TScBufferSegment(FEndObject).StartingOffset + EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);
  end;
end;

procedure TScReadOnlySequence.BoundsCheck(SliceStartOffset: cardinal; SliceStartObject: TObject; SliceEndOffset: cardinal; SliceEndObject: TObject);
var
  StartOffset, EndOffset: cardinal;
  SliceStartRange, SliceEndRange: Int64;
begin
  StartOffset := cardinal(FStartInteger);
  EndOffset := cardinal(FEndInteger);

  if FStartObject = FEndObject then begin
    if (SliceStartObject <> SliceEndObject) or (SliceStartObject <> FStartObject) or
      (SliceStartOffset > SliceEndOffset) or (SliceStartOffset < StartOffset) or (SliceEndOffset > EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);
  end
  else begin
    SliceStartRange := SliceStartOffset;
    SliceEndRange := SliceEndOffset;

    if SliceStartObject <> nil then
      SliceStartRange := SliceStartRange + TScBufferSegment(SliceStartObject).StartingOffset;

    if SliceEndObject <> nil then
      SliceEndRange := SliceEndRange + TScBufferSegment(SliceEndObject).StartingOffset;

    if SliceStartRange > SliceEndRange then
      raise ArgumentException.Create(SPositionOutOfRange);

    if (SliceStartRange < TScBufferSegment(FStartObject).StartingOffset + StartOffset) or
       (SliceEndRange > TScBufferSegment(FEndObject).StartingOffset + EndOffset) then
      raise ArgumentException.Create(SPositionOutOfRange);
  end;
end;

function TScReadOnlySequence.ToArray: TBytes;
var
  CurrentSegment: TScBufferSegment;
  Offset, Count: Int64;
begin
  Count := GetLength;
  SetLength(Result, Count);
  if Count = 0 then
    Exit;

  CurrentSegment := TScBufferSegment(FStartObject);
  if FStartObject = FEndObject then
    TScMemoryRefHelper.CopyTo(CurrentSegment.Memory, FStartInteger, @Result[0], Count)
  else begin
    TScMemoryRefHelper.CopyTo(CurrentSegment.Memory, FStartInteger, @Result[0], CurrentSegment.Memory.Length);
    Offset := CurrentSegment.Memory.Length;
    Dec(Count, CurrentSegment.Memory.Length);
    CurrentSegment := CurrentSegment.Next;

    while (Count > 0) and (CurrentSegment <> nil) and (CurrentSegment <> FEndObject) do begin
      TScMemoryRefHelper.CopyTo(CurrentSegment.Memory, 0, @Result[Offset], CurrentSegment.Memory.Length);
      Inc(Offset, CurrentSegment.Memory.Length);
      Dec(Count, CurrentSegment.Memory.Length);
      CurrentSegment := CurrentSegment.Next;
    end;

    if CurrentSegment = nil then
      raise InvalidOperationException.Create(SEndPositionNotReached);

    TScMemoryRefHelper.CopyTo(CurrentSegment.Memory, 0, @Result[Offset], FEndInteger);
  end;
end;

function TScReadOnlySequence.GetStartPos: TScSequencePosition;
begin
  Result.Obj := FStartObject;
  Result.Int := FStartInteger;
end;

function TScReadOnlySequence.GetEndPos: TScSequencePosition;
begin
  Result.Obj := FEndObject;
  Result.Int := FEndInteger;
end;

function TScReadOnlySequence.GetLength: Int64;
begin
  if FStartObject <> FEndObject then
    Result := (TScBufferSegment(FEndObject).StartingOffset + FEndInteger) - (TScBufferSegment(FStartObject).StartingOffset + FStartInteger)
  else
    Result := FEndInteger - FStartInteger;
end;

function TScReadOnlySequence.IsEmpty: boolean;
begin
  Result := Length = 0;
end;

function TScReadOnlySequence.IsSingleSegment: boolean;
begin
  Result := FStartObject = FEndObject;
end;

function TScReadOnlySequence.GetFirstBuffer: TScMemoryRef;
var
  Memory: TScMemoryRef;
begin
  if FStartObject = nil then begin
    Result := DefaultMemoryRef;
    Exit;
  end;

  Memory := TScBufferSegment(FStartObject).Memory;
  if FStartObject <> FEndObject then
    Result := TScMemoryRefHelper.Slice(Memory, FStartInteger)
  else
    Result := TScMemoryRefHelper.Slice(Memory, FStartInteger, FEndInteger - FStartInteger);
end;

{ TScReadOnlyStream }

constructor TScReadOnlyStream.Create(ReadOnlySequence: TScReadOnlySequence);
begin
  inherited Create;

  if ReadOnlySequence = nil then
    raise ArgumentException.Create('ReadOnlySequence');

  FReadOnlySequence := ReadOnlySequence;
  FPosition.Obj := FReadOnlySequence.FStartObject;
  FPosition.Int := FReadOnlySequence.FStartInteger;
  FStartingOffset := TScBufferSegment(FReadOnlySequence.FStartObject).StartingOffset + FReadOnlySequence.FStartInteger;
end;

function TScReadOnlyStream.Write(const Buffer; Count: Longint): Longint;
begin
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
  raise Exception.Create('The stream is read-only');
end;

function TScReadOnlyStream.GetSize: Int64;
begin
  Result := FReadOnlySequence.GetLength;
end;

function TScReadOnlyStream.Read(var Buffer; Count: Longint): Longint;
var
  CurrentSegment: TScBufferSegment;
  Offset, RemainSize: integer;
begin
  Result := 0;
  CurrentSegment := TScBufferSegment(FPosition.Obj);
  Offset := FPosition.Int;

  while (Count > 0) and (CurrentSegment <> nil) do begin
    if CurrentSegment = FReadOnlySequence.FEndObject then
      RemainSize := FReadOnlySequence.FEndInteger - Offset
    else
      RemainSize := CurrentSegment.Memory.Length - Offset;

    if RemainSize > 0 then begin
      if RemainSize > Count then
        RemainSize := Count;

      TScMemoryRefHelper.CopyTo(CurrentSegment.Memory, Offset, PtrOffset(@Buffer, Result), RemainSize);
      FPosition.Obj := CurrentSegment;
      FPosition.Int := Offset + RemainSize;
      Inc(Result, RemainSize);
      Dec(Count, RemainSize);
    end;

    CurrentSegment := CurrentSegment.Next;
    Offset := 0;
  end;
end;

function TScReadOnlyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  PositionOffset, Size: Int64;
begin
  if Offset < 0 then
    raise ArgumentException.Create('Offset');

  Size := FReadOnlySequence.GetLength;
  PositionOffset := TScBufferSegment(FPosition.Obj).StartingOffset + FPosition.Int - FStartingOffset;

  case Origin of
    soBeginning:
      Result := Offset;
    soCurrent:
      Result := PositionOffset + Offset;
    soEnd:
      Result := Size + Offset;
  else
    Result := PositionOffset;
  end;

  if Result >= Size then
    raise EScError.Create(sePositionMoreSize);

  if PositionOffset <> Result then
    FPosition := FReadOnlySequence.Seek(Result);
end;

{ TScExecMethodAsyncEvent }

constructor TScExecMethodAsyncEvent.Create(Owner: TScPipe; Method: TThreadMethod);
begin
  inherited Create;

  FOwner := Owner;
  FMethod := Method;
end;

procedure TScExecMethodAsyncEvent.InternalNotify;
begin
  if Assigned(FMethod) then
    FMethod();
end;

{ TScExecWithExceptionAsyncEvent }

constructor TScExecWithExceptionAsyncEvent.Create(Owner: TScPipe;
  Event: TNotifyWithExceptionEvent; Ex: Exception);
begin
  inherited Create;

  FOwner := Owner;
  FEvent := Event;
  FException := Ex;
end;

destructor TScExecWithExceptionAsyncEvent.Destroy;
begin
  FException.Free;
  inherited;
end;

procedure TScExecWithExceptionAsyncEvent.InternalNotify;
begin
  if Assigned(FEvent) then
    FEvent(FException);
end;

{ TScPipeReader }

constructor TScPipeReader.Create(Pipe: TScPipe);
begin
  inherited Create;
  FPipe := Pipe;
end;

function TScPipeReader.TryRead(var ReadResult: TScReadResult): boolean;
var
  IsAwaiting: boolean;
begin
  ReadResult := FPipe.Read(nil, IsAwaiting);
  Result := not IsAwaiting;
end;

function TScPipeReader.Read(MillisecondsTimeout: cardinal = INFINITE; CancellationToken: TScCancellationToken = nil): TScReadResult;
var
  IsAwaiting: boolean;
begin
  Result := FPipe.Read(CancellationToken, IsAwaiting);

  if IsAwaiting then begin
    if FPipe.FReaderAwaiter.WaitFor(MillisecondsTimeout) = wrTimeout then
      raise OperationCanceledException.CreateFmt(SReadingTimedOut, [MillisecondsTimeout]);

    Result := FPipe.GetReadResult;
  end;
end;

function TScPipeReader.GetResult: TScReadResult;
begin
  Result := FPipe.GetReadResult;
end;

procedure TScPipeReader.AdvanceTo(const Consumed: TScSequencePosition);
begin
  FPipe.AdvanceReader(Consumed);
end;

procedure TScPipeReader.AdvanceTo(const Consumed, Examined: TScSequencePosition);
begin
  FPipe.AdvanceReader(Consumed, Examined);
end;

procedure TScPipeReader.CancelPendingRead;
begin
  FPipe.CancelPendingRead;
end;

procedure TScPipeReader.Complete(E: Exception = nil);
begin
  FPipe.CompleteReader(E);
end;

procedure TScPipeReader.SetOnReadResumed(Event: TThreadMethod);
begin
  FPipe.SetOnReadResumed(Event);
end;

procedure TScPipeReader.SetOnWriterCompleted(Event: TScOnCompleted);
begin
  FPipe.SetOnWriterCompleted(Event);
end;

{ TScPipeWriter }

constructor TScPipeWriter.Create(Pipe: TScPipe);
begin
  inherited Create;
  FPipe := Pipe;
end;

function TScPipeWriter.GetMemory(const SizeHint: integer = 0): TScMemoryRef;
begin
  Result := FPipe.GetMemory(SizeHint);
end;

function TScPipeWriter.Flush(MillisecondsTimeout: cardinal = INFINITE; CancellationToken: TScCancellationToken = nil): TScFlushResult;
var
  IsAwaiting: boolean;
begin
  Result := FPipe.Flush(CancellationToken, IsAwaiting);

  if IsAwaiting then begin
    if FPipe.FWriterAwaiter.WaitFor(MillisecondsTimeout) = wrTimeout then
      raise OperationCanceledException.CreateFmt(SWritingTimedOut, [MillisecondsTimeout]);

    Result := FPipe.GetFlushResult;
  end;
end;

function TScPipeWriter.Write(const Source: TScMemoryRef; MillisecondsTimeout: cardinal = INFINITE; CancellationToken: TScCancellationToken = nil): TScFlushResult;
var
  IsAwaiting: boolean;
begin
  Result := FPipe.Write(Source, CancellationToken, IsAwaiting);

  if IsAwaiting then begin
    if FPipe.FWriterAwaiter.WaitFor(MillisecondsTimeout) = wrTimeout then
      raise OperationCanceledException.CreateFmt(SWritingTimedOut, [MillisecondsTimeout]);

    Result := FPipe.GetFlushResult;
  end;
end;

function TScPipeWriter.GetResult: TScFlushResult;
begin
  Result := FPipe.GetFlushResult;
end;

procedure TScPipeWriter.Advance(const Count: integer);
begin
  FPipe.Advance(Count);
end;

procedure TScPipeWriter.CancelPendingFlush;
begin
  FPipe.CancelPendingFlush;
end;

procedure TScPipeWriter.Complete(E: Exception = nil);
begin
  FPipe.CompleteWriter(E);
end;

procedure TScPipeWriter.SetOnFlushResumed(Event: TThreadMethod);
begin
  FPipe.SetOnFlushResumed(Event);
end;

procedure TScPipeWriter.SetOnReaderCompleted(Event: TScOnCompleted);
begin
  FPipe.SetOnReaderCompleted(Event);
end;

{ TScPipe }

constructor TScPipe.Create(const PipeOptions: TScPipeOptions);
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FBufferSegmentPool := TScBufferSegmentStack.Create(InitialSegmentPoolSize);
  FReaderAwaiter := CreateEvent;
  FWriterAwaiter := CreateEvent(True);

  FMinimumSegmentSize := PipeOptions.MinimumSegmentSize;
  if PipeOptions.PauseWriterThreshold = -1 then
    FPauseWriterThreshold := FMinimumSegmentSize * InitialSegmentPoolSize * 4   // 4096 * 16 * 4 = 256K
  else
    FPauseWriterThreshold := PipeOptions.PauseWriterThreshold;

  if PipeOptions.ResumeWriterThreshold = -1 then
    FResumeWriterThreshold := FMinimumSegmentSize * InitialSegmentPoolSize      // 4096 * 16 = 64K
  else
    FResumeWriterThreshold := PipeOptions.ResumeWriterThreshold;

  FReaderState := [sAwaiting];
  FWriterState := [];

  FLastExaminedOffset := -1;
  FEventsCallMode := ecDirectly;

  FReader := TScPipeReader.Create(Self);
  FWriter := TScPipeWriter.Create(Self);
end;

destructor TScPipe.Destroy;
begin
  CompletePipe;
  FReaderAwaiter.SetEvent;
  FWriterAwaiter.SetEvent;

  FReader.Free;
  FWriter.Free;

  FOnCompleteReaderException.Free;
  FOnCompleteWriterException.Free;
  FLock.Free;
  FBufferSegmentPool.Free;
  FReaderAwaiter.Free;
  FWriterAwaiter.Free;

  inherited;
end;

procedure TScPipe.ResetState;
begin
  FreeAndNil(FOnCompleteReaderException);
  FreeAndNil(FOnCompleteWriterException);

  FReaderCancellationToken := nil;
  FWriterCancellationToken := nil;
  FReaderState := [sAwaiting];
  FReaderAwaiter.ResetEvent;
  FWriterState := [];
  FWriterAwaiter.SetEvent;
  FOnReaderResumed := nil;
  FOnWriterResumed := nil;
  FStateOperations := [];

  FReadTailOffset := 0;
  FReadHeadOffset := 0;
  FLastExaminedOffset := -1;
  FUnflushedCount := 0;
  FUnconsumedCount := 0;
end;

procedure TScPipe.Reset;
begin
  FLock.Enter;
  try
    if not FDisposed then
      raise InvalidOperationException.Create(SReaderAndWriterHasToBeCompleted);

    FDisposed := False;
    ResetState;
  finally
    FLock.Leave;
  end;
end;

function TScPipe.IsReaderAwaiting: boolean;
begin
  Result := (sAwaiting in FReaderState) and not (sCanceled in FReaderState);
end;

function TScPipe.IsWriterAwaiting: boolean;
begin
  Result := (sAwaiting in FWriterState) and not (sCanceled in FWriterState);
end;

function TScPipe.IsReaderCompleted: boolean;
begin
  Result := sComplited in FReaderState;
end;

function TScPipe.IsWriterCompleted: boolean;
begin
  Result := sComplited in FWriterState;
end;

procedure TScPipe.BeginRead;
begin
  if soReading in FStateOperations then
    raise InvalidOperationException.Create(SReadingIsInProgress);

  Include(FStateOperations, soReading);
end;

procedure TScPipe.BeginTryRead;
begin
  if soReading in FStateOperations then
    raise InvalidOperationException.Create(SReadingIsInProgress);

  Include(FStateOperations, soTryReading);
end;

procedure TScPipe.EndRead;
begin
  if (FStateOperations * [soReading, soTryReading]) = [] then
    raise InvalidOperationException.Create(SNoReadingOperationToComplete);

  Exclude(FStateOperations, soReading);
  Exclude(FStateOperations, soTryReading);
end;

procedure TScPipe.BeginWrite;
begin
  Include(FStateOperations, soWriting);
end;

procedure TScPipe.EndWrite;
begin
  Exclude(FStateOperations, soWriting);
end;

function TScPipe.IsWritingActive: boolean;
begin
  Result := soWriting in FStateOperations;
end;

function TScPipe.IsReadingActive: boolean;
begin
  Result := soReading in FStateOperations;
end;

procedure TScPipe.CompletePipe;
var
  Segment, ReturnSegment: TScBufferSegment;
begin
  FLock.Enter;
  try
    if FDisposed then
      Exit;

    FDisposed := True;
    if FReadHead <> nil then
      Segment := FReadHead
    else
      Segment := FReadTail;

    ReturnSegment := nil;
    while Segment <> nil do begin
      ReturnSegment := Segment;
      Segment := Segment.Next;
      ReturnSegment.ResetMemory;
      ReturnSegment.Free;
    end;

    if ReturnSegment <> FReadTail then
      FReadTail.Free;

    if (ReturnSegment <> FWritingHead) and (FReadTail <> FWritingHead) then
      FWritingHead.Free;

    FWritingHead := nil;
    FReadHead := nil;
    FReadTail := nil;
    FLastExaminedOffset := -1;
  finally
    FLock.Leave;
  end;
end;

function TScPipe.GetMemory(SizeHint: integer): TScMemoryRef;
begin
  if IsWriterCompleted then
    raise InvalidOperationException.Create(SWritingAfterCompleted);

  if SizeHint < 0 then
    raise ArgumentException.Create('SizeHint');

  AllocateWritingMemoryIfNeeded(SizeHint);
  Result := FWritingMemory;
end;

procedure TScPipe.AllocateWritingMemoryIfNeeded(SizeHint: integer);
var
  NewSegment: TScBufferSegment;
begin
  if IsWritingActive and (FWritingMemory.Length > 0) and (FWritingMemory.Length >= SizeHint) then
    Exit;

  FLock.Enter;
  try
    BeginWrite;

    if FWritingHead = nil then begin
      NewSegment := AllocateSegment(SizeHint);

      FReadTail := NewSegment;
      FReadHead := NewSegment;
      FWritingHead := NewSegment;
      FLastExaminedOffset := 0;
    end
    else
    if (FWritingMemory.Length = 0) or (SizeHint > FWritingMemory.Length) then begin
      if FWritingHeadBufferedCount > 0 then begin
        FWritingHead.Length := FWritingHead.Length + FWritingHeadBufferedCount;
        FWritingHeadBufferedCount := 0;
      end;

      NewSegment := AllocateSegment(SizeHint);
      FWritingHead.SetNext(NewSegment);
      FWritingHead := NewSegment;
    end;
  finally
    FLock.Leave;
  end;
end;

function TScPipe.AllocateSegment(SizeHint: integer): TScBufferSegment;
var
  NewSegment: TScBufferSegment;
  SizeToRequest: integer;
begin
  if not FBufferSegmentPool.TryPop(NewSegment) then
    NewSegment := TScBufferSegment.Create;

  if DefaultMemoryPool = nil then
    raise InvalidOperationException.Create(SInternalError);

  SizeToRequest := Max(FMinimumSegmentSize, SizeHint);
  NewSegment.SetMemoryOwner(DefaultMemoryPool.Rent(SizeToRequest));

  FWritingMemory := NewSegment.AvailableMemory;
  Result := NewSegment;
end;

procedure TScPipe.ReturnSegment(Segment: TScBufferSegment);
begin
  Assert(Segment <> FReadHead);
  Assert(Segment <> FReadTail);
  Assert(Segment <> FWritingHead);

  if FBufferSegmentPool.Count < MaxSegmentPoolSize then
    FBufferSegmentPool.Push(Segment)
  else
    Segment.Free;
end;

procedure TScPipe.Advance(Count: integer);
begin
  FLock.Enter;
  try
    if cardinal(Count) > cardinal(FWritingMemory.Length) then // cardinal checks Count < 0
      raise ArgumentException.Create('Count');

    InternalAdvance(Count);
  finally
    FLock.Leave;
  end;
end;

procedure TScPipe.InternalAdvance(Count: integer);
begin
  Inc(FUnflushedCount, Count);
  Inc(FWritingHeadBufferedCount, Count);
  FWritingMemory := TScMemoryRefHelper.Slice(FWritingMemory, Count);
end;

function TScPipe.Commit: boolean;
var
  OldUnconsumedCount: Int64;
begin
  EndWrite;

  if FUnflushedCount = 0 then begin
    Result := False;
    Exit;
  end;

  FWritingHead.Length := FWritingHead.Length + FWritingHeadBufferedCount;
  // WritingHead -> ReadTail
  FReadTail := FWritingHead;
  FReadTailOffset := FWritingHead.Length;

  OldUnconsumedCount := FUnconsumedCount;
  FUnconsumedCount := FUnconsumedCount + FUnflushedCount;

  if (FPauseWriterThreshold > 0) and (OldUnconsumedCount < FPauseWriterThreshold) and (FUnconsumedCount >= FPauseWriterThreshold) and
     not IsReaderCompleted then
  begin
    Include(FWriterState, sAwaiting);
    FWriterAwaiter.ResetEvent;
  end;

  FUnflushedCount := 0;
  FWritingHeadBufferedCount := 0;
  Result := True;
  Exclude(FReaderState, sAwaiting);
  FReaderAwaiter.SetEvent;
end;

procedure TScPipe.PrepareFlush(out ReaderWasResumed: boolean; out IsAwaiting: boolean;
  out FlushResult: TScFlushResult; CancellationToken: TScCancellationToken);
begin
  ReaderWasResumed := Commit;

  if CancellationToken <> nil then
    CancellationToken.ThrowIfCancellationRequested;

  IsAwaiting := IsWriterAwaiting;

  if not IsAwaiting then
    SetFlushResult(FlushResult)
  else begin
    FlushResult := DefaultFlushResult;

    if (CancellationToken <> nil) and CancellationToken.CanBeCanceled then begin
      FWriterCancellationToken := CancellationToken;
      CancellationToken.Register(CancelPendingFlush);
    end;
  end;
end;

function TScPipe.Flush(CancellationToken: TScCancellationToken; out IsAwaiting: boolean): TScFlushResult;
var
  ReaderWasResumed: boolean;
begin
  FLock.Enter;
  try
    PrepareFlush(ReaderWasResumed, IsAwaiting, Result, CancellationToken);
  finally
    FLock.Leave;
  end;

  if ReaderWasResumed then
    DoOnReaderResumed;
end;

function TScPipe.Write(const Source: TScMemoryRef; CancellationToken: TScCancellationToken; out IsAwaiting: boolean): TScFlushResult;
var
  ReaderWasResumed: boolean;
  Src, WritingSrc: TScMemoryRef;
  WriteCount: integer;
  NewSegment: TScBufferSegment;
begin
  if IsWriterCompleted then
    raise InvalidOperationException.Create(SWritingAfterCompleted);

  FLock.Enter;
  try
    // Allocates memory and marks the state as writing
    AllocateWritingMemoryIfNeeded(0);

    if Source.Length <= FWritingMemory.Length then begin
      TScMemoryRefHelper.CopyTo(Source, FWritingMemory);
      InternalAdvance(Source.Length);
    end
    else begin
      Src := Source;

      while True do begin
        WriteCount := Min(FWritingMemory.Length, Src.Length);

        WritingSrc := TScMemoryRefHelper.Slice(Src, 0, WriteCount);
        TScMemoryRefHelper.CopyTo(WritingSrc, FWritingMemory);
        Src := TScMemoryRefHelper.Slice(Src, WriteCount);

        InternalAdvance(WriteCount);
        if Src.Length = 0 then
          break;

        FWritingHead.Length := FWritingHead.Length + WriteCount;
        FWritingHeadBufferedCount := 0;

        NewSegment := AllocateSegment(0);
        FWritingHead.SetNext(NewSegment);
        FWritingHead := NewSegment;
      end;
    end;

    PrepareFlush(ReaderWasResumed, IsAwaiting, Result, CancellationToken);
  finally
    FLock.Leave;
  end;

  if ReaderWasResumed then
    DoOnReaderResumed;
end;

procedure TScPipe.SetOnFlushResumed(Event: TThreadMethod);
begin
  FLock.Enter;
  try
    if IsWriterAwaiting then begin
      FOnWriterResumed := Event;
      Event := nil;
    end;
  finally
    FLock.Leave;
  end;

  ExecAsync(Event);
end;

function TScPipe.GetFlushResult: TScFlushResult;
var
  CancellationToken: TScCancellationToken;
begin
  CancellationToken := nil;

  try
    FLock.Enter;
    try
      if IsWriterAwaiting then
        raise InvalidOperationException.Create(SGetResultBeforeCompleted);

      CancellationToken := FWriterCancellationToken;
      FWriterCancellationToken := nil;

      SetFlushResult(Result);
    finally
      FLock.Leave;
    end;
  finally
    if CancellationToken <> nil then begin
      CancellationToken.Unregister(CancelPendingFlush);
      CancellationToken.ThrowIfCancellationRequested;
    end;
  end;
end;

procedure TScPipe.SetFlushResult(var FlushResult: TScFlushResult);
var
  TmpEx: Exception;
begin
  FlushResult.IsCanceled := sCanceled in FWriterState;
  Exclude(FWriterState, sCanceled);

  FlushResult.IsCompleted := IsReaderCompleted;
  if FlushResult.IsCompleted then
    if FOnCompleteReaderException <> nil then begin
      TmpEx := FOnCompleteReaderException;
      FOnCompleteReaderException := nil;
      raise TmpEx;
    end;
end;

procedure TScPipe.CancelPendingFlush;
begin
  FLock.Enter;
  try
    Include(FWriterState, sCanceled);
    FWriterAwaiter.SetEvent;
  finally
    FLock.Leave;
  end;

  DoOnWriterResumed;
end;

procedure TScPipe.CompleteWriter(E: Exception);
begin
  FLock.Enter;
  try
    Commit;

    FOnCompleteWriterException := E;
    Include(FWriterState, sComplited);
    Exclude(FReaderState, sAwaiting);
    FReaderAwaiter.SetEvent;
  finally
    FLock.Leave;
  end;

  if IsReaderCompleted then
    CompletePipe;

  DoOnWriterCompleted;
  DoOnReaderResumed;
end;

procedure TScPipe.SetOnWriterCompleted(Event: TScOnCompleted);
begin
  if not Assigned(Event) then
    raise ArgumentException.Create('Event');

  FLock.Enter;
  try
    if not IsWriterCompleted then begin
      FOnWriterCompleted := Event;
      Event := nil;
    end;
  finally
    FLock.Leave;
  end;

  if Assigned(Event) then
    ExecAsync(Event, nil);
end;

procedure TScPipe.AdvanceReader(const Consumed: TScSequencePosition);
begin
  AdvanceReader(Consumed, Consumed);
end;

procedure TScPipe.AdvanceReader(const Consumed, Examined: TScSequencePosition);
begin
  if IsReaderCompleted then
    raise InvalidOperationException.Create(SReadingAfterCompleted);

  AdvanceReader(TScBufferSegment(Consumed.Obj), Consumed.Int, TScBufferSegment(Examined.Obj), Examined.Int);
end;

procedure TScPipe.AdvanceReader(ConsumedSegment: TScBufferSegment; ConsumedOffset: integer;
  ExaminedSegment: TScBufferSegment; ExaminedOffset: integer);
var
  ReturnStart, ReturnEnd: TScBufferSegment;

  procedure MoveReturnEndToNextBlock;
  var
    NextBlock: TScBufferSegment;
  begin
    NextBlock := ReturnEnd.Next;
    if FReadTail = ReturnEnd then begin
      FReadTail := NextBlock;
      FReadTailOffset := 0;
    end;

    FReadHead := NextBlock;
    FReadHeadOffset := 0;

    ReturnEnd := NextBlock;
  end;

var
  WriterWasResumed: boolean;
  OldUnconsumedCount, ExaminedCount: Int64;
  ExaminedEverything: boolean;
  Next: TScBufferSegment;
begin
  // Error if examined < consumed
  if (ConsumedSegment <> nil) and (ExaminedSegment <> nil) and
    (TScBufferSegment.GetLength(ConsumedSegment, ConsumedOffset, ExaminedSegment, ExaminedOffset) < 0) then
    raise InvalidOperationException.Create(SInvalidExaminedOrConsumedPosition);

  ReturnStart := nil;
  ReturnEnd := nil;
  WriterWasResumed := False;

  FLock.Enter;
  try
    ExaminedEverything := (ExaminedSegment = FReadTail) and (ExaminedOffset = FReadTailOffset);

    if (ExaminedSegment <> nil) and (FLastExaminedOffset >= 0) then begin
      ExaminedCount := TScBufferSegment.GetLength(FLastExaminedOffset, ExaminedSegment, ExaminedOffset);
      if ExaminedCount < 0 then
        raise InvalidOperationException.Create(SInvalidExaminedPosition);
      if ExaminedCount > FUnconsumedCount then
        raise InvalidOperationException.Create(SInvalidExaminedPosition);

      OldUnconsumedCount := FUnconsumedCount;
      FUnconsumedCount := FUnconsumedCount - ExaminedCount;
      FLastExaminedOffset := ExaminedSegment.StartingOffset + ExaminedOffset;

      if (OldUnconsumedCount >= FResumeWriterThreshold) and (FUnconsumedCount < FResumeWriterThreshold) then begin
        WriterWasResumed := True;
        Exclude(FWriterState, sAwaiting);
        FWriterAwaiter.SetEvent;
      end;
    end;

    if ConsumedSegment <> nil then begin
      if FReadHead = nil then begin
        raise InvalidOperationException.Create(SAdvanceToInvalidCursor);
        Exit;
      end;

      ReturnStart := FReadHead;
      ReturnEnd := ConsumedSegment;

      if ConsumedOffset = ReturnEnd.Length then begin
        if FWritingHead <> ReturnEnd then
          MoveReturnEndToNextBlock
        else
        // FWritingHead = ReturnEnd = ConsumedSegment
        if FWritingHeadBufferedCount = 0 then begin
          if not IsWritingActive then begin
            FWritingHead := nil;
            FWritingMemory := DefaultMemoryRef;
            MoveReturnEndToNextBlock;
          end
          else begin
            FLastExaminedOffset := FReadTailOffset;
            FWritingHead.StartingOffset := 0;
            FReadHead := ConsumedSegment; // ConsumedSegment = FWritingHead
            FReadHeadOffset := ConsumedOffset;
          end;
        end
        else begin
          FReadHead := ConsumedSegment;
          FReadHeadOffset := ConsumedOffset;
        end;
      end
      else begin
        FReadHead := ConsumedSegment;
        FReadHeadOffset := ConsumedOffset;
      end;
    end;

    if ExaminedEverything and not IsWriterCompleted then begin
      Include(FReaderState, sAwaiting);
      FReaderAwaiter.ResetEvent;
    end;

    while (ReturnStart <> nil) and (ReturnStart <> ReturnEnd) do begin
      Next := ReturnStart.Next;
      ReturnStart.ResetMemory;
      ReturnSegment(ReturnStart);
      ReturnStart := Next;
    end;

    EndRead;
  finally
    FLock.Leave;
  end;

  if WriterWasResumed then
    DoOnWriterResumed;
end;

function TScPipe.Read(CancellationToken: TScCancellationToken; out IsAwaiting: boolean): TScReadResult;
begin
  if IsReaderCompleted then
    raise InvalidOperationException.Create(SReadingAfterCompleted);

  if CancellationToken <> nil then
    CancellationToken.ThrowIfCancellationRequested;

  FLock.Enter;
  try
    if IsReaderCompleted then
      raise InvalidOperationException.Create(SReadingAfterCompleted);

    IsAwaiting := IsReaderAwaiting;

    if not IsAwaiting then
      SetReadResult(Result)
    else begin
      Result := DefaultReadResult;

      if (CancellationToken <> nil) and CancellationToken.CanBeCanceled then begin
        FReaderCancellationToken := CancellationToken;
        CancellationToken.Register(CancelPendingRead);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TScPipe.SetOnReadResumed(Event: TThreadMethod);
begin
  FLock.Enter;
  try
    if IsReaderAwaiting then begin
      FOnReaderResumed := Event;
      Event := nil;
    end;
  finally
    FLock.Leave;
  end;

  ExecAsync(Event);
end;

function TScPipe.GetReadResult: TScReadResult;
var
  CancellationToken: TScCancellationToken;
begin
  CancellationToken := nil;

  try
    FLock.Enter;
    try
      if IsReaderAwaiting then
        raise InvalidOperationException.Create(SGetResultBeforeCompleted);

      CancellationToken := FReaderCancellationToken;
      FReaderCancellationToken := nil;

      SetReadResult(Result);
    finally
      FLock.Leave;
    end;
  finally
    if CancellationToken <> nil then begin
      CancellationToken.Unregister(CancelPendingRead);
      CancellationToken.ThrowIfCancellationRequested;
    end;
  end;
end;

procedure TScPipe.SetReadResult(out ReadResult: TScReadResult);
var
  TmpEx: Exception;
  ReadOnlySequence: TScReadOnlySequence;
begin
  ReadResult.IsCanceled := sCanceled in FReaderState;
  Exclude(FReaderState, sCanceled);

  ReadResult.IsCompleted := IsWriterCompleted;
  if ReadResult.IsCompleted then
    if FOnCompleteWriterException <> nil then begin
      TmpEx := FOnCompleteWriterException;
      FOnCompleteWriterException := nil;
      raise TmpEx;
    end;

  if FReadHead <> nil then begin
    ReadOnlySequence := TScReadOnlySequence.Create(FReadHead, FReadHeadOffset, FReadTail, FReadTailOffset);
    ReadResult.Buffer := ReadOnlySequence;
  end
  else
    ReadResult.Buffer := nil;

  if ReadResult.IsCanceled then
    BeginTryRead
  else
    BeginRead;
end;

procedure TScPipe.CancelPendingRead;
begin
  FLock.Enter;
  try
    Include(FReaderState, sCanceled);
    FReaderAwaiter.SetEvent;
  finally
    FLock.Leave;
  end;

  DoOnReaderResumed;
end;

procedure TScPipe.CompleteReader(E: Exception);
begin
  FLock.Enter;
  try
    if IsReadingActive then
      EndRead;

    FOnCompleteReaderException := E;
    Include(FReaderState, sComplited);
    Exclude(FWriterState, sAwaiting);
    FWriterAwaiter.SetEvent;
  finally
    FLock.Leave;
  end;

  if IsWriterCompleted then
    CompletePipe;

  DoOnReaderCompleted;
  DoOnWriterResumed;
end;

procedure TScPipe.SetOnReaderCompleted(Event: TScOnCompleted);
begin
  if not Assigned(Event) then
    raise ArgumentException.Create('Event');

  FLock.Enter;
  try
    if not IsReaderCompleted then begin
      FOnReaderCompleted := Event;
      Event := nil;
    end;
  finally
    FLock.Leave;
  end;

  if Assigned(Event) then
    ExecAsync(Event, nil);
end;

procedure TScPipe.DoOnReaderResumed;
var
  Event: TThreadMethod;
begin
  if not Assigned(FOnReaderResumed) then
    Exit;

  FLock.Enter;
  try
    Event := FOnReaderResumed;
    FOnReaderResumed := nil;
  finally
    FLock.Leave;
  end;

  ExecAsync(Event);
end;

procedure TScPipe.DoOnWriterResumed;
var
  Event: TThreadMethod;
begin
  if not Assigned(FOnWriterResumed) then
    Exit;

  FLock.Enter;
  try
    Event := FOnWriterResumed;
    FOnWriterResumed := nil;
  finally
    FLock.Leave;
  end;

  ExecAsync(Event);
end;

procedure TScPipe.DoOnReaderCompleted;
var
  Event: TNotifyWithExceptionEvent;
begin
  if not Assigned(FOnReaderCompleted) then
    Exit;

  FLock.Enter;
  try
    Event := FOnReaderCompleted;
    FOnReaderCompleted := nil;
  finally
    FLock.Leave;
  end;

  try
    ExecAsync(Event, FOnCompleteReaderException);
  finally
    FreeAndNil(FOnCompleteReaderException);
  end;
end;

procedure TScPipe.DoOnWriterCompleted;
var
  Event: TNotifyWithExceptionEvent;
begin
  if not Assigned(FOnWriterCompleted) then
    Exit;

  FLock.Enter;
  try
    Event := FOnWriterCompleted;
    FOnWriterCompleted := nil;
  finally
    FLock.Leave;
  end;

  try
    ExecAsync(Event, FOnCompleteWriterException);
  finally
    FreeAndNil(FOnCompleteWriterException);
  end;
end;

procedure TScPipe.ExecAsync(Event: TNotifyWithExceptionEvent; Ex: Exception);
var
  ExecWithExceptionAsyncEvent: TScExecWithExceptionAsyncEvent;
begin
  Assert(Assigned(Event));

  if FEventsCallMode = ecDirectly then
    Event(Ex)
  else begin
    if FEventsCallMode = ecAsynchronous then
      CheckIfAsyncEventProcessorStarted;

    ExecWithExceptionAsyncEvent := TScExecWithExceptionAsyncEvent.Create(Self, Event, CloneException(Ex));
    ExecWithExceptionAsyncEvent.Notify(FEventsCallMode);
  end;
end;

procedure TScPipe.ExecAsync(Event: TThreadMethod);
var
  ExecMethodAsyncEvent: TScExecMethodAsyncEvent;
begin
  if not Assigned(Event) then
    Exit;

  if FEventsCallMode = ecDirectly then
    Event()
  else begin
    if FEventsCallMode = ecAsynchronous then
      CheckIfAsyncEventProcessorStarted;

    ExecMethodAsyncEvent := TScExecMethodAsyncEvent.Create(Self, Event);
    ExecMethodAsyncEvent.Notify(FEventsCallMode);
  end;
end;

{ TScPipeWriterStream }

constructor TScPipeWriterStream.Create(PipeWriter: TScPipeWriter);
begin
  inherited Create;

  if PipeWriter = nil then
    raise ArgumentException.Create('PipeWriter');

  FPipeWriter := PipeWriter;
end;

function TScPipeWriterStream.Read(var Buffer; Count: Longint): Longint;
begin
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
  raise Exception.Create('The stream is write-only');
end;

function TScPipeWriterStream.GetSize: Int64;
begin
  Result := 0;
end;

function TScPipeWriterStream.Write(const Buffer; Count: Longint): Longint;
var
  WriterMemoryRef: TScMemoryRef;
  WriteCount: integer;
begin
  Result := 0;
  while Count > 0 do begin
    WriterMemoryRef := FPipeWriter.GetMemory;
    WriteCount := Min(Count, WriterMemoryRef.Length);

    TScMemoryRefHelper.CopyTo(PtrOffset(@Buffer, Result), WriteCount, WriterMemoryRef);
    Inc(Result, WriteCount);
    Dec(Count, WriteCount);
    FPipeWriter.Advance(WriteCount);
  end;
end;

function TScPipeWriterStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

{ TScDuplexPipe }

constructor TScDuplexPipe.Create(InternalPipe: TScPipe; Input: TScPipeReader; Output: TScPipeWriter);
begin
  inherited Create;

  FInternalPipe := InternalPipe;
  FInput := Input;
  FOutput := Output;
end;

destructor TScDuplexPipe.Destroy;
begin
  FInternalPipe.Free;

  inherited;
end;

class function TScDuplexPipe.CreateConnectionPair(const PipeOptions: TScPipeOptions): TScDuplexPipePair;
var
  Input, Output: TScPipe;
begin
  Input := TScPipe.Create(PipeOptions);
  Output := TScPipe.Create(PipeOptions);

  Result.Transport := TScDuplexPipe.Create(Input, Input.Reader, Output.Writer);
  Result.Application := TScDuplexPipe.Create(Output, Output.Reader, Input.Writer);
end;

initialization
  DefaultMemoryPool := TScMemoryPool.Create;

finalization
  FreeAndNil(DefaultMemoryPool);

end.
