
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSSLMessages;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes,
{$IFNDEF UNIDACPRO}
  TdsSSLTypes, TdsSSLConsts, TdsUtils;
{$ELSE}
  TdsSSLTypesUni, TdsSSLConstsUni, TdsUtilsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes,
  ScSSLTypes, ScConsts, ScUtils;
{$ENDIF}

const
  HANDSHAKE_HEADER_LENGTH = 4;
  RECORD_HEADER_LENGTH = 5;
  MAX_RECORD_LENGTH = 16384; // https://tools.ietf.org/html/rfc5246#section-6.2.1
  MAX_RECORD_FRAGMENT_LENGTH = MAX_RECORD_LENGTH + 2048;

type
  THandshakeMessage = class
  private
    FHandshakeType: TScHandshakeType;
    FFragment: TBytes;
    FDataOffset: integer;
    FReadOffset: integer;
    FWriteOffset: integer;
    FCapacity: integer;

    procedure InitHeader;
    procedure CheckAndReallocBuffer(Count: integer);

  public
    constructor Create(const Fragment: TBytes; Offset: integer); overload;
    constructor Create; overload;

    procedure Init(HandshakeType: TScHandshakeType);

    procedure Complete;

    procedure WriteBuf0(const Data: TBytes); overload;
    procedure WriteBuf0(const Data: TBytes; Offset, Count: integer); overload;
    procedure WriteBuf8(const Data: TBytes);
    procedure WriteBuf16(const Data: TBytes);
    procedure WriteBuf24(const Data: TBytes);
    procedure WriteInt8(Value: byte);
    procedure WriteInt8ByOffset(Value: byte; AOffset: integer);
    procedure WriteInt16(Value: word);
    procedure WriteInt16ByOffset(Value: word; AOffset: integer);
    procedure WriteInt24(Value: integer);
    procedure WriteInt24ByOffset(Value: integer; AOffset: integer);
    procedure WriteInt32(Value: cardinal);
    procedure WriteInt64(const Value: Int64);
    procedure SkipWrite(Count: integer);

    function ReadInt8: byte;
    function ReadInt16: word;
    function ReadInt24: integer;
    function ReadInt32: cardinal;
    function ReadInt64: Int64;
    function Read(Count: integer): TBytes;
    procedure ReadTo(var Buffer: TBytes; Offset, Count: integer);
    procedure SkipRead(Count: integer);
    function RestCount: integer;

    property HandshakeType: TScHandshakeType read FHandshakeType;
    property Fragment: TBytes read FFragment;
    property DataOffset: integer read FDataOffset;
    property ReadOffset: integer read FReadOffset;
    property WriteOffset: integer read FWriteOffset write FWriteOffset;
  end;

  TRecordMessage = class
  public
    ContentType: TScContentType;
    Version: TScSSLVersionRec;
    Fragment: TBytes;
    Uncompress: TBytes;
    ReadOffset: integer;
    WriteOffset: integer;
    MessageLength: integer;
    DataLength: integer;

    constructor Create;

    procedure CheckAndRealloc(ACount: integer);
    procedure ReduceBuffer;
    procedure InitHeader(DataLength: integer);
  end;

  TRecordMessageInfo = class
  public
    ContentType: TScContentType;
    Version: TScSSLVersionRec;
    MessageLength: integer;
    DataLength: integer;

    procedure InitHeader(Memory: PByteArray; DataLength: integer);
  end;

implementation

const
  INIT_BUFFER_SIZE = MAX_RECORD_FRAGMENT_LENGTH;
  INCR_BUFFER_SIZE = 8192;

{ THandshakeMessage }

constructor THandshakeMessage.Create(const Fragment: TBytes; Offset: integer);
var
  Size: integer;
begin
  inherited Create;

  Size := (Fragment[Offset + 1] shl 16) + (Fragment[Offset + 2] shl 8) + Fragment[Offset + 3];

  if Length(Fragment) < Offset + HANDSHAKE_HEADER_LENGTH + Size then
    raise EScError.Create(seInvalidInputArgs);

  FHandshakeType := TScLayersHelper.FindHandshakeType(Fragment[Offset]);

  FFragment := Fragment;
  FDataOffset := Offset + HANDSHAKE_HEADER_LENGTH;
  FReadOffset := Offset + HANDSHAKE_HEADER_LENGTH;
  FWriteOffset := Offset + HANDSHAKE_HEADER_LENGTH + Size;
end;

constructor THandshakeMessage.Create;
begin
  inherited Create;

  SetLength(FFragment, INIT_BUFFER_SIZE);
  FCapacity := INIT_BUFFER_SIZE;
  FDataOffset := HANDSHAKE_HEADER_LENGTH;
  FReadOffset := HANDSHAKE_HEADER_LENGTH;
  FWriteOffset := HANDSHAKE_HEADER_LENGTH;
end;

procedure THandshakeMessage.Init(HandshakeType: TScHandshakeType);
begin
  FHandshakeType := HandshakeType;
  FDataOffset := HANDSHAKE_HEADER_LENGTH;
  FReadOffset := HANDSHAKE_HEADER_LENGTH;
  FWriteOffset := HANDSHAKE_HEADER_LENGTH;
end;

procedure THandshakeMessage.InitHeader;
var
  Size: integer;
begin
  Size := FWriteOffset - FDataOffset;

  FFragment[0] := HANDSHAKE_TYPE_CODES[FHandshakeType];
  FFragment[1] := byte(Size shr 16);
  FFragment[2] := byte(Size shr 8);
  FFragment[3] := byte(Size);
end;

procedure THandshakeMessage.Complete;
begin
  InitHeader;
end;

procedure THandshakeMessage.CheckAndReallocBuffer(Count: integer);
var
  DefCount: integer;
begin
  DefCount := FWriteOffset + Count - FCapacity;
  if DefCount > 0 then begin
    if DefCount < INCR_BUFFER_SIZE then
      DefCount := INCR_BUFFER_SIZE;

    Inc(FCapacity, DefCount);
    SetLength(FFragment, FCapacity);
  end;
end;

procedure THandshakeMessage.WriteBuf0(const Data: TBytes);
var
  Count: integer;
begin
  Count := Length(Data);
  CheckAndReallocBuffer(Count);

  if Count > 0 then begin
    Move(Data[0], FFragment[FWriteOffset], Count);
    Inc(FWriteOffset, Count);
  end;
end;

procedure THandshakeMessage.WriteBuf0(const Data: TBytes; Offset, Count: integer);
begin
  CheckAndReallocBuffer(Count);

  if Count > 0 then begin
    Move(Data[Offset], FFragment[FWriteOffset], Count);
    Inc(FWriteOffset, Count);
  end;
end;

procedure THandshakeMessage.WriteBuf8(const Data: TBytes);
var
  Count: integer;
begin
  Count := byte(Length(Data));
  CheckAndReallocBuffer(Count + 1);

  FFragment[FWriteOffset] := Count;
  Inc(FWriteOffset);

  if Count > 0 then begin
    Move(Data[0], FFragment[FWriteOffset], Count);
    Inc(FWriteOffset, Count);
  end;
end;

procedure THandshakeMessage.WriteBuf16(const Data: TBytes);
var
  Count: integer;
begin
  Count := word(Length(Data));
  CheckAndReallocBuffer(Count + 2);

  FFragment[FWriteOffset] := byte(Count shr 8);
  FFragment[FWriteOffset + 1] := byte(Count);
  Inc(FWriteOffset, 2);

  if Count > 0 then begin
    Move(Data[0], FFragment[FWriteOffset], Count);
    Inc(FWriteOffset, Count);
  end;
end;

procedure THandshakeMessage.WriteBuf24(const Data: TBytes);
var
  Count: integer;
begin
  Count := Length(Data) and $FFFFFF;
  CheckAndReallocBuffer(Count + 3);

  FFragment[FWriteOffset] := byte(Count shr 16);
  FFragment[FWriteOffset + 1] := byte(Count shr 8);
  FFragment[FWriteOffset + 2] := byte(Count);
  Inc(FWriteOffset, 3);

  if Count > 0 then begin
    Move(Data[0], FFragment[FWriteOffset], Count);
    Inc(FWriteOffset, Count);
  end;
end;

procedure THandshakeMessage.WriteInt8(Value: byte);
begin
  CheckAndReallocBuffer(1);
  FFragment[FWriteOffset] := Value;
  Inc(FWriteOffset);
end;

procedure THandshakeMessage.WriteInt8ByOffset(Value: byte; AOffset: integer);
begin
  if AOffset >= FWriteOffset then
    raise EScError.Create(seInvalidInputArgs);

  FFragment[AOffset] := Value;
end;

procedure THandshakeMessage.WriteInt16(Value: word);
begin
  CheckAndReallocBuffer(2);
  FFragment[FWriteOffset] := byte(Value shr 8);
  FFragment[FWriteOffset + 1] := byte(Value);
  Inc(FWriteOffset, 2);
end;

procedure THandshakeMessage.WriteInt16ByOffset(Value: word; AOffset: integer);
begin
  if AOffset >= FWriteOffset then
    raise EScError.Create(seInvalidInputArgs);

  FFragment[AOffset] := byte(Value shr 8);
  FFragment[AOffset + 1] := byte(Value);
end;

procedure THandshakeMessage.WriteInt24(Value: integer);
begin
  CheckAndReallocBuffer(3);
  FFragment[FWriteOffset] := byte(Value shr 16);
  FFragment[FWriteOffset + 1] := byte(Value shr 8);
  FFragment[FWriteOffset + 2] := byte(Value);
  Inc(FWriteOffset, 3);
end;

procedure THandshakeMessage.WriteInt24ByOffset(Value: integer; AOffset: integer);
begin
  if AOffset >= FWriteOffset then
    raise EScError.Create(seInvalidInputArgs);

  FFragment[AOffset] := byte(Value shr 16);
  FFragment[AOffset + 1] := byte(Value shr 8);
  FFragment[AOffset + 2] := byte(Value);
end;

procedure THandshakeMessage.WriteInt32(Value: cardinal);
begin
  CheckAndReallocBuffer(4);
  FFragment[FWriteOffset] := byte(Value shr 24);
  FFragment[FWriteOffset + 1] := byte(Value shr 16);
  FFragment[FWriteOffset + 2] := byte(Value shr 8);
  FFragment[FWriteOffset + 3] := byte(Value);
  Inc(FWriteOffset, 4);
end;

procedure THandshakeMessage.WriteInt64(const Value: Int64);
begin
  CheckAndReallocBuffer(8);
  FFragment[FWriteOffset] := byte(Value shr 56);
  FFragment[FWriteOffset + 1] := byte(Value shr 48);
  FFragment[FWriteOffset + 2] := byte(Value shr 40);
  FFragment[FWriteOffset + 3] := byte(Value shr 32);
  FFragment[FWriteOffset + 4] := byte(Value shr 24);
  FFragment[FWriteOffset + 5] := byte(Value shr 16);
  FFragment[FWriteOffset + 6] := byte(Value shr 8);
  FFragment[FWriteOffset + 7] := byte(Value);
  Inc(FWriteOffset, 8);
end;

procedure THandshakeMessage.SkipWrite(Count: integer);
begin
  CheckAndReallocBuffer(Count);
  Inc(FWriteOffset, Count);
end;

function THandshakeMessage.ReadInt8: byte;
begin
  if FReadOffset >= FWriteOffset then
    raise EScError.Create(seInvalidMessage);

  Result := FFragment[FReadOffset];
  Inc(FReadOffset);
end;

function THandshakeMessage.ReadInt16: word;
begin
  if (FReadOffset + 2) > FWriteOffset then
    raise EScError.Create(seInvalidMessage);

  Result := (FFragment[FReadOffset] shl 8) +
             FFragment[FReadOffset + 1];
  Inc(FReadOffset, 2);
end;

function THandshakeMessage.ReadInt24: integer;
begin
  if (FReadOffset + 3) > FWriteOffset then
    raise EScError.Create(seInvalidMessage);

  Result := (FFragment[FReadOffset] shl 16) +
            (FFragment[FReadOffset + 1] shl 8) +
             FFragment[FReadOffset + 2];
  Inc(FReadOffset, 3);
end;

function THandshakeMessage.ReadInt32: cardinal;
begin
  if (FReadOffset + 4) > FWriteOffset then
    raise EScError.Create(seInvalidMessage);

  Result := (FFragment[FReadOffset] shl 24) +
            (FFragment[FReadOffset + 1] shl 16) +
            (FFragment[FReadOffset + 2] shl 8) +
             FFragment[FReadOffset + 3];
  Inc(FReadOffset, 4);
end;

function THandshakeMessage.ReadInt64: Int64;
begin
  if (FReadOffset + 8) > FWriteOffset then
    raise EScError.Create(seInvalidMessage);

  Result := (Int64(FFragment[FReadOffset]) shl 56) +
            (Int64(FFragment[FReadOffset + 1]) shl 48) +
            (Int64(FFragment[FReadOffset + 2]) shl 40) +
            (Int64(FFragment[FReadOffset + 3]) shl 32) +
            (Int64(FFragment[FReadOffset + 4]) shl 24) +
            (Int64(FFragment[FReadOffset + 5]) shl 16) +
            (Int64(FFragment[FReadOffset + 6]) shl 8) +
            FFragment[FReadOffset + 7];
  Inc(FReadOffset, 8);
end;

function THandshakeMessage.Read(Count: integer): TBytes;
begin
  if ((FReadOffset + Count) > FWriteOffset) or (Count < 0) then
    raise EScError.Create(seInvalidMessage);

  SetLength(Result, Count);
  if Count > 0 then
    Move(FFragment[FReadOffset], Result[0], Count);
  Inc(FReadOffset, Count);
end;

procedure THandshakeMessage.ReadTo(var Buffer: TBytes; Offset, Count: integer);
begin
  if ((FReadOffset + Count) > FWriteOffset) or (Count < 0) then
    raise EScError.Create(seInvalidMessage);

  if Count > 0 then
    Move(FFragment[FReadOffset], Buffer[Offset], Count);
  Inc(FReadOffset, Count);
end;

procedure THandshakeMessage.SkipRead(Count: integer);
begin
  if ((FReadOffset + Count) > FWriteOffset) or (Count < 0) then
    raise EScError.Create(seInvalidMessage);

  Inc(FReadOffset, Count);
end;

function THandshakeMessage.RestCount: integer;
begin
  Result := FWriteOffset - FReadOffset;
end;

{ TRecordMessage }

constructor TRecordMessage.Create;
begin
  inherited;

  System.SetLength(Fragment, MAX_RECORD_FRAGMENT_LENGTH * 3);
  ReadOffset := 0;
  WriteOffset := 0;
end;

procedure TRecordMessage.CheckAndRealloc(ACount: integer);
begin
  if WriteOffset + ACount > System.Length(Fragment) then
    System.SetLength(Fragment, WriteOffset + ACount);
end;

procedure TRecordMessage.ReduceBuffer;
begin
  if ReadOffset = WriteOffset then begin
    ReadOffset := 0;
    WriteOffset := 0;
  end
  else
  if ReadOffset >= MAX_RECORD_LENGTH then begin // performance increase
    Move(Fragment[ReadOffset], Fragment[0], WriteOffset - ReadOffset);
    Dec(WriteOffset, ReadOffset);
    ReadOffset := 0;
  end;
end;

procedure TRecordMessage.InitHeader(DataLength: integer);
begin
  Fragment[ReadOffset] := CONTENT_TYPE_CODES[ContentType];
  Fragment[ReadOffset + 1] := Version.Major;
  Fragment[ReadOffset + 2] := Version.Minor;
  Fragment[ReadOffset + 3] := byte(DataLength shr 8); // Length div 256
  Fragment[ReadOffset + 4] := byte(DataLength); // Length mod 256
end;

{ TRecordMessageInfo }

procedure TRecordMessageInfo.InitHeader(Memory: PByteArray; DataLength: integer);
begin
  Memory[0] := CONTENT_TYPE_CODES[ContentType];
  Memory[1] := Version.Major;
  Memory[2] := Version.Minor;
  Memory[3] := byte(DataLength shr 8); // Length div 256
  Memory[4] := byte(DataLength); // Length mod 256
end;

end.
