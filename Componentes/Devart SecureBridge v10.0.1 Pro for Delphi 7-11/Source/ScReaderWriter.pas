
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScReaderWriter;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRBigInteger;
{$ELSE}
  ScCLRClasses, ScSSHUtils, ScTypes, ScBigInteger, ScSSH2DataHandler;
{$ENDIF}

type
  PacketTypeSSH2 = type byte;

  TSSHDataReader = class
  protected
    FData: TBytes;
    FOffset: Integer;
    FCount: Integer;

    function GetRest: Integer;

  public
    constructor Create(const Image: TBytes); overload; virtual;
    constructor Create(const Image: TBytes; Count: Integer); overload;
    function PeekByte: Byte;
    function ReadBool: Boolean;
    function ReadByte: Byte;
    function ReadInt16: Word;
    function ReadInt32: Integer;
    function ReadUInt32: cardinal;
    function ReadInt64: Int64;
    function ReadString: TBytes; overload;
    function ReadString(const ABuffer: TBytes; const AOffset, ACount: Integer): Integer; overload;
    function Read(Count: Integer): TBytes;
    function ReadAsBigInteger: TBigInteger;
    function ReadAll: TBytes;
    procedure Reset; virtual;

    property Image: TBytes read FData;
    property Offset: Integer read FOffset;
    property Rest: Integer read GetRest;
  end;

  /// SSHDataStream implements a reusable bufer for Data serialization
  TSSHDataStream = class
  protected
    FBuffer: TBytes;
    FWritePos: Integer;
    FCapacity: Integer;
    procedure CheckAndReallocBuffer(Count: Integer);

  public
    constructor Create; overload;
    constructor Create(InitSize: Integer); overload;
    procedure Clear;
    function ToBytes: TBytes;
    procedure WriteBI(Data: TBigInteger);
    procedure WriteBuf(const Data: TBytes); overload;
    procedure WriteBuf(const Data: TValueArr; Offset, Count: Integer); overload;
    procedure WriteBool(Data: Boolean);
    procedure WriteByte(Data: Byte);
    procedure WriteInt16(Data: Word);
    procedure WriteInt32(Data: Integer);
    procedure WriteInt64(const Data: Int64);
    procedure WriteAStr(const Data: string);
    procedure WriteWStr(const Data: WideString);
    procedure WriteAsString(const Data: TBytes); overload;
    procedure WriteAsString(const Data: TValueArr; Offset, Count: Integer); overload;
    procedure WriteAsBigInteger(const Data: TBytes); overload;
    procedure WriteAsBigInteger(const Data: TBytes; Offset, Length: Integer); overload;

    property Data: TBytes read FBuffer;
    property DataLength: Integer read FWritePos;
  end;

  TSSH2DataReader = class(TSSHDataReader)
  public
  {$IFDEF SBRIDGE}
    constructor Create(Packet: TSsh2Packet); overload;
  {$ENDIF}
    function ReadBigIntWithBits: TBigInteger; //SSH2 Key File Only
    function ReadPacketType: PacketTypeSSH2;
  end;

  TSSH2DataStream = class(TSSHDataStream)
  public
    procedure WriteBigIntWithBits(bi: TBigInteger);
    procedure WritePacketType(pt: PacketTypeSSH2);
  end;

implementation

uses
{$IFNDEF SBRIDGE}
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLConsts;
{$ELSE}
  TdsUtilsUni, TdsSSLConstsUni;
{$ENDIF}
{$ELSE}
  ScUtils, ScConsts;
{$ENDIF}

const
  INIT_BUFFER_SIZE = 1024;

{ TSSHDataReader }

constructor TSSHDataReader.Create(const Image: TBytes);
begin
  inherited Create;

  FData := Image;
  FCount := Length(FData);
  FOffset := 0;
end;

constructor TSSHDataReader.Create(const Image: TBytes; Count: Integer);
begin
  inherited Create;

  FData := Image;
  FCount := Count;
  FOffset := 0;
end;

procedure TSSHDataReader.Reset;
begin
  FOffset := 0;
end;

function TSSHDataReader.PeekByte: Byte;
begin
  if FOffset >= FCount then
    raise EScError.Create(seUnexpectedEOP);

  Result := FData[FOffset];
end;

function TSSHDataReader.ReadBool: Boolean;
begin
  if FOffset >= FCount then
    raise EScError.Create(seUnexpectedEOP);

  if FData[FOffset] = 0 then
    Result := False
  else
    Result := True;
  Inc(FOffset);
end;

function TSSHDataReader.ReadByte: Byte;
begin
  if FOffset >= FCount then
    raise EScError.Create(seUnexpectedEOP);

  Result := FData[FOffset];
  Inc(FOffset);
end;

function TSSHDataReader.ReadInt16: Word;
begin
  if (FOffset + 2) > FCount then
    raise EScError.Create(seUnexpectedEOP);

  Result := (Word(FData[FOffset]) shl 8) +
            FData[FOffset + 1];
  Inc(FOffset, 2);
end;

function TSSHDataReader.ReadInt32: Integer;
begin
  if (FOffset + 4) > FCount then
    raise EScError.Create(seUnexpectedEOP);

  Result := (integer(FData[FOffset]) shl 24) +
            (integer(FData[FOffset + 1]) shl 16) +
            (integer(FData[FOffset + 2]) shl 8) +
             integer(FData[FOffset + 3]);
  Inc(FOffset, 4);
end;

function TSSHDataReader.ReadUInt32: cardinal;
begin
  if (FOffset + 4) > FCount then
    raise EScError.Create(seUnexpectedEOP);

  Result := (cardinal(FData[FOffset]) shl 24) +
            (cardinal(FData[FOffset + 1]) shl 16) +
            (cardinal(FData[FOffset + 2]) shl 8) +
             cardinal(FData[FOffset + 3]);
  Inc(FOffset, 4);
end;

function TSSHDataReader.ReadInt64: Int64;
begin
  if (FOffset + 8) > FCount then
    raise EScError.Create(seUnexpectedEOP);

  Result := (Int64(FData[FOffset]) shl 56) +
            (Int64(FData[FOffset + 1]) shl 48) +
            (Int64(FData[FOffset + 2]) shl 40) +
            (Int64(FData[FOffset + 3]) shl 32) +
            (Int64(FData[FOffset + 4]) shl 24) +
            (Int64(FData[FOffset + 5]) shl 16) +
            (Int64(FData[FOffset + 6]) shl 8) +
            FData[FOffset + 7];
  Inc(FOffset, 8);
end;

function TSSHDataReader.ReadString: TBytes;
var
  Length: Integer;
begin
  Length := ReadInt32;
  Result := Read(Length);
end;

function TSSHDataReader.ReadString(const ABuffer: TBytes; const AOffset, ACount: Integer): Integer;
begin
  Result := ReadInt32;
  if Result > ACount then
    raise EScError.Create(seUnexpectedEOP);

  if ((FOffset + Result) > FCount) or (Result < 0) then
    raise EScError.Create(seUnexpectedEOP);

  if Result > 0 then
    Buffer.BlockCopy(FData, FOffset, ABuffer, AOffset, Result);
  Inc(FOffset, Result);
end;

function TSSHDataReader.Read(Count: Integer): TBytes;
begin
  if ((FOffset + Count) > FCount) or (Count < 0) then
    raise EScError.Create(seUnexpectedEOP);

  SetLength(Result, Count);
  if Count > 0 then
    Buffer.BlockCopy(FData, FOffset, Result, 0, Count);
  Inc(FOffset, Count);
end;

function TSSHDataReader.ReadAsBigInteger: TBigInteger;
begin
  Result := TBigInteger.Create(ReadString);
end;

function TSSHDataReader.ReadAll: TBytes;
begin
  SetLength(Result, FCount - FOffset);
  if Length(Result) > 0 then
    Buffer.BlockCopy(FData, FOffset, Result, 0, Length(Result));
  FOffset := FCount;
end;

function TSSHDataReader.GetRest: Integer;
begin
  Result := FCount - FOffset;
end;

{ TSSHDataStream }

constructor TSSHDataStream.Create;
begin
  inherited;
  SetLength(FBuffer, INIT_BUFFER_SIZE);
  FCapacity := INIT_BUFFER_SIZE;
  FWritePos := 0;
end;

constructor TSSHDataStream.Create(InitSize: Integer);
begin
  inherited Create;
  SetLength(FBuffer, InitSize);
  FCapacity := InitSize;
  FWritePos := 0;
end;

procedure TSSHDataStream.CheckAndReallocBuffer(Count: Integer);
var
  DefCount: integer;
begin
  DefCount := FWritePos + Count - FCapacity;

  if DefCount > 0 then begin
    if FCapacity < INIT_BUFFER_SIZE then begin
      if DefCount < FCapacity then
        DefCount := FCapacity;
    end
    else begin
      if DefCount < INIT_BUFFER_SIZE then
        DefCount := INIT_BUFFER_SIZE;
    end;

    Inc(FCapacity, DefCount);
    SetLength(FBuffer, FCapacity);
  end;
end;

procedure TSSHDataStream.Clear;
begin
  FWritePos := 0;
end;

function TSSHDataStream.ToBytes: TBytes;
begin
  SetLength(Result, FWritePos);
  Buffer.BlockCopy(FBuffer, 0, Result, 0, FWritePos);
end;

procedure TSSHDataStream.WriteBI(Data: TBigInteger);
var
  buf: TBytes;
begin
  buf := Data.GetBytes;
  WriteAsBigInteger(buf);
end;

procedure TSSHDataStream.WriteBuf(const Data: TBytes);
var
  Count: Integer;
begin
  Count := Length(Data);
  CheckAndReallocBuffer(Count);
  Move(Data[0], FBuffer[FWritePos], Count);
  Inc(FWritePos, Count);
end;

procedure TSSHDataStream.WriteBuf(const Data: TValueArr; Offset, Count: Integer);
begin
  CheckAndReallocBuffer(Count);
  Move(Data[Offset], FBuffer[FWritePos], Count);
  Inc(FWritePos, Count);
end;

procedure TSSHDataStream.WriteBool(Data: Boolean);
begin
  CheckAndReallocBuffer(1);
  if Data then
    FBuffer[FWritePos] := 1
  else
    FBuffer[FWritePos] := 0;
  Inc(FWritePos);
end;

procedure TSSHDataStream.WriteByte(Data: Byte);
begin
  CheckAndReallocBuffer(1);
  FBuffer[FWritePos] := Data;
  Inc(FWritePos);
end;

procedure TSSHDataStream.WriteInt16(Data: Word);
begin
  CheckAndReallocBuffer(2);
  FBuffer[FWritePos] := byte(Data shr 8);
  FBuffer[FWritePos + 1] := byte(Data);
  Inc(FWritePos, 2);
end;

procedure TSSHDataStream.WriteInt32(Data: Integer);
begin
  CheckAndReallocBuffer(4);
  FBuffer[FWritePos] := byte(cardinal(Data) shr 24);
  FBuffer[FWritePos + 1] := byte(cardinal(Data) shr 16);
  FBuffer[FWritePos + 2] := byte(cardinal(Data) shr 8);
  FBuffer[FWritePos + 3] := byte(Data);
  Inc(FWritePos, 4);
end;

procedure TSSHDataStream.WriteInt64(const Data: Int64);
begin
  CheckAndReallocBuffer(8);
  FBuffer[FWritePos] := byte(Data shr 56);
  FBuffer[FWritePos + 1] := byte(Data shr 48);
  FBuffer[FWritePos + 2] := byte(Data shr 40);
  FBuffer[FWritePos + 3] := byte(Data shr 32);
  FBuffer[FWritePos + 4] := byte(Data shr 24);
  FBuffer[FWritePos + 5] := byte(Data shr 16);
  FBuffer[FWritePos + 6] := byte(Data shr 8);
  FBuffer[FWritePos + 7] := byte(Data);
  Inc(FWritePos, 8);
end;

procedure TSSHDataStream.WriteAStr(const Data: string);
begin
  WriteInt32(Length(Data));
  if Length(Data) > 0 then
    WriteBuf(Encoding.Default.GetBytes(Data));
end;

procedure TSSHDataStream.WriteWStr(const Data: WideString);
var
  buf: TBytes;
begin
  if Length(Data) > 0 then begin
    SetLength(buf, 0);
    buf := Encoding.UTF8.GetBytes(Data);
    WriteInt32(Length(buf));
    if Length(buf) > 0 then
      WriteBuf(buf);
  end
  else
    WriteInt32(0);
end;

procedure TSSHDataStream.WriteAsString(const Data: TBytes);
begin
  WriteInt32(Length(Data));
  if Length(Data) > 0 then
    WriteBuf(Data);
end;

procedure TSSHDataStream.WriteAsString(const Data: TValueArr; Offset, Count: Integer);
begin
  WriteInt32(Count);
  if Count > 0 then
    WriteBuf(Data, Offset, Count);
end;

procedure TSSHDataStream.WriteAsBigInteger(const Data: TBytes);
var
  len: Integer;
begin
  len := Length(Data);

  if len > 0 then begin
    if Data[0] >= $80 then begin
      WriteInt32(len + 1);
      WriteByte(0);
    end
    else
      WriteInt32(len);

    WriteBuf(Data);
  end
  else
    WriteInt32(0);
end;

procedure TSSHDataStream.WriteAsBigInteger(const Data: TBytes; Offset, Length: Integer);
var
  len: Integer;
begin
  len := Length;
  if (Length > 0) and (Data[Offset] >= $80) then begin
    WriteInt32(len + 1);
    WriteByte(0);
  end
  else
    WriteInt32(len);

  if Length > 0 then
    WriteBuf(TValueArr(Data), Offset, Length);
end;

{ TSSH2DataReader }

{$IFDEF SBRIDGE}
constructor TSSH2DataReader.Create(Packet: TSsh2Packet);
begin
  Create(Packet.Data, Packet.DataLength);
end;
{$ENDIF}

function TSSH2DataReader.ReadBigIntWithBits: TBigInteger;
var
  Bits: Integer;
begin
  Bits := ReadInt32;
  if Bits < 0 then
    raise EScError.Create(seUnexpectedEOP);

  Result := TBigInteger.Create(Read((Bits + 7) shr 3));
end;

function TSSH2DataReader.ReadPacketType: PacketTypeSSH2;
begin
  Result := PacketTypeSSH2(ReadByte);
end;

{ TSSH2DataStream }

procedure TSSH2DataStream.WriteBigIntWithBits(bi: TBigInteger);
begin
  WriteInt32(bi.BitCount);
  WriteBuf(bi.GetBytes);
end;

procedure TSSH2DataStream.WritePacketType(pt: PacketTypeSSH2);
begin
  WriteByte(pt);
end;

end.

