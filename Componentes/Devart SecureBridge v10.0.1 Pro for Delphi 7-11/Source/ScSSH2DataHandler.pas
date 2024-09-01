
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSH2DataHandler;

interface

uses
  SysUtils,
  ScCLRClasses, ScUtils, ScTypes, ScSSHUtils, ScDataHandler, ScRNG,
  ScCryptoTransformIntf, ScAlgorithmSupport, ScSSHSocket;

type
{ SSH2 Packet Structure

  uint32    packet_length
  byte      padding_length
  byte[n1]  payload; n1 = packet_length - padding_length - 1
  byte[n2]  random padding; n2 = padding_length (max 255)
  byte[m]   mac (message authentication code); m = mac_length
  4+1+n1+n2 must be a multiple of the cipher block size
}
  TSsh2Packet = class
  private
    FPacketLength: Integer;
    FPayloadLength: Integer;
    FData: TBytes;

  public
    procedure WriteTo(Strm: TPlainSocket);
    procedure InitWithPayload(const Payload: TBytes; Offset, Count: integer;
      Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression;
      Sequence: Integer; rnd: IScRandom);
    procedure InitFromStream(var Buf: TBytes; Offset: integer;
      Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression;
      Sequence: integer);

    property Data: TBytes read FData;
    property DataLength: Integer read FPayloadLength;
  end;

  TSsh2DataHandler = class(TDataHandler)
  private
    FFirstBlockDecrypted: Boolean;
    FSequence: Integer;
    FCipher: ICryptoTransform;
    FMac: IHashTransform;
    FCompression: IScCompression;
    FPacket: TSsh2Packet;

    //returns true if a new packet could be obtained
    function ConstructPacket(DataPacket: TDataPacket; out NeedBytes: integer): boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetCipher(c: ICryptoTransform; m: IHashTransform);
    procedure SetCompression(c: IScCompression);

    function ProcessData(DataPacket: TDataPacket; out NeedBytes: integer): TSsh2Packet;
    procedure OnData(DataPacket: TDataPacket; out NeedBytes: integer); override;
    procedure OnPacket(Packet: TSsh2Packet); virtual; abstract;
  end;

implementation

uses
  ScDECUtil, ScConsts;

{ TSsh2Packet }

procedure TSsh2Packet.WriteTo(Strm: TPlainSocket);
begin
  Strm.Write(FData, SEQUENCE_LENGTH, FPacketLength - SEQUENCE_LENGTH);
end;

procedure TSsh2Packet.InitWithPayload(const Payload: TBytes; Offset, Count: integer;
  Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression;
  Sequence: Integer; rnd: IScRandom);
var
  CipherBlockSize, MacSize: integer;
  r: integer;
  PBuf: TBytes;
  CompressLen: cardinal;
  MacBuf: TBytes;
begin
  SetLength(MacBuf, 0);
  if Compress <> nil then begin
    CompressLen := ((Count + (Count div 10) + 12) + 255) and not 255;
    SetLength(PBuf, CompressLen);
    Compress.Compress(TValueArr(Payload), Offset, Count, TValueArr(PBuf), 0, CompressLen);
    Count := CompressLen;
    Offset := 0;
  end
  else begin
    PBuf := Payload;
  end;

  if Cipher = nil then
    CipherBlockSize := 8
  else
    CipherBlockSize := Cipher.OutputBlockSize;

  if Mac = nil then
    MacSize := 0
  else
    MacSize := Mac.HashSize;

  // (packet_length(4) + padding_length(1) + payload + padding(r)) mod CipherBlockSize == 0
  // r =  CipherBlockSize - ((5 + payload) mod CipherBlockSize) = 16 - 5 - (payload mod CipherBlockSize)
  r := 11 - Count mod CipherBlockSize;
  while r < 4 do
    r := r + CipherBlockSize; //block size is 8, and Padding length is at least 4 bytes

  FPayloadLength := 1 + Count + r;
  FPacketLength := SEQUENCE_LENGTH + 4 + FPayloadLength + MacSize;
  if Length(FData) < FPacketLength then
    SetLength(FData, FPacketLength);

  PutIntBE(FPayloadLength, TValueArr(FData), SEQUENCE_LENGTH);
  FData[8] := byte(r);
  Buffer.BlockCopy(PBuf, Offset, FData, 9, Count);
  rnd.Random(FData, 9 + Count, r);

  if Mac <> nil then begin
    PutIntBE(Sequence, TValueArr(FData), 0);
    MacBuf := Mac.ComputeHash(TValueArr(Data), 0, SEQUENCE_LENGTH + 4 + FPayloadLength);
    Buffer.BlockCopy(MacBuf, 0, FData, SEQUENCE_LENGTH + 4 + FPayloadLength, Length(MacBuf));
  end;

  if Cipher <> nil then
    Cipher.TransformBlock(FData, SEQUENCE_LENGTH, 4 + FPayloadLength);
end;

procedure TSsh2Packet.InitFromStream(var Buf: TBytes; Offset: integer;
  Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression;
  Sequence: integer);
var
  PacketLength, PaddingLen: integer;
  InOffset, InLength, OutOffset, OutLength: cardinal;
  MacBuf: TBytes;
begin
  SetLength(MacBuf, 0);
  PacketLength := GetIntBE(Buf, Offset);
  if (PacketLength <= 0) or (PacketLength > SSH_MAX_PACKET_SIZE) then
    raise EScError.CreateFmt(SInvalidPacketSize, [PacketLength], seInvalidPacketSize);

  if Cipher <> nil then
    if 4 + PacketLength > Cipher.OutputBlockSize then
      Cipher.TransformBlock(Buf, Offset + Cipher.OutputBlockSize, 4 + PacketLength - Cipher.OutputBlockSize);

  PaddingLen := Buf[Offset + 4];
  if PaddingLen < 4 then
    raise EScError.CreateFmt(SInvalidPaddingSize, [PaddingLen], seInvalidPaddingSize);

  FPayloadLength := PacketLength - 1 - PaddingLen;

  try
    if Compress <> nil then begin
      if Length(FData) < FPayloadLength shl 2 then
        SetLength(FData, FPayloadLength shl 2); /// *4

      InOffset := Offset + 5;
      InLength := FPayloadLength;
      OutOffset := 0;
      OutLength := Length(FData);
      FPayloadLength := 0;

      while not Compress.Decompress(TValueArr(Buf), InOffset, InLength, TValueArr(FData), OutOffset, OutLength) do begin
        Inc(OutOffset, OutLength);
        OutLength := InLength shl 2;
        SetLength(FData, Length(FData) + integer(OutLength));
      end;

      FPayloadLength := OutOffset + OutLength;
    end
    else begin
      if Length(FData) < FPayloadLength then
        SetLength(FData, FPayloadLength);

      Buffer.BlockCopy(Buf, Offset + 5, FData, 0, FPayloadLength);
    end;
  except
    Buffer.BlockCopy(Buf, Offset + 5, FData, 0, FPayloadLength);
  end;

  if Mac <> nil then begin
    Assert((Offset - SEQUENCE_LENGTH) >= 0);
    PutIntBE(Sequence, TValueArr(Buf), Offset - SEQUENCE_LENGTH);
    MacBuf := Mac.ComputeHash(TValueArr(Buf), Offset - SEQUENCE_LENGTH, SEQUENCE_LENGTH + 4 + PacketLength);
    if MemCompare(@MacBuf[0], @Buf[Offset + 4 + PacketLength], Mac.HashSize) <> 0 then
      raise EScError.Create(seHashVerificationNotCorrespond);
  end;
end;

{ TSsh2DataHandler }

constructor TSsh2DataHandler.Create;
begin
  inherited Create;

  FFirstBlockDecrypted := False;
  FSequence := 0;
  FCipher := nil;
  FMac := nil;
  FCompression := nil;
  FPacket := TSsh2Packet.Create;
end;

destructor TSsh2DataHandler.Destroy;
begin
  FPacket.Free;
  FCipher := nil;
  FMac := nil;
  FCompression := nil;

  inherited;
end;

procedure TSsh2DataHandler.SetCipher(c: ICryptoTransform; m: IHashTransform);
begin
  FCipher := c;
  FMac := m;
end;

procedure TSsh2DataHandler.SetCompression(c: IScCompression);
begin
  FCompression := c;
end;

function TSsh2DataHandler.ProcessData(DataPacket: TDataPacket; out NeedBytes: integer): TSsh2Packet;
begin
  if ConstructPacket(DataPacket, NeedBytes) then begin
    Result := FPacket;
    DataPacket.ReduceBuffer;
  end
  else
    Result := nil;
end;

procedure TSsh2DataHandler.OnData(DataPacket: TDataPacket; out NeedBytes: integer);
begin
  NeedBytes := 0;
  try
    while ConstructPacket(DataPacket, NeedBytes) do
      OnPacket(FPacket);

    DataPacket.ReduceBuffer;
  except
    on ex: EScError do
      OnError(ex);
  end;
end;

function TSsh2DataHandler.ConstructPacket(DataPacket: TDataPacket; out NeedBytes: integer): boolean;
  function GetFirstBlockSize: integer;
  begin
    if FCipher <> nil then
      Result := FCipher.OutputBlockSize
    else
      Result := 4;
  end;

var
  PacketLength: integer;
begin
  Result := False;
  NeedBytes := 0;

  if DataPacket.WriteOffset = DataPacket.ReadOffset then
    Exit;

  if not FFirstBlockDecrypted then begin
    NeedBytes := GetFirstBlockSize - (DataPacket.WriteOffset - DataPacket.ReadOffset);
    if NeedBytes > 0 then
      Exit;

    if FCipher <> nil then
      FCipher.TransformBlock(DataPacket.Data, DataPacket.ReadOffset, FCipher.OutputBlockSize);
    FFirstBlockDecrypted := True;
  end;

  PacketLength := GetIntBE(DataPacket.Data, DataPacket.ReadOffset);
  NeedBytes := (4 + PacketLength) - (DataPacket.WriteOffset - DataPacket.ReadOffset);
  if FMac <> nil then
    NeedBytes := NeedBytes + FMac.HashSize;

  if NeedBytes > SSH_MAX_PACKET_SIZE then
    raise EScError.CreateFmt(SInvalidPacketSize, [NeedBytes], seInvalidPacketSize);
  if NeedBytes > 0 then
    Exit;

  FPacket.InitFromStream(DataPacket.Data, DataPacket.ReadOffset, FCipher, FMac, FCompression, FSequence);
  DataPacket.ReadOffset := DataPacket.ReadOffset + 4 + PacketLength;
  if FMac <> nil then
    DataPacket.ReadOffset := DataPacket.ReadOffset + FMac.HashSize;
  FFirstBlockDecrypted := False;

  Inc(FSequence);
  Result := True;
end;

end.

