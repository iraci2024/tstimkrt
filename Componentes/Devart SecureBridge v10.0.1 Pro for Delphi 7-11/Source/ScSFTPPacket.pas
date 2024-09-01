
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSFTPPacket;

interface

uses
  SysUtils, ScTypes, ScReaderWriter, ScSFTPConsts;

type
  TSFTPPacketReader = class(TSSHDataReader)
  public
    constructor Create(const Image: TBytes); override;
    procedure CheckAndReallocBuffer(Count: Integer);
    function ReadPacketType: SFTPPacketType;
    procedure Reset; override;
  end;

  TSFTPPacketWriter = class(TSSHDataStream)
  public
    procedure InitPacket(pt: SFTPPacketType);
    function PacketData: TBytes;
    property PacketLength: Integer read FWritePos;
  end;

implementation

uses
  Math, ScUtils, ScDECUtil;

const
  INIT_SIZE = 128;
  MAX_INCR  = 1024;

{ TSFTPPacketReader }

constructor TSFTPPacketReader.Create(const Image: TBytes);
begin
  inherited Create(Image);

  if FData = nil then
    SetLength(FData, INIT_SIZE);
end;

procedure TSFTPPacketReader.CheckAndReallocBuffer(Count: Integer);
var
  len, cnt: Integer;
begin
  len := Length(FData);
  if (FOffset + Count) > len then begin
    cnt := Max(Min(len, MAX_INCR), FOffset + Count - len);
    SetLength(FData, len + cnt);
  end;
  FCount := FCount + Count;
end;

function TSFTPPacketReader.ReadPacketType: SFTPPacketType;
begin
  Result := SFTPPacketType(ReadByte);
end;

procedure TSFTPPacketReader.Reset;
begin
  FOffset := 0;
  FCount := 0;
end;

{ TSFTPPacketWriter }

procedure TSFTPPacketWriter.InitPacket(pt: SFTPPacketType);
begin
  FWritePos := 4; // for packet length
  WriteByte(pt);
end;

function TSFTPPacketWriter.PacketData: TBytes;
begin
  PutIntBE(FWritePos - 4, TValueArr(FBuffer), 0);
  Result := FBuffer;
end;

end.

