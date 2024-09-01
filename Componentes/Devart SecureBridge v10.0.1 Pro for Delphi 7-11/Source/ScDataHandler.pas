
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScDataHandler;

interface

uses
  SysUtils,
  ScCLRClasses, ScTypes;

const
  MAX_BUFFER_SIZE = 256 * 1024 + 2048;
  SEQUENCE_LENGTH = 4;

type
  TDataPacket = class
    Data: TBytes;
    ReadOffset: integer;
    WriteOffset: integer;
    constructor Create;
    procedure CheckAndRealloc(ACount: integer);
    procedure ReduceBuffer;
    function HasUnreadData: boolean;
  end;

  TDataHandler = class
  public
    procedure OnData(DataPacket: TDataPacket; out NeedBytes: integer); virtual; abstract;
    procedure OnError(Error: Exception); virtual; abstract;
    procedure OnClosed; virtual; abstract;
  end;

implementation

{ TDataPacket }

constructor TDataPacket.Create;
begin
  inherited;
  SetLength(Data, MAX_BUFFER_SIZE);
  ReadOffset := SEQUENCE_LENGTH;
  WriteOffset := SEQUENCE_LENGTH;
end;

procedure TDataPacket.CheckAndRealloc(ACount: integer);
begin
  if ACount > Length(Data) - WriteOffset then
    SetLength(Data, WriteOffset + ACount);
end;

procedure TDataPacket.ReduceBuffer;
var
  cnt: integer;
begin
  if ReadOffset = WriteOffset then begin
    ReadOffset := SEQUENCE_LENGTH;
    WriteOffset := SEQUENCE_LENGTH;
  end
  else begin
    cnt := WriteOffset - ReadOffset;
    if (cnt > 1024) or (ReadOffset > 1024) then begin
      Buffer.BlockCopy(Data, ReadOffset, Data, SEQUENCE_LENGTH, cnt);
      ReadOffset := SEQUENCE_LENGTH;
      WriteOffset := SEQUENCE_LENGTH + cnt;
    end;
  end;
end;

function TDataPacket.HasUnreadData: boolean;
begin
  Result := ReadOffset < WriteOffset;
end;

end.

