
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSHSocket;

interface

uses
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.NetinetIn, Posix.SysSocket,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  {$IFNDEF BCB}
    {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  {$ENDIF}
  WinSock,
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  ScCLRClasses, ScVio,
  ScConsts, ScTypes, ScFunctions, ScUtils, ScSSHUtils, ScDataHandler;

type
  TPlainSocket = class
  private
    FDataPacket: TDataPacket;
    FLock: TCriticalSection;
    FVio: TCRVio;
    FHandler: TDataHandler;
    FClosed: Boolean;

  protected
    procedure RepeatCallback(Result: IScAsyncResult);
    function GetClosed: Boolean;
    procedure DoOnClose(Sender: TObject);

  public
    constructor Create(vio: TCRVio; h: TDataHandler);
    destructor Destroy; override;

    procedure Write(const data: TBytes; offset: Integer; length: Integer);
    procedure Close;
    procedure RepeatAsyncRead;
    function SyncRead(Count: integer = 0): TDataPacket;

    property Vio: TCRVio read FVio;
    property Closed: Boolean read GetClosed;
  end;


{$IFNDEF MSWINDOWS}
  PSockAddrIn = Psockaddr_in;
{$ENDIF}

  TIPEndPoint = class
  private
    FSockAddr: PSockAddrIn;
    function GetPort: integer;

  public
    constructor Create(SockAddr: PSockAddrIn);
    destructor Destroy; override;
    function ToString: string; reintroduce;

    property Port: integer read GetPort;
  end;

implementation

uses
  ScVioTcp;

{$IFDEF UNIX}
function inet_ntoa(Entry: in_addr): AnsiString;
begin
  Result := NetAddrToStr(Entry);
end;
{$ENDIF}

constructor TIPEndPoint.Create(SockAddr: PSockAddrIn);
var
  Size: {$IFDEF POSIX}socklen_t{$ELSE}Integer{$ENDIF};
begin
  inherited Create;

  if SockAddr = nil then
    FSockAddr := nil
  else begin
    Size := TCRVioTcp.GetSockAddrSize(PSockAddr(SockAddr));
    GetMem(FSockAddr, Size);
    Move(SockAddr^, FSockAddr^, Size);
  end;
end;

destructor TIPEndPoint.Destroy;
begin
  FreeMem(FSockAddr);

  inherited;
end;

function TIPEndPoint.ToString: string;
begin
  if FSockAddr = nil then
    Result := ''
  else
    Result := string(inet_ntoa({$IFDEF POSIX}Psockaddr_in{$ENDIF}(FSockAddr).sin_addr));
end;

function TIPEndPoint.GetPort: integer;
begin
  if FSockAddr = nil then
    Result := 0
  else
    Result := {$IFDEF POSIX}Psockaddr_in{$ENDIF}(FSockAddr).sin_port;
end;

{ TPlainSocket }

constructor TPlainSocket.Create(vio: TCRVio; h: TDataHandler);
begin
  inherited Create;

  FDataPacket := TDataPacket.Create;
  FLock := TCriticalSection.Create;

  FHandler := h;
  FVio := vio;
  FVio.OnClose := DoOnClose;
end;

destructor TPlainSocket.Destroy;
begin
  lock(FLock);
  try
    Close;
    FVio.Close;
  finally
    unlock(FLock);
  end;

  FHandler := nil;
  FLock.Free;
  FDataPacket.Free;

  inherited;
end;

procedure TPlainSocket.Close;
begin
  if FClosed then
    Exit;

  lock(FLock);
  try
    if FClosed then
      Exit;
    FClosed := True;
  finally
    unlock(FLock);
  end;

  try
    if FHandler <> nil then
      FHandler.OnClosed;
  except
  end;

  FVio.Close;
end;

procedure TPlainSocket.DoOnClose(Sender: TObject);
begin
  Close;
end;

function TPlainSocket.GetClosed: Boolean;
begin
  Result := not FVio.Connected;
end;

procedure TPlainSocket.Write(const data: TBytes; offset: Integer; length: Integer);
var
  WriteCount: Integer;
begin
  WriteCount := FVio.Write(TValueArr(data), offset, length);
  if WriteCount <> length then
    raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
end;

function TPlainSocket.SyncRead(Count: integer = 0): TDataPacket;
var
  len: integer;
begin
  if Count > 0 then begin
    FDataPacket.CheckAndRealloc(Count);
    while Count > 0 do begin
      len := FVio.ReadNoWait(TValueArr(FDataPacket.Data), FDataPacket.WriteOffset, Count);
      if len <= 0 then begin
        if FVio.LastError <> '' then
          raise SocketException.Create(FVio.LastError, FVio.LastErrorCode)
        else
          raise EScError.Create(seSocketClosed);
      end;
      FDataPacket.WriteOffset := FDataPacket.WriteOffset + len;
      Dec(Count, len);
    end;
  end
  else
    if not FDataPacket.HasUnreadData then begin
      FDataPacket.CheckAndRealloc(4096);
      len := FVio.ReadNoWait(TValueArr(FDataPacket.Data), FDataPacket.WriteOffset, Length(FDataPacket.Data) - FDataPacket.WriteOffset);
      if len <= 0 then begin
        if FVio.LastError <> '' then
          raise SocketException.Create(FVio.LastError, FVio.LastErrorCode)
        else
          raise EScError.Create(seSocketClosed);
      end;
      FDataPacket.WriteOffset := FDataPacket.WriteOffset + len;
    end;

  Result := FDataPacket;
end;

procedure TPlainSocket.RepeatAsyncRead;
begin
  FDataPacket.CheckAndRealloc(4096);
  FVio.BeginReceive(FDataPacket.Data, FDataPacket.WriteOffset, Length(FDataPacket.Data) - FDataPacket.WriteOffset, RepeatCallback, nil);
end;

procedure TPlainSocket.RepeatCallback(Result: IScAsyncResult);
var
  NeedBytes: Integer;
  Count: Integer;
begin
  lock(FLock);
  try
    try
      Count := FVio.EndReceive(Result);

      if (Count > 0) and (FHandler <> nil) then begin
        FDataPacket.WriteOffset := FDataPacket.WriteOffset + Count;
        FHandler.OnData(FDataPacket, NeedBytes);

        while NeedBytes > 0 do begin
          if NeedBytes > SSH_MAX_PACKET_SIZE then
            raise EScError.CreateFmt(SInvalidPacketSize, [NeedBytes], seInvalidPacketSize);

          FDataPacket.CheckAndRealloc(NeedBytes);

          while NeedBytes > 0 do begin
            Count := FVio.ReadNoWait(TValueArr(FDataPacket.Data), FDataPacket.WriteOffset, NeedBytes);
            if Count <= 0 then
              raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
            FDataPacket.WriteOffset := FDataPacket.WriteOffset + Count;
            Dec(NeedBytes, Count);
          end;

          FHandler.OnData(FDataPacket, NeedBytes);
        end;

        if Closed then begin
          Close;
          Exit;
        end;

        RepeatAsyncRead;
      end
      else
        Close;

    except
      on E: Exception do begin
        try
          if not FClosed and not (E is SocketException) then
            if FHandler <> nil then
              FHandler.OnError(E);
        finally
          Close;
        end;
      end;
    end;

  finally
    unlock(FLock);
  end;
end;

end.

