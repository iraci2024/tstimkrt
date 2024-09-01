
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSH2Channel;

interface

uses
  SysUtils, SyncObjs, Classes,
  ScCLRClasses, ScTypes, ScSSHUtils, ScClient, ScReaderWriter, ScBridge, ScSSHConsts;

type
  TScGetExitCodeProc = procedure(out ExitCode: cardinal) of object;
  TScWaitProc = procedure of object;

  TSsh2Channel = class(TSshChannel)
  private
    FInitRecvWindowSize: Int64;
    FRecvWindowSize: Int64;
    FSendWindowSize: cardinal;
    FSendMaxPacketSize: cardinal;
    FTransmitStream: TSSH2DataStream; // serialize data into network stream
    FWindowAdjustStream: TSSH2DataStream;
    FNewWindowAdjust: TEvent;
    FLockSendData: TCriticalSection;
    FLockReadData: TCriticalSection;
    FTerminalInfo: TScTerminalInfo;
    FExitStatusCode: integer;
    FOnDisconnect: TNotifyEvent;
    FOnWaitForClose: TScWaitProc;
    FOnGetExitCode: TScGetExitCodeProc;

    function GetOnReceiveData: TNotifyEvent;
    procedure SetOnReceiveData(Value: TNotifyEvent);
    procedure TransmitPacket(Stream: TSSH2DataStream);
    procedure ProcessChannelOpenResponse(pt: PacketTypeSSH2; Reader: TSSH2DataReader);

  public
    constructor Create(con: TSshConnection; chType: TScChannelType;
      RemoteID: Integer = -1; InitWindowSize: cardinal = 0; MaxPacketSize: cardinal = 0);
    destructor Destroy; override;

    function Transmit(const Data: TValueArr; Offset, Length: integer): Integer; override;
    procedure SendEOF;
    procedure Close; override;
    procedure SendEnvironmentVariable(const Name: string; const Value: string);
    procedure SendBreak(Time: Integer);
    procedure SendAlive;
    procedure SendTerminalReq(TerminalInfo: TScTerminalInfo);
    procedure SendWindowChangeReq(TerminalInfo: TScTerminalInfo);
    procedure SendShell;
    procedure SendExec(const Command: string);
    procedure SendSubsystem(const Subsystem: string);
    procedure SendExitStatus(Status: Integer);
    procedure SendWindowAdjust; override;
    procedure ProcessPacket(pt: PacketTypeSSH2; DataLength: Integer; re: TSSH2DataReader);
    procedure AfterChannelClosed; override;

    function GetRemainedWindowSize: integer;

    property ExitStatusCode: integer read FExitStatusCode;
    property OnReceiveData: TNotifyEvent read GetOnReceiveData write SetOnReceiveData;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnWaitForClose: TScWaitProc read FOnWaitForClose write FOnWaitForClose;
    property OnGetExitCode: TScGetExitCodeProc read FOnGetExitCode write FOnGetExitCode;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Math, ScUtils, ScConsts, ScVio, ScFunctions, ScSSH2Connection;

const
  HALF_PACKET_SIZE = 16 * 1024;

{ TSsh2Channel }

constructor TSsh2Channel.Create(con: TSshConnection; chType: TScChannelType;
  RemoteID: Integer = -1; InitWindowSize: cardinal = 0; MaxPacketSize: cardinal = 0);
begin
  inherited Create(con, chType);

  FRecvWindowSize := con.Params.WindowSize;
  FInitRecvWindowSize := con.Params.WindowSize;
  FSendWindowSize := InitWindowSize;
  FSendMaxPacketSize := MaxPacketSize;
  FRemoteID := RemoteID;

  FNewWindowAdjust := CreateEvent;
  FLockSendData := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;
  FLockReadData := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;
  FTransmitStream := TSSH2DataStream.Create;
  FWindowAdjustStream := TSSH2DataStream.Create(32);
  FTerminalInfo := TScTerminalInfo.Create;
end;

destructor TSsh2Channel.Destroy;
begin
  Dispose;

  FTransmitStream.Free;
  FWindowAdjustStream.Free;
  FNewWindowAdjust.Free;
  FTerminalInfo.Free;
  FLockSendData.Free;
  FLockReadData.Free;

  inherited;
end;

function TSsh2Channel.Transmit(const Data: TValueArr; Offset, Length: integer): Integer;
var
  SendSize: cardinal;
begin
  Result := 0;
  if IsClosed then
    Exit;

  while Length > Result do begin
    while FSendWindowSize = 0 do begin
      if FClosed then
        Exit;
      if FNewWindowAdjust.WaitFor(cardinal(Timeout)) <> wrSignaled then
        raise EScError.Create(seTimeoutSession);
      FNewWindowAdjust.ResetEvent;
    end;

    SendSize := Min(Min(FSendMaxPacketSize, FSendWindowSize), Cardinal(Length - Result));
    FTransmitStream.WritePacketType(SSH_MSG_CHANNEL_DATA);
    FTransmitStream.WriteInt32(FRemoteID);
    FTransmitStream.WriteAsString(Data, Offset + Result, integer(SendSize));
    try
      TransmitPacket(FTransmitStream);
      Result := Result + integer(SendSize);
    except
      on SocketException do
        Exit;
      else
        raise;
    end;

    lock(FLockSendData);
    Dec(FSendWindowSize, SendSize);
    unlock(FLockSendData);
  end;
end;

procedure TSsh2Channel.Close;
var
  Stream: TSSH2DataStream;
begin
  lock(FLockSendData);
  try
    try
      if FClosed then
        Exit;

      if not IsClosed and Ready then begin
        Stream := TSSH2DataStream.Create(16);
        try
          Stream.WritePacketType(SSH_MSG_CHANNEL_CLOSE);
          Stream.WriteInt32(FRemoteID);
          TransmitPacket(Stream);
        finally
          Stream.Free;
        end;
      end;
    finally
      FClosed := True;
      SetEOF;
      FNewWindowAdjust.SetEvent;
    end;
  finally
    unlock(FLockSendData);
  end;
end;

procedure TSsh2Channel.SendEOF;
var
  Stream: TSSH2DataStream;
begin
  if IsClosed then
    Exit;

  Stream := TSSH2DataStream.Create(16);
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_EOF);
    Stream.WriteInt32(FRemoteID);
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendEnvironmentVariable(const Name: string; const Value: string);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('env');
    Stream.WriteBool(False);
    Stream.WriteWStr(WideString(Name));
    Stream.WriteWStr(WideString(Value));
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendBreak(Time: Integer);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('break');
    Stream.WriteBool(True);
    Stream.WriteInt32(Time);
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendAlive;
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr(SKeepaliveCom);
    Stream.WriteBool(True);
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendTerminalReq(TerminalInfo: TScTerminalInfo);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('pty-req');
    Stream.WriteBool(False);
    Stream.WriteAStr(TerminalInfo.TerminalType);
    Stream.WriteInt32(TerminalInfo.Cols);
    Stream.WriteInt32(TerminalInfo.Rows);
    Stream.WriteInt32(TerminalInfo.Width);
    Stream.WriteInt32(TerminalInfo.Height);
    Stream.WriteAStr(''); //encoded terminal modes
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendWindowChangeReq(TerminalInfo: TScTerminalInfo);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('window-change');
    Stream.WriteBool(False);
    Stream.WriteInt32(TerminalInfo.Cols);
    Stream.WriteInt32(TerminalInfo.Rows);
    Stream.WriteInt32(TerminalInfo.Width);
    Stream.WriteInt32(TerminalInfo.Height);
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendShell;
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('shell');
    Stream.WriteBool(False);
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendExec(const Command: string);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('exec');
    Stream.WriteBool(False);
    Stream.WriteWStr(WideString(Command));
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendSubsystem(const Subsystem: string);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('subsystem');
    Stream.WriteBool(True);
    Stream.WriteAStr(Subsystem);
    TransmitPacket(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendExitStatus(Status: Integer);
var
  Stream: TSSH2DataStream;
begin
  Stream := TSSH2DataStream.Create;
  try
    Stream.WritePacketType(SSH_MSG_CHANNEL_REQUEST);
    Stream.WriteInt32(FRemoteID);
    Stream.WriteAStr('exit-status');
    Stream.WriteBool(False);
    Stream.WriteInt32(Status);
    TransmitPacket(Stream);

    if FExitStatusCode = 0 then
      FExitStatusCode := Status;
  finally
    Stream.Free;
  end;
end;

procedure TSsh2Channel.SendWindowAdjust;
begin
  lock(FLockConnection);
  try
    if (FConnection <> nil) and TSsh2Connection(FConnection).ReexchangingKeys then
      Exit;
  finally
    unlock(FLockConnection);
  end;

  while FRecvWindowSize <= HALF_PACKET_SIZE do begin
    FWindowAdjustStream.WritePacketType(SSH_MSG_CHANNEL_WINDOW_ADJUST);
    FWindowAdjustStream.WriteInt32(FRemoteID);
    FWindowAdjustStream.WriteInt32(integer(FInitRecvWindowSize));
    TransmitPacket(FWindowAdjustStream);
    FRecvWindowSize := FRecvWindowSize + FInitRecvWindowSize;
  end;
end;

function TSsh2Channel.GetRemainedWindowSize: integer;
begin
  lock(FLockReadData);
  Result := FRecvWindowSize;
  unlock(FLockReadData);
end;

procedure TSsh2Channel.TransmitPacket(Stream: TSSH2DataStream);
begin
  lock(FLockConnection);
  try
    if FConnection <> nil then
      TSsh2Connection(FConnection).TransmitPacket(Stream.Data, 0, Stream.DataLength)
    else
      raise EScError.Create(seConnectionNotDefined);
  finally
    unlock(FLockConnection);
    Stream.Clear;
  end;
end;

procedure TSsh2Channel.ProcessPacket(pt: PacketTypeSSH2; DataLength: Integer; re: TSSH2DataReader);
var
  NeedToClose: Boolean;

  procedure CheckAllowedShell(const Request: string);
  var
    Allowed: Boolean;
  begin
    lock(FLockConnection);
    try
      Allowed := False;
      if (FConnection <> nil) and (FConnection.Params <> nil) then
        Allowed := FConnection.Params.AllowedShell;

      if not Allowed then
        raise EScError.CreateFmt(SUnsupportedRequestType, [Request], seUnsupportedRequestType)
      else
      if not (cpAllowShell in FConnection.Params.ConnectionInfo.SSHChannelPermissions) then begin
        NeedToClose := True;
        raise EScError.Create(seChannelFailedAdministrativelyProhibited);
      end;
    finally
      unlock(FLockConnection);
    end;
  end;

var
  wr: TSSH2DataStream;
  len: Integer;
  Request, Subsystem: string;
  Reply: Boolean;
  Response: Byte;
  Command: string;
{$IFDEF MSWINDOWS}
  EnvName, EnvValue: string;
{$ENDIF}
  ExitCode: cardinal;
begin
  //NOTE: the offset of 're' is next to 'receipiant channel' field

  if not Ready then begin //when the negotiation is not completed
    ProcessChannelOpenResponse(pt, re);
    Exit;
  end;

  case pt of
    SSH_MSG_CHANNEL_WINDOW_ADJUST: begin
      lock(FLockSendData);
      try
        Inc(FSendWindowSize, re.ReadUInt32);
      finally
        unlock(FLockSendData);
      end;
      FNewWindowAdjust.SetEvent;
    end;

    SSH_MSG_CHANNEL_DATA: begin
      lock(FLockReadData);
      try
        len := re.ReadInt32;
        FReceiveBuffer.Write(TValueArr(re.Image), re.Offset, len);

        FRecvWindowSize := FRecvWindowSize - len;
        if FRecvWindowSize <= HALF_PACKET_SIZE then
          SendWindowAdjust;
      finally
        unlock(FLockReadData);
      end;
    end;

    SSH_MSG_CHANNEL_EXTENDED_DATA: begin
      // OnExtendedData(re.ReadInt32, re.ReadString);
    end;

    SSH_MSG_CHANNEL_REQUEST: begin
      Request := LowerCase(Encoding.Default.GetString(re.ReadString));
      Reply := re.ReadBool;
      Response := SSH_MSG_CHANNEL_FAILURE;
      NeedToClose := False;

      try
        if Request = 'env' then begin
          CheckAllowedShell(Request);

        {$IFDEF MSWINDOWS}
          EnvName := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(re.ReadString));
          EnvValue := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(re.ReadString));
          if SetEnvironmentVariable(PChar(EnvName), PChar(EnvValue)) then
            Response := SSH_MSG_CHANNEL_SUCCESS;
        {$ENDIF}
        end
        else if Request = 'pty-req' then begin
          CheckAllowedShell(Request);

          FTerminalInfo.TerminalType := Encoding.Default.GetString(re.ReadString);
          FTerminalInfo.Cols := re.ReadInt32;
          FTerminalInfo.Rows := re.ReadInt32;
          FTerminalInfo.Width := re.ReadInt32;
          FTerminalInfo.Height := re.ReadInt32;
          // re.ReadString; //encoded terminal modes
          Response := SSH_MSG_CHANNEL_SUCCESS;
        end
        else if Request = 'exit-status' then begin
          FExitStatusCode := re.ReadInt32; //status
        end
        else if Copy(Request, 1, Length(SKeepalive)) = SKeepalive then begin
          Response := SSH_MSG_CHANNEL_SUCCESS;
        end
        else if Request = SSimplePuttyTartarus then begin
          Response := SSH_MSG_CHANNEL_SUCCESS;
        end
        else if ChannelType = ctSession then begin
          Command := '';

          if Request = 'shell' then begin
            CheckAllowedShell(Request);
            Command := '';
          end
          else if Request = 'exec' then begin
            CheckAllowedShell(Request);
            Command := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(re.ReadString));
          end
          else if Request = 'subsystem' then begin
            Subsystem := Encoding.Default.GetString(re.ReadString);
            if Subsystem <> 'sftp' then
              raise EScError.CreateFmt(SUnsupportedSessionType, [Request + ' : ' + Subsystem], seUnsupportedSessionType);

            if not (cpAllowSFTP in FConnection.Params.ConnectionInfo.SSHChannelPermissions) then begin
              NeedToClose := True;
              raise EScError.Create(seChannelFailedAdministrativelyProhibited);
            end;

            Request := Subsystem;
          end
          else
            raise EScError.CreateFmt(SUnsupportedSessionType, [Request], seUnsupportedSessionType);

          try
            lock(FLockConnection);
            try
              if FConnection <> nil then
                if Assigned(FConnection.Params.OnOpenSession) then
                  FConnection.Params.OnOpenSession(Self, FTerminalInfo, Request, Command)
                else
                  raise EScError.Create(seUnexpectedPacketType);
            finally
              unlock(FLockConnection);
            end;

            Response := SSH_MSG_CHANNEL_SUCCESS;
          except
            NeedToClose := True;
          end;
        end;
      finally
        if Reply then begin //we reject unknown requests
          wr := TSSH2DataStream.Create;
        {$IFDEF MSWINDOWS}
          try
        {$ENDIF}
            wr.WriteByte(Response);
            wr.WriteInt32(FRemoteID);
            TransmitPacket(wr);
        {$IFDEF MSWINDOWS}
          finally
        {$ENDIF}
            wr.Free;
        {$IFDEF MSWINDOWS}
          end;
        {$ENDIF}
        end;

        if NeedToClose then
          Close;
      end;
    end;

    SSH_MSG_CHANNEL_EOF: begin
      if ChannelType = ctSession then begin
        if Assigned(OnWaitForClose) then
          OnWaitForClose;
        if Assigned(OnGetExitCode) then
          OnGetExitCode(ExitCode)
        else
          ExitCode := 0;

        SendExitStatus(integer(ExitCode));
      end;
      Close;
    end;

    SSH_MSG_CHANNEL_CLOSE: begin
      Close;
      Dispose;
    end;

    SSH_MSG_CHANNEL_FAILURE: begin
      // OnChannelSuccess(byte(pt), re.Image, re.Offset, re.Rest);
      if ChannelType = ctSession then
        Close;
    end;

    SSH_MSG_CHANNEL_SUCCESS: begin
      // OnChannelSuccess(byte(pt), re.Image, re.Offset, re.Rest);
    end;
  else
    // if pt <> 0 then
    //   OnMiscPacket(byte(pt), re.Image, re.Offset, re.Rest);
  end;
end;

procedure TSsh2Channel.ProcessChannelOpenResponse(
  pt: PacketTypeSSH2; Reader: TSSH2DataReader);
var
  ErrorMessage: string;
  ErrorCode: Integer;
begin
  if not Ready then begin
    if pt <> SSH_MSG_CHANNEL_OPEN_CONFIRMATION then begin
      if pt <> SSH_MSG_CHANNEL_OPEN_FAILURE then
        OnError(Format(SChannelFailed, [Integer(pt)]))
      else begin
        ErrorCode := Reader.ReadInt32;
        ErrorMessage := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString));
        if ErrorMessage = '' then begin
          case ErrorCode of
            SSH_OPEN_ADMINISTRATIVELY_PROHIBITED:
              ErrorMessage := SChannelFailedAdministrativelyProhibited;
            SSH_OPEN_CONNECT_FAILED:
              ErrorMessage := SChannelFailedConnectionFailed;
            SSH_OPEN_UNKNOWN_CHANNEL_TYPE:
              ErrorMessage := SChannelFailedUnknownChannelType;
          else
            ErrorMessage := Format(SChannelError, [ErrorCode]);
          end;
        end;
        OnError(ErrorMessage);
      end;
      Close;
    end
    else begin
      FRemoteID := Reader.ReadInt32;
      FSendWindowSize := Reader.ReadUInt32;
      FSendMaxPacketSize := Reader.ReadUInt32;

      SetReady;
      FNewWindowAdjust.SetEvent;
    end;
  end;
end;

procedure TSsh2Channel.AfterChannelClosed;
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(Self);
end;

function TSsh2Channel.GetOnReceiveData: TNotifyEvent;
begin
  Result := FReceiveBuffer.OnReceiveData;
end;

procedure TSsh2Channel.SetOnReceiveData(Value: TNotifyEvent);
begin
  FReceiveBuffer.OnReceiveData := Value;
end;

end.

