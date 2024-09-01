
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScClient;

interface

uses
  {$IFDEF VER16P}System.Types,{$ELSE}Types,{$ENDIF}
  Classes, SysUtils, SyncObjs,
  ScCLRClasses, ScTypes, ScFunctions, ScUtils, ScSSHUtils, ScVio,
  ScSSHConnectionParameter, ScSSHSocket, ScReceiveBuffer, ScDataHandler;

type
  TSshConnection = class;

  TSshChannel = class
  private
    FTimeout: Integer;
    FIsEOF: Boolean;
    FevReady: TEvent;
    FReady: Boolean;
    FErrorMsg: string;

    function GetDataLength: Integer;
    procedure ClearConnection;

  protected
    FType: TScChannelType;
    FLocalID: Integer;
    FRemoteID: Integer;
    FClosed: Boolean;
    FConnection: TSshConnection;
    FLockConnection: TCriticalSection;
    FReceiveBuffer: TReceiveBuffer; // buffer for Data recieved from socket
    function GetClosed: Boolean;
    procedure SetEOF;

  public
    constructor Create(con: TSshConnection; chType: TScChannelType);
    destructor Destroy; override;
    procedure Dispose;

    function Read(const Data: TValueArr; Offset, Count: Integer): Integer; overload;
    function Read(Stream: TStream): Integer; overload;
    // transmits channel Data
    function Transmit(const Data: TValueArr; Offset, Length: integer): Integer; virtual; abstract;
    // sends Close and closes this channel
    procedure Close; virtual; abstract;

    procedure SendWindowAdjust; virtual; abstract;
    function Readable(DataLen: Integer; MillisecondsTimeout: Integer): boolean;
    procedure SetReady;
    function WaitForReady(MillisecondsTimeout: Integer): boolean;
    procedure OnError(const msg: string);
    procedure AfterChannelClosed; virtual; abstract;

    property DataLength: Integer read GetDataLength;
    property LocalChannelID: Integer read FLocalID;
    property RemoteChannelID: Integer read FRemoteID;
    property ChannelType: TScChannelType read FType;
    property IsClosed: Boolean read GetClosed;
    property Timeout: Integer read FTimeout write FTimeout;
    property IsEOF: Boolean read FIsEOF;
    property Ready: Boolean read FReady;
    property LastErrorMessage: string read FErrorMsg;
  end;

  // SSHConnection abstract class introduces common behaviour of connection to SSH server
  TSshConnection = class
  private
    FChannelList: TStringList;
    FChannelSequence: Integer;
    // Returns the channel sequence number
    function GetChannelSequence: Integer;
    function GetClosed: Boolean;
    function VerifyVersion(const Version: string): string;

  protected
    FDisposed: boolean;
    FLockChannel: TCriticalSection;
    FLockChannelWaitCount: integer;
    FClosed: Boolean;
    FSessionID: TBytes;
    FStream: TPlainSocket;
    FParams: TSshConnectionParameters;

    // register new channel in the connection
    function RegisterChannel(ch: TSshChannel): Integer;
    // unregister channel from connection
    procedure UnregisterChannel(ID: Integer; Close: boolean = False);
    // Return channel from the storage by given id
    function FindChannel(ID: Integer): TSshChannel;
    procedure UnregisterAllChannels;
    procedure SendWindowAdjustForAllChannels;

    procedure VersionNegotiation; virtual; abstract;
    function ReadVersion: string;
    procedure SendVersion(Version: string);

    function ReadData(Count: integer = 0): TDataPacket; virtual; abstract;
    procedure Close; virtual; abstract;

  public
    constructor Create(AParams: TSshConnectionParameters); virtual;
    destructor Destroy; override;
    procedure Dispose; virtual;

    procedure Connect(vio: TCRVio); virtual; abstract;
    procedure Disconnect(const msg: string); virtual; abstract;

    procedure SendIgnorableData(const msg: string); overload; virtual; abstract;
    procedure SendIgnorableData(const msg: TValueArr; offset, length: Integer); overload; virtual; abstract;

    property Params: TSshConnectionParameters read FParams;
    property IsClosed: Boolean read GetClosed;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  StrUtils,
  ScConsts;

{ TSshChannel }

constructor TSshChannel.Create(con: TSshConnection; chType: TScChannelType);
begin
  inherited Create;

  Assert(con <> nil);

  FConnection := con;
  FType := chType;
  FClosed := False;
  FIsEOF := False;
  FReady := False;
  FLockConnection := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;
  FReceiveBuffer := TReceiveBuffer.Create(RECV_CHUNK_SIZE);
  FevReady := ScFunctions.CreateEvent;
  FLocalID := con.RegisterChannel(Self);
end;

destructor TSshChannel.Destroy;
begin
  FLockConnection.Free;
  FevReady.Free;
  FReceiveBuffer.Free;
  inherited;
end;

procedure TSshChannel.Dispose;
var
  Con: TSshConnection;
begin
  lock(FLockConnection);
  try
    Con := FConnection;
    if Con <> nil then
      InterlockedIncrement(Con.FLockChannelWaitCount);
  finally
    unlock(FLockConnection);
  end;

  try
    if Con <> nil then
      Con.UnregisterChannel(FlocalID);
  finally
    if Con <> nil then
      InterlockedDecrement(Con.FLockChannelWaitCount);
  end;
end;

procedure TSshChannel.ClearConnection;
begin
  lock(FLockConnection);
  try
    FConnection := nil;
  finally
    unlock(FLockConnection);
  end;
end;

procedure TSshChannel.SetEOF;
begin
  FIsEOF := True;
  FReceiveBuffer.CloseBuffer;
end;

procedure TSshChannel.SetReady;
begin
  FErrorMsg := '';
  FReady := True;
  FevReady.SetEvent;
end;

procedure TSshChannel.OnError(const msg: string);
begin
  FErrorMsg := msg;
  FReady := False;
  FevReady.SetEvent;
end;

function TSshChannel.WaitForReady(MillisecondsTimeout: Integer): boolean;
begin
  Result := FevReady.WaitFor(cardinal(MillisecondsTimeout)) = wrSignaled;
end;

function TSshChannel.GetClosed: Boolean;
begin
  lock(FLockConnection);
  try
    Result := FClosed or (FConnection = nil) or FConnection.IsClosed;
  finally
    unlock(FLockConnection);
  end;
end;

function TSshChannel.Read(const Data: TValueArr; Offset, Count: Integer): Integer;
begin
  Result := FReceiveBuffer.Read(Data, Offset, Count);
end;

function TSshChannel.Read(Stream: TStream): Integer;
begin
  Result := FReceiveBuffer.Read(Stream);
end;

function TSshChannel.Readable(DataLen: Integer; MillisecondsTimeout: Integer): boolean;
begin
  Result := FReceiveBuffer.WaitForData(DataLen, cardinal(MillisecondsTimeout));
end;

function TSshChannel.GetDataLength: Integer;
begin
  Result := FReceiveBuffer.DataLength;
end;

{ TSshConnection }

constructor TSshConnection.Create(AParams: TSshConnectionParameters);
begin
  inherited Create;

  FLockChannel := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;
  FChannelList := TStringList.Create;
  FChannelList.Duplicates := dupError;
  FChannelList.Sorted := True;

  FParams := TSshConnectionParameters.Create;
  FParams.Assign(AParams);

  FChannelSequence := 0;
  FClosed := True;
end;

destructor TSshConnection.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TSshConnection.Dispose;
begin
  lock(FLockChannel);
  FClosed := True;
  unlock(FLockChannel);

  UnregisterAllChannels;

  while FLockChannelWaitCount > 0 do begin
    lock(FLockChannel);
    unlock(FLockChannel);
    sleep(10);
  end;

  FreeAndNil(FLockChannel);
  FreeAndNil(FChannelList);
  FreeAndNil(FParams);
end;

function TSshConnection.GetClosed: Boolean;
begin
  if FStream = nil then
    Result := True
  else
    Result := FClosed or FStream.Closed;
end;

function TSshConnection.GetChannelSequence: Integer;
begin
  Inc(FChannelSequence);
  Result := FChannelSequence;
end;

function TSshConnection.RegisterChannel(ch: TSshChannel): Integer;
var
  ID: Integer;
begin
  lock(FLockChannel);
  try
    if FClosed then begin
      Result := 0;
      Exit;
    end;

    ID := GetChannelSequence;
    FChannelList.AddObject(IntToStr(ID), ch);
  finally
    unlock(FLockChannel);
  end;

  Result := ID;
end;

function TSshConnection.FindChannel(ID: Integer): TSshChannel;
var
  i: Integer;
begin
  Result := nil;

  lock(FLockChannel);
  try
    if FClosed then
      Exit;

    i := FChannelList.IndexOf(IntToStr(ID));
    if i > -1 then
      Result := TSshChannel(FChannelList.Objects[i]);
  finally
    unlock(FLockChannel);
  end;
end;

procedure TSshConnection.UnregisterChannel(ID: Integer; Close: boolean = False);
var
  Channel: TSshChannel;
  i: Integer;
begin
  lock(FLockChannel);
  try
    i := FChannelList.IndexOf(IntToStr(ID));
    if i >= 0 then begin
      Channel := TSshChannel(FChannelList.Objects[i]);

      try
        if Close then
          try
            Channel.Close;
          except
            on E: Exception do
              if E.ClassName <> SocketException.ClassName then
                raise;
          end;

      finally
        Channel.AfterChannelClosed;
        Channel.ClearConnection;

        FChannelList.Delete(i);
      end;
    end;
  finally
    unlock(FLockChannel);
  end;
end;

procedure TSshConnection.UnregisterAllChannels;
begin
  lock(FLockChannel);
  try
    while FChannelList.Count > 0 do
      UnregisterChannel(StrToInt(FChannelList[0]), True);
  finally
    unlock(FLockChannel);
  end;
end;

procedure TSshConnection.SendWindowAdjustForAllChannels;
var
  Channel: TSshChannel;
  i: Integer;
begin
  lock(FLockChannel);
  try
    if FClosed then
      Exit;

    for i := 0 to FChannelList.Count - 1 do begin
      Channel := TSshChannel(FChannelList.Objects[i]);
      Assert(Channel <> nil);
      Channel.SendWindowAdjust;
    end;
  finally
    unlock(FLockChannel);
  end;
end;

function TSshConnection.VerifyVersion(const Version: string): string;
var
  a, b, Comma: integer;
  Len: integer;
  Major, Minor: integer;
begin
//  if RightStr(Vers, 2) = #$0D#$0A then
//    FEndOfLine := #$0D#$0A
//  else
//    FEndOfLine := #$0A;

  Len := Length(Version);
  if (Version[Len] = #$0A) and (Version[Len - 1] = #$0D) then
    Result := Copy(Version, 1, Len - 2)
  else
  if Version[Len] = #$0A then
    Result := Copy(Version, 1, Len - 1)
  else
    Result := Version;

  //check compatibility
  a := Pos('-', Result);
  if a = 0 then
    raise EScError.CreateFmt(SInvalidServerVersion, [Result], seInvalidServerVersion);

  Inc(a);
  b := PosEx('-', Result, a);
  if b = 0 then
    raise EScError.CreateFmt(SInvalidServerVersion, [Result], seInvalidServerVersion);

  Comma := Pos('.', Copy(Result, a, b - a));
  if Comma = 0 then
    raise EScError.CreateFmt(SInvalidServerVersion, [Result], seInvalidServerVersion);

  Major := StrToInt(Copy(Result, a, Comma - 1));
  Minor := StrToInt(Copy(Result, a + Comma, b - a - Comma));

  if FParams.Protocol = pSSH1 then begin
    if Major <> 1 then
      raise EScError.CreateFmt(SNotCompatibleServerVersion, [1], seNotCompatibleServerVersion);
  end
  else begin
    if (Major > 2) or (Major <= 0) or ((Major = 1) and (Minor <> 99)) then
      raise EScError.CreateFmt(SNotCompatibleServerVersion, [2], seNotCompatibleServerVersion);
  end;
end;

function TSshConnection.ReadVersion: string;
var
  DataPacket: TDataPacket;
  StartReadOffset: integer;
  s: string;
  Len: integer;
begin
  DataPacket := nil;
  StartReadOffset := -1;

  repeat
    if DataPacket <> nil then
      DataPacket.ReadOffset := DataPacket.WriteOffset;

    DataPacket := ReadData;
    if StartReadOffset = -1 then
      StartReadOffset := DataPacket.ReadOffset;

    s := Encoding.ANSI.GetString(DataPacket.Data, StartReadOffset, DataPacket.WriteOffset - StartReadOffset);
    Len := Pos(#$0A, s);
  until (Len > 0) or (DataPacket.WriteOffset > 32*1024);

  s := Copy(s, 1, Len);
  Result := VerifyVersion(s);

  DataPacket.ReadOffset := StartReadOffset + Len;
  DataPacket.ReduceBuffer;
end;

procedure TSshConnection.SendVersion(Version: string);
var
  Data: TBytes;
begin
  if FParams.Protocol = pSSH1 then
    Version := Version + #10
  else
    Version := Version + #13#10;

  Data := Encoding.Default.GetBytes(Version); //ASCII
  FStream.Write(Data, 0, Length(Data));
end;

end.

