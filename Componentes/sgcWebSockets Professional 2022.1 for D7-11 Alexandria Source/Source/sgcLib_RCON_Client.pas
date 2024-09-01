{ ***************************************************************************
  sgcLib RCON component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }


unit sgcLib_RCON_Client;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Classes, sgcBase_Helpers, sgcTCP_Client, sgcTCP_Classes, sgcWebSocket_Types;

type

  TsgcRCON_Packet = class;

  TsgcRCONAuthenticateEvent = procedure(Sender: TObject; Authenticated: Boolean;
      const aPacket: TsgcRCON_Packet) of object;
  TsgcRCONResponseEvent = procedure(Sender: TObject; const aResponse: string; const aPacket: TsgcRCON_Packet) of object;
  TsgcRCONExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  TsgcRCON_Options = class(TPersistent)
  private
    FHost: string;
    FPassword: string;
    FPort: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Host: string read FHost write FHost;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
  end;

  TsgcRCON_Packet = class
  private
    FBody: string;
    FID: Integer;
    FPacketSize: Integer;
    FPacketType: Integer;
    function GetEmptyString: Char;
  private
    procedure DoAddBytes(Value: TBytes; var Bytes: TBytes);
    procedure DoAddByte(Value: Byte; var Bytes: TBytes);
    procedure DoAddInteger(Value: Integer; var Bytes: TBytes);
  private
    function DoReadIntegerFromBytes(const aBytes: TBytes; var aOffset: Integer):
        Integer;
    function DoReadStringFromBytes(const aBytes: TBytes; aLength: Integer; var
        aOffset: Integer): String;
  public
    function WriteBytes: TBytes;
    procedure ReadBytes(const aBytes: TBytes; var aOffset: Integer);
  public
    property Body: string read FBody write FBody;
    property ID: Integer read FID write FID;
    property PacketSize: Integer read FPacketSize;
    property PacketType: Integer read FPacketType write FPacketType;
    property EmptyString: Char read GetEmptyString;
  end;

  TsgcLib_RCON_Client = class(TsgcComponent_Base)
    { properties }
  private
    FRCON_Options: TsgcRCON_Options;
    procedure SetRCON_Options(const Value: TsgcRCON_Options);
    function GetNotifyEvents: TwsNotifyEvent;
    procedure SetNotifyEvents(const Value: TwsNotifyEvent);
  public
    property RCON_Options: TsgcRCON_Options read FRCON_Options write
        SetRCON_Options;
    property NotifyEvents: TwsNotifyEvent read GetNotifyEvents write
        SetNotifyEvents;
    { properties }

    { client }
  private
    FBuffer: TBytes;
    FTCPClient: TsgcTCPCLient;
    function GetTCPClient: TsgcTCPCLient;
  protected
    procedure OnConnectEvent(aConnection: TsgcTCPConnection); virtual;
    procedure OnDisconnectEvent(aConnection: TsgcTCPConnection); virtual;
    procedure OnBinaryEvent(Connection: TsgcTCPConnection; const Data:
        TMemoryStream); virtual;
    procedure OnExceptionEvent(Connection: TsgcTCPConnection; E: Exception);
        virtual;
  protected
    property TCPClient: TsgcTCPCLient read GetTCPClient write FTCPClient;
    { client }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { active }
  private
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    property Active: Boolean read GetActive write SetActive;
    { active }

    { methods }
  private
    FID: Integer;
    function GetNewID: Integer;
  protected
    procedure DoRCON_SendMessage(aMethod: Integer; const aBody: string; aId:
        Integer); virtual;
  protected
    procedure DoRCON_Authenticate; virtual;
    procedure DoRCON_ExecCommand(const aValue: string; aId: Integer);
  public
    procedure ExecCommand(const aValue: string; const aId: Integer = 0);
    { methods }

    { events }
  private
    FOnAuthenticate: TsgcRCONAuthenticateEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnException: TsgcRCONExceptionEvent;
    FOnResponse: TsgcRCONResponseEvent;
  public
    property OnAuthenticate: TsgcRCONAuthenticateEvent read FOnAuthenticate write
        FOnAuthenticate;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnException: TsgcRCONExceptionEvent read FOnException write
        FOnException;
    property OnResponse: TsgcRCONResponseEvent read FOnResponse write FOnResponse;
    { events }
  end;

implementation

uses
  sgcWebSocket_Helpers;

const
  CS_RCON_SERVERDATA_AUTH = 3;
  CS_RCON_SERVERDATA_AUTH_RESPONSE = 2;
  CS_RCON_SERVERDATA_EXECCOMMAND = 2;
  CS_RCON_SERVERDATA_RESPONSE_VALUE = 0;

constructor TsgcLib_RCON_Client.Create(aOwner: TComponent);
begin
  inherited;
  FID := 0;
  FRCON_Options := TsgcRCON_Options.Create;
end;

destructor TsgcLib_RCON_Client.Destroy;
begin
  sgcFree(FTCPClient);
  inherited;
end;

procedure TsgcLib_RCON_Client.DoRCON_Authenticate;
begin
  DoRCON_SendMessage(CS_RCON_SERVERDATA_AUTH, RCON_Options.Password, 0);
end;

procedure TsgcLib_RCON_Client.DoRCON_ExecCommand(const aValue: string; aId:
    Integer);
begin
  DoRCON_SendMessage(CS_RCON_SERVERDATA_EXECCOMMAND, aValue, aId);
end;

procedure TsgcLib_RCON_Client.DoRCON_SendMessage(aMethod: Integer; const aBody:
    string; aId: Integer);
var
  oPacket: TsgcRCON_Packet;
begin
  oPacket := TsgcRCON_Packet.Create;
  Try
    if aId > 0 then
      oPacket.ID := aId
    else
      oPacket.ID := GetNewID;
    oPacket.PacketType := aMethod;
    oPacket.Body := aBody;

    TCPClient.WriteData(oPacket.WriteBytes);
  Finally
    sgcFree(oPacket);
  End;
end;

procedure TsgcLib_RCON_Client.ExecCommand(const aValue: string; const aId:
    Integer = 0);
begin
  DoRCON_ExecCommand(aValue, aId);
end;

function TsgcLib_RCON_Client.GetActive: Boolean;
begin
  Result := TCPClient.Active;
end;

function TsgcLib_RCON_Client.GetNewID: Integer;
begin
  Result := FID + 1;
end;

function TsgcLib_RCON_Client.GetNotifyEvents: TwsNotifyEvent;
begin
  Result := TCPClient.NotifyEvents;
end;

function TsgcLib_RCON_Client.GetTCPClient: TsgcTCPCLient;
begin
  if not Assigned(FTCPClient) then
  begin
    FTCPClient := TsgcTCPCLient.Create(nil);
    FTCPClient.OnConnect := OnConnectEvent;
    FTCPClient.OnDisconnect := OnDisconnectEvent;
    FTCPClient.OnBinary := OnBinaryEvent;
    FTCPClient.OnException := OnExceptionEvent;
  end;
  Result := FTCPClient;
end;

procedure TsgcLib_RCON_Client.OnBinaryEvent(Connection: TsgcTCPConnection; const
    Data: TMemoryStream);
var
  oPacket: TsgcRCON_Packet;
  vOffset: Integer;
begin
  if Data.Size > 0 then
  begin
    vOffset := Length(FBuffer);
    SetLength(FBuffer, vOffset + Data.Size);
    Data.Read(FBuffer[vOffset], Data.Size);

    oPacket := TsgcRCON_Packet.Create;
    Try
      oPacket.ReadBytes(FBuffer, vOffset);

      case oPacket.PacketType of
        CS_RCON_SERVERDATA_AUTH_RESPONSE:
          begin
            if Assigned(FOnAuthenticate) then
              FOnAuthenticate(self, oPacket.ID > -1, oPacket);
          end;
        CS_RCON_SERVERDATA_RESPONSE_VALUE:
          begin
            if Assigned(FOnResponse) then
              FOnResponse(self, oPacket.Body, oPacket);
          end;
      end;

      if vOffset = Length(FBuffer) then
        SetLength(FBuffer, 0);
    Finally
      sgcFree(oPacket);
    End;
  end;
end;

procedure TsgcLib_RCON_Client.OnConnectEvent(aConnection: TsgcTCPConnection);
begin
  if Assigned(FOnConnect) then
    FOnConnect(self);

  FID := 0;
  DoRCON_Authenticate;
end;

procedure TsgcLib_RCON_Client.OnDisconnectEvent(aConnection: TsgcTCPConnection);
begin
  FID := 0;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(self);
end;

procedure TsgcLib_RCON_Client.OnExceptionEvent(Connection: TsgcTCPConnection; E:
    Exception);
begin
  if Assigned(FOnException) then
    FOnException(self, E);
end;

procedure TsgcLib_RCON_Client.SetActive(const Value: Boolean);
begin
  TCPClient.Host := RCON_Options.Host;
  TCPClient.Port := RCON_Options.Port;
  TCPClient.Active := Value;
end;

procedure TsgcLib_RCON_Client.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  TCPClient.NotifyEvents := Value;
end;

procedure TsgcLib_RCON_Client.SetRCON_Options(const Value: TsgcRCON_Options);
begin
  if Assigned(FRCON_Options) then
    FRCON_Options.Assign(Value);
end;

procedure TsgcRCON_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcRCON_Options then
  begin
    Host := TsgcRCON_Options(aSource).Host;
    Port := TsgcRCON_Options(aSource).Port;
    Password := TsgcRCON_Options(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcRCON_Packet.DoAddBytes(Value: TBytes; var Bytes: TBytes);
var
  i: Integer;
begin
  i := Length(Bytes);
  SetLength(Bytes, Length(Bytes) + Length(Value));
  sgcMove(Value[0], Bytes[i], Length(Value));
end;

procedure TsgcRCON_Packet.DoAddInteger(Value: Integer; var Bytes: TBytes);
var
  oBytes: TBytes;
begin
  SetLength(oBytes, SizeOf(Value));
  sgcMove(Value, oBytes[0], SizeOf(Value));

  DoAddBytes(oBytes, Bytes);
end;

procedure TsgcRCON_Packet.DoAddByte(Value: Byte; var Bytes: TBytes);
begin
  SetLength(Bytes, Length(Bytes) + 1);
  Bytes[Length(Bytes) - 1] := Value;
end;

function TsgcRCON_Packet.GetEmptyString: Char;
begin
  Result := Char(0);
end;

procedure TsgcRCON_Packet.ReadBytes(const aBytes: TBytes; var aOffset: Integer);
begin
  FPacketSize := DoReadIntegerFromBytes(aBytes, aOffset);
  ID := DoReadIntegerFromBytes(aBytes, aOffset);
  PacketType := DoReadIntegerFromBytes(aBytes, aOffset);
  Body := DoReadStringFromBytes(aBytes, FPacketSize - 10, aOffset);
  aOffset := aOffset + 2;
end;

function TsgcRCON_Packet.DoReadIntegerFromBytes(const aBytes: TBytes; var
    aOffset: Integer): Integer;
begin
  result := 0;
  if aOffset + 3 <= Length(aBytes) then
  begin
    result := aBytes[aOffset] ;
    result := result + aBytes[aOffset + 1] * 256;
    result := result + aBytes[aOffset + 2] * 256 * 256;
    result := result + aBytes[aOffset + 3] * 256 * 256 * 256;

    aOffset := aOffset + 4;
  end;
end;

function TsgcRCON_Packet.DoReadStringFromBytes(const aBytes: TBytes; aLength:
    Integer; var aOffset: Integer): String;
var
  oBytes: TBytes;
begin
  if aLength > 0 then
  begin
    SetLength(oBytes, aLength);
    sgcMove(aBytes[aOffset], oBytes[0], Length(oBytes));
    result := sgcGetUTF8StringFromBytes(oBytes);
    aOffset := aOffset + Length(oBytes);
  end;
end;

function TsgcRCON_Packet.WriteBytes: TBytes;
var
  oBody: TBytes;
begin
{$IFDEF D2009}
  oBody := TEncoding.Ascii.GetBytes(Body);
{$ELSE}
  SetLength(oBody, Length(Body));
  if Length(oBody) > 0 then
    sgcMove(Body[1], oBody[0], Length(Body));
{$ENDIF}

  // size
  DoAddInteger(10 + Length(oBody), result);
  // id
  DoAddInteger(ID, result);
  // type
  DoAddInteger(PacketType, result);
  // body
  DoAddBytes(oBody, result);
  DoAddByte(0, result);
  // empty string
  DoAddByte(0, result);
end;

end.
