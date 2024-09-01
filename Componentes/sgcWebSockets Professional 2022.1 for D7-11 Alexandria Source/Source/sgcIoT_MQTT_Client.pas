{ ***************************************************************************
  sgcIoT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcIoT_MQTT_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_IOT}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Helpers, sgcIoT_Client, sgcWebSocket_Client,
  sgcWebSocket_Protocol_MQTT_Client,
  sgcWebSocket_Classes, sgcWebSocket_Types, sgcWebSocket_Protocol_MQTT_Message;

Type
  TsgcIoTMQTTBeforeConnectEvent = TsgcWSMQTTBeforeConnectEvent;
  TsgcIoTMQTTConnectEvent = TsgcWSMQTTConnectEvent;
  TsgcIoTMQTTPingEvent = TsgcWSMQTTPingEvent;
  TsgcIoTMQTTPublishEvent = TsgcWSMQTTPublishEvent;
  TsgcIoTMQTTPubAckEvent = TsgcWSMQTTPubAckEvent;
  TsgcIoTMQTTPubRecEvent = TsgcWSMQTTPubRecEvent;
  TsgcIoTMQTTPubCompEvent = TsgcWSMQTTPubCompEvent;
  TsgcIoTMQTTSubscribeEvent = TsgcWSMQTTSubscribeEvent;
  TsgcIoTMQTTUnSubscribeEvent = TsgcWSMQTTUnSubscribeEvent;
  TsgcIoTMQTTDisconnectEvent = TsgcWSMQTTDisconnectEvent;

  TsgcIoTMQTT_Client = class(TsgcIoTComponent_Client)
    { MQTT }
  private
    FMQTT: TsgcWSProtocol_MQTT_Client;
    function GetMQTT: TsgcWSProtocol_MQTT_Client;
  protected
    property MQTT: TsgcWSProtocol_MQTT_Client read GetMQTT write FMQTT;
    { MQTT }

  { MQTT Properties }
  private
    FMQTTAuthentication: TsgcWSMQTTAuthentication_Options;
    FMQTTHeartBeat: TsgcWSMQTTHeartBeat_Options;
    procedure SetMQTTAuthentication(const Value: TsgcWSMQTTAuthentication_Options);
    procedure SetMQTTHeartBeat(const Value: TsgcWSMQTTHeartBeat_Options);
  protected
    property MQTTAuthentication: TsgcWSMQTTAuthentication_Options read
        FMQTTAuthentication write SetMQTTAuthentication;
    property MQTTHeartBeat: TsgcWSMQTTHeartBeat_Options read FMQTTHeartBeat write
        SetMQTTHeartBeat;
  { MQTT Properties }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  protected
    function DoSubscribe(const aTopic: String; aQoS: TmqttQoS = mtqsAtMostOnce)
      : Word; virtual;
    function DoUnSubscribe(const aTopic: string): Word; virtual;
  protected
    function DoPublish(const aTopic, aText: String;
      aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False): Word; virtual;
    function DoPublishAndWait(const aTopic, aText: String; aQoS: TmqttQoS =
        mtqsAtMostOnce; aRetain: Boolean = False; const aTimeout: integer = 10000):
        Boolean; virtual;
  public
    procedure Subscribe(const aTopic: String);
    procedure UnSubscribe(const aTopic: String);
  public
    function Publish(const aTopic, aText: String; aQoS: TmqttQoS = mtqsAtMostOnce;
        aRetain: Boolean = False): Integer;
    function PublishAndWait(const aTopic, aText: String; aQoS: TmqttQoS =
        mtqsAtMostOnce; aRetain: Boolean = False; const aTimeout: integer = 10000):
        Boolean;
    { methods }

    { events }
  protected
    FOnMQTTBeforeConnect: TsgcIoTMQTTBeforeConnectEvent;
    FOnMQTTConnect: TsgcIoTMQTTConnectEvent;
    FOnMQTTPing: TsgcIoTMQTTPingEvent;
    FOnMQTTPublish: TsgcIoTMQTTPublishEvent;
    FOnMQTTPubAck: TsgcIoTMQTTPubAckEvent;
    FOnMQTTPubRec: TsgcIoTMQTTPubRecEvent;
    FOnMQTTPubComp: TsgcIoTMQTTPubCompEvent;
    FOnMQTTSubscribe: TsgcIoTMQTTSubscribeEvent;
    FOnMQTTUnSubscribe: TsgcIoTMQTTUnSubscribeEvent;
    FOnMQTTDisconnect: TsgcIoTMQTTDisconnectEvent;
  protected
    procedure OnProtocolErrorEvent(Connection: TsgcWSConnection;
      const Error: string); virtual;
    procedure OnProtocolExceptionEvent(Connection: TsgcWSConnection;
      E: Exception); virtual;
    procedure OnProtocolConnectEvent(Connection: TsgcWSConnection); virtual;
    procedure OnProtocolDisconnectEvent(Connection: TsgcWSConnection;
      Code: Integer); virtual;
  protected
    procedure DoBeforeConnect; override;
  protected
    procedure OnMQTTBeforeConnectEvent(Connection: TsgcWSConnection;
      var aCleanSession: Boolean; var aClientIdentifier: String); virtual;
    procedure OnMQTTConnectEvent(Connection: TsgcWSConnection;
      const Session: Boolean; const ReturnCode: Integer;
      const ReturnReason: string;
      const ConnectProperties: TsgcWSMQTTCONNACKProperties); virtual;
    procedure OnMQTTDisconnectEvent(Connection: TsgcWSConnection; ReturnCode:
        Integer; const ReturnReason: String; DisconnectProperties:
        TsgcWSMQTTDISCONNECTProperties); virtual;
    procedure OnMQTTPublishEvent(Connection: TsgcWSConnection; aTopic, aText:
        String; PublishProperties: TsgcWSMQTTPUBLISHProperties); virtual;
    procedure OnMQTTSubscribeEvent(Connection: TsgcWSConnection;
      aPacketIdentifier: Word; aCodes: TsgcWSSUBACKS;
      SubscribeProperties: TsgcWSMQTTSUBACKProperties); virtual;
    procedure OnMQTTUnSubscribeEvent(Connection: TsgcWSConnection;
      aPacketIdentifier: Word; aCodes: TsgcWSUNSUBACKS;
      UnsubscribeProperties: TsgcWSMQTTUNSUBACKProperties); virtual;
    procedure OnMQTTPubAckEvent(Connection: TsgcWSConnection; aPacketIdentifier:
        Word; ReasonCode: Integer; const ReasonName: string; PubAckProperties:
        TsgcWSMQTTPUBACKProperties); virtual;
  public
    property OnMQTTBeforeConnect: TsgcIoTMQTTBeforeConnectEvent
      read FOnMQTTBeforeConnect write FOnMQTTBeforeConnect;
    property OnMQTTConnect: TsgcIoTMQTTConnectEvent read FOnMQTTConnect
      write FOnMQTTConnect;
    property OnMQTTPing: TsgcIoTMQTTPingEvent read FOnMQTTPing
      write FOnMQTTPing;
    property OnMQTTPublish: TsgcIoTMQTTPublishEvent read FOnMQTTPublish
      write FOnMQTTPublish;
    property OnMQTTPubAck: TsgcIoTMQTTPubAckEvent read FOnMQTTPubAck
      write FOnMQTTPubAck;
    property OnMQTTPubRec: TsgcIoTMQTTPubRecEvent read FOnMQTTPubRec
      write FOnMQTTPubRec;
    property OnMQTTPubComp: TsgcIoTMQTTPubCompEvent read FOnMQTTPubComp
      write FOnMQTTPubComp;
    property OnMQTTSubscribe: TsgcIoTMQTTSubscribeEvent read FOnMQTTSubscribe
      write FOnMQTTSubscribe;
    property OnMQTTUnSubscribe: TsgcIoTMQTTUnSubscribeEvent
      read FOnMQTTUnSubscribe write FOnMQTTUnSubscribe;
    property OnMQTTDisconnect: TsgcIoTMQTTDisconnectEvent read FOnMQTTDisconnect
      write FOnMQTTDisconnect;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_IOT}

constructor TsgcIoTMQTT_Client.Create(aOwner: TComponent);
begin
  inherited;
  MQTT.Client := TCPClient;
  FMQTTHeartBeat := TsgcWSMQTTHeartBeat_Options.Create;
  FMQTTAuthentication := TsgcWSMQTTAuthentication_Options.Create;
end;

destructor TsgcIoTMQTT_Client.Destroy;
begin
  sgcFree(FMQTTAuthentication);
  sgcFree(FMQTTHeartBeat);
  sgcFree(FMQTT);
  inherited;
end;

procedure TsgcIoTMQTT_Client.DoBeforeConnect;
begin
  inherited;
  MQTT.HeartBeat.Assign(MQTTHeartBeat);
  MQTT.Authentication.Assign(MQTTAuthentication);
end;

function TsgcIoTMQTT_Client.DoPublish(const aTopic, aText: String;
  aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False): Word;
begin
  Result := MQTT.Publish(aTopic, aText, aQoS, aRetain);
end;

function TsgcIoTMQTT_Client.DoPublishAndWait(const aTopic, aText: String; aQoS:
    TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False; const aTimeout:
    integer = 10000): Boolean;
begin
  Result := MQTT.PublishAndWait(aTopic, aText, aQoS, aRetain, nil, aTimeout);
end;

function TsgcIoTMQTT_Client.GetMQTT: TsgcWSProtocol_MQTT_Client;
begin
  if not Assigned(FMQTT) then
  begin
    FMQTT := TsgcWSProtocol_MQTT_Client.Create(nil);
    FMQTT.OnMQTTBeforeConnect := OnMQTTBeforeConnectEvent;
    FMQTT.OnMQTTConnect := OnMQTTConnectEvent;
    FMQTT.OnMQTTPublish := OnMQTTPublishEvent;
    FMQTT.OnMQTTSubscribe := OnMQTTSubscribeEvent;
    FMQTT.OnMQTTPubAck := OnMQTTPubAckEvent;
    FMQTT.OnMQTTUnSubscribe := OnMQTTUnSubscribeEvent;
    FMQTT.OnMQTTDisconnect := OnMQTTDisconnectEvent;
    FMQTT.OnConnect := OnProtocolConnectEvent;
    FMQTT.OnDisconnect := OnProtocolDisconnectEvent;
    FMQTT.OnException := OnProtocolExceptionEvent;
    FMQTT.OnError := OnProtocolErrorEvent;
  end;
  Result := FMQTT;
end;

procedure TsgcIoTMQTT_Client.OnProtocolConnectEvent
  (Connection: TsgcWSConnection);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Connection);
end;

procedure TsgcIoTMQTT_Client.OnProtocolDisconnectEvent
  (Connection: TsgcWSConnection; Code: Integer);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Connection, Code);
end;

procedure TsgcIoTMQTT_Client.OnProtocolErrorEvent(Connection: TsgcWSConnection;
  const Error: string);
begin
  if Assigned(FOnError) then
    FOnError(Connection, Error);
end;

procedure TsgcIoTMQTT_Client.OnProtocolExceptionEvent
  (Connection: TsgcWSConnection; E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Connection, E);
end;

procedure TsgcIoTMQTT_Client.OnMQTTBeforeConnectEvent
  (Connection: TsgcWSConnection; var aCleanSession: Boolean;
  var aClientIdentifier: String);
begin
  if Assigned(FOnMQTTBeforeConnect) then
    FOnMQTTBeforeConnect(Connection, aCleanSession, aClientIdentifier);
end;

procedure TsgcIoTMQTT_Client.OnMQTTConnectEvent(Connection: TsgcWSConnection;
  const Session: Boolean; const ReturnCode: Integer; const ReturnReason: string;
  const ConnectProperties: TsgcWSMQTTCONNACKProperties);
begin
  if Assigned(FOnMQTTConnect) then
    FOnMQTTConnect(Connection, Session, ReturnCode, ReturnReason,
      ConnectProperties);
end;

procedure TsgcIoTMQTT_Client.OnMQTTDisconnectEvent(Connection:
    TsgcWSConnection; ReturnCode: Integer; const ReturnReason: String;
    DisconnectProperties: TsgcWSMQTTDISCONNECTProperties);
begin
  if Assigned(FOnMQTTDisconnect) then
    FOnMQTTDisconnect(Connection, ReturnCode, ReturnReason, DisconnectProperties);
end;

procedure TsgcIoTMQTT_Client.OnMQTTPublishEvent(Connection: TsgcWSConnection;
    aTopic, aText: String; PublishProperties: TsgcWSMQTTPUBLISHProperties);
begin
  if Assigned(FOnMQTTPublish) then
    FOnMQTTPublish(Connection, aTopic, aText, PublishProperties);
end;

procedure TsgcIoTMQTT_Client.OnMQTTSubscribeEvent(Connection: TsgcWSConnection;
  aPacketIdentifier: Word; aCodes: TsgcWSSUBACKS;
  SubscribeProperties: TsgcWSMQTTSUBACKProperties);
begin
  if Assigned(FOnMQTTSubscribe) then
    FOnMQTTSubscribe(Connection, aPacketIdentifier, aCodes,
      SubscribeProperties);
end;

procedure TsgcIoTMQTT_Client.OnMQTTUnSubscribeEvent
  (Connection: TsgcWSConnection; aPacketIdentifier: Word;
  aCodes: TsgcWSUNSUBACKS; UnsubscribeProperties: TsgcWSMQTTUNSUBACKProperties);
begin
  if Assigned(FOnMQTTUnSubscribe) then
    FOnMQTTUnSubscribe(Connection, aPacketIdentifier, aCodes,
      UnsubscribeProperties);
end;

function TsgcIoTMQTT_Client.DoSubscribe(const aTopic: String;
  aQoS: TmqttQoS = mtqsAtMostOnce): Word;
begin
  Result := MQTT.Subscribe(aTopic, aQoS);
end;

function TsgcIoTMQTT_Client.DoUnSubscribe(const aTopic: string): Word;
begin
  Result := MQTT.UnSubscribe(aTopic);
end;

procedure TsgcIoTMQTT_Client.OnMQTTPubAckEvent(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; ReasonCode: Integer; const ReasonName: string;
    PubAckProperties: TsgcWSMQTTPUBACKProperties);
begin
  if Assigned(FOnMQTTPubAck) then
    FOnMQTTPubAck(Connection, aPacketIdentifier, ReasonCode, ReasonName, PubAckProperties);
end;

function TsgcIoTMQTT_Client.Publish(const aTopic, aText: String; aQoS: TmqttQoS
    = mtqsAtMostOnce; aRetain: Boolean = False): Integer;
begin
  result := DoPublish(aTopic, aText, aQoS, aRetain);
end;

function TsgcIoTMQTT_Client.PublishAndWait(const aTopic, aText: String; aQoS:
    TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False; const aTimeout:
    integer = 10000): Boolean;
begin
  result := DoPublishAndWait(aTopic, aText, aQoS, aRetain, aTimeout);
end;

procedure TsgcIoTMQTT_Client.SetMQTTAuthentication(const Value:
    TsgcWSMQTTAuthentication_Options);
begin
  if Assigned(FMQTTAuthentication) then
    FMQTTAuthentication.Assign(Value);
end;

procedure TsgcIoTMQTT_Client.SetMQTTHeartBeat(const Value:
    TsgcWSMQTTHeartBeat_Options);
begin
  if Assigned(FMQTTHeartBeat) then
    FMQTTHeartBeat.Assign(Value);
end;

procedure TsgcIoTMQTT_Client.Subscribe(const aTopic: String);
begin
  DoSubscribe(aTopic);
end;

procedure TsgcIoTMQTT_Client.UnSubscribe(const aTopic: String);
begin
  DoUnSubscribe(aTopic);
end;

{$ENDIF}

end.
