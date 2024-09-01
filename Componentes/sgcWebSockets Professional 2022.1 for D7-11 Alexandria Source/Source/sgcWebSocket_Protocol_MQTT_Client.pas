{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_MQTT_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgcbase
  sgcBase_Classes,
  // sgcWebSocket
  sgcJSON, sgcWebSocket_Protocol_MQTT_Message, sgcWebSocket_Types,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Classes_SyncObjs,
  sgcBase_Helpers;

type

  TsgcWSMQTTItemType = (mqttText, mqttStream);

  TsgcWSMQTTBeforeConnectEvent = procedure(Connection: TsgcWSConnection;
    var aCleanSession: Boolean; var aClientIdentifier: String) of object;
  TsgcWSMQTTConnectEvent = procedure(Connection: TsgcWSConnection;
    const Session: Boolean; const ReasonCode: Integer; const ReasonName: String;
    const ConnectProperties: TsgcWSMQTTCONNACKProperties) of object;
  TsgcWSMQTTPingEvent = procedure(Connection: TsgcWSConnection) of object;
  TsgcWSMQTTPublishEvent = procedure(Connection: TsgcWSConnection;
    aTopic, aText: String; PublishProperties: TsgcWSMQTTPUBLISHProperties)
    of object;
  TsgcWSMQTTPubAckEvent = procedure(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; ReasonCode: Integer; const ReasonName: String;
    PubAckProperties: TsgcWSMQTTPUBACKProperties) of object;
  TsgcWSMQTTPubRecEvent = procedure(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; ReasonCode: Integer; const ReasonName: String;
    PubRecProperties: TsgcWSMQTTPUBRECProperties) of object;
  TsgcWSMQTTPubRelEvent = procedure(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; ReasonCode: Integer; const ReasonName: String;
    PubRelProperties: TsgcWSMQTTPUBRELProperties) of object;
  TsgcWSMQTTPubCompEvent = procedure(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; ReasonCode: Integer; const ReasonName: String;
    PubCompProperties: TsgcWSMQTTPUBCOMPProperties) of object;
  TsgcWSMQTTSubscribeEvent = procedure(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; aCodes: TsgcWSSUBACKS;
    SubscribeProperties: TsgcWSMQTTSUBACKProperties) of object;
  TsgcWSMQTTUnSubscribeEvent = procedure(Connection: TsgcWSConnection;
    aPacketIdentifier: Word; aCodes: TsgcWSUNSUBACKS;
    UnsubscribeProperties: TsgcWSMQTTUNSUBACKProperties) of object;
  TsgcWSMQTTDisconnectEvent = procedure(Connection: TsgcWSConnection;
    ReasonCode: Integer; const ReasonName: String;
    DisconnectProperties: TsgcWSMQTTDISCONNECTProperties) of object;
  TsgcWSMQTTAuthEvent = procedure(Connection: TsgcWSConnection;
    ReasonCode: Integer; const ReasonName: String;
    AuthProperties: TsgcWSMQTTAUTHProperties) of object;

  TsgcWSComponent_HeartBeat = class(TsgcWSComponent)
  end;

  TsgcWSMQTTQoSItem = class
  private
    FDate: TDateTime;
    FItemType: TsgcWSMQTTItemType;
    FPacketIdentifier: Word;
    FPublishProperties: TsgcWSMQTTPublish_Properties;
    FQoS: TmqttQoS;
    FRetain: Boolean;
    FStream: TStream;
    FText: String;
    FTopic: String;
    procedure SetItemType(const Value: TsgcWSMQTTItemType);
    procedure SetPublishProperties(const Value: TsgcWSMQTTPublish_Properties);
    procedure SetStream(const Value: TStream);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Date: TDateTime read FDate write FDate;
    property ItemType: TsgcWSMQTTItemType read FItemType write SetItemType;
    property PacketIdentifier: Word read FPacketIdentifier
      write FPacketIdentifier;
    property PublishProperties: TsgcWSMQTTPublish_Properties
      read FPublishProperties write SetPublishProperties;
    property QoS: TmqttQoS read FQoS write FQoS;
    property Retain: Boolean read FRetain write FRetain;
    property Stream: TStream read FStream write SetStream;
    property Text: String read FText write FText;
    property Topic: String read FTopic write FTopic;
  end;

  TsgcWSMQTTQoSList = class({$IFDEF NEXTGEN}TList<TsgcWSMQTTQoSItem>{$ELSE}TObjectList{$ENDIF})

  end;

  TsgcWSMQTTQoS_Options = class(TPersistent)
  private
    FLevel: TmqttQoS;
    FInterval: Integer;
    FTimeout: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Level: TmqttQoS read FLevel write FLevel;
    property Interval: Integer read FInterval write FInterval;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TsgcWSMQTTAuthentication_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FPassword: String;
    FUserName: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Password: String read FPassword write FPassword;
    property UserName: String read FUserName write FUserName;
  end;

  TsgcWSMQTTLWT_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FQoS: TmqttQoS;
    FRetain: Boolean;
    FText: String;
    FTopic: String;
    FWillProperties: TsgcWSMQTTWill_Properties;
    procedure SetWillProperties(const Value: TsgcWSMQTTWill_Properties);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property QoS: TmqttQoS read FQoS write FQoS;
    property Retain: Boolean read FRetain write FRetain;
    property Text: String read FText write FText;
    property Topic: String read FTopic write FTopic;
    property WillProperties: TsgcWSMQTTWill_Properties read FWillProperties
      write SetWillProperties;
  end;

  TsgcWSMQTTHeartBeat_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FTimeout: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TsgcWSProtocol_MQTT_Client = class(TsgcWSProtocol_Client_Base)
    { packetidentifier }
  private
    FPacketIdentifier: Word;
    function GetPacketIdentifier: Word;
    { packetidentifier }

    { wsmessage }
  private
    FWSMessageId: String;
  protected
    FWSMessage: TsgcWSMQTTMessage;
    function GetWSMessage: TsgcWSMQTTMessage;
  protected
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection)
      : TsgcWSMQTTMessage;
  protected
    property WSMessage: TsgcWSMQTTMessage read GetWSMessage write FWSMessage;
    { wsmessage }

    { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); override;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); overload; override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    procedure DoEventException(aConnection: TSgcWSConnection; const Error: String;
        aException: Exception); override;
    { from TsgcWSComponent }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { from TsgcWSProtocol_Client }
  private
    FFirstPing: TDateTime;
    FLastPing: TDateTime;
    FLastPong: TDateTime;
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSProtocol_Client }

    { from TsgcWSProtocol_Client_Base }
  protected
    procedure DoWriteRawData(const aText: String); virtual;
  public
    procedure WriteData(const aText: String); override;
    { from TsgcWSProtocol_Client_Base }

  protected
    function GetWSMessageText: String; virtual;
    procedure DoWriteMessageText; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { procedures }
  private
    FLastPUBACK: Integer;
    FLastPUBCOMP: Integer;
    FWaitResponse: Boolean;
    function DoPublishAndWait(aPacketIdentifier: Word; const aTopic, aText: String;
        const aStream: TStream; aQoS: TmqttQoS; aRetain: Boolean;
        aPublishProperties: TsgcWSMQTTPublish_Properties = nil; const aTimeout:
        Integer = 10000): Boolean;
  private
    procedure DoWriteBytes(const aBytes: TBytes);
  private
    procedure DoConnect;
    procedure DoPing;
    procedure DoPublish(aPacketIdentifier: Word; const aTopic, aText: String;
      aQoS: TmqttQoS; aRetain: Boolean; const aDup: Boolean;
      aPublishProperties: TsgcWSMQTTPublish_Properties = nil); overload;
    procedure DoPublish(aPacketIdentifier: Word; const aTopic: String;
      aStream: TStream; aQoS: TmqttQoS; aRetain: Boolean; const aDup: Boolean;
      aPublishProperties: TsgcWSMQTTPublish_Properties = nil); overload;
    procedure DoPubAck(aPacketIdentifier: Word);
    procedure DoPubRec(aPacketIdentifier: Word);
    procedure DoPubRel(aPacketIdentifier: Word);
    procedure DoPubComp(aPacketIdentifier: Word);
    procedure DoSubscribe(aPacketIdentifier: Integer; aTopics: TsgcWSTopics;
      const aSubscribeProperties: TsgcWSMQTTSubscribe_Properties);
    procedure DoUnSubscribe(aPacketIdentifier: Integer; aTopics: TsgcWSTopics);
    procedure DoDisconnect(aReasonCode: Integer = 0;
      aDisconnectProperties: TsgcWSMQTTDisconnect_Properties = nil);
    procedure DoAuth(aReAuthenticate: Boolean;
      aAuthProperties: TsgcWSMQTTAuth_Properties = nil);
  public
    procedure Connect;
    procedure Ping;
    function Publish(const aTopic, aText: String;
      aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False;
      const aPublishProperties: TsgcWSMQTTPublish_Properties = nil)
      : Word; overload;
    function Publish(const aTopic: String; const aStream: TStream;
      aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False;
      const aPublishProperties: TsgcWSMQTTPublish_Properties = nil)
      : Word; overload;
    function PublishAndWait(const aTopic, aText: String;
      aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False;
      const aPublishProperties: TsgcWSMQTTPublish_Properties = nil;
      const aTimeout: Integer = 10000): Boolean; overload;
    function PublishAndWait(const aTopic: String; const aStream: TStream;
      aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False;
      const aPublishProperties: TsgcWSMQTTPublish_Properties = nil;
      const aTimeout: Integer = 10000): Boolean; overload;
    function Subscribe(const aTopic: String; aQoS: TmqttQoS = mtqsAtMostOnce;
      const aSubscribeProperties: TsgcWSMQTTSubscribe_Properties = nil)
      : Word; overload;
    function Subscribe(aTopics: TsgcWSTopics): Word; overload;
    function UnSubscribe(const aTopic: string;
      aUnsubscribeProperties: TsgcWSMQTTUnsubscribe_Properties = nil)
      : Word; overload;
    function UnSubscribe(aTopics: TsgcWSTopics): Word; overload;
    procedure Disconnect; overload;
    procedure Disconnect(aReasonCode: Integer;
      aDisconnectProperties: TsgcWSMQTTDisconnect_Properties = nil); overload;
    procedure Auth(aReAuthenticate: Boolean;
      aAuthProperties: TsgcWSMQTTAuth_Properties = nil);
    { procedures }

    { version }
  private
    FMQTTVersion: TwsMQTTVersion;
  protected
    procedure SetMQTTVersion(const Value: TwsMQTTVersion); virtual;
  public
    property MQTTVersion: TwsMQTTVersion read FMQTTVersion write SetMQTTVersion;
    { version }

    { QoS }
  private
    FQoSTimer: TsgcTimer;
    FQoS: TsgcWSMQTTQoS_Options;
    FQoSList: TsgcWSMQTTQoSList;
    function GetQoSList: TsgcWSMQTTQoSList;
  protected
    procedure DoStartQoS; virtual;
    procedure DoStopQoS; virtual;
  protected
    procedure DoQoSList; virtual;
  protected
    procedure DoDeleteStoredMessage(const aId: Word); virtual;
    procedure DoQueuePublish(const aPacketIdentifier: Word;
      const aTopic, aText: String; aQoS: TmqttQoS; aRetain: Boolean;
      aPublishProperties: TsgcWSMQTTPublish_Properties); overload; virtual;
    procedure DoQueuePublish(const aPacketIdentifier: Word;
      const aTopic: String; aStream: TStream; aQoS: TmqttQoS; aRetain: Boolean;
      aPublishProperties: TsgcWSMQTTPublish_Properties); overload; virtual;
  protected
    procedure OnQoSEvent(Sender: TObject); virtual;
    procedure OnQoSExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    procedure SetQoS(const Value: TsgcWSMQTTQoS_Options);
  protected
    property QoSList: TsgcWSMQTTQoSList read GetQoSList;
  public
    property QoS: TsgcWSMQTTQoS_Options read FQoS write SetQoS;
    { QoS }

    { LastWillTestament }
  private
    FLastWillTestament: TsgcWSMQTTLWT_Options;
    procedure SetLastWillTestament(const Value: TsgcWSMQTTLWT_Options);
  public
    property LastWillTestament: TsgcWSMQTTLWT_Options read FLastWillTestament
      write SetLastWillTestament;
    { LastWillTestament }

    { Authentication }
  private
    FAuthentication: TsgcWSMQTTAuthentication_Options;
    procedure SetAuthentication(const Value: TsgcWSMQTTAuthentication_Options);
  public
    property Authentication: TsgcWSMQTTAuthentication_Options
      read FAuthentication write SetAuthentication;
    { Authentication }

    { heartbeat }
  private
    FHeartBeat: TsgcWSMQTTHeartBeat_Options;
    procedure SetHeartBeat(const Value: TsgcWSMQTTHeartBeat_Options);
  public
    property HeartBeat: TsgcWSMQTTHeartBeat_Options read FHeartBeat
      write SetHeartBeat;
    { heartbeat }

    { connect properties }
  private
    FConnectProperties: TsgcWSMQTTConnect_Properties;
    procedure SetConnectProperties(const Value: TsgcWSMQTTConnect_Properties);
  public
    property ConnectProperties: TsgcWSMQTTConnect_Properties
      read FConnectProperties write SetConnectProperties;
    { connect properties }

    { events }
  private
    FOnMQTTBeforeConnect: TsgcWSMQTTBeforeConnectEvent;
    FOnMQTTConnect: TsgcWSMQTTConnectEvent;
    FOnMQTTPing: TsgcWSMQTTPingEvent;
    FOnMQTTPublish: TsgcWSMQTTPublishEvent;
    FOnMQTTPubAck: TsgcWSMQTTPubAckEvent;
    FOnMQTTPubRec: TsgcWSMQTTPubRecEvent;
    FOnMQTTPubRel: TsgcWSMQTTPubRelEvent;
    FOnMQTTPubComp: TsgcWSMQTTPubCompEvent;
    FOnMQTTSubscribe: TsgcWSMQTTSubscribeEvent;
    FOnMQTTUnSubscribe: TsgcWSMQTTUnSubscribeEvent;
    FOnMQTTDisconnect: TsgcWSMQTTDisconnectEvent;
    FOnMQTTAuth: TsgcWSMQTTAuthEvent;
  protected
    procedure DoEventMQTTBeforeConnect(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTConnect(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTPing(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTPublish(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTPubAck(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTPubRec(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTPubRel(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTPubComp(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTSubscribe(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTUnSubscribe(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTDisconnect(aConnection: TsgcWSConnection); virtual;
    procedure DoEventMQTTAuth(aConnection: TsgcWSConnection); virtual;
  public
    property OnMQTTBeforeConnect: TsgcWSMQTTBeforeConnectEvent
      read FOnMQTTBeforeConnect write FOnMQTTBeforeConnect;
    property OnMQTTConnect: TsgcWSMQTTConnectEvent read FOnMQTTConnect
      write FOnMQTTConnect;
    property OnMQTTPing: TsgcWSMQTTPingEvent read FOnMQTTPing write FOnMQTTPing;
    property OnMQTTPublish: TsgcWSMQTTPublishEvent read FOnMQTTPublish
      write FOnMQTTPublish;
    property OnMQTTPubAck: TsgcWSMQTTPubAckEvent read FOnMQTTPubAck
      write FOnMQTTPubAck;
    property OnMQTTPubRec: TsgcWSMQTTPubRecEvent read FOnMQTTPubRec
      write FOnMQTTPubRec;
    property OnMQTTPubRel: TsgcWSMQTTPubRelEvent read FOnMQTTPubRel
      write FOnMQTTPubRel;
    property OnMQTTPubComp: TsgcWSMQTTPubCompEvent read FOnMQTTPubComp
      write FOnMQTTPubComp;
    property OnMQTTSubscribe: TsgcWSMQTTSubscribeEvent read FOnMQTTSubscribe
      write FOnMQTTSubscribe;
    property OnMQTTUnSubscribe: TsgcWSMQTTUnSubscribeEvent
      read FOnMQTTUnSubscribe write FOnMQTTUnSubscribe;
    property OnMQTTDisconnect: TsgcWSMQTTDisconnectEvent read FOnMQTTDisconnect
      write FOnMQTTDisconnect;
    property OnMQTTAuth: TsgcWSMQTTAuthEvent read FOnMQTTAuth write FOnMQTTAuth;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  DateUtils,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  sgcWebSocket_Const, sgcWebSocket_Helpers;

const
  CS_QOS_LIST = 0;

type
  THackComponent_Client = class(TsgcWSComponent_Client);

constructor TsgcWSProtocol_MQTT_Client.Create(aOwner: TComponent);
begin
  inherited;
  FMQTTVersion := mqtt311;
  FWSMessageId := NewGuid;
  FProtocol := CS_PROTOCOL_MQTT;
  MsgType := msgBinary;
  FQoS := TsgcWSMQTTQoS_Options.Create;
  FQoS.Level := mtqsAtMostOnce;
  FQoS.Interval := 60;
  FQoS.Timeout := 300;
  FLastWillTestament := TsgcWSMQTTLWT_Options.Create;
  FLastWillTestament.Enabled := False;
  FAuthentication := TsgcWSMQTTAuthentication_Options.Create;
  FAuthentication.Enabled := False;
  FHeartBeat := TsgcWSMQTTHeartBeat_Options.Create;
  FHeartBeat.Enabled := True;
  FConnectProperties := TsgcWSMQTTConnect_Properties.Create;
end;

destructor TsgcWSProtocol_MQTT_Client.Destroy;
begin
  sgcFree(FConnectProperties);
  sgcFree(FHeartBeat);
  sgcFree(FAuthentication);
  sgcFree(FLastWillTestament);
  sgcFree(FQoSTimer);
  sgcFree(FQoS);
  sgcFree(FQoSList);
  inherited;
end;

procedure TsgcWSProtocol_MQTT_Client.Auth(aReAuthenticate: Boolean;
  aAuthProperties: TsgcWSMQTTAuth_Properties = nil);
begin
  DoAuth(aReAuthenticate, aAuthProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.Connect;
begin
  DoConnect;
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
begin
  inherited;
  // MQTT protocol only uses binary messages
end;

function TsgcWSProtocol_MQTT_Client.GetWSMessage: TsgcWSMQTTMessage;
begin
  result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessage) then
      FWSMessage := TsgcWSMQTTMessage.Create(self);
    result := FWSMessage;
  end;

  result.MQTTVersion := FMQTTVersion;
end;

procedure TsgcWSProtocol_MQTT_Client.Disconnect;
begin
  DoDisconnect;
end;

procedure TsgcWSProtocol_MQTT_Client.Disconnect(aReasonCode: Integer;
  aDisconnectProperties: TsgcWSMQTTDisconnect_Properties = nil);
begin
  DoDisconnect(aReasonCode, aDisconnectProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoAuth(aReAuthenticate: Boolean;
  aAuthProperties: TsgcWSMQTTAuth_Properties = nil);
begin
  if Assigned(aAuthProperties) then
  begin
    WSMessage.AuthProperties.Assign(aAuthProperties);
    WSMessage.AuthProperties.Enabled := True;
  end;

  DoWriteBytes(WSMessage.DoAuth(aReAuthenticate));
end;

procedure TsgcWSProtocol_MQTT_Client.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventBinary(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
var
  oMessage: TsgcWSMQTTMessage;
begin
  inherited;

  oMessage := WSMessage;
  Data.Position := 0;

  while Data.Position < Data.Size do
  begin
    if aConnection.Transport = trpTCP then
    begin
      if not oMessage.ReadTCP(Data) then
        exit;
    end
    else
      oMessage.Read(Data);

    case oMessage.FixedHeader.ControlPacket of
      mtcpReserved0:
        ;
      mtcpCONNECT:
        ;
      mtcpCONNACK:
        DoEventMQTTConnect(aConnection);
      mtcpPUBLISH:
        begin
          DoEventMQTTPublish(aConnection);
          Case oMessage.FixedHeader.QoS of
            mtqsAtLeastOnce:
              DoPubAck(oMessage.Publish.PacketIdentifier);
            mtqsExactlyOnce:
              DoPubRec(oMessage.Publish.PacketIdentifier);
          end;
        end;
      mtcpPUBACK:
        begin
          DoDeleteStoredMessage(oMessage.PUBACK.PacketIdentifier);
          DoEventMQTTPubAck(aConnection);
        end;
      mtcpPUBREC:
        begin
          DoEventMQTTPubRec(aConnection);
          DoPubRel(oMessage.PUBREC.PacketIdentifier);
        end;
      mtcpPUBREL:
        begin
          DoEventMQTTPubRel(aConnection);
          DoPubComp(oMessage.PUBREL.PacketIdentifier);
        end;
      mtcpPUBCOMP:
        begin
          DoDeleteStoredMessage(oMessage.PUBCOMP.PacketIdentifier);
          DoEventMQTTPubComp(aConnection);
        end;
      mtcpSUBSCRIBE:
        ;
      mtcpSUBACK:
        DoEventMQTTSubscribe(aConnection);
      mtcpUNSUBSCRIBE:
        ;
      mtcpUNSUBACK:
        DoEventMQTTUnSubscribe(aConnection);
      mtcpPINGREQ:
        ;
      mtcpPINGRESP:
        begin
          FLastPong := Now;
          DoEventMQTTPing(aConnection);
        end;
      mtcpDISCONNECT:
        begin
          aConnection.Close;
          DoEventMQTTDisconnect(aConnection);
        end;
      mtcpAUTH:
        ;
    end;
  end;
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
  FFirstPing := 0;
  FLastPing := 0;
  FLastPong := 0;

  DoStartQoS;

  DoEventMQTTBeforeConnect(aConnection);
  DoConnect;
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
var
  vId: String;
begin
  DoEventMQTTDisconnect(aConnection);

  vId := aConnection.Guid;
  if Assigned(FWSMessage) then
    FWSMessage.Clear;
  DoStopQoS;
  inherited;

  FWSConnection := nil;
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTConnect
  (aConnection: TsgcWSConnection);
begin
  if not WSMessage.CONNACK.Session then
  begin
    FPacketIdentifier := 0;
    QoSList.Clear;
  end;

  if Assigned(FOnMQTTConnect) then
    FOnMQTTConnect(aConnection, WSMessage.CONNACK.Session,
      WSMessage.CONNACK.ReasonCode, WSMessage.CONNACK.ReasonName,
      WSMessage.CONNACK.CONNACKProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  if Assigned(FWSMessage) then
    FWSMessage.Clear;
  DoStopQoS;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_MQTT_Client.DoInitialize
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FFirstPing := 0;
  FLastPing := 0;
  FLastPong := 0;
  FWSConnection := aConnection;

  DoStartQoS;
end;

procedure TsgcWSProtocol_MQTT_Client.DoDeleteStoredMessage(const aId: Word);
var
  i: Integer;
begin
  DoEnterCS(CS_QOS_LIST);
  Try
    for i := QoSList.Count - 1 Downto 0 do
    begin
      if TsgcWSMQTTQoSItem(QoSList.Items[i]).PacketIdentifier = aId then
      begin
        QoSList.Delete(i);
        break;
      end;
    end;
  Finally
    DoLeaveCS(CS_QOS_LIST);
  End;
end;

procedure TsgcWSProtocol_MQTT_Client.DoQoSList;
var
  i: Integer;
begin
  DoEnterCS(CS_QOS_LIST);
  Try
    for i := 0 to QoSList.Count - 1 do
    begin
      if Trunc((Now - TsgcWSMQTTQoSItem(QoSList.Items[i]).Date) * 86400) > QoS.Timeout
      then
      begin
        case TsgcWSMQTTQoSItem(QoSList.Items[i]).ItemType of
          mqttText:
            DoPublish(TsgcWSMQTTQoSItem(QoSList.Items[i]).PacketIdentifier,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).Topic,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).Text,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).QoS,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).Retain, True);
          mqttStream:
            DoPublish(TsgcWSMQTTQoSItem(QoSList.Items[i]).PacketIdentifier,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).Topic,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).Stream,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).QoS,
              TsgcWSMQTTQoSItem(QoSList.Items[i]).Retain, True);
        end;
      end;
    end;
  Finally
    DoLeaveCS(CS_QOS_LIST);
  End;
end;

procedure TsgcWSProtocol_MQTT_Client.DoQueuePublish(const aPacketIdentifier
  : Word; const aTopic, aText: String; aQoS: TmqttQoS; aRetain: Boolean;
  aPublishProperties: TsgcWSMQTTPublish_Properties);
var
  oItem: TsgcWSMQTTQoSItem;
begin
  if aPacketIdentifier = 0 then
    exit;

  case aQoS of
    mtqsAtLeastOnce, mtqsExactlyOnce:
      begin
        DoEnterCS(CS_QOS_LIST);
        Try
          oItem := TsgcWSMQTTQoSItem.Create;
          oItem.PacketIdentifier := aPacketIdentifier;
          oItem.Topic := aTopic;
          oItem.Text := aText;
          oItem.QoS := aQoS;
          oItem.Retain := aRetain;
          oItem.Date := Now;
          oItem.ItemType := mqttText;
          if Assigned(aPublishProperties) then
            oItem.PublishProperties := aPublishProperties;
          QoSList.Add(oItem)
        Finally
          DoLeaveCS(CS_QOS_LIST);
        End;
      end;
  end;
end;

procedure TsgcWSProtocol_MQTT_Client.DoStartQoS;
begin
  if QoS.Level <> mtqsAtMostOnce then
  begin
    if not Assigned(FQoSTimer) then
    begin
      FQoSTimer := TsgcTimer.Create;
      FQoSTimer.DebugName := 'QoS Timer';
      if Assigned(Client) then
        FQoSTimer.NotifyEvents := THackComponent_Client(Client).NotifyEvents;
      FQoSTimer.Interval := FQoS.Interval;
      FQoSTimer.OnTimer := OnQoSEvent;
      FQoSTimer.OnException := OnQoSExceptionEvent;
      FQoSTimer.Interval := QoS.Interval * 1000;
    end;

    if FQoSTimer.Interval > 0 then
    begin
      if not FQoSTimer.Enabled then
        FQoSTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcWSProtocol_MQTT_Client.DoStopQoS;
begin
  sgcThreadFree(FQoSTimer);
end;

procedure TsgcWSProtocol_MQTT_Client.DoConnect;
begin
  // ... enable / disable heartbeat in client
  if Assigned(Client) then
  begin
    // ... set heartbeat client properties
    Client.HeartBeat.Enabled := HeartBeat.Enabled;
    Client.HeartBeat.Interval := HeartBeat.Interval;
    Client.HeartBeat.Timeout := 0;

    // ... start client heartbeat
    TsgcWSComponent_HeartBeat(Client).DoStartHeartBeat;
  end;

  // ... send message
  WSMessage.ConnectProperties := ConnectProperties;
  WSMessage.WillProperties := LastWillTestament.WillProperties;

  DoWriteBytes(WSMessage.DoConnect(Authentication.Enabled, Authentication.UserName,
      Authentication.Password, LastWillTestament.Enabled, LastWillTestament.Topic,
      LastWillTestament.Text, LastWillTestament.QoS, LastWillTestament.Retain,
      HeartBeat.Enabled, HeartBeat.Interval));
end;

procedure TsgcWSProtocol_MQTT_Client.DoDisconnect(aReasonCode: Integer = 0;
  aDisconnectProperties: TsgcWSMQTTDisconnect_Properties = nil);
begin
  if Assigned(aDisconnectProperties) then
  begin
    WSMessage.DisconnectProperties.Assign(aDisconnectProperties);
    WSMessage.DisconnectProperties.Enabled := True;
  end;

  DoWriteBytes(WSMessage.DoDisconnect(aReasonCode));
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventException(aConnection:
    TSgcWSConnection; const Error: String; aException: Exception);
begin
  inherited;
  if Assigned(Client) then
    THackComponent_Client(Client).DoEventException(aConnection, aException.Message, aException);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTBeforeConnect
  (aConnection: TsgcWSConnection);
var
  vCleanSession: Boolean;
  vClientIdentifier: String;
begin
  vCleanSession := True;
  vClientIdentifier := GetWSMessageByConnection(aConnection).ClientId;

  if Assigned(FOnMQTTBeforeConnect) then
  begin
    FOnMQTTBeforeConnect(aConnection, vCleanSession, vClientIdentifier);
    if vCleanSession and (vClientIdentifier = WSMessage.ClientId) then
      vClientIdentifier := '';
    WSMessage.CleanSession := vCleanSession;
    WSMessage.ClientId := vClientIdentifier;
  end
  else
    WSMessage.ClientId := '';
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTDisconnect
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTDisconnect) then
    FOnMQTTDisconnect(aConnection, WSMessage.Disconnect.ReasonCode,
      WSMessage.Disconnect.ReasonName,
      WSMessage.Disconnect.DisconnectProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTAuth
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTAuth) then
    FOnMQTTAuth(aConnection, WSMessage.Auth.ReasonCode,
      WSMessage.Auth.ReasonName, WSMessage.Auth.AuthProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTPing
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTPing) then
    FOnMQTTPing(aConnection);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTPubAck
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTPubAck) then
    FOnMQTTPubAck(aConnection, WSMessage.PUBACK.PacketIdentifier,
      WSMessage.PUBACK.ReasonCode, WSMessage.PUBACK.ReasonName,
      WSMessage.PUBACK.PubAckProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTPubComp
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTPubComp) then
    FOnMQTTPubComp(aConnection, WSMessage.PUBCOMP.PacketIdentifier,
      WSMessage.PUBCOMP.ReasonCode, WSMessage.PUBCOMP.ReasonName,
      WSMessage.PUBCOMP.PubCompProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTPublish
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTPublish) then
    FOnMQTTPublish(aConnection, WSMessage.Publish.Topic, WSMessage.Publish.Text,
      WSMessage.Publish.PublishProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTPubRec
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTPubRec) then
    FOnMQTTPubRec(aConnection, WSMessage.PUBREC.PacketIdentifier,
      WSMessage.PUBREC.ReasonCode, WSMessage.PUBREC.ReasonName,
      WSMessage.PUBREC.PubRecProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTPubRel(aConnection:
    TsgcWSConnection);
begin
  if Assigned(FOnMQTTPubRel) then
    FOnMQTTPubRel(aConnection, WSMessage.PUBREL.PacketIdentifier,
      WSMessage.PUBREL.ReasonCode, WSMessage.PUBREL.ReasonName,
      WSMessage.PUBREL.PubRelProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTSubscribe
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTSubscribe) then
    FOnMQTTSubscribe(aConnection, WSMessage.SUBACK.PacketIdentifier,
      WSMessage.SUBACK.Codes, WSMessage.SUBACK.SUBACKProperties);
end;

procedure TsgcWSProtocol_MQTT_Client.DoEventMQTTUnSubscribe
  (aConnection: TsgcWSConnection);
begin
  if Assigned(FOnMQTTUnSubscribe) then
    FOnMQTTUnSubscribe(aConnection, WSMessage.UNSUBACK.PacketIdentifier,
      WSMessage.UNSUBACK.Codes, WSMessage.UNSUBACK.UNSUBACKProperties);
end;

function TsgcWSProtocol_MQTT_Client.DoKeepAlive: Boolean;
begin
{$IFNDEF D7}
{$IFNDEF D2006}
  inherited;
{$ENDIF}
{$ENDIF}
  if HeartBeat.Enabled then
  begin
    // ... timeout
    if HeartBeat.Timeout > 0 then
    begin
      if (FLastPing > FLastPong) and (SecondsBetWeen(FLastPing, FLastPong) >= HeartBeat.Timeout)  then
      begin
        if ((HeartBeat.Interval >= HeartBeat.Timeout) and (SecondsBetween(Now, FLastPing) >= HeartBeat.Timeout)
          or (HeartBeat.Timeout >= HeartBeat.Interval) and (SecondsBetween(Now, FFirstPing) >= HeartBeat.Timeout)) then
          raise Exception.Create(S_HEARTBEAT_TIMEOUT_EXCEEDED);
      end
      else
        FFirstPing := 0;
    end;
    // ... send ping
    Ping;
    if FFirstPing = 0 then
      FFirstPing := Now;
    FLastPing := Now;
    // ... result
    result := True;
  end
  else
    result := False;
end;

procedure TsgcWSProtocol_MQTT_Client.DoNotifyBinary(aConnection:
    TsgcWSConnection);
var
  oMessage: TsgcWSMQTTMessage;
  oData: TMemoryStream;
begin
  if FWaitResponse then
  begin
    oMessage := WSMessage;
    oData := TMemoryStream.Create;
    Try
      aConnection.MsgBinaryReceived.Position := 0;
      oData.CopyFrom(aConnection.MsgBinaryReceived, aConnection.MsgBinaryReceived.Size);
      oData.Position := 0;
      while oData.Position < oData.Size do
      begin
        if aConnection.Transport = trpTCP then
        begin
          if not oMessage.ReadTCP(oData) then
            exit;
        end
        else
          oMessage.Read(oData);

        case oMessage.FixedHeader.ControlPacket of
          mtcpPUBACK:
            FLastPUBACK := WSMessage.PUBACK.PacketIdentifier;
          mtcpPUBCOMP:
            FLastPUBCOMP := WSMessage.PUBCOMP.PacketIdentifier;
        end;
      end;
    Finally
      aConnection.MsgBinaryReceived.Position := 0;
      sgcFree(oData);
    End;
  end;

  inherited;
end;

procedure TsgcWSProtocol_MQTT_Client.DoPing;
begin
  DoWriteBytes(WSMessage.DoPing);
end;

procedure TsgcWSProtocol_MQTT_Client.DoPubAck(aPacketIdentifier: Word);
begin
  DoWriteBytes(WSMessage.DoPubAck(aPacketIdentifier));
end;

procedure TsgcWSProtocol_MQTT_Client.DoPubComp(aPacketIdentifier: Word);
begin
  DoWriteBytes(WSMessage.DoPubComp(aPacketIdentifier));
end;

procedure TsgcWSProtocol_MQTT_Client.DoPublish(aPacketIdentifier: Word;
  const aTopic, aText: String; aQoS: TmqttQoS; aRetain: Boolean;
  const aDup: Boolean; aPublishProperties: TsgcWSMQTTPublish_Properties = nil);
begin
  if not aDup then
    DoQueuePublish(aPacketIdentifier, aTopic, aText, aQoS, aRetain,
      aPublishProperties);

  if Assigned(aPublishProperties) then
  begin
    WSMessage.PublishProperties.Assign(aPublishProperties);
    WSMessage.PublishProperties.Enabled := True;
  end;

  DoWriteBytes(WSMessage.DoPublish(aPacketIdentifier, aTopic, aText, aQoS,
    aRetain, False));
end;

procedure TsgcWSProtocol_MQTT_Client.DoPublish(aPacketIdentifier: Word;
  const aTopic: String; aStream: TStream; aQoS: TmqttQoS; aRetain: Boolean;
  const aDup: Boolean; aPublishProperties: TsgcWSMQTTPublish_Properties = nil);
begin
  if not aDup then
    DoQueuePublish(aPacketIdentifier, aTopic, aStream, aQoS, aRetain,
      aPublishProperties);

  if Assigned(aPublishProperties) then
  begin
    WSMessage.PublishProperties.Assign(aPublishProperties);
    WSMessage.PublishProperties.Enabled := True;
  end;

  DoWriteBytes(WSMessage.DoPublish(aPacketIdentifier, aTopic, aStream, aQoS,
    aRetain, False));
end;

function TsgcWSProtocol_MQTT_Client.DoPublishAndWait(aPacketIdentifier: Word;
    const aTopic, aText: String; const aStream: TStream; aQoS: TmqttQoS;
    aRetain: Boolean; aPublishProperties: TsgcWSMQTTPublish_Properties = nil;
    const aTimeout: Integer = 10000): Boolean;
var
  vStart: Cardinal;
begin
  result := False;
  FLastPUBACK := 0;
  FLastPUBCOMP := 0;
  FWaitResponse := True;
  Try
    // ... set Dup to true to avoid queue these requests
    if Assigned(aStream) then
      DoPublish(aPacketIdentifier, aTopic, aStream, aQoS, aRetain, True,
        aPublishProperties)
    else
      DoPublish(aPacketIdentifier, aTopic, aText, aQoS, aRetain, True,
        aPublishProperties);

    if aQoS = mtqsAtMostOnce then
    begin
      result := True;
      exit;
    end
    else
    begin
      vStart := sgcGetTicks;
      repeat
        if (aQoS = mtqsAtLeastOnce) and (aPacketIdentifier = FLastPUBACK) then
        begin
          result := True;
          break;
        end
        else if (aQoS = mtqsExactlyOnce) and (aPacketIdentifier = FLastPUBCOMP)
        then
        begin
          result := True;
          break;
        end;
        sleep(1);
      until
        sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeout);
    end;
  Finally
    FWaitResponse := False;
  End;
end;

procedure TsgcWSProtocol_MQTT_Client.DoPubRec(aPacketIdentifier: Word);
begin
  DoWriteBytes(WSMessage.DoPubRec(aPacketIdentifier));
end;

procedure TsgcWSProtocol_MQTT_Client.DoPubRel(aPacketIdentifier: Word);
begin
  DoWriteBytes(WSMessage.DoPubRel(aPacketIdentifier));
end;

procedure TsgcWSProtocol_MQTT_Client.DoQueuePublish(const aPacketIdentifier
  : Word; const aTopic: String; aStream: TStream; aQoS: TmqttQoS;
  aRetain: Boolean; aPublishProperties: TsgcWSMQTTPublish_Properties);
var
  oItem: TsgcWSMQTTQoSItem;
begin
  if aPacketIdentifier = 0 then
    exit;

  case aQoS of
    mtqsAtLeastOnce, mtqsExactlyOnce:
      begin
        DoEnterCS(CS_QOS_LIST);
        Try
          oItem := TsgcWSMQTTQoSItem.Create;
          oItem.PacketIdentifier := aPacketIdentifier;
          oItem.Topic := aTopic;
          oItem.Stream := aStream;
          oItem.QoS := aQoS;
          oItem.Retain := aRetain;
          oItem.Date := Now;
          oItem.ItemType := mqttStream;
          oItem.PublishProperties := aPublishProperties;
          QoSList.Add(oItem)
        Finally
          DoLeaveCS(CS_QOS_LIST);
        End;
      end;
  end;
end;

procedure TsgcWSProtocol_MQTT_Client.DoSubscribe(aPacketIdentifier: Integer;
  aTopics: TsgcWSTopics; const aSubscribeProperties
  : TsgcWSMQTTSubscribe_Properties);
begin
  if Assigned(aSubscribeProperties) then
  begin
    WSMessage.SubscribeProperties := aSubscribeProperties;
    WSMessage.SubscribeProperties.Enabled := True;
  end;

  DoWriteBytes(WSMessage.DoSubscribe(aPacketIdentifier, aTopics));
end;

procedure TsgcWSProtocol_MQTT_Client.DoUnSubscribe(aPacketIdentifier: Integer;
  aTopics: TsgcWSTopics);
begin
  DoWriteBytes(WSMessage.DoUnSubscribe(aPacketIdentifier, aTopics));
end;

procedure TsgcWSProtocol_MQTT_Client.DoWriteBytes(const aBytes: TBytes);
var
  oStream: TMemoryStream;
begin
  DoEnterCS;
  Try
    oStream := TMemoryStream.Create;
    Try
      oStream.Write(aBytes[0], Length(aBytes));
      oStream.Position := 0;
      GetWSClient.WriteData(oStream);
    Finally
      sgcFree(oStream);
    End;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSProtocol_MQTT_Client.DoWriteMessageText;
begin
  inherited WriteData(GetWSMessageText);
end;

function TsgcWSProtocol_MQTT_Client.GetQoSList: TsgcWSMQTTQoSList;
begin
  if not Assigned(FQoSList) then
    FQoSList := TsgcWSMQTTQoSList.Create;
  result := FQoSList;
end;

function TsgcWSProtocol_MQTT_Client.GetWSMessageText: String;
begin

end;

procedure TsgcWSProtocol_MQTT_Client.OnQoSEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  DoQoSList;
end;

procedure TsgcWSProtocol_MQTT_Client.SetQoS(const Value: TsgcWSMQTTQoS_Options);
begin
  FQoS.Assign(Value);
end;

procedure TsgcWSProtocol_MQTT_Client.WriteData(const aText: String);
begin

end;

procedure TsgcWSProtocol_MQTT_Client.DoWriteRawData(const aText: String);
begin
  inherited WriteData(aText);
end;

function TsgcWSProtocol_MQTT_Client.GetPacketIdentifier: Word;
begin
  if FPacketIdentifier < High(Word) then
    FPacketIdentifier := FPacketIdentifier + 1
  else
    FPacketIdentifier := 1;

  result := FPacketIdentifier;
end;

function TsgcWSProtocol_MQTT_Client.GetWSMessageByConnection(const aConnection
  : TsgcWSConnection): TsgcWSMQTTMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSMQTTMessage.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    result := TsgcWSMQTTMessage(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_MQTT_Client.OnQoSExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoEventException(FWSConnection, E.message, E);
end;

procedure TsgcWSProtocol_MQTT_Client.Ping;
begin
  inherited;
  DoPing;
end;

function TsgcWSProtocol_MQTT_Client.Publish(const aTopic, aText: String;
  aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False;
  const aPublishProperties: TsgcWSMQTTPublish_Properties = nil): Word;
begin
  result := GetPacketIdentifier;

  DoPublish(result, aTopic, aText, aQoS, aRetain, False, aPublishProperties);
end;

function TsgcWSProtocol_MQTT_Client.Publish(const aTopic: String;
  const aStream: TStream; aQoS: TmqttQoS = mtqsAtMostOnce;
  aRetain: Boolean = False;
  const aPublishProperties: TsgcWSMQTTPublish_Properties = nil): Word;
begin
  result := GetPacketIdentifier;

  DoPublish(result, aTopic, aStream, aQoS, aRetain, False, aPublishProperties);
end;

function TsgcWSProtocol_MQTT_Client.PublishAndWait(const aTopic, aText: String;
  aQoS: TmqttQoS = mtqsAtMostOnce; aRetain: Boolean = False;
  const aPublishProperties: TsgcWSMQTTPublish_Properties = nil;
  const aTimeout: Integer = 10000): Boolean;
begin
  result := DoPublishAndWait(GetPacketIdentifier, aTopic, aText, nil, aQoS,
    aRetain, aPublishProperties, aTimeout);
end;

function TsgcWSProtocol_MQTT_Client.PublishAndWait(const aTopic: String;
  const aStream: TStream; aQoS: TmqttQoS = mtqsAtMostOnce;
  aRetain: Boolean = False;
  const aPublishProperties: TsgcWSMQTTPublish_Properties = nil;
  const aTimeout: Integer = 10000): Boolean;
begin
  result := DoPublishAndWait(GetPacketIdentifier, aTopic, '', aStream, aQoS,
    aRetain, aPublishProperties, aTimeout);
end;

procedure TsgcWSProtocol_MQTT_Client.SetAuthentication
  (const Value: TsgcWSMQTTAuthentication_Options);
begin
  FAuthentication.Assign(Value);
end;

procedure TsgcWSProtocol_MQTT_Client.SetConnectProperties
  (const Value: TsgcWSMQTTConnect_Properties);
begin
  FConnectProperties.Assign(Value);
end;

procedure TsgcWSProtocol_MQTT_Client.SetHeartBeat
  (const Value: TsgcWSMQTTHeartBeat_Options);
begin
  FHeartBeat.Assign(Value);
end;

procedure TsgcWSProtocol_MQTT_Client.SetLastWillTestament
  (const Value: TsgcWSMQTTLWT_Options);
begin
  FLastWillTestament.Assign(Value);
end;

procedure TsgcWSProtocol_MQTT_Client.SetMQTTVersion
  (const Value: TwsMQTTVersion);
begin
  FMQTTVersion := Value;
end;

function TsgcWSProtocol_MQTT_Client.Subscribe(const aTopic: String;
  aQoS: TmqttQoS = mtqsAtMostOnce;
  const aSubscribeProperties: TsgcWSMQTTSubscribe_Properties = nil): Word;
var
  oTopic: TsgcWSTopic;
  oTopics: TsgcWSTopics;
begin
  result := GetPacketIdentifier;

  oTopics := TsgcWSTopics.Create;
  Try
    oTopic := TsgcWSTopic.Create;
    oTopic.Topic := aTopic;
    oTopic.QoS := aQoS;
    oTopics.Add(oTopic);

    DoSubscribe(result, oTopics, aSubscribeProperties);
  Finally
    sgcFree(oTopics);
  End;
end;

function TsgcWSProtocol_MQTT_Client.Subscribe(aTopics: TsgcWSTopics): Word;
begin
  result := GetPacketIdentifier;

  DoSubscribe(result, aTopics, nil);
end;

function TsgcWSProtocol_MQTT_Client.UnSubscribe(const aTopic: string;
  aUnsubscribeProperties: TsgcWSMQTTUnsubscribe_Properties = nil): Word;
var
  oTopic: TsgcWSTopic;
  oTopics: TsgcWSTopics;
begin
  result := GetPacketIdentifier;

  oTopics := TsgcWSTopics.Create;
  Try
    oTopic := TsgcWSTopic.Create;
    oTopic.Topic := aTopic;
    oTopics.Add(oTopic);

    if Assigned(aUnsubscribeProperties) then
    begin
      WSMessage.UnsubscribeProperties.Assign(aUnsubscribeProperties);
      WSMessage.UnsubscribeProperties.Enabled := True;
    end;
    DoUnSubscribe(result, oTopics);
  Finally
    sgcFree(oTopics);
  End;
end;

function TsgcWSProtocol_MQTT_Client.UnSubscribe(aTopics: TsgcWSTopics): Word;
begin
  result := GetPacketIdentifier;

  DoUnSubscribe(result, aTopics);
end;

procedure TsgcWSMQTTQoS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTQoS_Options then
  begin
    Level := TsgcWSMQTTQoS_Options(aSource).Level;
    Interval := TsgcWSMQTTQoS_Options(aSource).Interval;
    Timeout := TsgcWSMQTTQoS_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSMQTTLWT_Options.Create;
begin
  inherited;
  Enabled := False;
  Topic := '';
  Text := '';
  QoS := mtqsAtMostOnce;
  Retain := False;
  FWillProperties := TsgcWSMQTTWill_Properties.Create;
end;

destructor TsgcWSMQTTLWT_Options.Destroy;
begin
  sgcFree(FWillProperties);
  inherited;
end;

procedure TsgcWSMQTTLWT_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTLWT_Options then
  begin
    Enabled := TsgcWSMQTTLWT_Options(aSource).Enabled;
    Topic := TsgcWSMQTTLWT_Options(aSource).Topic;
    Text := TsgcWSMQTTLWT_Options(aSource).Text;
    QoS := TsgcWSMQTTLWT_Options(aSource).QoS;
    Retain := TsgcWSMQTTLWT_Options(aSource).Retain;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSMQTTLWT_Options.SetWillProperties
  (const Value: TsgcWSMQTTWill_Properties);
begin
  if Assigned(FWillProperties) then
    FWillProperties.Assign(Value);
end;

constructor TsgcWSMQTTAuthentication_Options.Create;
begin
  inherited;
  Enabled := False;
  UserName := '';
  Password := '';
end;

procedure TsgcWSMQTTAuthentication_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTAuthentication_Options then
  begin
    Enabled := TsgcWSMQTTAuthentication_Options(aSource).Enabled;
    UserName := TsgcWSMQTTAuthentication_Options(aSource).UserName;
    Password := TsgcWSMQTTAuthentication_Options(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSMQTTHeartBeat_Options.Create;
begin
  inherited;
  Enabled := True;
  Interval := 300;
  Timeout := 0;
end;

procedure TsgcWSMQTTHeartBeat_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTHeartBeat_Options then
  begin
    Enabled := TsgcWSMQTTHeartBeat_Options(aSource).Enabled;
    Interval := TsgcWSMQTTHeartBeat_Options(aSource).Interval;
    Timeout := TsgcWSMQTTHeartBeat_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSMQTTQoSItem.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
  FPublishProperties := TsgcWSMQTTPublish_Properties.Create;
end;

destructor TsgcWSMQTTQoSItem.Destroy;
begin
  sgcFree(FPublishProperties);
  sgcFree(FStream);
  inherited;
end;

procedure TsgcWSMQTTQoSItem.SetItemType(const Value: TsgcWSMQTTItemType);
begin
  FItemType := Value;
end;

procedure TsgcWSMQTTQoSItem.SetPublishProperties(const Value
  : TsgcWSMQTTPublish_Properties);
begin
  if Assigned(Value) then
    FPublishProperties.Assign(Value)
  else
    sgcFree(FPublishProperties);
end;

procedure TsgcWSMQTTQoSItem.SetStream(const Value: TStream);
begin
  FStream.CopyFrom(Value, Value.Size);
  Value.Position := 0;
  FStream.Position := 0;
end;

{$ENDIF}

end.
