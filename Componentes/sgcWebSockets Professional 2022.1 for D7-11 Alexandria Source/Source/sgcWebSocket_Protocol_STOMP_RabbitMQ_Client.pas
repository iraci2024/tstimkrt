{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_STOMP_RabbitMQ_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Protocol_STOMP_Broker_Client, sgcSTOMP,
  sgcWebSocket_Const;

type

  TsgcWSRabbitMQSTOMPHeadersConnected = class;
  TsgcWSRabbitMQSTOMPHeadersMessage = class;
  TsgcWSRabbitMQSTOMPHeadersError = class;
  TsgcWSRabbitMQSTOMPHeadersReceipt = class;

  TsgcWSRabbitMQSTOMPConnectedEvent = procedure(Connection: TsgcWSConnection;
    Headers: TsgcWSRabbitMQSTOMPHeadersConnected) of object;
  TsgcWSRabbitMQSTOMPMessageEvent = procedure(Connection: TsgcWSConnection;
    MessageText: String; Headers: TsgcWSRabbitMQSTOMPHeadersMessage;
    Subscription: TsgcWSBrokerSTOMPSubscriptionItem) of object;
  TsgcWSRabbitMQSTOMPErrorEvent = procedure(Connection: TsgcWSConnection;
    MessageText: String; Headers: TsgcWSRabbitMQSTOMPHeadersError) of object;
  TsgcWSRabbitMQSTOMPReceiptEvent = procedure(Connection: TsgcWSConnection;
    Headers: TsgcWSRabbitMQSTOMPHeadersReceipt) of object;
  TsgcWSRabbitMQDisconnectedEvent = procedure(Connection: TsgcWSConnection; Code:
      Integer) of object;

  TsgcWSRabbitMQSTOMPHeadersConnected = class(TsgcWSBrokerSTOMPHeadersConnected)
  end;

  TsgcWSRabbitMQSTOMPHeadersMessage = class(TsgcWSBrokerSTOMPHeadersMessage)
  end;

  TsgcWSRabbitMQSTOMPHeadersError = class(TsgcWSBrokerSTOMPHeadersError)
  end;

  TsgcWSRabbitMQSTOMPHeadersReceipt = class(TsgcWSBrokerSTOMPHeadersReceipt)
  end;

  TsgcWSRabbitMQSTOMP_Queue_Options = class(TPersistent)
  private
    FDeadLetterExchange: string;
    FDeadLetterRoutingKey: String;
    FExpires: Integer;
    FMaxLength: Integer;
    FMaxLengthBytes: Integer;
    FMaxPriority: Integer;
    FMessageTTL: Integer;
  private
    FHeaders: TStringList;
    procedure DoHeaders;
  protected
    function GetHeaders: TStringList;
  public
    procedure Clear;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  public
    property MessageTTL: Integer read FMessageTTL write FMessageTTL;
    property Expires: Integer read FExpires write FExpires;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property MaxLengthBytes: Integer read FMaxLengthBytes write FMaxLengthBytes;
    property DeadLetterExchange
      : string read FDeadLetterExchange write FDeadLetterExchange;
    property DeadLetterRoutingKey
      : String read FDeadLetterRoutingKey write FDeadLetterRoutingKey;
    property MaxPriority: Integer read FMaxPriority write FMaxPriority;
  end;

  TsgcWSProtocol_STOMP_RabbitMQ_Client = class
    (TsgcWSProtocol_STOMP_Broker_Client)
    { from TsgcWSProtocol_STOMP_Broker_Client }
  protected
    procedure DoBrokerConnectEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); override;
    procedure DoBrokerMessageEvent(aConnection: TsgcWSConnection;
      aMessageText, aRawHeaders: String;
      aSubscription: TsgcWSBrokerSTOMPSubscriptionItem); override;
    procedure DoBrokerErrorEvent(aConnection: TsgcWSConnection;
      aMessageText: String; aRawHeaders: String); override;
    procedure DoBrokerReceiptEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); override;
    procedure DoBrokerDisconnectedEvent(aConnection: TsgcWSConnection; aCode:
        Integer); override;
    { from TsgcWSProtocol_STOMP_Broker_Client }

    { queue }
  private
    FQueue: TsgcWSRabbitMQSTOMP_Queue_Options;
    procedure SetQueue(const Value: TsgcWSRabbitMQSTOMP_Queue_Options);
  public
    property Queue
      : TsgcWSRabbitMQSTOMP_Queue_Options read FQueue write SetQueue;
    { queue }

    { constructor /  destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor /  destructor }

    { methods }
  private
    FSubscriptionHeaders: TStringList;
    FSendHeaders: TStringList;
    function GetSubscriptionHeaders(const aDurable, aAutoDelete: Boolean;
      aExclusive: Boolean; aOptions: TsgcWSRabbitMQSTOMP_Queue_Options)
      : TStringList;
    function GetSendHeaders(const aReplyTo: String): TStringList;
  protected
    procedure DoSubscribe(const aDestination: string;
      const aDurable, aAutoDelete, aExclusive: Boolean;
      const aACK: TsgcSTOMPACK; const aOptions
        : TsgcWSRabbitMQSTOMP_Queue_Options); virtual;
    procedure DoUnSubscribe(const aDestination: String); virtual;
    procedure DoPublish(const aDestination, aText, aContentType, aTransaction,
      aReplyTo: String);
  public
    procedure SubscribeEx(const aDestination: String;
      const aDurable: Boolean = True; const aAutoDelete: Boolean = False;
      const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
    procedure PublishEx(const aDestination, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '');
    procedure UnSubscribeEx(const aDestination: String);
  public
    procedure SubscribeTopic(const aTopic: String;
      const aDurable: Boolean = True; const aAutoDelete: Boolean = False;
      const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeTopic(const aTopic: String);
    procedure PublishTopic(const aTopic, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '');
  public
    procedure SubscribeQueue(const aQueue: String;
      const aDurable: Boolean = True; const aAutoDelete: Boolean = False;
      const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeQueue(const aQueue: String);
    procedure PublishQueue(const aQueue, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '');
  public
    procedure SubscribeQueueOutside(const aQueue: String;
      const aDurable: Boolean = True; const aAutoDelete: Boolean = False;
      const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeQueueOutside(const aQueue: String);
    procedure PublishQueueOutside(const aQueue, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '');
  public
    procedure SubscribeTemporaryQueue(const aQueue, aReplyTo: String;
      const aDurable: Boolean = True; const aAutoDelete: Boolean = False;
      const aExclusive: Boolean = False; const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeTemporaryQueue(const aQueue: String);
    procedure PublishTemporaryQueue(const aQueue, aReplyTo, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '');
  public
    procedure SubscribeExchange(const aName, aPattern: String;
      const aDurable: Boolean = True; const aAutoDelete: Boolean = False;
      const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeExchange(const aName, aPattern: String);
    procedure PublishExchange(const aName, aRoutingKey, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '');
    { methods }

    { events }
  private
    FOnRabbitMQConnected: TsgcWSRabbitMQSTOMPConnectedEvent;
    FOnRabbitMQDisconnected: TsgcWSRabbitMQDisconnectedEvent;
    FOnRabbitMQError: TsgcWSRabbitMQSTOMPErrorEvent;
    FOnRabbitMQMessage: TsgcWSRabbitMQSTOMPMessageEvent;
    FOnRabbitMQReceipt: TsgcWSRabbitMQSTOMPReceiptEvent;
  protected
    procedure DoRabbitMQConnectEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); virtual;
    procedure DoRabbitMQMessageEvent(aConnection: TsgcWSConnection;
      aMessageText, aRawHeaders: String;
      aSubscription: TsgcWSBrokerSTOMPSubscriptionItem); virtual;
    procedure DoRabbitMQErrorEvent(aConnection: TsgcWSConnection;
      aMessageText: String; aRawHeaders: String); virtual;
    procedure DoRabbitMQReceiptEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); virtual;
    procedure DoRabbitMQDisconnectedEvent(aConnection: TsgcWSConnection; aCode:
        Integer); virtual;
  public
    property OnRabbitMQConnected
      : TsgcWSRabbitMQSTOMPConnectedEvent read FOnRabbitMQConnected write
      FOnRabbitMQConnected;
    property OnRabbitMQMessage
      : TsgcWSRabbitMQSTOMPMessageEvent read FOnRabbitMQMessage write
      FOnRabbitMQMessage;
    property OnRabbitMQError
      : TsgcWSRabbitMQSTOMPErrorEvent read FOnRabbitMQError write
      FOnRabbitMQError;
    property OnRabbitMQReceipt
      : TsgcWSRabbitMQSTOMPReceiptEvent read FOnRabbitMQReceipt write
      FOnRabbitMQReceipt;
    property OnRabbitMQDisconnected: TsgcWSRabbitMQDisconnectedEvent read
        FOnRabbitMQDisconnected write FOnRabbitMQDisconnected;
    { events }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Helpers;

Const
  CS_STOMP_RABBITMQ_EXCHANGE = '/exchange';
  CS_STOMP_RABBITMQ_QUEUE = '/queue';
  CS_STOMP_RABBITMQ_QUEUE_OUTSIDE = '/amq/queue';
  CS_STOMP_RABBITMQ_DURABLE_TOPIC = '/topic';
  CS_STOMP_RABBITMQ_TEMPORARY_QUEUE = '/temp-queue';

Const
  CS_STOMP_RABBITMQ_DURABLE = 'durable';
  CS_STOMP_RABBITMQ_AUTO_DELETE = 'auto-delete';
  CS_STOMP_RABBITMQ_EXCLUSIVE = 'exclusive';
  CS_STOMP_RABBITMQ_REPLY_TO = 'reply-to';

constructor TsgcWSProtocol_STOMP_RabbitMQ_Client.Create(aOwner: TComponent);
begin
  inherited;
  FQueue := TsgcWSRabbitMQSTOMP_Queue_Options.Create;
end;

destructor TsgcWSProtocol_STOMP_RabbitMQ_Client.Destroy;
begin
  sgcFree(FSubscriptionHeaders);
  sgcFree(FSendHeaders);
  sgcFree(FQueue);
  inherited;
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoBrokerConnectEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
begin
  inherited;
  DoRabbitMQConnectEvent(aConnection, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoBrokerDisconnectedEvent(
    aConnection: TsgcWSConnection; aCode: Integer);
begin
  inherited;
  DoRabbitMQDisconnectedEvent(aConnection, aCode);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoBrokerErrorEvent
  (aConnection: TsgcWSConnection; aMessageText: String; aRawHeaders: String);
begin
  inherited;
  DoRabbitMQErrorEvent(aConnection, aMessageText, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoBrokerMessageEvent
  (aConnection: TsgcWSConnection; aMessageText, aRawHeaders: String;
  aSubscription: TsgcWSBrokerSTOMPSubscriptionItem);
begin
  inherited;
  DoRabbitMQMessageEvent(aConnection, aMessageText, aRawHeaders, aSubscription);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoBrokerReceiptEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
begin
  inherited;
  DoRabbitMQReceiptEvent(aConnection, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoPublish(const aDestination,
  aText, aContentType, aTransaction, aReplyTo: String);
begin
  Send(aDestination, aText, aContentType, aTransaction,
    GetSendHeaders(aReplyTo));
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoRabbitMQConnectEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
var
  oHeaders: TsgcWSRabbitMQSTOMPHeadersConnected;
begin
  if Assigned(FOnRabbitMQConnected) then
  begin
    oHeaders := TsgcWSRabbitMQSTOMPHeadersConnected.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnRabbitMQConnected(aConnection, oHeaders);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoRabbitMQDisconnectedEvent(
    aConnection: TsgcWSConnection; aCode: Integer);
begin
  if Assigned(FOnRabbitMQDisconnected) then
    FOnRabbitMQDisconnected(aConnection, aCode);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoRabbitMQErrorEvent
  (aConnection: TsgcWSConnection; aMessageText: String; aRawHeaders: String);
var
  oHeaders: TsgcWSRabbitMQSTOMPHeadersError;
begin
  if Assigned(FOnRabbitMQError) then
  begin
    oHeaders := TsgcWSRabbitMQSTOMPHeadersError.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnRabbitMQError(aConnection, aMessageText, oHeaders);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoRabbitMQMessageEvent
  (aConnection: TsgcWSConnection; aMessageText, aRawHeaders: String;
  aSubscription: TsgcWSBrokerSTOMPSubscriptionItem);
var
  oHeaders: TsgcWSRabbitMQSTOMPHeadersMessage;
begin
  if Assigned(FOnRabbitMQMessage) then
  begin
    oHeaders := TsgcWSRabbitMQSTOMPHeadersMessage.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnRabbitMQMessage(aConnection, aMessageText, oHeaders, aSubscription);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoRabbitMQReceiptEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
var
  oHeaders: TsgcWSRabbitMQSTOMPHeadersReceipt;
begin
  if Assigned(FOnRabbitMQReceipt) then
  begin
    oHeaders := TsgcWSRabbitMQSTOMPHeadersReceipt.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnRabbitMQReceipt(aConnection, oHeaders);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoSubscribe
  (const aDestination: string; const aDurable, aAutoDelete,
  aExclusive: Boolean; const aACK: TsgcSTOMPACK;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options);
var
  vId: String;
  vOptions: TsgcWSRabbitMQSTOMP_Queue_Options;
  oHeaders: TStrings;
  vDestination: String;
begin
  vOptions := aOptions;
  if not Assigned(vOptions) then
    vOptions := Queue;

  vId := GetNewId;
  vDestination := aDestination;
  oHeaders := GetSubscriptionHeaders(aDurable, aAutoDelete, aExclusive,
    vOptions);

  Subscribe(vId, vDestination, aACK, oHeaders);
  DoQueueSubscription(vId, vDestination, aACK, oHeaders);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.DoUnSubscribe
  (const aDestination: String);
var
  oSubscription: TsgcWSBrokerSTOMPSubscriptionItem;
  vDurable, vAutoDelete, vExclusive: Boolean;
begin
  oSubscription := GetSubscriptionByName(aDestination);
  if Assigned(oSubscription) then
  begin
    vDurable := oSubscription.Headers.IndexOf
      (CS_STOMP_RABBITMQ_DURABLE + ':true') <> -1;
    vAutoDelete := oSubscription.Headers.IndexOf
      (CS_STOMP_RABBITMQ_AUTO_DELETE + ':true') <> -1;
    vExclusive := oSubscription.Headers.IndexOf
      (CS_STOMP_RABBITMQ_EXCLUSIVE + ':true') <> -1;

    UnSubscribe(oSubscription.Id, GetSubscriptionHeaders(vDurable, vAutoDelete,
        vExclusive, nil));
    DoDeleteSubscription(oSubscription.Id);
  end;
end;

function TsgcWSProtocol_STOMP_RabbitMQ_Client.GetSubscriptionHeaders
  (const aDurable, aAutoDelete: Boolean; aExclusive: Boolean;
  aOptions: TsgcWSRabbitMQSTOMP_Queue_Options): TStringList;

  procedure DoAddHeader(const aName: String; aValue: Boolean);
  var
    vValue: String;
  begin
    vValue := 'false';
    if aValue then
      vValue := 'true';
    FSubscriptionHeaders.Add(aName + ':' + vValue);
  end;

var
  i: Integer;
  oOptions: TStringList;
begin
  if not Assigned(FSubscriptionHeaders) then
    FSubscriptionHeaders := TStringList.Create;
  Result := FSubscriptionHeaders;
  FSubscriptionHeaders.Clear;

  DoAddHeader(CS_STOMP_RABBITMQ_DURABLE, aDurable);
  DoAddHeader(CS_STOMP_RABBITMQ_AUTO_DELETE, aAutoDelete);
  DoAddHeader(CS_STOMP_RABBITMQ_EXCLUSIVE, aExclusive);

  if Assigned(aOptions) then
  begin
    oOptions := aOptions.GetHeaders;
    for i := 0 to oOptions.Count - 1 do
      FSubscriptionHeaders.Add(oOptions[i]);
  end;
end;

function TsgcWSProtocol_STOMP_RabbitMQ_Client.GetSendHeaders
  (const aReplyTo: String): TStringList;
begin
  if not Assigned(FSendHeaders) then
    FSendHeaders := TStringList.Create;
  Result := FSendHeaders;
  FSendHeaders.Clear;

  if aReplyTo <> '' then
    FSendHeaders.Add(CS_STOMP_RABBITMQ_REPLY_TO + ':' + aReplyTo);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.PublishEx(const aDestination,
  aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '');
begin
  DoPublish(aDestination, aText, aContentType, aTransaction, '');
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.PublishExchange(const aName,
  aRoutingKey, aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '');
var
  vDestination: String;
begin
  vDestination := CS_STOMP_RABBITMQ_EXCHANGE + '/' + aName;
  if aRoutingKey <> '' then
    vDestination := vDestination + '/' + aRoutingKey;

  DoPublish(vDestination, aText, aContentType, aTransaction, '');
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.PublishQueue(const aQueue,
  aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '');
begin
  DoPublish(CS_STOMP_RABBITMQ_QUEUE + '/' + aQueue, aText, aContentType,
    aTransaction, '');
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.PublishQueueOutside
  (const aQueue, aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '');
begin
  DoPublish(CS_STOMP_RABBITMQ_QUEUE_OUTSIDE + '/' + aQueue, aText,
    aContentType, aTransaction, '');
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.PublishTemporaryQueue
  (const aQueue, aReplyTo, aText: String;
  const aContentType: String = CS_TEXT_PLAIN; const aTransaction: String = '');
var
  oSubscription: TsgcWSBrokerSTOMPSubscriptionItem;
  vDestination: string;
  vReplyTo: string;
begin
  vDestination := CS_STOMP_RABBITMQ_QUEUE + '/' + aQueue;
  vReplyTo := CS_STOMP_RABBITMQ_TEMPORARY_QUEUE + '/' + aReplyTo;

  oSubscription := GetSubscriptionByName(vDestination);
  if Assigned(oSubscription) then
    oSubscription.ReplyTo := vReplyTo;

  DoPublish(vDestination, aText, aContentType, aTransaction, vReplyTo);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.PublishTopic(const aTopic,
  aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '');
begin
  DoPublish(CS_STOMP_RABBITMQ_DURABLE_TOPIC + '/' + aTopic, aText,
    aContentType, aTransaction, '');
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SetQueue
  (const Value: TsgcWSRabbitMQSTOMP_Queue_Options);
begin
  FQueue.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SubscribeEx
  (const aDestination: String; const aDurable: Boolean = True;
  const aAutoDelete: Boolean = False; const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(aDestination, aDurable, aAutoDelete, aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SubscribeExchange(const aName,
  aPattern: String; const aDurable: Boolean = True;
  const aAutoDelete: Boolean = False; const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
var
  vDestination: String;
begin
  vDestination := CS_STOMP_RABBITMQ_EXCHANGE + '/' + aName;
  if aPattern <> '' then
    vDestination := vDestination + '/' + aPattern;

  DoSubscribe(vDestination, aDurable, aAutoDelete, aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SubscribeQueue
  (const aQueue: String; const aDurable: Boolean = True;
  const aAutoDelete: Boolean = False; const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(CS_STOMP_RABBITMQ_QUEUE + '/' + aQueue, aDurable, aAutoDelete,
    aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SubscribeQueueOutside
  (const aQueue: String; const aDurable: Boolean = True;
  const aAutoDelete: Boolean = False; const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(CS_STOMP_RABBITMQ_QUEUE_OUTSIDE + '/' + aQueue, aDurable,
    aAutoDelete, aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SubscribeTemporaryQueue
  (const aQueue, aReplyTo: String; const aDurable: Boolean = True;
  const aAutoDelete: Boolean = False; const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(CS_STOMP_RABBITMQ_TEMPORARY_QUEUE + '/' + aQueue, aDurable,
    aAutoDelete, aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.SubscribeTopic
  (const aTopic: String; const aDurable: Boolean = True;
  const aAutoDelete: Boolean = False; const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSRabbitMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(CS_STOMP_RABBITMQ_DURABLE_TOPIC + '/' + aTopic, aDurable,
    aAutoDelete, aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.UnSubscribeEx
  (const aDestination: String);
begin
  DoUnSubscribe(aDestination);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.UnSubscribeExchange(const aName,
  aPattern: String);
var
  vDestination: String;
begin
  vDestination := CS_STOMP_RABBITMQ_EXCHANGE + '/' + aName;
  if aPattern <> '' then
    vDestination := vDestination + '/' + aPattern;

  DoUnSubscribe(vDestination);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.UnSubscribeQueue
  (const aQueue: String);
begin
  DoUnSubscribe(CS_STOMP_RABBITMQ_QUEUE + '/' + aQueue);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.UnSubscribeQueueOutside
  (const aQueue: String);
begin
  DoUnSubscribe(CS_STOMP_RABBITMQ_QUEUE_OUTSIDE + '/' + aQueue);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.UnSubscribeTemporaryQueue
  (const aQueue: String);
begin
  DoUnSubscribe(CS_STOMP_RABBITMQ_TEMPORARY_QUEUE + '/' + aQueue);
end;

procedure TsgcWSProtocol_STOMP_RabbitMQ_Client.UnSubscribeTopic
  (const aTopic: String);
begin
  DoUnSubscribe(CS_STOMP_RABBITMQ_DURABLE_TOPIC + '/' + aTopic);
end;

constructor TsgcWSRabbitMQSTOMP_Queue_Options.Create;
begin
  inherited;
  Clear;
end;

procedure TsgcWSRabbitMQSTOMP_Queue_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSRabbitMQSTOMP_Queue_Options then
  begin
    MessageTTL := TsgcWSRabbitMQSTOMP_Queue_Options(aSource).MessageTTL;
    Expires := TsgcWSRabbitMQSTOMP_Queue_Options(aSource).Expires;
    MaxLength := TsgcWSRabbitMQSTOMP_Queue_Options(aSource).MaxLength;
    MaxLengthBytes := TsgcWSRabbitMQSTOMP_Queue_Options(aSource).MaxLengthBytes;
    DeadLetterExchange := TsgcWSRabbitMQSTOMP_Queue_Options(aSource)
      .DeadLetterExchange;
    DeadLetterRoutingKey := TsgcWSRabbitMQSTOMP_Queue_Options(aSource)
      .DeadLetterRoutingKey;
    MaxPriority := TsgcWSRabbitMQSTOMP_Queue_Options(aSource).MaxPriority;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSRabbitMQSTOMP_Queue_Options.Clear;
begin
  FDeadLetterExchange := '';
  FDeadLetterRoutingKey := '';
  FExpires := 0;
  FMaxLength := 0;
  FMaxLengthBytes := 0;
  FMaxPriority := 0;
  FMessageTTL := 0;
end;

procedure TsgcWSRabbitMQSTOMP_Queue_Options.DoHeaders;
begin
  FHeaders.Clear;

  if MessageTTL <> 0 then
    FHeaders.Add('x-message-ttl:' + IntToStr(MessageTTL));
  if Expires <> 0 then
    FHeaders.Add('x-expires:' + IntToStr(Expires));
  if MaxLength <> 0 then
    FHeaders.Add('x-max-length:' + IntToStr(MaxLength));
  if MaxLengthBytes <> 0 then
    FHeaders.Add('x-max-length-bytes:' + IntToStr(MaxLengthBytes));
  if DeadLetterExchange <> '' then
    FHeaders.Add('x-dead-letter-exchange:' + DeadLetterExchange);
  if DeadLetterRoutingKey <> '' then
    FHeaders.Add('x-dead-letter-routing-key:' + DeadLetterRoutingKey);
  if MaxPriority <> 0 then
    FHeaders.Add('x-max-priority:' + IntToStr(MaxPriority));
end;

function TsgcWSRabbitMQSTOMP_Queue_Options.GetHeaders: TStringList;
begin
  if not Assigned(FHeaders) then
    FHeaders := TStringList.Create;
  DoHeaders;
  Result := FHeaders
end;
{$ENDIF}

end.
