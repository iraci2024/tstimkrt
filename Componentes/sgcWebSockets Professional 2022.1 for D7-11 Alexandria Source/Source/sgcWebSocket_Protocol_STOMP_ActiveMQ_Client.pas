{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_STOMP_ActiveMQ_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Protocol_STOMP_Broker_Client,
  sgcWebSocket_Const,
  sgcSTOMP;

type

  TsgcWSActiveMQSTOMPHeadersConnected = class;
  TsgcWSActiveMQSTOMPHeadersMessage = class;
  TsgcWSActiveMQSTOMPHeadersError = class;
  TsgcWSActiveMQSTOMPHeadersReceipt = class;

  TsgcWSActiveMQSTOMPConnectedEvent = procedure(Connection: TsgcWSConnection;
    Headers: TsgcWSActiveMQSTOMPHeadersConnected) of object;
  TsgcWSActiveMQSTOMPMessageEvent = procedure(Connection: TsgcWSConnection;
    MessageText: String; Headers: TsgcWSActiveMQSTOMPHeadersMessage;
    Subscription: TsgcWSBrokerSTOMPSubscriptionItem) of object;
  TsgcWSActiveMQSTOMPErrorEvent = procedure(Connection: TsgcWSConnection;
    MessageText: String; Headers: TsgcWSActiveMQSTOMPHeadersError) of object;
  TsgcWSActiveMQSTOMPReceiptEvent = procedure(Connection: TsgcWSConnection;
    Headers: TsgcWSActiveMQSTOMPHeadersReceipt) of object;
  TsgcWSActiveMQDisconnectedEvent = procedure(Connection: TsgcWSConnection; Code:
      Integer) of object;

  TsgcWSActiveMQSTOMPHeadersConnected = class(TsgcWSBrokerSTOMPHeadersConnected)
  end;

  TsgcWSActiveMQSTOMPHeadersMessage = class(TsgcWSBrokerSTOMPHeadersMessage)
  private
    FExpires: Integer;
    FPriority: Integer;
    FTimestamp: String;
  protected
    procedure DoRead; override;
  public
    property Expires: Integer read FExpires write FExpires;
    property Priority: Integer read FPriority write FPriority;
    property Timestamp: String read FTimestamp write FTimestamp;
  end;

  TsgcWSActiveMQSTOMPHeadersError = class(TsgcWSBrokerSTOMPHeadersError)
  end;

  TsgcWSActiveMQSTOMPHeadersReceipt = class(TsgcWSBrokerSTOMPHeadersReceipt)
  end;

  TsgcWSActiveMQSTOMP_Queue_Options = class(TPersistent)
  private
    FPriority: Byte;
    FSelector: String;
    FMaximumPendingMessageLimit: Integer;
    FNoLocal: Boolean;
    FPrefetchSize: Integer;
    FDispatchAsync: Boolean;
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
    property DispatchAsync: Boolean read FDispatchAsync write FDispatchAsync;
    property MaximumPendingMessageLimit
      : Integer read FMaximumPendingMessageLimit write
      FMaximumPendingMessageLimit;
    property NoLocal: Boolean read FNoLocal write FNoLocal;
    property PrefetchSize: Integer read FPrefetchSize write FPrefetchSize;
    property Priority: Byte read FPriority write FPriority;
    property Selector: String read FSelector write FSelector;
  end;

  TsgcWSActiveMQSTOMP_Message_Options = class(TPersistent)
  private
    FCorrelationId: String;
    FExpires: Integer;
    FJMSXGroupID: String;
    FJMSXGroupSeq: Integer;
    FMsgType: String;
    FPersistent: Boolean;
    FPriority: Integer;
    FReplyTo: String;
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
  published
    property CorrelationId: String read FCorrelationId write FCorrelationId;
    property Expires: Integer read FExpires write FExpires;
    property JMSXGroupID: String read FJMSXGroupID write FJMSXGroupID;
    property JMSXGroupSeq: Integer read FJMSXGroupSeq write FJMSXGroupSeq;
    property MsgType: String read FMsgType write FMsgType;
    property Persistent: Boolean read FPersistent write FPersistent;
    property Priority: Integer read FPriority write FPriority;
    property ReplyTo: String read FReplyTo write FReplyTo;
  end;

  TsgcWSActiveMQSTOMP_Options = class(TPersistent)
  private
    FClientId: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ClientId: String read FClientId write FClientId;
  end;

  TsgcWSProtocol_STOMP_ActiveMQ_Client = class
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
  protected
    procedure DoConnect(const aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol_STOMP_Broker_Client }

    { queue }
  private
    FQueue: TsgcWSActiveMQSTOMP_Queue_Options;
    procedure SetQueue(const Value: TsgcWSActiveMQSTOMP_Queue_Options);
  public
    property Queue
      : TsgcWSActiveMQSTOMP_Queue_Options read FQueue write SetQueue;
    { queue }

    { ActiveMQ_Options }
  private
    FActiveMQ_Options: TsgcWSActiveMQSTOMP_Options;
    procedure SetActiveMQ_Options(const Value: TsgcWSActiveMQSTOMP_Options);
  public
    property ActiveMQ_Options
      : TsgcWSActiveMQSTOMP_Options read FActiveMQ_Options write
      SetActiveMQ_Options;
    { ActiveMQ_Options }

    { constructor /  destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor /  destructor }

    { methods }
  private
    FSubscriptionHeaders: TStringList;
    FSendHeaders: TStringList;
    function GetSubscriptionHeaders(const aDurable: Boolean;
      aExclusive: Boolean; aOptions: TsgcWSActiveMQSTOMP_Queue_Options)
      : TStringList;
  protected
    procedure DoSubscribe(const aDestination: string;
      const aDurable, aExclusive: Boolean; const aACK: TsgcSTOMPACK;
      const aOptions: TsgcWSActiveMQSTOMP_Queue_Options); virtual;
    procedure DoUnSubscribe(const aDestination: String); virtual;
    procedure DoPublish(const aDestination, aText, aContentType,
      aTransaction: String;
      const aOptions
        : TsgcWSActiveMQSTOMP_Message_Options);
  public
    procedure SubscribeEx(const aDestination: String;
      const aDurable: Boolean = True; const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSActiveMQSTOMP_Queue_Options = nil);
    procedure PublishEx(const aDestination, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '';
      const aOptions: TsgcWSActiveMQSTOMP_Message_Options = nil);
    procedure UnSubscribeEx(const aDestination: String);
  public
    procedure SubscribeTopic(const aTopic: String;
      const aDurable: Boolean = True; const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSActiveMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeTopic(const aTopic: String);
    procedure PublishTopic(const aTopic, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '';
      const aOptions: TsgcWSActiveMQSTOMP_Message_Options = nil);
  public
    procedure SubscribeQueue(const aQueue: String;
      const aDurable: Boolean = True; const aExclusive: Boolean = False;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aOptions: TsgcWSActiveMQSTOMP_Queue_Options = nil);
    procedure UnSubscribeQueue(const aQueue: String);
    procedure PublishQueue(const aQueue, aText: String;
      const aContentType: String = CS_TEXT_PLAIN;
      const aTransaction: String = '';
      const aOptions: TsgcWSActiveMQSTOMP_Message_Options = nil);
    { methods }

    { events }
  private
    FOnActiveMQConnected: TsgcWSActiveMQSTOMPConnectedEvent;
    FOnActiveMQDisconnected: TsgcWSActiveMQDisconnectedEvent;
    FOnActiveMQError: TsgcWSActiveMQSTOMPErrorEvent;
    FOnActiveMQMessage: TsgcWSActiveMQSTOMPMessageEvent;
    FOnActiveMQReceipt: TsgcWSActiveMQSTOMPReceiptEvent;
  protected
    procedure DoActiveMQConnectEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); virtual;
    procedure DoActiveMQMessageEvent(aConnection: TsgcWSConnection;
      aMessageText, aRawHeaders: String;
      aSubscription: TsgcWSBrokerSTOMPSubscriptionItem); virtual;
    procedure DoActiveMQErrorEvent(aConnection: TsgcWSConnection;
      aMessageText: String; aRawHeaders: String); virtual;
    procedure DoActiveMQReceiptEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); virtual;
    procedure DoActiveMQDisconnectedEvent(aConnection: TsgcWSConnection; aCode:
        Integer); virtual;
  public
    property OnActiveMQConnected
      : TsgcWSActiveMQSTOMPConnectedEvent read FOnActiveMQConnected write
      FOnActiveMQConnected;
    property OnActiveMQMessage
      : TsgcWSActiveMQSTOMPMessageEvent read FOnActiveMQMessage write
      FOnActiveMQMessage;
    property OnActiveMQError
      : TsgcWSActiveMQSTOMPErrorEvent read FOnActiveMQError write
      FOnActiveMQError;
    property OnActiveMQReceipt
      : TsgcWSActiveMQSTOMPReceiptEvent read FOnActiveMQReceipt write
      FOnActiveMQReceipt;
    property OnActiveMQDisconnected: TsgcWSActiveMQDisconnectedEvent read
        FOnActiveMQDisconnected write FOnActiveMQDisconnected;
    { events }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Helpers;

Const
  CS_STOMP_ACTIVEMQ_QUEUE = '/queue';
  CS_STOMP_ACTIVEMQ_DURABLE_TOPIC = '/topic';

Const
  CS_STOMP_ACTIVEMQ_RETROACTIVE = 'retroactive';
  CS_STOMP_ACTIVEMQ_EXCLUSIVE = 'activemq.exclusive';

Const
  CS_STOMP_ACTIVEMQ_CLIENT_ID = 'activemq.client-id';

Const
  CS_STOMP_ACTIVEMQ_HEADER_EXPIRES = 'expires';
  CS_STOMP_ACTIVEMQ_HEADER_PRIORITY = 'priority';
  CS_STOMP_ACTIVEMQ_HEADER_TIMESTAMP = 'timestamp';

Const
  CS_STOMP_ACTIVEMQ_HEADERS_CORRELATION_ID = 'correlation-id';
  CS_STOMP_ACTIVEMQ_HEADERS_EXPIRES = 'expires';
  CS_STOMP_ACTIVEMQ_HEADERS_JMSXGROUPID = 'JMSXGroupID';
  CS_STOMP_ACTIVEMQ_HEADERS_JMSXGROUPSEQ = 'JMSXGroupSeq';
  CS_STOMP_ACTIVEMQ_HEADERS_PERSISTENT = 'persistent';
  CS_STOMP_ACTIVEMQ_HEADERS_PRIORITY = 'priority';
  CS_STOMP_ACTIVEMQ_HEADERS_REPLY_TO = 'reply-to';
  CS_STOMP_ACTIVEMQ_HEADERS_TYPE = 'type';

Const
  CS_STOMP_ACTIVEMQ_HEADERS_DISPATCHASYNC = 'activemq.dispatchAsync';
  CS_STOMP_ACTIVEMQ_HEADERS_MAXIMUMPENDINGMESSAGELIMIT =
    'activemq.maximumPendingMessageLimit';
  CS_STOMP_ACTIVEMQ_HEADERS_NOLOCAL = 'activemq.noLocal';
  CS_STOMP_ACTIVEMQ_HEADERS_PREFETCHSIZE = 'activemq.prefetchSize';
  CS_STOMP_ACTIVEMQ_HEADERS_AMQPRIORITY = 'activemq.priority';
  CS_STOMP_ACTIVEMQ_HEADERS_SELECTOR = 'selector';

constructor TsgcWSProtocol_STOMP_ActiveMQ_Client.Create(aOwner: TComponent);
begin
  inherited;
  FQueue := TsgcWSActiveMQSTOMP_Queue_Options.Create;
  FActiveMQ_Options := TsgcWSActiveMQSTOMP_Options.Create;
end;

destructor TsgcWSProtocol_STOMP_ActiveMQ_Client.Destroy;
begin
  sgcFree(FSubscriptionHeaders);
  sgcFree(FSendHeaders);
  sgcFree(FActiveMQ_Options);
  sgcFree(FQueue);
  inherited;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoBrokerConnectEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
begin
  inherited;
  DoActiveMQConnectEvent(aConnection, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoBrokerErrorEvent
  (aConnection: TsgcWSConnection; aMessageText: String; aRawHeaders: String);
begin
  inherited;
  DoActiveMQErrorEvent(aConnection, aMessageText, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoBrokerMessageEvent
  (aConnection: TsgcWSConnection; aMessageText, aRawHeaders: String;
  aSubscription: TsgcWSBrokerSTOMPSubscriptionItem);
begin
  inherited;
  DoActiveMQMessageEvent(aConnection, aMessageText, aRawHeaders, aSubscription);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoBrokerReceiptEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
begin
  inherited;
  DoActiveMQReceiptEvent(aConnection, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoPublish(const aDestination,
  aText, aContentType, aTransaction: String;
  const aOptions: TsgcWSActiveMQSTOMP_Message_Options);
var
  oCustomHeaders: TStrings;
begin
  oCustomHeaders := nil;
  if Assigned(aOptions) then
    oCustomHeaders := aOptions.GetHeaders;
  Send(aDestination, aText, aContentType, aTransaction, oCustomHeaders);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoActiveMQConnectEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
var
  oHeaders: TsgcWSActiveMQSTOMPHeadersConnected;
begin
  if Assigned(FOnActiveMQConnected) then
  begin
    oHeaders := TsgcWSActiveMQSTOMPHeadersConnected.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnActiveMQConnected(aConnection, oHeaders);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoActiveMQDisconnectedEvent(
    aConnection: TsgcWSConnection; aCode: Integer);
begin
  if Assigned(FOnActiveMQDisconnected) then
    FOnActiveMQDisconnected(aConnection, aCode);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoActiveMQErrorEvent
  (aConnection: TsgcWSConnection; aMessageText: String; aRawHeaders: String);
var
  oHeaders: TsgcWSActiveMQSTOMPHeadersError;
begin
  if Assigned(FOnActiveMQError) then
  begin
    oHeaders := TsgcWSActiveMQSTOMPHeadersError.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnActiveMQError(aConnection, aMessageText, oHeaders);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoActiveMQMessageEvent
  (aConnection: TsgcWSConnection; aMessageText, aRawHeaders: String;
  aSubscription: TsgcWSBrokerSTOMPSubscriptionItem);
var
  oHeaders: TsgcWSActiveMQSTOMPHeadersMessage;
begin
  if Assigned(FOnActiveMQMessage) then
  begin
    oHeaders := TsgcWSActiveMQSTOMPHeadersMessage.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnActiveMQMessage(aConnection, aMessageText, oHeaders, aSubscription);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoActiveMQReceiptEvent
  (aConnection: TsgcWSConnection; aRawHeaders: String);
var
  oHeaders: TsgcWSActiveMQSTOMPHeadersReceipt;
begin
  if Assigned(FOnActiveMQReceipt) then
  begin
    oHeaders := TsgcWSActiveMQSTOMPHeadersReceipt.Create(nil);
    Try
      oHeaders.Read(aRawHeaders);

      FOnActiveMQReceipt(aConnection, oHeaders);
    Finally
      sgcFree(oHeaders);
    End;
  end;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoBrokerDisconnectedEvent(
    aConnection: TsgcWSConnection; aCode: Integer);
begin
  inherited;
  DoActiveMQDisconnectedEvent(aConnection, aCode);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoConnect(const aConnection:
    TsgcWSConnection);
begin
  GetConnectHeaders.Clear;
  if ActiveMQ_Options.ClientId <> '' then
    GetConnectHeaders.Add
      (CS_STOMP_ACTIVEMQ_CLIENT_ID + ':' + ActiveMQ_Options.ClientId);
  inherited;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoSubscribe
  (const aDestination: string; const aDurable, aExclusive: Boolean;
  const aACK: TsgcSTOMPACK; const aOptions: TsgcWSActiveMQSTOMP_Queue_Options);
var
  vId: String;
  vOptions: TsgcWSActiveMQSTOMP_Queue_Options;
  oHeaders: TStrings;
  vDestination: String;
begin
  vOptions := aOptions;
  if not Assigned(vOptions) then
    vOptions := Queue;

  vId := GetNewId;
  vDestination := aDestination;
  oHeaders := GetSubscriptionHeaders(aDurable, aExclusive, vOptions);

  Subscribe(vId, vDestination, aACK, oHeaders);
  DoQueueSubscription(vId, vDestination, aACK, oHeaders);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.DoUnSubscribe
  (const aDestination: String);
var
  oSubscription: TsgcWSBrokerSTOMPSubscriptionItem;
  vDurable, vExclusive: Boolean;
begin
  oSubscription := GetSubscriptionByName(aDestination);
  if Assigned(oSubscription) then
  begin
    vDurable := not oSubscription.Headers.IndexOf
      (CS_STOMP_ACTIVEMQ_RETROACTIVE + ':true')
      <> -1;
    vExclusive := oSubscription.Headers.IndexOf
      (CS_STOMP_ACTIVEMQ_EXCLUSIVE + ':true') <> -1;

    UnSubscribe(oSubscription.Id,
      GetSubscriptionHeaders(vDurable, vExclusive, nil));
    DoDeleteSubscription(oSubscription.Id);
  end;
end;

function TsgcWSProtocol_STOMP_ActiveMQ_Client.GetSubscriptionHeaders
  (const aDurable: Boolean; aExclusive: Boolean;
  aOptions: TsgcWSActiveMQSTOMP_Queue_Options): TStringList;

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

  DoAddHeader(CS_STOMP_ACTIVEMQ_RETROACTIVE, aDurable);
  DoAddHeader(CS_STOMP_ACTIVEMQ_EXCLUSIVE, aExclusive);

  if Assigned(aOptions) then
  begin
    oOptions := aOptions.GetHeaders;
    for i := 0 to oOptions.Count - 1 do
      FSubscriptionHeaders.Add(oOptions[i]);
  end;
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.PublishEx(const aDestination,
  aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = ''; const aOptions
    : TsgcWSActiveMQSTOMP_Message_Options = nil);
begin
  DoPublish(aDestination, aText, aContentType, aTransaction, aOptions);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.PublishQueue(const aQueue,
  aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '';
  const aOptions: TsgcWSActiveMQSTOMP_Message_Options = nil);
begin
  DoPublish(CS_STOMP_ACTIVEMQ_QUEUE + '/' + aQueue, aText, aContentType,
    aTransaction, aOptions);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.PublishTopic(const aTopic,
  aText: String; const aContentType: String = CS_TEXT_PLAIN;
  const aTransaction: String = '';
  const aOptions: TsgcWSActiveMQSTOMP_Message_Options = nil);
begin
  DoPublish(CS_STOMP_ACTIVEMQ_DURABLE_TOPIC + '/' + aTopic, aText,
    aContentType, aTransaction, aOptions);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.SetActiveMQ_Options
  (const Value: TsgcWSActiveMQSTOMP_Options);
begin
  FActiveMQ_Options.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.SetQueue
  (const Value: TsgcWSActiveMQSTOMP_Queue_Options);
begin
  FQueue.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.SubscribeEx
  (const aDestination: String; const aDurable: Boolean = True;
  const aExclusive: Boolean = False;
  const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSActiveMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(aDestination, aDurable, aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.SubscribeQueue
  (const aQueue: String; const aDurable: Boolean = True;
  const aExclusive: Boolean = False; const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSActiveMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(CS_STOMP_ACTIVEMQ_QUEUE + '/' + aQueue, aDurable, aExclusive,
    aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.SubscribeTopic
  (const aTopic: String; const aDurable: Boolean = True;
  const aExclusive: Boolean = False; const aACK: TsgcSTOMPACK = ackAuto;
  const aOptions: TsgcWSActiveMQSTOMP_Queue_Options = nil);
begin
  DoSubscribe(CS_STOMP_ACTIVEMQ_DURABLE_TOPIC + '/' + aTopic, aDurable,
    aExclusive, aACK, aOptions);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.UnSubscribeEx
  (const aDestination: String);
begin
  DoUnSubscribe(aDestination);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.UnSubscribeQueue
  (const aQueue: String);
begin
  DoUnSubscribe(CS_STOMP_ACTIVEMQ_QUEUE + '/' + aQueue);
end;

procedure TsgcWSProtocol_STOMP_ActiveMQ_Client.UnSubscribeTopic
  (const aTopic: String);
begin
  DoUnSubscribe(CS_STOMP_ACTIVEMQ_DURABLE_TOPIC + '/' + aTopic);
end;

constructor TsgcWSActiveMQSTOMP_Queue_Options.Create;
begin
  inherited;
  Clear;
end;

procedure TsgcWSActiveMQSTOMP_Queue_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSActiveMQSTOMP_Queue_Options then
  begin
    DispatchAsync := TsgcWSActiveMQSTOMP_Queue_Options(aSource).DispatchAsync;
    MaximumPendingMessageLimit := TsgcWSActiveMQSTOMP_Queue_Options(aSource)
      .MaximumPendingMessageLimit;
    NoLocal := TsgcWSActiveMQSTOMP_Queue_Options(aSource).NoLocal;
    PrefetchSize := TsgcWSActiveMQSTOMP_Queue_Options(aSource).PrefetchSize;
    Priority := TsgcWSActiveMQSTOMP_Queue_Options(aSource).Priority;
    Selector := TsgcWSActiveMQSTOMP_Queue_Options(aSource).Selector;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSActiveMQSTOMP_Queue_Options.Clear;
begin
  FPriority := 0;
  FSelector := '';
  FMaximumPendingMessageLimit := 0;
  FNoLocal := False;
  FPrefetchSize := 0;
  FDispatchAsync := False;
end;

procedure TsgcWSActiveMQSTOMP_Queue_Options.DoHeaders;
begin
  FHeaders.Clear;

  if DispatchAsync then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_DISPATCHASYNC + ':true')
  else
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_DISPATCHASYNC + ':false');
  if MaximumPendingMessageLimit <> 0 then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_MAXIMUMPENDINGMESSAGELIMIT + ':' +
        IntToStr(MaximumPendingMessageLimit));
  if NoLocal then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_NOLOCAL + ':true')
  else
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_NOLOCAL + ':false');
  if PrefetchSize > 0 then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_PREFETCHSIZE + ':' + IntToStr
        (PrefetchSize));
  if Priority <> 0 then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_AMQPRIORITY + IntToStr(Priority));
  if Selector <> '' then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_SELECTOR + ':' + Selector);
end;

function TsgcWSActiveMQSTOMP_Queue_Options.GetHeaders: TStringList;
begin
  if not Assigned(FHeaders) then
    FHeaders := TStringList.Create;
  DoHeaders;
  Result := FHeaders
end;

procedure TsgcWSActiveMQSTOMPHeadersMessage.DoRead;
begin
  inherited;
  TryStrToInt(GetHeaderByName(CS_STOMP_ACTIVEMQ_HEADER_EXPIRES), FExpires);
  TryStrToInt(GetHeaderByName(CS_STOMP_ACTIVEMQ_HEADER_PRIORITY), FPriority);
  Timestamp := GetHeaderByName(CS_STOMP_ACTIVEMQ_HEADER_TIMESTAMP);
end;

constructor TsgcWSActiveMQSTOMP_Options.Create;
begin
  inherited;
  ClientId := '';
end;

procedure TsgcWSActiveMQSTOMP_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSActiveMQSTOMP_Options then
    ClientId := TsgcWSActiveMQSTOMP_Options(aSource).ClientId
  else
    inherited Assign(aSource);
end;

constructor TsgcWSActiveMQSTOMP_Message_Options.Create;
begin
  inherited;
  Clear;
end;

procedure TsgcWSActiveMQSTOMP_Message_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSActiveMQSTOMP_Message_Options then
  begin
    CorrelationId := TsgcWSActiveMQSTOMP_Message_Options(aSource).CorrelationId;
    Expires := TsgcWSActiveMQSTOMP_Message_Options(aSource).Expires;
    JMSXGroupID := TsgcWSActiveMQSTOMP_Message_Options(aSource).JMSXGroupID;
    JMSXGroupSeq := TsgcWSActiveMQSTOMP_Message_Options(aSource).JMSXGroupSeq;
    MsgType := TsgcWSActiveMQSTOMP_Message_Options(aSource).MsgType;
    Persistent := TsgcWSActiveMQSTOMP_Message_Options(aSource).Persistent;
    Priority := TsgcWSActiveMQSTOMP_Message_Options(aSource).Priority;
    ReplyTo := TsgcWSActiveMQSTOMP_Message_Options(aSource).ReplyTo;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSActiveMQSTOMP_Message_Options.Clear;
begin
  FCorrelationId := '';
  FExpires := 0;
  FJMSXGroupID := '';
  FJMSXGroupSeq := 0;
  FMsgType := '';
  FPersistent := False;
  FPriority := 0;
  FReplyTo := '';
end;

procedure TsgcWSActiveMQSTOMP_Message_Options.DoHeaders;
begin
  FHeaders.Clear;

  if CorrelationId <> '' then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_CORRELATION_ID + ':' +
        CorrelationId);
  if Expires > 0 then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_EXPIRES + ':' + IntToStr(Expires));
  if JMSXGroupID <> '' then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_JMSXGROUPID + ':' + JMSXGroupID);
  if JMSXGroupSeq > 0 then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_JMSXGROUPSEQ + ':' + IntToStr
        (JMSXGroupSeq));
  if Persistent then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_PERSISTENT + ':true')
  else
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_PERSISTENT + ':false');
  if Priority <> 0 then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_PRIORITY + ':' + IntToStr(Priority));
  if ReplyTo <> '' then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_REPLY_TO + ':' + ReplyTo);
  if MsgType <> '' then
    FHeaders.Add(CS_STOMP_ACTIVEMQ_HEADERS_TYPE + ':' + MsgType);
end;

function TsgcWSActiveMQSTOMP_Message_Options.GetHeaders: TStringList;
begin
  if not Assigned(FHeaders) then
    FHeaders := TStringList.Create;
  DoHeaders;
  Result := FHeaders
end;
{$ENDIF}

end.

