{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_AMQP_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Protocol_Base_Client, sgcWebSocket_Classes, sgcAMQP,
  sgcWebSocket_Protocol_AMQP_Message, sgcWebSocket_Classes_SyncObjs,
  sgcAMQP_Classes, sgcAMQP_Const, sgcBase_Helpers;

type

  TsgcWSAMQPAuthentication_Options = class(TPersistent)
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

  TsgcWSAMQPHeartBeat_Options = class(TPersistent)
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

  TsgcWSAMQP_Options = class(TPersistent)
  private
    FLocale: string;
    FMaxChannels: Word;
    FMaxFrameSize: Cardinal;
    FVirtualHost: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Locale: string read FLocale write FLocale;
    property MaxChannels: Word read FMaxChannels write FMaxChannels;
    property MaxFrameSize: Cardinal read FMaxFrameSize write FMaxFrameSize;
    property VirtualHost: String read FVirtualHost write FVirtualHost;
  end;

  TsgcWSProtocol_AMQP_Client_Base = class(TsgcWSProtocol_Client_Base)
    { wsmessage }
  private
    FWSMessageId: String;
  private
    procedure DoAMQPDisconnectedEvent(aCode: Integer);
    procedure DoAMQPInitialize(const aMessage: TsgcWSAMQPMessage);
  protected
    FWSMessage: TsgcWSAMQPMessage;
    function GetWSMessage: TsgcWSAMQPMessage;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection)
      : TsgcWSAMQPMessage;
  protected
    procedure OnAMQPWriteBytesEvent(Sender: TObject;
      const aBytes: TBytes); virtual;
    procedure OnAMQPAuthenticationEvent(Sender: TObject;
      aMechanisms: TsgcAMQPAuthentications;
      var Mechanism: TsgcAMQPAuthentication;
      var User, Password: string); virtual;
    procedure OnAMQPChallengeEvent(Sender: TObject; const aChallenge: String;
      var Challenge: String); virtual;
    procedure OnAMQPConnectEvent(Sender: TObject); virtual;
    procedure OnAMQPHeartBeatEvent(Sender: TObject); virtual;
    procedure OnAMQPDisconnectEvent(Sender: TObject;
      const aClose: TsgcAMQPFramePayload_Method_ConnectionClose;
      aAck: Boolean); virtual;
    procedure OnAMQPChannelOpenEvent(Sender: TObject;
      const aChannel: string); virtual;
    procedure OnAMQPChannelCloseEvent(Sender: TObject; const aChannel: string;
      const aChannelClose: TsgcAMQPFramePayload_Method_ChannelClose;
      aAck: Boolean); virtual;
    procedure OnAMQPChannelFlowEvent(Sender: TObject; const aChannel: string;
      aFlow, aAck: Boolean); virtual;
    procedure OnAMQPExchangeDeclareEvent(Sender: TObject;
      const aChannel, aExchange: string); virtual;
    procedure OnAMQPExchangeDeleteEvent(Sender: TObject;
      const aChannel, aExchange: string); virtual;
    procedure OnAMQPQueueDeclareEvent(Sender: TObject;
      const aChannel, aQueue: string;
      aMessageCount, aConsumerCount: Integer); virtual;
    procedure OnAMQPQueueBindEvent(Sender: TObject;
      const aChannel, aQueue, aExchange: string); virtual;
    procedure OnAMQPQueueUnBindEvent(Sender: TObject;
      const aChannel, aQueue, aExchange: string); virtual;
    procedure OnAMQPQueuePurgeEvent(Sender: TObject;
      const aChannel, aQueue: string; aMessageCount: Integer); virtual;
    procedure OnAMQPQueueDeleteEvent(Sender: TObject;
      const aChannel, aQueue: string; aMessageCount: Integer); virtual;
    procedure OnAMQPBasicQoSEvent(Sender: TObject; const aChannel: string;
      const aQoS: TsgcAMQPFramePayload_Method_BasicQoS); virtual;
    procedure OnAMQPBasicConsumeEvent(Sender: TObject;
      const aChannel, aConsumerTag: string); virtual;
    procedure OnAMQPBasicCancelConsumeEvent(Sender: TObject;
      const aChannel: string; const aConsumerTag: string); virtual;
    procedure OnAMQPBasicReturnEvent(Sender: TObject; const aChannel: string;
      const aReturn: TsgcAMQPFramePayload_Method_BasicReturn;
      const aMessage: TsgcAMQPMessageContent); virtual;
    procedure OnAMQPBasicDeliverEvent(Sender: TObject; const aChannel: string;
      const aDeliver: TsgcAMQPFramePayload_Method_BasicDeliver;
      const aMessage: TsgcAMQPMessageContent); virtual;
    procedure OnAMQPBasicGetOkEvent(Sender: TObject; const aChannel: string;
      const aGetOk: TsgcAMQPFramePayload_Method_BasicGetOk;
      const aMessage: TsgcAMQPMessageContent); virtual;
    procedure OnAMQPBasicGetEmptyEvent(Sender: TObject;
      const aChannel: string); virtual;
    procedure OnAMQPBasicRecoverOkEvent(Sender: TObject;
      const aChannel: string); virtual;
    procedure OnAMQPTransactionOkEvent(Sender: TObject; const aChannel: string;
      aTransaction: TsgcAMQPTransaction); virtual;
    procedure OnAMQPExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    procedure OnAMQPConnectedEvent(Sender: TObject;
      aVersion, aServer, aSession, aHeartBeat, aRawHeaders: String);
    procedure OnAMQPErrorEvent(Sender: TObject;
      aMessageText, aContentType: String; aContentLength: Integer;
      aReceiptId, aRawHeaders: String);
    procedure OnAMQPMessageEvent(Sender: TObject;
      aMessageText, aDestination, aMessageId, aSubscription, aAck, aContentType,
      aRawHeaders: String);
    procedure OnAMQPReceiptEvent(Sender: TObject;
      aReceiptId, aRawHeaders: String);
  protected
    property WSMessage: TsgcWSAMQPMessage read GetWSMessage write FWSMessage;
    { wsmessage }

    { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    function GetChannels: TsgcAMQPChannelThreads;
  public
    property Channels: TsgcAMQPChannelThreads read GetChannels;
    { properties }

    { procedures }
  protected
    procedure DoWriteAMQP(const aBytes: TBytes); virtual;
  protected
    procedure DoConnect(const aConnection: TsgcWSConnection); virtual;
    // ... connection
  public
    procedure Close(aReplyCode: Word; const aReplyText: string;
      aFailClassId: TsgcAMQPClass = amqpClassNone;
      aFailMethodId: TsgcAMQPMethod = amqpMethodNone); overload;
    function CloseEx(aReplyCode: Word; const aReplyText: string;
      aFailClassId: TsgcAMQPClass = amqpClassNone;
      aFailMethodId: TsgcAMQPMethod = amqpMethodNone): Boolean;
    procedure Close; overload;
    // ... channels
  public
    procedure OpenChannel(const aChannel: string);
    function OpenChannelEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure CloseChannel(const aChannel: string; aReplyCode: Word = 0;
      const aReplyText: string = '';
      aFailClassId: TsgcAMQPClass = amqpClassNone;
      aFailMethodId: TsgcAMQPMethod = amqpMethodNone); overload;
    function CloseChannelEx(const aChannel: string; aReplyCode: Word = 0;
      const aReplyText: string = '';
      aFailClassId: TsgcAMQPClass = amqpClassNone;
      aFailMethodId: TsgcAMQPMethod = amqpMethodNone;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean; overload;
    procedure EnableChannel(const aChannel: string);
    function EnableChannelEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure DisableChannel(const aChannel: string);
    function DisableChannelEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... exchanges
  public
    procedure DeclareExchange(const aChannel, aExchange: string;
      const aExchangeType: string = 'direct'; aPassive: Boolean = false;
      aDurable: Boolean = false; aAutoDelete: Boolean = false;
      aInternal: Boolean = false; aNoWait: Boolean = false);
    function DeclareExchangeEx(const aChannel, aExchange: string;
      const aExchangeType: string = 'direct'; aPassive: Boolean = false;
      aDurable: Boolean = false; aAutoDelete: Boolean = false;
      aInternal: Boolean = false; aNoWait: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure DeleteExchange(const aChannel, aExchange: string;
      aIfUnused: Boolean = false; aNoWait: Boolean = false);
    function DeleteExchangeEx(const aChannel, aExchange: string;
      aIfUnused: Boolean = false; aNoWait: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;

    // ... queues
  public
    procedure DeclareQueue(const aChannel, aQueue: string;
      aPassive: Boolean = false; aDurable: Boolean = false;
      aExclusive: Boolean = false; aAutoDelete: Boolean = false;
      aNoWait: Boolean = false);
    function DeclareQueueEx(const aChannel, aQueue: string;
      aPassive: Boolean = false; aDurable: Boolean = false;
      aExclusive: Boolean = false; aAutoDelete: Boolean = false;
      aNoWait: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure BindQueue(const aChannel, aQueue, aExchange, aRoutingKey: string;
      aNoWait: Boolean = false);
    function BindQueueEx(const aChannel, aQueue, aExchange, aRoutingKey: string;
      aNoWait: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure UnBindQueue(const aChannel, aQueue, aExchange,
      aRoutingKey: string);
    function UnBindQueueEx(const aChannel, aQueue, aExchange,
      aRoutingKey: string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT)
      : Boolean;
    procedure PurgeQueue(const aChannel, aQueue: string;
      aNoWait: Boolean = false);
    function PurgeQueueEx(const aChannel, aQueue: string;
      aNoWait: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure DeleteQueue(const aChannel, aQueue: string;
      aIfUnused: Boolean = false; aIfEmpty: Boolean = false;
      aNoWait: Boolean = false);
    function DeleteQueueEx(const aChannel, aQueue: string;
      aIfUnused: Boolean = false; aIfEmpty: Boolean = false;
      aNoWait: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... basic
  public
    procedure SetQoS(const aChannel: string; aPrefetchSize: Cardinal = 0;
      aPrefetchCount: Word = 0; aGlobal: Boolean = false);
    function SetQoSEx(const aChannel: string; aPrefetchSize: Cardinal = 0;
      aPrefetchCount: Word = 0; aGlobal: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure Consume(const aChannel, aQueue: string;
      const aConsumerTag: string = ''; aNoLocal: Boolean = false;
      aNoAck: Boolean = false; aExclusive: Boolean = false;
      aNoWait: Boolean = false);
    function ConsumeEx(const aChannel, aQueue: string; const aConsumerTag: string =
        ''; aNoLocal: Boolean = false; aNoAck: Boolean = false; aExclusive: Boolean
        = false; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure CancelConsume(const aChannel, aConsumerTag: string;
      aNoWait: Boolean = false);
    function CancelConsumeEx(const aChannel, aConsumerTag: string; aTimeout:
        Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey,
      aMessage: string; aMandatory: Boolean = false;
      aImmediate: Boolean = false); overload;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey: string;
      aMessage: TStream; aMandatory: Boolean = false;
      aImmediate: Boolean = false); overload;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey: string;
      const aHeader: TsgcAMQPBasicProperties; const aMessage: string;
      aMandatory: Boolean = false; aImmediate: Boolean = false); overload;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey: string;
      const aHeader: TsgcAMQPBasicProperties; const aMessage: TStream;
      aMandatory: Boolean = false; aImmediate: Boolean = false); overload;
    procedure GetMessage(const aChannel, aQueue: string;
      aNoAck: Boolean = false);
    function GetMessageEx(const aChannel, aQueue: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure AckMessage(const aChannel: string; aDeliveryTag: UInt64;
      aMultiple: Boolean = false);
    procedure RejectMessage(const aChannel: string; aDeliveryTag: UInt64;
      aRequeue: Boolean = false);
    procedure RecoverAsync(const aChannel: string; aRequeue: Boolean = false);
    procedure Recover(const aChannel: string; aRequeue: Boolean = false);
    function RecoverEx(const aChannel: string; aRequeue: Boolean = false;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... tx
  public
    procedure SelectTransaction(const aChannel: string);
    function SelectTransactionEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure CommitTransaction(const aChannel: string);
    function CommitTransactionEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure RollbackTransaction(const aChannel: string);
    function RollbackTransactionEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    { procedures }

    { Authentication }
  private
    FAuthentication: TsgcWSAMQPAuthentication_Options;
    procedure SetAuthentication(const Value: TsgcWSAMQPAuthentication_Options);
  public
    property Authentication: TsgcWSAMQPAuthentication_Options
      read FAuthentication write SetAuthentication;
    { Authentication }

    { heartbeat }
  private
    FHeartBeat: TsgcWSAMQPHeartBeat_Options;
    procedure SetHeartBeat(const Value: TsgcWSAMQPHeartBeat_Options);
  public
    property HeartBeat: TsgcWSAMQPHeartBeat_Options read FHeartBeat
      write SetHeartBeat;
    { heartbeat }

    { from TsgcWSProtocol_Client }
  private
    FFirstPing: TDateTime;
    FLastPing: TDateTime;
    FLastFrame: TDateTime;
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSProtocol_Client }

    { AMQPOptions }
  private
    FAMQPOptions: TsgcWSAMQP_Options;
    procedure SetAMQPOptions(const Value: TsgcWSAMQP_Options);
  public
    property AMQPOptions: TsgcWSAMQP_Options read FAMQPOptions
      write SetAMQPOptions;
    { AMQPOptions }

    { events }
  private
    FOnAMQPAuthentication: TsgcAMQPAuthenticationEvent;
    FOnAMQPBasicCancelConsume: TsgcAMQPBasicCancelConsumeEvent;
    FOnAMQPBasicConsume: TsgcAMQPBasicConsumeEvent;
    FOnAMQPBasicDeliver: TsgcAMQPBasicDeliverEvent;
    FOnAMQPBasicGetOk: TsgcAMQPBasicGetOkEvent;
    FOnAMQPChallenge: TsgcAMQPChallengeEvent;
    FOnAMQPChannelClose: TsgcAMQPChannelCloseEvent;
    FOnAMQPChannelFlow: TsgcAMQPChannelFlowEvent;
    FOnAMQPChannelOpen: TsgcAMQPChannelOpenEvent;
    FOnAMQPConnect: TsgcAMQPConnectEvent;
    FOnAMQPDisconnect: TsgcAMQPDisconnectEvent;
    FOnAMQPExchangeDeclare: TsgcAMQPExchangeDeclareEvent;
    FOnAMQPExchangeDelete: TsgcAMQPExchangeDeleteEvent;
    FOnAMQPHeartBeat: TsgcAMQPHeartBeatEvent;
    FOnAMQPBasicQoS: TsgcAMQPBasicQoSEvent;
    FOnAMQPBasicReturn: TsgcAMQPBasicReturnEvent;
    FOnAMQPBasicGetEmpty: TsgcAMQPBasicGetEmptyEvent;
    FOnAMQPBasicRecoverOk: TsgcAMQPBasicRecoverOkEvent;
    FOnAMQPException: TsgcAMQPExceptionEvent;
    FOnAMQPQueueBind: TsgcAMQPQueueBindEvent;
    FOnAMQPQueueDeclare: TsgcAMQPQueueDeclareEvent;
    FOnAMQPQueueDelete: TsgcAMQPQueueDeleteEvent;
    FOnAMQPQueuePurge: TsgcAMQPQueuePurgeEvent;
    FOnAMQPQueueUnBind: TsgcAMQPQueueUnBindEvent;
    FOnAMQPTransactionOk: TsgcAMQPTransactionOkEvent;
  public
    property OnAMQPAuthentication: TsgcAMQPAuthenticationEvent
      read FOnAMQPAuthentication write FOnAMQPAuthentication;
    property OnAMQPBasicCancelConsume: TsgcAMQPBasicCancelConsumeEvent
      read FOnAMQPBasicCancelConsume write FOnAMQPBasicCancelConsume;
    property OnAMQPBasicConsume: TsgcAMQPBasicConsumeEvent
      read FOnAMQPBasicConsume write FOnAMQPBasicConsume;
    property OnAMQPBasicDeliver: TsgcAMQPBasicDeliverEvent
      read FOnAMQPBasicDeliver write FOnAMQPBasicDeliver;
    property OnAMQPBasicGetOk: TsgcAMQPBasicGetOkEvent read FOnAMQPBasicGetOk
      write FOnAMQPBasicGetOk;
    property OnAMQPChallenge: TsgcAMQPChallengeEvent read FOnAMQPChallenge
      write FOnAMQPChallenge;
    property OnAMQPChannelClose: TsgcAMQPChannelCloseEvent
      read FOnAMQPChannelClose write FOnAMQPChannelClose;
    property OnAMQPChannelFlow: TsgcAMQPChannelFlowEvent read FOnAMQPChannelFlow
      write FOnAMQPChannelFlow;
    property OnAMQPChannelOpen: TsgcAMQPChannelOpenEvent read FOnAMQPChannelOpen
      write FOnAMQPChannelOpen;
    property OnAMQPConnect: TsgcAMQPConnectEvent read FOnAMQPConnect
      write FOnAMQPConnect;
    property OnAMQPDisconnect: TsgcAMQPDisconnectEvent read FOnAMQPDisconnect
      write FOnAMQPDisconnect;
    property OnAMQPExchangeDeclare: TsgcAMQPExchangeDeclareEvent
      read FOnAMQPExchangeDeclare write FOnAMQPExchangeDeclare;
    property OnAMQPExchangeDelete: TsgcAMQPExchangeDeleteEvent
      read FOnAMQPExchangeDelete write FOnAMQPExchangeDelete;
    property OnAMQPHeartBeat: TsgcAMQPHeartBeatEvent read FOnAMQPHeartBeat
      write FOnAMQPHeartBeat;
    property OnAMQPBasicQoS: TsgcAMQPBasicQoSEvent read FOnAMQPBasicQoS
      write FOnAMQPBasicQoS;
    property OnAMQPBasicReturn: TsgcAMQPBasicReturnEvent read FOnAMQPBasicReturn
      write FOnAMQPBasicReturn;
    property OnAMQPBasicGetEmpty: TsgcAMQPBasicGetEmptyEvent
      read FOnAMQPBasicGetEmpty write FOnAMQPBasicGetEmpty;
    property OnAMQPBasicRecoverOk: TsgcAMQPBasicRecoverOkEvent
      read FOnAMQPBasicRecoverOk write FOnAMQPBasicRecoverOk;
    property OnAMQPException: TsgcAMQPExceptionEvent read FOnAMQPException
      write FOnAMQPException;
    property OnAMQPQueueBind: TsgcAMQPQueueBindEvent read FOnAMQPQueueBind
      write FOnAMQPQueueBind;
    property OnAMQPQueueDeclare: TsgcAMQPQueueDeclareEvent
      read FOnAMQPQueueDeclare write FOnAMQPQueueDeclare;
    property OnAMQPQueueDelete: TsgcAMQPQueueDeleteEvent read FOnAMQPQueueDelete
      write FOnAMQPQueueDelete;
    property OnAMQPQueuePurge: TsgcAMQPQueuePurgeEvent read FOnAMQPQueuePurge
      write FOnAMQPQueuePurge;
    property OnAMQPQueueUnBind: TsgcAMQPQueueUnBindEvent read FOnAMQPQueueUnBind
      write FOnAMQPQueueUnBind;
    property OnAMQPTransactionOk: TsgcAMQPTransactionOkEvent
      read FOnAMQPTransactionOk write FOnAMQPTransactionOk;
    { events }
  end;

  TsgcWSProtocol_AMQP_Client = class(TsgcWSProtocol_AMQP_Client_Base)

  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  DateUtils,
  // sgc
  sgcWebSocket_Helpers, sgcAMQP_Helpers, sgcWebSocket_Const,
  sgcWebSocket_Types;

type
  TsgcWSComponent_HeartBeat = class(TsgcWSComponent)
  end;

constructor TsgcWSProtocol_AMQP_Client_Base.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageId := NewGuid;
  FProtocol := CS_PROTOCOL_AMQP;
  MsgType := msgBinary;
  FAuthentication := TsgcWSAMQPAuthentication_Options.Create;
  FAuthentication.Enabled := false;
  FHeartBeat := TsgcWSAMQPHeartBeat_Options.Create;
  FHeartBeat.Enabled := True;
  FAMQPOptions := TsgcWSAMQP_Options.Create;
end;

destructor TsgcWSProtocol_AMQP_Client_Base.Destroy;
begin
  sgcFree(FHeartBeat);
  sgcFree(FAMQPOptions);
  sgcFree(FAuthentication);
  inherited;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.AckMessage(const aChannel: string;
  aDeliveryTag: UInt64; aMultiple: Boolean = false);
begin
  WSMessage.AckMessage(aChannel, aDeliveryTag, aMultiple);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.BindQueue(const aChannel, aQueue,
  aExchange, aRoutingKey: string; aNoWait: Boolean = false);
begin
  WSMessage.BindQueue(aChannel, aQueue, aExchange, aRoutingKey, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.BindQueueEx(const aChannel, aQueue,
  aExchange, aRoutingKey: string; aNoWait: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.BindQueueEx(aChannel, aQueue, aExchange, aRoutingKey,
    aNoWait, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.CancelConsume(const aChannel,
  aConsumerTag: string; aNoWait: Boolean = false);
begin
  WSMessage.CancelConsume(aChannel, aConsumerTag, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.CancelConsumeEx(const aChannel,
    aConsumerTag: string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.CancelConsumeEx(aChannel, aConsumerTag, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.Close(aReplyCode: Word;
  const aReplyText: string; aFailClassId: TsgcAMQPClass = amqpClassNone;
  aFailMethodId: TsgcAMQPMethod = amqpMethodNone);
begin
  WSMessage.Close(aReplyCode, aReplyText, aFailClassId, aFailMethodId);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.Close;
begin
  WSMessage.Close(0, '', amqpClassNone, amqpMethodNone);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.CloseChannel(const aChannel: string;
  aReplyCode: Word = 0; const aReplyText: string = '';
  aFailClassId: TsgcAMQPClass = amqpClassNone;
  aFailMethodId: TsgcAMQPMethod = amqpMethodNone);
begin
  WSMessage.CloseChannel(aChannel, aReplyCode, aReplyText, aFailClassId,
    aFailMethodId);
end;

function TsgcWSProtocol_AMQP_Client_Base.CloseChannelEx(const aChannel: string;
  aReplyCode: Word = 0; const aReplyText: string = '';
  aFailClassId: TsgcAMQPClass = amqpClassNone;
  aFailMethodId: TsgcAMQPMethod = amqpMethodNone;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.CloseChannelEx(aChannel, aReplyCode, aReplyText,
    aFailClassId, aFailMethodId, aTimeout);
end;

function TsgcWSProtocol_AMQP_Client_Base.CloseEx(aReplyCode: Word;
  const aReplyText: string; aFailClassId: TsgcAMQPClass = amqpClassNone;
  aFailMethodId: TsgcAMQPMethod = amqpMethodNone): Boolean;
begin
  Result := WSMessage.CloseEx(aReplyCode, aReplyText, aFailClassId,
    aFailMethodId);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.CommitTransaction
  (const aChannel: string);
begin
  WSMessage.CommitTransaction(aChannel);
end;

function TsgcWSProtocol_AMQP_Client_Base.CommitTransactionEx(const aChannel
  : string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.CommitTransactionEx(aChannel, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.Consume(const aChannel,
  aQueue: string; const aConsumerTag: string = ''; aNoLocal: Boolean = false;
  aNoAck: Boolean = false; aExclusive: Boolean = false;
  aNoWait: Boolean = false);
begin
  WSMessage.Consume(aChannel, aQueue, aConsumerTag, aNoLocal, aNoAck,
    aExclusive, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.ConsumeEx(const aChannel, aQueue:
    string; const aConsumerTag: string = ''; aNoLocal: Boolean = false; aNoAck:
    Boolean = false; aExclusive: Boolean = false; aTimeout: Integer =
    CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.ConsumeEx(aChannel, aQueue, aConsumerTag, aNoLocal,
    aNoAck, aExclusive, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DeclareExchange(const aChannel,
  aExchange: string; const aExchangeType: string = 'direct';
  aPassive: Boolean = false; aDurable: Boolean = false;
  aAutoDelete: Boolean = false; aInternal: Boolean = false;
  aNoWait: Boolean = false);
begin
  WSMessage.DeclareExchange(aChannel, aExchange, aExchangeType, aPassive,
    aDurable, aAutoDelete, aInternal, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.DeclareExchangeEx(const aChannel,
  aExchange: string; const aExchangeType: string = 'direct';
  aPassive: Boolean = false; aDurable: Boolean = false;
  aAutoDelete: Boolean = false; aInternal: Boolean = false;
  aNoWait: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.DeclareExchangeEx(aChannel, aExchange, aExchangeType,
    aPassive, aDurable, aAutoDelete, aInternal, aNoWait, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DeclareQueue(const aChannel,
  aQueue: string; aPassive: Boolean = false; aDurable: Boolean = false;
  aExclusive: Boolean = false; aAutoDelete: Boolean = false;
  aNoWait: Boolean = false);
begin
  WSMessage.DeclareQueue(aChannel, aQueue, aPassive, aDurable, aExclusive,
    aAutoDelete, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.DeclareQueueEx(const aChannel,
  aQueue: string; aPassive: Boolean = false; aDurable: Boolean = false;
  aExclusive: Boolean = false; aAutoDelete: Boolean = false;
  aNoWait: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.DeclareQueueEx(aChannel, aQueue, aPassive, aDurable,
    aExclusive, aAutoDelete, aNoWait, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DeleteExchange(const aChannel,
  aExchange: string; aIfUnused: Boolean = false; aNoWait: Boolean = false);
begin
  WSMessage.DeleteExchange(aChannel, aExchange, aIfUnused, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.DeleteExchangeEx(const aChannel,
  aExchange: string; aIfUnused: Boolean = false; aNoWait: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.DeleteExchangeEx(aChannel, aExchange, aIfUnused, aNoWait,
    aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DeleteQueue(const aChannel,
  aQueue: string; aIfUnused: Boolean = false; aIfEmpty: Boolean = false;
  aNoWait: Boolean = false);
begin
  WSMessage.DeleteQueue(aChannel, aQueue, aIfUnused, aIfEmpty, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.DeleteQueueEx(const aChannel,
  aQueue: string; aIfUnused: Boolean = false; aIfEmpty: Boolean = false;
  aNoWait: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.DeleteQueueEx(aChannel, aQueue, aIfUnused, aIfEmpty,
    aNoWait, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DisableChannel(const aChannel
  : string);
begin
  WSMessage.DisableChannel(aChannel);
end;

function TsgcWSProtocol_AMQP_Client_Base.DisableChannelEx(const aChannel
  : string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.DisableChannelEx(aChannel, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoAMQPDisconnectedEvent
  (aCode: Integer);
var
  aClose: TsgcAMQPFramePayload_Method_ConnectionClose;
begin
  if Assigned(FOnAMQPDisconnect) then
  begin
    aClose := TsgcAMQPFramePayload_Method_ConnectionClose.Create;
    Try
      aClose.ReplyCode := aCode;
      FOnAMQPDisconnect(self, aClose, false);
    Finally
      sgcFree(aClose);
    End;
  end;
end;

function TsgcWSProtocol_AMQP_Client_Base.GetWSMessage: TsgcWSAMQPMessage;
begin
  Result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(Result) then
  begin
    if not Assigned(FWSMessage) then
    begin
      FWSMessage := TsgcWSAMQPMessage.Create(self);
      DoAMQPInitialize(FWSMessage);
    end;
    Result := FWSMessage;
  end;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoClear
  (aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
  FFirstPing := 0;
  FLastPing := 0;
  FLastFrame := 0;

  DoConnect(aConnection);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  WSMessage.Clear;
  DoAMQPDisconnectedEvent(Code);
  inherited;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoFinalize
  (aConnection: TsgcWSConnection);
begin
  inherited;
  if Assigned(FWSMessage) then
    FWSMessage.Clear;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoInitialize
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
  FFirstPing := 0;
  FLastPing := 0;
  FLastFrame := 0;

  DoConnect(aConnection);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoConnect(const aConnection
  : TsgcWSConnection);
var
  oMessage: TsgcWSAMQPMessage;
begin
  oMessage := GetWSMessageByConnection(aConnection);

  oMessage.Connect;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoEventBinary(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
begin

end;

function TsgcWSProtocol_AMQP_Client_Base.DoKeepAlive: Boolean;
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
      if (FLastPing > FLastFrame) and
        (SecondsBetWeen(FLastPing, FLastFrame) >= HeartBeat.Timeout) then
      begin
        if ((HeartBeat.Interval >= HeartBeat.Timeout) and
          (SecondsBetWeen(Now, FLastPing) >= HeartBeat.Timeout) or
          (HeartBeat.Timeout >= HeartBeat.Interval) and
          (SecondsBetWeen(Now, FFirstPing) >= HeartBeat.Timeout)) then
          raise Exception.Create(S_HEARTBEAT_TIMEOUT_EXCEEDED);
      end
      else
        FFirstPing := 0;
    end;
    // ... send ping
    WSMessage.Ping;
    if FFirstPing = 0 then
      FFirstPing := Now;
    FLastPing := Now;
    // ... result
    Result := True;
  end
  else
    Result := false;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoNotifyBinary
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FLastFrame := Now;

  if Assigned(aConnection) then
  begin
    if Assigned(aConnection.MsgBinaryReceived) then
    begin
      aConnection.MsgBinaryReceived.Position := 0;
      WSMessage.Read(aConnection.MsgBinaryReceived);
    end;
  end;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
begin
  WSMessage.Disconnect;
  inherited;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoAMQPInitialize(const aMessage:
    TsgcWSAMQPMessage);
begin
  // ... events
  aMessage.OnAMQPWriteBytes := OnAMQPWriteBytesEvent;
  aMessage.OnAMQPAuthentication := OnAMQPAuthenticationEvent;
  aMessage.OnAMQPChallenge := OnAMQPChallengeEvent;
  aMessage.OnAMQPConnect := OnAMQPConnectEvent;
  aMessage.OnAMQPHeartBeat := OnAMQPHeartBeatEvent;
  aMessage.OnAMQPDisconnect := OnAMQPDisconnectEvent;
  aMessage.OnAMQPChannelOpen := OnAMQPChannelOpenEvent;
  aMessage.OnAMQPChannelClose := OnAMQPChannelCloseEvent;
  aMessage.OnAMQPChannelFlow := OnAMQPChannelFlowEvent;
  aMessage.OnAMQPExchangeDeclare := OnAMQPExchangeDeclareEvent;
  aMessage.OnAMQPExchangeDelete := OnAMQPExchangeDeleteEvent;
  aMessage.OnAMQPQueueDeclare := OnAMQPQueueDeclareEvent;
  aMessage.OnAMQPQueueBind := OnAMQPQueueBindEvent;
  aMessage.OnAMQPQueueUnBind := OnAMQPQueueUnBindEvent;
  aMessage.OnAMQPQueuePurge := OnAMQPQueuePurgeEvent;
  aMessage.OnAMQPQueueDelete := OnAMQPQueueDeleteEvent;
  aMessage.OnAMQPBasicQoS := OnAMQPBasicQoSEvent;
  aMessage.OnAMQPBasicConsume := OnAMQPBasicConsumeEvent;
  aMessage.OnAMQPBasicCancelConsume := OnAMQPBasicCancelConsumeEvent;
  aMessage.OnAMQPBasicReturn := OnAMQPBasicReturnEvent;
  aMessage.OnAMQPBasicDeliver := OnAMQPBasicDeliverEvent;
  aMessage.OnAMQPBasicGetOk := OnAMQPBasicGetOkEvent;
  aMessage.OnAMQPBasicGetEmpty := OnAMQPBasicGetEmptyEvent;
  aMessage.OnAMQPBasicRecoverOk := OnAMQPBasicRecoverOkEvent;
  aMessage.OnAMQPTransactionOk := OnAMQPTransactionOkEvent;
  aMessage.OnAMQPException := OnAMQPExceptionEvent;

  // ... heartbeat
  aMessage.HeartBeat.Enabled := HeartBeat.Enabled;
  aMessage.HeartBeat.Interval := HeartBeat.Interval;

  // ... amqp options
  aMessage.AMQPOptions.VirtualHost := AMQPOptions.VirtualHost;
  aMessage.AMQPOptions.Locale := AMQPOptions.Locale;
  aMessage.AMQPOptions.MaxChannels := AMQPOptions.MaxChannels;
  aMessage.AMQPOptions.MaxFrameSize := AMQPOptions.MaxFrameSize;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.DoWriteAMQP(const aBytes: TBytes);
var
  oStream: TMemoryStream;
begin
  oStream := TMemoryStream.Create;
  Try
    sgcWriteAMQPBytes(oStream, aBytes);
    oStream.Position := 0;

    WriteData(oStream);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.EnableChannel(const aChannel: string);
begin
  WSMessage.EnableChannel(aChannel);
end;

function TsgcWSProtocol_AMQP_Client_Base.EnableChannelEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.EnableChannelEx(aChannel, aTimeout);
end;

function TsgcWSProtocol_AMQP_Client_Base.GetChannels: TsgcAMQPChannelThreads;
begin
  Result := WSMessage.Channels;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.GetMessage(const aChannel,
  aQueue: string; aNoAck: Boolean = false);
begin
  WSMessage.GetMessage(aChannel, aQueue, aNoAck);
end;

function TsgcWSProtocol_AMQP_Client_Base.GetMessageEx(const aChannel,
  aQueue: string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.GetMessageEx(aChannel, aQueue, aTimeout);
end;

function TsgcWSProtocol_AMQP_Client_Base.GetWSMessageByConnection
  (const aConnection: TsgcWSConnection): TsgcWSAMQPMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSAMQPMessage.Create(nil);
      DoAMQPInitialize(TsgcWSAMQPMessage(oItem.WSMessage));

      // TsgcWSAMQPMessage(oItem.WSMessage).OnConnected := OnAMQPConnectedEvent;
      // TsgcWSAMQPMessage(oItem.WSMessage).OnError := OnAMQPErrorEvent;
      // TsgcWSAMQPMessage(oItem.WSMessage).OnMessage := OnAMQPMessageEvent;
      // TsgcWSAMQPMessage(oItem.WSMessage).OnReceipt := OnAMQPReceiptEvent;
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    Result := TsgcWSAMQPMessage(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPAuthenticationEvent
  (Sender: TObject; aMechanisms: TsgcAMQPAuthentications;
  var Mechanism: TsgcAMQPAuthentication; var User, Password: string);
begin
  if Assigned(FOnAMQPAuthentication) then
    FOnAMQPAuthentication(self, aMechanisms, Mechanism, User, Password);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicCancelConsumeEvent
  (Sender: TObject; const aChannel: string; const aConsumerTag: string);
begin
  if Assigned(FOnAMQPBasicCancelConsume) then
    FOnAMQPBasicCancelConsume(self, aChannel, aConsumerTag);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicConsumeEvent
  (Sender: TObject; const aChannel, aConsumerTag: string);
begin
  if Assigned(FOnAMQPBasicConsume) then
    FOnAMQPBasicConsume(self, aChannel, aConsumerTag);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicDeliverEvent
  (Sender: TObject; const aChannel: string;
  const aDeliver: TsgcAMQPFramePayload_Method_BasicDeliver;
  const aMessage: TsgcAMQPMessageContent);
begin
  if Assigned(FOnAMQPBasicDeliver) then
    FOnAMQPBasicDeliver(self, aChannel, aDeliver, aMessage);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicGetEmptyEvent
  (Sender: TObject; const aChannel: string);
begin
  if Assigned(FOnAMQPBasicGetEmpty) then
    FOnAMQPBasicGetEmpty(self, aChannel);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicGetOkEvent(Sender: TObject;
  const aChannel: string; const aGetOk: TsgcAMQPFramePayload_Method_BasicGetOk;
  const aMessage: TsgcAMQPMessageContent);
begin
  if Assigned(FOnAMQPBasicGetOk) then
    FOnAMQPBasicGetOk(self, aChannel, aGetOk, aMessage);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPChallengeEvent(Sender: TObject;
  const aChallenge: String; var Challenge: String);
begin
  if Assigned(FOnAMQPChallenge) then
    FOnAMQPChallenge(self, aChallenge, Challenge);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPChannelCloseEvent
  (Sender: TObject; const aChannel: string;
  const aChannelClose: TsgcAMQPFramePayload_Method_ChannelClose; aAck: Boolean);
begin
  if Assigned(FOnAMQPChannelClose) then
    FOnAMQPChannelClose(self, aChannel, aChannelClose, aAck);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPChannelFlowEvent
  (Sender: TObject; const aChannel: string; aFlow, aAck: Boolean);
begin
  if Assigned(FOnAMQPChannelFlow) then
    FOnAMQPChannelFlow(self, aChannel, aFlow, aAck);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPChannelOpenEvent
  (Sender: TObject; const aChannel: string);
begin
  if Assigned(FOnAMQPChannelOpen) then
    FOnAMQPChannelOpen(self, aChannel);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPConnectedEvent(Sender: TObject;
  aVersion, aServer, aSession, aHeartBeat, aRawHeaders: String);
begin
  // DoAMQPConnectedEvent(aVersion, aServer, aSession, aHeartBeat, aRawHeaders);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPConnectEvent(Sender: TObject);
begin
  // ... enable / disable heartbeat in client
  if Assigned(Client) then
  begin
    // ... set heartbeat client properties
    Client.HeartBeat.Enabled := WSMessage.AMQPNegotiatedValues.HeartBeat > 0;
    Client.HeartBeat.Interval := WSMessage.AMQPNegotiatedValues.HeartBeat;
    Client.HeartBeat.Timeout := 0;

    // ... start client heartbeat
    if WSMessage.HeartBeat.Enabled then
      TsgcWSComponent_HeartBeat(Client).DoStartHeartBeat
    else
      TsgcWSComponent_HeartBeat(Client).DoStopHeartBeat;
  end;

  if Assigned(FOnAMQPConnect) then
    FOnAMQPConnect(self);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPDisconnectEvent(Sender: TObject;
  const aClose: TsgcAMQPFramePayload_Method_ConnectionClose; aAck: Boolean);
begin
  if Client.Active then
    Client.Active := false;

  if Assigned(FOnAMQPDisconnect) then
    FOnAMQPDisconnect(self, aClose, aAck);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPErrorEvent(Sender: TObject;
  aMessageText, aContentType: String; aContentLength: Integer;
  aReceiptId, aRawHeaders: String);
begin
  // DoAMQPErrorEvent(aMessageText, aContentType, aContentLength, aReceiptId,
  // aRawHeaders);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPExchangeDeclareEvent
  (Sender: TObject; const aChannel, aExchange: string);
begin
  if Assigned(FOnAMQPExchangeDeclare) then
    FOnAMQPExchangeDeclare(self, aChannel, aExchange);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPExchangeDeleteEvent
  (Sender: TObject; const aChannel, aExchange: string);
begin
  if Assigned(FOnAMQPExchangeDelete) then
    FOnAMQPExchangeDelete(self, aChannel, aExchange);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPHeartBeatEvent(Sender: TObject);
begin
  if Assigned(FOnAMQPHeartBeat) then
    FOnAMQPHeartBeat(self);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPMessageEvent(Sender: TObject;
  aMessageText, aDestination, aMessageId, aSubscription, aAck, aContentType,
  aRawHeaders: String);
begin
  // DoAMQPMessageEvent(aMessageText, aDestination, aMessageId, aSubscription,
  // aACK, aContentType, aRawHeaders);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicQoSEvent(Sender: TObject;
  const aChannel: string; const aQoS: TsgcAMQPFramePayload_Method_BasicQoS);
begin
  if Assigned(FOnAMQPBasicQoS) then
    FOnAMQPBasicQoS(self, aChannel, aQoS);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicRecoverOkEvent
  (Sender: TObject; const aChannel: string);
begin
  if Assigned(FOnAMQPBasicRecoverOk) then
    FOnAMQPBasicRecoverOk(self, aChannel);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPBasicReturnEvent
  (Sender: TObject; const aChannel: string;
  const aReturn: TsgcAMQPFramePayload_Method_BasicReturn;
  const aMessage: TsgcAMQPMessageContent);
begin
  if Assigned(FOnAMQPBasicReturn) then
    FOnAMQPBasicReturn(self, aChannel, aReturn, aMessage);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPExceptionEvent(Sender: TObject;
  E: Exception);
begin
  if Assigned(FOnAMQPException) then
    FOnAMQPException(self, E);

  if Client.Active then
    Client.Active := false;
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPQueueBindEvent(Sender: TObject;
  const aChannel, aQueue, aExchange: string);
begin
  if Assigned(FOnAMQPQueueBind) then
    FOnAMQPQueueBind(self, aChannel, aQueue, aExchange);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPQueueDeclareEvent
  (Sender: TObject; const aChannel, aQueue: string;
  aMessageCount, aConsumerCount: Integer);
begin
  if Assigned(FOnAMQPQueueDeclare) then
    FOnAMQPQueueDeclare(self, aChannel, aQueue, aMessageCount, aConsumerCount);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPQueueDeleteEvent
  (Sender: TObject; const aChannel, aQueue: string; aMessageCount: Integer);
begin
  if Assigned(FOnAMQPQueueDelete) then
    FOnAMQPQueueDelete(self, aChannel, aQueue, aMessageCount);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPQueuePurgeEvent(Sender: TObject;
  const aChannel, aQueue: string; aMessageCount: Integer);
begin
  if Assigned(FOnAMQPQueuePurge) then
    FOnAMQPQueuePurge(self, aChannel, aQueue, aMessageCount);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPQueueUnBindEvent
  (Sender: TObject; const aChannel, aQueue, aExchange: string);
begin
  if Assigned(FOnAMQPQueueUnBind) then
    FOnAMQPQueueUnBind(self, aChannel, aQueue, aExchange);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPReceiptEvent(Sender: TObject;
  aReceiptId, aRawHeaders: String);
begin
  // DoAMQPReceiptEvent(aReceiptId, aRawHeaders);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPTransactionOkEvent
  (Sender: TObject; const aChannel: string; aTransaction: TsgcAMQPTransaction);
begin
  if Assigned(FOnAMQPTransactionOk) then
    FOnAMQPTransactionOk(self, aChannel, aTransaction);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OnAMQPWriteBytesEvent(Sender: TObject;
  const aBytes: TBytes);
begin
  DoWriteAMQP(aBytes);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.OpenChannel(const aChannel: string);
begin
  WSMessage.OpenChannel(aChannel);
end;

function TsgcWSProtocol_AMQP_Client_Base.OpenChannelEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.OpenChannelEx(aChannel, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.PublishMessage(const aChannel,
  aExchange, aRoutingKey, aMessage: string; aMandatory: Boolean = false;
  aImmediate: Boolean = false);
begin
  WSMessage.PublishMessage(aChannel, aExchange, aRoutingKey, aMessage,
    aMandatory, aImmediate);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.PublishMessage(const aChannel,
  aExchange, aRoutingKey: string; const aHeader: TsgcAMQPBasicProperties;
  const aMessage: string; aMandatory: Boolean = false;
  aImmediate: Boolean = false);
begin
  WSMessage.PublishMessage(aChannel, aExchange, aRoutingKey, aHeader, aMessage,
    aMandatory, aImmediate);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.PublishMessage(const aChannel,
  aExchange, aRoutingKey: string; aMessage: TStream;
  aMandatory: Boolean = false; aImmediate: Boolean = false);
begin
  WSMessage.PublishMessage(aChannel, aExchange, aRoutingKey, aMessage,
    aMandatory, aImmediate);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.PublishMessage(const aChannel,
  aExchange, aRoutingKey: string; const aHeader: TsgcAMQPBasicProperties;
  const aMessage: TStream; aMandatory: Boolean = false;
  aImmediate: Boolean = false);
begin
  WSMessage.PublishMessage(aChannel, aExchange, aRoutingKey, aHeader, aMessage,
    aMandatory, aImmediate);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.PurgeQueue(const aChannel,
  aQueue: string; aNoWait: Boolean = false);
begin
  WSMessage.PurgeQueue(aChannel, aQueue, aNoWait);
end;

function TsgcWSProtocol_AMQP_Client_Base.PurgeQueueEx(const aChannel,
  aQueue: string; aNoWait: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.PurgeQueueEx(aChannel, aQueue, aNoWait, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.Recover(const aChannel: string;
  aRequeue: Boolean = false);
begin
  WSMessage.Recover(aChannel, aRequeue);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.RecoverAsync(const aChannel: string;
  aRequeue: Boolean = false);
begin
  WSMessage.RecoverAsync(aChannel, aRequeue);
end;

function TsgcWSProtocol_AMQP_Client_Base.RecoverEx(const aChannel: string;
  aRequeue: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.RecoverEx(aChannel, aRequeue, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.RejectMessage(const aChannel: string;
  aDeliveryTag: UInt64; aRequeue: Boolean = false);
begin
  WSMessage.RejectMessage(aChannel, aDeliveryTag, aRequeue);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.RollbackTransaction
  (const aChannel: string);
begin
  WSMessage.RollbackTransaction(aChannel);
end;

function TsgcWSProtocol_AMQP_Client_Base.RollbackTransactionEx
  (const aChannel: string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT)
  : Boolean;
begin
  Result := WSMessage.RollbackTransactionEx(aChannel, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.SelectTransaction
  (const aChannel: string);
begin
  WSMessage.SelectTransaction(aChannel);
end;

function TsgcWSProtocol_AMQP_Client_Base.SelectTransactionEx(const aChannel
  : string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.SelectTransactionEx(aChannel, aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.SetAuthentication
  (const Value: TsgcWSAMQPAuthentication_Options);
begin
  FAuthentication.Assign(Value);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.SetHeartBeat
  (const Value: TsgcWSAMQPHeartBeat_Options);
begin
  FHeartBeat.Assign(Value);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.SetAMQPOptions
  (const Value: TsgcWSAMQP_Options);
begin
  FAMQPOptions.Assign(Value);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.SetQoS(const aChannel: string;
  aPrefetchSize: Cardinal = 0; aPrefetchCount: Word = 0;
  aGlobal: Boolean = false);
begin
  WSMessage.SetQoS(aChannel, aPrefetchSize, aPrefetchCount, aGlobal);
end;

function TsgcWSProtocol_AMQP_Client_Base.SetQoSEx(const aChannel: string;
  aPrefetchSize: Cardinal = 0; aPrefetchCount: Word = 0;
  aGlobal: Boolean = false;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.SetQoSEx(aChannel, aPrefetchSize, aPrefetchCount, aGlobal,
    aTimeout);
end;

procedure TsgcWSProtocol_AMQP_Client_Base.UnBindQueue(const aChannel, aQueue,
  aExchange, aRoutingKey: string);
begin
  WSMessage.UnBindQueue(aChannel, aQueue, aExchange, aRoutingKey);
end;

function TsgcWSProtocol_AMQP_Client_Base.UnBindQueueEx(const aChannel, aQueue,
  aExchange, aRoutingKey: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  Result := WSMessage.UnBindQueueEx(aChannel, aQueue, aExchange, aRoutingKey,
    aTimeout);
end;

constructor TsgcWSAMQPAuthentication_Options.Create;
begin
  inherited;
  Enabled := false;
  UserName := '';
  Password := '';
end;

procedure TsgcWSAMQPAuthentication_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAMQPAuthentication_Options then
  begin
    Enabled := TsgcWSAMQPAuthentication_Options(aSource).Enabled;
    UserName := TsgcWSAMQPAuthentication_Options(aSource).UserName;
    Password := TsgcWSAMQPAuthentication_Options(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSAMQPHeartBeat_Options.Create;
begin
  inherited;
  Enabled := True;
  Interval := 60;
end;

procedure TsgcWSAMQPHeartBeat_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAMQPHeartBeat_Options then
  begin
    Enabled := TsgcWSAMQPHeartBeat_Options(aSource).Enabled;
    Interval := TsgcWSAMQPHeartBeat_Options(aSource).Interval;
    Timeout := TsgcWSAMQPHeartBeat_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSAMQP_Options.Create;
begin
  inherited;
  VirtualHost := CS_AMQP_DEFAULT_VIRTUAL_HOST;
  MaxChannels := CS_AMQP_DEFAULT_MAX_CHANNELS;
  MaxFrameSize := CS_AMQP_DEFAULT_MAX_FRAME_SIZE;
  Locale := CS_AMQP_DEFAULT_LOCALE;
end;

procedure TsgcWSAMQP_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAMQP_Options then
  begin
    VirtualHost := TsgcWSAMQP_Options(aSource).VirtualHost;
    Locale := TsgcWSAMQP_Options(aSource).Locale;
    MaxChannels := TsgcWSAMQP_Options(aSource).MaxChannels;
    MaxFrameSize := TsgcWSAMQP_Options(aSource).MaxFrameSize;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
