{ ***************************************************************************
  sgcAMQP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcAMQP_Classes;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes_Queues, sgcWebSocket_Classes_SyncObjs, sgcBase_Helpers;

type
  TsgcAMQPThreadExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

  TsgcAMQPException = class(Exception)
  private
    FCode: Integer;
  public
    property Code: Integer read FCode write FCode;
  end;

type
  TsgcAMQPFrameType = (amqpFrameNone, amqpFrameMethod, amqpFrameHeader,
    amqpFrameBody, amqpFrameHeartBeat);

type
  TsgcAMQPClass = (amqpClassNone, amqpClassConnection, amqpClassChannel,
    amqpClassExchange, amqpClassQueue, amqpClassBasic, amqpClassTx);

type
  TsgcAMQPMethod = (amqpMethodNone,
    // connections
    amqpConnNone, amqpConnStart, amqpConnStartOk, amqpConnSecure,
    amqpConnSecureOk, amqpConnTune, amqpConnTuneOk, amqpConnOpen,
    amqpConnOpenOk, amqpConnClose, amqpConnCloseOk,
    // channels
    amqpChannNone, amqpChannOpen, amqpChannOpenOk, amqpChannFlow,
    amqpChannFlowOk, amqpChannClose, amqpChannCloseOk,
    // exchange
    amqpExchNone, amqpExchDeclare, amqpExchDeclareOk, amqpExchDelete,
    amqpExchDeleteOk,
    // queue
    amqpQueueNone, amqpQueueDeclare, amqpQueueDeclareOk, amqpQueueBind,
    amqpQueueBindOk, amqpQueueUnBind, amqpQueueUnBindOk, amqpQueuePurge,
    amqpQueuePurgeOk, amqpQueueDelete, amqpQueueDeleteOk,
    // basic
    amqpBasicNone, amqpBasicQos, amqpBasicQosOk, amqpBasicConsume,
    amqpBasicConsumeOk, amqpBasicCancel, amqpBasicCancelOk, amqpBasicPublish,
    amqpBasicReturn, amqpBasicDeliver, amqpBasicGet, amqpBasicGetOk,
    amqpBasicGetEmpty, amqpBasicAck, amqpBasicReject, amqpBasicRecoverAsync,
    amqpBasicRecover, amqpBasicRecoverOk,
    // tx
    amqpTxNone, amqpTxSelect, amqpTxSelectOk, amqpTxCommit, amqpTxCommitOk,
    amqpTxRollback, amqpTxRollbackOk);

  TsgcAMQPMethods = set of TsgcAMQPMethod;

type
  TsgcAMQPTransaction = (amqpTransactionSelect, amqpTransactionCommit,
    amqpTransactionRollback);

type
  TsgcAMQPAuthentication = (amqpAuthNone, amqpAuthAnnonymous, amqpAuthPlain,
    amqpAuthAMQPlain, amqpAuthExternal);
  TsgcAMQPAuthentications = set of TsgcAMQPAuthentication;
  TsgcAMQPMessageDeliveryMode = (amqpMDMNone, amqpMDMNonPersistent,
    amqpMDMPersistent);

type
  TsgcAMQPBasicProperties = class
  private
    FOffset: Integer;
  private
    FAppId: string;
    FContentEncoding: string;
    FContentType: string;
    FCorrelationId: string;
    FDeliveryMode: TsgcAMQPMessageDeliveryMode;
    FExpiration: string;
    FHeaders: string;
    FMessageId: string;
    FPriority: UInt8;
    FReplyTo: string;
    FReserved: string;
    FTimeStamp: UInt64;
    FUserId: string;
    F_Type: string;
  private
    procedure DoInitialize;
  public
    constructor Create; virtual;
  public
    procedure Read(const aBytes: TBytes; aOffset: Integer = 0);
    function Write: TBytes;
  public
    property ContentType: string read FContentType write FContentType;
    property ContentEncoding: string read FContentEncoding
      write FContentEncoding;
    property Headers: string read FHeaders write FHeaders;
    property DeliveryMode: TsgcAMQPMessageDeliveryMode read FDeliveryMode
      write FDeliveryMode;
    property Priority: UInt8 read FPriority write FPriority;
    property CorrelationId: string read FCorrelationId write FCorrelationId;
    property ReplyTo: string read FReplyTo write FReplyTo;
    property Expiration: string read FExpiration write FExpiration;
    property MessageId: string read FMessageId write FMessageId;
    property TimeStamp: UInt64 read FTimeStamp write FTimeStamp;
    property _Type: string read F_Type write F_Type;
    property UserId: string read FUserId write FUserId;
    property AppId: string read FAppId write FAppId;
    property Reserved: string read FReserved write FReserved;
  end;

  TsgcAMQPHeader = class
  private
    F_Type: TsgcAMQPFrameType;
    FChannel: Word;
    FSize: Integer;
  public
    procedure Clear;
  public
    procedure Read(const aBytes: TBytes; aOffset: Integer);
  public
    property _Type: TsgcAMQPFrameType read F_Type write F_Type;
    property Channel: Word read FChannel write FChannel;
    property Size: Integer read FSize write FSize;
  end;

  TsgcAMQPPayload = class
  private
    FPayload: TBytes;
  public
    procedure Clear;
  public
    procedure Read(const aBytes: TBytes; aSize: Integer; aOffset: Integer = 0);
  public
    property Payload: TBytes read FPayload;
  end;

  TsgcAMQPFramePayloadType = class
  protected
    procedure DoRead(const aBytes: TBytes); virtual;
    function DoWrite: TBytes; virtual;
  public
    constructor Create; virtual;
  public
    procedure Read(const aBytes: TBytes);
    function Write: TBytes;
  private
    FOnReadFramePayload: TNotifyEvent;
  protected
    property OnReadFramePayload: TNotifyEvent read FOnReadFramePayload
      write FOnReadFramePayload;
  end;

  TsgcAMQPFramePayload_Base = class
  protected
    FClassId: TsgcAMQPClass;
    FMethodId: TsgcAMQPMethod;
  protected
    FOffset: Integer;
    procedure DoRead(const aBytes: TBytes); virtual;
    function DoWrite: TBytes; virtual;
  public
    constructor Create; virtual;
  public
    procedure Read(const aBytes: TBytes);
    function Write: TBytes;
  end;

  TsgcAMQPFramePayload_Method = class(TsgcAMQPFramePayload_Base)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  end;

  TsgcAMQPFramePayload_Method_Connection = class(TsgcAMQPFramePayload_Method)
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_ConnectionStart = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FLocales: string;
    FMechanisms: string;
    FServerProperties: TStringList;
    FVersionMajor: Byte;
    FVersionMinor: Byte;
    function GetServerProperties: TStringList;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Locales: string read FLocales write FLocales;
    property Mechanisms: string read FMechanisms write FMechanisms;
    property ServerProperties: TStringList read GetServerProperties
      write FServerProperties;
    property VersionMajor: Byte read FVersionMajor write FVersionMajor;
    property VersionMinor: Byte read FVersionMinor write FVersionMinor;
  end;

  TsgcAMQPFramePayload_Method_ConnectionStartOk = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FClientProperties: TStringList;
    FLocale: string;
    FMechanism: string;
    FPassword: string;
    FUser: string;
    function GetClientProperties: TStringList;
    function GetMechanismType: TsgcAMQPAuthentication;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property ClientProperties: TStringList read GetClientProperties
      write FClientProperties;
    property Locale: string read FLocale write FLocale;
    property Mechanism: string read FMechanism write FMechanism;
    property MechanismType: TsgcAMQPAuthentication read GetMechanismType;
    property Password: string read FPassword write FPassword;
    property User: string read FUser write FUser;
  end;

  TsgcAMQPFramePayload_Method_ConnectionSecure = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FChallenge: String;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property Challenge: String read FChallenge write FChallenge;
  end;

  TsgcAMQPFramePayload_Method_ConnectionSecureOk = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FChallenge: String;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Challenge: String read FChallenge write FChallenge;
  end;

  TsgcAMQPFramePayload_Method_ConnectionTune = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FChannelMax: Word;
    FFrameMax: Cardinal;
    FHeartBeat: Word;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property ChannelMax: Word read FChannelMax write FChannelMax;
    property FrameMax: Cardinal read FFrameMax write FFrameMax;
    property HeartBeat: Word read FHeartBeat write FHeartBeat;
  end;

  TsgcAMQPFramePayload_Method_ConnectionTuneOk = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FChannelMax: Word;
    FFrameMax: Cardinal;
    FHeartBeat: Word;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property ChannelMax: Word read FChannelMax write FChannelMax;
    property FrameMax: Cardinal read FFrameMax write FFrameMax;
    property HeartBeat: Word read FHeartBeat write FHeartBeat;
  end;

  TsgcAMQPFramePayload_Method_ConnectionOpen = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FReserved1: String;
    FReserved2: Boolean;
    FVirtualHost: string;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property VirtualHost: string read FVirtualHost write FVirtualHost;
    property Reserved1: String read FReserved1 write FReserved1;
    property Reserved2: Boolean read FReserved2 write FReserved2;
  end;

  TsgcAMQPFramePayload_Method_ConnectionOpenOk = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FReserved1: String;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property Reserved1: String read FReserved1 write FReserved1;
  end;

  TsgcAMQPFramePayload_Method_ConnectionClose = class
    (TsgcAMQPFramePayload_Method_Connection)
  private
    FFailClassId: TsgcAMQPClass;
    FFailMethodId: TsgcAMQPMethod;
    FReplyCode: Word;
    FReplyText: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property ReplyCode: Word read FReplyCode write FReplyCode;
    property ReplyText: string read FReplyText write FReplyText;
    property FailClassId: TsgcAMQPClass read FFailClassId write FFailClassId;
    property FailMethodId: TsgcAMQPMethod read FFailMethodId
      write FFailMethodId;
  end;

  TsgcAMQPFramePayload_Method_ConnectionCloseOk = class
    (TsgcAMQPFramePayload_Method_Connection)
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_Channel = class(TsgcAMQPFramePayload_Method)
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_ChannelOpen = class
    (TsgcAMQPFramePayload_Method_Channel)
  private
    FReserved1: String;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: String read FReserved1 write FReserved1;
  end;

  TsgcAMQPFramePayload_Method_ChannelOpenOk = class
    (TsgcAMQPFramePayload_Method_Channel)
  private
    FReserved1: String;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property Reserved1: String read FReserved1 write FReserved1;
  end;

  TsgcAMQPFramePayload_Method_ChannelFlow = class
    (TsgcAMQPFramePayload_Method_Channel)
  private
    FActive: Boolean;
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Active: Boolean read FActive write FActive;
  end;

  TsgcAMQPFramePayload_Method_ChannelFlowOk = class
    (TsgcAMQPFramePayload_Method_Channel)
  private
    FActive: Boolean;
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Active: Boolean read FActive write FActive;
  end;

  TsgcAMQPFramePayload_Method_ChannelClose = class
    (TsgcAMQPFramePayload_Method_Channel)
  private
    FFailClassId: TsgcAMQPClass;
    FFailMethodId: TsgcAMQPMethod;
    FReplyCode: Word;
    FReplyText: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property ReplyCode: Word read FReplyCode write FReplyCode;
    property ReplyText: string read FReplyText write FReplyText;
    property FailClassId: TsgcAMQPClass read FFailClassId write FFailClassId;
    property FailMethodId: TsgcAMQPMethod read FFailMethodId
      write FFailMethodId;
  end;

  TsgcAMQPFramePayload_Method_ChannelCloseOk = class
    (TsgcAMQPFramePayload_Method_Channel)
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_Exchange = class(TsgcAMQPFramePayload_Method)
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_ExchangeDeclare = class
    (TsgcAMQPFramePayload_Method_Exchange)
  private
    FArguments: string;
    FDurable: Boolean;
    FExchange: string;
    FNoWait: Boolean;
    FPassive: Boolean;
    FReserved1: UInt16;
    FAutoDelete: Boolean;
    FInternal: Boolean;
    FExchangeType: string;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Exchange: string read FExchange write FExchange;
    property ExchangeType: string read FExchangeType write FExchangeType;
    property Passive: Boolean read FPassive write FPassive;
    property Durable: Boolean read FDurable write FDurable;
    property AutoDelete: Boolean read FAutoDelete write FAutoDelete;
    property Internal: Boolean read FInternal write FInternal;
    property NoWait: Boolean read FNoWait write FNoWait;
    property Arguments: string read FArguments write FArguments;
  end;

  TsgcAMQPFramePayload_Method_ExchangeDeclareOk = class
    (TsgcAMQPFramePayload_Method_Exchange)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_ExchangeDelete = class
    (TsgcAMQPFramePayload_Method_Exchange)
  private
    FExchange: string;
    FIfUnused: Boolean;
    FNoWait: Boolean;
    FReserved1: UInt16;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Exchange: string read FExchange write FExchange;
    property IfUnused: Boolean read FIfUnused write FIfUnused;
    property NoWait: Boolean read FNoWait write FNoWait;
  end;

  TsgcAMQPFramePayload_Method_ExchangeDeleteOk = class
    (TsgcAMQPFramePayload_Method_Exchange)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_Queue = class(TsgcAMQPFramePayload_Method)
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_QueueDeclare = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FArguments: string;
    FDurable: Boolean;
    FQueue: string;
    FNoWait: Boolean;
    FPassive: Boolean;
    FReserved1: UInt16;
    FAutoDelete: Boolean;
    FExclusive: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: string read FQueue write FQueue;
    property Passive: Boolean read FPassive write FPassive;
    property Durable: Boolean read FDurable write FDurable;
    property Exclusive: Boolean read FExclusive write FExclusive;
    property AutoDelete: Boolean read FAutoDelete write FAutoDelete;
    property NoWait: Boolean read FNoWait write FNoWait;
    property Arguments: string read FArguments write FArguments;
  end;

  TsgcAMQPFramePayload_Method_QueueDeclareOk = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FConsumerCount: Int32;
    FMessageCount: Int32;
    FQueue: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property Queue: string read FQueue write FQueue;
    property MessageCount: Int32 read FMessageCount write FMessageCount;
    property ConsumerCount: Int32 read FConsumerCount write FConsumerCount;
  end;

  TsgcAMQPFramePayload_Method_QueueBind = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FArguments: string;
    FQueue: string;
    FNoWait: Boolean;
    FRoutingKey: string;
    FReserved1: UInt16;
    FExchange: string;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: string read FQueue write FQueue;
    property Exchange: string read FExchange write FExchange;
    property RoutingKey: string read FRoutingKey write FRoutingKey;
    property NoWait: Boolean read FNoWait write FNoWait;
    property Arguments: string read FArguments write FArguments;
  end;

  TsgcAMQPFramePayload_Method_QueueBindOk = class
    (TsgcAMQPFramePayload_Method_Queue)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_QueueUnBind = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FArguments: string;
    FQueue: string;
    FRoutingKey: string;
    FReserved1: UInt16;
    FExchange: string;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: string read FQueue write FQueue;
    property Exchange: string read FExchange write FExchange;
    property RoutingKey: string read FRoutingKey write FRoutingKey;
    property Arguments: string read FArguments write FArguments;
  end;

  TsgcAMQPFramePayload_Method_QueueUnBindOk = class
    (TsgcAMQPFramePayload_Method_Queue)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_QueuePurge = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FQueue: string;
    FNoWait: Boolean;
    FReserved1: UInt16;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: string read FQueue write FQueue;
    property NoWait: Boolean read FNoWait write FNoWait;
  end;

  TsgcAMQPFramePayload_Method_QueuePurgeOk = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FMessageCount: Int32;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property MessageCount: Int32 read FMessageCount write FMessageCount;
  end;

  TsgcAMQPFramePayload_Method_QueueDelete = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FQueue: string;
    FNoWait: Boolean;
    FIfEmpty: Boolean;
    FReserved1: UInt16;
    FIfUnused: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: string read FQueue write FQueue;
    property IfUnused: Boolean read FIfUnused write FIfUnused;
    property IfEmpty: Boolean read FIfEmpty write FIfEmpty;
    property NoWait: Boolean read FNoWait write FNoWait;
  end;

  TsgcAMQPFramePayload_Method_QueueDeleteOk = class
    (TsgcAMQPFramePayload_Method_Queue)
  private
    FMessageCount: Int32;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property MessageCount: Int32 read FMessageCount write FMessageCount;
  end;

  TsgcAMQPFramePayload_Method_Basic = class(TsgcAMQPFramePayload_Method)
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_BasicQoS = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FGlobal: Boolean;
    FPrefetchCount: Word;
    FPrefetchSize: Cardinal;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property PrefetchSize: Cardinal read FPrefetchSize write FPrefetchSize;
    property PrefetchCount: Word read FPrefetchCount write FPrefetchCount;
    property Global: Boolean read FGlobal write FGlobal;
  end;

  TsgcAMQPFramePayload_Method_BasicQoSOk = class
    (TsgcAMQPFramePayload_Method_Basic)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_BasicConsume = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FArguments: string;
    FConsumerTag: string;
    FExclusive: Boolean;
    FNoAck: Boolean;
    FNoLocal: Boolean;
    FNoWait: Boolean;
    FQueue: string;
    FReserved1: UInt16;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: string read FQueue write FQueue;
    property ConsumerTag: string read FConsumerTag write FConsumerTag;
    property NoLocal: Boolean read FNoLocal write FNoLocal;
    property NoAck: Boolean read FNoAck write FNoAck;
    property Exclusive: Boolean read FExclusive write FExclusive;
    property NoWait: Boolean read FNoWait write FNoWait;
    property Arguments: string read FArguments write FArguments;
  end;

  TsgcAMQPFramePayload_Method_BasicConsumeOk = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FConsumerTag: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property ConsumerTag: string read FConsumerTag write FConsumerTag;
  end;

  TsgcAMQPFramePayload_Method_BasicCancel = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FConsumerTag: string;
    FNoWait: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property ConsumerTag: string read FConsumerTag write FConsumerTag;
    property NoWait: Boolean read FNoWait write FNoWait;
  end;

  TsgcAMQPFramePayload_Method_BasicCancelOk = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FConsumerTag: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property ConsumerTag: string read FConsumerTag write FConsumerTag;
  end;

  TsgcAMQPFramePayload_Method_BasicPublish = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FExchange: string;
    FImmediate: Boolean;
    FMandatory: Boolean;
    FReserved1: UInt16;
    FRoutingKey: string;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Exchange: string read FExchange write FExchange;
    property RoutingKey: string read FRoutingKey write FRoutingKey;
    property Mandatory: Boolean read FMandatory write FMandatory;
    property Immediate: Boolean read FImmediate write FImmediate;
  end;

  TsgcAMQPFramePayload_Method_BasicReturn = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FExchange: string;
    FReplyCode: Word;
    FReplyText: string;
    FRoutingKey: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property ReplyCode: Word read FReplyCode write FReplyCode;
    property ReplyText: string read FReplyText write FReplyText;
    property Exchange: string read FExchange write FExchange;
    property RoutingKey: string read FRoutingKey write FRoutingKey;
  end;

  TsgcAMQPFramePayload_Method_BasicDeliver = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FRedelivered: Boolean;
    FConsumerTag: string;
    FDeliveryTag: UInt64;
    FExchange: string;
    FRoutingKey: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property ConsumerTag: string read FConsumerTag write FConsumerTag;
    property DeliveryTag: UInt64 read FDeliveryTag write FDeliveryTag;
    property Redelivered: Boolean read FRedelivered write FRedelivered;
    property Exchange: string read FExchange write FExchange;
    property RoutingKey: string read FRoutingKey write FRoutingKey;
  end;

  TsgcAMQPFramePayload_Method_BasicGet = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FNoAck: Boolean;
    FReserved1: UInt16;
    FQueue: String;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
    property Queue: String read FQueue write FQueue;
    property NoAck: Boolean read FNoAck write FNoAck;
  end;

  TsgcAMQPFramePayload_Method_BasicGetOk = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FRedelivered: Boolean;
    FMessageCount: Int32;
    FDeliveryTag: UInt64;
    FExchange: string;
    FRoutingKey: string;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property DeliveryTag: UInt64 read FDeliveryTag write FDeliveryTag;
    property Redelivered: Boolean read FRedelivered write FRedelivered;
    property Exchange: string read FExchange write FExchange;
    property RoutingKey: string read FRoutingKey write FRoutingKey;
    property MessageCount: Int32 read FMessageCount write FMessageCount;
  end;

  TsgcAMQPFramePayload_Method_BasicGetEmpty = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FReserved1: UInt16;
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  public
    property Reserved1: UInt16 read FReserved1 write FReserved1;
  end;

  TsgcAMQPFramePayload_Method_BasicAck = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FDeliveryTag: UInt64;
    FMultiple: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property DeliveryTag: UInt64 read FDeliveryTag write FDeliveryTag;
    property Multiple: Boolean read FMultiple write FMultiple;
  end;

  TsgcAMQPFramePayload_Method_BasicReject = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FDeliveryTag: UInt64;
    FRequeue: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property DeliveryTag: UInt64 read FDeliveryTag write FDeliveryTag;
    property Requeue: Boolean read FRequeue write FRequeue;
  end;

  TsgcAMQPFramePayload_Method_BasicRecoverAsync = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FRequeue: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Requeue: Boolean read FRequeue write FRequeue;
  end;

  TsgcAMQPFramePayload_Method_BasicRecover = class
    (TsgcAMQPFramePayload_Method_Basic)
  private
    FRequeue: Boolean;
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  public
    property Requeue: Boolean read FRequeue write FRequeue;
  end;

  TsgcAMQPFramePayload_Method_BasicRecoverOk = class
    (TsgcAMQPFramePayload_Method_Basic)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_Tx = class(TsgcAMQPFramePayload_Method)
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_TxSelect = class(TsgcAMQPFramePayload_Method_Tx)
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_TxSelectOk = class(TsgcAMQPFramePayload_Method_Tx)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_TxCommit = class(TsgcAMQPFramePayload_Method_Tx)
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_TxCommitOk = class(TsgcAMQPFramePayload_Method_Tx)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_TxRollback = class(TsgcAMQPFramePayload_Method_Tx)
  protected
    function DoWrite: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayload_Method_TxRollbackOk = class
    (TsgcAMQPFramePayload_Method_Tx)
  protected
    procedure DoRead(const aBytes: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcAMQPFramePayloadType_Method = class(TsgcAMQPFramePayloadType)
  private
    FClassId: TsgcAMQPClass;
    FMethodId: TsgcAMQPMethod;
    FPayload: TsgcAMQPFramePayload_Method;
    procedure SetPayload(const Value: TsgcAMQPFramePayload_Method);
  public
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    destructor Destroy; override;
  public
    property ClassId: TsgcAMQPClass read FClassId;
    property MethodId: TsgcAMQPMethod read FMethodId;
    property Payload: TsgcAMQPFramePayload_Method read FPayload
      write SetPayload;
  end;

  TsgcAMQPFramePayloadType_ContentHeader = class(TsgcAMQPFramePayloadType)
  private
    FOffset: Integer;
    FBodySize: UInt64;
    FClassId: TsgcAMQPClass;
    FWeight: UInt16;
    FHeader: TsgcAMQPBasicProperties;
    function GetHeader: TsgcAMQPBasicProperties;
    procedure SetHeader(const Value: TsgcAMQPBasicProperties);
  public
    destructor Destroy; override;
  public
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    property ClassId: TsgcAMQPClass read FClassId write FClassId;
    property Weight: UInt16 read FWeight write FWeight;
    property BodySize: UInt64 read FBodySize write FBodySize;
    property Header: TsgcAMQPBasicProperties read GetHeader write SetHeader;
  end;

  TsgcAMQPFramePayloadType_ContentBody = class(TsgcAMQPFramePayloadType)
  private
    FData: TStream;
    FFreeData: Boolean;
    FDataSize: Int64;
    function GetAsString: string;
    function GetData: TStream;
  protected
    procedure DoRead(const aBytes: TBytes); override;
    function DoWrite: TBytes; override;
  public
    destructor Destroy; override;
  public
    property AsString: string read GetAsString;
    property Data: TStream read GetData write FData;
    property DataSize: Int64 read FDataSize write FDataSize;
  end;

  TsgcAMQPFramePayloadType_HeartBeat = class(TsgcAMQPFramePayloadType)

  end;

  TsgcAMQPFrame = class
    { payload }
  private
    FPayload_Method: TsgcAMQPFramePayloadType_Method;
    FPayload_ContentHeader: TsgcAMQPFramePayloadType_ContentHeader;
    FPayload_ContentBody: TsgcAMQPFramePayloadType_ContentBody;
    FPayload_HeartBeat: TsgcAMQPFramePayloadType_HeartBeat;
    function GetPayload_Method: TsgcAMQPFramePayloadType_Method;
    function GetPayload_ContentHeader: TsgcAMQPFramePayloadType_ContentHeader;
    function GetPayload_ContentBody: TsgcAMQPFramePayloadType_ContentBody;
    function GetPayload_HeartBeat: TsgcAMQPFramePayloadType_HeartBeat;
  public
    property Payload_ContentBody: TsgcAMQPFramePayloadType_ContentBody
      read GetPayload_ContentBody write FPayload_ContentBody;
    property Payload_ContentHeader: TsgcAMQPFramePayloadType_ContentHeader
      read GetPayload_ContentHeader write FPayload_ContentHeader;
    property Payload_HeartBeat: TsgcAMQPFramePayloadType_HeartBeat
      read GetPayload_HeartBeat write FPayload_HeartBeat;
    property Payload_Method: TsgcAMQPFramePayloadType_Method
      read GetPayload_Method write FPayload_Method;
    { payload }

    { read }
  protected
    procedure OnReadFramePayloadEvent(Sender: TObject); virtual;
  protected
    procedure DoReadPayload; virtual;
    { read }

    { write }
  protected
    function DoWrite: TBytes;
  public
    function Write: TBytes;
    { write }

    { frame }
  private
    FHeader: TsgcAMQPHeader;
    FPayload: TsgcAMQPPayload;
    function GetHeader: TsgcAMQPHeader;
    function GetPayload: TsgcAMQPPayload;
  protected
    procedure DoValidateFrame(const aBytes: TBytes; aOffset: Integer); virtual;
  public
    procedure Clear;
  public
    function Read(const aBytes: TBytes; aOffset: Integer = 0): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property Header: TsgcAMQPHeader read GetHeader write FHeader;
    property Payload: TsgcAMQPPayload read GetPayload write FPayload;
    { frame }

    { properties }
  private
    FMaxSize: Integer;
  public
    property MaxSize: Integer read FMaxSize write FMaxSize;
    { properties }

    { events }
  private
    FOnReadFrame: TNotifyEvent;
  public
    property OnReadFrame: TNotifyEvent read FOnReadFrame write FOnReadFrame;
    { events }
  end;

  TsgcAMQPMessageContent = class
  private
    FHeader: TsgcAMQPFramePayloadType_ContentHeader;
    FBody: TsgcAMQPFramePayloadType_ContentBody;
    FPayload: TsgcAMQPFramePayload_Method_Basic;
    function GetBody: TsgcAMQPFramePayloadType_ContentBody;
    function GetHeader: TsgcAMQPFramePayloadType_ContentHeader;
    procedure SetBody(const Value: TsgcAMQPFramePayloadType_ContentBody);
    procedure SetHeader(const Value: TsgcAMQPFramePayloadType_ContentHeader);
    procedure SetPayload(const Value: TsgcAMQPFramePayload_Method_Basic);
  protected
    procedure DoClear; virtual;
  public
    destructor Destroy; override;
  public
    procedure Clear;
  public
    property Header: TsgcAMQPFramePayloadType_ContentHeader read GetHeader
      write SetHeader;
    property Body: TsgcAMQPFramePayloadType_ContentBody read GetBody
      write SetBody;
    property Payload: TsgcAMQPFramePayload_Method_Basic read FPayload
      write SetPayload;
  end;

  TsgcAMQPChannelThread = class(TThread)
    { from TThread }
  public
    destructor Destroy; override;
  public
    procedure Execute; override;
    { from TThread }

    { frames }
  private
    FFrames: TsgcQueue;
    function GetFrames: TsgcQueue;
  protected
    property Frames: TsgcQueue read GetFrames write FFrames;
  public
    procedure AddFrame(const aFrame: TsgcAMQPFrame);
    { frames }

    { properties }
  private
    FOnException: TsgcAMQPThreadExceptionEvent;
  public
    property OnException: TsgcAMQPThreadExceptionEvent read FOnException
      write FOnException;
    { properties }

    { events }
  private
    FOnAfterReadFrame: TNotifyEvent;
  public
    property OnAfterReadFrame: TNotifyEvent read FOnAfterReadFrame
      write FOnAfterReadFrame;
    { events }
  end;

  TsgcAMQPChannelThreadWaitRequest = class
  private
    FMethod: TsgcAMQPMethod;
    FMethods: TsgcAMQPMethods;
    FTerminated: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Initialize;
  public
    property Method: TsgcAMQPMethod read FMethod write FMethod;
    property Methods: TsgcAMQPMethods read FMethods write FMethods;
    property Terminated: Boolean read FTerminated write FTerminated;
  end;

  TsgcAMQPChannelThreadRequest = class
  private
    FExchangeDeclare: string;
    FExchangeDelete: string;
    FQueueBindExchange: string;
    FQueueBindQueue: string;
    FQueueDelete: string;
    FQueuePurge: string;
    FQueueUnBindExchange: string;
    FQueueUnBindQueue: string;
  public
    property ExchangeDeclare: string read FExchangeDeclare
      write FExchangeDeclare;
    property ExchangeDelete: string read FExchangeDelete write FExchangeDelete;
    property QueueBindExchange: string read FQueueBindExchange
      write FQueueBindExchange;
    property QueueBindQueue: string read FQueueBindQueue write FQueueBindQueue;
    property QueueDelete: string read FQueueDelete write FQueueDelete;
    property QueuePurge: string read FQueuePurge write FQueuePurge;
    property QueueUnBindExchange: string read FQueueUnBindExchange
      write FQueueUnBindExchange;
    property QueueUnBindQueue: string read FQueueUnBindQueue
      write FQueueUnBindQueue;
  end;

  TsgcAMQPChannelThreadItem = class(TsgcQueueItemBase)
    { properties }
  private
    FChannel: string;
    FThread: TsgcAMQPChannelThread;
    FChannelClose: TsgcAMQPFramePayload_Method_ChannelClose;
    FQoS: TsgcAMQPFramePayload_Method_BasicQoS;
    FFlow: Boolean;
    FMethodId: TsgcAMQPMethod;
    FWaitRequest: TsgcAMQPChannelThreadWaitRequest;
    FRequest: TsgcAMQPChannelThreadRequest;
    function GetChannelClose: TsgcAMQPFramePayload_Method_ChannelClose;
    function GetQoS: TsgcAMQPFramePayload_Method_BasicQoS;
    procedure SetChannelClose(const Value
      : TsgcAMQPFramePayload_Method_ChannelClose);
    procedure SetQoS(const Value: TsgcAMQPFramePayload_Method_BasicQoS);
    function GetWaitRequest: TsgcAMQPChannelThreadWaitRequest;
    function GetRequest: TsgcAMQPChannelThreadRequest;
  protected
    procedure OnThreadExceptionEvent(Sender: TObject; E: Exception); virtual;
    procedure OnThreadAfterReadFrameEvent(Sender: TObject); virtual;
  public
    property Channel: string read FChannel write FChannel;
    property ChannelClose: TsgcAMQPFramePayload_Method_ChannelClose
      read GetChannelClose write SetChannelClose;
    property Flow: Boolean read FFlow write FFlow;
    property MethodId: TsgcAMQPMethod read FMethodId write FMethodId;
    property QoS: TsgcAMQPFramePayload_Method_BasicQoS read GetQoS write SetQoS;
    property WaitRequest: TsgcAMQPChannelThreadWaitRequest read GetWaitRequest
      write FWaitRequest;
    property Request: TsgcAMQPChannelThreadRequest read GetRequest
      write FRequest;
    { properties }

    { messages }
  private
    FMessageContent: TsgcAMQPMessageContent;
    function GetMessageContent: TsgcAMQPMessageContent;
  public
    property MessageContent: TsgcAMQPMessageContent read GetMessageContent
      write FMessageContent;
    { messages }

    { constructor / destructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  public
    procedure AddFrame(const aFrame: TsgcAMQPFrame);
    procedure Terminate;
    { methods }

    { events }
  private
    FOnException: TsgcAMQPThreadExceptionEvent;
    FOnTerminate: TNotifyEvent;
  public
    property OnException: TsgcAMQPThreadExceptionEvent read FOnException
      write FOnException;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    { events }
  end;

  TsgcAMQPChannelThreads = class(TsgcQueue)
  private
    FId: TsgcThreadSafeInteger;
    function GetNewChannelId: Word;
  protected
    procedure OnThreadTerminateEvent(Sender: TObject); virtual;
    procedure OnThreadExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure AddFrame(const aFrame: TsgcAMQPFrame);
    function AddChannel(const aChannel: string): Word;
    function GetChannel(const aChannel: string)
      : TsgcAMQPChannelThreadItem; overload;
    function GetChannel(const aId: Word): TsgcAMQPChannelThreadItem; overload;

    { events }
  private
    FOnException: TsgcAMQPThreadExceptionEvent;
  public
    property OnException: TsgcAMQPThreadExceptionEvent read FOnException
      write FOnException;
    { events }
  end;

implementation

uses
  sgcAMQP_Helpers, sgcAMQP_Const, sgcBase_Const;

procedure TsgcAMQPHeader.Clear;
begin
  F_Type := amqpFrameNone;
  FChannel := 0;
  FSize := 0;
end;

procedure TsgcAMQPHeader.Read(const aBytes: TBytes; aOffset: Integer);
var
  vOffset: Integer;
begin
  F_Type := sgcGetAMQPFrameType(aBytes[aOffset]);
  vOffset := aOffset + 1;
  FChannel := sgcReadAMQPUInt16(aBytes, vOffset);
  FSize := sgcReadAMQPUInt32(aBytes, vOffset);
end;

procedure TsgcAMQPPayload.Clear;
begin
  SetLength(FPayload, 0);
end;

procedure TsgcAMQPPayload.Read(const aBytes: TBytes; aSize: Integer;
  aOffset: Integer = 0);
begin
  if aSize > 0 then
  begin
    SetLength(FPayload, aSize);
    sgcMove(aBytes[aOffset], FPayload[0], aSize);
  end;
end;

constructor TsgcAMQPFrame.Create;
begin
  inherited;
  FMaxSize := CS_AMQP_DEFAULT_MAX_FRAME_SIZE;
end;

destructor TsgcAMQPFrame.Destroy;
begin
  sgcFree(FHeader);
  sgcFree(FPayload);
  sgcFree(FPayload_Method);
  sgcFree(FPayload_ContentHeader);
  sgcFree(FPayload_ContentBody);
  sgcFree(FPayload_HeartBeat);
  inherited;
end;

procedure TsgcAMQPFrame.Clear;
begin
  Header.Clear;
  Payload.Clear;
  sgcFree(FPayload_Method);
  sgcFree(FPayload_ContentHeader);
  sgcFree(FPayload_ContentBody);
  sgcFree(FPayload_HeartBeat);
end;

procedure TsgcAMQPFrame.DoReadPayload;
begin
  case Header._Type of
    amqpFrameMethod:
      Payload_Method.Read(Payload.Payload);
    amqpFrameHeader:
      Payload_ContentHeader.Read(Payload.Payload);
    amqpFrameBody:
      Payload_ContentBody.Read(Payload.Payload);
    amqpFrameHeartBeat:
      Payload_HeartBeat.Read(Payload.Payload);
  end;
end;

procedure TsgcAMQPFrame.DoValidateFrame(const aBytes: TBytes; aOffset: Integer);
begin
  // close connection if a peer receives a frame with a type that is not one of these defined types,
  if Header._Type = amqpFrameNone then
    DoRaiseAMQPException(CS_AMQP_ERROR_CLOSE_CONNECTION);
  // The channel number in content frames MUST NOT be zero
  if (Header.Channel = 0) and ((Header._Type = amqpFrameHeader) or
    (Header._Type = amqpFrameBody)) then
    DoRaiseAMQPException(CS_AMQP_ERROR_CHANNEL);
  // The channel number MUST be zero for all heartbeat frames, and for method, header and body frames
  // that refer to the Connection class
  if (Header.Channel > 0) and (Header._Type = amqpFrameHeartBeat) then
    DoRaiseAMQPException(CS_AMQP_ERROR_FRAME);
  // A peer MUST NOT send frames larger than the agreed-upon size
  if Header.Size > MaxSize then
    DoRaiseAMQPException(CS_AMQP_ERROR_FRAME);
  // The frame-end octet MUST always be the hexadecimal value $CE
  if aBytes[aOffset + CS_AMQP_FRAME_HEADER_LENGTH + Header.Size] <> CS_AMQP_FRAME_END
  then
    DoRaiseAMQPException(CS_AMQP_ERROR_FRAME);

end;

function TsgcAMQPFrame.DoWrite: TBytes;
var
  vPayload: TBytes;
begin
  case Header._Type of
    amqpFrameMethod:
      vPayload := Payload_Method.Write;
    amqpFrameHeader:
      vPayload := Payload_ContentHeader.Write;
    amqpFrameBody:
      vPayload := Payload_ContentBody.Write;
    amqpFrameHeartBeat:
      vPayload := Payload_HeartBeat.Write;
  end;

  // ... header
  sgcWriteAMQPByte(sgcGetAMQPFrameTypeValue(Header._Type), result);
  sgcWriteAMQPUInt16(Header.Channel, result);
  sgcWriteAMQPUInt32(Length(vPayload), result);

  // ... payload
  sgcWriteBytes(vPayload, result);

  // ... end
  sgcWriteByte(CS_AMQP_FRAME_END, result);
end;

function TsgcAMQPFrame.GetHeader: TsgcAMQPHeader;
begin
  if not Assigned(FHeader) then
    FHeader := TsgcAMQPHeader.Create;
  result := FHeader;
end;

function TsgcAMQPFrame.GetPayload: TsgcAMQPPayload;
begin
  if not Assigned(FPayload) then
    FPayload := TsgcAMQPPayload.Create;
  result := FPayload;
end;

function TsgcAMQPFrame.GetPayload_ContentBody
  : TsgcAMQPFramePayloadType_ContentBody;
begin
  if not Assigned(FPayload_ContentBody) then
  begin
    FPayload_ContentBody := TsgcAMQPFramePayloadType_ContentBody.Create;
    FPayload_ContentBody.OnReadFramePayload := OnReadFramePayloadEvent;
  end;
  result := FPayload_ContentBody;
end;

function TsgcAMQPFrame.GetPayload_ContentHeader
  : TsgcAMQPFramePayloadType_ContentHeader;
begin
  if not Assigned(FPayload_ContentHeader) then
  begin
    FPayload_ContentHeader := TsgcAMQPFramePayloadType_ContentHeader.Create;
    FPayload_ContentHeader.OnReadFramePayload := OnReadFramePayloadEvent;
  end;
  result := FPayload_ContentHeader;
end;

function TsgcAMQPFrame.GetPayload_HeartBeat: TsgcAMQPFramePayloadType_HeartBeat;
begin
  if not Assigned(FPayload_HeartBeat) then
  begin
    FPayload_HeartBeat := TsgcAMQPFramePayloadType_HeartBeat.Create;
    FPayload_HeartBeat.OnReadFramePayload := OnReadFramePayloadEvent;
  end;
  result := FPayload_HeartBeat;
end;

function TsgcAMQPFrame.GetPayload_Method: TsgcAMQPFramePayloadType_Method;
begin
  if not Assigned(FPayload_Method) then
  begin
    FPayload_Method := TsgcAMQPFramePayloadType_Method.Create;
    FPayload_Method.OnReadFramePayload := OnReadFramePayloadEvent;
  end;
  result := FPayload_Method;
end;

procedure TsgcAMQPFrame.OnReadFramePayloadEvent(Sender: TObject);
begin
  if Assigned(FOnReadFrame) then
    FOnReadFrame(self);
end;

function TsgcAMQPFrame.Read(const aBytes: TBytes; aOffset: Integer = 0)
  : Integer;
begin
  result := 0;

  Header.Read(aBytes, aOffset);
  if Header.Size <= Length(aBytes) - aOffset - CS_AMQP_FRAME_HEADER_LENGTH - CS_AMQP_FRAME_END_LENGTH
  then
  begin
    Payload.Read(aBytes, Header.Size, aOffset + CS_AMQP_FRAME_HEADER_LENGTH);

    DoValidateFrame(aBytes, aOffset);

    result := Header.Size + CS_AMQP_FRAME_HEADER_LENGTH +
      CS_AMQP_FRAME_END_LENGTH;
  end;
end;

function TsgcAMQPFrame.Write: TBytes;
begin
  result := DoWrite;
end;

destructor TsgcAMQPChannelThread.Destroy;
begin
  sgcFree(FFrames);
  inherited;
end;

procedure TsgcAMQPChannelThread.AddFrame(const aFrame: TsgcAMQPFrame);
begin
  Frames.Add(aFrame);
end;

procedure TsgcAMQPChannelThread.Execute;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  inherited;
  while not Terminated do
  begin
    oList := Frames.LockList;
    Try
      if oList.Count > 0 then
      begin
        Try
          Try
            TsgcAMQPFrame(oList[0]).DoReadPayload;
            if Assigned(FOnAfterReadFrame) then
              FOnAfterReadFrame(oList[0]);
            {$IFDEF NEXTGEN}
            TObject(oList[0]).DisposeOf;
            {$ELSE}
            TObject(oList[0]).Free;
            {$ENDIF}
          Except
            On E: Exception do
            begin
              if Assigned(FOnException) then
                FOnException(self, E);
            end;
          End;
        Finally
          oList.Delete(0);
        end;
      end;
    Finally
      Frames.UnlockList;
    End;

    sleep(1);
  end;
end;

function TsgcAMQPChannelThread.GetFrames: TsgcQueue;
begin
  if not Assigned(FFrames) then
  begin
    FFrames := TsgcQueue.Create;
    FFrames.OwnObjects := True;
  end;
  result := FFrames;
end;

constructor TsgcAMQPChannelThreads.Create;
begin
  inherited;
  OwnObjects := True;
end;

destructor TsgcAMQPChannelThreads.Destroy;
begin
  sgcFree(FId);
  inherited;
end;

function TsgcAMQPChannelThreads.AddChannel(const aChannel: string): Word;
var
  oItem: TsgcAMQPChannelThreadItem;
begin
  result := 0;

  oItem := GetChannel(aChannel);

  if not Assigned(oItem) then
  begin
    result := GetNewChannelId;

    oItem := TsgcAMQPChannelThreadItem.Create;
    oItem.OnTerminate := OnThreadTerminateEvent;
    oItem.OnException := OnThreadExceptionEvent;
    oItem.ID := IntToStr(result);
    oItem.Channel := aChannel;
    Add(oItem);
  end;
end;

procedure TsgcAMQPChannelThreads.AddFrame(const aFrame: TsgcAMQPFrame);
var
  oItem: TsgcAMQPChannelThreadItem;
begin
  oItem := TsgcAMQPChannelThreadItem(GetItem(IntToStr(aFrame.Header.Channel)));

  if not Assigned(oItem) then
  begin
    oItem := TsgcAMQPChannelThreadItem.Create;
    oItem.OnTerminate := OnThreadTerminateEvent;
    oItem.OnException := OnThreadExceptionEvent;
    oItem.ID := IntToStr(aFrame.Header.Channel);
    Add(oItem);
  end;

  oItem.AddFrame(aFrame);
end;

function TsgcAMQPChannelThreads.GetChannel(const aChannel: string)
  : TsgcAMQPChannelThreadItem;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcAMQPChannelThreadItem(oList[i]).Channel = aChannel then
      begin
        result := TsgcAMQPChannelThreadItem(oList[i]);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcAMQPChannelThreads.GetChannel(const aId: Word)
  : TsgcAMQPChannelThreadItem;
begin
  result := TsgcAMQPChannelThreadItem(GetItem(IntToStr(aId)));
end;

function TsgcAMQPChannelThreads.GetNewChannelId: Word;
begin
  if not Assigned(FId) then
    FId := TsgcThreadSafeInteger.Create;
  FId.Inc;
  result := FId.Value;
end;

procedure TsgcAMQPChannelThreads.OnThreadExceptionEvent(Sender: TObject;
  E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Sender, E);
end;

procedure TsgcAMQPChannelThreads.OnThreadTerminateEvent(Sender: TObject);
var
  oItem: TsgcAMQPChannelThreadItem;
begin
  oItem := TsgcAMQPChannelThreadItem(Sender);
  if Assigned(oItem) then
    DeleteItem(oItem.ID)
end;

constructor TsgcAMQPChannelThreadItem.Create;
begin
  inherited;
  FFlow := True;
  FThread := TsgcAMQPChannelThread.Create(False);
  FThread.OnAfterReadFrame := OnThreadAfterReadFrameEvent;
  FThread.OnException := OnThreadExceptionEvent;
  FThread.FreeOnTerminate := True;
  FMethodId := amqpChannOpen;
  WaitRequest.Methods := [amqpChannOpenOk];
end;

destructor TsgcAMQPChannelThreadItem.Destroy;
begin
  if Assigned(FThread) then
    FThread.Terminate; // freed automatically when terminate
  sgcFree(FChannelClose);
  sgcFree(FQoS);
  sgcFree(FMessageContent);
  sgcFree(FWaitRequest);
  sgcFree(FRequest);
  inherited;
end;

procedure TsgcAMQPChannelThreadItem.AddFrame(const aFrame: TsgcAMQPFrame);
begin
  FThread.AddFrame(aFrame);
end;

function TsgcAMQPChannelThreadItem.GetChannelClose
  : TsgcAMQPFramePayload_Method_ChannelClose;
begin
  if not Assigned(FChannelClose) then
    FChannelClose := TsgcAMQPFramePayload_Method_ChannelClose.Create;
  result := FChannelClose;
end;

function TsgcAMQPChannelThreadItem.GetMessageContent: TsgcAMQPMessageContent;
begin
  if not Assigned(FMessageContent) then
    FMessageContent := TsgcAMQPMessageContent.Create;
  result := FMessageContent;
end;

function TsgcAMQPChannelThreadItem.GetQoS: TsgcAMQPFramePayload_Method_BasicQoS;
begin
  if not Assigned(FQoS) then
    FQoS := TsgcAMQPFramePayload_Method_BasicQoS.Create;
  result := FQoS;
end;

function TsgcAMQPChannelThreadItem.GetRequest: TsgcAMQPChannelThreadRequest;
begin
  if not Assigned(FRequest) then
    FRequest := TsgcAMQPChannelThreadRequest.Create;
  result := FRequest;
end;

function TsgcAMQPChannelThreadItem.GetWaitRequest
  : TsgcAMQPChannelThreadWaitRequest;
begin
  if not Assigned(FWaitRequest) then
    FWaitRequest := TsgcAMQPChannelThreadWaitRequest.Create;
  result := FWaitRequest;
end;

procedure TsgcAMQPChannelThreadItem.OnThreadAfterReadFrameEvent
  (Sender: TObject);
begin
  // prevent create WaitRequest after CloseChannel
  if not Assigned(FWaitRequest) then
    exit;

  if not WaitRequest.Terminated then
  begin
    if Assigned(Sender) then
    begin
      if Assigned(TsgcAMQPFrame(Sender).Payload_Method) then
      begin
        if TsgcAMQPFrame(Sender).Payload_Method.MethodId in WaitRequest.Methods
        then
        begin
          WaitRequest.Terminated := True;
          WaitRequest.Method := TsgcAMQPFrame(Sender).Payload_Method.MethodId;
        end;
      end;
    end;
  end;
end;

procedure TsgcAMQPChannelThreadItem.OnThreadExceptionEvent(Sender: TObject;
  E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(self, E);
end;

procedure TsgcAMQPChannelThreadItem.SetChannelClose
  (const Value: TsgcAMQPFramePayload_Method_ChannelClose);
begin
  if Assigned(Value) then
  begin
    ChannelClose.ReplyCode := Value.ReplyCode;
    ChannelClose.ReplyText := Value.ReplyText;
    ChannelClose.FailClassId := Value.FailClassId;
    ChannelClose.FailMethodId := Value.FailMethodId;
  end;
end;

procedure TsgcAMQPChannelThreadItem.SetQoS(const Value
  : TsgcAMQPFramePayload_Method_BasicQoS);
begin
  if Assigned(Value) then
  begin
    QoS.PrefetchSize := Value.PrefetchSize;
    QoS.PrefetchCount := Value.PrefetchCount;
    QoS.Global := Value.Global;
  end;
end;

procedure TsgcAMQPChannelThreadItem.Terminate;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;

    if Assigned(FOnTerminate) then
      FOnTerminate(self);
  end;
end;

destructor TsgcAMQPFramePayloadType_Method.Destroy;
begin
  sgcFree(FPayload);
  inherited;
end;

procedure TsgcAMQPFramePayloadType_Method.DoRead(const aBytes: TBytes);
var
  oPayload: TsgcAMQPFramePayload_Method;
begin
  inherited;
  oPayload := nil;

  FClassId := sgcGetAMQPClass(sgcGetUInt16FromBytes(aBytes));
  case FClassId of
    amqpClassConnection:
      begin
        FMethodId := sgcGetAMQPConnection(sgcGetUInt16FromBytes(aBytes, 2));
        case MethodId of
          amqpConnStart:
            oPayload := TsgcAMQPFramePayload_Method_ConnectionStart.Create;
          amqpConnStartOk:
            ;
          amqpConnSecure:
            oPayload := TsgcAMQPFramePayload_Method_ConnectionSecure.Create;
          amqpConnSecureOk:
            ;
          amqpConnTune:
            oPayload := TsgcAMQPFramePayload_Method_ConnectionTune.Create;
          amqpConnTuneOk:
            ;
          amqpConnOpen:
            ;
          amqpConnOpenOk:
            oPayload := TsgcAMQPFramePayload_Method_ConnectionOpenOk.Create;
          amqpConnClose:
            oPayload := TsgcAMQPFramePayload_Method_ConnectionClose.Create;
          amqpConnCloseOk:
            oPayload := TsgcAMQPFramePayload_Method_ConnectionCloseOk.Create;
        end;
      end;
    amqpClassChannel:
      begin
        FMethodId := sgcGetAMQPChannel(sgcGetUInt16FromBytes(aBytes, 2));
        case MethodId of
          amqpChannNone:
            ;
          amqpChannOpen:
            ;
          amqpChannOpenOk:
            oPayload := TsgcAMQPFramePayload_Method_ChannelOpenOk.Create;
          amqpChannFlow:
            oPayload := TsgcAMQPFramePayload_Method_ChannelFlow.Create;
          amqpChannFlowOk:
            oPayload := TsgcAMQPFramePayload_Method_ChannelFlowOk.Create;
          amqpChannClose:
            oPayload := TsgcAMQPFramePayload_Method_ChannelClose.Create;
          amqpChannCloseOk:
            oPayload := TsgcAMQPFramePayload_Method_ChannelCloseOk.Create;
        end;
      end;
    amqpClassExchange:
      begin
        FMethodId := sgcGetAMQPExchange(sgcGetUInt16FromBytes(aBytes, 2));
        case MethodId of
          amqpExchNone:
            ;
          amqpExchDeclare:
            ;
          amqpExchDeclareOk:
            oPayload := TsgcAMQPFramePayload_Method_ExchangeDeclareOk.Create;
          amqpExchDelete:
            ;
          amqpExchDeleteOk:
            oPayload := TsgcAMQPFramePayload_Method_ExchangeDeleteOk.Create;
        end;
      end;
    amqpClassQueue:
      begin
        FMethodId := sgcGetAMQPQueue(sgcGetUInt16FromBytes(aBytes, 2));
        case MethodId of
          amqpQueueDeclare:
            ;
          amqpQueueDeclareOk:
            oPayload := TsgcAMQPFramePayload_Method_QueueDeclareOk.Create;
          amqpQueueBind:
            ;
          amqpQueueBindOk:
            oPayload := TsgcAMQPFramePayload_Method_QueueBindOk.Create;
          amqpQueueUnBind:
            ;
          amqpQueueUnBindOk:
            oPayload := TsgcAMQPFramePayload_Method_QueueUnBindOk.Create;
          amqpQueuePurge:
            ;
          amqpQueuePurgeOk:
            oPayload := TsgcAMQPFramePayload_Method_QueuePurgeOk.Create;
          amqpQueueDelete:
            ;
          amqpQueueDeleteOk:
            oPayload := TsgcAMQPFramePayload_Method_QueueDeleteOk.Create;
        end;
      end;
    amqpClassBasic:
      begin
        FMethodId := sgcGetAMQPBasic(sgcGetUInt16FromBytes(aBytes, 2));
        case MethodId of
          amqpBasicNone:
            ;
          amqpBasicQos:
            ;
          amqpBasicQosOk:
            oPayload := TsgcAMQPFramePayload_Method_BasicQoSOk.Create;
          amqpBasicConsume:
            ;
          amqpBasicConsumeOk:
            oPayload := TsgcAMQPFramePayload_Method_BasicConsumeOk.Create;
          amqpBasicCancel:
            ;
          amqpBasicCancelOk:
            oPayload := TsgcAMQPFramePayload_Method_BasicCancelOk.Create;
          amqpBasicPublish:
            ;
          amqpBasicReturn:
            oPayload := TsgcAMQPFramePayload_Method_BasicReturn.Create;
          amqpBasicDeliver:
            oPayload := TsgcAMQPFramePayload_Method_BasicDeliver.Create;
          amqpBasicGet:
            ;
          amqpBasicGetOk:
            oPayload := TsgcAMQPFramePayload_Method_BasicGetOk.Create;
          amqpBasicGetEmpty:
            oPayload := TsgcAMQPFramePayload_Method_BasicGetEmpty.Create;
          amqpBasicAck:
            ;
          amqpBasicReject:
            ;
          amqpBasicRecoverAsync:
            ;
          amqpBasicRecover:
            ;
          amqpBasicRecoverOk:
            oPayload := TsgcAMQPFramePayload_Method_BasicRecoverOk.Create;
        end;
      end;
    amqpClassTx:
      begin
        FMethodId := sgcGetAMQPTX(sgcGetUInt16FromBytes(aBytes, 2));
        case MethodId of
          amqpTxSelect:
            ;
          amqpTxSelectOk:
            oPayload := TsgcAMQPFramePayload_Method_TxSelectOk.Create;
          amqpTxCommit:
            ;
          amqpTxCommitOk:
            oPayload := TsgcAMQPFramePayload_Method_TxCommitOk.Create;
          amqpTxRollback:
            ;
          amqpTxRollbackOk:
            oPayload := TsgcAMQPFramePayload_Method_TxRollbackOk.Create;
        end;
      end;
  end;

  Payload := oPayload;
  Payload.Read(aBytes);
end;

function TsgcAMQPFramePayloadType_Method.DoWrite: TBytes;
begin
  sgcWriteAMQPUInt16(sgcGetAMQPClassValue(ClassId), result);
  case ClassId of
    amqpClassConnection:
      sgcWriteAMQPUInt16(sgcGetAMQPConnectionValue(MethodId), result);
    amqpClassChannel:
      sgcWriteAMQPUInt16(sgcGetAMQPChannelValue(MethodId), result);
    amqpClassExchange:
      sgcWriteAMQPUInt16(sgcGetAMQPExchangeValue(MethodId), result);
    amqpClassQueue:
      sgcWriteAMQPUInt16(sgcGetAMQPQueueValue(MethodId), result);
    amqpClassBasic:
      sgcWriteAMQPUInt16(sgcGetAMQPBasicValue(MethodId), result);
    amqpClassTx:
      sgcWriteAMQPUInt16(sgcGetAMQPTxValue(MethodId), result);
  end;

  sgcWriteBytes(Payload.Write, result);
end;

procedure TsgcAMQPFramePayloadType_Method.SetPayload
  (const Value: TsgcAMQPFramePayload_Method);
begin
  FPayload := Value;

  if Assigned(Value) then
  begin
    FClassId := Value.FClassId;
    FMethodId := Value.FMethodId;
  end;
end;

constructor TsgcAMQPFramePayload_Method_ConnectionStart.Create;
begin
  inherited;
  FMethodId := amqpConnStart;
end;

destructor TsgcAMQPFramePayload_Method_ConnectionStart.Destroy;
begin
  sgcFree(FServerProperties);
  inherited;
end;

function TsgcAMQPFramePayload_Method_ConnectionStart.GetServerProperties
  : TStringList;
begin
  if not Assigned(FServerProperties) then
    FServerProperties := TStringList.Create;
  result := FServerProperties;
end;

procedure TsgcAMQPFramePayload_Method_ConnectionStart.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  ServerProperties.Clear;

  VersionMajor := sgcReadAMQPByte(aBytes, FOffset);
  VersionMinor := sgcReadAMQPByte(aBytes, FOffset);
  sgcReadAMQPStringList(aBytes, FOffset, FServerProperties);
  Mechanisms := sgcReadAMQPLongString(aBytes, FOffset);
  Locales := sgcReadAMQPLongString(aBytes, FOffset);
end;

procedure TsgcAMQPFramePayload_Method.DoRead(const aBytes: TBytes);
begin
  inherited;
  FOffset := 4;
end;

constructor TsgcAMQPFramePayload_Base.Create;
begin
  inherited;
end;

procedure TsgcAMQPFramePayload_Base.DoRead(const aBytes: TBytes);
begin

end;

function TsgcAMQPFramePayload_Base.DoWrite: TBytes;
begin

end;

procedure TsgcAMQPFramePayload_Base.Read(const aBytes: TBytes);
begin
  DoRead(aBytes);
end;

function TsgcAMQPFramePayload_Base.Write: TBytes;
begin
  result := DoWrite;
end;

constructor TsgcAMQPFramePayloadType.Create;
begin
  inherited;
end;

procedure TsgcAMQPFramePayloadType.DoRead(const aBytes: TBytes);
begin

end;

function TsgcAMQPFramePayloadType.DoWrite: TBytes;
begin
  result := nil;
end;

procedure TsgcAMQPFramePayloadType.Read(const aBytes: TBytes);
begin
  DoRead(aBytes);

  if Assigned(FOnReadFramePayload) then
    FOnReadFramePayload(self);
end;

function TsgcAMQPFramePayloadType.Write: TBytes;
begin
  result := DoWrite;
end;

constructor TsgcAMQPFramePayload_Method_ConnectionStartOk.Create;
begin
  inherited;
  FMethodId := amqpConnStartOk;
  FLocale := CS_AMQP_DEFAULT_LOCALE;
end;

destructor TsgcAMQPFramePayload_Method_ConnectionStartOk.Destroy;
begin
  sgcFree(FClientProperties);
  inherited;
end;

function TsgcAMQPFramePayload_Method_ConnectionStartOk.DoWrite: TBytes;
var
  oList: TStringList;
begin
  result := inherited DoWrite;

  sgcWriteAMQPStringList(ClientProperties, result);
  sgcWriteAMQPShortString(Mechanism, result);
  case MechanismType of
    amqpAuthPlain:
      sgcWriteAMQPLongString(#0 + User + #0 + Password, result);
    amqpAuthAMQPlain:
      begin
        oList := TStringList.Create;
        Try
          oList.Add('LOGIN=' + User);
          oList.Add('PASSWORD=' + Password);
          sgcWriteAMQPStringList(oList, result);
        Finally
          sgcFree(oList);
        End;
      end;
  else
    sgcWriteAMQPLongString('', result);
  end;
  sgcWriteAMQPShortString(Locale, result);
end;

function TsgcAMQPFramePayload_Method_ConnectionStartOk.GetClientProperties
  : TStringList;
var
  vPlatform: string;
begin
  if not Assigned(FClientProperties) then
  begin
    FClientProperties := TStringList.Create;
    FClientProperties.Add('product=' + CS_APPLICATION_NAME);
    FClientProperties.Add('version=' + CS_VERSION);
{$IFDEF LINUX64}
    vPlatform := 'linux';
{$ENDIF}
{$IFDEF MACOS}
    vPlatform := 'macos';
{$ENDIF}
{$IFDEF ANDROID}
    vPlatform := 'android';
{$ENDIF}
{$IFDEF IOS}
    vPlatform := 'ios';
{$ENDIF}
    if vPlatform = '' then
      vPlatform := 'windows';
    FClientProperties.Add('platform=' + vPlatform);
    FClientProperties.Add('copyright=Copyright (c) esegece.com');
    FClientProperties.Add('information=Built with ' + CS_APPLICATION_NAME + ' '
      + CS_VERSION + '.');
  end;
  result := FClientProperties;
end;

function TsgcAMQPFramePayload_Method_ConnectionStartOk.GetMechanismType
  : TsgcAMQPAuthentication;
begin
  result := sgcGetAMQPAuthentication(Mechanism);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionTune.Create;
begin
  inherited;
  FMethodId := amqpConnTune;
end;

procedure TsgcAMQPFramePayload_Method_ConnectionTune.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  ChannelMax := sgcReadAMQPUInt16(aBytes, FOffset);
  FrameMax := sgcReadAMQPUInt32(aBytes, FOffset);
  HeartBeat := sgcReadAMQPUInt16(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionSecure.Create;
begin
  inherited;
  FMethodId := amqpConnSecure;
end;

procedure TsgcAMQPFramePayload_Method_ConnectionSecure.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  Challenge := sgcReadAMQPLongString(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionSecureOk.Create;
begin
  inherited;
  FMethodId := amqpConnSecureOk;
end;

function TsgcAMQPFramePayload_Method_ConnectionSecureOk.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPLongString(Challenge, result);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionTuneOk.Create;
begin
  inherited;
  FMethodId := amqpConnTuneOk;
end;

function TsgcAMQPFramePayload_Method_ConnectionTuneOk.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(ChannelMax, result);
  sgcWriteAMQPUInt32(FrameMax, result);
  sgcWriteAMQPUInt16(HeartBeat, result);
end;

constructor TsgcAMQPFramePayload_Method_Connection.Create;
begin
  inherited;
  FClassId := amqpClassConnection;
end;

constructor TsgcAMQPFramePayload_Method_ConnectionOpen.Create;
begin
  inherited;
  FMethodId := amqpConnOpen;
  FVirtualHost := CS_AMQP_DEFAULT_VIRTUAL_HOST;
end;

function TsgcAMQPFramePayload_Method_ConnectionOpen.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPShortString(VirtualHost, result);
  sgcWriteAMQPShortString(Reserved1, result);
  sgcWriteAMQPBoolean(Reserved2, result);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionOpenOk.Create;
begin
  inherited;
  FMethodId := amqpConnOpenOk;
end;

procedure TsgcAMQPFramePayload_Method_ConnectionOpenOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  Reserved1 := sgcReadAMQPShortString(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionClose.Create;
begin
  inherited;
  FMethodId := amqpConnClose;
end;

procedure TsgcAMQPFramePayload_Method_ConnectionClose.DoRead
  (const aBytes: TBytes);
begin
  inherited;

  ReplyCode := sgcReadAMQPUInt16(aBytes, FOffset);
  ReplyText := sgcReadAMQPShortString(aBytes, FOffset);
  FailClassId := sgcGetAMQPClass(sgcReadAMQPUInt16(aBytes, FOffset));
  FailMethodId := sgcGetAMQPMethod(FailClassId, sgcReadAMQPUInt16(aBytes,
    FOffset));
end;

function TsgcAMQPFramePayload_Method_ConnectionClose.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(ReplyCode, result);
  sgcWriteAMQPShortString(ReplyText, result);
  sgcWriteAMQPUInt16(sgcGetAMQPClassValue(FailClassId), result);
  sgcWriteAMQPUInt16(sgcGetAMQPConnectionValue(FailMethodId), result);
end;

constructor TsgcAMQPFramePayload_Method_ConnectionCloseOk.Create;
begin
  inherited;
  FMethodId := amqpConnCloseOk;
end;

procedure TsgcAMQPFramePayload_Method_ConnectionCloseOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
end;

function TsgcAMQPFramePayload_Method_ConnectionCloseOk.DoWrite: TBytes;
begin
  result := inherited DoWrite;
end;

constructor TsgcAMQPFramePayload_Method_ChannelOpen.Create;
begin
  inherited;
  FMethodId := amqpChannOpen;
end;

function TsgcAMQPFramePayload_Method_ChannelOpen.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPShortString(Reserved1, result);
end;

constructor TsgcAMQPFramePayload_Method_ChannelOpenOk.Create;
begin
  inherited;
  FMethodId := amqpChannOpenOk;
end;

procedure TsgcAMQPFramePayload_Method_ChannelOpenOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  Reserved1 := sgcReadAMQPShortString(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_Channel.Create;
begin
  inherited;
  FClassId := amqpClassChannel;
end;

constructor TsgcAMQPFramePayload_Method_ChannelClose.Create;
begin
  inherited;
  FMethodId := amqpChannClose;
end;

procedure TsgcAMQPFramePayload_Method_ChannelClose.DoRead(const aBytes: TBytes);
begin
  inherited;

  ReplyCode := sgcReadAMQPUInt16(aBytes, FOffset);
  ReplyText := sgcReadAMQPShortString(aBytes, FOffset);
  FailClassId := sgcGetAMQPClass(sgcReadAMQPUInt16(aBytes, FOffset));
  FailMethodId := sgcGetAMQPMethod(FailClassId, sgcReadAMQPUInt16(aBytes,
    FOffset));
end;

function TsgcAMQPFramePayload_Method_ChannelClose.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(ReplyCode, result);
  sgcWriteAMQPShortString(ReplyText, result);
  sgcWriteAMQPUInt16(sgcGetAMQPClassValue(FailClassId), result);
  sgcWriteAMQPUInt16(sgcGetAMQPChannelValue(FailMethodId), result);
end;

constructor TsgcAMQPFramePayload_Method_ChannelCloseOk.Create;
begin
  inherited;
  FMethodId := amqpChannCloseOk;
end;

procedure TsgcAMQPFramePayload_Method_ChannelCloseOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
end;

function TsgcAMQPFramePayload_Method_ChannelCloseOk.DoWrite: TBytes;
begin
  result := inherited DoWrite;
end;

constructor TsgcAMQPFramePayload_Method_ChannelFlow.Create;
begin
  inherited;
  FMethodId := amqpChannFlow;
end;

procedure TsgcAMQPFramePayload_Method_ChannelFlow.DoRead(const aBytes: TBytes);
begin
  inherited;
  Active := sgcReadAMQPBoolean(aBytes, FOffset);
end;

function TsgcAMQPFramePayload_Method_ChannelFlow.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPBoolean(Active, result);
end;

constructor TsgcAMQPFramePayload_Method_ChannelFlowOk.Create;
begin
  inherited;
  FMethodId := amqpChannFlowOk;
end;

procedure TsgcAMQPFramePayload_Method_ChannelFlowOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  Active := sgcReadAMQPBoolean(aBytes, FOffset);
end;

function TsgcAMQPFramePayload_Method_ChannelFlowOk.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPBoolean(Active, result);
end;

constructor TsgcAMQPFramePayload_Method_Exchange.Create;
begin
  inherited;
  FClassId := amqpClassExchange;
end;

constructor TsgcAMQPFramePayload_Method_ExchangeDeclare.Create;
begin
  inherited;
  FMethodId := amqpExchDeclare;
end;

function TsgcAMQPFramePayload_Method_ExchangeDeclare.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Exchange, result);
  sgcWriteAMQPShortString(ExchangeType, result);
  i := Length(result);
  SetLength(result, i + 1);
  if Passive then
    sgcWriteAMQPBit(i, 0, result);
  if Durable then
    sgcWriteAMQPBit(i, 1, result);
  if AutoDelete then
    sgcWriteAMQPBit(i, 2, result);
  if Internal then
    sgcWriteAMQPBit(i, 3, result);
  if NoWait then
    sgcWriteAMQPBit(i, 4, result);
  sgcWriteAMQPFieldTable(Arguments, result);
end;

constructor TsgcAMQPFramePayload_Method_ExchangeDeclareOk.Create;
begin
  inherited;
  FMethodId := amqpExchDeclareOk;
end;

procedure TsgcAMQPFramePayload_Method_ExchangeDeclareOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_ExchangeDelete.Create;
begin
  inherited;
  FMethodId := amqpExchDelete;
end;

function TsgcAMQPFramePayload_Method_ExchangeDelete.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Exchange, result);
  i := Length(result);
  SetLength(result, i + 1);
  if IfUnused then
    sgcWriteAMQPBit(i, 0, result);
  if NoWait then
    sgcWriteAMQPBit(i, 1, result);
end;

constructor TsgcAMQPFramePayload_Method_ExchangeDeleteOk.Create;
begin
  inherited;
  FMethodId := amqpExchDeleteOk;
end;

procedure TsgcAMQPFramePayload_Method_ExchangeDeleteOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_Queue.Create;
begin
  inherited;
  FClassId := amqpClassQueue;
end;

constructor TsgcAMQPFramePayload_Method_QueueDeclare.Create;
begin
  inherited;
  FMethodId := amqpQueueDeclare;
end;

function TsgcAMQPFramePayload_Method_QueueDeclare.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  i := Length(result);
  SetLength(result, i + 1);
  if Passive then
    sgcWriteAMQPBit(i, 0, result);
  if Durable then
    sgcWriteAMQPBit(i, 1, result);
  if Exclusive then
    sgcWriteAMQPBit(i, 2, result);
  if AutoDelete then
    sgcWriteAMQPBit(i, 3, result);
  if NoWait then
    sgcWriteAMQPBit(i, 4, result);
  sgcWriteAMQPFieldTable(Arguments, result);
end;

constructor TsgcAMQPFramePayload_Method_QueueDeclareOk.Create;
begin
  inherited;
  FMethodId := amqpQueueDeclareOk;
end;

procedure TsgcAMQPFramePayload_Method_QueueDeclareOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;

  FQueue := sgcReadAMQPShortString(aBytes, FOffset);
  FMessageCount := sgcReadAMQPInt32(aBytes, FOffset);
  FConsumerCount := sgcReadAMQPInt32(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_QueueBind.Create;
begin
  inherited;
  FMethodId := amqpQueueBind;
end;

function TsgcAMQPFramePayload_Method_QueueBind.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  sgcWriteAMQPShortString(Exchange, result);
  sgcWriteAMQPShortString(RoutingKey, result);
  i := Length(result);
  SetLength(result, i + 1);
  if NoWait then
    sgcWriteAMQPBit(i, 0, result);
  sgcWriteAMQPFieldTable(Arguments, result);
end;

constructor TsgcAMQPFramePayload_Method_QueueBindOk.Create;
begin
  inherited;
  FMethodId := amqpQueueBindOk;
end;

procedure TsgcAMQPFramePayload_Method_QueueBindOk.DoRead(const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_QueueUnBind.Create;
begin
  inherited;
  FMethodId := amqpQueueUnBind;
end;

function TsgcAMQPFramePayload_Method_QueueUnBind.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  sgcWriteAMQPShortString(Exchange, result);
  sgcWriteAMQPShortString(RoutingKey, result);
  sgcWriteAMQPFieldTable(Arguments, result);
end;

constructor TsgcAMQPFramePayload_Method_QueueUnBindOk.Create;
begin
  inherited;
  FMethodId := amqpQueueUnBindOk;
end;

procedure TsgcAMQPFramePayload_Method_QueueUnBindOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_QueuePurge.Create;
begin
  inherited;
  FMethodId := amqpQueuePurge;
end;

function TsgcAMQPFramePayload_Method_QueuePurge.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  i := Length(result);
  SetLength(result, i + 1);
  if NoWait then
    sgcWriteAMQPBit(i, 0, result);
end;

constructor TsgcAMQPFramePayload_Method_QueuePurgeOk.Create;
begin
  inherited;
  FMethodId := amqpQueuePurgeOk;
end;

procedure TsgcAMQPFramePayload_Method_QueuePurgeOk.DoRead(const aBytes: TBytes);
begin
  inherited;
  FMessageCount := sgcReadAMQPInt32(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_QueueDelete.Create;
begin
  inherited;
  FMethodId := amqpQueueDelete;
end;

function TsgcAMQPFramePayload_Method_QueueDelete.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  i := Length(result);
  SetLength(result, i + 1);
  if IfUnused then
    sgcWriteAMQPBit(i, 0, result);
  if IfEmpty then
    sgcWriteAMQPBit(i, 1, result);
  if NoWait then
    sgcWriteAMQPBit(i, 2, result);
end;

constructor TsgcAMQPFramePayload_Method_QueueDeleteOk.Create;
begin
  inherited;
  FMethodId := amqpQueueDeleteOk;
end;

procedure TsgcAMQPFramePayload_Method_QueueDeleteOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  FMessageCount := sgcReadAMQPInt32(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_Basic.Create;
begin
  inherited;
  FClassId := amqpClassBasic;
end;

constructor TsgcAMQPFramePayload_Method_BasicQoS.Create;
begin
  inherited;
  FMethodId := amqpBasicQos;
end;

function TsgcAMQPFramePayload_Method_BasicQoS.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt32(PrefetchSize, result);
  sgcWriteAMQPUInt16(PrefetchCount, result);
  i := Length(result);
  SetLength(result, i + 1);
  if Global then
    sgcWriteAMQPBit(i, 0, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicQoSOk.Create;
begin
  inherited;
  FMethodId := amqpBasicQosOk;
end;

procedure TsgcAMQPFramePayload_Method_BasicQoSOk.DoRead(const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_BasicConsume.Create;
begin
  inherited;
  FMethodId := amqpBasicConsume;
end;

function TsgcAMQPFramePayload_Method_BasicConsume.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  sgcWriteAMQPShortString(ConsumerTag, result);
  i := Length(result);
  SetLength(result, i + 1);
  if NoLocal then
    sgcWriteAMQPBit(i, 0, result);
  if NoAck then
    sgcWriteAMQPBit(i, 1, result);
  if Exclusive then
    sgcWriteAMQPBit(i, 2, result);
  if NoWait then
    sgcWriteAMQPBit(i, 3, result);
  sgcWriteAMQPFieldTable(Arguments, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicConsumeOk.Create;
begin
  inherited;
  FMethodId := amqpBasicConsumeOk;
end;

procedure TsgcAMQPFramePayload_Method_BasicConsumeOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  FConsumerTag := sgcReadAMQPShortString(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_BasicCancel.Create;
begin
  inherited;
  FMethodId := amqpBasicCancel;
end;

function TsgcAMQPFramePayload_Method_BasicCancel.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPShortString(ConsumerTag, result);
  i := Length(result);
  SetLength(result, i + 1);
  if NoWait then
    sgcWriteAMQPBit(i, 0, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicCancelOk.Create;
begin
  inherited;
  FMethodId := amqpBasicCancelOk;
end;

procedure TsgcAMQPFramePayload_Method_BasicCancelOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  FConsumerTag := sgcReadAMQPShortString(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_BasicPublish.Create;
begin
  inherited;
  FMethodId := amqpBasicPublish;
end;

function TsgcAMQPFramePayload_Method_BasicPublish.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Exchange, result);
  sgcWriteAMQPShortString(RoutingKey, result);
  i := Length(result);
  SetLength(result, i + 1);
  if Mandatory then
    sgcWriteAMQPBit(i, 0, result);
  if Immediate then
    sgcWriteAMQPBit(i, 1, result);
end;

destructor TsgcAMQPFramePayloadType_ContentHeader.Destroy;
begin
  sgcFree(FHeader);
  inherited;
end;

procedure TsgcAMQPFramePayloadType_ContentHeader.DoRead(const aBytes: TBytes);
begin
  inherited;
  FOffset := 0;
  FClassId := sgcGetAMQPClass(sgcReadAMQPUInt16(aBytes, FOffset));
  if FClassId <> amqpClassBasic then
    DoRaiseAMQPException(CS_AMQP_ERROR_FRAME);
  FWeight := sgcReadAMQPUInt16(aBytes, FOffset);
  if FWeight <> 0 then
    DoRaiseAMQPException(CS_AMQP_ERROR_FRAME);
  FBodySize := sgcReadAMQPUInt64(aBytes, FOffset);
  Header.Read(aBytes, FOffset);
end;

function TsgcAMQPFramePayloadType_ContentHeader.DoWrite: TBytes;
begin
  sgcWriteAMQPUInt16(sgcGetAMQPClassValue(ClassId), result);
  sgcWriteAMQPUInt16(Weight, result);
  sgcWriteAMQPUInt64(BodySize, result);
  sgcWriteBytes(Header.Write, result);
end;

function TsgcAMQPFramePayloadType_ContentHeader.GetHeader
  : TsgcAMQPBasicProperties;
begin
  if not Assigned(FHeader) then
    FHeader := TsgcAMQPBasicProperties.Create;
  result := FHeader;
end;

procedure TsgcAMQPFramePayloadType_ContentHeader.SetHeader
  (const Value: TsgcAMQPBasicProperties);
begin
  if Assigned(Value) then
  begin
    Header.AppId := Value.AppId;
    Header.ContentEncoding := Value.ContentEncoding;
    Header.ContentType := Value.ContentType;
    Header.CorrelationId := Value.CorrelationId;
    Header.DeliveryMode := Value.DeliveryMode;
    Header.Expiration := Value.Expiration;
    Header.Headers := Value.Headers;
    Header.MessageId := Value.MessageId;
    Header.Priority := Value.Priority;
    Header.ReplyTo := Value.ReplyTo;
    Header.Reserved := Value.Reserved;
    Header.TimeStamp := Value.TimeStamp;
    Header.UserId := Value.UserId;
    Header._Type := Value._Type;
  end;
end;

constructor TsgcAMQPBasicProperties.Create;
begin
  inherited;
  DoInitialize;
end;

procedure TsgcAMQPBasicProperties.DoInitialize;
begin
  AppId := '';
  ContentEncoding := 'utf-8';
  ContentType := 'text/plain';
  CorrelationId := '';
  DeliveryMode := amqpMDMPersistent;
  Expiration := '';
  Headers := '';
  MessageId := '';
  Priority := 1;
  ReplyTo := '';
  Reserved := '';
  TimeStamp := 0;
  UserId := '';
  _Type := '';
end;

procedure TsgcAMQPBasicProperties.Read(const aBytes: TBytes;
  aOffset: Integer = 0);
var
  vPropertyFlags: UInt16;
begin
  FOffset := aOffset;

  vPropertyFlags := sgcReadAMQPUInt16(aBytes, FOffset);

  if (vPropertyFlags and $8000) = $8000 then
    ContentType := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $4000) = $4000 then
    ContentEncoding := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $2000) = $2000 then
    Headers := sgcReadAMQPFieldTable(aBytes, FOffset);
  if (vPropertyFlags and $1000) = $1000 then
    DeliveryMode := TsgcAMQPMessageDeliveryMode(sgcReadAMQPUInt8(aBytes,
      FOffset));
  if (vPropertyFlags and $800) = $800 then
    Priority := sgcReadAMQPUInt8(aBytes, FOffset);
  if (vPropertyFlags and $400) = $400 then
    CorrelationId := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $200) = $200 then
    ReplyTo := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $100) = $100 then
    Expiration := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $80) = $80 then
    MessageId := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $40) = $40 then
    TimeStamp := sgcReadAMQPUInt64(aBytes, FOffset);
  if (vPropertyFlags and $20) = $20 then
    _Type := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $10) = $10 then
    UserId := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $8) = $8 then
    AppId := sgcReadAMQPShortString(aBytes, FOffset);
  if (vPropertyFlags and $4) = $4 then
    Reserved := sgcReadAMQPShortString(aBytes, FOffset);
end;

function TsgcAMQPBasicProperties.Write: TBytes;
var
  vPropertyFlags: UInt16;
  vPropertyList: TBytes;

  procedure DoPropertyFlags(aBit: Byte);
  begin
    vPropertyFlags := vPropertyFlags + 1 shl aBit;
  end;

begin
  vPropertyFlags := 0;
  SetLength(vPropertyList, 0);

  if ContentType <> '' then
  begin
    sgcWriteAMQPShortString(ContentType, vPropertyList);
    DoPropertyFlags(15);
  end;
  if ContentEncoding <> '' then
  begin
    sgcWriteAMQPShortString(ContentEncoding, vPropertyList);
    DoPropertyFlags(14);
  end;
  if Headers <> '' then
  begin
    sgcWriteAMQPFieldTable(Headers, vPropertyList);
    DoPropertyFlags(13);
  end;
  if DeliveryMode <> amqpMDMNone then
  begin
    sgcWriteAMQPUInt8(Ord(DeliveryMode), vPropertyList);
    DoPropertyFlags(12);
  end;
  if Priority <> 0 then
  begin
    sgcWriteAMQPUInt8(Priority, vPropertyList);
    DoPropertyFlags(11);
  end;
  if CorrelationId <> '' then
  begin
    sgcWriteAMQPShortString(CorrelationId, vPropertyList);
    DoPropertyFlags(10);
  end;
  if ReplyTo <> '' then
  begin
    sgcWriteAMQPShortString(ReplyTo, vPropertyList);
    DoPropertyFlags(9);
  end;
  if Expiration <> '' then
  begin
    sgcWriteAMQPShortString(Expiration, vPropertyList);
    DoPropertyFlags(8);
  end;
  if MessageId <> '' then
  begin
    sgcWriteAMQPShortString(MessageId, vPropertyList);
    DoPropertyFlags(7);
  end;
  if TimeStamp <> 0 then
  begin
    sgcWriteAMQPUInt64(TimeStamp, vPropertyList);
    DoPropertyFlags(6);
  end;
  if _Type <> '' then
  begin
    sgcWriteAMQPShortString(_Type, vPropertyList);
    DoPropertyFlags(5);
  end;
  if UserId <> '' then
  begin
    sgcWriteAMQPShortString(UserId, vPropertyList);
    DoPropertyFlags(4);
  end;
  if AppId <> '' then
  begin
    sgcWriteAMQPShortString(AppId, vPropertyList);
    DoPropertyFlags(3);
  end;
  if Reserved <> '' then
  begin
    sgcWriteAMQPShortString(Reserved, vPropertyList);
    DoPropertyFlags(2);
  end;

  sgcWriteAMQPUInt16(vPropertyFlags, result);
  sgcWriteBytes(vPropertyList, result);
end;

destructor TsgcAMQPFramePayloadType_ContentBody.Destroy;
begin
  if FFreeData then
    sgcFree(FData);
  inherited;
end;

procedure TsgcAMQPFramePayloadType_ContentBody.DoRead(const aBytes: TBytes);
begin
  inherited;
  Data.Write(aBytes[0], Length(aBytes));
  Data.Position := 0;
  DataSize := FData.Size;
end;

function TsgcAMQPFramePayloadType_ContentBody.DoWrite: TBytes;
begin
  if Assigned(FData) then
  begin
    if Data.Size > 0 then
    begin
      SetLength(result, DataSize);
      Data.Read(result[0], DataSize);
    end;
  end;
end;

function TsgcAMQPFramePayloadType_ContentBody.GetAsString: string;
var
  oStream: TStringStream;
begin
  oStream := TStringStream.Create(''{$IFDEF D2009}, TEncoding.UTF8{$ENDIF});
  Try
    Data.Position := 0;
    oStream.CopyFrom(Data, Data.Size);
    Data.Position := 0;
    result := oStream.DataString;
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcAMQPFramePayloadType_ContentBody.GetData: TStream;
begin
  if not Assigned(FData) then
  begin
    FData := TMemoryStream.Create;
    FFreeData := True;
  end;
  result := FData;
end;

constructor TsgcAMQPFramePayload_Method_BasicReturn.Create;
begin
  inherited;
  FMethodId := amqpBasicReturn;
end;

procedure TsgcAMQPFramePayload_Method_BasicReturn.DoRead(const aBytes: TBytes);
begin
  inherited;
  ReplyCode := sgcReadAMQPUInt16(aBytes, FOffset);
  ReplyText := sgcReadAMQPShortString(aBytes, FOffset);
  Exchange := sgcReadAMQPShortString(aBytes, FOffset);
  RoutingKey := sgcReadAMQPShortString(aBytes, FOffset);
end;

destructor TsgcAMQPMessageContent.Destroy;
begin
  sgcFree(FHeader);
  sgcFree(FBody);
  sgcFree(FPayload);
  inherited;
end;

procedure TsgcAMQPMessageContent.Clear;
begin
  DoClear;
end;

procedure TsgcAMQPMessageContent.DoClear;
begin
  sgcFree(FHeader);
  sgcFree(FBody);
  sgcFree(FPayload);
end;

function TsgcAMQPMessageContent.GetBody: TsgcAMQPFramePayloadType_ContentBody;
begin
  if not Assigned(FBody) then
    FBody := TsgcAMQPFramePayloadType_ContentBody.Create;
  result := FBody;
end;

function TsgcAMQPMessageContent.GetHeader
  : TsgcAMQPFramePayloadType_ContentHeader;
begin
  if not Assigned(FHeader) then
    FHeader := TsgcAMQPFramePayloadType_ContentHeader.Create;
  result := FHeader;
end;

procedure TsgcAMQPMessageContent.SetBody(const Value
  : TsgcAMQPFramePayloadType_ContentBody);
begin
  if Assigned(Value) then
  begin
    Body.DataSize := Value.DataSize;
    Body.Data.CopyFrom(Value.Data, Value.Data.Size);
    Body.Data.Position := 0;
  end;
end;

procedure TsgcAMQPMessageContent.SetHeader(const Value
  : TsgcAMQPFramePayloadType_ContentHeader);
begin
  if Assigned(Value) then
  begin
    Header.ClassId := Value.ClassId;
    Header.Weight := Value.Weight;
    Header.BodySize := Value.BodySize;
    Header.Header := Value.Header;
  end;
end;

procedure TsgcAMQPMessageContent.SetPayload(const Value
  : TsgcAMQPFramePayload_Method_Basic);
begin
  if Assigned(Value) then
  begin
    sgcFree(FPayload);
    if Value.ClassType = TsgcAMQPFramePayload_Method_BasicReturn then
    begin
      FPayload := TsgcAMQPFramePayload_Method_BasicReturn.Create;
      TsgcAMQPFramePayload_Method_BasicReturn(FPayload).ReplyCode :=
        TsgcAMQPFramePayload_Method_BasicReturn(Value).ReplyCode;
      TsgcAMQPFramePayload_Method_BasicReturn(FPayload).ReplyText :=
        TsgcAMQPFramePayload_Method_BasicReturn(Value).ReplyText;
      TsgcAMQPFramePayload_Method_BasicReturn(FPayload).Exchange :=
        TsgcAMQPFramePayload_Method_BasicReturn(Value).Exchange;
      TsgcAMQPFramePayload_Method_BasicReturn(FPayload).RoutingKey :=
        TsgcAMQPFramePayload_Method_BasicReturn(Value).RoutingKey;
    end
    else if Value.ClassType = TsgcAMQPFramePayload_Method_BasicDeliver then
    begin
      FPayload := TsgcAMQPFramePayload_Method_BasicDeliver.Create;
      TsgcAMQPFramePayload_Method_BasicDeliver(FPayload).ConsumerTag :=
        TsgcAMQPFramePayload_Method_BasicDeliver(Value).ConsumerTag;
      TsgcAMQPFramePayload_Method_BasicDeliver(FPayload).DeliveryTag :=
        TsgcAMQPFramePayload_Method_BasicDeliver(Value).DeliveryTag;
      TsgcAMQPFramePayload_Method_BasicDeliver(FPayload).Redelivered :=
        TsgcAMQPFramePayload_Method_BasicDeliver(Value).Redelivered;
      TsgcAMQPFramePayload_Method_BasicDeliver(FPayload).Exchange :=
        TsgcAMQPFramePayload_Method_BasicDeliver(Value).Exchange;
      TsgcAMQPFramePayload_Method_BasicDeliver(FPayload).RoutingKey :=
        TsgcAMQPFramePayload_Method_BasicDeliver(Value).RoutingKey;
    end
    else if Value.ClassType = TsgcAMQPFramePayload_Method_BasicGetOk then
    begin
      FPayload := TsgcAMQPFramePayload_Method_BasicGetOk.Create;
      TsgcAMQPFramePayload_Method_BasicGetOk(FPayload).DeliveryTag :=
        TsgcAMQPFramePayload_Method_BasicGetOk(Value).DeliveryTag;
      TsgcAMQPFramePayload_Method_BasicGetOk(FPayload).Redelivered :=
        TsgcAMQPFramePayload_Method_BasicGetOk(Value).Redelivered;
      TsgcAMQPFramePayload_Method_BasicGetOk(FPayload).Exchange :=
        TsgcAMQPFramePayload_Method_BasicGetOk(Value).Exchange;
      TsgcAMQPFramePayload_Method_BasicGetOk(FPayload).RoutingKey :=
        TsgcAMQPFramePayload_Method_BasicGetOk(Value).RoutingKey;
      TsgcAMQPFramePayload_Method_BasicGetOk(FPayload).MessageCount :=
        TsgcAMQPFramePayload_Method_BasicGetOk(Value).MessageCount;
    end
    else
      DoRaiseAMQPException(CS_AMQP_ERROR_UNEXPECTED_FRAME);
  end;
end;

constructor TsgcAMQPFramePayload_Method_BasicDeliver.Create;
begin
  inherited;
  FMethodId := amqpBasicDeliver;
end;

procedure TsgcAMQPFramePayload_Method_BasicDeliver.DoRead(const aBytes: TBytes);
begin
  inherited;
  ConsumerTag := sgcReadAMQPShortString(aBytes, FOffset);
  DeliveryTag := sgcReadAMQPUInt64(aBytes, FOffset);
  Redelivered := sgcReadAMQPBoolean(aBytes, FOffset);
  Exchange := sgcReadAMQPShortString(aBytes, FOffset);
  RoutingKey := sgcReadAMQPShortString(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_BasicGet.Create;
begin
  inherited;
  FMethodId := amqpBasicGet;
end;

function TsgcAMQPFramePayload_Method_BasicGet.DoWrite: TBytes;
var
  i: Integer;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt16(Reserved1, result);
  sgcWriteAMQPShortString(Queue, result);
  i := Length(result);
  SetLength(result, i + 1);
  if NoAck then
    sgcWriteAMQPBit(i, 0, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicGetOk.Create;
begin
  inherited;
  FMethodId := amqpBasicGetOk;
end;

procedure TsgcAMQPFramePayload_Method_BasicGetOk.DoRead(const aBytes: TBytes);
begin
  inherited;
  DeliveryTag := sgcReadAMQPUInt64(aBytes, FOffset);
  Redelivered := sgcReadAMQPBoolean(aBytes, FOffset);
  Exchange := sgcReadAMQPShortString(aBytes, FOffset);
  RoutingKey := sgcReadAMQPShortString(aBytes, FOffset);
  MessageCount := sgcReadAMQPInt32(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_BasicGetEmpty.Create;
begin
  inherited;
  FMethodId := amqpBasicGetEmpty;
end;

procedure TsgcAMQPFramePayload_Method_BasicGetEmpty.DoRead
  (const aBytes: TBytes);
begin
  inherited;
  Reserved1 := sgcReadAMQPUInt16(aBytes, FOffset);
end;

constructor TsgcAMQPFramePayload_Method_BasicAck.Create;
begin
  inherited;
  FMethodId := amqpBasicAck;
end;

function TsgcAMQPFramePayload_Method_BasicAck.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt64(DeliveryTag, result);
  sgcWriteAMQPBoolean(Multiple, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicReject.Create;
begin
  inherited;
  FMethodId := amqpBasicReject;
end;

function TsgcAMQPFramePayload_Method_BasicReject.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPUInt64(DeliveryTag, result);
  sgcWriteAMQPBoolean(Requeue, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicRecoverAsync.Create;
begin
  inherited;
  FMethodId := amqpBasicRecoverAsync;
end;

function TsgcAMQPFramePayload_Method_BasicRecoverAsync.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPBoolean(Requeue, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicRecover.Create;
begin
  inherited;
  FMethodId := amqpBasicRecover;
end;

function TsgcAMQPFramePayload_Method_BasicRecover.DoWrite: TBytes;
begin
  result := inherited DoWrite;

  sgcWriteAMQPBoolean(Requeue, result);
end;

constructor TsgcAMQPFramePayload_Method_BasicRecoverOk.Create;
begin
  inherited;
  FMethodId := amqpBasicRecoverOk;
end;

procedure TsgcAMQPFramePayload_Method_BasicRecoverOk.DoRead
  (const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_Tx.Create;
begin
  inherited;
  FClassId := amqpClassTx;
end;

constructor TsgcAMQPFramePayload_Method_TxSelect.Create;
begin
  inherited;
  FMethodId := amqpTxSelect;
end;

function TsgcAMQPFramePayload_Method_TxSelect.DoWrite: TBytes;
begin
  result := inherited DoWrite;
end;

constructor TsgcAMQPFramePayload_Method_TxSelectOk.Create;
begin
  inherited;
  FMethodId := amqpTxSelectOk;
end;

procedure TsgcAMQPFramePayload_Method_TxSelectOk.DoRead(const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_TxCommit.Create;
begin
  inherited;
  FMethodId := amqpTxCommit;
end;

function TsgcAMQPFramePayload_Method_TxCommit.DoWrite: TBytes;
begin
  result := inherited DoWrite;
end;

constructor TsgcAMQPFramePayload_Method_TxCommitOk.Create;
begin
  inherited;
  FMethodId := amqpTxCommitOk;
end;

procedure TsgcAMQPFramePayload_Method_TxCommitOk.DoRead(const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPFramePayload_Method_TxRollback.Create;
begin
  inherited;
  FMethodId := amqpTxRollback;
end;

function TsgcAMQPFramePayload_Method_TxRollback.DoWrite: TBytes;
begin
  result := inherited DoWrite;
end;

constructor TsgcAMQPFramePayload_Method_TxRollbackOk.Create;
begin
  inherited;
  FMethodId := amqpTxRollbackOk;
end;

procedure TsgcAMQPFramePayload_Method_TxRollbackOk.DoRead(const aBytes: TBytes);
begin
  inherited;
end;

constructor TsgcAMQPChannelThreadWaitRequest.Create;
begin
  inherited;
  Initialize;
end;

destructor TsgcAMQPChannelThreadWaitRequest.Destroy;
begin
  inherited;
  // TODO -cMM: TsgcAMQPChannelThreadWaitRequest.Destroy default body inserted
end;

procedure TsgcAMQPChannelThreadWaitRequest.Initialize;
begin
  Method := amqpMethodNone;
  Methods := [];
  Terminated := False;
end;

end.
