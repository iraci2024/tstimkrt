{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_MQTT_Message;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcWebSocket_Types, sgcBase_Classes, sgcBase_Helpers;

type

  TwsMQTTPayloadFormat = (mqpfUnspecified, mqpfUTF8);
  TwsMQTTReadState = (mqrsFixedHeader, mqrsMessage);

  TsgcWSTopic = class
  private
    FQoS: TmqttQoS;
    FTopic: String;
  public
    property QoS: TmqttQoS read FQoS write FQoS;
    property Topic: String read FTopic write FTopic;
  end;

  TsgcWSTopics = class({$IFDEF NEXTGEN} TList<TsgcWSTopic>{$ELSE} TObjectList
{$ENDIF})

  end;

  TsgcWSSUBACK = class
  private
    FReasonCode: Integer;
    FReasonName: String;
  public
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: String read FReasonName write FReasonName;
  end;

  TsgcWSSUBACKS = class({$IFDEF NEXTGEN} TList<TsgcWSSUBACK>{$ELSE} TObjectList
{$ENDIF})
  private
    function GetItem(i: Integer): TsgcWSSUBACK;
    procedure SetItem(i: Integer; const Value: TsgcWSSUBACK);
  public
    property Item[i: Integer]: TsgcWSSUBACK read GetItem write SetItem;
  end;

  TsgcWSUNSUBACK = class
  private
    FReasonCode: Integer;
    FReasonName: String;
  public
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: String read FReasonName write FReasonName;
  end;

  TsgcWSUNSUBACKS = class({$IFDEF NEXTGEN} TList<TsgcWSUNSUBACK>{$ELSE} TObjectList
{$ENDIF})
  private
    function GetItem(i: Integer): TsgcWSUNSUBACK;
    procedure SetItem(i: Integer; const Value: TsgcWSUNSUBACK);
  public
    property Item[i: Integer]: TsgcWSUNSUBACK read GetItem write SetItem;
  end;

  TsgcWSMQTTFixedHeader = class(TComponent)
  private
    FControlPacket: TMQTTControlPacket;
    FDuplicate: Boolean;
    FRetain: Boolean;
    FQoS: TmqttQoS;
    FRemainingLength: Integer;
    function GetBytes: TIdBytes;
    function GetSize: Integer;
  public
    constructor Create(aOwner: TComponent); override;
  public
    property Bytes: TIdBytes read GetBytes;
    property Size: Integer read GetSize;
  public
    property ControlPacket: TMQTTControlPacket read FControlPacket
      write FControlPacket;
    property Duplicate: Boolean read FDuplicate write FDuplicate;
    property Retain: Boolean read FRetain write FRetain;
    property QoS: TmqttQoS read FQoS write FQoS;
    property RemainingLength: Integer read FRemainingLength
      write FRemainingLength;
  end;

  TsgcWSMQTTVariableHeader = class(TComponent)
  private
    FBytes: TIdBytes;
    function GetSize: Integer;
  public
    procedure AddByte(aByte: Byte);
    procedure AddBytes(aBytes: TIdBytes);
    procedure AddInteger(aValue: Integer);
    procedure AddWord(aValue: Word);
    procedure AddString(const aText: String; aHeader: Boolean = True);
    procedure AddBoolean(aValue: Boolean);
    procedure AddVarInteger(aValue: Integer);
  public
    procedure Clear; virtual;
  public
    property Bytes: TIdBytes read FBytes;
    property Size: Integer read GetSize;
  end;

  TsgcWSMQTTPayLoadHeader = class(TsgcWSMQTTVariableHeader)
  public
    procedure AddStream(const aStream: TStream);
  end;

  TsgcWSMQTTVariableHeaderPacketIdentifier = class(TsgcWSMQTTVariableHeader)
  private
    FPacketIdentifier: Integer;
  public
    procedure Clear; override;
  public
    property PacketIdentifier: Integer read FPacketIdentifier
      write FPacketIdentifier;
  end;

  TsgcWSMQTTReadProperties = class(TsgcWSMQTTVariableHeader)
  protected
    function GetInteger(const aBytes: TIdBytes; aLength: Integer;
      var aOffset: Integer): Integer;
    function GetBoolean(const aBytes: TIdBytes; var aOffset: Integer): Boolean;
    function GetString(const aBytes: TIdBytes; var aOffset: Integer): String;
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); virtual; abstract;
  public
    procedure ReadBytes(const aBytes: TIdBytes);
  end;

  TsgcWSMQTTCONNACKProperties = class(TsgcWSMQTTReadProperties)
  private
    FAssignedClientIdentifier: String;
    FAuthenticationData: string;
    FAuthenticationMethod: String;
    FMaximumPacketSize: Integer;
    FMaximumQoS: TmqttQoS;
    FReasonString: String;
    FReceiveMaximum: Integer;
    FResponseInformation: String;
    FRetainAvailable: Boolean;
    FServerKeepAlive: Integer;
    FServerReference: String;
    FSessionExpiryInterval: Integer;
    FSharedSubscriptionAvailable: Boolean;
    FSubscriptionIdentifiersAvailable: Boolean;
    FTopicAliasMaximum: Integer;
    FUserProperties: TStringList;
    FWildcardSubscriptionAvailable: Boolean;
    function GetUserProperties: TStringList;
    procedure SetSubscriptionIdentifiersAvailable(const Value: Boolean);
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property AssignedClientIdentifier: String read FAssignedClientIdentifier
      write FAssignedClientIdentifier;
    property AuthenticationData: string read FAuthenticationData
      write FAuthenticationData;
    property AuthenticationMethod: String read FAuthenticationMethod
      write FAuthenticationMethod;
    property MaximumPacketSize: Integer read FMaximumPacketSize
      write FMaximumPacketSize;
    property MaximumQoS: TmqttQoS read FMaximumQoS write FMaximumQoS;
    property ReasonString: String read FReasonString write FReasonString;
    property ReceiveMaximum: Integer read FReceiveMaximum write FReceiveMaximum;
    property ResponseInformation: String read FResponseInformation
      write FResponseInformation;
    property RetainAvailable: Boolean read FRetainAvailable
      write FRetainAvailable;
    property ServerKeepAlive: Integer read FServerKeepAlive
      write FServerKeepAlive;
    property ServerReference: String read FServerReference
      write FServerReference;
    property SessionExpiryInterval: Integer read FSessionExpiryInterval
      write FSessionExpiryInterval;
    property SharedSubscriptionAvailable: Boolean
      read FSharedSubscriptionAvailable write FSharedSubscriptionAvailable;
    property SubscriptionIdentifiersAvailable: Boolean
      read FSubscriptionIdentifiersAvailable
      write SetSubscriptionIdentifiersAvailable;
    property TopicAliasMaximum: Integer read FTopicAliasMaximum
      write FTopicAliasMaximum;
    property UserProperties: TStringList read GetUserProperties
      write FUserProperties;
    property WildcardSubscriptionAvailable: Boolean
      read FWildcardSubscriptionAvailable write FWildcardSubscriptionAvailable;
  end;

  TsgcWSMQTTCONNACK = class(TsgcWSMQTTVariableHeader)
  private
    FCONNACKProperties: TsgcWSMQTTCONNACKProperties;
    FReasonCode: Integer;
    FReasonName: String;
    FSession: Boolean;
    function GetCONNACKProperties: TsgcWSMQTTCONNACKProperties;
  public
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property CONNACKProperties: TsgcWSMQTTCONNACKProperties
      read GetCONNACKProperties write FCONNACKProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: String read FReasonName write FReasonName;
    property Session: Boolean read FSession write FSession;
  end;

  TsgcWSMQTTSUBACKProperties = class(TsgcWSMQTTReadProperties)
  private
    FReasonString: String;
    FUserProperties: TStringList;
    procedure SetUserProperties(const Value: TStringList);
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property ReasonString: String read FReasonString write FReasonString;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTSUBACK = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FCodes: TsgcWSSUBACKS;
    FSUBACKProperties: TsgcWSMQTTSUBACKProperties;
    function GetCodes: TsgcWSSUBACKS;
    function GetSUBACKProperties: TsgcWSMQTTSUBACKProperties;
  public
    procedure Clear; override;
  public
    destructor Destroy; override;
  public
    property Codes: TsgcWSSUBACKS read GetCodes write FCodes;
    property SUBACKProperties: TsgcWSMQTTSUBACKProperties
      read GetSUBACKProperties write FSUBACKProperties;
  end;

  TsgcWSMQTTPUBSUBACKProperties = class(TsgcWSMQTTReadProperties)
  private
    FReasonString: String;
    FUserProperties: TStringList;
    procedure SetUserProperties(const Value: TStringList);
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property ReasonString: String read FReasonString write FReasonString;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTUNSUBACKProperties = class(TsgcWSMQTTPUBSUBACKProperties)
  end;

  TsgcWSMQTTUNSUBACK = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FCodes: TsgcWSUNSUBACKS;
    FUNSUBACKProperties: TsgcWSMQTTUNSUBACKProperties;
    function GetCodes: TsgcWSUNSUBACKS;
    function GetUNSUBACKProperties: TsgcWSMQTTUNSUBACKProperties;
  public
    procedure Clear; override;
  public
    property Codes: TsgcWSUNSUBACKS read GetCodes write FCodes;
    property UNSUBACKProperties: TsgcWSMQTTUNSUBACKProperties
      read GetUNSUBACKProperties write FUNSUBACKProperties;
  end;

  TsgcWSMQTTPUBACKProperties = class(TsgcWSMQTTPUBSUBACKProperties)
  end;

  TsgcWSMQTTPUBACK = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FPUBACKProperties: TsgcWSMQTTPUBACKProperties;
    FReasonCode: Integer;
    FReasonName: string;
    function GetPUBACKProperties: TsgcWSMQTTPUBACKProperties;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property PUBACKProperties: TsgcWSMQTTPUBACKProperties
      read GetPUBACKProperties write FPUBACKProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: string read FReasonName write FReasonName;
  end;

  TsgcWSMQTTPUBRECProperties = class(TsgcWSMQTTPUBSUBACKProperties)
  end;

  TsgcWSMQTTPUBREC = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FPUBRECProperties: TsgcWSMQTTPUBRECProperties;
    FReasonCode: Integer;
    FReasonName: string;
    function GetPUBRECProperties: TsgcWSMQTTPUBRECProperties;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property PUBRECProperties: TsgcWSMQTTPUBRECProperties
      read GetPUBRECProperties write FPUBRECProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: string read FReasonName write FReasonName;
  end;

  TsgcWSMQTTPUBRELProperties = class(TsgcWSMQTTPUBSUBACKProperties)
  end;

  TsgcWSMQTTPUBREL = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FPUBRELProperties: TsgcWSMQTTPUBRELProperties;
    FReasonCode: Integer;
    FReasonName: string;
    function GetPUBRELProperties: TsgcWSMQTTPUBRELProperties;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property PUBRELProperties: TsgcWSMQTTPUBRELProperties
      read GetPUBRELProperties write FPUBRELProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: string read FReasonName write FReasonName;
  end;

  TsgcWSMQTTPUBCOMPProperties = class(TsgcWSMQTTPUBSUBACKProperties)
  end;

  TsgcWSMQTTPUBCOMP = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FPUBCOMPProperties: TsgcWSMQTTPUBCOMPProperties;
    FReasonCode: Integer;
    FReasonName: string;
    function GetPUBCOMPProperties: TsgcWSMQTTPUBCOMPProperties;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property PUBCOMPProperties: TsgcWSMQTTPUBCOMPProperties
      read GetPUBCOMPProperties write FPUBCOMPProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: string read FReasonName write FReasonName;
  end;

  TsgcWSMQTTDisconnect_Properties = class(TPersistent)
  private
    FEnabled: Boolean;
    FReasonString: String;
    FServerReference: String;
    FSessionExpiryInterval: Integer;
    FUserProperties: TStringList;
    function GetSessionExpiryInterval: Integer;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property ReasonString: String read FReasonString write FReasonString;
    property ServerReference: String read FServerReference
      write FServerReference;
    property SessionExpiryInterval: Integer read GetSessionExpiryInterval
      write FSessionExpiryInterval;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTDISCONNECTProperties = class(TsgcWSMQTTReadProperties)
  private
    FReasonString: String;
    FServerReference: String;
    FSessionExpiryInterval: Integer;
    FUserProperties: TStringList;
    function GetSessionExpiryInterval: Integer;
    function GetUserProperties: TStringList;
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property ReasonString: String read FReasonString write FReasonString;
    property ServerReference: String read FServerReference
      write FServerReference;
    property SessionExpiryInterval: Integer read GetSessionExpiryInterval
      write FSessionExpiryInterval;
    property UserProperties: TStringList read GetUserProperties
      write FUserProperties;
  end;

  TsgcWSMQTTDISCONNECT = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FDISCONNECTProperties: TsgcWSMQTTDISCONNECTProperties;
    FReasonCode: Integer;
    FReasonName: string;
    function GetDISCONNECTProperties: TsgcWSMQTTDISCONNECTProperties;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property DISCONNECTProperties: TsgcWSMQTTDISCONNECTProperties
      read GetDISCONNECTProperties write FDISCONNECTProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: string read FReasonName write FReasonName;
  end;

  TsgcWSMQTTConnectProperties = class(TsgcWSMQTTVariableHeader)
  end;

  TsgcWSMQTTConnect_Properties = class(TPersistent)
  private
    FAuthenticationMethod: String;
    FEnabled: Boolean;
    FMaximumPacketSize: Integer;
    FReceiveMaximum: Integer;
    FRequestProblemInformation: Boolean;
    FRequestResponseInformation: Boolean;
    FSessionExpiryInterval: Integer;
    FTopicAliasMaximum: Integer;
    FUserProperties: TStringList;
    function GetMaximumPacketSize: Integer;
    function GetReceiveMaximum: Integer;
    function GetSessionExpiryInterval: Integer;
    function GetTopicAliasMaximum: Integer;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AuthenticationMethod: String read FAuthenticationMethod
      write FAuthenticationMethod;
    property Enabled: Boolean read FEnabled write FEnabled;
    property MaximumPacketSize: Integer read GetMaximumPacketSize
      write FMaximumPacketSize;
    property ReceiveMaximum: Integer read GetReceiveMaximum
      write FReceiveMaximum;
    property RequestProblemInformation: Boolean read FRequestProblemInformation
      write FRequestProblemInformation;
    property RequestResponseInformation: Boolean
      read FRequestResponseInformation write FRequestResponseInformation;
    property SessionExpiryInterval: Integer read GetSessionExpiryInterval
      write FSessionExpiryInterval;
    property TopicAliasMaximum: Integer read GetTopicAliasMaximum
      write FTopicAliasMaximum;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTWillProperties = class(TsgcWSMQTTVariableHeader)
  end;

  TsgcWSMQTTWill_Properties = class(TPersistent)
  private
    FContentType: string;
    FCorrelationData: String;
    FEnabled: Boolean;
    FMessageExpiryInterval: Integer;
    FPayloadFormat: TwsMQTTPayloadFormat;
    FResponseTopic: String;
    FUserProperties: TStringList;
    FWillDelayInterval: Integer;
    function GetMessageExpiryInterval: Integer;
    function GetWillDelayInterval: Integer;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
  published
    property ContentType: string read FContentType write FContentType;
    property CorrelationData: String read FCorrelationData
      write FCorrelationData;
    property MessageExpiryInterval: Integer read GetMessageExpiryInterval
      write FMessageExpiryInterval;
    property PayloadFormat: TwsMQTTPayloadFormat read FPayloadFormat
      write FPayloadFormat;
    property ResponseTopic: String read FResponseTopic write FResponseTopic;
    property WillDelayInterval: Integer read GetWillDelayInterval
      write FWillDelayInterval;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTPublishProperties = class(TsgcWSMQTTReadProperties)
  private
    FContentType: string;
    FCorrelationData: String;
    FEnabled: Boolean;
    FMessageExpiryInterval: Integer;
    FPayloadFormat: TwsMQTTPayloadFormat;
    FResponseTopic: String;
    FSubscriptionIdentifier: Integer;
    FTopicAlias: Integer;
    FUserProperties: TStringList;
    function GetUserProperties: TStringList;
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  published
    property ContentType: string read FContentType write FContentType;
    property CorrelationData: String read FCorrelationData
      write FCorrelationData;
    property Enabled: Boolean read FEnabled write FEnabled;
    property MessageExpiryInterval: Integer read FMessageExpiryInterval
      write FMessageExpiryInterval;
    property PayloadFormat: TwsMQTTPayloadFormat read FPayloadFormat
      write FPayloadFormat;
    property ResponseTopic: String read FResponseTopic write FResponseTopic;
    property SubscriptionIdentifier: Integer read FSubscriptionIdentifier
      write FSubscriptionIdentifier;
    property TopicAlias: Integer read FTopicAlias write FTopicAlias;
    property UserProperties: TStringList read GetUserProperties
      write FUserProperties;
  end;

  TsgcWSMQTTPublish_Properties = class(TPersistent)
  private
    FContentType: string;
    FCorrelationData: String;
    FEnabled: Boolean;
    FMessageExpiryInterval: Integer;
    FPayloadFormat: TwsMQTTPayloadFormat;
    FResponseTopic: String;
    FSubscriptionIdentifier: Integer;
    FUserProperties: TStringList;
    FTopicAlias: Integer;
    function GetMessageExpiryInterval: Integer;
    function GetSubscriptionIdentifier: Integer;
    function GetTopicAlias: Integer;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ContentType: string read FContentType write FContentType;
    property CorrelationData: String read FCorrelationData
      write FCorrelationData;
    property Enabled: Boolean read FEnabled write FEnabled;
    property MessageExpiryInterval: Integer read GetMessageExpiryInterval
      write FMessageExpiryInterval;
    property PayloadFormat: TwsMQTTPayloadFormat read FPayloadFormat
      write FPayloadFormat;
    property ResponseTopic: String read FResponseTopic write FResponseTopic;
    property SubscriptionIdentifier: Integer read GetSubscriptionIdentifier
      write FSubscriptionIdentifier;
    property TopicAlias: Integer read GetTopicAlias write FTopicAlias;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTPUBLISH = class(TsgcWSMQTTVariableHeader)
  private
    FPacketIdentifier: Integer;
    FPUBLISHProperties: TsgcWSMQTTPublishProperties;
    FText: string;
    FTopic: String;
    function GetPUBLISHProperties: TsgcWSMQTTPublishProperties;
  public
    procedure Clear; override;
  public
    property PacketIdentifier: Integer read FPacketIdentifier
      write FPacketIdentifier;
    property PUBLISHProperties: TsgcWSMQTTPublishProperties
      read GetPUBLISHProperties write FPUBLISHProperties;
    property Text: string read FText write FText;
    property Topic: String read FTopic write FTopic;
  end;

  TsgcWSMQTTSubscribeProperties = class(TsgcWSMQTTVariableHeader)
  end;

  TsgcWSMQTTSubscribe_Properties = class(TPersistent)
  private
    FEnabled: Boolean;
    FSubscriptionIdentifier: Integer;
    FUserProperties: TStringList;
    function GetSubscriptionIdentifier: Integer;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property SubscriptionIdentifier: Integer read GetSubscriptionIdentifier
      write FSubscriptionIdentifier;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTUnSubscribeProperties = class(TsgcWSMQTTVariableHeader)
  end;

  TsgcWSMQTTUnsubscribe_Properties = class(TPersistent)
  private
    FEnabled: Boolean;
    FUserProperties: TStringList;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTAuth_Properties = class(TPersistent)
  private
    FEnabled: Boolean;
    FReasonString: String;
    FAuthenticationMethod: String;
    FAuthenticationData: String;
    FUserProperties: TStringList;
    procedure SetUserProperties(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property ReasonString: String read FReasonString write FReasonString;
    property AuthenticationMethod: String read FAuthenticationMethod
      write FAuthenticationMethod;
    property AuthenticationData: String read FAuthenticationData
      write FAuthenticationData;
    property UserProperties: TStringList read FUserProperties
      write SetUserProperties;
  end;

  TsgcWSMQTTAUTHProperties = class(TsgcWSMQTTReadProperties)
  private
    FReasonString: String;
    FAuthenticationMethod: String;
    FAuthenticationData: String;
    FUserProperties: TStringList;
    function GetUserProperties: TStringList;
  protected
    procedure DoReadBytes(const aBytes: TIdBytes); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property ReasonString: String read FReasonString write FReasonString;
    property AuthenticationMethod: String read FAuthenticationMethod
      write FAuthenticationMethod;
    property AuthenticationData: String read FAuthenticationData
      write FAuthenticationData;
    property UserProperties: TStringList read GetUserProperties
      write FUserProperties;
  end;

  TsgcWSMQTTAUTH = class(TsgcWSMQTTVariableHeaderPacketIdentifier)
  private
    FAUTHProperties: TsgcWSMQTTAUTHProperties;
    FReasonCode: Integer;
    FReasonName: string;
    function GetAUTHProperties: TsgcWSMQTTAUTHProperties;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    property AUTHProperties: TsgcWSMQTTAUTHProperties read GetAUTHProperties
      write FAUTHProperties;
    property ReasonCode: Integer read FReasonCode write FReasonCode;
    property ReasonName: string read FReasonName write FReasonName;
  end;

  TsgcWSMQTTReadPacketBuffer = class(TComponent)
  private
    FControlPacket: TMQTTControlPacket;
    FFixedHeaderLength: Integer;
    FStreamBuffer: TMemoryStream;
    FReadPacketState: TwsMQTTReadState;
    FRemainingLength: Integer;
    function GetStreamBuffer: TMemoryStream;
    procedure SetReadPacketState(const Value: TwsMQTTReadState);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Read(const aStream: TStream): Boolean;
  public
    property ControlPacket: TMQTTControlPacket read FControlPacket write
        FControlPacket;
    property FixedHeaderLength: Integer read FFixedHeaderLength write
        FFixedHeaderLength;
    property StreamBuffer: TMemoryStream read GetStreamBuffer write FStreamBuffer;
    property ReadPacketState: TwsMQTTReadState read FReadPacketState write
        SetReadPacketState;
    property RemainingLength: Integer read FRemainingLength write FRemainingLength;
  end;

  TsgcWSMQTTMessage = class(TsgcThreadSafeComponent)
  private
    FConnectionId: string;
    FStreamRead: TStream;
    FThreadId: Cardinal;
    FStreamReadPacketBuffer: TsgcWSMQTTReadPacketBuffer;
    function GetStreamReadPacketBuffer: TsgcWSMQTTReadPacketBuffer;
  protected
    property StreamReadPacketBuffer: TsgcWSMQTTReadPacketBuffer read
        GetStreamReadPacketBuffer write FStreamReadPacketBuffer;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Clear;
    procedure ClearStreamReadTCP;
  public
    property StreamRead: TStream read FStreamRead write FStreamRead;
  private
    function GetByte(aBit7, aBit6, aBit5, aBit4, aBit3, aBit2, aBit1,
      aBit0: Boolean): Byte;
  private
    function GetRemainingLength: TIdBytes;
    function GetConnectFlags(aAuthentication: Boolean;
      aUserName, aPassword: String; aWillFlag: Boolean; aQoS: TmqttQoS;
      aRetain: Boolean): Byte;
    function GetMSBLSB(aValue: Integer): TIdBytes;
  private
    procedure DoConnectProperties;
    procedure DoWillProperties;
    procedure DoPublishProperties;
    procedure DoSubscribeProperties;
    procedure DoUnsubscribeProperties;
    procedure DoDisconnectProperties;
    procedure DoAuthProperties;
  protected
    function WriteBytes: TBytes;
  private
    procedure DoReadFixedHeader;
  public
    procedure Read(const aStream: TStream); virtual;
    function ReadTCP(const aStream: TStream): Boolean; virtual;
  public
    function DoConnect(aAuthentication: Boolean; aAuthUser, aAuthPass: String;
        aWillFlag: Boolean; aWillTopic, aWillText: String; aWillQoS: TmqttQoS;
        aWillRetain, aKeepAliveEnabled: Boolean; aKeepAliveInterval: Integer):
        TBytes; virtual;
    function DoPing: TBytes; virtual;
    function DoPublish(aPacketIdentifier: Word; const aTopic, aText: String; aQoS:
        TmqttQoS; aRetain, aDuplicate: Boolean): TBytes; overload; virtual;
    function DoPublish(aPacketIdentifier: Word; const aTopic: String; aStream:
        TStream; aQoS: TmqttQoS; aRetain, aDuplicate: Boolean): TBytes; overload;
        virtual;
    function DoSubscribe(aPacketIdentifier: Word; aTopics: TsgcWSTopics): TBytes;
        virtual;
    function DoUnSubscribe(aPacketIdentifier: Word; aTopics: TsgcWSTopics): TBytes;
        virtual;
    function DoPubAck(aPacketIdentifier: Word): TBytes; virtual;
    function DoPubRec(aPacketIdentifier: Word): TBytes; virtual;
    function DoPubRel(aPacketIdentifier: Word): TBytes; virtual;
    function DoPubComp(aPacketIdentifier: Word): TBytes; virtual;
    function DoDisconnect(aReasonCode: Integer = 0): TBytes; virtual;
    function DoAuth(aReAuthenticate: Boolean): TBytes; virtual;
  public
    property ConnectionId: string read FConnectionId write FConnectionId;
    property ThreadId: Cardinal read FThreadId write FThreadId;
    { fixed header }
  private
    FFixedHeader: TsgcWSMQTTFixedHeader;
    function GetFixedHeader: TsgcWSMQTTFixedHeader;
  public
    property FixedHeader: TsgcWSMQTTFixedHeader read GetFixedHeader
      write FFixedHeader;
    { fixed header }

    { variable header }
  private
    FVariableHeader: TsgcWSMQTTVariableHeader;
    function GetVariableHeader: TsgcWSMQTTVariableHeader;
  public
    property VariableHeader: TsgcWSMQTTVariableHeader read GetVariableHeader
      write FVariableHeader;
    { variable header }

    { payload header }
  private
    FPayLoadHeader: TsgcWSMQTTPayLoadHeader;
    function GetPayLoadHeader: TsgcWSMQTTPayLoadHeader;
  public
    property PayLoadHeader: TsgcWSMQTTPayLoadHeader read GetPayLoadHeader
      write FPayLoadHeader;
    { payload header }

    { ACK }
  private
    FCONNACK: TsgcWSMQTTCONNACK;
    FPUBLISH: TsgcWSMQTTPUBLISH;
    FPUBACK: TsgcWSMQTTPUBACK;
    FPUBREC: TsgcWSMQTTPUBREC;
    FPUBREL: TsgcWSMQTTPUBREL;
    FPUBCOMP: TsgcWSMQTTPUBCOMP;
    FSUBACK: TsgcWSMQTTSUBACK;
    FUNSUBACK: TsgcWSMQTTUNSUBACK;
    FDISCONNECT: TsgcWSMQTTDISCONNECT;
    FAUTH: TsgcWSMQTTAUTH;
    function GetCONNACK: TsgcWSMQTTCONNACK;
    function GetPUBLISH: TsgcWSMQTTPUBLISH;
    function GetPUBACK: TsgcWSMQTTPUBACK;
    function GetPUBREC: TsgcWSMQTTPUBREC;
    function GetPUBREL: TsgcWSMQTTPUBREL;
    function GetPUBCOMP: TsgcWSMQTTPUBCOMP;
    function GetSUBACK: TsgcWSMQTTSUBACK;
    function GetUNSUBACK: TsgcWSMQTTUNSUBACK;
    function GetDISCONNECT: TsgcWSMQTTDISCONNECT;
    function GetAUTH: TsgcWSMQTTAUTH;
  public
    property CONNACK: TsgcWSMQTTCONNACK read GetCONNACK write FCONNACK;
    property PUBLISH: TsgcWSMQTTPUBLISH read GetPUBLISH write FPUBLISH;
    property PUBACK: TsgcWSMQTTPUBACK read GetPUBACK write FPUBACK;
    property PUBREC: TsgcWSMQTTPUBREC read GetPUBREC write FPUBREC;
    property PUBREL: TsgcWSMQTTPUBREL read GetPUBREL write FPUBREL;
    property PUBCOMP: TsgcWSMQTTPUBCOMP read GetPUBCOMP write FPUBCOMP;
    property SUBACK: TsgcWSMQTTSUBACK read GetSUBACK write FSUBACK;
    property UNSUBACK: TsgcWSMQTTUNSUBACK read GetUNSUBACK write FUNSUBACK;
    property DISCONNECT: TsgcWSMQTTDISCONNECT read GetDISCONNECT
      write FDISCONNECT;
    property AUTH: TsgcWSMQTTAUTH read GetAUTH write FAUTH;
    { ACK }

    { control packets }
  private
    procedure DoReadCONNACK(const aBytes: TIdBytes);
    procedure DoReadPUBLISH(const aBytes: TIdBytes);
    procedure DoReadPUBACK(const aBytes: TIdBytes);
    procedure DoReadPUBREC(const aBytes: TIdBytes);
    procedure DoReadPUBREL(const aBytes: TIdBytes);
    procedure DoReadPUBCOMP(const aBytes: TIdBytes);
    procedure DoReadSUBACK(const aBytes: TIdBytes);
    procedure DoReadUNSUBACK(const aBytes: TIdBytes);
    procedure DoReadDISCONNECT(const aBytes: TIdBytes);
    procedure DoReadAUTH(const aBytes: TIdBytes);
    { control packets }

    { properties }
  private
    FCleanSession: Boolean;
    FClientId: String;
    FMQTTVersion: TwsMQTTVersion;
    function GetClientId: String;
  public
    property CleanSession: Boolean read FCleanSession write FCleanSession;
    property ClientId: String read GetClientId write FClientId;
    property MQTTVersion: TwsMQTTVersion read FMQTTVersion write FMQTTVersion;
    { properties }

    { MQTT5 properties }
  private
    FConnectProperties: TsgcWSMQTTConnect_Properties;
    FWillProperties: TsgcWSMQTTWill_Properties;
    FPUBLISHProperties: TsgcWSMQTTPublish_Properties;
    FSubscribeProperties: TsgcWSMQTTSubscribe_Properties;
    FUnsubscribeProperties: TsgcWSMQTTUnsubscribe_Properties;
    FDISCONNECTProperties: TsgcWSMQTTDisconnect_Properties;
    FAUTHProperties: TsgcWSMQTTAuth_Properties;
    procedure SetConnectProperties(const Value: TsgcWSMQTTConnect_Properties);
    procedure SetWillProperties(const Value: TsgcWSMQTTWill_Properties);
    procedure SetPublishProperties(const Value: TsgcWSMQTTPublish_Properties);
    procedure SetSubscribeProperties(const Value
      : TsgcWSMQTTSubscribe_Properties);
    procedure SetUnsubscribeProperties(const Value
      : TsgcWSMQTTUnsubscribe_Properties);
    procedure SetDisconnectProperties(const Value
      : TsgcWSMQTTDisconnect_Properties);
    procedure SetAuthProperties(const Value: TsgcWSMQTTAuth_Properties);
  public
    property ConnectProperties: TsgcWSMQTTConnect_Properties
      read FConnectProperties write SetConnectProperties;
    property WillProperties: TsgcWSMQTTWill_Properties read FWillProperties
      write SetWillProperties;
    property PUBLISHProperties: TsgcWSMQTTPublish_Properties
      read FPUBLISHProperties write SetPublishProperties;
    property SubscribeProperties: TsgcWSMQTTSubscribe_Properties
      read FSubscribeProperties write SetSubscribeProperties;
    property UnsubscribeProperties: TsgcWSMQTTUnsubscribe_Properties
      read FUnsubscribeProperties write SetUnsubscribeProperties;
    property DISCONNECTProperties: TsgcWSMQTTDisconnect_Properties
      read FDISCONNECTProperties write SetDisconnectProperties;
    property AUTHProperties: TsgcWSMQTTAuth_Properties read FAUTHProperties
      write SetAuthProperties;
    { MQTT5 properties }
  end;

function GetReasonCodeMQTT311(aCode: Integer): String;
function GetReasonCode(aPacket: TMQTTControlPacket; aCode: Integer): String;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  StrUtils,
  sgcWebSocket_Helpers;

resourcestring
  S_MQTT_ERROR_MALFORMED_VARIABLE_BYTE_INTEGER = 'Malformed variable byte integer.';

procedure GetQosBits(aQoS: TmqttQoS; var aBit1, aBit0: Boolean);
begin
  case aQoS of
    mtqsAtMostOnce:
      begin
        aBit1 := False;
        aBit0 := False;
      end;
    mtqsAtLeastOnce:
      begin
        aBit1 := False;
        aBit0 := True;
      end;
    mtqsExactlyOnce:
      begin
        aBit1 := True;
        aBit0 := False;
      end;
    mtqsReserved:
      begin
        aBit1 := True;
        aBit0 := True;
      end;
  end;
end;

function ReadMSBLSB(aByte1, aByte2: Byte): Integer;
begin
  Result := aByte1 shl 8;
  Result := Result + aByte2;
end;

function ReadString(aBytes: TIdBytes; aHeader: Boolean = True;
  aIndex: Integer = 0): String;
var
  i, j, k, l: Integer;
  oStream: TsgcStringStream;
begin
  Result := '';
  l := Length(aBytes);

  if aHeader and (l > aIndex + 1) then
  begin
    k := ReadMSBLSB(aBytes[aIndex + 0], aBytes[aIndex + 1]) + aIndex + 1;
    if k >= l then
    begin
      j := aIndex;
      k := l - 1
    end
    else
      j := aIndex + 2;
  end
  else
  begin
    j := aIndex;
    k := l - 1;
  end;

  oStream := TsgcStringStream.Create('');
  Try
    for i := j to k do
    begin
      oStream.Write(aBytes[i], 1);
    end;
    Result := sgcBytesToString(oStream);
  Finally
    sgcFree(oStream);
  End;
end;

function GetReasonCode(aPacket: TMQTTControlPacket; aCode: Integer): String;
begin
  case aCode of
    0:
      begin
        case aPacket of
          mtcpSUBACK:
            Result := 'Granted QoS 0';
          mtcpDISCONNECT:
            Result := 'Normal disconnection';
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpPUBREL, mtcpPUBCOMP,
            mtcpUNSUBACK, mtcpAUTH:
            Result := 'Success';
        end;
      end;
    1:
      begin
        case aPacket of
          mtcpSUBACK:
            Result := 'Granted QoS 1';
        end;
      end;
    2:
      begin
        case aPacket of
          mtcpSUBACK:
            Result := 'Granted QoS 2';
        end;
      end;
    4:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Disconnect with Will Message';
        end;
      end;
    16:
      begin
        case aPacket of
          mtcpPUBACK, mtcpPUBREC:
            Result := 'No matching subscribers';
        end;
      end;
    17:
      begin
        case aPacket of
          mtcpUNSUBACK:
            Result := 'No subscription existed';
        end;
      end;
    24:
      begin
        case aPacket of
          mtcpAUTH:
            Result := 'Continue authentication';
        end;
      end;
    25:
      begin
        case aPacket of
          mtcpAUTH:
            Result := 'Re-authenticate';
        end;
      end;
    128:
      begin
        case aPacket of
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpSUBACK, mtcpUNSUBACK,
            mtcpDISCONNECT:
            Result := 'Unspecified error';
        end;
      end;
    129:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Malformed Packet';
        end;
      end;
    130:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Protocol Error';
        end;
      end;
    131:
      begin
        case aPacket of
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpSUBACK, mtcpUNSUBACK,
            mtcpDISCONNECT:
            Result := 'Implementation specific error';
        end;
      end;
    132:
      begin
        case aPacket of
          mtcpCONNACK:
            Result := 'Unsupported Protocol Version';
        end;
      end;
    133:
      begin
        case aPacket of
          mtcpCONNACK:
            Result := 'Client Identifier not valid';
        end;
      end;
    134:
      begin
        case aPacket of
          mtcpCONNACK:
            Result := 'Bad User Name or Password';
        end;
      end;
    135:
      begin
        case aPacket of
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpSUBACK, mtcpUNSUBACK,
            mtcpDISCONNECT:
            Result := 'Not authorized';
        end;
      end;
    136:
      begin
        case aPacket of
          mtcpCONNACK:
            Result := 'Server unavailable';
        end;
      end;
    137:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Server busy';
        end;
      end;
    138:
      begin
        case aPacket of
          mtcpCONNACK:
            Result := 'Banned';
        end;
      end;
    139:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Server shutting down';
        end;
      end;
    140:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Bad authentication method';
        end;
      end;
    141:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Keep Alive timeout';
        end;
      end;
    142:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Session taken over';
        end;
      end;
    143:
      begin
        case aPacket of
          mtcpSUBACK, mtcpUNSUBACK, mtcpDISCONNECT:
            Result := 'Topic Filter invalid';
        end;
      end;
    144:
      begin
        case aPacket of
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpDISCONNECT:
            Result := 'Topic Name invalid';
        end;
      end;
    145:
      begin
        case aPacket of
          mtcpPUBACK, mtcpPUBREC, mtcpSUBACK, mtcpUNSUBACK:
            Result := 'Packet Identifier in use';
        end;
      end;
    146:
      begin
        case aPacket of
          mtcpPUBREL, mtcpPUBCOMP:
            Result := 'Packet Identifier not found';
        end;
      end;
    147:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Receive Maximum exceeded';
        end;
      end;
    148:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Topic Alias invalid';
        end;
      end;
    149:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Packet too large';
        end;
      end;
    150:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Message rate too high';
        end;
      end;
    151:
      begin
        case aPacket of
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpSUBACK, mtcpDISCONNECT:
            Result := 'Quota exceeded';
        end;
      end;
    152:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Administrative action';
        end;
      end;
    153:
      begin
        case aPacket of
          mtcpCONNACK, mtcpPUBACK, mtcpPUBREC, mtcpDISCONNECT:
            Result := 'Payload format invalid';
        end;
      end;
    154:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Retain not supported';
        end;
      end;
    155:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'QoS not supported';
        end;
      end;
    156:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Use another server';
        end;
      end;
    157:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Server moved';
        end;
      end;
    158:
      begin
        case aPacket of
          mtcpSUBACK, mtcpDISCONNECT:
            Result := 'Shared Subscriptions not supported';
        end;
      end;
    159:
      begin
        case aPacket of
          mtcpCONNACK, mtcpDISCONNECT:
            Result := 'Connection rate exceeded';
        end;
      end;
    160:
      begin
        case aPacket of
          mtcpDISCONNECT:
            Result := 'Maximum connect time';
        end;
      end;
    161:
      begin
        case aPacket of
          mtcpSUBACK, mtcpDISCONNECT:
            Result := 'Subscription Identifiers not supported';
        end;
      end;
    162:
      begin
        case aPacket of
          mtcpSUBACK, mtcpDISCONNECT:
            Result := 'Wildcard Subscriptions not supported';
        end;
      end;
  end;
end;

function GetReasonCodeMQTT311(aCode: Integer): String;
begin
  Result := '';
  case aCode of
    0:
      Result := 'Connection Accepted';
    1:
      Result := 'Connection Refused, unacceptable protocol version';
    2:
      Result := 'Connection Refused, identifier rejected';
    3:
      Result := 'Connection Refused, Server unavailable';
    4:
      Result := 'Connection Refused, bad user name or password';
    5:
      Result := 'Connection Refused, not authorized';
  end;
end;

function GetVarInteger(const aBytes: TIdBytes; var aOffset: Integer): Integer;
var
  vMultiplier: integer;
begin
  vMultiplier := 1;
  result := 0;

  aOffset := aOffset - 1;
  repeat
    aOffset := aOffset + 1;
    result := result + (aBytes[aOffset] and 127) * vMultiplier;
    if (vMultiplier > 128*128*128) then
      raise Exception.Create(S_MQTT_ERROR_MALFORMED_VARIABLE_BYTE_INTEGER);
    vMultiplier := vMultiplier * 128;
  until
    (aBytes[aOffset] and 128) = 0;
  aOffset := aOffset + 1;
end;

function GetVarIntegerAsBytes(aValue: Integer): TIdBytes;
var
  vByte: Byte;
begin
  SetLength(result, 0);

  repeat
    vByte := aValue mod 128;
    aValue := aValue div 128;
    if aValue > 0 then
      vByte := vByte or 128;
    SetLength(result, Length(result) + 1);
    result[Length(result) - 1] := vByte;
  until
    aValue = 0;
end;



constructor TsgcWSMQTTMessage.Create(aOwner: TComponent);
begin
  inherited;
  FMQTTVersion := mqtt311;
  CleanSession := True;
  FConnectProperties := TsgcWSMQTTConnect_Properties.Create;
  FWillProperties := TsgcWSMQTTWill_Properties.Create;
  FPUBLISHProperties := TsgcWSMQTTPublish_Properties.Create;
  FSubscribeProperties := TsgcWSMQTTSubscribe_Properties.Create;
  FUnsubscribeProperties := TsgcWSMQTTUnsubscribe_Properties.Create;
  FDISCONNECTProperties := TsgcWSMQTTDisconnect_Properties.Create;
end;

destructor TsgcWSMQTTMessage.Destroy;
begin
  sgcFree(FDISCONNECTProperties);
  sgcFree(FUnsubscribeProperties);
  sgcFree(FSubscribeProperties);
  sgcFree(FPUBLISHProperties);
  sgcFree(FWillProperties);
  sgcFree(FConnectProperties);
  FStreamRead := nil;
  ClearStreamReadTCP;
  inherited;
end;

procedure TsgcWSMQTTMessage.Clear;
begin
  sgcFree(FFixedHeader);
end;

procedure TsgcWSMQTTMessage.ClearStreamReadTCP;
begin
  sgcFree(FStreamReadPacketBuffer);
end;

function TsgcWSMQTTMessage.DoAuth(aReAuthenticate: Boolean): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpAUTH;

    // variable header
    VariableHeader.Clear;
    if MQTTVersion = mqtt5 then
    begin
      if aReAuthenticate then
        VariableHeader.AddByte($19)
      else
        VariableHeader.AddByte($18);
    end;

    // auth properties
    DoAuthProperties;
    // PayLoad
    PayLoadHeader.Clear;

    // DoDisconnect
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoAuthProperties;
var
  i: Integer;
  oProperties: TsgcWSMQTTAUTHProperties;
begin
  if MQTTVersion = mqtt5 then
  begin
    if AUTHProperties.Enabled then
    begin
      oProperties := TsgcWSMQTTAUTHProperties.Create(nil);
      Try
        // ... Authentication Method
        if AUTHProperties.AuthenticationMethod <> '' then
        begin
          oProperties.AddByte($15);
          oProperties.AddString(AUTHProperties.AuthenticationMethod);
        end;
        // ... Authentication Data
        if AUTHProperties.AuthenticationData <> '' then
        begin
          oProperties.AddByte($16);
          oProperties.AddString(AUTHProperties.AuthenticationData);
        end;
        // ... Reason String
        if AUTHProperties.ReasonString <> '' then
        begin
          oProperties.AddByte($1F);
          oProperties.AddString(AUTHProperties.ReasonString);
        end;
        // ... User Property
        for i := 0 to AUTHProperties.UserProperties.Count - 1 do
        begin
          if AUTHProperties.UserProperties.Names[i] <> '' then
          begin
            oProperties.AddByte($26);
            oProperties.AddString(AUTHProperties.UserProperties.Names[i], True);
            oProperties.AddString(AUTHProperties.UserProperties.ValueFromIndex
              [i], True);
          end;
        end;

        VariableHeader.AddByte(oProperties.Size);
        VariableHeader.AddBytes(oProperties.Bytes);
      Finally
        sgcFree(oProperties);
      End;
    end
    else
      VariableHeader.AddByte(0); // no properties, size = 0
  end;
end;

procedure TsgcWSMQTTMessage.DoReadCONNACK(const aBytes: TIdBytes);
var
  vLength: Integer;
  vOffset: Integer;
  vBytes: TIdBytes;
begin
  CONNACK.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength = 2 then
        begin
          CONNACK.Session := aBytes[0] = 1;
          CONNACK.ReasonCode := Integer(aBytes[1]);
          CONNACK.ReasonName := GetReasonCodeMQTT311(Integer(aBytes[1]));
        end;
      end;
    mqtt5:
      begin
        if FixedHeader.RemainingLength >= 2 then
        begin
          CONNACK.Session := aBytes[0] = 1;
          CONNACK.ReasonCode := Integer(aBytes[1]);
          CONNACK.ReasonName := GetReasonCode(mtcpCONNACK, Integer(aBytes[1]));
          // read properties
          vOffset := 2;
          vLength := GetVarInteger(aBytes, vOffset);
          SetLength(vBytes, vLength);
          sgcMove(aBytes[vOffset], vBytes[0], vLength);
          CONNACK.CONNACKProperties.ReadBytes(vBytes);
        end;
      end;
  end;

end;

procedure TsgcWSMQTTMessage.DoReadFixedHeader;
var
  vMultiplier: Integer;
  vBytes: TIdBytes;
begin
  // ... byte 0
  ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes), 1);
  FixedHeader.ControlPacket := TMQTTControlPacket(Ord(vBytes[0] shr 4));
  if FixedHeader.ControlPacket = mtcpPUBLISH then
  begin
    // ... duplicate
    FixedHeader.Duplicate := (vBytes[0] and 8) <> 0;
    // ... QoS
    if ((vBytes[0] and 4) <> 0) and ((vBytes[0] and 2) = 0) then
      FixedHeader.QoS := mtqsExactlyOnce
    else if ((vBytes[0] and 4) = 0) and ((vBytes[0] and 2) <> 0) then
      FixedHeader.QoS := mtqsAtLeastOnce
    else if ((vBytes[0] and 4) = 0) and ((vBytes[0] and 2) = 0) then
      FixedHeader.QoS := mtqsAtMostOnce
    else
      FixedHeader.QoS := mtqsReserved;
    // ... retain
    FixedHeader.Retain := (vBytes[0] and 1) <> 0;
  end;

  // ... reamaining length
  vMultiplier := 1;
  FixedHeader.RemainingLength := 0;
  repeat
    ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes), 1);
    FixedHeader.RemainingLength := FixedHeader.RemainingLength + (vBytes[0] and 127) * vMultiplier;
    if (vMultiplier > 128*128*128) then
      raise Exception.Create(S_MQTT_ERROR_MALFORMED_VARIABLE_BYTE_INTEGER);
    vMultiplier := vMultiplier * 128;
  until
    (vBytes[0] and 128) = 0;
end;

function TsgcWSMQTTMessage.GetByte(aBit7, aBit6, aBit5, aBit4, aBit3, aBit2,
  aBit1, aBit0: Boolean): Byte;
begin
  Result := ((Ord(aBit7) shl 7) + (Ord(aBit6) shl 6) + (Ord(aBit5) shl 5) +
    (Ord(aBit4) shl 4) + (Ord(aBit3) shl 3) + (Ord(aBit2) shl 2) +
    (Ord(aBit1) shl 1) + Ord(aBit0));
end;

function TsgcWSMQTTMessage.GetClientId: String;
begin
  if FClientId = '' then
    FClientId := LeftStr(NewGuid, 23);
  Result := FClientId;
end;

function TsgcWSMQTTMessage.GetCONNACK: TsgcWSMQTTCONNACK;
begin
  if not Assigned(FCONNACK) then
    FCONNACK := TsgcWSMQTTCONNACK.Create(self);
  Result := FCONNACK;
end;

function TsgcWSMQTTMessage.GetConnectFlags(aAuthentication: Boolean;
  aUserName, aPassword: String; aWillFlag: Boolean; aQoS: TmqttQoS;
  aRetain: Boolean): Byte;
var
  vUsername, vPassword, vWillRetain, vWillQoS1, vWillQoS0, vWillFlag,
    vReserved: Boolean;
begin
  vUsername := False;
  vPassword := False;
  if aAuthentication then
  begin
    vUsername := aUserName <> '';
    vPassword := aPassword <> '';
  end;
  vWillRetain := aRetain;
  GetQosBits(aQoS, vWillQoS1, vWillQoS0);
  vWillFlag := aWillFlag;
  vReserved := False;

  Result := GetByte(vUsername, vPassword, vWillRetain, vWillQoS1, vWillQoS0,
    vWillFlag, CleanSession, vReserved);
end;

function TsgcWSMQTTMessage.GetFixedHeader: TsgcWSMQTTFixedHeader;
begin
  if not Assigned(FFixedHeader) then
    FFixedHeader := TsgcWSMQTTFixedHeader.Create(self);
  Result := FFixedHeader;
end;

function TsgcWSMQTTMessage.GetMSBLSB(aValue: Integer): TIdBytes;
begin
  SetLength(Result, 2);
  Result[0] := aValue div $100;
  Result[1] := aValue mod $100;
end;

function TsgcWSMQTTMessage.GetPayLoadHeader: TsgcWSMQTTPayLoadHeader;
begin
  if not Assigned(FPayLoadHeader) then
    FPayLoadHeader := TsgcWSMQTTPayLoadHeader.Create(self);
  Result := FPayLoadHeader;
end;

function TsgcWSMQTTMessage.GetRemainingLength: TIdBytes;
var
  vLength: Integer;
begin
  if FixedHeader.RemainingLength = 2 then
    vLength := 2
  else
    vLength := Length(VariableHeader.Bytes) + Length(PayLoadHeader.Bytes);

  result := GetVarIntegerAsBytes(vLength);
end;

function TsgcWSMQTTMessage.GetVariableHeader: TsgcWSMQTTVariableHeader;
begin
  if not Assigned(FVariableHeader) then
    FVariableHeader := TsgcWSMQTTVariableHeader.Create(self);
  Result := FVariableHeader;
end;

procedure TsgcWSMQTTMessage.Read(const aStream: TStream);
var
  vBytes: TIdBytes;
begin
  DoEnterCS;
  Try
    StreamRead := aStream;

    DoReadFixedHeader;

    if FixedHeader.RemainingLength = 0 then
      exit;

    case FixedHeader.ControlPacket of
      mtcpReserved0:
        ;
      mtcpCONNECT:
        ;
      mtcpCONNACK:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadCONNACK(vBytes);
        end;
      mtcpPUBLISH:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadPUBLISH(vBytes);
        end;
      mtcpPUBACK:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadPUBACK(vBytes);
        end;
      mtcpPUBREC:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadPUBREC(vBytes);
        end;
      mtcpPUBREL:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadPUBREL(vBytes);
        end;
      mtcpPUBCOMP:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadPUBCOMP(vBytes);
        end;
      mtcpSUBSCRIBE:
        ;
      mtcpSUBACK:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadSUBACK(vBytes);
        end;
      mtcpUNSUBSCRIBE:
        ;
      mtcpUNSUBACK:
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadUNSUBACK(vBytes);
        end;
      mtcpPINGREQ:
        ;
      mtcpPINGRESP:
        ;
      mtcpDISCONNECT:
        if MQTTVersion = mqtt5 then
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadDISCONNECT(vBytes);
        end;
      mtcpAUTH:
        if MQTTVersion = mqtt5 then
        begin
          ReadTIdBytesFromStream(StreamRead, TIdBytes(vBytes),
            FixedHeader.RemainingLength);
          DoReadAUTH(vBytes);
        end;
    end;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcWSMQTTMessage.DoConnect(aAuthentication: Boolean; aAuthUser,
    aAuthPass: String; aWillFlag: Boolean; aWillTopic, aWillText: String;
    aWillQoS: TmqttQoS; aWillRetain, aKeepAliveEnabled: Boolean;
    aKeepAliveInterval: Integer): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpCONNECT;

    // variable header
    VariableHeader.Clear;
    // ... protocol name
    VariableHeader.AddString('MQTT');
    // ... protocol level
    if MQTTVersion = mqtt5 then
      VariableHeader.AddByte(5)
    else
      VariableHeader.AddByte(4);
    // ... connect flags
    if not aWillFlag then
      aWillRetain := False;
    // if the Will Flag is set to 0, then Will Retain MUST be set to 0
    VariableHeader.AddByte(GetConnectFlags(aAuthentication, aAuthUser, aAuthPass,
      aWillFlag, aWillQoS, aWillRetain));
    // ... keep alive
    if aKeepAliveEnabled then
      VariableHeader.AddBytes(GetMSBLSB(aKeepAliveInterval))
    else
      VariableHeader.AddBytes(GetMSBLSB(0));
    // variable headers mqtt5
    DoConnectProperties;

    // PayLoad
    PayLoadHeader.Clear;
    PayLoadHeader.AddString(ClientId);
    // will properties
    WillProperties.Enabled := aWillFlag;
    // if WillFlag = True, will properties must be sent
    DoWillProperties;
    // willdata
    if aWillFlag then
    begin
      PayLoadHeader.AddString(aWillTopic);
      PayLoadHeader.AddString(aWillText);
    end;
    if aAuthentication then
    begin
      PayLoadHeader.AddString(aAuthUser);
      PayLoadHeader.AddString(aAuthPass);
    end;

    // DoConnect
    result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoConnectProperties;
var
  i: Integer;
  oProperties: TsgcWSMQTTConnectProperties;
begin
  if (MQTTVersion = mqtt5) then
  begin
    if ConnectProperties.Enabled then
    begin
      oProperties := TsgcWSMQTTConnectProperties.Create(nil);
      Try
        // ... Session Expiry Interval
        oProperties.AddByte($11);
        oProperties.AddInteger(ConnectProperties.SessionExpiryInterval);
        // ... Receive Maximum
        oProperties.AddByte($21);
        oProperties.AddWord(ConnectProperties.ReceiveMaximum);
        // ... Maximum Packet Size
        oProperties.AddByte($27);
        oProperties.AddInteger(ConnectProperties.MaximumPacketSize);
        // ... Topic Alias Maximum
        oProperties.AddByte($22);
        oProperties.AddWord(ConnectProperties.TopicAliasMaximum);
        // ... Request Response Information
        oProperties.AddByte($19);
        if ConnectProperties.RequestResponseInformation then
          oProperties.AddByte(1)
        else
          oProperties.AddByte(0);
        // ... Request Problem Information
        oProperties.AddByte($17);
        oProperties.AddBoolean(ConnectProperties.RequestProblemInformation);
        // ... User Property
        for i := 0 to ConnectProperties.UserProperties.Count - 1 do
        begin
          if ConnectProperties.UserProperties.Names[i] <> '' then
          begin
            oProperties.AddByte($26);
            oProperties.AddString(ConnectProperties.UserProperties.Names
              [i], True);
            oProperties.AddString(ConnectProperties.UserProperties.ValueFromIndex
              [i], True);
          end;
        end;
        // ... Authentication Method
        if ConnectProperties.AuthenticationMethod <> '' then
        begin
          oProperties.AddByte($15);
          oProperties.AddString(ConnectProperties.AuthenticationMethod, False);
        end;
        VariableHeader.AddByte(oProperties.Size);
        VariableHeader.AddBytes(oProperties.Bytes);
      Finally
        sgcFree(oProperties);
      End;
    end
    else
      VariableHeader.AddByte(0); // no properties, size = 0
  end
end;

function TsgcWSMQTTMessage.DoDisconnect(aReasonCode: Integer = 0): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpDISCONNECT;

    // variable header
    VariableHeader.Clear;
    if MQTTVersion = mqtt5 then
      VariableHeader.AddByte(aReasonCode);
    // disconnect properties
    DoDisconnectProperties;
    // PayLoad
    PayLoadHeader.Clear;

    // DoDisconnect
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoDisconnectProperties;
var
  i: Integer;
  oProperties: TsgcWSMQTTDISCONNECTProperties;
begin
  if MQTTVersion = mqtt5 then
  begin
    if DISCONNECTProperties.Enabled then
    begin
      oProperties := TsgcWSMQTTDISCONNECTProperties.Create(nil);
      Try
        // ... Session Expiry Interval
        oProperties.AddByte($11);
        oProperties.AddInteger(DISCONNECTProperties.SessionExpiryInterval);
        // ... Reason String
        if DISCONNECTProperties.ReasonString <> '' then
        begin
          oProperties.AddByte($1F);
          oProperties.AddString(DISCONNECTProperties.ReasonString);
        end;
        // ... User Property
        for i := 0 to DISCONNECTProperties.UserProperties.Count - 1 do
        begin
          if DISCONNECTProperties.UserProperties.Names[i] <> '' then
          begin
            oProperties.AddByte($26);
            oProperties.AddString(DISCONNECTProperties.UserProperties.Names
              [i], True);
            oProperties.AddString
              (DISCONNECTProperties.UserProperties.ValueFromIndex[i], True);
          end;
        end;
        // ... Server Reference
        if DISCONNECTProperties.ServerReference <> '' then
        begin
          oProperties.AddByte($22);
          oProperties.AddString(DISCONNECTProperties.ServerReference);
        end;

        VariableHeader.AddByte(oProperties.Size);
        VariableHeader.AddBytes(oProperties.Bytes);
      Finally
        sgcFree(oProperties);
      End;
    end
    else
      VariableHeader.AddByte(0); // no properties, size = 0
  end;
end;

function TsgcWSMQTTMessage.DoPing: TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpPINGREQ;

    // variable header
    VariableHeader.Clear;
    // PayLoad
    PayLoadHeader.Clear;

    // DoPing
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcWSMQTTMessage.DoPubAck(aPacketIdentifier: Word): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpPUBACK;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // PayLoad
    PayLoadHeader.Clear;

    // DoPubAck
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcWSMQTTMessage.DoPublish(aPacketIdentifier: Word; const aTopic,
    aText: String; aQoS: TmqttQoS; aRetain, aDuplicate: Boolean): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.Duplicate := False;
    FixedHeader.QoS := aQoS;
    FixedHeader.Retain := aRetain;
    FixedHeader.ControlPacket := mtcpPUBLISH;
    FixedHeader.Duplicate := aDuplicate;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddString(aTopic);
    if ((aQoS = mtqsAtLeastOnce) or (aQoS = mtqsExactlyOnce)) then
      VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // publish properties
    DoPublishProperties;

    // PayLoad
    PayLoadHeader.Clear;
    PayLoadHeader.AddString(aText, False);

    // DoSubscribe
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcWSMQTTMessage.DoPublish(aPacketIdentifier: Word; const aTopic:
    String; aStream: TStream; aQoS: TmqttQoS; aRetain, aDuplicate: Boolean):
    TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.Duplicate := False;
    FixedHeader.QoS := aQoS;
    FixedHeader.Retain := aRetain;
    FixedHeader.ControlPacket := mtcpPUBLISH;
    FixedHeader.Duplicate := aDuplicate;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddString(aTopic);
    if ((aQoS = mtqsAtLeastOnce) or (aQoS = mtqsExactlyOnce)) then
      VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // publish properties
    DoPublishProperties;

    // PayLoad
    PayLoadHeader.Clear;
    PayLoadHeader.AddStream(aStream);

    // DoSubscribe
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoPublishProperties;
var
  i: Integer;
  oPublish: TsgcWSMQTTPublishProperties;
begin
  if MQTTVersion = mqtt5 then
  begin
    if PUBLISHProperties.Enabled then
    begin
      oPublish := TsgcWSMQTTPublishProperties.Create(nil);
      Try
        // ... Payload Format Indicator
        oPublish.AddByte($01);
        oPublish.AddBoolean(PUBLISHProperties.PayloadFormat = mqpfUTF8);
        // ... Message Expiry Interval
        if PUBLISHProperties.MessageExpiryInterval > 0 then
        begin
          oPublish.AddByte($02);
          oPublish.AddInteger(PUBLISHProperties.MessageExpiryInterval);
        end;
        // ... Topic Alias
        if PUBLISHProperties.TopicAlias > 0 then
        begin
          oPublish.AddByte($23);
          oPublish.AddWord(PUBLISHProperties.TopicAlias);
        end;
        // ... Response Topic
        if PUBLISHProperties.ResponseTopic <> '' then
        begin
          oPublish.AddByte($08);
          oPublish.AddString(PUBLISHProperties.ResponseTopic);
        end;
        // ... Correlation Data
        if PUBLISHProperties.CorrelationData <> '' then
        begin
          oPublish.AddByte($09);
          oPublish.AddString(PUBLISHProperties.CorrelationData);
        end;
        // ...  User Property
        for i := 0 to PUBLISHProperties.UserProperties.Count - 1 do
        begin
          if PUBLISHProperties.UserProperties.Names[i] <> '' then
          begin
            oPublish.AddByte($26);
            oPublish.AddString(PUBLISHProperties.UserProperties.Names[i], True);
            oPublish.AddString(PUBLISHProperties.UserProperties.ValueFromIndex
              [i], True);
          end;
        end;
        // ... Subscription Identifier
        if PUBLISHProperties.SubscriptionIdentifier > 0 then
        begin
          oPublish.AddByte($11);
          oPublish.AddVarInteger(PUBLISHProperties.SubscriptionIdentifier);
        end;
        // ... Content Type
        if PUBLISHProperties.ContentType <> '' then
        begin
          oPublish.AddByte($03);
          oPublish.AddString(PUBLISHProperties.ContentType);
        end;

        VariableHeader.AddBytes(GetVarIntegerAsBytes(oPublish.Size));
        VariableHeader.AddBytes(oPublish.Bytes);

        PUBLISHProperties.Enabled := False; // disable for next message
      Finally
        sgcFree(oPublish);
      End;
    end
    else
      VariableHeader.AddByte(0); // no properties, size = 0
  end;
end;

function TsgcWSMQTTMessage.DoPubRec(aPacketIdentifier: Word): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpPUBREC;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // PayLoad
    PayLoadHeader.Clear;

    // DoPubRec
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcWSMQTTMessage.DoPubRel(aPacketIdentifier: Word): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpPUBREL;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // PayLoad
    PayLoadHeader.Clear;

    // DoPubRel
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcWSMQTTMessage.DoPubComp(aPacketIdentifier: Word): TBytes;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpPUBCOMP;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // PayLoad
    PayLoadHeader.Clear;

    // DoPubComp
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoReadAUTH(const aBytes: TIdBytes);
var
  vBytes: TIdBytes;
begin
  AUTH.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt5:
      begin
        if Length(aBytes) > 0 then
        begin
          AUTH.ReasonCode := aBytes[0];
          AUTH.ReasonName := GetReasonCode(mtcpAUTH, aBytes[0]);
          if Length(aBytes) > 1 then
          begin
            if aBytes[1] > 0 then
            begin
              // read properties
              SetLength(vBytes, aBytes[1]);
              sgcMove(aBytes[2], vBytes[0], Length(vBytes));
              AUTH.AUTHProperties.ReadBytes(vBytes);
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSMQTTMessage.DoReadDISCONNECT(const aBytes: TIdBytes);
var
  vBytes: TIdBytes;
begin
  DISCONNECT.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt5:
      begin
        if Length(aBytes) > 0 then
        begin
          DISCONNECT.ReasonCode := aBytes[0];
          DISCONNECT.ReasonName := GetReasonCode(mtcpDISCONNECT, aBytes[0]);
          if Length(aBytes) > 1 then
          begin
            if aBytes[1] > 0 then
            begin
              // read properties
              SetLength(vBytes, aBytes[1]);
              sgcMove(aBytes[2], vBytes[0], Length(vBytes));
              DISCONNECT.DISCONNECTProperties.ReadBytes(vBytes);
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSMQTTMessage.DoReadPUBACK(const aBytes: TIdBytes);
var
  vBytes: TIdBytes;
begin
  PUBACK.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength = 2 then
          PUBACK.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
      end;
    mqtt5:
      begin
        if Length(aBytes) > 1 then
        begin
          PUBACK.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
          if Length(aBytes) > 2 then
          begin
            PUBACK.ReasonCode := aBytes[2];
            PUBACK.ReasonName := GetReasonCode(mtcpPUBACK, aBytes[2]);
            if Length(aBytes) > 3 then
            begin
              if aBytes[3] > 0 then
              begin
                // read properties
                SetLength(vBytes, aBytes[3]);
                sgcMove(aBytes[4], vBytes[0], Length(vBytes));
                PUBACK.PUBACKProperties.ReadBytes(vBytes);
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSMQTTMessage.DoReadPUBCOMP(const aBytes: TIdBytes);
var
  vBytes: TIdBytes;
begin
  PUBCOMP.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength = 2 then
          PUBCOMP.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
      end;
    mqtt5:
      begin
        if Length(aBytes) > 1 then
        begin
          PUBCOMP.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
          if Length(aBytes) > 2 then
          begin
            PUBCOMP.ReasonCode := aBytes[2];
            PUBCOMP.ReasonName := GetReasonCode(mtcpPUBCOMP, aBytes[2]);
            if Length(aBytes) > 3 then
            begin
              // read properties
              SetLength(vBytes, aBytes[3]);
              sgcMove(aBytes[4], vBytes[0], Length(vBytes));
              PUBCOMP.PUBCOMPProperties.ReadBytes(vBytes);
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSMQTTMessage.DoReadPUBLISH(const aBytes: TIdBytes);
var
  vIndex: Integer;
  vBytes: TIdBytes;
  vLength: integer;
begin
  PUBLISH.Clear;

  if Length(aBytes) = 0 then
    exit;

  PUBLISH.Topic := ReadString(aBytes);

  PUBLISH.PacketIdentifier := 0;
  // there is no qos
  if FixedHeader.QoS = mtqsAtMostOnce then
    vIndex := ReadMSBLSB(aBytes[0], aBytes[1]) + 2
  else
  // 2 bytes qos
  begin
    vIndex := ReadMSBLSB(aBytes[0], aBytes[1]) + 4;
    PUBLISH.PacketIdentifier := ReadMSBLSB(aBytes[vIndex - 2],
      aBytes[vIndex - 1]);
  end;

  // read properties
  if MQTTVersion = mqtt5 then
  begin
    vLength := GetVarInteger(aBytes, vIndex);
    if vLength > 0 then
    begin
      SetLength(vBytes, vLength);
      sgcMove(aBytes[vIndex], vBytes[0], Length(vBytes));
      PUBLISH.PUBLISHProperties.ReadBytes(vBytes);
      vIndex := vIndex + vLength;
    end;
  end;

  PUBLISH.Text := ReadString(aBytes, False, vIndex);
end;

procedure TsgcWSMQTTMessage.DoReadPUBREC(const aBytes: TIdBytes);
var
  vBytes: TIdBytes;
begin
  PUBREC.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength = 2 then
          PUBREC.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
      end;
    mqtt5:
      begin
        if Length(aBytes) > 1 then
        begin
          PUBREC.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
          if Length(aBytes) > 2 then
          begin
            PUBREC.ReasonCode := aBytes[2];
            PUBREC.ReasonName := GetReasonCode(mtcpPUBREC, aBytes[2]);
            if Length(aBytes) > 3 then
            begin
              // read properties
              SetLength(vBytes, aBytes[3]);
              sgcMove(aBytes[4], vBytes[0], Length(vBytes));
              PUBREC.PUBRECProperties.ReadBytes(vBytes);
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSMQTTMessage.DoReadPUBREL(const aBytes: TIdBytes);
var
  vBytes: TIdBytes;
begin
  PUBREL.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength = 2 then
          PUBREL.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
      end;
    mqtt5:
      begin
        if Length(aBytes) > 1 then
        begin
          PUBREL.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
          if Length(aBytes) > 2 then
          begin
            PUBREL.ReasonCode := aBytes[2];
            PUBREL.ReasonName := GetReasonCode(mtcpPUBREL, aBytes[2]);
            if Length(aBytes) > 3 then
            begin
              // read properties
              SetLength(vBytes, aBytes[3]);
              sgcMove(aBytes[4], vBytes[0], Length(vBytes));
              PUBREL.PUBRELProperties.ReadBytes(vBytes);
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSMQTTMessage.DoReadSUBACK(const aBytes: TIdBytes);
var
  i, j: Integer;
  oSUBACK: TsgcWSSUBACK;
  vBytes: TIdBytes;
begin
  SUBACK.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength >= 2 then
        begin
          SUBACK.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
          j := 2;
          for i := j to Length(aBytes) - 1 do
          begin
            oSUBACK := TsgcWSSUBACK.Create;
            oSUBACK.ReasonCode := aBytes[i];
            oSUBACK.ReasonName := GetReasonCode(mtcpSUBACK, oSUBACK.ReasonCode);
            SUBACK.Codes.Add(oSUBACK);
          end;
        end;
      end;
    mqtt5:
      begin
        if FixedHeader.RemainingLength > 2 then
        begin
          SUBACK.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
          j := 3;
          if aBytes[2] > 0 then
          begin
            // read properties
            SetLength(vBytes, aBytes[2]);
            sgcMove(aBytes[3], vBytes[0], Length(vBytes));
            SUBACK.SUBACKProperties.ReadBytes(vBytes);
            j := j + aBytes[2];
          end;

          for i := j to Length(aBytes) - 1 do
          begin
            oSUBACK := TsgcWSSUBACK.Create;
            oSUBACK.ReasonCode := aBytes[i];
            oSUBACK.ReasonName := GetReasonCode(mtcpSUBACK, oSUBACK.ReasonCode);
            SUBACK.Codes.Add(oSUBACK);
          end;
        end;
      end;
  end;

end;

procedure TsgcWSMQTTMessage.DoReadUNSUBACK(const aBytes: TIdBytes);
var
  i, j: Integer;
  vBytes: TIdBytes;
  oUNSUBACK: TsgcWSUNSUBACK;
begin
  UNSUBACK.Clear;

  if Length(aBytes) = 0 then
    exit;

  case MQTTVersion of
    mqtt311:
      begin
        if FixedHeader.RemainingLength = 2 then
          UNSUBACK.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
      end;
    mqtt5:
      if FixedHeader.RemainingLength > 2 then
      begin
        UNSUBACK.PacketIdentifier := ReadMSBLSB(aBytes[0], aBytes[1]);
        j := 3;
        if aBytes[2] > 0 then
        begin
          // read properties
          SetLength(vBytes, aBytes[2]);
          sgcMove(aBytes[3], vBytes[0], Length(vBytes));
          UNSUBACK.UNSUBACKProperties.ReadBytes(vBytes);
          j := j + aBytes[2];
        end;

        for i := j to Length(aBytes) - 1 do
        begin
          oUNSUBACK := TsgcWSUNSUBACK.Create;
          oUNSUBACK.ReasonCode := aBytes[i];
          oUNSUBACK.ReasonName := GetReasonCode(mtcpUNSUBACK,
            oUNSUBACK.ReasonCode);
          UNSUBACK.Codes.Add(oUNSUBACK);
        end;
      end;
  end;
end;

function TsgcWSMQTTMessage.DoSubscribe(aPacketIdentifier: Word; aTopics:
    TsgcWSTopics): TBytes;
var
  i: Integer;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpSUBSCRIBE;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // subscribe properties
    DoSubscribeProperties;

    // PayLoad
    PayLoadHeader.Clear;
    for i := 0 to aTopics.Count - 1 do
    begin
      PayLoadHeader.AddString(TsgcWSTopic(aTopics[i]).Topic);
      PayLoadHeader.AddByte(Ord(TsgcWSTopic(aTopics[i]).QoS));
    end;

    // DoSubscribe
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoSubscribeProperties;
var
  i: Integer;
  oSubscribe: TsgcWSMQTTSubscribeProperties;
begin
  if MQTTVersion = mqtt5 then
  begin
    if SubscribeProperties.Enabled then
    begin
      oSubscribe := TsgcWSMQTTSubscribeProperties.Create(nil);
      Try
        // ... Subscription Identifier
        oSubscribe.AddByte($0B);
        oSubscribe.AddVarInteger(SubscribeProperties.SubscriptionIdentifier);
        // ...  User Property
        for i := 0 to SubscribeProperties.UserProperties.Count - 1 do
        begin
          if SubscribeProperties.UserProperties.Names[i] <> '' then
          begin
            oSubscribe.AddByte($26);
            oSubscribe.AddString(SubscribeProperties.UserProperties.Names
              [i], True);
            oSubscribe.AddString
              (SubscribeProperties.UserProperties.ValueFromIndex[i], True);
          end;
        end;

        VariableHeader.AddByte(oSubscribe.Size);
        VariableHeader.AddBytes(oSubscribe.Bytes);

        SubscribeProperties.Enabled := False; // disable for next message
      Finally
        sgcFree(oSubscribe);
      End;
    end
    else
      VariableHeader.AddByte(0); // no properties, size = 0
  end;
end;

procedure TsgcWSMQTTMessage.DoUnsubscribeProperties;
var
  i: Integer;
  oUnsubscribe: TsgcWSMQTTUnSubscribeProperties;
begin
  if MQTTVersion = mqtt5 then
  begin
    if UnsubscribeProperties.Enabled then
    begin
      oUnsubscribe := TsgcWSMQTTUnSubscribeProperties.Create(nil);
      Try
        // ...  User Property
        for i := 0 to UnsubscribeProperties.UserProperties.Count - 1 do
        begin
          if UnsubscribeProperties.UserProperties.Names[i] <> '' then
          begin
            oUnsubscribe.AddByte($26);
            oUnsubscribe.AddString(UnsubscribeProperties.UserProperties.Names
              [i], True);
            oUnsubscribe.AddString
              (UnsubscribeProperties.UserProperties.ValueFromIndex[i], True);
          end;
        end;

        VariableHeader.AddByte(oUnsubscribe.Size);
        VariableHeader.AddBytes(oUnsubscribe.Bytes);

        UnsubscribeProperties.Enabled := False; // disable for next message
      Finally
        sgcFree(oUnsubscribe);
      End;
    end
    else
      VariableHeader.AddByte(0); // no properties, size = 0
  end;
end;

function TsgcWSMQTTMessage.DoUnSubscribe(aPacketIdentifier: Word; aTopics:
    TsgcWSTopics): TBytes;
var
  i: Integer;
begin
  DoEnterCS;
  Try
    // Control packet
    FixedHeader.ControlPacket := mtcpUNSUBSCRIBE;

    // variable header
    VariableHeader.Clear;
    VariableHeader.AddBytes(GetMSBLSB(aPacketIdentifier));
    // subscribe properties
    DoUnsubscribeProperties;

    // PayLoad
    PayLoadHeader.Clear;
    for i := 0 to aTopics.Count - 1 do
      PayLoadHeader.AddString(TsgcWSTopic(aTopics[i]).Topic);

    // DoUnSubscribe
    Result := WriteBytes;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSMQTTMessage.DoWillProperties;
var
  i: Integer;
  oProperties: TsgcWSMQTTWillProperties;
begin
  if (MQTTVersion = mqtt5) and WillProperties.Enabled then
  begin
    oProperties := TsgcWSMQTTWillProperties.Create(nil);
    Try
      // ... Will Delay Interval
      oProperties.AddByte($18);
      oProperties.AddInteger(WillProperties.WillDelayInterval);
      // ... Payload Format Indicator
      oProperties.AddByte($01);
      oProperties.AddBoolean(WillProperties.PayloadFormat = mqpfUTF8);
      // ... Message Expiry Interval
      oProperties.AddByte($02);
      oProperties.AddInteger(WillProperties.MessageExpiryInterval);
      // ... Content Type
      if WillProperties.ContentType <> '' then
      begin
        oProperties.AddByte($03);
        oProperties.AddString(WillProperties.ContentType);
      end;
      // ... Response Topic
      if WillProperties.ResponseTopic <> '' then
      begin
        oProperties.AddByte($08);
        oProperties.AddString(WillProperties.ResponseTopic);
      end;
      // ... Correlation Data
      if WillProperties.CorrelationData <> '' then
      begin
        oProperties.AddByte($09);
        oProperties.AddString(WillProperties.CorrelationData);
      end;
      // ...  User Property
      for i := 0 to WillProperties.UserProperties.Count - 1 do
      begin
        if WillProperties.UserProperties.Names[i] <> '' then
        begin
          oProperties.AddByte($26);
          oProperties.AddString(WillProperties.UserProperties.Names[i], True);
          oProperties.AddString(WillProperties.UserProperties.ValueFromIndex
            [i], True);
        end;
      end;

      PayLoadHeader.AddByte(oProperties.Size);
      PayLoadHeader.AddBytes(oProperties.Bytes);
    Finally
      sgcFree(oProperties);
    End;
  end;
  // PayLoadHeader.AddByte(0); if WillFlag = False, properties must not be sent
end;

function TsgcWSMQTTMessage.GetDISCONNECT: TsgcWSMQTTDISCONNECT;
begin
  if not Assigned(FDISCONNECT) then
    FDISCONNECT := TsgcWSMQTTDISCONNECT.Create(self);
  Result := FDISCONNECT;

end;

function TsgcWSMQTTMessage.GetAUTH: TsgcWSMQTTAUTH;
begin
  if not Assigned(FAUTH) then
    FAUTH := TsgcWSMQTTAUTH.Create(self);
  Result := FAUTH;

end;

function TsgcWSMQTTMessage.GetPUBACK: TsgcWSMQTTPUBACK;
begin
  if not Assigned(FPUBACK) then
    FPUBACK := TsgcWSMQTTPUBACK.Create(self);
  Result := FPUBACK;
end;

function TsgcWSMQTTMessage.GetPUBCOMP: TsgcWSMQTTPUBCOMP;
begin
  if not Assigned(FPUBCOMP) then
    FPUBCOMP := TsgcWSMQTTPUBCOMP.Create(self);
  Result := FPUBCOMP;
end;

function TsgcWSMQTTMessage.GetPUBLISH: TsgcWSMQTTPUBLISH;
begin
  if not Assigned(FPUBLISH) then
    FPUBLISH := TsgcWSMQTTPUBLISH.Create(self);
  Result := FPUBLISH;
end;

function TsgcWSMQTTMessage.GetPUBREC: TsgcWSMQTTPUBREC;
begin
  if not Assigned(FPUBREC) then
    FPUBREC := TsgcWSMQTTPUBREC.Create(self);
  Result := FPUBREC;
end;

function TsgcWSMQTTMessage.GetPUBREL: TsgcWSMQTTPUBREL;
begin
  if not Assigned(FPUBREL) then
    FPUBREL := TsgcWSMQTTPUBREL.Create(self);
  Result := FPUBREL;
end;

function TsgcWSMQTTMessage.GetStreamReadPacketBuffer:
    TsgcWSMQTTReadPacketBuffer;
begin
  if not Assigned(FStreamReadPacketBuffer) then
    FStreamReadPacketBuffer := TsgcWSMQTTReadPacketBuffer.Create(nil);
  Result := FStreamReadPacketBuffer;
end;

function TsgcWSMQTTMessage.GetSUBACK: TsgcWSMQTTSUBACK;
begin
  if not Assigned(FSUBACK) then
    FSUBACK := TsgcWSMQTTSUBACK.Create(self);
  Result := FSUBACK;
end;

function TsgcWSMQTTMessage.GetUNSUBACK: TsgcWSMQTTUNSUBACK;
begin
  if not Assigned(FUNSUBACK) then
    FUNSUBACK := TsgcWSMQTTUNSUBACK.Create(self);
  Result := FUNSUBACK;
end;

function TsgcWSMQTTMessage.ReadTCP(const aStream: TStream): Boolean;
begin
  result := False;

  if StreamReadPacketBuffer.Read(aStream) then
  begin
    Read(StreamReadPacketBuffer.StreamBuffer);
    ClearStreamReadTCP;
    result := True;
  end;
end;

procedure TsgcWSMQTTMessage.SetConnectProperties(const Value
  : TsgcWSMQTTConnect_Properties);
begin
  FConnectProperties.Assign(Value);
end;

procedure TsgcWSMQTTMessage.SetPublishProperties(const Value
  : TsgcWSMQTTPublish_Properties);
begin
  FPUBLISHProperties.Assign(Value);
end;

procedure TsgcWSMQTTMessage.SetSubscribeProperties
  (const Value: TsgcWSMQTTSubscribe_Properties);
begin
  FSubscribeProperties.Assign(Value);
end;

procedure TsgcWSMQTTMessage.SetUnsubscribeProperties
  (const Value: TsgcWSMQTTUnsubscribe_Properties);
begin
  FUnsubscribeProperties.Assign(Value);
end;

procedure TsgcWSMQTTMessage.SetDisconnectProperties
  (const Value: TsgcWSMQTTDisconnect_Properties);
begin
  FDISCONNECTProperties.Assign(Value);
end;

procedure TsgcWSMQTTMessage.SetAuthProperties(const Value
  : TsgcWSMQTTAuth_Properties);
begin
  FAUTHProperties.Assign(Value);
end;

procedure TsgcWSMQTTMessage.SetWillProperties(const Value
  : TsgcWSMQTTWill_Properties);
begin
  FWillProperties.Assign(Value);
end;

function TsgcWSMQTTMessage.WriteBytes: TBytes;
var
  vBytes: TIdBytes;
begin
  // ... fixed header
  vBytes := FixedHeader.Bytes;
  sgcAddBytes(TIdBytes(vBytes), TIdBytes(result));
  // ... remaining length
  vBytes := GetRemainingLength;
  sgcAddBytes(TIdBytes(vBytes), TIdBytes(result));
  // ... variable header
  sgcAddBytes(TIdBytes(VariableHeader.Bytes), TIdBytes(result));
  // ... payload header
  sgcAddBytes(TIdBytes(PayLoadHeader.Bytes), TIdBytes(result));
end;

constructor TsgcWSMQTTFixedHeader.Create(aOwner: TComponent);
begin
  inherited;
  ControlPacket := mtcpReserved0;
  Duplicate := False;
  QoS := mtqsAtMostOnce;
  Retain := False;
end;

function TsgcWSMQTTFixedHeader.GetBytes: TIdBytes;
var
  vBit3, vBit2, vBit1, vBit0: Boolean;
begin
  RemainingLength := 0;

  vBit3 := False;
  vBit2 := False;
  vBit1 := False;
  vBit0 := False;

  case ControlPacket of
    mtcpCONNACK:
      RemainingLength := 2;
    mtcpPUBLISH:
      begin
        if Duplicate then
          vBit3 := True;
        GetQosBits(QoS, vBit2, vBit1);
        if Retain then
          vBit0 := True;
      end;
    mtcpPUBACK:
      RemainingLength := 2;
    mtcpPUBREC:
      RemainingLength := 2;
    mtcpPUBREL:
      begin
        RemainingLength := 2;
        vBit1 := True;
      end;
    mtcpPUBCOMP:
      RemainingLength := 2;
    mtcpSUBSCRIBE:
      vBit1 := True;
    mtcpUNSUBSCRIBE:
      vBit1 := True;
    mtcpUNSUBACK:
      RemainingLength := 2;
  end;

  SetLength(Result, 1);
  Result[0] := ((Ord(ControlPacket) shl 4) + (Ord(vBit3) shl 3) +
    (Ord(vBit2) shl 2) + (Ord(vBit1) shl 1) + Ord(vBit0));
end;

function TsgcWSMQTTFixedHeader.GetSize: Integer;
begin
  Result := Length(Bytes);
end;

procedure TsgcWSMQTTVariableHeader.AddBoolean(aValue: Boolean);
begin
  if aValue then
    AddByte(1)
  else
    AddByte(0);
end;

procedure TsgcWSMQTTVariableHeader.AddByte(aByte: Byte);
var
  i: Integer;
begin
  i := Length(FBytes);
  SetLength(FBytes, i + SizeOf(aByte));
  sgcMove(aByte, FBytes[i], SizeOf(aByte));
end;

procedure TsgcWSMQTTVariableHeader.AddBytes(aBytes: TIdBytes);
var
  i, j: Integer;
begin
  j := Length(aBytes);
  if j > 0 then
  begin
    i := Length(FBytes);
    SetLength(FBytes, i + j);
    sgcMove(aBytes[0], FBytes[i], j);
  end;
end;

procedure TsgcWSMQTTVariableHeader.AddInteger(aValue: Integer);
var
  i: Integer;
  vBytes: TIdBytes;
begin
  vBytes := ToBytes(aValue);
  for i := Length(vBytes) - 1 Downto 0 do
    AddByte(vBytes[i]);
end;

procedure TsgcWSMQTTVariableHeader.AddWord(aValue: Word);
var
  i: Integer;
  vBytes: TIdBytes;
begin
  vBytes := ToBytes(aValue);
  for i := Length(vBytes) - 1 Downto 0 do
    AddByte(vBytes[i]);
end;

procedure TsgcWSMQTTVariableHeader.Clear;
begin
  SetLength(FBytes, 0);
end;

procedure TsgcWSMQTTVariableHeader.AddString(const aText: String;
  aHeader: Boolean = True);
var
  vBytes: TIdBytes;
begin
{$IFDEF D2009}
  vBytes := TIdBytes(TEncoding.UTF8.GetBytes(aText));
{$ELSE}
  vBytes := sgcStringToBytes(UTF8Encode(aText));
{$ENDIF}
  if aHeader then
  begin
    AddByte(Length(vBytes) div $100);
    AddByte(Length(vBytes) mod $100);
  end;
  AddBytes(vBytes);
end;

procedure TsgcWSMQTTVariableHeader.AddVarInteger(aValue: Integer);
var
  vByte: Byte;
begin
  repeat
    vByte := aValue mod 128;
    aValue := aValue div 128;
    if aValue > 0 then
      vByte := vByte or 128;
    AddByte(vByte);
  until
    aValue = 0;
end;

function TsgcWSMQTTVariableHeader.GetSize: Integer;
begin
  Result := Length(Bytes);
end;

destructor TsgcWSMQTTSUBACK.Destroy;
begin
  sgcFree(FSUBACKProperties);
  sgcFree(FCodes);
  inherited;
end;

procedure TsgcWSMQTTSUBACK.Clear;
begin
  inherited;
  Codes.Clear;
  SUBACKProperties.Clear;
end;

function TsgcWSMQTTSUBACK.GetCodes: TsgcWSSUBACKS;
begin
  if not Assigned(FCodes) then
    FCodes := TsgcWSSUBACKS.Create;
  Result := FCodes;
end;

function TsgcWSMQTTSUBACK.GetSUBACKProperties: TsgcWSMQTTSUBACKProperties;
begin
  if not Assigned(FSUBACKProperties) then
    FSUBACKProperties := TsgcWSMQTTSUBACKProperties.Create(nil);
  Result := FSUBACKProperties;
end;

function TsgcWSSUBACKS.GetItem(i: Integer): TsgcWSSUBACK;
begin
  Result := TsgcWSSUBACK(Items[i]);
end;

procedure TsgcWSSUBACKS.SetItem(i: Integer; const Value: TsgcWSSUBACK);
begin
  Items[i] := Value;
end;

procedure TsgcWSMQTTVariableHeaderPacketIdentifier.Clear;
begin
  inherited;
  PacketIdentifier := 0;
end;

procedure TsgcWSMQTTPayLoadHeader.AddStream(const aStream: TStream);
begin
  aStream.Position := 0;
  ReadTIdBytesFromStream(aStream, FBytes, aStream.Size);
end;

constructor TsgcWSMQTTConnect_Properties.Create;
begin
  inherited;
  FEnabled := False;
  FSessionExpiryInterval := 0;
  FReceiveMaximum := 65535;
  FMaximumPacketSize := MaxInt;
  FTopicAliasMaximum := 0;
  FRequestResponseInformation := False;
  FRequestProblemInformation := True;
  FUserProperties := TStringList.Create;
  FAuthenticationMethod := '';
end;

destructor TsgcWSMQTTConnect_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTConnect_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTConnect_Properties then
  begin
    FEnabled := TsgcWSMQTTConnect_Properties(aSource).Enabled;
    FSessionExpiryInterval := TsgcWSMQTTConnect_Properties(aSource)
      .SessionExpiryInterval;
    FReceiveMaximum := TsgcWSMQTTConnect_Properties(aSource).ReceiveMaximum;
    FMaximumPacketSize := TsgcWSMQTTConnect_Properties(aSource)
      .MaximumPacketSize;
    FTopicAliasMaximum := TsgcWSMQTTConnect_Properties(aSource)
      .TopicAliasMaximum;
    FRequestResponseInformation := TsgcWSMQTTConnect_Properties(aSource)
      .RequestResponseInformation;
    FRequestProblemInformation := TsgcWSMQTTConnect_Properties(aSource)
      .RequestProblemInformation;
    FUserProperties.Text := TsgcWSMQTTConnect_Properties(aSource)
      .UserProperties.Text;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSMQTTConnect_Properties.GetMaximumPacketSize: Integer;
begin
  if FMaximumPacketSize < 1 then
    FMaximumPacketSize := MaxInt;
  Result := FMaximumPacketSize;
end;

function TsgcWSMQTTConnect_Properties.GetReceiveMaximum: Integer;
begin
  if FReceiveMaximum > 65535 then
    FReceiveMaximum := 65535;
  if FReceiveMaximum < 1 then
    FReceiveMaximum := 1;
  Result := FReceiveMaximum;
end;

function TsgcWSMQTTConnect_Properties.GetSessionExpiryInterval: Integer;
begin
  if FSessionExpiryInterval < 0 then
    FSessionExpiryInterval := 0;
  Result := FSessionExpiryInterval;
end;

function TsgcWSMQTTConnect_Properties.GetTopicAliasMaximum: Integer;
begin
  if FReceiveMaximum > 65535 then
    FReceiveMaximum := 65535;
  if FReceiveMaximum < 0 then
    FReceiveMaximum := 0;
  Result := FTopicAliasMaximum;
end;

procedure TsgcWSMQTTConnect_Properties.SetUserProperties
  (const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

constructor TsgcWSMQTTWill_Properties.Create;
begin
  inherited;
  FEnabled := False;
  FWillDelayInterval := 0;
  FPayloadFormat := mqpfUnspecified;
  FMessageExpiryInterval := 0;
  FContentType := '';
  FResponseTopic := '';
  FCorrelationData := '';
  FUserProperties := TStringList.Create;
end;

destructor TsgcWSMQTTWill_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTWill_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTWill_Properties then
  begin
    FEnabled := TsgcWSMQTTWill_Properties(aSource).Enabled;
    FContentType := TsgcWSMQTTWill_Properties(aSource).ContentType;
    FCorrelationData := TsgcWSMQTTWill_Properties(aSource).CorrelationData;
    FMessageExpiryInterval := TsgcWSMQTTWill_Properties(aSource)
      .MessageExpiryInterval;
    FPayloadFormat := TsgcWSMQTTWill_Properties(aSource).PayloadFormat;
    FResponseTopic := TsgcWSMQTTWill_Properties(aSource).ResponseTopic;
    FUserProperties.Text := TsgcWSMQTTWill_Properties(aSource)
      .UserProperties.Text;
    FWillDelayInterval := TsgcWSMQTTWill_Properties(aSource).WillDelayInterval;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSMQTTWill_Properties.GetMessageExpiryInterval: Integer;
begin
  if FMessageExpiryInterval < 0 then
    FMessageExpiryInterval := 0;
  Result := FMessageExpiryInterval;
end;

function TsgcWSMQTTWill_Properties.GetWillDelayInterval: Integer;
begin
  if FWillDelayInterval < 0 then
    FWillDelayInterval := 0;
  Result := FWillDelayInterval;
end;

procedure TsgcWSMQTTWill_Properties.SetUserProperties(const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

function TsgcWSMQTTReadProperties.GetBoolean(const aBytes: TIdBytes;
  var aOffset: Integer): Boolean;
begin
  Result := aBytes[aOffset] = 1;

  aOffset := aOffset + 1;
end;

function TsgcWSMQTTReadProperties.GetInteger(const aBytes: TIdBytes;
  aLength: Integer; var aOffset: Integer): Integer;
var
  i, j: Integer;
begin
  Result := 0;

  j := 1;
  for i := aOffset + aLength - 1 Downto aOffset do
  begin
    Result := Result + aBytes[i] * j;
    if j = 1 then
      j := 256
    else
      j := j * 256;
  end;

  aOffset := aOffset + aLength;
end;

function TsgcWSMQTTReadProperties.GetString(const aBytes: TIdBytes;
  var aOffset: Integer): String;
begin
  Result := ReadString(aBytes, True, aOffset);

  aOffset := aOffset + ReadMSBLSB(aBytes[aOffset + 0], aBytes[aOffset + 1]) + 2;
end;

procedure TsgcWSMQTTReadProperties.ReadBytes(const aBytes: TIdBytes);
begin
  DoReadBytes(aBytes);
end;

constructor TsgcWSMQTTCONNACKProperties.Create(aOwner: TComponent);
begin
  inherited;
  Clear;
end;

destructor TsgcWSMQTTCONNACKProperties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTCONNACKProperties.Clear;
begin
  inherited;
  SessionExpiryInterval := 0;
  ReceiveMaximum := 65535;
  MaximumQoS := mtqsExactlyOnce;
  RetainAvailable := True;
  MaximumPacketSize := MaxInt;
  AssignedClientIdentifier := '';
  TopicAliasMaximum := 0;
  ReasonString := '';
  UserProperties.Clear;
  WildcardSubscriptionAvailable := True;
  SubscriptionIdentifiersAvailable := True;
  SharedSubscriptionAvailable := True;
  ServerKeepAlive := 0;
  ResponseInformation := '';
  ServerReference := '';
  AuthenticationMethod := '';
  AuthenticationData := '';
end;

procedure TsgcWSMQTTCONNACKProperties.DoReadBytes(const aBytes: TIdBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < Length(aBytes) do
  begin
    i := i + 1;
    case aBytes[i - 1] of
      17:
        SessionExpiryInterval := GetInteger(aBytes, 4, i);
      33:
        ReceiveMaximum := GetInteger(aBytes, 2, i);
      36:
        begin
          if aBytes[i + 1] = 0 then
            MaximumQoS := mtqsAtMostOnce
          else if aBytes[i + 1] = 1 then
            MaximumQoS := mtqsAtLeastOnce;
          i := i + 2;
        end;
      37:
        RetainAvailable := GetBoolean(aBytes, i);
      39:
        MaximumPacketSize := GetInteger(aBytes, 4, i);
      18:
        AssignedClientIdentifier := GetString(aBytes, i);
      34:
        TopicAliasMaximum := GetInteger(aBytes, 2, i);
      31:
        ReasonString := GetString(aBytes, i);
      38:
        UserProperties.Add(GetString(aBytes, i) + '=' + GetString(aBytes, i));
      40:
        WildcardSubscriptionAvailable := GetBoolean(aBytes, i);
      41:
        SubscriptionIdentifiersAvailable := GetBoolean(aBytes, i);
      42:
        SharedSubscriptionAvailable := GetBoolean(aBytes, i);
      19:
        ServerKeepAlive := GetInteger(aBytes, 2, i);
      26:
        ResponseInformation := GetString(aBytes, i);
      28:
        ServerReference := GetString(aBytes, i);
      21:
        AuthenticationMethod := GetString(aBytes, i);
      22:
        AuthenticationData := GetString(aBytes, i);
    end;
  end;
end;

function TsgcWSMQTTCONNACKProperties.GetUserProperties: TStringList;
begin
  if not Assigned(FUserProperties) then
    FUserProperties := TStringList.Create;
  Result := FUserProperties;
end;

procedure TsgcWSMQTTCONNACKProperties.SetSubscriptionIdentifiersAvailable
  (const Value: Boolean);
begin
  FSubscriptionIdentifiersAvailable := Value;
end;

destructor TsgcWSMQTTCONNACK.Destroy;
begin
  sgcFree(FCONNACKProperties);
  inherited;
end;

procedure TsgcWSMQTTCONNACK.Clear;
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
  Session := False;
  CONNACKProperties.Clear;
end;

function TsgcWSMQTTCONNACK.GetCONNACKProperties: TsgcWSMQTTCONNACKProperties;
begin
  if not Assigned(FCONNACKProperties) then
    FCONNACKProperties := TsgcWSMQTTCONNACKProperties.Create(nil);
  Result := FCONNACKProperties;
end;

constructor TsgcWSMQTTPublish_Properties.Create;
begin
  inherited;
  FEnabled := False;
  FTopicAlias := 0;
  FPayloadFormat := mqpfUnspecified;
  FMessageExpiryInterval := 0;
  FContentType := '';
  FResponseTopic := '';
  FCorrelationData := '';
  FSubscriptionIdentifier := 0;
  FUserProperties := TStringList.Create;
end;

destructor TsgcWSMQTTPublish_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTPublish_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTPublish_Properties then
  begin
    FEnabled := TsgcWSMQTTPublish_Properties(aSource).Enabled;
    FContentType := TsgcWSMQTTPublish_Properties(aSource).ContentType;
    FCorrelationData := TsgcWSMQTTPublish_Properties(aSource).CorrelationData;
    FMessageExpiryInterval := TsgcWSMQTTPublish_Properties(aSource)
      .MessageExpiryInterval;
    FPayloadFormat := TsgcWSMQTTPublish_Properties(aSource).PayloadFormat;
    FResponseTopic := TsgcWSMQTTPublish_Properties(aSource).ResponseTopic;
    FUserProperties.Text := TsgcWSMQTTPublish_Properties(aSource)
      .UserProperties.Text;
    FTopicAlias := TsgcWSMQTTPublish_Properties(aSource).TopicAlias;
    FSubscriptionIdentifier := TsgcWSMQTTPublish_Properties(aSource)
      .SubscriptionIdentifier;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSMQTTPublish_Properties.GetMessageExpiryInterval: Integer;
begin
  if FMessageExpiryInterval < 0 then
    FMessageExpiryInterval := 0;
  Result := FMessageExpiryInterval;
end;

function TsgcWSMQTTPublish_Properties.GetSubscriptionIdentifier: Integer;
begin
  if FSubscriptionIdentifier < 0 then
    FSubscriptionIdentifier := 0;
  Result := FSubscriptionIdentifier;
end;

function TsgcWSMQTTPublish_Properties.GetTopicAlias: Integer;
begin
  if FTopicAlias < 1 then
    FTopicAlias := 0;
  if FTopicAlias > 65535 then
    FTopicAlias := 65535;
  Result := FTopicAlias;
end;

procedure TsgcWSMQTTPublish_Properties.SetUserProperties
  (const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

constructor TsgcWSMQTTSubscribe_Properties.Create;
begin
  inherited;
  FEnabled := False;
  FSubscriptionIdentifier := 1;
  FUserProperties := TStringList.Create;
end;

destructor TsgcWSMQTTSubscribe_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTSubscribe_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTSubscribe_Properties then
  begin
    FEnabled := TsgcWSMQTTSubscribe_Properties(aSource).Enabled;
    FUserProperties.Text := TsgcWSMQTTSubscribe_Properties(aSource)
      .UserProperties.Text;
    FSubscriptionIdentifier := TsgcWSMQTTSubscribe_Properties(aSource)
      .SubscriptionIdentifier;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSMQTTSubscribe_Properties.GetSubscriptionIdentifier: Integer;
begin
  if FSubscriptionIdentifier < 1 then
    FSubscriptionIdentifier := 1;
  Result := FSubscriptionIdentifier;
end;

procedure TsgcWSMQTTSubscribe_Properties.SetUserProperties
  (const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

constructor TsgcWSMQTTSUBACKProperties.Create(aOwner: TComponent);
begin
  inherited;
  FUserProperties := TStringList.Create;
  ReasonString := '';
end;

destructor TsgcWSMQTTSUBACKProperties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTSUBACKProperties.Clear;
begin
  inherited;
  ReasonString := '';
  UserProperties.Clear;
end;

procedure TsgcWSMQTTSUBACKProperties.DoReadBytes(const aBytes: TIdBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < Length(aBytes) do
  begin
    i := i + 1;
    case aBytes[i - 1] of
      31:
        ReasonString := GetString(aBytes, i);
      38:
        UserProperties.Add(GetString(aBytes, i) + '=' + GetString(aBytes, i));
    end;
  end;
end;

procedure TsgcWSMQTTSUBACKProperties.SetUserProperties
  (const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

constructor TsgcWSMQTTUnsubscribe_Properties.Create;
begin
  inherited;
  FEnabled := False;
  FUserProperties := TStringList.Create;
end;

destructor TsgcWSMQTTUnsubscribe_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTUnsubscribe_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTUnsubscribe_Properties then
  begin
    FEnabled := TsgcWSMQTTUnsubscribe_Properties(aSource).Enabled;
    FUserProperties.Text := TsgcWSMQTTUnsubscribe_Properties(aSource)
      .UserProperties.Text;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSMQTTUnsubscribe_Properties.SetUserProperties
  (const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

constructor TsgcWSMQTTPUBSUBACKProperties.Create(aOwner: TComponent);
begin
  inherited;
  FUserProperties := TStringList.Create;
  ReasonString := '';
end;

destructor TsgcWSMQTTPUBSUBACKProperties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTPUBSUBACKProperties.Clear;
begin
  inherited;
  ReasonString := '';
  UserProperties.Clear;
end;

procedure TsgcWSMQTTPUBSUBACKProperties.DoReadBytes(const aBytes: TIdBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < Length(aBytes) do
  begin
    i := i + 1;
    case aBytes[i - 1] of
      31:
        ReasonString := GetString(aBytes, i);
      38:
        UserProperties.Add(GetString(aBytes, i) + '=' + GetString(aBytes, i));
    end;
  end;
end;

procedure TsgcWSMQTTPUBSUBACKProperties.SetUserProperties
  (const Value: TStringList);
begin
  if Assigned(FUserProperties) then
    FUserProperties.Assign(Value);
end;

procedure TsgcWSMQTTUNSUBACK.Clear;
begin
  inherited;
  Codes.Clear;
  UNSUBACKProperties.Clear;
end;

function TsgcWSMQTTUNSUBACK.GetCodes: TsgcWSUNSUBACKS;
begin
  if not Assigned(FCodes) then
    FCodes := TsgcWSUNSUBACKS.Create;
  Result := FCodes;
end;

function TsgcWSMQTTUNSUBACK.GetUNSUBACKProperties: TsgcWSMQTTUNSUBACKProperties;
begin
  if not Assigned(FUNSUBACKProperties) then
    FUNSUBACKProperties := TsgcWSMQTTUNSUBACKProperties.Create(nil);
  Result := FUNSUBACKProperties;
end;

function TsgcWSUNSUBACKS.GetItem(i: Integer): TsgcWSUNSUBACK;
begin
  Result := TsgcWSUNSUBACK(Items[i]);
end;

procedure TsgcWSUNSUBACKS.SetItem(i: Integer; const Value: TsgcWSUNSUBACK);
begin
  Items[i] := Value;
end;

constructor TsgcWSMQTTPUBACK.Create(aOwner: TComponent);
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
end;

destructor TsgcWSMQTTPUBACK.Destroy;
begin
  sgcFree(FPUBACKProperties);
  inherited;
end;

procedure TsgcWSMQTTPUBACK.Clear;
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
  PUBACKProperties.Clear;
end;

function TsgcWSMQTTPUBACK.GetPUBACKProperties: TsgcWSMQTTPUBACKProperties;
begin
  if not Assigned(FPUBACKProperties) then
    FPUBACKProperties := TsgcWSMQTTPUBACKProperties.Create(nil);
  Result := FPUBACKProperties;
end;

constructor TsgcWSMQTTPUBREC.Create(aOwner: TComponent);
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
end;

destructor TsgcWSMQTTPUBREC.Destroy;
begin
  sgcFree(FPUBRECProperties);
  inherited;
end;

procedure TsgcWSMQTTPUBREC.Clear;
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
  PUBRECProperties.Clear;
end;

function TsgcWSMQTTPUBREC.GetPUBRECProperties: TsgcWSMQTTPUBRECProperties;
begin
  if not Assigned(FPUBRECProperties) then
    FPUBRECProperties := TsgcWSMQTTPUBRECProperties.Create(nil);
  Result := FPUBRECProperties;
end;

constructor TsgcWSMQTTPUBREL.Create(aOwner: TComponent);
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
end;

destructor TsgcWSMQTTPUBREL.Destroy;
begin
  sgcFree(FPUBRELProperties);
  inherited;
end;

procedure TsgcWSMQTTPUBREL.Clear;
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
  PUBRELProperties.Clear;
end;

function TsgcWSMQTTPUBREL.GetPUBRELProperties: TsgcWSMQTTPUBRELProperties;
begin
  if not Assigned(FPUBRELProperties) then
    FPUBRELProperties := TsgcWSMQTTPUBRELProperties.Create(nil);
  Result := FPUBRELProperties;
end;

constructor TsgcWSMQTTPUBCOMP.Create(aOwner: TComponent);
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
end;

destructor TsgcWSMQTTPUBCOMP.Destroy;
begin
  sgcFree(FPUBCOMPProperties);
  inherited;
end;

procedure TsgcWSMQTTPUBCOMP.Clear;
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
  PUBCOMPProperties.Clear;
end;

function TsgcWSMQTTPUBCOMP.GetPUBCOMPProperties: TsgcWSMQTTPUBCOMPProperties;
begin
  if not Assigned(FPUBCOMPProperties) then
    FPUBCOMPProperties := TsgcWSMQTTPUBCOMPProperties.Create(nil);
  Result := FPUBCOMPProperties;
end;

constructor TsgcWSMQTTDISCONNECTProperties.Create(aOwner: TComponent);
begin
  inherited;
  Clear;
end;

destructor TsgcWSMQTTDISCONNECTProperties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTDISCONNECTProperties.Clear;
begin
  inherited;
  SessionExpiryInterval := 0;
  ReasonString := '';
  ServerReference := '';
  UserProperties.Clear;
end;

procedure TsgcWSMQTTDISCONNECTProperties.DoReadBytes(const aBytes: TIdBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < Length(aBytes) do
  begin
    i := i + 1;
    case aBytes[i - 1] of
      17:
        SessionExpiryInterval := GetInteger(aBytes, 4, i);
      31:
        ReasonString := GetString(aBytes, i);
      38:
        UserProperties.Add(GetString(aBytes, i) + '=' + GetString(aBytes, i));
      28:
        ServerReference := GetString(aBytes, i);
    end;
  end;
end;

function TsgcWSMQTTDISCONNECTProperties.GetSessionExpiryInterval: Integer;
begin
  if FSessionExpiryInterval < 0 then
    FSessionExpiryInterval := 0;
  Result := FSessionExpiryInterval;
end;

function TsgcWSMQTTDISCONNECTProperties.GetUserProperties: TStringList;
begin
  if not Assigned(FUserProperties) then
    FUserProperties := TStringList.Create;
  Result := FUserProperties;
end;

constructor TsgcWSMQTTDISCONNECT.Create(aOwner: TComponent);
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
end;

destructor TsgcWSMQTTDISCONNECT.Destroy;
begin
  sgcFree(FDISCONNECTProperties);
  inherited;
end;

procedure TsgcWSMQTTDISCONNECT.Clear;
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
  DISCONNECTProperties.Clear;
end;

function TsgcWSMQTTDISCONNECT.GetDISCONNECTProperties
  : TsgcWSMQTTDISCONNECTProperties;
begin
  if not Assigned(FDISCONNECTProperties) then
    FDISCONNECTProperties := TsgcWSMQTTDISCONNECTProperties.Create(nil);
  Result := FDISCONNECTProperties;
end;

constructor TsgcWSMQTTDisconnect_Properties.Create;
begin
  inherited;
  FSessionExpiryInterval := 0;
  FUserProperties := TStringList.Create;
  FReasonString := '';
  FServerReference := '';
end;

destructor TsgcWSMQTTDisconnect_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTDisconnect_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTDisconnect_Properties then
  begin
    FEnabled := TsgcWSMQTTDisconnect_Properties(aSource).Enabled;
    FSessionExpiryInterval := TsgcWSMQTTDisconnect_Properties(aSource)
      .SessionExpiryInterval;
    FReasonString := TsgcWSMQTTDisconnect_Properties(aSource).ReasonString;
    FServerReference := TsgcWSMQTTDisconnect_Properties(aSource)
      .ServerReference;
    FUserProperties.Text := TsgcWSMQTTDisconnect_Properties(aSource)
      .UserProperties.Text;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSMQTTDisconnect_Properties.GetSessionExpiryInterval: Integer;
begin
  if FSessionExpiryInterval < 0 then
    FSessionExpiryInterval := 0;
  Result := FSessionExpiryInterval;
end;

procedure TsgcWSMQTTDisconnect_Properties.SetUserProperties
  (const Value: TStringList);
begin
  FUserProperties := Value;
end;

constructor TsgcWSMQTTAUTHProperties.Create(aOwner: TComponent);
begin
  inherited;
  Clear;
end;

destructor TsgcWSMQTTAUTHProperties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTAUTHProperties.Clear;
begin
  inherited;
  AuthenticationData := '';
  ReasonString := '';
  AuthenticationMethod := '';
  UserProperties.Clear;
end;

procedure TsgcWSMQTTAUTHProperties.DoReadBytes(const aBytes: TIdBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < Length(aBytes) do
  begin
    i := i + 1;
    case aBytes[i - 1] of
      21:
        AuthenticationMethod := GetString(aBytes, i);
      22:
        AuthenticationData := GetString(aBytes, i);
      31:
        ReasonString := GetString(aBytes, i);
      38:
        UserProperties.Add(GetString(aBytes, i) + '=' + GetString(aBytes, i));
    end;
  end;
end;

function TsgcWSMQTTAUTHProperties.GetUserProperties: TStringList;
begin
  if not Assigned(FUserProperties) then
    FUserProperties := TStringList.Create;
  Result := FUserProperties;
end;

constructor TsgcWSMQTTAuth_Properties.Create;
begin
  inherited;
  FAuthenticationData := '';
  FUserProperties := TStringList.Create;
  FReasonString := '';
  FAuthenticationMethod := '';
end;

destructor TsgcWSMQTTAuth_Properties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTAuth_Properties.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSMQTTAuth_Properties then
  begin
    FEnabled := TsgcWSMQTTAuth_Properties(aSource).Enabled;
    FAuthenticationData := TsgcWSMQTTAuth_Properties(aSource)
      .AuthenticationData;
    FReasonString := TsgcWSMQTTAuth_Properties(aSource).ReasonString;
    FAuthenticationMethod := TsgcWSMQTTAuth_Properties(aSource)
      .AuthenticationMethod;
    FUserProperties.Text := TsgcWSMQTTAuth_Properties(aSource)
      .UserProperties.Text;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSMQTTAuth_Properties.SetUserProperties(const Value: TStringList);
begin
  FUserProperties := Value;
end;

constructor TsgcWSMQTTAUTH.Create(aOwner: TComponent);
begin
  inherited;
  ReasonCode := 0;
  ReasonName := '';
end;

destructor TsgcWSMQTTAUTH.Destroy;
begin
  sgcFree(FAUTHProperties);
  inherited;
end;

function TsgcWSMQTTAUTH.GetAUTHProperties: TsgcWSMQTTAUTHProperties;
begin
  if not Assigned(FAUTHProperties) then
    FAUTHProperties := TsgcWSMQTTAUTHProperties.Create(nil);
  Result := FAUTHProperties;
end;

procedure TsgcWSMQTTPUBLISH.Clear;
begin
  inherited;
  PacketIdentifier := 0;
  Text := '';
  Topic := '';
  PUBLISHProperties.Clear;
end;

function TsgcWSMQTTPUBLISH.GetPUBLISHProperties: TsgcWSMQTTPublishProperties;
begin
  if not Assigned(FPUBLISHProperties) then
    FPUBLISHProperties := TsgcWSMQTTPublishProperties.Create(self);
  Result := FPUBLISHProperties;
end;

constructor TsgcWSMQTTPublishProperties.Create(aOwner: TComponent);
begin
  inherited;
  Clear;
end;

destructor TsgcWSMQTTPublishProperties.Destroy;
begin
  sgcFree(FUserProperties);
  inherited;
end;

procedure TsgcWSMQTTPublishProperties.Clear;
begin
  inherited;
  FEnabled := False;
  FTopicAlias := 0;
  FPayloadFormat := mqpfUnspecified;
  FMessageExpiryInterval := 0;
  FContentType := '';
  FResponseTopic := '';
  FCorrelationData := '';
  FSubscriptionIdentifier := 0;
  UserProperties.Clear;
end;

procedure TsgcWSMQTTPublishProperties.DoReadBytes(const aBytes: TIdBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < Length(aBytes) do
  begin
    i := i + 1;
    case aBytes[i - 1] of
      1:
        begin
          if GetBoolean(aBytes, i) then
            PayloadFormat := mqpfUTF8
          else
            PayloadFormat := mqpfUnspecified;
        end;
      2:
        MessageExpiryInterval := GetInteger(aBytes, 4, i);
      35:
        TopicAlias := GetInteger(aBytes, 2, i);
      8:
        ResponseTopic := GetString(aBytes, i);
      9:
        CorrelationData := GetString(aBytes, i);
      38:
        UserProperties.Add(GetString(aBytes, i) + '=' + GetString(aBytes, i));
      11:
        SubscriptionIdentifier := GetVarInteger(aBytes, i);
      3:
        ContentType := GetString(aBytes, i);
    end;
  end;
end;

function TsgcWSMQTTPublishProperties.GetUserProperties: TStringList;
begin
  if not Assigned(FUserProperties) then
    FUserProperties := TStringList.Create;
  Result := FUserProperties;
end;

constructor TsgcWSMQTTReadPacketBuffer.Create(aOwner: TComponent);
begin
  inherited;
  ReadPacketState := mqrsFixedHeader;
  FixedHeaderLength := 0;
  RemainingLength := 0;
end;

destructor TsgcWSMQTTReadPacketBuffer.Destroy;
begin
  sgcFree(FStreamBuffer);
  inherited;
end;

function TsgcWSMQTTReadPacketBuffer.GetStreamBuffer: TMemoryStream;
begin
  if not Assigned(FStreamBuffer) then
    FStreamBuffer := TMemoryStream.Create;
  Result := FStreamBuffer;
end;

function TsgcWSMQTTReadPacketBuffer.Read(const aStream: TStream): Boolean;
var
  oStream: TMemoryStream;
  vBytes: TIdBytes;
  vMultiplier: Integer;
begin
  result := False;

  // copy to buffer
  StreamBuffer.Position := StreamBuffer.Size;
  StreamBuffer.CopyFrom(aStream, aStream.Size);
  StreamBuffer.Position := 0;
  // clear
  TMemoryStream(aStream).Clear;

  case ReadPacketState of
    mqrsFixedHeader:
      begin
        // create copy of buffer to read if there are enough bytes
        oStream := TMemoryStream.Create;
        Try
          FixedHeaderLength := 1;
          oStream.CopyFrom(StreamBuffer, StreamBuffer.Size);
          StreamBuffer.Position := 0;
          oStream.Position := 0;
          ReadTIdBytesFromStream(oStream, vBytes, 1);
          ControlPacket := TMQTTControlPacket(Ord(vBytes[0] shr 4));

          vMultiplier := 1;
          repeat
            // requires more bytes to read remaining length
            if oStream.Position = oStream.Size then
              exit;
            ReadTIdBytesFromStream(oStream, vBytes, 1);
            RemainingLength := RemainingLength + (vBytes[0] and 127) * vMultiplier;
            if (vMultiplier > 128*128*128) then
              raise Exception.Create(S_MQTT_ERROR_MALFORMED_VARIABLE_BYTE_INTEGER);
            vMultiplier := vMultiplier * 128;
            FixedHeaderLength := FixedHeaderLength + 1;
          until
            (vBytes[0] and 128) = 0;

          result := oStream.Size >= (FixedHeaderLength + RemainingLength);
          // requires more bytes to read all message
          if not result then
            ReadPacketState := mqrsMessage;
        Finally
          sgcFree(oStream);
        End;
      end;
    mqrsMessage:
      begin
        result := StreamBuffer.Size >= (FixedHeaderLength + RemainingLength);
      end;
  end;

  // if there is more than one packet
  if StreamBuffer.Size > (FixedHeaderLength + RemainingLength) then
  begin
    StreamBuffer.Position := FixedHeaderLength + RemainingLength;
    aStream.CopyFrom(StreamBuffer, StreamBuffer.Size - StreamBuffer.Position);
    aStream.Position := 0;
    StreamBuffer.Position := 0;
  end;

  // reset packet state
  if result then
  begin
    if ReadPacketState = mqrsMessage then
      ReadPacketState := mqrsFixedHeader;
  end;
end;

procedure TsgcWSMQTTReadPacketBuffer.SetReadPacketState(const Value:
    TwsMQTTReadState);
begin
  FReadPacketState := Value;
  case FReadPacketState of
    mqrsFixedHeader:
      begin
        FixedHeaderLength := 0;
        RemainingLength := 0;
      end;
    mqrsMessage: ;
  end;
end;

{$ENDIF}

end.
