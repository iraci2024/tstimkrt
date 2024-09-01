{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }
unit sgcWebSocket_Protocols;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes
  // sgc
    , sgcWebSocket_Protocol_MQTT_Client, sgcWebSocket_Protocol_STOMP_Client,
  sgcWebSocket_Protocol_STOMP_RabbitMQ_Client,
  sgcWebSocket_Protocol_STOMP_ActiveMQ_Client,
  sgcWebSocket_Protocol_WAMP2_Client
{$IFDEF SGC_EDT_PRO}
    , sgcWebSocket_Protocol_Broker_Client, sgcWebSocket_Protocol_Broker_Server,
  sgcWebSocket_Protocol_sgc_Client, sgcWebSocket_Protocol_sgc_Server,
  sgcWebSocket_Protocol_Dataset_Client, sgcWebSocket_Protocol_Dataset_Server,
  sgcWebSocket_Protocol_WebRTC_Server, sgcWebSocket_Protocol_AppRTC_Server,
  sgcWebSocket_Protocol_WAMP_Client, sgcWebSocket_Protocol_WAMP_Server,
  sgcWebSocket_Protocol_Files_Client, sgcWebSocket_Protocol_Files_Server,
  sgcWebSocket_Protocol_Presence_Client, sgcWebSocket_Protocol_Presence_Server
{$ENDIF}
{$IFDEF SGC_AMQP}
, sgcWebSocket_Protocol_AMQP_Client
{$ENDIF}
    ;

type
  TsgcWSPClient_WAMP2 = class(TsgcWSProtocol_WAMP2_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnWAMPSession;
    property OnWAMPWelcome;
    property OnWAMPAbort;
    property OnWAMPChallenge;
    property OnWAMPGoodBye;
    property OnWAMPSubscribed;
    property OnWAMPUnsubscribed;
    property OnWAMPPublished;
    property OnWAMPEvent;
    property OnWAMPRegistered;
    property OnWAMPUnRegistered;
    property OnWAMPResult;
    property OnWAMPError;

    property Client;
    property Broker;

    property Version;
  end;

  TsgcWSPClient_MQTT = class(TsgcWSProtocol_MQTT_Client)
  published
    // property OnConnect;
    // property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    // property OnError;
    // property OnRawMessage;
    property OnMQTTBeforeConnect;
    property OnMQTTConnect;
    property OnMQTTPing;
    property OnMQTTPublish;
    property OnMQTTPubAck;
    property OnMQTTPubRec;
    property OnMQTTPubRel;
    property OnMQTTPubComp;
    property OnMQTTSubscribe;
    property OnMQTTUnSubscribe;
    property OnMQTTDisconnect;
    property OnMQTTAuth;

    property Client;
    property Broker;

    property Authentication;
    property HeartBeat;
    property LastWillTestament;
    property QoS;
    property ConnectProperties;
    property MQTTVersion;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_STOMP = class(TsgcWSProtocol_STOMP_Client)
  published
    // property OnConnect;
    // property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    // property OnError;
    // property OnRawMessage;
    property OnSTOMPConnected;
    property OnSTOMPMessage;
    property OnSTOMPReceipt;
    property OnSTOMPError;
    property OnSTOMPDisconnected;

    property Client;
    property Broker;

    property Authentication;
    property HeartBeat;
    property Versions;
    property Options;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_STOMP_RabbitMQ = class(TsgcWSProtocol_STOMP_RabbitMQ_Client)
  published
    // property OnConnect;
    // property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    // property OnError;
    // property OnRawMessage;
    property OnRabbitMQConnected;
    property OnRabbitMQMessage;
    property OnRabbitMQReceipt;
    property OnRabbitMQError;
    property OnRabbitMQDisconnected;

    property Client;
    property Broker;

    property Authentication;
    property HeartBeat;
    property Versions;
    property Options;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_STOMP_ActiveMQ = class(TsgcWSProtocol_STOMP_ActiveMQ_Client)
  published
    // property OnConnect;
    // property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    // property OnError;
    // property OnRawMessage;
    property OnActiveMQConnected;
    property OnActiveMQMessage;
    property OnActiveMQReceipt;
    property OnActiveMQError;
    property OnActiveMQDisconnected;

    property Client;
    property Broker;

    property Authentication;
    property HeartBeat;
    property Versions;
    property Options;
    property ActiveMQ_Options;
    property Guid;

    property Version;
  end;
{$IFDEF SGC_EDT_PRO}

  TsgcWSPServer_Broker = class(TsgcWSProtocol_Broker_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnException;

    property Server;

    property Version;
  end;

  TsgcWSPClient_Broker = class(TsgcWSProtocol_Broker_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnException;

    property Client;

    property Version;
  end;

  TsgcWSPServer_sgc = class(TsgcWSProtocol_sgc_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnBeforeSubscription;
    property OnSubscription;
    property OnUnSubscription;
    property OnRawMessage;
    property OnException;
    property RPCAuthentication;
    property OnNotification;
    property OnRPC;
    property OnRPCAuthentication;
    property OnBeforePublish;
    property OnAcknowledgment;

    property Server;
    property Broker;
    property QoS;
    property Guid;
    property Version;
  end;

  TsgcWSPClient_sgc = class(TsgcWSProtocol_sgc_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnSubscription;
    property OnUnSubscription;
    property OnRawMessage;
    property OnException;
    property OnRPCResult;
    property OnRPCError;
    property OnEvent;
    property OnAcknowledgment;
    property OnSession;

    property Client;
    property Broker;

    property QoS;
    property Guid;

    property Version;
  end;

  TsgcWSPServer_Dataset = class(TsgcWSProtocol_Dataset_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnBeforeSubscription;
    property OnSubscription;
    property OnUnSubscription;
    property OnRawMessage;
    property OnException;
    property OnBeforeNewRecord;
    property OnBeforeUpdateRecord;
    property OnBeforeDeleteRecord;
    property OnAfterNewRecord;
    property OnAfterUpdateRecord;
    property OnAfterDeleteRecord;
    property OnBeforeDatasetUpdate;
    property OnNotification;
    property OnRPC;
    property OnRPCAuthentication;
    property OnBeforePublish;

    property Server;
    property DataSet;
    property Broker;

    property RPCAuthentication;
    property AutoEscapeText;
    property EncodeBase64;
    property AutoSynchronize;
    property NotifyUpdates;
    property ApplyUpdates;
    property UpdateMode;
    property FormatSettings;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_Dataset = class(TsgcWSProtocol_Dataset_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnSubscription;
    property OnUnSubscription;
    property OnRawMessage;
    property OnException;
    property OnBeforeNewRecord;
    property OnBeforeUpdateRecord;
    property OnBeforeDeleteRecord;
    property OnAfterNewRecord;
    property OnAfterUpdateRecord;
    property OnAfterDeleteRecord;
    property OnBeforeDatasetUpdate;
    property OnBeforeSynchronize;
    property OnAfterSynchronize;
    property OnMetaData;
    property OnRPCResult;
    property OnRPCError;
    property OnEvent;
    property OnAcknowledgment;
    property OnSession;

    property Client;
    property DataSet;
    property Broker;

    property AutoEscapeText;
    property EncodeBase64;
    property AutoSubscribe;
    property NotifyUpdates;
    property ApplyUpdates;
    property UpdateMode;
    property QoS;
    property FormatSettings;
    property Guid;

    property Version;
  end;

  TsgcWSPServer_WebRTC = class(TsgcWSProtocol_WebRTC_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnBeforeSubscription;
    property OnSubscription;
    property OnUnSubscription;
    property OnRawMessage;
    property OnException;

    property Server;
    property Broker;
    property WebRTC;

    property Guid;

    property Version;
  end;

  TsgcWSPServer_AppRTC = class(TsgcWSProtocol_AppRTC_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnBeforeSubscription;
    property OnSubscription;
    property OnUnSubscription;
    property OnRawMessage;
    property OnException;

    property AppRTC;

    property Server;
    property Broker;
    property Guid;

    property Version;
  end;

  TsgcWSPServer_WAMP = class(TsgcWSProtocol_WAMP_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnException;
    property OnBeforeSubscription;
    property OnSubscription;
    property OnUnSubscription;
    property OnCall;
    property OnBeforeCancelCall;
    property OnPrefix;

    property Server;
    property Broker;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_WAMP = class(TsgcWSProtocol_WAMP_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnException;
    property OnWelcome;
    property OnCallResult;
    property OnCallProgressResult;
    property OnCallError;
    property OnEvent;

    property Client;
    property Broker;

    property Version;
  end;

  TsgcWSPServer_Files = class(TsgcWSProtocol_Files_Server)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnException;
    property OnBeforeSubscription;
    property OnSubscription;
    property OnUnSubscription;
    property OnFileReceivedAuthorization;
    property OnFileReceived;
    property OnFileReceivedError;
    property OnFileReceivedFragment;
    property OnFileBeforeSent;
    property OnFileSentFragmentRequest;
    property OnFileSentAcknowledgment;
    property OnFileSent;
    property OnFileSentError;

    property Server;
    property Broker;
    property Files;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_Files = class(TsgcWSProtocol_Files_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnException;
    property OnSubscription;
    property OnUnSubscription;
    property OnFileReceivedAuthorization;
    property OnFileReceived;
    property OnFileReceivedError;
    property OnFileReceivedFragment;
    property OnFileBeforeSent;
    property OnFileSentFragmentRequest;
    property OnFileSentAcknowledgment;
    property OnFileSent;
    property OnFileSentError;

    property Client;
    property Broker;
    property Files;
    property Guid;

    property Version;
  end;

  TsgcWSPServer_Presence = class(TsgcWSProtocol_Presence_Server)
  published
    property OnConnect;
    property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnException;
    property OnBeforeNewMember;
    property OnNewMember;
    property OnRemoveMember;
    property OnBeforeNewChannel;
    property OnNewChannel;
    property OnBeforeNewChannelMember;
    property OnNewChannelMember;
    property OnRemoveChannelMember;
    property OnBeforePublishMsg;
    property OnBeforeSendMembers;
    property OnErrorMemberChannel;
    property OnErrorPublishMsg;

    property Acknowledgment;
    property EncodeBase64;
    property Server;
    property Broker;
    property Guid;

    property Version;
  end;

  TsgcWSPClient_Presence = class(TsgcWSProtocol_Presence_Client)
  published
    property OnConnect;
    property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnRawMessage;
    property OnException;
    property OnSession;
    property OnNewMember;
    property OnRemoveMember;
    property OnNewChannelMember;
    property OnRemoveChannelMember;
    property OnPublishMsg;
    property OnGetMembers;
    property OnErrorMemberChannel;
    property OnErrorPublishMsg;
    property OnChannelInvitation;

    property Presence;
    property Acknowledgment;
    property EncodeBase64;
    property Client;
    property Broker;
    property Guid;

    property Version;
  end;

{$ENDIF}
{$ENDIF}

{$IFDEF SGC_AMQP}
  TsgcWSPClient_AMQP = class(TsgcWSProtocol_AMQP_Client)
  published
    property OnAMQPConnect;
    property OnAMQPAuthentication;
    property OnAMQPChallenge;
    property OnAMQPHeartBeat;
    property OnAMQPChannelOpen;
    property OnAMQPChannelClose;
    property OnAMQPChannelFlow;
    property OnAMQPExchangeDeclare;
    property OnAMQPExchangeDelete;
    property OnAMQPQueueDeclare;
    property OnAMQPQueueBind;
    property OnAMQPQueueUnBind;
    property OnAMQPQueuePurge;
    property OnAMQPQueueDelete;
    property OnAMQPBasicQoS;
    property OnAMQPBasicConsume;
    property OnAMQPBasicCancelConsume;
    property OnAMQPBasicReturn;
    property OnAMQPBasicDeliver;
    property OnAMQPBasicGetOk;
    property OnAMQPBasicGetEmpty;
    property OnAMQPBasicRecoverOk;
    property OnAMQPTransactionOk;
    property OnAMQPDisconnect;
    property OnAMQPException;

    property AMQPOptions;
    property HeartBeat;

    property Client;
    property Broker;
    property Guid;

    property Version;
  end;
{$ENDIF}

implementation

end.
