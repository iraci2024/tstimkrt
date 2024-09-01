// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocols.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocolsHPP
#define Sgcwebsocket_protocolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcWebSocket_Protocol_MQTT_Client.hpp>
#include <sgcWebSocket_Protocol_STOMP_Client.hpp>
#include <sgcWebSocket_Protocol_STOMP_RabbitMQ_Client.hpp>
#include <sgcWebSocket_Protocol_STOMP_ActiveMQ_Client.hpp>
#include <sgcWebSocket_Protocol_WAMP2_Client.hpp>
#include <sgcWebSocket_Protocol_Broker_Client.hpp>
#include <sgcWebSocket_Protocol_Broker_Server.hpp>
#include <sgcWebSocket_Protocol_sgc_Client.hpp>
#include <sgcWebSocket_Protocol_sgc_Server.hpp>
#include <sgcWebSocket_Protocol_Dataset_Client.hpp>
#include <sgcWebSocket_Protocol_Dataset_Server.hpp>
#include <sgcWebSocket_Protocol_WebRTC_Server.hpp>
#include <sgcWebSocket_Protocol_AppRTC_Server.hpp>
#include <sgcWebSocket_Protocol_WAMP_Client.hpp>
#include <sgcWebSocket_Protocol_WAMP_Server.hpp>
#include <sgcWebSocket_Protocol_Files_Client.hpp>
#include <sgcWebSocket_Protocol_Files_Server.hpp>
#include <sgcWebSocket_Protocol_Presence_Client.hpp>
#include <sgcWebSocket_Protocol_Presence_Server.hpp>
#include <sgcWebSocket_Protocol_AMQP_Client.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Protocol_MQTT_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_STOMP_Broker_Client.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Protocol_Dataset_Message.hpp>
#include <Data.DB.hpp>
#include <sgcWebSocket_Protocol_Files_Message.hpp>
#include <sgcWebSocket_Protocol_Presence_Message.hpp>
#include <sgcAMQP.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocols
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSPClient_WAMP2;
class DELPHICLASS TsgcWSPClient_MQTT;
class DELPHICLASS TsgcWSPClient_STOMP;
class DELPHICLASS TsgcWSPClient_STOMP_RabbitMQ;
class DELPHICLASS TsgcWSPClient_STOMP_ActiveMQ;
class DELPHICLASS TsgcWSPServer_Broker;
class DELPHICLASS TsgcWSPClient_Broker;
class DELPHICLASS TsgcWSPServer_sgc;
class DELPHICLASS TsgcWSPClient_sgc;
class DELPHICLASS TsgcWSPServer_Dataset;
class DELPHICLASS TsgcWSPClient_Dataset;
class DELPHICLASS TsgcWSPServer_WebRTC;
class DELPHICLASS TsgcWSPServer_AppRTC;
class DELPHICLASS TsgcWSPServer_WAMP;
class DELPHICLASS TsgcWSPClient_WAMP;
class DELPHICLASS TsgcWSPServer_Files;
class DELPHICLASS TsgcWSPClient_Files;
class DELPHICLASS TsgcWSPServer_Presence;
class DELPHICLASS TsgcWSPClient_Presence;
class DELPHICLASS TsgcWSPClient_AMQP;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSPClient_WAMP2 : public Sgcwebsocket_protocol_wamp2_client::TsgcWSProtocol_WAMP2_Client
{
	typedef Sgcwebsocket_protocol_wamp2_client::TsgcWSProtocol_WAMP2_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnRawMessage;
	__property OnWAMPSession;
	__property OnWAMPWelcome;
	__property OnWAMPAbort;
	__property OnWAMPChallenge;
	__property OnWAMPGoodBye;
	__property OnWAMPSubscribed;
	__property OnWAMPUnsubscribed;
	__property OnWAMPPublished;
	__property OnWAMPEvent;
	__property OnWAMPRegistered;
	__property OnWAMPUnRegistered;
	__property OnWAMPResult;
	__property OnWAMPError;
	__property Client;
	__property Broker;
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_WAMP2_Client.Create */ inline __fastcall virtual TsgcWSPClient_WAMP2(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_wamp2_client::TsgcWSProtocol_WAMP2_Client(aOwner) { }
	/* TsgcWSProtocol_WAMP2_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_WAMP2() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_MQTT : public Sgcwebsocket_protocol_mqtt_client::TsgcWSProtocol_MQTT_Client
{
	typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSProtocol_MQTT_Client inherited;
	
__published:
	__property OnMQTTBeforeConnect;
	__property OnMQTTConnect;
	__property OnMQTTPing;
	__property OnMQTTPublish;
	__property OnMQTTPubAck;
	__property OnMQTTPubRec;
	__property OnMQTTPubRel;
	__property OnMQTTPubComp;
	__property OnMQTTSubscribe;
	__property OnMQTTUnSubscribe;
	__property OnMQTTDisconnect;
	__property OnMQTTAuth;
	__property Client;
	__property Broker;
	__property Authentication;
	__property HeartBeat;
	__property LastWillTestament;
	__property QoS;
	__property ConnectProperties;
	__property MQTTVersion;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_MQTT_Client.Create */ inline __fastcall virtual TsgcWSPClient_MQTT(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_mqtt_client::TsgcWSProtocol_MQTT_Client(aOwner) { }
	/* TsgcWSProtocol_MQTT_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_MQTT() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_STOMP : public Sgcwebsocket_protocol_stomp_client::TsgcWSProtocol_STOMP_Client
{
	typedef Sgcwebsocket_protocol_stomp_client::TsgcWSProtocol_STOMP_Client inherited;
	
__published:
	__property OnSTOMPConnected;
	__property OnSTOMPMessage;
	__property OnSTOMPReceipt;
	__property OnSTOMPError;
	__property OnSTOMPDisconnected;
	__property Client;
	__property Broker;
	__property Authentication;
	__property HeartBeat;
	__property Versions;
	__property Options;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_STOMP_Client_Base.Create */ inline __fastcall virtual TsgcWSPClient_STOMP(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_client::TsgcWSProtocol_STOMP_Client(aOwner) { }
	/* TsgcWSProtocol_STOMP_Client_Base.Destroy */ inline __fastcall virtual ~TsgcWSPClient_STOMP() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_STOMP_RabbitMQ : public Sgcwebsocket_protocol_stomp_rabbitmq_client::TsgcWSProtocol_STOMP_RabbitMQ_Client
{
	typedef Sgcwebsocket_protocol_stomp_rabbitmq_client::TsgcWSProtocol_STOMP_RabbitMQ_Client inherited;
	
__published:
	__property OnRabbitMQConnected;
	__property OnRabbitMQMessage;
	__property OnRabbitMQReceipt;
	__property OnRabbitMQError;
	__property OnRabbitMQDisconnected;
	__property Client;
	__property Broker;
	__property Authentication;
	__property HeartBeat;
	__property Versions;
	__property Options;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_STOMP_RabbitMQ_Client.Create */ inline __fastcall virtual TsgcWSPClient_STOMP_RabbitMQ(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_rabbitmq_client::TsgcWSProtocol_STOMP_RabbitMQ_Client(aOwner) { }
	/* TsgcWSProtocol_STOMP_RabbitMQ_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_STOMP_RabbitMQ() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_STOMP_ActiveMQ : public Sgcwebsocket_protocol_stomp_activemq_client::TsgcWSProtocol_STOMP_ActiveMQ_Client
{
	typedef Sgcwebsocket_protocol_stomp_activemq_client::TsgcWSProtocol_STOMP_ActiveMQ_Client inherited;
	
__published:
	__property OnActiveMQConnected;
	__property OnActiveMQMessage;
	__property OnActiveMQReceipt;
	__property OnActiveMQError;
	__property OnActiveMQDisconnected;
	__property Client;
	__property Broker;
	__property Authentication;
	__property HeartBeat;
	__property Versions;
	__property Options;
	__property ActiveMQ_Options;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_STOMP_ActiveMQ_Client.Create */ inline __fastcall virtual TsgcWSPClient_STOMP_ActiveMQ(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_activemq_client::TsgcWSProtocol_STOMP_ActiveMQ_Client(aOwner) { }
	/* TsgcWSProtocol_STOMP_ActiveMQ_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_STOMP_ActiveMQ() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_Broker : public Sgcwebsocket_protocol_broker_server::TsgcWSProtocol_Broker_Server
{
	typedef Sgcwebsocket_protocol_broker_server::TsgcWSProtocol_Broker_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnException;
	__property Server;
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_Broker_Server.Create */ inline __fastcall virtual TsgcWSPServer_Broker(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_broker_server::TsgcWSProtocol_Broker_Server(aOwner) { }
	/* TsgcWSProtocol_Broker_Server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_Broker() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_Broker : public Sgcwebsocket_protocol_broker_client::TsgcWSProtocol_Broker_Client
{
	typedef Sgcwebsocket_protocol_broker_client::TsgcWSProtocol_Broker_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnException;
	__property Client;
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_Broker_Client.Create */ inline __fastcall virtual TsgcWSPClient_Broker(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_broker_client::TsgcWSProtocol_Broker_Client(aOwner) { }
	/* TsgcWSProtocol_Broker_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_Broker() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_sgc : public Sgcwebsocket_protocol_sgc_server::TsgcWSProtocol_sgc_Server
{
	typedef Sgcwebsocket_protocol_sgc_server::TsgcWSProtocol_sgc_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnBeforeSubscription;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnRawMessage;
	__property OnException;
	__property RPCAuthentication;
	__property OnNotification;
	__property OnRPC;
	__property OnRPCAuthentication;
	__property OnBeforePublish;
	__property OnAcknowledgment;
	__property Server;
	__property Broker;
	__property QoS;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_sgc_Server.Create */ inline __fastcall virtual TsgcWSPServer_sgc(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_sgc_server::TsgcWSProtocol_sgc_Server(aOwner) { }
	/* TsgcWSProtocol_sgc_Server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_sgc() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_sgc : public Sgcwebsocket_protocol_sgc_client::TsgcWSProtocol_sgc_Client
{
	typedef Sgcwebsocket_protocol_sgc_client::TsgcWSProtocol_sgc_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnRawMessage;
	__property OnException;
	__property OnRPCResult;
	__property OnRPCError;
	__property OnEvent;
	__property OnAcknowledgment;
	__property OnSession;
	__property Client;
	__property Broker;
	__property QoS;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_sgc_Client.Create */ inline __fastcall virtual TsgcWSPClient_sgc(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_sgc_client::TsgcWSProtocol_sgc_Client(aOwner) { }
	/* TsgcWSProtocol_sgc_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_sgc() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_Dataset : public Sgcwebsocket_protocol_dataset_server::TsgcWSProtocol_dataset_server
{
	typedef Sgcwebsocket_protocol_dataset_server::TsgcWSProtocol_dataset_server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnBeforeSubscription;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnRawMessage;
	__property OnException;
	__property OnBeforeNewRecord;
	__property OnBeforeUpdateRecord;
	__property OnBeforeDeleteRecord;
	__property OnAfterNewRecord;
	__property OnAfterUpdateRecord;
	__property OnAfterDeleteRecord;
	__property OnBeforeDatasetUpdate;
	__property OnNotification;
	__property OnRPC;
	__property OnRPCAuthentication;
	__property OnBeforePublish;
	__property Server;
	__property DataSet;
	__property Broker;
	__property RPCAuthentication;
	__property AutoEscapeText;
	__property EncodeBase64;
	__property AutoSynchronize;
	__property NotifyUpdates;
	__property ApplyUpdates;
	__property UpdateMode;
	__property FormatSettings;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_dataset_server.Create */ inline __fastcall virtual TsgcWSPServer_Dataset(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_dataset_server::TsgcWSProtocol_dataset_server(aOwner) { }
	/* TsgcWSProtocol_dataset_server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_Dataset() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_Dataset : public Sgcwebsocket_protocol_dataset_client::TsgcWSProtocol_dataset_client
{
	typedef Sgcwebsocket_protocol_dataset_client::TsgcWSProtocol_dataset_client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnRawMessage;
	__property OnException;
	__property OnBeforeNewRecord;
	__property OnBeforeUpdateRecord;
	__property OnBeforeDeleteRecord;
	__property OnAfterNewRecord;
	__property OnAfterUpdateRecord;
	__property OnAfterDeleteRecord;
	__property OnBeforeDatasetUpdate;
	__property OnBeforeSynchronize;
	__property OnAfterSynchronize;
	__property OnMetaData;
	__property OnRPCResult;
	__property OnRPCError;
	__property OnEvent;
	__property OnAcknowledgment;
	__property OnSession;
	__property Client;
	__property DataSet;
	__property Broker;
	__property AutoEscapeText;
	__property EncodeBase64;
	__property AutoSubscribe;
	__property NotifyUpdates;
	__property ApplyUpdates;
	__property UpdateMode;
	__property QoS;
	__property FormatSettings;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_dataset_client.Create */ inline __fastcall virtual TsgcWSPClient_Dataset(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_dataset_client::TsgcWSProtocol_dataset_client(aOwner) { }
	/* TsgcWSProtocol_dataset_client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_Dataset() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_WebRTC : public Sgcwebsocket_protocol_webrtc_server::TsgcWSProtocol_WebRTC_Server
{
	typedef Sgcwebsocket_protocol_webrtc_server::TsgcWSProtocol_WebRTC_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnBeforeSubscription;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnRawMessage;
	__property OnException;
	__property Server;
	__property Broker;
	__property WebRTC;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_WebRTC_Server.Create */ inline __fastcall virtual TsgcWSPServer_WebRTC(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_webrtc_server::TsgcWSProtocol_WebRTC_Server(aOwner) { }
	/* TsgcWSProtocol_WebRTC_Server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_WebRTC() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_AppRTC : public Sgcwebsocket_protocol_apprtc_server::TsgcWSProtocol_AppRTC_Server
{
	typedef Sgcwebsocket_protocol_apprtc_server::TsgcWSProtocol_AppRTC_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnBeforeSubscription;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnRawMessage;
	__property OnException;
	__property AppRTC;
	__property Server;
	__property Broker;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_AppRTC_Server.Create */ inline __fastcall virtual TsgcWSPServer_AppRTC(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_apprtc_server::TsgcWSProtocol_AppRTC_Server(aOwner) { }
	/* TsgcWSProtocol_AppRTC_Server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_AppRTC() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_WAMP : public Sgcwebsocket_protocol_wamp_server::TsgcWSProtocol_WAMP_Server
{
	typedef Sgcwebsocket_protocol_wamp_server::TsgcWSProtocol_WAMP_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnRawMessage;
	__property OnException;
	__property OnBeforeSubscription;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnCall;
	__property OnBeforeCancelCall;
	__property OnPrefix;
	__property Server;
	__property Broker;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_WAMP_Server.Create */ inline __fastcall virtual TsgcWSPServer_WAMP(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_wamp_server::TsgcWSProtocol_WAMP_Server(aOwner) { }
	
public:
	/* TsgcWSProtocol_Subscription_Server_Base.Destroy */ inline __fastcall virtual ~TsgcWSPServer_WAMP() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_WAMP : public Sgcwebsocket_protocol_wamp_client::TsgcWSProtocol_WAMP_Client
{
	typedef Sgcwebsocket_protocol_wamp_client::TsgcWSProtocol_WAMP_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnRawMessage;
	__property OnException;
	__property OnWelcome;
	__property OnCallResult;
	__property OnCallProgressResult;
	__property OnCallError;
	__property OnEvent;
	__property Client;
	__property Broker;
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_WAMP_Client.Create */ inline __fastcall virtual TsgcWSPClient_WAMP(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_wamp_client::TsgcWSProtocol_WAMP_Client(aOwner) { }
	/* TsgcWSProtocol_WAMP_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_WAMP() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_Files : public Sgcwebsocket_protocol_files_server::TsgcWSProtocol_Files_Server
{
	typedef Sgcwebsocket_protocol_files_server::TsgcWSProtocol_Files_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnRawMessage;
	__property OnException;
	__property OnBeforeSubscription;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnFileReceivedAuthorization;
	__property OnFileReceived;
	__property OnFileReceivedError;
	__property OnFileReceivedFragment;
	__property OnFileBeforeSent;
	__property OnFileSentFragmentRequest;
	__property OnFileSentAcknowledgment;
	__property OnFileSent;
	__property OnFileSentError;
	__property Server;
	__property Broker;
	__property Files;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_Files_Server.Create */ inline __fastcall virtual TsgcWSPServer_Files(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_files_server::TsgcWSProtocol_Files_Server(aOwner) { }
	/* TsgcWSProtocol_Files_Server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_Files() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_Files : public Sgcwebsocket_protocol_files_client::TsgcWSProtocol_Files_Client
{
	typedef Sgcwebsocket_protocol_files_client::TsgcWSProtocol_Files_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnError;
	__property OnRawMessage;
	__property OnException;
	__property OnSubscription;
	__property OnUnSubscription;
	__property OnFileReceivedAuthorization;
	__property OnFileReceived;
	__property OnFileReceivedError;
	__property OnFileReceivedFragment;
	__property OnFileBeforeSent;
	__property OnFileSentFragmentRequest;
	__property OnFileSentAcknowledgment;
	__property OnFileSent;
	__property OnFileSentError;
	__property Client;
	__property Broker;
	__property Files;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_Files_Client.Create */ inline __fastcall virtual TsgcWSPClient_Files(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_files_client::TsgcWSProtocol_Files_Client(aOwner) { }
	/* TsgcWSProtocol_Files_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_Files() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPServer_Presence : public Sgcwebsocket_protocol_presence_server::TsgcWSProtocol_Presence_Server
{
	typedef Sgcwebsocket_protocol_presence_server::TsgcWSProtocol_Presence_Server inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnError;
	__property OnRawMessage;
	__property OnException;
	__property OnBeforeNewMember;
	__property OnNewMember;
	__property OnRemoveMember;
	__property OnBeforeNewChannel;
	__property OnNewChannel;
	__property OnBeforeNewChannelMember;
	__property OnNewChannelMember;
	__property OnRemoveChannelMember;
	__property OnBeforePublishMsg;
	__property OnBeforeSendMembers;
	__property OnErrorMemberChannel;
	__property OnErrorPublishMsg;
	__property Acknowledgment;
	__property EncodeBase64;
	__property Server;
	__property Broker;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_Presence_Server.Create */ inline __fastcall virtual TsgcWSPServer_Presence(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_presence_server::TsgcWSProtocol_Presence_Server(aOwner) { }
	/* TsgcWSProtocol_Presence_Server.Destroy */ inline __fastcall virtual ~TsgcWSPServer_Presence() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_Presence : public Sgcwebsocket_protocol_presence_client::TsgcWSProtocol_Presence_Client
{
	typedef Sgcwebsocket_protocol_presence_client::TsgcWSProtocol_Presence_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnError;
	__property OnRawMessage;
	__property OnException;
	__property OnSession;
	__property OnNewMember;
	__property OnRemoveMember;
	__property OnNewChannelMember;
	__property OnRemoveChannelMember;
	__property OnPublishMsg;
	__property OnGetMembers;
	__property OnErrorMemberChannel;
	__property OnErrorPublishMsg;
	__property OnChannelInvitation;
	__property Presence;
	__property Acknowledgment;
	__property EncodeBase64;
	__property Client;
	__property Broker;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_Presence_Client.Create */ inline __fastcall virtual TsgcWSPClient_Presence(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_presence_client::TsgcWSProtocol_Presence_Client(aOwner) { }
	/* TsgcWSProtocol_Presence_Client.Destroy */ inline __fastcall virtual ~TsgcWSPClient_Presence() { }
	
};


class PASCALIMPLEMENTATION TsgcWSPClient_AMQP : public Sgcwebsocket_protocol_amqp_client::TsgcWSProtocol_AMQP_Client
{
	typedef Sgcwebsocket_protocol_amqp_client::TsgcWSProtocol_AMQP_Client inherited;
	
__published:
	__property OnAMQPConnect;
	__property OnAMQPAuthentication;
	__property OnAMQPChallenge;
	__property OnAMQPHeartBeat;
	__property OnAMQPChannelOpen;
	__property OnAMQPChannelClose;
	__property OnAMQPChannelFlow;
	__property OnAMQPExchangeDeclare;
	__property OnAMQPExchangeDelete;
	__property OnAMQPQueueDeclare;
	__property OnAMQPQueueBind;
	__property OnAMQPQueueUnBind;
	__property OnAMQPQueuePurge;
	__property OnAMQPQueueDelete;
	__property OnAMQPBasicQoS;
	__property OnAMQPBasicConsume;
	__property OnAMQPBasicCancelConsume;
	__property OnAMQPBasicReturn;
	__property OnAMQPBasicDeliver;
	__property OnAMQPBasicGetOk;
	__property OnAMQPBasicGetEmpty;
	__property OnAMQPBasicRecoverOk;
	__property OnAMQPTransactionOk;
	__property OnAMQPDisconnect;
	__property OnAMQPException;
	__property AMQPOptions;
	__property HeartBeat;
	__property Client;
	__property Broker;
	__property Guid = {default=0};
	__property Version = {default=0};
public:
	/* TsgcWSProtocol_AMQP_Client_Base.Create */ inline __fastcall virtual TsgcWSPClient_AMQP(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_amqp_client::TsgcWSProtocol_AMQP_Client(aOwner) { }
	/* TsgcWSProtocol_AMQP_Client_Base.Destroy */ inline __fastcall virtual ~TsgcWSPClient_AMQP() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocols */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOLS)
using namespace Sgcwebsocket_protocols;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocolsHPP
