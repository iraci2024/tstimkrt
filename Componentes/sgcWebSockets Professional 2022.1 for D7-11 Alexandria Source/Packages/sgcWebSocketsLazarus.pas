{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sgcWebSocketsLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  sgcJSON, sgcWebSocket, sgcWebSocket_Classes, sgcWebSocket_Client, 
   sgcWebSocket_Const, sgcWebSocket_CustomClient, 
  sgcWebSocket_CustomServer, sgcWebSocket_Extension_Base, 
  sgcWebSocket_Extension_DeflateFrame, sgcWebSocket_Extensions, 
  sgcWebSocket_Helpers, sgcWebSocket_HTTPResponse, 
  sgcWebSocket_Protocol_Base_Client, sgcWebSocket_Protocol_Base_Message, 
  sgcWebSocket_Protocol_Base_Server, sgcWebSocket_Protocol_Dataset_Client, 
  sgcWebSocket_Protocol_Dataset_Message, sgcWebSocket_Protocol_Dataset_Server, 
  sgcWebSocket_Protocol_sgc_Client, sgcWebSocket_Protocol_sgc_Message, 
  sgcWebSocket_Protocol_sgc_Server, sgcWebSocket_Protocol_WAMP_Client, 
  sgcWebSocket_Protocol_WAMP_Message, sgcWebSocket_Protocol_WAMP_Server, 
  sgcWebSocket_Protocol_WebRTC_Message, sgcWebSocket_Protocol_WebRTC_Server, 
  sgcWebSocket_Protocols, sgcWebSocket_Resources, sgcWebSocket_Server, 
  sgcWebSocket_Types, sgcWebSocket_Protocol_Broker_Client, 
  sgcWebSocket_Protocol_Broker_Server, sgcWebSocket_Protocol_Broker_Message, 
  sgcWebSocket_Protocols_Reg, sgcWebSocket_Reg, 
  sgcWebSocket_Extension_PerMessage_Deflate, sgcWebSocket_Server_Proxy, 
  sgcWebSocket_Protocol_Files_Server, sgcWebSocket_Protocol_Files_Client, 
  sgcWebSocket_Protocol_Files_Message, sgcWebSocket_LoadBalancer_Client, 
  sgcWebSocket_LoadBalancer_Message, sgcWebSocket_LoadBalancer_Server, 
  sgcWebSocket_Protocol_MQTT_Client, sgcWebSocket_Protocol_MQTT_Message, 
  sgcWebSocket_API_Bitfinex, sgcWebSocket_API_Blockchain, 
  sgcWebSocket_API_Pusher, sgcWebSocket_APIs, sgcWebSocket_APIs_Reg, sgcSTOMP, 
  sgcWebSocket_Protocol_STOMP_ActiveMQ_Client, 
  sgcWebSocket_Protocol_STOMP_Broker_Client, 
  sgcWebSocket_Protocol_STOMP_Client, sgcWebSocket_Protocol_STOMP_Message, 
  sgcWebSocket_Protocol_STOMP_RabbitMQ_Client, sgcWebSocket_API_SignalR, 
  sgcWebSocket_API_Bittrex, sgcWebSocket_API_Binance, 
  sgcWebSocket_API_Bitstamp, sgcWebSocket_Classes_Sockets, 
  sgcWebSocket_Server_Base, sgcHTTP, sgcTCP_Classes_Indy, 
    
    
  sgcHTTP_Const,  
   sgcWebSocket_API_Huobi, 
   sgcHTTP_OAuth_Client, sgcHTTP_OAuth2_Client, sgcHTTP_Helpers, 
  sgcWebSocket_API_Cex, sgcWebSocket_Protocol_AppRTC_Server, 
  sgcWebSocket_Protocol_Presence_Client, 
  sgcWebSocket_Protocol_Presence_Message, 
  sgcWebSocket_Protocol_Presence_Server, sgcWebSocket_API_Bitmex, 
  sgcWebSocket_Classes_SyncObjs, sgcWebSocket_Classes_Queues, 
  sgcWebSocket_API_SignalRCore, sgcWebSocket_API_Kraken, sgcBase_Classes, 
  sgcBase_Const, sgcBase_Helpers, sgcIoT, sgcIoT_Classes,  
  sgcHTTP_API_Coinbase, sgcIoT_Client, sgcIoT_MQTT_Client, 
  sgcIoT_Amazon_MQTT_Client, sgcIoT_Azure_MQTT_Client, 
  sgcWebSocket_API_Coinbase, sgcIoT_Reg, sgcTCP_Classes, sgcBase_Sync, 
   sgcTCP_Client,  sgcWebSocket_API_FXCM, 
  sgcSSL_SChannel, sgcSSL_SChannel_Indy, sgcSSL_WinSSPI, sgcHTTP_Classes, 
  sgcHTTP_Client, sgcHTTP_Reg, sgcHTTP_Amazon_AWS, 
  sgcHTTP_Amazon_SQS, sgcHTTP_Amazon_AWS_Signature, sgcWebSocket_API_Discord, 
  sgcHTTP_API_Binance, sgcHTTP_API, sgcHTTP_Google_Cloud, 
  sgcHTTP_Google_PubSub, sgcHTTP_Google_Calendar, sgcHTTP_API_Kraken, 
  sgcLib_Telegram, sgcLib_Telegram_Client, 
    
    
  sgcHTTP_JWT_Classes, sgcHTTP_JWT_Client, sgcHTTP_JWT_RSA,
  sgcHTTP_JWT_Helpers,  sgcHTTP_JWT_Types,
  sgcP2P_STUN_Classes, sgcP2P_STUN_Client, sgcP2P_STUN_Helpers, sgcP2P_STUN_Server, sgcP2P_STUN_Types,
      
  sgcWebSocket_API_FTX, sgcHTTP_API_FTX, sgcLib_RCON_Client, sgcHTTP_API_Cryptohopper, sgcLibs,
  sgcWebSocket_Server_API_RTCMultiConnection, sgcWebSocket_Server_API_Base,
  sgcWebSocket_Server_API_SocketIO, sgcWebSocket_Server_APIs,
  sgcAMQP, sgcAMQP_Classes, sgcAMQP_Client, sgcAMQP_Const, sgcAMQP_Helpers, sgcHTTP_API_Bitmex,
    
  sgcWebSocket_Protocol_AMQP_Client, sgcWebSocket_Protocol_AMQP_Message,
  sgcWebSocket_Protocol_AMQP_RabbitMQ_Client,
   sgcP2P, sgcP2P_Reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('sgcWebSocket_Protocols_Reg', 
    @sgcWebSocket_Protocols_Reg.Register);
  RegisterUnit('sgcWebSocket_Reg', @sgcWebSocket_Reg.Register);
  RegisterUnit('sgcWebSocket_APIs_Reg', @sgcWebSocket_APIs_Reg.Register);
  RegisterUnit('sgcIoT_Reg', @sgcIoT_Reg.Register);
  RegisterUnit('sgcP2P_Reg', @sgcP2P_Reg.Register);
  RegisterUnit('sgcHTTP_Reg', @sgcHTTP_Reg.Register);
end;

initialization
  RegisterPackage('sgcWebSocketsLazarus', @Register);
end.
