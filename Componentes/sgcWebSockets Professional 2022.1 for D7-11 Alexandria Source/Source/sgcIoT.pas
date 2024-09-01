{***************************************************************************
 sgcIoT component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}
unit sgcIoT;
interface
{$I sgcVer.inc}
{$IFDEF SGC_IOT}
uses
  Classes,
  // sgc
  sgcIoT_Amazon_MQTT_Client, sgcIoT_Azure_MQTT_Client;
type
  TsgcIoTAmazon_MQTT_Client = class(TsgcIoT_Amazon_MQTT_Client)
  published
    property Active;
    property Certificate;
    property Amazon;
    property MQTTHeartBeat;
    property MQTTAuthentication;
    property WatchDog;
    property LogFile;
    property SignatureV4;
    property CustomAuthentication;
    property OnConnect;
    property OnDisconnect;
    property OnException;
    property OnError;
    property OnMQTTBeforeConnect;
    property OnMQTTConnect;
    property OnMQTTPublish;
    property OnMQTTPubAck;
    property OnMQTTSubscribe;
    property OnMQTTUnSubscribe;
    property OnMQTTDisconnect;
    property Version;
  end;
  TsgcIoTAzure_MQTT_Client = class(TsgcIoT_Azure_MQTT_Client)
  published
    property Active;
    property Certificate;
    property SAS;
    property Azure;
    property MQTTHeartBeat;
    property WatchDog;
    property LogFile;
    property OnConnect;
    property OnDisconnect;
    property OnException;
    property OnError;
    property OnMQTTBeforeConnect;
    property OnMQTTConnect;
    property OnMQTTPublish;
    property OnMQTTPubAck;
    property OnMQTTSubscribe;
    property OnMQTTUnSubscribe;
    property OnMQTTDisconnect;
    property Version;
  end;
{$ENDIF}


implementation

end.
