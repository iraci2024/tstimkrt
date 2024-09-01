{ ***************************************************************************
  sgcIoT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcIoT_Amazon_MQTT_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_IOT}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes, sgcIoT_MQTT_Client;

type

  TawsIoTQoS = (awsIoTQoS0, awsIoTQoS1);

  TsgcIoT_Amazon_MQTT_Client_Options = class(TPersistent)
  private
    FClientId: String;
    FEndpoint: String;
    FPort: Integer;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property ClientId: String read FClientId write FClientId;
    property Endpoint: String read FEndpoint write FEndpoint;
    property Port: Integer read FPort write FPort;
  end;

  TsgcIoT_Amazon_MQTT_Signature_V4_Options = class(TPersistent)
  private
    FAccessKey: String;
    FEnabled: Boolean;
    FRegion: String;
    FSecretKey: String;
    FSessionToken: string;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AccessKey: String read FAccessKey write FAccessKey;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Region: String read FRegion write FRegion;
    property SecretKey: String read FSecretKey write FSecretKey;
    property SessionToken: string read FSessionToken write FSessionToken;
  end;

  TsgcIoT_Amazon_MQTT_Custom_Authentication_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FHeaders: TStringList;
    FParameters: String;
    FWebSockets: Boolean;
    procedure SetHeaders(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Headers: TStringList read FHeaders write SetHeaders;
    property Parameters: String read FParameters write FParameters;
    property WebSockets: Boolean read FWebSockets write FWebSockets;
  end;

  TsgcIoT_Amazon_MQTT_Client = class(TsgcIoTMQTT_Client)
    { from TsgcIoTComponent_Client }
  protected
    procedure DoBeforeConnect; override;
    { from TsgcIoTComponent_Client }

    { from TsgcIoTMQTT_Client }
  protected
    procedure OnMQTTBeforeConnectEvent(Connection: TsgcWSConnection;
      var aCleanSession: Boolean; var aClientIdentifier: String); override;
    { from TsgcIoTMQTT_Client }

    { properties }
  private
    FAmazon: TsgcIoT_Amazon_MQTT_Client_Options;
    function GetAmazon: TsgcIoT_Amazon_MQTT_Client_Options;
    procedure SetAmazon(const Value: TsgcIoT_Amazon_MQTT_Client_Options);
  public
    property Amazon: TsgcIoT_Amazon_MQTT_Client_Options read GetAmazon
      write SetAmazon;
    { properties }

    { signature v4 }
  private
    FSignatureV4: TsgcIoT_Amazon_MQTT_Signature_V4_Options;
    procedure SetSignatureV4(const Value
      : TsgcIoT_Amazon_MQTT_Signature_V4_Options);
    function GetSignatureV4QueryString: String;
  public
    property SignatureV4: TsgcIoT_Amazon_MQTT_Signature_V4_Options
      read FSignatureV4 write SetSignatureV4;
    { signature v4 }

    { custom authentication }
  private
    FCustomAuthentication: TsgcIoT_Amazon_MQTT_Custom_Authentication_Options;
    procedure SetCustomAuthentication(const Value
      : TsgcIoT_Amazon_MQTT_Custom_Authentication_Options);
  protected
    procedure OnHandshakeEvent(Connection: TsgcWSConnection;
      var Headers: TStringList); virtual;
  public
    property CustomAuthentication
      : TsgcIoT_Amazon_MQTT_Custom_Authentication_Options
      read FCustomAuthentication write SetCustomAuthentication;
    { custom authentication }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
    // ... subscribe client id events
  protected
    procedure DoSubscribe_ClientId(const aTopic, aClientId: String); virtual;
  public
    procedure Subscribe_ClientConnected(const aClientId: String);
    procedure Subscribe_ClientDisconnected(const aClientId: String);
    procedure Subscribe_ClientSubscribed(const aClientId: String);
    procedure Subscribe_ClientUnSubscribed(const aClientId: String);
    // ... publish rule event
  public
    procedure Publish_Rule(const aRuleName, aText: String);
    // ... subscribe thingname events
  protected
    procedure DoSubscribe_ThingName(const aTopic, aThingName: String); virtual;
    procedure DoPublish_ThingName(const aTopic, aThingName,
      aText: String); virtual;
  public
    procedure Publish_DeleteShadow(const aThingName, aText: String);
    procedure Subscribe_DeleteShadow(const aThingName: String);
    procedure Subscribe_ShadowDeleted(const aThingName: String);
    procedure Subscribe_ShadowRejected(const aThingName: String);
    procedure Publish_ShadowGet(const aThingName, aText: String);
    procedure Subscribe_ShadowGet(const aThingName: String);
    procedure Subscribe_ShadowGetAccepted(const aThingName: String);
    procedure Subscribe_ShadowGetRejected(const aThingName: String);
    procedure Publish_ShadowUpdate(const aThingName, aText: String);
    procedure Subscribe_ShadowUpdateAccepted(const aThingName: String);
    procedure Subscribe_ShadowUpdateRejected(const aThingName: String);
    procedure Subscribe_ShadowUpdateDelta(const aThingName: String);
    procedure Subscribe_ShadowUpdateDocuments(const aThingName: String);
    // ... general methods
  public
    procedure Subscribe(const aTopic: String; aQoS: TawsIoTQoS = awsIoTQoS0);
  public
    procedure Publish(const aTopic, aText: String;
      aQoS: TawsIoTQoS = awsIoTQoS0);
    { methods }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_IOT}

uses
  StrUtils,
{$IFDEF SGC_AWS}sgcHTTP_Amazon_AWS_Signature, {$ENDIF}
  sgcBase_Helpers, sgcWebSocket_Types;

const
  CS_IOT_AWS_SERVICE = 'iotdevicegateway';

const
  // AWS IoT publishes to this topic when an MQTT client with the specified client ID connects to AWS IoT
  CS_IOT_AWS_SUBSCRIBE_CONNECTED = '$aws/events/presence/connected/%s';
  // clientid
  // AWS IoT publishes to this topic when an MQTT client with the specified client ID disconnects to AWS IoT
  CS_IOT_AWS_SUBSCRIBE_DISCONNECTED = '$aws/events/presence/disconnected/%s';
  // clientid
  // AWS IoT publishes to this topic when an MQTT client with the specified client ID subscribes to an MQTT topic
  CS_IOT_AWS_SUBSCRIBE_SUBSCRIBED = '$aws/events/subscriptions/subscribed/%s';
  // clientid
  // AWS IoT publishes to this topic when an MQTT client with the specified client ID unsubscribes to an MQTT topic
  CS_IOT_AWS_SUBSCRIBE_UNSUBSCRIBED =
    '$aws/events/subscriptions/unsubscribed/%s'; // clientid
  // A device or an application publishes to this topic to trigger rules directly
  CS_IOT_AWS_PUBLISH_RULE = '$aws/rules/%s'; // rulename
  // A device or an application publishes to this topic to delete a shadow
  CS_IOT_AWS_PUBSUB_DELETE_SHADOW = '$aws/things/%s/shadow/delete'; // thingname
  // The Device Shadow service sends messages to this topic when a shadow is deleted
  CS_IOT_AWS_SUBSCRIBE_SHADOW_DELETED = '$aws/things/%s/shadow/delete/accepted';
  // thingname
  // The Device Shadow service sends messages to this topic when a request to delete a shadow is rejected
  CS_IOT_AWS_SUBSCRIBE_SHADOW_REJECTED =
    '$aws/things/%s/shadow/delete/rejected'; // thingname
  // An application or a thing publishes an empty message to this topic to get a shadow
  CS_IOT_AWS_PUBSUB_SHADOW_GET = '$aws/things/%s/shadow/get'; // thingname
  // The Device Shadow service sends messages to this topic when a request for a shadow is made successfully
  CS_IOT_AWS_SUBSCRIBE_SHADOW_GET_ACCEPTED =
    '$aws/things/%s/shadow/get/accepted'; // thingname
  // The Device Shadow service sends messages to this topic when a request for a shadow is rejected
  CS_IOT_AWS_SUBSCRIBE_SHADOW_GET_REJECTED =
    '$aws/things/%s/shadow/get/rejected'; // thingname
  // A thing or application publishes to this topic to update a shadow
  CS_IOT_AWS_PUBSUB_SHADOW_UPDATE = '$aws/things/%s/shadow/update'; // thingname
  // The Device Shadow service sends messages to this topic when an update is successfully made to a shadow
  CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_ACCEPTED =
    '$aws/things/%s/shadow/update/accepted'; // thingname
  // The Device Shadow service sends messages to this topic when an update to a shadow is rejected
  CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_REJECTED =
    '$aws/things/%s/shadow/update/rejected'; // thingname
  // The Device Shadow service sends messages to this topic when a difference is detected between the reported and desired sections of a shadow
  CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_DELTA =
    '$aws/things/%s/shadow/update/delta'; // thingname
  // AWS IoT publishes a state document to this topic whenever an update to the shadow is successfully performed
  CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_DOCUMENTS =
    '$aws/things/%s/shadow/update/documents'; // thingname

constructor TsgcIoT_Amazon_MQTT_Client.Create(aOwner: TComponent);
begin
  inherited;
  FAmazon := TsgcIoT_Amazon_MQTT_Client_Options.Create;
  FSignatureV4 := TsgcIoT_Amazon_MQTT_Signature_V4_Options.Create;
  FSignatureV4.Enabled := False;
  Certificate.Enabled := True;
  FCustomAuthentication :=
    TsgcIoT_Amazon_MQTT_Custom_Authentication_Options.Create;
  FCustomAuthentication.Enabled := False;
end;

destructor TsgcIoT_Amazon_MQTT_Client.Destroy;
begin
  sgcFree(FCustomAuthentication);
  sgcFree(FSignatureV4);
  sgcFree(FAmazon);
  inherited;
end;

procedure TsgcIoT_Amazon_MQTT_Client.DoPublish_ThingName(const aTopic,
  aThingName, aText: String);
begin
  DoPublish(Format(aTopic, [aThingName]), aText);
end;

procedure TsgcIoT_Amazon_MQTT_Client.DoSubscribe_ClientId(const aTopic,
  aClientId: String);
begin
  DoSubscribe(Format(aTopic, [aClientId]));
end;

procedure TsgcIoT_Amazon_MQTT_Client.DoSubscribe_ThingName(const aTopic,
  aThingName: String);
begin
  DoSubscribe(Format(aTopic, [aThingName]));
end;

function TsgcIoT_Amazon_MQTT_Client.GetAmazon
  : TsgcIoT_Amazon_MQTT_Client_Options;
begin
  Result := FAmazon;
end;

procedure TsgcIoT_Amazon_MQTT_Client.OnMQTTBeforeConnectEvent
  (Connection: TsgcWSConnection; var aCleanSession: Boolean;
  var aClientIdentifier: String);
begin
  inherited;
  if not Assigned(FOnMQTTBeforeConnect) then
  begin
    if Amazon.ClientId <> '' then
    begin
      aClientIdentifier := Amazon.ClientId;
      aCleanSession := False;
    end;
  end;
end;

procedure TsgcIoT_Amazon_MQTT_Client.Publish(const aTopic, aText: String;
  aQoS: TawsIoTQoS = awsIoTQoS0);
begin
  case aQoS of
    awsIoTQoS0:
      DoPublish(aTopic, aText, mtqsAtMostOnce);
    awsIoTQoS1:
      DoPublish(aTopic, aText, mtqsAtLeastOnce);
  end;
end;

procedure TsgcIoT_Amazon_MQTT_Client.Publish_DeleteShadow(const aThingName,
  aText: String);
begin
  DoPublish_ThingName(CS_IOT_AWS_PUBSUB_DELETE_SHADOW, aThingName, aText);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Publish_ShadowUpdate(const aThingName,
  aText: String);
begin
  DoPublish_ThingName(CS_IOT_AWS_PUBSUB_SHADOW_UPDATE, aThingName, aText);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Publish_ShadowGet(const aThingName,
  aText: String);
begin
  DoPublish_ThingName(CS_IOT_AWS_PUBSUB_SHADOW_GET, aThingName, aText);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Publish_Rule(const aRuleName,
  aText: String);
begin
  DoPublish(Format(CS_IOT_AWS_PUBLISH_RULE, [aRuleName]), aText);
end;

procedure TsgcIoT_Amazon_MQTT_Client.DoBeforeConnect;
{$IFDEF SGC_AWS}
var
  oSignatureV4: TsgcHTTP_Amazon_AWS_Signature_V4;
  vQueryString: String;
{$ENDIF}
begin
  inherited;
  TCPClient.Host := Amazon.Endpoint;
  TCPClient.Port := Amazon.Port;
{$IFDEF SGC_AWS}
  if SignatureV4.Enabled then
  begin
    TCPClient.Specifications.RFC6455 := True;
    TCPClient.Port := 443;

    oSignatureV4 := TsgcHTTP_Amazon_AWS_Signature_V4.Create(nil);
    Try
      oSignatureV4.Protocol := sv4pMQTT;
      oSignatureV4.SignatureV4Options.AccessKey := SignatureV4.AccessKey;
      oSignatureV4.SignatureV4Options.Region := SignatureV4.Region;
      oSignatureV4.SignatureV4Options.SecretKey := SignatureV4.SecretKey;
      oSignatureV4.SignatureV4Options.Service := CS_IOT_AWS_SERVICE;

      vQueryString := GetSignatureV4QueryString;
      oSignatureV4.SignGET('https://' + Amazon.Endpoint + '/mqtt?' +
        vQueryString);
      TCPClient.Options.Parameters := 'mqtt?' + vQueryString +
        '&X-Amz-Signature=' + oSignatureV4.Signature;
      if SignatureV4.SessionToken <> '' then
        TCPClient.Options.Parameters := TCPClient.Options.Parameters +
          '&X-Amz-Security-Token=' + URIEncode(SignatureV4.SessionToken);
    Finally
      sgcFree(oSignatureV4);
    End;
  end
  else
{$ENDIF}
    if Certificate.Enabled then
    begin
      TCPClient.Specifications.RFC6455 := False;
      // ... Clients that connect on port 443 with X.509 client certificate
      // ... authentication must implement ALPN
      if TCPClient.Port = 443 then
      begin
        TCPClient.TLSOptions.ALPNProtocols.Clear;
        TCPClient.TLSOptions.ALPNProtocols.Add('x-amzn-mqtt-ca');
      end;
    end
    // ... custom authentication
    else if CustomAuthentication.Enabled then
    begin
      TCPClient.Port := 443;
      TCPClient.Specifications.RFC6455 := CustomAuthentication.WebSockets;
      // ... Clients that connect over TCP
      // ... authentication must implement ALPN
      if TCPClient.Specifications.RFC6455 = False then
      begin
        TCPClient.TLSOptions.ALPNProtocols.Clear;
        TCPClient.TLSOptions.ALPNProtocols.Add('mqtt');
      end;
      TCPClient.Options.Parameters := CustomAuthentication.Parameters;
      if CustomAuthentication.Headers.Count > 0 then
        TCPClient.OnHandshake := OnHandshakeEvent;
    end;
end;

function TsgcIoT_Amazon_MQTT_Client.GetSignatureV4QueryString: String;
begin
  Result := 'X-Amz-Algorithm=' + CS_AWS_SIGV4_ALGORITHM + '&X-Amz-Credential=' +
    URIEncode(SignatureV4.AccessKey + '/' + LeftStr(FormatDateTime('yyyymmdd',
    Now), 8) + '/' + SignatureV4.Region + '/' + CS_IOT_AWS_SERVICE +
    '/aws4_request') + '&X-Amz-Date=' + sgcGetUTCString +
    '&X-Amz-SignedHeaders=host';
end;

procedure TsgcIoT_Amazon_MQTT_Client.OnHandshakeEvent
  (Connection: TsgcWSConnection; var Headers: TStringList);
var
  i: Integer;
begin
  if CustomAuthentication.Enabled then
  begin
    for i := 0 to CustomAuthentication.Headers.Count - 1 do
      Headers.Add(CustomAuthentication.Headers[i]);
  end;
end;

procedure TsgcIoT_Amazon_MQTT_Client.SetAmazon(const Value
  : TsgcIoT_Amazon_MQTT_Client_Options);
begin
  FAmazon.Assign(Value);
end;

procedure TsgcIoT_Amazon_MQTT_Client.SetCustomAuthentication
  (const Value: TsgcIoT_Amazon_MQTT_Custom_Authentication_Options);
begin
  if Assigned(FCustomAuthentication) then
    FCustomAuthentication.Assign(Value);
end;

procedure TsgcIoT_Amazon_MQTT_Client.SetSignatureV4
  (const Value: TsgcIoT_Amazon_MQTT_Signature_V4_Options);
begin
  if Assigned(FSignatureV4) then
    FSignatureV4.Assign(Value);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe(const aTopic: String;
  aQoS: TawsIoTQoS = awsIoTQoS0);
begin
  case aQoS of
    awsIoTQoS0:
      DoSubscribe(aTopic, mtqsAtMostOnce);
    awsIoTQoS1:
      DoSubscribe(aTopic, mtqsAtLeastOnce);
  end;
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ClientConnected
  (const aClientId: String);
begin
  DoSubscribe_ClientId(CS_IOT_AWS_SUBSCRIBE_CONNECTED, aClientId);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ClientDisconnected
  (const aClientId: String);
begin
  DoSubscribe_ClientId(CS_IOT_AWS_SUBSCRIBE_DISCONNECTED, aClientId);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ClientSubscribed
  (const aClientId: String);
begin
  DoSubscribe_ClientId(CS_IOT_AWS_SUBSCRIBE_SUBSCRIBED, aClientId);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ClientUnSubscribed
  (const aClientId: String);
begin
  DoSubscribe_ClientId(CS_IOT_AWS_SUBSCRIBE_UNSUBSCRIBED, aClientId);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_DeleteShadow
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_PUBSUB_DELETE_SHADOW, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowUpdateAccepted
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_ACCEPTED,
    aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowGet
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_PUBSUB_SHADOW_GET, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowDeleted
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_DELETED, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowGetAccepted
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_GET_ACCEPTED, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowGetRejected
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_GET_REJECTED, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowRejected
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_REJECTED, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowUpdateDelta
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_DELTA, aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowUpdateDocuments
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_DOCUMENTS,
    aThingName);
end;

procedure TsgcIoT_Amazon_MQTT_Client.Subscribe_ShadowUpdateRejected
  (const aThingName: String);
begin
  DoSubscribe_ThingName(CS_IOT_AWS_SUBSCRIBE_SHADOW_UPDATE_REJECTED,
    aThingName);
end;

constructor TsgcIoT_Amazon_MQTT_Client_Options.Create;
begin
  inherited;
  Port := 8883;
end;

procedure TsgcIoT_Amazon_MQTT_Client_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcIoT_Amazon_MQTT_Client_Options then
  begin
    ClientId := TsgcIoT_Amazon_MQTT_Client_Options(aSource).ClientId;
    Endpoint := TsgcIoT_Amazon_MQTT_Client_Options(aSource).Endpoint;
    Port := TsgcIoT_Amazon_MQTT_Client_Options(aSource).Port;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcIoT_Amazon_MQTT_Signature_V4_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_Amazon_AWS_Signature_V4_Options then
  begin
    Enabled := TsgcIoT_Amazon_MQTT_Signature_V4_Options(aSource).Enabled;
    Region := TsgcIoT_Amazon_MQTT_Signature_V4_Options(aSource).Region;
    AccessKey := TsgcIoT_Amazon_MQTT_Signature_V4_Options(aSource).AccessKey;
    SecretKey := TsgcIoT_Amazon_MQTT_Signature_V4_Options(aSource).SecretKey;
    SessionToken := TsgcIoT_Amazon_MQTT_Signature_V4_Options(aSource)
      .SessionToken;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcIoT_Amazon_MQTT_Custom_Authentication_Options.Create;
begin
  inherited;
  FHeaders := TStringList.Create;
  Enabled := False;
  WebSockets := True;
  Parameters := '/mqtt';
end;

destructor TsgcIoT_Amazon_MQTT_Custom_Authentication_Options.Destroy;
begin
  sgcFree(FHeaders);
  inherited;
end;

procedure TsgcIoT_Amazon_MQTT_Custom_Authentication_Options.Assign
  (aSource: TPersistent);
begin
  if aSource is TsgcIoT_Amazon_MQTT_Custom_Authentication_Options then
  begin
    Enabled := TsgcIoT_Amazon_MQTT_Custom_Authentication_Options
      (aSource).Enabled;
    Headers.Text := TsgcIoT_Amazon_MQTT_Custom_Authentication_Options(aSource)
      .Headers.Text;
    Parameters := TsgcIoT_Amazon_MQTT_Custom_Authentication_Options(aSource)
      .Parameters;
    WebSockets := TsgcIoT_Amazon_MQTT_Custom_Authentication_Options(aSource)
      .WebSockets;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcIoT_Amazon_MQTT_Custom_Authentication_Options.SetHeaders
  (const Value: TStringList);
begin
  if Assigned(FHeaders) then
    FHeaders.Assign(Value);
end;

{$ENDIF}

end.
