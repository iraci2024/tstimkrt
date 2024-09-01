{ ***************************************************************************
  sgcIoT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcIoT_Azure_MQTT_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_IOT}

uses
  Classes, SysUtils,
  // sgc
  sgcIoT_MQTT_Client, sgcWebSocket_Classes, sgcIoT_Client, sgcWebSocket_Types;

type
  TazuIoTQoS = (azuIoTQoS0, azuIoTQoS1);

  TsgcIoT_Azure_MQTT_Client_Options = class(TPersistent)
  private
    FDeviceId: String;
    FIoTHub: String;
    FModuleId: String;
    FWebSockets: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property DeviceId: String read FDeviceId write FDeviceId;
    property IoTHub: String read FIoTHub write FIoTHub;
    property ModuleId: String read FModuleId write FModuleId;
    property WebSockets: Boolean read FWebSockets write FWebSockets;
  end;

  TsgcIoT_Azure_MQTT_Client = class(TsgcIoTMQTT_Client)
  { from TsgcIoTComponent_Client }
  protected
    procedure OnMQTTBeforeConnectEvent(Connection: TsgcWSConnection; var
        aCleanSession: Boolean; var aClientIdentifier: String); override;
    procedure DoBeforeConnect; override;
  { from TsgcIoTComponent_Client }

  { helpers }
  private
    function GetPassword_SAS: string;
    function GetPassword_X509: string;
  private
    function GetTopicDeviceToCloud(const aProperties: TStrings = nil): String;
    function GetTopicCloudToDevice: String;
  private
    function GetTopicDeviceTwins: String;
    function GetTopicDeviceTwinsProperties(const aRequestId: string): String;
    function GetTopicDeviceTwinsUpdateProperties(const aRequestId: string): string;
  private
    function GetTopicDirectMethod: string;
    function GetTopicRespondPublicMethod(const aRequestId: string; aStatus:
        Integer): string;
  { helpers }

  { properties }
  private
    FAzure: TsgcIoT_Azure_MQTT_Client_Options;
    function GetAzure: TsgcIoT_Azure_MQTT_Client_Options;
    procedure SetAzure(const Value: TsgcIoT_Azure_MQTT_Client_Options);
  public
    property Azure: TsgcIoT_Azure_MQTT_Client_Options read GetAzure write SetAzure;
  { properties }

  { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor / destructor }

  { methods }
  // cloud to device
  public
    function Subscribe_CloudToDevice: Integer;
    function UnSubscribe_CloudToDevice: Integer;
  // device to cloud
  public
    function Send_DeviceToCloud(const aText: String; aQoS: TazuIoTQoS =
        azuIoTQoS0): Integer; overload;
    function Send_DeviceToCloud(const aText: String; const aProperties: TStrings;
        aQoS: TazuIoTQoS = azuIoTQoS0): Integer; overload;
    function SendAndWait_DeviceToCloud(const aText: String; const aTimeout: integer
        = 10000): Boolean; overload;
    function SendAndWait_DeviceToCloud(const aText: String; const aProperties:
        TStrings; const aTimeout: integer = 10000): Boolean; overload;
  // device twins
  public
    function Subscribe_DeviceTwins: Integer;
    function UnSubscribe_DeviceTwins: Integer;
  public
    procedure Get_DeviceTwinsProperties(const aRequestId: string; aQoS: TazuIoTQoS
        = azuIoTQoS0);
    function GetAndWait_DeviceTwinsProperties(const aRequestId: string; const
        aTimeout: integer = 10000): Boolean;
  public
    function Set_DeviceTwinsProperties(const aRequestId: string; aText: String;
        aQoS: TazuIoTQoS = azuIoTQoS0): Integer;
    function SetAndWait_DeviceTwinsProperties(const aRequestId: string; aText:
        String; const aTimeout: integer = 10000): Boolean;
  // direct methods
  public
    function Subscribe_DirectMethod: Integer;
    function UnSubscribe_DirectMethod: Integer;
  public
    function RespondPublicMethod(const aRequestId: string; aStatus: Integer; const
        aText: String; aQoS: TazuIoTQoS = azuIoTQoS0): Integer;
    function RespondAndWaitPublicMethod(const aRequestId: string; aStatus: Integer;
        const aText: String; const aTimeout: Integer = 10000): Boolean;
  { methods }
  end;


{$ENDIF}


implementation

{$IFDEF SGC_IOT}

uses
  DateUtils,
  // indy
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcHTTP_Helpers, sgcBase_Helpers, sgcWebSocket_Helpers;

const
  CS_AZURE_API_VERSION = '/api-version=2016-11-14';
  CS_PARAMETERS_WEBSOCKETS = '$iothub/websocket';
  CS_PASSWORD_SAS = 'SharedAccessSignature sr=%s&sig=%s&se=%s';
  CS_PASSWORD_X509 = 'HostName=%s;DeviceId=%s;x509=true';
  CS_TOPIC_DEVICE_TO_CLOUD = 'devices/%s/messages/events/';
  CS_TOPIC_DEVICE_MODULE_TO_CLOUD = 'devices/%s/modules/%s/messages/events/';
  CS_TOPIC_DEVICE_TWINS = '$iothub/twin/res/#';
  CS_TOPIC_DEVICE_TWINS_PROPS = '$iothub/twin/GET/?$rid=%s';
  CS_TOPIC_CLOUD_TO_DEVICE = 'devices/%s/messages/devicebound/#';
  CS_TOPIC_DEVICE_TWINS_UPDATE_REPORTED_PROPS = '$iothub/twin/PATCH/properties/reported/?$rid=%s';
  CS_TOPIC_DIRECT_METHOD = '$iothub/methods/POST/#';
  CS_TOPIC_RESPOND_DIRECT_METHOD = '$iothub/methods/res/%d/?$rid=%s';

procedure TsgcIoT_Azure_MQTT_Client_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcIoT_Azure_MQTT_Client_Options then
  begin
    DeviceId := TsgcIoT_Azure_MQTT_Client_Options(aSource).DeviceId;
    ModuleId := TsgcIoT_Azure_MQTT_Client_Options(aSource).ModuleId;
    IoTHub := TsgcIoT_Azure_MQTT_Client_Options(aSource).IoTHub;
    WebSockets := TsgcIoT_Azure_MQTT_Client_Options(aSource).WebSockets;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcIoT_Azure_MQTT_Client.Create(aOwner: TComponent);
begin
  inherited;
  FAzure := TsgcIoT_Azure_MQTT_Client_Options.Create;
  SAS.Enabled := True;
end;

destructor TsgcIoT_Azure_MQTT_Client.Destroy;
begin
  sgcFree(FAzure);
  inherited;
end;

function TsgcIoT_Azure_MQTT_Client.GetAzure: TsgcIoT_Azure_MQTT_Client_Options;
begin
  Result := FAzure;
end;

function TsgcIoT_Azure_MQTT_Client.GetPassword_SAS: string;
var
  vTimeStamp: string;
  vURIEncode: String;
  oStream: TsgcStringStream;
  i: Integer;
  vBytes: TIdBytes;
begin
  if SAS.SecretKey = '' then
    raise Exception.Create('If SAS is enabled, SecretKey cannot be empty.');

  vTimeStamp := GetDateTimeUnix(IncMinute(Now, SAS.Expiry), False); // set to false, because input is NOT UTC
  vURIEncode := Azure.IoTHub + '/devices/' + Azure.DeviceId;
  if Azure.ModuleId <> '' then
    vURIEncode := vURIEncode + '/modules/' + Azure.ModuleId;
  vURIEncode := URIEncode(vURIEncode);
  oStream := TsgcStringStream.Create('');
  Try
    DecodeBase64(SAS.SecretKey, TMemoryStream(oStream));
    SetLength(vBytes, oStream.Size);
    for i := 0 to oStream.Size do
      vBytes[i] := oStream.Bytes[i];

    Result := Format(CS_PASSWORD_SAS,[
      vURIEncode,
      URIEncode(GetHMACSHA256(vURIEncode + chr(10) + vTimeStamp, vBytes, True)),
      vTimeStamp]);
  Finally
    oStream.Free;
  End;
end;

procedure TsgcIoT_Azure_MQTT_Client.OnMQTTBeforeConnectEvent(Connection:
    TsgcWSConnection; var aCleanSession: Boolean; var aClientIdentifier:
    String);
begin
  inherited;
  if not Assigned(FOnMQTTBeforeConnect) then
  begin
    if Azure.DeviceId <> '' then
    begin
      aClientIdentifier := Azure.DeviceId;
      if Azure.ModuleId <> '' then
        aClientIdentifier := aClientIdentifier + '/' + Azure.ModuleId;
      aCleanSession := True;
    end;
  end;
end;

procedure TsgcIoT_Azure_MQTT_Client.DoBeforeConnect;
begin
  inherited;
  TCPClient.Host := Azure.IoTHub;
  if Azure.WebSockets then
  begin
    TCPClient.Specifications.RFC6455 := True;
    TCPClient.Port := 443;
    TCPClient.Options.Parameters := CS_PARAMETERS_WEBSOCKETS
  end
  else
  begin
    TCPClient.Specifications.RFC6455 := False;
    TCPClient.Port := 8883;
    TCPClient.Options.Parameters := '';
  end;

  MQTT.Authentication.Enabled := True;
  MQTT.Authentication.UserName := Azure.IoTHub + '/' + Azure.DeviceId;
  if Azure.ModuleId <> '' then
    MQTT.Authentication.UserName := MQTT.Authentication.UserName + '/' + Azure.ModuleId;
  MQTT.Authentication.UserName := MQTT.Authentication.UserName + CS_AZURE_API_VERSION; // required for device twins
  if SAS.Enabled then
    MQTT.Authentication.Password := GetPassword_SAS
  else
    MQTT.Authentication.Password := GetPassword_X509;
end;

function TsgcIoT_Azure_MQTT_Client.GetAndWait_DeviceTwinsProperties(const
    aRequestId: string; const aTimeout: integer = 10000): Boolean;
begin
  result := DoPublishAndWait(GetTopicDeviceTwinsProperties(aRequestId), '', mtqsAtLeastOnce, False, aTimeout);
end;

function TsgcIoT_Azure_MQTT_Client.GetPassword_X509: string;
begin
  Result := Format(CS_PASSWORD_X509, [aZURE.IoTHub, Azure.DeviceId]);
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicCloudToDevice: String;
begin
  result := Format(CS_TOPIC_CLOUD_TO_DEVICE, [Azure.DeviceId]);
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicDeviceToCloud(const aProperties:
    TStrings = nil): String;
var
  i: Integer;
  vName: string;
  vProperties: string;
  vValue: string;
begin
  if Azure.ModuleId <> '' then
    result := Format(CS_TOPIC_DEVICE_MODULE_TO_CLOUD, [Azure.DeviceId, Azure.ModuleId])
  else
    result := Format(CS_TOPIC_DEVICE_TO_CLOUD, [Azure.DeviceId]);
  if Assigned(aProperties) then
  begin
    vProperties := '';
    for i := 0 to aProperties.count - 1 do
    begin
      vName := sgcPathEncode(aProperties.Names[i]);
      vValue := sgcPathEncode(aProperties.ValueFromIndex[i]);
      if (vName <> '') then
      begin
        if vProperties <> '' then
          vProperties := vProperties + '&';
        if vValue = 'null' then
          vProperties := vProperties + vName
        else
          vProperties := vProperties + vName + '=' + vValue;
      end;
    end;
    if vProperties <> '' then
      result :=  result + vProperties;
  end;
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicDeviceTwins: String;
begin
  result := CS_TOPIC_DEVICE_TWINS;
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicDeviceTwinsProperties(const
    aRequestId: string): String;
begin
  Result := Format(CS_TOPIC_DEVICE_TWINS_PROPS, [aRequestId]);
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicDeviceTwinsUpdateProperties(const
    aRequestId: string): string;
begin
  Result := Format(CS_TOPIC_DEVICE_TWINS_UPDATE_REPORTED_PROPS, [aRequestId]);
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicDirectMethod: string;
begin
  result := CS_TOPIC_DIRECT_METHOD;
end;

function TsgcIoT_Azure_MQTT_Client.GetTopicRespondPublicMethod(const
    aRequestId: string; aStatus: Integer): string;
begin
  Result := Format(CS_TOPIC_RESPOND_DIRECT_METHOD, [aStatus, aRequestId]);
end;

procedure TsgcIoT_Azure_MQTT_Client.Get_DeviceTwinsProperties(const aRequestId:
    string; aQoS: TazuIoTQoS = azuIoTQoS0);
begin
  case aQoS of
    azuIoTQoS0: DoPublish(GetTopicDeviceTwinsProperties(aRequestId), '', mtqsAtMostOnce);
    azuIoTQoS1: DoPublish(GetTopicDeviceTwinsProperties(aRequestId), '', mtqsAtLeastOnce);
  end;
end;

function TsgcIoT_Azure_MQTT_Client.RespondAndWaitPublicMethod(const aRequestId:
    string; aStatus: Integer; const aText: String; const aTimeout: Integer =
    10000): Boolean;
begin
  result := DoPublishAndWait(GetTopicRespondPublicMethod(aRequestId, aStatus), aText, mtqsAtLeastOnce, False, aTimeout);
end;

function TsgcIoT_Azure_MQTT_Client.RespondPublicMethod(const aRequestId:
    string; aStatus: Integer; const aText: String; aQoS: TazuIoTQoS =
    azuIoTQoS0): Integer;
begin
  result := 0;

  case aQoS of
    azuIoTQoS0: result := DoPublish(GetTopicRespondPublicMethod(aRequestId, aStatus), aText, mtqsAtMostOnce);
    azuIoTQoS1: result := DoPublish(GetTopicRespondPublicMethod(aRequestId, aStatus), aText, mtqsAtLeastOnce);
  end;
end;

function TsgcIoT_Azure_MQTT_Client.SendAndWait_DeviceToCloud(const aText:
    String; const aTimeout: integer = 10000): Boolean;
begin
  result := DoPublishAndWait(GetTopicDeviceToCloud, aText, mtqsAtLeastOnce, False, aTimeout);
end;

function TsgcIoT_Azure_MQTT_Client.SendAndWait_DeviceToCloud(const aText:
    String; const aProperties: TStrings; const aTimeout: integer = 10000):
    Boolean;
begin
  Result := DoPublishAndWait(GetTopicDeviceToCloud(aProperties), aText, mtqsAtLeastOnce, False, aTimeout);
end;

function TsgcIoT_Azure_MQTT_Client.Send_DeviceToCloud(const aText: String;
    aQoS: TazuIoTQoS = azuIoTQoS0): Integer;
begin
  result := 0;

  case aQoS of
    azuIoTQoS0: result := DoPublish(GetTopicDeviceToCloud, aText, mtqsAtMostOnce);
    azuIoTQoS1: result := DoPublish(GetTopicDeviceToCloud, aText, mtqsAtLeastOnce);
  end;
end;

function TsgcIoT_Azure_MQTT_Client.Send_DeviceToCloud(const aText: String;
    const aProperties: TStrings; aQoS: TazuIoTQoS = azuIoTQoS0): Integer;
begin
  result := 0;

  case aQoS of
    azuIoTQoS0: result := DoPublish(GetTopicDeviceToCloud(aProperties), aText, mtqsAtMostOnce);
    azuIoTQoS1: result := DoPublish(GetTopicDeviceToCloud(aProperties), aText, mtqsAtLeastOnce);
  end;
end;

function TsgcIoT_Azure_MQTT_Client.SetAndWait_DeviceTwinsProperties(const
    aRequestId: string; aText: String; const aTimeout: integer = 10000):
    Boolean;
begin
  result := DoPublishAndWait(GetTopicDeviceTwinsUpdateProperties(aRequestId), aText, mtqsAtLeastOnce, False, aTimeout);
end;

procedure TsgcIoT_Azure_MQTT_Client.SetAzure(const Value:
    TsgcIoT_Azure_MQTT_Client_Options);
begin
  FAzure.Assign(Value);
end;

function TsgcIoT_Azure_MQTT_Client.Set_DeviceTwinsProperties(const aRequestId:
    string; aText: String; aQoS: TazuIoTQoS = azuIoTQoS0): Integer;
begin
  result := 0;

  case aQoS of
    azuIoTQoS0: result := DoPublish(GetTopicDeviceTwinsUpdateProperties(aRequestId), aText, mtqsAtMostOnce);
    azuIoTQoS1: result := DoPublish(GetTopicDeviceTwinsUpdateProperties(aRequestId), aText, mtqsAtLeastOnce);
  end;
end;

function TsgcIoT_Azure_MQTT_Client.Subscribe_CloudToDevice: Integer;
begin
  result := DoSubscribe(GetTopicCloudToDevice, mtqsAtMostOnce);
end;

function TsgcIoT_Azure_MQTT_Client.Subscribe_DeviceTwins: Integer;
begin
  result := DoSubscribe(GetTopicDeviceTwins, mtqsAtMostOnce);
end;

function TsgcIoT_Azure_MQTT_Client.Subscribe_DirectMethod: Integer;
begin
  result := DoSubscribe(GetTopicDirectMethod, mtqsAtMostOnce);
end;

function TsgcIoT_Azure_MQTT_Client.UnSubscribe_CloudToDevice: Integer;
begin
  result := DoUnSubscribe(GetTopicCloudToDevice);
end;

function TsgcIoT_Azure_MQTT_Client.UnSubscribe_DeviceTwins: Integer;
begin
  result := DoUnSubscribe(GetTopicDeviceTwins);
end;

function TsgcIoT_Azure_MQTT_Client.UnSubscribe_DirectMethod: Integer;
begin
  result := DoUnSubscribe(GetTopicDirectMethod);
end;

{$ENDIF}

end.
