{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_WebRTC_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  // sgc
  sgcJSON, sgcWebSocket_Protocol_WebRTC_Message,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Server,
  sgcWebSocket_HTTPResponse;

type

  TsgcWSWebRTC_Options = class(TPersistent)
  private
    FIceServers: TStringList;
    procedure SetIceServers(const Value: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property IceServers: TStringList read FIceServers write SetIceServers;
  end;


  TsgcWSProtocol_WebRTC_Server = class(TsgcWSProtocol_Subscription_Server_Base)
    { WSMessageWebRTC }
  private
    FWSMessageWebRTC: TsgcWSMessageWebRTC;
    function GetWSMessageWebRTC: TsgcWSMessageWebRTC;
  protected
    property WSMessageWebRTC: TsgcWSMessageWebRTC read GetWSMessageWebRTC write
      FWSMessageWebRTC;
    { WSMessageWebRTC }

    { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
      override;
    { from TsgcWSComponent }

    { webrtc subscriptions }
  private
    FWebRTCSubscriptions: TStringList;
  protected
    procedure WebRTCSubscription_Add(aSubscription: String);
    procedure WebRTCSubscription_Clear(aSubscription: String);
    function WebRTCSubscription_Count(aSubscription: String): Integer;
  protected
    function GetWebRTCSubscriptions: TStringList;
  public
    property WebRTCSubscriptions: TStringList read GetWebRTCSubscriptions write
      FWebRTCSubscriptions;
    { webrtc subscriptions }

    { properties }
  private
    FWebRTC: TsgcWSWebRTC_Options;
    procedure SetWebRTC(const Value: TsgcWSWebRTC_Options);
  public
    property WebRTC: TsgcWSWebRTC_Options read FWebRTC write SetWebRTC;
    { properties }


  protected
    procedure DoBroadCast(aMessage: TsgcWSMessageWebRTC; aChannel: String; Exclude:
        String = ''); virtual;
    procedure BroadcastWebRTC(aMessage: string; aChannel: string = ''; aGuid:
        String = ''); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { from TsgcWSComponent_Server }
  public
    procedure Broadcast(aMessage: string; aChannel: string = ''; Exclude: String =
        ''; Include: String = ''); override;
    function WriteData(aGuid, aMessage: string): Boolean; override;
    { from TsgcWSComponent_Server }
  end;

  TsgcWSProtocol_JS_WebRTC = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_HTML_WebRTC = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;


{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcWebSocket_Resources,
  sgcWebSocket_Server;

constructor TsgcWSProtocol_WebRTC_Server.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_WEBRTC;
  FWebRTC := TsgcWSWebRTC_Options.Create;
end;

destructor TsgcWSProtocol_WebRTC_Server.Destroy;
begin
  sgcFree(FWebRTCSubscriptions);
  sgcFree(FWebRTC);
  inherited;
end;

procedure TsgcWSProtocol_WebRTC_Server.Broadcast(aMessage: string; aChannel:
    string = ''; Exclude: String = ''; Include: String = '');
begin
  WSMessageWebRTC.DoEnterWrite;
  Try
    WSMessageWebRTC.Text := aMessage;
    WSMessageWebRTC.Method := CS_SGC_BROADCAST;
    WSMessageWebRTC.Channel := aChannel;
    WSMessageWebRTC.Guid := Guid;
    WSMessageWebRTC.WebRTC := '';
    DoBroadCast(WSMessageWebRTC, aChannel, Exclude);
  Finally
    WSMessageWebRTC.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WebRTC_Server.BroadcastWebRTC(aMessage: string;
    aChannel: string = ''; aGuid: String = '');
begin
  WSMessageWebRTC.DoEnterWrite;
  Try
    WSMessageWebRTC.Text := '';
    WSMessageWebRTC.Method := CS_SGC_WEBRTC;
    WSMessageWebRTC.Channel := aChannel;
    WSMessageWebRTC.Guid := Guid;
    WSMessageWebRTC.WebRTC := aMessage;
    DoBroadCast(WSMessageWebRTC, aChannel, aGuid);
  Finally
    WSMessageWebRTC.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WebRTC_Server.DoBroadCast(aMessage:
    TsgcWSMessageWebRTC; aChannel: String; Exclude: String = '');
var
  vChannel: string;
begin
  if aChannel <> '' then
  begin
    vChannel := Guid + '_' + aChannel;
    inherited Broadcast(aMessage.Write, vChannel, Exclude)
  end
  else
    inherited Broadcast(aMessage.Write, '', Exclude);
end;

procedure TsgcWSProtocol_WebRTC_Server.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  inherited;
  if WebRTC.IceServers.Text <> '' then
    aConnection.WriteData('{"method":"sgc@iceservers", "iceservers":' + WebRTC.IceServers.Text + '}');
end;

procedure TsgcWSProtocol_WebRTC_Server.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  inherited;
  // ... Unsubscribe
  WebRTCSubscription_Clear(Guid + '_' + aConnection.LastSubscription);
end;

procedure TsgcWSProtocol_WebRTC_Server.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  vAccept: Boolean;
begin
  if DoRawMessage(aConnection, Text) then
    exit;

  WSMessageWebRTC.Read(Text);
  WSMessageWebRTC.params := '';

  if WSMessageWebRTC.Method = CS_SGC_BROADCAST then
  begin
    WSMessageWebRTC.result := CS_SGC_BROADCAST;
    Broadcast(WSMessageWebRTC.Text, WSMessageWebRTC.Channel)
  end
  else if WSMessageWebRTC.Method = CS_SGC_PROTOCOL then
    aConnection.WriteData(aConnection.Protocol)
  else if WSMessageWebRTC.Method = CS_SGC_SUBSCRIBE then
  begin
    // ... OnBeforeSubscription
    vAccept := True;
    DoEventBeforeSubscription(aConnection, WSMessageWebRTC.Channel, vAccept);
    if vAccept then
    begin
      // ... subscribe
      aConnection.DoSubscribe(Guid + '_' + WSMessageWebRTC.Channel);
      if aConnection.Subscribed(Guid + '_' + WSMessageWebRTC.Channel) then
      begin
        WebRTCSubscription_Add(Guid + '_' + WSMessageWebRTC.Channel);
        WSMessageWebRTC.Guid := Guid;
        WSMessageWebRTC.Text := IntToStr
          (WebRTCSubscription_Count(Guid + '_' + WSMessageWebRTC.Channel));
        aConnection.LastSubscription := WSMessageWebRTC.Channel;
        DoNotifySubscription(aConnection);
        aConnection.WriteData(WSMessageWebRTC.Write);
      end;
    end;
  end
  else if WSMessageWebRTC.Method = CS_SGC_UNSUBSCRIBE then
  begin
    aConnection.DoUnsubscribe(Guid + '_' + WSMessageWebRTC.Channel);
    if not aConnection.Subscribed(Guid + '_' + WSMessageWebRTC.Channel) then
    begin
      WebRTCSubscription_Clear(Guid + '_' + WSMessageWebRTC.Channel);
      WSMessageWebRTC.Guid := Guid;
      aConnection.LastUnSubscription := WSMessageWebRTC.Channel;
      DoNotifyUnSubscription(aConnection);
      aConnection.WriteData(WSMessageWebRTC.Write);
    end;
  end
  else if WSMessageWebRTC.Method = CS_SGC_WEBRTC then
    BroadcastWebRTC(WSMessageWebRTC.WebRTC, WSMessageWebRTC.Channel, aConnection.Guid)
  else
  begin
    aConnection.MsgReceived := WSMessageWebRTC.Text;
    inherited;
    aConnection.MsgReceived := Text;
  end;
end;

function TsgcWSProtocol_WebRTC_Server.GetWebRTCSubscriptions: TStringList;
begin
  if not Assigned(FWebRTCSubscriptions) then
    FWebRTCSubscriptions := TStringList.Create;
  result := FWebRTCSubscriptions;
end;

function TsgcWSProtocol_WebRTC_Server.GetWSMessageWebRTC: TsgcWSMessageWebRTC;
begin
  if not Assigned(FWSMessageWebRTC) then
    FWSMessageWebRTC := TsgcWSMessageWebRTC.Create(self);
  result := FWSMessageWebRTC;
end;

procedure TsgcWSProtocol_WebRTC_Server.SetWebRTC(const Value:
    TsgcWSWebRTC_Options);
begin
  if Assigned(FWebRTC) then
    FWebRTC.Assign(Value);
end;

procedure TsgcWSProtocol_WebRTC_Server.WebRTCSubscription_Add(aSubscription:
    String);
var
  i: Integer;
begin
  i := WebRTCSubscriptions.IndexOfName(aSubscription);

  if i = -1 then
    WebRTCSubscriptions.Add(aSubscription + '=1')
  else
    WebRTCSubscriptions.Values[aSubscription] := IntToStr
      (StrToInt(WebRTCSubscriptions.Values[aSubscription]) + 1);
end;

function TsgcWSProtocol_WebRTC_Server.WebRTCSubscription_Count(aSubscription:
    String): Integer;
var
  i: Integer;
begin
  result := 0;

  i := WebRTCSubscriptions.IndexOfName(aSubscription);

  if i <> -1 then
    result := StrToInt(WebRTCSubscriptions.Values[aSubscription]);
end;

procedure TsgcWSProtocol_WebRTC_Server.WebRTCSubscription_Clear(aSubscription:
    String);
var
  i: Integer;
begin
  i := WebRTCSubscriptions.IndexOfName(aSubscription);

  if i > -1 then
    WebRTCSubscriptions.Delete(i);
end;

function TsgcWSProtocol_WebRTC_Server.WriteData(aGuid, aMessage: string):
    Boolean;
begin
  WSMessageWebRTC.DoEnterWrite;
  Try
    WSMessageWebRTC.Text := aMessage;
    WSMessageWebRTC.Method := '';
    WSMessageWebRTC.Channel := '';
    result := inherited WriteData(aGuid, WSMessageWebRTC.Write);
  Finally
    WSMessageWebRTC.DoLeaveWrite;
  End;
end;

function TsgcWSProtocol_JS_WebRTC.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_WEBRTC_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_WebRTC.GetFileName: string;
begin
  result := CS_JS_WEBRTC_ESEGECE_COM;
end;

class function TsgcWSProtocol_HTML_WebRTC.GetFileName: string;
begin
  result := CS_HTML_WEBRTC_ESEGECE_COM;
end;

function TsgcWSProtocol_HTML_WebRTC.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_HTML_WEBRTC_ESEGECE_COM);
end;

constructor TsgcWSWebRTC_Options.Create;
begin
  inherited;
  FIceServers := TStringList.Create;
end;

destructor TsgcWSWebRTC_Options.Destroy;
begin
  sgcFree(FIceServers);
  inherited;
end;

procedure TsgcWSWebRTC_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSWebRTC_Options then
  begin
    IceServers := TsgcWSWebRTC_Options(aSource).IceServers;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSWebRTC_Options.SetIceServers(const Value: TStringList);
begin
  FIceServers.Assign(Value);
end;

initialization

RegisterClass(TsgcWSProtocol_WebRTC_Server);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_WebRTC);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_WebRTC);

{$ENDIF}

end.
