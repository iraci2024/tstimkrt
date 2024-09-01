{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server_WebBrokerBridge;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdCustomTCPServer{$ELSE}IdCustomTCPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHTTPWebBrokerBridge{$ELSE}IdHTTPWebBrokerBridge{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomHTTPServer{$ELSE}IdCustomHTTPServer{$ENDIF},
  // sgc
  sgcWebSocket_Server, sgcWebSocket_CustomServer;

type
  TsgcWSHTTPCommandRequestEvent = procedure(AThread: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
    var aHandled: Boolean) of object;
  TsgcWSHTTPBeforeCommandEvent = procedure(AThread: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
    var aHandled: Boolean) of object;

  TsgcWSHTTPWebBrokerBridgeCustomServer = class(TIdHTTPWebBrokerBridge)
    { from TIdHTTPWebBrokerBridge }
    procedure DoCommandGet(AThread: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoCommandOther(AThread: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    { from TIdHTTPWebBrokerBridge }

    { events }
  private
    FOnCommandRequest: TsgcWSHTTPCommandRequestEvent;
    FOnStartup: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    FOnBeforeCommand: TsgcWSHTTPBeforeCommandEvent;
  protected
    procedure Startup; override;
    procedure Shutdown; override;
  protected
    property OnBeforeCommand: TsgcWSHTTPBeforeCommandEvent read FOnBeforeCommand
      write FOnBeforeCommand;
  public
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
    property OnCommandRequest: TsgcWSHTTPCommandRequestEvent
      read FOnCommandRequest write FOnCommandRequest;
    { events }

    { http }
  protected
    function DoExecute(AContext: TIdContext): Boolean; override;
  public
    procedure DoExecuteHTTP(AContext: TIdContext);
    { http }

    { ssl }
  protected
    FOnQueryHTTPSSLPort: TsgcWSQuerySSLPortEvent;
    procedure DoConnect(AContext: TIdContext); override;
    function DoQuerySSLPort(aPort: Integer): Boolean; reintroduce; virtual;
  public
    property OnExecute;
  public
    property OnQuerySSLPort: TsgcWSQuerySSLPortEvent read FOnQueryHTTPSSLPort
      write FOnQueryHTTPSSLPort;
    { ssl }
  end;

  TsgcWSHTTPWebBrokerBridgeServer = class(TsgcWSHTTPServer)
    { from TsgcWSComponent_Base }
  public
    property NotifyEvents;
    { from TsgcWSComponent_Base }

    { from TsgcWSHTTPServer }
  protected
    procedure DoExecuteHTTP(AContext: TIdContext); override;
    function GetCustomServer: TIdCustomTCPServer; override;
  protected
    procedure SetActive(const Value: Boolean); override;
    { from TsgcWSHTTPServer }

    { property }
  private
    function GetDefaultPort: Integer;
    function GetHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    procedure SetDefaultPort(const Value: Integer);
  public
    property DefaultPort: Integer read GetDefaultPort write SetDefaultPort;
    property HTTPWebBrokerBridge: TIdHTTPWebBrokerBridge
      read GetHTTPWebBrokerBridge;
    { property }

    { events }
  private
    FOnCommandRequest: TsgcWSHTTPCommandRequestEvent;
  protected
    procedure OnParseAuthenticationEvent(AContext: TIdContext; const AAuthType,
        AAuthData: String; var VUsername, VPassword: String; var VHandled:
        Boolean); virtual;
    procedure DoCommandRequestEvent(AThread: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      var aHandled: Boolean);
  protected
    procedure DoBeforeCommandEvent(AThread: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      var aHandled: Boolean); virtual;
  public
    property OnCommandRequest: TsgcWSHTTPCommandRequestEvent
      read FOnCommandRequest write FOnCommandRequest;
    { events }
  end;

implementation

type
  TsgcWSConnectionServer_Hack = class(TsgcWSConnectionServer)

  end;

procedure TsgcWSHTTPWebBrokerBridgeCustomServer.DoCommandGet
  (AThread: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnBeforeCommand) then
    FOnBeforeCommand(AThread, ARequestInfo, AResponseInfo, vHandled);
  if not vHandled then
  begin
    if Assigned(FOnCommandRequest) then
      FOnCommandRequest(AThread, ARequestInfo, AResponseInfo, vHandled);
    if vHandled then
    begin
      if Assigned(FOnCommandGet) then
        FOnCommandGet(AThread, ARequestInfo, AResponseInfo);
    end
    else
      inherited;
  end;
end;

procedure TsgcWSHTTPWebBrokerBridgeCustomServer.DoCommandOther
  (AThread: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnCommandRequest) then
    FOnCommandRequest(AThread, ARequestInfo, AResponseInfo, vHandled);
  if vHandled then
  begin
    if Assigned(FOnCommandOther) then
      FOnCommandOther(AThread, ARequestInfo, AResponseInfo);
  end
  else
    inherited;
end;

procedure TsgcWSHTTPWebBrokerBridgeCustomServer.DoConnect(AContext: TIdContext);
begin
  // ... customized DoConnect
  // inherited;
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
  begin
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough :=
      not DoQuerySSLPort(AContext.Connection.Socket.Binding.Port);
  end;
  if Assigned(OnConnect) then
    OnConnect(AContext);
end;

function TsgcWSHTTPWebBrokerBridgeCustomServer.DoExecute
  (AContext: TIdContext): Boolean;
begin
  if Assigned(FOnExecute) then
  begin
    FOnExecute(AContext);
    result := AContext.Connection.Connected;
  end
  else
    result := inherited DoExecute(AContext);
end;

procedure TsgcWSHTTPWebBrokerBridgeCustomServer.DoExecuteHTTP
  (AContext: TIdContext);
begin
  inherited DoExecute(AContext);
end;

function TsgcWSHTTPWebBrokerBridgeCustomServer.DoQuerySSLPort
  (aPort: Integer): Boolean;
begin
  result := not Assigned(FOnQueryHTTPSSLPort);
  if not result then
    FOnQueryHTTPSSLPort(aPort, result);
end;

procedure TsgcWSHTTPWebBrokerBridgeCustomServer.Shutdown;
begin
  inherited;
  if Assigned(FOnShutdown) then
    FOnShutdown(self);
end;

procedure TsgcWSHTTPWebBrokerBridgeCustomServer.Startup;
begin
  inherited;
  if Assigned(FOnStartup) then
    FOnStartup(self);
end;

procedure TsgcWSHTTPWebBrokerBridgeServer.DoBeforeCommandEvent
  (AThread: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; var aHandled: Boolean);
var
  oConnection: TsgcWSConnectionServer;
begin
  // ... forward http
  if Assigned(FOnBeforeForwardHTTP) then
  begin
    oConnection := TsgcWSConnectionServer
      (AThread.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
    if Assigned(oConnection) then
    begin
      if DoForwardHTTP(oconnection, ARequestInfo, AResponseInfo) then
      begin
        aHandled := True;
        exit;
      end;
    end;
  end;

  // ... oauth2
  if Assigned(Authentication.OAuth.OAuth2) then
  begin
    oConnection := TsgcWSConnectionServer
      (AThread.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
    if Assigned(oConnection) then
    begin
      if not Authentication.OAuth.OAuth2.IsOAuth2TokenValid(oConnection,
        oConnection.HeadersRequest) then
      begin
        aHandled := True;
        TsgcWSConnectionServer_Hack(oConnection)
          .DoHTTPError(401, 'Unauthorized');
        oConnection.Disconnect;
      end;
    end;
  end;

  // ... JWT
  if Assigned(Authentication.JWT.JWT) then
  begin
    oConnection := TsgcWSConnectionServer
      (AThread.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
    if Assigned(oConnection) then
    begin
      if not Authentication.JWT.JWT.IsJWTTokenValid(oConnection,
        oConnection.HeadersRequest) then
      begin
        aHandled := True;
        TsgcWSConnectionServer_Hack(oConnection)
          .DoHTTPError(401, 'Unauthorized');
        oConnection.Disconnect;
      end;
    end;
  end;
end;

procedure TsgcWSHTTPWebBrokerBridgeServer.DoExecuteHTTP(AContext: TIdContext);
begin
  TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).DoExecuteHTTP(AContext);
end;

function TsgcWSHTTPWebBrokerBridgeServer.GetCustomServer: TIdCustomTCPServer;
begin
  if not Assigned(FHTTPServer) then
  begin
    FHTTPServer := TsgcWSHTTPWebBrokerBridgeCustomServer.Create;
    FHTTPServer.DefaultPort := Port;

    // Events
    FHTTPServer.OnConnect := OnServerConnectEvent;
    FHTTPServer.OnDisconnect := OnServerDisconnectEvent;
    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnExecute :=
      OnServerExecuteEvent;
    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnQuerySSLPort :=
      OnGetSSLPortEvent;

    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnStartup :=
      OnServerStartupEvent;
    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnShutdown :=
      OnServerShutdownEvent;

    // Events HTTP
    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnCommandRequest :=
      DoCommandRequestEvent;
    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnCommandGet :=
      OnCommandGetEvent;
    TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnBeforeCommand :=
      DoBeforeCommandEvent;
    FHTTPServer.OnCommandOther := OnCommandOtherEvent;
    FHTTPServer.OnCreateSession := OnCreateSessionEvent;
    FHTTPServer.OnInvalidSession := OnInvalidSessionEvent;
    FHTTPServer.OnSessionStart := OnSessionStartEvent;
    FHTTPServer.OnSessionEnd := OnSessionEndEvent;
    FHTTPServer.OnException := OnExceptionEvent;
  end;

  result := FHTTPServer;
end;

function TsgcWSHTTPWebBrokerBridgeServer.GetDefaultPort: Integer;
begin
  result := Port;
end;

function TsgcWSHTTPWebBrokerBridgeServer.GetHTTPWebBrokerBridge
  : TIdHTTPWebBrokerBridge;
begin
  result := FHTTPServer as TIdHTTPWebBrokerBridge;
end;

procedure TsgcWSHTTPWebBrokerBridgeServer.DoCommandRequestEvent
  (AThread: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; var aHandled: Boolean);
begin
  aHandled := False;

  if Assigned(FOnCommandRequest) then
    FOnCommandRequest(AThread, ARequestInfo, AResponseInfo, aHandled);
end;

procedure TsgcWSHTTPWebBrokerBridgeServer.OnParseAuthenticationEvent(AContext:
    TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword:
    String; var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TsgcWSHTTPWebBrokerBridgeServer.SetActive(const Value: Boolean);
begin
  inherited;
  if not IsLoading and not IsDesigning then
  begin
    if Value then
    begin
      if Assigned(Authentication.OAuth.OAuth2) then
        TsgcWSHTTPWebBrokerBridgeCustomServer(FHTTPServer).OnParseAuthentication :=
        OnParseAuthenticationEvent;
    end;
  end;
end;

procedure TsgcWSHTTPWebBrokerBridgeServer.SetDefaultPort(const Value: Integer);
begin
  Port := Value;
end;

end.
