{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Server_Proxy;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // indy
  {$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdIOHandler{$ELSE}IdIOHandler{$ENDIF},
  // sgc
  sgcWebSocket_Server, sgcWebSocket_Types, sgcWebSocket_Classes,
  sgcWebSocket_CustomClient, sgcWebSocket_Client;

type

  TsgcWSOnSSLGetHandler = procedure(Sender: TObject; aType: TwsSSLHandler; var
      aSSLHandler: TIdSSLIOHandlerSocketBase) of object;
  TsgcWSOnSSLAfterCreateHandler = procedure(Sender: TObject; aType:
      TwsSSLHandler; aSSLHandler: TIdSSLIOHandlerSocketBase) of object;

  TsgcWSProxyServer_Options = class(TPersistent)
  private
    FHost: String;
    FMessageType: TwsMessageType;
    FPort: Integer;
    FTLS: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Host: String read FHost write FHost;
    property MessageType: TwsMessageType read FMessageType write FMessageType;
    property Port: Integer read FPort write FPort;
    property TLS: Boolean read FTLS write FTLS;
  end;


  TsgcWSProxyConnection = class(TComponent)
  { connection }
  private
    FWSConnection: TsgcWSConnection;
  { connection }

  { properties }
  private
    FTLS: Boolean;
    FMessageType: TwsMessageType;
    function GetHost: String;
    function GetPort: Integer;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
  public
    property Host: String read GetHost write SetHost;
    property MessageType: TwsMessageType read FMessageType write FMessageType;
    property Port: Integer read GetPort write SetPort;
    property TLS: Boolean read FTLS write FTLS;
  { properties }

  { client }
  private
    FTCPClient: TsgcWSCustomClient;
    function GetTCPClient: TsgcWSCustomClient;
  protected
    procedure OnReadEvent(Sender: TObject); virtual;
  protected
    property TCPClient: TsgcWSCustomClient read GetTCPClient write FTCPClient;
  { client }

  { ssl }
  private
    FHandlerSSL: TIdSSLIOHandlerSocketBase;
  protected
    FOnSSLGetHandler: TsgcWSOnSSLGetHandler;
    FOnSSLAfterCreateHandler: TsgcWSOnSSLAfterCreateHandler;
  protected
    procedure GetHandlerSSLEvent(const aSSLType: TwsSSLHandler; var aSSLHandler:
        TIdSSLIOHandlerSocketBase); virtual;
  protected
    property HandlerSSL: TIdSSLIOHandlerSocketBase read FHandlerSSL;
  public
    property OnSSLGetHandler: TsgcWSOnSSLGetHandler read FOnSSLGetHandler write
        FOnSSLGetHandler;
    property OnSSLAfterCreateHandler: TsgcWSOnSSLAfterCreateHandler read
        FOnSSLAfterCreateHandler write FOnSSLAfterCreateHandler;
  { ssl }

  { methods }
  public
    procedure Connect;
    procedure Disconnect;
  public
    procedure WriteData(const aText: String); overload;
    procedure WriteData(const aData: TStream); overload;
  { methods }
  end;

  TsgcWSProxyServer = class(TsgcWSServer)
  { from TsgcWSComponent_Base }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection; Data:
        TMemoryStream); override;
    procedure DoEventError(aConnection: TsgcWSConnection; const Error: string);
        override;
    procedure DoEventException(aConnection: TsgcWSConnection; const Error: String;
        aException: Exception); override;
  { from TsgcWSComponent_Base }

  { from TsgcWSComponent }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyMessage(aConnection: TsgcWSConnection); override;
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
  { from TsgcWSComponent }

  { proxy }
  private
    FProxy: TsgcWSProxyServer_Options;
  protected
    procedure SetProxy(const Value: TsgcWSProxyServer_Options); virtual;
  public
    property Proxy: TsgcWSProxyServer_Options read FProxy write SetProxy;
  { proxy }

  { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor / destructor }
  end;



implementation

uses
  sgcWebSocket_Helpers, sgcWebSocket_Const;

constructor TsgcWSProxyServer.Create(aOwner: TComponent);
begin
  inherited;
  // ... properties
  NotifyEvents := neNoSync;
  FProxy := TsgcWSProxyServer_Options.Create;
end;

destructor TsgcWSProxyServer.Destroy;
begin
  sgcFree(FProxy);
  inherited;
end;

procedure TsgcWSProxyServer.DoEventBinary(const aConnection: TsgcWSConnection;
    Data: TMemoryStream);
var
  oProxy: TsgcWSProxyConnection;
begin
  inherited;
  if Assigned(aConnection) then
  begin
    oProxy := TsgcWSProxyConnection(aConnection.Data);
    if Assigned(oProxy) then
      oProxy.WriteData(Data);
  end;
end;

procedure TsgcWSProxyServer.DoEventConnect(aConnection: TsgcWSConnection);
var
  oProxy: TsgcWSProxyConnection;
begin
  inherited;
  oProxy := TsgcWSProxyConnection.Create(nil);
  oProxy.FWSConnection := aConnection;
  oProxy.Host := Proxy.Host;
  oProxy.Port := Proxy.Port;
  oProxy.MessageType := Proxy.MessageType;
  oProxy.TLS := Proxy.TLS;

  aConnection.Data := oProxy;

  SynchronizeMethod(oProxy.Connect);
end;

procedure TsgcWSProxyServer.DoEventDisconnect(aConnection: TsgcWSConnection;
    Code: Integer);
var
  oProxy: TsgcWSProxyConnection;
begin
  inherited;
  if Assigned(aConnection) then
  begin
    oProxy := TsgcWSProxyConnection(aConnection.Data);
    if Assigned(oProxy) then
      SynchronizeMethod(oProxy.Disconnect);
  end;
end;

procedure TsgcWSProxyServer.DoEventError(aConnection: TsgcWSConnection; const
    Error: string);
begin
  inherited;
end;

procedure TsgcWSProxyServer.DoEventException(aConnection: TsgcWSConnection;
    const Error: String; aException: Exception);
begin
  inherited;
end;

procedure TsgcWSProxyServer.DoEventMessage(aConnection: TsgcWSConnection; const
    Text: string);
var
  oProxy: TsgcWSProxyConnection;
begin
  inherited;
  if Assigned(aConnection) then
  begin
    oProxy := TsgcWSProxyConnection(aConnection.Data);
    if Assigned(oProxy) then
      oProxy.WriteData(Text);
  end;
end;

procedure TsgcWSProxyServer.DoNotifyBinary(aConnection: TsgcWSConnection);
begin
  DoEventBinary(aConnection, aConnection.MsgBinaryReceived);
end;

procedure TsgcWSProxyServer.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  DoEventConnect(aConnection);
  inherited;
end;

procedure TsgcWSProxyServer.DoNotifyDisconnect(aConnection: TsgcWSConnection);
begin
  DoEventDisconnect(aConnection, CS_CLOSE_NORMAL);
  inherited;
end;

procedure TsgcWSProxyServer.DoNotifyMessage(aConnection: TsgcWSConnection);
begin
  DoEventMessage(aConnection, aConnection.MsgReceived);
end;

procedure TsgcWSProxyServer.SetProxy(const Value: TsgcWSProxyServer_Options);
begin
  FProxy.Assign(Value);
end;

procedure TsgcWSProxyConnection.Connect;
begin
  // ... initialize
  TCPClient.IOHandler := nil;
  sgcFree(FHandlerSSL);

  // ... tls
  if TLS then
  begin
    GetHandlerSSLEvent(sslProxy, FHandlerSSL);
    TCPClient.IOHandler := FHandlerSSL;
  end;

  // ... conect
  TCPClient.Connect;
end;

procedure TsgcWSProxyConnection.Disconnect;
begin
  TCPClient.Disconnect;
end;

procedure TsgcWSProxyConnection.GetHandlerSSLEvent(const aSSLType:
    TwsSSLHandler; var aSSLHandler: TIdSSLIOHandlerSocketBase);
begin
  // ... customer can assign own handler
  if Assigned(FOnSSLGetHandler) then
    FOnSSLGetHandler(Self, aSSLType, aSSLHandler);
  // ... if not customized, create by default
  if not Assigned(FHandlerSSL) then
  begin
    FHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create(self);
    FHandlerSSL.PassThrough := False;
    TIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method := sslvTLSv1;
    TIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Mode := sslmClient;
    if Assigned(FOnSSLAfterCreateHandler) then
      FOnSSLAfterCreateHandler(Self, aSSLType, aSSLHandler);
  end;
end;

function TsgcWSProxyConnection.GetHost: String;
begin
  Result := TCPClient.Host;
end;

function TsgcWSProxyConnection.GetPort: Integer;
begin
  Result := TCPClient.Port;
end;

function TsgcWSProxyConnection.GetTCPClient: TsgcWSCustomClient;
begin
  if not Assigned(FTCPClient) then
  begin
    FTCPClient := TsgcWSCustomClient.Create(self);
    FTCPClient.OnRead := OnReadEvent;
  end;
  Result := FTCPClient;
end;

procedure TsgcWSProxyConnection.OnReadEvent(Sender: TObject);
var
  vText: String;
  oStream: TMemoryStream;
begin
  if not Assigned(FWSConnection) then exit;

  case MessageType of
    mtText:
      begin
        vText := TCPClient.IOHandler.InputBufferAsString;
        Try
          FWSConnection.WriteData(vText);
        Finally
          TCPClient.IOHandler.InputBuffer.Clear;
        End;
      end;
    mtBinary:
      begin
        oStream := TMemoryStream.Create;
        Try
          TCPClient.IOHandler.InputBufferToStream(oStream);
          oStream.Position := 0;
          FWSConnection.WriteData(oStream);
        Finally
          sgcFree(oStream);
        End;
      end;
  end;
end;

procedure TsgcWSProxyConnection.SetHost(const Value: String);
begin
  TCPClient.Host := Value;
end;

procedure TsgcWSProxyConnection.SetPort(const Value: Integer);
begin
  TCPClient.Port := Value;
end;

procedure TsgcWSProxyConnection.WriteData(const aText: String);
begin
  TCPClient.IOHandler.Write(aText);
end;

procedure TsgcWSProxyConnection.WriteData(const aData: TStream);
begin
  TCPClient.IOHandler.Write(aData);
end;

constructor TsgcWSProxyServer_Options.Create;
begin
  inherited;
  Host := '127.0.0.1';
  Port := 80;
  MessageType := mtText;
end;

procedure TsgcWSProxyServer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSProxyServer_Options then
  begin
    Host := TsgcWSProxyServer_Options(aSource).Host;
    Port := TsgcWSProxyServer_Options(aSource).Port;
    MessageType := TsgcWSProxyServer_Options(aSource).MessageType;
    TLS := TsgcWSProxyServer_Options(aSource).TLS;
  end
  else
    inherited Assign(aSource);
end;

end.
