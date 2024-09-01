{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2022 - 2023                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCWebSocketServer;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes, SysUtils, IniFiles, IdContext, idIOHandler, IdTCPConnection, IdCustomHTTPServer,
  Generics.Collections, IdHTTPServer, IdScheduler, IdSchedulerOfThreadDefault, IdHeaderList,
  IdSSLOpenSSL, idGlobal, FMX.TMSFNCWebSocketCommon, FMX.TMSFNCCustomComponent,
  FMX.TMSFNCTypes;

type
  EWebsocketServer = class(EWebsocket);

  TTMSFNCWebSocketServerConnection = class;

  TTMSFNCWebSocketAllowConnectionEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketServerConnection; var AAllow: Boolean) of object;
  TTMSFNCWebSocketHandshakeSentEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketServerConnection) of object;

  TTMSFNCWebSocketServerConnection = class(TTMSFNCWebSocketConnection)
  private
    FUserData: TObject;
    FOwnsUserData: Boolean;
    FHandshakeResponseSent: Boolean;
    FOnAllow: TTMSFNCWebSocketAllowConnectionEvent;
    FPeerThread : TIdContext;
    FOnHandshakeResponseSent: TTMSFNCWebSocketHandshakeSentEvent;
  protected
    function GetConnection: TIDTCPConnection; override;
    procedure SaveRequest(ARequestInfo: TIdHTTPRequestInfo); virtual;
    procedure Handshake(const ARequestInfo: TIdHTTPRequestInfo; const AResponseInfo: TIdHTTPResponseInfo); virtual;
    function CheckRequest(const AResponseInfo: TIdHTTPResponseInfo): Boolean; virtual;
    function GetPeerIP: string; override;
    procedure Receive(const ARequestInfo: TIdHTTPRequestInfo; const AResponseInfo: TIdHTTPResponseInfo);
    function GetHandshakeCompleted: Boolean; override;
    property PeerThread: TIdContext read FPeerThread;
  public
    constructor Create(APeerThread: TIdContext; AOptions: TTMSFNCWebsocketOptions); reintroduce; overload;
    destructor Destroy; override;
    // Has handshake response been sent ?
    property HandshakeResponseSent: Boolean read FHandshakeResponseSent;
    // User data to associate with this connection
    property UserData: TObject read FUserData write FUserData;
    // If set to true, the owner data is freed when the connection is freed.
    property OwnsUserData: Boolean read FOwnsUserData write FOwnsUserData;
    // Called after correct headers were received to see if connection must be allowed.
    property OnAllow: TTMSFNCWebSocketAllowConnectionEvent read FOnAllow write FOnAllow;
    property OnHandshakeResponseSent: TTMSFNCWebSocketHandshakeSentEvent read FOnHandshakeResponseSent write FOnHandshakeResponseSent;
  end;

  TTMSFNCWebSocketConnections = class(TList<TTMSFNCWebSocketServerConnection>);

  TTMSFNCWebSocketHTTPServer = class(TIdHttpServer);

  TTMSFNCWebSocketSendToCallBack = reference to function (AConnection: TTMSFNCWebSocketServerConnection): Boolean;

  TTMSFNCWebSocketServerGetSSLPasswordEvent = procedure(Sender: TObject; var APassword: string) of object;

  TTMSFNCCustomWebSocketServer = class(TTMSFNCCustomComponent)
  private
    FPort: Integer;
    FHTTPServer: TTMSFNCWebSocketHTTPServer;
    FSSLHandler: TIdServerIOHandlerSSLOpenSSL;
    FThreadManager: TIdScheduler;
    FConnections: TTMSFNCWebSocketConnections;
    FOnConnect: TTMSFNCWebSocketConnectEvent;
    FOnMessageReceived: TTMSFNCWebSocketMessageEvent;
    FOnDisconnect: TTMSFNCWebSocketDisconnectEvent;
    FUseSSL: Boolean;
    FOnBinaryDataReceived: TTMSFNCWebSocketBinDataEvent;
    FOptions: TTMSFNCWebsocketOptions;
    FOnPing: TTMSFNCWebSocketControlEvent;
    FOnPong: TTMSFNCWebSocketControlEvent;
    FOnClose: TTMSFNCWebSocketControlEvent;
    FWebSocketVersion: Integer;
    FOnAllow: TTMSFNCWebSocketAllowConnectionEvent;
    FPathName: string;
    FOnCommandGet: TIdHTTPCommandEvent;
    FOnCommandOther: TIdHTTPCommandEvent;
    FRootCertificateFile: string;
    FCertificateKeyFile: string;
    FCertificateFile: string;
    FOnGetSSLPassword: TTMSFNCWebSocketServerGetSSLPasswordEvent;
    FSplitMessage: Boolean;
    FFrameSize: UInt64;
    FOnHandshakeResponseSent: TTMSFNCWebSocketHandshakeSentEvent;
    FServerActive: Boolean;
    FAutoSyncEvents: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetOptions(const Value: TTMSFNCWebsocketOptions);
    procedure SetOnCommandOther(const Value: TIdHTTPCommandEvent);
    procedure SetCertificateFile(const Value: string);
    procedure SetCertificateKeyFile(const Value: string);
    procedure SetRootCertificateFile(const Value: string);
    procedure SetUseSSL(const Value: Boolean);
    procedure SetFrameSize(const Value: UInt64);
    procedure SetSplitMessage(const Value: Boolean);
  protected
    procedure Loaded; override;
    function GetInstance: NativeUInt; override;
    function GetDocURL: string; override;
    function GetSSLHandler: TIdServerIOHandlerSSLOpenSSL; virtual;
    function GetHTTPServer: TTMSFNCWebSocketHTTPServer; virtual;
    function GetThreadManager: TIdScheduler; virtual;
    function GetConnections: TTMSFNCWebSocketConnections; virtual;
    function CreateWebsocketConnection(aContext: TIdContext; AOptions: TTMSFNCWebSocketOptions): TTMSFNCWebSocketServerConnection; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HTTPServerQuerySSLPort(APort: Word; var VUseSSL: Boolean); virtual;
    procedure HTTPServerConnect(aContext: TIdContext); virtual;
    procedure HTTPServerDisconnect(aContext: TIdContext); virtual;
    procedure HTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;
    procedure DoHTTPRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;
    procedure AllowConnection(Sender: TObject; AConnection: TTMSFNCWebSocketServerConnection; var aAllow : Boolean);
    procedure HandshakeResponseSent(Sender: TObject; AConnection: TTMSFNCWebSocketServerConnection);
    procedure MessageReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AMessage: string); virtual;
    procedure BinaryDataReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes; FrameType: TTMSFNCWebSocketFrameTypes); virtual;
    procedure PingReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aData: TBytes); virtual;
    procedure PongReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aData: TBytes); virtual;
    procedure CloseReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aData: TBytes); virtual;
    function DoIOHandlerVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
    procedure DoIOHandlerGetPassword(var Password: string);
    property ThreadManager: TIdScheduler read GetThreadManager;
    property Connections: TTMSFNCWebSocketConnections read GetConnections;
    property SSLHandler: TIdServerIOHandlerSSLOpenSSL read GetSSLHandler;
    property HTTPServer: TTMSFNCWebSocketHTTPServer read GetHTTPServer;
    property AutoSyncEvents: Boolean read FAutoSyncEvents write FAutoSyncEvents;
    // Websocket version to use
    property WebSocketVersion: Integer read FWebSocketVersion write FWebSocketVersion default CurrentWebSocketVersion;
    // PathName: when set, the request must match this.
    property PathName: string read FPathName write FPathName;
    // Start/stop server
    property Active: Boolean read GetActive write SetActive default False;
    // Certificate paths when using SSL. Setting these to a non-empty value will set UseSSL.
    property CertificateFile: string read FCertificateFile write SetCertificateFile;
    property CertificateKeyFile: string read FCertificateKeyFile write SetCertificateKeyFile;
    property RootCertificateFile: string read FRootCertificateFile write SetRootCertificateFile;
    // Must SSL be used ?
    property UseSSL: Boolean read FUseSSL write SetUseSSL default False;
    // Port to listen on
    property Port: Integer read FPort write FPort default DefaultPort;
    // Options regarding WebSocket Protocol
    property Options: TTMSFNCWebsocketOptions read FOptions write SetOptions default [];
    //Determine if messages should be split into multiple frames
    property SplitMessage: Boolean read FSplitMessage write SetSplitMessage default False;
    //Frame size if SplitMessage is set to true
    property FrameSize: UInt64 read FFrameSize write SetFrameSize default 0;
    // Called when a new connection is made.
    property OnConnect: TTMSFNCWebSocketConnectEvent read FOnConnect write FOnConnect;
    // Called when handshake was received, use this to disallow connection
    property OnAllow: TTMSFNCWebSocketAllowConnectionEvent read FOnAllow write FOnAllow;
    // Called when a text message is received.
    property OnMessageReceived: TTMSFNCWebSocketMessageEvent read FOnMessageReceived write FOnMessageReceived;
    // Called when a binary message is received.
    property OnBinaryDataReceived: TTMSFNCWebSocketBinDataEvent read FOnBinaryDataReceived write FOnBinaryDataReceived;
    // Called when a connection is disconnected
    property OnDisconnect: TTMSFNCWebSocketNotifyEvent read FOnDisconnect write FOnDisconnect;
    // Called when a ping is received.
    property OnPing: TTMSFNCWebSocketControlEvent read FOnPing write FOnPing;
    // Called when a pong is received.
    property OnPong: TTMSFNCWebSocketControlEvent read FOnPong write FOnPong;
    // Called when a close message is received.
    property OnClose: TTMSFNCWebSocketControlEvent read FOnClose write FOnClose;
    // Called when a non-Websocket request is received: set this if you want to handle regular HTTP requests as well.
    property OnCommandGet: TIdHTTPCommandEvent read FOnCommandGet write FOnCommandGet;
    // Called when a non-Websocket non-get request is received: set this if you want to handle regular HTTP requests as well.
    property OnCommandOther: TIdHTTPCommandEvent read FOnCommandOther write SetOnCommandOther;
    // Called when SSL certificate needs password
    property OnGetSSLPassword: TTMSFNCWebSocketServerGetSSLPasswordEvent read FOnGetSSLPassword write FOnGetSSLPassword;
    property OnHandshakeResponseSent: TTMSFNCWebSocketHandshakeSentEvent read FOnHandshakeResponseSent write FOnHandshakeResponseSent;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; APort: Integer); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure BroadcastMessage(AMessage: string); virtual;
    procedure BroadcastData(AData: TBytes); virtual;
    procedure SendMessageTo(AMessage: string; ASelector: TTMSFNCWebSocketSendToCallBack); virtual;
    procedure SendDataTo(AData: TBytes; ASelector: TTMSFNCWebSocketSendToCallBack); virtual;
    class function TransformCloseData(ABytes: TBytes; out AReason: string): Word;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TTMSFNCWebSocketServer = class(TTMSFNCCustomWebSocketServer)
  published
    property AutoSyncEvents;
    property PathName;
    property SplitMessage;
    property FrameSize;
    //property WebSocketVersion;
    property Active;
    property CertificateFile;
    property CertificateKeyfile;
    property RootCertificateFile;
    property UseSSL;
    property Port;
    property Options;
    property OnConnect;
    property OnAllow;
    property OnMessageReceived;
    property OnBinaryDataReceived;
    property OnDisconnect;
    property OnPing;
    property OnPong;
    property OnClose;
    property OnHandshakeResponseSent;
    property OnGetSSLPassword;
  end;

implementation

uses
   IdHashSHA, idCoderMime, IOUtils, UITypes, UIConsts, idStack
  {$IFDEF MACOS}
  ,MacApi.CoreFoundation
  {$ENDIF}
  ;

{$R TMSFNCWebSocketServer.res}

{ TTMSFNCCustomWebSocketServer }

procedure TTMSFNCCustomWebSocketServer.MessageReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AMessage: string);
begin
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(OnMessageReceived) and (AConnection.HandshakeCompleted) then
        OnMessageReceived(Self, AConnection, AMessage);
    end);
  end
  else
  begin
    if Assigned(OnMessageReceived) and (AConnection.HandshakeCompleted) then
      OnMessageReceived(Self, AConnection, AMessage);
  end;
end;

procedure TTMSFNCCustomWebSocketServer.AllowConnection(Sender: TObject; AConnection: TTMSFNCWebSocketServerConnection; var aAllow: Boolean);
begin
  if Assigned(FOnAllow) then
    OnAllow(Self, aConnection, aAllow);
end;

procedure TTMSFNCCustomWebSocketServer.BinaryDataReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData : TBytes; FrameType : TTMSFNCWebSocketFrameTypes);
begin
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(OnBinaryDataReceived) and (AConnection.HandshakeCompleted) then
        OnBinaryDataReceived(Self, AConnection, aData, FrameType);
    end);
  end
  else
  begin
    if Assigned(OnBinaryDataReceived) and (AConnection.HandshakeCompleted) then
      OnBinaryDataReceived(Self, AConnection, aData, FrameType);
  end;
end;


procedure TTMSFNCCustomWebSocketServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (aComponent = FHTTPServer) then
      FHTTPServer := nil
    else if (aComponent = FSSLHandler) then
      FSSLHandler := nil;
  end;
end;

procedure TTMSFNCCustomWebSocketServer.PingReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes);
begin
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnPing) then
        FOnPing(Self, aConnection, aData);
    end);
  end
  else
  begin
    if Assigned(FOnPing) then
      FOnPing(Self, aConnection, aData);
  end;
end;

procedure TTMSFNCCustomWebSocketServer.PongReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes);
begin
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnPong) then
        FOnPong(Self, aConnection, aData);
    end);
  end
  else
  begin
    if Assigned(FOnPong) then
      FOnPong(Self, aConnection, aData);
  end;
end;

procedure TTMSFNCCustomWebSocketServer.CloseReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const AData: TBytes);
begin
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnClose) then
        FOnClose(Self, aConnection, aData);
    end);
  end
  else
  begin
    if Assigned(FOnClose) then
      FOnClose(Self, aConnection, aData);
  end;
end;

procedure TTMSFNCCustomWebSocketServer.BroadcastData(AData: TBytes);
begin
  SendDataTo(aData, function(aConnection: TTMSFNCWebSocketServerConnection): Boolean
    begin
      Result := True;
    end);
end;

procedure TTMSFNCCustomWebSocketServer.BroadcastMessage(AMessage: string);
begin
  SendMessageTo(aMessage, function(aConnection: TTMSFNCWebSocketServerConnection): Boolean
    begin
      Result := True;
    end);
end;

constructor TTMSFNCCustomWebSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerActive := False;
  FSplitMessage := False;
  FFrameSize := 0;
  FUseSSL := False;
  FPort := DefaultPort;
  FWebSocketVersion := CurrentWebSocketVersion;
end;

constructor TTMSFNCCustomWebSocketServer.Create(AOwner: TComponent; APort: Integer);
begin
  Create(aOwner);
  FPort := APort;
end;

destructor TTMSFNCCustomWebSocketServer.Destroy;
begin
  if Assigned(FConnections) then
    FConnections.Free;

  if Assigned(FHTTPServer) then
  begin
    FHTTPServer.Active := False;
    FHTTPServer.Free;
  end;

  if Assigned(FThreadManager) then
    FThreadManager.Free;

  inherited;
end;

procedure TTMSFNCCustomWebSocketServer.DoHTTPRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if Assigned(OnCommandGet) then
  begin
    if AutoSyncEvents then
    begin
      TThread.Queue(nil,
      procedure
      begin
        OnCommandGet(AContext, ARequestInfo, AResponseInfo);
      end);
    end
    else
    begin
      OnCommandGet(AContext, ARequestInfo, AResponseInfo);
    end;
  end
  else
  begin
    if ARequestInfo.Document = '' then
      AResponseInfo.Redirect('/')
    else if ARequestInfo.Document = '/' then
    begin
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentType := 'text/html';
      AResponseInfo.CharSet := 'UTF-8';
    end
    else
      AResponseInfo.ResponseNo := 404;
  end
end;

procedure TTMSFNCCustomWebSocketServer.DoIOHandlerGetPassword(
  var Password: string);
begin
  if Assigned(OnGetSSLPassword) then
    OnGetSSLPassword(Self, Password);
end;

function TTMSFNCCustomWebSocketServer.DoIOHandlerVerifyPeer(Certificate: TIdX509;
  AOk: Boolean; ADepth, AError: Integer): Boolean;
begin
  if ADepth = 0 then
  begin
    Result := AOk;
  end
  else
  begin
    Result := True;
  end;
end;

function TTMSFNCCustomWebSocketServer.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FServerActive
  else
    Result := Assigned(FHTTPServer) and HTTPServer.Active;
end;

function TTMSFNCCustomWebSocketServer.GetConnections: TTMSFNCWebSocketConnections;
begin
  if not Assigned(FConnections) then
  begin
    FConnections := TTMSFNCWebSocketConnections.Create;
  end;

  Result := FConnections;
end;

function TTMSFNCCustomWebSocketServer.GetDocURL: string;
begin
  Result := 'https://download.tmssoftware.com/doc/tmsfncwebsocket/components/ttmsfncwebsocketserver';
end;

function TTMSFNCCustomWebSocketServer.GetHTTPServer: TTMSFNCWebSocketHTTPServer;
begin
  if not Assigned(FHTTPServer) then
  begin
    FHTTPServer := TTMSFNCWebSocketHTTPServer.Create(Self);
    FHTTPServer.FreeNotification(Self);
    FHTTPServer.Scheduler := ThreadManager;
    FHTTPServer.DefaultPort := Port;
    FHTTPServer.OnQuerySSLPort := HTTPServerQuerySSLPort;
    FHTTPServer.OnConnect := HTTPServerConnect;
    FHTTPServer.OnDisconnect := HTTPServerDisconnect;
    FHTTPServer.OnCommandGet := HTTPServerCommandGet;
    FHTTPServer.OnCommandOther := FOnCommandOther;
    if UseSSL then
      FHTTPServer.IOHandler := SSLHandler;
  end;

  Result := FHTTPServer;
end;

function TTMSFNCCustomWebSocketServer.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomWebSocketServer.GetSSLHandler: TIdServerIOHandlerSSLOpenSSL;
begin
  if not Assigned(FSSLHandler) then
  begin
    FSSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(self);
    FSSLHandler.FreeNotification(self);
    with FSSLHandler.SSLOptions do
    begin
      RootCertFile := FRootCertificateFile;
      KeyFile := FCertificateKeyFile;
      CertFile := FCertificateFile;
      Method := sslvSSLv23;
      Mode := sslmServer;
      VerifyDepth := 0;
      VerifyMode := [];
      SSLVersions := [sslvSSLv2, sslvSSLv3, sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    end;
    FSSLHandler.OnVerifyPeer := DoIOHandlerVerifyPeer;
    FSSLHandler.OnGetPassword := DoIOHandlerGetPassword;
  end;
  Result := FSSLHandler;
end;

function TTMSFNCCustomWebSocketServer.GetThreadManager: TIdScheduler;
begin
  if not Assigned(FThreadManager) then
  begin
    FThreadManager := TIdSchedulerOfThreadDefault.Create(nil);
  end;

  Result := FThreadManager;
end;

procedure TTMSFNCCustomWebSocketServer.SendDataTo(AData: TBytes; ASelector: TTMSFNCWebSocketSendToCallBack);
var
  ConnectionPtr: Pointer;
  Connection: TTMSFNCWebSocketServerConnection;
  F: TTMSFNCWebSocketFrame;
begin
  if SplitMessage then
  begin
    for ConnectionPtr in Connections do
    begin
      Connection := ConnectionPtr;
      if Connection.HandshakeCompleted and aSelector(Connection) then
        Connection.SendBytesInMultipleFrames(AData, FFrameSize);
    end;
  end
  else
  begin
    F := nil;
    try
      for ConnectionPtr in Connections do
      begin
        Connection := ConnectionPtr;
        if Connection.HandshakeCompleted and aSelector(Connection) then
        begin
          if (F = nil) then
            F := Connection.GetFrameClass.Create(focBinary, aData, True);
          Connection.SendFrame(F);
        end;
      end;
    finally
      F.Free;
    end;
  end;
end;

procedure TTMSFNCCustomWebSocketServer.SendMessageTo(AMessage: string;
  ASelector: TTMSFNCWebSocketSendToCallBack);
var
  ConnectionPtr: Pointer;
  Connection: TTMSFNCWebSocketServerConnection;
  F: TTMSFNCWebSocketFrame;
begin
  if SplitMessage then
  begin
    for ConnectionPtr in Connections do
    begin
      Connection := ConnectionPtr;
      if Connection.HandshakeCompleted and aSelector(Connection) then
        Connection.SendInMultipleFrames(AMessage, FFrameSize);
    end;
  end
  else
  begin
    // Create the message only once.
    F := nil;
    try
      for ConnectionPtr in Connections do
      begin
        Connection := ConnectionPtr;
        if Connection.HandshakeCompleted and aSelector(Connection) then
        begin
          if F = nil then
            F := Connection.GetFrameClass.Create(aMessage);
          Connection.SendFrame(F);
        end;
      end;
    finally
      F.Free;
    end;
  end;
end;

procedure TTMSFNCCustomWebSocketServer.SetActive(const Value: Boolean);
begin
  if FServerActive <> Value then
  begin
    FServerActive := Value;

    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    HTTPServer.Active := Value;
  end;
end;

procedure TTMSFNCCustomWebSocketServer.SetCertificateFile(const Value: string);
begin
  FCertificateFile := Value;
  if UseSSL then
    SSLHandler.SSLOptions.CertFile := FCertificateFile
end;

procedure TTMSFNCCustomWebSocketServer.SetCertificateKeyFile(const Value: string);
begin
  FCertificateKeyFile := Value;
  if UseSSL then
    SSLHandler.SSLOptions.KeyFile := FCertificateKeyFile;
end;

procedure TTMSFNCCustomWebSocketServer.SetFrameSize(const Value: UInt64);
begin
  if FFrameSize <> Value then
    FFrameSize := Value;
end;

procedure TTMSFNCCustomWebSocketServer.SetOnCommandOther(
  const Value: TIdHTTPCommandEvent);
begin
  FOnCommandOther := Value;
  If Assigned(FHTTPServer) then
    FHTTPServer.OnCommandOther := Value;
end;

procedure TTMSFNCCustomWebSocketServer.SetOptions(const Value: TTMSFNCWebsocketOptions);
begin
  if (FOptions = Value) then exit;
  if Active then
    Raise EWebSocketServer.Create(SErrActive);
  FOptions := Value;
end;

procedure TTMSFNCCustomWebSocketServer.SetRootCertificateFile(const Value: string);
begin
  FRootCertificateFile := Value;
  if UseSSL then
    SSLHandler.SSLOptions.RootCertFile := FRootCertificateFile;
end;

procedure TTMSFNCCustomWebSocketServer.SetSplitMessage(const Value: Boolean);
begin
  if FSplitMessage <> Value then
    FSplitMessage := Value;
end;

procedure TTMSFNCCustomWebSocketServer.SetUseSSL(const Value: Boolean);
begin
  if Active and not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    Raise EWebSocketServer.Create(SErrActive);
  FUseSSL := Value;
end;

class function TTMSFNCCustomWebSocketServer.TransformCloseData(ABytes: TBytes;
  out AReason: string): Word;
begin
  Result := 0;
  aReason := '';
  // 5.5.1 of spec: first 2 bytes must be close reason
  if Length(aBytes) > 1 then
    Result := gStack.NetworkToHost(TTMSFNCWebSocketBitConverter.InTo<Uint16>(aBytes, 0));
  // Rest, if any, is a UTF-8 encoded reason
  if Length(aBytes) > 2 then
    aReason := TEncoding.UTF8.GetString(aBytes, 2, Length(aBytes) - 2);
end;

procedure TTMSFNCCustomWebSocketServer.HandshakeResponseSent(Sender: TObject;
  AConnection: TTMSFNCWebSocketServerConnection);
begin
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(OnHandshakeResponseSent) then
        OnHandshakeResponseSent(Self, AConnection);
    end);
  end
  else
  begin
    if Assigned(OnHandshakeResponseSent) then
      OnHandshakeResponseSent(Self, AConnection);
  end;
end;

procedure TTMSFNCCustomWebSocketServer.HTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
const
  cSleepTime = 50;
var
  Client: TTMSFNCWebSocketServerConnection;
begin
  if (PathName <> '') then
  begin
    if not SameText(PathName, ARequestInfo.URI) and not SameText('/' + PathName, ARequestInfo.URI) then
    begin
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ResponseText := 'NOT FOUND';
      AResponseInfo.WriteContent;
      Exit;
    end;
  end;

  if not ARequestInfo.Connection.Contains('Upgrade') then
    DoHTTPRequest(AContext, ARequestInfo, AResponseInfo)
  else
  begin
    {$IF (DEFINED(IOS) OR DEFINED(ANDROID)) AND (COMPILERVERSION < 34)}
    Client := AContext.DataObject as TTMSFNCWebSocketServerConnection;
    {$ELSE}
    Client := AContext.Data as TTMSFNCWebSocketServerConnection;
    {$ENDIF}
    while Client.Connection.Connected and FHTTPServer.Active do
    begin
      Client.Receive(ARequestInfo, AResponseInfo);
      //Allow the thread to calm down
      TThread.Sleep(cSleepTime);
    end;
  end;
end;

constructor TTMSFNCCustomWebSocketServer.Create;
begin
  Create(nil);
end;

function TTMSFNCCustomWebSocketServer.CreateWebsocketConnection(AContext: TIdContext; AOptions: TTMSFNCWebSocketOptions): TTMSFNCWebSocketServerConnection;
begin
  Result := TTMSFNCWebSocketServerConnection.Create(aContext, aOptions);
end;

procedure TTMSFNCCustomWebSocketServer.HTTPServerConnect(AContext: TIdContext);
var
  Connection: TTMSFNCWebSocketServerConnection;
begin
  Connection := CreateWebsocketConnection(aContext, Options);
  Connections.Add(Connection);
  Connection.OnMessageReceived := MessageReceived;
  Connection.OnBinaryDataReceived := BinaryDataReceived;
  Connection.OnPing := PingReceived;
  Connection.OnPong := PongReceived;
  Connection.OnClose := CloseReceived;
  Connection.OnAllow := AllowConnection;
  Connection.OnHandshakeResponseSent := HandshakeResponseSent;
  Connection.WebSocketVersion := Self.WebSocketVersion;
  {$IF (DEFINED(IOS) OR DEFINED(ANDROID)) AND (COMPILERVERSION < 34)}
  aContext.DataObject := Connection;
  {$ELSE}
  aContext.Data := Connection;
  {$ENDIF}
  if AutoSyncEvents then
  begin
    TThread.Queue(nil,
    procedure
    begin
      if Assigned(OnConnect) then
        OnConnect(Self, Connection);
    end);
  end
  else
  begin
    if Assigned(OnConnect) then
      OnConnect(Self, Connection);
  end;
end;

procedure TTMSFNCCustomWebSocketServer.HTTPServerDisconnect(aContext: TIdContext);
var
  Connection: TTMSFNCWebSocketServerConnection;
begin
  {$IF (DEFINED(IOS) OR DEFINED(ANDROID)) AND (COMPILERVERSION < 34)}
  Connection := aContext.DataObject as TTMSFNCWebSocketServerConnection;
  {$ELSE}
  Connection := aContext.Data as TTMSFNCWebSocketServerConnection;
  {$ENDIF}
  try
    if AutoSyncEvents then
    begin
      TThread.Queue(nil,
      procedure
      begin
        if Assigned(OnDisconnect) then
          OnDisconnect(Self, Connection);
      end);
    end
    else
    begin
      if Assigned(OnDisconnect) then
        OnDisconnect(Self, Connection);
    end;
  finally
    {$IF (DEFINED(IOS) OR DEFINED(ANDROID)) AND (COMPILERVERSION < 34)}
    aContext.DataObject := nil;
    {$ELSE}
    aContext.Data := nil;
    {$ENDIF}
    Connections.Remove(Connection);
    Connection.Free;
  end;
end;

procedure TTMSFNCCustomWebSocketServer.HTTPServerQuerySSLPort(APort: Word;
  var VUseSSL: Boolean);
begin
  VUseSSL := FUseSSL;
end;

procedure TTMSFNCCustomWebSocketServer.Loaded;
begin
  inherited;
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    if FServerActive then
    begin
      FServerActive := False;
      Active := True;
    end;
  end;
end;

{ TTMSFNCWebSocketServerConnection }

constructor TTMSFNCWebSocketServerConnection.Create(APeerThread: TIdContext; AOptions: TTMSFNCWebsocketOptions);
begin
  Inherited Create(aOptions);
  FHandshakeResponseSent := False;
  FPeerThread := APeerThread;
end;

function TTMSFNCWebSocketServerConnection.CheckRequest(const AResponseInfo: TIdHTTPResponseInfo): Boolean;

  function Contains(const aHeader, aText: string): Boolean;
  var
    S: string;
  begin
    Result := False;
    for S in aHeader.Split([',', ' ']) do
      if SameText(Trim(S), aText) then
        Exit(True);
  end;

begin
  // Checks from section 4.2.1.
  with HandshakeRequest do
  begin
    Result := (Host <> '');  // 2.
    // Separate statements to ease debugging
    Result := Result and Contains(Connection, 'upgrade'); // 4.
    if not (twsoSkipUpgradeCheck in Options) then
      Result := Result and Contains(Upgrade, 'websocket'); // 3.
    Result := Result and (Key <> ''); // 5.
    Result := Result and (Version <> ''); // 6.
    if Result and not (twsoSkipVersionCheck In Options) then
      Result := (StrToIntDef(Version, 0) = WebSocketVersion);
  end;
  if Result and Assigned(OnAllow) then
    OnAllow(Self, Self, Result);
  if not Result then
  begin
    AResponseInfo.ResponseNo := 400;
    AResponseInfo.ResponseText := 'BAD REQUEST';
  end;
end;

procedure TTMSFNCWebSocketServerConnection.Receive(const ARequestInfo: TIdHTTPRequestInfo; const AResponseInfo: TIdHTTPResponseInfo);
var
  b: TBytesStream;
  bs: TBytes;
  MustClose: Boolean;
begin
  MustClose := False;
  if not HandshakeCompleted then
  begin
    SaveRequest(aRequestInfo);
    if CheckRequest(aResponseInfo) then
      Handshake(ARequestInfo, AResponseInfo)
    else
    begin
      AResponseInfo.WriteContent;
      MustClose := True;
    end;
  end
  else if (Connection.IOHandler.InputBuffer.Size > 0) then
  begin
    SetLength(bs, Connection.IOHandler.InputBuffer.Size);
    b := TBytesStream.Create(bs);
    try
      Connection.IOHandler.InputBufferToStream(b);
      bs := b.Bytes;
    finally
      b.Free;
    end;

    MustClose := not HandleAllPendingFrames(bs, Connection.IOHandler);
  end;
  if MustClose then
    Connection.Disconnect;
end;

destructor TTMSFNCWebSocketServerConnection.Destroy;
begin
  if OwnsUserData then
    FreeAndNil(FUserData);
  inherited;
end;

function TTMSFNCWebSocketServerConnection.GetHandshakeCompleted: Boolean;
begin
  Result := HandshakeResponseSent;
end;

function TTMSFNCWebSocketServerConnection.GetPeerIP: string;
begin
  if Assigned(Connection.Socket) then
    Result := Connection.Socket.Binding.PeerIP
  else
    Result := '';
end;

function TTMSFNCWebSocketServerConnection.GetConnection: TIDTCPConnection;
begin
  Result := PeerThread.Connection;
end;

procedure TTMSFNCWebSocketServerConnection.SaveRequest(ARequestInfo: TIdHTTPRequestInfo);
begin
  SetHandShakeRequest(TTMSFNCWebSocketRequest.Create(aRequestInfo.URI, ARequestInfo.RawHeaders));
  HandShakeRequest.Connection := aRequestInfo.Connection;
end;

procedure TTMSFNCWebSocketServerConnection.Handshake(const ARequestInfo: TIdHTTPRequestInfo; const AResponseInfo: TIdHTTPResponseInfo);
var
  k: string;
  hash: TIdHashSHA1;
begin
  // Preconditions
  Assert(not HandshakeResponseSent);

  try
    // Read request headers
    AResponseInfo.ResponseNo         := 101;
    AResponseInfo.ResponseText       := 'Switching Protocols';
    //Don't set CloseConnection, Linux bug:
    //https://github.com/IndySockets/Indy/issues/375
    //AResponseInfo.CloseConnection    := False;
    AResponseInfo.Connection         := 'Upgrade';
    AResponseInfo.CustomHeaders.Values['Upgrade'] := 'websocket';

    k := Trim(HandshakeRequest.Key) + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
    hash := TIdHashSHA1.Create;
    try
      k := TIdEncoderMIME.EncodeBytes(hash.HashString(k));
    finally
      hash.Free;
    end;

    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Accept'] := k;
    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Protocol'] := HandshakeRequest.Protocol;
    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Version'] := IntToStr(WebSocketVersion);
    AResponseInfo.WriteHeader;

    FHandshakeResponseSent := True;
//    TThread.Queue(nil,
//    procedure
//    begin
    if Assigned(OnHandshakeResponseSent) then
      OnHandshakeResponseSent(Self, Self);
//    end);
  except
    on E: TTMSFNCWebSocketHandshakeException do
    begin
      // Close the connection if the handshake failed
      Connection.Disconnect;
    end;
  end;
end;

end.
