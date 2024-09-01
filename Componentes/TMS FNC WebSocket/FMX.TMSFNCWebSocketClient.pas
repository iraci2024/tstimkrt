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

unit FMX.TMSFNCWebSocketClient;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  IdStack, IdGlobal, IdStackConsts, System.SysUtils, System.Classes,
  idTCPConnection, idIOhandlerSocket, idIOhandler, IdTCPClient, IdSSLOpenSSL,
  FMX.TMSFNCWebSocketCommon, FMX.TMSFNCCustomComponent, FMX.TMSFNCTypes;

const
  MaxheaderCount = 100;

type
  EWebSocketClient = class(EWebSocket);

  TTMSFNCWebSocketHandshakeRequest = class(TTMSFNCWebSocketRequest)
  private
    FPort: Word;
    FExtraHeaders: TStrings;
    procedure SetExtraHeaders(const Value: TStrings);
  protected
    procedure Add(AHeaders: TStrings; const AName, AValue, ADefault: string);
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    class function GenerateKey: string; static;
    procedure ToStrings(aHeaders: TStrings);
    property Port: Word read FPort write FPort;
    property ExtraHeaders: TStrings read FExtraHeaders write SetExtraHeaders;
  end;

  TTMSFNCWebSocketIncomingResult = (irNone, irOK, irClose);

  TTMSFNCWebSocketHandshakeResponse = class
  private
    FVersion: string;
    FExtensions: String;
    FProtocol: string;
    FUpgrade: string;
    FKey: string;
    FAccept: string;
    FRawHeaders: TStrings;
    FConnection: string;
  public
    constructor Create(aHeaders : TStrings);
    destructor Destroy; override;
    property RawHeaders: TStrings read FRawHeaders;
    property Upgrade: string read FUpgrade;
    property Protocol: string read FProtocol;
    property Version: string read FVersion;
    property Extensions: string read FExtensions;
    property Accept: string read FAccept;
    property Key: string read FKey;
    property Connection: string read FConnection;
  end;

  TTMSFNCWebSocketClientConnection = class(TTMSFNCWebSocketConnection)
  private
    FClient: TIDTCPClient;
    FHandshakeResponse: TTMSFNCWebSocketHandshakeResponse;
  protected
    constructor Create(aClient: TIDTCPClient; AOptions: TTMSFNCWebsocketOptions); reintroduce; overload;
    // Function CreateRequest(aURI : String; aHeaders : TStrings) : TTMSFNCWebSocketRequest;
    function ReadMessage: Boolean; virtual;
    function GetPeerIP: string; override;
    function GetConnection: TIDTCPConnection; override;
    function GetHandshakeCompleted: Boolean; override;
    function CheckIncoming(ATimeOut: Integer): TTMSFNCWebSocketIncomingResult;
    property Client: TIDTCPClient read FClient;
    property HandShakeResponse: TTMSFNCWebSocketHandshakeResponse read FHandshakeResponse write FHandshakeResponse;
  public
    destructor Destroy; override;
  end;

  TTMSFNCWebsocketClientHandShakeEvent = procedure(Sender: TObject; AHeaders: TStrings) of object;
  TTMSFNCWebsocketClientHandshakeResponseEvent = procedure(Sender: TObject; AResponse: TTMSFNCWebSocketHandshakeResponse; var AAllow: Boolean) of object;

  TTMSFNCWebSocketDriverErrorEvent = procedure(Sender: TObject; E: Exception) of object;

  TTMSFNCWebSocketMessageDriver = class
  private
    FInterval: Integer;
    FList: TThreadList;
    FReadSet: TIdSocketList;
    FExceptionSet: TIdSocketList;
    FOnDriverError: TTMSFNCWebSocketDriverErrorEvent;
  protected
    function WaitForData: Boolean;
    function CheckConnections : Boolean; virtual;
    procedure ReadConnections;
    property List: TThreadList read FList;
  public
    constructor Create(aInterval: Integer); virtual;
    destructor Destroy; override;
    procedure AddClient(aConnection: TTMSFNCWebSocketClientConnection);
    procedure RemoveClient(aConnection: TTMSFNCWebSocketClientConnection);
    procedure Execute; virtual; abstract;
    procedure Terminate; virtual; abstract;
    property Interval: Integer read FInterval;
    property OnDriverError: TTMSFNCWebSocketDriverErrorEvent read FOnDriverError write FOnDriverError;
  end;

  // Default message driver, works with thread that checks sockets for available data

  TTMSFNCWebSocketThreadedMessageDriver = class(TTMSFNCWebSocketMessageDriver)
  private
    FThread: TThread;
  protected
    type
      TTMSFNCWebSocketMessageDriverThread = class(TThread)
      public
        FDriver: TTMSFNCWebSocketThreadedMessageDriver;
        constructor Create(aDriver: TTMSFNCWebSocketThreadedMessageDriver);
        procedure Execute; override;
      end;
  public
    procedure Execute; override;
    procedure Terminate; override;
  end;

  TTMSFNCCustomWebsocketClient = class(TTMSFNCCustomComponent)
  private
    class var _defaultDriver: TTMSFNCWebSocketThreadedMessageDriver;
  private
    FPort: Integer;
    FActive: Boolean;
    FLoadActive: Boolean;
    FHostName: string;
    FUseSSL: Boolean;
    FPathName: string;
    FConnectTimeout: Integer;
    FOptions: TTMSFNCWebsocketOptions;
    FClient: TIdTCPClient;
    FCheckTimeOut: Integer;
    FAutoCheckMessages: Boolean;
    FHandShake: TTMSFNCWebSocketHandshakeRequest;
    FMessageDriver: TTMSFNCWebSocketMessageDriver; // Do not free
    FHandshakeResponse: TTMSFNCWebSocketHandshakeResponse;
    FOnSendHandShake: TTMSFNCWebsocketClientHandshakeEvent;
    FOnHandshakeResponse: TTMSFNCWebsocketClientHandshakeResponseEvent;
    FConnection: TTMSFNCWebSocketClientConnection;
    FOnMessageReceived: TTMSFNCWebSocketMessageEvent;
    FOnPing: TTMSFNCWebSocketControlEvent;
    FOnPong: TTMSFNCWebSocketControlEvent;
    FOnDisconnect: TTMSFNCWebSocketNotifyEvent;
    FOnBinaryDataReceived: TTMSFNCWebSocketBinDataEvent;
    FOnClose: TTMSFNCWebSocketControlEvent;
    FOnConnect: TTMSFNCWebSocketNotifyEvent;
    FOrigin: string;
    FAutoCheckInterval: Integer;
    FSplitMessage: Boolean;
    FFrameSize: UInt64;
    FAutoSyncEvents: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetHostName(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetUseSSL(const Value: Boolean);
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetPathName(const Value: string);
    procedure SetCheckTimeOut(const Value: Integer);
    procedure SetOptions(const Value: TTMSFNCWebsocketOptions);
    function IntGetMessageDriver: TTMSFNCWebSocketMessageDriver;
    procedure SetAutoCheckMessages(const Value: Boolean);
    procedure SendHeaders(aHeaders: TStrings);
    procedure SocketDisconnected(Sender: TObject);
    procedure SetOrigin(const Value: string);
    procedure SetAutoCheckInterval(const Value: Integer);
    procedure SetFrameSize(const Value: UInt64);
    procedure SetSplitMessage(const Value: Boolean);
  protected
    function GetInstance: NativeUInt; override;
    function GetDocURL: string; override;
    procedure CheckInactive;
    procedure Loaded; override;
    function CreateClientSocket(AClient: TIdTCPClient): TTMSFNCWebSocketClientConnection; virtual;
    procedure MessageReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AMessage: string); virtual;
    procedure BinaryDataReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes; FrameType: TTMSFNCWebSocketFrameTypes); virtual;
    procedure PingReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes); virtual;
    procedure PongReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes); virtual;
    procedure CloseReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AData: TBytes); virtual;
    function CheckHandShakeResponse(AHeaders: TStrings): Boolean; virtual;
    function CreateHandShakeRequest: TTMSFNCWebSocketHandshakeRequest; virtual;
    function CreateHandshakeResponse(AHeaders: TStrings): TTMSFNCWebSocketHandshakeResponse; virtual;
    procedure SendHandShakeRequest; virtual;
    function ReadHandShakeResponse: Boolean; virtual;
    function DoHandShake: Boolean;
    function CreateMessageDriver: TTMSFNCWebSocketMessageDriver;
    procedure DoHandleDisconnect;
    property AutoSyncEvents: Boolean read FAutoSyncEvents write FAutoSyncEvents default True;
    property Connection: TTMSFNCWebSocketClientConnection read FConnection;
    property MessageDriver: TTMSFNCWebSocketMessageDriver read IntGetMessageDriver;
    //Determine if messages should be split into multiple frames
    property SplitMessage: Boolean read FSplitMessage write SetSplitMessage default False;
    //Frame size if SplitMessage is set to true
    property FrameSize: UInt64 read FFrameSize write SetFrameSize default 0;
    // Host to connect to
    property HostName: string read FHostName write SetHostName;
    // Port to connect to
    property Port: Integer read FPort write SetPort default DefaultPort;
    // Connect/Disconnect
    property Active: Boolean read GetActive write SetActive default False;
    // User SSL when connecting
    property UseSSL: Boolean read FUseSSL write SetUseSSL default False;
    // Timeout for connect
    property ConnectTimeout: Integer read FConnectTimeout write SetConnectTimeout default 0;
    // Path/Document in HTTP URL for GET request
    property PathName: string read FPathName write SetPathName;
    // Origin - optional for non-browser clients
    property Origin: string read FOrigin write SetOrigin;
    // Options
    property Options: TTMSFNCWebsocketOptions read FOptions write SetOptions default [];
    // Check for message timeout
    property CheckTimeOut: Integer read FCheckTimeOut write SetCheckTimeOut default 0;
    // Check messages on regular intervals.
    property AutoCheckMessages: Boolean read FAutoCheckMessages write SetAutoCheckMessages default True;
    property AutoCheckInterval: Integer read FAutoCheckInterval write SetAutoCheckInterval default 50;
    // Called when handshake is about to be sent
    property OnSendHandshake: TTMSFNCWebsocketClientHandshakeEvent read FOnSendHandShake write FOnSendHandshake;
    // Called when handshake response is received
    property OnHandshakeResponse: TTMSFNCWebsocketClientHandshakeResponseEvent read FOnHandshakeResponse write FOnHandshakeResponse;
    // Called when a text message is received.
    property OnMessageReceived: TTMSFNCWebSocketMessageEvent read FOnMessageReceived write FOnMessageReceived;
    // Called when a binary message is received.
    property OnBinaryDataReceived: TTMSFNCWebSocketBinDataEvent read FOnBinaryDataReceived write FOnBinaryDataReceived;
    // Called when a connection is disconnected
    property OnDisconnect: TTMSFNCWebSocketNotifyEvent read FOnDisconnect write FOnDisconnect;
    // Called when a connection is established
    property OnConnect: TTMSFNCWebSocketNotifyEvent read FOnConnect write FOnConnect;
    // Called when a ping is received.
    property OnPing: TTMSFNCWebSocketControlEvent read FOnPing write FOnPing;
    // Called when a pong is received.
    property OnPong: TTMSFNCWebSocketControlEvent read FOnPong write FOnPong;
    // Called when a close message is received.
    property OnClose: TTMSFNCWebSocketControlEvent read FOnClose write FOnClose;
  public
    //class var CheckReadInterval: Integer;
    //class constructor Create;
    //class destructor Destroy;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    // Connect and perform handshake
    procedure Connect;
    // Send raw data
    procedure Send(ABytes: TBytes); overload;
    // Send a string message
    procedure Send(const AMessage: string); overload;
    // Send a ping message
    procedure SendMasked(ABytes: TBytes); overload;
    procedure SendMasked(const AMessage: string); overload;
    procedure Ping(AMessage: string);
    // Disconnect from server.
    procedure Disconnect(ASendClose: Boolean = True);
    // Check for incoming messages
    function CheckIncoming: TTMSFNCWebSocketIncomingResult;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TTMSFNCWebsocketClient = class(TTMSFNCCustomWebsocketClient)
  public
    property Origin;
    property AutoSyncEvents;
  published
    property Active;
    property HostName;
    property Port;
    property UseSSL;
    property PathName;
    property Options;
    property CheckTimeout;
    property AutoCheckMessages;
    property AutoCheckInterval;
    property SplitMessage;
    property FrameSize;
    property ConnectTimeout;
    property OnConnect;
    property OnDisconnect;
    property OnSendHandshake;
    property OnHandshakeResponse;
    property OnBinaryDataReceived;
    property OnMessageReceived;
    property OnPing;
    property OnPong;
  end;

implementation

uses
  System.Types, System.NetEncoding, idCoderMime, IdHashSHA;

const
  SErrTooManyHeaders = 'Too many response headers';

{$R TMSFNCWebSocketClient.res}

{ TTMSFNCCustomWebsocketClient }

procedure TTMSFNCCustomWebsocketClient.CheckInactive;
begin
  if Active and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    raise EWebSocketClient.Create(SErrActive);
end;

function TTMSFNCCustomWebsocketClient.CheckIncoming: TTMSFNCWebSocketIncomingResult;
begin
  If not Active then
    raise EWebSocketClient.Create(SErrInActive);

  if not Connection.HandshakeCompleted then
    raise EWebSocketClient.Create(SErrHandshakeNotComplete);

  Result := Connection.CheckIncoming(CheckTimeout);
  if (Result = irClose) then
    Disconnect(False);
end;

procedure TTMSFNCCustomWebsocketClient.CloseReceived(Sender: TObject;
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

  if AutoCheckMessages then
    MessageDriver.RemoveClient(Connection);

  if Assigned(FClient) then
    FClient.Disconnect;

  FActive := False;
end;

constructor TTMSFNCCustomWebsocketClient.Create;
begin
  Create(nil);
end;

function TTMSFNCCustomWebsocketClient.CreateClientSocket(AClient: TIdTCPClient): TTMSFNCWebSocketClientConnection;
begin
  Result := TTMSFNCWebSocketClientConnection.Create(aClient, FOptions);
end;

procedure TTMSFNCCustomWebsocketClient.SocketDisconnected(Sender: TObject);
begin
  FActive := False;
  if AutoCheckMessages then
    MessageDriver.RemoveClient(FConnection);

  if AutoSyncEvents then
    TThread.Queue(nil, DoHandleDisconnect)
  else
    DoHandleDisconnect;
end;

procedure TTMSFNCCustomWebsocketClient.Connect;
var
  ssl: TIdSSLIOHandlerSocketOpenSSL;
begin
  if Active then
    Exit;

  if not Assigned(FClient) then
    FClient := TIdTCPClient.Create(Self);

  FClient.Host := HostName;
  FClient.Port := Port;
  FClient.ConnectTimeout := Self.ConnectTimeout;
  FClient.OnDisconnected := SocketDisconnected;
  //TODO: provide the user with the possibility
  //to assign their own IOHandler?
  if FUseSSL and (FClient.IOHandler = nil) then
  begin
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(FClient);
    ssl.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    ssl.PassThrough := False;
    FClient.IOHandler := ssl;
  end;
  FClient.Connect;
  FConnection := CreateClientSocket(FClient);
  FConnection.OnMessageReceived := MessageReceived;
  FConnection.OnBinaryDataReceived := BinaryDataReceived;
  FConnection.OnPing := PingReceived;
  FConnection.OnPong := PongReceived;
  FConnection.OnClose := CloseReceived;
  FActive := True;

  if not DoHandShake then
    Disconnect(False)
  else
  begin
    if AutoCheckMessages then
      MessageDriver.AddClient(FConnection);

    if Assigned(OnConnect) then
      OnConnect(Self, FConnection);
  end;
end;

constructor TTMSFNCCustomWebsocketClient.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := [];
  FCheckTimeOut := 0;
  FConnectTimeout := 0;
  FActive := False;
  FPort := DefaultPort;
  FUseSSL := False;
  FAutoCheckMessages := True;
  FAutoSyncEvents := True;
  FAutoCheckInterval := 50;
end;

destructor TTMSFNCCustomWebsocketClient.Destroy;
begin
  Disconnect(False);
  FreeAndNil(FHandShake);
  FreeAndNil(FHandshakeResponse);
  FreeAndNil(FClient);
  FreeAndNil(FConnection);
  inherited;
end;

//class constructor TTMSFNCCustomWebsocketClient.Create;
//begin
//end;

function TTMSFNCCustomWebsocketClient.CreateHandShakeRequest: TTMSFNCWebSocketHandshakeRequest;
begin
  Result := TTMSFNCWebSocketHandshakeRequest.Create;
end;

procedure TTMSFNCCustomWebsocketClient.SendHeaders(AHeaders: TStrings);
var
  LBufferingStarted: Boolean;
  S: string;
  aIO: TIdIOHandler;
begin
  aIO := FClient.IOHandler;
  LBufferingStarted := not aIO.WriteBufferingActive;
  if LBufferingStarted then
    aIO.WriteBufferOpen;

  try
    for S in AHeaders do
      aIO.WriteLn(S);

    aIO.WriteLn;
    if LBufferingStarted then
      aIO.WriteBufferClose;
  except
    if LBufferingStarted then
      aIO.WriteBufferCancel;
    raise;
  end;
end;

procedure TTMSFNCCustomWebsocketClient.SendMasked(const AMessage: string);
begin
  if FSplitMessage then
    Connection.SendInMultipleFrames(AMessage, FFrameSize, True)
  else
    Connection.Send(aMessage, True);
end;

procedure TTMSFNCCustomWebsocketClient.SendMasked(ABytes: TBytes);
begin
  if FSplitMessage then
    Connection.SendBytesInMultipleFrames(ABytes, FFrameSize, True)
  else
    Connection.SendBytes(aBytes, True);
end;

procedure TTMSFNCCustomWebsocketClient.Send(const aMessage: string);
begin
  if FSplitMessage then
    Connection.SendInMultipleFrames(AMessage, FFrameSize)
  else
    Connection.Send(aMessage);
end;

procedure TTMSFNCCustomWebsocketClient.Send(aBytes: TBytes);
begin
  if FSplitMessage then
    Connection.SendBytesInMultipleFrames(ABytes, FFrameSize)
  else
    Connection.SendBytes(aBytes);
end;

procedure TTMSFNCCustomWebsocketClient.SendHandShakeRequest;
var
  aRequest: TTMSFNCWebSocketHandshakeRequest;
  aHeaders: TStrings;
begin
  aHeaders := nil;
  FreeAndNil(FHandShake);
  aRequest := CreateHandShakeRequest;
  try
    aRequest.Host := HostName;
    aRequest.Origin := Origin;
    aRequest.Port := Port;
    aRequest.Resource := PathName;
    aHeaders := TStringList.Create;
    aHeaders.NameValueSeparator := ':';
    aRequest.ToStrings(aHeaders);
    if Assigned(FOnSendHandshake) then
      FOnSendHandshake(self,aHeaders);
    // Do not use FClient.WriteHeader, it messes up the strings !
    SendHeaders(aHeaders);
    FHandShake := aRequest;
  finally
    aHeaders.Free;
    if FhandShake <> aRequest then
      aRequest.Free;
  end;
end;

function TTMSFNCCustomWebsocketClient.CreateHandshakeResponse(AHeaders: TStrings): TTMSFNCWebSocketHandshakeResponse;
begin
  Result := TTMSFNCWebSocketHandshakeResponse.Create(aHeaders);
end;

//class destructor TTMSFNCCustomWebsocketClient.Destroy;
//begin
//  if Assigned(_defaultDriver) then
//  begin
//    _defaultDriver.Terminate;
//    _defaultDriver.Free;
//  end;
//end;

procedure TTMSFNCCustomWebsocketClient.BinaryDataReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const AData: TBytes;
  FrameType: TTMSFNCWebSocketFrameTypes);
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

function TTMSFNCCustomWebsocketClient.CheckHandShakeResponse(AHeaders: TStrings): Boolean;
var
  K: string;
  hash: TIdHashSHA1;
begin
  FreeAndNil(FHandshakeResponse);
  FHandshakeResponse := CreateHandshakeResponse(aHeaders);
  k := Trim(FHandshake.Key) + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  hash := TIdHashSHA1.Create;
  try
    k := TIdEncoderMIME.EncodeBytes(hash.HashString(k));
  finally
    hash.Free;
  end;
  Result := SameText(K, FHandshakeResponse.Accept) and SameText(FHandshakeResponse.Upgrade, 'websocket');
  Result := Result and SameText(FHandshakeResponse.Connection, 'Upgrade');
  if FHandshakeResponse.Version <> '' then
    Result := Result and SameText(FHandshakeResponse.Version, FHandShake.Version);
end;

function TTMSFNCCustomWebsocketClient.ReadHandShakeResponse: Boolean;
var
  S: string;
  aHeaders: TStrings;
begin
  Result := False;
  aHeaders := TStringList.Create;
  try
    aHeaders.NameValueSeparator := ':';
    repeat
      S := FClient.IOHandler.ReadLn;
      aHeaders.Add(S);
    until (S = '') or (aHeaders.Count > MaxheaderCount);
    if (aHeaders.Count > MaxheaderCount) then
      raise EWebSocketClient.Create(SErrTooManyHeaders);

    Result := CheckHandShakeResponse(aHeaders);

    if Result and Assigned(FOnHandshakeResponse) then
       FOnHandshakeResponse(Self, FHandShakeResponse, Result);

    if Result then
      FConnection.HandshakeResponse := FHandShakeResponse;
  finally
    aHeaders.Free;
  end;
end;

procedure TTMSFNCCustomWebsocketClient.DoHandleDisconnect;
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(Self, FConnection);
end;

function TTMSFNCCustomWebsocketClient.DoHandShake: Boolean;
begin
  SendHandShakeRequest;
  Result := ReadHandShakeResponse;
end;

function TTMSFNCCustomWebsocketClient.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FLoadActive
  else
    Result := FActive;
end;

function TTMSFNCCustomWebsocketClient.GetDocURL: string;
begin
  Result := 'https://download.tmssoftware.com/doc/tmsfncwebsocket/components/ttmsfncwebsocketclient';
end;

function TTMSFNCCustomWebsocketClient.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomWebsocketClient.CreateMessageDriver: TTMSFNCWebSocketMessageDriver;
begin
  if _defaultDriver = nil then
  begin
    _defaultDriver := TTMSFNCWebSocketThreadedMessageDriver.Create(AutoCheckInterval);
    _defaultDriver.Execute;
  end;
  Result := _defaultDriver;
end;

function TTMSFNCCustomWebsocketClient.IntGetMessageDriver: TTMSFNCWebSocketMessageDriver;
begin
  if FMessageDriver = nil then
    FMessageDriver := CreateMessageDriver;
  Result := FMessageDriver;
end;

procedure TTMSFNCCustomWebsocketClient.Loaded;
begin
  inherited;
  if FLoadActive and not (csLoading in ComponentState) and not (csDesigning in ComponentState) and not (csReading in ComponentState) then
    Connect;
end;

procedure TTMSFNCCustomWebsocketClient.MessageReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const AMessage: string);
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

procedure TTMSFNCCustomWebsocketClient.Ping(AMessage: String);
begin
  FConnection.SendSimpleFrame(focPing, TEncoding.UTF8.GetBytes(aMessage));
end;

procedure TTMSFNCCustomWebsocketClient.PingReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const AData: TBytes);
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

procedure TTMSFNCCustomWebsocketClient.PongReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const AData: TBytes);
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

procedure TTMSFNCCustomWebsocketClient.Disconnect(ASendClose: Boolean = True);
begin
  if not Active then
    Exit;

  if ASendClose then
    FConnection.SendClose('')
  else
  begin
    if AutoCheckMessages then
      MessageDriver.RemoveClient(Connection);

    if Assigned(FClient) then
      FClient.Disconnect;

    FActive := False;
  end;
end;

procedure TTMSFNCCustomWebsocketClient.SetActive(const Value: Boolean);
begin
  FLoadActive := Value;
  if (csDesigning in ComponentState) then
    Exit;

  if Value then
    Connect
  else
    Disconnect;
end;

procedure TTMSFNCCustomWebsocketClient.SetAutoCheckInterval(const Value: Integer);
begin
  CheckInactive;
  FAutoCheckInterval := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetAutoCheckMessages(const Value: Boolean);
begin
  CheckInactive;
  FAutoCheckMessages := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetCheckTimeOut(const Value: Integer);
begin
  CheckInactive;
  FCheckTimeOut := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetConnectTimeout(const Value: Integer);
begin
  CheckInactive;
  FConnectTimeout := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetFrameSize(const Value: UInt64);
begin
  if FFrameSize <> Value then
    FFrameSize := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetHostName(const Value: String);
begin
  CheckInactive;
  FHostName := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetOptions(const Value: TTMSFNCWebsocketOptions);
begin
  CheckInactive;
  FOptions := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetOrigin(const Value: string);
begin
  CheckInactive;
  FOrigin := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetPort(const Value: Integer);
begin
  CheckInactive;
  FPort := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetSplitMessage(const Value: Boolean);
begin
  if FSplitMessage <> Value then
    FSplitMessage := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetPathName(const Value: string);
begin
  CheckInactive;
  FPathName := Value;
end;

procedure TTMSFNCCustomWebsocketClient.SetUseSSL(const Value: Boolean);
begin
  CheckInactive;
  FUseSSL := Value;
end;

{ TTMSFNCWebSocketHandshakeRequest }

procedure TTMSFNCWebSocketHandshakeRequest.Add(aHeaders: TStrings; const AName, aValue,
  ADefault: string);
var
  V: string;
begin
  V := aValue;
  if V = '' then
    V := aDefault;

  if V <> '' then
    aHeaders.Add(aName + ': ' + V);
end;

class function TTMSFNCWebSocketHandshakeRequest.GenerateKey: string;
var
  I: Integer;
  B: TIdBytes;
begin
  SetLength(B, 16);
  for I := 0 to 15 do
    B[i] := Random(256);

  Result := TIdEncoderMIME.EncodeBytes(B);
end;

constructor TTMSFNCWebSocketHandshakeRequest.Create;
begin
  Version := IntToStr(CurrentWebSocketVersion);
  FExtraHeaders := TStringList.Create;
  FExtraHeaders.NameValueSeparator := ':';
end;

destructor TTMSFNCWebSocketHandshakeRequest.Destroy;
begin
  FreeAndNil(FExtraHeaders);
  inherited;
end;

procedure TTMSFNCWebSocketHandshakeRequest.SetExtraHeaders(const Value: TStrings);
begin
  FExtraHeaders.Clear;
  FExtraHeaders.AddStrings(Value);
end;

procedure TTMSFNCWebSocketHandshakeRequest.ToStrings(AHeaders: TStrings);
var
  V: string;
begin
  aHeaders.Clear;
  if Resource = '' then
    Resource := '/';

  aHeaders.Add('GET ' + Resource + ' HTTP/1.1');
  V := Host;
  if (V <> '') and (Port <> 443) and (Port <> 80) then
    V := V + ':' + IntToStr(Port);
  Add(aHeaders, 'Host', V, '');
  Add(aHeaders, 'Upgrade', Upgrade, 'websocket');
  Add(aHeaders, 'Connection', Connection, 'Upgrade');
  Add(aHeaders, 'Origin', Origin, '');
  if Key = '' then
    Key := GenerateKey;

  Add(aHeaders, 'Sec-WebSocket-Key', Key, '');
  Add(aHeaders, 'Sec-WebSocket-Protocol', Protocol, '');
  Add(aHeaders, 'Sec-WebSocket-Version', Version, '');
  aHeaders.AddStrings(ExtraHeaders);
end;

{ TTMSFNCWebSocketHandshakeResponse }

constructor TTMSFNCWebSocketHandshakeResponse.Create(AHeaders: TStrings);
  function Get(AName: string): string;
  begin
    Result := Trim(aHeaders.Values[aName]);
  end;
begin
  FRawHeaders := TstringList.Create;
  FRawHeaders.NameValueSeparator := ':';
  FRawHeaders.AddStrings(aHeaders);
  FProtocol := Get('Sec-WebSocket-Protocol');
  FVersion := Get('Sec-WebSocket-Version');
  FAccept := Get('Sec-WebSocket-Accept');
  FKey := Get('Sec-WebSocket-Key');
  FUpgrade := Get('Upgrade');
  FConnection := Get('Connection');
end;

destructor TTMSFNCWebSocketHandshakeResponse.Destroy;
begin
  FreeAndNil(FRawHeaders);
  inherited;
end;

{ TTMSFNCWebSocketClientConnection }


function TTMSFNCWebSocketClientConnection.ReadMessage: Boolean;
var
  bs: TBytes;
  B: TBytesStream;
begin
  Result := False;
  if not Client.IOHandler.CheckForDataOnSource(0) then
    Exit;

  if (Client.IOHandler.InputBuffer.Size > 0) then
  begin
    SetLength(bs, Client.IOHandler.InputBuffer.Size);
    b := TBytesStream.Create(bs);
    try
      Client.IOHandler.InputBufferToStream(b);
      bs := b.Bytes;
    finally
      b.Free;
    end;

    Result := HandleAllPendingFrames(bs, Client.IOHandler);
  end;
end;

function TTMSFNCWebSocketClientConnection.CheckIncoming(ATimeout: Integer): TTMSFNCWebSocketIncomingResult;
begin
  if not FClient.IOHandler.Readable(aTimeout) then
    Result := irNone
  else
  begin
    if ReadMessage then
      Result := irOK
    else
      Result := irClose;
  end;
end;

constructor TTMSFNCWebSocketClientConnection.Create(AClient: TIDTCPClient; AOptions: TTMSFNCWebsocketOptions);
begin
  inherited Create(aOptions);
  FClient := aClient;
end;

destructor TTMSFNCWebSocketClientConnection.Destroy;
begin
  inherited;
end;

function TTMSFNCWebSocketClientConnection.GetHandshakeCompleted: Boolean;
begin
  Result := Assigned(FHandshakeResponse);
end;

function TTMSFNCWebSocketClientConnection.GetPeerIP: string;
begin
  Result := FClient.BoundIP;
end;

function TTMSFNCWebSocketClientConnection.GetConnection: TIDTCPConnection;
begin
  Result := FClient;
end;

{ TTMSFNCWebSocketMessageDriver }

procedure TTMSFNCWebSocketMessageDriver.AddClient(
  aConnection: TTMSFNCWebSocketClientConnection);
begin
  List.Add(aConnection);
end;

procedure TTMSFNCWebSocketMessageDriver.RemoveClient(aConnection: TTMSFNCWebSocketClientConnection);
begin
  FList.Remove(aConnection);
end;

function TTMSFNCWebSocketMessageDriver.WaitForData: Boolean;
begin
  Result := False;
  // FReadSet was populated by checkconnections
  FExceptionSet.Clear;
  if FReadSet.Count = 0 then
  begin
    TThread.Sleep(FInterval);
  end
  else
  begin
    try
      Result := FReadSet.Select(FReadSet, nil, FExceptionSet, FInterval);
    except
      Result := False;
    end;
  end;
end;

function TTMSFNCWebSocketMessageDriver.CheckConnections: Boolean;
var
  aList: TList;
  aClient: TTMSFNCWebSocketClientConnection;
  aSock: TIdIOHandlerSocket;
  I: Integer;
begin
  Result := False;
  aList := List.LockList;
  try
    FReadSet.Clear;
    for I := 0 to aList.Count - 1 do
    begin
      aClient := TTMSFNCWebSocketClientConnection(aList.Items[I]);
      if Assigned(aClient.Client) then
        aSock := aClient.Client.Socket
      else
        aSock := nil;

      if (aSock <> nil) and (asock.Binding <> nil) and (aSock.Binding.Handle > 0) and (aSock.Binding.Handle <> Id_INVALID_SOCKET) then
      begin
        // There is already data
        if not aClient.Client.IOHandler.InputBufferIsEmpty then
          Result := True;
        FReadSet.Add(aSock.Binding.Handle);
      end;
    end;
  finally
    List.UnlockList;
  end;
  if not Result then
    Result := WaitForData;
end;

constructor TTMSFNCWebSocketMessageDriver.Create(AInterval: Integer);
begin
  FList := TThreadList.Create;
  FReadSet := TIdSocketList.CreateSocketList;
  FExceptionSet := TIdSocketList.CreateSocketList;
  Finterval := aInterval;
end;

destructor TTMSFNCWebSocketMessageDriver.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FReadSet);
  FreeAndNil(FExceptionSet);
  inherited;
end;

procedure TTMSFNCWebSocketMessageDriver.ReadConnections;
var
  aList: TList;
  aClient: TTMSFNCWebSocketClientConnection;
  I: Integer;
begin
  try
    aList := List.LockList;
    try
      FReadSet.Clear;
      for I := 0 to aList.Count - 1 do
      begin
        aClient := TTMSFNCWebSocketClientConnection(aList.Items[I]);
        if Assigned(aClient.Client) then
        begin
          if not aClient.Client.IOHandler.InputBufferIsEmpty then
            aClient.CheckIncoming(0)
          else if aClient.Client.IOHandler.Readable(0) then
            aClient.CheckIncoming(0);
        end;
      end;
    finally
      List.UnlockList;
    end;
  except
    on E: Exception do
    begin
      if Assigned(OnDriverError) then
        OnDriverError(Self, E);
    end;
  end;
end;

{ TTMSFNCWebSocketThreadedMessageDriver }

procedure TTMSFNCWebSocketThreadedMessageDriver.Execute;
begin
  FThread := TTMSFNCWebSocketMessageDriverThread.Create(Self);
end;

procedure TTMSFNCWebSocketThreadedMessageDriver.Terminate;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

{ TTMSFNCWebSocketThreadedMessageDriver.TTMSFNCWebSocketMessageDriverThread }

constructor TTMSFNCWebSocketThreadedMessageDriver.TTMSFNCWebSocketMessageDriverThread.Create(ADriver: TTMSFNCWebSocketThreadedMessageDriver);
begin
  FDriver := aDriver;
  inherited Create;
end;

procedure TTMSFNCWebSocketThreadedMessageDriver.TTMSFNCWebSocketMessageDriverThread.Execute;
begin
  while not Terminated do
  begin
    if FDriver.CheckConnections then
      FDriver.ReadConnections
    else
      TThread.Sleep(FDriver.Interval);
  end;
end;

initialization

finalization
begin
  if Assigned(TTMSFNCCustomWebsocketClient._defaultDriver) then
  begin
    TTMSFNCCustomWebsocketClient._defaultDriver.Terminate;
    TTMSFNCCustomWebsocketClient._defaultDriver.Free;
  end;
end;

end.
