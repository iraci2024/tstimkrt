{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_STOMP_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Protocol_Base_Client, sgcWebSocket_Classes, sgcSTOMP,
  sgcWebSocket_Protocol_STOMP_Message, sgcWebSocket_Classes_SyncObjs;

type
  TsgcWSSTOMPConnectedEvent = procedure(Connection: TsgcWSConnection;
    Version, Server, Session, HeartBeat, RawHeaders: String) of object;
  TsgcWSSTOMPErrorEvent = procedure(Connection: TsgcWSConnection;
    MessageText, ContentType: String; ContentLength: Integer;
    ReceiptId, RawHeaders: String) of object;
  TsgcWSSTOMPMessageEvent = procedure(Connection: TsgcWSConnection;
    MessageText, Destination, MessageId, Subscription, ACK, ContentType,
    RawHeaders: String) of object;
  TsgcWSSTOMPReceiptEvent = procedure(Connection: TsgcWSConnection;
    ReceiptId, RawHeaders: String) of object;
  TsgcWSSTOMPDisconnectedEvent = procedure(Connection: TsgcWSConnection; Code:
      Integer) of object;


  TsgcWSSTOMPAuthentication_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FPassword: String;
    FUserName: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Password: String read FPassword write FPassword;
    property UserName: String read FUserName write FUserName;
  end;

  TsgcWSSTOMPHeartBeat_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FIncoming: Integer;
    FOutgoing: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Incoming: Integer read FIncoming write FIncoming;
    property Outgoing: Integer read FOutgoing write FOutgoing;
  end;

  TsgcWSSTOMPVersion_Options = class(TPersistent)
  private
    FV1_2: Boolean;
    FV1_0: Boolean;
    FV1_1: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property V1_2: Boolean read FV1_2 write FV1_2;
    property V1_0: Boolean read FV1_0 write FV1_0;
    property V1_1: Boolean read FV1_1 write FV1_1;
  end;

  TsgcWSSTOMP_Options = class(TPersistent)
  private
    FVirtualHost: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property VirtualHost: String read FVirtualHost write FVirtualHost;
  end;

  TsgcWSProtocol_STOMP_Client_Base = class(TsgcWSProtocol_Client_Base)
    { wsmessage }
  private
    FWSMessageId: String;
  protected
    FWSMessage: TsgcWSSTOMPMessage;
    function GetWSMessage: TsgcWSSTOMPMessage;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSSTOMPMessage;
  protected
    procedure OnSTOMPConnectedEvent(Sender: TObject;
      aVersion, aServer, aSession, aHeartBeat, aRawHeaders: String);
    procedure OnSTOMPErrorEvent(Sender: TObject;
      aMessageText, aContentType: String; aContentLength: Integer;
      aReceiptId, aRawHeaders: String);
    procedure OnSTOMPMessageEvent(Sender: TObject;
      aMessageText, aDestination, aMessageId, aSubscription, aACK,
      aContentType, aRawHeaders: String);
    procedure OnSTOMPReceiptEvent(Sender: TObject;
      aReceiptId, aRawHeaders: String);
  protected
    property WSMessage: TsgcWSSTOMPMessage read GetWSMessage write FWSMessage;
    { wsmessage }

    { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection; Data:
        TMemoryStream); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
      override;
    { from TsgcWSComponent }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { from TsgcWSProtocol_Client_Base }
  protected
    procedure DoWriteRawData(const aText: String); virtual;
  public
    { from TsgcWSProtocol_Client_Base }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { procedures }
  protected
    FConnectHeaders: TStringList;
    function GetConnectHeaders: TStringList;
    procedure DoConnect(const aConnection: TsgcWSConnection); virtual;
  protected
    procedure Send(const aDestination, aText: string;
      const aContentType: String = ''; const aTransaction: String = '';
      const aCustomHeaders: TStrings = nil); virtual;
    procedure Subscribe(const aId, aDestination: string;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aCustomHeaders: TStrings = nil); virtual;
    procedure UnSubscribe(const aId: string;
      const aCustomHeaders: TStrings = nil); virtual;
    procedure ACK(const aId: string; const aTransaction: String = ''); virtual;
    procedure NACK(const aId: string; const aTransaction: String = ''); virtual;
    procedure BeginTransaction(const aTransaction: String); virtual;
    procedure CommitTransaction(const aTransaction: String); virtual;
    procedure AbortTransaction(const aTransaction: String); virtual;
    procedure Disconnect(const aGraceful: Boolean = True); virtual;
    { procedures }

    { Authentication }
  private
    FAuthentication: TsgcWSSTOMPAuthentication_Options;
    procedure SetAuthentication(const Value: TsgcWSSTOMPAuthentication_Options);
  public
    property Authentication
      : TsgcWSSTOMPAuthentication_Options read FAuthentication write
      SetAuthentication;
    { Authentication }

    { heartbeat }
  private
    FHeartBeat: TsgcWSSTOMPHeartBeat_Options;
    procedure SetHeartBeat(const Value: TsgcWSSTOMPHeartBeat_Options);
  public
    property HeartBeat: TsgcWSSTOMPHeartBeat_Options read FHeartBeat write
      SetHeartBeat;
    { heartbeat }

    { Versions }
  private
    FVersions: TsgcWSSTOMPVersion_Options;
    function GetProtocolByVersion: String;
    procedure SetVersions(const Value: TsgcWSSTOMPVersion_Options);
  public
    property Versions: TsgcWSSTOMPVersion_Options read FVersions write SetVersions;
    { Versions }

    { options }
  private
    FOptions: TsgcWSSTOMP_Options;
    procedure SetOptions(const Value: TsgcWSSTOMP_Options);
  public
    property Options: TsgcWSSTOMP_Options read FOptions write SetOptions;
    { options }

    { events }
  private
    FOnSTOMPConnected: TsgcWSSTOMPConnectedEvent;
    FOnSTOMPDisconnected: TsgcWSSTOMPDisconnectedEvent;
    FOnSTOMPError: TsgcWSSTOMPErrorEvent;
    FOnSTOMPMessage: TsgcWSSTOMPMessageEvent;
    FOnSTOMPReceipt: TsgcWSSTOMPReceiptEvent;
    procedure DoSTOMPConnectedEvent(aVersion, aServer, aSession, aHeartBeat,
      aRawHeaders: String);
    procedure DoSTOMPErrorEvent(aMessageText, aContentType: String;
      aContentLength: Integer; aReceiptId, aRawHeaders: String);
    procedure DoSTOMPMessageEvent(aMessageText, aDestination, aMessageId,
      aSubscription, aACK, aContentType, aRawHeaders: String);
    procedure DoSTOMPReceiptEvent(aReceiptId, aRawHeaders: String);
    procedure DoSTOMPDisconnectedEvent(Code: Integer);
  public
    property OnSTOMPError
      : TsgcWSSTOMPErrorEvent read FOnSTOMPError write FOnSTOMPError;
    property OnSTOMPConnected
      : TsgcWSSTOMPConnectedEvent read FOnSTOMPConnected write
      FOnSTOMPConnected;
    property OnSTOMPMessage: TsgcWSSTOMPMessageEvent read FOnSTOMPMessage write
      FOnSTOMPMessage;
    property OnSTOMPReceipt: TsgcWSSTOMPReceiptEvent read FOnSTOMPReceipt write
      FOnSTOMPReceipt;
    property OnSTOMPDisconnected: TsgcWSSTOMPDisconnectedEvent read
        FOnSTOMPDisconnected write FOnSTOMPDisconnected;
    { events }
  end;


  TsgcWSProtocol_STOMP_Client = class(TsgcWSProtocol_STOMP_Client_Base)
  public
    procedure Send(const aDestination, aText: string; const aContentType: String =
        ''; const aTransaction: String = ''; const aCustomHeaders: TStrings = nil);
        override;
    procedure Subscribe(const aId, aDestination: string; const aACK: TsgcSTOMPACK =
        ackAuto; const aCustomHeaders: TStrings = nil); override;
    procedure UnSubscribe(const aId: string; const aCustomHeaders: TStrings = nil);
        override;
    procedure ACK(const aId: string; const aTransaction: String = ''); override;
    procedure NACK(const aId: string; const aTransaction: String = ''); override;
    procedure BeginTransaction(const aTransaction: String); override;
    procedure CommitTransaction(const aTransaction: String); override;
    procedure AbortTransaction(const aTransaction: String); override;
    procedure Disconnect(const aGraceful: Boolean = True); override;
  public
    property ConnectHeaders: TStringList read GetConnectHeaders write FConnectHeaders;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses sgcBase_Helpers, sgcWebSocket_Const, sgcWebSocket_Helpers;

const
  CS_QOS_LIST = 0;

constructor TsgcWSProtocol_STOMP_Client_Base.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageId := NewGuid;
  FVersions := TsgcWSSTOMPVersion_Options.Create;
  FProtocol := GetProtocolByVersion;
  FAuthentication := TsgcWSSTOMPAuthentication_Options.Create;
  FAuthentication.Enabled := False;
  FHeartBeat := TsgcWSSTOMPHeartBeat_Options.Create;
  FHeartBeat.Enabled := True;
  FOptions := TsgcWSSTOMP_Options.Create;
end;

destructor TsgcWSProtocol_STOMP_Client_Base.Destroy;
begin
  sgcFree(FHeartBeat);
  sgcFree(FConnectHeaders);
  sgcFree(FOptions);
  sgcFree(FVersions);
  sgcFree(FAuthentication);
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client_Base.AbortTransaction
  (const aTransaction: String);
begin
  WriteData(WSMessage.AbortTransaction(aTransaction));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.ACK(const aId: string; const
    aTransaction: String = '');
begin
  WriteData(WSMessage.ACK(aId, aTransaction));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.BeginTransaction
  (const aTransaction: String);
begin
  WriteData(WSMessage.BeginTransaction(aTransaction));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.CommitTransaction
  (const aTransaction: String);
begin
  WriteData(WSMessage.CommitTransaction(aTransaction));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
{$ENDIF}
  if DoRawMessage(aConnection, Text) then
    exit;

  if Text = WSMessage.Ping then
    WriteData(WSMessage.Pong)
  else
  begin
    if not WSMessage.Read(Text) then
      inherited;
  end;
end;

function TsgcWSProtocol_STOMP_Client_Base.GetWSMessage: TsgcWSSTOMPMessage;
begin
  result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessage) then
    begin
      FWSMessage := TsgcWSSTOMPMessage.Create(self);
      FWSMessage.OnConnected := OnSTOMPConnectedEvent;
      FWSMessage.OnError := OnSTOMPErrorEvent;
      FWSMessage.OnMessage := OnSTOMPMessageEvent;
      FWSMessage.OnReceipt := OnSTOMPReceiptEvent;
    end;
    result := FWSMessage;
  end;
end;

procedure TsgcWSProtocol_STOMP_Client_Base.Disconnect
  (const aGraceful: Boolean = True);
begin
  WriteData(WSMessage.Disconnect(aGraceful));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoClear(aConnection:
    TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoConnect(aConnection);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
var
  vId: String;
begin
  vId := aConnection.Guid;
  if Assigned(FWSMessage) then
    FWSMessage.Clear;
  DoSTOMPDisconnectedEvent(Code);
  inherited;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoFinalize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  if Assigned(FWSMessage) then
    FWSMessage.Clear;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoConnect(aConnection);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoConnect(const aConnection:
    TsgcWSConnection);
var
  oMessage: TsgcWSSTOMPMessage;
begin
  oMessage := GetWSMessageByConnection(aConnection);

  oMessage.Authentication.Enabled := Authentication.Enabled;
  oMessage.Authentication.UserName := Authentication.UserName;
  oMessage.Authentication.Password := Authentication.Password;

  oMessage.HeartBeat.Enabled := HeartBeat.Enabled;
  oMessage.HeartBeat.Incoming := HeartBeat.Incoming;
  oMessage.HeartBeat.Outgoing := HeartBeat.Outgoing;

  oMessage.Version.V1_2 := Versions.V1_2;
  oMessage.Version.V1_1 := Versions.V1_1;
  oMessage.Version.V1_0 := Versions.V1_0;

  WriteData(oMessage.Connect(Options.VirtualHost, GetConnectHeaders));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoEventBinary(const aConnection:
    TsgcWSConnection; Data: TMemoryStream);
begin
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoSTOMPConnectedEvent(aVersion, aServer,
  aSession, aHeartBeat, aRawHeaders: String);
begin
  if Assigned(FOnSTOMPConnected) then
    FOnSTOMPConnected(FWSConnection, aVersion, aServer, aSession, aHeartBeat,
      aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoSTOMPDisconnectedEvent(Code: Integer);
begin
  if Assigned(FOnSTOMPDisconnected) then
    FOnSTOMPDisconnected(FWSConnection, Code);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoSTOMPErrorEvent(aMessageText,
  aContentType: String; aContentLength: Integer;
  aReceiptId, aRawHeaders: String);
begin
  if Assigned(FOnSTOMPError) then
    FOnSTOMPError(FWSConnection, aMessageText, aContentType, aContentLength,
      aReceiptId, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoSTOMPMessageEvent(aMessageText,
  aDestination, aMessageId, aSubscription, aACK, aContentType,
  aRawHeaders: String);
begin
  if Assigned(FOnSTOMPMessage) then
    FOnSTOMPMessage(FWSConnection, aMessageText, aDestination, aMessageId,
      aSubscription, aACK, aContentType, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoSTOMPReceiptEvent(aReceiptId,
  aRawHeaders: String);
begin
  if Assigned(FOnSTOMPReceipt) then
    FOnSTOMPReceipt(FWSConnection, aReceiptId, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.DoWriteRawData(const aText: String);
begin
  inherited WriteData(aText);
end;

function TsgcWSProtocol_STOMP_Client_Base.GetConnectHeaders: TStringList;
begin
  if not Assigned(FConnectHeaders) then
    FConnectHeaders := TStringList.Create;
  Result := FConnectHeaders;
end;

function TsgcWSProtocol_STOMP_Client_Base.GetProtocolByVersion: String;
var
  oList: TsgcDelimitedStringList;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    if Versions.V1_2 then
      oList.Add('v12.stomp');
    if Versions.V1_1 then
      oList.Add('v11.stomp');
    if Versions.V1_0 then
      oList.Add('v10.stomp');
    result := oList.CommaText;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSProtocol_STOMP_Client_Base.GetWSMessageByConnection(const
    aConnection: TsgcWSConnection): TsgcWSSTOMPMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSSTOMPMessage.Create(nil);
      TsgcWSSTOMPMessage(oItem.WSMessage).OnConnected := OnSTOMPConnectedEvent;
      TsgcWSSTOMPMessage(oItem.WSMessage).OnError := OnSTOMPErrorEvent;
      TsgcWSSTOMPMessage(oItem.WSMessage).OnMessage := OnSTOMPMessageEvent;
      TsgcWSSTOMPMessage(oItem.WSMessage).OnReceipt := OnSTOMPReceiptEvent;
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    result := TsgcWSSTOMPMessage(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_STOMP_Client_Base.NACK(const aId: string; const
    aTransaction: String = '');
begin
  WriteData(WSMessage.NACK(aId, aTransaction));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.OnSTOMPConnectedEvent(Sender: TObject;
  aVersion, aServer, aSession, aHeartBeat, aRawHeaders: String);
begin
  DoSTOMPConnectedEvent(aVersion, aServer, aSession, aHeartBeat, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.OnSTOMPErrorEvent(Sender: TObject;
  aMessageText, aContentType: String; aContentLength: Integer;
  aReceiptId, aRawHeaders: String);
begin
  DoSTOMPErrorEvent(aMessageText, aContentType, aContentLength, aReceiptId,
    aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.OnSTOMPMessageEvent(Sender: TObject;
  aMessageText, aDestination, aMessageId, aSubscription, aACK, aContentType,
  aRawHeaders: String);
begin
  DoSTOMPMessageEvent(aMessageText, aDestination, aMessageId, aSubscription,
    aACK, aContentType, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.OnSTOMPReceiptEvent(Sender: TObject;
  aReceiptId, aRawHeaders: String);
begin
  DoSTOMPReceiptEvent(aReceiptId, aRawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.Send(const aDestination, aText: string;
  const aContentType: String = ''; const aTransaction: String = '';
  const aCustomHeaders: TStrings = nil);
begin
  WriteData(WSMessage.Send(aDestination, aText, aContentType, aTransaction,
      aCustomHeaders));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.SetAuthentication
  (const Value: TsgcWSSTOMPAuthentication_Options);
begin
  FAuthentication.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.SetHeartBeat
  (const Value: TsgcWSSTOMPHeartBeat_Options);
begin
  FHeartBeat.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.SetOptions
  (const Value: TsgcWSSTOMP_Options);
begin
  FOptions.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.SetVersions(const Value:
    TsgcWSSTOMPVersion_Options);
begin
  FVersions.Assign(Value);
end;

procedure TsgcWSProtocol_STOMP_Client_Base.Subscribe(const aId,
  aDestination: string; const aACK: TsgcSTOMPACK = ackAuto;
  const aCustomHeaders: TStrings = nil);
begin
  WriteData(WSMessage.Subscribe(aId, aDestination, aACK, aCustomHeaders));
end;

procedure TsgcWSProtocol_STOMP_Client_Base.UnSubscribe(const aId: string;
  const aCustomHeaders: TStrings = nil);
begin
  WriteData(WSMessage.UnSubscribe(aId, aCustomHeaders));
end;

constructor TsgcWSSTOMPAuthentication_Options.Create;
begin
  inherited;
  Enabled := False;
  UserName := '';
  Password := '';
end;

procedure TsgcWSSTOMPAuthentication_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSTOMPAuthentication_Options then
  begin
    Enabled := TsgcWSSTOMPAuthentication_Options(aSource).Enabled;
    UserName := TsgcWSSTOMPAuthentication_Options(aSource).UserName;
    Password := TsgcWSSTOMPAuthentication_Options(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSSTOMPHeartBeat_Options.Create;
begin
  inherited;
  Enabled := True;
  Incoming := 0;
  Outgoing := 0;
end;

procedure TsgcWSSTOMPHeartBeat_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSTOMPHeartBeat_Options then
  begin
    Enabled := TsgcWSSTOMPHeartBeat_Options(aSource).Enabled;
    Incoming := TsgcWSSTOMPHeartBeat_Options(aSource).Incoming;
    Outgoing := TsgcWSSTOMPHeartBeat_Options(aSource).Outgoing;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSSTOMPVersion_Options.Create;
begin
  inherited;
  V1_0 := True;
  V1_1 := True;
  V1_2 := True;
end;

procedure TsgcWSSTOMPVersion_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSTOMPVersion_Options then
  begin
    V1_0 := TsgcWSSTOMPVersion_Options(aSource).V1_0;
    V1_1 := TsgcWSSTOMPVersion_Options(aSource).V1_1;
    V1_2 := TsgcWSSTOMPVersion_Options(aSource).V1_2;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSSTOMP_Options.Create;
begin
  inherited;
  VirtualHost := '/';
end;

procedure TsgcWSSTOMP_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSTOMP_Options then
    VirtualHost := TsgcWSSTOMP_Options(aSource).VirtualHost
  else
    inherited Assign(aSource);
end;

procedure TsgcWSProtocol_STOMP_Client.AbortTransaction(const aTransaction:
    String);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.ACK(const aId: string; const
    aTransaction: String = '');
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.BeginTransaction(const aTransaction:
    String);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.CommitTransaction(const aTransaction:
    String);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.Disconnect(const aGraceful: Boolean =
    True);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.NACK(const aId: string; const
    aTransaction: String = '');
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.Send(const aDestination, aText: string;
    const aContentType: String = ''; const aTransaction: String = ''; const
    aCustomHeaders: TStrings = nil);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.Subscribe(const aId, aDestination:
    string; const aACK: TsgcSTOMPACK = ackAuto; const aCustomHeaders: TStrings
    = nil);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Client.UnSubscribe(const aId: string; const
    aCustomHeaders: TStrings = nil);
begin
  inherited;
end;

{$ENDIF}

end.
