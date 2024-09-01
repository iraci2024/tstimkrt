{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcSTOMP;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils, Math;

type

  TsgcSTOMPConnectedEvent = procedure(Sender: TObject;
    Version, Server, Session, HeartBeat, RawHeaders: String) of object;
  TsgcSTOMPErrorEvent = procedure(Sender: TObject;
    MessageText, ContentType: String; ContentLength: Integer;
    ReceiptId, RawHeaders: String) of object;
  TsgcSTOMPMessageEvent = procedure(Sender: TObject;
    MessageText, Destination, MessageId, Subscription, ACK, ContentType,
    RawHeaders: String) of object;
  TsgcSTOMPReceiptEvent = procedure(Sender: TObject;
    ReceiptId, RawHeaders: String) of object;

  TsgcSTOMPACK = (ackAuto, ackMultiple, ackIndividual);

  TsgcSTOMPAuthentication = class(TPersistent)
  private
    FEnabled: Boolean;
    FPassword: string;
    FUsername: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Password: string read FPassword write FPassword;
    property Username: String read FUsername write FUsername;
  end;

  TsgcSTOMPHeartBeat = class(TPersistent)
  private
    FEnabled: Boolean;
    FIncoming: Integer;
    FOutgoing: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Incoming: Integer read FIncoming write FIncoming;
    property Outgoing: Integer read FOutgoing write FOutgoing;
  end;

  TsgcSTOMPVersion = class(TPersistent)
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

  TsgcSTOMP = class(TComponent)
    { stomp parser }
  private
    FHeaders: TStringList;
    FBody: String;
  protected
    procedure DoClear;
    procedure AddCommand(const aCommand: String);
    procedure AddHeader(const aName, aValue: String);
    function Write: String;
  public
    procedure Clear;
    { stomp parser }

    { authentication }
  private
    FAuthentication: TsgcSTOMPAuthentication;
    procedure SetAuthentication(const Value: TsgcSTOMPAuthentication);
  public
    property Authentication: TsgcSTOMPAuthentication read FAuthentication write
      SetAuthentication;
    { authentication }

    { heartbeat }
  private
    FHeartBeat: TsgcSTOMPHeartBeat;
    procedure SetHeartBeat(const Value: TsgcSTOMPHeartBeat);
  public
    property HeartBeat: TsgcSTOMPHeartBeat read FHeartBeat write SetHeartBeat;
    { heartbeat }

    { version }
  private
    FVersion: TsgcSTOMPVersion;
    function GetAcceptVersion: string;
    procedure SetVersion(const Value: TsgcSTOMPVersion);
  public
    property Version: TsgcSTOMPVersion read FVersion write SetVersion;
    { version }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { client frames }
  private
    FReceipt: Integer;
  private
    function ReadContentLength(const aMessage: String): Integer;
  protected
    procedure DoConnect(const aHost: String; const aCustomHeaders: TStrings = nil);
    procedure DoSend(const aDestination, aMessage, aContentType,
      aTransaction: string; const aCustomHeaders: TStrings);
    procedure DoSubscribe(const aId, aDestination: string;
      const aACK: TsgcSTOMPACK; const aCustomHeaders: TStrings = nil);
    procedure DoUnSubscribe(const aId: string;
      const aCustomHeaders: TStrings = nil);
    procedure DoACK(const aId, aTransaction: string);
    procedure DoNACK(const aId, aTransaction: string);
    procedure DoBeginTransaction(const aTransaction: String);
    procedure DoCommitTransaction(const aTransaction: String);
    procedure DoAbortTransaction(const aTransaction: String);
    procedure DoDisconnect(const aGraceful: Boolean);
  public
    function Connect(const aHost: String; const aCustomHeaders: TStrings = nil):
        string;
    function Send(const aDestination, aText: string;
      const aContentType: String = ''; const aTransaction: String = '';
      const aCustomHeaders: TStrings = nil): string;
    function Subscribe(const aId, aDestination: string;
      const aACK: TsgcSTOMPACK = ackAuto;
      const aCustomHeaders: TStrings = nil): String;
    function UnSubscribe(const aId: string;
      const aCustomHeaders: TStrings = nil): string;
    function ACK(const aId: string; const aTransaction: String = ''): String;
    function NACK(const aId: string; const aTransaction: String = ''): String;
    function BeginTransaction(const aTransaction: String): String;
    function CommitTransaction(const aTransaction: String): String;
    function AbortTransaction(const aTransaction: String): String;
    function Disconnect(const aGraceful: Boolean = True): String;
    function Ping: String;
    function Pong: String;
    { client frames }

    { server frames }
  private
    function GetHeaderByName(const aName: String): String;
  private
    function GetContentType: String;
    function GetContentLength: Integer;
    function GetReceiptId: String;
    function GetVersion: String;
    function GetServer: String;
    function GetSession: String;
    function GetHeartBeat: String;
    function GetDestination: String;
    function GetMessageId: String;
    function GetSubscription: String;
    function GetACK: String;
  protected
    function DoReadConnected: Boolean;
    function DoReadError: Boolean;
    function DoReadMessage: Boolean;
    function DoReadReceipt: Boolean;
  protected
    function DoRead(aText: String): Boolean;
  public
    function Read(const aText: String): Boolean;
    { server frames }

    { events }
  private
    FOnConnected: TsgcSTOMPConnectedEvent;
    FOnError: TsgcSTOMPErrorEvent;
    FOnMessage: TsgcSTOMPMessageEvent;
    FOnReceipt: TsgcSTOMPReceiptEvent;
  protected
    procedure DoEventConnected(aVersion, aServer, aSession, aHeartBeat: String);
    procedure DoEventError(aMessageText, aContentType: String;
      aContentLength: Integer; aReceiptId: String);
    procedure DoEventMessage(aMessageText, aDestination, aMessageId,
      aSubscription, aACK, aContentType: String);
    procedure DoEventReceipt(aReceiptId: String);
  public
    property OnConnected
      : TsgcSTOMPConnectedEvent read FOnConnected write FOnConnected;
    property OnError: TsgcSTOMPErrorEvent read FOnError write FOnError;
    property OnMessage: TsgcSTOMPMessageEvent read FOnMessage write FOnMessage;
    property OnReceipt: TsgcSTOMPReceiptEvent read FOnReceipt write FOnReceipt;
    { events }
  end;

implementation

uses
  sgcWebSocket_Helpers;

Const
  CS_STOMP_COMMAND_CONNECT = 'CONNECT';
  CS_STOMP_HEADER_ACCEPT_VERSION = 'accept-version';
  CS_STOMP_HEADER_HOST = 'host';
  CS_STOMP_HEADER_LOGIN = 'login';
  CS_STOMP_HEADER_PASSCODE = 'passcode';
  CS_STOMP_HEADER_HEARTBEAT = 'heart-beat';

Const
  CS_STOMP_COMMAND_SEND = 'SEND';
  CS_STOMP_HEADER_DESTINATION = 'destination';
  CS_STOMP_HEADER_CONTENT_TYPE = 'content-type';
  CS_STOMP_HEADER_CONTENT_LENGTH = 'content-length';
  CS_STOMP_HEADER_TRANSACTION = 'transaction';

Const
  CS_STOMP_COMMAND_SUBSCRIBE = 'SUBSCRIBE';
  CS_STOMP_HEADER_ID = 'id';
  CS_STOMP_HEADER_ACK = 'ack';
  CS_STOMP_HEADER_MESSAGE_ID = 'message-id';
  CS_STOMP_HEADER_SUBSCRIPTION = 'subscription';

Const
  CS_STOMP_COMMAND_UNSUBSCRIBE = 'UNSUBSCRIBE';

Const
  CS_STOMP_COMMAND_ACK = 'ACK';

Const
  CS_STOMP_COMMAND_NACK = 'NACK';

Const
  CS_STOMP_COMMAND_BEGIN = 'BEGIN';

Const
  CS_STOMP_COMMAND_COMMIT = 'COMMIT';

Const
  CS_STOMP_COMMAND_ABORT = 'ABORT';

Const
  CS_STOMP_COMMAND_DISCONNECT = 'DISCONNECT';
  CS_STOMP_HEADER_RECEIPT = 'receipt';

Const
  CS_STOMP_COMMAND_CONNECTED = 'CONNECTED';
  CS_STOMP_HEADER_SERVER = 'server';
  CS_STOMP_HEADER_SESSION = 'session';
  CS_STOMP_HEADER_VERSION = 'version';

Const
  CS_STOMP_COMMAND_ERROR = 'ERROR';

Const
  CS_STOMP_COMMAND_MESSAGE = 'MESSAGE';

Const
  CS_STOMP_COMMAND_RECEIPT = 'RECEIPT';
  CS_STOMP_HEADER_RECEIPT_ID = 'receipt-id';

Const
  CS_STOMP_LINE_END = #10;
  CS_STOMP_FRAME_END = #0;

procedure TsgcSTOMPAuthentication.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTOMPAuthentication then
  begin
    Enabled := TsgcSTOMPAuthentication(aSource).Enabled;
    Username := TsgcSTOMPAuthentication(aSource).Username;
    Password := TsgcSTOMPAuthentication(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSTOMP.Create(aOwner: TComponent);
begin
  inherited;
  FHeaders := TStringList.Create;
  {$IFNDEF D7}
  FHeaders.LineBreak := CS_STOMP_LINE_END;
  {$ENDIF}
  FAuthentication := TsgcSTOMPAuthentication.Create;
  FHeartBeat := TsgcSTOMPHeartBeat.Create;
  FVersion := TsgcSTOMPVersion.Create;
end;

destructor TsgcSTOMP.Destroy;
begin
  sgcFree(FVersion);
  sgcFree(FHeartBeat);
  sgcFree(FAuthentication);
  sgcFree(FHeaders);
  inherited;
end;

function TsgcSTOMP.AbortTransaction(const aTransaction: String): String;
begin
  DoClear;
  DoAbortTransaction(aTransaction);
  result := Write;
end;

function TsgcSTOMP.ACK(const aId: string; const aTransaction: String = ''):
    String;
begin
  DoClear;
  DoACK(aId, aTransaction);
  result := Write;
end;

procedure TsgcSTOMP.AddCommand(const aCommand: String);
begin
  FHeaders.Add(aCommand);
end;

procedure TsgcSTOMP.AddHeader(const aName, aValue: String);
begin
  FHeaders.Add(Format('%s:%s', [aName, aValue]));
end;

function TsgcSTOMP.BeginTransaction(const aTransaction: String): String;
begin
  DoClear;
  DoBeginTransaction(aTransaction);
  result := Write;
end;

procedure TsgcSTOMP.Clear;
begin
  DoClear;
end;

function TsgcSTOMP.CommitTransaction(const aTransaction: String): String;
begin
  DoClear;
  DoCommitTransaction(aTransaction);
  result := Write;
end;

procedure TsgcSTOMP.DoClear;
begin
  FHeaders.Clear;
  FBody := '';
end;

function TsgcSTOMP.Connect(const aHost: String; const aCustomHeaders: TStrings
    = nil): string;
begin
  DoClear;
  DoConnect(aHost, aCustomHeaders);
  result := Write;
end;

function TsgcSTOMP.Disconnect(const aGraceful: Boolean = True): String;
begin
  DoClear;
  DoDisconnect(aGraceful);
  result := Write;
end;

procedure TsgcSTOMP.DoAbortTransaction(const aTransaction: String);
begin
  AddCommand(CS_STOMP_COMMAND_ABORT);
  AddHeader(CS_STOMP_HEADER_TRANSACTION, aTransaction);
end;

procedure TsgcSTOMP.DoACK(const aId, aTransaction: string);
begin
  AddCommand(CS_STOMP_COMMAND_ACK);
  AddHeader(CS_STOMP_HEADER_ID, aId);
  if aTransaction <> '' then
    AddHeader(CS_STOMP_HEADER_TRANSACTION, aTransaction);
end;

procedure TsgcSTOMP.DoBeginTransaction(const aTransaction: String);
begin
  AddCommand(CS_STOMP_COMMAND_BEGIN);
  AddHeader(CS_STOMP_HEADER_TRANSACTION, aTransaction);
end;

procedure TsgcSTOMP.DoCommitTransaction(const aTransaction: String);
begin
  AddCommand(CS_STOMP_COMMAND_COMMIT);
  AddHeader(CS_STOMP_HEADER_TRANSACTION, aTransaction);
end;

procedure TsgcSTOMP.DoConnect(const aHost: String; const aCustomHeaders:
    TStrings = nil);
var
  i: Integer;
begin
  AddCommand(CS_STOMP_COMMAND_CONNECT);
  if Authentication.Enabled then
  begin
    AddHeader(CS_STOMP_HEADER_LOGIN, Authentication.Username);
    AddHeader(CS_STOMP_HEADER_PASSCODE, Authentication.Password);
  end;
  if Version.V1_1 or Version.V1_2 then
  begin
    AddHeader(CS_STOMP_HEADER_ACCEPT_VERSION, GetAcceptVersion);
    AddHeader(CS_STOMP_HEADER_HOST, aHost);
    if HeartBeat.Enabled then
      AddHeader(CS_STOMP_HEADER_HEARTBEAT, Format('%d,%d', [HeartBeat.Outgoing,
          HeartBeat.Incoming]));
  end;
  if Assigned(aCustomHeaders) then
  begin
    for i := 0 to aCustomHeaders.Count - 1 do
      FHeaders.Add(aCustomHeaders[i]);
  end;
end;

procedure TsgcSTOMP.DoDisconnect(const aGraceful: Boolean);
begin
  AddCommand(CS_STOMP_COMMAND_DISCONNECT);
  if aGraceful then
  begin
    FReceipt := RandomRange(1, 9999);
    AddHeader(CS_STOMP_HEADER_RECEIPT, IntToStr(FReceipt));
  end;
end;

procedure TsgcSTOMP.DoEventConnected(aVersion, aServer, aSession,
  aHeartBeat: String);
begin
  if Assigned(FOnConnected) then
    FOnConnected(self, aVersion, aServer, aSession, aHeartBeat, FHeaders.Text);
end;

procedure TsgcSTOMP.DoEventError(aMessageText, aContentType: String;
  aContentLength: Integer; aReceiptId: String);
begin
  if Assigned(FOnError) then
    FOnError(self, aMessageText, aContentType, aContentLength, aReceiptId,
      FHeaders.Text);
end;

procedure TsgcSTOMP.DoEventMessage(aMessageText, aDestination, aMessageId,
  aSubscription, aACK, aContentType: String);
begin
  if Assigned(FOnMessage) then
    FOnMessage(self, aMessageText, aDestination, aMessageId, aSubscription,
      aACK, aContentType, FHeaders.Text);
end;

procedure TsgcSTOMP.DoEventReceipt(aReceiptId: String);
begin
  if Assigned(FOnReceipt) then
    FOnReceipt(self, aReceiptId, FHeaders.Text);
end;

procedure TsgcSTOMP.DoNACK(const aId, aTransaction: string);
begin
  AddCommand(CS_STOMP_COMMAND_NACK);
  AddHeader(CS_STOMP_HEADER_ID, aId);
  if aTransaction <> '' then
    AddHeader(CS_STOMP_HEADER_TRANSACTION, aTransaction);
end;

function TsgcSTOMP.DoRead(aText: String): Boolean;
var
  i, j: Integer;
  vCommand: String;
  vBody: Boolean;
begin
  result := False;

  vBody := False;
  j := 0;

  FHeaders.Text := aText;

  // ... read body
  for i := 0 to FHeaders.Count - 1 do
  begin
    case i of
      0:
        vCommand := FHeaders[i];
    else
      begin
        if FHeaders[i] = '' then
        begin
          if vBody = False then
            j := i;
          vBody := True;
        end;
        if vBody then
          FBody := FBody + FHeaders[i];
      end;
    end;
  end;

  // ... delete body
  for i := FHeaders.Count - 1 Downto j do
    FHeaders.Delete(i);

  // ... read method
  if vCommand = CS_STOMP_COMMAND_MESSAGE then
    result := DoReadMessage
  else if vCommand = CS_STOMP_COMMAND_ERROR then
    result := DoReadError
  else if vCommand = CS_STOMP_COMMAND_RECEIPT then
    result := DoReadReceipt
  else if vCommand = CS_STOMP_COMMAND_CONNECTED then
    result := DoReadConnected;
end;

function TsgcSTOMP.DoReadConnected: Boolean;
begin
  result := True;

  DoEventConnected(GetVersion, GetServer, GetSession, GetHeartBeat);
end;

function TsgcSTOMP.DoReadError: Boolean;
begin
  result := True;

  DoEventError(FBody, GetContentType, GetContentLength, GetReceiptId);
end;

function TsgcSTOMP.DoReadMessage: Boolean;
begin
  result := True;

  DoEventMessage(FBody, GetDestination, GetMessageId, GetSubscription, GetACK,
    GetContentType);
end;

function TsgcSTOMP.DoReadReceipt: Boolean;
begin
  result := True;

  DoEventReceipt(GetReceiptId);
end;

procedure TsgcSTOMP.DoSend(const aDestination, aMessage, aContentType,
  aTransaction: string; const aCustomHeaders: TStrings);
var
  i: Integer;
begin
  AddCommand(CS_STOMP_COMMAND_SEND);
  AddHeader(CS_STOMP_HEADER_DESTINATION, aDestination);
  if aContentType <> '' then
    AddHeader(CS_STOMP_HEADER_CONTENT_TYPE, aContentType);
  if aMessage <> '' then
    AddHeader(CS_STOMP_HEADER_CONTENT_LENGTH, IntToStr(ReadContentLength(aMessage)));
  if aTransaction <> '' then
    AddHeader(CS_STOMP_HEADER_TRANSACTION, aTransaction);
  if Assigned(aCustomHeaders) then
  begin
    for i := 0 to aCustomHeaders.Count - 1 do
      FHeaders.Add(aCustomHeaders[i]);
  end;

  FBody := aMessage;
end;

procedure TsgcSTOMP.DoSubscribe(const aId, aDestination: string;
  const aACK: TsgcSTOMPACK; const aCustomHeaders: TStrings = nil);
var
  i: Integer;
begin
  AddCommand(CS_STOMP_COMMAND_SUBSCRIBE);
  AddHeader(CS_STOMP_HEADER_ID, aId);
  AddHeader(CS_STOMP_HEADER_DESTINATION, aDestination);
  case aACK of
    ackAuto:
      AddHeader(CS_STOMP_HEADER_ACK, 'auto');
    ackMultiple:
      AddHeader(CS_STOMP_HEADER_ACK, 'client');
    ackIndividual:
      AddHeader(CS_STOMP_HEADER_ACK, 'client-individual');
  end;
  if Assigned(aCustomHeaders) then
  begin
    for i := 0 to aCustomHeaders.Count - 1 do
      FHeaders.Add(aCustomHeaders[i]);
  end;
end;

procedure TsgcSTOMP.DoUnSubscribe(const aId: string;
  const aCustomHeaders: TStrings = nil);
var
  i: Integer;
begin
  AddCommand(CS_STOMP_COMMAND_UNSUBSCRIBE);
  AddHeader(CS_STOMP_HEADER_ID, aId);
  if Assigned(aCustomHeaders) then
  begin
    for i := 0 to aCustomHeaders.Count - 1 do
      FHeaders.Add(aCustomHeaders[i]);
  end;
end;

function TsgcSTOMP.GetAcceptVersion: string;
var
  oList: TsgcDelimitedStringList;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    if Version.V1_0 then
      oList.Add('1.0');
    if Version.V1_1 then
      oList.Add('1.1');
    if Version.V1_2 then
      oList.Add('1.2');
    result := oList.CommaText;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcSTOMP.GetACK: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_ACK);
end;

function TsgcSTOMP.GetContentLength: Integer;
begin
  TryStrToInt(GetHeaderByName(CS_STOMP_HEADER_CONTENT_LENGTH), result);
end;

function TsgcSTOMP.ReadContentLength(const aMessage: String): Integer;
begin
  {$IFDEF NEXTGEN}
     {$IFDEF D10_1}
      Result := Length(UTF8String(aMessage));
     {$ELSE}
      Result := Length(String(UTF8Encode(aMessage)));
     {$ENDIF}
  {$ELSE}
    Result := Length(UTF8String(aMessage));
  {$ENDIF}
end;

function TsgcSTOMP.GetContentType: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_CONTENT_TYPE);
end;

function TsgcSTOMP.GetDestination: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_DESTINATION);
end;

function TsgcSTOMP.GetHeaderByName(const aName: String): String;
var
  i: Integer;
begin
  result := '';

  for i := 0 to FHeaders.Count - 1 do
  begin
    if LeftStr(FHeaders[i], Length(aName) + 1) = aName + ':' then
    begin
      result := MidStr(FHeaders[i], Length(aName) + 2, Length(FHeaders[i]));
      break;
    end;
  end;
end;

function TsgcSTOMP.GetHeartBeat: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_HEARTBEAT);
end;

function TsgcSTOMP.GetMessageId: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_MESSAGE_ID);
end;

function TsgcSTOMP.GetReceiptId: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_RECEIPT_ID);
end;

function TsgcSTOMP.GetServer: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_SERVER);
end;

function TsgcSTOMP.GetSession: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_SESSION);
end;

function TsgcSTOMP.GetSubscription: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_SUBSCRIPTION);
end;

function TsgcSTOMP.GetVersion: String;
begin
  result := GetHeaderByName(CS_STOMP_HEADER_VERSION);
end;

function TsgcSTOMP.NACK(const aId: string; const aTransaction: String = ''):
    String;
begin
  result := '';
  if Version.V1_1 or Version.V1_2 then
  begin
    DoClear;
    DoNACK(aId, aTransaction);
    result := Write;
  end;
end;

function TsgcSTOMP.Ping: String;
begin
  result := CS_STOMP_LINE_END;
end;

function TsgcSTOMP.Pong: String;
begin
  result := CS_STOMP_LINE_END;
end;

function TsgcSTOMP.Read(const aText: String): Boolean;
begin
  DoClear;
  result := DoRead(aText);
end;

function TsgcSTOMP.Send(const aDestination, aText: string;
  const aContentType: String = ''; const aTransaction: String = '';
  const aCustomHeaders: TStrings = nil): string;
begin
  DoClear;
  DoSend(aDestination, aText, aContentType, aTransaction, aCustomHeaders);
  result := Write;
end;

function TsgcSTOMP.Write: String;
begin
  result := FHeaders.Text + CS_STOMP_LINE_END + FBody + CS_STOMP_FRAME_END +
    CS_STOMP_LINE_END;
end;

procedure TsgcSTOMP.SetAuthentication(const Value: TsgcSTOMPAuthentication);
begin
  FAuthentication.Assign(Value);
end;

procedure TsgcSTOMP.SetHeartBeat(const Value: TsgcSTOMPHeartBeat);
begin
  FHeartBeat.Assign(Value);
end;

procedure TsgcSTOMP.SetVersion(const Value: TsgcSTOMPVersion);
begin
  FVersion.Assign(Value);
end;

function TsgcSTOMP.Subscribe(const aId, aDestination: string;
  const aACK: TsgcSTOMPACK = ackAuto; const aCustomHeaders: TStrings = nil)
  : String;
begin
  DoClear;
  DoSubscribe(aId, aDestination, aACK, aCustomHeaders);
  result := Write;
end;

function TsgcSTOMP.UnSubscribe(const aId: string;
  const aCustomHeaders: TStrings = nil): string;
begin
  DoClear;
  DoUnSubscribe(aId, aCustomHeaders);
  result := Write;
end;

procedure TsgcSTOMPHeartBeat.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTOMPHeartBeat then
  begin
    Enabled := TsgcSTOMPHeartBeat(aSource).Enabled;
    Incoming := TsgcSTOMPHeartBeat(aSource).Incoming;
    Outgoing := TsgcSTOMPHeartBeat(aSource).Outgoing;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSTOMPVersion.Create;
begin
  inherited;
  V1_2 := True;
  V1_1 := True;
  V1_0 := True;
end;

procedure TsgcSTOMPVersion.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTOMPVersion then
  begin
    V1_0 := TsgcSTOMPVersion(aSource).V1_0;
    V1_1 := TsgcSTOMPVersion(aSource).V1_1;
    V1_2 := TsgcSTOMPVersion(aSource).V1_2;
  end
  else
    inherited Assign(aSource);
end;

end.
