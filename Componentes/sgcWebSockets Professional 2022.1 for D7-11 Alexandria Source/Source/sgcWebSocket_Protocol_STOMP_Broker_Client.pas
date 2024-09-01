{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_STOMP_Broker_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Protocol_STOMP_Client, sgcWebSocket_Const,
  sgcSTOMP;

type
  TsgcWSBrokerACK = (ackAutomatic, ackAutoIndividual, ackManual);

  TsgcWSBrokerSTOMPHeadersBase = class(TComponent)
  private
    function GetText: String;
  protected
    FHeaders: TStringList;
    function GetHeaderByName(const aName: String): String;
  protected
    procedure DoRead; virtual; abstract;
  protected
    procedure Read(const aText: String); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Header(const aName: String): string;
    function HeaderExists(const aName: String): Boolean;
  public
    property Text: String read GetText;
  end;

  TsgcWSBrokerSTOMPHeadersConnected = class(TsgcWSBrokerSTOMPHeadersBase)
  private
    FHeartBeat: String;
    FServer: string;
    FSession: String;
    FVersion: String;
  protected
    procedure DoRead; override;
  public
    property HeartBeat: String read FHeartBeat write FHeartBeat;
    property Server: string read FServer write FServer;
    property Session: String read FSession write FSession;
    property Version: String read FVersion write FVersion;
  end;

  TsgcWSBrokerSTOMPHeadersMessage = class(TsgcWSBrokerSTOMPHeadersBase)
  private
    FACK: string;
    FContentLength: Integer;
    FContentType: String;
    FDestination: string;
    FMessageId: String;
    FRedelivered: Boolean;
    FReplyTo: String;
    FSubscription: string;
  protected
    procedure DoRead; override;
  public
    property ACK: string read FACK write FACK;
    property ContentLength: Integer read FContentLength write FContentLength;
    property ContentType: String read FContentType write FContentType;
    property Destination: string read FDestination write FDestination;
    property MessageId: String read FMessageId write FMessageId;
    property Redelivered: Boolean read FRedelivered write FRedelivered;
    property ReplyTo: String read FReplyTo write FReplyTo;
    property Subscription: string read FSubscription write FSubscription;
  end;

  TsgcWSBrokerSTOMPHeadersError = class(TsgcWSBrokerSTOMPHeadersBase)
  private
    FACK: string;
    FContentLength: Integer;
    FContentType: string;
    FDestination: string;
    FMessageId: string;
    FMessageText: string;
    FSubscription: string;
    FVersion: string;
  protected
    procedure DoRead; override;
  public
    property ACK: string read FACK write FACK;
    property ContentLength: Integer read FContentLength write FContentLength;
    property ContentType: string read FContentType write FContentType;
    property Destination: string read FDestination write FDestination;
    property MessageId: string read FMessageId write FMessageId;
    property MessageText: string read FMessageText write FMessageText;
    property Subscription: string read FSubscription write FSubscription;
    property Version: string read FVersion write FVersion;
  end;

  TsgcWSBrokerSTOMPHeadersReceipt = class(TsgcWSBrokerSTOMPHeadersBase)
  private
    FReceiptId: string;
  protected
    procedure DoRead; override;
  public
    property ReceiptId: string read FReceiptId write FReceiptId;
  end;

  TsgcWSBrokerSTOMPSubscriptionItem = class
  private
    FACK: TsgcSTOMPACK;
    FDate: TDateTime;
    FHeaders: TStringList;
    FId: string;
    FName: String;
    FReplyTo: String;
    function GetHeaders: TStringList;
  public
    destructor Destroy; override;
  public
    property ACK: TsgcSTOMPACK read FACK write FACK;
    property Date: TDateTime read FDate write FDate;
    property Headers: TStringList read GetHeaders write FHeaders;
    property Id: string read FId write FId;
    property Name: String read FName write FName;
    property ReplyTo: String read FReplyTo write FReplyTo;
  end;

  TsgcWSBrokerSTOMPSubscriptionList = class(
{$IFDEF NEXTGEN} TList<TsgcWSBrokerSTOMPSubscriptionItem>{$ELSE} TObjectList
{$ENDIF})

  end;

  TsgcWSBrokerSTOMP_Options = class(TPersistent)
  private
    FAcknowledgments: TsgcWSBrokerACK;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Acknowledgments: TsgcWSBrokerACK read FAcknowledgments
      write FAcknowledgments;
  end;

  TsgcWSProtocol_STOMP_Broker_Client = class(TsgcWSProtocol_STOMP_Client_Base)
    { stomp events }
  protected
    procedure OnBrokerConnectedEvent(Connection: TsgcWSConnection;
      Version, Server, Session, HeartBeat, RawHeaders: String);
    procedure OnBrokerMessageEvent(aConnection: TsgcWSConnection;
      aMessageText, aDestination, aMessageId, aSubscription, aACK, aContentType,
      aRawHeaders: String);
    procedure OnBrokerErrorEvent(Connection: TsgcWSConnection;
      MessageText, ContentType: String; ContentLength: Integer;
      ReceiptId, RawHeaders: String);
    procedure OnBrokerReceiptEvent(Connection: TsgcWSConnection;
      ReceiptId, RawHeaders: String);
    procedure OnBrokerDisconnectedEvent(Connection: TsgcWSConnection;
      Code: Integer);
  protected
    procedure DoBrokerConnectEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); virtual; abstract;
    procedure DoBrokerMessageEvent(aConnection: TsgcWSConnection;
      aMessageText, aRawHeaders: String;
      aSubscription: TsgcWSBrokerSTOMPSubscriptionItem); virtual; abstract;
    procedure DoBrokerErrorEvent(aConnection: TsgcWSConnection;
      aMessageText: String; aRawHeaders: String); virtual; abstract;
    procedure DoBrokerReceiptEvent(aConnection: TsgcWSConnection;
      aRawHeaders: String); virtual; abstract;
    procedure DoBrokerDisconnectedEvent(aConnection: TsgcWSConnection;
      aCode: Integer); virtual; abstract;
    { stomp events }

    { Broker_Options }
  private
    FBroker_Options: TsgcWSBrokerSTOMP_Options;
    procedure SetBroker_Options(const Value: TsgcWSBrokerSTOMP_Options);
  public
    property Broker_Options: TsgcWSBrokerSTOMP_Options read FBroker_Options
      write SetBroker_Options;
    { Broker_Options }

    { automatic ack }
  protected
    procedure DoAutomaticACK(oSubscription: TsgcWSBrokerSTOMPSubscriptionItem;
      aACK, aMessageId: String);
    { automatic ack }

    { helpers }
  protected
    function GetNewId: String; virtual;
    { helpers }

    { subscriptions }
  private
    FSubscriptionList: TsgcWSBrokerSTOMPSubscriptionList;
    function GetSubscriptionList: TsgcWSBrokerSTOMPSubscriptionList;
  protected
    procedure DoQueueSubscription(const aId, aName: string;
      const aACK: TsgcSTOMPACK; const aHeaders: TStrings); virtual;
    procedure DoDeleteSubscription(const aId: String); virtual;
  protected
    property SubscriptionList: TsgcWSBrokerSTOMPSubscriptionList
      read GetSubscriptionList;
  public
    function GetSubscriptionByName(const aName: String)
      : TsgcWSBrokerSTOMPSubscriptionItem;
    function GetSubscriptionById(const aId: String)
      : TsgcWSBrokerSTOMPSubscriptionItem;
    { subscriptions }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  public
    procedure BeginTransaction(const aTransaction: String); override;
    procedure CommitTransaction(const aTransaction: String); override;
    procedure AbortTransaction(const aTransaction: String); override;
  public
    procedure ACK(const aId: string; const aTransaction: String = ''); override;
    procedure NACK(const aId: string; const aTransaction: String = '');
      override;
  public
    procedure Disconnect(const aGraceful: Boolean = True); override;
    { methods }

    { properties }
  public
    property ConnectHeaders: TStringList read GetConnectHeaders write FConnectHeaders;
    { properties }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcBase_Helpers;

Const
  CS_SUBSCRIPTION_LIST = 0;

Const
  CS_STOMP_BROKER_HEADER_CONTENT_LENGTH = 'content-length';
  CS_STOMP_BROKER_HEADER_CONTENT_TYPE = 'content-type';
  CS_STOMP_BROKER_HEADER_DESTINATION = 'destination';
  CS_STOMP_BROKER_HEADER_MESSAGE_ID = 'message-id';
  CS_STOMP_BROKER_HEADER_REDELIVERED = 'redelivered';
  CS_STOMP_BROKER_HEADER_SUBSCRIPTION = 'subscription';
  CS_STOMP_BROKER_HEADER_REPLY_TO = 'reply-to';
  CS_STOMP_BROKER_HEADER_MESSAGE = 'message';

Const
  CS_STOMP_BROKER_HEADER_SERVER = 'server';
  CS_STOMP_BROKER_HEADER_VERSION = 'version';
  CS_STOMP_BROKER_HEADER_SESSION = 'session';
  CS_STOMP_BROKER_HEADER_HEARTBEAT = 'heart-beat';

Const
  CS_STOMP_BROKER_HEADER_ACK = 'ack';

Const
  CS_STOMP_BROKER_HEADER_RECEIPT_ID = 'receipt-id';

constructor TsgcWSBrokerSTOMP_Options.Create;
begin
  inherited;
  Acknowledgments := ackAutomatic;
end;

procedure TsgcWSBrokerSTOMP_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBrokerSTOMP_Options then
    Acknowledgments := TsgcWSBrokerSTOMP_Options(aSource).Acknowledgments
  else
    inherited Assign(aSource);
end;

constructor TsgcWSProtocol_STOMP_Broker_Client.Create(aOwner: TComponent);
begin
  inherited;
  FBroker_Options := TsgcWSBrokerSTOMP_Options.Create;
  OnSTOMPConnected := OnBrokerConnectedEvent;
  OnSTOMPMessage := OnBrokerMessageEvent;
  OnSTOMPError := OnBrokerErrorEvent;
  OnSTOMPReceipt := OnBrokerReceiptEvent;
  OnSTOMPDisconnected := OnBrokerDisconnectedEvent;
end;

destructor TsgcWSProtocol_STOMP_Broker_Client.Destroy;
begin
  sgcFree(FBroker_Options);
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.AbortTransaction
  (const aTransaction: String);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.ACK(const aId: string;
  const aTransaction: String = '');
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.BeginTransaction
  (const aTransaction: String);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.CommitTransaction
  (const aTransaction: String);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.Disconnect(const aGraceful
  : Boolean = True);
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.DoAutomaticACK
  (oSubscription: TsgcWSBrokerSTOMPSubscriptionItem; aACK, aMessageId: String);
var
  vSendACK: Boolean;
begin
  vSendACK := False;

  if Assigned(oSubscription) then
  begin
    // ... validate
    case oSubscription.ACK of
      ackMultiple:
        begin
          if Broker_Options.Acknowledgments = ackAutomatic then
            vSendACK := True;
        end;
      ackIndividual:
        begin
          if Broker_Options.Acknowledgments in [ackAutomatic, ackAutoIndividual]
          then
            vSendACK := True;
        end;
    end;

    // ... send ACK
    if vSendACK then
    begin
      if aACK <> '' then
        ACK(aACK)
      else if aMessageId <> '' then
        ACK(aMessageId);
    end;
  end;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.DoDeleteSubscription
  (const aId: String);
var
  i: Integer;
begin
  DoEnterCS(CS_SUBSCRIPTION_LIST);
  Try
    for i := SubscriptionList.Count - 1 Downto 0 do
    begin
      if TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i]).Id = aId
      then
        SubscriptionList.Delete(i);
    end;
  Finally
    DoLeaveCS(CS_SUBSCRIPTION_LIST);
  End;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.DoQueueSubscription(const aId,
  aName: string; const aACK: TsgcSTOMPACK; const aHeaders: TStrings);
var
  oItem: TsgcWSBrokerSTOMPSubscriptionItem;
begin
  if aId = '' then
    exit;

  DoEnterCS(CS_SUBSCRIPTION_LIST);
  Try
    oItem := TsgcWSBrokerSTOMPSubscriptionItem.Create;
    oItem.Id := aId;
    oItem.Name := aName;
    oItem.ACK := aACK;
    if Assigned(aHeaders) then
      oItem.Headers.Text := aHeaders.Text;
    oItem.Date := Now;
    SubscriptionList.Add(oItem)
  Finally
    DoLeaveCS(CS_SUBSCRIPTION_LIST);
  End;
end;

function TsgcWSProtocol_STOMP_Broker_Client.GetNewId: String;
begin
  Result := FormatDateTime('yyyymmddhhnnsszzz', Now);
end;

function TsgcWSProtocol_STOMP_Broker_Client.GetSubscriptionById
  (const aId: String): TsgcWSBrokerSTOMPSubscriptionItem;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to SubscriptionList.Count - 1 do
  begin
    if TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i]).Id = aId
    then
    begin
      Result := TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i]);
      break;
    end
    else if TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i])
      .ReplyTo = aId then
    begin
      Result := TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i]);
      break;
    end;
  end;
end;

function TsgcWSProtocol_STOMP_Broker_Client.GetSubscriptionByName
  (const aName: String): TsgcWSBrokerSTOMPSubscriptionItem;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to SubscriptionList.Count - 1 do
  begin
    if TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i]).Name = aName
    then
    begin
      Result := TsgcWSBrokerSTOMPSubscriptionItem(SubscriptionList.Items[i]);
      break;
    end;
  end;
end;

function TsgcWSProtocol_STOMP_Broker_Client.GetSubscriptionList
  : TsgcWSBrokerSTOMPSubscriptionList;
begin
  if not Assigned(FSubscriptionList) then
    FSubscriptionList := TsgcWSBrokerSTOMPSubscriptionList.Create;
  Result := FSubscriptionList;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.NACK(const aId: string;
  const aTransaction: String = '');
begin
  inherited;
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.OnBrokerConnectedEvent
  (Connection: TsgcWSConnection; Version, Server, Session, HeartBeat,
  RawHeaders: String);
begin
  DoBrokerConnectEvent(Connection, RawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.OnBrokerDisconnectedEvent
  (Connection: TsgcWSConnection; Code: Integer);
begin
  DoBrokerDisconnectedEvent(Connection, Code);
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.OnBrokerErrorEvent
  (Connection: TsgcWSConnection; MessageText, ContentType: String;
  ContentLength: Integer; ReceiptId, RawHeaders: String);
begin
  DoBrokerErrorEvent(Connection, MessageText, RawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.OnBrokerMessageEvent
  (aConnection: TsgcWSConnection; aMessageText, aDestination, aMessageId,
  aSubscription, aACK, aContentType, aRawHeaders: String);
var
  oSubscription: TsgcWSBrokerSTOMPSubscriptionItem;
begin
  oSubscription := GetSubscriptionById(aSubscription);

  DoBrokerMessageEvent(aConnection, aMessageText, aRawHeaders, oSubscription);

  DoAutomaticACK(oSubscription, aACK, aMessageId);
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.OnBrokerReceiptEvent
  (Connection: TsgcWSConnection; ReceiptId, RawHeaders: String);
begin
  DoBrokerReceiptEvent(Connection, RawHeaders);
end;

procedure TsgcWSProtocol_STOMP_Broker_Client.SetBroker_Options
  (const Value: TsgcWSBrokerSTOMP_Options);
begin
  FBroker_Options.Assign(Value);
end;

destructor TsgcWSBrokerSTOMPSubscriptionItem.Destroy;
begin
  sgcFree(FHeaders);
  inherited;
end;

function TsgcWSBrokerSTOMPSubscriptionItem.GetHeaders: TStringList;
begin
  if not Assigned(FHeaders) then
    FHeaders := TStringList.Create;
  Result := FHeaders;
end;

constructor TsgcWSBrokerSTOMPHeadersBase.Create(aOwner: TComponent);
begin
  inherited;
  FHeaders := TStringList.Create;
end;

destructor TsgcWSBrokerSTOMPHeadersBase.Destroy;
begin
  sgcFree(FHeaders);
  inherited;
end;

function TsgcWSBrokerSTOMPHeadersBase.GetHeaderByName
  (const aName: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to FHeaders.Count - 1 do
  begin
    if LeftStr(FHeaders[i], Length(aName) + 1) = aName + ':' then
    begin
      Result := MidStr(FHeaders[i], Length(aName) + 2, Length(FHeaders[i]));
      break;
    end;
  end;
end;

function TsgcWSBrokerSTOMPHeadersBase.GetText: String;
begin
  Result := FHeaders.Text;
end;

function TsgcWSBrokerSTOMPHeadersBase.Header(const aName: String): string;
begin
  Result := GetHeaderByName(aName);
end;

function TsgcWSBrokerSTOMPHeadersBase.HeaderExists(const aName: String)
  : Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FHeaders.Count - 1 do
  begin
    if LeftStr(FHeaders[i], Length(aName) + 1) = aName + ':' then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure TsgcWSBrokerSTOMPHeadersBase.Read(const aText: String);
begin
  FHeaders.Text := aText;
  DoRead;
end;

procedure TsgcWSBrokerSTOMPHeadersConnected.DoRead;
begin
  Server := GetHeaderByName(CS_STOMP_BROKER_HEADER_SERVER);
  Version := GetHeaderByName(CS_STOMP_BROKER_HEADER_VERSION);
  Session := GetHeaderByName(CS_STOMP_BROKER_HEADER_SESSION);
  HeartBeat := GetHeaderByName(CS_STOMP_BROKER_HEADER_HEARTBEAT);
end;

procedure TsgcWSBrokerSTOMPHeadersMessage.DoRead;
begin
  TryStrToInt(GetHeaderByName(CS_STOMP_BROKER_HEADER_CONTENT_LENGTH),
    FContentLength);
  ContentType := GetHeaderByName(CS_STOMP_BROKER_HEADER_CONTENT_TYPE);
  Destination := GetHeaderByName(CS_STOMP_BROKER_HEADER_DESTINATION);
  MessageId := GetHeaderByName(CS_STOMP_BROKER_HEADER_MESSAGE_ID);
  if sgcContainsText(GetHeaderByName(CS_STOMP_BROKER_HEADER_REDELIVERED), 'True')
  then
    Redelivered := True
  else
    Redelivered := False;
  Subscription := GetHeaderByName(CS_STOMP_BROKER_HEADER_SUBSCRIPTION);
  ReplyTo := GetHeaderByName(CS_STOMP_BROKER_HEADER_REPLY_TO);
  ACK := GetHeaderByName(CS_STOMP_BROKER_HEADER_ACK);
end;

procedure TsgcWSBrokerSTOMPHeadersError.DoRead;
begin
  Destination := GetHeaderByName(CS_STOMP_BROKER_HEADER_DESTINATION);
  MessageId := GetHeaderByName(CS_STOMP_BROKER_HEADER_MESSAGE_ID);
  Subscription := GetHeaderByName(CS_STOMP_BROKER_HEADER_SUBSCRIPTION);
  ACK := GetHeaderByName(CS_STOMP_BROKER_HEADER_ACK);
  ContentType := GetHeaderByName(CS_STOMP_BROKER_HEADER_CONTENT_TYPE);
  MessageText := GetHeaderByName(CS_STOMP_BROKER_HEADER_MESSAGE);
  Version := GetHeaderByName(CS_STOMP_BROKER_HEADER_VERSION);
  TryStrToInt(GetHeaderByName(CS_STOMP_BROKER_HEADER_CONTENT_LENGTH),
    FContentLength);
end;

procedure TsgcWSBrokerSTOMPHeadersReceipt.DoRead;
begin
  ReceiptId := GetHeaderByName(CS_STOMP_BROKER_HEADER_RECEIPT_ID);
end;
{$ENDIF}

end.
