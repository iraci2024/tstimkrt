{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Bittrex;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_API_SignalR, sgcWebSocket_Classes;

Type
  TsgcWSBittrexConnectEvent = procedure(Sender: TObject) of object;
  TsgcWSBittrexErrorEvent = procedure(Sender: TObject; Error: String) of object;
  TsgcWSBittrexSubscribedEvent = procedure(Sender: TObject; Channel: String)
    of object;
  TsgcWSBittrexUnSubscribedEvent = procedure(Sender: TObject; Channel: String)
    of object;
  TsgcWSBittrexMessageEvent = procedure(Sender: TObject;
    CallBack, PayLoad: String) of object;
  TsgcWSBittrexDisconnectEvent = procedure(Sender: TObject; aCloseCode: Integer)
    of object;
  TsgcWSBittrexAuthenticatedEvent = procedure(Sender: TObject) of object;
  TsgcWSBittrexHeartBeatEvent = procedure(Sender: TObject) of object;

  TsgcWSBittrexCandleInterval = (btxc1Min, btxc5Min, btxc5Hour, btxc1Day);
  TsgcWSBittrexOrderBookDepth = (btxd1, btxd25, btxd500);

  TsgcWSBittrex_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
  end;

  TsgcWS_API_Bittrex = class(TsgcWS_API_SignalR)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { from TsgcWS_API_SignalR }
  protected
    procedure DoEventSignalRConnect(const aMessageId, aData: string); override;
    procedure DoEventSignalRResult(const aInvocationId, aResult,
      aError: string); override;
    procedure DoEventSignalRMessage(const aMessageId, aData: string); override;
    procedure DoEventSignalRError(const aError: String); override;
    procedure DoEventSignalRDisconnect(const aCloseCode: Integer); override;
    { from TsgcWS_API_SignalR }

    { properties }
  private
    FBittrex: TsgcWSBittrex_Options;
    procedure SetBittrex(const Value: TsgcWSBittrex_Options);
  public
    property Bittrex: TsgcWSBittrex_Options read FBittrex write SetBittrex;
    { properties }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  private
    function DoDecodeGZIP(const aText: String): String;
    function GetHMACSHA512(const aValue, aSecret: String): String;
  private
    procedure DoWriteData(const aAction, aMethod: String);
  private
    procedure DoSubscribe(const aMethod: String);
    procedure DoUnSubscribe(const aMethod: String);
  private
    function GetTimeStamp: Int64;
    function GetAuthentication: string;
  private
    FIsAuthenticated: Boolean;
    FResponseIsAuthenticated: Boolean;
  public
    function IsAuthenticated(aTimeOut: Integer = 10000): Boolean;
    procedure Authenticate;
  public
    procedure SubscribeBalance;
    procedure UnSubscribeBalance;
  public
    procedure SubscribeHeartBeat;
    procedure UnSubscribeHeartBeat;
  private
    function GetCandleString(const aSymbol: String;
      aInterval: TsgcWSBittrexCandleInterval): string;
    function GetCandleIntervalString(aInterval
      : TsgcWSBittrexCandleInterval): string;
  public
    procedure SubscribeCandle(const aSymbol: String;
      aInterval: TsgcWSBittrexCandleInterval);
    procedure UnSubscribeCandle(const aSymbol: String;
      aInterval: TsgcWSBittrexCandleInterval);
  public
    procedure SubscribeDeposit;
    procedure UnSubscribeDeposit;
  public
    procedure SubscribeMarketSummaries;
    procedure UnSubscribeMarketSummaries;
  public
    procedure SubscribeMarketSummary(const aSymbol: String);
    procedure UnSubscribeMarketSummary(const aSymbol: String);
  public
    procedure SubscribeOrder;
    procedure UnSubscribeOrder;
  private
    function GetOrderBookString(const aSymbol: String;
      aDepth: TsgcWSBittrexOrderBookDepth): string;
    function GetOrderBookDepthString
      (aDepth: TsgcWSBittrexOrderBookDepth): string;
  public
    procedure SubscribeOrderBook(const aSymbol: String;
      aDepth: TsgcWSBittrexOrderBookDepth);
    procedure UnSubscribeOrderBook(const aSymbol: String;
      aDepth: TsgcWSBittrexOrderBookDepth);
  public
    procedure SubscribeTickers;
    procedure UnSubscribeTickers;
  public
    procedure SubscribeTicker(const aSymbol: String);
    procedure UnSubscribeTicker(const aSymbol: String);
  public
    procedure SubscribeTrade(const aSymbol: String);
    procedure UnSubscribeTrade(const aSymbol: String);
    { methods }

    { events }
  private
    FOnBittrexAuthenticated: TsgcWSBittrexAuthenticatedEvent;
    FOnBittrexConnect: TsgcWSBittrexConnectEvent;
    FOnBittrexDisconnect: TsgcWSBittrexDisconnectEvent;
    FOnBittrexError: TsgcWSBittrexErrorEvent;
    FOnBittrexHeartBeat: TsgcWSBittrexHeartBeatEvent;
    FOnBittrexMessage: TsgcWSBittrexMessageEvent;
    FOnBittrexSubscribed: TsgcWSBittrexSubscribedEvent;
    FOnBittrexUnSubscribed: TsgcWSBittrexUnSubscribedEvent;
  protected
    procedure DoEventBittrexConnect; virtual;
    procedure DoEventBittrexSubscribed(const aChannel: String); virtual;
    procedure DoEventBittrexUnSubscribed(const aChannel: String); virtual;
    procedure DoEventBittrexMessage(const aCallBack, aPayLoad: String); virtual;
    procedure DoEventBittrexError(const aError: String); virtual;
    procedure DoEventBittrexDisconnect(const aCloseCode: Integer); virtual;
    procedure DoEventBittrexAuthenticated; virtual;
    procedure DoEventBittrexHeartBeat; virtual;
  protected
    property OnBittrexConnect: TsgcWSBittrexConnectEvent read FOnBittrexConnect
      write FOnBittrexConnect;
    property OnBittrexSubscribed: TsgcWSBittrexSubscribedEvent
      read FOnBittrexSubscribed write FOnBittrexSubscribed;
    property OnBittrexUnSubscribed: TsgcWSBittrexUnSubscribedEvent
      read FOnBittrexUnSubscribed write FOnBittrexUnSubscribed;
    property OnBittrexMessage: TsgcWSBittrexMessageEvent read FOnBittrexMessage
      write FOnBittrexMessage;
    property OnBittrexError: TsgcWSBittrexErrorEvent read FOnBittrexError
      write FOnBittrexError;
    property OnBittrexDisconnect: TsgcWSBittrexDisconnectEvent
      read FOnBittrexDisconnect write FOnBittrexDisconnect;
    property OnBittrexAuthenticated: TsgcWSBittrexAuthenticatedEvent read
        FOnBittrexAuthenticated write FOnBittrexAuthenticated;
    property OnBittrexHeartBeat: TsgcWSBittrexHeartBeatEvent read
        FOnBittrexHeartBeat write FOnBittrexHeartBeat;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  StrUtils, DateUtils,
  // Indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdZLib{$ELSE}IdZLib{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
{$IFDEF INDY10_5_7}
{$IFDEF SGC_INDY}sgcIdHashSHA{$ELSE}IdHashSHA{$ENDIF},
{$ENDIF}
{$IFDEF INDY10_2}
{$IFDEF SGC_INDY}sgcIdHMAC{$ELSE}IdHMAC{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHMACSHA1{$ELSE}IdHMACSHA1{$ENDIF},
{$ENDIF}
  // sgc
  sgcBase_Helpers, sgcWebSocket_Types, sgcJSON;

Const
  CS_BITTREX_HUB = 'c3';

Const
  CS_BITTREX_ISAUTHENTICATED = 'isauthenticated';
  CS_BITTREX_AUTHENTICATE = 'authenticate';
  CS_BITTREX_AUTHENTICATION_EXPIRING = 'authenticationExpiring';

Const
  CS_BITTREX_SUBSCRIBE = 'Subscribe';
  CS_BITTREX_UNSUBSCRIBE = 'Unsubscribe';

Const
  CS_BITTREX_BALANCE = 'balance';
  CS_BITTREX_CANDLE = 'candle';
  CS_BITTREX_DEPOSIT = 'deposit';
  CS_BITTREX_HEARTBEAT = 'heartbeat';
  CS_BITTREX_MARKET_SUMMARIES = 'market_summaries';
  CS_BITTREX_MARKET_SUMMARY = 'market_summary';
  CS_BITTREX_ORDER = 'order';
  CS_BITTREX_ORDERBOOK = 'orderbook';
  CS_BITTREX_TICKERS = 'tickers';
  CS_BITTREX_TICKER = 'ticker';
  CS_BITTREX_TRADE = 'trade';

constructor TsgcWS_API_Bittrex.Create(aOwner: TComponent);
begin
  inherited;
  SignalR.UserAgent := 'Mozilla/5.0';
  SignalR.Hubs.Add(CS_BITTREX_HUB);
  FBittrex := TsgcWSBittrex_Options.Create;;
end;

destructor TsgcWS_API_Bittrex.Destroy;
begin
  sgcFree(FBittrex);
  inherited;
end;

procedure TsgcWS_API_Bittrex.Authenticate;
begin
  if Assigned(FClient) then
    FClient.WriteData('{"H":"' + CS_BITTREX_HUB + '","M":"' +
      CS_BITTREX_AUTHENTICATE + '","A":' + GetAuthentication + ',"I":"' +
      CS_BITTREX_AUTHENTICATE + '"}')
end;

function TsgcWS_API_Bittrex.GetHMACSHA512(const aValue,
  aSecret: String): String;
{$IFDEF INDY10_5_7}
var
  oHMAC: TIdHMACSHA512;
  oHash: TIdBytes;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not {$IFDEF SGC_INDY}sgcIdHashSHA{$ELSE}IdHashSHA{$ENDIF}.TIdHashSHA512.IsAvailable
  then
    raise Exception.Create('SHA512 hashing is not available!');
  oHMAC := TIdHMACSHA512.Create;
  try
    oHMAC.Key :=
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.
      GetBytes(aSecret);
    oHash := oHMAC.HashValue({$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.GetBytes(aValue));
    Result := LowerCase(ToHex(oHash));
  finally
    oHMAC.Free;
  end;
{$ELSE}
    raise Exception.Create('SHA512 hashing is not available!');
{$ENDIF}
end;

function TsgcWS_API_Bittrex.DoDecodeGZIP(const aText: String): String;
var
  oMemoryStream: TMemoryStream;
  oStringStream: TsgcStringStream;
begin
  Result := aText;

  oMemoryStream := TMemoryStream.Create;
  Try
    DecodeBase64(Result, oMemoryStream);
    oStringStream := TsgcStringStream.Create('');
    Try
      oMemoryStream.Position := 0;
{$IFDEF SGC_INDY}sgcIdZLib{$ELSE}IdZLib{$ENDIF}.DecompressStream(oMemoryStream, oStringStream);
      Result := oStringStream.DataString;
    Finally
      sgcFree(oStringStream);
    End;
  Finally
    sgcFree(oMemoryStream);
  End;
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexAuthenticated;
begin
  if Assigned(FOnBittrexAuthenticated) then
    FOnBittrexAuthenticated(self);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexConnect;
begin
  if Assigned(FOnBittrexConnect) then
    FOnBittrexConnect(self);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexDisconnect(const aCloseCode
  : Integer);
begin
  if Assigned(FOnBittrexDisconnect) then
    FOnBittrexDisconnect(self, aCloseCode);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexError(const aError: String);
begin
  if Assigned(FOnBittrexError) then
    FOnBittrexError(self, aError);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexHeartBeat;
begin
  if Assigned(FOnBittrexHeartBeat) then
    FOnBittrexHeartBeat(self);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexMessage(const aCallBack,
  aPayLoad: String);
begin
  if Assigned(FOnBittrexMessage) then
    FOnBittrexMessage(self, aCallBack, aPayLoad);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexSubscribed(const aChannel: String);
begin
  if Assigned(FOnBittrexSubscribed) then
    FOnBittrexSubscribed(self, aChannel);
end;

procedure TsgcWS_API_Bittrex.DoEventBittrexUnSubscribed(const aChannel: String);
begin
  if Assigned(FOnBittrexUnSubscribed) then
    FOnBittrexUnSubscribed(self, aChannel);
end;

procedure TsgcWS_API_Bittrex.DoEventSignalRConnect(const aMessageId,
  aData: string);
begin
  DoEventBittrexConnect;
end;

procedure TsgcWS_API_Bittrex.DoEventSignalRDisconnect(const aCloseCode
  : Integer);
begin
  DoEventBittrexDisconnect(aCloseCode);
end;

procedure TsgcWS_API_Bittrex.DoEventSignalRError(const aError: String);
begin
  DoEventBittrexError(aError);
end;

procedure TsgcWS_API_Bittrex.DoEventSignalRMessage(const aMessageId,
  aData: string);
var
  oJSON: TsgcJSON;
  vChannel: String;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aData);
    if oJSON.Count > 0 then
    begin
      if oJSON.Item[0].JSONObject.Node['M'] <> nil then
      begin
        vChannel := oJSON.Item[0].JSONObject.Node['M'].Value;
        if vChannel = CS_BITTREX_AUTHENTICATION_EXPIRING then
          Authenticate
        else if vChannel = CS_BITTREX_HEARTBEAT then
          DoEventBittrexHeartBeat
        else
        begin
          if oJSON.Item[0].JSONObject.Node['A'] <> nil then
          begin
            if oJSON.Item[0].JSONObject.Node['A'].JSONObject.Count = 1 then
              DoEventBittrexMessage(vChannel,
                DoDecodeGZIP(oJSON.Item[0].JSONObject.Node['A'].JSONObject.Item
                [0].Value));
          end;
        end;
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_Bittrex.DoEventSignalRResult(const aInvocationId, aResult,
  aError: string);
var
  oJSON: TsgcJSON;
  vAction, vMethod: String;

  procedure DecodeInvocation(const aInvocationId: String;
    var Action, Method: String);
  var
    i: Integer;
  begin
    Action := '';
    Method := '';
    if aInvocationId <> '' then
    begin
      i := Pos('_', aInvocationId);
      if i > 0 then
      begin
        Action := LeftStr(aInvocationId, i - 1);
        Method := MidStr(aInvocationId, i + 1, Length(aInvocationId));
      end;
    end;
  end;

begin
  if aResult <> '' then
  begin
    if aInvocationId = CS_BITTREX_ISAUTHENTICATED then
    begin
      FResponseIsAuthenticated := True;
      FIsAuthenticated := aResult = 'true';
    end
    else if aInvocationId = CS_BITTREX_AUTHENTICATE then
    begin
      oJSON := TsgcJSON.Create(nil);
      Try
        oJSON.Read(aResult);
        if oJSON.Node['Success'] <> nil then
        begin
          if oJSON.Node['Success'].Value = 'true' then
            DoEventBittrexAuthenticated
          else
          begin
            if oJSON.Node['ErrorCode'] <> nil then
              DoEventBittrexError(oJSON.Node['ErrorCode'].Value)
            else
              DoEventBittrexError('Authentication Error');
          end;
        end;
      Finally
        sgcFree(oJSON);
      End;
    end
    else
    begin
      oJSON := TsgcJSON.Create(nil);
      Try
        oJSON.Read(aResult);
        if oJSON.Count > 0 then
        begin
          if oJSON.Item[0].JSONObject.Node['Success'] <> nil then
          begin
            if oJSON.Item[0].JSONObject.Node['Success'].Value = 'true' then
            begin
              DecodeInvocation(aInvocationId, vAction, vMethod);
              if vAction = CS_BITTREX_SUBSCRIBE then
                DoEventBittrexSubscribed(vMethod)
              else if vAction = CS_BITTREX_UNSUBSCRIBE then
                DoEventBittrexUnSubscribed(vMethod);
            end
            else if oJSON.Item[0].JSONObject.Node['Success'].Value = 'false'
            then
            begin
              if oJSON.Item[0].JSONObject.Node['ErrorCode'] <> nil then
                DoEventBittrexError(oJSON.Item[0].JSONObject.Node
                  ['ErrorCode'].Value);
            end;
          end;
        end;
      Finally
        sgcFree(oJSON);
      End;
    end;
  end;
end;

procedure TsgcWS_API_Bittrex.DoSubscribe(const aMethod: String);
begin
  DoWriteData(CS_BITTREX_SUBSCRIBE, aMethod);
end;

procedure TsgcWS_API_Bittrex.DoUnSubscribe(const aMethod: String);
begin
  DoWriteData(CS_BITTREX_UNSUBSCRIBE, aMethod);
end;

procedure TsgcWS_API_Bittrex.DoWriteData(const aAction, aMethod: String);
var
  vMessage: String;
begin
  vMessage := '{' + '"H":"' + CS_BITTREX_HUB + '", ' + '"I":"' + aAction + '_' +
    aMethod + '", ' + '"M":"' + aAction + '", ' + '"A":[["' + aMethod + '"]]}';

  if Assigned(FClient) then
    FClient.WriteData(vMessage);
end;

function TsgcWS_API_Bittrex.GetAuthentication: string;
var
  vTimeStamp: Int64;
  vUUID, vSignature: String;
  oGuid: TGuid;
begin
  // ... timestamp
  vTimeStamp := GetTimeStamp;
  // ... uuid
  CreateGuid(oGuid);
  vUUID := LowerCase(GuidToString(oGuid));
  vUUID := sgcStringReplace(vUUID, '{', '');
  vUUID := sgcStringReplace(vUUID, '}', '');
  // ... signature
  vSignature := GetHMACSHA512(IntToStr(vTimeStamp) + vUUID, Bittrex.ApiSecret);

  Result := Format('["%s",%d,"%s","%s"]', [Bittrex.ApiKey, vTimeStamp, vUUID,
    vSignature]);
end;

function TsgcWS_API_Bittrex.GetCandleIntervalString
  (aInterval: TsgcWSBittrexCandleInterval): string;
begin
  case aInterval of
    btxc1Min:
      Result := 'MINUTE_1';
    btxc5Min:
      Result := 'MINUTE_5';
    btxc5Hour:
      Result := 'HOUR_1';
    btxc1Day:
      Result := 'DAY_1';
  end;
end;

function TsgcWS_API_Bittrex.GetCandleString(const aSymbol: String;
  aInterval: TsgcWSBittrexCandleInterval): string;
begin
  Result := aSymbol + '_' + GetCandleIntervalString(aInterval);
end;

function TsgcWS_API_Bittrex.GetOrderBookDepthString
  (aDepth: TsgcWSBittrexOrderBookDepth): string;
begin
  Result := '';
  case aDepth of
    btxd1:
      Result := '1';
    btxd25:
      Result := '25';
    btxd500:
      Result := '500';
  end;
end;

function TsgcWS_API_Bittrex.GetOrderBookString(const aSymbol: String;
  aDepth: TsgcWSBittrexOrderBookDepth): string;
begin
  Result := aSymbol + '_' + GetOrderBookDepthString(aDepth);
end;

function TsgcWS_API_Bittrex.GetTimeStamp: Int64;
begin
  {$IFDEF LAZARUS}
  Result := DateTimeToUnix(LocalTimeToUniversal(Now)) * 1000;
  {$ELSE}
  Result := DateTimeToUnix(Now{$IFDEF DXE6}, False{$ENDIF}) * 1000;
  {$ENDIF}
end;

function TsgcWS_API_Bittrex.GetURL: String;
begin
  Result := 'wss://socket-v3.bittrex.com/signalr';

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

function TsgcWS_API_Bittrex.IsAuthenticated(aTimeOut: Integer = 10000): Boolean;
var
  vStart: Cardinal;
begin
  Result := False;

  if Assigned(FClient) then
  begin
    FClient.WriteData('{"H":"' + CS_BITTREX_HUB + '","M":"' +
      CS_BITTREX_ISAUTHENTICATED + '","A":[],"I":"' +
      CS_BITTREX_ISAUTHENTICATED + '"}');

    // ... wait response
    FIsAuthenticated := False;
    FResponseIsAuthenticated := False;
    vStart := sgcGetTicks;
    repeat
      if FResponseIsAuthenticated then
      begin
        Result := FIsAuthenticated;
        break;
      end;
      sleep(1);
    until
      sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeOut);
  end;
end;

procedure TsgcWS_API_Bittrex.SetBittrex(const Value: TsgcWSBittrex_Options);
begin
  FBittrex.Assign(Value);
end;

procedure TsgcWS_API_Bittrex.SubscribeBalance;
begin
  DoSubscribe(CS_BITTREX_BALANCE);
end;

procedure TsgcWS_API_Bittrex.SubscribeCandle(const aSymbol: String;
  aInterval: TsgcWSBittrexCandleInterval);
begin
  DoSubscribe(CS_BITTREX_CANDLE + '_' + GetCandleString(aSymbol, aInterval));
end;

procedure TsgcWS_API_Bittrex.SubscribeDeposit;
begin
  DoSubscribe(CS_BITTREX_DEPOSIT);
end;

procedure TsgcWS_API_Bittrex.SubscribeHeartBeat;
begin
  DoSubscribe(CS_BITTREX_HEARTBEAT);
end;

procedure TsgcWS_API_Bittrex.SubscribeMarketSummaries;
begin
  DoSubscribe(CS_BITTREX_MARKET_SUMMARIES);
end;

procedure TsgcWS_API_Bittrex.SubscribeMarketSummary(const aSymbol: String);
begin
  DoSubscribe(CS_BITTREX_MARKET_SUMMARY + '_' + aSymbol);
end;

procedure TsgcWS_API_Bittrex.SubscribeOrder;
begin
  DoSubscribe(CS_BITTREX_ORDER);
end;

procedure TsgcWS_API_Bittrex.SubscribeOrderBook(const aSymbol: String;
  aDepth: TsgcWSBittrexOrderBookDepth);
begin
  DoSubscribe(CS_BITTREX_ORDERBOOK + '_' + GetOrderBookString(aSymbol, aDepth));
end;

procedure TsgcWS_API_Bittrex.SubscribeTicker(const aSymbol: String);
begin
  DoSubscribe(CS_BITTREX_TICKER + '_' + aSymbol);
end;

procedure TsgcWS_API_Bittrex.SubscribeTickers;
begin
  DoSubscribe(CS_BITTREX_TICKERS);
end;

procedure TsgcWS_API_Bittrex.SubscribeTrade(const aSymbol: String);
begin
  DoSubscribe(CS_BITTREX_TRADE + '_' + aSymbol);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeBalance;
begin
  DoUnSubscribe(CS_BITTREX_BALANCE);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeCandle(const aSymbol: String;
  aInterval: TsgcWSBittrexCandleInterval);
begin
  DoUnSubscribe(CS_BITTREX_CANDLE + '_' + GetCandleString(aSymbol, aInterval));
end;

procedure TsgcWS_API_Bittrex.UnSubscribeDeposit;
begin
  DoUnSubscribe(CS_BITTREX_DEPOSIT);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeHeartBeat;
begin
  DoUnSubscribe(CS_BITTREX_HEARTBEAT);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeMarketSummaries;
begin
  DoUnSubscribe(CS_BITTREX_MARKET_SUMMARIES);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeMarketSummary(const aSymbol: String);
begin
  DoUnSubscribe(CS_BITTREX_MARKET_SUMMARY + '_' + aSymbol);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeOrder;
begin
  DoUnSubscribe(CS_BITTREX_ORDER);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeOrderBook(const aSymbol: String;
  aDepth: TsgcWSBittrexOrderBookDepth);
begin
  DoUnSubscribe(CS_BITTREX_ORDERBOOK + '_' + GetOrderBookString
    (aSymbol, aDepth));
end;

procedure TsgcWS_API_Bittrex.UnSubscribeTicker(const aSymbol: String);
begin
  DoUnSubscribe(CS_BITTREX_TICKER + '_' + aSymbol);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeTickers;
begin
  DoUnSubscribe(CS_BITTREX_TICKERS);
end;

procedure TsgcWS_API_Bittrex.UnSubscribeTrade(const aSymbol: String);
begin
  DoUnSubscribe(CS_BITTREX_TRADE + '_' + aSymbol);
end;

procedure TsgcWSBittrex_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBittrex_Options then
  begin
    ApiKey := TsgcWSBittrex_Options(aSource).ApiKey;
    ApiSecret := TsgcWSBittrex_Options(aSource).ApiSecret;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
