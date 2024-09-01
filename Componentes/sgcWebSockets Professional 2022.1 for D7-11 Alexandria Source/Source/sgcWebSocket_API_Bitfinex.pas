{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Bitfinex;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON;

type
  TsgcWSBitfinexConnectEvent = procedure(Sender: TObject;
    Version: String) of object;
  TsgcWSBifinexInfoMessageEvent = procedure(Sender: TObject;
    Code, Msg: String) of object;
  TsgcWSBitfinexAuthEvent = procedure(Sender: TObject; Status: String;
    ChanId, Code: Integer; Msg: String) of object;
  TsgcWSBitfinexUnauthEvent = procedure(Sender: TObject; Status: String;
    ChanId: Integer) of object;
  TsgcWSBitfinexSubscribedEvent = procedure(Sender: TObject; Channel: String;
    ChanId: Integer; Symbol, Pair, Key: String) of object;
  TsgcWSBitfinexUnSubscribedEvent = procedure(Sender: TObject; Status: String;
      ChanId: Integer) of object;
  TsgcWSBitfinexUpdate = procedure(Sender: TObject; Data: String) of object;
  TsgcWSBitfinexErrorEvent = procedure(Sender: TObject; Status: String;
    ChanId, Code: Integer) of object;

  TsgcWSBitfinexPrecision = (P0, P1, P2, P3);
  TsgcWSBitfinexFrequency = (F0, F1);

  TsgcWS_API_Bitfinex = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_client }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { helpers }
  private
    function GetPrecision(aPrecision: TsgcWSBitfinexPrecision): String;
    function GetFrequency(aFrequency: TsgcWSBitfinexFrequency): String;
    { helpers }

    { read event }
  protected
    procedure DoReadEvent(const aEvent: string);
    { read event }

    { procedures }
  protected
    procedure DoPong(const aCid: String);
  public
    procedure Ping;
    procedure Configuration(aFlags: Integer);
  public
    procedure Authenticate(aAPIKey, aAuthPayload, aAuthNonce, aAuthSig: String);
    procedure UnAuthenticate;
  public
    procedure SubscribeTicker(const aSymbol: String);
    procedure UnSubscribeTicker(const aChanId: Integer);
  public
    procedure SubscribeTrades(const aSymbol: String);
    procedure UnSubscribeTrades(const aChanId: Integer);
  public
    procedure SubscribeOrderBook(const aPair: String;
      aPrecision: TsgcWSBitfinexPrecision = P0;
      aFrequency: TsgcWSBitfinexFrequency = F0; aLength: Integer = 25);
    procedure UnSubscribeOrderBook(aChanId: Integer);
  public
    procedure SubscribeRawOrderBook(const aPair: String);
    procedure UnSubscribeRawOrderBook(aChanId: Integer);
  public
    procedure SubscribeCandles(const aKey: String);
    procedure UnSubscribeCandles(aChanId: Integer);
    { procedures }

    { events }
  private
    FOnBitfinexAuth: TsgcWSBitfinexAuthEvent;
    FOnBitfinexConnect: TsgcWSBitfinexConnectEvent;
    FOnBitfinexError: TsgcWSBitfinexErrorEvent;
    FOnBitfinexInfoMsg: TsgcWSBifinexInfoMessageEvent;
    FOnBitfinexSubscribed: TsgcWSBitfinexSubscribedEvent;
    FOnBitfinexUnauth: TsgcWSBitfinexUnauthEvent;
    FOnBitfinexUnSubscribed: TsgcWSBitfinexUnSubscribedEvent;
    FOnBitfinexUpdate: TsgcWSBitfinexUpdate;
  protected
    procedure DoEventConnected; virtual;
    procedure DoEventAuthentication; virtual;
    procedure DoEventUnauthentication; virtual;
    procedure DoEventInfoMessage; virtual;
    procedure DoEventSubscribed; virtual;
    procedure DoEventUnSubscribed; virtual;
    procedure DoEventUpdate(aData: string); virtual;
    procedure DoEventErr; virtual;
  public
    property OnBitfinexInfoMsg
      : TsgcWSBifinexInfoMessageEvent read FOnBitfinexInfoMsg write
      FOnBitfinexInfoMsg;
    property OnBitfinexSubscribed
      : TsgcWSBitfinexSubscribedEvent read FOnBitfinexSubscribed write
      FOnBitfinexSubscribed;
    property OnBitfinexUnSubscribed
      : TsgcWSBitfinexUnSubscribedEvent read FOnBitfinexUnSubscribed write
      FOnBitfinexUnSubscribed;
    property OnBitfinexUpdate
      : TsgcWSBitfinexUpdate read FOnBitfinexUpdate write
      FOnBitfinexUpdate;
    property OnBitfinexAuth: TsgcWSBitfinexAuthEvent read FOnBitfinexAuth write
      FOnBitfinexAuth;
    property OnBitfinexUnauth
      : TsgcWSBitfinexUnauthEvent read FOnBitfinexUnauth write
      FOnBitfinexUnauth;
    property OnBitfinexError
      : TsgcWSBitfinexErrorEvent read FOnBitfinexError write
      FOnBitfinexError;
    property OnBitfinexConnect
      : TsgcWSBitfinexConnectEvent read FOnBitfinexConnect write
      FOnBitfinexConnect;
  end;

  // ... Configuration values
const
  CS_DEC_S = 8; // Enable all decimal as strings.
  CS_TIME_S = 32; // Enable all times as date strings.
  CS_SEQ_ALL = 65536; // Enable sequencing BETA FEATURE
  CHECKSUM = 131072; // Enable checksum for every book iteration. Checks the top 25 entries for each side of book. Checksum is a signed int.
{$ENDIF}

implementation

{$IFDEF SGC_APIS}

const
  CS_BITFINEX_AUTH = 'auth';
  CS_BITFINEX_UNAUTH = 'unauth';
  CS_BITFINEX_SUBSCRIBE = 'subscribe';
  CS_BITFINEX_UNSUBSCRIBE = 'unsubscribe';
  CS_BITFINEX_BOOK = 'book';
  CS_BITFINEX_TICKER = 'ticker';
  CS_BITFINEX_TRADES = 'trades';
  CS_BITFINEX_CANDLES = 'candles';
  CS_BITFINEX_PING = 'Ping';
  CS_BITFINEX_CONFIGURATION = 'Configuration';

procedure TsgcWS_API_Bitfinex.Authenticate(aAPIKey, aAuthPayload, aAuthNonce,
  aAuthSig: String);
begin
  FClient.WriteData('{' + '"apiKey": "' + aAPIKey + '", ' + '"event": "' +
      CS_BITFINEX_AUTH + '", ' + '"authPayload": "' + aAuthPayload + '", ' +
      '"authNonce": "' + aAuthNonce + '", ' + '"authSig": "' + aAuthSig +
      '" ' + '}');
end;

procedure TsgcWS_API_Bitfinex.DoEventInfoMessage;
var
  vCode, vMessage: String;
begin
  if Assigned(FOnBitfinexInfoMsg) then
  begin
    vCode := JSON.Node['code'].Value;
    if JSON.Node['message'] <> nil then
      vMessage := JSON.Node['message'].Value;
    FOnBitfinexInfoMsg(self, vCode, vMessage);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoPong(const aCid: String);
begin
  FClient.WriteData('{"event":"pong", "ts":' + FormatDateTime
      ('yyyymmddhhnnsszzz', Now) + ', "cid":' + aCid + '}');
end;

procedure TsgcWS_API_Bitfinex.DoReadEvent(const aEvent: string);
begin
  if aEvent = 'subscribed' then
    DoEventSubscribed
  else if aEvent = 'unsubscribed' then
    DoEventUnSubscribed
  else if aEvent = 'auth' then
    DoEventAuthentication
  else if aEvent = 'unauth' then
    DoEventUnauthentication
  else if aEvent = 'info' then
  begin
    if JSON.Node['code'] <> nil then
      DoEventInfoMessage
    else if JSON.Node['version'] <> nil then
      DoEventConnected
  end
  else if aEvent = 'Ping' then
  begin
    if JSON.Node['cid'] <> nil then
      DoPong(JSON.Node['cid'].Value);
  end
  else if aEvent = 'error' then
    DoEventErr;
end;

function TsgcWS_API_Bitfinex.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

procedure TsgcWS_API_Bitfinex.Ping;
begin
  FClient.WriteData('{' + '"event":"' + CS_BITFINEX_PING + '", ' + '"cid":' +
      FormatDateTime('hhnnsszzz', Now) + '}');
end;

procedure TsgcWS_API_Bitfinex.Configuration(aFlags: Integer);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_CONFIGURATION + '", ' +
      '"flags": ' + IntToStr(aFlags) + '}');
end;

procedure TsgcWS_API_Bitfinex.DoEventAuthentication;
var
  vStatus, vMsg: String;
  vChanId, vCode: Integer;
begin
  if Assigned(FOnBitfinexAuth) then
  begin
    vStatus := JSON.Node['status'].Value;
    vChanId := JSON.Node['chanId'].Value;
    vCode := JSON.Node['code'].Value;
    vMsg := JSON.Node['msg'].Value;

    FOnBitfinexAuth(self, vStatus, vChanId, vCode, vMsg);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventConnected;
var
  vVersion: String;
begin
  if Assigned(FOnBitfinexConnect) then
  begin
    vVersion := JSON.Node['version'].Value;

    FOnBitfinexConnect(self, vVersion);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventErr;
var
  vStatus: String;
  vChanId, vCode: Integer;
begin
  if Assigned(FOnBitfinexError) then
  begin
    vStatus := JSON.Node['status'].Value;
    vChanId := JSON.Node['chanId'].Value;
    vCode := JSON.Node['code'].Value;

    FOnBitfinexError(self, vStatus, vChanId, vCode);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    JSON.Clear;
    if LeftStr(Text, 1) = '[' then
      DoEventUpdate(Text)
    else
    begin
      JSON.Read(Text);

      if JSON.Node['event'] <> nil then
        DoReadEvent(JSON.Node['event'].Value)
      else
        inherited;
    end;
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventSubscribed;
var
  vChannel, vSymbol, vPair, vKey: String;
  vChanId: Integer;
begin
  vSymbol := '';
  vPair := '';
  vKey := '';
  if Assigned(FOnBitfinexSubscribed) then
  begin
    vChannel := JSON.Node['channel'].Value;
    vChanId := JSON.Node['chanId'].Value;
    if JSON.Node['symbol'] <> nil then
    begin
      vSymbol := JSON.Node['symbol'].Value;
      vPair := JSON.Node['pair'].Value;
    end
    else if JSON.Node['key'] <> nil then
      vKey := JSON.Node['key'].Value;

    FOnBitfinexSubscribed(self, vChannel, vChanId, vSymbol, vPair, vKey);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventUnauthentication;
var
  vStatus: String;
  vChanId: Integer;
begin
  if Assigned(FOnBitfinexUnauth) then
  begin
    vStatus := JSON.Node['status'].Value;
    vChanId := JSON.Node['chanId'].Value;

    FOnBitfinexUnauth(self, vStatus, vChanId);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventUnSubscribed;
var
  vStatus: String;
  vChanId: Integer;
begin
  if Assigned(FOnBitfinexUnSubscribed) then
  begin
    vStatus := JSON.Node['status'].Value;
    vChanId := JSON.Node['chanId'].Value;

    FOnBitfinexUnSubscribed(self, vStatus, vChanId);
  end;
end;

procedure TsgcWS_API_Bitfinex.DoEventUpdate(aData: string);
begin
  if Assigned(FOnBitfinexUpdate) then
    FOnBitfinexUpdate(self, aData);
end;

function TsgcWS_API_Bitfinex.GetFrequency(aFrequency: TsgcWSBitfinexFrequency)
  : String;
begin
  case aFrequency of
    F1:
      Result := 'F1';
  else
    Result := 'F0';
  end;
end;

function TsgcWS_API_Bitfinex.GetPrecision(aPrecision: TsgcWSBitfinexPrecision)
  : String;
begin
  case aPrecision of
    P1:
      Result := 'P1';
    P2:
      Result := 'P2';
    P3:
      Result := 'P3';
  else
    Result := 'P0';
  end;
end;

function TsgcWS_API_Bitfinex.GetURL: String;
begin
  Result := 'wss://api.bitfinex.com/ws/2/';
end;

procedure TsgcWS_API_Bitfinex.SubscribeCandles(const aKey: String);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_SUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_CANDLES + '", ' + '"key": "' + aKey + '"' +
      '}');
end;

procedure TsgcWS_API_Bitfinex.SubscribeOrderBook(const aPair: String;
  aPrecision: TsgcWSBitfinexPrecision = P0;
  aFrequency: TsgcWSBitfinexFrequency = F0; aLength: Integer = 25);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_SUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_BOOK + '", ' + '"pair": "' + aPair + '", ' +
      '"prec": "' + GetPrecision(aPrecision) + '", ' + '"freq": "' +
      GetFrequency(aFrequency) + '", ' + '"len": ' + IntToStr(aLength) + '}');
end;

procedure TsgcWS_API_Bitfinex.SubscribeRawOrderBook(const aPair: String);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_SUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_BOOK + '", ' + '"pair": "' + aPair + '", ' +
      '"prec": "R0"' + '}');
end;

procedure TsgcWS_API_Bitfinex.SubscribeTicker(const aSymbol: String);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_SUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_TICKER + '", ' + '"symbol": "' + aSymbol +
      '"' + '}');
end;

procedure TsgcWS_API_Bitfinex.SubscribeTrades(const aSymbol: String);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_SUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_TRADES + '", ' + '"symbol": "' + aSymbol +
      '"' + '}');
end;

procedure TsgcWS_API_Bitfinex.UnAuthenticate;
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_UNAUTH + '"' + '}');
end;

procedure TsgcWS_API_Bitfinex.UnSubscribeCandles(aChanId: Integer);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_UNSUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_CANDLES + '", ' + '"chanId": ' + IntToStr
      (aChanId) + '}');
end;

procedure TsgcWS_API_Bitfinex.UnSubscribeOrderBook(aChanId: Integer);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_UNSUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_BOOK + '", ' + '"chanId": ' + IntToStr
      (aChanId) + '}');
end;

procedure TsgcWS_API_Bitfinex.UnSubscribeRawOrderBook(aChanId: Integer);
begin
  UnSubscribeOrderBook(aChanId);
end;

procedure TsgcWS_API_Bitfinex.UnSubscribeTicker(const aChanId: Integer);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_UNSUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_TICKER + '", ' + '"chanId": ' + IntToStr
      (aChanId) + '}');
end;

procedure TsgcWS_API_Bitfinex.UnSubscribeTrades(const aChanId: Integer);
begin
  FClient.WriteData('{' + '"event": "' + CS_BITFINEX_UNSUBSCRIBE + '", ' +
      '"channel": "' + CS_BITFINEX_TRADES + '", ' + '"chanId": ' + IntToStr
      (aChanId) + '}');
end;
{$ENDIF}

end.
