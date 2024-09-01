{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Huobi;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON;

Type
  TsgcWSHuobiErrorEvent = procedure(Sender: TObject; Id, Code, Msg, Ts: string)
    of object;
  TsgcWSHuobiSubscribedEvent = procedure(Sender: TObject; Topic, Ts: String)
    of object;
  TsgcWSHuobiUnSubscribedEvent = procedure(Sender: TObject; Topic, Ts: String)
    of object;
  TsgcWSHuobiUpdateEvent = procedure(Sender: TObject; Data: String) of object;

  TsgcWSHuobiPeriods = (hup1Min, hup5Min, hup15Min, hup30Min, hup60Min, hup1Day,
    hup1Mon, hup1Week, hup1Year);
  TsgcWSHuobiDepths = (hudStep0, hudStep1, hudStep2, hudStep3, hudStep4,
    hudStep5);

  TsgcWSHuobi_Options = class(TPersistent)
  private
    FKey: String;
    FSecret: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Key: String read FKey write FKey;
    property Secret: String read FSecret write FSecret;
  end;

  TsgcWS_API_Huobi = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_client }
  protected
    function GetURL: String; override;
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSAPI_client }

    { helpers }
  private
    { helpers }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { properties }
  private
    FHuobi: TsgcWSHuobi_Options;
    FAuthenticated: Boolean;
  public
    property Huobi: TsgcWSHuobi_Options read FHuobi write FHuobi;
    property Authenticated: Boolean read FAuthenticated;
    { properties }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  public
    procedure Ping;
  private
    function GetPeriod(const aPeriod: TsgcWSHuobiPeriods): String;
    function GetDepth(const aDepth: TsgcWSHuobiDepths): String;
  private
    function DoDecodeGZIP(const aStream: TStream): String;
    function GetRandomId: String;
  private
    procedure DoWriteDataSubscription(const aMethod, aTopic: String; const aId:
        String = '');
  protected
    procedure DoWriteData(const aText: String);
    // ... subscription
  protected
    procedure DoSubscribe(const aTopic: String;
      const aId: String = ''); virtual;
    procedure DoUnSubscribe(const aTopic: String;
      const aId: String = ''); virtual;
  public
    procedure SubscribeKLine(const aSymbol: String; aPeriod: TsgcWSHuobiPeriods;
      const aId: String = '');
    procedure UnSubscribeKLine(const aSymbol: String;
      aPeriod: TsgcWSHuobiPeriods; const aId: String = '');
  public
    procedure SubscribeMarketDepth(const aSymbol: String;
      aDepth: TsgcWSHuobiDepths; const aId: String = '');
    procedure UnSubscribeMarketDepth(const aSymbol: String;
      aDepth: TsgcWSHuobiDepths; const aId: String = '');
  public
    procedure SubscribeTradeDetail(const aSymbol: String;
      const aId: String = '');
    procedure UnSubscribeTradeDetail(const aSymbol: String;
      const aId: String = '');
  public
    procedure SubscribeMarketDetail(const aSymbol: String;
      const aId: String = '');
    procedure UnSubscribeMarketDetail(const aSymbol: String;
      const aId: String = '');
  public
    procedure SubscribeMarketTickers(const aId: String = '');
    procedure UnSubscribeMarketTickers(const aId: String = '');
    // ... authentication
  private
    procedure DoAuthentication;
    procedure DoWriteDataAuthentication(const aTopic: String; const aId: String =
        '');
  protected
    procedure DoRequest(const aTopic: String; const aId: String = ''); virtual;
  public
    procedure GetAccounts(const aId: String = '');
    procedure GetOrders(const aSymbol: String; const aId: String = '');
    procedure GetAccountsList(const aId: String = '');
    procedure GetOrdersList(const aId: String = '');
    procedure GetOrdersDetail(const aId: String = '');
    { methods }

    { events }
  private
    FOnHuobiError: TsgcWSHuobiErrorEvent;
    FOnHuobiSubscribed: TsgcWSHuobiSubscribedEvent;
    FOnHuobiUnSubscribed: TsgcWSHuobiUnSubscribedEvent;
    FOnHuobiUpdate: TsgcWSHuobiUpdateEvent;
  protected
    procedure DoEventHuobiSubscribed; virtual;
    procedure DoEventHuobiUnSubscribed; virtual;
    procedure DoEventHuobiUpdate(const aData: string); virtual;
    procedure DoEventHuobiError; virtual;
  public
    property OnHuobiSubscribed: TsgcWSHuobiSubscribedEvent
      read FOnHuobiSubscribed write FOnHuobiSubscribed;
    property OnHuobiUnSubscribed: TsgcWSHuobiUnSubscribedEvent
      read FOnHuobiUnSubscribed write FOnHuobiUnSubscribed;
    property OnHuobiUpdate: TsgcWSHuobiUpdateEvent read FOnHuobiUpdate
      write FOnHuobiUpdate;
    property OnHuobiError: TsgcWSHuobiErrorEvent read FOnHuobiError
      write FOnHuobiError;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  // indy
  {$IFDEF SGC_INDY}sgcIdZlib{$ELSE}IdZlib{$ENDIF},
  // sgc
  sgcBase_Helpers, sgcWebSocket_Helpers;

constructor TsgcWS_API_Huobi.Create(aOwner: TComponent);
begin
  inherited;
  FHuobi := TsgcWSHuobi_Options.Create;
  FAuthenticated := False;
end;

destructor TsgcWS_API_Huobi.Destroy;
begin
  sgcFree(FHuobi);
  inherited;
end;

procedure TsgcWS_API_Huobi.DoAuthentication;
var
  vKey, vTimeStamp: String;

  function URIEncode(const aText: String): String;
  begin
    result := sgcStringReplace(aText, ':', '%3A');
    result := sgcStringReplace(result, ' ', '% 20');
  end;

begin
  FAuthenticated := False;

  vTimeStamp := FormatDateTime('yyyy-mm-dd', Now) + 'T' + FormatDateTime('hh:nn:ss', Now);
  vKey := URIEncode(
    'GET' + #10 +
    'api.huobipro.com' + #10 +
    '/ws/v1' + #10 +
    'AccessKeyId=' + Huobi.Key + '&' +
    'Signature Method=HmacSHA256&' +
    'SignatureVersion=2&' +
    'Timestamp=' + vTimeStamp);


  DoWriteData(
    '{' +
      '"op": "auth", ' +
      '"AccessKeyId": "' + Huobi.Key + '",' +
      '"SignatureMethod": "HmacSHA256",' +
      '"SignatureVersion": "2",' +
      '"Timestamp": "' + vTimeStamp + '",' +
      '"Signature": "' + GetHMACSHA256(vKey, Huobi.Secret) + '"' +
    '}');
end;

function TsgcWS_API_Huobi.DoDecodeGZIP(const aStream: TStream): String;
var
  oStringStream: TsgcStringStream;
begin
  result := '';

  oStringStream := TsgcStringStream.Create('');
  Try
    {$IFDEF SGC_INDY}sgcIdZlib{$ELSE}IdZlib{$ENDIF}.DecompressStream(aStream, oStringStream);
    result := oStringStream.DataString;
  Finally
    sgcFree(oStringStream);
  End;
end;

procedure TsgcWS_API_Huobi.DoEventBinary(const aConnection: TsgcWSConnection;
  Data: TMemoryStream);
var
  vText: string;
begin
  vText := DoDecodeGZIP(Data);

  if RawMessages then
    DoEventMessage(aConnection, vText)
  else
  begin
    JSON.Clear;
    JSON.Read(vText);
    if JSON.Node['ping'] <> nil then
      DoWriteData(sgcStringReplace(vText, 'ping', 'pong'))
    else if JSON.Node['ch'] <> nil then
      DoEventHuobiUpdate(vText)
    else if JSON.Node['status'] <> nil then
    begin
      if JSON.Node['status'].Value = 'error' then
        DoEventHuobiError
      else if JSON.Node['status'].Value = 'ok' then
      begin
        if JSON.Node['subbed'] <> nil then
          DoEventHuobiSubscribed
        else if JSON.Node['unsubbed'] <> nil then
          DoEventHuobiUnSubscribed;
      end;
    end
    else if JSON.Node['op'] <> nil then
    begin
      if JSON.Node['op'].Value = 'auth' then
        FAuthenticated := JSON.Node['err-code'].Value = 0;
    end;
    // inherited;
  end;
end;

procedure TsgcWS_API_Huobi.DoEventConnect(aConnection: TsgcWSConnection);
begin
  FAuthenticated := False;
  inherited;
end;

procedure TsgcWS_API_Huobi.DoEventHuobiError;
var
  vId, vCode, vMsg, vTS: String;
begin
  if Assigned(FOnHuobiError) then
  begin
    vId := JSON.Node['status'].Value;
    vCode := JSON.Node['err-code'].Value;
    vMsg := JSON.Node['err-msg'].Value;
    vTS := JSON.Node['ts'].Value;

    FOnHuobiError(self, vId, vCode, vMsg, vTS);
  end;
end;

procedure TsgcWS_API_Huobi.DoEventHuobiSubscribed;
var
  vTopic, vTS: String;
begin
  if Assigned(FOnHuobiSubscribed) then
  begin
    vTopic := JSON.Node['subbed'].Value;
    vTS := JSON.Node['ts'].Value;

    FOnHuobiSubscribed(self, vTopic, vTS);
  end;
end;

procedure TsgcWS_API_Huobi.DoEventHuobiUnSubscribed;
var
  vTopic, vTS: String;
begin
  if Assigned(FOnHuobiUnSubscribed) then
  begin
    vTopic := JSON.Node['unsubbed'].Value;
    vTS := JSON.Node['ts'].Value;

    FOnHuobiUnSubscribed(self, vTopic, vTS);
  end;
end;

procedure TsgcWS_API_Huobi.DoEventHuobiUpdate(const aData: string);
begin
  if Assigned(FOnHuobiUpdate) then
    FOnHuobiUpdate(self, aData);
end;

function TsgcWS_API_Huobi.DoKeepAlive: Boolean;
begin
  result := True;

  Ping;
end;

procedure TsgcWS_API_Huobi.DoRequest(const aTopic: String; const aId: String =
    '');
var
  vId: String;
begin
  vId := aId;
  if vId = '' then
    vId := GetRandomId;

  DoWriteData('{"req": "' + aTopic + '", "id": "' + vId + '"}');
end;

procedure TsgcWS_API_Huobi.DoWriteDataSubscription(const aMethod, aTopic:
    String; const aId: String = '');
var
  vId: String;
begin
  vId := aId;
  if vId = '' then
    vId := GetRandomId;

  DoWriteData('{"' + aMethod + '": "' + aTopic + '", "id": "' + vId + '"}');
end;

procedure TsgcWS_API_Huobi.DoSubscribe(const aTopic: String;
  const aId: String = '');
begin
  DoWriteDataSubscription('sub', aTopic, aId);
end;

procedure TsgcWS_API_Huobi.DoUnSubscribe(const aTopic: String;
  const aId: String = '');
begin
  DoWriteDataSubscription('unsub', aTopic, aId);
end;

procedure TsgcWS_API_Huobi.DoWriteData(const aText: String);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

procedure TsgcWS_API_Huobi.DoWriteDataAuthentication(const aTopic: String;
    const aId: String = '');
var
  vId: String;
begin
  vId := aId;
  if vId = '' then
    vId := GetRandomId;

  DoWriteData('{"op": "sub", "cid": "' + vId + '", "topic": "' + aTopic + '"}');
end;

procedure TsgcWS_API_Huobi.GetAccounts(const aId: String = '');
begin
  DoAuthentication;
  DoWriteDataAuthentication('accounts', aId);
end;

procedure TsgcWS_API_Huobi.GetAccountsList(const aId: String = '');
begin
  DoAuthentication;
  DoRequest('accounts.list', aId);
end;

function TsgcWS_API_Huobi.GetDepth(const aDepth: TsgcWSHuobiDepths): String;
begin
  result := '';
  case aDepth of
    hudStep0:
      result := 'step0';
    hudStep1:
      result := 'step1';
    hudStep2:
      result := 'step2';
    hudStep3:
      result := 'step3';
    hudStep4:
      result := 'step4';
    hudStep5:
      result := 'step5';
  end;
end;

function TsgcWS_API_Huobi.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  result := FJSON;
end;

procedure TsgcWS_API_Huobi.GetOrders(const aSymbol: String; const aId: String =
    '');
begin
  DoAuthentication;
  DoWriteDataAuthentication('orders.' + aSymbol, aId);
end;

procedure TsgcWS_API_Huobi.GetOrdersDetail(const aId: String = '');
begin
  DoAuthentication;
  DoRequest('orders.detail', aId);
end;

procedure TsgcWS_API_Huobi.GetOrdersList(const aId: String = '');
begin
  DoAuthentication;
  DoRequest('orders.list', aId);
end;

function TsgcWS_API_Huobi.GetPeriod(const aPeriod: TsgcWSHuobiPeriods): String;
begin
  result := '';
  case aPeriod of
    hup1Min:
      result := '1min';
    hup5Min:
      result := '5min';
    hup15Min:
      result := '15min';
    hup30Min:
      result := '30min';
    hup60Min:
      result := '60min';
    hup1Day:
      result := '1day';
    hup1Mon:
      result := '1mon';
    hup1Week:
      result := '1week';
    hup1Year:
      result := '1year';
  end;
end;

function TsgcWS_API_Huobi.GetRandomId: String;
begin
  result := FormatDateTime('yyyymmddhhnnsszzz', Now);
end;

function TsgcWS_API_Huobi.GetURL: String;
begin
  result := 'wss://api.huobi.pro/ws';
end;

procedure TsgcWS_API_Huobi.Ping;
begin
  DoWriteData('{"ping": ' + FormatDateTime('yyyymmddhhnnss', Now) + '}');
end;

procedure TsgcWS_API_Huobi.SubscribeKLine(const aSymbol: String;
  aPeriod: TsgcWSHuobiPeriods; const aId: String = '');
begin
  DoSubscribe('market.' + aSymbol + '.kline.' + GetPeriod(aPeriod));
end;

procedure TsgcWS_API_Huobi.SubscribeMarketDepth(const aSymbol: String;
  aDepth: TsgcWSHuobiDepths; const aId: String = '');
begin
  DoSubscribe('market.' + aSymbol + '.depth.' + GetDepth(aDepth));
end;

procedure TsgcWS_API_Huobi.SubscribeMarketDetail(const aSymbol: String;
  const aId: String = '');
begin
  DoSubscribe('market.' + aSymbol + '.detail');
end;

procedure TsgcWS_API_Huobi.SubscribeMarketTickers(const aId: String = '');
begin
  DoSubscribe('market.tickers');
end;

procedure TsgcWS_API_Huobi.SubscribeTradeDetail(const aSymbol: String;
  const aId: String = '');
begin
  DoSubscribe('market.' + aSymbol + '.trade.detail');
end;

procedure TsgcWS_API_Huobi.UnSubscribeKLine(const aSymbol: String;
  aPeriod: TsgcWSHuobiPeriods; const aId: String = '');
begin
  DoUnSubscribe('market.' + aSymbol + '.kline.' + GetPeriod(aPeriod));
end;

procedure TsgcWS_API_Huobi.UnSubscribeMarketDepth(const aSymbol: String;
  aDepth: TsgcWSHuobiDepths; const aId: String = '');
begin
  DoUnSubscribe('market.' + aSymbol + '.depth.' + GetDepth(aDepth));
end;

procedure TsgcWS_API_Huobi.UnSubscribeMarketDetail(const aSymbol: String;
  const aId: String = '');
begin
  DoUnSubscribe('market.' + aSymbol + '.detail');
end;

procedure TsgcWS_API_Huobi.UnSubscribeMarketTickers(const aId: String = '');
begin
  DoUnSubscribe('market.tickers');
end;

procedure TsgcWS_API_Huobi.UnSubscribeTradeDetail(const aSymbol: String;
  const aId: String = '');
begin
  DoUnSubscribe('market.' + aSymbol + '.trade.detail');
end;

procedure TsgcWSHuobi_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSHuobi_Options then
  begin
    Key := TsgcWSHuobi_Options(aSource).Key;
    Secret := TsgcWSHuobi_Options(aSource).Secret;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
