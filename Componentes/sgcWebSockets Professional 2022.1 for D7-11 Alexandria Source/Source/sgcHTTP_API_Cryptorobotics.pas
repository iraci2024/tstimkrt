{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_Cryptorobotics;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type
  TsgcHTTPCTRBOrderSide = (ctrosNone, ctrosBuy, ctrosSell);

type

  TsgcHTTPCryptorobotics_Signal_Parameters_Config = class
  private
    FBreakeven: Boolean;
    FLeverage: Integer;
    FStopLossLastCandle: Integer;
  public
    property Breakeven: Boolean read FBreakeven write FBreakeven;
    property Leverage: Integer read FLeverage write FLeverage;
    property StopLossLastCandle: Integer read FStopLossLastCandle
      write FStopLossLastCandle;
  end;

  TsgcHTTPCryptorobotics_Signal_Parameters = class
  private
    FComment: string;
    FConfig: TsgcHTTPCryptorobotics_Signal_Parameters_Config;
    FExchange: string;
    FExternalId: Integer;
    FFinishTime: TDateTime;
    FLastPrice: Extended;
    FMarket: string;
    FMaxPrice: Extended;
    FMinPrice: Extended;
    FOptionalExchanges: string;
    FSide: TsgcHTTPCTRBOrderSide;
    FStopLoss: Extended;
    FTargets: string;
    FTimeZone: string;
    function GetConfig: TsgcHTTPCryptorobotics_Signal_Parameters_Config;
  public
    destructor Destroy; override;
  public
    property Side: TsgcHTTPCTRBOrderSide read FSide write FSide;
    property Exchange: string read FExchange write FExchange;
    property OptionalExchanges: string read FOptionalExchanges
      write FOptionalExchanges;
    property Market: string read FMarket write FMarket;
    property MinPrice: Extended read FMinPrice write FMinPrice;
    property MaxPrice: Extended read FMaxPrice write FMaxPrice;
    property Targets: string read FTargets write FTargets;
    property StopLoss: Extended read FStopLoss write FStopLoss;
    property FinishTime: TDateTime read FFinishTime write FFinishTime;
    property TimeZone: string read FTimeZone write FTimeZone;
    property Comment: string read FComment write FComment;
    property Config: TsgcHTTPCryptorobotics_Signal_Parameters_Config
      read GetConfig write FConfig;
    property LastPrice: Extended read FLastPrice write FLastPrice;
    property ExternalId: Integer read FExternalId write FExternalId;
  end;

  TsgcHTTPCryptorobotics_Signal = class
  private
    FChannelHash: string;
    FChannelId: Integer;
    FParameters: TsgcHTTPCryptorobotics_Signal_Parameters;
    function GetParameters: TsgcHTTPCryptorobotics_Signal_Parameters;
  protected
    function AsJSON(aEdit: Boolean = False): string;
  public
    destructor Destroy; override;
  public
    property ChannelHash: string read FChannelHash write FChannelHash;
    property ChannelId: Integer read FChannelId write FChannelId;
    property Parameters: TsgcHTTPCryptorobotics_Signal_Parameters
      read GetParameters write FParameters;
  end;

type
  TsgcHTTPCryptoroboticsLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

type
  TsgcHTTPCryptoroboticsAuthentication_Options = class(TPersistent)
  private
    FUserId: string;
    FSecret: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property UserId: string read FUserId write FUserId;
    property Secret: String read FSecret write FSecret;
  end;

type
  TsgcHTTPCryptorobotics_Options = class(TPersistent)
  private
    FAuthentication: TsgcHTTPCryptoroboticsAuthentication_Options;
    FLogOptions: TsgcHTTPCryptoroboticsLog_Options;
    procedure SetAuthentication(const Value
      : TsgcHTTPCryptoroboticsAuthentication_Options);
    procedure SetLogOptions(const Value: TsgcHTTPCryptoroboticsLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Authentication: TsgcHTTPCryptoroboticsAuthentication_Options
      read FAuthentication write SetAuthentication;
    property LogOptions: TsgcHTTPCryptoroboticsLog_Options read FLogOptions
      write SetLogOptions;
  end;

  TsgcHTTP_API_Cryptorobotics = class(TsgcHTTPAPI_Client)
    { http requests }
  private
    procedure DoInitialize;
  private
    function GetBaseURL: string;
  protected
    function DoHTTP_GET(const aMethod: String): string; virtual;
    function DoHTTP_POST(const aMethod: String; const aBody: String = '')
      : string; virtual;
    { http requests }

    { properties }
  private
    FCryptoroboticsOptions: TsgcHTTPCryptorobotics_Options;
    procedure SetCryptoroboticsOptions(const Value
      : TsgcHTTPCryptorobotics_Options);
  public
    property CryptoroboticsOptions: TsgcHTTPCryptorobotics_Options
      read FCryptoroboticsOptions write SetCryptoroboticsOptions;
    { properties }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  private
    FToken: string;
  protected
    function DoCreateSignal(const aSignal: TsgcHTTPCryptorobotics_Signal)
      : string; virtual;
    function DoGetChannelList: string; virtual;
    function DoGetSignalsList(aChannelId, aLimit: Integer): string; virtual;
    function DoGetSignal(aSignalId: Integer; const aChannelHash: string;
      aExternalId: Integer): string;
    function DoEditSignal(aSignalId: Integer; const aChannelHash: string;
      aExternalId: Integer;
      const aSignal: TsgcHTTPCryptorobotics_Signal): string;
    function DoCancelSignal(aSignalId: Integer; const aChannelHash: string;
        aExternalId: Integer): string;
  protected
    procedure DoUserToken; virtual;
    function GetUserToken: string; virtual;
  public
    function GetChannelList: string;
    function GetSignalsList(aChannelId: Integer = 0;
      aLimit: Integer = 10): string;
    function GetSignalById(aSignalId: Integer): string;
    function GetSignalByHash(const aChannelHash: string;
      aExternalId: Integer): string;
    function CreateSignal(const aSignal: TsgcHTTPCryptorobotics_Signal): string;
    function EditSignalById(aSignalId: Integer;
      const aSignal: TsgcHTTPCryptorobotics_Signal): string;
    function EditSignalByHash(const aChannelHash: string; aExternalId: Integer;
      const aSignal: TsgcHTTPCryptorobotics_Signal): string;
    function CancelSignalById(aSignalId: Integer): string;
    function CancelSignalByHash(const aChannelHash: string; aExternalId: Integer):
        string;
    { methods }
  end;

resourcestring
  S_CRYPTOROBOTICS_ERROR_USER_TOKEN = 'Error trying to get the User Token';

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  StrUtils,
{$IFDEF SGC_INDY}sgcIdHTTP{$ELSE}IdHTTP{$ENDIF},
  sgcBase_Helpers, sgcJSON;

const
  CS_CRYPTOROBOTICS_ENDPOINT_BASE = 'https://cryptorobotics.net/';

const
  CS_CRYPTOROBOTICS_ENDPOINT_USER_TOKEN = 'login/getUserToken/%s?secret=%s';
  CS_CRYPTOROBOTICS_ENDPOINT_CHANNEL_LIST = 'signals/channel/list?token=%s';
  CS_CRYPTOROBOTICS_ENDPOINT_SIGNALS_LIST = 'signals/signal/list?token=%s';
  CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_CREATE = 'signals/signal/create?token=%s';
  CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_ITEM = 'signals/signal/item?token=%s';
  CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_EDIT = 'signals/signal/edit?token=%s';
  CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_CANCEL = 'signals/signal/cancel?token=%s';

function GetOrderSideStr(aOrderAction: TsgcHTTPCTRBOrderSide): string;
begin
  case aOrderAction of
    ctrosBuy:
      result := 'buy';
    ctrosSell:
      result := 'sell';
  else
    result := '';
  end;
end;

constructor TsgcHTTP_API_Cryptorobotics.Create(aOwner: TComponent);
begin
  inherited;
  FCryptoroboticsOptions := TsgcHTTPCryptorobotics_Options.Create;
end;

destructor TsgcHTTP_API_Cryptorobotics.Destroy;
begin
  sgcFree(FCryptoroboticsOptions);
  inherited;
end;

function TsgcHTTP_API_Cryptorobotics.CancelSignalByHash(
    const aChannelHash: string; aExternalId: Integer): string;
begin
  DoUserToken;

  Try
    result := DoCancelSignal(0, aChannelHash, aExternalId);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoCancelSignal(0, aChannelHash, aExternalId);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.CancelSignalById(aSignalId: Integer):
    string;
begin
  DoUserToken;

  Try
    result := DoCancelSignal(aSignalId, '', 0);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoCancelSignal(aSignalId, '', 0);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.DoGetChannelList: string;
begin
  result := DoHTTP_GET(Format(CS_CRYPTOROBOTICS_ENDPOINT_CHANNEL_LIST,
    [FToken]))
end;

function TsgcHTTP_API_Cryptorobotics.DoGetSignal(aSignalId: Integer;
  const aChannelHash: string; aExternalId: Integer): string;
var
  oParameters: TStringList;
  vURL: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := '&';
    if aSignalId <> 0 then
      oParameters.Add('id=' + IntToStr(aSignalId))
    else if aChannelHash <> '' then
    begin
      oParameters.Add('external_id=' + aChannelHash);
      oParameters.Add('channel_hash=' + IntToStr(aExternalId));
    end;

    vURL := Format(CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_ITEM, [FToken]);
    if oParameters.Count > 0 then
      vURL := vURL + '&' + oParameters.DelimitedText;

    result := DoHTTP_GET(vURL)
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Cryptorobotics.DoGetSignalsList(aChannelId,
  aLimit: Integer): string;
var
  oParameters: TStringList;
  vURL: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := '&';
    if aChannelId <> 0 then
      oParameters.Add('channel_id=' + IntToStr(aChannelId));
    if aLimit <> 10 then
      oParameters.Add('limit=' + IntToStr(aLimit));

    vURL := Format(CS_CRYPTOROBOTICS_ENDPOINT_SIGNALS_LIST, [FToken]);
    if oParameters.Count > 0 then
      vURL := vURL + '&' + oParameters.DelimitedText;

    result := DoHTTP_GET(vURL)
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Cryptorobotics.DoHTTP_GET(const aMethod: String): string;
var
  oHeaders: TStringList;
begin
  DoInitialize;

  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');

    Try
      result := Get(GetBaseURL + aMethod, oHeaders);
    Except
      On E: Exception do
      begin
        FToken := '';
        raise;
      end;
    end;
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_Cryptorobotics.DoHTTP_POST(const aMethod: String;
  const aBody: String = ''): string;
var
  oHeaders: TStringList;
begin
  DoInitialize;

  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');

    Try
      result := Post(GetBaseURL + aMethod, aBody, oHeaders);
    Except
      On E: Exception do
      begin
        FToken := '';
        raise;
      end;
    end;
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_Cryptorobotics.DoInitialize;
begin
  // ... log
  if Log <> CryptoroboticsOptions.LogOptions.Enabled then
  begin
    LogFileName := CryptoroboticsOptions.LogOptions.FileName;
    Log := CryptoroboticsOptions.LogOptions.Enabled;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.DoCreateSignal(const aSignal
  : TsgcHTTPCryptorobotics_Signal): string;
begin
  result := DoHTTP_POST(Format(CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_CREATE, [FToken]),
    aSignal.AsJSON);
end;

procedure TsgcHTTP_API_Cryptorobotics.DoUserToken;
begin
  if FToken = '' then
  begin
    FToken := GetUserToken;

    if FToken = '' then
      raise Exception.Create(S_CRYPTOROBOTICS_ERROR_USER_TOKEN);
  end;

  FToken := FToken;
end;

function TsgcHTTP_API_Cryptorobotics.GetBaseURL: string;
begin
  result := CS_CRYPTOROBOTICS_ENDPOINT_BASE;
end;

function TsgcHTTP_API_Cryptorobotics.GetChannelList: string;
begin
  DoUserToken;

  Try
    result := DoGetChannelList;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoGetChannelList;
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.GetSignalByHash(const aChannelHash: string;
  aExternalId: Integer): string;
begin
  DoUserToken;

  Try
    result := DoGetSignal(0, aChannelHash, aExternalId);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoGetSignal(0, aChannelHash, aExternalId);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.GetSignalById(aSignalId: Integer): string;
begin
  DoUserToken;

  Try
    result := DoGetSignal(aSignalId, '', 0);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoGetSignal(aSignalId, '', 0);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.GetSignalsList(aChannelId: Integer = 0;
  aLimit: Integer = 10): string;
begin
  DoUserToken;

  Try
    result := DoGetSignalsList(aChannelId, aLimit);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoGetSignalsList(aChannelId, aLimit);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.GetUserToken: string;
var
  vResponse: string;
  oJSON: TsgcJSON;
begin
  result := '';

  vResponse := DoHTTP_GET(Format(CS_CRYPTOROBOTICS_ENDPOINT_USER_TOKEN,
    [CryptoroboticsOptions.Authentication.UserId,
    CryptoroboticsOptions.Authentication.Secret]));

  if LeftStr(vResponse, 1) = '{' then
  begin
    oJSON := TsgcJSON.Create(nil);
    Try
      oJSON.Read(vResponse);
      if oJSON.Node['success'] <> nil then
      begin
        if oJSON.Node['success'].Value = 'true' then
          result := oJSON.Node['token'].Value;
      end;
    Finally
      sgcFree(oJSON);
    End;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.CreateSignal(const aSignal
  : TsgcHTTPCryptorobotics_Signal): string;
begin
  DoUserToken;

  Try
    result := DoCreateSignal(aSignal);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoCreateSignal(aSignal);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.DoCancelSignal(aSignalId: Integer;
    const aChannelHash: string; aExternalId: Integer): string;
var
  oParameters: TStringList;
  vURL: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := '&';
    if aSignalId <> 0 then
      oParameters.Add('id=' + IntToStr(aSignalId))
    else if aChannelHash <> '' then
    begin
      oParameters.Add('external_id=' + aChannelHash);
      oParameters.Add('channel_hash=' + IntToStr(aExternalId));
    end;

    vURL := Format(CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_CANCEL, [FToken]);
    if oParameters.Count > 0 then
      vURL := vURL + '&' + oParameters.DelimitedText;

    result := DoHTTP_POST(vURL, '')
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Cryptorobotics.DoEditSignal(aSignalId: Integer;
  const aChannelHash: string; aExternalId: Integer;
  const aSignal: TsgcHTTPCryptorobotics_Signal): string;
var
  oParameters: TStringList;
  vURL: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := '&';
    if aSignalId <> 0 then
      oParameters.Add('id=' + IntToStr(aSignalId))
    else if aChannelHash <> '' then
    begin
      oParameters.Add('external_id=' + aChannelHash);
      oParameters.Add('channel_hash=' + IntToStr(aExternalId));
    end;

    vURL := Format(CS_CRYPTOROBOTICS_ENDPOINT_SIGNAL_EDIT, [FToken]);
    if oParameters.Count > 0 then
      vURL := vURL + '&' + oParameters.DelimitedText;

    result := DoHTTP_POST(vURL, aSignal.AsJSON(True))
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Cryptorobotics.EditSignalByHash(const aChannelHash
  : string; aExternalId: Integer;
  const aSignal: TsgcHTTPCryptorobotics_Signal): string;
begin
  DoUserToken;

  Try
    result := DoEditSignal(0, aChannelHash, aExternalId, aSignal);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoEditSignal(0, aChannelHash, aExternalId, aSignal);
    end;
    On E: Exception Do
      raise;
  end;
end;

function TsgcHTTP_API_Cryptorobotics.EditSignalById(aSignalId: Integer;
  const aSignal: TsgcHTTPCryptorobotics_Signal): string;
begin
  DoUserToken;

  Try
    result := DoEditSignal(aSignalId, '', 0, aSignal);
  Except
    On E: EIdHTTPProtocolException Do
    begin
      DoUserToken;
      result := DoEditSignal(aSignalId, '', 0, aSignal);
    end;
    On E: Exception Do
      raise;
  end;
end;

procedure TsgcHTTP_API_Cryptorobotics.SetCryptoroboticsOptions
  (const Value: TsgcHTTPCryptorobotics_Options);
begin
  if Assigned(FCryptoroboticsOptions) then
    FCryptoroboticsOptions.Assign(Value);
end;

procedure TsgcHTTPCryptoroboticsLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptoroboticsLog_Options then
  begin
    Enabled := TsgcHTTPCryptoroboticsLog_Options(aSource).Enabled;
    FileName := TsgcHTTPCryptoroboticsLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPCryptorobotics_Options.Create;
begin
  inherited;
  FAuthentication := TsgcHTTPCryptoroboticsAuthentication_Options.Create;
  FLogOptions := TsgcHTTPCryptoroboticsLog_Options.Create;
end;

destructor TsgcHTTPCryptorobotics_Options.Destroy;
begin
  sgcFree(FAuthentication);
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPCryptorobotics_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptorobotics_Options then
  begin
    LogOptions := TsgcHTTPCryptorobotics_Options(aSource).LogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPCryptorobotics_Options.SetAuthentication
  (const Value: TsgcHTTPCryptoroboticsAuthentication_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcHTTPCryptorobotics_Options.SetLogOptions
  (const Value: TsgcHTTPCryptoroboticsLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

procedure TsgcHTTPCryptoroboticsAuthentication_Options.Assign
  (aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptoroboticsAuthentication_Options then
  begin
    UserId := TsgcHTTPCryptoroboticsAuthentication_Options(aSource).UserId;
    Secret := TsgcHTTPCryptoroboticsAuthentication_Options(aSource).Secret;
  end
  else
    inherited Assign(aSource);
end;

destructor TsgcHTTPCryptorobotics_Signal.Destroy;
begin
  sgcFree(FParameters);
  inherited;
end;

function TsgcHTTPCryptorobotics_Signal.AsJSON(aEdit: Boolean = False): string;
var
  oJSON: TsgcJSON;
  oParams, oConfig: IsgcJSON;
begin
  result := '';

  oJSON := TsgcJSON.Create(nil);
  Try
    if not aEdit then
    begin
      if ChannelId <> 0 then
        oJSON.AddPair('channel_id', ChannelId);
      if ChannelHash <> '' then
        oJSON.AddPair('channel_hash', ChannelHash);
    end;

    oParams := oJSON.AddObject('params').JSONObject;
    if not aEdit then
    begin
      oParams.AddPair('side', GetOrderSideStr(Parameters.Side));
      oParams.AddPair('exchange', Parameters.Exchange);
      if Parameters.OptionalExchanges <> '' then
        oParams.AddPair('optional_exchanges', Parameters.OptionalExchanges);
      oParams.AddPair('market', Parameters.Market);
    end;
    oParams.AddPair('min_price', Parameters.MinPrice);
    oParams.AddPair('max_price', Parameters.MaxPrice);
    oParams.AddPair('targets', Parameters.Targets);
    oParams.AddPair('stoploss', Parameters.StopLoss);
    if Parameters.FinishTime > 0 then
      oParams.AddPair('finish_time', FormatDateTime('yyyy-mm-dd hh:nn:ss',
        Parameters.FinishTime));
    if Parameters.TimeZone <> '' then
      oParams.AddPair('time_zone', Parameters.TimeZone);
    if Parameters.Comment <> '' then
      oParams.AddPair('comment', Parameters.Comment);
    oConfig := oParams.AddObject('config').JSONObject;
    if Parameters.Config.Leverage <> 0 then
      oConfig.AddPair('leverage', Parameters.Config.Leverage);
    oConfig.AddPair('breakeven', Parameters.Config.Breakeven);
    if Parameters.Config.StopLossLastCandle <> 0 then
      oConfig.AddPair('sl_last_candle', Parameters.Config.StopLossLastCandle);
    if not aEdit then
    begin
      if Parameters.LastPrice <> 0 then
        oParams.AddPair('last_price', Parameters.LastPrice);
      if Parameters.ExternalId <> 0 then
        oParams.AddPair('external_id', Parameters.ExternalId);
    end;

    result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTPCryptorobotics_Signal.GetParameters
  : TsgcHTTPCryptorobotics_Signal_Parameters;
begin
  if not Assigned(FParameters) then
    FParameters := TsgcHTTPCryptorobotics_Signal_Parameters.Create;
  result := FParameters;
end;

destructor TsgcHTTPCryptorobotics_Signal_Parameters.Destroy;
begin
  sgcFree(FConfig);
  inherited;
end;

function TsgcHTTPCryptorobotics_Signal_Parameters.GetConfig
  : TsgcHTTPCryptorobotics_Signal_Parameters_Config;
begin
  if not Assigned(FConfig) then
    FConfig := TsgcHTTPCryptorobotics_Signal_Parameters_Config.Create;
  result := FConfig;
end;

{$ENDIF}

end.
