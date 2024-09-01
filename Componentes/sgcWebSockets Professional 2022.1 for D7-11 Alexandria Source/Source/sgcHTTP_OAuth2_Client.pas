{ ***************************************************************************
  sgcHTTP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_OAuth2_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_OAUTH}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomHTTPServer{$ELSE}IdCustomHTTPServer{$ENDIF},
  // sgc
  sgcBase_Classes, sgcHTTP_OAuth_Client, sgcHTTP_Client;

type
  TsgcOnAuth2BeforeAuthorizeCode = procedure(Sender: TObject; var URL: String;
      var Handled: Boolean) of object;
  TsgcOnAuth2AfterAuthorizeCode = procedure(Sender: TObject; const Code, State,
      Scope, RawParams: String; var Handled: Boolean) of object;
  TsgcOnAuth2ErrorAuthorizeCode = procedure(Sender: TObject; const Error,
      Error_Description, Error_URI, State, RawParams: String) of object;

  TsgcOnAuth2BeforeAccessToken = procedure(Sender: TObject; var URL, Parameters:
      String; var Handled: Boolean) of object;
  TsgcOnAuth2AfterAccessToken = procedure(Sender: TObject; const Access_Token,
      Token_Type, Expires_In, Refresh_Token, Scope, RawParams: String; var
      Handled: Boolean) of object;
  TsgcOnAuth2ErrorAccessToken = procedure(Sender: TObject; const Error,
      Error_Description, Error_URI, RawParams: String) of object;

  TsgcOnAuth2BeforeRefreshToken = procedure(Sender: TObject; var URL, Parameters:
      String; var Handled: Boolean) of object;
  TsgcOnAuth2AfterRefreshToken = procedure(Sender: TObject; const Access_Token,
      Token_Type, Expires_In, Refresh_Token, Scope, RawParams: String; var
      Handled: Boolean) of object;
  TsgcOnAuth2ErrorRefreshToken = procedure(Sender: TObject; const Error,
      Error_Description, Error_URI, RawParams: String) of object;

  TsgcOnAuth2HTTPResponse = procedure(Sender: TObject; var Code: Integer; var
      Text: String) of object;

  TsgcOAuth2GrantTypes = (auth2Code, auth2ClientCredentials);

  TsgcHTTPOAuth2_Options = class(TPersistent)
  private
    FClientId: String;
    FClientSecret: String;
  private
    FGrantType: TsgcOAuth2GrantTypes;
    FPassword: String;
    FState: String;
    FUsername: String;
    function GetState: String;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  public
    property State: String read GetState;
  published
    property ClientId: String read FClientId write FClientId;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property Password: String read FPassword write FPassword;
    property Username: String read FUsername write FUsername;
    property GrantType: TsgcOAuth2GrantTypes read FGrantType write FGrantType;
  end;

  TsgcHTTPClient_Options = class(TPersistent)
  private
    FLogOptions: TsgcHttpLogFile;
    FTLSOptions: TsgcIdHTTPTLS_Options;
    procedure SetLogOptions(const Value: TsgcHttpLogFile);
    procedure SetTLSOptions(const Value: TsgcIdHTTPTLS_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property LogOptions: TsgcHttpLogFile read FLogOptions write SetLogOptions;
    property TLSOptions: TsgcIdHTTPTLS_Options read FTLSOptions write SetTLSOptions;
  end;

  TsgcHTTPComponentClient_OAuth2 = class(TsgcHTTPComponentClient_OAuth)
    { from TsgcHTTPComponent_OAuth_Base }
  protected
    procedure OnCommandGetEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;
    { from TsgcHTTPComponent_OAuth_Base }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { timer }
  private
    FTimer: TsgcTimer;
    function GetTimer: TsgcTimer;
    procedure DoStartTimer(aInterval: Integer);
  protected
    procedure OnTimerEvent(Sender: TObject); virtual;
    procedure OnTimerExceptionEvent(Sender: TObject; E: Exception); virtual;
    { timer }

    { methods }
  private
    FRefreshToken: String;
    FExpiresIn: Integer;
  private
    function GetAuthorizationURL: string;
  private
    procedure DoAccessTokenRequest(const aCode: String; out aResponseCode: Integer;
        out aResponseText: String); overload;
    procedure DoAccessTokenRequest; overload;

    procedure DoAccessTokenResponse(const aResponse: String);
    procedure DoAccessTokenError(const aResponse: String = '');
    procedure DoRefreshTokenRequest(const aCode: String);
    procedure DoRefreshTokenResponse(const aResponse: String);
    procedure DoRefreshTokenError(const aResponse: String = '');
  protected
    procedure DoAuthorizeCode; virtual;
  protected
    procedure DoAuthorization; virtual;
  public
    procedure Start; override;
    procedure Refresh(const aRefreshToken: String);
    procedure Stop; override;
    { methods }

    { properties }
  private
    FOAuth2Options: TsgcHTTPOAuth2_Options;
    FHTTPClientOptions: TsgcHTTPClient_Options;
    procedure SetHTTPClientOptions(const Value: TsgcHTTPClient_Options);
    procedure SetOAuth2Options(const Value: TsgcHTTPOAuth2_Options);
  public
    property HTTPClientOptions: TsgcHTTPClient_Options read FHTTPClientOptions write SetHTTPClientOptions;
    property OAuth2Options: TsgcHTTPOAuth2_Options read FOAuth2Options
      write SetOAuth2Options;
    { properties }

    { events }
  private
    FOnAfterAuthorizeCode: TsgcOnAuth2AfterAuthorizeCode;
    FOnBeforeAuthorizeCode: TsgcOnAuth2BeforeAuthorizeCode;
    FOnErrorAuthorizeCode: TsgcOnAuth2ErrorAuthorizeCode;
    FOnBeforeAccessToken: TsgcOnAuth2BeforeAccessToken;
    FOnAfterAccessToken: TsgcOnAuth2AfterAccessToken;
    FOnErrorAccessToken: TsgcOnAuth2ErrorAccessToken;
    FOnBeforeRefreshToken: TsgcOnAuth2BeforeRefreshToken;
    FOnAfterRefreshToken: TsgcOnAuth2AfterRefreshToken;
    FOnErrorRefreshToken: TsgcOnAuth2ErrorRefreshToken;
    FOnHTTPResponse: TsgcOnAuth2HTTPResponse;
  protected
    function DoBeforeAuthorizeCodeEvent(var URL: String): Boolean; virtual;
    function DoAfterAuthorizeCodeEvent(const Code, State, Scope, RawParams:
        String): Boolean; virtual;
    procedure DoErrorAuthorizeCodeEvent(const Error, Error_Description, Error_URI,
        State, RawParams: String);
    function DoBeforeAccessTokenEvent(var URL, Parameters: String): Boolean;
        virtual;
    function DoAfterAccessTokenEvent(const Access_Token, Token_Type, Expires_In,
        Refresh_Token, Scope, RawParams: String): Boolean; virtual;
    procedure DoErrorAccessTokenEvent(const Error, Error_Description, Error_URI,
        RawParams: String);
    function DoBeforeRefreshTokenEvent(var URL, Parameters: String): Boolean;
        virtual;
    function DoAfterRefreshTokenEvent(const Access_Token, Token_Type, Expires_In,
        Refresh_Token, Scope, RawParams: String): Boolean; virtual;
    procedure DoErrorRefreshTokenEvent(const Error, Error_Description, Error_URI,
        RawParams: String); virtual;
    procedure DoHTTPResponseEvent(var Code: Integer; var Text: String); virtual;
  public
    property OnAfterAuthorizeCode: TsgcOnAuth2AfterAuthorizeCode
      read FOnAfterAuthorizeCode write FOnAfterAuthorizeCode;
    property OnBeforeAuthorizeCode: TsgcOnAuth2BeforeAuthorizeCode
      read FOnBeforeAuthorizeCode write FOnBeforeAuthorizeCode;
    property OnErrorAuthorizeCode: TsgcOnAuth2ErrorAuthorizeCode
      read FOnErrorAuthorizeCode write FOnErrorAuthorizeCode;
    property OnAfterAccessToken: TsgcOnAuth2AfterAccessToken
      read FOnAfterAccessToken write FOnAfterAccessToken;
    property OnBeforeAccessToken: TsgcOnAuth2BeforeAccessToken
      read FOnBeforeAccessToken write FOnBeforeAccessToken;
    property OnErrorAccessToken: TsgcOnAuth2ErrorAccessToken
      read FOnErrorAccessToken write FOnErrorAccessToken;
    property OnAfterRefreshToken: TsgcOnAuth2AfterRefreshToken
      read FOnAfterRefreshToken write FOnAfterRefreshToken;
    property OnBeforeRefreshToken: TsgcOnAuth2BeforeRefreshToken
      read FOnBeforeRefreshToken write FOnBeforeRefreshToken;
    property OnErrorRefreshToken: TsgcOnAuth2ErrorRefreshToken
      read FOnErrorRefreshToken write FOnErrorRefreshToken;
    property OnHTTPResponse: TsgcOnAuth2HTTPResponse read FOnHTTPResponse write FOnHTTPResponse;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_OAUTH}

uses
  StrUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdUri{$ELSE}IdUri{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHTTP{$ELSE}IdHTTP{$ENDIF},
  // sgc
  sgcBase_Helpers, sgcJSON, sgcHTTP_Const;

const
  CS_OAUTH2_CODE = 'code';
  CS_OAUTH2_RESPONSE_TYPE = 'response_type';
  CS_OAUTH2_CLIENT_ID = 'client_id';
  CS_OAUTH2_CLIENT_SECRET = 'client_secret';
  CS_OAUTH2_REDIRECT_URI = 'redirect_uri';
  CS_OAUTH2_SCOPE = 'scope';
  CS_OAUTH2_STATE = 'state';
  CS_OAUTH2_ERROR = 'error';
  CS_OAUTH2_ERROR_DESCRIPTION = 'error_description';
  CS_OAUTH2_ERROR_URI = 'error_uri';
  CS_OAUTH2_GRANT_TYPE = 'grant_type';
  CS_OAUTH2_AUTHORIZATION_CODE = 'authorization_code';
  CS_OAUTH2_CLIENT_CREDENTIALS = 'client_credentials';
  CS_OAUTH2_ACCESS_TOKEN = 'access_token';
  CS_OAUTH2_TOKEN_TYPE = 'token_type';
  CS_OAUTH2_EXPIRES_IN = 'expires_in';
  CS_OAUTH2_REFRESH_TOKEN = 'refresh_token';

constructor TsgcHTTPComponentClient_OAuth2.Create(aOwner: TComponent);
begin
  inherited;
  FOAuth2Options := TsgcHTTPOAuth2_Options.Create;
  FHTTPClientOptions := TsgcHTTPClient_Options.Create;
end;

destructor TsgcHTTPComponentClient_OAuth2.Destroy;
begin
  sgcFree(FHTTPClientOptions);
  sgcThreadFree(FTimer);
  sgcFree(FOAuth2Options);
  inherited;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoAccessTokenError(const aResponse: String =
    '');
var
  oJSON: TsgcJSON;
  vError, vErrorDescription, vErrorURI: String;
begin
  vError := '';
  vErrorDescription := '';
  vErrorURI := '';

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aResponse);
    if oJSON.Node[CS_OAUTH2_ERROR] <> nil then
      vError := oJSON.Node[CS_OAUTH2_ERROR].Value;
    if oJSON.Node[CS_OAUTH2_ERROR_DESCRIPTION] <> nil then
      vErrorDescription := oJSON.Node[CS_OAUTH2_ERROR_DESCRIPTION].Value;
    if oJSON.Node[CS_OAUTH2_ERROR_URI] <> nil then
      vErrorURI := oJSON.Node[CS_OAUTH2_ERROR_URI].Value;
  Finally
    sgcFree(oJSON);
  End;

  DoErrorAccessTokenEvent(vError, vErrorDescription, vErrorURI, aResponse);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoAccessTokenRequest(const aCode:
    String; out aResponseCode: Integer; out aResponseText: String);
var
  oClient: TsgcIdHTTP;
  oRequest, oResponse: TStringStream;
  vURL, vParameters: String;
begin
  oClient := TsgcIdHTTP.Create(nil);
  Try
    if OAuth2Options.Username <> '' then
    begin
      oClient.Request.BasicAuthentication := true;
      oClient.Request.Username := OAuth2Options.Username;
      oClient.Request.Password := OAuth2Options.Password;
    end;
    oClient.Request.Accept := 'application/json';
    oClient.Request.ContentType := 'application/x-www-form-urlencoded';
    oClient.TLSOptions := HTTPClientOptions.TLSOptions;
    oClient.Log := HTTPClientOptions.LogOptions.FileName <> '';
    oClient.LogOptions := HTTPClientOptions.LogOptions;
    case OAuth2Options.GrantType of
      auth2Code:
        begin
          vParameters := CS_OAUTH2_GRANT_TYPE + '=' + CS_OAUTH2_AUTHORIZATION_CODE;
          vParameters := vParameters + '&' + CS_OAUTH2_CODE + '=' + aCode;
        end;
      auth2ClientCredentials:
        begin
          vParameters := CS_OAUTH2_GRANT_TYPE + '=' + CS_OAUTH2_CLIENT_CREDENTIALS;
          vParameters := vParameters + '&' + CS_OAUTH2_SCOPE + '='  + AuthorizationServerOptions.GetScope;
        end;
    end;
    if LocalServerOptions.GetURL <> '' then
      vParameters := vParameters + '&' + CS_OAUTH2_REDIRECT_URI + '=' +
        LocalServerOptions.GetURL;
    if OAuth2Options.ClientId <> '' then
      vParameters := vParameters + '&' + CS_OAUTH2_CLIENT_ID + '=' +
        OAuth2Options.ClientId;
    if OAuth2Options.ClientSecret <> '' then
      vParameters := vParameters + '&' + CS_OAUTH2_CLIENT_SECRET + '=' +
        OAuth2Options.ClientSecret;

    vURL := AuthorizationServerOptions.TokenURL;
    if DoBeforeAccessTokenEvent(vURL, vParameters) then
    begin
      oRequest := TStringStream.Create(vParameters);
      Try
        oResponse := TStringStream.Create('');
        Try
          if UpperCase(LeftStr(vURL, 5)) = 'HTTPS'
          then
          begin
            Try
              // ... post
              oClient.Post(vURL, oRequest, oResponse);
              // ... response
              aResponseCode := oClient.ResponseCode;
              if oClient.ResponseCode = 200 then
                aResponseText := 'The operation was successful, you can close your browser now.'
              else
                DoErrorAccessTokenEvent('invalid_request', oResponse.DataString, '', '');
              // ... event
              DoAccessTokenResponse(oResponse.DataString);
            Except
              on E: EIdHTTPProtocolException do
              begin
                aResponseCode := E.ErrorCode;
                aResponseText := E.ErrorMessage;
                if E.ErrorCode = 400 then
                  DoAccessTokenError(E.ErrorMessage)
                else
                  DoErrorAccessTokenEvent('invalid_request', E.ErrorMessage, '', '');
              end;
              on E: Exception do
              begin
                aResponseCode := 500;
                aResponseText := E.Message;
                DoErrorAccessTokenEvent('invalid_request', E.Message, '', '');
              end;
            end;
          end;
        Finally
          sgcFree(oResponse);
        End;
      Finally
        sgcFree(oRequest);
      End;
    end;
  Finally
    sgcFree(oClient);
  End;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoAccessTokenRequest;
var
  vResponseCode: Integer;
  vResponseText: string;
begin
  DoAccessTokenRequest('', vResponseCode, vResponseText);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoRefreshTokenRequest(const aCode: String);
var
  oClient: TsgcIdHTTP;
  oRequest, oResponse: TStringStream;
  vURL, vParameters: String;
begin
  FRefreshToken := aCode;

  oClient := TsgcIdHTTP.Create(nil);
  Try
    if OAuth2Options.Username <> '' then
    begin
      oClient.Request.BasicAuthentication := true;
      oClient.Request.Username := OAuth2Options.Username;
      oClient.Request.Password := OAuth2Options.Password;
    end;
    oClient.Request.Accept := 'application/json';
    oClient.Request.ContentType := 'application/x-www-form-urlencoded';
    oClient.TLSOptions := HTTPClientOptions.TLSOptions;
    oClient.Log := HTTPClientOptions.LogOptions.FileName <> '';
    oClient.LogOptions := HTTPClientOptions.LogOptions;

    vParameters := CS_OAUTH2_GRANT_TYPE + '=' + CS_OAUTH2_REFRESH_TOKEN;
    vParameters := vParameters + '&' + CS_OAUTH2_REFRESH_TOKEN + '=' + aCode;
    vParameters := vParameters + '&' + CS_OAUTH2_SCOPE + '=' +
      AuthorizationServerOptions.GetScope;
    if OAuth2Options.ClientId <> '' then
      vParameters := vParameters + '&' + CS_OAUTH2_CLIENT_ID + '=' +
        OAuth2Options.ClientId;
    if OAuth2Options.ClientSecret <> '' then
      vParameters := vParameters + '&' + CS_OAUTH2_CLIENT_SECRET + '=' +
        OAuth2Options.ClientSecret;


    vURL := AuthorizationServerOptions.TokenURL;
    if DoBeforeRefreshTokenEvent(vURL, vParameters) then
    begin
      oRequest := TStringStream.Create(vParameters);
      Try
        oResponse := TStringStream.Create('');
        Try
          if UpperCase(LeftStr(vURL, 5)) = 'HTTPS'
          then
          begin
            Try
              oClient.Post(vURL, oRequest, oResponse);
            Except
              on E: EIdHTTPProtocolException do
              begin
                if E.ErrorCode = 400 then
                  DoRefreshTokenError(E.ErrorMessage)
                else
                  DoErrorRefreshTokenEvent('invalid_request', E.ErrorMessage, '', '');
              end;
            end;
            if oClient.ResponseCode = 200 then
              DoRefreshTokenResponse(oResponse.DataString);
          end;
        Finally
          sgcFree(oResponse);
        End;
      Finally
        sgcFree(oRequest);
      End;
    end;
  Finally
    sgcFree(oClient);
  End;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoAccessTokenResponse
  (const aResponse: String);
var
  oJSON: TsgcJSON;
  vAccessToken, vExpiresIn, vRefreshToken, vScope, vTokenType: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aResponse);
    if oJSON.Node[CS_OAUTH2_ACCESS_TOKEN] <> nil then
      vAccessToken := oJSON.Node[CS_OAUTH2_ACCESS_TOKEN].Value;
    if oJSON.Node[CS_OAUTH2_TOKEN_TYPE] <> nil then
      vTokenType := oJSON.Node[CS_OAUTH2_TOKEN_TYPE].Value;
    if oJSON.Node[CS_OAUTH2_EXPIRES_IN] <> nil then
      vExpiresIn := oJSON.Node[CS_OAUTH2_EXPIRES_IN].Value;
    if oJSON.Node[CS_OAUTH2_REFRESH_TOKEN] <> nil then
      vRefreshToken := oJSON.Node[CS_OAUTH2_REFRESH_TOKEN].Value;
    if oJSON.Node[CS_OAUTH2_SCOPE] <> nil then
      vScope := oJSON.Node[CS_OAUTH2_SCOPE].Value;
  Finally
    sgcFree(oJSON);
  End;

  FRefreshToken := '';
  if DoAfterAccessTokenEvent(vAccessToken, vTokenType, vExpiresIn, vRefreshToken, vScope, aResponse) then
  begin
    FRefreshToken := vRefreshToken;
    TryStrToInt(vExpiresIn, FExpiresIn);
    if FExpiresIn > 0 then
      DoStartTimer(FExpiresIn + 1);
  end;
end;

function TsgcHTTPComponentClient_OAuth2.DoAfterAccessTokenEvent(const Access_Token,
    Token_Type, Expires_In, Refresh_Token, Scope, RawParams: String): Boolean;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnAfterAccessToken) then
    FOnAfterAccessToken(self, Access_Token, Token_Type, Expires_In, Refresh_Token, Scope, RawParams, vHandled);
  Result := not vHandled;

  if Result then
    DoAuthTokenEvent(Token_Type, Access_Token, RawParams);
end;

function TsgcHTTPComponentClient_OAuth2.DoAfterAuthorizeCodeEvent(const Code, State,
    Scope, RawParams: String): Boolean;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnAfterAuthorizeCode) then
    FOnAfterAuthorizeCode(self, Code, State, Scope, RawParams, vHandled);
  result := not vHandled;
end;

function TsgcHTTPComponentClient_OAuth2.DoAfterRefreshTokenEvent(const Access_Token,
    Token_Type, Expires_In, Refresh_Token, Scope, RawParams: String): Boolean;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnAfterRefreshToken) then
    FOnAfterRefreshToken(self, Access_Token, Token_Type, Expires_In, Refresh_Token, Scope, RawParams, vHandled);
  Result := not vHandled;

  if Result then
    DoAuthTokenEvent(Token_Type, Access_Token, RawParams);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoAuthorizeCode;
var
  vURL: string;
begin
  if AuthorizationServerOptions.AuthURL = '' then
    raise Exception.Create(S_ERROR_AUTHENTICATION_URL_IS_EMPTY);

  if AuthorizationServerOptions.TokenURL = '' then
    raise Exception.Create(S_ERROR_TOKEN_URL_IS_EMPTY);

  if OAuth2Options.ClientId = '' then
    raise Exception.Create(S_ERROR_CLIENT_ID_IS_EMPTY);

  // ... clear state value, to get a new one
  OAuth2Options.FState := '';

  vURL := GetAuthorizationURL;
  if DoBeforeAuthorizeCodeEvent(vURL) then
    if not DoGoToURL(vURL) then
      raise Exception.Create(S_ERROR_CANNOT_OPEN_WEBBROWSER);
end;

function TsgcHTTPComponentClient_OAuth2.DoBeforeAccessTokenEvent(var URL, Parameters:
    String): Boolean;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnBeforeAccessToken) then
    FOnBeforeAccessToken(self, URL, Parameters, vHandled);
  Result := not vHandled;
end;

function TsgcHTTPComponentClient_OAuth2.DoBeforeAuthorizeCodeEvent(var URL: String):
    Boolean;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnBeforeAuthorizeCode) then
    FOnBeforeAuthorizeCode(self, URL, vHandled);
  result := not vHandled;
end;

function TsgcHTTPComponentClient_OAuth2.DoBeforeRefreshTokenEvent(var URL,
    Parameters: String): Boolean;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnBeforeRefreshToken) then
    FOnBeforeRefreshToken(self, URL, Parameters, vHandled);
  Result := not vHandled;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoErrorAccessTokenEvent(const Error,
    Error_Description, Error_URI, RawParams: String);
begin
  if Assigned(FOnErrorAccessToken) then
    FOnErrorAccessToken(self, Error, Error_Description, Error_URI, RawParams);

  DoAuthTokenErrorEvent(Error, Error_Description, RawParams);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoErrorAuthorizeCodeEvent(const Error,
    Error_Description, Error_URI, State, RawParams: String);
begin
  if Assigned(FOnErrorAuthorizeCode) then
    FOnErrorAuthorizeCode(self, Error, Error_Description, Error_URI, State,
      RawParams);

  DoAuthTokenErrorEvent(Error, Error_Description, RawParams);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoErrorRefreshTokenEvent(const Error,
    Error_Description, Error_URI, RawParams: String);
begin
  if Assigned(FOnErrorRefreshToken) then
    FOnErrorRefreshToken(self, Error, Error_Description, Error_URI, RawParams);

  DoAuthTokenErrorEvent(Error, Error_Description, RawParams);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoRefreshTokenError(const aResponse: String
    = '');
var
  oJSON: TsgcJSON;
  vError, vErrorDescription, vErrorURI: String;
begin
  vError := '';
  vErrorDescription := '';
  vErrorURI := '';

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aResponse);
    if oJSON.Node[CS_OAUTH2_ERROR] <> nil then
      vError := oJSON.Node[CS_OAUTH2_ERROR].Value;
    if oJSON.Node[CS_OAUTH2_ERROR_DESCRIPTION] <> nil then
      vErrorDescription := oJSON.Node[CS_OAUTH2_ERROR_DESCRIPTION].Value;
    if oJSON.Node[CS_OAUTH2_ERROR_URI] <> nil then
      vErrorURI := oJSON.Node[CS_OAUTH2_ERROR_URI].Value;
  Finally
    sgcFree(oJSON);
  End;

  DoErrorRefreshTokenEvent(vError, vErrorDescription, vErrorURI, aResponse);
end;

procedure TsgcHTTPComponentClient_OAuth2.DoRefreshTokenResponse(const aResponse:
    String);
var
  oJSON: TsgcJSON;
  vAccessToken, vExpiresIn, vRefreshToken, vScope, vTokenType: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aResponse);
    if oJSON.Node[CS_OAUTH2_ACCESS_TOKEN] <> nil then
      vAccessToken := oJSON.Node[CS_OAUTH2_ACCESS_TOKEN].Value;
    if oJSON.Node[CS_OAUTH2_TOKEN_TYPE] <> nil then
      vTokenType := oJSON.Node[CS_OAUTH2_TOKEN_TYPE].Value;
    if oJSON.Node[CS_OAUTH2_EXPIRES_IN] <> nil then
      vExpiresIn := oJSON.Node[CS_OAUTH2_EXPIRES_IN].Value;
    if oJSON.Node[CS_OAUTH2_REFRESH_TOKEN] <> nil then
      vRefreshToken := oJSON.Node[CS_OAUTH2_REFRESH_TOKEN].Value;
    if oJSON.Node[CS_OAUTH2_SCOPE] <> nil then
      vScope := oJSON.Node[CS_OAUTH2_SCOPE].Value;
  Finally
    sgcFree(oJSON);
  End;

  if DoAfterRefreshTokenEvent(vAccessToken, vTokenType, vExpiresIn, vRefreshToken, vScope, aResponse) then
  begin
    if vRefreshToken <> '' then
      FRefreshToken := vRefreshToken;
    TryStrToInt(vExpiresIn, FExpiresIn);
    if FExpiresIn > 0 then
      DoStartTimer(FExpiresIn);
  end;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoAuthorization;
begin
  inherited;
  FRefreshToken := '';
  case OAuth2Options.GrantType of
    auth2Code:
      begin
        Try
          DoStartServer;
          DoAuthorizeCode;
        Except
          On E: Exception do
          begin
            DoStopServer;
            raise;
          end;
        end;
      end;
    auth2ClientCredentials:
      begin
        Try
          DoStartServer;
          DoAccessTokenRequest;
        Except
          On E: Exception do
          begin
            DoStopServer;
            raise;
          end;
        End;
      end;
  end;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoHTTPResponseEvent(var Code: Integer;
    var Text: String);
begin
  if Assigned(FOnHTTPResponse) then
    FOnHTTPResponse(self, Code, Text);
end;

function TsgcHTTPComponentClient_OAuth2.GetAuthorizationURL: string;
var
  oURI: TIdURI;
  vParams: String;
begin
  vParams := CS_OAUTH2_RESPONSE_TYPE + '=' + CS_OAUTH2_CODE;
  if OAuth2Options.ClientId <> '' then
    vParams := vParams + '&' + CS_OAUTH2_CLIENT_ID + '=' +
      OAuth2Options.ClientId;
  if LocalServerOptions.GetURL <> '' then
    vParams := vParams + '&' + CS_OAUTH2_REDIRECT_URI + '=' +
      LocalServerOptions.GetURL;
  vParams := vParams + '&' + CS_OAUTH2_SCOPE + '=' +
    AuthorizationServerOptions.GetScopeUnEncoded;
  vParams := vParams + '&' + CS_OAUTH2_STATE + '=' + OAuth2Options.State;

  oURI := TIdURI.Create('');
  Try
    result := AuthorizationServerOptions.AuthURL + '?' +
      oURI.ParamsEncode(vParams);
  Finally
    sgcFree(oURI);
  End;
end;

procedure TsgcHTTPComponentClient_OAuth2.DoStartTimer(aInterval: Integer);
begin
  if GetTimer.Enabled then
    GetTimer.Enabled := False;
  GetTimer.Interval := aInterval * 1000;
  GetTimer.Enabled := True;
end;

procedure TsgcHTTPComponentClient_OAuth2.Start;
begin
  DoAuthorization;
end;

function TsgcHTTPComponentClient_OAuth2.GetTimer: TsgcTimer;
begin
  if not Assigned(FTimer) then
  begin
    FTimer := TsgcTimer.Create;
    FTimer.DebugName := 'OAuth2 Timer';
    FTimer.OnTimer := OnTimerEvent;
    FTimer.OnException := OnTimerExceptionEvent;
  end;
  Result := FTimer;
end;

procedure TsgcHTTPComponentClient_OAuth2.OnCommandGetEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  i: Integer;
  vCode, vState, vScope: String;
  vError, vErrorDescription, vErrorURI: String;
  vResponseCode: Integer;
  vResponseText: String;
begin
  inherited;
  vCode := '';
  vState := '';
  vScope := '';
  vError := '';
  vErrorDescription := '';
  vErrorURI := '';

  for i := 0 to ARequestInfo.Params.Count - 1 do
  begin
    if ARequestInfo.Params.Names[i] = CS_OAUTH2_CODE then
      vCode := ARequestInfo.Params.ValueFromIndex[i]
    else if ARequestInfo.Params.Names[i] = CS_OAUTH2_STATE then
      vState := ARequestInfo.Params.ValueFromIndex[i]
    else if ARequestInfo.Params.Names[i] = CS_OAUTH2_SCOPE then
      vScope := ARequestInfo.Params.ValueFromIndex[i]
    else if ARequestInfo.Params.Names[i] = CS_OAUTH2_ERROR then
      vError := ARequestInfo.Params.ValueFromIndex[i]
    else if ARequestInfo.Params.Names[i] = CS_OAUTH2_ERROR_DESCRIPTION then
      vErrorDescription := ARequestInfo.Params.ValueFromIndex[i]
    else if ARequestInfo.Params.Names[i] = CS_OAUTH2_ERROR_URI then
      vErrorURI := ARequestInfo.Params.ValueFromIndex[i];
  end;

  if vState = OAuth2Options.State then
  begin
    if vError <> '' then
    begin
      DoErrorAuthorizeCodeEvent(vError, vErrorDescription, vErrorURI, vState,
        ARequestInfo.Params.Text);
      DoStopServer;
    end
    else if vState <> OAuth2Options.State then
    begin
      DoErrorAuthorizeCodeEvent(vError, Format('State Value %s is different from request %s', [vState, OAuth2Options.State]), vErrorURI, vState,
        ARequestInfo.Params.Text);
      DoStopServer;
    end
    else
    begin
      DoAfterAuthorizeCodeEvent(vCode, vState, vScope, ARequestInfo.Params.Text);
      DoAccessTokenRequest(vCode, vResponseCode, vResponseText);
      // ... response
      DoHTTPResponseEvent(vResponseCode, vResponseText);
      AResponseInfo.ResponseNo := vResponseCode;
      AResponseInfo.ContentText := vResponseText;
//      DoStopServer;
    end;
  end
  else
    AResponseInfo.ResponseNo := 500;
end;

procedure TsgcHTTPComponentClient_OAuth2.OnTimerEvent(Sender: TObject);
begin
  GetTimer.Enabled := False;
  if FRefreshToken <> '' then
    DoRefreshTokenRequest(FRefreshToken);
end;

procedure TsgcHTTPComponentClient_OAuth2.OnTimerExceptionEvent(Sender: TObject; E:
    Exception);
begin
  OnAuthTokenError(self, 'TimerException', E.Message, '');
end;

procedure TsgcHTTPComponentClient_OAuth2.Refresh(const aRefreshToken: String);
begin
  DoRefreshTokenRequest(aRefreshToken);
end;

procedure TsgcHTTPComponentClient_OAuth2.SetHTTPClientOptions(const Value:
    TsgcHTTPClient_Options);
begin
  if Assigned(FHTTPClientOptions) then
    FHTTPClientOptions.Assign(Value);
end;

procedure TsgcHTTPComponentClient_OAuth2.SetOAuth2Options
  (const Value: TsgcHTTPOAuth2_Options);
begin
  if Assigned(FOAuth2Options) then
    FOAuth2Options.Assign(Value);
end;

procedure TsgcHTTPComponentClient_OAuth2.Stop;
begin
  inherited;
  GetTimer.Enabled := False;
end;

constructor TsgcHTTPOAuth2_Options.Create;
begin
  inherited;
  FGrantType := auth2Code;
end;

procedure TsgcHTTPOAuth2_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPOAuth2_Options then
  begin
    ClientId := TsgcHTTPOAuth2_Options(aSource).ClientId;
    ClientSecret := TsgcHTTPOAuth2_Options(aSource).ClientSecret;
    Username := TsgcHTTPOAuth2_Options(aSource).Username;
    Password := TsgcHTTPOAuth2_Options(aSource).Password;
    GrantType := TsgcHTTPOAuth2_Options(aSource).GrantType;
  end
  else
    inherited Assign(aSource);
end;

function TsgcHTTPOAuth2_Options.GetState: String;
begin
  if FState = '' then
    FState := NewGuid;
  result := FState;
end;

constructor TsgcHTTPClient_Options.Create;
begin
  inherited;
  FTLSOptions := TsgcIdHTTPTLS_Options.Create;
  FLogOptions := TsgcHttpLogFile.Create;
end;

destructor TsgcHTTPClient_Options.Destroy;
begin
  sgcFree(FLogOptions);
  sgcFree(FTLSOptions);
  inherited;
end;

procedure TsgcHTTPClient_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPClient_Options then
  begin
    FTLSOptions.Assign(TsgcHTTPClient_Options(aSource).TLSOptions);
    FLogOptions.Assign(TsgcHTTPClient_Options(aSource).LogOptions);
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPClient_Options.SetLogOptions(const Value: TsgcHttpLogFile);
begin
  FLogOptions := Value;
end;

procedure TsgcHTTPClient_Options.SetTLSOptions(const Value:
    TsgcIdHTTPTLS_Options);
begin
  FTLSOptions := Value;
end;

{$ENDIF}

end.
