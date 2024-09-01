{ ***************************************************************************
  sgcHTTP Google component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Google_Cloud;

interface

{$I sgcVer.inc}
{$IFDEF SGC_GOOGLE_CLOUD}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Types, sgcBase_Helpers, sgcTCP_Classes, sgcHTTP_Classes,
  sgcBase_Classes, sgcHTTP_Client, sgcHTTP_OAuth_Client, sgcHTTP_OAuth2_Client,
  sgcHTTP_JWT_Client, sgcHTTP_JWT_Types;

type
  TsgcGoogleCloudAuthentication = (gcaOAuth2, gcaJWT);

  TsgcGoogleCloudLogFile = class(TsgcTCPLogFile)

  end;

  TsgcHTTP_Google_Cloud_Base = class(TsgcComponent_Base)

  end;

  TsgcHTTP_Google_Cloud_OAuth2_Options = class(TPersistent)
  private
    FClientId: String;
    FClientSecret: String;
    FLocalIP: String;
    FLocalPort: Integer;
    FRedirectURL: String;
    FScope: TStringList;
    procedure SetScope(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ClientId: String read FClientId write FClientId;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property LocalIP: String read FLocalIP write FLocalIP;
    property LocalPort: Integer read FLocalPort write FLocalPort;
    property RedirectURL: String read FRedirectURL write FRedirectURL;
    property Scope: TStringList read FScope write SetScope;
  end;

  TsgcHTTP_Google_Cloud_JWT_Options = class(TPersistent)
  private
    FAlgorithm: TsgcHTTP_JWT_Algorithm;
    FAPI_Endpoint: string;
    FClientEmail: string;
    FExpiry: Integer;
    FPrivateKey: TStringList;
    FPrivateKeyId: string;
    FProjectId: string;
    procedure SetPrivateKey(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property ProjectId: string read FProjectId write FProjectId;
  published
    property Algorithm: TsgcHTTP_JWT_Algorithm read FAlgorithm write FAlgorithm;
    property API_Endpoint: string read FAPI_Endpoint write FAPI_Endpoint;
    property ClientEmail: string read FClientEmail write FClientEmail;
    property Expiry: Integer read FExpiry write FExpiry;
    property PrivateKey: TStringList read FPrivateKey write SetPrivateKey;
    property PrivateKeyId: string read FPrivateKeyId write FPrivateKeyId;
  end;

  TsgcHTTP_Google_Cloud_Options = class(TPersistent)
  private
    FAuthentication: TsgcGoogleCloudAuthentication;
    FJWT: TsgcHTTP_Google_Cloud_JWT_Options;
    FOAuth2: TsgcHTTP_Google_Cloud_OAuth2_Options;
    procedure SetJWT(const Value: TsgcHTTP_Google_Cloud_JWT_Options);
    procedure SetOAuth2(const Value: TsgcHTTP_Google_Cloud_OAuth2_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Authentication: TsgcGoogleCloudAuthentication read FAuthentication
      write FAuthentication;
    property JWT: TsgcHTTP_Google_Cloud_JWT_Options read FJWT write SetJWT;
    property OAuth2: TsgcHTTP_Google_Cloud_OAuth2_Options read FOAuth2
      write SetOAuth2;
  end;

  TsgcHTTP_Google_Cloud_Client = class(TsgcHTTP_Google_Cloud_Base)
    { helpers }
  protected
    function DoPathEncode(const aValue: string): string;
  protected
    function AddQueryParameter(const aURL, aName: string; aValue: string)
      : string; overload;
    function AddQueryParameter(const aURL, aName: string; aValue: Integer)
      : string; overload;
    { helpers }

    { http client }
  private
    FHTTPClient: TsgcHTTPClient;
    function GetHTTPClient: TsgcHTTPClient;
  protected
    function DoGetScope: String; virtual; abstract;
    function DoHTTP(const aMethod, aURL: string; const aBody: string = '')
      : String; virtual;
  protected
    procedure OnAuthTokenEvent(Sender: TObject;
      const TokenType, Token, Data: String); virtual;
    procedure OnAuthTokenErrorEvent(Sender: TObject;
      const Error, ErrorDescription, Data: String); virtual;
  protected
    property HTTPClient: TsgcHTTPClient read GetHTTPClient write FHTTPClient;
    { http client }

    { oauth2 }
  private
    FOAuth2: TsgcHTTPComponentClient_OAuth2;
    function GetOAuth2: TsgcHTTPComponentClient_OAuth2;
  protected
    property OAuth2: TsgcHTTPComponentClient_OAuth2 read GetOAuth2
      write FOAuth2;
    { oauth2 }

    { jwt }
  private
    FJWT: TsgcHTTPComponentClient_JWT;
    function GetJWT: TsgcHTTPComponentClient_JWT;
  protected
    property JWT: TsgcHTTPComponentClient_JWT read GetJWT write FJWT;
    { jwt }

    { log file }
  private
    FLogFile: TsgcGoogleCloudLogFile;
    procedure SetLogFile(const Value: TsgcGoogleCloudLogFile);
  public
    property LogFile: TsgcGoogleCloudLogFile read FLogFile write SetLogFile;
    { log file }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { options }
  private
    FGoogleCloudOptions: TsgcHTTP_Google_Cloud_Options;
    procedure SetGoogleCloudOptions(const Value: TsgcHTTP_Google_Cloud_Options);
  public
    property GoogleCloudOptions: TsgcHTTP_Google_Cloud_Options
      read FGoogleCloudOptions write SetGoogleCloudOptions;
    { options }

    { methods }
  private
    function GetPrivateKeyFromString(const aValue: string): string;
  public
    procedure LoadSettingsFromFile(const aFileName: String);
    { methods }

    { events }
  private
    FOnAuthToken: TsgcOnAuthToken;
    FOnAuthTokenError: TsgcOnAuthTokenError;
  public
    function RefreshToken(const aToken: string): boolean;
  public
    property OnAuthToken: TsgcOnAuthToken read FOnAuthToken write FOnAuthToken;
    property OnAuthTokenError: TsgcOnAuthTokenError read FOnAuthTokenError
      write FOnAuthTokenError;
    { events }
  end;

const
  CS_GOOGLE_CLOUD_HTTP_GET = 'GET';
  CS_GOOGLE_CLOUD_HTTP_POST = 'POST';
  CS_GOOGLE_CLOUD_HTTP_PUT = 'PUT';
  CS_GOOGLE_CLOUD_HTTP_DELETE = 'DELETE';
  CS_GOOGLE_CLOUD_HTTP_PATCH = 'PATCH';

{$ENDIF}

implementation

{$IFDEF SGC_GOOGLE_CLOUD}

uses
{$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHTTP{$ELSE}IdHTTP{$ENDIF},
  sgcHTTP_Const, sgcJSON;

const
  CS_GOOGLE_CLOUD_AUTH_URL = 'https://accounts.google.com/o/oauth2/auth';
  CS_GOOGLE_CLOUD_TOKEN_URL = 'https://accounts.google.com/o/oauth2/token';

constructor TsgcHTTP_Google_Cloud_Client.Create(aOwner: TComponent);
begin
  inherited;
  FGoogleCloudOptions := TsgcHTTP_Google_Cloud_Options.Create;
  FLogFile := TsgcGoogleCloudLogFile.Create;
end;

destructor TsgcHTTP_Google_Cloud_Client.Destroy;
begin
  sgcFree(FJWT);
  sgcFree(FLogFile);
  sgcFree(FGoogleCloudOptions);
  sgcFree(FHTTPClient);
  inherited;
end;

function TsgcHTTP_Google_Cloud_Client.AddQueryParameter(const aURL,
  aName: string; aValue: string): string;
begin
  Result := aURL;
  if AnsiPos('?', Result) > 0 then
    Result := Result + '&'
  else
    Result := Result + '?';
  Result := Result + aName + '=' + DoPathEncode(aValue);
end;

function TsgcHTTP_Google_Cloud_Client.AddQueryParameter(const aURL,
  aName: string; aValue: Integer): string;
begin
  Result := AddQueryParameter(aURL, aName, IntToStr(aValue));
end;

function TsgcHTTP_Google_Cloud_Client.DoHTTP(const aMethod, aURL: string;
  const aBody: string = ''): String;
var
  oRequest: TStringStream;
begin
  Result := '';

  Try
    if aMethod = CS_GOOGLE_CLOUD_HTTP_GET then
      Result := HTTPClient.Get(aURL)
    else if aMethod = CS_GOOGLE_CLOUD_HTTP_POST then
    begin
      oRequest := TStringStream.Create(aBody);
      Try
        Result := HTTPClient.Post(aURL, oRequest);
      Finally
        sgcFree(oRequest);
      End;
    end
    else if aMethod = CS_GOOGLE_CLOUD_HTTP_PUT then
    begin
      oRequest := TStringStream.Create(aBody);
      Try
        Result := HTTPClient.Put(aURL, oRequest)
      Finally
        sgcFree(oRequest);
      End;
    end
    else if aMethod = CS_GOOGLE_CLOUD_HTTP_PATCH then
    begin
{$IFDEF INDY10_6_0_5122}
      oRequest := TStringStream.Create(aBody);
      Try
        Result := HTTPClient.Patch(aURL, oRequest)
      Finally
        sgcFree(oRequest);
      End;
{$ELSE}
      raise Exception.CreateFmt(S_ERROR_HTTP_METHOD_NOT_SUPPORTED_BY_INDY,
        ['Patch']);
{$ENDIF}
    end
{$IFDEF INDY10_5_5}
    else if aMethod = CS_GOOGLE_CLOUD_HTTP_DELETE then
{$IFDEF INDY10_6_0_5122}Result := {$ENDIF}HTTPClient.Delete(aURL)
{$ENDIF}
        ;
  Except
    On E: EIdHTTPProtocolException Do
      Result := E.ErrorMessage;
    On E: Exception do
      raise;
  End;
end;

function TsgcHTTP_Google_Cloud_Client.DoPathEncode(const aValue
  : string): string;
begin
  Result := TIdURI.PathEncode(aValue);
end;

function TsgcHTTP_Google_Cloud_Client.GetHTTPClient: TsgcHTTPClient;
begin
  if not Assigned(FHTTPClient) then
  begin
    FHTTPClient := TsgcHTTPClient.Create(nil);
    FHTTPClient.OnAuthToken := OnAuthTokenEvent;
    FHTTPClient.OnAuthTokenError := OnAuthTokenErrorEvent;
{$IFNDEF SGC_HTTP2}
    FHTTPClient.TLSOptions.Version := tls1_2;
{$ENDIF}
  end;
  case GoogleCloudOptions.Authentication of
    gcaOAuth2:
      begin
        FHTTPClient.Authentication.Token.OAuth := OAuth2;
        FHTTPClient.Authentication.Token.JWT := nil;
      end;
    gcaJWT:
      begin
        FHTTPClient.Authentication.Token.JWT := JWT;
        FHTTPClient.Authentication.Token.OAuth := nil;
      end;
  end;
{$IFDEF SGC_HTTP2}
  FHTTPClient.LogFile.Enabled := LogFile.Enabled;
  FHTTPClient.LogFile.FileName := LogFile.FileName;
{$ELSE}
  FHTTPClient.Log := LogFile.Enabled;
  FHTTPClient.LogOptions.FileName := LogFile.FileName;
{$ENDIF}
  Result := FHTTPClient;
end;

function TsgcHTTP_Google_Cloud_Client.GetJWT: TsgcHTTPComponentClient_JWT;
begin
  if not Assigned(FJWT) then
  begin
    FJWT := TsgcHTTPComponentClient_JWT.Create(nil);
    FJWT.JWTOptions.Header.alg := GoogleCloudOptions.JWT.Algorithm;
    FJWT.JWTOptions.Header.kid := GoogleCloudOptions.JWT.PrivateKeyId;
    FJWT.JWTOptions.PayLoad.iss := GoogleCloudOptions.JWT.ClientEmail;
    FJWT.JWTOptions.PayLoad.sub := GoogleCloudOptions.JWT.ClientEmail;
    FJWT.JWTOptions.PayLoad.aud := GoogleCloudOptions.JWT.API_Endpoint;
    FJWT.JWTOptions.RefreshTokenAfter := Trunc(GoogleCloudOptions.JWT.Expiry / 2);
    case FJWT.JWTOptions.Header.Alg of
      jwtHS256, jwtHS384, jwtHS512:
        begin
          FJWT.JWTOptions.Algorithms.HS.Secret := GoogleCloudOptions.JWT.PrivateKey.Text;
        end;
      jwtRS256, jwtRS384, jwtRS512:
        begin
          FJWT.JWTOptions.Algorithms.RS.PrivateKey.Text := GoogleCloudOptions.JWT.PrivateKey.Text;
        end;
      jwtES256, jwtES384, jwtES512:
        begin
          FJWT.JWTOptions.Algorithms.ES.PrivateKey.Text := GoogleCloudOptions.JWT.PrivateKey.Text;
        end;
    end;
  end;
  Result := FJWT;
end;

function TsgcHTTP_Google_Cloud_Client.GetOAuth2: TsgcHTTPComponentClient_OAuth2;
begin
  if not Assigned(FOAuth2) then
  begin
    GoogleCloudOptions.OAuth2.Scope.Text := DoGetScope;

    FOAuth2 := TsgcHTTPComponentClient_OAuth2.Create(nil);
    FOAuth2.AuthorizationServerOptions.AuthURL := CS_GOOGLE_CLOUD_AUTH_URL;
    FOAuth2.AuthorizationServerOptions.TokenURL := CS_GOOGLE_CLOUD_TOKEN_URL;
    FOAuth2.AuthorizationServerOptions.Scope.Text :=
      GoogleCloudOptions.OAuth2.Scope.Text;
    FOAuth2.OAuth2Options.ClientId := GoogleCloudOptions.OAuth2.ClientId;
    FOAuth2.OAuth2Options.ClientSecret :=
      GoogleCloudOptions.OAuth2.ClientSecret;
    FOAuth2.LocalServerOptions.IP := GoogleCloudOptions.OAuth2.LocalIP;
    FOAuth2.LocalServerOptions.Port := GoogleCloudOptions.OAuth2.LocalPort;
    FOAuth2.LocalServerOptions.RedirectURL :=
      GoogleCloudOptions.OAuth2.RedirectURL;
  end;
  Result := FOAuth2;
end;

function TsgcHTTP_Google_Cloud_Client.GetPrivateKeyFromString(const aValue:
    string): string;
begin
  Result := sgcStringReplace(aValue, '\n', #13#10);
end;

procedure TsgcHTTP_Google_Cloud_Client.LoadSettingsFromFile(const aFileName:
    String);
var
  oFile: TStringList;
  oJSON: TsgcJSON;
begin
  oFile := TStringList.Create;
  Try
    oFile.LoadFromFile(aFileName);
    if oFile.Count > 0 then
    begin
      oJSON := TsgcJSON.Create(nil);
      Try
        oJSON.Read(oFile.Text);

        if oJSON.Node['project_id'] <> nil then
          GoogleCloudOptions.JWT.ProjectId := oJSON.Node['project_id'].Value;
        if oJSON.Node['client_email'] <> nil then
          GoogleCloudOptions.JWT.ClientEmail := oJSON.Node['client_email'].Value;
        if oJSON.Node['private_key_id'] <> nil then
          GoogleCloudOptions.JWT.PrivateKeyId := oJSON.Node['private_key_id'].Value;
        if oJSON.Node['private_key'] <> nil then
          GoogleCloudOptions.JWT.PrivateKey.Text := GetPrivateKeyFromString(oJSON.Node['private_key'].Value);
      Finally
        sgcFree(oJSON);
      end;
    end;
  Finally
    sgcFree(oFile);
  End;
end;

procedure TsgcHTTP_Google_Cloud_Client.OnAuthTokenErrorEvent(Sender: TObject;
  const Error, ErrorDescription, Data: String);
begin
  if Assigned(FOnAuthTokenError) then
    FOnAuthTokenError(self, Error, ErrorDescription, Data);
end;

procedure TsgcHTTP_Google_Cloud_Client.OnAuthTokenEvent(Sender: TObject;
  const TokenType, Token, Data: String);
begin
  if Assigned(FOnAuthToken) then
    FOnAuthToken(self, TokenType, Token, Data);
end;

function TsgcHTTP_Google_Cloud_Client.RefreshToken(const aToken
  : string): boolean;
begin
  Result := HTTPClient.RefreshToken(aToken);
end;

procedure TsgcHTTP_Google_Cloud_Client.SetGoogleCloudOptions
  (const Value: TsgcHTTP_Google_Cloud_Options);
begin
  if Assigned(FGoogleCloudOptions) then
    FGoogleCloudOptions.Assign(Value);
end;

procedure TsgcHTTP_Google_Cloud_Client.SetLogFile
  (const Value: TsgcGoogleCloudLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

constructor TsgcHTTP_Google_Cloud_OAuth2_Options.Create;
begin
  inherited;
  LocalIP := '127.0.0.1';
  LocalPort := 8080;
  RedirectURL := '';
  FScope := TStringList.Create;
end;

destructor TsgcHTTP_Google_Cloud_OAuth2_Options.Destroy;
begin
  sgcFree(FScope);
  inherited;
end;

procedure TsgcHTTP_Google_Cloud_OAuth2_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_Google_Cloud_OAuth2_Options then
  begin
    ClientId := TsgcHTTP_Google_Cloud_OAuth2_Options(aSource).ClientId;
    ClientSecret := TsgcHTTP_Google_Cloud_OAuth2_Options(aSource).ClientSecret;
    Scope.Text := TsgcHTTP_Google_Cloud_OAuth2_Options(aSource).Scope.Text;
    LocalIP := TsgcHTTP_Google_Cloud_OAuth2_Options(aSource).LocalIP;
    LocalPort := TsgcHTTP_Google_Cloud_OAuth2_Options(aSource).LocalPort;
    RedirectURL := TsgcHTTP_Google_Cloud_OAuth2_Options(aSource).RedirectURL;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_Google_Cloud_OAuth2_Options.SetScope
  (const Value: TStringList);
begin
  if Assigned(FScope) then
    FScope.Assign(Value);
end;

constructor TsgcHTTP_Google_Cloud_Options.Create;
begin
  inherited;
  FAuthentication := gcaOAuth2;
  FOAuth2 := TsgcHTTP_Google_Cloud_OAuth2_Options.Create;
  FJWT := TsgcHTTP_Google_Cloud_JWT_Options.Create;
end;

destructor TsgcHTTP_Google_Cloud_Options.Destroy;
begin
  sgcFree(FJWT);
  sgcFree(FOAuth2);
  inherited;
end;

procedure TsgcHTTP_Google_Cloud_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_Google_Cloud_Options then
  begin
    FAuthentication := TsgcHTTP_Google_Cloud_Options(aSource).Authentication;
    OAuth2.Assign(TsgcHTTP_Google_Cloud_Options(aSource).OAuth2);
    JWT.Assign(TsgcHTTP_Google_Cloud_Options(aSource).JWT);
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_Google_Cloud_Options.SetJWT(const Value
  : TsgcHTTP_Google_Cloud_JWT_Options);
begin
  if Assigned(FJWT) then
    FJWT.Assign(Value);
end;

procedure TsgcHTTP_Google_Cloud_Options.SetOAuth2
  (const Value: TsgcHTTP_Google_Cloud_OAuth2_Options);
begin
  if Assigned(FOAuth2) then
    FOAuth2.Assign(Value);
end;

constructor TsgcHTTP_Google_Cloud_JWT_Options.Create;
begin
  inherited;
  Algorithm := jwtRS256;
  FPrivateKey := TStringList.Create;
  FExpiry := 3600;
end;

destructor TsgcHTTP_Google_Cloud_JWT_Options.Destroy;
begin
  sgcFree(FPrivateKey);
  inherited;
end;

procedure TsgcHTTP_Google_Cloud_JWT_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_Google_Cloud_JWT_Options then
  begin
    Algorithm := TsgcHTTP_Google_Cloud_JWT_Options(aSource).Algorithm;
    API_Endpoint := TsgcHTTP_Google_Cloud_JWT_Options(aSource).API_Endpoint;
    ClientEmail := TsgcHTTP_Google_Cloud_JWT_Options(aSource).ClientEmail;
    PrivateKeyId := TsgcHTTP_Google_Cloud_JWT_Options(aSource).PrivateKeyId;
    Expiry := TsgcHTTP_Google_Cloud_JWT_Options(aSource).Expiry;
    PrivateKey.Text := TsgcHTTP_Google_Cloud_JWT_Options(aSource).PrivateKey.Text;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_Google_Cloud_JWT_Options.SetPrivateKey(const Value:
    TStringList);
begin
  if Assigned(FPrivateKey) then
    FPrivateKey.Assign(Value);
end;

{$ENDIF}

end.
