{ ***************************************************************************
  sgcHTTP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }
unit sgcHTTP;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes
  // sgc
    , sgcHTTP_Client
{$IFDEF SGC_OAUTH}, sgcHTTP_OAuth2_Client{$ENDIF}
{$IFDEF SGC_OAUTH_SERVER}, sgcHTTP_OAuth2_Server{$ENDIF}
{$IFDEF SGC_JWT}, sgcHTTP_JWT_Client{$ENDIF}
{$IFDEF SGC_JWT_SERVER}, sgcHTTP_JWT_Server{$ENDIF}
{$IFDEF SGC_AWS}, sgcHTTP_Amazon_AWS_Signature, sgcHTTP_Amazon_SQS{$ENDIF}
{$IFDEF SGC_GOOGLE_CLOUD}, sgcHTTP_Google_PubSub,
  sgcHTTP_Google_Calendar{$ENDIF}
{$IFDEF SGC_EDT_ENT}
{$IFDEF SGC_HTTP2}, sgcHTTP2_Client {$ENDIF}
{$ENDIF}
    ;

type
{$IFDEF SGC_OAUTH}
  TsgcHTTP_OAuth2_Client = class(TsgcHTTPComponentClient_OAuth2)
  published
    property LocalServerOptions;
    property AuthorizationServerOptions;
    property OAuth2Options;
    property HTTPClientOptions;
  published
    property OnBeforeAuthorizeCode;
    property OnAfterAuthorizeCode;
    property OnErrorAuthorizeCode;
    property OnBeforeAccessToken;
    property OnAfterAccessToken;
    property OnErrorAccessToken;
    property OnBeforeRefreshToken;
    property OnAfterRefreshToken;
    property OnErrorRefreshToken;
    property OnHTTPResponse;
  published
    property Version;
  end;

{$IFDEF SGC_OAUTH_SERVER}
  TsgcHTTP_OAuth2_Server = class(TsgcHTTPComponentServer_OAuth2)
  published
    property OAuth2Options;
    property OnOAuth2BeforeRequest;
    property OnOAuth2BeforeDispatchPage;
    property OnOAuth2Authentication;
    property OnOAuth2AfterAccessToken;
    property OnOAuth2AfterRefreshToken;
    property OnOAuth2AfterValidateAccessToken;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}

{$IFDEF SGC_JWT}
  TsgcHTTP_JWT_Client = class(TsgcHTTPComponentClient_JWT)
  published
    property JWTOptions;
  published
    property Version;
  end;

{$IFDEF SGC_JWT_SERVER}
  TsgcHTTP_JWT_Server = class(TsgcHTTPComponentServer_JWT)
  published
    property JWTOptions;
  published
    property OnJWTBeforeRequest;
    property OnJWTAfterValidateToken;
    property OnJWTBeforeValidateToken;
    property OnJWTBeforeValidateSignature;
    property OnJWTException;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}

{$IFDEF SGC_AWS}
  TsgcHTTPAWS_SQS_Client = class(TsgcHTTP_Amazon_AWS_SQS_Client)
  published
    property AWSOptions;
    property LogFile;
  published
    property OnSQSBeforeRequest;
    property OnSQSResponse;
    property OnSQSError;
  published
    property Version;
  end;
{$ENDIF}

{$IFDEF SGC_GOOGLE_CLOUD}
  TsgcHTTPGoogleCloud_PubSub_Client = class(TsgcHTTP_Google_Cloud_PubSub_Client)
  published
    property GoogleCloudOptions;
    property LogFile;
  published
    property OnAuthToken;
    property OnAuthTokenError;
  published
    property Version;
  end;

  TsgcHTTPGoogleCloud_Calendar_Client = class
    (TsgcHTTP_Google_Cloud_Calendar_Client)
  published
    property GoogleCloudOptions;
    property LogFile;
    property Scopes;
  published
    property OnAuthToken;
    property OnAuthTokenError;
    property OnGetCalendar;
    property OnGetCalendarEvent;
    property OnError;
  published
    property Version;
  end;
{$ENDIF}

  TsgcHTTP1Client = class(TsgcIdHTTP)
  published
    property Authentication;
    property Log;
    property LogOptions;
    property TLSOptions;
  published
    property Version;
  end;

{$IFDEF SGC_EDT_ENT}
{$IFDEF SGC_HTTP2}
  TsgcHTTP2Client = class(TsgcHTTP2CLient_Base)
  published
    property Active;
    property Host;
    property Port;
  published
    property ConnectTimeout;
    property ReadTimeout;
    property TLS;
    property Proxy;
    property IPVersion;
    property NotifyEvents;
  published
    property OnHTTP2Connect;
    property OnHTTP2GoAway;
    property OnHTTP2RSTStream;
    property OnHTTP2Response;
    property OnHTTP2ResponseFragment;
    property OnHTTP2PushPromise;
    property OnHTTP2Disconnect;
    property OnHTTP2Exception;
    property OnHTTP2PendingRequests;
    property OnHTTP2Authorization;
    property OnHTTP2BeforeRequest;
    property OnSSLGetHandler;
    property OnSSLAfterCreateHandler;
  published
    property LogFile;
    property HeartBeat;
    property WatchDog;
    property Throttle;
    property TLSOptions;
    property Request;
    property Settings;
    property Authentication;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

end.
