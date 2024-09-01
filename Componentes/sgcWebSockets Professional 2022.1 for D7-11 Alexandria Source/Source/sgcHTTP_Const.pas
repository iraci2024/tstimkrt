{***************************************************************************
 sgcHTTP component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcHTTP_Const;

interface

{$I sgcVer.inc}

{$IFDEF SGC_HTTP}

resourcestring
  S_ERROR_CANNOT_OPEN_WEBBROWSER = 'WebBrowser cannot be opened.';
  S_ERROR_AUTHENTICATION_URL_IS_EMPTY = 'Authentication URL must not be empty.';
  S_ERROR_TOKEN_URL_IS_EMPTY = 'Token URL must not be empty.';
  S_ERROR_CLIENT_ID_IS_EMPTY = 'Client ID must not be empty.';
  S_ERROR_HTTP_METHOD_NOT_SUPPORTED_BY_INDY = 'HTTP %s method not supported by current Indy version.';

Const
  CS_HTTP_HEADER_AWS_DATE = 'X-Amz-Date';
  CS_HTTP_HEADER_AWS_CONTENT_TYPE = 'application/x-www-form-urlencoded; charset=utf-8';

Const
  CS_HTTP_OAUTH2_AUTH_URL = '/sgc/oauth2/auth';
  CS_HTTP_OAUTH2_TOKEN_URL = '/sgc/oauth2/token';
  CS_HTTP_OAUTH2_SIGNIN_URL = '/sgc/oauth2/signin';
  CS_HTTP_OAUTH2_SUCCESSFUL_URL = '/sgc/oauth2/successful';

Const
  CS_HTTP_COULD_NOT_LOAD_OPENSSL = 'Could not load SSL library.';

Const
  CS_SGC_HTML_OAUTH2_AUTHORIZATION: String = 'SGC_HTML_OAUTH2_AUTHORIZATION';

{$ENDIF}


implementation

{$IFDEF SGC_HTTP}

{$ENDIF}


end.
