{ ***************************************************************************
  sgcHTTP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY} sgcIdSSLOpenSSL {$ELSE} IdSSLOpenSSL {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSLOpenSSLHeaders {$ELSE} IdSSLOpenSSLHeaders {$ENDIF},
{$IFDEF SGC_INDY} sgcIdHTTP {$ELSE} IdHTTP {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSL {$ELSE} IdSSL {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandlerStack {$ELSE} IdIOHandlerStack {$ENDIF},
{$IFDEF SGC_INDY} sgcIdGlobal {$ELSE} IdGlobal {$ENDIF},
{$IFDEF SGC_INDY} sgcIdConnectThroughHttpProxy
{$ELSE} IdConnectThroughHttpProxy {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSocks {$ELSE} IdSocks {$ENDIF},
  // sgcHTTP2
{$IFDEF SGC_HTTP2} sgcHTTP2_Client, {$ENDIF}
  // sgc
  sgcHTTP_Classes, sgcHTTP_OAuth_Client, sgcHTTP_JWT_Client, sgcWebSocket_Types,
  sgcTCP_Classes, sgcSocket_Classes_Indy;

type

  TsgcHTTPAuthentication_Token = class(TPersistent)
  private
    FOAuth: TsgcHTTPComponentClient_OAuth;
    FJWT: TsgcHTTPComponentClient_JWT;
    FReadTimeout: Integer;
  public
    constructor Create; virtual;
  published
    property OAuth: TsgcHTTPComponentClient_OAuth read FOAuth write FOAuth;
    property JWT: TsgcHTTPComponentClient_JWT read FJWT write FJWT;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
  end;

  TsgcIdHTTPAuthentication_Options = class(TPersistent)
  private
    FToken: TsgcHTTPAuthentication_Token;
    procedure SetToken(const Value: TsgcHTTPAuthentication_Token);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Token: TsgcHTTPAuthentication_Token read FToken write SetToken;
  end;

  TsgcIdHTTPTLS_Options = class(TsgcTCPTLS_Options)
  public
    procedure Assign(aSource: TPersistent); override;
  end;

  TsgcHttpLogFile = class(TPersistent)
  private
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property FileName: String read FFileName write FFileName;
  end;

  TsgcIdHTTP = class(TIdHTTP)
    { from TIdCustomHTTP }
  protected
    procedure InitComponent; override;
{$IFNDEF MACOS32}
  protected
    procedure DoRequest(const AMethod: TIdHTTPMethod; AURL: string;
      aSource, AResponseContent: TStream; AIgnoreReplies: array of
{$IFNDEF D2009} SmallInt {$ELSE} Int16 {$ENDIF}); override;
    { from TIdCustomHTTP }

    { methods }
  private
    FTLSHandler: TIdSSLIOHandlerSocketBase;
    procedure DoTLS;
    procedure DoNoTLS;
  private
    procedure DoProxy(const aTLS: Boolean);
    procedure DoNoProxy;
  private
    FLogIntercept: TsgcIdLogFileClient;
    procedure DoLog;
    procedure DoNoLog;
{$ENDIF}
  protected
    procedure OnGetPasswordEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE} AnsiString {$ENDIF}); virtual;
    { methods }

    { constructor / destructor }
  public
    destructor Destroy; override;
    { constructor / destructor }

    { log file }
  private
    FLog: Boolean;
    FLogOptions: TsgcHttpLogFile;
    procedure SetLog(const Value: Boolean);
    procedure SetLogOptions(const Value: TsgcHttpLogFile);
  public
    property Log: Boolean read FLog write SetLog;
    property LogOptions: TsgcHttpLogFile read FLogOptions write SetLogOptions;
    { log file }

    { ssl }
  private
    FTLSOptions: TsgcIdHTTPTLS_Options;
    procedure SetTLSOptions(const Value: TsgcIdHTTPTLS_Options);
  public
    property TLSOptions: TsgcIdHTTPTLS_Options read FTLSOptions
      write SetTLSOptions;
    { ssl }

    { authentication }
  private
    FAuthToken: String;
    FAuthTokenType: String;
    FAuthTokenExpiry: TDateTime;
    FAuthentication: TsgcIdHTTPAuthentication_Options;
    procedure SetAuthentication(const Value: TsgcIdHTTPAuthentication_Options);
  protected
    procedure OnAuthTokenEvent(Sender: TObject;
      const TokenType, Token, Data: String); virtual;
    procedure OnAuthTokenErrorEvent(Sender: TObject;
      const Error, ErrorDescription, Data: String); virtual;
  protected
    procedure DoAuthentication; virtual;
  public
    property Authentication: TsgcIdHTTPAuthentication_Options
      read FAuthentication write SetAuthentication;
    { authentication }

    { proxy }
  private
    FProxy: TsgcHTTPProxy_Options;
    procedure SetProxy(const Value: TsgcHTTPProxy_Options);
{$IFNDEF MACOS32}
  private
    FProxyIOHandler: TIdIOHandlerStack;
    FProxyHTTP: TIdConnectThroughHttpProxy;
    FProxySocks: TIdSocksInfo;
  protected
    function GetProxyIOHandler(aTLS: Boolean): TIdIOHandlerStack;
    function GetProxyHTTP: TIdConnectThroughHttpProxy;
    function GetProxySocks: TIdSocksInfo;
{$ENDIF}
  public
    property Proxy: TsgcHTTPProxy_Options read FProxy write SetProxy;
    { proxy }

    { version }
  private
    function GetVersion: String;
  public
    property Version: String read GetVersion;
    { version }
  end;
{$IFDEF SGC_HTTP2}

  TsgcHTTPClient = class(TsgcHTTP2CLient_Base)
{$ELSE}
  TsgcHTTPClient = class(TsgcIdHTTP)
{$ENDIF}
    { authentication }
  private
    FOnAuthToken: TsgcOnAuthToken;
    FOnAuthTokenError: TsgcOnAuthTokenError;
  protected
    procedure OnAuthTokenEvent(Sender: TObject;
      const TokenType, Token, Data: String); override;
    procedure OnAuthTokenErrorEvent(Sender: TObject;
      const Error, ErrorDescription, Data: String); override;
  public
    function RefreshToken(const aToken: string): Boolean;
  public
    property OnAuthToken: TsgcOnAuthToken read FOnAuthToken write FOnAuthToken;
    property OnAuthTokenError: TsgcOnAuthTokenError read FOnAuthTokenError
      write FOnAuthTokenError;
    { authentication }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  StrUtils, DateUtils,
  // sgc
{$IFDEF SGC_SCHANNEL} sgcSSL_SChannel_Indy, {$ENDIF}
  sgcBase_Helpers, sgcBase_Const, sgcTCP_Client, sgcHTTP_OAuth2_Client;

procedure TsgcIdHTTPTLS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcIdHTTPTLS_Options then
  begin
    CertFile := TsgcIdHTTPTLS_Options(aSource).CertFile;
    KeyFile := TsgcIdHTTPTLS_Options(aSource).KeyFile;
    Password := TsgcIdHTTPTLS_Options(aSource).Password;
    RootCertFile := TsgcIdHTTPTLS_Options(aSource).RootCertFile;
    Version := TsgcIdHTTPTLS_Options(aSource).Version;
    VerifyCertificate := TsgcIdHTTPTLS_Options(aSource).VerifyCertificate;
    IOHandler := TsgcIdHTTPTLS_Options(aSource).IOHandler;
    ALPNProtocols := TsgcIdHTTPTLS_Options(aSource).ALPNProtocols;
    OpenSSL_Options := TsgcIdHTTPTLS_Options(aSource).OpenSSL_Options;
    SChannel_Options := TsgcIdHTTPTLS_Options(aSource).SChannel_Options;
  end
  else if aSource is TsgcTCPTLS_Options then
  begin
    CertFile := TsgcTCPTLS_Options(aSource).CertFile;
    KeyFile := TsgcTCPTLS_Options(aSource).KeyFile;
    Password := TsgcTCPTLS_Options(aSource).Password;
    RootCertFile := TsgcTCPTLS_Options(aSource).RootCertFile;
    Version := TsgcTCPTLS_Options(aSource).Version;
    VerifyCertificate := TsgcTCPTLS_Options(aSource).VerifyCertificate;
    IOHandler := TsgcTCPTLS_Options(aSource).IOHandler;
    ALPNProtocols := TsgcTCPTLS_Options(aSource).ALPNProtocols;
    OpenSSL_Options := TsgcTCPTLS_Options(aSource).OpenSSL_Options;
    SChannel_Options := TsgcTCPTLS_Options(aSource).SChannel_Options;
  end
  else
    inherited Assign(aSource);
end;

destructor TsgcIdHTTP.Destroy;
begin
{$IFNDEF MACOS32}
  if Log then
    DoNoLog;
{$ENDIF}
  sgcFree(FAuthentication);
  sgcFree(FLogOptions);
  sgcFree(FTLSOptions);
  sgcFree(FProxy);
  inherited;
end;

procedure TsgcIdHTTP.DoAuthentication;
var
  vStart: cardinal;
begin
  if Assigned(Authentication.Token.OAuth) or Assigned(Authentication.Token.JWT)
  then
  begin
    if Assigned(Authentication.Token.JWT) then
    begin
      if Now > FAuthTokenExpiry then
        FAuthToken := '';
    end;

    if FAuthToken = '' then
    begin
      if Assigned(Authentication.Token.OAuth) then
      begin
        Authentication.Token.OAuth.OnAuthToken := OnAuthTokenEvent;
        Authentication.Token.OAuth.OnAuthTokenError := OnAuthTokenErrorEvent;
        Authentication.Token.OAuth.Start;
      end
      else if Assigned(Authentication.Token.JWT) then
      begin
        Authentication.Token.JWT.OnAuthToken := OnAuthTokenEvent;
        Authentication.Token.JWT.OnAuthTokenError := OnAuthTokenErrorEvent;
        Authentication.Token.JWT.Start;
      end;
      // ... wait connect
      vStart := sgcGetTicks;
      repeat
        if FAuthToken <> '' then
          break;
        sleep(1);
      until sgcGetTickDiff(vStart, sgcGetTicks) >=
        cardinal(Authentication.Token.ReadTimeout);

    end;
    if FAuthToken <> '' then
    begin
      if Request.CustomHeaders.IndexOf('Authorization: ' + FAuthTokenType + ' '
        + FAuthToken) = -1 then
        Request.CustomHeaders.Add('Authorization: ' + FAuthTokenType + ' ' +
          FAuthToken);
    end;
  end;
end;
{$IFNDEF MACOS32}

procedure TsgcIdHTTP.DoRequest(const AMethod: TIdHTTPMethod; AURL: string;
  aSource, AResponseContent: TStream; AIgnoreReplies: array of
{$IFNDEF D2009} SmallInt {$ELSE} Int16 {$ENDIF});
begin
  // ... authentication
  DoAuthentication;

  // ... log
  if FLog then
    DoLog
  else
    DoNoLog;

  // ... tls
  if LeftStr(UpperCase(AURL), 5) = 'HTTPS' then
  begin
    DoTLS;
    if Proxy.Enabled then
      DoProxy(True);
  end
  else
  begin
    DoNoTLS;
    if Proxy.Enabled then
      DoProxy(False)
    else
      DoNoProxy;
  end;

  inherited DoRequest(AMethod, AURL, aSource, AResponseContent, AIgnoreReplies);
end;

procedure TsgcIdHTTP.DoTLS;
begin
  if not Assigned(FTLSHandler) then
  begin
{$IFDEF SGC_SCHANNEL}
    if TLSOptions.IOHandler = iohSChannel then
    begin
      FTLSHandler := TsgcIdSSLIOHandlerSocketSChannel.Create(self);
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).TLSVersion :=
        TLSOptions.Version;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler)
        .Certificate.VerifyCertificate := TLSOptions.VerifyCertificate;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).Certificate.CertHash :=
        TLSOptions.SChannel_Options.CertHash;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).Certificate.CertStorePath :=
        TLSOptions.SChannel_Options.CertStorePath;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).Certificate.CertStoreName :=
        TLSOptions.SChannel_Options.CertStoreName;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).CipherList :=
        TLSOptions.SChannel_Options.CipherList;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).Certificate.CertFile :=
        TLSOptions.CertFile;
      TsgcIdSSLIOHandlerSocketSChannel(FTLSHandler).Certificate.CertFilePassword
        := String(TLSOptions.Password);
    end
    else
{$ENDIF}
    begin
      FTLSHandler := TIdSSLIOHandlerSocketOpenSSL.Create(self);
      FTLSHandler.PassThrough := False;
{$IFDEF SGC_INDY_LIB}
      case TLSOptions.OpenSSL_Options.APIVersion of
        oslAPI_1_0:
          OPENSSL_API_VERSION := opSSL_1_0;
        oslAPI_1_1:
          OPENSSL_API_VERSION := opSSL_1_1;
        oslAPI_3_0:
          OPENSSL_API_VERSION := opSSL_3_0;
      end;
{$ENDIF}
      case TLSOptions.OpenSSL_Options.LibPath of
        oslpNone:
          ;
        oslpDefaultFolder:
          sgcIdOpenSSLSetLibPath(sgcGetOpenSSLDefaultFolder);
        oslpCustomPath:
          sgcIdOpenSSLSetLibPath(TLSOptions.OpenSSL_Options.LibPathCustom);
      end;
      {$IFDEF SGC_UNIX_SYMLINKS}
      case TLSOptions.OpenSSL_Options.UnixSymLinks of
        oslsSymLinksDefault:
          begin
            {$IFDEF OSX64}
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(False);
            {$ENDIF}
          end;
        oslsSymLinksLoadFirst:
          begin
            IdOpenSSLSetLoadSymLinksFirst(True);
            IdOpenSSLSetCanLoadSymLinks(True);
          end;
        oslsSymLinksLoad:
          begin
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(True);
          end;
        oslsSymLinksDontLoad:
          begin
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(False);
          end;
      end;
      {$ENDIF}
      case TLSOptions.Version of
        tls1_0:
          TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.Method :=
            sslvTLSv1;
{$IFDEF INDY10_6}
        tls1_1:
          TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.Method :=
            sslvTLSv1_1;
        tls1_2:
          TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.Method :=
            sslvTLSv1_2;
        tls1_3:
          TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.Method :=
{$IFDEF SGC_INDY_LIB} sslvTLSv1_3 {$ELSE} sslvTLSv1_2 {$ENDIF};
{$ENDIF}
      else
{$IFDEF INDY10_5_8}
        TsgcIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.SSLVersions :=
          [sslvTLSv1 {$IFDEF INDY10_6}, sslvTLSv1_1, sslvTLSv1_2
{$IFDEF SGC_INDY_LIB}, sslvTLSv1_3 {$ENDIF}{$ENDIF}];
{$ELSE}
        TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.Method :=
          sslvTLSv1;
{$ENDIF}
      end;
{$IFDEF SGC_INDY_LIB}
      TsgcIdSSLIOHandlerSocketOpenSSL(FTLSHandler).ALPNProtocols.Assign
        (TLSOptions.ALPNProtocols);
{$ENDIF}
      TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.Mode := sslmClient;
      TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.RootCertFile :=
        TLSOptions.RootCertFile;
      TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.CertFile :=
        TLSOptions.CertFile;
      TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.KeyFile :=
        TLSOptions.KeyFile;
      if TLSOptions.VerifyCertificate then
        TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.VerifyMode :=
          [sslvrfPeer]
      else
        TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).SSLOptions.VerifyMode := [];
      TIdSSLIOHandlerSocketOpenSSL(FTLSHandler).OnGetPassword :=
        OnGetPasswordEvent;
    end;

    IOHandler := FTLSHandler;
  end;
end;

procedure TsgcIdHTTP.DoNoTLS;
begin
  if Assigned(IOHandler) then
  begin
{$IFNDEF NEXTGEN}
    IOHandler.Free;
{$ENDIF}
    IOHandler := nil;
  end;
end;

procedure TsgcIdHTTP.DoLog;
begin
  if not Assigned(FLogIntercept) then
  begin
    FLogIntercept := TsgcIdLogFileClient.Create(nil);
    FLogIntercept.FileName := LogOptions.FileName;
    FLogIntercept.ReplaceCRLF := False;
    FLogIntercept.Active := True;
{$IFNDEF INDY10_2}
    if Assigned(Socket) then
      Socket.Intercept := FLogIntercept;
{$ELSE}
    Intercept := FLogIntercept;
{$ENDIF}
  end;
end;

procedure TsgcIdHTTP.DoNoLog;
begin
{$IFNDEF INDY10_2}
  if Assigned(Socket) then
  begin
    if Assigned(Socket.Intercept) then
    begin
{$IFNDEF NEXTGEN}
      Socket.Intercept.Free;
{$ENDIF}
      Socket.Intercept := nil;
    end;
  end;
{$ELSE}
  if Assigned(Intercept) then
  begin
{$IFNDEF NEXTGEN}
    Intercept.Free;
{$ENDIF}
    Intercept := nil;
  end;
{$ENDIF}
end;

procedure TsgcIdHTTP.DoNoProxy;
begin
  if Assigned(IOHandler) then
  begin
{$IFNDEF NEXTGEN}
    IOHandler.Free;
{$ENDIF}
    IOHandler := nil;
  end;
end;

procedure TsgcIdHTTP.DoProxy(const aTLS: Boolean);
begin
  if Proxy.Enabled then
  begin
    IOHandler := GetProxyIOHandler(aTLS);
    case Proxy.ProxyType of
      pxyHTTP:
        begin
          TIdIOHandlerStack(IOHandler).TransparentProxy := GetProxyHTTP;
          sgcFree(FProxySocks);
        end;
      pxySocks4, pxySocks4A, pxySocks5:
        begin
          TIdIOHandlerStack(IOHandler).TransparentProxy := GetProxySocks;
          sgcFree(FProxyHTTP);
        end;
    end;
    // ... set the host of the server to connect, not the proxy
    if aTLS and TLSOptions.VerifyCertificate then
    begin
      if Assigned(FTLSHandler) then
        FTLSHandler.URIToCheck := 'http://' + Host;
    end;
  end;
end;

function TsgcIdHTTP.GetProxyHTTP: TIdConnectThroughHttpProxy;
begin
  if not Assigned(FProxyHTTP) then
    FProxyHTTP := TIdConnectThroughHttpProxy.Create(self);
  FProxyHTTP.Host := Proxy.Host;
  FProxyHTTP.Port := Proxy.Port;
  FProxyHTTP.Username := Proxy.Username;
  FProxyHTTP.Password := Proxy.Password;
  FProxyHTTP.Enabled := Proxy.Enabled;
  result := FProxyHTTP;
end;

function TsgcIdHTTP.GetProxyIOHandler(aTLS: Boolean): TIdIOHandlerStack;
begin
  if not aTLS then
  begin
    if not Assigned(FProxyIOHandler) then
      FProxyIOHandler := TIdIOHandlerStack.Create(self);
    result := FProxyIOHandler;
  end
  else
    result := FTLSHandler;
end;

function TsgcIdHTTP.GetProxySocks: TIdSocksInfo;
begin
  if not Assigned(FProxySocks) then
    FProxySocks := TIdSocksInfo.Create(self);
  FProxySocks.Host := Proxy.Host;
  FProxySocks.Port := Proxy.Port;
  if Proxy.Username <> '' then
    FProxySocks.Authentication := saUsernamePassword
  else
    FProxySocks.Authentication := saNoAuthentication;
  FProxySocks.Username := Proxy.Username;
  FProxySocks.Password := Proxy.Password;
  case Proxy.ProxyType of
    pxySocks4:
      FProxySocks.Version := svSocks4;
    pxySocks4A:
      FProxySocks.Version := svSocks4A;
    pxySocks5:
      FProxySocks.Version := svSocks5;
  end;
  result := FProxySocks;
end;
{$ENDIF}

function TsgcIdHTTP.GetVersion: String;
begin
  result := CS_VERSION;
end;

procedure TsgcIdHTTP.OnAuthTokenErrorEvent(Sender: TObject;
  const Error, ErrorDescription, Data: String);
begin
  raise Exception.Create(Format('Error %s. %s', [Error, ErrorDescription]));
  FAuthTokenExpiry := 0;
end;

procedure TsgcIdHTTP.OnAuthTokenEvent(Sender: TObject;
  const TokenType, Token, Data: String);
begin
  FAuthTokenType := TokenType;
  FAuthToken := Token;
  if FAuthToken <> '' then
  begin
    if Assigned(Authentication.Token.JWT) then
      FAuthTokenExpiry := IncSecond(Now,
        Authentication.Token.JWT.JWTOptions.RefreshTokenAfter);
  end;
end;

procedure TsgcIdHTTP.InitComponent;
begin
  inherited;
  FTLSOptions := TsgcIdHTTPTLS_Options.Create;
  FLog := False;
  FLogOptions := TsgcHttpLogFile.Create;
  Request.UserAgent := CS_USER_AGENT;
  FAuthentication := TsgcIdHTTPAuthentication_Options.Create;
  FProxy := TsgcHTTPProxy_Options.Create;
end;

procedure TsgcIdHTTP.OnGetPasswordEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE} AnsiString {$ENDIF});
begin
  Password := TLSOptions.Password;
end;

procedure TsgcIdHTTP.SetAuthentication(const Value
  : TsgcIdHTTPAuthentication_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcIdHTTP.SetLog(const Value: Boolean);
begin
{$IFNDEF MACOS32}
  if ((Value = False) and (FLog = True)) then
    DoNoLog;
{$ENDIF}
  FLog := Value;
end;

procedure TsgcIdHTTP.SetLogOptions(const Value: TsgcHttpLogFile);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

procedure TsgcIdHTTP.SetProxy(const Value: TsgcHTTPProxy_Options);
begin
  if Assigned(FProxy) then
    FProxy.Assign(Value);
end;

procedure TsgcIdHTTP.SetTLSOptions(const Value: TsgcIdHTTPTLS_Options);
begin
  if Assigned(FTLSOptions) then
    FTLSOptions.Assign(Value);
end;

procedure TsgcHttpLogFile.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHttpLogFile then
  begin
    FileName := TsgcHttpLogFile(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcIdHTTPAuthentication_Options.Create;
begin
  inherited;
  FToken := TsgcHTTPAuthentication_Token.Create;
end;

destructor TsgcIdHTTPAuthentication_Options.Destroy;
begin
  sgcFree(FToken);
  inherited;
end;

procedure TsgcIdHTTPAuthentication_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcIdHTTPAuthentication_Options then
  begin
    FToken.Assign(TsgcIdHTTPAuthentication_Options(aSource).Token);
  end
  else
    inherited;
end;

procedure TsgcIdHTTPAuthentication_Options.SetToken
  (const Value: TsgcHTTPAuthentication_Token);
begin
  if Assigned(FToken) then
    FToken.Assign(Value);
end;

constructor TsgcHTTPAuthentication_Token.Create;
begin
  inherited;
  FOAuth := nil;
  FJWT := nil;
  ReadTimeout := 60000;
end;

procedure TsgcHTTPClient.OnAuthTokenErrorEvent(Sender: TObject;
  const Error, ErrorDescription, Data: String);
begin
  inherited;
  if Assigned(FOnAuthTokenError) then
    FOnAuthTokenError(self, Error, ErrorDescription, Data);
end;

procedure TsgcHTTPClient.OnAuthTokenEvent(Sender: TObject;
  const TokenType, Token, Data: String);
begin
  inherited;
  if Assigned(FOnAuthToken) then
    FOnAuthToken(self, TokenType, Token, Data);
end;

function TsgcHTTPClient.RefreshToken(const aToken: string): Boolean;
begin
  result := False;
  if Assigned(Authentication.Token.OAuth) then
  begin
    Authentication.Token.OAuth.OnAuthToken := OnAuthTokenEvent;
    Authentication.Token.OAuth.OnAuthTokenError := OnAuthTokenErrorEvent;

    TsgcHTTPComponentClient_OAuth2(Authentication.Token.OAuth).Refresh(aToken);
  end;
end;
{$ENDIF}

end.
