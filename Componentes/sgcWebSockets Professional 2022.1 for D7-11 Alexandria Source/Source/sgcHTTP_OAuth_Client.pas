{ ***************************************************************************
  sgcHTTP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_OAuth_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_OAUTH}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdHTTPServer{$ELSE}IdHTTPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSocketHandle{$ELSE}IdSocketHandle{$ENDIF},
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomHTTPServer{$ELSE}IdCustomHTTPServer{$ENDIF},
  // sgc
  sgcHTTP_Classes, sgcWebSocket_Classes;

type

  TsgcHTTPOAuthSSL_Options = class(TsgcWSSSL_Options)
  end;

  TsgcHTTPOAuth_LocalServerOptions = class(TPersistent)
  private
    FIP: String;
    FPort: Integer;
    FRedirectURL: String;
    FSSL: Boolean;
    FSSLOptions: TsgcHTTPOAuthSSL_Options;
    procedure SetSSLOptions(const Value: TsgcHTTPOAuthSSL_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    function GetURL: String;
  published
    property IP: String read FIP write FIP;
    property Port: Integer read FPort write FPort;
    property RedirectURL: String read FRedirectURL write FRedirectURL;
    property SSL: Boolean read FSSL write FSSL;
    property SSLOptions: TsgcHTTPOAuthSSL_Options read FSSLOptions write
        SetSSLOptions;
  end;

  TsgcHTTPOAuth_AuthorizationServer = class(TPersistent)
  private
    FAuthURL: String;
    FScope: TStringList;
    FTokenURL: String;
    procedure SetScope(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    function GetScope: String;
    function GetScopeUnEncoded: string;
  published
    property AuthURL: String read FAuthURL write FAuthURL;
    property Scope: TStringList read FScope write SetScope;
    property TokenURL: String read FTokenURL write FTokenURL;
  end;

  TsgcHTTPComponentClient_OAuth = class(TsgcHTTPComponentClientAuthToken_Base)
    { http server }
  protected
    FHTTPServer: TObject;
  protected
    procedure OnCommandGetEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
      virtual; abstract;
  protected
    procedure DoStartServer; virtual;
    procedure DoStopServer; virtual;
    { http server }

    { methods }
  protected
    function DoGoToURL(const aURL: String): Boolean;
  public
    procedure Start; virtual; abstract;
    procedure Stop; virtual;
    { methods }

    { constructor/destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor/destructor }

    { properties }
  private
    FAuthorizationServerOptions: TsgcHTTPOAuth_AuthorizationServer;
    FLocalServerOptions: TsgcHTTPOAuth_LocalServerOptions;
    procedure SetAuthorizationServerOptions(const Value
      : TsgcHTTPOAuth_AuthorizationServer);
    procedure SetLocalServerOptions(const Value
      : TsgcHTTPOAuth_LocalServerOptions);
  public
    property AuthorizationServerOptions: TsgcHTTPOAuth_AuthorizationServer
      read FAuthorizationServerOptions write SetAuthorizationServerOptions;
    property LocalServerOptions: TsgcHTTPOAuth_LocalServerOptions
      read FLocalServerOptions write SetLocalServerOptions;
    { properties }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_OAUTH}

uses
{$IFDEF ANDROID}Androidapi.JNI.Net, {$ENDIF}
{$IFDEF ANDROID}Androidapi.JNI.JavaTypes, {$ENDIF}
{$IFDEF ANDROID}Androidapi.JNI.App, {$ENDIF}
{$IFDEF ANDROID}{$IFDEF DXE6}Androidapi.Helpers, {$ENDIF}{$ENDIF}
{$IFDEF ANDROID}{$IFNDEF D10}FMX.Helpers.Android, {$ENDIF}{$ENDIF}
{$IFDEF ANDROID}Androidapi.JNI.GraphicsContentViewText, {$ENDIF}
{$IFDEF IOS}{$IFDEF DXE8}macapi.helpers, FMX.helpers.iOS, {$ENDIF}iOSapi.Foundation, {$ENDIF}
{$IFDEF MSWINDOWS}ShellAPI, {$ENDIF}
{$IFDEF MACOS}Posix.Stdlib, {$ENDIF}
  StrUtils,
  // sgc
{$IFDEF SGC_EDT_PRO}sgcWebSocket_Server, {$ENDIF}
  sgcBase_Helpers, sgcHTTP_Helpers;

constructor TsgcHTTPComponentClient_OAuth.Create(aOwner: TComponent);
begin
  inherited;
  FLocalServerOptions := TsgcHTTPOAuth_LocalServerOptions.Create;
  FAuthorizationServerOptions := TsgcHTTPOAuth_AuthorizationServer.Create;
end;

destructor TsgcHTTPComponentClient_OAuth.Destroy;
begin
  DoStopServer;
  sgcFree(FAuthorizationServerOptions);
  sgcFree(FLocalServerOptions);
  sgcFree(FHTTPServer);
  inherited;
end;


function TsgcHTTPComponentClient_OAuth.DoGoToURL(const aURL: String): Boolean;
{$IFDEF ANDROID}
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
    TJnet_Uri.JavaClass.parse(StringToJString(aURL)));
  {$IFDEF D10}
  TAndroidHelper.Activity.startActivity(Intent);
  {$ELSE}
  SharedActivity.startActivity(Intent);
  {$ENDIF}
  result := True;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  oShell: TShellExecuteInfoW;
  vFileName: WideString;
begin
  vFileName := aURL;
  FillChar(oShell, SizeOf(oShell), 0);
  with oShell do
  begin
    cbSize := SizeOf(TShellExecuteInfo);
    fmask := SEE_MASK_NOCLOSEPROCESS OR SEE_MASK_FLAG_DDEWAIT OR
      SEE_MASK_FLAG_NO_UI;
    Wnd := hInstance;
    lpVerb := 'open';
    lpFile := PWideChar(vFileName);
  end;
  Result := ShellExecuteExW(@oShell);
{$ELSE}
{$IFDEF IOS}
begin
{$IFDEF DXE8}
  SharedApplication.OpenURL(StrToNSUrl(aURL));
  result := True;
{$ELSE}
  result := False;
{$ENDIF}
{$ELSE}

{$IFDEF MACOS}
begin
  _system(PAnsiChar('open "' + AnsiString(aURL) + '"'));
  result := True;
{$ELSE}
begin
  result := False;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure TsgcHTTPComponentClient_OAuth.DoStartServer;
var
  oBinding: TIdSocketHandle;
  {$IFDEF SGC_EDT_PRO}
  oServer: TsgcWSHTTPServer;
  {$ELSE}
  oServer: TIdHTTPServer;
  {$ENDIF}
begin
  if not Assigned(FHTTPServer) then
  begin
    {$IFDEF SGC_EDT_PRO}
    FHTTPServer := TsgcWSHTTPServer.Create(nil);
    TsgcWSHTTPServer(FHTTPServer).OnCommandGet := OnCommandGetEvent;
    {$ELSE}
    FHTTPServer := TIdHTTPServer.Create(nil);
    TIdHTTPServer(FHTTPServer).OnCommandGet := OnCommandGetEvent;
    {$ENDIF}
  end;
  {$IFDEF SGC_EDT_PRO}
  oServer := TsgcWSHTTPServer(FHTTPServer);
  {$ELSE}
  oServer := TIdHTTPServer(FHTTPServer);
  {$ENDIF}

  if oServer.Active = False then
  begin
    {$IFDEF SGC_EDT_PRO}
    oServer.Port := LocalServerOptions.Port;
    oServer.SSL := LocalServerOptions.SSL;
    oServer.SSLOptions := LocalServerOptions.SSLOptions;
    {$ELSE}
    oServer.DefaultPort := LocalServerOptions.Port;
    {$ENDIF}
    // ... bindings
    oBinding := oServer.Bindings.Add;
    oBinding.IP := LocalServerOptions.IP;
    {$IFDEF SGC_EDT_PRO}
    if oServer.SSL then
      oBinding.Port := LocalServerOptions.SSLOptions.Port
    else
    {$ENDIF}
      oBinding.Port := LocalServerOptions.Port;

    oServer.Active := True;
  end;
end;

procedure TsgcHTTPComponentClient_OAuth.DoStopServer;
var
  {$IFDEF SGC_EDT_PRO}
  oServer: TsgcWSHTTPServer;
  {$ELSE}
  oServer: TIdHTTPServer;
  {$ENDIF}
begin
  {$IFDEF SGC_EDT_PRO}
  oServer := TsgcWSHTTPServer(FHTTPServer);
  {$ELSE}
  oServer := TIdHTTPServer(FHTTPServer);
  {$ENDIF}
  if Assigned(oServer) then
  begin
    if oServer.Active then
    begin
      oServer.Active := False;
      oServer.Bindings.Clear;
    end;
  end;
end;

procedure TsgcHTTPComponentClient_OAuth.SetAuthorizationServerOptions
  (const Value: TsgcHTTPOAuth_AuthorizationServer);
begin
  FAuthorizationServerOptions.Assign(Value);
end;

procedure TsgcHTTPComponentClient_OAuth.SetLocalServerOptions
  (const Value: TsgcHTTPOAuth_LocalServerOptions);
begin
  if Assigned(FLocalServerOptions) then
    FLocalServerOptions.Assign(Value);
end;

procedure TsgcHTTPComponentClient_OAuth.Stop;
begin
  DoStopServer;
end;

constructor TsgcHTTPOAuth_LocalServerOptions.Create;
begin
  inherited;
  FSSLOptions := TsgcHTTPOAuthSSL_Options.Create;
  IP := '127.0.0.1';
  Port := 8080;
  SSL := False;
end;

destructor TsgcHTTPOAuth_LocalServerOptions.Destroy;
begin
  sgcFree(FSSLOptions);
  inherited;
end;

procedure TsgcHTTPOAuth_LocalServerOptions.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPOAuth_LocalServerOptions then
  begin
    IP := TsgcHTTPOAuth_LocalServerOptions(aSource).IP;
    RedirectURL := TsgcHTTPOAuth_LocalServerOptions(aSource).RedirectURL;
    Port := TsgcHTTPOAuth_LocalServerOptions(aSource).Port;
    SSL := TsgcHTTPOAuth_LocalServerOptions(aSource).SSL;
    SSLOptions := TsgcHTTPOAuth_LocalServerOptions(aSource).SSLOptions;
  end
  else
    inherited Assign(aSource);
end;

function TsgcHTTPOAuth_LocalServerOptions.GetURL: String;
begin
  if RedirectURL <> '' then
    result := RedirectURL
  else
  begin
    if SSL then
      Result := 'https://' + IP + ':' + IntToStr(Port)
    else
      Result := 'http://' + IP + ':' + IntToStr(Port);
  end;
end;

procedure TsgcHTTPOAuth_LocalServerOptions.SetSSLOptions(const Value:
    TsgcHTTPOAuthSSL_Options);
begin
  FSSLOptions := Value;
end;

constructor TsgcHTTPOAuth_AuthorizationServer.Create;
begin
  inherited;
  FScope := TStringList.Create;
end;

destructor TsgcHTTPOAuth_AuthorizationServer.Destroy;
begin
  sgcFree(FScope);
  inherited;
end;

procedure TsgcHTTPOAuth_AuthorizationServer.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPOAuth_AuthorizationServer then
  begin
    AuthURL := TsgcHTTPOAuth_AuthorizationServer(aSource).AuthURL;
    TokenURL := TsgcHTTPOAuth_AuthorizationServer(aSource).TokenURL;
    Scope.Text := TsgcHTTPOAuth_AuthorizationServer(aSource).Scope.Text;
  end
  else
    inherited Assign(aSource);
end;

function TsgcHTTPOAuth_AuthorizationServer.GetScope: String;
begin
  Result := sgcPathEncode(GetScopeUnEncoded);
end;

function TsgcHTTPOAuth_AuthorizationServer.GetScopeUnEncoded: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Scope.Count - 1 do
  begin
    if Result = '' then
      Result := Trim(Scope[i])
    else
      Result := Result + ' ' + Trim(Scope[i]);
  end;
end;

procedure TsgcHTTPOAuth_AuthorizationServer.SetScope(const Value: TStringList);
begin
  if Assigned(FScope) then
    FScope.Assign(Value);
end;

{$ENDIF}

end.
