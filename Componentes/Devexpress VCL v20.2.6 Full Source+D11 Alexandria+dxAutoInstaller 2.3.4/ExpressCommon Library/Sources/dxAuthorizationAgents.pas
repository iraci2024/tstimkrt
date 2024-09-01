{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressOfficeCore Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxAuthorizationAgents;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Forms, Windows,
  Generics.Defaults, Generics.Collections,
  IdHTTPServer, IdContext, IdCustomHTTPServer,
  OleCtrls, SHDocVw, dxWinInet,
  dxCore, dxCoreClasses, dxForms, cxClasses,
  dxLayoutControl, dxLayoutContainer;

type
  EdxAuthorizationAgentException = class(Exception);

  { TdxCustomAuthorizationForm }

  TdxCustomAuthorizationForm = class(TdxForm) // for internal use
  protected type
    TCompleteEvent = procedure(const AUri: string; var AComplete: Boolean)
      of object;
  strict private
    FOnComplete: TCompleteEvent;
  protected
    FComplete: Boolean;
    FRedirectUri: string;
    procedure DoComplete;
    function GetDefaultHeight: Integer; virtual; abstract;
    function GetDefaultWidth: Integer; virtual; abstract;
    procedure Initialize; virtual;

    procedure Navigate(const AUri: string); virtual; abstract;

    property OnComplete: TCompleteEvent read FOnComplete write FOnComplete;
  public
    constructor CreateEx; virtual;
    function ShowModal: Integer; override;
  end;

  { TdxAuthorizationForm }

  TdxAuthorizationForm = class(TdxCustomAuthorizationForm)
  public const
    DefaultHeight: Integer = 500;
    DefaultWidth: Integer = 500;
  strict private
    FWebBrowser: TWebBrowser;
    procedure NavigateCompleteHandler(ASender: TObject; const pDisp: IDispatch;
      {$IFDEF DELPHI16}const {$ELSE}var {$ENDIF} URL: OleVariant);
    procedure TitleChangedHandler(ASender: TObject; const Text: WideString);
  protected
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure Navigate(const AUri: string); override;
  public
{$REGION 'for internal use'}
    constructor CreateEx; override;
    destructor Destroy; override;
    property WebBrowser: TWebBrowser read FWebBrowser;
{$ENDREGION}
  end;

  { TdxCustomAuthorizationAgent }

  TdxCustomAuthorizationAgent = class abstract(TComponent)
  public type
    TErrorEvent = procedure(Sender: TObject; const AErrorObject) of object;
  public const
    DefaultUserAgent: string = '';
  strict private
    FUserAgent: string;
    FIsAuthorized: Boolean;

    FOnFinishAuthorization: TNotifyEvent;
    FOnError: TErrorEvent;
    FOnStartAuthorization: TNotifyEvent;
    function IsUserAgentStored: Boolean;
  protected
    procedure DoFinishAuthorization; virtual; abstract;
    procedure DoError(const AErrorObject); virtual;
    function DoStartAuthorization: Boolean; virtual; abstract;
    function DoValidateAuthorization: Boolean; virtual; abstract;
    procedure Initialize; virtual;

    function IsDestroying: Boolean;
    function IsReady: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure FinishAuthorization;
    procedure StartAuthorization;
    procedure RestartAuthorization;
    procedure ValidateAuthorization;

    function GetAuthorizationHeader: string; virtual; abstract;

    property IsAuthorized: Boolean read FIsAuthorized;
  published
    property UserAgent: string read FUserAgent write FUserAgent
      stored IsUserAgentStored;
    property OnFinishAuthorization: TNotifyEvent read FOnFinishAuthorization
      write FOnFinishAuthorization;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnStartAuthorization: TNotifyEvent read FOnStartAuthorization
      write FOnStartAuthorization;
  end;

  TdxCustomAuthorizationAgentClass = class of TdxCustomAuthorizationAgent;

  { IdxOAuth2AuthorizationAgentScopeRequestor }

  IdxOAuth2AuthorizationAgentScopeRequestor = interface
    function GetScopes: TStringList;
  end;

  { TdxOAuth2AuthorizationAgent }

  TdxOAuth2AuthorizationAgent = class abstract(TdxCustomAuthorizationAgent)
  public type
    TReceiveAuthorizationCodeEvent = procedure(Sender: TObject;
      out AAuthorizationCode: string) of object;
    TReceiveAccessTokenEvent = procedure(Sender: TObject;
      out ATokenAccess, ATokenRefresh, ATokenType: string) of object;
    TGetClientSecretEvent = procedure(Sender: TObject;
      var AClientSecret: string) of object;
  public const
    DefaultRedirectUri: string = 'http://localhost';
    DefaultIncludeGrantedScopes: Boolean = True;
    DefaultTokenExpiresIn = 3600;

    HeaderContentType = 'Content-Type: application/x-www-form-urlencoded';
  strict private
  const
    FHeaderAuthorization = 'Authorization: %s %s';
    FJSONAccessTokenParamName = 'access_token';
    FJSONAccessTokenExpiresInParamName = 'expires_in';
    FJSONAccessTokenTypeParamName = 'token_type';
    FJSONRefreshTokenParamName = 'refresh_token';
  strict private
    FAccessToken: string;
    FAccessTokenExpiresIn: Integer;
    FAccessTokenType: string;
    FAuthorizationCode: string;
    FGetAccessTokenTime: Cardinal;
    FRefreshToken: string;

    FClientID: string;
    FClientSecret: string;
    FRedirectUri: string;
    FIncludeGrantedScopes: Boolean;

    FAdditionalScopes: TStrings;
    FCachedScopes: string;
    FScopeRequestors: TList<IdxOAuth2AuthorizationAgentScopeRequestor>;

    FOnGetClientSecret: TGetClientSecretEvent;
    FOnReceiveAuthorizationCode: TReceiveAuthorizationCodeEvent;
    FOnReceiveAccessToken: TReceiveAccessTokenEvent;
    procedure AuthorizationFormNavigateCompleteHandler(const AUri: string;
      var AComplete: Boolean);
    procedure SetAdditionalScopes(const Value: TStrings);
    procedure SetClientID(const Value: string);
  private
    function HasAuthorizationCode: Boolean;
    function HasAccessToken: Boolean;
    function HasRefreshToken: Boolean;

    function ReceiveAuthorizationCode: Boolean;
    function ReceiveAccessToken: Boolean;

    function IsIncludeGrantedScopesStored: Boolean;
    function IsRedirectUriStored: Boolean;
    procedure SetRedirectUri(const Value: string);
    procedure SetIncludeGrantedScopes(const Value: Boolean);
    procedure SetClientSecret(const Value: string);
  protected
    function CreateAuthorizationForm: TdxCustomAuthorizationForm; virtual;
    function DoReceiveAuthorizationCode: Boolean; virtual;
    function DoReceiveAccessToken: Boolean; overload; virtual;

    function CanUseClientSecret: Boolean; virtual;
    procedure DoFinishAuthorization; override;
    function DoStartAuthorization: Boolean; override;
    function DoValidateAuthorization: Boolean; override;
    procedure Initialize; override;

    function DoAuthorizationFormCompleteHandler(const AUri: string): Boolean;
    function DoGetClientSecret: string; virtual;
    function GetAuthEndPoint: string; virtual; abstract;
    function GetReceiveTokenEndPointParams: string; virtual; abstract;
    function GetRedirectUri: string; virtual;
    function GetRefreshTokenEndPointParams: string; virtual; abstract;
    function GetRevokeAccessTokenEndPoint: string; virtual; abstract;
    function GetTokenEndPointObjectName: string; virtual; abstract;
    function GetTokenEndPointServerName: string; virtual; abstract;

    function IsReady: Boolean; override;

    function GetHeader: string;
    function GetScopes: string;
    function GetScopeDelimiter: Char; virtual; abstract;
    procedure PrepareScopes(AList: TStrings); virtual;

    class function EscapeDataString(const AData: string): string; static;

    procedure Clear; virtual;
    procedure DoReceiveAccessToken(AJSONObject: TdxJSONObject);
      overload; virtual;
    procedure DoRedirectUriChanged; virtual;
    procedure DoRefreshAccessToken(AJSONObject: TdxJSONObject); virtual;

    property AuthorizationCode: string read FAuthorizationCode;
    property IncludeGrantedScopes: Boolean read FIncludeGrantedScopes
      write SetIncludeGrantedScopes stored IsIncludeGrantedScopesStored;
    property OnReceiveAuthorizationCode: TReceiveAuthorizationCodeEvent
      read FOnReceiveAuthorizationCode write FOnReceiveAuthorizationCode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load(AAccessToken, ARefreshToken, AAccessTokenType: string);

    function GetAuthorizationHeader: string; override;
    function IsAccessTokenValid: Boolean;

    function RefreshAccessToken: Boolean;
    procedure RevokeAccessToken;
    procedure ValidateAccessToken;

    property AccessToken: string read FAccessToken;
    property AccessTokenExpiresIn: Integer read FAccessTokenExpiresIn;
    property AccessTokenType: string read FAccessTokenType;
    property RefreshToken: string read FRefreshToken;

    procedure RefreshScopes(ARestartAgent: Boolean);
    procedure RegisterScopeRequestor(const AIntf
      : IdxOAuth2AuthorizationAgentScopeRequestor);
    procedure UnregisterScopeRequestor(const AIntf
      : IdxOAuth2AuthorizationAgentScopeRequestor);
  published
    property AdditionalScopes: TStrings read FAdditionalScopes
      write SetAdditionalScopes;
    property ClientID: string read FClientID write SetClientID;
    property ClientSecret: string read FClientSecret write SetClientSecret
      stored False;
    property RedirectUri: string read FRedirectUri write SetRedirectUri
      stored IsRedirectUriStored;
    property OnGetClientSecret: TGetClientSecretEvent read FOnGetClientSecret
      write FOnGetClientSecret;
    property OnReceiveAccessToken: TReceiveAccessTokenEvent
      read FOnReceiveAccessToken write FOnReceiveAccessToken;
  end;

  TdxOAuth2AuthorizationAgentClass = class of TdxOAuth2AuthorizationAgent;

  { TdxGoogleAPIOAuth2AuthorizationForm }

  TdxGoogleAPIOAuth2AuthorizationForm = class(TdxCustomAuthorizationForm)
  // for internal use
  strict private
  const
    FDefaultHeight: Integer = 200;
    FDefaultWidth: Integer = 500;
  strict private
    FHTTPServer: TIdHTTPServer;
    FLayoutControl: TdxLayoutControl;
    FLabeledItem: TdxLayoutLabeledItem;
    procedure CommandGetHandler(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  protected
    procedure DoShow; override;
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure Initialize; override;
    procedure Navigate(const AUri: string); override;
  public
    constructor CreateEx; override;
    destructor Destroy; override;
  end;

  { TdxGoogleAPIOAuth2AuthorizationAgent }

  TdxGoogleAPIOAuth2AuthorizationAgent = class(TdxOAuth2AuthorizationAgent)
  public const
    DefaultClientID: string = '';
    DefaultClientSecret: string = '';
  strict private
  const
{$REGION 'strict private const'}
    FAuthEndPoint = 'https://accounts.google.com/o/oauth2/v2/auth?' +
      'scope=%s&' + 'access_type=offline&' + 'include_granted_scopes=true&' +
      'state=state_parameter_passthrough_value&' + 'redirect_uri=%s&' +
      'response_type=code&' + 'client_id=%s';
    FRevokeTokenAccessEndPoint =
      'https://accounts.google.com/o/oauth2/revoke?token=%s';

    FTokenEndPointVerb = 'www.googleapis.com';
    FTokenEndPointObjectName = 'oauth2/v4/token';
    FReceiveTokenEndPointParams = 'code=%s&' + 'redirect_uri=%s&' +
      'client_id=%s&' + 'client_secret=%s&' + 'scope=%s&' +
      'grant_type=authorization_code';
    FRefreshTokenEndPointParams = 'client_secret=%s&' +
      'grant_type=refresh_token&' + 'refresh_token=%s&' + 'client_id=%s';
{$ENDREGION}
  strict private
    FDefaultRedirectUriPort: Word;
    function GetRedirectUriPort: Word;
    procedure SetRedirectUriPort(const Value: Word);
  protected
    procedure Initialize; override;

    function CreateAuthorizationForm: TdxCustomAuthorizationForm; override;
    function GetAuthEndPoint: string; override;
    function GetReceiveTokenEndPointParams: string; override;
    function GetRedirectUri: string; override;
    function GetRefreshTokenEndPointParams: string; override;
    function GetRevokeAccessTokenEndPoint: string; override;
    function GetTokenEndPointObjectName: string; override;
    function GetTokenEndPointServerName: string; override;
    function GetScopeDelimiter: Char; override;
    procedure DoRedirectUriChanged; override;
  published
    property RedirectUriPort: Word read GetRedirectUriPort
      write SetRedirectUriPort default 0;
  end;

  { TdxMicrosoftGraphAPIOAuth2AuthorizationAgent }

  TdxMicrosoftGraphAPIOAuth2AuthorizationAgent = class
    (TdxOAuth2AuthorizationAgent)
  public const
    DefaultClientID: string = '';
    DefaultClientSecret: string = '';
  strict private
  const
{$REGION 'strict private const'}
    FAuthEndPoint =
      'https://login.microsoftonline.com/common/oauth2/v2.0/authorize?' +
      'access_type=offline&' + 'client_id=%s&' +
      'response_type=code&response_mode=query&' + 'redirect_uri=%s&' +
      'scope=%s';
    FTokenEndPointVerb = 'login.microsoftonline.com';
    FTokenEndPointObjectName = 'common/oauth2/v2.0/token';
    FReceiveTokenEndPointParams = 'code=%s&' + 'redirect_uri=%s&' +
      'client_id=%s&' + 'client_secret=%s&' + 'scope=%s&' +
      'grant_type=authorization_code';
    FReceiveTokenWithoutClientSecretEndPointParams = 'code=%s&' +
      'redirect_uri=%s&' + 'client_id=%s&' + 'scope=%s&' +
      'grant_type=authorization_code';
    FRefreshTokenEndPointParams = 'client_id=%s&' + 'grant_type=refresh_token&'
      + 'scope=%s&' + 'refresh_token=%s&' + 'redirect_uri=%s&' +
      'client_secret=%s';
    FRefreshTokenWithoutClientSecretEndPointParams = 'client_id=%s&' +
      'grant_type=refresh_token&' + 'scope=%s&' + 'refresh_token=%s&' +
      'redirect_uri=%s';
    FRevokeTokenAccessEndPoint =
      'https://login.microsoftonline.com/common/oauth2/v2.0/revoke?token=%s';
{$ENDREGION}
  protected
    function CanUseClientSecret: Boolean; override;
    procedure Initialize; override;

    function GetAuthEndPoint: string; override;
    function GetReceiveTokenEndPointParams: string; override;
    function GetRefreshTokenEndPointParams: string; override;
    function GetRevokeAccessTokenEndPoint: string; override;
    function GetTokenEndPointObjectName: string; override;
    function GetTokenEndPointServerName: string; override;
    function GetScopeDelimiter: Char; override;
    procedure PrepareScopes(AList: TStrings); override;
  published
    property RedirectUri;
  end;

  { TdxAuthorizationAgentUserInfo }

  TdxAuthorizationAgentUserInfoClass = class of TdxAuthorizationAgentUserInfo;

  TdxAuthorizationAgentUserInfo = class abstract(TcxIUnknownObject)
  strict private
    FAuthorizationAgent: TdxCustomAuthorizationAgent;
  strict private
    class var FUserInfoClassDictionary
      : TDictionary<TdxCustomAuthorizationAgentClass,
      TdxAuthorizationAgentUserInfoClass>;
  protected
    FDisplayName: string;
    FMail: string;
    procedure DoUpdateInfo; virtual; abstract;

    class procedure Initialize; static;
    class procedure Finalize; static;
    class procedure Register(AUserInfoClass: TdxAuthorizationAgentUserInfoClass;
      AAuthorizationAgentClass: TdxCustomAuthorizationAgentClass); static;
  public
    constructor Create(AAuthorizationAgent: TdxCustomAuthorizationAgent);

    class function GetUserInfo(AAuthorizationAgent: TdxCustomAuthorizationAgent)
      : TdxAuthorizationAgentUserInfo; static;

    procedure UpdateInfo;

    property AuthorizationAgent: TdxCustomAuthorizationAgent
      read FAuthorizationAgent;
    property DisplayName: string read FDisplayName;
    property Mail: string read FMail;
  end;

  { TdxOAuth2AuthorizationAgentUserInfo }

  TdxOAuth2AuthorizationAgentUserInfo = class abstract
    (TdxAuthorizationAgentUserInfo, IdxOAuth2AuthorizationAgentScopeRequestor)
  strict private
    function GetAuthorizationAgent: TdxOAuth2AuthorizationAgent;
  protected
    procedure DoUpdateInfo; override;
    function GetUpdateUserInfoUri: string; virtual; abstract;
    procedure JSONToUserInfo(AObject: TdxJSONObject); virtual; abstract;
{$REGION 'IdxOAuth2AuthorizationAgentScopeRequestor'}
    function GetScopes: TStringList; virtual; abstract;
{$ENDREGION}
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property AuthorizationAgent: TdxOAuth2AuthorizationAgent
      read GetAuthorizationAgent;
  end;

  { TdxGoogleAPIOAuth2AuthorizationAgentUserInfo }

  TdxGoogleAPIOAuth2AuthorizationAgentUserInfo = class
    (TdxOAuth2AuthorizationAgentUserInfo)
  protected
    function GetUpdateUserInfoUri: string; override;
    procedure JSONToUserInfo(AObject: TdxJSONObject); override;
    function GetScopes: TStringList; override;
  end;

  { TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo }

  TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo = class
    (TdxOAuth2AuthorizationAgentUserInfo)
  protected
    function GetUpdateUserInfoUri: string; override;
    procedure JSONToUserInfo(AObject: TdxJSONObject); override;
    function GetScopes: TStringList; override;
  end;

implementation

uses
  Controls, Messages, MSHTML,
  dxAuthorizationAgentsStrs,
  dxUriRecord, dxStringHelper;

{ TdxCustomAuthorizationForm }

constructor TdxCustomAuthorizationForm.CreateEx;
begin
  inherited CreateNew(nil);
  BorderIcons := [biSystemMenu];
  Width := GetDefaultWidth;
  Height := GetDefaultHeight;
  Initialize;
end;

procedure TdxCustomAuthorizationForm.DoComplete;
begin
  OnComplete := nil;
  ModalResult := mrOk;
end;

function TdxCustomAuthorizationForm.ShowModal: Integer;
begin
  cxDialogsMetricsStore.InitDialog(Self);
  Result := inherited ShowModal;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

procedure TdxCustomAuthorizationForm.Initialize;
begin
  // do nothing
end;

{ TdxAuthorizationForm }

constructor TdxAuthorizationForm.CreateEx;
begin
  inherited CreateEx;
  FWebBrowser := TWebBrowser.Create(Self);
  FWebBrowser.Align := alClient;
  FWebBrowser.OnTitleChange := TitleChangedHandler;
  FWebBrowser.OnNavigateComplete2 := NavigateCompleteHandler;
  FWebBrowser.SetParentComponent(Self);
end;

destructor TdxAuthorizationForm.Destroy;
begin
  FreeAndNil(FWebBrowser);
  inherited Destroy;
end;

function TdxAuthorizationForm.GetDefaultHeight: Integer;
begin
  Result := DefaultHeight;
end;

function TdxAuthorizationForm.GetDefaultWidth: Integer;
begin
  Result := DefaultWidth;
end;

procedure TdxAuthorizationForm.NavigateCompleteHandler(ASender: TObject;
  const pDisp: IDispatch; {$IFDEF DELPHI16}const {$ELSE}var
  {$ENDIF} URL: OleVariant);
begin
  if FComplete then
    Exit;
  if Assigned(OnComplete) then
    OnComplete(URL, FComplete);
  if FComplete then
    DoComplete;
end;

procedure TdxAuthorizationForm.TitleChangedHandler(ASender: TObject;
  const Text: WideString);
begin
  Caption := Text;
end;

procedure TdxAuthorizationForm.Navigate(const AUri: string);
begin
  FWebBrowser.Navigate(AUri);
end;

{ TdxCustomAuthorizationAgent }

constructor TdxCustomAuthorizationAgent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initialize;
end;

procedure TdxCustomAuthorizationAgent.FinishAuthorization;
begin
  if not IsAuthorized then
    Exit;
  DoFinishAuthorization;
  FIsAuthorized := False;
  CallNotify(FOnFinishAuthorization, Self);
end;

procedure TdxCustomAuthorizationAgent.StartAuthorization;
begin
  if IsAuthorized then
    Exit;
  FIsAuthorized := DoStartAuthorization;
  if IsAuthorized then
    CallNotify(FOnStartAuthorization, Self);
end;

procedure TdxCustomAuthorizationAgent.RestartAuthorization;
begin
  if IsAuthorized then
  begin
    FinishAuthorization;
    StartAuthorization;
  end;
end;

procedure TdxCustomAuthorizationAgent.ValidateAuthorization;
begin
  if not IsAuthorized then
    Exit;
  if not DoValidateAuthorization then
    FinishAuthorization;
end;

procedure TdxCustomAuthorizationAgent.DoError(const AErrorObject);
begin
  if Assigned(FOnError) then
    FOnError(Self, AErrorObject);
end;

procedure TdxCustomAuthorizationAgent.Initialize;
begin
  FUserAgent := DefaultUserAgent;
end;

function TdxCustomAuthorizationAgent.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxCustomAuthorizationAgent.IsReady: Boolean;
begin
  Result := True;
end;

function TdxCustomAuthorizationAgent.IsUserAgentStored: Boolean;
begin
  Result := FUserAgent <> DefaultUserAgent;
end;

{ TdxOAuth2AuthorizationAgent }

constructor TdxOAuth2AuthorizationAgent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScopeRequestors := TList<IdxOAuth2AuthorizationAgentScopeRequestor>.Create;
  FAdditionalScopes := TStringList.Create;
end;

destructor TdxOAuth2AuthorizationAgent.Destroy;
begin
  FreeAndNil(FAdditionalScopes);
  FreeAndNil(FScopeRequestors);
  inherited Destroy;
end;

procedure TdxOAuth2AuthorizationAgent.Load(AAccessToken, ARefreshToken,
  AAccessTokenType: string);
begin
  FinishAuthorization;
  FAccessToken := AAccessToken;
  FRefreshToken := ARefreshToken;
  FAccessTokenType := AAccessTokenType;
  FAccessTokenExpiresIn := 0;
  FGetAccessTokenTime := 0;
end;

function TdxOAuth2AuthorizationAgent.GetAuthorizationHeader: string;
begin
  Result := Format(FHeaderAuthorization,
    [AccessTokenType, EscapeDataString(AccessToken)]);
end;

function TdxOAuth2AuthorizationAgent.RefreshAccessToken: Boolean;
var
  AParams: TBytes;
  AJSONObject: TdxJSONObject;
begin
  Result := False;
  if not HasRefreshToken then
    Exit;
  FAccessToken := '';

  Result := False;
  AParams := TEncoding.UTF8.GetBytes(GetRefreshTokenEndPointParams);
  AJSONObject := TdxHttpHelper.PostRequest(UserAgent,
    GetTokenEndPointServerName, GetTokenEndPointObjectName, GetHeader, AParams);
  if AJSONObject <> nil then
    try
      FAccessToken := AJSONObject.GetParamValue(FJSONAccessTokenParamName);
      if not TryStrToInt(AJSONObject.GetParamValue
        (FJSONAccessTokenExpiresInParamName), FAccessTokenExpiresIn) then
        FAccessTokenExpiresIn := DefaultTokenExpiresIn;
      FAccessTokenType := AJSONObject.GetParamValue
        (FJSONAccessTokenTypeParamName);
      if AJSONObject.HasParam(FJSONRefreshTokenParamName) then
        FRefreshToken := AJSONObject.GetParamValue(FJSONRefreshTokenParamName);
      DoRefreshAccessToken(AJSONObject);
      Result := HasAccessToken;
      if Result then
        FGetAccessTokenTime := GetTickCount;
    finally
      AJSONObject.Free;
    end;
end;

procedure TdxOAuth2AuthorizationAgent.RevokeAccessToken;
var
  AUri: string;
  AJSONObject: TdxJSONObject;
begin
  if not IsAuthorized then
    Exit;
  if not HasAccessToken then
    Exit;
  AUri := GetRevokeAccessTokenEndPoint;
  AJSONObject := TdxHttpHelper.GetRequest(UserAgent, AUri, GetHeader);
  try
  finally
    AJSONObject.Free;
  end;
  FinishAuthorization;
end;

procedure TdxOAuth2AuthorizationAgent.Clear;
begin
  FAccessToken := '';
  FAuthorizationCode := '';
  FAccessTokenExpiresIn := DefaultTokenExpiresIn;
  FRefreshToken := '';
end;

function TdxOAuth2AuthorizationAgent.CanUseClientSecret: Boolean;
begin
  Result := True;
end;

procedure TdxOAuth2AuthorizationAgent.DoFinishAuthorization;
begin
  Clear;
end;

function TdxOAuth2AuthorizationAgent.DoStartAuthorization: Boolean;
begin
  if HasAccessToken then
    ValidateAccessToken;
  Result := IsAccessTokenValid or ReceiveAccessToken;
end;

function TdxOAuth2AuthorizationAgent.DoValidateAuthorization: Boolean;
begin
  ValidateAccessToken;
  Result := IsAccessTokenValid;
end;

procedure TdxOAuth2AuthorizationAgent.Initialize;
begin
  inherited Initialize;
  FRedirectUri := DefaultRedirectUri;
  FIncludeGrantedScopes := DefaultIncludeGrantedScopes;
end;

function TdxOAuth2AuthorizationAgent.DoAuthorizationFormCompleteHandler
  (const AUri: string): Boolean;

  function ParseAuthorizationCode(const S: string): string;
  var
    AStrings: TStringList;
    I: Integer;
  begin
    Result := '';
    AStrings := TStringList.Create;
    try
      AStrings.Delimiter := '&';
      AStrings.DelimitedText := S;
      for I := 0 to AStrings.Count - 1 do
      begin
        if TdxStringHelper.StartsWith(AStrings[I], 'code=') then
        begin
          Result := Copy(AStrings[I], 6, Length(AStrings[I]));
          Break;
        end;
      end;
    finally
      AStrings.Free;
    end;
  end;

var
  AUriRecord: TdxURI;
begin
  Result := TdxStringHelper.StartsWith(AUri, GetRedirectUri);
  if Result then
  begin
    AUriRecord := TdxURI.Create(AUri);
    FAuthorizationCode := ParseAuthorizationCode(AUriRecord.Params);
  end;
end;

function TdxOAuth2AuthorizationAgent.GetScopes: string;
var
  I, J: Integer;
  AResult: TStringList;
  AScopes: TStringList;
  S: string;
begin
  if FCachedScopes <> '' then
    Exit(FCachedScopes);
  AResult := TStringList.Create;
  try
    AResult.Assign(AdditionalScopes);
    for I := 0 to FScopeRequestors.Count - 1 do
    begin
      AScopes := FScopeRequestors[I].GetScopes;
      try
        for J := 0 to AScopes.Count - 1 do
        begin
          S := Trim(AScopes[J]);
          if (Length(S) > 0) and (AResult.IndexOf(S) = -1) then
            AResult.Add(S);
        end;
        AScopes.Sort;
      finally
        AScopes.Free;
      end;
    end;
    PrepareScopes(AResult);
    AResult.Delimiter := GetScopeDelimiter;
    FCachedScopes := EscapeDataString(AResult.DelimitedText);
  finally
    AResult.Free;
  end;
  Result := FCachedScopes;
end;

procedure TdxOAuth2AuthorizationAgent.PrepareScopes(AList: TStrings);
begin
  // do nothing
end;

function TdxOAuth2AuthorizationAgent.DoGetClientSecret: string;
begin
  Result := ClientSecret;
  if Assigned(OnGetClientSecret) then
    OnGetClientSecret(Self, Result);
end;

function TdxOAuth2AuthorizationAgent.IsReady: Boolean;
begin
  Result := inherited IsReady and (ClientID <> '') and
    (not CanUseClientSecret or (ClientSecret <> '') or
    Assigned(OnGetClientSecret)) and
    ((FScopeRequestors.Count > 0) or (AdditionalScopes.Count > 0));
end;

function TdxOAuth2AuthorizationAgent.GetHeader: string;
begin
  Result := HeaderContentType;
end;

function TdxOAuth2AuthorizationAgent.GetRedirectUri: string;
begin
  Result := RedirectUri;
end;

class function TdxOAuth2AuthorizationAgent.EscapeDataString
  (const AData: string): string;
begin
  Result := TdxURI.EscapeDataString(AData);
end;

procedure TdxOAuth2AuthorizationAgent.DoReceiveAccessToken
  (AJSONObject: TdxJSONObject);
begin
  // do nothing
end;

procedure TdxOAuth2AuthorizationAgent.DoRedirectUriChanged;
begin
  // do nothing
end;

procedure TdxOAuth2AuthorizationAgent.DoRefreshAccessToken
  (AJSONObject: TdxJSONObject);
begin
  // do nothing
end;

procedure TdxOAuth2AuthorizationAgent.ValidateAccessToken;
begin
  if not IsAccessTokenValid then
    RefreshAccessToken;
end;

procedure TdxOAuth2AuthorizationAgent.RefreshScopes(ARestartAgent: Boolean);
var
  AOldScopes: string;
begin
  if IsDestroying then
    Exit;
  if IsAuthorized then
    AOldScopes := GetScopes;
  FCachedScopes := '';
  if not IsAuthorized then
    Exit;
  if AOldScopes <> GetScopes then
    if ARestartAgent then
      RestartAuthorization;
end;

procedure TdxOAuth2AuthorizationAgent.RegisterScopeRequestor
  (const AIntf: IdxOAuth2AuthorizationAgentScopeRequestor);
begin
  FScopeRequestors.Add(AIntf);
  RefreshScopes(False);
end;

procedure TdxOAuth2AuthorizationAgent.UnregisterScopeRequestor
  (const AIntf: IdxOAuth2AuthorizationAgentScopeRequestor);
begin
  if FScopeRequestors = nil then
    Exit;
  FScopeRequestors.Remove(AIntf);
  RefreshScopes(False);
end;

function TdxOAuth2AuthorizationAgent.IsAccessTokenValid: Boolean;
begin
  Result := HasAccessToken and ((GetTickCount - FGetAccessTokenTime) / 1000 <
    AccessTokenExpiresIn);
end;

function TdxOAuth2AuthorizationAgent.CreateAuthorizationForm
  : TdxCustomAuthorizationForm;
begin
  Result := TdxAuthorizationForm.CreateEx;
end;

function TdxOAuth2AuthorizationAgent.HasAuthorizationCode: Boolean;
begin
  Result := AuthorizationCode <> '';
end;

function TdxOAuth2AuthorizationAgent.HasAccessToken: Boolean;
begin
  Result := AccessToken <> '';
end;

function TdxOAuth2AuthorizationAgent.HasRefreshToken: Boolean;
begin
  Result := RefreshToken <> '';
end;

function TdxOAuth2AuthorizationAgent.DoReceiveAuthorizationCode: Boolean;
begin
  if Assigned(FOnReceiveAuthorizationCode) then
    FOnReceiveAuthorizationCode(Self, FAuthorizationCode);
  Result := HasAuthorizationCode;
end;

function TdxOAuth2AuthorizationAgent.DoReceiveAccessToken: Boolean;
begin
  if Assigned(FOnReceiveAccessToken) then
    FOnReceiveAccessToken(Self, FAccessToken, FRefreshToken, FAccessTokenType);
  Result := HasAccessToken;
end;

function TdxOAuth2AuthorizationAgent.ReceiveAuthorizationCode: Boolean;
var
  AAuthorizationForm: TdxCustomAuthorizationForm;
  AUri: string;
begin
  if HasAuthorizationCode then
    Exit(True);
  if DoReceiveAuthorizationCode then
    Exit(HasAuthorizationCode);
  try
    AUri := GetAuthEndPoint;
    AAuthorizationForm := CreateAuthorizationForm;
    try
      AAuthorizationForm.OnComplete := AuthorizationFormNavigateCompleteHandler;
      AAuthorizationForm.FRedirectUri := GetRedirectUri;
      AAuthorizationForm.Navigate(AUri);
      AAuthorizationForm.ShowModal;
      Result := HasAuthorizationCode;
    finally
      AAuthorizationForm.Free;
    end;
  except
    Result := False;
  end;
end;

function TdxOAuth2AuthorizationAgent.ReceiveAccessToken: Boolean;
var
  AParams: TBytes;
  AJSONObject: TdxJSONObject;
begin
  FAccessToken := '';

  Result := False;
  if DoReceiveAccessToken then
    Exit(True);
  if not HasAuthorizationCode and not ReceiveAuthorizationCode then
    Exit;
  AParams := TEncoding.UTF8.GetBytes(GetReceiveTokenEndPointParams);
  AJSONObject := TdxHttpHelper.PostRequest(UserAgent,
    GetTokenEndPointServerName, GetTokenEndPointObjectName, GetHeader, AParams);
  if AJSONObject <> nil then
    try
      FAccessToken := AJSONObject.GetParamValue(FJSONAccessTokenParamName);
      FRefreshToken := AJSONObject.GetParamValue(FJSONRefreshTokenParamName);
      if not TryStrToInt(AJSONObject.GetParamValue
        (FJSONAccessTokenExpiresInParamName), FAccessTokenExpiresIn) then
        FAccessTokenExpiresIn := DefaultTokenExpiresIn;
      FAccessTokenType := AJSONObject.GetParamValue
        (FJSONAccessTokenTypeParamName);
      DoReceiveAccessToken(AJSONObject);
      Result := HasAccessToken;
      if Result then
        FGetAccessTokenTime := GetTickCount
      else
        DoError(AJSONObject);
    finally
      AJSONObject.Free;
    end;
end;

function TdxOAuth2AuthorizationAgent.IsIncludeGrantedScopesStored: Boolean;
begin
  Result := IncludeGrantedScopes <> DefaultIncludeGrantedScopes;
end;

function TdxOAuth2AuthorizationAgent.IsRedirectUriStored: Boolean;
begin
  Result := RedirectUri <> DefaultRedirectUri;
end;

procedure TdxOAuth2AuthorizationAgent.AuthorizationFormNavigateCompleteHandler
  (const AUri: string; var AComplete: Boolean);
begin
  AComplete := DoAuthorizationFormCompleteHandler(AUri);
end;

procedure TdxOAuth2AuthorizationAgent.SetAdditionalScopes
  (const Value: TStrings);
begin
  FAdditionalScopes.Assign(Value);
end;

procedure TdxOAuth2AuthorizationAgent.SetClientID(const Value: string);
begin
  if FClientID <> Value then
  begin
    FinishAuthorization;
    FClientID := Value;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetClientSecret(const Value: string);
begin
  if FClientSecret <> Value then
  begin
    FinishAuthorization;
    FClientSecret := Value;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetIncludeGrantedScopes
  (const Value: Boolean);
begin
  if FIncludeGrantedScopes <> Value then
  begin
    FinishAuthorization;
    FIncludeGrantedScopes := Value;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetRedirectUri(const Value: string);
begin
  if FRedirectUri <> Value then
  begin
    FinishAuthorization;
    FRedirectUri := Value;
    DoRedirectUriChanged;
  end;
end;

{ TdxGoogleAPIOAuth2AuthorizationForm }

constructor TdxGoogleAPIOAuth2AuthorizationForm.CreateEx;
begin
  inherited CreateEx;
  BorderStyle := bsDialog;
  AutoSize := True;
  FHTTPServer := TIdHTTPServer.Create(Self);
  FHTTPServer.OnCommandGet := CommandGetHandler;
end;

destructor TdxGoogleAPIOAuth2AuthorizationForm.Destroy;
begin
  FreeAndNil(FHTTPServer);
  inherited Destroy;
end;

procedure TdxGoogleAPIOAuth2AuthorizationForm.DoShow;
var
  APort: Integer;
  AUri: TdxURI;
begin
  inherited DoShow;
  AUri := TdxURI.Create(FRedirectUri);
  if (AUri.Port <> '') and TryStrToInt(AUri.Port, APort) then
    FHTTPServer.DefaultPort := APort;
  FHTTPServer.Active := True;
end;

procedure TdxGoogleAPIOAuth2AuthorizationForm.CommandGetHandler
  (AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  AUri: string;
begin
  if FComplete then
    Exit;
  if Assigned(OnComplete) then
  begin
    AUri := Format('%s?%s', [FRedirectUri, ARequestInfo.UnparsedParams]);
    OnComplete(AUri, FComplete);
    if FComplete then
    begin
      AResponseInfo.ContentText :=
        cxGetResourceString
        (@sdxGoogleAPIOAuth2AuthorizationFormResponseContent);
      DoComplete;
    end;
  end;
  Close;
end;

function TdxGoogleAPIOAuth2AuthorizationForm.GetDefaultHeight: Integer;
begin
  Result := TdxGoogleAPIOAuth2AuthorizationForm.FDefaultHeight;
end;

function TdxGoogleAPIOAuth2AuthorizationForm.GetDefaultWidth: Integer;
begin
  Result := TdxGoogleAPIOAuth2AuthorizationForm.FDefaultWidth;
end;

procedure TdxGoogleAPIOAuth2AuthorizationForm.Initialize;
begin
  inherited Initialize;
  Caption := cxGetResourceString(@sdxGoogleAPIOAuth2AuthorizationFormCaption);
  FLayoutControl := TdxLayoutControl.Create(Self);
  FLayoutControl.SetParentComponent(Self);
  FLayoutControl.AutoSize := True;
  FLabeledItem := TdxLayoutLabeledItem(FLayoutControl.Container.Root.CreateItem
    (TdxLayoutLabeledItem));
  FLabeledItem.Caption := cxGetResourceString
    (@sdxGoogleAPIOAuth2AuthorizationFormMessage);
end;

procedure TdxGoogleAPIOAuth2AuthorizationForm.Navigate(const AUri: string);
begin
  dxShellExecute(Handle, AUri);
end;

{ TdxGoogleAPIOAuth2AuthorizationAgent }

procedure TdxGoogleAPIOAuth2AuthorizationAgent.Initialize;
begin
  inherited Initialize;
  ClientID := DefaultClientID;
  ClientSecret := DefaultClientSecret;
  Randomize;
  FDefaultRedirectUriPort := MAXWORD - 1024 - Random(MAXWORD - 1024 * 16);
end;

procedure TdxGoogleAPIOAuth2AuthorizationAgent.SetRedirectUriPort
  (const Value: Word);
begin
  if RedirectUriPort <> Value then
  begin
    FinishAuthorization;
    if Value <> 0 then
      RedirectUri := Format('%s:%d', [DefaultRedirectUri, Value])
    else
      RedirectUri := DefaultRedirectUri;
  end;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.CreateAuthorizationForm
  : TdxCustomAuthorizationForm;
begin
  Result := TdxGoogleAPIOAuth2AuthorizationForm.CreateEx
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetAuthEndPoint: string;
begin
  Result := Format(FAuthEndPoint, [GetScopes, EscapeDataString(GetRedirectUri),
    EscapeDataString(ClientID)]);
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.
  GetReceiveTokenEndPointParams: string;
begin
  Result := Format(FReceiveTokenEndPointParams,
    [EscapeDataString(AuthorizationCode), EscapeDataString(GetRedirectUri),
    EscapeDataString(ClientID), EscapeDataString(DoGetClientSecret), GetScopes])
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetRedirectUri: string;
var
  APort: Integer;
begin
  if RedirectUriPort <> 0 then
    APort := RedirectUriPort
  else
    APort := FDefaultRedirectUriPort;
  Result := Format('%s:%d', [DefaultRedirectUri, APort]);
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetRedirectUriPort: Word;
var
  APort: Integer;
  AUri: TdxURI;
begin
  AUri := TdxURI.Create(RedirectUri);
  if (AUri.Port <> '') and TryStrToInt(AUri.Port, APort) then
    Result := APort
  else
    Result := 0;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.
  GetRefreshTokenEndPointParams: string;
begin
  Result := Format(FRefreshTokenEndPointParams,
    [EscapeDataString(DoGetClientSecret), EscapeDataString(RefreshToken),
    EscapeDataString(ClientID)])
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.
  GetRevokeAccessTokenEndPoint: string;
begin
  Result := Format(FRevokeTokenAccessEndPoint, [EscapeDataString(AccessToken)]);
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.
  GetTokenEndPointObjectName: string;
begin
  Result := FTokenEndPointObjectName;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.
  GetTokenEndPointServerName: string;
begin
  Result := FTokenEndPointVerb;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetScopeDelimiter: Char;
begin
  Result := ' ';
end;

procedure TdxGoogleAPIOAuth2AuthorizationAgent.DoRedirectUriChanged;
const
  AExceptionText =
    'The redirect URI must be http://localhost:[PORT]. Alternatively, you can assign the necessary port to the RedirectUriPort property.';
var
  AUri: TdxURI;
begin
  AUri := TdxURI.Create(RedirectUri);
  if not(AUri.IsWebScheme or (LowerCase(AUri.Host) = 'localhost')) then
    raise EdxAuthorizationAgentException.Create(AExceptionText);
end;

{ TdxMicrosoftGraphAPIOAuth2AuthorizationAgent }

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetAuthEndPoint: string;
begin
  Result := Format(FAuthEndPoint, [ClientID, EscapeDataString(RedirectUri),
    GetScopes]);
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.
  GetReceiveTokenEndPointParams: string;
begin
  if CanUseClientSecret then
    Result := Format(FReceiveTokenEndPointParams,
      [EscapeDataString(AuthorizationCode), EscapeDataString(RedirectUri),
      EscapeDataString(ClientID), EscapeDataString(DoGetClientSecret),
      GetScopes])
  else
    Result := Format(FReceiveTokenWithoutClientSecretEndPointParams,
      [EscapeDataString(AuthorizationCode), EscapeDataString(RedirectUri),
      EscapeDataString(ClientID), GetScopes]);
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.
  GetRefreshTokenEndPointParams: string;
begin
  if CanUseClientSecret then
    Result := Format(FRefreshTokenEndPointParams, [EscapeDataString(ClientID),
      GetScopes, EscapeDataString(RefreshToken), EscapeDataString(RedirectUri),
      EscapeDataString(DoGetClientSecret)])
  else
    Result := Format(FRefreshTokenWithoutClientSecretEndPointParams,
      [EscapeDataString(ClientID), GetScopes, EscapeDataString(RefreshToken),
      EscapeDataString(RedirectUri)])
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.
  GetRevokeAccessTokenEndPoint: string;
begin
  Result := Format(FRevokeTokenAccessEndPoint, [EscapeDataString(AccessToken)]);
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.
  GetTokenEndPointObjectName: string;
begin
  Result := FTokenEndPointObjectName;
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.
  GetTokenEndPointServerName: string;
begin
  Result := FTokenEndPointVerb;
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetScopeDelimiter: Char;
begin
  Result := ' ';
end;

procedure TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.Initialize;
begin
  inherited Initialize;
  ClientID := DefaultClientID;
  ClientSecret := DefaultClientSecret;
end;

procedure TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.PrepareScopes
  (AList: TStrings);
begin
  AList.Insert(0, 'offline_access');
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.
  CanUseClientSecret: Boolean;
var
  AUri: TdxURI;
begin
  AUri := TdxURI.Create(RedirectUri);
  Result := AUri.IsWebScheme;
end;

{ TdxAuthorizationAgentUserInfo }

constructor TdxAuthorizationAgentUserInfo.Create(AAuthorizationAgent
  : TdxCustomAuthorizationAgent);
begin
  inherited Create;
  FAuthorizationAgent := AAuthorizationAgent;
end;

class function TdxAuthorizationAgentUserInfo.GetUserInfo(AAuthorizationAgent
  : TdxCustomAuthorizationAgent): TdxAuthorizationAgentUserInfo;
var
  AClass: TdxAuthorizationAgentUserInfoClass;
begin
  if not FUserInfoClassDictionary.TryGetValue
    (TdxCustomAuthorizationAgentClass(AAuthorizationAgent.ClassType), AClass)
  then
    Exit(nil);
  Result := AClass.Create(AAuthorizationAgent);
end;

procedure TdxAuthorizationAgentUserInfo.UpdateInfo;
begin
  AuthorizationAgent.StartAuthorization;
  if not AuthorizationAgent.IsAuthorized then
    Exit;
  DoUpdateInfo;
end;

class procedure TdxAuthorizationAgentUserInfo.Initialize;
begin
  FUserInfoClassDictionary := TDictionary<TdxCustomAuthorizationAgentClass,
    TdxAuthorizationAgentUserInfoClass>.Create;
end;

class procedure TdxAuthorizationAgentUserInfo.Finalize;
begin
  FreeAndNil(FUserInfoClassDictionary);
end;

class procedure TdxAuthorizationAgentUserInfo.Register(AUserInfoClass
  : TdxAuthorizationAgentUserInfoClass;
  AAuthorizationAgentClass: TdxCustomAuthorizationAgentClass);
begin
  FUserInfoClassDictionary.Add(AAuthorizationAgentClass, AUserInfoClass);
end;

{ TdxOAuth2AuthorizationAgentUserInfo }

procedure TdxOAuth2AuthorizationAgentUserInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  AuthorizationAgent.RegisterScopeRequestor(Self);
end;

procedure TdxOAuth2AuthorizationAgentUserInfo.BeforeDestruction;
begin
  AuthorizationAgent.UnregisterScopeRequestor(Self);
  inherited BeforeDestruction;
end;

procedure TdxOAuth2AuthorizationAgentUserInfo.DoUpdateInfo;
var
  AObject: TdxJSONObject;
  AHeader: string;
begin
  AHeader := TdxOAuth2AuthorizationAgent.HeaderContentType + #13#10 +
    AuthorizationAgent.GetAuthorizationHeader;
  AObject := TdxHttpHelper.GetRequest(AuthorizationAgent.UserAgent,
    GetUpdateUserInfoUri, AHeader);
  if Assigned(AObject) then
    try
      JSONToUserInfo(AObject);
    finally
      AObject.Free;
    end;
end;

function TdxOAuth2AuthorizationAgentUserInfo.GetAuthorizationAgent
  : TdxOAuth2AuthorizationAgent;
begin
  Result := TdxOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

{ TdxGoogleAPIOAuth2AuthorizationAgentUserInfo }

function TdxGoogleAPIOAuth2AuthorizationAgentUserInfo.GetScopes: TStringList;
begin
  Result := TStringList.Create;
  Result.Insert(0, 'profile');
  Result.Insert(0, 'email');
end;

function TdxGoogleAPIOAuth2AuthorizationAgentUserInfo.
  GetUpdateUserInfoUri: string;
begin
  Result := 'https://www.googleapis.com/userinfo/v2/me';
end;

procedure TdxGoogleAPIOAuth2AuthorizationAgentUserInfo.JSONToUserInfo
  (AObject: TdxJSONObject);
begin
  FDisplayName := AObject.GetParamValue('name');
  FMail := AObject.GetParamValue('email');
end;

{ TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo }

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo.GetScopes
  : TStringList;
begin
  Result := TStringList.Create;
  Result.Insert(0, 'User.Read');
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo.
  GetUpdateUserInfoUri: string;
begin
  Result := 'https://graph.microsoft.com/v1.0/me/';
end;

procedure TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo.JSONToUserInfo
  (AObject: TdxJSONObject);
begin
  FDisplayName := AObject.GetParamValue('displayName');
  FMail := AObject.GetParamValue('mail');
end;

initialization

GroupDescendentsWith(TdxCustomAuthorizationAgent, TControl);
TdxAuthorizationAgentUserInfo.Initialize;
TdxAuthorizationAgentUserInfo.Register
  (TdxGoogleAPIOAuth2AuthorizationAgentUserInfo,
  TdxGoogleAPIOAuth2AuthorizationAgent);
TdxAuthorizationAgentUserInfo.Register
  (TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo,
  TdxMicrosoftGraphAPIOAuth2AuthorizationAgent);

finalization

TdxAuthorizationAgentUserInfo.Finalize;

end.
