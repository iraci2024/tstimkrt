{ ***************************************************************************
  sgcJWT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders{$ELSE}IdSSLOpenSSLHeaders{$ENDIF},
  // sgc
  sgcHTTP_Classes, sgcHTTP_JWT_Types, sgcHTTP_JWT_Classes, sgcBase_Helpers,
  sgcHTTP_JWT_RSA, sgcHTTP_JWT_ES, sgcHTTP_JWT_HMAC, sgcSocket_Classes,
  sgcWebSocket_Types;

Type

  TsgcHTTP_JWT_Client_Algorithm_Options = class(TPersistent)

  end;

  TsgcHTTP_JWT_Client_HS_Options = class(TsgcHTTP_JWT_Client_Algorithm_Options)
  private
    FSecret: string;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Secret: string read FSecret write FSecret;
  end;

  TsgcHTTP_JWT_Client_RS_Options = class(TsgcHTTP_JWT_Client_Algorithm_Options)
  private
    FPrivateKey: TStringList;
    procedure SetPrivateKey(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property PrivateKey: TStringList read FPrivateKey write SetPrivateKey;
  end;

  TsgcHTTP_JWT_Client_ES_Options = class(TsgcHTTP_JWT_Client_Algorithm_Options)
  private
    FPrivateKey: TStringList;
    procedure SetPrivateKey(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property PrivateKey: TStringList read FPrivateKey write SetPrivateKey;
  end;

  TsgcHTTP_JWT_Client_Algorithms_Options = class(TPersistent)
  private
    FES: TsgcHTTP_JWT_Client_ES_Options;
    FHS: TsgcHTTP_JWT_Client_HS_Options;
    FRS: TsgcHTTP_JWT_Client_RS_Options;
    procedure SetES(const Value: TsgcHTTP_JWT_Client_ES_Options);
    procedure SetHS(const Value: TsgcHTTP_JWT_Client_HS_Options);
    procedure SetRS(const Value: TsgcHTTP_JWT_Client_RS_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ES: TsgcHTTP_JWT_Client_ES_Options read FES write SetES;
    property HS: TsgcHTTP_JWT_Client_HS_Options read FHS write SetHS;
    property RS: TsgcHTTP_JWT_Client_RS_Options read FRS write SetRS;
  end;

  TsgcHTTP_JWT_Client_Options = class(TPersistent)
  private
    FAlgorithms: TsgcHTTP_JWT_Client_Algorithms_Options;
    FHeader: TsgcHTTP_JWTHeader;
    FOpenSSL_Options: TsgcSocketOpenSSL_Options;
    FPayload: TsgcHTTP_JWTPayload;
    FRefreshTokenAfter: Integer;
    function GetHeader: TsgcHTTP_JWTHeader;
    function GetPayload: TsgcHTTP_JWTPayload;
    procedure SetAlgorithms(const Value: TsgcHTTP_JWT_Client_Algorithms_Options);
    procedure SetOpenSSL_Options(const Value: TsgcSocketOpenSSL_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Algorithms: TsgcHTTP_JWT_Client_Algorithms_Options read FAlgorithms
        write SetAlgorithms;
    property Header: TsgcHTTP_JWTHeader read GetHeader write FHeader;
    property OpenSSL_Options: TsgcSocketOpenSSL_Options read FOpenSSL_Options write
        SetOpenSSL_Options;
    property Payload: TsgcHTTP_JWTPayload read GetPayload write FPayload;
    property RefreshTokenAfter: Integer read FRefreshTokenAfter write
        FRefreshTokenAfter;
  end;

  TsgcHTTPComponentClient_JWT = class(TsgcHTTPComponentClientAuthToken_Base)
    { hmac }
  private
    FJWTHMAC: TsgcHTTP_JWT_HMAC;
    function GetJWTHMAC: TsgcHTTP_JWT_HMAC;
  protected
    property JWTHMAC: TsgcHTTP_JWT_HMAC read GetJWTHMAC write FJWTHMAC;
    { hmac }

    { rsa }
  private
    FJWTRSA: TsgcHTTP_JWT_RSA;
    function GetJWTRSA: TsgcHTTP_JWT_RSA;
  protected
    property JWTRSA: TsgcHTTP_JWT_RSA read GetJWTRSA write FJWTRSA;
    { rsa }

    { es }
  private
    FJWTES: TsgcHTTP_JWT_ES;
    function GetJWTES: TsgcHTTP_JWT_ES;
  protected
    property JWTES: TsgcHTTP_JWT_ES read GetJWTES write FJWTES;
    { es }

    { options }
  private
    FJWTOptions: TsgcHTTP_JWT_Client_Options;
    procedure SetJWTOptions(const Value: TsgcHTTP_JWT_Client_Options);
  public
    property JWTOptions: TsgcHTTP_JWT_Client_Options read FJWTOptions
      write SetJWTOptions;
    { options }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { sign }
  private
    procedure DoInitializeOpenSSL;
  private
    function GetValueToSign: TBytes;
    function DoEncodeBase64(const aValue: String): string;
  protected
    function DoSignHMAC: string; virtual;
    function DoSignRSA: string; virtual;
    function DoSignES: string; virtual;
  public
    procedure Start;
    function Sign: string;
    { sign }
  end;


{$ENDIF}

implementation

{$IFDEF SGC_JWT}

uses
  DateUtils, StrUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCoderMIME{$ELSE}IdCoderMIME{$ENDIF},
  // sgc
  sgcHTTP_JWT_Helpers;

constructor TsgcHTTPComponentClient_JWT.Create(aOwner: TComponent);
begin
  inherited;
  FJWTOptions := TsgcHTTP_JWT_Client_Options.Create;
end;

destructor TsgcHTTPComponentClient_JWT.Destroy;
begin
  sgcFree(FJWTOptions);
  sgcFree(FJWTES);
  sgcFree(FJWTRSA);
  sgcFree(FJWTHMAC);
  inherited;
end;

function TsgcHTTPComponentClient_JWT.DoEncodeBase64(const aValue
  : String): string;
begin
  result := EncodeBase64(aValue);
  while RightStr(result, 1) = '=' do
    result := LeftStr(result, Length(result) - 1);
end;

procedure TsgcHTTPComponentClient_JWT.DoInitializeOpenSSL;
begin
{$IFDEF SGC_INDY_LIB}
  case JWTOptions.OpenSSL_Options.APIVersion of
    oslAPI_1_0:
      OPENSSL_API_VERSION := opSSL_1_0;
    oslAPI_1_1:
      OPENSSL_API_VERSION := opSSL_1_1;
    oslAPI_3_0:
      OPENSSL_API_VERSION := opSSL_3_0;
  end;
{$ENDIF}
  case JWTOptions.OpenSSL_Options.LibPath of
    oslpNone:
      ;
    oslpDefaultFolder:
      sgcIdOpenSSLSetLibPath(sgcGetOpenSSLDefaultFolder);
    oslpCustomPath:
      sgcIdOpenSSLSetLibPath(JWTOptions.OpenSSL_Options.LibPathCustom);
  end;
  {$IFDEF SGC_UNIX_SYMLINKS}
  case JWTOptions.OpenSSL_Options.UnixSymLinks of
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
end;

function TsgcHTTPComponentClient_JWT.DoSignES: string;
var
  oKey, oResult: TBytes;
begin
  DoInitializeOpenSSL;

  oKey := sgcGetBytesFromUTF8String(JWTOptions.Algorithms.ES.PrivateKey.Text);
  case JWTOptions.Header.alg of
    jwtES256:
      oResult := JWTES.SignES256(GetValueToSign, oKey);
    jwtES384:
      oResult := JWTES.SignES384(GetValueToSign, oKey);
    jwtES512:
      oResult := JWTES.SignES512(GetValueToSign, oKey);
    else
      oResult := nil;
  end;

  result := DoEncodeBase64(JWTOptions.Header.JSONText) + '.' +
    DoEncodeBase64(JWTOptions.Payload.JSONText) + '.' +
    sgcURLEncode(sgcEncodeBase64(oResult));
end;

function TsgcHTTPComponentClient_JWT.DoSignHMAC: string;
var
  oSecret, oResult: TBytes;
begin
  DoInitializeOpenSSL;

  oSecret := sgcGetBytesFromUTF8String(JWTOptions.Algorithms.HS.Secret);
  case JWTOptions.Header.alg of
    jwtHS256:
      oResult := JWTHMAC.SignHMAC256(GetValueToSign, oSecret);
    jwtHS384:
      oResult := JWTHMAC.SignHMAC384(GetValueToSign, oSecret);
    jwtHS512:
      oResult := JWTHMAC.SignHMAC512(GetValueToSign, oSecret);
    else
      oResult := nil;
  end;

  result := DoEncodeBase64(JWTOptions.Header.JSONText) + '.' +
    DoEncodeBase64(JWTOptions.Payload.JSONText) + '.' +
    sgcURLEncode(sgcEncodeBase64(oResult));
end;

function TsgcHTTPComponentClient_JWT.DoSignRSA: string;
var
  oKey, oResult: TBytes;
begin
  DoInitializeOpenSSL;

  oKey := sgcGetBytesFromUTF8String(JWTOptions.Algorithms.RS.PrivateKey.Text);
  case JWTOptions.Header.alg of
    jwtRS256:
      oResult := JWTRSA.SignRSA256(GetValueToSign, oKey);
    jwtRS384:
      oResult := JWTRSA.SignRSA384(GetValueToSign, oKey);
    jwtRS512:
      oResult := JWTRSA.SignRSA512(GetValueToSign, oKey);
    else
      oResult := nil;
  end;

  result := DoEncodeBase64(JWTOptions.Header.JSONText) + '.' +
    DoEncodeBase64(JWTOptions.Payload.JSONText) + '.' +
    sgcURLEncode(sgcEncodeBase64(oResult));
end;

function TsgcHTTPComponentClient_JWT.GetJWTES: TsgcHTTP_JWT_ES;
begin
  if not Assigned(FJWTES) then
    FJWTES := TsgcHTTP_JWT_ES.Create;
  result := FJWTES;
end;

function TsgcHTTPComponentClient_JWT.GetJWTHMAC: TsgcHTTP_JWT_HMAC;
begin
  if not Assigned(FJWTHMAC) then
    FJWTHMAC := TsgcHTTP_JWT_HMAC.Create;
  result := FJWTHMAC;
end;

function TsgcHTTPComponentClient_JWT.GetJWTRSA: TsgcHTTP_JWT_RSA;
begin
  if not Assigned(FJWTRSA) then
    FJWTRSA := TsgcHTTP_JWT_RSA.Create;
  result := FJWTRSA;
end;

function TsgcHTTPComponentClient_JWT.GetValueToSign: TBytes;
var
  i: Integer;
  oBytes: TBytes;
begin
  // ... initializae
  SetLength(result, 0);
  // ... header
  result := sgcGetBytesFromUTF8String(DoEncodeBase64(JWTOptions.Header.JSONText));
  SetLength(result, Length(result) + 1);
  // ... separator
  result[Length(result) - 1] := Byte('.');
  // ... payload
  i := Length(result);
  oBytes := sgcGetBytesFromUTF8String(DoEncodeBase64(JWTOptions.Payload.JSONText));
  SetLength(result, Length(result) + Length(oBytes));
  sgcMove(oBytes[0], result[i], Length(oBytes));
end;

procedure TsgcHTTPComponentClient_JWT.SetJWTOptions
  (const Value: TsgcHTTP_JWT_Client_Options);
begin
  if Assigned(FJWTOptions) then
    FJWTOptions.Assign(Value);
end;

function TsgcHTTPComponentClient_JWT.Sign: string;
begin
  // ... update expiry if set
  if JWTOptions.RefreshTokenAfter > 0 then
  begin
    JWTOptions.Payload.iat := StrToInt64(GetDateTimeUnix(Now, False));
    JWTOptions.Payload.exp :=
      StrToInt64(GetDateTimeUnix(IncSecond(Now,
      JWTOptions.RefreshTokenAfter), False));
  end;

  case JWTOptions.Header.alg of
    jwtHS256, jwtHS384, jwtHS512:
      result := DoSignHMAC;
    jwtRS256, jwtRS384, jwtRS512:
      result := DoSignRSA;
    jwtES256, jwtES384, jwtES512:
      result := DoSignES;
  end;
end;

procedure TsgcHTTPComponentClient_JWT.Start;
var
  vToken: string;
begin
  Try
    vToken := Sign;
    if vToken <> '' then
      DoAuthTokenEvent('Bearer', vToken, '')
    else
      DoAuthTokenErrorEvent('JWT Error', 'JWT Token cannot be empty.', '');
  Except
    On E: Exception do
      DoAuthTokenErrorEvent('JWT Error', E.message, '');
  end;
end;

constructor TsgcHTTP_JWT_Client_Options.Create;
begin
  inherited;
  FAlgorithms := TsgcHTTP_JWT_Client_Algorithms_Options.Create;
  FOpenSSL_Options := TsgcSocketOpenSSL_Options.Create;
  FRefreshTokenAfter := 0;
end;

destructor TsgcHTTP_JWT_Client_Options.Destroy;
begin
  sgcFree(FOpenSSL_Options);
  sgcFree(FAlgorithms);
  sgcFree(FHeader);
  sgcFree(FPayload);
  inherited;
end;

procedure TsgcHTTP_JWT_Client_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWTPayload then
  begin
    Header.Assign(TsgcHTTP_JWT_Client_Options(aSource).Header);
    Payload.Assign(TsgcHTTP_JWT_Client_Options(aSource).Payload);
    Algorithms.Assign(TsgcHTTP_JWT_Client_Options(aSource).Algorithms);
    RefreshTokenAfter := TsgcHTTP_JWT_Client_Options(aSource).RefreshTokenAfter;
    OpenSSL_Options.Assign(TsgcHTTP_JWT_Client_Options(aSource).OpenSSL_Options);
  end
  else
    inherited Assign(aSource);
end;

function TsgcHTTP_JWT_Client_Options.GetHeader: TsgcHTTP_JWTHeader;
begin
  if not Assigned(FHeader) then
    FHeader := TsgcHTTP_JWTHeader.Create;
  Result := FHeader;
end;

function TsgcHTTP_JWT_Client_Options.GetPayload: TsgcHTTP_JWTPayload;
begin
  if not Assigned(FPayload) then
    FPayload := TsgcHTTP_JWTPayload.Create;
  Result := FPayload;
end;

procedure TsgcHTTP_JWT_Client_Options.SetAlgorithms(const Value:
    TsgcHTTP_JWT_Client_Algorithms_Options);
begin
  if Assigned(FAlgorithms) then
    FAlgorithms.Assign(Value);
end;

procedure TsgcHTTP_JWT_Client_Options.SetOpenSSL_Options(const Value:
    TsgcSocketOpenSSL_Options);
begin
  if Assigned(FOpenSSL_Options) then
    FOpenSSL_Options.Assign(Value);
end;

procedure TsgcHTTP_JWT_Client_HS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWT_Client_HS_Options then
  begin
    Secret := TsgcHTTP_JWT_Client_HS_Options(aSource).Secret;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTP_JWT_Client_RS_Options.Create;
begin
  inherited;
  FPrivateKey := TStringList.Create;
end;

destructor TsgcHTTP_JWT_Client_RS_Options.Destroy;
begin
  sgcFree(FPrivateKey);
  inherited;
end;

procedure TsgcHTTP_JWT_Client_RS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWT_Client_RS_Options then
  begin
    PrivateKey.Text := TsgcHTTP_JWT_Client_RS_Options(aSource).PrivateKey.Text;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_JWT_Client_RS_Options.SetPrivateKey(const Value:
    TStringList);
begin
  if Assigned(FPrivateKey) then
    FPrivateKey.Assign(Value);
end;

constructor TsgcHTTP_JWT_Client_ES_Options.Create;
begin
  inherited;
  FPrivateKey := TStringList.Create;
end;

destructor TsgcHTTP_JWT_Client_ES_Options.Destroy;
begin
  sgcFree(FPrivateKey);
  inherited;
end;

procedure TsgcHTTP_JWT_Client_ES_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWT_Client_ES_Options then
  begin
    PrivateKey.Text := TsgcHTTP_JWT_Client_ES_Options(aSource).PrivateKey.Text;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_JWT_Client_ES_Options.SetPrivateKey(const Value:
    TStringList);
begin
  if Assigned(FPrivateKey) then
    FPrivateKey.Assign(Value);
end;

constructor TsgcHTTP_JWT_Client_Algorithms_Options.Create;
begin
  inherited;
  FES := TsgcHTTP_JWT_Client_ES_Options.Create;
  FHS := TsgcHTTP_JWT_Client_HS_Options.Create;
  FRS := TsgcHTTP_JWT_Client_RS_Options.Create;
end;

destructor TsgcHTTP_JWT_Client_Algorithms_Options.Destroy;
begin
  sgcFree(FES);
  sgcFree(FHS);
  sgcFree(FRS);
  inherited;
end;

procedure TsgcHTTP_JWT_Client_Algorithms_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWT_Client_Algorithms_Options then
  begin
    FES.Assign(TsgcHTTP_JWT_Client_Algorithms_Options(aSource).ES);
    FHS.Assign(TsgcHTTP_JWT_Client_Algorithms_Options(aSource).HS);
    FRS.Assign(TsgcHTTP_JWT_Client_Algorithms_Options(aSource).RS);
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_JWT_Client_Algorithms_Options.SetES(const Value:
    TsgcHTTP_JWT_Client_ES_Options);
begin
  if Assigned(FES) then
    FES.Assign(Value);
end;

procedure TsgcHTTP_JWT_Client_Algorithms_Options.SetHS(const Value:
    TsgcHTTP_JWT_Client_HS_Options);
begin
  if Assigned(FHS) then
    FHS.Assign(Value);
end;

procedure TsgcHTTP_JWT_Client_Algorithms_Options.SetRS(const Value:
    TsgcHTTP_JWT_Client_RS_Options);
begin
  if Assigned(FRS) then
    FRS.Assign(Value);
end;

{$ENDIF}

end.
