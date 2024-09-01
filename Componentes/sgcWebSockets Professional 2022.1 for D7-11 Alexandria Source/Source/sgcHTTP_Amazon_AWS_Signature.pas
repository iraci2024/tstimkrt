{ ***************************************************************************
  sgcHTTP Amazon component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Amazon_AWS_Signature;

interface

{$I sgcVer.inc}
{$IFDEF SGC_AWS}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcBase_Helpers, sgcBase_Classes, sgcWebSocket_Types;

Type
  TsgcHTTP_SignatureV4_Protocol = (sv4pHTTP, sv4pMQTT);

  TsgcHTTP_Amazon_AWS_Signature = class(TsgcComponent_Base)
    { HMAC SHA256 }
  private
    {$IFDEF D10}
    function GetHMACSHA256_Hash(const aValue: String; aSecret: TBytes): TBytes;
    {$ENDIF}
    function GetHMACSHA256_Indy(const aValue: String;
      aSecret: TIdBytes): TBytes;
  protected
    function GetHMACSHA256(const aValue: String; aSecret: TBytes): TBytes; virtual;
    { HMAC SHA256 }

    { HASH SHA256 }
  private
    {$IFDEF D10}
    function GetHASHSHA256_Hash(const aValue: String): String;
    {$ENDIF}
    function GetHASHSHA256_Indy(const aValue: String): String;
  protected
    function GetHASHSHA256(const aValue: String): String; virtual;
    { HASH SHA256 }
  end;

  TsgcHTTP_Amazon_AWS_Signature_V4_Options = class(TPersistent)
  private
    FAccessKey: String;
    FRegion: String;
    FSecretKey: String;
    FService: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AccessKey: String read FAccessKey write FAccessKey;
    property Region: String read FRegion write FRegion;
    property SecretKey: String read FSecretKey write FSecretKey;
    property Service: String read FService write FService;
  end;

  TsgcHTTP_Amazon_AWS_Signature_V4 = class(TsgcHTTP_Amazon_AWS_Signature)
    { options }
  private
    FSignatureV4Options: TsgcHTTP_Amazon_AWS_Signature_V4_Options;
    procedure SetSignatureV4Options(const Value
      : TsgcHTTP_Amazon_AWS_Signature_V4_Options);
  public
    property SignatureV4Options: TsgcHTTP_Amazon_AWS_Signature_V4_Options
      read FSignatureV4Options write SetSignatureV4Options;
    { options }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  private
    FRequestDate: String;
    FSignedHeaders: String;
    FHashedCanonicalRequest: String;
    FStringToSign: String;
    FSignature: String;
    FProtocol: TsgcHTTP_SignatureV4_Protocol;
  private
    function GetCanonicalRequest(const aHTTPRequestMethod, aCanonicalURI,
      aCanonicalQueryString, aCanonicalHeaders, aSignedHeaders, aRequestPayload
      : String): string;
  private
    procedure DoCanonicalRequest(const aHTTPRequestMethod, aURL,
      aRequestPayload: String);
    procedure DoStringToSign(const aRegion, aService: String);
    procedure DoCalculateSignature(const aRegion, aService: String);
  protected
    procedure DoSign(const aRegion, aService, aHTTPRequestMethod, aURL,
      aRequestPayload: String); virtual;
  protected
    function GetAuthorizationHeader: String; virtual;
    function GetSignature: String; virtual;
  public
    procedure SignGET(const aURL: string);
    procedure SignPOST(const aURL, aRequestPayload: string);
  public
    property RequestDate: String read FRequestDate;
    property AuthorizationHeader: String read GetAuthorizationHeader;
    property Signature: String read GetSignature;
  public
    property Protocol: TsgcHTTP_SignatureV4_Protocol read FProtocol write FProtocol;
    { methods }
  end;

const
  CS_AWS_SIGV4_ALGORITHM = 'AWS4-HMAC-SHA256';

{$ENDIF}

implementation

{$IFDEF SGC_AWS}

uses
  StrUtils,
{$IFDEF DXE8}System.Hash, {$ENDIF}
{$IFDEF INDY10_5_7}{$IFDEF SGC_INDY}sgcIdHashSHA{$ELSE}IdHashSHA{$ENDIF},
{$ENDIF}
{$IFDEF INDY10_2}{$IFDEF SGC_INDY}sgcIdHMAC{$ELSE}IdHMAC{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHMACSHA1{$ELSE}IdHMACSHA1{$ENDIF}, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF};

function TsgcHTTP_Amazon_AWS_Signature.GetHASHSHA256(const aValue
  : String): String;
begin
  Try
    result := GetHASHSHA256_Indy(aValue);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHASHSHA256_Hash(aValue);
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

{$IFDEF D10}
function TsgcHTTP_Amazon_AWS_Signature.GetHASHSHA256_Hash
  (const aValue: String): String;
var
  oHashSHA: THashSHA2;
begin
  oHashSHA := THashSHA2.Create;
  Try
    oHashSHA.GetHashString(aValue);
    result := LowerCase(oHashSHA.GetHashString(aValue, SHA256));
  Finally
    {$IFDEF D10_4}
    FillChar(oHashSHA, SizeOf(oHashSHA), 0);
    {$ELSE}
    sgcFree(oHashSHA);
    {$ENDIF}
  End;
end;
{$ENDIF}

function TsgcHTTP_Amazon_AWS_Signature.GetHASHSHA256_Indy
  (const aValue: String): String;
{$IFDEF INDY10_5_7}
var
  oSHA256: TIdHashSHA256;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  oSHA256 := TIdHashSHA256.Create;
  Try
    result := LowerCase(oSHA256.HashStringAsHex(aValue));
  Finally
    sgcFree(oSHA256);
  End;
{$ELSE}
  raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

function TsgcHTTP_Amazon_AWS_Signature.GetHMACSHA256(const aValue: String;
    aSecret: TBytes): TBytes;
begin
  Try
    result := GetHMACSHA256_Indy(aValue, TIdBytes(aSecret));
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHMACSHA256_Hash(aValue, aSecret);
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

{$IFDEF D10}
function TsgcHTTP_Amazon_AWS_Signature.GetHMACSHA256_Hash(const aValue: String;
  aSecret: TBytes): TBytes;
var
  oHash: TBytes;
begin
  oHash := THashSHA2.GetHMACAsBytes(aValue, aSecret);
  result := Copy(oHash, 0, Length(oHash));
end;
{$ENDIF}

function TsgcHTTP_Amazon_AWS_Signature.GetHMACSHA256_Indy(const aValue: String;
  aSecret: TIdBytes): TBytes;
{$IFDEF INDY10_5_7}
var
  oHMAC: TIdHMACSHA256;
  oHash: TIdBytes;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  oHMAC := TIdHMACSHA256.Create;
  try
    oHMAC.Key := aSecret;
    oHash := oHMAC.HashValue({$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.GetBytes(aValue));
    result := TBytes(Copy(oHash, 0, Length(oHash)));
  finally
    oHMAC.Free;
  end;
{$ELSE}
    raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

constructor TsgcHTTP_Amazon_AWS_Signature_V4.Create(aOwner: TComponent);
begin
  inherited;
  FSignatureV4Options := TsgcHTTP_Amazon_AWS_Signature_V4_Options.Create;
  FProtocol := sv4pHTTP;
end;

destructor TsgcHTTP_Amazon_AWS_Signature_V4.Destroy;
begin
  sgcFree(FSignatureV4Options);
  inherited;
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.DoCalculateSignature(const aRegion,
  aService: String);
var
  kDate, kRegion, kService, kSigning: TBytes;
begin
  {$IFDEF INDY10_5_5}
  kDate := GetHMACSHA256(LeftStr(FRequestDate, 8),
    TBytes({$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.GetBytes('AWS4' +
    SignatureV4Options.SecretKey)));
  {$ELSE}
  kDate := GetHMACSHA256(LeftStr(FRequestDate, 8),
    TBytes(ToBytes(UTF8Encode('AWS4' +
    SignatureV4Options.SecretKey))));
  {$ENDIF}
  kRegion := GetHMACSHA256(SignatureV4Options.Region, kDate);
  kService := GetHMACSHA256(SignatureV4Options.Service, kRegion);
  kSigning := GetHMACSHA256('aws4_request', kService);

  FSignature := LowerCase(ToHex(TIdBytes(GetHMACSHA256(FStringToSign,
    kSigning))));
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.DoCanonicalRequest
  (const aHTTPRequestMethod, aURL, aRequestPayload: String);
var
  oURI: TIdURI;
  vHeaders: String;
begin
  FHashedCanonicalRequest := '';
  FRequestDate := sgcGetUTCString;

  oURI := TIdURI.Create(aURL);
  Try
    case Protocol of
      sv4pHTTP:
        begin
          vHeaders :=
            'content-type:application/x-www-form-urlencoded; charset=utf-8' + #10;
          vHeaders := vHeaders + 'host:' + oURI.Host + #10;
          vHeaders := vHeaders + 'x-amz-date:' + FRequestDate + #10;
          FSignedHeaders := 'content-type;host;x-amz-date';
        end;
      sv4pMQTT:
        begin
          vHeaders := 'host:' + oURI.Host + #10;
          oURI.Path := '/mqtt';
          FSignedHeaders := 'host';
        end;
    end;

    FHashedCanonicalRequest := GetCanonicalRequest(aHTTPRequestMethod,
      oURI.Path, oURI.Params, vHeaders, FSignedHeaders, aRequestPayload);
  Finally
    sgcFree(oURI);
  End;
end;

function TsgcHTTP_Amazon_AWS_Signature_V4.GetCanonicalRequest
  (const aHTTPRequestMethod, aCanonicalURI, aCanonicalQueryString,
  aCanonicalHeaders, aSignedHeaders, aRequestPayload: String): string;
var
  vRequest: String;
begin
  vRequest := aHTTPRequestMethod + #10;
  vRequest := vRequest + aCanonicalURI + #10;
  vRequest := vRequest + aCanonicalQueryString + #10;
  vRequest := vRequest + aCanonicalHeaders + #10;
  vRequest := vRequest + aSignedHeaders + #10;
  vRequest := vRequest + GetHASHSHA256(aRequestPayload);

  result := GetHASHSHA256(vRequest);
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.DoSign(const aRegion, aService,
  aHTTPRequestMethod, aURL, aRequestPayload: String);
begin
  DoCanonicalRequest(aHTTPRequestMethod, aURL, aRequestPayload);
  DoStringToSign(aRegion, aService);
  DoCalculateSignature(aRegion, aService);
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.SignGET(const aURL: string);
begin
  DoSign(SignatureV4Options.Region, SignatureV4Options.Service, 'GET',
    aURL, '');
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.SignPOST(const aURL,
    aRequestPayload: string);
begin
  DoSign(SignatureV4Options.Region, SignatureV4Options.Service, 'POST', aURL,
    aRequestPayload);
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.DoStringToSign(const aRegion,
  aService: String);
begin
  FStringToSign := CS_AWS_SIGV4_ALGORITHM + #10;
  FStringToSign := FStringToSign + FRequestDate + #10;
  FStringToSign := FStringToSign + LeftStr(FRequestDate, 8) + '/' + aRegion +
    '/' + aService + '/aws4_request' + #10;
  FStringToSign := FStringToSign + FHashedCanonicalRequest;
end;

function TsgcHTTP_Amazon_AWS_Signature_V4.GetAuthorizationHeader: String;
begin
  result := 'Authorization: ' + CS_AWS_SIGV4_ALGORITHM + ' ' + 'Credential=' +
    SignatureV4Options.AccessKey + '/' + LeftStr(FRequestDate, 8) + '/' +
    SignatureV4Options.Region + '/' + SignatureV4Options.Service +
    '/aws4_request, ' + 'SignedHeaders=' + FSignedHeaders + ', ' + 'Signature='
    + FSignature;
end;

function TsgcHTTP_Amazon_AWS_Signature_V4.GetSignature: String;
begin
  result := FSignature;
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4.SetSignatureV4Options
  (const Value: TsgcHTTP_Amazon_AWS_Signature_V4_Options);
begin
  if Assigned(FSignatureV4Options) then
    FSignatureV4Options.Assign(Value);
end;

procedure TsgcHTTP_Amazon_AWS_Signature_V4_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_Amazon_AWS_Signature_V4_Options then
  begin
    Region := TsgcHTTP_Amazon_AWS_Signature_V4_Options(aSource).Region;
    Service := TsgcHTTP_Amazon_AWS_Signature_V4_Options(aSource).Service;
    AccessKey := TsgcHTTP_Amazon_AWS_Signature_V4_Options(aSource).AccessKey;
    SecretKey := TsgcHTTP_Amazon_AWS_Signature_V4_Options(aSource).SecretKey;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
