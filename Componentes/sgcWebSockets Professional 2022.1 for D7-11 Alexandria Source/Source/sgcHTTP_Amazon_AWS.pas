{ ***************************************************************************
  sgcHTTP Amazon component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Amazon_AWS;

interface

{$I sgcVer.inc}

{$IFDEF SGC_AWS}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Classes, sgcHTTP_Client, sgcTCP_Classes,
  sgcHTTP_Amazon_AWS_Signature;

type

  TsgcHTTP_Amazon_AWS_Base = class(TsgcComponent_Base)
  end;

  TsgcAWSLogFile = class(TsgcTCPLogFile)
  end;

  TsgcHTTP_Amazon_AWS_Options = class(TPersistent)
  private
    FAccessKey: String;
    FRegion: String;
    FSecretKey: String;
    FService: String;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Service: String read FService write FService;
  published
    property AccessKey: String read FAccessKey write FAccessKey;
    property Region: String read FRegion write FRegion;
    property SecretKey: String read FSecretKey write FSecretKey;
  end;

  TsgcHTTP_Amazon_AWS_Client = class(TsgcHTTP_Amazon_AWS_Base)
  { aws signature v4 }
  private
    FSignature: TsgcHTTP_Amazon_AWS_Signature_V4;
    function GetSignature: TsgcHTTP_Amazon_AWS_Signature_V4;
  private
    procedure DoLogOptions;
    procedure DoSignatureOptions;
  private
    procedure DoAuthorizedRequest(const aURL: String);
  protected
    property Signature: TsgcHTTP_Amazon_AWS_Signature_V4 read GetSignature write FSignature;
  protected
    function DoBeforeGET_Request(var aURL: String): Boolean; virtual;
  protected
    procedure DoGetAuthorization(const aURL: String); virtual;
    procedure DoPostAuthorization(const aURL: String); virtual;
  public
    function GetURL(const aURL: String; var aResponse, aError: String): Boolean;
    function PostURL(const aURL: String; var aResponse, aError: String): Boolean;
  { aws signature v4 }

  { http client }
  private
    FHTTPClient: TsgcIdHTTP;
    function GetHTTPClient: TsgcIdHTTP;
  protected
    property HTTPClient: TsgcIdHTTP read GetHTTPClient write FHTTPClient;
  { http client }

  { aws options }
  private
    FAWSOptions: TsgcHTTP_Amazon_AWS_Options;
    procedure SetAWSOptions(const Value: TsgcHTTP_Amazon_AWS_Options);
  public
    property AWSOptions: TsgcHTTP_Amazon_AWS_Options read FAWSOptions write
        SetAWSOptions;
  { aws options }

  { log file }
  private
    FLogFile: TsgcAWSLogFile;
    procedure SetLogFile(const Value: TsgcAWSLogFile);
  public
    property LogFile: TsgcAWSLogFile read FLogFile write SetLogFile;
  { log file }

  { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor / destructor }

  { properties }
  private
    function GetEndPoint: String;
  public
    property EndPoint: String read GetEndPoint;
  { properties }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_AWS}

uses
  // indy
{$IFDEF SGC_INDY}sgcIdHttp{$ELSE}IdHttp{$ENDIF},
  sgcBase_Helpers, sgcHTTP_Const, sgcWebSocket_Types;

constructor TsgcHTTP_Amazon_AWS_Client.Create(aOwner: TComponent);
begin
  inherited;
  FAWSOptions := TsgcHTTP_Amazon_AWS_Options.Create;
  FLogFile := TsgcAWSLogFile.Create;
end;

destructor TsgcHTTP_Amazon_AWS_Client.Destroy;
begin
  sgcFree(FLogFile);
  sgcFree(FSignature);
  sgcFree(FAWSOptions);
  sgcFree(FHTTPClient);
  inherited;
end;

procedure TsgcHTTP_Amazon_AWS_Client.DoGetAuthorization(const aURL: String);
begin
  DoLogOptions;
  DoSignatureOptions;
  Signature.SignGET(aURL);
  DoAuthorizedRequest(aURL);
end;

procedure TsgcHTTP_Amazon_AWS_Client.DoAuthorizedRequest(const aURL: String);
begin
  HTTPClient.Request.CustomHeaders.Clear;
  HTTPClient.Request.CustomHeaders.Add(CS_HTTP_HEADER_AWS_DATE + ':' + Signature.RequestDate);
  HTTPClient.Request.CustomHeaders.Add(Signature.AuthorizationHeader);
  HTTPClient.Request.ContentType := CS_HTTP_HEADER_AWS_CONTENT_TYPE;
  HTTPClient.Request.Accept := '';
end;

function TsgcHTTP_Amazon_AWS_Client.DoBeforeGET_Request(var aURL: String):
    Boolean;
begin
  Result := True;
end;

procedure TsgcHTTP_Amazon_AWS_Client.DoLogOptions;
begin
  HTTPClient.LogOptions.FileName := LogFile.FileName;
  HTTPClient.Log := LogFile.Enabled;
end;

procedure TsgcHTTP_Amazon_AWS_Client.DoPostAuthorization(const aURL: String);
begin
  DoLogOptions;
  DoSignatureOptions;
//  Signature.SignPOST(aURL);
  DoAuthorizedRequest(aURL);
end;

procedure TsgcHTTP_Amazon_AWS_Client.DoSignatureOptions;
begin
  Signature.SignatureV4Options.Region := AWSOptions.Region;
  Signature.SignatureV4Options.Service := AWSOptions.Service;
  Signature.SignatureV4Options.AccessKey := AWSOptions.AccessKey;
  Signature.SignatureV4Options.SecretKey := AWSOptions.SecretKey;
end;

function TsgcHTTP_Amazon_AWS_Client.GetEndPoint: String;
begin
  Result := 'https://' + AWSOptions.Service + '.' + AWSOptions.Region + '.amazonaws.com';
end;

function TsgcHTTP_Amazon_AWS_Client.GetHTTPClient: TsgcIdHTTP;
begin
  if not Assigned(FHTTPClient) then
  begin
    FHTTPClient := TsgcIdHTTP.Create(nil);
    FHTTPClient.TLSOptions.Version := tls1_2;
  end;
  Result := FHTTPClient;
end;

function TsgcHTTP_Amazon_AWS_Client.GetSignature:
    TsgcHTTP_Amazon_AWS_Signature_V4;
begin
  if not Assigned(FSignature) then
    FSignature := TsgcHTTP_Amazon_AWS_Signature_V4.Create(nil);
  Result := FSignature;
end;

function TsgcHTTP_Amazon_AWS_Client.GetURL(const aURL: String; var aResponse,
    aError: String): Boolean;
var
  oStream: TStringStream;
  vURL: String;
begin
  result := False;
  aResponse := '';
  aError := '';

  DoGetAuthorization(aURL);

  oStream := TStringStream.Create('');
  Try
    Try
      vURL := aURL;
      if DoBeforeGET_Request(vURL) then
      begin
        HTTPClient.Get(vURL, oStream);
        aResponse := oStream.DataString;
        result := True;
      end;
    Except
      On E: EIdHTTPProtocolException Do
        aError := E.ErrorMessage;
      On E: Exception do
        raise;
    End;
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcHTTP_Amazon_AWS_Client.PostURL(const aURL: String; var aResponse,
    aError: String): Boolean;
begin
  result := False;
end;

procedure TsgcHTTP_Amazon_AWS_Client.SetAWSOptions(const Value:
    TsgcHTTP_Amazon_AWS_Options);
begin
  if Assigned(FAWSOptions) then
    FAWSOptions.Assign(Value);
end;

procedure TsgcHTTP_Amazon_AWS_Client.SetLogFile(const Value: TsgcAWSLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

procedure TsgcHTTP_Amazon_AWS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_Amazon_AWS_Options then
  begin
    Region := TsgcHTTP_Amazon_AWS_Options(aSource).Region;
    Service := TsgcHTTP_Amazon_AWS_Options(aSource).Service;
    AccessKey := TsgcHTTP_Amazon_AWS_Options(aSource).AccessKey;
    SecretKey := TsgcHTTP_Amazon_AWS_Options(aSource).SecretKey;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
