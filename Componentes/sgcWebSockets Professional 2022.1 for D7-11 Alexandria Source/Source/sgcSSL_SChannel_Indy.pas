{ ***************************************************************************
  sgcSSL component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcSSL_SChannel_Indy;

interface

{$I sgcVer.inc}
{$IFDEF SGC_SCHANNEL}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomTransparentProxy{$ELSE}IdCustomTransparentProxy{$ENDIF},
  // sgcSSL
  sgcSSL_SChannel,
  // websocket
  sgcWebSocket_Types;

type
  SChannelError = class(Exception)
  public
    CertStatus: Cardinal;
    constructor Create(AMsg: string; AStatus: Cardinal);
  end;

  TCredentialsEvent = procedure(Sender: TObject) of object;

  TsgcIdSSLIOHandlerSocketSChannel = class(TIdSSLIOHandlerSocketBase)
  private
    FALPNProtocols: TStringList;
    FSSL: THandle;
    FOnCredentials: TCredentialsEvent;
    FCertificate: TsgcSChannelCertificate;
    FCipherList: string;
    FTLSVersion: TwsTLSVersions;
    procedure SetCredentials(Value: TCredentialsEvent);
    procedure ConnectSSL;
    procedure CloseSSL;
    function GetALPNProtocols: TStringList;
    function GetTargetHost: string;
    procedure SetALPNProtocols(const Value: TStringList);
    procedure SetCertificate(const Value: TsgcSChannelCertificate);
  protected
    procedure SetPassThrough(const Value: Boolean); override;
    function RecvEnc(var ABuffer: TIdBytes): Integer; override;
    function SendEnc(const ABuffer: TIdBytes; const AOffset, ALength: Integer)
      : Integer; override;
    procedure ConnectClient; override;
  public
    constructor Create(aOwner: TComponent); virtual;
    destructor Destroy; override;
  public
    function Clone: TIdSSLIOHandlerSocketBase; override;
    procedure StartSSL; override;
    procedure Close; override;
    function Connected: Boolean; override;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;
    function GetALPNProtocol: String;
    function GetInfo: TsgcSChannelConnectionInfo;
    property OnCredentials: TCredentialsEvent read FOnCredentials
      write SetCredentials;
  public
    property ALPNProtocols: TStringList read GetALPNProtocols write
        SetALPNProtocols;
    property Certificate: TsgcSChannelCertificate read FCertificate write
        SetCertificate;
    property CipherList: string read FCipherList write FCipherList;
    property TLSVersion: TwsTLSVersions read FTLSVersion write FTLSVersion;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_SCHANNEL}

uses
  sgcBase_Helpers;

function TsgcIdSSLIOHandlerSocketSChannel.Clone: TIdSSLIOHandlerSocketBase;
begin
  Result := TsgcIdSSLIOHandlerSocketSChannel.Create(nil);
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.CloseSSL;
begin
  if FSSL <> 0 then
  begin
    SSLClose(FSSL);
    FSSL := 0;
  end;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.Close;
begin
  inherited;
  // close socket first and then free objects
  CloseSSL;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.ConnectClient;
var
  LPassThrough: Boolean;
begin
  LPassThrough := fPassThrough;
  fPassThrough := True;
  try
    inherited ConnectClient;
  finally
    fPassThrough := LPassThrough;
  end;
  StartSSL;
end;

function TsgcIdSSLIOHandlerSocketSChannel.Connected: Boolean;
begin
  if Passthrough then
    Result := inherited Connected
  else
    Result := FSSL <> 0;
end;

function TsgcIdSSLIOHandlerSocketSChannel.Readable(AMSec: Integer): Boolean;
begin
  if (FSSL <> 0) and (Passthrough = False) then
    Result := True
  else
    Result := inherited Readable(AMSec);
end;

function TsgcIdSSLIOHandlerSocketSChannel.RecvEnc
  (var ABuffer: TIdBytes): Integer;
begin
  Result := SSLRead(FSSL, ABuffer[0], Length(ABuffer));
  // ... avoid 10004 error
  if Result = CS_SChannel_ERROR_INTERRUPTED_SYSTEM_CALL * (-1) then
    Result := 0;
end;

function TsgcIdSSLIOHandlerSocketSChannel.SendEnc(const ABuffer: TIdBytes;
  const AOffset, ALength: Integer): Integer;
var
  vOfs: Integer;
  vLen: Integer;
  vCnt: Integer;
begin
  vOfs := AOffset;
  vLen := ALength;
  while vLen > 0 do
  begin
    vCnt := SSLWrite(FSSL, ABuffer[vOfs], vLen);
    if vCnt <= 0 then
    begin
      result := vCnt;
      Exit;
    end;
    Inc(vOfs, vCnt);
    Dec(vLen, vCnt);
  end;
  Result := vOfs - AOffset;
end;

procedure DoCredentials(SSL: THandle; UserData: Pointer);
begin
  with TsgcIdSSLIOHandlerSocketSChannel(UserData) do
  begin
    if Assigned(FOnCredentials) then
      FOnCredentials(TsgcIdSSLIOHandlerSocketSChannel(UserData));
  end;
end;

constructor TsgcIdSSLIOHandlerSocketSChannel.Create(aOwner: TComponent);
begin
  inherited;
  TLSVersion := tls1_0;
  FCertificate := TsgcSChannelCertificate.Create;
end;

destructor TsgcIdSSLIOHandlerSocketSChannel.Destroy;
begin
  sgcFree(FALPNProtocols);
  sgcFree(FCertificate);
  inherited;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.SetCredentials
  (Value: TCredentialsEvent);
begin
  FOnCredentials := Value;
  if FSSL <> 0 then
  begin
    if Assigned(FOnCredentials) then
      SSLCredentialsCallBack(FSSL, DoCredentials, Self)
    else
      SSLCredentialsCallBack(FSSL, nil, nil);
  end;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.SetPassThrough(const Value: Boolean);
begin
  if fPassThrough <> Value then
  begin
    if not Value then
    begin
      if BindingAllocated then
      begin
        ConnectSSL;
      end;
    end;
    fPassThrough := Value;
  end;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.StartSSL;
begin
  if not Passthrough then
    ConnectSSL;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.ConnectSSL;
var
  aHost: AnsiString;
begin
  aHost := AnsiString(GetTargetHost);
  FSSL := SSLStart(Binding.Handle, Certificate, aHost, TLSVersion, ALPNProtocols, CipherList);
  if FSSL = 0 then
    raise SChannelError.Create('SChannel initialization error: ' + vMsgError,
      CertStatus);
  if Assigned(FOnCredentials) then
    SSLCredentialsCallBack(FSSL, DoCredentials, Self);
end;

function TsgcIdSSLIOHandlerSocketSChannel.GetALPNProtocol: String;
begin
  Result := GetALPN(FSSL);
end;

function TsgcIdSSLIOHandlerSocketSChannel.GetALPNProtocols: TStringList;
begin
  if not Assigned(FALPNProtocols) then
    FALPNProtocols := TStringList.Create;
  Result := FALPNProtocols;
end;

function TsgcIdSSLIOHandlerSocketSChannel.GetInfo: TsgcSChannelConnectionInfo;
begin
  Result := GetConnectionInfo(FSSL);
end;

function TsgcIdSSLIOHandlerSocketSChannel.GetTargetHost: string;
var
  oURI: TIdURI;
  oTransparentProxy, oNextTransparentProxy: TIdCustomTransparentProxy;
begin
  Result := '';

  if URIToCheck <> '' then
  begin
    oURI := TIdURI.Create(URIToCheck);
    try
      Result := oURI.Host;
    finally
      oURI.Free;
    end;
    if Result <> '' then
      Exit;
  end;

  oTransparentProxy := FTransparentProxy;
  if Assigned(oTransparentProxy) then
  begin
    if oTransparentProxy.Enabled then
    begin
      repeat
        oNextTransparentProxy := oTransparentProxy.ChainedProxy;
        if not Assigned(oNextTransparentProxy) then
          Break;
        if not oNextTransparentProxy.Enabled then
          Break;
        oTransparentProxy := oNextTransparentProxy;
      until False;
      Result := oTransparentProxy.Host;
      if Result <> '' then
        Exit;
    end;
  end;

  Result := Host;
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.SetALPNProtocols(const Value:
    TStringList);
begin
  if Assigned(FALPNProtocols) then
    FALPNProtocols.Assign(Value);
end;

procedure TsgcIdSSLIOHandlerSocketSChannel.SetCertificate(const Value:
    TsgcSChannelCertificate);
begin
  if Assigned(FCertificate) then
    FCertificate.Assign(Value);
end;

{ SChannelError }

constructor SChannelError.Create(AMsg: string; AStatus: Cardinal);
begin
  CertStatus := AStatus;
  inherited Create(AMsg);
end;

{$ENDIF}

end.
