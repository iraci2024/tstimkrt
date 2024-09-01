{ ***************************************************************************
  sgcIoT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcIoT_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_IOT}

uses
  Classes, SysUtils,
  // Indy
  {$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
  // sgc
  sgcWebSocket_Classes, sgcIoT_Classes, sgcWebSocket_Client,
  sgcWebSocket_Types, sgcSocket_Classes;

type
  TsgcIoTConnectEvent = TsgcWSConnectEvent;
  TsgcIoTDisconnectEvent = TsgcWSDisconnectEvent;
  TsgcIoTErrorEvent = TsgcWSErrorEvent;
  TsgcIoTExceptionEvent = TsgcExceptionEvent;

  TsgcIoTComponent_Client_Certificate = class(TPersistent)
  private
    FCertFile: String;
    FEnabled: Boolean;
    FIOHandler: TwsTLSIOHandler;
    FKeyFile: String;
    FOpenSSL_Options: TsgcOpenSSLClient_Options;
    FPassword: {$IFDEF INDY10_6}String{$ELSE}AnsiString{$ENDIF};
    FSChannel_Options: TsgcSChannelClient_Options;
    FVersion: TwsTLSVersions;
    procedure SetOpenSSL_Options(const Value: TsgcOpenSSLClient_Options);
    procedure SetSChannel_Options(const Value: TsgcSChannelClient_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property CertFile: String read FCertFile write FCertFile;
    property KeyFile: String read FKeyFile write FKeyFile;
    property Password: {$IFDEF INDY10_6}String{$ELSE}AnsiString{$ENDIF} read FPassword write FPassword;
    property IOHandler: TwsTLSIOHandler read FIOHandler write FIOHandler;
    property OpenSSL_Options: TsgcOpenSSLClient_Options read FOpenSSL_Options write
        SetOpenSSL_Options;
    property SChannel_Options: TsgcSChannelClient_Options read FSChannel_Options
        write SetSChannel_Options;
    property Version: TwsTLSVersions read FVersion write FVersion;
  end;

  TsgcIoTComponent_Client_SAS = class(TPersistent)
  private
    FEnabled: Boolean;
    FExpiry: Integer;
    FSecretKey: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Expiry: Integer read FExpiry write FExpiry;
    property SecretKey: String read FSecretKey write FSecretKey;
  end;

  TsgcIoT_WSClient = class(TsgcWSClient)
  private
    FOnBeforeWatchDog: TNotifyEvent;
  protected
    procedure OnWatchDogEvent(Sender: TObject); override;
  public
    property OnBeforeWatchDog: TNotifyEvent read FOnBeforeWatchDog write
        FOnBeforeWatchDog;
  end;

  TsgcIoTComponent_Client = class(TsgcIoTComponent_Base_Client)
    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

  { TCP Client }
  private
    FTCPClient: TsgcIoT_WSClient;
    function GetTCPClient: TsgcIoT_WSClient;
  protected
    procedure OnConnectEvent(Connection: TsgcWSConnection); virtual;
    procedure OnDisconnectEvent(Connection: TsgcWSConnection; Code: Integer);
        virtual;
    procedure OnErrorEvent(Connection: TsgcWSConnection; const Error: string); virtual;
    procedure OnExceptionEvent(Connection: TsgcWSConnection; E: Exception); virtual;
    procedure OnBeforeWatchDogEvent(Sender: TObject); virtual;
  protected
    property TCPClient: TsgcIoT_WSClient read GetTCPClient write FTCPClient;
  { TCP Client }

  { HeartBeat }
  private
    FHeartBeat: TsgcWSHeartBeat_Options;
    procedure SetHeartBeat(const Value: TsgcWSHeartBeat_Options);
  protected
    property HeartBeat: TsgcWSHeartBeat_Options read FHeartBeat write SetHeartBeat;
  { HeartBeat }

  { WatchDog }
  private
    FWatchDog: TsgcWSWatchDogClient_Options;
    procedure SetWatchDog(const Value: TsgcWSWatchDogClient_Options);
  protected
    property WatchDog: TsgcWSWatchDogClient_Options read FWatchDog write
        SetWatchDog;
  { WatchDog }

  { LogFile }
  private
    FLogFile: TsgcWSLogFile;
    procedure SetLogFile(const Value: TsgcWSLogFile);
  protected
    property LogFile: TsgcWSLogFile read FLogFile write SetLogFile;
  { LogFile }

  { Certificate }
  private
    FCertificate: TsgcIoTComponent_Client_Certificate;
    function GetCertificate: TsgcIoTComponent_Client_Certificate;
    procedure SetCertificate(const Value: TsgcIoTComponent_Client_Certificate);
  protected
    property Certificate: TsgcIoTComponent_Client_Certificate read GetCertificate
        write SetCertificate;
  { Certificate }

  { sas }
  private
    FSAS: TsgcIoTComponent_Client_SAS;
    function GetSAS: TsgcIoTComponent_Client_SAS;
    procedure SetSAS(const Value: TsgcIoTComponent_Client_SAS);
  protected
    property SAS: TsgcIoTComponent_Client_SAS read GetSAS write SetSAS;
  { sas }

  { properties }
  private
    FActive: Boolean;
    function GetActive: Boolean;
    procedure SetURL(const Value: String);
    procedure SetActive(const Value: Boolean);
  protected
    procedure DoBeforeConnect; virtual;
  protected
    property URL: String write SetURL;
  public
    property Active: Boolean read GetActive write SetActive;
  { properties }

  { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor / destructor }

  { events }
  protected
    FOnConnect: TsgcIoTConnectEvent;
    FOnDisconnect: TsgcIoTDisconnectEvent;
    FOnError: TsgcIoTErrorEvent;
    FOnException: TsgcIoTExceptionEvent;
  protected
    property OnConnect: TsgcIoTConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TsgcIoTDisconnectEvent read FOnDisconnect write
        FOnDisconnect;
    property OnError: TsgcIoTErrorEvent read FOnError write FOnError;
    property OnException: TsgcIoTExceptionEvent read FOnException write
        FOnException;
  { events }
  end;


{$ENDIF}

implementation

{$IFDEF SGC_IOT}

uses
  sgcBase_Helpers;

constructor TsgcIoTComponent_Client.Create(aOwner: TComponent);
begin
  inherited;
  FCertificate := TsgcIoTComponent_Client_Certificate.Create;
  FCertificate.Enabled := False;
  FSAS := TsgcIoTComponent_Client_SAS.Create;
  FSAS.Enabled := False;
  FSAS.Expiry := 1440;
  FHeartBeat := TsgcWSHeartBeat_Options.Create;
  FHeartBeat.Enabled := False;
  FHeartBeat.Interval := 60;
  FHeartBeat.Timeout := 0;
  FWatchDog := TsgcWSWatchDogClient_Options.Create;
  FWatchDog.Enabled := False;
  FWatchDog.Interval := 10;
  FWatchDog.Attempts := 0;
  FLogFile := TsgcWSLogFile.Create;
  FLogFile.Enabled := False;
  FLogFile.FileName := '';
end;

destructor TsgcIoTComponent_Client.Destroy;
begin
  sgcFree(FLogFile);
  sgcFree(FWatchDog);
  sgcFree(FHeartBeat);
  sgcFree(FCertificate);
  sgcFree(FSAS);
  sgcFree(FTCPClient);
  inherited;
end;

procedure TsgcIoTComponent_Client.DoBeforeConnect;
begin
  // ... tls
  TCPClient.TLS := True;
  TCPClient.TLSOptions.CertFile := Certificate.CertFile;
  TCPClient.TLSOptions.KeyFile := Certificate.KeyFile;
  TCPClient.TLSOptions.Password := Certificate.Password;
  TCPClient.TLSOptions.Version := Certificate.Version;
  TCPClient.TLSOptions.IOHandler := Certificate.IOHandler;
  TCPClient.TLSOptions.OpenSSL_Options.Assign(Certificate.OpenSSL_Options);
  TCPClient.TLSOptions.SChannel_Options.Assign(Certificate.SChannel_Options);

  // ... heartbeat
  TCPClient.HeartBeat.Interval := HeartBeat.Interval;
  TCPClient.HeartBeat.Timeout := HeartBeat.Timeout;
  TCPClient.HeartBeat.Enabled := HeartBeat.Enabled;

  // ... watchdog
  TCPClient.WatchDog.Interval := WatchDog.Interval;
  TCPClient.WatchDog.Attempts := WatchDog.Attempts;
  TCPClient.WatchDog.Enabled := WatchDog.Enabled;

  // ... logfile
  TCPClient.LogFile.FileName := LogFile.FileName;
  TCPClient.LogFile.Enabled := LogFile.Enabled;
end;

function TsgcIoTComponent_Client.GetActive: Boolean;
begin
  if not IsDesigning and not IsLoading then
    Result := TCPClient.Active
  else
    Result := FActive;
end;

function TsgcIoTComponent_Client.GetCertificate:
    TsgcIoTComponent_Client_Certificate;
begin
  Result := FCertificate;
end;

function TsgcIoTComponent_Client.GetSAS: TsgcIoTComponent_Client_SAS;
begin
  Result := FSAS;
end;

function TsgcIoTComponent_Client.GetTCPClient: TsgcIoT_WSClient;
begin
  if not Assigned(FTCPClient) then
  begin
    FTCPClient := TsgcIoT_WSClient.Create(nil);
    FTCPClient.OnError := OnErrorEvent;
    FTCPClient.OnException := OnExceptionEvent;
    FTCPClient.OnBeforeWatchDog := OnBeforeWatchDogEvent;
  end;
  Result := FTCPClient;
end;

procedure TsgcIoTComponent_Client.Loaded;
begin
  inherited;
  if Active <> FActive then
    Active := FActive;
end;

procedure TsgcIoTComponent_Client.OnBeforeWatchDogEvent(Sender: TObject);
begin
  DoBeforeConnect;
end;

procedure TsgcIoTComponent_Client.OnConnectEvent(Connection: TsgcWSConnection);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Connection);
end;

procedure TsgcIoTComponent_Client.OnDisconnectEvent(Connection:
    TsgcWSConnection; Code: Integer);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Connection, Code);
end;

procedure TsgcIoTComponent_Client.OnErrorEvent(Connection: TsgcWSConnection;
    const Error: string);
begin
  if Assigned(FOnError) then
    FOnError(Connection, Error);
end;

procedure TsgcIoTComponent_Client.OnExceptionEvent(Connection:
    TsgcWSConnection; E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Connection, E);
end;

procedure TsgcIoTComponent_Client.SetActive(const Value: Boolean);
begin
  if not IsDesigning and not IsLoading then
  begin
    if Value then
      DoBeforeConnect;
    TCPClient.Active := Value;
  end
  else
    FActive := Value;
end;

procedure TsgcIoTComponent_Client.SetCertificate(const Value:
    TsgcIoTComponent_Client_Certificate);
begin
  FCertificate.Assign(Value);
end;

procedure TsgcIoTComponent_Client.SetHeartBeat(const Value:
    TsgcWSHeartBeat_Options);
begin
  if Assigned(FHeartBeat) then
    FHeartBeat.Assign(Value);
end;

procedure TsgcIoTComponent_Client.SetLogFile(const Value: TsgcWSLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

procedure TsgcIoTComponent_Client.SetSAS(const Value:
    TsgcIoTComponent_Client_SAS);
begin
  if Assigned(FSAS) then
    FSAS.Assign(Value);
end;

procedure TsgcIoTComponent_Client.SetURL(const Value: String);
begin
  TCPClient.URL := Value;
end;

procedure TsgcIoTComponent_Client.SetWatchDog(const Value:
    TsgcWSWatchDogClient_Options);
begin
  if Assigned(FWatchDog) then
    FWatchDog.Assign(Value);
end;

constructor TsgcIoTComponent_Client_Certificate.Create;
begin
  inherited;
  FOpenSSL_Options := TsgcOpenSSLClient_Options.Create;
  FSChannel_Options := TsgcSChannelClient_Options.Create;
  IOHandler := iohOpenSSL;
end;

destructor TsgcIoTComponent_Client_Certificate.Destroy;
begin
  sgcFree(FSChannel_Options);
  sgcFree(FOpenSSL_Options);
  inherited;
end;

procedure TsgcIoTComponent_Client_Certificate.Assign(aSource: TPersistent);
begin
  if aSource is TsgcIoTComponent_Client_Certificate then
  begin
    Enabled := TsgcIoTComponent_Client_Certificate(aSource).Enabled;
    CertFile := TsgcIoTComponent_Client_Certificate(aSource).CertFile;
    KeyFile := TsgcIoTComponent_Client_Certificate(aSource).KeyFile;
    Password := TsgcIoTComponent_Client_Certificate(aSource).Password;
    Version := TsgcIoTComponent_Client_Certificate(aSource).Version;
    IOHandler := TsgcIoTComponent_Client_Certificate(aSource).IOHandler;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcIoTComponent_Client_Certificate.SetOpenSSL_Options(const Value :
    TsgcOpenSSLClient_Options);
begin
  FOpenSSL_Options.Assign(Value);
end;

procedure TsgcIoTComponent_Client_Certificate.SetSChannel_Options(const Value:
    TsgcSChannelClient_Options);
begin
  FSChannel_Options.Assign(Value);
end;

procedure TsgcIoTComponent_Client_SAS.Assign(aSource: TPersistent);
begin
  if aSource is TsgcIoTComponent_Client_SAS then
  begin
    Enabled := TsgcIoTComponent_Client_SAS(aSource).Enabled;
    SecretKey := TsgcIoTComponent_Client_SAS(aSource).SecretKey;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcIoT_WSClient.OnWatchDogEvent(Sender: TObject);
begin
  if Assigned(FOnBeforeWatchDog) then
    FOnBeforeWatchDog(self);
  inherited;
end;

{$ENDIF}

end.
