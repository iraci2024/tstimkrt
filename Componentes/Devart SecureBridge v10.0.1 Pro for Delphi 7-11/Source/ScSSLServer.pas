
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSLServer;

interface

uses
  SysUtils, SyncObjs, Classes,
  ScTypes, ScFunctions, ScUtils,
  ScVio, ScVioTcp, ScThread, ScTCPServer,
  ScConsts, ScSSLTypes, ScSSLMessages, ScSSLExtensions,
  ScLayers, ScBridge, ScCertificateExts, ScSecureSocket;


const
  DefValProtocols = [spTls12, spTls13];
  DefValCompression = csNone;
  DefValEventsCallMode = ecAsynchronous;
  DefValAllowLoadCRLByHttp = True;
  DefValExtendedMasterSecretMode = emsmAllow;
  DefValNewSessionTicketLifetime = 600;
  DefValNewSessionTicketDistributedCount = 2;
  DefValSupportedKExNamedGroups = [
    kex_x25519,
    kex_secp256r1, kex_secp256k1, kex_secp384r1, kex_secp521r1,
    kex_sect283r1, kex_sect283k1, kex_sect409r1, kex_sect409k1,
    kex_sect571r1, kex_sect571k1,
    kex_ffDHE2048, kex_ffDHE3072, kex_ffDHE4096,
    kex_ffDHE6144, kex_ffDHE8192
  ];

type
  TScReceiveDataFromSourceEvent = function (Sender: TObject; var Data; Offset, Count: integer): integer of object;
  TScSendDataToTargetEvent = procedure (Sender: TObject; const Data; Offset, Count: integer) of object;

  TScSSLServerOptions = class(TPersistent)
  private
    FAsyncStartTLS: boolean;
    FAllowLoadCRLByHttp: boolean;
    FDisableCRLValidation: boolean;
    FIsClientCertificateRequired: boolean;
    FIgnoreClientCertificateConstraints: boolean;
    FIgnoreClientCertificateInsecurity: boolean;
    FIgnoreClientCertificateValidity: boolean;
    FTrustSelfSignedCertificate: boolean;
    FTrustStorageCertificates: boolean;
    FExtendedMasterSecretMode: TScExtendedMasterSecretMode;
    FClientInitiatedRenegotiationIsAllowed: boolean;
    FDisableCloseSocketOnShutdownAlert: boolean;
    FRecordSizeLimit: integer;
    FNewSessionTicketLifetime: integer;
    FNewSessionTicketDistributedCount: integer;

    procedure SetNewSessionTicketLifetime(Value: integer);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;

  published
    property AsyncStartTLS: boolean read FAsyncStartTLS write FAsyncStartTLS default False;
    property AllowLoadCRLByHttp: boolean read FAllowLoadCRLByHttp write FAllowLoadCRLByHttp default DefValAllowLoadCRLByHttp;
    property DisableCRLValidation: boolean read FDisableCRLValidation write FDisableCRLValidation default False;

    property IsClientCertificateRequired: boolean read FIsClientCertificateRequired write FIsClientCertificateRequired default False;
    property IgnoreClientCertificateConstraints: boolean read FIgnoreClientCertificateConstraints write FIgnoreClientCertificateConstraints default False;
    property IgnoreClientCertificateInsecurity: boolean read FIgnoreClientCertificateInsecurity write FIgnoreClientCertificateInsecurity default False;
    property IgnoreClientCertificateValidity: boolean read FIgnoreClientCertificateValidity write FIgnoreClientCertificateValidity default False;
    property TrustSelfSignedCertificate: boolean read FTrustSelfSignedCertificate write FTrustSelfSignedCertificate default False;
    property TrustStorageCertificates: boolean read FTrustStorageCertificates write FTrustStorageCertificates default False;

    property ExtendedMasterSecretMode: TScExtendedMasterSecretMode read FExtendedMasterSecretMode write FExtendedMasterSecretMode default DefValExtendedMasterSecretMode;
    property ClientInitiatedRenegotiationIsAllowed: boolean read FClientInitiatedRenegotiationIsAllowed write FClientInitiatedRenegotiationIsAllowed default False;
    property DisableCloseSocketOnShutdownAlert: boolean read FDisableCloseSocketOnShutdownAlert write FDisableCloseSocketOnShutdownAlert default False;
    property RecordSizeLimit: integer read FRecordSizeLimit write FRecordSizeLimit default 0;
    property NewSessionTicketLifetime: integer read FNewSessionTicketLifetime write SetNewSessionTicketLifetime default DefValNewSessionTicketLifetime;
    property NewSessionTicketDistributedCount: integer read FNewSessionTicketDistributedCount write FNewSessionTicketDistributedCount default DefValNewSessionTicketDistributedCount;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSSLServerConnection = class(TComponent)
  private
    FCipherSuites: TScSSLCipherSuites;
    FCompression: TScCompression;
    FProtocols: TScSSLProtocols;
    FSupportedKExNamedGroups: TScKExNamedGroupTypes;
    FTimeout: integer;
    FEventsCallMode: TScEventCallMode;

    FClientHelloExtensions: TTLSHelloExtensions;
    FServerHelloExtensions: TTLSHelloExtensions;
    FRequestDNList: TScDistinguishedNameList;

    FStorage: TScStorage;
    FCACertName: string;
    FCertNameChain: string;
    FCertificateChain: TCRList;

    FOptions: TScSSLServerOptions;
    FSecureConnection: TCustomSecureConnection;
    FTCPConnection: TScTCPConnection;

    FBeforeConnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;

    FOnClientCertificateValidation: TScRemoteCertificateValidationEvent;
    FOnObtainCRL: TScObtainCRLEvent;
    FAfterClientHello: TScAfterClientHelloMessageEvent;
    FOnCreateNewSessionTicket: TScCreateNewSessionTicketEvent;
    FOnDecodeTicket: TScDecodeTicketEvent;
    FOnGetTicketNameAndPassword: TScGetTicketNameAndPasswordEvent;
    FOnGetPasswordByTicketName: TScGetPasswordByTicketNameEvent;

    FOnConnect: TNotifyEvent;
    FOnReceiveDataFromSource: TScReceiveDataFromSourceEvent;
    FOnSendDataToTarget: TScSendDataToTargetEvent;

    FOnAsyncReceiveData: TScAsyncReceiveEvent;
    FOnAsyncError: TScErrorEvent;

    procedure CheckSecured;

    procedure OnCipherPropertyChanged(Sender: TObject);
    function CheckDefaultCipherSuites: boolean;
    procedure SetCipherSuites(Value: TScSSLCipherSuites);
    procedure SetCompression(const Value: TScCompression);
    procedure SetProtocols(const Value: TScSSLProtocols);
    procedure SetTimeout(const Value: integer);
    procedure SetOnAsyncReceiveData(Value: TScAsyncReceiveEvent);
    procedure SetEventsCallMode(Value: TScEventCallMode);
    procedure SetStorage(Value: TScStorage);
    procedure SetCACertName(const Value: string);
    procedure SetCertNameChain(const Value: string);
    procedure SetOptions(Value: TScSSLServerOptions);
    function GetSessionInfo: TScSSLSessionInfo;

    procedure DoAfterClientHello(Sender: TObject; var Cancel: boolean);
    procedure DoOnCreateNewSessionTicket(Sender: TObject;
      SessionInfo: TScSSLSessionInfo; NewSessionTicket: TScNewSessionTicket);
    procedure DoOnDecodeTicket(Sender: TObject; NewSessionTicket: TScNewSessionTicket;
      NewSessionInfo: TScSSLSessionInfo; var IsValid: boolean);
    procedure DoOnGetTicketNameAndPassword(Sender: TObject; out KeyName, Password: TBytes);
    procedure DoOnGetPasswordByTicketName(Sender: TObject; const Ticket: TBytes; out Password: TBytes);
    procedure DoAfterDisconnect(Sender: TObject);

    procedure ParseCertNameChain;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetIsSecure: boolean;
    procedure SetIsSecure(Value: boolean);

    procedure DoCertificateRequest(Acceptable: TScDistinguishedNameList; CertList: TCRList);
    procedure DoClientCertificateValidation(Sender: TObject; ClientCertificate: TScCertificate;
      CertificateList: TCRList; var Errors: TScCertificateStatusSet);
    procedure DoObtainCRL(Sender: TObject; DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL);

    procedure NotifyOnAsyncReceiveData;
    procedure DoAsyncReceiveData(Sender: TObject);

    function IsClosed: boolean;
    function GetConnected: boolean; virtual;
    procedure SetConnected(Value: boolean); virtual;
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;

    procedure DoOnConnect(Sender: TObject);
    function ReceiveDataFromSource(const Data: TValueArr; Offset, Count: integer): integer;
    procedure SendDataToTarget(const Data: TValueArr; Offset, Count: integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(Connection: TScTCPConnection); overload;
    procedure Init(OnConnect: TNotifyEvent; OnReceiveDataFromSource: TScReceiveDataFromSourceEvent;
      OnSendDataToTarget: TScSendDataToTargetEvent); overload;

    procedure CheckCertificateChain(out StatusSet: TScCertificateStatusSet); overload;
    procedure CheckCertificateChain; overload;

    procedure Connect;
    procedure Disconnect;

    function ReadBuffer(var Buffer; const Count: integer): integer; overload;
    function ReadBuffer(var Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function ReadNoWait(var Buffer; const Count: integer): integer; overload;
    function ReadNoWait(var Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function WriteBuffer(const Buffer; const Count: integer): integer; overload;
    function WriteBuffer(const Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function WaitForData(MillisecondsTimeout: integer = -1): boolean;

    function GetLastException: Exception;

    property ClientHelloExtensions: TTLSHelloExtensions read FClientHelloExtensions;
    property ServerHelloExtensions: TTLSHelloExtensions read FServerHelloExtensions;
    property RequestDNList: TScDistinguishedNameList read FRequestDNList;

    property Connected: boolean read GetConnected write SetConnected default False;
    property IsSecure: boolean read GetIsSecure write SetIsSecure default False;
    property SessionInfo: TScSSLSessionInfo read GetSessionInfo;

  published
    property CipherSuites: TScSSLCipherSuites read FCipherSuites write SetCipherSuites stored CheckDefaultCipherSuites;
    property Compression: TScCompression read FCompression write SetCompression default DefValCompression;
    property Protocols: TScSSLProtocols read FProtocols write SetProtocols default DefValProtocols;
    property SupportedKExNamedGroups: TScKExNamedGroupTypes read FSupportedKExNamedGroups write FSupportedKExNamedGroups default DefValSupportedKExNamedGroups;
    property Timeout: integer read FTimeout write SetTimeout default DEFAULT_TIMEOUT;
    property EventsCallMode: TScEventCallMode read FEventsCallMode write SetEventsCallMode default DefValEventsCallMode;

    property Options: TScSSLServerOptions read FOptions write SetOptions;

    property Storage: TScStorage read FStorage write SetStorage;
    property CACertName: string read FCACertName write SetCACertName;
    property CertNameChain: string read FCertNameChain write SetCertNameChain;

    property OnObtainCRL: TScObtainCRLEvent read FOnObtainCRL write FOnObtainCRL;
    property OnClientCertificateValidation: TScRemoteCertificateValidationEvent read FOnClientCertificateValidation write FOnClientCertificateValidation;

    property AfterClientHello: TScAfterClientHelloMessageEvent read FAfterClientHello write FAfterClientHello;
    property OnCreateNewSessionTicket: TScCreateNewSessionTicketEvent read FOnCreateNewSessionTicket write FOnCreateNewSessionTicket;
    property OnDecodeTicket: TScDecodeTicketEvent read FOnDecodeTicket write FOnDecodeTicket;
    property OnGetTicketNameAndPassword: TScGetTicketNameAndPasswordEvent read FOnGetTicketNameAndPassword write FOnGetTicketNameAndPassword;
    property OnGetPasswordByTicketName: TScGetPasswordByTicketNameEvent read FOnGetPasswordByTicketName write FOnGetPasswordByTicketName;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;

    property OnAsyncReceiveData: TScAsyncReceiveEvent read FOnAsyncReceiveData write SetOnAsyncReceiveData;
    property OnAsyncError: TScErrorEvent read FOnAsyncError write FOnAsyncError;

    property OnReceiveDataFromSource: TScReceiveDataFromSourceEvent read FOnReceiveDataFromSource write FOnReceiveDataFromSource;
    property OnSendDataToTarget: TScSendDataToTargetEvent read FOnSendDataToTarget write FOnSendDataToTarget;
  end;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  ScSSLClient;

{ TScSSLServerOptions }

constructor TScSSLServerOptions.Create;
begin
  inherited;

  FAsyncStartTLS := False;
  FAllowLoadCRLByHttp := DefValAllowLoadCRLByHttp;
  FDisableCRLValidation := False;
  FIsClientCertificateRequired := False;
  FIgnoreClientCertificateConstraints := False;
  FIgnoreClientCertificateInsecurity := False;
  FIgnoreClientCertificateValidity := False;
  FTrustSelfSignedCertificate := False;
  FTrustStorageCertificates := False;

  FExtendedMasterSecretMode := DefValExtendedMasterSecretMode;
  FClientInitiatedRenegotiationIsAllowed := False;
  FDisableCloseSocketOnShutdownAlert := False;
  FRecordSizeLimit := 0;
  FNewSessionTicketLifetime := DefValNewSessionTicketLifetime;
  FNewSessionTicketDistributedCount := DefValNewSessionTicketDistributedCount;
end;

procedure TScSSLServerOptions.AssignTo(Dest: TPersistent);
var
  Dst: TScSSLServerOptions;
begin
  if Dest is TScSSLServerOptions then begin
    Dst := TScSSLServerOptions(Dest);
    Dst.FAsyncStartTLS := FAsyncStartTLS;
    Dst.FAllowLoadCRLByHttp := FAllowLoadCRLByHttp;
    Dst.FDisableCRLValidation := FDisableCRLValidation;
    Dst.FIsClientCertificateRequired := FIsClientCertificateRequired;
    Dst.FIgnoreClientCertificateConstraints := FIgnoreClientCertificateConstraints;
    Dst.FIgnoreClientCertificateInsecurity := FIgnoreClientCertificateInsecurity;
    Dst.FIgnoreClientCertificateValidity := FIgnoreClientCertificateValidity;
    Dst.FTrustSelfSignedCertificate := FTrustSelfSignedCertificate;
    Dst.FTrustStorageCertificates := FTrustStorageCertificates;
    Dst.FExtendedMasterSecretMode := FExtendedMasterSecretMode;
    Dst.FClientInitiatedRenegotiationIsAllowed := FClientInitiatedRenegotiationIsAllowed;
    Dst.FDisableCloseSocketOnShutdownAlert := FDisableCloseSocketOnShutdownAlert;
    Dst.FRecordSizeLimit := FRecordSizeLimit;
    Dst.FNewSessionTicketLifetime := FNewSessionTicketLifetime;
    Dst.FNewSessionTicketDistributedCount := FNewSessionTicketDistributedCount;
  end
  else
    inherited;
end;

procedure TScSSLServerOptions.SetNewSessionTicketLifetime(Value: integer);
begin
  if (Value < 0) or (Value > 604800{7 days}) then
    raise Exception.Create(SNewSessionTicketLifetimeMoreMax);

  FNewSessionTicketLifetime := Value;
end;

{ TScSSLServerConnection }

constructor TScSSLServerConnection.Create(AOwner: TComponent);
begin
  inherited;

  FClientHelloExtensions := TTLSHelloExtensions.Create;
  FServerHelloExtensions := TTLSHelloExtensions.Create;
  FRequestDNList := TScDistinguishedNameList.Create;
  FCertificateChain := TCRList.Create;

  FOptions := TScSSLServerOptions.Create;
  FCipherSuites := TScSSLCipherSuites.Create(Self);
  _InitDefaultCipherSuites(FCipherSuites, True);
  FCipherSuites.OnChanged := OnCipherPropertyChanged;

  FCompression := DefValCompression;
  FProtocols := DefValProtocols;
  FSupportedKExNamedGroups := DefValSupportedKExNamedGroups;
  FTimeout := DEFAULT_TIMEOUT;
  FEventsCallMode := DefValEventsCallMode;
end;

destructor TScSSLServerConnection.Destroy;
begin
  try
    DoDisconnect;
  except
  end;

  DisposeAsyncEvent(NotifyOnAsyncReceiveData);

  FSecureConnection.Free;
  FTCPConnection.Free;

  FClientHelloExtensions.Free;
  FServerHelloExtensions.Free;
  FRequestDNList.Free;
  FCertificateChain.Free;
  FOptions.Free;
  FCipherSuites.Free;

  inherited;
end;

procedure TScSSLServerConnection.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSLServerConnection) then begin
    TScSSLServerConnection(Dest).CipherSuites := CipherSuites;
    TScSSLServerConnection(Dest).Compression := Compression;
    TScSSLServerConnection(Dest).Protocols := Protocols;
    TScSSLServerConnection(Dest).SupportedKExNamedGroups := SupportedKExNamedGroups;
    TScSSLServerConnection(Dest).Timeout := Timeout;
    TScSSLServerConnection(Dest).EventsCallMode := EventsCallMode;

    TScSSLServerConnection(Dest).Storage := Storage;
    TScSSLServerConnection(Dest).CACertName := CACertName;
    TScSSLServerConnection(Dest).CertNameChain := CertNameChain;
    TScSSLServerConnection(Dest).FOptions.Assign(FOptions);
  end
  else
    inherited;
end;

procedure TScSSLServerConnection.Init(Connection: TScTCPConnection);
begin
  FTCPConnection := Connection;
end;

procedure TScSSLServerConnection.Init(OnConnect: TNotifyEvent; OnReceiveDataFromSource: TScReceiveDataFromSourceEvent;
  OnSendDataToTarget: TScSendDataToTargetEvent);
begin
  FOnConnect := OnConnect;
  FOnReceiveDataFromSource := OnReceiveDataFromSource;
  FOnSendDataToTarget := OnSendDataToTarget;
end;

function TScSSLServerConnection.IsClosed: boolean;
begin
  if FSecureConnection = nil then
    Result := True
  else
    Result := not FSecureConnection.Connected and not FSecureConnection.HasData;
end;

function TScSSLServerConnection.GetConnected: boolean;
begin
  Result := (FSecureConnection <> nil) and FSecureConnection.Connected;
end;

procedure TScSSLServerConnection.SetConnected(Value: boolean);
begin
  if Value = GetConnected then
    Exit;

  if Value then begin
    if Assigned(BeforeConnect) then
      BeforeConnect(Self);

    DoConnect;

    if Assigned(AfterConnect) then
      AfterConnect(Self);
  end
  else begin
    if Assigned(BeforeDisconnect) then
      BeforeDisconnect(Self);

    DoDisconnect;
  end;
end;

procedure TScSSLServerConnection.Connect;
begin
  SetConnected(True);
end;

procedure TScSSLServerConnection.Disconnect;
begin
  SetConnected(False);
end;

procedure TScSSLServerConnection.DoConnect;
var
  CACertificate, ServerCertificate: TScCertificate;
begin
  if CACertName <> '' then begin
    if Storage = nil then
      raise EScError.Create(seStorageNoSet);

    CACertificate := Storage.Certificates.FindCertificate(CACertName);
    if CACertificate <> nil then
      CACertificate.Ready := True;
  end;

  ParseCertNameChain;
  if FCertificateChain.Count = 0 then
    raise EScError.Create(seServerCertificateNotSpecified);

  ServerCertificate := TScCertificate(FCertificateChain[0]);
  if not ServerCertificate.Key.IsPrivate then
    raise EScError.Create(seCertificateMustBePrivate);

  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  FreeAndNil(FSecureConnection);

  if FTCPConnection <> nil then
    FSecureConnection := TSecureSocket.Create(FTCPConnection.Vio, False)
  else
    FSecureConnection := TSecureConnection.Create;
  try
    if FSecureConnection is TSecureSocket then begin
      TSecureSocket(FSecureConnection).Timeout := FTimeout;
      TSecureSocket(FSecureConnection).OnClose := DoAfterDisconnect;
    end
    else begin
      TSecureConnection(FSecureConnection).OnConnect := DoOnConnect;
      TSecureConnection(FSecureConnection).OnReceiveDataFromSource := ReceiveDataFromSource;
      TSecureConnection(FSecureConnection).OnSendDataToTarget := SendDataToTarget;
      TSecureConnection(FSecureConnection).OnClose := DoAfterDisconnect;
    end;

    if Assigned(OnAsyncReceiveData) then
      FSecureConnection.OnAsyncReceive := DoAsyncReceiveData;
  except
    FreeAndNil(FSecureConnection);
    raise;
  end;
end;

procedure TScSSLServerConnection.DoDisconnect;
begin
  if GetConnected then
    try
      FSecureConnection.Shutdown;
    except
    end;

  if FSecureConnection <> nil then
    FSecureConnection.Close;
end;

procedure TScSSLServerConnection.ParseCertNameChain;
var
  CertNames: TStrings;
  CertName: string;
  Certificate: TScCertificate;
  i: integer;
begin
  if Storage = nil then
    raise EScError.Create(seStorageNoSet);

  if FCertificateChain.Count > 0 then
    Exit;

  if FCertNameChain = '' then
    Exit;

  CertNames := TStringList.Create;
  try
    Split(CertNames, FCertNameChain, ';', True);

    for i := 0 to CertNames.Count - 1 do begin
      CertName := UnquoteStr(CertNames[i]);
      Certificate := Storage.Certificates.CertificateByName(CertName);
      Certificate.Ready := True;
      FCertificateChain.Add(Certificate);
    end;
  finally
    CertNames.Free;
  end;
end;

procedure TScSSLServerConnection.CheckCertificateChain(out StatusSet: TScCertificateStatusSet);
var
  SecOptions: TScSecurityOptions;
begin
  ParseCertNameChain;

  if FCertificateChain.Count = 0 then
    raise EScError.Create(seServerCertificateNotSpecified);

  SecOptions := TScSecurityOptions.Create;
  try
    SecOptions.CertificateListIsUnsorted := False;
    SecOptions.CAStorage := Storage;
    if not FOptions.DisableCRLValidation then
      SecOptions.OnObtainCRL := DoObtainCRL;

    if CACertName <> '' then
      SecOptions.CACertificate := Storage.Certificates.FindCertificate(CACertName);

    StatusSet := TScTLSCertificateChain.VerifyChain(FCertificateChain, SecOptions);
  finally
    SecOptions.Free;
  end;
end;

procedure TScSSLServerConnection.CheckCertificateChain;
var
  StatusSet: TScCertificateStatusSet;
  St: TScCertificateStatus;
begin
  CheckCertificateChain(StatusSet);

  if StatusSet = [] then
    Exit;

  for St := High(TScCertificateStatus) downto Low(TScCertificateStatus) do begin
    if (St <> csValid) and (St in StatusSet) then
      raise EScError.Create(CERTIFICATE_STATUS_ERROR_CODE[St]);
  end;
end;

function TScSSLServerConnection.GetIsSecure: boolean;
begin
  if FSecureConnection <> nil then
    Result := FSecureConnection.IsSecure
  else
    Result := False;
end;

procedure TScSSLServerConnection.SetIsSecure(Value: boolean);
var
  SecOptions: TScSecurityOptions;
begin
  if FSecureConnection = nil then
    raise EScError.Create(seClientClosed);

  if FSecureConnection.IsSecure <> Value then begin
    if Value then begin
      SecOptions := TScSecurityOptions.Create;
      try
        SecOptions.Entity := ceServer;

        // create a security options for secure connection
        SecOptions.CipherAlgorithms := FCipherSuites.GetCipherSuites;
        SecOptions.Compression := FCompression;
        SecOptions.Protocols := FProtocols;
        SecOptions.SupportedKExNamedGroups := FSupportedKExNamedGroups;

        SecOptions.IsClientCertificateRequired := FOptions.IsClientCertificateRequired;
        SecOptions.IgnoreRemoteCertificateConstraints := FOptions.IgnoreClientCertificateConstraints;
        SecOptions.IgnoreRemoteCertificateInsecurity := FOptions.IgnoreClientCertificateInsecurity;
        SecOptions.IgnoreRemoteCertificateValidity := FOptions.IgnoreClientCertificateValidity;
        SecOptions.TrustSelfSignedCertificate := FOptions.TrustSelfSignedCertificate;

        SecOptions.ExtendedMasterSecretMode := FOptions.ExtendedMasterSecretMode;
        SecOptions.ClientInitiatedRenegotiationIsAllowed := FOptions.ClientInitiatedRenegotiationIsAllowed;
        SecOptions.DisableCloseSocketOnShutdownAlert := FOptions.DisableCloseSocketOnShutdownAlert;
        SecOptions.RecordSizeLimit := FOptions.RecordSizeLimit;
        SecOptions.NewSessionTicketLifetime := FOptions.NewSessionTicketLifetime;
        SecOptions.NewSessionTicketDistributedCount := FOptions.NewSessionTicketDistributedCount;
        SecOptions.AsyncStartTLS := FOptions.AsyncStartTLS;

        SecOptions.ClientHelloExtensions := FClientHelloExtensions;
        SecOptions.ServerHelloExtensions := FServerHelloExtensions;
        SecOptions.RequestDNList := FRequestDNList;

        if (Int64(FTimeout) * 1000) > MaxInt then
          SecOptions.Timeout := MaxInt
        else
          SecOptions.Timeout := FTimeout * 1000;

        if Storage <> nil then begin
          if CACertName <> '' then
            SecOptions.CACertificate := Storage.Certificates.FindCertificate(CACertName);

          if FOptions.TrustStorageCertificates then
            SecOptions.CAStorage := Storage;
        end;

        if FCertificateChain.Count = 0 then
          raise EScError.Create(seServerCertificateNotSpecified);
        SecOptions.OnCertificateRequest := DoCertificateRequest;

        SecOptions.OnRemoteCertificateValidation := DoClientCertificateValidation;
        if not FOptions.DisableCRLValidation then
          SecOptions.OnObtainCRL := DoObtainCRL;

        if Assigned(FAfterClientHello) then
          SecOptions.AfterClientHello := DoAfterClientHello;
        if Assigned(FOnCreateNewSessionTicket) then
          SecOptions.OnCreateNewSessionTicket := DoOnCreateNewSessionTicket;
        if Assigned(FOnDecodeTicket) then
          SecOptions.OnDecodeTicket := DoOnDecodeTicket;
        if Assigned(FOnGetTicketNameAndPassword) then
          SecOptions.OnGetTicketNameAndPassword := DoOnGetTicketNameAndPassword;
        if Assigned(FOnGetPasswordByTicketName) then
          SecOptions.OnGetPasswordByTicketName := DoOnGetPasswordByTicketName;

        FSecureConnection.Options := SecOptions;
      finally
        SecOptions.Free;
      end;
    end;

    FSecureConnection.IsSecure := Value;
  end;
end;

function TScSSLServerConnection.ReadBuffer(var Buffer; const Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureConnection.Read(@Buffer, 0, Count)
  else
    Result := 0;
end;

function TScSSLServerConnection.ReadBuffer(var Buffer: TBytes; const Offset, Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureConnection.Read(TValueArr(Buffer), Offset, Count)
  else
    Result := 0;
end;

function TScSSLServerConnection.ReadNoWait(var Buffer; const Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureConnection.ReadNoWait(@Buffer, 0, Count)
  else
    Result := 0;
end;

function TScSSLServerConnection.ReadNoWait(var Buffer: TBytes; const Offset, Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureConnection.ReadNoWait(TValueArr(Buffer), Offset, Count)
  else
    Result := 0;
end;

function TScSSLServerConnection.WriteBuffer(const Buffer; const Count: integer): integer;
begin
  if Connected then
    Result := FSecureConnection.Send(@Buffer, 0, Count)
  else
    Result := 0;
end;

function TScSSLServerConnection.WriteBuffer(const Buffer: TBytes; const Offset, Count: integer): integer;
begin
  if Connected then
    Result := FSecureConnection.Send(TValueArr(Buffer), Offset, Count)
  else
    Result := 0;
end;

function TScSSLServerConnection.WaitForData(MillisecondsTimeout: integer = -1): boolean;
begin
  if not IsClosed then
    Result := FSecureConnection.WaitForData(MillisecondsTimeout)
  else
    Result := False;
end;

procedure TScSSLServerConnection.DoOnConnect(Sender: TObject);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

function TScSSLServerConnection.ReceiveDataFromSource(const Data: TValueArr; Offset, Count: integer): integer;
begin
  if Assigned(OnReceiveDataFromSource) then
    Result := OnReceiveDataFromSource(Self, Data^, Offset, Count)
  else
    Result := 0;
end;

procedure TScSSLServerConnection.SendDataToTarget(const Data: TValueArr; Offset, Count: integer);
begin
  if Assigned(OnSendDataToTarget) then
    OnSendDataToTarget(Self, Data^, Offset, Count);
end;

procedure TScSSLServerConnection.DoCertificateRequest(Acceptable: TScDistinguishedNameList; CertList: TCRList);
var
  i: integer;
begin
  CertList.Clear;
  for i := 0 to FCertificateChain.Count - 1 do
    CertList.Add(FCertificateChain[i]);
end;

procedure TScSSLServerConnection.DoClientCertificateValidation(Sender: TObject;
  ClientCertificate: TScCertificate; CertificateList: TCRList; var Errors: TScCertificateStatusSet);
begin
  if Assigned(FOnClientCertificateValidation) then
    FOnClientCertificateValidation(Self, ClientCertificate, CertificateList, Errors);
end;

procedure TScSSLServerConnection.DoObtainCRL(Sender: TObject; DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL);
begin
  TScCRLList.DoObtainCRL(Self, Storage, FOptions.AllowLoadCRLByHttp,
    FOnObtainCRL, DistributionPointName, Update, CRL);
end;

procedure TScSSLServerConnection.SetOnAsyncReceiveData(Value: TScAsyncReceiveEvent);
begin
  if @Value <> @FOnAsyncReceiveData then begin
    FOnAsyncReceiveData := Value;

    if FSecureConnection <> nil then begin
      if Assigned(Value) then
        FSecureConnection.OnAsyncReceive := DoAsyncReceiveData
      else
        FSecureConnection.OnAsyncReceive := nil; // to make sure, that DoAsyncReceiveData will not be called more
    end;
  end;
end;

procedure TScSSLServerConnection.DoAsyncReceiveData(Sender: TObject);
begin
  if Assigned(OnAsyncReceiveData) then
    case FEventsCallMode of
      ecDirectly:
        NotifyOnAsyncReceiveData;
      ecAsynchronous:
        HandleEventAsync(NotifyOnAsyncReceiveData);
      ecSynchronous:
        SynchronizeWithMainThread(NotifyOnAsyncReceiveData);
    else
      Assert(False);
    end;
end;

procedure TScSSLServerConnection.NotifyOnAsyncReceiveData;
var
  Fail: boolean;
begin
  try
    if Assigned(FOnAsyncReceiveData) then
      FOnAsyncReceiveData(Self);
  except
    on E: Exception do
      if not (E is EAbort) then begin
        Fail := FEventsCallMode = ecDirectly;

        if Assigned(FOnAsyncError) then
          FOnAsyncError(Self, E);

        if Fail then
          raise;
      end;
  end;
end;

procedure TScSSLServerConnection.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FStorage) and (Operation = opRemove) then
    Storage := nil;

  inherited;
end;

procedure TScSSLServerConnection.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

procedure TScSSLServerConnection.DoAfterClientHello(Sender: TObject; var Cancel: boolean);
begin
  if Assigned(FAfterClientHello) then
    FAfterClientHello(Self, Cancel);
end;

procedure TScSSLServerConnection.DoOnCreateNewSessionTicket(Sender: TObject;
  SessionInfo: TScSSLSessionInfo; NewSessionTicket: TScNewSessionTicket);
begin
  if Assigned(FOnCreateNewSessionTicket) then
    FOnCreateNewSessionTicket(Self, SessionInfo, NewSessionTicket);
end;

procedure TScSSLServerConnection.DoOnDecodeTicket(Sender: TObject; NewSessionTicket: TScNewSessionTicket;
  NewSessionInfo: TScSSLSessionInfo; var IsValid: boolean);
begin
  if Assigned(FOnDecodeTicket) then
    FOnDecodeTicket(Self, NewSessionTicket, NewSessionInfo, IsValid);
end;

procedure TScSSLServerConnection.DoOnGetTicketNameAndPassword(Sender: TObject; out KeyName, Password: TBytes);
begin
  if Assigned(FOnGetTicketNameAndPassword) then
    FOnGetTicketNameAndPassword(Self, KeyName, Password);
end;

procedure TScSSLServerConnection.DoOnGetPasswordByTicketName(Sender: TObject; const Ticket: TBytes; out Password: TBytes);
begin
  if Assigned(FOnGetPasswordByTicketName) then
    FOnGetPasswordByTicketName(Self, Ticket, Password);
end;

procedure TScSSLServerConnection.SetStorage(Value: TScStorage);
begin
  if FStorage <> Value then begin
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);

    SetConnected(False);
    FStorage := Value;
    FCertificateChain.Clear;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TScSSLServerConnection.CheckSecured;
begin
  if Connected and IsSecure then
    raise EScError.Create(seClientOpened);
end;

function TScSSLServerConnection.GetLastException: Exception;
begin
  if FSecureConnection <> nil then
    Result := FSecureConnection.GetLastException
  else
    Result := nil;
end;

function TScSSLServerConnection.GetSessionInfo: TScSSLSessionInfo;
begin
  if FSecureConnection <> nil then
    Result := FSecureConnection.SessionInfo
  else
    Result := nil;
end;

procedure TScSSLServerConnection.OnCipherPropertyChanged(Sender: TObject);
begin
  CheckSecured;
end;

function TScSSLServerConnection.CheckDefaultCipherSuites: boolean;
begin
  Result := not _CheckDefaultCipherSuites(FCipherSuites, True);
end;

procedure TScSSLServerConnection.SetCipherSuites(Value: TScSSLCipherSuites);
begin
  if Value <> FCipherSuites then begin
    CheckSecured;
    FCipherSuites.Assign(Value);
  end;
end;

procedure TScSSLServerConnection.SetCompression(const Value: TScCompression);
begin
  if Value <> FCompression then begin
    CheckSecured;
    FCompression := Value;
  end;
end;

procedure TScSSLServerConnection.SetProtocols(const Value: TScSSLProtocols);
begin
  if Value <> FProtocols then begin
    CheckSecured;
    FProtocols := Value;
  end;
end;

procedure TScSSLServerConnection.SetOptions(Value: TScSSLServerOptions);
begin
  CheckSecured;
  FOptions.Assign(Value);
end;

procedure TScSSLServerConnection.SetTimeout(const Value: integer);
begin
  if Value <> FTimeout then begin
    CheckSecured;
    FTimeout := Value;
  end;
end;

procedure TScSSLServerConnection.SetEventsCallMode(Value: TScEventCallMode);
begin
  if Value <> FEventsCallMode then begin
    if Connected then
      raise EScError.Create(seClientOpened);

    FEventsCallMode := Value;
  end;
end;

procedure TScSSLServerConnection.SetCACertName(const Value: string);
begin
  if Value <> FCACertName then begin
    CheckSecured;
    FCACertName := Value;
  end;
end;

procedure TScSSLServerConnection.SetCertNameChain(const Value: string);
begin
  if Value <> FCertNameChain then begin
    CheckSecured;
    FCertNameChain := Value;
    FCertificateChain.Clear;
  end;
end;

end.
