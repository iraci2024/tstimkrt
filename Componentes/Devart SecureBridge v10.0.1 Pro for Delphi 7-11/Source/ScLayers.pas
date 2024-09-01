
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScLayers;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRRNG,
  CRCryptoTransformIntf, CRHash, CRHashAlgorithm, CRSymmetricAlgorithm, CRHMAC,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLTypes, TdsMD5SHA1CSP, TdsBridge,
  TdsCipherSuites, TdsSSLMessages, TdsSSLConsts, TdsSSLExtensions,
  TdsCertificateExts, TdsReceiveBuffer, TdsPipe;
{$ELSE}
  TdsUtilsUni, TdsSSLTypesUni, TdsMD5SHA1CSPUni, TdsBridgeUni,
  TdsCipherSuitesUni, TdsSSLMessagesUni, TdsSSLConstsUni, TdsSSLExtensionsUni,
  TdsCertificateExtsUni, TdsReceiveBufferUni, TdsPipeUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScRNG,
  ScCryptoTransformIntf, ScHash, ScHashAlgorithm, ScSymmetricAlgorithm, ScHMAC,
  ScUtils, ScSSLTypes, ScMD5SHA1CSP, ScBridge,
  ScCipherSuites, ScSSLMessages, ScConsts, ScSSLExtensions,
  ScCertificateExts, ScReceiveBuffer, ScPipe;
{$ENDIF}

type
  TRecordLayer = class;
  THandshakeLayer = class;
  THandshakeLayerClass = class of THandshakeLayer;
  THandshakeProtocolService = class;

  TScSendDataEvent = procedure (const Data: TValueArr; Offset, Count: integer) of object;
  TScReceiveDataEvent = function (const Data: TValueArr; Offset, Count: integer): integer of object;

  EScAlertError = class(EScError)

  end;

  TSecureController = class
  protected
    FIsDisposed: boolean;
    FLock: TCriticalSection;
    FLastException: Exception;

    FOptions: TScSecurityOptions;
    FReceiveMessage: TRecordMessage;
    FRecordLayer: TRecordLayer;
    FDecryptedBuffer: TReceiveBuffer;

    FOnSendDataToTarget: TScSendDataEvent;
    FOnReceiveDataFromSource: TScReceiveDataEvent;
    FOnClose: TNotifyEvent;

    procedure InternalStartTLS;
    procedure ReceiveDataFromSource(Count: integer = 0);
    function ProcessReceivedMessage: TScSSLStatus;
    function ProcessReceivedData: TScSSLStatus;

    procedure SendEncryptedData;
    procedure CloseConnection(e: Exception);

    function GetAvailable: integer;
    function GetRemoteCertificate: TScCertificate;
    function GetSessionInfo: TScSSLSessionInfo;

    function GetAfterReceiveAppData: TNotifyEvent;
    procedure SetAfterReceiveAppData(Value: TNotifyEvent);

    procedure RaiseLastError;

  public
    constructor Create(Options: TScSecurityOptions; HandshakeLayerClass: THandshakeLayerClass);
    destructor Destroy; override;

    procedure StartTLS; virtual;
    procedure ReStart; virtual;
    procedure Close;

    function WriteData(const Data: TValueArr; Offset, Count: integer): integer;
    function ReadDataSync(const Data: TValueArr; Offset, Count: integer): integer;

    function WaitForData(DataLen: integer; MillisecondsTimeout: cardinal): boolean;
    procedure QueueRenegotiate;
    procedure Shutdown;
    function GetLastException: Exception;

    property Available: integer read GetAvailable;
    property Options: TScSecurityOptions read FOptions;
    property RemoteCertificate: TScCertificate read GetRemoteCertificate;
    property SessionInfo: TScSSLSessionInfo read GetSessionInfo;

    property OnSendDataToTarget: TScSendDataEvent read FOnSendDataToTarget write FOnSendDataToTarget;
    property OnReceiveDataFromSource: TScReceiveDataEvent read FOnReceiveDataFromSource write FOnReceiveDataFromSource;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property AfterReceiveAppData: TNotifyEvent read GetAfterReceiveAppData write SetAfterReceiveAppData;
  end;

  TRecordLayer = class
  private
    FOptions: TScSecurityOptions;
    FRandom: IScRandom;
    FPipe: TScPipe;
    FPipeReader: TScPipeReader;
    FPipeWriter: TScPipeWriter;

    FEncryptor: TSymmetricAlgorithm;
    FDecryptor: TSymmetricAlgorithm;
    FLocalIV, FLocalAAD: TBytes; // for GCM mode
    FRemoteIV, FRemoteAAD: TBytes; // for GCM mode
    FRemoteMac: TBytes;
    FLocalHasher: TKeyedHashAlgorithm;
    FRemoteHasher: TKeyedHashAlgorithm;
    FLocalCompressor: IScCompression;
    FRemoteCompressor: IScCompression;
    FHandshakeLayerClass: THandshakeLayerClass;
    FHandshakeLayer: THandshakeLayer;
    FSendMessage: TRecordMessageInfo;
    FLockWriter: TCriticalSection;
    FLockReader: TCriticalSection;
    FWaitNegotiating: TEvent;
    FExpansionFragmentLength: integer;
    FMaxRecordLength: integer;

    FInputSequenceNumber: Int64; // records received from other side
    FOutputSequenceNumber: Int64; // records sent to other side

    function GetIsNegotiating: boolean;
    procedure SetIsNegotiating(Value: boolean);

    class procedure IncVector(var Vector: TBytes);
    class function GetSequenceBytes(const Number: Int64): TBytes;
    procedure WrapMessage(const Data: TValueArr; Size: integer);
    procedure UnwrapMessage(Message: TRecordMessage);

    function ProcessMessage(RecordMessage: TRecordMessage; AppDecryptedBuffer: TReceiveBuffer; out NeedBytes: integer): TScSSLStatus;

    procedure LockWriter;
    procedure UnlockWriter;
    procedure LockReader;
    procedure UnlockReader;

  public
    constructor Create(Options: TScSecurityOptions; HandshakeLayerClass: THandshakeLayerClass);
    destructor Destroy; override;

    procedure Reinitialize;

    procedure ChangeLocalState(const Encryptor: TSymmetricAlgorithm;
      const LocalIV: TBytes; const LocalHasher: TKeyedHashAlgorithm);
    procedure ChangeRemoteState(const Decryptor: TSymmetricAlgorithm;
      const RemoteIV: TBytes; const RemoteHasher: TKeyedHashAlgorithm);
    procedure EncryptAndStoreMessage(Message: THandshakeMessage);
    procedure EncryptAndStore(const Data: TValueArr; Size: integer; ContentType: TScContentType);

    procedure CreateClientHelloControlBytes;
    procedure CreateRenegotiateControlBytes;
    procedure CreateShutdownControlBytes;

    property IsNegotiating: boolean read GetIsNegotiating write SetIsNegotiating;
    property WaitNegotiating: TEvent read FWaitNegotiating;
    property MaxRecordLength: integer read FMaxRecordLength write FMaxRecordLength;

    property HandshakeLayer: THandshakeLayer read FHandshakeLayer;
    property Encryptor: TSymmetricAlgorithm read FEncryptor;
    property Decryptor: TSymmetricAlgorithm read FDecryptor;
  end;

  TCipherPart = (cpLocal, cpRemote, cpBoth);

  THandshakeLayer = class
  private
    FCipherSuite: TCipherSuite;
    FShutdownLock: TCriticalSection;
    FProcessMessageBuf: TBytes;
    FProcessMessageCount: integer;
    FIsChangeCipherSpecSent: boolean;

    FIsNegotiating: boolean;
    procedure SetIsNegotiating(Value: boolean);

    function GetIdentityDNSName: string;
    procedure VerifyCertificate(CertificateList: TCRList; out StatusSet: TScCertificateStatusSet);

  protected
    FRandom: IScRandom;
    FService: THandshakeProtocolService;
    FOptions: TScSecurityOptions;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FRecordLayer: TRecordLayer;
    FSendMessage: THandshakeMessage;

    FState: TScHandshakeType; // from last received message
    FClientServerRandom: TBytes;
    FSessionInfo: TScSSLSessionInfo;
    FOwnCertificateList: TCRList;
    FRemoteCertificate: TScCertificate;
    FInitialized: boolean;
    FFinished: boolean;
    FIsSessionResumption: boolean;
    FUseExtendedMasterSecret: boolean;
    FLastKeyUpdateRequest: TScKeyUpdateRequest;
    FIsRenegotiationStarted: boolean;

    class function GetUnixTimeBuf: TBytes;

    procedure Init; virtual;

    procedure CheckAndChangeProtocol(const RemoteProtocol: TScSSLProtocol);
    procedure SetService(Value: THandshakeProtocolService);
    procedure InitializeCiphers(CipherPart: TCipherPart);

    procedure ProcessCertificate(Message: THandshakeMessage);
    procedure ProcessCertificateVerify(Message: THandshakeMessage);

    function ProcessAlert(Message: TRecordMessage): TScSSLStatus;

    function GetHandshakeMessage(const Buf: TBytes; Offset, Count: integer): THandshakeMessage;
    procedure ClearNegotiationInfo;
    procedure ClearKeyExchangeInfo; virtual;

    function IsRequiredPostHandshakeAuth: boolean;
    function GetOwnCertificate: TScCertificate;

    procedure MakeCertificateMessage(CertList: TCRList);
    procedure MakeCertificateVerifyMessage(Certificate: TScCertificate);

    procedure MakeClientHelloMessage; virtual; abstract;
    procedure MakeHelloRequestMessage; virtual; abstract;
    procedure MakeRenegotiateMessage;

    procedure MakeKeyUpdateMessage(KeyUpdateRequest: TScKeyUpdateRequest);
    procedure ProcessKeyUpdate(Message: THandshakeMessage);

    procedure MakeShutdownMessage;

    procedure InternalProcessMessage(Message: THandshakeMessage); virtual; abstract;
    procedure ReplyToMessage(HandshakeType: TScHandshakeType); virtual; abstract;
    function ProcessMessage(Message: TRecordMessage): TScSSLStatus;

    procedure MakeChangeCipherSpec;
    function ProcessChangeCipherSpec(Message: TRecordMessage): TScSSLStatus;
    procedure MakeFinishedMessage;

  public
    constructor Create(RecordLayer: TRecordLayer; Options: TScSecurityOptions);
    destructor Destroy; override;

    function GetProtocol: TScSSLProtocol;

    procedure SendAlert(AlertDescription: TScAlertDescription; ErrorCode: TScErrorCode);

    property Options: TScSecurityOptions read FOptions;
    property SessionInfo: TScSSLSessionInfo read FSessionInfo;
    property IsNegotiating: boolean read FIsNegotiating write SetIsNegotiating;
    property ClientServerRandom: TBytes read FClientServerRandom;
  end;

  THandshakeProtocolService = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: THandshakeLayer;

    FEntity: TScSSLConnectionEnd;
    FNegotiationBuffer: TBytes;
    FNegotiationBufferPos: integer;
    FNegotiationHashPos: integer;
    FNegotiationHash: TBytes;
    FMasterSecret: TBytes;
    FTranscriptHashAlgorithm: THashAlgorithm;

    function GetCipherAlgorithm: TScSSLCipherAlgorithm;
    procedure SetMasterSecret(const Value: TBytes);
    procedure GetTicketKeyNameAndPassword(out TicketName, Password: TBytes);
    procedure GetPasswordByTicketKeyName(const Ticket: TBytes; out Password: TBytes);
    function CreateNewSessionTicket(SessionInfo: TScSSLSessionInfo): TScNewSessionTicketExt;
    function DecodeAndCheckTicket(NewSessionTicket: TScNewSessionTicket; NewSessionInfo: TScSSLSessionInfo): boolean;

  public
    constructor Create(Owner: THandshakeLayer; Entity: TScSSLConnectionEnd); overload;
    constructor Create(Clone: THandshakeProtocolService; Entity: TScSSLConnectionEnd); overload;
    destructor Destroy; override;

    procedure Clear;

    procedure StoreNegotiationData(Message: THandshakeMessage); overload;
    procedure StoreNegotiationData(const Buffer: TBytes; Offset, Count: integer); overload;
    function TranscriptHash: TBytes;
    procedure TranscriptClientHello1Hash; virtual;

    function InitializeCipherSuite(const Definition: TCipherDefinition;
      const CipherPart: TCipherPart): TCipherSuite; virtual; abstract;

    procedure CreateClientHello1Hash; virtual;
    procedure MakeEarlySecret(const PreSharedKey: TBytes); virtual;
    procedure MakeHandshakeTrafficSecret(const Premaster: TBytes); virtual;
    procedure MakeApplicationTrafficSecret; virtual;
    procedure MakeResumptionMasterSecret; virtual;
    procedure UpdateClientApplicationTrafficSecret; virtual;
    procedure UpdateServerApplicationTrafficSecret; virtual;
    procedure MakeMasterSecret(const Premaster: TBytes); overload;
    procedure MakeMasterSecret(const Premaster, Random: TBytes; const aLabel: array of byte); overload; virtual; abstract;

    procedure MakeCertificateRequestMessage(Message: THandshakeMessage; DNList: TScDistinguishedNameList); virtual;
    function ParseCertificateRequestMessage(Message: THandshakeMessage): TScDistinguishedNameList; virtual;
    procedure WriteSignatureAndHash(Message: THandshakeMessage); virtual;
    procedure ReadSignatureAndHash(Message: THandshakeMessage); virtual;
    procedure WriteCertificateMessage(Message: THandshakeMessage; CertList: TCRList); virtual;
    procedure ReadCertificateMessage(Message: THandshakeMessage; CertList: TCRList); virtual;
    procedure WriteCertificateVerifyMessage(Message: THandshakeMessage; Certificate: TScCertificate); virtual;
    procedure WriteServerKeyExchangeSign(Message: THandshakeMessage; Certificate: TScCertificate); virtual;
    procedure VerifyServerKeyExchange(Message: THandshakeMessage;
      RemoteCertificate: TScCertificate); virtual;
    procedure VerifyCertificateMessage(Message: THandshakeMessage; RemoteCertificate: TScCertificate); virtual;

    procedure WriteFinishedMessage(Message: THandshakeMessage); virtual; abstract;
    procedure VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer); virtual; abstract;

    procedure MakeNewSessionTicketMessage(Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo); virtual;
    procedure ParseNewSessionTicketMessage(Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo); virtual;

    procedure DecodeAndVerifyTicket(ClientHelloMessage: THandshakeMessage;
      ClientHelloExtensions: TTLSHelloExtensions; NewSessionInfo: TScSSLSessionInfo;
      out SelectedIdentity: integer); virtual;
    procedure WriteBinderKey(ClientHelloMessage: THandshakeMessage; const MasterSecret, TicketNonce: TBytes); virtual;
    function CreateHMACHash: THashAlgorithm; virtual; abstract;
    function GetProtocol: TScSSLProtocol; virtual; abstract;

    property MasterSecret: TBytes read FMasterSecret write SetMasterSecret;
  end;

  TScTLSCertificateChain = class
  public
    class function MatchesDomainName(const DNSName, DNSMask: string): boolean;
    class function VerifyChain(CertificateList: TCRList; Options: TScSecurityOptions): TScCertificateStatusSet;
  end;

implementation

{$IFNDEF SBRIDGE}
  {$UNDEF TRIAL}
  {$UNDEF TRIALCALL}
{$ENDIF}

uses
  Types,
{$IFNDEF SBRIDGE}
  CRDECUtil, CRFunctions, CRCipher,
{$IFNDEF UNIDACPRO}
  TdsCryptoAPIStorage,
  TdsAlgorithmSupport, TdsCertificateConsts,
  TdsSSL3HandshakeLayer, TdsTLS1HandshakeLayer, TdsTLS13HandshakeLayer;
{$ELSE}
  TdsCryptoAPIStorageUni,
  TdsAlgorithmSupportUni, TdsCertificateConstsUni,
  TdsSSL3HandshakeLayerUni, TdsTLS1HandshakeLayerUni, TdsTLS13HandshakeLayerUni;
{$ENDIF}
{$ELSE}
  ScDECUtil, ScFunctions, ScCipher,
  ScCryptoAPIStorage,
  ScAlgorithmSupport, ScCertificateConsts,
  ScSSL3HandshakeLayer, ScTLS1HandshakeLayer, ScTLS13HandshakeLayer;
{$ENDIF}

{$UNDEF USE_CRYPTOAPI_STORAGE}

{$IFNDEF ANDROID}
{$IFNDEF LINUX}
{$IFNDEF LINUX_BSD}
  {$DEFINE USE_CRYPTOAPI_STORAGE}
{$ENDIF}
{$ENDIF}
{$ENDIF}

var
{$IFDEF USE_CRYPTOAPI_STORAGE}
  CryptoAPISystemRootStorage, CryptoAPISystemCAStorage: TScCryptoAPIStorage;
{$ELSE}
  CryptoAPISystemRootStorage, CryptoAPISystemCAStorage: TScFileStorage;
{$ENDIF}

var
  TLSTicketName, TLSTicketPassword: TBytes;
  PrevTLSTicketName, PrevTLSTicketPassword: TBytes;
  TLSTicketPasswordCreateTime: cardinal = 0;
  TicketPasswordLock: TCriticalSection;

const
  SIZE_OF_SEQUENCE = 8;
  RECORD_LIMITS_ON_KEY_USAGE = 16777216{2^24};

const
  S_master_secret: array[0..12] of byte =  // 'master secret'
    (109, 97, 115, 116, 101, 114, 32, 115, 101, 99, 114, 101, 116);
  S_extended_master_secret: array[0..21] of byte =  // 'extended master secret'
    (101, 120, 116, 101, 110, 100, 101, 100, 32, 109, 97, 115, 116, 101, 114, 32, 115, 101, 99, 114, 101, 116);

constructor TSecureController.Create(Options: TScSecurityOptions; HandshakeLayerClass: THandshakeLayerClass);
begin
  inherited Create;

  FOptions := Options;

  FReceiveMessage := TRecordMessage.Create;
  FRecordLayer := TRecordLayer.Create(Options, HandshakeLayerClass);
  FDecryptedBuffer := TReceiveBuffer.Create(MAX_RECORD_LENGTH);

  FLock := TCriticalSection.Create;
  FLastException := nil;
end;

destructor TSecureController.Destroy;
begin
  try
    if not FOptions.DisableCloseOnDestroy then
      Close;
  except
  end;

  FLastException.Free;
  FLock.Free;

  FDecryptedBuffer.Free;
  FRecordLayer.Free;
  FReceiveMessage.Free;

  inherited;
end;

procedure TSecureController.RaiseLastError;
var
  e: Exception;
begin
  FLock.Enter;
  try
    if FLastException <> nil then begin
      e := FLastException;
      FLastException := nil;
      raise e;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSecureController.ReStart;
begin
  FRecordLayer.Reinitialize;
  StartTLS;
end;

procedure TSecureController.Close;
begin
  CloseConnection(nil);

  // to make sure, that the OnReceive method is completed
  if Assigned(OnClose) then
    OnClose(Self);
end;

procedure TSecureController.StartTLS;
begin
  if not (Assigned(FOnSendDataToTarget) and Assigned(FOnReceiveDataFromSource)) then
    raise EScError.Create(seInvalidInputArgs);

  // check Security options
  if FOptions.Protocols = [] then
    raise EScError.Create(seNormalConnectionIsNotSupported);

  if Length(FOptions.CipherAlgorithms) = 0 then
    raise EScError.Create(seInvalidCipherAlgorithm);

  if (FOptions.Entity = ceServer) and not Assigned(FOptions.OnCertificateRequest) then
    raise EScError.Create(seServerCertificateNotSpecified);

  // Start negotiation if not async mode
  if (FOptions.Entity = ceClient) or not FOptions.AsyncStartTLS then
    InternalStartTLS;
end;

procedure TSecureController.InternalStartTLS;

begin
  if FOptions.Entity = ceClient then begin
    FRecordLayer.CreateClientHelloControlBytes;
    SendEncryptedData;
  end;

  while FRecordLayer.IsNegotiating do begin
    ReceiveDataFromSource;
    ProcessReceivedData;
  end;
end;

procedure TSecureController.ReceiveDataFromSource(Count: integer = 0);
var
  len: integer;
begin
  if Count > 0 then begin
    FReceiveMessage.CheckAndRealloc(Count);
    while Count > 0 do begin
      len := FOnReceiveDataFromSource(TValueArr(FReceiveMessage.Fragment), FReceiveMessage.WriteOffset, Count);
      if len <= 0 then
        raise EScError.Create(seConnectionClosed);
      FReceiveMessage.WriteOffset := FReceiveMessage.WriteOffset + len;
      Dec(Count, len);
    end;
  end
  else
    if FReceiveMessage.WriteOffset <= FReceiveMessage.ReadOffset then begin
      FReceiveMessage.CheckAndRealloc(MAX_RECORD_FRAGMENT_LENGTH * 2);
      len := FOnReceiveDataFromSource(TValueArr(FReceiveMessage.Fragment), FReceiveMessage.WriteOffset, MAX_RECORD_FRAGMENT_LENGTH * 2);
      if len <= 0 then
        raise EScError.Create(seConnectionClosed);
      FReceiveMessage.WriteOffset := FReceiveMessage.WriteOffset + len;
    end;
end;

function TSecureController.ProcessReceivedMessage: TScSSLStatus;
var
  NeedBytes: integer;
begin
  Result := FRecordLayer.ProcessMessage(FReceiveMessage, FDecryptedBuffer, NeedBytes);

  while NeedBytes > 0 do begin
    ReceiveDataFromSource(NeedBytes);
    Result := FRecordLayer.ProcessMessage(FReceiveMessage, FDecryptedBuffer, NeedBytes);
  end;
end;

function TSecureController.ProcessReceivedData: TScSSLStatus;
begin
  try
    Result := ProcessReceivedMessage;
  finally
    // send Handshake messages
    SendEncryptedData; // in finally for send alert if was
  end;
end;

function TSecureController.WriteData(const Data: TValueArr; Offset, Count: integer): integer;
begin
  RaiseLastError;

  if FIsDisposed then
    raise EScError.Create(seConnectionClosed);

  if FRecordLayer.IsNegotiating then begin
    // we should not send application data if a negotiation is in progress
    if (FRecordLayer.WaitNegotiating.WaitFor(cardinal(FOptions.Timeout)) <> wrSignaled) or FIsDisposed then
      raise EScError.Create(seTimeoutSession);
  end;

  FRecordLayer.EncryptAndStore(@Data[Offset], Count, ctApplicationData);
  SendEncryptedData;
  Result := Count;

  RaiseLastError;
end;

procedure TSecureController.SendEncryptedData;
var
  ReadResult: TScReadResult;
  Position: TScSequencePosition;
  MemoryRef: TScMemoryRef;
begin
  FRecordLayer.LockReader;
  try
    if FRecordLayer.FPipeReader.TryRead(ReadResult) then
      if ReadResult.Buffer <> nil then begin
        try
          try
            try
              Position := ReadResult.Buffer.StartPos;
              while ReadResult.Buffer.TryGet(Position, MemoryRef) do
                FOnSendDataToTarget(TValueArr(MemoryRef.Memory), MemoryRef.Offset, MemoryRef.Length);
            finally
              FRecordLayer.FPipeReader.AdvanceTo(ReadResult.Buffer.EndPos);
            end;
          finally
            ReadResult.Buffer.Free;
          end;
        except
          on e: Exception do
            CloseConnection(e);
        end;
      end;
  finally
    FRecordLayer.UnlockReader;
  end;
end;

function TSecureController.ReadDataSync(const Data: TValueArr; Offset, Count: integer): integer;
var
  Status: TScSSLStatus;
begin
  Result := FDecryptedBuffer.Read(Data, Offset, Count);
  if Result > 0 then
    Exit;

  ReceiveDataFromSource(RECORD_HEADER_LENGTH);
  Status := ProcessReceivedData;

  Result := FDecryptedBuffer.Read(Data, Offset, Count);

  if Status = ssShuttedDown then  // Record layer instructs us to shut down
    CloseConnection(nil);
end;

procedure TSecureController.CloseConnection(e: Exception);
begin
  if FIsDisposed then
    Exit;

  FLock.Enter;
  try
    if FIsDisposed then
      Exit;

    FIsDisposed := True;
    FLastException := CloneException(e);
  finally
    FLock.Leave;
  end;

  try
    if Assigned(OnClose) then
      OnClose(Self);
  except
  end;

  FRecordLayer.IsNegotiating := False;
  FDecryptedBuffer.CloseBuffer;
end;

function TSecureController.WaitForData(DataLen: integer; MillisecondsTimeout: cardinal): boolean;
begin
  Result := FDecryptedBuffer.WaitForData(DataLen, MillisecondsTimeout);
end;

procedure TSecureController.QueueRenegotiate;
begin
  if FIsDisposed or FRecordLayer.IsNegotiating then
    raise EScError.Create(seInvalidOperation);

  FRecordLayer.CreateRenegotiateControlBytes;
  SendEncryptedData;
  RaiseLastError;
end;

procedure TSecureController.Shutdown;
begin
  RaiseLastError;

  if not FIsDisposed then begin
    if FRecordLayer.IsNegotiating then
      raise EScError.Create(seInvalidOperation);

    FRecordLayer.CreateShutdownControlBytes;
    SendEncryptedData;
  end;
end;

function TSecureController.GetLastException: Exception;
begin
  Result := FLastException;
end;

function TSecureController.GetAvailable: integer;
begin
  Result := FDecryptedBuffer.DataLength;
end;

function TSecureController.GetRemoteCertificate: TScCertificate;
begin
  Result := FRecordLayer.HandshakeLayer.FRemoteCertificate;
end;

function TSecureController.GetSessionInfo: TScSSLSessionInfo;
begin
  Result := FRecordLayer.HandshakeLayer.FSessionInfo;
end;

function TSecureController.GetAfterReceiveAppData: TNotifyEvent;
begin
  Result := FDecryptedBuffer.OnReceiveData;
end;

procedure TSecureController.SetAfterReceiveAppData(Value: TNotifyEvent);
begin
  FDecryptedBuffer.OnReceiveData := Value;
end;

{ TRecordLayer }

constructor TRecordLayer.Create(Options: TScSecurityOptions; HandshakeLayerClass: THandshakeLayerClass);
var
  PipeOptions: TScPipeOptions;
begin
  inherited Create;

  if Random = nil then
    raise Exception.Create(SInternalError);
  FRandom := Random;

  FLockWriter := TCriticalSection.Create;
  FLockReader := TCriticalSection.Create;

  PipeOptions.MinimumSegmentSize := DefaultMinimumSegmentSize;
  PipeOptions.PauseWriterThreshold := 0;
  PipeOptions.ResumeWriterThreshold := 0;
  FPipe := TScPipe.Create(PipeOptions);
  FPipeReader := FPipe.Reader;
  FPipeWriter := FPipe.Writer;

  FSendMessage := TRecordMessageInfo.Create;
  FWaitNegotiating := CreateEvent;
  FEncryptor := nil;
  FDecryptor := nil;
  FLocalHasher := nil;
  FRemoteHasher := nil;
  FLocalCompressor := nil;
  FRemoteCompressor := nil;
  FExpansionFragmentLength := 2048;
  FMaxRecordLength := MAX_RECORD_LENGTH;

  FOptions := Options;
  FHandshakeLayerClass := HandshakeLayerClass;
  FHandshakeLayer := FHandshakeLayerClass.Create(Self, Options);
end;

destructor TRecordLayer.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  FHandshakeLayer.Free;
{$ELSE}
  FHandshakeLayer := nil;
{$ENDIF}

  FLocalCompressor := nil;
  FRemoteCompressor := nil;
  FEncryptor.Free;
  FDecryptor.Free;
  FLocalHasher.Free;
  FRemoteHasher.Free;
  FLockWriter.Free;
  FLockReader.Free;
  FSendMessage.Free;
  FWaitNegotiating.Free;
  FPipe.Free;

  inherited;
end;

procedure TRecordLayer.Reinitialize;
begin
  FLockWriter.Enter;
  FLockReader.Enter;
  try
  {$IFNDEF AUTOREFCOUNT}
    FreeAndNil(FHandshakeLayer);
  {$ELSE}
    FHandshakeLayer := nil;
  {$ENDIF}

    FInputSequenceNumber := 0;
    FOutputSequenceNumber := 0;

    FLocalCompressor := nil;
    FRemoteCompressor := nil;
    FreeAndNil(FEncryptor);
    FreeAndNil(FDecryptor);
    FreeAndNil(FLocalHasher);
    FreeAndNil(FRemoteHasher);

    FHandshakeLayer := FHandshakeLayerClass.Create(Self, FOptions);
  finally
    FLockWriter.Leave;
    FLockReader.Leave;
  end;
end;

procedure TRecordLayer.ChangeLocalState(const Encryptor: TSymmetricAlgorithm;
  const LocalIV: TBytes; const LocalHasher: TKeyedHashAlgorithm);
begin
  if Encryptor = nil then
    raise EScError.Create(seInvalidInputArgs);

  FOutputSequenceNumber := 0;

  FreeAndNil(FEncryptor);
  FEncryptor := Encryptor;

  if FEncryptor.Mode = cmGCM then begin
    if (Length(LocalIV) < 4) or (Length(LocalIV) > 12) then
      FHandshakeLayer.SendAlert(adDecodeError, seInvalidInputArgs);

    SetLength(FLocalIV, 12);
    Move(LocalIV[0], FLocalIV[0], Length(LocalIV));
    if Length(LocalIV) < 12 then
      FRandom.Random(FLocalIV, Length(LocalIV), 12 - Length(LocalIV));

    if FHandshakeLayer.GetProtocol = spTls13 then
      SetLength(FLocalAAD, 5)
    else
      SetLength(FLocalAAD, 13);
  end;

  FreeAndNil(FLocalHasher);
  FLocalHasher := LocalHasher;

  if FHandshakeLayer.FSessionInfo.Compression = csRequired then
    FLocalCompressor := TScZCompression.Create
  else
    FLocalCompressor := nil;
end;

procedure TRecordLayer.ChangeRemoteState(const Decryptor: TSymmetricAlgorithm;
  const RemoteIV: TBytes; const RemoteHasher: TKeyedHashAlgorithm);
begin
  if Decryptor = nil then
    raise EScError.Create(seInvalidInputArgs);

  FInputSequenceNumber := 0;

  FreeAndNil(FDecryptor);
  FDecryptor := Decryptor;

  if FDecryptor.Mode = cmGCM then begin
    if (Length(RemoteIV) < 4) or (Length(RemoteIV) > 12) then
      FHandshakeLayer.SendAlert(adDecodeError, seInvalidInputArgs);

    SetLength(FRemoteIV, 12);
    Move(RemoteIV[0], FRemoteIV[0], Length(RemoteIV));

    if Length(RemoteIV) < 12 then
      FillChar(FRemoteIV[Length(RemoteIV)], 12 - Length(RemoteIV), 0);

    if FHandshakeLayer.GetProtocol = spTls13 then
      SetLength(FRemoteAAD, 5)
    else
      SetLength(FRemoteAAD, 13);
  end;

  FreeAndNil(FRemoteHasher);
  FRemoteHasher := RemoteHasher;

  if FRemoteHasher <> nil then
    SetLength(FRemoteMac, FRemoteHasher.HashSize);

  if FHandshakeLayer.FSessionInfo.Compression = csRequired then
    FRemoteCompressor := TScZCompression.Create
  else
    FRemoteCompressor := nil;

  if FHandshakeLayer.GetProtocol = spTls13 then
    FExpansionFragmentLength := 256
  else
  if FRemoteCompressor <> nil then
    FExpansionFragmentLength := 2048
  else
    FExpansionFragmentLength := 1024;
end;

class procedure TRecordLayer.IncVector(var Vector: TBytes);
var
  i: integer;
begin
  for i := Length(Vector) - 1 downto 4 do begin
    Vector[i] := byte(Vector[i] + 1);
    if Vector[i] <> 0 then
      break;
  end;
end;

class function TRecordLayer.GetSequenceBytes(const Number: Int64): TBytes;
begin
  Result := BitConverter.GetBytes(Number);
  if BitConverter.IsLittleEndian then
    ArrayReverse(Result, 0, Length(Result)); // TLS uses big endian [network] byte order
end;

procedure TRecordLayer.WrapMessage(const Data: TValueArr; Size: integer);
var
  Memory: TScMemoryRef;
  WriteOffset: integer;
  Protocol: TScSSLProtocol;
  LocalIV: array[0..11] of byte;
  NeedSendIV: boolean;
  CompressSize: cardinal;
  MsgLen, IVLen, MacLen, obs: integer;
  Padding: byte;
  i: integer;
  FlushResult: TScFlushResult;
begin
  Protocol := FHandshakeLayer.GetProtocol;

  Memory := FPipeWriter.GetMemory(Size + 2048);
  Memory.Memory := PtrOffset(Memory.Memory, Memory.Offset);
  Memory.Offset := 0;
  WriteOffset := RECORD_HEADER_LENGTH;

  // encrypt the message and MAC
  if (FEncryptor <> nil) and
    not ((Protocol = spTls13) and (FSendMessage.ContentType = ctChangeCipherSpec) and FHandshakeLayer.IsNegotiating) then
  begin
    // write IV
    IVLen := 0;
    NeedSendIV := (Protocol in [spTls11, spTls12]) and (FEncryptor.Mode = cmCBC);
    if NeedSendIV then begin
      FRandom.Random(@Memory.Memory[WriteOffset], FEncryptor.BlockSize);
      FEncryptor.SetIV(TValueArr(Memory.Memory), WriteOffset, FEncryptor.BlockSize);
      Inc(WriteOffset, FEncryptor.BlockSize);
    end
    else
    if (FEncryptor.Mode = cmGCM) and (Protocol <> spTls13) then begin
      Move(FLocalIV[4], Memory.Memory[WriteOffset], 8); {nonce}
      Inc(WriteOffset, 8);
      IVLen := 8;
      FEncryptor.SetIV(TValueArr(FLocalIV), 0, Length(FLocalIV));
      IncVector(FLocalIV);
    end;

    // Compress data
    if (FLocalCompressor <> nil) and (Size > 0) then begin
      CompressSize := Memory.Length - WriteOffset;
      FLocalCompressor.Compress(Data, 0, Size, TValueArr(Memory.Memory), WriteOffset, CompressSize, True);
      FSendMessage.DataLength := CompressSize;
    end
    else begin
      if Size > 0 then
        Move(Data^, Memory.Memory[WriteOffset], Size);
      FSendMessage.DataLength := Size;
    end;

    // calculate the MAC
    if FLocalHasher <> nil then begin
      FSendMessage.InitHeader(Memory.Memory, FSendMessage.DataLength);

      FLocalHasher.Initialize;
      FLocalHasher.TransformBlock(GetSequenceBytes(FOutputSequenceNumber), 0, SIZE_OF_SEQUENCE); // seq_num + ..

      if Protocol = spSsl3 then begin
        FLocalHasher.TransformBlock(TValueArr(Memory.Memory), 0, 1, TValueArr(Memory.Memory), 0); // .. + type
        FLocalHasher.TransformFinalBlock(@TValueArr(Memory.Memory)[3], FSendMessage.DataLength + 2); // .. + length + fragment
      end
      else begin
        FLocalHasher.TransformBlock(TValueArr(Memory.Memory), 0, RECORD_HEADER_LENGTH, TValueArr(Memory.Memory), 0);  // .. + type + version + length
        FLocalHasher.TransformFinalBlock(@TValueArr(Memory.Memory)[WriteOffset], FSendMessage.DataLength); // .. + fragment
      end;

      MacLen := FLocalHasher.HashSize;
      Move(FLocalHasher.Hash[0], Memory.Memory[WriteOffset + FSendMessage.DataLength], MacLen);
    end
    else
      MacLen := 0;

    // encrypt the message
    case FEncryptor.Mode of
      cmECB: begin // is stream cipher
        FSendMessage.MessageLength := FSendMessage.DataLength + MacLen;
      end;

      cmCBC: begin
        obs := FEncryptor.BlockSize;
        Padding := Byte((obs - (FSendMessage.DataLength + MacLen + 1) mod obs) mod obs);
        FSendMessage.MessageLength := FSendMessage.DataLength + MacLen + Padding + 1;

        if Protocol = spSsl3 then begin
          FRandom.Random(@TValueArr(Memory.Memory)[WriteOffset + FSendMessage.DataLength + MacLen], Padding);
          Memory.Memory[WriteOffset + FSendMessage.MessageLength - 1] := Padding;
        end
        else
          for i := FSendMessage.DataLength + MacLen to FSendMessage.MessageLength - 1 do
            Memory.Memory[WriteOffset + i] := Padding;
      end;

      cmGCM: begin
        if Protocol = spTls13 then begin
          PutInt64BE(FOutputSequenceNumber, TValueArr(@LocalIV), 4);
          Move(FLocalIV[0], LocalIV[0], 4);
          for i := 4 to 11 do
            LocalIV[i] := LocalIV[i] xor FLocalIV[i];
          FEncryptor.SetIV(TValueArr(@LocalIV), 0, Length(LocalIV));

          Memory.Memory[WriteOffset + FSendMessage.DataLength] := CONTENT_TYPE_CODES[FSendMessage.ContentType];
          FSendMessage.ContentType := ctApplicationData;
          Inc(FSendMessage.DataLength);
          FSendMessage.MessageLength := FSendMessage.DataLength;
          MsgLen := FSendMessage.MessageLength + FEncryptor.BlockSize{Tag};

          FLocalAAD[0] := CONTENT_TYPE_CODES[ctApplicationData];
          FLocalAAD[1] := FSendMessage.Version.Major;
          FLocalAAD[2] := FSendMessage.Version.Minor;
          FLocalAAD[3] := Byte(MsgLen shr 8);
          FLocalAAD[4] := Byte(MsgLen);
          FEncryptor.AAD := FLocalAAD;
        end
        else begin
          PutInt64BE(FOutputSequenceNumber, TValueArr(FLocalAAD), 0);
          FLocalAAD[8] := CONTENT_TYPE_CODES[FSendMessage.ContentType];
          FLocalAAD[9] := FSendMessage.Version.Major;
          FLocalAAD[10] := FSendMessage.Version.Minor;
          FLocalAAD[11] := Byte(FSendMessage.DataLength shr 8);
          FLocalAAD[12] := Byte(FSendMessage.DataLength);
          FEncryptor.AAD := FLocalAAD;
          FSendMessage.MessageLength := FSendMessage.DataLength;
        end;

      end;
    else
      Assert(False);
    end;

    FEncryptor.EncodeBuffer(TValueArr(Memory.Memory), WriteOffset, FSendMessage.MessageLength,
      TValueArr(Memory.Memory), WriteOffset);

    Inc(WriteOffset, FSendMessage.MessageLength);

    if NeedSendIV then
      FSendMessage.MessageLength := FSendMessage.MessageLength + FEncryptor.BlockSize{IV}
    else
    if FEncryptor.Mode = cmGCM then begin
      // write Tag
      Move(FEncryptor.Tag[0], Memory.Memory[WriteOffset], FEncryptor.BlockSize);
      Inc(WriteOffset, FEncryptor.BlockSize);
      FSendMessage.MessageLength := FSendMessage.MessageLength + IVLen{nonce} + FEncryptor.BlockSize{Tag};
    end;

    // final adjustments
    Inc(FOutputSequenceNumber);
  end
  else begin
    if Size > 0 then begin
      Move(Data^, Memory.Memory[WriteOffset], Size);
      Inc(WriteOffset, Size);
    end;
    FSendMessage.MessageLength := Size;
  end;

  FSendMessage.InitHeader(Memory.Memory, FSendMessage.MessageLength);

  FPipeWriter.Advance(WriteOffset);
  FlushResult := FPipeWriter.Flush;
  if FlushResult.IsCanceled or FlushResult.IsCompleted then
    FHandshakeLayer.SendAlert(adUnexpectedMessage, seErrorHandshakeProcedure);
end;

procedure TRecordLayer.UnwrapMessage(Message: TRecordMessage);
var
  Protocol: TScSSLProtocol;
  RemoteIV: array[0..11] of byte;
  IsSentIV, CipherError: boolean;
  Padding: byte;
  RealLen: integer;
  DataOffset: integer;
  InOffset, InLength, OutOffset, OutLen: cardinal;
  i: integer;
begin
  CipherError := False;
  Protocol := FHandshakeLayer.GetProtocol;

  // decrypt and verify the message
  if (FDecryptor <> nil) and not ((Protocol = spTls13) and (Message.ContentType = ctChangeCipherSpec) and FHandshakeLayer.IsNegotiating) then begin
    if (FRemoteHasher <> nil) and (Message.MessageLength <= FRemoteHasher.HashSize) then
      FHandshakeLayer.SendAlert(adBadRecordMac, seInvalidMessageLength);

    IsSentIV := (Protocol in [spTls11, spTls12]) and (FDecryptor.Mode = cmCBC);

    DataOffset := Message.ReadOffset + RECORD_HEADER_LENGTH;

    // decrypt the message
    case FDecryptor.Mode of
      cmECB: begin
        FDecryptor.DecodeBuffer(TValueArr(Message.Fragment), DataOffset, Message.MessageLength,
          TValueArr(Message.Fragment), DataOffset);

        Buffer.BlockCopy(Message.Fragment, DataOffset + Message.MessageLength - Length(FRemoteMac), FRemoteMac, 0, Length(FRemoteMac));

        Message.DataLength := Message.MessageLength - Length(FRemoteMac);
      end;

      cmCBC: begin
        if (Message.MessageLength mod FDecryptor.BlockSize) <> 0 then
          FHandshakeLayer.SendAlert(adBadRecordMac, seInvalidMessageLength);

        if IsSentIV then begin
          FDecryptor.SetIV(TValueArr(Message.Fragment), DataOffset, FDecryptor.BlockSize);
          Message.MessageLength := Message.MessageLength - FDecryptor.BlockSize;
          DataOffset := DataOffset + FDecryptor.BlockSize;
        end;

        FDecryptor.DecodeBuffer(TValueArr(Message.Fragment), DataOffset, Message.MessageLength,
          TValueArr(Message.Fragment), DataOffset);
        Padding := Message.Fragment[DataOffset + Message.MessageLength - 1];

        /// see tools.ietf.org/html/rfc5246#section-6.2.3.2
        if Message.MessageLength < (Padding + FRemoteHasher.HashSize + 1) then begin
          CipherError := True;
          Message.DataLength := Message.MessageLength - 1 - FRemoteHasher.HashSize;
        end
        else begin
          RealLen := Message.MessageLength - Padding - 1;
          Buffer.BlockCopy(Message.Fragment, DataOffset + RealLen - Length(FRemoteMac), FRemoteMac, 0, Length(FRemoteMac));

          if Protocol <> spSsl3 then begin // check padding
            for i := 0 to Padding - 1 do
              if Message.Fragment[DataOffset + RealLen + i] <> Padding then
                CipherError := True; /// https://www.ietf.org/rfc/rfc4346.txt [page 23]
          end;

          Message.DataLength := RealLen - Length(FRemoteMac);
        end;
      end;

      cmGCM: begin
        if Protocol = spTls13 then begin
          if Message.MessageLength < FDecryptor.BlockSize{Tag} then
            FHandshakeLayer.SendAlert(adBadRecordMac, seInvalidMessageLength);

          PutInt64BE(FInputSequenceNumber, TValueArr(@RemoteIV), 4);
          Move(FRemoteIV[0], RemoteIV[0], 4);
          for i := 4 to 11 do
            RemoteIV[i] := RemoteIV[i] xor FRemoteIV[i];
          FDecryptor.SetIV(TValueArr(@RemoteIV), 0, Length(RemoteIV));

          FRemoteAAD[0] := CONTENT_TYPE_CODES[Message.ContentType];
          FRemoteAAD[1] := Message.Version.Major;
          FRemoteAAD[2] := Message.Version.Minor;
          FRemoteAAD[3] := Byte(Message.MessageLength shr 8);
          FRemoteAAD[4] := Byte(Message.MessageLength);
          FDecryptor.AAD := FRemoteAAD;

          Message.DataLength := Message.MessageLength - FDecryptor.BlockSize{Tag};
          FDecryptor.SetReceivedTag(TValueArr(Message.Fragment), DataOffset + Message.DataLength, FDecryptor.BlockSize);
          FDecryptor.DecodeBuffer(TValueArr(Message.Fragment), DataOffset, Message.DataLength,
            TValueArr(Message.Fragment), DataOffset);

          while (Message.DataLength > 0) and (Message.Fragment[DataOffset + Message.DataLength - 1] = 0) do
            Dec(Message.DataLength);

          if Message.DataLength = 0 then
            FHandshakeLayer.SendAlert(adBadRecordMac, seCorruptMessage);

          Message.ContentType := TScLayersHelper.FindContentType(Message.Fragment[DataOffset + Message.DataLength - 1]);
          if Message.ContentType = ctChangeCipherSpec then
            FHandshakeLayer.SendAlert(adBadRecordMac, seCorruptMessage);

          Dec(Message.DataLength);
        end
        else begin
          if Message.MessageLength < (8{nonce} + FDecryptor.BlockSize{Tag}) then
            FHandshakeLayer.SendAlert(adBadRecordMac, seInvalidMessageLength);

          // Set IV
          Move(Message.Fragment[DataOffset], FRemoteIV[4], 8);
          FDecryptor.SetIV(TValueArr(FRemoteIV), 0, Length(FRemoteIV));

          DataOffset := DataOffset + 8;
          Message.ReadOffset := Message.ReadOffset + 8;
          Message.MessageLength := Message.MessageLength - 8{nonce};
          Message.DataLength := Message.MessageLength - FDecryptor.BlockSize{Tag};

          PutInt64BE(FInputSequenceNumber, TValueArr(FRemoteAAD), 0);
          FRemoteAAD[8] := CONTENT_TYPE_CODES[Message.ContentType];
          FRemoteAAD[9] := Message.Version.Major;
          FRemoteAAD[10] := Message.Version.Minor;
          FRemoteAAD[11] := Byte(Message.DataLength shr 8);
          FRemoteAAD[12] := Byte(Message.DataLength);
          FDecryptor.AAD := FRemoteAAD;
          FDecryptor.SetReceivedTag(TValueArr(Message.Fragment), DataOffset + Message.DataLength, FDecryptor.BlockSize);
          FDecryptor.DecodeBuffer(TValueArr(Message.Fragment), DataOffset, Message.DataLength,
            TValueArr(Message.Fragment), DataOffset);
        end;
      end;
    else
      Assert(False);
    end;

    // calculate the MAC
    if FRemoteHasher <> nil then begin
      FRemoteHasher.Initialize;
      FRemoteHasher.TransformBlock(GetSequenceBytes(FInputSequenceNumber), 0, SIZE_OF_SEQUENCE); // seq_num + ..

      Message.InitHeader(Message.DataLength);
      if Protocol = spSsl3 then begin
        FRemoteHasher.TransformBlock(Message.Fragment, Message.ReadOffset, 1); // .. + type
        FRemoteHasher.TransformFinalBlock(Message.Fragment, Message.ReadOffset + 3, Message.DataLength + 2); // .. + length + fragment
      end
      else begin
        FRemoteHasher.TransformBlock(Message.Fragment, Message.ReadOffset, RECORD_HEADER_LENGTH); // .. + type + version + length
        if IsSentIV then
          Message.ReadOffset := Message.ReadOffset + FDecryptor.BlockSize;
        FRemoteHasher.TransformFinalBlock(Message.Fragment, Message.ReadOffset + RECORD_HEADER_LENGTH, Message.DataLength); // .. + fragment
      end;

      // compare MACs
      if (Length(FRemoteHasher.Hash) <> Length(FRemoteMac)) or
         (MemCompare(@FRemoteHasher.Hash[0], @FRemoteMac[0], Length(FRemoteMac)) <> 0) then
        CipherError := True;
    end;

    // raise cipher error, if necessary
    if CipherError then
      FHandshakeLayer.SendAlert(adBadRecordMac, seCorruptMessage);

    Message.ReadOffset := Message.ReadOffset + RECORD_HEADER_LENGTH;

    if (FRemoteCompressor <> nil) and (Message.DataLength > 0) then begin
      if Message.Fragment = Message.Uncompress then
        Message.Uncompress := nil;
      if Length(Message.Uncompress) < (Message.DataLength shl 2) + Message.ReadOffset then
        SetLength(Message.Uncompress, (Message.DataLength shl 2) + Message.ReadOffset);

      InOffset := Message.ReadOffset;
      InLength := Message.DataLength;
      OutOffset := InOffset;
      OutLen := Length(Message.Uncompress) - Message.ReadOffset;

      while not FRemoteCompressor.Decompress(TValueArr(Message.Fragment), InOffset, InLength, TValueArr(Message.Uncompress), OutOffset, OutLen) do begin
        Inc(OutOffset, OutLen);
        OutLen := InLength shl 2;
        SetLength(Message.Uncompress, Length(Message.Uncompress) + integer(OutLen));
      end;

      Message.DataLength := integer(OutOffset + OutLen) - Message.ReadOffset;
    end
    else
      Message.Uncompress := Message.Fragment;

    // final adjustments
    Inc(FInputSequenceNumber);
  end
  else begin
    Message.Uncompress := Message.Fragment;
    Message.DataLength := Message.MessageLength;
    Message.ReadOffset := Message.ReadOffset + RECORD_HEADER_LENGTH;
  end;
end;

procedure TRecordLayer.EncryptAndStoreMessage(Message: THandshakeMessage);
begin
  EncryptAndStore(TValueArr(@Message.Fragment[Message.DataOffset - HANDSHAKE_HEADER_LENGTH]),
    Message.WriteOffset - Message.DataOffset + HANDSHAKE_HEADER_LENGTH, ctHandshake);
end;

procedure TRecordLayer.EncryptAndStore(const Data: TValueArr; Size: integer; ContentType: TScContentType);
var
  Offset, Count: integer;
begin
  FLockWriter.Enter;
  try
    FSendMessage.ContentType := ContentType;
    FSendMessage.Version := TScSSLVersionHelper.GetVersion(FHandshakeLayer.GetProtocol);
    Offset := 0;

    repeat //!!! to send an empty message (don't change to while)
      if Size > FMaxRecordLength then
        Count := FMaxRecordLength
      else
        Count := Size;

      WrapMessage(@Data[Offset], Count);
      Inc(Offset, Count);
      Dec(Size, Count);
    until Size = 0;

    if FOutputSequenceNumber > RECORD_LIMITS_ON_KEY_USAGE then
    if (FOutputSequenceNumber > RECORD_LIMITS_ON_KEY_USAGE) and ((FOptions.Entity = ceServer) or FOptions.UseClientInitiatedRenegotiation) then
      FHandshakeLayer.MakeRenegotiateMessage;
  finally
    FLockWriter.Leave;
  end;
end;

procedure TRecordLayer.LockWriter;
begin
  FLockWriter.Enter;
end;

procedure TRecordLayer.UnlockWriter;
begin
  FLockWriter.Leave;
end;

procedure TRecordLayer.LockReader;
begin
  FLockReader.Enter;
end;

procedure TRecordLayer.UnlockReader;
begin
  FLockReader.Leave;
end;

function TRecordLayer.ProcessMessage(RecordMessage: TRecordMessage; AppDecryptedBuffer: TReceiveBuffer;
  out NeedBytes: integer): TScSSLStatus;

  procedure CalcRecordMessageSize;
  var
    Count: integer;
  begin
    Count := RecordMessage.WriteOffset - RecordMessage.ReadOffset;
    if Count < RECORD_HEADER_LENGTH then begin
      RecordMessage.MessageLength := 0;
      if Count = 0 then
        NeedBytes := 0
      else
        NeedBytes := RECORD_HEADER_LENGTH - Count;
    end
    else begin
      RecordMessage.MessageLength := (RecordMessage.Fragment[RecordMessage.ReadOffset + 3] shl 8) +
        RecordMessage.Fragment[RecordMessage.ReadOffset + 4];
      if RecordMessage.MessageLength > FMaxRecordLength + FExpansionFragmentLength then
        FHandshakeLayer.SendAlert(adRecordOverflow, seInvalidMessageLength);

      NeedBytes := RecordMessage.MessageLength + RECORD_HEADER_LENGTH - Count;
    end;
  end;

begin
  Result := ssMessageIncomplete;

  // extract all record messages, if any, and process them
  CalcRecordMessageSize;
  if NeedBytes > 0 then
    Exit;

  while (RecordMessage.MessageLength > 0) and (NeedBytes <= 0) do begin
    RecordMessage.ContentType := TScLayersHelper.FindContentType(RecordMessage.Fragment[RecordMessage.ReadOffset]);
    RecordMessage.Version.Major := RecordMessage.Fragment[RecordMessage.ReadOffset + 1];
    RecordMessage.Version.Minor := RecordMessage.Fragment[RecordMessage.ReadOffset + 2];

    UnwrapMessage(RecordMessage); // decrypt and verify message
    if RecordMessage.DataLength > FMaxRecordLength then
      FHandshakeLayer.SendAlert(adRecordOverflow, seInvalidMessageLength);

    // process message
    if RecordMessage.ContentType = ctApplicationData then begin
      if FHandshakeLayer.IsNegotiating then
        FHandshakeLayer.SendAlert(adUnexpectedMessage, seErrorHandshakeProcedure)
      else
        AppDecryptedBuffer.Write(TValueArr(RecordMessage.Uncompress), RecordMessage.ReadOffset, RecordMessage.DataLength);

      Result := ssOK;
    end
    else
    if RecordMessage.ContentType = ctChangeCipherSpec then
      Result := FHandshakeLayer.ProcessChangeCipherSpec(RecordMessage)
    else
    if RecordMessage.ContentType = ctHandshake then
      Result := FHandshakeLayer.ProcessMessage(RecordMessage)
    else
    if RecordMessage.ContentType = ctAlert then
      Result := FHandshakeLayer.ProcessAlert(RecordMessage)
    else
      FHandshakeLayer.SendAlert(adUnexpectedMessage, seUnknownMessageType);

    RecordMessage.ReadOffset := RecordMessage.ReadOffset + RecordMessage.MessageLength;
    if Result = ssShuttedDown then
      break;

    CalcRecordMessageSize;
  end;

  RecordMessage.ReduceBuffer;

  if FHandshakeLayer.FIsRenegotiationStarted then begin
    if FInputSequenceNumber > (RECORD_LIMITS_ON_KEY_USAGE + 20) then
      FHandshakeLayer.SendAlert(adUnexpectedMessage, seErrorHandshakeProcedure);
  end
  else
  if (FInputSequenceNumber > RECORD_LIMITS_ON_KEY_USAGE) and ((FOptions.Entity = ceServer) or FOptions.UseClientInitiatedRenegotiation) then
    FHandshakeLayer.MakeRenegotiateMessage;
end;

procedure TRecordLayer.CreateClientHelloControlBytes;
begin
  LockWriter;
  try
    FHandshakeLayer.MakeClientHelloMessage;
  finally
    UnlockWriter;
  end;
end;

procedure TRecordLayer.CreateRenegotiateControlBytes;
begin
  LockWriter;
  try
    FHandshakeLayer.MakeRenegotiateMessage;
  finally
    UnlockWriter;
  end;
end;

procedure TRecordLayer.CreateShutdownControlBytes;
begin
  LockWriter;
  try
    FHandshakeLayer.MakeShutdownMessage;
  finally
    UnlockWriter;
  end;
end;

function TRecordLayer.GetIsNegotiating: boolean;
begin
  Result := FHandshakeLayer.IsNegotiating;
end;

procedure TRecordLayer.SetIsNegotiating(Value: boolean);
begin
  FHandshakeLayer.IsNegotiating := Value;
end;

{ THandshakeLayer }

constructor THandshakeLayer.Create(RecordLayer: TRecordLayer; Options: TScSecurityOptions);
begin
  inherited Create;

  if RecordLayer = nil then
    raise EScError.Create(seInvalidInputArgs);
  if Options = nil then
    raise EScError.Create(seInvalidInputArgs);

  if Random = nil then
    raise Exception.Create(SInternalError);
  FRandom := Random;

  FOwnCertificateList := TCRList.Create;
  FSessionInfo := TScSSLSessionInfo.Create;
  FShutdownLock := TCriticalSection.Create;
  FSendMessage := THandshakeMessage.Create;
  FRecordLayer := RecordLayer;
  FOptions := Options;
  FService := nil;

  FState := htNothing;
  SetLength(FProcessMessageBuf, MAX_RECORD_LENGTH * 2);
  FIsNegotiating := True;
  FUseExtendedMasterSecret := False;
  FInitialized := False;
  FFinished := False;
  FIsRenegotiationStarted := False;

  FSessionInfo.CipherAlgorithm := caNone;
  FRemoteCertificate := nil;

  SetLength(FClientServerRandom, 64);

  if FOptions.NewSessionTicketLifetime > 604800 then
    FOptions.NewSessionTicketLifetime := 604800;

  if (FOptions.AssignedSessionInfo <> nil) and FOptions.AssignedSessionInfo.Initialized then
    FSessionInfo.Assign(FOptions.AssignedSessionInfo);

  case TScSSLVersionHelper.GetMinProtocol(FOptions.Protocols) of
    spTls13:
      SetService(TTls13HandshakeProtocol.Create(Self, FOptions.Entity));
    spTls12:
      SetService(TTls12HandshakeProtocol.Create(Self, FOptions.Entity));
    spTls11:
      SetService(TTls11HandshakeProtocol.Create(Self, FOptions.Entity));
    spTls1:
      SetService(TTls10HandshakeProtocol.Create(Self, FOptions.Entity));
    spSsl3:
      SetService(TSsl3HandshakeProtocol.Create(Self, FOptions.Entity));
  else
    Assert(False);
  end;

  Init;
end;

destructor THandshakeLayer.Destroy;
begin
  ClearNegotiationInfo;
  ClearKeyExchangeInfo;
  FService.Free;

  FSendMessage.Free;
  FOwnCertificateList.Free;
  FRemoteCertificate.Free;
  FSessionInfo.Free;
  FShutdownLock.Free;

  inherited;
end;

procedure THandshakeLayer.Init;
begin
  FIsChangeCipherSpecSent := False;
  FIsSessionResumption := False;
end;

procedure THandshakeLayer.CheckAndChangeProtocol(const RemoteProtocol: TScSSLProtocol);
begin
  if FInitialized then begin
    if GetProtocol <> RemoteProtocol then
      SendAlert(adProtocolVersion, seNotAgreeOnProtocol);
  end
  else
  if RemoteProtocol in FOptions.Protocols then begin
    if GetProtocol <> RemoteProtocol then begin
      case RemoteProtocol of
        spTls13:
          SetService(TTls13HandshakeProtocol.Create(FService, FOptions.Entity));
        spTls12:
          SetService(TTls12HandshakeProtocol.Create(FService, FOptions.Entity));
        spTls11:
          SetService(TTls11HandshakeProtocol.Create(FService, FOptions.Entity));
        spTls1:
          SetService(TTls10HandshakeProtocol.Create(FService, FOptions.Entity));
        spSsl3:
          SetService(TSsl3HandshakeProtocol.Create(FService, FOptions.Entity));
      else
        Assert(False);
      end;
    end;
  end
  else
    SendAlert(adProtocolVersion, seNotAgreeOnProtocol);
end;

procedure THandshakeLayer.SetService(Value: THandshakeProtocolService);
begin
  FreeAndNil(FService);
  FService := Value;
  FSessionInfo.Protocol := FService.GetProtocol;
end;

procedure THandshakeLayer.InitializeCiphers(CipherPart: TCipherPart);
var
  CipherSuite: TCipherSuite;
begin
  CipherSuite := FService.InitializeCipherSuite(SSLCipherDefinitions[FSessionInfo.CipherAlgorithm], CipherPart);
  try
    if (CipherPart = cpLocal) or (CipherPart = cpBoth) then begin
      FRecordLayer.ChangeLocalState(CipherSuite.Encryptor, CipherSuite.LocalIV, CipherSuite.LocalHasher);
      CipherSuite.Encryptor := nil;
      CipherSuite.LocalHasher := nil;
    end;

    if (CipherPart = cpRemote) or (CipherPart = cpBoth) then begin
      FRecordLayer.ChangeRemoteState(CipherSuite.Decryptor, CipherSuite.RemoteIV, CipherSuite.RemoteHasher);
      CipherSuite.Decryptor := nil;
      CipherSuite.RemoteHasher := nil;
    end;
  finally
    CipherSuite.Free;
  end;
end;

procedure THandshakeLayer.ClearNegotiationInfo;
begin
  FService.Clear;
end;

procedure THandshakeLayer.ClearKeyExchangeInfo;
begin
  FillChar(FClientServerRandom[0], Length(FClientServerRandom), 0);

  FreeAndNil(FCipherSuite);
end;

function THandshakeLayer.GetHandshakeMessage(const Buf: TBytes; Offset, Count: integer): THandshakeMessage;
var
  Size: integer;
begin
  Result := nil;
  if (Offset + 4) > Count then
    Exit;

  Size := (Buf[Offset + 1] shl 16) + (Buf[Offset + 2] shl 8) + Buf[Offset + 3];
  if (Offset + 4 + Size) > Count then
    Exit;

  Result := THandshakeMessage.Create(Buf, Offset);
end;

function THandshakeLayer.ProcessMessage(Message: TRecordMessage): TScSSLStatus;
var
  hm: THandshakeMessage;
  MessageBuf: TBytes;
  PrevCount, Offset, Count: integer;
  IsFirst: boolean;
begin
  // https://tools.ietf.org/html/rfc8446#section-5.1
  if Message.DataLength = 0 then
    SendAlert(adDecodeError, seInvalidMessage);

  Result := ssOK;
  FRecordLayer.LockWriter;
  try
    PrevCount := FProcessMessageCount;

    if PrevCount > 0 then begin
      // copy the new bytes and the old bytes in one buffer
      if Length(FProcessMessageBuf) < FProcessMessageCount + Message.DataLength then
        SetLength(FProcessMessageBuf, FProcessMessageCount + Message.DataLength);

      Buffer.BlockCopy(Message.Uncompress, Message.ReadOffset, FProcessMessageBuf, FProcessMessageCount, Message.DataLength);
      Inc(FProcessMessageCount, Message.DataLength);

      MessageBuf := FProcessMessageBuf;
      Offset := 0;
      Count := FProcessMessageCount;
    end
    else begin
      MessageBuf := Message.Uncompress;
      Offset := Message.ReadOffset;
      Count := Message.ReadOffset + Message.DataLength;
    end;

    IsFirst := True;
    // loop through all messages in buffer, if any
    hm := GetHandshakeMessage(MessageBuf, Offset, Count);
    while hm <> nil do begin
      try
        // https://tools.ietf.org/html/rfc8446#section-5.1
        if ((hm.HandshakeType in [htClientHello, htServerHello, htEndOfEarlyData, htKeyUpdate]) and
           ((PrevCount > 0) or not IsFirst)) or
           ((hm.HandshakeType = htFinished) and (PrevCount > 0)) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

        InternalProcessMessage(hm);
        FState := hm.HandshakeType;
        if (hm.HandshakeType <> htHelloRequest) and (hm.HandshakeType <> htKeyUpdate) then
          FService.StoreNegotiationData(hm); // input message

        ReplyToMessage(hm.HandshakeType);
        Offset := Offset + (hm.WriteOffset - hm.DataOffset) + 4;
      finally
        hm.Free;
      end;

      IsFirst := False;
      hm := GetHandshakeMessage(MessageBuf, Offset, Count);
    end;

    if Count > Offset then begin
      Buffer.BlockCopy(MessageBuf, Offset, FProcessMessageBuf, 0, Count - Offset);
      FProcessMessageCount := Count - Offset;
    end
    else
      FProcessMessageCount := 0;

  finally
    FRecordLayer.UnlockWriter;
  end;
end;

function THandshakeLayer.ProcessChangeCipherSpec(Message: TRecordMessage): TScSSLStatus;
begin
  if (Message.DataLength <> 1) or (Message.Uncompress[Message.ReadOffset] <> 1) then
    SendAlert(adDecodeError, seInvalidMessage);

  if GetProtocol = spTls13 then begin
    if (FState <> htClientHello) and (FState <> htServerHello) then
      SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
  end
  else begin
    if FOptions.Entity = ceClient then begin
      if FIsSessionResumption then begin
        if (FState <> htServerHello) and (FState <> htNewSessionTicket) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if (FState <> htServerHelloDone) and (FState <> htNewSessionTicket) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
    end
    else begin
      if FIsSessionResumption then begin
        if FState <> htClientHello then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if FRemoteCertificate = nil then begin
          if FState <> htClientKeyExchange then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htCertificateVerify then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
    end;

    if FCipherSuite = nil then
      FCipherSuite := FService.InitializeCipherSuite(SSLCipherDefinitions[FSessionInfo.CipherAlgorithm], cpBoth);

    FRecordLayer.ChangeRemoteState(FCipherSuite.Decryptor, FCipherSuite.RemoteIV, FCipherSuite.RemoteHasher);
    FCipherSuite.Decryptor := nil;
    FCipherSuite.RemoteHasher := nil;
    FState := htChangeCipherSpec;
  end;

  Result := ssMessageIncomplete; // needs a finished message
end;

procedure THandshakeLayer.MakeChangeCipherSpec;
var
  Buf: TBytes;
begin
  if FIsChangeCipherSpecSent then
    Exit;

  SetLength(Buf, 1);
  Buf[0] := 1;
  FRecordLayer.EncryptAndStore(TValueArr(Buf), Length(Buf), ctChangeCipherSpec);
  FIsChangeCipherSpecSent := True;

  if GetProtocol <> spTls13 then begin
    if FCipherSuite = nil then
      FCipherSuite := FService.InitializeCipherSuite(SSLCipherDefinitions[FSessionInfo.CipherAlgorithm], cpBoth);

    FRecordLayer.ChangeLocalState(FCipherSuite.Encryptor, FCipherSuite.LocalIV, FCipherSuite.LocalHasher);
    FCipherSuite.Encryptor := nil;
    FCipherSuite.LocalHasher := nil;
  end;
end;

function THandshakeLayer.IsRequiredPostHandshakeAuth: boolean;
begin
  Result := (GetProtocol = spTls13) and
    (FOptions.ClientHelloExtensions.Find(TTLSPostHandshakeAuthExtension) <> nil);
end;

function THandshakeLayer.GetOwnCertificate: TScCertificate;
begin
  if FOwnCertificateList.Count > 0 then
    Result := TScCertificate(FOwnCertificateList[0])
  else
    Result := nil;
end;

procedure THandshakeLayer.MakeCertificateMessage(CertList: TCRList);
begin
  FSendMessage.Init(htCertificate);
  FService.WriteCertificateMessage(FSendMessage, CertList);

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure THandshakeLayer.MakeCertificateVerifyMessage(Certificate: TScCertificate);
begin
  FSendMessage.Init(htCertificateVerify);
  FService.WriteCertificateVerifyMessage(FSendMessage, Certificate);

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure THandshakeLayer.ProcessCertificate(Message: THandshakeMessage);
const
  IgnoreRemoteCertificateValidityStatuses: TScCertificateStatusSet = [
    csOtherError, csExpired];

  IgnoreRemoteCertificateConstraintsStatuses: TScCertificateStatusSet = [
    csInvalidBasicConstraints, csInvalidKeyUsage];

  IgnoreRemoteCertificateInsecurityStatuses: TScCertificateStatusSet = [
    csInsecureSignature];

var
  StatusSet: TScCertificateStatusSet;
  St: TScCertificateStatus;
  NeedVerify: boolean;
  OrigCertList: TCRList;
//  SignatureAlgorithmsExtension: TTLSSignatureAlgorithmsExtension;
//  SignatureSchemesExtension: TTLSSignatureSchemesExtension;
//  Corresponds: boolean;
  i: integer;
begin
  FreeAndNil(FRemoteCertificate);

  OrigCertList := TCRList.Create;
  try
    FService.ReadCertificateMessage(Message, OrigCertList);

    if OrigCertList.Count = 0 then begin
      if FOptions.Entity = ceClient then
        // https://tools.ietf.org/html/rfc8446#section-4.4.2.4
        SendAlert(adDecodeError, sePeerCertificateNotReceived)
      else begin
        FRemoteCertificate := nil;
        if Assigned(FOptions.OnRemoteCertificateValidation) then begin
          StatusSet := [];
          FOptions.OnRemoteCertificateValidation(Self, nil, OrigCertList, StatusSet);
          if StatusSet <> [] then
            SendAlert(adBadCertificate, seCertificateNotValid);
        end;

        Exit;
      end;
    end;

    FRemoteCertificate := TScCertificate(OrigCertList[0]);
    FSessionInfo.RemoteCertificate := FRemoteCertificate;
    if FRemoteCertificate.Key.Algorithm = aaDSA then
      SendAlert(adHandshakeFailure, seInvalidPublicKeyAlgorithm);
    if (FRemoteCertificate.Key.Algorithm = aaRSA) and (FRemoteCertificate.Key.BitCount < 1024) then
      SendAlert(adInsufficientSecurity, seErrorPublicKeyLength);

    if Assigned(FOptions.OnNeedVerifyCertificate) then
      FOptions.OnNeedVerifyCertificate(NeedVerify)
    else
      NeedVerify := True;

    if not NeedVerify or FOptions.TrustRemoteCertificate then
      Exit;

  (*
    // https://tools.ietf.org/html/rfc8446#section-4.4.2.2
    // Client can allow a certificate that was not even provided by the SignatureAlgorithms extension

    if FOptions.Entity = ceClient then begin
      SignatureSchemesExtension := TTLSSignatureSchemesExtension(FOptions.ClientHelloExtensions.FindExtensionByClass(TTLSSignatureAlgorithmsCertExtension));
      if SignatureSchemesExtension = nil then
        SignatureSchemesExtension := TTLSSignatureSchemesExtension(FOptions.ClientHelloExtensions.FindExtensionByClass(TTLSSignatureSchemesExtension));

      if SignatureSchemesExtension <> nil then begin
        Corresponds := False;
        for i := 0 to SignatureSchemesExtension.Count - 1 do begin
          if (SCHEME_ALGORITHMS[SignatureSchemesExtension.SignatureSchemes[i]].Signature = FRemoteCertificate.Key.Algorithm) and
             (SCHEME_ALGORITHMS[SignatureSchemesExtension.SignatureSchemes[i]].Hash = FRemoteCertificate.SignatureAlgorithm.HashAlgorithm)
          then begin
            Corresponds := True;
            Break;
          end;
        end;

        if not Corresponds then
          SendAlert(adBadCertificate, seCertificateNotCorrespondToRequiredSignatureAlgorithms);
      end
      else begin
        SignatureAlgorithmsExtension := TTLSSignatureAlgorithmsExtension(FOptions.ClientHelloExtensions.FindExtensionByClass(TTLSSignatureAlgorithmsExtension));
        if SignatureAlgorithmsExtension <> nil then begin
          Corresponds := False;
          for i := 0 to SignatureAlgorithmsExtension.Count - 1 do begin
            if (SignatureAlgorithmsExtension.Signatures[i] = FRemoteCertificate.Key.Algorithm) and
               (SignatureAlgorithmsExtension.Hashes[i] = FRemoteCertificate.SignatureAlgorithm.HashAlgorithm)
            then begin
              Corresponds := True;
              Break;
            end;
          end;

          if not Corresponds then
            SendAlert(adBadCertificate, seCertificateNotCorrespondToRequiredSignatureAlgorithms);
        end;
      end;
    end;
  *)

    VerifyCertificate(OrigCertList, StatusSet);

    if StatusSet <> [] then begin
      for St := High(TScCertificateStatus) downto Low(TScCertificateStatus) do
        if (St <> csValid) and (St in StatusSet) then begin
          if FOptions.IgnoreRemoteCertificateValidity and (St in IgnoreRemoteCertificateValidityStatuses) then
          else
          if FOptions.IgnoreRemoteCertificateConstraints and (St in IgnoreRemoteCertificateConstraintsStatuses) then
          else
          if FOptions.IgnoreRemoteCertificateInsecurity and (St in IgnoreRemoteCertificateInsecurityStatuses) then
          else
            SendAlert(adBadCertificate, CERTIFICATE_STATUS_ERROR_CODE[St]);
        end;
    end;

  finally
    if FRemoteCertificate <> nil then
      OrigCertList.Remove(FRemoteCertificate);

    for i := 0 to OrigCertList.Count - 1 do
    {$IFNDEF AUTOREFCOUNT}
      TScCertificate(OrigCertList[i]).Free;
    {$ELSE}
      OrigCertList[i] := nil;
    {$ENDIF}

    OrigCertList.Free;
  end;
end;

procedure THandshakeLayer.VerifyCertificate(CertificateList: TCRList; out StatusSet: TScCertificateStatusSet);
begin
  if CertificateList.Count = 0 then
    SendAlert(adHandshakeFailure, seCertificateNotExists);

  FOptions.CertificateListIsUnsorted := True;
  FOptions.IdentityDNSName := GetIdentityDNSName;
  StatusSet := TScTLSCertificateChain.VerifyChain(CertificateList, FOptions);
end;

function THandshakeLayer.GetIdentityDNSName: string;
var
  ServerNameExtension: TTLSServerNameExtension;
begin
  ServerNameExtension := TTLSServerNameExtension(FOptions.ClientHelloExtensions.Find(TTLSServerNameExtension));
  if (ServerNameExtension <> nil) and (ServerNameExtension.ServerNames.Count > 0) then
    Result := ServerNameExtension.ServerNames[ServerNameExtension.ServerNames.Count - 1]
  else
    Result := '';
end;

procedure THandshakeLayer.ProcessCertificateVerify(Message: THandshakeMessage);
begin
  FService.VerifyCertificateMessage(Message, FRemoteCertificate);
end;

procedure THandshakeLayer.MakeFinishedMessage;
var
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
begin
  FSendMessage.Init(htFinished);
  FService.WriteFinishedMessage(FSendMessage);
  FSendMessage.Complete;

  if FOptions.Entity = ceClient then begin
    RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ClientHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
    if RenegIndicatExtension <> nil then
      RenegIndicatExtension.SetClientFinishedMessage(FSendMessage.Fragment,
        FSendMessage.DataOffset, FSendMessage.WriteOffset - FSendMessage.DataOffset);
  end
  else begin
    RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ServerHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
    if RenegIndicatExtension <> nil then
      RenegIndicatExtension.SetServerFinishedMessage(FSendMessage.Fragment,
        FSendMessage.DataOffset, FSendMessage.WriteOffset - FSendMessage.DataOffset);
  end;

  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
  FFinished := True;
  FIsChangeCipherSpecSent := False;
end;

class function THandshakeLayer.GetUnixTimeBuf: TBytes;
var
  TotalSeconds: cardinal;
begin
  TotalSeconds := GetUnixTime;
  Result := BitConverter.GetBytes(TotalSeconds);
  if BitConverter.IsLittleEndian then
    ArrayReverse(Result, 0, Length(Result));
end;

procedure THandshakeLayer.SetIsNegotiating(Value: boolean);
begin
  if FIsNegotiating <> Value then begin
    FIsNegotiating := Value;
    if FIsNegotiating then
      FRecordLayer.FWaitNegotiating.ResetEvent
    else
      FRecordLayer.FWaitNegotiating.SetEvent;
  end;
end;

procedure THandshakeLayer.SendAlert(AlertDescription: TScAlertDescription; ErrorCode: TScErrorCode);
var
  Buf: TBytes;
  IsFatal: boolean;
begin
  SetLength(Buf, 2);

  IsFatal := not (AlertDescription in WARNING_ALERT_DESCRIPTIONS);

  if IsFatal then
    Buf[0] := ALERT_LEVEL_CODES[alFatal]
  else
    Buf[0] := ALERT_LEVEL_CODES[alWarning];

  Buf[1] := ALERT_DESCRIPTION_CODES[AlertDescription];
  FRecordLayer.EncryptAndStore(TValueArr(Buf), Length(Buf), ctAlert);

  raise EScAlertError.Create(ErrorCode);
end;

function THandshakeLayer.ProcessAlert(Message: TRecordMessage): TScSSLStatus;
var
  Level, Description: byte;
begin
  if Message.DataLength <> 2 then
    SendAlert(adDecodeError, seInvalidMessage);

  Level := Message.Uncompress[Message.ReadOffset];
  Description := Message.Uncompress[Message.ReadOffset + 1];
  if Level = ALERT_LEVEL_CODES[alFatal] then
    raise EScError.CreateFmt(SFailureAlert, [Description], seFailureAlert);

  if Description = ALERT_DESCRIPTION_CODES[adCloseNotify] then begin
    FShutdownLock.Enter;
    try
      if FState in [htShuttdownSent, htShuttedDown] then // we have already sent a shutdown notification
        FState := htShuttedDown
      else begin
        // send a shutdown notification
        FState := htShuttdownRecv;
        MakeShutdownMessage;
      end;
    finally
      FShutdownLock.Leave;
    end;

    Result:= ssShuttedDown;
  end
  else
    Result := ssOK;
end;

procedure THandshakeLayer.MakeShutdownMessage;
var
  Buf: TBytes;
begin
  FShutdownLock.Enter;
  try
    if FState in [htShuttdownSent, htShuttedDown] then
      Exit
    else
    if FState = htShuttdownRecv then
      FState := htShuttedDown
    else
      FState := htShuttdownSent;
  finally
    FShutdownLock.Leave;
  end;

  SetLength(Buf, 2);
  Buf[0] := ALERT_LEVEL_CODES[alWarning];
  Buf[1] := ALERT_DESCRIPTION_CODES[adCloseNotify];
  FRecordLayer.EncryptAndStore(TValueArr(Buf), Length(Buf), ctAlert);
end;

procedure THandshakeLayer.MakeRenegotiateMessage;
begin
  if IsNegotiating then // ignore hello request when Negotiating
    Exit;

  if FIsRenegotiationStarted then
    Exit;

  FIsRenegotiationStarted := True;

  if GetProtocol = spTls13 then
    MakeKeyUpdateMessage(kurUpdateRequested)
  else
    MakeHelloRequestMessage;
end;

procedure THandshakeLayer.MakeKeyUpdateMessage(KeyUpdateRequest: TScKeyUpdateRequest);
begin
  FSendMessage.Init(htKeyUpdate);
  FSendMessage.WriteInt8(KEY_UPDATE_REQUEST_CODES[KeyUpdateRequest]);

  FSendMessage.Complete;
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);

  if FOptions.Entity = ceClient then
    FService.UpdateClientApplicationTrafficSecret
  else
    FService.UpdateServerApplicationTrafficSecret;

  InitializeCiphers(cpLocal);
end;

procedure THandshakeLayer.ProcessKeyUpdate(Message: THandshakeMessage);
var
  Val: byte;
begin
  if Message.RestCount <> 1 then
    SendAlert(adDecodeError, seInvalidMessage);

  FIsRenegotiationStarted := False;

  Val := Message.ReadInt8;
  if Val = KEY_UPDATE_REQUEST_CODES[kurUpdateNotRequested] then
    FLastKeyUpdateRequest := kurUpdateNotRequested
  else
  if Val = KEY_UPDATE_REQUEST_CODES[kurUpdateRequested] then
    FLastKeyUpdateRequest := kurUpdateRequested
  else
    SendAlert(adIllegalParameter, seInvalidMessage);

  if FOptions.Entity = ceClient then
    FService.UpdateServerApplicationTrafficSecret
  else
    FService.UpdateClientApplicationTrafficSecret;

  InitializeCiphers(cpRemote);
end;

function THandshakeLayer.GetProtocol: TScSSLProtocol;
begin
  Result := FService.GetProtocol;
end;

{ THandshakeProtocolService }

constructor THandshakeProtocolService.Create(Owner: THandshakeLayer; Entity: TScSSLConnectionEnd);
begin
  inherited Create;

  FOwner := Owner;
  FEntity := Entity;
end;

constructor THandshakeProtocolService.Create(Clone: THandshakeProtocolService; Entity: TScSSLConnectionEnd);
begin
  if Clone = nil then
    raise EScError.Create(seInvalidInputArgs);

  Create(Clone.FOwner, Entity);

  FNegotiationBuffer := Clone.FNegotiationBuffer;
  Clone.FNegotiationBuffer := nil;
  FNegotiationBufferPos := Clone.FNegotiationBufferPos;

  FMasterSecret := Clone.FMasterSecret;
  Clone.FMasterSecret := nil;
end;

destructor THandshakeProtocolService.Destroy;
begin
  Clear;

  FTranscriptHashAlgorithm.Free;
  inherited;
end;

function THandshakeProtocolService.GetCipherAlgorithm: TScSSLCipherAlgorithm;
begin
  Result := FOwner.FSessionInfo.CipherAlgorithm;
end;

procedure THandshakeProtocolService.Clear;
begin
  if Length(FNegotiationBuffer) > 0 then
    FillChar(FNegotiationBuffer[0], FNegotiationBufferPos, 0);
  FNegotiationBufferPos := 0;

  if Length(FMasterSecret) > 0 then begin
    FillChar(FMasterSecret[0], Length(FMasterSecret), 0);
    SetLength(FMasterSecret, 0);
  end;
end;

procedure THandshakeProtocolService.SetMasterSecret(const Value: TBytes);
begin
  SetLength(FMasterSecret, Length(Value));
  if Length(Value) > 0 then
    Move(Value[0], FMasterSecret[0], Length(Value));
end;

procedure THandshakeProtocolService.StoreNegotiationData(Message: THandshakeMessage);
begin
  StoreNegotiationData(Message.Fragment, Message.DataOffset - HANDSHAKE_HEADER_LENGTH,
    Message.WriteOffset - Message.DataOffset + HANDSHAKE_HEADER_LENGTH);
end;

procedure THandshakeProtocolService.StoreNegotiationData(const Buffer: TBytes; Offset, Count: integer);
var
  NegotiationLen: integer;
begin
  if Count = 0 then
    Exit;

  NegotiationLen := Length(FNegotiationBuffer);
  if (FNegotiationBufferPos + Count) > NegotiationLen then begin
    if NegotiationLen = 0 then
      NegotiationLen := MAX_RECORD_LENGTH
    else
      NegotiationLen := NegotiationLen + NegotiationLen div 2;

    if (FNegotiationBufferPos + Count) > NegotiationLen then
      SetLength(FNegotiationBuffer, FNegotiationBufferPos + Count)
    else
      SetLength(FNegotiationBuffer, NegotiationLen);
  end;

  Move(Buffer[Offset], FNegotiationBuffer[FNegotiationBufferPos], Count);
  Inc(FNegotiationBufferPos, Count);
end;

function THandshakeProtocolService.TranscriptHash: TBytes;
begin
  if FTranscriptHashAlgorithm = nil then
    FTranscriptHashAlgorithm := CreateHMACHash;

  if FNegotiationHashPos <> FNegotiationBufferPos then begin
    FNegotiationHash := FTranscriptHashAlgorithm.ComputeHash(TValueArr(FNegotiationBuffer), 0, FNegotiationBufferPos);
    FNegotiationHashPos := FNegotiationBufferPos;
  end;
  Result := FNegotiationHash;
end;

procedure THandshakeProtocolService.TranscriptClientHello1Hash;
begin
  // None
end;

procedure THandshakeProtocolService.CreateClientHello1Hash;
begin
  // None
end;

procedure THandshakeProtocolService.MakeEarlySecret(const PreSharedKey: TBytes);
begin
  // None
end;

procedure THandshakeProtocolService.MakeHandshakeTrafficSecret(const Premaster: TBytes);
begin
  // None
end;

procedure THandshakeProtocolService.MakeApplicationTrafficSecret;
begin
  // None
end;

procedure THandshakeProtocolService.MakeResumptionMasterSecret;
begin
  // None
end;

procedure THandshakeProtocolService.UpdateClientApplicationTrafficSecret;
begin
  // None
end;

procedure THandshakeProtocolService.UpdateServerApplicationTrafficSecret;
begin
  // None
end;

procedure THandshakeProtocolService.MakeMasterSecret(const Premaster: TBytes);
begin
  if FOwner.FUseExtendedMasterSecret then // rfc 7627
    MakeMasterSecret(Premaster, TranscriptHash(), S_extended_master_secret)
  else
    MakeMasterSecret(Premaster, FOwner.ClientServerRandom, S_master_secret);

  if (FOwner.Options.ServerHelloExtensions.Find(TTLSSessionTicketExtension) <> nil) or
     (Length(FOwner.SessionInfo.SessionID) > 0)
  then
    FOwner.SessionInfo.MasterSecret := FMasterSecret;
end;

procedure THandshakeProtocolService.WriteCertificateMessage(Message: THandshakeMessage;
  CertList: TCRList);
var
  Certificate: TScCertificate;
  RawData: TBytes;
  LenOffset: integer;
  i: integer;
begin
  if CertList = nil then begin
    Message.WriteInt24(0);
    Exit;
  end;

  LenOffset := Message.WriteOffset;
  Message.SkipWrite(3); // Len

  SetLength(RawData, 0);
  for i := 0 to CertList.Count - 1 do begin
    Certificate := TScCertificate(CertList[i]);
    if Certificate = nil then
      Continue;

    RawData := Certificate.GetRawData;
    Message.WriteBuf24(RawData);
  end;

  Message.WriteInt24ByOffset(Message.WriteOffset - LenOffset - 3, LenOffset);
end;

procedure THandshakeProtocolService.ReadCertificateMessage(Message: THandshakeMessage; CertList: TCRList);
var
  Stream: TMemoryStream;
  Cert: TScCertificate;
  Len: integer;
begin
  Len := Message.ReadInt24; // certificate_list
  if Len > Message.RestCount then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

  Stream := TMemoryStream.Create;
  try
    while Message.RestCount > 0 do begin
      Len := Message.ReadInt24;
      if Len > Message.RestCount then
        FOwner.SendAlert(adDecodeError, seInvalidMessage);

      Stream.Position := 0;
      Stream.WriteBuffer(Message.Fragment[Message.ReadOffset], Len);
      Message.SkipRead(Len);
      Stream.Size := Len;
      Stream.Position := 0;

      Cert := TScCertificate.Create(nil);
      try
        Cert.ImportFrom(Stream);
        Cert.CertName := IntToStr(CertList.Count);
        CertList.Add(Cert);
      except
        Cert.Free;
        FOwner.SendAlert(adDecodeError, seInvalidMessage);
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure THandshakeProtocolService.WriteCertificateVerifyMessage(Message: THandshakeMessage;
  Certificate: TScCertificate);
var
  csp: TMD5SHA1CryptoServiceProvider;
  SignBuf: TBytes;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  csp := TMD5SHA1CryptoServiceProvider.Create;
  try
    csp.Protocol := GetProtocol;
    csp.MasterKey := FMasterSecret;
    csp.TransformFinalBlock(FNegotiationBuffer, 0, FNegotiationBufferPos);
    SignBuf := Certificate.Sign(csp.Hash, haNone);

    Message.WriteBuf16(SignBuf);
  finally
    csp.Free;
  end;
end;

procedure THandshakeProtocolService.VerifyCertificateMessage(Message: THandshakeMessage;
  RemoteCertificate: TScCertificate);
var
  csp: TMD5SHA1CryptoServiceProvider;
  Signature: TBytes;
  Size: integer;
begin
  if RemoteCertificate = nil then
    FOwner.SendAlert(adHandshakeFailure, seServerCertificateNotReceived);

  Size := Message.ReadInt16;
  Signature := Message.Read(Size); // holds the signature returned by the server

  csp := TMD5SHA1CryptoServiceProvider.Create;
  try
    csp.Protocol := GetProtocol;
    csp.MasterKey := FMasterSecret;
    csp.TransformFinalBlock(FNegotiationBuffer, 0, FNegotiationBufferPos);

    if not RemoteCertificate.VerifyHashSign(csp.Hash, Signature, haNone) then
      FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
  finally
    csp.Free;
  end;
end;

procedure THandshakeProtocolService.WriteServerKeyExchangeSign(Message: THandshakeMessage;
  Certificate: TScCertificate);
var
  csp: THashAlgorithm;
  SignBuf: TBytes;
begin
  if Certificate = nil then
    FOwner.SendAlert(adHandshakeFailure, seServerCertificateNotSpecified);

  // compute verification hashes
  csp := TMD5SHA1CryptoServiceProvider.Create;
  try
    csp.TransformBlock(FOwner.ClientServerRandom, 0, 64);
    csp.TransformFinalBlock(Message.Fragment, Message.DataOffset, Message.WriteOffset - Message.DataOffset);

    SignBuf := Certificate.Sign(csp.Hash, haNone);
    Message.WriteBuf16(SignBuf);
  finally
    csp.Free;
  end;
end;

procedure THandshakeProtocolService.VerifyServerKeyExchange(Message: THandshakeMessage;
  RemoteCertificate: TScCertificate);
var
  csp: THashAlgorithm;
  Signature: TBytes;
  Size: integer;
begin
  if RemoteCertificate = nil then
    FOwner.SendAlert(adHandshakeFailure, seServerCertificateNotReceived);

  // compute verification hashes
  csp := TMD5SHA1CryptoServiceProvider.Create;
  try
    csp.TransformBlock(FOwner.ClientServerRandom, 0, 64);
    csp.TransformFinalBlock(Message.Fragment, Message.DataOffset, Message.ReadOffset - Message.DataOffset);

    // verify the signature
    Size := Message.ReadInt16;
    Signature := Message.Read(Size); // holds the signature returned by the server

    if not RemoteCertificate.VerifySign(csp.Hash, Signature, haNone) then
      FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
  finally
    csp.Free;
  end;
end;

procedure THandshakeProtocolService.MakeCertificateRequestMessage(Message: THandshakeMessage;
  DNList: TScDistinguishedNameList);
var
  DN: TScDistinguishedName;
  DNBuf: TBytes;
  LenOffset: integer;
  i: integer;
begin
  Message.WriteInt8(2); // Length
  Message.WriteInt8(ctRsaSign);
  Message.WriteInt8(ctECdsaSign);

  WriteSignatureAndHash(Message);

  if DNList = nil then begin
    Message.WriteInt16(0); // Len
    Exit;
  end;

  LenOffset := Message.WriteOffset;
  Message.SkipWrite(2); // Len
  SetLength(DNBuf, 0);

  for i := 0 to DNList.Count - 1 do begin
    DN := TScDistinguishedName(DNList[i]);
    if DN = nil then
      continue;

    DNBuf := DN.Encode;
    Message.WriteBuf16(DNBuf);
  end;

  Message.WriteInt16ByOffset(Message.WriteOffset - LenOffset - 2, LenOffset);
end;

function THandshakeProtocolService.ParseCertificateRequestMessage(Message: THandshakeMessage): TScDistinguishedNameList;
var
  DN: TScDistinguishedName;
  SupportsCerts: boolean;
  Size, DNLen: integer;
  i: integer;
begin
  // get supported certificate types
  SupportsCerts := False;
  Size := Message.ReadInt8;
  for i := 1 to Size do begin
    if Message.ReadInt8 in [ctRsaSign, ctECdsaSign] then // need to read all
      SupportsCerts := True;
  end;

  ReadSignatureAndHash(Message);

  // get list of distinguished names
  Result := TScDistinguishedNameList.Create;
  try
    if Message.RestCount = 0 then
      Exit;

    DNLen := Message.ReadInt16; // common length of DN
    while DNLen > 0 do begin
      Size := Message.ReadInt16;
      if Size = 0 then
        Break;
      if Size > Message.RestCount then
        FOwner.SendAlert(adDecodeError, seInvalidMessage);

      DN := TScDistinguishedName.Create;
      Result.Add(DN);
      DN.Decode(Message.Fragment, Message.ReadOffset, Size);
      Message.SkipRead(Size);
      Dec(DNLen, Size + 2);
    end;

    if not SupportsCerts then
      FreeAndNil(Result);
  except
    Result.Free;
    FOwner.SendAlert(adDecodeError, seInvalidMessage);
  end;
end;

procedure THandshakeProtocolService.WriteSignatureAndHash(Message: THandshakeMessage);
begin
  // None
end;

procedure THandshakeProtocolService.ReadSignatureAndHash(Message: THandshakeMessage);
begin
  // None
end;

procedure THandshakeProtocolService.GetTicketKeyNameAndPassword(out TicketName, Password: TBytes);
var
  Lifetime: cardinal;
begin
  if Assigned(FOwner.Options.OnGetTicketNameAndPassword) then
    FOwner.Options.OnGetTicketNameAndPassword(Self, TicketName, Password)
  else begin
    TicketPasswordLock.Enter;
    try
      Lifetime := GetUnixTime - TLSTicketPasswordCreateTime;
      // Recreate every 24 hours
      if (Lifetime > SecsPerDay) or (Length(TLSTicketPassword) = 0) then begin
        PrevTLSTicketName := TLSTicketName;
        PrevTLSTicketPassword := TLSTicketPassword;
        TLSTicketName := nil;
        TLSTicketPassword := nil;

        SetLength(TLSTicketName, 16);
        Random.Random(TLSTicketName, 0, Length(TLSTicketName));

        SetLength(TLSTicketPassword, 16);
        Random.Random(TLSTicketPassword, 0, Length(TLSTicketPassword));
      end;
    finally
      TicketPasswordLock.Leave;
    end;

    TicketName := TLSTicketName;
    Password := TLSTicketPassword;
  end;
end;

procedure THandshakeProtocolService.GetPasswordByTicketKeyName(const Ticket: TBytes; out Password: TBytes);
begin
  if Assigned(FOwner.Options.OnGetPasswordByTicketName) then
    FOwner.Options.OnGetPasswordByTicketName(Self, Ticket, Password)
  else begin
    if (Length(TLSTicketName) > 0) and (Length(Ticket) >= Length(TLSTicketName)) and
       (MemCompare(@Ticket[0], @TLSTicketName[0], Length(TLSTicketName)) = 0) then
      Password := TLSTicketPassword
    else
    if (Length(PrevTLSTicketName) > 0) and (Length(Ticket) >= Length(PrevTLSTicketName)) and
       (MemCompare(@Ticket[0], @PrevTLSTicketName[0], Length(PrevTLSTicketName)) = 0) then
      Password := PrevTLSTicketPassword
    else
      SetLength(Password, 0);
  end;
end;

function THandshakeProtocolService.CreateNewSessionTicket(
  SessionInfo: TScSSLSessionInfo): TScNewSessionTicketExt;
var
  NewSessionTicket: TScNewSessionTicketExt;
  Nonce, TicketKeyName, Password: TBytes;
  Ticket: TBytes;
  Len: integer;
  Cipher: TSymmetricAlgorithm;
  HMAC: THMAC;
begin
{$IFNDEF VER25P}
  Result := nil;
{$ENDIF}

  NewSessionTicket := TScNewSessionTicketExt.Create;
  try
    NewSessionTicket.CreateTime := GetUnixTime;

    NewSessionTicket.Lifetime := FOwner.Options.NewSessionTicketLifetime;
    Random.Random(@NewSessionTicket.AgeAdd, 4);

    SetLength(Nonce, 8);
    Nonce[7] := byte(SessionInfo.NewSessionTicketCount);
    SessionInfo.NewSessionTicketCount := SessionInfo.NewSessionTicketCount + 1;
    NewSessionTicket.Nonce := Nonce;

    if Assigned(FOwner.Options.OnCreateNewSessionTicket) then begin
      FOwner.Options.OnCreateNewSessionTicket(Self, SessionInfo, NewSessionTicket);
    end
    else begin
      // create ticket info
      SetLength(Ticket, 192);

      GetTicketKeyNameAndPassword(TicketKeyName, Password);
      if Length(TicketKeyName) <> 16 then
        raise EScError.Create(seInternalError);
      // Key name [16]
      Move(TicketKeyName[0], Ticket[0], 16);

      // IV [16]
      Random.Random(Ticket, 16, 16);

      // Create time [4]
      Move(NewSessionTicket.CreateTime, Ticket[32], 4);
      // Age add [4]
      Move(NewSessionTicket.AgeAdd, Ticket[36], 4);

      // Nonce [8]
      Move(NewSessionTicket.Nonce[0], Ticket[40], 8);

      // Protocol version [1]
      Ticket[48] := byte(GetProtocol);

      // Cipher suite [2]
      TCipherSuites.CipherAlgorithmToBuf(SessionInfo.CipherAlgorithm, Ticket, 49);

      // Compression [1]
      if SessionInfo.Compression = csRequired then
        Ticket[51] := 1
      else
        Ticket[51] := 0;

      // SessionID len [1]
      Len := Length(SessionInfo.SessionID);
      Ticket[52] := Len;
      // SessionID [32]
      if (Len > 0) and (Len <= 32) then
        Move(SessionInfo.SessionID[0], Ticket[53], Len);

      // MasterSecret len [1]
      Len := Length(SessionInfo.MasterSecret);
      Ticket[85] := Len;
      // MasterSecret [48]
      if (Len > 0) and (Len <= 48) then
        Move(SessionInfo.MasterSecret[0], Ticket[86], Len);

      // padding [mod 32]
      Random.Random(Ticket, 134, 26);

      // encryption
      Cipher := TCipher_Rijndael.Create;
      try
        Cipher.Mode := cmCBC;
        Cipher.Key := Password;
        Cipher.SetIV(TValueArr(Ticket), 16, 16);
        Cipher.EncodeBuffer(TValueArr(Ticket), 32, 160-32, TValueArr(Ticket), 32);
      finally
        Cipher.Free;
      end;

      // mac [32]
      HMAC := THMAC.Create(THash_SHA2_256, Password);
      try
        HMAC.ComputeHash(TValueArr(Ticket), 0, 160);
        Move(HMAC.Hash[0], Ticket[160], 32);
      finally
        HMAC.Free;
      end;

      NewSessionTicket.Ticket := Ticket;
    end;

    Result := NewSessionTicket;
    NewSessionTicket := nil;
  finally
    NewSessionTicket.Free;
  end;
end;

function THandshakeProtocolService.DecodeAndCheckTicket(
  NewSessionTicket: TScNewSessionTicket; NewSessionInfo: TScSSLSessionInfo): boolean;
var
  Password, Ticket: TBytes;
  Nonce, SessionID, MasterSecret: TBytes;
  Len: integer;
  Cipher: TSymmetricAlgorithm;
  HMAC: THMAC;
  Lifetime: integer;
  d: cardinal;
  w: word;
begin
  Result := False;
  SetLength(Ticket, 0);

  if Assigned(FOwner.Options.OnDecodeTicket) then begin
    FOwner.Options.OnDecodeTicket(Self, NewSessionTicket, NewSessionInfo, Result);
    if not Result then
      Exit;
  end
  else begin
    Ticket := NewSessionTicket.Ticket;
    if Length(Ticket) < 192 then
      Exit;

    GetPasswordByTicketKeyName(Ticket, Password);
    if Length(Password) = 0 then
      Exit;

    HMAC := THMAC.Create(THash_SHA2_256, Password);
    try
      HMAC.ComputeHash(TValueArr(Ticket), 0, 160);

      if (Length(HMAC.Hash) <> 32) or
         (MemCompare(@Ticket[160], @HMAC.Hash[0], 32) <> 0) then
        Exit;
    finally
      HMAC.Free;
    end;

    Cipher := TCipher_Rijndael.Create;
    try
      Cipher.Mode := cmCBC;
      Cipher.Key := Password;
      Cipher.SetIV(TValueArr(Ticket), 16, 16);
      Cipher.DecodeBuffer(TValueArr(Ticket), 32, 160-32, TValueArr(Ticket), 32);
    finally
      Cipher.Free;
    end;

    // Create time [4]
    Move(Ticket[32], d, 4);
    NewSessionTicket.CreateTime := d;
    // Age add [4]
    Move(Ticket[36], d, 4);
    NewSessionTicket.AgeAdd := d;

    // Nonce [8]
    SetLength(Nonce, 8);
    Move(Ticket[40], Nonce[0], 8);
    NewSessionInfo.TicketNonce := Nonce;

    // Protocol version [1]
    NewSessionInfo.Protocol := TScSSLProtocol(Ticket[48]);
    if Ticket[48] > byte(High(TScSSLProtocol)) then
      Exit;

    // Cipher suite [2]
    w := (Ticket[49] shl 8) + Ticket[50];
    NewSessionInfo.CipherAlgorithm := TCipherSuites.ToCipherAlgorithm(w);
    if NewSessionInfo.CipherAlgorithm = caNone then
      Exit;

    // Compression [1]
    if Ticket[51] = 1 then
      NewSessionInfo.Compression := csRequired
    else
      NewSessionInfo.Compression := csNone;

    // SessionID len [1]
    Len := Ticket[52];
    // SessionID [32]
    if (Len > 0) and (Len <= 32) then begin
      SetLength(SessionID, Len);
      Move(Ticket[53], SessionID[0], Len);
      NewSessionInfo.SessionID := SessionID;
    end
    else
      NewSessionInfo.SessionID := nil;

    // MasterSecret len [1]
    Len := Ticket[85];
    // MasterSecret [48]
    if (Len > 0) and (Len <= 48) then begin
      SetLength(MasterSecret, Len);
      Move(Ticket[86], MasterSecret[0], Len);
      NewSessionInfo.MasterSecret := MasterSecret;
    end
    else begin
      NewSessionInfo.MasterSecret := nil;
      Exit;
    end;
  end;

  if NewSessionTicket.Lifetime > 0 then begin
    // calc the lifetime specified by the client
    if NewSessionTicket.Lifetime >= NewSessionTicket.AgeAdd then
      Lifetime := NewSessionTicket.Lifetime - NewSessionTicket.AgeAdd
    else
      Lifetime := Cardinal($FFFFFFFF) - NewSessionTicket.AgeAdd + NewSessionTicket.Lifetime + 1;

    if Lifetime > FOwner.Options.NewSessionTicketLifetime * 1000 then
      Exit; // client time expired
  end;

  Lifetime := GetUnixTime - NewSessionTicket.CreateTime;
  if (Lifetime < 0) or (Lifetime > FOwner.Options.NewSessionTicketLifetime) then
    Exit; // server time expired

  Result := True;
end;

procedure THandshakeProtocolService.MakeNewSessionTicketMessage(
  Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo);
var
  NewSessionTicket: TScNewSessionTicketExt;
begin
  SessionInfo.MasterSecret := FMasterSecret;

  NewSessionTicket := CreateNewSessionTicket(SessionInfo);
  try
    Message.WriteInt32(NewSessionTicket.Lifetime);
    Message.WriteBuf16(NewSessionTicket.Ticket);
  finally
    NewSessionTicket.Free;
  end;
end;

procedure THandshakeProtocolService.ParseNewSessionTicketMessage(
  Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo);
var
  NewSessionTicket: TScNewSessionTicketExt;
  Size: integer;
begin
  NewSessionTicket := TScNewSessionTicketExt.Create;
  SessionInfo.NewSessionTickets.Add(NewSessionTicket);

  NewSessionTicket.Lifetime := Message.ReadInt32;
  Size := Message.ReadInt16;
  NewSessionTicket.Ticket := Message.Read(Size);

  NewSessionTicket.CreateTime := GetUnixTime;
  SessionInfo.MasterSecret := FMasterSecret;
end;

procedure THandshakeProtocolService.DecodeAndVerifyTicket(ClientHelloMessage: THandshakeMessage;
  ClientHelloExtensions: TTLSHelloExtensions; NewSessionInfo: TScSSLSessionInfo;
  out SelectedIdentity: integer);
var
  SessionTicketExtension: TTLSSessionTicketExtension;
  NewSessionTicket: TScNewSessionTicket;
begin
  SelectedIdentity := -1;

  SessionTicketExtension := TTLSSessionTicketExtension(ClientHelloExtensions.Find(TTLSSessionTicketExtension));
  if (SessionTicketExtension = nil) or (Length(SessionTicketExtension.Ticket) = 0) then
    Exit;

  NewSessionTicket := TScNewSessionTicket.Create;
  try
    NewSessionTicket.Ticket := SessionTicketExtension.Ticket;
    if DecodeAndCheckTicket(NewSessionTicket, NewSessionInfo) then begin
      SetMasterSecret(NewSessionInfo.MasterSecret);
      SelectedIdentity := 0;
    end;
  finally
    NewSessionTicket.Free;
  end;
end;

procedure THandshakeProtocolService.WriteBinderKey(ClientHelloMessage: THandshakeMessage;
  const MasterSecret, TicketNonce: TBytes);
begin
  // None
end;

{ TScTLSCertificateChain }

class function TScTLSCertificateChain.VerifyChain(CertificateList: TCRList;
  Options: TScSecurityOptions): TScCertificateStatusSet;
var
  Extension: TScCertificateExtension;
  GeneralName: TScGeneralName;
  HasDnsName, DnsNameIsEqual: boolean;
  ExtendedKeyUsages: TScOIds;
  ExtendedKeyUsageIsValid: boolean;
  i, n: integer;

  SortCertificateList: TCRList;
  CurCertificate, TmpCertificate: TScCertificate;
  HostCertificate: TScCertificate;
  RootCertificate: TScCertificate;
{$IFNDEF IOS}
  CACertificate: TScCertificate;
{$ELSE}
  TrustStatus: TScCertificateStatus;
{$ENDIF}
begin
  if CertificateList.Count = 0 then
    raise EScError.Create(seCertificateNotExists);

  SortCertificateList := TCRList.Create;
  try
    CurCertificate := TScCertificate(CertificateList[0]);
    SortCertificateList.Add(CurCertificate);

    if Options.CertificateListIsUnsorted then begin
      if not CurCertificate.IssuerName.Equals(CurCertificate.SubjectName) then begin
        for n := 1 to CertificateList.Count - 1 do begin
          i := n;
          while i < CertificateList.Count do begin
            if CurCertificate.IssuerName.Equals(TScCertificate(CertificateList[i]).SubjectName) then begin
              CertificateList.Move(i, n);
              break;
            end;
            Inc(i);
          end;

          if i = CertificateList.Count then
            break;

          TmpCertificate := TScCertificate(CertificateList[n]);
          if TmpCertificate.IssuerName.Equals(TmpCertificate.SubjectName) then
            break;

          CurCertificate := TmpCertificate;
          SortCertificateList.Add(CurCertificate);
        end;
      end;
    end
    else begin
      for i := 1 to CertificateList.Count - 1 do begin
        CurCertificate := TScCertificate(CertificateList[i]);
        SortCertificateList.Add(CurCertificate);
      end;
    end;

    if (SortCertificateList.Count = 1) and CurCertificate.IsSelfSigned then begin
      CurCertificate.VerifyCertificateChain(nil, nil, Result);
      if Options.TrustSelfSignedCertificate then
        Result := []
      else
        Result := [csUntrustedRoot, csOtherError];
    end
    else begin
      if Options.CACertificate <> nil then begin
        SortCertificateList.Add(Options.CACertificate);
        TScCertificateChain.VerifyChain(Options.OnObtainCRL, SortCertificateList, Result);
        SortCertificateList.Remove(Options.CACertificate);
      end
      else begin
        RootCertificate := nil;
        CurCertificate := TScCertificate(SortCertificateList[SortCertificateList.Count - 1]);

      {$IFNDEF IOS}
        if CryptoAPISystemRootStorage <> nil then
          RootCertificate := CryptoAPISystemRootStorage.Certificates.FindCertificateBySubject(CurCertificate.IssuerName);

        if (RootCertificate = nil) and (CryptoAPISystemCAStorage <> nil) then begin
          CACertificate := CryptoAPISystemCAStorage.Certificates.FindCertificateBySubject(CurCertificate.IssuerName);

          if CACertificate <> nil then begin
            SortCertificateList.Add(CACertificate);
            if CryptoAPISystemRootStorage <> nil then
              RootCertificate := CryptoAPISystemRootStorage.Certificates.FindCertificateBySubject(CACertificate.IssuerName);
          end;
        end;

        if RootCertificate <> nil then
          SortCertificateList.Add(RootCertificate);
      {$ENDIF IOS}

        if (RootCertificate = nil) and (Options.CAStorage <> nil) then begin
          RootCertificate := Options.CAStorage.Certificates.FindCertificateBySubject(CurCertificate.IssuerName);
          if RootCertificate <> nil then begin
            SortCertificateList.Add(RootCertificate);

          {$IFNDEF IOS}
            if not RootCertificate.IsSelfSigned and (CryptoAPISystemRootStorage <> nil) then begin
              RootCertificate := CryptoAPISystemRootStorage.Certificates.FindCertificateBySubject(RootCertificate.IssuerName);
              if RootCertificate <> nil then
                SortCertificateList.Add(RootCertificate);
            end;
          {$ENDIF IOS}
          end;
        end;

        TScCertificateChain.VerifyChain(Options.OnObtainCRL, SortCertificateList, Result);

      {$IFDEF IOS}
        TrustStatus := TScCryptoAPIStorage.CheckTrustCertificateByIssuer(CurCertificate);
        if TrustStatus = csValid then
          Exclude(Result, csUntrustedRoot)
        else
          Include(Result, TrustStatus);
      {$ENDIF}
      end;
    end;

    HostCertificate := TScCertificate(SortCertificateList[0]);

    if Options.IdentityDNSName <> '' then begin
      HasDnsName := False;
      DnsNameIsEqual := False;

      Extension := HostCertificate.Extensions.FindExtensionByClass(TScCertSubjectAlternativeNameExtension);
      if Extension <> nil then begin
        for n := 0 to TScCertSubjectAlternativeNameExtension(Extension).GeneralNames.Count - 1 do begin
          GeneralName := TScCertSubjectAlternativeNameExtension(Extension).GeneralNames[n];
          if CompareText(GeneralName.Name, 'DNSName') = 0 then begin
            HasDnsName := True;
            if MatchesDomainName(Options.IdentityDNSName, GeneralName.Value) then begin
              DnsNameIsEqual := True;
              break;
            end;
          end;
        end;
      end;

      if not HasDnsName then
        if MatchesDomainName(Options.IdentityDNSName, HostCertificate.SubjectName.Values[OID_COMMON_NAME]) then
          DnsNameIsEqual := True;

      if not DnsNameIsEqual then
        Include(Result, csInvalidSubjectName);
    end;

    Extension := HostCertificate.Extensions.FindExtensionByClass(TScCertKeyUsageExtension);
    if Extension <> nil then begin
      // https://tools.ietf.org/html/rfc8446#section-4.4.2.2
      if not (kfDigitalSignature in TScCertKeyUsageExtension(Extension).KeyUsages) then
        Include(Result, csInvalidKeyUsage);
    end;

    Extension := HostCertificate.Extensions.FindExtensionByClass(TScCertExtendedKeyUsageExtension);
    if (Extension <> nil) and Extension.Critical then begin
      ExtendedKeyUsageIsValid := False;
      ExtendedKeyUsages := TScCertExtendedKeyUsageExtension(Extension).ExtendedKeyUsages;
      for n := 0 to ExtendedKeyUsages.Count - 1 do
        if ExtendedKeyUsages[n].Value = OID_KP_SERVER_AUTH then begin
          ExtendedKeyUsageIsValid := True;
          break;
        end;

      if not ExtendedKeyUsageIsValid then
        Include(Result, csInvalidKeyUsage);
    end;

    if Assigned(Options.OnRemoteCertificateValidation) then
      Options.OnRemoteCertificateValidation(nil, HostCertificate, SortCertificateList, Result);
  finally
    SortCertificateList.Free;
  end;
end;

class function TScTLSCertificateChain.MatchesDomainName(const DNSName, DNSMask: string): boolean;
var
  DNSParts, DNSMaskes: TStringList;
  i: integer;
begin
  DNSParts := Split(DNSName, '.');
  DNSMaskes := Split(DNSMask, '.');

  try
    Result := DNSParts.Count = DNSMaskes.Count;
    if not Result then
      Exit;

    for i := 0 to DNSParts.Count - 1 do begin
      Result := ScMatchesMask(DNSParts[i], DNSMaskes[i]);
      if not Result then
        Exit;
    end;
  finally
    DNSParts.Free;
    DNSMaskes.Free;
  end;
end;

initialization

{$IFDEF USE_CRYPTOAPI_STORAGE}
  CryptoAPISystemRootStorage := TScCryptoAPIStorage.Create(nil);
{$IFDEF MSWINDOWS}
  CryptoAPISystemCAStorage := TScCryptoAPIStorage.Create(nil);
{$ENDIF}
{$ELSE}
  CryptoAPISystemRootStorage := TScFileStorage.Create(nil);
  CryptoAPISystemRootStorage.CertificateExt := '0';
{$ENDIF}

{$IFDEF ANDROID}
  CryptoAPISystemRootStorage.Path := '/system/etc/security/cacerts';
{$ELSE}
{$IFDEF LINUX}
  CryptoAPISystemRootStorage.Path := '/etc/ssl/certs';
{$ELSE}
{$IFDEF LINUX_BSD}
  CryptoAPISystemRootStorage.Path := '/etc/ssl/certs';
{$ELSE}
{$IFDEF MSWINDOWS}
  CryptoAPISystemRootStorage.CertStoreName := 'Root';
  CryptoAPISystemCAStorage.CertStoreName := 'CA';
{$ELSE}
  CryptoAPISystemRootStorage.KeychainPath := '/System/Library/Keychains/SystemRootCertificates.keychain';
{$ENDIF MSWINDOWS}
{$ENDIF LINUX_BSD}
{$ENDIF LINUX}
{$ENDIF ANDROID}
  TicketPasswordLock := TCriticalSection.Create;

finalization
  FreeAndNil(CryptoAPISystemRootStorage);
  FreeAndNil(CryptoAPISystemCAStorage);
  FreeAndNil(TicketPasswordLock);

end.
