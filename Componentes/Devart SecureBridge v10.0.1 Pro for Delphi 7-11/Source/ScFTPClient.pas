
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScFTPClient;

interface

uses
{$IFDEF POSIX}
  Posix.Unistd,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
{$IFNDEF NEXTGEN}
  Contnrs,
{$ENDIF}
  ScTypes, ScConsts, ScCLRClasses, ScUtils, ScBridge,
  ScVio, ScVioTcp, ScSecureConnection, ScSSLClient,
  ScFTPListParser;

type
  TScFTPTransferType = (ttASCII, ttBinary);
  TScFTPFileStructure = (fsFile, fsRecord, fsPage);
//  TScFTPTransferMode = (tmStream, tmDeflate); // Stream, Block, Compressed, Deflate
  TScFTPAuthCommand = (acAuto, acAuthTLS, acAuthTLSC, acAuthSSL, acAuthTLSP);

const
  DEF_ListenTimeout = 15;
  DEF_UsePassive = False;
  DEF_IgnoreServerPassiveHost = False;
  DEF_TransferType = ttASCII;
  DEF_UseCompression = False;
  DEF_UseNATFastConnection = False;
  DEF_UseExtendedDataAddress = False;
  DEF_UseClearingControlChannel = False;
  DEF_UseExtList = True;
  DEF_AuthCommand = acAuto;
  DEF_TLSMode = tmDisableTLS;
  DEF_EncryptDataChannel = False;
  DEFAULT_BLOCK_SIZE = 64 * 1024; // 64Kb

type
  TScFTPErrorEvent = procedure(Sender: TObject;
    ErrorCode: integer; const ErrorMessage: string; var Fail: boolean) of object;
  TScFTPBeforeUploadFileEvent = procedure(Sender: TObject; Source: TStream;
    const DestFile: string; var StartPos: Int64) of object;
  TScFTPAfterUploadFileEvent = procedure(Sender: TObject; Source: TStream;
    const DestFile: string; StartPos: Int64) of object;
  TScFTPBeforeDownloadFileEvent = procedure(Sender: TObject; const SourceFile: string;
    Dest: TStream; var StartPos: Int64) of object;
  TScFTPAfterDownloadFileEvent = procedure(Sender: TObject; const SourceFile: string;
    Dest: TStream; StartPos: Int64) of object;
  TScFTPOnRetrieveListEvent = procedure(Sender: TObject; const Path: string) of object;

  TScFTPClient = class;

  TScFTPClientOptions = class(TPersistent)
  private
    FClient: TScFTPClient;
    FBindAddress: string;
    FTCPKeepAlive: boolean;
    FIPVersion: TIPVersion;
    FSocketReceiveBufferSize: integer;
    FSocketSendBufferSize: integer;
    FBlockSize: integer;
    FIgnoreServerPassiveHost: boolean;
    FUseNATFastConnection: boolean;
    FUseExtendedDataAddress: boolean;
    FUseClearingControlChannel: boolean;
    FUseExtList: boolean;

    procedure SetBindAddress(const Value: string);
    procedure SetTCPKeepAlive(Value: boolean);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetSocketReceiveBufferSize(Value: integer);
    procedure SetSocketSendBufferSize(Value: integer);
    procedure SetUseNATFastConnection(Value: boolean);
    procedure SetUseExtendedDataAddress(Value: boolean);
    procedure SetUseClearingControlChannel(Value: boolean);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Client: TScFTPClient);
  published
    property BindAddress: string read FBindAddress write SetBindAddress; // useful on systems with more than one IP.
    property TCPKeepAlive: boolean read FTCPKeepAlive write SetTCPKeepAlive default True;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property SocketReceiveBufferSize: integer read FSocketReceiveBufferSize write SetSocketReceiveBufferSize default DEFAULT_SOCKET_BUFFER_SIZE;
    property SocketSendBufferSize: integer read FSocketSendBufferSize write SetSocketSendBufferSize default DEFAULT_SOCKET_BUFFER_SIZE;

    property BlockSize: integer read FBlockSize write FBlockSize default DEFAULT_BLOCK_SIZE;
    property IgnoreServerPassiveHost: boolean read FIgnoreServerPassiveHost write FIgnoreServerPassiveHost default DEF_IgnoreServerPassiveHost;
    property UseNATFastConnection: boolean read FUseNATFastConnection write SetUseNATFastConnection default DEF_UseNATFastConnection;
    property UseExtendedDataAddress: boolean read FUseExtendedDataAddress write SetUseExtendedDataAddress default DEF_UseExtendedDataAddress;
    property UseClearingControlChannel: boolean read FUseClearingControlChannel write SetUseClearingControlChannel default DEF_UseClearingControlChannel;
    property UseExtList: boolean read FUseExtList write FUseExtList default DEF_UseExtList;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScFTPClient = class(TComponent)
  private
    FProxyOptions: TProxyOptions;
    FHostName: string;
    FPort: integer;
    FTimeout: integer;
    FUsername: string;
    FPassword: string;
    FAccountInfo: string;
    FDataIP: string;
    FDataPort: integer;
    FListenTimeout: integer;
    FUsePassive: boolean;
    FPassiveIP: string;
    FPassivePort: integer;
    FTransferType: TScFTPTransferType;
    FUsedTransferType: TScFTPTransferType;

    // URI specifics
    FUri: string;
    FScheme: string;
    FQuery: string;
    FPortNo: string;
    FResource: string;
    FParameters: string;
    FPath: string;
    FFragment: string;

    FUseCompression: boolean;
    FIsUsedCompression: boolean;
    FCompressor: TScZCompression;
    FIsCompressionSupported: boolean;

    FReadBuffer: TBytes;
    FWriteBuffer: TBytes;
    FTotalUploadSize: Int64;
    FTotalDownloadSize: Int64;

    FSystemDescription: string;
    FServerDescription: string;
    FFormattedReply: TStringList;
    FSPMidLines: boolean;
    FLastReply: TStringList;
    FLastReplyCode: integer;
    FOptions: TScFTPClientOptions;
    FControlConnection: TScSecureConnection;
    FDataConnection: TScSecureConnection;
    FListenSocket: TCRVioTcp;
    FIsAborted: boolean;
    FAbortedLock: TCriticalSection;
    FSSLOptions: TScSSLClientOptions;

    FCapabilities: TStrings;
    FFeaturesRequested: boolean;
    FExtListSupported: boolean;
    FIsUsedNATFastConnection: boolean;
    FIsUsedExtendedDataAddress: boolean; // NAT extensions (RFC 2428)
    FIsDataSettingsSent: boolean;
    FIsUsedClearingControlChannel: boolean;
    FAuthCommand: TScFTPAuthCommand;
    FTLSMode: TScTLSMode;
    FEncryptDataChannel: boolean;
    FIsUsedSSCN: boolean;

    FListStrings: TStrings;
    FListType: TScFTPListType;
    FDirFormat: string;
    FDirectoryListing: TScFTPDirectoryListing;
    FEncoding: Encoding;

    FBeforeConnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnSendCommand: TScSendLineEvent;
    FOnReadReply: TScReadLineEvent;

    FOnBanner: TScBannerEvent;
    FOnError: TScFTPErrorEvent;
    FOnProgress: TScOnProgressEvent;
    FBeforeUploadFile: TScFTPBeforeUploadFileEvent;
    FAfterUploadFile: TScFTPAfterUploadFileEvent;
    FBeforeDownloadFile: TScFTPBeforeDownloadFileEvent;
    FAfterDownloadFile: TScFTPAfterDownloadFileEvent;
    FBeforeRetrieveList: TScFTPOnRetrieveListEvent;
    FAfterRetrieveList: TScFTPOnRetrieveListEvent;
    FBeforeParseListing: TNotifyEvent;
    FAfterParseListing: TNotifyEvent;

    procedure SetSSLOptions(Value: TScSSLClientOptions);
    procedure SetUri(const Value: string);
    procedure SetHostName(const Value: string);
  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: integer);
    procedure SetUsername(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetTimeout(const Value: integer);
    procedure SetOptions(Value: TScFTPClientOptions);
    procedure SetProxyOptions(Value: TProxyOptions);
    procedure SetUseCompression(const Value: boolean);
    procedure TrySendTransferMode;

    procedure SetUsePassive(Value: boolean);
    procedure TryNATFastConnection;
    procedure SetAuthCommand(const Value: TScFTPAuthCommand);
    procedure SetTLSMode(const Value: TScTLSMode);
    procedure SetEncryptDataChannel(Value: boolean);
    procedure SetSocketReceiveBufferSize(Value: integer);
    procedure SetSocketSendBufferSize(Value: integer);

    function GetUseUTF8: boolean;
    procedure SetUseUTF8(Value: boolean);

    function GetActive: boolean;
    procedure CheckInactive;

    procedure DoAfterDisconnect(Sender: TObject);
    procedure SendCommandEvent(const Line: string);
    procedure ReadReplyEvent(const Line: string);
    procedure DoBanner(const Banner: string);

    function CreateConnection(const AHost: string; APort: integer): TScSecureConnection;
    function InitConnection(Vio: TCRVioTcp): TScSecureConnection;
    procedure Listen(const ListenAddress: string; const Port: integer);
    procedure PrepareDataConnection(ATransferType: TScFTPTransferType; const Command: string);
    procedure OpenDataConnection(const Command: string);
    procedure CloseDataConnection;
    procedure OnWriteData(Stream: TStream; const Data: TBytes; Count: integer);
    function OnReadData(Stream: TStream; var Data: TBytes; Count: integer): integer;
    procedure DoProgress(const Total, Current: Int64);
    function GetDirectoryListing: TScFTPDirectoryListing;
    function GetIsSecure: boolean;

    function GetAuthExt: string;
    function GetEncoding: Encoding;
    // procedure ExtractExtParams(const Command: string; Results: TStrings);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    class function IsEndMarker(const Line: string): boolean;
    class function IsEndReply(const ReplyCode, Line: string): boolean;
    class function IsValidCode(const StrCode: string): boolean;
    procedure GetResponse;
    procedure CheckResponse(const AllowedResponses: array of integer);
    function GetFormattedReply: TStringList;

    procedure WriteStr(const Data: string; AEncoding: Encoding = nil);
    procedure SendDataSettings;
    procedure SendPort(const IP: string; Port: integer);
    procedure SendEPort(const IP: string; Port: integer; IPVersion: TIPVersion);
    procedure SendPassive(UseCPassive: boolean = False);
    procedure SendEPassive;
    procedure SendTransferType(const Value: TScFTPTransferType);
    procedure ParseIPAddress(const Response: string; out IP: string; out Port: integer);
    procedure ParseEPSV(const Response: string; out IP: string; out Port: integer);
    procedure Syst;
    procedure SendPret(const Command: string);
    procedure SetSSCN(Value: boolean);
    procedure CheckFeatures;

    procedure InternalUpload(const Command, DestFile: string; Source: TStream; Append: boolean = False; StartPos: Int64 = -1);
    procedure InternalDownload(const Command: string; Dest: TStream; StartPos: Int64; ATransferType: TScFTPTransferType);
    procedure InternalListDir(List: TStrings; const Path: string; DetailInfo: boolean);

    class procedure ValidateFXP(SourceClient, DestClient: TScFTPClient);
    procedure SendFileOtherClient(DestClient: TScFTPClient; const SrcFile, DstFile: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure RaiseLastCommandError;
    function SendCmd(const Command: string; const AllowedResponses: array of integer): integer;

    procedure Account(const AAccountInfo: string);
    procedure Login; overload;
    procedure Login(AUsername, APassword, AAccountInfo: string); overload;
    procedure ChangeDir(const Path: string);
    procedure ChangeDirUp;
    procedure MountStructure(const Path: string);
    procedure Reinitialize;
    procedure SendFileStructure(const Value: TScFTPFileStructure);

    procedure Abort;
    procedure Allocate(Size: integer);
    procedure Rename(const OldPath, NewPath: string);
    procedure Delete(const Filename: string);
    procedure RemoveDir(const Path: string);
    procedure MakeDir(const Path: string);
    function GetCurrentDir: string;
    procedure Site(const Command: string);
    procedure Status(const Path: string = ''; StatusResponse: TStrings = nil);
    procedure Help(const Command: string = ''; HelpInfo: TStrings = nil);
    procedure Noop;
    function Size(const FileName: string): Int64;
    procedure SetCommandOptions(const Command, Options: string);
    function IsExtSupported(const Command: string): boolean;
    function IsTLSSupported: boolean;

    procedure Upload(const SourceFile, DestFile: string; Append: boolean = False; StartPos: Int64 = -1); overload;
    procedure Upload(Source: TStream; const DestFile: string; Append: boolean = False; StartPos: Int64 = -1); overload;
    procedure UploadWithUniqueName(const SourceFile: string; StartPos: Int64 = -1); overload;
    procedure UploadWithUniqueName(Source: TStream; StartPos: Int64 = -1); overload;
    procedure Dowload(const SourceFile, DestFile: string; Overwrite: boolean = False; StartPos: Int64 = -1); overload;
    procedure Dowload(const SourceFile: string; Dest: TStream; StartPos: Int64 = -1); overload;

    procedure ListDir(List: TStrings = nil; const Path: string = '');
    procedure ListDirDetails(List: TStrings = nil; const Path: string = '');
    procedure ExtListDirDetails(List: TStrings = nil; const Path: string = '');
    procedure GetListItem(List: TStrings; DirList: TScFTPDirectoryListing; const Item: string);

    class procedure SiteToSiteTransfer(SourceClient, DestClient: TScFTPClient;
      const SourceFile, DestFile: string);

    property Active: boolean read GetActive;
    property DirectoryListing: TScFTPDirectoryListing read GetDirectoryListing;
    property DirectoryFormat: string read FDirFormat;
    property IsCompressionSupported: boolean read FIsCompressionSupported;
    property SystemDescription: string read FSystemDescription;
    property ServerDescription: string read FServerDescription;
    property IsUsedNATFastConnection: boolean read FIsUsedNATFastConnection;
    property IsUsedExtendedDataAddress: boolean read FIsUsedExtendedDataAddress;
    property IsSecure: boolean read GetIsSecure;
    property ReplyCode: integer read FLastReplyCode;
    property FormattedReply: TStringList read GetFormattedReply;

    property CEncoding: Encoding read FEncoding write FEncoding;

  published
    property HostName: string read FHostName write SetHostName;
    property Port: integer read FPort write SetPort default FTP_DEFAULT_PORT;
    property Timeout: integer read FTimeout write SetTimeout default DEFAULT_TIMEOUT;
    property Username: string read FUsername write SetUsername;
    property Password: string read FPassword write SetPassword;
    property AccountInfo: string read FAccountInfo write FAccountInfo;
    property DataIP: string read FDataIP write FDataIP;
    property DataPort: integer read FDataPort write FDataPort default 0;
    property ListenTimeout: integer read FListenTimeout write FListenTimeout default DEF_ListenTimeout;
    property UsePassive: boolean read FUsePassive write SetUsePassive default DEF_UsePassive;
    property TransferType: TScFTPTransferType read FTransferType write FTransferType default DEF_TransferType;
    property UseCompression: boolean read FUseCompression write SetUseCompression default DEF_UseCompression;
    property Uri: string read FUri write SetUri;
    property UseUTF8: boolean read GetUseUTF8 write SetUseUTF8 default False;

    property AuthCommand: TScFTPAuthCommand read FAuthCommand write SetAuthCommand default DEF_AuthCommand;
    property TLSMode: TScTLSMode read FTLSMode write SetTLSMode default DEF_TLSMode;
    property EncryptDataChannel: boolean read FEncryptDataChannel write SetEncryptDataChannel default DEF_EncryptDataChannel;

    property Options: TScFTPClientOptions read FOptions write SetOptions;
    property ProxyOptions: TProxyOptions read FProxyOptions write SetProxyOptions;
    property SSLOptions: TScSSLClientOptions read FSSLOptions write SetSSLOptions;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property OnSendCommand: TScSendLineEvent read FOnSendCommand write FOnSendCommand;
    property OnReadReply: TScReadLineEvent read FOnReadReply write FOnReadReply;

    property OnBanner: TScBannerEvent read FOnBanner write FOnBanner;
    property OnError: TScFTPErrorEvent read FOnError write FOnError;
    property OnProgress: TScOnProgressEvent read FOnProgress write FOnProgress;
    property BeforeUploadFile: TScFTPBeforeUploadFileEvent read FBeforeUploadFile write FBeforeUploadFile;
    property AfterUploadFile: TScFTPAfterUploadFileEvent read FAfterUploadFile write FAfterUploadFile;
    property BeforeDownloadFile: TScFTPBeforeDownloadFileEvent read FBeforeDownloadFile write FBeforeDownloadFile;
    property AfterDownloadFile: TScFTPAfterDownloadFileEvent read FAfterDownloadFile write FAfterDownloadFile;
    property BeforeRetrieveList: TScFTPOnRetrieveListEvent read FBeforeRetrieveList write FBeforeRetrieveList;
    property AfterRetrieveList: TScFTPOnRetrieveListEvent read FAfterRetrieveList write FAfterRetrieveList;
    property BeforeParseListing: TNotifyEvent read FBeforeParseListing write FBeforeParseListing;
    property AfterParseListing: TNotifyEvent read FAfterParseListing write FAfterParseListing;
  end;

  EScFTPError = class(EScError)
  protected
    FFTPErrorCode: integer;
  public
    constructor Create(FTPErrorCode: integer; const ErrorMessage: string);
    property FTPErrorCode: integer read FFTPErrorCode;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinSock, {$IFNDEF FPC}Windows,{$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
  Posix.NetinetIn, Posix.SysSocket,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
{$IFDEF VER17P}
  Types,
{$ENDIF}
  StrUtils, ScFunctions;

const
  FtpScheme = 'ftp:';
  TLS_AUTH_NAMES: array [0..3] of string = ('TLS', 'TLS-C', 'SSL', 'TLS-P');

{ EScFTPError }

constructor EScFTPError.Create(FTPErrorCode: integer; const ErrorMessage: string);
begin
  inherited CreateFmt(SFTPServerError, [ErrorMessage], seFTPServerError);
  FFTPErrorCode := FTPErrorCode;
end;

{ TScFTPClientOptions }

constructor TScFTPClientOptions.Create(Client: TScFTPClient);
begin
  inherited Create;

  Assert(Client <> nil);
  FClient := Client;
  FTCPKeepAlive := True;
  FIPVersion := DefValIPVersion;
  FSocketReceiveBufferSize := DEFAULT_SOCKET_BUFFER_SIZE;
  FSocketSendBufferSize := DEFAULT_SOCKET_BUFFER_SIZE;
  FBlockSize := DEFAULT_BLOCK_SIZE;
  FIgnoreServerPassiveHost := DEF_IgnoreServerPassiveHost;
  FUseNATFastConnection := DEF_UseNATFastConnection;
  FUseExtendedDataAddress := DEF_UseExtendedDataAddress;
  FUseClearingControlChannel := DEF_UseClearingControlChannel;
  FUseExtList := DEF_UseExtList;
end;

procedure TScFTPClientOptions.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScFTPClientOptions) then begin
    TScFTPClientOptions(Dest).FClient.CheckInactive;
    TScFTPClientOptions(Dest).FBindAddress := FBindAddress;
    TScFTPClientOptions(Dest).FTCPKeepAlive := FTCPKeepAlive;
    TScFTPClientOptions(Dest).FIPVersion := FIPVersion;
    TScFTPClientOptions(Dest).FSocketReceiveBufferSize := FSocketReceiveBufferSize;
    TScFTPClientOptions(Dest).FSocketSendBufferSize := FSocketSendBufferSize;
    TScFTPClientOptions(Dest).FBlockSize := FBlockSize;
    TScFTPClientOptions(Dest).FIgnoreServerPassiveHost := FIgnoreServerPassiveHost;
    TScFTPClientOptions(Dest).FUseNATFastConnection := FUseNATFastConnection;
    TScFTPClientOptions(Dest).FUseExtendedDataAddress := FUseExtendedDataAddress;
    TScFTPClientOptions(Dest).FUseClearingControlChannel := FUseClearingControlChannel;
    TScFTPClientOptions(Dest).FUseExtList := FUseExtList;
  end
  else
    inherited;
end;

procedure TScFTPClientOptions.SetBindAddress(const Value: string);
begin
  if Value <> FBindAddress then begin
    FClient.CheckInactive;
    FBindAddress := Trim(Value);
  end;
end;

procedure TScFTPClientOptions.SetTCPKeepAlive(Value: boolean);
begin
  if Value <> FTCPKeepAlive then begin
    FClient.CheckInactive;
    FTCPKeepAlive := Value;
  end;
end;

procedure TScFTPClientOptions.SetIPVersion(const Value: TIPVersion);
begin
  if Value <> FIPVersion then begin
    FClient.CheckInactive;
    FIPVersion := Value;
    if FIPVersion = ivIPv6 then
      FUseExtendedDataAddress := True;
  end;
end;

procedure TScFTPClientOptions.SetSocketReceiveBufferSize(Value: integer);
begin
  if Value <> FSocketReceiveBufferSize then begin
    FSocketReceiveBufferSize := Value;
    FClient.SetSocketReceiveBufferSize(Value);
  end;
end;

procedure TScFTPClientOptions.SetSocketSendBufferSize(Value: integer);
begin
  if Value <> FSocketSendBufferSize then begin
    FSocketSendBufferSize := Value;
    FClient.SetSocketSendBufferSize(Value);
  end;
end;

procedure TScFTPClientOptions.SetUseNATFastConnection(Value: boolean);
begin
  if Value <> FUseNATFastConnection then begin
    FClient.CheckInactive;
    FUseNATFastConnection := Value;

    if FUseNATFastConnection then begin
      FClient.FUsePassive := True;
      FUseExtendedDataAddress := True;
    end;
  end;
end;

procedure TScFTPClientOptions.SetUseExtendedDataAddress(Value: boolean);
begin
  if not Value and (FIPVersion = ivIPv6) then
    raise EScError.Create(seFTPSetUseExtendedDataAddressWithIPv6);

  if not Value and FUseNATFastConnection then
    raise EScError.Create(seFTPSetUseExtendedDataAddressWithUseNATFastConnection);

  if Value <> FUseExtendedDataAddress then begin
    FClient.CheckInactive;
    FUseExtendedDataAddress := Value;
  end;
end;

procedure TScFTPClientOptions.SetUseClearingControlChannel(Value: boolean);
begin
  if FClient.Active then
    if Value and (FClient.FTLSMode = tmDisableTLS) then
      raise EScError.Create(seFTPSetUseClearingControlChannelWithNoTLS);

  FUseClearingControlChannel := Value;
end;

{ TScFTPClient }

constructor TScFTPClient.Create(AOwner: TComponent);
begin
  inherited;

  FProxyOptions := TProxyOptions.Create;
  FSSLOptions := TScSSLClientOptions.Create(Self);
  FOptions := TScFTPClientOptions.Create(Self);
  FLastReply := TStringList.Create;
  FFormattedReply := TStringList.Create;
  FCapabilities := TStringList.Create;
  FListStrings := TStringList.Create;
  FAbortedLock := TCriticalSection.Create;

  FPort := FTP_DEFAULT_PORT;
  FTimeout := DEFAULT_TIMEOUT;
  FListenTimeout := DEF_ListenTimeout;
  FUsePassive := DEF_UsePassive;
  FTransferType := DEF_TransferType;
  FUsedTransferType := TScFTPTransferType(-1); // reset to unused value
  FUseCompression := DEF_UseCompression;
  FIsUsedCompression := False;
  FAuthCommand := DEF_AuthCommand;
  FTLSMode := DEF_TLSMode;
  FEncryptDataChannel := DEF_EncryptDataChannel;
  FIsUsedNATFastConnection := False;
  FIsUsedExtendedDataAddress := False;
  FExtListSupported := False;
end;

destructor TScFTPClient.Destroy;
begin
  Disconnect;

  FDirectoryListing.Free;
  FCompressor.Free;

  FProxyOptions.Free;
  FSSLOptions.Free;
  FOptions.Free;
  FLastReply.Free;
  FFormattedReply.Free;
  FCapabilities.Free;
  FListStrings.Free;
  FAbortedLock.Free;

  inherited;
end;

procedure TScFTPClient.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScFTPClient) then begin
    TScFTPClient(Dest).CheckInactive;
    TScFTPClient(Dest).FProxyOptions.Assign(FProxyOptions);
    TScFTPClient(Dest).FSSLOptions.Assign(FSSLOptions);
    TScFTPClient(Dest).FOptions.Assign(FOptions);
    TScFTPClient(Dest).FHostName := FHostName;
    TScFTPClient(Dest).FPort := FPort;
    TScFTPClient(Dest).FTimeout := FTimeout;
    TScFTPClient(Dest).FUsername := FUsername;
    TScFTPClient(Dest).FPassword := FPassword;
    TScFTPClient(Dest).FAccountInfo := FAccountInfo;
    TScFTPClient(Dest).FDataIP := FDataIP;
    TScFTPClient(Dest).FDataPort := FDataPort;
    TScFTPClient(Dest).FListenTimeout := FListenTimeout;
    TScFTPClient(Dest).FUsePassive := FUsePassive;
    TScFTPClient(Dest).FTransferType := FTransferType;
    TScFTPClient(Dest).SetUseCompression(FUseCompression);
    TScFTPClient(Dest).FAuthCommand := FAuthCommand;
    TScFTPClient(Dest).FTLSMode := FTLSMode;
    TScFTPClient(Dest).FEncryptDataChannel := FEncryptDataChannel;
  end
  else
    inherited;
end;

procedure TScFTPClient.Notification(Component: TComponent; Operation: TOperation);
begin
  if (FSSLOptions <> nil) and (Component = FSSLOptions.Storage) and (Operation = opRemove) then
    FSSLOptions.Storage := nil;

  inherited;
end;

function TScFTPClient.GetEncoding: Encoding;
begin
  if FEncoding <> nil then
    Result := FEncoding
  else
    Result := Encoding.Default;
end;

function TScFTPClient.GetActive: boolean;
begin
  Result := (FControlConnection <> nil) and FControlConnection.CheckIsConnected;
end;

function TScFTPClient.GetIsSecure: boolean;
begin
  Result := (FControlConnection <> nil) and FControlConnection.IsSecure;
end;

function TScFTPClient.CreateConnection(const AHost: string; APort: integer): TScSecureConnection;
var
  ConnectionParameters: TScSecureConnectionParameters;
  opt: integer;
begin
  if (AHost = '') or (APort = 0) then
    raise EScError.CreateFmt(SInvalidInputArg, ['Host, Port'], seInvalidInputArg);

  ConnectionParameters := TScSecureConnectionParameters.Create;
  try
    ConnectionParameters.BindAddress := Options.BindAddress;
    ConnectionParameters.Hostname := AHost;
    ConnectionParameters.Port := APort;
    ConnectionParameters.IPVersion := Options.IPVersion;
    ConnectionParameters.Timeout := FTimeout;
    ConnectionParameters.IsSecure := FTLSMode <> tmDisableTLS;
    ConnectionParameters.ProxyOptions.Assign(FProxyOptions);
    ConnectionParameters.SSLOptions.Assign(FSSLOptions);
    ConnectionParameters.SSLOptions.DisableInsertEmptyFragment := True;
    ConnectionParameters.SSLOptions.UseSecureSessionResumption := True;

    Result := TScSecureConnection.Create;
    try
      Result.CreateVio(ConnectionParameters);
      Result.Connect;

      if Options.TCPKeepAlive then
        opt := 1
      else
        opt := 0;
      Result.SetSocketOption(SOL_SOCKET, SO_KEEPALIVE, opt);

      if Options.SocketReceiveBufferSize > 0 then
        Result.SetSocketOption(SOL_SOCKET, SO_RCVBUF, Options.SocketReceiveBufferSize);
      if Options.SocketSendBufferSize > 0 then
        Result.SetSocketOption(SOL_SOCKET, SO_SNDBUF, Options.SocketSendBufferSize);
    except
      Result.Free;
      raise;
    end;
  finally
    ConnectionParameters.Free;
  end;
end;

function TScFTPClient.InitConnection(Vio: TCRVioTcp): TScSecureConnection;
var
  ConnectionParameters: TScSecureConnectionParameters;
begin
  ConnectionParameters := TScSecureConnectionParameters.Create;
  try
    ConnectionParameters.Timeout := FTimeout;
    ConnectionParameters.IsSecure := FTLSMode <> tmDisableTLS;
    ConnectionParameters.SSLOptions.Assign(FSSLOptions);

    Result := TScSecureConnection.Create;
    try
      Result.InitVio(ConnectionParameters, Vio);

      if Options.SocketReceiveBufferSize > 0 then
        Vio.SetSocketOption(SOL_SOCKET, SO_RCVBUF, Options.SocketReceiveBufferSize);
      if Options.SocketSendBufferSize > 0 then
        Vio.SetSocketOption(SOL_SOCKET, SO_SNDBUF, Options.SocketSendBufferSize);
    except
      Result.Free;
      raise;
    end;
  finally
    ConnectionParameters.Free;
  end;
end;

procedure TScFTPClient.Connect;
var
  CurHostName: string;
begin
  if Active then
    raise EScError.Create(seFTPClientActive);

  if Assigned(BeforeConnect) then
    BeforeConnect(Self);

  if (FTLSMode = tmDisableTLS) and (FEncryptDataChannel or Options.UseClearingControlChannel) then
    raise EScError.Create(seFTPEncryptionNoCompatibility);

  if FHostName = '' then
    CurHostName := LOCAL_HOST
  else
    CurHostName := FHostName;

  if FPort = 0 then
    FPort := FTP_DEFAULT_PORT;

  FreeAndNil(FDirectoryListing);
  FListStrings.Clear;
  FCapabilities.Clear;
  FSystemDescription := '';
  FIsUsedCompression := False;
  FIsDataSettingsSent := False;
  FFeaturesRequested := False;
  FUsedTransferType := TScFTPTransferType(-1); // reset to unused value
  FIsUsedClearingControlChannel := False;
  FIsUsedNATFastConnection := False;
  FIsUsedExtendedDataAddress := Options.UseExtendedDataAddress;
  FIsUsedSSCN := False;

  FControlConnection.Release;
  FControlConnection := nil;

  FControlConnection := CreateConnection(CurHostName, FPort);
  try
    FControlConnection.AfterDisconnect := DoAfterDisconnect;

    if FTLSMode = tmImplicitTLS then
      FControlConnection.IsSecure := True;

    GetResponse;
    if (FLastReplyCode div 100) = 1 then begin
      DoBanner(GetFormattedReply.Text);
      GetResponse;
    end;
    CheckResponse([220]);

    if FLastReply.Count > 0 then
      FServerDescription := FLastReply[0]
    else
      FServerDescription := '';

    Syst;
  except
    FControlConnection.Release;
    FControlConnection := nil;
    raise;
  end;

  if Assigned(AfterConnect) then
    AfterConnect(Self);
end;

procedure TScFTPClient.Disconnect;
begin
  if Assigned(BeforeDisconnect) then
    BeforeDisconnect(Self);

  if Active then
    try
      WriteStr('QUIT' + CRLF);
      if FControlConnection.CheckForDataOnSource(1, 100) > 0 then
        GetResponse;
    except
    end;

  FControlConnection.Release;
  FControlConnection := nil;

  FDataConnection.Release;
  FDataConnection := nil;

  FreeAndNil(FListenSocket);
end;

procedure TScFTPClient.GetResponse;
var
  ReplyLine, ReplyDesc, ReplyCode: string;
begin
  FLastReply.Clear;
  FLastReplyCode := 0;
  FSPMidLines := False;

  ReplyLine := FControlConnection.ReadLine(GetEncoding);
  ReadReplyEvent(ReplyLine);

  ReplyCode := Trim(Copy(ReplyLine, 1, 4));
  if (Length(ReplyCode) >= 4) and (ReplyCode[4] = '-') then
    SetLength(ReplyCode, 3);
  FLastReply.Add(Copy(ReplyLine, Length(ReplyCode) + 2, MaxInt));

  if (Length(ReplyLine) >= 4) and (ReplyLine[4] = '-') then begin
    repeat
      ReplyLine := FControlConnection.ReadLine(GetEncoding);
      ReadReplyEvent(ReplyLine);

      if AnsiSameText(LeftStr(ReplyLine, 3), ReplyCode) then
        ReplyDesc := Copy(ReplyLine, 5{xyz }, MaxInt)
      else begin
        if (Length(ReplyLine) >= 1) and (ReplyLine[1] = ' ') then
          FSPMidLines := True;
        ReplyDesc := TrimLeft(ReplyLine);
      end;
      FLastReply.Add(ReplyDesc);
    until IsEndReply(ReplyCode, ReplyLine);
  end;

  if not IsValidCode(ReplyCode) then
    raise EScError.CreateFmt(SInvalidReplyCode, [ReplyCode], seInvalidReplyCode);
  FLastReplyCode := StrToIntDef(ReplyCode, 0);
end;

class function TScFTPClient.IsEndMarker(const Line: string): boolean;
begin
  Result := (Length(Line) >= 3) and IsNumeric(Copy(Line, 1, 3));
  if Result then
    Result := (Length(Line) = 3) or ((Length(Line) >= 4) and (Line[4] = ' '));
end;

class function TScFTPClient.IsEndReply(const ReplyCode, Line: string): boolean;
begin
  Result := IsEndMarker(Line) and AnsiSameText(LeftStr(Line, 3), ReplyCode);
end;

class function TScFTPClient.IsValidCode(const StrCode: string): boolean;
var
  Code: integer;
begin
  if TryStrToInt(StrCode, Code) then
    Result := (Code >= 100) and (Code <= 599)
  else
    Result := False;
end;

function TScFTPClient.GetFormattedReply: TStringList;
var
  StrCode: string;
  i: integer;
begin
  FFormattedReply.Clear;

  if FLastReplyCode > 0 then begin
    StrCode := IntToStr(FLastReplyCode);
    if FLastReply.Count > 0 then begin
      for i := 0 to FLastReply.Count - 2 do begin
        if (i = 0) or not FSPMidLines then
          FFormattedReply.Add(StrCode + '-' + FLastReply[i])
        else
          FFormattedReply.Add(' ' + FLastReply[i]);
      end;

      FFormattedReply.Add(StrCode + ' ' + FLastReply[FLastReply.Count - 1]);
    end
    else
      FFormattedReply.Add(StrCode + ' ');
  end
  else
  if FLastReply.Count > 0 then
    FFormattedReply.AddStrings(FLastReply);

  Result := FFormattedReply;
end;

procedure TScFTPClient.CheckResponse(const AllowedResponses: array of integer);
var
  Fail: boolean;
  ErrorMessage: string;
  i: integer;
begin
  for i := Low(AllowedResponses) to High(AllowedResponses) do begin
    if FLastReplyCode = AllowedResponses[i] then
      Exit;
  end;

  // FTP server can return 421 when it is going to shut down the connection
  if (High(AllowedResponses) > -1) or (FLastReplyCode = 421) then begin
    if FLastReplyCode = 421 then
      try
        Disconnect;
      except
      end;

    Fail := True;
    ErrorMessage := FLastReply.Text;
    if Assigned(OnError) then
      OnError(Self, FLastReplyCode, ErrorMessage, Fail);
    if Fail then
      raise EScFTPError.Create(FLastReplyCode, ErrorMessage);
  end;
end;

procedure TScFTPClient.RaiseLastCommandError;
begin
  raise EScFTPError.Create(FLastReplyCode, FLastReply.Text);
end;

procedure TScFTPClient.WriteStr(const Data: string; AEncoding: Encoding = nil);
begin
  if AEncoding <> nil then
    FControlConnection.Write(AEncoding.GetBytes(Data))
  else
    FControlConnection.Write(GetEncoding.GetBytes(Data));

  SendCommandEvent(Data);
end;

function TScFTPClient.SendCmd(const Command: string; const AllowedResponses: array of integer): integer;
begin
  if not Active then
    raise EScError.Create(seFTPClientNotConnected);

  WriteStr(Command + CRLF);
  GetResponse;
  CheckResponse(AllowedResponses);
  Result := FLastReplyCode;
end;

procedure TScFTPClient.Login;
begin
  Login(FUsername, FPassword, FAccountInfo);
end;

procedure TScFTPClient.Login(AUsername, APassword, AAccountInfo: string);
var
  i: integer;
begin
  if FTLSMode in [tmRequireExplicitTLS, tmAllowExplicitTLS] then begin
    if FAuthCommand = acAuto then begin
      for i := 0 to 3 do begin
        SendCmd('AUTH ' + TLS_AUTH_NAMES[i], []);
        if (FLastReplyCode = 234) or (FLastReplyCode = 334) then begin
          try
            FControlConnection.IsSecure := True;
          except
            if FTLSMode = tmRequireExplicitTLS then
              raise;
          end;
          Break;
        end
        else
        if (FLastReplyCode div 500) <> 1 then begin
          if FTLSMode = tmRequireExplicitTLS then begin
            Disconnect;
            raise EScError.Create(seSSLNegotiationCommandFailed);
          end;
          Break;
        end;
      end;
    end
    else begin
      SendCmd('AUTH ' + TLS_AUTH_NAMES[Ord(FAuthCommand) - 1], []);
      if (FLastReplyCode = 234) or (FLastReplyCode = 334) then begin
        try
          FControlConnection.IsSecure := True;
        except
          if FTLSMode = tmRequireExplicitTLS then
            raise;
        end;
      end
      else begin
        if FTLSMode = tmRequireExplicitTLS then begin
          Disconnect;
          raise EScError.Create(seSSLNegotiationCommandFailed);
        end;
      end;
    end;

    if not FControlConnection.IsSecure and (FTLSMode = tmRequireExplicitTLS) then begin
      Disconnect;
      raise EScError.Create(seSSLNegotiationCommandFailed);
    end;
  end;

  SendCmd('USER ' + AUsername, [230, 232, 331, 530]);
  if (FLastReplyCode = 530) and
     (AUsername = '') and (APassword = '') and (FUsername = '') and (FPassword = '')
  then begin
    FUsername := 'anonymous';
    FPassword := 'guest';
    AUsername := 'anonymous';
    APassword := 'guest';
    SendCmd('USER ' + AUsername, [230, 232, 331]);
  end
  else
    CheckResponse([230, 232, 331]);

  if FLastReplyCode = 331 then begin
    if APassword <> '' then
      SendCmd('PASS ' + APassword, [230, 332])
    else
      CheckResponse([230, 232]); // raise error if response was 331

    if (FLastReplyCode = 332) or ((Pos('ACCOUNT', FLastReply.Text) > 0) and (AAccountInfo <> '')) then begin
      if AAccountInfo <> '' then
        SendCmd('ACCT ' + AAccountInfo, [230, 202])
      else
        CheckResponse([230]); // raise error if response was 332
    end;
  end;

  if FSystemDescription = '' then
    Syst;

  CheckFeatures;

  if Options.UseNATFastConnection and not FIsUsedNATFastConnection then
    TryNATFastConnection;
end;

procedure TScFTPClient.TryNATFastConnection;
begin
  if IsExtSupported('EPSV') then begin
    if SendCmd('EPSV ALL', [200, 229]) = 229 then // 229 - Server treats EPSV ALL as only EPSV command
      SendCmd('ABOR', []);

    FIsUsedNATFastConnection := True;
  end;
end;

procedure TScFTPClient.Account(const AAccountInfo: string);
begin
  if AAccountInfo = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['AccountInfo'], seInvalidInputArg);

  SendCmd('ACCT ' + AAccountInfo, [230]);
end;

procedure TScFTPClient.ChangeDir(const Path: string);
begin
  if Path = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Path'], seInvalidInputArg);

  SendCmd('CWD ' + Path, [200, 250]);
end;

procedure TScFTPClient.ChangeDirUp;
begin
  SendCmd('CDUP', [200, 250]);
end;

procedure TScFTPClient.MountStructure(const Path: string);
begin
  if Path = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Path'], seInvalidInputArg);

  SendCmd('SMNT ' + Path, [202, 250]);
end;

procedure TScFTPClient.Reinitialize;
begin
  if SendCmd('REIN', [120, 220]) = 120 then begin
    GetResponse;
    CheckResponse([220]);
  end;

  FreeAndNil(FDirectoryListing);
  FListStrings.Clear;
  FIsDataSettingsSent := False;
  FIsUsedClearingControlChannel := False;

  if FControlConnection.IsSecure and (FTLSMode <> tmImplicitTLS) then begin
    FControlConnection.DisableCloseSocketOnShutdownAlert := False;
    FControlConnection.IsSecure := False;
  end;
end;

procedure TScFTPClient.ParseIPAddress(const Response: string; out IP: string; out Port: integer);
var
  Len: integer;
  i, j: integer;
  s: string;
begin
  Len := Length(Response);

  i := 1;
  while (i <= Len) and (Response[i] <> ',') do
    Inc(i);

  if i > Len then
    raise EScError.CreateFmt(SFTPInvalidPortFormat, [Response], seFTPInvalidPortFormat);

  Dec(i);
  while CharInSet(Response[i], ['0'..'9']) do
    Dec(i);

  Inc(i);
  IP := '';
  for j := 0 to 3 do begin
    if j > 0 then
      IP := IP + '.';

    while (i <= Len) and CharInSet(Response[i], ['0'..'9']) do begin
      IP := IP + Response[i];
      Inc(i);
    end;

    if i > Len then
      raise EScError.CreateFmt(SFTPInvalidPortFormat, [Response], seFTPInvalidPortFormat);
    Inc(i);
  end;

  Port := 0;
  for j := 0 to 1 do begin
    s := '';
    while (i <= Len) and CharInSet(Response[i], ['0'..'9']) do begin
      s := s + Response[i];
      Inc(i);
    end;
    Inc(i);
    Port := (Port shl 8) + StrToInt(s);
  end;
end;

procedure TScFTPClient.ParseEPSV(const Response: string; out IP: string; out Port: integer);
var
  Left, Right: integer;
  p: integer;
  Delim: Char;
  s: string;
begin
  // Response - '229 Entering Extended Passive Mode (|||59028|)'
  s := Trim(Response);
  Left := Pos('(', s);
  Right := Pos(')', s);
  s := Copy(s, Left + 1, Right - Left - 1);
  Delim := s[1];

  p := Pos(Delim, s);
  System.Delete(s, 1, p);
  p := Pos(Delim, s);
  System.Delete(s, 1, p);

  p := Pos(Delim, s);
  IP := Copy(s, 1, p - 1);
  System.Delete(s, 1, p);
  if IP = '' then
    IP := HostName;

  p := Pos(Delim, s);
  s := Trim(Copy(s, 1, p - 1));
  Port := StrToIntDef(s, 0);
  if (Port < 1) or (Port > 65535) then
    raise EScError.CreateFmt(SFTPInvalidPortFormat, [Response], seFTPInvalidPortFormat);
end;

procedure TScFTPClient.SendPassive(UseCPassive: boolean = False);

  // is not private/reserved/etc
  function IsRoutableAddress(const IP: string): boolean;
  begin
    Result :=
      not SameText(LeftStr(IP, 3), '127') and                    // Loopback   127.0.0.0-127.255.255.255
      not SameText(LeftStr(IP, 3), '10.') and                    // Private    10.0.0.0-10.255.255.255
      not SameText(LeftStr(IP, 7), '169.254') and                // Link-local 169.254.0.0-169.254.255.255
      not SameText(LeftStr(IP, 7), '192.168') and                // Private    192.168.0.0-192.168.255.255
      not (SameText(LeftStr(IP, 3), '172') and (IP[7] = '.') and // Private    172.16.0.0-172.31.255.255
      (StrToInt(Copy(IP, 5, 2)) in [16..31]));
  end;

var
  ControlIP: string;
begin
  SendDataSettings;

  if UseCPassive then
    SendCmd('CPSV', [227])
  else
    SendCmd('PASV', [227]);

  ParseIPAddress(FLastReply[0], FPassiveIP, FPassivePort);

  if not IsRoutableAddress(FPassiveIP) then begin
    ControlIP := FControlConnection.GetRemoteIP;
    if IsRoutableAddress(ControlIP) then
      FPassiveIP := ControlIP;
  end;
end;

procedure TScFTPClient.SendEPassive;
begin
  SendDataSettings;

  SendCMD('EPSV', []);

  if FLastReplyCode = 229 then begin
    try
      ParseEPSV(FLastReply[0], FPassiveIP, FPassivePort);
    except
      SendCmd('ABOR', []);
      raise;
    end;
  end
  else begin
    SendPassive;
    FIsUsedExtendedDataAddress := False;
  end;
end;

procedure TScFTPClient.SendPort(const IP: string; Port: integer);
begin
   SendDataSettings;

  if IP = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['IP'], seInvalidInputArg);

  SendCmd('PORT ' + StringReplace(IP, '.', ',', [rfReplaceAll]) + ',' +
    IntToStr(Port shr 8) + ',' + IntToStr(Port and 255), [200]);
end;

procedure TScFTPClient.SendEPort(const IP: string; Port: integer; IPVersion: TIPVersion);
const
  IP_Versions: array[TIPVersion] of string = ('1', '2', '2');
begin
  SendDataSettings;

  SendCmd('EPRT |' + IP_Versions[IPVersion] + '|' + IP + '|' + IntToStr(Port) + '|', []);

  if FLastReplyCode <> 200 then begin
    SendPort(IP, Port);
    FIsUsedExtendedDataAddress := False;
  end;
end;

procedure TScFTPClient.SendDataSettings;
begin
  if (FTLSMode = tmDisableTLS) or FIsDataSettingsSent then
    Exit;

  FIsDataSettingsSent := True;

  // protect buffer size
  SendCmd('PBSZ 0', [200]);

  // protect data channel
  // 'C' - Clear; 'S' - Safe; 'E' - Confidential; 'P' - Private
  if FEncryptDataChannel then
    SendCmd('PROT P', [200])
  else
    SendCmd('PROT C', [200]);

  if Options.UseClearingControlChannel then begin
    FControlConnection.DisableCloseSocketOnShutdownAlert := True;

    SendCmd('CCC', []);
    FIsUsedClearingControlChannel := (FLastReplyCode div 100) = 2;
    if FIsUsedClearingControlChannel then
      FControlConnection.IsSecure := False;
  end;
end;

procedure TScFTPClient.SendTransferType(const Value: TScFTPTransferType);
const
  Transfer_Types: array[TScFTPTransferType] of string = ('A', 'I');
begin
  if FUsedTransferType <> Value then begin
    SendCmd('TYPE ' + Transfer_Types[Value], [200]);
    FUsedTransferType := Value;
  end;
end;

procedure TScFTPClient.SendFileStructure(const Value: TScFTPFileStructure);
const
  File_Structures: array[TScFTPFileStructure] of string = ('F', 'R', 'P');
begin
  SendCmd('STRU ' + File_Structures[Value], [200]);
end;

procedure TScFTPClient.TrySendTransferMode;
//const
//  Transfer_Modes: array[TScFTPTransferMode] of string = ('S', 'Z');
begin
  if FUseCompression <> FIsUsedCompression then begin
    CheckFeatures;

    if FUseCompression and not FIsCompressionSupported then
      raise EScError.Create(seCompressionNotSupported);

    if FUseCompression then
      SendCmd('MODE Z', [200])
    else
      SendCmd('MODE S', [200]);
    FIsUsedCompression := FUseCompression;
  end;
end;

procedure TScFTPClient.Allocate(Size: integer);
begin
  SendCmd('ALLO ' + IntToStr(Size), [200, 202]);
end;

procedure TScFTPClient.Rename(const OldPath, NewPath: string);
begin
  if OldPath = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['OldPath'], seInvalidInputArg);

  if NewPath = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['NewPath'], seInvalidInputArg);

  SendCmd('RNFR ' + OldPath, [350]);
  SendCmd('RNTO ' + NewPath, [250]);
end;

procedure TScFTPClient.Delete(const Filename: string);
begin
  if Filename = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Filename'], seInvalidInputArg);

  SendCmd('DELE ' + Filename, [200, 250, 257]); // 200 - Linksys NSLU2 NAS, 257 - Ultimodule IDAL
end;

procedure TScFTPClient.RemoveDir(const Path: string);
begin
  if Path = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Path'], seInvalidInputArg);

  SendCmd('RMD ' + Path, [250]);
end;

procedure TScFTPClient.MakeDir(const Path: string);
begin
  if Path = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Path'], seInvalidInputArg);

  SendCmd('MKD ' + Path, [257]);
end;

function TScFTPClient.GetCurrentDir: string;
var
  p: integer;
begin
  SendCmd('PWD', [257]);
  Result := FLastReply[0];

  // Remove all before and after "..."
  p := Pos('"', Result);
  if p = 0 then
     Exit;
  System.Delete(Result, 1, p);

  p := Pos('"', Result);
  if p = 0 then
     Exit;
  Result := Copy(Result, 1, p - 1);
end;

procedure TScFTPClient.Site(const Command: string);
begin
  if Command = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Command'], seInvalidInputArg);

  SendCmd('SITE ' + Command, [200]);
end;

procedure TScFTPClient.Syst;
begin
  SendCmd('SYST', [200, 215, 500, 530]);
  if FLastReplyCode = 530 then
    FSystemDescription := ''
  else
  if FLastReplyCode = 500 then
    FSystemDescription := SUnknown
  else
    FSystemDescription := FLastReply[0];
end;

procedure TScFTPClient.Status(const Path: string = ''; StatusResponse: TStrings = nil);
begin
  SendCmd(Trim('STAT ' + Path), [211, 212, 213]);
  if StatusResponse <> nil then
    StatusResponse.Assign(FLastReply);
end;

procedure TScFTPClient.Help(const Command: string = ''; HelpInfo: TStrings = nil);
begin
  SendCmd(Trim('HELP ' + Command), [211, 214]);
  if HelpInfo <> nil then
    HelpInfo.Assign(FLastReply);
end;

procedure TScFTPClient.Noop;
begin
  SendCmd('NOOP', [200]);
end;

procedure TScFTPClient.Abort;
begin
  if Active then
    WriteStr('ABOR' + CRLF);

  if Assigned(FDataConnection) then begin
    FAbortedLock.Acquire;
    FDataConnection.Disconnect;
    FIsAborted := True;
    FAbortedLock.Release;
  end
  else begin
    GetResponse;
    CheckResponse([]);
  end;
end;

procedure TScFTPClient.SendPret(const Command: string);
begin
  if IsExtSupported('PRET') then
    SendCmd('PRET ' + Command, []); // don't check response as some servers don't work for some cases
end;

procedure TScFTPClient.SetSSCN(Value: boolean);
begin
  if Value then begin
    if not FIsUsedSSCN then begin
      SendCmd('SSCN ON', [200]);
      FIsUsedSSCN := True;
    end;
  end
  else
    if FIsUsedSSCN then
      SendCmd('SSCN OFF', [200]);
end;

procedure TScFTPClient.SetCommandOptions(const Command, Options: string);
begin
  SendCmd('OPTS ' + Command + ' ' + Options, [200]);
end;

function TScFTPClient.Size(const FileName: string): Int64;
var
  Str: string;
begin
  SendTransferType(ttBinary);

  if SendCmd('SIZE ' + FileName, []) = 213 then begin
    Str := Trim(FLastReply.Text);
    System.Delete(Str, 1, Pos(' ', Str));
    Result := StrToInt64Def(Str, -1);
  end
  else
    Result := -1;
end;

procedure TScFTPClient.CheckFeatures;
var
  i: integer;
begin
  if FFeaturesRequested then
    Exit;

  FCapabilities.Clear;
  SendCmd('FEAT', []);
  FFeaturesRequested := True;

  if FLastReplyCode in [211, 221{Ipswitch's WS-FTP}] then begin
    FCapabilities.AddStrings(FLastReply);

    // Remove first and last lines to get only the list
    if FCapabilities.Count > 0 then
      FCapabilities.Delete(0);
    if FCapabilities.Count > 0 then
      FCapabilities.Delete(FCapabilities.Count - 1);
  end;

  if FIsUsedExtendedDataAddress then
    FIsUsedExtendedDataAddress := IsExtSupported('EPRT') and IsExtSupported('EPSV');

  FExtListSupported := IsExtSupported('MLSD') or IsExtSupported('MLST');

  FIsCompressionSupported := False;
  for i := 0 to FCapabilities.Count - 1 do begin
    if Trim(FCapabilities[i]) = 'MODE Z' then begin
      FIsCompressionSupported := True;
      Break;
    end;
  end;
end;

function TScFTPClient.IsExtSupported(const Command: string): boolean;
var
  Ext: string;
  i: integer;
begin
  CheckFeatures;

  Result := False;

  for i := 0 to FCapabilities.Count - 1 do begin
    Ext := TrimLeft(FCapabilities[i]);

    if SameText(ExtractFirstWord(Ext, ' '), Command) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function TScFTPClient.GetAuthExt: string;
var
  Ext, Param: string;
  i, j: integer;
begin
  CheckFeatures;

  Result := '';

  for i := 0 to FCapabilities.Count - 1 do begin
    Ext := TrimLeft(FCapabilities[i]);

    if SameText(ExtractFirstWord(Ext, ' '), 'AUTH') then begin
      repeat
        Param := Trim(ExtractFirstWord(Ext, ';'));

        for j := Low(TLS_AUTH_NAMES) to High(TLS_AUTH_NAMES) do begin
          if SameText(Param, TLS_AUTH_NAMES[j]) then begin
            Result := 'AUTH ' + Param;
            Exit;
          end;
        end;
      until Ext = '';

      Exit;
    end;
  end;
end;
{
procedure TScFTPClient.ExtractExtParams(const Command: string; Results: TStrings);
var
  Ext, Param: string;
  i: integer;
begin
  CheckFeatures;

  Results.Clear;

  for i := 0 to FCapabilities.Count - 1 do begin
    Ext := Trim(FCapabilities[i]);

    if SameText(ExtractFirstWord(Ext, ' '), Command) then begin
      while Ext <> '' do begin
        Param := Trim(ExtractFirstWord(Ext, ';'));
        Results.Add(Param);
      end;

      Exit;
    end;
  end;
end;
}

function TScFTPClient.IsTLSSupported: boolean;
begin
  Result := GetAuthExt <> '';
end;

procedure TScFTPClient.Listen(const ListenAddress: string; const Port: integer);
begin
  FreeAndNil(FListenSocket);

  FListenSocket := TCRVioTcp.Create;
  FListenSocket.BindAddress := ListenAddress;
  FListenSocket.Port := Port;
  FListenSocket.IPVersion := Options.IPVersion;

  FListenSocket.Bind;
  FListenSocket.Listen(1);
end;

procedure TScFTPClient.PrepareDataConnection(ATransferType: TScFTPTransferType; const Command: string);
var
  IP, BindingIP: string;
  BindingPort: integer;
begin
  CheckFeatures;
  TrySendTransferMode;
  SendTransferType(ATransferType);

  if FUsePassive then begin
    SendPret(Command);
    // PASV or EPSV
    if FIsUsedExtendedDataAddress then
      SendEPassive
    else
      SendPassive;

    if Options.IgnoreServerPassiveHost then begin
      IP := FControlConnection.GetRemoteIP;
      if IP <> '' then
        FPassiveIP := IP;
    end;
  end
  else begin
    if FDataIP <> '' then
      BindingIP := FDataIP
    else
    if FControlConnection <> nil then
      BindingIP := FControlConnection.GetLocalIP;

    if BindingIP = '' then
      BindingIP := Options.BindAddress;

    Listen(BindingIP, FDataPort);

    BindingIP := FListenSocket.GetLocalIP;
    BindingPort := FListenSocket.GetLocalPort;

    if FIsUsedExtendedDataAddress then
      SendEPort(BindingIP, BindingPort, FOptions.IPVersion)
    else
      SendPort(BindingIP, BindingPort);
  end;
end;

procedure TScFTPClient.OpenDataConnection(const Command: string);
var
  NewSd: NativeInt;
  From: PSockAddr;
  Vio: TCRVioTcp;
begin
  Assert(FDataConnection = nil);

  if FUsePassive then begin
    WriteStr(Command + CRLF);
    FDataConnection := CreateConnection(FPassiveIP, FPassivePort);
    GetResponse;
    CheckResponse([110, 125, 150, 154]);
  end
  else begin
    Assert(FListenSocket <> nil);

    SendCmd(Command, [125, 150, 154]);

    try
      if FListenSocket.WaitForData(FListenTimeout * 1000) then begin
        FListenSocket.Accept(NewSd, From);
        Vio := FListenSocket.CreateNew(NewSd, From);
        FDataConnection := InitConnection(Vio);
      end
      else
        raise EScError.Create(seAcceptTimedOut);
    finally
      FreeAndNil(FListenSocket);
    end;
  end;

  if (FTLSMode <> tmDisableTLS) and FEncryptDataChannel then begin
    FDataConnection.AssignSession(FControlConnection);
    FDataConnection.IsSecure := True;
  end;
end;

procedure TScFTPClient.CloseDataConnection;
const
  AbortReplies: array [0..5] of integer = (226, 425, 426, 450, 451, 550);
var
  IsAborted: boolean;
  PrevReplyCode: integer;
  i: integer;
begin
  if FDataConnection <> nil then begin
    FDataConnection.Disconnect;
    FDataConnection.Release;
    FDataConnection := nil;
  end;

  if (FLastReplyCode div 100) > 2 then
    Exit;

  FAbortedLock.Acquire;
  IsAborted := FIsAborted;
  FAbortedLock.Release;

  if IsAborted then begin
    GetResponse;
    CheckResponse([225, 226, 250, 425, 426, 450, 451, 550, 552]);
    PrevReplyCode := FLastReplyCode;

    for i := Low(AbortReplies) to High(AbortReplies) do
      if PrevReplyCode = AbortReplies[i] then begin
        GetResponse;
        CheckResponse([225, 226]);
        Break;
      end;

    if PrevReplyCode = 226 then
      if FControlConnection.WaitForData(10) then begin
        GetResponse;
        CheckResponse(AbortReplies);
      end;
  end
  else begin
    GetResponse;
    CheckResponse([225, 226, 250]);
  end;
end;

procedure TScFTPClient.DoProgress(const Total, Current: Int64);
var
  Cancel: boolean;
begin
  if Assigned(FOnProgress) then begin
    Cancel := False;
    FOnProgress(Self, Total, Current, Cancel);

    if Cancel then
      raise EScError.Create(seFTPOperationCancelled);
  end;
end;

procedure TScFTPClient.UploadWithUniqueName(const SourceFile: string; StartPos: Int64 = -1);
var
  SourceStream: TStream;
begin
  if Trim(SourceFile) = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['SourceFile'], seInvalidInputArg);

  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    UploadWithUniqueName(SourceStream, StartPos);
  finally
    SourceStream.Free;
  end;
end;

procedure TScFTPClient.UploadWithUniqueName(Source: TStream; StartPos: Int64 = -1);
begin
  InternalUpload('STOU', '', Source, True, StartPos);
end;

procedure TScFTPClient.Upload(const SourceFile, DestFile: string;
  Append: boolean = False; StartPos: Int64 = -1);
var
  SourceStream: TStream;
begin
  if Trim(SourceFile) = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['SourceFile'], seInvalidInputArg);

  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    Upload(SourceStream, DestFile, Append, StartPos);
  finally
    SourceStream.Free;
  end;
end;

procedure TScFTPClient.Upload(Source: TStream; const DestFile: string;
  Append: boolean = False; StartPos: Int64 = -1);
var
  Command: string;
begin
  if Trim(DestFile) = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['DestFile'], seInvalidInputArg);

  if Append then
    Command := 'APPE ' + DestFile
  else
    Command := 'STOR ' + DestFile;

  InternalUpload(Command, DestFile, Source, Append, StartPos);
end;

procedure TScFTPClient.InternalUpload(const Command, DestFile: string; Source: TStream;
  Append: boolean = False; StartPos: Int64 = -1);
var
  Compressor: TScZCompression;
  UploadedSize: Int64;
  CurBlockSize: integer;
  Fail: boolean;
begin
  if Source = nil then
    raise EScError.CreateFmt(SInvalidInputArg, ['Source'], seInvalidInputArg);
  if Command = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Command'], seInvalidInputArg);

  if FUseCompression then begin
    if FCompressor = nil then
      raise EScError.Create(seCompressorNotAssigned);
    Compressor := FCompressor;
  end
  else
    Compressor := nil;

  FIsAborted := False;

  if StartPos > -1 then
    Source.Position := StartPos;

  if Assigned(BeforeUploadFile) then
    BeforeUploadFile(Self, Source, DestFile, StartPos);

  try
    PrepareDataConnection(FTransferType, Command);

    if not Append and (StartPos > -1) then
      SendCmd('REST ' + IntToStr(StartPos), [350]);

    try
      OpenDataConnection(Command);

      FTotalUploadSize := Source.Size - Source.Position;
      DoProgress(FTotalUploadSize, 0);

      if Compressor <> nil then
        Compressor.CompressStream(Source, OnWriteData)
      else begin
        SetLength(FWriteBuffer, Options.BlockSize);

        UploadedSize := 0;
        while FTotalUploadSize > UploadedSize do begin
          CurBlockSize := Source.Read(FWriteBuffer[0], Options.BlockSize);
          if CurBlockSize = 0 then
            raise EScError.Create(seStreamReadError);

          FDataConnection.Write(TValueArr(FWriteBuffer), 0, CurBlockSize);
          Inc(UploadedSize, CurBlockSize);
          DoProgress(FTotalUploadSize, UploadedSize);

//          if FControlConnection.WaitForData(0) then begin
//            GetResponse;
//            CheckResponse([225, 226, 250]);
//          end;
        end;
      end;
    finally
      CloseDataConnection;
    end;
  except
    on EScFTPError do
      raise;
    on E: Exception do begin
      Fail := True;
      if Assigned(OnError) then
        OnError(Self, -1, E.Message, Fail);
      if Fail then
        raise;
    end;
  end;

  if Assigned(AfterUploadFile) then
    AfterUploadFile(Self, Source, DestFile, StartPos);
end;

procedure TScFTPClient.OnWriteData(Stream: TStream; const Data: TBytes; Count: integer);
begin
  FDataConnection.Write(TValueArr(Buffer), 0, Count);
  DoProgress(FTotalUploadSize, FTotalUploadSize - (Stream.Size - Stream.Position));
end;

procedure TScFTPClient.Dowload(const SourceFile, DestFile: string; Overwrite: boolean = False; StartPos: Int64 = -1);
var
  DestStream: TStream;
begin
  if Trim(DestFile) = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['DestFile'], seInvalidInputArg);

  if FileExists(DestFile) then begin
    if Overwrite then
      SysUtils.DeleteFile(DestFile)
    else
      raise EFCreateError.CreateFmt('Cannot create file "%s". The file already exists', [ExpandFileName(DestFile)]);
  end;

  DestStream := TFileStream.Create(DestFile, fmCreate);
  try
    Dowload(SourceFile, DestStream, StartPos);
  finally
    DestStream.Free;
  end;
end;

procedure TScFTPClient.Dowload(const SourceFile: string; Dest: TStream; StartPos: Int64 = -1);
begin
  if Trim(SourceFile) = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['SourceFile'], seInvalidInputArg);

  if Assigned(BeforeDownloadFile) then
    BeforeDownloadFile(Self, SourceFile, Dest, StartPos);

  InternalDownload('RETR ' + SourceFile, Dest, StartPos, FTransferType);

  if Assigned(AfterDownloadFile) then
    AfterDownloadFile(Self, SourceFile, Dest, StartPos);
end;

procedure TScFTPClient.InternalDownload(const Command: string; Dest: TStream; StartPos: Int64; ATransferType: TScFTPTransferType);
var
  Compressor: TScZCompression;
  CurBlockSize: integer;
  Fail: boolean;
begin
  if Dest = nil then
    raise EScError.CreateFmt(SInvalidInputArg, ['Dest'], seInvalidInputArg);
  if Command = '' then
    raise EScError.CreateFmt(SInvalidInputArg, ['Command'], seInvalidInputArg);

  if FUseCompression then begin
    if FCompressor = nil then
      raise EScError.Create(seCompressorNotAssigned);
    Compressor := FCompressor;
  end
  else
    Compressor := nil;

  FIsAborted := False;

  if StartPos > -1 then
    Dest.Position := StartPos;

  try
    PrepareDataConnection(ATransferType, Command);

    if StartPos > -1 then
      SendCmd('REST ' + IntToStr(StartPos), [350]);

    try
      OpenDataConnection(Command);

      FTotalDownloadSize := 0;

      if Compressor <> nil then
        Compressor.DecompressToStream(Dest, OnReadData)
      else begin
        SetLength(FReadBuffer, Options.BlockSize);

        while True do begin
          CurBlockSize := FDataConnection.ReadNoWait(TValueArr(FReadBuffer), 0, Options.BlockSize);
          if CurBlockSize = 0 then
            Break;

          Inc(FTotalDownloadSize, CurBlockSize);
          Dest.WriteBuffer(FReadBuffer[0], CurBlockSize);
          DoProgress(0, FTotalDownloadSize);

//          if FControlConnection.WaitForData(0) then begin
//            GetResponse;
//            CheckResponse([225, 226, 250]);
//          end;
        end;
      end;

      DoProgress(FTotalDownloadSize, FTotalDownloadSize);
    finally
      CloseDataConnection;
    end;
  except
    on EScFTPError do
      raise;
    on E: Exception do begin
      Fail := True;
      if Assigned(OnError) then
        OnError(Self, -1, E.Message, Fail);
      if Fail then
        raise;
    end;
  end;
end;

function TScFTPClient.OnReadData(Stream: TStream; var Data: TBytes; Count: integer): integer;
begin
  Result := FDataConnection.ReadNoWait(TValueArr(Data), 0, Count);
  Inc(FTotalDownloadSize, Result);
  DoProgress(0, FTotalDownloadSize);
end;

procedure TScFTPClient.ListDir(List: TStrings = nil; const Path: string = '');
begin
  InternalListDir(List, Path, False);
end;

procedure TScFTPClient.ListDirDetails(List: TStrings = nil; const Path: string = '');
begin
  InternalListDir(List, Path, True);
end;

procedure TScFTPClient.InternalListDir(List: TStrings; const Path: string; DetailInfo: boolean);
var
  MemoryStream: TMemoryStream;
  buf: TBytes;
  Command: string;
begin
  CheckFeatures;

  if DetailInfo and Options.UseExtList and FExtListSupported then begin
    ExtListDirDetails(List, Path);
    Exit;
  end;

  if Assigned(BeforeRetrieveList) then
    BeforeRetrieveList(Self, Path);

  MemoryStream := TMemoryStream.Create;
  try
    if DetailInfo then
      Command := 'LIST'
    else
      Command := 'NLST';

    InternalDownload(Trim(Command + ' ' + Path), MemoryStream, -1, ttASCII);
    FreeAndNil(FDirectoryListing);

    FDirFormat := '';
    MemoryStream.Position := 0;
    SetLength(buf, MemoryStream.Size);
    MemoryStream.Read(buf[0], Length(buf));
    FListStrings.Text := GetEncoding.GetString(buf);

    if DetailInfo then
      FListType := ltList
    else
      FListType := ltNameList;
  finally
    MemoryStream.Free;
  end;

  if List <> nil then
    List.Assign(FListStrings);

  if Assigned(AfterRetrieveList) then
    AfterRetrieveList(Self, Path);
end;

procedure TScFTPClient.ExtListDirDetails(List: TStrings = nil; const Path: string = '');
var
  MemoryStream: TMemoryStream;
  buf: TBytes;
begin
  if Assigned(BeforeRetrieveList) then
    BeforeRetrieveList(Self, Path);

  MemoryStream := TMemoryStream.Create;
  try
    InternalDownload(Trim('MLSD ' + Path), MemoryStream, -1, FTransferType);
    FreeAndNil(FDirectoryListing);

    MemoryStream.Position := 0;
    SetLength(buf, MemoryStream.Size);
    MemoryStream.Read(buf[0], Length(buf));

    FListStrings.Text := GetEncoding.GetString(buf);
    FListType := ltMList;
    FDirFormat := 'MLST';
  finally
    MemoryStream.Free;
  end;

  if Assigned(List) then
    List.Assign(FListStrings);

  if Assigned(AfterRetrieveList) then
    AfterRetrieveList(Self, Path);
end;

procedure TScFTPClient.GetListItem(List: TStrings; DirList: TScFTPDirectoryListing; const Item: string);
var
  Format: string;
  i: integer;
begin
  if List = nil then
    raise EScError.CreateFmt(SInvalidInputArg, ['List'], seInvalidInputArg);

  List.Clear;

  SendCmd(Trim('MLST ' + Item), [250]);

  for i := 0 to FLastReply.Count - 1 do begin
    if Pos(';', FLastReply[i]) > 0 then
      List.Add(FLastReply[i]);
  end;

  if Assigned(DirList) then
    TryParseListing(List, DirList, '', ltMList, Format);
end;

function TScFTPClient.GetDirectoryListing: TScFTPDirectoryListing;
begin
  if FDirectoryListing = nil then begin
    FDirectoryListing := TScFTPDirectoryListing.Create;

    if Assigned(FBeforeParseListing) then
      FBeforeParseListing(Self);

    try
      if FListStrings.Count > 0 then
        TryParseListing(FListStrings, FDirectoryListing, SystemDescription, FListType, FDirFormat)
      else begin
        FDirFormat := '';
        FDirectoryListing.Clear;
      end;

    finally
      if Assigned(FAfterParseListing) then
        FAfterParseListing(Self);
    end;
  end;

  Result := FDirectoryListing;
end;

class procedure TScFTPClient.SiteToSiteTransfer(SourceClient, DestClient: TScFTPClient;
  const SourceFile, DestFile: string);
var
  UseCPassive: boolean;
begin
  ValidateFXP(SourceClient, DestClient);

  UseCPassive := False;
  if SourceClient.GetIsSecure then begin
    if SourceClient.GetIsSecure and SourceClient.EncryptDataChannel and not SourceClient.IsExtSupported('SSCN') then begin
      SourceClient.SetSSCN(True);
      DestClient.SetSSCN(False);
    end
    else
    if DestClient.GetIsSecure and DestClient.EncryptDataChannel and not DestClient.IsExtSupported('SSCN') then begin
      DestClient.SetSSCN(True);
      SourceClient.SetSSCN(False);
    end
    else
    if DestClient.Options.IPVersion = ivIPv4 then
      UseCPassive := True;
  end;

  if UseCPassive then
    DestClient.SendPassive(True)
  else
  if DestClient.IsUsedExtendedDataAddress then
    DestClient.SendEPassive
  else
    DestClient.SendPassive;

  if SourceClient.IsUsedExtendedDataAddress then
    SourceClient.SendEPort(DestClient.FPassiveIP, DestClient.FPassivePort, DestClient.Options.IPVersion)
  else
    SourceClient.SendPort(DestClient.FPassiveIP, DestClient.FPassivePort);

  SourceClient.SendFileOtherClient(DestClient, SourceFile, DestFile);
end;

class procedure TScFTPClient.ValidateFXP(SourceClient, DestClient: TScFTPClient);
begin
  if DestClient.FIsUsedNATFastConnection then
    raise EScError.Create(seFTPSiteToSiteWhenNATFastConnection);

  if SourceClient.FIsUsedCompression <> DestClient.FIsUsedCompression then
    raise EScError.Create(seFTPTransferModesDifferent);

  if SourceClient.Options.IPVersion <> DestClient.Options.IPVersion then
    raise EScError.Create(seFTPIPVersionsDifferent);

  if SourceClient.GetIsSecure <> DestClient.GetIsSecure then
    raise EScError.Create(seFTPDataProtectionDifferent);

  if SourceClient.GetIsSecure and not (SourceClient.IsExtSupported('SSCN') or DestClient.IsExtSupported('SSCN')) then
    if (DestClient.Options.IPVersion = ivIPv4) and not DestClient.IsExtSupported('CPSV') then
      raise EScError.Create(seFTPSSCNNotSupported);
end;

procedure TScFTPClient.SendFileOtherClient(DestClient: TScFTPClient; const SrcFile, DstFile: string);
var
  DestFile: string;
begin
  if DstFile = '' then
    DestFile := SrcFile
  else
    DestFile := DstFile;

  DestClient.SendCmd('STOR ' + DestFile, [110, 125, 150]);
  try
    SendCmd('RETR ' + SrcFile, [110, 125, 150]);
  except
    DestClient.Abort;
    raise;
  end;

  DestClient.GetResponse;
  GetResponse;
  DestClient.CheckResponse([225, 226, 250]);
  CheckResponse([225, 226, 250]);
end;

procedure TScFTPClient.SendCommandEvent(const Line: string);
begin
  if Assigned(FOnSendCommand) then
    FOnSendCommand(Self, Line);
end;

procedure TScFTPClient.ReadReplyEvent(const Line: string);
begin
  if Assigned(FOnReadReply) then
    FOnReadReply(Self, Line);
end;

procedure TScFTPClient.DoBanner(const Banner: string);
begin
  if Assigned(FOnBanner) then
    FOnBanner(Self, Banner);
end;

procedure TScFTPClient.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

procedure TScFTPClient.CheckInactive;
begin
  if Active then
    raise EScError.Create(seClientOpened);
end;

function TScFTPClient.GetUseUTF8: boolean;
begin
  Result := FEncoding = Encoding.UTF8;
end;

procedure TScFTPClient.SetUseUTF8(Value: boolean);
begin
  if Value then
    FEncoding := Encoding.UTF8
  else
    if (FEncoding = Encoding.UTF8) or (FEncoding = Encoding.Unicode) or (FEncoding = Encoding.BigEndianUnicode) then
      FEncoding := Encoding.Default;
end;

procedure TScFTPClient.SetUseCompression(const Value: boolean);
begin
  if FUseCompression <> Value then begin
    FUseCompression := Value;
    if FUseCompression and (FCompressor = nil) then
      FCompressor := TScZCompression.Create;

    if Active then
      TrySendTransferMode; // check the server's compatibility on setting at once
  end;
end;

procedure TScFTPClient.SetUsePassive(Value: boolean);
begin
  if not Value and Options.UseNATFastConnection then
    raise EScError.Create(seFTPSetNoPassiveModeWithUseNATFastConnection);

  FUsePassive := Value;
end;

procedure TScFTPClient.SetTLSMode(const Value: TScTLSMode);
begin
  if FTLSMode <> Value then begin
    CheckInactive;
    FTLSMode := Value;
  end;
end;

procedure TScFTPClient.SetEncryptDataChannel(Value: boolean);
begin
  if FEncryptDataChannel <> Value then begin
    if Active then begin
      if Value and (FTLSMode = tmDisableTLS) then
        raise EScError.Create(seFTPSetEncryptDataChannelWithNoTLS);

      if FIsUsedClearingControlChannel then
        raise EScError.Create(seFTPSetEncryptDataChannelAfterCCC);

      FIsDataSettingsSent := False;
    end;

    FEncryptDataChannel := Value;
  end;
end;

procedure TScFTPClient.SetAuthCommand(const Value: TScFTPAuthCommand);
begin
  if FAuthCommand <> Value then begin
    CheckInactive;
    FAuthCommand := Value;
  end;
end;

procedure TScFTPClient.SetSocketReceiveBufferSize(Value: integer);
begin
  if GetActive and (Value > 0) then
    FControlConnection.SetSocketOption(SOL_SOCKET, SO_RCVBUF, Value);
end;

procedure TScFTPClient.SetSocketSendBufferSize(Value: integer);
begin
  if GetActive and (Value > 0) then
    FControlConnection.SetSocketOption(SOL_SOCKET, SO_SNDBUF, Value);
end;

procedure TScFTPClient.SetUri(const Value: string);
var
  TrimValue: string;
begin
  TrimValue := Trim(Value);
  if FUri <> TrimValue then begin
    CheckInactive;

    FUri := TrimValue;

    TScHttpParser.ParseURL(FUri, FScheme, FUserName, FPassword,
      FHostName, FPortNo, FPath, FResource, FParameters, FQuery, FFragment);
    if FScheme = '' then
      FScheme := FtpScheme;
    if FPortNo = '' then
      FPort := FTP_DEFAULT_PORT
    else
      FPort := StrToInt(Copy(FPortNo, 2, Length(FPortNo)));

    if (FTLSMode = tmDisableTLS) and AnsiSameText(FScheme, 'ftps:') then begin
      FTLSMode := tmRequireExplicitTLS;
      FEncryptDataChannel := True;
    end;
  end;
end;

procedure TScFTPClient.SetHostName(const Value: string);
begin
  if Value <> FHostName then begin
    CheckInactive;
    FHostName := Trim(Value);
  end;
end;

procedure TScFTPClient.SetPort(const Value: integer);
begin
  if Value <> FPort then begin
    CheckInactive;
    FPort := Value;
  end;
end;

procedure TScFTPClient.SetUsername(const Value: string);
begin
  if Value <> FUsername then begin
    CheckInactive;
    FUsername := Trim(Value);
  end;
end;

procedure TScFTPClient.SetPassword(const Value: string);
begin
  if Value <> FPassword then begin
    CheckInactive;
    FPassword := Trim(Value);
  end;
end;

procedure TScFTPClient.SetTimeout(const Value: integer);
begin
  if Value <> FTimeout then begin
    CheckInactive;
    FTimeout := Value;
  end;
end;

procedure TScFTPClient.SetOptions(Value: TScFTPClientOptions);
begin
  if Value <> FOptions then begin
    CheckInactive;
    FOptions.Assign(Value);
  end;
end;

procedure TScFTPClient.SetProxyOptions(Value: TProxyOptions);
begin
  if Value <> FProxyOptions then begin
    CheckInactive;
    FProxyOptions.Assign(Value);
  end;
end;

procedure TScFTPClient.SetSSLOptions(Value: TScSSLClientOptions);
begin
  if Value <> FSSLOptions then begin
    CheckInactive;
    FSSLOptions.Assign(Value);
  end;
end;

end.

