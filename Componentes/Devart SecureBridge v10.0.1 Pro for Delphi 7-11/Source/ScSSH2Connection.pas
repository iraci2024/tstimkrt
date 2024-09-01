
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSH2Connection;

interface

uses
  Classes, SysUtils, SyncObjs,
  ScCLRClasses, ScVio, ScSSHConnectionParameter, ScReaderWriter,
  ScCryptoTransformIntf, ScSymmetricAlgorithm, ScAlgorithmSupport, ScHashAlgorithm,
  ScBigInteger, ScClient, ScSSHSocket, ScDataHandler, ScSSH2DataHandler,
  ScSSHUtils, ScUtils, ScThread, ScConsts, ScBridge, ScTypes, ScSSH2Channel;


type
  TSsh2Connection = class;
  TKeyExchanger = class;
  TKeyExchangerClass = class of TKeyExchanger;

  TCallbackSsh2PacketHandler = class(TSsh2DataHandler)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TSsh2Connection;

  public
    constructor Create(con: TSsh2Connection);
    procedure OnPacket(Packet: TSsh2Packet); override;
    procedure OnError(Error: Exception); override;
    procedure OnClosed; override;
  end;

  TUserAuthenticationRequest = (arGetBanner, arAuthentication, arSignKey, arReAuthentication, arFinish);
  TAuthenticationResult = (arSuccess, arFailure, arDisconnected);

  TForwardedPort = record
    ListenPort: Integer;
    ToHost: string;
    ToPort: Integer;
  end;

  TSsh2Connection = class(TSshConnection)
  private
    FSequence: Integer; // Packet count for transmission and reception
    FEncCipher: ICryptoTransform;
    FMac: IHashTransform; // MAC for transmission and reception
    FAsyncKeyExchanger: TKeyExchanger;
    FAllDataCount: Int64;
    FServerAliveCount: Integer;
    FLockPacket: TCriticalSection;
    FTimer: TScIntervalProcessor;
    FConnected: boolean;
    FTransmitPacket: TSsh2Packet;
    FUserAuthFailureError: string;
    FCurAuthenticationRequest: TUserAuthenticationRequest;
    FRemotePortForwardingRequestList: TCRList;
    FForwardedPorts: array of TForwardedPort;

    procedure ProcessChannelOpenRequest(Reader: TSSH2DataReader);
    procedure ProcessGlobalRequest(Reader: TSSH2DataReader);
    function ProcessPacket(Packet: TSsh2Packet): boolean;
    procedure OnTimer(Sender: TObject);
    procedure ResetAliveCount;
    function GetReexchangingKeys: boolean;
    procedure AddData(Count: Int64);

  protected
    FTmpClientCompression: IScCompression;
    FTmpServerCompression: IScCompression;
    FCompression: IScCompression;
    FDataHandler: TSsh2DataHandler;
    FKeyExchangerClass: TKeyExchangerClass;

    procedure AsyncReceivePacket(Packet: TSsh2Packet);
    procedure DoUserAuthentication; virtual;
    procedure UserAuthentication; virtual;
    procedure PrepareConnectionClose;
    function ProcessAuthenticationResponse: TAuthenticationResult;
    function ReadData(Count: integer = 0): TDataPacket; override;
    procedure VersionNegotiation; override;
    procedure ServiceNegotiation; virtual;
    procedure Close; override;

    procedure RequestForwardedPort(const AllowedHost: string; const BindPort: Integer);
    procedure RequestCancelForwardedPort(const Host: string; const Port: Integer);

  protected
    procedure OnDebugMessage(AlwaysDisplay: boolean; const msg: TBytes);
    procedure OnIgnoreMessage(const msg: TBytes);
    procedure OnUnknownMessage(mType: Byte; const Data: TBytes);
    procedure OnSuccessRequest(mType: Byte; const Data: TBytes);
    procedure OnFailureRequest(mType: Byte; const Data: TBytes);
    procedure OnError(Sender: TObject; Error: Exception);
    procedure DoAfterConnect(Sender: TObject);
    procedure DoAfterDisconnect(Sender: TObject);

    function CheckSessionRequest: boolean; virtual;
    function CheckPortForwardingRequest(IsRemote: boolean;
      const RemoteHost: string; const RemotePort: Integer;
      out DestHost: string; out DestPort: Integer): boolean; virtual;

    procedure InternalTransmitPacket(const Payload: TBytes; Offset, Length: Integer);

    procedure SendAlive;
    procedure ReexchangeKeys;
    procedure RefreshSendKeys(const SessionID: TBytes;
      Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression);
    procedure RefreshRecvKeys(Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression);

  public
    constructor Create(AParams: TSshConnectionParameters); override;
    procedure Dispose; override;

    procedure Connect(vio: TCRVio); override;
    procedure Disconnect(const msg: string); override;

    function ForwardPort(const RemoteHost: string; const RemotePort: Integer;
      const OriginatorHost: string = LOCAL_HOST; const OriginatorPort: Integer = 4000): TSsh2Channel; virtual;
    function OpenSession: TSsh2Channel;
    function ConfirmChannel(const ChannelRequest: TChannelRequest): TSsh2Channel;
    function OpenForwardedPort(const ListenHost: string; const ListenPort: Integer;
      const ToHost: string; const ToPort: Integer; const MillisecondsTimeout: Integer): boolean;
    procedure CancelForwardedPort(const ListenHost: string; const ListenPort: Integer);

    procedure SendIgnorableData(const msg: string); overload; override;
    procedure SendIgnorableData(const msg: TValueArr; offset, length: Integer); overload; override;

    procedure TransmitPacket(const Payload: TBytes; Offset, Length: Integer);

    // synchronous reception
    function SyncReceivePacket: TSsh2Packet;

    property ReexchangingKeys: boolean read GetReexchangingKeys;
  end;

  TKeyExchangeStatus = (
    sINITIAL, sWAIT_INIT,
    sWAIT_DH_REPLY, sWAIT_DHE_GROUP, sWAIT_DHE_REPLY, sWAIT_ECDH_REPLY,
    sWAIT_NEWKEYS, sFINISHED
  );

  TKeyExchanger = class
  private
    FStartedByHost: boolean; // True if the host sent KEXINIT first
    function VerifyHostKey(const KeyAndCert, Signature, Data: TBytes): boolean;

  protected
    FCon: TSsh2Connection;
    FServerInitPayload: TBytes; // Payload of KEXINIT message
    FClientInitPayload: TBytes;
    FStatus: TKeyExchangeStatus;

    FKeyExchangeAlgorithm: TScKeyExchangeAlgorithm;
    FDHGroupSizeMin: Integer;
    FDHGroupSizePreffered: Integer;
    FDHGroupSizeMax: Integer;

    FDHGroupPrime: TBigInteger;
    FExchSafePrime: TBigInteger;
    g: TBigInteger;
    x: TBigInteger;
    y: TBigInteger;
    e: TBigInteger;
    f: TBigInteger;
    FPreKeyBuf: TBytes;
    FECName: TScECName;
    FECKey: TScKey;
    FClientKeyBuf: TBytes;
    FServerKeyBuf: TBytes;
    FSignatureHashAlg: TScHashAlgorithm;

    FHash: TBytes;
    FSessionID: TBytes;

    function CreateHashAlgorithm: THashAlgorithm;
    function DecideAlgorithm(const MainAlg, SecondaryAlg: string; ErrorCode: TScErrorCode): string; virtual;
    function DeriveKey(const PreKeyBuf, Hash: TBytes; ch: Char; Len: Integer): TBytes;
    procedure SendInit(out InitPayload: TBytes);
    procedure ProcessInit(Packet: TSsh2Packet; out InitPayload: TBytes);
    procedure SendDHInit;
    procedure ProcessDHReply(Packet: TSsh2Packet);
    procedure SendDHERequest;
    procedure ProcessDHEGroup(Packet: TSsh2Packet);
    procedure SendDHEInit;
    procedure ProcessDHEReply(Packet: TSsh2Packet);
    procedure SendECDHInit;
    procedure ProcessECDHReply(Packet: TSsh2Packet);

    procedure SendNewKeys;
    procedure ProcessNewKeys(Packet: TSsh2Packet);
    procedure CalcECDHKeyHash(const KeyAndCert: TBytes);
    procedure CalcDHEKeyHash(const KeyAndCert: TBytes);
    procedure CalcDHKeyHash(const KeyAndCert: TBytes);
    procedure CreateEncAlg(out Cipher: TSymmetricAlgorithm;
      out Mac: IHashTransform; out Compress: IScCompression); virtual;
    procedure CreateDecAlg(out Cipher: TSymmetricAlgorithm;
      out Mac: IHashTransform; out Compress: IScCompression); virtual;

  public
    constructor Create(con: TSsh2Connection; const SessionID: TBytes); virtual;
    destructor Destroy; override;

    function SyncKeyExchange: boolean; virtual;
    procedure StartReexchange; virtual;
    procedure AsyncProcessPacket(Packet: TSsh2Packet); virtual;

    class function VerifyKeySignature(const PublicKey, Data, Signature: TBytes; SignatureHashAlg: TScHashAlgorithm): TScKey;
  end;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  {$IFDEF VER16P}System.Types,{$ELSE}Types,{$ENDIF}
  ScCipherSuites, ScHash, ScSSHConsts, ScFunctions;

{ TPortForwardingEvent }

type
  TPortForwardingEvent = class
  protected
    FSuccess: boolean;
    FEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPortForwardingEvent.Create;
begin
  inherited Create;

  FEvent := CreateEvent;
end;

destructor TPortForwardingEvent.Destroy;
begin
  FEvent.Free;
  inherited;
end;

{ TCallbackSsh2PacketHandler }

constructor TCallbackSsh2PacketHandler.Create(con: TSsh2Connection);
begin
  inherited Create;

  FConnection := con;
end;

procedure TCallbackSsh2PacketHandler.OnPacket(Packet: TSsh2Packet);
begin
  FConnection.AsyncReceivePacket(Packet);
end;

procedure TCallbackSsh2PacketHandler.OnError(Error: Exception);
begin
  Assert(FConnection <> nil);
  FConnection.OnError(FConnection, Error);
end;

procedure TCallbackSsh2PacketHandler.OnClosed;
begin
  Assert(FConnection <> nil);

  try
    FConnection.PrepareConnectionClose;
  finally
    if FConnection.FConnected then
      FConnection.DoAfterDisconnect(FConnection);
  end;
end;

{ TSsh2Connection }

constructor TSsh2Connection.Create(AParams: TSshConnectionParameters);
begin
  inherited Create(AParams);

  FKeyExchangerClass := TKeyExchanger;
  FAsyncKeyExchanger := nil;
  FTimer := nil;

  FLockPacket := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;
  FDataHandler := TCallbackSsh2PacketHandler.Create(Self);
  FTransmitPacket := TSsh2Packet.Create;
  FRemotePortForwardingRequestList := TCRList.Create;
end;

procedure TSsh2Connection.Dispose;
begin
  if FDisposed then
    Exit;

  try
    Close; // must be sure that Socket is closed
  except
  end;

  FreeAndNil(FAsyncKeyExchanger);
  FEncCipher := nil;
  FMac := nil;
  FTmpClientCompression := nil;
  FTmpServerCompression := nil;
  FCompression := nil;

  FreeAndNil(FTimer);
  FreeAndNil(FLockPacket);
  FreeAndNil(FStream);
  FreeAndNil(FDataHandler);
  FreeAndNil(FTransmitPacket);
  FreeAndNil(FRemotePortForwardingRequestList);

  inherited;
  FDisposed := True;
end;

procedure TSsh2Connection.PrepareConnectionClose;
begin
  if FClosed then
    Exit;

  if FTimer <> nil then
    FTimer.Enabled := False;
  UnregisterAllChannels;
  FClosed := True;
end;

procedure TSsh2Connection.Close;
begin
  PrepareConnectionClose;

  if FStream <> nil then begin
    FStream.Close;
    FStream.Vio.Close; // must be sure that Socket is closed
  end;
end;

procedure TSsh2Connection.VersionNegotiation;
var
  Version: string;
begin
  if FParams.ClientVersion = '' then
    FParams.ClientVersion := TSshUtils.ClientVersionString(pSSH2);

  SendVersion(FParams.ClientVersion); // Param.ClientVersion is set in TScSSHClient
  try
    Version := ReadVersion();
  finally
    TScSSHConnectionInfoUtils.SetVersion(FParams.ConnectionInfo, Version);
  end;
  FParams.ServerVersion := Version;
end;

procedure TSsh2Connection.ServiceNegotiation;
var
  wr: TSSH2DataStream;
  re: TSSH2DataReader;
  t: PacketTypeSSH2;
  s: string;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_SERVICE_REQUEST);
    wr.WriteAStr(Sssh_userauth);
    TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;

  re := TSSH2DataReader.Create(SyncReceivePacket);
  try
    t := re.ReadPacketType;
    if t <> SSH_MSG_SERVICE_ACCEPT then
      raise EScError.Create(seUnexpectedPacketType);

    s := Encoding.Default.GetString(re.ReadString);
    if s <> Sssh_userauth then
      raise EScError.Create(seInvalidMessage);
  finally
    re.Free;
  end;
end;

procedure TSsh2Connection.Connect(vio: TCRVio);
var
  kex: TKeyExchanger;
begin
  FStream := TPlainSocket.Create(vio, FDataHandler);
  try
    VersionNegotiation;

    kex := FKeyExchangerClass.Create(Self, nil);
    try
      if not kex.SyncKeyExchange then
        raise EScError.Create(seAuthenticationFailed);
    finally
      kex.Free;
    end;

    ServiceNegotiation;
    DoUserAuthentication;

    FClosed := False;
    FConnected := True;
    FStream.RepeatAsyncRead;
    DoAfterConnect(Self);
  except
    try
      FStream.Close;
    except
    end;
    FStream.Free;
    FStream := nil;
    raise;
  end;

  if FParams.AliveInterval > 0 then begin
    if FTimer = nil then
      FTimer := TScIntervalProcessor.Create(nil);
    FTimer.Interval := FParams.AliveInterval * 1000;
    FTimer.OnTimer := OnTimer;
    FTimer.Enabled := True;
  end;
end;

procedure TSsh2Connection.DoUserAuthentication;
begin
  UserAuthentication;
end;

procedure TSsh2Connection.UserAuthentication;
  procedure UserAuthenticationRequest;
  var
    ServiceName: string;
    wr: TSSH2DataStream;
    Signpack, Signsource: TSSH2DataStream;
    HashAlg: TScHashAlgorithm;
    PublicKeyAlgorithmStr: string;
    SSHKeyData: TBytes;
  begin
    SetLength(SSHKeyData, 0);
    ServiceName := Sssh_Connection;
    Signsource := nil;
    Signpack := nil;
    wr := TSSH2DataStream.Create;

    try
      wr.WritePacketType(SSH_MSG_USERAUTH_REQUEST);
      wr.WriteWStr(WideString(FParams.UserName));
      wr.WriteAStr(ServiceName);

      case FCurAuthenticationRequest of
        arGetBanner: begin
          wr.WriteAStr(SNone);
          FCurAuthenticationRequest := arAuthentication;
        end;

        arAuthentication: begin
          case FParams.AuthenticationType of
            atPassword: begin
              // Password authentication
              wr.WriteAStr(Spassword);
              wr.WriteBool(False);
              wr.WriteWStr(WideString(FParams.Password));
            end;

            atKeyboardInteractive: begin
              // Keyboard-interactive authentication
              wr.WriteAStr(Skeyboard_interactive);
              wr.WriteAStr(''); // lang
              wr.WriteAStr(''); // submethod
            end;

            atPublicKey: begin
              // Public key authentication
              if FParams.ClientKey = nil then
                raise EScError.Create(sePrivateKeyNotFound);
              FParams.ClientKey.Ready := True;

              wr.WriteAStr(Spublickey);
              wr.WriteBool(False);
              wr.WriteAStr(CipherFactory.PublicKeyAlgorithmToSSH2FullName(FParams.ClientKey.Algorithm, FParams.ClientKey.ECData.ECName));
              wr.WriteAsString(TScKeyUtils.GetSSHKeyData(FParams.ClientKey));
            end;
          else
            Assert(False);
          end;

          FCurAuthenticationRequest := arFinish;
        end;

        arSignKey: begin
          PublicKeyAlgorithmStr := CipherFactory.PublicKeyAlgorithmToSSH2FullName(FParams.ClientKey.Algorithm, FParams.ClientKey.ECData.ECName);
          SSHKeyData := TScKeyUtils.GetSSHKeyData(FParams.ClientKey);
          if FParams.ClientKey.Algorithm = aaEC then begin
            if FParams.ClientKey.ECData.ECCryptography = nil then
              raise EScError.Create(seInternalError);

            FParams.ClientKey.ECData.ECCryptography.IsSSHFormat := True;
          end;
          HashAlg := CipherFactory.PublicKeyAlgorithmToHashAlgorithm(FParams.ClientKey.Algorithm, FParams.ClientKey.ECData.ECName);

          Signsource := TSSH2DataStream.Create;
          Signsource.WriteAsString(FSessionID);
          Signsource.WritePacketType(SSH_MSG_USERAUTH_REQUEST);
          Signsource.WriteWStr(WideString(FParams.UserName));
          Signsource.WriteAStr(ServiceName);
          Signsource.WriteAStr(Spublickey);
          Signsource.WriteBool(True);
          Signsource.WriteAStr(PublicKeyAlgorithmStr);
          Signsource.WriteAsString(SSHKeyData);

          Signpack := TSSH2DataStream.Create;
          Signpack.WriteAStr(PublicKeyAlgorithmStr);
          Signpack.WriteAsString(FParams.ClientKey.Sign(Signsource.ToBytes, HashAlg));

          wr.WriteAStr(Spublickey);
          wr.WriteBool(True);
          wr.WriteAStr(PublicKeyAlgorithmStr);
          wr.WriteAsString(SSHKeyData);
          wr.WriteAsString(Signpack.ToBytes);

          FCurAuthenticationRequest := arReAuthentication;
        end;

        arReAuthentication: begin
          FParams.AuthenticationType := atPassword;
          // Password authentication
          wr.WriteAStr(Spassword);
          wr.WriteBool(False);
          wr.WriteWStr(WideString(FParams.Password));
          FCurAuthenticationRequest := arFinish;
        end;
      end;

      TransmitPacket(wr.Data, 0, wr.DataLength);
    finally
      wr.Free;
      Signsource.Free;
      Signpack.Free;
    end;
  end;

var
  Res: TAuthenticationResult;
begin
  FUserAuthFailureError := '';
  FCurAuthenticationRequest := arGetBanner;

  repeat
    UserAuthenticationRequest;
    Res := ProcessAuthenticationResponse;
  until FCurAuthenticationRequest = arFinish;

  if Res <> arSuccess then
    raise EScError.CreateFmt(SAuthenticationFailed + #$D#$A + FUserAuthFailureError, [], seAuthenticationFailed);
end;

function TSsh2Connection.ProcessAuthenticationResponse: TAuthenticationResult;

  procedure KeyboardInteractiveAuthResponse(const Input: TStringDynArray);
  var
    wr: TSSH2DataStream;
    i: Integer;
  begin
    wr := TSSH2DataStream.Create;
    try
      wr.WritePacketType(SSH_MSG_USERAUTH_INFO_RESPONSE);
      wr.WriteInt32(Length(Input));
      for i := 0 to Length(Input) - 1 do
        wr.WriteWStr(WideString(Input[i]));
      TransmitPacket(wr.Data, 0, wr.DataLength);
    finally
      wr.Free;
    end;
  end;

var
  Response: TSSH2DataReader;
  h: PacketTypeSSH2;
  msg, Name, Instr: string;
  Prompts, Responses: TStringDynArray;
  i, num: Integer;
begin
  while True do begin
    Response := TSSH2DataReader.Create(SyncReceivePacket);
    try
      h := Response.ReadPacketType;
      case h of
        SSH_MSG_DISCONNECT: begin
          Response.ReadInt32; // errorcode
          Result := arFailure;
          Exit;
        end;
        SSH_MSG_USERAUTH_FAILURE: begin
          FUserAuthFailureError := Encoding.Default.GetString(Response.ReadString);
          Result := arFailure;
          Exit;
        end;
        SSH_MSG_USERAUTH_SUCCESS: begin
          if FTmpClientCompression <> nil then begin
            FCompression := FTmpClientCompression;
            FTmpClientCompression := nil;
          end;
          if FTmpServerCompression <> nil then begin
            FDataHandler.SetCompression(FTmpServerCompression);
            FTmpServerCompression := nil;
          end;

          FCurAuthenticationRequest := arFinish;
          Result := arSuccess;
          Exit;
        end;
        SSH_MSG_USERAUTH_BANNER: begin
          msg := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString));
          if Assigned(FParams.OnBanner) then
            FParams.OnBanner(Self, msg);
        end;

        SSH_MSG_USERAUTH_INFO_REQUEST {= SSH_MSG_USERAUTH_PK_OK}: begin
          if FParams.AuthenticationType = atPublicKey then begin
            Response.ReadString; // public key algorithm name from the request
            Response.ReadString; // public key blob from the request
            FCurAuthenticationRequest := arSignKey;
            Result := arSuccess;
            Exit;
          end
          else
          if FParams.AuthenticationType = atKeyboardInteractive then begin
            Name := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString));
            Instr := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString));
            Response.ReadString; // lang := Encoding.UTF8.GetString(Response.ReadString);
            num  := Response.ReadInt32;
            SetLength(Prompts, num);
            SetLength(Responses, num);
            for i := 0 to num - 1 do begin
              Prompts[i] := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString));
              Response.ReadBool; // echo
              Responses[i] := '';
            end;

            if Assigned(FParams.OnAuthenticationPrompt) then
              FParams.OnAuthenticationPrompt(Self, Name, Instr, Prompts, Responses)
            else
              if (Length(Prompts) = 1) and SameText(Copy(Prompts[0], 1, Length(Spassword)), Spassword) then
                Responses[0] := FParams.Password;

            KeyboardInteractiveAuthResponse(Responses);
          end
          else
            raise EScError.Create(seUnexpectedPacketType);
        end;

      else
        raise EScError.Create(seUnexpectedPacketType);
      end;
    finally
      Response.Free;
    end;
  end;
end;

function TSsh2Connection.ForwardPort(const RemoteHost: string; const RemotePort: Integer;
  const OriginatorHost: string = LOCAL_HOST; const OriginatorPort: Integer = 4000): TSsh2Channel;
var
  wr: TSSH2DataStream;
begin
  if RemoteHost = '' then
    raise EScError.Create(seWrongHostname);

  Result := TSsh2Channel.Create(Self, ctForwardedLocalToRemote);
  try
    wr := TSSH2DataStream.Create;
    try
      wr.WritePacketType(SSH_MSG_CHANNEL_OPEN);
      wr.WriteAStr(Sdirect_tcpip);
      wr.WriteInt32(Result.LocalChannelID);
      wr.WriteInt32(FParams.WindowSize); // initial window size
      wr.WriteInt32(FParams.MaxPacketSize); // max Packet size
      wr.WriteAStr(RemoteHost);
      wr.WriteInt32(RemotePort);
      wr.WriteAStr(OriginatorHost);
      wr.WriteInt32(OriginatorPort);
      TransmitPacket(wr.Data, 0, wr.DataLength);
    finally
      wr.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

function TSsh2Connection.OpenSession: TSsh2Channel;
var
  wr: TSSH2DataStream;
begin
  Result := TSsh2Channel.Create(Self, ctSession);

  try
    wr := TSSH2DataStream.Create;
    try
      wr.WritePacketType(SSH_MSG_CHANNEL_OPEN);
      wr.WriteAStr(Ssession);
      wr.WriteInt32(Result.LocalChannelID);
      wr.WriteInt32(FParams.WindowSize); // initial window size
      wr.WriteInt32(FParams.MaxPacketSize); // max Packet size
      TransmitPacket(wr.Data, 0, wr.DataLength);
    finally
      wr.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

function TSsh2Connection.ConfirmChannel(const ChannelRequest: TChannelRequest): TSsh2Channel;
var
  wr: TSSH2DataStream;
begin
  Result := TSsh2Channel.Create(Self, ChannelRequest.ChannelType,
    ChannelRequest.RemoteChannelID, ChannelRequest.RemoteWindowSize, ChannelRequest.RemoteMaxPacketSize);

  try
    wr := TSSH2DataStream.Create;
    try
      wr.WritePacketType(SSH_MSG_CHANNEL_OPEN_CONFIRMATION);
      wr.WriteInt32(ChannelRequest.RemoteChannelID);
      wr.WriteInt32(Result.LocalChannelID);
      wr.WriteInt32(FParams.WindowSize); // initial window size
      wr.WriteInt32(FParams.MaxPacketSize); // max Packet size

      TransmitPacket(wr.Data, 0, wr.DataLength);
    finally
      wr.Free;
    end;

    Result.SetReady;
  except
    Result.Free;
    raise;
  end;
end;

procedure TSsh2Connection.RequestForwardedPort(const AllowedHost: string; const BindPort: Integer);
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_GLOBAL_REQUEST);
    wr.WriteAStr(Stcpip_forward);
    wr.WriteBool(True);
    wr.WriteAStr(AllowedHost);
    wr.WriteInt32(BindPort);
    TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TSsh2Connection.RequestCancelForwardedPort(const Host: string; const Port: Integer);
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_GLOBAL_REQUEST);
    wr.WriteAStr(Scancel_tcpip_forward);
    wr.WriteBool(True);
    wr.WriteAStr(Host);
    wr.WriteInt32(Port);
    TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TSsh2Connection.ProcessChannelOpenRequest(Reader: TSSH2DataReader);

  procedure TransmitFailure(RemoteChannelID: Integer; ErrorCode: Integer);
  var
    wr: TSSH2DataStream;
  begin
    wr := TSSH2DataStream.Create;
    try
      wr.WritePacketType(SSH_MSG_CHANNEL_OPEN_FAILURE);
      wr.WriteInt32(RemoteChannelID);
      wr.WriteInt32(ErrorCode);
      wr.WriteWStr(''); // Description
      wr.WriteAStr(''); // lang tag
      TransmitPacket(wr.Data, 0, wr.DataLength);
    finally
      wr.Free;
    end;
  end;

var
  Method: string;
  ChannelRequest: TChannelRequest;
  Host: string;
  Port: Integer;
  IsRemote: boolean;
  Result: integer;
begin
  Method := Encoding.Default.GetString(Reader.ReadString);
  ChannelRequest.RemoteChannelID := Reader.ReadInt32;
  ChannelRequest.RemoteWindowSize := Reader.ReadUInt32;
  ChannelRequest.RemoteMaxPacketSize := Reader.ReadUInt32;

  if (Method = Sdirect_tcpip) or (Method = Sforwarded_tcpip) then begin
    Host := Encoding.Default.GetString(Reader.ReadString);
    Port := Reader.ReadInt32;
    Reader.ReadString; // OriginatorIP
    Reader.ReadInt32; // OriginatorPort
    IsRemote := Method = Sforwarded_tcpip;
    ChannelRequest.ChannelType := ctForwardedRemoteToLocal;

    if CheckPortForwardingRequest(IsRemote, Host, Port, ChannelRequest.Host, ChannelRequest.Port) then begin
      try
        if Assigned(FParams.OnEstablishPortForwarding) then
          FParams.OnEstablishPortForwarding(ChannelRequest);

        Result := 0;
      except
        on SocketException do
          Result := SSH_OPEN_CONNECT_FAILED
        else
          Result := SSH_OPEN_ADMINISTRATIVELY_PROHIBITED;
      end;
    end
    else
      Result := SSH_OPEN_ADMINISTRATIVELY_PROHIBITED;
  end
  else
  if Method = Ssession then begin
    if CheckSessionRequest then begin
      try
        ChannelRequest.Host := '';
        ChannelRequest.Port := 0;
        ChannelRequest.ChannelType := ctSession;

        if Assigned(FParams.OnEstablishSession) then
          FParams.OnEstablishSession(ChannelRequest);

        Result := 0;
      except
        Result := SSH_OPEN_ADMINISTRATIVELY_PROHIBITED;
      end;
    end
    else
      Result := SSH_OPEN_ADMINISTRATIVELY_PROHIBITED;
  end
  else
    Result := SSH_OPEN_UNKNOWN_CHANNEL_TYPE;

  if Result <> 0 then
    TransmitFailure(ChannelRequest.RemoteChannelID, Result);
end;

procedure TSsh2Connection.ProcessGlobalRequest(Reader: TSSH2DataReader);

  function CheckRemotePortForwardingRequest(const Host: string; const Port: Integer): boolean;
  begin
    // Allowed only on server side
    if Assigned(FParams.OnRemotePortForwardingRequest) then begin
      Result := cpAllowRemoteForwarding in FParams.ConnectionInfo.SSHChannelPermissions;
      FParams.OnRemotePortForwardingRequest(Self, FParams.ConnectionInfo, Host, Port, Result);
    end
    else
      Result := False;
  end;

  function CheckCancelRemotePortForwardingRequest(const Host: string; const Port: Integer): boolean;
  begin
    // Allowed only on server side
    if Assigned(FParams.OnCancelRemotePortForwardingRequest) then begin
      Result := True;
      FParams.OnCancelRemotePortForwardingRequest(Self, FParams.ConnectionInfo, Host, Port);
    end
    else
      Result := False;
  end;

  procedure Response(Reply: boolean; ResponseType: PacketTypeSSH2);
  var
    wr: TSSH2DataStream;
  begin
    if Reply then begin
      wr := TSSH2DataStream.Create;
      try
        wr.WritePacketType(ResponseType);
        TransmitPacket(wr.Data, 0, wr.DataLength);
      finally
        wr.Free;
      end;
    end;
  end;

var
  Method: string;
  Host: string;
  Port: Integer;
  Reply: boolean;
  ResponseType: PacketTypeSSH2;
begin
  Method := Encoding.Default.GetString(Reader.ReadString);
  Reply := Reader.ReadBool;
  ResponseType := SSH_MSG_REQUEST_FAILURE;

  try
    if Method = Stcpip_forward then begin
      Host := Encoding.Default.GetString(Reader.ReadString);
      Port := Reader.ReadInt32;

      if CheckRemotePortForwardingRequest(Host, Port) then begin
        Assert(Assigned(FParams.OnEstablishRemotePortForwarding));
        FParams.OnEstablishRemotePortForwarding(Host, Port);
        ResponseType := SSH_MSG_REQUEST_SUCCESS;
      end;
    end
    else if Method = Scancel_tcpip_forward then begin
      Host := Encoding.Default.GetString(Reader.ReadString);
      Port := Reader.ReadInt32;

      if CheckCancelRemotePortForwardingRequest(Host, Port) then begin
        Assert(Assigned(FParams.OnCancelRemotePortForwarding));
        FParams.OnCancelRemotePortForwarding(Host, Port);
        ResponseType := SSH_MSG_REQUEST_SUCCESS;
      end;
    end;

  except
    Response(Reply, ResponseType);
    raise;
  end;

  Response(Reply, ResponseType);
end;

procedure TSsh2Connection.InternalTransmitPacket(const Payload: TBytes; Offset, Length: Integer);
begin
  if (FStream = nil) or FStream.Closed then
    Exit;

  lock(FLockPacket);
  try
    FTransmitPacket.InitWithPayload(Payload, Offset, Length, FEncCipher, FMac, FCompression, FSequence, FParams.Random);
    Inc(FSequence);
    FTransmitPacket.WriteTo(FStream);
  finally
    unlock(FLockPacket);
  end;

  AddData(Length);
end;

procedure TSsh2Connection.TransmitPacket(const Payload: TBytes; Offset, Length: Integer);
var
  rnd: TBytes;
begin
  InternalTransmitPacket(Payload, Offset, Length);

  if (FParams.MsgIgnoreRate > 0) and (FEncCipher <> nil) then begin
    SetLength(rnd, 1);
    FParams.Random.Random(rnd, 0, 1);
    rnd[0] := rnd[0] mod 101;
    if rnd[0] <= FParams.MsgIgnoreRate then
      SendIgnorableData(TValueArr(Payload), Offset, Length);
  end;
end;

function TSsh2Connection.ReadData(Count: integer = 0): TDataPacket;
begin
  try
    Result := FStream.SyncRead(Count);
  except
    on e: SocketException do
      raise EScError.CreateFmt(e.Message, [], seSocketClosed)
    else
      raise;
  end;
end;

function TSsh2Connection.SyncReceivePacket: TSsh2Packet;
var
  DataPacket: TDataPacket;
  NeedBytes: integer;
  r: TSSH2DataReader;
  pt: PacketTypeSSH2;
  msg: string;
begin
  while True do begin
    NeedBytes := 0;
    repeat
      DataPacket := ReadData(NeedBytes);
      Result := FDataHandler.ProcessData(DataPacket, NeedBytes);
    until (Result <> nil) or (NeedBytes = 0);

    if Result = nil then
      raise EScError.Create(seNoPacketReceived);

    r := TSSH2DataReader.Create(Result.Data, Result.DataLength);
    try
      pt := r.ReadPacketType;
      if pt = SSH_MSG_IGNORE then
        OnIgnoreMessage(r.ReadString)
      else
      if pt = SSH_MSG_DEBUG then
        OnDebugMessage(r.ReadBool, r.ReadString)
      else
      if pt = SSH_MSG_DISCONNECT then begin
        r.ReadInt32; // errorcode
        msg := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(r.ReadString));
        raise EScError.CreateFmt(SConnectionClosedWithMessage, [msg], seConnectionClosedWithMessage);
      end
      else
        Exit;
    finally
      r.Free;
    end;
  end; // end while
end;

procedure TSsh2Connection.AsyncReceivePacket(Packet: TSsh2Packet);
begin
  try
    ProcessPacket(Packet);
  except
    on E: Exception do
      if not FClosed then
        OnError(Self, E);
  end;
end;

function TSsh2Connection.ProcessPacket(Packet: TSsh2Packet): boolean;
var
  r: TSSH2DataReader;
  pt: PacketTypeSSH2;
  LocalChannel: Integer;
  ch: TSshChannel;
begin
  r := TSSH2DataReader.Create(Packet);
  try
    pt := r.ReadPacketType;

    if pt = SSH_MSG_DISCONNECT then begin
      r.ReadInt32; // errorcode
      PrepareConnectionClose;
      Result := False;
    end else
    if (FRemotePortForwardingRequestList.Count > 0) and
      ((pt = SSH_MSG_REQUEST_SUCCESS) or (pt = SSH_MSG_REQUEST_FAILURE)) then begin
      if pt = SSH_MSG_REQUEST_SUCCESS then
        OnSuccessRequest(byte(pt), r.Image)
      else
      if pt = SSH_MSG_REQUEST_FAILURE then
        OnFailureRequest(byte(pt), r.Image);

      Result := True;
    end else
    if pt = SSH_MSG_CHANNEL_OPEN then begin
      ProcessChannelOpenRequest(r);
      Result := True;
    end else
    if pt = SSH_MSG_GLOBAL_REQUEST then begin
      ProcessGlobalRequest(r);
      Result := True;
    end else
    if (pt >= SSH_MSG_CHANNEL_OPEN_CONFIRMATION) and
       (pt <= SSH_MSG_CHANNEL_FAILURE) then begin
      LocalChannel := r.ReadInt32;

      lock(FLockChannel);
      try
        // AddData(r.Rest);
        ch := FindChannel(LocalChannel);
        if ch <> nil then
          TSsh2Channel(ch).ProcessPacket(pt, 5 + r.Rest, r);
      finally
        unlock(FLockChannel);
      end;

      Result := True;
    end else
    if pt = SSH_MSG_IGNORE then begin
      if r.Rest > 0 then
        OnIgnoreMessage(r.ReadString);
      Result := True;
    end else
    if FAsyncKeyExchanger <> nil then begin
      FAsyncKeyExchanger.AsyncProcessPacket(Packet);

      if FAsyncKeyExchanger.FStatus = sFINISHED then begin
        FreeAndNil(FAsyncKeyExchanger);
        SendWindowAdjustForAllChannels;
      end;
      Result := True;
    end else
    if pt = SSH_MSG_KEX_INIT then begin
      FAsyncKeyExchanger := FKeyExchangerClass.Create(Self, FSessionID);
      FAsyncKeyExchanger.AsyncProcessPacket(Packet);
      Result := True;
    end else
    if (pt = SSH_MSG_REQUEST_SUCCESS) or (pt = SSH_MSG_REQUEST_FAILURE) then begin
      ResetAliveCount;
      OnUnknownMessage(byte(pt), r.Image);
      Result := False;
    end
    else begin
      OnUnknownMessage(byte(pt), r.Image);
      Result := False;
    end;

  finally
    r.Free;
  end;
end;

procedure TSsh2Connection.OnTimer(Sender: TObject);
begin
  Inc(FServerAliveCount);

  if FServerAliveCount > FParams.AliveCountMax then begin
    Disconnect(STimeoutSession);
    ResetAliveCount;
  end
  else
    SendAlive;
end;

procedure TSsh2Connection.ResetAliveCount;
begin
  FServerAliveCount := 0;
end;

procedure TSsh2Connection.Disconnect(const msg: string);
var
  wr: TSSH2DataStream;
begin
  if FClosed then
    Exit;

  PrepareConnectionClose;

  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_DISCONNECT);
    wr.WriteInt32(0);
    wr.WriteWStr(WideString(msg));
    wr.WriteAStr(''); // language
    InternalTransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;

  FStream.Close;
end;

procedure TSsh2Connection.SendIgnorableData(const msg: string);
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_IGNORE);
    wr.WriteAStr(msg);
    InternalTransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TSsh2Connection.SendIgnorableData(const msg: TValueArr; offset, length: Integer);
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_IGNORE);
    wr.WriteAsString(msg, offset, length);
    InternalTransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TSsh2Connection.SendAlive;
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_GLOBAL_REQUEST);
    wr.WriteAStr(SKeepaliveCom);
    wr.WriteBool(True);
    InternalTransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TSsh2Connection.ReexchangeKeys;
begin
  if FAsyncKeyExchanger = nil then
    FAsyncKeyExchanger := FKeyExchangerClass.Create(Self, FSessionID);
  FAsyncKeyExchanger.StartReexchange;
end;

function TSsh2Connection.GetReexchangingKeys: boolean;
begin
  Result := FAsyncKeyExchanger <> nil;
end;

procedure TSsh2Connection.RefreshSendKeys(const SessionID: TBytes;
  Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression);
begin
  SetLength(FSessionID, Length(SessionID));
  Buffer.BlockCopy(SessionID, 0, FSessionID, 0, Length(SessionID));

  FEncCipher := Cipher;
  FMac := Mac;

  if FParams.ConnectionInfo.CompressionClient = csaZLibOpenSSH then begin
    FTmpClientCompression := Compress;
    FCompression := nil;
  end
  else begin
    FTmpClientCompression := nil;
    FCompression := Compress;
  end;
end;

procedure TSsh2Connection.RefreshRecvKeys(Cipher: ICryptoTransform; Mac: IHashTransform; Compress: IScCompression);
begin
  if FParams.CheckMACError then
    FDataHandler.SetCipher(Cipher, Mac)
  else
    FDataHandler.SetCipher(Cipher, nil);

  if FParams.ConnectionInfo.CompressionServer = csaZLibOpenSSH then begin
    FTmpServerCompression := Compress;
    FDataHandler.SetCompression(nil);
  end
  else begin
    FTmpServerCompression := nil;
    FDataHandler.SetCompression(Compress);
  end;
end;

procedure TSsh2Connection.AddData(Count: Int64);
begin
  Inc(FAllDataCount, Count);
  if (FParams.RekeyLimitSize > 0) and (FAllDataCount >= FParams.RekeyLimitSize) then begin
    FAllDataCount := 0;
    ReexchangeKeys;
  end;
end;

function TSsh2Connection.OpenForwardedPort(const ListenHost: string; const ListenPort: Integer;
  const ToHost: string; const ToPort: Integer; const MillisecondsTimeout: Integer): boolean;
var
  PortForwardEvent: TPortForwardingEvent;
  Idx: Integer;
begin
  if ListenHost = '' then
    raise EScError.Create(seWrongHostname);

  PortForwardEvent := TPortForwardingEvent.Create;
  try
    FRemotePortForwardingRequestList.Add(PortForwardEvent);
    RequestForwardedPort(ListenHost, ListenPort);

    PortForwardEvent.FEvent.WaitFor(cardinal(MillisecondsTimeout));
    Result := PortForwardEvent.FSuccess;

    if Result then begin
      SetLength(FForwardedPorts, Length(FForwardedPorts) + 1);
      Idx := Length(FForwardedPorts) - 1;
      FForwardedPorts[Idx].ListenPort := ListenPort;
      FForwardedPorts[Idx].ToHost := ToHost;
      FForwardedPorts[Idx].ToPort := ToPort;
    end;
  finally
    FRemotePortForwardingRequestList.Remove(PortForwardEvent);
    PortForwardEvent.Free;
  end;
end;

procedure TSsh2Connection.CancelForwardedPort(const ListenHost: string; const ListenPort: Integer);
var
  i: Integer;
begin
  if ListenHost = '' then
    raise EScError.Create(seWrongHostname);

  RequestCancelForwardedPort(ListenHost, ListenPort);

  for i := 0 to Length(FForwardedPorts) - 1 do
    if FForwardedPorts[i].ListenPort = ListenPort then
      FForwardedPorts[i].ListenPort := -1;
end;

function TSsh2Connection.CheckPortForwardingRequest(IsRemote: boolean;
  const RemoteHost: string; const RemotePort: Integer;
  out DestHost: string; out DestPort: Integer): boolean;
var
  i: Integer;
begin
  Result := False;

  if IsRemote and (RemotePort > 0) then
    for i := 0 to Length(FForwardedPorts) - 1 do
      if FForwardedPorts[i].ListenPort = RemotePort then begin
        DestHost := FForwardedPorts[i].ToHost;
        DestPort := FForwardedPorts[i].ToPort;
        Result := True;
        break;
      end;
end;

function TSsh2Connection.CheckSessionRequest: boolean;
begin
  Result := False;
end;

procedure TSsh2Connection.OnSuccessRequest(mType: Byte; const Data: TBytes);
var
  PortForwardingEvent: TPortForwardingEvent;
begin
  PortForwardingEvent := TPortForwardingEvent(FRemotePortForwardingRequestList[0]);
  PortForwardingEvent.FSuccess := True;
  PortForwardingEvent.FEvent.SetEvent;
end;

procedure TSsh2Connection.OnFailureRequest(mType: Byte; const Data: TBytes);
var
  PortForwardingEvent: TPortForwardingEvent;
begin
  PortForwardingEvent := TPortForwardingEvent(FRemotePortForwardingRequestList[0]);
  PortForwardingEvent.FSuccess := False;
  PortForwardingEvent.FEvent.SetEvent;
end;

procedure TSsh2Connection.OnError(Sender: TObject; Error: Exception);
begin
  if Assigned(FParams.OnError) then
    FParams.OnError(Sender, Error);
end;

procedure TSsh2Connection.DoAfterConnect(Sender: TObject);
begin
  if Assigned(FParams.AfterConnect) then
    FParams.AfterConnect(Sender);
end;

procedure TSsh2Connection.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FParams.AfterDisconnect) then
    FParams.AfterDisconnect(Sender);
end;

procedure TSsh2Connection.OnDebugMessage(AlwaysDisplay: boolean; const msg: TBytes);
begin

end;

procedure TSsh2Connection.OnIgnoreMessage(const msg: TBytes);
begin

end;

procedure TSsh2Connection.OnUnknownMessage(mType: Byte; const Data: TBytes);
begin

end;

{ TKeyExchanger }

constructor TKeyExchanger.Create(con: TSsh2Connection; const SessionID: TBytes);
begin
  inherited Create;

  FCon := con;
  if SessionID <> nil then begin
    SetLength(FSessionID, Length(SessionID));
    Buffer.BlockCopy(SessionID, 0, FSessionID, 0, Length(SessionID));
  end;

  FDHGroupSizeMin := 1024;
  FDHGroupSizePreffered := 2048;
  FDHGroupSizeMax := 8192;

  FStatus := sINITIAL;
end;

destructor TKeyExchanger.Destroy;
begin
  FCon := nil;
  g.Free;
  x.Free;
  y.Free;
  e.Free;
  f.Free;
  FDHGroupPrime.Free;
  FExchSafePrime.Free;
  FECKey.Free;

  inherited;
end;

function TKeyExchanger.SyncKeyExchange: boolean;
begin
  FStartedByHost := False;

  SendInit(FClientInitPayload);
  ProcessInit(FCon.SyncReceivePacket, FServerInitPayload);

  case FKeyExchangeAlgorithm of
    keDHGroup1Sha1, keDHGroup14Sha1: begin
      SendDHInit;
      ProcessDHReply(FCon.SyncReceivePacket);
    end;
    keDHExchSha1, keDHExchSha256: begin
      SendDHERequest;
      ProcessDHEGroup(FCon.SyncReceivePacket);
      SendDHEInit;
      ProcessDHEReply(FCon.SyncReceivePacket);
    end;
    keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256: begin
      SendECDHInit;
      ProcessECDHReply(FCon.SyncReceivePacket);
    end;
  else
    Assert(False);
  end;

  SendNewKeys;
  ProcessNewKeys(FCon.SyncReceivePacket);
  Result := True;
end;

procedure TKeyExchanger.StartReexchange;
begin
  FStartedByHost := False;
  SendInit(FClientInitPayload);
end;

procedure TKeyExchanger.AsyncProcessPacket(Packet: TSsh2Packet);
begin
  case FStatus of
    sINITIAL: begin
      FStartedByHost := True;
      ProcessInit(Packet, FServerInitPayload);
      SendInit(FClientInitPayload);

      case FKeyExchangeAlgorithm of
        keDHGroup1Sha1, keDHGroup14Sha1:
          SendDHInit;
        keDHExchSha1, keDHExchSha256:
          SendDHERequest;
        keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256:
          SendECDHInit;
      else
        Assert(False);
      end;
    end;

    sWAIT_INIT: begin
      ProcessInit(Packet, FServerInitPayload);

      case FKeyExchangeAlgorithm of
        keDHGroup1Sha1, keDHGroup14Sha1:
          SendDHInit;
        keDHExchSha1, keDHExchSha256:
          SendDHERequest;
        keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256:
          SendECDHInit;
      else
        Assert(False);
      end;
    end;

    sWAIT_DH_REPLY: begin
      ProcessDHReply(Packet);
      SendNewKeys;
    end;

    sWAIT_DHE_GROUP: begin
      ProcessDHEGroup(Packet);
      SendDHEInit;
    end;

    sWAIT_DHE_REPLY: begin
      ProcessDHEReply(Packet);
      SendNewKeys;
    end;

    sWAIT_ECDH_REPLY: begin
      ProcessECDHReply(Packet);
      SendNewKeys;
    end;

    sWAIT_NEWKEYS: begin
      ProcessNewKeys(Packet);
      Assert(FStatus = sFINISHED);
    end;
  end;
end;

procedure TKeyExchanger.SendInit(out InitPayload: TBytes);
var
  wr: TSSH2DataStream;
  Cookie: TBytes;
  Alg: string;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_KEX_INIT);
    SetLength(Cookie, 16);
    FCon.Params.Random.Random(Cookie, 0, Length(Cookie));
    wr.WriteBuf(Cookie);

    if FCon.Params.KeyExchangeAlgorithms = '' then
      raise EScError.Create(seVerifyKeyExAlgFailed);
    wr.WriteAStr(FCon.Params.KeyExchangeAlgorithms); // kex_algorithms

    if FCon.Params.PreferableHostKeyAlgorithms = '' then
      raise EScError.Create(seVerifyHostKeyFailed);
    wr.WriteAStr(FCon.Params.PreferableHostKeyAlgorithms); // server_host_key_algorithms

    Assert(FCon.Params.ConnectionInfo <> nil);
    Alg := FCon.Params.ConnectionInfo.CiphersClient.AsString;
    if Alg = '' then
      raise EScError.Create(seVerifyEncAlgFailed);
    wr.WriteAStr(Alg); // encryption_algorithms_client_to_server

    Alg := FCon.Params.ConnectionInfo.CiphersServer.AsString;
    if Alg = '' then
      raise EScError.Create(seVerifyEncAlgFailed);
    wr.WriteAStr(Alg); // encryption_algorithms_server_to_client

    Alg := FCon.Params.ConnectionInfo.HMACAlgorithms.AsString;
    if Alg = '' then
      raise EScError.Create(seVerifyMacAlgFailed);
    wr.WriteAStr(Alg); // mac_algorithms_client_to_server
    wr.WriteAStr(Alg); // mac_algorithms_server_to_client

    if FCon.Params.CompressionClientAlgorithms = '' then
      raise EScError.Create(seVerifyCompressAlgFailed);
    wr.WriteAStr(FCon.Params.CompressionClientAlgorithms); // compression_algorithms_client_to_server

    if FCon.Params.CompressionServerAlgorithms = '' then
      raise EScError.Create(seVerifyCompressAlgFailed);
    wr.WriteAStr(FCon.Params.CompressionServerAlgorithms); // compression_algorithms_server_to_client

    wr.WriteAStr('');    // languages_client_to_server
    wr.WriteAStr('');    // languages_server_to_client
    wr.WriteBool(False); // Indicates whether a guessed key exchange packet follows
    wr.WriteInt32(0);    // reserved for future extension

    InitPayload := wr.ToBytes;
    FStatus := sWAIT_INIT;
    FCon.TransmitPacket(InitPayload, 0, Length(InitPayload));
  finally
    wr.Free;
  end;
end;

procedure TKeyExchanger.ProcessInit(Packet: TSsh2Packet; out InitPayload: TBytes);

  function DecideKeyExchangeAlgorithm(const Alg: string): TScKeyExchangeAlgorithm;
  begin
    Result := CipherFactory.SSH2NameToKeyExchangeAlgorithm(Alg);
  end;

  function DecideHostKeyAlgorithm(const Alg: string): TScAsymmetricAlgorithm;
  begin
    Result := CipherFactory.SSH2NameToPublicKeyAlgorithm(Alg);
  end;

  function DecideCipherAlgorithm(const MainAlg, SecondaryAlg: string): TScSymmetricAlgorithm;
  begin
    Result := CipherFactory.SSH2NameToCipherAlgorithm(DecideAlgorithm(MainAlg, SecondaryAlg, seVerifyEncAlgFailed));
  end;

  function DecideMacAlgorithm(const MainAlg, SecondaryAlg: string): TScHMACAlgorithm;
  begin
    Result := CipherFactory.SSH2NameToHMACAlgorithm(DecideAlgorithm(MainAlg, SecondaryAlg, seVerifyMacAlgFailed));
  end;

  function DecideCompressionAlgorithm(const MainAlg, SecondaryAlg: string): TScCompressionAlgorithm;
  begin
    Result := CipherFactory.SSH2NameToCompressionAlgorithm(DecideAlgorithm(MainAlg, SecondaryAlg, seVerifyCompressAlgFailed));
  end;

var
  re: TSSH2DataReader;
  Head: TBytes;
  str: string;
  SKeyExchangeAlgorithm, SHostKeyAlgorithm: string;
  Flag: boolean;
begin
  SetLength(InitPayload, Packet.DataLength);
  Buffer.BlockCopy(Packet.Data, 0, InitPayload, 0, Packet.DataLength);

  re := TSSH2DataReader.Create(InitPayload);
{$IFNDEF VER25P}
  Flag := False;
{$ENDIF}
  try
    Head := re.Read(17); // Type and cookie
    if Head[0] <> byte(SSH_MSG_KEX_INIT) then
      raise EScError.Create(seUnexpectedPacketType);

    Assert(FCon.Params.ConnectionInfo <> nil);
    str := Encoding.Default.GetString(re.ReadString); // kex_algorithms
    SKeyExchangeAlgorithm := DecideAlgorithm(FCon.Params.KeyExchangeAlgorithms, str, seInvalidKeyExchangeAlgorithm);
    FKeyExchangeAlgorithm := DecideKeyExchangeAlgorithm(SKeyExchangeAlgorithm);
    case FKeyExchangeAlgorithm of
      keDHGroup1Sha1:
        FDHGroupPrime := TBigInteger.Create(DH_PRIME_GROUP_1024);
      keDHGroup14Sha1:
        FDHGroupPrime := TBigInteger.Create(DH_PRIME_GROUP_2048);
      keDHExchSha1, keDHExchSha256:
        ;
      keECDHSha2Nistp256:
        FECName := secp256r1;
      keECDHSha2Nistp384:
        FECName := secp384r1;
      keECDHSha2Nistp521:
        FECName := secp521r1;
      keCurve25519Sha256:
        FECName := x25519;
    else
      raise EScError.Create(seInvalidKeyExchangeAlgorithm);
    end;

    str := Encoding.Default.GetString(re.ReadString); // Host_key
    SHostKeyAlgorithm := DecideAlgorithm(FCon.Params.PreferableHostKeyAlgorithms, str, seVerifyHostKeyFailed);
    TScSSHConnectionInfoUtils.SetHostKeyAlgorithmName(FCon.Params.ConnectionInfo, SHostKeyAlgorithm);
    TScSSHConnectionInfoUtils.SetHostKeyAlgorithm(FCon.Params.ConnectionInfo, DecideHostKeyAlgorithm(SHostKeyAlgorithm));
    FSignatureHashAlg := CipherFactory.SSH2NameToHashAlgorithm(SHostKeyAlgorithm);

    str := Encoding.Default.GetString(re.ReadString); // enc_cs
    TScSSHConnectionInfoUtils.SetCipherClient(FCon.Params.ConnectionInfo,
      DecideCipherAlgorithm(FCon.Params.ConnectionInfo.CiphersClient.AsString, str));

    str := Encoding.Default.GetString(re.ReadString); // enc_sc
    TScSSHConnectionInfoUtils.SetCipherServer(FCon.Params.ConnectionInfo,
      DecideCipherAlgorithm(FCon.Params.ConnectionInfo.CiphersServer.AsString, str));

    str := Encoding.Default.GetString(re.ReadString); // mac_cs
    TScSSHConnectionInfoUtils.SetHMACClient(FCon.Params.ConnectionInfo,
      DecideMacAlgorithm(FCon.Params.ConnectionInfo.HMACAlgorithms.AsString, str));

    str := Encoding.Default.GetString(re.ReadString); // mac_sc
    TScSSHConnectionInfoUtils.SetHMACServer(FCon.Params.ConnectionInfo,
      DecideMacAlgorithm(FCon.Params.ConnectionInfo.HMACAlgorithms.AsString, str));

    str := Encoding.Default.GetString(re.ReadString); // comp_cs
    TScSSHConnectionInfoUtils.SetCompressionClient(FCon.Params.ConnectionInfo,
      DecideCompressionAlgorithm(FCon.Params.CompressionClientAlgorithms, str));

    str := Encoding.Default.GetString(re.ReadString); // comp_sc
    TScSSHConnectionInfoUtils.SetCompressionServer(FCon.Params.ConnectionInfo,
      DecideCompressionAlgorithm(FCon.Params.CompressionServerAlgorithms, str));

    re.ReadString; /// str := Encoding.Default.GetString(re.ReadString); // lang_cs
    re.ReadString; /// str := Encoding.Default.GetString(re.ReadString); // lang_sc

    Flag := re.ReadBool;
    re.ReadInt32; // reserved
    Assert(re.Rest = 0);

  finally
    re.Free;
  end;

  if Flag then
    raise EScError.Create(seInvalidMessage);
end;

procedure TKeyExchanger.SendDHInit;
var
  sx: TBytes;
  wr: TSSH2DataStream;
begin
  // Round1 computes and sends [e]
  SetLength(sx, 64);
  FCon.Params.Random.Random(sx, 0, Length(sx));

  x := TBigInteger.Create(sx);
  g := TBigInteger.Create(2);
  e := g.ModPow(x, FDHGroupPrime);
  FillChar(sx[0], Length(sx), 0);

  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_KEX_DH_INIT);
    wr.WriteBI(e);
    FStatus := sWAIT_DH_REPLY;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TKeyExchanger.ProcessDHReply(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
  KeyAndCert, Signature: TBytes;
  k: TBigInteger;
begin
  SetLength(KeyAndCert, 0);
  SetLength(Signature, 0);

  k := nil;
  re := TSSH2DataReader.Create(Packet);
  try
    // Round2 receives Response
    h  := re.ReadPacketType;
    if h <> SSH_MSG_KEX_DH_REPLY then
      raise EScError.Create(seUnexpectedPacketType);

    KeyAndCert := re.ReadString;
    f := re.ReadAsBigInteger;
    if f.IsNegativeOrZero or f.GreaterOrEqual(FDHGroupPrime) then
      raise EScError.Create(seServerF_OutOfRange);

    Signature := re.ReadString;
    Assert(re.Rest = 0);

    k := f.ModPow(x, FDHGroupPrime);
    FPreKeyBuf := k.GetBytes;

    // Round3 calc Hash
    CalcDHKeyHash(KeyAndCert);

    if not VerifyHostKey(KeyAndCert, Signature, FHash) then
      raise EScError.Create(seHostKeyNotVerifed);

    if FSessionID = nil then
      FSessionID := FHash;

  finally
    re.Free;
    k.Free;
  end;
end;

procedure TKeyExchanger.SendDHERequest;
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_KEX_DH_GEX_REQUEST);
    wr.WriteInt32(FDHGroupSizeMin);
    wr.WriteInt32(FDHGroupSizePreffered);
    wr.WriteInt32(FDHGroupSizeMax);
    FStatus := sWAIT_DHE_GROUP;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TKeyExchanger.ProcessDHEGroup(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
begin
  re := TSSH2DataReader.Create(Packet);
  try
    // Round2 receives Response
    h  := re.ReadPacketType;
    if h <> SSH_MSG_KEX_DH_GEX_GROUP then
      raise EScError.Create(seUnexpectedPacketType);

    FExchSafePrime := re.ReadAsBigInteger;
    g := re.ReadAsBigInteger; // generator for subgroup in GF(p)
  finally
    re.Free;
  end;
end;

procedure TKeyExchanger.SendDHEInit;
var
  wr: TSSH2DataStream;
  sx: TBytes;
begin
  // C generates a random number x, where 1 < x < (p-1)/2.
  // It computes e = g^x mod p, and sends "e" to S.
  if FExchSafePrime.BitCount >= 2048 then
    SetLength(sx, 128)
  else
    SetLength(sx, 64);

  FCon.Params.Random.Random(sx, 0, Length(sx));
  x := TBigInteger.Create(sx);
  e := g.ModPow(x, FExchSafePrime);
  FillChar(sx[0], Length(sx), 0);

  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_KEX_DH_GEX_INIT);
    wr.WriteBI(e);
    FStatus := sWAIT_DHE_REPLY;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TKeyExchanger.ProcessDHEReply(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
  KeyAndCert, Signature: TBytes;
  k: TBigInteger;
begin
  SetLength(KeyAndCert, 0);
  SetLength(Signature, 0);

  k := nil;
  re := TSSH2DataReader.Create(Packet);
  try
    // Round2 receives Response
    h  := re.ReadPacketType;
    if h <> SSH_MSG_KEX_DH_GEX_REPLY then
      raise EScError.Create(seUnexpectedPacketType);

    KeyAndCert := re.ReadString;
    f := re.ReadAsBigInteger;
    if f.IsNegativeOrZero or f.GreaterOrEqual(FExchSafePrime) then
      raise EScError.Create(seServerF_OutOfRange);

    Signature := re.ReadString;
    Assert(re.Rest = 0);

    k := f.ModPow(x, FExchSafePrime);
    FPreKeyBuf := k.GetBytes;

    // Round3 calc Hash H
    CalcDHEKeyHash(KeyAndCert);

    if not VerifyHostKey(KeyAndCert, Signature, FHash) then
      raise EScError.Create(seHostKeyNotVerifed);

    if FSessionID = nil then
      FSessionID := FHash;

  finally
    re.Free;
    k.Free;
  end;
end;

procedure TKeyExchanger.SendECDHInit;
var
  wr: TSSH2DataStream;
begin
  FECKey := TScKey.Create(nil);
  FECKey.GenerateEC(FECName, FCon.Params.Random);
  FClientKeyBuf := FECKey.ECData.ECCryptography.EncodePointToOctetString(FECKey.ECData.PublicPoint);

  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_KEX_ECDH_INIT);
    wr.WriteAsString(FClientKeyBuf);
    FStatus := sWAIT_ECDH_REPLY;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TKeyExchanger.ProcessECDHReply(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
  KeyAndCert, Signature: TBytes;
  ServerPoint, SecretPoint: TScCustomECPoint;
  i: integer;
begin
  SetLength(KeyAndCert, 0);
  SetLength(Signature, 0);

  re := TSSH2DataReader.Create(Packet);
  try
    h  := re.ReadPacketType;
    if h <> SSH_MSG_KEX_ECDH_REPLY then
      raise EScError.Create(seUnexpectedPacketType);

    KeyAndCert := re.ReadString;
    FServerKeyBuf := re.ReadString;
    Signature := re.ReadString;
    Assert(re.Rest = 0);

    Assert(FECKey <> nil);
    SecretPoint := nil;
    ServerPoint := FECKey.ECData.ECCryptography.DecodePointFromOctetString(FServerKeyBuf, 0, Length(FServerKeyBuf));
    try
      SecretPoint := FECKey.ECData.ECCryptography.MulPoint(ServerPoint, FECKey.ECData.PrivateKoef);

      if SecretPoint is TScECPointBI then
        FPreKeyBuf := TScECPointBI(SecretPoint).X.GetBytes
      else
      if SecretPoint is TScECPointX25519 then begin
        SetLength(FPreKeyBuf, 32);
        Move(TScECPointX25519(SecretPoint).PointField[0], FPreKeyBuf[0], 32);
        // https://tools.ietf.org/html/draft-ietf-tls-rfc4492bis-17#section-5.11
        i := 0;
        while (i < 32) and (FPreKeyBuf[i] = 0) do
          Inc(i);
        if i = 32 then
          raise EScError.Create(seIllegalKeyShareParameters);
      end
      else
        Assert(False);
    finally
      ServerPoint.Free;
      SecretPoint.Free;
    end;

    CalcECDHKeyHash(KeyAndCert);

    if not VerifyHostKey(KeyAndCert, Signature, FHash) then
      raise EScError.Create(seHostKeyNotVerifed);

    if FSessionID = nil then
      FSessionID := FHash;

  finally
    re.Free;
  end;
end;

procedure TKeyExchanger.CalcECDHKeyHash(const KeyAndCert: TBytes);
var
  wr: TSSH2DataStream;
  csp: THashAlgorithm;
begin
  wr := TSSH2DataStream.Create;
  csp := CreateHashAlgorithm;
  try
    wr.WriteAStr(FCon.Params.ClientVersion);
    wr.WriteAStr(FCon.Params.ServerVersion);
    wr.WriteAsString(FClientInitPayload);
    wr.WriteAsString(FServerInitPayload);
    wr.WriteAsString(KeyAndCert);
    wr.WriteAsString(FClientKeyBuf);
    wr.WriteAsString(FServerKeyBuf);
    wr.WriteAsBigInteger(FPreKeyBuf);

    FHash := csp.ComputeHash(wr.ToBytes);
  finally
    wr.Free;
    csp.Free;
  end;
end;

procedure TKeyExchanger.CalcDHEKeyHash(const KeyAndCert: TBytes);
var
  wr: TSSH2DataStream;
  csp: THashAlgorithm;
begin
  wr := TSSH2DataStream.Create;
  csp := CreateHashAlgorithm;
  try
    wr.WriteAStr(FCon.Params.ClientVersion);
    wr.WriteAStr(FCon.Params.ServerVersion);
    wr.WriteAsString(FClientInitPayload);
    wr.WriteAsString(FServerInitPayload);
    wr.WriteAsString(KeyAndCert);
    wr.WriteInt32(FDHGroupSizeMin);
    wr.WriteInt32(FDHGroupSizePreffered);
    wr.WriteInt32(FDHGroupSizeMax);
    wr.WriteBI(FExchSafePrime);
    wr.WriteBI(g);
    wr.WriteBI(e);
    wr.WriteBI(f);
    wr.WriteAsBigInteger(FPreKeyBuf);

    FHash := csp.ComputeHash(wr.ToBytes);
  finally
    wr.Free;
    csp.Free;
  end;
end;

procedure TKeyExchanger.CalcDHKeyHash(const KeyAndCert: TBytes);
var
  wr: TSSH2DataStream;
  csp: THashAlgorithm;
begin
  wr := TSSH2DataStream.Create;
  csp := CreateHashAlgorithm;
  try
    wr.WriteAStr(FCon.Params.ClientVersion);
    wr.WriteAStr(FCon.Params.ServerVersion);
    wr.WriteAsString(FClientInitPayload);
    wr.WriteAsString(FServerInitPayload);
    wr.WriteAsString(KeyAndCert);
    wr.WriteBI(e);
    wr.WriteBI(f);
    wr.WriteAsBigInteger(FPreKeyBuf);

    FHash := csp.ComputeHash(wr.ToBytes);
  finally
    wr.Free;
    csp.Free;
  end;
end;

procedure TKeyExchanger.SendNewKeys;
var
  tmp: TBytes;
  Cipher: TSymmetricAlgorithm;
  Mac: IHashTransform;
  Compress: IScCompression;
begin
  FStatus := sWAIT_NEWKEYS;
  SetLength(tmp, 1);
  tmp[0] := byte(SSH_MSG_NEWKEYS);
  FCon.TransmitPacket(tmp, 0, 1);

  CreateEncAlg(Cipher, Mac, Compress);
  FCon.RefreshSendKeys(FSessionID, Cipher.CreateEncryptor, Mac, Compress);
end;

procedure TKeyExchanger.ProcessNewKeys(Packet: TSsh2Packet);
var
  Cipher: TSymmetricAlgorithm;
  Mac: IHashTransform;
  Compress: IScCompression;
begin
  // confirms new key
  if (Packet.DataLength <> 1) or (Packet.Data[0] <> byte(SSH_MSG_NEWKEYS)) then
    raise EScError.Create(seUnexpectedPacketType);

  CreateDecAlg(Cipher, Mac, Compress);
  FCon.RefreshRecvKeys(Cipher.CreateDecryptor, Mac, Compress);

  FStatus := sFINISHED;
end;

function TKeyExchanger.CreateHashAlgorithm: THashAlgorithm;
var
  B: integer;
begin
  if FKeyExchangeAlgorithm = keCurve25519Sha256 then
    Result := THash_SHA2_256.Create
  else
  if FKeyExchangeAlgorithm in [keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521] then begin
    B := EllipticCurvesParameters[FECName].Size shl 3; // * 8;

    if B > 384 then
      Result := THash_SHA2_512.Create
    else
    if B > 256 then
      Result := THash_SHA2_384.Create
    else
      Result := THash_SHA2_256.Create;
  end
  else
  if FKeyExchangeAlgorithm = keDHExchSha256 then
    Result := THash_SHA2_256.Create
  else
    Result := THash_SHA1.Create;
end;

procedure TKeyExchanger.CreateEncAlg(out Cipher: TSymmetricAlgorithm;
  out Mac: IHashTransform; out Compress: IScCompression);
begin
  Cipher := CipherFactory.CreateCipher({pSSH2, }FCon.Params.ConnectionInfo.CipherClient,
    DeriveKey(FPreKeyBuf, FHash, 'C', CipherFactory.GetKeySize(FCon.Params.ConnectionInfo.CipherClient)),
    DeriveKey(FPreKeyBuf, FHash, 'A', CipherFactory.GetBlockSize(FCon.Params.ConnectionInfo.CipherClient)));

  Mac := CipherFactory.CreateHMAC(FCon.Params.ConnectionInfo.HMACClient,
    DeriveKey(FPreKeyBuf, FHash, 'E', CipherFactory.GetHMACSize(FCon.Params.ConnectionInfo.HMACClient)));

  if FCon.Params.ConnectionInfo.CompressionClient in [csaZLib, csaZLibOpenSSH] then
    Compress := TScZCompression.Create
  else
    Compress := nil;
end;

procedure TKeyExchanger.CreateDecAlg(out Cipher: TSymmetricAlgorithm;
  out Mac: IHashTransform; out Compress: IScCompression);
begin
  Cipher := CipherFactory.CreateCipher({pSSH2, }FCon.Params.ConnectionInfo.CipherServer,
    DeriveKey(FPreKeyBuf, FHash, 'D', CipherFactory.GetKeySize(FCon.Params.ConnectionInfo.CipherServer)),
    DeriveKey(FPreKeyBuf, FHash, 'B', CipherFactory.GetBlockSize(FCon.Params.ConnectionInfo.CipherServer)));

  Mac := CipherFactory.CreateHMAC(FCon.Params.ConnectionInfo.HMACServer,
    DeriveKey(FPreKeyBuf, FHash, 'F', CipherFactory.GetHMACSize(FCon.Params.ConnectionInfo.HMACServer)));

  if FCon.Params.ConnectionInfo.CompressionServer in [csaZLib, csaZLibOpenSSH] then
    Compress := TScZCompression.Create
  else
    Compress := nil;
end;

function TKeyExchanger.VerifyHostKey(const KeyAndCert, Signature, Data: TBytes): boolean;

  function CheckWithExistHostKey(HostKey: TScKey): boolean;
  begin
    try
      if FCon.Params.ServerKey <> nil then begin
        FCon.Params.ServerKey.Ready := True;
        Result := HostKey.Equals(FCon.Params.ServerKey);
      end
      else
        Result := False;
    except
      Result := False;
    end;

    if not Result then
      if Assigned(FCon.Params.OnServerKeyValidation) then
        FCon.Params.OnServerKeyValidation(Self, HostKey, Result);
  end;

var
  re1, re2: TSSH2DataReader;
  Algorithm: string;
  Sigbody: TBytes;
  HostKey: TScKey;
  SHostKeyAlgorithm: string;
begin
{$IFNDEF VER25P}
  Result := False;
{$ENDIF}
  SetLength(Sigbody, 0);
  re1 := nil;
  re2 := nil;
  HostKey := nil;

  try
    re1 := TSSH2DataReader.Create(KeyAndCert);
    Algorithm := Encoding.Default.GetString(re1.ReadString);
    if CipherFactory.SSH2NameToPublicKeyAlgorithm(Algorithm) <> FCon.Params.ConnectionInfo.HostKeyAlgorithm then
      raise EScError.Create(seVerifyHostKeyFailed);

    re2 := TSSH2DataReader.Create(Signature);
    Algorithm := Encoding.Default.GetString(re2.ReadString);
    if Algorithm <> FCon.Params.ConnectionInfo.HostKeyAlgorithmName then begin
      if CipherFactory.SSH2NameToPublicKeyAlgorithm(Algorithm) <> FCon.Params.ConnectionInfo.HostKeyAlgorithm then
        raise EScError.Create(seVerifyHostKeyFailed);

      SHostKeyAlgorithm := DecideAlgorithm(FCon.Params.PreferableHostKeyAlgorithms, Algorithm, seVerifyHostKeyFailed);
      TScSSHConnectionInfoUtils.SetHostKeyAlgorithmName(FCon.Params.ConnectionInfo, SHostKeyAlgorithm);
      FSignatureHashAlg := CipherFactory.SSH2NameToHashAlgorithm(SHostKeyAlgorithm);
    end;

    Sigbody := re2.ReadString;
    Assert(re2.Rest = 0);
    HostKey := VerifyKeySignature(KeyAndCert, Data, Sigbody, FSignatureHashAlg);

    // ask the client whether he accepts the host key
    if not FStartedByHost and not CheckWithExistHostKey(HostKey) then
      Result := False
    else
      Result := True;

  finally
    re1.Free;
    re2.Free;
    HostKey.Free;
  end;
end;

class function TKeyExchanger.VerifyKeySignature(const PublicKey, Data, Signature: TBytes; SignatureHashAlg: TScHashAlgorithm): TScKey;
begin
  Result := TScKey.Create(nil);
  try
    TScKeyUtils.SetSSHKeyData(Result, PublicKey);

    if Result.Algorithm = aaEC then begin
      if Result.ECData.ECCryptography = nil then
        raise EScError.Create(seInternalError);

      Result.ECData.ECCryptography.IsSSHFormat := True;
    end;

    if SignatureHashAlg = haNone then
      SignatureHashAlg := CipherFactory.PublicKeyAlgorithmToHashAlgorithm(Result.Algorithm, Result.ECData.ECName);

    if not Result.VerifySign(Data, Signature, SignatureHashAlg) then
      raise EScError.Create(seHostKeyNotVerifed);
  except
    Result.Free;
    raise;
  end;
end;

function TKeyExchanger.DeriveKey(const PreKeyBuf, Hash: TBytes; ch: Char; Len: Integer): TBytes;
var
  wr: TSSH2DataStream;
  H: TBytes;
  csp: THashAlgorithm;
  Offset: integer;
begin
  SetLength(Result, Len);
  wr := TSSH2DataStream.Create;
  csp := CreateHashAlgorithm;

  try
    wr.WriteAsBigInteger(PreKeyBuf);
    wr.WriteBuf(Hash);
    wr.WriteByte(byte(AnsiChar(ch)));
    wr.WriteBuf(FSessionID);
    H := csp.ComputeHash(wr.ToBytes);

    if Len <= csp.HashSize then
      Buffer.BlockCopy(H, 0, Result, 0, Len)
    else begin
      Buffer.BlockCopy(H, 0, Result, 0, csp.HashSize);
      Offset := csp.HashSize;

      while Len >= Offset do begin
        wr.Clear;
        wr.WriteAsBigInteger(PreKeyBuf);
        wr.WriteBuf(Hash);
        wr.WriteBuf(TValueArr(Result), 0, Offset);
        H := csp.ComputeHash(wr.ToBytes);
        if Len > (Offset + csp.HashSize) then
          Buffer.BlockCopy(H, 0, Result, Offset, csp.HashSize)
        else
          Buffer.BlockCopy(H, 0, Result, Offset, Len - Offset);
        Inc(Offset, csp.HashSize);
      end;
    end;

  finally
    wr.Free;
    csp.Free;
  end;
end;

function TKeyExchanger.DecideAlgorithm(const MainAlg, SecondaryAlg: string; ErrorCode: TScErrorCode): string;
var
  Main, Second: TStringList;
  i: Integer;
begin
  Second := Split(SecondaryAlg, ',');
  Main := Split(MainAlg, ',');

  try
    for i := 0 to Main.Count - 1 do
      if Second.IndexOf(Main[i]) > -1 then begin
        Result := Main[i];
        Exit;
      end;
  finally
    Second.Free;
    Main.Free;
  end;

  raise EScError.CreateFmt(ScErrorMessages[ErrorCode] + ' (' + MainAlg + ' <-> ' + SecondaryAlg + ')', [], ErrorCode);
end;

initialization

finalization

end.

