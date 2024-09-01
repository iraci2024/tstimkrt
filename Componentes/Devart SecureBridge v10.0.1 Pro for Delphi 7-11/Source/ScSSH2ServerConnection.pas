
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSH2ServerConnection;

interface

uses
  SysUtils,
  ScCLRClasses, ScConsts, ScSSHConnectionParameter, ScTypes, ScUtils, ScSSHUtils,
  ScSSH2DataHandler, ScClient, ScSSH2Connection, ScCryptoTransformIntf,
  ScSymmetricAlgorithm, ScAlgorithmSupport, ScBridge, ScSSH2Channel;


type
  TSsh2ServerConnection = class(TSsh2Connection)
  protected
    procedure DoUserAuthentication; override;
    procedure UserAuthentication; override;
    procedure VersionNegotiation; override;
    procedure ServiceNegotiation; override;

    function CheckSessionRequest: boolean; override;
    function CheckPortForwardingRequest(IsRemote: boolean;
      const RemoteHost: string; const RemotePort: Integer;
      out DestHost: string; out DestPort: Integer): boolean; override;

  public
    constructor Create(AParams: TSshConnectionParameters); override;

    function ForwardPort(const RemoteHost: string; const RemotePort: Integer;
      const OriginatorHost: string = LOCAL_HOST; const OriginatorPort: Integer = 4000): TSsh2Channel; override;
  end;

  TServerKeyExchanger = class(TKeyExchanger)
  private
    procedure ProcessDHInit(Packet: TSsh2Packet);
    procedure SendDHReply;

    procedure ProcessDHERequest(Packet: TSsh2Packet);
    procedure SendDHEGroup;
    procedure ProcessDHEInit(Packet: TSsh2Packet);
    procedure SendDHEReply;

    procedure ProcessECDHInit(Packet: TSsh2Packet);
    procedure SendECDHReply;
  protected
    function DecideAlgorithm(const MainAlg, SecondaryAlg: string; ErrorCode: TScErrorCode): string; override;
    procedure CreateEncAlg(out Cipher: TSymmetricAlgorithm;
      out Mac: IHashTransform; out Compress: IScCompression); override;
    procedure CreateDecAlg(out Cipher: TSymmetricAlgorithm;
      out Mac: IHashTransform; out Compress: IScCompression); override;

  public
    function SyncKeyExchange: boolean; override;
    procedure StartReexchange; override;
    procedure AsyncProcessPacket(Packet: TSsh2Packet); override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  ScVio, ScHash, ScReaderWriter, ScBigInteger, ScSSHConsts, ScRNG, ScCipherSuites;

{ TSsh2ServerConnection }

constructor TSsh2ServerConnection.Create(AParams: TSshConnectionParameters);
begin
  inherited Create(AParams);

  FKeyExchangerClass := TServerKeyExchanger;
end;

procedure TSsh2ServerConnection.VersionNegotiation;
var
  Version: string;
begin
  if FParams.ServerVersion = '' then
    FParams.ServerVersion := TSshUtils.ServerVersionString(FParams.Protocol);

  SendVersion(FParams.ServerVersion);
  Version := ReadVersion();
  TScSSHConnectionInfoUtils.SetVersion(FParams.ConnectionInfo, Version);
  FParams.ClientVersion := Version;
end;

procedure TSsh2ServerConnection.ServiceNegotiation;
var
  wr: TSSH2DataStream;
  re: TSSH2DataReader;
  t: PacketTypeSSH2;
  ServiceName: string;
begin
  re := TSSH2DataReader.Create(SyncReceivePacket);
  try
    t := re.ReadPacketType;
    if t <> SSH_MSG_SERVICE_REQUEST then
      raise EScError.Create(seUnexpectedPacketType);

    ServiceName := Encoding.Default.GetString(re.ReadString);
    if ServiceName <> Sssh_userauth then
      raise EScError.Create(seUnexpectedPacket);
  finally
    re.Free;
  end;

  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_SERVICE_ACCEPT);
    wr.WriteAStr(Sssh_userauth);
    TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TSsh2ServerConnection.DoUserAuthentication;
begin
  UserAuthentication;
end;

procedure TSsh2ServerConnection.UserAuthentication;

var
  KeyIsVerified: boolean;

  function VerifyClientKey(const KeyAndCert: TBytes): boolean;
  var
    PublicKey: TScKey;
  begin
    PublicKey := TScKey.Create(nil);
    try
      TScKeyUtils.SetSSHKeyData(PublicKey, KeyAndCert);

      Result := False;
      if Assigned(FParams.OnCheckUserKey) then
        FParams.OnCheckUserKey(Self, FParams.ConnectionInfo, PublicKey, Result);
      if Result then
        KeyIsVerified := True;
    finally
      PublicKey.Free;
    end;
  end;

  function VerifyKeySignature(const UserName, ServiceName, Algorithm: string;
    const KeyAndCert, Signature: TBytes): boolean;
  var
    SignSource: TSSH2DataStream;
    re: TSSH2DataReader;
    s: string;
    SignBody: TBytes;
    Hash: TBytes;
    PublicKey: TScKey;
    SignatureHashAlg: TScHashAlgorithm;
  begin
    SignSource := TSSH2DataStream.Create;
    try
      SignSource.WriteAsString(FsessionID);
      SignSource.WritePacketType(SSH_MSG_USERAUTH_REQUEST);
      SignSource.WriteWStr(WideString(UserName));
      SignSource.WriteAStr(ServiceName);
      SignSource.WriteAStr(Spublickey);
      SignSource.WriteBool(True);
      SignSource.WriteAStr(Algorithm);
      SignSource.WriteAsString(KeyAndCert);
      Hash := SignSource.ToBytes;
    finally
      SignSource.Free;
    end;

    re := TSSH2DataReader.Create(KeyAndCert);
    try
      s := Encoding.Default.GetString(re.ReadString);
      if CipherFactory.SSH2NameToPublicKeyAlgorithm(s) <> CipherFactory.SSH2NameToPublicKeyAlgorithm(Algorithm) then begin
        Result := False;
        Exit;
      end;
    finally
      re.Free;
    end;

    SetLength(SignBody, 0);
    re := TSSH2DataReader.Create(Signature);
    try
      s := Encoding.Default.GetString(re.ReadString);
      if s <> Algorithm then begin
        Result := False;
        Exit;
      end;

      SignBody := re.ReadString;
      Assert(re.Rest = 0);
    finally
      re.Free;
    end;

    SignatureHashAlg := CipherFactory.SSH2NameToHashAlgorithm(Algorithm);
    PublicKey := TKeyExchanger.VerifyKeySignature(KeyAndCert, Hash, SignBody, SignatureHashAlg);
    try
      Result := KeyIsVerified;
      if not KeyIsVerified and Assigned(FParams.OnCheckUserKey) then begin
        FParams.OnCheckUserKey(Self, FParams.ConnectionInfo, PublicKey, Result);
        KeyIsVerified := Result;
      end;
    finally
      PublicKey.Free;
    end;
  end;

  function SendAuthenticationResponse: TAuthenticationResult;
    function SendMsgFailure(Write: TSSH2DataStream; Timeout: integer = 0): TAuthenticationResult;
    begin
      if Timeout > 0 then
        Sleep(Timeout);
        
      Write.WritePacketType(SSH_MSG_USERAUTH_FAILURE);
      Write.WriteAStr(Spassword + ',' + Spublickey);
      Write.WriteBool(False);
      TransmitPacket(Write.Data, 0, Write.DataLength);
      Result := arFailure;
    end;

    function SendMsgSuccess(Write: TSSH2DataStream): TAuthenticationResult;
    begin
      Write.WritePacketType(SSH_MSG_USERAUTH_SUCCESS);
      TransmitPacket(Write.Data, 0, Write.DataLength);
      Result := arSuccess;

      if FTmpClientCompression <> nil then begin
        FCompression := FTmpClientCompression;
        FTmpClientCompression := nil;
      end;
      if FTmpServerCompression <> nil then begin
        FDataHandler.SetCompression(FTmpServerCompression);
        FTmpServerCompression := nil;
      end;
    end;

    function SendMsgPublicKeyOk(Write: TSSH2DataStream; const Alg: string; const Key: TBytes): TAuthenticationResult;
    begin
      Write.WritePacketType(SSH_MSG_USERAUTH_PK_OK);
      Write.WriteAStr(Alg);
      Write.WriteAsString(Key);
      TransmitPacket(Write.Data, 0, Write.DataLength);
      Result := arFailure;
    end;

    function ReadKeyboardInteractiveAuthResponse: string;
    var
      Response: TSSH2DataReader;
      h: PacketTypeSSH2;
    begin
      Response := TSSH2DataReader.Create(SyncReceivePacket);
      try
        h := Response.ReadPacketType;
        if h <> SSH_MSG_USERAUTH_INFO_RESPONSE then
          raise EScError.Create(seUnexpectedPacketType);

        if Response.ReadInt32 = 1 then // num Responses
          Result := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString))
        else
          Result := '';
      finally
        Response.Free;
      end;
    end;

  var
    Response: TSSH2DataReader;
    wr: TSSH2DataStream;
    h: PacketTypeSSH2;
    ServiceName, Method, Algorithm: string;
    Flag: boolean;
    KeyAndCert, Signature: TBytes;
    Accept: boolean;
  begin
    SetLength(KeyAndCert, 0);
    SetLength(Signature, 0);
    Response := TSSH2DataReader.Create(SyncReceivePacket);

    wr := TSSH2DataStream.Create;
    try
    {$IFNDEF VER25P}
      Result := arFailure;
    {$ENDIF}
      h := Response.ReadPacketType;
      if h = SSH_MSG_DISCONNECT then begin
        Result := arDisconnected;
        Exit;
      end
      else
      if h <> SSH_MSG_USERAUTH_REQUEST then
        raise EScError.Create(seUnexpectedPacketType);

      FParams.UserName := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString));
      ServiceName := Encoding.Default.GetString(Response.ReadString);
      Method := Encoding.Default.GetString(Response.ReadString);
      TScSSHConnectionInfoUtils.SetUser(FParams.ConnectionInfo, FParams.UserName);

      if ServiceName <> Sssh_connection then begin
        Result := SendMsgFailure(wr);
        Exit;
      end;

      if Method = SNone then begin
        if Length(FParams.Banner) > 0 then begin
          wr.WritePacketType(SSH_MSG_USERAUTH_BANNER);
          wr.WriteWStr(WideString(FParams.Banner));
          wr.WriteAStr('');
          TransmitPacket(wr.Data, 0, wr.DataLength);
          wr.Clear;
        end;

        Result := SendMsgFailure(wr);
      end
      else
      if (Method = Skeyboard_interactive) and
         (atKeyboardInteractive in FParams.ServerAuthenticationsType) then begin
        wr.WritePacketType(SSH_MSG_USERAUTH_INFO_REQUEST);
        wr.WriteAStr(SPassword_authentication); // name
        wr.WriteAStr(''); // instruction
        wr.WriteAStr(''); // lang
        wr.WriteInt32(1); // num-prompts
        wr.WriteAStr(SPasswordPrompt); // prompts[0]
        wr.WriteBool(False); // echo
        TransmitPacket(wr.Data, 0, wr.DataLength);
        wr.Clear;

        FParams.Password := ReadKeyboardInteractiveAuthResponse;
        Accept := False;
        if Assigned(FParams.OnCheckUserPass) then
          FParams.OnCheckUserPass(Self, FParams.ConnectionInfo, FParams.Password, Accept);

        if Accept then
          Result := SendMsgSuccess(wr)
        else
          Result := SendMsgFailure(wr);
      end
      else
      if (Method = Spassword) and
         (atPassword in FParams.ServerAuthenticationsType) then begin
        Flag := Response.ReadBool;
        FParams.Password := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Response.ReadString));

        Accept := False;
        if Assigned(FParams.OnCheckUserPass) then
          FParams.OnCheckUserPass(Self, FParams.ConnectionInfo, FParams.Password, Accept);

        if not Flag and Accept then
          Result := SendMsgSuccess(wr)
        else
          Result := SendMsgFailure(wr, 1000);
      end
      else
      if (Method = Spublickey) and
         (atPublicKey in FParams.ServerAuthenticationsType) then begin
        Flag := Response.ReadBool;

        Algorithm := Encoding.Default.GetString(Response.ReadString);
        KeyAndCert := Response.ReadString;

        if not Flag then begin
          if VerifyClientKey(KeyAndCert) then
            Result := SendMsgPublicKeyOk(wr, Algorithm, KeyAndCert)
          else
            Result := SendMsgFailure(wr);
        end
        else begin
          Signature := Response.ReadString;
          Assert(Response.Rest = 0);

          if VerifyKeySignature(FParams.UserName, ServiceName, Algorithm, KeyAndCert, Signature) then
            Result := SendMsgSuccess(wr)
          else
            Result := SendMsgFailure(wr, 1000);
        end;
      end
      else
        Result := SendMsgFailure(wr);

    finally
      Response.Free;
      wr.Free;
    end;
  end;

var
  Count: Integer;
  Res: TAuthenticationResult;
begin
  KeyIsVerified := False;
  Count := 0;
  repeat
    Res := SendAuthenticationResponse;
    Inc(Count);
  until (Res <> arFailure) or (Count >= 4);

  if Res <> arSuccess then
    raise EScError.CreateFmt(SAuthenticationFailed, [], seAuthenticationFailed);
end;

function TSsh2ServerConnection.ForwardPort(const RemoteHost: string; const RemotePort: Integer;
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
      wr.WriteAStr(Sforwarded_tcpip);
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

function TSsh2ServerConnection.CheckSessionRequest: boolean;
begin
  Result := True;
end;

function TSsh2ServerConnection.CheckPortForwardingRequest(IsRemote: boolean;
  const RemoteHost: string; const RemotePort: Integer;
  out DestHost: string; out DestPort: Integer): boolean;
begin
  DestHost := RemoteHost;
  DestPort := RemotePort;
  Result := not IsRemote and (cpAllowLocalForwarding in FParams.ConnectionInfo.SSHChannelPermissions);
end;

{ TServerKeyExchanger }

function TServerKeyExchanger.SyncKeyExchange: boolean;
begin
  try
    SendInit(FServerInitPayload);
    ProcessInit(FCon.SyncReceivePacket, FClientInitPayload);

    case FKeyExchangeAlgorithm of
      keDHGroup1Sha1, keDHGroup14Sha1: begin
        ProcessDHInit(FCon.SyncReceivePacket);
        SendDHReply;
      end;
      keDHExchSha1, keDHExchSha256: begin
        ProcessDHERequest(FCon.SyncReceivePacket);
        SendDHEGroup;
        ProcessDHEInit(FCon.SyncReceivePacket);
        SendDHEReply;
      end;
      keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256: begin
        ProcessECDHInit(FCon.SyncReceivePacket);
        SendECDHReply;
      end;
    else
      Assert(False);
    end;

    SendNewKeys;
    ProcessNewKeys(FCon.SyncReceivePacket);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TServerKeyExchanger.StartReexchange;
begin
  SendInit(FServerInitPayload);
end;

procedure TServerKeyExchanger.AsyncProcessPacket(Packet: TSsh2Packet);
begin
  case FStatus of
    sINITIAL: begin
      ProcessInit(Packet, FClientInitPayload);
      SendInit(FServerInitPayload);

      case FKeyExchangeAlgorithm of
        keDHGroup1Sha1, keDHGroup14Sha1:
          FStatus := sWAIT_DH_REPLY;
        keDHExchSha1, keDHExchSha256:
          FStatus := sWAIT_DHE_GROUP;
        keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256:
          FStatus := sWAIT_ECDH_REPLY;
      else
        Assert(False);
      end;
    end;

    sWAIT_INIT: begin
      ProcessInit(Packet, FClientInitPayload);

      case FKeyExchangeAlgorithm of
        keDHGroup1Sha1, keDHGroup14Sha1:
          FStatus := sWAIT_DH_REPLY;
        keDHExchSha1, keDHExchSha256:
          FStatus := sWAIT_DHE_GROUP;
        keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256:
          FStatus := sWAIT_ECDH_REPLY;
      else
        Assert(False);
      end;
    end;

    sWAIT_DH_REPLY: begin
      ProcessDHInit(Packet);
      SendDHReply;
    end;

    sWAIT_DHE_GROUP: begin
      ProcessDHERequest(Packet);
      SendDHEGroup;
    end;

    sWAIT_DHE_REPLY: begin
      ProcessDHEInit(Packet);
      SendDHEReply;
    end;

    sWAIT_ECDH_REPLY: begin
      ProcessECDHInit(Packet);
      SendECDHReply;
    end;

    sWAIT_NEWKEYS: begin
      SendNewKeys;
      ProcessNewKeys(Packet);
    end;
  end;
end;

procedure TServerKeyExchanger.ProcessDHInit(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
begin
  re := TSSH2DataReader.Create(Packet.Data, Packet.DataLength);
  try
    h := re.ReadPacketType;
    if h <> SSH_MSG_KEX_DH_INIT then
      raise EScError.Create(seUnexpectedPacketType);

    e := re.ReadAsBigInteger;
    if e.IsNegativeOrZero or e.GreaterOrEqual(FDHGroupPrime) then
      raise EScError.Create(seClientE_OutOfRange);

    Assert(re.Rest = 0);
  finally
    re.Free;
  end;
end;

procedure TServerKeyExchanger.SendDHReply;
var
  wr: TSSH2DataStream;
  SignPack: TSSH2DataStream;
  sy: TBytes;
  k, t: TBigInteger;
  HostKey: TScKey;
  KeyAndCert: TBytes;
begin
  SetLength(sy, 64);
  FCon.Params.Random.Random(sy, 0, Length(sy));
  y := TBigInteger.Create(sy);
  FillChar(sy[0], Length(sy), 0);

  k := nil;
  t := TBigInteger.Create(2);
  try
    k := e.ModPow(y, FDHGroupPrime);
    FPreKeyBuf := k.GetBytes;
    f := t.ModPow(y, FDHGroupPrime);
  finally
    k.Free;
    t.Free;
  end;

  SignPack := nil;
  SetLength(KeyAndCert, 0);
  wr := TSSH2DataStream.Create;
  try
    HostKey := FCon.Params.HostKeys[FCon.Params.ConnectionInfo.HostKeyAlgorithm];
    if HostKey = nil then
      raise EScError.Create(seHostKeysNotFound);

    HostKey.Ready := True;
    if HostKey.Algorithm <> FCon.Params.ConnectionInfo.HostKeyAlgorithm then
      raise EScError.Create(seHostKeysNotFound);

    wr.WritePacketType(SSH_MSG_KEX_DH_REPLY);
    KeyAndCert := TScKeyUtils.GetSSHKeyData(HostKey);
    wr.WriteAsString(KeyAndCert);
    wr.WriteBI(f);

    CalcDHKeyHash(KeyAndCert);

    SignPack := TSSH2DataStream.Create;
    SignPack.WriteAStr(FCon.Params.ConnectionInfo.HostKeyAlgorithmName);
    SignPack.WriteAsString(HostKey.Sign(FHash, FSignatureHashAlg));
    wr.WriteAsString(SignPack.ToBytes);

    FStatus := sWAIT_NEWKEYS;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);

    if FSessionID = nil then
      FSessionID := FHash;

  finally
    SignPack.Free;
    wr.Free;
  end;
end;

procedure TServerKeyExchanger.ProcessDHERequest(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
begin
  re := TSSH2DataReader.Create(Packet.Data, Packet.DataLength);
  try
    h := re.ReadPacketType;
    if h <> SSH_MSG_KEX_DH_GEX_REQUEST then
      raise EScError.Create(seUnexpectedPacketType);

    FDHGroupSizeMin := re.ReadInt32;
    FDHGroupSizePreffered := re.ReadInt32;
    FDHGroupSizeMax := re.ReadInt32;
    Assert(re.Rest = 0);
  finally
    re.Free;
  end;
end;

procedure TServerKeyExchanger.SendDHEGroup;
var
  UserSafe, UserGenerator: TBigInteger;
  wr: TSSH2DataStream;
begin
  // S finds a group that best matches the client's request, and sends "p || g" to C.

  UserSafe := nil;
  UserGenerator := nil;
  if Assigned(FCon.Params.OnGetDHEGroup) then
    FCon.Params.OnGetDHEGroup(Self, FDHGroupSizeMin, FDHGroupSizePreffered, FDHGroupSizeMax, FCon.Params.Random, UserSafe, UserGenerator);

  if (UserSafe <> nil) and (UserGenerator <> nil) then begin
    FExchSafePrime := UserSafe;
    g := UserGenerator;
  end
  else begin
    if (FDHGroupSizePreffered > 6144) and (FDHGroupSizeMax >= 8192) then
      FExchSafePrime := TBigInteger.Create(DH_PRIME_GROUP_8192)
    else
    if (FDHGroupSizePreffered > 4096) and (FDHGroupSizeMax >= 6144) then
      FExchSafePrime := TBigInteger.Create(DH_PRIME_GROUP_6144)
    else
    if (FDHGroupSizePreffered > 3072) and (FDHGroupSizeMax >= 4096) then
      FExchSafePrime := TBigInteger.Create(DH_PRIME_GROUP_4096)
    else
    if (FDHGroupSizePreffered > 2048) and (FDHGroupSizeMax >= 3072) then
      FExchSafePrime := TBigInteger.Create(DH_PRIME_GROUP_3072)
    else
      FExchSafePrime := TBigInteger.Create(DH_PRIME_GROUP_2048);

    g := TBigInteger.Create(2);
  end;

  wr := TSSH2DataStream.Create;
  try
    wr.WritePacketType(SSH_MSG_KEX_DH_GEX_GROUP);
    wr.WriteBI(FExchSafePrime);
    wr.WriteBI(g);

    FStatus := sWAIT_DHE_REPLY;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);
  finally
    wr.Free;
  end;
end;

procedure TServerKeyExchanger.ProcessDHEInit(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
begin
  re := TSSH2DataReader.Create(Packet.Data, Packet.DataLength);
  try
    h := re.ReadPacketType;
    if h <> SSH_MSG_KEX_DH_GEX_INIT then
      raise EScError.Create(seUnexpectedPacketType);

    e := re.ReadAsBigInteger;
    if e.IsNegativeOrZero or e.GreaterOrEqual(FExchSafePrime) then
      raise EScError.Create(seClientE_OutOfRange);
      
    Assert(re.Rest = 0);
  finally
    re.Free;
  end;
end;

procedure TServerKeyExchanger.SendDHEReply;
var
  wr: TSSH2DataStream;
  SignPack: TSSH2DataStream;
  sy: TBytes;
  k: TBigInteger;
  HostKey: TScKey;
  KeyAndCert: TBytes;
begin
  // S generates a random number y, where 0 < y < (p-1)/2
  if FExchSafePrime.BitCount >= 2048 then
    SetLength(sy, 128)
  else
    SetLength(sy, 64);
  FCon.Params.Random.Random(sy, 0, Length(sy));
  y := TBigInteger.Create(sy);
  FillChar(sy[0], Length(sy), 0);

  // It computes K = e^y mod p,
  k := e.ModPow(y, FExchSafePrime);
  try
    FPreKeyBuf := k.GetBytes;
  finally
    k.Free;
  end;

  // It computes f = g^y mod p.
  f := g.ModPow(y, FExchSafePrime);

  SignPack := nil;
  SetLength(KeyAndCert, 0);
  wr := TSSH2DataStream.Create;
  try
    HostKey := FCon.Params.HostKeys[FCon.Params.ConnectionInfo.HostKeyAlgorithm];
    if HostKey = nil then
      raise EScError.Create(seHostKeysNotFound);

    HostKey.Ready := True;
    if HostKey.Algorithm <> FCon.Params.ConnectionInfo.HostKeyAlgorithm then
      raise EScError.Create(seHostKeysNotFound);

    wr.WritePacketType(SSH_MSG_KEX_DH_GEX_REPLY);
    KeyAndCert := TScKeyUtils.GetSSHKeyData(HostKey);
    wr.WriteAsString(KeyAndCert);
    wr.WriteBI(f);

    CalcDHEKeyHash(KeyAndCert);

    SignPack := TSSH2DataStream.Create;
    SignPack.WriteAStr(FCon.Params.ConnectionInfo.HostKeyAlgorithmName);
    SignPack.WriteAsString(HostKey.Sign(FHash, FSignatureHashAlg));
    wr.WriteAsString(SignPack.ToBytes);

    FStatus := sWAIT_NEWKEYS;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);

    if FSessionID = nil then
      FSessionID := FHash;

  finally
    SignPack.Free;
    wr.Free;
  end;
end;

procedure TServerKeyExchanger.ProcessECDHInit(Packet: TSsh2Packet);
var
  re: TSSH2DataReader;
  h: PacketTypeSSH2;
begin
  re := TSSH2DataReader.Create(Packet.Data, Packet.DataLength);
  try
    h := re.ReadPacketType;
    if h <> SSH_MSG_KEX_ECDH_INIT then
      raise EScError.Create(seUnexpectedPacketType);

    FClientKeyBuf := re.ReadString;
    Assert(re.Rest = 0);
  finally
    re.Free;
  end;
end;

procedure TServerKeyExchanger.SendECDHReply;
var
  ClientPoint, SecretPoint: TScCustomECPoint;
  wr: TSSH2DataStream;
  SignPack: TSSH2DataStream;
  HostKey: TScKey;
  KeyAndCert: TBytes;
  i: integer;
begin
  FECKey := TScKey.Create(nil);
  FECKey.GenerateEC(FECName, FCon.Params.Random);
  FServerKeyBuf := FECKey.ECData.ECCryptography.EncodePointToOctetString(FECKey.ECData.PublicPoint);

  SecretPoint := nil;
  ClientPoint := FECKey.ECData.ECCryptography.DecodePointFromOctetString(FClientKeyBuf, 0, Length(FClientKeyBuf));
  try
    SecretPoint := FECKey.ECData.ECCryptography.MulPoint(ClientPoint, FECKey.ECData.PrivateKoef);

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
    ClientPoint.Free;
    SecretPoint.Free;
  end;

  SignPack := nil;
  SetLength(KeyAndCert, 0);
  wr := TSSH2DataStream.Create;
  try
    HostKey := FCon.Params.HostKeys[FCon.Params.ConnectionInfo.HostKeyAlgorithm];
    if HostKey = nil then
      raise EScError.Create(seHostKeysNotFound);

    HostKey.Ready := True;
    if HostKey.Algorithm <> FCon.Params.ConnectionInfo.HostKeyAlgorithm then
      raise EScError.Create(seHostKeysNotFound);

    if HostKey.Algorithm = aaEC then begin
      if HostKey.ECData.ECCryptography = nil then
        raise EScError.Create(seInternalError);

      HostKey.ECData.ECCryptography.IsSSHFormat := True;
    end;

    wr.WritePacketType(SSH_MSG_KEX_ECDH_REPLY);
    KeyAndCert := TScKeyUtils.GetSSHKeyData(HostKey);

    wr.WriteAsString(KeyAndCert);
    wr.WriteAsString(FServerKeyBuf);

    CalcECDHKeyHash(KeyAndCert);

    SignPack := TSSH2DataStream.Create;
    SignPack.WriteAStr(FCon.Params.ConnectionInfo.HostKeyAlgorithmName);
    SignPack.WriteAsString(HostKey.Sign(FHash, FSignatureHashAlg));
    wr.WriteAsString(SignPack.ToBytes);

    FStatus := sWAIT_NEWKEYS;
    FCon.TransmitPacket(wr.Data, 0, wr.DataLength);

    if FSessionID = nil then
      FSessionID := FHash;

  finally
    SignPack.Free;
    wr.Free;
  end;
end;

procedure TServerKeyExchanger.CreateEncAlg(out Cipher: TSymmetricAlgorithm;
  out Mac: IHashTransform; out Compress: IScCompression);
begin
  inherited CreateDecAlg(Cipher, Mac, Compress);
end;

procedure TServerKeyExchanger.CreateDecAlg(out Cipher: TSymmetricAlgorithm;
  out Mac: IHashTransform; out Compress: IScCompression);
begin
  inherited CreateEncAlg(Cipher, Mac, Compress);
end;

function TServerKeyExchanger.DecideAlgorithm(const MainAlg, SecondaryAlg: string; ErrorCode: TScErrorCode): string;
begin
  Result := inherited DecideAlgorithm(SecondaryAlg, MainAlg, ErrorCode);
end;

initialization

finalization

end.

