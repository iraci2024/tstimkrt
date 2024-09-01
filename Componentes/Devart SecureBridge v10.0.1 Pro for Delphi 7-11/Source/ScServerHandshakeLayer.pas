
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScServerHandshakeLayer;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLTypes, TdsCipherSuites, TdsBridge, TdsCertificateExts,
  TdsSSLMessages, TdsLayers, TdsSSLExtensions;
{$ELSE}
  TdsUtilsUni, TdsSSLTypesUni, TdsCipherSuitesUni, TdsBridgeUni, TdsCertificateExtsUni,
  TdsSSLMessagesUni, TdsLayersUni, TdsSSLExtensionsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions,
  ScUtils, ScSSLTypes, ScCipherSuites, ScBridge, ScCertificateExts,
  ScSSLMessages, ScLayers, ScSSLExtensions;
{$ENDIF}

type
(*
  Client                     Server

  ClientHello       -------->
                                ServerHello
                                Certificate*
                                ServerKeyExchange*
                                CertificateRequest*
                    <--------   ServerHelloDone
  Certificate*
  ClientKeyExchange
  CertificateVerify*
  [ChangeCipherSpec]
  Finished          -------->
                                [ChangeCipherSpec]
                    <--------   Finished
  Application Data  <------->   Application Data
*)

  TServerHandshakeLayer = class(THandshakeLayer)
  private
    FNeedHelloRetryRequest: boolean;
    FPreMasterSecret: TBytes;
    FClientProtocol: TScSSLProtocol;
    FDHKey: TScDHData;
    FECKey: TScKey;
    FNewSessionInfo: TScSSLSessionInfo;
    FHasSignatureAlgorithmsExtension: boolean;
    FHasSupportedGroupsExtension: boolean;
    FHasKeyShareExtension: boolean;

    function SelectCipherSuite(const CipherAlgorithms: TScSSLCipherAlgorithmArray): TScSSLCipherAlgorithm;
    function SelectCompression(const Compressions: TScCompressionArray): TScCompression;
    procedure CheckNewSessionTicket(ClientHelloMessage: THandshakeMessage; NewSessionInfo: TScSSLSessionInfo);
    procedure CheckKeyShares;
    procedure ProcessKeyShares;
  protected
    procedure Init; override;
    procedure ClearKeyExchangeInfo; override;

    procedure InternalProcessMessage(Message: THandshakeMessage); override;
    procedure ReplyToMessage(HandshakeType: TScHandshakeType); override;

    procedure ProcessClientHello(Message: THandshakeMessage);
    procedure ProcessClientKeyExchange(Message: THandshakeMessage);
    procedure ProcessFinished(Message: THandshakeMessage);
    procedure ResponseFinished;
    procedure ResponseClientHello;
    procedure MakeServerHelloMessage;
    procedure MakeEncryptedExtensionMessage;
    procedure MakeServerKeyExchangeMessage;
    procedure MakeCertificateRequestMessage;
    procedure MakeServerHelloDoneMessage;
    procedure MakeMasterSecret;
    procedure MakeHelloRequestMessage; override;
    procedure MakeNewSessionTicketMessage;

  public
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF VER17P}
{$IFNDEF NEXTGEN}
  Contnrs, Types,
{$ENDIF}
{$ENDIF}
{$IFNDEF SBRIDGE}
  CRDECUtil, CRBigInteger, CRHashAlgorithm, CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsAlgorithmSupport, TdsMD5SHA1CSP, TdsCertificateConsts;
{$ELSE}
  TdsSSLConstsUni, TdsAlgorithmSupportUni, TdsMD5SHA1CSPUni, TdsCertificateConstsUni;
{$ENDIF}
{$ELSE}
  ScDECUtil, ScBigInteger, ScHashAlgorithm, ScSymmetricAlgorithm,
  ScConsts, ScAlgorithmSupport, ScMD5SHA1CSP, ScCertificateConsts;
{$ENDIF}

{ TServerHandshakeLayer }

destructor TServerHandshakeLayer.Destroy;
begin
  FNewSessionInfo.Free;

  inherited;
end;

procedure TServerHandshakeLayer.Init;
begin
  inherited;

  FNeedHelloRetryRequest := False;

  if FNewSessionInfo = nil then
    FNewSessionInfo := TScSSLSessionInfo.Create;
end;

procedure TServerHandshakeLayer.ClearKeyExchangeInfo;
begin
  inherited;

  FreeAndNil(FECKey);
  FreeAndNil(FDHKey.P);
  FreeAndNil(FDHKey.G);
  FreeAndNil(FDHKey.Y);
  FreeAndNil(FDHKey.X);
end;

procedure TServerHandshakeLayer.InternalProcessMessage(Message: THandshakeMessage);
begin
  case Message.HandshakeType of
    htClientHello: begin
      if GetProtocol = spTls13 then begin
        if FNeedHelloRetryRequest then begin
          if FState <> htClientHello then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htNothing then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else begin
        if (FState <> htNothing) and (FState <> htFinished) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

        if not FOptions.ClientInitiatedRenegotiationIsAllowed and FFinished then
          Exit;
      end;

      ProcessClientHello(Message);
    end;

    htCertificate: begin
      if FState <> htClientHello then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessCertificate(Message);
    end;

    htCertificateVerify: begin
      if GetProtocol = spTls13 then begin
        if FState <> htCertificate then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if FState <> htClientKeyExchange then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessCertificateVerify(Message);
    end;

    htClientKeyExchange: begin
      if GetProtocol = spTls13 then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType)
      else begin
        if FOptions.IsClientCertificateRequired and not FIsSessionResumption then begin
          if FState <> htCertificate then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htClientHello then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end;

      ProcessClientKeyExchange(Message);
    end;

    htFinished: begin
      if GetProtocol = spTls13 then begin
        if FOptions.IsClientCertificateRequired and not FIsSessionResumption then begin
          if (FState <> htCertificate) and (FState <> htCertificateVerify) then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htClientHello then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if FState <> htChangeCipherSpec then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessFinished(Message);
    end;

    htKeyUpdate: begin
      if GetProtocol = spTls13 then begin
        if not FFinished then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessKeyUpdate(Message);
    end;
  else
    SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
  end;
end;

procedure TServerHandshakeLayer.ReplyToMessage(HandshakeType: TScHandshakeType);
begin
  case HandshakeType of
    htClientHello:
      ResponseClientHello;

    htClientKeyExchange:
      MakeMasterSecret;

    htFinished:
      ResponseFinished;

    htKeyUpdate: begin
      if FLastKeyUpdateRequest = kurUpdateRequested then
        MakeKeyUpdateMessage(kurUpdateNotRequested);
    end;
  end;
end;

procedure TServerHandshakeLayer.ProcessClientHello(Message: THandshakeMessage);
var
  ClientVersion: TScSSLVersionRec;
  TmpProtocol: TScSSLProtocol;
  SupportedProtocols: TScSSLProtocols;
  LengthSID, BufLen: integer;
  ClientCipherAlgorithms: TScSSLCipherAlgorithmArray;
  ClientCompressions: TScCompressionArray;
  ClientHelloExtension: TTLSHelloExtension;
  RenegIndicatExtensionC, RenegIndicatExtensionS: TTLSRenegotiationIndicationExtension;
  SupportedVersionsExtensionS: TTLSSupportedVersionsExtension;
  MaxFragmentLengthExtensionC, MaxFragmentLengthExtensionS: TTLSMaxFragmentLengthExtension;
  RecordSizeLimitExtensionC, RecordSizeLimitExtensionS: TTLSRecordSizeLimitExtension;
  CipherAlgorithm: TScSSLCipherAlgorithm;
  Compression: TScCompression;
  Cancel: boolean;
  i: integer;
begin
  if (FOptions.ClientHelloExtensions = nil) or (FOptions.ServerHelloExtensions = nil) then
    raise EScError.Create(seInvalidInputArgs);

  IsNegotiating := True;
  FIsRenegotiationStarted := False;
  FFinished := False;

  ClientVersion.Major := Message.ReadInt8;
  ClientVersion.Minor := Message.ReadInt8;
  FClientProtocol := TScSSLVersionHelper.GetProtocol(ClientVersion);
  if ClientVersion.Major < 3 then
    SendAlert(adProtocolVersion, seNotAgreeOnProtocol);

  // the time from the client [1 cardinal] and the random bytes [28 bytes]
  Message.ReadTo(FClientServerRandom, 0, 32);

  // the session ID [0..32 bytes]
  LengthSID := Message.ReadInt8;
  if LengthSID > 32 then
    SendAlert(adDecodeError, seInvalidMessage);
  FSessionInfo.SessionID := Message.Read(LengthSID);
  // If SessionID will be used for session resuming then it needs to check all settings:
  // protocol version, cipher algorithm, compression, TTLSServerNameExtension.ServerNames,
  // and UseExtendedMasterSecret must be True.
  // Instead, it proceeds with a full handshake to establish a new session.

  // cipher suite
  BufLen := Message.ReadInt16;
  if BufLen = 0 then
    SendAlert(adDecodeError, seInvalidMessage);

  SetLength(ClientCipherAlgorithms, BufLen div 2);
  for i := 0 to Length(ClientCipherAlgorithms) - 1 do
    ClientCipherAlgorithms[i] := TCipherSuites.ToCipherAlgorithm(Message.ReadInt16);

  // compression method
  BufLen := Message.ReadInt8;
  if (BufLen = 0) or (BufLen > 3) then
    SendAlert(adDecodeError, seInvalidMessage);

  SetLength(ClientCompressions, BufLen);
  for i := 0 to Length(ClientCompressions) - 1 do
    ClientCompressions[i] := TScCompressionHelper.ToCompression(Message.ReadInt8);

  // Client extensions
  FUseExtendedMasterSecret := False;
  FHasSignatureAlgorithmsExtension := False;
  FHasSupportedGroupsExtension := False;
  FHasKeyShareExtension := False;
  RenegIndicatExtensionC := nil;
  RenegIndicatExtensionS := TTLSRenegotiationIndicationExtension(FOptions.ServerHelloExtensions.Find(TTLSRenegotiationIndicationExtension));

  FRecordLayer.MaxRecordLength := MAX_RECORD_LENGTH; // default

  FOptions.ClientHelloExtensions.Clear;
  if Message.RestCount > 0 then begin
    try
      FOptions.ClientHelloExtensions.Parse(Message, ceServer);
    except
      on E: EScError do begin
        if E.ErrorCode = seInvalidMessage then
          SendAlert(adDecodeError, seInvalidMessage)
        else
          SendAlert(adIllegalParameter, E.ErrorCode);
      end
      else
        SendAlert(adDecodeError, seInvalidMessage);
    end;

    MaxFragmentLengthExtensionC := nil;
    RecordSizeLimitExtensionC := nil;

    for i := 0 to FOptions.ClientHelloExtensions.Count - 1 do begin
      ClientHelloExtension := FOptions.ClientHelloExtensions.Extensions[i];

      if ClientHelloExtension.ClassType = TTLSSupportedVersionsExtension then begin
        // Select protocol
        SupportedProtocols := TTLSSupportedVersionsExtension(ClientHelloExtension).Versions * FOptions.Protocols;
        if SupportedProtocols = [] then
          SendAlert(adProtocolVersion, seNotAgreeOnProtocol);

        FClientProtocol := TScSSLVersionHelper.GetMaxProtocol(SupportedProtocols);
      end
      else
      if ClientHelloExtension.ClassType = TTLSExtendedMasterSecretExtension then begin
        if FOptions.ExtendedMasterSecretMode <> emsmDisable then
          FUseExtendedMasterSecret := True;
      end
      else
      if ClientHelloExtension.ClassType = TTLSMaxFragmentLengthExtension then begin
        if RecordSizeLimitExtensionC <> nil then
          continue;

        MaxFragmentLengthExtensionC := TTLSMaxFragmentLengthExtension(ClientHelloExtension);
        FRecordLayer.MaxRecordLength := MaxFragmentLengthExtensionC.MaxFragmentLengthInByte;

        MaxFragmentLengthExtensionS := TTLSMaxFragmentLengthExtension(FOptions.ServerHelloExtensions.FindOrCreate(TTLSMaxFragmentLengthExtension));
        MaxFragmentLengthExtensionS.MaxFragmentLength := MaxFragmentLengthExtensionC.MaxFragmentLength;
      end
      else
      if ClientHelloExtension.ClassType = TTLSRecordSizeLimitExtension then begin
        if MaxFragmentLengthExtensionC <> nil then
          FOptions.ServerHelloExtensions.RemoveIfExists(TTLSMaxFragmentLengthExtension);

        RecordSizeLimitExtensionC := TTLSRecordSizeLimitExtension(ClientHelloExtension);
        FRecordLayer.MaxRecordLength := RecordSizeLimitExtensionC.RecordSizeLimit;

        if FOptions.RecordSizeLimit > 0 then begin
          RecordSizeLimitExtensionS := TTLSRecordSizeLimitExtension(FOptions.ServerHelloExtensions.FindOrCreate(TTLSRecordSizeLimitExtension));
          RecordSizeLimitExtensionS.RecordSizeLimit := FOptions.RecordSizeLimit;
        end;
      end
      else
      if ClientHelloExtension.ClassType = TTLSRenegotiationIndicationExtension then
        RenegIndicatExtensionC := TTLSRenegotiationIndicationExtension(ClientHelloExtension)
      else
      if ClientHelloExtension.ClassType = TTLSSignatureAlgorithmsExtension then
        FHasSignatureAlgorithmsExtension := True
      else
      if ClientHelloExtension.ClassType = TTLSSupportedGroupsExtension then
        FHasSupportedGroupsExtension := True
      else
      if ClientHelloExtension.ClassType = TTLSKeyShareExtension then
        FHasKeyShareExtension := True
      else
      if ClientHelloExtension.ClassType = TTLSPreSharedKeyExtension then begin
        if i < FOptions.ClientHelloExtensions.Count - 1 then
          SendAlert(adIllegalParameter, seInvalidMessage);
      end;
    end;
  end;

  CheckAndChangeProtocol(TScSSLVersionHelper.GetMaxSupportedProtocol(FOptions.Protocols, FClientProtocol));

  FIsSessionResumption := False;
  if not FInitialized then
    CheckNewSessionTicket(Message, FNewSessionInfo);

  if FIsSessionResumption and
     TScSSLVersionHelper.CheckMaxSupportedProtocol([FNewSessionInfo.Protocol], FClientProtocol, TmpProtocol) then
    CheckAndChangeProtocol(TmpProtocol);

  if GetProtocol = spTls13 then begin
    SupportedVersionsExtensionS := TTLSSupportedVersionsExtension(FOptions.ServerHelloExtensions.FindOrCreate(TTLSSupportedVersionsExtension));
    SupportedVersionsExtensionS.SelectedVersion := FClientProtocol;

    if not (FHasSignatureAlgorithmsExtension and FHasSupportedGroupsExtension and FHasKeyShareExtension) then
      SendAlert(adMissingExtension, seMissingExtension);

    FUseExtendedMasterSecret := False;
  end
  else begin
    if RenegIndicatExtensionC <> nil then begin
      // 1st flight
      if RenegIndicatExtensionS = nil then begin
        RenegIndicatExtensionS := TTLSRenegotiationIndicationExtension.Create;
        FOptions.ServerHelloExtensions.Add(RenegIndicatExtensionS);
      end
      else
        // 2st flight
        RenegIndicatExtensionS.Renegotiate;

      RenegIndicatExtensionS.Check(RenegIndicatExtensionC);
    end
    else
      if (RenegIndicatExtensionS <> nil) or FInitialized then
        // 2nd flight and client doesn't send TTLSRenegotiationIndicationExtension
        SendAlert(adMissingExtension, seRenegotiationDenied);

    if GetProtocol = spSsl3 then
      FUseExtendedMasterSecret := False
    else begin
      if (FOptions.ExtendedMasterSecretMode = emsmRequire) and not FUseExtendedMasterSecret then
        SendAlert(adHandshakeFailure, seExtendedMasterSecretModeRequired);

      if FUseExtendedMasterSecret then
        FOptions.ServerHelloExtensions.FindOrCreate(TTLSExtendedMasterSecretExtension);
    end;

    if not FUseExtendedMasterSecret then
      FIsSessionResumption := False;
  end;

  // select cipher suite
  CipherAlgorithm := SelectCipherSuite(ClientCipherAlgorithms);
  // select compression
  Compression := SelectCompression(ClientCompressions);

  if FInitialized then begin
    // when Renegotiation or after Hello Retry request
    if FSessionInfo.CipherAlgorithm <> CipherAlgorithm then
      SendAlert(adHandshakeFailure, seVerifyEncAlgFailed);
    if FSessionInfo.Compression <> Compression then
      SendAlert(adHandshakeFailure, seVerifyCompressAlgFailed);
    if FSessionInfo.UseExtendedMasterSecret <> FUseExtendedMasterSecret then
      SendAlert(adHandshakeFailure, seExtendedMasterSecretModeRequired);
  end
  else begin
    FSessionInfo.CipherAlgorithm := CipherAlgorithm;
    FSessionInfo.Compression := Compression;
    FSessionInfo.UseExtendedMasterSecret := FUseExtendedMasterSecret;
  end;

  if Assigned(FOptions.AfterClientHello) then begin
    Cancel := False;
    FOptions.AfterClientHello(Self, Cancel); // to check extensions
    if Cancel then
      SendAlert(adHandshakeFailure, seSSLNegotiationCommandFailed);
  end;
end;

function TServerHandshakeLayer.SelectCipherSuite(
  const CipherAlgorithms: TScSSLCipherAlgorithmArray): TScSSLCipherAlgorithm;
var
  cd: TCipherDefinition;
  i, j: integer;
begin
  Result := caNone;

  if FIsSessionResumption then begin
    for i := 0 to Length(CipherAlgorithms) - 1 do begin
      if CipherAlgorithms[i] = FNewSessionInfo.CipherAlgorithm then
        Result := FNewSessionInfo.CipherAlgorithm;
        break;
    end;

    if Result = caNone then
      FIsSessionResumption := False;
  end;

  if Result = caNone then begin
    for i := 0 to Length(CipherAlgorithms) - 1 do begin
      cd := SSLCipherDefinitions[CipherAlgorithms[i]];
      if not (GetProtocol in cd.SupportedProtocols) then
        continue;

      for j := 0 to Length(FOptions.CipherAlgorithms) - 1 do begin
        if CipherAlgorithms[i] = FOptions.CipherAlgorithms[j] then begin
          Result := CipherAlgorithms[i];
          break;
        end;
      end;

      if Result <> caNone then
        break;
    end;
  end;

  if Result = caNone then
    SendAlert(adHandshakeFailure, seVerifyEncAlgFailed);

  if (Result in ECDHEAlgorithms) and not FHasSupportedGroupsExtension then
    SendAlert(adMissingExtension, seMissingExtension);
end;

function TServerHandshakeLayer.SelectCompression(
  const Compressions: TScCompressionArray): TScCompression;
var
  i: integer;
begin
  Result := csAllowed;

  case FOptions.Compression of
    csNone: begin
      for i := 0 to Length(Compressions) - 1 do begin
        if Compressions[i] = csNone then begin
          Result := csNone;
          break;
        end;
      end;
    end;
    csRequired: begin
      for i := 0 to Length(Compressions) - 1 do begin
        if Compressions[i] = csRequired then begin
          Result := csRequired;
          break;
        end;
      end;
    end
  else
    Assert(False);
  end;

  if FOptions.Compression <> Result then
    SendAlert(adHandshakeFailure, seVerifyCompressAlgFailed);

  if FIsSessionResumption then begin
    if ((FNewSessionInfo.Compression = csRequired) and (Result = csNone)) or
       ((FNewSessionInfo.Compression = csNone) and (Result = csRequired)) then
      FIsSessionResumption := False
    else
      Result := FNewSessionInfo.Compression;
  end;

  if (GetProtocol = spTls13) and (Result <> csNone) then
    SendAlert(adIllegalParameter, seVerifyCompressAlgFailed);
end;

procedure TServerHandshakeLayer.CheckNewSessionTicket(ClientHelloMessage: THandshakeMessage;
  NewSessionInfo: TScSSLSessionInfo);
var
  HelloExtension: TTLSHelloExtension;
  PreSharedKeyExtensionS: TTLSPreSharedKeyExtension;
  PskKeyExchangeModesExtensionC: TTLSPskKeyExchangeModesExtension;
  SelectedIdentity: integer;
  SessionTicketExtensionC: TTLSSessionTicketExtension;
begin
  FIsSessionResumption := False;

  if GetProtocol = spTls13 then begin
    HelloExtension := FOptions.ClientHelloExtensions.Extensions[FOptions.ClientHelloExtensions.Count - 1];
    if not (HelloExtension is TTLSPreSharedKeyExtension) then
      Exit;

    PskKeyExchangeModesExtensionC := TTLSPskKeyExchangeModesExtension(FOptions.ClientHelloExtensions.Find(TTLSPskKeyExchangeModesExtension));
    if (PskKeyExchangeModesExtensionC <> nil) and not (kemPskWithECDHE in PskKeyExchangeModesExtensionC.Modes) then
      Exit;

    FService.DecodeAndVerifyTicket(ClientHelloMessage, FOptions.ClientHelloExtensions, NewSessionInfo, SelectedIdentity);

    if SelectedIdentity >= 0 then begin
      PreSharedKeyExtensionS := TTLSPreSharedKeyExtension(FOptions.ServerHelloExtensions.FindOrCreate(TTLSPreSharedKeyExtension));
      PreSharedKeyExtensionS.SelectedIdentity := SelectedIdentity;
      FIsSessionResumption := True;
    end;
  end
  else begin
    SessionTicketExtensionC := TTLSSessionTicketExtension(FOptions.ClientHelloExtensions.Find(TTLSSessionTicketExtension));
    if SessionTicketExtensionC <> nil then begin
      if FOptions.NewSessionTicketDistributedCount > 0 then
        FOptions.ServerHelloExtensions.FindOrCreate(TTLSSessionTicketExtension);

      FService.DecodeAndVerifyTicket(ClientHelloMessage, FOptions.ClientHelloExtensions, NewSessionInfo, SelectedIdentity);
      if SelectedIdentity >= 0 then
        FIsSessionResumption := True;
    end;
  end;
end;

procedure TServerHandshakeLayer.MakeServerHelloMessage;
var
  Version: TScSSLVersionRec;
  SelectedCipherBuf, SelectedCompressionBuf: TBytes;
  ServerTime: TBytes;
begin
  if FOptions.ServerHelloExtensions = nil then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(SelectedCipherBuf, 2);
  TCipherSuites.CipherAlgorithmToBuf(FSessionInfo.CipherAlgorithm, SelectedCipherBuf, 0);
  SelectedCompressionBuf := TScCompressionHelper.CompressionToBuf(FSessionInfo.Compression);

  // When presenting a ticket, the client MAY generate and include a
  // Session ID in the TLS ClientHello. If the server accepts the ticket
  // and the Session ID is not empty, then it MUST respond with the same
  // Session ID present in the ClientHello. This allows the client to
  // easily differentiate when the server is resuming a session from when
  // it is falling back to a full handshake.
  if not FIsSessionResumption and (GetProtocol <> spTls13) then
    FSessionInfo.SessionID := nil;

  FSendMessage.Init(htServerHello);
  Version := TScSSLVersionHelper.GetVersion(GetProtocol);
  FSendMessage.WriteInt8(Version.Major);
  FSendMessage.WriteInt8(Version.Minor);

  if FNeedHelloRetryRequest then
    Move(HelloRetryRequestID[0], FClientServerRandom[32], 32)
  else begin
    SetLength(ServerTime, 0);
    ServerTime := GetUnixTimeBuf;
    Move(ServerTime[0], FClientServerRandom[32], 4);
    FRandom.Random(FClientServerRandom, 32+4, 28);

    if ((spTls12 in FOptions.Protocols) or (spTls13 in FOptions.Protocols)) and (GetProtocol in [spSsl3, spTls1, spTls11]) then
      Move(TLS11Protection[0], FClientServerRandom[32+24], 8)
    else
    if (spTls13 in FOptions.Protocols) and (GetProtocol = spTls12) then
      Move(TLS12Protection[0], FClientServerRandom[32+24], 8);
  end;

  FSendMessage.WriteBuf0(FClientServerRandom, 32, 32);
  FSendMessage.WriteBuf8(FSessionInfo.SessionID);
  FSendMessage.WriteBuf0(SelectedCipherBuf);
  FSendMessage.WriteBuf0(SelectedCompressionBuf);
  FOptions.ServerHelloExtensions.Encode(FSendMessage, ceServer, GetProtocol);

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);

  FInitialized := True;
end;

procedure TServerHandshakeLayer.ResponseClientHello;
begin
  if GetProtocol = spTls13 then begin
    CheckKeyShares;

    if not FInitialized and not FIsSessionResumption then begin
      FService.CreateClientHello1Hash;
      FService.MakeEarlySecret(nil);
      if FNeedHelloRetryRequest then
        FService.TranscriptClientHello1Hash;
    end;

    MakeServerHelloMessage;

    // https://tools.ietf.org/html/rfc8446#appendix-D.4
    MakeChangeCipherSpec;

    if FNeedHelloRetryRequest then
      Exit;

    ProcessKeyShares;

    MakeEncryptedExtensionMessage;

    if not FIsSessionResumption then begin
      if FOptions.IsClientCertificateRequired then
        MakeCertificateRequestMessage;

      if Assigned(FOptions.OnCertificateRequest) then
        FOptions.OnCertificateRequest(nil, FOwnCertificateList);

      MakeCertificateMessage(FOwnCertificateList);
      if FOwnCertificateList.Count > 0 then
        MakeCertificateVerifyMessage(TScCertificate(FOwnCertificateList[0]));
    end;

    MakeFinishedMessage;
    FService.MakeApplicationTrafficSecret;
    InitializeCiphers(cpLocal);
  end
  else begin
    if not FOptions.ClientInitiatedRenegotiationIsAllowed and FFinished then
      Exit;

    MakeServerHelloMessage;

    if not FIsSessionResumption then begin
      if Assigned(FOptions.OnCertificateRequest) then
        FOptions.OnCertificateRequest(nil, FOwnCertificateList);

      MakeCertificateMessage(FOwnCertificateList);

      MakeServerKeyExchangeMessage;
      if FOptions.IsClientCertificateRequired then
        MakeCertificateRequestMessage;

      MakeServerHelloDoneMessage;
    end
    else begin
      MakeNewSessionTicketMessage;
      MakeChangeCipherSpec;
      MakeFinishedMessage;
    end;
  end;
end;

procedure TServerHandshakeLayer.CheckKeyShares;
var
  SupportedGroupsExtensionC: TTLSSupportedGroupsExtension;
  KeyShareExtensionC, KeyShareExtensionS: TTLSKeyShareExtension;
  KExNamedGroup: TScKExNamedGroupType;
  IsFound: boolean;
  i: integer;
begin
  SupportedGroupsExtensionC := TTLSSupportedGroupsExtension(FOptions.ClientHelloExtensions.Find(TTLSSupportedGroupsExtension));
  KeyShareExtensionC := TTLSKeyShareExtension(FOptions.ClientHelloExtensions.Find(TTLSKeyShareExtension));
  if (SupportedGroupsExtensionC = nil) or (KeyShareExtensionC = nil) then
    SendAlert(adMissingExtension, seVerifyKeyExAlgFailed);

  IsFound := False;
  KExNamedGroup := kex_x25519;

  for i := 0 to KeyShareExtensionC.Count - 1 do begin
    if KeyShareExtensionC.KExNamedGroups[i] in FOptions.SupportedKExNamedGroups then begin
      KExNamedGroup := KeyShareExtensionC.KExNamedGroups[i];
      IsFound := True;
      break;
    end;
  end;

  if not IsFound then begin
    if FNeedHelloRetryRequest then
      SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);

    FNeedHelloRetryRequest := True;

    for i := 0 to SupportedGroupsExtensionC.Count - 1 do begin
      if SupportedGroupsExtensionC.KExNamedGroups[i] in FOptions.SupportedKExNamedGroups then begin
        KExNamedGroup := SupportedGroupsExtensionC.KExNamedGroups[i];
        IsFound := True;
        break;
      end;
    end;
  end
  else
    FNeedHelloRetryRequest := False;

  if not IsFound then
    SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);

  KeyShareExtensionS := TTLSKeyShareExtension(FOptions.ServerHelloExtensions.FindOrCreate(TTLSKeyShareExtension));
  if KeyShareExtensionS.Count = 0 then
    KeyShareExtensionS.Add(KExNamedGroup)
  else
  if KeyShareExtensionS.KExNamedGroups[0] <> KExNamedGroup then
    SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);

  KeyShareExtensionS.OnlyRequest := FNeedHelloRetryRequest;
end;

procedure TServerHandshakeLayer.ProcessKeyShares;
var
  KeyShareExtensionC, KeyShareExtensionS: TTLSKeyShareExtension;
  KExNamedGroup: TScKExNamedGroupType;
  FFGType: TScFiniteFieldGroupType;
  ECKey: TScKey;
  ClientPoint, SecretPoint: TScCustomECPoint;
  Xs, Yc, K: TBigInteger;
  PreMasterSecret: TBytes;
  P: TBigInteger;
  No, i: integer;
begin
  KeyShareExtensionC := TTLSKeyShareExtension(FOptions.ClientHelloExtensions.Find(TTLSKeyShareExtension));
  KeyShareExtensionS := TTLSKeyShareExtension(FOptions.ServerHelloExtensions.Find(TTLSKeyShareExtension));

  if (KeyShareExtensionC = nil) or (KeyShareExtensionS = nil) or (KeyShareExtensionS.Count <> 1) then
    SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

  KExNamedGroup := KeyShareExtensionS.KExNamedGroups[0];
  No := 0;
  while No < KeyShareExtensionC.Count do begin
    if KeyShareExtensionC.KExNamedGroups[No] = KExNamedGroup then
      Break;
    Inc(No);
  end;

  if No = KeyShareExtensionC.Count then
    SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

  if integer(KExNamedGroup) <= integer(High(TScECName)) then begin
    ClientPoint := KeyShareExtensionC.PublicECPoints[No];
    ECKey := KeyShareExtensionS.PrivateECKeys[0];
    if (ECKey = nil) or (ClientPoint = nil) then
      SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

    SecretPoint := ECKey.ECData.ECCryptography.MulPoint(ClientPoint, ECKey.ECData.PrivateKoef);
    try
      if SecretPoint is TScECPointBI then
        PreMasterSecret := TScECPointBI(SecretPoint).X.GetBytes(ECKey.ECData.ECCryptography.Size)
      else
      if SecretPoint is TScECPointX25519 then begin
        SetLength(PreMasterSecret, 32);
        Move(TScECPointX25519(SecretPoint).PointField[0], PreMasterSecret[0], 32);
        i := 0;
        while (i < 32) and (PreMasterSecret[i] = 0) do
          Inc(i);
        if i = 32 then
          SendAlert(adIllegalParameter, seIllegalKeyShareParameters);
      end
      else
        Assert(False);

    finally
      SecretPoint.Free;
    end;
  end
  else begin
    FFGType := TScFiniteFieldGroupType(integer(KExNamedGroup) - integer(High(TScECName)) - 1);
    Yc := KeyShareExtensionC.PublicYs[No];
    Xs := KeyShareExtensionS.PrivateXs[0];

    if (Xs = nil) or (Yc = nil) then
      SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

    P := FFDHE_PRIME(FFGType);
    // https://tools.ietf.org/html/rfc8446#section-4.2.8.1
    if (Yc.BitCount <= 1) or Yc.GreaterOrEqual(P) then
      SendAlert(adIllegalParameter, seWrongExtensionData);

    K := Yc.ModPow(Xs, P);
    try
      // https://tools.ietf.org/html/rfc8446#section-7.4.1
      PreMasterSecret := K.GetBytes(P.BitCount shr 3);
    finally
      K.Free;
    end;
  end;

  FService.MakeHandshakeTrafficSecret(PreMasterSecret);
  FillChar(PreMasterSecret[0], Length(PreMasterSecret), 0);

  InitializeCiphers(cpBoth);
end;

procedure TServerHandshakeLayer.MakeEncryptedExtensionMessage;
begin
  FSendMessage.Init(htEncryptedExtensions);
  FOptions.ServerHelloExtensions.Encode(FSendMessage, ceServer, GetProtocol);

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure TServerHandshakeLayer.MakeServerKeyExchangeMessage;
var
  SupportedGroupsExtensionC: TTLSSupportedGroupsExtension;
  KExNamedGroup: TScKExNamedGroupType;
  Ys: TBigInteger;
  RandomBuf, PublicPointBuf: TBytes;
  OwnCertificate: TScCertificate;
  i: integer;
begin
  SupportedGroupsExtensionC := TTLSSupportedGroupsExtension(FOptions.ClientHelloExtensions.Find(TTLSSupportedGroupsExtension));

  FSendMessage.Init(htServerKeyExchange);

  if FSessionInfo.CipherAlgorithm in DHEAlgorithms then begin
    KExNamedGroup := kex_ffDHE2048;
    if SupportedGroupsExtensionC <> nil then begin
      for i := 0 to SupportedGroupsExtensionC.Count - 1 do begin
        // is FiniteFieldGroupType
        if integer(SupportedGroupsExtensionC.KExNamedGroups[i]) > integer(High(TScECName)) then begin
          KExNamedGroup := SupportedGroupsExtensionC.KExNamedGroups[i];
          break;
        end;
      end;
    end;

    FreeAndNil(FDHKey.P);
    FreeAndNil(FDHKey.G);

    case KExNamedGroup of
      kex_ffDHE2048:
        FDHKey.P := TBigInteger.Create(DH_PRIME_GROUP_2048);
      kex_ffDHE3072:
        FDHKey.P := TBigInteger.Create(DH_PRIME_GROUP_3072);
      kex_ffDHE4096:
        FDHKey.P := TBigInteger.Create(DH_PRIME_GROUP_4096);
      kex_ffDHE6144:
        FDHKey.P := TBigInteger.Create(DH_PRIME_GROUP_6144);
      kex_ffDHE8192:
        FDHKey.P := TBigInteger.Create(DH_PRIME_GROUP_8192);
    else
      FDHKey.P := TBigInteger.Create(DH_PRIME_GROUP_2048);
    end;

    FDHKey.G := TBigInteger.Create(2);

    Ys := nil;
    try
      if FDHKey.P.BitCount >= 2048 then
        SetLength(RandomBuf, 128)
      else
        SetLength(RandomBuf, 64);

      FRandom.Random(RandomBuf, 0, Length(RandomBuf));
      if RandomBuf[0] > $7F then
        RandomBuf[0] := RandomBuf[0] div 2;
      if RandomBuf[0] = 0 then
        RandomBuf[0] := 1;

      FreeAndNil(FDHKey.X);
      FDHKey.X := TBigInteger.Create(RandomBuf);
      Ys := FDHKey.G.ModPow(FDHKey.X, FDHKey.P);

      FSendMessage.WriteBuf16(FDHKey.P.GetBytes);
      FSendMessage.WriteBuf16(FDHKey.G.GetBytes);
      FSendMessage.WriteBuf16(Ys.GetBytes);
    finally
      FillChar(RandomBuf[0], Length(RandomBuf), 0);
      Ys.Free;
    end;
  end
  else
  if FSessionInfo.CipherAlgorithm in ECDHEAlgorithms then begin
    KExNamedGroup := kex_x25519;
    if SupportedGroupsExtensionC <> nil then begin
      for i := 0 to SupportedGroupsExtensionC.Count - 1 do begin
        if integer(SupportedGroupsExtensionC.KExNamedGroups[i]) <= integer(High(TScECName)) then begin
          KExNamedGroup := SupportedGroupsExtensionC.KExNamedGroups[i];
          break;
        end;
      end;
    end;

    FSendMessage.WriteInt8(ELLIPTIC_CURVE_DOMAIN_CODES[ecdtNamedCurve]);
    FSendMessage.WriteInt16(KEX_NAMED_GROUP_CODES[KExNamedGroup]);

    FreeAndNil(FECKey);
    FECKey := TScKey.Create(nil);
    FECKey.GenerateEC(TScECName(byte(KExNamedGroup)));

    SetLength(PublicPointBuf, 0);
    PublicPointBuf := FECKey.ECData.ECCryptography.EncodePointToOctetString(FECKey.ECData.PublicPoint);
    FSendMessage.WriteBuf8(PublicPointBuf);
  end
  else begin
    OwnCertificate := GetOwnCertificate;
    if (OwnCertificate = nil) or not OwnCertificate.Key.IsPrivate then
      SendAlert(adHandshakeFailure, seCertificateMustBePrivate);

    Exit; // Don't send the ServerKeyExchange message
  end;

  FService.WriteServerKeyExchangeSign(FSendMessage, GetOwnCertificate);

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure TServerHandshakeLayer.ProcessClientKeyExchange(Message: THandshakeMessage);
var
  Yc, K: TBigInteger;
  PByteCount: integer;
  Size: integer;
  MaxVersion: TScSSLVersionRec;
  RemoteKey: TScKey;
  EncPreMaster: TBytes;
  ClientPoint, SecretPoint: TScCustomECPoint;
  OwnCertificate: TScCertificate;
  i: integer;
begin
  if FSessionInfo.CipherAlgorithm in DHEAlgorithms then begin
    Yc := nil;
    K := nil;
    try
      Size := Message.ReadInt16;
      Yc := TBigInteger.Create(Message.Fragment, Message.ReadOffset, Size);
      Message.SkipRead(Size);

      K := Yc.ModPow(FDHKey.X, FDHKey.P);
      FPreMasterSecret := K.GetBytes;

      PByteCount := (FDHKey.P.BitCount + 7) shr 3;
      if Length(FPreMasterSecret) < PByteCount then
        SendAlert(adIllegalParameter, seIllegalKeyShareParameters);
    finally
      FreeAndNil(FDHKey.X);
      Yc.Free;
      K.Free;
    end;
  end
  else
  if FSessionInfo.CipherAlgorithm in ECDHEAlgorithms then begin
    Assert(FECKey <> nil);

    Size := Message.ReadInt8;
    if Message.RestCount < Size then
      SendAlert(adDecodeError, seInvalidMessage);

    ClientPoint := FECKey.ECData.ECCryptography.DecodePointFromOctetString(Message.Fragment, Message.ReadOffset, Size);
    SecretPoint := nil;
    try
      SecretPoint := FECKey.ECData.ECCryptography.MulPoint(ClientPoint, FECKey.ECData.PrivateKoef);
      if SecretPoint is TScECPointBI then
        FPreMasterSecret := TScECPointBI(SecretPoint).X.GetBytes(FECKey.ECData.ECCryptography.Size)
      else
      if SecretPoint is TScECPointX25519 then begin
        SetLength(FPreMasterSecret, 32);
        Move(TScECPointX25519(SecretPoint).PointField[0], FPreMasterSecret[0], 32);
        // https://tools.ietf.org/html/draft-ietf-tls-rfc4492bis-17#section-5.11
        i := 0;
        while (i < 32) and (FPreMasterSecret[i] = 0) do
          Inc(i);

        if i = 32 then
          SendAlert(adIllegalParameter, seIllegalKeyShareParameters);
      end
      else
        Assert(False);
    finally
      SecretPoint.Free;
      ClientPoint.Free;
    end;
  end
  else begin
    if GetProtocol = spSsl3 then
      Size := Message.RestCount
    else
      Size := Message.ReadInt16;

    SetLength(EncPreMaster, 0);
    EncPreMaster := Message.Read(Size);

    OwnCertificate := GetOwnCertificate;
    if OwnCertificate = nil then begin
      RemoteKey := nil;
      SendAlert(adHandshakeFailure, seServerCertificateNotSpecified);
    end
    else
      RemoteKey := OwnCertificate.Key;

    try
      FPreMasterSecret := RemoteKey.Decrypt(EncPreMaster);

      if Length(FPreMasterSecret) <> 48 then
        SendAlert(adDecodeError, seInvalidEncData);

      MaxVersion := TScSSLVersionHelper.GetVersion(FClientProtocol);
      if (FPreMasterSecret[0] <> MaxVersion.Major) or (FPreMasterSecret[1] <> MaxVersion.Minor) then
        SendAlert(adDecodeError, seInvalidEncData);
    except
      // Don't generate alert to prevent attack
      // https://tools.ietf.org/html/rfc5246#section-7.4.7.1
      SetLength(FPreMasterSecret, 48);
      FRandom.Random(FPreMasterSecret, 0, Length(FPreMasterSecret));
    end;
  end;
end;

procedure TServerHandshakeLayer.MakeMasterSecret;
begin
  FService.MakeMasterSecret(FPreMasterSecret);
  FillChar(FPreMasterSecret[0], Length(FPreMasterSecret), 0);
end;

procedure TServerHandshakeLayer.MakeCertificateRequestMessage;
begin
  FSendMessage.Init(htCertificateRequest);
  FService.MakeCertificateRequestMessage(FSendMessage, FOptions.RequestDNList);

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure TServerHandshakeLayer.MakeServerHelloDoneMessage;
begin
  FSendMessage.Init(htServerHelloDone);
  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure TServerHandshakeLayer.ProcessFinished(Message: THandshakeMessage);
var
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
begin
  RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ServerHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
  if RenegIndicatExtension <> nil then
    RenegIndicatExtension.SetClientFinishedMessage(Message.Fragment, Message.ReadOffset, Message.RestCount);

  // check hash received from client
  FService.VerifyFinishedMessage(Message.Fragment, Message.ReadOffset, Message.RestCount);
end;

procedure TServerHandshakeLayer.ResponseFinished;
var
  i: integer;
begin
  if GetProtocol = spTls13 then begin
    InitializeCiphers(cpRemote);

    FService.MakeResumptionMasterSecret;

    for i := 0 to FOptions.NewSessionTicketDistributedCount - 1 do
      MakeNewSessionTicketMessage;
  end
  else begin
    if not FFinished then begin
      if (FOptions.ServerHelloExtensions.Find(TTLSSessionTicketExtension) <> nil) and (FOptions.NewSessionTicketDistributedCount > 0) then
        MakeNewSessionTicketMessage;

      MakeChangeCipherSpec;
      MakeFinishedMessage;
    end;
  end;

  IsNegotiating := False;
  ClearKeyExchangeInfo;
  ClearNegotiationInfo;
end;

procedure TServerHandshakeLayer.MakeHelloRequestMessage;
var
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
begin
  RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ClientHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
  if RenegIndicatExtension <> nil then
    RenegIndicatExtension.Renegotiate;

  FSendMessage.Init(htHelloRequest);
  FSendMessage.Complete;
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

procedure TServerHandshakeLayer.MakeNewSessionTicketMessage;
begin
  if GetProtocol = spSsl3 then
    Exit;

  FSendMessage.Init(htNewSessionTicket);
  FService.MakeNewSessionTicketMessage(FSendMessage, FSessionInfo);

  FSendMessage.Complete;
  if GetProtocol <> spTls13 then
    FService.StoreNegotiationData(FSendMessage); // output message

  FRecordLayer.EncryptAndStoreMessage(FSendMessage);
end;

end.

