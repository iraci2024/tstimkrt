
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScClientHandshakeLayer;
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
  [Application Data]<------->   [Application Data]
*)
(*
  // TLS 1.3
  Client                     Server

  ClientHello       -------->
                                ServerHello
                                EncryptedExtensions
                                CertificateRequest*
                                Certificate*
                                CertificateVerify*
                                Finished
                    <-------    [Application Data]
  Certificate*
  CertificateVerify*
  Finished          -------->
  [Application Data]<------->   [Application Data]
*)

  TClientHandshakeLayer = class(THandshakeLayer)
  private
    FWasHelloRetryRequest: boolean;
    FMutualAuthentication: boolean;
    FRSAServerKey: TScKey;
    FDHServerKey: TScDHData;
    FECClientKey: TScKey;
    FECServerPoint: TScCustomECPoint;

    procedure SetEllipticCurvesExtension;
    procedure SetNewSessionTicketExtension(NewSessionTicket: TScNewSessionTicket);

  protected
    procedure ClearKeyExchangeInfo; override;
    procedure InternalProcessMessage(Message: THandshakeMessage); override;
    procedure ReplyToMessage(HandshakeType: TScHandshakeType); override;

    procedure MakeClientHelloMessage; override;
    procedure MakeClientKeyExchangeMessage;
    procedure ProcessServerHello(Message: THandshakeMessage);
    procedure PrepareKeyShares;
    procedure ProcessKeyShares;
    procedure ProcessEncryptedExtensions(Message: THandshakeMessage);
    procedure ProcessServerKeyExchange(Message: THandshakeMessage);
    procedure ProcessCertificateRequest(Message: THandshakeMessage);
    procedure ProcessServerHelloDone(Message: THandshakeMessage);
    procedure ResponseServerHelloDone;
    procedure ProcessFinished(Message: THandshakeMessage);
    procedure ResponseFinished;
    procedure MakeAuthenticationMessages;
    procedure ProcessNewSessionTicket(Message: THandshakeMessage);
    procedure ResponsePostHandshakeCertificateRequest;
    procedure MakeHelloRequestMessage; override;
    procedure ProcessHelloRequest(Message: THandshakeMessage);
  end;

implementation

uses
{$IFDEF VER17P}
{$IFNDEF NEXTGEN}
  Contnrs, Types,
{$ENDIF}
{$ENDIF}
{$IFNDEF SBRIDGE}
  CRDECUtil, CRBigInteger, CRHashAlgorithm, CRHash, CRHMAC, CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsCertificateConsts;
{$ELSE}
  TdsSSLConstsUni, TdsCertificateConstsUni;
{$ENDIF}
{$ELSE}
  ScDECUtil, ScBigInteger, ScHashAlgorithm, ScHash, ScHMAC, ScSymmetricAlgorithm,
  ScConsts, ScCertificateConsts;
{$ENDIF}

{ TClientHandshakeLayer }

procedure TClientHandshakeLayer.InternalProcessMessage(Message: THandshakeMessage);
begin
  case Message.HandshakeType of
    htServerHello: begin
      if FState <> htClientHello then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessServerHello(Message);
    end;

    htEncryptedExtensions: begin
      if GetProtocol = spTls13 then begin
        if FState <> htServerHello then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessEncryptedExtensions(Message);
    end;

    htCertificateRequest: begin
      if GetProtocol = spTls13 then begin
        if FFinished then begin // Post-Handshake authentication
          if not IsRequiredPostHandshakeAuth then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
        if FState <> htEncryptedExtensions then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if (FState <> htCertificate) and (FState <> htServerKeyExchange) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessCertificateRequest(Message);
    end;

    htCertificate: begin
      if GetProtocol = spTls13 then begin
        if (FState <> htEncryptedExtensions) and (FState <> htCertificateRequest) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if FState <> htServerHello then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessCertificate(Message);
    end;

    htCertificateVerify: begin
      if GetProtocol = spTls13 then begin
        if FState <> htCertificate then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType)
      end
      else
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessCertificateVerify(Message);
    end;

    htServerKeyExchange: begin
      if GetProtocol = spTls13 then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType)
      else begin
        if FSessionInfo.CipherAlgorithm in AnonAlgorithms then begin
          if FState <> htServerHello then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htCertificate then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end;

      ProcessServerKeyExchange(Message);
    end;

    htServerHelloDone: begin
      if GetProtocol = spTls13 then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType)
      else
      if (FState <> htCertificate) and (FState <> htServerKeyExchange) and (FState <> htCertificateRequest) then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessServerHelloDone(Message);
    end;

    htFinished: begin
      if GetProtocol = spTls13 then begin
        if FIsSessionResumption then begin
          if FState <> htEncryptedExtensions then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htCertificateVerify then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else
        if FState <> htChangeCipherSpec then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessFinished(Message);
    end;

    htHelloRequest: begin
      if GetProtocol = spTls13 then
        SendAlert(adUnexpectedMessage, seUnexpectedPacketType)
      else
        if FState <> htFinished then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);

      ProcessHelloRequest(Message);
    end;

    htNewSessionTicket: begin
      if GetProtocol = spTls13 then begin
        if (FState <> htFinished) and (FState <> htNewSessionTicket) then
          SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end
      else begin
        if FIsSessionResumption then begin
          if FState <> htServerHello then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
        end
        else
          if FState <> htServerHelloDone then
            SendAlert(adUnexpectedMessage, seUnexpectedPacketType);
      end;

      ProcessNewSessionTicket(Message);
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

procedure TClientHandshakeLayer.ReplyToMessage(HandshakeType: TScHandshakeType);
begin
  case HandshakeType of
    htServerHello: begin
      if GetProtocol = spTls13 then begin
        // https://tools.ietf.org/html/rfc8446#appendix-D.4
        MakeChangeCipherSpec;

        if FWasHelloRetryRequest then begin
          PrepareKeyShares;
          MakeClientHelloMessage;
        end
        else
          ProcessKeyShares;
      end;
    end;

    htServerHelloDone:
      ResponseServerHelloDone;

    htFinished:
      ResponseFinished;

    htHelloRequest:
      MakeHelloRequestMessage;

    htCertificateRequest: begin
      if FFinished then // Post-Handshake authentication
        ResponsePostHandshakeCertificateRequest;
    end;

    htKeyUpdate: begin
      if FLastKeyUpdateRequest = kurUpdateRequested then
        MakeKeyUpdateMessage(kurUpdateNotRequested);
    end;
  end;
end;

procedure TClientHandshakeLayer.ClearKeyExchangeInfo;
begin
  inherited;

  FreeAndNil(FRSAServerKey);
  FreeAndNil(FECClientKey);
  FreeAndNil(FECServerPoint);

  FreeAndNil(FDHServerKey.P);
  FreeAndNil(FDHServerKey.G);
  FreeAndNil(FDHServerKey.Y);
  FreeAndNil(FDHServerKey.X);
end;

procedure TClientHandshakeLayer.MakeHelloRequestMessage;
var
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
begin
  if IsNegotiating then // ignore hello request when Negotiating
    Exit;

  RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ClientHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
  if RenegIndicatExtension <> nil then
    RenegIndicatExtension.Renegotiate;

  MakeClientHelloMessage;
end;

procedure TClientHandshakeLayer.ProcessHelloRequest(Message: THandshakeMessage);
var
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
begin
  if Message.RestCount <> 0 then
    raise EScError.Create(seInvalidMessage);

  if IsNegotiating then // ignore hello request when Negotiating
    Exit;

  RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ClientHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
  if ((RenegIndicatExtension <> nil) and not RenegIndicatExtension.IsRemoteSupport) or
    (RenegIndicatExtension = nil)
  then
    raise EScError.Create(seRenegotiationDenied);
end;

procedure TClientHandshakeLayer.SetEllipticCurvesExtension;
var
  EllipticCurvePointFormatsExtension: TTLSEllipticCurvePointFormatsExtension;
  SupportedGroupsExtension: TTLSSupportedGroupsExtension;
  KeyShareExtension: TTLSKeyShareExtension;
  ECDHEAlgorithmsUsed: boolean;
  CipherAlgorithm: TScSSLCipherAlgorithm;
  i: integer;
begin
  ECDHEAlgorithmsUsed := False;
  for i := 0 to Length(FOptions.CipherAlgorithms) - 1 do begin
    CipherAlgorithm := FOptions.CipherAlgorithms[i];
    if (FOptions.Protocols * SSLCipherDefinitions[CipherAlgorithm].SupportedProtocols) <> [] then
      if (CipherAlgorithm in ECDHEAlgorithms) or
         (CipherAlgorithm in [caAES_128_GCM_SHA256, caAES_256_GCM_SHA384]) // TLS 1.3
      then begin
        ECDHEAlgorithmsUsed := True;
        Break;
      end;
  end;

  if ECDHEAlgorithmsUsed then begin
    EllipticCurvePointFormatsExtension := TTLSEllipticCurvePointFormatsExtension(FOptions.ClientHelloExtensions.FindOrCreate(TTLSEllipticCurvePointFormatsExtension));
    EllipticCurvePointFormatsExtension.Formats := [ecpfUncompressed];

    SupportedGroupsExtension := TTLSSupportedGroupsExtension(FOptions.ClientHelloExtensions.FindOrCreate(TTLSSupportedGroupsExtension));
    if SupportedGroupsExtension.Count = 0 then begin
      SupportedGroupsExtension.Add(x25519);
      SupportedGroupsExtension.Add(secp384r1);
      SupportedGroupsExtension.Add(secp256r1);
      SupportedGroupsExtension.Add(secp521r1);
      SupportedGroupsExtension.Add(sect571r1);
      SupportedGroupsExtension.Add(sect571k1);
      SupportedGroupsExtension.Add(sect409r1);
      SupportedGroupsExtension.Add(sect409k1);
      SupportedGroupsExtension.Add(sect283r1);
      SupportedGroupsExtension.Add(sect283k1);
      SupportedGroupsExtension.Add(secp256k1);

      if spTls13 in FOptions.Protocols then begin
        SupportedGroupsExtension.Add(ffDHE2048);
        SupportedGroupsExtension.Add(ffDHE3072);
        SupportedGroupsExtension.Add(ffDHE4096);
        SupportedGroupsExtension.Add(ffDHE6144);
        SupportedGroupsExtension.Add(ffDHE8192);

        KeyShareExtension := TTLSKeyShareExtension(FOptions.ClientHelloExtensions.FindOrCreate(TTLSKeyShareExtension));
        if KeyShareExtension.Count = 0 then // User MAY add values if necessary
          KeyShareExtension.Add(x25519);
      end;
    end;
  end
  else begin
    FOptions.ClientHelloExtensions.RemoveIfExists(TTLSEllipticCurvePointFormatsExtension);
    FOptions.ClientHelloExtensions.RemoveIfExists(TTLSSupportedGroupsExtension);
  end;
end;

procedure TClientHandshakeLayer.SetNewSessionTicketExtension(NewSessionTicket: TScNewSessionTicket);
var
  PreSharedKeyExtension: TTLSPreSharedKeyExtension;
begin
  if spTls13 in FOptions.Protocols then begin
    FOptions.ClientHelloExtensions.FindOrCreate(TTLSPskKeyExchangeModesExtension);

    PreSharedKeyExtension := TTLSPreSharedKeyExtension(FOptions.ClientHelloExtensions.FindOrCreate(TTLSPreSharedKeyExtension));
    PreSharedKeyExtension.HashAlgorithm := SSLCipherDefinitions[FSessionInfo.CipherAlgorithm].HashAlgorithm;
    PreSharedKeyExtension.Add(NewSessionTicket);
  end;
end;

procedure TClientHandshakeLayer.MakeClientHelloMessage;
var
  SupportedVersionsExtension: TTLSSupportedVersionsExtension;
  NewSessionTicket: TScNewSessionTicket;
  MaxVersion: TScSSLVersionRec;
  cd: TCipherDefinition;
  ID: TBytes;
  AllowedCiphersBuf, AllowedCompressionsBuf: TBytes;
  ClientTime: TBytes;
  TicketAge: cardinal;
  i, n: integer;
begin
  if FOptions.ClientHelloExtensions = nil then
    raise EScError.Create(seInvalidInputArgs);

  FFinished := False;
  FMutualAuthentication := False;
  FUseExtendedMasterSecret := False;

  // must be before checking of FSessionInfo.Initialized - to set once for every connection
  if not FInitialized then
    SetEllipticCurvesExtension;

  if not FInitialized and FSessionInfo.Initialized then begin
    FService.MasterSecret := FSessionInfo.MasterSecret;
    FInitialized := True;
    FIsSessionResumption := True;
  end
  else
    FIsSessionResumption := False;

  if FInitialized then begin
    MaxVersion := TScSSLVersionHelper.GetVersion(GetProtocol);
    SetLength(AllowedCiphersBuf, 2);
    TCipherSuites.CipherAlgorithmToBuf(FSessionInfo.CipherAlgorithm, AllowedCiphersBuf, 0);
    AllowedCompressionsBuf := TScCompressionHelper.CompressionToBuf(FSessionInfo.Compression);
  end
  else begin
    MaxVersion := TScSSLVersionHelper.GetVersion(TScSSLVersionHelper.GetMaxProtocol(FOptions.Protocols));

    SetLength(AllowedCiphersBuf, Length(FOptions.CipherAlgorithms) * 2);
    n := 0;
    for i := 0 to Length(FOptions.CipherAlgorithms) - 1 do begin
      cd := SSLCipherDefinitions[FOptions.CipherAlgorithms[i]];
      if (FOptions.Protocols * cd.SupportedProtocols) <> [] then begin
        TCipherSuites.CipherAlgorithmToBuf(FOptions.CipherAlgorithms[i], AllowedCiphersBuf, n);
        Inc(n, 2);
      end;
    end;
    SetLength(AllowedCiphersBuf, n);
    if n = 0 then
      raise EScError.Create(seInvalidCipherAlgorithm);

    AllowedCompressionsBuf := TScCompressionHelper.CompressionToBuf(FOptions.Compression);

    if FOptions.ClientHelloExtensions.Find(TTLSCookieExtension) <> nil then
      raise EScError.Create(seCookiesInInitialClientHello);
  end;

  if not FInitialized and (spTls13 in FOptions.Protocols) then begin
    SetLength(ID, 32);
    FRandom.Random(ID, 0, 32);
    FSessionInfo.SessionID := ID;
  end;
  // if Length(SessionID) = 0 - do not resume a session, and do not let the other side cache it

  if (not FInitialized and (spTls13 in FOptions.Protocols)) or (FInitialized and (GetProtocol = spTls13)) then begin
    SupportedVersionsExtension := TTLSSupportedVersionsExtension(FOptions.ClientHelloExtensions.FindOrCreate(TTLSSupportedVersionsExtension));
    SupportedVersionsExtension.Versions := [spTls13];
  end;

  if FInitialized and not FWasHelloRetryRequest and (spTls13 in FOptions.Protocols) and
    (FSessionInfo.NewSessionTickets.Count > 0)
  then begin
    NewSessionTicket := FSessionInfo.NewSessionTickets[0];
    TicketAge := GetUnixTime - NewSessionTicket.CreateTime;
    if TicketAge < NewSessionTicket.Lifetime then
      SetNewSessionTicketExtension(NewSessionTicket); // Must be last extension in the list
  end
  else
    NewSessionTicket := nil;

  FSendMessage.Init(htClientHello);
  FSendMessage.WriteInt8(MaxVersion.Major);
  FSendMessage.WriteInt8(MaxVersion.Minor);

  ClientTime := GetUnixTimeBuf;
  Move(ClientTime[0], FClientServerRandom[0], 4);
  FRandom.Random(FClientServerRandom, 4, 28);

  FSendMessage.WriteBuf0(FClientServerRandom, 0, 32);
  FSendMessage.WriteBuf8(FSessionInfo.SessionID);
  FSendMessage.WriteBuf16(AllowedCiphersBuf);
  FSendMessage.WriteBuf8(AllowedCompressionsBuf);
  FOptions.ClientHelloExtensions.Encode(FSendMessage, ceClient);

  FSendMessage.Complete;

  if NewSessionTicket <> nil then
    FService.WriteBinderKey(FSendMessage, FSessionInfo.MasterSecret, NewSessionTicket.Nonce);

  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);

  FState := htClientHello;
end;

procedure TClientHandshakeLayer.ProcessServerHello(Message: THandshakeMessage);
var
  ServerVersion: TScSSLVersionRec;
  ServerProtocol: TScSSLProtocol;
  LengthSID: integer;
  ServerSessionID: TBytes;
  ClientHelloExtension, ServerHelloExtension: TTLSHelloExtension;
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
  HasRenegIndicatExtension, HasMaxFragmentLengthExtension,
  HasRecordSizeLimitExtension: boolean;
  CookieExtension: TTLSCookieExtension;
  CipherAlgorithm: TScSSLCipherAlgorithm;
  CipherAlgorithmIsAllowed: boolean;
  Compression: TScCompression;
  i: integer;
begin
  if FOptions.ServerHelloExtensions = nil then
    raise EScError.Create(seInvalidInputArgs);

  FIsRenegotiationStarted := False;
  IsNegotiating := True;

  ServerVersion.Major := Message.ReadInt8;
  ServerVersion.Minor := Message.ReadInt8;
  ServerProtocol := TScSSLVersionHelper.GetProtocol(ServerVersion);
  if ServerVersion.Major < 3 then
    SendAlert(adProtocolVersion, seNotAgreeOnProtocol);

  // the time from the server [1 cardinal] and the random bytes [28 bytes]
  Message.ReadTo(FClientServerRandom, 32, 32);

  if MemCompare(@FClientServerRandom[32], @HelloRetryRequestID[0], 32) = 0 then begin
    if FWasHelloRetryRequest then // check that first time
      SendAlert(adUnexpectedMessage, seUnexpectedPacketType)
    else
      FWasHelloRetryRequest := True;
  end
  else
    FWasHelloRetryRequest := False;

  // the session ID [0..32 bytes]
  LengthSID := Message.ReadInt8;
  if LengthSID > 32 then
    raise EScError.Create(seInvalidMessage);
  ServerSessionID := Message.Read(LengthSID);

  // the selected cipher suite
  CipherAlgorithm := TCipherSuites.ToCipherAlgorithm(Message.ReadInt16);
  CipherAlgorithmIsAllowed := False;
  for i := 0 to Length(FOptions.CipherAlgorithms) - 1 do begin
    if CipherAlgorithm = FOptions.CipherAlgorithms[i] then begin
      CipherAlgorithmIsAllowed := True;
      break;
    end;
  end;
  if not CipherAlgorithmIsAllowed then
    SendAlert(adIllegalParameter, seVerifyEncAlgFailed);

  // the selected compression method
  Compression := TScCompressionHelper.ToCompression(Message.ReadInt8);
  if (FOptions.Compression <> csAllowed) and (FOptions.Compression <> Compression) then
    SendAlert(adIllegalParameter, seVerifyCompressAlgFailed);

  if not FInitialized then begin
    FSessionInfo.CipherAlgorithm := CipherAlgorithm;
    FSessionInfo.Compression := Compression;
    FSessionInfo.DisableInsertEmptyFragment := FOptions.DisableInsertEmptyFragment;
  end
  else begin
    // when Renegotiation or after Hello Retry request
    if FSessionInfo.CipherAlgorithm <> CipherAlgorithm then
      SendAlert(adIllegalParameter, seVerifyEncAlgFailed);
    if FSessionInfo.Compression <> Compression then
      SendAlert(adIllegalParameter, seVerifyCompressAlgFailed);
  end;

  // Server extensions
  HasRenegIndicatExtension := False;
  RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ClientHelloExtensions.Find(TTLSRenegotiationIndicationExtension));

  FOptions.ServerHelloExtensions.Clear;
  if Message.RestCount > 0 then begin
    try
      FOptions.ServerHelloExtensions.Parse(Message, ceClient);
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

    HasMaxFragmentLengthExtension := False;
    HasRecordSizeLimitExtension := False;

    for i := 0 to FOptions.ServerHelloExtensions.Count - 1 do begin
      ServerHelloExtension := FOptions.ServerHelloExtensions.Extensions[i];

      if ServerHelloExtension.ClassType = TTLSUnknownExtension then
        raise EScError.CreateFmt(SUnknownExtensionClass, [IntToStr(TTLSUnknownExtension(ServerHelloExtension).TypeCode)], seUnknownExtensionClass)
      else
      if (ServerHelloExtension.ClassType = TTLSSignatureAlgorithmsExtension) or
         (ServerHelloExtension.ClassType = TTLSSignatureAlgorithmsCertExtension) or
         (ServerHelloExtension.ClassType = TTLSPostHandshakeAuthExtension)
      then
        raise EScError.CreateFmt(SUnsupportedExtension, [ServerHelloExtension.ClassName], seUnsupportedExtension)
      else
      if ServerHelloExtension.ClassType = TTLSCookieExtension then begin
        if FWasHelloRetryRequest then begin
          CookieExtension := TTLSCookieExtension(FOptions.ClientHelloExtensions.Find(TTLSCookieExtension));
          if CookieExtension = nil then begin
            CookieExtension := TTLSCookieExtension.Create;
            CookieExtension.Assign(ServerHelloExtension);
            FOptions.ClientHelloExtensions.Add(CookieExtension);
          end
          else
            raise EScError.Create(seCookiesInInitialClientHello);
        end
        else
          raise EScError.CreateFmt(SUnsupportedExtension, [ServerHelloExtension.ClassName], seUnsupportedExtension)
      end
      else begin
        ClientHelloExtension := FOptions.ClientHelloExtensions.Find(TTLSHelloExtensionClass(ServerHelloExtension.ClassType));
        if ClientHelloExtension = nil then
          raise EScError.CreateFmt(SUnsupportedExtension, [ServerHelloExtension.ClassName], seUnsupportedExtension);

        if ServerHelloExtension.ClassType = TTLSSupportedVersionsExtension then begin
          if not (ServerProtocol in [spTls12, spTls13]) then
            raise EScError.CreateFmt(SUnsupportedExtension, [IntToStr(EXTENSION_TYPE_CODES[etSupportedVersions])], seUnsupportedExtension);

          if TTLSSupportedVersionsExtension(ServerHelloExtension).SelectedVersion <> spTls13 then
            raise EScError.Create(seWrongExtensionData);

          ServerProtocol := spTls13;
        end
        else
        if ServerHelloExtension.ClassType = TTLSExtendedMasterSecretExtension then
          FUseExtendedMasterSecret := True
        else
        if ServerHelloExtension.ClassType = TTLSMaxFragmentLengthExtension then begin
          if HasRecordSizeLimitExtension then
            SendAlert(adIllegalParameter, seWrongExtensionData);

          if TTLSMaxFragmentLengthExtension(ClientHelloExtension).MaxFragmentLength <> TTLSMaxFragmentLengthExtension(ServerHelloExtension).MaxFragmentLength then
            SendAlert(adIllegalParameter, seWrongExtensionData);

          FRecordLayer.MaxRecordLength := TTLSMaxFragmentLengthExtension(ServerHelloExtension).MaxFragmentLengthInByte;
          HasMaxFragmentLengthExtension := True;
        end
        else
        if ServerHelloExtension.ClassType = TTLSRecordSizeLimitExtension then begin
          if HasMaxFragmentLengthExtension then
            SendAlert(adIllegalParameter, seWrongExtensionData);

          FRecordLayer.MaxRecordLength := TTLSRecordSizeLimitExtension(ServerHelloExtension).RecordSizeLimit;
          HasRecordSizeLimitExtension := True;
        end
        else
        if ServerHelloExtension.ClassType = TTLSRenegotiationIndicationExtension then begin
          HasRenegIndicatExtension := True;
          Assert(RenegIndicatExtension <> nil);
          RenegIndicatExtension.Check(TTLSRenegotiationIndicationExtension(ServerHelloExtension));
        end;
      end;
    end;
  end;

  CheckAndChangeProtocol(ServerProtocol);

  if GetProtocol = spTls13 then begin
    if HasRenegIndicatExtension then
      raise EScError.Create(seRenegotiationDenied);

    // tools.ietf.org/html/rfc8446#section-4.1.3
    if (Length(FSessionInfo.SessionID) <> Length(ServerSessionID)) or (CompareBuf(FSessionInfo.SessionID, ServerSessionID, Length(ServerSessionID)) <> 0) then
      SendAlert(adIllegalParameter, seIllegalSessionIDParameter);

    FIsSessionResumption := FOptions.ServerHelloExtensions.Find(TTLSPreSharedKeyExtension) <> nil;
    if FSessionInfo.Initialized and not FIsSessionResumption then
      FInitialized := False;
  end
  else begin
    FSessionInfo.SessionID := ServerSessionID;
    FWasHelloRetryRequest := False;

    if FInitialized and (FSessionInfo.UseExtendedMasterSecret <> FUseExtendedMasterSecret) then
      raise EScError.CreateFmt(SUnsupportedExtension, [TTLSExtendedMasterSecretExtension.ClassName], seUnsupportedExtension);

    FSessionInfo.UseExtendedMasterSecret := FUseExtendedMasterSecret;

    if (RenegIndicatExtension <> nil) and RenegIndicatExtension.IsRemoteSupport and
       not HasRenegIndicatExtension then
      raise EScError.Create(seRenegotiationDenied);
  end;

  if not FInitialized then
    FSessionInfo.Protocol := GetProtocol
  else
    // when Renegotiation or after Hello Retry request
    if FSessionInfo.Protocol <> GetProtocol then
      SendAlert(adIllegalParameter, seNotAgreeOnProtocol);

  if not FWasHelloRetryRequest then begin
    if (spTls13 in FOptions.Protocols) and (GetProtocol <> spTls13) then begin
      if MemCompare(@FClientServerRandom[32+24], @TLS12Protection[0], 8) = 0 then
        SendAlert(adIllegalParameter, seIllegalRandomParameter);
      if MemCompare(@FClientServerRandom[32+24], @TLS11Protection[0], 8) = 0 then
        SendAlert(adIllegalParameter, seIllegalRandomParameter);
    end
    else
    if (spTls12 in FOptions.Protocols) and (GetProtocol <> spTls12) and (GetProtocol <> spTls13) then begin
      if MemCompare(@FClientServerRandom[32+24], @TLS11Protection[0], 8) = 0 then
        SendAlert(adIllegalParameter, seIllegalRandomParameter);
    end;
  end;

  if not (GetProtocol in SSLCipherDefinitions[FSessionInfo.CipherAlgorithm].SupportedProtocols) then
    raise EScError.Create(seInvalidCipherAlgorithm);

  if (GetProtocol = spTls13) and not FInitialized then begin
    FService.CreateClientHello1Hash;
    FService.MakeEarlySecret(nil);
    if FWasHelloRetryRequest then
      FService.TranscriptClientHello1Hash;
  end;

  FInitialized := True;
end;

procedure TClientHandshakeLayer.PrepareKeyShares;
var
  SupportedGroupsExtensionC: TTLSSupportedGroupsExtension;
  KeyShareExtensionC, KeyShareExtensionS: TTLSKeyShareExtension;
  KExNamedGroup: TScKExNamedGroupType;
  IsFound: boolean;
  i: integer;
begin
  KeyShareExtensionC := TTLSKeyShareExtension(FOptions.ClientHelloExtensions.Find(TTLSKeyShareExtension));
  KeyShareExtensionS := TTLSKeyShareExtension(FOptions.ServerHelloExtensions.Find(TTLSKeyShareExtension));
  if (KeyShareExtensionC = nil) or (KeyShareExtensionS = nil) or (KeyShareExtensionS.Count <> 1) then
    SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);

  SupportedGroupsExtensionC := TTLSSupportedGroupsExtension(FOptions.ClientHelloExtensions.Find(TTLSSupportedGroupsExtension));
  if SupportedGroupsExtensionC = nil then
    SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);

  KExNamedGroup := KeyShareExtensionS.KExNamedGroups[0];

  IsFound := False;
  for i := 0 to SupportedGroupsExtensionC.Count - 1 do begin
    if SupportedGroupsExtensionC.KExNamedGroups[i] = KExNamedGroup then begin
      IsFound := True;
      Break;
    end;
  end;

  if not IsFound then
    SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);

  for i := 0 to KeyShareExtensionC.Count - 1 do begin
    if KeyShareExtensionC.KExNamedGroups[i] = KExNamedGroup then
      SendAlert(adIllegalParameter, seVerifyKeyExAlgFailed);
  end;

  SupportedGroupsExtensionC.Clear;
  SupportedGroupsExtensionC.Add(KExNamedGroup);
  KeyShareExtensionC.Clear;
  KeyShareExtensionC.Add(KExNamedGroup);
end;

procedure TClientHandshakeLayer.ProcessKeyShares;
var
  KeyShareExtensionC, KeyShareExtensionS: TTLSKeyShareExtension;
  KExNamedGroupType: TScKExNamedGroupType;
  FFGType: TScFiniteFieldGroupType;
  ECKey: TScKey;
  ServerPoint, SecretPoint: TScCustomECPoint;
  Ys, Xc, K: TBigInteger;
  PreMasterSecret: TBytes;
  P: TBigInteger;
  No, i: integer;
begin
  KeyShareExtensionC := TTLSKeyShareExtension(FOptions.ClientHelloExtensions.Find(TTLSKeyShareExtension));
  KeyShareExtensionS := TTLSKeyShareExtension(FOptions.ServerHelloExtensions.Find(TTLSKeyShareExtension));

  if (KeyShareExtensionC = nil) or (KeyShareExtensionS = nil) or (KeyShareExtensionS.Count <> 1) then
    SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

  KExNamedGroupType := KeyShareExtensionS.KExNamedGroups[0];
  No := 0;
  while No < KeyShareExtensionC.Count do begin
    if KeyShareExtensionC.KExNamedGroups[No] = KExNamedGroupType then
      Break;
    Inc(No);
  end;

  if No = KeyShareExtensionC.Count then
    SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

  if integer(KExNamedGroupType) <= integer(High(TScECName)) then begin
    ECKey := KeyShareExtensionC.PrivateECKeys[No];
    ServerPoint := KeyShareExtensionS.PublicECPoints[0];
    if (ECKey = nil) or (ServerPoint = nil) then
      SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

    SecretPoint := ECKey.ECData.ECCryptography.MulPoint(ServerPoint, ECKey.ECData.PrivateKoef);
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
    FFGType := TScFiniteFieldGroupType(integer(KExNamedGroupType) - integer(High(TScECName)) - 1);
    Xc := KeyShareExtensionC.PrivateXs[No];
    Ys := KeyShareExtensionS.PublicYs[0];
    if (Ys = nil) or (Xc = nil) then
      SendAlert(adIllegalParameter, seIllegalKeyShareParameters);

    P := FFDHE_PRIME(FFGType);
    // https://tools.ietf.org/html/rfc8446#section-4.2.8.1
    if (Ys.BitCount <= 1) or Ys.GreaterOrEqual(P) then
      SendAlert(adIllegalParameter, seWrongExtensionData);

    K := Ys.ModPow(Xc, P);
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

procedure TClientHandshakeLayer.ProcessEncryptedExtensions(Message: THandshakeMessage);
var
  ClientHelloExtension, ServerHelloExtension: TTLSHelloExtension;
  HasMaxFragmentLengthExtension, HasRecordSizeLimitExtension: boolean;
  i: integer;
begin
  try
    FOptions.ServerHelloExtensions.Parse(Message, ceClient);
  except
    SendAlert(adIllegalParameter, seWrongExtensionData);
  end;

  HasMaxFragmentLengthExtension := False;
  HasRecordSizeLimitExtension := False;

  for i := 0 to FOptions.ServerHelloExtensions.Count - 1 do begin
    ServerHelloExtension := FOptions.ServerHelloExtensions.Extensions[i];

    if ServerHelloExtension.ClassType = TTLSUnknownExtension then
      raise EScError.CreateFmt(SUnknownExtensionClass, [IntToStr(TTLSUnknownExtension(ServerHelloExtension).TypeCode)], seUnknownExtensionClass)
    else begin
      ClientHelloExtension := FOptions.ClientHelloExtensions.Find(TTLSHelloExtensionClass(ServerHelloExtension.ClassType));
      if ClientHelloExtension = nil then
        raise EScError.CreateFmt(SUnsupportedExtension, [ServerHelloExtension.ClassName], seUnsupportedExtension);

      if ServerHelloExtension.ClassType = TTLSMaxFragmentLengthExtension then begin
        if HasRecordSizeLimitExtension then
          SendAlert(adIllegalParameter, seWrongExtensionData);

        if TTLSMaxFragmentLengthExtension(ClientHelloExtension).MaxFragmentLength <> TTLSMaxFragmentLengthExtension(ServerHelloExtension).MaxFragmentLength then
          SendAlert(adIllegalParameter, seWrongExtensionData);

        FRecordLayer.MaxRecordLength := TTLSMaxFragmentLengthExtension(ServerHelloExtension).MaxFragmentLengthInByte;
        HasMaxFragmentLengthExtension := True;
      end
      else
      if ServerHelloExtension.ClassType = TTLSRecordSizeLimitExtension then begin
        if HasMaxFragmentLengthExtension then
          SendAlert(adIllegalParameter, seWrongExtensionData);

        FRecordLayer.MaxRecordLength := TTLSRecordSizeLimitExtension(ServerHelloExtension).RecordSizeLimit;
        HasRecordSizeLimitExtension := True;
      end;
    end;
  end;
end;

procedure TClientHandshakeLayer.ProcessServerKeyExchange(Message: THandshakeMessage);
var
  RSAData: TScRSAData;
  ECName: TScECName;
  MinDHEKeyLen: integer;
  Size: integer;
begin
  if not (FSessionInfo.CipherAlgorithm in AnonAlgorithms) then begin
    if not (FSessionInfo.CipherAlgorithm in DHEAlgorithms) and not (FSessionInfo.CipherAlgorithm in ECDHEAlgorithms) then
      raise EScError.Create(seErrorServerKeyExchangeNonExportable);

    if FRemoteCertificate = nil then
      raise EScError.Create(seServerCertificateNotReceived);
  end;

  if (FSessionInfo.CipherAlgorithm in DHEAlgorithms) or (FSessionInfo.CipherAlgorithm in AnonAlgorithms) then begin
    FreeAndNil(FDHServerKey.P);
    FreeAndNil(FDHServerKey.G);
    FreeAndNil(FDHServerKey.Y);
    MinDHEKeyLen := FOptions.MinDHEKeyLength div 8;

    // extract P, G, Y
    Size := Message.ReadInt16; // P
    if Size < MinDHEKeyLen then
      raise EScError.Create(seErrorDiffieHelmanKeyLength);
    FDHServerKey.P := TBigInteger.Create(Message.Fragment, Message.ReadOffset, Size);
    Message.SkipRead(Size);

    Size := Message.ReadInt16; // G
    FDHServerKey.G := TBigInteger.Create(Message.Fragment, Message.ReadOffset, Size);
    Message.SkipRead(Size);

    Size := Message.ReadInt16; // Y
    if Size < MinDHEKeyLen then  // MySQL server 5.1.73 uses 512 bit
      raise EScError.Create(seErrorDiffieHelmanKeyLength);
    FDHServerKey.Y := TBigInteger.Create(Message.Fragment, Message.ReadOffset, Size);
    Message.SkipRead(Size);
  end
  else
  if FSessionInfo.CipherAlgorithm in ECDHEAlgorithms then begin
    if Message.ReadInt8 <> ELLIPTIC_CURVE_DOMAIN_CODES[ecdtNamedCurve] then
      raise EScError.Create(seECurveDomainTypeNotSupported);

    ECName := TCipherSuites.ToEllipticCurve(Message.ReadInt16);
    if ECName = x25519 then
      Size := EllipticCurvesParameters[ECName].Size
    else
      Size := EllipticCurvesParameters[ECName].Size * 2 + 1; // format of EC Point
    if Message.ReadInt8 <> Size then
      raise EScError.Create(seInvalidMessage);

    if Message.RestCount < Size then
      raise EScError.Create(seInvalidMessage);

    FreeAndNil(FECClientKey);
    FECClientKey := TScKey.Create(nil);
    FECClientKey.GenerateEC(ECName);
    FECServerPoint := FECClientKey.ECData.ECCryptography.DecodePointFromOctetString(Message.Fragment, Message.ReadOffset, Size);
    Message.SkipRead(Size);
  end
  else begin
    FillChar(RSAData, SizeOf(RSAData), 0);
    // extract modulus and exponent
    Size := Message.ReadInt16;
    RSAData.PublicModulus := TBigInteger.Create(Message.Fragment, Message.ReadOffset, Size);
    Message.SkipRead(Size);

    Size := Message.ReadInt16;
    RSAData.PublicExponent := TBigInteger.Create(Message.Fragment, Message.ReadOffset, Size);
    Message.SkipRead(Size);

    FreeAndNil(FRSAServerKey);
    FRSAServerKey := TScKey.Create(nil);
    TScKeyUtils.SetRSAData(FRSAServerKey, RSAData);
    if FRSAServerKey.BitCount < 1024 then
      raise EScError.Create(seErrorServerKeyExchangePublicKey);
  end;

  if not (FSessionInfo.CipherAlgorithm in AnonAlgorithms) then
    FService.VerifyServerKeyExchange(Message, FRemoteCertificate);
end;

procedure TClientHandshakeLayer.MakeClientKeyExchangeMessage;
var
  RandomBuf, tmp: TBytes;
  PByteCount: integer;
  PreMasterSecret: TBytes;
  Xc, Yc, K: TBigInteger;
  SecretPoint: TScCustomECPoint;
  MaxVersion: TScSSLVersionRec;
  RemoteKey: TScKey;
  i: integer;
begin
  SetLength(tmp, 0);

  FSendMessage.Init(htClientKeyExchange);

  if (FSessionInfo.CipherAlgorithm in DHEAlgorithms) or (FSessionInfo.CipherAlgorithm in AnonAlgorithms) then begin
    Xc := nil;
    Yc := nil;
    K := nil;
    try
      PByteCount := (FDHServerKey.P.BitCount + 7) shr 3;
      if FDHServerKey.P.BitCount >= 2048 then
        SetLength(RandomBuf, 128)
      else
        SetLength(RandomBuf, 64);

      repeat
        FRandom.Random(RandomBuf, 0, Length(RandomBuf));
        if RandomBuf[0] > $7F then
          RandomBuf[0] := RandomBuf[0] div 2;
        if RandomBuf[0] = 0 then
          RandomBuf[0] := 1;

        FreeAndNil(Xc);
        Xc := TBigInteger.Create(RandomBuf);
        FreeAndNil(Yc);
        Yc := FDHServerKey.G.ModPow(Xc, FDHServerKey.P);

        FreeAndNil(K);
        K := FDHServerKey.Y.ModPow(Xc, FDHServerKey.P);
        PreMasterSecret := K.GetBytes;
      until Length(PreMasterSecret) = PByteCount;

      tmp := Yc.GetBytes(PByteCount);
      FSendMessage.WriteBuf16(tmp);
    finally
      FillChar(RandomBuf[0], Length(RandomBuf), 0);
      Xc.Free;
      Yc.Free;
      K.Free;
    end;
  end
  else
  if FSessionInfo.CipherAlgorithm in ECDHEAlgorithms then begin
    Assert(FECClientKey <> nil);

    tmp := FECClientKey.ECData.ECCryptography.EncodePointToOctetString(FECClientKey.ECData.PublicPoint);
    FSendMessage.WriteBuf8(tmp);

    SecretPoint := nil;
    try
      SecretPoint := FECClientKey.ECData.ECCryptography.MulPoint(FECServerPoint, FECClientKey.ECData.PrivateKoef);
      if SecretPoint is TScECPointBI then
        PreMasterSecret := TScECPointBI(SecretPoint).X.GetBytes(FECClientKey.ECData.ECCryptography.Size)
      else
      if SecretPoint is TScECPointX25519 then begin
        SetLength(PreMasterSecret, 32);
        Move(TScECPointX25519(SecretPoint).PointField[0], PreMasterSecret[0], 32);
        // https://tools.ietf.org/html/draft-ietf-tls-rfc4492bis-17#section-5.11
        i := 0;
        while (i < 32) and (PreMasterSecret[i] = 0) do
          Inc(i);

        if i = 32 then
          raise EScError.Create(seIllegalKeyShareParameters);
      end
      else
        Assert(False);
    finally
      SecretPoint.Free;
    end;
  end
  else begin
    SetLength(PreMasterSecret, 48);
    FRandom.Random(PreMasterSecret, 0, Length(PreMasterSecret));
    MaxVersion := TScSSLVersionHelper.GetVersion(TScSSLVersionHelper.GetMaxProtocol(FOptions.Protocols));
    PreMasterSecret[0] := MaxVersion.Major;
    PreMasterSecret[1] := MaxVersion.Minor;

    if FRSAServerKey = nil then
      if FRemoteCertificate = nil then
        raise EScError.Create(seServerCertificateNotReceived)
      else
        RemoteKey := FRemoteCertificate.Key
    else
      RemoteKey := FRSAServerKey;

    tmp := RemoteKey.Encrypt(PreMasterSecret);
    if GetProtocol = spSsl3 then
      FSendMessage.WriteBuf0(tmp)
    else
      FSendMessage.WriteBuf16(tmp);
  end;

  FSendMessage.Complete;
  FService.StoreNegotiationData(FSendMessage); // output message
  FRecordLayer.EncryptAndStoreMessage(FSendMessage);

  FService.MakeMasterSecret(PreMasterSecret);
  FillChar(PreMasterSecret[0], Length(PreMasterSecret), 0);
end;

procedure TClientHandshakeLayer.ProcessCertificateRequest(Message: THandshakeMessage);
var
  DNList: TScDistinguishedNameList;
begin
  DNList := FService.ParseCertificateRequestMessage(Message);
  try
    FOwnCertificateList.Clear;
    if (DNList <> nil) and Assigned(FOptions.OnCertificateRequest) then
      FOptions.OnCertificateRequest(DNList, FOwnCertificateList);
  finally
    DNList.Free;
  end;

  FMutualAuthentication := not FIsSessionResumption;
end;

procedure TClientHandshakeLayer.ProcessServerHelloDone(Message: THandshakeMessage);
begin
  if Message.RestCount <> 0 then
    raise EScError.Create(seInvalidMessage);

  FIsSessionResumption := False;
end;

procedure TClientHandshakeLayer.ResponseServerHelloDone;
begin
  // send Certificate [optional]
  if FMutualAuthentication then
    MakeCertificateMessage(FOwnCertificateList);

  // send ClientKeyExchange
  MakeClientKeyExchangeMessage;

  // send CertificateVerify [optional]
  if FMutualAuthentication and (FOwnCertificateList.Count > 0) then
    MakeCertificateVerifyMessage(TScCertificate(FOwnCertificateList[0]));

  // send ChangeCipherSpec
  MakeChangeCipherSpec;

  // send Finished
  MakeFinishedMessage;

  // send empty record [http://www.openssl.org/~bodo/tls-cbc.txt]
  if FRecordLayer.Encryptor.Mode = cmCBC then
    /// To avoid a certain CBC IV attack, the client sends an empty message after the handshake and before the actual application payload.
    /// Unfortunately, some broken implementations do not support empty packets, so sending this empty packet can be turned off
    /// by specifying the DisableInsertEmptyFragment option.
    if not FOptions.DisableInsertEmptyFragment then
      FRecordLayer.EncryptAndStore(nil, 0, ctApplicationData);
end;

procedure TClientHandshakeLayer.ProcessFinished(Message: THandshakeMessage);
var
  RenegIndicatExtension: TTLSRenegotiationIndicationExtension;
begin
  RenegIndicatExtension := TTLSRenegotiationIndicationExtension(FOptions.ClientHelloExtensions.Find(TTLSRenegotiationIndicationExtension));
  if RenegIndicatExtension <> nil then
    RenegIndicatExtension.SetServerFinishedMessage(Message.Fragment, Message.ReadOffset, Message.RestCount);

  // check hash received from server
  FService.VerifyFinishedMessage(Message.Fragment, Message.ReadOffset, Message.RestCount);
end;

procedure TClientHandshakeLayer.ResponseFinished;
begin
  if GetProtocol = spTls13 then begin
    FService.MakeApplicationTrafficSecret;

    if not FFinished then
      MakeAuthenticationMessages;

    InitializeCiphers(cpBoth);
  end
  else begin
    if not FFinished then begin
      // send ChangeCipherSpec
      MakeChangeCipherSpec;

      // send Finished
      MakeFinishedMessage;
    end;

    IsNegotiating := False;
  end;

  ClearKeyExchangeInfo;
  if FMutualAuthentication or not IsRequiredPostHandshakeAuth then
    ClearNegotiationInfo;
end;

procedure TClientHandshakeLayer.MakeAuthenticationMessages;
begin
  if FMutualAuthentication then begin
    // send Certificate [optional]
    MakeCertificateMessage(FOwnCertificateList);

      // send CertificateVerify [optional]
    if FOwnCertificateList.Count > 0 then
      MakeCertificateVerifyMessage(TScCertificate(FOwnCertificateList[0]));
  end;

  // send Finished
  MakeFinishedMessage;
  IsNegotiating := False;
  FService.MakeResumptionMasterSecret;
end;

procedure TClientHandshakeLayer.ResponsePostHandshakeCertificateRequest;
begin
  MakeAuthenticationMessages;
  ClearNegotiationInfo;
end;

procedure TClientHandshakeLayer.ProcessNewSessionTicket(Message: THandshakeMessage);
begin
  if GetProtocol <> spTls13 then begin
    if FOptions.ServerHelloExtensions.Find(TTLSSessionTicketExtension) = nil then
      raise EScError.Create(seSessionTicketDenied);
  end;

  FService.ParseNewSessionTicketMessage(Message, FSessionInfo);
end;

end.

