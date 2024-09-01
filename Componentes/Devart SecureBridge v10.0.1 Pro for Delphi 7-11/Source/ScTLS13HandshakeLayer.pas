
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScTLS13HandshakeLayer;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions, CRHash, CRHashAlgorithm, CRHMAC,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLTypes, TdsCipherSuites, TdsBridge, TdsCertificateExts,
  TdsLayers, TdsSSLMessages, TdsAlgorithmSupport, TdsSSLExtensions;
{$ELSE}
  TdsUtilsUni, TdsSSLTypesUni, TdsCipherSuitesUni, TdsBridgeUni, TdsCertificateExtsUni,
  TdsLayersUni, TdsSSLMessagesUni, TdsAlgorithmSupportUni, TdsSSLExtensionsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions, ScHash, ScHashAlgorithm, ScHMAC,
  ScUtils, ScSSLTypes, ScCipherSuites, ScBridge, ScCertificateExts,
  ScLayers, ScSSLMessages, ScAlgorithmSupport, ScSSLExtensions;
{$ENDIF}

type
  { T = T(1) | T(2) | T(3) | ... | T(N)
    T(0) = empty string (zero length)
    T(1) = HMAC-Hash(PRK, T(0) | info | $01)
    T(2) = HMAC-Hash(PRK, T(1) | info | $02)
    T(3) = HMAC-Hash(PRK, T(2) | info | $03)
  }
  THMACDeriveKeyBytes = class
  private
    FHMAC: THMAC;
    FInfo: TBytes;
    FNextBytes: TBytes;
    FReadCount: integer;
    FTi: TBytes;
    function GetNextBytes: TBytes;

  public
    constructor Create(HashAlgorithm: THashAlgorithmClass; const Secret, Info: TBytes);
    destructor Destroy; override;

    function GetBytes(Count: integer): TBytes;
    procedure Reset;
  end;

  TTls13HandshakeProtocol = class(THandshakeProtocolService)
  private
    FEmptyStrHash: TBytes;
    FClientHello1Hash: TBytes;
    FDerivedSecret: TBytes;
    FResBinderKey: TBytes;
    FFinishedBinderKey: TBytes;
//    FExtBinderKey: TBytes;
//    FClientEarlyTrafficSecret: TBytes;
//    FEarlyExporterMasterSecret: TBytes;
    FClientTrafficSecret: TBytes;
    FServerTrafficSecret: TBytes;
    FClientHandshakeTrafficSecret: TBytes;
    FServerHandshakeTrafficSecret: TBytes;
    FClientApplicationTrafficSecret: TBytes;
    FServerApplicationTrafficSecret: TBytes;
//    FExporterMasterSecret: TBytes;
    FResumptionMasterSecret: TBytes;
    FLocalCertificateRequestContext: TBytes;
    FRemoteCertificateRequestContext: TBytes;

    function HKDFExtract(const Salt, IKM: TBytes): TBytes;
    function HKDFExpand(const PRK, Info: TBytes; Size: integer): TBytes;
    function HKDFExpandLabel(const Secret: TBytes; const aLabel: array of byte;
      const Context: TBytes; Size: integer): TBytes;
    function EmptyStrHash: TBytes;
    function DeriveSecret(const Secret: TBytes; const aLabel: array of byte; const HashMessages: TBytes): TBytes;
    function GetHashSize: integer;
    function CalcBinderKey(const MessageBuf: TBytes; Offset, Count: integer;
      const MasterSecret, TicketNonce: TBytes): TBytes;

  public
    function InitializeCipherSuite(const Definition: TCipherDefinition;
      const CipherPart: TCipherPart): TCipherSuite; override;

    procedure CreateClientHello1Hash; override;
    procedure TranscriptClientHello1Hash; override;
    procedure MakeEarlySecret(const PreSharedKey: TBytes); override;
    procedure MakeHandshakeTrafficSecret(const Premaster: TBytes); override;
    procedure MakeApplicationTrafficSecret; override;
    procedure MakeResumptionMasterSecret; override;
    procedure UpdateClientApplicationTrafficSecret; override;
    procedure UpdateServerApplicationTrafficSecret; override;
    procedure MakeMasterSecret(const Premaster, Random: TBytes; const aLabel: array of byte); override;

    procedure MakeCertificateRequestMessage(Message: THandshakeMessage; DNList: TScDistinguishedNameList); override;
    function ParseCertificateRequestMessage(Message: THandshakeMessage): TScDistinguishedNameList; override;
    procedure WriteCertificateMessage(Message: THandshakeMessage; CertList: TCRList); override;
    procedure ReadCertificateMessage(Message: THandshakeMessage; CertList: TCRList); override;
    procedure WriteCertificateVerifyMessage(Message: THandshakeMessage; Certificate: TScCertificate); override;
    procedure VerifyCertificateMessage(Message: THandshakeMessage; RemoteCertificate: TScCertificate); override;

    procedure WriteFinishedMessage(Message: THandshakeMessage); override;
    procedure VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer); override;

    procedure MakeNewSessionTicketMessage(Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo); override;
    procedure ParseNewSessionTicketMessage(Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo); override;

    procedure WriteBinderKey(ClientHelloMessage: THandshakeMessage; const MasterSecret, TicketNonce: TBytes); override;
    procedure DecodeAndVerifyTicket(ClientHelloMessage: THandshakeMessage;
      ClientHelloExtensions: TTLSHelloExtensions; NewSessionInfo: TScSSLSessionInfo;
      out SelectedIdentity: integer); override;
    function CreateHMACHash: THashAlgorithm; override;
    function GetHashAlgorithmClass: THashAlgorithmClass;
    function GetProtocol: TScSSLProtocol; override;
  end;

implementation

uses
{$IFNDEF SBRIDGE}
  CRDECUtil, CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsCertificateConsts;
{$ELSE}
  TdsSSLConstsUni, TdsCertificateConstsUni;
{$ENDIF}
{$ELSE}
  ScDECUtil, ScSymmetricAlgorithm,
  ScConsts, ScCertificateConsts;
{$ENDIF}

const
  S_tls13_: array[0..5] of byte = (116, 108, 115, 49, 51, 32); // 'tls13 '
  S_key: array[0..2] of byte = (107, 101, 121); // 'key'
  S_iv: array[0..1] of byte = (105, 118);       // 'iv'
  S_traffic_upd: array[0..10] of byte = (116, 114, 97, 102, 102, 105, 99, 32, 117, 112, 100); // 'traffic upd'
  S_finished: array[0..7] of byte = (102, 105, 110, 105, 115, 104, 101, 100); // 'finished'
//  S_ext_binder: array[0..9] of byte = (101, 120, 116, 32, 98, 105, 110, 100, 101, 114); // 'ext binder'
  S_res_binder: array[0..9] of byte = (114, 101, 115, 32, 98, 105, 110, 100, 101, 114); // 'res binder'
//  S_c_e_traffic: array[0..10] of byte = (99, 32, 101, 32, 116, 114, 97, 102, 102, 105, 99); // 'c e traffic'
//  S_e_exp_master: array[0..11] of byte = (101, 32, 101, 120, 112, 32, 109, 97, 115, 116, 101, 114); // 'e exp master'
  S_derived: array[0..6] of byte = (100, 101, 114, 105, 118, 101, 100); // 'derived'
  S_c_hs_traffic: array[0..11] of byte = (99, 32, 104, 115, 32, 116, 114, 97, 102, 102, 105, 99); // 'c hs traffic'
  S_s_hs_traffic: array[0..11] of byte = (115, 32, 104, 115, 32, 116, 114, 97, 102, 102, 105, 99); // 's hs traffic'
  S_c_ap_traffic: array[0..11] of byte = (99, 32, 97, 112, 32, 116, 114, 97, 102, 102, 105, 99); // 'c ap traffic'
  S_s_ap_traffic: array[0..11] of byte = (115, 32, 97, 112, 32, 116, 114, 97, 102, 102, 105, 99); // 's ap traffic'
//  S_exp_master: array[0..9] of byte = (101, 120, 112, 32, 109, 97, 115, 116, 101, 114); // 'exp master'
  S_res_master: array[0..9] of byte = (114, 101, 115, 32, 109, 97, 115, 116, 101, 114); // 'res master'
  S_resumption: array[0..9] of byte = (114, 101, 115, 117, 109, 112, 116, 105, 111, 110); // 'resumption'
  S_TLS13_client_CertificateVerify: array[0..32] of byte =  // 'TLS 1.3, client CertificateVerify'
    (84, 76, 83, 32, 49, 46, 51, 44, 32, 99, 108, 105, 101, 110, 116, 32, 67, 101,
     114, 116, 105, 102, 105, 99, 97, 116, 101, 86, 101, 114, 105, 102, 121);
  S_TLS13_server_CertificateVerify: array[0..32] of byte =  // 'TLS 1.3, server CertificateVerify'
    (84, 76, 83, 32, 49, 46, 51, 44, 32, 115, 101, 114, 118, 101, 114, 32, 67, 101,
     114, 116, 105, 102, 105, 99, 97, 116, 101, 86, 101, 114, 105, 102, 121);

{ THMACDeriveKeyBytes }

constructor THMACDeriveKeyBytes.Create(HashAlgorithm: THashAlgorithmClass; const Secret, Info: TBytes);
begin
  inherited Create;

  if (HashAlgorithm = nil) or (Length(Secret) = 0) then
    raise EScError.Create(seInvalidInputArgs);

  FHMAC := THMAC.Create(HashAlgorithm, Secret);

  SetLength(FInfo, Length(Info) + 1);
  if Length(Info) > 0 then
    Move(Info[0], FInfo[0], Length(Info));
  FInfo[Length(Info)] := 1;

  Reset;
end;

destructor THMACDeriveKeyBytes.Destroy;
begin
  FHMAC.Free;
  FillChar(FInfo[0], Length(FInfo), 0);
  FillChar(FNextBytes[0], Length(FNextBytes), 0);
  FillChar(FTi[0], Length(FTi), 0);

  inherited;
end;

procedure THMACDeriveKeyBytes.Reset;
begin
  FInfo[Length(FInfo) - 1] := 0;
  FTi := nil; // T(0)
  FNextBytes := GetNextBytes;
  FReadCount := 0;
end;

function THMACDeriveKeyBytes.GetNextBytes: TBytes;
begin
  // calculate next T
  FInfo[Length(FInfo) - 1] := FInfo[Length(FInfo) - 1] + 1;
  if Length(FTi) > 0 then
    FHMAC.TransformBlock(FTi, 0, Length(FTi));
  FHMAC.TransformFinalBlock(FInfo, 0, Length(FInfo));
  FTi := FHMAC.Hash;
  FHMAC.Initialize;

  Result := FTi;
end;

function THMACDeriveKeyBytes.GetBytes(Count: integer): TBytes;
var
  Filled: integer;
begin
  if Count < 0 then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(Result, Count);
  Filled := 0;
  while Filled < Count do begin
    if (Filled + Length(FNextBytes) - FReadCount) >= Count then begin
      Buffer.BlockCopy(FNextBytes, FReadCount, Result, Filled, Count - Filled);
      FReadCount := FReadCount + Count - Filled;
      Filled := Length(Result);
    end
    else begin
      Buffer.BlockCopy(FNextBytes, FReadCount, Result, Filled, Length(FNextBytes) - FReadCount);
      Filled := Filled + Length(FNextBytes) - FReadCount;
      FNextBytes := GetNextBytes;
      FReadCount := 0;
    end;
  end;
end;

{ TTls13HandshakeProtocol }

function TTls13HandshakeProtocol.InitializeCipherSuite(const Definition: TCipherDefinition;
  const CipherPart: TCipherPart): TCipherSuite;
var
  BulkEnc, BulkDec: TSymmetricAlgorithm;
  LocalKey, RemoteKey, LocalIV, RemoteIV: TBytes;
begin
  if Definition.CipherMode <> cmGCM then
    raise EScError.Create(seInvalidInputArgs);

{$IFNDEF VER10P}
  SetLength(LocalKey, 0);
  SetLength(RemoteKey, 0);
  SetLength(LocalIV, 0);
  SetLength(RemoteIV, 0);
{$ENDIF}  

  // generate the cipher objects
  Result := TCipherSuite.Create;
  try
    if (CipherPart = cpLocal) or (CipherPart = cpBoth) then begin
      BulkEnc := TSymmetricAlgorithm(Definition.CipherAlgorithmClass.Create);
      try
        if FEntity = ceClient then begin
          LocalKey := HKDFExpandLabel(FClientTrafficSecret, S_key, nil, Definition.KeyMaterialLength);
          LocalIV := HKDFExpandLabel(FClientTrafficSecret, S_iv, nil, 12);
        end
        else begin
          LocalKey := HKDFExpandLabel(FServerTrafficSecret, S_key, nil, Definition.KeyMaterialLength);
          LocalIV := HKDFExpandLabel(FServerTrafficSecret, S_iv, nil, 12);
        end;

        BulkEnc.Mode := Definition.CipherMode;
        BulkEnc.Key := LocalKey;
      except
        BulkEnc.Free;
        raise;
      end;

      Result.Encryptor := BulkEnc;
      Result.LocalHasher := nil;
      Result.LocalIV := LocalIV;

      // clear sensitive data
      FillChar(LocalKey[0], Length(LocalKey), 0);
    end;

    if (CipherPart = cpRemote) or (CipherPart = cpBoth) then begin
      BulkDec := TSymmetricAlgorithm(Definition.CipherAlgorithmClass.Create);
      try
        if FEntity = ceClient then begin
          RemoteKey := HKDFExpandLabel(FServerTrafficSecret, S_key, nil, Definition.KeyMaterialLength);
          RemoteIV := HKDFExpandLabel(FServerTrafficSecret, S_iv, nil, 12);
        end
        else begin
          RemoteKey := HKDFExpandLabel(FClientTrafficSecret, S_key, nil, Definition.KeyMaterialLength);
          RemoteIV := HKDFExpandLabel(FClientTrafficSecret, S_iv, nil, 12);
        end;

        BulkDec.Mode := Definition.CipherMode;
        BulkDec.Key := RemoteKey;
      except
        BulkDec.Free;
        raise;
      end;

      Result.Decryptor := BulkDec;
      Result.RemoteHasher := nil;
      Result.RemoteIV := RemoteIV;

      // clear sensitive data
      FillChar(RemoteKey[0], Length(RemoteKey), 0);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TTls13HandshakeProtocol.HKDFExtract(const Salt, IKM{Input Keying Material}: TBytes): TBytes;
var
  HMAC: THMAC;
begin
  HMAC := THMAC.Create(GetHashAlgorithmClass, Salt);
  try
    Result := HMAC.ComputeHash(IKM);
  finally
    HMAC.Free;
  end;
end;

function TTls13HandshakeProtocol.HKDFExpand(const PRK, Info: TBytes; Size: integer): TBytes;
var
  HKDF: THMACDeriveKeyBytes;
begin
  HKDF := THMACDeriveKeyBytes.Create(GetHashAlgorithmClass, PRK, Info);
  try
    Result := HKDF.GetBytes(Size);
  finally
    HKDF.Free;
  end;
end;

function TTls13HandshakeProtocol.HKDFExpandLabel(const Secret: TBytes;
  const aLabel: array of byte; const Context: TBytes; Size: integer): TBytes;
//  struct HkdfLabel {
//    uint16 Size;
//    opaque label<7..255> = "tls13 " + Label;
//    opaque context<0..255>;
//  };
var
  HkdfLabel: TBytes;
  LabelLen: byte;
begin
  if Length(Context) > 255 then
    FOwner.SendAlert(adDecodeError, seInvalidInputArgs);

  LabelLen := byte(6{S_tls13_} + Length(aLabel)); // 'tls13 ' + aLabel1
  SetLength(HkdfLabel, 2{Size} + 1{len} + LabelLen + 1{len} + Length(Context));

  HkdfLabel[0] := Byte(Size shr 8);
  HkdfLabel[1] := Byte(Size);

  HkdfLabel[2] := LabelLen;
  Move(S_tls13_[0], HkdfLabel[3], 6{S_tls13_});
  Move(aLabel[0], HkdfLabel[3 + 6], Length(aLabel));

  HkdfLabel[3 + LabelLen] := Byte(Length(Context));
  if Length(Context) > 0 then
    Move(Context[0], HkdfLabel[4 + LabelLen], Length(Context));

  Result := HKDFExpand(Secret, HkdfLabel, Size);
end;

function TTls13HandshakeProtocol.EmptyStrHash: TBytes;
var
  HashAlg: THashAlgorithm;
begin
  if Length(FEmptyStrHash) = 0 then begin
    HashAlg := CreateHMACHash;
    try
      FEmptyStrHash := HashAlg.ComputeHash(nil, 0, 0);
    finally
      HashAlg.Free;
    end;
  end;

  Result := FEmptyStrHash;
end;

function TTls13HandshakeProtocol.DeriveSecret(const Secret: TBytes;
  const aLabel: array of byte; const HashMessages: TBytes): TBytes;
begin
  Result := HKDFExpandLabel(Secret, aLabel, HashMessages, Length(HashMessages));
end;

function TTls13HandshakeProtocol.GetHashSize: integer;
begin
  Result := CipherFactory.GetHashSize(SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm);
end;

procedure TTls13HandshakeProtocol.MakeMasterSecret(const Premaster, Random: TBytes; const aLabel: array of byte);
begin
  // None
end;

procedure TTls13HandshakeProtocol.CreateClientHello1Hash;
begin
  FClientHello1Hash := TranscriptHash;
end;

procedure TTls13HandshakeProtocol.TranscriptClientHello1Hash;
var
  hm: THandshakeMessage;
begin
  FNegotiationBufferPos := 0;
  hm := THandshakeMessage.Create;
  try
    hm.Init(htMessageHash);
    hm.WriteBuf0(FClientHello1Hash);
    StoreNegotiationData(hm);
  finally
    hm.Free;
  end;
end;

procedure TTls13HandshakeProtocol.MakeEarlySecret(const PreSharedKey: TBytes);
var
  EarlySecret: TBytes;
  HasPreSharedKey: boolean;
  PreSharedKeyBuf: TBytes;
begin
  HasPreSharedKey := Length(PreSharedKey) > 0;
  if HasPreSharedKey then
    PreSharedKeyBuf := PreSharedKey
  else
    SetLength(PreSharedKeyBuf, GetHashSize); // 0

  EarlySecret := HKDFExtract(nil{Salt}, PreSharedKeyBuf);

  if HasPreSharedKey then begin
    FResBinderKey := DeriveSecret(EarlySecret, S_res_binder, EmptyStrHash);
    FFinishedBinderKey := HKDFExpandLabel(FResBinderKey, S_finished, nil, GetHashSize);
  end;

//  FExtBinderKey := DeriveSecret(EarlySecret, S_ext_binder, EmptyStrHash);
//  FClientEarlyTrafficSecret := DeriveSecret(EarlySecret, S_c_e_traffic, FClientHello1Hash); // ClientHello
//  FEarlyExporterMasterSecret := DeriveSecret(EarlySecret, S_e_exp_master, FClientHello1Hash); // ClientHello

  FDerivedSecret := DeriveSecret(EarlySecret, S_derived, EmptyStrHash);
  FillChar(EarlySecret[0], Length(EarlySecret), 0);
end;

procedure TTls13HandshakeProtocol.MakeHandshakeTrafficSecret(const Premaster: TBytes);
var
  HandshakeSecret: TBytes;
  HashMessages: TBytes;
begin
  HandshakeSecret := HKDFExtract(FDerivedSecret, Premaster);

  HashMessages := TranscriptHash;
  FClientHandshakeTrafficSecret := DeriveSecret(HandshakeSecret, S_c_hs_traffic, HashMessages); // ClientHello+ServerHello
  FClientTrafficSecret := FClientHandshakeTrafficSecret;

  FServerHandshakeTrafficSecret := DeriveSecret(HandshakeSecret, S_s_hs_traffic, HashMessages); // ClientHello+ServerHello
  FServerTrafficSecret := FServerHandshakeTrafficSecret;

  FDerivedSecret := DeriveSecret(HandshakeSecret, S_derived, EmptyStrHash);
  FillChar(HandshakeSecret[0], Length(HandshakeSecret), 0);
end;

procedure TTls13HandshakeProtocol.MakeApplicationTrafficSecret;
var
  IKM, MasterSecret: TBytes;
  HashMessages: TBytes;
begin
  SetLength(IKM, GetHashSize); // 0
  MasterSecret := HKDFExtract(FDerivedSecret, IKM);

  HashMessages := TranscriptHash;
  FClientApplicationTrafficSecret := DeriveSecret(MasterSecret, S_c_ap_traffic, HashMessages); // ClientHello+ServerHello+...server Finished
  FClientTrafficSecret := FClientApplicationTrafficSecret;

  FServerApplicationTrafficSecret := DeriveSecret(MasterSecret, S_s_ap_traffic, HashMessages); // ClientHello+ServerHello+...server Finished
  FServerTrafficSecret := FServerApplicationTrafficSecret;

//  FExporterMasterSecret := DeriveSecret(MasterSecret, S_exp_master, HashMessages); // ClientHello+ServerHello+...server Finished

  FDerivedSecret := MasterSecret;
end;

procedure TTls13HandshakeProtocol.MakeResumptionMasterSecret;
var
  HashMessages: TBytes;
begin
  HashMessages := TranscriptHash;
  FResumptionMasterSecret := DeriveSecret(FDerivedSecret, S_res_master, HashMessages); // ClientHello+ServerHello+...client Finished

  FillChar(FDerivedSecret[0], Length(FDerivedSecret), 0);
  SetLength(FDerivedSecret, 0);
end;

procedure TTls13HandshakeProtocol.UpdateClientApplicationTrafficSecret;
var
  TrafficUpdSecret: TBytes;
begin
  TrafficUpdSecret := HKDFExpandLabel(FClientApplicationTrafficSecret, S_traffic_upd, nil, GetHashSize);
  FillChar(FClientApplicationTrafficSecret[0], Length(FClientApplicationTrafficSecret), 0);
  FClientApplicationTrafficSecret := TrafficUpdSecret;
  FClientTrafficSecret := TrafficUpdSecret;
end;

procedure TTls13HandshakeProtocol.UpdateServerApplicationTrafficSecret;
var
  TrafficUpdSecret: TBytes;
begin
  TrafficUpdSecret := HKDFExpandLabel(FServerApplicationTrafficSecret, S_traffic_upd, nil, GetHashSize);
  FillChar(FServerApplicationTrafficSecret[0], Length(FServerApplicationTrafficSecret), 0);
  FServerApplicationTrafficSecret := TrafficUpdSecret;
  FServerTrafficSecret := TrafficUpdSecret;
end;

procedure TTls13HandshakeProtocol.MakeCertificateRequestMessage(Message: THandshakeMessage;
  DNList: TScDistinguishedNameList);
begin
  SetLength(FLocalCertificateRequestContext, 16);
  Random.Random(FLocalCertificateRequestContext, 0, Length(FLocalCertificateRequestContext));

  Message.WriteBuf8(FLocalCertificateRequestContext);
  Message.WriteInt16(0); // Extensions
end;

function TTls13HandshakeProtocol.ParseCertificateRequestMessage(Message: THandshakeMessage): TScDistinguishedNameList;
var
  CRCLen: integer;
  Extensions: TTLSHelloExtensions;
begin
  Result := nil;
  CRCLen := Message.ReadInt8;
  FRemoteCertificateRequestContext := Message.Read(CRCLen);

  Extensions := TTLSHelloExtensions.Create;
  try
    Extensions.Parse(Message, FEntity);
  finally
    Extensions.Free;
  end;
end;

procedure TTls13HandshakeProtocol.WriteCertificateMessage(Message: THandshakeMessage;
  CertList: TCRList);
var
  Certificate: TScCertificate;
  RawData: TBytes;
  i, LenOffset: integer;
begin
  if CertList = nil then begin
    Message.WriteInt8(0);
    Message.WriteInt24(0);
    Exit;
  end;

  Message.WriteBuf8(FRemoteCertificateRequestContext);

  LenOffset := Message.WriteOffset;
  Message.SkipWrite(3); // Len

  SetLength(RawData, 0);
  for i := 0 to CertList.Count - 1 do begin
    Certificate := TScCertificate(CertList[i]);
    if Certificate = nil then
      Continue;

    RawData := Certificate.GetRawData;
    Message.WriteBuf24(RawData);
    Message.WriteInt16(0); // extensions length
  end;

  Message.WriteInt24ByOffset(Message.WriteOffset - LenOffset - 3, LenOffset);
end;

procedure TTls13HandshakeProtocol.ReadCertificateMessage(Message: THandshakeMessage;
  CertList: TCRList);
var
  Stream: TMemoryStream;
  Cert: TScCertificate;
  CertificateRequestContext: TBytes;
  Len: integer;
begin
  Len := Message.ReadInt8;
  CertificateRequestContext := Message.Read(Len);
  if Length(CertificateRequestContext) <> Length(FLocalCertificateRequestContext) then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);
  if (Length(CertificateRequestContext) > 0) and
     (MemCompare(@CertificateRequestContext[0], @FLocalCertificateRequestContext[0], Length(CertificateRequestContext)) <> 0)
  then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

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

      Len := Message.ReadInt16; // Extensions length
      Message.SkipRead(Len);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTls13HandshakeProtocol.WriteCertificateVerifyMessage(Message: THandshakeMessage;
  Certificate: TScCertificate);
var
  SignAlgsExt: TTLSSignatureAlgorithmsExtension;
  ss, SignatureScheme: TScSSLSignatureScheme;
  HashAlg: TScHashAlgorithm;
  Signature, Content, HandshakeHash: TBytes;
  ContextStrLen: integer;
  i: integer;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  SignatureScheme := ssRSA_PSS_PSS_SHA256;
  case Certificate.Key.Algorithm of
    aaRSA:
      if Certificate.SignatureAlgorithm.Algorithm.Value = OID_RSA_PSS_ENCRYPTION then
        SignatureScheme := ssRSA_PSS_PSS_SHA256
      else
        SignatureScheme := ssRSA_PSS_RSAE_SHA256;

    aaEC:
      SignatureScheme := ssECDSA_SECP256r1_SHA256;
  else
    FOwner.SendAlert(adUnsupportedCertificate, seCannotSignData);
  end;

  if FEntity = ceServer then begin
    SignAlgsExt := TTLSSignatureAlgorithmsExtension(FOwner.Options.ClientHelloExtensions.Find(TTLSSignatureAlgorithmsExtension));
    if SignAlgsExt <> nil then begin
      for i := 0 to SignAlgsExt.Count - 1 do begin
        ss := SignAlgsExt.SignatureSchemes[i];
        if (Certificate.Key.Algorithm = SCHEME_ALGORITHMS[ss].Signature) and
           (SCHEME_ALGORITHMS[ss].Hash <> haSHA1)
        then begin
          SignatureScheme := ss;
          Break;
        end;
      end;
    end;
  end;

  HandshakeHash := TranscriptHash;
  ContextStrLen := Length(S_TLS13_client_CertificateVerify);
  SetLength(Content, 64 + ContextStrLen + 1 + Length(HandshakeHash));
  FillChar(Content[0], 64, $20);

  if FEntity = ceClient then
    Move(S_TLS13_client_CertificateVerify[0], Content[64], ContextStrLen)
  else
    Move(S_TLS13_server_CertificateVerify[0], Content[64], ContextStrLen);

  Move(HandshakeHash[0], Content[64 + ContextStrLen + 1{#0}], Length(HandshakeHash));

  HashAlg := SCHEME_ALGORITHMS[SignatureScheme].Hash;
  if SCHEME_ALGORITHMS[SignatureScheme].Padding = pmPSS then begin
    Certificate.Key.PSSParams.HashAlgorithm := HashAlg;
    Certificate.Key.PSSParams.MaskGenHashAlgorithm := HashAlg;
    Certificate.Key.PSSParams.SaltLength := CipherFactory.GetHashSize(HashAlg);
  end;

  Signature := Certificate.Sign(Content, HashAlg, SCHEME_ALGORITHMS[SignatureScheme].Padding);

  Message.WriteInt16(SIGNATURE_SCHEME_CODES[SignatureScheme]);
  Message.WriteBuf16(Signature);
end;

procedure TTls13HandshakeProtocol.VerifyCertificateMessage(Message: THandshakeMessage;
  RemoteCertificate: TScCertificate);
var
  SignatureScheme: TScSSLSignatureScheme;
  HashAlg: TScHashAlgorithm;
  Signature, Content, HandshakeHash: TBytes;
  ContextStrLen, Size: integer;
begin
  if RemoteCertificate = nil then
    raise EScError.Create(sePeerCertificateNotReceived);

  SignatureScheme := TCipherSuites.ToSignatureScheme(Message.ReadInt16);
  if SignatureScheme = TScSSLSignatureScheme(-1) then
    FOwner.SendAlert(adDecryptError, seInvalidSignatureSchemeAlgorithm);

  Size := Message.ReadInt16;
  if Size = 0 then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

  Signature := Message.Read(Size);

  if SCHEME_ALGORITHMS[SignatureScheme].Signature <> RemoteCertificate.Key.Algorithm then
    FOwner.SendAlert(adDecryptError, seCertificateNotCorrespondToRequiredSignatureAlgorithms);

  if SCHEME_ALGORITHMS[SignatureScheme].Hash = haSHA1 then
    FOwner.SendAlert(adInsufficientSecurity, seInvalidHashAlgorithm);

  HandshakeHash := TranscriptHash;
  ContextStrLen := Length(S_TLS13_client_CertificateVerify);
  SetLength(Content, 64 + ContextStrLen + 1 + Length(HandshakeHash));
  FillChar(Content[0], 64, $20);

  if FEntity = ceClient then
    Move(S_TLS13_server_CertificateVerify[0], Content[64], ContextStrLen)
  else
    Move(S_TLS13_client_CertificateVerify[0], Content[64], ContextStrLen);

  Move(HandshakeHash[0], Content[64 + ContextStrLen + 1{#0}], Length(HandshakeHash));

  HashAlg := SCHEME_ALGORITHMS[SignatureScheme].Hash;
  if SCHEME_ALGORITHMS[SignatureScheme].Padding = pmPSS then begin
    RemoteCertificate.Key.PSSParams.HashAlgorithm := HashAlg;
    RemoteCertificate.Key.PSSParams.MaskGenHashAlgorithm := HashAlg;
    RemoteCertificate.Key.PSSParams.SaltLength := CipherFactory.GetHashSize(HashAlg);
  end;

  if not RemoteCertificate.VerifySign(Content, Signature, HashAlg, SCHEME_ALGORITHMS[SignatureScheme].Padding) then
    FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
end;

procedure TTls13HandshakeProtocol.WriteFinishedMessage(Message: THandshakeMessage);
var
  HMAC: THMAC;
  HandshakeHash, BaseKey, FinishedKey: TBytes;
begin
  HandshakeHash := TranscriptHash;

  if FEntity = ceClient then begin
    if Length(FClientHandshakeTrafficSecret) > 0 then
      BaseKey := FClientHandshakeTrafficSecret
    else
      BaseKey := FClientTrafficSecret;
  end
  else begin
    if Length(FServerHandshakeTrafficSecret) > 0 then
      BaseKey := FServerHandshakeTrafficSecret
    else
      BaseKey := FServerTrafficSecret;
  end;

  FinishedKey := HKDFExpandLabel(BaseKey, S_finished, nil, Length(HandshakeHash));
  HMAC := THMAC.Create(GetHashAlgorithmClass, FinishedKey);
  try
    Message.WriteBuf0(HMAC.ComputeHash(HandshakeHash));
  finally
    HMAC.Free;
  end;

  if FEntity = ceClient then
    SetLength(FClientHandshakeTrafficSecret, 0)
  else
    SetLength(FServerHandshakeTrafficSecret, 0);
end;

procedure TTls13HandshakeProtocol.VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer);
var
  HMAC: THMAC;
  HandshakeHash, BaseKey, FinishedKey, VerifyData: TBytes;
  i: integer;
begin
  HandshakeHash := TranscriptHash;

  if FEntity = ceClient then begin
    if Length(FServerHandshakeTrafficSecret) > 0 then
      BaseKey := FServerHandshakeTrafficSecret
    else
      BaseKey := FServerTrafficSecret;
  end
  else begin
    if Length(FClientHandshakeTrafficSecret) > 0 then
      BaseKey := FClientHandshakeTrafficSecret
    else
      BaseKey := FClientTrafficSecret;
  end;

  FinishedKey := HKDFExpandLabel(BaseKey, S_finished, nil, Length(HandshakeHash));
  HMAC := THMAC.Create(GetHashAlgorithmClass, FinishedKey);
  try
    VerifyData := HMAC.ComputeHash(HandshakeHash);
  finally
    HMAC.Free;
  end;

  if FEntity = ceClient then
    SetLength(FServerHandshakeTrafficSecret, 0)
  else
    SetLength(FClientHandshakeTrafficSecret, 0);

  if Length(VerifyData) <> Size then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

  for i := 0 to Size - 1 do
    if VerifyData[i] <> PeerFinished[Offset + i] then
      FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
end;

procedure TTls13HandshakeProtocol.MakeNewSessionTicketMessage(
  Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo);
var
  NewSessionTicket: TScNewSessionTicketExt;
begin
  SessionInfo.MasterSecret := FResumptionMasterSecret;

  NewSessionTicket := CreateNewSessionTicket(SessionInfo);
  try
    Message.WriteInt32(NewSessionTicket.Lifetime);
    Message.WriteInt32(NewSessionTicket.AgeAdd);
    Message.WriteBuf8(NewSessionTicket.Nonce);
    Message.WriteBuf16(NewSessionTicket.Ticket);
    Message.WriteInt16(0); // Extensions length
  finally
    NewSessionTicket.Free;
  end;
end;

procedure TTls13HandshakeProtocol.ParseNewSessionTicketMessage(
  Message: THandshakeMessage; SessionInfo: TScSSLSessionInfo);
var
  NewSessionTicket: TScNewSessionTicketExt;
  Size: integer;
begin
  NewSessionTicket := TScNewSessionTicketExt.Create;
  SessionInfo.NewSessionTickets.Add(NewSessionTicket);

  NewSessionTicket.Lifetime := Message.ReadInt32;
  NewSessionTicket.AgeAdd := Message.ReadInt32;
  Size := Message.ReadInt8;
  NewSessionTicket.Nonce := Message.Read(Size);
  Size := Message.ReadInt16;
  NewSessionTicket.Ticket := Message.Read(Size);
  NewSessionTicket.Extensions.Parse(Message, FEntity);

  NewSessionTicket.CreateTime := GetUnixTime;
  SessionInfo.MasterSecret := FResumptionMasterSecret;
end;

function TTls13HandshakeProtocol.CalcBinderKey(const MessageBuf: TBytes; Offset, Count: integer;
  const MasterSecret, TicketNonce: TBytes): TBytes;
var
  ha: THashAlgorithm;
  HMAC: THMAC;
  PreSharedKey, HashBuf: TBytes;
begin
  // https://tools.ietf.org/html/rfc8446#section-4.2.11.2

  PreSharedKey := HKDFExpandLabel(MasterSecret, S_resumption, TicketNonce, GetHashSize);
  MakeEarlySecret(PreSharedKey);

  ha := CreateHMACHash;
  try
    HashBuf := ha.ComputeHash(TValueArr(MessageBuf), Offset, Count);
  finally
    ha.Free;
  end;

  HMAC := THMAC.Create(GetHashAlgorithmClass, FFinishedBinderKey);
  try
    Result := HMAC.ComputeHash(HashBuf);
  finally
    HMAC.Free;
  end;
end;

procedure TTls13HandshakeProtocol.WriteBinderKey(ClientHelloMessage: THandshakeMessage;
  const MasterSecret, TicketNonce: TBytes);
var
  HashBuf: TBytes;
  StartOffset: integer;
begin
  StartOffset := ClientHelloMessage.DataOffset - HANDSHAKE_HEADER_LENGTH;
  HashBuf := CalcBinderKey(ClientHelloMessage.Fragment, StartOffset,
    ClientHelloMessage.WriteOffset - GetHashSize - 3{hash_len} - StartOffset, MasterSecret, TicketNonce);
  Move(HashBuf[0], ClientHelloMessage.Fragment[ClientHelloMessage.WriteOffset - Length(HashBuf)], Length(HashBuf));
end;

procedure TTls13HandshakeProtocol.DecodeAndVerifyTicket(ClientHelloMessage: THandshakeMessage;
  ClientHelloExtensions: TTLSHelloExtensions; NewSessionInfo: TScSSLSessionInfo;
  out SelectedIdentity: integer);
var
  NewSessionTicketC: TScNewSessionTicket;
  PreSharedKeyExtension: TTLSPreSharedKeyExtension;
  HashBuf: TBytes;
  BinderHashLen: integer;
  StartOffset: integer;
  i: integer;
begin
  SetLength(HashBuf, 0);
  PreSharedKeyExtension := TTLSPreSharedKeyExtension(ClientHelloExtensions.Find(TTLSPreSharedKeyExtension));

  for i := 0 to PreSharedKeyExtension.Count - 1 do begin
    NewSessionTicketC := PreSharedKeyExtension.NewSessionTickets[i];
    // find not expired ticket
    if not DecodeAndCheckTicket(NewSessionTicketC, NewSessionInfo) then
      continue;

    FOwner.SessionInfo.CipherAlgorithm := NewSessionInfo.CipherAlgorithm;
    BinderHashLen := PreSharedKeyExtension.Count * (GetHashSize + 1) + 2{hashes_len};

    // check PSK binder hash
    StartOffset := ClientHelloMessage.DataOffset - HANDSHAKE_HEADER_LENGTH;
    HashBuf := CalcBinderKey(ClientHelloMessage.Fragment, StartOffset,
      ClientHelloMessage.ReadOffset - BinderHashLen - StartOffset, NewSessionInfo.MasterSecret, NewSessionInfo.TicketNonce);

    if (Length(NewSessionTicketC.Hash) = Length(HashBuf)) and
       (MemCompare(@NewSessionTicketC.Hash[0], @HashBuf[0], Length(HashBuf)) = 0) then
    begin
      SelectedIdentity := i;
      Exit;
    end;

    SetLength(FDerivedSecret, 0); // to reset EarlySecret if Ticket was rejected
  end;

  SelectedIdentity := -1;
end;

function TTls13HandshakeProtocol.CreateHMACHash: THashAlgorithm;
begin
  Result := GetHashAlgorithmClass.Create;
end;

function TTls13HandshakeProtocol.GetHashAlgorithmClass: THashAlgorithmClass;
begin
  if SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm = haSHA2_384 then
    Result := THash_SHA2_384
  else
    Result := THash_SHA2_256;
end;

function TTls13HandshakeProtocol.GetProtocol: TScSSLProtocol;
begin
  Result := spTls13;
end;

end.

