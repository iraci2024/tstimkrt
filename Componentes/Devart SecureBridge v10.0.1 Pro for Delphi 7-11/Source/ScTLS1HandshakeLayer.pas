
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScTLS1HandshakeLayer;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRHash, CRHashAlgorithm, CRHMAC,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLTypes, TdsCipherSuites, TdsBridge,
  TdsLayers, TdsSSLMessages, TdsAlgorithmSupport;
{$ELSE}
  TdsUtilsUni, TdsSSLTypesUni, TdsCipherSuitesUni, TdsBridgeUni,
  TdsLayersUni, TdsSSLMessagesUni, TdsAlgorithmSupportUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScHash, ScHashAlgorithm, ScHMAC,
  ScUtils, ScSSLTypes, ScCipherSuites, ScBridge,
  ScLayers, ScSSLMessages, ScAlgorithmSupport;
{$ENDIF}

type
  { P_hash(secret, seed) = HMAC_hash(secret, A(1) + seed) +
                           HMAC_hash(secret, A(2) + seed) +
                           HMAC_hash(secret, A(3) + seed) + ...
     Where + indicates concatenation.
     A() is defined as:
       A(0) = seed
       A(i) = HMAC_hash(secret, A(i-1))
  }
  TExpansionDeriveBytes = class
  private
    FHMAC: THMAC;
    FHashSize: integer; // in bytes
    FSeed: TBytes;
    FNextBytes: TBytes;
    FReadCount: integer;
    FAi: TBytes;
  protected
    procedure Initialize(HashAlgorithm: THashAlgorithmClass; const Secret, Seed: TBytes);
    function GetNextBytes: TBytes;
  public
    constructor Create(HashAlgorithm: THashAlgorithmClass; const Secret, Seed: TBytes);
    destructor Destroy; override;

    function GetBytes(Count: integer): TBytes;
    procedure Reset;
  end;

  TPseudoRandomDeriveBytes = class
  protected
    FHashAlgorithm: TScHashAlgorithm;
    procedure Initialize(const aSecret: TBytes; const aLabel: array of byte; const aSeed: TBytes); virtual; abstract;

  public
    constructor Create(const aSecret: TBytes; const aLabel: array of byte;
      const aSeed: TBytes; HashAlgorithm: TScHashAlgorithm);

    function GetBytes(Count: integer): TBytes; virtual; abstract;
  end;

  { PRF(secret, label, seed) = P_MD5(S1, label + seed) XOR P_SHA1(S2, label + seed); }
  // SSL, TLS1_0, TLS1_1
  TPseudoRandomDeriveBytes1 = class(TPseudoRandomDeriveBytes)
  private
    FMD5: TExpansionDeriveBytes;
    FSHA1: TExpansionDeriveBytes;

  protected
    procedure Initialize(const aSecret: TBytes; const aLabel: array of byte; const aSeed: TBytes); override;

  public
    destructor Destroy; override;

    function GetBytes(Count: integer): TBytes; override;
  end;

  // TLS1_2
  TPseudoRandomDeriveBytes12 = class(TPseudoRandomDeriveBytes)
  private
    FHMAC: TExpansionDeriveBytes;

  protected
    procedure Initialize(const aSecret: TBytes; const aLabel: array of byte; const aSeed: TBytes); override;

  public
    destructor Destroy; override;

    function GetBytes(Count: integer): TBytes; override;
  end;

  TPseudoRandomDeriveBytesClass = class of TPseudoRandomDeriveBytes;

  TTlsCustomHandshakeProtocol = class(THandshakeProtocolService)
  protected
    function GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass; virtual; abstract;
  public
    function InitializeCipherSuite(const Definition: TCipherDefinition;
      const CipherPart: TCipherPart): TCipherSuite; override;

    procedure MakeMasterSecret(const Premaster, Random: TBytes; const aLabel: array of byte); override;
    procedure WriteFinishedMessage(Message: THandshakeMessage); override;
    procedure VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer); override;
  end;

  TTls10HandshakeProtocol = class(TTlsCustomHandshakeProtocol)
  protected
    function GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass; override;
  public
    function CreateHMACHash: THashAlgorithm; override;
    function GetProtocol: TScSSLProtocol; override;
  end;

  TTls11HandshakeProtocol = class(TTlsCustomHandshakeProtocol)
  protected
    function GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass; override;
  public
    function CreateHMACHash: THashAlgorithm; override;
    function GetProtocol: TScSSLProtocol; override;
  end;

  TTls12HandshakeProtocol = class(TTlsCustomHandshakeProtocol)
  private
    FRemoteAllowedSignatureSchemes: array of TScSSLSignatureScheme;
  protected
    function GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass; override;
  public
    procedure WriteCertificateVerifyMessage(Message: THandshakeMessage; Certificate: TScCertificate); override;
    procedure VerifyCertificateMessage(Message: THandshakeMessage; RemoteCertificate: TScCertificate); override;
    procedure WriteServerKeyExchangeSign(Message: THandshakeMessage; Certificate: TScCertificate); override;
    procedure VerifyServerKeyExchange(Message: THandshakeMessage;
      RemoteCertificate: TScCertificate); override;
    procedure WriteSignatureAndHash(Message: THandshakeMessage); override;
    procedure ReadSignatureAndHash(Message: THandshakeMessage); override;

    function CreateHMACHash: THashAlgorithm; override;
    function GetProtocol: TScSSLProtocol; override;
  end;

implementation

uses
{$IFNDEF SBRIDGE}
  CRDECUtil, CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsMD5SHA1CSP, TdsCertificateExts, TdsSSLExtensions;
{$ELSE}
  TdsSSLConstsUni, TdsMD5SHA1CSPUni, TdsCertificateExtsUni, TdsSSLExtensionsUni;
{$ENDIF}
{$ELSE}
  ScDECUtil, ScSymmetricAlgorithm,
  ScConsts, ScMD5SHA1CSP, ScCertificateExts, ScSSLExtensions;
{$ENDIF}

const
  S_key_expansion: array[0..12] of byte =              // 'key expansion'
    (107, 101, 121, 32, 101, 120, 112, 97, 110, 115, 105, 111, 110);
//  S_client_write_key: array[0..15] of byte =           // 'client write key'
//    (99, 108, 105, 101, 110, 116, 32, 119, 114, 105, 116, 101, 32, 107, 101, 121);
//  S_server_write_key: array[0..15] of byte =          // 'server write key'
//    (115, 101, 114, 118, 101, 114, 32, 119, 114, 105, 116, 101, 32, 107, 101, 121);
//  S_IV_block: array[0..7] of byte =                   // 'IV block'
//    (73, 86, 32, 98, 108, 111, 99, 107);
  S_client_finished: array[0..14] of byte =            // 'client finished'
    (99, 108, 105, 101, 110, 116, 32, 102, 105, 110, 105, 115, 104, 101, 100);
  S_server_finished: array[0..14] of byte =            // 'server finished'
    (115, 101, 114, 118, 101, 114, 32, 102, 105, 110, 105, 115, 104, 101, 100);

const
  Allowed_Signature_Schemes: array [0..12] of TScSSLSignatureScheme = (
    ssRSA_PKCS1_SHA256, ssRSA_PKCS1_SHA384, ssRSA_PKCS1_SHA512,
    ssECDSA_SECP256r1_SHA256, ssECDSA_SECP384r1_SHA384, ssECDSA_SECP521r1_SHA512,
    ssRSA_PSS_RSAE_SHA256, ssRSA_PSS_RSAE_SHA384, ssRSA_PSS_RSAE_SHA512,
    ssRSA_PSS_PSS_SHA256, ssRSA_PSS_PSS_SHA384, ssRSA_PSS_PSS_SHA512,
    ssEd25519
  );

{ TExpansionDeriveBytes }

constructor TExpansionDeriveBytes.Create(HashAlgorithm: THashAlgorithmClass; const Secret, Seed: TBytes);
begin
  inherited Create;
  Initialize(HashAlgorithm, Secret, Seed);
end;

destructor TExpansionDeriveBytes.Destroy;
begin
  FHMAC.Free;
  FillChar(FSeed[0], Length(FSeed), 0);
  FillChar(FNextBytes[0], Length(FNextBytes), 0);
  FillChar(FAi[0], Length(FAi), 0);

  inherited;
end;

procedure TExpansionDeriveBytes.Initialize(HashAlgorithm: THashAlgorithmClass; const Secret, Seed: TBytes);
begin
  if (HashAlgorithm = nil) or (Length(Seed) = 0) then
    raise EScError.Create(seInvalidInputArgs);

  FHMAC := THMAC.Create(HashAlgorithm, Secret);
  FSeed := Seed;
  FHashSize := FHMAC.HashSize;
  Reset;
end;

procedure TExpansionDeriveBytes.Reset;
begin
  FAi := FHMAC.ComputeHash(FSeed); // A(1)
  FNextBytes := GetNextBytes;
  FReadCount := 0;
end;

function TExpansionDeriveBytes.GetNextBytes: TBytes;
begin
  FHMAC.TransformBlock(FAi, 0, FHashSize);
  FHMAC.TransformFinalBlock(FSeed, 0, Length(FSeed));
  Result := FHMAC.Hash;
  FHMAC.Initialize;

  // calculate next A
  FAi := FHMAC.ComputeHash(FAi);
end;

function TExpansionDeriveBytes.GetBytes(Count: integer): TBytes;
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

{ TPseudoRandomDeriveBytes }

constructor TPseudoRandomDeriveBytes.Create(const aSecret: TBytes; const aLabel: array of byte;
  const aSeed: TBytes; HashAlgorithm: TScHashAlgorithm);
begin
  inherited Create;

  if Length(aLabel) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  FHashAlgorithm := HashAlgorithm;
  Initialize(aSecret, aLabel, aSeed);
end;

{ TPseudoRandomDeriveBytes1 }

destructor TPseudoRandomDeriveBytes1.Destroy;
begin
  FMD5.Free;
  FSHA1.Free;

  inherited;
end;

procedure TPseudoRandomDeriveBytes1.Initialize(const aSecret: TBytes;
  const aLabel: array of byte; const aSeed: TBytes);
var
  ls, s1, s2: TBytes;
  Len: integer;
begin
  // ls = label + seed
  SetLength(ls, Length(aLabel) + Length(aSeed));
  Move(aLabel[0], ls[0], Length(aLabel));
  Move(aSeed[0], ls[Length(aLabel)], Length(aSeed));

  // split the secret in two halves
  Len := Length(aSecret);
  if (Len mod 2) = 0 then
    Len := Len shr 1
  else
    Len := (Len shr 1) + 1;

  SetLength(s1, Len);
  SetLength(s2, Len);
  if Len > 0 then begin
    Buffer.BlockCopy(aSecret, 0, s1, 0, Len);
    Buffer.BlockCopy(aSecret, Length(aSecret) - Len, s2, 0, Len);
  end;

  // create TExpansionDeriveBytes objects
  FMD5 := TExpansionDeriveBytes.Create(THash_MD5, s1, ls);
  FSHA1 := TExpansionDeriveBytes.Create(THash_SHA1, s2, ls);
  FillChar(s1[0], Length(s1), 0);
  FillChar(s2[0], Length(s2), 0);
end;

function TPseudoRandomDeriveBytes1.GetBytes(Count: integer): TBytes;
var
  md5, sha1: TBytes;
  i: integer;
begin
  md5 := FMD5.GetBytes(Count);
  sha1 := FSHA1.GetBytes(Count);

  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
    Result[i] := md5[i] xor sha1[i];
end;

{ TPseudoRandomDeriveBytes12 }

destructor TPseudoRandomDeriveBytes12.Destroy;
begin
  FHMAC.Free;

  inherited;
end;

procedure TPseudoRandomDeriveBytes12.Initialize(const aSecret: TBytes;
  const aLabel: array of byte; const aSeed: TBytes);
var
  HashAlgorithm: THashAlgorithmClass;
  ls: TBytes;
begin
  // ls = label + seed
  SetLength(ls, Length(aLabel) + Length(aSeed));
  Move(aLabel[0], ls[0], Length(aLabel));
  Move(aSeed[0], ls[Length(aLabel)], Length(aSeed));

  if FHashAlgorithm = haSHA2_384 then // rfc5289
    HashAlgorithm := THash_SHA2_384
  else
    HashAlgorithm := THash_SHA2_256;

  // create ExpansionDeriveBytes objects
  FHMAC := TExpansionDeriveBytes.Create(HashAlgorithm, aSecret, ls);
end;

function TPseudoRandomDeriveBytes12.GetBytes(Count: integer): TBytes;
begin
  Result := FHMAC.GetBytes(Count);
end;

{ TTlsCustomHandshakeProtocol }

function TTlsCustomHandshakeProtocol.InitializeCipherSuite(const Definition: TCipherDefinition;
  const CipherPart: TCipherPart): TCipherSuite;
var
  BulkEnc, BulkDec: TSymmetricAlgorithm;
  ClientMac, ServerMac, ClientKey, ServerKey, ClientIV, ServerIV: TBytes;
  ServerClientRandom: TBytes;
  prf: TPseudoRandomDeriveBytes;
  BulkIVSize: integer;
begin
  // generate the cipher objects
  Result := TCipherSuite.Create;
  try
    BulkEnc := nil;
    BulkDec := nil;
    try
      BulkEnc := TSymmetricAlgorithm(Definition.CipherAlgorithmClass.Create);
      BulkDec := TSymmetricAlgorithm(Definition.CipherAlgorithmClass.Create);
      if Definition.CipherMode = cmECB then
        BulkIVSize := 0
      else
      if Definition.CipherMode = cmGCM then
        BulkIVSize := 4
      else
        BulkIVSize := BulkEnc.BlockSize;

      // get the keys and IVs
      SetLength(ServerClientRandom, 64);
      Buffer.BlockCopy(FOwner.ClientServerRandom, 32, ServerClientRandom, 0, 32);
      Buffer.BlockCopy(FOwner.ClientServerRandom, 0, ServerClientRandom, 32, 32);

      prf := GetPseudoRandomDeriveBytesClass.Create(FMasterSecret, S_key_expansion, ServerClientRandom, Definition.HashAlgorithm);
      try
        if Definition.CipherMode <> cmGCM then begin
          ClientMac := prf.GetBytes(CipherFactory.GetHashSize(Definition.HashAlgorithm));
          ServerMac := prf.GetBytes(CipherFactory.GetHashSize(Definition.HashAlgorithm));
        end
        else begin
          ClientMac := nil;
          ServerMac := nil;
        end;
        ClientKey := prf.GetBytes(Definition.KeyMaterialLength);
        ServerKey := prf.GetBytes(Definition.KeyMaterialLength);
        ClientIV := prf.GetBytes(BulkIVSize);
        ServerIV := prf.GetBytes(BulkIVSize);
      finally
        prf.Free;
      end;

      BulkEnc.Mode := Definition.CipherMode;
      BulkDec.Mode := Definition.CipherMode;

      BulkEnc.Key := ClientKey;
      BulkDec.Key := ServerKey;

      if Definition.CipherMode <> cmGCM then begin
        BulkEnc.IV := ClientIV;
        BulkDec.IV := ServerIV;
      end;

      if FEntity = ceClient then begin
        Result.Encryptor := BulkEnc;
        Result.Decryptor := BulkDec;
      end
      else begin
        Result.Encryptor := BulkDec;
        Result.Decryptor := BulkEnc;
      end;
    except
      BulkEnc.Free;
      BulkDec.Free;
      raise;
    end;

    if Definition.CipherMode <> cmGCM then begin
      if FEntity = ceClient then begin
        Result.LocalHasher := THMAC.Create(CipherFactory.HashClass(Definition.HashAlgorithm), ClientMac);
        Result.RemoteHasher := THMAC.Create(CipherFactory.HashClass(Definition.HashAlgorithm), ServerMac);
      end
      else begin
        Result.LocalHasher := THMAC.Create(CipherFactory.HashClass(Definition.HashAlgorithm), ServerMac);
        Result.RemoteHasher := THMAC.Create(CipherFactory.HashClass(Definition.HashAlgorithm), ClientMac);
      end;
    end
    else begin
      Result.LocalHasher := nil;
      Result.RemoteHasher := nil;

      if FEntity = ceClient then begin
        Result.LocalIV := ClientIV;
        Result.RemoteIV := ServerIV;
      end
      else begin
        Result.LocalIV := ServerIV;
        Result.RemoteIV := ClientIV;
      end;

      ClientIV := nil;
      ServerIV := nil;
    end;
  except
    Result.Free;
    raise;
  end;

  // clear sensitive data
  if Length(ClientMac) > 0 then
    FillChar(ClientMac[0], Length(ClientMac), 0);
  if Length(ServerMac) > 0 then
    FillChar(ServerMac[0], Length(ServerMac), 0);
  FillChar(ClientKey[0], Length(ClientKey), 0);
  FillChar(ServerKey[0], Length(ServerKey), 0);
  if Length(ClientIV) > 0 then
    FillChar(ClientIV[0], Length(ClientIV), 0);
  if Length(ServerIV) > 0 then
    FillChar(ServerIV[0], Length(ServerIV), 0);
  if Length(ServerClientRandom) > 0 then
    FillChar(ServerClientRandom[0], Length(ServerClientRandom), 0);
end;

procedure TTlsCustomHandshakeProtocol.MakeMasterSecret(const Premaster, Random: TBytes;
  const aLabel: array of byte);
var
  prf: TPseudoRandomDeriveBytes;
begin
  prf := GetPseudoRandomDeriveBytesClass.Create(Premaster, aLabel, Random, SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm);
  try
    FMasterSecret := prf.GetBytes(48);
  finally
    prf.Free;
  end;
end;

procedure TTlsCustomHandshakeProtocol.WriteFinishedMessage(Message: THandshakeMessage);
var
  prf: TPseudoRandomDeriveBytes;
  LocalHash: TBytes;
begin
  LocalHash := TranscriptHash;

  if FEntity = ceClient then
    prf := GetPseudoRandomDeriveBytesClass.Create(FMasterSecret, S_client_finished,
      LocalHash, SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm)
  else
    prf := GetPseudoRandomDeriveBytesClass.Create(FMasterSecret, S_server_finished,
      LocalHash, SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm);

  try
    Message.WriteBuf0(prf.GetBytes(12));
  finally
    prf.Free;
  end;
end;

procedure TTlsCustomHandshakeProtocol.VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer);
var
  prf: TPseudoRandomDeriveBytes;
  PrfBytes: TBytes;
  RemoteHash: TBytes;
  i: integer;
begin
  if Size <> 12 then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

  RemoteHash := TranscriptHash;

  if FEntity = ceClient then
    prf := GetPseudoRandomDeriveBytesClass.Create(FMasterSecret, S_server_finished,
      RemoteHash, SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm)
  else
    prf := GetPseudoRandomDeriveBytesClass.Create(FMasterSecret, S_client_finished,
      RemoteHash, SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm);

  try
    PrfBytes := prf.GetBytes(12);
  finally
    prf.Free;
  end;

  for i := 0 to Length(PrfBytes) - 1 do
    if PrfBytes[i] <> PeerFinished[Offset + i] then
      FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
end;

{ TTls10HandshakeProtocol }

function TTls10HandshakeProtocol.GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass;
begin
  Result := TPseudoRandomDeriveBytes1;
end;

function TTls10HandshakeProtocol.CreateHMACHash: THashAlgorithm;
begin
  Result := TMD5SHA1CryptoServiceProvider.Create;
  TMD5SHA1CryptoServiceProvider(Result).Protocol := spTls1;
end;

function TTls10HandshakeProtocol.GetProtocol: TScSSLProtocol;
begin
  Result := spTls1;
end;

{ TTls11HandshakeProtocol }

function TTls11HandshakeProtocol.GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass;
begin
  Result := TPseudoRandomDeriveBytes1;
end;

function TTls11HandshakeProtocol.CreateHMACHash: THashAlgorithm;
begin
  Result := TMD5SHA1CryptoServiceProvider.Create;
  TMD5SHA1CryptoServiceProvider(Result).Protocol := spTls1;
end;

function TTls11HandshakeProtocol.GetProtocol: TScSSLProtocol;
begin
  Result := spTls11;
end;

{ TTls12HandshakeProtocol }

procedure TTls12HandshakeProtocol.WriteCertificateVerifyMessage(Message: THandshakeMessage;
  Certificate: TScCertificate);
var
  SignatureScheme: TScSSLSignatureScheme;
  HashAlg: TScHashAlgorithm;
  csp: THashAlgorithm;
  Found: boolean;
  SignBuf, SignHash: TBytes;
  i: integer;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  SignatureScheme := ssRSA_PKCS1_SHA1;
  Found := False;
  for i := 0 to Length(FRemoteAllowedSignatureSchemes) - 1 do begin
    if (Certificate.Key.Algorithm = SCHEME_ALGORITHMS[FRemoteAllowedSignatureSchemes[i]].Signature) and
        // https://tools.ietf.org/html/rfc8446#section-4.2.3
        // RSASSA-PSS RSAE  vs  RSASSA-PSS PSS
       ((SCHEME_ALGORITHMS[FRemoteAllowedSignatureSchemes[i]].KeyPadding = {$IFNDEF SBRIDGE}{$IFNDEF UNIDACPRO}TdsCertificateExts{$ELSE}TdsCertificateExtsUni{$ENDIF}{$ELSE}ScCertificateExts{$ENDIF}.pmNone) or
        (SCHEME_ALGORITHMS[FRemoteAllowedSignatureSchemes[i]].KeyPadding = Certificate.SignatureAlgorithm.PaddingMode))
    then begin
      SignatureScheme := FRemoteAllowedSignatureSchemes[i];
      Found := True;
      Break;
    end;
  end;

  if not Found then
    FOwner.SendAlert(adUnsupportedCertificate, seCertificateNotCorrespondToCertificateRequest);

  HashAlg := SCHEME_ALGORITHMS[SignatureScheme].Hash;
  if SCHEME_ALGORITHMS[SignatureScheme].Padding = pmPSS then begin
    Certificate.Key.PSSParams.HashAlgorithm := HashAlg;
    Certificate.Key.PSSParams.MaskGenHashAlgorithm := HashAlg;
    Certificate.Key.PSSParams.SaltLength := CipherFactory.GetHashSize(HashAlg);
  end;

  csp := CipherFactory.CreateHash(HashAlg);
  try
    SignHash := csp.ComputeHash(TValueArr(FNegotiationBuffer), 0, FNegotiationBufferPos);
  finally
    csp.Free;
  end;

  SignBuf := Certificate.SignHash(SignHash, HashAlg, SCHEME_ALGORITHMS[SignatureScheme].Padding);

  Message.WriteInt16(SIGNATURE_SCHEME_CODES[SignatureScheme]);
  Message.WriteBuf16(SignBuf);
end;

procedure TTls12HandshakeProtocol.VerifyCertificateMessage(Message: THandshakeMessage;
  RemoteCertificate: TScCertificate);
var
  SignatureScheme: TScSSLSignatureScheme;
  HashAlg: TScHashAlgorithm;
  csp: THashAlgorithm;
  DataHash, Signature: TBytes;
  Size: integer;
  IsAllowed: boolean;
  i: integer;
begin
  if RemoteCertificate = nil then
    raise EScError.Create(sePeerCertificateNotReceived);

  SignatureScheme := TCipherSuites.ToSignatureScheme(Message.ReadInt16);
  if SignatureScheme = TScSSLSignatureScheme(-1) then
    FOwner.SendAlert(adDecryptError, seInvalidSignatureSchemeAlgorithm);

  if SCHEME_ALGORITHMS[SignatureScheme].Signature <> RemoteCertificate.Key.Algorithm then
    FOwner.SendAlert(adDecryptError, seCertificateNotCorrespondToRequiredSignatureAlgorithms);

  IsAllowed := False;
  for i := 0 to Length(Allowed_Signature_Schemes) - 1 do begin
    if SignatureScheme = Allowed_Signature_Schemes[i] then begin
      IsAllowed := True;
      Break;
    end;
  end;

  if not IsAllowed then
    FOwner.SendAlert(adInsufficientSecurity, seCertificateNotCorrespondToRequiredSignatureAlgorithms);

  HashAlg := SCHEME_ALGORITHMS[SignatureScheme].Hash;
  if SCHEME_ALGORITHMS[SignatureScheme].Padding = pmPSS then begin
    RemoteCertificate.Key.PSSParams.HashAlgorithm := HashAlg;
    RemoteCertificate.Key.PSSParams.MaskGenHashAlgorithm := HashAlg;
    RemoteCertificate.Key.PSSParams.SaltLength := CipherFactory.GetHashSize(HashAlg);
  end;

  Size := Message.ReadInt16;
  if Size = 0 then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

  Signature := Message.Read(Size);

  csp := CipherFactory.CreateHash(HashAlg);
  try
    DataHash := csp.ComputeHash(TValueArr(FNegotiationBuffer), 0, FNegotiationBufferPos);
  finally
    csp.Free;
  end;

  if not RemoteCertificate.VerifyHashSign(DataHash, Signature, HashAlg, SCHEME_ALGORITHMS[SignatureScheme].Padding) then
    FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
end;

procedure TTls12HandshakeProtocol.WriteServerKeyExchangeSign(Message: THandshakeMessage;
  Certificate: TScCertificate);
var
  SignAlgsExt: TTLSSignatureAlgorithmsExtension;
  ss, SignatureScheme: TScSSLSignatureScheme;
  csp: THashAlgorithm;
  SignBuf: TBytes;
  i: integer;
begin
  if Certificate = nil then
    raise EScError.Create(seServerCertificateNotSpecified);

  SignatureScheme := ssRSA_PKCS1_SHA256;
  case Certificate.Key.Algorithm of
    aaRSA:
      SignatureScheme := ssRSA_PKCS1_SHA256;
    aaEC:
      SignatureScheme := ssECDSA_SECP256r1_SHA256;
  else
    FOwner.SendAlert(adUnsupportedCertificate, seCannotSignData);
  end;

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

  csp := CipherFactory.CreateHash(SCHEME_ALGORITHMS[SignatureScheme].Hash);
  try
    csp.TransformBlock(FOwner.ClientServerRandom, 0, 64);
    csp.TransformFinalBlock(Message.Fragment, Message.DataOffset, Message.WriteOffset - Message.DataOffset);

    SignBuf := Certificate.SignHash(csp.Hash, SCHEME_ALGORITHMS[SignatureScheme].Hash, SCHEME_ALGORITHMS[SignatureScheme].Padding);
  finally
    csp.Free;
  end;

  Message.WriteInt16(SIGNATURE_SCHEME_CODES[SignatureScheme]);
  Message.WriteBuf16(SignBuf);
end;

procedure TTls12HandshakeProtocol.VerifyServerKeyExchange(Message: THandshakeMessage;
  RemoteCertificate: TScCertificate);
var
  SignatureScheme: TScSSLSignatureScheme;
  HashAlg: TScHashAlgorithm;
  csp: THashAlgorithm;
  Signature: TBytes;
  EndDataOffset, Size: integer;
begin
  if RemoteCertificate = nil then
    raise EScError.Create(seServerCertificateNotReceived);

  EndDataOffset := Message.ReadOffset;

  SignatureScheme := TCipherSuites.ToSignatureScheme(Message.ReadInt16);
  if SignatureScheme = TScSSLSignatureScheme(-1) then
    FOwner.SendAlert(adDecryptError, seInvalidSignatureSchemeAlgorithm);

  if SCHEME_ALGORITHMS[SignatureScheme].Signature <> RemoteCertificate.Key.Algorithm then
    FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);

  Size := Message.ReadInt16;
  Signature := Message.Read(Size); // holds the signature returned by the server

  HashAlg := SCHEME_ALGORITHMS[SignatureScheme].Hash;
  if SCHEME_ALGORITHMS[SignatureScheme].Padding = pmPSS then begin
    RemoteCertificate.Key.PSSParams.HashAlgorithm := HashAlg;
    RemoteCertificate.Key.PSSParams.MaskGenHashAlgorithm := HashAlg;
    RemoteCertificate.Key.PSSParams.SaltLength := CipherFactory.GetHashSize(HashAlg);
  end;

  csp := CipherFactory.CreateHash(HashAlg);
  try
    csp.TransformBlock(FOwner.ClientServerRandom, 0, 64);
    csp.TransformFinalBlock(Message.Fragment, Message.DataOffset, EndDataOffset - Message.DataOffset);

    if not RemoteCertificate.VerifyHashSign(csp.Hash, Signature, HashAlg, SCHEME_ALGORITHMS[SignatureScheme].Padding) then
      FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
  finally
    csp.Free;
  end;
end;

procedure TTls12HandshakeProtocol.WriteSignatureAndHash(Message: THandshakeMessage);
var
  i: integer;
begin
  Message.WriteInt16(Length(Allowed_Signature_Schemes) * 2{word});

  for i := 0 to Length(Allowed_Signature_Schemes) - 1 do
    Message.WriteInt16(SIGNATURE_SCHEME_CODES[Allowed_Signature_Schemes[i]]);
end;

procedure TTls12HandshakeProtocol.ReadSignatureAndHash(Message: THandshakeMessage);
var
  DataSize: integer;
  i: integer;
begin
  DataSize := Message.ReadInt16;
  if DataSize > 0 then begin
    DataSize := DataSize div 2;
    SetLength(FRemoteAllowedSignatureSchemes, DataSize);
    i := 0;
    while i < DataSize do begin
      FRemoteAllowedSignatureSchemes[i] := TCipherSuites.ToSignatureScheme(Message.ReadInt16);
      if FRemoteAllowedSignatureSchemes[i] = TScSSLSignatureScheme(-1) then
        Dec(DataSize)
      else
        Inc(i);
    end;
    SetLength(FRemoteAllowedSignatureSchemes, DataSize);
  end;
end;

function TTls12HandshakeProtocol.GetPseudoRandomDeriveBytesClass: TPseudoRandomDeriveBytesClass;
begin
  Result := TPseudoRandomDeriveBytes12;
end;

function TTls12HandshakeProtocol.CreateHMACHash: THashAlgorithm;
begin
  if SSLCipherDefinitions[GetCipherAlgorithm].HashAlgorithm = haSHA2_384 then // rfc5289
    Result := THash_SHA2_384.Create
  else
    Result := THash_SHA2_256.Create;
end;

function TTls12HandshakeProtocol.GetProtocol: TScSSLProtocol;
begin
  Result := spTls12;
end;

end.

