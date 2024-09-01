
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSSL3HandshakeLayer;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRHash, CRHashAlgorithm, CRHMAC,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLTypes, TdsCipherSuites,
  TdsLayers, TdsSSLMessages, TdsAlgorithmSupport;
{$ELSE}
  TdsUtilsUni, TdsSSLTypesUni, TdsCipherSuitesUni,
  TdsLayersUni, TdsSSLMessagesUni, TdsAlgorithmSupportUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScHash, ScHashAlgorithm,
  ScUtils, ScSSLTypes, ScHMAC, ScCipherSuites,
  ScLayers, ScSSLMessages, ScAlgorithmSupport;
{$ENDIF}

type
  { master_secret =
    MD5(pre_master_secret + SHA('A' + pre_master_secret +
      ClientHello.random + ServerHello.random)) +
    MD5(pre_master_secret + SHA('BB' + pre_master_secret +
      ClientHello.random + ServerHello.random)) +
    MD5(pre_master_secret + SHA('CCC' + pre_master_secret +
      ClientHello.random + ServerHello.random));
  }
  TSsl3DeriveBytes = class
  private
    FSecret: TBytes;
    FNextBytes: TBytes;
    FReadCount: Integer;
    FRandom: TBytes;
    FMD5: THash_MD5;
    FSHA1: THash_SHA1;
    FIteration: Integer;
  protected
    function GetNextBytes: TBytes;
  public
    constructor Create(const Secret, Random: TBytes; ClientServer: Boolean);
    destructor Destroy; override;

    function GetBytes(cb: Integer): TBytes;
    procedure Reset;
  end;

  // hash(MAC_write_secret + pad_2 +
  //   hash (MAC_write_secret + pad_1 + seq_num + length + content));
  TSsl3RecordMAC = class(TKeyedHashAlgorithm)
  private
    FHashAlgorithm: THashAlgorithm;
    FIsHashing: Boolean;
    FPadding_36, FPadding_5C: TBytes;
  protected
    function Get_HashSize: Integer; override;
    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create(HashAlg: TScHashAlgorithm; const rgbKey: TBytes); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
  end;

  // hash(master_secret + pad2 + hash(handshake_messages +
  //      Sender + master_secret + pad1));
  TSsl3HandshakeMac = class(TKeyedHashAlgorithm)
  private
    FHashAlgorithm: THashAlgorithm;
    FPadding_36, FPadding_5C: TBytes;
  protected
    function Get_HashSize: Integer; override;
    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create(HashAlg: TScHashAlgorithm; Hash: THashAlgorithm; const rgbKey: TBytes); reintroduce;
    procedure Initialize; override;
  end;

  TSsl3HandshakeProtocol = class(THandshakeProtocolService)
  public
    function InitializeCipherSuite(const Definition: TCipherDefinition;
      const CipherPart: TCipherPart): TCipherSuite; override;

    function CreateHMACHash: THashAlgorithm; override;
    procedure MakeMasterSecret(const Premaster, Random: TBytes; const aLabel: array of byte); override;
    procedure WriteFinishedMessage(Message: THandshakeMessage); override;
    procedure VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer); override;
    function GetProtocol: TScSSLProtocol; override;
  end;

implementation

uses
{$IFNDEF SBRIDGE}
  CRDECUtil, CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsMD5SHA1CSP, TdsSSLConsts;
{$ELSE}
  TdsMD5SHA1CSPUni, TdsSSLConstsUni;
{$ENDIF}
{$ELSE}
  ScDECUtil, ScSymmetricAlgorithm,
  ScMD5SHA1CSP, ScConsts;
{$ENDIF}

const
  FinalClientBlock: array[0..3] of byte = ($43, $4C, $4E, $54);
  FinalServerBlock: array[0..3] of byte = ($53, $52, $56, $52);

{ TSsl3DeriveBytes }

// ClientServer: true if random bytes should be processed as first the client bytes, then the server bytes
//               false otherwise
constructor TSsl3DeriveBytes.Create(const Secret, Random: TBytes; ClientServer: Boolean);
begin
  inherited Create;

  if Length(Random) <> 64 then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(FSecret, Length(Secret));
  if Length(Secret) > 0 then
    Buffer.BlockCopy(Secret, 0, FSecret, 0, Length(Secret));

  SetLength(FRandom, 64);
  if ClientServer then
    Buffer.BlockCopy(Random, 0, FRandom, 0, 64)
  else begin
    Buffer.BlockCopy(Random, 0, FRandom, 32, 32); // client to server random
    Buffer.BlockCopy(Random, 32, FRandom, 0, 32); // server to client random
  end;

  FMD5 := THash_MD5.Create;
  FSHA1 := THash_SHA1.Create;
  Reset;
end;

destructor TSsl3DeriveBytes.Destroy;
begin
  FMD5.Free;
  FSHA1.Free;

  FillChar(FSecret[0], Length(FSecret), 0);
  FillChar(FNextBytes[0], Length(FNextBytes), 0);
  FillChar(FRandom[0], Length(FRandom), 0);

  inherited;
end;

function TSsl3DeriveBytes.GetNextBytes: TBytes;
var
  buf: TBytes;
  i: Integer;
begin
  if FIteration > 26 then
    raise EScError.Create(seSsl3PsepdoRandomError);

  SetLength(buf, FIteration);
  for i := 0 to FIteration - 1 do
    buf[i] := Byte(64{'A'} + FIteration);

  FSHA1.TransformBlock(buf, 0, Length(buf));
  FSHA1.TransformBlock(FSecret, 0, Length(FSecret));
  FSHA1.TransformFinalBlock(FRandom, 0, Length(FRandom));
  FMD5.TransformBlock(FSecret, 0, Length(FSecret));
  FMD5.TransformFinalBlock(FSHA1.Hash, 0, 20);

  // finalize
  Result := FMD5.Hash;
  FSHA1.Initialize;
  FMD5.Initialize;
  Inc(FIteration);
end;

function TSsl3DeriveBytes.GetBytes(cb: Integer): TBytes;
var
  Filled: Integer;
begin
  if cb < 0 then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(Result, cb);
  Filled := 0;
  while Filled < Length(Result) do begin
    if (Filled + Length(FNextBytes) - FReadCount) >= cb then begin
      Buffer.BlockCopy(FNextBytes, FReadCount, Result, Filled, cb - Filled);
      FReadCount := FReadCount + cb - Filled;
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

procedure TSsl3DeriveBytes.Reset;
begin
  FIteration := 1;
  FNextBytes := GetNextBytes;
  FReadCount := 0;
end;

{ TSsl3RecordMAC }

constructor TSsl3RecordMAC.Create(HashAlg: TScHashAlgorithm; const rgbKey: TBytes);
var
  FPadSize: Integer;
begin
  inherited Create;

  if Length(rgbKey) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  if HashAlg = haMD5 then begin
    FHashAlgorithm := THash_MD5.Create;
    FPadSize := 48;
  end
  else if HashAlg = haSHA1 then begin
    FHashAlgorithm := THash_SHA1.Create;
    FPadSize := 40;
  end
  else begin
    FPadSize := 0;
    Assert(False);
  end;

  SetLength(FPadding_36, FPadSize);
  SetLength(FPadding_5C, FPadSize);
  FillChar(FPadding_36[0], FPadSize, $36);
  FillChar(FPadding_5C[0], FPadSize, $5C);

  SetLength(FKeyValue, Length(rgbKey));
  Buffer.BlockCopy(rgbKey, 0, FKeyValue, 0, Length(rgbKey));
  Initialize;
end;

destructor TSsl3RecordMAC.Destroy;
begin
  FHashAlgorithm.Free;
  inherited;
end;

procedure TSsl3RecordMAC.Initialize;
begin
  FHashAlgorithm.Initialize;
  FIsHashing := False;
  FState := 0;
end;

procedure TSsl3RecordMAC.HashCore(const Data: TValueArr; Offset, Count: Integer);
begin
  if not FIsHashing then begin
    FHashAlgorithm.TransformBlock(FKeyValue, 0, Length(FKeyValue));
    FHashAlgorithm.TransformBlock(FPadding_36, 0, Length(FPadding_36));
    FIsHashing := True;
  end;

  FHashAlgorithm.TransformBlock(Data, Offset, Count, Data, Offset);
end;

function TSsl3RecordMAC.HashFinal: TBytes;
var
  DataHash: TBytes;
begin
  FHashAlgorithm.TransformFinalBlock(nil, 0, 0); // finalize inner hash
  DataHash := FHashAlgorithm.Hash;

  FHashAlgorithm.Initialize;
  FHashAlgorithm.TransformBlock(FKeyValue, 0, Length(FKeyValue));
  FHashAlgorithm.TransformBlock(FPadding_5C, 0, Length(FPadding_5C));
  FHashAlgorithm.TransformFinalBlock(DataHash, 0, Length(DataHash));
  FIsHashing := False; // allow key change
  Result := FHashAlgorithm.Hash;
end;

function TSsl3RecordMAC.get_HashSize: Integer;
begin
  Result := FHashAlgorithm.HashSize;
end;

{ TSsl3HandshakeMac }

constructor TSsl3HandshakeMac.Create(HashAlg: TScHashAlgorithm; Hash: THashAlgorithm; const rgbKey: TBytes);
var
  FPadSize: Integer;
begin
  inherited Create;

  if Length(rgbKey) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  if HashAlg = haMD5 then
    FPadSize := 48
  else if HashAlg = haSHA1 then
    FPadSize := 40
  else begin
    FPadSize := 0;
    Assert(False);
  end;

  SetLength(FPadding_36, FPadSize);
  SetLength(FPadding_5C, FPadSize);
  FillChar(FPadding_36[0], FPadSize, $36);
  FillChar(FPadding_5C[0], FPadSize, $5C);

  FHashAlgorithm := Hash;
  SetLength(FKeyValue, Length(rgbKey));
  Buffer.BlockCopy(rgbKey, 0, FKeyValue, 0, Length(rgbKey));
end;

procedure TSsl3HandshakeMac.Initialize;
begin
  FHashAlgorithm.Initialize;
  FState := 0;
end;

procedure TSsl3HandshakeMac.HashCore(const Data: TValueArr; Offset, Count: Integer);
begin
  FHashAlgorithm.TransformBlock(Data, Offset, Count, Data, Offset);
end;

function TSsl3HandshakeMac.HashFinal: TBytes;
var
  DataHash: TBytes;
begin
  FHashAlgorithm.TransformBlock(FKeyValue, 0, Length(FKeyValue));
  FHashAlgorithm.TransformFinalBlock(FPadding_36, 0, Length(FPadding_36)); // finalize inner hash
  DataHash := FHashAlgorithm.Hash;

  FHashAlgorithm.Initialize;
  FHashAlgorithm.TransformBlock(FKeyValue, 0, Length(FKeyValue));
  FHashAlgorithm.TransformBlock(FPadding_5C, 0, Length(FPadding_5C));
  FHashAlgorithm.TransformFinalBlock(DataHash, 0, Length(DataHash));

  Result := FHashAlgorithm.Hash;
end;

function TSsl3HandshakeMac.get_HashSize: Integer;
begin
  Result := FHashAlgorithm.HashSize;
end;

{ TSsl3HandshakeProtocol }

function TSsl3HandshakeProtocol.InitializeCipherSuite(const Definition: TCipherDefinition;
  const CipherPart: TCipherPart): TCipherSuite;
var
  BulkEnc, BulkDec: TSymmetricAlgorithm;
  ClientMac, ServerMac, ClientKey, ServerKey, ClientIV, ServerIV: TBytes;
  prf: TSsl3DeriveBytes;
  BulkIVSize: integer;
begin
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
        BulkIVSize := BulkEnc.BlockSize;

      // get the keys and IVs
      prf := TSsl3DeriveBytes.Create(FMasterSecret, FOwner.ClientServerRandom, False);
      try
        ClientMac := prf.GetBytes(CipherFactory.GetHashSize(Definition.HashAlgorithm));
        ServerMac := prf.GetBytes(CipherFactory.GetHashSize(Definition.HashAlgorithm));
        ClientKey := prf.GetBytes(Definition.KeyMaterialLength);
        ServerKey := prf.GetBytes(Definition.KeyMaterialLength);
        ClientIV := prf.GetBytes(BulkIVSize);
        ServerIV := prf.GetBytes(BulkIVSize);
      finally
        prf.Free;
      end;

      BulkEnc.Mode := Definition.CipherMode;
      BulkEnc.Key := ClientKey;
      BulkEnc.IV := ClientIV;

      BulkDec.Mode := Definition.CipherMode;
      BulkDec.Key := ServerKey;
      BulkDec.IV := ServerIV;

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

    // generate the cipher objects
    if FEntity = ceClient then begin
      Result.LocalHasher := TSsl3RecordMAC.Create(Definition.HashAlgorithm, ClientMac);
      Result.RemoteHasher := TSsl3RecordMAC.Create(Definition.HashAlgorithm, ServerMac);
    end
    else begin
      Result.LocalHasher := TSsl3RecordMAC.Create(Definition.HashAlgorithm, ServerMac);
      Result.RemoteHasher := TSsl3RecordMAC.Create(Definition.HashAlgorithm, ClientMac);
    end;
  except
    Result.Free;
    raise;
  end;

  // clear sensitive data
  FillChar(ClientMac[0], Length(ClientMac), 0);
  FillChar(ServerMac[0], Length(ServerMac), 0);
  FillChar(ClientKey[0], Length(ClientKey), 0);
  FillChar(ServerKey[0], Length(ServerKey), 0);
  if Length(ClientIV) > 0 then
    FillChar(ClientIV[0], Length(ClientIV), 0);
  if Length(ServerIV) > 0 then
    FillChar(ServerIV[0], Length(ServerIV), 0);
end;

function TSsl3HandshakeProtocol.CreateHMACHash: THashAlgorithm;
begin
  Result := TMD5SHA1CryptoServiceProvider.Create;
  TMD5SHA1CryptoServiceProvider(Result).Protocol := spSsl3;
end;

procedure TSsl3HandshakeProtocol.MakeMasterSecret(const Premaster, Random: TBytes;
  const aLabel: array of byte);
var
  prf: TSsl3DeriveBytes;
begin
  prf := TSsl3DeriveBytes.Create(Premaster, Random, True);
  try
    FMasterSecret := prf.GetBytes(48);
  finally
    prf.Free;
  end;
end;

procedure TSsl3HandshakeProtocol.WriteFinishedMessage(Message: THandshakeMessage);
var
  LocalHash: TMD5SHA1CryptoServiceProvider;
begin
  LocalHash := TMD5SHA1CryptoServiceProvider.Create;
  try
    LocalHash.Protocol := spSsl3;
    LocalHash.MasterKey := FMasterSecret;
    LocalHash.TransformBlock(FNegotiationBuffer, 0, FNegotiationBufferPos);

    if FEntity = ceClient then
      LocalHash.TransformFinalBlock(@FinalClientBlock[0], Length(FinalClientBlock))
    else
      LocalHash.TransformFinalBlock(@FinalServerBlock[0], Length(FinalServerBlock));

    Message.WriteBuf0(LocalHash.Hash);
  finally
    LocalHash.Free;
  end;
end;

procedure TSsl3HandshakeProtocol.VerifyFinishedMessage(const PeerFinished: TBytes; Offset, Size: integer);
var
  RemoteHash: TMD5SHA1CryptoServiceProvider;
  i: integer;
begin
  if Size <> 36 then
    FOwner.SendAlert(adDecodeError, seInvalidMessage);

  RemoteHash := TMD5SHA1CryptoServiceProvider.Create;
  try
    RemoteHash.Protocol := spSsl3;
    RemoteHash.MasterKey := FMasterSecret;
    RemoteHash.TransformBlock(FNegotiationBuffer, 0, FNegotiationBufferPos);

    if FEntity = ceClient then
      RemoteHash.TransformFinalBlock(@FinalServerBlock[0], Length(FinalServerBlock))
    else
      RemoteHash.TransformFinalBlock(@FinalClientBlock[0], Length(FinalClientBlock));

    for i := 0 to Length(RemoteHash.Hash) - 1 do
      if RemoteHash.Hash[i] <> PeerFinished[Offset + i] then
        FOwner.SendAlert(adDecryptError, seHashVerificationNotCorrespond);
  finally
    RemoteHash.Free;
  end;
end;

function TSsl3HandshakeProtocol.GetProtocol: TScSSLProtocol;
begin
  Result := spSsl3;
end;

end.

