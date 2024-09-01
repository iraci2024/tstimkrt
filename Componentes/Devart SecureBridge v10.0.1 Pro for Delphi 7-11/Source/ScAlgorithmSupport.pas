
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScAlgorithmSupport;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CRTypes, CRSymmetricAlgorithm, CRHashAlgorithm, CRCryptoTransformIntf,
{$IFNDEF UNIDACPRO}
  TdsUtils;
{$ELSE}
  TdsUtilsUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScSymmetricAlgorithm, ScHashAlgorithm, ScCryptoTransformIntf,
  ScUtils;
{$ENDIF}

type
  /// Creates a cipher from given parameters
  CipherFactory = class
  public
    class function CreateCipher(Algorithm: TScSymmetricAlgorithm;
      const Key, IV: TBytes): TSymmetricAlgorithm;
    class function CreateCipherEx(Algorithm: TScSymmetricAlgorithmEx;
      CipherMode: TCipherMode): TSymmetricAlgorithm;
    class function CreateHash(Algorithm: TScHashAlgorithm): THashAlgorithm;
    class function HashClass(Algorithm: TScHashAlgorithm): THashAlgorithmClass;
    class function CreateHMAC(Algorithm: TScHMACAlgorithm; const Key: TBytes): IHashTransform;
    class function GetHMACSize(Algorithm: TScHMACAlgorithm): Integer;
    class function GetKeySize(Algorithm: TScSymmetricAlgorithm): Integer;
    class function GetBlockSize(Algorithm: TScSymmetricAlgorithm): Integer;
    class function GetHashSize(HashAlgorithm: TScHashAlgorithm): Integer;
    class function CipherAlgorithmToSSH2Name(Algorithm: TScSymmetricAlgorithm): string;
    class function SSH2NameToCipherAlgorithm(const Name: string): TScSymmetricAlgorithm;
    class function PublicKeyAlgorithmToSSH2Name(Algorithm: TScAsymmetricAlgorithm): string;
    class function PublicKeyAlgorithmToSSH2FullName(Algorithm: TScAsymmetricAlgorithm; ECName: TScECName): string;
    class function PublicKeyAlgorithmToSSH2ListName(Algorithm: TScAsymmetricAlgorithm): string;
    class function PublicKeyAlgorithmToHashAlgorithm(Algorithm: TScAsymmetricAlgorithm; ECName: TScECName = x25519): TScHashAlgorithm;
    class function SSH2NameToPublicKeyAlgorithm(const Name: string): TScAsymmetricAlgorithm;
    class function SSH2NameToHashAlgorithm(const Name: string): TScHashAlgorithm;
    class function PublicKeyAlgorithmToName(Algorithm: TScAsymmetricAlgorithm): string;
    class function NameToPublicKeyAlgorithm(const Name: string): TScAsymmetricAlgorithm;
    class function HashAlgorithmToName(Algorithm: TScHashAlgorithm): string;
    class function NameToHashAlgorithm(const Name: string): TScHashAlgorithm;
    class function SSH2NameToHMACAlgorithm(const Name: string): TScHMACAlgorithm;
    class function HMACAlgorithmToSSH2Name(Algorithm: TScHMACAlgorithm): string;
    class function CompressionAlgorithmToSSH2Name(Algorithm: TScCompressionAlgorithm): string;
    class function SSH2NameToCompressionAlgorithm(const Name: string): TScCompressionAlgorithm;
    class function KeyExchangeAlgorithmToSSH2Name(Algorithm: TScKeyExchangeAlgorithm): string;
    class function SSH2NameToKeyExchangeAlgorithm(const Name: string): TScKeyExchangeAlgorithm;
    class function OidToHashAlgorithm(const HashOid: string): TScHashAlgorithm;
    class function HashAlgorithmToOid(const HashAlgorithm: TScHashAlgorithm): string;
    class function OidToSignatureAlgorithm(const SignatureOid: string): TScSignatureAlgorithm;
    class function SignatureAlgorithmToOid(const SignatureAlgorithm: TScSignatureAlgorithm): string;
    class function GetHashAlgFromSignAlg(const SignAlg: TScSignatureAlgorithm): TScHashAlgorithm;
    class function OidToEncryptionAlgorithm(const EncryptionOid: string): TScSymmetricAlgorithm;
    class function EncryptionAlgorithmToOid(const EncryptionAlgorithm: TScSymmetricAlgorithm): string;
  end;

implementation

uses
{$IFNDEF SBRIDGE}
  CRCipher, CRHash, CRHMAC,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsCertificateConsts, TdsCipherSuites;
{$ELSE}
  TdsSSLConstsUni, TdsCertificateConstsUni, TdsCipherSuitesUni;
{$ENDIF}
{$ELSE}
  ScCipher, ScHash, ScHMAC,
  ScConsts, ScCertificateConsts, ScCipherSuites;
{$ENDIF}

{ CipherFactory }

class function CipherFactory.CreateCipher(Algorithm: TScSymmetricAlgorithm;
  const Key, IV: TBytes): TSymmetricAlgorithm;
begin
  case Algorithm of
    saTripleDES_cbc: begin
      Result := TCipher_3DES.Create;
      Result.Mode := cmCBC;
      Result.Key := Key;
      Result.IV := IV;
    end;
    saBlowfish_cbc: begin
      Result := TCipher_Blowfish.Create;
      Result.Mode := cmCBC;
      Result.Key := Key;
      Result.IV  := IV;
    end;
    saAES128_cbc, saAES192_cbc, saAES256_cbc: begin
      Result := TCipher_Rijndael.Create;
      Result.Mode := cmCBC;
      Result.Key := Key;
      Result.IV  := IV;
    end;
    saCast128_cbc: begin
      Result := TCipher_Cast128.Create;
      Result.Mode := cmCBC;
      Result.Key := Key;
      Result.IV  := IV;
    end;
    saTripleDES_ctr: begin
      Result := TCipher_3DES.Create;
      Result.Mode := cmCTR;
      Result.Key := Key;
      Result.IV := IV;
    end;
    saBlowfish_ctr: begin
      Result := TCipher_Blowfish.Create;
      Result.Mode := cmCTR;
      Result.Key := Key;
      Result.IV  := IV;
    end;
    saAES128_ctr, saAES192_ctr, saAES256_ctr: begin
      Result := TCipher_Rijndael.Create;
      Result.Mode := cmCTR;
      Result.Key := Key;
      Result.IV := IV;
    end;
    saCast128_ctr: begin
      Result := TCipher_Cast128.Create;
      Result.Mode := cmCTR;
      Result.Key := Key;
      Result.IV := IV;
    end;
    else
      raise EScError.Create(seInvalidCipherAlgorithm);
  end;
end;

class function CipherFactory.CreateCipherEx(Algorithm: TScSymmetricAlgorithmEx;
  CipherMode: TCipherMode): TSymmetricAlgorithm;
begin
  case Algorithm of
    sa_DES:
      Result := TCipher_1DES.Create;
    sa_TripleDES:
      Result := TCipher_3DES.Create;
    sa_Blowfish:
      Result := TCipher_Blowfish.Create;
    sa_AES128, sa_AES192, sa_AES256:
      Result := TCipher_Rijndael.Create;
    sa_Cast128:
      Result := TCipher_Cast128.Create;
    sa_RC2:
      Result := TCipher_RC2.Create;
    sa_RC4:
      Result := TCipher_RC4.Create;
    else
      raise EScError.Create(seInvalidCipherAlgorithm);
  end;

  Result.Mode := CipherMode;
end;

class function CipherFactory.CreateHash(Algorithm: TScHashAlgorithm): THashAlgorithm;
begin
  case Algorithm of
    haSHA1:
      Result := THash_SHA1.Create;
    haSHA2_256:
      Result := THash_SHA2_256.Create;
    haSHA2_512:
      Result := THash_SHA2_512.Create;
    haSHA2_224:
      Result := THash_SHA2_224.Create;
    haSHA2_384:
      Result := THash_SHA2_384.Create;
    haMD2:
      Result := THash_MD2.Create;
    haMD4:
      Result := THash_MD4.Create;
    haMD5:
      Result := THash_MD5.Create;
    else
      raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

class function CipherFactory.HashClass(Algorithm: TScHashAlgorithm): THashAlgorithmClass;
begin
  case Algorithm of
    haSHA1:
      Result := THash_SHA1;
    haSHA2_256:
      Result := THash_SHA2_256;
    haSHA2_512:
      Result := THash_SHA2_512;
    haSHA2_224:
      Result := THash_SHA2_224;
    haSHA2_384:
      Result := THash_SHA2_384;
    haMD2:
      Result := THash_MD2;
    haMD4:
      Result := THash_MD4;
    haMD5:
      Result := THash_MD5;
    else
      raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

class function CipherFactory.CreateHMAC(Algorithm: TScHMACAlgorithm; const Key: TBytes): IHashTransform;
var
  HashAlgClass: THashAlgorithmClass;
begin
  case Algorithm of
    hmacSHA1:
      HashAlgClass := THash_SHA1;
    hmacSHA2_256:
      HashAlgClass := THash_SHA2_256;
    hmacSHA2_512:
      HashAlgClass := THash_SHA2_512;
    hmacSHA2_224:
      HashAlgClass := THash_SHA2_224;
    hmacSHA2_384:
      HashAlgClass := THash_SHA2_384;
    hmacMD5:
      HashAlgClass := THash_MD5;
    else
      raise EScError.Create(seInvalidHashAlgorithm);
  end;

  Result := THMAC.Create(HashAlgClass, Key);
end;

class function CipherFactory.GetHMACSize(Algorithm: TScHMACAlgorithm): Integer;
begin
  case Algorithm of
    hmacSHA1:
      Result := THash_SHA1.GetHashSize;
    hmacSHA2_256:
      Result := THash_SHA2_256.GetHashSize;
    hmacSHA2_512:
      Result := THash_SHA2_512.GetHashSize;
    hmacSHA2_224:
      Result := THash_SHA2_224.GetHashSize;
    hmacSHA2_384:
      Result := THash_SHA2_384.GetHashSize;
    hmacMD5:
      Result := THash_MD5.GetHashSize;
    else
      raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

/// returns necessary Key size from Algorithm in bytes
class function CipherFactory.GetKeySize(Algorithm: TScSymmetricAlgorithm): Integer;
begin
  case Algorithm of
    saTripleDES_cbc, saTripleDES_ctr:
      Result := 24;
    saBlowfish_cbc:
      Result := 16;
    saBlowfish_ctr:
      Result := 32; // rfc4344
    saAES128_cbc, saAES128_ctr:
      Result := 16;
    saAES192_cbc, saAES192_ctr:
      Result := 24;
    saAES256_cbc, saAES256_ctr:
      Result := 32;
    saCast128_cbc, saCast128_ctr:
      Result := 16;
    else
      raise EScError.Create(seInvalidCipherAlgorithm);
  end;
end;

/// returns the block size from Algorithm in bytes
class function CipherFactory.GetBlockSize(Algorithm: TScSymmetricAlgorithm): Integer;
begin
  case Algorithm of
    saTripleDES_cbc, saTripleDES_ctr:
      Result := 8;
    saBlowfish_cbc, saBlowfish_ctr:
      Result := 8;
    saAES128_cbc, saAES192_cbc, saAES256_cbc:
      Result := 16;
    saAES128_ctr, saAES192_ctr, saAES256_ctr:
      Result := 16;
    saCast128_cbc, saCast128_ctr:
      Result := 8;
    else
      raise EScError.Create(seInvalidCipherAlgorithm);
  end;
end;

class function CipherFactory.GetHashSize(HashAlgorithm: TScHashAlgorithm): Integer;
begin
  case HashAlgorithm of
    haNone:
      Result := 0;
    haSHA1:
      Result := THash_SHA1.GetHashSize;
    haSHA2_256:
      Result := THash_SHA2_256.GetHashSize;
    haSHA2_512:
      Result := THash_SHA2_512.GetHashSize;
    haSHA2_224:
      Result := THash_SHA2_224.GetHashSize;
    haSHA2_384:
      Result := THash_SHA2_384.GetHashSize;
    haMD5:
      Result := THash_MD5.GetHashSize;
    haMD4:
      Result := THash_MD4.GetHashSize;
    haMD2:
      Result := THash_MD2.GetHashSize;
    else
      raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

class function CipherFactory.CipherAlgorithmToSSH2Name(Algorithm: TScSymmetricAlgorithm): string;
begin
  case Algorithm of
    saTripleDES_cbc:
      Result := STripleDES_cbc;
    saBlowfish_cbc:
      Result := SBlowfish_cbc;
    saAES128_cbc:
      Result := SAES128_cbc;
    saAES192_cbc:
      Result := SAES192_cbc;
    saAES256_cbc:
      Result := SAES256_cbc;
    saCast128_cbc:
      Result := SCast128_cbc;
    saTripleDES_ctr:
      Result := STripleDES_ctr;
    saBlowfish_ctr:
      Result := SBlowfish_ctr;
    saAES128_ctr:
      Result := SAES128_ctr;
    saAES192_ctr:
      Result := SAES192_ctr;
    saAES256_ctr:
      Result := SAES256_ctr;
    saCast128_ctr:
      Result := SCast128_ctr;
    else
      raise EScError.Create(seInvalidCipherAlgorithm);
  end;
end;

class function CipherFactory.SSH2NameToCipherAlgorithm(const Name: string): TScSymmetricAlgorithm;
begin
  if Name = STripleDES_cbc then
    Result := saTripleDES_cbc
  else if Name = STripleDES_ctr then
    Result := saTripleDES_ctr
  else if Name = SBlowfish_cbc then
    Result := saBlowfish_cbc
  else if Name = SBlowfish_ctr then
    Result := saBlowfish_ctr
  else if Name = SAES128_cbc then
    Result := saAES128_cbc
  else if Name = SAES128_ctr then
    Result := saAES128_ctr
  else if Name = SAES192_cbc then
    Result := saAES192_cbc
  else if Name = SAES192_ctr then
    Result := saAES192_ctr
  else if Name = SAES256_cbc then
    Result := saAES256_cbc
  else if Name = SAES256_ctr then
    Result := saAES256_ctr
  else if Name = SCast128_cbc then
    Result := saCast128_cbc
  else if Name = SCast128_ctr then
    Result := saCast128_ctr
  else
    raise EScError.Create(seInvalidCipherAlgorithm);
end;

class function CipherFactory.PublicKeyAlgorithmToSSH2Name(Algorithm: TScAsymmetricAlgorithm): string;
begin
  case Algorithm of
    aaDSA:
      Result := DSA_TYPE_HEADER;
    aaRSA:
      Result := RSA_TYPE_HEADER;
    aaEC:
      Result := ECDSA_TYPE_HEADER;
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
  end;
end;

class function CipherFactory.PublicKeyAlgorithmToSSH2FullName(Algorithm: TScAsymmetricAlgorithm; ECName: TScECName): string;
var
  SshId: string;
begin
  case Algorithm of
    aaDSA:
      Result := DSA_TYPE_HEADER;
    aaRSA:
      Result := RSA_TYPE_HEADER;
    aaEC: begin
      SshId := TCipherSuites.ECNameToSshId(ECName);
      if SshId = '' then
        raise EScError.Create(seUnknownEllipticCurveId);

      if ECName = x25519 then
        Result := SshId
      else
        Result := ECDSA_SHA2_TYPE_HEADER + SshId;
    end;
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
  end;
end;

class function CipherFactory.PublicKeyAlgorithmToSSH2ListName(Algorithm: TScAsymmetricAlgorithm): string;
begin
  case Algorithm of
    aaDSA:
      Result := DSA_TYPE_HEADER;
    aaRSA:
      Result := RSA_SHA256_TYPE_HEADER + ',' + RSA_SHA512_TYPE_HEADER + ',' + RSA_TYPE_HEADER;
    aaEC:
      Result := SECDSA_Sha2_Nistp521 + ',' + SECDSA_Sha2_Nistp384 + ',' + SECDSA_Sha2_Nistp256 +
        ',' + SCurve25519_Sha256 + ',' + ED25519_TYPE_HEADER;
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
  end;
end;

class function CipherFactory.PublicKeyAlgorithmToHashAlgorithm(Algorithm: TScAsymmetricAlgorithm; ECName: TScECName = x25519): TScHashAlgorithm;
var
  B: integer;
begin
  case Algorithm of
    aaDSA:
      Result := haSHA1;
    aaRSA:
      Result := haSHA1;
    aaEC: begin
      if ECName = x25519 then
        Result := haNone
      else begin
        B := EllipticCurvesParameters[ECName].Size shl 3; // * 8;

        if B > 384 then
          Result := haSHA2_512
        else
        if B > 256 then
          Result := haSHA2_384
        else
          Result := haSHA2_256;
      end;
    end;
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
  end;
end;

class function CipherFactory.SSH2NameToPublicKeyAlgorithm(const Name: string): TScAsymmetricAlgorithm;
begin
  if Name = DSA_TYPE_HEADER then
    Result := aaDSA
  else
  if (Name = RSA_TYPE_HEADER) or (Name = RSA_SHA256_TYPE_HEADER) or (Name = RSA_SHA512_TYPE_HEADER) then
    Result := aaRSA
  else
  if (Name = SCurve25519_Sha256) or (Name = ED25519_TYPE_HEADER) then
    Result := aaEC
  else
  if Copy(Name, 1, Length(ECDSA_TYPE_HEADER)) = ECDSA_TYPE_HEADER then
    Result := aaEC
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;

class function CipherFactory.SSH2NameToHashAlgorithm(const Name: string): TScHashAlgorithm;
begin
  if (Name = SCurve25519_Sha256) or (Name = RSA_SHA256_TYPE_HEADER) or (Name = SECDSA_Sha2_Nistp256) then
    Result := haSHA2_256
  else
  if Name = SECDSA_Sha2_Nistp384 then
    Result := haSHA2_384
  else
  if (Name = RSA_SHA512_TYPE_HEADER) or (Name = SECDSA_Sha2_Nistp521) then
    Result := haSHA2_512
  else
  if Name = ED25519_TYPE_HEADER then
    Result := haNone
  else
    Result := haSHA1;
end;

class function CipherFactory.PublicKeyAlgorithmToName(Algorithm: TScAsymmetricAlgorithm): string;
begin
  case Algorithm of
    aaDSA:
      Result := 'DSA';
    aaRSA:
      Result := 'RSA';
    aaEC:
      Result := 'EC';
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
  end;
end;

class function CipherFactory.NameToPublicKeyAlgorithm(const Name: string): TScAsymmetricAlgorithm;
begin
  if Name = 'DSA' then
    Result := aaDSA
  else if Name = 'RSA' then
    Result := aaRSA
  else if Name = 'EC' then
    Result := aaEC
  else
    raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;

class function CipherFactory.HashAlgorithmToName(Algorithm: TScHashAlgorithm): string;
begin
  case Algorithm of
    haNone:
      Result := SHASH_None;
    haSHA1:
      Result := SHASH_SHA1;
    haSHA2_256:
      Result := SHASH_SHA2_256;
    haSHA2_512:
      Result := SHASH_SHA2_512;
    haSHA2_224:
      Result := SHASH_SHA2_224;
    haSHA2_384:
      Result := SHASH_SHA2_384;
    haMD5:
      Result := SHASH_MD5;
    haMD4:
      Result := SHASH_MD4;
    haMD2:
      Result := SHASH_MD2;
  else
    raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

class function CipherFactory.NameToHashAlgorithm(const Name: string): TScHashAlgorithm;
begin
  if Name = SHASH_None then
    Result := haNone
  else if Name = SHASH_SHA1 then
    Result := haSHA1
  else if Name = SHASH_SHA2_256 then
    Result := haSHA2_256
  else if Name = SHASH_SHA2_512 then
    Result := haSHA2_512
  else if Name = SHASH_SHA2_224 then
    Result := haSHA2_224
  else if Name = SHASH_SHA2_384 then
    Result := haSHA2_384
  else if Name = SHASH_MD5 then
    Result := haMD5
  else if Name = SHASH_MD4 then
    Result := haMD4
  else if Name = SHASH_MD2 then
    Result := haMD2
  else
    raise EScError.Create(seInvalidHashAlgorithm);
end;

class function CipherFactory.SSH2NameToHMACAlgorithm(const Name: string): TScHMACAlgorithm;
begin
  if Name = Shmac_sha1 then
    Result := hmacSHA1
  else
  if Name = Shmac_sha2_256 then
    Result := hmacSHA2_256
  else
  if Name = Shmac_sha2_224 then
    Result := hmacSHA2_224
  else
  if Name = Shmac_sha2_512 then
    Result := hmacSHA2_512
  else
  if Name = Shmac_sha2_384 then
    Result := hmacSHA2_384
  else
  if Name = Shmac_md5 then
    Result := hmacMD5
  else
    raise EScError.Create(seInvalidHashAlgorithm);
end;

class function CipherFactory.HMACAlgorithmToSSH2Name(Algorithm: TScHMACAlgorithm): string;
begin
  case Algorithm of
    hmacSHA1:
      Result := Shmac_sha1;
    hmacSHA2_256:
      Result := Shmac_sha2_256;
    hmacSHA2_224:
      Result := Shmac_sha2_224;
    hmacSHA2_512:
      Result := Shmac_sha2_512;
    hmacSHA2_384:
      Result := Shmac_sha2_384;
    hmacMD5:
      Result := Shmac_md5;
    else
      raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

class function CipherFactory.CompressionAlgorithmToSSH2Name(Algorithm: TScCompressionAlgorithm): string;
begin
  case Algorithm of
    csaNone:
      Result := SNone;
    csaZLib:
      Result := SZLib;
    csaZLibOpenSSH:
      Result := SZLibOpenSSH;
    else
      raise EScError.Create(seInvalidCompressionAlgorithm);
  end;
end;

class function CipherFactory.SSH2NameToCompressionAlgorithm(const Name: string): TScCompressionAlgorithm;
begin
  if Name = SNone then
    Result := csaNone
  else if Name = SZLib then
    Result := csaZLib
  else if Name = SZLibOpenSSH then
    Result := csaZLibOpenSSH
  else
    raise EScError.Create(seInvalidCompressionAlgorithm);
end;

class function CipherFactory.KeyExchangeAlgorithmToSSH2Name(Algorithm: TScKeyExchangeAlgorithm): string;
begin
  case Algorithm of
    keDHGroup1Sha1:
      Result := SDiffieHellmanGroup1;
    keDHGroup14Sha1:
      Result := SDiffieHellmanGroup14;
    keDHExchSha1:
      Result := SDiffieHellmanExchSHA1;
    keDHExchSha256:
      Result := SDiffieHellmanExchSHA256;
    keECDHSha2Nistp256:
      Result := SECDH_Sha2_Nistp256;
    keECDHSha2Nistp384:
      Result := SECDH_Sha2_Nistp384;
    keECDHSha2Nistp521:
      Result := SECDH_Sha2_Nistp521;
    keCurve25519Sha256:
      Result := SCurve25519_Sha256;
  else
    raise EScError.Create(seInvalidKeyExchangeAlgorithm);
  end;
end;

class function CipherFactory.SSH2NameToKeyExchangeAlgorithm(const Name: string): TScKeyExchangeAlgorithm;
begin
  if Name = SDiffieHellmanGroup1 then
    Result := keDHGroup1Sha1
  else if Name = SDiffieHellmanGroup14 then
    Result := keDHGroup14Sha1
  else if Name = SDiffieHellmanExchSHA1 then
    Result := keDHExchSha1
  else if Name = SDiffieHellmanExchSHA256 then
    Result := keDHExchSha256
  else if Name = SECDH_Sha2_Nistp256 then
    Result := keECDHSha2Nistp256
  else if Name = SECDH_Sha2_Nistp384 then
    Result := keECDHSha2Nistp384
  else if Name = SECDH_Sha2_Nistp521 then
    Result := keECDHSha2Nistp521
  else if Name = SCurve25519_Sha256 then
    Result := keCurve25519Sha256
  else
    raise EScError.Create(seInvalidKeyExchangeAlgorithm);
end;

class function CipherFactory.OidToHashAlgorithm(const HashOid: string): TScHashAlgorithm;
begin
  if HashOid = '' then
    Result := haNone
  else
  if HashOid = OID_SHA1 then
    Result := haSHA1
  else
  if HashOid = OID_SHA256 then
    Result := haSHA2_256
  else
  if HashOid = OID_SHA384 then
    Result := haSHA2_384
  else
  if HashOid = OID_SHA512 then
    Result := haSHA2_512
  else
  if HashOid = OID_SHA224 then
    Result := haSHA2_224
  else
  if HashOid = OID_MD5 then
    Result := haMD5
  else
  if HashOid = OID_MD4 then
    Result := haMD4
  else
  if HashOid = OID_MD2 then
    Result := haMD2
  else
    raise EScError.Create(seUnknownHashAlgorithm);
end;

class function CipherFactory.HashAlgorithmToOid(const HashAlgorithm: TScHashAlgorithm): string;
begin
  case HashAlgorithm of
    haSHA1:
      Result := OID_SHA1;
    haSHA2_256:
      Result := OID_SHA256;
    haSHA2_512:
      Result := OID_SHA512;
    haSHA2_224:
      Result := OID_SHA224;
    haSHA2_384:
      Result := OID_SHA384;
    haMD2:
      Result := OID_MD2;
    haMD4:
      Result := OID_MD4;
    haMD5:
      Result := OID_MD5;
  else
    raise EScError.Create(seUnknownHashAlgorithm);
  end;
end;

class function CipherFactory.OidToSignatureAlgorithm(const SignatureOid: string): TScSignatureAlgorithm;
begin
  if SignatureOid = '' then
    Result := saUnknown
  else
  if SignatureOid = OID_RSA_ENCRYPTION then
    Result := saRSA_Encryption
  else
  if SignatureOid = OID_RSA_PSS_ENCRYPTION then
    Result := saRSA_PSS_Encryption
  else
  if SignatureOid = OID_SHA1_WITH_RSA_ENC then
    Result := saSHA1_RSA_Encryption
  else
  if SignatureOid = OID_SHA256_WITH_RSA_ENC then
    Result := saSHA256_RSA_Encryption
  else
  if SignatureOid = OID_SHA224_WITH_RSA_ENC then
    Result := saSHA224_RSA_Encryption
  else
  if SignatureOid = OID_SHA512_WITH_RSA_ENC then
    Result := saSHA512_RSA_Encryption
  else
  if SignatureOid = OID_SHA384_WITH_RSA_ENC then
    Result := saSHA384_RSA_Encryption
  else
  if SignatureOid = OID_MD5_WITH_RSA_ENC then
    Result := saMD5_RSA_Encryption
  else
  if SignatureOid = OID_MD4_WITH_RSA_ENC then
    Result := saMD4_RSA_Encryption
  else
  if SignatureOid = OID_MD2_WITH_RSA_ENC then
    Result := saMD2_RSA_Encryption
  else
  if SignatureOid = OID_DSA_ENCRYPTION then
    Result := saDSA_Encryption
  else
  if SignatureOid = OID_DSA_WITH_SHA1 then
    Result := saDSA_SHA1
  else
  if SignatureOid = OID_DSA_WITH_SHA256 then
    Result := saDSA_SHA256
  else
  if SignatureOid = OID_DSA_WITH_SHA224 then
    Result := saDSA_SHA224
  else
  if SignatureOid = OID_ECDSA_WITH_SHA1 then
    Result := saEC_SHA1
  else
  if SignatureOid = OID_ECDSA_WITH_SHA224 then
    Result := saEC_SHA224
  else
  if SignatureOid = OID_ECDSA_WITH_SHA256 then
    Result := saEC_SHA256
  else
  if SignatureOid = OID_ECDSA_WITH_SHA384 then
    Result := saEC_SHA384
  else
  if SignatureOid = OID_ECDSA_WITH_SHA512 then
    Result := saEC_SHA512
  else
    raise EScError.Create(seUnknownSignatureAlgorithm);
end;

class function CipherFactory.SignatureAlgorithmToOid(const SignatureAlgorithm: TScSignatureAlgorithm): string;
begin
  case SignatureAlgorithm of
    saRSA_Encryption:
      Result := OID_RSA_ENCRYPTION;
    saRSA_PSS_Encryption:
      Result := OID_RSA_PSS_ENCRYPTION;
    saSHA1_RSA_Encryption:
      Result := OID_SHA1_WITH_RSA_ENC;
    saSHA256_RSA_Encryption:
      Result := OID_SHA256_WITH_RSA_ENC;
    saSHA224_RSA_Encryption:
      Result := OID_SHA224_WITH_RSA_ENC;
    saSHA512_RSA_Encryption:
      Result := OID_SHA512_WITH_RSA_ENC;
    saSHA384_RSA_Encryption:
      Result := OID_SHA384_WITH_RSA_ENC;
    saMD5_RSA_Encryption:
      Result := OID_MD5_WITH_RSA_ENC;
    saMD4_RSA_Encryption:
      Result := OID_MD4_WITH_RSA_ENC;
    saMD2_RSA_Encryption:
      Result := OID_MD2_WITH_RSA_ENC;
    saDSA_Encryption:
      Result := OID_DSA_ENCRYPTION;
    saDSA_SHA1:
      Result := OID_DSA_WITH_SHA1;
    saDSA_SHA256:
      Result := OID_DSA_WITH_SHA256;
    saDSA_SHA224:
      Result := OID_DSA_WITH_SHA224;
  else
    raise EScError.Create(seUnknownSignatureAlgorithm);
  end;
end;

class function CipherFactory.GetHashAlgFromSignAlg(const SignAlg: TScSignatureAlgorithm): TScHashAlgorithm;
begin
  case SignAlg of
    saRSA_Encryption:
      Result := haNone;
    saRSA_PSS_Encryption:
      Result := haNone;
    saSHA1_RSA_Encryption:
      Result := haSHA1;
    saSHA256_RSA_Encryption:
      Result := haSHA2_256;
    saSHA224_RSA_Encryption:
      Result := haSHA2_224;
    saSHA512_RSA_Encryption:
      Result := haSHA2_512;
    saSHA384_RSA_Encryption:
      Result := haSHA2_384;
    saMD5_RSA_Encryption:
      Result := haMD5;
    saMD4_RSA_Encryption:
      Result := haMD4;
    saMD2_RSA_Encryption:
      Result := haMD2;
    saDSA_Encryption:
      Result := haNone;
    saDSA_SHA1:
      Result := haSHA1;
    saDSA_SHA256:
      Result := haSHA2_256;
    saDSA_SHA224:
      Result := haSHA2_224;
  else
    Result := haNone;
  end;
end;

class function CipherFactory.OidToEncryptionAlgorithm(const EncryptionOid: string): TScSymmetricAlgorithm;
begin
  if EncryptionOid = OID_AES256_CBC then
    Result := saAES256_cbc
  else
  if EncryptionOid = OID_AES192_CBC then
    Result := saAES192_cbc
  else
  if EncryptionOid = OID_AES128_CBC then
    Result := saAES128_cbc
  else
  if EncryptionOid = OID_DES_EDE3_CBC then
    Result := saTripleDES_cbc
  else
    raise EScError.Create(seCipherNotSupported);
end;

class function CipherFactory.EncryptionAlgorithmToOid(const EncryptionAlgorithm: TScSymmetricAlgorithm): string;
begin
  case EncryptionAlgorithm of
    saTripleDES_cbc:
      Result := OID_DES_EDE3_CBC;
    saAES128_cbc:
      Result := OID_AES128_CBC;
    saAES192_cbc:
      Result := OID_AES192_CBC;
    saAES256_cbc:
      Result := OID_AES256_CBC;
  else
    raise EScError.Create(seCipherNotSupported);
  end;
end;

end.

