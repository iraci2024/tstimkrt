
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScCipherSuites;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CRTypes, CRCryptoTransformIntf, CRSymmetricAlgorithm, CRHashAlgorithm,
  CRCipher, CRHash, CRHMAC,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsUtils, TdsBridge, TdsCertificateConsts, TdsSSLTypes;
{$ELSE}
  TdsSSLConstsUni, TdsUtilsUni, TdsBridgeUni, TdsCertificateConstsUni, TdsSSLTypesUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScCryptoTransformIntf, ScSymmetricAlgorithm, ScHashAlgorithm,
  ScCipher, ScHash,
  ScConsts, ScUtils, ScBridge, ScCertificateConsts, ScHMAC, ScSSLTypes;
{$ENDIF}

type
  TCipherDefinition = record
    CipherAlgorithmClass: TSymmetricAlgorithmClass;
    CipherMode: TCipherMode;
    KeyMaterialLength: integer; // in bytes
    ExpandedKeyLength: integer; // in bytes
    HashAlgorithm: TScHashAlgorithm;
    SupportedProtocols: TScSSLProtocols;
  end;

  TCipherSuite = class
  public
    Encryptor: TSymmetricAlgorithm;
    Decryptor: TSymmetricAlgorithm;
    LocalHasher: TKeyedHashAlgorithm;
    RemoteHasher: TKeyedHashAlgorithm;
    LocalIV: TBytes;
    RemoteIV: TBytes;

    destructor Destroy; override;
  end;

  TCipherSuites = class
  public
    class function ToCipherAlgorithm(Value: word): TScSSLCipherAlgorithm;
    class procedure CipherAlgorithmToBuf(Algorithm: TScSSLCipherAlgorithm; const Buffer: TBytes; Offset: integer);
    class function CipherAlgorithmToName(const Algorithm: TScSSLCipherAlgorithm): string;
    class function NameToCipherAlgorithm(const Name: string): TScSSLCipherAlgorithm;

    class function ToSignatureScheme(Value: word): TScSSLSignatureScheme;

    class function ToEllipticCurve(Value: word): TScECName;
    class function OidToECName(const OID: string): TScECName;
    class function ECNameToOid(ECName: TScECName): string;
    class function SshIdToECName(const ID: string): TScECName;
    class function ECNameToSshId(ECName: TScECName): string;
  end;

const
  SSLCipherDefinitions: array[TScSSLCipherAlgorithm] of TCipherDefinition = (
    (// caNone
     CipherAlgorithmClass: TSymmetricAlgorithm; CipherMode: cmCBC;
     KeyMaterialLength: 0; ExpandedKeyLength: 0;
     HashAlgorithm: haSHA1;
     SupportedProtocols: []),
    (// caAnon_RC4_128_MD5
     CipherAlgorithmClass: TCipher_RC4; CipherMode: cmECB;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haMD5;
     SupportedProtocols: [spSsl3, spTls1]),
    (// caAnon_3DES_168_CBC_SHA
     CipherAlgorithmClass: TCipher_3DES; CipherMode: cmCBC;
     KeyMaterialLength: 24; ExpandedKeyLength: 24;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caAnon_AES_128_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caAnon_AES_256_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caAnon_AES_128_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caAnon_AES_256_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caRSA_RC4_128_MD5
     CipherAlgorithmClass: TCipher_RC4; CipherMode: cmECB;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haMD5;
     SupportedProtocols: [spSsl3]),
    (// caRSA_RC4_128_SHA
     CipherAlgorithmClass: TCipher_RC4; CipherMode: cmECB;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1]),
    (// caRSA_DES_56_CBC_SHA
     CipherAlgorithmClass: TCipher_1DES; CipherMode: cmCBC;
     KeyMaterialLength: 8; ExpandedKeyLength: 8;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11]),
    (// caRSA_3DES_168_CBC_SHA
     CipherAlgorithmClass: TCipher_3DES; CipherMode: cmCBC;
     KeyMaterialLength: 24; ExpandedKeyLength: 24;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caRSA_AES_128_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caRSA_AES_256_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caRSA_AES_128_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caRSA_AES_256_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caRSA_AES_128_GCM_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls12]),
    (// caRSA_AES_256_GCM_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls12]),
    (// caDHE_RSA_DES_56_CBC_SHA
     CipherAlgorithmClass: TCipher_1DES; CipherMode: cmCBC;
     KeyMaterialLength: 8; ExpandedKeyLength: 8;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caDHE_RSA_3DES_168_CBC_SHA
     CipherAlgorithmClass: TCipher_3DES; CipherMode: cmCBC;
     KeyMaterialLength: 24; ExpandedKeyLength: 24;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caDHE_RSA_AES_128_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caDHE_RSA_AES_256_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caDHE_RSA_AES_128_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caDHE_RSA_AES_256_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caDHE_RSA_AES_128_GCM_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls12]),
    (// caDHE_RSA_AES_256_GCM_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls12]),
    (// caECDHE_RSA_RC4_128_SHA
     CipherAlgorithmClass: TCipher_RC4; CipherMode: cmECB;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3]),
    (// caECDHE_RSA_3DES_168_CBC_SHA
     CipherAlgorithmClass: TCipher_3DES; CipherMode: cmCBC;
     KeyMaterialLength: 24; ExpandedKeyLength: 24;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caECDHE_RSA_AES_128_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caECDHE_RSA_AES_256_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caECDHE_RSA_AES_128_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls1, spTls11, spTls12]),
    (// caECDHE_RSA_AES_256_CBC_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls1, spTls11, spTls12]),
    (// caECDHE_RSA_AES_128_GCM_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls12]),
    (// caECDHE_RSA_AES_256_GCM_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls12]),
    (// caECDHE_ECDSA_RC4_128_SHA
     CipherAlgorithmClass: TCipher_RC4; CipherMode: cmECB;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3]),
    (// caECDHE_ECDSA_3DES_168_CBC_SHA
     CipherAlgorithmClass: TCipher_3DES; CipherMode: cmCBC;
     KeyMaterialLength: 24; ExpandedKeyLength: 24;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caECDHE_ECDSA_AES_128_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caECDHE_ECDSA_AES_256_CBC_SHA
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA1;
     SupportedProtocols: [spSsl3, spTls1, spTls11, spTls12]),
    (// caECDHE_ECDSA_AES_128_CBC_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls1, spTls11, spTls12]),
    (// caECDHE_ECDSA_AES_256_CBC_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmCBC;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls1, spTls11, spTls12]),
    (// caECDHE_ECDSA_AES_128_GCM_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls12]),
    (// caECDHE_ECDSA_AES_256_GCM_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls12]),
    (// caAES_128_GCM_SHA256
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 16; ExpandedKeyLength: 16;
     HashAlgorithm: haSHA2_256;
     SupportedProtocols: [spTls13]),
    (// caAES_256_GCM_SHA384
     CipherAlgorithmClass: TCipher_Rijndael; CipherMode: cmGCM;
     KeyMaterialLength: 32; ExpandedKeyLength: 32;
     HashAlgorithm: haSHA2_384;
     SupportedProtocols: [spTls13])
  );

  EllipticCurvesParameters: array[TScECName] of TScECParameters = (
    (ECName: x25519; // https://tools.ietf.org/html/rfc8032#section-5.1
     CryptographyClass: TScECCryptographyX25519;
     Size: 32;
     p:  '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED';
     a:  '76D06';
     b:  '0';
     // d = -121665/121666 = '52036CEE2B6FFE738CC740797779E89800700A4D4141D8AB75EB4DCA135978A3'
     Gx: '216936D3CD6E53FEC0A4E231FDD6DC5C692CC7609525A7B2C9562D608F25D51A';
     Gy: '6666666666666666666666666666666666666666666666666666666666666658';
     n:  '1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED';
     OID: OID_X25519;
     SSHName: ED25519_TYPE_HEADER),
    (ECName: secp160r1;
     CryptographyClass: TScECCryptographyFp;
     Size: 20;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF';
     a:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC';
     b:  '1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45';
     Gx: '4A96B5688EF573284664698968C38BB913CBFC82';
     Gy: '23A628553168947D59DCC912042351377AC5FB32';
     n:  '0100000000000000000001F4C8F927AED3CA752257';
     OID: OID_secp160r1;
     SSHName: ''),
    (ECName: secp160r2;
     CryptographyClass: TScECCryptographyFp;
     Size: 20;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73';
     a:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC70';
     b:  'B4E134D3FB59EB8BAB57274904664D5AF50388BA';
     Gx: '52DCB034293A117E1F4FF11B30F7199D3144CE6D';
     Gy: 'FEAFFEF2E331F296E071FA0DF9982CFEA7D43F2E';
     n:  '0100000000000000000000351EE786A818F3A1A16B';
     OID: OID_secp160r2;
     SSHName: ''),
    (ECName: secp160k1;
     CryptographyClass: TScECCryptographyFp;
     Size: 20;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73';
     a:  '0';
     b:  '7';
     Gx: '3B4C382CE37AA192A4019E763036F4F5DD4D7EBB';
     Gy: '938CF935318FDCED6BC28286531733C3F03C4FEE';
     n:  '0100000000000000000001B8FA16DFAB9ACA16B6B3';
     OID: OID_secp160k1;
     SSHName: ''),
    (ECName: secp192r1;
     CryptographyClass: TScECCryptographyFp;
     Size: 24;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF';
     a:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC';
     b:  '64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1';
     Gx: '188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012';
     Gy: '07192B95FFC8DA78631011ED6B24CDD573F977A11E794811';
     n:  'FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831';
     OID: OID_secp192r1;
     SSHName: SSH_NIST_p192),
    (ECName: secp192k1;
     CryptographyClass: TScECCryptographyFp;
     Size: 24;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37';
     a:  '0';
     b:  '3';
     Gx: 'DB4FF10EC057E9AE26B07D0280B7F4341DA5D1B1EAE06C7D';
     Gy: '9B2F2F6D9C5628A7844163D015BE86344082AA88D95E2F9D';
     n:  'FFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D';
     OID: OID_secp192k1;
     SSHName: ''),
    (ECName: secp224r1;
     CryptographyClass: TScECCryptographyFp;
     Size: 28;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001';
     a:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE';
     b:  'B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4';
     Gx: 'B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21';
     Gy: 'BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34';
     n:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D';
     OID: OID_secp224r1;
     SSHName: SSH_NIST_p224),
    (ECName: secp224k1;
     CryptographyClass: TScECCryptographyFp;
     Size: 28;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D';
     a:  '0';
     b:  '5';
     Gx: 'A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C';
     Gy: '7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5';
     n:  '010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7';
     OID: OID_secp224k1;
     SSHName: ''),
    (ECName: secp256r1;
     CryptographyClass: TScECCryptographyFp;
     Size: 32;
     p:  'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF';
     a:  'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC';
     b:  '5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B';
     Gx: '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296';
     Gy: '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5';
     n:  'FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551';
     OID: OID_secp256r1;
     SSHName: SSH_NIST_p256),
    (ECName: secp256k1;
     CryptographyClass: TScECCryptographyFp;
     Size: 32;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F';
     a:  '0';
     b:  '7';
     Gx: '79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798';
     Gy: '483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8';
     n:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141';
     OID: OID_secp256k1;
     SSHName: ''),
    (ECName: secp384r1;
     CryptographyClass: TScECCryptographyFp;
     Size: 48;
     p:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF';
     a:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC';
     b:  'B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF';
     Gx: 'AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7';
     Gy: '3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F';
     n:  'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973';
     OID: OID_secp384r1;
     SSHName: SSH_NIST_p384),
    (ECName: secp521r1;
     CryptographyClass: TScECCryptographyFp;
     Size: 66;
     p:  '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
     a:  '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC';
     b:  '0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00';
     Gx: '00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66';
     Gy: '011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD16650';
     n:  '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409';
     OID: OID_secp521r1;
     SSHName: SSH_NIST_p521),

    (ECName: sect163r1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 21;
     p:  '0800000000000000000000000000000000000000C9';
     a:  '07B6882CAAEFA84F9554FF8428BD88E246D2782AE2';
     b:  '0713612DCDDCB40AAB946BDA29CA91F73AF958AFD9';
     Gx: '0369979697AB43897789566789567F787A7876A654';
     Gy: '00435EDB42EFAFB2989D51FEFCE3C80988F41FF883';
     n:  '03FFFFFFFFFFFFFFFFFFFF48AAB689C29CA710279B';
     OID: OID_sect163r1;
     SSHName: ''),
    (ECName: sect163r2;
     CryptographyClass: TScECCryptographyF2m;
     Size: 21;
     p:  '0800000000000000000000000000000000000000C9';
     a:  '1';
     b:  '020A601907B8C953CA1481EB10512F78744A3205FD';
     Gx: '03F0EBA16286A2D57EA0991168D4994637E8343E36';
     Gy: '00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1';
     n:  '040000000000000000000292FE77E70C12A4234C33';
     OID: OID_sect163r2;
     SSHName: ''),
    (ECName: sect163k1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 21;
     p:  '0800000000000000000000000000000000000000C9';
     a:  '1';
     b:  '1';
     Gx: '02FE13C0537BBC11ACAA07D793DE4E6D5E5C94EEE8';
     Gy: '0289070FB05D38FF58321F2E800536D538CCDAA3D9';
     n:  '04000000000000000000020108A2E0CC0D99F8A5EF';
     OID: OID_sect163k1;
     SSHName: SSH_NIST_k163),
    (ECName: sect193r1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 25;
     p:  '02000000000000000000000000000000000000000000008001';
     a:  '0017858FEB7A98975169E171F77B4087DE098AC8A911DF7B01';
     b:  '00FDFB49BFE6C3A89FACADAA7A1E5BBC7CC1C2E5D831478814';
     Gx: '01F481BC5F0FF84A74AD6CDF6FDEF4BF6179625372D8C0C5E1';
     Gy: '0025E399F2903712CCF3EA9E3A1AD17FB0B3201B6AF7CE1B05';
     n:  '01000000000000000000000000C7F34A778F443ACC920EBA49';
     OID: OID_sect193r1;
     SSHName: ''),
    (ECName: sect193r2;
     CryptographyClass: TScECCryptographyF2m;
     Size: 25;
     p:  '02000000000000000000000000000000000000000000008001';
     a:  '0163F35A5137C2CE3EA6ED8667190B0BC43ECD69977702709B';
     b:  '00C9BB9E8927D4D64C377E2AB2856A5B16E3EFB7F61D4316AE';
     Gx: '00D9B67D192E0367C803F39E1A7E82CA14A651350AAE617E8F';
     Gy: '01CE94335607C304AC29E7DEFBD9CA01F596F927224CDECF6C';
     n:  '010000000000000000000000015AAB561B005413CCD4EE99D5';
     OID: OID_sect193r2;
     SSHName: ''),
    (ECName: sect233r1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 30;
     p:  '020000000000000000000000000000000000000004000000000000000001';
     a:  '1';
     b:  '0066647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD';
     Gx: '00FAC9DFCBAC8313BB2139F1BB755FEF65BC391F8B36F8F8EB7371FD558B';
     Gy: '01006A08A41903350678E58528BEBF8A0BEFF867A7CA36716F7E01F81052';
     n:  '01000000000000000000000000000013E974E72F8A6922031D2603CFE0D7';
     OID: OID_sect233r1;
     SSHName: SSH_NIST_b233),
    (ECName: sect233k1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 30;
     p:  '020000000000000000000000000000000000000004000000000000000001';
     a:  '0';
     b:  '1';
     Gx: '017232BA853A7E731AF129F22FF4149563A419C26BF50A4C9D6EEFAD6126';
     Gy: '01DB537DECE819B7F70F555A67C427A8CD9BF18AEB9B56E0C11056FAE6A3';
     n:  '008000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF';
     OID: OID_sect233k1;
     SSHName: SSH_NIST_k233),
    (ECName: sect239k1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 30;
     p:  '800000000000000000004000000000000000000000000000000000000001';
     a:  '0';
     b:  '1';
     Gx: '29A0B6A887A983E9730988A68727A8B2D126C44CC2CC7B2A6555193035DC';
     Gy: '76310804F12E549BDB011C103089E73510ACB275FC312A5DC6B76553F0CA';
     n:  '2000000000000000000000000000005A79FEC67CB6E91F1C1DA800E478A5';
     OID: OID_sect239k1;
     SSHName: ''),
    (ECName: sect283r1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 36;
     p:  '0800000000000000000000000000000000000000000000000000000000000000000010A1';
     a:  '1';
     b:  '027B680AC8B8596DA5A4AF8A19A0303FCA97FD7645309FA2A581485AF6263E313B79A2F5';
     Gx: '05F939258DB7DD90E1934F8C70B0DFEC2EED25B8557EAC9C80E2E198F8CDBECD86B12053';
     Gy: '03676854FE24141CB98FE6D4B20D02B4516FF702350EDDB0826779C813F0DF45BE8112F4';
     n:  '03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF90399660FC938A90165B042A7CEFADB307';
     OID: OID_sect283r1;
     SSHName: ''),
    (ECName: sect283k1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 36;
     p:  '0800000000000000000000000000000000000000000000000000000000000000000010A1';
     a:  '0';
     b:  '1';
     Gx: '0503213F78CA44883F1A3B8162F188E553CD265F23C1567A16876913B0C2AC2458492836';
     Gy: '01CCDA380F1C9E318D90F95D07E5426FE87E45C0E8184698E45962364E34116177DD2259';
     n:  '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E061E163C61';
     OID: OID_sect283k1;
     SSHName: SSH_NIST_k283),
    (ECName: sect409r1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 52;
     p:  '02000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001';
     a:  '1';
     b:  '0021A5C2C8EE9FEB5C4B9A753B7B476B7FD6422EF1F3DD674761FA99D6AC27C8A9A197B272822F6CD57A55AA4F50AE317B13545F';
     Gx: '015D4860D088DDB3496B0C6064756260441CDE4AF1771D4DB01FFE5B34E59703DC255A868A1180515603AEAB60794E54BB7996A7';
     Gy: '0061B1CFAB6BE5F32BBFA78324ED106A7636B9C5A7BD198D0158AA4F5488D08F38514F1FDF4B4F40D2181B3681C364BA0273C706';
     n:  '010000000000000000000000000000000000000000000000000001E2AAD6A612F33307BE5FA47C3C9E052F838164CD37D9A21173';
     OID: OID_sect409r1;
     SSHName: SSH_NIST_b409),
    (ECName: sect409k1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 52;
     p:  '02000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001';
     a:  '0';
     b:  '1';
     Gx: '0060F05F658F49C1AD3AB1890F7184210EFD0987E307C84C27ACCFB8F9F67CC2C460189EB5AAAA62EE222EB1B35540CFE9023746';
     Gy: '01E369050B7C4E42ACBA1DACBF04299C3460782F918EA427E6325165E9EA10E3DA5F6C42E9C55215AA9CA27A5863EC48D8E0286B';
     n:  '007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5F83B2D4EA20400EC4557D5ED3E3E7CA5B4B5C83B8E01E5FCF';
     OID: OID_sect409k1;
     SSHName: SSH_NIST_k409),
    (ECName: sect571r1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 72;
     p:  '080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425';
     a:  '1';
     b:  '02F40E7E2221F295DE297117B7F3D62F5C6A97FFCB8CEFF1CD6BA8CE4A9A18AD84FFABBD8EFA59332BE7AD6756A66E294AFD185A78FF12AA520E4DE739BACA0C7FFEFF7F2955727A';
     Gx: '0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABDBDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927E1E7769C8EEC2D19';
     Gy: '037BF27342DA639B6DCCFFFEB73D69D78C6C27A6009CBBCA1980F8533921E8A684423E43BAB08A576291AF8F461BB2A8B3531D2F0485C19B16E2F1516E23DD3C1A4827AF1B8AC15B';
     n:  '03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE661CE18FF55987308059B186823851EC7DD9CA1161DE93D5174D66E8382E9BB2FE84E47';
     OID: OID_sect571r1;
     SSHName: ''),
    (ECName: sect571k1;
     CryptographyClass: TScECCryptographyF2m;
     Size: 72;
     p:  '080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425';
     a:  '0';
     b:  '1';
     Gx: '026EB7A859923FBC82189631F8103FE4AC9CA2970012D5D46024804801841CA44370958493B205E647DA304DB4CEB08CBBD1BA39494776FB988B47174DCA88C7E2945283A01C8972';
     Gy: '0349DC807F4FBF374F4AEADE3BCA95314DD58CEC9F307A54FFC61EFC006D8A2C9D4979C0AC44AEA74FBEBBB9F772AEDCB620B01A7BA7AF1B320430C8591984F601CD4C143EF1C7A3';
     n:  '020000000000000000000000000000000000000000000000000000000000000000000000131850E1F19A63E4B391A8DB917F4138B630D84BE5D639381E91DEB45CFE778F637C1001';
     OID: OID_sect571k1;
     SSHName: SSH_NIST_t571)
  );

  ELLIPTIC_CURVE_DOMAIN_CODES: array [TScECurveDomainType] of byte = (
    1 {ecdtExplicitPrime},
    2 {ecdtExplicitChar2},
    3 {ecdtNamedCurve}
  );

  KEX_NAMED_GROUP_CODES: array [TScKExNamedGroupType] of word = (
    29 {_x25519},
    16 {secp160r1}, 17 {secp160r2}, 15 {secp160k1},
    19 {secp192r1}, 18 {secp192k1}, 21 {secp224r1}, 20 {secp224k1},
    23 {secp256r1}, 22 {secp256k1}, 24 {secp384r1}, 25 {secp521r1},
     2 {sect163r1},  3 {sect163r2},  1 {sect163k1},
     4 {sect193r1},  5 {sect193r2},  7 {sect233r1},  6 {sect233k1}, 8 {sect239k1},
    10 {sect283r1},  9 {sect283k1}, 12 {sect409r1}, 11 {sect409k1},
    14 {sect571r1}, 13 {sect571k1},
    $0100 {ffDHE2048}, $0101 {ffDHE3072}, $0102 {ffDHE4096},
    $0103 {ffDHE6144}, $0104 {ffDHE8192}
  );

implementation

uses
{$IFNDEF SBRIDGE}
{$IFNDEF UNIDACPRO}
  TdsTLS1HandshakeLayer, TdsTLS13HandshakeLayer, TdsSSL3HandshakeLayer;
{$ELSE}
  TdsTLS1HandshakeLayerUni, TdsTLS13HandshakeLayerUni, TdsSSL3HandshakeLayerUni;
{$ENDIF}
{$ELSE}
  ScTLS1HandshakeLayer, ScTLS13HandshakeLayer, ScSSL3HandshakeLayer;
{$ENDIF}

const
  CIPHER_ALGORITHM_CODES: array [TScSSLCipherAlgorithm] of array [0..1] of byte = (
    ($00, $00) {caNone},
    ($00, $18) {caAnon_RC4_128_MD5},
    ($00, $1B) {caAnon_3DES_168_CBC_SHA},
    ($00, $34) {caAnon_AES_128_CBC_SHA},
    ($00, $3A) {caAnon_AES_256_CBC_SHA},
    ($00, $6C) {caAnon_AES_128_CBC_SHA256},
    ($00, $6D) {caAnon_AES_256_CBC_SHA256},
    ($00, $04) {caRSA_RC4_128_MD5},
    ($00, $05) {caRSA_RC4_128_SHA},
    ($00, $09) {caRSA_DES_56_CBC_SHA},
    ($00, $0A) {caRSA_3DES_168_CBC_SHA},
    ($00, $2F) {caRSA_AES_128_CBC_SHA},
    ($00, $35) {caRSA_AES_256_CBC_SHA},
    ($00, $3C) {caRSA_AES_128_CBC_SHA256},
    ($00, $3D) {caRSA_AES_256_CBC_SHA256},
    ($00, $9C) {caRSA_AES_128_GCM_SHA256},
    ($00, $9D) {caRSA_AES_256_GCM_SHA384},
    ($00, $15) {caDHE_RSA_DES_56_CBC_SHA},
    ($00, $16) {caDHE_RSA_3DES_168_CBC_SHA},
    ($00, $33) {caDHE_RSA_AES_128_CBC_SHA},
    ($00, $39) {caDHE_RSA_AES_256_CBC_SHA},
    ($00, $67) {caDHE_RSA_AES_128_CBC_SHA256},
    ($00, $6B) {caDHE_RSA_AES_256_CBC_SHA256},
    ($00, $9E) {caDHE_RSA_AES_128_GCM_SHA256},
    ($00, $9F) {caDHE_RSA_AES_256_GCM_SHA384},
    ($C0, $11) {caECDHE_RSA_RC4_128_SHA},
    ($C0, $12) {caECDHE_RSA_3DES_168_CBC_SHA},
    ($C0, $13) {caECDHE_RSA_AES_128_CBC_SHA},
    ($C0, $14) {caECDHE_RSA_AES_256_CBC_SHA},
    ($C0, $27) {caECDHE_RSA_AES_128_CBC_SHA256},
    ($C0, $28) {caECDHE_RSA_AES_256_CBC_SHA384},
    ($C0, $2F) {caECDHE_RSA_AES_128_GCM_SHA256},
    ($C0, $30) {caECDHE_RSA_AES_256_GCM_SHA384},
    ($C0, $07) {caECDHE_ECDSA_RC4_128_SHA},
    ($C0, $08) {caECDHE_ECDSA_3DES_168_CBC_SHA},
    ($C0, $09) {caECDHE_ECDSA_AES_128_CBC_SHA},
    ($C0, $0A) {caECDHE_ECDSA_AES_256_CBC_SHA},
    ($C0, $23) {caECDHE_ECDSA_AES_128_CBC_SHA256},
    ($C0, $24) {caECDHE_ECDSA_AES_256_CBC_SHA384},
    ($C0, $2B) {caECDHE_ECDSA_AES_128_GCM_SHA256},
    ($C0, $2C) {caECDHE_ECDSA_AES_256_GCM_SHA384},
    ($13, $01) {caAES_128_GCM_SHA256},
    ($13, $02) {caAES_256_GCM_SHA384}
  );

  CIPHER_ALGORITHM_NAMES: array [TScSSLCipherAlgorithm] of string = (
    NONE                           {caNone},
    Anon_RC4_128_MD5               {caAnon_RC4_128_MD5},
    Anon_3DES_168_CBC_SHA          {caAnon_3DES_168_CBC_SHA},
    Anon_AES_128_CBC_SHA           {caAnon_AES_128_CBC_SHA},
    Anon_AES_256_CBC_SHA           {caAnon_AES_256_CBC_SHA},
    Anon_AES_128_CBC_SHA256        {caAnon_AES_128_CBC_SHA256},
    Anon_AES_256_CBC_SHA256        {caAnon_AES_256_CBC_SHA256},
    RSA_RC4_128_MD5                {caRSA_RC4_128_MD5},
    RSA_RC4_128_SHA                {caRSA_RC4_128_SHA},
    RSA_DES_56_CBC_SHA             {caRSA_DES_56_CBC_SHA},
    RSA_3DES_168_CBC_SHA           {caRSA_3DES_168_CBC_SHA},
    RSA_AES_128_CBC_SHA            {caRSA_AES_128_CBC_SHA},
    RSA_AES_256_CBC_SHA            {caRSA_AES_256_CBC_SHA},
    RSA_AES_128_CBC_SHA256         {caRSA_AES_128_CBC_SHA256},
    RSA_AES_256_CBC_SHA256         {caRSA_AES_256_CBC_SHA256},
    RSA_AES_128_GCM_SHA256         {caRSA_AES_128_GCM_SHA256},
    RSA_AES_256_GCM_SHA384         {caRSA_AES_256_GCM_SHA384},
    DHE_RSA_DES_56_CBC_SHA         {caDHE_RSA_DES_56_CBC_SHA},
    DHE_RSA_3DES_168_CBC_SHA       {caDHE_RSA_3DES_168_CBC_SHA},
    DHE_RSA_AES_128_CBC_SHA        {caDHE_RSA_AES_128_CBC_SHA},
    DHE_RSA_AES_256_CBC_SHA        {caDHE_RSA_AES_256_CBC_SHA},
    DHE_RSA_AES_128_CBC_SHA256     {caDHE_RSA_AES_128_CBC_SHA256},
    DHE_RSA_AES_256_CBC_SHA256     {caDHE_RSA_AES_256_CBC_SHA256},
    DHE_RSA_AES_128_GCM_SHA256     {caDHE_RSA_AES_128_GCM_SHA256},
    DHE_RSA_AES_256_GCM_SHA384     {caDHE_RSA_AES_256_GCM_SHA384},
    ECDHE_RSA_RC4_128_SHA          {caECDHE_RSA_RC4_128_SHA},
    ECDHE_RSA_3DES_168_CBC_SHA     {caECDHE_RSA_3DES_168_CBC_SHA},
    ECDHE_RSA_AES_128_CBC_SHA      {caECDHE_RSA_AES_128_CBC_SHA},
    ECDHE_RSA_AES_256_CBC_SHA      {caECDHE_RSA_AES_256_CBC_SHA},
    ECDHE_RSA_AES_128_CBC_SHA256   {caECDHE_RSA_AES_128_CBC_SHA256},
    ECDHE_RSA_AES_256_CBC_SHA384   {caECDHE_RSA_AES_256_CBC_SHA384},
    ECDHE_RSA_AES_128_GCM_SHA256   {caECDHE_RSA_AES_128_GCM_SHA256},
    ECDHE_RSA_AES_256_GCM_SHA384   {caECDHE_RSA_AES_256_GCM_SHA384},
    ECDHE_ECDSA_RC4_128_SHA        {caECDHE_ECDSA_RC4_128_SHA},
    ECDHE_ECDSA_3DES_168_CBC_SHA   {caECDHE_ECDSA_3DES_168_CBC_SHA},
    ECDHE_ECDSA_AES_128_CBC_SHA    {caECDHE_ECDSA_AES_128_CBC_SHA},
    ECDHE_ECDSA_AES_256_CBC_SHA    {caECDHE_ECDSA_AES_256_CBC_SHA},
    ECDHE_ECDSA_AES_128_CBC_SHA256 {caECDHE_ECDSA_AES_128_CBC_SHA256},
    ECDHE_ECDSA_AES_256_CBC_SHA384 {caECDHE_ECDSA_AES_256_CBC_SHA384},
    ECDHE_ECDSA_AES_128_GCM_SHA256 {caECDHE_ECDSA_AES_128_GCM_SHA256},
    ECDHE_ECDSA_AES_256_GCM_SHA384 {caECDHE_ECDSA_AES_256_GCM_SHA384},
    AES_128_GCM_SHA256             {caAES_128_GCM_SHA256},
    AES_256_GCM_SHA384             {caAES_256_GCM_SHA384}
  );

{ TCipherSuite }

destructor TCipherSuite.Destroy;
begin
  Encryptor.Free;
  Decryptor.Free;
  LocalHasher.Free;
  RemoteHasher.Free;

  inherited;
end;

{ TCipherSuites }

class function TCipherSuites.ToCipherAlgorithm(Value: word): TScSSLCipherAlgorithm;
var
  c: TScSSLCipherAlgorithm;
  b1, b2: byte;
begin
  Result := caNone;
  b1 := Value shr 8;
  b2 := byte(Value);

  for c := Low(TScSSLCipherAlgorithm) to High(TScSSLCipherAlgorithm) do
    if (b1 = CIPHER_ALGORITHM_CODES[c][0]) and (b2 = CIPHER_ALGORITHM_CODES[c][1]) then begin
      Result := c;
      Exit;
    end;
end;

class procedure TCipherSuites.CipherAlgorithmToBuf(Algorithm: TScSSLCipherAlgorithm;
  const Buffer: TBytes; Offset: integer);
begin
  if Length(Buffer) < (Offset + 2) then
    raise EScError.Create(seInvalidInputArgs);

  Buffer[Offset] := CIPHER_ALGORITHM_CODES[Algorithm][0];
  Buffer[Offset + 1] := CIPHER_ALGORITHM_CODES[Algorithm][1];
end;

class function TCipherSuites.CipherAlgorithmToName(const Algorithm: TScSSLCipherAlgorithm): string;
begin
  Result := CIPHER_ALGORITHM_NAMES[Algorithm];
end;

class function TCipherSuites.NameToCipherAlgorithm(const Name: string): TScSSLCipherAlgorithm;
var
  UName: string;
  c: TScSSLCipherAlgorithm;
begin
  UName := UpperCase(StringReplace(Name, '-', '_', [rfReplaceAll]));

  for c := Low(TScSSLCipherAlgorithm) to High(TScSSLCipherAlgorithm) do
    if UName = CIPHER_ALGORITHM_NAMES[c] then begin
      Result := c;
      Exit;
    end;

  raise EScError.Create(seInvalidCipherAlgorithm);
end;

class function TCipherSuites.ToSignatureScheme(Value: word): TScSSLSignatureScheme;
var
  ss: TScSSLSignatureScheme;
begin
  for ss := Low(TScSSLSignatureScheme) to High(TScSSLSignatureScheme) do
    if Value = SIGNATURE_SCHEME_CODES[ss] then begin
      Result := ss;
      Exit;
    end;

  Result := TScSSLSignatureScheme(-1);
end;

class function TCipherSuites.ToEllipticCurve(Value: word): TScECName;
var
  ECName: TScECName;
begin
  for ECName := Low(TScECName) to High(TScECName) do
    if Value = KEX_NAMED_GROUP_CODES[TScKExNamedGroupType(byte(ECName))] then begin
      Result := ECName;
      Exit;
    end;

  raise EScError.Create(seInvalidEllipticCurveName);
end;

class function TCipherSuites.OidToECName(const OID: string): TScECName;
begin
  for Result := Low(TScECName) to High(TScECName) do
    if OID = EllipticCurvesParameters[Result].OID then
      Exit;

  raise EScError.Create(seUnknownEllipticCurveId);
end;

class function TCipherSuites.ECNameToOid(ECName: TScECName): string;
begin
  Result := EllipticCurvesParameters[ECName].OID;
end;

class function TCipherSuites.SshIdToECName(const ID: string): TScECName;
begin
  for Result := Low(TScECName) to High(TScECName) do
    if ID = EllipticCurvesParameters[Result].SSHName then
      Exit;

  raise EScError.Create(seUnknownEllipticCurveId);
end;

class function TCipherSuites.ECNameToSshId(ECName: TScECName): string;
begin
  Result := EllipticCurvesParameters[ECName].SSHName;
end;

end.
