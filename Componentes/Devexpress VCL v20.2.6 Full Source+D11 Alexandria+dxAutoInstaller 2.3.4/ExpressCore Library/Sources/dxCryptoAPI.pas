{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressCore Library }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxCryptoAPI; // for internal use

{$I cxVer.inc}

interface

uses
  Windows;

{$IFDEF WIN32}
(*$HPPEMIT '#pragma link "crypt32.lib"' *)
(*$HPPEMIT '#pragma link "cryptui.lib"' *)
{$ELSE}
(*$HPPEMIT '#pragma link "crypt32.a"' *)
(*$HPPEMIT '#pragma link "cryptui.a"' *)
{$ENDIF}

const
  CryptoProviderEnhancedRSA_AES =
    'Microsoft Enhanced RSA and AES Cryptographic Provider';
  CryptoProviderEnhancedRSA_AES_XP =
    'Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)';

const
  PROV_RSA_FULL = 1;
{$EXTERNALSYM PROV_RSA_FULL}
  PROV_RSA_SIG = 2;
{$EXTERNALSYM PROV_RSA_SIG}
  PROV_RSA_AES = 24;
{$EXTERNALSYM PROV_RSA_AES}

const
  CRYPT_VERIFYCONTEXT = DWORD($F0000000);
{$EXTERNALSYM CRYPT_VERIFYCONTEXT}
  CRYPT_NEWKEYSET = $00000008;
{$EXTERNALSYM CRYPT_NEWKEYSET}
  CRYPT_DELETEKEYSET = $00000010;
{$EXTERNALSYM CRYPT_DELETEKEYSET}
  CRYPT_MACHINE_KEYSET = $00000020;
{$EXTERNALSYM CRYPT_MACHINE_KEYSET}
  CRYPT_SILENT = $00000040;
{$EXTERNALSYM CRYPT_SILENT}

type
  ALG_ID = Cardinal;
{$EXTERNALSYM ALG_ID}

const
  CALG_3DES = $00006603;
{$EXTERNALSYM CALG_3DES}
  CALG_3DES_112 = $00006609;
{$EXTERNALSYM CALG_3DES_112}
  CALG_AES = $00006611;
{$EXTERNALSYM CALG_AES}
  CALG_AES_128 = $0000660E;
{$EXTERNALSYM CALG_AES_128}
  CALG_AES_192 = $0000660F;
{$EXTERNALSYM CALG_AES_192}
  CALG_AES_256 = $00006610;
{$EXTERNALSYM CALG_AES_256}
  CALG_AGREEDKEY_ANY = $0000AA03;
{$EXTERNALSYM CALG_AGREEDKEY_ANY}
  CALG_CYLINK_MEK = $0000660C;
{$EXTERNALSYM CALG_CYLINK_MEK}
  CALG_DES = $00006601;
{$EXTERNALSYM CALG_DES}
  CALG_DESX = $00006604;
{$EXTERNALSYM CALG_DESX}
  CALG_DH_EPHEM = $0000AA02;
{$EXTERNALSYM CALG_DH_EPHEM}
  CALG_DH_SF = $0000AA01;
{$EXTERNALSYM CALG_DH_SF}
  CALG_DSS_SIGN = $00002200;
{$EXTERNALSYM CALG_DSS_SIGN}
  CALG_HASH_REPLACE_OWF = $0000800B;
{$EXTERNALSYM CALG_HASH_REPLACE_OWF}
  CALG_HUGHES_MD5 = $0000A003;
{$EXTERNALSYM CALG_HUGHES_MD5}
  CALG_HMAC = $00008009;
{$EXTERNALSYM CALG_HMAC}
  CALG_MAC = $00008005;
{$EXTERNALSYM CALG_MAC}
  CALG_MD2 = $00008001;
{$EXTERNALSYM CALG_MD2}
  CALG_MD4 = $00008002;
{$EXTERNALSYM CALG_MD4}
  CALG_MD5 = $00008003;
{$EXTERNALSYM CALG_MD5}
  CALG_NO_SIGN = $00002000;
{$EXTERNALSYM CALG_NO_SIGN}
  CALG_RC2 = $00006602;
{$EXTERNALSYM CALG_RC2}
  CALG_RC4 = $00006801;
{$EXTERNALSYM CALG_RC4}
  CALG_RC5 = $0000660D;
{$EXTERNALSYM CALG_RC5}
  CALG_RSA_KEYX = $0000A400;
{$EXTERNALSYM CALG_RSA_KEYX}
  CALG_RSA_SIGN = $00002400;
{$EXTERNALSYM CALG_RSA_SIGN}
  CALG_SHA = $00008004;
{$EXTERNALSYM CALG_SHA}
  CALG_SHA1 = $00008004;
{$EXTERNALSYM CALG_SHA1}
  CALG_SHA_256 = $0000800C;
{$EXTERNALSYM CALG_SHA_256}
  CALG_SHA_384 = $0000800D;
{$EXTERNALSYM CALG_SHA_384}
  CALG_SHA_512 = $0000800E;
{$EXTERNALSYM CALG_SHA_512}
  CALG_ECDH = $0000AA05;
{$EXTERNALSYM CALG_ECDH}
  CALG_ECDH_EPHEM = $0000AE06;
{$EXTERNALSYM CALG_ECDH_EPHEM}
  CALG_ECDSA = $00002203;
{$EXTERNALSYM CALG_ECDSA}

const
  HP_ALGID = $0001; // Hash algorithm
{$EXTERNALSYM HP_ALGID}
  HP_HASHVAL = $0002; // Hash value
{$EXTERNALSYM HP_HASHVAL}
  HP_HASHSIZE = $0004; // Hash value size
{$EXTERNALSYM HP_HASHSIZE}
  HP_HMAC_INFO = $0005; // information for creating an HMAC
{$EXTERNALSYM HP_HMAC_INFO}
  HP_TLS1PRF_LABEL = $0006; // label for TLS1 PRF
{$EXTERNALSYM HP_TLS1PRF_LABEL}
  HP_TLS1PRF_SEED = $0007; // seed for TLS1 PRF
{$EXTERNALSYM HP_TLS1PRF_SEED}
  // dwParam

  KP_IV = 1; // Initialization vector
{$EXTERNALSYM KP_IV}
  KP_SALT = 2; // Salt value
{$EXTERNALSYM KP_SALT}
  KP_PADDING = 3; // Padding values
{$EXTERNALSYM KP_PADDING}
  KP_MODE = 4; // Mode of the cipher
{$EXTERNALSYM KP_MODE}
  KP_MODE_BITS = 5; // Number of bits to feedback
{$EXTERNALSYM KP_MODE_BITS}
  KP_PERMISSIONS = 6; // Key permissions DWORD
{$EXTERNALSYM KP_PERMISSIONS}
  KP_ALGID = 7; // Key algorithm
{$EXTERNALSYM KP_ALGID}
  KP_BLOCKLEN = 8; // Block size of the cipher
{$EXTERNALSYM KP_BLOCKLEN}
  KP_KEYLEN = 9; // Length of key in bits
{$EXTERNALSYM KP_KEYLEN}
  KP_SALT_EX = 10; // Length of salt in bytes
{$EXTERNALSYM KP_SALT_EX}
  KP_P = 11; // DSS/Diffie-Hellman P value
{$EXTERNALSYM KP_P}
  KP_G = 12; // DSS/Diffie-Hellman G value
{$EXTERNALSYM KP_G}
  KP_Q = 13; // DSS Q value
{$EXTERNALSYM KP_Q}
  KP_X = 14; // Diffie-Hellman X value
{$EXTERNALSYM KP_X}
  KP_Y = 15; // Y value
{$EXTERNALSYM KP_Y}
  KP_RA = 16; // Fortezza RA value
{$EXTERNALSYM KP_RA}
  KP_RB = 17; // Fortezza RB value
{$EXTERNALSYM KP_RB}
  KP_INFO = 18; // for putting information into an RSA envelope
{$EXTERNALSYM KP_INFO}
  KP_EFFECTIVE_KEYLEN = 19; // setting and getting RC2 effective key length
{$EXTERNALSYM KP_EFFECTIVE_KEYLEN}
  KP_SCHANNEL_ALG = 20; // for setting the Secure Channel algorithms
{$EXTERNALSYM KP_SCHANNEL_ALG}
  KP_CLIENT_RANDOM = 21; // for setting the Secure Channel client random data
{$EXTERNALSYM KP_CLIENT_RANDOM}
  KP_SERVER_RANDOM = 22; // for setting the Secure Channel server random data
{$EXTERNALSYM KP_SERVER_RANDOM}
  KP_RP = 23;
{$EXTERNALSYM KP_RP}
  KP_PRECOMP_MD5 = 24;
{$EXTERNALSYM KP_PRECOMP_MD5}
  KP_PRECOMP_SHA = 25;
{$EXTERNALSYM KP_PRECOMP_SHA}
  KP_CERTIFICATE = 26; // for setting Secure Channel certificate data (PCT1)
{$EXTERNALSYM KP_CERTIFICATE}
  KP_CLEAR_KEY = 27; // for setting Secure Channel clear key data (PCT1)
{$EXTERNALSYM KP_CLEAR_KEY}
  KP_PUB_EX_LEN = 28;
{$EXTERNALSYM KP_PUB_EX_LEN}
  KP_PUB_EX_VAL = 29;
{$EXTERNALSYM KP_PUB_EX_VAL}
  KP_KEYVAL = 30;
{$EXTERNALSYM KP_KEYVAL}
  KP_ADMIN_PIN = 31;
{$EXTERNALSYM KP_ADMIN_PIN}
  KP_KEYEXCHANGE_PIN = 32;
{$EXTERNALSYM KP_KEYEXCHANGE_PIN}
  KP_SIGNATURE_PIN = 33;
{$EXTERNALSYM KP_SIGNATURE_PIN}
  KP_PREHASH = 34;
{$EXTERNALSYM KP_PREHASH}
  KP_OAEP_PARAMS = 36; // for setting OAEP params on RSA keys
{$EXTERNALSYM KP_OAEP_PARAMS}
  KP_CMS_KEY_INFO = 37;
{$EXTERNALSYM KP_CMS_KEY_INFO}
  KP_CMS_DH_KEY_INFO = 38;
{$EXTERNALSYM KP_CMS_DH_KEY_INFO}
  KP_PUB_PARAMS = 39; // for setting public parameters
{$EXTERNALSYM KP_PUB_PARAMS}
  KP_VERIFY_PARAMS = 40; // for verifying DSA and DH parameters
{$EXTERNALSYM KP_VERIFY_PARAMS}
  KP_HIGHEST_VERSION = 41; // for TLS protocol version setting
{$EXTERNALSYM KP_HIGHEST_VERSION}
  // KP_PADDING

  PKCS5_PADDING = 1; // PKCS 5 (sec 6.2) padding method
{$EXTERNALSYM PKCS5_PADDING}
  RANDOM_PADDING = 2;
{$EXTERNALSYM RANDOM_PADDING}
  ZERO_PADDING = 3;
{$EXTERNALSYM ZERO_PADDING}
  // KP_MODE

  CRYPT_MODE_CBC = 1; // Cipher block chaining
{$EXTERNALSYM CRYPT_MODE_CBC}
  CRYPT_MODE_ECB = 2; // Electronic code book
{$EXTERNALSYM CRYPT_MODE_ECB}
  CRYPT_MODE_OFB = 3; // Output feedback mode
{$EXTERNALSYM CRYPT_MODE_OFB}
  CRYPT_MODE_CFB = 4; // Cipher feedback mode
{$EXTERNALSYM CRYPT_MODE_CFB}
  CRYPT_MODE_CTS = 5; // CipherText stealing mode
{$EXTERNALSYM CRYPT_MODE_CTS}
  // KP_PERMISSIONS

  CRYPT_ENCRYPT = $0001; // Allow encryption
{$EXTERNALSYM CRYPT_ENCRYPT}
  CRYPT_DECRYPT = $0002; // Allow decryption
{$EXTERNALSYM CRYPT_DECRYPT}
  CRYPT_EXPORT = $0004; // Allow key to be exported
{$EXTERNALSYM CRYPT_EXPORT}
  CRYPT_READ = $0008; // Allow parameters to be read
{$EXTERNALSYM CRYPT_READ}
  CRYPT_WRITE = $0010; // Allow parameters to be set
{$EXTERNALSYM CRYPT_WRITE}
  CRYPT_MAC = $0020; // Allow MACs to be used with key
{$EXTERNALSYM CRYPT_MAC}
  CRYPT_EXPORT_KEY = $0040; // Allow key to be used for exporting keys
{$EXTERNALSYM CRYPT_EXPORT_KEY}
  CRYPT_IMPORT_KEY = $0080; // Allow key to be used for importing keys
{$EXTERNALSYM CRYPT_IMPORT_KEY}
  // exported key blob definitions

  SIMPLEBLOB = $1;
{$EXTERNALSYM SIMPLEBLOB}
  PUBLICKEYBLOB = $6;
{$EXTERNALSYM PUBLICKEYBLOB}
  PRIVATEKEYBLOB = $7;
{$EXTERNALSYM PRIVATEKEYBLOB}
  PLAINTEXTKEYBLOB = $8;
{$EXTERNALSYM PLAINTEXTKEYBLOB}
  OPAQUEKEYBLOB = $9;
{$EXTERNALSYM OPAQUEKEYBLOB}
  PUBLICKEYBLOBEX = $A;
{$EXTERNALSYM PUBLICKEYBLOBEX}
  SYMMETRICWRAPKEYBLOB = $B;
{$EXTERNALSYM SYMMETRICWRAPKEYBLOB}
  AT_KEYEXCHANGE = 1;
{$EXTERNALSYM AT_KEYEXCHANGE}
  AT_SIGNATURE = 2;
{$EXTERNALSYM AT_SIGNATURE}
  CRYPT_USERDATA = 1;
{$EXTERNALSYM CRYPT_USERDATA}
  CUR_BLOB_VERSION = 2;
{$EXTERNALSYM CUR_BLOB_VERSION}

type
  PdxPublicKeyStructure = ^TdxPublicKeyStructure;

  TdxPublicKeyStructure = record
    bType: BYTE;
    bVersion: BYTE;
    reserved: WORD;
    aiKeyAlg: ALG_ID;
  end;

  PdxHMACInfo = ^TdxHMACInfo;

  TdxHMACInfo = record
    HashAlgid: ALG_ID;
    pbInnerString: PBYTE;
    cbInnerString: DWORD;
    pbOuterString: PBYTE;
    cbOuterString: DWORD;
  end;

const
  szOID_RSA_SHA1RSA = '1.2.840.113549.1.1.5';
{$EXTERNALSYM szOID_RSA_SHA1RSA}
  szOID_RSA_emailAddr = '1.2.840.113549.1.9.1';
{$EXTERNALSYM szOID_RSA_emailAddr}
  szOID_SUBJECT_KEY_IDENTIFIER = '2.5.29.14';
{$EXTERNALSYM szOID_SUBJECT_KEY_IDENTIFIER}
  szOID_SUBJECT_ALT_NAME2 = '2.5.29.17';
{$EXTERNALSYM szOID_SUBJECT_ALT_NAME2}

type
  HCERTSTORE = Pointer;
{$EXTERNALSYM HCERTSTORE}
  PHCERTSTORE = ^HCERTSTORE;
{$EXTERNALSYM PHCERTSTORE}
  HCRYPTPROV = ULONG_PTR;
{$EXTERNALSYM HCRYPTPROV}
  HCRYPTKEY = ULONG_PTR;
{$EXTERNALSYM HCRYPTKEY}
  HCRYPTHASH = ULONG_PTR;
{$EXTERNALSYM HCRYPTHASH}
  PHCRYPTPROV = ^HCRYPTPROV;
{$NODEFINE PHCRYPTPROV}
  PHCRYPTKEY = ^HCRYPTKEY;
{$NODEFINE PHCRYPTKEY}
  PHCRYPTHASH = ^HCRYPTHASH;
{$NODEFINE PHCRYPTHASH}
  PPCCERT_CONTEXT = Pointer;
  PPCCRL_CONTEXT = Pointer;

const
  CERT_NAME_ISSUER_FLAG = 1;
{$EXTERNALSYM CERT_NAME_ISSUER_FLAG}
  CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
{$EXTERNALSYM CERT_NAME_SIMPLE_DISPLAY_TYPE}
  X509_ASN_ENCODING = $00000001;
{$EXTERNALSYM X509_ASN_ENCODING}
  PKCS_7_ASN_ENCODING = $00010000;
{$EXTERNALSYM PKCS_7_ASN_ENCODING}
  CERT_STORE_PROV_MEMORY = LPCSTR(2);
{$EXTERNALSYM CERT_STORE_PROV_MEMORY}
  CERT_STORE_PROV_SYSTEM = LPCSTR(10);
{$EXTERNALSYM CERT_STORE_PROV_SYSTEM}
  X509_NAME = LPCSTR(7);
{$EXTERNALSYM X509_NAME}
  // The Certificate intended key usage
  CERT_DIGITAL_SIGNATURE_KEY_USAGE = $80;
{$EXTERNALSYM CERT_DIGITAL_SIGNATURE_KEY_USAGE}
  CERT_NON_REPUDIATION_KEY_USAGE = $40;
{$EXTERNALSYM CERT_NON_REPUDIATION_KEY_USAGE}
  CERT_KEY_ENCIPHERMENT_KEY_USAGE = $20;
{$EXTERNALSYM CERT_KEY_ENCIPHERMENT_KEY_USAGE}
  CERT_DATA_ENCIPHERMENT_KEY_USAGE = $10;
{$EXTERNALSYM CERT_DATA_ENCIPHERMENT_KEY_USAGE}
  CERT_KEY_AGREEMENT_KEY_USAGE = $08;
{$EXTERNALSYM CERT_KEY_AGREEMENT_KEY_USAGE}
  CERT_KEY_CERT_SIGN_KEY_USAGE = $04;
{$EXTERNALSYM CERT_KEY_CERT_SIGN_KEY_USAGE}
  CERT_OFFLINE_CRL_SIGN_KEY_USAGE = $02;
{$EXTERNALSYM CERT_OFFLINE_CRL_SIGN_KEY_USAGE}
  CERT_KEY_PROV_INFO_PROP_ID = 2;
{$EXTERNALSYM CERT_KEY_PROV_INFO_PROP_ID}
  CERT_HASH_PROP_ID = 3;
{$EXTERNALSYM CERT_HASH_PROP_ID}
  CERT_FRIENDLY_NAME_PROP_ID = 11;
{$EXTERNALSYM CERT_FRIENDLY_NAME_PROP_ID}
  CERT_V1 = 0;
{$EXTERNALSYM CERT_V1}
  CERT_V2 = 1;
{$EXTERNALSYM CERT_V2}
  CERT_V3 = 2;
{$EXTERNALSYM CERT_V3}
  CERT_ALT_NAME_RFC822_NAME = 2;
{$EXTERNALSYM CERT_ALT_NAME_RFC822_NAME}
  CERT_X500_NAME_STR = 3;
{$EXTERNALSYM CERT_X500_NAME_STR}
  CERT_NAME_STR_NO_PLUS_FLAG = $20000000;
{$EXTERNALSYM CERT_NAME_STR_NO_PLUS_FLAG}

type
  PCRYPTOAPI_BLOB = ^CRYPTOAPI_BLOB;

  CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;

  CRYPT_DATA_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_DATA_BLOB = ^CRYPT_DATA_BLOB;
  CRYPT_INTEGER_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_INTEGER_BLOB = ^CRYPT_INTEGER_BLOB;
  CERT_NAME_BLOB = CRYPTOAPI_BLOB;
  PCERT_NAME_BLOB = ^CERT_NAME_BLOB;
  CRYPT_OBJID_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_OBJID_BLOB = ^CRYPT_OBJID_BLOB;
  CERT_RDN_VALUE_BLOB = CRYPTOAPI_BLOB;
  PCERT_RDN_VALUE_BLOB = ^CERT_RDN_VALUE_BLOB;
  CRYPT_ATTR_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_ATTR_BLOB = ^CRYPT_ATTR_BLOB;

  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;

  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;

  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;

  CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
    cUnusedBits: DWORD;
  end;

  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;

  CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;

  PCERT_EXTENSION = ^CERT_EXTENSION;

  CERT_EXTENSION = record
    pszObjId: LPSTR;
    fCritical: BOOL;
    Value: CRYPT_OBJID_BLOB;
  end;

  PCERT_INFO = ^CERT_INFO;

  CERT_INFO = record
  public
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: FILETIME;
    NotAfter: FILETIME;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;

  PCERT_CONTEXT = ^CERT_CONTEXT;

  CERT_CONTEXT = record
  public
    dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    HCERTSTORE: HCERTSTORE;
  end;

  PCCRYPTUI_VIEWCERTIFICATE_STRUCT = ^CRYPTUI_VIEWCERTIFICATE_STRUCT;

  CRYPTUI_VIEWCERTIFICATE_STRUCT = record
  public
    dwSize: DWORD;
    hwndParent: HWND;
    dwFlags: DWORD;
    szTitle: LPCTSTR;
    pCertContext: PCERT_CONTEXT;
    rgszPurposes: Pointer;
    cPurposes: DWORD;
    hWVTStateData: Pointer;
    fpCryptProviderDataTrustedUsage: BOOL;
    idxSigner: DWORD;
    idxCert: DWORD;
    fCounterSigner: BOOL;
    idxCounterSigner: DWORD;
    cStores: DWORD;
    rghStores: PCERT_CONTEXT;
    cPropSheetPages: DWORD;
    rgPropSheetPages: Pointer;
    nStartPage: DWORD;
  end;

  PCCRYPTUI_SELECTCERTIFICATE_STRUCT = ^CRYPTUI_SELECTCERTIFICATE_STRUCT;

  CRYPTUI_SELECTCERTIFICATE_STRUCT = record
  public
    dwSize: Cardinal;
    hwndParent: DWORD;
    dwFlags: Cardinal;
    szTitle: Pointer;
    dwDontUseColumn: Cardinal;
    szDisplayString: Pointer;
    pFilterCallback: DWORD;
    pDisplayCallback: DWORD;
    pvCallbackData: DWORD;
    cDisplayStores: Cardinal;
    rghDisplayStores: PHCERTSTORE;
    cStores: Cardinal;
    rghStores: DWORD;
    cPropSheetPages: Cardinal;
    rgPropSheetPages: DWORD;
    hSelectedCertStore: PHCERTSTORE;
  end;

  PCERT_ALT_NAME_ENTRY = ^CERT_ALT_NAME_ENTRY;

  CERT_ALT_NAME_ENTRY = record
    dwAltNameChoice: DWORD;
    case Integer of
      0:
        ();
      1:
        (pwszRfc822Name: LPWSTR);
      2:
        (pwszDNSName: LPWSTR);
      3:
        ();
      4:
        (DirectoryName: CERT_NAME_BLOB);
      5:
        ();
      6:
        (pwszURL: LPWSTR);
      7:
        (IPAddress: CRYPT_DATA_BLOB);
      8:
        (pszRegisteredID: LPSTR);
  end;

  PCERT_ALT_NAME_INFO = ^CERT_ALT_NAME_INFO;

  CERT_ALT_NAME_INFO = record
    cAltEntry: DWORD;
    rgAltEntry: PCERT_ALT_NAME_ENTRY;
  end;

  PCERT_RDN_ATTR = ^CERT_RDN_ATTR;

  CERT_RDN_ATTR = record
    pszObjId: LPSTR;
    dwValueType: DWORD;
    Value: CERT_RDN_VALUE_BLOB;
  end;

  PCERT_RDN = ^CERT_RDN;

  CERT_RDN = record
    cRDNAttr: DWORD;
    rgRDNAttr: PCERT_RDN_ATTR;
  end;

  PCERT_NAME_INFO = ^CERT_NAME_INFO;

  CERT_NAME_INFO = record
    cRDN: DWORD;
    rgRDN: PCERT_RDN;
  end;

  PCTL_USAGE = ^CTL_USAGE;

  CTL_USAGE = record
    cUsageIdentifier: DWORD;
    rgpszUsageIdentifier: PLPSTR;
  end;

  CERT_ENHKEY_USAGE = CTL_USAGE;
  PCERT_ENHKEY_USAGE = ^CERT_ENHKEY_USAGE;

  PCRYPT_OID_INFO = ^CRYPT_OID_INFO;

  CRYPT_OID_INFO = record
    cbSize: DWORD;
    pszOID: LPCSTR;
    pwszName: LPCWSTR;
    dwGroupId: DWORD;

    EnumValue: record
      case Integer of
        0:
          (dwValue: DWORD);
        1:
          (Algid: ALG_ID);
        2:
          (dwLength: DWORD);
    end;

    ExtraInfo: CRYPT_DATA_BLOB;
  end;

const

  CRYPT_OID_INFO_OID_KEY = 1;
{$EXTERNALSYM CRYPT_OID_INFO_OID_KEY}

type
  PCRYPT_KEY_PROV_PARAM = ^CRYPT_KEY_PROV_PARAM;

  CRYPT_KEY_PROV_PARAM = record
    dwParam: DWORD;
    pbData: PBYTE;
    cbData: DWORD;
    dwFlags: DWORD;
  end;

  PCRYPT_KEY_PROV_INFO = ^CRYPT_KEY_PROV_INFO;

  CRYPT_KEY_PROV_INFO = record
    pwszContainerName: LPWSTR;
    pwszProvName: LPWSTR;
    dwProvType: DWORD;
    dwFlags: DWORD;
    cProvParam: DWORD;
    rgProvParam: PCRYPT_KEY_PROV_PARAM;
    dwKeySpec: DWORD;
  end;

  PCRYPT_ATTRIBUTE = ^CRYPT_ATTRIBUTE;

  CRYPT_ATTRIBUTE = record
    pszObjId: LPSTR;
    cValue: DWORD;
    rgValue: PCRYPT_ATTR_BLOB;
  end;

  PCRYPT_SIGN_MESSAGE_PARA = ^CRYPT_SIGN_MESSAGE_PARA;

  CRYPT_SIGN_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    pSigningCert: PCERT_CONTEXT;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: Pointer;
    cMsgCert: DWORD;
    rgpMsgCert: PPCCERT_CONTEXT;
    cMsgCrl: DWORD;
    rgpMsgCrl: PPCCRL_CONTEXT;
    cAuthAttr: DWORD;
    rgAuthAttr: PCRYPT_ATTRIBUTE;
    cUnauthAttr: DWORD;
    rgUnauthAttr: PCRYPT_ATTRIBUTE;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
  end;

const
  crypt32 = 'crypt32.dll';
{$EXTERNALSYM crypt32}
  cryptui = 'cryptui.dll';
{$EXTERNALSYM cryptui}
  CRYPT_E_NOT_FOUND = $80092004;
{$EXTERNALSYM CRYPT_E_NOT_FOUND}
function CryptAcquireContext(var phProv: HCRYPTPROV; pszContainer: LPCTSTR;
  pszProvider: LPCTSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptAcquireContext}
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: ULONG_PTR)
  : BOOL; stdcall;
{$EXTERNALSYM CryptReleaseContext}
function CryptCreateHash(hProv: HCRYPTPROV; Algid: ALG_ID; hKey: HCRYPTKEY;
  dwFlags: DWORD; var phHash: HCRYPTHASH): BOOL; stdcall;
{$EXTERNALSYM CryptCreateHash}
function CryptHashData(hHash: HCRYPTHASH; pbData: LPBYTE;
  dwDataLen, dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptHashData}
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;
{$EXTERNALSYM CryptDestroyHash}
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: LPBYTE;
  var pdwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptGetHashParam}
function CryptSetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: LPBYTE;
  dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetHashParam}
function CryptGetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: LPBYTE;
  var pdwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptGetKeyParam}
function CryptDuplicateKey(hKey: HCRYPTKEY; pdwReserved: LPDWORD;
  dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptDuplicateKey}
function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptDestroyKey}
function CryptImportKey(hProv: HCRYPTPROV; pbData: LPBYTE; dwDataLen: DWORD;
  hPubKey: HCRYPTKEY; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptImportKey}
function CryptDeriveKey(hProv: HCRYPTPROV; Algid: ALG_ID; hBaseData: HCRYPTHASH;
  dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptDeriveKey}
function CryptSetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: LPBYTE;
  dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetKeyParam}
function CryptDecodeObject(dwCertEncodingType: DWORD; lpszStructType: LPCSTR;
  const pbEncoded: PBYTE; cbEncoded: DWORD; dwFlags: DWORD;
  pvStructInfo: Pointer; pcbStructInfo: PDWORD): BOOL; stdcall;
{$EXTERNALSYM CryptDecodeObject}
function CryptDecrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
  dwFlags: DWORD; pbData: LPBYTE; var pdwDataLen: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptDecrypt}
function CryptEncrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
  dwFlags: DWORD; pbData: LPBYTE; var pdwDataLen: DWORD; dwBufLen: DWORD)
  : BOOL; stdcall;
{$EXTERNALSYM CryptEncrypt}
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: LPBYTE)
  : BOOL; stdcall;
{$EXTERNALSYM CryptGenRandom}
function CryptSignMessage(pSignPara: PCRYPT_SIGN_MESSAGE_PARA;
  fDetachedSignature: BOOL; cToBeSigned: DWORD; rgpbToBeSigned: PBYTE;
  rgcbToBeSigned: PDWORD; pbSignedBlob: PBYTE; pcbSignedBlob: PDWORD)
  : BOOL; stdcall;
{$EXTERNALSYM CryptSignMessage}
function CryptUIDlgViewCertificate(pViewCertificateInfo
  : PCCRYPTUI_VIEWCERTIFICATE_STRUCT; pfPropertiesChanged: PBOOL)
  : BOOL; stdcall;
{$EXTERNALSYM CryptUIDlgViewCertificate}
function CryptUIDlgSelectCertificate(pSelectCertificateInfo
  : PCCRYPTUI_SELECTCERTIFICATE_STRUCT): HCERTSTORE; stdcall;
{$EXTERNALSYM CryptUIDlgSelectCertificate}
function CertAddCertificateLinkToStore(HCERTSTORE: HCERTSTORE;
  pCertContext: PCERT_CONTEXT; dwAddDisposition: DWORD;
  ppStoreContext: PPCCERT_CONTEXT): BOOL; stdcall;
{$EXTERNALSYM CertAddCertificateLinkToStore}
function CertCloseStore(HCERTSTORE: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CertCloseStore}
function CertControlStore(HCERTSTORE: HCERTSTORE; dwFlags, dwCtrlType: DWORD;
  pvCtrlPara: Pointer): BOOL;
stdcall
{$EXTERNALSYM CertControlStore}
  function CertDuplicateCertificateContext(pCertContext: PCERT_CONTEXT)
  : PCERT_CONTEXT;
stdcall;
{$EXTERNALSYM CertDuplicateCertificateContext}
function CertEnumCertificatesInStore(HCERTSTORE: HCERTSTORE;
  pPrevCertContext: PCERT_CONTEXT): PCERT_CONTEXT; stdcall;
{$EXTERNALSYM CertEnumCertificatesInStore}
function CertGetCertificateContextProperty(pCertContext: PCERT_CONTEXT;
  dwPropId: DWORD; pvData: Pointer; pcbData: PDWORD): BOOL; stdcall;
{$EXTERNALSYM CertGetCertificateContextProperty}
function CertGetEnhancedKeyUsage(pCertContext: PCERT_CONTEXT; dwFlags: DWORD;
  pUsage: PCERT_ENHKEY_USAGE; pcbUsage: PDWORD): BOOL; stdcall;
{$EXTERNALSYM CertGetEnhancedKeyUsage}
function CertGetIntendedKeyUsage(dwCertEncodingType: DWORD;
  pCertInfo: PCERT_INFO; pbKeyUsage: PBYTE; cbKeyUsage: DWORD): BOOL; stdcall;
{$EXTERNALSYM CertGetIntendedKeyUsage}
function CertGetPublicKeyLength(dwCertEncodingType: DWORD;
  pPublicKey: PCERT_PUBLIC_KEY_INFO): DWORD; stdcall;
{$EXTERNALSYM CertGetPublicKeyLength}
function CertFreeCertificateContext(pCertContext: PCERT_CONTEXT): BOOL; stdcall;
{$EXTERNALSYM CertFreeCertificateContext}
function CertFindExtension(pszObjId: LPCSTR; cExtensions: DWORD;
  rgExtensions: PCERT_EXTENSION): PCERT_EXTENSION; stdcall;
{$EXTERNALSYM CertFindExtension}
function CryptFindOIDInfo(dwKeyType: DWORD; pvKey: Pointer; dwGroupId: DWORD)
  : PCRYPT_OID_INFO; stdcall;
{$EXTERNALSYM CryptFindOIDInfo}
function CertGetNameString(pCertContext: PCERT_CONTEXT; dwType: DWORD;
  dwFlags: DWORD; pvTypePara: Pointer; pszNameString: PWideChar;
  cchNameString: DWORD): DWORD; stdcall;
{$EXTERNALSYM CertGetNameString}
function CertNameToStr(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB;
  dwStrType: DWORD; psz: PWideChar; csz: DWORD): DWORD; stdcall;
{$EXTERNALSYM CertNameToStr}
function CertOpenStore(lpszStoreProvider: LPCSTR; dwEncodingType: DWORD;
  HCRYPTPROV: HCRYPTPROV; dwFlags: DWORD; pvPara: Pointer): HCERTSTORE; stdcall;
{$EXTERNALSYM CertOpenStore}
function PFXImportCertStore(pPFX: PCRYPT_DATA_BLOB; szPassword: PWideChar;
  dwFlags: DWORD): HCERTSTORE; stdcall;
{$EXTERNALSYM PFXImportCertStore}
procedure CryptCheck(AResult: LongBool);

implementation

uses
  SysUtils;

function CryptAcquireContext; external advapi32 name 'CryptAcquireContextW';
function CryptReleaseContext; external advapi32 name 'CryptReleaseContext';

function CryptCreateHash; external advapi32 name 'CryptCreateHash';
function CryptDestroyHash; external advapi32 name 'CryptDestroyHash';
function CryptGetHashParam; external advapi32 name 'CryptGetHashParam';
function CryptHashData; external advapi32 name 'CryptHashData';
function CryptSetHashParam; external advapi32 name 'CryptSetHashParam';

function CryptDeriveKey; external advapi32 name 'CryptDeriveKey';
function CryptDestroyKey; external advapi32 name 'CryptDestroyKey';
function CryptDuplicateKey; external advapi32 name 'CryptDuplicateKey';
function CryptGetKeyParam; external advapi32 name 'CryptGetKeyParam';
function CryptImportKey; external advapi32 name 'CryptImportKey';
function CryptSetKeyParam; external advapi32 name 'CryptSetKeyParam';

function CryptDecodeObject; external crypt32 name 'CryptDecodeObject';
function CryptDecrypt; external advapi32 name 'CryptDecrypt';
function CryptEncrypt; external advapi32 name 'CryptEncrypt';
function CryptGenRandom; external advapi32 name 'CryptGenRandom';
function CryptSignMessage; external crypt32 name 'CryptSignMessage';

function CertAddCertificateLinkToStore;
  external crypt32 name 'CertAddCertificateLinkToStore';
function CertCloseStore; external crypt32 name 'CertCloseStore';
function CertControlStore; external crypt32 name 'CertControlStore';
function CertDuplicateCertificateContext;
  external crypt32 name 'CertDuplicateCertificateContext';
function CertEnumCertificatesInStore;
  external crypt32 name 'CertEnumCertificatesInStore';
function CertFindExtension; external crypt32 name 'CertFindExtension';
function CertFreeCertificateContext;
  external crypt32 name 'CertFreeCertificateContext';
function CertGetCertificateContextProperty;
  external crypt32 name 'CertGetCertificateContextProperty';
function CertGetEnhancedKeyUsage;
  external crypt32 name 'CertGetEnhancedKeyUsage';
function CertGetIntendedKeyUsage;
  external crypt32 name 'CertGetIntendedKeyUsage';
function CertGetNameString; external crypt32 name 'CertGetNameStringW';
function CertGetPublicKeyLength; external crypt32 name 'CertGetPublicKeyLength';
function CertNameToStr; external crypt32 name 'CertNameToStrW';
function CertOpenStore; external crypt32 name 'CertOpenStore';
function CryptFindOIDInfo; external crypt32 name 'CryptFindOIDInfo';
function CryptUIDlgViewCertificate;
  external cryptui name 'CryptUIDlgViewCertificateA';
function CryptUIDlgSelectCertificate;
  external cryptui name 'CryptUIDlgSelectCertificateW';
function PFXImportCertStore; external crypt32 name 'PFXImportCertStore';

procedure CryptCheck(AResult: LongBool);
begin
  if not AResult then
    RaiseLastOSError;
end;

end.
