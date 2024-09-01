// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcSSL_WinSSPI.pas' rev: 35.00 (Windows)

#ifndef Sgcssl_winsspiHPP
#define Sgcssl_winsspiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcssl_winsspi
{
//-- forward type declarations -----------------------------------------------
struct CRYPTOAPI_BLOB;
struct CRYPT_ATTRIBUTE;
struct CRYPT_ALGORITHM_IDENTIFIER;
struct CRYPT_BIT_BLOB;
struct CERT_PUBLIC_KEY_INFO;
struct CERT_EXTENSION;
struct CERT_INFO;
struct CERT_CONTEXT;
struct SCHANNEL_CRED;
struct HTTPSPolicyCallbackData;
struct CERT_CHAIN_POLICY_PARA;
struct CERT_CHAIN_POLICY_STATUS;
struct CTL_USAGE;
struct CERT_USAGE_MATCH;
struct CERT_CHAIN_PARA;
struct CERT_TRUST_STATUS;
struct CERT_REVOCATION_INFO;
struct CERT_CHAIN_ELEMENT;
struct CTL_ENTRY;
struct CTL_INFO;
struct CTL_CONTEXT;
struct CERT_TRUST_LIST_INFO;
struct CERT_SIMPLE_CHAIN;
struct CERT_CHAIN_CONTEXT;
struct TSecHandle;
struct TSecPkgInfo;
struct TSecBuffer;
struct TSecBufferDesc;
struct TSecPkgContextStreamSizes;
struct TSecPkgContext_ApplicationProtocol;
struct TSecurityFunctionTable;
struct TRSAPubKey;
struct TSecPkgContextConnectionInfo;
struct TSecPkgContextIssuerListInfoEx;
struct CERT_CHAIN_FIND_BY_ISSUER_PARA;
struct CRYPTPROTECT_PROMPTSTRUCT;
//-- type declarations -------------------------------------------------------
typedef unsigned ALG_ID;

typedef NativeUInt HCERTSTORE;

typedef NativeUInt HCRYPTMSG;

typedef NativeUInt HCERTCHAINENGINE;

typedef NativeUInt HCRYPTPROV;

typedef NativeUInt HCRYPTHASH;

typedef NativeUInt HCRYPTKEY;

typedef NativeUInt HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;

struct DECLSPEC_DRECORD CRYPTOAPI_BLOB
{
public:
	unsigned cbData;
	System::Byte *pbData;
};


typedef CRYPTOAPI_BLOB CRYPT_DATA_BLOB;

typedef CRYPTOAPI_BLOB CRYPT_ATTR_BLOB;

typedef CRYPTOAPI_BLOB CRYPT_INTEGER_BLOB;

typedef CRYPTOAPI_BLOB CRYPT_OBJID_BLOB;

typedef CRYPTOAPI_BLOB CERT_NAME_BLOB;

typedef CRYPTOAPI_BLOB *PCRYPT_ATTR_BLOB;

typedef CRYPTOAPI_BLOB *PCERT_NAME_BLOB;

struct DECLSPEC_DRECORD CRYPT_ATTRIBUTE
{
public:
	char *pszObjId;
	unsigned cValue;
	CRYPTOAPI_BLOB *rgValue;
};


typedef CRYPT_ATTRIBUTE *PCRYPT_ATTRIBUTE;

struct DECLSPEC_DRECORD CRYPT_ALGORITHM_IDENTIFIER
{
public:
	char *pszObjId;
	CRYPTOAPI_BLOB Parameters;
};


struct DECLSPEC_DRECORD CRYPT_BIT_BLOB
{
public:
	unsigned cbData;
	char *pbData;
	unsigned cUnusedBits;
};


struct DECLSPEC_DRECORD CERT_PUBLIC_KEY_INFO
{
public:
	CRYPT_ALGORITHM_IDENTIFIER Algorithm;
	CRYPT_BIT_BLOB PublicKey;
};


typedef CERT_PUBLIC_KEY_INFO *PCERT_PUBLIC_KEY_INFO;

struct DECLSPEC_DRECORD CERT_EXTENSION
{
public:
	char *pszObjId;
	System::LongBool fCritical;
	CRYPTOAPI_BLOB Value;
};


typedef CERT_EXTENSION *PCERT_EXTENSION;

struct DECLSPEC_DRECORD CERT_INFO
{
public:
	unsigned dwVersion;
	CRYPTOAPI_BLOB SerialNumber;
	CRYPT_ALGORITHM_IDENTIFIER SignatureAlgorithm;
	CRYPTOAPI_BLOB Issuer;
	_FILETIME NotBefore;
	_FILETIME NotAfter;
	CRYPTOAPI_BLOB Subject;
	CERT_PUBLIC_KEY_INFO SubjectPublicKeyInfo;
	CRYPT_BIT_BLOB IssuerUniqueId;
	CRYPT_BIT_BLOB SubjectUniqueId;
	unsigned cExtension;
	CERT_EXTENSION *rgExtension;
};


typedef CERT_INFO *PCERT_INFO;

struct DECLSPEC_DRECORD CERT_CONTEXT
{
public:
	unsigned dwCertEncodingType;
	System::Byte *pbCertEncoded;
	unsigned cbCertEncoded;
	CERT_INFO *pCertInfo;
	HCERTSTORE HCERTSTORE;
};


typedef CERT_CONTEXT *PCCERT_CONTEXT;

struct DECLSPEC_DRECORD SCHANNEL_CRED
{
public:
	unsigned dwVersion;
	unsigned cCreds;
	PCCERT_CONTEXT *paCred;
	HCERTSTORE hRootStore;
	unsigned cMappers;
	void *aphMappers;
	unsigned cSupportedAlgs;
	unsigned *palgSupportedAlgs;
	unsigned grbitEnabledProtocols;
	unsigned dwMinimumCipherStrength;
	unsigned dwMaximumCipherStrength;
	unsigned dwSessionLifespan;
	unsigned dwFlags;
	unsigned dwCredFormat;
};


struct DECLSPEC_DRECORD HTTPSPolicyCallbackData
{
public:
	unsigned cbSize;
	unsigned dwAuthType;
	unsigned fdwChecks;
	System::WideChar *pwszServerName;
};


struct DECLSPEC_DRECORD CERT_CHAIN_POLICY_PARA
{
public:
	unsigned cbSize;
	unsigned dwFlags;
	void *pvExtraPolicyPara;
};


typedef CERT_CHAIN_POLICY_PARA *PCERT_CHAIN_POLICY_PARA;

struct DECLSPEC_DRECORD CERT_CHAIN_POLICY_STATUS
{
public:
	unsigned cbSize;
	unsigned dwError;
	int lChainIndex;
	int lElementIndex;
	void *pvExtraPolicyStatus;
};


typedef CERT_CHAIN_POLICY_STATUS *PCERT_CHAIN_POLICY_STATUS;

struct DECLSPEC_DRECORD CTL_USAGE
{
public:
	unsigned cUsageIdentifier;
	char *rgpszUsageIdentifier;
};


typedef CTL_USAGE CERT_ENHKEY_USAGE;

typedef CTL_USAGE *PCERT_ENHKEY_USAGE;

struct DECLSPEC_DRECORD CERT_USAGE_MATCH
{
public:
	unsigned dwType;
	CTL_USAGE Usage;
};


struct DECLSPEC_DRECORD CERT_CHAIN_PARA
{
public:
	unsigned cbSize;
	CERT_USAGE_MATCH RequestedUsage;
};


typedef CERT_CHAIN_PARA *PCERT_CHAIN_PARA;

struct DECLSPEC_DRECORD CERT_TRUST_STATUS
{
public:
	unsigned dwErrorStatus;
	unsigned dwInfoStatus;
};


struct DECLSPEC_DRECORD CERT_REVOCATION_INFO
{
public:
	unsigned cbSize;
	unsigned dwRevocationResult;
	char *pszRevocationOid;
	void *pvOidSpecificInfo;
};


typedef CERT_REVOCATION_INFO *PCERT_REVOCATION_INFO;

struct DECLSPEC_DRECORD CERT_CHAIN_ELEMENT
{
public:
	unsigned cbSize;
	CERT_CONTEXT *pCertContext;
	CERT_TRUST_STATUS TrustStatus;
	CERT_REVOCATION_INFO *pRevocationInfo;
	CTL_USAGE *pIssuanceUsage;
	CTL_USAGE *pApplicationUsage;
	System::WideChar *pwszExtendedErrorInfo;
};


typedef CERT_CHAIN_ELEMENT *PCERT_CHAIN_ELEMENT;

struct DECLSPEC_DRECORD CTL_ENTRY
{
public:
	CRYPTOAPI_BLOB SubjectIdentifier;
	unsigned cAttribute;
	CRYPT_ATTRIBUTE *rgAttribute;
};


typedef CTL_ENTRY *PCTL_ENTRY;

struct DECLSPEC_DRECORD CTL_INFO
{
public:
	unsigned dwVersion;
	CTL_USAGE SubjectUsage;
	CRYPTOAPI_BLOB ListIdentifier;
	CRYPTOAPI_BLOB SequenceNumber;
	_FILETIME ThisUpdate;
	_FILETIME NextUpdate;
	CRYPT_ALGORITHM_IDENTIFIER SubjectAlgorithm;
	unsigned cCTLEntry;
	CTL_ENTRY *rgCTLEntry;
	unsigned cExtension;
	CERT_EXTENSION *rgExtension;
};


typedef CTL_INFO *PCTL_INFO;

struct DECLSPEC_DRECORD CTL_CONTEXT
{
public:
	unsigned dwMsgAndCertEncodingType;
	System::Byte *pbCtlEncoded;
	unsigned cbCtlEncoded;
	CTL_INFO *pCtlInfo;
	HCERTSTORE HCERTSTORE;
	HCRYPTMSG HCRYPTMSG;
	System::Byte *pbCtlContent;
	unsigned cbCtlContent;
};


typedef CTL_CONTEXT *PCCTL_CONTEXT;

struct DECLSPEC_DRECORD CERT_TRUST_LIST_INFO
{
public:
	unsigned cbSize;
	CTL_ENTRY *pCtlEntry;
	CTL_CONTEXT *pCtlContext;
};


typedef CERT_TRUST_LIST_INFO *PCERT_TRUST_LIST_INFO;

struct DECLSPEC_DRECORD CERT_SIMPLE_CHAIN
{
public:
	unsigned cbSize;
	CERT_TRUST_STATUS TrustStatus;
	unsigned cElement;
	PCERT_CHAIN_ELEMENT *rgpElement;
	CERT_TRUST_LIST_INFO *pTrustListInfo;
};


typedef CERT_SIMPLE_CHAIN *PCERT_SIMPLE_CHAIN;

typedef CERT_CHAIN_CONTEXT *PCCERT_CHAIN_CONTEXT;

typedef PCERT_SIMPLE_CHAIN *PPCERT_SIMPLE_CHAIN;

struct DECLSPEC_DRECORD CERT_CHAIN_CONTEXT
{
public:
	unsigned cbSize;
	CERT_TRUST_STATUS TrustStatus;
	unsigned cChain;
	PCERT_SIMPLE_CHAIN *rgpChain;
	unsigned cLowerQualityChainContext;
	PCCERT_CHAIN_CONTEXT *rgpLowerQualityChainContext;
	System::LongBool fHasRevocationFreshnessTime;
	unsigned dwRevocationFreshnessTime;
	unsigned dwCreationFlags;
	GUID ChainId;
};


typedef int SECURITY_STATUS;

typedef __int64 SECURITY_INTEGER;

typedef __int64 *PSECURITY_INTEGER;

typedef __int64 TimeStamp;

typedef __int64 *PTimeStamp;

struct DECLSPEC_DRECORD TSecHandle
{
public:
	NativeUInt dwLower;
	NativeUInt dwUpper;
};


typedef TSecHandle TCredHandle;

typedef TSecHandle TCtxtHandle;

typedef TSecHandle *PSecHandle;

typedef PSecHandle PCredHandle;

typedef PSecHandle PCtxtHandle;

struct DECLSPEC_DRECORD TSecPkgInfo
{
public:
	unsigned fCapabilities;
	System::Word wVersion;
	System::Word wRPCID;
	unsigned cbMaxToken;
	System::WideChar *Name;
	System::WideChar *Comment;
};


typedef TSecPkgInfo *PSecPkgInfo;

struct DECLSPEC_DRECORD TSecBuffer
{
public:
	unsigned cbBuffer;
	unsigned BufferType;
	void *pvBuffer;
};


typedef TSecBuffer *PSecBuffer;

struct DECLSPEC_DRECORD TSecBufferDesc
{
public:
	unsigned ulVersion;
	unsigned cBuffers;
	PSecBuffer *pBuffers;
};


typedef TSecBufferDesc *PSecBufferDesc;

struct DECLSPEC_DRECORD TSecPkgContextStreamSizes
{
public:
	unsigned cbHeader;
	unsigned cbTrailer;
	unsigned cbMaximumMessage;
	unsigned cBuffers;
	unsigned cbBlockSize;
};


typedef void __stdcall (*TSecGetKeyFn)(void * Arg, void * Principal, unsigned KeyVer, /* out */ void * &Key, /* out */ int &Status);

enum DECLSPEC_DENUM SEC_APPLICATION_PROTOCOL_NEGOTIATION_STATUS : unsigned char { SecApplicationProtocolNegotiationStatus_None, SecApplicationProtocolNegotiationStatus_Success, SecApplicationProtocolNegotiationStatus_SelectedClientOnly };

enum DECLSPEC_DENUM SEC_APPLICATION_PROTOCOL_NEGOTIATION_EXT : unsigned char { SecApplicationProtocolNegotiationExt_None, SecApplicationProtocolNegotiationExt_NPN, SecApplicationProtocolNegotiationExt_ALPN };

struct DECLSPEC_DRECORD TSecPkgContext_ApplicationProtocol
{
public:
	int ProtoNegoStatus;
	int ProtoNegoExt;
	System::Byte ProtocolIdSize;
	System::StaticArray<System::Byte, 255> ProtocolId;
};


typedef TSecPkgContext_ApplicationProtocol *PSecPkgContext_ApplicationProtocol;

struct DECLSPEC_DRECORD TSecurityFunctionTable
{
public:
	unsigned dwVersion;
	int __stdcall (*EnumerateSecurityPackages)(/* out */ unsigned &pcPackages, /* out */ PSecPkgInfo &ppPackageInfo);
	int __stdcall (*QueryCredentialsAttributes)(PSecHandle phCredential, unsigned ulAttribute, void * pBuffer);
	int __stdcall (*AcquireCredentialsHandle)(System::WideChar * pszPrincipal, System::WideChar * pszPackage, unsigned fCredentialUse, void * pvLogonId, void * pAuthData, TSecGetKeyFn pGetKeyFn, void * pvGetKeyArgument, PSecHandle phCredential, PTimeStamp ptsExpiry);
	int __stdcall (*FreeCredentialsHandle)(PSecHandle phCredential);
	void *Reserved2;
	int __stdcall (*InitializeSecurityContext)(PSecHandle phCredential, PSecHandle phContext, System::WideChar * pszTargetName, unsigned fContextReq, unsigned Reserved1, unsigned TargetDataRep, PSecBufferDesc pInput, unsigned Reserved2, PSecHandle phNewContext, PSecBufferDesc pOutput, /* out */ unsigned &pfContextAttr, PTimeStamp ptsExpiry);
	int __stdcall (*AcceptSecurityContext)(PSecHandle phCredential, PSecHandle phContext, PSecBufferDesc pInput, unsigned fContextReq, unsigned TargetDataRep, PSecHandle phNewContext, PSecBufferDesc pOutput, /* out */ unsigned &pfContextAttr, PTimeStamp ptsExpiry);
	int __stdcall (*CompleteAuthToken)(PSecHandle phContext, PSecBufferDesc pToken);
	int __stdcall (*DeleteSecurityContext)(PSecHandle phContext);
	int __stdcall (*ApplyControlToken)(PSecHandle phContext, PSecBufferDesc pInput);
	int __stdcall (*QueryContextAttributes)(PSecHandle phContext, unsigned ulAttribute, void * pBuffer);
	int __stdcall (*ImpersonateSecurityContext)(PSecHandle phContext);
	int __stdcall (*RevertSecurityContext)(PSecHandle phContext);
	int __stdcall (*MakeSignature)(PSecHandle phContext, unsigned fQOP, PSecBufferDesc pMessage, unsigned MessageSeqNo);
	int __stdcall (*VerifySignature)(PSecHandle phContext, PSecBufferDesc pMessage, unsigned MessageSeqNo, /* out */ unsigned &pfQOP);
	int __stdcall (*FreeContextBuffer)(void * pvContextBuffer);
	int __stdcall (*QuerySecurityPackageInfo)(System::WideChar * pszPackageName, /* out */ PSecPkgInfo &ppPackageInfo);
	void *Reserved3;
	void *Reserved4;
	int __stdcall (*ExportSecurityContext)(PSecHandle phContext, unsigned fFlags, PSecBuffer pPackedContext, /* out */ System::PPointer &pToken);
	int __stdcall (*ImportSecurityContext)(System::WideChar * pszPackage, PSecBuffer pPackedContext, void * Token, /* out */ PSecHandle &phContext);
	int __stdcall (*AddCredentials)(PSecHandle hCredentials, System::WideChar * pszPrincipal, System::WideChar * pszPackage, unsigned fCredentialUse, void * pAuthData, TSecGetKeyFn pGetKeyFn, void * pvGetKeyArgument, PTimeStamp ptsExpiry);
	void *Reserved8;
	int __stdcall (*QuerySecurityContextToken)(PSecHandle phContext, /* out */ void * &Token);
	int __stdcall (*EncryptMessage)(PSecHandle phContext, unsigned fQOP, PSecBufferDesc pMessage, unsigned MessageSeqNo);
	int __stdcall (*DecryptMessage)(PSecHandle phContext, PSecBufferDesc pMessage, unsigned MessageSeqNo, System::PCardinal pfQOP);
	int __stdcall (*SetContextAttributes)(PSecHandle phContext, unsigned ulAttribute, void * pBuffer, unsigned cbBuffer);
	int __stdcall (*SetCredentialsAttributes)(PSecHandle phCredential, unsigned ulAttribute, void * pBuffer, unsigned cbBuffer);
	void *Reserved9;
};


typedef TSecurityFunctionTable *PSecurityFunctionTable;

typedef PSecurityFunctionTable __stdcall (*INIT_SECURITY_INTERFACE)(void);

struct DECLSPEC_DRECORD TRSAPubKey
{
public:
	int Magic;
	int BitLen;
	int PubExp;
};


struct DECLSPEC_DRECORD TSecPkgContextConnectionInfo
{
public:
	unsigned dwProtocol;
	unsigned aiCipher;
	unsigned dwCipherStrength;
	unsigned aiHash;
	unsigned dwHashStrength;
	unsigned aiExch;
	unsigned dwExchStrength;
};


struct DECLSPEC_DRECORD TSecPkgContextIssuerListInfoEx
{
public:
	CRYPTOAPI_BLOB *aIssuers;
	unsigned cIssuers;
};


typedef int __stdcall (*PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK)(PCCERT_CONTEXT pCert, void * pvFindArg);

struct DECLSPEC_DRECORD CERT_CHAIN_FIND_BY_ISSUER_PARA
{
public:
	unsigned cbSize;
	char *pszUsageIdentifier;
	unsigned dwKeySpec;
	unsigned dwAcquirePrivateKeyFlags;
	unsigned cIssuer;
	CRYPTOAPI_BLOB *rgIssuer;
	PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK pfnFindCallback;
	void *pvFindArg;
	unsigned *pdwIssuerChainIndex;
	unsigned *pdwIssuerElementIndex;
};


typedef CRYPTOAPI_BLOB DATA_BLOB;

typedef CRYPTOAPI_BLOB *PDATA_BLOB;

struct DECLSPEC_DRECORD CRYPTPROTECT_PROMPTSTRUCT
{
public:
	unsigned cbSize;
	unsigned dwPromptFlags;
	HWND hwndApp;
	System::WideChar *szPrompt;
};


typedef CRYPTPROTECT_PROMPTSTRUCT *PCRYPTPROTECT_PROMPTSTRUCT;

typedef PSecurityFunctionTable __stdcall (*TsgcInitSecurityInterface)(void);

typedef unsigned __stdcall (*TsgcCertNameToStr)(unsigned dwCertEncodingType, const CRYPTOAPI_BLOB &pName, unsigned dwStrType, System::WideChar * psz, unsigned csz);

typedef PCCERT_CONTEXT __stdcall (*TsgcCertGetIssuerCertificateFromStore)(HCERTSTORE HCERTSTORE, PCCERT_CONTEXT pSubjectContext, PCCERT_CONTEXT pPrevIssuerContext, unsigned* pdwFlags);

typedef System::LongBool __stdcall (*TsgcCertFreeCertificateContext)(PCCERT_CONTEXT pCertContext);

typedef System::LongBool __stdcall (*TsgcCertGetCertificateChain)(HCERTCHAINENGINE hChainEngine, PCCERT_CONTEXT pCertContext, Winapi::Windows::PFileTime pTime, HCERTSTORE hAdditionalStore, const CERT_CHAIN_PARA &pChainPara, unsigned dwFlags, void * pvReserved, PCCERT_CHAIN_CONTEXT &ppChainContext);

typedef System::LongBool __stdcall (*TsgcCertVerifyCertificateChainPolicy)(char * pszPolicyOID, PCCERT_CHAIN_CONTEXT pChainContext, const CERT_CHAIN_POLICY_PARA &pPolicyPara, const CERT_CHAIN_POLICY_STATUS &pPolicyStatus);

typedef System::LongBool __stdcall (*TsgcCertFreeCertificateChainEngine)(HCERTCHAINENGINE hChainEngine);

typedef System::LongBool __stdcall (*TsgcCertFreeCertificateChain)(PCCERT_CHAIN_CONTEXT pChainContext);

typedef HCERTSTORE __stdcall (*TsgcCertOpenStore)(char * lpszStoreProvider, unsigned dwEncodingType, HCRYPTPROV hProv, unsigned dwFlags, const void * pvPara);

typedef HCERTSTORE __stdcall (*TsgcCertOpenSystemStore)(HCRYPTPROV hProv, System::WideChar * szSubsystemProtocol);

typedef PCCERT_CONTEXT __stdcall (*TsgcCertFindCertificateInStore)(HCERTSTORE HCERTSTORE, unsigned dwCertEncodingType, unsigned dwFindFlags, unsigned dwFindType, const void * pvFindPara, PCCERT_CONTEXT pPrevCertContext);

typedef PCCERT_CHAIN_CONTEXT __stdcall (*TsgcCertFindChainInStore)(HCERTSTORE HCERTSTORE, unsigned dwCertEncodingType, unsigned dwFindFlags, unsigned dwFindType, const void * pvFindPara, PCCERT_CHAIN_CONTEXT pPrevChainContext);

typedef System::LongBool __stdcall (*TsgcCertCloseStore)(HCERTSTORE HCERTSTORE, unsigned dwFlags);

typedef System::LongBool __stdcall (*TsgcCryptProtectData)(CRYPTOAPI_BLOB &pDataIn, System::WideChar * ppszDataDecr, PDATA_BLOB pOptionalEntropy, int pvReserved, PCRYPTPROTECT_PROMPTSTRUCT pPromptStruct, unsigned dwFlags, CRYPTOAPI_BLOB &pDataOut);

typedef System::LongBool __stdcall (*TsgcCryptUnprotectData)(CRYPTOAPI_BLOB &pDataIn, System::PPWideChar ppszDataDecr, PDATA_BLOB pOptionalEntropy, int pvReserved, PCRYPTPROTECT_PROMPTSTRUCT pPromptStruct, unsigned dwFlags, CRYPTOAPI_BLOB &pDataOut);

typedef HCERTSTORE __stdcall (*TsgcPFXImportCertStore)(CRYPTOAPI_BLOB &pPFX, System::WideChar * szPassword, unsigned dwFlags);

typedef System::LongBool __stdcall (*TsgcCryptStringToBinary)(System::WideChar * pszString, unsigned cchString, unsigned dwFlags, System::PByte pbBinary, unsigned &pcbBinary, unsigned* pdwSkip, unsigned* pdwFlags);

//-- var, const, procedure ---------------------------------------------------
static const System::Byte MAX_PROTOCOL_ID_SIZE = System::Byte(0xff);
static const System::Int8 PROV_RSA_FULL = System::Int8(0x1);
static const System::Int8 CRYPT_SILENT = System::Int8(0x40);
static const unsigned CRYPT_VERIFYCONTEXT = unsigned(0xf0000000);
static const System::Int8 CRYPT_ACQUIRE_CACHE_FLAG = System::Int8(0x1);
static const System::Int8 CRYPT_ACQUIRE_USE_PROV_INFO_FLAG = System::Int8(0x2);
static const System::Int8 CRYPT_ACQUIRE_COMPARE_KEY_FLAG = System::Int8(0x4);
static const System::Int8 CRYPT_ACQUIRE_SILENT_FLAG = System::Int8(0x40);
static const System::Int8 AT_SIGNATURE = System::Int8(0x2);
static const System::Int8 CRYPT_EXPORTABLE = System::Int8(0x1);
static const int RSA1024BIT_KEY = int(0x4000000);
static const System::Int8 HP_HASHSIZE = System::Int8(0x4);
static const System::Int8 HP_HASHVAL = System::Int8(0x2);
static const System::Int8 PUBLICKEYBLOB = System::Int8(0x6);
extern "C" System::LongBool __stdcall CryptAcquireContext(HCRYPTPROV &phProv, System::WideChar * pszContainer, System::WideChar * pszProvider, int dwProvType, int dwFlags);
extern "C" System::LongBool __stdcall CryptAcquireCertificatePrivateKey(PCCERT_CONTEXT pCert, unsigned dwFlags, void * pvParameters, NativeUInt &phCryptProvOrNCryptKey, unsigned &pdwKeySpec, System::LongBool &pfCallerFreeProvOrNCryptKey);
extern "C" PCCERT_CONTEXT __stdcall CertCreateCertificateContext(unsigned dwCertEncodingType, void * pbCertEncoded, unsigned cbCertEncoded);
extern "C" System::LongBool __stdcall CryptImportPublicKeyInfo(HCRYPTPROV HCRYPTPROV, unsigned dwCertEncodingType, PCERT_PUBLIC_KEY_INFO pInfo, HCRYPTKEY &phKey);
extern "C" System::LongBool __stdcall CryptGenKey(HCRYPTPROV hProv, int Algid, int dwFlags, HCRYPTKEY &phKey);
extern "C" System::LongBool __stdcall CryptCreateHash(HCRYPTPROV hProv, int Algid, HCRYPTKEY hKey, int dwFlags, HCRYPTHASH &phHash);
extern "C" System::LongBool __stdcall CryptHashData(HCRYPTHASH hHash, const void * pbData, int dwDataLen, int dwFlags);
extern "C" System::LongBool __stdcall CryptGetHashParam(HCRYPTHASH hHash, int dwParam, void * pbData, unsigned &pdwDataLen, int dwFlags);
extern "C" System::LongBool __stdcall CryptSignHash(HCRYPTHASH hHash, int dwKeySpec, System::WideChar * sDescription, int dwFlags, void * pbSignature, unsigned &pdwSigLen);
extern "C" System::LongBool __stdcall CryptExportKey(HCRYPTKEY hKey, HCRYPTKEY hExpKey, int dwBlobType, int dwFlags, void * pbData, unsigned &pdwDataLen);
extern "C" System::LongBool __stdcall CryptDestroyKey(HCRYPTKEY hKey);
extern "C" System::LongBool __stdcall CryptDestroyHash(HCRYPTHASH hHash);
extern "C" System::LongBool __stdcall CryptReleaseContext(HCRYPTPROV hProv, int dwFlags);
static const System::Int8 SECPKG_CRED_INBOUND = System::Int8(0x1);
static const System::Int8 SECPKG_CRED_OUTBOUND = System::Int8(0x2);
static const System::Int8 SECPKG_CRED_BOTH = System::Int8(0x3);
static const System::Int8 SECPKG_CRED_DEFAULT = System::Int8(0x4);
static const unsigned SECPKG_CRED_RESERVED = unsigned(0xf0000000);
static const System::Int8 SECBUFFER_VERSION = System::Int8(0x0);
static const System::Int8 SECBUFFER_EMPTY = System::Int8(0x0);
static const System::Int8 SECBUFFER_DATA = System::Int8(0x1);
static const System::Int8 SECBUFFER_TOKEN = System::Int8(0x2);
static const System::Int8 SECBUFFER_PKG_PARAMS = System::Int8(0x3);
static const System::Int8 SECBUFFER_MISSING = System::Int8(0x4);
static const System::Int8 SECBUFFER_EXTRA = System::Int8(0x5);
static const System::Int8 SECBUFFER_STREAM_TRAILER = System::Int8(0x6);
static const System::Int8 SECBUFFER_STREAM_HEADER = System::Int8(0x7);
static const System::Int8 SECBUFFER_NEGOTIATION_INFO = System::Int8(0x8);
static const System::Int8 SECBUFFER_PADDING = System::Int8(0x9);
static const System::Int8 SECBUFFER_STREAM = System::Int8(0xa);
static const System::Int8 SECBUFFER_MECHLIST = System::Int8(0xb);
static const System::Int8 SECBUFFER_MECHLIST_SIGNATURE = System::Int8(0xc);
static const System::Int8 SECBUFFER_TARGET = System::Int8(0xd);
static const System::Int8 SECBUFFER_CHANNEL_BINDINGS = System::Int8(0xe);
static const System::Int8 SECBUFFER_CHANGE_PASS_RESPONSE = System::Int8(0xf);
static const System::Int8 SECBUFFER_TARGET_HOST = System::Int8(0x10);
static const System::Int8 SECBUFFER_ALERT = System::Int8(0x11);
static const System::Int8 SECBUFFER_APPLICATION_PROTOCOLS = System::Int8(0x12);
static const System::Int8 SECBUFFER_SRTP_PROTECTION_PROFILES = System::Int8(0x13);
static const System::Int8 SECBUFFER_SRTP_MASTER_KEY_IDENTIFIER = System::Int8(0x14);
static const System::Int8 SECBUFFER_TOKEN_BINDING = System::Int8(0x15);
static const System::Int8 SECBUFFER_PRESHARED_KEY = System::Int8(0x16);
static const System::Int8 SECBUFFER_PRESHARED_KEY_IDENTITY = System::Int8(0x17);
static const System::Int8 SECBUFFER_DTLS_MTU = System::Int8(0x18);
static const unsigned SECBUFFER_ATTRMASK = unsigned(0xf0000000);
static const unsigned SECBUFFER_READONLY = unsigned(0x80000000);
static const int SECBUFFER_READONLY_WITH_CHECKSUM = int(0x10000000);
static const int SECBUFFER_RESERVED = int(0x60000000);
static const System::Int8 SECURITY_NATIVE_DREP = System::Int8(0x10);
static const System::Int8 SECURITY_NETWORK_DREP = System::Int8(0x0);
static const System::Int8 SECPKG_ATTR_SIZES = System::Int8(0x0);
static const System::Int8 SECPKG_ATTR_NAMES = System::Int8(0x1);
static const System::Int8 SECPKG_ATTR_LIFESPAN = System::Int8(0x2);
static const System::Int8 SECPKG_ATTR_DCE_INFO = System::Int8(0x3);
static const System::Int8 SECPKG_ATTR_STREAM_SIZES = System::Int8(0x4);
static const System::Int8 SECPKG_ATTR_KEY_INFO = System::Int8(0x5);
static const System::Int8 SECPKG_ATTR_AUTHORITY = System::Int8(0x6);
static const System::Int8 SECPKG_ATTR_PROTO_INFO = System::Int8(0x7);
static const System::Int8 SECPKG_ATTR_PASSWORD_EXPIRY = System::Int8(0x8);
static const System::Int8 SECPKG_ATTR_SESSION_KEY = System::Int8(0x9);
static const System::Int8 SECPKG_ATTR_PACKAGE_INFO = System::Int8(0xa);
static const System::Int8 SECPKG_ATTR_USER_FLAGS = System::Int8(0xb);
static const System::Int8 SECPKG_ATTR_NEGOTIATION_INFO = System::Int8(0xc);
static const System::Int8 SECPKG_ATTR_NATIVE_NAMES = System::Int8(0xd);
static const System::Int8 SECPKG_ATTR_FLAGS = System::Int8(0xe);
static const System::Int8 SECPKG_ATTR_USE_VALIDATED = System::Int8(0xf);
static const System::Int8 SECPKG_ATTR_CREDENTIAL_NAME = System::Int8(0x10);
static const System::Int8 SECPKG_ATTR_TARGET_INFORMATION = System::Int8(0x11);
static const System::Int8 SECPKG_ATTR_ACCESS_TOKEN = System::Int8(0x12);
static const System::Int8 SECPKG_ATTR_TARGET = System::Int8(0x13);
static const System::Int8 SECPKG_ATTR_AUTHENTICATION_ID = System::Int8(0x14);
static const System::Int8 SECPKG_ATTR_LOGOFF_TIME = System::Int8(0x15);
static const System::Int8 SECPKG_ATTR_NEGO_KEYS = System::Int8(0x16);
static const System::Int8 SECPKG_ATTR_PROMPTING_NEEDED = System::Int8(0x18);
static const System::Int8 SECPKG_ATTR_UNIQUE_BINDINGS = System::Int8(0x19);
static const System::Int8 SECPKG_ATTR_ENDPOINT_BINDINGS = System::Int8(0x1a);
static const System::Int8 SECPKG_ATTR_CLIENT_SPECIFIED_TARGET = System::Int8(0x1b);
static const System::Int8 SECPKG_ATTR_LAST_CLIENT_TOKEN_STATUS = System::Int8(0x1e);
static const System::Int8 SECPKG_ATTR_NEGO_PKG_INFO = System::Int8(0x1f);
static const System::Int8 SECPKG_ATTR_NEGO_STATUS = System::Int8(0x20);
static const System::Int8 SECPKG_ATTR_CONTEXT_DELETED = System::Int8(0x21);
static const System::Int8 SECPKG_ATTR_DTLS_MTU = System::Int8(0x22);
static const System::Int8 SECPKG_ATTR_APPLICATION_PROTOCOL = System::Int8(0x23);
static const System::Int8 SECPKG_ATTR_DATAGRAM_SIZES = System::Int8(0x4);
static const System::Byte SECPKG_ATTR_SUBJECT_SECURITY_ATTRIBUTES = System::Byte(0x80);
static const System::Int8 ISC_REQ_DELEGATE = System::Int8(0x1);
static const System::Int8 ISC_REQ_MUTUAL_AUTH = System::Int8(0x2);
static const System::Int8 ISC_REQ_REPLAY_DETECT = System::Int8(0x4);
static const System::Int8 ISC_REQ_SEQUENCE_DETECT = System::Int8(0x8);
static const System::Int8 ISC_REQ_CONFIDENTIALITY = System::Int8(0x10);
static const System::Int8 ISC_REQ_USE_SESSION_KEY = System::Int8(0x20);
static const System::Int8 ISC_REQ_PROMPT_FOR_CREDS = System::Int8(0x40);
static const System::Byte ISC_REQ_USE_SUPPLIED_CREDS = System::Byte(0x80);
static const System::Word ISC_REQ_ALLOCATE_MEMORY = System::Word(0x100);
static const System::Word ISC_REQ_USE_DCE_STYLE = System::Word(0x200);
static const System::Word ISC_REQ_DATAGRAM = System::Word(0x400);
static const System::Word ISC_REQ_CONNECTION = System::Word(0x800);
static const System::Word ISC_REQ_CALL_LEVEL = System::Word(0x1000);
static const System::Word ISC_REQ_FRAGMENT_SUPPLIED = System::Word(0x2000);
static const System::Word ISC_REQ_EXTENDED_ERROR = System::Word(0x4000);
static const System::Word ISC_REQ_STREAM = System::Word(0x8000);
static const int ISC_REQ_INTEGRITY = int(0x10000);
static const int ISC_REQ_IDENTIFY = int(0x20000);
static const int ISC_REQ_NULL_SESSION = int(0x40000);
static const int ISC_REQ_MANUAL_CRED_VALIDATION = int(0x80000);
static const int ISC_REQ_RESERVED1 = int(0x100000);
static const int ISC_REQ_FRAGMENT_TO_FIT = int(0x200000);
static const int ISC_REQ_FORWARD_CREDENTIALS = int(0x400000);
static const int ISC_REQ_NO_INTEGRITY = int(0x800000);
static const int ISC_REQ_USE_HTTP_STYLE = int(0x1000000);
static const int ISC_REQ_UNVERIFIED_TARGET_NAME = int(0x20000000);
static const int ISC_REQ_CONFIDENTIALITY_ONLY = int(0x40000000);
static const System::Int8 ISC_RET_DELEGATE = System::Int8(0x1);
static const System::Int8 ISC_RET_MUTUAL_AUTH = System::Int8(0x2);
static const System::Int8 ISC_RET_REPLAY_DETECT = System::Int8(0x4);
static const System::Int8 ISC_RET_SEQUENCE_DETECT = System::Int8(0x8);
static const System::Int8 ISC_RET_CONFIDENTIALITY = System::Int8(0x10);
static const System::Int8 ISC_RET_USE_SESSION_KEY = System::Int8(0x20);
static const System::Int8 ISC_RET_USED_COLLECTED_CREDS = System::Int8(0x40);
static const System::Byte ISC_RET_USED_SUPPLIED_CREDS = System::Byte(0x80);
static const System::Word ISC_RET_ALLOCATED_MEMORY = System::Word(0x100);
static const System::Word ISC_RET_USED_DCE_STYLE = System::Word(0x200);
static const System::Word ISC_RET_DATAGRAM = System::Word(0x400);
static const System::Word ISC_RET_CONNECTION = System::Word(0x800);
static const System::Word ISC_RET_INTERMEDIATE_RETURN = System::Word(0x1000);
static const System::Word ISC_RET_CALL_LEVEL = System::Word(0x2000);
static const System::Word ISC_RET_EXTENDED_ERROR = System::Word(0x4000);
static const System::Word ISC_RET_STREAM = System::Word(0x8000);
static const int ISC_RET_INTEGRITY = int(0x10000);
static const int ISC_RET_IDENTIFY = int(0x20000);
static const int ISC_RET_NULL_SESSION = int(0x40000);
static const int ISC_RET_MANUAL_CRED_VALIDATION = int(0x80000);
static const int ISC_RET_RESERVED1 = int(0x100000);
static const int ISC_RET_FRAGMENT_ONLY = int(0x200000);
static const int ISC_RET_FORWARD_CREDENTIALS = int(0x400000);
static const int ISC_RET_USED_HTTP_STYLE = int(0x1000000);
static const int ISC_RET_NO_ADDITIONAL_TOKEN = int(0x2000000);
static const int ISC_RET_REAUTHENTICATION = int(0x8000000);
static const int ISC_RET_CONFIDENTIALITY_ONLY = int(0x40000000);
static const System::Int8 CERT_SIMPLE_NAME_STR = System::Int8(0x1);
static const System::Int8 CERT_OID_NAME_STR = System::Int8(0x2);
static const System::Int8 CERT_X500_NAME_STR = System::Int8(0x3);
static const int CERT_NAME_STR_REVERSE_FLAG = int(0x2000000);
static const int CERT_NAME_STR_COMMA_FLAG = int(0x4000000);
static const int CERT_NAME_STR_CRLF_FLAG = int(0x8000000);
static const int CERT_NAME_STR_NO_QUOTING_FLAG = int(0x10000000);
static const int CERT_NAME_STR_NO_PLUS_FLAG = int(0x20000000);
static const int CERT_NAME_STR_SEMICOLON_FLAG = int(0x40000000);
static const System::Int8 ALG_CLASS_ANY = System::Int8(0x0);
static const System::Word ALG_CLASS_SIGNATURE = System::Word(0x2000);
static const System::Word ALG_CLASS_MSG_ENCRYPT = System::Word(0x4000);
static const System::Word ALG_CLASS_DATA_ENCRYPT = System::Word(0x6000);
static const System::Word ALG_CLASS_HASH = System::Word(0x8000);
static const System::Word ALG_CLASS_KEY_EXCHANGE = System::Word(0xa000);
static const System::Int8 ALG_TYPE_ANY = System::Int8(0x0);
static const System::Word ALG_TYPE_DSS = System::Word(0x200);
static const System::Word ALG_TYPE_RSA = System::Word(0x400);
static const System::Word ALG_TYPE_BLOCK = System::Word(0x600);
static const System::Word ALG_TYPE_STREAM = System::Word(0x800);
static const System::Word ALG_TYPE_DH = System::Word(0xa00);
static const System::Word ALG_TYPE_SECURECHANNEL = System::Word(0xc00);
static const System::Int8 ALG_SID_ANY = System::Int8(0x0);
static const System::Int8 ALG_SID_RSA_ANY = System::Int8(0x0);
static const System::Int8 ALG_SID_RSA_PKCS = System::Int8(0x1);
static const System::Int8 ALG_SID_RSA_MSATWORK = System::Int8(0x2);
static const System::Int8 ALG_SID_RSA_ENTRUST = System::Int8(0x3);
static const System::Int8 ALG_SID_RSA_PGP = System::Int8(0x4);
static const System::Int8 ALG_SID_DSS_ANY = System::Int8(0x0);
static const System::Int8 ALG_SID_DSS_PKCS = System::Int8(0x1);
static const System::Int8 ALG_SID_DSS_DMS = System::Int8(0x2);
static const System::Int8 ALG_SID_DES = System::Int8(0x1);
static const System::Int8 ALG_SID_3DES = System::Int8(0x3);
static const System::Int8 ALG_SID_DESX = System::Int8(0x4);
static const System::Int8 ALG_SID_IDEA = System::Int8(0x5);
static const System::Int8 ALG_SID_CAST = System::Int8(0x6);
static const System::Int8 ALG_SID_SAFERSK64 = System::Int8(0x7);
static const System::Int8 ALD_SID_SAFERSK128 = System::Int8(0x8);
static const System::Int8 ALG_SID_SAFERSK128 = System::Int8(0x8);
static const System::Int8 ALG_SID_3DES_112 = System::Int8(0x9);
static const System::Int8 ALG_SID_CYLINK_MEK = System::Int8(0xc);
static const System::Int8 ALG_SID_RC5 = System::Int8(0xd);
static const System::Int8 ALG_SID_AES_128 = System::Int8(0xe);
static const System::Int8 ALG_SID_AES_192 = System::Int8(0xf);
static const System::Int8 ALG_SID_AES_256 = System::Int8(0x10);
static const System::Int8 ALG_SID_AES = System::Int8(0x11);
static const System::Int8 ALG_SID_SKIPJACK = System::Int8(0xa);
static const System::Int8 ALG_SID_TEK = System::Int8(0xb);
static const System::Int8 CRYPT_MODE_CBCI = System::Int8(0x6);
static const System::Int8 CRYPT_MODE_CFBP = System::Int8(0x7);
static const System::Int8 CRYPT_MODE_OFBP = System::Int8(0x8);
static const System::Int8 CRYPT_MODE_CBCOFM = System::Int8(0x9);
static const System::Int8 CRYPT_MODE_CBCOFMI = System::Int8(0xa);
static const System::Int8 ALG_SID_RC2 = System::Int8(0x2);
static const System::Int8 ALG_SID_RC4 = System::Int8(0x1);
static const System::Int8 ALG_SID_SEAL = System::Int8(0x2);
static const System::Int8 ALG_SID_DH_SANDF = System::Int8(0x1);
static const System::Int8 ALG_SID_DH_EPHEM = System::Int8(0x2);
static const System::Int8 ALG_SID_AGREED_KEY_ANY = System::Int8(0x3);
static const System::Int8 ALG_SID_KEA = System::Int8(0x4);
static const System::Int8 ALG_SID_MD2 = System::Int8(0x1);
static const System::Int8 ALG_SID_MD4 = System::Int8(0x2);
static const System::Int8 ALG_SID_MD5 = System::Int8(0x3);
static const System::Int8 ALG_SID_SHA = System::Int8(0x4);
static const System::Int8 ALG_SID_SHA1 = System::Int8(0x4);
static const System::Int8 ALG_SID_MAC = System::Int8(0x5);
static const System::Int8 ALG_SID_RIPEMD = System::Int8(0x6);
static const System::Int8 ALG_SID_RIPEMD160 = System::Int8(0x7);
static const System::Int8 ALG_SID_SSL3SHAMD5 = System::Int8(0x8);
static const System::Int8 ALG_SID_HMAC = System::Int8(0x9);
static const System::Int8 ALG_SID_SHA_256 = System::Int8(0xc);
static const System::Int8 ALG_SID_SHA_384 = System::Int8(0xd);
static const System::Int8 ALG_SID_SHA_512 = System::Int8(0xe);
static const System::Int8 ALG_SID_SSL3_MASTER = System::Int8(0x1);
static const System::Int8 ALG_SID_SCHANNEL_MASTER_HASH = System::Int8(0x2);
static const System::Int8 ALG_SID_SCHANNEL_MAC_KEY = System::Int8(0x3);
static const System::Int8 ALG_SID_PCT1_MASTER = System::Int8(0x4);
static const System::Int8 ALG_SID_SSL2_MASTER = System::Int8(0x5);
static const System::Int8 ALG_SID_TLS1_MASTER = System::Int8(0x6);
static const System::Int8 ALG_SID_SCHANNEL_ENC_KEY = System::Int8(0x7);
static const System::Int8 ALG_SID_EXAMPLE = System::Int8(0x50);
static const System::Word CALG_MD2 = System::Word(0x8001);
static const System::Word CALG_MD4 = System::Word(0x8002);
static const System::Word CALG_MD5 = System::Word(0x8003);
static const System::Word CALG_SHA = System::Word(0x8004);
static const System::Word CALG_SHA1 = System::Word(0x8004);
static const System::Word CALG_MAC = System::Word(0x8005);
static const System::Word CALG_RSA_SIGN = System::Word(0x2400);
static const System::Word CALG_DSS_SIGN = System::Word(0x2200);
static const System::Word CALG_RSA_KEYX = System::Word(0xa400);
static const System::Word CALG_DES = System::Word(0x6601);
static const System::Word CALG_3DES_112 = System::Word(0x6609);
static const System::Word CALG_3DES = System::Word(0x6603);
static const System::Word CALG_RC2 = System::Word(0x6602);
static const System::Word CALG_RC4 = System::Word(0x6801);
static const System::Word CALG_SEAL = System::Word(0x6802);
static const System::Word CALG_DH_SF = System::Word(0xaa01);
static const System::Word CALG_DH_EPHEM = System::Word(0xaa02);
static const System::Word CALG_AGREEDKEY_ANY = System::Word(0xaa03);
static const System::Word CALG_KEA_KEYX = System::Word(0xaa04);
static const System::Word CALG_HUGHES_MD5 = System::Word(0xa003);
static const System::Word CALG_SKIPJACK = System::Word(0x660a);
static const System::Word CALG_TEK = System::Word(0x660b);
static const System::Word CALG_CYLINK_MEK = System::Word(0x660c);
static const System::Word CALG_SSL3_SHAMD5 = System::Word(0x8008);
static const System::Word CALG_SSL3_MASTER = System::Word(0x4c01);
static const System::Word CALG_SCHANNEL_MASTER_HASH = System::Word(0x4c02);
static const System::Word CALG_SCHANNEL_MAC_KEY = System::Word(0x4c03);
static const System::Word CALG_SCHANNEL_ENC_KEY = System::Word(0x4c07);
static const System::Word CALG_PCT1_MASTER = System::Word(0x4c04);
static const System::Word CALG_SSL2_MASTER = System::Word(0x4c05);
static const System::Word CALG_TLS1_MASTER = System::Word(0x4c06);
static const System::Word CALG_RC5 = System::Word(0x660d);
static const System::Word CALG_HMAC = System::Word(0x8009);
static const System::Word CALG_AES_128 = System::Word(0x660e);
static const System::Word CALG_AES_192 = System::Word(0x660f);
static const System::Word CALG_AES_256 = System::Word(0x6610);
static const System::Word CALG_AES = System::Word(0x6611);
static const System::Word CALG_SHA_256 = System::Word(0x800c);
static const System::Word CALG_SHA_384 = System::Word(0x800d);
static const System::Word CALG_SHA_512 = System::Word(0x800e);
static const System::Int8 SP_PROT_PCT1_SERVER = System::Int8(0x1);
static const System::Int8 SP_PROT_PCT1_CLIENT = System::Int8(0x2);
static const System::Int8 SP_PROT_PCT1 = System::Int8(0x3);
static const System::Int8 SP_PROT_SSL2_SERVER = System::Int8(0x4);
static const System::Int8 SP_PROT_SSL2_CLIENT = System::Int8(0x8);
static const System::Int8 SP_PROT_SSL2 = System::Int8(0xc);
static const System::Int8 SP_PROT_SSL3_SERVER = System::Int8(0x10);
static const System::Int8 SP_PROT_SSL3_CLIENT = System::Int8(0x20);
static const System::Int8 SP_PROT_SSL3 = System::Int8(0x30);
static const System::Int8 SP_PROT_TLS1_SERVER = System::Int8(0x40);
static const System::Byte SP_PROT_TLS1_CLIENT = System::Byte(0x80);
static const System::Byte SP_PROT_TLS1 = System::Byte(0xc0);
static const System::Word SP_PROT_TLS1_1_SERVER = System::Word(0x100);
static const System::Word SP_PROT_TLS1_1_CLIENT = System::Word(0x200);
static const System::Word SP_PROT_TLS1_1 = System::Word(0x300);
static const System::Word SP_PROT_TLS1_2_SERVER = System::Word(0x400);
static const System::Word SP_PROT_TLS1_2_CLIENT = System::Word(0x800);
static const System::Word SP_PROT_TLS1_2 = System::Word(0xc00);
static const System::Word SP_PROT_TLS1_3_SERVER = System::Word(0x1000);
static const System::Word SP_PROT_TLS1_3_CLIENT = System::Word(0x2000);
static const System::Word SP_PROT_TLS1_3 = System::Word(0x3000);
static const int SP_PROT_DTLS_SERVER = int(0x10000);
static const int SP_PROT_DTLS_CLIENT = int(0x20000);
static const int SP_PROT_DTLS = int(0x30000);
static const int SP_PROT_DTLS1_2_SERVER = int(0x40000);
static const int SP_PROT_DTLS1_2_CLIENT = int(0x80000);
static const int SP_PROT_DTLS1_2 = int(0xc0000);
static const System::Byte SP_PROT_SSL3TLS1_CLIENTS = System::Byte(0xa0);
static const System::Int8 SP_PROT_SSL3TLS1_SERVERS = System::Int8(0x50);
static const System::Byte SP_PROT_SSL3TLS1 = System::Byte(0xf0);
static const int SP_PROT_UNI_SERVER = int(0x40000000);
static const unsigned SP_PROT_UNI_CLIENT = unsigned(0x80000000);
static const unsigned SP_PROT_UNI = unsigned(0xc0000000);
static const unsigned SP_PROT_ALL = unsigned(0xffffffff);
static const System::Int8 SP_PROT_NONE = System::Int8(0x0);
static const unsigned SP_PROT_CLIENTS = unsigned(0x800000aa);
static const int SP_PROT_SERVERS = int(0x40000055);
static const int SEC_I_CONTINUE_NEEDED = int(0x90312);
static const int SEC_I_CONTEXT_EXPIRED = int(0x90317);
static const int SEC_I_INCOMPLETE_CREDENTIALS = int(0x90320);
static const int SEC_I_RENEGOTIATE = int(0x90321);
static const System::Int8 SEC_E_OK = System::Int8(0x0);
static const unsigned SEC_E_INSUFFICIENT_MEMORY = unsigned(0x80090300);
static const unsigned SEC_E_INVALID_HANDLE = unsigned(0x80090301);
static const unsigned SEC_E_UNSUPPORTED_FUNCTION = unsigned(0x80090302);
static const unsigned SEC_E_INTERNAL_ERROR = unsigned(0x80090304);
static const unsigned SEC_E_INVALID_TOKEN = unsigned(0x80090308);
static const unsigned SEC_E_QOP_NOT_SUPPORTED = unsigned(0x8009030a);
static const unsigned SEC_E_UNKNOWN_CREDENTIALS = unsigned(0x8009030d);
static const unsigned SEC_E_NO_CREDENTIALS = unsigned(0x8009030e);
static const unsigned SEC_E_MESSAGE_ALTERED = unsigned(0x8009030f);
static const unsigned SEC_E_OUT_OF_SEQUENCE = unsigned(0x80090310);
static const unsigned SEC_E_INCOMPLETE_MESSAGE = unsigned(0x80090318);
static const unsigned SEC_E_BUFFER_TOO_SMALL = unsigned(0x80090321);
static const unsigned SEC_E_WRONG_PRINCIPAL = unsigned(0x80090322);
static const unsigned SEC_E_UNTRUSTED_ROOT = unsigned(0x80090325);
static const unsigned SEC_E_ILLEGAL_MESSAGE = unsigned(0x80090326);
static const unsigned SEC_E_ENCRYPT_FAILURE = unsigned(0x80090329);
static const unsigned SEC_E_DECRYPT_FAILURE = unsigned(0x80090330);
static const unsigned SEC_E_CRYPTO_SYSTEM_INVALID = unsigned(0x80090337);
static const unsigned CRYPT_E_NOT_FOUND = unsigned(0x80092004);
static const unsigned CERT_E_UNTRUSTEDROOT = unsigned(0x800b0109);
static const unsigned CERT_E_CN_NO_MATCH = unsigned(0x800b010f);
static const System::Int8 SECPKG_ATTR_REMOTE_CERT_CONTEXT = System::Int8(0x53);
static const System::Int8 SECPKG_ATTR_LOCAL_CERT_CONTEXT = System::Int8(0x54);
static const System::Int8 SECPKG_ATTR_ROOT_STORE = System::Int8(0x55);
static const System::Int8 SECPKG_ATTR_SUPPORTED_ALGS = System::Int8(0x56);
static const System::Int8 SECPKG_ATTR_CIPHER_STRENGTHS = System::Int8(0x57);
static const System::Int8 SECPKG_ATTR_SUPPORTED_PROTOCOLS = System::Int8(0x58);
static const System::Int8 SECPKG_ATTR_ISSUER_LIST_EX = System::Int8(0x59);
static const System::Int8 SECPKG_ATTR_CONNECTION_INFO = System::Int8(0x5a);
#define szOID_PKIX_KP_SERVER_AUTH L"1.3.6.1.5.5.7.3.1"
#define szOID_SERVER_GATED_CRYPTO L"1.3.6.1.4.1.311.10.3.3"
#define szOID_SGC_NETSCAPE L"2.16.840.1.113730.4.1"
#define szOID_PKIX_KP_CLIENT_AUTH L"1.3.6.1.5.5.7.3.2"
static const System::Int8 SCHANNEL_SHUTDOWN = System::Int8(0x1);
static const System::Int8 SCHANNEL_CRED_VERSION = System::Int8(0x4);
static const System::Int8 SCH_CRED_NO_SYSTEM_MAPPER = System::Int8(0x2);
static const System::Int8 SCH_CRED_NO_SERVERNAME_CHECK = System::Int8(0x4);
static const System::Int8 SCH_CRED_MANUAL_CRED_VALIDATION = System::Int8(0x8);
static const System::Int8 SCH_CRED_NO_DEFAULT_CREDS = System::Int8(0x10);
static const System::Int8 SCH_CRED_AUTO_CRED_VALIDATION = System::Int8(0x20);
static const System::Int8 SCH_CRED_USE_DEFAULT_CREDS = System::Int8(0x40);
static const int SCH_SEND_AUX_RECORD = int(0x200000);
static const int SCH_SEND_ROOT_CERT = int(0x40000);
static const int SCH_USE_STRONG_CRYPTO = int(0x400000);
static const int SCH_USE_PRESHAREDKEY_ONLY = int(0x800000);
static const System::Word SCH_CRED_REVOCATION_CHECK_END_CERT = System::Word(0x100);
static const System::Word SCH_CRED_REVOCATION_CHECK_CHAIN = System::Word(0x200);
static const System::Word SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = System::Word(0x400);
static const System::Word SCH_CRED_IGNORE_NO_REVOCATION_CHECK = System::Word(0x800);
static const System::Word SCH_CRED_IGNORE_REVOCATION_OFFLINE = System::Word(0x1000);
static const System::Int8 AUTHTYPE_CLIENT = System::Int8(0x1);
static const System::Int8 AUTHTYPE_SERVER = System::Int8(0x2);
static const System::Int8 USAGE_MATCH_TYPE_AND = System::Int8(0x0);
static const System::Int8 USAGE_MATCH_TYPE_OR = System::Int8(0x1);
#define UNISP_NAME L"Microsoft Unified Security Protocol Provider"
#define MS_ENH_RSA_AES_PROV L"Microsoft Enhanced RSA and AES Cryptographic Provider"
static const System::Int8 PROV_RSA_AES = System::Int8(0x18);
#define CERT_CHAIN_POLICY_BASE (char *)(0x1)
#define CERT_CHAIN_POLICY_AUTHENTICODE (char *)(0x2)
#define CERT_CHAIN_POLICY_AUTHENTICODE_TS (char *)(0x3)
#define CERT_CHAIN_POLICY_SSL (char *)(0x4)
#define CERT_CHAIN_POLICY_BASIC_CONSTRAINTS (char *)(0x5)
#define CERT_CHAIN_POLICY_NT_AUTH (char *)(0x6)
#define CERT_CHAIN_POLICY_MICROSOFT_ROOT (char *)(0x7)
static const System::Int8 CERT_INFO_VERSION_FLAG = System::Int8(0x1);
static const System::Int8 CERT_INFO_SERIAL_NUMBER_FLAG = System::Int8(0x2);
static const System::Int8 CERT_INFO_SIGNATURE_ALGORITHM_FLAG = System::Int8(0x3);
static const System::Int8 CERT_INFO_ISSUER_FLAG = System::Int8(0x4);
static const System::Int8 CERT_INFO_NOT_BEFORE_FLAG = System::Int8(0x5);
static const System::Int8 CERT_INFO_NOT_AFTER_FLAG = System::Int8(0x6);
static const System::Int8 CERT_INFO_SUBJECT_FLAG = System::Int8(0x7);
static const System::Int8 CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = System::Int8(0x8);
static const System::Int8 CERT_INFO_ISSUER_UNIQUE_ID_FLAG = System::Int8(0x9);
static const System::Int8 CERT_INFO_SUBJECT_UNIQUE_ID_FLAG = System::Int8(0xa);
static const System::Int8 CERT_INFO_EXTENSION_FLAG = System::Int8(0xb);
static const System::Int8 CERT_COMPARE_SHIFT = System::Int8(0x10);
static const System::Int8 CERT_COMPARE_ANY = System::Int8(0x0);
static const System::Int8 CERT_COMPARE_SHA1_HASH = System::Int8(0x1);
static const System::Int8 CERT_COMPARE_NAME = System::Int8(0x2);
static const System::Int8 CERT_COMPARE_ATTR = System::Int8(0x3);
static const System::Int8 CERT_COMPARE_MD5_HASH = System::Int8(0x4);
static const System::Int8 CERT_COMPARE_PROPERTY = System::Int8(0x5);
static const System::Int8 CERT_COMPARE_PUBLIC_KEY = System::Int8(0x6);
static const System::Int8 CERT_COMPARE_HASH = System::Int8(0x1);
static const System::Int8 CERT_COMPARE_NAME_STR_A = System::Int8(0x7);
static const System::Int8 CERT_COMPARE_NAME_STR_W = System::Int8(0x8);
static const System::Int8 CERT_COMPARE_KEY_SPEC = System::Int8(0x9);
static const System::Int8 CERT_COMPARE_ENHKEY_USAGE = System::Int8(0xa);
static const System::Int8 CERT_COMPARE_CTL_USAGE = System::Int8(0xa);
static const System::Int8 CERT_FIND_ANY = System::Int8(0x0);
static const int CERT_FIND_SHA1_HASH = int(0x10000);
static const int CERT_FIND_MD5_HASH = int(0x40000);
static const int CERT_FIND_HASH = int(0x10000);
static const int CERT_FIND_PROPERTY = int(0x50000);
static const int CERT_FIND_PUBLIC_KEY = int(0x60000);
static const int CERT_FIND_EXISTING = int(0xd0000);
static const int CERT_FIND_SUBJECT_NAME = int(0x20007);
static const int CERT_FIND_SUBJECT_ATTR = int(0x30007);
static const int CERT_FIND_ISSUER_NAME = int(0x20004);
static const int CERT_FIND_ISSUER_ATTR = int(0x30004);
static const int CERT_FIND_SUBJECT_STR_A = int(0x70007);
static const int CERT_FIND_SUBJECT_STR_W = int(0x80007);
static const int CERT_FIND_SUBJECT_STR = int(0x80007);
static const int CERT_FIND_ISSUER_STR_A = int(0x70004);
static const int CERT_FIND_ISSUER_STR_W = int(0x80004);
static const int CERT_FIND_ISSUER_STR = int(0x80004);
static const int CERT_FIND_KEY_SPEC = int(0x90000);
static const int CERT_FIND_ENHKEY_USAGE = int(0xa0000);
static const int CERT_FIND_CTL_USAGE = int(0xa0000);
static const System::Int8 CRYPT_ASN_ENCODING = System::Int8(0x1);
static const System::Int8 CRYPT_NDR_ENCODING = System::Int8(0x2);
static const System::Int8 X509_ASN_ENCODING = System::Int8(0x1);
static const System::Int8 X509_NDR_ENCODING = System::Int8(0x2);
static const int PKCS_7_ASN_ENCODING = int(0x10000);
static const int PKCS_7_NDR_ENCODING = int(0x20000);
static const System::Int8 CERT_CHAIN_FIND_BY_ISSUER = System::Int8(0x1);
#define CERT_STORE_PROV_PKCS7 (char *)(0x5)
#define CERT_STORE_PROV_SERIALIZED (char *)(0x6)
#define CERT_STORE_PROV_FILENAME_A (char *)(0x7)
#define CERT_STORE_PROV_FILENAME_W (char *)(0x8)
#define CERT_STORE_PROV_FILENAME (char *)(0x8)
#define CERT_STORE_PROV_SYSTEM_A (char *)(0x9)
#define CERT_STORE_PROV_SYSTEM_W (char *)(0xa)
#define CERT_STORE_PROV_SYSTEM (char *)(0xa)
static const System::Word CERT_STORE_OPEN_EXISTING_FLAG = System::Word(0x4000);
static const int CERT_SYSTEM_STORE_LOCATION_MASK = int(0x30000);
static const int CERT_SYSTEM_STORE_CURRENT_USER = int(0x10000);
static const int CERT_SYSTEM_STORE_LOCAL_MACHINE = int(0x20000);
static const int CERT_SYSTEM_STORE_CURRENT_SERVICE = int(0x40000);
static const int CERT_SYSTEM_STORE_SERVICES = int(0x50000);
static const int CERT_SYSTEM_STORE_USERS = int(0x60000);
static const int CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY = int(0x70000);
static const int CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY = int(0x80000);
static const int CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE = int(0x90000);
static const int CERT_SYSTEM_STORE_UNPROTECTED_FLAG = int(0x40000000);
static const unsigned CERT_SYSTEM_STORE_RELOCATE_FLAG = unsigned(0x80000000);
static const System::Int8 CRYPT_STRING_HEX = System::Int8(0x4);
static const System::Int8 CERT_THUMBPRINT_DATA_LEN = System::Int8(0x14);
extern DELPHI_PACKAGE PSecurityFunctionTable __stdcall InitSecurityInterface(void);
extern DELPHI_PACKAGE unsigned __stdcall CertNameToStr(unsigned dwCertEncodingType, const CRYPTOAPI_BLOB &pName, unsigned dwStrType, System::WideChar * psz, unsigned csz);
extern DELPHI_PACKAGE PCCERT_CONTEXT __stdcall CertGetIssuerCertificateFromStore(HCERTSTORE HCERTSTORE, PCCERT_CONTEXT pSubjectContext, PCCERT_CONTEXT pPrevIssuerContext, unsigned* pdwFlags);
extern DELPHI_PACKAGE System::LongBool __stdcall CertFreeCertificateContext(PCCERT_CONTEXT pCertContext);
extern DELPHI_PACKAGE System::LongBool __stdcall CertGetCertificateChain(HCERTCHAINENGINE hChainEngine, PCCERT_CONTEXT pCertContext, Winapi::Windows::PFileTime pTime, HCERTSTORE hAdditionalStore, const CERT_CHAIN_PARA &pChainPara, unsigned dwFlags, void * pvReserved, PCCERT_CHAIN_CONTEXT &ppChainContext);
extern DELPHI_PACKAGE System::LongBool __stdcall CertVerifyCertificateChainPolicy(char * pszPolicyOID, PCCERT_CHAIN_CONTEXT pChainContext, const CERT_CHAIN_POLICY_PARA &pPolicyPara, const CERT_CHAIN_POLICY_STATUS &pPolicyStatus);
extern DELPHI_PACKAGE System::LongBool __stdcall CertFreeCertificateChainEngine(HCERTCHAINENGINE hChainEngine);
extern DELPHI_PACKAGE System::LongBool __stdcall CertFreeCertificateChain(PCCERT_CHAIN_CONTEXT pChainContext);
extern DELPHI_PACKAGE HCERTSTORE __stdcall CertOpenStore(char * lpszStoreProvider, unsigned dwEncodingType, HCRYPTPROV hProv, unsigned dwFlags, const void * pvPara);
extern DELPHI_PACKAGE HCERTSTORE __stdcall CertOpenSystemStore(HCRYPTPROV hProv, System::WideChar * szSubsystemProtocol);
extern DELPHI_PACKAGE PCCERT_CONTEXT __stdcall CertFindCertificateInStore(HCERTSTORE HCERTSTORE, unsigned dwCertEncodingType, unsigned dwFindFlags, unsigned dwFindType, const void * pvFindPara, PCCERT_CONTEXT pPrevCertContext);
extern DELPHI_PACKAGE PCCERT_CHAIN_CONTEXT __stdcall CertFindChainInStore(HCERTSTORE HCERTSTORE, unsigned dwCertEncodingType, unsigned dwFindFlags, unsigned dwFindType, const void * pvFindPara, PCCERT_CHAIN_CONTEXT pPrevChainContext);
extern DELPHI_PACKAGE System::LongBool __stdcall CertCloseStore(HCERTSTORE HCERTSTORE, unsigned dwFlags);
extern DELPHI_PACKAGE System::LongBool __stdcall CryptProtectData(CRYPTOAPI_BLOB &pDataIn, System::WideChar * ppszDataDecr, PDATA_BLOB pOptionalEntropy, int pvReserved, PCRYPTPROTECT_PROMPTSTRUCT pPromptStruct, unsigned dwFlags, CRYPTOAPI_BLOB &pDataOut);
extern DELPHI_PACKAGE System::LongBool __stdcall CryptUnprotectData(CRYPTOAPI_BLOB &pDataIn, System::PPWideChar ppszDataDecr, PDATA_BLOB pOptionalEntropy, int pvReserved, PCRYPTPROTECT_PROMPTSTRUCT pPromptStruct, unsigned dwFlags, CRYPTOAPI_BLOB &pDataOut);
extern DELPHI_PACKAGE HCERTSTORE __stdcall PFXImportCertStore(CRYPTOAPI_BLOB &pPFX, System::WideChar * szPassword, unsigned dwFlags);
extern DELPHI_PACKAGE System::LongBool __stdcall CryptStringToBinary(System::WideChar * pszString, unsigned cchString, unsigned dwFlags, System::PByte pbBinary, unsigned &pcbBinary, unsigned* pdwSkip, unsigned* pdwFlags);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CertName(PCCERT_CONTEXT Cert, CRYPTOAPI_BLOB &Blob);
}	/* namespace Sgcssl_winsspi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCSSL_WINSSPI)
using namespace Sgcssl_winsspi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcssl_winsspiHPP
