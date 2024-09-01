{ ***************************************************************************
  sgcJWT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_RSA;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}
{$IFDEF SGC_INDY}
{$I IdCompilerDefines.inc}
{$ENDIF}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders{$ELSE}IdSSLOpenSSLHeaders{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // sgc
  sgcHTTP_JWT_Classes, sgcHTTP_JWT_Types, sgcBase_Helpers;

type
  PBytes = ^TBytes;

  TsgcHTTP_JWT_RSA = class(TsgcHTTP_JWT_Base)
  protected
    function LoadPublicKey(const aValue: string):
    {$IFDEF INDY10_6_0_5169}PRSA{$ELSE}Pointer{$ENDIF};
    procedure DoLoadOpenSSLMethods; virtual;
  public
    function SignRSA256(aValue, aKey: TBytes): TBytes;
    function SignRSA384(aValue, aKey: TBytes): TBytes;
    function SignRSA512(aValue, aKey: TBytes): TBytes;
  protected
    function DoSignRSA(aValue, aKey: TBytes; aAlgorithm: TsgcHTTP_JWT_Algorithm)
      : TBytes; virtual;
    function DoValidateRSA(const aHeader, aPayload, aSignature,
      aPublicKey: String; aAlgorithm: TsgcHTTP_JWT_Algorithm): Boolean;
  public
    function ValidateRSA256(const aHeader, aPayload, aSignature,
      aPublicKey: String): Boolean;
    function ValidateRSA384(const aHeader, aPayload, aSignature,
      aPublicKey: String): Boolean;
    function ValidateRSA512(const aHeader, aPayload, aSignature,
      aPublicKey: String): Boolean;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_JWT}

uses
  sgcHTTP_Const, sgcHTTP_JWT_Helpers;

{$IFDEF INDY10_6_0_5169}
{$IFDEF IOS}
{$IFNDEF SGC_OPENSSL_API_1_1}
{$IFNDEF INDY10_6_2_D10_4}
{$IFDEF SGC_JWT_IOS}
function EVP_MD_CTX_create: PEVP_MD_CTX cdecl;
  external 'libssl.a' name 'EVP_MD_CTX_create';
procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX)cdecl;
  external 'libssl.a' name 'EVP_MD_CTX_destroy';
{$ELSE}

function EVP_MD_CTX_create: PEVP_MD_CTX;
begin
  result := nil;
end;

procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX);
begin
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF SGC_JWT_IOS}
function RSA_verify(_type: Integer; m: PBytes; m_len: Cardinal; sigbuf: PBytes;
  siglen: Cardinal; rsa: PRSA): Integer; cdecl;
  external 'libssl.a' name 'RSA_verify';
function SHA256(const d: PBytes; n: NativeUInt; md: PBytes): PBytes cdecl;
  external 'libssl.a' name 'SHA256';
function SHA384(const d: PBytes; n: NativeUInt; md: PBytes): PBytes cdecl;
  external 'libssl.a' name 'SHA384';
function SHA512(const d: PBytes; n: NativeUInt; md: PBytes): PBytes cdecl;
  external 'libssl.a' name 'SHA512';
function PEM_read_bio_RSA_PUBKEY(bp: PBIO; x: PPRSA; cb: ppem_password_cb;
  u: Pointer): PRSA cdecl; external 'libssl.a' name 'PEM_read_bio_RSA_PUBKEY';
{$ELSE}

function RSA_verify(_type: Integer; m: PBytes; m_len: Cardinal; sigbuf: PBytes;
  siglen: Cardinal; rsa: PRSA): Integer;
begin
  result := 0;
end;

function SHA256(const d: PBytes; n: NativeUInt; md: PBytes): PBytes;
begin
  result := nil;
end;

function SHA384(const d: PBytes; n: NativeUInt; md: PBytes): PBytes;
begin
  result := nil;
end;

function SHA512(const d: PBytes; n: NativeUInt; md: PBytes): PBytes;
begin
  result := nil;
end;

function PEM_read_bio_RSA_PUBKEY(bp: PBIO; x: PPRSA; cb: ppem_password_cb;
  u: Pointer): PRSA;
begin
  result := nil;
end;
{$ENDIF}
{$ELSE}

var
{$IFNDEF SGC_OPENSSL_API_1_1}
{$IFNDEF INDY10_6_2_D10_4}
  EVP_MD_CTX_create: function: PEVP_MD_CTX cdecl;
  EVP_MD_CTX_destroy: procedure(ctx: PEVP_MD_CTX); cdecl;
{$ENDIF}
{$ENDIF}
  RSA_verify: function(_type: Integer; m: PBytes; m_len: Cardinal;
    sigbuf: PBytes; siglen: Cardinal; rsa: PRSA): Integer; cdecl;
  SHA256: function(const d: PBytes; n: NativeUInt; md: PBytes): PBytes cdecl;
  SHA384: function(const d: PBytes; n: NativeUInt; md: PBytes): PBytes cdecl;
  SHA512: function(const d: PBytes; n: NativeUInt; md: PBytes): PBytes cdecl;
  PEM_read_bio_RSA_PUBKEY: function(bp: PBIO; x: PPRSA; cb: ppem_password_cb;
    u: Pointer): PRSA cdecl;
{$ENDIF}
{$ENDIF}

procedure TsgcHTTP_JWT_RSA.DoLoadOpenSSLMethods;
begin
{$IFDEF INDY10_6_0_5169}
{$IFNDEF IOS}
{$IFNDEF INDY10_6_2_D10_4}
{$IFNDEF SGC_OPENSSL_API_1_1}
  EVP_MD_CTX_create :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'EVP_MD_CTX_create');
  if @EVP_MD_CTX_create = nil then
    raise Exception.Create('Method EVP_MD_CTX_create cannot be loaded.');

  EVP_MD_CTX_destroy :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'EVP_MD_CTX_destroy');
  if @EVP_MD_CTX_destroy = nil then
    raise Exception.Create('Method EVP_MD_CTX_destroy cannot be loaded.');
{$ENDIF}
{$ENDIF}
  RSA_verify :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle, 'RSA_verify');
  if @RSA_verify = nil then
    raise Exception.Create('Method RSA_verify cannot be loaded.');

  SHA256 :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle, 'SHA256');
  if @SHA256 = nil then
    raise Exception.Create('Method SHA256 cannot be loaded.');

  SHA384 :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle, 'SHA384');
  if @SHA384 = nil then
    raise Exception.Create('Method SHA384 cannot be loaded.');

  SHA512 :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle, 'SHA512');
  if @SHA512 = nil then
    raise Exception.Create('Method SHA512 cannot be loaded.');

  PEM_read_bio_RSA_PUBKEY :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'PEM_read_bio_RSA_PUBKEY');
  if @PEM_read_bio_RSA_PUBKEY = nil then
    raise Exception.Create('Method PEM_read_bio_RSA_PUBKEY cannot be loaded.');

{$ENDIF}
{$ENDIF}
end;

function TsgcHTTP_JWT_RSA.DoSignRSA(aValue, aKey: TBytes;
  aAlgorithm: TsgcHTTP_JWT_Algorithm): TBytes;
{$IFDEF INDY10_6_0_5169}
var
  oPrivKeyBIO: PBIO;
  oPrivKey: pEVP_PKEY;
  oRSA: PRSA;
  oCTX: PEVP_MD_CTX;
  oSHA: PEVP_MD;
  vLength: Integer;
  oSig: Pointer;
{$ENDIF}
begin
{$IFDEF INDY10_6_0_5169}
  if not LoadOpenSSLLibrary then
    raise EIdOSSLCouldNotLoadSSLLibrary.Create(CS_HTTP_COULD_NOT_LOAD_OPENSSL);
  DoLoadOpenSSLMethods;

  // ... read private key
  oPrivKeyBIO := BIO_new(BIO_s_mem);
  try
    BIO_write(oPrivKeyBIO, @aKey[0], Length(aKey));
    oRSA := PEM_read_bio_RSAPrivateKey(oPrivKeyBIO, nil, nil, nil);
    if oRSA = nil then
      raise Exception.Create('Unable to load private key');
  finally
    BIO_free(oPrivKeyBIO);
  end;

  try
    oPrivKey := EVP_PKEY_new();
    if EVP_PKEY_set1_RSA(oPrivKey, oRSA) <> 1 then
      raise Exception.Create('Unable to extract private key');
    try
      case aAlgorithm of
        jwtRS256:
          oSHA := EVP_sha256();
        jwtRS384:
          oSHA := EVP_sha384();
        jwtRS512:
          oSHA := EVP_sha512();
      else
        raise Exception.Create('Unsupported signing algorithm!');
      end;

{$IFDEF SGC_INDY_LIB}
{$IFDEF SGC_OPENSSL_API_1_1}
      oCTX := EVP_MD_CTX_new;
{$ELSE}
      case OPENSSL_API_VERSION of
        opSSL_1_0:
          oCTX := EVP_MD_CTX_create;
      else
        oCTX := EVP_MD_CTX_new;
      end;
{$ENDIF}
{$ELSE}
      oCTX := EVP_MD_CTX_create;
{$ENDIF}
      try
        if EVP_DigestSignInit(oCTX, nil, oSHA, nil, oPrivKey) <> 1 then
          raise Exception.Create('Unable to init context');
        if EVP_DigestSignUpdate(oCTX, @aValue[0], Length(aValue)) <> 1 then
          raise Exception.Create('Unable to update context with payload');

        // Get signature, first read signature len
        EVP_DigestSignFinal(oCTX, nil, @vLength);
        oSig := OPENSSL_malloc(vLength);
        try
          EVP_DigestSignFinal(oCTX, oSig, @vLength);
          SetLength(result, vLength);
          sgcMove(oSig^, result[0], vLength);
        finally
          CRYPTO_free(oSig);
        end;
      finally
{$IFDEF SGC_INDY_LIB}
{$IFDEF SGC_OPENSSL_API_1_1}
        EVP_MD_CTX_free(oCTX);
{$ELSE}
        case OPENSSL_API_VERSION of
          opSSL_1_0:
            EVP_MD_CTX_destroy(oCTX);
        else
          EVP_MD_CTX_free(oCTX);
        end;
{$ENDIF}
{$ELSE}
        EVP_MD_CTX_destroy(oCTX);
{$ENDIF}
      end;
    finally
      EVP_PKEY_free(oPrivKey);
    end;
  finally
    RSA_Free(oRSA);
  end;
{$ELSE}
  raise Exception.Create('DoSignRSA not supported. Indy version is too old.');
{$ENDIF}
end;

function TsgcHTTP_JWT_RSA.DoValidateRSA(const aHeader, aPayload, aSignature,
  aPublicKey: String; aAlgorithm: TsgcHTTP_JWT_Algorithm): Boolean;
{$IFDEF INDY10_6_0_5169}
var
  oPublicKey: PRSA;
  vType, vLength: Integer;
  oHash, oJWT, oSignature: TBytes;
{$ENDIF}
begin
{$IFDEF INDY10_6_0_5169}
  if not LoadOpenSSLLibrary then
    raise EIdOSSLCouldNotLoadSSLLibrary.Create(CS_HTTP_COULD_NOT_LOAD_OPENSSL);
  DoLoadOpenSSLMethods;

  oPublicKey := LoadPublicKey(aPublicKey);

  oJWT := sgcGetBytesFromUTF8String(aHeader + '.' + aPayload);
  oSignature := sgcDecodeBase64(sgcURLDecode(aSignature));
  case aAlgorithm of
    jwtRS256:
      begin
        vType := 672;
        vLength := SHA256_DIGEST_LENGTH;
        SetLength(oHash, vLength);
        SHA256(@oJWT[0], Length(oJWT), @oHash[0]);
      end;
    jwtRS384:
      begin
        vType := 673;
        vLength := SHA384_DIGEST_LENGTH;
        SetLength(oHash, vLength);
        SHA384(@oJWT[0], Length(oJWT), @oHash[0]);
      end;
    jwtRS512:
      begin
        vType := 674;
        vLength := SHA512_DIGEST_LENGTH;
        SetLength(oHash, vLength);
        SHA512(@oJWT[0], Length(oJWT), @oHash[0]);
      end
  else
    raise Exception.Create('[RSA] Unsupported signing algorithm!');
  end;

  result := RSA_verify(vType, @oHash[0], vLength, @oSignature[0],
    Length(oSignature), oPublicKey) = 1;
{$ELSE}
  result := False;
{$ENDIF}
end;

function TsgcHTTP_JWT_RSA.LoadPublicKey(const aValue: string):
{$IFDEF INDY10_6_0_5169}PRSA{$ELSE}Pointer{$ENDIF};
{$IFDEF INDY10_6_0_5169}
var
  oBio: PBIO;
  oBytes: TBytes;
{$ENDIF}
begin
{$IFDEF INDY10_6_0_5169}
  oBytes := sgcGetBytesFromUTF8String(aValue);

  oBio := BIO_new(BIO_s_mem);
  try
    BIO_write(oBio, @oBytes[0], Length(oBytes));
    if LeftStr(aValue, Length('-----BEGIN RSA PUBLIC KEY-----')) = '-----BEGIN RSA PUBLIC KEY-----'
    then
      result := PEM_read_bio_RSAPublicKey(oBio, nil, nil, nil)
    else
      result := PEM_read_bio_RSA_PUBKEY(oBio, nil, nil, nil);
    if result = nil then
      raise Exception.Create('Cannot Load Public Key.');
  finally
    BIO_free(oBio);
  end;
{$ELSE}
  raise Exception.Create
    ('LoadPublicKey not supported. Indy version is too old.');
{$ENDIF}
end;

function TsgcHTTP_JWT_RSA.SignRSA256(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignRSA(aValue, aKey, jwtRS256);
end;

function TsgcHTTP_JWT_RSA.SignRSA384(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignRSA(aValue, aKey, jwtRS384);
end;

function TsgcHTTP_JWT_RSA.SignRSA512(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignRSA(aValue, aKey, jwtRS512);
end;

function TsgcHTTP_JWT_RSA.ValidateRSA256(const aHeader, aPayload, aSignature,
  aPublicKey: String): Boolean;
begin
  result := DoValidateRSA(aHeader, aPayload, aSignature, aPublicKey, jwtRS256);
end;

function TsgcHTTP_JWT_RSA.ValidateRSA384(const aHeader, aPayload, aSignature,
  aPublicKey: String): Boolean;
begin
  result := DoValidateRSA(aHeader, aPayload, aSignature, aPublicKey, jwtRS384);
end;

function TsgcHTTP_JWT_RSA.ValidateRSA512(const aHeader, aPayload, aSignature,
  aPublicKey: String): Boolean;
begin
  result := DoValidateRSA(aHeader, aPayload, aSignature, aPublicKey, jwtRS512);
end;

{$ENDIF}

end.
