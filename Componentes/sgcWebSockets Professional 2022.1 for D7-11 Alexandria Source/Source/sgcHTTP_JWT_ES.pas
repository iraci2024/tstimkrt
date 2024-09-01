{ ***************************************************************************
  sgcSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_ES;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}
{$UNDEF SGC_WIN64_INT64}
{$IFDEF DXE2}
{$IFNDEF DXE4}
{$IFDEF WIN64}
{$DEFINE SGC_WIN64_INT64}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF LAZARUS}
{$IFDEF WIN64}
{$DEFINE SGC_WIN64_INT64}
{$ENDIF}
{$ENDIF}
{$IFDEF SGC_INDY}
{$I IdCompilerDefines.inc}
{$ENDIF}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders{$ELSE}IdSSLOpenSSLHeaders{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // sgc
  sgcHTTP_JWT_Classes, sgcHTTP_JWT_Types, sgcBase_Helpers;

{$IFNDEF INDY10_6_0_5169}

type
  PECDSA_SIG = Pointer;
{$ENDIF}

type
  TsgcHTTP_JWT_ES = class(TsgcHTTP_JWT_Base)
{$IFDEF INDY10_6_0_5169}
  private
    function DoConvertRS(aSignature: Pointer;
      aAlgorithm: TsgcHTTP_JWT_Algorithm): TBytes;
    function DoSignatureRS(const aSignature: TBytes): Pointer;
    function LoadPublicKey(const aValue: String): Pointer;
{$ENDIF}
  public
    function SignES256(aValue, aKey: TBytes): TBytes;
    function SignES384(aValue, aKey: TBytes): TBytes;
    function SignES512(aValue, aKey: TBytes): TBytes;
  protected
    procedure DoLoadOpenSSLMethods; virtual;
  protected
    function DoSignES(aValue, aKey: TBytes; aAlgorithm: TsgcHTTP_JWT_Algorithm)
      : TBytes; virtual;
    function DoValidateES(const aHeader, aPayload, aSignature,
      aPublicKey: String; aAlgorithm: TsgcHTTP_JWT_Algorithm): Boolean; virtual;
  public
    function ValidateES256(const aHeader, aPayload, aSignature,
      aPublicKey: String): Boolean;
    function ValidateES384(const aHeader, aPayload, aSignature,
      aPublicKey: String): Boolean;
    function ValidateES512(const aHeader, aPayload, aSignature,
      aPublicKey: String): Boolean;
  end;
{$ENDIF}

implementation

uses
  sgcHTTP_Const, sgcHTTP_JWT_Helpers;

{$IFDEF SGC_JWT}
{$IFDEF INDY10_6_0_5169}
{$IFDEF IOS}
{$IFDEF IOS_STATIC}
function ECDSA_do_sign(const dgst: Pointer; dgst_len: integer; eckey: PEC_KEY)
  : PECDSA_SIG; cdecl; external 'libssl.a' name 'ECDSA_do_sign';
procedure ECDSA_SIG_free(sig: PECDSA_SIG); cdecl;
  external 'libssl.a' name 'ECDSA_SIG_free';
function BN_num_bits(const a: PBIGNUM): integer; cdecl;
  external 'libssl.a' name 'BN_num_bits';
function BN_bn2bin(const a: PBIGNUM; _to: Pointer): integer cdecl;
  external 'libssl.a' name 'BN_bn2bin';
function BN_bin2bn(const s: Pointer; len: integer; ret: PBIGNUM): PBIGNUM cdecl;
  external 'libssl.a' name 'BN_bin2bn';
function ECDSA_do_verify(const dgst: Pointer; dgst_len: integer;
  sig: PECDSA_SIG; eckey: PEC_KEY): integer cdecl;
  external 'libssl.a' name 'ECDSA_do_verify';
procedure EC_KEY_free(pKey: PEC_KEY)cdecl;
  external 'libssl.a' name 'EC_KEY_free';
function ECDSA_SIG_new: PECDSA_SIG cdecl;
  external 'libssl.a' name 'ECDSA_SIG_new';
function PEM_read_bio_PUBKEY(bp: pBIO; x: PPEVP_PKEY; cb: ppem_password_cb;
  u: Pointer): PEVP_PKEY; cdecl; external 'libssl.a' name 'PEM_read_bio_PUBKEY';
{$ELSE}

function ECDSA_do_sign(const dgst: Pointer; dgst_len: integer; eckey: PEC_KEY)
  : PECDSA_SIG;
begin
  result := nil;
end;

procedure ECDSA_SIG_free(sig: PECDSA_SIG);
begin
end;

function BN_num_bits(const a: PBIGNUM): integer;
begin
  result := 0;
end;

function BN_bn2bin(const a: PBIGNUM; _to: Pointer): integer;
begin
  result := 0;
end;

function BN_bin2bn(const s: Pointer; len: integer; ret: PBIGNUM): PBIGNUM;
begin
  result := nil;
end;

function ECDSA_do_verify(const dgst: Pointer; dgst_len: integer;
  sig: PECDSA_SIG; eckey: PEC_KEY): integer;
begin
  result := 0;
end;

procedure EC_KEY_free(pKey: PEC_KEY);
begin
end;

function ECDSA_SIG_new: PECDSA_SIG;
begin
  result := nil;
end;

function PEM_read_bio_PUBKEY(bp: pBIO; x: PPEVP_PKEY; cb: ppem_password_cb;
  u: Pointer): PEVP_PKEY;
begin
  result := nil;
end;
{$ENDIF}
{$ELSE}

var
  ECDSA_do_sign: function(const dgst: Pointer; dgst_len: integer;
    eckey: PEC_KEY): PECDSA_SIG cdecl;
  ECDSA_SIG_free: procedure(sig: PECDSA_SIG)cdecl;
  BN_num_bits: function(const a: PBIGNUM): integer cdecl;
  BN_bn2bin: function(const a: PBIGNUM; _to: Pointer): integer cdecl;
  BN_bin2bn: function(const s: Pointer; len: integer; ret: PBIGNUM)
    : PBIGNUM cdecl;
  ECDSA_do_verify: function(const dgst: Pointer; dgst_len: integer;
    sig: PECDSA_SIG; eckey: PEC_KEY): integer cdecl;
  EC_KEY_free: procedure(pKey: PEC_KEY)cdecl;
  ECDSA_SIG_new: function(): PECDSA_SIG cdecl;
  PEM_read_bio_PUBKEY: function(bp: pBIO; x: PPEVP_PKEY; cb: ppem_password_cb;
    u: Pointer): PEVP_PKEY; cdecl;
{$ENDIF}
{$ENDIF}
{$IFDEF INDY10_6_0_5169}

function TsgcHTTP_JWT_ES.DoConvertRS(aSignature: Pointer;
  aAlgorithm: TsgcHTTP_JWT_Algorithm): TBytes;
var
  vLength, vLengthR, vLengthS: integer;
begin
  vLength := 0;

  case aAlgorithm of
    jwtES256:
      vLength := 64;
    jwtES384:
      vLength := 96;
    jwtES512:
      vLength := 132;
  end;

  vLengthR := Trunc((BN_num_bits(PECDSA_SIG(aSignature).r) + 7) / 8);
  vLengthS := Trunc((BN_num_bits(PECDSA_SIG(aSignature).s) + 7) / 8);

  SetLength(result, vLength);
  FillChar(result[0], vLength, #0);

  BN_bn2bin(PECDSA_SIG(aSignature).r,
    Pointer({$IFDEF SGC_WIN64_INT64}Int64{$ELSE}integer{$ENDIF}(result) +
    (vLength div 2) - vLengthR));
  BN_bn2bin(PECDSA_SIG(aSignature).s,
    Pointer({$IFDEF SGC_WIN64_INT64}Int64{$ELSE}integer{$ENDIF}(result) +
    vLength - vLengthS));
end;

function TsgcHTTP_JWT_ES.DoSignatureRS(const aSignature: TBytes): Pointer;
var
  vLength: integer;
begin
  result := ECDSA_SIG_new();
  vLength := Length(aSignature) div 2;

  BN_bin2bn(Pointer(aSignature), vLength, PECDSA_SIG(result).r);
  BN_bin2bn(Pointer({$IFDEF SGC_WIN64_INT64}Int64{$ELSE}integer{$ENDIF}(aSignature) + vLength), vLength, PECDSA_SIG(result).s);
end;

function TsgcHTTP_JWT_ES.LoadPublicKey(const aValue: String): Pointer;
var
  oBytes: TBytes;
  oBIO: pBIO;
begin
  if not LoadOpenSSLLibrary then
    raise EIdOSSLCouldNotLoadSSLLibrary.Create(CS_HTTP_COULD_NOT_LOAD_OPENSSL);

{$IFNDEF IOS}
  PEM_read_bio_PUBKEY :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'PEM_read_bio_PUBKEY');
  if @PEM_read_bio_PUBKEY = nil then
    raise Exception.Create('Method PEM_read_bio_PUBKEY cannot be loaded.');
{$ENDIF}
  oBytes := sgcGetBytesFromUTF8String(aValue);
  oBIO := BIO_new(BIO_s_mem);
  try
    BIO_write(oBIO, @oBytes[0], Length(oBytes));

    result := PEM_read_bio_PUBKEY(oBIO, nil, nil, nil);
    if result = nil then
      raise Exception.Create('Cannot Load Public Key.');
  Finally
    BIO_free(oBIO);
  End;
end;
{$ENDIF}

procedure TsgcHTTP_JWT_ES.DoLoadOpenSSLMethods;
begin
{$IFDEF INDY10_6_0_5169}
{$IFNDEF IOS}
  ECDSA_do_sign :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'ECDSA_do_sign');
  if @ECDSA_do_sign = nil then
    raise Exception.Create('Method ECDSA_do_sign cannot be loaded.');

  ECDSA_SIG_free :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'ECDSA_SIG_free');
  if @ECDSA_SIG_free = nil then
    raise Exception.Create('Method ECDSA_SIG_free cannot be loaded.');

  BN_num_bits :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'BN_num_bits');
  if @BN_num_bits = nil then
    raise Exception.Create('Method BN_num_bits cannot be loaded.');

  BN_bn2bin :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle, 'BN_bn2bin');
  if @BN_bn2bin = nil then
    raise Exception.Create('Method BN_bn2bin cannot be loaded.');

  BN_bin2bn :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle, 'BN_bin2bn');
  if @BN_bin2bn = nil then
    raise Exception.Create('Method BN_bin2bn cannot be loaded.');

  ECDSA_do_verify :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'ECDSA_do_verify');
  if @ECDSA_do_verify = nil then
    raise Exception.Create('Method ECDSA_do_verify cannot be loaded.');

  EC_KEY_free :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'EC_KEY_free');
  if @EC_KEY_free = nil then
    raise Exception.Create('Method EC_KEY_free cannot be loaded.');

  ECDSA_SIG_new :=
{$IFDEF WINDOWS}Windows.{$ENDIF}GetProcAddress(GetCryptLibHandle,
    'ECDSA_SIG_new');
  if @ECDSA_SIG_new = nil then
    raise Exception.Create('Method ECDSA_SIG_new cannot be loaded.');
{$ENDIF}
{$ENDIF}
end;

function TsgcHTTP_JWT_ES.DoSignES(aValue, aKey: TBytes;
  aAlgorithm: TsgcHTTP_JWT_Algorithm): TBytes;
{$IFDEF INDY10_6_0_5169}
var
  oPrivKeyBIO: pBIO;
  oPrivKey: PEVP_PKEY;
  oEC_EKEY: PEC_KEY;
  oBytes: TBytes;
  oSig: PECDSA_SIG;
{$ENDIF}
begin
{$IFDEF INDY10_6_0_5169}
  oBytes := nil;

  if not LoadOpenSSLLibrary then
    raise EIdOSSLCouldNotLoadSSLLibrary.Create(CS_HTTP_COULD_NOT_LOAD_OPENSSL);
  DoLoadOpenSSLMethods;

  // ... read private key
  oPrivKeyBIO := BIO_new(BIO_s_mem);
  try
    BIO_write(oPrivKeyBIO, @aKey[0], Length(aKey));
    oPrivKey := PEM_read_bio_PrivateKey(oPrivKeyBIO, nil, nil, nil);
    if oPrivKey = nil then
      raise Exception.Create('Unable to load private key');
    Try
      // ... get EC Key
      oEC_EKEY := EVP_PKEY_get1_EC_KEY(oPrivKey);
      if oEC_EKEY = nil then
        raise Exception.Create('Unable to get EC Key');

      // ... hash SHA256
      case aAlgorithm of
        jwtES256:
          oBytes := GetHashSHA256(aValue);
        jwtES384:
          oBytes := GetHashSHA384(aValue);
        jwtES512:
          oBytes := GetHashSHA512(aValue);
      end;

      // ... sign ECDSA
      oSig := ECDSA_do_sign(@oBytes[0], Length(oBytes), oEC_EKEY);
      if oSig = nil then
        raise Exception.Create('Unable to sign ECDSA');

      // ... convert RS
      try
        result := DoConvertRS(oSig, aAlgorithm);
      finally
        ECDSA_SIG_free(oSig);
      end;
    Finally
      EVP_PKEY_free(oPrivKey);
    End;
  Finally
    BIO_free(oPrivKeyBIO);
  End;
{$ELSE}
  raise Exception.Create('DoSignES not supported. Indy version is too old.');
{$ENDIF}
end;

function TsgcHTTP_JWT_ES.DoValidateES(const aHeader, aPayload, aSignature,
  aPublicKey: String; aAlgorithm: TsgcHTTP_JWT_Algorithm): Boolean;
{$IFDEF INDY10_6_0_5169}
var
  oPublicKey: PEVP_PKEY;
  oECKey: PEC_KEY;
  oSig: PECDSA_SIG;
  oHash: TBytes;
{$ENDIF}
begin
{$IFDEF INDY10_6_0_5169}
  oHash := nil;

  oPublicKey := PEVP_PKEY(LoadPublicKey(aPublicKey));

  DoLoadOpenSSLMethods;

  oECKey := EVP_PKEY_get1_EC_KEY(oPublicKey);
  if oECKey = nil then
    raise Exception.Create('Cannot get EC_KEY.');
  try
    oSig := PECDSA_SIG
      (DoSignatureRS(sgcDecodeBase64(sgcURLDecode(aSignature))));
    try
      // ... hash SHA256
      case aAlgorithm of
        jwtES256:
          oHash := GetHashSHA256(sgcGetBytesFromUTF8String(aHeader + '.' +
            aPayload));
        jwtES384:
          oHash := GetHashSHA384(sgcGetBytesFromUTF8String(aHeader + '.' +
            aPayload));
        jwtES512:
          oHash := GetHashSHA512(sgcGetBytesFromUTF8String(aHeader + '.' +
            aPayload));
      end;
      result := ECDSA_do_verify(@oHash[0], Length(oHash), oSig, oECKey) = 1;
    finally
      ECDSA_SIG_free(oSig);
    end;
  finally
    EC_KEY_free(oECKey);
  end;
{$ELSE}
  result := False;
{$ENDIF}
end;

function TsgcHTTP_JWT_ES.SignES256(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignES(aValue, aKey, jwtES256);
end;

function TsgcHTTP_JWT_ES.SignES384(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignES(aValue, aKey, jwtES384);
end;

function TsgcHTTP_JWT_ES.SignES512(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignES(aValue, aKey, jwtES512);
end;

function TsgcHTTP_JWT_ES.ValidateES256(const aHeader, aPayload, aSignature,
  aPublicKey: String): Boolean;
begin
  result := DoValidateES(aHeader, aPayload, aSignature, aPublicKey, jwtES256);
end;

function TsgcHTTP_JWT_ES.ValidateES384(const aHeader, aPayload, aSignature,
  aPublicKey: String): Boolean;
begin
  result := DoValidateES(aHeader, aPayload, aSignature, aPublicKey, jwtES384);
end;

function TsgcHTTP_JWT_ES.ValidateES512(const aHeader, aPayload, aSignature,
  aPublicKey: String): Boolean;
begin
  result := DoValidateES(aHeader, aPayload, aSignature, aPublicKey, jwtES512);
end;

{$ENDIF}

end.
