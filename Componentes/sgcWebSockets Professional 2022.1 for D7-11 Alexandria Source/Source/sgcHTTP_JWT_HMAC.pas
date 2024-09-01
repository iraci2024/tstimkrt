{ ***************************************************************************
  sgcJWT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_HMAC;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders{$ELSE}IdSSLOpenSSLHeaders{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
  // sgc
  sgcBase_Helpers, sgcHTTP_JWT_Classes, sgcHTTP_JWT_Types;

type
  TsgcHTTP_JWT_HMAC = class(TsgcHTTP_JWT_Base)
  protected
    function DoSignHMAC(aValue, aSecret: TBytes;
      aAlgorithm: TsgcHTTP_JWT_Algorithm): TBytes; virtual;
    function DoValidateHMAC(const aHeader, aPayload, aSignature,
      aSecret: String; aAlgorithm: TsgcHTTP_JWT_Algorithm): Boolean;
  public
    function SignHMAC256(aValue, aKey: TBytes): TBytes;
    function SignHMAC384(aValue, aKey: TBytes): TBytes;
    function SignHMAC512(aValue, aKey: TBytes): TBytes;
  public
    function ValidateHMAC256(const aHeader, aPayload, aSignature,
      aSecret: String): Boolean;
    function ValidateHMAC384(const aHeader, aPayload, aSignature,
      aSecret: String): Boolean;
    function ValidateHMAC512(const aHeader, aPayload, aSignature,
      aSecret: String): Boolean;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_JWT}

uses
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF INDY10_5_7}
{$IFDEF SGC_INDY}sgcIdHashSHA{$ELSE}IdHashSHA{$ENDIF},
{$ENDIF}
{$IFDEF INDY10_2}
{$IFDEF SGC_INDY}sgcIdHMAC{$ELSE}IdHMAC{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHMACSHA1{$ELSE}IdHMACSHA1{$ENDIF},
{$ENDIF}
  sgcHTTP_Const, sgcHTTP_JWT_Helpers;

function TsgcHTTP_JWT_HMAC.DoSignHMAC(aValue, aSecret: TBytes;
  aAlgorithm: TsgcHTTP_JWT_Algorithm): TBytes;
{$IFDEF INDY10_6_0_5169}
var
  oHMAC: TIdHMAC;
{$ENDIF}
begin
{$IFDEF INDY10_6_0_5169}
  if not LoadOpenSSLLibrary then
    raise EIdOSSLCouldNotLoadSSLLibrary.Create(CS_HTTP_COULD_NOT_LOAD_OPENSSL);

  case aAlgorithm of
    jwtHS256:
      begin
        if not TIdHashSHA256.IsAvailable then
          raise Exception.Create('SHA256 hashing is not available!');
        oHMAC := TIdHMACSHA256.Create;
      end;
    jwtHS384:
      begin
        if not TIdHashSHA256.IsAvailable then
          raise Exception.Create('SHA384 hashing is not available!');
        oHMAC := TIdHMACSHA384.Create;
      end;
    jwtHS512:
      begin
        if not TIdHashSHA256.IsAvailable then
          raise Exception.Create('SHA512 hashing is not available!');
        oHMAC := TIdHMACSHA512.Create;
      end;
  end;

  Try
    oHMAC.Key := TIdBytes(aSecret);
    result := TBytes(oHMAC.HashValue(TIdBytes(aValue)));
  Finally
    sgcFree(oHMAC);
  End;
{$ELSE}
    raise Exception.Create('DoSignHMAC not supported. Indy version is too old.');
{$ENDIF}
end;

function TsgcHTTP_JWT_HMAC.DoValidateHMAC(const aHeader, aPayload, aSignature,
  aSecret: String; aAlgorithm: TsgcHTTP_JWT_Algorithm): Boolean;
var
  oBytes: TBytes;
begin
  oBytes := DoSignHMAC(sgcGetBytesFromUTF8String(aHeader + '.' + aPayload),
    sgcGetBytesFromUTF8String(aSecret), aAlgorithm);
  result := sgcURLEncode(sgcEncodeBase64(oBytes)) = aSignature;
end;

function TsgcHTTP_JWT_HMAC.SignHMAC256(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignHMAC(aValue, aKey, jwtHS256);
end;

function TsgcHTTP_JWT_HMAC.SignHMAC384(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignHMAC(aValue, aKey, jwtHS384);
end;

function TsgcHTTP_JWT_HMAC.SignHMAC512(aValue, aKey: TBytes): TBytes;
begin
  result := DoSignHMAC(aValue, aKey, jwtHS512);
end;

function TsgcHTTP_JWT_HMAC.ValidateHMAC256(const aHeader, aPayload, aSignature,
  aSecret: String): Boolean;
begin
  result := DoValidateHMAC(aHeader, aPayload, aSignature, aSecret, jwtHS256);
end;

function TsgcHTTP_JWT_HMAC.ValidateHMAC384(const aHeader, aPayload, aSignature,
  aSecret: String): Boolean;
begin
  result := DoValidateHMAC(aHeader, aPayload, aSignature, aSecret, jwtHS384);
end;

function TsgcHTTP_JWT_HMAC.ValidateHMAC512(const aHeader, aPayload, aSignature,
  aSecret: String): Boolean;
begin
  result := DoValidateHMAC(aHeader, aPayload, aSignature, aSecret, jwtHS512);
end;

{$ENDIF}

end.
