{ ***************************************************************************
  sgcJWT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_Types;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}

Type
  TsgcHTTP_JWT_Algorithm = (jwtNone, jwtHS256, jwtHS384, jwtHS512, jwtRS256,
    jwtRS384, jwtRS512, jwtES256, jwtES384, jwtES512);

function GetAlgorithmString(aAlgorithm: TsgcHTTP_JWT_Algorithm): string;
function GetAlgorithmFromString(const aValue: string): TsgcHTTP_JWT_Algorithm;

{$ENDIF}

implementation

{$IFDEF SGC_JWT}

function GetAlgorithmString(aAlgorithm: TsgcHTTP_JWT_Algorithm): string;
begin
  case aAlgorithm of
    jwtHS256:
      result := 'HS256';
    jwtHS384:
      result := 'HS384';
    jwtHS512:
      result := 'HS512';
    jwtRS256:
      result := 'RS256';
    jwtRS384:
      result := 'RS384';
    jwtRS512:
      result := 'RS512';
    jwtES256:
      result := 'ES256';
    jwtES384:
      result := 'ES384';
    jwtES512:
      result := 'ES512';
  end;
end;

function GetAlgorithmFromString(const aValue: string): TsgcHTTP_JWT_Algorithm;
begin
  result := jwtNone;
  if aValue = 'HS256' then
    result := jwtHS256
  else if aValue = 'HS384' then
    result := jwtHS384
  else if aValue = 'HS512' then
    result := jwtHS512
  else if aValue = 'RS256' then
    result := jwtRS256
  else if aValue = 'RS384' then
    result := jwtRS384
  else if aValue = 'RS512' then
    result := jwtRS512
  else if aValue = 'ES256' then
    result := jwtES256
  else if aValue = 'ES384' then
    result := jwtES384
  else if aValue = 'ES512' then
    result := jwtES512;
end;

{$ENDIF}

end.
