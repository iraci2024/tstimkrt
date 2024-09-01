{ ***************************************************************************
  sgcJWT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_Helpers;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}

uses
  Classes, SysUtils, StrUtils,
  {$IFDEF DXE7}System.NetEncoding,{$ENDIF}
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdCoderMime{$ELSE}IdCoderMime{$ENDIF},
  sgcBase_Helpers;


function sgcURLEncode(const aValue: string): string; overload;
function sgcURLDecode(const aValue: string): string;
function sgcEncodeBase64(const aBytes: TBytes): string;
function sgcDecodeBase64(const aValue: string): TBytes;


{$ENDIF}

implementation

{$IFDEF SGC_JWT}

function sgcEncodeBase64(const aBytes: TBytes): string;
begin
  {$IFDEF DXE7}
  Result := sgcGetUTF8StringFromBytes(TNetEncoding.Base64.Encode(aBytes));
  {$ELSE}
    {$IFDEF INDY10_5_7}
    result := TIdEncoderMIME.EncodeBytes(TIdBytes(aBytes));
    {$ELSE}
    raise Exception.Create('sgcEncodeBase64 not supported. Indy version is too old.');
    {$ENDIF}
  {$ENDIF}
end;

function sgcURLEncode(const aValue: string): string; overload;
begin
  result := sgcStringReplace(aValue, #13#10, '');
  result := sgcStringReplace(result, #13, '');
  result := sgcStringReplace(result, #10, '');
  while RightStr(result, 1) = '=' do
    result := LeftStr(result, Length(result) - 1);
  result := sgcStringReplace(result, '+', '-');
  result := sgcStringReplace(result, '/', '_');
end;

function sgcURLDecode(const aValue: string): string;
begin
  result := aValue + StringOfChar('=', (4 - Length(aValue) mod 4) mod 4);
  result := sgcStringReplace(result, '-', '+');
  result := sgcStringReplace(result, '_', '/');
end;

function sgcDecodeBase64(const aValue: string): TBytes;
begin
  {$IFDEF DXE7}
  Result := TNetEncoding.Base64.Decode(sgcGetBytesFromUTF8String(aValue));
  {$ELSE}
    {$IFDEF INDY10_5_7}
    result := TBytes(TIdDecoderMIME.DecodeBytes(aValue));
    {$ELSE}
    raise Exception.Create('sgcDecodeBase64 not supported. Indy version is too old.');
    {$ENDIF}
  {$ENDIF}
end;



{$ENDIF}

end.
