{***************************************************************************
 sgcHTTP component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcHTTP_Helpers;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils;

{$IFDEF SGC_HTTP}

function GetParamsFromHeaders(const aHeaders: TStringList): string;
function sgcPathEncode(const aPath: String): string;
function GetBoundaryFromContentType(const aContentType: string): string;
{$ENDIF}


implementation

{$IFDEF SGC_HTTP}

uses
  {$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
  sgcBase_Helpers, sgcWebSocket_Helpers;

function GetParamsFromHeaders(const aHeaders: TStringList): string;
var
  i, j: Integer;
  vHeader: String;
begin
  result := '';

  {$IFDEF SGC_HTTP2}
  if LeftStr(aHeaders[0], 7) = ':method' then
    vHeader := aHeaders.values[':path']
  else
  {$ENDIF}
    vHeader := aHeaders[0];

  i := AnsiPos('?', vHeader);
  if i > 0 then
  begin
    result := MidStr(vHeader, i + 1, Length(vHeader));
    j := AnsiPos(' ', result);
    if j > 0 then
      result := LeftStr(result, j - 1);
  end;
end;

function sgcPathEncode(const aPath: String): string;
begin
  Result := TIdURI.PathEncode(aPath);
  Result := sgcStringReplace(Result, '/', '%2F');
  Result := sgcStringReplace(Result, ':', '%3A');
end;

function GetBoundaryFromContentType(const aContentType: string): string;
var
  oList: TStringList;
begin
  oList := TStringList.Create;
  Try
    oList.Delimiter := ';';
    oList.DelimitedText := aContentType;
    result := oList.Values['boundary'];
  Finally
    sgcFree(oList);
  End;
end;

{$ENDIF}


end.
