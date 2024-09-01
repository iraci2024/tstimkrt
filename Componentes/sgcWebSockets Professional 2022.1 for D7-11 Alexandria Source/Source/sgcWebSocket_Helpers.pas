{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Helpers;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
{$IFNDEF LAZARUS}
  Masks,
{$ENDIF}
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCoderMime{$ELSE}IdCoderMime{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHeaderList{$ELSE}IdHeaderList{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSync{$ELSE}IdSync{$ENDIF},
{$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
  sgcBase_Helpers;

type
  TsgcArrayOfBytes =
  {$IFDEF DXE}TArray<System.Byte>{$ELSE} Array of Byte{$ENDIF};

  TsgcDelimitedStringList = class(TStringList)
  public
    constructor Create;
  end;

  TsgcFileStream = class(TFileStream)
{$IFNDEF D2007}
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string; Mode: Word);
      reintroduce; overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
      reintroduce; overload;
  public
    property FileName: string read FFileName;
{$ENDIF}
  end;

function HexToBase64(aText: string): string;
function sgcBytesToString(aStream: TsgcStringStream): String;
function sgcStringToBytes(const aText: String): TIdBytes;
function DecodeGETHeader(aHeaders: TStringList): String;
function DecodePOSTHeader(aHeaders: TStringList): String;
function DecodeGETFullPath(aHeaders: TStringList): String;
function DecodePOSTFullPath(aHeaders: TStringList): String;
function DecodeGETProtocol(aHeaders: TStringList): String;
function IsWebSocketHeader(aHeaders: TStringList): Boolean;
function IsWebsocketAuthenticationHeader(aHeaders: TStringList): Boolean;
function IsAuthenticationHeader(aHeaders: TStringList): Boolean;
function IsRequestAuthenticationSessionHeader(aHeaders: TStringList): Boolean;
function IsRequestFlashPolicy(aHeaders: TStringList): Boolean;
function IsAuthenticationURLHeader(aHeaders: TStringList): Boolean;
function IsAuthenticationSessionHeader(aHeaders: TStringList): Boolean;
function IsAuthenticationBasicHeader(aHeaders: TStringList): Boolean;
function IsWatchDogConnection(aHeaders: TStringList; aSecret: String): Boolean;
function IsOAuth2Header(aHeaders: TStringList; aAuthURL: String;
  aMethod: String = 'GET'): Boolean;
function IsSSEHeader(aHeaders: TStringList): Boolean;
function IsXHRHeader(aHeaders: TStringList): Boolean;
function IsHTTPProtocolHeader(aHeaders: TStringList): Boolean;
function IsHTTP2Header(aValue: string): Boolean;
function IsHTTPHeader(aHeaders: TStringList): Boolean;
function DecodeGETFileName(aText: string): String;
function DecodeAuthorizationBasic(aHeaders: TStringList): String;
function DecodeAuthorizationBearer(aHeaders: TStringList): String;
function DecodeWSSURL(const aText: String; var Host: String; var Port: Integer;
  var SSL: Boolean): Boolean;

procedure ResizeStream(aStream: TStream; aSize: Integer);

function sgcStartsText(const aText, aSubText: String): Boolean;
function sgcMatchesMask(const FileName, Mask: string): Boolean;
procedure sgcFree({$IFDEF D10_4}const [ref] Obj: TObject{$ELSE}var Obj{$ENDIF});

procedure sgcWSStreamWrite(const aHeader: String; var Stream: TMemoryStream);
function sgcWSStreamWriteFile(const aHeader: String; const Stream: TFileStream)
  : TFileStream;
procedure sgcWSStreamRead(var Stream: TMemoryStream; var Header: String);

function GetTEMPDirectory: String;

function GetEncodedString(const aText: String): string;
function GetDecodedString(const aText: String): string;

function sgcArrayOfBytesToStringWithoutNull(aArray: TsgcArrayOfBytes): string;
function sgcArrayOfBytesToString(aArray: TsgcArrayOfBytes): String;
function StringTosgcArrayOfBytes(aString: string): TsgcArrayOfBytes;
procedure sgcAddBytes(aBytesOrg: TIdBytes; var aBytesDest: TIdBytes);

procedure SynchronizeMethod(aMethod: TThreadMethod);
procedure NotifyMethod(aMethod: TThreadMethod);

procedure ParseURL(const aURL: String; out Protocol, Host, Port, Path,
  Query: String);

function DateToMilliseconds(aDate: TDateTime): Int64;
function sgcFetch(var aText: string; const aDelim: string;
  const aDelete: Boolean = True): string;

function sgcGetContentLength(const aHeaders: TStringList): Int64; overload;
function sgcGetContentLength(const aHeaders: TIdHeaderList): Int64; overload;
function sgcGetContentLength(const aHeader: string): Int64; overload;

implementation

uses
{$IFDEF SGC_HTTP2} sgcHTTP2_Const, {$ENDIF}
  sgcWebSocket_Const;

const
  MemoryDelta = $2000;

function HexToBase64(aText: string): string;
{$IFDEF NEXTGEN}
  function Encode_Byte(b: Byte): Char;
  const
    Base64Code
      : string =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  begin
    Result := Base64Code[(b and $3F){$IFDEF LINUX64} + 1{$ENDIF}];
  end;
{$ELSE}
  function Encode_Byte(b: Byte): AnsiChar;
  const
    Base64Code: string[64]
      = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  begin
    Result := Base64Code[(b and $3F) + 1];
  end;
{$ENDIF}

var
  i: Integer;
begin
  aText := HexToString(aText);

  i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF};
  Result := '';
  while i <= Length(aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
  begin
    Result := Result + String(Encode_Byte((Byte(aText[i]) shr 2) and $FF));
    if i + 1 <= Length(aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} then
      Result := Result +
        String(Encode_Byte(((Byte(aText[i]) shl 4) or (Byte(aText[i + 1]) shr 4)
        ) and $FF))
    else
      Result := Result + String(Encode_Byte((Byte(aText[i]) shl 4) and $FF));
    if i + 2 <= Length(aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} then
      Result := Result +
        String(Encode_Byte(((Byte(aText[i + 1]) shl 2) or
        (Byte(aText[i + 2]) shr 6)) and $FF))
    else if i + 1 <= Length(aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} then
      Result := Result + String(Encode_Byte((Byte(aText[i + 1]) shl 2) and $FF))
    else
      Result := Result + '=';
    if i + 2 <= Length(aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} then
      Result := Result + String(Encode_Byte(Byte(aText[i + 2])))
    else
      Result := Result + '=';
    Inc(i, 3);
  end;
end;




function sgcBytesToString(aStream: TsgcStringStream): String;
{$IFDEF LAZARUS }
var
  i: Integer;
{$ENDIF}
begin
  Result := '';
{$IFDEF LAZARUS}
{$IFDEF WINDOWS}
  if MultiByteToWideChar(65001, 8, PAnsiChar(aStream.Bytes),
    Length(aStream.Bytes), nil, 0) = 0 then
    exit;
{$ENDIF}
  For i := 0 to Length(aStream.Bytes) - 1 do
    Result := Result + Char(aStream.Bytes[i]);
{$ELSE}
{$IFDEF INDY10_5_5}
{$IFDEF D2010}
  Result := BytesToString(TIdBytes(aStream.Bytes), 0, aStream.size,
    {$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF});
{$ELSE}
  {$IFDEF LAZARUS}
  Result := BytesToString(TIdBytes(aStream.Bytes), 0, aStream.size,
    {$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF});
  {$ELSE}
  Result := UTF8Decode(aStream.DataString);
  {$ENDIF}
{$ENDIF}    
{$ELSE}
{$IFDEF INDY10_2}
    Result := BytesToString(TIdBytes(aStream.Bytes), 0, aStream.size);
{$ELSE}
    Result := UTF8Decode(aStream.DataString);
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function sgcStringToBytes(const aText: String): TIdBytes;
begin
  SetLength(Result, Length(aText));
  if Length(Result) > 0 then
    sgcMove(aText[1], Result[0], Length(aText));
end;

function DecodeGETHeader(aHeaders: TStringList): String;
begin
  Result := '';
  if aHeaders.Count > 0 then
  begin
    if sgcMatchesMask(aHeaders[0], 'GET *') then
      Result := aHeaders[0];
  end;
end;

function DecodeGETFileName(aText: string): String;
var
  i: Integer;
begin
  if aText <> '' then
  begin
    i := AnsiPos(' HTTP', aText);
    Result := MidStr(aText, 5, i - 5);
    Result := MidStr(Result, LastDelimiter('/', Result) + 1, Length(Result));
  end;
end;

function DecodeGETFullPath(aHeaders: TStringList): String;
var
  i: Integer;
  vText: String;
begin
  vText := DecodeGETHeader(aHeaders);
  if vText <> '' then
  begin
    i := AnsiPos(' HTTP', vText);
    Result := Trim(MidStr(vText, 5, i - 5));
  end;
end;

function IsWebSocketHeader(aHeaders: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  if IsHTTPHeader(aHeaders) then
  begin
    if DecodeGETHeader(aHeaders) <> '' then
    begin
      for i := 0 to aHeaders.Count - 1 do
      begin
        if sgcContainsText(aHeaders[i], 'Upgrade: websocket') then
        begin
          Result := True;
          break;
        end;
      end;
    end;
  end;
end;

procedure ResizeStream(aStream: TStream; aSize: Integer);
var
  oStream: TMemoryStream;
begin
  aStream.Position := aStream.size - aSize;
  oStream := TMemoryStream.Create;
  Try
    oStream.CopyFrom(aStream, aSize);
    aStream.size := aSize;
    aStream.Position := 0;
    oStream.Position := 0;
    aStream.CopyFrom(oStream, aSize);
  Finally
    sgcFree(oStream);
  End;
end;

function IsAuthenticationHeader(aHeaders: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcContainsText(aHeaders[i], CS_AUTHORIZATION)
    {$IFDEF SGC_HTTP2} or sgcContainsText(aHeaders[i], CS_AUTHORIZATION_HTTP2){$ENDIF} then
    begin
      Result := True;
      break;
    end;
  end;
end;

function IsWebsocketAuthenticationHeader(aHeaders: TStringList): Boolean;
begin
  Result := IsWebSocketHeader(aHeaders);
  if Result then
    Result := sgcContainsText(DecodeGETHeader(aHeaders), 'GET /sgc/auth/');
end;

function IsRequestAuthenticationSessionHeader(aHeaders: TStringList): Boolean;
begin
  Result := sgcContainsText(DecodeGETHeader(aHeaders),
    'GET ' + CS_REQ_AUTH_SESSION);
end;

function IsAuthenticationURLHeader(aHeaders: TStringList): Boolean;
begin
  Result := sgcContainsText(DecodeGETHeader(aHeaders), 'GET ' + CS_AUTH_URL);
end;

function IsAuthenticationSessionHeader(aHeaders: TStringList): Boolean;
begin
  Result := sgcContainsText(DecodeGETHeader(aHeaders),
    'GET ' + CS_AUTH_SESSION);
end;

function sgcMatchesMask(const FileName, Mask: string): Boolean;
begin
{$IFDEF LAZARUS}
  Result := IsWild(FileName, Mask, True);
{$ELSE}
  Result := MatchesMask(FileName, Mask);
{$ENDIF}
end;

constructor TsgcDelimitedStringList.Create;
begin
  inherited;
  Delimiter := CS_DELIMITER;
{$IFDEF D2006}
  StrictDelimiter := True;
{$ENDIF}
end;

{$IFNDEF D2009}
{$ENDIF}

procedure sgcFree({$IFDEF D10_4}const [ref] Obj: TObject{$ELSE}var Obj{$ENDIF});
begin
  sgcBase_Helpers.sgcFree(Obj);
end;

function IsAuthenticationBasicHeader(aHeaders: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcContainsText(aHeaders[i], CS_AUTH_BASIC) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function DecodeAuthorizationBasic(aHeaders: TStringList): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcContainsText(aHeaders[i], CS_AUTH_BASIC) then
    begin
      Result := DecodeBase64(MidStr(aHeaders[i], Length(CS_AUTH_BASIC) + 2,
        Length(aHeaders[i])));
      break;
    end;
  end;
end;

function DecodeGETProtocol(aHeaders: TStringList): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcMatchesMask(aHeaders[i], 'Sec-WebSocket-Protocol: *') then
    begin
      Result := Copy(aHeaders[i], 25, Length(aHeaders[i]) - 24);
      break;
    end;
  end;
end;

function IsRequestFlashPolicy(aHeaders: TStringList): Boolean;
begin
  Result := False;
  if aHeaders.Count > 0 then
    Result := aHeaders[0] = CS_FLASH_POLICY_REQUEST;
end;

function IsSSEHeader(aHeaders: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcMatchesMask(aHeaders[i], 'Accept: text/event-stream') then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure sgcWSStreamWrite(const aHeader: String; var Stream: TMemoryStream);
var
  oSize: TsgcStringStream;
  oHeader: TsgcStringStream;
  oStream: TMemoryStream;
begin
  Stream.Position := 0;

  oStream := TMemoryStream.Create;
  Try
    // ... write header
    oHeader := TsgcStringStream.Create(aHeader);
    Try
      oSize := TsgcStringStream.Create(Format('%.10d', [oHeader.size]));
      Try
        oStream.CopyFrom(oSize, oSize.size);
        oStream.CopyFrom(oHeader, oHeader.size);
      Finally
        sgcFree(oSize);
      End;
    Finally
      sgcFree(oHeader);
    End;

    // ... write stream
    oStream.CopyFrom(Stream, Stream.size);
    oStream.Position := 0;

    // ... return stream;
    Stream.Clear;
    Stream.CopyFrom(oStream, oStream.size);
    Stream.Position := 0;
  Finally
    sgcFree(oStream);
  End;
end;

procedure sgcWSStreamRead(var Stream: TMemoryStream; var Header: String);
var
  oHeader: TsgcStringStream;
  oStream: TMemoryStream;
  vSize: Integer;
begin
  Header := '';

  Stream.Position := 0;
  // ... read header
  oHeader := TsgcStringStream.Create('');
  Try
    oHeader.CopyFrom(Stream, 10);
    TryStrToInt(oHeader.DataString, vSize);
    if vSize > 0 then
    begin
      oHeader.size := 0;
      oHeader.Position := 0;
      oHeader.CopyFrom(Stream, vSize);
      Header := oHeader.DataString;

      // ... read stream
      oStream := TMemoryStream.Create;
      Try
        oStream.CopyFrom(Stream, Stream.size - oHeader.Position - 10);
        oStream.Position := 0;

        // ... return stream;
        Stream.Clear;
        Stream.CopyFrom(oStream, oStream.size);
        Stream.Position := 0;
      Finally
        sgcFree(oStream);
      End;
    end;
  Finally
    sgcFree(oHeader);
  End;
end;

function IsXHRHeader(aHeaders: TStringList): Boolean;
begin
  Result := sgcMatchesMask(DecodePOSTHeader(aHeaders),
    'POST ' + CS_REQ_XHR + '*');
  // http/2
{$IFDEF SGC_HTTP2}
  if not result then
  begin
    if aHeaders.Count > 0 then
    begin
      if aHeaders[0] = ':method=POST' then
        Result := sgcMatchesMask(aHeaders.Values[':path'], '*' + CS_REQ_XHR + '*');
    end;
  end;
{$ENDIF}
end;

function DecodePOSTHeader(aHeaders: TStringList): String;
begin
  Result := '';
  if aHeaders.Count > 0 then
  begin
    if sgcMatchesMask(aHeaders[0], 'POST *') then
      Result := aHeaders[0];
  end;
end;

function DecodePOSTFullPath(aHeaders: TStringList): String;
var
  i: Integer;
  vText: String;
begin
  vText := DecodePOSTHeader(aHeaders);
  if vText <> '' then
  begin
    i := AnsiPos(' HTTP', vText);
    Result := Trim(MidStr(vText, 5, i - 5));
  end;
  // http/2
  {$IFDEF SGC_HTTP2}
  if result = '' then
  begin
    if aHeaders.Count > 0 then
    begin
    if aHeaders[0] = ':method=POST' then
      Result := aHeaders.Values[':path'];
    end;
  end;
  {$ENDIF}
end;

function sgcWSStreamWriteFile(const aHeader: String; const Stream: TFileStream)
  : TFileStream;
var
  oSize: TsgcStringStream;
  oHeader: TsgcStringStream;
  vFileName: string;
begin
  Stream.Position := 0;

  vFileName := GetTEMPDirectory + NewGuid;
  Result := TFileStream.Create(vFileName, fmCreate);

  // ... write header
  oHeader := TsgcStringStream.Create(aHeader);
  Try
    oSize := TsgcStringStream.Create(Format('%.10d', [oHeader.size]));
    Try
      Result.CopyFrom(oSize, oSize.size);
      Result.CopyFrom(oHeader, oHeader.size);
    Finally
      sgcFree(oSize);
    End;
  Finally
    sgcFree(oHeader);
  End;

  // ... write stream
  Result.CopyFrom(Stream, Stream.size);
  Result.Position := 0;
end;

function GetTEMPDirectory: String;
begin
{$IFDEF LAZARUS}
  Result := IncludeTrailingPathDelimiter
    (SysUtils.GetEnvironmentVariable('TEMP'));
{$ELSE}
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
{$ENDIF}
end;

function DecodeWSSURL(const aText: String; var Host: String; var Port: Integer;
  var SSL: Boolean): Boolean;
var
  oList: TsgcDelimitedStringList;
begin
  Result := False;

  oList := TsgcDelimitedStringList.Create;
  Try
    oList.Delimiter := ':';
    oList.DelimitedText := aText;
    if oList.Count = 3 then
    begin
      if UpperCase(oList[0]) = 'WSS' then
        SSL := True
      else
        SSL := False;
      Host := sgcStringReplace(oList[1], '/', '');
      Port := StrToInt(oList[2]);

      Result := True;
    end;
  Finally
    sgcFree(oList);
  End;
end;

function GetEncodedString(const aText: String): string;
begin
  Result := aText;
  if LeftStr(aText, Length(CS_ENC_B64)) <> CS_ENC_B64 then
    Result := CS_ENC_B64 + EncodeBase64(aText);
end;

function GetDecodedString(const aText: String): string;
begin
  Result := aText;
  if LeftStr(aText, Length(CS_ENC_B64)) = CS_ENC_B64 then
    Result := DecodeBase64(MidStr(aText, Length(CS_ENC_B64) + 1,
      Length(aText)));
end;

function sgcArrayOfBytesToString(aArray: TsgcArrayOfBytes): String;
begin
{$IFDEF DXE}
  Result := TEncoding.UTF8.GetString(aArray);
{$ELSE}
  SetString(Result, PAnsiChar(@aArray[0]), Length(aArray));
{$ENDIF}
end;

function StringTosgcArrayOfBytes(aString: string): TsgcArrayOfBytes;
{$IFNDEF DXE}
var
  vString: {$IFDEF D2009}RawByteString{$ELSE}String{$ENDIF};
{$ENDIF}
begin
{$IFDEF DXE}
  Result := TEncoding.UTF8.GetBytes(aString);
{$ELSE}
  vString := {$IFDEF D2009}RawByteString{$ENDIF}(aString);
  SetLength(Result, Length(vString));
  if Length(Result) > 0 then
    sgcMove(vString[1], Result[0], Length(vString));
{$ENDIF}
end;

function sgcArrayOfBytesToStringWithoutNull(aArray: TsgcArrayOfBytes): string;
var
  i, j: Integer;
  vArray: TsgcArrayOfBytes;
begin
  Result := '';
  j := 0;
  SetLength(vArray, Length(aArray));

  for i := 0 to Length(aArray) - 1 do
  begin
    if aArray[i] <> 0 then
    begin
      vArray[j] := aArray[i];
      Inc(j);
    end;
  end;

  if j > 0 then
  begin
    SetLength(vArray, j);
    Result := sgcArrayOfBytesToString(vArray);
  end;
end;

procedure NotifyMethod(aMethod: TThreadMethod);
begin
{$IFDEF D10_2}
  TThread.Queue(nil, aMethod)
{$ELSE}
  TIdNotify.NotifyMethod(aMethod);
{$ENDIF}
end;

procedure SynchronizeMethod(aMethod: TThreadMethod);
begin
{$IFDEF INDY10_6_2_5366}
  TThread.Synchronize(nil, aMethod);
{$ELSE}
  TIdSync.SynchronizeMethod(aMethod);
{$ENDIF}
end;

procedure ParseURL(const aURL: String; out Protocol, Host, Port, Path,
  Query: String);
var
  oURI: TIdURI;
begin
  oURI := TIdURI.Create(aURL);
  Try
    Protocol := oURI.Protocol;
    Host := oURI.Host;
    Port := oURI.Port;
    Path := oURI.Path;
    if oURI.Document <> '' then
      Path := Path + oURI.Document;
    Query := oURI.Params;
  Finally
    sgcFree(oURI);
  End;
end;

procedure sgcAddBytes(aBytesOrg: TIdBytes; var aBytesDest: TIdBytes);
var
  i: Integer;
begin
  i := Length(aBytesDest);
  SetLength(aBytesDest, i + Length(aBytesOrg));
  CopyTIdBytes(aBytesOrg, 0, aBytesDest, i, Length(aBytesOrg));
end;

function DateToMilliseconds(aDate: TDateTime): Int64;
var
  oTimeStamp: TTimeStamp;
begin
  oTimeStamp := DateTimeToTimeStamp(aDate);
  Result := (Int64(oTimeStamp.Date) * MSecsPerDay) + oTimeStamp.Time;
end;

function IsHTTPHeader(aHeaders: TStringList): Boolean;
begin
  Result := False;
  if aHeaders.Count > 0 then
  begin
    if sgcMatchesMask(aHeaders[0], '*HTTP/1.*') then
      Result := True;
  end;
end;

function sgcFetch(var aText: string; const aDelim: string;
  const aDelete: Boolean = True): string;
var
  i: Integer;
begin
  if aDelim = #0 then
    i := Pos(aDelim, aText)
  else
    i := IndyPos(UpperCase(aDelim), UpperCase(aText));
  if i = 0 then
  begin
    Result := aText;
    if aDelete then
      aText := '';
  end
  else
  begin
    Result := Copy(aText, 1, i - 1);
    if aDelete then
      aText := Copy(aText, i + Length(aDelim), MaxInt);
  end;
end;

function IsHTTPProtocolHeader(aHeaders: TStringList): Boolean;
begin
  Result := sgcMatchesMask(DecodePOSTHeader(aHeaders),
    'POST ' + CS_REQ_PROTOCOL + '*');
  // http/2
{$IFDEF SGC_HTTP2}
  if not result then
  begin
    if aHeaders.Count > 0 then
    begin
      if aHeaders[0] = ':method=POST' then
        Result := sgcMatchesMask(aHeaders.Values[':path'], '*' + CS_REQ_PROTOCOL + '*');
    end;
  end;
{$ENDIF}
end;

function sgcStartsText(const aText, aSubText: String): Boolean;
begin
{$IFDEF D2006}
  Result := StartsText(aSubText, aText);
{$ELSE}
  Result := AnsiStartsText(aSubText, aText);
{$ENDIF}
end;

function IsHTTP2Header(aValue: string): Boolean;
begin
{$IFDEF SGC_HTTP2}
  Result := LeftStr(aValue, Length(CS_HTTP2_CLIENT_PREFACE)) = CS_HTTP2_CLIENT_PREFACE;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWatchDogConnection(aHeaders: TStringList; aSecret: String): Boolean;
begin
  Result := False;
  if aHeaders.Count > 0 then
  begin
    if sgcMatchesMask(aHeaders[0], '*/' + aSecret + '*') then
      Result := True;
  end;
end;

function IsOAuth2Header(aHeaders: TStringList; aAuthURL: String;
  aMethod: String = 'GET'): Boolean;
var
  i: Integer;
begin
  Result := False;

  // http1.*
  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcMatchesMask(aHeaders[i], '*' + aMethod + '*' + aAuthURL + '?*') then
    begin
      Result := True;
      break;
    end;
  end;

  // http/2
{$IFDEF SGC_HTTP2}
  if not Result then
    Result := sgcMatchesMask(aHeaders.Values[':path'], '*' + aAuthURL + '?*');
{$ENDIF}
end;

function DecodeAuthorizationBearer(aHeaders: TStringList): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to aHeaders.Count - 1 do
  begin
    if sgcContainsText(aHeaders[i], CS_AUTH_BEARER) then
    begin
      Result := MidStr(aHeaders[i], Length(CS_AUTH_BEARER) + 2,
        Length(aHeaders[i]));
      break;
    end
{$IFDEF SGC_HTTP2}
    else if sgcContainsText(aHeaders[i], CS_AUTH_BEARER_HTTP2) then
    begin
      Result := MidStr(aHeaders[i], Length(CS_AUTH_BEARER_HTTP2) + 2,
        Length(aHeaders[i]));
      break;
    end
{$ENDIF}
  end;
end;

function sgcGetContentLength(const aHeaders: TStringList): Int64;
begin
  result := sgcGetContentLength(aHeaders.Values['Content-Length']);
  {$IFDEF SGC_HTTP2}
  if result = -1 then
    result := sgcGetContentLength(aHeaders.Values['content-length']);
  {$ENDIF}
end;

function sgcGetContentLength(const aHeader: string): Int64;
begin
  result := -1;
  if aHeader <> '' then
  begin
  {$IFDEF D2009}
    Result := StrToInt64(aHeader);
  {$ELSE}
    Result := StrToInt64Def(aHeader, -1);
  {$ENDIF}
  end;
end;

function sgcGetContentLength(const aHeaders: TIdHeaderList): Int64; overload;
begin
  result := sgcGetContentLength(aHeaders.Values['Content-Length']);
  {$IFDEF SGC_HTTP2}
  if result = -1 then
  begin
    aHeaders.NameValueSeparator := '=';
    Try
      result := sgcGetContentLength(aHeaders.Values['content-length']);
    Finally
      aHeaders.NameValueSeparator := ':';
    End;
  end;
  {$ENDIF}
end;

{$IFNDEF D2007}

constructor TsgcFileStream.Create(const AFileName: string; Mode: Word);
begin
  inherited;
  FFileName := AFileName;
end;

constructor TsgcFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
begin
  inherited;
  FFileName := AFileName;
end;

{$ENDIF}

initialization


finalization

end.
