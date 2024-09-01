{ ***************************************************************************
  sgcBase component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcBase_Helpers;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF DXE2} System.Generics.Collections, {$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF};

type
{$IFNDEF D2007}
  TBytes = array of Byte;
{$ENDIF}

{$IFNDEF D10}
{$IFNDEF D2009}
  Int8 = Shortint;
{$NODEFINE Int8}
  UInt8 = Byte;
{$NODEFINE UInt8}
  Int16 = SmallInt;
{$NODEFINE Int16}
  UInt16 = Word;
{$NODEFINE UInt16}
  Int32 = Integer;
{$NODEFINE Int32}
  UInt32 = Cardinal;
{$NODEFINE UInt32}
{$ENDIF}
  PInt8 = ^Shortint;
{$NODEFINE PInt8}
  PUInt8 = ^Byte;
{$NODEFINE PUInt8}
  PInt16 = ^SmallInt;
{$NODEFINE PInt16}
  PUInt16 = ^Word;
{$NODEFINE PUInt16}
  PInt32 = ^Integer;
{$NODEFINE PInt32}
  PUInt32 = ^UInt32;
{$NODEFINE PUInt32}
  PInt64 = ^Int64;
{$NODEFINE PInt64}
  PUInt64 = ^UInt64;
{$NODEFINE PUInt64}
{$ENDIF}

  TsgcStringStream = class(TStringStream)
{$IFNDEF D2009}
  private
    FBytes: TBytes;
    function GetBytes: TBytes;
  protected
    procedure DoBytes; virtual;
  public
    constructor Create(const ABytes: TBytes); overload;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  public
    property Bytes: TBytes read GetBytes;
{$ENDIF}
  end;

  TsgcBytesStream = class(TMemoryStream)
  private
    function GetBytes: TBytes;
  public
    constructor Create(const ABytes: {$IFDEF D7}TIdBytes{$ELSE}TBytes{$ENDIF});
  public
    property Bytes: TBytes read GetBytes;
  end;

  TsgcSearchList = class
  private
{$IFDEF DXE2}
    FList: TDictionary<string, string>;
{$ELSE}
    FList: TStringList;
{$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Add(const aKey, aValue: String);
    procedure Delete(const aKey: String);
    function Get(const aKey: String): string;
  end;

procedure sgcFree({$IFDEF D10_4}const [ref] Obj: TObject{$ELSE}var Obj{$ENDIF});
procedure sgcTryFree({$IFDEF D10_4}const [ref] Obj
  : TObject{$ELSE}var Obj{$ENDIF});

function GetHMACSHA1(const aValue, aSecret: TBytes): TBytes;
function GetHMACSHA256(const aValue, aSecret: String; aBase64: Boolean = False)
  : String; overload;
function GetHMACSHA256(const aValue: String; aSecret: TIdBytes;
  aBase64: Boolean = False): String; overload;
function GetHMACSHA256(const aValue, aSecret: TBytes): TBytes; overload;
function GetHashSHA256(const aValue: String): TBytes; overload;
function GetHashSHA256(const aValue: TBytes): TBytes; overload;
function GetHashSHA384(const aValue: TBytes): TBytes;
function GetHashSHA512(const aValue: TBytes): TBytes;
function GetHMACSHA512(const aValue, aSecret: TBytes;
  aBase64: Boolean = False): String;
function GetMD5(AText: String): String; overload;
function GetMD5(const ABytes: TBytes): TBytes; overload;
function HexToString(const AText: String): String;
function HexToBytes(AText: String): TIdBytes;

function GetDateTimeUnix(aDateTime: Extended; aUTC: Boolean): String;
function URIEncode(const AText: String): String;
function sgcStringReplace(const AText, aFromText, aToText: String): String;
function sgcGetUTCString(aDate: TDateTime = 0): String;
function sgcGetNodeValue(const aXML, aNode: String): String;
procedure sgcGetNodeList(const aXML, aNode: String; var Nodes: TStringList);
function sgcQuotedStr(const aValue: string): string;

function NewGuid: String;

function sgcGetTicks: Cardinal;
function sgcGetTickDiff(const AOldTickCount, ANewTickCount: Cardinal): Cardinal;

function EncodeBase64(const AText: String; const aUTF8: Boolean = False)
  : String; overload;
function EncodeBase64(const aStream: TMemoryStream): String; overload;
function DecodeBase64(const AText: String): String; overload;
procedure DecodeBase64(const AText: String;
  var aStream: TMemoryStream); overload;
procedure DecodeBase64(const AText: String; var ABytes: TBytes); overload;

function sgcDecodeJSON(const aJSON: string): string;
function sgcEncodeJSON(const AText: String): String;

procedure sgcSetThreadName(const aName: string);

function sgcGetUTF8StringFromBytes(const ABytes: TBytes): String;
function sgcGetBytesFromUTF8String(const aValue: string): TBytes;

function sgcBytesToStringRaw(const ABytes: TBytes): String;
procedure sgcMove(const Source; var Dest; Count: NativeInt);
function sgcGetOpenSSLDefaultFolder: string;
procedure sgcIdOpenSSLSetLibPath(const APath: String);

function sgcContainsText(const AText, aSubText: String): Boolean;
function sgcStringToHex(const aValue: string): string;

// ... bytes
procedure sgcReverseBytes(var aValue: TBytes);
// ... bytes read
function sgcReadByte(const ABytes: TBytes; aOffset: Integer = 0): Byte;
function sgcReadBytes(const ABytes: TBytes; aOffset: Integer = 0;
  aLength: Integer = 0): TBytes;
function sgcReadReverseBytes(const ABytes: TBytes; aOffset: Integer = 0;
  aLength: Integer = 0): TBytes;
function sgcGetInt8FromBytes(const ABytes: TBytes; aOffset: Integer = 0)
  : ShortInt;
function sgcGetUInt8FromBytes(const ABytes: TBytes; aOffset: Integer = 0): Byte;
function sgcGetInt16FromBytes(const ABytes: TBytes; aOffset: Integer = 0)
  : SmallInt;
function sgcGetUInt16FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Word;
function sgcGetInt32FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Int32;
function sgcGetUInt32FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): UInt32;
function sgcGetInt64FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Int64;
function sgcGetUInt64FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): UInt64;
function sgcGetSingleFromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Single;
function sgcGetDoubleFromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Double;
function sgcGetDecimalFromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Double;
// ... bytes write
procedure sgcWriteBytes(const aSource: TBytes; var aDest: TBytes);
procedure sgcWriteByte(const aValue: Byte; var aDest: TBytes);
procedure sgcWriteInt8(const aValue: Int8; var aDest: TBytes);
procedure sgcWriteUInt8(const aValue: UInt8; var aDest: TBytes);
procedure sgcWriteInt16(const aValue: Int16; var aDest: TBytes);
procedure sgcWriteUInt16(const aValue: UInt16; var aDest: TBytes);
procedure sgcWriteInt32(const aValue: Int32; var aDest: TBytes);
procedure sgcWriteUInt32(const aValue: UInt32; var aDest: TBytes);
procedure sgcWriteInt64(const aValue: Int64; var aDest: TBytes);
procedure sgcWriteUInt64(const aValue: UInt64; var aDest: TBytes);
procedure sgcWriteSingle(const aValue: Single; var aDest: TBytes);
procedure sgcWriteDouble(const aValue: Double; var aDest: TBytes);
procedure sgcWriteDecimal(const aValue: Double; var aDest: TBytes);

implementation

uses
  DateUtils, Math,
  // -->start sgc_trial
{$IFDEF SGC_TRIAL}
{$IFNDEF NEXTGEN}
{$IFNDEF BCB}
{$IFNDEF MACOS}
  sgcBase_Const,
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  // <--end sgc_trial
{$IFDEF DXE2}IOUtils, {$ENDIF}
{$IFDEF DXE8}System.Hash, {$ENDIF}
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // indy
{$IFNDEF INDY10_2}IdSysWin32, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdHashMessageDigest{$ELSE}IdHashMessageDigest{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders{$ELSE}IdSSLOpenSSLHeaders{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCoderMIME{$ELSE}IdCoderMIME{$ENDIF},
{$IFDEF INDY10_5_7}{$IFDEF SGC_INDY}sgcIdHashSHA{$ELSE}IdHashSHA{$ENDIF},
{$ENDIF}
{$IFDEF INDY10_2}{$IFDEF SGC_INDY}sgcIdHMAC{$ELSE}IdHMAC{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHMACSHA1{$ELSE}IdHMACSHA1{$ENDIF}, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF};

procedure sgcFree({$IFDEF D10_4}const [ref] Obj: TObject{$ELSE}var Obj{$ENDIF});
begin
{$IFNDEF NEXTGEN}
  FreeAndNil(Obj);
{$ELSE}
  TObject(Obj).DisposeOf;
  TObject(Obj) := nil;
{$ENDIF}
end;

procedure sgcTryFree({$IFDEF D10_4}const [ref] Obj
  : TObject{$ELSE}var Obj{$ENDIF});
begin
  Try
    sgcFree(Obj);
  Except
    // nothing
  End;
end;

function GetHMACSHA256_Indy(const aValue: String; aSecret: TIdBytes;
  aBase64: Boolean = False): String;
{$IFDEF INDY10_5_7}
var
  oHMAC: TIdHMACSHA256;
  oHash: TIdBytes;
  oEncoder: TIdEncoderMIME;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  oHMAC := TIdHMACSHA256.Create;
  try
    oHMAC.Key := aSecret;
    oHash := oHMAC.HashValue({$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.GetBytes(aValue));
    if aBase64 then
    begin
      oEncoder := TIdEncoderMIME.Create;
      Try
        result := oEncoder.EncodeBytes(oHash);
      Finally
        sgcFree(oEncoder);
      End
    end
    else
      result := LowerCase(ToHex(oHash));
  finally
    oHMAC.Free;
  end;
{$ELSE}
    raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

function GetHMACSHA256_Indy_Bytes(const aValue, aSecret: TBytes): TBytes;
{$IFDEF INDY10_5_7}
var
  oHMAC: TIdHMACSHA256;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  oHMAC := TIdHMACSHA256.Create;
  try
    oHMAC.Key := TIdBytes(aSecret);
    result := TBytes(oHMAC.HashValue(TIdBytes(aValue)));
  finally
    oHMAC.Free;
  end;
{$ELSE}
  raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

function GetHMACSHA512_Indy(const aValue: TIdBytes; aSecret: TIdBytes;
  aBase64: Boolean = False): String;
{$IFDEF INDY10_5_7}
var
  oHMAC: TIdHMACSHA512;
  oHash: TIdBytes;
  oEncoder: TIdEncoderMIME;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA512.IsAvailable then
    raise Exception.Create('SHA512 hashing is not available!');
  oHMAC := TIdHMACSHA512.Create;
  try
    oHMAC.Key := aSecret;
    oHash := oHMAC.HashValue(aValue);
    if aBase64 then
    begin
      oEncoder := TIdEncoderMIME.Create;
      Try
        result := oEncoder.EncodeBytes(oHash);
      Finally
        sgcFree(oEncoder);
      End
    end
    else
      result := LowerCase(ToHex(oHash));
  finally
    oHMAC.Free;
  end;
{$ELSE}
  raise Exception.Create('SHA512 hashing is not available!');
{$ENDIF}
end;

function GetHashSHA256_Indy(const aValue: TBytes): TBytes;
{$IFDEF INDY10_5_7}
var
  oHash: TIdHashSHA256;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  oHash := TIdHashSHA256.Create;
  Try
    result := TBytes(oHash.HashBytes(TIdBytes(aValue)));
  Finally
    oHash.Free;
  End;
{$ELSE}
  raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

function GetHashSHA384_Indy(const aValue: TBytes): TBytes;
{$IFDEF INDY10_5_7}
var
  oHash: TIdHashSHA384;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA384.IsAvailable then
    raise Exception.Create('SHA384 hashing is not available!');
  oHash := TIdHashSHA384.Create;
  Try
    result := TBytes(oHash.HashBytes(TIdBytes(aValue)));
  Finally
    oHash.Free;
  End;
{$ELSE}
  raise Exception.Create('SHA384 hashing is not available!');
{$ENDIF}
end;

function GetHashSHA512_Indy(const aValue: TBytes): TBytes;
{$IFDEF INDY10_5_7}
var
  oHash: TIdHashSHA512;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  LoadOpenSSLLibrary;
  if not TIdHashSHA512.IsAvailable then
    raise Exception.Create('SHA512 hashing is not available!');
  oHash := TIdHashSHA512.Create;
  Try
    result := TBytes(oHash.HashBytes(TIdBytes(aValue)));
  Finally
    oHash.Free;
  End;
{$ELSE}
  raise Exception.Create('SHA512 hashing is not available!');
{$ENDIF}
end;

{$IFDEF D10}

function GetHMACSHA256_Hash(const aValue: String; aSecret: TIdBytes;
  aBase64: Boolean = False): String;
var
  oEncoder: TIdEncoderMIME;
  oHash: TBytes;
begin
  oHash := THashSHA2.GetHMACAsBytes(aValue, BytesToString(aSecret));
  if aBase64 then
  begin
    oEncoder := TIdEncoderMIME.Create;
    Try
      result := oEncoder.EncodeBytes(TIdBytes(oHash));
    Finally
      sgcFree(oEncoder);
    End
  end
  else
    result := LowerCase(ToHex(TIdBytes(oHash)));
end;

function GetHMACSHA256_Hash_Bytes(const aValue, aSecret: TBytes): TBytes;
begin
  result := THashSHA2.GetHMACAsBytes(aValue, aSecret);
end;

function GetHMACSHA512_Hash(const aValue: TBytes; aSecret: TBytes;
  aBase64: Boolean = False): String;
var
  oEncoder: TIdEncoderMIME;
  oHash: TBytes;
begin
  oHash := THashSHA2.GetHMACAsBytes(aValue, aSecret,
    THashSHA2.TSHA2Version.SHA512);
  if aBase64 then
  begin
    oEncoder := TIdEncoderMIME.Create;
    Try
      result := oEncoder.EncodeBytes(TIdBytes(oHash));
    Finally
      sgcFree(oEncoder);
    End
  end
  else
    result := LowerCase(ToHex(TIdBytes(oHash)));
end;

function GetHashSHA256_Hash(const aValue: String): TBytes;
begin
  result := THashSHA2.GetHashBytes(aValue, THashSHA2.TSHA2Version.SHA256);
end;

function GetHashSHA384_Hash(const aValue: String): TBytes;
begin
  result := THashSHA2.GetHashBytes(aValue, THashSHA2.TSHA2Version.SHA384);
end;

function GetHashSHA512_Hash(const aValue: String): TBytes;
begin
  result := THashSHA2.GetHashBytes(aValue, THashSHA2.TSHA2Version.SHA512);
end;

{$ENDIF}

function GetHashSHA256(const aValue: String): TBytes;
{$IFDEF INDY10_5_7}
var
  oBytes: TBytes;
{$ENDIF}
begin
{$IFDEF INDY10_5_7}
  Try
    oBytes := TBytes({$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.GetBytes(aValue));
    result := GetHashSHA256_Indy(oBytes);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHashSHA256_Hash(aValue);
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
{$ELSE}
  raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

function GetHashSHA256(const aValue: TBytes): TBytes;
begin
  Try
    result := GetHashSHA256_Indy(aValue);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHashSHA256_Hash(TEncoding.Ansi.GetString(aValue));
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

function GetHashSHA384(const aValue: TBytes): TBytes;
begin
  Try
    result := GetHashSHA384_Indy(aValue);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHashSHA384_Hash(TEncoding.Ansi.GetString(aValue));
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

function GetHashSHA512(const aValue: TBytes): TBytes;
begin
  Try
    result := GetHashSHA512_Indy(aValue);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHashSHA512_Hash(TEncoding.Ansi.GetString(aValue));
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

function GetHMACSHA256(const aValue: String; aSecret: TIdBytes;
  aBase64: Boolean = False): String;
begin
  Try
    result := GetHMACSHA256_Indy(aValue, aSecret, aBase64);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHMACSHA256_Hash(aValue, aSecret, aBase64);
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

function GetHMACSHA256(const aValue, aSecret: String;
  aBase64: Boolean = False): String;
begin
{$IFDEF INDY10_5_7}
  result := GetHMACSHA256(aValue,
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}.
    GetBytes(aSecret), aBase64);
{$ELSE}
    raise Exception.Create('SHA256 hashing is not available!');
{$ENDIF}
end;

function GetDateTimeUnix(aDateTime: Extended; aUTC: Boolean): String;
{$IFNDEF DXE6}
{$IFNDEF LAZARUS}
{$IFDEF MSWINDOWS}
var
  ST: SystemTime;
  DT: TDateTime;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
{$IFDEF LAZARUS}
  result := IntToStr(DateTimeToUnix(aDateTime));
{$ELSE}
{$IFDEF DXE6}
  result := IntToStr(DateTimeToUnix(aDateTime, aUTC));
{$ELSE}
{$IFDEF MSWINDOWS}
  Windows.GetSystemTime(ST);
  DT := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime(ST.wHour,
    ST.wMinute, ST.wSecond, ST.wMilliseconds);
  result := IntToStr(SecondsBetween(DT, UnixDateDelta));
{$ELSE}
  result := IntToStr(DateTimeToUnix(aDateTime));
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function URIEncode(const AText: String): String;
var
  i: Integer;
  c: Char;
  vInArray: Boolean;
begin
  result := '';
  for i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF} to Length
    (AText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
  begin
    vInArray := False;
    c := AText[i];
    case c of
      'A' .. 'Z':
        vInArray := True;
      'a' .. 'z':
        vInArray := True;
      '0':
        vInArray := True;
      '1' .. '9':
        vInArray := True;
      '-':
        vInArray := True;
      '_':
        vInArray := True;
      '~':
        vInArray := True;
      '.':
        vInArray := True;
    end;
    if not vInArray then
      result := result + '%' + IntToHex(Ord(AText[i]), 2)
    else
      result := result + AText[i];
  end;
end;

function sgcStringReplace(const AText, aFromText, aToText: String): String;
begin
  result := {$IFDEF D7}AnsiReplaceStr{$ELSE}{$IFDEF LAZARUS}StringReplace{$ELSE}ReplaceStr{$ENDIF}{$ENDIF}(AText, aFromText, aToText{$IFDEF LAZARUS}, [rfReplaceAll]{$ENDIF});
end;

function NewGuid: String;
var
  oGuid: TGuid;
begin
  CreateGuid(oGuid);
  result := GuidToString(oGuid);
  result := sgcStringReplace(result, '{', '');
  result := sgcStringReplace(result, '}', '');
  result := sgcStringReplace(result, '-', '');
end;

function sgcGetUTCString(aDate: TDateTime = 0): String;
var
  vDate: TDateTime;
begin
  vDate := aDate;
  if vDate = 0 then
    vDate := Now;
  vDate := vDate - {$IFNDEF INDY10_2}TIdSysWin32.{$ENDIF}OffsetFromUTC;
  result := FormatDateTime('yyyymmdd', vDate) + 'T' + FormatDateTime('hhnnss',
    vDate) + 'Z';
end;

function sgcGetNodeValue(const aXML, aNode: String): String;
var
  i, j: Integer;
begin
  result := '';

  i := AnsiPos('<' + aNode + '>', aXML);
  if i > 0 then
  begin
    j := AnsiPos('</' + aNode + '>', aXML);
    if j > i then
      result := MidStr(aXML, i + Length(aNode) + 2, j - i - Length(aNode) - 2);
  end;
end;

procedure sgcGetNodeList(const aXML, aNode: String; var Nodes: TStringList);
var
  vXML: String;
  vNode: string;
begin
  vXML := aXML;
  repeat
    vNode := sgcGetNodeValue(vXML, aNode);
    if vNode <> '' then
    begin
      Nodes.Add(vNode);
      vXML := MidStr(vXML, AnsiPos('</' + aNode + '>', vXML) + Length(aNode),
        Length(vXML));
    end;
  until vNode = '';
end;

function sgcGetTicks: Cardinal;
begin
  result := {$IFDEF NEXTGEN}TThread.GetTickCount{$ELSE}{$IFDEF INDY10_6_2_5263}Ticks64{$ELSE}Ticks{$ENDIF}{$ENDIF};
end;

function sgcGetTickDiff(const AOldTickCount, ANewTickCount: Cardinal): Cardinal;
begin
{$IFDEF INDY10_6_2_5263}
  result := GetTickDiff64(AOldTickCount, ANewTickCount);
{$ELSE}
  result := GetTickDiff(AOldTickCount, ANewTickCount);
{$ENDIF}
end;

function EncodeBase64(const AText: String;
  const aUTF8: Boolean = False): String;
var
  oEncoder: TIdEncoderMIME;
  oStream: TsgcStringStream;
begin
  oEncoder := TIdEncoderMIME.Create;
  Try
    if aUTF8 then
    begin
{$IFDEF D2009}
      oStream := TsgcStringStream.Create(AText, TEncoding.UTF8);
{$ELSE}
      oStream := TsgcStringStream.Create(UTF8Encode(AText));
{$ENDIF}
    end
    else
      oStream := TsgcStringStream.Create(AText);
    Try
      result := oEncoder.Encode(oStream);
    Finally
      sgcFree(oStream);
    End;
  Finally
    sgcFree(oEncoder);
  End;
end;

function EncodeBase64(const aStream: TMemoryStream): String;
var
  oEncoder: TIdEncoderMIME;
begin
  oEncoder := TIdEncoderMIME.Create;
  Try
    aStream.Position := 0;
    result := oEncoder.Encode(aStream);
  Finally
    sgcFree(oEncoder);
  End;
end;

function DecodeBase64(const AText: String): String;
var
  oDecoder: TIdDecoderMIME;
begin
  oDecoder := TIdDecoderMIME.Create;
  Try
    result := oDecoder.DecodeString(AText);
  Finally
    sgcFree(oDecoder);
  End;
end;

procedure DecodeBase64(const AText: String; var aStream: TMemoryStream);
var
  oDecoder: TIdDecoderMIME;
begin
  oDecoder := TIdDecoderMIME.Create;
  Try
    oDecoder.DecodeBegin(aStream);
    Try
      oDecoder.Decode(AText);
    Finally
      oDecoder.DecodeEnd
    End;
  Finally
    sgcFree(oDecoder);
  End;
end;

function GetHMACSHA512(const aValue, aSecret: TBytes;
  aBase64: Boolean = False): String;
begin
  Try
    result := GetHMACSHA512_Indy(TIdBytes(aValue), TIdBytes(aSecret), aBase64);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHMACSHA512_Hash(aValue, aSecret, aBase64);
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

function GetMD5(AText: String): String;
var
  oMD5: TIdHashMessageDigest5;
begin
  oMD5 := TIdHashMessageDigest5.Create;
  try
{$IFDEF INDY10_2}
    result := oMD5.HashBytesAsHex(HexToBytes(AText));
{$ELSE}
    result := oMD5.AsHex(oMD5.HashValue(HexToString(AText)));
{$ENDIF}
  finally
    sgcFree(oMD5);
  end;
end;

function sgcDecodeJSON(const aJSON: string): string;
var
  i: Integer;
  j: Integer;
  c: Integer;
begin
  result := '';
  SetLength(result, Length(aJSON));
  i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF};
  j := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF};
  while i <= Length(aJSON) do
  begin
    if aJSON[i] = '\' then
    begin
      if i < Length(aJSON) then
      begin
        if aJSON[i + 1] = '\' then
        begin
          result[j] := '\';
          inc(i, 2);
        end
        else if aJSON[i + 1] = '"' then
        begin
          result[j] := '"';
          inc(i, 2);
        end
        else if UpperCase(String(aJSON[i + 1])) = 'R' then
        begin
          result[j] := Chr(13);
          inc(i, 2);
        end
        else if UpperCase(String(aJSON[i + 1])) = 'N' then
        begin
          result[j] := Chr(10);
          inc(i, 2);
        end
        else if UpperCase(String(aJSON[i + 1])) = 'T' then
        begin
          result[j] := Chr(9);
          inc(i, 2);
        end
        else if (aJSON[i + 1] = 'u') and (i + 1 + 4 <= Length(aJSON)) and
          TryStrToInt('$' + string(Copy(aJSON, i +
{$IFDEF SGC_ZEROBASEDSTRINGS}3{$ELSE}2{$ENDIF}, 4)), c) then
        begin
          inc(i, 6);
          result[j] := {$IFDEF D2009}WideChar{$ELSE}Char{$ENDIF}(c);
        end
        else
          raise Exception.CreateFmt('Invalid code at position %d', [i]);
      end
      else
        raise Exception.Create('Unexpected end of string');
    end
    else
    begin
      result[j] := {$IFDEF D2009}WideChar{$ELSE}Char{$ENDIF}(aJSON[i]);
      inc(i);
    end;
    inc(j);
  end;

  if j > 1 then
    SetLength(result, j - 1);
end;

function sgcEncodeJSON(const AText: String): String;
const
  cs_escape = '\';
  cs_quotation_mark = '"';
  cs_back_slash = '\';
  cs_slash = '/';
  cs_backspace = #8;
  cs_form_feed = #12;
  cs_new_line = #10;
  cs_carriage_return = #13;
  cs_horizontal_tab = #9;
var
  vChar: Char;
{$IFDEF D7}
  i: Integer;
{$ENDIF}
begin
  result := '';
{$IFDEF D7}
  for i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF} to Length
    (AText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
  begin
    vChar := AText[i];
{$ELSE}
  for vChar in AText do
  begin
{$ENDIF}
    case vChar of
      cs_quotation_mark:
        result := result + cs_escape + cs_quotation_mark;
      cs_back_slash:
        result := result + cs_escape + cs_back_slash;
      cs_slash:
        result := result + cs_escape + cs_slash;
      cs_backspace:
        result := result + cs_escape + 'b';
      cs_form_feed:
        result := result + cs_escape + 'f';
      cs_new_line:
        result := result + cs_escape + 'n';
      cs_carriage_return:
        result := result + cs_escape + 'r';
      cs_horizontal_tab:
        result := result + cs_escape + 't';
    else
      begin
        if (Integer(vChar) < 32) or (Integer(vChar) > 126) then
          result := result + cs_escape + 'u' + IntToHex(Integer(vChar), 4)
        else
          result := result + vChar;
      end;
    end;
  end;
end;

procedure sgcSetThreadName(const aName: string);
begin
{$IFDEF MSWINDOWS}
{$IFDEF D2010}
  SetThreadName(aName);
{$ENDIF}
{$ENDIF}
end;

function sgcGetUTF8StringFromBytes(const ABytes: TBytes): String;
begin
{$IFDEF DXE}
  result := TEncoding.UTF8.GetString(ABytes);
{$ELSE}
  SetString(result, PAnsiChar(@ABytes[0]), Length(ABytes));
  result := UTF8Decode(result);
{$ENDIF}
end;

function sgcGetBytesFromUTF8String(const aValue: string): TBytes;
{$IFNDEF D2009}
var
  vValue: string;
{$ENDIF}
begin
{$IFDEF D2009}
  result := TEncoding.UTF8.GetBytes(aValue);
{$ELSE}
  vValue := UTF8Encode(aValue);
  SetLength(result, Length(vValue));
  if Length(result) > 0 then
    sgcMove(vValue[1], result[0], Length(vValue));
{$ENDIF}
end;

constructor TsgcBytesStream.Create(const ABytes:
{$IFDEF D7}TIdBytes{$ELSE}TBytes{$ENDIF});
begin
  inherited Create;
  if Length(ABytes) > 0 then
  begin
    WriteBuffer(ABytes[0], Length(ABytes));
    Position := 0;
  end;
end;

function TsgcBytesStream.GetBytes: TBytes;
begin
  SetLength(result, Size);
  Read(result, Size);
end;

constructor TsgcSearchList.Create;
begin
  inherited;
{$IFDEF DXE2}
  FList := TDictionary<string, string>.Create;
{$ELSE}
  FList := TStringList.Create;
  FList.Duplicates := dupIgnore;
  FList.Sorted := True;
{$ENDIF}
end;

destructor TsgcSearchList.Destroy;
begin
  sgcFree(FList);
  inherited;
end;

procedure TsgcSearchList.Add(const aKey, aValue: String);
{$IFNDEF DXE2}
var
  i: Integer;
  vValue: string;
{$ENDIF}
begin
{$IFDEF DXE2}
  if FList.ContainsKey(aKey) then
    FList.Remove(aKey);
  FList.Add(aKey, aValue);
{$ELSE}
  i := FList.IndexOfName(aKey);
  if i > -1 then
    FList.Delete(i);
  vValue := '';
  if FList.Count > 0 then
    vValue := ',';
  vValue := vValue + aKey + '=' + aValue;
  FList.CommaText := FList.CommaText + vValue;
{$ENDIF}
end;

procedure TsgcSearchList.Delete(const aKey: String);
{$IFNDEF DXE2}
var
  i: Integer;
{$ENDIF}
begin
{$IFDEF DXE2}
  FList.Remove(aKey);
{$ELSE}
  i := FList.IndexOfName(aKey);
  if i > -1 then
    FList.Delete(i);
{$ENDIF}
end;

function TsgcSearchList.Get(const aKey: String): string;
begin
{$IFDEF DXE2}
  FList.TryGetValue(aKey, result);
{$ELSE}
  result := FList.Values[aKey];
{$ENDIF}
end;

function sgcBytesToStringRaw(const ABytes: TBytes): String;
begin
{$IFDEF INDY10_5_7}
  result := BytesToStringRaw(TIdBytes(ABytes));
{$ELSE}
  if Length(ABytes) > 0 then
  begin
{$IFDEF D2010}
    result := Indy8BitEncoding.GetString(ABytes, 0, -1);
{$ELSE}
    SetString(result, PAnsiChar(@ABytes[0]), Length(ABytes));
{$ENDIF}
  end
  else
    result := '';
{$ENDIF}
end;

procedure sgcMove(const Source; var Dest; Count: NativeInt);
begin
  // ... avoid Range Check error
  if Count > 0 then
    Move(Source, Dest, Count);
end;

procedure DecodeBase64(const AText: String; var ABytes: TBytes); overload;
var
  oDecoder: TIdDecoderMIME;
begin
  oDecoder := TIdDecoderMIME.Create;
  Try
{$IFDEF INDY10_5_5}
    ABytes := TBytes(oDecoder.DecodeBytes(AText));
{$ELSE}
    ABytes := sgcGetBytesFromUTF8String(oDecoder.DecodeString(AText));
{$ENDIF}
  Finally
    sgcFree(oDecoder);
  End;
end;

function sgcGetOpenSSLDefaultFolder: string;
begin
{$IFDEF MSWINDOWS}
  result := ExtractFilePath(ParamStr(0));
{$ELSE}
{$IFDEF MACOS}
  // ... .NET library
  if IsLibrary then
    result := '../MacOS'
  else
    result := TPath.GetDirectoryName(ParamStr(0));
{$ELSE}
{$IFDEF ANDROID}
  result := TPath.GetDocumentsPath;
{$ELSE}
  result := '';
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure sgcIdOpenSSLSetLibPath(const APath: String);
begin
{$IFNDEF IOS}
{$IFDEF MACOS}
{$IFDEF DXE3}
  IdOpenSSLSetLibPath(APath);
{$ENDIF}
{$ELSE}
{$IFDEF INDY10_6}
  IdOpenSSLSetLibPath(APath);
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function HexToBytes(AText: String): TIdBytes;
var
  i: Integer;
begin
  SetLength(result, Length(AText) div 2);
  for i := 1 to Length(AText) div 2 do
    result[i - 1] := StrToInt('$' + Copy(AText, (i - 1) * 2 + 1, 2));
end;

function HexToString(const AText: String): String;
var
  i: Integer;
begin
  result := '';
  for i := 1 to Length(AText) div 2 do
    result := result + Char(StrToInt('$' + Copy(AText, (i - 1) * 2 + 1, 2)));
end;

function GetHMACSHA1(const aValue, aSecret: TBytes): TBytes;
{$IFDEF INDY10_2}
var
  oHMAC: TIdHMACSHA1;
{$ENDIF}
begin
{$IFDEF INDY10_2}
  oHMAC := TIdHMACSHA1.Create;
  Try
    oHMAC.Key := TIdBytes(aSecret);
    result := TBytes(oHMAC.HashValue(TIdBytes(aValue)));
  Finally
    sgcFree(oHMAC);
  End;
{$ELSE}
  raise Exception.Create('Requires Indy Version 10.2 or later');
{$ENDIF}
end;

function GetMD5(const ABytes: TBytes): TBytes;
{$IFDEF INDY10_2}
var
  oMD5: TIdHashMessageDigest5;
{$ENDIF}
begin
{$IFDEF INDY10_2}
  oMD5 := TIdHashMessageDigest5.Create;
  try
    result := TBytes(oMD5.HashBytes(TIdBytes(ABytes)));
  finally
    sgcFree(oMD5);
  end;
{$ELSE}
  raise Exception.Create('Requires Indy Version 10.2 or later');
{$ENDIF}
end;

function GetHMACSHA256(const aValue, aSecret: TBytes): TBytes;
begin
  Try
    result := GetHMACSHA256_Indy_Bytes(aValue, aSecret);
  Except
    On E: Exception do
    begin
{$IFDEF D10}
      result := GetHMACSHA256_Hash_Bytes(aValue, aSecret);
{$ELSE}
      raise;
{$ENDIF}
    end;
  End;
end;

function sgcQuotedStr(const aValue: string): string;
begin
  result := Trim(aValue);
  if LeftStr(aValue, 1) <> '"' then
    result := '"' + result;
  if RightStr(aValue, 1) <> '"' then
    result := result + '"';
end;

function sgcContainsText(const AText, aSubText: String): Boolean;
begin
{$IFDEF D2006}
  result := ContainsText(AText, aSubText);
{$ELSE}
  result := AnsiContainsText(AText, aSubText);
{$ENDIF}
end;

function sgcStringToHex(const aValue: string): string;
var
  i: Integer;
begin
  result := '';
  for i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF} to Length
    (aValue){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
    result := result + IntToHex(Ord(aValue[i]), 2);
end;

function sgcGetInt8FromBytes(const ABytes: TBytes; aOffset: Integer = 0)
  : ShortInt;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Int8));
  result := PShortInt(@vBytes[0])^;
end;

function sgcGetUInt8FromBytes(const ABytes: TBytes; aOffset: Integer = 0): Byte;
begin
  result := PByte(@ABytes[aOffset])^;
end;

function sgcReadByte(const ABytes: TBytes; aOffset: Integer = 0): Byte;
begin
  result := ABytes[aOffset];
end;

function sgcGetInt16FromBytes(const ABytes: TBytes; aOffset: Integer = 0)
  : SmallInt;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(SmallInt));
  result := PSmallInt(@vBytes[0])^;
end;

function sgcGetUInt16FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Word;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Word));
  result := PWord(@vBytes[0])^;
end;

function sgcGetInt32FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Int32;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Int32));
  result := PInt32(@vBytes[0])^;
end;

function sgcGetUInt32FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): UInt32;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(UInt32));
  result := PUInt32(@vBytes[0])^;
end;

function sgcGetInt64FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Int64;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Int64));
  result := PInt64(@vBytes[0])^;
end;

function sgcGetUInt64FromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): UInt64;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Int64));
  result := PUInt64(@vBytes[0])^;
end;

function sgcGetSingleFromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Single;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Single));
  result := PSingle(@vBytes[0])^;
end;

procedure sgcReverseBytes(var aValue: TBytes);
var
  i: Integer;
  Tmp: Byte;
  iMax: Integer;
begin
  iMax := High(aValue);
  for i := 0 to iMax div 2 do
  begin
    Tmp := aValue[i];
    aValue[i] := aValue[iMax - i];
    aValue[iMax - i] := Tmp;
  end;
end;

function sgcReadBytes(const ABytes: TBytes; aOffset: Integer = 0;
  aLength: Integer = 0): TBytes;
begin
  if aLength = 0 then
    aLength := Length(ABytes);
  SetLength(result, aLength);
  sgcMove(ABytes[aOffset], result[0], aLength);
end;

function sgcReadReverseBytes(const ABytes: TBytes; aOffset: Integer = 0;
  aLength: Integer = 0): TBytes;
begin
  result := sgcReadBytes(ABytes, aOffset, aLength);
  sgcReverseBytes(result);
end;

function sgcGetDoubleFromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Double;
var
  vBytes: TBytes;
begin
  vBytes := sgcReadReverseBytes(ABytes, aOffset, SizeOf(Double));
  result := PDouble(@vBytes[0])^;
end;

function sgcGetDecimalFromBytes(const ABytes: TBytes;
  aOffset: Integer = 0): Double;
var
  vBytes: TBytes;
  vPrecision: Byte;
  vInt32: UInt32;
begin
  vPrecision := ABytes[aOffset];
  SetLength(vBytes, 4);
  sgcMove(vBytes[0], vInt32, SizeOf(UInt32));
  result := vInt32 / Power(10, vPrecision);
end;

procedure sgcWriteBytes(const aSource: TBytes; var aDest: TBytes);
var
  i: Integer;
begin
  if Assigned(aSource) then
  begin
    i := Length(aDest);
    SetLength(aDest, Length(aDest) + Length(aSource));
    sgcMove(aSource[0], aDest[i], Length(aSource));
  end;
end;

procedure sgcWriteByte(const aValue: Byte; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + 1);
  aDest[i] := aValue;
end;

procedure sgcWriteUInt32(const aValue: UInt32; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PUInt32(@aDest[i])^ := aValue;
end;

procedure sgcWriteInt32(const aValue: Int32; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PInt32(@aDest[i])^ := aValue;
end;

procedure sgcWriteInt8(const aValue: Int8; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PInt8(@aDest[i])^ := aValue;
end;

procedure sgcWriteUInt8(const aValue: UInt8; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PUInt8(@aDest[i])^ := aValue;
end;

procedure sgcWriteInt16(const aValue: Int16; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PInt16(@aDest[i])^ := aValue;
end;

procedure sgcWriteUInt16(const aValue: UInt16; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PUInt16(@aDest[i])^ := aValue;
end;

procedure sgcWriteSingle(const aValue: Single; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PSingle(@aDest[i])^ := aValue;
end;

procedure sgcWriteDouble(const aValue: Double; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PDouble(@aDest[i])^ := aValue;
end;

procedure sgcWriteDecimal(const aValue: Double; var aDest: TBytes);
var
  vValue: UInt32;
  n: Double;
  i: Integer;
  j: Byte;
begin;
  j := 2; // precision
  n := 1 / Power(10, 2 + j);
  vValue := Trunc((aValue + n) * Power(10, j));
  i := Length(aDest);
  SetLength(aDest, i + 1);
  aDest[i] := j;
  sgcWriteUInt32(vValue, aDest);
end;

procedure sgcWriteInt64(const aValue: Int64; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PInt64(@aDest[i])^ := aValue;
end;

procedure sgcWriteUInt64(const aValue: UInt64; var aDest: TBytes);
var
  i: Integer;
begin
  i := Length(aDest);
  SetLength(aDest, i + SizeOf(aValue));
  PUInt64(@aDest[i])^ := aValue;
end;

{$IFNDEF D2009}

constructor TsgcStringStream.Create(const ABytes: TBytes);
begin
  inherited Create('');
  Write(ABytes, Length(ABytes));
end;

procedure TsgcStringStream.DoBytes;
begin
  SetLength(FBytes, Length(DataString));
  if Length(FBytes) > 0 then
    sgcMove(DataString[1], FBytes[0], Length(DataString));
end;

function TsgcStringStream.GetBytes: TBytes;
begin
  if (DataString <> '') and (Length(FBytes) = 0) then
    DoBytes;
  result := FBytes;
end;

function TsgcStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  result := inherited Write(Buffer, Count);
  DoBytes;
end;

{$ENDIF}

initialization

// -->start sgc_trial
{$IFDEF SGC_TRIAL}
{$IFNDEF NEXTGEN}
{$IFNDEF CONSOLE}
{$IFNDEF MACOS}
{$WARNINGS Off}
if DebugHook = 0 then
begin
  if ((Now > EncodeDate(2022, 3, 28)) and (FormatDateTime('s', Now) = '9')) then
  begin
    raise Exception.Create
      (DecodeBase64
      ('VGhpcyBkZW1vIHZlcnNpb24gY2FuIG9ubHkgcnVuIGZvciBhIGxpbWl0ZWQgdGltZSBwZXJpb2QuIFBsZWFzZSB2aXNpdCB3d3cuZXNlZ2VjZS5jb20gdG8gcHVyY2hhc2UgeW91ciBjb3B5IG9mIHRoZSBsaWJyYXJ5Lg==')
      );
  end;
end;
{$WARNINGS On}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
// <--end sgc_trial

finalization

end.
