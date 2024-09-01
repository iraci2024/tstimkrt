{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressOfficeCore Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxWinInet; // for internal use

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Windows,
{$IFDEF DELPHIXE6}
  JSON,
{$ELSE}
  DBXJSON,
{$ENDIF}
  Classes, WinInet, Generics.Collections,
  dxCore, dxThreading, cxClasses;

{$IFDEF WIN32}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$ENDIF}

type
  TdxWebTaskManager = class;

  TdxJSONValue = TJSONValue;
  TdxJSONObject = TJSONObject;
  TdxJSONPair = TJSONPair;
  TdxJSONArray = TJSONArray;
  TdxJSONString = TJSONString;
  TdxJSONTrue = TJSONTrue;
  TdxJSONFalse = TJSONFalse;
  TdxJSONNull = TJSONNull;

  TdxInternet = HINTERNET;

  TdxFileTransferProgressProc = reference to procedure(const APosition,
    ASize: Int64);

  { EdxHttpError }

  EdxHttpError = class(EdxException)
  public
    constructor Create; overload;
    constructor Create(ACode: Integer; const AText: string); overload;
  end;

  { TdxHttpHelper }

  TdxHttpHelper = class
  public type
    TInterruptOperationFunc = reference to function(const AUri: string)
      : Boolean;
    TSendRequestResultFunc = reference to procedure(ARequest: HINTERNET);
  public const
{$REGION 'public const'}
    HTTP_VERSION_1_1 = 'HTTP/1.1';
    InternetOpenFlags: DWORD = 0;
    InternetOpenProxy: string = '';
    InternetOpenProxyBypass: string = '';
    InternetOpenAccessType: DWORD = INTERNET_OPEN_TYPE_PRECONFIG;
    ContentTypeJSONHeader = 'Content-Type: application/json';
    DefaultOpenRequestFlags = INTERNET_SERVICE_HTTP or INTERNET_FLAG_SECURE or
      INTERNET_FLAG_PASSIVE or INTERNET_FLAG_KEEP_CONNECTION or
      INTERNET_FLAG_DONT_CACHE;
{$ENDREGION}
  strict private
    class function GetInternetOpenUrlFlags(const AUri: string): DWORD; static;
  protected
    class function GetJSONObject(ARequest: HINTERNET): TdxJSONObject;
      overload; static;
    class function SendRequest(const AUserAgent, AServerName, AObjectName,
      AHeader, AVerb: string; const AParams: TdxJSONObject): TdxJSONObject;
      overload; static;
  public
    class procedure CloseConnection(var AConnection: TdxInternet); static;
    class procedure CloseRequest(var ARequest: TdxInternet); static;
    class procedure CloseSession(var ASession: TdxInternet); static;
    class function OpenConnection(ASession: TdxInternet;
      const AServerName: string; out AConnection: TdxInternet): Boolean; static;
    class function OpenRequest(AConnect: HINTERNET;
      const AVerb, AObjectName: string; out ARequest: TdxInternet)
      : Boolean; static;
    class function OpenRequestURL(ASession: HINTERNET; const AURL: string;
      out ARequest: TdxInternet): Boolean; overload; static;
    class function OpenRequestURL(ASession: HINTERNET; const AURL: string;
      AOffset, ASize: Int64; out ARequest: TdxInternet): Boolean;
      overload; static;
    class function OpenRequestURL(ASession: HINTERNET;
      const AURL, AHeaders: string; out ARequest: TdxInternet): Boolean;
      overload; static;
    class function OpenSession(const AUserAgent: string;
      out ASession: TdxInternet): Boolean; static;

    class function ConcatenateHeaders(const AHeaders: array of string)
      : string; static;
    class function SendRequest(const AUserAgent, AServerName, AObjectName,
      AHeader, AVerb: string; const AParams: TBytes): TdxJSONObject;
      overload; static;

    class function JSONObjectToBytes(const AObject: TJSONObject)
      : TBytes; static;

    class function DeleteRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; static;
    class function GetRequest(const AUserAgent, AUri, AHeader: string)
      : TdxJSONObject; overload; static;
    class function GetRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
      overload; static;
    class function GetRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TBytes): TdxJSONObject; overload; static;
    class function GetStream(const AUserAgent, AUri, AHeader: string;
      const AStream: TStream;
      const AInterruptFunc: TInterruptOperationFunc = nil): Boolean; static;
    class function PatchRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; static;
    class function PostRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
      overload; static;
    class function PostRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TBytes): TdxJSONObject; overload; static;
    class function PutRequest(const AUserAgent, AServerName, AObjectName,
      AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; static;
  end;

  { TdxHttpRequest }

  TdxHttpRequest = class
  public
    class function GetContentLength(ARequest: TdxInternet): Int64; static;
    class function GetQueryValueDWORD(ARequest: TdxInternet; ID: DWORD)
      : DWORD; static;
    class function GetQueryValueString(ARequest: TdxInternet; ID: DWORD)
      : string; static;
    class function GetResponseCustomHeader(ARequest: HINTERNET;
      const AHeader: string): string; static;
    class function GetResponseHeaders(ARequest: HINTERNET): TStrings; static;
    class function GetStatusCode(ARequest: TdxInternet): Integer; static;
    class function GetStatusText(ARequest: TdxInternet): string; static;
    class function HasData(ARequest: TdxInternet): Boolean; static;
    class function ReadData(ARequest: TdxInternet; var ABuffer: TBytes;
      out ACount: DWORD): Boolean; static;
  end;

  { TJSONValueHelper }

  TJSONValueHelper = class helper for TJSONValue
  public
    class function CreateBooleanValue(const Value: Boolean)
      : TdxJSONValue; static;
    function Get(const AParamName: string): TdxJSONValue;
    function GetChild(const AParentName, AParamName: string): TdxJSONValue;
    function GetChildParamValue(const AParentName, AParamName: string): string;
    function GetPair(const AParamName: string): TdxJSONPair;
    function GetParamValue(const AParamName: string): string;
    function HasChildParam(const AParentName, AParamName: string): Boolean;
    function HasParam(const AParamName: string): Boolean;
    function IsArray: Boolean;
    function IsTrue: Boolean;
  end;

{$IFNDEF DELPHIXE6}
  { TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
  private
    function GetCount: Integer;
    function GetItem(Index: Integer): TJSONPair;
  public
{$IFNDEF DELPHI2010}
    procedure AddPair(const AName, AValue: UnicodeString); overload;
{$ENDIF}
    function GetValue(const AParamName: string): TdxJSONValue;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJSONPair read GetItem;
  end;

  { TJSONArrayHelper }

  TJSONArrayHelper = class helper for TJSONArray
  private
    function GetCount: Integer;
    function GetItem(Index: Integer): TJSONValue;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJSONValue read GetItem;
  end;

{$ENDIF}
  { TdxWebTask }

  TdxWebTask = class abstract(TInterfacedObject, IdxTask)
  strict private
    FHeader: string;
    FOwner: TdxWebTaskManager;
    FOwnerLink: TcxObjectLink;
    FSyncMode: TdxThreadMethodCallMode;
    FTaskHandle: THandle;
    FUserAgent: string;
  protected
    function GetHeader: string; virtual; abstract;
    function GetUserAgent: string; virtual; abstract;
    procedure UpdateRequestParams; virtual;

    function IsValid: Boolean; virtual;
    procedure DoError(AObject: TObject); virtual; abstract;
    procedure DoComplete; virtual; abstract;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; virtual; abstract;

    procedure Synchronize(AThreadProc: TThreadProcedure);

    property Header: string read FHeader;
    property Owner: TdxWebTaskManager read FOwner;
    property TaskHandle: THandle read FTaskHandle write FTaskHandle;
    property UserAgent: string read FUserAgent;
  public
    constructor Create(AOwner: TdxWebTaskManager);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function IsEqual(const ATask: TdxWebTask): Boolean; virtual;

{$REGION 'IdxTask'}
    function Run(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus;
    procedure OnComplete(AStatus: TdxTaskCompletedStatus);
{$ENDREGION}
  end;

  { TdxWebTaskManager }

  TdxWebTaskManager = class(TPersistent)
  public type
    TForEachProcRef = reference to procedure(ATask: TdxWebTask);
  strict private
    FIsDestroying: Boolean;
    FTasks: TList<TdxWebTask>;
    FUseCurrentThread: Boolean;
    procedure SetCurrentThread(const Value: Boolean);
  protected
    procedure Remove(const ATask: TdxWebTask);
    property IsDestroying: Boolean read FIsDestroying;
    property Tasks: TList<TdxWebTask> read FTasks;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure CancelAllTasks;
    procedure CancelTask(const ATask: TdxWebTask);
    procedure ForEach(AProc: TForEachProcRef);
    function HasSameTask(const ATask: TdxWebTask): Boolean;
    procedure RunTask(const ATask: TdxWebTask); virtual;
    property UseCurrentThread: Boolean read FUseCurrentThread
      write SetCurrentThread;
  end;

procedure dxCheckHttpResult(AResult: Boolean);

implementation

uses
  DateUtils,
  cxDateUtils, dxUriRecord, dxStringHelper;

procedure dxCheckHttpResult(AResult: Boolean);
begin
  if not AResult then
    raise EdxHttpError.Create;
end;

{ EdxHttpError }

constructor EdxHttpError.Create;
var
  ABuffer: array [Byte] of WideChar;
  ABufferLength: DWORD;
  AError: DWORD;
  AErrorMessage: string;
begin
  ABufferLength := Length(ABuffer);
  if InternetGetLastResponseInfoW(AError, @ABuffer[0], ABufferLength) and
    (AError <> 0) then
  begin
    SetString(AErrorMessage, PWideChar(@ABuffer[0]), ABufferLength);
    Create(AError, AErrorMessage);
  end
  else
  begin
    AError := GetLastError;
    if AError <> 0 then
      Create(AError, SysErrorMessage(AError))
    else
      Create(-1, 'Unknown Error');
  end;
end;

constructor EdxHttpError.Create(ACode: Integer; const AText: string);
begin
  CreateFmt('ERROR: %d, %s', [ACode, AText]);
end;

{ TdxHttpHelper }

class function TdxHttpHelper.GetJSONObject(ARequest: HINTERNET): TdxJSONObject;
const
  ABufSize = 1024;
var
  ABuf, ABuffer: TBytes;
  ACount: DWORD;
  AReadByteCount: DWORD;
  AResult: TdxJSONValue;
{$IFNDEF DELPHIXE}
  I: Cardinal;
  AIsString: Boolean;
{$ENDIF}
begin
  Result := nil;

  SetLength(ABuf, ABufSize);
  AReadByteCount := 0;
  repeat
    ACount := 0;
    InternetReadFile(ARequest, @ABuf[0], ABufSize, ACount);
    AReadByteCount := AReadByteCount + ACount;
    if ACount > 0 then
    begin
      SetLength(ABuffer, AReadByteCount);
      Move(ABuf[0], ABuffer[AReadByteCount - ACount], ACount);
    end;
  until (ACount = 0);

{$IFNDEF DELPHIXE}
  AIsString := False;
  for I := AReadByteCount - 1 downto 0 do
  begin
    if ABuffer[I] = Ord('"') then
    begin
      if AIsString then
      begin
        if ABuffer[I - 1] <> Ord('\') then
          AIsString := False;
      end
      else
        AIsString := True;
    end
    else
    begin
      if not AIsString and (ABuffer[I] in [10, 13, 32]) then
      begin
        Move(ABuffer[I + 1], ABuffer[I], AReadByteCount - I);
        Dec(AReadByteCount);
      end;
    end;
    SetLength(ABuffer, AReadByteCount);
  end;
{$ENDIF}
  AResult := TJSONObject.ParseJSONValue(ABuffer, 0, AReadByteCount);
  try
    if AResult is TJSONObject then
      Result := TJSONObject(AResult);
  finally
    if Result <> AResult then
      AResult.Free;
  end;
end;

class function TdxHttpHelper.ConcatenateHeaders(const AHeaders
  : array of string): string;
const
  ASeparator = #$0D + #$0A;
var
  I: Integer;
  AHeader: string;
begin
  Result := '';
  for I := 0 to Length(AHeaders) - 1 do
  begin
    AHeader := Trim(AHeaders[I]);
    if AHeader <> '' then
    begin
      if Result <> '' then
        Result := Result + ASeparator;
      Result := Result + AHeader;
    end;
  end;
end;

class function TdxHttpHelper.DeleteRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'DELETE', AParams);
end;

class function TdxHttpHelper.GetRequest(const AUserAgent, AUri, AHeader: string)
  : TdxJSONObject;
var
  ASession: TdxInternet;
  ARequest: TdxInternet;
begin
  Result := nil;
  if OpenSession(AUserAgent, ASession) then
    try
      if OpenRequestURL(ASession, AUri, AHeader, ARequest) then
        try
          Result := TdxHttpHelper.GetJSONObject(ARequest);
        finally
          CloseRequest(ARequest);
        end;
    finally
      CloseSession(ASession);
    end;
end;

class function TdxHttpHelper.SendRequest(const AUserAgent, AServerName,
  AObjectName, AHeader, AVerb: string; const AParams: TBytes): TdxJSONObject;
var
  AConnect: HINTERNET;
  ARequest: HINTERNET;
  ASession: HINTERNET;
begin
  Result := nil;
  if OpenSession(AUserAgent, ASession) then
    try
      if OpenConnection(ASession, AServerName, AConnect) then
        try
          if OpenRequest(AConnect, AVerb, AObjectName, ARequest) then
            try
              if not HTTPSendRequest(ARequest, PChar(AHeader), Length(AHeader),
                @AParams[0], Length(AParams)) then
                Exit;
              Result := TdxHttpHelper.GetJSONObject(ARequest);
            finally
              CloseRequest(ARequest);
            end;
        finally
          CloseConnection(AConnect);
        end;
    finally
      CloseSession(ASession);
    end;
end;

class function TdxHttpHelper.JSONObjectToBytes(const AObject
  : TJSONObject): TBytes;
begin
  if AObject <> nil then
    Result := TEncoding.UTF8.GetBytes(AObject.ToString)
  else
    Result := nil;
end;

class function TdxHttpHelper.SendRequest(const AUserAgent, AServerName,
  AObjectName, AHeader, AVerb: string; const AParams: TdxJSONObject)
  : TdxJSONObject;
var
  ABytes: TBytes;
begin
  ABytes := JSONObjectToBytes(AParams);
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    AVerb, ABytes);
end;

class function TdxHttpHelper.GetRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'GET', AParams);
end;

class function TdxHttpHelper.GetRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TBytes): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'GET', AParams);
end;

class function TdxHttpHelper.GetStream(const AUserAgent, AUri, AHeader: string;
  const AStream: TStream;
  const AInterruptFunc: TInterruptOperationFunc = nil): Boolean;
const
  BufferSize = 1024;
var
  ABuffer: TBytes;
  ABytesRead: DWORD;
  ARequest: HINTERNET;
  ASession: HINTERNET;
begin
  Result := False;
  if OpenSession(AUserAgent, ASession) then
    try
      if OpenRequestURL(ASession, AUri, AHeader, ARequest) then
        try
          SetLength(ABuffer, BufferSize);
          while TdxHttpRequest.ReadData(ARequest, ABuffer, ABytesRead) do
          begin
            if (ABytesRead = 0) or (GetLastError <> 0) then
              Break;
            if Assigned(AInterruptFunc) and AInterruptFunc(AUri) then
              Exit(False);
            AStream.WriteBuffer(ABuffer, ABytesRead);
            Result := True;
          end;
        finally
          CloseRequest(ARequest);
        end;
    finally
      CloseSession(ASession);
    end;
end;

class function TdxHttpHelper.PostRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'POST', AParams);
end;

class function TdxHttpHelper.PostRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TBytes): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'POST', AParams);
end;

class function TdxHttpHelper.PatchRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'PATCH', AParams);
end;

class function TdxHttpHelper.PutRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader,
    'PUT', AParams);
end;

class function TdxHttpHelper.OpenRequest(AConnect: HINTERNET;
  const AVerb, AObjectName: string; out ARequest: TdxInternet): Boolean;
begin
  ARequest := HttpOpenRequest(AConnect, PChar(AVerb), PChar(AObjectName),
    HTTP_VERSION_1_1, '', nil, DefaultOpenRequestFlags, 0);
  Result := ARequest <> nil;
end;

class function TdxHttpHelper.GetInternetOpenUrlFlags(const AUri: string): DWORD;
var
  AUriRecord: TdxUri;
begin
  Result := INTERNET_FLAG_NEED_FILE or INTERNET_FLAG_KEEP_CONNECTION or
    INTERNET_FLAG_PASSIVE or INTERNET_FLAG_DONT_CACHE;
  AUriRecord := TdxUri.Create(AUri);
  if AUriRecord.IsAbsoluteUri and AUriRecord.IsSecurityUri then
    Result := Result or SECURITY_INTERNET_MASK;
end;

class function TdxHttpHelper.OpenRequestURL(ASession: HINTERNET;
  const AURL: string; out ARequest: TdxInternet): Boolean;
begin
  ARequest := InternetOpenUrl(ASession, PChar(AURL), nil, 0,
    GetInternetOpenUrlFlags(AURL), 0);
  Result := ARequest <> nil;
end;

class function TdxHttpHelper.OpenRequestURL(ASession: HINTERNET;
  const AURL: string; AOffset, ASize: Int64; out ARequest: TdxInternet)
  : Boolean;
var
  AHeaders: string;
begin
  AHeaders := EmptyStr;
  if (AOffset > 0) or (ASize > 0) then
  begin
    AHeaders := 'Range: bytes=' + IntToStr(AOffset) + '-';
    if ASize >= 0 then
      AHeaders := AHeaders + IntToStr(AOffset + ASize - 1);
  end;
  Result := TdxHttpHelper.OpenRequestURL(ASession, AURL, AHeaders, ARequest);
end;

class function TdxHttpHelper.OpenRequestURL(ASession: HINTERNET;
  const AURL, AHeaders: string; out ARequest: TdxInternet): Boolean;
begin
  ARequest := InternetOpenUrl(ASession, PChar(AURL), PChar(AHeaders),
    Length(AHeaders), GetInternetOpenUrlFlags(AURL), 0);
  Result := ARequest <> nil;
end;

class procedure TdxHttpHelper.CloseConnection(var AConnection: TdxInternet);
begin
  InternetCloseHandle(AConnection);
end;

class procedure TdxHttpHelper.CloseRequest(var ARequest: TdxInternet);
begin
  InternetCloseHandle(ARequest);
end;

class procedure TdxHttpHelper.CloseSession(var ASession: TdxInternet);
begin
  InternetCloseHandle(ASession);
end;

class function TdxHttpHelper.OpenConnection(ASession: TdxInternet;
  const AServerName: string; out AConnection: TdxInternet): Boolean;
begin
  AConnection := InternetConnect(ASession, PChar(AServerName),
    INTERNET_DEFAULT_HTTPS_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  Result := AConnection <> nil;
end;

class function TdxHttpHelper.OpenSession(const AUserAgent: string;
  out ASession: TdxInternet): Boolean;
begin
  ASession := InternetOpen(PChar(AUserAgent), InternetOpenAccessType,
    PChar(InternetOpenProxy), PChar(InternetOpenProxyBypass),
    InternetOpenFlags);
  Result := ASession <> nil;
end;

{ TdxHttpRequest }

class function TdxHttpRequest.GetContentLength(ARequest: TdxInternet): Int64;
begin
  Result := GetQueryValueDWORD(ARequest, HTTP_QUERY_CONTENT_LENGTH);
end;

class function TdxHttpRequest.GetQueryValueDWORD(ARequest: TdxInternet;
  ID: DWORD): DWORD;
var
  ABufferLength: DWORD;
  AReserved: DWORD;
begin
  AReserved := 0;
  ABufferLength := SizeOf(Result);
  if not HttpQueryInfo(ARequest, ID or HTTP_QUERY_FLAG_NUMBER, @Result,
    ABufferLength, AReserved) then
    Result := 0;
end;

class function TdxHttpRequest.GetQueryValueString(ARequest: TdxInternet;
  ID: DWORD): string;
var
  ABufferLength: DWORD;
  AReserved: DWORD;
begin
  AReserved := 0;
  ABufferLength := 0;
  HttpQueryInfoW(ARequest, ID, nil, ABufferLength, AReserved);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    AReserved := 0;
    SetLength(Result, ABufferLength div SizeOf(WideChar));
    HttpQueryInfoW(ARequest, ID, @Result[1], ABufferLength, AReserved);
    SetLength(Result, ABufferLength div SizeOf(WideChar));
  end
  else
    Result := EmptyStr;
end;

class function TdxHttpRequest.GetResponseCustomHeader(ARequest: HINTERNET;
  const AHeader: string): string;
begin
  Result := GetQueryValueString(ARequest, HTTP_QUERY_CUSTOM);
end;

class function TdxHttpRequest.GetResponseHeaders(ARequest: HINTERNET): TStrings;
begin
  Result := TStringList.Create;
  Result.Text := GetQueryValueString(ARequest, HTTP_QUERY_RAW_HEADERS_CRLF);
end;

class function TdxHttpRequest.GetStatusCode(ARequest: TdxInternet): Integer;
begin
  Result := GetQueryValueDWORD(ARequest, HTTP_QUERY_STATUS_CODE);
end;

class function TdxHttpRequest.GetStatusText(ARequest: TdxInternet): string;
begin
  Result := GetQueryValueString(ARequest, HTTP_QUERY_STATUS_TEXT);
end;

class function TdxHttpRequest.HasData(ARequest: TdxInternet): Boolean;
var
  ASize: DWORD;
begin
  Result := InternetQueryDataAvailable(ARequest, ASize, 0, 0) and (ASize > 0);
end;

class function TdxHttpRequest.ReadData(ARequest: TdxInternet;
  var ABuffer: TBytes; out ACount: DWORD): Boolean;
begin
  ACount := 0;
  Result := InternetReadFile(ARequest, @ABuffer[0], Length(ABuffer), ACount);
end;

{ TJSONValueHelper }

class function TJSONValueHelper.CreateBooleanValue(const Value: Boolean)
  : TdxJSONValue;
begin
  if Value then
    Result := TJSONTrue.Create
  else
    Result := TJSONFalse.Create;
end;

function TJSONValueHelper.GetPair(const AParamName: string): TdxJSONPair;
{$IFNDEF DELPHIXE}
var
  I: Integer;
{$ENDIF}
begin
  Result := nil;
  if Self is TdxJSONObject then
  begin
{$IFNDEF DELPHIXE}
    for I := 0 to TdxJSONObject(Self).Size - 1 do
      if TdxJSONObject(Self).Get(I).JsonString.Value = AParamName then
      begin
        Result := TdxJSONObject(Self).Get(I);
        Break;
      end;
{$ELSE}
    Result := TdxJSONObject(Self).Get(AParamName);
{$ENDIF}
  end;
end;

function TJSONValueHelper.Get(const AParamName: string): TdxJSONValue;
var
  APair: TJSONPair;
begin
  APair := GetPair(AParamName);
  if APair <> nil then
    Result := APair.JsonValue
  else
    Result := nil;
end;

function TJSONValueHelper.GetChild(const AParentName, AParamName: string)
  : TdxJSONValue;
var
  AValue: TdxJSONValue;
begin
  AValue := Self.Get(AParentName);
  if AValue <> nil then
    Result := AValue.Get(AParamName)
  else
    Result := nil;
end;

function TJSONValueHelper.GetChildParamValue(const AParentName,
  AParamName: string): string;
var
  AValue: TdxJSONValue;
begin
  AValue := Self.GetChild(AParentName, AParamName);
  if AValue <> nil then
    Result := AValue.Value
  else
    Result := '';
end;

function TJSONValueHelper.GetParamValue(const AParamName: string): string;
var
  AObject: TdxJSONValue;
begin
  AObject := Get(AParamName);
  if AObject <> nil then
    Result := AObject.Value
  else
    Result := '';
end;

function TJSONValueHelper.HasParam(const AParamName: string): Boolean;
begin
  Result := Get(AParamName) <> nil;
end;

function TJSONValueHelper.IsArray: Boolean;
begin
  Result := Self is TdxJSONArray;
end;

function TJSONValueHelper.IsTrue: Boolean;
begin
  Result := Self is TdxJSONTrue;
end;

function TJSONValueHelper.HasChildParam(const AParentName,
  AParamName: string): Boolean;
var
  AObject: TdxJSONValue;
begin
  AObject := Get(AParentName);
  Result := (AObject <> nil) and (AObject.HasParam(AParamName));
end;

{$IFNDEF DELPHIXE6}
{ TJSONObjectHelper }

{$IFNDEF DELPHI2010}

procedure TJSONObjectHelper.AddPair(const AName, AValue: UnicodeString);
begin
  AddPair(TJSONPair.Create(AName, AValue));
end;
{$ENDIF}

function TJSONObjectHelper.GetValue(const AParamName: string): TdxJSONValue;
begin
  Result := TJSONValue(Self).Get(AParamName);
end;

function TJSONObjectHelper.GetCount: Integer;
begin
  Result := Size;
end;

function TJSONObjectHelper.GetItem(Index: Integer): TJSONPair;
begin
  Result := Get(Index);
end;

{ TJSONArrayHelper }

function TJSONArrayHelper.GetCount: Integer;
begin
  Result := Size;
end;

function TJSONArrayHelper.GetItem(Index: Integer): TJSONValue;
begin
  Result := Get(Index);
end;
{$ENDIF}
{ TdxWebTask }

constructor TdxWebTask.Create(AOwner: TdxWebTaskManager);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLink := cxAddObjectLink(Owner);

  if dxIsMainThread then
    FSyncMode := tmcmSync
  else
    FSyncMode := tmcmAsync;
end;

destructor TdxWebTask.Destroy;
begin
  cxRemoveObjectLink(FOwnerLink);
  inherited Destroy;
end;

procedure TdxWebTask.BeforeDestruction;
begin
  if FOwnerLink.Ref <> nil then
    Owner.Remove(Self);
  inherited BeforeDestruction;
end;

function TdxWebTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := ClassType = ATask.ClassType;
end;

procedure TdxWebTask.Synchronize(AThreadProc: TThreadProcedure);
begin
  dxCallThreadProcedure(AThreadProc, FSyncMode);
end;

procedure TdxWebTask.OnComplete(AStatus: TdxTaskCompletedStatus);
begin
  if AStatus = TdxTaskCompletedStatus.Success then
    DoComplete;
end;

function TdxWebTask.Run(const ACancelStatus: TdxTaskCancelCallback)
  : TdxTaskCompletedStatus;
begin
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);
  try
    Result := DoRun(ACancelStatus);
  except
    on E: Exception do
    begin
      Result := TdxTaskCompletedStatus.Fail;
      DoError(E);
    end;
  end;
end;

procedure TdxWebTask.UpdateRequestParams;
begin
  FUserAgent := GetUserAgent;
  FHeader := GetHeader;
end;

function TdxWebTask.IsValid: Boolean;
begin
  Result := FOwnerLink.Ref <> nil;
end;

{ TdxWebTaskManager }

constructor TdxWebTaskManager.Create;
begin
  inherited Create;
  FTasks := TList<TdxWebTask>.Create;
  FUseCurrentThread := False;
end;

destructor TdxWebTaskManager.Destroy;
begin
  FreeAndNil(FTasks);
  inherited Destroy;
end;

procedure TdxWebTaskManager.BeforeDestruction;
begin
  FIsDestroying := True;
  inherited BeforeDestruction;
  cxClearObjectLinks(Self);
  CancelAllTasks;
end;

procedure TdxWebTaskManager.RunTask(const ATask: TdxWebTask);
begin
  FTasks.Add(ATask);
  if UseCurrentThread then
    ATask.TaskHandle := dxTasksDispatcher.RunInCurrentThread(ATask)
  else
    ATask.TaskHandle := dxTasksDispatcher.Run(ATask);
end;

procedure TdxWebTaskManager.SetCurrentThread(const Value: Boolean);
begin
  if UseCurrentThread <> Value then
  begin
    CancelAllTasks;
    FUseCurrentThread := Value;
  end;
end;

procedure TdxWebTaskManager.CancelTask(const ATask: TdxWebTask);
begin
  FTasks.Remove(ATask);
  dxTasksDispatcher.Cancel(ATask.TaskHandle)
end;

procedure TdxWebTaskManager.ForEach(AProc: TForEachProcRef);
var
  ATask: TdxWebTask;
begin
  for ATask in Tasks do
    AProc(ATask);
end;

function TdxWebTaskManager.HasSameTask(const ATask: TdxWebTask): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Tasks.Count - 1 downto 0 do
  begin
    Result := Tasks[I].IsEqual(ATask);
    if Result then
      Exit;
  end;
end;

procedure TdxWebTaskManager.CancelAllTasks;
begin
  while FTasks.Count > 0 do
    CancelTask(FTasks.Last);
end;

procedure TdxWebTaskManager.Remove(const ATask: TdxWebTask);
begin
  FTasks.Remove(ATask)
end;

end.
