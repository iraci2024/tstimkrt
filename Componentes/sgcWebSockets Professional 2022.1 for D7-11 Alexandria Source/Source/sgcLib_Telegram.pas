{ ***************************************************************************
  sgcLib Telegram component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcLib_Telegram;

interface

{$I sgcVer.inc}

{$IFDEF SGC_TELEGRAM}
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LAZARUS}
  {$IFDEF LINUX}
  Dynlibs,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Classes;

{$IFDEF NEXTGEN}
Type
  PAnsiChar = MarshaledAString;
{$ENDIF}

{$IFDEF IOS64}
function td_json_client_create(): Pointer; cdecl; external 'libtdjson.a' name 'td_json_client_create' dependency 'c++','z';
procedure td_json_client_send(Client: Pointer; Request: PAnsiChar); cdecl; external 'libtdjson.a' name 'td_json_client_send' dependency 'c++','z';
function td_json_client_receive(Client: Pointer; Timeout: Double): PAnsiChar; cdecl; external 'libtdjson.a' name 'td_json_client_receive' dependency 'c++','z';
function td_json_client_execute(Client: Pointer; Request: PAnsiChar): PAnsiChar; cdecl; external 'libtdjson.a' name 'td_json_client_execute' dependency 'c++','z';
procedure td_json_client_destroy(Client: Pointer); cdecl; external 'libtdjson.a' name 'td_json_client_destroy' dependency 'c++','z';
{$ELSE}
Type
  Tsgctd_json_client_create = function(): Pointer; cdecl;
  function td_json_client_create: Pointer; cdecl;

Type
  Tsgctd_json_client_send = procedure(Client: Pointer; Request: PAnsiChar); cdecl;
  procedure td_json_client_send(Client: Pointer; Request: PAnsiChar); cdecl;

Type
  Tsgctd_json_client_receive = function(Client: Pointer; Timeout: Double): PAnsiChar; cdecl;
  function td_json_client_receive(Client: Pointer; Timeout: Double): PAnsiChar; cdecl;

Type
  Tsgctd_json_client_execute = function(Client: Pointer; Request: PAnsiChar): PAnsiChar; cdecl;
  function td_json_client_execute(Client: Pointer; Request: PAnsiChar): PAnsiChar; cdecl;

Type
  Tsgctd_json_client_destroy = procedure(Client: Pointer); cdecl;
  procedure td_json_client_destroy(Client: Pointer); cdecl;

Type
  Tsgctd_fatal_error_event = procedure(Error_Message: PAnsiChar) of object;
  Tsgctd_set_log_fatal_error_callback = procedure(OnFatalErrorCallback: Tsgctd_fatal_error_event); cdecl;
  procedure td_set_log_fatal_error_callback(OnFatalErrorCallback: Tsgctd_fatal_error_event); cdecl;

  procedure SetTDJsonPath(const aPath: String);

{$ENDIF}

{$ENDIF}

implementation

{$IFDEF SGC_TELEGRAM}
{$IFNDEF IOS64}
const
  {$IFDEF MSWINDOWS}
  tdjson = 'tdjson.dll';
  {$ENDIF}
  {$IFDEF OSX64}
  tdjson = 'libtdjson.dylib';
  {$ENDIF}
  {$IFDEF LINUX64}
  tdjson = 'libtdjson.so';
  {$ENDIF}
  {$IFDEF LINUX}
  {$IFDEF LAZARUS}
  tdjson = 'libtdjson.so';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID64}
  tdjson = 'libtdjsonandroid.so';
  {$ENDIF}

var
  tdjsonPath: string = '';

procedure SetTDJsonPath(const aPath: String);
begin
  if aPath <> '' then
    tdjsonPath := SysUtils.IncludeTrailingPathDelimiter(aPath)
  else
    tdjsonPath := '';
end;

var
  otd_json_client_create: Tsgctd_json_client_create;
  otd_json_client_send: Tsgctd_json_client_send;
  otd_json_client_receive: Tsgctd_json_client_receive;
  otd_json_client_execute: Tsgctd_json_client_execute;
  otd_json_client_destroy: Tsgctd_json_client_destroy;
  {$IFNDEF ANDROID64}
  otd_set_log_fatal_error_callback: Tsgctd_set_log_fatal_error_callback;
  {$ENDIF}

var
  vInitialized: Boolean = False;
  vModule: HModule = 0;

procedure DoInitialize();
{$IFDEF MSWINDOWS}
var
  oBuffer: array[0..255] of Char;
{$ENDIF}
begin
  if not vInitialized then
  begin
    {$IFDEF IOS64}
    vModule := dlopen(MarshaledAString(tdjsonPath + tdjson), RTLD_LAZY);
    {$ELSE}
    vModule := LoadLibrary(PChar(tdjsonPath + tdjson));
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    if (vModule <= HINSTANCE_ERROR) then
    begin
      ZeroMemory(@oBuffer, SizeOf(oBuffer));
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError(), 0, oBuffer, SizeOf(oBuffer), nil);
      raise Exception.Create(oBuffer);
    end;
    {$ELSE}
    if vModule = 0 then
      raise Exception.Create('Library ' + tdjsonPath + tdjson + ' can not be loaded');
    {$ENDIF}
    vInitialized := True;
  end;
end;

function td_json_client_create: Pointer;
begin
  DoInitialize();
  if not Assigned(otd_json_client_create) then
    @otd_json_client_create := GetProcAddress(vModule, {$IFDEF ANDROID64}'_' + {$ENDIF}'td_json_client_create');

  result := otd_json_client_create;
end;

procedure td_json_client_send(Client: Pointer; Request: PAnsiChar);
begin
  DoInitialize();
  if not Assigned(otd_json_client_send) then
    @otd_json_client_send := GetProcAddress(vModule, {$IFDEF ANDROID64}'_' + {$ENDIF}'td_json_client_send');

  if Assigned(Client) then
    otd_json_client_send(Client, Request);
end;

function td_json_client_receive(Client: Pointer; Timeout: Double): PAnsiChar;
begin
  result := nil;

  DoInitialize();
  if not Assigned(otd_json_client_receive) then
    @otd_json_client_receive := GetProcAddress(vModule, {$IFDEF ANDROID64}'_' + {$ENDIF}'td_json_client_receive');

  if Assigned(Client) then
    result := otd_json_client_receive(Client, Timeout);
end;

function td_json_client_execute(Client: Pointer; Request: PAnsiChar): PAnsiChar;
begin
  result := nil;

  DoInitialize();
  if not Assigned(otd_json_client_execute) then
    @otd_json_client_execute := GetProcAddress(vModule, {$IFDEF ANDROID64}'_' + {$ENDIF}'td_json_client_execute');

  if Assigned(Client) then
    result := otd_json_client_execute(Client, Request);
end;

procedure td_json_client_destroy(Client: Pointer);
begin
  DoInitialize();
  if not Assigned(otd_json_client_destroy) then
    @otd_json_client_destroy := GetProcAddress(vModule, {$IFDEF ANDROID64}'_' + {$ENDIF}'td_json_client_destroy');

  if Assigned(Client) then
    otd_json_client_destroy(Client);
end;

procedure td_set_log_fatal_error_callback(OnFatalErrorCallback: Tsgctd_fatal_error_event); cdecl;
begin
  {$IFNDEF ANDROID64}
  DoInitialize();
  if not Assigned(otd_set_log_fatal_error_callback) then
    @otd_set_log_fatal_error_callback := GetProcAddress(vModule, 'td_set_log_fatal_error_callback');

  otd_set_log_fatal_error_callback(OnFatalErrorCallback);
  {$ENDIF}
end;

{$ENDIF}
{$ENDIF}
end.

