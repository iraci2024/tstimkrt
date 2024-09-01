{***************************************************************************
 sgcWebSocket component
 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Classes_WinHTTP;

interface

{$I sgcVer.inc}

{$IFDEF SGC_WINHTTP}

uses
  Classes, SysUtils,
  // websocket
  sgcWebSocket_Classes, sgcWebSocket_Helpers, sgcWebSocket_Types,
  sgcWebSocket_Const, sgcWebSocket_WinAPI, sgcBase_Helpers;

type
  TsgcWSConnection_WinHTTP = class(TsgcWSConnection)

  { TCPConnection }
  protected
    function GetActive: Boolean; override;
  { TCPConnection }

  { fields }
  private
    FWSSession: HINTERNET;
  protected
    function GetIP: String; override;
    function GetPort: Integer; override;
    function GetLocalIP: String; override;
    function GetLocalPort: Integer; override;
    function GetIPVersion: TwsIPVersion; override;
    function GetURL: String; override;
  public
    property WSSession: HINTERNET read FWSSession write FWSSession;
  { fields }

  { disconnect }
  protected
    procedure DoDisconnectIfActive; override;
  private
    FDoDisconnect: Boolean;
    procedure DoDisconnect(const aError: string = ''; aCloseCode: Integer =
        CS_CLOSE_NORMAL);
  public
    procedure Disconnect; overload; override;
    procedure Disconnect(const aCloseCode: Integer); overload; override;
  protected
    procedure DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL); override;
  public
    procedure Close; overload; override;
    procedure Close(const aCloseCode: Integer); overload; override;
  { disconnect }

  { ping / pong }
  protected
    procedure DoPing(const aText: string); override;
  { ping / pong }


  { write data }
  private
    procedure DoWriteData_RFC6455(const aText: string; aOpCode: TOpCode;
        aFragmented: Boolean = False); overload;
    procedure DoWriteData_RFC6455(aStream: TStream; aOpCode: TOpCode; aFragmented:
        Boolean = False); overload;
    procedure DoWriteData_RFC6455_Fragmented(const aText: string; aOpCode: TOpCode;
        aSize: Integer; const aSkipFirst: Boolean = False); overload;
    procedure DoWriteData_RFC6455_Fragmented(aStream: TStream; aOpCode: TOpCode;
        aSize: Integer; const aSkipFirst: Boolean = False); overload;
  protected
    procedure DoWriteData(const aText: string; const aSize: Integer = 0; const
        aStreaming: TwsStreaming = stmNone); overload; override;
    procedure DoWriteData(const aStream: TStream; const aSize: Integer = 0; const
        aStreaming: TwsStreaming = stmNone); overload; override;
  { write data }

  { constructor }
  public
    constructor Create; override;
    destructor Destroy; override;
  { constructor }

  end;


  TsgcWSComponent_Client_WinHTTP = class(TsgcWSComponent_WSClient)

  end;


{$ENDIF}

implementation

{$IFDEF SGC_WINHTTP}

{ TsgcWSConnection_WinHTTP }

constructor TsgcWSConnection_WinHTTP.Create;
begin
  inherited;
  FDoDisconnect := False;
end;

destructor TsgcWSConnection_WinHTTP.Destroy;
begin
  inherited;
end;

procedure TsgcWSConnection_WinHTTP.Close(const aCloseCode: Integer);
begin
  DoDisconnect('', aCloseCode);
end;

procedure TsgcWSConnection_WinHTTP.Close;
begin
  DoDisconnect;
end;

procedure TsgcWSConnection_WinHTTP.Disconnect;
begin
  inherited;
  DoDisconnect;
end;

procedure TsgcWSConnection_WinHTTP.Disconnect(const aCloseCode: Integer);
begin
  inherited;
  DoDisconnect('', aCloseCode);
end;

procedure TsgcWSConnection_WinHTTP.DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL);
begin
  inherited;
  DoDisconnect('', aCloseCode);
end;

procedure TsgcWSConnection_WinHTTP.DoDisconnect(const aError: string = ''; aCloseCode:
    Integer = CS_CLOSE_NORMAL);
var
  dwError: Cardinal;
  vBuffer: TsgcArrayOfBytes;
begin
  inherited;
  if Assigned(FWSSession) then
  begin
    vBuffer := StringTosgcArrayOfBytes(aError);

    if aError = '' then
      dwError := WinHttpWebSocketShutdown(WSSession, aCloseCode, nil, 0)
    else
      dwError := WinHttpWebSocketShutdown(WSSession, aCloseCode, vBuffer, Length(vBuffer));

    if dwError <> 0 then
      raise Exception.Create(SysErrorMessage(dwError));
  end;
end;

procedure TsgcWSConnection_WinHTTP.DoDisconnectIfActive;
begin
  inherited;
  if Assigned(FWSSession) then
    DoDisconnect;
end;

procedure TsgcWSConnection_WinHTTP.DoWriteData(const aText: string; const aSize:
    Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  case aStreaming of
    stmNone:
      begin
        if ((aSize = 0) or (aSize >= Length(aText))) then
          DoWriteData_RFC6455(aText, opText)
        else
          DoWriteData_RFC6455_Fragmented(aText, opText, aSize);
      end;
    stmFirst:
      DoWriteData_RFC6455(aText, opBinary, True);
    stmContinue:
      DoWriteData_RFC6455(aText, opContinuation, True);
    stmLast:
      DoWriteData_RFC6455(aText, opContinuation);
    stmContinueAndLast:
      begin
        if ((aSize = 0) or (aSize >= Length(aText))) then
          DoWriteData_RFC6455(aText, opContinuation)
        else
          DoWriteData_RFC6455_Fragmented(aText, opText, aSize, True)
      end;
  end;
end;

procedure TsgcWSConnection_WinHTTP.DoWriteData(const aStream: TStream; const
    aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  case aStreaming of
    stmNone:
      begin
        if ((aSize = 0) or (aSize >= aStream.size)) then
          DoWriteData_RFC6455(aStream, opBinary)
        else
          DoWriteData_RFC6455_Fragmented(aStream, opBinary, aSize);
      end;
    stmFirst:
      DoWriteData_RFC6455(aStream, opBinary, True);
    stmContinue:
      DoWriteData_RFC6455(aStream, opContinuation, True);
    stmLast:
      DoWriteData_RFC6455(aStream, opContinuation);
    stmContinueAndLast:
      begin
        if ((aSize = 0) or (aSize >= aStream.size)) then
          DoWriteData_RFC6455(aStream, opContinuation)
        else
          DoWriteData_RFC6455_Fragmented(aStream, opBinary, aSize, True)
      end;
  end;
end;

procedure TsgcWSConnection_WinHTTP.DoWriteData_RFC6455(aStream: TStream;
    aOpCode: TOpCode; aFragmented: Boolean = False);
var
  dwError: Cardinal;
  vBuffer: TsgcArrayOfBytes;
begin
  if Assigned(FWSSession) then
  begin
    SetLength(vBuffer, aStream.Size);
    {$IFDEF DXE3}
    aStream.ReadData(vBuffer, aStream.Size);
    {$ELSE}
    aStream.Read(vBuffer[0], aStream.Size);
    {$ENDIF}

    if aFragmented then
      dwError := WinHttpWebSocketSend(FWSSession, WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE, vBuffer, aStream.Size)
    else
      dwError := WinHttpWebSocketSend(FWSSession, WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE, vBuffer, aStream.Size);

    if dwError <> 0 then
      raise Exception.Create(SysErrorMessage(dwError));

    SendBytes := SendBytes + Length(vBuffer);
  end;
end;

procedure TsgcWSConnection_WinHTTP.DoWriteData_RFC6455(const aText: string;
    aOpCode: TOpCode; aFragmented: Boolean = False);
var
  dwError: Cardinal;
  vBuffer: TsgcArrayOfBytes;
begin
  if Assigned(FWSSession) then
  begin
    vBuffer := StringTosgcArrayOfBytes(aText);

    if aFragmented then
      dwError := WinHttpWebSocketSend(FWSSession, WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE, vBuffer, Length(vBuffer))
    else
      dwError := WinHttpWebSocketSend(FWSSession, WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE, vBuffer, Length(vBuffer));

    if dwError <> 0 then
      raise Exception.Create(SysErrorMessage(dwError));

    SendBytes := SendBytes + Length(vBuffer);
  end;
end;

procedure TsgcWSConnection_WinHTTP.DoWriteData_RFC6455_Fragmented(aStream:
    TStream; aOpCode: TOpCode; aSize: Integer; const aSkipFirst: Boolean =
    False);
var
  i: Int64;
  j: Integer;
  oStream: TMemoryStream;
begin
  // ... initialize
  i := 0;
  j := 0;
  aStream.Position := 0;
  // ... send fragmented streams
  oStream := TMemoryStream.Create;
  Try
    while i < aStream.size do
    begin
      if aStream.size - i < aSize then
        oStream.CopyFrom(aStream, aStream.size - i)
      else
        oStream.CopyFrom(aStream, aSize);
      i := i + oStream.size;
      if (j = 0) and not (aSkipFirst) then
        DoWriteData_RFC6455(oStream, aOpCode, True)
      else if i >= aStream.size then
        DoWriteData_RFC6455(oStream, opContinuation)
      else
        DoWriteData_RFC6455(oStream, opContinuation, True);
      oStream.Clear;
      inc(j);
    end;
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSConnection_WinHTTP.DoWriteData_RFC6455_Fragmented(const aText:
    string; aOpCode: TOpCode; aSize: Integer; const aSkipFirst: Boolean =
    False);
var
  oStream: TsgcStringStream;
begin
  oStream := TsgcStringStream.Create({$IFDEF NEXTGEN}String{$ENDIF}(UTF8Encode(aText)));
  Try
    DoWriteData_RFC6455_Fragmented(TStream(oStream), aOpCode, aSize, aSkipFirst);
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcWSConnection_WinHTTP.GetActive: Boolean;
begin
  result := Assigned(FWSSession);
end;

function TsgcWSConnection_WinHTTP.GetIP: String;
begin
  result := '';
end;

function TsgcWSConnection_WinHTTP.GetLocalIP: String;
begin
  result := '';
end;

function TsgcWSConnection_WinHTTP.GetLocalPort: Integer;
begin
  result := 0;
end;

function TsgcWSConnection_WinHTTP.GetPort: Integer;
begin
  result := 0;
end;

function TsgcWSConnection_WinHTTP.GetURL: String;
begin
  Result := '';
end;

procedure TsgcWSConnection_WinHTTP.DoPing(const aText: string);
begin
  inherited;
  raise TsgcWSException.Create(S_FEATURE_UNSUPPORTED);
end;

function TsgcWSConnection_WinHTTP.GetIPVersion: TwsIPVersion;
begin
  Result := ipV4;
end;

{$ENDIF}

end.
