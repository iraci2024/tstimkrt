{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Classes_Indy;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, Math,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
{$IFDEF MSWINDOWS}Windows{$ELSE}SyncObjs{$ENDIF},
  // websocket
  sgcWebSocket_Classes, sgcWebSocket_Types, sgcWebSocket_Helpers,
  sgcWebSocket_Const, sgcWebSocket_Classes_SyncObjs,
  sgcWebSocket_Extensions, sgcTCP_Classes, sgcBase_Helpers,
  sgcSocket_Classes_Indy,
{$IFDEF SGC_HTTP2}sgcWebSocket_Classes_HTTP2, sgcHTTP2_Types, {$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdTCPConnection{$ELSE}IdTCPConnection{$ENDIF},
{$IFDEF INDY10_5_7}
{$IFDEF SGC_INDY}sgcIdHashSHA{$ELSE}IdHashSHA{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobalProtocols{$ELSE}IdGlobalProtocols{$ENDIF}
{$ELSE}
{$IFDEF SGC_INDY}sgcIdHashSHA1{$ELSE}IdHashSHA1{$ENDIF}
{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHeaderList{$ELSE}IdHeaderList{$ENDIF},
{$IFDEF SGC_INDY}sgcIdThread{$ELSE}IdThread{$ENDIF},
{$IFDEF SGC_INDY}sgcIdException{$ELSE}IdException{$ENDIF},
{$IFDEF SGC_INDY}sgcIdComponent{$ELSE}IdComponent{$ENDIF},
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
{$IFDEF SGC_INDY}sgcIdIntercept{$ELSE}IdIntercept{$ENDIF},
{$IFDEF SGC_INDY}sgcIdServerInterceptLogBase{$ELSE}IdServerInterceptLogBase{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomHTTPServer{$ELSE}IdCustomHTTPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF};

type
  TsgcWSHTTP2RequestEvent = procedure(const aConnection: TsgcWSConnection;
    const aHeaders: TStringList; const Bytes: TBytes;
    aStreamIdentifierRequest: Integer; aStreamIdentifierPush: Integer)
    of object;

  TsgcThreadListQueuePings = class
    (TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  end;

  TsgcWSMsg = class
  private
    FContinuation: Boolean;
    FFragmented: Boolean;
    FMaskIndex: Integer;
    FOpCode: TOpcode;
    FPayLoad: TPayLoad;
    FPayLoadLength: Integer;
    FPLState: Integer;
    FReadBuffer: TIdBytes;
    FReadMasked: Boolean;
    FState: Integer;
    FStream: TStream;
    function GetStream: TStream;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Free;
  protected
    Mask: array [0 .. 3] of Byte;
  public
    procedure Clear;
    procedure ClearStream;
    procedure ClearReadBuffer;
  public
    property Continuation: Boolean read FContinuation write FContinuation;
    property Fragmented: Boolean read FFragmented write FFragmented;
    property MaskIndex: Integer read FMaskIndex write FMaskIndex;
    property OpCode: TOpcode read FOpCode write FOpCode;
    property PayLoad: TPayLoad read FPayLoad write FPayLoad;
    property PayLoadLength: Integer read FPayLoadLength write FPayLoadLength;
    property PLState: Integer read FPLState write FPLState;
    property ReadBuffer: TIdBytes read FReadBuffer write FReadBuffer;
    property ReadMasked: Boolean read FReadMasked write FReadMasked;
    property State: Integer read FState write FState;
    property Stream: TStream read GetStream write FStream;
  end;

  TsgcWSThrottle = class(TsgcTCPThrottle)
  end;

  TsgcPing = class
  private
    FDate: TDateTime;
    FText: String;
  public
    property Date: TDateTime read FDate write FDate;
    property Text: String read FText write FText;
  end;

  TsgcWSServerForwardBase = class
  private
    FEnabled: Boolean;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TsgcWSIdLogFileClient = class(TsgcIdLogFileClient)
  private
    FSentData: Boolean;
    FUnMaskFrames: Boolean;
  protected
    procedure InitComponent; override;
    procedure LogSentData({$IFDEF INDY10_2}const {$ENDIF}AText, AData: string); override;
  public
    property UnMaskFrames: Boolean read FUnMaskFrames write FUnMaskFrames;
  end;

  TsgcWSIdLogFileServer = class(TsgcIdLogFileServer)
  private
    FUnMaskFrames: Boolean;
  public
    function Accept(aConnection: TComponent): TIdConnectionIntercept; override;
  public
    property UnMaskFrames: Boolean read FUnMaskFrames write FUnMaskFrames;
  end;

  TsgcWSIdLogFileServerConnection = class(TIdServerInterceptLogFileConnection)
  private
    FUnMaskFrames: Boolean;
  public
    procedure LogRecvDecodedString(const AText: string);
  public
    property UnMaskFrames: Boolean read FUnMaskFrames write FUnMaskFrames;
  end;

  TsgcWSServerForwardHTTP = class(TsgcWSServerForwardBase)
  private
    FDocument: string;
    FTLSOptions: TsgcTCPTLS_Options;
    FURL: string;
    function GetTLSOptions: TsgcTCPTLS_Options;
  public
    destructor Destroy; override;
  public
    function GetForwardURL: string;
  public
    property Document: string read FDocument write FDocument;
    property TLSOptions: TsgcTCPTLS_Options read GetTLSOptions
      write FTLSOptions;
    property URL: string read FURL write FURL;
  end;

  TsgcWSHandshake = class(TsgcWSHandshake_Base)
    { keys }
  protected
    function GetWSAccept(const aKey: String): String; overload; // spRFC6455
    function GetWSAccept(aKey1, aKey2, aKey3: String): TIdBytes; overload;
    { keys }
  end;

  TsgcWSConnection_Base_Indy = class
    ({$IFDEF SGC_HTTP2}TsgcWSConnection_Base_HTTP2{$ELSE}TsgcWSConnection_Abstract{$ENDIF})
    { helpers }
  protected
    function IsControlFrame(aOpCode: TOpcode): Boolean;
    { helpers }

    { TCPConnection }
  protected
    function ExistsTCPConnection: Boolean; virtual; abstract;
    { TCPConnection }

    { message }
  private
    FWSMessage: TsgcWSMsg;
    FWSControlCode: TsgcWSMsg;
    FWSMSG: TsgcWSMsg;
    FReadBufferSize: Integer;
    function GetWSMessage: TsgcWSMsg;
    function GetWSControlCode: TsgcWSMsg;
    function GetWSMSG: TsgcWSMsg;
    procedure SetWSMSG(const Value: TsgcWSMsg);
  protected
    property WSMSG: TsgcWSMsg read GetWSMSG write SetWSMSG;
    property WSMessage: TsgcWSMsg read GetWSMessage write FWSMessage;
    property WSControlCode: TsgcWSMsg read GetWSControlCode
      write FWSControlCode;
    property ReadBufferSize: Integer read FReadBufferSize write FReadBufferSize;
    { message }

    { protocol fields }
  private
    FStream: TStream;
  protected
    FState: Integer;
    { protocol fields }

    { disconnect }
  protected
    procedure DoDisconnectIfActive; override;
  protected
    FDoDisconnect: Boolean;
    FMustDisconnect: Boolean;
    procedure DoDisconnect(const aError: string = '';
      aCloseCode: Integer = CS_CLOSE_NORMAL); virtual;
  public
    procedure DisconnectPeer(aCallOnDisconnectIfClosed: Boolean = False); virtual;
        abstract;
    procedure Disconnect; overload; override;
    procedure Disconnect(const aCloseCode: Integer); overload; override;
  protected
    procedure DoClearBuffer; virtual; abstract;
    procedure DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL); override;
  public
    procedure Close; overload; override;
    procedure Close(const aCloseCode: Integer); overload; override;
    { disconnect }

    { Ping / pong }
  private
    FQueuePingList: TsgcThreadListQueuePings;
    function GetQueuePingList: TsgcThreadListQueuePings;
    procedure ClearPingList;
  protected
    property QueuePingList: TsgcThreadListQueuePings read GetQueuePingList
      write FQueuePingList;
    procedure DoPing(const AText: string); override;
    { Ping / pong }

    { encode }
  private
    function DoEncodeHeader(aOpCode: TOpcode;
      aFragmented: Boolean = False): Byte;
    function DoEncodeMaskingKey: TIdBytes;
    function DoEncodePayLoad(aLength: Int64): TIdBytes;
    function DoEncodeMessage(aBytes, aBytesMsg: TIdBytes): TIdBytes;
    { encode }

    { Handshake }
  private
    FHeadersRequest: TIdHeaderList;
    function GetHeadersRequest: TIdHeaderList;
  protected
    procedure DoHandshake; virtual; abstract;
  public
    property HeadersRequest: TIdHeaderList read GetHeadersRequest;
    { Handshake }

    { write data }
  protected
    procedure DoWriteData_RFC6455(const AText: string; aOpCode: TOpcode;
      aFragmented: Boolean = False); overload; virtual;
    procedure DoWriteData_RFC6455(aStream: TStream; aOpCode: TOpcode;
      aFragmented: Boolean = False); overload; virtual;
    procedure DoWriteData_RFC6455_Fragmented(const AText: string;
      aOpCode: TOpcode; aSize: Integer; const aSkipFirst: Boolean = False);
      overload; virtual;
    procedure DoWriteData_RFC6455_Fragmented(aStream: TStream; aOpCode: TOpcode;
      aSize: Integer; const aSkipFirst: Boolean = False); overload; virtual;
    procedure DoWriteData_Hixie76(const AText: string;
      aOpCode: TOpcode); virtual;
    { write data }

    { sse }
  protected
    procedure DoWriteData_SSE(const AText: String);
    { sse }

    { tcp }
  protected
    procedure DoWriteData_TCP(const AText: String); overload;
    procedure DoWriteData_TCP(const aStream: TStream); overload;
    { tcp }

    { forward http }
  private
    FForward_HTTP: TsgcWSServerForwardHTTP;
  protected
    function GetForward_HTTP: TsgcWSServerForwardHTTP;
  public
    property Forward_HTTP: TsgcWSServerForwardHTTP read GetForward_HTTP;
    { forward http }

    { alt-svc }
  private
    FAltSvc: String;
  protected
    property AltSvc: String read FAltSvc write FAltSvc;
    { alt-svc }

    { http }
  protected
    procedure DoHTTPResponse(const aContent, aContentType: String; const
        aCustomHeaders: TStrings = nil; aStreamIdentifier: Integer = 0); overload;
    procedure DoHTTPResponse(const aStream: TStream; aContentType: String; const
        aCustomHeaders: TStrings = nil; aStreamIdentifier: Integer = 0); overload;
        virtual; abstract;
    procedure DoHTTPError(aCode: Integer = 404;
      aDescription: string = 'Not Found'); virtual; abstract;
    procedure DoHTTPRedirect(const aURL: string); virtual; abstract;
    { http }

    { write data }
  protected
    procedure DoWriteData(const AText: string; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; override;
    procedure DoWriteData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; override;
    { write data }

    { constructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor }

    { ReadData }
  private
    function IsValidUTF8(aStream: TsgcStringStream): Boolean;
  private
    procedure DoWriteWSMSGBuffer;
    procedure DoWriteWSMSGFragmented;
  protected
    procedure DoLogRead(aOpCode: TOpcode; const aStream: TStream); virtual;
    procedure DoLogWrite(aOpCode: TOpcode; const AText: String;
      aFragmented: Boolean = False); overload; virtual;
    procedure DoLogWrite(aOpCode: TOpcode; const aStream: TStream;
      aFragmented: Boolean = False); overload; virtual;
  protected
    procedure DoWriteWSMSG; virtual;
  protected
    function GetPayloadReadLength: Integer; virtual;
  protected
    function DoReadUTF8(aStream: TStream): String; virtual;
  protected
    procedure DoReadData_RFC6455(aErrorIfNotMasked: Boolean = False);
    procedure DoReadData_RFC8441;
    procedure DoReadData_Hixie76;
    procedure DoReadData_TCP;
  protected
    procedure ReadData(aErrorIfNotMasked: Boolean = False); virtual; abstract;
    { ReadData }
  end;

  TsgcWSConnection_Indy = class(TsgcWSConnection_Base_Indy)
    { TCPConnection }
  private
    FTCPConnection: TIdTCPConnection;
    function GetTCPConnection: TIdTCPConnection;
    procedure SetTCPConnection(const Value: TIdTCPConnection);
  protected
    property TCPConnection: TIdTCPConnection read GetTCPConnection
      write SetTCPConnection;
  protected
    function ExistsTCPConnection: Boolean; override;
    function GetActive: Boolean; override;
    { TCPConnection }

    { log }
  private
    function GetOpCodeString(aOpCode: TOpcode): string;
  protected
    procedure DoAvoidLogStart; virtual;
    procedure DoAvoidLogEnd; virtual;
  protected
    procedure DoLogRead(aOpCode: TOpcode; const aStream: TStream); override;
    procedure DoLogWrite(aOpCode: TOpcode; const AText: String;
      aFragmented: Boolean = False); overload; override;
    procedure DoLogWrite(aOpCode: TOpcode; const aStream: TStream;
      aFragmented: Boolean = False); overload; override;
    { log }

    { from TsgcWSConnection_Base_Indy }
  protected
    procedure DoWriteData_RFC6455(const AText: string; aOpCode: TOpcode;
      aFragmented: Boolean = False); overload; override;
    procedure DoWriteData_RFC6455(aStream: TStream; aOpCode: TOpcode;
      aFragmented: Boolean = False); overload; override;
    procedure DoWriteData_RFC6455_Fragmented(const AText: string;
      aOpCode: TOpcode; aSize: Integer; const aSkipFirst: Boolean = False);
      overload; override;
    procedure DoWriteData_RFC6455_Fragmented(aStream: TStream; aOpCode: TOpcode;
      aSize: Integer; const aSkipFirst: Boolean = False); overload; override;
    { from TsgcWSConnection_Base_Indy }

    { http }
  protected
    procedure DoHTTPResponse(const aStream: TStream; aContentType: String; const
        aCustomHeaders: TStrings = nil; aStreamIdentifier: Integer = 0); overload;
        override;
    procedure DoHTTPError(aCode: Integer = 404;
      aDescription: string = 'Not Found'); override;
    procedure DoHTTPRedirect(const aURL: string); override;
    { http }

    { fields }
  protected
    function GetIP: String; override;
    function GetPort: Integer; override;
    function GetLocalIP: String; override;
    function GetLocalPort: Integer; override;
    function GetIPVersion: TwsIPVersion; override;
    { fields }

    { disconnect }
  protected
    procedure DoClearBuffer; override;
  public
    procedure DisconnectPeer(aCallOnDisconnectIfClosed: Boolean = False); override;
    { disconnect }

    { write data }
  protected
    procedure DoWriteBytes(aBuffer: TIdBytes); override;
    procedure DoWriteLn(const AText: String = ''); override;
  protected
    procedure DoWriteBufferOpen; override;
    procedure DoWriteBufferFlush; override;
    procedure DoWriteBufferClose; override;
    { write data }

    { read data }
  protected
    procedure DoReadBytes(var aBuffer: TIdBytes; aCount: Integer); override;
    function GetReadBufferSize: Integer; override;
    procedure DoAfterReadData; virtual;
  protected
    procedure ReadData(aErrorIfNotMasked: Boolean = False); override;
    { read data }

    { constructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor }

    { properties }
  private
    FReadTimeOut: Integer;
  public
    property ReadTimeOut: Integer read FReadTimeOut write FReadTimeOut;
    { properties }
  end;

  TsgcWSComponent_Client_Indy = Class(TsgcWSComponent_WSClient)
    { Throttle }
  private
    FThrottle: TsgcWSThrottle;
  protected
    function GetThrottle: TsgcWSThrottle; virtual;
    procedure SetThrottle(const Value: TsgcWSThrottle); virtual;
  public
    property Throttle: TsgcWSThrottle read GetThrottle write SetThrottle;
    { Throttle }

    { constructor / destructor }
  public
    destructor Destroy; override;
    { constructor / destructor }
  End;

implementation

uses
  StrUtils,
  // sgc
  sgcBase_Const
{$IFDEF SGC_HTTP2}
    , sgcHTTP2_Const
{$ENDIF}
{$IFDEF SGC_INDY_IOCP}
    , sgcIndy_IOHandler_IOCP, sgcIndy_IOHandler_IOCP_OpenSSL
{$ENDIF}
    ;

function TsgcWSHandshake.GetWSAccept(const aKey: String): String;
var
  oHash: TIdHashSHA1;
begin
  oHash := TIdHashSHA1.Create;
  Try
    Result := aKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
{$IFDEF INDY10_2}
    Result := HexToBase64(oHash.HashStringAsHex(Result));
{$ELSE}
    Result := HexToBase64(oHash.AsHex(oHash.HashValue(Result)));
{$ENDIF}
  Finally
    sgcFree(oHash);
  End;
end;

function TsgcWSHandshake.GetWSAccept(aKey1, aKey2, aKey3: String): TIdBytes;

  procedure GetWSKey(const aKey: String; var Numbers, Spaces: Cardinal);
  var
    i: Integer;
    vChar: {$IFDEF NEXTGEN}Char{$ELSE}AnsiChar{$ENDIF};
  begin
    Numbers := 0;
    Spaces := 0;
    for i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF} to Length
      (aKey){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
    begin
      vChar := {$IFDEF NEXTGEN}Char{$ELSE}AnsiChar{$ENDIF}(aKey[i]);
{$IFDEF NEXTGEN}
      if Byte(vChar) in [0 .. 9] then
{$ELSE}
      if vChar in ['0' .. '9'] then
{$ENDIF}
        Numbers := Cardinal(ord(aKey[i])) - Cardinal(ord('0')) + Numbers * 10
      else if vChar = ' ' then
        Spaces := Spaces + 1;
    end;
    if Spaces = 0 then
      raise TsgcWSException.Create(S_ERROR_DECODING_SEC_WEBSOCKET_KEY);
  end;
var
  vNumbers: Cardinal;
  vSpaces: Cardinal;
  vKey1, vKey2: String;
begin
  GetWSKey(aKey1, vNumbers, vSpaces);
  vKey1 := IntToHex((vNumbers div vSpaces), 8);
  GetWSKey(aKey2, vNumbers, vSpaces);
  vKey2 := IntToHex((vNumbers div vSpaces), 8);
  Result := HexToBytes(GetMD5(vKey1 + vKey2 + aKey3));
end;

{ TsgcWSConnection_Base_Indy }

constructor TsgcWSConnection_Base_Indy.Create;
begin
  inherited;
  FReadBufferSize := 16384;
  FDoDisconnect := False;
  FMustDisconnect := False;
end;

destructor TsgcWSConnection_Base_Indy.Destroy;
begin
  ClearPingList;
  sgcFree(FQueuePingList);
  sgcFree(FWSMessage);
  sgcFree(FWSControlCode);
  FWSMSG := nil;
  // set nil, is not created only assigned by FWSMessage or FWSControlCode
  sgcFree(FHeadersRequest);
  sgcFree(FForward_HTTP);
  inherited;
end;

procedure TsgcWSConnection_Base_Indy.Close(const aCloseCode: Integer);
begin
  inherited;
  case Transport of
    trpRFC6455, trpRFC8441, trpFlash:
      DoClose(aCloseCode);
{$IFDEF SGC_HTTP2}
    trpHTTP2:
      DoHTTP2Close(Th2ErrorCodes(aCloseCode));
{$ENDIF}
  else
    DoDisconnect('', aCloseCode);
  end;
end;

procedure TsgcWSConnection_Base_Indy.Close;
begin
  inherited;
  case Transport of
    trpRFC6455, trpRFC8441, trpFlash:
      DoClose;
{$IFDEF SGC_HTTP2}
    trpHTTP2:
      DoHTTP2Close;
{$ENDIF}
  else
    DoDisconnect;
  end;
end;

procedure TsgcWSConnection_Base_Indy.Disconnect;
begin
  inherited;
  DoDisconnect;
end;

procedure TsgcWSConnection_Base_Indy.Disconnect(const aCloseCode: Integer);
begin
  inherited;
  DoDisconnect('', aCloseCode);
end;

procedure TsgcWSConnection_Base_Indy.DoClose(aCloseCode
  : Integer = CS_CLOSE_NORMAL);
var
  oStream: TMemoryStream;
  oByte: TIdBytes;
begin
  inherited;
  oByte := nil;

  // ... clear buffer
  DoClearBuffer;

  // ... send stream
  case Transport of
    trpRFC6455, trpRFC8441, trpFlash:
      begin
        oStream := TMemoryStream.Create;
        Try
          oByte := ToBytes(aCloseCode);
          oStream.Write(oByte[1], 1);
          oStream.Write(oByte[0], 1);

          DoWriteData_RFC6455(oStream, opClose);
        Finally
          sgcFree(oStream);
        End;
      end;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoDisconnect(const aError: string = '';
  aCloseCode: Integer = CS_CLOSE_NORMAL);
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, self, 'DoDisconnect ', '[Error]: ' + aError);
{$ENDIF}
  // ... if already disconnected don't try to disconnect again
  if FDoDisconnect or (Disconnected = False) and (ExistsTCPConnection = False)
  then
    exit
  else
  begin
    FDoDisconnect := True;
    Disconnected := True;
  end;

  Try
    // ... close connection
    if CleanDisconnect then
    begin
      Try
        DoClose(aCloseCode);
      Except
        // if there is any exception don't raise
        // just let the flow continue and disconnect
      End;
    end;

    // ... disconnect and raise error if exists
    DisconnectPeer;
  Except
    On E: EIdSilentException do
    begin
      if RaiseDisconnectExceptions then
        raise;
    end;
    On E: Exception do
      raise;
  End;

  if aError <> '' then
    if RaiseDisconnectExceptions then
      raise TsgcWSException.Create(aError);
end;

procedure TsgcWSConnection_Base_Indy.DoDisconnectIfActive;
begin
  inherited;
  // ... if already disconnected don't try to disconnect again
  if FDoDisconnect or Disconnected then
  begin
    Disconnected := True;
    exit;
  end
  else
  begin
    FDoDisconnect := True;
    Disconnected := True;
  end;

  // ... not check if active, can freeze thread
  DisconnectPeer(True);
end;

function TsgcWSConnection_Base_Indy.DoEncodeHeader(aOpCode: TOpcode;
  aFragmented: Boolean = False): Byte;
begin
  // ... header
  if aFragmented then
    Result := $0 or CS_Frames[ord(aOpCode)]
  else
    Result := $80 or CS_Frames[ord(aOpCode)];
  // ... deflate frame
  if not IsControlFrame(aOpCode) and (aOpCode <> opContinuation) then
    Extensions.EncodeHeader(Result);
end;

function TsgcWSConnection_Base_Indy.DoEncodeMaskingKey: TIdBytes;
var
  i: Integer;
begin
  SetLength(Result, 4);
  Randomize;
  for i := 0 to 3 do
    Result[i] := RandomRange(0, 255);
end;

function TsgcWSConnection_Base_Indy.DoEncodePayLoad(aLength: Int64): TIdBytes;

  function GetPayLoad(aLength: Integer): Byte;
  begin
    if Masked then
      Result := aLength OR $80
    else
      Result := aLength;
  end;

begin
  Case aLength of
    0 .. 125:
      begin
        AppendByte(Result, GetPayLoad(aLength));
      end;
    126 .. 65535:
      begin
        AppendByte(Result, GetPayLoad(126));
        AppendByte(Result, (aLength shr 8) AND $FF);
        AppendByte(Result, aLength AND $FF);
      end;
  else
    begin
      AppendByte(Result, GetPayLoad(127));
      AppendByte(Result, aLength shr 56 and $FF);
      AppendByte(Result, aLength shr 48 and $FF);
      AppendByte(Result, aLength shr 40 and $FF);
      AppendByte(Result, aLength shr 32 and $FF);
      AppendByte(Result, aLength shr 24 and $FF);
      AppendByte(Result, aLength shr 16 and $FF);
      AppendByte(Result, aLength shr 8 and $FF);
      AppendByte(Result, aLength and $FF);
    end;
  End;
end;

function TsgcWSConnection_Base_Indy.DoEncodeMessage(aBytes, aBytesMsg: TIdBytes)
  : TIdBytes;
var
  i, j: Integer;
  vMask: TIdBytes;
begin
  Result := aBytesMsg;

  if Masked then
  begin
    j := 0;
    SetLength(vMask, 4);
    for i := Length(aBytes) - 4 to Length(aBytes) - 1 do
    begin
      vMask[j] := aBytes[i];
      inc(j);
    end;

    for i := 0 to Length(Result) - 1 do
      Result[i] := Result[i] xor vMask[i mod 4];
  end;
end;

procedure TsgcWSConnection_Indy.DoReadBytes(var aBuffer: TIdBytes;
  aCount: Integer);
var
  vCount: Integer;
begin
  if Assigned(TCPConnection) then
  begin
    if Assigned(TCPConnection.IOHandler) then
    begin
      vCount := aCount;
      if aCount > TCPConnection.IOHandler.InputBuffer.Size then
        vCount := TCPConnection.IOHandler.InputBuffer.Size;
      TCPConnection.IOHandler.ReadBytes(aBuffer, vCount);
      if vCount = -1 then
        vCount := Length(aBuffer);
      RecBytes := RecBytes + vCount;
    end;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoReadData_Hixie76;
var
  vBuffer: TIdBytes;
begin
  DoReadBytes(vBuffer, 1);
  case FState of
    0:
      begin
        if vBuffer[0] = $00 then
          inc(FState)
        else if vBuffer[0] = $FF then
        begin
          DoReadBytes(vBuffer, 1);
          if vBuffer[0] = $00 then
            DoWriteData_Hixie76('', opClose);
        end
        else
          exit
      end;
    1:
      begin
        if vBuffer[0] <> $FF then
        begin
          if not Assigned(FStream) then
            FStream := TsgcStringStream.Create('');
          FStream.Write(vBuffer[0], 1);
        end
        else
        begin
{$IFDEF INDY10_5_5}
          DoMessageEvent(BytesToString(TIdBytes(TsgcStringStream(FStream)
            .Bytes), 0, FStream.Size,
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}));
{$ELSE}
{$IFDEF INDY10_2}
            DoMessageEvent(BytesToString(TIdBytes(TsgcStringStream(FStream)
            .Bytes), 0, FStream.Size));
{$ELSE}
            DoMessageEvent(UTF8Decode(TsgcStringStream(FStream).DataString));
{$ENDIF}
{$ENDIF}
          sgcFree(FStream);
          FState := 0;
        end;
      end;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoReadData_RFC6455(aErrorIfNotMasked
  : Boolean = False);
var
  i: Integer;
  vBuffer: TIdBytes;
begin
  case WSMSG.State of
    0:
      begin
        DoReadBytes(vBuffer, 1);
        CloseCode := 0;
        CloseReason := '';
        // ... opcode
        WSMSG.Continuation := False;
        case vBuffer[0] and $0F of
          $0:
            begin
              WSMSG := WSMessage;
              WSMSG.Continuation := True;
              if WSMSG.OpCode = opNone then
                DoDisconnect(S_UNFRAGMENTED_MESSAGE_AFTER_FIN,
                  CS_CLOSE_PROTOCOL_ERROR);
            end;
          $1:
            begin
              if WSMSG.Fragmented then
                DoDisconnect(S_OPTEXT_AFTER_CONTINUATION_UNFINISHED,
                  CS_CLOSE_PROTOCOL_ERROR);
              WSMessage.OpCode := opText;
              WSMSG := WSMessage;
            end;
          $2:
            begin
              if WSMSG.Fragmented then
                DoDisconnect(S_OPBINARY_AFTER_CONTINUATION_UNFINISHED,
                  CS_CLOSE_PROTOCOL_ERROR);
              WSMessage.OpCode := opBinary;
              WSMSG := WSMessage;
            end;
          $8:
            begin
              WSControlCode.OpCode := opClose;
              WSMSG := WSControlCode;
            end;
          $9:
            begin
              WSControlCode.OpCode := opPing;
              WSMSG := WSControlCode;
            end;
          $A:
            begin
              WSControlCode.OpCode := opPong;
              WSMSG := WSControlCode;
            end;
        else
          DoDisconnect(S_INVALID_OPTCODE, CS_CLOSE_PROTOCOL_ERROR);
        end;
        // ... fragmented
        WSMSG.Fragmented := (vBuffer[0] and $80) <> $80;

        // ... not allow rsv bits if not extension negotiated (only check if no opContinuation)
        if not WSMSG.Continuation then
        begin
          if (vBuffer[0] and $70) <> $0 then
          begin
            if not Extensions.ExtensionNegotiated then
              DoDisconnect(S_INVALID_RSV_NO_EXTENSION, CS_CLOSE_PROTOCOL_ERROR);
          end;
        end;
        // ... deflate frame
        if not IsControlFrame(WSMessage.OpCode) and not WSMSG.Continuation then
          Extensions.DecodeHeader(vBuffer[0]);
        // ... verify if control optcode is fragmented
        if WSMSG.Fragmented then
        begin
          case WSMSG.OpCode of
            opClose, opPing, opPong:
              DoDisconnect(S_OPTCODE_MUST_NOT_FRAGMENTED,
                CS_CLOSE_PROTOCOL_ERROR);
          else
            WSMSG.State := WSMSG.State + 1;
          end;
        end
        else
          WSMSG.State := WSMSG.State + 1;
      end;
    1:
      begin
        DoReadBytes(vBuffer, 1);
        // mask
        WSMSG.ReadMasked := True;
        if (vBuffer[0] and $80) <> $80 then
        begin
          if aErrorIfNotMasked then
            DoDisconnect(S_MESSAGE_NOT_MASKED, CS_CLOSE_PROTOCOL_ERROR)
          else
            WSMSG.ReadMasked := False;
        end;

        // payload
        WSMSG.MaskIndex := 0;
        WSMSG.PayLoadLength := vBuffer[0] and $7F;
        case WSMSG.PayLoadLength of
          0 .. 125:
            begin
              WSMSG.PayLoad := pa7bits;
              WSMSG.State := WSMSG.State + 1;
              if not WSMSG.ReadMasked then
              begin
                if WSMSG.PayLoadLength > 0 then
                  WSMSG.State := WSMSG.State + 1
                else
                begin
                  // ... send message if not fragmented
                  DoWriteWSMSG;
                  exit;
                end;
              end;
            end;
          126:
            WSMSG.PayLoad := pa16bits;
          127:
            WSMSG.PayLoad := pa64bits;
        else
          DoDisconnect(S_PAYLOAD_NOT_CORRECT);
        end;
        // ... control frames are only allowed to have payload up to and including 125 octets
        case WSMSG.OpCode of
          opClose, opPing, opPong:
            begin
              if WSMSG.PayLoad <> pa7bits then
                DoDisconnect(S_CONTROL_FRAMES_UP_125, CS_CLOSE_PROTOCOL_ERROR);
            end;
        end;
        WSMSG.State := WSMSG.State + 1;
      end;
    2:
      begin
        DoReadBytes(vBuffer, 1);
        case WSMSG.PLState of
          0:
            WSMSG.PayLoadLength := vBuffer[0];
        else
          WSMSG.PayLoadLength := WSMSG.PayLoadLength shl 8 or vBuffer[0];
        end;

        if ((WSMSG.PLState = 1) and (WSMSG.PayLoad = pa16bits) or
          (WSMSG.PLState = 7) and (WSMSG.PayLoad = pa64bits)) then
        begin
          WSMSG.State := WSMSG.State + 1;
          if not WSMSG.ReadMasked then
            WSMSG.State := WSMSG.State + 1;
        end
        else
          WSMSG.PLState := WSMSG.PLState + 1;
      end;
    3:
      begin
        DoReadBytes(vBuffer, 1);
        // mask
        WSMSG.Mask[WSMSG.MaskIndex] := vBuffer[0];

        if (WSMSG.MaskIndex = 3) then
        begin
          if WSMSG.PayLoadLength = 0 then
          begin
            if (WSMSG.OpCode = opClose) then // code error is optional
              DoDisconnect
            else
            begin
              // ... send message
              DoWriteWSMSG;
              exit;
            end;
          end
          else
            WSMSG.State := WSMSG.State + 1;
        end
        else
          WSMSG.MaskIndex := WSMSG.MaskIndex + 1;
      end;
    4:
      begin
        // ... read message
        if WSMSG.PayLoadLength > ReadBufferSize then
          DoReadBytes(vBuffer, ReadBufferSize)
        else
          DoReadBytes(vBuffer, WSMSG.PayLoadLength);

        // ... already disconnected
        if Length(vBuffer) = 0 then
        begin
          Disconnected := True;
          exit;
        end;

        // ... buffer hasn't enough data
        if (WSMSG.PayLoadLength > Length(vBuffer)) then
        begin
          sgcAddBytes(vBuffer, WSMSG.FReadBuffer);
          WSMSG.PayLoadLength := WSMSG.PayLoadLength - Length(vBuffer);
          exit;
        end;

        // ... final buffer chunk
        if Length(WSMSG.FReadBuffer) > 0 then
        begin
          sgcAddBytes(vBuffer, WSMSG.FReadBuffer);
          SetLength(vBuffer, 0);
          sgcAddBytes(WSMSG.FReadBuffer, vBuffer);
          WSMSG.PayLoadLength := Length(vBuffer);
          SetLength(WSMSG.FReadBuffer, 0);
        end;

        // ... read masked message
        if WSMSG.ReadMasked then
        begin
          for i := 0 to Length(vBuffer) - 1 do
            vBuffer[i] := vBuffer[i] xor WSMSG.Mask[i mod 4];
        end;

        // ... copy to stream
        if WSMSG.OpCode <> opClose then
        begin
{$IFDEF D2010}
          if WSMSG.Stream.Size = 0 then
          begin
{$IFNDEF NEXTGEN}
            WSMSG.Stream.Free;
{$ENDIF}
            // faster that write
            WSMSG.Stream := TBytesStream.Create(TBytes(vBuffer));
            if Extensions.ExtensionNegotiated then
              WSMSG.Stream.Position := Length(vBuffer)
            else if WSMSG.Fragmented then
              WSMSG.Stream.Position := Length(vBuffer);
          end
          else
{$ENDIF}
            WSMSG.Stream.Write(vBuffer[0], WSMSG.PayLoadLength);
          SetLength(vBuffer, 0);
        end
        else
        begin
          // ... close code
          if WSMSG.OpCode = opClose then // read close code
          begin
            case Length(vBuffer) of
              0: { optional code }
                ;
              1:
                DoDisconnect(S_INVALID_PAYLOAD_LENGTH_CLOSE,
                  CS_CLOSE_PROTOCOL_ERROR);
              2:
                begin
                  CloseCode := vBuffer[0];
                  CloseCode := CloseCode shl 8 or vBuffer[1];
                  case CloseCode of
                    1 .. 999:
                      DoDisconnect(S_INVALID_CLOSE_CODE,
                        CS_CLOSE_PROTOCOL_ERROR);
                    1000 .. 2999:
                      case CloseCode of
                        CS_CLOSE_RESERVED, CS_CLOSE_NO_STATUS_RECEIVED,
                          CS_CLOSE_ABNORMAL_CLOSURE:
                          DoDisconnect(S_INVALID_CLOSE_CODE,
                            CS_CLOSE_PROTOCOL_ERROR);
                        CS_CLOSE_NORMAL, CS_CLOSE_GOING_AWAY,
                          CS_CLOSE_PROTOCOL_ERROR, CS_CLOSE_UNSUPPORTED_DATA,
                          CS_CLOSE_INVALID_PAYLOAD_DATA,
                          CS_CLOSE_POLICY_VIOLATION, CS_CLOSE_MESSAGE_TOO_BIG,
                          CS_CLOSE_MANDATORY_EXTENSION,
                          CS_CLOSE_INTERNAL_SERVER_ERROR:
                          DoDisconnect('', CS_CLOSE_NORMAL);
                      else
                        DoDisconnect(S_INVALID_CLOSE_CODE,
                          CS_CLOSE_PROTOCOL_ERROR);
                      end;
                    3000 .. 4999:
                      DoDisconnect('', CS_CLOSE_NORMAL);
                  else
                    DoDisconnect(S_INVALID_CLOSE_CODE, CS_CLOSE_PROTOCOL_ERROR);
                  end;
                end
            else
              begin
                CloseCode := vBuffer[0];
                CloseCode := CloseCode shl 8 or vBuffer[1];

                WSMSG.Stream.WriteBuffer(vBuffer[2], WSMSG.PayLoadLength - 2);
                CloseReason := DoReadUTF8(WSMSG.Stream);
              end;
            end;
          end;
        end;
        if Assigned(WSMSG) then
          WSMSG.PayLoadLength := 0;
        // ... handle message
        if ExistsTCPConnection then
          DoWriteWSMSG;
      end;
  else
    Disconnected := True;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteWSMSG;
begin
  case WSMSG.OpCode of
    opBinary:
      begin
        case FragmentedMessages of
          frgOnlyBuffer:
            begin
              if not WSMSG.Fragmented then
                DoWriteWSMSGBuffer
              else
                DoWriteWSMSGFragmented;
            end;
          frgOnlyFragmented:
            DoWriteWSMSGFragmented;
          frgAll:
            begin
              DoWriteWSMSGFragmented;
              if not WSMSG.Fragmented then
                DoWriteWSMSGBuffer;
            end;
        end;
      end;
    opText:
      begin
        case FragmentedMessages of
          frgOnlyBuffer:
            if not WSMSG.Fragmented then
              DoWriteWSMSGBuffer
            else
            begin
              WSMSG.State := 0;
              WSMSG.PLState := 0;
            end;
          frgOnlyFragmented:
            DoWriteWSMSGFragmented;
          frgAll:
            begin
              DoWriteWSMSGFragmented;
              if not WSMSG.Fragmented then
                DoWriteWSMSGBuffer;
            end;
        end;
      end;
  else
    DoWriteWSMSGBuffer;
  end;
end;

procedure TsgcWSConnection_Indy.DoWriteBytes(aBuffer: TIdBytes);
begin
  inherited;
  TCPConnection.IOHandler.Write(aBuffer);
  SendBytes := SendBytes + Length(aBuffer);
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_Hixie76(const AText: string;
  aOpCode: TOpcode);
var
  Bytes: TIdBytes;
begin
  if ((aOpCode = opText)) or (aOpCode = opClose) then
  begin
    DoWriteBufferOpen;
    Try
      if aOpCode = opText then
      begin
        AppendByte(Bytes, $00);
{$IFDEF INDY10_5_5}
        AppendBytes(Bytes, ToBytes(AText,
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}));
{$ELSE}
          AppendBytes(Bytes, ToBytes(UTF8Encode(AText)));
{$ENDIF}
        AppendByte(Bytes, $FF);
      end
      else if aOpCode = opClose then
      begin
        AppendByte(Bytes, $FF);
        AppendByte(Bytes, $00);
      end;
      DoWriteBytes(Bytes);
      DoWriteBufferFlush;
    Finally
      DoWriteBufferClose;
    End;
  end;
end;

procedure TsgcWSConnection_Indy.ReadData(aErrorIfNotMasked: Boolean = False);
begin
  if not Enabled then
  begin
    DoHandshake;
    FState := 0;
  end
  else
  begin
    while ExistsTCPConnection and
      (TCPConnection.IOHandler.InputBufferIsEmpty = False) and
      (Disconnected = False) and (FMustDisconnect = False) do
    begin
      case Transport of
        trpTCP:
          DoReadData_TCP;
        trpRFC8441:
          DoReadData_RFC8441;
{$IFDEF SGC_HTTP2}
        trpHTTP2:
          DoReadData_HTTP2;
{$ENDIF}
      else
        case Specification of
          spRFC6455:
            DoReadData_RFC6455(aErrorIfNotMasked);
          spHixie76:
            DoReadData_Hixie76;
        end;
      end;

      // ... check if inputbuffer has grown
      if ExistsTCPConnection then
      begin
        if TCPConnection.IOHandler.InputBufferIsEmpty and (Disconnected = False)
          and (FMustDisconnect = False) then
          TCPConnection.IOHandler.CheckForDataOnSource(ReadTimeOut);
      end;
    end;

    DoAfterReadData;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_RFC6455(const AText: string;
  aOpCode: TOpcode; aFragmented: Boolean = False);
var
  vBytes: TIdBytes;
  oStream: TsgcStringStream;
begin
  // ... inflate frame
  if Extensions.Compression then
  begin
    oStream := TsgcStringStream.Create
      ({$IFDEF NEXTGEN}String{$ENDIF}(UTF8Encode(AText)));
    Try
      DoWriteData_RFC6455(TStream(oStream), aOpCode);
    Finally
      sgcFree(oStream);
    End;
  end
  else
  begin
    // ... write data
    DoEnterCS;
    Try
      DoWriteBufferOpen;
      Try
        Try
          AppendByte(vBytes, DoEncodeHeader(aOpCode, aFragmented));
          if AText = #0 then
            AppendBytes(vBytes, DoEncodePayLoad(1))
          else
{$IFDEF LAZARUS}
            AppendBytes(vBytes,
              DoEncodePayLoad(Length(sgcStringToBytes(AText))));
{$ELSE}
            AppendBytes(vBytes, DoEncodePayLoad(Length(UTF8Encode(AText))));
{$ENDIF}
          if Masked then
            AppendBytes(vBytes, DoEncodeMaskingKey);
{$IFDEF LAZARUS}
          AppendBytes(vBytes, DoEncodeMessage(vBytes, sgcStringToBytes(AText)));
{$ELSE}
{$IFDEF INDY10_5_5}
          AppendBytes(vBytes, DoEncodeMessage(vBytes, ToBytes(AText,
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF})));
{$ELSE}
            AppendBytes(vBytes, DoEncodeMessage(vBytes,
            ToBytes(UTF8Encode(AText))));
{$ENDIF}
{$ENDIF}
          DoWriteBytes(vBytes);
          DoWriteBufferFlush;
{$IFDEF SGC_DEBUG}
          DoLog(self, self, 'DoWriteData_RFC6455',
            '[Text]: ' + AText + ' [OpCode]: ' + TOpCode_String[ord(aOpCode)]);
{$ENDIF}
        Except
          On E: Exception do
          begin
{$IFDEF SGC_DEBUG}
            DoLog(self, self, 'DoWriteData_RFC6455', '[Exception]: ' + E.message
              + ' [Text]: ' + AText);
{$ENDIF}
            raise TsgcWSException.CreateFmt(S_ERROR_WRITING_MESSAGE,
              [E.message]);
          end;
        End;
      Finally
        DoWriteBufferClose;
      End;
    Finally
      DoLeaveCS;
    End;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_RFC6455(aStream: TStream;
  aOpCode: TOpcode; aFragmented: Boolean = False);
var
  vBytes, vBytesMsg: TIdBytes;
  oStream: TStream;
  vFreeStream: Boolean;
begin
  vFreeStream := False;
  // ... initialize stream
  aStream.Position := 0;

  // ... don't compress fragmented messages
  if not IsControlFrame(aOpCode) and not aFragmented and (aOpCode <> opContinuation) then
  begin
    // ... copy stream if modified (maybe is broadcasted)
    oStream := TMemoryStream.Create;
    oStream.CopyFrom(aStream, aStream.Size);
    Try
      DoEnterCS;
      Try
        Extensions.WriteStream(oStream);
      Finally
        DoLeaveCS;
      End;
    Except
      On E: Exception do
        raise TsgcWSException.CreateFmt(S_ERROR_INFLATING_FRAME, [E.message]);
    End;
    vFreeStream := True;
  end
  else
    oStream := aStream;
  // ... seek
  oStream.Position := 0;
  DoEnterCS;
  Try
    // ... write data
    DoWriteBufferOpen;
    Try
      Try
        AppendByte(vBytes, DoEncodeHeader(aOpCode, aFragmented));
        AppendBytes(vBytes, DoEncodePayLoad(oStream.Size));
        if Masked then
          AppendBytes(vBytes, DoEncodeMaskingKey);
        ReadTIdBytesFromStream(oStream, vBytesMsg, oStream.Size);
        AppendBytes(vBytes, DoEncodeMessage(vBytes, vBytesMsg));
        DoWriteBytes(vBytes);
        DoWriteBufferFlush;
{$IFDEF SGC_DEBUG}
        DoLog(self, self, 'DoWriteData_RFC6455',
          '[OpCode]: ' + TOpCode_String[ord(aOpCode)]);
{$ENDIF}
      Except
        On E: Exception do
        begin
{$IFDEF SGC_DEBUG}
          DoLog(self, self, 'DoWriteData_RFC6455', '[Exception]: ' + E.message);
{$ENDIF}
          raise TsgcWSException.CreateFmt(S_ERROR_WRITING_MESSAGE, [E.message]);
        end;
      End;
    Finally
      DoWriteBufferClose;
    End;
  Finally
    if vFreeStream then
      oStream.Free;
    DoLeaveCS;
  End;
end;

function TsgcWSConnection_Base_Indy.GetHeadersRequest: TIdHeaderList;
begin
  if not Assigned(FHeadersRequest) then
{$IFDEF INDY10_5_7}
    FHeadersRequest := TIdHeaderList.Create(QuoteHTTP);
{$ELSE}
    FHeadersRequest := TIdHeaderList.Create;
{$ENDIF}
  Result := FHeadersRequest;
end;

function TsgcWSConnection_Base_Indy.GetWSMessage: TsgcWSMsg;
begin
  if not Assigned(FWSMessage) then
    FWSMessage := TsgcWSMsg.Create;
  Result := FWSMessage;
end;

function TsgcWSConnection_Indy.GetTCPConnection: TIdTCPConnection;
begin
  Result := FTCPConnection;
end;

function TsgcWSConnection_Base_Indy.GetWSControlCode: TsgcWSMsg;
begin
  if not Assigned(FWSControlCode) then
    FWSControlCode := TsgcWSMsg.Create;
  Result := FWSControlCode;
end;

function TsgcWSConnection_Base_Indy.GetWSMSG: TsgcWSMsg;
begin
  if not Assigned(FWSMSG) then
  begin
    if not Disconnected and not FFreed then
      FWSMSG := WSMessage; // not recreate if already disconnected
  end;
  Result := FWSMSG;
end;

function TsgcWSConnection_Base_Indy.DoReadUTF8(aStream: TStream): String;
begin
  if IsValidUTF8(TsgcStringStream(aStream)) then
  begin
    Result := sgcBytesToString(TsgcStringStream(aStream));

    if (Result = '') and (aStream.Size > 0) then
      if (Result <> #0) then // ... x00
        if (TsgcStringStream(aStream).Bytes[aStream.Size - 1] <> $F4) then
          // ... multibyte
          DoDisconnect(S_INVALID_UTF8_MESSAGE, CS_CLOSE_INVALID_PAYLOAD_DATA);
  end
  else
    DoDisconnect(S_INVALID_UTF8_MESSAGE, CS_CLOSE_INVALID_PAYLOAD_DATA)
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData(const AText: string;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  if Enabled then
  begin
    case Transport of
      trpSSE:
        DoWriteData_SSE(AText);
      trpTCP:
        DoWriteData_TCP(AText);
    else
      case Specification of
        spRFC6455:
          begin
            case aStreaming of
              stmNone:
                begin
                  if ((aSize = 0) or (aSize >= Length(AText))) then
                    DoWriteData_RFC6455(AText, opText)
                  else
                    DoWriteData_RFC6455_Fragmented(AText, opText, aSize);
                end;
              stmFirst:
                DoWriteData_RFC6455(AText, opBinary, True);
              stmContinue:
                DoWriteData_RFC6455(AText, opContinuation, True);
              stmLast:
                DoWriteData_RFC6455(AText, opContinuation);
              stmContinueAndLast:
                begin
                  if ((aSize = 0) or (aSize >= Length(AText))) then
                    DoWriteData_RFC6455(AText, opContinuation)
                  else
                    DoWriteData_RFC6455_Fragmented(AText, opText, aSize, True)
                end;
            end;
          end;
        spHixie76:
          DoWriteData_Hixie76(AText, opText);
      end;
    end;
  end
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData(const aStream: TStream;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  if Enabled then
  begin
    case Transport of
      trpTCP:
        DoWriteData_TCP(aStream);
    else
      case Specification of
        spRFC6455:
          begin
            case aStreaming of
              stmNone:
                begin
                  if ((aSize = 0) or (aSize >= aStream.Size)) then
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
                  if ((aSize = 0) or (aSize >= aStream.Size)) then
                    DoWriteData_RFC6455(aStream, opContinuation)
                  else
                    DoWriteData_RFC6455_Fragmented(aStream, opBinary,
                      aSize, True)
                end;
            end;
          end;
      end;
    end;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_RFC6455_Fragmented
  (aStream: TStream; aOpCode: TOpcode; aSize: Integer;
  const aSkipFirst: Boolean = False);
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
    while i < aStream.Size do
    begin
      if aStream.Size - i < aSize then
        oStream.CopyFrom(aStream, aStream.Size - i)
      else
        oStream.CopyFrom(aStream, aSize);
      i := i + oStream.Size;
      if (j = 0) and not(aSkipFirst) then
        DoWriteData_RFC6455(oStream, aOpCode, True)
      else if i >= aStream.Size then
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

procedure TsgcWSConnection_Base_Indy.DoWriteData_RFC6455_Fragmented
  (const AText: string; aOpCode: TOpcode; aSize: Integer;
  const aSkipFirst: Boolean = False);
var
  oStream: TsgcStringStream;
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, self, 'DoWriteData_RFC6455_Fragmented',
    '[Text]: ' + AText + ' [OpCode]: ' + TOpCode_String[ord(aOpCode)]);
{$ENDIF}
  oStream := TsgcStringStream.Create
    ({$IFDEF NEXTGEN}String{$ENDIF}(UTF8Encode(AText)));
  Try
    DoWriteData_RFC6455_Fragmented(TStream(oStream), aOpCode, aSize,
      aSkipFirst);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_SSE(const AText: String);
var
  vBytes: TIdBytes;
  oHeader: TStringList;
  vText: String;
begin
  DoWriteBufferOpen;
  Try
    // ... header
    oHeader := TStringList.Create;
    Try
      oHeader.Add('HTTP/1.1 200 OK');
      oHeader.Add('Content-Type: text/event-stream');
      oHeader.Add
        ('Cache-Control: no-cache, no-store, max-age=0, must-revalidate');
      oHeader.Add('Expires: Fri, 01 Jan 1990 00:00:00 GMT');
      oHeader.Add('Pragma: no-cache');
      oHeader.Add('Connection: close');
      oHeader.Add('');

{$IFDEF LAZARUS}
      AppendBytes(vBytes, sgcStringToBytes(oHeader.Text));
{$ELSE}
{$IFDEF INDY10_5_5}
      AppendBytes(vBytes, ToBytes(oHeader.Text,
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}));
{$ELSE}
        AppendBytes(vBytes, ToBytes(UTF8Encode(oHeader.Text)));
{$ENDIF}
{$ENDIF}
    Finally
      sgcFree(oHeader);
    End;

    // ... data
    vText := AText;
    if (sgcMatchesMask(vText, 'data:*') = False) and
      (sgcMatchesMask(vText, 'retry:*') = False) and
      (sgcMatchesMask(vText, 'event:*') = False) and
      (sgcMatchesMask(vText, 'id:*') = False) then
      vText := 'data: ' + vText;

    // ... write
{$IFDEF LAZARUS}
    AppendBytes(vBytes, DoEncodeMessage(vBytes,
      sgcStringToBytes('data: ' + vText + #13#10)));
{$ELSE}
{$IFDEF INDY10_5_5}
    AppendBytes(vBytes, DoEncodeMessage(vBytes,
      ToBytes('data: ' + vText + #13#10,
{$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF})));
{$ELSE}
      AppendBytes(vBytes, DoEncodeMessage(vBytes,
      ToBytes(UTF8Encode('data: ' + vText + #13#10))));
{$ENDIF}
{$ENDIF}
    DoWriteBytes(vBytes);

    DoWriteBufferFlush;
  Finally
    DoWriteBufferClose;
  End;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteWSMSGBuffer;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oPing: TsgcPing;
  vPong: String;
  vDisconnectByPing: Boolean;
begin
  // ... apply extension if needed
  DoEnterCS;
  Try
    Try
      Extensions.ReadStream(WSMSG.FStream);
    Except
      On E: Exception do
        raise TsgcWSException.CreateFmt(S_ERROR_DEFLATING_FRAME, [E.message]);
    End;
  Finally
    DoLeaveCS;
  End;
  WSMSG.Stream.Position := 0;

  DoLogRead(WSMSG.OpCode, WSMSG.Stream);

  case WSMSG.OpCode of
    opText:
      DoMessageEvent(DoReadUTF8(WSMSG.Stream));
    opBinary:
      DoBinaryEvent(TMemoryStream(WSMSG.Stream));
    opClose:
      DoDisconnect;
    opPing:
      begin
{$IFDEF SGC_DEBUG}
        DoLog(self, self, 'Send Pong');
{$ENDIF}
        DoWriteData_RFC6455(WSMSG.Stream, opPong);
      end;
    opPong:
      begin
        LastPong := Now;
        // ... initalize
        vDisconnectByPing := False;
        // ... search Ping
        oList := QueuePingList.LockList;
        Try
          if oList.count = 0 then // do nothing on unsolicited pong
            WSMSG.Clear
          else
          begin
            // ... initialize
            vDisconnectByPing := True;
            // ... search Ping in list
            vPong := DoReadUTF8(WSMSG.Stream);
            for i := 0 to oList.count - 1 do
            begin
              if TsgcPing(oList.Items[i]).Text = vPong then
              begin
{$IFDEF SGC_DEBUG}
                DoLog(self, self, 'Received Pong', '[Pong]: ' + vPong);
{$ENDIF}
                oPing := TsgcPing(oList.Items[0]);
                sgcFree(oPing);
                oList.Delete(0);
                FPong := vPong;
                vDisconnectByPing := False;
                break;
              end;
            end;
          end;
        Finally
          QueuePingList.UnlockList;
        End;
        // ... if Ping not found in queue
        if vDisconnectByPing then
        begin
{$IFDEF SGC_DEBUG}
          DoLog(self, self, 'Received Pong not found', '[Pong]: ' + vPong);
{$ENDIF}
          // ... clear
          WSMSG.Clear;
          // ... raise
          DoDisconnect(S_PONG_DIFFERENT_PING);
        end;
      end;
  end;
  if Assigned(FWSMSG) then
    WSMSG.Clear;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteWSMSGFragmented;
begin
  if FragmentedMessages in [frgOnlyFragmented, frgAll] then
  begin
    // ... apply extension if needed
    DoEnterCS;
    Try
      Try
        Extensions.ReadStream(WSMSG.FStream);
      Except
        On E: Exception do
          raise TsgcWSException.CreateFmt(S_ERROR_DEFLATING_FRAME, [E.message]);
      End;
    Finally
      DoLeaveCS;
    End;
    WSMSG.Stream.Position := 0;

    // ... event
    DoFragmentedEvent(TMemoryStream(WSMSG.Stream), WSMSG.OpCode,
      (WSMSG.Fragmented and not WSMSG.Continuation) or
      (WSMSG.Fragmented and WSMSG.Continuation));
  end;
  // ... clear
  WSMSG.State := 0;
  WSMSG.PLState := 0;
  if FragmentedMessages in [frgOnlyFragmented] then
    WSMSG.ClearStream;
end;

function TsgcWSConnection_Indy.GetIP: String;
begin
  if (FIP = '') and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
        FIP := TCPConnection.Socket.Binding.PeerIP;
    except
      // nothing
    end;
  end;
  Result := FIP;
end;

function TsgcWSConnection_Indy.GetLocalIP: String;
begin
  if (FLocalIP = '') and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
        FLocalIP := TCPConnection.Socket.Binding.IP;
    except
      // nothing
    end;
  end;
  Result := FLocalIP;
end;

function TsgcWSConnection_Indy.GetLocalPort: Integer;
begin
  if (FLocalPort = 0) and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
        FLocalPort := TCPConnection.Socket.Binding.Port;
    except
      // nothing
    end;
  end;
  Result := FLocalPort;
end;

function TsgcWSConnection_Indy.GetPort: Integer;
begin
  if (FPort = 0) and Assigned(TCPConnection) then
  begin
    if Assigned(TCPConnection.Socket) then
    begin
      Try
        FPort := TCPConnection.Socket.Binding.PeerPort;
      Except
        // nothing
      End;
    end;
  end;

  Result := FPort;
end;

function TsgcWSConnection_Base_Indy.GetQueuePingList: TsgcThreadListQueuePings;
begin
  if not Assigned(FQueuePingList) then
    FQueuePingList := TsgcThreadListQueuePings.Create;
  Result := FQueuePingList;
end;

function TsgcWSConnection_Base_Indy.IsControlFrame(aOpCode: TOpcode): Boolean;
begin
  Result := aOpCode in [opClose, opPing, opPong];
end;

function TsgcWSConnection_Base_Indy.IsValidUTF8
  (aStream: TsgcStringStream): Boolean;
var
  i, j: Integer;
begin
  Result := True;

  if not ValidateUTF8 then
    exit;

  if aStream.Size = 0 then
    exit;

  Result := False;

  i := 0;
  j := aStream.Size;
  while i < j do
  begin
    case aStream.Bytes[i] of
      $00 .. $7F:
        i := i + 1;

      $C2 .. $DF:
        if (i + 1 < j) and ((aStream.Bytes[i + 1]) in [$80 .. $BF]) then
          i := i + 2
        else
          break;

      $E0:
        if (i + 2 < j) and ((aStream.Bytes[i + 1]) in [$A0 .. $BF]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) then
          i := i + 3
        else
          break;

      $E1 .. $EC:
        if (i + 2 < j) and ((aStream.Bytes[i + 1]) in [$80 .. $BF]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) then
          i := i + 3
        else
          break;

      $ED:
        if (i + 2 < j) and ((aStream.Bytes[i + 1]) in [$80 .. $9F]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) then
          i := i + 3
        else
          break;

      $EE .. $EF:
        if (i + 2 < j) and ((aStream.Bytes[i + 1]) in [$80 .. $BF]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) then
          i := i + 3
        else
          break;

      $F0:
        if (i + 3 < j) and ((aStream.Bytes[i + 1]) in [$90 .. $BF]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) and
          ((aStream.Bytes[i + 3]) in [$80 .. $BF]) then
          i := i + 4
        else
          break;

      $F1 .. $F3:
        if (i + 3 < j) and ((aStream.Bytes[i + 1]) in [$80 .. $BF]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) and
          ((aStream.Bytes[i + 3]) in [$80 .. $BF]) then
          i := i + 4
        else
          break;

      $F4:
        if (i + 3 < j) and ((aStream.Bytes[i + 1]) in [$80 .. $8F]) and
          ((aStream.Bytes[i + 2]) in [$80 .. $BF]) and
          ((aStream.Bytes[i + 3]) in [$80 .. $BF]) then
          i := i + 4
        else if (i + 1 = j) then
          i := i + 1
        else
          break;
    else
      exit;
    end;
  end;

  if i = j then
    Result := True;
end;

procedure TsgcWSConnection_Base_Indy.DoPing(const AText: string);
var
  oPing: TsgcPing;
begin
  inherited;
  // ... add ping to queue
  if Transport in [trpRFC6455, trpRFC8441, trpFlash] then
  begin
    oPing := TsgcPing.Create;
    oPing.Text := AText;
    oPing.Date := Now;

    QueuePingList.Add(oPing)
  end;

  // ... send ping
  case Transport of
    trpRFC6455, trpRFC8441, trpFlash:
      DoWriteData_RFC6455(AText, opPing);
    trpSSE:
      DoWriteData_SSE('event: Ping' + Char(10) + 'data: ' + AText);
    trpTCP:
      DoWriteData_TCP(AText);
{$IFDEF SGC_HTTP2}
    trpHTTP2:
      begin
        HTTP2FrameWrite.Frame_Ping.ACK := False;
        HTTP2FrameWrite.Frame_Ping.WriteBytes;
      end;
{$ENDIF}
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoReadData_TCP;
var
  vBuffer: TIdBytes;
  vSize, vLength: Integer;
  i, j: Integer;
begin
  vSize := GetReadBufferSize;
  DoReadBytes(vBuffer, vSize);

  case TCPEndOfFrameScanBuffer of
    eofScanAllBytes:
      begin
        sgcAddBytes(vBuffer, WSMSG.FReadBuffer);
        vSize := Length(WSMSG.ReadBuffer);
      end
  else
    WSMSG.Stream.Write(vBuffer[0], vSize);
  end;

  vLength := Length(FTCPEndOfFrame);
  case TCPEndOfFrameScanBuffer of
    eofScanNone:
      begin
        WSMSG.Stream.Position := 0;
        DoBinaryEvent(TMemoryStream(WSMSG.Stream));
        WSMSG.ClearStream;
      end;
    eofScanLatestBytes:
      begin
        if vSize >= vLength then
        begin
          for i := vLength - 1 downto 0 do
          begin
            if (vBuffer[vSize - i - 1] <> FTCPEndOfFrame[vLength - i - 1]) then
              break
            else
            begin
              if i = 0 then
              begin
                WSMSG.Stream.Position := 0;
                DoBinaryEvent(TMemoryStream(WSMSG.Stream));
                WSMSG.ClearStream;
              end;
            end;
          end;
        end;
      end;
    eofScanAllBytes:
      begin
        if vSize >= vLength then
        begin
          for i := 0 to Length(WSMSG.ReadBuffer) - 1 do
          begin
            for j := 0 to vLength - 1 do
            begin
              if WSMSG.ReadBuffer[i + j] <> FTCPEndOfFrame[j] then
                break
              else
              begin
                if j = vLength - 1 then
                begin
                  WSMSG.Stream.Write(WSMSG.ReadBuffer[0], i + j + 1);
                  WSMSG.Stream.Position := 0;
                  DoBinaryEvent(TMemoryStream(WSMSG.Stream));
                  // no more data, clear stream
                  if i + j + 1 = Length(WSMSG.ReadBuffer) then
                  begin
                    WSMSG.Clear;
                    exit;
                  end
                  // remaining data
                  else
                  begin
                    // resize ReadBuffer
                    SetLength(vBuffer, 0);
                    SetLength(vBuffer, Length(WSMSG.ReadBuffer) - i - j - 1);
                    CopyTIdBytes(WSMSG.ReadBuffer, i + j + 1, vBuffer, 0,
                      Length(WSMSG.ReadBuffer) - i - j - 1);
                    WSMSG.Clear;
                    sgcAddBytes(vBuffer, WSMSG.FReadBuffer);
                    // read remaining data
                    DoReadData_TCP;
                    exit;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_TCP(const AText: String);
var
  vBytes: TIdBytes;
begin
  DoEnterCS;
  Try
    SetLength(vBytes, 0);

    DoWriteBufferOpen;
    Try
      // ... write
  {$IFDEF LAZARUS}
      AppendBytes(vBytes, sgcStringToBytes(AText));
  {$ELSE}
  {$IFDEF INDY10_5_5}
      AppendBytes(vBytes, ToBytes(AText,
  {$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF}));
  {$ELSE}
        AppendBytes(vBytes, ToBytes(UTF8Encode(AText)));
  {$ENDIF}
  {$ENDIF}
      DoWriteBytes(vBytes);

      DoWriteBufferFlush;
    Finally
      DoWriteBufferClose;
    End;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSConnection_Base_Indy.DoWriteData_TCP(const aStream: TStream);
var
  vBytes: TIdBytes;
begin
  DoEnterCS;
  Try
    aStream.Position := 0;
    DoWriteBufferOpen;
    Try
      // ... write
      SetLength(vBytes, aStream.Size);
      aStream.Read(vBytes[0], aStream.Size);
      DoWriteBytes(vBytes);
      // TCPConnection.IOHandler.WriteLn;

      DoWriteBufferFlush;
    Finally
      DoWriteBufferClose;
    End;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcWSConnection_Indy.SetTCPConnection(const Value: TIdTCPConnection);
begin
  if Assigned(self) then
  begin
    FTCPConnection := Value;
    if Value <> nil then
    begin
      GetIP;
      GetPort;
    end;
  end;
end;

procedure TsgcWSConnection_Base_Indy.ClearPingList;
{$IFNDEF NEXTGEN}
var
  i: Integer;
  oList: TList;
  oObject: TObject;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
  oList := QueuePingList.LockList;
  Try
    for i := oList.count - 1 Downto 0 do
    begin
      oObject := oList[i];
      sgcTryFree(oObject);
      oList.Delete(i);
    end;
  Finally
    QueuePingList.UnlockList;
  End;
{$ENDIF}
end;

procedure TsgcWSConnection_Base_Indy.DoReadData_RFC8441;
var
  vBuffer: TIdBytes;
  vSize: Integer;
begin
  vSize := GetReadBufferSize;
  DoReadBytes(vBuffer, vSize);
end;

procedure TsgcWSConnection_Base_Indy.DoHTTPResponse(const aContent,
    aContentType: String; const aCustomHeaders: TStrings = nil;
    aStreamIdentifier: Integer = 0);
var
  oStream: TsgcStringStream;
begin
  oStream := TsgcStringStream.Create(aContent);
  Try
    DoHTTPResponse(oStream, aContentType, aCustomHeaders, aStreamIdentifier);
  Finally
  // ... don't free the stream
  // ... it's freed internal
//    sgcFree(oStream);
  End;
end;

procedure TsgcWSConnection_Base_Indy.DoLogRead(aOpCode: TOpcode;
  const aStream: TStream);
begin
  // nothing
end;

procedure TsgcWSConnection_Base_Indy.DoLogWrite(aOpCode: TOpcode;
  const AText: String; aFragmented: Boolean = False);
begin

end;

procedure TsgcWSConnection_Base_Indy.DoLogWrite(aOpCode: TOpcode;
  const aStream: TStream; aFragmented: Boolean = False);
begin

end;

function TsgcWSConnection_Base_Indy.GetForward_HTTP: TsgcWSServerForwardHTTP;
begin
  if not Assigned(FForward_HTTP) then
    FForward_HTTP := TsgcWSServerForwardHTTP.Create;
  Result := FForward_HTTP;
end;

{$IFDEF SGC_HTTP2}
{$ENDIF}

function TsgcWSConnection_Base_Indy.GetPayloadReadLength: Integer;
begin
  Result := WSMSG.PayLoadLength;
end;

procedure TsgcWSConnection_Base_Indy.SetWSMSG(const Value: TsgcWSMsg);
begin
  FWSMSG := Value;
end;

constructor TsgcWSConnection_Indy.Create;
begin
  inherited;
  FReadTimeOut := 10;
end;

destructor TsgcWSConnection_Indy.Destroy;
begin
  FTCPConnection := nil;
  inherited;
end;

procedure TsgcWSConnection_Indy.DisconnectPeer(aCallOnDisconnectIfClosed:
    Boolean = False);
begin
  if Assigned(TCPConnection) then
  begin
    if Assigned(TCPConnection.IOHandler) then
    begin
      Disconnected := True;

      if Assigned(FQueueLevel2) then
        FQueueLevel2.FreeObjects;
      if Assigned(FQueueLevel1) then
        FQueueLevel1.FreeObjects;
      if Assigned(FQueueLevel0) then
        FQueueLevel0.FreeObjects;

      // don't read ssl messages if disconnected
      TCPConnection.IOHandler.WriteBufferClear;
      TCPConnection.IOHandler.InputBuffer.Clear;

{$IFNDEF INDY10_6_2_D10_4} // from this indy version, setting passtrhough := True, locks the thread
{$IFNDEF NEXTGEN}
      if TCPConnection.IOHandler is TIdSSLIOHandlerSocketBase then
        TIdSSLIOHandlerSocketBase(TCPConnection.IOHandler).PassThrough := True;
      // don't read ssl messages if disconnected
{$ENDIF}
{$ENDIF}
      Try
        // ... if IOHandler is already closed, notify the disconnection
        // ... otherwise OnDisconnect event maybe it's not called
        if aCallOnDisconnectIfClosed and (TCPConnection.IOHandler.Opened = False) then
          TCPConnection.OnStatus(nil, hsDisconnected, '')
        else
          TCPConnection.Disconnect(False);
      Except
        On E: EIdSilentException do
        begin
          if RaiseDisconnectExceptions then
            raise;
        end;
        On E: Exception do
          raise;
      End;
{$IFDEF SGC_DEBUG}
      DoLog(self, self, 'DisconnectPeer');
{$ENDIF}
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoAfterReadData;
begin
{$IFDEF SGC_HTTP2}
  case Transport of
    trpHTTP2:
      DoHTTP2SendPushPromisePending;
  end;
{$ENDIF}
end;

procedure TsgcWSConnection_Indy.DoAvoidLogEnd;
begin
  if Assigned(TCPConnection.IOHandler.Intercept) then
  begin
    if TCPConnection.IOHandler.Intercept.ClassType = TsgcWSIdLogFileClient then
      TsgcWSIdLogFileClient(TCPConnection.IOHandler.Intercept).FSentData := True;
  end;
end;

procedure TsgcWSConnection_Indy.DoAvoidLogStart;
begin
  if Assigned(TCPConnection.IOHandler.Intercept) then
  begin
    if TCPConnection.IOHandler.Intercept.ClassType = TsgcWSIdLogFileClient then
      TsgcWSIdLogFileClient(TCPConnection.IOHandler.Intercept).FSentData := False;
  end;
end;

procedure TsgcWSConnection_Indy.DoClearBuffer;
begin
  inherited;
  if Assigned(TCPConnection) then
  begin
    if not TCPConnection.IOHandler.InputBufferIsEmpty then
      TCPConnection.IOHandler.InputBuffer.Clear;
  end;
end;

procedure TsgcWSConnection_Indy.DoHTTPError(aCode: Integer = 404;
  aDescription: string = 'Not Found');
begin
  inherited;
  case Transport of
    trpHTTP2:
      begin
{$IFDEF SGC_HTTP2}
        DoHTTP2Error(aCode, aDescription);
{$ENDIF}
      end
  else
    begin
      TCPConnection.IOHandler.WriteBufferOpen;
      Try
        TCPConnection.IOHandler.WriteLn(Format('HTTP/1.1 %d %s',
          [aCode, aDescription]));
        TCPConnection.IOHandler.WriteLn('');

        TCPConnection.IOHandler.WriteBufferFlush;
      Finally
        TCPConnection.IOHandler.WriteBufferClose;
      End;
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoHTTPRedirect(const aURL: string);
begin
  inherited;
  case Transport of
    trpHTTP2:
      begin
{$IFDEF SGC_HTTP2}
        DoHTTP2Redirect(aURL);
{$ENDIF}
      end
  else
    begin
      TCPConnection.IOHandler.WriteBufferOpen;
      Try
        TCPConnection.IOHandler.WriteLn('HTTP/1.1 302 Moved Temporarily');
        TCPConnection.IOHandler.WriteLn('Connection: close');
        TCPConnection.IOHandler.WriteLn('Location: ' + aURL);
        TCPConnection.IOHandler.WriteLn('Server: ' + CS_APPLICATION_NAME + ' ' +
          CS_VERSION);
        TCPConnection.IOHandler.WriteLn;
        TCPConnection.IOHandler.WriteBufferFlush;
      Finally
        TCPConnection.IOHandler.WriteBufferClose;
      End;
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoHTTPResponse(const aStream: TStream;
    aContentType: String; const aCustomHeaders: TStrings = nil;
    aStreamIdentifier: Integer = 0);
var
  i: Integer;
begin
  inherited;
  case Transport of
    trpHTTP2:
      begin
{$IFDEF SGC_HTTP2}
        DoHTTP2Response(aStream, aContentType, aCustomheaders, aStreamIdentifier);
{$ENDIF}
      end
  else
    begin
      TCPConnection.IOHandler.WriteBufferOpen;
      Try
        TCPConnection.IOHandler.WriteLn('HTTP/1.1 200 OK');
        TCPConnection.IOHandler.WriteLn('Cache-Control: no-cache');
        TCPConnection.IOHandler.WriteLn('Content-Type: ' + aContentType);
        if AltSvc <> '' then
          TCPConnection.IOHandler.WriteLn(AltSvc);
        if Assigned(aCustomHeaders) then
        begin
          for i := 0 to aCustomheaders.Count - 1 do
            TCPConnection.IOHandler.WriteLn(aCustomHeaders[i]);
        end;
        TCPConnection.IOHandler.WriteLn('Server: ' + CS_APPLICATION_NAME + ' ' +
          CS_VERSION);

        TCPConnection.IOHandler.WriteLn;
        TCPConnection.IOHandler.Write(aStream);
//        TCPConnection.IOHandler.WriteLn;
        TCPConnection.IOHandler.WriteBufferFlush;
      Finally
        TCPConnection.IOHandler.WriteBufferClose;
        {$IFDEF D10_4}
        sgcFree(aStream);
        {$ELSE}
          if Assigned(aStream) then
          begin
            {$IFDEF NEXTGEN}
            aStream.DisposeOf;
            {$ELSE}
            aStream.Free;
            {$ENDIF}
          end;
        {$ENDIF}
      End;
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoLogRead(aOpCode: TOpcode;
  const aStream: TStream);
var
  oStream: TsgcStringStream;
begin
  inherited;
  if Assigned(TCPConnection.IOHandler.Intercept) then
  begin
    if TCPConnection.IOHandler.Intercept.ClassType = TsgcWSIdLogFileServerConnection
    then
    begin
      if TsgcWSIdLogFileServerConnection(TCPConnection.IOHandler.Intercept).UnMaskFrames then
      begin
        oStream := TsgcStringStream.Create('');
        Try
          oStream.CopyFrom(aStream, aStream.size);
          aStream.Position := 0;
          TsgcWSIdLogFileServerConnection(TCPConnection.IOHandler.Intercept)
            .LogRecvDecodedString(Format('[%s] %s', [GetOpCodeString(aOpCode),
            oStream.DataString]));
        Finally
          sgcFree(oStream);
        End;
      end;
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoLogWrite(aOpCode: TOpcode;
  const AText: String; aFragmented: Boolean = False);
var
  vText: string;
begin
  inherited;
  if Assigned(TCPConnection.IOHandler.Intercept) then
  begin
    if TCPConnection.IOHandler.Intercept.ClassType = TsgcWSIdLogFileClient then
    begin
      if TsgcWSIdLogFileClient(TCPConnection.IOHandler.Intercept).UnMaskFrames then
      begin
        vText := Format('[%s] %s', [GetOpCodeString(aOpCode), AText]);
        if aFragmented then
          vText := '[fragmented] ' + vText;
        TsgcWSIdLogFileClient(TCPConnection.IOHandler.Intercept)
          .LogSendDecodedString(vText);
      end;
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoLogWrite(aOpCode: TOpcode;
  const aStream: TStream; aFragmented: Boolean = False);
var
  oStream: TsgcStringStream;
  vText: string;
begin
  inherited;
  if Assigned(TCPConnection.IOHandler.Intercept) then
  begin
    if TCPConnection.IOHandler.Intercept.ClassType = TsgcWSIdLogFileClient then
    begin
      if TsgcWSIdLogFileClient(TCPConnection.IOHandler.Intercept).UnMaskFrames then
      begin
        oStream := TsgcStringStream.Create('');
        Try
          aStream.Position := 0;
          oStream.CopyFrom(aStream, aStream.Size);
          vText := Format('[%s] %s', [GetOpCodeString(aOpCode),
            oStream.DataString]);
          if aFragmented then
            vText := '[fragmented] ' + vText;
          TsgcWSIdLogFileClient(TCPConnection.IOHandler.Intercept)
            .LogSendDecodedString(vText);
        Finally
          sgcFree(oStream);
        End;
      end;
    end;
  end;
end;

procedure TsgcWSConnection_Indy.DoWriteBufferClose;
begin
  inherited;
  TCPConnection.IOHandler.WriteBufferClose;
  DoAvoidLogEnd;
end;

procedure TsgcWSConnection_Indy.DoWriteBufferFlush;
begin
  inherited;
  TCPConnection.IOHandler.WriteBufferFlush;
end;

procedure TsgcWSConnection_Indy.DoWriteBufferOpen;
begin
  DoAvoidLogStart;
  inherited;
  TCPConnection.IOHandler.WriteBufferOpen;
end;

procedure TsgcWSConnection_Indy.DoWriteData_RFC6455(const AText: string;
  aOpCode: TOpcode; aFragmented: Boolean = False);
begin
  inherited;
  DoLogWrite(aOpCode, AText);
end;

procedure TsgcWSConnection_Indy.DoWriteData_RFC6455(aStream: TStream;
  aOpCode: TOpcode; aFragmented: Boolean = False);
begin
  inherited;
  DoLogWrite(aOpCode, aStream);
end;

procedure TsgcWSConnection_Indy.DoWriteData_RFC6455_Fragmented
  (const AText: string; aOpCode: TOpcode; aSize: Integer;
  const aSkipFirst: Boolean = False);
begin
  inherited;
  DoLogWrite(aOpCode, AText);
end;

procedure TsgcWSConnection_Indy.DoWriteData_RFC6455_Fragmented(aStream: TStream;
  aOpCode: TOpcode; aSize: Integer; const aSkipFirst: Boolean = False);
begin
  inherited;
  DoLogWrite(aOpCode, aStream);
end;

procedure TsgcWSConnection_Indy.DoWriteLn(const AText: String = '');
begin
  inherited;
  if AText <> '' then
    TCPConnection.IOHandler.WriteLn(AText)
  else
    TCPConnection.IOHandler.WriteLn;
end;

function TsgcWSConnection_Indy.ExistsTCPConnection: Boolean;
begin
  Result := Assigned(FTCPConnection);
end;

function TsgcWSConnection_Indy.GetActive: Boolean;
begin
  Result := False;
  if Assigned(TCPConnection) then
  begin
    Try
      if TCPConnection.IOHandler <> nil then
        Result := TCPConnection.Connected;
    Except
      // nothing
    End;
  end;
end;

function TsgcWSConnection_Indy.GetIPVersion: TwsIPVersion;
begin
  if (FIPVersion = ipV4) and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
      begin
        case TCPConnection.Socket.Binding.IPVersion of
          Id_IPv4: FIPVersion := ipV4;
          Id_IPv6: FIPVersion := ipV6;
        end;
      end;
    except
      // nothing
    end;
  end;
  Result := FIPVersion;
end;

function TsgcWSConnection_Indy.GetOpCodeString(aOpCode: TOpcode): string;
begin
  case aOpCode of
    opContinuation:
      Result := 'continuation';
    opText:
      Result := 'text';
    opBinary:
      Result := 'binary';
    opClose:
      Result := 'close';
    opPing:
      Result := 'ping';
    opPong:
      Result := 'pong';
  else
    Result := 'none';
  end
end;

function TsgcWSConnection_Indy.GetReadBufferSize: Integer;
begin
  Result := TCPConnection.IOHandler.InputBuffer.Size;
end;

constructor TsgcWSMsg.Create;
begin
  Clear;
end;

destructor TsgcWSMsg.Destroy;
begin
  ClearStream;
  inherited;
end;

procedure TsgcWSMsg.Clear;
begin
  ClearStream;

  Continuation := False;
  Fragmented := False;
  MaskIndex := 0;
  OpCode := opNone;
  PayLoad := pa7bits;
  PayLoadLength := 0;
  PLState := 0;
  ReadMasked := False;
  State := 0;
  ClearReadBuffer;
end;

procedure TsgcWSMsg.ClearReadBuffer;
begin
  SetLength(FReadBuffer, 0);
end;

procedure TsgcWSMsg.ClearStream;
begin
  sgcFree(FStream);
end;

procedure TsgcWSMsg.Free;
begin
  if self <> nil then
    Destroy;
end;

function TsgcWSMsg.GetStream: TStream;
begin
  if not Assigned(FStream) then
  begin
    case OpCode of
      opBinary:
        FStream := TMemoryStream.Create;
    else
      FStream := TsgcStringStream.Create('');
    end;
  end;
  Result := FStream;
end;

destructor TsgcWSComponent_Client_Indy.Destroy;
begin
  sgcFree(FThrottle);
  inherited;
end;

function TsgcWSComponent_Client_Indy.GetThrottle: TsgcWSThrottle;
begin
  if not Assigned(FThrottle) then
    FThrottle := TsgcWSThrottle.Create;
  Result := FThrottle;
end;

procedure TsgcWSComponent_Client_Indy.SetThrottle(const Value: TsgcWSThrottle);
begin
  Throttle.Assign(Value);
end;

destructor TsgcWSServerForwardHTTP.Destroy;
begin
  sgcFree(FTLSOptions);
  inherited;
end;

function TsgcWSServerForwardHTTP.GetForwardURL: string;
begin
  Result := URL + Document;
end;

function TsgcWSServerForwardHTTP.GetTLSOptions: TsgcTCPTLS_Options;
begin
  if not Assigned(FTLSOptions) then
    FTLSOptions := TsgcTCPTLS_Options.Create;
  Result := FTLSOptions;
end;

procedure TsgcWSIdLogFileServerConnection.LogRecvDecodedString
  (const AText: string);
begin
  FServerInterceptLog.LogWriteString(GetConnectionID + ' ' + '<<-- ' +
    DateTimeToStr(Now) + ': ' + AText + EOL);
end;

function TsgcWSIdLogFileServer.Accept(aConnection: TComponent)
  : TIdConnectionIntercept;
begin
  Result := TsgcWSIdLogFileServerConnection.Create(nil);
  TsgcWSIdLogFileServerConnection(Result).FServerInterceptLog := self;
  TsgcWSIdLogFileServerConnection(Result).LogTime := FLogTime;
  TsgcWSIdLogFileServerConnection(Result).ReplaceCRLF := FReplaceCRLF;
  TsgcWSIdLogFileServerConnection(Result).UnMaskFrames := UnMaskFrames;
  TsgcWSIdLogFileServerConnection(Result).Active := True;
end;

procedure TsgcWSIdLogFileClient.InitComponent;
begin
  FSentData := True;
  inherited;
end;

procedure TsgcWSIdLogFileClient.LogSentData({$IFDEF INDY10_2}const {$ENDIF}AText, AData: string);
begin
  if (UnMaskFrames = False) or FSentData then
    inherited LogSentData(AText, AData);
end;

end.
