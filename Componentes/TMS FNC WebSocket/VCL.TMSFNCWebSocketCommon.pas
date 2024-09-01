{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2022                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit VCL.TMSFNCWebSocketCommon;

interface

uses
  Classes, SysUtils, IniFiles, IdContext, idIOHandler, IdTCPConnection,
  IdCustomHTTPServer,
  Generics.Collections, IdHTTPServer, IdScheduler, IdSchedulerOfThreadDefault,
  IdHeaderList,
  IdSSLOpenSSL, idGlobal, IdException;

const
  Fin: Byte = $80;
  TwoBytesLengthCode: Byte = 126;
  EightBytesLengthCode: Byte = 127;
  DefaultPort = 8888;
  CurrentWebSocketVersion = 13;

type
  EWebSocket = class(Exception);

  TTMSFNCWebSocketBitConverter = class
    class function InTo<T>(Buffer: TArray<Byte>; Index: Integer): T;
    class procedure From<T>(Value: T; var Buffer: TArray<Byte>);
  end;

  TTMSFNCWebSocketTLSVersion = (wstTLS1, wstTLS1_1, wstTLS1_2);
  TTMSFNCWebSocketTLSVersions = set of TTMSFNCWebSocketTLSVersion;

  TTMSFNCWebSocketFrameType = (ftFirst, ftContinuation, ftLast);
  TTMSFNCWebSocketFrameTypes = set of TTMSFNCWebSocketFrameType;

  TTMSFNCWebSocketFrameOpcode = (focContinuation = $0, focText = $1,
    focBinary = $2, focClose = $8, focPing = $9, focPong = $A);

  TTMSFNCWebSocketFrame = class
  private
    FOpcode: TTMSFNCWebSocketFrameOpcode;
    FIsFin: Boolean;
    FPayloadLength: Uint64;
    FUnmaskedPayload: TBytes;
    FMaskingKey: Integer;
    FIsMasked: Boolean;
    FRSV: Byte;
  protected
    procedure ReadFromBuffer(var Buffer: TBytes;
      aIOHandler: TIdIOHandler); virtual;
    procedure ReadPayload(var Content: TBytes; aIOHandler: TIdIOHandler;
      aOffset: Integer); virtual;
  public
    constructor Create(AOpcode: TTMSFNCWebSocketFrameOpcode; APayload: TBytes; AIsFin: Boolean); overload; virtual;
    constructor Create(AOpcode: TTMSFNCWebSocketFrameOpcode); overload; virtual;
    constructor Create(const aMessage: string); overload; virtual;
    function ToBuffer: TBytes;
    class function FromBuffer(var Buffer: TBytes; aIOHandler: TIdIOHandler): TTMSFNCWebSocketFrame;
    class function UnMask(var payload: TBytes; Key: Integer; Offset: Integer = 0): TBytes;
    property RSV: Byte read FRSV write FRSV;
    property IsFin: Boolean read FIsFin write FIsFin;
    property IsMasked: Boolean read FIsMasked write FIsMasked;
    property PayloadLength: UInt64 read FPayloadLength;
    property MaskingKey: Integer read FMaskingKey;
    property UnmaskedPayload: TBytes read FUnmaskedPayload write FUnmaskedPayload;
    property Opcode: TTMSFNCWebSocketFrameOpcode read FOpcode;
  end;

  TTMSFNCWebSocketFrameClass = class of TTMSFNCWebSocketFrame;

  TTMSFNCWebSocketConnection = class;

  TTMSFNCWebSocketBinDataEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aData: TBytes; AFrameType: TTMSFNCWebSocketFrameTypes) of object;
  TTMSFNCWebSocketControlEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aData: TBytes) of object;
  TTMSFNCWebSocketMessageEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aMessage: string) of object;
  TTMSFNCWebSocketConnectEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketConnection) of object;
  TTMSFNCWebSocketNotifyEvent = procedure(Sender: TObject; AConnection: TTMSFNCWebSocketConnection) of object;
  TTMSFNCWebSocketDisconnectEvent = TTMSFNCWebSocketNotifyEvent;
  TTMSFNCWebSocketException = class(Exception);
  TTMSFNCWebSocketHandshakeException = class(TTMSFNCWebSocketException);

  TTMSFNCWebSocketRequest = class
  private
    FHost: string;
    FOrigin: string;
    FKey: string;
    FProtocol: string;
    FVersion: String;
    FResource: string;
    FExtensions: String;
    FRawHeaders: TStrings;
    FConnection: string;
    FUpgrade: string;
  protected
  public
    constructor Create(aResource: string; const AHeaderList: TStrings);
    Destructor Destroy; override;
    Property RawHeaders: TStrings read FRawHeaders;
    property Resource: string read FResource write FResource;
    property Host: string read FHost write FHost;
    property Origin: string read FOrigin write FOrigin;
    property Connection: string read FConnection write FConnection;
    property Upgrade: string read FUpgrade write FUpgrade;
    property Protocol: string read FProtocol write FProtocol;
    property Version: string read FVersion write FVersion;
    property Extensions: string read FExtensions write FExtensions;
    property Key: string read FKey write FKey;
  end;

  TTMSFNCWebsocketOption = (twsoFrameByFrame, // Do not assemble frames
    twsoSkipUpgradeCheck, // Do not check handshake Upgrade header
    twsoSkipVersionCheck, // Do not check handshake version
    twsoManualPong, // Do not send Pong automatically
    twsoManualClose // Do not send close reply automatically
    );

  TTMSFNCWebsocketOptions = set of TTMSFNCWebsocketOption;

  TTMSFNCWebSocketConnection = class
  private
    FInitialOpcode: TTMSFNCWebSocketFrameOpcode;
    FMessageContent: TBytes;
    FHandshakeRequest: TTMSFNCWebSocketRequest;
    FOnMessageReceived: TTMSFNCWebSocketMessageEvent;
    FOnBinaryDataReceived: TTMSFNCWebSocketBinDataEvent;
    FOptions: TTMSFNCWebsocketOptions;
    FOnPing: TTMSFNCWebSocketControlEvent;
    FOnPong: TTMSFNCWebSocketControlEvent;
    FOnClose: TTMSFNCWebSocketControlEvent;
    FCloseSent: Boolean;
    FWebSocketVersion: Integer;
    procedure DoFrameEvents(aFrame: TTMSFNCWebSocketFrame);
  protected const
    FRAME_START = #$00;
    FRAME_SIZE_START = #$80;
    FRAME_END = #$FF;
    procedure SetHandShakeRequest(aRequest: TTMSFNCWebSocketRequest);
    function GetPeerIP: string; virtual; abstract;
    function GetConnection: TIDTCPConnection; virtual; abstract;
    function HandleFrame(aFrame: TTMSFNCWebSocketFrame): Boolean; virtual;
    function HandleAllPendingFrames(ABuffer: TBytes; aIOHandler: TIdIOHandler): Boolean; virtual;
    function GetHandshakeCompleted: Boolean; virtual; abstract;
    property Connection: TIDTCPConnection read GetConnection;
  public
    constructor Create(aOptions: TTMSFNCWebsocketOptions); virtual;
    destructor Destroy; override;
    // Descendents can override this to provide custom frames
    function GetFrameClass: TTMSFNCWebSocketFrameClass; virtual;
    // Send raw frame. No checking is done !
    procedure SendFrame(aFrame: TTMSFNCWebSocketFrame); virtual;
    // Send message
    procedure Send(const aMessage: string; const AMasked: Boolean = False);
    // Send binary data
    procedure SendBytes(const ABytes: TBytes; const AMasked: Boolean = False);
    // Send message in multiple frames
    procedure SendInMultipleFrames(const AMessage: string; AFrameLength: UInt64; const AMasked: Boolean = False);
    procedure SendBytesInMultipleFrames(var ABytes: TBytes; AFrameLength: UInt64; const AMasked: Boolean = False);
    // Send control frame. focPing,focPong,focClose
    procedure SendSimpleFrame(AOpcode: TTMSFNCWebSocketFrameOpcode; aData: TBytes = nil);
    // Send close with message data
    procedure SendClose(aData: TBytes = nil); overload;
    procedure SendClose(aMessage: string); overload;
    // Socket version to check for
    property WebSocketVersion: Integer read FWebSocketVersion write FWebSocketVersion;
    // Peer IP address
    property PeerIP: string read GetPeerIP;
    // Options passed by server
    property Options: TTMSFNCWebsocketOptions read FOptions;
    // Request headers during handshake
    property HandshakeRequest: TTMSFNCWebSocketRequest read FHandshakeRequest;
    // Has handshake been completed ?
    property HandshakeCompleted: Boolean read GetHandshakeCompleted;
    // Has close frame response been sent ?
    property CloseSent: Boolean read FCloseSent;
    // Called when binary data was received
    property OnBinaryDataReceived: TTMSFNCWebSocketBinDataEvent read FOnBinaryDataReceived write FOnBinaryDataReceived;
    // Called when text data was received
    property OnMessageReceived: TTMSFNCWebSocketMessageEvent read FOnMessageReceived write FOnMessageReceived;
    // Called when Ping control message came in (pong has been sent before this is called).
    property OnPing: TTMSFNCWebSocketControlEvent read FOnPing write FOnPing;
    // Called when Pong control message came in.
    property OnPong: TTMSFNCWebSocketControlEvent read FOnPong write FOnPong;
    // Called when close control message came in.
    property OnClose: TTMSFNCWebSocketControlEvent read FOnClose write FOnClose;
  end;

resourcestring
  SErrActive = 'Operation cannot be performed while the websocket server is active';
  SErrInActive = 'Operation cannot be performed while the websocket server is not active';
  SErrHandshakeNotComplete = 'Operation cannot be performed while the handshake is not completed';

implementation

uses
  IdCoderMime, idStack;

resourcestring
  SErrNotSimpleOperation = 'Opcode %d is not a simple operation';
  SErrCloseSent = 'Close already sent, cannot send more data';

  { TTMSFNCWebSocketFrame }

constructor TTMSFNCWebSocketFrame.Create(AOpcode: TTMSFNCWebSocketFrameOpcode;
  APayload: TBytes; AIsFin: Boolean);
begin
  IsFin := AIsFin;
  FOpcode := AOpcode;
  UnmaskedPayload := APayload;
  if Assigned(UnmaskedPayload) then
    FPayloadLength := Cardinal(Length(UnmaskedPayload));
  IsMasked := False;
end;

constructor TTMSFNCWebSocketFrame.Create(AOpcode: TTMSFNCWebSocketFrameOpcode);
begin
  FOpcode := AOpcode;
  IsFin := False;
  UnmaskedPayload := [];
  FPayloadLength := 0;
  FIsMasked := False;
end;

procedure ReverseBytes(Source: TBytes; var Dest: TBytes; Index: Integer;
  Size: Integer);
var
  I: Integer;
begin
  SetLength(Dest, Size);
  for I := 0 to Size - 1 do
    Dest[Size - 1 - I] := Source[Index + I];
end;

constructor TTMSFNCWebSocketFrame.Create(const aMessage: string);
var
  payload: TBytes;
begin
  payload := TEncoding.UTF8.GetBytes(aMessage);
  Create(focText, payload, True);
end;

class function TTMSFNCWebSocketFrame.FromBuffer(var Buffer: TBytes;
  aIOHandler: TIdIOHandler): TTMSFNCWebSocketFrame;
begin
  Result := TTMSFNCWebSocketFrame.Create;
  try
    Result.ReadFromBuffer(Buffer, aIOHandler);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TTMSFNCWebSocketFrame.ReadFromBuffer(var Buffer: TBytes;
  aIOHandler: TIdIOHandler);
var
  payloadStartIndex: Integer;
  MaxLen: Uint64;
  firstNibble, secondNibble: Byte;
  Content: TBytes;
  paylen16, I: UInt16;
begin
  payloadStartIndex := 2;
  firstNibble := Buffer[0] and $F0;
  secondNibble := Buffer[0] and $0F;

  if (firstNibble and Fin) = Fin then
    IsFin := True;
  RSV := firstNibble and (not Fin);
  FOpcode := TTMSFNCWebSocketFrameOpcode(secondNibble);
  FIsMasked := Boolean((Buffer[1] and $90) shr 7);
  FPayloadLength := Buffer[1] and $7F;

  if PayloadLength = TwoBytesLengthCode then
  begin
    Move(Buffer[payloadStartIndex], paylen16, SizeOf(UInt16));
    FPayloadLength := gStack.NetworkToHost(paylen16);
    payloadStartIndex := payloadStartIndex + SizeOf(UInt16);
  end
  else if PayloadLength = EightBytesLengthCode then
  begin
    Move(Buffer[payloadStartIndex], FPayloadLength, SizeOf(UInt64));
    FPayloadLength := gStack.NetworkToHost(FPayloadLength);
    payloadStartIndex := payloadStartIndex + SizeOf(UInt64);
  end;

  if IsMasked then
  begin
    FMaskingKey := TTMSFNCWebSocketBitConverter.InTo<Int32>(Buffer,
      payloadStartIndex);
    payloadStartIndex := payloadStartIndex + SizeOf(Int32);
  end;

  SetLength(Content, PayloadLength);
  // Available bytes in buffer
  MaxLen := Length(Buffer) - payloadStartIndex;
  if PayloadLength > MaxLen then
  begin
    if MaxLen <> 0 then
      Move(Buffer[payloadStartIndex], Content[0], MaxLen);

    // read the rest
    ReadPayload(Content, aIOHandler, MaxLen);
    SetLength(Buffer, 0);
  end
  else
  begin
    if FPayloadLength <> 0 then
      Move(Buffer[payloadStartIndex], Content[0], FPayloadLength);
    if MaxLen <> FPayloadLength then
    begin
      for I := 0 to (MaxLen - FPayloadLength) - 1 do
        Buffer[I] := Buffer[FPayloadLength + Cardinal(payloadStartIndex) + I];
    end;
    SetLength(Buffer, MaxLen - FPayloadLength);
  end;

  if IsMasked then
    UnMask(Content, FMaskingKey);
  FUnmaskedPayload := Content;
end;

procedure TTMSFNCWebSocketFrame.ReadPayload(var Content: TBytes;
  aIOHandler: TIdIOHandler; aOffset: Integer);
const
  MaxBufSize = 32 * 1024;
var
  Buf: TIDBytes;
  aPos, toRead: UInt64;
  aCount: Integer;
begin
  aPos := aOffset;
  toRead := FPayloadLength - aPos;
  repeat
    aCount := toRead;
    if aCount > MaxBufSize then
      aCount := MaxBufSize;
    SetLength(Buf, aCount);
    aIOHandler.ReadBytes(Buf, aCount, False);
    Move(Buf[0], Content[aPos], aCount);
    Inc(aPos, aCount);
    toRead := FPayloadLength - aPos;
  until (toRead <= 0);
end;

function TTMSFNCWebSocketFrame.ToBuffer: TBytes;
var
  LenByte, firstByte: Byte;
  Buffer, LengthBytes, maskingBytes: TBytes;
  aOffset, I: Integer;
  pLen16: UInt16;
  pLen64: UInt64;
begin
  Result := nil;
  firstByte := Byte(Opcode);
  if IsFin then
    firstByte := firstByte or Fin;
  if PayloadLength < TwoBytesLengthCode then
  begin
    aOffset := 2;
    LenByte := Byte(PayloadLength);
    LengthBytes := [];
  end
  else if PayloadLength < 65536 {(1 shl 16)} then
  begin
    aOffset := 4;
    LenByte := TwoBytesLengthCode;
    pLen16 := PayloadLength;
    SetLength(LengthBytes, SizeOf(UInt16));
    TTMSFNCWebSocketBitConverter.From(gStack.HostToNetWork(pLen16), LengthBytes);
  end
  else
  begin
    aOffset := 10;
    LenByte := EightBytesLengthCode;
    pLen64 := PayloadLength;
    SetLength(LengthBytes, SizeOf(UInt64));
    TTMSFNCWebSocketBitConverter.From<UInt64>(gStack.HostToNetWork(pLen64), LengthBytes);
  end;
  // Typecast to silence compiler warning
  // Spec says bit 62 cannot be set, so typecast is safe.
  if IsMasked then
  begin
    LenByte := LenByte or $80;
    aOffset := aOffset + 4;
  end;
  SetLength(Buffer, aOffset + Int64(PayloadLength));
  Buffer[0] := firstByte;
  Buffer[1] := LenByte;
  for I := 0 to Length(LengthBytes) - 1 do
    Buffer[2 + I] := LengthBytes[I];

  if IsMasked then
  begin
    SetLength(maskingBytes, 4);
    TTMSFNCWebSocketBitConverter.From<Integer>(MaskingKey, maskingBytes);
    for I := 0 to Length(maskingBytes) - 1 do
      Buffer[aOffset - 4 + I] := maskingBytes[I];
  end;

  if PayloadLength > 0 then
  begin
    for I := 0 to PayloadLength - 1 do
      Buffer[aOffset + I] := UnmaskedPayload[I];
  end;

  if IsMasked then
    UnMask(Buffer, MaskingKey, aOffset);

  Result := Buffer;
end;

class function TTMSFNCWebSocketFrame.UnMask(var payload: TBytes;
  Key: Integer; Offset: Integer): TBytes;
var
  currentMaskIndex: Integer;
  byteKeys: TBytes;
  I: Integer;
begin
  currentMaskIndex := 0;
  SetLength(byteKeys, SizeOf(Key));
  TTMSFNCWebSocketBitConverter.From<Integer>(Key, byteKeys);
  for I := Offset to Length(payload) - 1 do
  begin
    payload[I] := payload[I] XOR byteKeys[currentMaskIndex];
    currentMaskIndex := (currentMaskIndex + 1) mod 4;
  end;
end;

{ TTMSFNCWebSocketBitConverter }

class procedure TTMSFNCWebSocketBitConverter.From<T>(Value: T;
  var Buffer: TArray<Byte>);
begin
  Move(Value, Buffer[0], SizeOf(T));
end;

class function TTMSFNCWebSocketBitConverter.InTo<T>(Buffer: TArray<Byte>;
  Index: Integer): T;
begin
  Move(Buffer[Index], Result, SizeOf(T));
end;

{ TTMSFNCWebSocketRequest }

constructor TTMSFNCWebSocketRequest.Create(aResource: string;
  const AHeaderList: TStrings);

  function Get(aName: string): string;
  begin
    Result := Trim(FRawHeaders.Values[aName]);
  end;

var
  k: string;
begin
  FResource := aResource;
  if not Assigned(AHeaderList) then
    Exit;

  FRawHeaders := TStringList.Create;
  FRawHeaders.NameValueSeparator := ':';
  FRawHeaders.AddStrings(AHeaderList);
  FHost := Get('Host');
  FUpgrade := Get('Upgrade');
  FOrigin := Get('Origin');
  FProtocol := Get('Sec-WebSocket-Protocol');
  FVersion := Get('Sec-WebSocket-Version');
  FExtensions := Get('Sec-WebSocket-Extensions');
  k := Get('Sec-WebSocket-Key');
  if (Length(TIdDecoderMIME.DecodeBytes(k)) = 16) then
    FKey := k;
end;

destructor TTMSFNCWebSocketRequest.Destroy;
begin
  FreeAndNil(FRawHeaders);
  inherited;
end;

{ TTMSFNCWebSocketConnection }

procedure TTMSFNCWebSocketConnection.SendSimpleFrame
  (AOpcode: TTMSFNCWebSocketFrameOpcode; aData: TBytes = nil);
var
  aFrame: TTMSFNCWebSocketFrame;
begin
  if not(AOpcode in [focClose, focPing, focPong]) then
    Raise EWebSocket.CreateFmt(SErrNotSimpleOperation, [Ord(AOpcode)]);
  aFrame := GetFrameClass.Create(AOpcode, aData, True);
  try
    SendFrame(aFrame);
  finally
    aFrame.Free;
  end;
end;

procedure TTMSFNCWebSocketConnection.SetHandShakeRequest
  (aRequest: TTMSFNCWebSocketRequest);
begin
  FreeAndNil(FHandshakeRequest);
  FHandshakeRequest := aRequest;
end;

constructor TTMSFNCWebSocketConnection.Create(aOptions: TTMSFNCWebsocketOptions);
begin
  FOptions := aOptions;
  FWebSocketVersion := CurrentWebSocketVersion;
end;

destructor TTMSFNCWebSocketConnection.Destroy;
begin
  FreeAndNil(FHandshakeRequest);
  inherited;
end;

procedure TTMSFNCWebSocketConnection.DoFrameEvents(aFrame: TTMSFNCWebSocketFrame);
var
  msg: string;
  FT: TTMSFNCWebSocketFrameTypes;
begin
  case FInitialOpcode of
    focPing:
    begin
      if Assigned(FOnPing) then
        FOnPing(Self, Self, FMessageContent);
    end;
    focPong:
    begin
      if Assigned(FOnPong) then
        FOnPong(Self, Self, FMessageContent);
    end;
    focClose:
    begin
      if Assigned(FOnClose) then
        FOnClose(Self, Self, FMessageContent);
    end;
    focBinary:
    begin
      FT := [];
      if aFrame.Opcode = focBinary then
        FT := [ftFirst]
      else
        FT := [ftContinuation];
      if aFrame.IsFin then
        FT := FT + [ftLast];
      if Assigned(OnBinaryDataReceived) then
        OnBinaryDataReceived(Self, Self, FMessageContent, FT);
      FMessageContent := nil;
    end;
    focText:
    begin
      msg := TEncoding.UTF8.GetString(FMessageContent);
      if (msg <> '') and Assigned(OnMessageReceived) then
        OnMessageReceived(Self, Self, msg);
      FMessageContent := nil;
    end;
  end;
end;

function TTMSFNCWebSocketConnection.HandleAllPendingFrames(ABuffer: TBytes;
  aIOHandler: TIdIOHandler): Boolean;
var
  f: TTMSFNCWebSocketFrame;
begin
  Result := True;
  while Length(ABuffer) > 0 do
  begin
    f := TTMSFNCWebSocketFrame.FromBuffer(ABuffer, Connection.IOHandler);
    try
      Result := HandleFrame(F);
      if not Result then
        Exit;
    finally
      F.Free;
    end;
  end;
end;

function TTMSFNCWebSocketConnection.HandleFrame
  (aFrame: TTMSFNCWebSocketFrame): Boolean;
// Return true if we can continue handling frames,
// Return false if we must close after handling this frame.
begin
  Result := True;
  // here we handle payload.
  if aFrame.Opcode <> focContinuation then
    FInitialOpcode := aFrame.Opcode;
  case aFrame.Opcode of
    focContinuation: FMessageContent := FMessageContent + aFrame.UnmaskedPayload;
    focPing:
    begin
      // We need to reply pong (section 5.5.2)
      // Send reply ASAP, unless user asked not to.
      if not(twsoManualPong in Options) then
        SendSimpleFrame(focPong, aFrame.UnmaskedPayload);
      FMessageContent := aFrame.UnmaskedPayload;
    end;
    focClose:
    begin
      // If we sent the initial close, this is the reply, and we disconnect.
      if FCloseSent then
        Result := False
      else
      // We need to reply close (section 5.5.1)
      begin
        FMessageContent := aFrame.UnmaskedPayload;
        if not(twsoManualClose in Options) then
          SendSimpleFrame(focClose);
      end;
    end;
    focPong, focBinary, focText: FMessageContent := aFrame.UnmaskedPayload;
  end;
  // here we handle payload.
  if (aFrame.IsFin) or (twsoFrameByFrame in Options) then
    DoFrameEvents(aFrame);
end;

(*
  Function TTMSFNCWebSocketConnection.CreateRequest(aUri : String; aHeaders: Tstrings): TTMSFNCWebSocketRequest;
  begin
  Result:= TTMSFNCWebSocketRequest.Create(aUri,aHeaders);
  end;
*)

function TTMSFNCWebSocketConnection.GetFrameClass: TTMSFNCWebSocketFrameClass;
begin
  Result := TTMSFNCWebSocketFrame;
end;

procedure TTMSFNCWebSocketConnection.Send(const aMessage: string; const AMasked: Boolean);
var
  fs: TTMSFNCWebSocketFrame;
begin
  Assert(HandshakeCompleted, 'Handshake not completed!');
  fs := GetFrameClass.Create(aMessage);
  fs.IsMasked := AMasked;
  try
    SendFrame(fs);
  finally
    fs.Free;
  end;
end;

procedure TTMSFNCWebSocketConnection.SendBytes(const ABytes: TBytes; const AMasked: Boolean);
var
  fs: TTMSFNCWebSocketFrame;
begin
  Assert(HandshakeCompleted, 'Handshake not completed!');
  fs := TTMSFNCWebSocketFrame.Create(focBinary, ABytes, True);
  fs.IsMasked := AMasked;
  try
    SendFrame(fs);
  finally
    fs.Free;
  end;
end;

procedure TTMSFNCWebSocketConnection.SendClose(aMessage: string);
begin
  SendClose(TEncoding.UTF8.GetBytes(aMessage));
end;

procedure TTMSFNCWebSocketConnection.SendClose(aData: TBytes);
begin
  SendSimpleFrame(focClose, aData);
end;

procedure TTMSFNCWebSocketConnection.SendFrame(aFrame: TTMSFNCWebSocketFrame);
begin
  if FCloseSent then
    raise EWebSocket.Create(SErrCloseSent);
  GetConnection.IOHandler.CheckForDisconnect(aFrame.Opcode <> focClose, True);
  if GetConnection.IOHandler.Connected then
    GetConnection.IOHandler.Write(TIDBytes(aFrame.ToBuffer));
  if aFrame.Opcode = focClose then
    FCloseSent := True;
end;

procedure TTMSFNCWebSocketConnection.SendInMultipleFrames(const AMessage: string;
  AFrameLength: UInt64; const AMasked: Boolean);
var
  payload: TBytes;
begin
  payload := TEncoding.UTF8.GetBytes(AMessage);
  SendBytesInMultipleFrames(payload, AFrameLength, AMasked);
end;

procedure TTMSFNCWebSocketConnection.SendBytesInMultipleFrames(var ABytes: TBytes; AFrameLength: UInt64; const AMasked: Boolean);
var
  fs: TTMSFNCWebSocketFrame;
  len, bufLen: UInt64;
  offset: Cardinal;
  isFin, firstFrame: Boolean;
  buf: TBytes;
begin
  Assert(HandshakeCompleted, 'Handshake not completed!');
  firstFrame := True;

  len := Length(ABytes);
  offset := 2;
  if len > 65535 then
    offset := offset + 8
  else if len > 125 then
    offset := offset + 2;

  repeat
    isFin := (len + offset) <= AFrameLength;

    bufLen := IndyMin(len, AFrameLength - offset);
    SetLength(buf, bufLen);
    Move(ABytes[0], buf[0], bufLen);

//    for I := 0 to High(ABytes) - 1 do
//      ABytes[I] := ABytes[bufLen + I];

    ABytes := Copy(ABytes, bufLen, Uint64(Length(ABytes)) - bufLen);
    SetLength(ABytes, len - bufLen);

    if firstFrame then
    begin
      fs := TTMSFNCWebSocketFrame.Create(focBinary, buf, isFin);
      firstFrame := False;
    end
    else
      fs := TTMSFNCWebSocketFrame.Create(focContinuation, buf, isFin);

    fs.IsMasked := AMasked;

    try
      SendFrame(fs);
    finally
      fs.Free;
    end;

    len := len - bufLen;
  until not isFin;
end;

end.
