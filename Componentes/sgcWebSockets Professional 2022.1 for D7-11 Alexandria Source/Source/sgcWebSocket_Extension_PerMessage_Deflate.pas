{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Extension_PerMessage_Deflate;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdZLib{$ELSE}IdZLib{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdStream{$ELSE}IdStream{$ENDIF}
{$IFDEF INDY10_2}
    , {$IFDEF SGC_INDY}sgcIdZLibHeaders{$ELSE}IdZLibHeaders{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCTypes{$ELSE}IdCTypes{$ENDIF}
{$ENDIF},
  // sgc
  sgcWebSocket_Extension_Base;

type
{$IFNDEF D7}
{$IFNDEF D2010}
{$IFDEF INDY10_2}
  TIdC_UINT = Cardinal;
{$ELSE}
  TIdC_UINT = Integer;
{$ENDIF}
  PIdC_UINT = ^TIdC_UINT;
  PtrInt = LongInt;

  RawByteString = type AnsiString;
  PRawByteString = ^RawByteString;
  _UTF8Str = UTF8String;
  _PUTF8Str = PUTF8String;
  _RawByteStr = RawByteString;
  _PRawByteStr = PRawByteString;
{$ENDIF}
{$ELSE}
  RawByteString = type AnsiString;
{$ENDIF}
  TMemStreamAccess = class(TMemoryStream);

  TsgcWSExtension_PerMessage_Deflate = class(TsgcWSExtension_Base)

    { from TsgcWSExtension_Base }
  public
    function DecodeExtension(const aExtension: string): Boolean; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    procedure DecodeHeader(aByte: Byte); override;
    procedure EncodeHeader(var aByte: Byte); override;
  public
    function GetHeaderExtension: String; override;
  public
    function GetName: String; override;
    { from TsgcWSExtension_Base }

  private
    FMessageCompressed: Boolean;
    { helpers }
  protected
    function ExpandStream(AStream: TStream; const ACapacity: Int64): Boolean;
    function DMAOfStream(AStream: TStream; out Available: TIdC_UINT): Pointer;
    function CanResizeDMAStream(AStream: TStream): Boolean;
    { helpers }

    { deflate }
  private
    FDefStream: TStringStream;
  private
    function GetDefStream: TStringStream;
  protected
    property DefStream: TStringStream read GetDefStream write FDefStream;
  protected
    procedure DoDeflateFrame(InStream, OutStream: TStream);
    { deflate }

    { inflate }
  private
    FReadBytes: TIdBytes;
    FInfStream: TStringStream;
  private
    procedure DoInitializeInflateBuffer;
  private
    function GetInfStream: TStringStream;
    procedure SetClientMaxWindowBits(const Value: Integer);
    procedure SetClientNoContextTakeOver(const Value: Boolean);
    procedure SetServerMaxWindowBits(const Value: Integer);
    procedure SetServerNoContextTakeOver(const Value: Boolean);
  protected
    property InfStream: TStringStream read GetInfStream write FInfStream;
  protected
    procedure DoInflateFrame(InStream, OutStream: TStream);
    { inflate }

  public
    constructor Create; override;
    destructor Destroy; override;

  public
    procedure DeflateFrame(var aStream: TStream);
    procedure InflateFrame(var aStream: TStream);
  public
    property MessageCompressed: Boolean read FMessageCompressed;
  private
    FServerMaxWindowBits: Integer;
    FServerNoContextTakeOver: Boolean;
    FClientMaxWindowBits: Integer;
    FClientNoContextTakeOver: Boolean;
  private
    FMemLevel: Integer;
  published
    property ClientMaxWindowBits: Integer read FClientMaxWindowBits
      write SetClientMaxWindowBits;
    property ClientNoContextTakeOver: Boolean read FClientNoContextTakeOver
      write SetClientNoContextTakeOver;
    property MemLevel: Integer read FMemLevel write FMemLevel;
    property ServerMaxWindowBits: Integer read FServerMaxWindowBits
      write SetServerMaxWindowBits;
    property ServerNoContextTakeOver: Boolean read FServerNoContextTakeOver
      write SetServerNoContextTakeOver;
  End;

implementation

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcWebSocket_Types,
  sgcBase_Helpers;

{$IFNDEF INDY10_2}

function DeflateInit2(var stream: TZStreamRec;
  level, method, windowBits, MemLevel, strategy: Integer): Integer;
begin
  result := deflateInit2_(stream, level, method, windowBits, MemLevel, strategy,
    ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(stream, windowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;
{$ENDIF}

constructor TsgcWSExtension_PerMessage_Deflate.Create;
begin
  inherited;
  SetLength(FReadBytes, 0);
  ServerMaxWindowBits := CS_DEFAULT_WINDOW_BITS;
  ServerNoContextTakeOver := False;
  ClientMaxWindowBits := CS_DEFAULT_WINDOW_BITS;
  ClientNoContextTakeOver := False;
  MemLevel := MAX_MEM_LEVEL;
end;

destructor TsgcWSExtension_PerMessage_Deflate.Destroy;
begin
  sgcFree(FDefStream);
  sgcFree(FInfStream);
end;

procedure TsgcWSExtension_PerMessage_Deflate.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSExtension_PerMessage_Deflate then
  begin
    Mode := TsgcWSExtension_PerMessage_Deflate(aSource).Mode;
    Enabled := TsgcWSExtension_PerMessage_Deflate(aSource).Enabled;
    ServerMaxWindowBits := TsgcWSExtension_PerMessage_Deflate(aSource)
      .ServerMaxWindowBits;
    ServerNoContextTakeOver := TsgcWSExtension_PerMessage_Deflate(aSource)
      .ServerNoContextTakeOver;
    ClientMaxWindowBits := TsgcWSExtension_PerMessage_Deflate(aSource)
      .ClientMaxWindowBits;
    ClientNoContextTakeOver := TsgcWSExtension_PerMessage_Deflate(aSource)
      .ClientNoContextTakeOver;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSExtension_PerMessage_Deflate.ExpandStream(AStream: TStream;
  const ACapacity: Int64): Boolean;
begin
  result := True;
  AStream.Size := ACapacity;
  if AStream is TMemoryStream then
  begin
    AStream.Size := TMemStreamAccess(AStream).Capacity;
  end;
end;

function TsgcWSExtension_PerMessage_Deflate.CanResizeDMAStream
  (AStream: TStream): Boolean;
begin
  result := (AStream is TCustomMemoryStream) or (AStream is TStringStream);
end;

function TsgcWSExtension_PerMessage_Deflate.DecodeExtension(const aExtension
  : string): Boolean;
var
  i: Integer;
  oList: TsgcDelimitedStringList;
begin
  inherited DecodeExtension(aExtension);
  result := sgcContainsText(aExtension, CS_PERMESSAGE_DEFLATE);
  if result then
  begin
    FHeaderExtension := GetName;

    oList := TsgcDelimitedStringList.Create;
    Try
      oList.Delimiter := ';';
      oList.DelimitedText := aExtension;
      for i := 0 to oList.Count - 1 do
      begin
        // ... client
        if Trim(oList.Names[i]) = CS_CLIENT_MAX_WINDOW_BITS then
        begin
          ClientMaxWindowBits := StrToInt(Trim(oList.ValueFromIndex[i]));
          FHeaderExtension := FHeaderExtension + ';' + CS_CLIENT_MAX_WINDOW_BITS
            + '=' + IntToStr(ClientMaxWindowBits)
        end
        else if Trim(oList[i]) = CS_CLIENT_MAX_WINDOW_BITS then
          FHeaderExtension := FHeaderExtension + ';' + CS_CLIENT_MAX_WINDOW_BITS
            + '=' + IntToStr(ClientMaxWindowBits)
        else if Trim(oList[i]) = CS_CLIENT_NO_CONTEXT_TAKE_OVER then
        begin
          FHeaderExtension := FHeaderExtension + ';' +
            CS_CLIENT_NO_CONTEXT_TAKE_OVER;
          ClientNoContextTakeOver := True;
        end

        // ... server
        else if Trim(oList.Names[i]) = CS_SERVER_MAX_WINDOW_BITS then
        begin
          ServerMaxWindowBits := StrToInt(Trim(oList.ValueFromIndex[i]));
          FHeaderExtension := FHeaderExtension + ';' + CS_SERVER_MAX_WINDOW_BITS
            + '=' + IntToStr(ServerMaxWindowBits)
        end
        else if Trim(oList[i]) = CS_SERVER_MAX_WINDOW_BITS then
          FHeaderExtension := FHeaderExtension + ';' + CS_SERVER_MAX_WINDOW_BITS
            + '=' + IntToStr(ServerMaxWindowBits)
        else if Trim(oList[i]) = CS_SERVER_NO_CONTEXT_TAKE_OVER then
        begin
          FHeaderExtension := FHeaderExtension + ';' +
            CS_SERVER_NO_CONTEXT_TAKE_OVER;
          ServerNoContextTakeOver := True;
        end;
      end;
    Finally
      sgcFree(oList);
    End;
  end;
end;

procedure TsgcWSExtension_PerMessage_Deflate.DecodeHeader(aByte: Byte);
begin
  FMessageCompressed := (aByte and $40) = $40;
end;

function TsgcWSExtension_PerMessage_Deflate.DMAOfStream(AStream: TStream;
  out Available: TIdC_UINT): Pointer;
{$IFDEF LAZARUS}
var
  vAddress: LongInt;
{$ENDIF}
begin
  if AStream is TCustomMemoryStream then
    result := TCustomMemoryStream(AStream).Memory
  else
    result := nil;

  if result <> nil then
  begin
    Available := AStream.Size - AStream.Position;
{$IFDEF LAZARUS}
    vAddress := PtrInt(result);
    Inc(vAddress, AStream.Position);
{$ELSE}
    Inc(PtrInt(result), AStream.Position);
{$ENDIF}
  end
  else
    Available := 0;
end;

procedure TsgcWSExtension_PerMessage_Deflate.DoDeflateFrame(InStream,
    OutStream: TStream);
const
  BufSize = CS_DEFLATE_BUFFER;
var
  strm: TZStreamRec;
  InBuf, OutBuf: {$IFDEF NEXTGEN}PIdAnsiChar{$ELSE}PAnsiChar{$ENDIF};
  UseInBuf, UseOutBuf: Boolean;
  LastOutCount: TIdC_UINT;

  procedure WriteOut;
  begin
    if UseOutBuf then
    begin
      if LastOutCount > 0 then
        OutStream.Write(OutBuf^, LastOutCount - strm.avail_out);

      strm.avail_out := BufSize;
      strm.next_out := OutBuf;
    end
    else
    begin
      if strm.avail_out = 0 then
        ExpandStream(OutStream, OutStream.Size + BufSize);
      OutStream.Seek(LastOutCount - strm.avail_out, soCurrent);
      strm.next_out := DMAOfStream(OutStream, strm.avail_out);
    end;
    LastOutCount := strm.avail_out;
  end;

var
  Finished: Boolean;
  zResult: Integer;
begin
  FillChar(strm, SizeOf(strm), 0);

  InBuf := nil;
  OutBuf := nil;
  LastOutCount := 0;

  strm.next_in := DMAOfStream(InStream, strm.avail_in);
  UseInBuf := strm.next_in = nil;

  if UseInBuf then
    GetMem(InBuf, BufSize);

  try
    UseOutBuf := not CanResizeDMAStream(OutStream);
    if UseOutBuf then
      GetMem(OutBuf, BufSize);

    try
      case Mode of
        appServer:
          CCheck(DeflateInit2(strm, Z_BEST_SPEED, Z_DEFLATED,
            -ServerMaxWindowBits, MemLevel, Z_DEFAULT_STRATEGY));
        appClient:
          CCheck(DeflateInit2(strm, Z_BEST_SPEED, Z_DEFLATED,
            -ClientMaxWindowBits, MemLevel, Z_DEFAULT_STRATEGY));
      end;
      if not(((Mode = appClient) and (ClientNoContextTakeOver = True)) or
        ((Mode = appServer) and (ServerNoContextTakeOver = True))) then
      begin
        if DefStream.Size > 0 then
{$IFDEF LAZARUS}
          CCheck(deflateSetDictionary(strm, PAnsiChar(DefStream.DataString),
            DefStream.Size));
{$ELSE}
{$IFNDEF D2009}
          CCheck(deflateSetDictionary(strm,
            PAnsiChar(RawByteString(DefStream.DataString)), DefStream.Size));
{$ELSE}
          CCheck(deflateSetDictionary(strm, DefStream.Memory, DefStream.Size));
{$ENDIF}
{$ENDIF}
      end;

      DefStream.CopyFrom(InStream, InStream.Size);

      InStream.Position := 0;
      repeat
        if strm.avail_in = 0 then
        begin
          if UseInBuf then
          begin
            strm.avail_in := InStream.Read(InBuf^, BufSize);
            strm.next_in := InBuf;
          end;
        end;

        if strm.avail_out = 0 then
          WriteOut;

        zResult := deflate(strm, Z_SYNC_FLUSH);
        if zResult <> Z_BUF_ERROR then
          CCheck(zResult);

      until strm.avail_out > 0;

      repeat
        zResult := deflate(strm, Z_FINISH);
        Finished := zResult = Z_STREAM_END;
        if zResult <> Z_BUF_ERROR then
          CCheck(zResult);
        WriteOut;
      until Finished;

      if not UseOutBuf then
        OutStream.Size := OutStream.Position;

      if UseInBuf then
        InStream.Seek(-strm.avail_in, soCurrent)
      else
        InStream.Seek(strm.total_in, soCurrent);

      CCheck(deflateEnd(strm));
    finally
      if OutBuf <> nil then
        FreeMem(OutBuf);
    end;
  finally
    if InBuf <> nil then
      FreeMem(InBuf);
  end;

  if OutStream.Size > 6 then
    OutStream.Size := OutStream.Size - 6;
end;

procedure TsgcWSExtension_PerMessage_Deflate.InflateFrame(var aStream: TStream);
var
  oStream: TMemoryStream;
begin
  DoInitializeInflateBuffer;

  AStream.WriteBuffer(CS_Deflate_Bytes, Length(CS_Deflate_Bytes));
  AStream.Position := 0;

  oStream := TMemoryStream.Create;
  Try
    DoInflateFrame(AStream, oStream);

    AStream.Size := 0;
    oStream.Position := 0;
    AStream.CopyFrom(oStream, oStream.Size);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSExtension_PerMessage_Deflate.DoInflateFrame(InStream,
    OutStream: TStream);
var
  strm: TZStreamRec;
  i: Integer;
  vBuffer: Array [0 .. CS_DEFLATE_BUFFER - 1] of
{$IFDEF NEXTGEN}Byte{$ELSE}AnsiChar{$ENDIF};
  zResult: Integer;
begin
  FillChar(strm, SizeOf(strm), 0);
  strm.zalloc := nil;
  strm.zfree := nil;
  strm.next_in := nil;
  strm.avail_in := 0;

  case Mode of
    appServer:
      DCheck(InflateInit2(strm, -ClientMaxWindowBits));
    appClient:
      DCheck(InflateInit2(strm, -ServerMaxWindowBits));
  end;

  if not(((Mode = appClient) and (ServerNoContextTakeOver)) or
    ((Mode = appServer) and (ClientNoContextTakeOver))) then
  begin
    if InfStream.Size > 0 then
{$IFDEF LAZARUS}
      DCheck(inflateSetDictionary(strm, PAnsiChar(InfStream.DataString),
        InfStream.Size));
{$ELSE}
{$IFNDEF D2009}
      DCheck(inflateSetDictionary(strm,
        PAnsiChar(RawByteString(InfStream.DataString)), InfStream.Size));
{$ELSE}
      DCheck(inflateSetDictionary(strm, InfStream.Memory, InfStream.Size));
{$ENDIF}
{$ENDIF}
  end;

  InStream.Position := 0;

  while True do
  begin
    strm.avail_in := InStream.Read(vBuffer, CS_INFLATE_BUFFER);
    if strm.avail_in < 1 then
      Break;
    strm.next_in := {$IFDEF NEXTGEN}@{$ENDIF}vBuffer;

    repeat
      strm.next_out := @FReadBytes[0];
      strm.avail_out := CS_DEFLATE_BUFFER;
      zResult := inflate(strm, Z_NO_FLUSH);
      if zResult <> Z_BUF_ERROR then
        DCheck(zResult);

      i := CS_DEFLATE_BUFFER - strm.avail_out;
      if i > 0 then
        OutStream.WriteBuffer(FReadBytes[0], i);
    until (strm.avail_in = 0) and (strm.avail_out > 0);
  end;

  // handle Z_BUF_ERROR
  repeat
    strm.next_out := @FReadBytes[0];
    strm.avail_out := CS_DEFLATE_BUFFER;

    zResult := inflate(strm, Z_FINISH);
    if zResult <> Z_BUF_ERROR then
      zResult := DCheck(zResult);
    i := CS_DEFLATE_BUFFER - strm.avail_out;
    if i > 0 then
      OutStream.WriteBuffer(FReadBytes[0], i);
  until ((zResult = Z_STREAM_END) and (strm.avail_out > 0)) or
    (zResult = Z_BUF_ERROR);

  DCheck(inflateEnd(strm));

  // ... copy to buffer
  OutStream.Position := 0;
  InfStream.CopyFrom(OutStream, OutStream.Size);
end;

procedure TsgcWSExtension_PerMessage_Deflate.DoInitializeInflateBuffer;
begin
  if Length(FReadBytes) = 0 then
    SetLength(FReadBytes, CS_DEFLATE_BUFFER);
end;

procedure TsgcWSExtension_PerMessage_Deflate.EncodeHeader(var aByte: Byte);
begin
  aByte := aByte or $40;
end;

function TsgcWSExtension_PerMessage_Deflate.GetInfStream: TStringStream;
begin
  if not Assigned(FInfStream) then
    FInfStream := TStringStream.Create('')
  else if FInfStream.Size > CS_DEFLATE_BUFFER then
    ResizeStream(FInfStream, CS_DEFLATE_BUFFER);
  result := FInfStream;
end;

function TsgcWSExtension_PerMessage_Deflate.GetHeaderExtension: String;
begin
  case Mode of
    appClient:
      begin
        FHeaderExtension := GetName;
        if ClientMaxWindowBits <> CS_DEFAULT_WINDOW_BITS then
          FHeaderExtension := FHeaderExtension + ';' + CS_CLIENT_MAX_WINDOW_BITS
            + '=' + IntToStr(ClientMaxWindowBits);
        if ClientNoContextTakeOver then
          FHeaderExtension := FHeaderExtension + ';' +
            CS_CLIENT_NO_CONTEXT_TAKE_OVER;
        if ServerMaxWindowBits <> CS_DEFAULT_WINDOW_BITS then
          FHeaderExtension := FHeaderExtension + ';' + CS_SERVER_MAX_WINDOW_BITS
            + '=' + IntToStr(ServerMaxWindowBits);
        if ServerNoContextTakeOver then
          FHeaderExtension := FHeaderExtension + ';' +
            CS_SERVER_NO_CONTEXT_TAKE_OVER;
      end;
  end;
  result := inherited GetHeaderExtension;
end;

function TsgcWSExtension_PerMessage_Deflate.GetDefStream: TStringStream;
begin
  if not Assigned(FDefStream) then
    FDefStream := TStringStream.Create('')
  else if FDefStream.Size > CS_DEFLATE_BUFFER then
    ResizeStream(FDefStream, CS_DEFLATE_BUFFER);
  result := FDefStream;
end;

function TsgcWSExtension_PerMessage_Deflate.GetName: String;
begin
  case Mode of
    appClient:
      result := CS_PERMESSAGE_DEFLATE + '; client_max_window_bits';
  else
    result := CS_PERMESSAGE_DEFLATE;
  end;
end;

procedure TsgcWSExtension_PerMessage_Deflate.DeflateFrame(var aStream: TStream);
var
  oStream: TMemoryStream;
begin
  oStream := TMemoryStream.Create;
  Try
    AStream.Position := 0;
    DoDeflateFrame(AStream, oStream);

    AStream.Size := 0;
    oStream.Position := 0;
    AStream.CopyFrom(oStream, oStream.Size);
    AStream.Position := 0;
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSExtension_PerMessage_Deflate.SetClientMaxWindowBits
  (const Value: Integer);
begin
  if (Value < 8) or (Value > 15) then
    raise Exception.CreateFmt(S_INVALID_WINDOWBITS, [Value]);
  FClientMaxWindowBits := Value;
end;

procedure TsgcWSExtension_PerMessage_Deflate.SetClientNoContextTakeOver
  (const Value: Boolean);
begin
  FClientNoContextTakeOver := Value;
end;

procedure TsgcWSExtension_PerMessage_Deflate.SetServerMaxWindowBits
  (const Value: Integer);
begin
  if (Value < 8) or (Value > 15) then
    raise Exception.CreateFmt(S_INVALID_WINDOWBITS, [Value]);
  FServerMaxWindowBits := Value;
end;

procedure TsgcWSExtension_PerMessage_Deflate.SetServerNoContextTakeOver
  (const Value: Boolean);
begin
  FServerNoContextTakeOver := Value;
end;

end.
