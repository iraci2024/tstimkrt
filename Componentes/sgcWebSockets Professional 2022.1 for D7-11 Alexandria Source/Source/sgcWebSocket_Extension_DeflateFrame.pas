{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Extension_DeflateFrame;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // indy
  {$IFDEF SGC_INDY}sgcIdZLib{$ELSE}IdZLib{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdStream{$ELSE}IdStream{$ENDIF},
  {$IFDEF INDY10_2}
    {$IFDEF SGC_INDY}sgcIdZLibHeaders{$ELSE}IdZLibHeaders{$ENDIF},
    {$IFDEF SGC_INDY}sgcIdCTypes{$ELSE}IdCTypes{$ENDIF},
  {$ENDIF}
  // sgc
  sgcWebSocket_Extension_Base;

type
  {$IFNDEF D7}
    {$IFNDEF D2010}
    {$IFDEF INDY10_2}
    TIdC_UINT  = Cardinal;
    {$ELSE}
    TIdC_UINT  = Integer;
    {$ENDIF}
    PIdC_UINT  = ^TIdC_UINT;
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

  TsgcWSExtension_DeflateFrame = class(TsgcWSExtension_Base)

  { from TsgcWSExtension_Base }
  public
    function DecodeExtension(const aExtension: string): Boolean; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    procedure DecodeHeader(aByte: Byte); override;
    procedure EncodeHeader(var aByte: Byte); override;
  public
    function GetName: String; override;
  { from TsgcWSExtension_Base }

  private
    FWindowBits: Integer;
    FMessageCompressed: Boolean;
    procedure SetWindowBits(const Value: Integer);

  { helpers }
  protected
    function ExpandStream(AStream: TStream; const ACapacity : Int64): Boolean;
    function DMAOfStream(AStream: TStream; out Available: TIdC_UINT): Pointer;
    function CanResizeDMAStream(AStream: TStream): boolean;
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
  protected
    property InfStream: TStringStream read GetInfStream write FInfStream;
  protected
    function IdRead(var aStrmRec: TZStreamRec; var VBuffer: TIdBytes; AOffset:
        Integer): Longint;
    function DoRead(var aStrmRec: TZStreamRec; var aBuffer): LongInt;
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
  published
    property WindowBits: Integer read FWindowBits write SetWindowBits;
  End;



implementation

uses
  sgcBase_Helpers, sgcWebSocket_Const, sgcWebSocket_Helpers;


{$IFNDEF INDY10_2}
function DeflateInit2(var stream: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
begin
  result := deflateInit2_(stream,level,method,windowBits,memLevel,strategy,
    ZLIB_VERSION,SizeOf(TZStreamRec));
end;


function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(stream,windowBits,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
{$ENDIF}

constructor TsgcWSExtension_DeflateFrame.Create;
begin
  inherited;
  SetLength(FReadBytes, 0);
  FHeaderExtension := GetName;
  WindowBits := 15;
end;

destructor TsgcWSExtension_DeflateFrame.Destroy;
begin
  sgcFree(FDefStream);
  sgcFree(FInfStream);
end;

procedure TsgcWSExtension_DeflateFrame.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSExtension_DeflateFrame then
  begin
    Mode := TsgcWSExtension_DeflateFrame(aSource).Mode;
    Enabled := TsgcWSExtension_DeflateFrame(aSource).Enabled;
    WindowBits := TsgcWSExtension_DeflateFrame(aSource).WindowBits;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSExtension_DeflateFrame.IdRead(var aStrmRec: TZStreamRec; var
    VBuffer: TIdBytes; AOffset: Integer): Longint;
begin
  aStrmRec.next_out := {$IFNDEF NEXTGEN}PAnsiChar{$ENDIF}(@VBuffer[AOffset]);
  aStrmRec.avail_out := CS_DEFLATE_BUFFER;
  aStrmRec.total_out := 0;
  while (aStrmRec.avail_out > 0) and (aStrmRec.avail_in > 0) do
  begin
    if CCheck(inflate(aStrmRec, Z_NO_FLUSH)) = Z_STREAM_END then
      Break;
  end;
  Result := CS_DEFLATE_BUFFER - aStrmRec.avail_out;
end;


function TsgcWSExtension_DeflateFrame.ExpandStream(AStream: TStream; const ACapacity : Int64): Boolean;
begin
  Result := True;
  AStream.Size := ACapacity;
  if AStream is TMemoryStream then begin
    AStream.Size := TMemStreamAccess(AStream).Capacity;
  end;
end;


function TsgcWSExtension_DeflateFrame.CanResizeDMAStream(AStream: TStream): boolean;
begin
  Result := (AStream is TCustomMemoryStream) or
            (AStream is TStringStream);
end;

function TsgcWSExtension_DeflateFrame.DecodeExtension(const aExtension:
    string): Boolean;
begin
  inherited DecodeExtension(aExtension);
  result := sgcContainsText(aExtension, GetName);
end;

procedure TsgcWSExtension_DeflateFrame.DecodeHeader(aByte: Byte);
begin
  FMessageCompressed := (aByte and $40) = $40;
end;

function TsgcWSExtension_DeflateFrame.DMAOfStream(AStream: TStream; out Available: TIdC_UINT): Pointer;
{$IFDEF LAZARUS}
var
  vAddress: LongInt;
{$ENDIF}
begin
  if AStream is TCustomMemoryStream then
    Result := TCustomMemoryStream(AStream).Memory
  else
    Result := nil;

  if Result <> nil then
  begin
    Available := AStream.Size - AStream.Position;
    {$IFDEF LAZARUS}
    vAddress := PtrInt(Result);
    Inc(vAddress, AStream.Position);
    {$ELSE}
    Inc(PtrInt(Result), AStream.Position);
    {$ENDIF}
  end
  else
    Available := 0;
end;

procedure TsgcWSExtension_DeflateFrame.DoDeflateFrame(InStream, OutStream:
    TStream);
const
  BufSize = 65356;
var
  strm   : TZStreamRec;
  InBuf, OutBuf : {$IFDEF NEXTGEN}PIdAnsiChar{$ELSE}PAnsiChar{$ENDIF};
  UseInBuf, UseOutBuf : boolean;
  LastOutCount : TIdC_UINT;

  procedure WriteOut;
  begin
    if UseOutBuf then
    begin
      if LastOutCount > 0 then
        OutStream.Write(OutBuf^, LastOutCount - strm.avail_out);

      strm.avail_out := BufSize;
      strm.next_out  := OutBuf;
    end else
    begin
      if strm.avail_out = 0 then
        ExpandStream(OutStream, OutStream.Size + BufSize);

      OutStream.Seek(LastOutCount - strm.avail_out, soCurrent);
      strm.next_out  := DMAOfStream(OutStream, strm.avail_out);
    end;
    LastOutCount := strm.avail_out;
  end;

var
  Finished : boolean;
begin
  FillChar(strm, SizeOf(strm), 0);

  InBuf          := nil;
  OutBuf         := nil;
  LastOutCount   := 0;

  strm.next_in   := DMAOfStream(InStream, strm.avail_in);
  UseInBuf := strm.next_in = nil;

  if UseInBuf then
    GetMem(InBuf, BufSize);

  try
    UseOutBuf := not CanResizeDMAStream(OutStream);
    if UseOutBuf then begin
      GetMem(OutBuf, BufSize);
    end;

    try
      CCheck(deflateInit2(strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -FWindowBits, 1, Z_DEFAULT_STRATEGY));
      if DefStream.Size > 0 then
      {$IFDEF LAZARUS}
        CCheck(deflateSetDictionary(strm, PAnsiChar(DefStream.DataString), DefStream.Size));
      {$ELSE}
        {$IFNDEF D2009}
        CCheck(deflateSetDictionary(strm, PAnsiChar(RawByteString(DefStream.DataString)), DefStream.Size));
        {$ELSE}
        CCheck(deflateSetDictionary(strm, DefStream.Memory, DefStream.Size));
        {$ENDIF}
      {$ENDIF}

      DefStream.CopyFrom(InStream, InStream.Size);
      InStream.Position := 0;
      repeat
        if strm.avail_in = 0 then
        begin
          if UseInBuf then
          begin
            strm.avail_in := InStream.Read(InBuf^, BufSize);
            strm.next_in  := InBuf;
          end;
        end;

        if strm.avail_out = 0 then
          WriteOut;

        CCheck(deflate(strm, Z_SYNC_FLUSH));
      until
        strm.avail_out > 0;

      repeat
        Finished := CCheck(deflate(strm, Z_FINISH)) = Z_STREAM_END;
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

procedure TsgcWSExtension_DeflateFrame.InflateFrame(var aStream: TStream);
var
  oStream: TMemoryStream;
begin
  DoInitializeInflateBuffer;

  aStream.WriteBuffer(CS_Deflate_Bytes, Length(CS_Deflate_Bytes));
  aStream.Position := 0;

  oStream := TMemoryStream.Create;
  Try
    DoInflateFrame(aStream, oStream);

    aStream.Size := 0;
    oStream.Position := 0;
    aStream.CopyFrom(oStream, oStream.Size);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSExtension_DeflateFrame.DoInflateFrame(InStream, OutStream:
    TStream);
var
  strm: TZStreamRec;
  i: integer;
  vBuffer: Array [0..CS_DEFLATE_BUFFER-1] of {$IFDEF NEXTGEN}Byte{$ELSE}AnsiChar{$ENDIF};
begin

  fillchar(strm, SizeOf(strm), 0);
  strm.zalloc := nil;
  strm.zfree := nil;
  strm.next_in   := nil;
  strm.avail_in  := 0;

  DCheck(inflateInit2(strm, -WindowBits));

  if InfStream.Size > 0 then
    {$IFDEF LAZARUS}
    DCheck(inflateSetDictionary(strm, PAnsiChar(InfStream.DataString), InfStream.Size));
    {$ELSE}
    {$IFNDEF D2009}
    DCheck(inflateSetDictionary(strm, PAnsiChar(RawByteString(InfStream.DataString)), InfStream.Size));
    {$ELSE}
    DCheck(inflateSetDictionary(strm, InfStream.Memory, InfStream.Size));
    {$ENDIF}
    {$ENDIF}

  InStream.Position := 0;

  while True do
  begin
    strm.avail_in := InStream.Read(vBuffer, CS_INFLATE_BUFFER);
    if strm.avail_in = 0 then
      break;
    strm.next_in  := {$IFDEF NEXTGEN}@{$ENDIF}vBuffer;

    repeat
      strm.next_out := @FReadBytes[0];
      strm.avail_out := CS_DEFLATE_BUFFER;
      DCheck(inflate(strm,Z_NO_FLUSH));
      i := CS_DEFLATE_BUFFER - strm.avail_out;
      OutStream.WriteBuffer(FReadBytes[0], i);
    until
      (strm.avail_in = 0) and (strm.avail_out > 0);
  end;

  DCheck(inflateEnd(strm));

  // ... copy to buffer
  OutStream.Position := 0;
  InfStream.CopyFrom(OutStream, OutStream.Size);
end;

procedure TsgcWSExtension_DeflateFrame.DoInitializeInflateBuffer;
begin
  if Length(FReadBytes) = 0 then
    SetLength(FReadBytes, CS_DEFLATE_BUFFER);
end;

function TsgcWSExtension_DeflateFrame.DoRead(var aStrmRec: TZStreamRec; var
    aBuffer): LongInt;
var
  LBytes: TIdBytes;
begin
  SetLength(LBytes, CS_DEFLATE_BUFFER);
  Result := IdRead(aStrmRec, LBytes, 0);
  if Result > 0 then begin
    sgcMove(LBytes[0], aBuffer, Result);
  end;
end;

procedure TsgcWSExtension_DeflateFrame.EncodeHeader(var aByte: Byte);
begin
  aByte := aByte or $40;
end;

function TsgcWSExtension_DeflateFrame.GetInfStream: TStringStream;
begin
  if not Assigned(FInfStream) then
    FInfStream := TStringStream.Create('')
  else if FInfStream.Size > CS_DEFLATE_BUFFER then
    ResizeStream(FInfStream, CS_DEFLATE_BUFFER);
  Result := FInfStream;
end;

function TsgcWSExtension_DeflateFrame.GetDefStream: TStringStream;
begin
  if not Assigned(FDefStream) then
    FDefStream := TStringStream.Create('')
  else if FDefStream.Size > CS_DEFLATE_BUFFER then
    ResizeStream(FDefStream, CS_DEFLATE_BUFFER);
  Result := FDefStream;
end;

function TsgcWSExtension_DeflateFrame.GetName: String;
begin
  Result := CS_DEFLATE_FRAME;
end;

procedure TsgcWSExtension_DeflateFrame.DeflateFrame(var aStream: TStream);
var
  oStream: TMemoryStream;
begin
  oStream := TMemoryStream.Create;
  Try
    aStream.Position := 0;
    DoDeflateFrame(aStream, oStream);

    aStream.Size := 0;
    oStream.Position := 0;
    aStream.CopyFrom(oStream, oStream.Size);
    aStream.Position := 0;
  Finally
    sgcFree(oStream);
  End;
end;


procedure TsgcWSExtension_DeflateFrame.SetWindowBits(const Value: Integer);
begin
  if (Value < 8) or (Value > 15) then
    raise Exception.CreateFmt(S_INVALID_WINDOWBITS, [Value]);
  FWindowBits := Value;
end;


end.
