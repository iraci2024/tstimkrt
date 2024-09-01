unit ZFZLib;

interface

uses SysUtils, Classes;

const
  ZLIB_VERSION: PAnsiChar = '1.2.11'; // do not localize

type

{$IFNDEF D6H}
  PByte = ^Byte;
{$ENDIF}

  alloc_func = function(opaque: Pointer; Items, Size: Cardinal): Pointer; cdecl;
  free_func = procedure(opaque, address: Pointer); cdecl;

  z_stream = record
    next_in: PByte;      // next input byte
    avail_in: Cardinal;  // number of bytes available at next_in
    total_in: LongWord;  // total nb of input bytes read so far
    next_out: PByte;     // next output byte will go here
    avail_out: Cardinal; // remaining free space at next_out
    total_out: LongWord; // total nb of bytes output so far
    msg: PAnsiChar;      // last error message, NULL if no error
    state: Pointer;      // not visible by applications
    zalloc: alloc_func;  // used to allocate the internal state
    zfree: free_func;    // used to free the internal state
    opaque: Pointer;     // private data object passed to zalloc and zfree
    data_type: Integer;  // best guess about the data type: binary or text
                         // for deflate, or the decoding state for inflate
    adler: LongWord;     // Adler-32 or CRC-32 value of the uncompressed data
    reserved: LongWord;  // reserved for future use
  end;


                        (* constants *)
const
  Z_NO_FLUSH       = 0;
  Z_PARTIAL_FLUSH  = 1;
  Z_SYNC_FLUSH     = 2;
  Z_FULL_FLUSH     = 3;
  Z_FINISH         = 4;
  Z_BLOCK          = 5;
  Z_TREES          = 6;
  // Allowed flush values; see deflate() and inflate() below for details


  Z_OK             = 0;
  Z_STREAM_END     = 1;
  Z_NEED_DICT      = 2;
  Z_ERRNO          = (-1);
  Z_STREAM_ERROR   = (-2);
  Z_DATA_ERROR     = (-3);
  Z_MEM_ERROR      = (-4);
  Z_BUF_ERROR      = (-5);
  Z_VERSION_ERROR  = (-6);
  // Return codes for the compression/decompression functions. Negative values
  // are errors, positive values are used for special but normal events.

function CCheck(code: integer): integer;
function DCheck(code: integer): integer;

{ CompressBuf compresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure ZLIBCompressBuf(const InBuf: Pointer;
  InBytes: integer; out OutBuf: Pointer;
  out OutBytes: integer; compMode: byte = 1);


{ DecompressBuf decompresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
       OutEstimate = zero, or est. size of the decompressed data
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure ZLIBDecompressBuf(const InBuf: Pointer; InBytes: integer;
  OutEstimate: integer; out OutBuf: Pointer; out OutBytes: integer);



function deflate(var strm: z_stream; flush: Integer): Integer; cdecl; external;

function deflateEnd(var strm: z_stream): Integer; cdecl; external;

function inflate(var strm: z_stream; flush: Integer): Integer; cdecl; external;

function inflateEnd(var strm: z_stream): Integer; cdecl; external;

function deflateInit_(var strm: z_stream; level: Integer;
  version: PAnsiChar; stream_size: Integer): Integer; cdecl; external;

function inflateInit_(var strm: z_stream;
  version: PAnsiChar; stream_size: Integer): Integer; cdecl; external;


type
  TZAlloc = alloc_func;
  TZFree = free_func;
  TZStreamRec = z_stream;

function zcAlloc(AppData: Pointer; Items, Size: Cardinal): Pointer; cdecl;
procedure zcFree(AppData, Block: Pointer); cdecl;
procedure memset(P: Pointer; B: byte; Count: integer); cdecl;
procedure memcpy(dest, Source: Pointer; Count: integer); cdecl;

type
  EZLibError = class(Exception);
  ECompressionError = class(EZLibError);
  EDecompressionError = class(EZLibError);

const
  {** return code messages **************************************************}
  z_errmsg: array [0..9] of PAnsiChar = (
    'need dictionary',      // Z_NEED_DICT      (2)  //do not localize
    'stream end',           // Z_STREAM_END     (1)  //do not localize
    '',                     // Z_OK             (0)  //do not localize
    'file error',           // Z_ERRNO          (-1) //do not localize
    'stream error',         // Z_STREAM_ERROR   (-2) //do not localize
    'data error',           // Z_DATA_ERROR     (-3) //do not localize
    'insufficient memory',  // Z_MEM_ERROR      (-4) //do not localize
    'buffer error',         // Z_BUF_ERROR      (-5) //do not localize
    'incompatible version', // Z_VERSION_ERROR  (-6) //do not localize
    ''                                               //do not localize
    );

implementation

{//$IFDEF MSWINDOWS}
  {$IFDEF WIN32}
    {$L zlib32\deflate.obj}
    {$L zlib32\inflate.obj}
    {$L zlib32\inftrees.obj}
    {$L zlib32\infback.obj}
    {$L zlib32\inffast.obj}
    {$L zlib32\trees.obj}
    {$L zlib32\compress.obj}
    {$L zlib32\adler32.obj}
    {$L zlib32\crc32.obj}
  {$ENDIF WIN32}
  {$IFDEF WIN64}
    {$L zlib64\deflate.obj}
    {$L zlib64\inflate.obj}
    {$L zlib64\inftrees.obj}
    {$L zlib64\infback.obj}
    {$L zlib64\inffast.obj}
    {$L zlib64\trees.obj}
    {$L zlib64\compress.obj}
    {$L zlib64\adler32.obj}
    {$L zlib64\crc32.obj}
  {$ENDIF WIN64}
{//s$ENDIF}


function zcAlloc(AppData: Pointer; Items, Size: Cardinal): Pointer;
begin
  Result := AllocMem(Items * Size);
end;

procedure zcFree(AppData, Block: Pointer);
begin
  FreeMem(Block);
end;

procedure memset(P: Pointer; B: byte; Count: integer); cdecl;
begin
  FillChar(P^, Count, B);
end;

procedure memcpy(dest, Source: Pointer; Count: integer); cdecl;
begin
  Move(Source^, dest^, Count);
end;

{$IFDEF WIN32}procedure _llmod; asm jmp System.@_llmod end;{$ENDIF}

function CCheck(code: integer): integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create('error '+IntToStr(code)); //!!
end;

function DCheck(code: integer): integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create('error');  //!!
end;

procedure ZLIBCompressBuf(const InBuf: Pointer; InBytes: integer;
  out OutBuf: Pointer; out OutBytes: integer;
  compMode: byte = 1);
var
  strm: TZStreamRec;
  P:    Pointer;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := zcAlloc;
  strm.zfree  := zcFree;
  OutBytes    := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  //  GetMem(OutBuf, OutBytes);
  OutBuf      := AllocMem(OutBytes);
  try
    strm.next_in   := InBuf;
    strm.avail_in  := InBytes;
    strm.next_out  := OutBuf;
    strm.avail_out := OutBytes;
    CCheck(deflateInit_(strm, compMode, zlib_version, sizeof(strm)));
    try
      while CCheck(deflate(strm, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
{$IFNDEF WIN64}
        strm.next_out  := PByte(integer(OutBuf) + (integer(strm.next_out) - integer(P)));
{$ELSE}
        strm.next_out  := PByte(NativeInt(OutBuf) + (NativeInt(strm.next_out) - NativeInt(P)));
{$ENDIF}
        strm.avail_out := 256;
      end;
    finally
      CCheck(deflateEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;


procedure ZLIBDecompressBuf(const InBuf: Pointer; InBytes: integer;
  OutEstimate: integer; out OutBuf: Pointer; out OutBytes: integer);
label
  m_exit;
var
  zstream: TZStreamRec;
  delta: integer;
  x: integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  delta := (InBytes + 255) and not 255;

  if outEstimate = 0 then
    outBytes := delta
  else
    outBytes := outEstimate;

  GetMem(outBuf, outBytes);

  //  try
  zstream.next_in   := inBuf;
  zstream.avail_in  := InBytes;
  zstream.next_out  := outBuf;
  zstream.avail_out := outBytes;

  //    DCheck(InflateInit_(zstream,zlib_version,sizeof(zstream)));
  if (InflateInit_(zstream, zlib_version, sizeof(zstream)) < 0) then
    goto m_exit;

  //    try
  while (True) do
  begin
    //      DCheck(inflate(zstream,Z_NO_FLUSH)) <> Z_STREAM_END
    x := inflate(zstream, Z_NO_FLUSH);
    if (x < 0) then
      goto m_exit;

    if (x = Z_STREAM_END) then
      break;

    Inc(outBytes, delta);
    ReallocMem(outBuf, outBytes);
{$IFNDEF WIN64}
    zstream.next_out  := PByte(integer(outBuf) + integer(zstream.total_out));
{$ELSE}
    zstream.next_out  := PByte(NativeInt(outBuf) + zstream.total_out);
{$ENDIF}
    zstream.avail_out := delta;
  end; // while
       //    finally
       //      DCheck(inflateEnd(zstream));
  if (inflateEnd(zstream) < 0) then
    goto m_exit;
  //    end;

  ReallocMem(outBuf, zstream.total_out);
  outBytes := zstream.total_out;
  Exit;

  m_exit:
    inflateEnd(zstream);
  // error
  //  except
  if (outBuf <> nil) then
    FreeMem(outBuf);
  outBuf   := nil;
  outBytes := 0;
  //    raise;
  //  end;
end;


end.

