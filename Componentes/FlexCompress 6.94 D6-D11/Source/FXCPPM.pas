unit FXCPPM;

{$I FXCVer.Inc}
{$I CompilerDefines.inc}

{DEFINE PPM_USE_SYNC}  // memory economy for MultiThread:
                        // if defined then 1 compressor for all threads

interface

uses Sysutils, Classes, Windows;



const
  ppmd_version = 'G';



const

 PPM_MO: array [1..9] of Byte = (2,3,4, 5, 7, 8,10, 13, 16); // Model Order
 //PPM_SA: array [1..9] of Byte = (2,3,7,16,22,25,40,100,100); // MBytes RAM
 PPM_SA: array [1..9] of byte = (2, 3, 7, 8, 13, 30, 50, 100, 100);


 { PPMCompressBuf compresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure PPMCompressBuf(const InBuf: Pointer; InSize: Integer;
                         out OutBuf: Pointer; out OutSize: Integer;
                         CompressionMode: Integer = 9);


procedure PPMDecompressBuf(const InBuf: Pointer; InSize: Integer;
                           out OutBuf: Pointer; out OutSize: Integer);




implementation

procedure __InitExceptBlockLDTC; begin end;


{$ifdef WIN32}
  {$L PPMD32\ppmd.OBJ}
{$else}
  {$L PPMD64\ppmd.OBJ}
{$endif}



{$ifdef PPM_USE_SYNC}
var
  FCSect:                TRTLCriticalSection;

procedure LockForPPMCompression;
begin
  EnterCriticalSection(FCSect);
end;

procedure UnlockForPPMCompression;
begin
  LeaveCriticalSection(FCSect);
end;
{$endif}


function PPMCompressBuffer(inBuf  : Pointer;
                           inSize : Integer;
                           outBuf : Pointer;
										       Max_Order:integer=6;
                           SASize:integer=10) : Integer; external;

function PPMDecompressBuffer(
                            inBuf  : Pointer;
                            inSize : Integer;
                            outBuf : Pointer
                            ) : Integer; external;



procedure memset(P: Pointer; B: Byte; count: Integer);
begin
  FillChar(P^, count, B);
end;

procedure memcpy(dest, source: Pointer; count: Integer);
begin
  Move(source^, dest^, count);
end;


function aa_malloc(count : integer) : Pointer;
begin
 result := AllocMem(count);
end;

procedure aa_free(buffer : Pointer);
begin
 FreeMem(buffer);
end;


procedure PPMCompressBuf(const InBuf: Pointer; InSize: Integer;
  out OutBuf: Pointer; out OutSize: Integer; CompressionMode: Integer);
begin
  try
    // some memory reserve for none-compressible data
    OutSize := InSize + InSize div 20 + 50;
    if CompressionMode < 1 then CompressionMode := 1;
    if CompressionMode > 9 then CompressionMode := 9;
{$ifdef PPM_USE_SYNC}
    LockForPPMCompression;
{$endif}
 try
   OutBuf := AllocMem(OutSize);
   OutSize := PPMCompressBuffer(
              InBuf,InSize,OutBuf,
              PPM_MO[CompressionMode],
              PPM_SA[CompressionMode]
              );
   ReallocMem(OutBuf, OutSize);
 finally
{$ifdef PPM_USE_SYNC}
   UnlockForPPMCompression;
{$endif}
 end;
except
 raise;
end;
if (OutSize <= 0) then
 raise Exception.Create('PPM Compression error');
end;

procedure PPMDecompressBuf(const InBuf: Pointer; InSize: Integer;
                           out OutBuf: Pointer; out OutSize: Integer);
begin
  if Outsize <=0 then
    raise Exception.Create('PPM DecpmpressBuf: OutSize must be set');
  try
{$ifdef PPM_USE_SYNC}
    LockForPPMCompression;
{$endif}
    try
      OutBuf := AllocMem(OutSize);
      OutSize := PPMDecompressBuffer(InBuf,InSize,OutBuf);
    finally
{$ifdef PPM_USE_SYNC}
      UnlockForPPMCompression;
{$endif}
    end;
  except
    raise;
  end;
  if (OutSize <= 0) then
    raise Exception.Create('PPM Decompression error');
end;

{$ifdef PPM_USE_SYNC}
initialization
  InitializeCriticalSection(FCSect);

finalization
  DeleteCriticalSection(FCSect);
{$endif}

end.
