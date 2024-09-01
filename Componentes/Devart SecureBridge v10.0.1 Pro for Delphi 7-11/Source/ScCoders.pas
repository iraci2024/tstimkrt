
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScCoders;
{$ENDIF}

interface

uses
  SysUtils, Classes, Math,
{$IFNDEF SBRIDGE}
{$IFNDEF EntityDAC}
  CRTypes, CRFunctions, CLRClasses;
{$ELSE}
  EntityDAC.Common.Types, EntityDAC.Common.Functions, EntityDAC.Common.CLRClasses;
{$ENDIF}{$ELSE}
  ScTypes, ScFunctions, ScCLRClasses;
{$ENDIF}

type
  TScCoderClass = class of TScCoder;

  TScCoder = class
  protected
    FFileName: string;
    FPermissionCode: integer;

    FRequiredBlockSize: integer;
    FIsProcessedSingleBuffer: boolean;
    FEncodeTable: TBytes;
    FDecodeTable: array of integer;
    procedure Init(const CodingStr: string; PaddingVal: char);

    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); virtual; abstract;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); virtual; abstract;

    procedure DoBeforeEncode; virtual;
    function GetPrefix: TBytes; virtual;
    function GetPostfix: TBytes; virtual;
    procedure ParsePrefix(const Data: TBytes; Offset, Count: integer; out PrefixCount: integer); virtual;
    procedure ParsePostfix(const Data: TBytes; Offset, Count: integer; out PostfixCount: integer); virtual;
  public
    constructor Create; virtual;

    function Encode(const Data: string): string; overload;
    function Encode(const Data: TBytes): TBytes; overload;
    function Encode(const Data: TBytes; Offset, Count: integer): TBytes; overload;
    procedure Encode(InStream, OutStream: TStream; Count: integer = -1); overload;

    function Decode(const Data: string): string; overload;
    function Decode(const Data: TBytes): TBytes; overload;
    function Decode(const Data: TBytes; Offset, Count: integer): TBytes; overload;
    procedure Decode(InStream, OutStream: TStream; Count: integer = -1); overload;

    property FileName: string read FFileName write FFileName;
    property PermissionCode: integer read FPermissionCode write FPermissionCode;
  end;

  TScCustomBase64Coder = class(TScCoder)
  protected
    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); override;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); override;
  end;

  TScBase64Coder = class(TScCustomBase64Coder)
  public
    constructor Create; override;
  end;

  // rfc 1741
  TScBinHex4Coder = class(TScCustomBase64Coder)
  private
    function CalcCRC(const Buffer: TBytes; Offset, Count: integer): word;

  protected
    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); override;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); override;
  public
    constructor Create; override;
  end;

  TScCustomCoderUUE = class(TScCustomBase64Coder)
  protected
    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); override;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); override;

    function GetPrefix: TBytes; override;
    function GetPostfix: TBytes; override;
    procedure ParsePrefix(const Data: TBytes; Offset, Count: integer; out PrefixCount: integer); override;
    procedure ParsePostfix(const Data: TBytes; Offset, Count: integer; out PostfixCount: integer); override;
  public
    constructor Create; override;
  end;

  TScCoderUUE = class(TScCustomCoderUUE)
  public
    constructor Create; override;
  end;

  TScCoderXXE = class(TScCustomCoderUUE)
  public
    constructor Create; override;
  end;

  TScCoderQuotedPrintable = class(TScCoder)
  private
    FCurLinePos: integer;
  protected
    procedure DoBeforeEncode; override;
    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); override;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); override;
  public
  end;

  TScCoderQuotedPrintableSpecial = class(TScCoder)
  private
    FSpecials: TBytes;
    FPrefix: TBytes;
    FPostfix: TBytes;
  protected
    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); override;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); override;
  public
    procedure Init(const Specials, Prefix, Postfix: string);
  end;

  TScCoderMIME = class(TScCustomBase64Coder)
  protected
    procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount: integer); override;

    procedure InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
      var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean); override;
  public
    constructor Create; override;
  end;

implementation

const
  BUFFER_SIZE = 64 * 1024 - 1; // must be divisible by 3
  INDEX_TABLE_SIZE = 64;
  EOL = #13#10;
  PRINTABLE_LINE_LENGTH = 73;

  Base64_CODING_TABLE  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  BinHex4_CODING_TABLE = '!"#$%&''()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr';
  BinHex4_IDENT_STR = '(This file must be converted with BinHex 4.0)'#13#10;
  UUE_CODING_TABLE = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  XXE_CODING_TABLE = '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

  TwoHexUpperLookup: packed array[0..255] of array[1..2] of Char =
  ('00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
   '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
   '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
   '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
   '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
   '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
   '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
   '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
   '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
   '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
   'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
   'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
   'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
   'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
   'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
   'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF');

{ TScCoder }

constructor TScCoder.Create;
begin
  inherited Create;

  FIsProcessedSingleBuffer := False;
  FRequiredBlockSize := 0;
end;

procedure TScCoder.Init(const CodingStr: string; PaddingVal: char);
var
  i, c: integer;
begin
  if Length(CodingStr) <> INDEX_TABLE_SIZE then
    raise ArgumentException.Create('CodingStr');

  SetLength(FEncodeTable, INDEX_TABLE_SIZE + 1{Padding});
  SetLength(FDecodeTable, 128);

  for i := 1 to Length(CodingStr) do
    FEncodeTable[i - 1] := Ord(CodingStr[i]);

  FEncodeTable[INDEX_TABLE_SIZE] := Ord(PaddingVal);

  for i := 0 to Length(FDecodeTable) - 1 do
    FDecodeTable[i] := -1;

  c := 0;
  for i := 1 to Length(CodingStr) do begin
    FDecodeTable[Ord(CodingStr[i])] := c;
    Inc(c);
  end;
end;

procedure TScCoder.DoBeforeEncode;
begin

end;

function TScCoder.GetPrefix: TBytes;
begin
  SetLength(Result, 0);
end;

function TScCoder.GetPostfix: TBytes;
begin
  SetLength(Result, 0);
end;

procedure TScCoder.ParsePrefix(const Data: TBytes; Offset, Count: integer; out PrefixCount: integer);
begin
  PrefixCount := 0;
end;

procedure TScCoder.ParsePostfix(const Data: TBytes; Offset, Count: integer; out PostfixCount: integer);
begin
  PostfixCount := 0;
end;

function TScCoder.Encode(const Data: string): string;
var
  ResBuf: TBytes;
begin
  ResBuf := Encode(Encoding.ASCII.GetBytes(Data));
  Result := Encoding.ASCII.GetString(ResBuf);
end;

function TScCoder.Encode(const Data: TBytes): TBytes;
begin
  Result := Encode(Data, 0, Length(Data));
end;

function TScCoder.Encode(const Data: TBytes; Offset, Count: integer): TBytes;
var
  Prefix, Postfix: TBytes;
  OutOffset, OutCount: integer;
begin
  DoBeforeEncode;
  Prefix := GetPrefix;
  Postfix := GetPostfix;

  if Length(Prefix) > 0 then begin
    OutCount := ((Count + 2) div 3) * 4 + Length(Prefix) + Length(Postfix);
    SetLength(Result, OutCount);

    Move(Prefix[0], Result[0], Length(Prefix));
    OutOffset := Length(Prefix);
  end
  else
    OutOffset := 0;

  InternalEncode(Data, Offset, Count, Result, OutOffset, OutCount);
  Inc(OutOffset, OutCount);

  if Length(Postfix) > 0 then begin
    if Length(Result) < OutOffset + Length(Postfix) then
      SetLength(Result, OutOffset + Length(Postfix));

    Move(Postfix[0], Result[OutOffset], Length(Postfix));
    Inc(OutOffset, Length(Postfix));
  end;

  if Length(Result) <> OutOffset then
    SetLength(Result, OutOffset);
end;

procedure TScCoder.Encode(InStream, OutStream: TStream; Count: integer = -1);
var
  Prefix, Postfix: TBytes;
  StreamSize: Int64;
  BufferSize: integer;
  InBuf, OutBuf: TBytes;
  InCount, OutCount: integer;
begin
  if (InStream = nil) or (OutStream = nil) or (InStream = OutStream) then
    raise ArgumentException.Create('Invalid input arguments InStream, OutStream');

  DoBeforeEncode;

  if Count < 0 then
    StreamSize := InStream.Size - InStream.Position
  else
    StreamSize := Min(InStream.Size - InStream.Position, Count);

  if not FIsProcessedSingleBuffer and (StreamSize > BUFFER_SIZE) then begin
    if FRequiredBlockSize > 0 then
      BufferSize := (BUFFER_SIZE div FRequiredBlockSize) * FRequiredBlockSize
    else
      BufferSize := BUFFER_SIZE;
  end
  else
    BufferSize := integer(StreamSize);

  // Assert((BufferSize mod 3) = 0);
  SetLength(InBuf, BufferSize);
  SetLength(OutBuf, BufferSize);
  InCount := BufferSize;

  Prefix := GetPrefix;
  if Length(Prefix) > 0 then
    OutStream.WriteBuffer(Prefix[0], Length(Prefix));

  while StreamSize > 0 do begin
    if StreamSize < BufferSize then
      InCount := StreamSize;

    InStream.ReadBuffer(InBuf[0], InCount);
    InternalEncode(InBuf, 0, InCount, OutBuf, 0, OutCount);
    OutStream.WriteBuffer(OutBuf[0], OutCount);
    Dec(StreamSize, InCount);
  end;

  Postfix := GetPostfix;
  if Length(Postfix) > 0 then
    OutStream.WriteBuffer(Postfix[0], Length(Postfix));
end;

function TScCoder.Decode(const Data: string): string;
var
  ResBuf: TBytes;
begin
  ResBuf := Decode(Encoding.ASCII.GetBytes(Data));
  Result := Encoding.ASCII.GetString(ResBuf);
end;

function TScCoder.Decode(const Data: TBytes): TBytes;
begin
  Result := Decode(Data, 0, Length(Data));
end;

function TScCoder.Decode(const Data: TBytes; Offset, Count: integer): TBytes;
var
  IsFinished: boolean;
  ResCount, UnReadCount: integer;
begin
  SetLength(Result, (Count * 3) div 4);
  IsFinished := True;

  ParsePrefix(Data, Offset, Count, ResCount);
  Inc(Offset, ResCount);
  Inc(Count, ResCount);

  InternalDecode(Data, Offset, Count, Result, 0, ResCount, UnReadCount, IsFinished);
  if Length(Result) > ResCount then
    SetLength(Result, ResCount);

  ParsePostfix(Data, Offset, Count, ResCount);
end;

procedure TScCoder.Decode(InStream, OutStream: TStream; Count: integer = -1);
var
  IsFinished: boolean;
  StreamSize: Int64;
  BufferSize: integer;
  InBuf, OutBuf: TBytes;
  InCount, OutCount, UnReadCount: integer;
  PrefixCount: integer;
begin
  if (InStream = nil) or (OutStream = nil) or (InStream = OutStream) then
    raise ArgumentException.Create('Invalid input arguments InStream, OutStream');

  if Count < 0 then
    StreamSize := InStream.Size - InStream.Position
  else
    StreamSize := Min(InStream.Size - InStream.Position, Count);

  if not FIsProcessedSingleBuffer and (StreamSize > BUFFER_SIZE) then
    BufferSize := BUFFER_SIZE
  else
    BufferSize := integer(StreamSize);

  SetLength(InBuf, BufferSize);
  SetLength(OutBuf, BufferSize);

  InCount := InStream.Read(InBuf[0], BufferSize);
  if InCount = 0 then
    Exit;

  ParsePrefix(InBuf, 0, InCount, PrefixCount);
  InStream.Position := InStream.Position - (InCount - PrefixCount);
  Dec(StreamSize, PrefixCount);

  IsFinished := False;
  InCount := BufferSize;
  while StreamSize > 0 do begin
    if StreamSize <= BufferSize then begin
      InCount := StreamSize;
      IsFinished := True;
      if InCount = 0 then
        break;
    end;

    InStream.ReadBuffer(InBuf[0], InCount);
    InternalDecode(InBuf, 0, InCount, OutBuf, 0, OutCount, UnReadCount, IsFinished);
    OutStream.WriteBuffer(OutBuf[0], OutCount);
    Dec(StreamSize, InCount - UnReadCount);

    if UnReadCount > 0 then
      InStream.Position := InStream.Position - UnReadCount;

    if IsFinished then
      break;
  end;

  OutStream.Size := OutStream.Position;
end;

{ TScCustomBase64Coder }

procedure TScCustomBase64Coder.InternalEncode(const Data: TBytes; Offset, Count: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  x1, x2, x3, x4: Byte;
  i, j, r, n: integer;
begin
  r := Count mod 3;
  if r <> 0 then
    OutCount := (Count div 3) * 4 + 4
  else
    OutCount := (Count div 3) * 4;

  if Length(OutBuf) < OutOffset + OutCount then
    SetLength(OutBuf, OutOffset + OutCount);

  n := Offset + (Count - r);
  i := Offset;
  j := OutOffset;
  while i < n do begin
    x1 := (Data[i] {and $fc}) shr 2;
    x2 := ((Data[i    ] and $03) shl 4) or ((Data[i + 1] {and $f0}) shr 4);
    x3 := ((Data[i + 1] and $0f) shl 2) or ((Data[i + 2] {and $c0}) shr 6);
    x4 := (Data[i + 2] and $3f);
    OutBuf[j    ] := FEncodeTable[x1];
    OutBuf[j + 1] := FEncodeTable[x2];
    OutBuf[j + 2] := FEncodeTable[x3];
    OutBuf[j + 3] := FEncodeTable[x4];
    i := i + 3;
    j := j + 4;
  end;

  if r <> 0 then begin
    x1 := (Data[i] {and $fc}) shr 2;
    x2 := (Data[i] and $03) shl 4;
    if r = 2 then begin
      x2 := x2 or ((Data[i + 1] {and $f0}) shr 4);
      x3 := (Data[i + 1] and $0f) shl 2;
    end
    else
      x3 := INDEX_TABLE_SIZE;
    x4 := INDEX_TABLE_SIZE;
    OutBuf[j    ] := FEncodeTable[x1];
    OutBuf[j + 1] := FEncodeTable[x2];
    OutBuf[j + 2] := FEncodeTable[x3];
    OutBuf[j + 3] := FEncodeTable[x4];
  end;
end;

procedure TScCustomBase64Coder.InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean);
var
  LastInOffset: integer;
  i, j, c, v: integer;
  Bits: integer;
begin
  OutCount := (InCount * 3) div 4;
  if Length(OutBuf) < OutOffset + OutCount then
    SetLength(OutBuf, OutOffset + OutCount);

  OutCount := 0;
  LastInOffset := InOffset;

  Bits := 0;
  v := 0;
  j := OutOffset;
  for i := InOffset to InOffset + InCount - 1 do begin
    if InBuf[i] > 127 then
      continue;

    c := FDecodeTable[InBuf[i]];
    if c < 0 then begin
      if InBuf[i] = FEncodeTable[INDEX_TABLE_SIZE] then begin
        if i >= InOffset + InCount - 2 then
          LastInOffset := InOffset + InCount;

        IsFinished := True;
        break;
      end;
      continue;
    end;

    v := (v shl 6) or c;
    Bits := Bits + 6;
    if Bits >= 8 then begin
      Bits := Bits - 8;
      OutBuf[j] := byte(v shr Bits);
      Inc(j);
      if Bits = 0 then begin
        LastInOffset := i + 1;
        OutCount := j;
      end;
    end;
  end;

  if IsFinished then
    OutCount := j;
  if OutCount > 0 then
    OutCount := OutCount - OutOffset;

  UnReadCount := InCount - (LastInOffset - InOffset);
end;

{ TScBase64Coder }

constructor TScBase64Coder.Create;
begin
  inherited;

  Init(Base64_CODING_TABLE, '=');
end;

{ TScBinHex4Coder }

constructor TScBinHex4Coder.Create;
begin
  inherited;

  Init(BinHex4_CODING_TABLE, '=');
  FIsProcessedSingleBuffer := True;
end;

function TScBinHex4Coder.CalcCRC(const Buffer: TBytes; Offset, Count: integer): word;
var
  B: byte;
  Shifted: boolean;
  i, n: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do begin
    B := Buffer[Offset + i];

    for n := 0 to 7 do begin
      Shifted := (Result and $8000) <> 0;
      Result := (Result shl 1) or (B shr 7);
      if Shifted then
        Result := Result xor $1021;
      B := byte(B shl 1);
    end;
  end;
end;

procedure TScBinHex4Coder.InternalEncode(const Data: TBytes; Offset, Count: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  InCount, InOffset, EncOutCount: integer;
  Mod3, Count90: integer;
  BlockCount, NewLength: integer;
  FileNameBuf, IdentStrBuf: TBytes;
  InBuf: TBytes;
  CRC: word;
  i: integer;
begin
  if FFileName = '' then
    raise ArgumentException.Create('FileName');

  FileNameBuf := Encoding.ANSI.GetBytes(FFileName);
  // BinHex 4.0 allows filename max 255 bytes
  if Length(FileNameBuf) > 255 then
    SetLength(FileNameBuf, 255);

  // $90 -> $90 $00
  Count90 := 0;
  for i := Offset to Offset + Count - 1 do begin
    if Data[i] = $90 then
      Inc(Count90);
  end;

  InCount := Length(FileNameBuf) + 1{Len} + 1{Ver} + 8{Type, Creator} + 2{Flags} + 4{Len} + 4{Len} + 2{CRC} + Count + Count90 + 2{CRC};

  // Block must be multiple by 3
  Mod3 := InCount mod 3;
  if Mod3 > 0 then
    Inc(InCount, 3 - Mod3);

  SetLength(InBuf, InCount);

  InBuf[0] := Length(FileNameBuf);
  if Length(FileNameBuf) > 0 then
    Move(FileNameBuf[0], InBuf[1], Length(FileNameBuf));
  InOffset := 1 + Length(FileNameBuf);

  InBuf[InOffset] := 0; // Version
  Inc(InOffset);

  FillChar(InBuf[InOffset], 8, 32{' '}); // Type + Creator
  Inc(InOffset, 8);

  InBuf[InOffset] := 0;     // Flag
  InBuf[InOffset + 1] := 0; // Flag
  Inc(InOffset, 2);

  InBuf[InOffset] := byte(Count + Count90); // Data length
  InBuf[InOffset + 1] := byte((Count + Count90) shr 8);
  InBuf[InOffset + 2] := byte((Count + Count90) shr 16);
  InBuf[InOffset + 3] := byte((Count + Count90) shr 24);
  Inc(InOffset, 4);

  InBuf[InOffset] := 0; // Resource length
  InBuf[InOffset + 1] := 0;
  InBuf[InOffset + 2] := 0;
  InBuf[InOffset + 3] := 0;
  Inc(InOffset, 4);

  CRC := CalcCRC(InBuf, 0, InOffset);
  InBuf[InOffset] := byte(CRC);
  InBuf[InOffset + 1] := byte(CRC shr 8);
  Inc(InOffset, 2);

  Move(Data[Offset], InBuf[InOffset], Count);

  if Count90 > 0 then begin
    for i := InOffset to InOffset + Count - 1 do begin
      if InBuf[i] = $90 then begin
        Move(InBuf[i + 1], InBuf[i + 2], Count + InOffset - i - 1);
        InBuf[i + 1] := 0;
        Inc(Count);
      end;
    end;
  end;

  CRC := CalcCRC(InBuf, InOffset, Count);
  Inc(InOffset, Count);
  InBuf[InOffset] := byte(CRC);
  InBuf[InOffset + 1] := byte(CRC shr 8);
  Inc(InOffset, 2);

  EncOutCount := (((InOffset + 2) div 3) * 4) + 2{Start+End};
  // Blocks by 64 chars per line
  BlockCount := EncOutCount shr 6; // div 64

  IdentStrBuf := Encoding.ANSI.GetBytes(BinHex4_IDENT_STR);
  OutCount := EncOutCount + Length(IdentStrBuf);

  NewLength := OutOffset + OutCount + (BlockCount + 1) * 2{EOL};
  if Length(OutBuf) < NewLength then
    SetLength(OutBuf, NewLength);

  Move(IdentStrBuf[0], OutBuf[OutOffset], Length(IdentStrBuf));
  Inc(OutOffset, Length(IdentStrBuf));

  inherited InternalEncode(InBuf, 0, InOffset, OutBuf, OutOffset + 1, EncOutCount);
  Inc(EncOutCount, 2{Start+End});

  OutBuf[OutOffset] := 58; // Start ":"
  OutBuf[OutOffset + EncOutCount - 1] := 58; // End ":"

  // Write 64 chars per line + CRLF
  for i := 1 to BlockCount do begin
    Move(OutBuf[OutOffset + i * 64], OutBuf[OutOffset + i * 64 + 2], EncOutCount - i * 64);
    OutBuf[OutOffset + i * 64] := 13;
    OutBuf[OutOffset + i * 64 + 1] := 10;
    Inc(OutOffset, 2);
    Inc(OutCount, 2);
  end;

  EncOutCount := EncOutCount - BlockCount * 64;
  if EncOutCount > 0 then begin
    OutBuf[OutOffset + BlockCount * 64 + EncOutCount] := 13;
    OutBuf[OutOffset + BlockCount * 64 + EncOutCount + 1] := 10;
    Inc(OutCount, 2);
  end;
end;

procedure TScBinHex4Coder.InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean);
var
  StartPos, DataOffset, DataLength: integer;
  Seq: integer;
  i: integer;
begin
  OutCount := 0;
  UnReadCount := 0;

  // Don't check if the BinHex4_IDENT_STR string is present

  StartPos := -1;
  for i := InOffset to InOffset + InCount - 1 do begin
    if InBuf[i] = 58 then begin // ":"
      if StartPos = -1 then begin
        InOffset := i;
        StartPos := InOffset;
      end
      else begin
        // Second ":"
        InCount := StartPos - InOffset;
        StartPos := -2;
        break;
      end;
    end
    else
      if (StartPos > -1) and (InBuf[i] <> 13) and (InBuf[i] <> 10) then begin
        InBuf[StartPos] := InBuf[i];
        Inc(StartPos);
      end;
  end;

  if StartPos = -1 then
    raise InvalidDataException.Create('Missing a starting colon');
  if StartPos <> -2 then
    raise InvalidDataException.Create('Missing a terminating colon');

  if InCount = 0 then
    Exit;

  IsFinished := True;
  inherited InternalDecode(InBuf, InOffset, InCount, OutBuf, OutOffset, OutCount, UnReadCount, IsFinished);

  DataOffset := OutOffset + 1{Len} + OutBuf[OutOffset]; // skip filename
  Inc(DataOffset, 1{Ver} + 8{Type, Creator} + 2{Flags});
  DataLength := (OutBuf[DataOffset + 3] shl 24) + (OutBuf[DataOffset + 2] shl 16) + (OutBuf[DataOffset + 1] shl 8) + OutBuf[DataOffset];

  Inc(DataOffset, 4);
  if DataLength = 0 then
    // save resource fork
    DataLength := (OutBuf[DataOffset + 3] shl 24) + (OutBuf[DataOffset + 2] shl 16) + (OutBuf[DataOffset + 1] shl 8) + OutBuf[DataOffset];

  Inc(DataOffset, 4);
  Inc(DataOffset, 2);
  if DataLength > OutCount then
    DataLength := OutCount;

  // Uncompression ($90 - marker)
  i := DataOffset;
  while i < DataOffset + DataLength - 1 do begin
    if OutBuf[i] = $90 then begin
      if OutBuf[i + 1] = 0 then begin
        // 11 22 90 00 33 -> 11 22 90 33
        Move(OutBuf[i + 2], OutBuf[i + 1], DataOffset + DataLength - i - 2);
        Dec(DataLength, 1);
        Inc(i);
      end
      else
      if OutBuf[i + 1] = 1 then begin
        // 11 22 90 01 33 -> 11 22 33
        Move(OutBuf[i + 2], OutBuf[i], DataOffset + DataLength - i - 2);
        Dec(DataLength, 2);
      end
      else
      if OutBuf[i + 1] = 2 then begin
        // 11 22 90 02 33 -> 11 22 22 33
        OutBuf[i] := OutBuf[i - 1];
        Move(OutBuf[i + 2], OutBuf[i + 1], DataOffset + DataLength - i - 2);
        Dec(DataLength, 1);
        Inc(i);
      end
      else
      if OutBuf[i + 1] = 3 then begin
        // 11 22 90 03 33 -> 11 22 22 22 33
        OutBuf[i] := OutBuf[i - 1];
        OutBuf[i + 1] := OutBuf[i - 1];
        Inc(i, 2);
      end
      else begin
        Seq := OutBuf[i + 1];

        //11 22 90 04 33 -> 11 22 22 22 22 33
        OutBuf[i] := OutBuf[i - 1];
        OutBuf[i + 1] := OutBuf[i - 1];

        if Length(OutBuf) < DataOffset + DataLength + Seq - 3 then
          SetLength(OutBuf, DataOffset + DataLength + Seq - 3);

        Move(OutBuf[i + 2], OutBuf[i + Seq - 1], DataOffset + DataLength - i - 2);
        FillChar(OutBuf[i + 2], Seq - 3, OutBuf[i - 1]);
        Inc(DataLength, Seq - 3);
        Inc(i, Seq - 1);
      end;
    end
    else
      Inc(i);
  end;

  Move(OutBuf[DataOffset], OutBuf[OutOffset], DataLength);
  OutCount := DataLength;
end;

{ TScCustomCoderUUE }

constructor TScCustomCoderUUE.Create;
begin
  inherited;

  FRequiredBlockSize := 45;
end;

function TScCustomCoderUUE.GetPrefix: TBytes;
var
  Str: string;
begin
  Str := 'begin ' + IntToStr(FPermissionCode) + ' ' + FFileName + EOL;
  Result := Encoding.ANSI.GetBytes(Str);
end;

function TScCustomCoderUUE.GetPostfix: TBytes;
var
  Str: string;
begin
  Str := Chr(FEncodeTable[INDEX_TABLE_SIZE]) + EOL + 'end' + EOL;
  Result := Encoding.ANSI.GetBytes(Str);
end;

procedure TScCustomCoderUUE.ParsePrefix(const Data: TBytes; Offset, Count: integer; out PrefixCount: integer);
var
  Str: string;
  i: integer;
begin
  PrefixCount := 0;

  for i := 0 to Count - 2 do begin
    if (Data[i + Offset] = 13) and (Data[i + Offset + 1] = 10) then begin
      Str := Encoding.ANSI.GetString(Data, Offset, i);
      if Copy(Str, 1, 6) = 'begin ' then
        PrefixCount := i + 2;

      Exit;
    end;
  end;
end;

procedure TScCustomCoderUUE.ParsePostfix(const Data: TBytes; Offset, Count: integer; out PostfixCount: integer);
begin
  PostfixCount := 0;
end;

procedure TScCustomCoderUUE.InternalEncode(const Data: TBytes; Offset, Count: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  BlockSize, TmpOutCount: integer;
begin
  OutCount := ((Count + 44) div 45) * (60 + 1{Len} + 2{EOL});
  if Length(OutBuf) < OutOffset + OutCount then
    SetLength(OutBuf, OutOffset + OutCount);

  OutCount := 0;
  BlockSize := 45;

  while Count > 0 do begin
    if Count < 45 then
      BlockSize := Count;

    inherited InternalEncode(Data, Offset, BlockSize, OutBuf, OutOffset + 1, TmpOutCount);

    OutBuf[OutOffset] := FEncodeTable[BlockSize];
    Inc(OutOffset, TmpOutCount + 1);
    OutBuf[OutOffset] := 13;
    OutBuf[OutOffset + 1] := 10;
    Inc(OutOffset, 2);
    Inc(OutCount, TmpOutCount + 3);

    Inc(Offset, BlockSize);
    Dec(Count, BlockSize);
  end;
end;

procedure TScCustomCoderUUE.InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean);
var
  BlockSize, Expected, TmpOutCount: integer;
  TmpIsFinished: boolean;
begin
  OutCount := 0;
  UnReadCount := 0;

  while InCount > 0 do begin
    BlockSize := FDecodeTable[InBuf[InOffset]];
    if BlockSize < 0 then
      raise InvalidDataException.Create('Data contain illegal characters');

    Inc(InOffset);
    Dec(InCount);
    if (BlockSize = 0) or (InCount = 0) then begin
      UnReadCount := InCount;
      Exit;
    end;

    Expected := ((BlockSize + 2) div 3) * 4;
    if (Expected + 2{EOL}) > InCount then begin
      if IsFinished then begin
        if Expected > InCount then
          Expected := InCount;
      end
      else begin
        UnReadCount := InCount + 1{BlockSize};
        Exit;
      end;
    end;

    inherited InternalDecode(InBuf, InOffset, Expected, OutBuf, OutOffset, TmpOutCount, UnReadCount, TmpIsFinished);
    Inc(InOffset, Expected - UnReadCount);
    Dec(InCount, Expected - UnReadCount);
    Inc(OutOffset, TmpOutCount);
    Inc(OutCount, Min(BlockSize, TmpOutCount));

    if InCount > 1 then begin
      Inc(InOffset, 2{EOL});
      Dec(InCount, 2{EOL});
    end;
  end;
end;

{ TScCoderUUE }

constructor TScCoderUUE.Create;
begin
  inherited;

  Init(UUE_CODING_TABLE, '`');
end;

{ TScCoderXXE }

constructor TScCoderXXE.Create;
begin
  inherited;

  Init(XXE_CODING_TABLE, '+');
end;

{ TScCoderQuotedPrintable }

procedure TScCoderQuotedPrintable.DoBeforeEncode;
begin
  FCurLinePos := 0;
end;

procedure TScCoderQuotedPrintable.InternalEncode(const Data: TBytes; Offset, Count: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  EndPos: integer;
  i: integer;
begin
  OutCount := Count * 3 + ((Count div PRINTABLE_LINE_LENGTH) + 2) * 3;
  if Length(OutBuf) < OutOffset + OutCount then
    SetLength(OutBuf, OutOffset + OutCount);

  OutCount := 0;
  EndPos := Offset + Count - 1;
  for i := Offset to EndPos do begin
    if (Data[i] = 13{CR}) or (Data[i] = 10{LF}) then begin
      OutBuf[OutOffset] := Data[i];
      Inc(OutOffset);
      Inc(OutCount);
      FCurLinePos := 0;
    end
    else
    // !"#$%&''()*+,-./0123456789:;<>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmonpqrstuvwxyz{|}~
    if (Data[i] >= 33) and (Data[i] <= 126) and (Data[i] <> 61{=}) then begin
      if (Data[i] = 46{.}) and (FCurLinePos = 0) then begin
        OutBuf[OutOffset] := 61{=};
        OutBuf[OutOffset + 1] := Ord(TwoHexUpperLookup[Data[i]][1]);
        OutBuf[OutOffset + 2] := Ord(TwoHexUpperLookup[Data[i]][2]);
        Inc(OutOffset, 3);
        Inc(OutCount, 3);
        Inc(FCurLinePos, 3);
      end
      else begin
        OutBuf[OutOffset] := Data[i];
        Inc(OutOffset);
        Inc(OutCount);
        Inc(FCurLinePos);
      end;
    end
    else
    if ((Data[i] = 9{TAB}) or (Data[i] = 32{SP})) and (i < EndPos) then begin
      if (Data[i + 1] = 13{CR}) or (Data[i + 1] = 10{LF}) then begin
        OutBuf[OutOffset] := 61{=};
        OutBuf[OutOffset + 1] := Ord(TwoHexUpperLookup[Data[i]][1]);
        OutBuf[OutOffset + 2] := Ord(TwoHexUpperLookup[Data[i]][2]);
        Inc(OutOffset, 3);
        Inc(OutCount, 3);
        Inc(FCurLinePos, 3);
      end
      else begin
        OutBuf[OutOffset] := Data[i];
        Inc(OutOffset);
        Inc(OutCount);
        Inc(FCurLinePos);
      end;
    end
    else begin
      OutBuf[OutOffset] := 61{=};
      OutBuf[OutOffset + 1] := Ord(TwoHexUpperLookup[Data[i]][1]);
      OutBuf[OutOffset + 2] := Ord(TwoHexUpperLookup[Data[i]][2]);
      Inc(OutOffset, 3);
      Inc(OutCount, 3);
      Inc(FCurLinePos, 3);
    end;

    if (FCurLinePos >= PRINTABLE_LINE_LENGTH) and (i < EndPos) then begin
      OutBuf[OutOffset] := 61{=};
      OutBuf[OutOffset + 1] := 13;
      OutBuf[OutOffset + 2] := 10;
      Inc(OutOffset, 3);
      Inc(OutCount, 3);
      FCurLinePos := 0;
    end;
  end;
end;

procedure TScCoderQuotedPrintable.InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean);
var
  EndPos: integer;
  Skip: boolean;
  B: byte;
  i: integer;
begin
  OutCount := 0;
  UnReadCount := 0;

  if Length(OutBuf) < OutOffset + InCount then
    SetLength(OutBuf, OutOffset + InCount);

  EndPos := InOffset + InCount - 1;
  i := InOffset;

  while i <= EndPos do begin
    if InBuf[i] = 61{=} then begin
      if (i + 2) <= EndPos then begin
        if (InBuf[i + 1] = 13) and (InBuf[i + 2] = 10) then begin
          Inc(i, 3);
        end
        else begin
          Skip := False;
          Inc(i);
          case InBuf[i] of
            48..57: // 0-9
              B := InBuf[i] - 48;
            65..70: // A-F
              B := InBuf[i] - 65 + 10;
            97..102:// a-f
              B := InBuf[i] - 97 + 10;
            else begin
              B := 0;
              Skip := True;
            end;
          end;
          Inc(i);

          case InBuf[i] of
            48..57: // 0-9
              B := (B shl 4) or (InBuf[i] - 48);
            65..70: // A-F
              B := (B shl 4) or (InBuf[i] - 65 + 10);
            97..102:// a-f
              B := (B shl 4) or (InBuf[i] - 97 + 10);
            else
              Skip := True;
          end;
          Inc(i);

          if Skip then begin
            OutBuf[OutOffset] := 61{=};
            OutBuf[OutOffset + 1] := InBuf[i - 2];
            OutBuf[OutOffset + 2] := InBuf[i - 1];
            Inc(OutOffset, 3);
            Inc(OutCount, 3);
          end
          else begin
            OutBuf[OutOffset] := B;
            Inc(OutOffset);
            Inc(OutCount);
          end;
        end;
      end
      else begin
        if not IsFinished then
          UnReadCount := InCount - (i - InOffset);
        Break;
      end;
    end
    else
    if ((InBuf[i] >= 33) and (InBuf[i] <= 126)) or (InBuf[i] in [9, 10, 13, 32]) then begin
      OutBuf[OutOffset] := InBuf[i];
      Inc(OutOffset);
      Inc(OutCount);
      Inc(i);
    end
    else
      raise InvalidDataException.Create('Data contain illegal characters');
  end;
end;

{ TScCoderQuotedPrintableSpecial }

procedure TScCoderQuotedPrintableSpecial.Init(const Specials, Prefix, Postfix: string);
begin
  FSpecials := Encoding.ASCII.GetBytes(Specials);
  FPrefix := Encoding.ASCII.GetBytes(Prefix);
  FPostfix := Encoding.ASCII.GetBytes(Postfix);
end;

procedure TScCoderQuotedPrintableSpecial.InternalEncode(const Data: TBytes;
  Offset, Count: integer; var OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  EndPos, LinePos: integer;
  i: integer;
begin
  OutCount := Count * 3 + ((Count div PRINTABLE_LINE_LENGTH) + 2) * 3;
  if Length(OutBuf) < OutOffset + OutCount then
    SetLength(OutBuf, OutOffset + OutCount);

  OutCount := 0;
  EndPos := Offset + Count - 1;
  LinePos := Length(FPrefix) + Length(FPostfix);

  for i := Offset to EndPos do begin
    if (Data[i] >= 33) and (Data[i] <= 126) and not ByteInSet(Data[i], FSpecials) then begin
      OutBuf[OutOffset] := Data[i];
      Inc(OutOffset);
      Inc(OutCount);
      Inc(LinePos);
    end
    else
    if Data[i] = 32{' '} then begin
      OutBuf[OutOffset] := 95{_};
      Inc(OutOffset);
      Inc(OutCount);
      Inc(LinePos);
    end
    else begin
      OutBuf[OutOffset] := 61{=};
      OutBuf[OutOffset + 1] := Ord(TwoHexUpperLookup[Data[i]][1]);
      OutBuf[OutOffset + 2] := Ord(TwoHexUpperLookup[Data[i]][2]);
      Inc(OutOffset, 3);
      Inc(OutCount, 3);
      Inc(LinePos, 3);
    end;

    if (LinePos >= PRINTABLE_LINE_LENGTH) and (i < EndPos) then begin
      Move(FPostfix[0], OutBuf[OutOffset], Length(FPostfix));
      Inc(OutOffset, Length(FPostfix));
      Inc(OutCount, Length(FPostfix));

      OutBuf[OutOffset] := 13;
      OutBuf[OutOffset + 1] := 10;
      OutBuf[OutOffset + 2] := 32{' '};
      Inc(OutOffset, 3);
      Inc(OutCount, 3);

      Move(FPrefix[0], OutBuf[OutOffset], Length(FPrefix));
      Inc(OutOffset, Length(FPrefix));
      Inc(OutCount, Length(FPrefix));

      LinePos := Length(FPrefix) + Length(FPostfix);
    end;
  end;
end;

procedure TScCoderQuotedPrintableSpecial.InternalDecode(const InBuf: TBytes;
  InOffset, InCount: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer;
  var IsFinished: boolean);
var
  EndPos: integer;
  Skip: boolean;
  B: byte;
  i: integer;
begin
  OutCount := 0;
  UnReadCount := 0;

  if Length(OutBuf) < OutOffset + InCount then
    SetLength(OutBuf, OutOffset + InCount);

  EndPos := InOffset + InCount - 1;
  i := InOffset;

  while i <= EndPos do begin
    if InBuf[i] = 61{=} then begin
      if (i + 2) <= EndPos then begin
        if (InBuf[i + 1] = 13) and (InBuf[i + 2] = 10) then begin
          Inc(i, 3);
        end
        else begin
          Skip := False;
          Inc(i);
          case InBuf[i] of
            48..57: // 0-9
              B := InBuf[i] - 48;
            65..70: // A-F
              B := InBuf[i] - 65 + 10;
            97..102:// a-f
              B := InBuf[i] - 97 + 10;
            else begin
              B := 0;
              Skip := True;
            end;
          end;
          Inc(i);

          case InBuf[i] of
            48..57: // 0-9
              B := (B shl 4) or (InBuf[i] - 48);
            65..70: // A-F
              B := (B shl 4) or (InBuf[i] - 65 + 10);
            97..102:// a-f
              B := (B shl 4) or (InBuf[i] - 97 + 10);
            else
              Skip := True;
          end;
          Inc(i);

          if Skip then
            B := 32{' '};

          OutBuf[OutOffset] := B;
          Inc(OutOffset);
          Inc(OutCount);
        end;
      end
      else begin
        if not IsFinished then
          UnReadCount := InCount - (i - InOffset);
        Break;
      end;
    end
    else
    if InBuf[i] = 95{_} then begin
      OutBuf[OutOffset] := 32{' '};
      Inc(OutOffset);
      Inc(OutCount);
      Inc(i);
    end
    else begin
      OutBuf[OutOffset] := InBuf[i];
      Inc(OutOffset);
      Inc(OutCount);
      Inc(i);
    end;
  end;
end;

{ TScCoderMIME }

constructor TScCoderMIME.Create;
begin
  inherited;

  Init(Base64_CODING_TABLE, '=');

  FRequiredBlockSize := 57;
end;

procedure TScCoderMIME.InternalEncode(const Data: TBytes; Offset, Count: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  BlockSize: integer;
  TmpOutCount: integer;
begin
  OutCount := ((Count + 56) div 57) * (76 + 2{EOL});
  if Length(OutBuf) < OutOffset + OutCount then
    SetLength(OutBuf, OutOffset + OutCount);

  OutCount := 0;
  BlockSize := 57;

  while Count > 0 do begin
    if Count < 57 then
      BlockSize := Count;

    inherited InternalEncode(Data, Offset, BlockSize, OutBuf, OutOffset, TmpOutCount);

    Inc(OutOffset, TmpOutCount);
    Inc(OutCount, TmpOutCount);
//    if TmpOutCount >= 76 then begin
      OutBuf[OutOffset] := 13;
      OutBuf[OutOffset + 1] := 10;
      Inc(OutOffset, 2);
      Inc(OutCount, 2);
//    end;

    Inc(Offset, BlockSize);
    Dec(Count, BlockSize);
  end;
end;

procedure TScCoderMIME.InternalDecode(const InBuf: TBytes; InOffset, InCount: integer;
  var OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinished: boolean);
var
  EOLOffset, EOLCount, TmpOutCount: integer;
  TmpIsFinished: boolean;
begin
  OutCount := 0;
  UnReadCount := 0;

  while InCount > 0 do begin
    EOLOffset := 0;
    EOLCount := 0;
    while EOLOffset < InCount do begin
      if InBuf[InOffset + EOLOffset] = 13 then begin
        if ((EOLOffset + 1 < InCount) and (InBuf[InOffset + EOLOffset + 1] = 10)) then begin
          Dec(EOLOffset);
          EOLCount := 2;
          Break;
        end
        else
        if IsFinished then begin
          Dec(EOLOffset);
          EOLCount := 1;
          Break;
        end
        else begin
          UnReadCount := InCount;
          Exit;
        end;
      end;
      Inc(EOLOffset);
    end;

    if (EOLCount = 0) and not IsFinished then begin
      UnReadCount := InCount;
      Exit;
    end;

    if EOLOffset <= 0 then
      Exit;

    Inc(EOLOffset);
    inherited InternalDecode(InBuf, InOffset, EOLOffset, OutBuf, OutOffset, TmpOutCount, UnReadCount, TmpIsFinished);
    Inc(InOffset, EOLOffset + EOLCount - UnReadCount);
    Dec(InCount, EOLOffset + EOLCount - UnReadCount);
    Inc(OutOffset, TmpOutCount);
    Inc(OutCount, TmpOutCount);

    if (UnReadCount > 0) and not IsFinished then
      Exit;
  end;
end;

end.
