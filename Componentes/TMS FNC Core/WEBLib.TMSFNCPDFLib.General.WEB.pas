{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
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

unit WEBLib.TMSFNCPDFLib.General.WEB;

{$I WEBLib.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types, WEBLib.TMSFNCPDFCoreLibBase,
  WEBLib.Graphics, WEBLib.TMSFNCTypes, WEBLib.TMSFNCUtils
  {$IFDEF WEBLIB}
  ,Contnrs
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  {$IFDEF WEBLIB}
  Int8    = ShortInt;
  Int16   = SmallInt;
  Int32   = Integer;
  UInt8   = Byte;
  UInt16  = Word;
  UInt32  = Cardinal;
  {$ENDIF}

  TTMSFNCGeneralPDFLibFontMetrics = record
    CapHeight: Integer;
    Ascent: Integer;
    Descent: Integer;
    FontBox: TRect;
    ItalicAngle: Integer;
    Fixed: Boolean;
    TrueType: Boolean;
  end;

  {$IFDEF WEBLIB}

  { TTMSFNCGeneralPDFLibFontItem }

  TTMSFNCGeneralPDFLibFontItem = class
  private
    FAscent: Integer;
    FBBox: TRect;
    FDescent: Integer;
    FCapHeight: Integer;
    FFileName, FName: string;
    FItalicAngle: Single;
    FFixed: Boolean;
    FStyle: TFontStyles;
    FData: TTMSFNCUtilsFile;
  public
    property FileName: string read FFileName write FFileName;
    property Data: TTMSFNCUtilsFile read FData write FData;
    property Name: String read FName write FName;
    property Style: TFontStyles read FStyle write FStyle;
    property Fixed: Boolean read FFixed write FFixed;
    property BBox: TRect read FBBox write FBBox;
    property Ascent: Integer read FAscent write FAscent;
    property Descent: Integer read FDescent write FDescent;
    property CapHeight: Integer read FCapHeight write FCapHeight;
    property ItalicAngle: Single read FItalicAngle write FItalicAngle;
  end;

  TTMSFNCGeneralPDFLibFontItems = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCGeneralPDFLibFontItem;
    procedure SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibFontItem);
  public
    property Items[Index: Integer]: TTMSFNCGeneralPDFLibFontItem read GetItem write SetItem; default;
  end;
  {$ENDIF}

  { TTMSFNCGeneralPDFLibInitializer }

  TTMSFNCGeneralPDFLibInitializer = class
  private
    {$IFDEF WEBLIB}
    FFontCol: TTMSFNCGeneralPDFLibFontItems;
    {$ENDIF}
  protected
    {$IFDEF WEBLIB}
    procedure InternalInitializeFontProperties(AStream: TStream; var AFamilyName: string; var ABold: Boolean; var AItalic: Boolean; var AFixed: Boolean; var AAscent: Integer; var ADescent: Integer; var AItalicAngle: Single; var ACapHeight: Integer; var AFontBox: TRect);
    {$ENDIF}
  public
    constructor Create;
    procedure InitializeFontFallBackList(AList: TStrings);
    destructor Destroy; override;
  end;

  { TTMSFNCGeneralPDFLibFontInitializer }

  TTMSFNCGeneralPDFLibFontInitializer = class
  private
    {$IFDEF WEBLIB}
    FTTFMetrics: TTMSFNCGeneralPDFLibFontMetrics;
    FTTFDataStream: TStringStream;
    FTTFFileStream: TMemoryStream;
    FTTFCreateFontPackage: Boolean;
    FTTFEmbed: TMemoryStream;
    {$ENDIF}
    FFullFont: Boolean;
    FBase: string;
    FSize: Single;
    FUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    FGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    FCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    FCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    FUnitsPerEm: UInt32;
    FMainInitializer: TTMSFNCGeneralPDFLibInitializer;
    FIsFixedWidth: Boolean;
  protected
    {$IFDEF WEBLIB}
    procedure InternalInitializeCharWidths(AStream: TStream);
    function CompressFontStream(AStream: TMemoryStream): TStringStream;
    {$ENDIF}
    procedure InternalInitializeFontFile;
  public
    constructor Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const {%H-}AStyle: TFontStyles; const ASize: Single);
    destructor Destroy; override;
    function GetFontMetrics: TTMSFNCGeneralPDFLibFontMetrics;
    function GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    function GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    function GetUnitsPerEM: UInt32;
    function GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetGlyphIds: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function UnitsPerEm: UInt32;
    function GetTTFDataLength: Integer;
    function GetTTFDataCompressedLength: Int64;
    function GetTTFDataCompressed: TStringStream;
    procedure CompressTTFData;
    procedure InitializeCharWidths;
    procedure InitializeFontFile;
    property IsFixedWidth: Boolean read FIsFixedWidth write FIsFixedWidth;
    property FullFont: Boolean read FFullFont write FFullFont;
  end;

implementation

uses
  Math, SysUtils, WEBLib.TMSFNCPDFLib;

{$IFDEF WEBLIB}

{ TTMSFNCGeneralPDFLibFontTTFChar }
type
  TTMSFNCGeneralPDFLibFontTTFChar = class
  private
    FCharCode: Integer;
    FDescription: PChar;
    FNewIndex: Integer;
    FOrgIndex: Integer;
  public
    constructor Create(const ACharCode: Integer; const  ANewIndex: Integer; const AOrgIndex: Integer; const ADescription: PChar);
    property CharCode: Integer read FCharCode write FCharCode;
    property NewIndex: Integer read FNewIndex write FNewIndex;
    property OrgIndex: Integer read FOrgIndex write FOrgIndex;
    property Description: PChar read FDescription write FDescription;
  end;

  TTMSFNCGeneralPDFLibFontNameRecord = record
    pID: UInt16;
    eID: UInt16;
    lID: UInt16;
    nID: UInt16;
    sl: UInt16;
    so: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontNameEntry = record
    i: TTMSFNCGeneralPDFLibFontNameRecord;
    v : String;
  end;
  TTMSFNCGeneralPDFLibFontNameEntries = array of TTMSFNCGeneralPDFLibFontNameEntry;

  TTMSFNCGeneralPDFLibFontOS2Data = record
    v: UInt16;
    xavgchw: Int16;
    uswecl: UInt16;
    uswicl: UInt16;
    fst: Int16;
    ysubxsize: Int16;
    ysubysize: Int16;
    ysubxoff: Int16;
    ysubyoff: Int16;
    ysupxsize: Int16;
    ysupysize: Int16;
    ysupxoff: Int16;
    ysupyoff: Int16;
    ystos: Int16;
    ystop: Int16;
    sfc: Int8;
    sfsc: Int8;
    pns: TBytes;
    ulur1: UInt32;
    ulur2: UInt32;
    ulur3: UInt32;
    ulur4: UInt32;
    avndid : string;
    fssel: UInt16;
    usfchidx: UInt16;
    uslchidx : UInt16;
    stasc: Int16;
    stdesc: Int16;
    stlgap: Int16;
    uswasc: UInt16;
    uswdesc: UInt16;
    ulcpr1: UInt32;
    ulcpr2: UInt32;
    sxh: Int16;
    sch: Int16;
    usdc: UInt16;
    usbc: UInt16;
    usmaxct: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontCmapHead = record
    v: Int32;
    fr: Int32;
    csa: UInt32;
    mn: UInt32;
    fl: UInt16;
    uem: UInt16;
    cd: Int64;
    md: Int64;
    xmin: Int16;
    ymin: Int16;
    xmax: Int16;
    ymax: Int16;
    ms: UInt16;
    lr: UInt16;
    fd: Int16;
    ilf: Int16;
    gdf: Int16;
  end;

  TTMSFNCGeneralPDFLibFontCmap = record
    pID: UInt16;
    psID: UInt16;
    o: UInt32;
  end;

  TTMSFNCGeneralPDFLibFontCmapArray = array[byte] of TTMSFNCGeneralPDFLibFontCmap;

  TTMSFNCGeneralPDFLibFontCmapHeader = record
    v: UInt16;
    n: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontCmapFormat4 = record
    fmt: UInt16;
    l: UInt16;
    lng: UInt16;
    sgX2: UInt16;
    shrange: UInt16;
    selr: UInt16;
    rng: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontHead = record
    v: Double;
    fr: Double;
    csa: UInt32;
    mn: UInt32;
    f: UInt16;
    upe: UInt16;
    cd: Int64;
    md: Int64;
    bb: array[0..3] of Int16;
    ms: UInt16;
    lrpp: UInt16;
    fdh: Int16;
    ilf: Int16;
    gldf: Int16;
  end;

  TTMSFNCGeneralPDFLibFontHhea = record
    v: Double;
    asc: UInt16;
    desc: UInt16;
    lg: UInt16;
    awm: UInt16;
    mlsb: UInt16;
    mrsb: UInt16;
    xme: UInt16;
    csr: Int16;
    cslr: Int16;
    co: Int16;
    r1: Int16;
    r2: Int16;
    r3: Int16;
    r4: Int16;
    mdf: Int16;
    nohm: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontMaxp = record
    v: Double;
    ng: UInt16;
    mxp: UInt16;
    mxc: UInt16;
    mxcp: UInt16;
    mxcc: UInt16;
    mxz: UInt16;
    mxtp: UInt16;
    mxs: UInt16;
    mxfd: UInt16;
    mxid: UInt16;
    mxse: UInt16;
    mxsoi: UInt16;
    mxce: UInt16;
    mxcd: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontPostScript = record
    ft: Double;
    ia: Int32;
    ups: Int16;
    utck: Int16;
    isfp: UInt32;
    minmtype42: UInt32;
    maxmtype42: UInt32;
    minmtype1: UInt32;
    maxmtype1: UInt32;
    ng: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontTableDir = record
    st: UInt32;
    nt: UInt16;
    sr: UInt16;
    es: UInt16;
    rs: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontTableDirEntry = record
    t: string;
    cs: UInt32;
    o: UInt32;
    l: UInt32;
  end;

  TTMSFNCGeneralPDFLibFontTableDirEntries = array of TTMSFNCGeneralPDFLibFontTableDirEntry;

  TTMSFNCGeneralPDFLibFontTableType = (ttUnknown, ttHead, tthhea, ttmaxp, tthmtx, ttcmap, ttname, ttOS2, ttpost, ttglyf, ttloca, ttprep, ttfpgm, ttcvt);

const
  TableNames: array[TTMSFNCGeneralPDFLibFontTableType] of string = ('','head','hhea','maxp','hmtx','cmap','name','OS/2','post','glyf','loca','prep','fpgm','cvt ');
  MAC_GLYPH_NAMES: array of string = ('.notdef','.null', 'nonmarkingreturn', 'space', 'exclam', 'quotedbl',
        'numbersign', 'dollar', 'percent', 'ampersand', 'quotesingle',
        'parenleft', 'parenright', 'asterisk', 'plus', 'comma', 'hyphen',
        'period', 'slash', 'zero', 'one', 'two', 'three', 'four', 'five',
        'six', 'seven', 'eight', 'nine', 'colon', 'semicolon', 'less',
        'equal', 'greater', 'question', 'at', 'A', 'B', 'C', 'D', 'E', 'F',
        'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
        'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'bracketleft', 'backslash',
        'bracketright', 'asciicircum', 'underscore', 'grave', 'a', 'b',
        'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
        'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'braceleft',
        'bar', 'braceright', 'asciitilde', 'Adieresis', 'Aring',
        'Ccedilla', 'Eacute', 'Ntilde', 'Odieresis', 'Udieresis', 'aacute',
        'agrave', 'acircumflex', 'adieresis', 'atilde', 'aring',
        'ccedilla', 'eacute', 'egrave', 'ecircumflex', 'edieresis',
        'iacute', 'igrave', 'icircumflex', 'idieresis', 'ntilde', 'oacute',
        'ograve', 'ocircumflex', 'odieresis', 'otilde', 'uacute', 'ugrave',
        'ucircumflex', 'udieresis', 'dagger', 'degree', 'cent', 'sterling',
        'section', 'bullet', 'paragraph', 'germandbls', 'registered',
        'copyright', 'trademark', 'acute', 'dieresis', 'notequal', 'AE',
        'Oslash', 'infinity', 'plusminus', 'lessequal', 'greaterequal',
        'yen', 'mu', 'partialdiff', 'summation', 'product', 'pi',
        'integral', 'ordfeminine', 'ordmasculine', 'Omega', 'ae', 'oslash',
        'questiondown', 'exclamdown', 'logicalnot', 'radical', 'florin',
        'approxequal', 'Delta', 'guillemotleft', 'guillemotright',
        'ellipsis', 'nonbreakingspace', 'Agrave', 'Atilde', 'Otilde', 'OE',
        'oe', 'endash', 'emdash', 'quotedblleft', 'quotedblright',
        'quoteleft', 'quoteright', 'divide', 'lozenge', 'ydieresis',
        'Ydieresis', 'fraction', 'currency', 'guilsinglleft',
        'guilsinglright', 'fi', 'fl', 'daggerdbl', 'periodcentered',
        'quotesinglbase', 'quotedblbase', 'perthousand', 'Acircumflex',
        'Ecircumflex', 'Aacute', 'Edieresis', 'Egrave', 'Iacute',
        'Icircumflex', 'Idieresis', 'Igrave', 'Oacute', 'Ocircumflex',
        'apple', 'Ograve', 'Uacute', 'Ucircumflex', 'Ugrave', 'dotlessi',
        'circumflex', 'tilde', 'macron', 'breve', 'dotaccent', 'ring',
        'cedilla', 'hungarumlaut', 'ogonek', 'caron', 'Lslash', 'lslash',
        'Scaron', 'scaron', 'Zcaron', 'zcaron', 'brokenbar', 'Eth', 'eth',
        'Yacute', 'yacute', 'Thorn', 'thorn', 'minus', 'multiply',
        'onesuperior', 'twosuperior', 'threesuperior', 'onehalf',
        'onequarter', 'threequarters', 'franc', 'Gbreve', 'gbreve',
        'Idotaccent', 'Scedilla', 'scedilla', 'Cacute', 'cacute', 'Ccaron',
        'ccaron', 'dcroat');

function ReadUInt8(AStream: TStream; AOffset: Int64 = -1): UInt8;
var
  b: TBytes;
  off: Int64;
begin
  if AOffset <> -1 then
  begin
    off := AStream.Position;
    AStream.Position := AStream.Position + AOffset;
  end;

  SetLength(b, 1);
  AStream.Read(b, 1);
  Result := b[0];

  if AOffset <> -1 then
    AStream.Position := off;
end;

function GetUInt16(ABytes: TBytes; APosition: Integer): UInt16;
begin
  Result := ((ABytes[APosition * 2] shl 8) + ABytes[(APosition * 2) + 1]) shr 0;
end;

function ReadUInt16(AStream: TStream; AOffset: Int64 = -1): UInt16;
var
  b: TBytes;
  off: Int64;
begin
  if AOffset <> -1 then
  begin
    off := AStream.Position;
    AStream.Position := AStream.Position + AOffset;
  end;

  SetLength(b, 2);
  AStream.Read(b, 2);
  Result := ((b[0] shl 8) + b[1]) shr 0;

  if AOffset <> -1 then
    AStream.Position := off;
end;

function ReadUInt16Array(AStream: TStream; ALength: Integer): array of UInt16;
var
  I: Integer;
begin
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := ReadUInt16(AStream);
end;

function ReadInt32(AStream: TStream; AOffset: Int64 = -1): UInt32;
var
  b: TBytes;
  off: Int64;
begin
  if AOffset <> -1 then
  begin
    off := AStream.Position;
    AStream.Position := AStream.Position + AOffset;
  end;

  SetLength(b, 4);
  AStream.Read(b, 4);
  Result := (b[0] shl 24) + (b[1] shl 16) + (b[2] shl 8) + b[3];

  if AOffset <> -1 then
    AStream.Position := off;
end;

function ReadInt16(AStream: TStream; AApplyFix: Boolean = True; AOffset: Int64 = -1): Int16;
begin
  Result := ReadUInt16(AStream, AOffset);
  if AApplyFix and ((Result and $8000) = $8000) then
    Result := Result - (1 shl 16);
end;

function ReadFixed(AStream: TStream): Double;
var
  d, f: Int16;
begin
  d := ReadInt16(AStream, False);
  f := ReadUInt16(AStream);
  Result := d + (f / 65535);
end;

function ReadUInt32(AStream: TStream; AOffset: Int64 = -1): Int32;
begin
  Result := ReadInt32(AStream, AOffset) shr 0;
end;

function Log2I(i: Integer): Integer;
var
  ret: Integer;
begin
  ret := -1;
  if ((i and $FFFF0000) <> 0) then
  begin
    i := i shr 16;
    ret := ret + 16;
  end;

  if ((i and $FF00) <> 0) then
  begin
    i := i shr 8;
    ret := ret + 8;
  end;

  if ((i and $FF) <> 0) then
  begin
    i := i shr 4;
    ret := ret + 4;
  end;

  if ((i and $C) <> 0) then
  begin
    i := i shr 2;
    ret := ret + 2;
  end;

  if ((i and $2) <> 0) then
  begin
    i := i shr 1;
    ret := ret + 1;
  end;

  if (i <> 0) then
    ret := ret + 1;

  Result := ret;
end;

function HighestOneBit(i: Integer): Integer;
begin
  i := i or (i shr 1);
  i := i or (i shr 2);
  i := i or (i shr 4);
  i := i or (i shr 8);
  i := i or (i shr 16);
  Result := i - (i shr 1);
end;

function ReadBytes(AStream: TStream; ALength: Integer; AOffset: Int64 = -1): TBytes;
var
  b: TBytes;
  off: Int64;
begin
  if AOffset <> -1 then
  begin
    off := AStream.Position;
    AStream.Position := AStream.Position + AOffset;
  end;

  SetLength(b, ALength);
  AStream.Read(b, ALength);
  Result := b;

  if AOffset <> -1 then
    AStream.Position := off;
end;

function ReadString(AStream: TStream; ALength: Integer; AOffset: Int64 = -1): string;
var
  s: string;
  b: TBytes;
  bt: Byte;
  I: Integer;
begin
  b := ReadBytes(AStream, ALength, AOffset);
  for I := 0 to Length(b) - 1 do
  begin
    bt := b[I];
    asm
      s += String.fromCharCode(bt);
    end;
  end;

  Result := s;
end;

function BuildUint32(high, low: Int32): UInt32; overload;
begin
  Result := ((high and $FFFF) shl 16) or (low and $FFFF);
end;

function BuildUInt32(bytes: TBytes): UInt32; overload;
begin
  Result := ((bytes[0] and $FF) shl 24) or
            ((bytes[1] and $FF) shl 16) or
            ((bytes[2] and $FF) shl 8) or
            (bytes[3] and $FF);
end;

procedure WriteBytes(AStream: TStream; ABytes: TBytes; AOffset, ALength: Integer);
begin
  AStream.Write(ABytes, AOffset, ALength);
end;

procedure WriteInt8(AStream: TStream; AValue: Int8);
var
  b: TBytes;
begin
  SetLength(b, 1);
  b[0] := AValue;
  AStream.Write(b, 1);
end;

procedure WriteInt16(AStream: TStream; AValue: Int32);
begin
  WriteInt8(AStream, (AValue shr 8) and $FF);
  WriteInt8(AStream, AValue and $FF);
end;

procedure WriteInt32(AStream: TStream; AValue: Int32);
begin
  WriteInt8(AStream, (AValue shr 24) and $FF);
  WriteInt8(AStream, (AValue shr 16) and $FF);
  WriteInt8(AStream, (AValue shr 8) and $FF);
  WriteInt8(AStream, AValue and $FF);
end;

procedure WriteInt64(AStream: TStream; AValue: Int64);
begin
  WriteInt8(AStream, (AValue shr 56) and $FF);
  WriteInt8(AStream, (AValue shr 48) and $FF);
  WriteInt8(AStream, (AValue shr 40) and $FF);
  WriteInt8(AStream, (AValue shr 32) and $FF);
  WriteInt8(AStream, (AValue shr 24) and $FF);
  WriteInt8(AStream, (AValue shr 16) and $FF);
  WriteInt8(AStream, (AValue shr 8) and $FF);
  WriteInt8(AStream, AValue and $FF);
end;

procedure WriteUInt16(AStream: TStream; AValue: UInt32);
begin
  WriteInt16(AStream, AValue);
end;

procedure WriteUInt32(AStream: TStream; AValue: UInt32);
begin
  WriteInt32(AStream, AValue);
end;

{$WARNINGS OFF}
procedure WriteUInt64(AStream: TStream; AValue: UInt64);
begin
  WriteInt64(AStream, AValue);
end;
{$WARNINGS ON}

procedure WriteFixed(AStream: TStream; AValue: Double);
var
  ip, fp: Int8;
begin
  ip := Floor(AValue);
  fp := Floor((AValue - ip) * 65536);
  WriteInt16(AStream, ip);
  WriteInt16(AStream, fp);
end;

function StringToBytes(AValue: string): TBytes;
var
  str: string;
begin
  str := AValue;
  asm
    var ch, st, re = [];
    for (var i = 0; i < str.length; i++ ) {
      ch = str.charCodeAt(i);
      st = [];
      do {
        st.push( ch & 0xFF );
        ch = ch >> 8;
    }
    while (ch);
      re = re.concat(st.reverse());
    }

    return re;
  end;
end;

function CalculateTableCheckSum(AStream: TStream; AStart, ALength: Integer): UInt32;
var
  n: Integer;
  o: Int64;
begin
  o := AStream.Position;
  AStream.Position := AStart;
  n := (ALength + 3) div 4;
  while n > 0 do
  begin
    Result := (Result + (ReadUint32(AStream) and $ffffffff)) shr 0;
    Dec(n);
  end;
  AStream.Position := o;
end;

function GetTableType(const AName : String): TTMSFNCGeneralPDFLibFontTableType;
begin
  Result := High(TTMSFNCGeneralPDFLibFontTableType);
  while (Result <> ttUnknown) and (CompareText(AName, TableNames[Result]) <> 0) do
    Result := Pred(Result);
end;

function GetTableForName(ATableDir: TTMSFNCGeneralPDFLibFontTableDir; ATables: TTMSFNCGeneralPDFLibFontTableDirEntries; ATableType: TTMSFNCGeneralPDFLibFontTableType): TTMSFNCGeneralPDFLibFontTableDirEntry;
var
  I: Integer;
begin
  for I := 0 to ATableDir.nt - 1 do
  begin
    if GetTableType(ATables[I].t) = ATableType then
    begin
      Result := ATables[I];
      Break;
    end;
  end;
end;

{ TTMSFNCGeneralPDFLibFontTTFChar }

constructor TTMSFNCGeneralPDFLibFontTTFChar.Create(const ACharCode: Integer;
  const ANewIndex: Integer; const AOrgIndex: Integer; const ADescription: PChar
  );
begin
  FCharCode := ACharCode;
  FNewIndex := ANewIndex;
  FOrgIndex := AOrgIndex;
  FDescription := ADescription;
end;

{$ENDIF}

{ TTMSFNCGeneralPDFLibFontInitializer }

procedure TTMSFNCGeneralPDFLibFontInitializer.CompressTTFData;
begin
  {$IFDEF WEBLIB}
  if FullFont then
    FTTFDataStream := CompressFontStream(FTTFFileStream)
  else
    FTTFDataStream := CompressFontStream(FTTFEmbed);
  {$ENDIF}
end;

constructor TTMSFNCGeneralPDFLibFontInitializer.Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const AStyle: TFontStyles; const ASize: Single);
{$IFDEF WEBLIB}
var
  dt: TTMSFNCUtilsFile;
  fontit: TTMSFNCGeneralPDFLibFontItem;

  function SearchForFont(AFontName: string; AStyle: TFontStyles): TTMSFNCGeneralPDFLibFontItem;
  var
    fti: TTMSFNCGeneralPDFLibFontItem;
    I: Integer;
  begin
    Result := nil;
    if Assigned(FMainInitializer) and Assigned(FMainInitializer.FFontCol) then
    begin
      for I := 0 to FMainInitializer.FFontCol.Count - 1 do
      begin
        fti := FMainInitializer.FFontCol[I];
        if (LowerCase(fti.Name) = LowerCase(AFontName)) and (fti.Style = AStyle) then
        begin
          Result := fti;
          Break;
        end;
      end;
    end;
  end;

{$ENDIF}
begin
  FMainInitializer := AMainInitializer;
  FBase := ABase;
  FSize := ASize;
  FUsedCharArray := TTMSFNCPDFGraphicsLibUsedFontCharArray.Create;
  FGlyphIDs := TTMSFNCPDFGraphicsLibUsedFontCharArray.Create;
  {$IFDEF WEBLIB}
  FTTFCreateFontPackage := True;
  FTTFFileStream := TMemoryStream.Create;
  FTTFEmbed := TMemoryStream.Create;

  fontit := nil;
  if (TFontStyle.fsBold in AStyle) and (TFontStyle.fsItalic in AStyle) then
    fontit := SearchForFont(FBase, [TFontStyle.fsBold, TFontStyle.fsItalic])
  else if TFontStyle.fsBold in AStyle then
    fontit := SearchForFont(FBase, [TFontStyle.fsBold])
  else if TFontStyle.fsItalic in AStyle then
    fontit := SearchForFont(FBase, [TFontStyle.fsItalic]);

  if not Assigned(fontit) then
    fontit := SearchForFont(FBase, []);

  if Assigned(fontit) then
  begin
    dt := fontit.Data;
    FTTFMetrics.CapHeight := fontit.CapHeight;
    FTTFMetrics.Ascent := fontit.Ascent;
    FTTFMetrics.Descent := fontit.Descent;
    FTTFMetrics.FontBox := fontit.BBox;
    FTTFMetrics.ItalicAngle := Round(fontit.ItalicAngle);
    FTTFMetrics.Fixed := fontit.Fixed;
    FTTFMetrics.TrueType := True;
  end
  else
  begin
    dt.Data := nil;
    dt.Name := '';
    FTTFMetrics.CapHeight := 0;
    FTTFMetrics.Ascent := 0;
    FTTFMetrics.Descent := 0;
    FTTFMetrics.FontBox := Bounds(0, 0, 0, 0);
    FTTFMetrics.ItalicAngle := 0;
    FTTFMetrics.Fixed := False;
    FTTFMetrics.TrueType := False;
  end;

  if Assigned(dt.Data) then
  begin
    asm
      $Self.FTTFFileStream.Write(dt.Data, dt.Data.length);
    end;
  end;

  FTTFFileStream.Position := 0;
  {$ENDIF}
end;

destructor TTMSFNCGeneralPDFLibFontInitializer.Destroy;
begin
  {$IFDEF WEBLIB}
  if Assigned(FTTFEmbed) then
  begin
    FTTFEmbed.Free;
    FTTFEmbed := nil;
  end;
  if Assigned(FTTFFileStream) then
  begin
    FTTFFileStream.Free;
    FTTFFileStream := nil;
  end;
  {$ENDIF}
  if Assigned(FUsedCharArray) then
  begin
    FUsedCharArray.Free;
    FUsedCharArray := nil;
  end;
  if Assigned(FGlyphIDs) then
  begin
    FGlyphIDs.Free;
    FGlyphIDs := nil;
  end;
  inherited;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
begin
  Result := FCharArray;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
var
  ca: TTMSFNCPDFGraphicsLibFontCharWidths;
  I: Integer;
begin
  SetLength(ca, Length(FCharWidths));
  for I := 0 to Length(ca) - 1 do
    ca[I] := FCharWidths[I];

  Result := ca;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetFontMetrics: TTMSFNCGeneralPDFLibFontMetrics;
begin
  Result.Ascent := 0;
  Result.CapHeight := 0;
  Result.Descent := 0;
  Result.FontBox := Bounds(0, 0, 0, 0);
  Result.ItalicAngle := 0;
  Result.Fixed := False;
  Result.TrueType := True;
  {$IFDEF WEBLIB}
  if Assigned(FMainInitializer) then
  begin
    Result.CapHeight := FTTFMetrics.CapHeight;
    Result.Ascent := FTTFMetrics.Ascent;
    Result.Descent := FTTFMetrics.Descent;
    Result.FontBox := FTTFMetrics.FontBox;
    Result.ItalicAngle := FTTFMetrics.ItalicAngle;
    Result.Fixed := FTTFMetrics.Fixed;
    Result.TrueType := FTTFMetrics.TrueType;
  end;
  {$ENDIF}
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeCharWidths;
begin
  {$IFDEF WEBLIB}
  if Assigned(FMainInitializer) then
    InternalInitializeCharWidths(FTTFFileStream);
  {$ENDIF}
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeFontFile;
begin
  InternalInitializeFontFile;
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InternalInitializeFontFile;
{$IFDEF WEBLIB}
var
  tblName: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblOS2: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblPost: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblHead: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblHhea: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblMaxp: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblLoca: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblGlyf: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblHmtx: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblCmap: TTMSFNCGeneralPDFLibFontTableDirEntry;
  osdt: TTMSFNCGeneralPDFLibFontOS2Data;
  ps: TTMSFNCGeneralPDFLibFontPostScript;
  h: TTMSFNCGeneralPDFLibFontHead;
  hh: TTMSFNCGeneralPDFLibFontHhea;
  mxp: TTMSFNCGeneralPDFLibFontMaxp;
  I, idx, nidx, maxidx, nuc, v: Integer;
  tbldr: TTMSFNCGeneralPDFLibFontTableDir;
  tbls: TTMSFNCGeneralPDFLibFontTableDirEntries;
  st, nst, fs: TMemoryStream;
  newtbls: array of TBytes;
  newOffsets: array of Int32;
  checksum, offset: UInt32;
  offsets: array of Int32;
  newOffsetsTables: array of Int32;
  glyphNames: array of string;
  glyphNameIdx: array of Integer;
  nameArray: array of string;
const
  newTableNames: array of string = ('OS/2','cmap','glyf','head','hhea','hmtx','loca','maxp','name','post');

  function CompareVal(const Item1, Item2: Integer): Integer;
  begin
    Result := CompareValue(Item1, Item2);
  end;

  function AddCompoundReferences: Boolean;
  var
    glyphIdsToAdd: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    lastOff, off, glyphId, ogid, flags, ofs, len: Integer;
    buf: TBytes;
  begin
    st.Position := tblGlyf.o;
    for I := 0 to FGlyphIDs.Count - 1 do
    begin
      glyphId := FGlyphIDs[I];
      ofs := offsets[glyphId];
      len := offsets[glyphId + 1] - ofs;
      if not IsNan(len) then
      begin     
        st.Position := st.Position + (ofs - lastOff);
        buf := ReadBytes(st, len);
        if (Length(buf) >= 2) and (buf[0] = 255) and (buf[1] = 255) then
        begin
          off := 2*5;
          repeat
            flags := ((buf[off] and $FF) shl 8) or (buf[off+1] and $FF);
            off := off + 2;
            ogid := ((buf[off] and $FF) shl 8) or (buf[off+1] and $FF);
            if FGlyphIDs.IndexOf(ogid) = -1 then
            begin
              if not Assigned(glyphIdsToAdd) then
                glyphIdsToAdd := TTMSFNCPDFGraphicsLibUsedFontCharArray.Create;

              glyphIdsToAdd.Add(ogid);
            end;

            off := off + 2;
            if (flags and (1 shl 0)) <> 0 then
              off := off + (2 * 2)
            else
              off := off + 2;

            if (flags and (1 shl 7)) <> 0 then
              off := off + 2 * 4
            else if (flags and (1 shl 6)) <> 0 then
              off := off + 2 * 2
            else if (flags and (1 shl 3)) <> 0 then
              off := off + 2;
          until (flags and (1 shl 5)) = 0;
        end;
      end;

      lastoff := offsets[glyphId + 1];
    end;

    if Assigned(glyphIdsToAdd) then
    begin
      for I := 0 to glyphIdsToAdd.Count - 1 do
        FGlyphIDs.Add(glyphIdsToAdd[I]);

      FGlyphIDs.Sort(TListSortCompare(@CompareVal));

      Result := False;
      glyphIdsToAdd.Free;
    end
    else
      Result := True;
  end;

  function BuildHeadTable: TBytes;
  begin
    nst.Clear;
    WriteFixed(nst, h.v);
    WriteFixed(nst, h.fr);
    WriteUInt32(nst, 0);
    WriteUInt32(nst, h.mn);
    WriteUInt16(nst, h.f);
    WriteUInt16(nst, h.upe);
    WriteUInt64(nst, h.cd);
    WriteUInt64(nst, h.md);
    WriteInt16(nst, h.bb[0]);
    WriteInt16(nst, h.bb[1]);
    WriteInt16(nst, h.bb[2]);
    WriteInt16(nst, h.bb[3]);
    WriteUInt16(nst, h.ms);
    WriteUInt16(nst, h.lrpp);
    WriteInt16(nst, h.fdh);
    WriteInt16(nst, 1);
    WriteInt16(nst, h.gldf);
    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildHheaTable: TBytes;
  begin
    nst.Clear;
    WriteFixed(nst, hh.v);
    WriteUInt16(nst, hh.asc);
    WriteUInt16(nst, hh.desc);
    WriteUInt16(nst, hh.lg);
    WriteUInt16(nst, hh.awm);
    WriteUInt16(nst, hh.mlsb);
    WriteUInt16(nst, hh.mrsb);
    WriteUInt16(nst, hh.xme);
    WriteInt16(nst, hh.csr);
    WriteInt16(nst, hh.cslr);
    WriteInt16(nst, hh.co);
    WriteInt16(nst, hh.r1);
    WriteInt16(nst, hh.r2);
    WriteInt16(nst, hh.r3);
    WriteInt16(nst, hh.r4);
    WriteInt16(nst, hh.mdf);
    WriteUInt16(nst, FGlyphIDs.SubSetCount(0, hh.nohm));
    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildMaxpTable: TBytes;
  begin
    nst.Clear;
    WriteFixed(nst, 1.0);
    WriteUInt16(nst, FGlyphIDs.Count);
    WriteUInt16(nst, mxp.mxp);
    WriteUInt16(nst, mxp.mxc);
    WriteUInt16(nst, mxp.mxcp);
    WriteUInt16(nst, mxp.mxcc);
    WriteUInt16(nst, mxp.mxz);
    WriteUInt16(nst, mxp.mxtp);
    WriteUInt16(nst, mxp.mxs);
    WriteUInt16(nst, mxp.mxfd);
    WriteUInt16(nst, 0);
    WriteUInt16(nst, mxp.mxse);
    WriteUInt16(nst, 0);
    WriteUInt16(nst, mxp.mxce);
    WriteUInt16(nst, mxp.mxcd);
    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildNameTable: TBytes;
  begin
    st.Position := tblName.o;
    Result := ReadBytes(st, tblName.l);
  end;

  function BuildOS2Table: TBytes;
  var
    tagBytes: TBytes;
  begin
    nst.Clear;
    WriteUInt16(nst,0);
    WriteInt16(nst, osdt.xavgchw);
    WriteUInt16(nst, osdt.uswecl);
    WriteUInt16(nst, osdt.uswicl);
    WriteInt16(nst, osdt.fst);
    WriteInt16(nst, osdt.ysubxsize);
    WriteInt16(nst, osdt.ysubysize);
    WriteInt16(nst, osdt.ysubxoff);
    WriteInt16(nst, osdt.ysubyoff);
    WriteInt16(nst, osdt.ysupxsize);
    WriteInt16(nst, osdt.ysupysize);
    WriteInt16(nst, osdt.ysupxoff);
    WriteInt16(nst, osdt.ysupyoff);
    WriteInt16(nst, osdt.ystos);
    WriteInt16(nst, osdt.ystop);
    WriteInt8(nst, osdt.sfc);
    WriteInt8(nst, osdt.sfsc);    
    WriteBytes(nst, osdt.pns, 0, Length(osdt.pns));
    WriteUInt32(nst, 0);
    WriteUInt32(nst, 0);
    WriteUInt32(nst, 0);
    WriteUInt32(nst, 0);
    tagBytes := StringToBytes(osdt.avndid);
    WriteBytes(nst, tagBytes, 0, Length(tagBytes));
    WriteUInt16(nst, osdt.fssel);
    if FUsedCharArray.Count > 0 then
    begin
      WriteUInt16(nst, FUsedCharArray[0]);
      WriteUInt16(nst, FUsedCharArray[FUsedCharArray.Count - 1]);
    end
    else
    begin
      WriteUInt16(nst, 0);
      WriteUInt16(nst, 0);
    end;
    WriteUInt16(nst, osdt.stasc);
    WriteUInt16(nst, osdt.stdesc);
    WriteUInt16(nst, osdt.stlgap);
    WriteUInt16(nst, osdt.uswasc);
    WriteUInt16(nst, osdt.uswdesc);
    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildGlyfTable: TBytes;
  var
    glyphId, lastOff, newOff, iOff,
    ofs, len, off, flags, ogid, ngid, np, numberOfContours, instructionLength: Integer;
    buf: TBytes;
  const
    PAD_BUF: TBytes = [0, 0, 0];
  begin
    nst.Clear;
    st.Position := tblGlyf.o;

    for I := 0 to FGlyphIDs.Count - 1 do
    begin
      glyphId := FGlyphIDs[I];
      ofs := offsets[glyphId];
      len := offsets[glyphId + 1] - ofs;
      newOffsets[iOff] := newOff;
      iOff := iOff + 1;
      st.Position := st.Position + (ofs - lastOff);
      buf := ReadBytes(st, len);
      if (Length(buf) >= 2) and (buf[0] = 255) and (buf[1] = 255) then
      begin
        off := 2*5;
        flags := 0;
        repeat
          buf[off] := buf[off] and $FE;
          flags := ((buf[off] and $FF) shl 8) or (buf[off+1] and $FF);
          off := off + 2;
          ogid := ((buf[off] and $FF) shl 8) or (buf[off+1] and $FF);
          if FGlyphIDs.IndexOf(ogid) = -1 then
          begin
            FGlyphIDs.Add(ogid);
            FGlyphIDs.Sort(TListSortCompare(@CompareVal));
          end;

          ngid := FGlyphIDs.HeadSetCount(ogid);
          buf[off] := ngid shr 8;
          buf[off+1] := ngid;
          off := off + 2;

          if (flags and (1 shl 0)) <> 0 then
            off := off + (2 * 2)
          else
            off := off + 2;

          if (flags and (1 shl 7)) <> 0 then
            off := off + 2 * 4
          else if (flags and (1 shl 6)) <> 0 then
            off := off + 2 * 2
          else if (flags and (1 shl 3)) <> 0 then
            off := off + 2;

        until (flags and (1 shl 5) = 0);

        WriteBytes(nst, buf, 0, off);
        newOff := newOff + off;
      end
      else if Length(buf) > 0 then
      begin
        numberOfContours := ((buf[0] and $FF) shl 8) or (buf[1] and $FF);
        off := 2*5 + numberOfContours * 2;
        WriteBytes(nst, buf, 0, off);
        newOff := newOff + off;
        instructionLength := ((buf[off] and $FF) shl 8) or (buf[off + 1] and $FF);
        WriteInt8(nst, 0);
        WriteInt8(nst, 0);
        newOff := newOff + 2;
        off := off + 2 + instructionLength;
        WriteBytes(nst, buf, off, Length(buf) - off);
        newOff := newOff + Length(buf) - off;
      end;

      if (newOff mod 4) <> 0 then
      begin
        np := 4 - (newOff mod 4);
        WriteBytes(nst, PAD_BUF, 0, np);
        newOff := newOff + np;
      end;

      lastOff := offsets[glyphId + 1];
    end;

    newOffsets[iOff] := newOff;
    iOff := iOff + 1;

    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildLocaTable: TBytes;
  begin
    nst.Clear;
    for I := 0 to Length(newOffsets) - 1 do
      WriteUInt32(nst, newOffsets[I]);

    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildCmapTable: TBytes;
  var
    startCode: array of Integer;
    endCode: array of Integer;
    idDelta: array of Integer;
    nSeg, nSegHigh: Integer;
    curChar, lastChar, prevChar, curGid, lastGid, searchRange: Integer;
  begin
    nst.Clear;
    WriteUInt16(nst, 0);
    WriteUInt16(nst, 1);
    WriteUInt16(nst, 3);
    WriteUInt16(nst, 1);
    WriteUInt32(nst, 4 * 2 + 4);

    SetLength(startCode, FUsedCharArray.Count + 1);
    SetLength(endCode, FUsedCharArray.Count + 1);
    SetLength(idDelta, FUsedCharArray.Count + 1);

    if FUsedCharArray.Count > 0 then
    begin
      lastChar := FUsedCharArray[0];
      prevChar := lastChar;
      lastGid := FGlyphIDs.HeadSetCount(FGlyphIDs[1]);

      for I := 1 to FUsedCharArray.Count - 1 do
      begin
        curChar := FUsedCharArray[I];
        curGid := FGlyphIDs.HeadSetCount(FGlyphIDs[I + 1]);
        if (curChar <> prevChar + 1) or (curGid - lastGid <> curChar - lastChar) then
        begin
          if lastGid <> 0 then
          begin
            startCode[nseg] := lastChar;
            endCode[nseg] := prevchar;
            idDelta[nseg] := lastgid - lastChar;
            nseg := nseg + 1;
          end
          else if (lastChar <> prevChar) then
          begin
            startCode[nseg] := lastChar + 1;
            endCode[nseg] := prevChar;
            idDelta[nseg] := lastGid - lastChar;
            nseg := nseg + 1;
          end;
          lastGid := curGid;
          lastChar := curChar;
        end;
        prevChar := curChar;
      end;

      startCode[nseg] := lastChar;
      endCode[nseg] := prevChar;
      idDelta[nseg] := lastGid - lastChar;
      nseg := nseg + 1;
    end;

    startCode[nseg] := $FFFF;
    endCode[nseg] := $FFFF;
    idDelta[nseg] := 1;
    nseg := nseg + 1;

    searchRange := Floor(2 * Power(2, log2(nseg)));
    writeUint16(nst, 4);
    writeUint16(nst, 8 * 2 + nseg * 4*2);
    writeUint16(nst, 0);
    writeUint16(nst, nseg * 2);
    writeUint16(nst, searchRange);
    writeUint16(nst, Floor(log2(searchRange div 2)));
    writeUint16(nst, 2 * nseg - searchRange);

    for I := 0 to nseg - 1 do
      WriteUInt16(nst, endCode[I]);

    WriteUInt16(nst, 0);

    for I := 0 to nseg - 1 do
      WriteUInt16(nst, startCode[I]);

    for I := 0 to nseg - 1 do
      WriteUInt16(nst, idDelta[I]);

    for I := 0 to nseg - 1 do
      WriteUInt16(nst, 0);

    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildHmtxTable: TBytes;
  var
    lastOff, off, nSkip, n, glyphId: Integer;
    buf: TBytes;
  begin
    nst.Clear;
    st.Position := tblHmtx.o;

    for I := 0 to FGlyphIDs.Count - 1 do
    begin
      glyphId := FGlyphIDs[I];
      if glyphId < hh.nohm then
        off := glyphId * 4
      else
        off := hh.nohm + 4 * (glyphId - hh.nohm) * 2;

      if off <> lastOff then
      begin
        nSkip := off - lastOff;
        st.Position := st.Position + nSkip;
      end;

      if glyphId < hh.nohm then
        n := 4
      else
        n := 2;

      buf := ReadBytes(st, n);
      WriteBytes(nst, buf, 0, n);
      lastOff := off + n;
    end;

    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function BuildPostTable: TBytes;
  var
    uc: Integer;
    n: string;
    J, macId, o: Integer;
    names: array of string;
    buf: TBytes;
  begin
    nst.Clear;
    WriteFixed(nst, 2.0);
    WriteFixed(nst, ps.ia);
    WriteInt16(nst, ps.ups);
    WriteInt16(nst, ps.utck);
    WriteUInt32(nst, ps.isfp);
    WriteUInt32(nst, ps.minmtype42);
    WriteUInt32(nst, ps.minmtype42);
    WriteUInt32(nst, ps.minmtype42);
    WriteUInt32(nst, ps.minmtype42);
    WriteUInt16(nst, FGlyphIDs.Count);

    for I := 0 to FGlyphIDs.Count - 1 do
    begin
      uc := FGlyphIDs[I];
      n := '';
      if (uc >= 0) and (uc <= Length(glyphNames) - 1) then
        n := glyphNames[uc];

      macId := -1;
      for J := 0 to Length(MAC_GLYPH_NAMES) - 1 do
      begin
        if MAC_GLYPH_NAMES[J] = n then
        begin
          macId := J;
          Break;
        end;
      end;

      if macId <> -1 then
      begin
        WriteUInt16(nst, macId);
      end
      else
      begin
        o := -1;
        for J := 0 to Length(names) - 1 do
        begin
          if names[J] = n then
          begin
            o := J;
            Break;
          end;
        end;

        if o = -1 then
        begin
          o := Length(names);
          SetLength(names, o + 1);
          names[o] := n;
        end;

        WriteUInt16(nst, 258 + o);
      end;
    end;

    for I := 0 to Length(names) - 1 do
    begin
      buf := StringToBytes(names[I]);
      WriteInt8(nst, Length(buf));
      WriteBytes(nst, buf, 0, Length(buf));
    end;

    SetLength(Result, Length(nst.Bytes));
    for I := 0 to Length(nst.Bytes) - 1 do
      Result[I] := nst.Bytes[I];
  end;

  function WriteFileHeader(ATableLength: Integer): UInt32;
  var
    last, nTables, mask, entrySelector, searchRange: Integer;
  begin
    nTables := ATableLength;
    WriteInt32(fs, $00010000);
    WriteInt16(fs, nTables);

    mask := highestOneBit(nTables);
    searchRange := mask * 16;
    WriteInt16(fs, searchRange);
    entrySelector := Log2I(mask);
    WriteInt16(fs, entrySelector);
    last := 16 * nTables - searchRange;
    WriteInt16(fs, last);
    Result := $00010000 + BuildUint32(nTables, searchRange) + BuildUint32(entrySelector, last);
  end;

  function WriteTableHeader(tag: string; AOffset: Int32; bytes: TBytes): UInt32;
  var
    n, nup: Int32;
    checksum: Int32;
    tagBytes: TBytes;
  begin
    n := Length(bytes);
    checksum := 0;
    for nup := 0 to n - 1 do
      checksum := checksum + (bytes[nup] and $FF) shl (24 - (nup mod 4) * 8);

    checksum := checksum and $FFFFFFFF;

    tagBytes := StringToBytes(tag);
    WriteBytes(fs, tagBytes, 0, 4);
    WriteInt32(fs, checksum);
    WriteInt32(fs, AOffset);
    WriteInt32(fs, Length(bytes));

    Result := BuildUInt32(tagBytes) + checksum + checksum + AOffset + Length(bytes);
  end;

  procedure WriteTableBody(bytes: TBytes);
  var
    n: Integer;
  const
    PAD_BUF: TBytes = [0, 0, 0];
  begin
    n := Length(bytes);
    WriteBytes(fs, bytes, 0, n);
    if n mod 4 <> 0 then
      WriteBytes(fs, PAD_BUF, 0, 4 - (n mod 4));
  end;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  st := FTTFFileStream;
  if not Assigned(st) then
    Exit;

  st.Position := 0;
  if st.Size = 0 then
    Exit;

  tbldr.st := ReadUInt32(st) div 65536;
  tbldr.nt := ReadUInt16(st);
  tbldr.sr := ReadUInt16(st);
  tbldr.es := ReadUInt16(st);
  tbldr.rs := ReadUInt16(st);

  SetLength(tbls, tbldr.nt);
  for I := 0 to tbldr.nt - 1 do
  begin
    tbls[I].t := ReadString(st, 4);
    tbls[I].cs := ReadUInt32(st);
    tbls[I].o := ReadUInt32(st);
    tbls[I].l := ReadUInt32(st);
  end;

  tblname := GetTableForName(tbldr, tbls, ttname);
  tblos2 := GetTableForName(tbldr, tbls, ttOS2);
  if tblos2.o <= st.Size then
  begin
    st.Position := tblos2.o;
    osdt.v := ReadUInt16(st);
    osdt.xavgchw := ReadInt16(st, False);
    osdt.uswecl := ReadUInt16(st);
    osdt.uswicl := ReadUInt16(st);
    osdt.fst := ReadInt16(st, False);
    osdt.ysubxsize := ReadInt16(st, False);
    osdt.ysubysize := ReadInt16(st, False);
    osdt.ysubxoff := ReadInt16(st, False);
    osdt.ysubyoff := ReadInt16(st, False);
    osdt.ysupxsize := ReadInt16(st, False);
    osdt.ysupysize := ReadInt16(st, False);
    osdt.ysupxoff := ReadInt16(st, False);
    osdt.ysupyoff := ReadInt16(st, False);
    osdt.ystos := ReadInt16(st, False);
    osdt.ystop := ReadInt16(st, False);
    osdt.sfc := ReadInt16(st, False);
    osdt.pns := ReadBytes(st, 10);
    osdt.ulur1 := ReadUInt32(st);
    osdt.ulur2 := ReadUInt32(st);
    osdt.ulur3 := ReadUInt32(st);
    osdt.ulur4 := ReadUInt32(st);
    osdt.avndid := ReadString(st, 4);
    osdt.fssel := ReadUInt16(st);
    osdt.usfchidx := ReadUInt16(st);
    osdt.uslchidx := ReadUInt16(st);
    osdt.stasc := ReadInt16(st, False);
    osdt.stdesc := ReadInt16(st, False);
    osdt.stlgap := ReadInt16(st, False);
    osdt.uswasc := ReadUInt16(st);
    osdt.uswdesc := ReadUInt16(st);
  end;

  tblPost := GetTableForName(tbldr, tbls, ttpost);
  if tblPost.o < st.Size then
  begin
    st.Position := tblPost.o;
    ps.ft := ReadFixed(st);
    ps.ia := ReadInt32(st);
    ps.ups := ReadInt16(st, False);
    ps.utck := ReadInt16(st, False);
    ps.isfp := ReadUInt32(st);
    ps.minmtype42 := ReadUInt32(st);
    ps.maxmtype42 := ReadUInt32(st);
    ps.minmtype1 := ReadUInt32(st);
    ps.maxmtype1 := ReadUInt32(st);
    ps.ng := ReadUInt16(st);

    if ps.ft = 1 then
    begin
      SetLength(glyphNames, Length(MAC_GLYPH_NAMES));
      for I := 0 to Length(MAC_GLYPH_NAMES) - 1 do
        glyphNames[I] := MAC_GLYPH_NAMES[I];
    end
    else if ps.ft = 2 then
    begin
      SetLength(glyphNames, ps.ng - 1);
      maxidx := -2147483648;
      for I := 0 to ps.ng - 1 do
      begin
        nidx := ReadUInt16(st);
        glyphNameIdx[I] := nidx;
        if nidx <= 32767 then
          maxidx := Max(maxidx, nidx);
      end;

      if maxidx >= Length(MAC_GLYPH_NAMES) then
      begin
        SetLength(nameArray, maxidx - Length(MAC_GLYPH_NAMES) + 1);
        for I := 0 to Length(nameArray) - 1 do
        begin
          nuc := ReadUInt8(st);
          nameArray[i] := ReadString(st, nuc);
        end;
      end;

      for I := 0 to ps.ng - 1 do
      begin
        nidx := glyphNameIdx[I];
        if nidx < Length(MAC_GLYPH_NAMES) then
          glyphNames[i] := MAC_GLYPH_NAMES[nidx]
        else if (nidx >= Length(MAC_GLYPH_NAMES)) and (nidx <= 32767) then
          glyphNames[I] := nameArray[nidx - Length(MAC_GLYPH_NAMES)]
        else
          glyphNames[I] := '.undefined';
      end;
    end;
  end;

  tblHmtx := GetTableForName(tbldr, tbls, tthmtx);
  tblCmap := GetTableForName(tbldr, tbls, ttcmap);

  tblHead := GetTableForName(tbldr, tbls, ttHead);
  if tblHead.o < st.Size then
  begin
    st.Position := tblHead.o;
    h.v := ReadFixed(st);
    h.fr := ReadFixed(st);
    h.csa := ReadUInt32(st);
    h.mn := ReadUInt32(st);
    h.f := ReadUInt16(st);
    h.upe := ReadUInt16(st);
    h.cd := ReadInt32(st) + ReadInt32(st);
    h.md := ReadInt32(st) + ReadInt32(st);
    for i := 0 to 3 do
      h.bb[i] := ReadInt16(st, False);
    h.ms := ReadUInt16(st);
    h.lrpp := ReadUInt16(st);
    h.fdh := ReadInt16(st, False);
    h.ilf := ReadInt16(st, False);
    h.gldf := ReadInt16(st, False);
  end;

  tblHhea := GetTableForName(tbldr, tbls, ttHhea);
  if tblHhea.o < st.Size then
  begin
    st.Position := tblHhea.o;
    hh.v := ReadFixed(st);
    hh.asc := ReadUInt16(st);
    hh.desc := ReadUInt16(st);
    hh.lg := ReadUInt16(st);
    hh.awm := ReadUInt16(st);
    hh.mlsb := ReadUInt16(st);
    hh.mrsb := ReadUInt16(st);
    hh.xme := ReadUInt16(st);
    hh.csr := ReadInt16(st, False);
    hh.cslr := ReadInt16(st, False);
    hh.co := ReadInt16(st, False);
    hh.r1 := ReadInt16(st, False);
    hh.r2 := ReadInt16(st, False);
    hh.r3 := ReadInt16(st, False);
    hh.r4 := ReadInt16(st, False);
    hh.mdf := ReadInt16(st, False);
    hh.nohm := ReadUInt16(st);
  end;

  tblMaxp := GetTableForName(tbldr, tbls, ttMaxp);
  if tblMaxp.o < st.Size then
  begin
    st.Position := tblMaxp.o;
    mxp.v := ReadFixed(st);
    mxp.ng := ReadUInt16(st);
    mxp.mxp := ReadUInt16(st);
    mxp.mxc := ReadUInt16(st);
    mxp.mxcp := ReadUInt16(st);
    mxp.mxcc := ReadUInt16(st);
    mxp.mxz := ReadUInt16(st);
    mxp.mxtp := ReadUInt16(st);
    mxp.mxs := ReadUInt16(st);
    mxp.mxfd := ReadUInt16(st);
    mxp.mxid := ReadUInt16(st);
    mxp.mxse := ReadUInt16(st);
    mxp.mxsoi := ReadUInt16(st);
    mxp.mxce := ReadUInt16(st);
    mxp.mxcd := ReadUInt16(st);
  end;

  tblLoca := GetTableForName(tbldr, tbls, ttloca);
  if tblLoca.o < st.Size then
  begin
    st.Position := tblLoca.o;
    SetLength(offsets, mxp.ng + 1);
    for I := 0 to mxp.ng + 1 do
    begin
      if h.ilf = 0 then
        offsets[I] := ReadUInt16(st) * 2
      else if h.ilf = 1 then
        offsets[I] := ReadInt32(st);
    end;
  end;

  tblGlyf := GetTableForName(tbldr, tbls, ttglyf);

  nst := TMemoryStream.Create;
  try
    FGlyphIDs.Clear;
    FGlyphIDs.Add(0);
    for I := 0 to FUsedCharArray.Count - 1 do
    begin
      v := FUsedCharArray[I];
      idx := FCharArray.IndexOf(v);
      if idx > -1 then
      begin
        v := FCharWidths[idx].g;
        FGlyphIDs.Add(v);
      end
      else
        FGlyphIDs.Add(0);
    end;

    AddCompoundReferences;

    FGlyphIDs.Sort(TListSortCompare(@CompareVal));

    SetLength(newtbls, Length(newTableNames));
    SetLength(newOffsets, FGlyphIDs.Count + 1);

    newtbls[3] := BuildHeadTable;
    newtbls[4] := BuildHheaTable;
    newtbls[7] := BuildMaxpTable;
    newtbls[8] := BuildNameTable;
    newtbls[0] := BuildOS2Table;
    newtbls[2] := BuildGlyfTable;
    newtbls[6] := BuildLocaTable;
    newtbls[1] := BuildCmapTable;
    newtbls[5] := BuildHmtxTable;
    newtbls[9] := BuildPostTable;
  finally
    nst.Free;
  end;

  fs := TMemoryStream.Create;
  try
    checksum := WriteFileHeader(Length(newTableNames));
    offset := 12 + 16 * Length(newTableNames);
    SetLength(newOffsetsTables, Length(newTableNames));
    for I := 0 to Length(newTableNames) - 1 do
    begin
      checksum := checksum + WriteTableHeader(newTableNames[i], offset, newtbls[I]);
      newOffsetsTables[I] := offset;
      offset := offset + Round(((Length(newtbls[i]) + 3) / 4) * 4);
    end;

    checksum := $B1B0AFBA - (checksum and $FFFFFFFF);
    newtbls[3][8] := checksum shr 24;
    newtbls[3][9] := checksum shr 16;
    newtbls[3][10] := checksum shr 8;
    newtbls[3][11] := checksum;

    for I := 0 to Length(newTableNames) - 1 do
    begin
      fs.Position := newOffsetsTables[I];
      WriteTableBody(newtbls[I]);
    end;

    FTTFEmbed.LoadFromStream(fs);
  finally
    fs.Free;
  end;
{$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.UnitsPerEm: UInt32;
begin
  Result := FUnitsPerEm;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressed: TStringStream;
begin
  {$IFDEF WEBLIB}
  Result := FTTFDataStream;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := nil;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressedLength: Int64;
begin
  Result := 0;
  {$IFDEF WEBLIB}
  if Assigned(FTTFDataStream) then
    Result := FTTFDataStream.Size;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataLength: Integer;
begin
  {$IFDEF WEBLIB}
  Result := FTTFEmbed.Size;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := 0;
  {$ENDIF}
end;

{$IFDEF WEBLIB}
function TTMSFNCGeneralPDFLibFontInitializer.CompressFontStream(
  AStream: TMemoryStream): TStringStream;
var
  vDest: TStringStream;
  vSource: TMemoryStream;
  vCompressor: TCompressionStream;
begin
  Result := nil;
  if AStream.Size = 0 then
    Exit;

  vDest := CreateStringStream;
  try
  	vCompressor := TCompressionStream.Create(clMax, vDest);
    try
      vSource := AStream;
      vCompressor.CopyFrom(vSource, 0);
    finally
      vCompressor.Free;
    end;

    vDest.Position := 0;
    Result := vDest;
  finally
  end;
end;

procedure TTMSFNCGeneralPDFLibInitializer.InternalInitializeFontProperties(AStream: TStream; var AFamilyName: string; var ABold: Boolean; var AItalic: Boolean; var AFixed: Boolean; var AAscent: Integer; var ADescent: Integer; var AItalicAngle: Single; var ACapHeight: Integer; var AFontBox: TRect);
var
  tblname: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblos2: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblpost: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblhead: TTMSFNCGeneralPDFLibFontTableDirEntry;
  ps: TTMSFNCGeneralPDFLibFontPostScript;
  h: TTMSFNCGeneralPDFLibFontHead;
  osdt: TTMSFNCGeneralPDFLibFontOS2Data;
  N: TTMSFNCGeneralPDFLibFontNameRecord;
  E: TTMSFNCGeneralPDFLibFontNameEntries;
  I, J, cnt : Integer;
  StringOffset: Word;
  sp: LongWord;
  tbldr: TTMSFNCGeneralPDFLibFontTableDir;
  tbls: TTMSFNCGeneralPDFLibFontTableDirEntries;
  W: array of UInt16;
  s: string;
begin
  if not Assigned(AStream) then
    Exit;

  AStream.Position := 0;
  if AStream.Size = 0 then
    Exit;

  tbldr.st := ReadUInt32(AStream) div 65536;
  tbldr.nt := ReadUInt16(AStream);
  tbldr.sr := ReadUInt16(AStream);
  tbldr.es := ReadUInt16(AStream);
  tbldr.rs := ReadUInt16(AStream);

  SetLength(tbls, tbldr.nt);
  for I := 0 to tbldr.nt - 1 do
  begin
    tbls[I].t := ReadString(AStream, 4);
    tbls[I].cs := ReadUInt32(AStream);
    tbls[I].o := ReadUInt32(AStream);
    tbls[I].l := ReadUInt32(AStream);
  end;

  tblname := GetTableForName(tbldr, tbls, ttname);
  if tblname.o <= AStream.Size then
  begin
    AStream.Position := tblname.o;
    sp := AStream.Position;
    ReadUInt16(AStream);
    cnt := ReadUInt16(AStream);
    StringOffset := ReadUInt16(AStream);
    SetLength(E, cnt);
    for I := 0 to cnt - 1 do
    begin
      N.pID := ReadUInt16(AStream);
      N.eID := ReadUInt16(AStream);
      N.lID := ReadUInt16(AStream);
      N.nID := ReadUInt16(AStream);
      N.sl := ReadUInt16(AStream);
      N.so := ReadUInt16(AStream);
      E[i].i := N;
    end;

    for I := 0 to cnt - 1 do
    begin
      AStream.Position := Int64(sp) + StringOffset + E[i].i.so;
      if E[i].i.eID = 1 then
      begin
        SetLength(W, E[i].i.sl div 2);

        for J := 0 to Length(W) - 1 do
          W[J] := ReadUInt16(AStream);

        asm
          s = String.fromCharCode.apply(null, W);
        end;

        E[i].v := s;
      end
      else
        E[i].v := ReadString(AStream, E[i].i.sl);

      if (AFamilyName = '') and (E[i].i.nID = 1) and (E[i].i.lID = 1033) and (E[i].i.eID = 1) then
        AFamilyName := E[i].v;
    end;
  end;

  tblos2 := GetTableForName(tbldr, tbls, ttOS2);
  if tblos2.o <= AStream.Size then
  begin
    AStream.Position := tblos2.o;
    osdt.v := ReadUInt16(AStream);
    osdt.xavgchw := ReadInt16(AStream);
    osdt.uswecl := ReadUInt16(AStream);
    osdt.uswicl := ReadUInt16(AStream);
    osdt.fst := ReadInt16(AStream);
    osdt.ysubxsize := ReadInt16(AStream);
    osdt.ysubysize := ReadInt16(AStream);
    osdt.ysubxoff := ReadInt16(AStream);
    osdt.ysubyoff := ReadInt16(AStream);
    osdt.ysupxsize := ReadInt16(AStream);
    osdt.ysupysize := ReadInt16(AStream);
    osdt.ysupxoff := ReadInt16(AStream);
    osdt.ysupyoff := ReadInt16(AStream);
    osdt.ystos := ReadInt16(AStream);
    osdt.ystop := ReadInt16(AStream);
    osdt.sfc := ReadInt16(AStream);
    osdt.pns := ReadBytes(AStream, 10);
    osdt.ulur1 := ReadUInt32(AStream);
    osdt.ulur2 := ReadUInt32(AStream);
    osdt.ulur3 := ReadUInt32(AStream);
    osdt.ulur4 := ReadUInt32(AStream);
    osdt.avndid := ReadString(AStream, 4);
    osdt.fssel := ReadUInt16(AStream);
    osdt.usfchidx := ReadUInt16(AStream);
    osdt.uslchidx := ReadUInt16(AStream);
    osdt.stasc := ReadInt16(AStream);
    osdt.stdesc := ReadInt16(AStream);
    osdt.stlgap := ReadInt16(AStream);
    osdt.uswasc := ReadUInt16(AStream);
    osdt.uswdesc := ReadUInt16(AStream);

    if osdt.v >= 1 then
    begin
      osdt.ulcpr1 := ReadUInt32(AStream);
      osdt.ulcpr2 := ReadUInt32(AStream);
    end;

    if osdt.v >= 2 then
    begin
      osdt.sxh := Int16(ReadUInt16(AStream));
      osdt.sch := Int16(ReadUInt16(AStream));
      osdt.usdc := ReadUInt16(AStream);
      osdt.usbc := ReadUInt16(AStream);
      osdt.usmaxct := ReadUInt16(AStream);
    end;

    ABold := (osdt.fssel and 32) <> 0;
    AAscent := osdt.stasc;
    ADescent := osdt.stdesc;
    if osdt.v >= 2 then
      ACapHeight := osdt.sch
    else
      ACapHeight := AAscent;
  end;

  tblPost := GetTableForName(tbldr, tbls, ttpost);
  if tblPost.o < AStream.Size then
  begin
    AStream.Position := tblPost.o;
    ps.ft := ReadFixed(AStream);
    ps.ia := ReadInt32(AStream);
    ps.ups := ReadInt16(AStream);
    ps.utck := ReadInt16(AStream);
    ps.isfp := ReadUInt32(AStream);
    ps.minmtype42 := ReadUInt32(AStream);
    ps.maxmtype42 := ReadUInt32(AStream);
    ps.minmtype1 := ReadUInt32(AStream);
    ps.maxmtype1 := ReadUInt32(AStream);

    AItalicAngle := ps.ia / 65536;
    AItalic := AItalicAngle <> 0;
    AFixed := ps.isfp <> 0;
  end;

  tblHead := GetTableForName(tbldr, tbls, ttHead);
  if tblHead.o < AStream.Size then
  begin
    AStream.Position := tblHead.o;
    h.v := ReadFixed(AStream);
    h.fr := ReadFixed(AStream);
    h.csa := ReadUInt32(AStream);
    h.mn := ReadUInt32(AStream);
    h.f := ReadUInt16(AStream);
    h.upe := ReadUInt16(AStream);
    h.cd := ReadInt32(AStream) + ReadInt32(AStream);
    h.md := ReadInt32(AStream) + ReadInt32(AStream);
    for i := 0 to 3 do
      h.bb[i] := ReadInt16(AStream);
    h.ms := ReadUInt16(AStream);
    h.lrpp := ReadUInt16(AStream);
    h.fdh := ReadInt16(AStream);
    h.ilf := ReadInt16(AStream);
    h.gldf := ReadInt16(AStream);

    AFontBox := Rect(h.bb[0], h.bb[3], h.bb[2], h.bb[1]);
  end;
end;

{$WARNINGS OFF}
procedure TTMSFNCGeneralPDFLibFontInitializer.InternalInitializeCharWidths(AStream: TStream);
var
  i, n, glyphId, glyphIndex, maxGlyphId: Integer;
  off: UInt32;
  h: TTMSFNCGeneralPDFLibFontCmapHeader;
  hd: TTMSFNCGeneralPDFLibFontCmapHead;
  ft: TTMSFNCGeneralPDFLibFontCmapFormat4;
  mxp: TTMSFNCGeneralPDFLibFontMaxP;
  fdht, fdhh: TBytes;
  tblcmap, tblmaxp, tblhead, tblhmtx, tblhhea: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tbldr: TTMSFNCGeneralPDFLibFontTableDir;
  tbls: TTMSFNCGeneralPDFLibFontTableDirEntries;
  pid, eid: UInt16;
  J: Integer;
  oldp, savep: Int64;
  glyphw: TTMSFNCPDFGraphicsLibFontCharWidths;
  uem: UInt32;
  wl, nol: UInt16;
  endCount: array of UInt16;
  startCount: array of UInt16;
  idDelta: array of UInt16;
  idRangeOffsetPosition: Int64;
  idRangeOffset: array of UInt16;
  start, &end, delta, rangeOffset: Integer;
  segmentRangeOffset, glyphOffset: Int64;
begin
  if not Assigned(AStream) then
    Exit;

  AStream.Position := 0;
  if AStream.Size = 0 then
    Exit;

  tbldr.st := ReadUInt32(AStream) div 65536;
  tbldr.nt := ReadUInt16(AStream);
  tbldr.sr := ReadUInt16(AStream);
  tbldr.es := ReadUInt16(AStream);
  tbldr.rs := ReadUInt16(AStream);

  SetLength(tbls, tbldr.nt);
  for I := 0 to tbldr.nt - 1 do
  begin
    tbls[I].t := ReadString(AStream, 4);
    tbls[I].cs := ReadUInt32(AStream);
    tbls[I].o := ReadUInt32(AStream);
    tbls[I].l := ReadUInt32(AStream);
  end;

  tblMaxp := GetTableForName(tbldr, tbls, ttMaxp);
  if tblMaxp.o < AStream.Size then
  begin
    AStream.Position := tblMaxp.o;
    mxp.v := ReadFixed(AStream);
    mxp.ng := ReadUInt16(AStream);
  end;

  tblcmap := GetTableForName(tbldr, tbls, ttcmap);
  if tblcmap.o <= AStream.Size then
  begin
    AStream.Position := tblcmap.o;

    h.v := ReadUInt16(AStream);
    h.n := ReadUInt16(AStream);

    off := -1;
    for I := 0 to h.n - 1 do
    begin
      pid := ReadUInt16(AStream, (i * 8));
      eid := ReadUInt16(AStream, (i * 8) + 2);
      if pid = 3 then
      begin
        if eid = 0 then
        begin
          off := ReadUInt32(AStream, 4 + (i * 8));
        end
        else if eid = 1 then
        begin
          off := ReadUInt32(AStream, 4 + (i * 8));
          Break;
        end;
      end;
    end;

    if (off <> 0) and (off and 1 = 0) then
    begin
      AStream.Position := tblcmap.o + off;
      savep := tblcmap.o;
      ft.fmt := ReadUInt16(AStream);

      if ft.fmt = 4 then
      begin
        ft.l := ReadUInt16(AStream);
        ft.lng := ReadUInt16(AStream);
        ft.sgX2 := ReadUInt16(AStream);
        n := ft.sgX2 div 2;
        ft.shrange := ReadUInt16(AStream);
        ft.selr := ReadUInt16(AStream);
        ft.rng := ReadUInt16(AStream);

        try

          endCount := ReadUInt16Array(AStream, n);
          ReadUInt16(AStream);
          startCount := ReadUInt16Array(AStream, n);
          idDelta := ReadUInt16Array(AStream, n);
          idRangeOffsetPosition := AStream.Position;
          idRangeOffset := ReadUInt16Array(AStream, n);

          FCharArray.c := 0;
          SetLength(FCharArray.v, 0);
          SetLength(FCharWidths, 0);

          maxGlyphId := 0;
          for I := 0 to n - 1 do
          begin
            start := startCount[i];
            &end := endCount[I];
            delta := idDelta[i];
            rangeOffset := idRangeOffset[i];
            segmentRangeOffset := idRangeOffsetPosition + (i * 2) + rangeOffset;
            if (start <> 65535) and (&end <> 65535) then
            begin
              for J := start to &end do
              begin
                if rangeOffset = 0 then
                begin
                  glyphId := (j + delta) and $FFFF;
                  maxGlyphId := Max(glyphId, maxGlyphId);

                  Inc(FCharArray.c);
                  SetLength(FCharArray.v, FCharArray.c);
                  SetLength(FCharWidths, FCharArray.c);

                  FCharArray.v[FCharArray.c - 1] := J;
                  FCharWidths[FCharArray.c - 1].g := glyphId;
                end
                else
                begin
                  glyphOffset := segmentRangeOffset + ((J - start) * 2);
                  AStream.Position := glyphOffset;
                  glyphIndex := ReadUInt16(AStream);
                  if (glyphIndex <> 0) then
                  begin
                    glyphIndex := (glyphIndex + delta) and $FFFF;
                    maxGlyphId := Max(glyphIndex, maxGlyphId);

                    Inc(FCharArray.c);
                    SetLength(FCharArray.v, FCharArray.c);
                    SetLength(FCharWidths, FCharArray.c);

                    FCharArray.v[FCharArray.c - 1] := J;
                    FCharWidths[FCharArray.c - 1].g := glyphIndex;
                  end;
                end;
              end;
            end;
          end;

          tblhmtx := GetTableForName(tbldr, tbls, tthmtx);
          if tblhmtx.o <= AStream.Size then
          begin
            AStream.Position := tblhmtx.o;
            fdht := ReadBytes(AStream, tblhmtx.l);

            tblhhea := GetTableForName(tbldr, tbls, tthhea);
            if tblhhea.o <= AStream.Size then
            begin
              AStream.Position := tblhhea.o;
              fdhh := ReadBytes(AStream, tblhhea.l);

              tblhead := GetTableForName(tbldr, tbls, tthead);
              if tblhead.o <= AStream.Size then
              begin
                AStream.Position := tblhead.o;
                ReadBytes(AStream, 18);
                hd.uem := ReadUInt16(AStream);

                uem := hd.uem;

                if uem <> 0 then
                begin
                  FUnitsPerEm := uem;
                  wl := (GetUInt16(fdht, 0) * 1000) div uem;
                  if IsFixedWidth then
                  begin
                    for i := 0 to FCharArray.c - 1 do
                      FCharWidths[i].w := wl
                  end
                  else
                  begin
                    nol := GetUInt16(fdhh, 17);
                    for i := 0 to FCharArray.c - 1 do
                    begin
                      if FCharWidths[i].G <> 0 then
                      begin
                        if FCharWidths[i].G <= nol then
                          FCharWidths[i].w := (GetUInt16(fdht, FCharWidths[i].g * 2) * 1000) div uem
                        else
                          FCharWidths[i].w := wl;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
        end;
      end;
    end;
  end;
  AStream.Position := 0;
end;
{$WARNINGS ON}
{$ENDIF}

function TTMSFNCGeneralPDFLibFontInitializer.GetUnitsPerEM: UInt32;
begin
  Result := FUnitsPerEm;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetGlyphIds: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := FGlyphIDs;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := FUsedCharArray;
end;

{ TTMSFNCGeneralPDFLibInitializer }

constructor TTMSFNCGeneralPDFLibInitializer.Create;

{$IFDEF WEBLIB}
procedure FontFileSearch;
var
  fn{, fnu}: string;
  fni: TTMSFNCGeneralPDFLibFontItem;
  fci: TTMSFNCPDFLibFontCacheItem;
  cnt: Integer;
  I: Integer;
begin
  cnt := GetFontCacheCount;
  for I := 0 to cnt - 1 do
  begin
    fci := GetFontCacheItem(I);
    if Assigned(fci) and fci.Loaded then
    begin
      //fnu := UpperCase(ExtractFileExt(fci.Name));
      //if (fnu = '.TTF') then
      begin
        fni := TTMSFNCGeneralPDFLibFontItem.Create;
        fni.FileName := fn;
        fni.Data := fci.Data;
        FFontCol.Add(fni);
      end;
    end;
  end;
end;

var
  fname: string;
  ms: TMemoryStream;
  fti: TTMSFNCGeneralPDFLibFontItem;
  b, i: Boolean;
  f: Boolean;
  bb: TRect;
  a, d, ch: Integer;
  it: Single;
  J: Integer;
{$ENDIF}
begin
{$IFDEF WEBLIB}
  FFontCol := TTMSFNCGeneralPDFLibFontItems.Create;
  FontFileSearch;

  for J := 0 to FFontCol.Count - 1 do
  begin
    fti := FFontCol[J];
    fname := '';
    b := False;
    i := False;
    f := False;
    a := 0;
    d := 0;
    ch := 0;
    it := 0;
    bb := Rect(0, 0, 0, 0);
    ms := TMemoryStream.Create;
    try
      asm
        ms.Write(fti.FData.Data, fti.FData.Data.length);
      end;
      ms.Position := 0;
      InternalInitializeFontProperties(ms, fname, b, i, f, a, d, it, ch, bb);
      fti.Name := fname;
      fti.Style := [];
      if b then
        fti.Style := fti.Style + [TFontStyle.fsBold];
      if i then
        fti.Style := fti.Style + [TFontStyle.fsItalic];
      fti.Fixed := f;
      fti.Ascent := a;
      fti.Descent := d;
      fti.ItalicAngle := it;
      fti.CapHeight := ch;
      fti.BBox := bb;
    finally
      ms.Free;
    end;
  end;
{$ENDIF}
end;

destructor TTMSFNCGeneralPDFLibInitializer.Destroy;
begin
  {$IFDEF WEBLIB}
  if Assigned(FFontCol) then
  begin
    FFontCol.Free;
    FFontCol := nil;
  end;
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCGeneralPDFLibInitializer.InitializeFontFallBackList(
  AList: TStrings);
begin
  AList.Add('Tahoma');
  AList.Add('SimSun');
  AList.Add('Arial Unicode MS');
end;

{$IFDEF WEBLIB}
function TTMSFNCGeneralPDFLibFontItems.GetItem(Index: Integer): TTMSFNCGeneralPDFLibFontItem;
begin
  Result := TTMSFNCGeneralPDFLibFontItem(inherited Items[Index]);
end;

procedure TTMSFNCGeneralPDFLibFontItems.SetItem(Index: Integer; const Value: TTMSFNCGeneralPDFLibFontItem);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

end.


