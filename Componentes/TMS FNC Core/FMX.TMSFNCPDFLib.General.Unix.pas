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

unit FMX.TMSFNCPDFLib.General.Unix;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF FMXLIB}
{$IFDEF LINUX}
{$DEFINE USEUNIX}
{$DEFINE LINUX_OK}
{$ENDIF}
{$ENDIF}

{$IFDEF LCLLIB}
{$IFDEF UNIX}
{$DEFINE USEUNIX}
{$ENDIF}
{$ENDIF}

{$RANGECHECKS OFF}

interface

uses
  Classes, Types, FMX.TMSFNCPDFCoreLibBase,
  FMX.Graphics
  {$IFDEF FMXLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,{%H-}fgl
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  AnsiString = string;
  WideString = string;
  PtrUInt = UInt64;
  {$ENDIF}
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

  {$IFDEF USEUNIX}

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
  public
    property FileName: string read FFileName write FFileName;
    property Name: String read FName write FName;
    property Style: TFontStyles read FStyle write FStyle;
    property Fixed: Boolean read FFixed write FFixed;
    property BBox: TRect read FBBox write FBBox;
    property Ascent: Integer read FAscent write FAscent;
    property Descent: Integer read FDescent write FDescent;
    property CapHeight: Integer read FCapHeight write FCapHeight;
    property ItalicAngle: Single read FItalicAngle write FItalicAngle;
  end;

  TTMSFNCGeneralPDFLibFontItems = TObjectList<TTMSFNCGeneralPDFLibFontItem>;
  {$ENDIF}

  { TTMSFNCGeneralPDFLibInitializer }

  TTMSFNCGeneralPDFLibInitializer = class
  private
    {$IFDEF USEUNIX}
    FFontCol: TTMSFNCGeneralPDFLibFontItems;
    {$ENDIF}
  protected
    {$IFDEF USEUNIX}
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
    {$IFDEF USEUNIX}
    FTTFMetrics: TTMSFNCGeneralPDFLibFontMetrics;
    FTTFDataStream: TStringStream;
    FTTFFileStream: TMemoryStream;
    FTTFCreateFontPackage: Boolean;
    FTTFEmbed: TMemoryStream;
    {$ENDIF}
    FBase: string;
    FSize: Single;
    FUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    FCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    FCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    FUnitsPerEm: UInt32;
    FMainInitializer: TTMSFNCGeneralPDFLibInitializer;
    FIsFixedWidth: Boolean;
  protected
    {$IFDEF USEUNIX}
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
    function GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetUnitsPerEM: UInt32;
    function GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function UnitsPerEm: UInt32;
    function GetTTFDataLength: Integer;
    function GetTTFDataCompressedLength: Int64;
    function GetTTFDataCompressed: TStringStream;
    procedure CompressTTFData;
    procedure InitializeCharWidths;
    procedure InitializeFontFile;
    property IsFixedWidth: Boolean read FIsFixedWidth write FIsFixedWidth;
  end;

implementation

uses
  SysUtils, FMX.TMSFNCUtils
  {$IFDEF LCLLIB}
  ,{%H-}zstream
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  ,zlib
  {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF USEUNIX}

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

  TTMSFNCGeneralPDFLibFontNameRecord = Packed Record
    pID: UInt16;
    eID: UInt16;
    lID: UInt16;
    nID: UInt16;
    sl: UInt16;
    so: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontNameEntry = Packed Record
    i: TTMSFNCGeneralPDFLibFontNameRecord;
    v : String;
  end;
  TTMSFNCGeneralPDFLibFontNameEntries = array of TTMSFNCGeneralPDFLibFontNameEntry;

  TTMSFNCGeneralPDFLibFontOS2Data = Packed Record
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
    sfc: Int16;
    pns: Array[0..9] of byte;
    ulur1: UInt32;
    ulur2: UInt32;
    ulur3: UInt32;
    ulur4: UInt32;
    avndid : Array[0..3] of byte;
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

  TTMSFNCGeneralPDFLibFontCmapHead = packed record
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

  TTMSFNCGeneralPDFLibFontCmapArray = packed array[byte] of packed record
    pID: UInt16;
    psID: UInt16;
    o: UInt32;
  end;

  TTMSFNCGeneralPDFLibFontCmapHeader = packed record
    v: UInt16;
    n: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontCmapFormat4 = packed record
    fmt: UInt16;
    l: UInt16;
    lng: UInt16;
    sgX2: UInt16;
    shrange: UInt16;
    selr: UInt16;
    rng: UInt16;
  end;

  TTMSFNCGeneralPDFLibFontFixedVersionRec = packed record
    case Integer of
      0:  (mnr, mjr: UInt16);
      1:  (v: UInt32);
  end;

  TTMSFNCGeneralPDFLibFontHead = Packed record
    fv: TTMSFNCGeneralPDFLibFontFixedVersionRec;
    fr: TTMSFNCGeneralPDFLibFontFixedVersionRec;
    csa: UInt32;
    mn: UInt32;
    f: UInt16;
    upe: UInt16;
    ct: Int64;
    md: Int64;
    bb: Packed array[0..3] of Int16;
    ms: UInt16;
    lrpp: UInt16;
    fdh: Int16;
    ilf: Int16;
    gldf: Int16;
  end;

  TTMSFNCGeneralPDFLibFontPostScript = Packed Record
    fmt: TTMSFNCGeneralPDFLibFontFixedVersionRec;
    ia: Int32;
    ups: Int16;
    utck: Int16;
    isfp: UInt32;
    minmtype42: UInt32;
    maxmtype42: UInt32;
    minmtype1: UInt32;
    maxmtype1: UInt32;
  end;

  TTMSFNCGeneralPDFLibFontTableDir = Packed Record
    fv : TTMSFNCGeneralPDFLibFontFixedVersionRec;
    nt : UInt16;
    sr : UInt16;
    es : UInt16;
    rs : UInt16;
  end;

  TTMSFNCGeneralPDFLibFontTableDirEntry = Packed Record
    t: Array[0..3] of Byte;
    cs: UInt32;
    o: UInt32;
    l: UInt32;
  end;
  TTMSFNCGeneralPDFLibFontTableDirEntries = array of TTMSFNCGeneralPDFLibFontTableDirEntry;

  TTMSFNCGeneralPDFLibFontTableType = (ttUnknown, ttHead, tthhea, ttmaxp, tthmtx, ttcmap, ttname, ttOS2, ttpost, ttglyf, ttloca, ttprep, ttfpgm, ttcvt);

  PInt16Array = ^TInt16Array;
  TInt16Array = array[Byte] of Int16;

const
  TableNames: array[TTMSFNCGeneralPDFLibFontTableType] of String = ('','head','hhea','maxp','hmtx','cmap','name','OS/2','post','glyf','loca','prep','fpgm','cvt ');

function GetBit(const Bits; aIndex: Integer): Boolean;
begin
  Result := PIntegerArray(@Bits)^[aIndex shr 5] and (1 shl (aIndex and 31)) <> 0;
end;

function FixMinorVersion(const AMinor: word): word;
var
  d: double;
begin
  d := AMinor / 65536;
  Result := round(d*10000);
end;

{$IFDEF FMXLIB}
{$IFDEF LINUX}
{$RANGECHECKS OFF}
function BEToN(AValue: UInt16): UInt16; overload;
begin
  Result := (AValue shr 8) or (AValue shl 8);
end;

function BEToN(AValue: UInt32): UInt32; overload;
begin
  Result := (AValue shr 24) or ((AValue and $FF0000) shr 8) or ((AValue and $FF00) shl 8) or ((AValue and $FF) shl 24);
end;

function BEToN(AValue: Int16): Int16; overload;
begin
  Result := BEToN(UInt16(AValue));
end;

function BEToN(AValue: Int64): Int64; overload;
var
  t1, t2: Int64;
begin
  t1 := AValue shr 32;
  t1 := BEToN(UInt32(t1));
  t2 := AValue and $FFFFFFFF;
  t2 := BEToN(UInt32(t2));

  Result := (t2 shl 32) or t1;
end;

function BEToN(AValue: Int32): Int32; overload;
begin
  Result := BEToN(UInt32(AValue));
end;

{$ENDIF}
{$ENDIF}

function ReadUInt32(AStream: TStream): UInt32;
begin
  Result := 0;
  AStream.ReadBuffer(Result,SizeOf(Result));
  Result := BEtoN(Result);
end;

function ReadUInt16(AStream: TStream): UInt16;
begin
  Result := 0;
  AStream.ReadBuffer(Result,SizeOf(Result));
  Result := BEtoN(Result);
end;

procedure FillMem(Dest: pointer; Size: Int32; Data: Byte );
begin
  FillChar(Dest^, Size, Data);
end;

function GetTableType(const AName : String): TTMSFNCGeneralPDFLibFontTableType;
begin
  Result := High(TTMSFNCGeneralPDFLibFontTableType);
  while (Result <> ttUnknown) and (CompareText(AName, TableNames[Result]) <> 0) do
    Result := Pred(Result);
end;

function GetTableForName(ATableDir: TTMSFNCGeneralPDFLibFontTableDir; ATables: TTMSFNCGeneralPDFLibFontTableDirEntries; ATableType: TTMSFNCGeneralPDFLibFontTableType): TTMSFNCGeneralPDFLibFontTableDirEntry;
var
  I, K: Integer;
  b:  TBytes;
begin
  for I := 0 to ATableDir.nt - 1 do
  begin
    SetLength(b, Length(ATables[I].t));
    for K := 0 to Length(b) - 1 do
      b[K] := ATables[I].t[K];

    if GetTableType(TEncoding.UTF8.GetString(b)) = ATableType then
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
  {$IFDEF USEUNIX}
  FTTFDataStream := CompressFontStream(FTTFEmbed);
  {$ENDIF}
end;

constructor TTMSFNCGeneralPDFLibFontInitializer.Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const AStyle: TFontStyles; const ASize: Single);
{$IFDEF USEUNIX}
var
  fn: String;
  fontit: TTMSFNCGeneralPDFLibFontItem;

  function SearchForFont(AFontName: string; AStyle: TFontStyles): TTMSFNCGeneralPDFLibFontItem;
  var
    fti: TTMSFNCGeneralPDFLibFontItem;
  begin
    Result := nil;
    if Assigned(FMainInitializer) and Assigned(FMainInitializer.FFontCol) then
    begin
      for fti in FMainInitializer.FFontCol do
      begin
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
  {$IFDEF USEUNIX}
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
    fn := fontit.FileName;
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
    fn := '';
    FTTFMetrics.CapHeight := 0;
    FTTFMetrics.Ascent := 0;
    FTTFMetrics.Descent := 0;
    FTTFMetrics.FontBox := Bounds(0, 0, 0, 0);
    FTTFMetrics.ItalicAngle := 0;
    FTTFMetrics.Fixed := False;
    FTTFMetrics.TrueType := False;
  end;

  if FileExists(fn) then
    FTTFFileStream.LoadFromFile(fn);

  FTTFFileStream.Position := 0;
  {$ENDIF}
end;

destructor TTMSFNCGeneralPDFLibFontInitializer.Destroy;
begin
  {$IFDEF USEUNIX}
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
  inherited;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
begin
  Result := FCharArray;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
begin
  Result := FCharWidths;
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
  {$IFDEF USEUNIX}
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

function TTMSFNCGeneralPDFLibFontInitializer.GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := nil;
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeCharWidths;
begin
  {$IFDEF USEUNIX}
  if Assigned(FMainInitializer) then
    InternalInitializeCharWidths(FTTFFileStream);
  {$ENDIF}
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeFontFile;
begin
  InternalInitializeFontFile;
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InternalInitializeFontFile;
begin
{$IFDEF USEUNIX}
  if not Assigned(FTTFFileStream) then
    Exit;

  FTTFFileStream.Position := 0;
  if FTTFFileStream.Size = 0 then
    Exit;

  FTTFEmbed.LoadFromStream(FTTFFileStream);
{$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.UnitsPerEm: UInt32;
begin
  Result := FUnitsPerEm;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressed: TStringStream;
begin
  {$IFDEF USEUNIX}
  Result := FTTFDataStream;
  {$ENDIF}
  {$IFNDEF USEUNIX}
  Result := nil;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressedLength: Int64;
begin
  Result := 0;
  {$IFDEF USEUNIX}
  if Assigned(FTTFDataStream) then
    Result := FTTFDataStream.Size;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataLength: Integer;
begin
  {$IFDEF USEUNIX}
  Result := FTTFEmbed.Size;
  {$ENDIF}
  {$IFNDEF USEUNIX}
  Result := 0;
  {$ENDIF}
end;

{$IFDEF USEUNIX}
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
  tbldr: TTMSFNCGeneralPDFLibFontTableDir;
  tbls: TTMSFNCGeneralPDFLibFontTableDirEntries;
  tblname: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblos2: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblpost: TTMSFNCGeneralPDFLibFontTableDirEntry;
  tblhead: TTMSFNCGeneralPDFLibFontTableDirEntry;
  ps: TTMSFNCGeneralPDFLibFontPostScript;
  h: TTMSFNCGeneralPDFLibFontHead;
  osdt: TTMSFNCGeneralPDFLibFontOS2Data;
  I: Integer;
  N: TTMSFNCGeneralPDFLibFontNameRecord;
  E: TTMSFNCGeneralPDFLibFontNameEntries;
  J, cnt : Integer;
  StringOffset: Word;
  S : AnsiString;
  W : Widestring;
  WA : Array of word;
  sp: LongWord;
begin
  if not Assigned(AStream) then
    Exit;

  AStream.Position := 0;
  if AStream.Size = 0 then
    Exit;

  AStream.ReadBuffer({%H-}tbldr, Sizeof(TTMSFNCGeneralPDFLibFontTableDir));
  tbldr.fv.v := BEtoN(tbldr.fv.v);
  tbldr.fv.mnr := FixMinorVersion(tbldr.fv.mnr);
  tbldr.nt := BeToN(tbldr.nt);
  tbldr.sr := BeToN(tbldr.sr);
  tbldr.es := BeToN(tbldr.es);
  tbldr.rs := BeToN(tbldr.rs);
  SetLength(tbls, tbldr.nt);
  AStream.ReadBuffer(tbls[0], tbldr.nt * Sizeof(TTMSFNCGeneralPDFLibFontTableDirEntry));

  for I := 0 to Length(tbls) - 1 do
  begin
    tbls[I].cs := BeToN(tbls[I].cs);
    tbls[I].o := BeToN(tbls[I].o);
    tbls[I].l := BeToN(tbls[I].l);
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
    FillMem(@N, SizeOf(TTMSFNCGeneralPDFLibFontNameRecord), 0);
    for I := 0 to cnt - 1 do
    begin
      AStream.ReadBuffer(N, SizeOf(TTMSFNCGeneralPDFLibFontNameRecord));
      N.pID := BeTon(N.pID);
      N.eID := BeTon(N.eID);
      N.lID := BeTon(N.lID);
      N.nID := BeTon(N.nID);
      N.sl := BeTon(N.sl);
      N.so := BeToN(N.so);
      E[i].i := N;
    end;

    for I := 0 to cnt - 1 do
    begin
      AStream.Position := Int64(sp) + StringOffset + E[i].i.so;
      if E[i].i.eID = 1 then
      begin
        SetLength(WA, E[i].i.sl div 2);
        SetLength(W, Length(WA));
        AStream.Read(WA[0],SizeOf(Word) * Length(W));
        for J := 0 to Length(WA)-1 do
          W[J + 1] := WideChar(Beton(WA[J]));
        E[i].v := string(W);
      end
      else
      begin
        SetLength(S, E[i].i.sl);
        AStream.Read(S[1], SizeOf(Byte) * Length(S));
        E[i].v := S;
      end;

      if (AFamilyName = '') and (E[i].i.nID = 1) and (E[i].i.lID = 1033) and (E[i].i.eID = 1) then
        AFamilyName := E[i].v;
    end;
  end;

  tblos2 := GetTableForName(tbldr, tbls, ttOS2);
  if tblos2.o <= AStream.Size then
  begin
    AStream.Position := tblos2.o;
    {$IFNDEF LCLLIB}
    FillChar({%H-}osdt, SizeOf(TTMSFNCGeneralPDFLibFontOS2Data) div 2,0);
    {$ELSE}
    FillWord({%H-}osdt, SizeOf(TTMSFNCGeneralPDFLibFontOS2Data) div 2,0);
    {$ENDIF}
    AStream.ReadBuffer({%H-}osdt, SizeOf(TTMSFNCGeneralPDFLibFontOS2Data) - 18);
    osdt.v := BeToN(osdt.v);
    osdt.xavgchw := BeToN(osdt.xavgchw);
    osdt.uswecl := BeToN(osdt.uswecl);
    osdt.uswicl := BeToN(osdt.uswicl);
    osdt.fst := BeToN(osdt.fst);
    osdt.ysubxsize := BeToN(osdt.ysubxsize);
    osdt.ysubysize := BeToN(osdt.ysubysize);
    osdt.ysubxoff := BeToN(osdt.ysubxoff);
    osdt.ysubyoff := BeToN(osdt.ysubyoff);
    osdt.ysupxsize := BeToN(osdt.ysupxsize);
    osdt.ysupysize := BeToN(osdt.ysupysize);
    osdt.ysupxoff := BeToN(osdt.ysupxoff);
    osdt.ysupyoff := BeToN(osdt.ysupyoff);
    osdt.ystos := BeToN(osdt.ystos);
    osdt.ystop := BeToN(osdt.ystop);
    osdt.sfc := BeToN(osdt.sfc);
    osdt.ulur1 := BeToN(osdt.ulur1);
    osdt.ulur2 := BeToN(osdt.ulur2);
    osdt.ulur3 := BeToN(osdt.ulur3);
    osdt.ulur4 := BeToN(osdt.ulur4);
    osdt.fssel := BeToN(osdt.fssel);
    osdt.usfchidx := BeToN(osdt.usfchidx);
    osdt.uslchidx := BeToN(osdt.uslchidx);
    osdt.stasc := BeToN(osdt.stasc);
    osdt.stdesc := BeToN(osdt.stdesc);
    osdt.stlgap := BeToN(osdt.stlgap);
    osdt.uswasc := BeToN(osdt.uswasc);
    osdt.uswdesc := BeToN(osdt.uswdesc);

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
    AStream.ReadBuffer({%H-}ps, SizeOf(TTMSFNCGeneralPDFLibFontPostScript));
    ps.fmt.v := BEtoN(ps.fmt.v);
    ps.fmt.mnr := FixMinorVersion(ps.fmt.mnr);
    ps.ia := BeToN(ps.ia);
    ps.ups := BeToN(ps.ups);
    ps.utck := BeToN(ps.utck);
    ps.isfp := BeToN(ps.isfp);
    ps.minmtype42 := BeToN(ps.minmtype42);
    ps.maxmtype42 := BeToN(ps.maxmtype42);
    ps.minmtype1 := BeToN(ps.minmtype1);
    ps.maxmtype1 := BeToN(ps.maxmtype1);

    AItalicAngle := ps.ia / 65536;
    AItalic := AItalicAngle <> 0;
    AFixed := ps.isfp <> 0;
  end;

  tblHead := GetTableForName(tbldr, tbls, ttHead);
  if tblHead.o < AStream.Size then
  begin
    AStream.Position := tblHead.o;
    AStream.ReadBuffer({%H-}h, SizeOf(h));
    h.fv.v := BEtoN(h.fv.v);
    h.fv.mnr := FixMinorVersion(h.fv.mnr);
    h.fr.v := BEtoN(h.fr.v);
    h.fr.mnr := FixMinorVersion(h.fr.mnr);
    h.ct := BEtoN(h.ct);
    h.md := BEtoN(h.md);
    for i := 0 to 3 do
      h.bb[i] := BEtoN(h.bb[i]);
    h.csa := BEtoN(h.csa);
    h.mn := BEtoN(h.mn);
    h.f := BEtoN(h.f);
    h.upe := BEtoN(h.upe);
    h.ms := BEtoN(h.ms);
    h.lrpp := BEtoN(h.lrpp);
    h.fdh := BEtoN(h.fdh);
    h.ilf := BEtoN(h.ilf);
    h.gldf := BEtoN(h.gldf);

    AFontBox := Rect(h.bb[0], h.bb[3], h.bb[2], h.bb[1]);
  end;
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InternalInitializeCharWidths(AStream: TStream);
var
  i, n, c, ndx: Integer;
  off: UInt32;
  gidx: integer;
  id, gi: Integer;
  wl, nol: word;
  fdcm, fdcmr, fdcmrbk, fdhd, fdhdr, fdht, fdhtr, fdhh, fdhhr: TWordDynArray;
  st: ^TTMSFNCGeneralPDFLibFontCmapArray absolute fdcmr;
  h: ^TTMSFNCGeneralPDFLibFontCmapHeader;
  ft: ^TTMSFNCGeneralPDFLibFontCmapFormat4;
  ec, sc: PWordArray;
  ida: PInt16Array;
  idr: PWordArray;
  gia: PWordArray;
  hd: ^TTMSFNCGeneralPDFLibFontCmapHead;
  uem: UInt32;
  tbldr: TTMSFNCGeneralPDFLibFontTableDir;
  tbls: TTMSFNCGeneralPDFLibFontTableDirEntries;
  tblcmap, tblhead, tblhhea, tblhmtx: TTMSFNCGeneralPDFLibFontTableDirEntry;
begin
  if not Assigned(AStream) then
    Exit;

  AStream.Position := 0;
  if AStream.Size = 0 then
    Exit;

  AStream.ReadBuffer({%H-}tbldr, Sizeof(TTMSFNCGeneralPDFLibFontTableDir));
  tbldr.fv.v := BEtoN(tbldr.fv.v);
  tbldr.fv.mnr := FixMinorVersion(tbldr.fv.mnr);
  tbldr.nt := BeToN(tbldr.nt);
  tbldr.sr := BeToN(tbldr.sr);
  tbldr.es := BeToN(tbldr.es);
  tbldr.rs := BeToN(tbldr.rs);
  SetLength(tbls, tbldr.nt);
  AStream.ReadBuffer(tbls[0], tbldr.nt * Sizeof(TTMSFNCGeneralPDFLibFontTableDirEntry));

  for I := 0 to Length(tbls) - 1 do
  begin
    tbls[I].cs := BeToN(tbls[I].cs);
    tbls[I].o := BeToN(tbls[I].o);
    tbls[I].l := BeToN(tbls[I].l);
  end;

  tblcmap := GetTableForName(tbldr, tbls, ttcmap);
  if tblcmap.o <= AStream.Size then
  begin
    AStream.Position := tblcmap.o;
    SetLength(fdcm, tblcmap.l);
    AStream.Read(fdcm[0], Length(fdcm));

    SetLength(fdcmr, Length(fdcm));
    for I := 0 to Length(fdcm) - 1 do
      fdcmr[I] := BEtoN(fdcm[I]);

    fdcmrbk := fdcmr;
    h := @fdcmr[0];
    Inc({%H-}PtrUInt(fdcmr), SizeOf(TTMSFNCGeneralPDFLibFontCmapHeader));
    off := 0;
    for i := 0 to h^.n - 1 do
    begin
      if st^[i].pID = 3 then
      begin
        if st^[i].psID = 0 then
          off := st^[i].o
        else if st^[i].psID = 1 then
        begin
          off := st^[i].o;
          Break;
        end;
      end;
    end;

    if (off <> 0) and (off and 1 = 0) then
    begin
      i := LongRec(off).Lo;
      LongRec(off).Lo := LongRec(off).Hi;
      LongRec(off).Hi := i;

      if off <= Length(fdcmrbk) * 2 then
      begin
        {$HINTS OFF}
        ft := {%H-}Pointer({%H-}PtrUInt(fdcmrbk) + {%H-}off);
        {$HINTS ON}

        if ft^.fmt = 4 then
        begin
          ec := {%H-}pointer({%H-}PtrUInt(@ft^.fmt) + SizeOf(TTMSFNCGeneralPDFLibFontCmapFormat4));
          sc := {%H-}pointer({%H-}PtrUInt(ec) + ft^.sgX2 + 2);
          ida := {%H-}pointer({%H-}PtrUInt(sc) + ft^.sgX2);
          idr := {%H-}Pointer({%H-}PtrUInt(ida) + ft^.sgX2);
          gia := {%H-}Pointer({%H-}PtrUInt(idr) + ft^.sgX2);

          tblhead := GetTableForName(tbldr, tbls, ttHead);
          if tblhead.o <= AStream.Size then
          begin
            AStream.Position := tblhead.o;
            SetLength(fdhd, tblhead.l);
            AStream.Read(fdhd[0], Length(fdhd));

            SetLength(fdhdr, Length(fdhd));
            for I := 0 to Length(fdhd) - 1 do
              fdhdr[I] := BEtoN(fdhd[I]);

            hd := @fdhdr[0];

            tblhmtx := GetTableForName(tbldr, tbls, tthmtx);
            if tblhmtx.o <= AStream.Size then
            begin
              AStream.Position := tblhmtx.o;
              SetLength(fdht, tblhmtx.l);
              AStream.Read(fdht[0], Length(fdht));

              SetLength(fdhtr, Length(fdht));
              for I := 0 to Length(fdht) - 1 do
                fdhtr[I] := BEtoN(fdht[I]);

              tblhhea := GetTableForName(tbldr, tbls, tthhea);
              if tblhhea.o <= AStream.Size then
              begin
                AStream.Position := tblhhea.o;
                SetLength(fdhh, tblhhea.l);
                AStream.Read(fdhh[0], Length(fdhh));

                SetLength(fdhhr, Length(fdhh));
                for I := 0 to Length(fdhh) - 1 do
                  fdhhr[I] := BEtoN(fdhh[I]);

                n := ft^.sgX2 shr 1;

                for i := 0 to n - 1 do
                  FCharArray.c := FCharArray.c + ec^[i] - sc^[i] + 1;

                SetLength(FCharArray.v, FCharArray.c);
                SetLength(FCharWidths, FCharArray.c);

                ndx := 0;
                for i := 0 to n - 1 do
                begin
                  id := ida^[i];
                  gi := idr^[i];
                  if gi <> 0 then
                    gi := gi shr 1 + i - n - sc^[i];

                  for c := sc^[i] to ec^[i] do
                  begin
                    FCharArray.v[ndx] := c;
                    if gi = 0 then
                      gidx := c + id
                    else
                    begin
                      gidx := gia^[gi + c];
                      if gidx <> 0 then
                        Inc(gidx, id);
                    end;

                    FCharWidths[ndx].g := gidx;
                    inc(ndx);
                  end;
                end;

                uem := hd^.uem;

                if uem <> 0 then
                begin
                  FUnitsPerEm := uem;
                  wl := (UInt32(fdhtr[0]) * 1000) div uem;
                  if IsFixedWidth then
                  begin
                    for i := 0 to FCharArray.c - 1 do
                      FCharWidths[i].w := wl
                  end
                  else
                  begin
                    nol := fdhhr[17];
                    for i := 0 to FCharArray.c - 1 do
                    begin
                      if FCharWidths[i].G <> 0 then
                      begin
                        if FCharWidths[i].G <= nol then
                          FCharWidths[i].w := (UInt32(fdhtr[FCharWidths[i].g * 2]) * 1000) div uem
                        else
                          FCharWidths[i].w := wl;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  AStream.Position := 0;
end;
{$ENDIF}

function TTMSFNCGeneralPDFLibFontInitializer.GetUnitsPerEM: UInt32;
begin
  Result := FUnitsPerEm;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := FUsedCharArray;
end;

{ TTMSFNCGeneralPDFLibInitializer }

constructor TTMSFNCGeneralPDFLibInitializer.Create;

{$IFDEF USEUNIX}
procedure FontFileSearch(const dirName: string);
var
  searchResult: TSearchRec;
  fn, fnu: string;
  fni: TTMSFNCGeneralPDFLibFontItem;
begin
  if FindFirst(dirName + '/*', faDirectory, searchResult) = 0 then
  begin
    try
      repeat
        fn := IncludeTrailingBackSlash(dirName) + searchResult.Name;
        if ((searchResult.Attr and faDirectory) = 0) then
        begin
          fnu := UpperCase(ExtractFileExt(searchResult.Name));
          if (fnu = '.TTF') then
          begin
            fni := TTMSFNCGeneralPDFLibFontItem.Create;
            fni.FileName := fn;
            FFontCol.Add(fni);
          end;
        end
        else if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
          FontFileSearch(fn);

      until FindNext(searchResult) <> 0
    finally
      FindClose(searchResult);
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
{$ENDIF}
begin
{$IFDEF USEUNIX}
  FFontCol := TTMSFNCGeneralPDFLibFontItems.Create;
  {$IFDEF LINUX}
  FontFileSearch('/usr/share/fonts/truetype');
  if FFontCol.Count = 0 then
    FontFileSearch('/usr/share/fonts');
  {$ENDIF}
  {$IFDEF DARWIN}
  FontFileSearch('/library/fonts/');
  if FFontCol.Count = 0 then
    FontFileSearch('/system/library/fonts/supplemental/');
  {$ENDIF}

  for fti in FFontCol do
  begin
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
      ms.LoadFromFile(fti.FileName);
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
  {$IFDEF USEUNIX}
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
  {$IFDEF LINUX}
  AList.Add('DejaVu Sans');
  AList.Add('FreeMono');
  AList.Add('TakaoPGothic');
  {$ENDIF}
  {$IFDEF DARWIN}
  AList.Add('Arial');
  AList.Add('Tahoma');
  AList.Add('TakaoPGothic');
  AList.Add('AppleGothic');
  {$ENDIF}
  AList.Add('Arial Unicode MS');
end;

end.


