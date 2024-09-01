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

unit FMX.TMSFNCPDFLib.General.iOS;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes, Types, FMX.TMSFNCPDFCoreLibBase,
  FMX.Graphics
  {$IFDEF IOS}
  ,iOSApi.CocoaTypes, iOSApi.CoreText
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
  TTMSFNCGeneralPDFLibFontMetrics = record
    CapHeight: Integer;
    Ascent: Integer;
    Descent: Integer;
    FontBox: TRect;
    ItalicAngle: Integer;
    Fixed: Boolean;
    TrueType: Boolean;
  end;

  TTMSFNCGeneralPDFLibInitializer = class
  public
    constructor Create;
    procedure InitializeFontFallBackList(AList: TStrings);
    destructor Destroy; override;
  end;

  TTMSFNCGeneralPDFLibFontInitializer = class
  private
    {$IFDEF IOS}
    FFontRef: CTFontRef;
    FTTFCreatePackage: Boolean;
    FTTFData: string;
    FTTFDataStream: TStringStream;
    {$ENDIF}
    FBase: string;
    FSize: Single;
    FUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    FCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    FCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    FMainInitializer: TTMSFNCGeneralPDFLibInitializer;
    FIsFixedWidth: Boolean;
  protected
    {$IFDEF IOS}
    procedure InternalInitializeCharWidths(AFontRef: CTFontRef);
    function CompressFontString(AValue: string): TStringStream;
    {$ENDIF}
  public
    constructor Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const AStyle: TFontStyles; const ASize: Single);
    destructor Destroy; override;
    function GetFontMetrics: TTMSFNCGeneralPDFLibFontMetrics;
    function GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    function GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    function GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetUnitsPerEM: Cardinal;
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
  SysUtils
  {$IFDEF IOS}
  ,MacApi.CoreFoundation, iOSApi.Foundation, iOSApi.CoreGraphics
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,zlib
  {$ENDIF}
  ;

{ TTMSFNCGeneralPDFLibFontInitializer }

{$IFDEF IOS}
function TTMSFNCGeneralPDFLibFontInitializer.CompressFontString(AValue: string): TStringStream;
var
  vDest: TStringStream;
  vSource: TStream;
  vCompressor: TCompressionStream;
begin
  vDest := TStringStream.Create('');
  try
  	vCompressor := TCompressionStream.Create(clMax, vDest);
    try
      vSource := TStringStream.Create(AValue);
      try
        vCompressor.CopyFrom(vSource, 0);
      finally
        vSource.Free;
      end;
    finally
      vCompressor.Free;
    end;

    vDest.Position := 0;
    Result := vDest;
  finally
  end;
end;
{$ENDIF}

procedure TTMSFNCGeneralPDFLibFontInitializer.CompressTTFData;
begin
  {$IFDEF IOS}
  FTTFDataStream := CompressFontString(FTTFData);
  {$ENDIF}
end;

constructor TTMSFNCGeneralPDFLibFontInitializer.Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const AStyle: TFontStyles; const ASize: Single);
 {$IFDEF IOS}
const
   im: CGAffineTransform = (
    a: 1;
    b: 0;
    c: 0.176326981;
    d: 1;
    tx: 0;
    ty: 0
  );

var
  nf: CTFontRef;
  m: PCGAffineTransform;
 {$ENDIF}
begin
  FMainInitializer := AMainInitializer;
  FBase := ABase;
  FSize := ASize;
  FUsedCharArray := TTMSFNCPDFGraphicsLibUsedFontCharArray.Create;
  {$IFDEF IOS}
  FTTFCreatePackage := True;
  FTTFData := '';

  m := nil;
  FFontRef := CTFontCreateWithName(CFSTR(FBase), FSize, nil);
  try
    if TFontStyle.fsItalic in AStyle then
    begin
      nf := CTFontCreateCopyWithSymbolicTraits(FFontRef, 0, nil, kCTFontItalicTrait, kCTFontItalicTrait);
      if Assigned(nf) then
      begin
        CFRelease(FFontRef);
        FFontRef := nf;
      end
      else
      begin
        m := @im;
        nf := CTFontCreateWithName(CFSTR(FBase), FSize, @im);
        if nf <> nil then
        begin
          CFRelease(FFontRef);
          FFontRef := nf;
        end;
      end;
    end;

    if TFontStyle.fsBold in AStyle then
    begin
      nf := CTFontCreateCopyWithSymbolicTraits(FFontRef, 0, m, kCTFontBoldTrait, kCTFontBoldTrait);
      if nf <> nil then
      begin
        CFRelease(FFontRef);
        FFontRef := nf;
      end;
    end;
  except
    CFRelease(FFontRef);
  end;

  {$ENDIF}
end;

destructor TTMSFNCGeneralPDFLibFontInitializer.Destroy;
begin
  if Assigned(FUsedCharArray) then
  begin
    FUsedCharArray.Free;
    FUsedCharArray := nil;
  end;
  {$IFDEF IOS}
  if Assigned(FFontRef) then
  begin
    CFRelease(FFontRef);
    FFontRef := nil;
  end;
  {$ENDIF}
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
{$IFDEF IOS}
var
  r: NSRect;
  ff: CFNumberRef;
  fmt: SInt16;
{$ENDIF}
begin
  Result.Ascent := 0;
  Result.CapHeight := 0;
  Result.Descent := 0;
  Result.FontBox := Bounds(0, 0, 0, 0);
  Result.ItalicAngle := 0;
  Result.Fixed := False;
  Result.TrueType := False;
  {$IFDEF IOS}
  if Assigned(FMainInitializer) then
  begin
    Result.CapHeight := Round(CTFontGetCapHeight(FFontRef));
    Result.Ascent := Round(CTFontGetAscent(FFontRef));
    Result.Descent := Round(CTFontGetDescent(FFontRef));
    r := CTFontGetBoundingBox(FFontRef);
    Result.FontBox.Left := Round(r.origin.x);
    Result.FontBox.Bottom := Round(r.origin.y);
    Result.FontBox.Right := Round(r.origin.x + r.size.width);
    Result.FontBox.Top := Round(r.origin.y + r.size.height);
    Result.ItalicAngle := Round(CTFontGetSlantAngle(FFontRef));

    ff := CTFontCopyAttribute(FFontRef, kCTFontFormatAttribute);
    if Assigned(ff) then
    begin
      CFNumberGetValue(ff, kCFNumberSInt16Type, @fmt);
      CFRelease(ff);
      if fmt = kCTFontFormatTrueType then
        Result.TrueType := True;
    end;

    ff := CTFontCopyAttribute(FFontRef, kCTFontFixedAdvanceAttribute);
    if Assigned(ff) then
    begin
      CFRelease(ff);
      Result.Fixed := True;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := nil;
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeCharWidths;
begin
  {$IFDEF IOS}
  if Assigned(FMainInitializer) then
    InternalInitializeCharWidths(FFontRef);
  {$ENDIF}
end;

{$IFDEF IOS}
procedure TTMSFNCGeneralPDFLibFontInitializer.InternalInitializeCharWidths(AFontRef: CTFontRef);
type
  TTMSFNCGeneralPDFLibFontCmapHead = packed record
    v: longint;
    fr: longint;
    csa: cardinal;
    mn: cardinal;
    fl: word;
    uem: word;
    cd: Int64;
    md: Int64;
    xmin: SmallInt;
    ymin: SmallInt;
    xmax: SmallInt;
    ymax: SmallInt;
    ms: word;
    lr: word;
    fd: SmallInt;
    ilf: SmallInt;
    gdf: SmallInt;
  end;

  TTMSFNCGeneralPDFLibFontCmapArray = packed array[byte] of packed record
    pID: word;
    psID: word;
    o: Cardinal;
  end;

  TTMSFNCGeneralPDFLibFontCmapHeader = packed record
    v: word;
    n: word;
  end;

  TTMSFNCGeneralPDFLibFontCmapFormat4 = packed record
    fmt: word;
    l: word;
    lng: word;
    sgX2: word;
    shrange: word;
    selr: word;
    rng: word;
  end;

  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[Byte] of SmallInt;

  function GetBit(const Bits; aIndex: Integer): Boolean;
  begin
    Result := PIntegerArray(@Bits)^[aIndex shr 5] and (1 shl (aIndex and 31)) <> 0;
  end;

var
  fd, hh: pointer;
  i, n, c, ndx: Integer;
  off: Integer;
  gidx: integer;
  id, gi: Integer;
  wl, nol: word;
  fdcm, fdcmr, fdcmrbk, fdhd, fdhdr, fdht, fdhtr, fdhh, fdhhr: TWordDynArray;
  st: ^TTMSFNCGeneralPDFLibFontCmapArray absolute fdcmr;
  h: ^TTMSFNCGeneralPDFLibFontCmapHeader;
  ft: ^TTMSFNCGeneralPDFLibFontCmapFormat4;
  ec, sc: PWordArray;
  ida: PSmallIntArray;
  idr: PWordArray;
  gia: PWordArray;
  hd: ^TTMSFNCGeneralPDFLibFontCmapHead;
  hdp: Pointer;
  uem: Cardinal;

  function SwapVal(AValue: NativeInt): NativeInt;
  begin
    Result := (AValue and $FF) shl 8 + ((AValue and $FF00) shr 8);
  end;

begin
  fd := CTFontCopyTable(AFontRef, kCTFontTableCmap, kCTFontTableOptionNoOptions);
  if Assigned(fd) then
  begin
    SetLength(fdcm, CFDataGetLength(fd));
    CFDataGetBytes(fd, CFRangeMake(0, CFDataGetLength(fd)), @(fdcm[0]));
    CFRelease(fd);

    SetLength(fdcmr, Length(fdcm));
    for I := 0 to Length(fdcm) - 1 do
      fdcmr[I] := SwapVal(fdcm[I]);

    fdcmrbk := fdcmr;
    h := @fdcmr[0];
    Inc(NativeInt(fdcmr), SizeOf(TTMSFNCGeneralPDFLibFontCmapHeader));
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

    if (off = 0) or (off and 1 <> 0) then
      Exit;

    i := LongRec(off).Lo;
    LongRec(off).Lo := LongRec(off).Hi;
    LongRec(off).Hi := i;

    if off > Length(fdcmrbk) * 2 then
      Exit;

    ft := Pointer(Integer(fdcmrbk) + off);

    if ft^.fmt <> 4 then
      Exit;

    ec := pointer(Integer(@ft^.fmt) + SizeOf(TTMSFNCGeneralPDFLibFontCmapFormat4));
    sc := pointer(Integer(ec) + ft^.sgX2 + 2);
    ida := pointer(Integer(sc) + ft^.sgX2);
    idr := Pointer(Integer(ida) + ft^.sgX2);
    gia := Pointer(Integer(idr) + ft^.sgX2);

    hdp := CTFontCopyTable(AFontRef, kCTFontTableHead, kCTFontTableOptionNoOptions);
    if Assigned(hdp) then
    begin
      SetLength(fdhd, CFDataGetLength(hdp));
      CFDataGetBytes(hdp, CFRangeMake(0, CFDataGetLength(hdp)), @(fdhd[0]));
      CFRelease(hdp);

      SetLength(fdhdr, Length(fdhd));
      for I := 0 to Length(fdhd) - 1 do
        fdhdr[I] := SwapVal(fdhd[I]);

      hd := @fdhdr[0];

      fd := CTFontCopyTable(AFontRef, kCTFontTableHmtx, kCTFontTableOptionNoOptions);
      if Assigned(fd) then
      begin
        SetLength(fdht, CFDataGetLength(fd));
        CFDataGetBytes(fd, CFRangeMake(0, CFDataGetLength(fd)), @(fdht[0]));
        CFRelease(fd);

        SetLength(fdhtr, Length(fdht));
        for I := 0 to Length(fdht) - 1 do
          fdhtr[I] := SwapVal(fdht[I]);

        hh := CTFontCopyTable(AFontRef, kCTFontTableHhea, kCTFontTableOptionNoOptions);
        if Assigned(hh) then
        begin
          SetLength(fdhh, CFDataGetLength(hh));
          CFDataGetBytes(hh, CFRangeMake(0, CFDataGetLength(hh)), @(fdhh[0]));
          CFRelease(hh);

          SetLength(fdhhr, Length(fdhh));
          for I := 0 to Length(fdhh) - 1 do
            fdhhr[I] := SwapVal(fdhh[I]);

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
            wl := (Cardinal(fdhtr[0]) * 1000) div uem;
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
                    FCharWidths[i].w := (cardinal(fdhtr[FCharWidths[i].g * 2]) * 1000) div uem
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

{$ENDIF}

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeFontFile;
{$IFDEF IOS}
const
  AllTables: array[0..18] of string = ('head', 'hhea', 'maxp', 'os/2', 'hmtx', 'ltsh', 'vdmx', 'hdmx', 'cmap', 'fpgm', 'prep', 'cvt ', 'loca', 'glyf', 'kern', 'name', 'post', 'gasp', 'pclt');

function GetTableId(const ATableName: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(ATableName) <> 4 then
    Exit;

  for i := 1 to Length(ATableName) do
    Result := Result or (Byte(ATableName[i]) shl ($18 - ((i - 1) * 8)));
end;

var
  fd: CFDataRef;
  I: Integer;
  b: String;
{$ENDIF}
begin
  {$IFDEF IOS}
  if FTTFCreatePackage then
  begin
    for I := 0 to Length(AllTables) - 1 do
    begin
      fd := CTFontCopyTable(FFontRef, CTFontTableTag(GetTableId(AllTables[I])), kCTFontTableOptionNoOptions);
      if Assigned(fd) then
      begin
        SetLength(b, CFDataGetLength(fd));
        CFDataGetBytes(fd, CFRangeMake(0, CFDataGetLength(fd)), @(b[1]));
        CFRelease(fd);
        FTTFData := FTTFData + b;
      end;
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressed: TStringStream;
begin
  {$IFDEF IOS}
  Result := FTTFDataStream;
  {$ENDIF}
  {$IFNDEF IOS}
  Result := nil;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressedLength: Int64;
begin
  Result := 0;
  {$IFDEF IOS}
  if Assigned(FTTFDataStream) then
    Result := FTTFDataStream.Size;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataLength: Integer;
begin
  {$IFDEF IOS}
  Result := Length(FTTFData);
  {$ENDIF}
  {$IFNDEF IOS}
  Result := 0;
  {$ENDIF}
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetUnitsPerEM: Cardinal;
begin
  Result := 0;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := FUsedCharArray;
end;

{ TTMSFNCGeneralPDFLibInitializer }

constructor TTMSFNCGeneralPDFLibInitializer.Create;
begin
end;

destructor TTMSFNCGeneralPDFLibInitializer.Destroy;
begin
  inherited;
end;

procedure TTMSFNCGeneralPDFLibInitializer.InitializeFontFallBackList(
  AList: TStrings);
begin
  AList.Add('Tahoma');
  AList.Add('Yu Gothic');
  AList.Add('Arial Unicode MS');
end;

end.

