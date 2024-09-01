unit acAlphaImageList;
{$I sDefs.inc}
//{$DEFINE ACDEBUG}

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, CommCtrl, ImgList,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2}UITypes, {$ENDIF}
  sConst, acntUtils;


type
  TsImageFormat = (ifPNG, ifICO, ifBMP32);
{$IFNDEF NOTFORHELP}
  TsAlphaImageList = class;
  TsImgListItems = class;


  TsImgListItem = class(TCollectionItem)
  private
    FText,
    FImageName: string;
    FImageFormat: TsImageFormat;
    FPixelFormat: TPixelFormat;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
  public
    OrigWidth,
    OrigHeight: integer;
    CacheBmp: TBitmap;
    ImgData: TMemoryStream;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property ImageFormat: TsImageFormat read FImageFormat write FImageFormat;
    property ImageName: string read FImageName write FImageName stored True;
    property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat default pf32bit;
    property Text: string read FText write FText;
  end;


  TsImgListItems = class(TCollection)
  protected
    FOwner: TsAlphaImageList;
    function  GetItem(Index: Integer): TsImgListItem;
    procedure SetItem(Index: Integer; Value: TsImgListItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TsAlphaImageList);
    destructor Destroy; override;
    property Items[Index: Integer]: TsImgListItem read GetItem write SetItem; default;
  end;
{$ENDIF}


  TacImageList = class(TImageList)
  protected
{$IFNDEF NOTFORHELP}
  {$IFNDEF ACDEBUG}
    ForeColor: TColor;
  {$ELSE}
    FForeColor: TColor;
    procedure SetForeColor(const Value: TColor);
  {$ENDIF}
  {$IFDEF DELPHI7UP}
    procedure ReadData (Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  {$ENDIF}
    procedure AddAsIcon(Bmp: TBitmap; Ndx: integer);
    procedure ClearCache(ImgIndex: integer = -1); virtual; abstract;
  public
    PixelsPerInch: integer;
    IgnoreTransparency: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFNDEF DELPHI_XE8}
    function Count: integer; virtual; abstract;
  {$ENDIF}
{$ENDIF} // NOTFORHELP
    function CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap; virtual; abstract;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean; virtual; abstract;
  {$IFDEF ACDEBUG}
    property ForeColor: TColor read FForeColor write SetForeColor;
  {$ENDIF}
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsAlphaImageList = class(TacImageList)
{$IFNDEF NOTFORHELP}
  private
    FLoaded,
    FUseCache,
    AcChanging,
    FAllowScale,
    ItemsUpdating,
    StdListIsGenerated: boolean;

    FItems: TsImgListItems;
    FBkColor: TColor;
    function GetBkColor: TColor;
    procedure SetItems   (const Value: TsImgListItems);
    procedure SetBkColor (const Value: TColor);
    procedure SetUseCache(const Value: boolean);
    function GetDimension(const Index: Integer): integer;
    procedure SetDimension(const Index, Value: integer);
  protected
    FSavedScale,
    SavedHeight,
    SavedWidth: integer;
    procedure SetNewScale(Value: Integer);

    procedure CreateImgList;
    function CanScale: boolean;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); {$IFNDEF FPC}override;{$ENDIF}
    procedure KillImgList;
    function IsDuplicated: boolean;
    procedure ItemsClear;
    procedure ClearCache(ImgIndex: integer = -1); override;
  public
    StdMode: boolean;
    DoubleData: boolean;
    procedure AcBeginUpdate;
    procedure AcEndUpdate(DoChange: boolean = True);
    procedure Change; {$IFNDEF FPC}override;{$ENDIF}
    procedure Delete(Index: Integer); reintroduce;
    procedure Clear; reintroduce;
    procedure Replace(AIndex: integer; ABmp32: TBitmap); reintroduce;
    function Add(Image, Mask: TBitmap): Integer; reintroduce;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyImages(const ImgList: TsAlphaImageList);
  {$IFNDEF DELPHI_XE8}
    function Count: integer; override;
  {$ELSE}
    function GetCount: Integer; override;
  {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFromStd;
    procedure GenerateStdList;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean; override;
    function CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap; override;
    function AddImage(Value: TCustomImageList; Index: Integer): Integer; //reintroduce;
    procedure Loaded; override;
    procedure LoadFromFile(const FileName: acString);
    procedure LoadFromPngStream(const Stream: TStream);
    function TryLoadFromFile(const FileName: acString): boolean;
    function TryLoadFromPngStream(Stream: TStream): Boolean;
    procedure MoveItem(CurIndex, NewIndex: integer);
    procedure SetNewDimensions(Value: HImageList);
    property AllowScale: boolean read FAllowScale write FAllowScale default True;
  published
    property Height index 0 read GetDimension write SetDimension;
    property Width  index 1 read GetDimension write SetDimension;

    property BkColor: TColor read GetBkColor write SetBkColor default clNone;
    property Items: TsImgListItems read FItems write SetItems;
    property UseCache: boolean read FUseCache write SetUseCache default True;
{$ENDIF} // NOTFORHELP
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsVirtualImageList = class(TCustomImageList)
{$IFNDEF NOTFORHELP}
  private
    FWidth,
    FHeight: integer;

    FUseCache,
    AcChanging,
    FStdHandleUsed,
    StdListIsGenerated: boolean;

    CachedImages: array of TBitmap;
    FImageChangeLink: TChangeLink;
    FAlphaImageList: TacImageList;
    FForeColor: TColor;
    procedure SetAlphaImageList(const Value: TacImageList);
    procedure SetUseCache      (const Value: boolean);
    procedure SetInteger(const Index, Value: integer);
    procedure SetForeColor(const Value: TColor);
    procedure SetStdHandleUsed(const Value: boolean);
  protected
    State: integer;
  {$IFDEF DELPHI7UP}
    procedure ReadData (Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  {$ENDIF}
    function CanUseCache: boolean;
    procedure CreateImgList;
    procedure KillImgList;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); {$IFNDEF FPC}override;{$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ImageListChange(Sender: TObject);
    procedure AddAsIcon(Bmp: TBitmap; Ndx: integer);
    property ForeColor: TColor read FForeColor write SetForeColor;
  public
    PixelsPerInch: integer;
    CurrentScale: integer;
    function ScaleValue: real;
    procedure AcBeginUpdate;
    procedure AcEndUpdate(DoChange: boolean = True);
    procedure Change; {$IFNDEF FPC}override;{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFNDEF DELPHI_XE8}
    function Count: integer;
  {$ELSE}
    function GetCount: Integer; override;
  {$ENDIF}
    procedure GenerateStdList;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
    function CreateBitmap32(Index: Integer): TBitmap;
    procedure ClearItems(ImgIndex: integer = -1);
    procedure Loaded; override;
    procedure RenderCacheNow(ItemIndex: integer = -1);
    procedure UpdateList(IgnoreGenerated: boolean = True);
    procedure SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
  published
    property Height: integer Index 0 read FHeight write SetInteger default 16;
    property Width:  integer Index 1 read FWidth  write SetInteger default 16;
{$ENDIF} // NOTFORHELP
    property AlphaImageList: TacImageList read FAlphaImageList write SetAlphaImageList;
    property StdHandleUsed: boolean read FStdHandleUsed write SetStdHandleUsed default True;
    property UseCache:      boolean read FUseCache      write SetUseCache      default True;
  end;


{$IFNDEF NOTFORHELP}
  TsCharImageList = class;
  TacBlendingMode = (bmInactive, bmAlways);

  TacCharListItem = class(TCollectionItem)
  private
    FAngle,
    FOffsetY,
    ActBlend,
    FAddedSize,
    CachedState,
    FOrientation,
    FAddedOffsetX,
    FAddedOffsetY: integer;

    FFontName,
    FImageName,
    FAddedFontName: string;

    FColor,
    FAddedColor,
    FSecondColor: TColor;

    FChar,
    FAddedChar: Word;

    FPitch: TFontPitch;
    FStyle: TFontStyles;
    FScalingFactor: real;
    FDrawContour: boolean;
    FCharset: TFontCharset;
    FAddedCharset: TFontCharset;
    procedure SetPitch(const Value: TFontPitch);
    procedure SetStyle(const Value: TFontStyles);
    // If cache image exists then GetImage returns this cache image
    // if cache is not used then result of GetImage should be destroyed later
    function GetImage(CharHeight: integer = 0): TBitmap;
    function ImgList: TsCharImageList;
    procedure SetScalingFactor(const Value: real);
    function NotDefScaling: boolean;
    function NotDefFont: boolean;
    function NotEmptyAdded: boolean;
    procedure SetAngle(const Value: integer);
    procedure SetDrawContour(const Value: boolean);

    procedure SetInteger(const Index, Value: integer);
    procedure SetWord(const Index: Integer; const Value: Word);
    procedure SetAddedFontName(const Value: string);
    procedure SetColor(const Index: Integer; const Value: TColor);
    procedure SetFontName(const Value: string);
    procedure SetCharset(const Index: Integer; const Value: TFontCharset);
  public
    CacheBmp: TBitmap;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    function ActualWidth(UseAngle: boolean): integer;
    destructor Destroy; override;
    procedure Invalidate;
  published
    property Angle: integer read FAngle write SetAngle default 0;
    property Pitch: TFontPitch read FPitch write SetPitch default fpDefault;
    property Style: TFontStyles read FStyle write SetStyle default [];
    property ScalingFactor: real read FScalingFactor write SetScalingFactor stored NotDefScaling;
    property DrawContour: boolean read FDrawContour write SetDrawContour default False;
    property ImageName: string read FImageName write FImageName stored True;

    property Orientation:  integer index 0 read FOrientation  write SetInteger default 0;
    property OffsetY:      integer index 1 read FOffsetY      write SetInteger default 0;

    property AddedOffsetX: integer index 2 read FAddedOffsetX write SetInteger default 0;
    property AddedOffsetY: integer index 3 read FAddedOffsetY write SetInteger default 0;
    property AddedSize:    integer index 4 read FAddedSize    write SetInteger default 50;

    property Char:      Word index 0 read FChar      write SetWord default 0;
    property AddedChar: Word index 1 read FAddedChar write SetWord default 0;

    property FontName:      string read FFontName      write SetFontName stored NotDefFont;
    property AddedFontName: string read FAddedFontName write SetAddedFontName stored NotEmptyAdded;

    property Color:       TColor index 0 read FColor       write SetColor default clNone;
    property SecondColor: TColor index 1 read FSecondColor write SetColor default clNone;
    property AddedColor:  TColor index 2 read FAddedColor  write SetColor default clNone;

    property Charset:      TFontCharset index 0 read FCharset      write SetCharset default DEFAULT_CHARSET;
    property AddedCharset: TFontCharset index 1 read FAddedCharset write SetCharset default DEFAULT_CHARSET;
  end;


  TacCharListItems = class(TCollection)
  protected
    FOwner: TsCharImageList;
    function  GetItem(Index: Integer): TacCharListItem;
    procedure SetItem(Index: Integer; Value: TacCharListItem);
    function GetOwner: TPersistent; override;
    procedure CheckItems;
  public
    constructor Create(AOwner: TsCharImageList);
    destructor Destroy; override;
    property Items[Index: Integer]: TacCharListItem read GetItem write SetItem; default;
  end;


  TacCharItemData = record
    Angle,
    OffsetY,
    Orientation: integer;

    Color,
    SecondColor: TColor;

    Char:        Word;
    Handle:      hFont;
    FontName:    string;
    DrawContour: boolean;
    Pitch:       TFontPitch;
    Style:       TFontStyles;
    Charset:     TFontCharset;
    ScalingFactor: real;

    AddedOffsetX,
    AddedOffsetY,
    AddedSize:    integer;
    AddedChar:    word;
    AddedColor:   TColor;
    AddedCharset: TFontCharset;
  end;


  TacEmbeddedFont = class(TCollectionItem)
  protected
    FFileName,
    FFontName: acString;
    Handle: THandle;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
  public
    FontData: TMemoryStream;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    procedure UnLoadFont;
    procedure LoadFont;
  published
    property FileName: acString read FFileName write FFileName;
    property FontName: acString read FFontName write FFontName;
  end;


  TacEmbeddedFonts = class(TCollection)
  protected
    FOwner: TsCharImageList;
    function GetItem(Index: Integer): TacEmbeddedFont;
    procedure SetItem(Index: Integer; const Value: TacEmbeddedFont);
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    constructor Create(AOwner: TsCharImageList);
    property Items[Index: Integer]: TacEmbeddedFont read GetItem write SetItem; default;
  end;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsCharImageList = class(TacImageList)
{$IFNDEF NOTFORHELP}
  private
    AcChanging,
    FAllowScale,
    StdListIsGenerated: boolean;

    ForeColor2,
    FBkColor: TColor;
    FItems: TacCharListItems;
    FEmbeddedFonts: TacEmbeddedFonts;
    FBlendingMode: TacBlendingMode;
    FBlendValue: byte;
    function GetBkColor: TColor;
    procedure SetItems   (const Value: TacCharListItems);
    procedure SetBkColor (const Value: TColor);
    function GetDimension(const Index: Integer): integer;
    procedure SetDimension(const Index, Value: integer);
    procedure SetEmbeddedFonts(const Value: TacEmbeddedFonts);
    procedure SetBlendingMode(const Value: TacBlendingMode);
    procedure SetBlendValue(const Value: byte);
  protected
    FLoaded: boolean;
    State,
    FSavedScale,
    SavedHeight,
    SavedWidth: integer;
    procedure SetNewScale(Value: Integer);

    procedure CreateImgList;
    function CanScale: boolean;
    function CanUseCache: boolean;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure PrepareBmp(Bmp: TBitmap; Index: integer; const aRect: TRect; CharHeight: integer; Offset: integer);

    procedure UnLoadAllFonts;
    procedure LoadAllFonts;
  public
    procedure AcBeginUpdate;
    procedure AcEndUpdate(DoChange: boolean = True);
    procedure UpdateStd(i: integer);
    function AddItem(const ItemData: TacCharItemData): integer;
    procedure SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
    procedure KillImgList;
    procedure Change; override;
    procedure Clear; reintroduce;
    procedure ClearCache(ImgIndex: integer = -1); override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyImages(const ImgList: TsCharImageList);
    function FontIndex(const aFontName: acString): integer;
{$IFNDEF DELPHI_XE8}
    function Count: integer; override;
{$ELSE}
    function GetCount: Integer; override;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateStdList(BaseColor: TColor = clNone);
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean; override;
    function CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap; override;
    function CreateBitmap32Color(Index: Integer; aWidth, aHeight: integer; CharColor: TColor; GlyphHeight: integer = 0): TBitmap;
    function ItemIsCached(Item: TacCharListItem; RequiredWidth, RequiredHeight, CharHeight: integer; RequiredColor: TColor): boolean;
    procedure Loaded; override;
    procedure SetNewDimensions(Value: HImageList);
    function AddEmbeddedFont(const FileName, FontName: acString): boolean;
    property AllowScale: boolean read FAllowScale write FAllowScale default True;
  published
    property Height index 0 read GetDimension write SetDimension default 24;
    property Width  index 1 read GetDimension write SetDimension default 26;

    property EmbeddedFonts: TacEmbeddedFonts read FEmbeddedFonts write SetEmbeddedFonts;
    property Items: TacCharListItems read FItems write SetItems;
    property BkColor: TColor read GetBkColor write SetBkColor default clNone;
{$ENDIF} // NOTFORHELP
    property BlendValue: byte read FBlendValue write SetBlendValue default MAXBYTE;
    property BlendingMode: TacBlendingMode read FBlendingMode write SetBlendingMode default bmInactive;
  end;


{$IFNDEF NOTFORHELP}
TIterImagesProc = procedure(ImgList: TCustomImageList; Data: Longint);

procedure IterateImageLists(CallBack: TIterImagesProc; Data: Longint);
function GetImageFormat(const FileName: acString; var ImageFormat: TsImageFormat): boolean; overload;
function GetImageFormat(const Stream: TStream; var ImageFormat: TsImageFormat): boolean; overload;
function DrawAlphaImgList(const ImgList: TCustomImageList; const DestBmp: TBitmap; const Left: integer; const Top: integer;
                          const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; const State: integer;
                          const NumGlyphs: integer; const Reflected: boolean; PPI: integer): TSize;
procedure DrawAlphaImgListDC(const ImgList: TCustomImageList; const DC: hdc; const Left: integer; const Top: integer;
                             const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; const State: integer;
                             const NumGlyphs: integer; const Reflected: boolean; PPI: integer = 96);

function GetDefScaling(CharSet: TFontCharset; const FontName: string): real;
{$ENDIF}
function AddImageFromRes(aInstance: LongWord; ImageList: TsAlphaimageList; const ResName: String; aImageFormat: TsImageFormat): Boolean; // Png must be compiled in resource as RcData
function SetImagesPPI(ImgList: TCustomImageList; const PPI: integer): boolean;
procedure SetImagesState(ImgList: TCustomImageList; const AState: integer);
procedure ClearImgListCache(ImgList: TCustomImageList; ImgIndex: integer = -1);

procedure ScaleImageList(ImgList: TCustomImageList; PPI: Longint);
procedure SetBaseColor(ImgList: TCustomImageList; BaseColor: integer);

{$IFNDEF NOTFORHELP}
var
  acIgnoreBlendColor: boolean = False;
{$ENDIF}


implementation

uses
  math, ShellAPI, Dialogs, ActiveX, Forms,
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  sAlphaGraph, sThirdParty, sSkinManager, sGraphUtils, acgpUtils, sVCLUtils;


var
  acListsArray: TList = nil;


procedure IterateImageLists(CallBack: TIterImagesProc; Data: Longint);
var
  i: integer;
begin
  for i := 0 to acListsArray.Count - 1 do
    CallBack(TCustomImageList(acListsArray[i]), Data);
end;


function GetBpp: integer;
var
  ScreenDC: hdc;
begin
  ScreenDC := GetDC(0);
  try
    Result := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC)
  end;
end;


function HaveMagic(const Stream: TStream; const Magic: Pointer; const Size: integer): Boolean; overload;
var
  MagicBuf: array [0..7] of Byte;
  len: integer;
begin
  FillChar(MagicBuf, 8, #0);
  len := min(Size, SizeOf(MagicBuf));
  Result := (Stream.Size - Stream.Position) > len;
  if Result then begin
    Stream.ReadBuffer(MagicBuf, len);
    Result := CompareMem(@MagicBuf, Magic, len);
  end;
end;


function HaveMagic(const FileName: string; const Magic: Pointer; const Size: integer): Boolean; overload;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := HaveMagic(Stream, Magic, Size);
  finally
    Stream.Free;
  end;
end;


function GetImageFormat(const FileName: acString; var ImageFormat: TsImageFormat): boolean;
const
  IcoMagic: array [0..1] of Byte = (0, 0);
  BmpMagic: array [0..1] of Byte = (66, 77);
var
  s: string;
begin
  Result := False;
  // Check format
  if HaveMagic(FileName, @PNGMagic, 8) then begin // Png
    ImageFormat := ifPNG;
    Result := True;
  end
  else
    if HaveMagic(FileName, @IcoMagic, 2) then begin // Ico
      s := LowerCase(ExtractFileExt(FileName));
      System.Delete(s, 1, 1);
      if s = acIcoExt then begin
        ImageFormat := ifICO;
        Result := True;
      end;
    end
    else
      if HaveMagic(FileName, @BmpMagic, 2) then begin // Bmp32
        ImageFormat := ifBMP32;
        Result := True;
      end;
end;


function GetImageFormat(const Stream: TStream; var ImageFormat: TsImageFormat): boolean;
begin
  ImageFormat := ifPNG;
  Result := True;
end;


function DrawAlphaImgList(const ImgList: TCustomImageList; const DestBmp: TBitmap; const Left: integer; const Top: integer;
  const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; const State: integer; const NumGlyphs: integer; const Reflected: boolean; PPI: integer): TSize;
var
  Bmp: TBitmap;
  R1, R2: TRect;
  actBlend, actState: integer;
  vil: TsVirtualImageList;
  imgWidth, imgHeight, propWidth, propHeight, propW, w, Count: integer;
begin
  if (DestBmp.Width > 0) and ImgList.HandleAllocated and (ImageIndex >= 0) then begin
    // Get actual size of glyph
    imgWidth  := GetImageWidth (ImgList, ImageIndex, PPI, True);
    imgHeight := GetImageHeight(ImgList, ImageIndex, PPI, True);
    // Images width and height declared in properties
    propWidth := imgWidth;
    propHeight := imgHeight;
    Count := max(1, NumGlyphs);
    w := imgWidth div Count;
    propW := propWidth div Count;
    if State >= Count then
      actState := Count - 1
    else
      actState := State;

    // Get real rectangle for glyph
    R1.Left := Left + (propW - w) div 2;
    R1.Top  := Top  + (propHeight - imgHeight) div 2;
    R1 := Rect(R1.Left, R1.Top, R1.Left + w, R1.Top + ImgHeight);
    R2 := MkRect(w, ImgHeight);
    OffsetRect(R2, w * actState, 0);
    // Real size of glyph
    Result := MkSize(w, ImgHeight);
    // Fill bitmap by BG color
    if ImgList.BkColor <> clNone then
      FillDC(DestBmp.Canvas.Handle, R1, ColorToRGB(ImgList.BkColor));

    Bmp := nil;
    SetImagesPPI(ImgList, PPI);
    if ImgList is TacImageList then begin
      // Set color of char
      if ImgList is TsCharImageList then begin
        TsCharImageList(ImgList).SetCharColor(DestBmp.Canvas.Font.Color, ImageIndex, DestBmp.Canvas.Font.Size = 0);
        actBlend := Blend;
      end
      else begin
        TacImageList(ImgList).ForeColor := DestBmp.Canvas.Font.Color;
        actBlend := Blend;
      end;
      // Creating of temp bitmap with char
      if State > 0 then
        SetImagesState(ImgList, State);

      Bmp := TacImageList(ImgList).CreateBitmap32(ImageIndex, ImgWidth, ImgHeight);
      if State > 0 then
        SetImagesState(ImgList, 0);

      TacImageList(ImgList).ForeColor := clNone;

      if R2.Left < 0 then begin
        R1.Left := R1.Left + R2.Left;
        R2.Left := 0;
      end;
      if R2.Top < 0 then begin
        R1.Top := R1.Top + R2.Top;
        R2.Top := 0;
      end;
      R2.Right := min(Bmp.Width, R2.Right);
      R2.Bottom := min(Bmp.Height, R2.Bottom);
      // Painting of bitmap
      if Bmp <> nil then
        if TacImageList(ImgList).IgnoreTransparency then
          BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          CopyBmp32(R1, R2, DestBmp, Bmp, EmptyCI, False, GrayedColor, actBlend, Reflected);
    end
    else
      if ImgList is TsVirtualImageList then begin
        vil := TsVirtualImageList(ImgList);
        vil.SetCharColor(DestBmp.Canvas.Font.Color, ImageIndex, DestBmp.Canvas.Font.Size = 0);
        vil.State := State;
        actBlend := Blend;

        if vil.CanUseCache then begin
          if Length(vil.CachedImages) <= ImageIndex then
            SetLength(vil.CachedImages, vil.AlphaImageList.Count);

          if Length(vil.CachedImages) > ImageIndex then begin
            if (vil.CachedImages[ImageIndex] <> nil) and
                 ((vil.CachedImages[ImageIndex].Width <> imgWidth) or (vil.CachedImages[ImageIndex].Height <> imgHeight) or (vil.CachedImages[ImageIndex].Canvas.Font.Color <> vil.ForeColor)) then
              FreeAndNil(vil.CachedImages[ImageIndex]);

            if vil.CachedImages[ImageIndex] = nil then
              vil.CachedImages[ImageIndex] := vil.AlphaImageList.CreateBitmap32(ImageIndex, ImgWidth, ImgHeight, GetImageHeight(vil, -1, PPI));

            if vil.AlphaImageList.IgnoreTransparency then
              with vil.CachedImages[ImageIndex] do
                BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, Width, Height, vil.CachedImages[ImageIndex].Canvas.Handle, 0, 0, SRCCOPY)
            else
              CopyBmp32(R1, R2, DestBmp, vil.CachedImages[ImageIndex], EmptyCI, False, GrayedColor, Blend, Reflected);
          end;
        end
        else begin
          Bmp := CreateBmp32(ImgWidth, max(0, ImgHeight));
//          Bmp := CreateBmp32(w, max(0, ImgHeight));
          SetImagesState(vil.AlphaImageList, State);
          vil.GetBitmap32(ImageIndex, Bmp, GetImageHeight(ImgList, -1{ImageIndex}, PPI));
          SetImagesState(vil.AlphaImageList, 0);
          if Bmp <> nil then
            if vil.AlphaImageList.IgnoreTransparency then
              BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY)
            else
              CopyBmp32(R1, R2, DestBmp, Bmp, EmptyCI, False, GrayedColor, actBlend, Reflected);
        end;
        vil.State := 0;
        vil.ForeColor := clNone;
      end
      else begin
        Bmp := CreateBmp32(ImgWidth, ImgHeight);
        BitBlt(Bmp.Canvas.Handle, R2.Left, R2.Top, WidthOf(R2), HeightOf(R2), DestBmp.Canvas.Handle, R1.Left, R1.Top, SRCCOPY);
        Bmp.Canvas.Font.Color := DestBmp.Canvas.Font.Color;
        ImgList.Draw(Bmp.Canvas, 0, 0, ImageIndex, True);
        BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, WidthOf(R2), HeightOf(R2), Bmp.Canvas.Handle, R2.Left, R2.Top, SRCCOPY);
      end;

    SetImagesPPI(ImgList, 96);
    FreeAndNil(Bmp);
  end;
end;


procedure DrawAlphaImgListDC(const ImgList: TCustomImageList; const DC: hdc; const Left: integer; const Top: integer;
  const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; const State: integer; const NumGlyphs: integer; const Reflected: boolean; PPI: integer = 96);
var
  Bmp: TBitmap;
  vil: TsVirtualImageList;
  imgWidth, imgHeight: integer;
begin
  imgWidth  := GetImageWidth (ImgList, ImageIndex, PPI);
  imgHeight := GetImageHeight(ImgList, ImageIndex, PPI);
  Bmp := CreateBmp32(ImgWidth, ImgHeight);
  if ImgList is TsVirtualImageList then begin
    vil := TsVirtualImageList(ImgList);
    Bmp.Canvas.Font.Color := vil.ForeColor;
    if Reflected and (vil.AlphaImageList is TsCharImageList) then
      Bmp.Height := Bmp.Height + Round(GetImageHeight(ImgList, -1, PPI) * TsCharImageList(vil.AlphaImageList).Items[ImageIndex].ScalingFactor) div 2
    else
      Bmp.Height := Bmp.Height + GetImageHeight(ImgList, -1, PPI) div 2;

    if (vil.AlphaImageList is TsCharImageList) and (TsCharImageList(vil.AlphaImageList).Items[ImageIndex].Color <> clNone) then
      Bmp.Canvas.Font.Size := 0; // Def color is not allowed
  end
  else
    if ImgList is TsCharImageList then begin
      Bmp.Canvas.Font.Color := TsCharImageList(ImgList).ForeColor;
      if Reflected then
        Bmp.Height := Bmp.Height + Round(GetImageHeight(ImgList, -1, PPI) * TsCharImageList(ImgList).Items[ImageIndex].ScalingFactor) div 2;

      if TsCharImageList(ImgList).Items[ImageIndex].Color <> clNone then
        Bmp.Canvas.Font.Size := 0; // Def color is not allowed
    end
    else
      if Reflected then
        Bmp.Height := Bmp.Height + GetImageHeight(ImgList, -1, PPI) div 2;

  BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, DC, Left, Top, SRCCOPY);
  DrawAlphaImgList(ImgList, Bmp, 0, 0, ImageIndex, Blend, GrayedColor, State, NumGlyphs, Reflected, PPI);
  BitBlt(DC, Left, Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  FreeAndNil(Bmp);
end;


function GetDefScaling(CharSet: TFontCharset; const FontName: string): real;
const
  CharSize = 96;
var
  f: TFont;
  i: integer;
begin
  f := TFont.Create;
  try
    f.Charset := Charset;
    f.Name := FontName;
    f.Height := -CharSize;
    i := GetFontHeight(f.Handle, True);
    Result := i / CharSize;
  finally
    f.Free;
  end;
end;


function SetImagesPPI(ImgList: TCustomImageList; const PPI: integer): boolean;
begin
  Result := False;
  if ImgList is TsAlphaImageList then begin
    if TacImageList(ImgList).PixelsPerInch <> PPI then begin
      TacImageList(ImgList).PixelsPerInch := PPI;
      Result := True;
    end;
  end
  else
    if ImgList is TsCharImageList then begin
      if TsCharImageList(ImgList).AllowScale and (TsCharImageList(ImgList).PixelsPerInch <> PPI) then begin
        TsCharImageList(ImgList).PixelsPerInch := PPI;
        Result := True;
      end;
    end
    else
      if ImgList is TsVirtualImageList then
        if TsVirtualImageList(ImgList).PixelsPerInch <> PPI then begin
          TsVirtualImageList(ImgList).PixelsPerInch := PPI;
          Result := True;
        end;
end;


procedure SetImagesState(ImgList: TCustomImageList; const AState: integer);
begin
  if ImgList is TsCharImageList then
    TsCharImageList(ImgList).State := AState
  else
    if ImgList is TsVirtualImageList then
      TsVirtualImageList(ImgList).State := AState;
end;


procedure ClearImgListCache(ImgList: TCustomImageList; ImgIndex: integer = -1);
begin
  if ImgList is TacImageList then
    TacImageList(ImgList).ClearCache(ImgIndex)
  else
    if ImgList is TsVirtualImageList then
      TsVirtualImageList(ImgList).ClearItems(ImgIndex);
end;


procedure ScaleImageList(ImgList: TCustomImageList; PPI: Longint);
begin
  if SetImagesPPI(ImgList, PPI) then // If PPI is really changed
    if ImgList is TsCharImageList then
      if TsCharImageList(ImgList).AllowScale then begin
        TsCharImageList(ImgList).AcBeginUpdate;
        try
          TsCharImageList(ImgList).KillImgList;
          TImageList(ImgList).Width := GetImageWidth(ImgList, -1, TsCharImageList(ImgList).PixelsPerInch);
          TImageList(ImgList).Height := GetImageHeight(ImgList, -1, TsCharImageList(ImgList).PixelsPerInch);
          TsCharImageList(ImgList).GenerateStdList;
        finally
          TsCharImageList(ImgList).AcEndUpdate;
        end;
      end;
end;


procedure SetBaseColor(ImgList: TCustomImageList; BaseColor: integer);
begin
  if ImgList is TsCharImageList then begin
    TsCharImageList(ImgList).AcBeginUpdate;
    try
      TsCharImageList(ImgList).KillImgList;
      if DefaultManager <> nil then begin
        TsCharImageList(ImgList).PixelsPerInch := DefaultManager.Options.PixelsPerInch;
//        TImageList(ImgList).Width := GetImageWidth(ImgList, -1, DefaultManager.Options.PixelsPerInch);
//        TImageList(ImgList).Height := GetImageHeight(ImgList, -1, DefaultManager.Options.PixelsPerInch);
      end;
      TsCharImageList(ImgList).GenerateStdList(TColor(BaseColor));
      TsCharImageList(ImgList).PixelsPerInch := 96;
      TsCharImageList(ImgList).ForeColor := clNone;
    finally
      TsCharImageList(ImgList).AcEndUpdate;
    end;
  end;
end;


function AddImageFromRes(aInstance: LongWord; ImageList: TsAlphaimageList; const ResName: String; aImageFormat: TsImageFormat): Boolean;
var
  hIc: HICON;
  Ico: hIcon;
  Bmp: TBitmap;
  rs: TResourceStream;
begin
  Result := False;
  case aImageFormat of
    ifICO: begin
      hIc := LoadImage(aInstance, PChar(ResName), IMAGE_ICON, ImageList.Width, ImageList.Height, 0);
      if hIc <> 0 then
        try
          if ImageList_AddIcon(ImageList.Handle, hIc) <> -1 then
            Result := True;
        finally
          DestroyIcon(hIc);
        end;
    end;

    ifBMP32: begin
      Bmp := CreateBmp32;
      Bmp.LoadFromResourceName(aInstance, ResName);
      with TsImgListItem.Create(ImageList.Items) do begin
        ImageFormat := aImageFormat;
        Bmp.SaveToStream(ImgData);
      end;
      Ico := MakeIcon32(Bmp);
      Result := ImageList_AddIcon(ImageList.Handle, Ico) <> -1;
      DestroyIcon(Ico);
      FreeAndNil(Bmp);
    end

    else begin
      rs := TResourceStream.Create(aInstance, ResName, RT_RCDATA);
      rs.Seek(0, 0);
      with TsImgListItem.Create(ImageList.Items) do begin // Add to Items
        ImageFormat := ifPNG;
        ImgData.LoadFromStream(rs);
        rs.Position := 0;
      end;
      rs.Seek(0, 0);
      Bmp := CreateBmp32;
      LoadBmpFromPngStream(Bmp, rs);
      FreeAndNil(rs);
      Ico := MakeIcon32(Bmp);
      Result := ImageList_AddIcon(ImageList.Handle, Ico) <> -1;
      DestroyIcon(Ico);
      FreeAndNil(Bmp);
    end;
  end;
end;


procedure TsAlphaImageList.AfterConstruction;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FLoaded := True;
end;


function GetColor(Value: DWORD): TColor;
begin
  case Value of
    CLR_NONE:    Result := clNone;
    CLR_DEFAULT: Result := clDefault
    else         Result := TColor(Value);
  end;
end;


procedure TsAlphaImageList.Assign(Source: TPersistent);
var
  ImageList: TsAlphaImageList;
begin
  if Source = nil then
    KillImgList
  else
    if Source is TsAlphaImageList then begin
      AcBeginUpdate;
      inherited Clear;
      ImageList := TsAlphaImageList(Source);
      Masked := ImageList.Masked;
      ImageType := ImageList.ImageType;
      DrawingStyle := ImageList.DrawingStyle;
      ShareImages := ImageList.ShareImages;
      SetNewDimensions(ImageList.Handle);
      KillImgList;
      if not HandleAllocated then
        CreateImgList
      else
        ImageList_SetIconSize(Handle, Width, Height);

      BkColor := GetColor(ImageList_GetBkColor(ImageList.Handle));
      BlendColor := ImageList.BlendColor;
      CopyImages(ImageList);
      AcEndUpdate(False);
    end
    else
      inherited Assign(Source);
end;


procedure TsAlphaImageList.AssignTo(Dest: TPersistent);
var
  ImageList: TsAlphaImageList;
begin
  if Dest is TsAlphaImageList then begin
    ImageList := TsAlphaImageList(Dest);
    ImageList.AcBeginUpdate;
    ImageList.Masked := Masked;
    ImageList.ImageType := ImageType;
    ImageList.DrawingStyle := DrawingStyle;
    ImageList.ShareImages := ShareImages;
    ImageList.BlendColor := BlendColor;

    ImageList.Clear;
    ImageList.KillImgList;
    ImageList.SetNewDimensions(Self.Handle);
    if not ImageList.HandleAllocated then
      ImageList.CreateImgList
    else
      ImageList_SetIconSize(ImageList.Handle, ImageList.Width, ImageList.Height);

    ImageList.BkColor := GetColor(ImageList_GetBkColor(Self.Handle));
    ImageList.CopyImages(Self);
    ImageList.AcEndUpdate(False);
  end
  else
    inherited AssignTo(Dest);
end;


procedure TsAlphaImageList.CopyImages(const ImgList: TsAlphaImageList);
var
  i: integer;
  Ico: hIcon;
begin
  if HandleAllocated then begin
    ImageList_SetBkColor(ImgList.Handle, CLR_NONE);
    if IsDuplicated then begin
      Items.Clear;
      for i := 0 to ImgList.Items.Count - 1 do
        with TsImgListItem(Items.Add) do begin
          ImageFormat := ImgList.Items[i].ImageFormat;
          ImageName   := ImgList.Items[i].ImageName;
          PixelFormat := ImgList.Items[i].PixelFormat;
          Text        := ImgList.Items[i].Text;
          ImgData.LoadFromStream(ImgList.Items[i].ImgData);
          ImgList.Items[i].ImgData.Position := 0;
        end;

      GenerateStdList;
    end
    else begin
      inherited Clear;
      ImageList_SetBkColor(Handle, CLR_NONE);
      for i := 0 to ImgList.Count - 1 do begin
        Ico := ImageList_GetIcon(ImgList.Handle, i, ILD_TRANSPARENT);
        ImageList_AddIcon(Handle, Ico);
        DestroyIcon(Ico);
      end;
    end;
  end;
end;


constructor TsAlphaImageList.Create(AOwner: TComponent);
begin
  FLoaded := False;
  FSavedScale := 0;
  inherited;
  FItems := TsImgListItems.Create(Self);
  FAllowScale := True;
  StdMode := False;
  FBkColor := clNone;
  FUseCache := True;
  ItemsUpdating := False;
  DoubleData := True;
end;


procedure TsAlphaImageList.CreateImgList;
begin
{$IFNDEF FPC}
  if (SavedWidth = 0) and FLoaded and not (csLoading in ComponentState) then begin
    SavedWidth := Width;
    SavedHeight := Height;
  end;
  Handle := ImageList_Create(SavedWidth, SavedHeight, ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);
{$ELSE}
  ReferenceNeeded;
{$ENDIF}
end;


procedure TsAlphaImageList.Delete(Index: Integer);
begin
  if Items.Count > 0 then begin
    Items.Delete(Index);
    Change;
  end;
end;


destructor TsAlphaImageList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;


procedure TsAlphaImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Ico: hIcon;
  w, h: integer;
  TmpBmp, Bmp: TBitmap;
begin
  if HandleAllocated and (Index >= 0) then
    if IsValidIndex(Index, Items.Count) then
      case Items[Index].ImageFormat of
        ifPng, ifBmp32: begin
          w := Width * PixelsPerInch div 96;
          h := Height * PixelsPerInch div 96;
          Bmp := CreateBitmap32(Index, w, h);
          if Bmp <> nil then
            try
              Bmp.Canvas.Lock;
              TmpBmp := CreateBmp32(w, h);
              TmpBmp.Canvas.Lock;
              try
                BitBlt(TmpBmp.Canvas.Handle, 0, 0, w, h, Canvas.Handle, X, Y, SRCCOPY);
                CopyBmp32(MkRect(w, h), MkRect(w, h), TmpBmp, Bmp, MakeCacheInfo(TmpBmp), False, clNone, 0, False);
                BitBlt(Canvas.Handle, X, Y, w, h, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                TmpBmp.Canvas.Unlock;
                FreeAndNil(TmpBmp);
              end;
              Bmp.Canvas.Unlock;
            finally
              FreeAndNil(Bmp);
            end;
        end;

        ifIco: begin
          ImageList_SetBkColor(Handle, CLR_NONE);
          Ico := ImageList_GetIcon(Handle, Index, ILD_TRANSPARENT);
          if Ico > 0 then begin
            DrawIconEx(Canvas.Handle, X, Y, Ico, Width, Height, 0, 0, DI_NORMAL);
            DestroyIcon(Ico);
          end;
        end;
      end
    else begin
      ImageList_SetBkColor(Handle, CLR_NONE);
      Ico := ImageList_GetIcon(Handle, Index, ILD_TRANSPARENT);
      if Ico > 0 then begin
        DrawIconEx(Canvas.Handle, X, Y, Ico, Width, Height, 0, 0, DI_NORMAL);
        DestroyIcon(Ico);
      end;
    end;
end;


procedure TsAlphaImageList.GenerateStdList;
var
  i: integer;
  Bmp: TBitmap;
  Icon: TIcon;
begin
  if not StdMode then begin
    // Check if default size was not initialized yet
    if (SavedHeight = 0) and (inherited Width = 16) and (inherited Height = 16) and not AcChanging then begin
      AcChanging := True;
      KillImgList;
      Width  := inherited Width;
      Height := inherited Height;
    end;

    if SavedHeight <> 0 then begin
      if not HandleAllocated then
        CreateImgList;

      if HandleAllocated then begin
        AcChanging := True;
        inherited Clear;
        for i := 0 to Items.Count - 1 do
          case Items[i].ImageFormat of
            ifPNG: begin
              Items[i].ImgData.Seek(0, 0);
              Bmp := CreateBmp32;
              LoadBmpFromPngStream(Bmp, Items[i].ImgData);
              Items[i].OrigWidth := Bmp.Width;
              Items[i].OrigHeight := Bmp.Height;
              AddAsIcon(Bmp, -1);
              FreeAndNil(Bmp);
            end;

            ifBMP32: begin
              Bmp := CreateBmp32;
              Items[i].ImgData.Seek(0, 0);
              Bmp.LoadFromStream(Items[i].ImgData);
              Items[i].ImgData.Position := 0;
              Items[i].OrigWidth := Bmp.Width;
              Items[i].OrigHeight := Bmp.Height;
              Bmp.PixelFormat := pf32bit;
              AddAsIcon(Bmp, -1);
              FreeAndNil(Bmp);
            end;

            ifICO: begin
              Icon := TIcon.Create;
              Items[i].ImgData.Seek(0, 0);
              Icon.LoadFromStream(Items[i].ImgData);
              Items[i].ImgData.Position := 0;
              Items[i].OrigWidth := Icon.Width;
              Items[i].OrigHeight := Icon.Height;
              ImageList_AddIcon(Handle, Icon.Handle);
              FreeAndNil(Icon);
            end;
          end;

        if Items.Count > 0 then begin
          StdListIsGenerated := True;
          if not IsDuplicated then begin
            ItemsUpdating := True;
            Items.Clear;
            ItemsUpdating := False;
          end;
        end;
        AcChanging := False;
      end;
    end;
  end;
end;


function TsAlphaImageList.GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
var
  Ico: hIcon;
  iInfo: TIconInfo;
  TmpBmp, Bmp: TBitmap;
begin
  Result := False;
  if HandleAllocated and (Image <> nil) and IsValidIndex(Index, Count) then
    if IsDuplicated and (Index < Items.Count) and (Items[Index].ImgData.Size > 0) then // Using of original image if exists
      case Items[Index].ImageFormat of
        ifPNG: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = Image.Width) and (Items[Index].CacheBmp.Height = Image.Height) then begin
              Image.Assign(Items[Index].CacheBmp);
              Result := True;
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          Bmp := CreateBmp32;
          Bmp.Transparent := Image.Transparent;
          Items[Index].ImgData.Seek(0, 0);
          LoadBmpFromPngStream(Bmp, Items[Index].ImgData);
          if (Items[Index].OrigWidth <> Image.Width) or (Items[Index].OrigHeight <> Image.Height) then begin // If must be scaled
            Image.PixelFormat := pf32bit;
            Stretch(Bmp, Image, Image.Width, Image.Height, ftMitchell);
          end
          else
            Image.Assign(Bmp);

          FreeAndNil(Bmp);
          if Image.PixelFormat <> pf32bit then begin
            Image.PixelFormat := pf32bit;
            CheckEmptyAChannel(Image);
          end;
          // Make Cache
          if FUseCache then
            if (Image.Width = Width) and (Image.Height = Height) then begin
              Items[Index].CacheBmp := CreateBmp32(Image.Width, Image.Height);
              Items[Index].CacheBmp.Assign(Image);
            end;

          Result := True;
        end;

        ifBMP32: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = Image.Width) and (Items[Index].CacheBmp.Height = Image.Height) then begin
              Image.Assign(Items[Index].CacheBmp);
              Result := True;
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          Items[Index].ImgData.Seek(0, 0);
          if (Items[Index].OrigWidth <> Image.Width) or (Items[Index].OrigHeight <> Image.Height) then begin // If must be scaled
            Bmp := CreateBmp32;
            Bmp.Transparent := Image.Transparent;
            Bmp.LoadFromStream(Items[Index].ImgData);
            Image.PixelFormat := pf32bit;
            Stretch(Bmp, Image, Image.Width, Image.Height, ftMitchell);
            FreeAndNil(Bmp);
          end
          else
            Image.LoadFromStream(Items[Index].ImgData);

          Items[Index].ImgData.Position := 0;
          if Image.PixelFormat <> pf32bit then begin
            Image.PixelFormat := pf32bit;
            CheckEmptyAChannel(Image);
          end;
          // Make Cache
          if FUseCache then 
            if (Image.Width = Width) and (Image.Height = Height) then begin
              Items[Index].CacheBmp := CreateBmp32(Image.Width, Image.Height);
              Items[Index].CacheBmp.Assign(Image);
            end;

          Result := True;
        end

        else begin
          Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
          if Ico <> 0 then begin
            TmpBmp := CreateBmp32(Image.Width, Image.Height);
            if GetIconInfo(Ico, iInfo) then begin
              TmpBmp.Handle := iInfo.hbmColor;
              TmpBmp.HandleType := bmDIB;
              TmpBmp.PixelFormat := pf32bit;

              if not IsWinVistaUp and (GetBpp < 32) then // Update alpha channel
                CheckEmptyAChannel(TmpBmp);

              Image.Assign(TmpBmp);

              DeleteObject(iInfo.hbmMask);
              Result := True;
            end;
            FreeAndNil(TmpBmp);
            DestroyIcon(Ico);
          end;
        end;
      end
    else begin
      Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
      if Ico <> 0 then begin
        TmpBmp := CreateBmp32(Image.Width, Image.Height);
        if GetIconInfo(Ico, iInfo) then begin
          TmpBmp.Handle := iInfo.hbmColor;
          TmpBmp.HandleType := bmDIB;
          TmpBmp.PixelFormat := pf32bit;

          if not IsWinVistaUp and (GetBpp < 32) then // Update alpha channel
            CheckEmptyAChannel(TmpBmp);

          Image.Assign(TmpBmp);
          DeleteObject(iInfo.hbmMask);
          Result := True;
        end;
        FreeAndNil(TmpBmp);
        DestroyIcon(Ico);
      end;
    end;
end;


function TsAlphaImageList.GetBkColor: TColor;
begin
  if FBkColor = clNone then
    Result := inherited BkColor
  else
    Result := FBkColor;
end;


{$IFNDEF DELPHI_XE8}
function TsAlphaImageList.Count: Integer;
{$ELSE}
function TsAlphaImageList.GetCount: Integer;
{$ENDIF}
begin
  Result := Items.Count;
  if (Result < ImageList_GetImageCount(Handle)) and ((Result = 0) or (Items[0].ImageFormat = ifICO)) then
    UpdateFromStd;
end;


function TsAlphaImageList.GetDimension(const Index: Integer): integer;
begin
  if Index = 0 then
    Result := inherited Height
  else
    Result := inherited Width;
end;


function TsAlphaImageList.CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap;
var
  Ico: hIcon;
  iInfo: TIconInfo;
  D0, D: PByteArray;
  mBmp, Bmp: TBitmap;
  S0, S: PRGBAArray_;
  X, Y, DeltaS, DeltaD: integer;
begin
  Result := CreateBmp32(aWidth, aHeight); // Create bitmap in any case
  if HandleAllocated and IsValidIndex(Index, Count) then
    if IsDuplicated and (Index < Items.Count) and (Items[Index].ImgData.Size > 0) then begin // Using of original image if exists
      case Items[Index].ImageFormat of
        ifPNG: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = aWidth) and (Items[Index].CacheBmp.Height = aHeight) then begin
              Result.Assign(Items[Index].CacheBmp);
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          if (Items[Index].OrigWidth <> aWidth) or (Items[Index].OrigHeight <> aHeight) then begin // If must be scaled
            Bmp := CreateBmp32;
            Items[Index].ImgData.Seek(0, 0);
            LoadBmpFromPngStream(Bmp, Items[Index].ImgData);
            Stretch(Bmp, Result, aWidth, aHeight, ftMitchell);
            FreeAndNil(Bmp);
          end
          else begin
            Items[Index].ImgData.Seek(0, 0);
            LoadBmpFromPngStream(Result, Items[Index].ImgData);
            if Result.PixelFormat <> pf32bit then begin
              Result.PixelFormat := pf32bit;
              CheckEmptyAChannel(Result);
            end;
          end;
          // Make Cache
          if (BlendColor <> clNone) and not acIgnoreBlendColor then
            ChangeBitmapPixels(Result, ChangeColorTone, acColorToRGB(BlendColor), clFuchsia);

          if FUseCache then begin
            Items[Index].CacheBmp := CreateBmp32(aWidth, aHeight);
            Items[Index].CacheBmp.Assign(Result);
          end;
        end;

        ifBMP32: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = aWidth) and (Items[Index].CacheBmp.Height = aHeight) then begin
              Result.Assign(Items[Index].CacheBmp);
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          if (Items[Index].OrigWidth <> aWidth) or (Items[Index].OrigHeight <> aHeight) then begin // If must be scaled
            Bmp := CreateBmp32;
            Items[Index].ImgData.Seek(0, 0);
            Bmp.LoadFromStream(Items[Index].ImgData);
            if Bmp.PixelFormat <> pf32bit then begin
              Bmp.PixelFormat := pf32bit;
              CheckEmptyAChannel(Bmp);
            end;
            Stretch(Bmp, Result, aWidth, aHeight, ftMitchell);
            FreeAndNil(Bmp);
          end
          else begin
            Items[Index].ImgData.Seek(0, 0);
            Result.LoadFromStream(Items[Index].ImgData);
            if Result.PixelFormat <> pf32bit then begin
              Result.PixelFormat := pf32bit;
              CheckEmptyAChannel(Result);
            end;
          end;
          Items[Index].ImgData.Position := 0;
          // Make Cache
          if FUseCache then begin
            Items[Index].CacheBmp := CreateBmp32(aWidth, aHeight);
            Items[Index].CacheBmp.Assign(Result);
          end;
        end

        else begin
          Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
          if Ico <> 0 then begin
            if GetIconInfo(Ico, iInfo) then begin
              Result.Handle := iInfo.hbmColor;
              Result.HandleType := bmDIB;
              Result.PixelFormat := pf32bit;
              if not IsWinVistaUp then // Update alpha channel
                if (GetBpp < 32) or ((DefaultManager = nil) or DefaultManager.Options.CheckEmptyAlpha) then
                  CheckEmptyAChannel(Result);

              DeleteObject(iInfo.hbmMask);
            end;
            DestroyIcon(Ico);
          end;
        end;
      end;
    end
    else begin
      Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
      if Ico <> 0 then begin
        if GetIconInfo(Ico, iInfo) then begin
          Result.Handle := iInfo.hbmColor;
          Result.HandleType := bmDIB;
          Result.PixelFormat := pf32bit;
          if iInfo.hbmMask <> 0 then begin
            mBmp := CreateBmp32;
            mBmp.Handle := iInfo.hbmMask;
            mBmp.PixelFormat := pf8bit;
            if (mBmp.Width = Result.Width) and (mBmp.Height = Result.Height) then begin
              if InitLine(Result, Pointer(S0), DeltaS) and InitLine(mBmp, Pointer(D0), DeltaD) then
                for Y := 0 to Result.Height - 1 do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                  for X := 0 to Result.Width - 1 do
                    S[X].A := MaxByte - D[X];
                end;
            end;
            mBmp.Free;
          end
          else
            if not IsWinVistaUp then // Update alpha channel
              if (GetBpp < 32) or ((DefaultManager = nil) or DefaultManager.Options.CheckEmptyAlpha) then
                CheckEmptyAChannel(Result);

          DeleteObject(iInfo.hbmMask);
        end;
        DestroyIcon(Ico);
      end;
    end;
end;


function TsAlphaImageList.IsDuplicated: boolean;
begin
  Result := DoubleData or (csDesigning in ComponentState);
end;


procedure TsAlphaImageList.KillImgList;
begin
{$IFNDEF FPC}
  Handle := 0;
{$ELSE}
  if HandleAllocated and not ShareImages then
    ImageList_Destroy(Handle);
{$ENDIF}
  if not AcChanging then
    Change;
end;


procedure TsAlphaImageList.Loaded;
var
  w, h: integer;
begin
  inherited;
  FLoaded := True;
  if SavedWidth = 0 then begin
    w := inherited Width;
    h := inherited Height;
    if (w = 16) and (h = 16) then begin // If default size - reinit
      AcChanging := True;
      KillImgList;
      SetDimension(1, w);
      SetDimension(0, h);
    end;
  end
  else
    KillImgList;

  if SavedWidth = 0 then
    SetDimension(1, Width);

  if SavedHeight = 0 then
    SetDimension(0, Height);

  if not StdListIsGenerated then begin
    GenerateStdList;
    StdListIsGenerated := True; // Set the flag even if iconlist is empty
    Change;
  end;
end;


procedure TsAlphaImageList.LoadFromFile(const FileName: acString);
begin
  if not TryLoadFromfile(FileName) then begin
    SetLastError(ERROR_FILE_INVALID);
    raise Exception.Create('Cannot load ' + FileName + s_0D0A + 'Invalid or unexpected image format.');
  end;
end;


procedure TsAlphaImageList.SetNewScale(Value: Integer);
var
  b: boolean;
begin
  if (FSavedScale <> Value) and CanScale then begin
    b := AcChanging;
    AcChanging := True;
    FSavedScale := Value;
    inherited Clear;
    KillImgList;
    GenerateStdList;
    AcChanging := b;
    if not AcChanging then
      inherited Change;
  end;
end;


procedure TsAlphaImageList.SetBkColor(const Value: TColor);
begin
  FBkColor := Value;
  inherited BkColor := Value;
  if HandleAllocated then
    ImageList_SetBkColor(Handle, Value);
end;


procedure TsAlphaImageList.SetDimension(const Index, Value: integer);
begin
  case Index of
    0: begin
      if FLoaded then begin
        SavedHeight := Value;
      end;
      inherited Height := Value;
    end;

    1: begin
      if FLoaded then begin
        SavedWidth := Value;
      end;
      inherited Width := Value;
    end;
  end;
  if not (csLoading in ComponentState) and FLoaded and HandleAllocated and (Count <= Items.Count) and not AcChanging {If can generate new} then begin
    GenerateStdList;
    Change;
  end;
end;


procedure TsAlphaImageList.SetItems(const Value: TsImgListItems);
begin
  FItems.Assign(Value);
end;


procedure TsAlphaImageList.SetNewDimensions(Value: HImageList);
var
  AHeight, AWidth: Integer;
begin
  AWidth := Width;
  AHeight := Height;
  ImageList_GetIconSize(Value, AWidth, AHeight);
  Width := AWidth;
  Height := AHeight;
end;


function TsAlphaImageList.TryLoadFromfile(const FileName: acString): boolean;
var
  Ico: HICON;
  Bmp: TBitmap;
  iInfo: TIconInfo;
  S0, S: PRGBAArray_;
  iFormat: TsImageFormat;
  w, h, X, Y, DeltaS: integer;
begin
  Result := False;
  Ico := 0;
  if not HandleAllocated then
    GenerateStdList;

  if HandleAllocated and GetImageFormat(FileName, iFormat) then begin
    if IsDuplicated then // If double data used
      with TsImgListItem(Items.Add) do begin
        ImgData.LoadFromFile(FileName);
        ImageFormat := iFormat;
        ImageName := ExtractFileName(FileName);
        ImageName := Copy(ImageName, 1, Length(ImageName) - 4);
        case iFormat of
          ifPNG: begin
            Bmp := CreateBmp32;
            LoadBmpFromPngStream(Bmp, ImgData);
            OrigWidth := Bmp.Width;
            OrigHeight := Bmp.Height;
            Ico := MakeIcon32(Bmp);
            FreeAndNil(Bmp);
          end;

          ifBMP32: begin
            PixelFormat := pf32bit;
            Bmp := CreateBmp32;
            Bmp.LoadFromStream(ImgData);
            ImgData.Position := 0;
            OrigWidth := Bmp.Width;
            OrigHeight := Bmp.Height;
            if Bmp.PixelFormat <> pf32bit then begin // Convert to 32 with alpha channel
              Bmp.PixelFormat := pf32bit;
              ChangeBitmapPixels(Bmp, MakeAlphaPixel, 0, Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
            end;
            Ico := MakeIcon32(Bmp);
            FreeAndNil(Bmp);
          end;

          ifICO: begin
            Ico := {$IFDEF TNTUNICODE}ExtractIconW{$ELSE}ExtractIcon{$ENDIF}(hInstance, PacChar(FileName), 0);
            GetIconInfo(Ico, iInfo);
            Bmp := CreateBmp32;
            Bmp.Handle := iInfo.hbmColor;
            Bmp.HandleType := bmDIB;
            OrigWidth := Bmp.Width;
            OrigHeight := Bmp.Height;
            PixelFormat := pf24bit;
            w := Bmp.Width - 1;
            h := Bmp.Height - 1;
            if Bmp.PixelFormat = pf32bit then // Check the alpha channel
              if InitLine(Bmp, Pointer(S0), DeltaS) then
                for Y := 0 to h do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  for X := 0 to w do
                    if S[X].A <> 0 then begin
                      PixelFormat := pf32bit;
                      Break;
                    end;

                  if PixelFormat = pf32bit then
                    Break;
                end;

            FreeAndNil(Bmp);
            DeleteObject(iInfo.hbmColor);
            DeleteObject(iInfo.hbmMask);
          end;
        end;
      end
    else
      case iFormat of
        ifPNG: begin
          Bmp := CreateBmp32;
          LoadBmpFromPngFile(Bmp, FileName);
          Ico := MakeIcon32(Bmp);
          FreeAndNil(Bmp);
        end;

        ifBMP32: begin
          Bmp := CreateBmp32;
          Bmp.LoadFromFile(FileName);
          Ico := MakeIcon32(Bmp);
          FreeAndNil(Bmp);
        end;

        ifICO:
          Ico := {$IFDEF TNTUNICODE}ExtractIconW{$ELSE}ExtractIcon{$ENDIF}(hInstance, PacChar(FileName), 0);
      end;

    if Ico <> 0 then begin
      Result := ImageList_AddIcon(Handle, Ico) >= 0;
      DestroyIcon(Ico);
    end;
    Change;
  end;
end;


procedure TsAlphaImageList.ItemsClear;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].ImgData.Clear;

  Items.Clear;
end;


function TsAlphaImageList.CanScale: boolean;
begin
  Result := FAllowScale and FLoaded and (DefaultManager <> nil) and not (csDesigning in ComponentState) and (Items <> nil) and (Items.Count > 0);
end;


procedure TsAlphaImageList.Change;
var
  b: boolean;
  Bmp: TBitmap;
  iInfo: TIconInfo;
  Ico, NewIco: HICON;
  S0, S: PRGBAArray_S;
  X, Y, DeltaS, i, c, h, w: integer;
begin
  if not AcChanging then
    if HandleAllocated and not (csLoading in ComponentState) and StdListIsGenerated then begin
      if not (IsDuplicated and (Count <= Items.Count)) or (csDesigning in ComponentState) then
        inherited;

      if not (csDesigning in ComponentState) then
        if IsDuplicated and (Count <= Items.Count) {If icon was not added using AddIcon or other std. way (not stored in Items)} then begin
          if Count < Items.Count then begin
            AcChanging := True;
            GenerateStdList;
            AcChanging := False;
          end;
          inherited;
        end
        else begin
          c := ImageList_GetImageCount(Handle) - 1;
          if c >= 0 then begin
            Bmp := CreateBmp32;
            for i := 0 to c do begin
              Ico := ImageList_GetIcon(Handle, i, ILD_NORMAL);
              GetIconInfo(Ico, iInfo);
              DestroyIcon(Ico);
              Bmp.Handle := iInfo.hbmColor;
              Bmp.HandleType := bmDIB;
              b := False;
              h := Bmp.Height - 1;
              w := Bmp.Width - 1;
              Bmp.PixelFormat := pf32bit;
              if InitLine(Bmp, Pointer(S0), DeltaS) then begin
                for Y := 0 to h do begin // Check if AlphaChannel is empty
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  for X := 0 to w do
                    if S[X].SA <> 0 then begin
                      b := True;
                      Break;
                    end;

                  if b then
                    Break;
                end;
                if not b then begin
                  for Y := 0 to h do begin
                    S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                    for X := 0 to w do
                      with S[X] do
                        if SC <> sFuchsia.C then
                          SA := MaxByte;
                  end;
                  iInfo.hbmColor := Bmp.Handle;
                  NewIco := CreateIconIndirect(iInfo);
                  ImageList_ReplaceIcon(Handle, i, NewIco);
                  DestroyIcon(NewIco);
                end;
              end;
              DeleteObject(iInfo.hbmColor);
              DeleteObject(iInfo.hbmMask);
            end;
            FreeAndNil(Bmp);
          end;
        end;
    end;
end;


procedure TsAlphaImageList.Clear;
begin
  ItemsClear;
  inherited Clear;
end;


procedure TsAlphaImageList.ClearCache(ImgIndex: integer);
var
  i: integer;
begin
  if IsValidIndex(ImgIndex, Items.Count) then
    FreeAndNil(Items[ImgIndex].CacheBmp)
  else
    for i := 0 to Items.Count - 1 do
      if Items[i].CacheBmp <> nil then
        FreeAndNil(Items[i].CacheBmp);
end;


procedure TsAlphaImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;


procedure TsAlphaImageList.AcEndUpdate(DoChange: boolean = True);
begin
  AcChanging := False;
  if DoChange and ((DefaultManager = nil) or not (msSkinChanging in DefaultManager.ManagerState)) then begin
    GenerateStdList;
    Change;
  end;
end;


procedure TsAlphaImageList.SetUseCache(const Value: boolean);
begin
  if FUseCache <> Value then begin
    FUseCache := Value;
    if not Value then
      ClearCache;
  end;
end;


procedure TsAlphaImageList.MoveItem(CurIndex, NewIndex: integer);
begin
  Items[CurIndex].Index := NewIndex;
  Move(CurIndex, NewIndex);
end;


procedure TsAlphaImageList.Replace(AIndex: integer; ABmp32: TBitmap);
var
  Ico: HICON;
begin
  if HandleAllocated and (Items.Count > AIndex) then
    try
      if ABmp32.PixelFormat <> pf32bit then begin
        ABmp32.PixelFormat := pf32bit;
        FillAlphaRect(ABmp32, MkRect(ABmp32), MaxByte);
      end;

      if IsDuplicated then // If double data used
        with Items[AIndex] do begin
          FreeAndNil(CacheBmp);
          ImgData.Clear;
          ABmp32.SaveToStream(ImgData);
          ImageFormat := ifBMP32;
        end;

      Ico := MakeIcon32(ABmp32);
      if Ico <> 0 then begin
        ImageList_ReplaceIcon(Handle, AIndex, Ico);
        DestroyIcon(Ico);
      end;
      GenerateStdList;
      Change;
    except
    end;
end;


function TsAlphaImageList.Add(Image, Mask: TBitmap): Integer;
var
  Ico: hIcon;
  C: TsColor;
begin
  if IsDuplicated then // If double data used
    with TsImgListItem(Items.Add) do begin
      if Image.PixelFormat <> pf32bit then begin
        Image.PixelFormat := pf32bit;
        if Image.Transparent then begin
          C.C := Image.TransparentColor;
          C.A := 0;
          FillAlphaRect(Image, MkRect(Image), MaxByte, C.C);
        end
        else
          FillAlphaRect(Image, MkRect(Image), MaxByte, clNone);
      end;
      Image.SaveToStream(ImgData);
      ImageFormat := ifBMP32;
      PixelFormat := pf32bit;
      OrigWidth := Image.Width;
      OrigHeight := Image.Height;
      Ico := MakeIcon32(Image);
    end
  else
    Ico := MakeIcon32(Image);

  if Ico <> 0 then begin
    Result := ImageList_AddIcon(Handle, Ico);
    DestroyIcon(Ico);
  end
  else
    Result := -1;

  Change;
end;


function TsAlphaImageList.AddImage(Value: TCustomImageList; Index: Integer): Integer;
begin
  if Value is TsAlphaImageList then begin
    if IsValidIndex(Index, TsAlphaImageList(Value).Items.Count) then begin
      acBeginUpdate;
      Items.Add.Assign(TsAlphaImageList(Value).Items[Index]);
      acEndUpdate;
      Result := Items.Count;
    end
    else
      Result := -1;
  end
  else
{$IFDEF DELPHI7UP}
    Result := inherited AddImage(Value, Index);
{$ELSE}
    Result := 0;
{$ENDIF}
end;


constructor TsImgListItems.Create(AOwner: TsAlphaImageList);
begin
  inherited Create(TsImgListItem);
  FOwner := AOwner;
end;


destructor TsImgListItems.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TsImgListItems.GetItem(Index: Integer): TsImgListItem;
begin
  Result := TsImgListItem(inherited GetItem(Index));
end;


function TsImgListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TsImgListItems.SetItem(Index: Integer; Value: TsImgListItem);
begin
  inherited SetItem(Index, Value);
end;


procedure TsImgListItem.Assign(Source: TPersistent);
begin
  if Source <> nil then begin
    ImageFormat := TsImgListItem(Source).ImageFormat;
    ImageName   := TsImgListItem(Source).ImageName;
    PixelFormat := TsImgListItem(Source).PixelFormat;
    OrigWidth   := TsImgListItem(Source).OrigWidth;
    OrigHeight  := TsImgListItem(Source).OrigHeight;
    Text        := TsImgListItem(Source).Text;
    ImgData.LoadFromStream(TsImgListItem(Source).ImgData);
    TsImgListItem(Source).ImgData.Position := 0;
  end
  else
    inherited;
end;


procedure TsImgListItem.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    TsImgListItem(Dest).ImageFormat := ImageFormat;
    TsImgListItem(Dest).ImageName   := ImageName;
    TsImgListItem(Dest).PixelFormat := PixelFormat;
    TsImgListItem(Dest).OrigWidth   := OrigWidth;
    TsImgListItem(Dest).OrigHeight  := OrigHeight;
    TsImgListItem(Dest).Text        := Text;
    TsImgListItem(Dest).ImgData.LoadFromStream(ImgData);
    ImgData.Position := 0;
  end
  else
    inherited;
end;


constructor TsImgListItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  CacheBmp := nil;
  ImgData := TMemoryStream.Create;
  FImageName := '';
  FPixelFormat := pf32bit;
end;


procedure TsImgListItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ImgData', ReadData, WriteData, True);
end;


destructor TsImgListItem.Destroy;
begin
  with TsImgListItems(Collection).FOwner do
    if not (csDestroying in ComponentState) then
      if not ItemsUpdating and HandleAllocated then
        ImageList_Remove(Handle, Index);

  FreeAndNil(ImgData);
  if CacheBmp <> nil then
    FreeAndNil(CacheBmp);

  inherited Destroy;
end;


procedure TsImgListItem.ReadData(Reader: TStream);
begin
  ImgData.LoadFromStream(Reader);
end;


procedure TsImgListItem.WriteData(Writer: TStream);
begin
  ImgData.SaveToStream(Writer);
end;


procedure TsVirtualImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;


procedure TsVirtualImageList.AcEndUpdate(DoChange: boolean);
begin
  AcChanging := False;
  if not HandleAllocated then
    CreateImgList;

  UpdateList;
  if DoChange and ((DefaultManager = nil) or not (msSkinChanging in DefaultManager.ManagerState)) then
    Change;
end;


procedure TsVirtualImageList.AddAsIcon(Bmp: TBitmap; Ndx: integer);
var
  Ico: hIcon;
  TmpBmp: TBitmap;
begin
  if Bmp <> nil then begin
    if (Bmp.Width <> Width) or (Bmp.Height <> Height) then begin
      TmpBmp := CreateBmp32(Width, Height);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, Bmp.Canvas.Handle, (Bmp.Width - TmpBmp.Width) div 2, (Bmp.Height - TmpBmp.Height) div 2, SRCCOPY);
    end
    else
      TmpBmp := Bmp;

    if IsNTFamily and {$IFDEF DELPHI7UP}acThemesEnabled{$ELSE}False{$ENDIF} then
      Ico := MakeCompIcon(TmpBmp)
    else
      Ico := MakeIcon32(TmpBmp);

    if Ndx < 0 then
      ImageList_AddIcon(Handle, Ico)
    else
      ImageList_ReplaceIcon(Handle, Ndx, Ico);

    DestroyIcon(Ico);
    if TmpBmp <> Bmp then
      TmpBmp.Free;
  end;
end;


function TsVirtualImageList.CanUseCache: boolean;
begin
  Result := UseCache and (State = 0);
end;


procedure TsVirtualImageList.Change;
begin
  if not AcChanging then
    inherited;
end;


procedure TsVirtualImageList.ClearItems(ImgIndex: integer = -1);
var
  i: integer;
begin
  if ImgIndex >= 0 then begin
    if IsValidIndex(ImgIndex, Length(CachedImages)) then
      FreeAndNil(CachedImages[ImgIndex])
  end
  else
    for i := 0 to Length(CachedImages) - 1 do
      if CachedImages[i] <> nil then
        FreeAndNil(CachedImages[i]);
end;


constructor TsVirtualImageList.Create(AOwner: TComponent);
begin
  inherited;
  FHeight := 16;
  FWidth := 16;
  CurrentScale := 0;
  FUseCache := True;
  ForeColor := clNone;
  FStdHandleUsed := True;
  DrawingStyle := dsTransparent;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  PixelsPerInch := 96;
  State := 0;
  if acListsArray = nil then
    acListsArray := TList.Create;

  acListsArray.Add(Self);
end;


procedure TsVirtualImageList.CreateImgList;
begin
{$IFNDEF FPC}
  Handle := ImageList_Create(Width, Height, ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);
{$ELSE}
  ReferenceNeeded;
{$ENDIF}
end;


destructor TsVirtualImageList.Destroy;
begin
  if acListsArray <> nil then
    acListsArray.Extract(Self);

  FreeAndNil(FImageChangeLink);
  ClearItems;
  inherited;
end;


procedure TsVirtualImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  TmpBmp, IcoBmp: TBitmap;
  imgWidth, imgHeight, deltaX, deltaY: integer;
begin
  if HandleAllocated and IsValidIndex(Index, Count) then begin
    if Canvas.Font.Size = 0 then // If default char color can't be used
      ForeColor := Canvas.Font.Color
    else
      if (FAlphaImageList is TsCharImageList) and (TsCharImageList(FAlphaImageList).Items[Index].Color <> clNone) then
        ForeColor := clNone
      else
        ForeColor := Canvas.Font.Color;

    if CanUseCache and FUseCache then begin
      imgWidth := GetImageWidth(Self, Index, PixelsPerInch);
      imgHeight := GetImageHeight(Self, Index, PixelsPerInch);
      if Length(CachedImages) <= Index then
        SetLength(CachedImages, Index + 1);

      if CachedImages[Index] <> nil then begin
        if (CachedImages[Index].Width <> imgWidth) and (CachedImages[Index].Height <> imgHeight) or
              (CachedImages[Index].Canvas.Font.Color <> ForeColor) then
          FreeAndNil(CachedImages[Index]);
      end;
      if CachedImages[Index] = nil then begin
        CachedImages[Index] := CreateBmp32(imgWidth, imgHeight);
        CachedImages[Index].Canvas.Font.Color := ForeColor;
        FAlphaImageList.GetBitmap32(Index, CachedImages[Index], GetImageHeight(Self, -1, PixelsPerInch));
      end;
      IcoBmp := CachedImages[Index];
    end
    else begin
      IcoBmp := CreateBmp32(GetImageWidth(Self, Index, PixelsPerInch), GetImageHeight(Self, Index, PixelsPerInch));
      IcoBmp.Canvas.Font.Color := ForeColor;
      if State > 0 then
        SetImagesState(FAlphaImageList, State);

      FAlphaImageList.GetBitmap32(Index, IcoBmp, GetImageHeight(Self, -1, PixelsPerInch));
      if State > 0 then
        SetImagesState(FAlphaImageList, State);
    end;

    TmpBmp := CreateBmpLike(IcoBmp);
    TmpBmp.Canvas.Lock;
    try
      deltaX := 0;
      deltaY := 0;
      if FAlphaImageList is TsCharImageList then begin
        if TsCharImageList(FAlphaImageList).Items[Index].ScalingFactor <> 1 then begin
          deltaX := (IcoBmp.Width  - GetImageWidth (Self, Index, PixelsPerInch)) div 2;
          deltaY := (IcoBmp.Height - GetImageHeight(Self, Index, PixelsPerInch)) div 2;
        end;
      end;
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, Canvas.Handle, X - deltaX, Y - deltaY, SRCCOPY);
      CopyBmp32(MkRect(TmpBmp), MkRect(IcoBmp), TmpBmp, IcoBmp, MakeCacheInfo(TmpBmp), False, clNone, 0, False);
      BitBlt(Canvas.Handle, X - deltaX, Y - deltaY, TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      TmpBmp.Canvas.Unlock;
      FreeAndNil(TmpBmp);
      if not CanUseCache or not FUseCache then
        FreeAndNil(IcoBmp);
    end;
    ForeColor := clNone;
  end;
end;


procedure TsVirtualImageList.GenerateStdList;
var
  i: integer;
  Bmp: TBitmap;
begin
  if HandleAllocated and StdHandleUsed and not AcChanging then begin
    ForeColor := clNone;
    AcChanging := True;
    ClearItems;
    inherited Clear;
    if FAlphaImageList <> nil then begin
      if FUseCache then
        SetLength(CachedImages, FAlphaImageList.Count);

      for i := 0 to FAlphaImageList.Count - 1 do begin
        Bmp := CreateBmp32(Width, Height);
        if FAlphaImageList.GetBitmap32(i, Bmp, GetImageHeight(Self, -1, PixelsPerInch)) then begin
          AddAsIcon(Bmp, -1);
          if FUseCache then begin
            FreeAndNil(CachedImages[i]);
            CachedImages[i] := Bmp;
          end
          else
            Bmp.Free;
        end
        else
          Bmp.Free;
      end;
    end;
    StdListIsGenerated := True;
    AcChanging := False;
  end;
end;


function TsVirtualImageList.GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
begin
  Result := False;
  if HandleAllocated and (Index >= 0) and (FAlphaImageList <> nil) then begin
    if GlyphHeight = 0 then
      GlyphHeight := GetImageHeight(Self, -1, PixelsPerInch);

    if CanUseCache and UseCache and (Image.Width = Width) and (Image.Height = Height) then begin
      if Count > Index then begin
        if CachedImages[Index] = nil then begin
          CachedImages[Index] := CreateBmp32(Width, Height);
          FAlphaImageList.GetBitmap32(Index, CachedImages[Index], GlyphHeight);
          CachedImages[Index].Canvas.Font.Color := ForeColor;
        end;
        if CachedImages[Index] <> nil then
          CopyBmp(Image, CachedImages[Index]);
      end;
    end
    else
      if (FAlphaImageList <> nil) and (GetImageCount(FAlphaImageList) > Index) then begin
        FAlphaImageList.GetBitmap32(Index, Image, GlyphHeight);
        Result := True;
      end;
  end;
end;


{$IFNDEF DELPHI_XE8}
function TsVirtualImageList.Count: Integer;
{$ELSE}
function TsVirtualImageList.GetCount: Integer;
{$ENDIF}
begin
  if FAlphaImageList <> nil then
    Result := FAlphaImageList.Count
  else
    Result := 0;
end;


procedure TsVirtualImageList.ImageListChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    UpdateList;
end;


procedure TsVirtualImageList.KillImgList;
begin
{$IFNDEF FPC}
  Handle := 0;
{$ELSE}
  if HandleAllocated and not ShareImages then
    ImageList_Destroy(Handle);
{$ENDIF}
  Change;
end;


procedure TsVirtualImageList.Loaded;
begin
  inherited;
  KillImgList;
  CreateImgList;

  CurrentScale := 0;
  UpdateList(False);
  GenerateStdList;
end;


procedure TsVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAlphaImageList) then
    AlphaImageList := nil;
end;

{$IFDEF DELPHI_XE}
type
  TImageListWriteExProc = function(ImageList: HIMAGELIST; Flags: DWORD;
    Stream: IStream): HRESULT; {$IFNDEF CLR}stdcall;{$ENDIF}

var
  ImageListWriteExProc: TImageListWriteExProc;
{$ENDIF} // IFDEF DELPHI_XE

{$IFDEF DELPHI7UP}
procedure TsVirtualImageList.ReadData(Stream: TStream);
begin
// Data is virtual
end;

procedure TsVirtualImageList.WriteData(Stream: TStream); // Handler added for work with ActionList
  {$IFDEF DELPHI_XE}
var
  SA: TStreamAdapter;
  ComCtrlHandle: THandle;
  ImgLst: TImageList;
  {$ENDIF}
begin
  {$IFDEF DELPHI_XE}
  if not (csWriting in ComponentState) then begin
    // Code required for work of ActionList designer
    ComCtrlHandle := GetModuleHandle(comctl32);
    ImageListWriteExProc := GetProcAddress(ComCtrlHandle, 'ImageList_WriteEx');
    SA := TStreamAdapter.Create(Stream);
    try // Save empty bitmap, data is duplicated in Items
      ImgLst := TImageList.Create(Self);
      ImgLst.Width := 1;
      ImgLst.Height := 1;
      ImgLst.Masked := False;
      ImgLst.ColorDepth := cd4bit;
      if Assigned(ImageListWriteExProc) then
        ImageListWriteExProc(ImgLst.Handle, 1 {ILP_DOWNLEVEL}, SA)
      else
        ImageList_Write(ImgLst.Handle, SA);

      ImgLst.Free;
    finally
      SA.Free;
    end;
  end;
  {$ENDIF}
end;
{$ENDIF} // IFDEF DELPHI7UP


procedure TsVirtualImageList.RenderCacheNow(ItemIndex: integer = -1);
var
  i: integer;

  procedure MakeCache(AIndex: integer);
  begin
    if CachedImages[AIndex] = nil then begin
      CachedImages[AIndex] := CreateBmp32(Width, Height);
      FAlphaImageList.GetBitmap32(AIndex, CachedImages[AIndex], GetImageHeight(Self, -1, PixelsPerInch));
    end;
  end;

begin
  if UseCache then
    if ItemIndex < 0 then
      for i := 0 to Count - 1 do
        MakeCache(i)
    else
      MakeCache(ItemIndex);
end;


function TsVirtualImageList.ScaleValue: real;
begin
  Result := GetImageWidth(Self, -1, 100) / GetImageWidth(AlphaImageList, -1, 100);
end;


procedure TsVirtualImageList.SetAlphaImageList(const Value: TacImageList);
begin
  if FAlphaImageList <> nil then
    FAlphaImageList.UnRegisterChanges(FImageChangeLink);

  FAlphaImageList := Value;
  if FAlphaImageList <> nil then
    FAlphaImageList.RegisterChanges(FImageChangeLink);

  if not AcChanging then
    HandleNeeded;

  if not (csLoading in ComponentState) then
    UpdateList;
end;


procedure TsVirtualImageList.SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
begin
  if AlphaImageList is TsCharImageList then
    if IgnoreDefault then
      ForeColor := AColor
    else
      if (AIndex >= 0) and (TsCharImageList(AlphaImageList).Items[AIndex].Color <> clNone) then
        ForeColor := TsCharImageList(AlphaImageList).Items[AIndex].Color
      else
        ForeColor := AColor;
end;


procedure TsVirtualImageList.SetForeColor(const Value: TColor);
begin
  FForeColor := Value;
  if AlphaImageList is TsCharImageList then
    TsCharImageList(AlphaImageList).ForeColor := FForeColor;
end;


procedure TsVirtualImageList.SetInteger(const Index, Value: integer);
begin
  case Index of
    0: if FHeight <> Value then begin
      FHeight := Value;
      KillImgList;
      if not AcChanging then begin // Immediate update if AcBeginUpdate was not called
        if HandleAllocated and StdHandleUsed then
          Loaded
        else
          CreateImgList;

        Clear;
        UpdateList;
      end;
    end;

    1: if FWidth <> Value then begin
      FWidth := Value;
      KillImgList;
      if not AcChanging then begin // Immediate update if AcBeginUpdate was not called
        if HandleAllocated and StdHandleUsed then
          Loaded
        else
          CreateImgList;

        Clear;
        UpdateList;
      end;
    end;
  end;
end;


procedure TsVirtualImageList.SetStdHandleUsed(const Value: boolean);
begin
  FStdHandleUsed := Value;
  if not FStdHandleUsed then
    inherited Clear
  else
    GenerateStdList;
end;


procedure TsVirtualImageList.SetUseCache(const Value: boolean);
begin
  if FUseCache <> Value then begin
    if not Value then
      ClearItems;

    FUseCache := Value;
    UpdateList;
  end;
end;


procedure TsVirtualImageList.UpdateList;
begin
  if (IgnoreGenerated or not StdListIsGenerated) and not AcChanging and HandleAllocated then begin
    GenerateStdList;
    StdListIsGenerated := Count > 0;
    Change;
  end;
end;


function TsVirtualImageList.CreateBitmap32(Index: Integer): TBitmap;
begin
  if Count > Index then
    if FUseCache then begin
      Result := CreateBmp32;
      GetBitmap32(Index, Result, GetImageHeight(Self, Index, PixelsPerInch));
    end
    else
      if FAlphaImageList <> nil then
        Result := FAlphaImageList.CreateBitmap32(Index, Width, Height, GetImageHeight(Self, -1, PixelsPerInch))
      else
        Result := nil
  else
    Result := nil;
end;


function TsAlphaImageList.TryLoadFromPngStream(Stream: TStream): Boolean;
var
  Ico: HICON;
  Bmp: TBitmap;
  iFormat: TsImageFormat;
begin
  Result := False;
  Ico := 0;
  try
    if HandleAllocated and GetImageFormat(Stream, iFormat) then begin
      if IsDuplicated then // If double data used
        with TsImgListItem(Items.Add) do begin
          ImgData.LoadFromStream(Stream);
          ImgData.Position := 0;
          ImageFormat := iFormat;
          case iFormat of
            ifPNG: begin
              PixelFormat := pf32bit;
              Bmp := CreateBmp32;
              LoadBmpFromPngStream(Bmp, ImgData);
              Ico := MakeIcon32(Bmp);
              FreeAndNil(Bmp);
            end;
          end;
        end
      else
        case iFormat of
          ifPNG: begin
            Bmp := CreateBmp32;
            LoadBmpFromPngStream(Bmp, Stream);
            Ico := MakeIcon32(Bmp);
            FreeAndNil(Bmp);
          end;
        end;

      if Ico <> 0 then begin
        Result := ImageList_AddIcon(Handle, Ico) >= 0;
        DestroyIcon(Ico);
      end;
      Change;
    end;
  except
    Result := False;
  end;
end;


procedure TsAlphaImageList.UpdateFromStd;
var
  ico: hIcon;
  icon: TIcon;
  i, c: integer;
begin
  ItemsUpdating := True;
  Items.Clear;
  ItemsUpdating := False;
  c := ImageList_GetImageCount(Handle);
  for i := 0 to c - 1 do begin
    ico := ImageList_GetIcon(Handle, i, ILD_NORMAL);
    if ico <> 0 then
      with TsImgListItem(Items.Add) do begin
        Icon := TIcon.Create;
        Icon.Handle := ico;
        OrigWidth := Icon.Width;
        OrigHeight := Icon.Height;

        Icon.SaveToStream(ImgData);
        FreeAndNil(Icon);
        ImageFormat := ifICO;
      end;
  end;
end;


procedure TsAlphaImageList.LoadFromPngStream(const Stream: TStream);
begin
  if not TryLoadFromPngStream(Stream) then
    MessageDlg('Cannot load from stream' + s_0D0A + 'Invalid or unexpected image format.', mtError, [mbOk], 0);
end;


function TacCharListItem.ActualWidth(UseAngle: boolean): integer;
begin
  Result := Round(ImgList.SavedWidth * ScalingFactor);
  if UseAngle and (Angle <> 0) then
    Result := Trunc(Abs(Result * acCos(Angle)) + Abs(ImgList.SavedHeight * ScalingFactor * acSin(Angle))) + 1;
end;


procedure TacCharListItem.Assign(Source: TPersistent);
begin
  if Source <> nil then begin
    FScalingFactor := TacCharListItem(Source).ScalingFactor;
    FFontName    := TacCharListItem(Source).FontName;
    FCharset     := TacCharListItem(Source).Charset;
    FColor       := TacCharListItem(Source).Color;
    FSecondColor := TacCharListItem(Source).SecondColor;
    FOrientation := TacCharListItem(Source).Orientation;
    FPitch       := TacCharListItem(Source).Pitch;
    FStyle       := TacCharListItem(Source).Style;
    FChar        := TacCharListItem(Source).Char;
    FDrawContour := TacCharListItem(Source).DrawContour;
    FAngle       := TacCharListItem(Source).Angle;
    FImageName   := TacCharListItem(Source).ImageName;
    FOffsetY     := TacCharListItem(Source).OffsetY;

    FAddedSize    := TacCharListItem(Source).AddedSize;
    FAddedChar    := TacCharListItem(Source).AddedChar;
    FAddedColor   := TacCharListItem(Source).AddedColor;
    FAddedCharset := TacCharListItem(Source).AddedCharset;
    FAddedFontName := TacCharListItem(Source).AddedFontName;
  end
  else
    inherited;
end;


procedure TacCharListItem.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then
    Dest.Assign(Self)
  else
    inherited;
end;


constructor TacCharListItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FScalingFactor := 1;
  CacheBmp     := nil;
  FImageName   := '';
  FChar        := 0;
  FFontName    := s_FontAwesome;
  FColor       := clNone;
  FSecondColor := clNone;
  FCharset     := DEFAULT_CHARSET;
  FOrientation := 0;
  FAngle       := 0;
  FPitch       := fpDefault;
  FStyle       := [];
  FOffsetY     := 0;
  FDrawContour := False;
  CachedState  := 0;

  FAddedSize    := 50;
  FAddedChar    := 0;
  FAddedColor   := clNone;
  FAddedCharset := DEFAULT_CHARSET;
end;


destructor TacCharListItem.Destroy;
begin
  if CacheBmp <> nil then
    FreeAndNil(CacheBmp);

  inherited Destroy;
end;


function TacCharListItem.GetImage(CharHeight: integer = 0): TBitmap;
var
  actHeight, actWidth: integer;
  C: TColor;
begin
  with ImgList do begin
    actHeight := GetImageHeight(TacCharListItems(Collection).FOwner, Index, PixelsPerInch);
    actWidth  := GetImageWidth (TacCharListItems(Collection).FOwner, Index, PixelsPerInch);
    if CharHeight = 0 then
      CharHeight := GetImageHeight(TacCharListItems(Collection).FOwner, -1, PixelsPerInch);

    if ImgList.ForeColor <> clNone then
      C := ForeColor
    else
      C := Color;

    if CanUseCache then begin
      if (CacheBmp <> nil) and not ItemIsCached(Items[Index], actWidth, actHeight, CharHeight, C) then
        FreeAndNil(CacheBmp);

      if CacheBmp = nil then begin
        CacheBmp := CreateBmp32(actWidth, actHeight);
        PrepareBmp(CacheBmp, Index, MkRect(CacheBmp), CharHeight, OffsetY);
        CacheBmp.Canvas.Font.Color := C; // Save color for further comparing
        CachedState := State;
        if State = 0 then
          ActBlend := TacCharListItems(Collection).FOwner.BlendValue
        else
          ActBlend := MaxByte;
      end;
      Result := CacheBmp;
    end
    else begin
      Result := CreateBmp32(actWidth, actHeight);
      PrepareBmp(Result, Index, MkRect(Result), CharHeight, OffsetY);
    end;
  end;
end;


function TacCharListItem.ImgList: TsCharImageList;
begin
  Result := TacCharListItems(Collection).FOwner;
end;


procedure TacCharListItem.Invalidate;
begin
  FreeAndNil(CacheBmp);
  ImgList.Change;
end;


function TacCharListItem.NotDefFont: boolean;
begin
  Result := FontName <> s_FontAwesome;
end;


function TacCharListItem.NotDefScaling: boolean;
begin
  Result := FScalingFactor <> 1;
end;


function TacCharListItem.NotEmptyAdded: boolean;
begin
  Result := (FAddedChar <> 0) and (FAddedFontName <> '');
end;


procedure TacCharListItem.SetCharset(const Index: Integer; const Value: TFontCharset);

  procedure ChangeProp(var Prop: TFontCharset; Value: TFontCharset);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FCharset,      Value);
    1: ChangeProp(FAddedCharset, Value);
  end;
end;


procedure TacCharListItem.SetColor(const Index: Integer; const Value: TColor);

  procedure ChangeProp(var Prop: TColor; Value: TColor);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FColor,       Value);
    1: ChangeProp(FSecondColor, Value);
    2: ChangeProp(FAddedColor,  Value);
  end;
end;


procedure TacCharListItem.SetDrawContour(const Value: boolean);
begin
  if FDrawContour <> Value then begin
    FDrawContour := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetFontName(const Value: string);
begin
  if FFontName <> Value then begin
    FFontName := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FOrientation,  Value);
    1: ChangeProp(FOffsetY,      Value);
    2: ChangeProp(FAddedOffsetX, Value);
    3: ChangeProp(FAddedOffsetY, Value);
    4: ChangeProp(FAddedSize,    Value);
  end;
end;


procedure TacCharListItem.SetPitch(const Value: TFontPitch);
begin
  if FPitch <> Value then begin
    FPitch := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetScalingFactor(const Value: real);
begin
  if FScalingFactor <> Value then begin
    FScalingFactor := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetAddedFontName(const Value: string);
begin
  if FAddedFontName <> Value then begin
    FAddedFontName := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetStyle(const Value: TFontStyles);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetWord(const Index: Integer; const Value: Word);

  procedure ChangeProp(var Prop: Word; Value: Word);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FChar,      Value);
    1: ChangeProp(FAddedChar, Value);
  end;
end;


procedure TacCharListItem.SetAngle(const Value: integer);
begin
  if FAngle <> Value then begin
    FAngle := Value;
    Invalidate;
  end;
end;


procedure TsCharImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;


procedure TsCharImageList.AcEndUpdate(DoChange: boolean);
begin
  AcChanging := False;
  if DoChange and ((DefaultManager = nil) or not (msSkinChanging in DefaultManager.ManagerState)) then
    Change;
end;


function TsCharImageList.AddEmbeddedFont(const FileName, FontName: acString): boolean;
var
  ef: TacEmbeddedFont;
begin
  if FileExists(FileName) then begin
    if (FontIndex(FontName) < 0) then begin
      ef := TacEmbeddedFont(EmbeddedFonts.Add);
      ef.FFontName := FontName;
      ef.FFileName := FileName;
      ef.FontData.LoadFromFile(FileName);
      ef.LoadFont;
    end;
    Result := True;
  end
  else
    Result := False;
end;


function TsCharImageList.AddItem(const ItemData: TacCharItemData): integer;
var
  Bmp: TBitmap;
  cli: TacCharListItem;
begin
  cli := TacCharListItem.Create(Items);
  cli.FChar           := ItemData.Char;
  cli.FFontName       := ItemData.FontName;
  cli.FCharset        := ItemData.Charset;
  cli.FColor          := ItemData.Color;
  cli.FSecondColor    := ItemData.SecondColor;
  cli.FOrientation    := ItemData.Orientation;
  cli.FPitch          := ItemData.Pitch;
  cli.FAngle          := ItemData.Angle;
  cli.FDrawContour    := ItemData.DrawContour;
  cli.FStyle          := ItemData.Style;
  cli.FScalingFactor  := ItemData.ScalingFactor;
  cli.OffsetY         := ItemData.OffsetY;

  Bmp := cli.GetImage;
  AddAsIcon(Bmp, -1);
  if not CanUseCache then
    Bmp.Free;

  Result := Items.Count - 1;
end;


procedure TsCharImageList.AfterConstruction;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FLoaded := True;
end;


procedure TsCharImageList.Assign(Source: TPersistent);
var
  ImageList: TsCharImageList;
begin
  if Source = nil then begin
    KillImgList
  end
  else
    if Source is TsCharImageList then begin
      AcBeginUpdate;
      inherited Clear;
      ImageList := TsCharImageList(Source);
      Masked := ImageList.Masked;
      ImageType := ImageList.ImageType;
      DrawingStyle := ImageList.DrawingStyle;
      ShareImages := ImageList.ShareImages;

      Width := ImageList.SavedWidth;
      Height := ImageList.SavedHeight;
      SavedWidth := ImageList.SavedWidth;
      SavedHeight := ImageList.SavedHeight;
      KillImgList;

      if not HandleAllocated then
        CreateImgList
      else
        ImageList_SetIconSize(Handle, Width, Height);

      if (ImageList <> nil) and ImageList.HandleAllocated then
        BkColor := GetColor(ImageList_GetBkColor(ImageList.Handle));

      BlendValue := ImageList.BlendValue;
      BlendColor := ImageList.BlendColor;
      EmbeddedFonts.Assign(ImageList.EmbeddedFonts);
      CopyImages(ImageList);
      GenerateStdList;
      AcEndUpdate(False);
    end
    else
      inherited Assign(Source);
end;


procedure TsCharImageList.AssignTo(Dest: TPersistent);
begin
  if Dest is TsCharImageList then
    Dest.Assign(Self)
  else
    inherited AssignTo(Dest);
end;


function TsCharImageList.CanScale: boolean;
begin
  Result := FAllowScale and FLoaded and (DefaultManager <> nil) and not (csDesigning in ComponentState) and (Items <> nil) and (Items.Count > 0);
end;


function TsCharImageList.CanUseCache: boolean;
begin
  case BlendingMode of
    bmInactive: Result := (State = 0)
    else {bmAlways:} Result := True;
  end;
end;


procedure TsCharImageList.Change;
begin
  if not AcChanging and (StdListIsGenerated or (Count = 0)) and ([csLoading] * ComponentState = []) then
    if HandleAllocated then
      if Count <= Items.Count {If icon was not added using AddIcon or other std. way (not stored in Items)} then begin
        if Count <> Items.Count then begin
          AcChanging := True;
          GenerateStdList;
          AcChanging := False;
        end;
        inherited;
      end;
end;


procedure TsCharImageList.Clear;
begin
  Items.Clear;
  inherited Clear;
end;


procedure TsCharImageList.ClearCache(ImgIndex: integer = -1);
var
  i: integer;
begin
  if IsValidIndex(ImgIndex, Items.Count) then
    FreeAndNil(Items[ImgIndex].CacheBmp)
  else
    for i := 0 to Items.Count - 1 do
      FreeAndNil(Items[i].CacheBmp);
end;


procedure TsCharImageList.CopyImages(const ImgList: TsCharImageList);
var
  i: integer;
begin
  if HandleAllocated then begin
    ImageList_SetBkColor(ImgList.Handle, CLR_NONE);
    Items.Clear;
    for i := 0 to ImgList.Items.Count - 1 do
      TacCharListItem(Items.Add).Assign(ImgList.Items[i]);

    GenerateStdList;
  end;
end;


constructor TsCharImageList.Create(AOwner: TComponent);
begin
  inherited;
  FLoaded := False;
  FSavedScale := 0;
  Height := 24;
  Width := 26;
  State := 0;
  PixelsPerInch := DefaultPPI;
  FEmbeddedFonts := TacEmbeddedFonts.Create(Self);
  FItems := TacCharListItems.Create(Self);
  ForeColor2 := clNone;
  FAllowScale := True;
  FBlendValue := MAXBYTE;
  FBlendingMode := bmInactive;
  FBkColor := clNone;
end;


function TsCharImageList.CreateBitmap32Color(Index, aWidth, aHeight: integer; CharColor: TColor; GlyphHeight: integer = 0): TBitmap;
var
  Offs: integer;
begin
  Result := nil;
  if HandleAllocated and IsValidIndex(Index, Items.Count) then begin
    ForeColor := CharColor;
    if GlyphHeight = 0 then
      GlyphHeight := GetImageHeight(Self, -1, PixelsPerInch);

    if (Items[Index].CacheBmp <> nil) and not ItemIsCached(Items[Index], aWidth, aHeight, GlyphHeight, CharColor) then
      FreeAndNil(Items[Index].CacheBmp);

    if CanUseCache then begin
      if Items[Index].CacheBmp = nil then begin
        if FSavedScale <> 0 then
          Offs := Items[Index].OffsetY * aPPIArray[FSavedScale] div 96
        else
          Offs := Items[Index].OffsetY;

        Items[Index].CacheBmp := CreateBmp32(aWidth, aHeight);
        Items[Index].ActBlend := BlendValue;
        Items[Index].CachedState := State;
        PrepareBmp(Items[Index].CacheBmp, Index, MkRect(aWidth, aHeight), GlyphHeight, Offs);
        Items[Index].CacheBmp.Canvas.Font.Color := CharColor; // Save color for further comparing
      end;
      Result := CreateBmp32(aWidth, aHeight);
      Result.Assign(Items[Index].CacheBmp);
    end
    else begin
      if FSavedScale <> 0 then
        Offs := Items[Index].OffsetY * aPPIArray[FSavedScale] div 96
      else
        Offs := Items[Index].OffsetY;

      Result := CreateBmp32(aWidth, aHeight);
      PrepareBmp(Result, Index, MkRect(aWidth, aHeight), GlyphHeight, Offs);
    end;
  end;
end;


function TsCharImageList.CreateBitmap32(Index, aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap;
begin
  Result := CreateBitmap32Color(Index, aWidth, aHeight, ForeColor, GlyphHeight);
end;


procedure TsCharImageList.CreateImgList;
var
  Size: TSize;
  iPPI: integer;
begin
  if (SavedWidth = 0) and FLoaded and not (csLoading in ComponentState) then begin
    SavedWidth := Width;
    SavedHeight := Height;
  end;
  if csDesigning in ComponentState then
    iPPI := 96
  else
    if AcChanging then
      iPPI := PixelsPerInch
    else
      if (DefaultManager = nil) or (DefaultManager.Options.ScaleMode = smVCL) then
        iPPI := DefaultPPI
      else
        iPPI := DefaultManager.Options.PixelsPerInch;

  Size := MkSize(SavedWidth * iPPI div 96, SavedHeight * iPPI div 96);
  Handle := ImageList_Create(Size.cx, Size.cy, ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);
end;


destructor TsCharImageList.Destroy;
begin
  FEmbeddedFonts.Free;
  FreeAndNil(FItems);
  inherited;
end;


procedure TsCharImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Bmp, SrcImage: TBitmap;
begin
  if HandleAllocated and IsValidIndex(Index, Items.Count) then begin
    Bmp := CreateBmp32;//(Width, Height);
    if Bmp <> nil then
      try
        Bmp.Canvas.Lock;
        if Items[Index].Color = clNone then
          ForeColor := Canvas.Font.Color;

        SrcImage := Items[Index].GetImage;
        Bmp.Height := SrcImage.Height;
        Bmp.Width := SrcImage.Width;
        // Copy BG
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, X, Y, SRCCOPY);
        // Paing glyph
        PaintBmp32(Bmp, SrcImage);
        // Copy back
        BitBlt(Canvas.Handle, X, Y, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Canvas.Unlock;
        ForeColor := clNone;
      finally
        if not CanUseCache then
          FreeAndNil(SrcImage);

        FreeAndNil(Bmp);
      end;
  end;
end;

////////////////////////////////////////////////////////////////

type
  PFontStroke = ^TFontStroke;
  TFontStroke = record
    GlyphNumber: integer;
    Pt1, Pt2: TPoint;
  end;

  TEnumStrokesCallback = function(Idx: integer; const Stroke: TFontStroke): boolean of object;

  TacStrokeCollection = class(TList)
  protected
    function GetNumGlyphs: integer;
    function GetFontStroke(Idx: integer): TFontStroke;
    procedure FreeStrokes;
    function GetBounds: TRect;
  public
    destructor Destroy; override;
    { Returns the index of the first stroke for the glyph number GlyphNumber }
    function StartOfGlyph(GlyphNumber: integer): integer;
    { Returns the count of strokes for the glyph number GlyphNumber. }
    function GlyphNumStrokes(GlyphNumber: integer): integer;
    { Enumerates all strokes of all glyphs }
//    procedure EnumStrokes(Callback: TEnumStrokesCallback);
    { Returns the number of glyphs }
    property NumGlyphs: integer read GetNumGlyphs;
    { Returns the stroke number Idx. Use StrartOfGlyph to determine
      the index of the first stroke for a given glyph.
      Use GlyphNumStrokes to determine the number of strokes a glyph is. }
    property Stroke[Idx: integer]: TFontStroke read GetFontStroke;
    { Returns the smallest rectangle that completely bounds all glyphs }
    property Bounds: TRect read GetBounds;
  end;


destructor TacStrokeCollection.Destroy;
begin
  FreeStrokes;
  inherited Destroy;
end;

{
procedure TacStrokeCollection.EnumStrokes(Callback: TEnumStrokesCallback);
var
  i: integer;
begin
  if Assigned(Callback) then
    for i := 0 to Count - 1 do
      if not Callback(i, PFontStroke(Items[i])^) then
        Break;
end;
}

procedure TacStrokeCollection.FreeStrokes;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    FreeMem(Items[i]);
    Items[i] := nil;
  end;
  Pack;
end;


function TacStrokeCollection.GetBounds: TRect;
var
  i: integer;
  fs: PFontStroke;
begin
  if Count = 0 then
    Result := MkRect
  else begin
    Result := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
    for i := 0 to Count - 1 do begin
      fs := Items[i];
      if fs^.Pt1.X < Result.Left then
        Result.Left := fs^.Pt1.X;

      if fs^.Pt1.X > Result.Right then
        Result.Right := fs^.Pt1.X;

      if fs^.Pt2.X < Result.Left then
        Result.Left := fs^.Pt2.X;

      if fs^.Pt2.X > Result.Right then
        Result.Right := fs^.Pt2.X;

      if fs^.Pt1.Y < Result.Top then
        Result.Top := fs^.Pt1.Y;

      if fs^.Pt1.Y > Result.Bottom then
        Result.Bottom := fs^.Pt1.Y;

      if fs^.Pt2.Y < Result.Top then
        Result.Top := fs^.Pt2.Y;

      if fs^.Pt2.Y > Result.Bottom then
        Result.Bottom := fs^.Pt2.Y;
    end;
  end;
end;


function TacStrokeCollection.GetFontStroke(Idx: integer): TFontStroke;
begin
  if (Idx >= 0) and (Idx < Count) then
    Result := PFontStroke(Items[Idx])^
  else
    Error('', 0);
end;


function TacStrokeCollection.GetNumGlyphs: integer;
begin
  if Count = 0 then
    Result := 0
  else
    Result := PFontStroke(Items[Count - 1])^.GlyphNumber + 1;
end;


function TacStrokeCollection.GlyphNumStrokes(GlyphNumber: integer): integer;
var
  sog, eog: integer;
begin
  sog := StartOfGlyph(GlyphNumber);
  if sog < 0 then
    Result := -1
  else begin
    eog := StartOfGlyph(GlyphNumber + 1);
    if eog < 0 then
      eog := Count;

    Result := eog - sog;
  end;
end;


function TacStrokeCollection.StartOfGlyph(GlyphNumber: integer): integer;
var
  ng, i: integer;
begin
  ng := GetNumGlyphs;
  if (GlyphNumber < 0) or (GlyphNumber >= ng) then
    Result := -1
  else begin
    for i := 0 to Count - 1 do
      if PFontStroke(Items[i])^.GlyphNumber = GlyphNumber then
        break;

    Result := i;
  end;
end;


function Fix2Double(AFix: TFixed): double;
begin
  Result := AFix.fract / 65536.0 + AFix.Value;
end;


procedure DrawContour(ACanvas: TCanvas; AChar: WideChar; var ARect: TRect; ALineWidth, AAngle: integer; CalcRect: boolean);
const
  SplinePrecision = 7;
type
  TFXPArray = array [0 .. MaxInt div SizeOf(TPointFX) - 1] of TPointFX;
  PFXPArray = ^TFXPArray;
var
  gWidth, gHeight, rWidth, rHeight, X, Y, i, j, k, sog: integer;
  Glyphs: TacStrokeCollection;
  polys, pps: ^TacPointF;
  polyCounts, ppc: ^UINT;
  XdivY, sx, sy: double; // Scaling factors
  stroke: TFontStroke;
  GlyphBounds: TRect;
  Center: TacPointF;
  ActSize: TSize;

  function GetCharacterGlyphs(CharCode: integer): TacStrokeCollection;
  var
    F: TFont;
    m2: TMAT2;
    dc, mdc: HDC;
    Done: boolean;
    pc: PTTPolyCurve;
    lpAPFX: PFXPArray;
    gm: TGLYPHMETRICS;
    ps, p1, p2: TacPointF;
    pfxA, pfxB, pfXC: TacPointF;
    bufPtr, buf: PTTPolygonHeader;
    polyN, pcType, res, bufSize, EmSize, i: integer;
    PointSize, CurveSize, HeaderSize, ofs, ofs2, pcSize: integer;

    procedure NewStroke(AGlyphNumber: integer; var APt1, APt2: TacPointF);
    var
      ps: PFontStroke;
    begin
      ps := AllocMem(SizeOf(TFontStroke));
      ps^.GlyphNumber := AGlyphNumber;
      ps^.Pt1.X := Round(APt1.X);
      ps^.Pt1.Y := Round(APt1.Y);
      ps^.Pt2.X := Round(APt2.X);
      ps^.Pt2.Y := Round(APt2.Y);
      Result.Add(ps);
    end;

    procedure DrawQSpline(APolyN: integer; var pa, pb, pc: TacPointF);
    var
      di, i: double;
      p1, p2: TacPointF;
    begin
      di := 1.0 / SplinePrecision;
      i := di;
      p2 := pa;
      while i <= 1.0 do begin
        if i - di / 2 > 1.0 - di then
          i := 1.0;

        p1 := p2;
        p2.X := (pa.X - 2 * pb.X + pc.X) * sqr(i) + (2 * pb.X - 2 * pa.X) * i + pa.X;
        p2.Y := (pa.Y - 2 * pb.Y + pc.Y) * sqr(i) + (2 * pb.Y - 2 * pa.Y) * i + pa.Y;
        if (p1.X <> p2.X) or (p1.Y <> p2.Y) then
          NewStroke(APolyN, p1, p2);

        i := i + di;
      end;
      pc := p2;
    end;

  begin
    Result := nil;
    // Init device context
    dc := GetDC(0);
    mdc := CreateCompatibleDC(dc);
    ReleaseDC(0, dc);

    F := TFont.Create;
    try
      F.Assign(ACanvas.Font);
      EmSize := 512;//ACanvas.Font.Height * 8;
      F.Size := EmSize;
      SelectObject(mdc, F.Handle);
    finally
      F.Free;
    end;

    m2.eM11.Value := 1;
    m2.eM11.fract := 1; // Identity matrix

    m2.eM12.Value := 0;
    m2.eM12.fract := 1; // |1,0|

    m2.eM21.Value := 0;
    m2.eM21.fract := 1; // |0,1|

    m2.eM22.Value := 1;
    m2.eM22.fract := 1;

    // Recieve required buffer size
    bufSize := GetGlyphOutlineW(mdc, CharCode, GGO_NATIVE, gm, 0, nil, m2);
    if (bufSize <> integer(GDI_ERROR)) and (bufSize <> 0) then begin
      bufPtr := AllocMem(bufSize);
      buf := bufPtr;
      // Receive info into the buffer
      res := GetGlyphOutlineW(mdc, CharCode, GGO_NATIVE, gm, bufSize, PAnsiChar(buf), m2);
      if (res <> integer(GDI_ERROR)) and (buf^.dwType = TT_POLYGON_TYPE) then begin
        Result := TacStrokeCollection.Create;
        Done := False;
        ofs := 0;
        polyN := 0;
        HeaderSize := SizeOf(TTTPolygonHeader);
        PointSize := SizeOf(TPointFX);
        CurveSize := SizeOf(TTTPOLYCURVE);
        while not Done do begin
          ps.X := Fix2Double(buf^.pfxStart.X);
          ps.Y := Fix2Double(buf^.pfxStart.Y);
          pcSize := integer(buf^.cb) - HeaderSize;
          PAnsiChar(pc) := PAnsiChar(buf) + HeaderSize;
          ofs2 := 0;
          p2 := ps;
          while not Done and (ofs2 < pcSize) do begin
            pcType := pc^.wType;
            case pcType of
              TT_PRIM_LINE: begin
                lpAPFX := PFXPArray(@pc^.apfx[0]);
                for i := 0 to pc^.cpfx - 1 do begin
                  p1 := p2;
                  p2.X := Fix2Double(lpAPFX^[i].X);
                  p2.Y := Fix2Double(lpAPFX^[i].Y);
                  if (p1.X <> p2.X) or (p1.Y <> p2.Y) then
                    NewStroke(polyN, p1, p2);
                end;
              end;

              TT_PRIM_QSPLINE: begin
                lpAPFX := PFXPArray(@pc^.apfx[0]);
                pfxA := p2;
                for i := 0 to pc^.cpfx - 2 do begin
                  pfxB.X := Fix2Double(lpAPFX^[i].X);
                  pfxB.Y := Fix2Double(lpAPFX^[i].Y);
                  pfXC.X := Fix2Double(lpAPFX^[i + 1].X);
                  pfXC.Y := Fix2Double(lpAPFX^[i + 1].Y);
                  if i < pc^.cpfx - 2 then begin
                    pfXC.X := (pfXC.X + pfxB.X) / 2;
                    pfXC.Y := (pfXC.Y + pfxB.Y) / 2;
                  end;
                  DrawQSpline(polyN, pfxA, pfxB, pfXC);
                  pfxA := pfXC;
                end;
                p2 := pfXC;
              end;
            end;
            ofs2 := ofs2 + CurveSize + (pc^.cpfx - 1) * PointSize;
            PAnsiChar(pc) := PAnsiChar(pc) + CurveSize + (pc^.cpfx - 1) * PointSize;
          end;
          if not Done then begin
            p1 := p2;
            p2 := ps;
            if (p1.X <> p2.X) or (p1.Y <> p2.Y) then
              NewStroke(polyN, p1, p2);

            ofs := ofs + pcSize + HeaderSize;
            Done := (ofs >= bufSize - HeaderSize);
            PAnsiChar(buf) := PAnsiChar(pc);
            inc(polyN);
          end;
        end;
      end;
      FreeMem(bufPtr);
    end;
    DeleteDC(mdc);
  end;

begin
  Glyphs := GetCharacterGlyphs(ord(AChar));
  if Glyphs <> nil then begin
    rWidth := WidthOf(ARect);
    rHeight := HeightOf(ARect);

    GlyphBounds := Glyphs.Bounds;
    gWidth := WidthOf(GlyphBounds);
    gHeight := HeightOf(GlyphBounds);

    XdivY := gWidth / gHeight;
    if (XdivY * rHeight > rWidth) then begin // If Height of image is bigger
      ActSize.cx := rWidth - integer(CalcRect) * 6;
      ActSize.cy := ActSize.cx * gHeight div gWidth;
    end
    else begin
      ActSize.cy := min(rHeight - integer(CalcRect) * 4, ACanvas.Font.Height);
      ActSize.cx := ActSize.cy * gWidth div gHeight;
    end;


    if CalcRect then
      ARect := MkRect(ActSize)
    else begin
      X := (rWidth  - ActSize.cx) div 2 + ARect.Left;
      Y := (rHeight - ActSize.cy) div 2 + ARect.Top;
      // Compute the scaling factors
      sx := ActSize.cx / (gWidth + 1);
      sy := ActSize.cy / (gHeight + 1);
      // Allocate memory for the (all) points
      polys := AllocMem(SizeOf(TacPointF) * Glyphs.Count);
      // Allocate memory for the "points per polygon" counters
      polyCounts := AllocMem(SizeOf(UINT) * Glyphs.NumGlyphs);
      // Copy Glyphs' points to the allocated buffers
      pps := polys;
      ppc := polyCounts;
      if AAngle <> 0 then begin
        Center.X := aRect.Left + rWidth / 2;
        Center.Y := aRect.Top + rHeight / 2;
      end;
      for i := 0 to Glyphs.NumGlyphs - 1 do begin
        ppc^ := Glyphs.GlyphNumStrokes(i);
        sog := Glyphs.StartOfGlyph(i);
        for j := 0 to ppc^ - 1 do begin
          // Get a stroke
          stroke := Glyphs.stroke[sog + j];
          // Scale
          pps^.X := X + Round((stroke.Pt1.X - GlyphBounds.Left) * sx);
          pps^.Y := Y + rHeight - Round((stroke.Pt1.Y - GlyphBounds.Top) * sy) - 2;
          // Rotate
          if AAngle <> 0 then
            acgpRotatePoints(PacGpPoints(@pps), Center, 1, 360 - AAngle);

          pps := pointer(PAnsiChar(pps) + SizeOf(TPoint));
        end;
        ppc := pointer(PAnsiChar(ppc) + SizeOf(ppc^));
      end;
      j := 0;
      for i := 0 to Glyphs.NumGlyphs - 1 do begin
        k := PUINT(acNativeUInt(polyCounts) + acNativeUInt(i * SizeOf(UINT)))^;
        acgpDrawPolygon(ACanvas.Handle, clBlack, pointer(acNativeUInt(polys) + acNativeUInt(j * SizeOf(TacPointF))), k, ALineWidth);
        inc(j, k);
      end;
      FreeMem(polys);
      FreeMem(polyCounts);
    end;
    Glyphs.Free;
  end;
end;


type
  TacAlphaProc = procedure (var i: byte; C: TsColor_M; ABlendValue: byte);


procedure aProcBlended(var i: byte; C: TsColor_M; ABlendValue: byte);
begin
  with C do
    i := ((MaxByte - (MR + MG shl 1 {x2} + MB) shr 2) * ABlendValue) shr 8;
end;

procedure aProcOpaque(var i: byte; C: TsColor_M; ABlendValue: byte);
begin
  with C do
    i := MaxByte - (MR + MG shl 1 {x2} + MB) shr 2;
end;


procedure TsCharImageList.PrepareBmp(Bmp: TBitmap; Index: integer; const aRect: TRect; CharHeight: integer; Offset: integer);
var
  h, DeltaS, X, Y, dy: integer;
  Item: TacCharListItem;
  S0, S: PRGBAArray_;
  TmpBmp: TBitmap;
  ws: WideString;
  R, nR: TRect;
  alpha: byte;
  Col, Col2: TColor;

  newC: TsColor;
  newR, newG, newB, dR, dG, dB: single;

  GetAlpha: TacAlphaProc;

  procedure UpdateFontQuality;
  begin
{$IFDEF DELPHI_XE}
    if CharHeight > 24 then
      Bmp.Canvas.Font.Quality := fqAntialiased
    else
      Bmp.Canvas.Font.Quality := fqClearType;
{$ENDIF}
  end;

  procedure DrawAddedGlyph;
  var
    aR, cR: TRect;
    actC: TsColor;
    cSize: TSize;
    wc: WideString;
    Alpha: byte;
    Pad, X, Y, DeltaM, DeltaP, mSize: integer;
    AlphaBmp, TmpBmp: TBitmap;
    M0, M: PRGBAArray_M;
    P0, P: PRGBAArray;
  begin
    if Item.AddedChar <> 0 then begin
      wc := WideChar(Item.AddedChar);
      mSize := HeightOf(aRect) * Item.AddedSize div 100;

      Bmp.Canvas.Font.Charset := Item.AddedCharset;
      Bmp.Canvas.Font.Name := Item.AddedFontName;
      Bmp.Canvas.Font.Height := -mSize;

      acGetTextExtentW(Bmp.Canvas.Handle, wc, cSize);

      cR.Left   := aRect.Right - cSize.cx; // Left side of char
      cR.Top    := aRect.Bottom - cSize.cy;

      if (cR.Left < 0) or (cR.Top < 0) then // If main image is smaller then exit
        Exit;

      cR.Right  := cR.Left + cSize.cx;
      cR.Bottom := cR.Top  + cSize.cy;

      if Item.AddedColor = clNone then
        actC.C := Col
      else
        actC.C := SwapRedBlue(acColorToRGB(Item.AddedColor));

      TmpBmp := CreateBmp32(cR);
      TmpBmp.Canvas.Font.Assign(Bmp.Canvas.Font);
      TmpBmp.Canvas.Font.Color := 0;
      aR := MkRect(TmpBmp);
      DrawTextW(TmpBmp.Canvas.Handle, PWideChar(wc), 1, aR, DT_NOPREFIX or DT_SINGLELINE or DT_BOTTOM{DT_VCENTER or DT_EXTERNALLEADING} or DT_NOCLIP);

      Pad := max(3, WidthOf(aRect) div 30);
      AlphaBmp := CreateBmp32(cSize.cx + Pad * 2, cSize.cy + Pad * 2);
      BitBlt(AlphaBmp.Canvas.Handle, Pad, Pad, TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
      acgpBlur(AlphaBmp, Pad);
      if (State = 0) and (FBlendValue <> MAXBYTE) then
        GetAlpha := aProcBlended
      else
        GetAlpha := aProcOpaque;

      if InitLine(Bmp, Pointer(S0), DeltaS) and InitLine(TmpBmp, Pointer(M0), DeltaM) and InitLine(AlphaBmp, Pointer(P0), DeltaP) then
        for Y := 0 to AlphaBmp.Height - 1 do begin
          if Y + cR.Top - Pad >= Bmp.Height then
            Break;

          S := Pointer(PAnsiChar(S0) + DeltaS * (Y + cR.Top - Pad)); // Destination bmp
          P := Pointer(PAnsiChar(P0) + DeltaP * Y);                  // Bmp with blur
          if (Y >= Pad) and (Y < AlphaBmp.Height - 1 - Pad) then
            M := Pointer(PAnsiChar(M0) + DeltaM * (Y - Pad))         // Mask
          else
            M := nil;

          for X := 0 to AlphaBmp.Width - 1 do
            if X + cR.Left - Pad > 0 then
              with TsColor_(S[X + cR.Left - Pad]) do
                if X + cR.Left - Pad < Bmp.Width then begin
                  if (M <> nil) and (X >= Pad) and (X < AlphaBmp.Width - Pad) then
                    GetAlpha(Alpha, TsColor_M(M[X - Pad]), FBlendValue)
                  else
                    Alpha := 0;

                  if Alpha = 0 then
                    A := min(A, max(0, MaxByte - (MaxByte - P[X].R) * 6))
                  else begin
                    R := ((actC.B - R) * Alpha + R shl 8) shr 8;
                    G := ((actC.G - G) * Alpha + G shl 8) shr 8;
                    B := ((actC.R - B) * Alpha + B shl 8) shr 8;
                    A := Alpha;
                  end;
                end;
        end;

      AlphaBmp.Free;
      TmpBmp.Free;
    end;
  end;

begin
  Bmp.PixelFormat := pf32bit;
  if (Bmp.Width = 0) or (Bmp.Height = 0) then begin
    Bmp.Width := CharHeight;
    Bmp.Height := CharHeight;
  end;
  Item := Items[Index];

  Bmp.Canvas.Font.Pitch   := Item.Pitch;
  Bmp.Canvas.Font.Style   := Item.Style;

  Bmp.Canvas.Font.Charset := Item.Charset;
  Bmp.Canvas.Font.Name    := Item.FontName;
  Bmp.Canvas.Font.Color   := clBlack;
  ws := WideChar(Item.Char);

  Bmp.Canvas.Font.Height  := Round(CharHeight * Item.FScalingFactor);

  FillRect32(Bmp, MkRect(Bmp), clWhite, clBlack);
  // Calc
  R := aRect;
  if Item.DrawContour then
    DrawContour(Bmp.Canvas, ws[1], R, CharHeight div 48 + 1, 0, True)
  else
    DrawTextW(Bmp.Canvas.Handle, PWideChar(ws), 1, R, DT_NOPREFIX or DT_CALCRECT or DT_EXTERNALLEADING or DT_NOCLIP);

  nR.Left := (Bmp.Width - WidthOf(R)) div 2;
  nR.Top := (Bmp.Height - HeightOf(R)) div 2;
  nR.Right := nR.Left + WidthOf(R);
  nR.Bottom := nR.Top + HeightOf(R);
  if Offset <> 0 then begin
    dy := Round(Offset * Item.FScalingFactor);
    OffsetRect(nR, 0, dy);
  end;

  if Item.Angle <> 0 then begin
    if Item.DrawContour then
      DrawContour(Bmp.Canvas, ws[1], nR, CharHeight div 48 + 1, Item.Angle, False)
    else begin
      TmpBmp := CreateBmpLike(Bmp);
      TmpBmp.Canvas.Font.Assign(Bmp.Canvas.Font);
      UpdateFontQuality;
      DrawTextW(TmpBmp.Canvas.Handle, PWideChar(ws), 1, nR, DT_NOPREFIX or DT_VCENTER{ or DT_CENTER} or DT_EXTERNALLEADING or DT_NOCLIP);
      acgpDrawRotatedBitmap(Bmp.Canvas.Handle, TmpBmp, MkRect(TmpBmp), Item.Angle mod i360);
      TmpBmp.Free;
    end;
  end
  else
    if Item.DrawContour then
      DrawContour(Bmp.Canvas, ws[1], nR, CharHeight div 48 + 1, 0, False)
    else begin
      UpdateFontQuality;
      DrawTextW(Bmp.Canvas.Handle, PWideChar(ws), 1, nR, DT_NOPREFIX or DT_VCENTER{ or DT_CENTER} or DT_EXTERNALLEADING or DT_NOCLIP);
    end;

  if ForeColor <> clNone then
    Col := SwapRedBlue(acColorToRGB(ForeColor))
  else
    if Item.Color <> clNone then
      Col := SwapRedBlue(acColorToRGB(Item.Color))
    else
      Col := 0;

  if (Item.SecondColor <> clNone) and ((ForeColor = clNone) or (ForeColor <> SwapRedBlue(acColorToRGB(Item.Color)))) and (ForeColor2 = clNone {ColorTone is not defined}) then
    Col2 := SwapRedBlue(acColorToRGB(Item.SecondColor))
  else
    Col2 := clNone;

  if InitLine(Bmp, Pointer(S0), DeltaS) then
    if Col2 = clNone then // Not gradient
      if (State = 0) and (FBlendValue <> MAXBYTE) then
        for Y := 0 to Bmp.Height - 1 do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          for X := 0 to Bmp.Width - 1 do
            with TsColor_(S[X]) do begin
              alpha := MaxByte - (R + G shl 1 {x2} + B) shr 2;
              C := Col;
              A := (alpha * FBlendValue) shr 8;
            end;
        end
      else
        for Y := 0 to Bmp.Height - 1 do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          for X := 0 to Bmp.Width - 1 do
            with TsColor_(S[X]) do begin
              alpha := MaxByte - (R + G shl 1 {x2} + B) shr 2;
              C := Col;
              A := alpha;
            end;
        end
    else begin
      h := Bmp.Height - 1;
      dR := (TsColor(Col2).R - TsColor(Col).R) / h;
      dG := (TsColor(Col2).G - TsColor(Col).G) / h;
      dB := (TsColor(Col2).B - TsColor(Col).B) / h;

      newR := TsColor(Col).R;
      newG := TsColor(Col).G;
      newB := TsColor(Col).B;

      if (State = 0) and (FBlendValue <> MAXBYTE) then
        for Y := 0 to h do begin
          newC.R := Round(newR);
          newC.G := Round(newG);
          newC.B := Round(newB);
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          for X := 0 to Bmp.Width - 1 do
            with TsColor_(S[X]) do begin
              alpha := MaxByte - (R + G shl 1 {x2} + B) shr 2;
              if alpha = 0 then
                C := 0
              else
                C := newC.C;

              A := (alpha * FBlendValue) shr 8;
            end;

          newR := newR + dR;
          newG := newG + dG;
          newB := newB + dB;
        end
      else
        for Y := 0 to h do begin
          newC.R := Round(newR);
          newC.G := Round(newG);
          newC.B := Round(newB);
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          for X := 0 to Bmp.Width - 1 do
            with TsColor_(S[X]) do begin
              alpha := MaxByte - (R + G shl 1 {x2} + B) shr 2;
              if alpha = 0 then
                C := 0
              else
                C := newC.C;

              A := alpha;
            end;

          newR := newR + dR;
          newG := newG + dG;
          newB := newB + dB;
        end;
    end;

  DrawAddedGlyph;
end;


function TsCharImageList.FontIndex(const aFontName: acString): integer;
var
  i: integer;
begin
  for i := 0 to EmbeddedFonts.Count - 1 do
    if CompareText(EmbeddedFonts[i].FontName, aFontName) = 0 then begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;


procedure TsCharImageList.GenerateStdList(BaseColor: TColor = clNone);
var
  iPPI, i: integer;
  Bmp: TBitmap;
  b: boolean;
begin
//  ForeColor := BaseColor;
  // Check if default size was not initialized yet
  if (SavedHeight = 0) and (inherited Width = 16) and (inherited Height = 16) and not AcChanging then begin
    AcChanging := True;
    KillImgList;
    b := AcChanging;
    AcChanging := True;
    Width  := inherited Width;
    AcChanging := b;
    Height := inherited Height;
  end;
  if SavedHeight <> 0 then begin
    if not HandleAllocated then
      CreateImgList;

    if HandleAllocated then begin
      if AcChanging then
        iPPI := PixelsPerInch
      else
        if (DefaultManager = nil) or (DefaultManager.Options.ScaleMode = smVCL) then
          iPPI := DefaultPPI
        else
          iPPI := DefaultManager.Options.PixelsPerInch;

      AcChanging := True;
      inherited Clear;
//      ForeColor := clNone;
      for i := 0 to Items.Count - 1 do begin
        if Items[i].FColor <> clNone then // CustomColor
          ForeColor := clNone
        else
          if (BaseColor = clNone) and (DefaultManager <> nil) and DefaultManager.IsActive then
            ForeColor := acColorToRGB(clWindowText)
          else
            ForeColor := BaseColor;

        Bmp := CreateBitmap32(i, GetImageWidth(Self, I, iPPI), GetImageHeight(Self, I, iPPI), GetImageHeight(Self, -1, iPPI));
        AddAsIcon(Bmp, -1);
        FreeAndNil(Bmp);
      end;
      ForeColor := clNone;
      if Items.Count > 0 then
        StdListIsGenerated := True;

      AcChanging := False;
    end;
  end;
end;


function TsCharImageList.GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
var
  Bmp: TBitmap;
begin
  if HandleAllocated and (Image <> nil) and IsValidIndex(Index, Count) then
    if (GetImageWidth(Self, Index, PixelsPerInch) = Image.Width) and (GetImageHeight(Self, Index, PixelsPerInch) = Image.Height) then begin
      Bmp := Items[Index].GetImage(GlyphHeight);
      Image.Assign(Bmp);
      if not CanUseCache then
        Bmp.Free;
    end
    else begin
      Image.PixelFormat := pf32bit;
      if GlyphHeight = 0 then
        GlyphHeight := Image.Height;
      PrepareBmp(Image, Index, MkRect(Image), GlyphHeight, Items[Index].OffsetY * GlyphHeight div GetImageHeight(Self, -1, iff(PixelsPerInch <> 0, PixelsPerInch, 96)));
    end;

  Result := True;
end;


function TsCharImageList.GetBkColor: TColor;
begin
  if FBkColor = clNone then
    Result := inherited BkColor
  else
    Result := FBkColor;
end;


{$IFNDEF DELPHI_XE8}
function TsCharImageList.Count: Integer;
{$ELSE}
function TsCharImageList.GetCount: Integer;
{$ENDIF}
begin
  if Items <> nil then
    Result := Items.Count
  else
    Result := 0;
end;


function TsCharImageList.GetDimension(const Index: Integer): integer;
begin
  if Index = 0 then
    Result := inherited Height
  else
    Result := inherited Width;
end;


function TsCharImageList.ItemIsCached(Item: TacCharListItem; RequiredWidth, RequiredHeight, CharHeight: integer; RequiredColor: TColor): boolean;
begin
  Result := (State = Item.CachedState) and (Item.CacheBmp.Width = RequiredWidth) and (Item.CacheBmp.Height = RequiredHeight) and (Item.ActBlend = BlendValue);
  if Result then
    Result := Item.CacheBmp.Canvas.Font.Color = RequiredColor;
end;


procedure TsCharImageList.KillImgList;
begin
//  if HandleAllocated and not ShareImages then
//    ImageList_Destroy(Handle);
  Handle := 0;
  StdListIsGenerated := False;
  if not AcChanging then
    Change;
end;


procedure TsCharImageList.LoadAllFonts;
var
  i: integer;
begin
  for i := 0 to EmbeddedFonts.Count - 1 do
    EmbeddedFonts[i].LoadFont;
end;


procedure TsCharImageList.Loaded;
var
  w, h: integer;
begin
  inherited;
  LoadAllFonts;
  FLoaded := True;
  if SavedWidth = 0 then begin
    w := inherited Width;
    h := inherited Height;
    if (w = 16) and (h = 16) then begin // If default size - reinit
      AcChanging := True;
      KillImgList;
      SetDimension(1, w);
      SetDimension(0, h);
    end
    else
      KillImgList;
  end
  else
    KillImgList;

  if SavedWidth = 0 then
    SetDimension(1, Width);

  if SavedHeight = 0 then
    SetDimension(0, Height);

  Items.CheckItems;
  if not StdListIsGenerated then begin
    GenerateStdList;
    StdListIsGenerated := True; // Set the flag even if iconlist is empty
    Change;
  end;
end;


procedure TsCharImageList.SetBkColor(const Value: TColor);
begin
  FBkColor := Value;
  inherited BkColor := Value;
  if HandleAllocated then
    if FBkColor = clNone then
      ImageList_SetBkColor(Handle, CLR_NONE)
    else
      ImageList_SetBkColor(Handle, Value);
end;


procedure TsCharImageList.SetBlendingMode(const Value: TacBlendingMode);
begin
  if FBlendingMode <> Value then begin
    FBlendingMode := Value;
    if not (csLoading in ComponentState) and FLoaded and (Count <= Items.Count) and not AcChanging {If can generate new} then begin
      GenerateStdList;
      Change;
    end;
  end;
end;


procedure TsCharImageList.SetBlendValue(const Value: byte);
begin
  if FBlendValue <> Value then begin
    FBlendValue := Value;
    if not (csLoading in ComponentState) and FLoaded and (Count <= Items.Count) and not AcChanging {If can generate new} then begin
      GenerateStdList;
      Change;
    end;
  end;
end;


procedure TsCharImageList.SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
begin
  if IgnoreDefault then begin
    ForeColor := AColor;
    ForeColor2 := AColor;
  end
  else begin
    ForeColor2 := clNone;
    if (AIndex >= 0) and (Items[AIndex].Color <> clNone) then
      ForeColor := Items[AIndex].Color
    else
      ForeColor := AColor;
  end;
end;


procedure TsCharImageList.SetDimension(const Index, Value: integer);
begin
  case Index of
    0: begin
      if FLoaded or (csDesigning in ComponentState) then
        SavedHeight := Value;

      inherited Height := Value;
    end;

    1: begin
      if FLoaded or (csDesigning in ComponentState) then
        SavedWidth := Value;

      inherited Width := Value;
    end;
  end;
  if not (csLoading in ComponentState) and FLoaded and (Count <= Items.Count) and not AcChanging {If can generate new} then begin
    GenerateStdList;
    Change;
  end;
end;


procedure TsCharImageList.SetEmbeddedFonts(const Value: TacEmbeddedFonts);
begin
  FEmbeddedFonts.Assign(Value);
end;


procedure TsCharImageList.SetItems(const Value: TacCharListItems);
begin
  FItems.Assign(Value);
end;


procedure TsCharImageList.SetNewDimensions(Value: HImageList);
var
  AHeight, AWidth: Integer;
begin
  AWidth := Width;
  AHeight := Height;
  ImageList_GetIconSize(Value, AWidth, AHeight);
  Width := AWidth;
  Height := AHeight;
end;


procedure TsCharImageList.SetNewScale(Value: Integer);
var
  b: boolean;
begin
  if (FSavedScale <> Value) and CanScale then begin
    b := AcChanging;
    AcChanging := True;
    FSavedScale := Value;
    inherited Clear;
    KillImgList;
    GenerateStdList;
    AcChanging := b;
    if not AcChanging then
      inherited Change;
  end;
end;


procedure TsCharImageList.UnLoadAllFonts;
var
  i: integer;
begin
  for i := 0 to EmbeddedFonts.Count - 1 do
    EmbeddedFonts[i].UnloadFont;
end;


procedure TsCharImageList.UpdateStd(i: integer);
var
  Bmp: TBitmap;
begin
  ForeColor := clNone;
  Bmp := Items[i].GetImage;
  ImageList_Replace(Handle, i, Bmp.Handle, Bmp.MaskHandle);
  if not CanUseCache then
    Bmp.Free;
end;


procedure TacCharListItems.CheckItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FScalingFactor = 0 then begin
      Items[i].FScalingFactor := GetDefScaling(Items[i].Charset, Items[i].FontName);
      Items[i].Color := clNone;
    end;
end;


constructor TacCharListItems.Create(AOwner: TsCharImageList);
begin
  inherited Create(TacCharListItem);
  FOwner := AOwner;
end;


destructor TacCharListItems.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TacCharListItems.GetItem(Index: Integer): TacCharListItem;
begin
  Result := TacCharListItem(inherited GetItem(Index));
end;


function TacCharListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TacCharListItems.SetItem(Index: Integer; Value: TacCharListItem);
begin
  inherited SetItem(Index, Value);
end;


constructor TacImageList.Create(AOwner: TComponent);
begin
  inherited;
  PixelsPerInch := 96;
  ForeColor := clNone;
  IgnoreTransparency := False;
  if acListsArray = nil then
    acListsArray := TList.Create;

  acListsArray.Add(Self);
end;


procedure TacImageList.AddAsIcon(Bmp: TBitmap; Ndx: integer);
var
  Ico: hIcon;
  Size: integer;
  TmpBmp: TBitmap;
begin
  if Bmp <> nil then begin
    if ((Bmp.Width <> Width) or (Bmp.Height <> Height)) then begin
      Size := max(Width, Height);
      TmpBmp := CreateBmp32(Size, Size);
      FillRect32(TmpBmp, MkRect(TmpBmp), 0, 0);
      if (Bmp.Height > 1) and (Bmp.Width > 1) then
        Stretch(Bmp, TmpBmp, TmpBmp.Width * Size div Width, TmpBmp.Height * Size div Height, ftMitchell)
      else
        FillRect32(TmpBmp, MkRect(TmpBmp), 0, 1);
    end
    else
      TmpBmp := Bmp;

    if IsNTFamily and {$IFDEF DELPHI7UP}acThemesEnabled{$ELSE}False{$ENDIF} then
      Ico := MakeCompIcon(TmpBmp)
    else
      Ico := MakeIcon32(TmpBmp);

    if Ndx < 0 then
      ImageList_AddIcon(Handle, Ico)
    else
      ImageList_ReplaceIcon(Handle, Ndx, Ico);

    DestroyIcon(Ico);
    if TmpBmp <> Bmp then
      TmpBmp.Free;
  end;
end;


{$IFDEF DELPHI7UP}
procedure TacImageList.ReadData(Stream: TStream);
begin
// Data is duplicated in Items
end;

procedure TacImageList.WriteData(Stream: TStream);
  {$IFDEF DELPHI_XE}
var
  SA: TStreamAdapter;
  ComCtrlHandle: THandle;
  ImgLst: TImageList;
  {$ENDIF}
begin
  {$IFDEF DELPHI_XE}
  if not (csWriting in ComponentState) then begin
    // Code required for work of ActionList designer
    ComCtrlHandle := GetModuleHandle(comctl32);
    ImageListWriteExProc := GetProcAddress(ComCtrlHandle, 'ImageList_WriteEx');
    SA := TStreamAdapter.Create(Stream);
    try // Save empty bitmap, data is duplicated in Items
      ImgLst := TImageList.Create(Self);
      ImgLst.Width := 1;
      ImgLst.Height := 1;
      ImgLst.Masked := False;
      ImgLst.ColorDepth := cd4bit;
      if Assigned(ImageListWriteExProc) then
        ImageListWriteExProc(ImgLst.Handle, 1 {ILP_DOWNLEVEL}, SA)
      else
        ImageList_Write(ImgLst.Handle, SA);

      ImgLst.Free;
    finally
      SA.Free;
    end;
  end;
  {$ENDIF}
end;
{$ENDIF} // IFDEF DELPHI7UP


procedure TacEmbeddedFonts.Assign(Source: TPersistent);
var
  i: integer;
begin
  Clear;
{$IFNDEF NOFONTRES}
  if (TacEmbeddedFonts(Source).Count = 0) or (TacEmbeddedFonts(Source)[0].FontData.Size <> 0) then
    TacEmbeddedFont(Add).FontName := 'FontAwesome';
{$ENDIF}
  for i := 0 to TacEmbeddedFonts(Source).Count - 1 do
    TacEmbeddedFont(Add).Assign(TacEmbeddedFonts(Source)[i]);
end;


constructor TacEmbeddedFonts.Create(AOwner: TsCharImageList);
begin
  inherited Create(TacEmbeddedFont);
  FOwner := AOwner;
end;


destructor TacEmbeddedFonts.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TacEmbeddedFonts.GetItem(Index: Integer): TacEmbeddedFont;
begin
  Result := TacEmbeddedFont(inherited GetItem(Index));
end;


function TacEmbeddedFonts.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TacEmbeddedFonts.SetItem(Index: Integer; const Value: TacEmbeddedFont);
begin
  inherited SetItem(Index, Value);
end;


procedure TacEmbeddedFont.Assign(Source: TPersistent);
begin
  FFileName := TacEmbeddedFont(Source).FileName;
  FFontName := TacEmbeddedFont(Source).FontName;
  TacEmbeddedFont(Source).FontData.SaveToStream(FontData);
end;


procedure TacEmbeddedFont.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then
    Dest.Assign(Self)
  else
    inherited;
end;


constructor TacEmbeddedFont.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FontData := TMemoryStream.Create;
end;


procedure TacEmbeddedFont.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('FontData', ReadData, WriteData, True);
end;


destructor TacEmbeddedFont.Destroy;
begin
  FontData.Free;
  inherited Destroy;
end;


procedure TacEmbeddedFont.LoadFont;
var
  FontCount: DWord;
begin
  if FontData.Size > 0 then begin
    FontCount := 1;
    Handle := AddFontMemResourceEx(FontData.Memory, FontData.Size, nil, @FontCount);
  end
  else
    Handle := 0;
end;


procedure TacEmbeddedFont.ReadData(Reader: TStream);
begin
  FontData.LoadFromStream(Reader);
end;


procedure TacEmbeddedFont.UnLoadFont;
begin
  if Handle <> 0 then
    RemoveFontMemResourceEx(Handle);
end;


procedure TacEmbeddedFont.WriteData(Writer: TStream);
begin
  FontData.SaveToStream(Writer);
end;


destructor TacImageList.Destroy;
begin
  if acListsArray <> nil then
    acListsArray.Extract(Self);

  inherited;
end;


{$IFDEF ACDEBUG}
procedure TacImageList.SetForeColor(const Value: TColor);
begin
  FForeColor := Value;
end;
{$ENDIF}


{$IFNDEF NOFONTRES}
var
  ResStream: TResourceStream;
  FontsCount: DWord;
  awFont: THandle = 0;
{$ENDIF}


initialization

{$IFNDEF NOFONTRES}
{$R acFontData.res}
  if awFont = 0 then begin
    ResStream := TResourceStream.Create(hInstance, 'awFont', RT_RCDATA);
    awFont := AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FontsCount);
    ResStream.Free;
  end;
{$ENDIF}


finalization
{$IFNDEF NOFONTRES}
  if awFont <> 0 then
    RemoveFontMemResourceEx(awFont);
{$ENDIF}

  FreeAndNil(acListsArray);

end.
