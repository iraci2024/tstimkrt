{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2019 - 2021                               }
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

unit FMX.TMSFNCTypes;

{$I FMX.TMSFNCDefines.inc}

{$IFNDEF WEBLIB}
{$HINTS OFF}
{$IFDEF LCLLIB}
{$DEFINE USETRECTF}
{$ENDIF}
{$IFDEF VCLLIB}
{$IF COMPILERVERSION < 23}
{$DEFINE USETRECTF}
{$IFEND}
{$ENDIF}
{$HINTS ON}
{$ENDIF}

{$IFDEF WEBLIB}
{$DEFINE USETRECTF}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE FMXWEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE USEOWNEDCOLLECTION}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFNDEF LCLWEBLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 22}
{$DEFINE USEOWNEDCOLLECTION}
{$IFEND}
{$HINTS ON}
{$ENDIF}

{$DEFINE SVGSUPPORT}

{$IFNDEF LCLWEBLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 25}
{$DEFINE SUPPORTSHELPERS}
{$IFEND}
{$HINTS ON}
{$ELSE}
{$DEFINE SUPPORTSHELPERS}
{$ENDIF}

{$IFDEF FNCLIB}
{$DEFINE JSONSUPPORT}
{$ELSE}
{$HINTS OFF}
{$IF COMPILERVERSION > 27}
{$DEFINE JSONSUPPORT}
{$IFEND}
{$HINTS ON}
{$ENDIF}

interface

uses
  {$IFDEF VCLLIB}
  Windows,
  {$IFDEF SVGSUPPORT}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  Vcl.BaseImageCollection,
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  UITypes,
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  LCLType,
  {$ENDIF}
  Types, Classes, FMX.Graphics
  ,SysUtils, FMX.Controls, Math
  {$IFDEF WEBLIB}
  ,WEBLib.JSON
  {$ENDIF}
  {$IFDEF JSONSUPPORT}
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  ,JSON
  {$ELSE}
  ,DBXJSON
  {$IFEND}
  {$HINTS ON}
  {$ELSE}
  ,fpjson
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

type
  ITMSFNCProductInfo = interface
  ['{C53329EA-7B3A-4507-AD9E-88ACD6A85054}']
    function GetVersion: string;
    function GetDocURL: string;
    function GetTipsURL: string;
  end;

const
  TTMSFNCBaseDocURL = 'https://download.tmssoftware.com/doc/';
  TTMSFNCBaseTipsURL = 'https://www.tmssoftware.com/site/tmsfnccore.asp?s=faq';
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$IF COMPILERVERSION > 28}
  pidWeb = $10000;
  {$ELSE}
  pidWeb = $1000;
  {$IFEND}
  {$ENDIF}
  {$IFNDEF WEBLIB}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$HINTS OFF}
  TMSPlatformsDesktop = pidWin32 or pidWin64 or pidOSX32{$IF COMPILERVERSION > 32} or pidOSX64{$IFEND}{$IF COMPILERVERSION > 33} or pidLinux64{$IFEND};
  TMSPlatforms = pidWin32 or pidWin64 or pidOSX32{$IF COMPILERVERSION > 33} or pidLinux64{$IFEND} or {$IF COMPILERVERSION > 32}pidOSX64 or pidiOSSimulator32 or pidiOSSimulator64{$ELSE}pidiOSSimulator{$IFEND} or {$IF COMPILERVERSION > 28}pidiOSDevice32 or pidiOSDevice64{$ELSE}pidiOSDevice{$IFEND}
    or {$IF COMPILERVERSION > 32}{$IF COMPILERVERSION > 34}pidAndroidArm32 or pidAndroidArm64{$ELSE}pidAndroid32Arm or pidAndroid64Arm{$IFEND}{$ELSE}pidAndroid{$IFEND};
  {$HINTS ON}
  TMSPlatformsWeb = TMSPlatforms{$IFDEF FNCLIB} or pidWeb{$ENDIF};
  TMSPlatformsWebDesktop = TMSPlatformsDesktop{$IFDEF FNCLIB} or pidWeb{$ENDIF};
  KEY_CANCEL = VKCANCEL;
  KEY_CONTROL = VKCONTROL;
  KEY_SHIFT = VKSHIFT;
  KEY_ESCAPE = VKESCAPE;
  KEY_INSERT = VKINSERT;
  KEY_DELETE = VKDELETE;
  KEY_TAB = VKTAB;
  KEY_SUBTRACT = VKSUBTRACT;
  KEY_ADD = VKADD;
  KEY_MULTIPLY = VKMULTIPLY;
  KEY_DIVIDE = VKDIVIDE;
  KEY_PRIOR = VKPRIOR;
  KEY_NEXT = VKNEXT;
  KEY_UP = VKUP;
  KEY_DOWN = VKDOWN;
  KEY_RIGHT = VKRIGHT;
  KEY_LEFT = VKLEFT;
  KEY_HOME = VKHOME;
  KEY_END = VKEND;
  KEY_RETURN = VKRETURN;
  KEY_SPACE = VKSPACE;
  KEY_MENU = VKMENU;
  KEY_BACK = VKBACK;
  KEY_F1 = VKF1;
  KEY_F2 = VKF2;
  KEY_F3 = VKF3;
  KEY_F4 = VKF4;
  KEY_F5 = VKF5;
  KEY_F6 = VKF6;
  KEY_F7 = VKF7;
  KEY_F8 = VKF8;
  KEY_F9 = VKF9;
  KEY_F10 = VKF10;
  KEY_F11 = VKF11;
  KEY_F12 = VKF12;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$ENDIF}
  {$IF COMPILERVERSION > 22}
  TMSPlatformsDesktop = pidWin32 or pidWin64;
  TMSPlatforms = pidWin32 or pidWin64;
  TMSPlatformsWeb = TMSPlatforms{$IFDEF FNCLIB} or pidWeb{$ENDIF};
  TMSPlatformsWebDesktop = TMSPlatformsDesktop{$IFDEF FNCLIB} or pidWeb{$ENDIF};
  {$ELSE}
  ssCommand = ssCtrl;
  TMSPlatformsDesktop = 0;
  TMSPlatforms = 0;
  TMSPlatformsWeb = 0;
  TMSPlatformsWebDesktop = 0;
  {$IFEND}
  {$IFNDEF WEBLIB}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ssCommand = ssCtrl;
  {$ENDIF}
  KEY_CANCEL = VK_CANCEL;
  KEY_CONTROL = VK_CONTROL;
  KEY_SHIFT = VK_SHIFT;
  KEY_ESCAPE = VK_ESCAPE;
  KEY_INSERT = VK_INSERT;
  KEY_DELETE = VK_DELETE;
  KEY_TAB = VK_TAB;
  KEY_SUBTRACT = VK_SUBTRACT;
  KEY_ADD = VK_ADD;
  KEY_MULTIPLY = VK_MULTIPLY;
  KEY_DIVIDE = VK_DIVIDE;
  KEY_PRIOR = VK_PRIOR;
  KEY_NEXT = VK_NEXT;
  KEY_UP = VK_UP;
  KEY_DOWN = VK_DOWN;
  KEY_RIGHT = VK_RIGHT;
  KEY_LEFT = VK_LEFT;
  KEY_HOME = VK_HOME;
  KEY_END = VK_END;
  KEY_RETURN = VK_RETURN;
  KEY_SPACE = VK_SPACE;
  KEY_MENU = VK_MENU;
  KEY_BACK = VK_BACK;
  KEY_F1 = VK_F1;
  KEY_F2 = VK_F2;
  KEY_F3 = VK_F3;
  KEY_F4 = VK_F4;
  KEY_F5 = VK_F5;
  KEY_F6 = VK_F6;
  KEY_F7 = VK_F7;
  KEY_F8 = VK_F8;
  KEY_F9 = VK_F9;
  KEY_F10 = VK_F10;
  KEY_F11 = VK_F11;
  KEY_F12 = VK_F12;
  {$ENDIF}

{$IFDEF WEBLIB}
const
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';
{$ENDIF}

type
  {$IFDEF WEBLIB}
  TResourceStream = TObject;

  TTMSFNCStreamLoadedEvent = reference to procedure;

  TTMSFNCMemoryStream = class helper for TCustomMemoryStream
  private
    function GetBytes: TBytes;
  public
    procedure OpenFile(FileType: string);
    procedure SaveToFile(FileName: string);
    property Bytes: TBytes read GetBytes;
  end;

  PChar = string;
  {$ENDIF}

  {$IFDEF LCLLIB}
  TJSONValue = TJSONData;
  {$ENDIF}

  TTMSFNCMouseButton = TMouseButton;

  {$IFDEF USETRECTF}
  {$IFNDEF WEBLIB}
  PSizeF = ^TSizeF;
  {$ENDIF}
  TSizeF = record
    cx: Single;
    cy: Single;
  {$IFNDEF WEBLIB}
  public
    property Width: Single read cx write cx;
    property Height: Single read cy write cy;
  {$ENDIF}
  end;

  {$IFNDEF WEBLIB}
  TPointFType = array [0..1] of Single;
  PPointF = ^TPointF;
  {$ENDIF}
  TPointF = record
    {$IFDEF WEBLIB}
    X, Y: Single;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    function Length: Single;
    procedure Offset(const ADeltaX, ADeltaY: Single);
    case Integer of
      0: (V: TPointFType;);
      1: (X: Single;
          Y: Single;);
    {$ENDIF}
  end;

  {$IFNDEF WEBLIB}
  PRectF = ^TRectF;
  {$ENDIF}
  TRectF = record
  {$IFNDEF WEBLIB}
  private
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetSize: TSizeF;
    procedure SetSize(const Value: TSizeF);
  public
    procedure Offset(const DX, DY: Single);
    procedure Inflate(const DX, DY: Single);
    function CenterAt(const ADesignatedArea: TRectF): TRectF;
    function SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
    function FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
    function FitInto(const ADesignatedArea: TRectF): TRectF; overload;
    function IsEmpty: Boolean;
    function IntersectsWith(const R: TRectF): Boolean;
    function CenterPoint: TPointF;
    function Empty: TRectF;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Size: TSizeF read GetSize write SetSize;
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TPointF);
  {$ENDIF}
  {$IFDEF WEBLIB}
    Left, Top, Right, Bottom: Single;
  {$ENDIF}
  end;
  {$ENDIF}

  TTMSFNCBitmap = class;

  {$IFDEF FMXLIB}
  TTMSFNCSVGImportColor = TAlphaColor;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TTMSFNCSVGImportColor = TColor;
  {$ENDIF}

  TTMSFNCSVGImport = class(TPersistent)
  private
    FRotationAngle: Single;
    FViewRect: TRectF;
    FGrayScale: Boolean;
    FCustomFillColor: TTMSFNCSVGImportColor;
    FCustomStrokeColor: TTMSFNCSVGImportColor;
    FTintColor: TTMSFNCSVGImportColor;
    FCustomOpacity: Single;
  public
    function HasElements: Boolean; virtual; abstract;
    function ElementCount: Integer; virtual; abstract;
    function GenerateBitmap(AWidth: Single = - 1; AHeight: Single = -1): TTMSFNCBitmap; virtual; abstract;
    constructor Create; virtual;
    procedure Clear; virtual; abstract;
    procedure Draw(const ACanvas: TCanvas; ARect: TRectF; ANative: Boolean = False); virtual; abstract;
    procedure LoadFromText(const AText: string); virtual; abstract;
    procedure LoadFromFile(const AFile: string); virtual; abstract;
    procedure LoadFromStream(const AStream: TStream); virtual; abstract;
    procedure SaveToStream(const AStream: TStream); virtual; abstract;
    procedure SaveToFile(const AFile: string); virtual; abstract;
    property ViewRect: TRectF read FViewRect write FViewRect;
  published
    property GrayScale: Boolean read FGrayScale write FGrayScale default False;
    property CustomFillColor: TTMSFNCSVGImportColor read FCustomFillColor write FCustomFillColor;
    property CustomStrokeColor: TTMSFNCSVGImportColor read FCustomStrokeColor write FCustomStrokeColor;
    property TintColor: TTMSFNCSVGImportColor read FTintColor write FTintColor;
    property CustomOpacity: Single read FCustomOpacity write FCustomOpacity;
    property RotationAngle: Single read FRotationAngle write FRotationAngle;
  end;

  {$IFDEF CMNLIB}
  TTMSFNCSVGBitmap = class(TGraphic)
  strict private
    FSVG: TTMSFNCSVGImport;
    FStream: TMemoryStream;
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
    function GetEmpty: Boolean; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    {$IFDEF LCLLIB}
    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    function GetTransparent: Boolean; override;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    {$IFDEF VCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 32}
    class function CanLoadFromStream(Stream: TStream): Boolean; override;
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    {$IFDEF LCLLIB}
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    {$ENDIF}     
    function HasSVG: Boolean; virtual;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToFile(const FileName: String); override;
    property FileName: TFileName read FFileName write SetFileName;
    property SVG: TTMSFNCSVGImport read FSVG;
  end;

  {$IFNDEF FNCLIB}
  TTMSFNCSVGBitmap = class(TTMSFNCSVGBitmap);
  {$ENDIF}

  {$IFDEF SVGSUPPORT}
  {$IFDEF VCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 33}
  TTMSFNCSVGImageCollectionItem = class(TCollectionItem)
  private
    FRotationAngle: Single;
    FData: TPicture;
    FName: String;
    FGrayScale: Boolean;
    FCustomFillColor: TTMSFNCSVGImportColor;
    FCustomStrokeColor: TTMSFNCSVGImportColor;
    FTintColor: TTMSFNCSVGImportColor;
    FCustomOpacity: Single;
    procedure SetData(const Value: TPicture);
    procedure SetName(const Value: String);
    function IsTintColorStored: Boolean;
    function IsCustomFillColorStored: Boolean;
    function IsCustomStrokeColorStored: Boolean;
    function IsCustomOpacityStored: Boolean;
    function IsRotationAngleStored: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    destructor Destroy; override;
  published
    property Data: TPicture read FData write SetData;
    property Name: String read FName write SetName;
    property GrayScale: Boolean read FGrayScale write FGrayScale default False;
    property TintColor: TTMSFNCSVGImportColor read FTintColor write FTintColor stored IsTintColorStored nodefault;
    property CustomFillColor: TTMSFNCSVGImportColor read FCustomFillColor write FCustomFillColor stored IsCustomFillColorStored nodefault;
    property CustomStrokeColor: TTMSFNCSVGImportColor read FCustomStrokeColor write FCustomStrokeColor stored IsCustomStrokeColorStored nodefault;
    property CustomOpacity: Single read FCustomOpacity write FCustomOpacity stored IsCustomOpacityStored nodefault;
    property RotationAngle: Single read FRotationAngle write FRotationAngle stored IsRotationAngleStored nodefault;
  end;

  TTMSFNCSVGImageCollectionItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TTMSFNCSVGImageCollectionItem;
    procedure SetItem(Index: Integer; Value: TTMSFNCSVGImageCollectionItem);
  public
    function Add: TTMSFNCSVGImageCollectionItem;
    property Items[Index: Integer]: TTMSFNCSVGImageCollectionItem read GetItem write SetItem; default;
  end;

  TTMSFNCSVGImageCollection = class(TCustomImageCollection)
  private
    FImages: TTMSFNCSVGImageCollectionItems;
    procedure SetImages(const Value: TTMSFNCSVGImageCollectionItems);
  protected
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); overload; override;
    procedure Draw(ACanvas: TCanvas; X, Y: Integer; AIndex: Integer; AProportional: Boolean = False); overload; virtual;
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    function GetItemByIndex(AIndex: Integer): TTMSFNCSVGImageCollectionItem; virtual;
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; overload; override;
  published
    property Images: TTMSFNCSVGImageCollectionItems read FImages write SetImages;
  end;
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMXLIB}
  TTMSFNCDrawBitmap = TBitmap;
  TTMSFNCBitmapHelperClass = TBitmap;
  TTMSFNCBitmap = class(TBitmap)
  private
    FSVG: TTMSFNCSVGImport;
    procedure ReadSVG(Stream: TStream);
    procedure WriteSVG(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Draw(ACanvas: TCanvas; const Rect: TRectF); virtual;
    constructor CreateFromStream(const AStream: TStream); override;
    constructor CreateFromFile(const AFileName: string); override;
    function HasSVG: Boolean; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromBase64(const ABase64: string); virtual;
    procedure LoadFromSVG(const ASVG: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const AFilename: string); virtual;
    function SaveToBase64: string; virtual;
    property SVG: TTMSFNCSVGImport read FSVG;
  end;
  {$ENDIF}
  {$IFDEF CMNLIB}
  TTMSFNCDrawBitmap = TGraphic;
  TTMSFNCBitmapHelperClass = TPicture;
  TTMSFNCBitmap = class(TPicture)
  private
    function GetSVG: TTMSFNCSVGImport;
  public
    function HasSVG: Boolean; virtual;
    property SVG: TTMSFNCSVGImport read GetSVG;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCBitmap = class(TBitmap)
  public
    constructor CreateFromStream(const AStream: TStream); virtual;
  end;
  TTMSFNCDrawBitmap = TGraphic;
  TTMSFNCBitmapHelperClass = TGraphic;
  {$ENDIF}

  {$IFNDEF LCLLIB}
  {$IFNDEF USETRECTF}
  TRectFHelper = record helper for TRectF
    function CenterAt(const ADesignatedArea: TRectF): TRectF;
    function SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
    function FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
    function FitInto(const ADesignatedArea: TRectF): TRectF; overload;
  end;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF WEBLIB}
  TTMSFNCBitmapHelper = class helper for TTMSFNCBitmapHelperClass
    procedure LoadFromSVG(ASVG: string);
    procedure LoadFromBase64(ABase64: string);
    procedure LoadFromURL(AURL: String); overload;
    procedure LoadFromURL(AURL: String; {%H-}AInstance: NativeUInt); overload;
    procedure LoadFromResource(AResourceName: String); overload;
    procedure LoadFromResource(AResourceName: String; {%H-}AInstance: NativeUInt); overload;
    function SaveToBase64: string;
    {$IFDEF CMNLIB}
    {$IFDEF VCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION < 32}
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    class function CreateFromFile(AFileName: string): TTMSFNCBitmap; overload;
    class function CreateFromStream(AStream: TStream): TTMSFNCBitmap; overload;
    {$ENDIF}
    class function CreateFromResource(AResourceName: string): TTMSFNCBitmap; overload;
    class function CreateFromResource(AResourceName: string; {%H-}AInstance: NativeUInt): TTMSFNCBitmap; overload;
    class function CreateFromURL(AURL: string): TTMSFNCBitmap; overload;
    class function CreateFromURL(AURL: string; {%H-}AInstance: NativeUInt): TTMSFNCBitmap; overload;
    class function CreateFromBase64(ABase64: string): TTMSFNCBitmap;
    class function CreateFromSVG(ASVG: string): TTMSFNCBitmap;
  end;
  {$ENDIF}

  {$IFDEF SUPPORTSHELPERS}
  TTMSFNCObjectExcludePropertyListArray = array of string;

  TTMSFNCObjectHelper = class helper for TObject
  {$IFDEF JSONSUPPORT}
  private
    function ToJSONValue: TJSONValue;
  {$ENDIF}
    {$IFDEF JSONSUPPORT}
  public
    property JSONValue: TJSONValue read ToJSONValue;
    {$ENDIF}
    function ToJSON(AExcludedProperties: TTMSFNCObjectExcludePropertyListArray): string; overload;
    function ToJSON: string; overload;
    procedure FromJSON(const Value: string);
    property JSON: string read ToJSON write FromJSON;
    procedure Log;
    procedure SaveToJSONFile(const AFileName: string);
    procedure LoadFromJSONFile(const AFileName: string);
    procedure SaveToJSONStream(const AStream: TStream);
    procedure LoadFromJSONStream(const AStream: TStream);
  end;

  {$IFDEF JSONSUPPORT}
  TTMSFNCJSONValueHelper = class helper for TJSONValue
  private
    function GetProperties(AName: string): TJSONValue;
    function GetAsArray: TJSONArray;
    function GetAsString: string;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
  public
    property Properties[AName: string]: TJSONValue read GetProperties; default;
    property AsArray: TJSONArray read GetAsArray;
    property AsString: string read GetAsString;
    property AsDouble: Double read GetAsDouble;
    property AsInteger: Integer read GetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean;
    function Find(const APath: string): TJSONValue;
  end;

  TTMSFNCJSONArrayHelper = class helper for TJSONArray
  private
    function GetValues(AIndex: Integer): TJSONValue;
    function GetLength: Integer;
  public
    property Values[AIndex: Integer]: TJSONValue read GetValues; default;
    property Length: Integer read GetLength;
  end;
  {$ENDIF}
  {$ENDIF}

  TTMSFNCMargins = class(TPersistent)
  private
    FRight: Single;
    FBottom: Single;
    FTop: Single;
    FLeft: Single;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);
    function IsLeftStored: Boolean;
    function IsTopStored: Boolean;
    function IsRightStored: Boolean;
    function IsBottomStored: Boolean;
  protected
    procedure Changed;
  public
    constructor Create; overload;
    constructor Create(const ARect: TRectF); overload;
    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean;
    function Rect: TRectF;
    function PaddingRect(const R: TRectF): TRectF;
  published
    property Left: Single read FLeft write SetLeft stored IsLeftStored nodefault;
    property Top: Single read FTop write SetTop stored IsTopStored nodefault;
    property Right: Single read FRight write SetRight stored IsRightStored nodefault;
    property Bottom: Single read FBottom write SetBottom stored IsBottomStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTMSFNCPoint = class(TPersistent)
  private
    FY: Single;
    FX: Single;
    FOnChange: TNotifyEvent;
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    function IsXStored: Boolean;
    function IsYStored: Boolean;
  protected
    procedure Changed;
  public
    constructor Create; overload;
    constructor Create(const APoint: TPointF); overload;
    procedure Assign(Source: TPersistent); override;
  published
    property X: Single read FX write SetX stored IsXStored nodefault;
    property Y: Single read FY write SetY stored IsYStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFNDEF LIMITEDGRAPHICSMODE}
  TTMSFNCScaledBitmap = class(TCollectionItem)
  private
    FBitmap: TTMSFNCBitmap;
    FScale: Single;
    FBitmapName: string;
    function IsScaleStored: Boolean;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
    procedure SetScale(const Value: Single);
    procedure SetBitmapName(const Value: string);
  protected
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property BitmapName: string read FBitmapName write SetBitmapName;
    property Scale: Single read FScale write SetScale stored IsScaleStored nodefault;
  end;

  TTMSFNCScaledBitmaps = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItems(Index: Integer): TTMSFNCScaledBitmap;
    procedure SetItems(Index: Integer; const Value: TTMSFNCScaledBitmap);
    function GetBitmap(Scale: Single): TTMSFNCBitmap;
    procedure SetBitmap(Scale: Single; const Value: TTMSFNCBitmap);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Add(Scale: Single = 1.0): TTMSFNCScaledBitmap; overload;
    {$IFNDEF WEBLIB}
    function AddBitmapFromFile(FileName: string; Scale: Single = 1.0): TTMSFNCScaledBitmap;
    {$ENDIF}
    function AddBitmapFromResource(ResourceName: String; Scale: Single = 1.0): TTMSFNCScaledBitmap; overload;
    function AddBitmapFromResource(ResourceName: String; AInstance: NativeUInt; Scale: Single = 1.0): TTMSFNCScaledBitmap; overload;
    function AddBitmap(Bitmap: TTMSFNCBitmap; Scale: Single = 1.0): TTMSFNCScaledBitmap;
    function AddBitmapName(BitmapName: string; Scale: Single = 1.0): TTMSFNCScaledBitmap;
    function AddDrawBitmap(Bitmap: TTMSFNCDrawBitmap; Scale: Single = 1.0): TTMSFNCScaledBitmap;
    function Insert(Index: Integer): TTMSFNCScaledBitmap; overload;
    function Insert(Index: Integer; Scale: Single): TTMSFNCScaledBitmap; overload;
    function GetBitmapByScale(Scale: Single): TTMSFNCBitmap;
    function GetItemByScale(Scale: Single): TTMSFNCScaledBitmap;
    property Items[Index: Integer]: TTMSFNCScaledBitmap read GetItems write SetItems; default;
    property Bitmaps[Scale: Single]: TTMSFNCBitmap read GetBitmap write SetBitmap;
  end;
  {$ENDIF}

  TTMSFNCPictureFormat = (pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone);

  ITMSFNCCustomEditor = interface
    ['{CC0C60B7-75F3-47CE-8A7F-8005A19F12E8}']
    procedure SetText(AValue: String);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelLength(AValue: Integer);
    function GetTextLength: Integer;
  end;

  {$IFDEF USEOWNEDCOLLECTION}
  {$IFDEF LCLLIB}generic {$ENDIF}TTMSFNCOwnedCollection<T: class> = class(TOwnedCollection)
  public type
    TTMSFNCCollection = {$IFDEF LCLLIB}specialize {$ENDIF}TTMSFNCOwnedCollection<T>;

    TEnumerator = class
    private
      FIndex: Integer;
      FCol: TTMSFNCCollection;
    public
      constructor Create(ACol: TTMSFNCCollection);
      function GetCurrent: T;
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  public
    function GetEnumerator: TEnumerator;
    function GetItem(AIndex: Integer): T;
  end;
  {$ELSE}
  TTMSFNCOwnedCollection = class(TOwnedCollection);
  {$ENDIF}

  {$IFNDEF WEBLIB}
  TTMSFNCPersistent = class(TInterfacedPersistent);
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCPersistent = class(TPersistent);
  {$ENDIF}

  TTMSFNCValueRelationShip = TValueRelationShip;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
function ConvertToRectF(const Rect: TRect): TRectF; overload;
function ConvertToRectF(const Rect: TRectF): TRectF; overload;
function ConvertToRect(const Rect: TRectF): TRect; overload;
function ConvertToRect(const Rect: TRect): TRect; overload;
function ConvertToSizeF(const Size: TSize): TSizeF; overload;
function ConvertToSizeF(const Size: TSizeF): TSizeF; overload;
function ConvertToSize(const Size: TSizeF): TSize; overload;
function ConvertToSize(const Size: TSize): TSize; overload;
function ConvertToPointF(const Point: TPoint): TPointF; overload;
function ConvertToPointF(const Point: TPointF): TPointF; overload;
function ConvertToPoint(const Point: TPointF): TPoint; overload;
function ConvertToPoint(const Point: TPoint): TPoint; overload;
function OffsetRectEx(var R: TRect; DX, DY: Integer): Boolean; overload;
function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean; overload;
function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
function IntersectRectEx(const Rect1: TRectF; const Rect2: TRectF): Boolean; overload;
function IntersectRectEx(out Rect: TRectF; const R1, R2: TRectF): Boolean; overload;
function EqualRectEx(const R1, R2: TRectF): Boolean; overload;
function EqualRectEx(const R1, R2: TRect): Boolean; overload;
function RectWidthEx(const Rect: TRect): Integer; overload;
function RectWidthEx(const Rect: TRectF): Single; overload;
function RectHeightEx(const Rect: TRect): Integer; overload;
function RectHeightEx(const Rect: TRectF): Single; overload;
function RectCenterEx(var R: TRect; const B: TRect): TRect; overload;
function RectCenterEx(var R: TRectF; const B: TRectF): TRectF; overload;
function RectCenterAtEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF;
function RectSnapToPixelEx(const Rect: TRectF; const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF; overload;
function GetPointLength(const Point: TPointF): Single;
function MakeRectF(Left, Top, Width, Height: Single): TRectF;
function CenterPointEx(const R: TRectF): TPointF;
function CompareValueEx(const A, B: Extended; Epsilon: Extended = 0): TTMSFNCValueRelationship;
function RectIsEmpty(const R: TRectF): Boolean;
function EmptyRect: TRectF;
function RectIntersectsWithEx(const ARect: TRectF; const R: TRectF): Boolean;
{$IFDEF USETRECTF}
function RectF(Left, Top, Right, Bottom: Single): TRectF; {$IFNDEF WEBLIB}inline; {$ENDIF}overload;
function PointF(X, Y: Single): TPointF; {$IFNDEF WEBLIB}inline; {$ENDIF}overload;
{$ENDIF}
function BitmapToDrawBitmap(ABitmap: TTMSFNCBitmapHelperClass): TTMSFNCDrawBitmap; overload;
function IsBitmapEmpty(ABitmap: TTMSFNCBitmapHelperClass): Boolean;
function CharInArray(AChar: Char; ACharArray: array of char): Boolean;
function CharIsNumber(AChar: Char): Boolean;
function CharIsLetter(AChar: Char): Boolean;
function CharIsHex(AChar: Char): Boolean;
function CharIsLetterOrNumber(AChar: Char): Boolean;
{$IFDEF WEBLIB}
function VarToStr(const S: string): string;
function VarToDateTime(const S: string): TDateTime;
function AnsiPos(const Substr, S: string): Integer;
function AnsiUpperCase(const S: string): string;
function AnsiLowerCase(const S: string): string;
function HInstance: Integer;
{$ENDIF}
function CreateNewGUID: string;

{$IFDEF SVGSUPPORT}
{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 33}

{$IFNDEF FNCLIB}
{$R TMSFNCSVGImageCollection.dcr}
{$ENDIF}

{$IFDEF FNCLIB}
{$IFNDEF LCLLIB}
{$R 'TMSFNCSVGImageCollectionComp.dcr'}
{$ENDIF}
{$IFDEF LCLLIB}
{$R 'TMSFNCSVGImageCollectionCompSmall.dcr'}
{$ENDIF}
{$ENDIF}
procedure Register;
{$IFEND}
{$HINTS ON}
{$ENDIF}
{$ENDIF}

implementation

uses
  FMX.TMSFNCUtils, FMX.TMSFNCPersistence
  {$IFNDEF WEBLIB}
  {$IFDEF SVGSUPPORT}
  ,FMX.TMSFNCGraphicsSVGEngine
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,JS
  {$IFDEF ELECTRON}
  ,WEBLib.Electron, libelectron
  {$ENDIF}
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,AnsiStrings
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,PNGImage, JPEG, GifImg
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 24}
  ,Character
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF CMNLIB}
  ,Masks
  {$ENDIF}
  ;

{$IFDEF SVGSUPPORT}
{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 33}

{$IFDEF FNCLIB}
{$R 'TMSFNCSVGImageCollection.res'}
{$ENDIF}

procedure Register;
begin
  {$IFDEF SVGSUPPORT}
  RegisterComponents('TMS FNC UI', [TTMSFNCSVGImageCollection]);
  {$ENDIF}
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}
{$ENDIF}

{$IFDEF WEBLIB}
function VarToStr(const S: string): string;
begin
  Result := S;
end;

function VarToDateTime(const S: string): TDateTime;
begin
  Result := StrToDateTime(S);
end;

function HInstance: Integer;
begin
  Result := 0;
end;

function AnsiUpperCase(const S: string): string;
begin
  Result := UpperCase(S);
end;

function AnsiLowercase(const S: string): string;
begin
  Result := LowerCase(S);
end;

function AnsiPos(const Substr, S: string): Integer;
begin
  Result := Pos(SubStr, S);
end;
{$ENDIF}

function CreateNewGUID: string;
{$IFDEF LCLWEBLIB}
var
  g: TGUID;
{$ENDIF}
begin
  {$IFNDEF LCLWEBLIB}
  Result := TGuid.NewGuid.ToString;
  {$ENDIF}
  {$IFDEF LCLWEBLIB}
  CreateGUID(g);
  Result := GUIDToString(g);
  {$ENDIF}
end;

function CharIsHex(AChar: Char): Boolean;
begin
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$WARNINGS OFF}
  {$IF COMPILERVERSION > 24}
  Result := AChar.IsNumber or AChar.IsInArray(['a', 'b', 'c', 'd', 'e', 'f']) or AChar.IsInArray(['A', 'B', 'C', 'D', 'E', 'F']);
  {$ELSE}
  Result := (AChar in ['0'..'9']) or (AChar in ['a'..'f']) or (AChar in ['A'..'F']);
  {$IFEND}
  {$WARNINGS ON}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLWEBLIB}
  Result := (AChar in ['0'..'9']) or (AChar in ['a'..'f']) or (AChar in ['A'..'F']);
  {$ENDIF}
end;

function CharIsLetter(AChar: Char): Boolean;
begin
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$WARNINGS OFF}
  {$IF COMPILERVERSION > 24}
  Result := AChar.IsLetter;
  {$ELSE}
  Result := (AChar in ['a'..'z']) or (AChar in ['A'..'Z']);
  {$IFEND}
  {$WARNINGS ON}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLWEBLIB}
  Result := (AChar in ['a'..'z']) or (AChar in ['A'..'Z']);
  {$ENDIF}
end;

function CharIsNumber(AChar: Char): Boolean;
begin
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$WARNINGS OFF}
  {$IF COMPILERVERSION > 24}
  Result := AChar.IsNumber;
  {$ELSE}
  Result := (AChar in ['0'..'9']);
  {$IFEND}
  {$WARNINGS ON}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLWEBLIB}
  Result := (AChar in ['0'..'9']);
  {$ENDIF}
end;

function CharIsLetterOrNumber(AChar: Char): Boolean;
begin
  Result := CharIsLetter(AChar) or CharIsNumber(AChar);
end;

function CharInArray(AChar: Char; ACharArray: array of char): Boolean;
var
  ch: Char;
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ACharArray) - 1 do
  begin
    ch := ACharArray[I];
    if ch = AChar then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsBitmapEmpty(ABitmap: TTMSFNCBitmapHelperClass): Boolean;
var
  b: TTMSFNCDrawBitmap;
begin
  Result := True;
  if Assigned(ABitmap) then
  begin
    b := BitmapToDrawBitmap(ABitmap);
    if Assigned(b) then
    begin
      {$IFDEF CMNLIB}
      Result := b.Empty;
      {$ENDIF}
      {$IFDEF WEBLIB}
      Result := b.Empty;
      {$ENDIF}
      {$IFDEF FMXLIB}
      Result := b.IsEmpty
      {$ENDIF}
    end;
  end;
end;

function BitmapToDrawBitmap(ABitmap: TTMSFNCBitmapHelperClass): TTMSFNCDrawBitmap;
begin
  Result := nil;
  if Assigned(ABitmap) then
  begin
    {$IFDEF CMNLIB}
    Result := ABitmap.Graphic;
    {$ENDIF}
    {$IFDEF WEBLIB}
    Result := ABitmap;
    {$ENDIF}
    {$IFDEF FMXLIB}
    Result := ABitmap;
    {$ENDIF}
  end;
end;

{$IFNDEF WEBLIB}

{ TTMSFNCBitmapHelper }

class function TTMSFNCBitmapHelper.CreateFromResource(AResourceName: String): TTMSFNCBitmap;
begin
  Result := CreateFromResource(AResourceName, TTMSFNCUtils.GetHInstance);
end;

class function TTMSFNCBitmapHelper.CreateFromResource(AResourceName: String; AInstance: NativeUInt): TTMSFNCBitmap;
begin
  Result := TTMSFNCBitmap.Create;
  Result.LoadFromResource(AResourceName, AInstance);
end;

class function TTMSFNCBitmapHelper.CreateFromBase64(ABase64: string): TTMSFNCBitmap;
begin
  Result := TTMSFNCBitmap.Create;
  Result.LoadFromBase64(ABase64);
end;

class function TTMSFNCBitmapHelper.CreateFromSVG(ASVG: string): TTMSFNCBitmap;
begin
  Result := TTMSFNCBitmap.Create;
  Result.LoadFromSVG(ASVG);
end;

class function TTMSFNCBitmapHelper.CreateFromURL(AURL: String): TTMSFNCBitmap;
begin
  Result := CreateFromURL(AURL, TTMSFNCUtils.GetHInstance);
end;

class function TTMSFNCBitmapHelper.CreateFromURL(AURL: String; AInstance: NativeUInt): TTMSFNCBitmap;
begin
  Result := TTMSFNCBitmap.Create;
  Result.LoadFromURL(AURL, AInstance);
end;

{$IFDEF CMNLIB}
{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION < 32}
procedure TTMSFNCBitmapHelper.SaveToStream(AStream: TStream);
begin
  if Graphic <> nil then Graphic.SaveToStream(AStream);
end;

procedure TTMSFNCBitmapHelper.LoadFromStream(AStream: TStream);
var
  GraphicClass: TGraphicClass;
  ms: TMemoryStream;
  ng: TGraphic;
begin
  ms := TMemoryStream.Create;
  try
    AStream.Position := 0;
    ms.LoadFromStream(AStream);
    ms.Position := 0;
    if TTMSFNCUtils.FindGraphicClass(ms.Memory^, ms.Size, GraphicClass) then
    begin
      if GraphicClass <> nil then
      begin
        ng := GraphicClass.Create;
        try
          ng.LoadFromStream(ms);
          Graphic := ng;
        finally
          ng.Free;
        end;
      end;
    end;
  finally
    ms.Free;
  end;
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}

class function TTMSFNCBitmapHelper.CreateFromFile(AFileName: string): TTMSFNCBitmap;
begin
  Result := TTMSFNCBitmap.Create;
  Result.LoadFromFile(AFileName);
end;

class function TTMSFNCBitmapHelper.CreateFromStream(AStream: TStream): TTMSFNCBitmap;
begin
  Result := TTMSFNCBitmap.Create;
  Result.LoadFromStream(AStream);
end;
{$ENDIF}

function TTMSFNCBitmapHelper.SaveToBase64: string;
begin
  Result := TTMSFNCUtils.SaveBitmapToBase64(Self);
end;

procedure TTMSFNCBitmapHelper.LoadFromBase64(ABase64: string);
begin
  TTMSFNCUtils.LoadBitmapFromBase64(ABase64, Self);
end;

procedure TTMSFNCBitmapHelper.LoadFromSVG(ASVG: string);
var
  ms: TStringStream;
begin
  ms := TStringStream.Create(ASVG);
  try
    if Assigned(ms) then
    begin
      ms.Position := 0;
      (Self as TTMSFNCBitmap).LoadFromStream(ms);
    end;
  finally
    if Assigned(ms) then
      ms.Free;
  end;
end;

procedure TTMSFNCBitmapHelper.LoadFromURL(AURL: String);
begin
  LoadFromURL(AURL, TTMSFNCUtils.GetHInstance);
end;

procedure TTMSFNCBitmapHelper.LoadFromURL(AURL: String; AInstance: NativeUInt);
var
  ms: TMemoryStream;
begin
  if Pos('HTTP', Uppercase(AURL)) <> 1 then
  begin
    LoadFromFile(AURL);
    Exit;
  end;

  ms := TTMSFNCUtils.DownloadImage(AURL);
  try
    if Assigned(ms) then
    begin
      ms.Position := 0;
      (Self as TTMSFNCBitmap).LoadFromStream(ms);
    end;
  finally
    if Assigned(ms) then
      ms.Free;
  end;
end;

procedure TTMSFNCBitmapHelper.LoadFromResource(AResourceName: String);
begin
  LoadFromResource(AResourceName, TTMSFNCUtils.GetHInstance);
end;

procedure TTMSFNCBitmapHelper.LoadFromResource(AResourceName: String; AInstance: NativeUInt);
var
  r: TResourceStream;
begin
  r := nil;
  try
    r := TTMSFNCUtils.GetResourceStream(AResourceName, AInstance);
    if Assigned(r) then
    begin
      if Self is TTMSFNCBitmap then
        (Self as TTMSFNCBitmap).LoadFromStream(r)
      else
        Self.LoadFromStream(r);
    end;
  finally
    if Assigned(r) then
      r.Free;
  end;
end;
{$ENDIF}

{$IFDEF USETRECTF}

{ TRectF }

{$IFNDEF WEBLIB}
function TRectF.GetSize: TSizeF;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

procedure TRectF.SetSize(const Value: TSizeF);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

function TRectF.GetHeight: Single;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TRectF.SetHeight(const Value: Single);
begin
  Self.Bottom := Self.Top + Value;
end;

function TRectF.GetWidth: Single;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectF.SetWidth(const Value: Single);
begin
  Self.Right := Self.Left + Value;
end;

function TRectF.CenterPoint: TPointF;
begin
  Result.X := (Right - Left) / 2.0 + Left;
  Result.Y := (Bottom - Top) / 2.0 + Top;
end;

function TRectF.IntersectsWith(const R: TRectF): Boolean;
begin
  Result := not ( (Self.BottomRight.X < R.TopLeft.X) or
                  (Self.BottomRight.Y < R.TopLeft.Y) or
                  (R.BottomRight.X < Self.TopLeft.X) or
                  (R.BottomRight.Y < Self.TopLeft.Y) );
end;

procedure TRectF.Offset(const DX, DY: Single);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

procedure TRectF.Inflate(const DX, DY: Single);
begin
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
end;

function TRectF.Empty: TRectF;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (Right < Left) or SameValue(Right, Left)
         or (Bottom < Top) or SameValue(Bottom, Top);
end;
{$ENDIF}

{$IFNDEF WEBLIB}
procedure TPointF.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

function TPointF.Length: Single;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;
{$ENDIF}

function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function OffsetRectEx(var R: TRect; DX, DY: Integer): Boolean;
begin
  {$IFNDEF WEBLIB}
  if @R <> nil then
  {$ENDIF}
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  {$IFNDEF WEBLIB}
  else
    Result := False;
  {$ENDIF}
end;

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
begin
  {$IFNDEF WEBLIB}
  if @R <> nil then
  {$ENDIF}
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  {$IFNDEF WEBLIB}
  else
    Result := False;
  {$ENDIF}
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
begin
  R.Left := R.Left - DX;
  R.Right := R.Right + DX;
  R.Top := R.Top - DY;
  R.Bottom := R.Bottom + DY;
end;

function IntersectRectEx(const Rect1: TRectF; const Rect2: TRectF): Boolean;
begin
  Result := (Rect1.Left < Rect2.Right)
        and (Rect1.Right > Rect2.Left)
        and (Rect1.Top < Rect2.Bottom)
        and (Rect1.Bottom > Rect2.Top);
end;

function EqualRectEx(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

function EqualRectEx(const R1, R2: TRectF): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

{$ELSE}

function EqualRectEx(const R1, R2: TRect): Boolean;
begin
  Result := EqualRect(R1, R2);
end;

function EqualRectEx(const R1, R2: TRectF): Boolean;
begin
  Result := EqualRect(R1, R2);
end;

function IntersectRectEx(const Rect1: TRectF; const Rect2: TRectF): Boolean;
begin
  Result := IntersectRect(Rect1, Rect2);
end;

function OffsetRectEx(var R: TRect; DX, DY: Integer): Boolean;
begin
  Result := OffsetRect(R, DX, DY);
end;

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
begin
  Result := OffsetRect(R, DX, DY);
end;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
begin
  InflateRect(R, DX, DY);
end;

function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := PtInRect(Rect, P);
end;

{$ENDIF}

{$IFNDEF WEBLIB}
function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) then
  begin
    Ratio := 1;
    Exit(Self);
  end;

  if (Self.Width / ADesignatedArea.Width) > (Self.Height / ADesignatedArea.Height) then
    Ratio := Self.Width / ADesignatedArea.Width
  else
    Ratio := Self.Height / ADesignatedArea.Height;

  Result := RectF(0, 0, Self.Width / Ratio, Self.Height / Ratio);
  RectCenterEx(Result, ADesignatedArea);
end;

function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.FitInto(const ADesignatedArea: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result := FitInto(ADesignatedArea, Ratio);
end;

function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.CenterAt(const ADesignatedArea: TRectF): TRectF;
begin
  Result := Self;
  RectCenterEx(Result, ADesignatedArea);
end;

function {$IFDEF USETRECTF}TRectF{$ELSE}{$IFNDEF LCLLIB}TRectFHelper{$ELSE}TRectF{$ENDIF}{$ENDIF}.SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
var
  LScale, HalfPixel: Single;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := System.Trunc(Self.Left * LScale) / LScale;
  Result.Top := System.Trunc(Self.Top * LScale) / LScale;
  Result.Width := System.Round(Self.Width * LScale) / LScale;
  Result.Height := System.Round(Self.Height * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    Result.Offset(HalfPixel, HalfPixel);
  end;
end;
{$ENDIF}

function RectWidthEx(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeightEx(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectWidthEx(const Rect: TRectF): Single;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeightEx(const Rect: TRectF): Single;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectCenterAtEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF;
begin
  Result := Rect;
  RectCenterEx(Result, ADesignatedArea);
end;

function RectSnapToPixelEx(const Rect: TRectF; const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
var
  LScale, HalfPixel: Single;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := Trunc(Rect.Left * LScale) / LScale;
  Result.Top := Trunc(Rect.Top * LScale) / LScale;
  Result.Right := Result.Left + Round((Rect.Right - Rect.Left) * LScale) / LScale;
  Result.Bottom := Result.Top + Round((Rect.Bottom - Rect.Top) * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    OffsetRectEx(Result, HalfPixel, HalfPixel);
  end;
end;

function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF; out Ratio: Single): TRectF;
begin
  if (ADesignatedArea.Right - ADesignatedArea.Left <= 0) or (ADesignatedArea.Bottom - ADesignatedArea.Top <= 0) then
  begin
    Ratio := 1;
    Exit(Rect);
  end;

  if ((Rect.Right - Rect.Left) / (ADesignatedArea.Right - ADesignatedArea.Left)) > ((Rect.Bottom - Rect.Top) / (ADesignatedArea.Bottom - ADesignatedArea.Top)) then
    Ratio := (Rect.Right - Rect.Left) / (ADesignatedArea.Right - ADesignatedArea.Left)
  else
    Ratio := (Rect.Bottom - Rect.Top) / (ADesignatedArea.Bottom - ADesignatedArea.Top);

  Result := RectF(0, 0, (Rect.Right - Rect.Left) / Ratio, (Rect.Bottom - Rect.Top) / Ratio);
  RectCenterEx(Result, ADesignatedArea);
end;

function RectFitIntoEx(const Rect: TRectF; const ADesignatedArea: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result := RectFitIntoEx(Rect, ADesignatedArea, Ratio);
end;

function RectCenterEx(var R: TRect; const B: TRect): TRect;
begin
  OffsetRectEx(R, -R.Left, -R.Top);
  OffsetRectEx(R, (RectWidthEx(B) - RectWidthEx(R)) div 2, (RectHeightEx(B) - RectHeightEx(R)) div 2);
  OffsetRectEx(R, B.Left, B.Top);
  Result := R;
end;

function RectCenterEx(var R: TRectF; const B: TRectF): TRectF;
begin
  OffsetRectEx(R, -R.Left, -R.Top);
  OffsetRectEx(R, Round((RectWidthEx(B) - RectWidthEx(R)) / 2), Round((RectHeightEx(B) - RectHeightEx(R)) / 2));
  OffsetRectEx(R, B.Left, B.Top);
  Result := R;
end;

function IntersectRectEx(out Rect: TRectF; const R1, R2: TRectF): Boolean;
var
  tmpRect: TRectF;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  {$IFDEF WEBLIB}
  Result := not RectIsEmpty(tmpRect);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := not tmpRect.IsEmpty;
  {$ENDIF}
  if not Result then
  begin
    tmpRect.Top := 0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

function EmptyRect: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
end;

function RectIsEmpty(const R: TRectF): Boolean;
begin
  Result := (R.Right < R.Left) or SameValue(R.Right, R.Left)
         or (R.Bottom < R.Top) or SameValue(R.Bottom, R.Top);
end;

function CompareValueEx(const A, B: Extended; Epsilon: Extended = 0): TTMSFNCValueRelationShip;
begin
  if SameValue(A, B, Epsilon) then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

function RectIntersectsWithEx(const ARect: TRectF; const R: TRectF): Boolean;
begin
  Result := (ARect.Left < R.Right)
        and (ARect.Right > R.Left)
        and (ARect.Top < R.Bottom)
        and (ARect.Bottom > R.Top);
end;

function CenterPointEx(const R: TRectF): TPointF;
begin
  Result.X := (R.Right - R.Left) / 2.0 + R.Left;
  Result.Y := (R.Bottom - R.Top) / 2.0 + R.Top;
end;

function MakeRectF(Left, Top, Width, Height: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  {$IFDEF WEBLIB}
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result.Width := Width;
  Result.Height := Height;
  {$ENDIF}
end;

function GetPointLength(const Point: TPointF): Single;
begin
  Result := Sqrt(Sqr(Point.X) + Sqr(Point.Y));
end;

function ConvertToRect(const Rect: TRect): TRect;
begin
  Result := Rect;
end;

function ConvertToRect(const Rect: TRectF): TRect;
begin
  Result.Left := Round(Rect.Left);
  Result.Top := Round(Rect.Top);
  Result.Right := Round(Rect.Right);
  Result.Bottom := Round(Rect.Bottom);
end;

function ConvertToRectF(const Rect: TRectF): TRectF;
begin
  Result := Rect;
end;

function ConvertToRectF(const Rect: TRect): TRectF;
begin
  Result.Left := Rect.Left;
  Result.Top := Rect.Top;
  Result.Right := Rect.Right;
  Result.Bottom := Rect.Bottom;
end;

function ConvertToPoint(const Point: TPoint): TPoint;
begin
  Result := Point;
end;

function ConvertToPoint(const Point: TPointF): TPoint;
begin
  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;

function ConvertToPointF(const Point: TPointF): TPointF;
begin
  Result := Point;
end;

function ConvertToPointF(const Point: TPoint): TPointF;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

function ConvertToSize(const Size: TSize): TSize;
begin
  Result := Size;
end;

function ConvertToSize(const Size: TSizeF): TSize;
begin
  Result.cx := Round(Size.cx);
  Result.cy := Round(Size.cy);
end;

function ConvertToSizeF(const Size: TSizeF): TSizeF;
begin
  Result := Size;
end;

function ConvertToSizeF(const Size: TSize): TSizeF;
begin
  Result.cx := Size.cx;
  Result.cy := Size.cy;
end;

{ TTMSFNCMargins }

procedure TTMSFNCMargins.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCMargins then
  begin
    FLeft := (Source as TTMSFNCMargins).Left;
    FTop := (Source as TTMSFNCMargins).Top;
    FRight := (Source as TTMSFNCMargins).Right;
    FBottom := (Source as TTMSFNCMargins).Bottom;
  end;
end;

procedure TTMSFNCMargins.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TTMSFNCMargins.Create(const ARect: TRectF);
begin
  FLeft := ARect.Left;
  FBottom := ARect.Bottom;
  FRight := ARect.Right;
  FTop := ARect.Top;
end;

constructor TTMSFNCMargins.Create;
begin
  FLeft := 0;
  FBottom := 0;
  FRight := 0;
  FTop := 0;
end;

function TTMSFNCMargins.Empty: Boolean;
var
  r: TRectF;
begin
  r := RectF(Left, Top, Right, Bottom);
  {$IFDEF WEBLIB}
  Result := RectIsEmpty(R);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := R.IsEmpty;
  {$ENDIF}
end;

procedure TTMSFNCMargins.SetBottom(const Value: Single);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TTMSFNCMargins.SetLeft(const Value: Single);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TTMSFNCMargins.SetRight(const Value: Single);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TTMSFNCMargins.SetTop(const Value: Single);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

function TTMSFNCMargins.IsLeftStored: Boolean;
begin
  Result := Left <> 0;
end;

function TTMSFNCMargins.IsTopStored: Boolean;
begin
  Result := Top <> 0;
end;

function TTMSFNCMargins.PaddingRect(const R: TRectF): TRectF;
begin
  Result := RectF(R.Left + FLeft, R.Top + FTop, R.Right - FRight, R.Bottom - FBottom);
end;

function TTMSFNCMargins.Rect: TRectF;
begin
  Result := RectF(Left, Top, Right, Bottom);
end;

function TTMSFNCMargins.IsBottomStored: Boolean;
begin
  Result := Bottom <> 0;
end;

function TTMSFNCMargins.IsRightStored: Boolean;
begin
  Result := Right <> 0;
end;

{ TTMSFNCPoint }

procedure TTMSFNCPoint.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCPoint then
  begin
    FX := (Source as TTMSFNCPoint).X;
    FY := (Source as TTMSFNCPoint).Y;
  end;
end;

procedure TTMSFNCPoint.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TTMSFNCPoint.Create(const APoint: TPointF);
begin
  FX := APoint.X;
  FY := APoint.Y;
end;

constructor TTMSFNCPoint.Create;
begin
  FX := 0;
  FY := 0;
end;

procedure TTMSFNCPoint.SetX(const Value: Single);
begin
  if FX <> Value then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TTMSFNCPoint.SetY(const Value: Single);
begin
  if FY <> Value then
  begin
    FY := Value;
    Changed;
  end;
end;

function TTMSFNCPoint.IsXStored: Boolean;
begin
  Result := X <> 0;
end;

function TTMSFNCPoint.IsYStored: Boolean;
begin
  Result := Y <> 0;
end;

{$IFNDEF LIMITEDGRAPHICSMODE}

{ TTMSFNCScaledBitmap }

procedure TTMSFNCScaledBitmap.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCScaledBitmap then
  begin
    FBitmap.Assign((Source as TTMSFNCScaledBitmap).Bitmap);
    FBitmapName := (Source as TTMSFNCScaledBitmap).BitmapName;
    FScale := (Source as TTMSFNCScaledBitmap).Scale;
  end;
end;

procedure TTMSFNCScaledBitmap.BitmapChanged(Sender: TObject);
begin
  Changed(False);
end;

constructor TTMSFNCScaledBitmap.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TTMSFNCBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FScale := 1.0;
end;

destructor TTMSFNCScaledBitmap.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TTMSFNCScaledBitmap.IsScaleStored: Boolean;
begin
  Result := Scale <> 1.0;
end;

procedure TTMSFNCScaledBitmap.SetBitmap(const Value: TTMSFNCBitmap);
begin
  if FBitmap <> Value then
  begin
    FBitmap.Assign(Value);
    Changed(False);
  end;
end;

procedure TTMSFNCScaledBitmap.SetBitmapName(const Value: string);
begin
  if FBitmapName <> Value then
  begin
    FBitmapName := Value;
    Changed(False);
  end;
end;

procedure TTMSFNCScaledBitmap.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    Changed(False);
  end;
end;

{ TTMSFNCScaledBitmaps }

function TTMSFNCScaledBitmaps.Add(Scale: Single = 1.0): TTMSFNCScaledBitmap;
begin
  Result := TTMSFNCScaledBitmap(inherited Add);
  Result.Scale := Scale;
end;

function TTMSFNCScaledBitmaps.AddBitmap(Bitmap: TTMSFNCBitmap; Scale: Single = 1.0): TTMSFNCScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.Assign(Bitmap);
end;

{$IFNDEF WEBLIB}
function TTMSFNCScaledBitmaps.AddBitmapFromFile(FileName: string;
  Scale: Single): TTMSFNCScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.LoadFromFile(FileName);
end;
{$ENDIF}

function TTMSFNCScaledBitmaps.AddBitmapFromResource(ResourceName: String; Scale: Single = 1.0): TTMSFNCScaledBitmap;
begin
  {$IFDEF WEBLIB}
  Result := AddBitmapFromResource(ResourceName, 0, Scale);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := AddBitmapFromResource(ResourceName, HInstance, Scale);
  {$ENDIF}
end;

function TTMSFNCScaledBitmaps.AddBitmapFromResource(ResourceName: String; AInstance: NativeUInt; Scale: Single = 1.0): TTMSFNCScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.LoadFromResource(ResourceName, AInstance);
end;

function TTMSFNCScaledBitmaps.AddBitmapName(BitmapName: string; Scale: Single = 1.0): TTMSFNCScaledBitmap;
begin
  Result := Add(Scale);
  Result.BitmapName := BitmapName;
end;

function TTMSFNCScaledBitmaps.AddDrawBitmap(Bitmap: TTMSFNCDrawBitmap;
  Scale: Single): TTMSFNCScaledBitmap;
begin
  Result := Add(Scale);
  Result.Bitmap.Assign(Bitmap);
end;

constructor TTMSFNCScaledBitmaps.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTMSFNCScaledBitmap);
end;

function TTMSFNCScaledBitmaps.GetBitmap(Scale: Single): TTMSFNCBitmap;
begin
  Result := GetBitmapByScale(Scale);
end;

function TTMSFNCScaledBitmaps.GetBitmapByScale(
  Scale: Single): TTMSFNCBitmap;
var
  it: TTMSFNCScaledBitmap;
begin
  Result := nil;
  it := GetItemByScale(Scale);
  if Assigned(it) then
    Result := it.Bitmap;
end;

function TTMSFNCScaledBitmaps.GetItemByScale(Scale: Single): TTMSFNCScaledBitmap;
var
  I: Integer;
  cl: TTMSFNCScaledBitmap;
  it: TTMSFNCScaledBitmap;
  mx: Single;
begin
  Result := nil;
  cl := nil;
  mx := 0;
  for I := 0 to Count - 1 do
  begin
    it := Items[I];
    if it.Scale = Scale then
    begin
      Result := it;
      Break;
    end
    else if (it.Scale <> Scale) and (it.Scale > mx) then
    begin
      cl := it;
      mx := it.Scale;
    end;
  end;

  if Result = nil then
    Result := cl;
end;

function TTMSFNCScaledBitmaps.GetItems(Index: Integer): TTMSFNCScaledBitmap;
begin
  Result := TTMSFNCScaledBitmap(inherited Items[Index]);
end;

function TTMSFNCScaledBitmaps.Insert(Index: Integer;
  Scale: Single): TTMSFNCScaledBitmap;
begin
  Result := TTMSFNCScaledBitmap(inherited Insert(Index));
  Result.Scale := Scale;
end;

function TTMSFNCScaledBitmaps.Insert(Index: integer): TTMSFNCScaledBitmap;
begin
  Result := TTMSFNCScaledBitmap(inherited Insert(Index));
end;

procedure TTMSFNCScaledBitmaps.SetBitmap(Scale: Single;
  const Value: TTMSFNCBitmap);
var
  b: TTMSFNCScaledBitmap;
begin
  b := GetItemByScale(Scale);
  if not Assigned(b) then
    b := Add(Scale);

  b.Bitmap.Assign(Value);
end;

procedure TTMSFNCScaledBitmaps.SetItems(Index: Integer; const Value: TTMSFNCScaledBitmap);
begin
  inherited Items[Index] := Value;
end;

procedure TTMSFNCScaledBitmaps.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$ENDIF}

{$IFDEF WEBLIB}
function TTMSFNCMemoryStream.GetBytes: TBytes;
var
  p: Int64;
  b: TBytes;
begin
  p := Position;
  Position := 0;
  SetLength(b, Size);
  Read(b, 0, Length(b));
  Result := b;
  Position := p;
end;

{$HINTS OFF}
procedure TTMSFNCMemoryStream.OpenFile(FileType: string);
var
  b: TBytes;
  s: string;
  f: string;
begin
  {$IFDEF FNCLIB}
  f := 'tmp.' + TTMSFNCUtils.GetFileExtension(FileType);
  {$ENDIF}

  {$IFDEF ELECTRON}
  if isElectron then
  begin
    s := ElectronPath.GetTemp + PathDelim + f;
    SaveToFile(s);
    ElectronShell.OpenItem(s);
  end
  else
  {$ENDIF}
  begin
    b := Bytes;
    s := FileType;
    asm
      var file = new Blob([new Uint8Array(b)], {type: s});
      var fileURL = window.URL.createObjectURL(file);
      if (window.navigator && window.navigator.msSaveOrOpenBlob) {
        window.navigator.msSaveOrOpenBlob(file, f);
        return;
      }
      window.open(fileURL);
    end;
  end;
end;

procedure TTMSFNCMemoryStream.SaveToFile(FileName: string);
var
  s: string;
  b: TBytes;
  {$IFDEF ELECTRON}
  nfs: TNodeFS;
  {$ENDIF}
begin
  s := FileName;
  b := Bytes;
  {$IFDEF ELECTRON}
  nfs := FS;
  {$ENDIF}
  asm
    var userAgent = navigator.userAgent.toLowerCase();
    if (userAgent.indexOf('electron/') > -1) {
      var arr = new Uint8Array(b);
      nfs.writeFileSync(s, arr);
    } else {
      var newBlob = new Blob([new Uint8Array(b)], {type: "octet/stream"})

      if (window.navigator && window.navigator.msSaveOrOpenBlob) {
        window.navigator.msSaveOrOpenBlob(newBlob, s);
        return;
      }

      const data = window.URL.createObjectURL(newBlob);
      var link = document.createElement('a');
      link.href = data;
      link.download = s;
      document.body.appendChild(link);
      link.click();
      setTimeout(function(){
       document.body.removeChild(link);
        window.URL.revokeObjectURL(data);
      }, 100);
    }
  end;
end;
{$HINTS ON}
{$ENDIF}

{$IFDEF USEOWNEDCOLLECTION}

{ TTMSFNCOwnedCollection<T>.TEnumerator }

constructor TTMSFNCOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.TEnumerator.Create(ACol: TTMSFNCCollection);
begin
  inherited Create;
  FIndex := -1;
  FCol := ACol;
end;

function TTMSFNCOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.TEnumerator.GetCurrent: T;
begin
  Result := FCol.GetItem(FIndex);
end;

function TTMSFNCOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCol.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TTMSFNCOwnedCollection<T> }

function TTMSFNCOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TTMSFNCOwnedCollection{$IFNDEF LCLLIB}<T>{$ENDIF}.GetItem(AIndex: Integer): T;
begin
  Result := Items[AIndex] as T;
end;
{$ENDIF}

{$IFDEF WEBLIB}
constructor TTMSFNCBitmap.CreateFromStream(const AStream: TStream);
begin
  Create;
  LoadFromStream(AStream);
end;
{$ENDIF}

{$IFDEF FMXLIB}
constructor TTMSFNCBitmap.CreateFromStream(const AStream: TStream);
begin
  Create;
  LoadFromStream(AStream);
end;

constructor TTMSFNCBitmap.CreateFromFile(const AFileName: string);
begin
  Create;
  LoadFromFile(AFileName);
end;

destructor TTMSFNCBitmap.Destroy;
begin
  FreeAndNil(FSVG);
  inherited;
end;

procedure TTMSFNCBitmap.Assign(Source: TPersistent);
var
  ms: TMemoryStream;
begin
  inherited Assign(Source);
  if (Source is TTMSFNCBitmap) and Assigned((Source as TTMSFNCBitmap).SVG) then
  begin
    ms := TMemoryStream.Create;
    try
      {$IFDEF SVGSUPPORT}
      TTMSFNCGraphicsSVGImport((Source as TTMSFNCBitmap).SVG).SaveToStream(ms);
      {$ENDIF}
      ms.Position := 0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end
  else if (Source = nil) or ((Source is TTMSFNCBitmap) and (Source as TTMSFNCBitmap).IsEmpty) then
    FreeAndNil(FSVG);
end;

procedure TTMSFNCBitmap.DefineProperties(Filer: TFiler);
  function DoWrite: Boolean;
  begin
    Result := False;
    {$IFDEF SVGSUPPORT}
    if Assigned(FSVG) then
      Result := TTMSFNCGraphicsSVGImport(FSVG).HasElements;
    {$ENDIF}
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('SVG', ReadSVG, WriteSVG, DoWrite);
end;

procedure TTMSFNCBitmap.ReadSVG(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TTMSFNCBitmap.WriteSVG(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TTMSFNCBitmap.SaveToFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTMSFNCBitmap.LoadFromFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTMSFNCBitmap.Draw(ACanvas: TCanvas; const Rect: TRectF);
begin
  if not HasSVG then
  begin
    ACanvas.DrawBitmap(Self, RectF(0, 0, Width, Height), Rect, 1);
    Exit;
  end;

  {$IFDEF SVGSUPPORT}
  if Assigned(SVG) then
    TTMSFNCGraphicsSVGImport(SVG).Draw(ACanvas, Rect{$IFDEF CMNLIB}, True{$ENDIF});
  {$ENDIF}
end;

function TTMSFNCBitmap.HasSVG: Boolean;
begin
  Result := Assigned(SVG) {$IFDEF SVGSUPPORT} and TTMSFNCGraphicsSVGImport(SVG).HasElements{$ENDIF};
end;

procedure TTMSFNCBitmap.SaveToStream(Stream: TStream);
begin
  {$IFDEF SVGSUPPORT}
  if Assigned(FSVG) then
    TTMSFNCGraphicsSVGImport(FSVG).SaveToStream(Stream)
  else
  {$ENDIF}
    inherited SaveToStream(Stream);
end;

function TTMSFNCBitmap.SaveToBase64: string;
begin
  Result := TTMSFNCUtils.SaveBitmapToBase64(Self);
end;

procedure TTMSFNCBitmap.LoadFromBase64(const ABase64: string);
begin
  TTMSFNCUtils.LoadBitmapFromBase64(ABase64, Self);
end;

procedure TTMSFNCBitmap.LoadFromSVG(const ASVG: string);
begin
  {$IFDEF SVGSUPPORT}
  if not Assigned(FSVG) then
    FSVG := TTMSFNCGraphicsSVGImport.Create;

  TTMSFNCGraphicsSVGImport(FSVG).LoadFromText(ASVG);
  inherited Assign(TTMSFNCGraphicsSVGImport(FSVG).GenerateBitmap);
  {$ENDIF}
end;

procedure TTMSFNCBitmap.LoadFromStream(Stream: TStream);
var
  s: string;
begin
  s := TTMSFNCUtils.GetImageType(Stream, False);
  if s <> '' then
  begin
    FreeAndNil(FSVG);
    inherited LoadFromStream(Stream);
  end
  else
  begin
    {$IFDEF SVGSUPPORT}
    if not Assigned(FSVG) then
      FSVG := TTMSFNCGraphicsSVGImport.Create;

    TTMSFNCGraphicsSVGImport(FSVG).LoadFromStream(Stream);
    inherited Assign(TTMSFNCGraphicsSVGImport(FSVG).GenerateBitmap);
    {$ENDIF}
  end;
end;
{$ENDIF}

{$IFDEF SUPPORTSHELPERS}
{$IFDEF JSONSUPPORT}

{ TTMSFNCJSONValueHelper }

function TTMSFNCJSONValueHelper.GetProperties(AName: string): TJSONValue;
begin
  Result := TTMSFNCUtils.GetJSONValue(Self, AName);
end;

function TTMSFNCJSONValueHelper.GetAsString: string;
begin
  Result := '';
  if Assigned(Self) then
    Result := Self.Value;
end;

function TTMSFNCJSONValueHelper.Find(const APath: string): TJSONValue;
begin
  Result := TTMSFNCUtils.FindJSONValue(Self, APath);
end;

function TTMSFNCJSONValueHelper.GetAsBoolean: Boolean;
begin
  Result := TTMSFNCUtils.GetJSONValueAsBoolean(Self);
end;

function TTMSFNCJSONValueHelper.GetAsInteger: Integer;
begin
  Result := TTMSFNCUtils.GetJSONValueAsInteger(Self);
end;

function TTMSFNCJSONValueHelper.GetAsDouble: Double;
begin
  Result := TTMSFNCUtils.GetJSONValueAsDouble(Self);
end;

function TTMSFNCJSONValueHelper.GetAsArray: TJSONArray;
begin
  Result := nil;
  if Self is TJSONArray then
    Result := (Self as TJSONArray);
end;

{ TTMSFNCJSONArrayHelper }

function TTMSFNCJSONArrayHelper.GetLength: Integer;
begin
  Result := TTMSFNCUtils.GetJSONArraySize(Self);
end;

function TTMSFNCJSONArrayHelper.GetValues(AIndex: Integer): TJSONValue;
begin
  Result := TTMSFNCUtils.GetJSONArrayItem(Self, AIndex);
end;
{$ENDIF}

{ TTMSFNCObjectHelper }

procedure TTMSFNCObjectHelper.FromJSON(const Value: string);
var
  s: TStringStream;
  cs: string;
  obj: TObject;
begin
  s := TStringStream.Create(Value{$IFDEF WEBLIB}, TEncoding.Ansi{$ENDIF});
  try
    obj := TTMSFNCPersistence.IOReference;
    cs := TTMSFNCPersistence.ClassTypeVariable;
    TTMSFNCPersistence.ClassTypeVariable := '';
    TTMSFNCPersistence.IOReference := Self;
    TTMSFNCPersistence.LoadSettingsFromStream(Self, s);
    TTMSFNCPersistence.ClassTypeVariable := cs;
    TTMSFNCPersistence.IOReference := obj;
  finally
    s.Free;
  end;
end;

procedure TTMSFNCObjectHelper.Log;
begin
  TTMSFNCUtils.Log(JSON);
end;

procedure TTMSFNCObjectHelper.LoadFromJSONStream(const AStream: TStream);
begin
  TTMSFNCPersistence.LoadSettingsFromStream(Self, AStream);
end;

procedure TTMSFNCObjectHelper.SaveToJSONStream(const AStream: TStream);
begin
  TTMSFNCPersistence.SaveSettingsToStream(Self, AStream);
end;

{$IFDEF JSONSUPPORT}
function TTMSFNCObjectHelper.ToJSONValue: TJSONValue;
begin
  Result := TTMSFNCUtils.ParseJSON(JSON);
end;
{$ENDIF}

function TTMSFNCObjectHelper.ToJSON(
  AExcludedProperties: TTMSFNCObjectExcludePropertyListArray): string;
var
  s: TStringStream;
  p: TTMSFNCPersistence;
  arr: TTMSFNCExcludePropertyListArray;
  I: Integer;
  ep: TTMSFNCExcludePropertyListArray;
begin
  SetLength(arr, Length(AExcludedProperties));
  for I := 0 to Length(AExcludedProperties) - 1 do
    arr[I] := AExcludedProperties[I];

  s := TStringStream.Create(''{$IFDEF WEBLIB}, TEncoding.Ansi{$ENDIF});

  p := TTMSFNCPersistence.Create;
  try
    ep := p.ExcludeProperties;
    p.ExcludeProperties := arr;
    p.SaveSettingsToStream(Self, s);
    p.ExcludeProperties := ep;
    Result := s.DataString;
  finally
    p.Free;
    s.Free;
  end;
end;

procedure TTMSFNCObjectHelper.LoadFromJSONFile(const AFileName: string);
begin
  TTMSFNCPersistence.LoadSettingsFromFile(Self, AFileName);
end;

procedure TTMSFNCObjectHelper.SaveToJSONFile(const AFileName: string);
begin
  TTMSFNCPersistence.SaveSettingsToFile(Self, AFileName);
end;

function TTMSFNCObjectHelper.ToJSON: string;
var
  s: TStringStream;
begin
  s := TStringStream.Create(''{$IFDEF WEBLIB}, TEncoding.Ansi{$ENDIF});
  try
    TTMSFNCPersistence.SaveSettingsToStream(Self, s);
    Result := s.DataString;
  finally
    s.Free;
  end;
end;
{$ENDIF}

{ TTMSFNCSVGImport }

constructor TTMSFNCSVGImport.Create;
begin
  FRotationAngle := 0;
  FCustomOpacity := -1;
  {$IFDEF FMXLIB}
  FCustomFillColor := $00000000;
  FCustomStrokeColor := $00000000;
  FTintColor := $00000000;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  FCustomFillColor := -1;
  FCustomStrokeColor := -1;
  FTintColor := -1;
  {$ENDIF}
end;

{$IFDEF CMNLIB}

{ TTMSFNCBitmap }

function TTMSFNCBitmap.GetSVG: TTMSFNCSVGImport;
begin
  Result := nil;
  if HasSVG then
    Result := (Graphic as TTMSFNCSVGBitmap).SVG;
end;

function TTMSFNCBitmap.HasSVG: Boolean;
begin
  Result := Assigned(Graphic) and (Graphic is TTMSFNCSVGBitmap) and (Graphic as TTMSFNCSVGBitmap).HasSVG;
end;

{ TTMSFNCSVGBitmap }

{$IFDEF LCLLIB}
class function TTMSFNCSVGBitmap.IsStreamFormatSupported(Stream: TStream): Boolean;
begin
  Result := TTMSFNCUtils.GetImageType(Stream, False) = '';
end;
{$ENDIF}     

{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 32}
class function TTMSFNCSVGBitmap.CanLoadFromStream(Stream: TStream): Boolean;
begin
  Result := TTMSFNCUtils.GetImageType(Stream, False) = '';
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}

constructor TTMSFNCSVGBitmap.Create;
begin
  inherited;
  {$IFDEF SVGSUPPORT}
  FSVG := TTMSFNCGraphicsSVGImport.Create;
  {$ENDIF}
  FStream := TMemoryStream.Create;
end;

destructor TTMSFNCSVGBitmap.Destroy;
begin
  FreeAndNil(FSVG);
  FreeAndNil(FStream);
  inherited;
end;

procedure TTMSFNCSVGBitmap.Clear;
begin
  if Assigned(FSVG) then
    FSVG.Clear;

  FFileName := '';
  Changed(Self);
end;

procedure TTMSFNCSVGBitmap.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCSVGBitmap) then
  begin
    try
      FSVG.Clear;
      FStream.Clear;
      FStream.LoadFromStream(TTMSFNCSVGBitmap(Source).FStream);
      {$IFDEF SVGSUPPORT}
      TTMSFNCGraphicsSVGImport(FSVG).LoadFromStream(FStream);
      {$ENDIF}
    except
    end;
    Changed(Self);
  end;
end;

function TTMSFNCSVGBitmap.HasSVG: Boolean;
begin
  Result := Assigned(SVG){$IFDEF SVGSUPPORT} and TTMSFNCGraphicsSVGImport(SVG).HasElements{$ENDIF};
end;

procedure TTMSFNCSVGBitmap.AssignTo(Dest: TPersistent);
begin
  if Dest is TTMSFNCSVGBitmap then
    TTMSFNCSVGBitmap(Dest).Assign(Self);
end;

procedure TTMSFNCSVGBitmap.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;

  LoadFromFile(Value);
end;

procedure TTMSFNCSVGBitmap.ReadData(Stream: TStream);
var
  Size: LongInt;
begin
  Stream.Read(Size, SizeOf(Size));
  FStream.Clear;
  if Stream.Size > 0 then
  begin
    FStream.CopyFrom(Stream, Size);
    {$IFDEF SVGSUPPORT}
    TTMSFNCGraphicsSVGImport(SVG).LoadFromStream(FStream);
    {$ENDIF}
  end;
end;

procedure TTMSFNCSVGBitmap.WriteData(Stream: TStream);
var
  Size: LongInt;
begin
  Size := FStream.Size;
  Stream.Write(Size, SizeOf(Size));
  FStream.Position := 0;
  FStream.SaveToStream(Stream);
end;

procedure TTMSFNCSVGBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
  {$IFDEF SVGSUPPORT}
var
  r: TRectF;
  si: TTMSFNCGraphicsSVGImport;
  {$ENDIF}
begin
  if not Assigned(FSVG) then
    Exit;

  if Empty then
    Exit;

  {$IFDEF SVGSUPPORT}
  si := TTMSFNCGraphicsSVGImport(FSVG);
  r := ConvertToRectF(Rect);
  si.Draw(ACanvas, r{$IFDEF CMNLIB}, True{$ENDIF});
  {$ENDIF}
end;

function TTMSFNCSVGBitmap.GetEmpty: Boolean;
begin
  Result := True;
  {$IFDEF SVGSUPPORT}
  if Assigned(FSVG) then
    Result := not TTMSFNCGraphicsSVGImport(FSVG).HasElements;
  {$ENDIF}
end;

{$IFDEF LCLLIB}
procedure TTMSFNCSVGBitmap.SetTransparent(Value: Boolean);
begin
end;

function TTMSFNCSVGBitmap.GetTransparent: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCSVGBitmap.SetWidth(Value: Integer);
begin
end;

procedure TTMSFNCSVGBitmap.SetHeight(Value: Integer);
begin
end;
{$ENDIF}

function TTMSFNCSVGBitmap.GetWidth: Integer;
begin
  Result := 0;
  {$IFDEF SVGSUPPORT}
  if Assigned(FSVG) then
    Result := Round((TTMSFNCGraphicsSVGImport(FSVG).ViewRect.Right - TTMSFNCGraphicsSVGImport(FSVG).ViewRect.Left) * TTMSFNCUtils.GetDPIScale);
  {$ENDIF}
end;

function TTMSFNCSVGBitmap.GetHeight: Integer;
begin
  Result := 0;
  {$IFDEF SVGSUPPORT}
  if Assigned(FSVG) then
    Result := Round((TTMSFNCGraphicsSVGImport(FSVG).ViewRect.Bottom - TTMSFNCGraphicsSVGImport(FSVG).ViewRect.Top) * TTMSFNCUtils.GetDPIScale);
  {$ENDIF}
end;

procedure TTMSFNCSVGBitmap.SaveToFile(const FileName: String);
var
  Stream: TStream;
begin
  if not Assigned(FSVG) then
    Exit;

  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTMSFNCSVGBitmap.LoadFromFile(const Filename: String);
begin
  if not Assigned(FSVG) then
    Exit;

  FStream.Clear;
  FStream.LoadFromFile(FileName);
  {$IFDEF SVGSUPPORT}
  TTMSFNCGraphicsSVGImport(FSVG).LoadFromStream(FStream);
  {$ENDIF}
  Changed(Self);
end;

procedure TTMSFNCSVGBitmap.LoadFromStream(Stream: TStream);
begin
  if not Assigned(FSVG) or not Assigned(Stream) then
    Exit;

  try
    FFileName := '';
    FStream.LoadFromStream(Stream);
    {$IFDEF SVGSUPPORT}
    TTMSFNCGraphicsSVGImport(FSVG).LoadFromStream(FStream);
    {$ENDIF}
  except
  end;
  Changed(Self);
end;

procedure TTMSFNCSVGBitmap.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FStream.Position := 0;
    FStream.SaveToStream(Stream);
  end;
end;

{$IFDEF SVGSUPPORT}
{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 33}

{ TTMSFNCSVGImageCollection }

procedure TTMSFNCSVGImageCollection.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCSVGImageCollection then
    FImages.Assign((Source as TTMSFNCSVGImageCollection).FImages)
  else
    inherited;
end;

constructor TTMSFNCSVGImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TTMSFNCSVGImageCollectionItems.Create(Self, TTMSFNCSVGImageCollectionItem);
end;

destructor TTMSFNCSVGImageCollection.Destroy;
begin
  FImages.Free;
  inherited;
end;

procedure TTMSFNCSVGImageCollection.Draw(ACanvas: TCanvas; X, Y, AIndex: Integer;
  AProportional: Boolean);
var
  it: TTMSFNCSVGImageCollectionItem;
begin
  it := GetItemByIndex(AIndex);
  if Assigned(it) then
    Draw(ACanvas, Bounds(X, Y, it.Data.Width, it.Data.Height), AIndex, AProportional);
end;

procedure TTMSFNCSVGImageCollection.Draw(ACanvas: TCanvas; ARect: TRect;
  AIndex: Integer; AProportional: Boolean);
var
  it: TTMSFNCSVGImageCollectionItem;
  {$IFDEF SVGSUPPORT}
  si: TTMSFNCGraphicsSVGImport;
  {$ENDIF}
  r: TRect;

  function UpdateRectForProportionalSize(ARect: TRect; AWidth, AHeight: Integer; AStretch: Boolean): TRect;
  var
    w, h, cw, ch: Integer;
    xyaspect: Double;
  begin
    Result := ARect;
    if AWidth * AHeight = 0 then
      Exit;

    w := AWidth;
    h := AHeight;
    cw := ARect.Width;
    ch := ARect.Height;

    if AStretch or ((w > cw) or (h > ch)) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
       end
       else
       begin
         h := ch;
         w := Trunc(ch * xyaspect);
         if w > cw then
         begin
           w := cw;
           h := Trunc(cw / xyaspect);
         end;
       end;
    end;

    Result := Rect(0, 0, w, h);
    OffsetRect(Result, ARect.Left + (cw - w) div 2, ARect.Top + (ch - h) div 2);
  end;
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    it := Images[AIndex];
    if Assigned(it.Data.Graphic) then
    begin
      r := ARect;
      if AProportional then
        r := UpdateRectForProportionalSize(r, it.Data.Width, it.Data.Height, False);

      if (it.Data.Graphic is TTMSFNCSVGBitmap) then
      begin
        {$IFDEF SVGSUPPORT}
        si := TTMSFNCGraphicsSVGImport((it.Data.Graphic as TTMSFNCSVGBitmap).SVG);
        si.GrayScale := it.GrayScale;
        si.CustomFillColor := it.CustomFillColor;
        si.CustomStrokeColor := it.CustomStrokeColor;
        si.TintColor := it.TintColor;
        si.CustomOpacity := it.CustomOpacity;
	si.RotationAngle := it.RotationAngle;
        si.Draw(ACanvas, ConvertToRectF(r), True);
        {$ENDIF}
      end
      else
        ACanvas.StretchDraw(r, it.Data.Graphic);
    end;
  end;
end;

function TTMSFNCSVGImageCollection.GetBitmap(AIndex, AWidth,
  AHeight: Integer): TBitmap;
var
  it: TTMSFNCSVGImageCollectionItem;
  {$IFDEF SVGSUPPORT}
  si: TTMSFNCGraphicsSVGImport;
  {$ENDIF}
  p: Pointer;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    it := Images[AIndex];
    if Assigned(it.Data.Graphic) then
    begin
      Result := TBitmap.Create;
      Result.PixelFormat := pf32Bit;
      Result.Width := AWidth;
      Result.Height := AHeight;
      Result.HandleType := bmDIB;
      Result.IgnorePalette := true;
      Result.AlphaFormat := afIgnored;

      p := Result.ScanLine[AHeight - 1];
      ZeroMemory(p, AWidth * AHeight * 4);

      if (it.Data.Graphic is TTMSFNCSVGBitmap) then
      begin
        {$IFDEF SVGSUPPORT}
        si := TTMSFNCGraphicsSVGImport((it.Data.Graphic as TTMSFNCSVGBitmap).SVG);
        si.GrayScale := it.GrayScale;
        si.CustomFillColor := it.CustomFillColor;
        si.CustomStrokeColor := it.CustomStrokeColor;
        si.TintColor := it.TintColor;
        si.CustomOpacity := it.CustomOpacity;
	si.RotationAngle := it.RotationAngle;
        si.Draw(Result.Canvas, RectF(0, 0, AWidth, AHeight), True)
        {$ENDIF}
      end
      else
        Result.Canvas.StretchDraw(Bounds(0, 0, AWidth, AHeight), it.Data.Graphic);
    end;
  end;
end;

function TTMSFNCSVGImageCollection.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TTMSFNCSVGImageCollection.GetIndexByName(const AName: String): Integer;
var
  I: Integer;
  S: String;
begin
  Result := -1;
  S := LowerCase(AName);
  for I := 0 to FImages.Count - 1 do
    if LowerCase(FImages[I].Name) = S then
      Exit(I);
end;

function TTMSFNCSVGImageCollection.GetItemByIndex(
  AIndex: Integer): TTMSFNCSVGImageCollectionItem;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Images[AIndex];
end;

function TTMSFNCSVGImageCollection.GetNameByIndex(AIndex: Integer): String;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Images[AIndex].Name;
end;

function TTMSFNCSVGImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (Count > 0) and (AIndex >= 0) and (AIndex < Count);
end;

procedure TTMSFNCSVGImageCollection.SetImages(const Value: TTMSFNCSVGImageCollectionItems);
begin
  FImages.Assign(Value);
end;

{ TTMSFNCSVGImageCollectionItem }

constructor TTMSFNCSVGImageCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FData := TPicture.Create;
  FName := '';
  FGrayScale := False;
  FCustomFillColor := -1;
  FCustomStrokeColor := -1;
  FTintColor := -1;
  FCustomOpacity := -1;
  FRotationAngle := 0;
end;

function TTMSFNCSVGImageCollectionItem.IsCustomFillColorStored: Boolean;
begin
  Result := CustomFillColor <> -1;
end;

function TTMSFNCSVGImageCollectionItem.IsRotationAngleStored: Boolean;
begin
  Result := RotationAngle <> 0;
end;

function TTMSFNCSVGImageCollectionItem.IsTintColorStored: Boolean;
begin
  Result := TintColor <> -1;
end;

function TTMSFNCSVGImageCollectionItem.IsCustomStrokeColorStored: Boolean;
begin
  Result := CustomStrokeColor <> -1;
end;

function TTMSFNCSVGImageCollectionItem.IsCustomOpacityStored: Boolean;
begin
  Result := CustomOpacity <> -1;
end;

procedure TTMSFNCSVGImageCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCSVGImageCollectionItem then
  begin
    FRotationAngle := (Source as TTMSFNCSVGImageCollectionItem).RotationAngle;
    FData.Assign((Source as TTMSFNCSVGImageCollectionItem).Data);
    FName := (Source as TTMSFNCSVGImageCollectionItem).Name;
    FGrayScale := (Source as TTMSFNCSVGImageCollectionItem).GrayScale;
    FCustomFillColor := (Source as TTMSFNCSVGImageCollectionItem).CustomFillColor;
    FCustomStrokeColor := (Source as TTMSFNCSVGImageCollectionItem).CustomStrokeColor;
    FTintColor := (Source as TTMSFNCSVGImageCollectionItem).TintColor;
    FCustomOpacity := (Source as TTMSFNCSVGImageCollectionItem).CustomOpacity;
  end
  else
    inherited;
end;

function TTMSFNCSVGImageCollectionItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  {$IFDEF WEBLIB}
  else
    Result := '';
  {$ENDIF}
  {$IFNDEF WEBLIB}
  else
    Result := inherited;
  {$ENDIF}
end;

destructor TTMSFNCSVGImageCollectionItem.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TTMSFNCSVGImageCollectionItem.SetData(const Value: TPicture);
begin
  FData.Assign(Value);
end;

procedure TTMSFNCSVGImageCollectionItem.SetName(const Value: String);
begin
  FName := Value;
end;

{ TTMSFNCSVGImageCollectionItems }

function TTMSFNCSVGImageCollectionItems.GetItem(Index: Integer): TTMSFNCSVGImageCollectionItem;
begin
  Result := TTMSFNCSVGImageCollectionItem(inherited GetItem(Index));
end;

procedure TTMSFNCSVGImageCollectionItems.SetItem(Index: Integer; Value: TTMSFNCSVGImageCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TTMSFNCSVGImageCollectionItems.Add: TTMSFNCSVGImageCollectionItem;
begin
  Result := TTMSFNCSVGImageCollectionItem(inherited Add);
end;
{$IFEND}
{$HINTS ON}
{$ENDIF}

function IsGraphicClassRegistered(const FileExt: String): Boolean;
var
  Ext: String;
  List: TStringList;
  I: Integer;
begin
  Result := False;
  Ext := FileExt;
  List := TStringList.Create;
  try
    List.Delimiter := ';';
    List.StrictDelimiter := True;
    List.DelimitedText := GraphicFileMask(TGraphic);
    for I := 0 to List.Count-1 do
    begin
      if MatchesMask(FileExt, List[I]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    List.Free;
  end;
end;

initialization
begin
  if not Assigned(GetClass(TTMSFNCSVGBitmap.ClassName)) then
  begin
    RegisterClass(TTMSFNCSVGBitmap);
    TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TTMSFNCSVGBitmap);
  end;
end;

finalization
begin
  TPicture.UnregisterGraphicClass(TTMSFNCSVGBitmap);
  UnRegisterClass(TTMSFNCSVGBitmap);
end;
{$ENDIF}
{$ENDIF}

end.

