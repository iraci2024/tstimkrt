unit tapioca;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  Windows;

const
  // Magnifier Class Name
  WC_MAGNIFIERA: AnsiString = 'Magnifier';
  WC_MAGNIFIERW: WideString = 'Magnifier';
  WC_MAGNIFIER = 'Magnifier';

  // Magnifier Window Styles
  MS_SHOWMAGNIFIEDCURSOR = $0001;
  MS_CLIPAROUNDCURSOR = $0002;
  MS_INVERTCOLORS = $0004;

  // Filter Modes
  MW_FILTERMODE_EXCLUDE = 0;
  MW_FILTERMODE_INCLUDE = 1;

type
  tagMAGTRANSFORM = record
    v: array [1 .. 3, 1 .. 3] of Single;
  end;

  MAGTRANSFORM = tagMAGTRANSFORM;
  TMagTransform = tagMAGTRANSFORM;
  PMagTransform = ^TMagTransform;

  tagMAGIMAGEHEADER = record
    width: UINT;
    height: UINT;
    format: TGUID;
    stride: UINT;
    offset: UINT;
    cbSize: UINT;
  end;

  MAGIMAGEHEADER = tagMAGIMAGEHEADER;
  TMagImageHeader = tagMAGIMAGEHEADER;
  PMagImageHeader = ^TMagImageHeader;

  tagMAGCOLOREFFECT = record
    transform: array [1 .. 5, 1 .. 5] of Single;
  end;

  MAGCOLOREFFECT = tagMAGCOLOREFFECT;
  TMagColorEffect = tagMAGCOLOREFFECT;
  PMagColorEffect = ^TMagColorEffect;

  TMagImageScalingCallback = function(hwnd: hwnd; srcdata: Pointer;
    srcheader: MAGIMAGEHEADER; destdata: Pointer; destheader: MAGIMAGEHEADER;
    unclipped: TRect; clipped: TRect; dirty: HRGN): BOOL; stdcall;

  THWNDArray = array [0 .. 0] of hwnd;
  PHWNDArray = ^THWNDArray;

  // Public Functions
function MagInitialize(): BOOL; stdcall;
function MagUninitialize(): BOOL; stdcall;

function MagSetWindowSource(hwnd: hwnd; rect: TRect): BOOL; stdcall;
function MagGetWindowSource(hwnd: hwnd; var rect: TRect): BOOL; stdcall;
function MagSetWindowTransform(hwnd: hwnd; var transform: TMagTransform)
  : BOOL; stdcall;
function MagGetWindowTransform(hwnd: hwnd; var transform: TMagTransform)
  : BOOL; stdcall;
function MagSetWindowFilterList(hwnd: hwnd; dwFilterMode: DWORD; count: Integer;
  pHWND: PHWNDArray): BOOL; stdcall;
function MagGetWindowFilterList(hwnd: hwnd; var dwFilterMode: DWORD;
  count: Integer; pHWND: PHWNDArray): Integer; stdcall;
function MagSetImageScalingCallback(hwnd: hwnd;
  MagImageScalingCallback: TMagImageScalingCallback): BOOL; stdcall;
// MagImageScalingCallback WINAPI MagGetImageScalingCallback(HWND hwnd );
function MagSetColorEffect(hwnd: hwnd; var Effect: TMagColorEffect)
  : BOOL; stdcall;
function MagGetColorEffect(hwnd: hwnd; var Effect: TMagColorEffect)
  : BOOL; stdcall;

implementation

const
  MagnificationDll = 'Magnification.dll';

function MagInitialize; external MagnificationDll name 'MagInitialize';
function MagUninitialize; external MagnificationDll name 'MagUninitialize';
function MagSetWindowSource;
  external MagnificationDll name 'MagSetWindowSource';
function MagGetWindowSource;
  external MagnificationDll name 'MagGetWindowSource';
function MagSetWindowTransform;
  external MagnificationDll name 'MagSetWindowTransform';
function MagGetWindowTransform;
  external MagnificationDll name 'MagGetWindowTransform';
function MagSetWindowFilterList;
  external MagnificationDll name 'MagSetWindowFilterList';
function MagGetWindowFilterList;
  external MagnificationDll name 'MagGetWindowFilterList';
function MagSetImageScalingCallback;
  external MagnificationDll name 'MagSetImageScalingCallback';
function MagSetColorEffect; external MagnificationDll name 'MagSetColorEffect';
function MagGetColorEffect; external MagnificationDll name 'MagGetColorEffect';

end.
