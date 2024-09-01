{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE }
{ PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit cxCustomCanvas;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Generics.Collections, Generics.Defaults, Classes,
  Graphics, Controls,
  cxGeometry, dxCore, dxCoreClasses, dxSmartImage, dxGDIPlusAPI,
  dxGDIPlusClasses, dxCoreGraphics,
  cxDrawTextUtils, dxDPIAwareUtils;

type
  TcxRotationAngle = (ra0, raPlus90, raMinus90, ra180);

  { IcxControlCanvas }

  IcxControlCanvas = interface
    ['{E40B5E3C-CD3C-4D2C-81DA-2A41800CA183}']
    procedure BeginPaint;
    procedure EndPaint;
  end;

  { IcxControlDirectCanvas }

  IcxControlDirectCanvas = interface(IcxControlCanvas)
    ['{0920AC2D-5CED-4DF4-ABCB-4D12248AC63D}']
    procedure CopyToDC(DC: HDC); overload;
    procedure CopyToDC(DC: HDC; const ATargetRect, ASourceRect: TRect);
      overload;
    procedure SetWndHandle(AHandle: HWND);
  end;

  { IcxCanvasCacheControl }

  IcxCanvasCacheControl = interface
    ['{A1DEDC74-DBA2-4EAC-BC91-5016E48F4E7B}']
    procedure FlushCache;
  end;

  TcxCustomCanvas = class;
  TcxGdiBasedCanvas = class;

  { TcxCanvasBasedResource }

  TcxCanvasBasedResource = class // for internal use
  protected
    FCanvas: TcxCustomCanvas;
  public
    constructor Create(ACanvas: TcxCustomCanvas);
    //
    property Canvas: TcxCustomCanvas read FCanvas;
  end;

  { TcxCanvasBasedResourceCacheKey }

  TcxCanvasBasedResourceCacheKey = packed record // for internal use
    Owner: Pointer;
    Size: TSize;
    Part: Integer;
    State: Integer;
    TargetDPI: Word;

    class function Create(AOwner: Pointer; const ASize: TSize; AState: Integer;
      APart: Integer = 0; ATargetDPI: Word = dxDefaultDPI)
      : TcxCanvasBasedResourceCacheKey; static;
  end;

  { TcxCanvasBasedImage }

  PcxCanvasBasedImage = ^TcxCanvasBasedImage; // for internal use

  TcxCanvasBasedImage = class(TcxCanvasBasedResource) // for internal use
  strict private
    FHeight: Integer;
    FWidth: Integer;

    function GetClientRect: TRect; inline;
  public
    constructor Create(ACanvas: TcxCustomCanvas; AWidth, AHeight: Integer);
    procedure Draw(const ATargetRect: TRect; AAlpha: Byte = MaxByte);
      overload; virtual;
    procedure Draw(const ATargetRect, ASourceRect: TRect;
      AAlpha: Byte = MaxByte); overload; virtual; abstract;
    function Empty: Boolean;

    property ClientRect: TRect read GetClientRect;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

  { TcxCanvasBasedFont }

  TcxCanvasBasedFont = class(TcxCanvasBasedResource); // for internal use

  { TcxCanvasBasedTextLayout }

  TcxCanvasBasedTextLayout = class(TcxCanvasBasedResource) // for internal use
  strict private
    FLayoutIsDirty: Boolean;
  protected
    FFlags: Integer;
    FMaxHeight: Integer;
    FMaxRowCount: Integer;
    FMaxWidth: Integer;
    FPaintOnGlass: Boolean;
    FRotation: TcxRotationAngle;
    FRowCount: Integer;
    FSize: TSize;
    FText: string;

    function CalculateRowCount(ARowHeight: Single): Integer;
    procedure DoCalculateLayout; virtual; abstract;
    procedure DoDraw(const R: TRect); virtual;
    function IsVertical: Boolean; inline;
    procedure ApplyFlags; virtual;
    procedure LayoutChanged; virtual;
    procedure TextChanged; virtual;
  public
    procedure Draw(const ARect: TRect); virtual;
    function MeasureSize: TSize;
    procedure SetColor(AColor: TColor); virtual; abstract;
    procedure SetFlags(ACxTOFlags: Integer); virtual;
    procedure SetFont(AFont: TcxCanvasBasedFont;
      AOwnership: TdxObjectOwnership = ooReferenced); overload;
      virtual; abstract;
    procedure SetFont(AFont: TFont;
      AOwnership: TdxObjectOwnership = ooReferenced); overload; virtual;
    procedure SetLayoutConstraints(AMaxWidth, AMaxHeight: Integer;
      AMaxRowCount: Integer = 0); overload;
    procedure SetLayoutConstraints(const R: TRect); overload;
    procedure SetPaintOnGlass(AValue: Boolean);
    procedure SetRotation(ARotation: TcxRotationAngle);
    procedure SetText(const AText: string); virtual;
  end;

  { TcxCustomCanvas }

  TcxCanvasNativeDrawProc = reference to procedure(ACanvas: TcxGdiBasedCanvas;
    const R: TRect); // for internal use
  TcxCanvasNativeDrawExProc = reference to procedure(ACanvas: TdxGPCanvas;
    const R: TRect); // for internal use

  TcxCanvasImageStretchQuality = (isqStandard, isqHigh);

  TcxCustomCanvas = class(TcxIUnknownObject)
  strict private
    FImageStretchQuality: TcxCanvasImageStretchQuality;

    function GetUseRightToLeftAlignment: Boolean;
    procedure SetUseRightToLeftAlignment(const Value: Boolean);
  protected
    FUseRightToLeftAlignment: TdxDefaultBoolean;

    function GetDefaultUseRightToLeftAlignment: Boolean; virtual;
    function GetIsLowColorsMode: Boolean; virtual;
    function GetWindowOrg: TPoint; virtual; abstract;
    procedure SetWindowOrg(const Value: TPoint); virtual; abstract;
  public
    procedure AfterConstruction; override;

    function CheckIsValid(var AResource { : TcxCanvasBasedResource } ): Boolean;
      inline; // for internal use
    function CreateFonT(AFont: TFont): TcxCanvasBasedFont; virtual; abstract;
    // for internal use
    function CreateImage(ABitmap: TBitmap;
      AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage; overload;
      virtual; abstract; // for internal use
    function CreateImage(ABitmap: TdxCustomFastDIB;
      AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage; overload;
      virtual; abstract; // for internal use
    function CreateImage(AGpImage: TdxGPImage;
      AOwnership: TdxObjectOwnership = ooCloned): TcxCanvasBasedImage; overload;
      virtual; // for internal use
    function CreateImage(AGpImage: TdxGPImageHandle): TcxCanvasBasedImage;
      overload; virtual; // for internal use
    function CreateImage(AGraphic: TGraphic): TcxCanvasBasedImage; overload;
      virtual; // for internal use
    function CreateTextLayout: TcxCanvasBasedTextLayout; virtual; abstract;
    // for internal use

    procedure DrawNativeObject(const R: TRect;
      const ACacheKey: TcxCanvasBasedResourceCacheKey;
      AProc: TcxCanvasNativeDrawExProc); overload; virtual; // for internal use
    procedure DrawNativeObject(const R: TRect;
      const ACacheKey: TcxCanvasBasedResourceCacheKey;
      AProc: TcxCanvasNativeDrawProc); overload; virtual; abstract;
    // for internal use

    procedure DrawBitmap(ABitmap: TBitmap; const ATargetRect: TRect;
      ACache: PcxCanvasBasedImage = nil); overload;
    procedure DrawBitmap(ABitmap: TBitmap; const ATargetRect: TRect;
      AAlphaFormat: TAlphaFormat; ACache: PcxCanvasBasedImage = nil); overload;
    procedure DrawBitmap(ABitmap: TBitmap;
      const ATargetRect, ASourceRect: TRect;
      ACache: PcxCanvasBasedImage = nil); overload;
    procedure DrawBitmap(ABitmap: TBitmap;
      const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
      ACache: PcxCanvasBasedImage = nil); overload; virtual;
    procedure DrawBitmap(ABitmap: TdxFastDIB; const ATargetRect: TRect;
      AAlphaFormat: TAlphaFormat; ACache: PcxCanvasBasedImage = nil); overload;
    procedure DrawBitmap(ABitmap: TdxFastDIB;
      const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
      ACache: PcxCanvasBasedImage = nil); overload; virtual;
    procedure DrawBitmap(ABitmap: TdxGpFastDIB; const ATargetRect: TRect;
      AAlphaFormat: TAlphaFormat; ACache: PcxCanvasBasedImage = nil); overload;
    procedure DrawBitmap(ABitmap: TdxGpFastDIB;
      const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
      ACache: PcxCanvasBasedImage = nil); overload; virtual;

    procedure DrawImage(AImage: TdxGPImage; const ATargetRect: TRect;
      ACache: PcxCanvasBasedImage = nil); overload; virtual;

    procedure DrawComplexFrame(const R: TRect;
      ALeftTopColor, ARightBottomColor: TColor;
      ABorders: TcxBorders = cxBordersAll; ABorderWidth: Integer = 1); virtual;
    procedure DrawEdge(const R: TRect; ASunken, AOuter: Boolean;
      ABorders: TcxBorders = cxBordersAll); virtual;
    procedure FrameRect(const R: TRect; AColor: TColor; ALineWidth: Integer = 1;
      ABorders: TcxBorders = cxBordersAll); overload; virtual;
    procedure FrameRect(const R: TRect; AColor: TdxAlphaColor;
      ALineWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll);
      overload; virtual;

    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer; AColor: TColor;
      APenWidth: Integer = 1); virtual; abstract;
    procedure FillPixel(X, Y: Integer; AColor: TColor); virtual;
    procedure FillPolygon(const P: array of TPoint; AColor: TColor); virtual;
    procedure FillRect(const R: TRect; AColor: TColor); overload;
      virtual; abstract;
    procedure FillRect(const R: TRect; AColor: TColor; AAlpha: Byte); overload;
    procedure FillRect(const R: TRect; AColor: TdxAlphaColor); overload;
      virtual; abstract;
    procedure FillRect(const R: TRect; AImage: TcxCanvasBasedImage); overload;
      virtual; // for internal use
    procedure FillRect(const R: TRect; AImage: TdxGPImage;
      ACache: PcxCanvasBasedImage = nil); overload; virtual;
    procedure FillRectByGradient(const ARect: TRect; AColor1, AColor2: TColor;
      AHorizontal: Boolean); overload; virtual;
    procedure FillRectByGradient(const ARect: TRect;
      AColor1, AColor2: TdxAlphaColor; AGradient: TdxGpLinearGradientMode);
      overload; virtual; abstract;
    procedure FocusRectangle(const R: TRect); virtual; abstract;
    procedure Line(const P1, P2: TPoint; AColor: TColor; APenWidth: Integer = 1;
      APenStyle: TPenStyle = psSolid); virtual;
    procedure Polygon(const P: array of TPoint; ABrushColor, APenColor: TColor);
      virtual; abstract;
    procedure Polyline(const P: array of TPoint; AColor: TColor;
      APenWidth: Integer = 1; APenStyle: TPenStyle = psSolid); virtual;
      abstract;
    procedure Rectangle(const R: TRect; ABrushColor: TColor; APenColor: TColor;
      APenStyle: TPenStyle; APenWidth: Integer = 1); overload; virtual;
      abstract;

    procedure ModifyWorldTransform(const AForm: TXForm); virtual; abstract;
    procedure RestoreWorldTransform; virtual; abstract;
    procedure SaveWorldTransform; virtual; abstract;

    procedure RestoreState; virtual; abstract;
    procedure SaveState; virtual; abstract;

    function MoveWindowOrg(const P: TPoint): TPoint; virtual;

    procedure IntersectClipRect(const R: TRect); virtual; abstract;
    function RectVisible(const R: TRect): Boolean; virtual; abstract;
    procedure RestoreClipRegion; virtual; abstract;
    procedure SaveClipRegion; virtual; abstract;

    property IsLowColorsMode: Boolean read GetIsLowColorsMode;
    property ImageStretchQuality: TcxCanvasImageStretchQuality
      read FImageStretchQuality write FImageStretchQuality;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment
      write SetUseRightToLeftAlignment;
    property WindowOrg: TPoint read GetWindowOrg write SetWindowOrg;
  end;

  { TcxGdiBasedCanvas }

  TcxGdiBasedCanvas = class(TcxCustomCanvas)
  strict private
    FBaseOrigin: TPoint;
    FBrushOrigin: TPoint;
    FSavedWorldTransforms: TStack<TXForm>;

    function GetDCOrigin: TPoint;
  protected
    procedure RestoreBaseOrigin;
    procedure SaveBaseOrigin;

    function GetHandle: HDC; virtual; abstract;
    function GetIsLowColorsMode: Boolean; override;
    function GetViewportOrg: TPoint;
    function GetWindowOrg: TPoint; override;
    procedure SetViewportOrg(const P: TPoint);
    procedure SetWindowOrg(const P: TPoint); override;
  public
    destructor Destroy; override;

    function CreateFonT(AFont: TFont): TcxCanvasBasedFont; override;
    // for internal use
    function CreateImage(ABitmap: TBitmap;
      AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage; override;
    // for internal use
    function CreateImage(ABitmap: TdxCustomFastDIB;
      AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage; override;
    // for internal use
    function CreateImage(AGpImage: TdxGPImage;
      AOwnership: TdxObjectOwnership = ooCloned): TcxCanvasBasedImage; override;
    // for internal use
    function CreateTextLayout: TcxCanvasBasedTextLayout; override;
    // for internal use

    procedure DrawNativeObject(const R: TRect;
      const ACacheKey: TcxCanvasBasedResourceCacheKey;
      AProc: TcxCanvasNativeDrawProc); override; // for internal use

    procedure DrawBitmap(ABitmap: TBitmap;
      const ATargetRect, ASourceRect: TRect;
      AAlphaFormat: TAlphaFormat = afIgnored;
      ACache: PcxCanvasBasedImage = nil); override;
    procedure DrawBitmap(ABitmap: TdxFastDIB;
      const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
      ACache: PcxCanvasBasedImage = nil); override;
    procedure DrawBitmap(ABitmap: TdxGpFastDIB;
      const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
      ACache: PcxCanvasBasedImage = nil); override;

    procedure DrawImage(AImage: TdxGPImage; const ATargetRect: TRect;
      ACache: PcxCanvasBasedImage = nil); override;

    procedure FillPixel(X, Y: Integer; AColor: TColor); override;
    procedure FillRect(const R: TRect; AColor: TColor); override;
    procedure FillRect(const R: TRect; AColor: TdxAlphaColor); override;
    procedure FillRect(const R: TRect; AImage: TcxCanvasBasedImage); override;
    procedure FillRect(const R: TRect; AImage: TdxGPImage;
      ACache: PcxCanvasBasedImage = nil); override;
    procedure FillRectByGradient(const ARect: TRect; AColor1, AColor2: TColor;
      AHorizontal: Boolean); override;
    procedure FillRectByGradient(const ARect: TRect;
      AColor1, AColor2: TdxAlphaColor; AMode: TdxGpLinearGradientMode);
      override;
    procedure FocusRectangle(const R: TRect); override;

    procedure ModifyWorldTransform(const AForm: TXForm); override;
    procedure RestoreWorldTransform; override;
    procedure SaveWorldTransform; override;

    procedure ExcludeClipRect(const R: TRect);
    procedure IntersectClipRect(const R: TRect); override;
    function RectVisible(const R: TRect): Boolean; override;

    property BaseOrigin: TPoint read FBaseOrigin;
    property DCOrigin: TPoint read GetDCOrigin;
    property Handle: HDC read GetHandle;
    property ViewportOrg: TPoint read GetViewportOrg write SetViewportOrg;
  end;

  { TcxGdiCanvasBasedFont }

  TcxGdiCanvasBasedFont = class(TcxCanvasBasedFont) // for internal use
  public
    NativeFont: TFont;

    constructor Create(AFont: TFont);
    destructor Destroy; override;
    function Equals(Obj: TObject): Boolean; override;
  end;

  { TcxGdiCanvasBasedImage }

  TcxGdiCanvasBasedImage = class(TcxCanvasBasedImage) // for internal use
  strict private
    FImage: TdxGPImage;
    FImageOwnership: TdxObjectOwnership;
  public
    constructor Create(ACanvas: TcxGdiBasedCanvas; AImage: TdxGPImage;
      AOwnership: TdxObjectOwnership = ooOwned);
    destructor Destroy; override;
    procedure Draw(const ATargetRect, ASourceRect: TRect;
      AAlpha: Byte = MaxByte); overload; override;
    //
    property Image: TdxGPImage read FImage;
  end;

  { TcxGdiCanvasBasedTextLayout }

  TcxGdiCanvasBasedTextLayout = class(TcxCanvasBasedTextLayout)
  // for internal use
  strict private
    procedure ReleaseFont; inline;
  protected
    FColor: TColor;
    FFont: TFont;
    FFontOwnership: TdxObjectOwnership;
    FForceEndEllipsis: Boolean;
    FTextRows: TcxTextRows;

    procedure DoCalculateLayout; override;
    procedure DoDraw(const R: TRect); override;
  public
    constructor Create(ACanvas: TcxCustomCanvas);
    destructor Destroy; override;
    procedure SetColor(ATextColor: TColor); override;
    procedure SetFont(AFont: TcxCanvasBasedFont;
      AOwnership: TdxObjectOwnership = ooReferenced); override;
    procedure SetFont(AFont: TFont;
      AOwnership: TdxObjectOwnership = ooReferenced); override;
    //
    property NativeFont: TFont read FFont;
  end;

const
  cxOppositeRotationAngle: array [TcxRotationAngle] of TcxRotationAngle = (ra0,
    raMinus90, raPlus90, ra180);
  cxRotationAngleToAngle: array [TcxRotationAngle] of Integer = (0, 90,
    -90, 180);

implementation

uses
  Math, cxGraphics, cxControls;

type
  TCustomControlAccess = class(TCustomControl);
  TdxCustomSmartImageAccess = class(TdxCustomSmartImage);

  TCanvasHelper = class helper for TCanvas
  public
    procedure SelectFont;
  end;

  { TCanvasHelper }

procedure TCanvasHelper.SelectFont;
begin
  RequiredState([csHandleValid, csFontValid]);
end;

{ TcxCanvasBasedResource }

constructor TcxCanvasBasedResource.Create(ACanvas: TcxCustomCanvas);
begin
  FCanvas := ACanvas;
end;

{ TcxCanvasBasedResourceCacheKey }

class function TcxCanvasBasedResourceCacheKey.Create(AOwner: Pointer;
  const ASize: TSize; AState, APart: Integer; ATargetDPI: Word)
  : TcxCanvasBasedResourceCacheKey;
begin
  Result.Owner := AOwner;
  Result.Size := ASize;
  Result.Part := APart;
  Result.State := AState;
  Result.TargetDPI := ATargetDPI;
end;

{ TcxCanvasBasedImage }

constructor TcxCanvasBasedImage.Create(ACanvas: TcxCustomCanvas;
  AWidth, AHeight: Integer);
begin
  inherited Create(ACanvas);
  FHeight := AHeight;
  FWidth := AWidth;
end;

procedure TcxCanvasBasedImage.Draw(const ATargetRect: TRect;
  AAlpha: Byte = MaxByte);
begin
  Draw(ATargetRect, ClientRect, AAlpha);
end;

function TcxCanvasBasedImage.Empty: Boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;

function TcxCanvasBasedImage.GetClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

{ TcxCanvasBasedTextLayout }

procedure TcxCanvasBasedTextLayout.Draw(const ARect: TRect);

  function CalculateOrigin(const ARect: TRect; const ASize: TSize): TPoint;
  begin
    if FFlags and CXTO_RIGHT = CXTO_RIGHT then
      Result.X := ARect.Right - ASize.cx
    else if FFlags and CXTO_CENTER_HORIZONTALLY = CXTO_CENTER_HORIZONTALLY then
      Result.X := (ARect.Right + ARect.Left - ASize.cx) div 2
    else
      Result.X := ARect.Left;

    if FFlags and CXTO_PREVENT_LEFT_EXCEED = CXTO_PREVENT_LEFT_EXCEED then
      Result.X := Max(Result.X, ARect.Left);

    if FFlags and CXTO_BOTTOM = CXTO_BOTTOM then
      Result.Y := ARect.Bottom - ASize.cy
    else if FFlags and CXTO_CENTER_VERTICALLY = CXTO_CENTER_VERTICALLY then
      Result.Y := (ARect.Bottom + ARect.Top - ASize.cy) div 2
    else
      Result.Y := ARect.Top;

    if FFlags and CXTO_PREVENT_TOP_EXCEED = CXTO_PREVENT_TOP_EXCEED then
      Result.Y := Max(Result.Y, ARect.Top);
  end;

  procedure DrawCore(const ARect: TRect);
  var
    AActualRect: TRect;
    ACenter: TPoint;
    AHeight: Integer;
    AWidth: Integer;
  begin
    if FRotation <> ra0 then
    begin
      Canvas.SaveWorldTransform;
      try
        ACenter := cxRectCenter(ARect);

        Canvas.ModifyWorldTransform(TXForm.CreateTranslateMatrix(ACenter.X,
          ACenter.Y));
        Canvas.ModifyWorldTransform
          (TXForm.CreateRotationMatrix(cxRotationAngleToAngle[FRotation]));
        Canvas.ModifyWorldTransform(TXForm.CreateTranslateMatrix(-ACenter.X,
          -ACenter.Y));

        AActualRect := ARect;
        if IsVertical then
        begin
          AWidth := cxRectWidth(ARect);
          AHeight := cxRectHeight(ARect);
          AActualRect := cxRectBounds(ACenter.X - AHeight div 2,
            ACenter.Y - AWidth div 2, AHeight, AWidth);
        end;

        DoDraw(AActualRect);
      finally
        Canvas.RestoreWorldTransform;
      end
    end
    else
      DoDraw(ARect);
  end;

var
  AClipRect: TRect;
  ASize: TSize;
  ATextBounds: TRect;
begin
  ASize := MeasureSize;
  ATextBounds := cxRectBounds(CalculateOrigin(ARect, ASize), ASize.cx,
    ASize.cy);
  if cxRectIntersect(AClipRect, ATextBounds, ARect) then
  begin
    if cxRectIsEqual(AClipRect, ATextBounds) then
      DrawCore(ATextBounds)
    else
    begin
      Canvas.SaveClipRegion;
      try
        Canvas.IntersectClipRect(AClipRect);
        DrawCore(ATextBounds);
      finally
        Canvas.RestoreClipRegion;
      end;
    end;
  end;
end;

function TcxCanvasBasedTextLayout.MeasureSize: TSize;
begin
  if FLayoutIsDirty then
  begin
    DoCalculateLayout;
    FLayoutIsDirty := False;
  end;
  Result := FSize;
end;

procedure TcxCanvasBasedTextLayout.SetFlags(ACxTOFlags: Integer);
const
  LayoutRelatedFlags = CXTO_WORDBREAK or CXTO_SINGLELINE or CXTO_END_ELLIPSIS or
    CXTO_AUTOINDENTS or CXTO_CHARBREAK;
begin
  if FFlags <> ACxTOFlags then
  begin
    if (FFlags and LayoutRelatedFlags) <> (ACxTOFlags and LayoutRelatedFlags)
    then
      FLayoutIsDirty := True;
    FFlags := ACxTOFlags;
    ApplyFlags;
  end;
end;

procedure TcxCanvasBasedTextLayout.SetFont(AFont: TFont;
  AOwnership: TdxObjectOwnership = ooReferenced);
begin
  SetFont(Canvas.CreateFonT(AFont), ooOwned);
  if AOwnership = ooOwned then
    AFont.Free;
end;

procedure TcxCanvasBasedTextLayout.SetLayoutConstraints(AMaxWidth,
  AMaxHeight: Integer; AMaxRowCount: Integer = 0);
begin
  AMaxWidth := Max(AMaxWidth, 0);
  AMaxHeight := Max(AMaxHeight, 0);
  AMaxRowCount := Max(AMaxRowCount, 0);

  if AMaxRowCount <> FMaxRowCount then
  begin
    FMaxRowCount := AMaxRowCount;
    FLayoutIsDirty := True;
  end;

  if AMaxWidth <> FMaxWidth then
  begin
    if (AMaxWidth < FSize.cx) then
      FLayoutIsDirty := True;
    if (AMaxWidth > FSize.cx) and (FFlags and CXTO_WORDBREAK <> 0) then
      FLayoutIsDirty := True;
    if (FFlags and CXTO_END_ELLIPSIS <> 0) then
      FLayoutIsDirty := True;
    FMaxWidth := AMaxWidth;
  end;

  if AMaxHeight <> FMaxHeight then
  begin
    if (AMaxHeight < FSize.cy) or (AMaxHeight > FSize.cy) and
      (FFlags and CXTO_END_ELLIPSIS <> 0) then
      FLayoutIsDirty := True;
    FMaxHeight := AMaxHeight;
  end;
end;

procedure TcxCanvasBasedTextLayout.SetLayoutConstraints(const R: TRect);
begin
  SetLayoutConstraints(cxRectWidth(R), cxRectHeight(R));
end;

procedure TcxCanvasBasedTextLayout.SetPaintOnGlass(AValue: Boolean);
begin
  FPaintOnGlass := AValue;
end;

procedure TcxCanvasBasedTextLayout.SetRotation(ARotation: TcxRotationAngle);
begin
  if FRotation <> ARotation then
  begin
    FRotation := ARotation;
    LayoutChanged;
  end;
end;

procedure TcxCanvasBasedTextLayout.SetText(const AText: string);
begin
  if AText <> FText then
  begin
    FText := AText;
    TextChanged;
  end;
end;

procedure TcxCanvasBasedTextLayout.ApplyFlags;
begin
  // do nothing;
end;

function TcxCanvasBasedTextLayout.CalculateRowCount(ARowHeight: Single)
  : Integer;
begin
  if FFlags and CXTO_EDITCONTROL <> 0 then
    Result := Trunc(FMaxHeight / ARowHeight)
  else
    Result := Ceil(FMaxHeight / ARowHeight);
end;

procedure TcxCanvasBasedTextLayout.DoDraw(const R: TRect);
begin
  // do nothing
end;

function TcxCanvasBasedTextLayout.IsVertical: Boolean;
begin
  Result := FRotation in [raPlus90, raMinus90];
end;

procedure TcxCanvasBasedTextLayout.TextChanged;
begin
  FLayoutIsDirty := True;
end;

procedure TcxCanvasBasedTextLayout.LayoutChanged;
begin
  FLayoutIsDirty := True;
end;

{ TcxCustomCanvas }

procedure TcxCustomCanvas.AfterConstruction;
begin
  inherited;
  FUseRightToLeftAlignment := bDefault;
end;

function TcxCustomCanvas.CheckIsValid
  (var AResource { : TcxCanvasBasedResource } ): Boolean;
begin
  if (TcxCanvasBasedResource(AResource) <> nil) and
    (TcxCanvasBasedResource(AResource).Canvas <> Self) then
    FreeAndNil(TcxCanvasBasedResource(AResource));
  Result := TcxCanvasBasedResource(AResource) <> nil;
end;

function TcxCustomCanvas.CreateImage(AGraphic: TGraphic): TcxCanvasBasedImage;
var
  ABitmap: TBitmap;
begin
  if AGraphic is TdxGPImage then
    Result := CreateImage(TdxGPImage(AGraphic))
  else
  begin
    ABitmap := cxGetAsBitmap(AGraphic);
    try
      Result := CreateImage(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;
end;

function TcxCustomCanvas.CreateImage(AGpImage: TdxGPImageHandle)
  : TcxCanvasBasedImage;
var
  ABitmap: TBitmap;
begin
  ABitmap := AGpImage.GetAsBitmap;
  try
    Result := CreateImage(ABitmap, afPremultiplied);
  finally
    ABitmap.Free;
  end;
end;

function TcxCustomCanvas.CreateImage(AGpImage: TdxGPImage;
  AOwnership: TdxObjectOwnership = ooCloned): TcxCanvasBasedImage;
var
  ABitmap: TBitmap;
  AHandle: TdxSmartImageCustomHandle;
begin
  AHandle := TdxCustomSmartImageAccess(AGpImage).Handle;
  if AHandle is TdxGPImageHandle then
    Result := CreateImage(TdxGPImageHandle(AHandle))
  else
  begin
    ABitmap := AGpImage.GetAsBitmap;
    try
      Result := CreateImage(ABitmap, afPremultiplied);
    finally
      ABitmap.Free;
    end;
  end;
  if AOwnership = ooOwned then
    AGpImage.Free;
end;

procedure TcxCustomCanvas.DrawNativeObject(const R: TRect;
  const ACacheKey: TcxCanvasBasedResourceCacheKey;
  AProc: TcxCanvasNativeDrawExProc);
begin
  if RectVisible(R) then
    DrawNativeObject(R, ACacheKey,
      procedure(ACanvas: TcxGdiBasedCanvas; const R: TRect)
      begin
        dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
        AProc(dxGPPaintCanvas, R);
        dxGPPaintCanvas.EndPaint;
      end);
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TBitmap; const ATargetRect: TRect;
ACache: PcxCanvasBasedImage);
begin
  DrawBitmap(ABitmap, ATargetRect, ABitmap.AlphaFormat, ACache);
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TBitmap; const ATargetRect: TRect;
AAlphaFormat: TAlphaFormat; ACache: PcxCanvasBasedImage);
begin
  DrawBitmap(ABitmap, ATargetRect, cxGetImageClientRect(ABitmap),
    AAlphaFormat, ACache);
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TBitmap;
const ATargetRect, ASourceRect: TRect; ACache: PcxCanvasBasedImage);
begin
  DrawBitmap(ABitmap, ATargetRect, ASourceRect, ABitmap.AlphaFormat, ACache);
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TBitmap;
const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage);
var
  AImage: TcxCanvasBasedImage;
begin
  if ACache <> nil then
  begin
    if not CheckIsValid(ACache^) then
      ACache^ := CreateImage(ABitmap, AAlphaFormat);
    ACache^.Draw(ATargetRect, ASourceRect);
  end
  else
  begin
    AImage := CreateImage(ABitmap, AAlphaFormat);
    try
      AImage.Draw(ATargetRect, ASourceRect);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TdxFastDIB;
const ATargetRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage);
begin
  DrawBitmap(ABitmap, ATargetRect, ABitmap.ClientRect, AAlphaFormat, ACache);
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TdxFastDIB;
const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage);
var
  AImage: TcxCanvasBasedImage;
begin
  if ACache <> nil then
  begin
    if not CheckIsValid(ACache^) then
      ACache^ := CreateImage(ABitmap, AAlphaFormat);
    ACache^.Draw(ATargetRect, ASourceRect);
  end
  else
  begin
    AImage := CreateImage(ABitmap, AAlphaFormat);
    try
      AImage.Draw(ATargetRect, ASourceRect);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TdxGpFastDIB;
const ATargetRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage = nil);
begin
  DrawBitmap(ABitmap, ATargetRect, ABitmap.ClientRect, AAlphaFormat, ACache);
end;

procedure TcxCustomCanvas.DrawBitmap(ABitmap: TdxGpFastDIB;
const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage = nil);
var
  AImage: TcxCanvasBasedImage;
begin
  if ACache <> nil then
  begin
    if not CheckIsValid(ACache^) then
      ACache^ := CreateImage(ABitmap, AAlphaFormat);
    ACache^.Draw(ATargetRect, ASourceRect);
  end
  else
  begin
    AImage := CreateImage(ABitmap, AAlphaFormat);
    try
      AImage.Draw(ATargetRect, ASourceRect);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TcxCustomCanvas.DrawImage(AImage: TdxGPImage;
const ATargetRect: TRect; ACache: PcxCanvasBasedImage = nil);
var
  ACanvasImage: TcxCanvasBasedImage;
begin
  if ACache <> nil then
  begin
    if not CheckIsValid(ACache^) then
      ACache^ := CreateImage(AImage);
    ACache^.Draw(ATargetRect);
  end
  else
  begin
    ACanvasImage := CreateImage(AImage);
    try
      ACanvasImage.Draw(ATargetRect);
    finally
      ACanvasImage.Free;
    end;
  end;
end;

procedure TcxCustomCanvas.DrawComplexFrame(const R: TRect;
ALeftTopColor, ARightBottomColor: TColor; ABorders: TcxBorders;
ABorderWidth: Integer);
begin
  FrameRect(R, ALeftTopColor, ABorderWidth, ABorders - [bRight, bBottom]);
  FrameRect(R, ARightBottomColor, ABorderWidth, ABorders - [bLeft, bTop]);
end;

procedure TcxCustomCanvas.DrawEdge(const R: TRect; ASunken, AOuter: Boolean;
ABorders: TcxBorders);
begin
  if ASunken then
  begin
    if AOuter then
      DrawComplexFrame(R, clBtnShadow, clBtnHighlight, ABorders)
    else
      DrawComplexFrame(R, cl3DDkShadow { clBtnText } ,
        cl3DLight { clBtnFace } , ABorders)
  end
  else
  begin
    if AOuter then
      DrawComplexFrame(R, cl3DLight { clBtnFace } ,
        cl3DDkShadow { clBtnText } , ABorders)
    else
      DrawComplexFrame(R, clBtnHighlight, clBtnShadow, ABorders);
  end;
end;

procedure TcxCustomCanvas.FrameRect(const R: TRect; AColor: TColor;
ALineWidth: Integer; ABorders: TcxBorders);
begin
  if IsRectEmpty(R) or (AColor = clNone) then
    Exit;
  if bLeft in ABorders then
    FillRect(cxRect(R.Left, R.Top, Min(R.Left + ALineWidth, R.Right),
      R.Bottom), AColor);
  if bRight in ABorders then
    FillRect(cxRect(Max(R.Right - ALineWidth, R.Left), R.Top, R.Right,
      R.Bottom), AColor);
  if bTop in ABorders then
    FillRect(cxRect(R.Left, R.Top, R.Right, Min(R.Top + ALineWidth, R.Bottom)
      ), AColor);
  if bBottom in ABorders then
    FillRect(cxRect(R.Left, Max(R.Bottom - ALineWidth, R.Top), R.Right,
      R.Bottom), AColor);
end;

procedure TcxCustomCanvas.FrameRect(const R: TRect; AColor: TdxAlphaColor;
ALineWidth: Integer; ABorders: TcxBorders);
var
  R1: TRect;
begin
  if IsRectEmpty(R) or (AColor = TdxAlphaColors.Empty) or
    (AColor = TdxAlphaColors.Transparent) then
    Exit;

  R1 := R;

  if bLeft in ABorders then
  begin
    FillRect(cxRect(R1.Left, R1.Top, Min(R1.Left + ALineWidth, R1.Right),
      R1.Bottom), AColor);
    Inc(R1.Left, ALineWidth);
  end;

  if bRight in ABorders then
  begin
    FillRect(cxRect(Max(R1.Right - ALineWidth, R1.Left), R1.Top, R1.Right,
      R1.Bottom), AColor);
    Dec(R1.Right, ALineWidth);
  end;

  if bTop in ABorders then
    FillRect(cxRect(R1.Left, R1.Top, R1.Right, Min(R1.Top + ALineWidth,
      R1.Bottom)), AColor);

  if bBottom in ABorders then
    FillRect(cxRect(R1.Left, Max(R1.Bottom - ALineWidth, R1.Top), R1.Right,
      R1.Bottom), AColor);
end;

procedure TcxCustomCanvas.FillPixel(X, Y: Integer; AColor: TColor);
begin
  FillRect(cxRect(X, Y, X + 1, Y + 1), AColor);
end;

procedure TcxCustomCanvas.FillPolygon(const P: array of TPoint; AColor: TColor);
begin
  Polygon(P, AColor, AColor);
end;

procedure TcxCustomCanvas.FillRect(const R: TRect; AColor: TColor;
AAlpha: Byte);
begin
  FillRect(R, dxColorToAlphaColor(AColor, AAlpha));
end;

procedure TcxCustomCanvas.FillRect(const R: TRect; AImage: TcxCanvasBasedImage);
var
  ACol, AColCount: Integer;
  ARect: TRect;
  ARow, ARowCount: Integer;
begin
  SaveClipRegion;
  try
    IntersectClipRect(R);
    AColCount := Ceil(cxRectWidth(R) / AImage.Width);
    ARowCount := Ceil(cxRectHeight(R) / AImage.Height);
    for ACol := 0 to AColCount - 1 do
    begin
      ARect := cxRectBounds(R.Left + ACol * AImage.Width, R.Top, AImage.Width,
        AImage.Height);
      for ARow := 0 to ARowCount - 1 do
      begin
        AImage.Draw(ARect);
        ARect := cxRectOffsetVert(ARect, AImage.Height);
      end;
    end;
  finally
    RestoreClipRegion;
  end;
end;

procedure TcxCustomCanvas.FillRect(const R: TRect; AImage: TdxGPImage;
ACache: PcxCanvasBasedImage = nil);
var
  ACanvasImage: TcxCanvasBasedImage;
begin
  if ACache <> nil then
  begin
    if not CheckIsValid(ACache^) then
      ACache^ := CreateImage(AImage);
    FillRect(R, ACache^);
  end
  else
  begin
    ACanvasImage := CreateImage(AImage);
    try
      FillRect(R, ACanvasImage);
    finally
      ACanvasImage.Free;
    end;
  end;
end;

procedure TcxCustomCanvas.Line(const P1, P2: TPoint; AColor: TColor;
APenWidth: Integer = 1; APenStyle: TPenStyle = psSolid);
begin
  Polyline([P1, P2], AColor, APenWidth, APenStyle);
end;

procedure TcxCustomCanvas.FillRectByGradient(const ARect: TRect;
AColor1, AColor2: TColor; AHorizontal: Boolean);
const
  Mode: array [Boolean] of TdxGpLinearGradientMode =
    (LinearGradientModeVertical, LinearGradientModeHorizontal);
begin
  FillRectByGradient(ARect, dxColorToAlphaColor(AColor1),
    dxColorToAlphaColor(AColor2), Mode[AHorizontal]);
end;

function TcxCustomCanvas.MoveWindowOrg(const P: TPoint): TPoint;
begin
  Result := WindowOrg;
  WindowOrg := cxPointOffset(Result, P, False);
end;

function TcxCustomCanvas.GetDefaultUseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;

function TcxCustomCanvas.GetIsLowColorsMode: Boolean;
begin
  Result := False;
end;

function TcxCustomCanvas.GetUseRightToLeftAlignment: Boolean;
begin
  if FUseRightToLeftAlignment = bDefault then
    Result := GetDefaultUseRightToLeftAlignment
  else
    Result := FUseRightToLeftAlignment = bTrue;
end;

procedure TcxCustomCanvas.SetUseRightToLeftAlignment(const Value: Boolean);
begin
  FUseRightToLeftAlignment := dxBooleanToDefaultBoolean(Value);
end;

{ TcxGdiBasedCanvas }

destructor TcxGdiBasedCanvas.Destroy;
begin
  FreeAndNil(FSavedWorldTransforms);
  inherited Destroy;
end;

function TcxGdiBasedCanvas.CreateFonT(AFont: TFont): TcxCanvasBasedFont;
begin
  Result := TcxGdiCanvasBasedFont.Create(AFont);
end;

function TcxGdiBasedCanvas.CreateImage(ABitmap: TBitmap;
AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage;
begin
  Result := TcxGdiCanvasBasedImage.Create(Self,
    TdxGPImage.CreateFromBitmap(ABitmap));
end;

function TcxGdiBasedCanvas.CreateImage(ABitmap: TdxCustomFastDIB;
AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage;
begin
  Result := TcxGdiCanvasBasedImage.Create(Self,
    TdxGPImage.CreateFromBits(ABitmap.Width, ABitmap.Height, ABitmap.Bits,
    AAlphaFormat));
end;

function TcxGdiBasedCanvas.CreateImage(AGpImage: TdxGPImage;
AOwnership: TdxObjectOwnership = ooCloned): TcxCanvasBasedImage;
begin
  Result := TcxGdiCanvasBasedImage.Create(Self, AGpImage, AOwnership);
end;

function TcxGdiBasedCanvas.CreateTextLayout: TcxCanvasBasedTextLayout;
begin
  Result := TcxGdiCanvasBasedTextLayout.Create(Self);
end;

procedure TcxGdiBasedCanvas.DrawNativeObject(const R: TRect;
const ACacheKey: TcxCanvasBasedResourceCacheKey;
AProc: TcxCanvasNativeDrawProc);
begin
  AProc(Self, R);
end;

procedure TcxGdiBasedCanvas.DrawBitmap(ABitmap: TBitmap;
const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage);
begin
  if ImageStretchQuality = isqHigh then
    inherited
  else if AAlphaFormat <> afIgnored then
    cxAlphaBlend(Handle, ABitmap, ATargetRect, ASourceRect)
  else
    cxStretchBlt(Handle, ABitmap.Canvas.Handle, ATargetRect,
      ASourceRect, SRCCOPY);
end;

procedure TcxGdiBasedCanvas.DrawBitmap(ABitmap: TdxFastDIB;
const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage);
begin
  if ImageStretchQuality = isqHigh then
    inherited
  else if AAlphaFormat <> afIgnored then
    cxAlphaBlend(Handle, ABitmap.DC, ATargetRect, ASourceRect)
  else
    cxStretchBlt(Handle, ABitmap.DC, ATargetRect, ASourceRect, SRCCOPY);
end;

procedure TcxGdiBasedCanvas.DrawBitmap(ABitmap: TdxGpFastDIB;
const ATargetRect, ASourceRect: TRect; AAlphaFormat: TAlphaFormat;
ACache: PcxCanvasBasedImage = nil);
const
  CompositionModeMap: array [TAlphaFormat] of TdxGpCompositionMode =
    (cmSourceCopy, cmSourceOver, cmSourceOver);
begin
  dxGPPaintCanvas.BeginPaint(Handle, ATargetRect);
  try
    dxGPPaintCanvas.InterpolationMode := dxGpSmoothStretchModeMap
      [ImageStretchQuality = isqHigh];
    dxGPPaintCanvas.CompositionMode := CompositionModeMap[AAlphaFormat];
    dxGPPaintCanvas.PixelOffsetMode := PixelOffsetModeHalf;
    dxGpDrawImage(dxGPPaintCanvas.Handle, ATargetRect, ASourceRect,
      ABitmap.Handle);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TcxGdiBasedCanvas.DrawImage(AImage: TdxGPImage;
const ATargetRect: TRect; ACache: PcxCanvasBasedImage = nil);
begin
  AImage.StretchDraw(Handle, ATargetRect);
end;

function TcxGdiBasedCanvas.RectVisible(const R: TRect): Boolean;
begin
  Result := Windows.RectVisible(Handle, R);
end;

procedure TcxGdiBasedCanvas.FillRectByGradient(const ARect: TRect;
AColor1, AColor2: TColor; AHorizontal: Boolean);
begin
  FillGradientRect(Handle, ARect, AColor1, AColor2, AHorizontal);
end;

procedure TcxGdiBasedCanvas.FillPixel(X, Y: Integer; AColor: TColor);
begin
  SetPixel(Handle, X, Y, ColorToRGB(AColor));
end;

procedure TcxGdiBasedCanvas.FillRect(const R: TRect; AColor: TColor);
begin
  Windows.FillRect(Handle, R, TdxSolidBrushCache.Get(AColor));
end;

procedure TcxGdiBasedCanvas.FillRect(const R: TRect; AColor: TdxAlphaColor);
begin
  dxGPPaintCanvas.BeginPaint(Handle, R);
  dxGPPaintCanvas.FillRectangle(R, AColor);
  dxGPPaintCanvas.EndPaint;
end;

procedure TcxGdiBasedCanvas.FillRect(const R: TRect;
AImage: TcxCanvasBasedImage);
begin
  FillRect(R, TcxGdiCanvasBasedImage(AImage).Image);
end;

procedure TcxGdiBasedCanvas.FillRect(const R: TRect; AImage: TdxGPImage;
ACache: PcxCanvasBasedImage = nil);
begin
  dxGpTilePart(Handle, R, AImage.ClientRect, AImage.Handle);
end;

procedure TcxGdiBasedCanvas.FillRectByGradient(const ARect: TRect;
AColor1, AColor2: TdxAlphaColor; AMode: TdxGpLinearGradientMode);
begin
  dxGPPaintCanvas.BeginPaint(Handle, ARect);
  dxGPPaintCanvas.FillRectangleByGradient(ARect, AColor1, AColor2, AMode);
  dxGPPaintCanvas.EndPaint;
end;

procedure TcxGdiBasedCanvas.FocusRectangle(const R: TRect);
var
  AOldMode: Integer;
begin
  AOldMode := SetMapMode(Handle, MM_TEXT);
  Windows.DrawFocusRect(Handle, R);
  SetMapMode(Handle, AOldMode);
end;

procedure TcxGdiBasedCanvas.ModifyWorldTransform(const AForm: TXForm);
begin
  SetGraphicsMode(Handle, GM_ADVANCED);
  Windows.ModifyWorldTransform(Handle, AForm, MWT_LEFTMULTIPLY);
end;

procedure TcxGdiBasedCanvas.SaveWorldTransform;
var
  ATransform: TXForm;
begin
  if FSavedWorldTransforms = nil then
    FSavedWorldTransforms := TStack<TXForm>.Create;
  GetWorldTransform(Handle, ATransform);
  FSavedWorldTransforms.Push(ATransform);
end;

procedure TcxGdiBasedCanvas.RestoreWorldTransform;
begin
  SetWorldTransform(Handle, FSavedWorldTransforms.Pop);
end;

procedure TcxGdiBasedCanvas.ExcludeClipRect(const R: TRect);
begin
  Windows.ExcludeClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TcxGdiBasedCanvas.IntersectClipRect(const R: TRect);
begin
  Windows.IntersectClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
end;

function TcxGdiBasedCanvas.GetDCOrigin: TPoint;
var
  AWindowOrg, AViewportOrg: TPoint;
begin
  GetWindowOrgEx(Handle, AWindowOrg);
  GetViewportOrgEx(Handle, AViewportOrg);
  Result := Point(AViewportOrg.X - AWindowOrg.X, AViewportOrg.Y - AWindowOrg.Y);
end;

procedure TcxGdiBasedCanvas.SaveBaseOrigin;
begin
  GetWindowOrgEx(Handle, FBaseOrigin);
  SetBrushOrgEx(Handle, BaseOrigin.X, BaseOrigin.Y, @FBrushOrigin);
end;

procedure TcxGdiBasedCanvas.RestoreBaseOrigin;
begin
  SetBrushOrgEx(Handle, FBrushOrigin.X, FBrushOrigin.Y, nil);
  FBrushOrigin := cxNullPoint;
  FBaseOrigin := cxNullPoint;
end;

function TcxGdiBasedCanvas.GetIsLowColorsMode: Boolean;
begin
  Result := (GetDeviceCaps(Handle, BITSPIXEL) <= 16) or
    (GetDeviceCaps(Handle, NUMCOLORS) > 1) or
    (GetDeviceCaps(Handle, TECHNOLOGY) = DT_RASPRINTER);
end;

function TcxGdiBasedCanvas.GetViewportOrg: TPoint;
begin
  GetViewportOrgEx(Handle, Result);
end;

procedure TcxGdiBasedCanvas.SetViewportOrg(const P: TPoint);
begin
  SetViewportOrgEx(Handle, P.X, P.Y, nil);
end;

function TcxGdiBasedCanvas.GetWindowOrg: TPoint;
begin
  GetWindowOrgEx(Handle, Result);
  Result := cxPointOffset(Result, FBaseOrigin, False);
end;

procedure TcxGdiBasedCanvas.SetWindowOrg(const P: TPoint);
begin
  SetWindowOrgEx(Handle, P.X + FBaseOrigin.X, P.Y + FBaseOrigin.Y, nil);
end;

{ TcxGdiCanvasBasedFont }

constructor TcxGdiCanvasBasedFont.Create(AFont: TFont);
begin
  NativeFont := TFont.Create;
  NativeFont.Assign(AFont);
end;

destructor TcxGdiCanvasBasedFont.Destroy;
begin
  FreeAndNil(NativeFont);
  inherited;
end;

function TcxGdiCanvasBasedFont.Equals(Obj: TObject): Boolean;
begin
  Result := (ClassType = Obj.ClassType) and dxAreFontsEqual(NativeFont,
    TcxGdiCanvasBasedFont(Obj).NativeFont);
end;

{ TcxGdiCanvasBasedTextLayout }

constructor TcxGdiCanvasBasedTextLayout.Create(ACanvas: TcxCustomCanvas);
begin
  FCanvas := ACanvas;
end;

destructor TcxGdiCanvasBasedTextLayout.Destroy;
begin
  ReleaseFont;
  cxResetTextRows(FTextRows);
  inherited Destroy;
end;

procedure TcxGdiCanvasBasedTextLayout.SetColor(ATextColor: TColor);
begin
  FColor := ATextColor;
end;

procedure TcxGdiCanvasBasedTextLayout.SetFont(AFont: TcxCanvasBasedFont;
AOwnership: TdxObjectOwnership);
begin
  if AOwnership = ooOwned then
    try
      if not dxAreFontsEqual(TcxGdiCanvasBasedFont(AFont).NativeFont, NativeFont)
      then
      begin
        ReleaseFont;
        ExchangePointers(TcxGdiCanvasBasedFont(AFont).NativeFont, FFont);
      end;
    finally
      AFont.Free;
    end
  else
    SetFont(TcxGdiCanvasBasedFont(AFont).NativeFont, AOwnership);
end;

procedure TcxGdiCanvasBasedTextLayout.SetFont(AFont: TFont;
AOwnership: TdxObjectOwnership);
begin
  if not dxAreFontsEqual(AFont, NativeFont) then
  begin
    ReleaseFont;
    if AOwnership = ooCloned then
    begin
      FFont := TFont.Create;
      FFont.Assign(AFont);
      FFontOwnership := ooOwned;
    end
    else
    begin
      FFont := AFont;
      FFontOwnership := AOwnership;
    end;
  end
  else if AOwnership = ooOwned then
    AFont.Free;
end;

procedure TcxGdiCanvasBasedTextLayout.DoCalculateLayout;
var
  AMaxHeight: Integer;
  AMaxWidth: Integer;
  AMeasureDC: HDC;
  ARect: TRect;
  ARowCount: Integer;
  ATextParams: TcxTextParams;
begin
  TdxTextMeasurer.MeasureCanvas.Font.Assign(NativeFont);
  TdxTextMeasurer.MeasureCanvas.SelectFont;
  AMeasureDC := TdxTextMeasurer.MeasureCanvas.Handle;
  AMaxHeight := FMaxHeight;
  AMaxWidth := FMaxWidth;

  if IsVertical then
  begin
    SetGraphicsMode(AMeasureDC, GM_ADVANCED);
    SetWorldTransform(AMeasureDC, TXForm.CreateRotationMatrix(-90));
    ExchangeIntegers(AMaxHeight, AMaxWidth);
  end;

  if (AMaxWidth = 0) and (FMaxRowCount > 0) and
    (FFlags and CXTO_WORDBREAK = CXTO_WORDBREAK) then
    ARect := TdxTextMeasurer.TextRectTO(AMeasureDC, FText, FMaxRowCount, FFlags)
  else
    ARect := cxRect(0, 0, IfThen(AMaxWidth > 0, AMaxWidth, MaxWord),
      AMaxHeight);

  ATextParams := cxCalcTextParams(AMeasureDC, FFlags or CXTO_CALCROWCOUNT);
  ATextParams.CharBreak := FFlags and CXTO_CHARBREAK <> 0;
  while True do
  begin
    FForceEndEllipsis := not cxMakeTextRows(AMeasureDC, PChar(FText),
      Length(FText), ARect, ATextParams, FTextRows, ARowCount);

    FRowCount := ARowCount;
    if FMaxRowCount > 0 then
      FRowCount := Min(FRowCount, FMaxRowCount);
    if AMaxHeight > 0 then
      FRowCount := Min(FRowCount, CalculateRowCount(ATextParams.FullRowHeight));

    FSize.cx := cxGetLongestTextRowWidth(FTextRows, FRowCount);
    FSize.cy := FRowCount * ATextParams.FullRowHeight;
    if cxRectWidth(ARect) < FSize.cx then
      ARect.Right := ARect.Left + FSize.cx
    else
      Break;
  end;

  if FFlags and CXTO_END_ELLIPSIS = CXTO_END_ELLIPSIS then
  begin
    FForceEndEllipsis := FForceEndEllipsis or (FRowCount < ARowCount) or
      (AMaxWidth > 0) and (FSize.cx > AMaxWidth);
    if AMaxWidth > 0 then
      FSize.cx := Min(FSize.cx, AMaxWidth);
    if AMaxHeight > 0 then
      FSize.cy := Min(FSize.cy, AMaxHeight);
  end;

  if IsVertical then
  begin
    SetWorldTransform(AMeasureDC, TXForm.CreateIdentityMatrix);
    FSize := cxSize(FSize.cy, FSize.cx);
  end;
end;

procedure TcxGdiCanvasBasedTextLayout.DoDraw(const R: TRect);
var
  APrevFont: HFONT;
  APrevFontColor: TColor;
  ATextParams: TcxTextParams;
  DC: HDC;
begin
  DC := TcxGdiBasedCanvas(FCanvas).Handle;

  APrevFont := SelectObject(DC, NativeFont.Handle);
  APrevFontColor := GetTextColor(DC);
  SetTextColor(DC, ColorToRGB(FColor));

  ATextParams := cxCalcTextParams(DC, FFlags);
  cxPlaceTextRows(DC, R, ATextParams, FTextRows, FRowCount);

  if FPaintOnGlass then
    cxTextRowsOutAlphaBlend(DC, R, ATextParams, FTextRows, FRowCount,
      FForceEndEllipsis)
  else
    cxTextRowsOutHighlight(DC, R, ATextParams, FTextRows, FRowCount, 0, 0,
      clNone, clNone, FForceEndEllipsis);

  SetTextColor(DC, APrevFontColor);
  SelectObject(DC, APrevFont);
end;

procedure TcxGdiCanvasBasedTextLayout.ReleaseFont;
begin
  if FFontOwnership = ooOwned then
    FFont.Free;
  FFont := nil;
end;

{ TcxGdiCanvasBasedImage }

constructor TcxGdiCanvasBasedImage.Create(ACanvas: TcxGdiBasedCanvas;
AImage: TdxGPImage; AOwnership: TdxObjectOwnership = ooOwned);
begin
  inherited Create(ACanvas, AImage.Width, AImage.Height);

  if AOwnership = ooCloned then
  begin
    FImage := AImage.Clone;
    FImageOwnership := ooOwned;
  end
  else
  begin
    FImage := AImage;
    FImageOwnership := AOwnership;
  end;
end;

destructor TcxGdiCanvasBasedImage.Destroy;
begin
  if FImageOwnership = ooOwned then
    FreeAndNil(FImage);
  inherited;
end;

procedure TcxGdiCanvasBasedImage.Draw(const ATargetRect, ASourceRect: TRect;
AAlpha: Byte);
begin
  FImage.StretchDraw(TcxGdiBasedCanvas(FCanvas).Handle, ATargetRect,
    ASourceRect, AAlpha);
end;

end.
