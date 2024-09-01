{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSkins Library }
{ }
{ Copyright (c) 2006-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING }
{ VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSkinscxSchedulerPainter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, cxSchedulerCustomResourceView, cxDateUtils,
  cxSchedulerCustomControls, dxSkinsCore, Math,
  cxLookAndFeels, cxLookAndFeelPainters, cxGraphics, Graphics, cxGeometry,
  cxSchedulerUtils, cxScheduler, cxClasses,
  cxSchedulerAgendaView, dxSkinInfo, dxSkinsStrs, dxGDIPlusAPI,
  dxGDIPlusClasses, Contnrs;

type

  { TcxSchedulerExternalSkinPainter }

  TcxSchedulerExternalSkinPainter = class(TcxSchedulerExternalPainter)
  strict private
  const
    MaxEventsCacheCapacity = 256;
  strict private
    FEventsCache: TObjectList;

    procedure DrawClippedElement(ACanvas: TcxCanvas; AElement: TdxSkinElement;
      ABorders: TcxBorders; R: TRect);
    function GetEventSelectionOffsets(AViewInfo
      : TcxSchedulerEventCellViewInfo): TRect;
    function GetSkinInfo(out AInfo: TdxSkinInfo): Boolean; inline;
    function IsSkinAvailable: Boolean;
  protected
    function DoDrawEvent(AViewInfo: TcxSchedulerEventCellViewInfo)
      : Boolean; virtual;
    procedure DoDrawEventCore(AViewInfo: TcxSchedulerEventCellViewInfo;
      const ABounds: TRect; AAppointment, AAppointmentMask: TdxSkinElement);
    procedure DrawCustomCurrentTime(ACanvas: TcxCanvas; AColor: TColor;
      AStart: TDateTime; const ABounds: TRect;
      AUseRightToLeftAlignment: Boolean); override;
    function GetSeparatorColor(AViewInfo: TcxSchedulerEventCellViewInfo;
      ABorderColor: TdxSkinColor): TColor;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure FlushCache; override;

    procedure AdjustEditStyleViewParams(var AViewParams
      : TcxViewParams); override;
    procedure DoCustomDrawButton(AViewInfo
      : TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean); override;
    procedure DrawAllDayArea(ACanvas: TcxCanvas; const ARect: TRect;
      ABorderColor: TColor; ABorders: TcxBorders; AViewParams: TcxViewParams;
      ASelected: Boolean; ATransparent: Boolean); override;
    function DrawCurrentTimeFirst: Boolean; override;
    procedure DrawEvent(AViewInfo: TcxSchedulerEventCellViewInfo); override;
    procedure DrawEventAsProgressText(AViewInfo: TcxSchedulerEventCellViewInfo;
      AContent: TRect; AProgressRect: TRect; const AText: string); override;
    procedure DrawEventLabel(AViewInfo: TcxSchedulerEventCellViewInfo;
      const R: TRect; AColor: TColor); override;
    procedure DrawTimeGridCurrentTime(ACanvas: TcxCanvas; AColor: TColor;
      const ATimeLineRect: TRect); override;
    procedure DrawTimeGridHeader(ACanvas: TcxCanvas; ABorderColor: TColor;
      AViewInfo: TcxSchedulerCustomViewInfoItem; ABorders: TcxBorders;
      ASelected: Boolean); override;
    function DrawTimeGridTimeScaleTicks: Boolean; override;
    procedure DrawTimeLine(ACanvas: TcxCanvas; const ARect: TRect;
      AViewParams: TcxViewParams; ABorders: TcxBorders;
      ABorderColor: TColor); override;
    procedure DrawTimeRulerBackground(ACanvas: TcxCanvas; const ARect: TRect;
      ABorders: TcxBorders; ABorderColor: TColor; AViewParams: TcxViewParams;
      ATransparent: Boolean); override;
    procedure DrawShadow(ACanvas: TcxCanvas; const ARect, AVisibleRect: TRect;
      AScaleFactor: TdxScaleFactor); override;
    function DrawShadowFirst: Boolean; override;
    function GetEventSelectionBorderSize(AViewInfo
      : TcxSchedulerEventCellViewInfo): Integer; override;
    function GetEventBorderColor(AViewInfo: TcxSchedulerEventCellViewInfo)
      : TColor; override;
    function GetEventBorderSize(AViewInfo: TcxSchedulerEventCellViewInfo)
      : Integer; override;
    function GetEventLabelSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function GetEventSelectionExtends: TRect; override;
    function GetEventSelectionExtends(AViewInfo: TcxSchedulerEventCellViewInfo)
      : TRect; override;
    function MoreButtonSize(ASize: TSize; AScaleFactor: TdxScaleFactor = nil)
      : TSize; override;
    function NeedDrawSelection: Boolean; override;
  end;

implementation

uses
  Types, cxControls, cxSchedulerStorage, dxDPIAwareUtils, dxCoreGraphics;

const
  cxHeaderStateToButtonState: array [Boolean] of TcxButtonState =
    (cxbsNormal, cxbsHot);

type
  TdxSkinInfoAccess = class(TdxSkinInfo);
  TcxSchedulerEventCellViewInfoAccess = class(TcxSchedulerEventCellViewInfo);

  { TcxSchedulerSkinPainterEventCacheItem }

  TcxSchedulerSkinPainterEventCacheItem = class
  public
    Color: TColor;
    Image: GpImage;
    Selected: Boolean;
    Size: TSize;
    TargetDPI: Integer;
    UseRTL: Boolean;

    constructor Create(const ASize: TSize;
      AViewInfo: TcxSchedulerEventCellViewInfo);
    destructor Destroy; override;
    procedure Draw(DC: HDC; const R: TRect);
    function Equals(const ASize: TSize;
      AViewInfo: TcxSchedulerEventCellViewInfo): Boolean; reintroduce;
  end;

  { TcxSchedulerSkinPainterEventCacheItem }

constructor TcxSchedulerSkinPainterEventCacheItem.Create(const ASize: TSize;
  AViewInfo: TcxSchedulerEventCellViewInfo);
begin
  Size := ASize;
  Color := AViewInfo.ViewParams.Color;
  TargetDPI := AViewInfo.ScaleFactor.TargetDPI;
  Selected := AViewInfo.Selected;
  UseRTL := AViewInfo.UseRightToLeftAlignment;
  Image := dxGpCreateBitmap(ASize);
end;

destructor TcxSchedulerSkinPainterEventCacheItem.Destroy;
begin
  GdipDisposeImage(Image);
  inherited;
end;

procedure TcxSchedulerSkinPainterEventCacheItem.Draw(DC: HDC; const R: TRect);
begin
  dxGPPaintCanvas.BeginPaint(DC, R);
  try
    GdipCheck(GdipDrawImageRectI(dxGPPaintCanvas.Handle, Image, R.Left, R.Top,
      R.Right - R.Left, R.Bottom - R.Top));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TcxSchedulerSkinPainterEventCacheItem.Equals(const ASize: TSize;
  AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
begin
  Result := cxSizeIsEqual(ASize, Size) and (Color = AViewInfo.ViewParams.Color)
    and (Selected = AViewInfo.Selected) and
    (UseRTL = AViewInfo.UseRightToLeftAlignment) and
    (TargetDPI = AViewInfo.ScaleFactor.TargetDPI);
end;

{ TcxSchedulerSkinViewItemsPainter }

destructor TcxSchedulerExternalSkinPainter.Destroy;
begin
  FreeAndNil(FEventsCache);
  inherited;
end;

procedure TcxSchedulerExternalSkinPainter.AfterConstruction;
begin
  inherited;
  FEventsCache := TObjectList.Create;
  FEventsCache.Capacity := MaxEventsCacheCapacity;
end;

procedure TcxSchedulerExternalSkinPainter.DrawClippedElement(ACanvas: TcxCanvas;
  AElement: TdxSkinElement; ABorders: TcxBorders; R: TRect);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(R);
    AElement.Draw(ACanvas.Handle, cxRectExcludeBorders(R,
      AElement.Image.Margins.Margin, ABorders));
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxSchedulerExternalSkinPainter.IsSkinAvailable: Boolean;
begin
  Result := Painter.LookAndFeelStyle = lfsSkin;
end;

function TcxSchedulerExternalSkinPainter.GetSkinInfo
  (out AInfo: TdxSkinInfo): Boolean;
begin
  Result := (Painter <> nil) and Painter.GetPainterData(AInfo);
end;

function TcxSchedulerExternalSkinPainter.MoreButtonSize(ASize: TSize;
  AScaleFactor: TdxScaleFactor = nil): TSize;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    Result := dxSkinGetElementSize(ASkinInfo.SchedulerMoreButton, AScaleFactor)
  else
    Result := inherited MoreButtonSize(ASize, AScaleFactor);
end;

procedure TcxSchedulerExternalSkinPainter.AdjustEditStyleViewParams
  (var AViewParams: TcxViewParams);
begin
  if IsSkinAvailable then
  begin
    AViewParams.Color := clWindow;
    AViewParams.TextColor := clWindowText;
  end;
end;

procedure TcxSchedulerExternalSkinPainter.DoCustomDrawButton
  (AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean);
var
  ASkinInfo: TdxSkinInfo;
begin
  inherited DoCustomDrawButton(AViewInfo, ADone);

  if not ADone and GetSkinInfo(ASkinInfo) then
  begin
    ADone := ASkinInfo.SchedulerMoreButton <> nil;
    if ADone then
      ASkinInfo.SchedulerMoreButton.Draw(AViewInfo.Canvas.Handle,
        AViewInfo.Bounds, AViewInfo.ScaleFactor, Byte(AViewInfo.IsDown));
  end;
end;

procedure TcxSchedulerExternalSkinPainter.DrawAllDayArea(ACanvas: TcxCanvas;
  const ARect: TRect; ABorderColor: TColor; ABorders: TcxBorders;
  AViewParams: TcxViewParams; ASelected: Boolean; ATransparent: Boolean);
var
  AElement: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerAllDayArea[ASelected]
  else
    AElement := nil;

  if AElement <> nil then
    DrawClippedElement(ACanvas, AElement, ABorders, ARect)
  else
    inherited DrawAllDayArea(ACanvas, ARect, ABorderColor, ABorders,
      AViewParams, ASelected, ATransparent);
end;

procedure TcxSchedulerExternalSkinPainter.DrawCustomCurrentTime
  (ACanvas: TcxCanvas; AColor: TColor; AStart: TDateTime; const ABounds: TRect;
  AUseRightToLeftAlignment: Boolean);

  procedure DrawRotatedIndicator(AElement: TdxSkinElement;
    const AIndicatorRect: TRect);
  var
    ABitmap: TcxBitmap;
    I: Integer;
  begin
    ABitmap := TcxBitmap.CreateSize(AIndicatorRect);
    try
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect,
        AIndicatorRect.TopLeft, SRCCOPY);
      ABitmap.Rotate(ra180, False);
      for I := 0 to 1 do
        AElement.Draw(ABitmap.Canvas.Handle, cxRectOffset(AIndicatorRect,
          -AIndicatorRect.Left, -AIndicatorRect.Top), I);
      ABitmap.Rotate(ra180, False);
      cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, AIndicatorRect,
        cxNullPoint, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end;

var
  AElement: TdxSkinElement;
  AIndicatorRect: TRect;
  ASkinInfo: TdxSkinInfo;
  Y, I: Integer;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerCurrentTimeIndicator
  else
    AElement := nil;

  if AElement <> nil then
  begin
    Y := Trunc(ABounds.Top + ((dxTimeOf(Now) - dxTimeOf(AStart)) *
      cxRectHeight(ABounds)) / HourToTime);

    AIndicatorRect := cxRectSetTop(ABounds, Y, 0);
    AIndicatorRect := cxRectCenterVertically(AIndicatorRect,
      dxSkinGetElementSize(AElement, dxDefaultScaleFactor).cy);
    Inc(AIndicatorRect.Left, 5);
    Dec(AIndicatorRect.Right);

    if cxRectIntersect(AIndicatorRect, ABounds) then
    begin
      if AUseRightToLeftAlignment then
        DrawRotatedIndicator(AElement, TdxRightToLeftLayoutConverter.ConvertRect
          (AIndicatorRect, ABounds))
      else
        for I := 0 to 1 do
          AElement.Draw(ACanvas.Handle, AIndicatorRect, I);
    end;
  end
  else
    inherited DrawCustomCurrentTime(ACanvas, AColor, AStart, ABounds,
      AUseRightToLeftAlignment);
end;

function TcxSchedulerExternalSkinPainter.NeedDrawSelection: Boolean;
begin
  Result := not IsSkinAvailable;
end;

function TcxSchedulerExternalSkinPainter.DrawCurrentTimeFirst: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerExternalSkinPainter.DrawEvent
  (AViewInfo: TcxSchedulerEventCellViewInfo);
begin
  if (AViewInfo is TcxSchedulerAgendaViewEventCellViewInfo) or
    not DoDrawEvent(AViewInfo) then
    inherited DrawEvent(AViewInfo);
end;

function TcxSchedulerExternalSkinPainter.DoDrawEvent
  (AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;

  function GetBounds(AAppointment: TdxSkinElement): TRect;
  begin
    cxRectIntersect(Result, AViewInfo.Bounds, cxRectInflate(AViewInfo.ClipRect,
      AAppointment.Image.Margins.Margin));
    if AViewInfo.Selected then
      Result := cxRectInflate(Result, GetEventSelectionOffsets(AViewInfo));
  end;

var
  AAppointment: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  Result := False;
  if not AViewInfo.Transparent and GetSkinInfo(ASkinInfo) then
  begin
    AAppointment := ASkinInfo.SchedulerAppointment
      [IsRectEmpty(AViewInfo.TimeLineRect)];
    if AAppointment <> nil then
    begin
      DoDrawEventCore(AViewInfo, GetBounds(AAppointment), AAppointment,
        ASkinInfo.SchedulerAppointmentMask);
      AViewInfo.SeparatorColor := GetSeparatorColor(AViewInfo,
        ASkinInfo.SchedulerAppointmentBorder);
      AViewInfo.Transparent := True;
      Result := True;
    end;
  end;
end;

procedure TcxSchedulerExternalSkinPainter.DoDrawEventCore
  (AViewInfo: TcxSchedulerEventCellViewInfo; const ABounds: TRect;
  AAppointment, AAppointmentMask: TdxSkinElement);
const
  SelectedToElementState: array [Boolean] of TdxSkinElementState =
    (esNormal, esHot);

  function IsDefaultContentColor(AColor: TColor): Boolean;
  begin
    if not cxColorIsValid(AColor) then
      Result := True
    else if AViewInfo.Selected then
      Result := AColor = Painter.DefaultSelectionColor
    else if AViewInfo.ViewStyle = svsClassic then
      Result := AColor = Painter.DefaultSchedulerEventColorClassic
        (AViewInfo.Event.IsAllDayOrLonger)
    else
      Result := AColor = Painter.DefaultSchedulerEventColor
        (AViewInfo.Event.IsAllDayOrLonger);
  end;

  procedure ColorizeContent(ACanvas: TdxGPCanvas; const ABounds: TRect;
    AColor: TdxAlphaColor);
  const
    ImageIndexMap: array [Boolean] of Integer = (1, 0);
  var
    AMaskBmp: TcxAlphaDIB;
    ARegion: TcxRegionHandle;
  begin
    AMaskBmp := TcxAlphaDIB.Create(ABounds);
    try
      AAppointmentMask.RightToLeftDependentDraw(AMaskBmp.CanvasHandle,
        AMaskBmp.ClientRect, AViewInfo.ScaleFactor,
        AViewInfo.UseRightToLeftAlignment,
        ImageIndexMap[IsRectEmpty(AViewInfo.TimeLineRect)]);

      ARegion := cxCreateRegionFromBitmap(AMaskBmp.Pixels, AMaskBmp.Width,
        AMaskBmp.Height, clBlack);
      if ARegion <> 0 then
        try
          OffsetRgn(ARegion, ABounds.Left, ABounds.Top);
          ACanvas.SetClipRegion(ARegion, gmIntersect);
          ACanvas.FillRectangle(ABounds, AColor);
        finally
          DeleteObject(ARegion);
        end
      else
        ACanvas.FillRectangle(ABounds, AColor);
    finally
      AMaskBmp.Free;
    end;
  end;

  function CreateCacheItem(const ASize: TSize)
    : TcxSchedulerSkinPainterEventCacheItem;
  var
    AGraphics: GpGraphics;
  begin
    Result := TcxSchedulerSkinPainterEventCacheItem.Create(ASize, AViewInfo);
    GdipCheck(GdipGetImageGraphicsContext(Result.Image, AGraphics));
    try
      dxGPPaintCanvas.BeginPaint(AGraphics);
      try
        AAppointment.RightToLeftDependentDraw(dxGPPaintCanvas, cxRect(ASize),
          AViewInfo.ScaleFactor, AViewInfo.UseRightToLeftAlignment, 0,
          SelectedToElementState[AViewInfo.Selected]);
        if not IsDefaultContentColor(AViewInfo.ViewParams.Color) then
        begin
          if AAppointmentMask <> nil then
            ColorizeContent(dxGPPaintCanvas, cxRect(ASize),
              dxColorToAlphaColor(Result.Color, 120));
        end;
      finally
        dxGPPaintCanvas.EndPaint;
      end;
    finally
      GdipCheck(GdipDeleteGraphics(AGraphics));
    end;
  end;

  function GetCacheItem(const ASize: TSize)
    : TcxSchedulerSkinPainterEventCacheItem;
  var
    I: Integer;
  begin
    for I := 0 to FEventsCache.Count - 1 do
    begin
      Result := TcxSchedulerSkinPainterEventCacheItem(FEventsCache.List[I]);
      if Result.Equals(ASize, AViewInfo) then
        Exit;
    end;
    if FEventsCache.Count >= MaxEventsCacheCapacity then
      FEventsCache.Delete(0);
    Result := CreateCacheItem(ASize);
    FEventsCache.Add(Result);
  end;

var
  ASize: TSize;
begin
  ASize := cxSize(ABounds);
  if (ASize.cx > 0) and (ASize.cy > 0) then
    GetCacheItem(ASize).Draw(AViewInfo.Canvas.Handle, ABounds)
end;

procedure TcxSchedulerExternalSkinPainter.DrawEventAsProgressText
  (AViewInfo: TcxSchedulerEventCellViewInfo; AContent: TRect;
  AProgressRect: TRect; const AText: string);
begin
  if IsSkinAvailable then
    cxDrawText(AViewInfo.Canvas.Handle, AText, AContent,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE)
  else
    inherited DrawEventAsProgressText(AViewInfo, AContent,
      AProgressRect, AText);
end;

procedure TcxSchedulerExternalSkinPainter.DrawEventLabel
  (AViewInfo: TcxSchedulerEventCellViewInfo; const R: TRect; AColor: TColor);
var
  AElement: TdxSkinElement;
  APrevSmoothingMode: TdxGPSmoothingMode;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerLabelCircle
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawEventLabel(AViewInfo, R, AColor)
  else
  begin
    AElement.Draw(AViewInfo.Canvas.Handle, R, 0, esNormal);

    dxGPPaintCanvas.BeginPaint(AViewInfo.Canvas.Handle, R);
    try
      APrevSmoothingMode := dxGPPaintCanvas.SmoothingMode;
      dxGPPaintCanvas.SmoothingMode := smAntiAlias;
      dxGPPaintCanvas.Ellipse(R, TdxAlphaColors.Empty,
        dxMakeAlphaColor(AColor, 240));
      dxGPPaintCanvas.SmoothingMode := APrevSmoothingMode;
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TcxSchedulerExternalSkinPainter.DrawTimeGridCurrentTime
  (ACanvas: TcxCanvas; AColor: TColor; const ATimeLineRect: TRect);
var
  AElement: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerTimeGridCurrentTimeIndicator
  else
    AElement := nil;

  if AElement <> nil then
    AElement.Draw(ACanvas.Handle, ATimeLineRect)
  else
    inherited DrawTimeGridCurrentTime(ACanvas, AColor, ATimeLineRect);
end;

procedure TcxSchedulerExternalSkinPainter.DrawTimeGridHeader(ACanvas: TcxCanvas;
  ABorderColor: TColor; AViewInfo: TcxSchedulerCustomViewInfoItem;
  ABorders: TcxBorders; ASelected: Boolean);
var
  AElement: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerTimeGridHeader[ASelected]
  else
    AElement := nil;

  if AElement <> nil then
    DrawClippedElement(ACanvas, AElement, ABorders, AViewInfo.Bounds)
  else
    inherited DrawTimeGridHeader(ACanvas, ABorderColor, AViewInfo, ABorders,
      ASelected);
end;

function TcxSchedulerExternalSkinPainter.DrawTimeGridTimeScaleTicks: Boolean;
begin
  Result := not IsSkinAvailable;
end;

procedure TcxSchedulerExternalSkinPainter.DrawTimeLine(ACanvas: TcxCanvas;
  const ARect: TRect; AViewParams: TcxViewParams; ABorders: TcxBorders;
  ABorderColor: TColor);
var
  AElement: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerTimeLine
  else
    AElement := nil;

  if AElement <> nil then
    DrawClippedElement(ACanvas, AElement, ABorders, ARect)
  else
    inherited DrawTimeLine(ACanvas, ARect, AViewParams, ABorders, ABorderColor);
end;

procedure TcxSchedulerExternalSkinPainter.DrawTimeRulerBackground
  (ACanvas: TcxCanvas; const ARect: TRect; ABorders: TcxBorders;
  ABorderColor: TColor; AViewParams: TcxViewParams; ATransparent: Boolean);
var
  AElement: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerTimeRuler
  else
    AElement := nil;

  if AElement <> nil then
    DrawClippedElement(ACanvas, AElement, ABorders, ARect)
  else
    inherited DrawTimeRulerBackground(ACanvas, ARect, ABorders, ABorderColor,
      AViewParams, ATransparent);
end;

procedure TcxSchedulerExternalSkinPainter.FlushCache;
begin
  inherited;
  FEventsCache.Clear;
end;

function TcxSchedulerExternalSkinPainter.GetEventBorderColor
  (AViewInfo: TcxSchedulerEventCellViewInfo): TColor;
var
  ABorderColor: TdxSkinColor;
  ADest: TColor;
  ADestAsQuad: TRGBQuad;
  ASkinInfo: TdxSkinInfo;
  ASource: TColor;
begin
  if GetSkinInfo(ASkinInfo) then
  begin
    ABorderColor := TdxSkinInfoAccess(ASkinInfo)
      .GetColorByName(ASkinInfo.SchedulerAppointment[True],
      sdxSchedulerAppointmentBorderColor);

    if ABorderColor <> nil then
    begin
      ASource := ColorToRgb(ABorderColor.Value);
      ADest := ColorToRgb(AViewInfo.ViewParams.Color);
      if (ASource = clBlack) and (ADest <> clWhite) then
      begin
        ADestAsQuad := dxColorToRGBQuad(ADest);
        ADestAsQuad.rgbRed := MulDiv(ADestAsQuad.rgbRed, 120, MaxByte);
        ADestAsQuad.rgbBlue := MulDiv(ADestAsQuad.rgbBlue, 120, MaxByte);
        ADestAsQuad.rgbGreen := MulDiv(ADestAsQuad.rgbGreen, 120, MaxByte);
        Result := dxRGBQuadToColor(ADestAsQuad);
      end
      else
        Result := ASource;
    end
    else
      Result := GetSeparatorColor(AViewInfo,
        ASkinInfo.SchedulerAppointmentBorder);
  end
  else
    Result := inherited GetEventBorderColor(AViewInfo);
end;

function TcxSchedulerExternalSkinPainter.GetEventBorderSize
  (AViewInfo: TcxSchedulerEventCellViewInfo): Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
  begin
    if ASkinInfo.SchedulerAppointmentBorderSize <> nil then
      Result := ASkinInfo.SchedulerAppointmentBorderSize.Value
    else
      Result := 0
  end
  else
    Result := inherited GetEventBorderSize(AViewInfo);
end;

function TcxSchedulerExternalSkinPainter.GetEventLabelSize
  (AScaleFactor: TdxScaleFactor): TSize;
var
  AElement: TdxSkinElement;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
    AElement := ASkinInfo.SchedulerLabelCircle
  else
    AElement := nil;

  if AElement <> nil then
    Result := AScaleFactor.Apply(AElement.Size)
  else
    Result := inherited GetEventLabelSize(AScaleFactor);
end;

function TcxSchedulerExternalSkinPainter.GetEventSelectionBorderSize
  (AViewInfo: TcxSchedulerEventCellViewInfo): Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) and (ASkinInfo.SchedulerAppointmentBorderSize <> nil)
  then
    Result := Min(ASkinInfo.SchedulerAppointmentBorderSize.Value, 1)
  else
    Result := inherited GetEventSelectionBorderSize(AViewInfo);
end;

function TcxSchedulerExternalSkinPainter.GetEventSelectionExtends: TRect;
begin
  Result := GetEventSelectionOffsets(nil);
end;

function TcxSchedulerExternalSkinPainter.GetEventSelectionExtends
  (AViewInfo: TcxSchedulerEventCellViewInfo): TRect;
begin
  if IsSkinAvailable then
    Result := GetEventSelectionOffsets(AViewInfo)
  else
    Result := inherited GetEventSelectionExtends(AViewInfo);
end;

function TcxSchedulerExternalSkinPainter.GetEventSelectionOffsets
  (AViewInfo: TcxSchedulerEventCellViewInfo): TRect;
var
  ABorderSize: Integer;
begin
  ABorderSize := GetEventSelectionBorderSize(AViewInfo);
  if (AViewInfo = nil) or (AViewInfo.ViewStyle = svsClassic) then
  begin
    Result := cxRect(ABorderSize, ABorderSize, ABorderSize, ABorderSize);
    if not IsRectEmpty(AViewInfo.TimeLineRect) then
      Result.Left := cxTimeLineWidth;
    if AViewInfo.UseRightToLeftAlignment and
      ((Result.Left <> 0) or (Result.Right <> 0)) then
      Result := TdxRightToLeftLayoutConverter.ConvertOffsets(Result);
  end
  else
  begin
    Result := cxRect(ABorderSize, 0, 0, 0);
    if not IsRectEmpty(AViewInfo.TimeLineRect) and
      (AViewInfo.Bounds.Left > AViewInfo.TimeLineRect.Left) then
      Inc(Result.Left, AViewInfo.Bounds.Left - (AViewInfo.TimeLineRect.Left -
        Max(TcxSchedulerEventCellViewInfoAccess(AViewInfo).BorderSize, 1) + 1));
  end;
end;

function TcxSchedulerExternalSkinPainter.GetSeparatorColor
  (AViewInfo: TcxSchedulerEventCellViewInfo;
  ABorderColor: TdxSkinColor): TColor;
begin
  Result := clDefault;
  if ABorderColor <> nil then
    Result := ABorderColor.Value;
  if not cxColorIsValid(Result) then
    Result := AViewInfo.SeparatorColor;
end;

procedure TcxSchedulerExternalSkinPainter.DrawShadow(ACanvas: TcxCanvas;
  const ARect, AVisibleRect: TRect; AScaleFactor: TdxScaleFactor);

  function GetBottomShadowRect(const R: TRect; AShadowSize: Integer): TRect;
  begin
    if ACanvas.UseRightToLeftAlignment then
      Result := cxRect(R.Left - AShadowSize, R.Bottom - AShadowSize,
        R.Right - AShadowSize, R.Bottom + AShadowSize)
    else
      Result := cxRect(R.Left + AShadowSize, R.Bottom - AShadowSize,
        R.Right + AShadowSize, R.Bottom + AShadowSize)
  end;

  function GetRightShadowRect(const R: TRect; AShadowSize: Integer): TRect;
  begin
    if ACanvas.UseRightToLeftAlignment then
      Result := cxRect(R.Left - AShadowSize, R.Top + AShadowSize,
        R.Left + AShadowSize, R.Bottom - AShadowSize)
    else
      Result := cxRect(R.Right - AShadowSize, R.Top + AShadowSize,
        R.Right + AShadowSize, R.Bottom - AShadowSize)
  end;

  procedure DrawShadowLine(AShadow: TdxSkinElement; const ARect: TRect);
  begin
    if AShadow <> nil then
    begin
      AShadow.CacheCapacity := MaxEventsCacheCapacity;
      AShadow.UseCache := True;
      AShadow.RightToLeftDependentDraw(ACanvas.Handle, ARect,
        dxDefaultScaleFactor, ACanvas.UseRightToLeftAlignment);
    end;
  end;

var
  AShadowSize: Integer;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinInfo(ASkinInfo) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(AVisibleRect);
      AShadowSize := AScaleFactor.Apply(4);
      DrawShadowLine(ASkinInfo.SchedulerAppointmentShadow[False],
        GetBottomShadowRect(ARect, AShadowSize));
      DrawShadowLine(ASkinInfo.SchedulerAppointmentShadow[True],
        GetRightShadowRect(ARect, AShadowSize));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawShadow(ACanvas, ARect, AVisibleRect, AScaleFactor);
end;

function TcxSchedulerExternalSkinPainter.DrawShadowFirst: Boolean;
begin
  Result := IsSkinAvailable or inherited DrawShadowFirst;
end;

initialization

ExternalPainterClass := TcxSchedulerExternalSkinPainter;

finalization

ExternalPainterClass := TcxSchedulerExternalPainter;

end.
