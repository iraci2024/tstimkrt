{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressRichEditControl }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxRichEdit.LayoutEngine.BoxMeasurer; // for internal use

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Classes, Types, SysUtils, Graphics,
  dxCore, cxGeometry, dxCoreClasses, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxFontHelpers,
  dxTextViewInfoCache,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Simple;

type
  { TdxGdiBoxMeasurer }

  TdxGdiBoxMeasurer = class(TdxBoxMeasurer)
  strict private
    FGraphics: TdxGraphics;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AGraphics: TdxGraphics); reintroduce;

    function GetHdc: HDC; virtual;
    procedure ReleaseHdc(ADC: HDC); virtual;
    function MeasureCharactersBounds(const AText: string;
      AFontInfo: TdxFontInfo; const ABounds: TRect): TArray<TRect>;
      overload; override;
    function CreateTextViewInfo(const AText: string; AFontInfo: TdxFontInfo)
      : TdxTextViewInfo; override;
    function SnapToPixels(AValue: Integer; ADpi: Single): Integer;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer)
      : Boolean; overload; override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo;
      const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean;
      overload; override;

    property Graphics: TdxGraphics read FGraphics;
  end;

  { TdxGdiBoxMeasurerLockHdc }

  TdxGdiBoxMeasurerLockHdc = class(TdxGdiBoxMeasurer)
  strict private
    FHdcGraphics: TdxGraphics;
    FHdc: THandle;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AGraphics: TdxGraphics; AHdcGraphics: TdxGraphics); reintroduce;
    destructor Destroy; override;
    function GetHdc: HDC; override;
    procedure ReleaseHdc(ADC: HDC); override;
    procedure ObtainCachedHdc;
    procedure ReleaseCachedHdc;
  end;

  { TdxRectangleUtils }

  TdxRectangleUtils = class sealed
  public
    class function BoundingRectangle(const APoints: array of TPoint): TRect;
      overload; static;
    class function BoundingRectangle(const ABounds: TRect;
      ATransform: TdxTransformMatrix): TRect; overload; static;
  end;

implementation

uses
  RTLConsts, Math, dxTypeHelpers;

{ TdxGdiBoxMeasurer }

constructor TdxGdiBoxMeasurer.Create(ADocumentModel: TdxCustomDocumentModel;
  AGraphics: TdxGraphics);
begin
  inherited Create(ADocumentModel);
  FGraphics := AGraphics;
end;

function TdxGdiBoxMeasurer.CreateTextViewInfo(const AText: string;
  AFontInfo: TdxFontInfo): TdxTextViewInfo;
var
  ADC: HDC;
begin
  ADC := GetHdc;
  try
    SelectObject(ADC, AFontInfo.GdiFontHandle);
    Result := CreateTextViewInfoCore(ADC, AText, AFontInfo);
  finally
    ReleaseHdc(ADC);
  end;
end;

function TdxGdiBoxMeasurer.GetHdc: HDC;
begin
  Result := FGraphics.GetHdc;
end;

procedure TdxGdiBoxMeasurer.ReleaseHdc(ADC: HDC);
begin
  FGraphics.ReleaseHdc(ADC);
end;

function TdxGdiBoxMeasurer.MeasureCharactersBounds(const AText: string;
  AFontInfo: TdxFontInfo; const ABounds: TRect): TArray<TRect>;
var
  ADC: HDC;
begin
  TMonitor.Enter(FGraphics);
  try
    ADC := FGraphics.GetHdc;
    try
      Result := MeasureCharactersBounds(ADC, AText, AFontInfo, ABounds);
    finally
      FGraphics.ReleaseHdc(ADC);
    end;
  finally
    TMonitor.Exit(FGraphics);
  end;
end;

function TdxGdiBoxMeasurer.SnapToPixels(AValue: Integer; ADpi: Single): Integer;
begin
  Result := DocumentModel.LayoutUnitConverter.SnapToPixels(AValue, ADpi);
end;

function TdxGdiBoxMeasurer.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo;
  AMaxWidth: Integer): Boolean;
var
  ARun: TdxTextRunBase;
begin
  ARun := TdxTextRunBase(PieceTable.Runs[ABoxInfo.StartPos.RunIndex]);
  Result := ARun.TryAdjustEndPositionToFit(ABoxInfo, AMaxWidth, Self);
end;

function TdxGdiBoxMeasurer.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo;
  const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean;
var
  AHdc: THandle;
  ALengthToFit, ANewEndOffset: Integer;
begin
  AHdc := GetHdc;
  try
    ALengthToFit := FindLengthToFit(AHdc, AText, AFontInfo, AMaxWidth);
    if ALengthToFit > 0 then
    begin
      ANewEndOffset := ABoxInfo.StartPos.Offset + ALengthToFit - 1;
      if ANewEndOffset > ABoxInfo.EndPos.Offset then
        Exit(False);
      ABoxInfo.EndPos := TdxFormatterPosition.Create(ABoxInfo.EndPos.RunIndex,
        ANewEndOffset, ABoxInfo.EndPos.BoxIndex);
      Result := True;
    end
    else
      Result := False;
  finally
    ReleaseHdc(AHdc);
  end;
end;

{ TdxGdiBoxMeasurerLockHdc }

constructor TdxGdiBoxMeasurerLockHdc.Create(ADocumentModel
  : TdxCustomDocumentModel; AGraphics: TdxGraphics; AHdcGraphics: TdxGraphics);
begin
  inherited Create(ADocumentModel, AGraphics);
  Assert(AHdcGraphics <> nil);
  FHdcGraphics := AHdcGraphics;
  ObtainCachedHdc;
end;

destructor TdxGdiBoxMeasurerLockHdc.Destroy;
begin
  ReleaseCachedHdc;
  inherited Destroy;
end;

procedure TdxGdiBoxMeasurerLockHdc.ObtainCachedHdc;
begin
  FHdc := FHdcGraphics.GetHdc;
end;

procedure TdxGdiBoxMeasurerLockHdc.ReleaseCachedHdc;
begin
  if FHdc <> 0 then
  begin
    FHdcGraphics.ReleaseHdc(FHdc);
    FHdc := 0;
  end;
end;

function TdxGdiBoxMeasurerLockHdc.GetHdc: HDC;
begin
  if FHdc <> 0 then
    Result := FHdc
  else
    Result := inherited GetHdc;
end;

procedure TdxGdiBoxMeasurerLockHdc.ReleaseHdc(ADC: HDC);
begin
  if FHdc = 0 then
    inherited ReleaseHdc(ADC);
end;

{ TdxRectangleUtils }

class function TdxRectangleUtils.BoundingRectangle(const APoints
  : array of TPoint): TRect;
var
  I, ACount, AMaxX, AMaxY, AMinX, AMinY: Integer;
begin
  ACount := Length(APoints);
  if ACount = 0 then
    Exit(TRect.Null);
  AMinX := APoints[0].X;
  AMaxX := AMinX;
  AMinY := APoints[0].Y;
  AMaxY := AMinY;
  for I := 1 to ACount - 1 do
  begin
    AMinX := Min(APoints[I].X, AMinX);
    AMinY := Min(APoints[I].Y, AMinY);
    AMaxX := Max(APoints[I].X, AMaxX);
    AMaxY := Max(APoints[I].Y, AMaxY);
  end;
  Result.Init(AMinX, AMinY, AMaxX, AMaxY);
end;

class function TdxRectangleUtils.BoundingRectangle(const ABounds: TRect;
  ATransform: TdxTransformMatrix): TRect;
var
  APt0, APt1, APt2, APt3: TPoint;
begin
  APt0 := ATransform.TransformPoint(Point(ABounds.Right, ABounds.Bottom));
  APt1 := ATransform.TransformPoint(Point(ABounds.Right, ABounds.Top));
  APt2 := ATransform.TransformPoint(Point(ABounds.Left, ABounds.Bottom));
  APt3 := ATransform.TransformPoint(ABounds.Location);

  Result := TdxRectangleUtils.BoundingRectangle([APt0, APt1, APt2, APt3]);
end;

end.
