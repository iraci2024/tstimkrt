{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library controls }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
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

unit dxDirectX.D2D.Utils;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Windows, ActiveX, Types, D2D1, DxgiFormat, Graphics, dxCore, dxCoreGraphics,
  dxDirectX.D2D.Types;

type

  { EdxDirectXError }

  EdxDirectXError = class(EdxException)
  strict private
    FErrorCode: HRESULT;
  public
    constructor Create(AErrorCode: HRESULT);
    //
    property ErrorCode: HRESULT read FErrorCode;
  end;

procedure CheckD2D1Result(AValue: HRESULT); inline;
function D2D1Bitmap(const AContext: ID2D1DeviceContext; ABitmap: TBitmap;
  AAlphaFormat: TAlphaFormat): ID2D1Bitmap1; overload;
function D2D1Bitmap(const AContext: ID2D1DeviceContext;
  ABitmap: TdxCustomFastDIB; AAlphaFormat: TAlphaFormat): ID2D1Bitmap1;
  overload;
function D2D1Bitmap(const AContext: ID2D1DeviceContext; ABits: PByte;
  AWidth, AHeight, AStride: Integer; AAlphaFormat: TAlphaFormat)
  : ID2D1Bitmap1; overload;
function D2D1ColorF(const AColor: TdxAlphaColor): TD2D1ColorF; overload; inline;
function D2D1PointF(const X, Y: Single): TD2D1Point2F; inline;
function D2D1Rect(const R: TRect): TD2D1RectF; overload; inline;
function D2D1Rect(const L, T, R, B: Single): TD2D1RectF; overload; inline;
function D2D1SizeU(const W, H: Integer): TD2D1SizeU; inline;
function D2D1Matrix3x2(const XForm: TXForm): TD2D1Matrix3x2F;
function D2D1PointsToArcSegment(const P1, P2, P3, P4: TD2D1Point2F;
  out ACenter, A, B: TD2D1Point2F): TD2D1ArcSegment;

implementation

uses
  dxFading, Math;

const
  scxDirectXInvalidOperation = 'Invalid operation in DirectX (Code: %d)';

type

  { TdxPARGBToARGB }

  TdxPARGBToARGB = class
  strict private
    class var Data: array [Byte, Byte] of Byte;
    class var DataInitialized: Boolean;

    class procedure CheckInitialized;
  public
    class function Convert(ASource: PRGBQuad; ACount: Integer): PRGBQuad;
  end;

procedure CheckD2D1Result(AValue: HRESULT);
begin
  if Failed(AValue) then
    raise EdxDirectXError.Create(AValue);
end;

function D2D1Bitmap(const AContext: ID2D1DeviceContext;
  ABitmap: TdxCustomFastDIB; AAlphaFormat: TAlphaFormat): ID2D1Bitmap1;
begin
  Result := D2D1Bitmap(AContext, PByte(ABitmap.Bits), ABitmap.Width,
    ABitmap.Height, ABitmap.Width * 4, AAlphaFormat);
end;

function D2D1Bitmap(const AContext: ID2D1DeviceContext; ABits: PByte;
  AWidth, AHeight, AStride: Integer; AAlphaFormat: TAlphaFormat): ID2D1Bitmap1;
const
  AlphaModeMap: array [TAlphaFormat] of
  {$IFDEF DELPHI104SYDNEY}D2D1_ALPHA_MODE{$ELSE}Integer{$ENDIF} =
    (D2D1_ALPHA_MODE_IGNORE, D2D1_ALPHA_MODE_STRAIGHT,
    D2D1_ALPHA_MODE_PREMULTIPLIED);
var
  ABitmapProperties: TD2D1BitmapProperties1;
  ATempBits: PByte;
begin
  if AAlphaFormat = afDefined then
  begin
    ATempBits := PByte(TdxPARGBToARGB.Convert(PRGBQuad(ABits),
      AWidth * AHeight));
    try
      Result := D2D1Bitmap(AContext, ATempBits, AWidth, AHeight,
        AWidth * SizeOf(TRGBQuad), afPremultiplied);
    finally
      FreeMem(ATempBits);
    end;
  end
  else
  begin
    ZeroMemory(@ABitmapProperties, SizeOf(ABitmapProperties));
    ABitmapProperties.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    ABitmapProperties.pixelFormat.alphaMode := AlphaModeMap[AAlphaFormat];
    CheckD2D1Result(AContext.CreateBitmap(D2D1SizeU(AWidth, AHeight), ABits,
      AStride, @ABitmapProperties, Result));
  end;
end;

function D2D1Bitmap(const AContext: ID2D1DeviceContext; ABitmap: TBitmap;
  AAlphaFormat: TAlphaFormat): ID2D1Bitmap1;
var
  AColors: TRGBColors;
begin
  if ABitmap.pixelFormat <> pf32bit then
    AAlphaFormat := afIgnored;

  GetBitmapBits(ABitmap, AColors, True);
  if AAlphaFormat <> afIgnored then
    TdxFadingHelper.CorrectAlphaChannel(AColors);
  Result := D2D1Bitmap(AContext, @AColors[0], ABitmap.Width, ABitmap.Height,
    ABitmap.Width * 4, AAlphaFormat);
end;

function D2D1ColorF(const AColor: TdxAlphaColor): TD2D1ColorF; inline;
begin
  Result.R := dxGetRed(AColor) / 255;
  Result.g := dxGetGreen(AColor) / 255;
  Result.B := dxGetBlue(AColor) / 255;
  Result.A := dxGetAlpha(AColor) / 255;
end;

function D2D1Rect(const R: TRect): TD2D1RectF; inline;
begin
  Result.left := R.left;
  Result.top := R.top;
  Result.right := R.right;
  Result.bottom := R.bottom;
end;

function D2D1Rect(const L, T, R, B: Single): TD2D1RectF;
begin
  Result.left := L;
  Result.top := T;
  Result.right := R;
  Result.bottom := B;
end;

function D2D1PointF(const X, Y: Single): TD2D1Point2F;
begin
  Result.X := X;
  Result.Y := Y;
end;

function D2D1SizeU(const W, H: Integer): TD2D1SizeU;
begin
  Result.Height := H;
  Result.Width := W;
end;

function D2D1Matrix3x2(const XForm: TXForm): TD2D1Matrix3x2F;
begin
  Result._11 := XForm.eM11;
  Result._12 := XForm.eM12;
  Result._21 := XForm.eM21;
  Result._22 := XForm.eM22;
  Result._31 := XForm.eDx;
  Result._32 := XForm.eDy;
end;

function D2D1PointsToArcSegment(const P1, P2, P3, P4: TD2D1Point2F;
  out ACenter, A, B: TD2D1Point2F): TD2D1ArcSegment;
var
  AA, BB: Single;
  AAngle1: Single;
  AAngle2: Single;
  AAngleDelta: Single;
  ABottomRightPoint: TD2D1Point2F;
  ATopLeftPoint: TD2D1Point2F;
  ASlope: Single;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.sweepDirection := D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE;

  try
    ATopLeftPoint.X := Min(P1.X, P2.X);
    ATopLeftPoint.Y := Min(P1.Y, P2.Y);
    ABottomRightPoint.X := Max(P1.X, P2.X);
    ABottomRightPoint.Y := Max(P1.Y, P2.Y);
    ACenter := D2D1PointF((ABottomRightPoint.X + ATopLeftPoint.X) * 0.5,
      (ABottomRightPoint.Y + ATopLeftPoint.Y) * 0.5);

    AAngle1 := ArcTan2(ACenter.Y - P3.Y - 0.5, ACenter.X - P3.X - 0.5);
    AAngle2 := ArcTan2(ACenter.Y - P4.Y - 0.5, ACenter.X - P4.X - 0.5);
    AAngle1 := Pi - AAngle1;
    AAngle2 := Pi - AAngle2;

    AAngleDelta := AAngle2 - AAngle1;
    if (AAngleDelta < 0) and (-AAngleDelta > 0.001) then
      AAngleDelta := AAngleDelta + 2 * Pi;
    if AAngleDelta > Pi then
      Result.arcSize := D2D1_ARC_SIZE_LARGE
    else
      Result.arcSize := D2D1_ARC_SIZE_SMALL;

    Result.size := D2D1SizeF((ABottomRightPoint.X - ATopLeftPoint.X - 1) * 0.5,
      (ABottomRightPoint.Y - ATopLeftPoint.Y - 1) * 0.5);

    AA := Result.size.Width * Result.size.Width;
    BB := Result.size.Height * Result.size.Height;
    ASlope := Sqr(P3.Y - ACenter.Y) / Max(Sqr(P3.X - ACenter.X), 0.1);

    A.X := Sqrt(AA * BB / (BB + AA * ASlope));
    A.Y := Sqrt(BB * (1 - Sqr(A.X) / AA));

    if (AAngle1 < Pi / 2) or (AAngle1 > 3 * Pi / 2) then
      A.X := ACenter.X + A.X
    else
      A.X := ACenter.X - A.X;

    if AAngle1 > Pi then
      A.Y := ACenter.Y + A.Y
    else
      A.Y := ACenter.Y - A.Y;

    ASlope := Sqr(P4.Y - ACenter.Y) / Max(Sqr(P4.X - ACenter.X), 0.1);
    B.X := Sqrt(AA * BB / (BB + AA * ASlope));
    B.Y := Sqrt(BB * (1 - Sqr(B.X) / AA));

    if (AAngle2 < Pi / 2) or (AAngle2 > 3 * Pi / 2) then
      B.X := ACenter.X + B.X
    else
      B.X := ACenter.X - B.X;

    if AAngle2 > Pi then
      B.Y := ACenter.Y + B.Y
    else
      B.Y := ACenter.Y - B.Y;

    Result.point := B;
  finally
  end;
end;

{ EdxDirectXError }

constructor EdxDirectXError.Create(AErrorCode: HRESULT);
begin
  FErrorCode := AErrorCode;
  CreateFmt(scxDirectXInvalidOperation, [Ord(ErrorCode)]);
end;

{ TdxPARGBToARGB }

class procedure TdxPARGBToARGB.CheckInitialized;
var
  I, J: Integer;
begin
  if not DataInitialized then
  begin
    DataInitialized := True;
    for I := 0 to 255 do
      for J := I to 255 do
      begin
        Data[I, J] := MulDiv(I, J, 255);
        Data[J, I] := Data[I, J];
      end;
  end;
end;

class function TdxPARGBToARGB.Convert(ASource: PRGBQuad; ACount: Integer)
  : PRGBQuad;
var
  ATarget: PRGBQuad;
begin
  CheckInitialized;
  Result := AllocMem(SizeOf(TRGBQuad) * ACount);
  ATarget := PRGBQuad(Result);
  while ACount > 0 do
  begin
    if ASource^.rgbReserved < MaxByte then
    begin
      ATarget^.rgbRed := Data[ASource^.rgbRed, ASource^.rgbReserved];
      ATarget^.rgbBlue := Data[ASource^.rgbBlue, ASource^.rgbReserved];
      ATarget^.rgbGreen := Data[ASource^.rgbGreen, ASource^.rgbReserved];
      ATarget^.rgbReserved := ASource^.rgbReserved;
    end
    else
      PDWORD(ATarget)^ := PDWORD(ASource)^;

    Inc(ATarget);
    Inc(ASource);
    Dec(ACount);
  end;
end;

end.
