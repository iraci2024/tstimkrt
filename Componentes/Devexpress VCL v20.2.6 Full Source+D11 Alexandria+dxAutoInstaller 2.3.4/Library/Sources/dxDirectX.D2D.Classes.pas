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

unit dxDirectX.D2D.Classes;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Windows, Graphics, D2D1, dxCore, dxDirectX.D2D.Types;

type

  { IdxDirect2DSupport }

  IdxDirect2DSupport = interface
    ['{CE653B42-C5CA-4731-95FE-8ACCBD0A9D00}']
  end; // for internal use only

  { IdxDirect2DSupport2 }

  IdxDirect2DSupport2 = interface
    ['{CE653B42-C5CA-4731-95FE-8ACCBD0A9D02}']
    function IsAllowed: Boolean;
  end; // for internal use only

  { TdxDirect2DFontCache }

  TdxDirect2DFontCache = class
  strict private
  const
    Capacity = 32;
  strict private
    class var FInstance: TObject;
  public
    class procedure Finalize;
    class function Get(AFont: TFont): IDWriteTextFormat;
  end; // for internal use only

  { TdxDirect2DPenStyleCache }

  TdxDirect2DPenStyleCache = class
  strict private
    class var FCache: array [TPenStyle] of ID2D1StrokeStyle1;
  public
    class function Get(AStyle: TPenStyle): ID2D1StrokeStyle1;
  end; // for internal use only

function dxIsDirect2DSupported(AObject: TObject): Boolean;

implementation

uses
  SysUtils, dxCoreGraphics;

type

  { TdxDirect2DFontCacheKey }

  TdxDirect2DFontCacheKey = record
    Name: string;
    Size: Integer;
    Style: TDWriteFontStyle;
    Weight: TDWriteFontWeight;

    class function Create(AFont: TFont): TdxDirect2DFontCacheKey; static;
  end;

  { TdxDirect2DFontCacheImpl }

  TdxDirect2DFontCacheImpl = class(TdxValueCacheManager<TdxDirect2DFontCacheKey,
    IDWriteTextFormat>)
  protected
    function CreateValue(const Key: TdxDirect2DFontCacheKey): IDWriteTextFormat;
  public
    function Get(const Key: TdxDirect2DFontCacheKey): IDWriteTextFormat;
      reintroduce;
  end;

function dxIsDirect2DSupported(AObject: TObject): Boolean;
var
  AIntf: IdxDirect2DSupport2;
begin
  if Supports(AObject, IdxDirect2DSupport2, AIntf) then
    Result := AIntf.IsAllowed
  else
    Result := Supports(AObject, IdxDirect2DSupport);
end;

{ TdxDirect2DFontCache }

class procedure TdxDirect2DFontCache.Finalize;
begin
  FreeAndNil(FInstance);
end;

class function TdxDirect2DFontCache.Get(AFont: TFont): IDWriteTextFormat;
var
  AInstance: TdxDirect2DFontCacheImpl;
begin
  if FInstance = nil then
  begin
    AInstance := TdxDirect2DFontCacheImpl.Create(Capacity);
    if InterlockedCompareExchangePointer(Pointer(FInstance), Pointer(AInstance),
      nil) <> nil then
      AInstance.Free;
  end;
  Result := TdxDirect2DFontCacheImpl(FInstance)
    .Get(TdxDirect2DFontCacheKey.Create(AFont));
end;

{ TdxDirect2DFontCacheKey }

class function TdxDirect2DFontCacheKey.Create(AFont: TFont)
  : TdxDirect2DFontCacheKey;
begin
  Result.Name := AFont.Name;
  Result.Size := -AFont.Height;

  if fsItalic in AFont.Style then
    Result.Style := DWRITE_FONT_STYLE_ITALIC
  else
    Result.Style := DWRITE_FONT_STYLE_NORMAL;

  if fsBold in AFont.Style then
    Result.Weight := DWRITE_FONT_WEIGHT_BOLD
  else
    Result.Weight := DWRITE_FONT_WEIGHT_NORMAL;
end;

{ TdxDirect2DFontCacheImpl }

function TdxDirect2DFontCacheImpl.CreateValue
  (const Key: TdxDirect2DFontCacheKey): IDWriteTextFormat;
begin
  DWriteFactory.CreateTextFormat(PWideChar(Key.Name), nil, Key.Weight,
    Key.Style, DWRITE_FONT_STRETCH_NORMAL, Key.Size, 'en-us', Result);
end;

function TdxDirect2DFontCacheImpl.Get(const Key: TdxDirect2DFontCacheKey)
  : IDWriteTextFormat;
begin
  if not inherited Get(Key, Result) then
  begin
    Result := CreateValue(Key);
    Add(Key, Result);
  end;
end;

{ TdxDirect2DPenStyleCache }

class function TdxDirect2DPenStyleCache.Get(AStyle: TPenStyle)
  : ID2D1StrokeStyle1;
const
  Styles: array [psDash .. psDashDotDot] of TD2D1DashStyle =
    (D2D1_DASH_STYLE_DASH, D2D1_DASH_STYLE_DOT, D2D1_DASH_STYLE_DASH_DOT,
    D2D1_DASH_STYLE_DASH_DOT_DOT);
var
  AProperties: TD2D1StrokeStyleProperties1;
begin
  Result := FCache[AStyle];
  if Result = nil then
  begin
    AProperties.StartCap := D2D1_CAP_STYLE_FLAT;
    AProperties.EndCap := D2D1_CAP_STYLE_FLAT;
    AProperties.dashCap := D2D1_CAP_STYLE_FLAT;
    AProperties.LineJoin := D2D1_LINE_JOIN_MITER;
    AProperties.MiterLimit := 10;
    AProperties.DashStyle := Styles[AStyle];
    AProperties.DashOffset := 0;
    AProperties.TransformType := D2D1_STROKE_TRANSFORM_TYPE_NORMAL;
    D2D1Factory1.CreateStrokeStyle(@AProperties, nil, 0, Result);
    FCache[AStyle] := Result;
  end;
end;

initialization

finalization

TdxDirect2DFontCache.Finalize;

end.
