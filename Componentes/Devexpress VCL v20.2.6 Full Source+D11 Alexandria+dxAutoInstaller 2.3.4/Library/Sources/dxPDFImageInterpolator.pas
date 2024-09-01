{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressPDFViewer }
{ }
{ Copyright (c) 2015-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL }
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

unit dxPDFImageInterpolator;

{$I cxVer.inc}

interface

uses
  SysUtils, dxPDFTypes, dxPDFImageUtils;

type
  { TdxPDFSourceImageScanlineInfo }

  TdxPDFSourceImageScanlineInfo = record
  strict private
    FEndIndex: Integer;
    FPixelInfo: TdxPDFSourceImagePixelInfoArray;
    FStartIndex: Integer;
  public
    class function Create(const APixelInfo: TdxPDFSourceImagePixelInfoArray;
      AStartIndex, AEndIndex: Integer): TdxPDFSourceImageScanlineInfo; static;
    property EndIndex: Integer read FEndIndex;
    property PixelInfo: TdxPDFSourceImagePixelInfoArray read FPixelInfo;
    property StartIndex: Integer read FStartIndex;
  end;

  TdxPDFSourceImageScanlineInfoArray = array of TdxPDFSourceImageScanlineInfo;

  { TdxPDFSuperSamplingInterpolator }

  TdxPDFSuperSamplingInterpolator = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FDataSource: IdxPDFImageScanlineSource;
    FPixelInfo: TdxPDFSourceImageScanlineInfoArray;
    FWindowSize: Integer;
  strict protected
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes); virtual; abstract;

    property DataSource: IdxPDFImageScanlineSource read FDataSource;
    property ComponentCount: Integer read GetComponentCount;
    property PixelInfo: TdxPDFSourceImageScanlineInfoArray read FPixelInfo;
    property WindowSize: Integer read FWindowSize;
  public
    constructor Create(const ADataSource: IdxPDFImageScanlineSource;
      ATargetDimension, ASourceDimension: Integer);
  end;

  { TdxPDFSuperSamplingHorizontalInterpolator }

  TdxPDFSuperSamplingHorizontalInterpolator = class
    (TdxPDFSuperSamplingInterpolator)
  strict private
    FSourceScanline: TBytes;
    FWidth: Integer;
  strict protected
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const ADataSource: IdxPDFImageScanlineSource;
      AWidth, ASourceWidth: Integer);
  end;

  { TdxPDFSuperSamplingVerticalInterpolator }

  TdxPDFSuperSamplingVerticalInterpolator = class
    (TdxPDFSuperSamplingInterpolator)
  strict private
    FCurrentY: Integer;
    FLastRowIndex: Integer;
    FPreviousLastIndex: Integer;
    FSourceScanlines: array of TBytes;
    FSourceWidth: Integer;
    procedure FillBuffers(const AScanlineInfo: TdxPDFSourceImageScanlineInfo);
  strict protected
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const ADataSource: IdxPDFImageScanlineSource;
      ASourceWidth, AHeight, ASourceHeight: Integer);
  end;

  { TdxPDFBilinearUpsamplingInterpolator }

  TdxPDFBilinearUpsamplingInterpolator = class
    (TdxPDFImageScanlineSourceDecorator)
  strict protected
  type
{$REGION 'internal types'}
    TConvolutionWindowInfo = record
    strict private
      FStartPosition: Integer;
      FWeights: TdxPDFFixedPointNumbers;
    public
      class function Create(const AWeight: TdxPDFFixedPointNumbers;
        AStartPosition: Integer): TConvolutionWindowInfo; static;
      property StartPosition: Integer read FStartPosition;
      property Weights: TdxPDFFixedPointNumbers read FWeights;
    end;

    TConvolutionWindowInfoArray = array of TConvolutionWindowInfo;
{$ENDREGION}
  strict private
    FConvolutionWindowInfo: TConvolutionWindowInfoArray;
    FConvolutionWindowSize: Integer;
  protected
    property ConvolutionWindowInfo: TConvolutionWindowInfoArray
      read FConvolutionWindowInfo;
    property ConvolutionWindowSize: Integer read FConvolutionWindowSize;
  public
    constructor Create(const ADataSource: IdxPDFImageScanlineSource;
      ASourceWidth, ATargetDimension, ASourceDimension: Integer); reintroduce;
    class function CalculateLinearInterpolationWeight(AValue: Single)
      : Single; static;
  end;

  { TdxPDFBilinearUpsamplingHorizontalInterpolator }

  TdxPDFBilinearUpsamplingHorizontalInterpolator = class
    (TdxPDFBilinearUpsamplingInterpolator)
  strict private
    FSourceScanline: TBytes;
    FWidth: Integer;
  strict protected
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const ADataSource: IdxPDFImageScanlineSource;
      AWidth, ASourceWidth: Integer);
  end;

  { TdxPDFBilinearUpsamplingVerticalInterpolator }

  TdxPDFBilinearUpsamplingVerticalInterpolator = class
    (TdxPDFBilinearUpsamplingInterpolator)
  strict private
    FCurrentSourceY: Integer;
    FCurrentY: Integer;
    FSourceBuffers: array of TBytes;
    FSourceHeight: Integer;
    function ReadNextSourceScanline: TBytes;
  strict protected
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const ADataSource: IdxPDFImageScanlineSource;
      ASourceWidth, AHeight, ASourceHeight: Integer);
  end;

implementation

uses
  Types, Math, dxCore, dxPDFUtils;

procedure AddPixelInfo(const AInfo: TdxPDFSourceImagePixelInfo;
  var AArray: TdxPDFSourceImagePixelInfoArray);
var
  L: Integer;
begin
  L := Length(AArray);
  SetLength(AArray, L + 1);
  AArray[L] := AInfo;
end;

{ TdxPDFSourceImageScanlineInfo }

class function TdxPDFSourceImageScanlineInfo.Create(const APixelInfo
  : TdxPDFSourceImagePixelInfoArray; AStartIndex, AEndIndex: Integer)
  : TdxPDFSourceImageScanlineInfo;
begin
  Result.FPixelInfo := APixelInfo;
  Result.FStartIndex := AStartIndex;
  Result.FEndIndex := AEndIndex;
end;

{ TdxPDFSuperSamplingInterpolator }

constructor TdxPDFSuperSamplingInterpolator.Create(const ADataSource
  : IdxPDFImageScanlineSource; ATargetDimension, ASourceDimension: Integer);
var
  ADimensionFactor, AStartPosition, AEndPosition, AFirstElementWeight, ASum,
    ALastElementWeight: Single;
  APixelInfoArray: TdxPDFSourceImagePixelInfoArray;
  AStartPositionIndex, AEndPositionIndex, AActualStartIndex,
    AActualEndPositionIndex, AActualEndIndex, I, J: Integer;
  AWeight: TdxPDFFixedPointNumber;
begin
  inherited Create;
  FDataSource := ADataSource;
  SetLength(FPixelInfo, ATargetDimension);
  ADimensionFactor := ASourceDimension / ATargetDimension;
  for I := 0 to ATargetDimension - 1 do
  begin
    SetLength(APixelInfoArray, 0);
    AStartPosition := I * ADimensionFactor;
    AEndPosition := (I + 1) * ADimensionFactor;
    AStartPositionIndex := Ceil(AStartPosition);
    AEndPositionIndex := Floor(AEndPosition);
    AFirstElementWeight := AStartPositionIndex - AStartPosition;
    ASum := Min(ASourceDimension, AEndPosition) - AStartPosition;
    AActualStartIndex := AStartPositionIndex;
    if AFirstElementWeight <> 0 then
    begin
      AddPixelInfo(TdxPDFSourceImagePixelInfo.Create(AStartPositionIndex - 1,
        TdxPDFFixedPointNumber.Create(AFirstElementWeight / ASum)),
        APixelInfoArray);
      Dec(AActualStartIndex);
    end;
    AActualEndPositionIndex := Min(AEndPositionIndex, ASourceDimension);
    AWeight := TdxPDFFixedPointNumber.Create(1 / ASum);
    for J := AStartPositionIndex to AActualEndPositionIndex - 1 do
      AddPixelInfo(TdxPDFSourceImagePixelInfo.Create(J, AWeight),
        APixelInfoArray);
    ALastElementWeight := AEndPosition - AEndPositionIndex;
    AActualEndIndex := AEndPositionIndex - 1;
    if (ALastElementWeight <> 0) and (AEndPositionIndex < ASourceDimension) then
    begin
      AddPixelInfo(TdxPDFSourceImagePixelInfo.Create(AEndPositionIndex,
        TdxPDFFixedPointNumber.Create(ALastElementWeight / ASum)),
        APixelInfoArray);
      Inc(AActualEndIndex);
    end;
    FWindowSize := Max(Length(APixelInfoArray), FWindowSize);
    FPixelInfo[I] := TdxPDFSourceImageScanlineInfo.Create(APixelInfoArray,
      AActualStartIndex, AActualEndIndex);
  end;
end;

function TdxPDFSuperSamplingInterpolator.GetComponentCount: Integer;
begin
  Result := FDataSource.ComponentCount;
end;

function TdxPDFSuperSamplingInterpolator.GetHasAlpha: Boolean;
begin
  Result := FDataSource.HasAlpha;
end;

procedure TdxPDFSuperSamplingInterpolator.FillBuffer(var ABuffer: TBytes;
  AScanlineCount: Integer);
begin
  // do nothing
end;

{ TdxPDFSuperSamplingHorizontalInterpolator }

constructor TdxPDFSuperSamplingHorizontalInterpolator.Create(const ADataSource
  : IdxPDFImageScanlineSource; AWidth, ASourceWidth: Integer);
begin
  inherited Create(ADataSource, AWidth, ASourceWidth);
  FWidth := AWidth;
  SetLength(FSourceScanline, ASourceWidth * ComponentCount);
end;

procedure TdxPDFSuperSamplingHorizontalInterpolator.FillNextScanline
  (var AScanline: TBytes);
var
  AComponentIndex, ADestinationOffset, AComponentCount, AOffset, I, J: Integer;
  AInfo: TdxPDFSourceImagePixelInfo;
  APixelInfoArray: TdxPDFSourceImagePixelInfoArray;
  AValues: TdxPDFFixedPointNumbers;
begin
  DataSource.FillNextScanline(FSourceScanline);
  ADestinationOffset := 0;
  AComponentCount := ComponentCount;
  SetLength(AValues, AComponentCount);
  for I := 0 to FWidth - 1 do
  begin
    SetLength(AValues, 0);
    SetLength(AValues, AComponentCount);
    APixelInfoArray := PixelInfo[I].PixelInfo;
    for J := 0 to Length(APixelInfoArray) - 1 do
    begin
      AInfo := APixelInfoArray[J];
      AOffset := AInfo.Index * AComponentCount;
      for AComponentIndex := 0 to AComponentCount - 1 do
      begin
        AValues[AComponentIndex] := AValues[AComponentIndex] + FSourceScanline
          [AOffset] * AInfo.Factor;
        Inc(AOffset);
      end;
    end;
    for AComponentIndex := 0 to AComponentCount - 1 do
    begin
      AScanline[ADestinationOffset] := AValues[AComponentIndex].RoundToByte;
      Inc(ADestinationOffset);
    end;
  end;
end;

{ TdxPDFSuperSamplingVerticalInterpolator }

constructor TdxPDFSuperSamplingVerticalInterpolator.Create(const ADataSource
  : IdxPDFImageScanlineSource; ASourceWidth, AHeight, ASourceHeight: Integer);
var
  I: Integer;
begin
  inherited Create(ADataSource, AHeight, ASourceHeight);
  FPreviousLastIndex := -1;
  FSourceWidth := ASourceWidth;
  SetLength(FSourceScanlines, WindowSize);
  for I := 0 to WindowSize - 1 do
    SetLength(FSourceScanlines[I], ASourceWidth * ADataSource.ComponentCount);
end;

procedure TdxPDFSuperSamplingVerticalInterpolator.FillBuffers
  (const AScanlineInfo: TdxPDFSourceImageScanlineInfo);
var
  AShouldSaveLastScanline: Boolean;
  I: Integer;
  AData: TBytes;
begin
  AShouldSaveLastScanline := AScanlineInfo.StartIndex = FPreviousLastIndex;
  if AShouldSaveLastScanline then
  begin
    FSourceScanlines[0] := TdxByteArray.Clone(FSourceScanlines[FLastRowIndex]);
    AData := FSourceScanlines[FLastRowIndex];
    SetLength(AData, 0);
    SetLength(AData, FSourceWidth * ComponentCount);
  end;
  FPreviousLastIndex := AScanlineInfo.EndIndex;
  FLastRowIndex := AScanlineInfo.EndIndex - AScanlineInfo.StartIndex;
  for I := IfThen(AShouldSaveLastScanline, 1, 0) to FLastRowIndex do
    DataSource.FillNextScanline(FSourceScanlines[I]);
end;

procedure TdxPDFSuperSamplingVerticalInterpolator.FillNextScanline
  (var AScanline: TBytes);
var
  AComponentIndex, AComponentCount, ASourceOffset, I: Integer;
  APixelInfo: TdxPDFSourceImagePixelInfo;
  AScanlineInfo: TdxPDFSourceImageScanlineInfo;
  ASourceData: TBytes;
  AValues: TdxPDFFixedPointNumbers;
begin
  AScanlineInfo := PixelInfo[FCurrentY];
  FillBuffers(AScanlineInfo);
  ASourceOffset := 0;
  AComponentCount := ComponentCount;
  for I := 0 to FSourceWidth - 1 do
  begin
    SetLength(AValues, 0);
    SetLength(AValues, AComponentCount);
    for APixelInfo in AScanlineInfo.PixelInfo do
    begin
      ASourceData := FSourceScanlines
        [APixelInfo.Index - AScanlineInfo.StartIndex];
      for AComponentIndex := 0 to AComponentCount - 1 do
        AValues[AComponentIndex] := AValues[AComponentIndex] + ASourceData
          [ASourceOffset + AComponentIndex] * APixelInfo.Factor;
    end;
    for AComponentIndex := 0 to AComponentCount - 1 do
      AScanline[ASourceOffset + AComponentIndex] := AValues[AComponentIndex]
        .RoundToByte;
    Inc(ASourceOffset, AComponentCount);
  end;
  Inc(FCurrentY);
end;

{ TdxPDFBilinearUpsamplingInterpolator }

constructor TdxPDFBilinearUpsamplingInterpolator.Create(const ADataSource
  : IdxPDFImageScanlineSource; ASourceWidth, ATargetDimension, ASourceDimension
  : Integer);
var
  AActualWeightArray: TdxPDFFixedPointNumbers;
  ADimensionSizeFactor, APosition, AWeightSum, AWeight: Single;
  AStartPosition, ASourceIndex, AWeightCount, I, J: Integer;
  AWeightArray: TDoubleDynArray;
begin
  inherited Create(ADataSource, ASourceWidth);
  FConvolutionWindowSize := 2;
  SetLength(FConvolutionWindowInfo, ATargetDimension);
  ADimensionSizeFactor := ASourceDimension / ATargetDimension;
  for I := 0 to ATargetDimension - 1 do
  begin
    APosition := I * ADimensionSizeFactor;
    AStartPosition := Floor(APosition);
    AWeightSum := 0;
    SetLength(AWeightArray, 0);
    for J := 0 to FConvolutionWindowSize - 1 do
    begin
      ASourceIndex := AStartPosition + J;
      if ASourceIndex < ASourceDimension then
      begin
        AWeight := CalculateLinearInterpolationWeight(ASourceIndex - APosition);
        TdxPDFUtils.AddValue(AWeight, AWeightArray);
        AWeightSum := AWeightSum + AWeight;
      end;
    end;
    AWeightCount := Length(AWeightArray);
    SetLength(AActualWeightArray, AWeightCount);
    for J := 0 to AWeightCount - 1 do
      AActualWeightArray[J] := TdxPDFFixedPointNumber.Create
        (AWeightArray[J] / AWeightSum);
    FConvolutionWindowInfo[I] := TConvolutionWindowInfo.Create
      (AActualWeightArray, AStartPosition);
  end;
end;

class function TdxPDFBilinearUpsamplingInterpolator.
  CalculateLinearInterpolationWeight(AValue: Single): Single;
begin
  AValue := Abs(AValue);
  Result := IfThen(AValue < 1, 1 - AValue);
end;

{ TdxPDFBilinearUpsamplingInterpolator.TConvolutionWindowInfo }

class function TdxPDFBilinearUpsamplingInterpolator.TConvolutionWindowInfo.
  Create(const AWeight: TdxPDFFixedPointNumbers; AStartPosition: Integer)
  : TConvolutionWindowInfo;
begin
  Result.FWeights := AWeight;
  Result.FStartPosition := AStartPosition;
end;

{ TdxPDFBilinearUpsamplingHorizontalInterpolator }

constructor TdxPDFBilinearUpsamplingHorizontalInterpolator.Create
  (const ADataSource: IdxPDFImageScanlineSource; AWidth, ASourceWidth: Integer);
begin
  inherited Create(ADataSource, ASourceWidth, AWidth, ASourceWidth);
  FWidth := AWidth;
  SetLength(FSourceScanline, ASourceWidth * ComponentCount);
end;

procedure TdxPDFBilinearUpsamplingHorizontalInterpolator.FillNextScanline
  (var AScanline: TBytes);
var
  AComponentCount, AResultOffset, ASourceIndex, AComponentIndex, AOffset, I,
    J: Integer;
  AValues: TdxPDFFixedPointNumbers;
  AWeight: TdxPDFFixedPointNumber;
  AWindowInfo: TConvolutionWindowInfo;
begin
  Source.FillNextScanline(FSourceScanline);
  AComponentCount := ComponentCount;
  AResultOffset := 0;
  for I := 0 to FWidth - 1 do
  begin
    SetLength(AValues, 0);
    SetLength(AValues, AComponentCount);
    AWindowInfo := ConvolutionWindowInfo[I];
    ASourceIndex := AWindowInfo.StartPosition;
    for J := 0 to Length(AWindowInfo.Weights) - 1 do
    begin
      AWeight := AWindowInfo.Weights[J];
      AOffset := ASourceIndex * AComponentCount;
      for AComponentIndex := 0 to AComponentCount - 1 do
      begin
        AValues[AComponentIndex] := AValues[AComponentIndex] + FSourceScanline
          [AOffset] * AWeight;
        Inc(AOffset);
      end;
      Inc(ASourceIndex);
    end;
    for AComponentIndex := 0 to AComponentCount - 1 do
    begin
      AScanline[AResultOffset] := AValues[AComponentIndex].RoundToByte;
      Inc(AResultOffset);
    end;
  end;
end;

{ TdxPDFBilinearUpsamplingVerticalInterpolator }

constructor TdxPDFBilinearUpsamplingVerticalInterpolator.Create
  (const ADataSource: IdxPDFImageScanlineSource;
  ASourceWidth, AHeight, ASourceHeight: Integer);
begin
  inherited Create(ADataSource, ASourceWidth, AHeight, ASourceHeight);
  FCurrentSourceY := -1;
  FSourceHeight := ASourceHeight;
  SetLength(FSourceBuffers, ConvolutionWindowSize);
  FSourceBuffers[1] := ReadNextSourceScanline;
end;

function TdxPDFBilinearUpsamplingVerticalInterpolator.
  ReadNextSourceScanline: TBytes;
begin
  SetLength(Result, SourceWidth * ComponentCount);
  Source.FillNextScanline(Result);
end;

procedure TdxPDFBilinearUpsamplingVerticalInterpolator.FillNextScanline
  (var AScanline: TBytes);
var
  AComponentCount, ASourceOffset, AComponentIndex, I, J: Integer;
  ASourceData: TBytes;
  AValues: TdxPDFFixedPointNumbers;
  AWeight: TdxPDFFixedPointNumber;
  AWindowInfo: TConvolutionWindowInfo;
begin
  AWindowInfo := ConvolutionWindowInfo[FCurrentY];
  if (AWindowInfo.StartPosition > FCurrentSourceY) and
    (AWindowInfo.StartPosition < (FSourceHeight - 1)) then
  begin
    FCurrentSourceY := AWindowInfo.StartPosition;
    FSourceBuffers[0] := FSourceBuffers[1];
    FSourceBuffers[1] := ReadNextSourceScanline;
  end;
  ASourceOffset := 0;
  AComponentCount := ComponentCount;
  for I := 0 to SourceWidth - 1 do
  begin
    SetLength(AValues, 0);
    SetLength(AValues, AComponentCount);
    for J := 0 to Length(AWindowInfo.Weights) - 1 do
    begin
      AWeight := AWindowInfo.Weights[J];
      ASourceData := FSourceBuffers[AWindowInfo.StartPosition -
        FCurrentSourceY + J];
      for AComponentIndex := 0 to AComponentCount - 1 do
        AValues[AComponentIndex] := AValues[AComponentIndex] + ASourceData
          [ASourceOffset + AComponentIndex] * AWeight;
    end;
    for AComponentIndex := 0 to AComponentCount - 1 do
      AScanline[ASourceOffset + AComponentIndex] := AValues[AComponentIndex]
        .RoundToByte;
    Inc(ASourceOffset, AComponentCount);
  end;
  Inc(FCurrentY);
end;

end.
