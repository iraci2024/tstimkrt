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

unit dxPDFDCTDecoder; // for internal use

{$I cxVer.inc}

interface

uses
  SysUtils, dxPDFTypes, dxPDFImageUtils;

const
  dxPDFUseExternalDCTDecoder: Boolean = True;

type
  { TdxPDFDCTDecoderFactory }

  TdxPDFDCTDecoderFactory = class
  public
    function CreateSource(const AImageData: TBytes;
      const AImage: IdxPDFDocumentImage): IdxPDFImageScanlineSource;
      virtual; abstract;
  end;

  { TdxPDFGDIPlusDCTDecoderFactory }

  TdxPDFGDIPlusDCTDecoderFactory = class(TdxPDFDCTDecoderFactory)
  public
    function CreateSource(const AImageData: TBytes;
      const AImage: IdxPDFDocumentImage): IdxPDFImageScanlineSource; override;
  end;

  { TdxPDFDCTDecodeResult }

  TdxPDFDCTDecodeResult = record
  public
    Data: TBytes;
    Stride: Integer;
  end;

  { TdxPDFDCTDecoder }

  TdxPDFDCTDecoder = class
  strict private
    class var FFactory: TdxPDFDCTDecoderFactory;
  strict private
    FImageData: TBytes;
    FImageHeight: Integer;
    FImageWidth: Integer;
    class function GetFactory: TdxPDFDCTDecoderFactory;
  protected
    class procedure Finalize;
    function DoDecode: TdxPDFDCTDecodeResult;
    procedure Initialize(const AData: TBytes; AWidth, AHeight: Integer);
  public
    class function CreateScanlineSource(const AImageData: TBytes;
      const AImage: IdxPDFDocumentImage; AComponentCount: Integer)
      : IdxPDFImageScanlineSource; static;
    class function Decode(const AData: TBytes; AWidth, AHeight: Integer)
      : TdxPDFDCTDecodeResult; static;
  end;

  { TdxPDFDCTDataValidator }

  TdxPDFDCTDataValidator = class
  public
    class function ChangeImageHeight(const AImageData: TBytes;
      ANewHeight: Integer): TBytes; static;
  end;

  { TdxPDFGDIPlusImageScanlineSource }

  TdxPDFGDIPlusImageScanlineSource = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FColorKeyMask: TdxPDFRanges;
    FComponentCount: Integer;
    FOffset: Integer;
    FSourceData: TdxPDFDCTDecodeResult;
    FWidth: Integer;
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);
  public
    constructor Create(const AImageData: TBytes;
      const AImage: IdxPDFDocumentImage);
  end;

implementation

uses
  Classes, Graphics, Math, ActiveX, dxCore, cxVariants, dxCoreClasses,
  dxGDIPlusAPI, dxGDIPlusClasses, dxJPX, dxPDFWIC,
  dxPDFUtils;

{ TdxPDFGDIPlusDCTDecoderFactory }

function TdxPDFGDIPlusDCTDecoderFactory.CreateSource(const AImageData: TBytes;
  const AImage: IdxPDFDocumentImage): IdxPDFImageScanlineSource;
begin
  Result := TdxPDFGDIPlusImageScanlineSource.Create(AImageData, AImage);
end;

{ TdxPDFGDIPlusImageScanlineSource }

constructor TdxPDFGDIPlusImageScanlineSource.Create(const AImageData: TBytes;
  const AImage: IdxPDFDocumentImage);
begin
  inherited Create;
  FWidth := AImage.GetWidth;
  FSourceData := TdxPDFDCTDecoder.Decode(AImageData, FWidth, AImage.GetHeight);
  FComponentCount := AImage.GetColorSpaceComponentCount;
  FColorKeyMask := AImage.GetColorKeyMask;
end;

function TdxPDFGDIPlusImageScanlineSource.GetComponentCount: Integer;
begin
  Result := FComponentCount + Integer(GetHasAlpha);
end;

function TdxPDFGDIPlusImageScanlineSource.GetHasAlpha: Boolean;
begin
  Result := FColorKeyMask <> nil;
end;

procedure TdxPDFGDIPlusImageScanlineSource.FillBuffer(var ABuffer: TBytes;
  AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFGDIPlusImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  ASourceStride, I, ASourceOffset, ADestinationOffset, C, ADataOffset: Integer;
  AShouldApplyColorKeyMask, AIsMasked: Boolean;
  ASourceDataBytes: TBytes;
  AValue: Byte;
begin
  ASourceStride := FSourceData.Stride;
  AShouldApplyColorKeyMask := GetHasAlpha;
  ASourceDataBytes := FSourceData.Data;

  ASourceOffset := FOffset;
  ADestinationOffset := 0;
  for I := 0 to FWidth - 1 do
  begin
    AIsMasked := AShouldApplyColorKeyMask;
    if FComponentCount = 4 then
      for C := 0 to FComponentCount - 1 do
      begin
        AValue := Byte((255 - ASourceDataBytes[ASourceOffset + C]));
        AScanline[ADestinationOffset] := AValue;
        Inc(ADestinationOffset);
        if AIsMasked then
          AIsMasked := AIsMasked and FColorKeyMask[C].Contains(AValue);
      end
    else
    begin
      ADataOffset := ASourceOffset + FComponentCount - 1;
      for C := 0 to FComponentCount - 1 do
      begin
        AValue := ASourceDataBytes[ADataOffset];
        AScanline[ADestinationOffset] := AValue;
        Inc(ADestinationOffset);
        if AIsMasked then
          AIsMasked := AIsMasked and FColorKeyMask[C].Contains(AValue);
        Dec(ADataOffset);
      end;
    end;
    if AShouldApplyColorKeyMask then
    begin
      AScanline[ADestinationOffset] := IfThen(not AIsMasked, 255);
      Inc(ADestinationOffset);
    end;
    Inc(ASourceOffset, FComponentCount);
  end;
  FOffset := FOffset + ASourceStride;
end;

{ TdxPDFDCTDecoder }

class function TdxPDFDCTDecoder.CreateScanlineSource(const AImageData: TBytes;
  const AImage: IdxPDFDocumentImage; AComponentCount: Integer)
  : IdxPDFImageScanlineSource;
begin
  Result := GetFactory.CreateSource(AImageData, AImage);
end;

class function TdxPDFDCTDecoder.Decode(const AData: TBytes;
  AWidth, AHeight: Integer): TdxPDFDCTDecodeResult;
var
  ADecoder: TdxPDFDCTDecoder;
begin
  ADecoder := TdxPDFDCTDecoder.Create;
  try
    ADecoder.Initialize(AData, AWidth, AHeight);
    Result := ADecoder.DoDecode;
  finally
    ADecoder.Free;
  end;
end;

class procedure TdxPDFDCTDecoder.Finalize;
begin
  FreeAndNil(FFactory);
end;

procedure TdxPDFDCTDecoder.Initialize(const AData: TBytes;
  AWidth, AHeight: Integer);
begin
  FImageData := AData;
  FImageWidth := AWidth;
  FImageHeight := AHeight;
end;

class function TdxPDFDCTDecoder.GetFactory: TdxPDFDCTDecoderFactory;
begin
  if FFactory = nil then
  begin
    if TdxPDFUtils.UseWIC and dxPDFUseExternalDCTDecoder then
      FFactory := TdxPDFWICDCTDecoderFactory.Create
    else
      FFactory := TdxPDFGDIPlusDCTDecoderFactory.Create
  end;
  Result := FFactory;
end;

function TdxPDFDCTDecoder.DoDecode: TdxPDFDCTDecodeResult;

  function CreateBitmapStream(const AData: TBytes): TMemoryStream;
  begin
    Result := TMemoryStream.Create;
    Result.WriteBuffer(AData[0], Length(AData));
    Result.Position := 0;
  end;

  procedure TryDecode(const AData: TBytes);
  var
    AImage: TdxGPImage;
    ABitmapData: TBitmapData;
    ABitmapStream: TMemoryStream;
    ABitmapFormat: TdxGpPixelFormat;
    ARect: TdxGpRect;
  begin
    ARect := MakeRect(0, 0, FImageWidth, FImageHeight);
    ABitmapStream := CreateBitmapStream(FImageData);
    try
      AImage := TdxGPImage.CreateFromStream(ABitmapStream);
      try
        GdipCheck(GdipGetImagePixelFormat(AImage.Handle, ABitmapFormat));
        GdipCheck(GdipBitmapLockBits(AImage.Handle, @ARect, 1, ABitmapFormat,
          @ABitmapData));
        Result.Stride := ABitmapData.Stride;
        try
          SetLength(Result.Data, Result.Stride * FImageHeight);
          cxCopyData(ABitmapData.Scan0, @Result.Data[0], Length(Result.Data));
        finally
          GdipCheck(GdipBitmapUnlockBits(AImage.Handle, @ABitmapData));
        end;
      finally
        AImage.Free;
      end;
    finally
      ABitmapStream.Free;
    end;
  end;

begin
  try
    TryDecode(FImageData);
  except
    TryDecode(TdxPDFDCTDataValidator.ChangeImageHeight(FImageData,
      FImageHeight));
  end;
end;

{ TdxPDFDCTDataValidator }

class function TdxPDFDCTDataValidator.ChangeImageHeight(const AImageData
  : TBytes; ANewHeight: Integer): TBytes;
var
  AIndex: Integer;
  AMarkerType: Byte;
  AReader: TdxBigEndianStreamReader;
  AStream: TBytesStream;
begin
  AStream := TBytesStream.Create(AImageData);
  try
    AReader := TdxBigEndianStreamReader.Create(AStream);
    try
      while not AReader.IsEOF do
      begin
        if AReader.ReadByte <> $FF then
          TdxPDFUtils.RaiseException;
        AMarkerType := AReader.ReadByte;
        if ((AMarkerType >= $C0) and (AMarkerType <= $C3) or
          (AMarkerType >= $C5) and (AMarkerType <= $C7) or (AMarkerType >= $C9)
          and (AMarkerType <= $CB)) then
        begin
          AIndex := Integer(AStream.Position) + 3;
          Result := TdxByteArray.Clone(AImageData);
          Result[AIndex] := Byte(ANewHeight shr 8);
          Result[AIndex + 1] := Byte(ANewHeight);
          Break;
        end
        else if (AMarkerType <> $D8) and (AMarkerType <> $D9) then
          AStream.Position := AStream.Position + AReader.ReadInt16;
      end;
    finally
      AReader.Free;
    end;
  finally
    AStream.Free;
  end;
end;

initialization

finalization

TdxPDFDCTDecoder.Finalize;

end.
