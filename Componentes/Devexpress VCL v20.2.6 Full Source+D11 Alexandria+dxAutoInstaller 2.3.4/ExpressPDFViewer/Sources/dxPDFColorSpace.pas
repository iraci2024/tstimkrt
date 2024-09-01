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

unit dxPDFColorSpace;

{$I cxVer.inc}

interface

uses
  SysUtils, Types, Classes, Generics.Defaults, Generics.Collections, Math,
  dxCoreClasses,
  dxPDFCore, dxPDFFunction, dxPDFBase, dxPDFTypes, dxPDFImageUtils;

type

  { TdxPDFICCBasedColorSpace }

  TdxPDFICCBasedColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FData: TBytes;
    FRanges: TdxPDFRanges;

    function CreateDefaultColorSpace: TdxPDFCustomColorSpace;
    function GetStream(AObject: TdxPDFBase): TdxPDFStream;
    procedure InitializeRanges;
    procedure DoReadRanges(AArray: TdxPDFArray);
    procedure DoWriteRanges(AArray: TdxPDFArray);
    procedure ReadAlternativeColorSpace(ADictionary: TdxPDFDictionary);
    procedure ReadComponentCount(ADictionary: TdxPDFDictionary);
    procedure ReadRanges(ADictionary: TdxPDFDictionary);
    procedure WriteAlternativeColorSpace(ADictionary: TdxPDFWriterDictionary);
    procedure WriteComponentCount(ADictionary: TdxPDFDictionary);
    procedure WriteRanges(ADictionary: TdxPDFDictionary);
  protected
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
  public
    class function GetTypeName: string; override;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer)
      : TdxPDFRanges; override;
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFCustomDeviceColorSpace }

  TdxPDFCustomDeviceColorSpaceClass = class of TdxPDFCustomDeviceColorSpace;

  TdxPDFCustomDeviceColorSpace = class(TdxPDFCustomColorSpace)
  public
    function AlternateTransform(const AColor: TdxPDFColor)
      : TdxPDFColor; override;
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
  end;

  { TdxPDFRGBDeviceColorSpace }

  TdxPDFRGBDeviceColorSpace = class(TdxPDFCustomDeviceColorSpace)
  protected
    function GetComponentCount: Integer; override;
  public
    class function GetTypeName: string; override;
    function AlternateTransform(const AColor: TdxPDFColor)
      : TdxPDFColor; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFGrayDeviceColorSpace }

  TdxPDFGrayDeviceColorSpace = class(TdxPDFCustomDeviceColorSpace)
  protected
    function GetComponentCount: Integer; override;
  public
    class function GetTypeName: string; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFCMYKDeviceColorSpace }

  TdxPDFCMYKDeviceColorSpace = class(TdxPDFCustomDeviceColorSpace)
  protected
    function GetComponentCount: Integer; override;
  public
    class function GetTypeName: string; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFIndexedColorSpace }

  TdxPDFIndexedColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FLookupTable: TBytes;
    FMaxIndex: Integer;

    procedure ReadLookupTable(AObject: TdxPDFBase);
    procedure ReadMaxIndex(AObject: TdxPDFBase);
  protected
    function CanRead(ASize: Integer): Boolean; override;
    function GetComponentCount: Integer; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
    procedure Initialize; override;
  public
    class function GetTypeName: string; override;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer)
      : TdxPDFRanges; override;
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
    function Transform(const AImage: IdxPDFDocumentImage;
      const AData: IdxPDFImageScanlineSource;
      const AParameters: TdxPDFImageParameters)
      : TdxPDFScanlineTransformationResult; override;

    property LookupTable: TBytes read FLookupTable;
    property MaxIndex: Integer read FMaxIndex;
  end;

  { TdxPDFSpecialColorSpace }

  TdxPDFSpecialColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FTintTransform: TdxPDFCustomFunction;

    procedure SetTintTransform(const AValue: TdxPDFCustomFunction);
  protected
    procedure DestroySubClasses; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
    procedure Initialize; override;

    property TintTransform: TdxPDFCustomFunction read FTintTransform
      write SetTintTransform;
  public
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFSeparationColorSpace }

  TdxPDFSeparationColorSpace = class(TdxPDFSpecialColorSpace)
  strict private
    procedure ReadName(AObject: TdxPDFBase);
  protected
    function CanRead(ASize: Integer): Boolean; override;
    function GetComponentCount: Integer; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFDeviceNColorSpace }

  TdxPDFDeviceNColorSpace = class(TdxPDFSpecialColorSpace)
  strict private
    FNames: TStringList;

    function IsValidNames(ANames: TStringList): Boolean;
    procedure ReadNames(AObject: TdxPDFBase);
    function WriteNames: TdxPDFArray;
  protected
    function CanRead(ASize: Integer): Boolean; override;
    function GetComponentCount: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;

    property Names: TStringList read FNames;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFNChannelColorSpace }

  TdxPDFNChannelColorSpace = class(TdxPDFDeviceNColorSpace)
  strict private
    FColorants: TObjectList<TdxPDFSeparationColorSpace>;
    FComponents: TStrings;

    function GetColorSpaceDictionary(AObject: TdxPDFBase)
      : TdxPDFReaderDictionary;
    procedure ReadColorants(ADictionary: TdxPDFReaderDictionary);
    procedure ReadComponentNames(ADictionary: TdxPDFReaderDictionary);
    function WriteAttributes(AHelper: TdxPDFWriterHelper): TdxPDFObject;
    function WriteColorants(AHelper: TdxPDFWriterHelper): TdxPDFObject;
    function WriteComponents: TdxPDFArray;
    function WriteProcessAttributes(AHelper: TdxPDFWriterHelper): TdxPDFObject;
  protected
    function CanRead(ASize: Integer): Boolean; override;
    procedure CheckComponentCount; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoRead(AArray: TdxPDFArray); overload; override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFPatternColorSpace }

  TdxPDFPatternColorSpace = class(TdxPDFCustomColorSpace)
  protected
    procedure Initialize; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
  public
    class function GetTypeName: string; override;
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
  end;

  { TdxPDFCIEColor }

  TdxPDFCIEColor = record
  public
    X: Double;
    Y: Double;
    Z: Double;

    class function Create(const AArray: TdxPDFArray): TdxPDFCIEColor; static;
    function IsEmpty: Boolean;
    function Write: TdxPDFArray;
  end;

  { TdxPDFGamma }

  TdxPDFGamma = record
  strict private
    FBlue: Double;
    FGreen: Double;
    FRed: Double;
  public
    class function Create: TdxPDFGamma; overload; static;
    class function Create(AArray: TdxPDFArray): TdxPDFGamma; overload; static;
    function IsDefault: Boolean;
    function Write: TdxPDFArray;

    property Blue: Double read FBlue;
    property Green: Double read FGreen;
    property Red: Double read FRed;
  end;

  { TdxPDFColorSpaceMatrix }

  TdxPDFColorSpaceMatrix = record
  strict private
    FXA: Double;
    FYA: Double;
    FZA: Double;
    FXB: Double;
    FYB: Double;
    FZB: Double;
    FXC: Double;
    FYC: Double;
    FZC: Double;
  public
    class function Create: TdxPDFColorSpaceMatrix; overload; static;
    class function Create(AArray: TdxPDFArray): TdxPDFColorSpaceMatrix;
      overload; static;
    function IsIdentity: Boolean;
    function Write: TdxPDFArray;

    property XA: Double read FXA;
    property YA: Double read FYA;
    property ZA: Double read FZA;
    property XB: Double read FXB;
    property YB: Double read FYB;
    property ZB: Double read FZB;
    property XC: Double read FXC;
    property YC: Double read FYC;
    property ZC: Double read FZC;
  end;

  { TdxPDFCIEBasedColorSpace }

  TdxPDFCIEBasedColorSpace = class(TdxPDFCustomColorSpace)
  strict private
    FBlackPoint: TdxPDFCIEColor;
    FWhitePoint: TdxPDFCIEColor;

    procedure ReadBlackPoint(AArray: TdxPDFArray);
    procedure ReadWhitePoint(AArray: TdxPDFArray);
  protected
    function CanRead(ASize: Integer): Boolean; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function GetColorSpaceDictionary(AObject: TdxPDFBase)
      : TdxPDFReaderDictionary;

    property BlackPoint: TdxPDFCIEColor read FBlackPoint;
    property WhitePoint: TdxPDFCIEColor read FWhitePoint;
  end;

  { TdxPDFCalRGBColorSpace }

  TdxPDFCalRGBColorSpace = class(TdxPDFCIEBasedColorSpace)
  strict private
    FGamma: TdxPDFGamma;
    FMatrix: TdxPDFColorSpaceMatrix;

    function ColorComponentTransferFunction(AComponent: Double): Double;
  protected
    function GetComponentCount: Integer; override;
    procedure CreateSubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
    //
    property Gamma: TdxPDFGamma read FGamma;
    property Matrix: TdxPDFColorSpaceMatrix read FMatrix;
  end;

  { TdxPDFCalGrayColorSpace }

  TdxPDFCalGrayColorSpace = class(TdxPDFCIEBasedColorSpace)
  strict private
    FGammaValue: Double;
  protected
    function GetComponentCount: Integer; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function Transform(const AColorComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFLabColorSpace }

  TdxPDFLabColorSpace = class(TdxPDFCIEBasedColorSpace)
  strict private
    FRangeA: TdxPDFRange;
    FRangeB: TdxPDFRange;

    function CorrectRange(const ARange: TdxPDFRange; AValue: Double): Double;
    function GammaFunction(X: Double): Double;
  protected
    function CanRead(ASize: Integer): Boolean; override;
    function GetComponentCount: Integer; override;
    function GetDecodedImageScanlineSource(const ADecoratingSource
      : IdxPDFImageScanlineSource; const AImage: IdxPDFDocumentImage;
      AWidth: Integer): IdxPDFImageScanlineSource; override;
    procedure Initialize; override;
    procedure DoRead(AArray: TdxPDFArray); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer)
      : TdxPDFRanges; override;
    function Transform(const AComponents: TDoubleDynArray)
      : TDoubleDynArray; override;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; override;
  end;

implementation

uses
  dxCore, dxPDFUtils;

type
  TdxPDFCustomColorSpaceAccess = class(TdxPDFCustomColorSpace);
  TdxPDFDictionaryAccess = class(TdxPDFDictionary);
  TdxPDFNumericObjectAccess = class(TdxPDFNumericObject);
  TdxPDFObjectAccess = class(TdxPDFObject);

  { TdxPDFICCBasedColorSpace }

class function TdxPDFICCBasedColorSpace.GetTypeName: string;
begin
  Result := 'ICCBased';
end;

function TdxPDFICCBasedColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;
begin
  Result := AlternateColorSpace.AlternateTransform
    (TdxPDFColor.Create(AComponents)).Components;
end;

function TdxPDFICCBasedColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := AlternateColorSpace.Transform(AData, AWidth);
end;

function TdxPDFICCBasedColorSpace.CreateDefaultDecodeArray(ABitsPerComponent
  : Integer): TdxPDFRanges;
begin
  SetLength(Result, 0);
  TdxPDFUtils.AddData(FRanges, Result);
end;

procedure TdxPDFICCBasedColorSpace.DoRead(AArray: TdxPDFArray);
var
  AStream: TdxPDFStream;
begin
  inherited DoRead(AArray);
  if (AArray <> nil) and (AArray.Count = 2) then
  begin
    AStream := GetStream(AArray[1]);
    FData := AStream.UncompressedData;
    ReadComponentCount(AStream.Dictionary);
    InitializeRanges;
    ReadAlternativeColorSpace(AStream.Dictionary);
    ReadRanges(AStream.Dictionary);
  end
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFICCBasedColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
var
  ADictionary: TdxPDFWriterDictionary;
begin
  inherited;

  ADictionary := AHelper.CreateDictionary;
  WriteComponentCount(ADictionary);
  WriteAlternativeColorSpace(ADictionary);
  WriteRanges(ADictionary);
  ADictionary.SetStreamData(FData);
  AArray.AddReference(ADictionary);
end;

procedure TdxPDFICCBasedColorSpace.Initialize;
begin
  InitializeRanges;
end;

procedure TdxPDFICCBasedColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadRanges(ADictionary);
end;

function TdxPDFICCBasedColorSpace.CreateDefaultColorSpace
  : TdxPDFCustomColorSpace;
begin
  case ComponentCount of
    1:
      Result := TdxPDFGrayDeviceColorSpace.Create(Self);
    4:
      Result := TdxPDFCMYKDeviceColorSpace.Create(Self);
  else
    Result := TdxPDFRGBDeviceColorSpace.Create(Self);
  end;
end;

function TdxPDFICCBasedColorSpace.GetStream(AObject: TdxPDFBase): TdxPDFStream;
begin
  case AObject.ObjectType of
    otIndirectReference:
      Result := Repository.GetStream(TdxPDFReference(AObject).Number);
    otStream:
      Result := TdxPDFStream(AObject);
  else
    Result := nil;
  end;
end;

procedure TdxPDFICCBasedColorSpace.InitializeRanges;
var
  I: Integer;
begin
  SetLength(FRanges, ComponentCount);
  for I := 0 to ComponentCount - 1 do
    FRanges[I] := TdxPDFRange.Create(0, 1);
end;

procedure TdxPDFICCBasedColorSpace.DoReadRanges(AArray: TdxPDFArray);

  function CalculateRange(AArray: TdxPDFArray; AIndex: Integer): TdxPDFRange;
  var
    AMin, AMax: Double;
  begin
    AMax := TdxPDFNumericObjectAccess(AArray[AIndex]).InternalValue;
    AMin := TdxPDFNumericObjectAccess(AArray[AIndex + 1]).InternalValue;
    AMin := Min(AMin, AMax);
    AMax := Max(AMin, AMax);
    Result := TdxPDFRange.Create(AMin, AMax);
  end;

var
  I, AIndex, ALength: Integer;
begin
  ALength := AArray.Count div 2;
  SetLength(FRanges, ALength);
  AIndex := 0;
  for I := 0 to ALength - 1 do
  begin
    FRanges[I] := CalculateRange(AArray, AIndex);
    Inc(AIndex, 2);
  end;
end;

procedure TdxPDFICCBasedColorSpace.DoWriteRanges(AArray: TdxPDFArray);
var
  I: Integer;
begin
  for I := 0 to Length(FRanges) - 1 do
  begin
    AArray.Add(FRanges[I].Min);
    AArray.Add(FRanges[I].Max);
  end;
end;

procedure TdxPDFICCBasedColorSpace.ReadAlternativeColorSpace
  (ADictionary: TdxPDFDictionary);
var
  AObject: TdxPDFBase;
begin
  if (ADictionary <> nil) and ADictionary.TryGetObject(TdxPDFKeywords.Alternate,
    AObject) then
    AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject)
  else
    AlternateColorSpace := CreateDefaultColorSpace;
end;

procedure TdxPDFICCBasedColorSpace.ReadComponentCount
  (ADictionary: TdxPDFDictionary);
begin
  if ADictionary <> nil then
  begin
    ComponentCount := ADictionary.GetInteger(TdxPDFKeywords.Count);
    if not TdxPDFUtils.IsIntegerValid(ComponentCount) then
      TdxPDFUtils.Abort;
  end;
end;

procedure TdxPDFICCBasedColorSpace.ReadRanges(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
begin
  if ADictionary.GetArray(TdxPDFKeywords.Range, AArray) then
  begin
    if AArray.Count <> ComponentCount * 2 then
      TdxPDFUtils.Abort
    else
      DoReadRanges(AArray);
  end;
end;

procedure TdxPDFICCBasedColorSpace.WriteAlternativeColorSpace
  (ADictionary: TdxPDFWriterDictionary);
begin
  // TODO: Optimization - Is AlternateColorSpace default?
  ADictionary.AddReference(TdxPDFKeywords.Alternate, AlternateColorSpace);
end;

procedure TdxPDFICCBasedColorSpace.WriteComponentCount
  (ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.Count, ComponentCount);
end;

procedure TdxPDFICCBasedColorSpace.WriteRanges(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
begin
  if Length(FRanges) <> 0 then
  begin
    AArray := TdxPDFArray.Create;
    DoWriteRanges(AArray);
    ADictionary.Add(TdxPDFKeywords.Range, AArray);
  end;
end;

{ TdxPDFCustomDeviceColorSpace }

function TdxPDFCustomDeviceColorSpace.AlternateTransform
  (const AColor: TdxPDFColor): TdxPDFColor;
begin
  Result := TdxPDFARGBColor.Convert(TdxPDFARGBColor.Create(AColor));
end;

function TdxPDFCustomDeviceColorSpace.Transform(const AComponents
  : TDoubleDynArray): TDoubleDynArray;
var
  I: Integer;
  AComponent, ACurrentComponent: Double;
begin
  Result := inherited Transform(AComponents);
  if Length(AComponents) > 0 then
    for AComponent in AComponents do
      if not InRange(AComponent, 0, 1) then
      begin
        SetLength(Result, Length(AComponents));
        for I := 0 to Length(AComponents) - 1 do
        begin
          ACurrentComponent := AComponents[I];
          if ACurrentComponent < 0 then
            Result[I] := 0
          else if ACurrentComponent > 1 then
            Result[I] := 1
          else
            Result[I] := ACurrentComponent;
        end;
        Break;
      end;
end;

{ TdxPDFRGBDeviceColorSpace }

class function TdxPDFRGBDeviceColorSpace.GetTypeName: string;
begin
  Result := 'DeviceRGB';
end;

function TdxPDFRGBDeviceColorSpace.AlternateTransform(const AColor: TdxPDFColor)
  : TdxPDFColor;
begin
  Result := AColor;
end;

function TdxPDFRGBDeviceColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create(AData, pfArgb24bpp);
end;

function TdxPDFRGBDeviceColorSpace.GetComponentCount: Integer;
begin
  Result := 3;
end;

{ TdxPDFGrayDeviceColorSpace }

class function TdxPDFGrayDeviceColorSpace.GetTypeName: string;
begin
  Result := 'DeviceGray';
end;

function TdxPDFGrayDeviceColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create(AData, pfGray8bit);
end;

function TdxPDFGrayDeviceColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

{ TdxPDFCMYKDeviceColorSpace }

class function TdxPDFCMYKDeviceColorSpace.GetTypeName: string;
begin
  Result := 'DeviceCMYK';
end;

function TdxPDFCMYKDeviceColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create
    (TdxPDFCMYKToRGBImageScanlineSource.Create(AData, AWidth), pfArgb24bpp);
end;

function TdxPDFCMYKDeviceColorSpace.GetComponentCount: Integer;
begin
  Result := 4;
end;

{ TdxPDFIndexedColorSpace }

class function TdxPDFIndexedColorSpace.GetTypeName: string;
begin
  Result := 'Indexed';
end;

function TdxPDFIndexedColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;

  function CreateTransformedComponents(const AColorComponents: TDoubleDynArray)
    : TDoubleDynArray;
  var
    I, AIndex, AComponentCount: Integer;
  begin
    AComponentCount := AlternateColorSpace.ComponentCount;
    SetLength(Result, AComponentCount);
    AIndex := Round(AColorComponents[0]) * AComponentCount;
    for I := 0 to AComponentCount - 1 do
    begin
      Result[I] := FLookupTable[AIndex] / 255;
      Inc(AIndex);
    end;
  end;

begin
  if Length(AComponents) = 1 then
    Result := AlternateColorSpace.Transform
      (CreateTransformedComponents(AComponents))
  else
    Result := inherited Transform(AComponents);
end;

function TdxPDFIndexedColorSpace.Transform(const AImage: IdxPDFDocumentImage;
  const AData: IdxPDFImageScanlineSource;
  const AParameters: TdxPDFImageParameters): TdxPDFScanlineTransformationResult;
var
  ADecoratedData: IdxPDFImageScanlineSource;
begin
  ADecoratedData := GetDecodedImageScanlineSource(AData, AImage,
    AImage.GetWidth);
  ADecoratedData := dxPDFImageScanlineSourceFactory.CreateIndexedScanlineSource
    (ADecoratedData, AImage.GetWidth, AImage.GetHeight,
    AImage.GetBitsPerComponent, FLookupTable,
    AlternateColorSpace.ComponentCount);
  ADecoratedData := AImage.GetInterpolatedScanlineSource(ADecoratedData,
    AParameters);
  Result := AlternateColorSpace.Transform(ADecoratedData, AParameters.Width);
end;

function TdxPDFIndexedColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 4;
end;

function TdxPDFIndexedColorSpace.CreateDefaultDecodeArray(ABitsPerComponent
  : Integer): TdxPDFRanges;
begin
  SetLength(Result, 1);
  Result[0] := TdxPDFRange.Create(0, 1 shl ABitsPerComponent - 1);
end;

function TdxPDFIndexedColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

procedure TdxPDFIndexedColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AArray[1]);
  ReadMaxIndex(AArray[2]);
  ReadLookupTable(AArray[3]);
end;

procedure TdxPDFIndexedColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
begin
  inherited;
  AArray.Add(AHelper.PrepareToWrite(AlternateColorSpace));
  AArray.Add(MaxIndex);
  AArray.AddBytes(FLookupTable);
end;

procedure TdxPDFIndexedColorSpace.Initialize;
begin
  inherited Initialize;
  FMaxIndex := dxPDFInvalidValue;
  SetLength(FLookupTable, 0);
end;

procedure TdxPDFIndexedColorSpace.ReadLookupTable(AObject: TdxPDFBase);
var
  AExpectedLength: Integer;
  AValue: TdxPDFReferencedObject;
begin
  if AObject.ObjectType = otString then
    FLookupTable := TdxPDFUtils.StrToByteArray(TdxPDFString(AObject).Value)
  else
  begin
    AValue := Repository.ResolveReference(AObject);
    FLookupTable := (AValue as TdxPDFStream).UncompressedData;
  end;
  AExpectedLength := AlternateColorSpace.ComponentCount * (FMaxIndex + 1);
  if Length(FLookupTable) <> AExpectedLength then
    SetLength(FLookupTable, AExpectedLength)
end;

procedure TdxPDFIndexedColorSpace.ReadMaxIndex(AObject: TdxPDFBase);
begin
  if AObject.ObjectType = otInteger then
    FMaxIndex := Min(Max(TdxPDFInteger(AObject).Value, 0), 255);
end;

{ TdxPDFSpecialColorSpace }

function TdxPDFSpecialColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;
begin
  Result := AlternateColorSpace.AlternateTransform
    (TdxPDFColor.Create(TintTransform.CreateTransformedComponents(AComponents)))
    .Components;
end;

function TdxPDFSpecialColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create
    (TdxPDFCIEBasedImageScanlineSource.Create(AData, Self, AWidth,
    ComponentCount), pfArgb24bpp);
end;

procedure TdxPDFSpecialColorSpace.DestroySubClasses;
begin
  TintTransform := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFSpecialColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AArray[2]);
  TintTransform := TdxPDFCustomFunction.Parse(Repository, AArray[3]);
  if TintTransform.RangeCount <> AlternateColorSpace.ComponentCount then
    TdxPDFUtils.Abort;
end;

procedure TdxPDFSpecialColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
begin
  inherited;

  AArray.Add(0); // reserved for descendants
  AArray.Add(AHelper.PrepareToWrite(AlternateColorSpace));
  AArray.AddReference(TintTransform);
end;

procedure TdxPDFSpecialColorSpace.Initialize;
begin
  inherited Initialize;
  TintTransform := nil;
end;

procedure TdxPDFSpecialColorSpace.SetTintTransform(const AValue
  : TdxPDFCustomFunction);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FTintTransform));
end;

{ TdxPDFSeparationColorSpace }

class function TdxPDFSeparationColorSpace.GetTypeName: string;
begin
  Result := 'Separation';
end;

function TdxPDFSeparationColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 4;
end;

function TdxPDFSeparationColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

procedure TdxPDFSeparationColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  ReadName(AArray[1]);
end;

procedure TdxPDFSeparationColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
begin
  inherited;
  AArray[1] := TdxPDFName.Create(Name);
end;

procedure TdxPDFSeparationColorSpace.ReadName(AObject: TdxPDFBase);
begin
  if (AObject.ObjectType in [otName, otString]) and
    (Length(TintTransform.Domain) = 1) then
    Name := (AObject as TdxPDFString).Value
  else
    TdxPDFUtils.Abort;
end;

{ TdxPDFDeviceNColorSpace }

class function TdxPDFDeviceNColorSpace.GetTypeName: string;
begin
  Result := 'DeviceN';
end;

function TdxPDFDeviceNColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := (ASize = 4) or (ASize = 5);
end;

function TdxPDFDeviceNColorSpace.GetComponentCount: Integer;
begin
  Result := FNames.Count;
end;

procedure TdxPDFDeviceNColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FNames := TStringList.Create;
end;

procedure TdxPDFDeviceNColorSpace.DestroySubClasses;
begin
  FreeAndNil(FNames);
  inherited DestroySubClasses;
end;

procedure TdxPDFDeviceNColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  if (AlternateColorSpace is TdxPDFRGBDeviceColorSpace) or
    (AlternateColorSpace is TdxPDFCMYKDeviceColorSpace) or
    (AlternateColorSpace is TdxPDFGrayDeviceColorSpace) or
    (AlternateColorSpace is TdxPDFICCBasedColorSpace) or
    (AlternateColorSpace is TdxPDFCIEBasedColorSpace) then
    ReadNames(AArray[1])
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFDeviceNColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
begin
  inherited;
  AArray[1] := WriteNames;
end;

function TdxPDFDeviceNColorSpace.IsValidNames(ANames: TStringList): Boolean;
var
  I, J: Integer;
begin
  Result := True;
  if ANames.Count > 1 then
    for I := 1 to ANames.Count - 1 do
      if ANames[I] <> 'None' then
        for J := 0 to I - 1 do
          if ANames[I] = ANames[J] then
            Exit(False);
end;

procedure TdxPDFDeviceNColorSpace.ReadNames(AObject: TdxPDFBase);
var
  I: Integer;
  AArray: TdxPDFArray;
  AName: TdxPDFName;
begin
  if AObject.ObjectType = otArray then
  begin
    AArray := AObject as TdxPDFArray;
    for I := 0 to AArray.Count - 1 do
    begin
      AName := AArray[I] as TdxPDFName;
      if AName.Value <> '' then
        FNames.Add(AName.Value);
    end;
    IsValidNames(FNames);
  end;
end;

function TdxPDFDeviceNColorSpace.WriteNames: TdxPDFArray;
var
  I: Integer;
begin
  Result := TdxPDFArray.Create;
  for I := 0 to Names.Count - 1 do
    Result.AddName(Names[I]);
end;

{ TdxPDFNChannelColorSpace }

class function TdxPDFNChannelColorSpace.GetTypeName: string;
begin
  Result := 'NChannel';
end;

function TdxPDFNChannelColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 5;
end;

procedure TdxPDFNChannelColorSpace.CheckComponentCount;
begin
  // do nothing
end;

procedure TdxPDFNChannelColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorants := TObjectList<TdxPDFSeparationColorSpace>.Create;
  FComponents := TStringList.Create;
end;

procedure TdxPDFNChannelColorSpace.DestroySubClasses;
begin
  FreeAndNil(FComponents);
  FreeAndNil(FColorants);
  inherited DestroySubClasses;
end;

procedure TdxPDFNChannelColorSpace.DoRead(AArray: TdxPDFArray);
var
  ADictionary: TdxPDFDictionary;
begin
  inherited DoRead(AArray);
  ADictionary := GetColorSpaceDictionary(AArray[4]);
  if ADictionary <> nil then
  begin
    ReadColorants(ADictionary.GetDictionary(TdxPDFKeywords.Colorants)
      as TdxPDFReaderDictionary);
    ReadComponentNames(ADictionary.GetDictionary(TdxPDFKeywords.Process)
      as TdxPDFReaderDictionary);
    ComponentCount := Names.Count;
  end;
end;

procedure TdxPDFNChannelColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
begin
  inherited;
  AArray[0] := TdxPDFName.Create(TdxPDFDeviceNColorSpace.GetTypeName);
  AArray.AddReference(WriteAttributes(AHelper));
end;

function TdxPDFNChannelColorSpace.GetColorSpaceDictionary(AObject: TdxPDFBase)
  : TdxPDFReaderDictionary;
begin
  if AObject.ObjectType <> otDictionary then
  begin
    Result := Repository.GetDictionary((AObject as TdxPDFReference).Number)
      as TdxPDFReaderDictionary;
    if Result = nil then
      TdxPDFUtils.Abort;
  end
  else
    Result := AObject as TdxPDFReaderDictionary;
end;

procedure TdxPDFNChannelColorSpace.ReadColorants(ADictionary
  : TdxPDFReaderDictionary);
var
  AColorSpace: TdxPDFSeparationColorSpace;
  AKey: string;
begin
  FColorants.Clear;
  if ADictionary <> nil then
    for AKey in TdxPDFDictionaryAccess(ADictionary).Items.Keys do
    begin
      AColorSpace := TdxPDFCustomColorSpace.Parse(Repository,
        ADictionary.GetObject(AKey)) as TdxPDFSeparationColorSpace;
      if (AColorSpace = nil) or (AColorSpace.Name <> AKey) or
        (AColorSpace.ComponentCount <> 1) then
        TdxPDFUtils.Abort;
      FColorants.Add(AColorSpace);
    end;
end;

procedure TdxPDFNChannelColorSpace.ReadComponentNames
  (ADictionary: TdxPDFReaderDictionary);
var
  AComponent: TdxPDFBase;
  AComponents: TdxPDFArray;
  AObject: TdxPDFBase;
  I: Integer;
begin
  FComponents.Clear;
  if ADictionary <> nil then
  begin
    if not ADictionary.GetArray(TdxPDFKeywords.Components, AComponents) then
      TdxPDFUtils.Abort;
    if not ADictionary.TryGetObject(TdxPDFKeywords.ColorSpace, AObject) then
      TdxPDFUtils.Abort;

    AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
    if AComponents.Count <> AlternateColorSpace.ComponentCount then
      TdxPDFUtils.Abort;

    for I := 0 to AComponents.Count - 1 do
    begin
      AComponent := AComponents[I];
      if AComponent.ObjectType = otName then
        FComponents.Add(TdxPDFName(AComponent).Value)
      else
        TdxPDFUtils.Abort;
    end;
  end;
end;

function TdxPDFNChannelColorSpace.WriteAttributes(AHelper: TdxPDFWriterHelper)
  : TdxPDFObject;
var
  AData: TdxPDFWriterDictionary;
begin
  AData := AHelper.CreateDictionary;
  AData.AddName(TdxPDFKeywords.Subtype, GetTypeName);
  if FColorants.Count > 0 then
    AData.AddReference(TdxPDFKeywords.Colorants, WriteColorants(AHelper));
  if FComponents.Count > 0 then
    AData.AddReference(TdxPDFKeywords.Process, WriteProcessAttributes(AHelper));
  Result := AHelper.CreateIndirectObject(AData);
end;

function TdxPDFNChannelColorSpace.WriteColorants(AHelper: TdxPDFWriterHelper)
  : TdxPDFObject;
var
  AColorSpace: TdxPDFCustomColorSpace;
  AData: TdxPDFWriterDictionary;
  I: Integer;
begin
  AData := AHelper.CreateDictionary;
  for I := 0 to FColorants.Count - 1 do
  begin
    AColorSpace := FColorants[I];
    AData.AddReference(TdxPDFCustomColorSpaceAccess(AColorSpace).Name,
      AColorSpace);
  end;
  Result := AHelper.CreateIndirectObject(AData);
end;

function TdxPDFNChannelColorSpace.WriteComponents: TdxPDFArray;
var
  I: Integer;
begin
  Result := TdxPDFArray.Create;
  for I := 0 to FComponents.Count - 1 do
    Result.AddName(FComponents[I]);
end;

function TdxPDFNChannelColorSpace.WriteProcessAttributes
  (AHelper: TdxPDFWriterHelper): TdxPDFObject;
var
  AData: TdxPDFDictionary;
begin
  AData := TdxPDFDictionary.Create;
  AData.Add(TdxPDFKeywords.ColorSpace,
    AHelper.PrepareToWrite(AlternateColorSpace));
  AData.Add(TdxPDFKeywords.Components, WriteComponents);
  Result := AHelper.CreateIndirectObject(AData);
end;

{ TdxPDFPatternColorSpace }

class function TdxPDFPatternColorSpace.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pattern;
end;

function TdxPDFPatternColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;
begin
  Result := AlternateColorSpace.Transform(AComponents);
end;

procedure TdxPDFPatternColorSpace.Initialize;
begin
  inherited Initialize;
  AlternateColorSpace := TdxPDFRGBDeviceColorSpace.Create(Self);
end;

procedure TdxPDFPatternColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  if AArray.Count = 2 then
    AlternateColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AArray[1]);
end;

procedure TdxPDFPatternColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
begin
  inherited;
  AArray.Add(AHelper.PrepareToWrite(AlternateColorSpace));
end;

{ TdxPDFCIEColor }

class function TdxPDFCIEColor.Create(const AArray: TdxPDFArray): TdxPDFCIEColor;
begin
  if AArray.Count = 3 then
  begin
    Result.X := Max(TdxPDFUtils.ConvertToDouble(AArray[0]), 0);
    Result.Y := Max(TdxPDFUtils.ConvertToDouble(AArray[1]), 0);
    Result.Z := Max(TdxPDFUtils.ConvertToDouble(AArray[2]), 0);
  end;
end;

function TdxPDFCIEColor.IsEmpty: Boolean;
begin
  Result := (X = 0) and (Y = 0) and (Z = 0);
end;

function TdxPDFCIEColor.Write: TdxPDFArray;
begin
  Result := TdxPDFArray.Create;
  Result.Add(X);
  Result.Add(Y);
  Result.Add(Z);
end;

{ TdxPDFGamma }

class function TdxPDFGamma.Create: TdxPDFGamma;
begin
  Result.FRed := 1;
  Result.FGreen := 1;
  Result.FBlue := 1;
end;

class function TdxPDFGamma.Create(AArray: TdxPDFArray): TdxPDFGamma;
begin
  Result := Create;
  if AArray.Count = 3 then
  begin
    Result.FRed := TdxPDFUtils.ConvertToDouble(AArray[0]);
    Result.FGreen := TdxPDFUtils.ConvertToDouble(AArray[1]);
    Result.FBlue := TdxPDFUtils.ConvertToDouble(AArray[2]);
  end;
end;

function TdxPDFGamma.IsDefault: Boolean;
begin
  Result := (FRed = 1) and (FGreen = 1) and (FBlue = 1);
end;

function TdxPDFGamma.Write: TdxPDFArray;
begin
  Result := TdxPDFArray.Create;
  Result.Add(FRed);
  Result.Add(FGreen);
  Result.Add(FBlue);
end;

{ TdxPDFColorSpaceMatrix }

class function TdxPDFColorSpaceMatrix.Create: TdxPDFColorSpaceMatrix;
begin
  Result.FXA := 1;
  Result.FYA := 0;
  Result.FZA := 0;
  Result.FXB := 0;
  Result.FYB := 1;
  Result.FZB := 0;
  Result.FXC := 0;
  Result.FYC := 0;
  Result.FZC := 1;
end;

class function TdxPDFColorSpaceMatrix.Create(AArray: TdxPDFArray)
  : TdxPDFColorSpaceMatrix;
begin
  Result := Create;
  if AArray.Count = 9 then
  begin
    Result.FXA := TdxPDFUtils.ConvertToDouble(AArray[0]);
    Result.FYA := TdxPDFUtils.ConvertToDouble(AArray[1]);
    Result.FZA := TdxPDFUtils.ConvertToDouble(AArray[2]);
    Result.FXB := TdxPDFUtils.ConvertToDouble(AArray[3]);
    Result.FYB := TdxPDFUtils.ConvertToDouble(AArray[4]);
    Result.FZB := TdxPDFUtils.ConvertToDouble(AArray[5]);
    Result.FXC := TdxPDFUtils.ConvertToDouble(AArray[6]);
    Result.FYC := TdxPDFUtils.ConvertToDouble(AArray[7]);
    Result.FZC := TdxPDFUtils.ConvertToDouble(AArray[8]);
  end;
end;

function TdxPDFColorSpaceMatrix.IsIdentity: Boolean;
begin
  Result := (FXA = 1) and (FYA = 0) and (FZA = 0) and (FXB = 0) and (FYB = 1)
    and (FZB = 0) and (FXC = 0) and (FYC = 0) and (FZC = 1);
end;

function TdxPDFColorSpaceMatrix.Write: TdxPDFArray;
begin
  Result := TdxPDFArray.Create;
  Result.Add(FXA);
  Result.Add(FYA);
  Result.Add(FZA);
  Result.Add(FXB);
  Result.Add(FYB);
  Result.Add(FZB);
  Result.Add(FXC);
  Result.Add(FYC);
  Result.Add(FZC);
end;

{ TdxPDFCIEBasedColorSpace }

function TdxPDFCIEBasedColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 2;
end;

procedure TdxPDFCIEBasedColorSpace.DoRead(AArray: TdxPDFArray);
begin
  inherited DoRead(AArray);
  if CanRead(AArray.Count) then
    Read(GetColorSpaceDictionary(AArray[1]) as TdxPDFReaderDictionary)
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFCIEBasedColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
  AArray: TdxPDFWriterArray);
var
  ADictionary: TdxPDFWriterDictionary;
begin
  inherited;
  ADictionary := AHelper.CreateDictionary;
  Write(AHelper, ADictionary);
  AArray.Add(ADictionary);
end;

procedure TdxPDFCIEBasedColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    ReadWhitePoint(ADictionary.GetArray(TdxPDFKeywords.WhitePoint));
    ReadBlackPoint(ADictionary.GetArray(TdxPDFKeywords.BlackPoint));
  end;
end;

procedure TdxPDFCIEBasedColorSpace.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.WhitePoint, WhitePoint.Write);
  ADictionary.Add(TdxPDFKeywords.BlackPoint, BlackPoint.Write);
end;

function TdxPDFCIEBasedColorSpace.GetColorSpaceDictionary(AObject: TdxPDFBase)
  : TdxPDFReaderDictionary;
begin
  case AObject.ObjectType of
    otDictionary:
      Result := AObject as TdxPDFReaderDictionary;
    otIndirectReference:
      begin
        Result := Repository.GetDictionary(TdxPDFReference(AObject).Number)
          as TdxPDFReaderDictionary;
        if Result = nil then
          TdxPDFUtils.Abort;
      end;
    otArray:
      begin
        if TdxPDFArray(AObject).Count <> 2 then
          TdxPDFUtils.RaiseTestException('Incorrect array value count');
        Result := GetColorSpaceDictionary(TdxPDFArray(AObject)[1])
          as TdxPDFReaderDictionary
      end
  else
    Result := nil;
  end;
end;

procedure TdxPDFCIEBasedColorSpace.ReadBlackPoint(AArray: TdxPDFArray);
begin
  if AArray <> nil then
    FBlackPoint := TdxPDFCIEColor.Create(AArray);
end;

procedure TdxPDFCIEBasedColorSpace.ReadWhitePoint(AArray: TdxPDFArray);
begin
  if AArray <> nil then
  begin
    FWhitePoint := TdxPDFCIEColor.Create(AArray);
    if (FWhitePoint.X <= 0) or (FWhitePoint.Y <> 1) or (FWhitePoint.Z <= 0) then
      TdxPDFUtils.Abort;
  end
  else
    TdxPDFUtils.Abort;
end;

{ TdxPDFCalRGBColorSpace }

class function TdxPDFCalRGBColorSpace.GetTypeName: string;
begin
  Result := 'CalRGB';
end;

function TdxPDFCalRGBColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;
var
  ARed, AGreen, ABlue, X, Y, Z: Double;
begin
  ARed := Power(AComponents[0], Gamma.Red);
  AGreen := Power(AComponents[1], Gamma.Green);
  ABlue := Power(AComponents[2], Gamma.Blue);
  if Matrix.IsIdentity then
  begin
    ARed := ColorComponentTransferFunction(ARed);
    AGreen := ColorComponentTransferFunction(AGreen);
    ABlue := ColorComponentTransferFunction(ABlue);
    X := TdxPDFColor.ClipColorComponent
      ((ARed * 0.4124 + AGreen * 0.3576 + ABlue * 0.1805 - BlackPoint.X) /
      (WhitePoint.X - BlackPoint.X));
    Y := TdxPDFColor.ClipColorComponent
      ((ARed * 0.2126 + AGreen * 0.7152 + ABlue * 0.0722 - BlackPoint.Y) /
      (WhitePoint.Y - BlackPoint.Y));
    Z := TdxPDFColor.ClipColorComponent
      ((ARed * 0.0193 + AGreen * 0.1192 + ABlue * 0.9505 - BlackPoint.Z) /
      (WhitePoint.Z - BlackPoint.Z));
  end
  else
  begin
    X := Matrix.XA * ARed + Matrix.XB * AGreen + Matrix.XC * ABlue;
    Y := Matrix.YA * ARed + Matrix.YB * AGreen + Matrix.YC * ABlue;
    Z := Matrix.ZA * ARed + Matrix.ZB * AGreen + Matrix.ZC * ABlue;
  end;
  Result := TdxPDFColor.GetComponents(X, Y, Z, WhitePoint.Z);
end;

function TdxPDFCalRGBColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create
    (TdxPDFCIEBasedImageScanlineSource.Create(AData, Self, AWidth, 3),
    pfArgb24bpp);
end;

function TdxPDFCalRGBColorSpace.GetComponentCount: Integer;
begin
  Result := 3;
end;

procedure TdxPDFCalRGBColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FGamma := TdxPDFGamma.Create;
  FMatrix := TdxPDFColorSpaceMatrix.Create;
end;

procedure TdxPDFCalRGBColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
begin
  inherited Read(ADictionary);
  if ADictionary.GetArray(TdxPDFKeywords.Gamma, AArray) then
    FGamma := TdxPDFGamma.Create(AArray);
  if ADictionary.GetArray(TdxPDFKeywords.Matrix, AArray) then
    FMatrix := TdxPDFColorSpaceMatrix.Create(AArray);
end;

procedure TdxPDFCalRGBColorSpace.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  inherited Write(AHelper, ADictionary);
  if not Gamma.IsDefault then
    ADictionary.Add(TdxPDFKeywords.Gamma, Gamma.Write);
  if not Matrix.IsIdentity then
    ADictionary.Add(TdxPDFKeywords.Matrix, Matrix.Write);
end;

function TdxPDFCalRGBColorSpace.ColorComponentTransferFunction
  (AComponent: Double): Double;
begin
  if AComponent > 0.04045 then
    Result := Power((AComponent + 0.055) / 1.055, 2.4)
  else
    Result := AComponent / 12.92;
end;

{ TdxPDFCalGrayColorSpace }

class function TdxPDFCalGrayColorSpace.GetTypeName: string;
begin
  Result := 'CalGray';
end;

function TdxPDFCalGrayColorSpace.Transform(const AColorComponents
  : TDoubleDynArray): TDoubleDynArray;
var
  ALuminosity: Double;
begin
  ALuminosity := TdxPDFUtils.Max
    (116 * Power(BlackPoint.Y + (WhitePoint.Y - BlackPoint.Y) *
    Power(AColorComponents[0], FGammaValue), 1.0 / 3.0) - 16, 0) / 100.0;
  SetLength(Result, 3);
  Result[0] := ALuminosity;
  Result[1] := ALuminosity;
  Result[2] := ALuminosity;
end;

function TdxPDFCalGrayColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create
    (TdxPDFCIEBasedImageScanlineSource.Create(AData, Self, AWidth, 1),
    pfArgb24bpp);
end;

function TdxPDFCalGrayColorSpace.GetComponentCount: Integer;
begin
  Result := 1;
end;

procedure TdxPDFCalGrayColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  FGammaValue := Max(ADictionary.GetDouble(TdxPDFKeywords.Gamma, 1), 0);
end;

procedure TdxPDFCalGrayColorSpace.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  inherited Write(AHelper, ADictionary);
  ADictionary.Add(TdxPDFKeywords.Gamma, FGammaValue);
end;

{ TdxPDFLabColorSpace }

class function TdxPDFLabColorSpace.GetTypeName: string;
begin
  Result := 'Lab';
end;

function TdxPDFLabColorSpace.CreateDefaultDecodeArray(ABitsPerComponent
  : Integer): TdxPDFRanges;
begin
  SetLength(Result, 3);
  Result[0] := TdxPDFRange.Create(0, 100);
  Result[1] := TdxPDFRange.Create(FRangeA.Min, FRangeA.Max);
  Result[2] := TdxPDFRange.Create(FRangeB.Min, FRangeB.Max);
end;

function TdxPDFLabColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;
var
  M: Double;
begin
  M := (AComponents[0] + 16) / 116;
  Result := TdxPDFColor.GetComponents
    (BlackPoint.X + (WhitePoint.X - BlackPoint.X) *
    GammaFunction(M + CorrectRange(FRangeA, AComponents[1]) / 500),
    BlackPoint.Y + (WhitePoint.Y - BlackPoint.Y) * GammaFunction(M),
    BlackPoint.Z + (WhitePoint.Z - BlackPoint.Z) *
    GammaFunction(M - CorrectRange(FRangeB, AComponents[2]) / 200),
    WhitePoint.Z);
end;

function TdxPDFLabColorSpace.Transform(const AData: IdxPDFImageScanlineSource;
  AWidth: Integer): TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create(AData, pfArgb24bpp);
end;

function TdxPDFLabColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := ASize = 4;
end;

function TdxPDFLabColorSpace.GetComponentCount: Integer;
begin
  Result := 3;
end;

function TdxPDFLabColorSpace.GetDecodedImageScanlineSource
  (const ADecoratingSource: IdxPDFImageScanlineSource;
  const AImage: IdxPDFDocumentImage; AWidth: Integer)
  : IdxPDFImageScanlineSource;
begin
  Result := TdxPDFLabColorSpaceImageScanlineSource.Create(ADecoratingSource,
    Self, AImage.GetDecodeRanges, AWidth);
end;

procedure TdxPDFLabColorSpace.Initialize;
begin
  inherited Initialize;
  FRangeA := TdxPDFRange.Create(-100, 100);
  FRangeB := TdxPDFRange.Create(-100, 100);
end;

procedure TdxPDFLabColorSpace.DoRead(AArray: TdxPDFArray);
begin
  Read(GetColorSpaceDictionary(AArray));
end;

procedure TdxPDFLabColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  AMinA, AMaxA, AMinB, AMaxB: Double;
begin
  inherited Read(ADictionary);

  if ADictionary.GetArray(TdxPDFKeywords.Range, AArray) and CanRead(AArray.Count)
  then
  begin
    AMinA := TdxPDFUtils.ConvertToDouble(AArray[0]);
    AMaxA := TdxPDFUtils.ConvertToDouble(AArray[1]);
    AMinB := TdxPDFUtils.ConvertToDouble(AArray[2]);
    AMaxB := TdxPDFUtils.ConvertToDouble(AArray[3]);
    if (AMaxA < AMinA) or (AMaxB < AMinB) then
      TdxPDFUtils.Abort;
    FRangeA := TdxPDFRange.Create(AMinA, AMaxA);
    FRangeB := TdxPDFRange.Create(AMinB, AMaxB);
  end;
end;

procedure TdxPDFLabColorSpace.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
var
  ARange: TdxPDFArray;
begin
  inherited Write(AHelper, ADictionary);
  ARange := TdxPDFArray.Create;
  ARange.Add(FRangeA.Min);
  ARange.Add(FRangeA.Max);
  ARange.Add(FRangeB.Min);
  ARange.Add(FRangeB.Max);
  ADictionary.Add(TdxPDFKeywords.Range, ARange);
end;

function TdxPDFLabColorSpace.CorrectRange(const ARange: TdxPDFRange;
  AValue: Double): Double;
begin
  Result := (AValue - ARange.Min) / (ARange.Max - ARange.Min) * 200 - 100;
end;

function TdxPDFLabColorSpace.GammaFunction(X: Double): Double;
begin
  if X >= 6.0 / 29.0 then
    Result := X * X * X
  else
    Result := (X - 4.0 / 29.0) * 108.0 / 841.0;
end;

initialization

dxPDFRegisterDocumentObjectClass(TdxPDFICCBasedColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFCMYKDeviceColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFRGBDeviceColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFGrayDeviceColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFPatternColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFSeparationColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFDeviceNColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFNChannelColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFIndexedColorSpace);
dxPDFRegisterDocumentObjectClass('I', TdxPDFIndexedColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFCalRGBColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFCalGrayColorSpace);
dxPDFRegisterDocumentObjectClass(TdxPDFLabColorSpace);
dxPDFRegisterDocumentObjectClass('CMYK', TdxPDFCMYKDeviceColorSpace);
dxPDFRegisterDocumentObjectClass('G', TdxPDFGrayDeviceColorSpace);
dxPDFRegisterDocumentObjectClass('RGB', TdxPDFRGBDeviceColorSpace);

finalization

dxPDFUnregisterDocumentObjectClass(TdxPDFLabColorSpace);
dxPDFUnregisterDocumentObjectClass('RGB', TdxPDFRGBDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass('G', TdxPDFGrayDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass('CMYK', TdxPDFCMYKDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFCalGrayColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFCalRGBColorSpace);
dxPDFUnregisterDocumentObjectClass('I', TdxPDFIndexedColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFIndexedColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFNChannelColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFDeviceNColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFSeparationColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFPatternColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFGrayDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFRGBDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFGrayDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFCMYKDeviceColorSpace);
dxPDFUnregisterDocumentObjectClass(TdxPDFICCBasedColorSpace);

end.
