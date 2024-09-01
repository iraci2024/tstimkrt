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

unit dxPDFFunction;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, dxPDFBase,
  dxPDFTypes, dxPDFCore, dxPDFPostScript;

type
  TdxPDFSampledDataCustomConverterClass = class of
    TdxPDFSampledDataCustomConverter;
  TdxPDFSampledDataCustomConverter = class;

  { TdxPDFCustomFunction }

  TdxPDFCustomFunction = class(TdxPDFObject)
  strict private
    FDomain: TdxPDFRanges;
    FRanges: TdxPDFRanges;

    class function DoParse(ARepository: TdxPDFCustomRepository;
      AObject: TdxPDFBase): TdxPDFCustomFunction;
    class function GetFunctionDictionary(AObject: TdxPDFBase; out AData: TBytes)
      : TdxPDFReaderDictionary;
    class function GetFunctionType(ADictionary: TdxPDFReaderDictionary)
      : Integer;

    function CreateDomain(AArray: TdxPDFArray): TdxPDFRanges; overload;
    function CreateDomain(AArray: TdxPDFArray; AIndex: Integer)
      : TdxPDFRanges; overload;
    procedure ReadDomain(ADictionary: TdxPDFDictionary);
    procedure RestrictValues(const AValues: TDoubleDynArray;
      const ARanges: TdxPDFRanges);
    procedure ValidateData(const AData: TBytes);
  protected
    class function Number: Integer; virtual;
    function GetRangeCount: Integer; virtual;
    function IsValidData(const AData: TBytes): Boolean; virtual;
    function NeedCheckEmptyRanges: Boolean; virtual;
    function PerformTransformation(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; virtual;
    procedure Read(ADictionary: TdxPDFReaderDictionary; const AData: TBytes);
      reintroduce; virtual;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function CompareRanges(const R1, R2: TdxPDFRanges): Boolean;
    function Interpolate(AValue, AMinValue, AMaxValue: Double;
      const ARange: TdxPDFRange): Double; inline;
    procedure ReadRanges(ADictionary: TdxPDFDictionary; const AKey: string;
      var ARanges: TdxPDFRanges);

    property Ranges: TdxPDFRanges read FRanges;
  public
    class function GetTypeName: string; override;
    class function Parse(ARepository: TdxPDFCustomRepository;
      AObject: TdxPDFBase): TdxPDFCustomFunction;

    function CreateTransformedComponents(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; virtual;
    function IsSame(AFunction: TdxPDFCustomFunction): Boolean; virtual;

    property Domain: TdxPDFRanges read FDomain;
    property RangeCount: Integer read GetRangeCount;
  end;

  { TdxPDFCustomPredefinedFunction }

  TdxPDFCustomPredefinedFunction = class(TdxPDFCustomFunction)
  public
    function CreateTransformedComponents(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; override;
  end;

  { TdxPDFDefaultFunction }

  TdxPDFDefaultFunction = class(TdxPDFCustomPredefinedFunction)
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFIdentityFunction }

  TdxPDFIdentityFunction = class(TdxPDFCustomPredefinedFunction)
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFSampledFunction }

  TdxPDFSampledFunction = class(TdxPDFCustomFunction)
  strict private
    FBitsPerSample: Integer;
    FDecode: TdxPDFRanges;
    FEncode: TdxPDFRanges;
    FOrder: Integer;
    FSamples: TInt64DynArray;
    FSizes: TIntegerDynArray;

    function CreateDecode: TdxPDFRanges;
    function CreateEncode: TdxPDFRanges;
    function CreateRanges(AArray: TdxPDFArray; ASize: Integer): TdxPDFRanges;
    function GetDataConverterClass(ABitsPerSample: Integer)
      : TdxPDFSampledDataCustomConverterClass;
    function GetMaxSampleValue: Int64;
    function GetSampleCount: Integer;
    function Interpolate(AArgument, AMinArgument, AMaxArgument: Double;
      const ARange: TdxPDFRange; AMinValue, AMaxValue: Double): Double;
      overload;
    function InterpolateSamples(const ACoordinates: TDoubleDynArray)
      : TDoubleDynArray;
    function Normalize(AValue: Double; AMin: Double; AMax: Double): Double;
    procedure ReadBitsPerSample(ADictionary: TdxPDFDictionary);
    procedure ReadDecode(ADictionary: TdxPDFDictionary);
    procedure ReadEncode(ADictionary: TdxPDFDictionary);
    procedure ReadInterpolation(ADictionary: TdxPDFDictionary);
    procedure ReadSamples(const AData: TBytes);
    procedure ReadSizes(ADictionary: TdxPDFDictionary);
    function WriteSamples: TBytes;
  protected
    function IsCubicInterpolation: Boolean;
    function IsValidData(const AData: TBytes): Boolean; override;
    function PerformTransformation(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      const AData: TBytes); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property BitsPerSample: Integer read FBitsPerSample;
    property Decode: TdxPDFRanges read FDecode;
    property Encode: TdxPDFRanges read FEncode;
    property Order: Integer read FOrder;
    property Samples: TInt64DynArray read FSamples;
    property Sizes: TIntegerDynArray read FSizes;
  public
    class function Number: Integer; override;
    function IsSame(AFunction: TdxPDFCustomFunction): Boolean; override;
  end;

  { TdxPDFSampledDataCustomConverter }

  TdxPDFSampledDataCustomConverter = class
  strict private
    FBitsPerSample: Integer;
    FSampleCount: Integer;
  protected
    property BitsPerSample: Integer read FBitsPerSample;
    property SampleCount: Integer read FSampleCount;
  public
    constructor Create(ABitsPerSample: Integer; ASampleCount: Integer);
    function Convert(const AData: TBytes): TInt64DynArray; overload; virtual;
    function Convert(const AData: TInt64DynArray): TBytes; overload; virtual;
  end;

  { TdxPDFSampledDataSingleByteConverter }

  TdxPDFSampledDataSingleByteConverter = class(TdxPDFSampledDataCustomConverter)
  public
    function Convert(const AData: TBytes): TInt64DynArray; override;
    function Convert(const AData: TInt64DynArray): TBytes; override;
  end;

  { TdxPDFSampledDataLowBitsCountConverter }

  TdxPDFSampledDataLowBitsCountConverter = class
    (TdxPDFSampledDataCustomConverter)
  strict private
    function GetDataLength: Integer; inline;
    function GetDivisor: Integer; inline;
    function GetLastElementSize: Integer; inline;
  public
    function Convert(const AData: TBytes): TInt64DynArray; override;
    function Convert(const AData: TInt64DynArray): TBytes; override;
  end;

  { TdxPDFSampledDataHighBitsCountConverter }

  TdxPDFSampledDataHighBitsCountConverter = class
    (TdxPDFSampledDataCustomConverter)
  strict private
    function GetMultiplier: Integer;
  public
    function Convert(const AData: TBytes): TInt64DynArray; override;
    function Convert(const AData: TInt64DynArray): TBytes; override;
  end;

  { TdxPDFExponentialInterpolationFunction }

  TdxPDFExponentialInterpolationFunction = class(TdxPDFCustomFunction)
  strict private
    FC0: TDoubleDynArray;
    FC1: TDoubleDynArray;
    FExponent: Double;

    function CreateArray(AArray: TdxPDFArray; AElementCount: Integer)
      : TDoubleDynArray;
    function CompareArrays(V1, V2: TDoubleDynArray): Boolean;
    procedure ReadC0(ADictionary: TdxPDFDictionary);
    procedure ReadC1(ADictionary: TdxPDFDictionary);
    procedure ReadExponent(ADictionary: TdxPDFDictionary);
    function WriteC0(AHelper: TdxPDFWriterHelper): TdxPDFArray;
    function WriteC1(AHelper: TdxPDFWriterHelper): TdxPDFArray;
  protected
    function GetRangeCount: Integer; override;
    function IsValidData(const AData: TBytes): Boolean; override;
    function NeedCheckEmptyRanges: Boolean; override;
    function PerformTransformation(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      const AData: TBytes); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property C0: TDoubleDynArray read FC0;
    property C1: TDoubleDynArray read FC1;
    property Exponent: Double read FExponent;
  public
    class function Number: Integer; override;
    function IsSame(AFunction: TdxPDFCustomFunction): Boolean; override;
  end;

  { TdxPDFStitchingFunction }

  TdxPDFStitchingFunction = class(TdxPDFCustomFunction)
  strict private
    FBounds: TDoubleDynArray;
    FEncode: TdxPDFRanges;
    FFunctions: TObjectList<TdxPDFCustomFunction>;

    function CreateFunctions(AObject: TdxPDFBase; AIsArray: Boolean)
      : TObjectList<TdxPDFCustomFunction>;
    procedure ReadBounds(ADictionary: TdxPDFDictionary);
    procedure ReadFunctions(ADictionary: TdxPDFReaderDictionary);
    procedure WriteFunctions(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  protected
    function GetRangeCount: Integer; override;
    function NeedCheckEmptyRanges: Boolean; override;
    function PerformTransformation(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      const AData: TBytes); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function Number: Integer; override;

    property Functions: TObjectList<TdxPDFCustomFunction> read FFunctions;
    property Bounds: TDoubleDynArray read FBounds;
  end;

  { TdxPDFPostScriptCalculatorFunction }

  TdxPDFPostScriptCalculatorFunction = class(TdxPDFCustomFunction)
  strict private
    FCode: TBytes;
    FInterpreter: TdxPDFPostScriptInterpreter;
    FProgram: TdxPDFReferencedObjects;
  protected
    function IsValidData(const AData: TBytes): Boolean; override;
    function PerformTransformation(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      const AData: TBytes); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property Code: TBytes read FCode;
  public
    destructor Destroy; override;

    class function Number: Integer; override;
    function IsSame(AFunction: TdxPDFCustomFunction): Boolean; override;
  end;

  { TdxPDFPostScriptGrayToCMYKColorFunction }

  TdxPDFPostScriptGrayToCMYKColorFunction = class
    (TdxPDFPostScriptCalculatorFunction)
  protected
    function PerformTransformation(const AArguments: TDoubleDynArray)
      : TDoubleDynArray; override;
  end;

implementation

uses
  Math, dxCore, dxPDFUtils;

type
  TdxPDFCustomFunctionClass = class of TdxPDFCustomFunction;

  { TdxPDFCustomFunction }

class function TdxPDFCustomFunction.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Functions + ' ';
end;

class function TdxPDFCustomFunction.Parse(ARepository: TdxPDFCustomRepository;
  AObject: TdxPDFBase): TdxPDFCustomFunction;
begin
  case AObject.ObjectType of
    otName, otDictionary:
      Result := DoParse(ARepository, AObject);
    otIndirectReference:
      Result := DoParse(ARepository,
        ARepository.GetObject(TdxPDFReference(AObject).Number) as TdxPDFBase);
    otStream:
      Result := DoParse(ARepository, TdxPDFStream(AObject));
  else
    Result := nil;
  end;
end;

function TdxPDFCustomFunction.CreateTransformedComponents(const AArguments
  : TDoubleDynArray): TDoubleDynArray;
begin
  RestrictValues(AArguments, Domain);
  Result := PerformTransformation(AArguments);
  if Ranges <> nil then
    RestrictValues(Result, Ranges);
end;

function TdxPDFCustomFunction.IsSame(AFunction: TdxPDFCustomFunction): Boolean;
var
  ACustomFunction: TdxPDFCustomFunction;
begin
  ACustomFunction := AFunction as TdxPDFCustomFunction;
  Result := (ACustomFunction <> nil) and (Number = ACustomFunction.Number) and
    CompareRanges(FDomain, ACustomFunction.Domain) and
    CompareRanges(FRanges, ACustomFunction.Ranges);
end;

function TdxPDFCustomFunction.GetRangeCount: Integer;
begin
  Result := Length(FRanges);
end;

function TdxPDFCustomFunction.IsValidData(const AData: TBytes): Boolean;
begin
  Result := True;
end;

function TdxPDFCustomFunction.NeedCheckEmptyRanges: Boolean;
begin
  Result := True;
end;

function TdxPDFCustomFunction.PerformTransformation(const AArguments
  : TDoubleDynArray): TDoubleDynArray;
begin
  TdxPDFUtils.AddData(AArguments, Result);
end;

class function TdxPDFCustomFunction.Number: Integer;
begin
  Result := -1;
end;

procedure TdxPDFCustomFunction.Read(ADictionary: TdxPDFReaderDictionary;
  const AData: TBytes);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    ValidateData(AData);
    ReadDomain(ADictionary);
    ReadRanges(ADictionary, TdxPDFKeywords.Range, FRanges);
  end;
end;

procedure TdxPDFCustomFunction.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FunctionType, Number);
  ADictionary.Add(TdxPDFKeywords.Domain, FDomain);
  ADictionary.Add(TdxPDFKeywords.Range, FRanges);
end;

function TdxPDFCustomFunction.CompareRanges(const R1, R2: TdxPDFRanges)
  : Boolean;
var
  I, ALength: Integer;
begin
  if R1 = nil then
    Exit(R2 = nil);
  ALength := Length(R1);
  if (R2 = nil) or (Length(R2) <> ALength) then
    Exit(False);
  for I := 0 to ALength - 1 do
    if (R1[I].Min <> R2[I].Min) or (R1[I].Max <> R2[I].Max) then
      Exit(False);
  Result := True;
end;

function TdxPDFCustomFunction.Interpolate(AValue, AMinValue, AMaxValue: Double;
  const ARange: TdxPDFRange): Double;
var
  AAddition, ADivider: Double;
begin
  AAddition := (AValue - AMinValue) * (ARange.Max - ARange.Min);
  ADivider := AMaxValue - AMinValue;
  if AAddition + ADivider = AAddition then
    Result := ARange.Min
  else
    Result := ARange.Min + AAddition / ADivider;
end;

procedure TdxPDFCustomFunction.ReadRanges(ADictionary: TdxPDFDictionary;
  const AKey: string; var ARanges: TdxPDFRanges);
var
  AArray: TdxPDFArray;
begin
  AArray := ADictionary.GetArray(AKey);
  if AArray <> nil then
    ARanges := CreateDomain(AArray)
  else if NeedCheckEmptyRanges then
    TdxPDFUtils.Abort;
end;

class function TdxPDFCustomFunction.DoParse(ARepository: TdxPDFCustomRepository;
  AObject: TdxPDFBase): TdxPDFCustomFunction;
const
  StringData = '{dup 0 mul exch dup 0 mul exch dup 0 mul exch 1 mul }';
  AlternateStringData1 =
    '{1.000000 2 1 roll 1.000000 2 1 roll 1.000000 2 1 roll 0 index 1.000000 \ncvr exch sub 2 1 ';
  AlternateStringData2 =
    'roll 5 -1 roll 1.000000 cvr exch sub 5 1 \nroll 4 -1 roll 1.000000 cvr exch sub 4 1 roll 3 ';
  AlternateStringData3 =
    '-1 roll 1.000000 \ncvr exch sub 3 1 roll 2 -1 roll 1.000000 cvr exch sub 2 1 \nroll pop }';
var
  S: string;
  AData: TBytes;
  ADictionary: TdxPDFReaderDictionary;
  AFunctionClass: TdxPDFCustomFunctionClass;
  AFunctionType: Integer;
begin
  ADictionary := GetFunctionDictionary(AObject, AData);

  if ADictionary <> nil then
    AFunctionType := GetFunctionType(ADictionary)
  else
    AFunctionType := -1;

  case AFunctionType of
    0:
      AFunctionClass := TdxPDFSampledFunction;
    2:
      AFunctionClass := TdxPDFExponentialInterpolationFunction;
    3:
      AFunctionClass := TdxPDFStitchingFunction;
    4:
      begin
        S := TdxPDFUtils.ConvertToUTF8String(AData);
        if (S = StringData) or (S = AlternateStringData1 + AlternateStringData2
          + AlternateStringData3) then
          AFunctionClass := TdxPDFPostScriptGrayToCMYKColorFunction
        else
          AFunctionClass := TdxPDFPostScriptCalculatorFunction;
      end;
  else
    if (AObject <> nil) and (AObject.ObjectType = otName) and
      (TdxPDFString(AObject).Value = 'Identity') then
      AFunctionClass := TdxPDFIdentityFunction
    else
      AFunctionClass := nil;
  end;

  if AFunctionClass <> nil then
  begin
    Result := AFunctionClass.Create(nil);
    Result.Read(ADictionary, AData);
  end
  else
  begin
    Result := nil;
    TdxPDFUtils.RaiseTestException('Unknown function');
  end;
end;

class function TdxPDFCustomFunction.GetFunctionDictionary(AObject: TdxPDFBase;
  out AData: TBytes): TdxPDFReaderDictionary;
begin
  Result := nil;
  if AObject <> nil then
  begin
    AData := nil;
    case AObject.ObjectType of
      otDictionary:
        begin
          Result := AObject as TdxPDFReaderDictionary;
          if TdxPDFDictionary(AObject).StreamRef <> nil then
            AData := TdxPDFDictionary(AObject).StreamRef.UncompressedData
        end;
      otStream:
        begin
          Result := TdxPDFStream(AObject).Dictionary as TdxPDFReaderDictionary;
          AData := TdxPDFStream(AObject).UncompressedData;
        end;
    else
      Result := nil;
    end;
  end;
end;

class function TdxPDFCustomFunction.GetFunctionType
  (ADictionary: TdxPDFReaderDictionary): Integer;
begin
  Result := ADictionary.GetInteger(TdxPDFKeywords.FunctionType);
  if not TdxPDFUtils.IsIntegerValid(Result) then
    TdxPDFUtils.RaiseTestException('Number is no valid');
end;

function TdxPDFCustomFunction.CreateDomain(AArray: TdxPDFArray): TdxPDFRanges;
var
  I, J, AIndex: Integer;
  ATemp: TdxPDFRanges;
begin
  if (AArray = nil) or (AArray.Count = 0) or (AArray.Count mod 2 > 0) then
    TdxPDFUtils.RaiseTestException;
  AIndex := 0;
  SetLength(Result, 0);
  for I := 0 to AArray.Count div 2 - 1 do
  begin
    ATemp := CreateDomain(AArray, AIndex);
    for J := 0 to Length(ATemp) - 1 do
      TdxPDFUtils.AddRange(TdxPDFRange.Create(ATemp[J].Min,
        ATemp[J].Max), Result);
    Inc(AIndex, 2);
  end;
end;

function TdxPDFCustomFunction.CreateDomain(AArray: TdxPDFArray; AIndex: Integer)
  : TdxPDFRanges;
var
  AMin, AMax: Double;
begin
  AMin := TdxPDFUtils.ConvertToDouble(AArray[AIndex]);
  AMax := TdxPDFUtils.ConvertToDouble(AArray[AIndex + 1]);
  SetLength(Result, 0);
  TdxPDFUtils.AddRange(TdxPDFRange.Create(Min(AMin, AMax), Max(AMin, AMax)
    ), Result);
end;

procedure TdxPDFCustomFunction.ReadDomain(ADictionary: TdxPDFDictionary);
begin
  FDomain := CreateDomain(ADictionary.GetArray(TdxPDFKeywords.Domain));
end;

procedure TdxPDFCustomFunction.RestrictValues(const AValues: TDoubleDynArray;
  const ARanges: TdxPDFRanges);
var
  I, ALength: Integer;
begin
  ALength := Length(ARanges);
  if Length(AValues) = ALength then
    for I := 0 to ALength - 1 do
      if AValues[I] < ARanges[I].Min then
        AValues[I] := ARanges[I].Min
      else if AValues[I] > ARanges[I].Max then
        AValues[I] := ARanges[I].Max;
end;

procedure TdxPDFCustomFunction.ValidateData(const AData: TBytes);
begin
  if not IsValidData(AData) then
    TdxPDFUtils.RaiseTestException(ClassName + ': Invalid function data');
end;

{ TdxPDFCustomPredefinedFunction }

function TdxPDFCustomPredefinedFunction.CreateTransformedComponents
  (const AArguments: TDoubleDynArray): TDoubleDynArray;
begin
  Result := AArguments;
end;

{ TdxPDFDefaultFunction }

class function TdxPDFDefaultFunction.GetTypeName: string;
begin
  Result := 'Default';
end;

{ TdxPDFIdentityFunction }

class function TdxPDFIdentityFunction.GetTypeName: string;
begin
  Result := 'Identity';
end;

{ TdxPDFSampledFunction }

class function TdxPDFSampledFunction.Number: Integer;
begin
  Result := 0;
end;

function TdxPDFSampledFunction.IsSame(AFunction: TdxPDFCustomFunction): Boolean;
var
  I, ALength: Integer;
  ASampledFunction: TdxPDFSampledFunction;
begin
  if not inherited IsSame(AFunction) then
    Exit(False);

  ASampledFunction := AFunction as TdxPDFSampledFunction;

  if (FBitsPerSample <> ASampledFunction.BitsPerSample) or
    (Order <> ASampledFunction.Order) or not CompareRanges(FEncode,
    ASampledFunction.Encode) or not CompareRanges(FDecode,
    ASampledFunction.Decode) then
    Exit(False);

  ALength := Length(ASampledFunction.Sizes);
  if Length(FSizes) <> ALength then
    Exit(False);

  for I := 0 to ALength - 1 do
    if FSizes[I] <> ASampledFunction.Sizes[I] then
      Exit(False);

  ALength := Length(ASampledFunction.Samples);
  if Length(Samples) <> ALength then
    Exit(False);

  for I := 0 to ALength - 1 do
    if FSamples[I] <> ASampledFunction.Samples[I] then
      Exit(False);

  Result := True;
end;

function TdxPDFSampledFunction.IsCubicInterpolation: Boolean;
begin
  Result := FOrder = 3;
end;

function TdxPDFSampledFunction.IsValidData(const AData: TBytes): Boolean;
begin
  Result := AData <> nil;
end;

function TdxPDFSampledFunction.PerformTransformation(const AArguments
  : TDoubleDynArray): TDoubleDynArray;
var
  I, ALength: Integer;
  AMaxSampleValue: Int64;
begin
  ALength := Length(Domain);
  if Length(AArguments) <> ALength then
  begin
    SetLength(Result, RangeCount);
    for I := 0 to RangeCount - 1 do
      Result[I] := dxPDFInvalidValue;
  end
  else
  begin
    for I := 0 to Length(AArguments) - 1 do
      AArguments[I] := Interpolate(AArguments[I], Domain[I].Min, Domain[I].Max,
        FEncode[I], 0, FSizes[I] - 1);
    Result := InterpolateSamples(AArguments);
    AMaxSampleValue := GetMaxSampleValue;
    for I := 0 to RangeCount - 1 do
      Result[I] := Interpolate(Result[I], 0, AMaxSampleValue, Decode[I],
        Ranges[I].Min, Ranges[I].Max);
  end;
end;

procedure TdxPDFSampledFunction.Read(ADictionary: TdxPDFReaderDictionary;
  const AData: TBytes);
begin
  inherited Read(ADictionary, AData);
  ReadSizes(ADictionary);
  ReadBitsPerSample(ADictionary);
  ReadInterpolation(ADictionary);
  ReadEncode(ADictionary);
  ReadDecode(ADictionary);
  ReadSamples(AData);
end;

procedure TdxPDFSampledFunction.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  inherited;

  ADictionary.Add(TdxPDFKeywords.Size, FSizes);
  ADictionary.Add(TdxPDFKeywords.BitsPerSample, FBitsPerSample);
  ADictionary.Add(TdxPDFKeywords.Order, FOrder);
  ADictionary.Add(TdxPDFKeywords.Encode, FEncode);
  ADictionary.Add(TdxPDFKeywords.Decode, FDecode);
  ADictionary.SetStreamData(WriteSamples);
end;

function TdxPDFSampledFunction.CreateDecode: TdxPDFRanges;
var
  I, ALength: Integer;
begin
  ALength := Length(Ranges);
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := TdxPDFRange.Create(Ranges[I].Min, Ranges[I].Max);
end;

function TdxPDFSampledFunction.CreateEncode: TdxPDFRanges;
var
  I, ALength: Integer;
begin
  ALength := Length(FSizes);
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := TdxPDFRange.Create(0, FSizes[I] - 1);
end;

function TdxPDFSampledFunction.CreateRanges(AArray: TdxPDFArray; ASize: Integer)
  : TdxPDFRanges;
var
  I, AIndex: Integer;
begin
  Result := nil;
  if AArray.Count = ASize * 2 then
  begin
    AIndex := 0;
    SetLength(Result, ASize);
    for I := 0 to ASize - 1 do
    begin
      Result[I] := TdxPDFRange.Create(TdxPDFUtils.ConvertToDouble(AArray[AIndex]
        ), TdxPDFUtils.ConvertToDouble(AArray[AIndex + 1]));
      Inc(AIndex, 2);
    end;
  end;
end;

function TdxPDFSampledFunction.GetSampleCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to Length(FSizes) - 1 do
    Result := Result * FSizes[I];
  Result := Result * Length(Ranges);
end;

function TdxPDFSampledFunction.GetDataConverterClass(ABitsPerSample: Integer)
  : TdxPDFSampledDataCustomConverterClass;
begin
  case ABitsPerSample of
    1, 2, 4:
      Result := TdxPDFSampledDataLowBitsCountConverter;
    8:
      Result := TdxPDFSampledDataSingleByteConverter;
    16, 24, 32:
      Result := TdxPDFSampledDataHighBitsCountConverter;
  else
    Result := nil;
  end;
end;

function TdxPDFSampledFunction.GetMaxSampleValue: Int64;
begin
  case FBitsPerSample of
    1:
      Result := 1;
    2:
      Result := 3;
    4:
      Result := 15;
    12:
      Result := 4095;
    16:
      Result := 65535;
    24:
      Result := 16777215;
    32:
      Result := 4294967295;
  else
    Result := 255;
  end;
end;

function TdxPDFSampledFunction.Interpolate(AArgument, AMinArgument,
  AMaxArgument: Double; const ARange: TdxPDFRange;
  AMinValue, AMaxValue: Double): Double;
begin
  AArgument := Normalize(AArgument, AMinArgument, AMaxArgument);
  AArgument := Interpolate(AArgument, AMinArgument, AMaxArgument, ARange);
  Result := Normalize(AArgument, AMinValue, AMaxValue);
end;

function TdxPDFSampledFunction.InterpolateSamples(const ACoordinates
  : TDoubleDynArray): TDoubleDynArray;
var
  ACode, APosition, ARate, I, ACoordinate0, AComponent: Integer;
  AFactor, ACoefficient: Double;
begin
  SetLength(Result, RangeCount);
  for I := 0 to RangeCount - 1 do
    Result[I] := 0;
  for ACode := 0 to (1 shl Length(ACoordinates)) - 1 do
  begin
    AFactor := 1;
    APosition := 0;
    ARate := 1;
    for I := 0 to Length(ACoordinates) - 1 do
    begin
      ACoordinate0 := Trunc(ACoordinates[I]);
      ACoefficient := ACoordinates[I] - ACoordinate0;
      if (ACode and (1 shl I)) = 0 then
      begin
        AFactor := AFactor * (1 - ACoefficient);
        Inc(APosition, ACoordinate0 * ARate);
      end
      else
      begin
        AFactor := AFactor * ACoefficient;
        Inc(APosition, (ACoordinate0 + 1) * ARate);
      end;
      ARate := ARate * FSizes[I];
    end;
    if AFactor > 0 then
    begin
      APosition := APosition * RangeCount;
      for AComponent := 0 to RangeCount - 1 do
      begin
        Result[AComponent] := Result[AComponent] + FSamples[APosition]
          * AFactor;
        Inc(APosition);
      end;
    end;
  end;
end;

function TdxPDFSampledFunction.Normalize(AValue: Double; AMin: Double;
  AMax: Double): Double;
begin
  Result := Min(Math.Max(AValue, AMin), AMax);
end;

procedure TdxPDFSampledFunction.ReadBitsPerSample
  (ADictionary: TdxPDFDictionary);
var
  AValue: Integer;
begin
  AValue := ADictionary.GetInteger(TdxPDFKeywords.BitsPerSample);
  if not TdxPDFUtils.IsIntegerValid(AValue) then
    TdxPDFUtils.Abort;
  FBitsPerSample := AValue;
end;

procedure TdxPDFSampledFunction.ReadDecode(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
begin
  AArray := ADictionary.GetArray(TdxPDFKeywords.Decode);
  if AArray <> nil then
    FDecode := CreateRanges(AArray, Length(Ranges))
  else
    FDecode := CreateDecode;
end;

procedure TdxPDFSampledFunction.ReadEncode(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
begin
  AArray := ADictionary.GetArray(TdxPDFKeywords.Encode);
  if AArray <> nil then
    FEncode := CreateRanges(AArray, Length(FSizes))
  else
    FEncode := CreateEncode;
end;

procedure TdxPDFSampledFunction.ReadInterpolation
  (ADictionary: TdxPDFDictionary);
begin
  FOrder := ADictionary.GetInteger(TdxPDFKeywords.Order);
end;

procedure TdxPDFSampledFunction.ReadSamples(const AData: TBytes);
var
  AConverter: TdxPDFSampledDataCustomConverter;
begin
  AConverter := GetDataConverterClass(FBitsPerSample).Create(FBitsPerSample,
    GetSampleCount);
  try
    FSamples := AConverter.Convert(AData);
  finally
    AConverter.Free;
  end;
end;

procedure TdxPDFSampledFunction.ReadSizes(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
  AElement: TdxPDFBase;
begin
  SetLength(FSizes, 0);
  AArray := ADictionary.GetArray(TdxPDFKeywords.Size);
  if AArray <> nil then
  begin
    for AElement in AArray.ElementList do
      if AElement.ObjectType = otInteger then
        TdxPDFUtils.AddValue(TdxPDFInteger(AElement).Value, FSizes)
      else
        TdxPDFUtils.RaiseTestException;
  end;
end;

function TdxPDFSampledFunction.WriteSamples: TBytes;
var
  AConverter: TdxPDFSampledDataCustomConverter;
begin
  AConverter := GetDataConverterClass(FBitsPerSample).Create(FBitsPerSample,
    GetSampleCount);
  try
    Result := AConverter.Convert(FSamples);
  finally
    AConverter.Free;
  end;
end;

{ TdxPDFSampledDataCustomConverter }

constructor TdxPDFSampledDataCustomConverter.Create(ABitsPerSample: Integer;
  ASampleCount: Integer);
begin
  inherited Create;
  FBitsPerSample := ABitsPerSample;
  FSampleCount := ASampleCount;
end;

function TdxPDFSampledDataCustomConverter.Convert
  (const AData: TInt64DynArray): TBytes;
begin
  SetLength(Result, SampleCount);
  FillChar(Result[0], SampleCount, 0);
end;

function TdxPDFSampledDataCustomConverter.Convert(const AData: TBytes)
  : TInt64DynArray;
begin
  SetLength(Result, SampleCount);
  FillChar(Result[0], SizeOf(Int64) * SampleCount, 0);
end;

{ TdxPDFSampledDataSingleByteConverter }

function TdxPDFSampledDataSingleByteConverter.Convert(const AData: TBytes)
  : TInt64DynArray;
var
  I: Integer;
begin
  if Length(AData) >= SampleCount then
  begin
    Result := inherited Convert(AData);
    for I := 0 to Length(Result) - 1 do
      Result[I] := AData[I];
  end
  else
    SetLength(Result, 0);
end;

function TdxPDFSampledDataSingleByteConverter.Convert
  (const AData: TInt64DynArray): TBytes;
var
  I: Integer;
begin
  if Length(AData) >= SampleCount then
  begin
    Result := inherited Convert(AData);
    for I := 0 to Length(Result) - 1 do
      Result[I] := AData[I];
  end
  else
    SetLength(Result, 0);
end;

{ TdxPDFSampledDataLowBitsCountConverter }

function TdxPDFSampledDataLowBitsCountConverter.Convert(const AData: TBytes)
  : TInt64DynArray;
var
  I, J, AIndex, AShift: Integer;
  AMask, ACurrentMask: Byte;
begin
  SetLength(Result, 0);
  if Length(AData) >= GetDataLength + IfThen(GetLastElementSize = 0, 0, 1) then
  begin
    Result := inherited Convert(AData);
    AIndex := 0;
    AMask := $0 shr BitsPerSample xor $FF;
    for I := 0 to GetDataLength - 1 do
    begin
      ACurrentMask := AMask;
      AShift := 8 - BitsPerSample;
      for J := 0 to GetDivisor - 1 do
      begin
        Result[AIndex] := AData[I] and ACurrentMask shr AShift;
        Inc(AIndex);
        Dec(AShift, BitsPerSample);
        ACurrentMask := BitsPerSample;
      end;
    end;
    if GetLastElementSize > 0 then
    begin
      AShift := 8 - BitsPerSample;
      for J := 0 to GetLastElementSize - 1 do
      begin
        Result[AIndex] := AData[GetLastElementSize - 1] and AMask shr AShift;
        Inc(AIndex);
        Dec(AShift, BitsPerSample);
      end;
    end;
  end;
end;

function TdxPDFSampledDataLowBitsCountConverter.Convert
  (const AData: TInt64DynArray): TBytes;
begin
  TdxPDFUtils.RaiseException
    ('TdxPDFSampledDataLowBitsCountConverter.Convert is not implemented');
end;

function TdxPDFSampledDataLowBitsCountConverter.GetDataLength: Integer;
begin
  Result := SampleCount div GetDivisor;
end;

function TdxPDFSampledDataLowBitsCountConverter.GetDivisor: Integer;
begin
  Result := 8 div BitsPerSample;
end;

function TdxPDFSampledDataLowBitsCountConverter.GetLastElementSize: Integer;
begin
  Result := SampleCount mod GetDivisor;
end;

{ TdxPDFSampledDataHighBitsCountConverter }

function TdxPDFSampledDataHighBitsCountConverter.GetMultiplier: Integer;
begin
  Result := BitsPerSample div 8;
end;

function TdxPDFSampledDataHighBitsCountConverter.Convert(const AData: TBytes)
  : TInt64DynArray;
var
  ASample: Int64;
  I, J, AIndex: Integer;
begin
  SetLength(Result, 0);
  if Length(AData) >= SampleCount * GetMultiplier then
  begin
    Result := inherited Convert(AData);
    AIndex := 0;
    for I := 0 to SampleCount - 1 do
    begin
      ASample := AData[AIndex];
      Inc(AIndex);
      for J := 1 to GetMultiplier - 1 do
      begin
        ASample := ASample shl 8 + AData[AIndex];
        Inc(AIndex);
      end;
      Result[I] := ASample;
    end;
  end;
end;

function TdxPDFSampledDataHighBitsCountConverter.Convert
  (const AData: TInt64DynArray): TBytes;
begin
  TdxPDFUtils.RaiseException
    ('TdxPDFSampledDataHighBitsCountConverter.Convert is not implemented');
end;

{ TdxPDFExponentialInterpolationFunction }

class function TdxPDFExponentialInterpolationFunction.Number: Integer;
begin
  Result := 2;
end;

function TdxPDFExponentialInterpolationFunction.IsSame
  (AFunction: TdxPDFCustomFunction): Boolean;
var
  AInterpolationFunction: TdxPDFExponentialInterpolationFunction;
begin
  if not inherited IsSame(AFunction) then
    Exit(False);
  AInterpolationFunction := TdxPDFExponentialInterpolationFunction(AFunction);
  Result := (FExponent = AInterpolationFunction.Exponent) and
    CompareArrays(FC0, AInterpolationFunction.C0) and
    CompareArrays(FC1, AInterpolationFunction.C1);
end;

function TdxPDFExponentialInterpolationFunction.GetRangeCount: Integer;
begin
  Result := Length(FC0);
end;

function TdxPDFExponentialInterpolationFunction.IsValidData
  (const AData: TBytes): Boolean;
begin
  Result := AData = nil;
end;

function TdxPDFExponentialInterpolationFunction.NeedCheckEmptyRanges: Boolean;
begin
  Result := False;
end;

function TdxPDFExponentialInterpolationFunction.PerformTransformation
  (const AArguments: TDoubleDynArray): TDoubleDynArray;
var
  I: Integer;
  AArgument: Double;
begin
  if Length(AArguments) <> 1 then
    TdxPDFUtils.AddData(AArguments, Result)
  else
  begin
    SetLength(Result, RangeCount);
    AArgument := Power(AArguments[0], FExponent);
    for I := 0 to RangeCount - 1 do
      Result[I] := FC0[I] + AArgument * (FC1[I] - FC0[I]);
  end;
end;

procedure TdxPDFExponentialInterpolationFunction.
  Read(ADictionary: TdxPDFReaderDictionary; const AData: TBytes);
begin
  inherited Read(ADictionary, AData);
  ReadC0(ADictionary);
  ReadC1(ADictionary);
  ReadExponent(ADictionary);
end;

procedure TdxPDFExponentialInterpolationFunction.
  Write(AHelper: TdxPDFWriterHelper; ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.Add(TdxPDFKeywords.C0, WriteC0(AHelper));
  ADictionary.Add(TdxPDFKeywords.C1, WriteC1(AHelper));
  ADictionary.Add(TdxPDFKeywords.Count, FExponent);
end;

function TdxPDFExponentialInterpolationFunction.CreateArray(AArray: TdxPDFArray;
  AElementCount: Integer): TDoubleDynArray;
begin
  if (AArray <> nil) and (AArray.Count = AElementCount) then
    Result := TdxPDFUtils.ArrayToDoubleArray(AArray)
  else
    Result := nil;
end;

function TdxPDFExponentialInterpolationFunction.CompareArrays(V1,
  V2: TDoubleDynArray): Boolean;
var
  I, ALength: Integer;
begin
  ALength := Length(V1);
  if ALength <> Length(V2) then
    Exit(False);
  for I := 0 to ALength - 1 do
    if V1[I] <> V2[I] then
      Exit(False);
  Result := True;
end;

procedure TdxPDFExponentialInterpolationFunction.ReadC0
  (ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
  ALength: Integer;
begin
  AArray := ADictionary.GetArray(TdxPDFKeywords.C0);
  if AArray = nil then
  begin
    SetLength(FC0, 0);
    TdxPDFUtils.AddValue(0, FC0);
  end
  else
  begin
    ALength := Length(Ranges);
    FC0 := CreateArray(AArray, IfThen(ALength > 0, ALength, AArray.Count));
  end;
end;

procedure TdxPDFExponentialInterpolationFunction.ReadC1
  (ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
begin
  if ADictionary <> nil then
  begin
    AArray := ADictionary.GetArray(TdxPDFKeywords.C1);
    if AArray = nil then
    begin
      SetLength(FC1, 0);
      TdxPDFUtils.AddValue(1, FC1);
    end
    else
      FC1 := CreateArray(AArray, Length(FC0));
    if Length(FC0) <> Length(FC1) then
      TdxPDFUtils.Abort;
  end;
end;

procedure TdxPDFExponentialInterpolationFunction.ReadExponent
  (ADictionary: TdxPDFDictionary);
begin
  FExponent := ADictionary.GetDouble(TdxPDFKeywords.Count);
  if not TdxPDFUtils.IsIntegerValid(Trunc(FExponent)) then
    TdxPDFUtils.Abort;
end;

function TdxPDFExponentialInterpolationFunction.WriteC0
  (AHelper: TdxPDFWriterHelper): TdxPDFArray;
begin
  Result := AHelper.CreateArray;
  Result.Add(FC0);
end;

function TdxPDFExponentialInterpolationFunction.WriteC1
  (AHelper: TdxPDFWriterHelper): TdxPDFArray;
begin
  Result := AHelper.CreateArray;
  Result.Add(FC1);
end;

{ TdxPDFStitchingFunction }

class function TdxPDFStitchingFunction.Number: Integer;
begin
  Result := 3;
end;

function TdxPDFStitchingFunction.GetRangeCount: Integer;
begin
  if Length(Ranges) = 0 then
    Result := FFunctions[0].RangeCount
  else
    Result := inherited GetRangeCount;
end;

function TdxPDFStitchingFunction.NeedCheckEmptyRanges: Boolean;
begin
  Result := False;
end;

function TdxPDFStitchingFunction.PerformTransformation(const AArguments
  : TDoubleDynArray): TDoubleDynArray;
var
  ADomain, AEncodeRange: TdxPDFRange;
  AXMin, AXMax: Double;
  AFunction: TdxPDFCustomFunction;
  AWasFound: Boolean;
  I, ABoundsLength: Integer;
  ATempResult: TDoubleDynArray;
begin
  if Length(AArguments) <> 1 then
    Exit(AArguments);
  ADomain := Domain[0];
  AFunction := nil;
  AXMin := ADomain.Min;
  AXMax := ADomain.Max;
  AEncodeRange := TdxPDFRange.Invalid;
  AWasFound := False;
  ABoundsLength := Length(FBounds);
  for I := 0 to ABoundsLength - 1 do
  begin
    AXMax := FBounds[I];
    if AArguments[0] < AXMax then
    begin
      AFunction := FFunctions[I];
      AEncodeRange := FEncode[I];
      AWasFound := True;
      Break;
    end;
    AXMin := AXMax;
  end;
  if not AWasFound then
  begin
    AFunction := FFunctions[ABoundsLength];
    AEncodeRange := FEncode[ABoundsLength];
    AXMax := ADomain.Max;
  end;
  SetLength(ATempResult, 1);
  ATempResult[0] := Interpolate(AArguments[0], AXMin, AXMax, AEncodeRange);
  Result := AFunction.CreateTransformedComponents(ATempResult);
end;

procedure TdxPDFStitchingFunction.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFunctions := TObjectList<TdxPDFCustomFunction>.Create;
end;

procedure TdxPDFStitchingFunction.DestroySubClasses;
begin
  FreeAndNil(FFunctions);
  inherited DestroySubClasses;
end;

procedure TdxPDFStitchingFunction.Read(ADictionary: TdxPDFReaderDictionary;
  const AData: TBytes);
begin
  inherited Read(ADictionary, AData);
  ReadFunctions(ADictionary);
  ReadRanges(ADictionary, TdxPDFKeywords.Encode, FEncode);
  if (Length(Domain) > 1) or (FFunctions.Count < 1) or
    (Length(FEncode) <> FFunctions.Count) then
    TdxPDFUtils.RaiseTestException;
  ReadBounds(ADictionary);
end;

procedure TdxPDFStitchingFunction.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  WriteFunctions(AHelper, ADictionary);
  ADictionary.Add(TdxPDFKeywords.Encode, FEncode);
  ADictionary.Add(TdxPDFKeywords.Bounds, FBounds, False);
end;

function TdxPDFStitchingFunction.CreateFunctions(AObject: TdxPDFBase;
  AIsArray: Boolean): TObjectList<TdxPDFCustomFunction>;
var
  AFunctionObject: TdxPDFBase;
begin
  Result := TObjectList<TdxPDFCustomFunction>.Create;
  if AObject.ObjectType = otArray then
  begin
    for AFunctionObject in TdxPDFArray(AObject).ElementList do
      Result.Add(TdxPDFCustomFunction.Parse(Repository, AFunctionObject))
  end
  else if AIsArray then
    TdxPDFUtils.RaiseTestException(ClassName + ' CreateFunctions')
  else
    Result.Add(TdxPDFCustomFunction.Parse(Repository, AObject));
end;

procedure TdxPDFStitchingFunction.ReadBounds(ADictionary: TdxPDFDictionary);
var
  I, ALength: Integer;
begin
  FBounds := ADictionary.CreateNumericList(TdxPDFKeywords.Bounds);
  ALength := Length(FBounds);
  if ALength <> FFunctions.Count - 1 then
    TdxPDFUtils.RaiseTestException;
  if ALength = 0 then
  begin
    if Domain[0].Min >= Domain[0].Max then
      TdxPDFUtils.RaiseTestException;
  end
  else
  begin
    if Domain[0].Min > FBounds[0] then
      TdxPDFUtils.RaiseTestException;
    for I := 1 to ALength - 1 do
    begin
      if FBounds[I] < FBounds[0] then
        TdxPDFUtils.RaiseTestException;
    end;
    if Domain[0].Max < FBounds[0] then
      TdxPDFUtils.RaiseTestException;
  end;
end;

procedure TdxPDFStitchingFunction.ReadFunctions(ADictionary
  : TdxPDFReaderDictionary);
var
  AFunction: TdxPDFCustomFunction;
  AObject: TdxPDFBase;
begin
  AObject := ADictionary.GetObject(TdxPDFKeywords.Functions);
  if AObject <> nil then
  begin
    FFunctions.Free;
    FFunctions := CreateFunctions(Repository.ResolveReference(AObject), True);
    for AFunction in FFunctions do
    begin
      if AFunction.RangeCount <> RangeCount then
        TdxPDFUtils.RaiseException;
    end;
  end;
end;

procedure TdxPDFStitchingFunction.WriteFunctions(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
var
  AFunction: TdxPDFCustomFunction;
  AFunctions: TdxPDFWriterArray;
begin
  AFunctions := AHelper.CreateArray;
  for AFunction in FFunctions do
    AFunctions.AddReference(AFunction);
  ADictionary.Add(TdxPDFKeywords.Functions, AFunctions);
end;

{ TdxPDFPostScriptCalculatorFunction }

destructor TdxPDFPostScriptCalculatorFunction.Destroy;
begin
  FreeAndNil(FProgram);
  FreeAndNil(FInterpreter);
  inherited Destroy;
end;

class function TdxPDFPostScriptCalculatorFunction.Number: Integer;
begin
  Result := 4;
end;

function TdxPDFPostScriptCalculatorFunction.IsSame
  (AFunction: TdxPDFCustomFunction): Boolean;
begin
  Result := inherited IsSame(AFunction) and
    (FCode = TdxPDFPostScriptCalculatorFunction(AFunction).Code);
end;

function TdxPDFPostScriptCalculatorFunction.IsValidData
  (const AData: TBytes): Boolean;
begin
  Result := AData <> nil;
end;

function TdxPDFPostScriptCalculatorFunction.PerformTransformation
  (const AArguments: TDoubleDynArray): TDoubleDynArray;
var
  I: Integer;
  AArgument: Double;
  AOperators: TdxPDFReferencedObjects;
  AParser: TdxPDFPostScriptFileParser;
begin
  if FProgram = nil then
  begin
    AParser := TdxPDFPostScriptFileParser.Create;
    try
      AOperators := AParser.Read(FCode);
    finally
      AParser.Free;
    end;
    if (AOperators <> nil) and (AOperators.Count <> 1) then
      TdxPDFUtils.Abort;
    FProgram := TdxPDFReferencedObjects.Create;
    for I := 0 to TdxPDFArray(AOperators[0]).Count - 1 do
      FProgram.Add(TdxPDFArray(AOperators[0])[I]);
    FInterpreter := TdxPDFPostScriptInterpreter.Create;
    AOperators.Free;
  end;
  for AArgument in AArguments do
    FInterpreter.Stack.Push(TdxPDFDouble.Create(AArgument));
  FInterpreter.Execute(FProgram);

  SetLength(Result, GetRangeCount);
  for I := 0 to GetRangeCount - 1 do
    Result[GetRangeCount - 1 - I] := FInterpreter.Stack.PopAsDouble;
  FInterpreter.Stack.Clear;
end;

procedure TdxPDFPostScriptCalculatorFunction.
  Read(ADictionary: TdxPDFReaderDictionary; const AData: TBytes);
begin
  inherited Read(ADictionary, AData);
  TdxPDFUtils.AddData(AData, FCode);
end;

procedure TdxPDFPostScriptCalculatorFunction.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.SetStreamData(FCode);
end;

{ TdxPDFPostScriptGrayToCMYKColorFunction }

function TdxPDFPostScriptGrayToCMYKColorFunction.PerformTransformation
  (const AArguments: TDoubleDynArray): TDoubleDynArray;
begin
  if Length(AArguments) = 1 then
  begin
    SetLength(Result, 4);
    Result[3] := AArguments[0];
  end
  else
    TdxPDFUtils.RaiseTestException
      ('TdxPDFPostScriptGrayToCMYKColorFunction.PerformTransformation');
end;

initialization

dxPDFRegisterDocumentObjectClass(TdxPDFDefaultFunction);
dxPDFRegisterDocumentObjectClass(TdxPDFIdentityFunction);

finalization

dxPDFUnregisterDocumentObjectClass(TdxPDFIdentityFunction);
dxPDFUnregisterDocumentObjectClass(TdxPDFDefaultFunction);

end.
