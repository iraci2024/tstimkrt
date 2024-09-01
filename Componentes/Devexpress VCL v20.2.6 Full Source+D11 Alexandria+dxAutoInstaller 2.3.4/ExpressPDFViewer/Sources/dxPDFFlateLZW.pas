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

unit dxPDFFlateLZW; // for internal use

{$I cxVer.inc}
{$IFNDEF DELPHIXE3}
{$M-}
{$ENDIF}

interface

uses
  SysUtils, Classes, dxPDFTypes, dxPDFStreamFilter, dxPDFImageUtils;

type
  TdxPDFFlateLZWFilterPredictor = (fpNoPrediction = 1, fpTIFFPredictor = 2,
    fpPNGNonePrediction = 10, fpPNGSubPrediction = 11, fpPNGUpPrediction = 12,
    fpPNGAveragePrediction = 13, fpPNGPaethPrediction = 14,
    fpPNGOptimumPrediction = 15);
  TdxPDFPNGPrediction = (ppNone, ppSub, ppUp, ppAverage, ppPaeth);

  { IdxPDFFlateDataSource }

  IdxPDFFlateDataSource = interface
    ['{53ED767D-B5F9-4A7C-92B6-75A2ABE54B8D}']
    procedure FillBuffer(var ABuffer: TBytes);
  end;

  { TdxPDFFlateLZWDecodeFilter }

  TdxPDFFlateLZWDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
  const
    DefaultBitsPerComponent = 8;
    DefaultColumnCount = 1;
    DefaultColorCount = 1;
    DefaultPredictor = 1;
  strict private
    FBitsPerComponent: Integer;
    FColors: Integer;
    FColumns: Integer;
    FPredictor: TdxPDFFlateLZWFilterPredictor;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TdxPDFDictionary); override;

    function PerformDecode(const AData: TBytes): TBytes; virtual; abstract;
    procedure DoInitialize(ADictionary: TdxPDFDictionary); virtual;

    property BitsPerComponent: Integer read FBitsPerComponent;
    property Colors: Integer read FColors;
    property Columns: Integer read FColumns;
    property Predictor: TdxPDFFlateLZWFilterPredictor read FPredictor;
  public
    procedure Write(AParameters: TdxPDFDictionary); override;
  end;

  { TdxPDFFlateDecodeFilter }

  TdxPDFFlateDecodeFilter = class(TdxPDFFlateLZWDecodeFilter)
  protected
    function PerformDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    function Encode(const AData: TBytes): TBytes;
  end;

  { TdxPDFLZWStreamFilter }

  TdxPDFLZWDecodeFilter = class(TdxPDFFlateLZWDecodeFilter)
  strict private
  const
    DefaultEarlyChange = True;
  strict protected
    FEarlyChangeValue: Boolean;
  protected
    function PerformDecode(const AData: TBytes): TBytes; override;
    procedure DoInitialize(ADictionary: TdxPDFDictionary); override;
    procedure Initialize(ADecodeParameters: TdxPDFDictionary); override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    procedure Write(AParameters: TdxPDFDictionary); override;
  end;

  { TdxPDFFlateLZWDecodeFilterPredictor }

  TdxPDFFlateLZWDecodeFilterPredictor = class
  strict private
    FBytesPerPixel: Integer;
    FRowLength: Integer;
  protected
    function ActualRowLength: Integer; virtual; abstract;
    function Decode(const AData: TBytes): TBytes; overload; virtual; abstract;
    function CalculateRowCount(ADataLength: Integer): Integer;

    property BytesPerPixel: Integer read FBytesPerPixel;
    property RowLength: Integer read FRowLength;
  public
    constructor Create(AFilter: TdxPDFFlateLZWDecodeFilter); virtual;
    class function Decode(const AData: TBytes;
      AFilter: TdxPDFFlateLZWDecodeFilter): TBytes; overload; static;
  end;

  { TdxPDFFlateLZWDecodeFilterPredictorDataSource }

  TdxPDFFlateLZWDecodeFilterPredictorDataSource = class(TInterfacedObject,
    IdxPDFFlateDataSource)
  strict private
    FBitsPerComponent: Integer;
    FBytesPerPixel: Integer;
    FPreviousPixel: TBytes;
    FRowInitialOffset: Integer;
    FRowLength: Integer;
    FRowOffset: Integer;
    FSource: IdxPDFFlateDataSource;
    function GetRowLength: Integer;
  protected
    FCurrentRow: TBytes;

    procedure ProcessRow; virtual; abstract;
    procedure StartNextRow; virtual;
    procedure FillBuffer(var ABuffer: TBytes);

    property BitsPerComponent: Integer read FBitsPerComponent;
    property BytesPerPixel: Integer read FBytesPerPixel;
    property PreviousPixel: TBytes read FPreviousPixel;
    property RowLength: Integer read GetRowLength;
  public
    constructor Create(AFilter: TdxPDFFlateLZWDecodeFilter;
      const ASource: IdxPDFFlateDataSource; ARowInitialOffset: Integer);
  end;

  { TdxPDFPNGPredictorDataSource }

  TdxPDFPNGPredictorDataSource = class
    (TdxPDFFlateLZWDecodeFilterPredictorDataSource)
  strict private
    FPNGPrediction: TdxPDFPNGPrediction;
    FPreviousRow: TBytes;
    FTopLeftPixel: TBytes;
  protected
    procedure ProcessRow; override;
    procedure StartNextRow; override;
  public
    constructor Create(AFilter: TdxPDFFlateLZWDecodeFilter;
      const ASource: IdxPDFFlateDataSource);
  end;

  { TdxPDFTIFFPredictorDataSource }

  TdxPDFTIFFPredictorDataSource = class
    (TdxPDFFlateLZWDecodeFilterPredictorDataSource)
  protected
    procedure ProcessRow; override;
    procedure Process16bppRow;
  public
    constructor Create(AFilter: TdxPDFFlateLZWDecodeFilter;
      const ASource: IdxPDFFlateDataSource);
  end;

  { TdxPDFFlateEncoder }

  TdxPDFFlateEncoder = class
  public
    class function Encode(const AData: TBytes): TBytes; static;
  end;

implementation

uses
  Types, Generics.Defaults, Generics.Collections, Math, ZLib, dxCore, dxHash,
  dxPDFUtils;

type
  TdxPDFFlateLZWDecodeFilterPredictorClass = class of
    TdxPDFFlateLZWDecodeFilterPredictor;

  { TdxPDFPNGPredictor }

  TdxPDFPNGPredictor = class(TdxPDFFlateLZWDecodeFilterPredictor)
  protected
    function ActualRowLength: Integer; override;
    function Decode(const AData: TBytes): TBytes; override;
  end;

  { TdxPDFTIFFPrediction }

  TdxPDFTIFFPredictor = class(TdxPDFFlateLZWDecodeFilterPredictor)
  strict private
    FBitsPerComponent: Integer;
    FComponentCount: Integer;
    function Decode16bpp(const AData: TBytes): TBytes;
  protected
    function ActualRowLength: Integer; override;
    function Decode(const AData: TBytes): TBytes; override;
  public
    constructor Create(AFilter: TdxPDFFlateLZWDecodeFilter); override;
  end;

  { TdxLZWDecoder }

  TdxLZWDecoder = class
  strict private
    FClearTable: Integer;
    FCurrentDictionaryEntryLength: Integer;
    FCurrentDictionaryMaxEntryLength: Integer;
    FCurrentDictionarySize: Integer;
    FCurrentPosition: Integer;
    FCurrentSequenceLength: Integer;
    FCurrentSymbol: Byte;
    FData: TBytes;
    FDataSize: Integer;
    FDictionary: TDictionary<Integer, TBytes>;
    FEarlyChangeValue: Boolean;
    FEndOfData: Integer;
    FInitialSequenceLength: Integer;
    FNextCode: Integer;
    FRemainBits: Integer;

    function DoDecode: TBytes;
    procedure CalculateNextCode;
    procedure InitializeDictionaryParameters;
    procedure InitializeParameters(const AData: TBytes;
      AInitialSequenceLength: Integer);
    procedure PopulateDictionary(const ACurrentSequence, ANextSequence: TBytes);
    procedure PopulateNextSequence(const ACurrentSequence: TBytes;
      var ANextSequence: TBytes);
  public
    constructor Create;
    destructor Destroy; override;

    function Decode(const AData: TBytes; AInitialSequenceLength: Integer;
      AEarlyChangeValue: Boolean = False): TBytes; overload;
  end;

  { TdxPDFFlateLZWDecodeFilter }

function TdxPDFFlateLZWDecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  APerformedData: TBytes;
begin
  APerformedData := PerformDecode(AData);
  if FPredictor = fpNoPrediction then
    Result := APerformedData
  else
    Result := TdxPDFFlateLZWDecodeFilterPredictor.Decode(APerformedData, Self);
end;

procedure TdxPDFFlateLZWDecodeFilter.Initialize(ADecodeParameters
  : TdxPDFDictionary);
begin
  inherited Initialize(ADecodeParameters);
  FBitsPerComponent := DefaultBitsPerComponent;
  FColors := DefaultColorCount;
  FColumns := DefaultColumnCount;
  FPredictor := fpNoPrediction;
  if ADecodeParameters <> nil then
    DoInitialize(ADecodeParameters);
end;

procedure TdxPDFFlateLZWDecodeFilter.Write(AParameters: TdxPDFDictionary);
begin
  inherited Write(AParameters);
  AParameters.Add(TdxPDFKeywords.BitsPerComponent, FBitsPerComponent,
    DefaultBitsPerComponent);
  AParameters.Add(TdxPDFKeywords.Colors, FColors, DefaultColorCount);
  AParameters.Add(TdxPDFKeywords.Columns, FColumns, DefaultColumnCount);
  AParameters.Add(TdxPDFKeywords.Predictor, Ord(FPredictor), DefaultPredictor);
end;

procedure TdxPDFFlateLZWDecodeFilter.DoInitialize
  (ADictionary: TdxPDFDictionary);

  procedure SetParameter(ADictionary: TdxPDFDictionary;
    const AParameterKey: string; out AParameter: Integer);
  begin
    if ADictionary.Contains(AParameterKey) then
      AParameter := ADictionary.GetInteger(AParameterKey)
  end;

begin
  SetParameter(ADictionary, TdxPDFKeywords.BitsPerComponent, FBitsPerComponent);
  SetParameter(ADictionary, TdxPDFKeywords.Colors, FColors);
  SetParameter(ADictionary, TdxPDFKeywords.Columns, FColumns);
  FPredictor := TdxPDFFlateLZWFilterPredictor
    (ADictionary.GetInteger(TdxPDFKeywords.Predictor, DefaultPredictor));
end;

{ TdxLZWDecoder }

constructor TdxLZWDecoder.Create;
begin
  inherited Create;
  FDictionary := TDictionary<Integer, TBytes>.Create;
end;

destructor TdxLZWDecoder.Destroy;
begin
  SetLength(FData, 0);
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxLZWDecoder.Decode(const AData: TBytes;
  AInitialSequenceLength: Integer; AEarlyChangeValue: Boolean = False): TBytes;
begin
  if (AInitialSequenceLength >= 3) and (AInitialSequenceLength <= 9) and
    (Length(AData) > 0) then
  begin
    FEarlyChangeValue := AEarlyChangeValue;
    InitializeParameters(AData, AInitialSequenceLength);
    Result := DoDecode;
  end
  else
    Result := AData;
end;

function TdxLZWDecoder.DoDecode: TBytes;
var
  ACurrentSequence, ANextSequence: TBytes;
begin
  SetLength(ACurrentSequence, 0);
  CalculateNextCode;
  while FNextCode <> FEndOfData do
  begin
    if FNextCode = FClearTable then
    begin
      InitializeDictionaryParameters;
      SetLength(ACurrentSequence, 0);
    end
    else
    begin
      PopulateNextSequence(ACurrentSequence, ANextSequence);
      TdxPDFUtils.AddData(ANextSequence, Result);
      PopulateDictionary(ACurrentSequence, ANextSequence);
      ACurrentSequence := ANextSequence;
    end;
    CalculateNextCode;
  end;
end;

procedure TdxLZWDecoder.CalculateNextCode;
var
  ABitsToRead: Integer;
begin
  FNextCode := 0;
  ABitsToRead := FCurrentDictionaryEntryLength - FRemainBits;
  while ABitsToRead > 0 do
  begin
    Inc(FNextCode, FCurrentSymbol shl ABitsToRead);
    Inc(FCurrentPosition);
    if FCurrentPosition >= FDataSize then
    begin
      FNextCode := FEndOfData;
      Exit;
    end;
    FCurrentSymbol := FData[FCurrentPosition];
    FRemainBits := 8;
    Dec(ABitsToRead, FRemainBits);
  end;
  FRemainBits := -ABitsToRead;
  Inc(FNextCode, FCurrentSymbol shr FRemainBits);
  FCurrentSymbol := Byte(FCurrentSymbol and (1 shl FRemainBits - 1));
end;

procedure TdxLZWDecoder.InitializeDictionaryParameters;
begin
  FCurrentDictionaryEntryLength := FInitialSequenceLength;
  FCurrentDictionaryMaxEntryLength := (1 shl FInitialSequenceLength) - 1;
  FCurrentDictionarySize := FEndOfData + 1;
  FDictionary.Clear;
end;

procedure TdxLZWDecoder.InitializeParameters(const AData: TBytes;
  AInitialSequenceLength: Integer);
begin
  TdxPDFUtils.AddData(AData, FData);
  FInitialSequenceLength := AInitialSequenceLength;
  FCurrentPosition := 0;
  FRemainBits := 8;
  FDataSize := Length(AData);
  FCurrentSymbol := FData[FCurrentPosition];
  FClearTable := 1 shl (AInitialSequenceLength - 1);
  FEndOfData := FClearTable + 1;
  InitializeDictionaryParameters;
end;

procedure TdxLZWDecoder.PopulateDictionary(const ACurrentSequence,
  ANextSequence: TBytes);

  procedure EnsureEntryLength;
  begin
    if (FCurrentDictionarySize = FCurrentDictionaryMaxEntryLength) and
      (FCurrentDictionaryEntryLength < 12) then
    begin
      Inc(FCurrentDictionaryEntryLength);
      FCurrentDictionaryMaxEntryLength :=
        ((FCurrentDictionaryMaxEntryLength + 1) shl 1) - 1;
    end;
  end;

  procedure AddEntry(const ADictionaryEntry: TBytes);
  begin
    FDictionary.Add(FCurrentDictionarySize, ADictionaryEntry);
    Inc(FCurrentDictionarySize);
  end;

var
  ADictionaryEntry: TBytes;
begin
  if FCurrentSequenceLength > 0 then
  begin
    SetLength(ADictionaryEntry, FCurrentSequenceLength + 1);
    TdxPDFUtils.CopyData(ACurrentSequence, 0, ADictionaryEntry, 0,
      Length(ACurrentSequence));
    ADictionaryEntry[FCurrentSequenceLength] := ANextSequence[0];
    if FEarlyChangeValue then
    begin
      AddEntry(ADictionaryEntry);
      EnsureEntryLength;
    end
    else
    begin
      EnsureEntryLength;
      AddEntry(ADictionaryEntry);
    end;
  end;
end;

procedure TdxLZWDecoder.PopulateNextSequence(const ACurrentSequence: TBytes;
  var ANextSequence: TBytes);
begin
  FCurrentSequenceLength := Length(ACurrentSequence);
  SetLength(ANextSequence, 0);
  if FNextCode < FClearTable then
  begin
    SetLength(ANextSequence, 1);
    ANextSequence[0] := FNextCode;
  end
  else if FNextCode < FCurrentDictionarySize then
    ANextSequence := FDictionary[FNextCode]
  else
  begin
    SetLength(ANextSequence, FCurrentSequenceLength + 1);
    TdxPDFUtils.CopyData(ACurrentSequence, 0, ANextSequence, 0,
      Length(ACurrentSequence));
    ANextSequence[FCurrentSequenceLength] := ACurrentSequence[0];
  end;
end;

{ TdxPDFFlateDecodeFilter }

class function TdxPDFFlateDecodeFilter.GetName: string;
begin
  Result := TdxPDFKeywords.FlateDecode;
end;

class function TdxPDFFlateDecodeFilter.GetShortName: string;
begin
  Result := 'Fl';
end;

function TdxPDFFlateDecodeFilter.Encode(const AData: TBytes): TBytes;
begin
  Result := TdxPDFFlateEncoder.Encode(AData);
end;

function TdxPDFFlateDecodeFilter.PerformDecode(const AData: TBytes): TBytes;
var
  ABuffer: TBytes;
  ACompressionMethod, AFlags: Byte;
  ACount, ACompressionInfo, ADataLength: Integer;
  ADecompressionStream: TZDecompressionStream;
  AInput: TBytesStream;
  APrevStreamPosition: Int64;
begin
  SetLength(Result, 0);
  ADataLength := Length(AData);
  if ADataLength < 2 then
    Exit;
  ACompressionMethod := AData[0];
  AFlags := AData[1];
  ACompressionInfo := (ACompressionMethod and $F0) shr 4 + 8;
  if ((ACompressionMethod and $0F) <> 8) or (ACompressionInfo > 15) or
    ((AFlags and $20) > 0) or (((ACompressionMethod * 256 + AFlags) mod 31) > 0)
  then
    Exit;
  if ADataLength = 2 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  AInput := TBytesStream.Create(AData);
  try
    ADecompressionStream := TDecompressionStream.Create(AInput);
    try
      while True do
      begin
        SetLength(ABuffer, 256);
        APrevStreamPosition := ADecompressionStream.Position;
        try
          ACount := ADecompressionStream.Read(ABuffer[0], 256);
        except
          ACount := ADecompressionStream.Position - APrevStreamPosition;
        end;
        if ACount = 256 then
          TdxPDFUtils.AddData(ABuffer, Result)
        else
        begin
          if ACount <> 0 then
          begin
            SetLength(ABuffer, ACount);
            TdxPDFUtils.AddData(ABuffer, Result);
          end;
          Break;
        end;
      end;
    finally
      ADecompressionStream.Free;
    end;
  finally
    AInput.Free;
  end;
end;

{ TdxPDFLZWDecodeFilter }

class function TdxPDFLZWDecodeFilter.GetName: string;
begin
  Result := TdxPDFKeywords.LZWDecode;
end;

class function TdxPDFLZWDecodeFilter.GetShortName: string;
begin
  Result := 'LZW';
end;

procedure TdxPDFLZWDecodeFilter.Write(AParameters: TdxPDFDictionary);
begin
  inherited Write(AParameters);
  AParameters.Add(TdxPDFKeywords.EarlyChange, FEarlyChangeValue,
    DefaultEarlyChange);
end;

function TdxPDFLZWDecodeFilter.PerformDecode(const AData: TBytes): TBytes;
var
  ADecoder: TdxLZWDecoder;
begin
  ADecoder := TdxLZWDecoder.Create;
  try
    Result := ADecoder.Decode(AData, 9, FEarlyChangeValue);
  finally
    ADecoder.Free;
  end;
end;

procedure TdxPDFLZWDecodeFilter.DoInitialize(ADictionary: TdxPDFDictionary);
begin
  FEarlyChangeValue := ADictionary.GetBoolean(TdxPDFKeywords.EarlyChange,
    DefaultEarlyChange);
end;

procedure TdxPDFLZWDecodeFilter.Initialize(ADecodeParameters: TdxPDFDictionary);
begin
  FEarlyChangeValue := True;
  inherited Initialize(ADecodeParameters);
end;

{ TdxPDFFlateLZWDecodeFilterPredictor }

constructor TdxPDFFlateLZWDecodeFilterPredictor.Create
  (AFilter: TdxPDFFlateLZWDecodeFilter);
var
  ABitsFactor, ARowComponentCount: Integer;
begin
  inherited Create;
  FBytesPerPixel := Max(AFilter.BitsPerComponent * AFilter.Colors div 8, 1);
  if AFilter.BitsPerComponent = 16 then
    FRowLength := AFilter.Columns * AFilter.Colors * 2
  else
  begin
    ABitsFactor := 8 div AFilter.BitsPerComponent;
    ARowComponentCount := AFilter.Columns * AFilter.Colors;
    FRowLength := ARowComponentCount div ABitsFactor;
    if ARowComponentCount mod ABitsFactor <> 0 then
      Inc(FRowLength);
  end;
end;

class function TdxPDFFlateLZWDecodeFilterPredictor.Decode(const AData: TBytes;
  AFilter: TdxPDFFlateLZWDecodeFilter): TBytes;
var
  APredictor: TdxPDFFlateLZWDecodeFilterPredictor;
  APredictorClass: TdxPDFFlateLZWDecodeFilterPredictorClass;
begin
  case AFilter.Predictor of
    fpPNGNonePrediction, fpPNGSubPrediction, fpPNGUpPrediction,
      fpPNGAveragePrediction, fpPNGPaethPrediction, fpPNGOptimumPrediction:
      APredictorClass := TdxPDFPNGPredictor;
    fpTIFFPredictor:
      APredictorClass := TdxPDFTIFFPredictor;
  else
    Exit(AData);
  end;
  APredictor := APredictorClass.Create(AFilter);
  try
    Result := APredictor.Decode(AData);
  finally
    APredictor.Free;
  end;
end;

function TdxPDFFlateLZWDecodeFilterPredictor.CalculateRowCount
  (ADataLength: Integer): Integer;
begin
  Result := ADataLength div ActualRowLength;
  if ADataLength mod ActualRowLength <> 0 then
    Inc(Result);
end;

{ TdxPDFPNGPredictor }

function TdxPDFPNGPredictor.ActualRowLength: Integer;
begin
  Result := RowLength + 1;
end;

function TdxPDFPNGPredictor.Decode(const AData: TBytes): TBytes;
var
  AActualDataElement, AChoose, ADataElement, ALeft, ATop, ATopLeft: Byte;
  AComponent, ADataLength, ADLeft, ADTop, ADTopLeft, APixelIndex: Integer;
  APNGPrediction: TdxPDFPNGPrediction;
  APrediction, AResultIndex, ARow, ARowCount, ASourceIndex: Integer;
  APreviousRow, APreviousPixel, ATopLeftPixel: TBytes;
begin
  ADataLength := Length(AData);
  ARowCount := CalculateRowCount(ADataLength);
  SetLength(Result, RowLength * ARowCount);
  SetLength(APreviousRow, RowLength);
  SetLength(APreviousPixel, BytesPerPixel);
  SetLength(ATopLeftPixel, BytesPerPixel);
  ASourceIndex := 0;
  AResultIndex := 0;
  for ARow := 0 to ARowCount - 1 do
  begin
    TdxPDFUtils.ClearData(APreviousPixel);
    TdxPDFUtils.ClearData(ATopLeftPixel);
    APNGPrediction := TdxPDFPNGPrediction(AData[ASourceIndex]);
    Inc(ASourceIndex);
    APixelIndex := 0;
    for AComponent := 0 to RowLength - 1 do
    begin
      ADataElement := AData[ASourceIndex];
      Inc(ASourceIndex);
      case APNGPrediction of
        ppNone:
          AActualDataElement := ADataElement;
        ppSub:
          AActualDataElement := APreviousPixel[APixelIndex] + ADataElement;
        ppUp:
          AActualDataElement := APreviousRow[AComponent] + ADataElement;
        ppAverage:
          AActualDataElement :=
            (APreviousPixel[APixelIndex] + APreviousRow[AComponent]) div 2 +
            ADataElement;
        ppPaeth:
          begin
            ALeft := APreviousPixel[APixelIndex];
            ATop := APreviousRow[AComponent];
            ATopLeft := ATopLeftPixel[APixelIndex];
            APrediction := ALeft + ATop - ATopLeft;
            ADLeft := Abs(APrediction - ALeft);
            ADTop := Abs(APrediction - ATop);
            ADTopLeft := Abs(APrediction - ATopLeft);
            if ADLeft <= ADTop then
              AChoose := IfThen(ADLeft <= ADTopLeft, ALeft, ATopLeft)
            else
              AChoose := IfThen(ADTop <= ADTopLeft, ATop, ATopLeft);
            AActualDataElement := AChoose + ADataElement;
          end;
      else
        TdxPDFUtils.Abort;
        AActualDataElement := ADataElement;
      end;
      Result[AResultIndex] := AActualDataElement;
      ATopLeftPixel[APixelIndex] := APreviousRow[AComponent];
      APreviousRow[AComponent] := AActualDataElement;
      APreviousPixel[APixelIndex] := AActualDataElement;
      Inc(AResultIndex);
      Inc(APixelIndex);
      if APixelIndex = BytesPerPixel then
        APixelIndex := 0;
      if ASourceIndex >= ADataLength then
        Break;
    end;
  end;
end;

{ TdxPDFTIFFPredictor }

constructor TdxPDFTIFFPredictor.Create(AFilter: TdxPDFFlateLZWDecodeFilter);
begin
  inherited Create(AFilter);
  FBitsPerComponent := AFilter.BitsPerComponent;
  FComponentCount := AFilter.Colors;
end;

function TdxPDFTIFFPredictor.ActualRowLength: Integer;
begin
  Result := RowLength;
end;

function TdxPDFTIFFPredictor.Decode(const AData: TBytes): TBytes;
var
  AActualDataElement, ADataElement: Byte;
  AComponent, ADataLength, APixelIndex, AResultIndex, ARow, ARowCount,
    ASourceIndex: Integer;
  APreviousPixel: TBytes;
begin
  if FBitsPerComponent <> 16 then
  begin
    ADataLength := Length(AData);
    ARowCount := CalculateRowCount(ADataLength);
    SetLength(Result, ADataLength);
    SetLength(APreviousPixel, BytesPerPixel);
    ASourceIndex := 0;
    AResultIndex := 0;
    for ARow := 0 to ARowCount - 1 do
    begin
      TdxPDFUtils.ClearData(APreviousPixel);
      APixelIndex := 0;
      for AComponent := 0 to RowLength - 1 do
      begin
        ADataElement := AData[ASourceIndex];
        AActualDataElement := APreviousPixel[APixelIndex] + ADataElement;
        Result[AResultIndex] := AActualDataElement;
        APreviousPixel[APixelIndex] := AActualDataElement;
        Inc(AResultIndex);
        Inc(APixelIndex);
        if APixelIndex = BytesPerPixel then
          APixelIndex := 0;
        Inc(ASourceIndex);
        if ASourceIndex >= ADataLength then
          Break;
      end;
    end;
  end
  else
    Result := Decode16bpp(AData);
end;

function TdxPDFTIFFPredictor.Decode16bpp(const AData: TBytes): TBytes;
var
  AActualValue, AComponent, AComponentIndex, ADataLength, AResultIndex: Integer;
  APreviousPixel: TIntegerDynArray;
  ARow, ARowCount, ASample, ASourceIndex: Integer;
begin
  ADataLength := Length(AData);
  ARowCount := CalculateRowCount(ADataLength);
  SetLength(Result, ADataLength);
  SetLength(APreviousPixel, FComponentCount);
  ASourceIndex := 0;
  AResultIndex := 0;
  for ARow := 0 to ARowCount - 1 do
  begin
    FillChar(APreviousPixel[0], FComponentCount * SizeOf(Integer), 0);
    AComponentIndex := 0;
    for AComponent := 0 to RowLength div 2 - 1 do
    begin
      ASample := (AData[ASourceIndex] shl 8) or AData[ASourceIndex + 1];
      Inc(ASourceIndex, 2);
      AActualValue := ASample + APreviousPixel[AComponentIndex];
      Result[AResultIndex] := Byte(AActualValue shr 8);
      Result[AResultIndex + 1] := Byte(AActualValue);
      Inc(AResultIndex, 2);
      APreviousPixel[AComponentIndex] := AActualValue;
      Inc(AComponentIndex);
      if AComponentIndex = FComponentCount then
        AComponentIndex := 0;
      if ASourceIndex >= ADataLength - 1 then
        Break;
    end;
  end;
end;

{ TdxPDFFlateLZWDecodeFilterPredictorDataSource }

constructor TdxPDFFlateLZWDecodeFilterPredictorDataSource.Create
  (AFilter: TdxPDFFlateLZWDecodeFilter; const ASource: IdxPDFFlateDataSource;
  ARowInitialOffset: Integer);
var
  ABitsFactor, ARowComponentCount: Integer;
begin
  inherited Create;
  FSource := ASource;
  FRowInitialOffset := ARowInitialOffset;
  FBitsPerComponent := AFilter.BitsPerComponent;
  FBytesPerPixel := FBitsPerComponent * AFilter.Colors div 8;
  if FBytesPerPixel = 0 then
    FBytesPerPixel := 1;
  if FBitsPerComponent = 16 then
    FRowLength := AFilter.Columns * AFilter.Colors * 2
  else
  begin
    ABitsFactor := 8 div FBitsPerComponent;
    ARowComponentCount := AFilter.Columns * AFilter.Colors;
    FRowLength := ARowComponentCount div ABitsFactor;
    if ARowComponentCount mod ABitsFactor <> 0 then
      Inc(FRowLength);
  end;
  SetLength(FPreviousPixel, FBytesPerPixel);
  SetLength(FCurrentRow, FRowLength + ARowInitialOffset);
end;

function TdxPDFFlateLZWDecodeFilterPredictorDataSource.GetRowLength: Integer;
begin
  Result := FRowLength + FRowInitialOffset;
end;

procedure TdxPDFFlateLZWDecodeFilterPredictorDataSource.FillBuffer
  (var ABuffer: TBytes);
var
  ABufferSize, ARemain: Integer;
begin
  ABufferSize := Length(ABuffer);
  if FRowOffset = 0 then
    StartNextRow
  else
  begin
    ARemain := FRowLength - FRowOffset + FRowInitialOffset;
    if ARemain >= ABufferSize then
    begin
      TdxPDFUtils.CopyData(FCurrentRow, FRowOffset, ABuffer, 0, ABufferSize);
      FRowOffset := FRowOffset + ABufferSize;
      Exit;
    end
    else
    begin
      Dec(ABufferSize, ARemain);
      TdxPDFUtils.CopyData(FCurrentRow, FRowOffset, ABuffer, 0, ARemain);
      FRowOffset := 0;
      StartNextRow;
    end;
  end;
  while ABufferSize <> 0 do
  begin
    ProcessRow;
    if ABufferSize <= FRowLength then
    begin
      TdxPDFUtils.CopyData(FCurrentRow, FRowOffset, ABuffer,
        Length(ABuffer) - ABufferSize, ABufferSize);
      FRowOffset := FRowOffset + ABufferSize;
      if ABufferSize = FRowLength then
        FRowOffset := 0;
      Exit;
    end
    else
    begin
      TdxPDFUtils.CopyData(FCurrentRow, FRowOffset, ABuffer,
        Length(ABuffer) - ABufferSize, FRowLength);
      FRowOffset := 0;
      Dec(ABufferSize, FRowLength);
      StartNextRow;
    end;
  end;
end;

procedure TdxPDFFlateLZWDecodeFilterPredictorDataSource.StartNextRow;
begin
  TdxPDFUtils.ClearData(FPreviousPixel);
  FSource.FillBuffer(FCurrentRow);
  FRowOffset := FRowInitialOffset;
end;

{ TdxPDFPNGPredictorDataSource }

constructor TdxPDFPNGPredictorDataSource.Create
  (AFilter: TdxPDFFlateLZWDecodeFilter; const ASource: IdxPDFFlateDataSource);
begin
  inherited Create(AFilter, ASource, 1);
  SetLength(FPreviousRow, RowLength);
  SetLength(FTopLeftPixel, BytesPerPixel);
end;

procedure TdxPDFPNGPredictorDataSource.StartNextRow;
begin
  FPreviousRow := FCurrentRow;
  SetLength(FCurrentRow, RowLength);
  inherited StartNextRow;
  TdxPDFUtils.ClearData(FTopLeftPixel);
  FPNGPrediction := TdxPDFPNGPrediction(FCurrentRow[0]);
end;

procedure TdxPDFPNGPredictorDataSource.ProcessRow;
var
  AActualDataElement, AChoose, ADataElement, ALeft, ATop, ATopLeft: Byte;
  AComponent, ADLeft, ADTop, ADTopLeft, APixelIndex, APrediction: Integer;
begin
  APixelIndex := 0;
  for AComponent := 1 to RowLength - 1 do
  begin
    ADataElement := FCurrentRow[AComponent];
    case FPNGPrediction of
      ppNone:
        AActualDataElement := ADataElement;
      ppSub:
        AActualDataElement := PreviousPixel[APixelIndex] + ADataElement;
      ppUp:
        AActualDataElement := FPreviousRow[AComponent] + ADataElement;
      ppAverage:
        AActualDataElement :=
          (PreviousPixel[APixelIndex] + FPreviousRow[AComponent]) div 2 +
          ADataElement;
      ppPaeth:
        begin
          ALeft := PreviousPixel[APixelIndex];
          ATop := FPreviousRow[AComponent];
          ATopLeft := FTopLeftPixel[APixelIndex];
          APrediction := ALeft + ATop - ATopLeft;
          ADLeft := Abs(APrediction - ALeft);
          ADTop := Abs(APrediction - ATop);
          ADTopLeft := Abs(APrediction - ATopLeft);
          if ADLeft <= ADTop then
            AChoose := IfThen(ADLeft <= ADTopLeft, ALeft, ATopLeft)
          else
            AChoose := IfThen(ADTop <= ADTopLeft, ATop, ATopLeft);
          AActualDataElement := AChoose + ADataElement;
        end;
    else
      TdxPDFUtils.Abort;
      AActualDataElement := ADataElement;
    end;
    FCurrentRow[AComponent] := AActualDataElement;
    FTopLeftPixel[APixelIndex] := FPreviousRow[AComponent];
    FPreviousRow[AComponent] := AActualDataElement;
    PreviousPixel[APixelIndex] := AActualDataElement;
    Inc(APixelIndex);
    if APixelIndex = BytesPerPixel then
      APixelIndex := 0;
  end;
end;

{ TdxPDFTIFFPredictorDataSource }

constructor TdxPDFTIFFPredictorDataSource.Create
  (AFilter: TdxPDFFlateLZWDecodeFilter; const ASource: IdxPDFFlateDataSource);
begin
  inherited Create(AFilter, ASource, 0);
end;

procedure TdxPDFTIFFPredictorDataSource.ProcessRow;
var
  AComponent, APixelIndex: Integer;
  AActualDataElement: Byte;
begin
  if BitsPerComponent <> 16 then
  begin
    APixelIndex := 0;
    for AComponent := 0 to RowLength - 1 do
    begin
      AActualDataElement := PreviousPixel[APixelIndex] + FCurrentRow
        [AComponent];
      FCurrentRow[AComponent] := AActualDataElement;
      PreviousPixel[APixelIndex] := AActualDataElement;
      Inc(APixelIndex);
      if APixelIndex = BytesPerPixel then
        APixelIndex := 0;
    end;
  end
  else
    Process16bppRow;
end;

procedure TdxPDFTIFFPredictorDataSource.Process16bppRow;
var
  AActualValue, AComponent, ADataIndex, APixelIndex: Integer;
  H, L: Byte;
begin
  APixelIndex := 0;
  ADataIndex := 0;
  for AComponent := 0 to RowLength div 2 - 1 do
  begin
    AActualValue := (FCurrentRow[ADataIndex] shl 8 or
      FCurrentRow[ADataIndex + 1]) + (PreviousPixel[APixelIndex] shl 8 or
      PreviousPixel[APixelIndex + 1]);
    H := AActualValue shr 8;
    L := AActualValue;
    FCurrentRow[ADataIndex] := H;
    FCurrentRow[ADataIndex + 1] := L;
    PreviousPixel[APixelIndex] := H;
    PreviousPixel[APixelIndex + 1] := L;
    Inc(ADataIndex, 2);
    Inc(APixelIndex, 2);
    if APixelIndex = BytesPerPixel then
      APixelIndex := 0;
  end;
end;

{ TdxPDFFlateEncoder }

class function TdxPDFFlateEncoder.Encode(const AData: TBytes): TBytes;
var
  ACompressionStream: TCompressionStream;
  AOutput: TBytesStream;
begin
  AOutput := TBytesStream.Create;
  try
    ACompressionStream := TCompressionStream.Create(AOutput);
    try
      ACompressionStream.WriteBuffer(AData[0], Length(AData));
    finally
      ACompressionStream.Free;
    end;
    SetLength(Result, AOutput.Size);
    AOutput.Position := 0;
    AOutput.ReadBuffer(Result[0], AOutput.Size);
  finally
    AOutput.Free;
  end;
end;

end.
