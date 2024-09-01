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

unit dxPDFStreamFilter;

{$I cxVer.inc}
{$IFNDEF DELPHIXE3}
{$M-}
{$ENDIF}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, dxPDFBase, dxPDFTypes,
  dxJBIG2, dxPDFImageUtils;

type
  { TdxPDFCustomStreamFilter }

  TdxPDFCustomStreamFilterClass = class of TdxPDFCustomStreamFilter;

  TdxPDFCustomStreamFilter = class(TdxPDFReferencedObject)
  strict private
    FEODToken: TdxPDFTokenDescription;
  protected
    function CreateEODToken: TdxPDFTokenDescription; virtual;
    function DoDecode(const AData: TBytes): TBytes; virtual; abstract;
    procedure Initialize(ADecodeParameters: TdxPDFDictionary); virtual;
  public
    constructor Create(ADecodeParameters: TdxPDFDictionary);
    destructor Destroy; override;
    class function GetName: string; virtual;
    class function GetShortName: string; virtual;

    function CreateScanlineSource(const AImage: IdxPDFDocumentImage;
      AComponentCount: Integer; const AData: TBytes)
      : TdxPDFScanlineTransformationResult; virtual;
    function Decode(const AData: TBytes): TBytes;
    function EODToken: TdxPDFTokenDescription;
    procedure Write(AParameters: TdxPDFDictionary); virtual;
  end;

  { TdxPDFStreamFilters }

  TdxPDFStreamFilters = class(TdxPDFObjectList<TdxPDFCustomStreamFilter>);

  { TdxPDFStreamFilterFactory }

  TdxPDFStreamFilterFactory = class
  strict private
    class var FFactory: TdxPDFFactory<TdxPDFCustomStreamFilterClass>;

    class procedure CheckInitialized;
    class procedure RegisterFilter(AFilterClass: TdxPDFCustomStreamFilterClass);
  protected
    class procedure Finalize;
  public
    class function Create(const AFilterName: string; AParameters: TObject)
      : TdxPDFCustomStreamFilter;
  end;

  { TdxPDFUnknownStreamFilter }

  TdxPDFUnknownStreamFilter = class(TdxPDFCustomStreamFilter)
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFASCIIHexDecodeFilter }

  TdxPDFASCIIHexDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
  const
    FZero = Byte('0');
    FOne = Byte('1');
    FTwo = Byte('2');
    FThree = Byte('3');
    FFour = Byte('4');
    FFive = Byte('5');
    FSix = Byte('6');
    FSeven = Byte('7');
    FEight = Byte('8');
    FNine = Byte('9');
    FA = Byte('a');
    FB = Byte('b');
    FC = Byte('c');
    FD = Byte('d');
    FE = Byte('e');
    FF = Byte('f');
    FCapitalA = Byte('A');
    FCapitalB = Byte('B');
    FCapitalC = Byte('C');
    FCapitalD = Byte('D');
    FCapitalE = Byte('E');
    FCapitalF = Byte('F');
  protected
    function CreateEODToken: TdxPDFTokenDescription; override;
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFDCTDecodeFilter }

  TdxPDFDCTDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FColorTransform: Boolean;
    function RemoveLeadingSpaces(const AData: TBytes): TBytes;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TdxPDFDictionary); override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;

    function CreateScanlineSource(const AImage: IdxPDFDocumentImage;
      AComponentCount: Integer; const AData: TBytes)
      : TdxPDFScanlineTransformationResult; override;
    procedure Write(AParameters: TdxPDFDictionary); override;

    property ColorTransform: Boolean read FColorTransform;
  end;

  { TdxPDFJPXDecodeFilter }

  TdxPDFJPXDecodeFilter = class(TdxPDFCustomStreamFilter)
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    function CreateScanlineSource(const AImage: IdxPDFDocumentImage;
      AComponentCount: Integer; const AData: TBytes)
      : TdxPDFScanlineTransformationResult; override;
  end;

  { TdxPDFASCII85DecodeFilter }

  TdxPDFASCII85DecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FGroup: TBytes;
    FGroupSize: Integer;
    FIndex: Integer;
    FMaxValue: Int64;
    FMultiplier1: Int64;
    FMultiplier2: Int64;
    FMultiplier3: Int64;
    FMultiplier4: Int64;
    function CheckEncodedData(AData: Byte): Boolean;
    function ConvertCurrentGroup: Int64;
    procedure DecodeGroup(AGroupSize: Integer; var AResult: TBytes);
    procedure ProcessFinalGroup(var AResult: TBytes);
    procedure ProcessGroup(AData: Byte; var AResult: TBytes);
  protected
    function CreateEODToken: TdxPDFTokenDescription; override;
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TdxPDFDictionary); override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFRunLengthDecodeFilter }

  TdxPDFRunLengthDecodeFilter = class(TdxPDFCustomStreamFilter)
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  TdxPDFCCITTFaxEncodingScheme = (fesTwoDimensional, fesOneDimensional,
    fesMixed);

  { TdxPDFCCITTFaxDecodeFilter }

  TdxPDFCCITTFaxDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FBlackIs1: Boolean;
    FColumns: Integer;
    FDamagedRowsBeforeError: Integer;
    FEncodedByteAlign: Boolean;
    FEncodingScheme: TdxPDFCCITTFaxEncodingScheme;
    FEndOfBlock: Boolean;
    FEndOfLine: Boolean;
    FRows: Integer;
    FTwoDimensionalLineCount: Integer;
    function PerformDecode(const AData: TBytes): TBytes;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(AParameters: TdxPDFDictionary); override;

    property BlackIs1: Boolean read FBlackIs1;
    property Columns: Integer read FColumns;
    property DamagedRowsBeforeError: Integer read FDamagedRowsBeforeError;
    property EncodedByteAlign: Boolean read FEncodedByteAlign;
    property EncodingScheme: TdxPDFCCITTFaxEncodingScheme read FEncodingScheme;
    property EndOfBlock: Boolean read FEndOfBlock;
    property EndOfLine: Boolean read FEndOfLine;
    property Rows: Integer read FRows;
    property TwoDimensionalLineCount: Integer read FTwoDimensionalLineCount;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    procedure Write(AParameters: TdxPDFDictionary); override;
  end;

  { TdxJBIG2GlobalSegments }

  TdxJBIG2GlobalSegments = class(TdxPDFReferencedObject)
  strict private
    FData: TBytes;
  public
    constructor Create(const AData: TBytes);

    class function Parse(ARepository: TdxPDFCustomRepository;
      AValue: TdxPDFBase): TdxJBIG2GlobalSegments; static;

    property Data: TBytes read FData;
  end;

  { TdxPDFJBIG2DecodeFilter }

  TdxPDFJBIG2DecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FGlobalSegments: TdxJBIG2GlobalSegments;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TdxPDFDictionary); override;

    property GlobalSegments: TdxJBIG2GlobalSegments read FGlobalSegments;
  public
    destructor Destroy; override;
    class function GetName: string; override;
    class function GetShortName: string; override;
    procedure Write(AParameters: TdxPDFDictionary); override;
  end;

function dxPDFCreateFilterList(ADictionary: TdxPDFDictionary)
  : TdxPDFStreamFilters;

implementation

uses
  Math, Classes, Character, ZLib, dxCore, dxGDIPlusClasses, dxGDIPlusAPI, dxJPX,
  dxPDFCore, dxPDFUtils,
  dxPDFDCTDecoder, dxPDFFlateLZW;

type
  EdxPDFCCITTFaxDecodeException = class(EdxPDFAbortException);

  { TdxPDFHuffmanTreeNode }

  TdxPDFHuffmanTreeNode = class(TObject);

  { TdxPDFHuffmanTreeLeaf }

  TdxPDFHuffmanTreeLeaf = class(TdxPDFHuffmanTreeNode)
  strict private
    FRunLength: Integer;
  public
    constructor Create(ARunLength: Integer);

    property RunLength: Integer read FRunLength;
  end;

  { TdxPDFHuffmanTreeBranch }

  TdxPDFHuffmanTreeBranch = class(TdxPDFHuffmanTreeNode)
  strict private
    FZero: TdxPDFHuffmanTreeNode;
    FOne: TdxPDFHuffmanTreeNode;
  public
    destructor Destroy; override;

    procedure Fill(const ASequence: string; ARunLength: Integer); overload;
    procedure Fill(ADictionary: TdxPDFStringIntegerDictionary); overload;

    property Zero: TdxPDFHuffmanTreeNode read FZero;
    property One: TdxPDFHuffmanTreeNode read FOne;
  end;

  TdxPDFCCITTFaxCodingMode = (fcmPass, fcmHorizontal, fcmVertical0,
    fcmVerticalRight1, fcmVerticalRight2, fcmVerticalRight3, fcmVerticalLeft1,
    fcmVerticalLeft2, fcmVerticalLeft3, fcmEndOfData);

  { TdxPDFCCITTFaxDecoder }

  TdxPDFCCITTFaxDecoder = class
  strict private
    FBlackRunLengths: TdxPDFStringIntegerDictionary;
    FBlackTree: TdxPDFHuffmanTreeBranch;
    FCommonRunLengths: TdxPDFStringIntegerDictionary;
    FWhiteRunLengths: TdxPDFStringIntegerDictionary;
    FWhiteTree: TdxPDFHuffmanTreeBranch;

    FAlignEncodedBytes: Boolean;
    FBlackIs1: Boolean;
    FColumns: Integer;
    FCurrentByte: Byte;
    FCurrentByteOffset: Integer;
    FCurrentPosition: Integer;
    FData: TBytes;
    FDecodingLine: TBytes;
    FIsBlack: Boolean;
    FLength: Integer;
    FLineSize: Integer;
    FReferenceLine: TBytes;
    FResult: TBytes;
    FSize: Integer;
    FTwoDimensionalLineCount: Integer;

    FA0: Integer;
    FA1: Integer;
    FB1: Integer;
    FB2: Integer;

    function IsCompleted: Boolean;
    function IsEOF: Boolean;
    function DecodeGroup3Line: Boolean;
    function DecodeGroup4: Integer;
    function FillByte(B: Byte; AStart, AEnd: Integer; ABlack: Boolean): Byte;
    function FillDecodingLine(AA0, AA1: Integer; ABlack: Boolean): Boolean;
    function Finish: Boolean;
    function FindRunningLengthPart(ABranch: TdxPDFHuffmanTreeBranch): Integer;
    function FindRunningLength(ABranch: TdxPDFHuffmanTreeBranch): Integer;
    function FindB(AStartPosition: Integer; AIsWhite: Boolean): Integer;
    function ReadBit: Boolean; inline;
    function ReadMode: TdxPDFCCITTFaxCodingMode; inline;
    procedure AccumulateResult;
    procedure DecodeGroup3;
    procedure DecodeGroup3TwoDimensional;
    procedure InitializeLengths;
    procedure InitializeBlackRunLengths;
    procedure InitializeCommonRunLengths;
    procedure InitializeWhiteRunLengths;
    procedure NextLine; inline;
    procedure MoveNextByte;
    procedure ReadEOL; inline;
  public
    constructor Create;
    destructor Destroy; override;

    function Decode(AFilter: TdxPDFCCITTFaxDecodeFilter;
      const AData: TBytes): TBytes;
  end;

function dxPDFCreateFilterList(ADictionary: TdxPDFDictionary)
  : TdxPDFStreamFilters;

  function GetDecodeParametersObject: TdxPDFBase;
  begin
    Result := ADictionary.GetObject(TdxPDFKeywords.DecodeParameters);
    if (Result = nil) or (Result.ObjectType = otDictionary) then
      Result := nil;
  end;

  function GetDecodeParameters(AObject: TdxPDFBase; AFilterIndex: Integer)
    : TdxPDFBase;
  begin
    if (AObject <> nil) and (AObject.ObjectType = otArray) and
      (TdxPDFArray(AObject).Count > 0) then
    begin
      Result := TdxPDFArray(AObject)
        [Min(AFilterIndex, TdxPDFArray(AObject).Count - 1)];
      Result := (ADictionary as TdxPDFReaderDictionary)
        .Repository.ResolveReference(Result);
    end
    else
      Result := nil;
  end;

  procedure AddFilter(AName: TdxPDFBase; AParameters: TObject;
    AFilters: TdxPDFStreamFilters);
  begin
    AFilters.Add(TdxPDFStreamFilterFactory.Create(TdxPDFName(AName).Value,
      AParameters));
  end;

var
  AFilter: TdxPDFBase;
  AParameters: TdxPDFBase;
  I: Integer;
begin
  Result := TdxPDFStreamFilters.Create;
  if ADictionary <> nil then
  begin
    AFilter := ADictionary.GetObject(TdxPDFKeywords.Filter);
    if AFilter <> nil then
      case AFilter.ObjectType of
        otName, otString:
          AddFilter(AFilter,
            ADictionary.GetObject(TdxPDFKeywords.DecodeParameters), Result);

        otArray:
          begin
            AParameters := GetDecodeParametersObject;
            for I := 0 to TdxPDFArray(AFilter).Count - 1 do
              AddFilter(TdxPDFArray(AFilter)[I],
                GetDecodeParameters(AParameters, I), Result);
          end;
      end;
  end;
end;

{ TdxPDFHuffmanTreeLeaf }

constructor TdxPDFHuffmanTreeLeaf.Create(ARunLength: Integer);
begin
  inherited Create;
  FRunLength := ARunLength;
end;

{ TdxPDFHuffmanTreeBranch }

destructor TdxPDFHuffmanTreeBranch.Destroy;
begin
  FreeAndNil(FOne);
  FreeAndNil(FZero);
  inherited Destroy;
end;

procedure TdxPDFHuffmanTreeBranch.Fill(const ASequence: string;
  ARunLength: Integer);
var
  AIsOne: Boolean;
  ANewSequence: string;
  ABranch: TdxPDFHuffmanTreeBranch;
begin
  ABranch := nil;
  AIsOne := False;
  if ASequence = '' then
    TdxPDFUtils.Abort;
  case ASequence[1] of
    '0':
      AIsOne := False;
    '1':
      AIsOne := True;
  else
    TdxPDFUtils.Abort;
  end;
  ANewSequence := Copy(ASequence, 2, MaxInt);

  if ANewSequence = '' then
    if AIsOne then
    begin
      if FOne <> nil then
        TdxPDFUtils.Abort;
      FOne := TdxPDFHuffmanTreeLeaf.Create(ARunLength);
    end
    else
    begin
      if FZero <> nil then
        TdxPDFUtils.Abort;
      FZero := TdxPDFHuffmanTreeLeaf.Create(ARunLength);
    end
  else
  begin
    if AIsOne then
      if FOne = nil then
      begin
        ABranch := TdxPDFHuffmanTreeBranch.Create;
        FOne := ABranch;
      end
      else
      begin
        ABranch := FOne as TdxPDFHuffmanTreeBranch;
        if ABranch = nil then
          TdxPDFUtils.Abort;
      end
    else if FZero = nil then
    begin
      if ABranch <> nil then
        ABranch.Free;
      ABranch := TdxPDFHuffmanTreeBranch.Create;
      FZero := ABranch;
    end
    else
    begin
      ABranch := FZero as TdxPDFHuffmanTreeBranch;
      if ABranch = nil then
        TdxPDFUtils.Abort;
    end;
    ABranch.Fill(ANewSequence, ARunLength);
  end;
end;

procedure TdxPDFHuffmanTreeBranch.Fill(ADictionary
  : TdxPDFStringIntegerDictionary);
var
  APair: TPair<string, Integer>;
begin
  for APair in ADictionary do
    Fill(APair.Key, APair.Value);
end;

{ TdxPDFCCITTFaxDecoder }

constructor TdxPDFCCITTFaxDecoder.Create;
begin
  inherited Create;
  FCurrentByteOffset := 7;
end;

destructor TdxPDFCCITTFaxDecoder.Destroy;
begin
  SetLength(FResult, 0);
  FreeAndNil(FBlackTree);
  FreeAndNil(FWhiteTree);
  FreeAndNil(FCommonRunLengths);
  FreeAndNil(FBlackRunLengths);
  FreeAndNil(FWhiteRunLengths);
  inherited Destroy;
end;

function TdxPDFCCITTFaxDecoder.Decode(AFilter: TdxPDFCCITTFaxDecodeFilter;
  const AData: TBytes): TBytes;
begin
  FData := AData;
  FLength := Length(AData);
  FBlackIs1 := AFilter.BlackIs1;
  FAlignEncodedBytes := AFilter.EncodedByteAlign;
  FTwoDimensionalLineCount := AFilter.TwoDimensionalLineCount;
  FColumns := AFilter.Columns;

  FSize := FColumns div 8;
  if FColumns mod 8 <> 0 then
    Inc(FSize);
  FSize := FSize * AFilter.Rows;

  SetLength(FResult, 0);

  FLineSize := FColumns div 8;
  if FColumns mod 8 > 0 then
    Inc(FLineSize);
  SetLength(FReferenceLine, FLineSize);
  SetLength(FDecodingLine, FLineSize);
  if FLength > 0 then
    FCurrentByte := AData[0];
  FB1 := FColumns;
  FB2 := FColumns;

  InitializeLengths;

  FWhiteTree := TdxPDFHuffmanTreeBranch.Create;
  FWhiteTree.Fill(FWhiteRunLengths);
  FWhiteTree.Fill(FCommonRunLengths);

  FBlackTree := TdxPDFHuffmanTreeBranch.Create;
  FBlackTree.Fill(FBlackRunLengths);
  FBlackTree.Fill(FCommonRunLengths);

  if FLength > 0 then
    case AFilter.EncodingScheme of
      fesTwoDimensional:
        DecodeGroup4;
      fesOneDimensional:
        DecodeGroup3;
    else
      DecodeGroup3TwoDimensional;
    end;
  SetLength(Result, Length(FResult));
  TdxPDFUtils.CopyData(FResult, 0, Result, 0, Length(FResult));
  FResult := TdxByteArray.Resize(FResult, 0);
end;

function TdxPDFCCITTFaxDecoder.IsEOF: Boolean;
begin
  Result := FCurrentPosition >= FLength;
end;

function TdxPDFCCITTFaxDecoder.FillByte(B: Byte; AStart, AEnd: Integer;
  ABlack: Boolean): Byte;
var
  AMask: Byte;
begin
  AMask := ($FF shr AStart) and ($FF shl (8 - AEnd));
  if ABlack then
    Result := B and ($FF xor AMask)
  else
    Result := B or AMask;
end;

function TdxPDFCCITTFaxDecoder.FillDecodingLine(AA0, AA1: Integer;
  ABlack: Boolean): Boolean;
var
  AStartByteIndex, AEndByteIndex, I: Integer;
begin
  Result := True;
  if (AA0 <> 0) or (AA1 <> 0) then
  begin
    if (AA1 <= AA0) or (AA1 > FColumns) then
      raise EdxPDFCCITTFaxDecodeException.Create('');
    AStartByteIndex := AA0 div 8;
    AEndByteIndex := AA1 div 8;
    if AStartByteIndex = AEndByteIndex then
      FDecodingLine[AStartByteIndex] := FillByte(FDecodingLine[AStartByteIndex],
        AA0 mod 8, AA1 mod 8, ABlack)
    else
    begin
      FDecodingLine[AStartByteIndex] := FillByte(FDecodingLine[AStartByteIndex],
        AA0 mod 8, 8, ABlack);
      for I := AStartByteIndex + 1 to AEndByteIndex - 1 do
        if ABlack then
          FDecodingLine[I] := 0
        else
          FDecodingLine[I] := $FF;
      if AEndByteIndex < FLineSize then
        FDecodingLine[AEndByteIndex] := FillByte(FDecodingLine[AEndByteIndex],
          0, AA1 mod 8, ABlack);
    end;
  end;
end;

function TdxPDFCCITTFaxDecoder.Finish: Boolean;
begin
  AccumulateResult;
  FA0 := 0;
  FIsBlack := False;
  Result := not IsCompleted;
end;

function TdxPDFCCITTFaxDecoder.IsCompleted: Boolean;
begin
  if FSize = 0 then
    Result := IsEOF
  else
    Result := Length(FResult) >= FSize;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeLengths;
begin
  InitializeWhiteRunLengths;
  InitializeBlackRunLengths;
  InitializeCommonRunLengths;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeBlackRunLengths;
begin
  FBlackRunLengths := TdxPDFStringIntegerDictionary.Create;
  FBlackRunLengths.Add('0000110111', 0);
  FBlackRunLengths.Add('010', 1);
  FBlackRunLengths.Add('11', 2);
  FBlackRunLengths.Add('10', 3);
  FBlackRunLengths.Add('011', 4);
  FBlackRunLengths.Add('0011', 5);
  FBlackRunLengths.Add('0010', 6);
  FBlackRunLengths.Add('00011', 7);
  FBlackRunLengths.Add('000101', 8);
  FBlackRunLengths.Add('000100', 9);
  FBlackRunLengths.Add('0000100', 10);
  FBlackRunLengths.Add('0000101', 11);
  FBlackRunLengths.Add('0000111', 12);
  FBlackRunLengths.Add('00000100', 13);
  FBlackRunLengths.Add('00000111', 14);
  FBlackRunLengths.Add('000011000', 15);
  FBlackRunLengths.Add('0000010111', 16);
  FBlackRunLengths.Add('0000011000', 17);
  FBlackRunLengths.Add('0000001000', 18);
  FBlackRunLengths.Add('00001100111', 19);
  FBlackRunLengths.Add('00001101000', 20);
  FBlackRunLengths.Add('00001101100', 21);
  FBlackRunLengths.Add('00000110111', 22);
  FBlackRunLengths.Add('00000101000', 23);
  FBlackRunLengths.Add('00000010111', 24);
  FBlackRunLengths.Add('00000011000', 25);
  FBlackRunLengths.Add('000011001010', 26);
  FBlackRunLengths.Add('000011001011', 27);
  FBlackRunLengths.Add('000011001100', 28);
  FBlackRunLengths.Add('000011001101', 29);
  FBlackRunLengths.Add('000001101000', 30);
  FBlackRunLengths.Add('000001101001', 31);
  FBlackRunLengths.Add('000001101010', 32);
  FBlackRunLengths.Add('000001101011', 33);
  FBlackRunLengths.Add('000011010010', 34);
  FBlackRunLengths.Add('000011010011', 35);
  FBlackRunLengths.Add('000011010100', 36);
  FBlackRunLengths.Add('000011010101', 37);
  FBlackRunLengths.Add('000011010110', 38);
  FBlackRunLengths.Add('000011010111', 39);
  FBlackRunLengths.Add('000001101100', 40);
  FBlackRunLengths.Add('000001101101', 41);
  FBlackRunLengths.Add('000011011010', 42);
  FBlackRunLengths.Add('000011011011', 43);
  FBlackRunLengths.Add('000001010100', 44);
  FBlackRunLengths.Add('000001010101', 45);
  FBlackRunLengths.Add('000001010110', 46);
  FBlackRunLengths.Add('000001010111', 47);
  FBlackRunLengths.Add('000001100100', 48);
  FBlackRunLengths.Add('000001100101', 49);
  FBlackRunLengths.Add('000001010010', 50);
  FBlackRunLengths.Add('000001010011', 51);
  FBlackRunLengths.Add('000000100100', 52);
  FBlackRunLengths.Add('000000110111', 53);
  FBlackRunLengths.Add('000000111000', 54);
  FBlackRunLengths.Add('000000100111', 55);
  FBlackRunLengths.Add('000000101000', 56);
  FBlackRunLengths.Add('000001011000', 57);
  FBlackRunLengths.Add('000001011001', 58);
  FBlackRunLengths.Add('000000101011', 59);
  FBlackRunLengths.Add('000000101100', 60);
  FBlackRunLengths.Add('000001011010', 61);
  FBlackRunLengths.Add('000001100110', 62);
  FBlackRunLengths.Add('000001100111', 63);
  FBlackRunLengths.Add('0000001111', 64);
  FBlackRunLengths.Add('000011001000', 128);
  FBlackRunLengths.Add('000011001001', 192);
  FBlackRunLengths.Add('000001011011', 256);
  FBlackRunLengths.Add('000000110011', 320);
  FBlackRunLengths.Add('000000110100', 384);
  FBlackRunLengths.Add('000000110101', 448);
  FBlackRunLengths.Add('0000001101100', 512);
  FBlackRunLengths.Add('0000001101101', 576);
  FBlackRunLengths.Add('0000001001010', 640);
  FBlackRunLengths.Add('0000001001011', 704);
  FBlackRunLengths.Add('0000001001100', 768);
  FBlackRunLengths.Add('0000001001101', 832);
  FBlackRunLengths.Add('0000001110010', 896);
  FBlackRunLengths.Add('0000001110011', 960);
  FBlackRunLengths.Add('0000001110100', 1024);
  FBlackRunLengths.Add('0000001110101', 1088);
  FBlackRunLengths.Add('0000001110110', 1152);
  FBlackRunLengths.Add('0000001110111', 1216);
  FBlackRunLengths.Add('0000001010010', 1280);
  FBlackRunLengths.Add('0000001010011', 1344);
  FBlackRunLengths.Add('0000001010100', 1408);
  FBlackRunLengths.Add('0000001010101', 1472);
  FBlackRunLengths.Add('0000001011010', 1536);
  FBlackRunLengths.Add('0000001011011', 1600);
  FBlackRunLengths.Add('0000001100100', 1664);
  FBlackRunLengths.Add('0000001100101', 1728);
  FBlackRunLengths.Add('000000000001', -1);
  FBlackRunLengths.TrimExcess;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeCommonRunLengths;
begin
  FCommonRunLengths := TdxPDFStringIntegerDictionary.Create;
  FCommonRunLengths.Add('00000001000', 1792);
  FCommonRunLengths.Add('00000001100', 1856);
  FCommonRunLengths.Add('00000001101', 1920);
  FCommonRunLengths.Add('000000010010', 1984);
  FCommonRunLengths.Add('000000010011', 2048);
  FCommonRunLengths.Add('000000010100', 2112);
  FCommonRunLengths.Add('000000010101', 2176);
  FCommonRunLengths.Add('000000010110', 2240);
  FCommonRunLengths.Add('000000010111', 2304);
  FCommonRunLengths.Add('000000011100', 2368);
  FCommonRunLengths.Add('000000011101', 2432);
  FCommonRunLengths.Add('000000011110', 2496);
  FCommonRunLengths.Add('000000011111', 2560);
  FCommonRunLengths.TrimExcess;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeWhiteRunLengths;
begin
  FWhiteRunLengths := TdxPDFStringIntegerDictionary.Create;
  FWhiteRunLengths.Add('00110101', 0);
  FWhiteRunLengths.Add('000111', 1);
  FWhiteRunLengths.Add('0111', 2);
  FWhiteRunLengths.Add('1000', 3);
  FWhiteRunLengths.Add('1011', 4);
  FWhiteRunLengths.Add('1100', 5);
  FWhiteRunLengths.Add('1110', 6);
  FWhiteRunLengths.Add('1111', 7);
  FWhiteRunLengths.Add('10011', 8);
  FWhiteRunLengths.Add('10100', 9);
  FWhiteRunLengths.Add('00111', 10);
  FWhiteRunLengths.Add('01000', 11);
  FWhiteRunLengths.Add('001000', 12);
  FWhiteRunLengths.Add('000011', 13);
  FWhiteRunLengths.Add('110100', 14);
  FWhiteRunLengths.Add('110101', 15);
  FWhiteRunLengths.Add('101010', 16);
  FWhiteRunLengths.Add('101011', 17);
  FWhiteRunLengths.Add('0100111', 18);
  FWhiteRunLengths.Add('0001100', 19);
  FWhiteRunLengths.Add('0001000', 20);
  FWhiteRunLengths.Add('0010111', 21);
  FWhiteRunLengths.Add('0000011', 22);
  FWhiteRunLengths.Add('0000100', 23);
  FWhiteRunLengths.Add('0101000', 24);
  FWhiteRunLengths.Add('0101011', 25);
  FWhiteRunLengths.Add('0010011', 26);
  FWhiteRunLengths.Add('0100100', 27);
  FWhiteRunLengths.Add('0011000', 28);
  FWhiteRunLengths.Add('00000010', 29);
  FWhiteRunLengths.Add('00000011', 30);
  FWhiteRunLengths.Add('00011010', 31);
  FWhiteRunLengths.Add('00011011', 32);
  FWhiteRunLengths.Add('00010010', 33);
  FWhiteRunLengths.Add('00010011', 34);
  FWhiteRunLengths.Add('00010100', 35);
  FWhiteRunLengths.Add('00010101', 36);
  FWhiteRunLengths.Add('00010110', 37);
  FWhiteRunLengths.Add('00010111', 38);
  FWhiteRunLengths.Add('00101000', 39);
  FWhiteRunLengths.Add('00101001', 40);
  FWhiteRunLengths.Add('00101010', 41);
  FWhiteRunLengths.Add('00101011', 42);
  FWhiteRunLengths.Add('00101100', 43);
  FWhiteRunLengths.Add('00101101', 44);
  FWhiteRunLengths.Add('00000100', 45);
  FWhiteRunLengths.Add('00000101', 46);
  FWhiteRunLengths.Add('00001010', 47);
  FWhiteRunLengths.Add('00001011', 48);
  FWhiteRunLengths.Add('01010010', 49);
  FWhiteRunLengths.Add('01010011', 50);
  FWhiteRunLengths.Add('01010100', 51);
  FWhiteRunLengths.Add('01010101', 52);
  FWhiteRunLengths.Add('00100100', 53);
  FWhiteRunLengths.Add('00100101', 54);
  FWhiteRunLengths.Add('01011000', 55);
  FWhiteRunLengths.Add('01011001', 56);
  FWhiteRunLengths.Add('01011010', 57);
  FWhiteRunLengths.Add('01011011', 58);
  FWhiteRunLengths.Add('01001010', 59);
  FWhiteRunLengths.Add('01001011', 60);
  FWhiteRunLengths.Add('00110010', 61);
  FWhiteRunLengths.Add('00110011', 62);
  FWhiteRunLengths.Add('00110100', 63);
  FWhiteRunLengths.Add('11011', 64);
  FWhiteRunLengths.Add('10010', 128);
  FWhiteRunLengths.Add('010111', 192);
  FWhiteRunLengths.Add('0110111', 256);
  FWhiteRunLengths.Add('00110110', 320);
  FWhiteRunLengths.Add('00110111', 384);
  FWhiteRunLengths.Add('01100100', 448);
  FWhiteRunLengths.Add('01100101', 512);
  FWhiteRunLengths.Add('01101000', 576);
  FWhiteRunLengths.Add('01100111', 640);
  FWhiteRunLengths.Add('011001100', 704);
  FWhiteRunLengths.Add('011001101', 768);
  FWhiteRunLengths.Add('011010010', 832);
  FWhiteRunLengths.Add('011010011', 896);
  FWhiteRunLengths.Add('011010100', 960);
  FWhiteRunLengths.Add('011010101', 1024);
  FWhiteRunLengths.Add('011010110', 1088);
  FWhiteRunLengths.Add('011010111', 1152);
  FWhiteRunLengths.Add('011011000', 1216);
  FWhiteRunLengths.Add('011011001', 1280);
  FWhiteRunLengths.Add('011011010', 1344);
  FWhiteRunLengths.Add('011011011', 1408);
  FWhiteRunLengths.Add('010011000', 1472);
  FWhiteRunLengths.Add('010011001', 1536);
  FWhiteRunLengths.Add('010011010', 1600);
  FWhiteRunLengths.Add('011000', 1664);
  FWhiteRunLengths.Add('010011011', 1728);
  FWhiteRunLengths.Add('000000000001', -1);
  FWhiteRunLengths.TrimExcess;
end;

procedure TdxPDFCCITTFaxDecoder.MoveNextByte;
begin
  Inc(FCurrentPosition);
  if FCurrentPosition < FLength then
    FCurrentByte := FData[FCurrentPosition];
  FCurrentByteOffset := 7;
end;

function TdxPDFCCITTFaxDecoder.ReadBit: Boolean;
begin
  Result := IsEOF;
  if not Result then
  begin
    Result := ((FCurrentByte shr FCurrentByteOffset) and 1) = 1;
    Dec(FCurrentByteOffset);
    if FCurrentByteOffset < 0 then
      MoveNextByte;
  end;
end;

function TdxPDFCCITTFaxDecoder.ReadMode: TdxPDFCCITTFaxCodingMode;
var
  AWordLength: Integer;
begin
  AWordLength := 1;
  while not ReadBit do
  begin
    Inc(AWordLength);
    if IsEOF then
      Exit(fcmEndOfData);
  end;
  case AWordLength of
    1:
      Result := fcmVertical0;
    2:
      if ReadBit then
        Result := fcmVerticalRight1
      else
        Result := fcmVerticalLeft1;
    3:
      Result := fcmHorizontal;
    4:
      Result := fcmPass;
    5:
      if ReadBit then
        Result := fcmVerticalRight2
      else
        Result := fcmVerticalLeft2;
    6:
      if ReadBit then
        Result := fcmVerticalRight3
      else
        Result := fcmVerticalLeft3;
  else
    Result := fcmEndOfData;
  end;
end;

function TdxPDFCCITTFaxDecoder.FindRunningLengthPart
  (ABranch: TdxPDFHuffmanTreeBranch): Integer;
var
  I, J: Integer;
  ANextBit: Boolean;
  ANextNode: TdxPDFHuffmanTreeNode;
begin
  Result := -1;
  I := 0;
  try
    while FCurrentPosition < FLength do
    begin
      ANextBit := ReadBit;
      if ANextBit then
        ANextNode := ABranch.One
      else
        ANextNode := ABranch.Zero;
      if ANextNode = nil then
      begin
        if ANextBit then
          Exit(-1);
        for J := I to 9 do
          if ReadBit then
            Exit(-1);
        while not ReadBit do;
        Exit(-1);
      end;
      if ANextNode is TdxPDFHuffmanTreeLeaf then
        Exit(TdxPDFHuffmanTreeLeaf(ANextNode).RunLength)
      else if not(ANextNode is TdxPDFHuffmanTreeBranch) then
        Exit(-1)
      else
        ABranch := TdxPDFHuffmanTreeBranch(ANextNode);
      Inc(I);
    end;
  except
    Exit(-1);
  end;
end;

function TdxPDFCCITTFaxDecoder.FindRunningLength
  (ABranch: TdxPDFHuffmanTreeBranch): Integer;
var
  ACode, AResult: Integer;
begin
  ACode := FindRunningLengthPart(ABranch);
  if ACode < 64 then
    Exit(ACode);
  AResult := ACode;
  while ACode = 2560 do
  begin
    ACode := FindRunningLengthPart(ABranch);
    Inc(AResult, ACode);
  end;
  if ACode >= 64 then
  begin
    ACode := FindRunningLengthPart(ABranch);
    Inc(AResult, ACode);
  end;
  Result := AResult;
end;

function TdxPDFCCITTFaxDecoder.FindB(AStartPosition: Integer;
  AIsWhite: Boolean): Integer;
var
  AResult, ABytePosition, AByteOffset, I: Integer;
  B: Byte;
begin
  if AStartPosition = FColumns then
    Exit(FColumns);
  AResult := AStartPosition;
  ABytePosition := AStartPosition div 8;
  AByteOffset := AStartPosition mod 8;
  B := FReferenceLine[ABytePosition] shl AByteOffset;
  for I := 0 to FColumns - 1 do
  begin
    if ((B and $80) = $80) = AIsWhite then
      Exit(AResult);
    Inc(AByteOffset);
    if AByteOffset = 8 then
    begin
      Inc(ABytePosition);
      if ABytePosition = FLineSize then
        Exit(FColumns);
      B := FReferenceLine[ABytePosition];
      AByteOffset := 0;
    end
    else
      B := B shl 1;
    Inc(AResult);
  end;
  Result := FColumns;
end;

procedure TdxPDFCCITTFaxDecoder.NextLine;
var
  ATemp: TBytes;
begin
  ATemp := FReferenceLine;
  FReferenceLine := FDecodingLine;
  FDecodingLine := ATemp;
  FIsBlack := False;
  FA0 := 0;
  FB1 := FindB(0, False);
  FB2 := FindB(FB1, True);
end;

procedure TdxPDFCCITTFaxDecoder.ReadEOL;
var
  AWordLength: Integer;
begin
  AWordLength := 1;
  while not ReadBit do
    Inc(AWordLength);
  if AWordLength < 12 then
    TdxPDFUtils.Abort;
  NextLine;
end;

procedure TdxPDFCCITTFaxDecoder.AccumulateResult;
var
  I: Integer;
begin
  if FBlackIs1 then
  begin
    for I := 0 to FLineSize - 1 do
      FReferenceLine[I] := FDecodingLine[I] xor $FF;
    TdxPDFUtils.AddData(FReferenceLine, FResult);
  end
  else
    TdxPDFUtils.AddData(FDecodingLine, FResult);
  if FAlignEncodedBytes and (FCurrentByteOffset < 7) then
    MoveNextByte;
end;

function TdxPDFCCITTFaxDecoder.DecodeGroup3Line: Boolean;
var
  ATree: TdxPDFHuffmanTreeBranch;
  ARunningLength, I: Integer;
begin
  while True do
  begin
    if FIsBlack then
      ATree := FBlackTree
    else
      ATree := FWhiteTree;
    ARunningLength := FindRunningLength(ATree);
    if ARunningLength < 0 then
    begin
      ARunningLength := FindRunningLength(ATree);
      if ARunningLength < 0 then
      begin
        for I := 0 to 4 - 1 do
          if FindRunningLength(ATree) > 0 then
            TdxPDFUtils.Abort;
        Exit(False);
      end;
    end;
    FA1 := FA0 + ARunningLength;
    Result := FillDecodingLine(FA0, FA1, FIsBlack);
    FA0 := FA1;
    FIsBlack := not FIsBlack;
    if not Result then
      Exit(Finish)
    else if FA0 = FColumns then
      Exit(Finish);
  end;
end;

function TdxPDFCCITTFaxDecoder.DecodeGroup4: Integer;
var
  AA2, ALinesCount, AStartingLength, ATerminatingLength: Integer;
  AMode: TdxPDFCCITTFaxCodingMode;
  AStartingTree, ATerminatingTree: TdxPDFHuffmanTreeBranch;
begin
  ALinesCount := 0;

  while True do
  begin
    AMode := ReadMode;
    case AMode of
      fcmEndOfData:
        if FA0 <> 0 then
          TdxPDFUtils.Abort
        else
          Exit(ALinesCount);
      fcmPass:
        begin
          if not FillDecodingLine(FA0, FB2, FIsBlack) then
            TdxPDFUtils.Abort;
          FIsBlack := not FIsBlack;
          FA0 := FB2;
        end;
      fcmHorizontal:
        begin
          if FIsBlack then
          begin
            AStartingTree := FBlackTree;
            ATerminatingTree := FWhiteTree;
          end
          else
          begin
            AStartingTree := FWhiteTree;
            ATerminatingTree := FBlackTree;
          end;
          AStartingLength := FindRunningLength(AStartingTree);
          if AStartingLength < 0 then
            TdxPDFUtils.Abort;
          ATerminatingLength := FindRunningLength(ATerminatingTree);
          if ATerminatingLength < 0 then
            TdxPDFUtils.Abort;
          FA1 := FA0 + AStartingLength;
          AA2 := FA1 + ATerminatingLength;
          if AStartingLength > 0 then
            if not FillDecodingLine(FA0, FA1, FIsBlack) then
              TdxPDFUtils.Abort;
          if ATerminatingLength > 0 then
            if not FillDecodingLine(FA1, AA2, not FIsBlack) then
              TdxPDFUtils.Abort;
          FIsBlack := not FIsBlack;
          FA0 := AA2;
        end;
    else
      case AMode of
        fcmVertical0:
          FA1 := FB1;
        fcmVerticalRight1:
          FA1 := FB1 + 1;
        fcmVerticalRight2:
          FA1 := FB1 + 2;
        fcmVerticalRight3:
          FA1 := FB1 + 3;
        fcmVerticalLeft1:
          FA1 := FB1 - 1;
        fcmVerticalLeft2:
          FA1 := FB1 - 2;
        fcmVerticalLeft3:
          FA1 := FB1 - 3;
      end;
      if not FillDecodingLine(FA0, FA1, FIsBlack) then
        TdxPDFUtils.Abort;
      FA0 := FA1;
    end;
    if FA0 = FColumns then
    begin
      AccumulateResult;
      Inc(ALinesCount);
      if IsCompleted and (FCurrentPosition >= FLength - 1) then
        Exit(ALinesCount);
      NextLine;
    end
    else
    begin
      FIsBlack := not FIsBlack;
      FB1 := FindB(FindB(FA0, not FIsBlack), FIsBlack);
      FB2 := FindB(FB1, not FIsBlack);
    end;
  end;
end;

procedure TdxPDFCCITTFaxDecoder.DecodeGroup3;
begin
  while DecodeGroup3Line do;
end;

procedure TdxPDFCCITTFaxDecoder.DecodeGroup3TwoDimensional;
var
  ARemainTwoDimensionalLineCount: Integer;
begin
  ReadEOL;
  if not ReadBit then
    TdxPDFUtils.Abort;
  if DecodeGroup3Line then
  begin
    ReadEOL;
    ARemainTwoDimensionalLineCount := FTwoDimensionalLineCount;
    while not IsCompleted do
      if ReadBit then
      begin
        if ARemainTwoDimensionalLineCount > 0 then
          TdxPDFUtils.Abort;
        if not DecodeGroup3Line then
          Break;
        ARemainTwoDimensionalLineCount := FTwoDimensionalLineCount;
        ReadEOL;
      end
      else
        ARemainTwoDimensionalLineCount :=
          Max(0, ARemainTwoDimensionalLineCount - DecodeGroup4);
    if ARemainTwoDimensionalLineCount > 0 then
      TdxPDFUtils.Abort;
  end;
end;

{ TdxPDFCustomStreamFilter }

constructor TdxPDFCustomStreamFilter.Create(ADecodeParameters
  : TdxPDFDictionary);
begin
  inherited Create;
  Initialize(ADecodeParameters);
end;

destructor TdxPDFCustomStreamFilter.Destroy;
begin
  FreeAndNil(FEODToken);
  inherited Destroy;
end;

class function TdxPDFCustomStreamFilter.GetName: string;
begin
  Result := '';
end;

class function TdxPDFCustomStreamFilter.GetShortName: string;
begin
  Result := '';
end;

function TdxPDFCustomStreamFilter.Decode(const AData: TBytes): TBytes;
begin
  try
    Result := DoDecode(AData);
  except
    on E: EdxPDFAbortException do
      SetLength(Result, 0)
    else
      raise;
  end;
end;

function TdxPDFCustomStreamFilter.EODToken: TdxPDFTokenDescription;
begin
  Result := FEODToken;
end;

procedure TdxPDFCustomStreamFilter.Write(AParameters: TdxPDFDictionary);
begin
  // do nothing
end;

function TdxPDFCustomStreamFilter.CreateScanlineSource
  (const AImage: IdxPDFDocumentImage; AComponentCount: Integer;
  const AData: TBytes): TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create
    (TdxPDFCommonImageScanlineSource.CreateImageScanlineSource(Decode(AData),
    AImage, AComponentCount));
end;

function TdxPDFCustomStreamFilter.CreateEODToken: TdxPDFTokenDescription;
begin
  Result := nil
end;

procedure TdxPDFCustomStreamFilter.Initialize(ADecodeParameters
  : TdxPDFDictionary);
begin
  FEODToken := CreateEODToken;
end;

{ TdxPDFUnknownStreamFilter }

class function TdxPDFUnknownStreamFilter.GetName: string;
begin
  Result := '';
end;

class function TdxPDFUnknownStreamFilter.GetShortName: string;
begin
  Result := '_';
end;

function TdxPDFUnknownStreamFilter.DoDecode(const AData: TBytes): TBytes;
begin
  Result := AData;
end;

{ TdxPDFASCIIHexDecodeFilter }

class function TdxPDFASCIIHexDecodeFilter.GetName: string;
begin
  Result := 'ASCIIHexDecode';
end;

class function TdxPDFASCIIHexDecodeFilter.GetShortName: string;
begin
  Result := 'AHx';
end;

function TdxPDFASCIIHexDecodeFilter.CreateEODToken: TdxPDFTokenDescription;
var
  ASequence: TBytes;
begin
  SetLength(ASequence, 1);
  ASequence[0] := $3E;
  Result := TdxPDFTokenDescription.Create(ASequence);
end;

function TdxPDFASCIIHexDecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  AHigh: Boolean;
  ADecoded, AElement, ADigit: Byte;
begin
  SetLength(Result, 0);
  AHigh := True;
  ADecoded := 0;
  ADigit := 0;
  for AElement in AData do
  begin
    if not TdxPDFUtils.IsWhiteSpace(AElement) then
    begin
      case AElement of
        FZero, FOne, FTwo, FThree, FFour, FFive, FSix, FSeven, FEight, FNine:
          ADigit := AElement - FZero;
        FA, FB, FC, FD, FE, FF:
          ADigit := AElement - FA + TdxPDFDefinedSymbols.LineFeed;
        FCapitalA, FCapitalB, FCapitalC, FCapitalD, FCapitalE, FCapitalF:
          ADigit := AElement - FCapitalA + TdxPDFDefinedSymbols.LineFeed;
        TdxPDFDefinedSymbols.EndObject:
          if not AHigh then
            TdxPDFUtils.AddByte(ADecoded, Result)
          else
            Break;
      else
        TdxPDFUtils.RaiseTestException;
        Break;
      end;

      if AHigh then
        ADecoded := ADigit shl 4
      else
        TdxPDFUtils.AddByte(ADecoded + ADigit, Result);
      AHigh := not AHigh;
    end;
  end;
end;

{ TdxPDFDCTDecodeFilter }

function TdxPDFDCTDecodeFilter.CreateScanlineSource(const AImage
  : IdxPDFDocumentImage; AComponentCount: Integer; const AData: TBytes)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Create
    (TdxPDFDCTDecoder.CreateScanlineSource(RemoveLeadingSpaces(AData), AImage,
    AComponentCount));
end;

class function TdxPDFDCTDecodeFilter.GetName: string;
begin
  Result := 'DCTDecode';
end;

class function TdxPDFDCTDecodeFilter.GetShortName: string;
begin
  Result := 'DCT';
end;

function TdxPDFDCTDecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  Result := AData;
end;

procedure TdxPDFDCTDecodeFilter.Initialize(ADecodeParameters: TdxPDFDictionary);
var
  AColorTransformValue: Integer;
begin
  inherited Initialize(ADecodeParameters);

  if ADecodeParameters <> nil then
  begin
    AColorTransformValue := ADecodeParameters.GetInteger
      (TdxPDFKeywords.ColorTransform);
    if TdxPDFUtils.IsIntegerValid(AColorTransformValue) then
      case AColorTransformValue of
        0:
          FColorTransform := False;
        1:
          FColorTransform := True;
      else
        TdxPDFUtils.Abort;
      end;
  end;
end;

function TdxPDFDCTDecodeFilter.RemoveLeadingSpaces(const AData: TBytes): TBytes;
var
  ADataLength, I, AActualLength: Integer;
  AActualData: TBytes;
begin
  ADataLength := Length(AData);
  for I := 0 to ADataLength - 1 do
    if not TdxPDFUtils.IsSpace(AData[I]) then
    begin
      if I <> 0 then
      begin
        AActualLength := ADataLength - I;
        SetLength(AActualData, AActualLength);
        TdxPDFUtils.CopyData(AData, I, AActualData, 0, AActualLength);
        Exit(AActualData);
      end;
      Break;
    end;
  Result := AData;
end;

procedure TdxPDFDCTDecodeFilter.Write(AParameters: TdxPDFDictionary);
begin
  inherited;
  if FColorTransform then
    AParameters.Add(TdxPDFKeywords.ColorTransform, 1);
end;

{ TdxPDFJPXDecodeFilter }

class function TdxPDFJPXDecodeFilter.GetName: string;
begin
  Result := 'JPXDecode';
end;

class function TdxPDFJPXDecodeFilter.GetShortName: string;
begin
  Result := 'JPX';
end;

function TdxPDFJPXDecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  TdxPDFUtils.Abort;
end;

function TdxPDFJPXDecodeFilter.CreateScanlineSource(const AImage
  : IdxPDFDocumentImage; AComponentCount: Integer; const AData: TBytes)
  : TdxPDFScanlineTransformationResult;
var
  AActualComponentCount: Integer;
  AHasAlpha: Boolean;
  AJPXImage: TdxJPXImage;
  APixelFormat: TdxPDFPixelFormat;
begin
  try
    AJPXImage := TdxJPXImage.DecodeImage(AData);
    AActualComponentCount := Length(AJPXImage.Size.Components);
    AHasAlpha := (AActualComponentCount = 4) and AImage.HasSMaskInData;
    if AHasAlpha then
      Result := TdxPDFScanlineTransformationResult.Create
        (TdxPDFJPXImageScanlineSource.Create(AJPXImage, AActualComponentCount,
        True), pfArgb32bpp)
    else
    begin
      if AActualComponentCount = 1 then
        APixelFormat := pfGray8bit
      else
        APixelFormat := pfArgb24bpp;
      Result := TdxPDFScanlineTransformationResult.Create
        (TdxPDFJPXImageScanlineSource.Create(AJPXImage, AActualComponentCount,
        False), APixelFormat);
    end;
  except
    TdxPDFUtils.Abort;
  end;
end;

{ TdxPDFASCII85DecodeFilter }

class function TdxPDFASCII85DecodeFilter.GetName: string;
begin
  Result := 'ASCII85Decode';
end;

class function TdxPDFASCII85DecodeFilter.GetShortName: string;
begin
  Result := 'A85';
end;

function TdxPDFASCII85DecodeFilter.CreateEODToken: TdxPDFTokenDescription;
var
  ASequence: TBytes;
begin
  SetLength(ASequence, 2);
  ASequence[0] := $7E;
  ASequence[1] := $3E;
  Result := TdxPDFTokenDescription.Create(ASequence);
end;

function TdxPDFASCII85DecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  ADataLength, APosition, I: Integer;
  ANext: Byte;
begin
  Result := AData;
  ADataLength := Length(AData);
  if ADataLength > 0 then
  begin
    SetLength(FGroup, FGroupSize);
    SetLength(Result, 0);
    APosition := 0;
    while APosition < ADataLength do
    begin
      ANext := AData[APosition];
      Inc(APosition);
      if not TdxPDFUtils.IsWhiteSpace(ANext) then
        case ANext of
          Byte('~'):
            begin
              while APosition < ADataLength do
              begin
                ANext := AData[APosition];
                Inc(APosition);
                if ANext = TdxPDFDefinedSymbols.EndObject then
                  Break;
                if TdxPDFUtils.IsWhiteSpace(ANext) then
                  TdxPDFUtils.RaiseTestException;
              end;
              Break;
            end;
          Byte('z'):
            if FIndex = 0 then
              for I := 0 to 3 do
                TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.Null, Result)
            else
              TdxPDFUtils.RaiseTestException;
        else
          ProcessGroup(ANext, Result);
        end;
    end;
    ProcessFinalGroup(Result);
  end;
  SetLength(FGroup, 0);
end;

procedure TdxPDFASCII85DecodeFilter.Initialize(ADecodeParameters
  : TdxPDFDictionary);
begin
  inherited Initialize(ADecodeParameters);
  FMaxValue := (Int64(1) shl 32) - 1;
  FMultiplier1 := 85;
  FMultiplier2 := FMultiplier1 * FMultiplier1;
  FMultiplier3 := FMultiplier2 * FMultiplier1;
  FMultiplier4 := FMultiplier3 * FMultiplier1;
  FIndex := 0;
  FGroupSize := 5;
end;

function TdxPDFASCII85DecodeFilter.CheckEncodedData(AData: Byte): Boolean;
begin
  Result := (AData >= TdxPDFDefinedSymbols.ExclamationMark) and
    (AData <= Byte('u'));
end;

function TdxPDFASCII85DecodeFilter.ConvertCurrentGroup: Int64;
begin
  Result := (FGroup[0] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier4 +
    (FGroup[1] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier3 +
    (FGroup[2] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier2 +
    (FGroup[3] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier1 +
    FGroup[4] - TdxPDFDefinedSymbols.ExclamationMark;
end;

procedure TdxPDFASCII85DecodeFilter.DecodeGroup(AGroupSize: Integer;
  var AResult: TBytes);
var
  AValue: Int64;
begin
  AValue := Max(Min(ConvertCurrentGroup, FMaxValue), 0);
  TdxPDFUtils.AddByte((AValue and $FF000000) shr 24, AResult);
  Dec(AGroupSize);
  if AGroupSize > 0 then
  begin
    Dec(AGroupSize);
    TdxPDFUtils.AddByte((AValue and $FF0000) shr 16, AResult);
    if AGroupSize > 0 then
    begin
      Dec(AGroupSize);
      TdxPDFUtils.AddByte((AValue and $FF00) shr 8, AResult);
      if AGroupSize > 0 then
        TdxPDFUtils.AddByte(AValue and $FF, AResult);
    end;
  end;
  FIndex := 0;
end;

procedure TdxPDFASCII85DecodeFilter.ProcessFinalGroup(var AResult: TBytes);
var
  I: Integer;
begin
  if FIndex > 0 then
  begin
    for I := FIndex to FGroupSize - 1 do
      FGroup[I] := Byte('u');
    DecodeGroup(FIndex - 1, AResult);
  end;
end;

procedure TdxPDFASCII85DecodeFilter.ProcessGroup(AData: Byte;
  var AResult: TBytes);
begin
  if CheckEncodedData(AData) then
  begin
    FGroup[FIndex] := AData;
    Inc(FIndex);
    if FIndex = FGroupSize then
      DecodeGroup(4, AResult);
  end;
end;

{ TdxPDFRunLengthDecodeFilter }

class function TdxPDFRunLengthDecodeFilter.GetName: string;
begin
  Result := 'RunLengthDecode';
end;

class function TdxPDFRunLengthDecodeFilter.GetShortName: string;
begin
  Result := 'RL';
end;

function TdxPDFRunLengthDecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  B: Byte;
  I, ACount, AState: Integer;
begin
  SetLength(Result, 0);
  AState := 1;
  ACount := 0;
  for B in AData do
    case AState of
      1:
        if B <> 128 then
        begin
          if B <= 127 then
          begin
            AState := 2;
            ACount := B + 1;
          end
          else
          begin
            AState := 3;
            ACount := 257 - B;
          end
        end
        else
          Break;
      2:
        begin
          TdxPDFUtils.AddByte(B, Result);
          Dec(ACount);
          if ACount = 0 then
            AState := 1;
        end;
      3:
        begin
          AState := 1;
          for I := 0 to ACount - 1 do
            TdxPDFUtils.AddByte(B, Result);
        end;
    end;
end;

{ TdxPDFCCITTFaxDecodeFilter }

procedure TdxPDFCCITTFaxDecodeFilter.Write(AParameters: TdxPDFDictionary);
begin
  inherited Write(AParameters);
  if EncodingScheme <> fesOneDimensional then
    AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterEncodingScheme,
      IfThen(EncodingScheme = fesMixed, TwoDimensionalLineCount + 1, -1));
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterEndOfLine, EndOfLine);
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterEncodedByteAlign,
    EncodedByteAlign);
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterColumns, Columns);
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterRows, Rows);
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterEndOfBlock, EndOfBlock);
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterBlackIs1, BlackIs1);
  AParameters.Add(TdxPDFKeywords.CCITTFaxDecodeFilterDamagedRowsBeforeError,
    DamagedRowsBeforeError);
end;

class function TdxPDFCCITTFaxDecodeFilter.GetName: string;
begin
  Result := 'CCITTFaxDecode';
end;

class function TdxPDFCCITTFaxDecodeFilter.GetShortName: string;
begin
  Result := 'CCF';
end;

function TdxPDFCCITTFaxDecodeFilter.PerformDecode(const AData: TBytes): TBytes;
var
  ADecoder: TdxPDFCCITTFaxDecoder;
begin
  ADecoder := TdxPDFCCITTFaxDecoder.Create;
  try
    Result := ADecoder.Decode(Self, AData);
  finally
    ADecoder.Free;
  end;
end;

function TdxPDFCCITTFaxDecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  try
    Result := PerformDecode(AData);
  except
    on EdxPDFCCITTFaxDecodeException do
    begin
      FEncodedByteAlign := not EncodedByteAlign;
      Result := PerformDecode(AData);
    end
    else
      raise;
  end;
end;

procedure TdxPDFCCITTFaxDecodeFilter.Initialize(AParameters: TdxPDFDictionary);
var
  K: Integer;
  ADefaultColumns: Integer;
begin
  inherited;
  ADefaultColumns := 1728;
  FColumns := ADefaultColumns;
  FEndOfBlock := True;

  if AParameters = nil then
    FEncodingScheme := fesOneDimensional
  else
  begin
    K := AParameters.GetInteger
      (TdxPDFKeywords.CCITTFaxDecodeFilterEncodingScheme, 0);
    if K < 0 then
      FEncodingScheme := fesTwoDimensional
    else if K = 0 then
      FEncodingScheme := fesOneDimensional
    else
    begin
      FEncodingScheme := fesMixed;
      FTwoDimensionalLineCount := K - 1;
    end;
    FEndOfLine := AParameters.GetBoolean
      (TdxPDFKeywords.CCITTFaxDecodeFilterEndOfLine);
    FEncodedByteAlign := AParameters.GetBoolean
      (TdxPDFKeywords.CCITTFaxDecodeFilterEncodedByteAlign);
    FColumns := AParameters.GetInteger
      (TdxPDFKeywords.CCITTFaxDecodeFilterColumns, ADefaultColumns);
    FRows := AParameters.GetInteger(TdxPDFKeywords.CCITTFaxDecodeFilterRows, 0);
    FEndOfBlock := AParameters.GetBoolean
      (TdxPDFKeywords.CCITTFaxDecodeFilterEndOfBlock, True);
    FBlackIs1 := AParameters.GetBoolean
      (TdxPDFKeywords.CCITTFaxDecodeFilterBlackIs1);
    FDamagedRowsBeforeError := AParameters.GetInteger
      (TdxPDFKeywords.CCITTFaxDecodeFilterDamagedRowsBeforeError, 0);
    if (FColumns <= 0) or (FRows < 0) or (FDamagedRowsBeforeError < 0) then
      TdxPDFUtils.RaiseTestException;
  end;
end;

{ TdxJBIG2GlobalSegments }

constructor TdxJBIG2GlobalSegments.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
end;

class function TdxJBIG2GlobalSegments.Parse(ARepository: TdxPDFCustomRepository;
  AValue: TdxPDFBase): TdxJBIG2GlobalSegments;
begin
  case AValue.ObjectType of
    otIndirectReference:
      Result := TdxJBIG2GlobalSegments.Parse(ARepository,
        ARepository.GetStream(TdxPDFReference(AValue).Number));
    otStream:
      Result := TdxJBIG2GlobalSegments.Create(TdxPDFStream(AValue)
        .UncompressedData);
    otString:
      Result := TdxJBIG2GlobalSegments.Create
        (TdxPDFUtils.StrToByteArray(TdxPDFString(AValue).Value));
  else
    Result := nil;
    TdxPDFUtils.Abort;
  end;
end;

{ TdxPDFJBIG2DecodeFilter }

destructor TdxPDFJBIG2DecodeFilter.Destroy;
begin
  FreeAndNil(FGlobalSegments);
  inherited Destroy;
end;

class function TdxPDFJBIG2DecodeFilter.GetName: string;
begin
  Result := 'JBIG2Decode'
end;

class function TdxPDFJBIG2DecodeFilter.GetShortName: string;
begin
  Result := 'JBIG2Decode';
end;

function TdxPDFJBIG2DecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  if FGlobalSegments <> nil then
    Result := TdxJBIG2Image.Decode(AData, FGlobalSegments.Data)
  else
    Result := TdxJBIG2Image.Decode(AData, nil);
end;

procedure TdxPDFJBIG2DecodeFilter.Initialize(ADecodeParameters
  : TdxPDFDictionary);
var
  AReference: TdxPDFBase;
begin
  inherited Initialize(ADecodeParameters);

  if ADecodeParameters is TdxPDFReaderDictionary then
  begin
    AReference := TdxPDFReaderDictionary(ADecodeParameters)
      .GetObject(TdxPDFKeywords.JBIG2Globals) as TdxPDFBase;
    if (AReference <> nil) and (AReference.ObjectType <> otIndirectReference)
    then
      FGlobalSegments := TdxJBIG2GlobalSegments.Parse
        (TdxPDFReaderDictionary(ADecodeParameters).Repository, AReference);
  end;
end;

procedure TdxPDFJBIG2DecodeFilter.Write(AParameters: TdxPDFDictionary);
begin
  inherited Write(AParameters);
  if GlobalSegments <> nil then
    AParameters.AddReference(TdxPDFKeywords.JBIG2Globals, GlobalSegments.Data);
end;

{ TdxPDFStreamFilterFactory }

class function TdxPDFStreamFilterFactory.Create(const AFilterName: string;
  AParameters: TObject): TdxPDFCustomStreamFilter;
var
  AFilterClass: TdxPDFCustomStreamFilterClass;
  AFilterParams: TdxPDFDictionary;
begin
  CheckInitialized;

  if AParameters is TdxPDFDictionary then
    AFilterParams := TdxPDFDictionary(AParameters)
  else
    AFilterParams := nil;

  if FFactory.TryGetClass(AFilterName, AFilterClass) then
    Result := AFilterClass.Create(AFilterParams)
  else
    Result := TdxPDFUnknownStreamFilter.Create(AFilterParams);
end;

class procedure TdxPDFStreamFilterFactory.CheckInitialized;
begin
  if FFactory = nil then
  begin
    FFactory := TdxPDFFactory<TdxPDFCustomStreamFilterClass>.Create;
    RegisterFilter(TdxPDFFlateDecodeFilter);
    RegisterFilter(TdxPDFLZWDecodeFilter);
    RegisterFilter(TdxPDFASCIIHexDecodeFilter);
    RegisterFilter(TdxPDFASCII85DecodeFilter);
    RegisterFilter(TdxPDFRunLengthDecodeFilter);
    RegisterFilter(TdxPDFDCTDecodeFilter);
    RegisterFilter(TdxPDFCCITTFaxDecodeFilter);
    RegisterFilter(TdxPDFJBIG2DecodeFilter);
    RegisterFilter(TdxPDFJPXDecodeFilter);
  end;
end;

class procedure TdxPDFStreamFilterFactory.Finalize;
begin
  FreeAndNil(FFactory);
end;

class procedure TdxPDFStreamFilterFactory.RegisterFilter
  (AFilterClass: TdxPDFCustomStreamFilterClass);
begin
  if not FFactory.ContainsKey(AFilterClass.GetName) then
  begin
    FFactory.Register(AFilterClass.GetName, AFilterClass);
    if AFilterClass.GetName <> AFilterClass.GetShortName then
      FFactory.Register(AFilterClass.GetShortName, AFilterClass);
  end;
end;

initialization

finalization

TdxPDFStreamFilterFactory.Finalize;

end.
