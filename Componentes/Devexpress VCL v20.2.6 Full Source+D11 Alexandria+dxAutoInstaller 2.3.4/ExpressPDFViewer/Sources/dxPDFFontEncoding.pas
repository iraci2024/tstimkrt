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

unit dxPDFFontEncoding;

{$I cxVer.inc}

interface

uses
  SysUtils, Types, Generics.Defaults, Generics.Collections, Classes,
  dxCoreClasses, dxPDFBase, dxPDFTypes, dxPDFCore,
  dxPDFCharacterMapping, dxFontFile;

type
  TdxPDFSimpleFontEncoding = class;
  TdxPDFSimpleFontEncodingClass = class of TdxPDFSimpleFontEncoding;

  { TdxPDFSimpleFontEncoding }

  TdxPDFSimpleFontEncoding = class(TdxPDFCustomEncoding)
  strict private
    procedure ReadDifferences(ADictionary: TdxPDFDictionary);
    procedure WriteDifferences(ADictionary: TdxPDFDictionary);
  protected
    FDifferences: TdxPDFSortedIntegerStringDictionary;
    function GetShortName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    function UseShortWrite: Boolean; override;
  public
    class function CreateEncoding(const ABaseFont, AEncodingName: string;
      ADifferences: TdxPDFSortedIntegerStringDictionary = nil)
      : TdxPDFSimpleFontEncoding; static;

    function GetGlyphName(ACode: Byte): string;
    function GetStringData(const ABytes: TBytes;
      const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData; override;
    function ShouldUseEmbeddedFontEncoding: Boolean; override;

    function IsEmpty: Boolean; virtual;

    property Differences: TdxPDFSortedIntegerStringDictionary read FDifferences;
  end;

  { TdxPDFMacRomanEncoding }

  TdxPDFMacRomanEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
    function UseShortWrite: Boolean; override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFSymbolEncoding }

  TdxPDFSymbolEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFStandardEncoding }

  TdxPDFStandardEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  public
    class function GetTypeName: string; override;
    function IsEmpty: Boolean; override;
    function ShouldUseEmbeddedFontEncoding: Boolean; override;
  end;

  { TdxPDFDocEncoding }

  TdxPDFDocEncoding = class(TdxPDFStandardEncoding)
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFWinAnsiEncoding }

  TdxPDFWinAnsiEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFZapfDingbatsEncoding }

  TdxPDFZapfDingbatsEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFUnicodeConverter }

  TdxPDFUnicodeConverter = class
  public
    class function GetGlyphCode(AEncoding: TdxPDFSimpleFontEncoding;
      ACode: Word): Word; overload;
    class function GetGlyphCode(AEncoding: TdxPDFSimpleFontEncoding;
      AGlyphCodes: TdxPDFWordDictionary; ACode: Word): Word; overload;
    class function TryGetGlyphCode(const AGlyphName: string;
      out AGlyphCode: Word): Boolean; static;
  end;

  { TdxPDFCompositeFontEncoding }

  TdxPDFCompositeFontEncoding = class(TdxPDFCustomEncoding)
  public
    class function Parse(ARepository: TdxPDFCustomRepository;
      ASourceObject: TdxPDFBase): TdxPDFCompositeFontEncoding;
    function ShouldUseEmbeddedFontEncoding: Boolean; override;
    function IsVertical: Boolean; virtual;
  end;

  { TdxPDFCustomIdentityEncoding }

  TdxPDFCustomIdentityEncoding = class(TdxPDFCompositeFontEncoding)
  protected
    function GetShortName: string; override;
    function UseShortWrite: Boolean; override;
  public
    function GetStringData(const ABytes: TBytes;
      const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData; override;
  end;

  { TdxPDFHorizontalIdentityEncoding }

  TdxPDFHorizontalIdentityEncoding = class(TdxPDFCustomIdentityEncoding)
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFVerticalIdentityEncoding }

  TdxPDFVerticalIdentityEncoding = class(TdxPDFCustomIdentityEncoding)
  public
    class function GetTypeName: string; override;
    function IsVertical: Boolean; override;
  end;

  { TdxPDFPredefinedCompositeFontEncoding }

  TdxPDFPredefinedCompositeFontEncoding = class(TdxPDFCompositeFontEncoding)
  strict private
    FHorizontalNames: TStringList;
    FName: string;
    FVerticalNames: TStringList;
  protected
    function GetShortName: string; override;
    function UseShortWrite: Boolean; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;

    property HorizontalNames: TStringList read FHorizontalNames;
    property VerticalNames: TStringList read FVerticalNames;
  public
    constructor Create(const AName: string); reintroduce;
    function GetStringData(const ABytes: TBytes;
      const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData; override;
    function IsVertical: Boolean; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  end;

  { TdxPDFCustomCompositeFontEncoding }

  TdxPDFCustomCompositeFontEncoding = class(TdxPDFCompositeFontEncoding)
  strict private
    FBaseEncoding: TdxPDFCompositeFontEncoding;
    FCharacterMapping: TdxPDFCharacterMapping;
    FCIDSystemInfo: TdxPDFCIDSystemInfo;
    FName: string;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; const AData: TBytes);
      reintroduce;
  public
    function GetStringData(const ABytes: TBytes;
      const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  end;

implementation

uses
  Variants, dxCore, dxStringHelper, dxPDFUtils;

type
  TdxFontFileUnicodeConverterAccess = class(TdxFontFileUnicodeConverter);
  TdxPDFCharacterMappingAccess = class(TdxPDFCharacterMapping);
  TdxPDFCustomCompositeFontEncodingAccess = class
    (TdxPDFCustomCompositeFontEncoding);
  TdxPDFObjectAccess = class(TdxPDFObject);

  { TdxPDFCustomCompositeFontEncoding }

function TdxPDFCustomCompositeFontEncoding.GetStringData(const ABytes: TBytes;
  const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData;
begin
  Result := FCharacterMapping.GetStringData(ABytes, AGlyphOffsets);
end;

procedure TdxPDFCustomCompositeFontEncoding.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.CMap);
  ADictionary.AddName(TdxPDFKeywords.CMapName, FName);
  ADictionary.AddReference(TdxPDFKeywords.CIDSystemInfo, FCIDSystemInfo);
  if IsVertical then
    ADictionary.Add(TdxPDFKeywords.WMode, 1);
  ADictionary.AddReference(TdxPDFKeywords.UseCMap, FBaseEncoding);
  ADictionary.SetStreamData(TdxPDFCharacterMappingAccess
    (FCharacterMapping).Data);
end;

procedure TdxPDFCustomCompositeFontEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCIDSystemInfo := TdxPDFCIDSystemInfo.Create(Self);
end;

procedure TdxPDFCustomCompositeFontEncoding.DestroySubClasses;
begin
  FreeAndNil(FCharacterMapping);
  FreeAndNil(FBaseEncoding);
  FreeAndNil(FCIDSystemInfo);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomCompositeFontEncoding.
  Read(ADictionary: TdxPDFReaderDictionary; const AData: TBytes);
var
  AUseCMap: TdxPDFBase;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    FName := ADictionary.GetString(TdxPDFKeywords.CMapName);
    if ADictionary.TryGetObject(TdxPDFKeywords.UseCMap, AUseCMap) then
      FBaseEncoding := TdxPDFCompositeFontEncoding.Parse(Repository, AUseCMap);
    FCharacterMapping := TdxPDFCMapStreamParser.Parse(Repository, AData);
    TdxPDFObjectAccess(FCIDSystemInfo).
      Read(ADictionary.GetDictionary(TdxPDFKeywords.CIDSystemInfo));
  end;
end;

{ TdxPDFSimpleFontEncoding }

class function TdxPDFSimpleFontEncoding.CreateEncoding(const ABaseFont,
  AEncodingName: string;
  ADifferences: TdxPDFSortedIntegerStringDictionary = nil)
  : TdxPDFSimpleFontEncoding;
var
  AClass: TdxPDFObjectClass;
begin
  if AEncodingName <> '' then
    AClass := dxPDFGetDocumentObjectClass(AEncodingName)
  else if ABaseFont = TdxPDFMacRomanEncoding.GetTypeName then
    AClass := TdxPDFMacRomanEncoding
  else if ABaseFont = TdxPDFZapfDingbatsEncoding.GetTypeName then
    AClass := TdxPDFZapfDingbatsEncoding
  else if ABaseFont = TdxPDFSymbolEncoding.GetTypeName then
    AClass := TdxPDFSymbolEncoding
  else
    AClass := TdxPDFStandardEncoding;
  Result := AClass.Create(nil) as TdxPDFSimpleFontEncoding;
  if ADifferences <> nil then
  begin
    Result.FDifferences.Free;
    Result.FDifferences := ADifferences;
  end;
end;

function TdxPDFSimpleFontEncoding.GetGlyphName(ACode: Byte): string;
begin
  if not FDifferences.TryGetValue(ACode, Result) then
    if not FontFileEncoding.Dictionary.TryGetValue(ACode, Result) then
      Result := TdxGlyphNames._notdef;
end;

function TdxPDFSimpleFontEncoding.GetStringData(const ABytes: TBytes;
  const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData;
var
  I: Integer;
  ACharCodes: TdxPDFBytesDynArray;
  AOffsets: TDoubleDynArray;
  AStr: TWordDynArray;
begin
  SetLength(ACharCodes, Length(ABytes));
  SetLength(AStr, Length(ABytes));
  for I := 0 to Length(ABytes) - 1 do
  begin
    AStr[I] := Word(ABytes[I]);
    SetLength(ACharCodes[I], 1);
    ACharCodes[I][0] := ABytes[I];
  end;
  TdxPDFUtils.AddData(AGlyphOffsets, AOffsets);
  if Length(AGlyphOffsets) = 0 then
    for I := 0 to Length(ABytes) + 1 do
      TdxPDFUtils.AddValue(0, AOffsets);
  Result := TdxPDFStringCommandData.Create(ACharCodes, AStr, AOffsets);
end;

function TdxPDFSimpleFontEncoding.ShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := False;
end;

function TdxPDFSimpleFontEncoding.IsEmpty: Boolean;
begin
  Result := False;
end;

function TdxPDFSimpleFontEncoding.UseShortWrite: Boolean;
begin
  Result := FDifferences.Count = 0;
end;

procedure TdxPDFSimpleFontEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FDifferences := TdxPDFSortedIntegerStringDictionary.Create;
end;

function TdxPDFSimpleFontEncoding.GetShortName: string;
begin
  Result := inherited;
  if (Result <> TdxPDFMacRomanEncoding.GetTypeName) and
    (Result <> TdxPDFWinAnsiEncoding.GetTypeName) then
    Result := '';
end;

procedure TdxPDFSimpleFontEncoding.DestroySubClasses;
begin
  FreeAndNil(FDifferences);
  inherited DestroySubClasses;
end;

procedure TdxPDFSimpleFontEncoding.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadDifferences(ADictionary);
end;

procedure TdxPDFSimpleFontEncoding.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Encoding);
  ADictionary.AddName(TdxPDFKeywords.BaseEncoding, GetShortName);
  if Differences.Count > 0 then
    WriteDifferences(ADictionary);
end;

procedure TdxPDFSimpleFontEncoding.ReadDifferences
  (ADictionary: TdxPDFDictionary);
var
  I, ACode: Integer;
  AArray: TdxPDFArray;
  ADifference: TdxPDFBase;
begin
  FDifferences.Clear;
  if ADictionary <> nil then
  begin
    AArray := ADictionary.GetArray(TdxPDFKeywords.Differences);
    ACode := 0;
    if (AArray <> nil) and (AArray.Count > 0) then
      for I := 0 to AArray.Count - 1 do
      begin
        ADifference := AArray[I] as TdxPDFBase;
        if not(ADifference.ObjectType in [otName, otString]) then
          ACode := TdxPDFInteger(ADifference).Value
        else
        begin
          FDifferences.AddOrSetValue(ACode, TdxPDFName(ADifference).Value);
          Inc(ACode);
        end;
      end;
  end;
end;

procedure TdxPDFSimpleFontEncoding.WriteDifferences
  (ADictionary: TdxPDFDictionary);
var
  ADifferences: TdxPDFArray;
  AKey: Integer;
  APrevious: Integer;
begin
  ADifferences := TdxPDFArray.Create;
  APrevious := MinInt;
  for AKey in FDifferences.SortedKeys do
  begin
    Inc(APrevious);
    if AKey <> APrevious then
    begin
      ADifferences.Add(AKey);
      APrevious := AKey;
    end;
    ADifferences.Add(TdxPDFName.Create(FDifferences.Items[AKey]));
  end;
  ADictionary.Add(TdxPDFKeywords.Differences, ADifferences);
end;

{ TdxPDFMacRomanEncoding }

class function TdxPDFMacRomanEncoding.GetTypeName: string;
begin
  Result := 'MacRomanEncoding';
end;

function TdxPDFMacRomanEncoding.GetFontFileEncodingClass
  : TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileMacRomanEncoding;
end;

function TdxPDFMacRomanEncoding.UseShortWrite: Boolean;
begin
  Result := False;
end;

{ TdxPDFSymbolEncoding }

class function TdxPDFSymbolEncoding.GetTypeName: string;
begin
  Result := 'Symbol';
end;

function TdxPDFSymbolEncoding.GetFontFileEncodingClass
  : TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileSymbolEncoding;
end;

{ TdxPDFStandardEncoding }

class function TdxPDFStandardEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.StandardEncoding;
end;

function TdxPDFStandardEncoding.IsEmpty: Boolean;
begin
  Result := Differences.Count = 0;
end;

function TdxPDFStandardEncoding.ShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := True;
end;

function TdxPDFStandardEncoding.GetFontFileEncodingClass
  : TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileStandardEncoding;
end;

{ TdxPDFDocEncoding }

class function TdxPDFDocEncoding.GetTypeName: string;
begin
  Result := 'PDFDocEncoding';
end;

{ TdxPDFWinAnsiEncoding }

class function TdxPDFWinAnsiEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.WinAnsiEncoding;
end;

function TdxPDFWinAnsiEncoding.GetFontFileEncodingClass
  : TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileWinAnsiEncoding;
end;

{ TdxPDFZapfDingbatsEncoding }

class function TdxPDFZapfDingbatsEncoding.GetTypeName: string;
begin
  Result := 'ZapfDingbats';
end;

function TdxPDFZapfDingbatsEncoding.GetFontFileEncodingClass
  : TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileZapfDingbatsEncoding;
end;

{ TdxPDFUnicodeConverter }

class function TdxPDFUnicodeConverter.GetGlyphCode
  (AEncoding: TdxPDFSimpleFontEncoding; ACode: Word): Word;
begin
  Result := GetGlyphCode(AEncoding,
    TdxFontFileUnicodeConverterAccess(dxFontFileUnicodeConverter)
    .GlyphCodes, ACode);
end;

class function TdxPDFUnicodeConverter.GetGlyphCode
  (AEncoding: TdxPDFSimpleFontEncoding; AGlyphCodes: TdxPDFWordDictionary;
  ACode: Word): Word;
var
  AGlyphName: string;
begin
  AGlyphName := AEncoding.GetGlyphName(ACode);
  if not AGlyphCodes.TryGetValue(AGlyphName, Result) then
    if not dxFontFileUnicodeConverter.FindCode(AGlyphName, Result) then
      if not((AGlyphName = TdxGlyphNames.Zdotaccent) and
        AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, Result)) then
        if not((AGlyphName = TdxGlyphNames.LowerZdotaccent) and
          AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, Result)) then
          Result := ACode;
end;

class function TdxPDFUnicodeConverter.TryGetGlyphCode(const AGlyphName: string;
  out AGlyphCode: Word): Boolean;
var
  AActualGlyphCode: Integer;
  AGlyphNameLength: Integer;
  AGlyphCodes: TdxPDFWordDictionary;
begin
  AGlyphCodes := TdxFontFileUnicodeConverterAccess(dxFontFileUnicodeConverter)
    .GlyphCodes;
  if AGlyphCodes.TryGetValue(AGlyphName, AGlyphCode) then
    Exit(True)
  else
  begin
    AGlyphNameLength := Length(AGlyphName);
    if (AGlyphNameLength = 5) and (TdxPDFUtils.StartsWith(AGlyphName, 'u') or
      TdxPDFUtils.StartsWith(AGlyphName, 'U')) then
      if TryStrToInt(TdxStringHelper.Substring(AGlyphName, 2), AActualGlyphCode)
      then
      begin
        AGlyphCode := Word(AActualGlyphCode);
        Exit(True);
      end;
    if (AGlyphNameLength = 7) and (Pos('uni', AGlyphName) = 0) then
      if TryStrToInt(TdxStringHelper.Substring(AGlyphName, 4), AActualGlyphCode)
      then
      begin
        AGlyphCode := Word(AActualGlyphCode);
        Exit(True);
      end;
  end;
  AGlyphCode := 0;
  Result := False;
end;

{ TdxPDFCustomIdentityEncoding }

function TdxPDFCustomIdentityEncoding.GetStringData(const ABytes: TBytes;
  const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData;
var
  ACharCodes: TdxPDFBytesDynArray;
  AHighByte, ALowByte: Byte;
  AIsEmptyOffsets: Boolean;
  ALength, I, AByteIndex: Integer;
  AOffsets: TDoubleDynArray;
  AStr: TWordDynArray;
begin
  ALength := Length(ABytes) div 2;
  SetLength(AStr, ALength);
  SetLength(ACharCodes, ALength);
  SetLength(AOffsets, ALength + 1);
  AIsEmptyOffsets := Length(AGlyphOffsets) = 0;
  AByteIndex := 0;
  for I := 0 to ALength - 1 do
  begin
    if not AIsEmptyOffsets then
      AOffsets[I] := AGlyphOffsets[AByteIndex];
    AHighByte := ABytes[AByteIndex];
    ALowByte := ABytes[AByteIndex + 1];
    Inc(AByteIndex, 2);
    AStr[I] := AHighByte shl 8 + ALowByte;
    SetLength(ACharCodes[I], 2);
    ACharCodes[I][0] := AHighByte;
    ACharCodes[I][1] := ALowByte;
  end;
  Result := TdxPDFStringCommandData.Create(ACharCodes, AStr, AOffsets);
end;

function TdxPDFCustomIdentityEncoding.GetShortName: string;
begin
  Result := GetTypeName;
end;

function TdxPDFCustomIdentityEncoding.UseShortWrite: Boolean;
begin
  Result := True;
end;

{ TdxPDFCompositeFontEncoding }

class function TdxPDFCompositeFontEncoding.Parse(ARepository
  : TdxPDFCustomRepository; ASourceObject: TdxPDFBase)
  : TdxPDFCompositeFontEncoding;
var
  AName: string;
  AStream: TdxPDFStream;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if ASourceObject <> nil then
    case ASourceObject.ObjectType of
      otDictionary, otStream:
        begin
          if ASourceObject.ObjectType = otStream then
          begin
            AStream := TdxPDFStream(ASourceObject);
            ADictionary := AStream.Dictionary as TdxPDFReaderDictionary;
          end
          else
          begin
            ADictionary := ASourceObject as TdxPDFReaderDictionary;
            AStream := ADictionary.StreamRef;
          end;
          if ADictionary <> nil then
          begin
            Result := TdxPDFCustomCompositeFontEncoding.Create(nil);
            TdxPDFCustomCompositeFontEncodingAccess(Result).
              Read(ADictionary, AStream.UncompressedData);
          end;
        end;
      otName, otString:
        begin
          AName := TdxPDFName(ASourceObject).Value;
          if AName = TdxPDFVerticalIdentityEncoding.GetTypeName then
            Result := TdxPDFVerticalIdentityEncoding.Create(nil)
          else if AName = TdxPDFHorizontalIdentityEncoding.GetTypeName then
            Result := TdxPDFHorizontalIdentityEncoding.Create(nil)
          else
            Result := TdxPDFPredefinedCompositeFontEncoding.Create(AName)
              as TdxPDFCompositeFontEncoding
        end;
    end
  else
    Result := TdxPDFHorizontalIdentityEncoding.Create(nil);
end;

function TdxPDFCompositeFontEncoding.ShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := False;
end;

function TdxPDFCompositeFontEncoding.IsVertical: Boolean;
begin
  Result := False;
end;

{ TdxPDFVerticalIdentityEncoding }

function TdxPDFVerticalIdentityEncoding.IsVertical: Boolean;
begin
  Result := True;
end;

class function TdxPDFVerticalIdentityEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.IdentityV;
end;

{ TdxPDFPredefinedCompositeFontEncoding }

constructor TdxPDFPredefinedCompositeFontEncoding.Create(const AName: string);
begin
  inherited Create(nil);
  FName := AName;
end;

function TdxPDFPredefinedCompositeFontEncoding.GetStringData
  (const ABytes: TBytes; const AGlyphOffsets: TDoubleDynArray)
  : TdxPDFStringCommandData;
var
  I, ALength: Integer;
  AByteIndex: Integer;
  ACharCodes: TdxPDFBytesDynArray;
  AOffsets: TDoubleDynArray;
  AStr: TWordDynArray;
begin
  ALength := Length(ABytes) div 2;
  SetLength(ACharCodes, ALength);
  SetLength(AStr, ALength);
  SetLength(AOffsets, ALength + 1);
  for I := 0 to ALength do
    AOffsets[I] := 0;
  AByteIndex := 0;
  if Length(AGlyphOffsets) > 0 then
    for I := 0 to ALength - 1 do
      AOffsets[I] := AGlyphOffsets[AByteIndex];
  Result := TdxPDFStringCommandData.Create(ACharCodes, AStr, AOffsets);
end;

function TdxPDFPredefinedCompositeFontEncoding.IsVertical: Boolean;
begin
  Result := True;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.
  Write(AHelper: TdxPDFWriterHelper; ADictionary: TdxPDFWriterDictionary);
begin
  RaiseWriteNotImplementedException;
end;

function TdxPDFPredefinedCompositeFontEncoding.GetShortName: string;
begin
  Result := FName;
end;

function TdxPDFPredefinedCompositeFontEncoding.UseShortWrite: Boolean;
begin
  Result := True;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FHorizontalNames := TStringList.Create;
  FVerticalNames := TStringList.Create;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.DestroySubClasses;
begin
  FreeAndNil(FVerticalNames);
  FreeAndNil(FHorizontalNames);
  inherited DestroySubClasses;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.Initialize;
begin
  FHorizontalNames.Add('GB-EUC-H');
  FHorizontalNames.Add('GBpc-EUC-H');
  FHorizontalNames.Add('GBK-EUC-H');
  FHorizontalNames.Add('GBKp-EUC-H');
  FHorizontalNames.Add('GBK2K-H');
  FHorizontalNames.Add('UniGB-UCS2-H');
  FHorizontalNames.Add('UniGB-UTF16-H');
  FHorizontalNames.Add('B5pc-H');
  FHorizontalNames.Add('HKscs-B5-H');
  FHorizontalNames.Add('ETen-B5-H');
  FHorizontalNames.Add('ETenms-B5-H');
  FHorizontalNames.Add('CNS-EUC-H');
  FHorizontalNames.Add('UniCNS-UCS2-H');
  FHorizontalNames.Add('UniCNS-UTF16-H');
  FHorizontalNames.Add('83pv-RKSJ-H');
  FHorizontalNames.Add('90ms-RKSJ-H');
  FHorizontalNames.Add('90msp-RKSJ-H');
  FHorizontalNames.Add('90pv-RKSJ-H');
  FHorizontalNames.Add('Add-RKSJ-H');
  FHorizontalNames.Add('EUC-H');
  FHorizontalNames.Add('Ext-RKSJ-H');
  FHorizontalNames.Add('H');
  FHorizontalNames.Add('UniJIS-UCS2-H');
  FHorizontalNames.Add('UniJIS-UCS2-HW-H');
  FHorizontalNames.Add('UniJIS-UTF16-H');
  FHorizontalNames.Add('KSC-EUC-H');
  FHorizontalNames.Add('KSCms-UHC-H');
  FHorizontalNames.Add('KSCms-UHC-HW-H');
  FHorizontalNames.Add('KSCpc-EUC-H');
  FHorizontalNames.Add('UniKS-UCS2-H');
  FHorizontalNames.Add('UniKS-UTF16-H');

  FVerticalNames.Add('GB-EUC-V');
  FVerticalNames.Add('GBpc-EUC-V');
  FVerticalNames.Add('GBK-EUC-V');
  FVerticalNames.Add('GBKp-EUC-V');
  FVerticalNames.Add('GBK2K-V');
  FVerticalNames.Add('UniGB-UCS2-V');
  FVerticalNames.Add('UniGB-UTF16-V');
  FVerticalNames.Add('B5pc-V');
  FVerticalNames.Add('HKscs-B5-V');
  FVerticalNames.Add('ETen-B5-V');
  FVerticalNames.Add('ETenms-B5-V');
  FVerticalNames.Add('CNS-EUC-V');
  FVerticalNames.Add('UniCNS-UCS2-V');
  FVerticalNames.Add('UniCNS-UTF16-V');
  FVerticalNames.Add('90ms-RKSJ-V');
  FVerticalNames.Add('90msp-RKSJ-V');
  FVerticalNames.Add('Add-RKSJ-V');
  FVerticalNames.Add('EUC-V');
  FVerticalNames.Add('Ext-RKSJ-V');
  FVerticalNames.Add('V');
  FVerticalNames.Add('UniJIS-UCS2-V');
  FVerticalNames.Add('UniJIS-UCS2-HW-V');
  FVerticalNames.Add('UniJIS-UTF16-V');
  FVerticalNames.Add('KSC-EUC-V');
  FVerticalNames.Add('KSCms-UHC-V');
  FVerticalNames.Add('KSCms-UHC-HW-V');
  FVerticalNames.Add('UniKS-UCS2-V');
  FVerticalNames.Add('UniKS-UTF16-V');
end;

{ TdxPDFHorizontalIdentityEncoding }

class function TdxPDFHorizontalIdentityEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.IdentityH;
end;

procedure dxPDFRegisterPredefinedCompositeFontEncodings;

  procedure RegisterEncodingNames(ANames: TStringList);
  var
    I: Integer;
  begin
    for I := 0 to ANames.Count - 1 do
      dxPDFRegisterDocumentObjectClass(ANames[I],
        TdxPDFPredefinedCompositeFontEncoding);
  end;

var
  AEncodingNames: TStringList;
  ATempEncoding: TdxPDFPredefinedCompositeFontEncoding;
begin
  ATempEncoding := TdxPDFPredefinedCompositeFontEncoding.Create('TempFont');
  try
    AEncodingNames := ATempEncoding.HorizontalNames;
    RegisterEncodingNames(AEncodingNames);
    AEncodingNames := ATempEncoding.VerticalNames;
    RegisterEncodingNames(AEncodingNames);
  finally
    ATempEncoding.Free;
  end;
end;

procedure dxPDFUnregisterPredefinedCompositeFontEncodings;

  procedure UnregisterEncodingNames(ANames: TStringList);
  var
    I: Integer;
  begin
    for I := 0 to ANames.Count - 1 do
      dxPDFUnregisterDocumentObjectClass(ANames[I],
        TdxPDFPredefinedCompositeFontEncoding);
  end;

var
  AEncodingNames: TStringList;
  ATempEncoding: TdxPDFPredefinedCompositeFontEncoding;
begin
  ATempEncoding := TdxPDFPredefinedCompositeFontEncoding.Create('TempFont');
  try
    AEncodingNames := ATempEncoding.VerticalNames;
    UnregisterEncodingNames(AEncodingNames);
    AEncodingNames := ATempEncoding.HorizontalNames;
    UnregisterEncodingNames(AEncodingNames);
  finally
    ATempEncoding.Free;
  end;
end;

initialization

dxPDFRegisterDocumentObjectClass(TdxPDFMacRomanEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFStandardEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFWinAnsiEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFSymbolEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFZapfDingbatsEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFHorizontalIdentityEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFVerticalIdentityEncoding);
dxPDFRegisterDocumentObjectClass(TdxPDFDocEncoding);
dxPDFRegisterPredefinedCompositeFontEncodings;

finalization

dxPDFUnregisterPredefinedCompositeFontEncodings;
dxPDFUnregisterDocumentObjectClass(TdxPDFDocEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFVerticalIdentityEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFHorizontalIdentityEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFZapfDingbatsEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFSymbolEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFWinAnsiEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFStandardEncoding);
dxPDFUnregisterDocumentObjectClass(TdxPDFMacRomanEncoding);

end.
