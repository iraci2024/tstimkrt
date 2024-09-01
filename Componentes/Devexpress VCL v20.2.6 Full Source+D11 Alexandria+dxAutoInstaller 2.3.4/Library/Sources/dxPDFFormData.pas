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

unit dxPDFFormData; // for internal use

{$I cxVer.inc}

interface

uses
  Types, Generics.Defaults, Generics.Collections, Classes, dxPDFCore;

type
  TdxPDFFormDataFormat = (dfFDF, dfXFDF, dfXML, dfTXT);

  { TdxPDFFormDataValue }

  TdxPDFFormDataValue = class
  protected
    function GetValue: Variant; virtual; abstract;
    procedure ResetValue; virtual; abstract;
    procedure SetValue(const AValue: Variant); virtual; abstract;
  public
    property Value: Variant read GetValue write SetValue;
  end;

  { TdxPDFFormData }

  TdxPDFFormData = class
  strict private
    FItems: TObjectDictionary<string, TdxPDFFormData>;
    FName: string;
    FValueWrapper: TdxPDFFormDataValue;

    function GetItem(const AKey: string): TdxPDFFormData;
    function GetValue: Variant;
    procedure SetItem(const AKey: string; AValue: TdxPDFFormData);
    procedure SetValue(const AValue: Variant);
  protected
    property Dictionary: TObjectDictionary<string, TdxPDFFormData> read FItems;

    function IsPasswordField: Boolean;
    procedure Assign(AData: TdxPDFFormData);
    procedure Load(AStream: TStream); overload;
    procedure Load(AStream: TStream; AFormat: TdxPDFFormDataFormat); overload;
    procedure Load(const AFileName: string); overload;
    procedure Reset;
    procedure Save(const AFileName: string); overload;
    procedure Save(AStream: TStream; AFormat: TdxPDFFormDataFormat); overload;

    property Items[const AKey: string]: TdxPDFFormData read GetItem
      write SetItem; default;
    property Name: string read FName;
    property Value: Variant read GetValue write SetValue;
  public
    class function DetectFormat(AStream: TStream): TdxPDFFormDataFormat; static;
    constructor Create; overload;
    constructor Create(AField: TdxPDFInteractiveFormField); overload;
    // for internal use
    destructor Destroy; override;
  end;

implementation

uses
  IOUtils, StrUtils, SysUtils, dxCore, dxStringHelper, dxXMLDoc, dxPDFBase,
  dxPDFTypes, dxPDFParser,
  dxPDFDocument, dxPDFUtils;

const
  dxFDFVersion = '1.2';
  dxPDFFormDataFieldNameDelimiter = '.';
  dxPDFXFDF = 'xfdf';
  dxPDFXFDFAttributeName = 'name';
  dxPDFXFDFField = 'field';
  dxPDFXFDFFields = 'fields';
  dxPDFXFDFNamespace = 'http://ns.adobe.com/xfdf/';
  dxPDFXFDFOriginalAttributeName =
    '{http://ns.adobe.com/xfdf-transition/}original';

type
  { TdxPDFFormDataFieldValue }

  TdxPDFFormDataFieldValue = class(TdxPDFFormDataValue)
  strict private
    FField: TdxPDFInteractiveFormField;
  protected
    function GetValue: Variant; override;
    procedure ResetValue; override;
    procedure SetValue(const AValue: Variant); override;
  public
    constructor Create(AField: TdxPDFInteractiveFormField);
  end;

  { TdxPDFFormDataDetachedValue }

  TdxPDFFormDataDetachedValue = class(TdxPDFFormDataValue)
  strict private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
    procedure ResetValue; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  { TdxFDFDocumentReader }

  TdxFDFDocumentReader = class
  strict private
    FRepository: TdxPDFDocumentRepository;
    FStream: TMemoryStream;
    FRootObjectNumber: Integer;
    function FindFields(out AFields: TdxPDFArray): Boolean;
    function FindTrailerPositionAndReadObjects(out ATrailerPosition
      : Int64): Boolean;
    function ReadRootObjectNumber(ATrailerPosition: Int64): Boolean;
    function ReadTrailerData: TBytes;
    procedure ReadField(AData: TdxPDFFormData; const AParentName: string;
      ADictionary: TdxPDFDictionary);
    procedure ReadFields(AFields: TdxPDFArray; AData: TdxPDFFormData;
      const AParentName: string);
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure Read(AData: TdxPDFFormData);
  end;

  { TdxPDFFormDataReader }

  TdxPDFFormDataReader = class
  strict private
    FData: TdxPDFFormData;
    procedure ReadFDF(AStream: TStream);
    procedure ReadTXT(AStream: TStream);
    procedure ReadXML(AStream: TStream);
    procedure ReadXMLNode(ANode: TdxXMLNode);
  public
    constructor Create(AData: TdxPDFFormData);
    procedure Read(AStream: TStream; AFormat: TdxPDFFormDataFormat);
  end;

  { TdxFDFCatalog }

  TdxFDFCatalog = class(TdxPDFObject)
  strict private
    FData: TdxPDFFormData;
    function CreateDictionary(AHelper: TdxPDFWriterHelper;
      AData: TdxPDFFormData; AOnlyKids: Boolean): TdxPDFDictionary;
  protected
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    constructor Create(AData: TdxPDFFormData);
  end;

  { TdxFDFWriter }

  TdxFDFWriter = class(TdxPDFDocumentCustomWriter)
  strict private
    FCatalog: TdxFDFCatalog;
  strict protected
    function GetVersion: string; override;
    function HasXRef: Boolean; override;
    procedure PopulateTrailer(ADictionary: TdxPDFWriterDictionary); override;
    procedure RegisterTrailerObjects; override;
  public
    constructor Create(AStream: TStream; ACatalog: TdxFDFCatalog);
  end;

  { TdxPDFFormDataWriterPair }

  TdxPDFFormDataWriterPair = record
  public
    Data: TdxPDFFormData;
    Key: string;
    class function Create(const AKey: string; AData: TdxPDFFormData)
      : TdxPDFFormDataWriterPair; static;
  end;

  TdxPDFFormDataWriterWriteProc = reference to procedure(const AKey: string;
    const AValue: Variant);

  { TdxPDFFormDataWriter }

  TdxPDFFormDataWriter = class
  strict private
    FData: TdxPDFFormData;
    procedure WriteData(AWriteProc: TdxPDFFormDataWriterWriteProc);
    procedure WriteFDF(AStream: TStream);
    procedure WriteTXT(AStream: TStream);
    procedure WriteXFDF(AStream: TStream);
    procedure WriteXML(AStream: TStream);
  public
    class procedure Write(AStream: TStream; AData: TdxPDFFormData;
      AFormat: TdxPDFFormDataFormat); static;
    constructor Create(AData: TdxPDFFormData);
  end;

procedure dxPDFFormDataRaiseException;
begin
  TdxPDFUtils.RaiseException
    ('An error occurred while reading the form data from the specified file.');
end;

{ TdxPDFFormDataFieldValue }

constructor TdxPDFFormDataFieldValue.Create(AField: TdxPDFInteractiveFormField);
begin
  inherited Create;
  FField := AField;
end;

function TdxPDFFormDataFieldValue.GetValue: Variant;
begin
  Result := FField.GetValue;
end;

procedure TdxPDFFormDataFieldValue.ResetValue;
begin
  Value := FField.DefaultValue;
end;

procedure TdxPDFFormDataFieldValue.SetValue(const AValue: Variant);
begin
  FField.SetValue(AValue, nil);
end;

{ TdxPDFFormDataDetachedValue }

function TdxPDFFormDataDetachedValue.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TdxPDFFormDataDetachedValue.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TdxPDFFormDataDetachedValue.ResetValue;
begin
  FValue := varEmpty;
end;

{ TdxPDFFormData }

class function TdxPDFFormData.DetectFormat(AStream: TStream)
  : TdxPDFFormDataFormat;
var
  AFirstLine: string;
begin
  AFirstLine := ReplaceStr(TdxPDFUtils.ReadLine(AStream), #$EF#$BB#$BF, '');
  AStream.Position := 0;
  if TdxStringHelper.StartsWith(AFirstLine, '%FDF-') then
    Result := dfFDF
  else if TdxStringHelper.StartsWith(AFirstLine, '<?xml') then
    Result := dfXML
  else
    Result := dfTXT;
end;

constructor TdxPDFFormData.Create;
begin
  inherited Create;
  FItems := TObjectDictionary<string, TdxPDFFormData>.Create([doOwnsValues]);
  FValueWrapper := TdxPDFFormDataDetachedValue.Create;
end;

constructor TdxPDFFormData.Create(AField: TdxPDFInteractiveFormField);
begin
  Create;
  FreeAndNil(FValueWrapper);
  FValueWrapper := TdxPDFFormDataFieldValue.Create(AField);
end;

destructor TdxPDFFormData.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FValueWrapper);
  inherited Destroy;
end;

function TdxPDFFormData.IsPasswordField: Boolean;
begin
  Result := False;
end;

procedure TdxPDFFormData.Assign(AData: TdxPDFFormData);
var
  ADestinationData, ASourceData: TdxPDFFormData;
begin
  if (AData = nil) or (AData.Name <> Name) then
    Exit;
  SetValue(AData.Value);
  for ASourceData in AData.FItems.Values do
    if FItems.TryGetValue(ASourceData.Name, ADestinationData) then
      ADestinationData.Assign(ASourceData);
end;

procedure TdxPDFFormData.Load(AStream: TStream);
begin
  Load(AStream, DetectFormat(AStream));
end;

procedure TdxPDFFormData.Load(AStream: TStream; AFormat: TdxPDFFormDataFormat);
var
  AReader: TdxPDFFormDataReader;
begin
  AReader := TdxPDFFormDataReader.Create(Self);
  try
    AReader.Read(AStream, AFormat);
  finally
    AReader.Free;
  end;
end;

procedure TdxPDFFormData.Load(const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Load(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxPDFFormData.Reset;
var
  AData: TdxPDFFormData;
begin
  if FValueWrapper <> nil then
    FValueWrapper.ResetValue;
  for AData in FItems.Values do
    AData.Reset;
end;

procedure TdxPDFFormData.Save(const AFileName: string);

  function DetectFormat: TdxPDFFormDataFormat;
  var
    AExtension: string;
  begin
    AExtension := LowerCase(ReplaceStr(TPath.GetExtension(AFileName), '.', ''));
    if AExtension = 'xml' then
      Result := dfXML
    else if AExtension = 'xfd' then
      Result := dfXFDF
    else if AExtension = 'fdf' then
      Result := dfFDF
    else
      Result := dfTXT;
  end;

var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    Save(AStream, DetectFormat);
  finally
    AStream.Free;
  end;
end;

procedure TdxPDFFormData.Save(AStream: TStream; AFormat: TdxPDFFormDataFormat);
begin
  TdxPDFFormDataWriter.Write(AStream, Self, AFormat);
end;

function TdxPDFFormData.GetItem(const AKey: string): TdxPDFFormData;
var
  ASplittedName: TStringDynArray;
begin
  if not FItems.TryGetValue(AKey, Result) then
  begin
    ASplittedName := TdxPDFUtils.Split(AKey,
      dxPDFFormDataFieldNameDelimiter, 2);
    if Length(ASplittedName) > 1 then
      Exit(FItems[ASplittedName[0]][ASplittedName[1]]);
    Result := TdxPDFFormData.Create;
    Result.FName := AKey;
    FItems.Add(AKey, Result);
  end;
end;

function TdxPDFFormData.GetValue: Variant;
begin
  Result := FValueWrapper.Value;
end;

procedure TdxPDFFormData.SetItem(const AKey: string; AValue: TdxPDFFormData);
begin
  if AValue = nil then
    FItems.Remove(AKey)
  else
  begin
    AValue.FName := AKey;
    FItems.AddOrSetValue(AKey, AValue);
  end;
end;

procedure TdxPDFFormData.SetValue(const AValue: Variant);
begin
  if FValueWrapper <> nil then
    FValueWrapper.Value := AValue;
end;

{ TdxFDFDocumentReader }

constructor TdxFDFDocumentReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FStream.LoadFromStream(AStream);
  FRepository := TdxPDFDocumentRepository.Create(FStream);
end;

destructor TdxFDFDocumentReader.Destroy;
begin
  FreeAndNil(FRepository);
  inherited Destroy;
end;

procedure TdxFDFDocumentReader.Read(AData: TdxPDFFormData);
var
  AFields: TdxPDFArray;
  ATrailerPosition: Int64;
begin
  TdxPDFUtils.ReadLine(FStream);
  if FindTrailerPositionAndReadObjects(ATrailerPosition) and
    ReadRootObjectNumber(ATrailerPosition) and FindFields(AFields) then
    ReadFields(AFields, AData, '')
  else
    dxPDFFormDataRaiseException;
end;

procedure TdxFDFDocumentReader.ReadField(AData: TdxPDFFormData;
  const AParentName: string; ADictionary: TdxPDFDictionary);

  function AList(AArray: TdxPDFArray): string;
  var
    AList: TStringList;
    AValue: TdxPDFBase;
  begin
    AList := TStringList.Create;
    try
      for AValue in AArray.ElementList do
        if AValue.ObjectType = otString then
          AList.Add(TdxPDFString(AValue).Value)
        else
          AList.Add('');
      Result := AList.Text;
    finally
      AList.Free;
    end;
  end;

var
  AKey: string;
  AValueAsArray: TdxPDFArray;
  AValueAsString: string;
begin
  AKey := ADictionary.GetString(TdxPDFKeywords.FDFField);
  if AParentName <> '' then
    AKey := AParentName + dxPDFFormDataFieldNameDelimiter + AKey;
  if ADictionary.TryGetArray(TdxPDFKeywords.ShortValue, AValueAsArray) then
    AData[AKey].Value := AList(AValueAsArray)
  else if ADictionary.TryGetString(TdxPDFKeywords.ShortValue, AValueAsString)
  then
    AData[AKey].Value := AValueAsString
  else if ADictionary.TryGetArray(TdxPDFKeywords.Kids, AValueAsArray) then
    ReadFields(AValueAsArray, AData, AKey)
end;

procedure TdxFDFDocumentReader.ReadFields(AFields: TdxPDFArray;
  AData: TdxPDFFormData; const AParentName: string);
var
  AValue: TdxPDFBase;
begin
  for AValue in AFields.ElementList do
    if AValue.ObjectType = otDictionary then
      ReadField(AData, AParentName, TdxPDFDictionary(AValue));
end;

function TdxFDFDocumentReader.FindFields(out AFields: TdxPDFArray): Boolean;
var
  ADictionary: TdxPDFDictionary;
begin
  Result := FRepository.TryGetDictionary(FRootObjectNumber, ADictionary) and
    ADictionary.TryGetDictionary(TdxPDFKeywords.FDF, ADictionary) and
    ADictionary.TryGetArray(TdxPDFKeywords.Fields, AFields);
end;

function TdxFDFDocumentReader.FindTrailerPositionAndReadObjects
  (out ATrailerPosition: Int64): Boolean;
var
  AObject: TdxPDFBase;
  AToken: TdxPDFTokenDescription;
begin
  try
    FRepository.Parser.SaveCurrentPosition;
    FRepository.Parser.SkipSpaces;
    FRepository.Parser.RestoreCurrentPosition;
    AToken := TdxPDFTokenDescription.Create(TdxPDFKeywords.Trailer);
    try
      if FRepository.Parser.FindToken(AToken) then
      begin
        ATrailerPosition := FStream.Position;
        FRepository.Parser.RestoreCurrentPosition;
        repeat
          FRepository.Parser.SaveCurrentPosition;
          if FRepository.Parser.ReadToken(AToken) then
            Break;
          FRepository.Parser.RestoreCurrentPosition;
          AObject := FRepository.Parser.ReadObject(FRepository.Parser.Position);
          if AObject <> nil then
            FRepository.Add(AObject.Number, AObject);
        until not FRepository.Parser.IsEOF;
      end
    finally
      AToken.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TdxFDFDocumentReader.ReadRootObjectNumber(ATrailerPosition
  : Int64): Boolean;
var
  AData: TBytes;
  ATrailer: TdxPDFDictionary;
begin
  FStream.Position := ATrailerPosition;
  AData := ReadTrailerData;
  ATrailer := FRepository.Parser.ReadDictionary(AData);
  try
    Result := (ATrailer <> nil) and ATrailer.TryGetReference
      (TdxPDFKeywords.Root, FRootObjectNumber);
  finally
    dxPDFFreeObject(ATrailer);
  end;
end;

function TdxFDFDocumentReader.ReadTrailerData: TBytes;
var
  ASymbol: Byte;
  AToken: TdxPDFTokenDescription;
begin
  SetLength(Result, 0);
  AToken := TdxPDFTokenDescription.Create(TdxPDFKeywords.EOF);
  try
    while not FRepository.Parser.IsEOF do
    begin
      ASymbol := FRepository.Parser.ReadByte;
      TdxPDFUtils.AddByte(ASymbol, Result);
      if AToken.Compare(ASymbol) then
      begin
        TdxPDFUtils.DeleteData(Length(Result) - AToken.SequenceLength,
          AToken.SequenceLength, Result);
        Break;
      end;
    end;
  finally
    AToken.Free;
  end;
end;

{ TdxPDFFormDataReader }

procedure TdxPDFFormDataReader.Read(AStream: TStream;
  AFormat: TdxPDFFormDataFormat);
begin
  try
    case AFormat of
      dfFDF:
        ReadFDF(AStream);
      dfXML, dfXFDF:
        ReadXML(AStream);
    else
      ReadTXT(AStream);
    end;
  except
    dxPDFFormDataRaiseException;
  end;
end;

constructor TdxPDFFormDataReader.Create(AData: TdxPDFFormData);
begin
  inherited Create;
  FData := AData;
end;

procedure TdxPDFFormDataReader.ReadFDF(AStream: TStream);
var
  AReader: TdxFDFDocumentReader;
begin
  AReader := TdxFDFDocumentReader.Create(AStream);
  try
    AReader.Read(FData);
  finally
    AReader.Free;
  end;
end;

procedure TdxPDFFormDataReader.ReadTXT(AStream: TStream);

  procedure PopulateData(out AKeys, AValues: TStringDynArray);
  var
    AStringList: TStringList;
  begin
    AStringList := TStringList.Create;
    try
      AStringList.LoadFromStream(AStream);
      if AStringList.Count < 2 then
        Exit;
      AKeys := TdxPDFUtils.Split(AStringList[0], #9);
      AValues := TdxPDFUtils.Split(TdxStringHelper.TrimEnd(AStringList[1],
        [#10, #13]), #9);
    finally
      AStringList.Free;
    end;
  end;

var
  AKey, AValue: string;
  AKeyCount, AValueCount, I: Integer;
  AKeys, AValues: TStringDynArray;
begin
  PopulateData(AKeys, AValues);
  AKeyCount := Length(AKeys);
  AValueCount := Length(AValues);
  if (AKeyCount > 0) and (AValueCount > 0) then
  begin
    for I := 0 to AKeyCount - 1 do
    begin
      AKey := AKeys[I];
      if I < AValueCount then
        AValue := AValues[I]
      else
        AValue := '';
      if TdxStringHelper.StartsWith(AValue, '"') then
      begin
        AValue := TdxStringHelper.Substring(AValue, 1, Length(AValue) - 2);
        AValue := ReplaceStr(AValue, '""', '"');
        FData.Items[AKey].Value := TdxStringHelper.Split(AValue, [#10]);
      end
      else
        FData.Items[AKey].Value := AValue;
    end;
  end;
end;

procedure TdxPDFFormDataReader.ReadXML(AStream: TStream);
var
  AData: TdxXMLDocument;
  AFields: TdxXMLNode;
begin
  AData := TdxXMLDocument.Create;
  try
    AData.LoadFromStream(AStream);
    if AData.Root.FindChild([dxPDFXFDF, dxPDFXFDFFields], AFields) then
    begin
      if AFields.NameAsString = dxPDFXFDF then
        AFields := AFields.FindChild(dxPDFXFDFFields);
      if (AFields <> nil) and (AFields.NameAsString = dxPDFXFDFFields) then
        AFields.ForEach(procedure(ANode: TdxXMLNode; AUserData: Pointer)
          begin
            ReadXMLNode(ANode);
          end);
    end;
  finally
    AData.Free;
  end;
end;

procedure TdxPDFFormDataReader.ReadXMLNode(ANode: TdxXMLNode);
var
  AAttribute: TdxXMLNodeAttribute;
  AKey: string;
  AValues: TStringDynArray;
begin
  if ANode.Attributes.Find(dxPDFXFDFAttributeName, AAttribute) or
    ANode.Attributes.Find(dxPDFXFDFOriginalAttributeName, AAttribute) then
    AKey := dxXMLStringToString(AAttribute.Name)
  else
    AKey := ANode.NameAsString;
  SetLength(AValues, 0);
  ANode.ForEach(procedure(ANode: TdxXMLNode; AUserData: Pointer)
    begin
      TdxPDFUtils.AddValue(ANode.TextAsString, AValues);
    end);
  case Length(AValues) of
    0:
      FData.Items[AKey].Value := ANode.TextAsString;
    1:
      FData.Items[AKey].Value := AValues[0];
  else
    FData.Items[AKey].Value := AValues;
  end;
end;

{ TdxPDFFormDataWriter }

class procedure TdxPDFFormDataWriter.Write(AStream: TStream;
AData: TdxPDFFormData; AFormat: TdxPDFFormDataFormat);
var
  AWriter: TdxPDFFormDataWriter;
begin
  AWriter := TdxPDFFormDataWriter.Create(AData);
  try
    case AFormat of
      dfFDF:
        AWriter.WriteFDF(AStream);
      dfXML:
        AWriter.WriteXML(AStream);
      dfXFDF:
        AWriter.WriteXFDF(AStream);
    else
      AWriter.WriteTXT(AStream);
    end;
  finally
    AWriter.Free;
  end;
end;

constructor TdxPDFFormDataWriter.Create(AData: TdxPDFFormData);
begin
  inherited Create;
  FData := AData;
end;

procedure TdxPDFFormDataWriter.WriteData(AWriteProc
  : TdxPDFFormDataWriterWriteProc);
var
  AChildForm: TdxPDFFormData;
  AItem: TdxPDFFormDataWriterPair;
  APair: TPair<string, TdxPDFFormData>;
  AQueue: TQueue<TdxPDFFormDataWriterPair>;
begin
  AQueue := TQueue<TdxPDFFormDataWriterPair>.Create;
  try
    for APair in FData.Dictionary do
      AQueue.Enqueue(TdxPDFFormDataWriterPair.Create(APair.Key, APair.Value));
    while AQueue.Count > 0 do
    begin
      AItem := AQueue.Dequeue;
      AWriteProc(AItem.Key, AItem.Data.Value);
      for AChildForm in AItem.Data.Dictionary.Values do
        AQueue.Enqueue(TdxPDFFormDataWriterPair.Create(AItem.Key +
          dxPDFFormDataFieldNameDelimiter + AChildForm.Name, AChildForm))
    end;
  finally
    AQueue.Free;
  end;
end;

procedure TdxPDFFormDataWriter.WriteFDF(AStream: TStream);
var
  ACatalog: TdxFDFCatalog;
  AWriter: TdxFDFWriter;
begin
  ACatalog := TdxFDFCatalog.Create(FData);
  try
    AWriter := TdxFDFWriter.Create(AStream, ACatalog);
    try
      AWriter.Write;
    finally
      AWriter.Free;
    end;
  finally
    ACatalog.Free;
  end;
end;

procedure TdxPDFFormDataWriter.WriteTXT(AStream: TStream);
var
  ANames, AValues: string;
begin
  WriteData(procedure(const AName: string; const AValue: Variant)
    begin
      ANames := ANames + AName + #9;
      AValues := AValues + '"' + AValue + '"';
    end);
  WriteStringToStream(AStream, dxStringToAnsiString(ANames));
  WriteStringToStream(AStream, dxCRLF);
  WriteStringToStream(AStream, dxStringToAnsiString(AValues));
  WriteStringToStream(AStream, dxCRLF);
end;

procedure TdxPDFFormDataWriter.WriteXML(AStream: TStream);
var
  ADocument: TdxXMLDocument;
  AFields: TdxXMLNode;
begin
  ADocument := TdxXMLDocument.Create;
  try
    ADocument.AutoIndent := True;
    AFields := ADocument.Root.AddChild(dxPDFXFDFFields);
    WriteData(procedure(const AName: string; const AValue: Variant)
      var
        AField: TdxXMLNode;
      begin
        AField := AFields.AddChild(dxStringToXMLString(AName));
        AField.TextAsString := AValue;
      end);
    ADocument.SaveToStream(AStream);
  finally
    ADocument.Free;
  end;
end;

procedure TdxPDFFormDataWriter.WriteXFDF(AStream: TStream);
var
  ADocument: TdxXMLDocument;
  AXFDF, AFields: TdxXMLNode;
begin
  ADocument := TdxXMLDocument.Create;
  try
    ADocument.AutoIndent := True;
    AXFDF := ADocument.Root.AddChild(dxPDFXFDF);
    AXFDF.SetAttribute('xmlns', dxPDFXFDFNamespace);
    AFields := AXFDF.AddChild(dxPDFXFDFFields);
    WriteData(procedure(const AName: string; const AValue: Variant)
      var
        AField: TdxXMLNode;
      begin
        AField := AFields.AddChild(dxPDFXFDFField);
        AField.SetAttribute(dxPDFXFDFAttributeName, AName);
        AField.AddChild('value').TextAsString := AValue;
      end);
    ADocument.SaveToStream(AStream);
  finally
    ADocument.Free;
  end;
end;

{ TdxPDFFormDataWriterPair }

class function TdxPDFFormDataWriterPair.Create(const AKey: string;
AData: TdxPDFFormData): TdxPDFFormDataWriterPair;
begin
  Result.Key := AKey;
  Result.Data := AData;
end;

{ TdxFDFCatalog }

constructor TdxFDFCatalog.Create(AData: TdxPDFFormData);
begin
  inherited Create;
  FData := AData;
end;

procedure TdxFDFCatalog.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Catalog);
  ADictionary.Add(TdxPDFKeywords.FDF, CreateDictionary(AHelper, FData, True));
end;

function TdxFDFCatalog.CreateDictionary(AHelper: TdxPDFWriterHelper;
AData: TdxPDFFormData; AOnlyKids: Boolean): TdxPDFDictionary;
var
  AArray: TdxPDFArray;
  ADictionary: TdxPDFDictionary;
  APair: TPair<string, TdxPDFFormData>;
  AValueAsString: string;
begin
  ADictionary := AHelper.CreateDictionary;
  if not AOnlyKids and (AData.Name <> '') then
    ADictionary.Add(TdxPDFKeywords.FDFField, AData.Name);
  if AData.Dictionary.Count > 0 then
  begin
    AArray := AHelper.CreateArray;
    for APair in AData.Dictionary do
      if not APair.Value.IsPasswordField then
        AArray.Add(CreateDictionary(AHelper, APair.Value, False));
    ADictionary.Add(TdxPDFKeywords.Kids, AArray);
  end
  else if not AOnlyKids then
  begin
    AValueAsString := AData.Value;
    ADictionary.Add('V', AValueAsString);
  end;
  Result := ADictionary;
end;

{ TdxFDFWriter }

constructor TdxFDFWriter.Create(AStream: TStream; ACatalog: TdxFDFCatalog);
begin
  inherited Create(AStream);
  FCatalog := ACatalog;
end;

function TdxFDFWriter.GetVersion: string;
begin
  Result := '%FDF-' + dxFDFVersion;
end;

function TdxFDFWriter.HasXRef: Boolean;
begin
  Result := False;
end;

procedure TdxFDFWriter.PopulateTrailer(ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddReference(TdxPDFKeywords.Root, FCatalog);
end;

procedure TdxFDFWriter.RegisterTrailerObjects;
begin
  Helper.RegisterIndirectObject(FCatalog);
end;

end.
