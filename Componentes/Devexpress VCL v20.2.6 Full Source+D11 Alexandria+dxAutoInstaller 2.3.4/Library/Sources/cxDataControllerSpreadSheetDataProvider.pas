{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSpreadSheet }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxDataControllerSpreadSheetDataProvider;

{$I cxVer.Inc}

interface

uses
  Windows, Types, Classes, dxCoreClasses, cxCustomData,
  dxSpreadSheetCoreFormulas, dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreReferences,
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetTypes;

type
  TcxDataControllerSpreadSheetDataProvider = class;

  { TcxDataControllerSpreadSheetCellData }

  TcxDataControllerSpreadSheetCellData = class(TInterfacedObject,
    IdxSpreadSheetCellData) // for internal use only
  strict private
    FDataProvider: TcxDataControllerSpreadSheetDataProvider;
    FItemIndex: Integer;
    FRecordIndex: Integer;
  protected
    // IdxSpreadSheetCellData
    function GetAsError: TdxSpreadSheetFormulaErrorCode;
    function GetAsFloat: Double;
    function GetAsFormula: TObject { TdxSpreadSheetCustomFormula };
    function GetAsVariant: Variant;
    function GetDataType: TdxSpreadSheetCellDataType;
    function GetIsEmpty: Boolean;
    function IsNumericValue: Boolean;
    // methods
    function GetIsError: Boolean;

    // IdxSpreadSheetCellData
    property AsError: TdxSpreadSheetFormulaErrorCode read GetAsError;
    property AsFloat: Double read GetAsFloat;
    property AsFormula: TObject read GetAsFormula;
    property AsVariant: Variant read GetAsVariant;
    property DataType: TdxSpreadSheetCellDataType read GetDataType;
    property IsEmpty: Boolean read GetIsEmpty;
    // properties
    property DataProvider: TcxDataControllerSpreadSheetDataProvider
      read FDataProvider;
    property IsError: Boolean read GetIsError;
    property ItemIndex: Integer read FItemIndex;
    property RecordIndex: Integer read FRecordIndex;
  public
    constructor Create(ADataProvider: TcxDataControllerSpreadSheetDataProvider;
      ARecordIndex, AItemIndex: Integer);
  end;

  { TcxDataControllerSpreadSheetFormulaReferenceToken }

  TcxDataControllerSpreadSheetFormulaReferenceToken = class
    (TdxSpreadSheetFormulaReferenceToken)
  strict private
    FProvider: TcxDataControllerSpreadSheetDataProvider;
  protected
    function ReferenceToString: string; override;
  public
    constructor Create(AProvider: TcxDataControllerSpreadSheetDataProvider;
      AItemIndex: Integer); reintroduce;
  end;

  { TcxDataControllerSpreadSheetFormulaUnknownReferenceToken }

  TcxDataControllerSpreadSheetFormulaUnknownReferenceToken = class
    (TdxSpreadSheetFormulaUnknownToken)
  protected
    procedure ToString(AStack: TStringList); override;
  end;

  { TcxDataControllerSpreadSheetFormulaParser }

  TcxDataControllerSpreadSheetFormulaParser = class
    (TdxSpreadSheetCustomFormulaParser) // for internal use only
  public const
    ItemReferenceStartMark = '[';
    ItemReferenceFinishMark = ']';
  strict private
  const
    StateItemReference = TdxSpreadSheetCustomFormulaParser.StateLast + 1;
  strict private
    FDataProvider: TcxDataControllerSpreadSheetDataProvider;
  protected
    function CreateDefinedNameToken(const AToken
      : TdxSpreadSheetFormulaParserToken;
      out ADefinedNameToken: TdxSpreadSheetFormulaToken;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil): Boolean; override;
    function CreateItemReferenceToken(AItemIndex: Integer)
      : TdxSpreadSheetFormulaReferenceToken; overload; virtual;
    function CreateItemReferenceToken
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken; overload;
    procedure PopulateStateMachine; override;

    property DataProvider: TcxDataControllerSpreadSheetDataProvider
      read FDataProvider;
  public
    constructor Create(ADataProvider: TcxDataControllerSpreadSheetDataProvider);
      reintroduce;
  end;

  { TcxDataControllerSpreadSheetFormulaController }

  TcxDataControllerSpreadSheetFormulaController = class
    (TdxSpreadSheetCustomFormulaController) // for internal use only
  strict private
    FDataProvider: TcxDataControllerSpreadSheetDataProvider;
  protected
    function CreateParser: TObject;
    { TdxSpreadSheetCustomFormulaParser } override;
    function GetFormatSettings: TdxSpreadSheetFormatSettings; override;

    property DataProvider: TcxDataControllerSpreadSheetDataProvider
      read FDataProvider;
  public
    constructor Create(ADataProvider: TcxDataControllerSpreadSheetDataProvider);
  end;

  { TcxDataControllerSpreadSheetFormatSettings }

  TcxDataControllerSpreadSheetFormatSettings = class
    (TdxSpreadSheetFormatSettings) // for internal use only
  protected
    function GetLocaleID: Integer; override;
  end;

  { TcxDataControllerSpreadSheetInvariantFormatSettings }

  TcxDataControllerSpreadSheetInvariantFormatSettings = class
    (TcxDataControllerSpreadSheetFormatSettings) // for internal use only
  protected
    function GetLocaleID: Integer; override;
  end;

  { TcxDataControllerConditionalFormattingReferencesFormatter }

  TcxDataControllerConditionalFormattingReferencesFormatter = class
  protected
    FProvider: TcxDataControllerSpreadSheetDataProvider;
  public
    constructor Create(AProvider: TcxDataControllerSpreadSheetDataProvider);
    function ToString(const AArea: TRect): string; reintroduce;
      overload; virtual;
    function ToString(const AColumn: Integer): string; reintroduce;
      overload; virtual;
  end;

  { TcxDataControllerSpreadSheetDataProvider }

  TcxDataControllerSpreadSheetDataProvider = class(TcxIUnknownObject,
    IdxSpreadSheetViewData) // for internal use only
  strict private
    class var FInvariantFormatSettings
      : TcxDataControllerSpreadSheetInvariantFormatSettings;
  strict private
    FDataController: TcxCustomDataController;
    FSavedFormatSettings: TcxDataControllerSpreadSheetFormatSettings;
  protected
    FFormatSettings: TcxDataControllerSpreadSheetFormatSettings;
    FReferencesFormatter
      : TcxDataControllerConditionalFormattingReferencesFormatter;

    function ReferenceToString(AColumn: Integer): string; overload;
    function ReferenceToString(const AArea: TRect): string; overload;

    procedure RollbackFormatSettings; virtual;
    procedure SaveFormatSettings; virtual;

    procedure ForEachCell(const AArea: TRect;
      AProc: TdxSpreadSheetViewForEachCellProc;
      AGoForward: Boolean = True); virtual;
    function GetCellData(const ARow, AColumn: Integer)
      : IdxSpreadSheetCellData; virtual;
    function GetMaxColumnIndex: Integer; virtual;
    function GetMaxRowIndex: Integer; virtual;
    function GetNextColumnWithNonEmptyCell(const ARow, AColumn: Integer;
      const AGoForward: Boolean = True): Integer; virtual;
    function GetNextRowWithNonEmptyCell(const ARow, AColumn: Integer;
      const AGoForward: Boolean = True): Integer; virtual;
    function IsRowVisible(const ARow: Integer): Boolean; virtual;
    procedure SetCellData(const ARow, AColumn: Integer; const AValue: Variant;
      const AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    function CreateFormatSettings
      : TcxDataControllerSpreadSheetFormatSettings; virtual;
    function GetErrorCode(ARecordIndex, AItemIndex: Integer)
      : TdxSpreadSheetFormulaErrorCode; virtual;
    function GetItemByDisplayName(const AValue: string): Integer; virtual;
    function GetItemDisplayName(AItemIndex: Integer): string; overload;
    function GetItemDisplayName(AItem: TObject): string; overload;
      virtual; abstract;
    function GetValue(ARecordIndex, AItemIndex: Integer): Variant; virtual;
    function IsIndependent: Boolean; virtual;

    class procedure Initialize; static;
    class procedure Finalize; static;

    class property InvariantFormatSettings
      : TcxDataControllerSpreadSheetInvariantFormatSettings
      read FInvariantFormatSettings;
  public
    constructor Create(ADataController: TcxCustomDataController); virtual;
    destructor Destroy; override;

    property DataController: TcxCustomDataController read FDataController;
    property FormatSettings: TcxDataControllerSpreadSheetFormatSettings
      read FFormatSettings;
  end;

implementation

uses
  Variants, SysUtils, Math, dxCore, dxStringHelper, cxGeometry, cxVariants,
  dxSpreadSheetNumberFormat, dxSpreadSheetUtils;

{ TcxDataControllerSpreadSheetCellData }

constructor TcxDataControllerSpreadSheetCellData.Create(ADataProvider
  : TcxDataControllerSpreadSheetDataProvider;
  ARecordIndex, AItemIndex: Integer);
begin
  inherited Create;
  FDataProvider := ADataProvider;
  FRecordIndex := ARecordIndex;
  FItemIndex := AItemIndex;
end;

function TcxDataControllerSpreadSheetCellData.GetAsError
  : TdxSpreadSheetFormulaErrorCode;
begin
  Result := DataProvider.GetErrorCode(RecordIndex, ItemIndex);
end;

function TcxDataControllerSpreadSheetCellData.GetAsFloat: Double;
begin
  Result := AsVariant;
end;

function TcxDataControllerSpreadSheetCellData.GetAsFormula
  : TObject { TdxSpreadSheetCustomFormula };
begin
  Result := nil;
end;

function TcxDataControllerSpreadSheetCellData.GetAsVariant: Variant;
begin
  Result := DataProvider.GetValue(RecordIndex, ItemIndex);
end;

function TcxDataControllerSpreadSheetCellData.GetDataType
  : TdxSpreadSheetCellDataType;
begin
  if IsError then
    Result := cdtError
  else if IsEmpty then
    Result := cdtBlank
  else
    Result := dxGetDataTypeByVariantValue(AsVariant);
end;

function TcxDataControllerSpreadSheetCellData.GetIsEmpty: Boolean;
var
  AValue: Variant;
begin
  AValue := AsVariant;
  Result := dxSpreadSheetIsNullValue(AValue);
end;

function TcxDataControllerSpreadSheetCellData.IsNumericValue: Boolean;
var
  AFloatValue: Single;
  AVariant: Variant;
begin
  AVariant := AsVariant;
  Result := VarIsNumericEx(AVariant) or VarIsDate(AVariant) or
    VarIsStr(AVariant) and TryStrToFloat(VarToStr(AVariant), AFloatValue);
end;

function TcxDataControllerSpreadSheetCellData.GetIsError: Boolean;
begin
  Result := AsError <> ecNone;
end;

{ TcxDataControllerSpreadSheetFormulaReferenceToken }

constructor TcxDataControllerSpreadSheetFormulaReferenceToken.Create
  (AProvider: TcxDataControllerSpreadSheetDataProvider; AItemIndex: Integer);
begin
  inherited Create(TdxSpreadSheetReference.Create(0, False),
    TdxSpreadSheetReference.Create(AItemIndex, True));
  FProvider := AProvider;
end;

function TcxDataControllerSpreadSheetFormulaReferenceToken.
  ReferenceToString: string;
begin
  Result := FProvider.ReferenceToString(Column.Offset);
end;

{ TcxDataControllerSpreadSheetFormulaUnknownReferenceToken }

procedure TcxDataControllerSpreadSheetFormulaUnknownReferenceToken.ToString
  (AStack: TStringList);
begin
  AStack.Add(Format('%s%s%s',
    [TcxDataControllerSpreadSheetFormulaParser.ItemReferenceStartMark, Name,
    TcxDataControllerSpreadSheetFormulaParser.ItemReferenceFinishMark]));
end;

{ TcxDataControllerSpreadSheetFormulaParser }

constructor TcxDataControllerSpreadSheetFormulaParser.Create
  (ADataProvider: TcxDataControllerSpreadSheetDataProvider);
begin
  inherited Create;
  FDataProvider := ADataProvider;
  FormatSettings.Assign(ADataProvider.FormatSettings);
end;

function TcxDataControllerSpreadSheetFormulaParser.CreateDefinedNameToken
  (const AToken: TdxSpreadSheetFormulaParserToken;
  out ADefinedNameToken: TdxSpreadSheetFormulaToken;
  ALink: TdxSpreadSheet3DReferenceCustomLink = nil): Boolean;
begin
  ADefinedNameToken := CreateUnknownNameToken(AToken, ALink);
  Result := True;
end;

function TcxDataControllerSpreadSheetFormulaParser.CreateItemReferenceToken
  (AItemIndex: Integer): TdxSpreadSheetFormulaReferenceToken;
begin
  Result := TcxDataControllerSpreadSheetFormulaReferenceToken.Create
    (FDataProvider, AItemIndex);
end;

function TcxDataControllerSpreadSheetFormulaParser.CreateItemReferenceToken
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AItemIndex: Integer;
  AToken: TdxSpreadSheetFormulaParserToken;
  AName: string;
begin
  AToken := AStack.Pop;

  AName := AToken.GetString(FFormulaSourceText);
  AItemIndex := DataProvider.GetItemByDisplayName(AName);
  if AItemIndex >= 0 then
    Result := CreateItemReferenceToken(AItemIndex)
  else
    Result := TcxDataControllerSpreadSheetFormulaUnknownReferenceToken.
      Create(AName);

  InitializeSourceBounds(Result, AToken.StartPosition -
    Length(ItemReferenceStartMark), AToken.FinishPosition +
    Length(ItemReferenceStartMark));
end;

procedure TcxDataControllerSpreadSheetFormulaParser.PopulateStateMachine;
var
  AState: TdxSpreadSheetFormulaParserState;
begin
  inherited;
  AState := StateMachine.GetState(StateStart);
  AState.RemoveTokenController(IsReference);
  AState.SetNextState(TokenExternalReference, StateItemReference);

  AState := StateMachine.CreateState(StateItemReference);
  AState.SetTokenCreator(CreateItemReferenceToken);
end;

{ TcxDataControllerSpreadSheetFormulaController }

constructor TcxDataControllerSpreadSheetFormulaController.Create
  (ADataProvider: TcxDataControllerSpreadSheetDataProvider);
begin
  inherited Create;
  FDataProvider := ADataProvider;
end;

function TcxDataControllerSpreadSheetFormulaController.CreateParser: TObject;
begin
  Result := TcxDataControllerSpreadSheetFormulaParser.Create(FDataProvider);
end;

function TcxDataControllerSpreadSheetFormulaController.GetFormatSettings
  : TdxSpreadSheetFormatSettings;
begin
  Result := DataProvider.FormatSettings;
end;

{ TcxDataControllerSpreadSheetFormatSettings }

function TcxDataControllerSpreadSheetFormatSettings.GetLocaleID: Integer;
begin
  Result := GetThreadLocale;
end;

{ TcxDataControllerSpreadSheetInvariantFormatSettings }

function TcxDataControllerSpreadSheetInvariantFormatSettings.
  GetLocaleID: Integer;
begin
  Result := dxGetInvariantLocaleID;
end;

{ TcxDataControllerConditionalFormattingReferencesFormatter }

constructor TcxDataControllerConditionalFormattingReferencesFormatter.Create
  (AProvider: TcxDataControllerSpreadSheetDataProvider);
begin
  inherited Create;
  FProvider := AProvider;
end;

function TcxDataControllerConditionalFormattingReferencesFormatter.ToString
  (const AColumn: Integer): string;
begin
  Result := Format('[%s]', [ToString(cxRect(AColumn, 0, AColumn, 0))]);
end;

function TcxDataControllerConditionalFormattingReferencesFormatter.ToString
  (const AArea: TRect): string;
var
  AReferenceName: string;
  ABuffer: TStringBuilder;
  I, AStart, ARight: Integer;
begin
  ARight := Min(FProvider.DataController.ItemCount - 1, AArea.Right);
  AStart := Max(0, AArea.Left);
  if AStart <= ARight then
  begin
    ABuffer := TdxStringBuilderManager.Get;
    try
      for I := AStart to ARight do
      begin
        AReferenceName := FProvider.GetItemDisplayName(I);
        if ABuffer.Length > 0 then
          ABuffer.Append(', ');
        ABuffer.Append(AReferenceName);
      end;
      Result := ABuffer.ToString;
    finally
      TdxStringBuilderManager.Release(ABuffer);
    end;
  end
  else
    Result := '';
end;

{ TcxDataControllerSpreadSheetDataProvider }

constructor TcxDataControllerSpreadSheetDataProvider.Create(ADataController
  : TcxCustomDataController);
begin
  inherited Create;
  FDataController := ADataController;
  FReferencesFormatter :=
    TcxDataControllerConditionalFormattingReferencesFormatter.Create(Self);
  if IsIndependent then
    FFormatSettings := CreateFormatSettings;
end;

destructor TcxDataControllerSpreadSheetDataProvider.Destroy;
begin
  FreeAndNil(FReferencesFormatter);
  if IsIndependent then
    FreeAndNil(FFormatSettings);
  inherited Destroy;
end;

function TcxDataControllerSpreadSheetDataProvider.CreateFormatSettings
  : TcxDataControllerSpreadSheetFormatSettings;
begin
  Result := TcxDataControllerSpreadSheetFormatSettings.Create;
end;

function TcxDataControllerSpreadSheetDataProvider.GetErrorCode(ARecordIndex,
  AItemIndex: Integer): TdxSpreadSheetFormulaErrorCode;
var
  AError: Integer;
begin
  AError := DataController.ErrorCodes[ARecordIndex, AItemIndex];
  Result := TdxSpreadSheetFormulaErrorCode(AError);
end;

function TcxDataControllerSpreadSheetDataProvider.GetItemByDisplayName
  (const AValue: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to DataController.GetItemCount - 1 do
    if AnsiSameText(GetItemDisplayName(I), AValue) then
      Exit(I);
end;

function TcxDataControllerSpreadSheetDataProvider.GetItemDisplayName
  (AItemIndex: Integer): string;
var
  AItem: TObject;
begin
  AItem := DataController.GetItem(AItemIndex);
  Result := GetItemDisplayName(AItem);
end;

function TcxDataControllerSpreadSheetDataProvider.GetValue(ARecordIndex,
  AItemIndex: Integer): Variant;
begin
  Result := DataController.Values[ARecordIndex, AItemIndex];
end;

function TcxDataControllerSpreadSheetDataProvider.IsIndependent: Boolean;
begin
  Result := True;
end;

class procedure TcxDataControllerSpreadSheetDataProvider.Initialize;
begin
  FInvariantFormatSettings :=
    TcxDataControllerSpreadSheetInvariantFormatSettings.Create;
end;

class procedure TcxDataControllerSpreadSheetDataProvider.Finalize;
begin
  FreeAndNil(FInvariantFormatSettings);
end;

function TcxDataControllerSpreadSheetDataProvider.ReferenceToString
  (AColumn: Integer): string;
begin
  Result := FReferencesFormatter.ToString(AColumn);
end;

function TcxDataControllerSpreadSheetDataProvider.ReferenceToString
  (const AArea: TRect): string;
begin
  Result := FReferencesFormatter.ToString(AArea);
end;

procedure TcxDataControllerSpreadSheetDataProvider.RollbackFormatSettings;
begin
  FFormatSettings := FSavedFormatSettings;
  FSavedFormatSettings := nil;
end;

procedure TcxDataControllerSpreadSheetDataProvider.SaveFormatSettings;
begin
  FSavedFormatSettings := FFormatSettings;
end;

procedure TcxDataControllerSpreadSheetDataProvider.ForEachCell
  (const AArea: TRect; AProc: TdxSpreadSheetViewForEachCellProc;
  AGoForward: Boolean);
begin
  // do nothing
end;

function TcxDataControllerSpreadSheetDataProvider.GetCellData(const ARow,
  AColumn: Integer): IdxSpreadSheetCellData;
begin
  Result := TcxDataControllerSpreadSheetCellData.Create(Self, ARow, AColumn);
end;

function TcxDataControllerSpreadSheetDataProvider.GetMaxColumnIndex: Integer;
begin
  Result := DataController.ItemCount - 1;
end;

function TcxDataControllerSpreadSheetDataProvider.GetMaxRowIndex: Integer;
begin
  Result := DataController.RecordCount - 1;
end;

function TcxDataControllerSpreadSheetDataProvider.GetNextColumnWithNonEmptyCell
  (const ARow, AColumn: Integer; const AGoForward: Boolean): Integer;
begin
  Result := MaxInt;
end;

function TcxDataControllerSpreadSheetDataProvider.GetNextRowWithNonEmptyCell
  (const ARow, AColumn: Integer; const AGoForward: Boolean): Integer;
begin
  Result := MaxInt;
end;

function TcxDataControllerSpreadSheetDataProvider.IsRowVisible
  (const ARow: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxDataControllerSpreadSheetDataProvider.SetCellData(const ARow,
  AColumn: Integer; const AValue: Variant;
  const AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
end;

initialization

TcxDataControllerSpreadSheetDataProvider.Initialize;

finalization

TcxDataControllerSpreadSheetDataProvider.Finalize;

end.
