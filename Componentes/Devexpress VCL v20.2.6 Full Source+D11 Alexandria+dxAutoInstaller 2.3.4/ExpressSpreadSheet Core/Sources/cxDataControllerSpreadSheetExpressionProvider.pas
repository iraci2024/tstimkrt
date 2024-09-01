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

unit cxDataControllerSpreadSheetExpressionProvider;

{$I cxVer.Inc}

interface

uses
  Windows, dxCoreClasses, cxCustomData, dxSpreadSheetCoreFormulas,
  dxSpreadSheetClasses, dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetTypes,
  cxDataControllerSpreadSheetDataProvider;

type
  TcxDataControllerSpreadSheetExpressionDataProvider = class;

  { IcxDataControllerSpreadSheetExpressionItem }

  IcxDataControllerSpreadSheetExpressionItem = interface
  // for internal use only
    ['{58B7DD4A-B654-44FE-B91C-5BDAEB28F6F6}']
    function CanUseInExpression: Boolean;
    function GetReferenceName: string;
  end;

  { TcxDataControllerSpreadSheetExpressionFormulaParser }

  TcxDataControllerSpreadSheetExpressionFormulaParser = class
    (TcxDataControllerSpreadSheetFormulaParser)
  protected
    procedure PopulateKnownOperations; override;
    procedure PopulateStateMachine; override;
  end;

  { TcxDataControllerSpreadSheetExpressionFormulaController }

  TcxDataControllerSpreadSheetExpressionFormulaController = class
    (TcxDataControllerSpreadSheetFormulaController)
  protected
    function CreateParser: TObject; override;
  end;

  { TcxDataControllerSpreadSheetExpressionFormula }

  TcxDataControllerSpreadSheetExpressionFormula = class
    (TdxSpreadSheetCustomFormula) // for internal use only
  strict private
    FController: TcxDataControllerSpreadSheetFormulaController;
    FDataProvider: TcxDataControllerSpreadSheetExpressionDataProvider;
  protected
    function CalculateByRecord(ARecordIndex: Integer;
      out AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant; virtual;
    function CreateController
      : TcxDataControllerSpreadSheetFormulaController; virtual;
    function GetController: TdxSpreadSheetCustomFormulaController; override;
    function GetView: TObject; override;

    property DataProvider: TcxDataControllerSpreadSheetExpressionDataProvider
      read FDataProvider;
  public
    constructor Create(ADataProvider
      : TcxDataControllerSpreadSheetExpressionDataProvider); virtual;
    destructor Destroy; override;
  end;

  { TcxDataControllerSpreadSheetExpressionDataProvider }

  TcxDataControllerSpreadSheetExpressionDataProvider = class
    (TcxDataControllerSpreadSheetDataProvider)
  strict private
    FActiveRow: Integer;
  protected
    function CanUseItemInExpression(AItemIndex: Integer): Boolean; virtual;
    function GetCellData(const ARow: Integer; const AColumn: Integer)
      : IdxSpreadSheetCellData; override;
    function GetAsExpressionItem(AItem: TObject)
      : IcxDataControllerSpreadSheetExpressionItem;
    function GetItemDisplayName(AItem: TObject): string; override;

    property ActiveRow: Integer read FActiveRow write FActiveRow;
  end;

  { TcxDataControllerSpreadSheetExpressionParser }

  TcxDataControllerSpreadSheetExpressionParser = class
    (TcxDataCustomExpressionParser)
  public
    procedure Parse(const AExpression: string; AFormula: TObject); override;
  end;

  { TcxDataControllerSpreadSheetExpressionCalculator }

  TcxDataControllerSpreadSheetExpressionCalculator = class
    (TcxDataCustomExpressionCalculator) // for internal use only
  protected
    function Calculate(AFormula: TObject; ARecordIndex: Integer;
      out AErrorCode: Integer): Variant; override;
  end;

  { TcxDataControllerSpreadSheetExpressionProvider }

  TcxDataControllerSpreadSheetExpressionProvider = class
    (TcxDataCustomExpressionProvider)
  strict private
    FDataProvider: TcxDataControllerSpreadSheetExpressionDataProvider;
    function GetParser: TcxDataControllerSpreadSheetExpressionParser; inline;
  protected
    function ConvertExpression(const AExpression: string;
      AFrom, ATo: TcxDataControllerSpreadSheetFormatSettings): string;
    function CreateCalculator: TcxDataCustomExpressionCalculator; override;
    function CreateDataProvider
      : TcxDataControllerSpreadSheetExpressionDataProvider; virtual;
    function CreateParser: TcxDataCustomExpressionParser; override;
    function CreateFormula: TObject; override;
    function ErrorCodeToString(ACode: Integer): string; override;
    function GetVarCastErrorCode: Integer; override;

    property DataProvider: TcxDataControllerSpreadSheetExpressionDataProvider
      read FDataProvider;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    destructor Destroy; override;

    function CanUseItemInExpression(AItemIndex: Integer): Boolean; override;
    procedure EditExpression(AItemIndex: Integer); override;
    procedure EditExpression(var AExpression: string); override;
    function ExpressionToInvariantExpression(const AExpression: string)
      : string; override;
    function GetItemReferenceName(AItemIndex: Integer): string; override;
    function InvariantExpressionToExpression(const AExpression: string)
      : string; override;

    property Parser: TcxDataControllerSpreadSheetExpressionParser
      read GetParser;
  end;

implementation

uses
  Variants, SysUtils, cxVariants, dxSpreadSheetNumberFormat, dxSpreadSheetUtils,
  dxExpressionEditor;

{ TcxDataControllerSpreadSheetExpressionFormulaParser }

procedure TcxDataControllerSpreadSheetExpressionFormulaParser.
  PopulateKnownOperations;
begin
  inherited PopulateKnownOperations;
  FKnownOperations.Remove(Ord(opRange));
  FKnownOperations.Remove(Ord(opIsect));
end;

procedure TcxDataControllerSpreadSheetExpressionFormulaParser.
  PopulateStateMachine;
var
  AState: TdxSpreadSheetFormulaParserState;
begin
  inherited PopulateStateMachine;
  AState := StateMachine.GetState(StateStart);
  AState.RemoveTokenController(IsArray);
end;

{ TcxDataControllerSpreadSheetExpressionFormulaController }

function TcxDataControllerSpreadSheetExpressionFormulaController.
  CreateParser: TObject;
begin
  Result := TcxDataControllerSpreadSheetExpressionFormulaParser.Create
    (DataProvider);
end;

{ TcxDataControllerSpreadSheetExpressionFormula }

constructor TcxDataControllerSpreadSheetExpressionFormula.Create
  (ADataProvider: TcxDataControllerSpreadSheetExpressionDataProvider);
begin
  inherited Create;
  FDataProvider := ADataProvider;
  FController := CreateController;
  Controller.Add(Self);
end;

destructor TcxDataControllerSpreadSheetExpressionFormula.Destroy;
begin
  FreeAndNil(FController);
  inherited Destroy;
end;

function TcxDataControllerSpreadSheetExpressionFormula.CalculateByRecord
  (ARecordIndex: Integer;
  out AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
begin
  if IsCalculationInProcess and ResultValue.HasCircularReferences then
    ResultValue.SetError(ecValue)
  else
  begin
    DataProvider.ActiveRow := ARecordIndex;
    Controller.Calculate;
  end;
  Result := Value;
  AErrorCode := ActualErrorCode;
end;

function TcxDataControllerSpreadSheetExpressionFormula.CreateController
  : TcxDataControllerSpreadSheetFormulaController;
begin
  Result := TcxDataControllerSpreadSheetExpressionFormulaController.Create
    (DataProvider);
end;

function TcxDataControllerSpreadSheetExpressionFormula.GetController
  : TdxSpreadSheetCustomFormulaController;
begin
  Result := FController;
end;

function TcxDataControllerSpreadSheetExpressionFormula.GetView: TObject;
begin
  Result := DataProvider;
end;

{ TcxDataControllerSpreadSheetExpressionDataProvider }

function TcxDataControllerSpreadSheetExpressionDataProvider.
  CanUseItemInExpression(AItemIndex: Integer): Boolean;
var
  AItem: TObject;
  AExpressionItem: IcxDataControllerSpreadSheetExpressionItem;
begin
  AItem := DataController.GetItem(AItemIndex);
  AExpressionItem := GetAsExpressionItem(AItem);
  Result := (AExpressionItem <> nil) and AExpressionItem.CanUseInExpression;
end;

function TcxDataControllerSpreadSheetExpressionDataProvider.GetCellData
  (const ARow: Integer; const AColumn: Integer): IdxSpreadSheetCellData;
begin
  Result := inherited GetCellData(ActiveRow, AColumn);
end;

function TcxDataControllerSpreadSheetExpressionDataProvider.GetAsExpressionItem
  (AItem: TObject): IcxDataControllerSpreadSheetExpressionItem;
begin
  Supports(AItem, IcxDataControllerSpreadSheetExpressionItem, Result);
end;

function TcxDataControllerSpreadSheetExpressionDataProvider.GetItemDisplayName
  (AItem: TObject): string;
var
  AExpressionItem: IcxDataControllerSpreadSheetExpressionItem;
begin
  AExpressionItem := GetAsExpressionItem(AItem);
  if AExpressionItem <> nil then
    Result := AExpressionItem.GetReferenceName
  else
    Result := '';
end;

{ TcxDataControllerSpreadSheetExpressionParser }

procedure TcxDataControllerSpreadSheetExpressionParser.Parse(const AExpression
  : string; AFormula: TObject);
begin
  dxSpreadSheetParseReference(AExpression, AFormula);
end;

{ TcxDataControllerSpreadSheetExpressionCalculator }

function TcxDataControllerSpreadSheetExpressionCalculator.Calculate
  (AFormula: TObject; ARecordIndex: Integer; out AErrorCode: Integer): Variant;
var
  ASpreadSheetErrorCode: TdxSpreadSheetFormulaErrorCode;
  AExpressionFormula: TcxDataControllerSpreadSheetExpressionFormula
    absolute AFormula;
begin
  Result := AExpressionFormula.CalculateByRecord(ARecordIndex,
    ASpreadSheetErrorCode);
  AErrorCode := Integer(ASpreadSheetErrorCode);
end;

{ TcxDataControllerSpreadSheetExpressionProvider }

constructor TcxDataControllerSpreadSheetExpressionProvider.Create
  (ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
  FDataProvider := CreateDataProvider;
end;

destructor TcxDataControllerSpreadSheetExpressionProvider.Destroy;
begin
  FreeAndNil(FDataProvider);
  inherited Destroy;
end;

function TcxDataControllerSpreadSheetExpressionProvider.CanUseItemInExpression
  (AItemIndex: Integer): Boolean;
begin
  Result := DataProvider.CanUseItemInExpression(AItemIndex);
end;

procedure TcxDataControllerSpreadSheetExpressionProvider.EditExpression
  (AItemIndex: Integer);
begin
  ShowExpressionEditor(DataController, AItemIndex);
end;

procedure TcxDataControllerSpreadSheetExpressionProvider.EditExpression
  (var AExpression: string);
begin
  ShowExpressionEditor(DataController, AExpression);
end;

function TcxDataControllerSpreadSheetExpressionProvider.
  ExpressionToInvariantExpression(const AExpression: string): string;
begin
  Result := ConvertExpression(AExpression, DataProvider.FormatSettings,
    DataProvider.InvariantFormatSettings);
end;

function TcxDataControllerSpreadSheetExpressionProvider.GetItemReferenceName
  (AItemIndex: Integer): string;
begin
  Result := DataProvider.GetItemDisplayName(AItemIndex);
end;

function TcxDataControllerSpreadSheetExpressionProvider.
  InvariantExpressionToExpression(const AExpression: string): string;
begin
  Result := ConvertExpression(AExpression, DataProvider.InvariantFormatSettings,
    DataProvider.FormatSettings);
end;

function TcxDataControllerSpreadSheetExpressionProvider.ConvertExpression
  (const AExpression: string;
  AFrom, ATo: TcxDataControllerSpreadSheetFormatSettings): string;
var
  AEQ: string;
  AHasEQ: Boolean;
  AFormula: TcxDataControllerSpreadSheetExpressionFormula;
begin
  Result := AExpression;
  if Result = '' then
    Exit;
  AFormula := TcxDataControllerSpreadSheetExpressionFormula(CreateFormula);
  try
    DataProvider.SaveFormatSettings;
    DataProvider.FFormatSettings := AFrom;
    try
      AEQ := DataProvider.FormatSettings.Operations[opEQ];
      AHasEQ := AnsiPos(AEQ, Result) = 1;
      Parser.Parse(Result, AFormula);
    finally
      DataProvider.RollbackFormatSettings;
    end;
    DataProvider.SaveFormatSettings;
    DataProvider.FFormatSettings := ATo;
    try
      Result := AFormula.AsText;
      AEQ := DataProvider.FormatSettings.Operations[opEQ];
      if not AHasEQ and (AnsiPos(AEQ, Result) = 1) then
        Delete(Result, 1, Length(AEQ));
    finally
      DataProvider.RollbackFormatSettings;
    end;
  finally
    AFormula.Free;
  end;
end;

function TcxDataControllerSpreadSheetExpressionProvider.CreateCalculator
  : TcxDataCustomExpressionCalculator;
begin
  Result := TcxDataControllerSpreadSheetExpressionCalculator.Create;
end;

function TcxDataControllerSpreadSheetExpressionProvider.CreateDataProvider
  : TcxDataControllerSpreadSheetExpressionDataProvider;
begin
  Result := TcxDataControllerSpreadSheetExpressionDataProvider.Create
    (DataController);
end;

function TcxDataControllerSpreadSheetExpressionProvider.CreateParser
  : TcxDataCustomExpressionParser;
begin
  Result := TcxDataControllerSpreadSheetExpressionParser.Create;
end;

function TcxDataControllerSpreadSheetExpressionProvider.CreateFormula: TObject;
begin
  Result := TcxDataControllerSpreadSheetExpressionFormula.Create(DataProvider);
end;

function TcxDataControllerSpreadSheetExpressionProvider.ErrorCodeToString
  (ACode: Integer): string;
begin
  Result := dxSpreadSheetErrorCodeToString
    (TdxSpreadSheetFormulaErrorCode(ACode));
end;

function TcxDataControllerSpreadSheetExpressionProvider.
  GetVarCastErrorCode: Integer;
begin
  Result := Integer(ecValue);
end;

function TcxDataControllerSpreadSheetExpressionProvider.GetParser
  : TcxDataControllerSpreadSheetExpressionParser;
begin
  Result := TcxDataControllerSpreadSheetExpressionParser(inherited Parser);
end;

end.
