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

unit dxExpressionEditorHelpers;

{$I cxVer.Inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, Contnrs, Graphics, Generics.Collections,
  Generics.Defaults, Controls, cxClasses, dxCore,
  cxDataControllerSpreadSheetDataProvider, dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetTypes, dxSpreadSheetFunctions, dxExpressionRichEdit, cxRichEdit,
  dxMessages,
  cxDataControllerSpreadSheetExpressionProvider, cxCustomData, dxCoreGraphics,
  cxLookAndFeelPainters,
  dxSpreadSheetCoreFormulasParser;

type
  { TdxExpressionOperatorInfo }

  TdxExpressionOperatorInfo = class
  strict private
    FDescriptionPtr: TcxResourceStringID;
    FCaption: string;
    FOperation: TdxSpreadSheetFormulaOperation;
  public
    constructor Create(const AOperation: TdxSpreadSheetFormulaOperation;
      const ACaption: string; const ADescriptionPtr: TcxResourceStringID);
    property DescriptionPtr: TcxResourceStringID read FDescriptionPtr;
    property Caption: string read FCaption;
    property Operation: TdxSpreadSheetFormulaOperation read FOperation;
  end;

  { TdxExpressionOperatorInfosRepository }

  TdxExpressionOperatorInfosRepository = class
  strict private
    FItems: TcxObjectList;
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): TdxExpressionOperatorInfo; inline;
  protected
    function GetInfoByOperation(const AOperation
      : TdxSpreadSheetFormulaOperation): TdxExpressionOperatorInfo;
    procedure Register(const AOperation: TdxSpreadSheetFormulaOperation;
      const ACaption: string; const ADescriptionPtr: TcxResourceStringID);
    procedure Unregister(const AOperation: TdxSpreadSheetFormulaOperation);
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxExpressionOperatorInfo read GetItem;
  end;

  { TdxExpressionConstantInfo }

  TdxExpressionConstantInfo = class
  strict private
    FData: TdxSpreadSheetFunctionInfo;
  public
    constructor Create(AData: TdxSpreadSheetFunctionInfo);

    property Data: TdxSpreadSheetFunctionInfo read FData;
  end;

  { TdxExpressionConstantInfosRepository }

  TdxExpressionConstantInfosRepository = class
  strict private
    FItems: TcxObjectList;
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): TdxExpressionConstantInfo; inline;
  protected
    procedure Register(AInfo: TdxSpreadSheetFunctionInfo);
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxExpressionConstantInfo read GetItem;
  end;

  { TdxExpressionRichEditIterator }

  TdxExpressionRichEditIterator = class
  public const
    BreakChars = ' [](){}!@#$%^&*-`~''";:,.?/\|+=<>'#13#10;
  strict private
    FRichEdit: TdxExpressionRichEdit;

    function IsBreakChar(AChar: Char): Boolean;
    function GetText: string; inline;
  protected
    function IsBackwardBreakChar(APos: Integer): Boolean; virtual;
    function IsForwardBreakChar(APos: Integer): Boolean; virtual;

    property Text: string read GetText;
  public
    constructor Create(ARichEdit: TdxExpressionRichEdit);

    function GetBackwardPosition(AStart: Integer = -1): Integer;
    function GetForwardPosition(AStart: Integer = -1): Integer;

    function GetFilterText(AStart: Integer = -1): string; virtual;
  end;

  { TdxExpressionRichEditFieldIterator }

  TdxExpressionRichEditFieldIterator = class(TdxExpressionRichEditIterator)
  protected
    function IsBackwardBreakChar(APos: Integer): Boolean; override;
    function IsForwardBreakChar(APos: Integer): Boolean; override;
  public
    function GetFilterText(AStart: Integer = -1): string; override;
  end;

  { TdxExpressionRichEditOperatorIterator }

  TdxExpressionRichEditOperatorIterator = class(TdxExpressionRichEditIterator)
  strict private
  const
    BreakChars = '!@#$&(){}[]\|,.?';
  protected
    function IsBackwardBreakChar(APos: Integer): Boolean; override;
    function IsForwardBreakChar(APos: Integer): Boolean; override;
  end;

  { TdxExpressionContextParser }

  TdxExpressionContextParserContextType = (Unknown, Field, &Function,
    &Operator, Value);

  TdxExpressionContextCustomParser = class
    (TcxDataControllerSpreadSheetExpressionFormulaParser)
  strict private
    FInternalErrorIndex: Integer;
  protected
    function CreateFormula: TcxDataControllerSpreadSheetExpressionFormula;
    function IsFormulaText(const S: string; out AOffset: Integer)
      : Boolean; override;
    procedure SetErrorIndex(AErrorIndex: Integer;
      AErrorCode: TdxSpreadSheetFormulaParserErrorCode); override;

    property InternalErrorIndex: Integer read FInternalErrorIndex;
  public
    function GetContextType(const AFormulaText: string; APos: Integer)
      : TdxExpressionContextParserContextType;
  end;

  { TdxExpressionContextParser }

  TdxExpressionContextParser = class(TdxExpressionContextCustomParser)
  protected
    function ValidateTokens(AToken: TdxSpreadSheetFormulaToken)
      : Boolean; override;
  end;

  { TdxExpressionValidator }

  TdxExpressionValidatorErrorCode = (None, MissingClosingFieldMark,
    MissingClosingStringMark, MissingClosingExpressionMark, InvalidExpression,
    UnknownFieldName, UnknownFunctionName);

  TdxExpressionValidator = class(TdxExpressionContextCustomParser)
  public
    function Validate(const AFormulaText: string;
      out AErrorCode: TdxExpressionValidatorErrorCode;
      out AStart, AFinish: Integer): Boolean;
  end;

  { TdxExpressionHighlighter }

  TdxExpressionHighlighter = class
  strict private
  const
    DefaultColorField = $605EEB;
    DefaultColorFunction = $ED8C5F;
    DefaultColorValue = $39962D;
    BreakChars = ' [](){}!@#$%^&*-`~''";:,?/\|+=<>'#13#10;
  strict private
  type
{$REGION 'Internal Types'}
    TTokenInfo = class
    protected
      Position: Integer;
      Length: Integer;
      &Type: TdxExpressionContextParserContextType;
    end;
{$ENDREGION}
  strict private
    FFieldColor: TColor;
    FFunctionColor: TColor;
    FTextColor: TColor;
    FTokens: TObjectList;
    FValueColor: TColor;
  protected
    procedure AddToken(const AStartIndex, AFinishIndex: Integer;
      AType: TdxExpressionContextParserContextType);
    function CanApplyDefaultTextColor: Boolean; virtual;
    procedure CalculateTokens(const AText: string); virtual;
    function GetColor(AType: TdxExpressionContextParserContextType)
      : TColor; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply(AEdit: TcxCustomRichEdit);
    procedure UpdateColors(ATextColor: TColor);

    property TextColor: TColor read FTextColor;
    property FieldColor: TColor read FFieldColor;
    property FunctionColor: TColor read FFunctionColor;
    property ValueColor: TColor read FValueColor;
  end;

  { TdxExpressionErrorHighlighter }

  TdxExpressionErrorHighlighter = class(TdxExpressionHighlighter)
  public const
    DefaultErrorColor = clRed;
  strict private
    FFinish: Integer;
    FStart: Integer;
  protected
    function CanApplyDefaultTextColor: Boolean; override;
    procedure CalculateTokens(const AText: string); override;
    function GetColor(AType: TdxExpressionContextParserContextType)
      : TColor; override;
  public
    constructor Create(AStart, AFinish: Integer); reintroduce;
  end;

  { TdxCustomExpressionEditorController }

  TdxCustomExpressionEditorController = class
  public type
    TEnumFunctionsProc = reference to procedure
      (AInfo: TdxSpreadSheetFunctionInfo);
    TEnumFieldsProc = reference to procedure(AItemIndex: Integer);
    TEnumOperatorsProc = reference to procedure
      (AInfo: TdxExpressionOperatorInfo);
    TEnumConstantsProc = reference to procedure
      (AInfo: TdxExpressionConstantInfo);
  strict private
    FContextParser: TdxExpressionContextParser;
    FExpressionOwner: TObject;
    FHighlighter: TdxExpressionHighlighter;
    FImageColorPaletteForSelected: IdxColorPalette;
    FImageColorPalettes: array [TdxExpressionContextParserContextType]
      of IdxColorPalette;

    class function GetFunctionParamCount(const AInfo
      : TdxSpreadSheetFunctionInfo): Integer; static;

    function GetContextParser: TdxExpressionContextParser;
    function GetDataControllerItemIndex(AItem: TObject): Integer;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetProvider
      : TcxDataControllerSpreadSheetExpressionProvider; inline;
  protected
    function CreateIterator(AContextType: TdxExpressionContextParserContextType)
      : TdxExpressionRichEditIterator;
    function GetSelectionContextType: TdxExpressionContextParserContextType;

    function GetDataController: TcxCustomDataController; virtual; abstract;
    function GetDataProvider
      : TcxDataControllerSpreadSheetExpressionDataProvider; virtual;
    function GetRichEdit: TdxExpressionRichEdit; virtual; abstract;

    function IsFunctionTypeAllowed(AType: TdxSpreadSheetFunctionType): Boolean;
    procedure EnumConstants(AProc: TEnumConstantsProc);
    procedure EnumFields(AProc: TEnumFieldsProc);
    procedure EnumFunctions(AProc: TEnumFunctionsProc);
    procedure EnumOperators(AProc: TEnumOperatorsProc);

    property ContextParser: TdxExpressionContextParser read GetContextParser;
  public
    constructor Create;
    destructor Destroy; override;

    function GetDescription(AData: TObject): string;
    function GetErrorMessage(const AErrorCode
      : TdxExpressionValidatorErrorCode): string;
    function GetFieldDescription(AItemIndex: Integer): string;
    function GetFieldImageIndex(AItemIndex: Integer): Integer;
    function GetFilterText(AContextType
      : TdxExpressionContextParserContextType): string;
    function GetFunctionDescription(AInfo: TdxSpreadSheetFunctionInfo): string;
    function GetImageColorPalette(AType: TdxExpressionContextParserContextType;
      ASelected: Boolean): IdxColorPalette;
    function HasError(out AErrorCode: TdxExpressionValidatorErrorCode;
      out AStart, AFinish: Integer): Boolean;
    procedure HighlightTokens;
    procedure PopulateSuggestions(AList: TdxExpressionRichEditSuggestionList);
    procedure PostSuggestion(const AText: string; AData: Pointer;
      AIsReplace: Boolean = True);
    procedure UpdateLookAndFeel;

    property DataController: TcxCustomDataController read GetDataController;
    property DataProvider: TcxDataControllerSpreadSheetExpressionDataProvider
      read GetDataProvider;
    property ExpressionOwner: TObject read FExpressionOwner
      write FExpressionOwner;
    property Highlighter: TdxExpressionHighlighter read FHighlighter;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property Provider: TcxDataControllerSpreadSheetExpressionProvider
      read GetProvider;
    property RichEdit: TdxExpressionRichEdit read GetRichEdit;
  end;

implementation

uses
  SysUtils, dxSpreadSheetCoreStrs, dxSpreadSheetCoreDialogsStrs, Math,
  dxSpreadSheetClasses, cxDataStorage, dxStringHelper, dxTypeHelpers,
  cxGraphics, cxRichEditUtils,
  cxGeometry;

type
  TcxCustomRichEditAccess = class(TcxCustomRichEdit);
  TcxDataControllerSpreadSheetExpressionProviderAccess = class
    (TcxDataControllerSpreadSheetExpressionProvider);

var
  FOperatorInfosRepository: TdxExpressionOperatorInfosRepository;
  FConstantInfosRepository: TdxExpressionConstantInfosRepository;

function dxExpressionOperatorInfosRepository
  : TdxExpressionOperatorInfosRepository;
begin
  if FOperatorInfosRepository = nil then
    FOperatorInfosRepository := TdxExpressionOperatorInfosRepository.Create;
  Result := FOperatorInfosRepository;
end;

function dxExpressionConstantInfosRepository
  : TdxExpressionConstantInfosRepository;
begin
  if FConstantInfosRepository = nil then
    FConstantInfosRepository := TdxExpressionConstantInfosRepository.Create;
  Result := FConstantInfosRepository;
end;

{ TdxExpressionOperatorInfo }

constructor TdxExpressionOperatorInfo.Create(const AOperation
  : TdxSpreadSheetFormulaOperation; const ACaption: string;
  const ADescriptionPtr: TcxResourceStringID);
begin
  inherited Create;
  FOperation := AOperation;
  FCaption := ACaption;
  FDescriptionPtr := ADescriptionPtr;
end;

{ TdxExpressionOperatorInfosRepository }

constructor TdxExpressionOperatorInfosRepository.Create;
begin
  inherited Create;
  FItems := TcxObjectList.Create;
end;

destructor TdxExpressionOperatorInfosRepository.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxExpressionOperatorInfosRepository.GetInfoByOperation
  (const AOperation: TdxSpreadSheetFormulaOperation): TdxExpressionOperatorInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Operation = AOperation then
      Exit(Items[I]);
  Result := nil;
end;

procedure TdxExpressionOperatorInfosRepository.Register(const AOperation
  : TdxSpreadSheetFormulaOperation; const ACaption: string;
  const ADescriptionPtr: TcxResourceStringID);
begin
  Unregister(AOperation);
  FItems.Add(TdxExpressionOperatorInfo.Create(AOperation, ACaption,
    ADescriptionPtr));
  FItems.SortList(function(Item1, Item2: Pointer): Integer
    begin
      Result := Ord(TdxExpressionOperatorInfo(Item1).Operation) -
        Ord(TdxExpressionOperatorInfo(Item2).Operation);
    end);
end;

procedure TdxExpressionOperatorInfosRepository.Unregister(const AOperation
  : TdxSpreadSheetFormulaOperation);
var
  AInfo: TdxExpressionOperatorInfo;
begin
  AInfo := GetInfoByOperation(AOperation);
  if AInfo <> nil then
    FItems.Remove(AInfo);
end;

function TdxExpressionOperatorInfosRepository.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxExpressionOperatorInfosRepository.GetItem(Index: Integer)
  : TdxExpressionOperatorInfo;
begin
  Result := TdxExpressionOperatorInfo(FItems[Index]);
end;

{ TdxExpressionConstantInfo }

constructor TdxExpressionConstantInfo.Create(AData: TdxSpreadSheetFunctionInfo);
begin
  inherited Create;
  FData := AData;
end;

{ TdxExpressionConstantInfosRepository }

constructor TdxExpressionConstantInfosRepository.Create;
begin
  inherited Create;
  FItems := TcxObjectList.Create;
end;

destructor TdxExpressionConstantInfosRepository.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxExpressionConstantInfosRepository.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxExpressionConstantInfosRepository.GetItem(Index: Integer)
  : TdxExpressionConstantInfo;
begin
  Result := TdxExpressionConstantInfo(FItems[Index])
end;

procedure TdxExpressionConstantInfosRepository.Register
  (AInfo: TdxSpreadSheetFunctionInfo);
begin
  FItems.Add(TdxExpressionConstantInfo.Create(AInfo));
end;

{ TdxExpressionRichEditIterator }

constructor TdxExpressionRichEditIterator.Create
  (ARichEdit: TdxExpressionRichEdit);
begin
  inherited Create;
  FRichEdit := ARichEdit;
end;

function TdxExpressionRichEditIterator.GetBackwardPosition
  (AStart: Integer = -1): Integer;
begin
  if AStart = -1 then
    Result := FRichEdit.SelStart
  else
    Result := AStart;
  while (Result > 0) and not IsBackwardBreakChar(Result) do
    Dec(Result);
end;

function TdxExpressionRichEditIterator.GetForwardPosition
  (AStart: Integer = -1): Integer;
var
  ALength: Integer;
begin
  if AStart = -1 then
    Result := FRichEdit.SelStart
  else
    Result := AStart;
  ALength := Length(Text);
  while (Result < ALength) and not IsForwardBreakChar(Result + 1) do
    Inc(Result);
end;

function TdxExpressionRichEditIterator.GetFilterText
  (AStart: Integer = -1): string;
var
  APos: Integer;
begin
  APos := GetBackwardPosition(AStart);
  Result := Copy(FRichEdit.Text, APos + 1, FRichEdit.SelStart - APos);
  Result := UpperCase(Result);
end;

function TdxExpressionRichEditIterator.IsBackwardBreakChar
  (APos: Integer): Boolean;
begin
  Result := IsBreakChar(Text[APos]);
end;

function TdxExpressionRichEditIterator.IsForwardBreakChar
  (APos: Integer): Boolean;
begin
  Result := IsBreakChar(Text[APos]);
end;

function TdxExpressionRichEditIterator.IsBreakChar(AChar: Char): Boolean;
begin
  Result := Pos(AChar, BreakChars) > 0;
end;

function TdxExpressionRichEditIterator.GetText: string;
begin
  Result := FRichEdit.Text;
end;

{ TdxExpressionRichEditFieldIterator }

function TdxExpressionRichEditFieldIterator.GetFilterText
  (AStart: Integer = -1): string;
begin
  Result := inherited GetFilterText(AStart);
  if Length(Result) > 0 then
  begin
    if Result[Length(Result)] = TcxDataControllerSpreadSheetFormulaParser.ItemReferenceFinishMark
    then
      Exit('');
    if Result[1] = TcxDataControllerSpreadSheetFormulaParser.ItemReferenceStartMark
    then
      Delete(Result, 1, 1);
  end;
end;

function TdxExpressionRichEditFieldIterator.IsBackwardBreakChar
  (APos: Integer): Boolean;
begin
  Result := Text[APos + 1] = TcxDataControllerSpreadSheetFormulaParser.
    ItemReferenceStartMark;
end;

function TdxExpressionRichEditFieldIterator.IsForwardBreakChar
  (APos: Integer): Boolean;
begin
  Result := (APos > 0) and
    (Text[APos - 1] = TcxDataControllerSpreadSheetFormulaParser.
    ItemReferenceFinishMark);
end;

{ TdxExpressionRichEditOperatorIterator }

function TdxExpressionRichEditOperatorIterator.IsBackwardBreakChar
  (APos: Integer): Boolean;
begin
  Result := IsForwardBreakChar(APos);
end;

function TdxExpressionRichEditOperatorIterator.IsForwardBreakChar
  (APos: Integer): Boolean;
var
  AChar: Char;
begin
  AChar := Text[APos];
  Result := dxCharIsAlpha(AChar) or dxCharIsNumeric(AChar) or
    dxWideIsSpace(AChar) or (Pos(AChar, BreakChars) > 0);
end;

{ TdxExpressionContextParser }

function TdxExpressionContextCustomParser.GetContextType(const AFormulaText
  : string; APos: Integer): TdxExpressionContextParserContextType;
var
  AFormula: TcxDataControllerSpreadSheetExpressionFormula;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := TdxExpressionContextParserContextType.Unknown;
  AFormula := CreateFormula;
  try
    if ParseFormula(AFormulaText, AFormula) then
    begin
      AToken := TdxSpreadSheetFormulaToken.FindToken(APos, AFormula.Tokens);
      if AToken = nil then
      begin
        if ErrorCode
          in [TdxSpreadSheetFormulaParserErrorCode.
          pecUnexpectedEndOfExternalReference,
          TdxSpreadSheetFormulaParserErrorCode.pecInvalidReference] then
          Result := TdxExpressionContextParserContextType.Field
        else
          Result := TdxExpressionContextParserContextType.Unknown;
      end
      else if AToken is TdxSpreadSheetFormulaOperationToken then
        Result := TdxExpressionContextParserContextType.&Operator
      else if AToken is TdxSpreadSheetFormulaFunctionToken then
        Result := TdxExpressionContextParserContextType.&Function
      else if AToken is TdxSpreadSheetFormulaUnknownFunctionToken then
        Result := TdxExpressionContextParserContextType.&Function
      else if AToken is TdxSpreadSheetFormulaReferenceToken then
        Result := TdxExpressionContextParserContextType.Field
      else if AToken is TcxDataControllerSpreadSheetFormulaUnknownReferenceToken
      then
        Result := TdxExpressionContextParserContextType.Field
      else if AToken is TdxSpreadSheetFormulaUnknownNameToken then
        Result := TdxExpressionContextParserContextType.Unknown
      else if AToken is TdxSpreadSheetFormulaNullToken then
        Result := TdxExpressionContextParserContextType.Unknown
      else
        Result := TdxExpressionContextParserContextType.Value;
    end;
  finally
    AFormula.Free;
  end;
end;

procedure TdxExpressionContextCustomParser.SetErrorIndex(AErrorIndex: Integer;
AErrorCode: TdxSpreadSheetFormulaParserErrorCode);
begin
  if FErrorCode = TdxSpreadSheetFormulaParserErrorCode.pecNone then
  begin
    FErrorCode := AErrorCode;
    FInternalErrorIndex := AErrorIndex;
  end;
end;

function TdxExpressionContextCustomParser.CreateFormula
  : TcxDataControllerSpreadSheetExpressionFormula;
begin
  Result := TcxDataControllerSpreadSheetExpressionFormula
    (TcxDataControllerSpreadSheetExpressionProviderAccess
    (DataProvider.DataController.ExpressionProvider).CreateFormula);
end;

function TdxExpressionContextCustomParser.IsFormulaText(const S: string;
out AOffset: Integer): Boolean;
begin
  Result := True;
  AOffset := 0;
end;

{ TdxExpressionContextParser }

function TdxExpressionContextParser.ValidateTokens
  (AToken: TdxSpreadSheetFormulaToken): Boolean;
begin
  Result := True;
end;

{ TdxExpressionValidator }

function TdxExpressionValidator.Validate(const AFormulaText: string;
out AErrorCode: TdxExpressionValidatorErrorCode;
out AStart, AFinish: Integer): Boolean;

  function FindError(AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    Result := False;
    if AToken = nil then
      Exit;
    if AToken.InheritsFrom
      (TcxDataControllerSpreadSheetFormulaUnknownReferenceToken) then
    begin
      Result := True;
      AErrorCode := TdxExpressionValidatorErrorCode.UnknownFieldName;
    end
    else if AToken.InheritsFrom(TdxSpreadSheetFormulaUnknownFunctionToken) then
    begin
      Result := True;
      AErrorCode := TdxExpressionValidatorErrorCode.UnknownFunctionName;
    end;

    if Result then
    begin
      AStart := AToken.SourceStart;
      AFinish := AToken.SourceFinish;
      Exit;
    end;

    Result := FindError(AToken.Next);
    if Result or not AToken.HasChildren then
      Exit;

    Result := FindError(AToken.FirstChild);
  end;

  function GetValidatorErrorCodeByParserErrorCode
    : TdxExpressionValidatorErrorCode;
  begin
    case FErrorCode of
      pecNone:
        Result := TdxExpressionValidatorErrorCode.None;
      pecUnexpectedEndOfExternalReference:
        Result := TdxExpressionValidatorErrorCode.MissingClosingFieldMark;
      pecUnexpectedEndOfString:
        Result := TdxExpressionValidatorErrorCode.MissingClosingStringMark;
      pecUnexpectedEndOfExpression:
        Result := TdxExpressionValidatorErrorCode.MissingClosingExpressionMark;
      pecInvalidExpression:
        Result := TdxExpressionValidatorErrorCode.InvalidExpression;
    else
      Result := TdxExpressionValidatorErrorCode.InvalidExpression;
    end;
  end;

  function CalculateMisspelledFinish(APos: Integer;
  ARootToken: TdxSpreadSheetFormulaToken): Integer;
  begin
    Result := APos;
    if ARootToken = nil then
      Exit;
    if ARootToken.SourceStart >= AStart then
      Result := Min(Result, ARootToken.SourceStart)
    else if ARootToken.SourceFinish >= AStart then
      Result := Min(Result, ARootToken.SourceFinish);

    Result := CalculateMisspelledFinish(Result, ARootToken.Next);
    if not ARootToken.HasChildren then
      Exit;
    Result := CalculateMisspelledFinish(Result, ARootToken.FirstChild);
  end;

  procedure SetMisspelledBoundsByParser
    (AFormula: TcxDataControllerSpreadSheetExpressionFormula);
  var
    AToken: TdxSpreadSheetFormulaToken;
  begin
    AToken := TdxSpreadSheetFormulaToken.FindToken(InternalErrorIndex,
      AFormula.Tokens);
    if AToken <> nil then
    begin
      if FErrorCode = pecInvalidExpression then
      begin
        if AToken.Parent <> nil then
        begin
          AStart := AToken.Parent.SourceStart;
          AFinish := AToken.Parent.SourceFinish;
        end
        else
        begin
          AStart := 1;
          AFinish := Length(AFormulaText);
        end;
      end
      else
      begin
        AStart := InternalErrorIndex;
        if FErrorCode in [pecUnexpectedEndOfString,
          pecUnexpectedEndOfExpression, pecUnexpectedEndOfArray] then
          AFinish := AStart
        else
          AFinish := AToken.SourceFinish;
      end;
    end
    else
    begin
      AStart := InternalErrorIndex;
      AFinish := CalculateMisspelledFinish(Length(AFormulaText),
        AFormula.Tokens);
    end;
    AFinish := Min(AFinish, Length(AFormulaText));
  end;

var
  AFormula: TcxDataControllerSpreadSheetExpressionFormula;
begin
  AFormula := CreateFormula;
  try
    if ParseFormula(AFormulaText, AFormula) then
    begin
      Result := (FErrorCode = TdxSpreadSheetFormulaParserErrorCode.pecNone) or
        (InternalErrorIndex <= 0);
      if not Result then
      begin
        SetMisspelledBoundsByParser(AFormula);
        AErrorCode := GetValidatorErrorCodeByParserErrorCode;
      end
      else
        Result := not FindError(AFormula.Tokens);
    end
    else
    begin
      Result := False;
      AErrorCode := TdxExpressionValidatorErrorCode.InvalidExpression;
      AStart := 1;
      AFinish := Length(AFormulaText);
    end;
  finally
    AFormula.Free;
  end;
end;

{ TdxExpressionHighlighter }

constructor TdxExpressionHighlighter.Create;
begin
  inherited Create;
  FTokens := TObjectList.Create;
  UpdateColors(clWindowText);
end;

destructor TdxExpressionHighlighter.Destroy;
begin
  FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TdxExpressionHighlighter.AddToken(const AStartIndex,
  AFinishIndex: Integer; AType: TdxExpressionContextParserContextType);
var
  ATokenInfo: TTokenInfo;
begin
  ATokenInfo := TTokenInfo.Create;
  ATokenInfo.Position := AStartIndex;
  ATokenInfo.Length := AFinishIndex - AStartIndex + 1;
  ATokenInfo.&Type := AType;
  FTokens.Add(ATokenInfo);
end;

procedure TdxExpressionHighlighter.Apply(AEdit: TcxCustomRichEdit);

  procedure ApplyToken(const ATokenInfo: TTokenInfo);
  begin
    AEdit.SelStart := ATokenInfo.Position - 1;
    AEdit.SelLength := ATokenInfo.Length;
    AEdit.SelAttributes.Color := GetColor(ATokenInfo.&Type);
  end;

var
  ASavedSelLength: Integer;
  ASavedSelStart: Integer;
  I: Integer;
begin
  CalculateTokens(GetAdjustedRichEditText(AEdit));

  SendMessage(AEdit.Handle, WM_SETREDRAW, 0, 0);
  try
    AEdit.LockChangeEvents(True, False);
    ASavedSelStart := AEdit.SelStart;
    ASavedSelLength := AEdit.SelLength;
    try
      if CanApplyDefaultTextColor then
      begin
        AEdit.SelectAll;
        AEdit.SelAttributes.Color := FTextColor;
      end;
      for I := 0 to FTokens.Count - 1 do
        ApplyToken(FTokens.List[I]);
    finally
      AEdit.SelStart := ASavedSelStart;
      AEdit.SelLength := ASavedSelLength;
      AEdit.LockChangeEvents(False, False);
    end;
  finally
    SendMessage(AEdit.Handle, WM_SETREDRAW, 1, 1);
    AEdit.InvalidateWithChildren;
  end;
end;

procedure TdxExpressionHighlighter.UpdateColors(ATextColor: TColor);

  function MakeColor(const ABaseColor: TdxHSL; ADefaultColor: TColor): TColor;
  begin
    Result := TdxColorSpaceConverter.HSLToColor
      (TdxColorSpaceConverter.ColorToHSL(ADefaultColor).H, ABaseColor.S,
      ABaseColor.L);
  end;

var
  ABaseColor: TdxHSL;
begin
  FTextColor := ATextColor;

  ABaseColor := TdxColorSpaceConverter.ColorToHSL(FTextColor);
  ABaseColor.L := Min(Max(ABaseColor.L, 0.4), 0.8);
  ABaseColor.S := Min(Max(ABaseColor.S, 0.6), 0.9);

  FFieldColor := MakeColor(ABaseColor, DefaultColorField);
  FFunctionColor := MakeColor(ABaseColor, DefaultColorFunction);
  FValueColor := MakeColor(ABaseColor, DefaultColorValue);
end;

procedure TdxExpressionHighlighter.CalculateTokens(const AText: string);

  procedure ProcessToken(var AIndex: Integer; ALength: Integer;
  AType: TdxExpressionContextParserContextType; ABreakChar: Char);
  var
    AStartIndex: Integer;
  begin
    AStartIndex := AIndex;
    Inc(AIndex);
    while (AIndex <= ALength) and (AText[AIndex] <> ABreakChar) do
      Inc(AIndex);
    AddToken(AStartIndex, AIndex, AType);
    Inc(AIndex);
  end;

var
  AIndex: Integer;
  ALength: Integer;
  AStartIndex: Integer;
begin
  FTokens.Clear;
  ALength := Length(AText);
  AIndex := 1;

  while AIndex <= ALength do
    case AText[AIndex] of
      TcxDataControllerSpreadSheetFormulaParser.ItemReferenceStartMark:
        ProcessToken(AIndex, ALength,
          TdxExpressionContextParserContextType.Field,
          TcxDataControllerSpreadSheetFormulaParser.ItemReferenceFinishMark);

      ')':
        begin
          AddToken(AIndex, AIndex,
            TdxExpressionContextParserContextType.&Function);
          Inc(AIndex);
        end;

      '(':
        begin
          AStartIndex := AIndex - 1;
          while (AStartIndex >= 1) and
            (Pos(AText[AStartIndex], BreakChars) = 0) do
            Dec(AStartIndex);
          AddToken(AStartIndex + 1, AIndex,
            TdxExpressionContextParserContextType.&Function);
          Inc(AIndex);
        end;

    else
      if AText[AIndex] = dxStringMarkChar then
        ProcessToken(AIndex, ALength,
          TdxExpressionContextParserContextType.Value, dxStringMarkChar)
      else if AText[AIndex] = dxStringMarkChar2 then
        ProcessToken(AIndex, ALength,
          TdxExpressionContextParserContextType.Value, dxStringMarkChar2)
      else
        Inc(AIndex);
    end;
end;

function TdxExpressionHighlighter.CanApplyDefaultTextColor: Boolean;
begin
  Result := True;
end;

function TdxExpressionHighlighter.GetColor
  (AType: TdxExpressionContextParserContextType): TColor;
begin
  case AType of
    TdxExpressionContextParserContextType.Field:
      Result := FieldColor;
    TdxExpressionContextParserContextType.&Function:
      Result := FunctionColor;
    TdxExpressionContextParserContextType.Value:
      Result := ValueColor;
  else
    Result := TextColor;
  end;
end;

{ TdxExpressionErrorHighlighter }

procedure TdxExpressionErrorHighlighter.CalculateTokens(const AText: string);
begin
  AddToken(FStart, FFinish, TdxExpressionContextParserContextType.Unknown);
end;

function TdxExpressionErrorHighlighter.CanApplyDefaultTextColor: Boolean;
begin
  Result := False;
end;

constructor TdxExpressionErrorHighlighter.Create(AStart, AFinish: Integer);
begin
  inherited Create;
  FStart := AStart;
  FFinish := AFinish;
end;

function TdxExpressionErrorHighlighter.GetColor
  (AType: TdxExpressionContextParserContextType): TColor;
begin
  Result := DefaultErrorColor;
end;

{ TdxCustomExpressionEditorController }

constructor TdxCustomExpressionEditorController.Create;
begin
  inherited Create;
  FHighlighter := TdxExpressionHighlighter.Create;
end;

destructor TdxCustomExpressionEditorController.Destroy;
begin
  FreeAndNil(FContextParser);
  FreeAndNil(FHighlighter);
  inherited Destroy;
end;

procedure TdxCustomExpressionEditorController.EnumConstants
  (AProc: TEnumConstantsProc);
var
  I: Integer;
begin
  for I := 0 to dxExpressionConstantInfosRepository.Count - 1 do
    AProc(dxExpressionConstantInfosRepository.Items[I]);
end;

procedure TdxCustomExpressionEditorController.EnumFields
  (AProc: TEnumFieldsProc);
var
  I: Integer;
begin
  for I := 0 to DataController.ItemCount - 1 do
    if DataController.ExpressionProvider.CanUseItemInExpression(I) and
      ((FExpressionOwner = nil) or
      (FExpressionOwner <> DataController.GetItem(I))) then
      AProc(I);
end;

procedure TdxCustomExpressionEditorController.EnumFunctions
  (AProc: TEnumFunctionsProc);
var
  AFunctionInfo: TdxSpreadSheetFunctionInfo;
  AFunctions: TdxSpreadSheetFunctionsRepository;
  I: Integer;
begin
  AFunctions := dxSpreadSheetFunctionsRepository;
  for I := 0 to AFunctions.Count - 1 do
  begin
    AFunctionInfo := AFunctions.Items[I];
    if AFunctionInfo.IsValid and AFunctionInfo.AvailableInContainerControls and
      IsFunctionTypeAllowed(AFunctionInfo.TypeID) then
      AProc(AFunctionInfo);
  end;
end;

procedure TdxCustomExpressionEditorController.EnumOperators
  (AProc: TEnumOperatorsProc);
var
  AInfo: TdxExpressionOperatorInfo;
  I: Integer;
begin
  for I := 0 to dxExpressionOperatorInfosRepository.Count - 1 do
  begin
    AInfo := dxExpressionOperatorInfosRepository.Items[I];
    AProc(AInfo);
  end;
end;

function TdxCustomExpressionEditorController.CreateIterator
  (AContextType: TdxExpressionContextParserContextType)
  : TdxExpressionRichEditIterator;
begin
  if AContextType = TdxExpressionContextParserContextType.Field then
    Result := TdxExpressionRichEditFieldIterator.Create(RichEdit)
  else if AContextType = TdxExpressionContextParserContextType.Operator then
    Result := TdxExpressionRichEditOperatorIterator.Create(RichEdit)
  else
    Result := TdxExpressionRichEditIterator.Create(RichEdit);
end;

function TdxCustomExpressionEditorController.GetSelectionContextType
  : TdxExpressionContextParserContextType;

  function IsField: Boolean;
  var
    AIterator: TdxExpressionRichEditIterator;
    APos: Integer;
  begin
    AIterator := CreateIterator(TdxExpressionContextParserContextType.Unknown);
    try
      APos := AIterator.GetBackwardPosition;
    finally
      AIterator.Free;
    end;
    Result := (Length(RichEdit.Text) > APos) and (APos > 0) and
      (RichEdit.Text[APos] = TcxDataControllerSpreadSheetFormulaParser.
      ItemReferenceStartMark);
  end;

  function IsEndOfFieldOrFunctionOrValue: Boolean;
  var
    APos: Integer;
  begin
    Result := False;
    APos := RichEdit.SelStart;
    while (APos > 0) and (Pos(RichEdit.Text[APos], ' '#13#10#9) > 0) do
      Dec(APos);
    if (APos <= 0) or (Pos(RichEdit.Text[APos], ',;') > 0) then
      Exit;
    Result := ContextParser.GetContextType(RichEdit.Text, APos)
      in [TdxExpressionContextParserContextType.Field,
      TdxExpressionContextParserContextType.&Function,
      TdxExpressionContextParserContextType.Value];
  end;

begin
  Result := TdxExpressionContextParserContextType.Unknown;
  if Length(Trim(RichEdit.Text)) = 0 then
    Exit;
  Result := ContextParser.GetContextType(RichEdit.Text, RichEdit.SelStart);
  if Result = TdxExpressionContextParserContextType.Unknown then
  begin
    if IsField then
      Result := TdxExpressionContextParserContextType.Field
    else if IsEndOfFieldOrFunctionOrValue then
      Result := TdxExpressionContextParserContextType.&Operator;
  end
  else if (Result = TdxExpressionContextParserContextType.&Function) and
    (RichEdit.SelStart > 0) and (RichEdit.Text[RichEdit.SelStart] = ')') then
    Result := TdxExpressionContextParserContextType.&Operator;
end;

function TdxCustomExpressionEditorController.GetDescription
  (AData: TObject): string;
var
  AItemIndex: Integer;
begin
  if AData is TdxSpreadSheetFunctionInfo then
    Result := GetFunctionDescription(TdxSpreadSheetFunctionInfo(AData))
  else if AData is TdxExpressionConstantInfo then
    Result := cxGetResourceString(TdxExpressionConstantInfo(AData)
      .Data.DescriptionPtr)
  else if AData is TdxExpressionOperatorInfo then
    Result := cxGetResourceString(TdxExpressionOperatorInfo(AData)
      .DescriptionPtr)
  else
  begin
    AItemIndex := GetDataControllerItemIndex(AData);
    if AItemIndex > -1 then
      Result := GetFieldDescription(AItemIndex)
    else
      Result := '';
  end;
end;

function TdxCustomExpressionEditorController.GetErrorMessage(const AErrorCode
  : TdxExpressionValidatorErrorCode): string;
begin
  case AErrorCode of
    TdxExpressionValidatorErrorCode.MissingClosingFieldMark:
      Result := cxGetResourceString
        (@sdxExpressionEditorMissingClosingFieldMark);
    TdxExpressionValidatorErrorCode.MissingClosingStringMark:
      Result := cxGetResourceString
        (@sdxExpressionEditorMissingClosingStringMark);
    TdxExpressionValidatorErrorCode.UnknownFieldName:
      Result := cxGetResourceString(@sdxExpressionEditorUnknownField);
    TdxExpressionValidatorErrorCode.UnknownFunctionName:
      Result := cxGetResourceString(@sdxExpressionEditorUnknownFunction);
  else
    Result := cxGetResourceString(@sdxExpressionEditorSyntaxError);
  end;
end;

function TdxCustomExpressionEditorController.GetFieldDescription
  (AItemIndex: Integer): string;
var
  AName: string;
  AType: TcxValueTypeClass;
begin
  AName := DataController.ExpressionProvider.GetItemReferenceName(AItemIndex);
  AType := DataController.GetItemValueTypeClass(AItemIndex);
  if AType <> nil then
    Result := Format(cxGetResourceString(@sdxExpressionEditorFieldDescription),
      [AType.Caption, AName])
  else
    Result := AName;
end;

function TdxCustomExpressionEditorController.GetFunctionDescription
  (AInfo: TdxSpreadSheetFunctionInfo): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    ABuffer.Append('{\rtf1\b ');
    ABuffer.Append(AInfo.ToString);
    ABuffer.Append('\b0\par\par ');
    ABuffer.Append(cxGetResourceString(AInfo.DescriptionPtr));
    ABuffer.Append('}');
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

function TdxCustomExpressionEditorController.GetImageColorPalette
  (AType: TdxExpressionContextParserContextType; ASelected: Boolean)
  : IdxColorPalette;
begin
  if ASelected then
    Result := FImageColorPaletteForSelected
  else
    Result := FImageColorPalettes[AType];
end;

function TdxCustomExpressionEditorController.HasError(out AErrorCode
  : TdxExpressionValidatorErrorCode; out AStart, AFinish: Integer): Boolean;
var
  AValidator: TdxExpressionValidator;
begin
  AValidator := TdxExpressionValidator.Create(DataProvider);
  try
    Result := not AValidator.Validate(GetAdjustedRichEditText(RichEdit),
      AErrorCode, AStart, AFinish);
  finally
    AValidator.Free;
  end;
end;

function TdxCustomExpressionEditorController.GetFieldImageIndex
  (AItemIndex: Integer): Integer;
begin
  Result := -1;
  if DataController.GetItemValueTypeClass(AItemIndex) <> nil then
    case DataController.GetItemValueTypeClass(AItemIndex).GetVarType of
      varBoolean:
        Result := 1;
      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord,
        varInt64, varUInt64:
        Result := 5;
      varSingle, varDouble, varCurrency:
        Result := 3;
      varDate:
        Result := 2;
      varOleStr, varString, varUString:
        Result := 6;
    else
      Result := 7;
    end;
end;

function TdxCustomExpressionEditorController.GetFilterText
  (AContextType: TdxExpressionContextParserContextType): string;
var
  AIterator: TdxExpressionRichEditIterator;
begin
  if RichEdit.SelLength > 0 then
    Exit(UpperCase(RichEdit.SelText));

  AIterator := CreateIterator(AContextType);
  try
    Result := AIterator.GetFilterText(RichEdit.SelStart);
  finally
    AIterator.Free;
  end;
end;

procedure TdxCustomExpressionEditorController.HighlightTokens;
begin
  FHighlighter.Apply(RichEdit);
end;

function TdxCustomExpressionEditorController.IsFunctionTypeAllowed
  (AType: TdxSpreadSheetFunctionType): Boolean;
begin
  Result := not(AType in [ftLookupAndReference, ftDatabase]);
end;

procedure TdxCustomExpressionEditorController.PopulateSuggestions
  (AList: TdxExpressionRichEditSuggestionList);
var
  AFilterText: string;
  AContextType: TdxExpressionContextParserContextType;
begin
  AContextType := GetSelectionContextType;
  AFilterText := GetFilterText(AContextType);
  if AContextType in [TdxExpressionContextParserContextType.Unknown,
    TdxExpressionContextParserContextType.Field] then
    EnumFields(procedure(AItemIndex: Integer)
      var
        AName: string;
      begin
        AName := DataController.ExpressionProvider.GetItemReferenceName
          (AItemIndex);
        if (AFilterText = '') or TdxStringHelper.StartsWith(UpperCase(AName),
          AFilterText) then
          AList.Add(TdxExpressionRichEditSuggestion.Create(AName,
            GetFieldImageIndex(AItemIndex),
            DataController.GetItem(AItemIndex)));
      end);

  if AContextType in [TdxExpressionContextParserContextType.Unknown,
    TdxExpressionContextParserContextType.&Function] then
    EnumFunctions(procedure(AFunctionInfo: TdxSpreadSheetFunctionInfo)
      var
        AName: string;
      begin
        AName := AFunctionInfo.Name;
        if (AFilterText = '') or TdxStringHelper.StartsWith(UpperCase(AName),
          AFilterText) then
          AList.Add(TdxExpressionRichEditSuggestion.Create(AName, 0,
            AFunctionInfo));
      end);

  if AContextType = TdxExpressionContextParserContextType.&Operator then
    EnumOperators(procedure(AInfo: TdxExpressionOperatorInfo)
      var
        AName: string;
      begin
        AName := AInfo.Caption;
        AList.Add(TdxExpressionRichEditSuggestion.Create(AName, -1, AInfo));
      end);

  if (AContextType in [TdxExpressionContextParserContextType.Unknown,
    TdxExpressionContextParserContextType.Field,
    TdxExpressionContextParserContextType.&Function,
    TdxExpressionContextParserContextType.&Operator]) and (AList.Count = 0) then
    AList.Add(TdxExpressionRichEditSuggestion.Create
      (cxGetResourceString(@sdxExpressionEditorNoSuggestions)));
end;

procedure TdxCustomExpressionEditorController.PostSuggestion
  (const AText: string; AData: Pointer; AIsReplace: Boolean);

  function CalculatePastedFunction(AInfo: TdxSpreadSheetFunctionInfo;
  var ASelStartCorrection: Integer): string;
  var
    AParamCount: Integer;
    I: Integer;
    AParams: string;
    APos: Integer;
    AParamsSeparator: string;
  begin
    Result := AInfo.Name;
    APos := RichEdit.SelStart + RichEdit.SelLength;
    if APos <= Length(RichEdit.Text) then
    begin
      if (APos = Length(RichEdit.Text)) or (RichEdit.Text[APos + 1] <> '(') then
      begin
        AParamCount := GetFunctionParamCount(AInfo);
        case AParamCount of
          0:
            begin
              Result := Format('%s()', [Result]);
              ASelStartCorrection := 0;
            end;
          1:
            begin
              Result := Format('%s()', [Result]);
              ASelStartCorrection := -1;
            end;
        else
          ASelStartCorrection := Length(Result);
          AParamsSeparator :=
            Format('%s ', [DataProvider.FormatSettings.ListSeparator]);
          AParams := AParamsSeparator;
          for I := 2 to AParamCount - 1 do
            AParams := AParams + AParamsSeparator;
          Result := Format('%s(%s)', [Result, AParams]);
          ASelStartCorrection := ASelStartCorrection - Length(Result) + 1;
        end;
      end
      else
        ASelStartCorrection := 1;
    end;
  end;

  function CalculatePastedField(const AIntf
    : IcxDataControllerSpreadSheetExpressionItem;
  var ASelStartCorrection: Integer): string;
  begin
    Result := Format('[%s]', [AIntf.GetReferenceName]);
    ASelStartCorrection := 2;
  end;

  function CalculatePastedText(var ASelStartCorrection: Integer): string;
  var
    AIntf: IcxDataControllerSpreadSheetExpressionItem;
  begin
    if Supports(TObject(AData), IcxDataControllerSpreadSheetExpressionItem,
      AIntf) then
      Result := CalculatePastedField(AIntf, ASelStartCorrection)
    else if TObject(AData) is TdxSpreadSheetFunctionInfo then
      Result := CalculatePastedFunction(TdxSpreadSheetFunctionInfo(AData),
        ASelStartCorrection)
    else
      Result := AText;

    if (RichEdit.SelStart > 0) and
      (Pos(RichEdit.Text[RichEdit.SelStart], ' ('#13#10) = 0) then
      Result := Format(' %s', [Result]);
  end;

var
  ASelStartCorrection: Integer;
  AIterator: TdxExpressionRichEditIterator;
begin
  RichEdit.Lines.BeginUpdate;
  try
    RichEdit.Properties.HideSelection := True;
    if (RichEdit.SelLength = 0) and AIsReplace then
    begin
      AIterator := CreateIterator(GetSelectionContextType);
      try
        RichEdit.SelStart := AIterator.GetBackwardPosition;
        RichEdit.SelLength := AIterator.GetForwardPosition(RichEdit.SelStart) -
          RichEdit.SelStart;
      finally
        AIterator.Free;
      end;
    end;
    ASelStartCorrection := 0;
    RichEdit.SelText := CalculatePastedText(ASelStartCorrection);
    RichEdit.SelStart := RichEdit.SelStart + ASelStartCorrection;
  finally
    RichEdit.Properties.HideSelection := False;
    RichEdit.Lines.EndUpdate;
  end;
  PostMessage(RichEdit.Handle, DXM_CONTAINERSETFOCUS, 0, 0);
end;

procedure TdxCustomExpressionEditorController.UpdateLookAndFeel;

  function CreatePalette(const AColor, ATextColor, AAccentColor: TColor)
    : IdxColorPalette;
  var
    APalette: TdxAdvancedColorPalette;
  begin
    APalette := TdxAdvancedColorPalette.Create;
    APalette.FillColors['Black'] := dxColorToAlphaColor(ATextColor);
    APalette.FillColors['Blue'] := dxColorToAlphaColor(AAccentColor);
    APalette.FillColors['Green'] := dxColorToAlphaColor(AAccentColor);
    APalette.FillColors['Red'] := dxColorToAlphaColor(AAccentColor);
    APalette.FillColors['White'] := dxColorToAlphaColor(AColor);
    APalette.FillColors['Yellow'] := dxColorToAlphaColor(AAccentColor);
    Result := APalette;
  end;

var
  AColor: TColor;
  ATextColor: TColor;
begin
  Highlighter.UpdateColors(TcxCustomRichEditAccess(RichEdit).VisibleFontColor);

  ATextColor := Painter.DefaultSelectionTextColor;
  FImageColorPaletteForSelected := CreatePalette(Painter.DefaultSelectionColor,
    ATextColor, ATextColor);

  AColor := cxGetActualColor(Painter.DefaultEditorBackgroundColor(False),
    clWindow);
  ATextColor := cxGetActualColor(Painter.DefaultEditorTextColor(False),
    clWindowText);
  FImageColorPalettes[TdxExpressionContextParserContextType.Field] :=
    CreatePalette(AColor, ATextColor, Highlighter.FieldColor);
  FImageColorPalettes[TdxExpressionContextParserContextType.Function] :=
    CreatePalette(AColor, ATextColor, Highlighter.FunctionColor);
  FImageColorPalettes[TdxExpressionContextParserContextType.Value] :=
    CreatePalette(AColor, ATextColor, Highlighter.ValueColor);
end;

class function TdxCustomExpressionEditorController.GetFunctionParamCount
  (const AInfo: TdxSpreadSheetFunctionInfo): Integer;
var
  AParamKind: TdxSpreadSheetFunctionParamKindInfo;
begin
  Result := 0;
  if Assigned(AInfo.ParamInfo) then
  begin
    AInfo.ParamInfo(Result, AParamKind);
    if (Result = 1) and
      (AParamKind[0] = TdxSpreadSheetFunctionParamKind.fpkUnlimited) then
      Result := 2;
  end;
end;

function TdxCustomExpressionEditorController.GetContextParser
  : TdxExpressionContextParser;
begin
  if FContextParser = nil then
    FContextParser := TdxExpressionContextParser.Create(DataProvider);
  Result := FContextParser;
end;

function TdxCustomExpressionEditorController.GetDataControllerItemIndex
  (AItem: TObject): Integer;
var
  I: Integer;
begin
  for I := 0 to DataController.ItemCount - 1 do
    if DataController.GetItem(I) = AItem then
      Exit(I);
  Result := -1;
end;

function TdxCustomExpressionEditorController.GetDataProvider
  : TcxDataControllerSpreadSheetExpressionDataProvider;
begin
  Result := TcxDataControllerSpreadSheetExpressionProviderAccess(Provider)
    .DataProvider;
end;

function TdxCustomExpressionEditorController.GetPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := RichEdit.Style.LookAndFeel.Painter;
end;

function TdxCustomExpressionEditorController.GetProvider
  : TcxDataControllerSpreadSheetExpressionProvider;
begin
  Result := TcxDataControllerSpreadSheetExpressionProvider
    (DataController.ExpressionProvider);
end;

{ --- }

procedure RegisterOperatorInfos;
begin
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opAdd, '+',
    @sdxExpressionEditorOperatorAddDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opSub, '-',
    @sdxExpressionEditorOperatorSubDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opMul, '*',
    @sdxExpressionEditorOperatorMulDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opDiv, '/',
    @sdxExpressionEditorOperatorDivDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opPower, '^',
    @sdxExpressionEditorOperatorPowerDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opConcat, '&',
    @sdxExpressionEditorOperatorConcatDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opLT, '<',
    @sdxExpressionEditorOperatorLTDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opLE, '<=',
    @sdxExpressionEditorOperatorLEDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opEQ, '=',
    @sdxExpressionEditorOperatorEQDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opGE, '>=',
    @sdxExpressionEditorOperatorGEDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opGT, '>',
    @sdxExpressionEditorOperatorGTDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opNE, '<>',
    @sdxExpressionEditorOperatorNEDescription);
  dxExpressionOperatorInfosRepository.Register
    (TdxSpreadSheetFormulaOperation.opPercent, '%',
    @sdxExpressionEditorOperatorPercentDescription);
end;

procedure RegisterConstantInfos;
var
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AInfo := dxSpreadSheetFunctionsRepository.GetInfoByID(34);
  if AInfo <> nil then
    dxExpressionConstantInfosRepository.Register(AInfo);
  AInfo := dxSpreadSheetFunctionsRepository.GetInfoByID(35);
  if AInfo <> nil then
    dxExpressionConstantInfosRepository.Register(AInfo);
end;

initialization

RegisterOperatorInfos;
RegisterConstantInfos;

finalization

FreeAndNil(FOperatorInfosRepository);
FreeAndNil(FConstantInfosRepository);

end.
