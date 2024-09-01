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

unit dxSpreadSheetFormulas;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Variants, Classes, Types, StrUtils, dxCore, dxCoreClasses,
  cxClasses, cxFormats,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetUtils, dxSpreadSheetStrs,
  dxSpreadSheetClasses,
  cxVariants, Generics.Defaults, Generics.Collections, cxGeometry,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreReferences, dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions, dxSpreadSheetCoreFormulasParser;

{ Aliases }

type
  TdxSpreadSheetFunctionParamKind = dxSpreadSheetCoreFormulas.
    TdxSpreadSheetFunctionParamKind;
  TdxSpreadSheetFunctionParamKindInfo = dxSpreadSheetCoreFormulas.
    TdxSpreadSheetFunctionParamKindInfo;
  TdxSpreadSheetFunctionResultKind = dxSpreadSheetCoreFormulas.
    TdxSpreadSheetFunctionResultKind;

const
  frkArray = dxSpreadSheetCoreFormulas.frkArray;
  frkNonArrayValue = dxSpreadSheetCoreFormulas.frkNonArrayValue;
  frkValue = dxSpreadSheetCoreFormulas.frkValue;

  fpkArray = dxSpreadSheetCoreFormulas.fpkArray;
  fpkNonRequiredArray = dxSpreadSheetCoreFormulas.fpkNonRequiredArray;
  fpkNonRequiredUnlimited = dxSpreadSheetCoreFormulas.fpkNonRequiredUnlimited;
  fpkNonRequiredValue = dxSpreadSheetCoreFormulas.fpkNonRequiredValue;
  fpkUnlimited = dxSpreadSheetCoreFormulas.fpkUnlimited;
  fpkValue = dxSpreadSheetCoreFormulas.fpkValue;

type
  { TdxSpreadSheet3DExternalReferenceLink }

  TdxSpreadSheet3DExternalReferenceLink = class
    (TdxSpreadSheet3DReferenceCustomExternalLink)
  strict private
    function IsFileProtocol(const ATarget: string): Boolean;
  public
    function ToString: string; override;
  end;

  { TdxSpreadSheetFormulaDefinedNameToken }

  TdxSpreadSheetFormulaDefinedNameToken = class
    (TdxSpreadSheetFormulaCustomDefinedNameToken)
  strict private
    FDefinedNames: TdxSpreadSheetDefinedNames;
    FLink: TdxSpreadSheet3DReferenceCustomLink;

    function GetDefinedName: TdxSpreadSheetDefinedName;
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure CalculateDimension(var ADimension
      : TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(const AValue: string;
      ADefinedNames: TdxSpreadSheetDefinedNames = nil;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil); reintroduce;
    destructor Destroy; override;
    procedure EnumReferences
      (AProc: TdxSpreadSheetFormulaEnumReferenceTokensProc;
      AProcessDefinedNames: Boolean = False); override;
    function ExtractColumn(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    function UpdateValue(ADefinedName: TdxSpreadSheetDefinedName;
      const ANewName: string): Boolean;
    //
    property DefinedName: TdxSpreadSheetDefinedName read GetDefinedName;
    property DefinedNames: TdxSpreadSheetDefinedNames read FDefinedNames;
    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink;
  end;

  { TdxSpreadSheetFormulaUnknownNameToken }

  TdxSpreadSheetFormulaUnknownNameToken = class
    (TdxSpreadSheetFormulaDefinedNameToken)
  public
    function ExtractColumn(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
  end;

  { TdxSpreadSheetFormulaParser }

  TdxSpreadSheetFormulaParser = class(TdxSpreadSheetCustomFormulaParser)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;

    function GetDefinedNames: TdxSpreadSheetDefinedNames; inline;
    function GetSheet: TdxSpreadSheetTableView;
  protected
    function Create3DReferenceAutoLink
      : TdxSpreadSheet3DReferenceCustomLink; override;
    function Create3DReferenceExternalLink(ALinkId: Integer;
      const AName: string)
      : TdxSpreadSheet3DReferenceCustomExternalLink; override;
    function CreateDefinedNameToken(const AToken
      : TdxSpreadSheetFormulaParserToken;
      out ADefinedNameToken: TdxSpreadSheetFormulaToken;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil): Boolean; override;
    function GetViewByName(APosition, ALength: Integer; out AView: TObject)
      : Boolean; override;

    property DefinedNames: TdxSpreadSheetDefinedNames read GetDefinedNames;
    property Sheet: TdxSpreadSheetTableView read GetSheet;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet);
      reintroduce; virtual;
    function ParseFormula(const AFormulaText: string;
      AFormula: TdxSpreadSheetCustomFormula): Boolean; overload; override;
    function ParseFormula(const AFormulaText: string; ACell: TdxSpreadSheetCell)
      : Boolean; reintroduce; overload; virtual;

    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

  { TdxSpreadSheetFormulaReferenceInfo }

  TdxSpreadSheetFormulaReferenceInfo = class
  public
    Area: TRect;
    DisplayBounds: TRect;
    Length: Integer;
    PositionInText: Integer;
  end;

  { TdxSpreadSheetFormulaReferences }

  TdxSpreadSheetFormulaReferences = class(TcxObjectList)
  strict private
    function GetItem(Index: Integer)
      : TdxSpreadSheetFormulaReferenceInfo; inline;
  public
    procedure Initialize(ACell: TdxSpreadSheetCell; const AFormulaText: string);
    //
    property Items[Index: Integer]: TdxSpreadSheetFormulaReferenceInfo
      read GetItem; default;
  end;

  { TdxSpreadSheetFormulaReferencesParser }

  TdxSpreadSheetFormulaReferencesParser = class(TdxSpreadSheetFormulaParser)
  strict private
  const
    Delimiters = #0#9#10#11#13#32'.,<>=!*^&%#@?:;"()[]{}+|-/\'#$201C#$201D;
  protected const
    TokenText = TdxSpreadSheetCustomFormulaParser.TokenLast + 1;
    StateText = TdxSpreadSheetCustomFormulaParser.StateLast + 1;
  protected
    function CreateTokenText(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function GetTokensFromStringRange(var APosition, AFinishPos: Integer;
      AParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc)
      : TdxSpreadSheetFormulaToken; override;
    function IsDelimiter(const C: Char): Boolean; inline;
    function IsText(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    procedure PopulateStateMachine; override;
    procedure SetErrorIndex(AErrorIndex: Integer;
      AErrorCode: TdxSpreadSheetFormulaParserErrorCode); override;
    function ValidateTokens(AToken: TdxSpreadSheetFormulaToken)
      : Boolean; override;
  end;

implementation

uses
  Math, dxSpreadSheetCoreStrs, dxStringHelper;

type
  TFormulaAccess = class(TdxSpreadSheetFormula);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);
  TResultAccess = class(TdxSpreadSheetFormulaResult);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetExternalLinksAccess = class(TdxSpreadSheetExternalLinks);

  { TdxSpreadSheetFormulaUnknownNameToken }

function TdxSpreadSheetFormulaUnknownNameToken.ExtractColumn
  (const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetVector;
begin
  Result := TdxSpreadSheetSimpleVector.Create;
  AErrorCode := ecName;
end;

function TdxSpreadSheetFormulaUnknownNameToken.ExtractRow(const AIndex: Integer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  Result := TdxSpreadSheetSimpleVector.Create;
  AErrorCode := ecName;
end;

procedure TdxSpreadSheetFormulaUnknownNameToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := 0;
  AErrorCode := ecName;
end;

{ TdxSpreadSheet3DExternalReferenceLink }

function TdxSpreadSheet3DExternalReferenceLink.ToString: string;
var
  ABuffer: TStringBuilder;
  AExternalLink: TdxSpreadSheetExternalLink;
begin
  AExternalLink := TdxSpreadSheetExternalLink(Data);
  if AExternalLink = nil then
    Result := Name
  else if TdxSpreadSheetInvalidObject.IsInvalid(AExternalLink) then
  begin
    if Name <> '' then
      Result := Name
    else
      Result := serRefError;
  end
  else
  begin
    ABuffer := TdxStringBuilderManager.Get;
    try
      if Owner.FormatSettings.ExpandExternalLinks then
      begin
        if IsFileProtocol(AExternalLink.ActualTarget) then
          ABuffer.Append(AExternalLink.ActualTarget)
        else
        begin
          ABuffer.Append(dxReferenceLeftParenthesis);
          ABuffer.Append(AExternalLink.ActualTarget);
          ABuffer.Append(dxReferenceRightParenthesis);
        end;
      end
      else
      begin
        ABuffer.Append(dxReferenceLeftParenthesis);
        ABuffer.Append(AExternalLink.Index + 1);
        ABuffer.Append(dxReferenceRightParenthesis);
      end;

      if Name <> '' then
      begin
        ABuffer.Append(Name);
        ABuffer.Append(dxRefSeparator);
      end;

      Result := ABuffer.ToString;
    finally
      TdxStringBuilderManager.Release(ABuffer);
    end;
  end;
end;

function TdxSpreadSheet3DExternalReferenceLink.IsFileProtocol
  (const ATarget: string): Boolean;
begin
  Result := StartsText('file:', ATarget);
end;

{ TdxSpreadSheetFormulaDefinedNameToken }

constructor TdxSpreadSheetFormulaDefinedNameToken.Create(const AValue: string;
  ADefinedNames: TdxSpreadSheetDefinedNames = nil;
  ALink: TdxSpreadSheet3DReferenceCustomLink = nil);
begin
  inherited Create(AValue);
  FDefinedNames := ADefinedNames;
  SetLink(FLink, ALink);
end;

destructor TdxSpreadSheetFormulaDefinedNameToken.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormulaDefinedNameToken.EnumReferences
  (AProc: TdxSpreadSheetFormulaEnumReferenceTokensProc;
  AProcessDefinedNames: Boolean = False);
var
  ALink: TdxSpreadSheetDefinedNameAccess;
begin
  if AProcessDefinedNames then
  begin
    ALink := TdxSpreadSheetDefinedNameAccess(DefinedName);
    if (ALink <> nil) and (ALink.Formula <> nil) then
      ALink.Formula.EnumReferences(AProc, True);
  end;
end;

function TdxSpreadSheetFormulaDefinedNameToken.ExtractColumn
  (const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetVector;
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    Result := TTokenAccess(TResultAccess(AResult).FirstItem)
      .ExtractColumn(AIndex, AErrorCode);
  finally
    AResult.Free;
  end;
end;

function TdxSpreadSheetFormulaDefinedNameToken.ExtractRow(const AIndex: Integer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    Result := TTokenAccess(TResultAccess(AResult).FirstItem)
      .ExtractRow(AIndex, AErrorCode);
  finally
    AResult.Free;
  end;
end;

procedure TdxSpreadSheetFormulaDefinedNameToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    AErrorCode := AResult.ErrorCode;
    if AResult.Validate then
      AValue := AResult.Value;
  finally
    AResult.Free;
  end;
end;

function TdxSpreadSheetFormulaDefinedNameToken.UpdateValue
  (ADefinedName: TdxSpreadSheetDefinedName; const ANewName: string): Boolean;
begin
  Result := DefinedName = ADefinedName;
  if Result then
    Value := ANewName;
end;

procedure TdxSpreadSheetFormulaDefinedNameToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
var
  ALink: TdxSpreadSheetDefinedNameAccess;
begin
  ALink := TdxSpreadSheetDefinedNameAccess(DefinedName);
  if (ALink <> nil) and (ALink.Formula <> nil) and
    (ALink.Formula.ErrorCode = ecNone) then
    AResult.AddResultValue(ALink.Formula.ResultValue)
  else
    AResult.SetError(ecName);
end;

procedure TdxSpreadSheetFormulaDefinedNameToken.CalculateDimension
  (var ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    if AResult.Validate then
      TTokenAccess(TResultAccess(AResult).Items[0])
        .CalculateDimension(ADimension, AErrorCode)
    else
      AErrorCode := AResult.ErrorCode;
  finally
    AResult.Free;
  end;
end;

procedure TdxSpreadSheetFormulaDefinedNameToken.ToString(AStack: TStringList);
begin
  if Link <> nil then
    AStack.Add(Link.ToString + Value)
  else
    AStack.Add(Value);
end;

function TdxSpreadSheetFormulaDefinedNameToken.GetDefinedName
  : TdxSpreadSheetDefinedName;
var
  AData: TObject;
begin
  if DefinedNames = nil then
    Exit(nil);
  if Link <> nil then
  begin
    AData := Link.ActualData;
    if AData <> nil then
      Result := DefinedNames.GetItemByName(Value,
        AData as TdxSpreadSheetCustomView)
    else
      Result := nil;
  end
  else
  begin
    Result := DefinedNames.GetItemByName(Value,
      Owner.View as TdxSpreadSheetCustomView);
    if Result = nil then
      Result := DefinedNames.GetItemByName(Value, nil);
  end;
end;

{ TdxSpreadSheetFormulaParser }

constructor TdxSpreadSheetFormulaParser.Create(ASpreadSheet
  : TdxCustomSpreadSheet);
begin
  inherited Create;
  FSpreadSheet := ASpreadSheet;
  FormatSettings.Assign(ASpreadSheet.FormulaController.FormatSettings);
end;

function TdxSpreadSheetFormulaParser.ParseFormula(const AFormulaText: string;
  ACell: TdxSpreadSheetCell): Boolean;
var
  AFormula: TdxSpreadSheetFormula;
begin
  AFormula := TdxSpreadSheetFormula.Create(ACell);
  try
    Result := ParseFormula(AFormulaText, AFormula);
    if Result then
      ACell.AsFormula := AFormula
    else
      FreeAndNil(AFormula);
  except
    SetErrorIndex(Length(AFormula.SourceText), pecNone);
    FreeAndNil(AFormula);
    Result := False;
  end;
end;

function TdxSpreadSheetFormulaParser.ParseFormula(const AFormulaText: string;
  AFormula: TdxSpreadSheetCustomFormula): Boolean;
begin
  Result := inherited ParseFormula(AFormulaText, AFormula);
end;

function TdxSpreadSheetFormulaParser.GetViewByName(APosition, ALength: Integer;
  out AView: TObject): Boolean;
var
  ASheetIndex: Integer;
begin
  for ASheetIndex := 0 to SpreadSheet.SheetCount - 1 do
  begin
    if dxSpreadSheetCompareText(SpreadSheet.Sheets[ASheetIndex].Caption,
      @FFormulaText[APosition], ALength) = 0 then
    begin
      AView := SpreadSheet.Sheets[ASheetIndex];
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxSpreadSheetFormulaParser.Create3DReferenceAutoLink
  : TdxSpreadSheet3DReferenceCustomLink;
var
  ASheet: TdxSpreadSheetCustomView;
begin
  if (Formula <> nil) and not TFormulaAccess(Formula).IsLinkedToCell then
  begin
    ASheet := Sheet;
    if ASheet = nil then
      ASheet := SpreadSheet.ActiveSheetAsTable;
    Result := TdxSpreadSheet3DReferenceAutoLink.Create(ASheet);
  end
  else
    Result := nil;
end;

function TdxSpreadSheetFormulaParser.Create3DReferenceExternalLink
  (ALinkId: Integer; const AName: string)
  : TdxSpreadSheet3DReferenceCustomExternalLink;
begin
  Result := TdxSpreadSheet3DExternalReferenceLink.Create
    (SpreadSheet.ExternalLinks[ALinkId - 1]);
  Result.Name := AName;
end;

function TdxSpreadSheetFormulaParser.CreateDefinedNameToken
  (const AToken: TdxSpreadSheetFormulaParserToken;
  out ADefinedNameToken: TdxSpreadSheetFormulaToken;
  ALink: TdxSpreadSheet3DReferenceCustomLink = nil): Boolean;
var
  ADefinedName: string;
begin
  ADefinedName := AToken.GetString(FFormulaText);

  if ALink <> nil then
    Result := DefinedNames.Contains(ADefinedName,
      TdxSpreadSheetTableView(ALink.Data))
  else
    Result := DefinedNames.Contains(ADefinedName, Sheet) or
      DefinedNames.Contains(ADefinedName, nil);

  if Result then
  begin
    ADefinedNameToken := TdxSpreadSheetFormulaDefinedNameToken.Create
      (AToken.GetString(FFormulaSourceText), DefinedNames,
      ALink as TdxSpreadSheet3DReferenceLink)
  end;
end;

function TdxSpreadSheetFormulaParser.GetDefinedNames
  : TdxSpreadSheetDefinedNames;
begin
  Result := SpreadSheet.DefinedNames;
end;

function TdxSpreadSheetFormulaParser.GetSheet: TdxSpreadSheetTableView;
begin
  if Formula <> nil then
    Result := Formula.View as TdxSpreadSheetTableView
  else
    Result := nil;
end;

{ TdxSpreadSheetFormulaReferences }

procedure TdxSpreadSheetFormulaReferences.Initialize(ACell: TdxSpreadSheetCell;
  const AFormulaText: string);
var
  AFormula: TdxSpreadSheetFormula;
  AParser: TdxSpreadSheetFormulaReferencesParser;
begin
  Clear;

  AFormula := TdxSpreadSheetFormula.Create(ACell);
  try
    AParser := TdxSpreadSheetFormulaReferencesParser.Create(ACell.SpreadSheet);
    try
      if AParser.ParseFormula(AFormulaText, AFormula) then
      begin
        AFormula.EnumReferences(
          procedure(const AArea: TRect; ASheet: TObject;
            AToken: TdxSpreadSheetFormulaToken)
          var
            AInfo: TdxSpreadSheetFormulaReferenceInfo;
          begin
            if (ASheet = nil) or (ASheet = AFormula.View) then
            begin
              AInfo := TdxSpreadSheetFormulaReferenceInfo.Create;
              AInfo.Area := AArea;
              AInfo.PositionInText := AToken.SourceStart;
              AInfo.Length := AToken.SourceFinish - AToken.SourceStart + 1;
              Add(AInfo);
            end;
          end);
      end;
    finally
      AParser.Free;
    end;
  finally
    AFormula.Free;
  end;
end;

function TdxSpreadSheetFormulaReferences.GetItem(Index: Integer)
  : TdxSpreadSheetFormulaReferenceInfo;
begin
  Result := TdxSpreadSheetFormulaReferenceInfo(List[Index]);
end;

{ TdxSpreadSheetFormulaReferencesParser }

function TdxSpreadSheetFormulaReferencesParser.CreateTokenText
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Pop;
  Result := TdxSpreadSheetFormulaTextValueToken.Create
    (AToken.GetString(FFormulaSourceText));
  InitializeSourceBounds(Result, AToken);
end;

function TdxSpreadSheetFormulaReferencesParser.GetTokensFromStringRange
  (var APosition, AFinishPos: Integer;
AParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc)
  : TdxSpreadSheetFormulaToken;
var
  APrevToken: TdxSpreadSheetFormulaToken;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  APrevToken := nil;
  while APosition <= AFinishPos do
  begin
    AToken := GetNextToken(APosition, AFinishPos);
    if AToken = nil then
      Break;
    if Result = nil then
      Result := AToken;
    TdxSpreadSheetFormulaToken.Add(APrevToken, AToken);
    APrevToken := AToken;
  end;
  if Formula.ErrorIndex <> 0 then
    TdxSpreadSheetFormulaToken.DestroyTokens(Result);
end;

function TdxSpreadSheetFormulaReferencesParser.IsDelimiter
  (const C: Char): Boolean;
begin
  Result := Pos(C, Delimiters) > 0;
end;

function TdxSpreadSheetFormulaReferencesParser.IsText(var APosition: Integer;
AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  AIndex: Integer;
begin
  if IsDelimiter(FFormulaText[APosition]) then
  begin
    AToken.Initialize(TokenText, APosition, APosition);
    Inc(APosition);
  end
  else
  begin
    AIndex := APosition;
    while (AIndex <= AFinishPos) and not IsDelimiter(FFormulaText[AIndex]) do
      Inc(AIndex);
    AToken.Initialize(TokenText, APosition, AIndex - 1);
    APosition := AIndex;
  end;
  Result := True;
end;

procedure TdxSpreadSheetFormulaReferencesParser.SetErrorIndex
  (AErrorIndex: Integer; AErrorCode: TdxSpreadSheetFormulaParserErrorCode);
begin
  // do nothing
end;

function TdxSpreadSheetFormulaReferencesParser.ValidateTokens
  (AToken: TdxSpreadSheetFormulaToken): Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetFormulaReferencesParser.PopulateStateMachine;
var
  AState: TdxSpreadSheetFormulaParserState;
begin
  inherited;
  AState := StateMachine.CreateState(StateStart);
  AState.RemoveTokenController(IsSubExpression);
  AState.RemoveTokenController(IsArray);
  AState.RemoveTokenController(IsOperation);
  AState.AddTokenController(IsText);
  AState.SetNextState(TokenText, StateText);
  StateMachine.CreateState(StateText).SetTokenCreator(CreateTokenText);
end;

end.
