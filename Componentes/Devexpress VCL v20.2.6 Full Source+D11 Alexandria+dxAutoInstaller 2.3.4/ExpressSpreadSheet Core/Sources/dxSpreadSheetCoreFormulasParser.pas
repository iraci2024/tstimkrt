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

unit dxSpreadSheetCoreFormulasParser;

{$I cxVer.Inc}

interface

uses
  Types, Windows, SysUtils, Variants, Classes, Generics.Collections,
  Generics.Defaults,
  dxCore, dxStringHelper, dxGenerics,
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetCoreReferences,
  dxSpreadSheetFunctions,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

type

  TdxSpreadSheetFormulaParserErrorCode = (pecNone, pecUnknownIdentifier,
    pecUnknownIdentifierInParameterList, pecUnexpectedEndOfExternalReference,
    pecUnexpectedEndOfString, pecUnexpectedEndOfExpression,
    pecUnexpectedEndOfArray, pecInvalidReference, pecInvalidExpression);

  { TdxSpreadSheetFormulaParserToken }

  TdxSpreadSheetFormulaParserToken = record
    ID: Integer;
    Data: Int64;
    Data2: Int64;
    StartPosition: Integer;
    FinishPosition: Integer;

    function Equals(const AText, ACandidate: string): Boolean;
    procedure Initialize(const ATokenId, AStartPosition, AFinishPosition
      : Integer);
    function GetLength: Integer; inline;
    function GetString(const AText: string): string; inline;
    procedure Reset; inline;
  end;

  { TdxSpreadSheetFormulaParserTokenList }

  TdxSpreadSheetFormulaParserTokenList = class
    (TList<TdxSpreadSheetFormulaParserToken>)
  public
    function Pop: TdxSpreadSheetFormulaParserToken;
  end;

  TdxSpreadSheetFormulaParserCheckFunc = function(APosition,
    AFinishPos: Integer): Boolean of object;
  TdxSpreadSheetFormulaParserTokenCreator = function
    (AStack: TdxSpreadSheetFormulaParserTokenList)
    : TdxSpreadSheetFormulaToken of object;
  TdxSpreadSheetFormulaParserTokenController = function(var APosition: Integer;
    AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken)
    : Boolean of object;

  { TdxSpreadSheetFormulaParserState }

  TdxSpreadSheetFormulaParserState = class
  strict private
    FNextState: TDictionary<Integer, Integer>;
    FTokenControllers: TList<TdxSpreadSheetFormulaParserTokenController>;
    FTokenCreator: TdxSpreadSheetFormulaParserTokenCreator;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTokenController(AController
      : TdxSpreadSheetFormulaParserTokenController);
    function CreateToken(AStack: TdxSpreadSheetFormulaParserTokenList;
      ASourceOffset: Word = 0): TdxSpreadSheetFormulaToken;
    function GetNextState(ATokenId: Integer; out AStateId: Integer): Boolean;
    function GetNextToken(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    procedure RemoveTokenController(AController
      : TdxSpreadSheetFormulaParserTokenController);
    procedure SetNextState(const ATokenId, AStateId: Integer);
    procedure SetTokenCreator(ATokenCreator
      : TdxSpreadSheetFormulaParserTokenCreator);
  end;

  { TdxSpreadSheetFormulaParserStateMachine }

  TdxSpreadSheetFormulaParserStateMachine = class
  strict private
    FStates: TObjectDictionary<Integer, TdxSpreadSheetFormulaParserState>;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateState(StateId: Integer): TdxSpreadSheetFormulaParserState;
    function GetState(StateId: Integer): TdxSpreadSheetFormulaParserState;
  end;

  { TdxSpreadSheetAbstractFormulaParser }

  TdxSpreadSheetAbstractFormulaParser = class
  strict private
  const
    BreakLineAndSpaceCharacters: TdxAnsiCharSet = [#13, #10, ' '];
  strict private
    FFormatSettings: TdxSpreadSheetFormatSettings;
    FFormula: TdxSpreadSheetCustomFormula;
    FFormulaOffset: Integer;
    FParserInitialized: Boolean;
    FStateMachine: TdxSpreadSheetFormulaParserStateMachine;

    function GetR1C1Reference: Boolean; inline;
  protected const
    StateStart = 0;
  protected
    FAnchorColumn: Integer;
    FAnchorRow: Integer;
    FCanSpaceBeSignificant: Boolean;
    FErrorCode: TdxSpreadSheetFormulaParserErrorCode;
    FErrorIndex: Integer;
    FFormulaSourceText: string;
    FFormulaText: string;
    FParameterSeparatorCheckFunc: TdxSpreadSheetFormulaParserCheckFunc;

    function CheckError: Boolean; inline;
    function CheckText(APosition, AFinishPos: Integer; const ACandidate: Char)
      : Boolean; overload; inline;
    function CheckText(APosition, AFinishPos: Integer; const ACandidate: string)
      : Boolean; overload; inline;

    function IsArraySeparator(APosition, AFinishPos: Integer): Boolean; inline;
    function IsBreakChar(APosition, AFinishPos: Integer): Boolean; inline;
    function IsFormulaText(const S: string; out AOffset: Integer)
      : Boolean; virtual;
    function IsListOrArraySeparator(APosition, AFinishPos: Integer)
      : Boolean; inline;
    function IsListSeparator(APosition, AFinishPos: Integer): Boolean; inline;

    function DoFullParse(APosition, AFinishPos: Integer)
      : TdxSpreadSheetFormulaToken; virtual;
    function DoParse(var APosition, AFinishPos: Integer;
      AIsParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc)
      : TdxSpreadSheetFormulaToken; virtual;

    procedure Initialize(const AText: string;
      AAnchorRow, AAnchorColumn: Integer); virtual;
    procedure InitializeParser; virtual;
    procedure InitializeSourceBounds(AToken: TdxSpreadSheetFormulaToken;
      AStart, AFinish: Word); overload; inline;
    procedure InitializeSourceBounds(AToken: TdxSpreadSheetFormulaToken;
      const AParserToken: TdxSpreadSheetFormulaParserToken); overload; inline;

    function GetNextToken(var APosition: Integer; AFinishPos: Integer)
      : TdxSpreadSheetFormulaToken; virtual;
    function GetParameters(AParent: TdxSpreadSheetFormulaToken;
      APosition, AFinishPos: Integer;
      AIsParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc)
      : Boolean; virtual;
    function GetTokensFromStringRange(var APosition, AFinishPos: Integer;
      AIsParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc)
      : TdxSpreadSheetFormulaToken; virtual;
    procedure PopulateStateMachine; virtual; abstract;
    procedure SetErrorIndex(AErrorIndex: Integer;
      AErrorCode: TdxSpreadSheetFormulaParserErrorCode); virtual;
    procedure SkipInsignificantSpaces(var APosition: Integer;
      AFinishPos: Integer); inline;

    function CompactTokens(AToken: TdxSpreadSheetFormulaToken)
      : TdxSpreadSheetFormulaToken; virtual;
    function ValidateTokens(AToken: TdxSpreadSheetFormulaToken)
      : Boolean; virtual;

    property StateMachine: TdxSpreadSheetFormulaParserStateMachine
      read FStateMachine;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ParseFormula(const AFormulaText: string;
      AFormula: TdxSpreadSheetCustomFormula): Boolean; virtual;

    property AnchorColumn: Integer read FAnchorColumn;
    property AnchorRow: Integer read FAnchorRow;
    property ErrorCode: TdxSpreadSheetFormulaParserErrorCode read FErrorCode;
    property FormatSettings: TdxSpreadSheetFormatSettings read FFormatSettings;
    property Formula: TdxSpreadSheetCustomFormula read FFormula;
    property R1C1Reference: Boolean read GetR1C1Reference;
  end;

  { TdxSpreadSheetFormulaParserIdentList }

  TdxSpreadSheetFormulaParserIdentList = class
  strict private
    FData: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Ident: string; AData: TdxNativeInt);
    procedure Clear;
    function Contains(const S: string): Boolean;
    function Find(const S: string; var AStartPos: Integer; AFinishPos: Integer;
      out AData: TdxNativeInt): Boolean;
    procedure Remove(AData: TdxNativeInt);
  end;

  { TdxSpreadSheetCustomFormulaParser }

  TdxSpreadSheetCustomFormulaParser = class(TdxSpreadSheetAbstractFormulaParser)
  protected type
    TTokenToReferenceAdapter = function(AToken: TdxSpreadSheetFormulaToken;
      out ARow, AColumn: TdxSpreadSheetReference;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean of object;
  protected const
    TokenArray = 1;
    TokenError = 2;
    TokenFloat = 3;
    TokenIdent = 4;
    TokenInteger = 5;
    TokenOperator = 6;
    TokenReference = 7;
    TokenReferenceError = 8;
    TokenReferenceSeparator = 9;
    TokenFullColumnReference = 10;
    TokenString = 11;
    TokenSubExpression = 12;
    TokenExternalReference = 13;
    TokenLast = TokenExternalReference;

    StateArray = 1;
    StateError = 2;
    StateErrorOrReference = 3;
    StateFunction = 4;
    StateIdent = 5;
    StateNumber = 6;
    StateOperator = 8;
    StateStringOr3DReference = 9;
    StateSubExpression = 10;
    State3DReference = 11;
    State3DReferencePre1 = 12;
    State3DReferencePre2 = 13;
    StateExternalReferencePre1 = 14;
    StateExternalReferencePre2 = 15;
    StateFullColumnReference = 16;
    StateExternalNameOrFunction = 17;
    StateExternalFunction = 18;
    StateLast = StateExternalFunction;
  protected
    FIsRangeOperatorSupported: Boolean;
    FKnownErrors: TdxSpreadSheetFormulaParserIdentList;
    FKnownOperations: TdxSpreadSheetFormulaParserIdentList;
    FTokenToReferenceAdapters: TdxClassDictionary<TTokenToReferenceAdapter>;

    function FindStringRightBound(const S: string; const AMark: Char;
      APosition, AFinishPos: Integer): Integer; inline;
    function FindSubExpressionRightBound(const AClosingTag: Char;
      AStartPos, AFinishPos: Integer): Integer; inline;
    function GetSourceStart(AStack: TdxSpreadSheetFormulaParserTokenList)
      : Word; inline;
    function SkipString(const AMark: Char; AStartPos, AFinishPos: Integer)
      : Integer; inline;
    function SkipSubExpression(const AOpeningTag, AClosingTag: Char;
      AStartPos, AFinishPos: Integer): Integer; inline;

    function CreateToken3DReference
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenArray(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenError(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenExternalFunction
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenExternalName
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenFullColumnReference
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenFunction(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenIdent(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken; virtual;
    function CreateTokenNumber(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenOperator(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenString(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function CreateTokenSubExpression
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;

    function Create3DAreaReferenceToken(const ARow, AColumn, ARow2,
      AColumn2: TdxSpreadSheetReference;
      ALink1: TdxSpreadSheet3DReferenceCustomLink = nil;
      ALink2: TdxSpreadSheet3DReferenceCustomLink = nil)
      : TdxSpreadSheetFormulaToken; inline;
    function Create3DReferenceAutoLink
      : TdxSpreadSheet3DReferenceCustomLink; virtual;
    function Create3DReferenceExternalLink(ALinkId: Integer;
      const AName: string)
      : TdxSpreadSheet3DReferenceCustomExternalLink; virtual;
    function Create3DReferenceLink(AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheet3DReferenceCustomLink;
    function Create3DReferenceToken(const ARow,
      AColumn: TdxSpreadSheetReference;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil)
      : TdxSpreadSheetFormulaToken; inline;
    function CreateDefinedNameToken(const AToken
      : TdxSpreadSheetFormulaParserToken;
      out ADefinedNameToken: TdxSpreadSheetFormulaToken;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil): Boolean; virtual;
    function CreateUnknownNameToken(const AToken
      : TdxSpreadSheetFormulaParserToken;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil)
      : TdxSpreadSheetFormulaToken;

    function IsArray(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsError(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsExternalReference(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsFunctionParameters(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsIdent(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsNumber(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsOperation(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsReference(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsReferenceSeparator(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsString(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
    function IsSubExpression(var APosition: Integer; AFinishPos: Integer;
      var AToken: TdxSpreadSheetFormulaParserToken): Boolean;

    function TokenAreaReferenceAsReference(AToken: TdxSpreadSheetFormulaToken;
      out ARow, AColumn: TdxSpreadSheetReference;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
    function TokenErrorAsReference(AToken: TdxSpreadSheetFormulaToken;
      out ARow, AColumn: TdxSpreadSheetReference;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
    function TokenIntegerAsReference(AToken: TdxSpreadSheetFormulaToken;
      out ARow, AColumn: TdxSpreadSheetReference;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
    function TokenReferenceAsReference(AToken: TdxSpreadSheetFormulaToken;
      out ARow, AColumn: TdxSpreadSheetReference;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
    function Token3DReferenceAsReference(AToken: TdxSpreadSheetFormulaToken;
      out ARow, AColumn: TdxSpreadSheetReference;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;

    function CheckReference(const AToken: TdxSpreadSheetFormulaParserToken;
      var ARow, AColumn: TdxSpreadSheetReference): Boolean; inline;
    function GetExternalLinkInfo(const AToken: TdxSpreadSheetFormulaParserToken;
      out ALinkId: Integer; out AName: string;
      AStack: TdxSpreadSheetFormulaParserTokenList = nil): Boolean;
    function GetNextToken(var APosition: Integer; AFinishPos: Integer)
      : TdxSpreadSheetFormulaToken; override;
    function GetViewByName(APosition, ALength: Integer; out AView: TObject)
      : Boolean; virtual;
    function ParseReference(var AStartPos, AFinishPos: Integer;
      var ARow, AColumn: TdxSpreadSheetReference): Boolean; inline;

    procedure InitializeParser; override;
    procedure ValidateReference(AIndex: Integer;
      var AReference: TdxSpreadSheetReference);
    procedure ValidateReferences(var ARow, AColumn: TdxSpreadSheetReference);

    procedure PopulateKnownErrors; virtual;
    procedure PopulateKnownOperations; virtual;
    procedure PopulateStateMachine; override;
    procedure PopulateTokenToReferenceAdapters; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

function dxBeginsWith(const ASubText, AText: string;
  AStartPos, AFinishPos: Integer): Boolean; inline;
function dxSpreadSheetFormulaExcludeEqualSymbol(const S: string): string;
function dxSpreadSheetFormulaIncludeEqualSymbol(const S: string): string;

implementation

uses
  dxSpreadSheetCoreStrs, Math, StrUtils, cxGeometry;

type
  TFormulaAccess = class(TdxSpreadSheetCustomFormula);

function dxBeginsWith(const ASubText, AText: string;
  AStartPos, AFinishPos: Integer): Boolean; inline;
var
  L: Integer;
begin
  L := Length(ASubText);
  Result := (L > 0) and (AStartPos + L - 1 <= AFinishPos) and
    CompareMem(@AText[AStartPos], @ASubText[1], L * SizeOf(Char));
end;

function dxSpreadSheetFormulaExcludeEqualSymbol(const S: string): string;
begin
  if (Length(S) > 0) and (S[1] = dxDefaultOperations[opEQ]) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

function dxSpreadSheetFormulaIncludeEqualSymbol(const S: string): string;
begin
  if (Length(S) > 0) and (S[1] <> dxDefaultOperations[opEQ]) then
    Result := dxDefaultOperations[opEQ] + S
  else
    Result := S;
end;

{ TdxSpreadSheetFormulaParserToken }

function TdxSpreadSheetFormulaParserToken.Equals(const AText,
  ACandidate: string): Boolean;
begin
  Result := (Length(ACandidate) = GetLength) and
    CompareMem(@AText[StartPosition], @ACandidate[1], GetLength);
end;

procedure TdxSpreadSheetFormulaParserToken.Initialize(const ATokenId,
  AStartPosition, AFinishPosition: Integer);
begin
  Reset;
  ID := ATokenId;
  StartPosition := AStartPosition;
  FinishPosition := AFinishPosition;
end;

function TdxSpreadSheetFormulaParserToken.GetLength: Integer;
begin
  Result := FinishPosition - StartPosition + 1;
end;

function TdxSpreadSheetFormulaParserToken.GetString
  (const AText: string): string;
begin
  Result := Copy(AText, StartPosition, GetLength);
end;

procedure TdxSpreadSheetFormulaParserToken.Reset;
begin
  ZeroMemory(@Self, SizeOf(Self));
end;

{ TdxSpreadSheetFormulaParserTokenList }

function TdxSpreadSheetFormulaParserTokenList.Pop
  : TdxSpreadSheetFormulaParserToken;
begin
  Result := Last;
  Delete(Count - 1);
end;

{ TdxSpreadSheetFormulaParserState }

constructor TdxSpreadSheetFormulaParserState.Create;
begin
  FNextState := TDictionary<Integer, Integer>.Create;
  FTokenControllers := TList<TdxSpreadSheetFormulaParserTokenController>.Create;
end;

destructor TdxSpreadSheetFormulaParserState.Destroy;
begin
  FreeAndNil(FTokenControllers);
  FreeAndNil(FNextState);
  inherited;
end;

procedure TdxSpreadSheetFormulaParserState.AddTokenController
  (AController: TdxSpreadSheetFormulaParserTokenController);
begin
  FTokenControllers.Add(AController);
end;

function TdxSpreadSheetFormulaParserState.CreateToken
  (AStack: TdxSpreadSheetFormulaParserTokenList; ASourceOffset: Word = 0)
  : TdxSpreadSheetFormulaToken;
begin
  if Assigned(FTokenCreator) then
    Result := FTokenCreator(AStack)
  else
    Result := nil;
end;

function TdxSpreadSheetFormulaParserState.GetNextState(ATokenId: Integer;
  out AStateId: Integer): Boolean;
begin
  Result := FNextState.TryGetValue(ATokenId, AStateId);
end;

function TdxSpreadSheetFormulaParserState.GetNextToken(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  I: Integer;
begin
  for I := 0 to FTokenControllers.Count - 1 do
  begin
    if FTokenControllers{$IFDEF DELPHIXE3}.List{$ENDIF}[I]
      (APosition, AFinishPos, AToken) then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxSpreadSheetFormulaParserState.RemoveTokenController
  (AController: TdxSpreadSheetFormulaParserTokenController);
{$IFNDEF DELPHIXE2}
var
  AItem: TdxSpreadSheetFormulaParserTokenController;
  I: Integer;
{$ENDIF}
begin
{$IFDEF DELPHIXE2}
  FTokenControllers.Remove(AController);
{$ELSE}
  for I := 0 to FTokenControllers.Count - 1 do
  begin
    AItem := FTokenControllers[I];
    if dxSameMethods(AItem, AController) then
    begin
      FTokenControllers.Delete(I);
      Break;
    end;
  end;
{$ENDIF}
end;

procedure TdxSpreadSheetFormulaParserState.SetNextState(const ATokenId,
  AStateId: Integer);
begin
  FNextState.AddOrSetValue(ATokenId, AStateId);
end;

procedure TdxSpreadSheetFormulaParserState.SetTokenCreator
  (ATokenCreator: TdxSpreadSheetFormulaParserTokenCreator);
begin
  FTokenCreator := ATokenCreator;
end;

{ TdxSpreadSheetFormulaParserStateMachine }

constructor TdxSpreadSheetFormulaParserStateMachine.Create;
begin
  FStates := TObjectDictionary<Integer, TdxSpreadSheetFormulaParserState>.Create
    ([doOwnsValues]);
end;

destructor TdxSpreadSheetFormulaParserStateMachine.Destroy;
begin
  FreeAndNil(FStates);
  inherited;
end;

function TdxSpreadSheetFormulaParserStateMachine.CreateState(StateId: Integer)
  : TdxSpreadSheetFormulaParserState;
begin
  if not FStates.TryGetValue(StateId, Result) then
  begin
    Result := TdxSpreadSheetFormulaParserState.Create;
    FStates.Add(StateId, Result);
  end;
end;

function TdxSpreadSheetFormulaParserStateMachine.GetState(StateId: Integer)
  : TdxSpreadSheetFormulaParserState;
begin
  Result := FStates.Items[StateId];
end;

{ TdxSpreadSheetAbstractFormulaParser }

constructor TdxSpreadSheetAbstractFormulaParser.Create;
begin
  FParserInitialized := False;
  FStateMachine := TdxSpreadSheetFormulaParserStateMachine.Create;
  FFormatSettings := TdxSpreadSheetFormatSettings.Create;
end;

destructor TdxSpreadSheetAbstractFormulaParser.Destroy;
begin
  FreeAndNil(FFormatSettings);
  FreeAndNil(FStateMachine);
  inherited;
end;

function TdxSpreadSheetAbstractFormulaParser.ParseFormula(const AFormulaText
  : string; AFormula: TdxSpreadSheetCustomFormula): Boolean;
begin
  FFormula := AFormula;
  Result := IsFormulaText(AFormulaText, FFormulaOffset);
  if Result then
  begin
    Initialize(Copy(AFormulaText, FFormulaOffset + 1, MaxInt),
      Formula.AnchorRow, Formula.AnchorColumn);
    TFormulaAccess(FFormula).DestroyTokens;
    TFormulaAccess(FFormula).SourceText := FFormulaSourceText;
    TFormulaAccess(FFormula).FTokens := DoFullParse(1, Length(FFormulaText));
  end;
end;

function TdxSpreadSheetAbstractFormulaParser.CheckError: Boolean;
begin
  Result := FErrorIndex = 0;
end;

function TdxSpreadSheetAbstractFormulaParser.CheckText(APosition,
  AFinishPos: Integer; const ACandidate: Char): Boolean;
begin
  Result := (APosition <= AFinishPos) and
    (Ord(FFormulaText[APosition]) = Ord(ACandidate));
end;

function TdxSpreadSheetAbstractFormulaParser.CheckText(APosition,
  AFinishPos: Integer; const ACandidate: string): Boolean;
begin
  Result := dxBeginsWith(ACandidate, FFormulaText, APosition, AFinishPos);
end;

function TdxSpreadSheetAbstractFormulaParser.IsArraySeparator(APosition,
  AFinishPos: Integer): Boolean;
begin
  Result := CheckText(APosition, AFinishPos, FormatSettings.ArraySeparator);
end;

function TdxSpreadSheetAbstractFormulaParser.IsBreakChar(APosition,
  AFinishPos: Integer): Boolean;
begin
  Result := (APosition <= AFinishPos) and
    (Pos(FFormulaText[APosition], FormatSettings.BreakChars) > 0);
end;

function TdxSpreadSheetAbstractFormulaParser.IsFormulaText(const S: string;
  out AOffset: Integer): Boolean;
begin
  Result := StartsStr(FormatSettings.Operations[opEQ], S);
  if Result then
    AOffset := Length(FormatSettings.Operations[opEQ]);
end;

function TdxSpreadSheetAbstractFormulaParser.IsListOrArraySeparator(APosition,
  AFinishPos: Integer): Boolean;
var
  ACharCode: Word;
begin
  Result := False;
  if APosition <= AFinishPos then
  begin
    ACharCode := Ord(FFormulaText[APosition]);
    Result := (ACharCode = Ord(FFormatSettings.ArraySeparator)) or
      (ACharCode = Ord(FFormatSettings.ListSeparator));
  end;
end;

function TdxSpreadSheetAbstractFormulaParser.IsListSeparator(APosition,
  AFinishPos: Integer): Boolean;
begin
  Result := CheckText(APosition, AFinishPos, FormatSettings.ListSeparator);
end;

function TdxSpreadSheetAbstractFormulaParser.DoFullParse(APosition,
  AFinishPos: Integer): TdxSpreadSheetFormulaToken;
var
  ASavePos: Integer;
begin
  ASavePos := APosition;
  Result := DoParse(APosition, AFinishPos, IsListSeparator);
  if CheckError and (APosition < AFinishPos) then
  begin
    if IsListSeparator(APosition, AFinishPos) then
    begin
      TdxSpreadSheetFormulaToken.DestroyTokens(Result);
      Result := TdxSpreadSheetListToken.Create;
      Result.Owner := Formula;
      GetParameters(Result, ASavePos, AFinishPos, FParameterSeparatorCheckFunc);
      InitializeSourceBounds(Result, ASavePos, AFinishPos);
    end
    else
    begin
      SetErrorIndex(APosition, pecUnknownIdentifierInParameterList);
      if not CheckError then
        TdxSpreadSheetFormulaToken.DestroyTokens(Result);
    end;
  end;
end;

function TdxSpreadSheetAbstractFormulaParser.DoParse(var APosition,
  AFinishPos: Integer; AIsParameterSeparatorProc
  : TdxSpreadSheetFormulaParserCheckFunc): TdxSpreadSheetFormulaToken;

  procedure AddTokenFromStack(var AList, AStackPeek
    : TdxSpreadSheetFormulaToken); inline;
  var
    AToken: TdxSpreadSheetFormulaToken;
  begin
    AToken := AStackPeek;
    AStackPeek := AToken.Prev;
    AToken.ExtractFromList;
    AList := TdxSpreadSheetFormulaToken.Add(AList, AToken);
  end;

  function IsUnaryMinus(AToken: TdxSpreadSheetFormulaToken): Boolean; inline;
  begin
    Result := (AToken.ClassType = TdxSpreadSheetFormulaOperationToken) and
      (TdxSpreadSheetFormulaOperationToken(AToken).Operation = opUminus);
  end;

var
  ANextToken: TdxSpreadSheetFormulaToken;
  AStack: TdxSpreadSheetFormulaToken;
  AStartPosition: Integer;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  AStack := nil;
  AStartPosition := APosition;
  AToken := GetTokensFromStringRange(APosition, AFinishPos,
    AIsParameterSeparatorProc);
  while AToken <> nil do
  begin
    ANextToken := AToken.Next;
    AToken.ExtractFromList;
    if AToken.Priority < 0 then
      Result := TdxSpreadSheetFormulaToken.Add(Result, AToken)
    else
    begin
      while (AStack <> nil) and (AStack.Priority >= AToken.Priority) and
        not(IsUnaryMinus(AStack) and IsUnaryMinus(AToken)) do
        AddTokenFromStack(Result, AStack);
      AStack := TdxSpreadSheetFormulaToken.Add(AStack, AToken);
    end;
    AToken := ANextToken;
  end;
  while AStack <> nil do
    AddTokenFromStack(Result, AStack);

  if Result <> nil then
  begin
    Result := Result.FirstSibling;
    if not ValidateTokens(Result) then
    begin
      SetErrorIndex(AStartPosition, pecInvalidExpression);
      if not CheckError then
        TdxSpreadSheetFormulaToken.DestroyTokens(Result);
    end;
  end;
end;

function TdxSpreadSheetAbstractFormulaParser.GetNextToken
  (var APosition: Integer; AFinishPos: Integer): TdxSpreadSheetFormulaToken;
var
  ANextStateId: Integer;
  AStack: TdxSpreadSheetFormulaParserTokenList;
  AState: TdxSpreadSheetFormulaParserState;
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  Result := nil;
  SkipInsignificantSpaces(APosition, AFinishPos);
  if APosition > AFinishPos then
    Exit;
  if FFormulaText[APosition] = '' then
  begin
    Inc(APosition);
    Exit;
  end;

  AStack := TdxSpreadSheetFormulaParserTokenList.Create;
  try
    AStack.Capacity := 4;
    ANextStateId := StateStart;
    while CheckError do
    begin
      AState := StateMachine.GetState(ANextStateId);
      if AState.GetNextToken(APosition, AFinishPos, AToken) then
        AStack.Add(AToken)
      else
      begin
        Result := AState.CreateToken(AStack, FFormulaOffset);
        Break;
      end;
      if not AState.GetNextState(AToken.ID, ANextStateId) then
      begin
        Result := AState.CreateToken(AStack, FFormulaOffset);
        Break;
      end;
    end;
  finally
    AStack.Free;
  end;

  if Result = nil then
  begin
    SetErrorIndex(APosition, pecUnknownIdentifier);
    Exit;
  end;

{$IFDEF DEBUG}
  dxTestCheck(Result.SourceStart > 0, Result.ClassName +
    '''s source bounds was not defined');
{$ENDIF}
  Result.Owner := Formula;
end;

function TdxSpreadSheetAbstractFormulaParser.GetParameters
  (AParent: TdxSpreadSheetFormulaToken; APosition, AFinishPos: Integer;
  AIsParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc): Boolean;

  procedure AddParameter(ATokens: TdxSpreadSheetFormulaToken;
    ASourceStart, ASourceFinish: Integer);
  var
    AParameter: TdxSpreadSheetFormulaToken;
  begin
    if ATokens = nil then
      Exit;
    if CheckError then
    begin
      if ATokens.ClassType = TdxSpreadSheetFormulaNullToken then
      begin
        Dec(ASourceStart);
        ASourceFinish := ASourceStart - 1;
        InitializeSourceBounds(ATokens, ASourceStart, ASourceFinish);
      end;

      AParameter := TdxSpreadSheetFormulaToken.Create;
      TdxSpreadSheetFormulaToken.AddChild(AParameter, ATokens);
      TdxSpreadSheetFormulaToken.AddChild(AParent, AParameter);
      InitializeSourceBounds(AParameter, ASourceStart, ASourceFinish);
    end
    else
      TdxSpreadSheetFormulaToken.DestroyTokens(ATokens);
  end;

var
  AParameter: TdxSpreadSheetFormulaToken;
  AStartPos: Integer;
begin
  while CheckError and (APosition <= AFinishPos) do
  begin
    AStartPos := APosition;
    AParameter := DoParse(APosition, AFinishPos, AIsParameterSeparatorProc);
    AddParameter(AParameter, AStartPos, APosition - 1);

    if IsListSeparator(APosition, AFinishPos) then
    begin
      while APosition <= AFinishPos do
      begin
        Inc(APosition);
        SkipInsignificantSpaces(APosition, AFinishPos);
        if (APosition > AFinishPos) or IsListSeparator(APosition, AFinishPos)
        then
          AddParameter(TdxSpreadSheetFormulaNullToken.Create, APosition, -1)
        else
          Break;
      end;
    end
    else

      if IsArraySeparator(APosition, AFinishPos) then
    begin
      AParameter := TdxSpreadSheetFormulaArrayRowSeparator.Create;
      InitializeSourceBounds(AParameter, APosition, APosition);
      TdxSpreadSheetFormulaToken.AddChild(AParent, AParameter);
      Inc(APosition)
    end
    else
    begin
      if APosition < AFinishPos then
        SetErrorIndex(APosition, pecUnknownIdentifierInParameterList);
      Break;
    end;
  end;
  Result := CheckError;
end;

procedure TdxSpreadSheetAbstractFormulaParser.Initialize(const AText: string;
  AAnchorRow, AAnchorColumn: Integer);
begin
  if not FParserInitialized then
  begin
    InitializeParser;
    FParserInitialized := True;
  end;

  FErrorIndex := 0;
  FErrorCode := pecNone;
  FAnchorRow := AAnchorRow;
  FAnchorColumn := AAnchorColumn;
  FFormulaSourceText := AText;
  FFormulaText := UpperCase(FFormulaSourceText);
end;

procedure TdxSpreadSheetAbstractFormulaParser.InitializeParser;
begin
  FParameterSeparatorCheckFunc := IsListOrArraySeparator;
end;

procedure TdxSpreadSheetAbstractFormulaParser.InitializeSourceBounds
  (AToken: TdxSpreadSheetFormulaToken;
  const AParserToken: TdxSpreadSheetFormulaParserToken);
begin
  InitializeSourceBounds(AToken, AParserToken.StartPosition,
    AParserToken.FinishPosition);
end;

procedure TdxSpreadSheetAbstractFormulaParser.InitializeSourceBounds
  (AToken: TdxSpreadSheetFormulaToken; AStart, AFinish: Word);
begin
  if AToken <> nil then
  begin
    AToken.SourceStart := AStart + FFormulaOffset;
    AToken.SourceFinish := AFinish + FFormulaOffset;
  end;
end;

function TdxSpreadSheetAbstractFormulaParser.GetTokensFromStringRange
  (var APosition, AFinishPos: Integer;
  AIsParameterSeparatorProc: TdxSpreadSheetFormulaParserCheckFunc)
  : TdxSpreadSheetFormulaToken;
var
  APrevToken: TdxSpreadSheetFormulaToken;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  APrevToken := nil;

  SkipInsignificantSpaces(APosition, AFinishPos);
  if AIsParameterSeparatorProc(APosition, AFinishPos) then
    Result := TdxSpreadSheetFormulaNullToken.Create
  else
    while (APosition <= AFinishPos) and not AIsParameterSeparatorProc(APosition,
      AFinishPos) do
    begin
      AToken := GetNextToken(APosition, AFinishPos);
      if AToken = nil then
        Break;
      if Result = nil then
        Result := AToken;
      TdxSpreadSheetFormulaToken.Add(APrevToken, AToken);
      APrevToken := AToken;
      SkipInsignificantSpaces(APosition, AFinishPos);
    end;

  Result := CompactTokens(Result);
  if Result = nil then
    SetErrorIndex(APosition, pecInvalidExpression);
  if not CheckError then
    TdxSpreadSheetFormulaToken.DestroyTokens(Result);
end;

procedure TdxSpreadSheetAbstractFormulaParser.SetErrorIndex
  (AErrorIndex: Integer; AErrorCode: TdxSpreadSheetFormulaParserErrorCode);
const
  ErrorCodeMap: array [TdxSpreadSheetFormulaParserErrorCode]
    of TdxSpreadSheetFormulaErrorCode = (ecNone, ecName, ecName, ecName, ecNone,
    ecNone, ecNone, ecName, ecNone);
begin
  if CheckError then
  begin
    FErrorIndex := AErrorIndex;
    FErrorCode := AErrorCode;
    if Formula <> nil then
      TFormulaAccess(Formula).SetError(ErrorCodeMap[AErrorCode], AErrorIndex);
  end;
  if Formula <> nil then
    TdxSpreadSheetFormulaToken.DestroyTokens(TFormulaAccess(Formula).FTokens);
end;

procedure TdxSpreadSheetAbstractFormulaParser.SkipInsignificantSpaces
  (var APosition: Integer; AFinishPos: Integer);
var
  P: Integer;
begin
  if FCanSpaceBeSignificant then
  begin
    while (APosition <= AFinishPos) and dxCharInSet(FFormulaText[APosition],
      dxBreakLineCharacters) do
      Inc(APosition);

    if CheckText(APosition, AFinishPos, ' ') then
    begin
      P := APosition;
      while CheckText(P, AFinishPos, ' ') do
        Inc(P);
      if (APosition = 1) or (P > AFinishPos) or
        IsBreakChar(APosition - 1, AFinishPos) or IsBreakChar(P, AFinishPos)
      then
      begin
        APosition := P;
        while (APosition <= AFinishPos) and dxCharInSet(FFormulaText[APosition],
          dxBreakLineCharacters) do
          Inc(APosition);
      end;
    end;
  end
  else
    while (APosition <= AFinishPos) and dxCharInSet(FFormulaText[APosition],
      BreakLineAndSpaceCharacters) do
      Inc(APosition);
end;

function TdxSpreadSheetAbstractFormulaParser.CompactTokens
  (AToken: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaToken;
var
  ANextToken: TdxSpreadSheetFormulaToken;
  AOperationToken: TdxSpreadSheetFormulaOperationToken;
  APrevToken: TdxSpreadSheetFormulaToken;
begin
  Result := AToken;
  while AToken <> nil do
  begin
    APrevToken := AToken.Prev;
    ANextToken := AToken.Next;

    if AToken.ClassType = TdxSpreadSheetFormulaOperationToken then
    begin
      AOperationToken := TdxSpreadSheetFormulaOperationToken(AToken);
      case AOperationToken.Operation of
        opIsect:
          if not((APrevToken is TdxSpreadSheetFormulaReferenceToken) and
            (ANextToken is TdxSpreadSheetFormulaReferenceToken)) then
            TdxSpreadSheetFormulaToken.Remove(Result, AToken);

        opPercent:
          if APrevToken = nil then
          begin
            TdxSpreadSheetFormulaToken.DestroyTokens(Result);
            Exit;
          end;

      else
        AOperationToken.CheckNeighbors;
        if ANextToken is TdxSpreadSheetFormulaNumericValueToken then
        begin
          if TdxSpreadSheetFormulaNumericValueToken(ANextToken)
            .CombineWithUnaryOperation(AOperationToken.Operation) then
            TdxSpreadSheetFormulaToken.Remove(Result, AToken);
        end;
      end;
    end;

    AToken := ANextToken;
  end;
end;

function TdxSpreadSheetAbstractFormulaParser.ValidateTokens
  (AToken: TdxSpreadSheetFormulaToken): Boolean;

  function SkipOperand(var AToken: TdxSpreadSheetFormulaToken): Boolean;
  var
    AOperation: TdxSpreadSheetFormulaOperation;
  begin
    if AToken = nil then
      Exit(False);
    if AToken.ClassType = TdxSpreadSheetFormulaOperationToken then
    begin
      AOperation := TdxSpreadSheetFormulaOperationToken(AToken).Operation;
      AToken := AToken.Prev;
      Result := SkipOperand(AToken);
      if not(AOperation in [opUplus, opUminus, opPercent]) then
        Result := Result and SkipOperand(AToken);
    end
    else
    begin
      AToken := AToken.Prev;
      Result := True;
    end;
  end;

begin
  AToken := AToken.LastSibling;
  Result := SkipOperand(AToken) and (AToken = nil);
end;

function TdxSpreadSheetAbstractFormulaParser.GetR1C1Reference: Boolean;
begin
  Result := FormatSettings.R1C1Reference;
end;

{ TdxSpreadSheetFormulaParserIdentList }

constructor TdxSpreadSheetFormulaParserIdentList.Create;
begin
  inherited Create;
  FData := TStringList.Create;
end;

destructor TdxSpreadSheetFormulaParserIdentList.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TdxSpreadSheetFormulaParserIdentList.Add(const Ident: string;
  AData: TdxNativeInt);
var
  L, H, I, C: Integer;
begin
  if FData.IndexOf(Ident) < 0 then
  begin
    L := 0;
    H := FData.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := Length(FData[I]) - Length(Ident);
      if C < 0 then
        L := I + 1
      else
        H := I - 1;
    end;
    FData.InsertObject(L, Ident, TObject(AData));
  end;
end;

procedure TdxSpreadSheetFormulaParserIdentList.Clear;
begin
  FData.Clear;
end;

function TdxSpreadSheetFormulaParserIdentList.Contains(const S: string)
  : Boolean;
begin
  Result := FData.IndexOf(S) >= 0;
end;

function TdxSpreadSheetFormulaParserIdentList.Find(const S: string;
  var AStartPos: Integer; AFinishPos: Integer; out AData: TdxNativeInt)
  : Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FData.Count - 1 downto 0 do
  begin
    Result := dxBeginsWith(FData[I], S, AStartPos, AFinishPos);
    if Result then
    begin
      AData := TdxNativeInt(FData.Objects[I]);
      Inc(AStartPos, Length(FData[I]));
      Break;
    end;
  end;
end;

procedure TdxSpreadSheetFormulaParserIdentList.Remove(AData: TdxNativeInt);
var
  AIndex: Integer;
begin
  AIndex := FData.IndexOfObject(TObject(AData));
  if AIndex <> -1 then
    FData.Delete(AIndex);
end;

{ TdxSpreadSheetCustomFormulaParser }

constructor TdxSpreadSheetCustomFormulaParser.Create;
begin
  inherited;
  FKnownErrors := TdxSpreadSheetFormulaParserIdentList.Create;
  FKnownOperations := TdxSpreadSheetFormulaParserIdentList.Create;
  FTokenToReferenceAdapters :=
    TdxClassDictionary<TTokenToReferenceAdapter>.Create;
end;

destructor TdxSpreadSheetCustomFormulaParser.Destroy;
begin
  FreeAndNil(FTokenToReferenceAdapters);
  FreeAndNil(FKnownOperations);
  FreeAndNil(FKnownErrors);
  inherited;
end;

function TdxSpreadSheetCustomFormulaParser.FindStringRightBound(const S: string;
  const AMark: Char; APosition, AFinishPos: Integer): Integer;
begin
  Result := -1;
  while APosition <= AFinishPos do
  begin
    if Ord(S[APosition]) = Ord(AMark) then
    begin
      if (APosition + 1 <= AFinishPos) and (Ord(S[APosition + 1]) = Ord(AMark))
      then
        Inc(APosition, 2)
      else
        Exit(APosition - 1);
    end
    else
      Inc(APosition);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.FindSubExpressionRightBound
  (const AClosingTag: Char; AStartPos, AFinishPos: Integer): Integer;
var
  AChar: Word;
begin
  Result := -1;
  while (AStartPos <= AFinishPos) and CheckError do
  begin
    AChar := Ord(FFormulaText[AStartPos]);
    if AChar = Ord(dxStringMarkChar) then
      AStartPos := SkipString(dxStringMarkChar, AStartPos, AFinishPos)
    else if AChar = Ord(dxStringMarkChar2) then
      AStartPos := SkipString(dxStringMarkChar2, AStartPos, AFinishPos)
    else if AChar = Ord(dxLeftParenthesis) then
      AStartPos := SkipSubExpression(dxLeftParenthesis, dxRightParenthesis,
        AStartPos, AFinishPos)
    else if AChar = Ord(dxLeftArrayParenthesis) then
      AStartPos := SkipSubExpression(dxLeftArrayParenthesis,
        dxRightArrayParenthesis, AStartPos, AFinishPos)
    else if AChar = Ord(AClosingTag) then
      Exit(AStartPos - 1)
    else
      Inc(AStartPos);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.GetSourceStart
  (AStack: TdxSpreadSheetFormulaParserTokenList): Word;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.First;
  Result := AToken.StartPosition;
  if AToken.ID in [TokenString, TokenExternalReference] then
    Dec(Result);
end;

function TdxSpreadSheetCustomFormulaParser.SkipString(const AMark: Char;
  AStartPos, AFinishPos: Integer): Integer;
begin
  Result := FindStringRightBound(FFormulaText, AMark, AStartPos + 1,
    AFinishPos);
  if Result > 0 then
    Inc(Result, 2)
  else
  begin
    SetErrorIndex(AStartPos, pecUnexpectedEndOfString);
    Result := AFinishPos + 1;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.SkipSubExpression(const AOpeningTag,
  AClosingTag: Char; AStartPos, AFinishPos: Integer): Integer;
begin
  Result := FindSubExpressionRightBound(AClosingTag, AStartPos + 1, AFinishPos);
  if Result > 0 then
    Inc(Result, 2)
  else
  begin
    SetErrorIndex(AStartPos, pecUnexpectedEndOfExpression);
    Result := AFinishPos + 1;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenExternalFunction
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  ANameToken: TdxSpreadSheetFormulaParserToken;
  AParamsToken: TdxSpreadSheetFormulaParserToken;
  AStartPosition: Word;
begin
  AStartPosition := GetSourceStart(AStack);
  AParamsToken := AStack.Pop;
  ANameToken := AStack.Pop;
  Result := TdxSpreadSheetFormulaUnknownFunctionToken.Create
    (ANameToken.GetString(FFormulaSourceText), Create3DReferenceLink(AStack));
  GetParameters(Result, AParamsToken.StartPosition, AParamsToken.FinishPosition,
    FParameterSeparatorCheckFunc);
  InitializeSourceBounds(Result, AStartPosition, AParamsToken.FinishPosition +
    1 { Length(dxRightParenthesis) } );
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenExternalName
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AStartPosition: Word;
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AStartPosition := GetSourceStart(AStack);
  AToken := AStack.Pop;
  Result := CreateUnknownNameToken(AToken, Create3DReferenceLink(AStack));
  InitializeSourceBounds(Result, AStartPosition, AToken.FinishPosition);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenFullColumnReference
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  ASourceStart: Word;
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  ASourceStart := GetSourceStart(AStack);

  AToken := AStack.Pop;
  Result := Create3DAreaReferenceToken(TdxSpreadSheetReference.AllItems,
    TdxSpreadSheetReference.Create(AToken.Data),
    TdxSpreadSheetReference.AllItems,
    TdxSpreadSheetReference.Create(AToken.Data2),
    Create3DReferenceLink(AStack), nil);

  InitializeSourceBounds(Result, ASourceStart, AToken.FinishPosition);
end;

function TdxSpreadSheetCustomFormulaParser.CreateToken3DReference
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AColumn, ARow: TdxSpreadSheetReference;
  AToken: TdxSpreadSheetFormulaParserToken;
  ALink: TdxSpreadSheet3DReferenceCustomLink;
  ASourceStart: Word;
begin
  ASourceStart := GetSourceStart(AStack);

  AToken := AStack.Pop;
  ALink := Create3DReferenceLink(AStack);
  if AToken.ID = TokenReference then
  begin
    Result := Create3DReferenceToken
      (TdxSpreadSheetReference.Create(AToken.Data),
      TdxSpreadSheetReference.Create(AToken.Data2), ALink);
  end
  else if not CreateDefinedNameToken(AToken, Result, ALink) then
  begin
    if CheckReference(AToken, ARow, AColumn) then
      Result := Create3DReferenceToken(ARow, AColumn, ALink)
    else
      Result := CreateUnknownNameToken(AToken, ALink);
  end;

  InitializeSourceBounds(Result, ASourceStart, AToken.FinishPosition);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenArray
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Last;
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  GetParameters(Result, AToken.StartPosition, AToken.FinishPosition,
    IsListOrArraySeparator);
  InitializeSourceBounds(Result, AToken.StartPosition -
    1 { Length(dxLeftParenthesis) } , AToken.FinishPosition +
    1 { Length(dxRightArrayParenthesis) } );
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenError
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Pop;
  Result := TdxSpreadSheetFormulaErrorValueToken.Create
    (TdxSpreadSheetFormulaErrorCode(AToken.Data));
  InitializeSourceBounds(Result, AToken);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenFunction
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AInfo: TdxSpreadSheetFunctionInfo;
  ANameToken: TdxSpreadSheetFormulaParserToken;
  AParamsToken: TdxSpreadSheetFormulaParserToken;
begin
  AParamsToken := AStack.Pop;
  ANameToken := AStack.Pop;
  AInfo := dxSpreadSheetFunctionsRepository.GetInfoByName
    (@FFormulaText[ANameToken.StartPosition], ANameToken.GetLength);
  if AInfo <> nil then
  begin
    Result := TdxSpreadSheetFormulaFunctionToken.Create(AInfo);
    GetParameters(Result, AParamsToken.StartPosition,
      AParamsToken.FinishPosition, FParameterSeparatorCheckFunc);
    TdxSpreadSheetFormulaFunctionToken(Result).InitializeParamInfo;
  end
  else
  begin
    Result := TdxSpreadSheetFormulaUnknownFunctionToken.Create
      (ANameToken.GetString(FFormulaSourceText));
    GetParameters(Result, AParamsToken.StartPosition,
      AParamsToken.FinishPosition, FParameterSeparatorCheckFunc);
  end;
  InitializeSourceBounds(Result, ANameToken.StartPosition,
    AParamsToken.FinishPosition + 1 { Length(dxRightParenthesis) } );
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenIdent
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AColumn, ARow: TdxSpreadSheetReference;
  ADefinedName: TdxSpreadSheetFormulaToken;
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Pop;
  if AToken.Equals(FFormulaText, dxBoolToString[False]) then
    Result := TdxSpreadSheetFormulaBooleanValueToken.Create(False)
  else if AToken.Equals(FFormulaText, dxBoolToString[True]) then
    Result := TdxSpreadSheetFormulaBooleanValueToken.Create(True)
  else if CreateDefinedNameToken(AToken, ADefinedName) then
    Result := ADefinedName
  else if CheckReference(AToken, ARow, AColumn) then
    Result := Create3DReferenceToken(ARow, AColumn, Create3DReferenceAutoLink)
  else
    Result := CreateUnknownNameToken(AToken);

  InitializeSourceBounds(Result, AToken);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenNumber
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
  ATokenValue: string;
  ATokenValueAsDouble: Double;
  ATokenValueAsInteger: Integer;
begin
  AToken := AStack.Last;
  ATokenValue := AToken.GetString(FFormulaText);
  if (AToken.ID = TokenInteger) and TryStrToInt(ATokenValue,
    ATokenValueAsInteger) then
    Result := TdxSpreadSheetFormulaIntegerValueToken.Create
      (StrToInt(ATokenValue))
  else if TryStrToFloat(ATokenValue, ATokenValueAsDouble, FormatSettings.Data)
  then
    Result := TdxSpreadSheetFormulaFloatValueToken.Create(ATokenValueAsDouble)
  else
    Result := nil;

  InitializeSourceBounds(Result, AToken);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenOperator
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Last;
  Result := TdxSpreadSheetFormulaOperationToken.Create
    (TdxSpreadSheetFormulaOperation(AToken.Data));
  InitializeSourceBounds(Result, AToken);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenString
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Last;
  Result := TdxSpreadSheetFormulaStringValueToken.Create
    (dxSpreadSheetUnescapeString(AToken.GetString(FFormulaSourceText),
    Char(AToken.Data)));
  InitializeSourceBounds(Result, AToken.StartPosition - 1,
    AToken.FinishPosition + 1);
end;

function TdxSpreadSheetCustomFormulaParser.CreateTokenSubExpression
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AToken: TdxSpreadSheetFormulaParserToken;
begin
  AToken := AStack.Pop;
  Result := TdxSpreadSheetFormulaParenthesesToken.Create;
  if AToken.FinishPosition >= AToken.StartPosition then
    TdxSpreadSheetFormulaToken.AddChild(Result,
      DoFullParse(AToken.StartPosition, AToken.FinishPosition));
  InitializeSourceBounds(Result, AToken.StartPosition -
    1 { Length(dxLeftParenthesis) } , AToken.FinishPosition +
    1 { Length(dxRightParenthesis) } );
end;

function TdxSpreadSheetCustomFormulaParser.Create3DAreaReferenceToken
  (const ARow, AColumn, ARow2, AColumn2: TdxSpreadSheetReference;
  ALink1, ALink2: TdxSpreadSheet3DReferenceCustomLink)
  : TdxSpreadSheetFormulaToken;
begin
  if (ALink1 <> nil) or (ALink2 <> nil) then
    Result := TdxSpreadSheetFormula3DAreaReferenceToken.Create(ALink1, ALink2,
      ARow, AColumn, ARow2, AColumn2)
  else
    Result := TdxSpreadSheetFormulaAreaReferenceToken.Create(ARow, AColumn,
      ARow2, AColumn2);
end;

function TdxSpreadSheetCustomFormulaParser.Create3DReferenceAutoLink
  : TdxSpreadSheet3DReferenceCustomLink;
begin
  Result := nil;
end;

function TdxSpreadSheetCustomFormulaParser.Create3DReferenceLink
  (AStack: TdxSpreadSheetFormulaParserTokenList)
  : TdxSpreadSheet3DReferenceCustomLink;
var
  ALinkId: Integer;
  AToken: TdxSpreadSheetFormulaParserToken;
  AView: TObject;
  AViewName: string;
begin
  if AStack.Count = 0 then
    Exit(Create3DReferenceAutoLink);

  AToken := AStack.Pop;
  if AToken.ID = TokenReferenceSeparator then
    AToken := AStack.Pop;

  if AToken.ID = TokenReferenceError then
    Result := TdxSpreadSheet3DReferenceLink.Create
      (TdxSpreadSheetInvalidObject.Instance)
  else if GetExternalLinkInfo(AToken, ALinkId, AViewName, AStack) then
    Result := Create3DReferenceExternalLink(ALinkId, AViewName)
  else if GetViewByName(AToken.StartPosition, AToken.GetLength, AView) then
    Result := TdxSpreadSheet3DReferenceLink.Create(AView)
  else
    Result := TdxSpreadSheet3DReferenceLink.Create
      (TdxSpreadSheetInvalidObject.Instance);
end;

function TdxSpreadSheetCustomFormulaParser.Create3DReferenceExternalLink
  (ALinkId: Integer; const AName: string)
  : TdxSpreadSheet3DReferenceCustomExternalLink;
begin
  Result := nil;
end;

function TdxSpreadSheetCustomFormulaParser.Create3DReferenceToken(const ARow,
  AColumn: TdxSpreadSheetReference; ALink: TdxSpreadSheet3DReferenceCustomLink)
  : TdxSpreadSheetFormulaToken;
begin
  if ALink <> nil then
    Result := TdxSpreadSheetFormula3DReferenceToken.Create(ALink, ARow, AColumn)
  else
    Result := TdxSpreadSheetFormulaReferenceToken.Create(ARow, AColumn);
end;

function TdxSpreadSheetCustomFormulaParser.CreateDefinedNameToken
  (const AToken: TdxSpreadSheetFormulaParserToken;
  out ADefinedNameToken: TdxSpreadSheetFormulaToken;
  ALink: TdxSpreadSheet3DReferenceCustomLink = nil): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomFormulaParser.CreateUnknownNameToken
  (const AToken: TdxSpreadSheetFormulaParserToken;
  ALink: TdxSpreadSheet3DReferenceCustomLink = nil): TdxSpreadSheetFormulaToken;
begin
  Result := TdxSpreadSheetFormulaUnknownNameToken.Create
    (AToken.GetString(FFormulaSourceText), ALink);
end;

function TdxSpreadSheetCustomFormulaParser.IsArray(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  ARightBoundPosition: Integer;
begin
  Result := CheckText(APosition, AFinishPos, dxLeftArrayParenthesis);
  if Result then
  begin
    Inc(APosition);
    ARightBoundPosition := FindSubExpressionRightBound(dxRightArrayParenthesis,
      APosition, AFinishPos);
    if ARightBoundPosition < 0 then
    begin
      SetErrorIndex(APosition - 1, pecUnexpectedEndOfArray);
      ARightBoundPosition := AFinishPos;
    end;
    AToken.Initialize(TokenArray, APosition, ARightBoundPosition);
    APosition := ARightBoundPosition + 2;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsError(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
const
  TokenIDMap: array [Boolean] of Integer = (TokenError, TokenReferenceError);
var
  AData: TdxNativeInt;
  AScanPosition: Integer;
begin
  Result := False;
  if CheckText(APosition, AFinishPos, dxErrorPrefix) then
  begin
    AScanPosition := APosition;
    Result := FKnownErrors.Find(FFormulaText, AScanPosition, AFinishPos, AData);
    if Result then
    begin
      AToken.Initialize(TokenIDMap[AData = Ord(ecRefErr)], APosition,
        AScanPosition - 1);
      AToken.Data := AData;
      APosition := AScanPosition;
    end;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsExternalReference
  (var APosition: Integer; AFinishPos: Integer;
  var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  ARightBoundPosition: Integer;
begin
  Result := CheckText(APosition, AFinishPos, dxReferenceLeftParenthesis);
  if Result then
  begin
    Inc(APosition);
    ARightBoundPosition := FindSubExpressionRightBound
      (dxReferenceRightParenthesis, APosition, AFinishPos);
    if ARightBoundPosition < 0 then
    begin
      SetErrorIndex(APosition - 1, pecUnexpectedEndOfExternalReference);
      ARightBoundPosition := AFinishPos;
    end;
    AToken.Initialize(TokenExternalReference, APosition, ARightBoundPosition);
    APosition := ARightBoundPosition + 2;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsFunctionParameters
  (var APosition: Integer; AFinishPos: Integer;
  var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  AScanPosition: Integer;
begin
  AScanPosition := APosition;
  SkipInsignificantSpaces(AScanPosition, AFinishPos);
  Result := IsSubExpression(AScanPosition, AFinishPos, AToken);
  if Result then
    APosition := AScanPosition;
end;

function TdxSpreadSheetCustomFormulaParser.IsIdent(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  AScanPosition: Integer;
begin
  AScanPosition := APosition;
  while (AScanPosition <= AFinishPos) and not IsBreakChar(AScanPosition,
    AFinishPos) do
    Inc(AScanPosition);
  Result := AScanPosition > APosition;
  if Result then
  begin
    AToken.Initialize(TokenIdent, APosition, AScanPosition - 1);
    APosition := AScanPosition;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsNumber(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
const
  NumericStateMachine: array [0 .. 3, -1 .. 14] of SmallInt = (
    { 0  1  2  3  4  5  6  7  8  9   +   -   .   E  #0 }
    (-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 1, 2, -1),
    (-2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -2, 2, -1),
    (-2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, -2, -2, -1),
    (-2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, -1, -1, -2, -2, -1));

  function GetCharIndex(const C: Char): Integer;
  begin
    if dxCharInSet(C, ['0' .. '9']) then
      Result := Ord(C) - Ord('0')
    else if Ord(C) = Ord('+') then
      Result := 10
    else if Ord(C) = Ord('-') then
      Result := 11
    else if Ord(C) = Ord(FormatSettings.DecimalSeparator) then
      Result := 12
    else if Ord(C) = Ord(dxExponentChar) then
      Result := 13
    else if Pos(C, FormatSettings.BreakChars) > 0 then
      Result := 14
    else
      Result := -1;
  end;

const
  TokenMap: array [Boolean] of Integer = (TokenFloat, TokenInteger);
var
  AMachineState: Integer;
  ANextMachineState: Integer;
  AScanPosition: Integer;
begin
  Result := (APosition <= AFinishPos) and
    dxWideIsNumeric(FFormulaText[APosition]);
  if Result then
  begin
    AMachineState := 0;
    AScanPosition := APosition;
    while AScanPosition <= AFinishPos do
    begin
      ANextMachineState := NumericStateMachine[AMachineState,
        GetCharIndex(FFormulaText[AScanPosition])];
      if ANextMachineState < 0 then
      begin
        if ANextMachineState = -2 then
          Exit(False);
        Break;
      end;
      AMachineState := ANextMachineState;
      Inc(AScanPosition);
    end;
    AToken.Initialize(TokenMap[AMachineState = 0], APosition,
      AScanPosition - 1);
    APosition := AScanPosition;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsOperation(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  AData: TdxNativeInt;
  AStartPosition: Integer;
begin
  AStartPosition := APosition;
  Result := FKnownOperations.Find(FFormulaText, APosition, AFinishPos, AData);
  if Result then
  begin
    AToken.Initialize(TokenOperator, AStartPosition, APosition - 1);
    AToken.Data := AData;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsReference(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  ARow, AColumn, AColumn2: TdxSpreadSheetReference;
  AScanPosition: Integer;
begin
  Result := False;
  AScanPosition := APosition;
  if ParseReference(AScanPosition, AFinishPos, ARow, AColumn) then
  begin
    if R1C1Reference or ARow.IsAbsolute or AColumn.IsAbsolute and not ARow.IsAllItems
    then
    begin
      AToken.Initialize(TokenReference, APosition, AScanPosition - 1);
      AToken.Data := ARow.Value;
      AToken.Data2 := AColumn.Value;
      APosition := AScanPosition;
      Result := True;
    end
    else
    begin
      if not ARow.IsAllItems or AColumn.IsError or AColumn.IsAllItems then
        Exit(False);
      SkipInsignificantSpaces(AScanPosition, AFinishPos);
      if not CheckText(AScanPosition, AFinishPos, dxAreaSeparator) then
        Exit(False);
      Inc(AScanPosition);
      if not ParseReference(AScanPosition, AFinishPos, ARow, AColumn2) then
        Exit(False);
      if not ARow.IsAllItems or AColumn2.IsError or AColumn2.IsAllItems then
        Exit(False);
      AToken.Initialize(TokenFullColumnReference, APosition, AScanPosition - 1);
      AToken.Data := AColumn.Value;
      AToken.Data2 := AColumn2.Value;
      APosition := AScanPosition;
      Result := True;
    end;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsReferenceSeparator
  (var APosition: Integer; AFinishPos: Integer;
  var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
begin
  Result := CheckText(APosition, AFinishPos, dxRefSeparator);
  if Result then
  begin
    AToken.Initialize(TokenReferenceSeparator, APosition, APosition);
    Inc(APosition);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsString(var APosition: Integer;
  AFinishPos: Integer; var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  AMark: Char;
  ARightBoundPosition: Integer;
begin
  if CheckText(APosition, AFinishPos, dxStringMarkChar) then
    AMark := dxStringMarkChar
  else if CheckText(APosition, AFinishPos, dxStringMarkChar2) then
    AMark := dxStringMarkChar2
  else
    AMark := #0;

  Result := AMark <> #0;
  if Result then
  begin
    Inc(APosition);
    ARightBoundPosition := FindStringRightBound(FFormulaText, AMark, APosition,
      AFinishPos);
    if ARightBoundPosition < 0 then
    begin
      SetErrorIndex(APosition - 1, pecUnexpectedEndOfString);
      ARightBoundPosition := AFinishPos;
    end;
    AToken.Initialize(TokenString, APosition, ARightBoundPosition);
    AToken.Data := Ord(AMark);
    APosition := ARightBoundPosition + 2;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsSubExpression
  (var APosition: Integer; AFinishPos: Integer;
  var AToken: TdxSpreadSheetFormulaParserToken): Boolean;
var
  ARightBoundPosition: Integer;
begin
  Result := CheckText(APosition, AFinishPos, dxLeftParenthesis);
  if Result then
  begin
    Inc(APosition);
    ARightBoundPosition := FindSubExpressionRightBound(dxRightParenthesis,
      APosition, AFinishPos);
    if ARightBoundPosition < 0 then
    begin
      SetErrorIndex(APosition - 1, pecUnexpectedEndOfExpression);
      ARightBoundPosition := AFinishPos;
    end;
    AToken.Initialize(TokenSubExpression, APosition, ARightBoundPosition);
    APosition := ARightBoundPosition + 2;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.TokenAreaReferenceAsReference
  (AToken: TdxSpreadSheetFormulaToken;
  out ARow, AColumn: TdxSpreadSheetReference;
  out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomFormulaParser.TokenErrorAsReference
  (AToken: TdxSpreadSheetFormulaToken;
  out ARow, AColumn: TdxSpreadSheetReference;
  out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  Result := TdxSpreadSheetFormulaErrorValueToken(AToken).ErrorCode = ecRefErr;
  if Result then
  begin
    AColumn := TdxSpreadSheetReference.Invalid;
    ARow := TdxSpreadSheetReference.Invalid;
    ALink := nil;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.TokenIntegerAsReference
  (AToken: TdxSpreadSheetFormulaToken;
  out ARow, AColumn: TdxSpreadSheetReference;
  out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  ALink := nil;
  AColumn := TdxSpreadSheetReference.AllItems;
  ARow := TdxSpreadSheetReference.Create
    (TdxSpreadSheetFormulaIntegerValueToken(AToken).Value - 1, False);
  ValidateReferences(ARow, AColumn);
  Result := True;
end;

function TdxSpreadSheetCustomFormulaParser.TokenReferenceAsReference
  (AToken: TdxSpreadSheetFormulaToken;
  out ARow, AColumn: TdxSpreadSheetReference;
  out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  ARow := TdxSpreadSheetFormulaReferenceToken(AToken).Row;
  AColumn := TdxSpreadSheetFormulaReferenceToken(AToken).Column;
  ALink := nil;
  Result := True;
end;

function TdxSpreadSheetCustomFormulaParser.Token3DReferenceAsReference
  (AToken: TdxSpreadSheetFormulaToken;
  out ARow, AColumn: TdxSpreadSheetReference;
  out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  ARow := TdxSpreadSheetFormula3DReferenceToken(AToken).Row;
  AColumn := TdxSpreadSheetFormula3DReferenceToken(AToken).Column;
  ALink := TdxSpreadSheetFormula3DReferenceToken(AToken).Link;
  Result := True;
end;

function TdxSpreadSheetCustomFormulaParser.CheckReference
  (const AToken: TdxSpreadSheetFormulaParserToken;
  var ARow, AColumn: TdxSpreadSheetReference): Boolean;
var
  AStartPos: Integer;
  AFinishPos: Integer;
begin
  Result := AToken.ID = TokenReferenceError;
  if Result then
  begin
    ARow := TdxSpreadSheetReference.Invalid;
    AColumn := TdxSpreadSheetReference.Invalid;
  end
  else
  begin
    AStartPos := AToken.StartPosition;
    AFinishPos := AToken.FinishPosition + 1;
    Result := ParseReference(AStartPos, AFinishPos, ARow, AColumn) and
      (AStartPos >= AFinishPos);
    Result := Result and (not ARow.IsAllItems or R1C1Reference);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.ParseReference(var AStartPos,
  AFinishPos: Integer; var ARow, AColumn: TdxSpreadSheetReference): Boolean;
begin
  Result := TdxSpreadSheetReferenceParser.Parse(FFormulaText, R1C1Reference,
    AStartPos, AFinishPos, ARow, AColumn);
  if Result then
    ValidateReferences(ARow, AColumn);
end;

procedure TdxSpreadSheetCustomFormulaParser.InitializeParser;
begin
  PopulateKnownOperations;
  PopulateKnownErrors;
  PopulateStateMachine;
  PopulateTokenToReferenceAdapters;

  FCanSpaceBeSignificant := FKnownOperations.Contains(' ');
  FIsRangeOperatorSupported := FKnownOperations.Contains(dxAreaSeparator);

  if FIsRangeOperatorSupported and
    (dxAreaSeparator = FormatSettings.ArraySeparator) then
    FParameterSeparatorCheckFunc := IsListSeparator
  else
    FParameterSeparatorCheckFunc := IsListOrArraySeparator;
end;

procedure TdxSpreadSheetCustomFormulaParser.ValidateReference(AIndex: Integer;
  var AReference: TdxSpreadSheetReference);
begin
  if not(R1C1Reference or AReference.IsAllItems or AReference.IsAbsolute) then
    AReference.Offset := AReference.Offset - AIndex;
end;

procedure TdxSpreadSheetCustomFormulaParser.ValidateReferences(var ARow,
  AColumn: TdxSpreadSheetReference);
begin
  ValidateReference(AnchorRow, ARow);
  ValidateReference(AnchorColumn, AColumn);
end;

function TdxSpreadSheetCustomFormulaParser.GetExternalLinkInfo
  (const AToken: TdxSpreadSheetFormulaParserToken; out ALinkId: Integer;
  out AName: string;
  AStack: TdxSpreadSheetFormulaParserTokenList = nil): Boolean;

  function ExternalReferenceTokenToLinkID(const AToken
    : TdxSpreadSheetFormulaParserToken): Integer;
  begin
    Result := StrToIntDef(AToken.GetString(FFormulaText), -1);
    if Result <= 0 then
      SetErrorIndex(AToken.StartPosition - 1, pecInvalidReference);
  end;

var
  AReferenceToken: TdxSpreadSheetFormulaParserToken;
  AScanPosition: Integer;
begin
  Result := False;
  case AToken.ID of
    TokenExternalReference:
      begin
        ALinkId := ExternalReferenceTokenToLinkID(AToken);
        Result := CheckError;
      end;

    TokenString:
      begin
        AScanPosition := AToken.StartPosition;
        if IsExternalReference(AScanPosition, AToken.FinishPosition,
          AReferenceToken) then
        begin
          ALinkId := ExternalReferenceTokenToLinkID(AReferenceToken);
          AName := Copy(FFormulaSourceText, AScanPosition,
            AToken.FinishPosition - AScanPosition + 1);
          Result := CheckError;
        end;
      end;

  else
    if (AStack <> nil) and (AStack.Count > 0) and
      (AStack.Last.ID = TokenExternalReference) then
    begin
      ALinkId := ExternalReferenceTokenToLinkID(AStack.Pop);
      AName := AToken.GetString(FFormulaSourceText);
      Result := CheckError;
    end;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.GetNextToken(var APosition: Integer;
  AFinishPos: Integer): TdxSpreadSheetFormulaToken;

  function GetReferenceInfo(AToken: TdxSpreadSheetFormulaToken;
    out ARow, AColumn: TdxSpreadSheetReference;
    out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
  var
    AAdapter: TTokenToReferenceAdapter;
  begin
    Result := (AToken <> nil) and CheckError and
      FTokenToReferenceAdapters.TryGetValue(AToken.ClassType, AAdapter) and
      AAdapter(AToken, ARow, AColumn, ALink);
  end;

  procedure PrepareLinks(var ALink1,
    ALink2: TdxSpreadSheet3DReferenceCustomLink);
  begin
    if (ALink1 <> nil) and (ALink2 is TdxSpreadSheet3DReferenceAutoLink) then
      ALink2 := nil;
    if (ALink2 <> nil) and (ALink1 is TdxSpreadSheet3DReferenceAutoLink) then
      ALink1 := nil;

    if ALink1 <> nil then
      ALink1 := ALink1.Clone;
    if ALink2 <> nil then
      ALink2 := ALink2.Clone;
  end;

var
  AColumn1: TdxSpreadSheetReference;
  AColumn2: TdxSpreadSheetReference;
  ALink1: TdxSpreadSheet3DReferenceCustomLink;
  ALink2: TdxSpreadSheet3DReferenceCustomLink;
  ANextToken: TdxSpreadSheetFormulaToken;
  ARow1: TdxSpreadSheetReference;
  ARow2: TdxSpreadSheetReference;
  ATempPosition: Integer;
  ASourceFinish: Integer;
  ASourceStart: Integer;
begin
  Result := inherited GetNextToken(APosition, AFinishPos);

  if FIsRangeOperatorSupported and GetReferenceInfo(Result, ARow1, AColumn1,
    ALink1) then
  begin
    ATempPosition := APosition;
    SkipInsignificantSpaces(ATempPosition, AFinishPos);
    if CheckText(ATempPosition, AFinishPos, dxAreaSeparator) then
    begin
      Inc(ATempPosition);
      ANextToken := inherited GetNextToken(ATempPosition, AFinishPos);
      if GetReferenceInfo(ANextToken, ARow2, AColumn2, ALink2) then
      begin
        ASourceFinish := ANextToken.SourceFinish;
        ASourceStart := Result.SourceStart;

        PrepareLinks(ALink1, ALink2);
        FreeAndNil(ANextToken);
        FreeAndNil(Result);

        Result := Create3DAreaReferenceToken(ARow1, AColumn1, ARow2, AColumn2,
          ALink1, ALink2);
        Result.SourceFinish := ASourceFinish;
        Result.SourceStart := ASourceStart;
        Result.Owner := Formula;

        APosition := ATempPosition;
      end
      else
        FreeAndNil(ANextToken);
    end;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.GetViewByName(APosition,
  ALength: Integer; out AView: TObject): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomFormulaParser.PopulateKnownErrors;
begin
  FKnownErrors.Clear;
  FKnownErrors.Add(serDivZeroError, Ord(ecDivByZero));
  FKnownErrors.Add(serNAError, Ord(ecNA));
  FKnownErrors.Add(serNameError, Ord(ecName));
  FKnownErrors.Add(serNullError, Ord(ecNull));
  FKnownErrors.Add(serNumError, Ord(ecNUM));
  FKnownErrors.Add(serRefError, Ord(ecRefErr));
  FKnownErrors.Add(serValueError, Ord(ecValue));
end;

procedure TdxSpreadSheetCustomFormulaParser.PopulateKnownOperations;
var
  AOperation: TdxSpreadSheetFormulaOperation;
begin
  FKnownOperations.Clear;
  for AOperation := Low(TdxSpreadSheetFormulaOperation)
    to High(TdxSpreadSheetFormulaOperation) do
    FKnownOperations.Add(FormatSettings.Operations[AOperation],
      Ord(AOperation));
end;

procedure TdxSpreadSheetCustomFormulaParser.PopulateStateMachine;

  procedure PopulateFinalStates;
  begin
    StateMachine.CreateState(StateExternalFunction)
      .SetTokenCreator(CreateTokenExternalFunction);
    StateMachine.CreateState(State3DReference)
      .SetTokenCreator(CreateToken3DReference);
    StateMachine.CreateState(StateFullColumnReference)
      .SetTokenCreator(CreateTokenFullColumnReference);
    StateMachine.CreateState(StateSubExpression)
      .SetTokenCreator(CreateTokenSubExpression);
    StateMachine.CreateState(StateNumber).SetTokenCreator(CreateTokenNumber);
    StateMachine.CreateState(StateOperator)
      .SetTokenCreator(CreateTokenOperator);
    StateMachine.CreateState(StateFunction)
      .SetTokenCreator(CreateTokenFunction);
    StateMachine.CreateState(StateArray).SetTokenCreator(CreateTokenArray);
    StateMachine.CreateState(StateError).SetTokenCreator(CreateTokenError);
  end;

  procedure PopulateInitialStates;
  var
    AState: TdxSpreadSheetFormulaParserState;
  begin
    AState := StateMachine.CreateState(StateStart);
    AState.AddTokenController(IsArray);
    AState.AddTokenController(IsNumber);
    AState.AddTokenController(IsString);
    AState.AddTokenController(IsSubExpression);
    AState.AddTokenController(IsError);
    AState.AddTokenController(IsExternalReference);
    AState.AddTokenController(IsOperation);
    AState.AddTokenController(IsReference);
    AState.AddTokenController(IsIdent); // must be last
    AState.SetNextState(TokenArray, StateArray);
    AState.SetNextState(TokenError, StateError);
    AState.SetNextState(TokenFloat, StateNumber);
    AState.SetNextState(TokenIdent, StateIdent);
    AState.SetNextState(TokenInteger, StateNumber);
    AState.SetNextState(TokenOperator, StateOperator);
    AState.SetNextState(TokenReference, State3DReference);
    AState.SetNextState(TokenReferenceError, StateErrorOrReference);
    AState.SetNextState(TokenString, StateStringOr3DReference);
    AState.SetNextState(TokenSubExpression, StateSubExpression);
    AState.SetNextState(TokenFullColumnReference, StateFullColumnReference);
    AState.SetNextState(TokenExternalReference, StateExternalReferencePre1);
  end;

var
  AState: TdxSpreadSheetFormulaParserState;
begin
  PopulateInitialStates;

  AState := StateMachine.CreateState(StateErrorOrReference);
  AState.AddTokenController(IsNumber);
  AState.AddTokenController(IsError);
  AState.AddTokenController(IsReference);
  AState.AddTokenController(IsIdent);
  AState.SetTokenCreator(CreateTokenError);
  AState.SetNextState(TokenFullColumnReference, StateFullColumnReference);
  AState.SetNextState(TokenIdent, State3DReference);
  AState.SetNextState(TokenInteger, State3DReference);
  AState.SetNextState(TokenReferenceError, State3DReference);
  AState.SetNextState(TokenReference, State3DReference);

  AState := StateMachine.CreateState(StateStringOr3DReference);
  AState.AddTokenController(IsReferenceSeparator);
  AState.SetTokenCreator(CreateTokenString);
  AState.SetNextState(TokenReferenceSeparator, State3DReferencePre2);

  AState := StateMachine.CreateState(StateExternalReferencePre1);
  AState.AddTokenController(IsReferenceSeparator);
  AState.AddTokenController(IsIdent);
  AState.SetNextState(TokenReferenceSeparator, StateExternalReferencePre2);
  AState.SetNextState(TokenIdent, State3DReferencePre1);

  AState := StateMachine.CreateState(StateExternalReferencePre2);
  AState.AddTokenController(IsIdent);
  AState.SetNextState(TokenIdent, StateExternalNameOrFunction);

  AState := StateMachine.CreateState(StateExternalNameOrFunction);
  AState.AddTokenController(IsFunctionParameters);
  AState.SetTokenCreator(CreateTokenExternalName);
  AState.SetNextState(TokenSubExpression, StateExternalFunction);

  AState := StateMachine.CreateState(State3DReferencePre1);
  AState.AddTokenController(IsReferenceSeparator);
  AState.SetNextState(TokenReferenceSeparator, State3DReferencePre2);

  AState := StateMachine.CreateState(State3DReferencePre2);
  AState.AddTokenController(IsError);
  AState.AddTokenController(IsReference);
  AState.AddTokenController(IsIdent);
  AState.SetNextState(TokenIdent, State3DReference);
  AState.SetNextState(TokenReference, State3DReference);
  AState.SetNextState(TokenReferenceError, State3DReference);
  AState.SetNextState(TokenFullColumnReference, StateFullColumnReference);

  AState := StateMachine.CreateState(StateIdent);
  AState.AddTokenController(IsReferenceSeparator);
  AState.AddTokenController(IsFunctionParameters);
  AState.SetNextState(TokenReferenceSeparator, State3DReferencePre2);
  AState.SetNextState(TokenSubExpression, StateFunction);
  AState.SetTokenCreator(CreateTokenIdent);

  PopulateFinalStates;
end;

procedure TdxSpreadSheetCustomFormulaParser.PopulateTokenToReferenceAdapters;
begin
  FTokenToReferenceAdapters.Add(TdxSpreadSheetFormula3DReferenceToken,
    Token3DReferenceAsReference);
  FTokenToReferenceAdapters.Add(TdxSpreadSheetFormulaAreaReferenceToken,
    TokenAreaReferenceAsReference);
  FTokenToReferenceAdapters.Add(TdxSpreadSheetFormulaIntegerValueToken,
    TokenIntegerAsReference);
  FTokenToReferenceAdapters.Add(TdxSpreadSheetFormulaReferenceToken,
    TokenReferenceAsReference);
  FTokenToReferenceAdapters.Add(TdxSpreadSheetFormulaErrorValueToken,
    TokenErrorAsReference);
end;

end.
