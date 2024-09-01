{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressRichEditControl }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL }
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

unit dxRichEdit.DocumentModel.PieceTableIterators; // for internal use

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.Platform.Font;

type
  { TdxPieceTableIterator }

  TdxPieceTableIterator = class abstract
  strict private
    FPieceTable: TdxSimplePieceTable;
  protected
    procedure MoveBackCore(var APos: TdxDocumentModelPosition);
      virtual; abstract;
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition);
      virtual; abstract;
    procedure UpdateModelPositionByLogPosition
      (var APos: TdxDocumentModelPosition);
    property PieceTable: TdxSimplePieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); virtual;

    function MoveBack(const APos: TdxDocumentModelPosition)
      : TdxDocumentModelPosition;
    function MoveForward(const APos: TdxDocumentModelPosition)
      : TdxDocumentModelPosition;

    function IsEndOfDocument(const APos: TdxDocumentModelPosition)
      : Boolean; inline;
    function IsNewElement(const ACurrentPosition
      : TdxDocumentModelPosition): Boolean;
    function IsStartOfDocument(const APos: TdxDocumentModelPosition)
      : Boolean; inline;
  end;

  { TdxCharactersDocumentModelIterator }

  TdxCharactersDocumentModelIterator = class(TdxPieceTableIterator)
  protected
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override; final;
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition);
      override; final;

    procedure SkipForward(var APos: TdxDocumentModelPosition); virtual;
    procedure SkipBackward(var APos: TdxDocumentModelPosition); virtual;
  end;

  { TdxVisibleCharactersStopAtFieldsDocumentModelIterator }

  TdxVisibleCharactersStopAtFieldsDocumentModelIterator = class
    (TdxCharactersDocumentModelIterator)
  strict private
    FFilter: IdxVisibleTextFilter;
  protected
    function IsFieldDelimiter(const APos: TdxDocumentModelPosition)
      : Boolean; virtual;
    procedure SkipForward(var APos: TdxDocumentModelPosition); override;
    procedure SkipBackward(var APos: TdxDocumentModelPosition); override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
  end;

  { TdxWordsDocumentModelIteratorBase }

  TdxWordsDocumentModelIteratorBase = class abstract(TdxPieceTableIterator)
  strict private
    FCachedRunIndex: TdxRunIndex;
    FCachedRunText: string;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;

    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    procedure SkipForward(AIterator: TdxPieceTableIterator;
      var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); virtual;
    procedure SkipBackward(AIterator: TdxPieceTableIterator;
      var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); virtual;
  end;

  { TdxWordsDocumentModelIterator }

  TdxWordsDocumentModelIterator = class(TdxWordsDocumentModelIteratorBase)
  strict private
    class function IsNonWordSymbol(AChar: Char): Boolean; inline;
    class function IsSpecialSymbol(AChar: Char): Boolean; inline;
  protected
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;
  public
    class function IsWordsDelimiter(const AChar: Char): Boolean; static;
    class function IsNotNonWordsSymbols(const AChar: Char): Boolean; static;
    class function IsSpace(const AChar: Char): Boolean; static; inline;

    function IsInsideWord(const ACurrentPosition
      : TdxDocumentModelPosition): Boolean;
    function IsAtWord(const ACurrentPosition: TdxDocumentModelPosition)
      : Boolean;
  end;

  { TdxVisibleWordsIterator }

  TdxVisibleWordsIterator = class(TdxWordsDocumentModelIterator)
  strict private
    FFilter: IdxVisibleTextFilter;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;

    procedure SkipForward(AIterator: TdxPieceTableIterator;
      var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); override;
    procedure SkipBackward(AIterator: TdxPieceTableIterator;
      var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); override;
  end;

  { TdxParagraphsDocumentModelIterator }

  TdxParagraphsDocumentModelIterator = class(TdxPieceTableIterator)
  protected
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
  end;

  { TdxSpellCheckerWordIterator }

  TdxSpellCheckerWordIterator = class(TdxPieceTableIterator)
  strict private
    FCachedRunIndex: TdxRunIndex;
    FCachedRunText: string;
    procedure SkipBackwardInvisibleRuns(var APos: TdxDocumentModelPosition);
    procedure SkipForwardInvisibleRuns(var APos: TdxDocumentModelPosition);
    function GetVisibleTextFilter: IdxVisibleTextFilter;
  protected
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveToWordStartCore(var APos: TdxDocumentModelPosition); overload;

    property VisibleTextFilter: IdxVisibleTextFilter read GetVisibleTextFilter;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
    function MoveToWordEnd(const APos: TdxDocumentModelPosition)
      : TdxDocumentModelPosition;
    function MoveToWordStart(const APos: TdxDocumentModelPosition;
      ASkipNotLetterOrDigitChar: Boolean = True): TdxDocumentModelPosition;
      overload; virtual;
    procedure MoveToNextWordEnd(var APos: TdxDocumentModelPosition);
    procedure MoveToPrevWordStart(var APos: TdxDocumentModelPosition);
    // for internal use
    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    function IsNotWordsSeparators(const ACh: Char): Boolean; inline;
    function IsWordsSeparators(const ACh: Char): Boolean;
    function IsNotLetterOrDigit(const ACh: Char): Boolean; inline;
    function IsLetterOrDigit(const ACh: Char): Boolean; inline;
    procedure MoveToPrevChar(var APos: TdxDocumentModelPosition);
    procedure MoveToPrevRun(var APos: TdxDocumentModelPosition;
      ARunEndPos: TdxDocumentLogPosition);
    procedure MoveToNextChar(var APos: TdxDocumentModelPosition);
    function MoveToNextRun(var APos: TdxDocumentModelPosition;
      ARunStartPos: TdxDocumentLogPosition): Boolean;
    procedure MoveToWordEndCore(var APos: TdxDocumentModelPosition;
      AWordWithDots: Boolean = False); overload;
    procedure SkipBackward(var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>);
    procedure SkipForward(var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>);

    property PieceTable;
    property CachedRunIndex: TdxRunIndex read FCachedRunIndex;
  end;

implementation

uses
  Math, RTLConsts, Character, dxCore, dxCoreClasses,
  dxRichEdit.DocumentModel.FieldRange,
  dxCharacters;

{ TdxPieceTableIterator }

constructor TdxPieceTableIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxPieceTableIterator.IsEndOfDocument
  (const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := APos.LogPosition >= PieceTable.DocumentEndLogPosition;
end;

function TdxPieceTableIterator.IsNewElement(const ACurrentPosition
  : TdxDocumentModelPosition): Boolean;
var
  APos: TdxDocumentModelPosition;
begin
  APos := MoveBack(ACurrentPosition);
  APos := MoveForward(APos);
  Result := APos = ACurrentPosition;
end;

function TdxPieceTableIterator.IsStartOfDocument
  (const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := APos.LogPosition = PieceTable.DocumentStartLogPosition;
end;

function TdxPieceTableIterator.MoveBack(const APos: TdxDocumentModelPosition)
  : TdxDocumentModelPosition;
begin
  Result := APos;
  if IsStartOfDocument(Result) then
    Exit;
  MoveBackCore(Result);
end;

function TdxPieceTableIterator.MoveForward(const APos: TdxDocumentModelPosition)
  : TdxDocumentModelPosition;
begin
  Result := APos;
  if IsEndOfDocument(Result) then
    Exit;
  MoveForwardCore(Result);
end;

procedure TdxPieceTableIterator.UpdateModelPositionByLogPosition
  (var APos: TdxDocumentModelPosition);
var
  AParagraph: TdxParagraphBase;
  ALogPosition: TdxDocumentLogPosition;
  ARunStart: TdxDocumentLogPosition;
  AParagraphIndex, ARunIndex: Integer;
begin
  AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  ALogPosition := APos.LogPosition;
  if (ALogPosition >= APos.RunStartLogPosition) and
    (ALogPosition < APos.RunEndLogPosition) then
    Exit;
  AParagraphIndex := APos.ParagraphIndex;
  while (ALogPosition > AParagraph.EndLogPosition) or
    (ALogPosition < AParagraph.LogPosition) do
  begin
    if ALogPosition < AParagraph.LogPosition then
      Dec(AParagraphIndex)
    else
      Inc(AParagraphIndex);
    AParagraph := PieceTable.Paragraphs[AParagraphIndex];
  end;
  APos.ParagraphIndex := AParagraphIndex;
  ARunIndex := AParagraph.FirstRunIndex;
  ARunStart := AParagraph.LogPosition;
  while (ALogPosition >= ARunStart + PieceTable.Runs[ARunIndex].Length) do
  begin
    Inc(ARunStart, PieceTable.Runs[ARunIndex].Length);
    Inc(ARunIndex);
  end;
  APos.RunIndex := ARunIndex;
  APos.RunStartLogPosition := ARunStart;
end;

{ TdxWordsDocumentModelIteratorBase }

constructor TdxWordsDocumentModelIteratorBase.Create
  (APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FCachedRunIndex := -1;
end;

function TdxWordsDocumentModelIteratorBase.GetCharacter
  (const APos: TdxDocumentModelPosition): Char;
var
  AIndex: Integer;
begin
  if APos.RunIndex <> FCachedRunIndex then
  begin
    FCachedRunIndex := APos.RunIndex;
    FCachedRunText := PieceTable.GetRunNonEmptyText(FCachedRunIndex);
  end;
  AIndex := Min(APos.LogPosition, APos.PieceTable.DocumentEndLogPosition) -
    APos.RunStartLogPosition;
  if AIndex = Length(FCachedRunText) then
  begin
    if AIndex > 0 then
      Dec(AIndex);
  end;
  Result := FCachedRunText[AIndex + 1]
end;

procedure TdxWordsDocumentModelIteratorBase.SkipBackward
  (AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
  const APredicate: TdxPredicate<Char>);
begin
  while not IsStartOfDocument(APos) and APredicate(GetCharacter(APos)) do
    AIterator.MoveBackCore(APos);
end;

procedure TdxWordsDocumentModelIteratorBase.SkipForward
  (AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
  const APredicate: TdxPredicate<Char>);
begin
  while not IsEndOfDocument(APos) and APredicate(GetCharacter(APos)) do
    AIterator.MoveForwardCore(APos);
end;

{ TdxWordsDocumentModelIterator }

class function TdxWordsDocumentModelIterator.IsSpace(const AChar: Char)
  : Boolean;
begin
  case AChar of
    TdxCharacters.Space, TdxCharacters.NonBreakingSpace, TdxCharacters.EnSpace,
      TdxCharacters.EmSpace, TdxCharacters.QmSpace:
      Result := True
  else
    Result := False
  end;
end;

class function TdxWordsDocumentModelIterator.IsSpecialSymbol
  (AChar: Char): Boolean;
begin
  Result := CharInSet(AChar, [TdxCharacters.PageBreak, TdxCharacters.LineBreak,
    TdxCharacters.ColumnBreak, TdxCharacters.SectionMark,
    TdxCharacters.ParagraphMark, TdxCharacters.TabMark]);
end;

class function TdxWordsDocumentModelIterator.IsNonWordSymbol
  (AChar: Char): Boolean;
begin
  Result := IsSpace(AChar) or IsSpecialSymbol(AChar) or IsWordsDelimiter(AChar);
end;

function TdxWordsDocumentModelIterator.IsInsideWord(const ACurrentPosition
  : TdxDocumentModelPosition): Boolean;
begin
  Assert(ACurrentPosition.IsValid);
  Result := not IsNonWordSymbol(GetCharacter(ACurrentPosition)) and
    not IsNewElement(ACurrentPosition);
end;

function TdxWordsDocumentModelIterator.IsAtWord(const ACurrentPosition
  : TdxDocumentModelPosition): Boolean;
begin
  Assert(ACurrentPosition.IsValid);
  Result := not IsNonWordSymbol(GetCharacter(ACurrentPosition));
end;

class function TdxWordsDocumentModelIterator.IsNotNonWordsSymbols
  (const AChar: Char): Boolean;
begin
  Result := not IsNonWordSymbol(AChar);
end;

procedure TdxWordsDocumentModelIterator.MoveBackCore
  (var APos: TdxDocumentModelPosition);
var
  AIterator: TdxCharactersDocumentModelIterator;
  AChar: Char;
begin
  AIterator := TdxCharactersDocumentModelIterator.Create(PieceTable);
  try
    AIterator.MoveBackCore(APos);
    if IsSpecialSymbol(GetCharacter(APos)) then
      Exit;
    SkipBackward(AIterator, APos, IsSpace);
    AChar := GetCharacter(APos);
    if IsSpecialSymbol(AChar) then
    begin
      if not IsStartOfDocument(APos) then
        AIterator.MoveForwardCore(APos);
      Exit;
    end;
    if IsWordsDelimiter(AChar) then
    begin
      SkipBackward(AIterator, APos, IsWordsDelimiter);
      if not IsStartOfDocument(APos) or not IsWordsDelimiter(GetCharacter(APos))
      then
        AIterator.MoveForwardCore(APos);
    end
    else
    begin
      SkipBackward(AIterator, APos, IsNotNonWordsSymbols);
      if not IsStartOfDocument(APos) or IsNonWordSymbol(GetCharacter(APos)) then
        AIterator.MoveForwardCore(APos);
    end;
  finally
    AIterator.Free;
  end;
end;

procedure TdxWordsDocumentModelIterator.MoveForwardCore
  (var APos: TdxDocumentModelPosition);
var
  AIterator: TdxCharactersDocumentModelIterator;
  AChar: Char;
begin
  AIterator := TdxCharactersDocumentModelIterator.Create(PieceTable);
  try
    AChar := GetCharacter(APos);
    if IsSpecialSymbol(AChar) then
    begin
      AIterator.MoveForwardCore(APos);
      Exit;
    end;
    if IsWordsDelimiter(AChar) then
      SkipForward(AIterator, APos, IsWordsDelimiter)
    else
      SkipForward(AIterator, APos, IsNotNonWordsSymbols);
    SkipForward(AIterator, APos, IsSpace);
  finally
    AIterator.Free;
  end;
end;

class function TdxWordsDocumentModelIterator.IsWordsDelimiter
  (const AChar: Char): Boolean;
begin
  case AChar of
    TdxCharacters.Dot, ',', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
      TdxCharacters.Dash, TdxCharacters.EmDash, TdxCharacters.EnDash,
      TdxCharacters.Hyphen, TdxCharacters.Underscore, '=', '+', '[', ']', '{',
      '}', '\', '|', ';', ':', '''', '"', '<', '>', '/', '?', '`', '~',
      TdxCharacters.TrademarkSymbol, TdxCharacters.CopyrightSymbol,
      TdxCharacters.RegisteredTrademarkSymbol, TdxCharacters.Ellipsis,
      TdxCharacters.LeftDoubleQuote, TdxCharacters.LeftSingleQuote,
      TdxCharacters.RightDoubleQuote:
      Result := True
  else
    Result := False;
  end;
end;

{ TdxVisibleWordsIterator }

constructor TdxVisibleWordsIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FFilter := APieceTable.VisibleTextFilter;
end;

procedure TdxVisibleWordsIterator.SkipForward(AIterator: TdxPieceTableIterator;
  var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsEndOfDocument(APos) and
    (APredicate(GetCharacter(APos)) or not FFilter.IsRunVisible
    (APos.RunIndex)) do
    AIterator.MoveForwardCore(APos);
end;

procedure TdxVisibleWordsIterator.SkipBackward(AIterator: TdxPieceTableIterator;
  var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsStartOfDocument(APos) and
    (APredicate(GetCharacter(APos)) or not FFilter.IsRunVisible
    (APos.RunIndex)) do
    AIterator.MoveBackCore(APos);
end;

{ TdxCharactersDocumentModelIterator }

procedure TdxCharactersDocumentModelIterator.MoveBackCore
  (var APos: TdxDocumentModelPosition);
begin
  SkipBackward(APos);
  UpdateModelPositionByLogPosition(APos);
end;

procedure TdxCharactersDocumentModelIterator.MoveForwardCore
  (var APos: TdxDocumentModelPosition);
begin
  SkipForward(APos);
  UpdateModelPositionByLogPosition(APos);
end;

procedure TdxCharactersDocumentModelIterator.SkipBackward
  (var APos: TdxDocumentModelPosition);
begin
  APos.LogPosition := APos.LogPosition - 1;
end;

procedure TdxCharactersDocumentModelIterator.SkipForward
  (var APos: TdxDocumentModelPosition);
begin
  APos.LogPosition := APos.LogPosition + 1;
end;

{ TdxVisibleCharactersStopAtFieldsDocumentModelIterator }

constructor TdxVisibleCharactersStopAtFieldsDocumentModelIterator.Create
  (APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FFilter := APieceTable.VisibleTextFilter;
end;

function TdxVisibleCharactersStopAtFieldsDocumentModelIterator.IsFieldDelimiter
  (const APos: TdxDocumentModelPosition): Boolean;
var
  ARun: TdxRunBase;
begin
  ARun := APos.PieceTable.Runs[APos.RunIndex];
  Result := (ARun is TdxFieldCodeRunBase) or (ARun is TdxFieldResultEndRun);
end;

procedure TdxVisibleCharactersStopAtFieldsDocumentModelIterator.SkipForward
  (var APos: TdxDocumentModelPosition);
begin
  if IsFieldDelimiter(APos) then
    Exit;

  repeat
    inherited SkipForward(APos);
    if IsFieldDelimiter(APos) then
    begin
      inherited SkipBackward(APos);
      Exit;
    end;
  until IsEndOfDocument(APos) or FFilter.IsRunVisible(APos.RunIndex);
end;

procedure TdxVisibleCharactersStopAtFieldsDocumentModelIterator.SkipBackward
  (var APos: TdxDocumentModelPosition);
begin
  if IsFieldDelimiter(APos) then
    Exit;

  repeat
    inherited SkipBackward(APos);
    if IsFieldDelimiter(APos) then
    begin
      inherited SkipForward(APos);
      Exit;
    end;
  until IsStartOfDocument(APos) or FFilter.IsRunVisible(APos.RunIndex);
end;

{ TdxParagraphsDocumentModelIterator }

procedure TdxParagraphsDocumentModelIterator.MoveBackCore
  (var APos: TdxDocumentModelPosition);
var
  AParagraph: TdxParagraphBase;
begin
  AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  if (APos.LogPosition = AParagraph.LogPosition) and (APos.ParagraphIndex > 0)
  then
    APos.ParagraphIndex := APos.ParagraphIndex - 1;
  AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  APos.LogPosition := AParagraph.LogPosition;
  APos.RunStartLogPosition := APos.LogPosition;
  APos.RunIndex := AParagraph.FirstRunIndex;
end;

procedure TdxParagraphsDocumentModelIterator.MoveForwardCore
  (var APos: TdxDocumentModelPosition);
var
  ALastParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraphBase;
  ALastParagraph: TdxParagraphBase;
begin
  ALastParagraphIndex := PieceTable.Paragraphs.Last.Index;
  if APos.ParagraphIndex > ALastParagraphIndex then
    Exit;
  APos.ParagraphIndex := APos.ParagraphIndex + 1;
  if APos.ParagraphIndex <= ALastParagraphIndex then
  begin
    AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
    APos.LogPosition := AParagraph.LogPosition;
    APos.RunIndex := AParagraph.FirstRunIndex;
  end
  else
  begin
    ALastParagraph := PieceTable.Paragraphs.Last;
    APos.LogPosition := ALastParagraph.EndLogPosition + 1;
    APos.RunIndex := ALastParagraph.LastRunIndex + 1;
  end;
  APos.RunStartLogPosition := APos.LogPosition;
end;

{ TdxSpellCheckerWordIterator }

constructor TdxSpellCheckerWordIterator.Create(APieceTable
  : TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FCachedRunIndex := -1;
end;

function TdxSpellCheckerWordIterator.IsWordsSeparators(const ACh: Char)
  : Boolean;
begin
  case ACh of
    TdxCharacters.Space, TdxCharacters.TabMark, #10,
      TdxCharacters.ParagraphMark, '(', ')', '[', ']', '{', '}', '<',
      '>', '/', '\',

      TdxCharacters.ObjectMark, TdxCharacters.NonBreakingSpace,
      TdxCharacters.EnSpace, TdxCharacters.EmSpace, TdxCharacters.QmSpace,
      TdxCharacters.LineBreak, TdxCharacters.ColumnBreak,
      TdxCharacters.SectionMark, TdxCharacters.PageBreak:
      Result := True
  else
    Result := False
  end;
end;

function TdxSpellCheckerWordIterator.IsNotWordsSeparators
  (const ACh: Char): Boolean;
begin
  Result := not IsWordsSeparators(ACh);
end;

function TdxSpellCheckerWordIterator.IsLetterOrDigit(const ACh: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}ACh.IsLetterOrDigit{$ELSE}TCharacter.
    IsLetterOrDigit(ACh){$ENDIF};
end;

function TdxSpellCheckerWordIterator.IsNotLetterOrDigit
  (const ACh: Char): Boolean;
begin
  Result := not IsLetterOrDigit(ACh);
end;

procedure TdxSpellCheckerWordIterator.SkipBackwardInvisibleRuns
  (var APos: TdxDocumentModelPosition);
begin
  if not VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    MoveToPrevChar(APos);
end;

procedure TdxSpellCheckerWordIterator.SkipForwardInvisibleRuns
  (var APos: TdxDocumentModelPosition);
begin
  if not VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    MoveToNextChar(APos);
end;

function TdxSpellCheckerWordIterator.GetVisibleTextFilter: IdxVisibleTextFilter;
begin
  Result := PieceTable.VisibleTextFilter;
end;

procedure TdxSpellCheckerWordIterator.SkipForward
  (var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsEndOfDocument(APos) and APredicate(GetCharacter(APos)) do
    MoveToNextChar(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToNextChar
  (var APos: TdxDocumentModelPosition);
var
  ARuns: TdxTextRunCollection;
  AEndRunIndex: TdxRunIndex;
begin
  if IsEndOfDocument(APos) then
    Exit;
  APos.LogPosition := APos.LogPosition + 1;
  if APos.LogPosition <= APos.RunEndLogPosition then
    Exit;

  ARuns := PieceTable.Runs;
  AEndRunIndex := ARuns.Count - 1;
  while MoveToNextRun(APos, APos.LogPosition) and (APos.RunIndex < AEndRunIndex)
    and not VisibleTextFilter.IsRunVisible(APos.RunIndex) do
    APos.LogPosition := APos.LogPosition + ARuns[APos.RunIndex].Length;
end;

function TdxSpellCheckerWordIterator.MoveToNextRun
  (var APos: TdxDocumentModelPosition;
  ARunStartPos: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
  if IsEndOfDocument(APos) then
    Exit;
  Result := True;
  APos.RunIndex := APos.RunIndex + 1;
  APos.ParagraphIndex := PieceTable.Runs[APos.RunIndex].Paragraph.Index;
  APos.RunStartLogPosition := ARunStartPos;
end;

procedure TdxSpellCheckerWordIterator.SkipBackward
  (var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsStartOfDocument(APos) and APredicate(GetCharacter(APos)) do
    MoveToPrevChar(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToPrevChar
  (var APos: TdxDocumentModelPosition);
var
  ARuns: TdxTextRunCollection;
  AFirstRunIndex: TdxRunIndex;
begin
  APos.LogPosition := APos.LogPosition - 1;
  if APos.LogPosition >= APos.RunStartLogPosition then
    Exit;

  ARuns := PieceTable.Runs;
  MoveToPrevRun(APos, APos.LogPosition);
  AFirstRunIndex := 0;
  while (APos.RunIndex > AFirstRunIndex) and not VisibleTextFilter.IsRunVisible
    (APos.RunIndex) do
  begin
    APos.LogPosition := APos.RunEndLogPosition - ARuns[APos.RunIndex].Length;
    MoveToPrevRun(APos, APos.LogPosition);
  end;
end;

procedure TdxSpellCheckerWordIterator.MoveToPrevRun
  (var APos: TdxDocumentModelPosition; ARunEndPos: TdxDocumentLogPosition);
var
  ARun: TdxTextRunBase;
begin
  APos.RunIndex := APos.RunIndex - 1;
  ARun := PieceTable.Runs[APos.RunIndex];
  APos.ParagraphIndex := ARun.Paragraph.Index;
  APos.RunStartLogPosition := ARunEndPos - ARun.Length + 1;
end;

procedure TdxSpellCheckerWordIterator.MoveForwardCore
  (var APos: TdxDocumentModelPosition);
var
  APosClone: TdxDocumentModelPosition;
begin
  APosClone := APos;
  SkipForwardInvisibleRuns(APosClone);
  if not IsLetterOrDigit(GetCharacter(APosClone)) then
    SkipForward(APosClone, IsNotLetterOrDigit);
  if IsEndOfDocument(APosClone) then
    Exit;
  APos.CopyFrom(APosClone);
  MoveToWordEndCore(APos);
end;

function TdxSpellCheckerWordIterator.GetCharacter
  (const APos: TdxDocumentModelPosition): Char;
begin
  if APos.RunIndex <> FCachedRunIndex then
  begin
    FCachedRunIndex := APos.RunIndex;
    FCachedRunText := PieceTable.GetRunNonEmptyText(FCachedRunIndex);
  end;
  Result := FCachedRunText[APos.RunOffset + 1];
end;

procedure TdxSpellCheckerWordIterator.MoveBackCore
  (var APos: TdxDocumentModelPosition);
begin
  SkipBackwardInvisibleRuns(APos);
  if not IsLetterOrDigit(GetCharacter(APos)) then
    SkipBackward(APos, IsNotLetterOrDigit);
  SkipBackward(APos, IsNotWordsSeparators);
  SkipBackward(APos, IsNotLetterOrDigit);
  if IsStartOfDocument(APos) then
    Exit;
  MoveToWordStartCore(APos);
  MoveToWordEndCore(APos);
end;

function TdxSpellCheckerWordIterator.MoveToWordEnd
  (const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  SkipForwardInvisibleRuns(Result);
  if IsEndOfDocument(Result) then
    Exit(Result);
  MoveToWordEndCore(Result);
end;

procedure TdxSpellCheckerWordIterator.MoveToWordEndCore
  (var APos: TdxDocumentModelPosition; AWordWithDots: Boolean = False);
var
  ANewPos: TdxDocumentModelPosition;
  AIsLastCharDot: Boolean;
begin
  SkipForward(APos, IsLetterOrDigit);
  if IsWordsSeparators(GetCharacter(APos)) or IsEndOfDocument(APos) then
    Exit;

  ANewPos := APos;
  AIsLastCharDot := GetCharacter(ANewPos) = '.';
  MoveToNextChar(ANewPos);
  if not IsLetterOrDigit(GetCharacter(ANewPos)) then
  begin
    if (AWordWithDots and AIsLastCharDot) and not IsEndOfDocument(ANewPos) then
      APos.CopyFrom(ANewPos);
    Exit;
  end;
  APos.CopyFrom(ANewPos);
  MoveToWordEndCore(APos, AWordWithDots or AIsLastCharDot);
end;

function TdxSpellCheckerWordIterator.MoveToWordStart
  (const APos: TdxDocumentModelPosition;
  ASkipNotLetterOrDigitChar: Boolean = True): TdxDocumentModelPosition;
begin
  Result := APos;
  SkipBackwardInvisibleRuns(Result);
  if not IsLetterOrDigit(GetCharacter(Result)) then
  begin
    if not ASkipNotLetterOrDigitChar then
      Exit(Result);
    SkipBackward(Result, IsNotLetterOrDigit);
  end;
  MoveToWordStartCore(Result);
end;

procedure TdxSpellCheckerWordIterator.MoveToWordStartCore
  (var APos: TdxDocumentModelPosition);
var
  ANewPos: TdxDocumentModelPosition;
begin
  SkipBackward(APos, IsLetterOrDigit);
  if IsWordsSeparators(GetCharacter(APos)) then
  begin
    MoveToNextChar(APos);
    Exit;
  end;
  if IsStartOfDocument(APos) then
    Exit;
  ANewPos := APos;
  MoveToPrevChar(ANewPos);
  if not IsLetterOrDigit(GetCharacter(ANewPos)) then
  begin
    MoveToNextChar(APos);
    Exit;
  end;
  APos.CopyFrom(ANewPos);
  MoveToWordStartCore(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToNextWordEnd
  (var APos: TdxDocumentModelPosition);
begin
  SkipForwardInvisibleRuns(APos);
  if IsEndOfDocument(APos) then
    Exit;
  if not IsLetterOrDigit(GetCharacter(APos)) then
    SkipForward(APos, IsNotLetterOrDigit);
  SkipForward(APos, IsNotWordsSeparators);
  SkipForward(APos, IsNotLetterOrDigit);
  MoveToWordEndCore(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToPrevWordStart
  (var APos: TdxDocumentModelPosition);
begin
  SkipBackwardInvisibleRuns(APos);
  if IsStartOfDocument(APos) then
    Exit;
  if not IsLetterOrDigit(GetCharacter(APos)) then
    SkipBackward(APos, IsNotLetterOrDigit);
  SkipBackward(APos, IsNotWordsSeparators);
  SkipBackward(APos, IsNotLetterOrDigit);
  MoveToWordStartCore(APos);
end;

end.
