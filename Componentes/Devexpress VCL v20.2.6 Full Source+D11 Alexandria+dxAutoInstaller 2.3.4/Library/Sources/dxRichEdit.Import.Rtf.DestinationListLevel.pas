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

unit dxRichEdit.Import.Rtf.DestinationListLevel; // for internal use

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, Windows, Math, Generics.Defaults,
  Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Section;

type
  TdxListLevelNumbersDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxListLevelTextDestination }

  TdxListLevelTextDestination = class(TdxStringValueDestination)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateInnerKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FLevelTemplateId: Integer;
    class procedure ListLevelTemplateIdKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    function CreateEmptyClone: TdxStringValueDestination; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;

    property LevelTemplateId: Integer read FLevelTemplateId;
  end;

  { TdxListLevelDestination }

  TdxListLevelDestination = class(TdxRichEditRtfDestinationBase)
  public const
    NumberingFormats: array [0 .. 65] of TdxNumberingFormat =
      (TdxNumberingFormat.Decimal, TdxNumberingFormat.UpperRoman,
      TdxNumberingFormat.LowerRoman, TdxNumberingFormat.UpperLetter,
      TdxNumberingFormat.LowerLetter, TdxNumberingFormat.Ordinal,
      TdxNumberingFormat.CardinalText, TdxNumberingFormat.OrdinalText,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.AIUEOHiragana,
      TdxNumberingFormat.Iroha, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.DecimalEnclosedCircle, TdxNumberingFormat.None,
      TdxNumberingFormat.AIUEOFullWidthHiragana,
      TdxNumberingFormat.IrohaFullWidth, TdxNumberingFormat.DecimalZero,
      TdxNumberingFormat.Bullet, TdxNumberingFormat.Ganada,
      TdxNumberingFormat.Chosung, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.KoreanCounting, TdxNumberingFormat.KoreanDigital,
      TdxNumberingFormat.KoreanDigital2, TdxNumberingFormat.KoreanLegal,
      TdxNumberingFormat.Hebrew1, TdxNumberingFormat.ArabicAlpha,
      TdxNumberingFormat.Hebrew2, TdxNumberingFormat.ArabicAbjad,
      TdxNumberingFormat.HindiVowels, TdxNumberingFormat.HindiConsonants,
      TdxNumberingFormat.HindiNumbers, TdxNumberingFormat.HindiDescriptive,
      TdxNumberingFormat.ThaiLetters, TdxNumberingFormat.ThaiNumbers,
      TdxNumberingFormat.ThaiDescriptive,
      TdxNumberingFormat.VietnameseDescriptive, TdxNumberingFormat.NumberInDash,
      TdxNumberingFormat.RussianLower, TdxNumberingFormat.RussianUpper,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None, TdxNumberingFormat.None,
      TdxNumberingFormat.None);
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FLevel: TdxRtfListLevel;

    procedure ApplyListLevelCharacterProperties;
    procedure ApplyListLevelParagraphProperties;
    procedure TryToHandleFinishOfListLevelNumbersDestination(ANestedDestination
      : TdxRichEditRtfDestinationBase);
    procedure TryToHandleFinishOfListLevelTextDestination(ANestedDestination
      : TdxRichEditRtfDestinationBase);

    class procedure ListLevelAlignmentKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelFollowKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelIndentKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelLegalKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelNoRestartKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelNumberingFormatKeywordHandler
      (AImporter: TdxRtfImporter; AParameterValue: Integer;
      AHasParameter: Boolean); inline; static;
    class procedure ListLevelNumbersKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelOldKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelPictureKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelPictureNoSizeKeywordHandler
      (AImporter: TdxRtfImporter; AParameterValue: Integer;
      AHasParameter: Boolean); inline; static;
    class procedure ListLevelPrevKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelPrevspaceKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelSpaceKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelStartAtKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelTentativeKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelTextKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ParagraphStyleKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    procedure SetLevel(const Value: TdxRtfListLevel);
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessControlCharCore(AChar: Char); override;
    procedure ProcessCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string;
      AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
    procedure BeforePopRtfState; override;
    procedure NestedGroupFinished(ADestination
      : TdxRichEditRtfDestinationBase); override;
    class function NumberingFormatsIndexOf(ANumberingFormat: TdxNumberingFormat)
      : Integer; static;

    property Level: TdxRtfListLevel read FLevel;
  end;

  TdxListOverrideLevelDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FOverrideLevel: TdxRtfListOverrideLevel;
    class procedure ListOverrideFormatKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideListLevelKeywordHandler
      (AImporter: TdxRtfImporter; AParameterValue: Integer;
      AHasParameter: Boolean); inline; static;
    class procedure ListOverrideStartAtKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideStartAtValueKeywordHandler
      (AImporter: TdxRtfImporter; AParameterValue: Integer;
      AHasParameter: Boolean); inline; static;
    procedure SetOverrideLevel(const Value: TdxRtfListOverrideLevel);
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
  public
    constructor Create(AImporter: TdxRtfImporter); override;

    property OverrideLevel: TdxRtfListOverrideLevel read FOverrideLevel;
  end;

implementation

uses
  RTLConsts, Contnrs,
  dxRichEdit.Import.Rtf.DestinationDefault,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxCharacters,
  dxFontHelpers,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Import.Rtf.ParagraphFormatting;

{ TdxListLevelNumbersDestination }

function TdxListLevelNumbersDestination.CreateEmptyClone
  : TdxStringValueDestination;
begin
  Result := TdxListLevelNumbersDestination.Create(Importer);
end;

{ TdxListLevelTextDestination }

function TdxListLevelTextDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := inherited CreateClone;
  TdxListLevelTextDestination(Result).FLevelTemplateId := LevelTemplateId;
end;

class function TdxListLevelTextDestination.GetKeywordHT
  : TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxListLevelTextDestination.Initialize;
begin
  FKeywordHT := CreateInnerKeywordHT;
end;

class destructor TdxListLevelTextDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

function TdxListLevelTextDestination.CreateEmptyClone
  : TdxStringValueDestination;
begin
  Result := TdxListLevelTextDestination.Create(Importer);
end;

class function TdxListLevelTextDestination.CreateInnerKeywordHT
  : TdxKeywordTranslatorTable;
begin
  Result := CreateKeywordHT;
  Result.Add('leveltemplateid', ListLevelTemplateIdKeywordHandler);
end;

class procedure TdxListLevelTextDestination.ListLevelTemplateIdKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  TdxListLevelTextDestination(AImporter.Destination).FLevelTemplateId :=
    IfThen(AHasParameter, AParameterValue, 0);
end;

{ TdxListLevelDestination }

constructor TdxListLevelDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FLevel := TdxRtfListLevel.Create(Importer.DocumentModel);
  Importer.RtfLevels.Add(FLevel);
end;

function TdxListLevelDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxListLevelDestination.Create(Importer);
  TdxListLevelDestination(Result).SetLevel(Level);
end;

class function TdxListLevelDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxListLevelDestination.CreateKeywordHT
  : TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('levelstartat', ListLevelStartAtKeywordHandler);
  Result.Add('lvltentative', ListLevelTentativeKeywordHandler);
  Result.Add('levelnfc', ListLevelNumberingFormatKeywordHandler);
  Result.Add('leveljc', ListLevelAlignmentKeywordHandler);
  Result.Add('levelnfcn', ListLevelNumberingFormatKeywordHandler);
  Result.Add('leveljcn', ListLevelAlignmentKeywordHandler);
  Result.Add('levelold', ListLevelOldKeywordHandler);
  Result.Add('levelprev', ListLevelPrevKeywordHandler);
  Result.Add('levelprevspace', ListLevelPrevspaceKeywordHandler);
  Result.Add('levelindent', ListLevelIndentKeywordHandler);
  Result.Add('levelspace', ListLevelSpaceKeywordHandler);
  Result.Add('leveltext', ListLevelTextKeywordHandler);
  Result.Add('levelnumbers', ListLevelNumbersKeywordHandler);
  Result.Add('levelfollow', ListLevelFollowKeywordHandler);
  Result.Add('levellegal', ListLevelLegalKeywordHandler);
  Result.Add('levelnorestart', ListLevelNoRestartKeywordHandler);
  Result.Add('levelpicture', ListLevelPictureKeywordHandler);
  Result.Add('levelpicturenosize', ListLevelPictureNoSizeKeywordHandler);
  Result.Add('s', ParagraphStyleKeywordHandler);
  TdxDefaultDestination.AddParagraphPropertiesKeywords(Result);
  TdxDefaultDestination.AddCharacterPropertiesKeywords(Result);
end;

class constructor TdxListLevelDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxListLevelDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

procedure TdxListLevelDestination.BeforePopRtfState;
begin
  ApplyListLevelParagraphProperties;
  ApplyListLevelCharacterProperties;
end;

procedure TdxListLevelDestination.NestedGroupFinished
  (ADestination: TdxRichEditRtfDestinationBase);
begin
  TryToHandleFinishOfListLevelTextDestination(ADestination);
  TryToHandleFinishOfListLevelNumbersDestination(ADestination);
end;

class function TdxListLevelDestination.NumberingFormatsIndexOf(ANumberingFormat
  : TdxNumberingFormat): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(NumberingFormats) - 1 do
    if NumberingFormats[I] = ANumberingFormat then
      Exit(I);
  Result := -1;
end;

procedure TdxListLevelDestination.SetLevel(const Value: TdxRtfListLevel);
begin
  Importer.RtfLevels.Extract(FLevel);
  FLevel.Free;
  FLevel := Value;
end;

procedure TdxListLevelDestination.ProcessCharCore(AChar: Char);
begin
  // do nothing
end;

procedure TdxListLevelDestination.ProcessControlCharCore(AChar: Char);
begin
  // do nothing
end;

function TdxListLevelDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
var
  ATranslator: TdxTranslateKeywordEvent;
begin
  if KeywordHT.TryGetValue(AKeyword, ATranslator) then
  begin
    ATranslator(Importer, AParameterValue, AHasParameter);
    Result := True;
  end
  else
    Result := False;
end;

procedure TdxListLevelDestination.ApplyListLevelCharacterProperties;
var
  ADestination: TdxListLevelDestination;
begin
  ADestination := TdxListLevelDestination(Importer.Destination);
  ADestination.Level.CharacterProperties.CopyFrom
    (Importer.Position.CharacterFormatting);
end;

procedure TdxListLevelDestination.ApplyListLevelParagraphProperties;
var
  ADestination: TdxListLevelDestination;
  AFormatting: TdxParagraphFormattingBase;
  AParentParagraphProperties: TdxMergedParagraphProperties;
  AFormattingInfo: TdxRtfParagraphFormattingInfo;
begin
  ADestination := TdxListLevelDestination(Importer.Destination);
  AFormatting := Importer.DocumentModel.DefaultParagraphProperties.Info;
  AParentParagraphProperties := TdxMergedParagraphProperties.Create
    (AFormatting.Info, AFormatting.Options);
  try
    AFormattingInfo := Importer.Position.ParagraphFormattingInfo;
    Importer.ApplyLineSpacing(AFormattingInfo);
    Importer.ApplyParagraphProperties(ADestination.Level.ParagraphProperties,
      AFormattingInfo, AParentParagraphProperties);
  finally
    AParentParagraphProperties.Free;
  end;
end;

procedure TdxListLevelDestination.TryToHandleFinishOfListLevelNumbersDestination
  (ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ACurrentDestination: TdxListLevelDestination;
  ADestination: TdxListLevelNumbersDestination;
begin
  if ANestedDestination is TdxListLevelNumbersDestination then
  begin
    ACurrentDestination := TdxListLevelDestination(Importer.Destination);
    ADestination := TdxListLevelNumbersDestination(ANestedDestination);
    ACurrentDestination.Level.Number := ADestination.Value;
  end;
end;

procedure TdxListLevelDestination.TryToHandleFinishOfListLevelTextDestination
  (ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ACurrentDestination: TdxListLevelDestination;
  ADestination: TdxListLevelTextDestination;
begin
  if ANestedDestination is TdxListLevelTextDestination then
  begin
    ACurrentDestination := TdxListLevelDestination(Importer.Destination);
    ADestination := TdxListLevelTextDestination(ANestedDestination);
    ACurrentDestination.Level.Text := ADestination.Value;
    ACurrentDestination.Level.ListLevelProperties.TemplateCode :=
      ADestination.LevelTemplateId;
  end;
end;

class procedure TdxListLevelDestination.ListLevelAlignmentKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    TdxListLevelDestination(AImporter.Destination)
      .Level.ListLevelProperties.Alignment :=
      TdxListNumberAlignment(AParameterValue);
end;

class procedure TdxListLevelDestination.ListLevelFollowKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListLevelDestination;
begin
  if AHasParameter then
  begin
    ADestination := TdxListLevelDestination(AImporter.Destination);
    case AParameterValue of
      0:
        ADestination.Level.ListLevelProperties.Separator :=
          TdxCharacters.TabMark;
      1:
        ADestination.Level.ListLevelProperties.Separator := TdxCharacters.Space;
    else
      ADestination.Level.ListLevelProperties.Separator := #0;
    end;
  end;
end;

class procedure TdxListLevelDestination.ListLevelIndentKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    TdxListLevelDestination(AImporter.Destination)
      .Level.ListLevelProperties.LegacyIndent :=
      AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxListLevelDestination.ListLevelLegalKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    TdxListLevelDestination(AImporter.Destination)
      .Level.ListLevelProperties.ConvertPreviousLevelNumberingToDecimal :=
      AParameterValue <> 0;
end;

class procedure TdxListLevelDestination.ListLevelNoRestartKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    TdxListLevelDestination(AImporter.Destination)
      .Level.ListLevelProperties.SuppressRestart := AParameterValue <> 0;
end;

class procedure TdxListLevelDestination.ListLevelNumberingFormatKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListLevelDestination;
begin
  if AHasParameter and (AParameterValue >= 0) and
    (AParameterValue < Length(NumberingFormats)) then
  begin
    ADestination := TdxListLevelDestination(AImporter.Destination);
    ADestination.Level.ListLevelProperties.Format :=
      NumberingFormats[AParameterValue];
  end;
end;

class procedure TdxListLevelDestination.ListLevelNumbersKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxListLevelNumbersDestination.Create(AImporter);
end;

class procedure TdxListLevelDestination.ListLevelOldKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0) then
    TdxListLevelDestination(AImporter.Destination)
      .Level.ListLevelProperties.Legacy := True;
end;

class procedure TdxListLevelDestination.ListLevelPictureKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  // do nothing
end;

class procedure TdxListLevelDestination.ListLevelPictureNoSizeKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListLevelDestination;
begin
  ADestination := TdxListLevelDestination(AImporter.Destination);
  ADestination.Level.ListLevelProperties.SuppressBulletResize := True;
end;

class procedure TdxListLevelDestination.ListLevelPrevKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  // do nothing
end;

class procedure TdxListLevelDestination.ListLevelPrevspaceKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  // do nothing
end;

class procedure TdxListLevelDestination.ListLevelSpaceKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListLevelDestination;
begin
  if AHasParameter then
  begin
    ADestination := TdxListLevelDestination(AImporter.Destination);
    ADestination.Level.ListLevelProperties.LegacySpace :=
      AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  end;
end;

class procedure TdxListLevelDestination.ListLevelStartAtKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    TdxListLevelDestination(AImporter.Destination)
      .Level.ListLevelProperties.Start := AParameterValue;
end;

class procedure TdxListLevelDestination.ListLevelTentativeKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  // do nothing
end;

class procedure TdxListLevelDestination.ListLevelTextKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxListLevelTextDestination.Create(AImporter)
end;

class procedure TdxListLevelDestination.ParagraphStyleKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListLevelDestination;
begin
  ADestination := TdxListLevelDestination(AImporter.Destination);
  if AImporter.ParagraphStyleCollectionIndex.ContainsKey(AParameterValue) then
    ADestination.Level.ParagraphStyleIndex :=
      AImporter.ParagraphStyleCollectionIndex[AParameterValue];
end;

{ TdxListOverrideLevelDestination }

constructor TdxListOverrideLevelDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FOverrideLevel := TdxRtfListOverrideLevel.Create(Importer.DocumentModel);
end;

function TdxListOverrideLevelDestination.CreateClone
  : TdxRichEditRtfDestinationBase;
begin
  Result := TdxListOverrideLevelDestination.Create(Importer);
  TdxListOverrideLevelDestination(Result).SetOverrideLevel(OverrideLevel);
end;

class function TdxListOverrideLevelDestination.GetKeywordHT
  : TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxListOverrideLevelDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxListOverrideLevelDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxListOverrideLevelDestination.CreateKeywordHT
  : TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('listoverrideformat', ListOverrideFormatKeywordHandler);
  Result.Add('listoverridestartat', ListOverrideStartAtKeywordHandler);
  Result.Add('levelstartat', ListOverrideStartAtValueKeywordHandler);
  Result.Add('listlevel', ListOverrideListLevelKeywordHandler);
end;

procedure TdxListOverrideLevelDestination.SetOverrideLevel
  (const Value: TdxRtfListOverrideLevel);
begin
  FOverrideLevel.Free;
  FOverrideLevel := Value;
end;

class procedure TdxListOverrideLevelDestination.ListOverrideFormatKeywordHandler
  (AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  TdxListOverrideLevelDestination(AImporter.Destination)
    .OverrideLevel.OverrideFormat := True;
end;

class procedure TdxListOverrideLevelDestination.
  ListOverrideListLevelKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListOverrideLevelDestination;
  ANewDestination: TdxListLevelDestination;
begin
  ADestination := TdxListOverrideLevelDestination(AImporter.Destination);
  ANewDestination := TdxListLevelDestination.Create(AImporter);
  AImporter.Destination := ANewDestination;
  ADestination.OverrideLevel.Level := ANewDestination.Level;
end;

class procedure TdxListOverrideLevelDestination.
  ListOverrideStartAtKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  TdxListOverrideLevelDestination(AImporter.Destination)
    .OverrideLevel.OverrideStartAt := True;
end;

class procedure TdxListOverrideLevelDestination.
  ListOverrideStartAtValueKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    TdxListOverrideLevelDestination(AImporter.Destination).OverrideLevel.StartAt
      := AParameterValue;
end;

end.
