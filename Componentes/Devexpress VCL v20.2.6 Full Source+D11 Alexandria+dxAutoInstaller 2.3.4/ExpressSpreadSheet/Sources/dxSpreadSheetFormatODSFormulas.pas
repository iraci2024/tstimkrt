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

unit dxSpreadSheetFormatODSFormulas;

{$I cxVer.Inc}

interface

uses
  Windows, Classes, Generics.Collections, Generics.Defaults,
  dxCore, dxSpreadSheetTypes, dxSpreadSheetFormulas, dxSpreadSheetCore,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreReferences, dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetCoreFormulasParser;

type

  { TdxSpreadSheetODSFormula }

  TdxSpreadSheetODSFormula = class
  public
    class function Convert(const ASpreadSheet: TdxCustomSpreadSheet;
      const S: string): string;
  end;

  { TdxSpreadSheetODSFormulaHelper }

  TdxSpreadSheetODSFormulaHelper = class(TdxSpreadSheetFormula)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    function GetController: TdxSpreadSheetCustomFormulaController; override;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); reintroduce;
  end;

  { TdxSpreadSheetODSFormulaParser }

  TdxSpreadSheetODSFormulaParser = class(TdxSpreadSheetFormulaParser)
  strict private
  const
    StateODSReference = TdxSpreadSheetFormulaParser.StateLast + 1;
  strict private
    function ParseReference(var APosition: Integer; AFinishPos: Integer;
      out AColumnIndex, ARowIndex: Integer;
      out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
  protected
    function CreateTokenODSReference
      (AStack: TdxSpreadSheetFormulaParserTokenList)
      : TdxSpreadSheetFormulaToken;
    function IsFormulaText(const S: string; out AOffset: Integer)
      : Boolean; override;
    procedure PopulateStateMachine; override;
  public const
    Tag = 'of:=';
    Tag2 = 'oooc:=';
  public
    function ExtractParams(const AFunctionParams: string): TStringList;
  end;

implementation

uses
  SysUtils, dxSpreadSheetStrs, dxSpreadSheetUtils, dxSpreadSheetFormatUtils,
  dxSpreadSheetClasses,
  dxSpreadSheetCoreStrs, StrUtils;

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);

  { TdxSpreadSheetODSFormula }

class function TdxSpreadSheetODSFormula.Convert(const ASpreadSheet
  : TdxCustomSpreadSheet; const S: string): string;
var
  AFormula: TdxSpreadSheetFormula;
  AParser: TdxSpreadSheetODSFormulaParser;
begin
  AParser := TdxSpreadSheetODSFormulaParser.Create(ASpreadSheet);
  try
    AFormula := TdxSpreadSheetODSFormulaHelper.Create(ASpreadSheet);
    try
      if AParser.ParseFormula(S, AFormula) then
        Result := AFormula.AsText
      else
        Result := '';
    finally
      AFormula.Free;
    end;
  finally
    AParser.Free;
  end;
end;

{ TdxSpreadSheetODSFormulaHelper }

constructor TdxSpreadSheetODSFormulaHelper.Create(ASpreadSheet
  : TdxCustomSpreadSheet);
begin
  inherited Create(nil);
  FSpreadSheet := ASpreadSheet;
end;

function TdxSpreadSheetODSFormulaHelper.GetController
  : TdxSpreadSheetCustomFormulaController;
begin
  Result := TdxCustomSpreadSheetAccess(FSpreadSheet).FormulaController;
end;

{ TdxSpreadSheetODSFormulaParser }

function TdxSpreadSheetODSFormulaParser.ExtractParams(const AFunctionParams
  : string): TStringList;
var
  AFinishPos: Integer;
  ALength: Integer;
  APosition: Integer;
begin
  Result := TStringList.Create;

  FFormulaText := AFunctionParams;
  APosition := 0;
  AFinishPos := Length(AFunctionParams) - 1;
  repeat
    ALength := FindSubExpressionRightBound(',', APosition, AFinishPos) -
      APosition;
    if ALength = 0 then
      ALength := AFinishPos - APosition + 1;
    if ALength > 0 then
    begin
      Result.Add(Copy(AFunctionParams, APosition + 1, ALength));
      Inc(APosition, ALength + 1);
    end;
  until ALength <= 0;
end;

function TdxSpreadSheetODSFormulaParser.CreateTokenODSReference
  (AStack: TdxSpreadSheetFormulaParserTokenList): TdxSpreadSheetFormulaToken;
var
  AColumn1Index, AColumn2Index: Integer;
  ALink1, ALink2: TdxSpreadSheet3DReferenceCustomLink;
  ARow1Index, ARow2Index: Integer;
  AToken: TdxSpreadSheetFormulaParserToken;
  ASourceStart: Word;
begin
  ARow2Index := -1;
  AColumn2Index := -1;

  ASourceStart := GetSourceStart(AStack);
  AToken := AStack.Pop;
  if not ParseReference(AToken.StartPosition, AToken.FinishPosition,
    AColumn1Index, ARow1Index, ALink1) then
  begin
    SetErrorIndex(AToken.StartPosition, pecInvalidReference);
    Exit(nil);
  end;

  if CheckText(AToken.StartPosition, AToken.FinishPosition, ':') then
  begin
    Inc(AToken.StartPosition);
    ParseReference(AToken.StartPosition, AToken.FinishPosition, AColumn2Index,
      ARow2Index, ALink2);
  end
  else
    ALink2 := nil;

  if (ARow2Index >= 0) and (AColumn2Index >= 0) then
    Result := Create3DAreaReferenceToken
      (TdxSpreadSheetReference.Create(ARow1Index, True),
      TdxSpreadSheetReference.Create(AColumn1Index, True),
      TdxSpreadSheetReference.Create(ARow2Index, True),
      TdxSpreadSheetReference.Create(AColumn2Index, True), ALink1, ALink2)
  else
    Result := Create3DReferenceToken(TdxSpreadSheetReference.Create(ARow1Index,
      True), TdxSpreadSheetReference.Create(AColumn1Index, True), ALink1);

  InitializeSourceBounds(Result, ASourceStart, AToken.FinishPosition);
end;

function TdxSpreadSheetODSFormulaParser.IsFormulaText(const S: string;
  out AOffset: Integer): Boolean;
begin
  if StartsStr(Tag, S) then
  begin
    AOffset := Length(Tag);
    Exit(True);
  end;

  if StartsStr(Tag2, S) then
  begin
    AOffset := Length(Tag2);
    Exit(True);
  end;

  Result := False;
end;

procedure TdxSpreadSheetODSFormulaParser.PopulateStateMachine;
var
  AState: TdxSpreadSheetFormulaParserState;
begin
  inherited;

  AState := StateMachine.GetState(StateStart);
  AState.SetNextState(TokenExternalReference, StateODSReference);

  StateMachine.CreateState(StateODSReference)
    .SetTokenCreator(CreateTokenODSReference);
end;

function TdxSpreadSheetODSFormulaParser.ParseReference(var APosition: Integer;
  AFinishPos: Integer; out AColumnIndex, ARowIndex: Integer;
  out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;

  procedure GetToken(out S, L: Integer);
  var
    AStringMark: Char;
  begin
    if CheckText(APosition, AFinishPos, dxStringMarkChar) then
      AStringMark := dxStringMarkChar
    else if CheckText(APosition, AFinishPos, dxStringMarkChar2) then
      AStringMark := dxStringMarkChar2
    else
      AStringMark := #0;

    if AStringMark <> #0 then
    begin
      Inc(APosition);
      S := APosition;
      AFinishPos := FindStringRightBound(FFormulaText, AStringMark, APosition,
        AFinishPos);
      if AFinishPos > 0 then
      begin
        L := AFinishPos - APosition + 1;
        APosition := AFinishPos + 2;
      end
      else
        SetErrorIndex(APosition - 1, pecUnexpectedEndOfString);
    end
    else
    begin
      S := APosition;
      while (APosition <= AFinishPos) and
        not dxCharInSet(FFormulaText[APosition], ['.', '#', ']', '$', ':']) do
        Inc(APosition);
      L := APosition - S;
    end;
  end;

var
  ASheet: TObject;
  L, S: Integer;
begin
  ARowIndex := -1;
  AColumnIndex := -1;

  GetToken(S, L);
  if CheckText(APosition, AFinishPos, '#') then
  begin
    ALink := TdxSpreadSheet3DExternalReferenceLink.Create
      (SpreadSheet.ExternalLinks.Add(Copy(FFormulaSourceText, S, L)));
    Inc(APosition);
    if not CheckText(APosition, AFinishPos, '$') then
    begin
      SetErrorIndex(APosition, pecInvalidReference);
      FreeAndNil(ALink);
      Exit(False);
    end;
    Inc(APosition);
    GetToken(S, L);
    TdxSpreadSheet3DExternalReferenceLink(ALink).Name :=
      Copy(FFormulaSourceText, S, L);
  end
  else
  begin
    if CheckText(APosition, AFinishPos, '$') then
    begin
      Inc(APosition);
      GetToken(S, L);
    end;
    if L > 0 then
    begin
      if GetViewByName(S, L, ASheet) then
        ALink := TdxSpreadSheet3DReferenceLink.Create(ASheet)
      else
        ALink := TdxSpreadSheet3DReferenceLink.Create
          (TdxSpreadSheetInvalidObject.Instance)
    end
    else
      ALink := nil;
  end;

  if not CheckText(APosition, AFinishPos, '.') then
  begin
    SetErrorIndex(APosition, pecInvalidReference);
    FreeAndNil(ALink);
    Exit(False);
  end;

  Inc(APosition);
  if CheckText(APosition, AFinishPos, '$') then
    Inc(APosition);
  GetToken(S, L);
  if CheckText(APosition, AFinishPos, '$') then
  begin
    AColumnIndex := TdxSpreadSheetColumnReferences.IndexByName
      (Copy(FFormulaSourceText, S, L));
    Inc(APosition);
    GetToken(S, L);
    ARowIndex := StrToIntDef(Copy(FFormulaSourceText, S, L), 0) - 1;
  end
  else
    dxStringToReference(Copy(FFormulaSourceText, S, L), AColumnIndex,
      ARowIndex);

  Result := True;
end;

end.
