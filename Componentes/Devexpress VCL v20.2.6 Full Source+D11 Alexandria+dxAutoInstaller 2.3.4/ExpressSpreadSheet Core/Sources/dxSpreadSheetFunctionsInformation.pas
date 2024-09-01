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

unit dxSpreadSheetFunctionsInformation; // for internal use

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnIsBlank(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsErr(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsError(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsEven(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsLogical(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsNA(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsNonText(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsNumber(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsOdd(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsRef(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsText(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnN(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
procedure fnNA(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);

procedure fpiIsBlank(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsErr(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsError(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsEven(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsLogical(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsNA(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsNonText(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsNumber(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsOdd(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsRef(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsText(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiN(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNA(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

implementation

uses
  cxVariants, Classes,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetFunctionsStrs;

type
  TdxSpreadSheetCustomFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TdxSpreadSheetFormulaReferenceTokenAccess = class
    (TdxSpreadSheetFormulaReferenceToken);

procedure SetSenderResult(Sender: TdxSpreadSheetFormulaResult; AValue: Variant);
begin
  Sender.SetError(ecNone);
  Sender.AddValue(AValue);
end;

procedure fnIsBlank(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AValue: Variant;
begin
  if Sender.ExtractParameter(AValue, AParams) then
    Sender.AddValue(dxSpreadSheetIsNullValue(AValue));
end;

procedure fnSetCustomErrorResult(Sender: TdxSpreadSheetFormulaResult;
  AResult: Boolean);
begin
  Sender.AddValue(AResult);
  Sender.SetError(ecNone);
end;

procedure fnIsErr(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
begin
  fnSetCustomErrorResult(Sender, not(Sender.ExtractErrorCode(AParams)
    in [ecNone, ecNA]));
end;

procedure fnIsError(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
begin
  fnSetCustomErrorResult(Sender, Sender.ExtractErrorCode(AParams) <> ecNone);
end;

procedure fnIsEven(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameterWithoutBoolean(AParameter, AParams) then
    Sender.AddValue(not Odd(Trunc(AParameter)));
end;

procedure fnIsLogical(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractParameter(AParameter, AParams) then
    Sender.AddValue(dxIsLogical(AParameter));
end;

procedure fnIsNA(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
begin
  fnSetCustomErrorResult(Sender, Sender.ExtractErrorCode(AParams) = ecNA);
end;

procedure fnIsNonText(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractParameter(AParameter, AParams) then
    Sender.AddValue(not dxIsText(AParameter))
  else
    SetSenderResult(Sender, True);
end;

procedure fnIsNumber(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractParameter(AParameter, AParams) then
    Sender.AddValue(dxIsNumberOrDateTime(AParameter))
  else
    SetSenderResult(Sender, False);
end;

procedure fnIsOdd(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractNumericParameterWithoutBoolean(AParameter, AParams) then
    Sender.AddValue(Odd(Trunc(AParameter)));
end;

procedure fnIsRef(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  ATempResult: TdxSpreadSheetFormulaResult;
begin
  if AParams.ChildCount > 1 then
    Sender.AddValue(False)
  else if AParams.FirstChild is TdxSpreadSheetFormulaReferenceToken then
    Sender.AddValue(TdxSpreadSheetFormulaReferenceTokenAccess
      (AParams.FirstChild).IsValid)
  else if AParams.FirstChild is TdxSpreadSheetFormulaCustomDefinedNameToken then
  begin
    ATempResult := TdxSpreadSheetCustomFormulaAccess(Sender.Owner)
      .Calculate(AParams.FirstChild);
    try
      Sender.AddValue((ATempResult.Count > 0) and
        (ATempResult.LastItem is TdxSpreadSheetFormulaReferenceToken) and
        TdxSpreadSheetFormulaReferenceTokenAccess(ATempResult.LastItem)
        .IsValid);
    finally
      ATempResult.Free;
    end;
  end
  else
    Sender.AddValue(False);
end;

procedure fnIsText(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractParameter(AParameter, AParams) then
    Sender.AddValue(dxIsText(AParameter))
  else
    SetSenderResult(Sender, False);
end;

procedure fnN(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if not Sender.ExtractParameter(AParameter, AParams) then
    Exit;
  if VarIsNumeric(AParameter) or dxIsDateTime(AParameter) then
  begin
    Sender.ExtractNumericParameter(AParameter, AParams);
    SetSenderResult(Sender, AParameter);
  end
  else if dxIsText(AParameter) then
    SetSenderResult(Sender, 0);
end;

procedure fnNA(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.SetError(ecNA);
end;

procedure fpiIsBlank(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsErr(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsError(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsEven(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsLogical(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsNA(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsNonText(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsNumber(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsOdd(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsRef(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsText(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiN(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiNA(var AParamCount: Integer;
  var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

{ RegisterFunctions }

type
  TdxSpreadSheetFunctionsRepositoryAccess = class
    (TdxSpreadSheetFunctionsRepository);

procedure RegisterFunctions(ARepository
  : TdxSpreadSheetFunctionsRepositoryAccess);
begin
  ARepository.InternalAdd(@sfnCell, nil, nil, frkValue, 125, ftInformation,
    @sfnCellDescription);
  ARepository.InternalAdd(@sfnError_Type, nil, nil, frkValue, 261,
    ftInformation, @sfnError_TypeDescription);
  ARepository.InternalAdd(@sfnInfo, nil, nil, frkValue, 244, ftInformation,
    @sfnInfoDescription);
  ARepository.InternalAdd(@sfnIsBlank, fnIsBlank, fpiIsBlank, frkValue, 129,
    ftInformation, @sfnIsBlankDescription);
  ARepository.InternalAdd(@sfnIsErr, fnIsErr, fpiIsErr, frkValue, 126,
    ftInformation, @sfnIsErrDescription);
  ARepository.InternalAdd(@sfnIsError, fnIsError, fpiIsError, frkValue, 3,
    ftInformation, @sfnIsErrorDescription);
  ARepository.InternalAdd(@sfnIsEven, fnIsEven, fpiIsEven, frkValue, 255,
    ftInformation, @sfnIsEvenDescription);
  ARepository.InternalAdd(@sfnIsFormula, nil, nil, frkValue, 255, ftInformation,
    @sfnIsFormulaDescription);
  ARepository.InternalAdd(@sfnIsLogical, fnIsLogical, fpiIsLogical, frkValue,
    198, ftInformation, @sfnIsLogicalDescription);
  ARepository.InternalAdd(@sfnIsNA, fnIsNA, fpiIsNA, frkValue, 2, ftInformation,
    @sfnIsNADescription);
  ARepository.InternalAdd(@sfnIsNonText, fnIsNonText, fpiIsNonText, frkValue,
    190, ftInformation, @sfnIsNonTextDescription);
  ARepository.InternalAdd(@sfnIsNumber, fnIsNumber, fpiIsNumber, frkValue, 128,
    ftInformation, @sfnIsNumberDescription);
  ARepository.InternalAdd(@sfnIsOdd, fnIsOdd, fpiIsOdd, frkValue, 255,
    ftInformation, @sfnIsOddDescription);
  ARepository.InternalAdd(@sfnIsRef, fnIsRef, fpiIsRef, frkValue, 105,
    ftInformation, @sfnIsRefDescription);
  ARepository.InternalAdd(@sfnIsText, fnIsText, fpiIsText, frkValue, 127,
    ftInformation, @sfnIsTextDescription);
  ARepository.InternalAdd(@sfnN, fnN, fpiN, frkValue, 131, ftInformation,
    @sfnNDescription);
  ARepository.InternalAdd(@sfnNA, fnNA, fpiNA, frkValue, 10, ftInformation,
    @sfnNADescription);
  ARepository.InternalAdd(@sfnSheet, nil, nil, frkValue, 255, ftInformation,
    @sfnSheetDescription, 1);
  ARepository.InternalAdd(@sfnSheets, nil, nil, frkValue, 255, ftInformation,
    @sfnSheetsDescription, 1);
  ARepository.InternalAdd(@sfnType, nil, nil, frkValue, 86, ftInformation,
    @sfnTypeDescription);
end;

initialization

RegisterFunctions(TdxSpreadSheetFunctionsRepositoryAccess
  (dxSpreadSheetFunctionsRepository));

end.
