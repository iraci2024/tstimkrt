{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressDataController }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL }
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

unit cxFilterConsts;

{$I cxVer.inc}

interface

resourcestring
  // base operators
  cxSFilterOperatorEqual = 'Equals';
  cxSFilterOperatorNotEqual = 'Does not equal';
  cxSFilterOperatorLess = 'Is less than';
  cxSFilterOperatorLessEqual = 'Is less than or equal to';
  cxSFilterOperatorGreater = 'Is greater than';
  cxSFilterOperatorGreaterEqual = 'Is greater than or equal to';
  cxSFilterOperatorLike = 'Is like';
  cxSFilterOperatorNotLike = 'Is not like';
  cxSFilterOperatorBetween = 'Is between';
  cxSFilterOperatorNotBetween = 'Is not between';
  cxSFilterOperatorInList = 'Is any of';
  cxSFilterOperatorNotInList = 'Is none of';

  cxSFilterOperatorYesterday = 'Is yesterday';
  cxSFilterOperatorToday = 'Is today';
  cxSFilterOperatorTomorrow = 'Is tomorrow';

  cxSFilterOperatorLast7Days = 'Is last 7 days';
  cxSFilterOperatorLastWeek = 'Is last week';
  cxSFilterOperatorLast14Days = 'Is last 14 days';
  cxSFilterOperatorLastTwoWeeks = 'Is last two weeks';
  cxSFilterOperatorLast30Days = 'Is last 30 days';
  cxSFilterOperatorLastMonth = 'Is last month';
  cxSFilterOperatorLastYear = 'Is last year';
  cxSFilterOperatorPast = 'Is past';

  cxSFilterOperatorThisWeek = 'Is this week';
  cxSFilterOperatorThisMonth = 'Is this month';
  cxSFilterOperatorThisYear = 'Is this year';

  cxSFilterOperatorNext7Days = 'Is next 7 days';
  cxSFilterOperatorNextWeek = 'Is next week';
  cxSFilterOperatorNext14Days = 'Is next 14 days';
  cxSFilterOperatorNextTwoWeeks = 'Is next two weeks';
  cxSFilterOperatorNext30Days = 'Is next 30 days';
  cxSFilterOperatorNextMonth = 'Is next month';
  cxSFilterOperatorNextYear = 'Is next year';
  cxSFilterOperatorFuture = 'Is future';

  cxSFilterAndCaption = 'And';
  cxSFilterOrCaption = 'Or';
  cxSFilterNotCaption = 'Not';
  cxSFilterBlankCaption = 'Blank';

  cxSFilterFromCaption = 'from';
  cxSFilterToCaption = 'to';

  // derived
  cxSFilterOperatorIsNull = 'Is blank';
  cxSFilterOperatorIsNotNull = 'Is not blank';
  cxSFilterOperatorBeginsWith = 'Begins with';
  cxSFilterOperatorDoesNotBeginWith = 'Does not begin with';
  cxSFilterOperatorEndsWith = 'Ends with';
  cxSFilterOperatorDoesNotEndWith = 'Does not end with';
  cxSFilterOperatorContains = 'Contains';
  cxSFilterOperatorDoesNotContain = 'Does not contain';
  // filter listbox's values
  cxSFilterBoxAllCaption = '(All)';
  cxSFilterBoxCustomCaption = '(Custom...)';
  cxSFilterBoxBlanksCaption = '(Blanks)';
  cxSFilterBoxNonBlanksCaption = '(NonBlanks)';

function cxSFilterString(AResString: Pointer): string;

type
  TcxFilterGetResourceStringFunc = function(AResString: Pointer): string;

var
  cxFilterGetResourceStringFunc: TcxFilterGetResourceStringFunc;

implementation

uses
  dxCore;

function cxSFilterString(AResString: Pointer): string;
begin
  if Assigned(cxFilterGetResourceStringFunc) then
    Result := cxFilterGetResourceStringFunc(AResString)
  else
    Result := LoadResString(AResString);
end;

procedure AddExpressDataFilterResourceStringNames
  (AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('cxSFilterOperatorEqual', @cxSFilterOperatorEqual);
  InternalAdd('cxSFilterOperatorNotEqual', @cxSFilterOperatorNotEqual);
  InternalAdd('cxSFilterOperatorLess', @cxSFilterOperatorLess);
  InternalAdd('cxSFilterOperatorLessEqual', @cxSFilterOperatorLessEqual);
  InternalAdd('cxSFilterOperatorGreater', @cxSFilterOperatorGreater);
  InternalAdd('cxSFilterOperatorGreaterEqual', @cxSFilterOperatorGreaterEqual);
  InternalAdd('cxSFilterOperatorLike', @cxSFilterOperatorLike);
  InternalAdd('cxSFilterOperatorNotLike', @cxSFilterOperatorNotLike);
  InternalAdd('cxSFilterOperatorBetween', @cxSFilterOperatorBetween);
  InternalAdd('cxSFilterOperatorNotBetween', @cxSFilterOperatorNotBetween);
  InternalAdd('cxSFilterOperatorInList', @cxSFilterOperatorInList);
  InternalAdd('cxSFilterOperatorNotInList', @cxSFilterOperatorNotInList);
  InternalAdd('cxSFilterOperatorYesterday', @cxSFilterOperatorYesterday);
  InternalAdd('cxSFilterOperatorToday', @cxSFilterOperatorToday);
  InternalAdd('cxSFilterOperatorTomorrow', @cxSFilterOperatorTomorrow);
  InternalAdd('cxSFilterOperatorLast7Days', @cxSFilterOperatorLast7Days);
  InternalAdd('cxSFilterOperatorLastWeek', @cxSFilterOperatorLastWeek);
  InternalAdd('cxSFilterOperatorLast14Days', @cxSFilterOperatorLast14Days);
  InternalAdd('cxSFilterOperatorLastTwoWeeks', @cxSFilterOperatorLastTwoWeeks);
  InternalAdd('cxSFilterOperatorLast30Days', @cxSFilterOperatorLast30Days);
  InternalAdd('cxSFilterOperatorLastMonth', @cxSFilterOperatorLastMonth);
  InternalAdd('cxSFilterOperatorLastYear', @cxSFilterOperatorLastYear);
  InternalAdd('cxSFilterOperatorPast', @cxSFilterOperatorPast);
  InternalAdd('cxSFilterOperatorThisWeek', @cxSFilterOperatorThisWeek);
  InternalAdd('cxSFilterOperatorThisMonth', @cxSFilterOperatorThisMonth);
  InternalAdd('cxSFilterOperatorThisYear', @cxSFilterOperatorThisYear);
  InternalAdd('cxSFilterOperatorNext7Days', @cxSFilterOperatorNext7Days);
  InternalAdd('cxSFilterOperatorNextWeek', @cxSFilterOperatorNextWeek);
  InternalAdd('cxSFilterOperatorNext14Days', @cxSFilterOperatorNext14Days);
  InternalAdd('cxSFilterOperatorNextTwoWeeks', @cxSFilterOperatorNextTwoWeeks);
  InternalAdd('cxSFilterOperatorNext30Days', @cxSFilterOperatorNext30Days);
  InternalAdd('cxSFilterOperatorNextMonth', @cxSFilterOperatorNextMonth);
  InternalAdd('cxSFilterOperatorNextYear', @cxSFilterOperatorNextYear);
  InternalAdd('cxSFilterOperatorFuture', @cxSFilterOperatorFuture);
  InternalAdd('cxSFilterAndCaption', @cxSFilterAndCaption);
  InternalAdd('cxSFilterOrCaption', @cxSFilterOrCaption);
  InternalAdd('cxSFilterNotCaption', @cxSFilterNotCaption);
  InternalAdd('cxSFilterBlankCaption', @cxSFilterBlankCaption);
  InternalAdd('cxSFilterFromCaption', @cxSFilterFromCaption);
  InternalAdd('cxSFilterToCaption', @cxSFilterToCaption);
  InternalAdd('cxSFilterOperatorIsNull', @cxSFilterOperatorIsNull);
  InternalAdd('cxSFilterOperatorIsNotNull', @cxSFilterOperatorIsNotNull);
  InternalAdd('cxSFilterOperatorBeginsWith', @cxSFilterOperatorBeginsWith);
  InternalAdd('cxSFilterOperatorDoesNotBeginWith',
    @cxSFilterOperatorDoesNotBeginWith);
  InternalAdd('cxSFilterOperatorEndsWith', @cxSFilterOperatorEndsWith);
  InternalAdd('cxSFilterOperatorDoesNotEndWith',
    @cxSFilterOperatorDoesNotEndWith);
  InternalAdd('cxSFilterOperatorContains', @cxSFilterOperatorContains);
  InternalAdd('cxSFilterOperatorDoesNotContain',
    @cxSFilterOperatorDoesNotContain);
  InternalAdd('cxSFilterBoxAllCaption', @cxSFilterBoxAllCaption);
  InternalAdd('cxSFilterBoxCustomCaption', @cxSFilterBoxCustomCaption);
  InternalAdd('cxSFilterBoxBlanksCaption', @cxSFilterBoxBlanksCaption);
  InternalAdd('cxSFilterBoxNonBlanksCaption', @cxSFilterBoxNonBlanksCaption);
end;

initialization

dxResourceStringsRepository.RegisterProduct('ExpressDataFilter',
  @AddExpressDataFilterResourceStringNames);

finalization

dxResourceStringsRepository.UnRegisterProduct('ExpressDataFilter');

end.
