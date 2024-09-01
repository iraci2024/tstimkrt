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

unit dxSpreadSheetCoreFormulasTokens;

{$I cxVer.Inc}

interface

uses
  Windows, Types, SysUtils, Classes, Variants, Math,
  // CX
  cxClasses,
  cxGeometry,
  cxVariants,
  dxCoreClasses,
  // SpreadSheet
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreReferences,
  dxSpreadSheetFunctions,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

type
  TdxSpreadSheetFormulaArrayToken = class;

{$REGION 'Simple Tokens'}
  { TdxSpreadSheetFormulaNullToken }

  TdxSpreadSheetFormulaNullToken = class(TdxSpreadSheetFormulaToken)
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(AStack: TStringList); override;
  public
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
  end;

  { TdxSpreadSheetFormulaStringValueToken }

  TdxSpreadSheetFormulaStringValueToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FValue: string;
  protected
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(const AValue: string); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: string read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaTextValueToken }

  TdxSpreadSheetFormulaTextValueToken = class
    (TdxSpreadSheetFormulaStringValueToken)
  protected
    procedure ToString(AStack: TStringList); override;
  end;

  { TdxSpreadSheetFormulaVariantToken }

  TdxSpreadSheetFormulaVariantToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FValue: Variant;
  public
    constructor Create(const AValue: Variant); virtual;
    procedure ConvertNullValueToZero;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Variant read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaBooleanValueToken }

  TdxSpreadSheetFormulaBooleanValueToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FValue: Boolean;
  protected
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(AValue: Boolean); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Boolean read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaNumericValueToken }

  TdxSpreadSheetFormulaNumericValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    function CombineWithSign(ASign: TValueSign): Boolean; virtual; abstract;
  public
    function CombineWithUnaryOperation(AOperation
      : TdxSpreadSheetFormulaOperation): Boolean;
  end;

  { TdxSpreadSheetFormulaIntegerValueToken }

  TdxSpreadSheetFormulaIntegerValueToken = class
    (TdxSpreadSheetFormulaNumericValueToken)
  strict private
    FValue: Integer;
  protected
    function CombineWithSign(ASign: TValueSign): Boolean; override;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(AValue: Integer); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Integer read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaFloatValueToken }

  TdxSpreadSheetFormulaFloatValueToken = class
    (TdxSpreadSheetFormulaNumericValueToken)
  strict private
    FValue: Double;
  protected
    function CombineWithSign(ASign: TValueSign): Boolean; override;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(const AValue: Double); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Double read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaCurrencyValueToken }

  TdxSpreadSheetFormulaCurrencyValueToken = class
    (TdxSpreadSheetFormulaNumericValueToken)
  strict private
    FValue: Currency;
  protected
    function CombineWithSign(ASign: TValueSign): Boolean; override;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(AValue: Currency); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Currency read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaDateTimeValueToken }

  TdxSpreadSheetFormulaDateTimeValueToken = class
    (TdxSpreadSheetFormulaFloatValueToken)
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(AStack: TStringList); override;
  public
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
  end;

{$ENDREGION}
  { TdxSpreadSheetFormulaOperationToken }

  TdxSpreadSheetFormulaOperationToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FOperation: TdxSpreadSheetFormulaOperation;

    function CalculateAsArray(var AResult: TdxSpreadSheetFormulaResult;
      ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
      ADimension: TdxSpreadSheetFormulaTokenDimension)
      : TdxSpreadSheetFormulaArrayToken;
    procedure ExcludeResultValue(var AResult: TdxSpreadSheetFormulaResult;
      const ANumAtLast: Integer);
    procedure ExtractIterationOperands(var AResult: TdxSpreadSheetFormulaResult;
      const ACalculateAsArray: Boolean;
      ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
      const ARowIndex, AColumnIndex: Integer; var ALeft, ARight: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode);
    function GetElementaryResult(AResult: TdxSpreadSheetFormulaResult;
      const ALeftValue, ARightValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant; inline;
    function IsMustBeLeftValue: Boolean;
    procedure ResetResultErrorCode(var AResult: TdxSpreadSheetFormulaResult;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure CalculateReferences(AResult: TdxSpreadSheetFormulaResult);
    function ExtractReference(AResult: TdxSpreadSheetFormulaResult;
      var AView: TObject; var AArea: TRect): Boolean;
    function GetTokenPriority: Integer; override;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(AOperation: TdxSpreadSheetFormulaOperation); virtual;
    function CheckNeighbors: Boolean;
    //
    property Operation: TdxSpreadSheetFormulaOperation read FOperation;
  end;

  { TdxSpreadSheetFormulaErrorValueToken }

  TdxSpreadSheetFormulaErrorValueToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FErrorCode: TdxSpreadSheetFormulaErrorCode;
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property ErrorCode: TdxSpreadSheetFormulaErrorCode read FErrorCode;
  end;

  { TdxSpreadSheetFormulaCustomDefinedNameToken }

  TdxSpreadSheetFormulaCustomDefinedNameToken = class
    (TdxSpreadSheetFormulaStringValueToken)
  public
    function CanConvertStrToNumber: Boolean; override;
  end;

{$REGION 'Array Formula Tokens'}
  { TdxSpreadSheetListToken }

  TdxSpreadSheetListToken = class(TdxSpreadSheetFormulaToken)
  protected
    procedure ParametersToString(ABuffer: TStringBuilder); virtual;
    procedure ToString(AStack: TStringList); override;
  public
    function IsEnumeration: Boolean; override;
  end;

  { TdxSpreadSheetFormulaArrayToken }

  TdxSpreadSheetFormulaArrayToken = class(TdxSpreadSheetListToken)
  strict private
    FIndex: array of TdxSpreadSheetFormulaToken;
    FSize: TSize;
  protected
    procedure CalculateDimension(var ADimension
      : TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure CalculateIndex;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function MakeIndex(ARowIndex, AColumnIndex: Integer): Integer; inline;
    procedure ToString(AStack: TStringList); override;
  public
    function CanConvertStrToNumber: Boolean; override;
    function ExtractColumnAsArray(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetFormulaArrayToken; virtual;
    function ExtractColumn(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    function ExtractRowAsArray(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetFormulaArrayToken; virtual;
    function GetArray(var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure GetValueAsArrayItem(const ARow, AColumn: Integer;
      var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    property Size: TSize read FSize;
  end;

  { TdxSpreadSheetFormulaArrayRowSeparator }

  TdxSpreadSheetFormulaArrayRowSeparator = class(TdxSpreadSheetFormulaToken);

{$ENDREGION}
{$REGION 'Reference Tokens'}
  { TdxSpreadSheetFormulaReferenceToken }

  TdxSpreadSheetFormulaReferenceToken = class(TdxSpreadSheetFormulaToken)
  strict private
    function GetAbsoluteColumn: Boolean;
    function GetAbsoluteRow: Boolean;
    function GetActualColumn: Integer; inline;
    function GetActualRow: Integer; inline;
    function GetAnchorColumn: Integer; inline;
    function GetAnchorRow: Integer; inline;
    function GetIsError: Boolean;
    function GetR1C1Reference: Boolean;
    procedure SetIsError(AValue: Boolean);
  protected
    FColumn: TdxSpreadSheetReference;
    FRow: TdxSpreadSheetReference;

    function ExtractReference(var AView: TObject; var AArea: TRect)
      : Boolean; virtual;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function IsValid: Boolean; virtual;
    procedure Offset(DY, DX: Integer); override;

    // To String
    function ReferenceToString: string; virtual;
    procedure ToString(AStack: TStringList); override;

    procedure UpdateReferences(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint;
      AMode: TdxSpreadSheetFormulaUpdateReferencesMode;
      var AModified: Boolean); override;
    procedure UpdateReferencesCore(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint;
      AMode: TdxSpreadSheetFormulaUpdateReferencesMode;
      var AModified: Boolean); virtual;

    property R1C1Reference: Boolean read GetR1C1Reference;
  public
    constructor Create(const ARow, AColumn: TdxSpreadSheetReference);
    function CanConvertStrToNumber: Boolean; override;
    procedure EnumReferences
      (AProc: TdxSpreadSheetFormulaEnumReferenceTokensProc;
      AProcessDefinedNames: Boolean = False); override;
    function ExtractVector(ALength: Integer; AIsVertical: Boolean)
      : TdxSpreadSheetVector;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property AbsoluteColumn: Boolean read GetAbsoluteColumn;
    property AbsoluteRow: Boolean read GetAbsoluteRow;
    property ActualColumn: Integer read GetActualColumn;
    property ActualRow: Integer read GetActualRow;
    property AnchorColumn: Integer read GetAnchorColumn;
    property AnchorRow: Integer read GetAnchorRow;
    property IsError: Boolean read GetIsError write SetIsError;

    property Column: TdxSpreadSheetReference read FColumn;
    property Row: TdxSpreadSheetReference read FRow;
  end;

  { TdxSpreadSheetFormulaAreaReferenceToken }

  TdxSpreadSheetFormulaAreaReferenceToken = class
    (TdxSpreadSheetFormulaReferenceToken)
  strict private
    procedure CalculateActualRowAndColumnIndexes(var AActualRow, AActualRow2,
      AActualColumn, AActualColumn2: Integer); inline;
    procedure ExchangeReferences(var ARef, ARef2: TdxSpreadSheetReference);
    function GetAbsoluteColumn2: Boolean;
    function GetAbsoluteRow2: Boolean;
    function GetActualColumn2: Integer;
    function GetActualRow2: Integer;
  protected
    FColumn2: TdxSpreadSheetReference;
    FRow2: TdxSpreadSheetReference;

    procedure CalculateDimension(var ADimension
      : TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    function ExtractReference(var AView: TObject; var AArea: TRect)
      : Boolean; override;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function IsValid: Boolean; override;
    procedure Offset(DY, DX: Integer); override;
    function ReferenceToString: string; override;
    procedure UpdateReferencesCore(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint;
      AMode: TdxSpreadSheetFormulaUpdateReferencesMode;
      var AModified: Boolean); override;
  public
    constructor Create(const ARow, AColumn, ARow2,
      AColumn2: TdxSpreadSheetReference);
    procedure Check;
    function ExtractColumn(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetVector; override;
    function GetValueAsText(out AText: string): Boolean; override;
    procedure GetValueAsArrayItem(const ARow, AColumn: Integer;
      var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure GetValueRelatedWithCell(const ACell: TPoint; var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    function IsEnumeration: Boolean; override;

    property AbsoluteColumn2: Boolean read GetAbsoluteColumn2;
    property AbsoluteRow2: Boolean read GetAbsoluteRow2;
    property ActualColumn2: Integer read GetActualColumn2;
    property ActualRow2: Integer read GetActualRow2;
  end;

  { TdxSpreadSheetFormula3DReferenceToken }

  TdxSpreadSheetFormula3DReferenceToken = class
    (TdxSpreadSheetFormulaReferenceToken)
  strict private
    FLink: TdxSpreadSheet3DReferenceCustomLink;
  protected
    function ExtractReference(var AView: TObject; var AArea: TRect)
      : Boolean; override;
    function GetView: TObject; override;
    function IsValid: Boolean; override;
    function ReferenceToString: string; override;
  public
    constructor Create(ALink: TdxSpreadSheet3DReferenceCustomLink;
      const ARow, AColumn: TdxSpreadSheetReference);
    destructor Destroy; override;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink;
  end;

  { TdxSpreadSheet3DReferenceLink }

  TdxSpreadSheet3DReferenceLink = class(TdxSpreadSheet3DReferenceCustomLink)
  public
    function ToString: string; override;
  end;

  { TdxSpreadSheet3DReferenceAutoLink }

  TdxSpreadSheet3DReferenceAutoLink = class(TdxSpreadSheet3DReferenceLink);

  { TdxSpreadSheetFormula3DAreaReferenceToken }

  TdxSpreadSheetFormula3DAreaReferenceToken = class
    (TdxSpreadSheetFormulaAreaReferenceToken)
  protected
    FLink: TdxSpreadSheet3DReferenceCustomLink;
    FLink2: TdxSpreadSheet3DReferenceCustomLink;

    function ExtractReference(var AView: TObject; var AArea: TRect)
      : Boolean; override;
    function GetView: TObject; override;
    function IsValid: Boolean; override;
    function ReferenceToString: string; override;
  public
    constructor Create(ALink, ALink2: TdxSpreadSheet3DReferenceCustomLink;
      const ARow, AColumn, ARow2, AColumn2: TdxSpreadSheetReference);
    destructor Destroy; override;

    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink;
    property Link2: TdxSpreadSheet3DReferenceCustomLink read FLink2;
    property AbsoluteColumn;
    property AbsoluteColumn2;
    property AbsoluteRow;
    property AbsoluteRow2;
    property ActualColumn;
    property ActualColumn2;
    property ActualRow;
    property ActualRow2;
  end;

{$ENDREGION}
{$REGION 'Functions'}
  { TdxSpreadSheetFormulaFunctionToken }

  TdxSpreadSheetFunctionFakeParams = array of TdxSpreadSheetFormulaToken;

  TdxSpreadSheetFormulaFunctionToken = class(TdxSpreadSheetListToken)
  strict private
    FCachedResultErrorCode: TdxSpreadSheetFormulaErrorCode;
    FCachedResultValue: Variant;
    FCalculatedDataList: TcxObjectList;
    FChildrenOrder: TList;
    FFakeParams: TdxSpreadSheetFunctionFakeParams;
    FFirstChildParent: TdxSpreadSheetFormulaToken;
    FFirstFakeToken: TdxSpreadSheetFormulaToken;
    FInformation: TdxSpreadSheetFunctionInfo;
    FIsCachedResultPresent: Boolean;
    FIsDirtyParamInfo: Boolean;
    FMaxParamCount: Integer;
    FParamKind: TdxSpreadSheetFunctionParamKindInfo;

    procedure CalculateAsArray(AResult: TdxSpreadSheetFormulaResult);
    procedure ClearTemporaryData;
    function CreateArrayCopy(const AArray: TdxSpreadSheetFormulaToken)
      : TdxSpreadSheetFormulaToken;
    function CreateErrorToken(const AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetFormulaToken;
    function CreateFakeToken(AParam: TdxSpreadSheetFormulaToken;
      const AIndex: Integer;
      var ADimension: TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetFormulaToken;
    procedure DestroyFakeTokensChildren(var AResult
      : TdxSpreadSheetFormulaResult);
    function HasArrayArgument: Boolean;
    procedure InitializeFakeParams;
    function IsArrayInsteadValue(const AIndex: Integer;
      AParam: TdxSpreadSheetFormulaToken; ACheckedClass: TClass): Boolean;
    function IsExpectedValueParam(AIndex: Integer): Boolean;
    function GetCalculatedDataList: TcxObjectList;
    function GetFakeToken(AIndex: Integer; AParam: TdxSpreadSheetFormulaToken;
      var ADimension: TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode)
      : TdxSpreadSheetFormulaToken;
    function GetMaxParamCount: Integer;
    function GetParamKind: TdxSpreadSheetFunctionParamKindInfo;
    procedure PopulateFakeTokensByChildren(const ARow, AColumn: Integer);
    procedure RestoreChildrenOrder;
    procedure SpecifyMaxParamCount;
    procedure StoreChildrenOrder;
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure CalculateDimension(var ADimension
      : TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure CleanFunctionTokenCachedResult; override;
    procedure DoCacheResult(AResult: TdxSpreadSheetFormulaResult);
    procedure DoInformationProc(var AResult: TdxSpreadSheetFormulaResult);
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function GetCachedValue(var AResult: TdxSpreadSheetFormulaResult): Boolean;
    function NeedAddFeatureFunctionPrefixToFunctionName: Boolean;
    function NeedForceDimensionCalculating: Boolean; override;
    procedure ToString(AStack: TStringList); override;

    property CalculatedDataList: TcxObjectList read GetCalculatedDataList;
  public
    constructor Create(AInformation: TdxSpreadSheetFunctionInfo); virtual;
    destructor Destroy; override;
    procedure InitializeParamInfo;
    function IsEnumeration: Boolean; override;

    property Information: TdxSpreadSheetFunctionInfo read FInformation;
    property MaxParamCount: Integer read GetMaxParamCount;
    property ParamKind: TdxSpreadSheetFunctionParamKindInfo read GetParamKind;
  end;

{$ENDREGION}
  { TdxSpreadSheetFormulaParenthesesToken }

  TdxSpreadSheetFormulaParenthesesToken = class(TdxSpreadSheetFormulaToken)
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(AStack: TStringList); override;
  end;

{$REGION 'Unknown Tokens'}
  { TdxSpreadSheetFormulaUnknownToken }

  TdxSpreadSheetFormulaUnknownToken = class(TdxSpreadSheetListToken)
  strict private
    FLink: TdxSpreadSheet3DReferenceCustomLink;
    FName: string;

    procedure SetLink(const Value: TdxSpreadSheet3DReferenceCustomLink);
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    function LinkAsString: string;
    procedure ToString(AStack: TStringList); override;
  public
    constructor Create(const AName: string;
      ALink: TdxSpreadSheet3DReferenceCustomLink = nil); virtual;
    destructor Destroy; override;
    function IsEnumeration: Boolean; override;

    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink write SetLink;
    property Name: string read FName;
  end;

  { TdxSpreadSheetFormulaUnknownFunctionToken }

  TdxSpreadSheetFormulaUnknownFunctionToken = class
    (TdxSpreadSheetFormulaUnknownToken)
  protected
    procedure ToString(AStack: TStringList); override;
  end;

  { TdxSpreadSheetFormulaUnknownNameToken }

  TdxSpreadSheetFormulaUnknownNameToken = class
    (TdxSpreadSheetFormulaUnknownToken);

{$ENDREGION}
  // backward compatibility
  TdxSpreadSheetFormula3DAreaReference =
    TdxSpreadSheetFormula3DAreaReferenceToken;
  TdxSpreadSheetFormula3DReference = TdxSpreadSheetFormula3DReferenceToken;
  TdxSpreadSheetFormulaAreaReference = TdxSpreadSheetFormulaAreaReferenceToken;
  TdxSpreadSheetFormulaReference = TdxSpreadSheetFormulaReferenceToken;

procedure dxSpreadSheetAddReferenceToken(AResult: TdxSpreadSheetFormulaResult;
  AView: TObject; const R: TRect);
function dxSpreadSheetCreateResultToken(const AResultValue: Variant;
  const AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetFormulaToken; inline;

implementation

uses
  dxSpreadSheetCoreStrs, dxStringHelper;

type
  TControllerAccess = class(TdxSpreadSheetCustomFormulaController);
  TFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TResultAccess = class(TdxSpreadSheetFormulaResult);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);

  { Helpers }

procedure dxSpreadSheetAddReferenceToken(AResult: TdxSpreadSheetFormulaResult;
  AView: TObject; const R: TRect);
var
  AReference: TdxSpreadSheetFormulaReferenceToken;
begin
  if (R.Top = R.Bottom) and (R.Left = R.Right) then
  begin
    AReference := TdxSpreadSheetFormula3DReferenceToken.Create
      (TdxSpreadSheet3DReferenceLink.Create(AView),
      TdxSpreadSheetReference.Create(R.Top, -1, dxSpreadSheetMaxRowIndex),
      TdxSpreadSheetReference.Create(R.Left, -1, dxSpreadSheetMaxColumnIndex));
  end
  else
  begin
    AReference := TdxSpreadSheetFormula3DAreaReferenceToken.Create
      (TdxSpreadSheet3DReferenceLink.Create(AView),
      TdxSpreadSheet3DReferenceLink.Create(AView),
      TdxSpreadSheetReference.Create(R.Top, -1, dxSpreadSheetMaxRowIndex),
      TdxSpreadSheetReference.Create(R.Left, -1, dxSpreadSheetMaxColumnIndex),
      TdxSpreadSheetReference.Create(R.Bottom, -1, dxSpreadSheetMaxRowIndex),
      TdxSpreadSheetReference.Create(R.Right, -1, dxSpreadSheetMaxColumnIndex));
  end;
  AResult.AddTemporary(AReference);
end;

function dxSpreadSheetCreateResultToken(const AResultValue: Variant;
  const AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
begin
  if AErrorCode = ecNone then
    Result := TdxSpreadSheetFormulaVariantToken.Create(AResultValue)
  else
    Result := TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode);
end;

{$REGION 'Simple Tokens'}
{ TdxSpreadSheetFormulaEmptyToken }

procedure TdxSpreadSheetFormulaNullToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.AddValue(Null)
end;

procedure TdxSpreadSheetFormulaNullToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := 0;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaNullToken.ToString(AStack: TStringList);
begin
  AStack.Add('');
end;

{ TdxSpreadSheetFormulaStringValueToken }

constructor TdxSpreadSheetFormulaStringValueToken.Create(const AValue: string);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaStringValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AErrorCode := ecNone;
  AValue := FValue;
end;

procedure TdxSpreadSheetFormulaStringValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(dxSpreadSheetTextValueAsString(Value));
end;

{ TdxSpreadSheetFormulaTextValueToken }

procedure TdxSpreadSheetFormulaTextValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(Value);
end;

{ TdxSpreadSheetFormulaVariantToken }

constructor TdxSpreadSheetFormulaVariantToken.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaVariantToken.ConvertNullValueToZero;
begin
  if dxSpreadSheetIsNullValue(FValue) then
    FValue := 0;
end;

procedure TdxSpreadSheetFormulaVariantToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

{ TdxSpreadSheetFormulaBooleanValueToken }

constructor TdxSpreadSheetFormulaBooleanValueToken.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaBooleanValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaBooleanValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(dxBoolToString[FValue]);
end;

{ TdxSpreadSheetFormulaNumericValueToken }

function TdxSpreadSheetFormulaNumericValueToken.CombineWithUnaryOperation
  (AOperation: TdxSpreadSheetFormulaOperation): Boolean;
begin
  case AOperation of
    opUminus:
      Result := CombineWithSign(-1);
    opUplus:
      Result := CombineWithSign(1);
  else
    Result := False;
  end;
end;

{ TdxSpreadSheetFormulaIntegerValueToken }

constructor TdxSpreadSheetFormulaIntegerValueToken.Create(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaIntegerValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

function TdxSpreadSheetFormulaIntegerValueToken.CombineWithSign
  (ASign: TValueSign): Boolean;
begin
  Result := Sign(Value) <> ASign;
  if Result then
    Value := -Value;
end;

procedure TdxSpreadSheetFormulaIntegerValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(IntToStr(FValue));
end;

{ TdxSpreadSheetFormulaFloatValueToken }

constructor TdxSpreadSheetFormulaFloatValueToken.Create(const AValue: Double);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaFloatValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

function TdxSpreadSheetFormulaFloatValueToken.CombineWithSign
  (ASign: TValueSign): Boolean;
begin
  Result := Sign(Value) <> ASign;
  if Result then
    Value := -Value;
end;

procedure TdxSpreadSheetFormulaFloatValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(dxFloatToStr(FValue, FormatSettings.Data));
end;

{ TdxSpreadSheetFormulaCurrencyValueToken }

constructor TdxSpreadSheetFormulaCurrencyValueToken.Create(AValue: Currency);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaCurrencyValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

function TdxSpreadSheetFormulaCurrencyValueToken.CombineWithSign
  (ASign: TValueSign): Boolean;
begin
  Result := Sign(Value) <> ASign;
  if Result then
    Value := -Value;
end;

procedure TdxSpreadSheetFormulaCurrencyValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(dxFloatToStr(FValue, FormatSettings.Data));
end;

{ TdxSpreadSheetFormulaDateTimeValueToken }

procedure TdxSpreadSheetFormulaDateTimeValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := TDateTime(Value);
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaDateTimeValueToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.AddValue(TDateTime(Value));
end;

procedure TdxSpreadSheetFormulaDateTimeValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(DateTimeToStr(Value));
end;
{$ENDREGION}
{ TdxSpreadSheetFormulaOperationToken }

constructor TdxSpreadSheetFormulaOperationToken.Create
  (AOperation: TdxSpreadSheetFormulaOperation);
begin
  FOperation := AOperation;
end;

function TdxSpreadSheetFormulaOperationToken.CheckNeighbors: Boolean;
begin
  if (Operation = opPercent) and (Prev = nil) then
    Exit(False);

  if Operation = opSub then
  begin
    if (Prev = nil) or (Prev.Priority >= 1) or
      (Prev is TdxSpreadSheetFormulaOperationToken) and
      (TdxSpreadSheetFormulaOperationToken(Prev).Operation = opUminus) then
      FOperation := opUminus;
  end;

  if Operation = opAdd then
  begin
    if (Prev = nil) or (Prev.Priority >= 1) then
      FOperation := opUplus;
  end;

  Result := True;
end;

procedure TdxSpreadSheetFormulaOperationToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);

  function PrepareToken(AIndex: Integer; var AToken: TdxSpreadSheetFormulaToken;
    var ADimension: TdxSpreadSheetFormulaTokenDimension;
    var ACalculateAsArray: Boolean;
    var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
  begin
    if InRange(AIndex, 0, AResult.Count - 1) then
    begin
      AToken := AResult.Items[AIndex];
      ACalculateAsArray := ACalculateAsArray or
        (AToken is TdxSpreadSheetFormulaArrayToken);
      if ACalculateAsArray then
        ADimension := AToken.GetDimension(AErrorCode)
      else
        ADimension.SetDimension(1, 1);
    end
    else
    begin
      AToken := nil;
      ADimension.SetDimension(0, 0);
    end;
    Result := AErrorCode = ecNone;
  end;

  function PrepareCalculation(var ALeftToken, ARightToken
    : TdxSpreadSheetFormulaToken;
    var AResultDimension: TdxSpreadSheetFormulaTokenDimension;
    var ACalculateAsArray: Boolean): TdxSpreadSheetFormulaErrorCode;
  var
    ALeftDimension: TdxSpreadSheetFormulaTokenDimension;
    ARightDimension: TdxSpreadSheetFormulaTokenDimension;
  begin
    Result := ecNone;
    ACalculateAsArray := Owner.IsArrayFormula or TFormulaAccess(Owner)
      .NeedCalculateParamsAsArray;
    if not PrepareToken(AResult.Count - 1, ARightToken, ARightDimension,
      ACalculateAsArray, Result) then
      Exit;
    if not PrepareToken(AResult.Count - 2, ALeftToken, ALeftDimension,
      ACalculateAsArray, Result) then
      Exit;
    if (ALeftToken = nil) and (ARightToken = nil) then
      Exit(ecValue);
    if ACalculateAsArray then
    begin
      AResultDimension.SetDimension(Max(ALeftDimension.RowCount,
        ARightDimension.RowCount), Max(ALeftDimension.ColumnCount,
        ARightDimension.ColumnCount))
    end
    else
      AResultDimension.SetDimension(1, 1);
  end;

var
  ACalculateAsArray: Boolean;
  AChildResult: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ALeft, ARight: Variant;
  ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
begin
  if Operation in [opIsect, opUnion, opRange] then
  begin
    CalculateReferences(AResult);
    Exit;
  end;

  AErrorCode := PrepareCalculation(ALeftToken, ARightToken, ADimension,
    ACalculateAsArray);
  if AErrorCode <> ecNone then
  begin
    AResult.SetError(AErrorCode);
    Exit;
  end;

  if ACalculateAsArray then
    AChildResult := CalculateAsArray(AResult, ALeftToken, ARightToken,
      ADimension)
  else
  begin
    ExtractIterationOperands(AResult, ACalculateAsArray, ALeftToken,
      ARightToken, 0, 0, ALeft, ARight, AErrorCode);
    if (Operation = opUplus) and (ALeftToken = nil) then
    begin
      TTokenAccess(ARightToken).Calculate(AResult);
      AChildResult := AResult.Items[AResult.Count - 1];
    end
    else
    begin
      AChildResult := dxSpreadSheetCreateResultToken
        (GetElementaryResult(AResult, ALeft, ARight, AErrorCode), AErrorCode);
      AResult.AddTemporary(AChildResult);
    end;
  end;

  if AChildResult is TdxSpreadSheetFormulaErrorValueToken then
    AResult.SetError(TdxSpreadSheetFormulaErrorValueToken(AChildResult)
      .ErrorCode);
end;

function TdxSpreadSheetFormulaOperationToken.CalculateAsArray
  (var AResult: TdxSpreadSheetFormulaResult;
  ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension)
  : TdxSpreadSheetFormulaArrayToken;
var
  AArrayToken, ANewToken: TdxSpreadSheetFormulaToken;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ALeft, ARight, AIterationResult: Variant;
  ARepeatingWhenIsDirty: Boolean;
  ARowIndex, AColumnIndex: Integer;
begin
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  AResult.AddTemporary(Result);
  AArrayToken := nil;
  ARepeatingWhenIsDirty := False;
  repeat
    TTokenAccess(Result).IsDirty := False;
    for ARowIndex := 0 to ADimension.RowCount - 1 do
      for AColumnIndex := 0 to ADimension.ColumnCount - 1 do
      begin
        if (AColumnIndex = 0) and (ARowIndex > 0) then
          if not ARepeatingWhenIsDirty then
            TdxSpreadSheetFormulaToken.AddChild(Result,
              TdxSpreadSheetFormulaArrayRowSeparator.Create)
          else
            AArrayToken := AArrayToken.Next;

        ExtractIterationOperands(AResult, True, ALeftToken, ARightToken,
          ARowIndex, AColumnIndex, ALeft, ARight, AErrorCode);
        AIterationResult := GetElementaryResult(AResult, ALeft, ARight,
          AErrorCode);
        if ARepeatingWhenIsDirty then
        begin
          ANewToken := AArrayToken;
          ANewToken.FirstChild.Free;
        end
        else
          ANewToken := TdxSpreadSheetFormulaToken.Create;

        TdxSpreadSheetFormulaToken.AddChild(ANewToken,
          dxSpreadSheetCreateResultToken(AIterationResult, AErrorCode));
        if not ARepeatingWhenIsDirty then
          TdxSpreadSheetFormulaToken.AddChild(Result, ANewToken);
        AErrorCode := ecNone;

        if ARepeatingWhenIsDirty then
          AArrayToken := AArrayToken.Next;
      end;
    TTokenAccess(Result).ClearIsDimensionCalculated;
    ARepeatingWhenIsDirty := not ARepeatingWhenIsDirty and
      TTokenAccess(Result).IsDirty;
    if ARepeatingWhenIsDirty then
      AArrayToken := Result.FirstChild;
  until not ARepeatingWhenIsDirty;

  ExcludeResultValue(AResult, 2);
  if IsMustBeLeftValue then
    ExcludeResultValue(AResult, 2);
end;

procedure TdxSpreadSheetFormulaOperationToken.CalculateReferences
  (AResult: TdxSpreadSheetFormulaResult);
var
  R1, R2: TRect;
  ASheet1, ASheet2: TObject;
begin
  if ExtractReference(AResult, ASheet2, R2) and
    ExtractReference(AResult, ASheet1, R1) then
  begin
    if ASheet1 = nil then
      ASheet1 := ASheet2;
    if ASheet1 = nil then
      ASheet1 := AResult.Owner.View;

    if Operation = opIsect then
    begin
      if not dxSpreadSheetIntersects(R1, R2, R1) then
        AResult.SetError(ecNull);
    end
    else if Operation = opUnion then
      R1 := dxSpreadSheetCellsUnion(R1, R2)
    else
      R1.BottomRight := R2.BottomRight;

    dxSpreadSheetAddReferenceToken(AResult, ASheet1, R1);
  end;
end;

procedure TdxSpreadSheetFormulaOperationToken.ExcludeResultValue
  (var AResult: TdxSpreadSheetFormulaResult; const ANumAtLast: Integer);
var
  ACount: Integer;
begin
  ACount := TResultAccess(AResult).Count;
  if ACount > 0 then
    TResultAccess(AResult).Delete(ACount - ANumAtLast);
end;

function TdxSpreadSheetFormulaOperationToken.ExtractReference
  (AResult: TdxSpreadSheetFormulaResult; var AView: TObject;
  var AArea: TRect): Boolean;
var
  AOwnership: TStreamOwnership;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := AResult.ErrorCode = ecNone;
  if Result then
  begin
    AToken := AResult.ExtractValueToken(AOwnership);
    try
      Result := AToken is TdxSpreadSheetFormulaReferenceToken;
      if not Result or not TdxSpreadSheetFormulaReferenceToken(AToken)
        .ExtractReference(AView, AArea) then
        AResult.SetError(ecRefErr);
      AArea := cxRectAdjust(AArea);
    finally
      if AOwnership = soOwned then
        AToken.Free;
    end;
  end;
end;

procedure TdxSpreadSheetFormulaOperationToken.ExtractIterationOperands
  (var AResult: TdxSpreadSheetFormulaResult; const ACalculateAsArray: Boolean;
  ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
  const ARowIndex, AColumnIndex: Integer; var ALeft, ARight: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);

  procedure InternalGetValue(AToken: TdxSpreadSheetFormulaToken;
    var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  var
    ACanConvertStrToNumber: Boolean;
  begin
    if AToken is TdxSpreadSheetFormulaAreaReferenceToken then
    begin
      AToken.GetValueRelatedWithCell(Owner.Anchor, AValue, AErrorCode);
      ExcludeResultValue(AResult, 1);
    end
    else if AToken is TdxSpreadSheetFormulaReferenceToken then
    begin
      AToken.GetValue(AValue, AErrorCode);
      ExcludeResultValue(AResult, 1);
    end
    else
    begin
      AValue := AResult.ExtractValue(ACanConvertStrToNumber);
      ResetResultErrorCode(AResult, AErrorCode);
    end;
  end;

begin
  ALeft := Null;
  if ACalculateAsArray then
  begin
    ARight := ARightToken.GetValueFromArray(ARowIndex, AColumnIndex,
      AErrorCode);
    if (AErrorCode = ecNone) and IsMustBeLeftValue then
      ALeft := ALeftToken.GetValueFromArray(ARowIndex, AColumnIndex,
        AErrorCode);
  end
  else
  begin
    InternalGetValue(ARightToken, ARight, AErrorCode);
    if IsMustBeLeftValue then
      if AErrorCode = ecNone then
        InternalGetValue(ALeftToken, ALeft, AErrorCode)
      else
        ExcludeResultValue(AResult, 1);
  end;
  if (AErrorCode = ecNone) and (Operation = opConcat) then
  begin
    ARight := VarToStr(ARight);
    ALeft := VarToStr(ALeft);
  end;
end;

function TdxSpreadSheetFormulaOperationToken.GetElementaryResult
  (AResult: TdxSpreadSheetFormulaResult; const ALeftValue, ARightValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
var
  ALeft, ARight: Variant;
begin
  Result := Null;
  ALeft := ALeftValue;
  ARight := ARightValue;
  if AErrorCode <> ecNone then
    Exit;

  if Operation in [opAdd, opSub, opMul] then
  begin
    if dxSpreadSheetIsNullValue(ALeft) and dxSpreadSheetIsNullValue(ARight) then
      Exit(0);
  end;

  if Operation in [opAdd, opSub, opMul, opDiv, opPower, opUplus, opUminus] then
  begin
    if not(AResult.ConvertToNumeric(ALeft, True, False) and
      AResult.ConvertToNumeric(ARight, True, False)) then
    begin
      AErrorCode := ecValue;
      Exit;
    end;
  end;

  AErrorCode := ecNone;
  case FOperation of
    opAdd, opConcat:
      Result := ALeft + ARight;
    opSub:
      Result := ALeft - ARight;
    opMul:
      Result := ALeft * ARight;
    opDiv:
      try
        if Abs(ARight) <= MinDouble then
          AErrorCode := ecDivByZero
        else
          Result := Extended(ALeft) / Extended(ARight);
      except
        AErrorCode := ecDivByZero;
      end;

    opPower:
      if (ALeft = 0) and (ARight = 0) then
        AErrorCode := ecNUM
      else
        Result := Power(ALeft, ARight);

    opUplus:
      Result := ARight;
    opUminus:
      Result := -ARight;
    opPercent:
      Result := ARight / 100;

    opLT:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) < 0;
    opLE:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) <= 0;
    opEQ:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) = 0;
    opGE:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) >= 0;
    opGT:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) > 0;
    opNE:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) <> 0;
  end;
end;

function TdxSpreadSheetFormulaOperationToken.IsMustBeLeftValue: Boolean;
begin
  Result := FOperation in [opAdd, opConcat, opSub, opMul, opDiv, opPower, opLT,
    opLE, opEQ, opGE, opGT, opNE];
end;

function TdxSpreadSheetFormulaOperationToken.GetTokenPriority: Integer;
const
  OperationToPriority: array [TdxSpreadSheetFormulaOperation] of Integer = (2,
    2, 3, 3, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 6, -1, -1);
begin
  Result := OperationToPriority[FOperation];
end;

procedure TdxSpreadSheetFormulaOperationToken.ResetResultErrorCode
  (var AResult: TdxSpreadSheetFormulaResult;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AErrorCode := AResult.ErrorCode;
  AResult.SetError(ecNone);
end;

procedure TdxSpreadSheetFormulaOperationToken.ToString(AStack: TStringList);

  function ExtractFromStack(ALevel: Integer): string;
  begin
    Inc(ALevel, AStack.Count);
    if ALevel >= 0 then
    begin
      Result := AStack[ALevel];
      AStack.Delete(ALevel);
    end
    else
      Result := EmptyStr;
  end;

var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    case FOperation of
      opUplus, opUminus:
        begin
          ABuffer.Append(FormatSettings.Operations[FOperation]);
          ABuffer.Append(ExtractFromStack(-1));
        end;

      opPercent:
        begin
          ABuffer.Append(ExtractFromStack(-1));
          ABuffer.Append(FormatSettings.Operations[FOperation]);
        end;

      opParen:
        begin
          ABuffer.Append(dxLeftParenthesis);
          ABuffer.Append(ExtractFromStack(-1));
          ABuffer.Append(dxRightParenthesis);
        end;

    else
      ABuffer.Append(ExtractFromStack(-2));
      ABuffer.Append(FormatSettings.Operations[FOperation]);
      ABuffer.Append(ExtractFromStack(-1));
    end;
    AStack.Add(ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

{ TdxSpreadSheetFormulaErrorValueToken }

constructor TdxSpreadSheetFormulaErrorValueToken.Create
  (AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  FErrorCode := AErrorCode;
end;

procedure TdxSpreadSheetFormulaErrorValueToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := dxSpreadSheetErrorCodeToString(FErrorCode);
  AErrorCode := FErrorCode;
end;

procedure TdxSpreadSheetFormulaErrorValueToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.SetError(FErrorCode);
end;

procedure TdxSpreadSheetFormulaErrorValueToken.ToString(AStack: TStringList);
begin
  AStack.Add(dxSpreadSheetErrorCodeToString(FErrorCode));
end;

{ TdxSpreadSheetFormulaCustomDefinedNameToken }

function TdxSpreadSheetFormulaCustomDefinedNameToken.
  CanConvertStrToNumber: Boolean;
begin
  Result := False;
end;

{$REGION 'Array Formula Tokens'}
{ TdxSpreadSheetListToken }

function TdxSpreadSheetListToken.IsEnumeration: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetListToken.ParametersToString(ABuffer: TStringBuilder);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := FirstChild;
  while AToken <> nil do
  begin
    if AToken.HasChildren then
    begin
      dxSpreadSheetExpressionToText(ABuffer, AToken.FirstChild);
      if (AToken.Next <> nil) and AToken.Next.HasChildren then
        ABuffer.Append(FormatSettings.ListSeparator);
    end
    else
      ABuffer.Append(FormatSettings.ArraySeparator);

    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetListToken.ToString(AStack: TStringList);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    ParametersToString(ABuffer);
    AStack.Add(ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

{ TdxSpreadSheetFormulaArrayToken }

procedure TdxSpreadSheetFormulaArrayToken.CalculateDimension(var ADimension
  : TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  I: Integer;
begin
  CalculateIndex;

  AErrorCode := ecNone;
  for I := 0 to Length(FIndex) - 1 do
    if FIndex[I] = nil then
    begin
      AErrorCode := ecValue;
      Break;
    end;

  if (AErrorCode <> ecNone) or (FSize.cx = 0) then
    ADimension.SetDimension(0, 0)
  else
    ADimension.SetDimension(FSize.cy, FSize.cx);
end;

procedure TdxSpreadSheetFormulaArrayToken.CalculateIndex;

  procedure CalculateDimensions(out X, Y: Integer);
  var
    ACount: Integer;
    AToken: TdxSpreadSheetFormulaToken;
  begin
    X := 0;
    Y := 1;
    ACount := 0;
    AToken := FirstChild;
    while AToken <> nil do
    begin
      if AToken.ClassType = TdxSpreadSheetFormulaArrayRowSeparator then
      begin
        X := Max(X, ACount);
        ACount := 0;
        Inc(Y);
      end
      else
        Inc(ACount);

      AToken := AToken.Next;
    end;
    X := Max(X, ACount);
  end;

  procedure PopulateIndex;
  var
    AIndex: Integer;
    ARowIndex: Integer;
    AToken: TdxSpreadSheetFormulaToken;
  begin
    AIndex := 0;
    ARowIndex := 0;
    AToken := FirstChild;
    while AToken <> nil do
    begin
      if AToken.ClassType = TdxSpreadSheetFormulaArrayRowSeparator then
      begin
        Inc(ARowIndex);
        AIndex := MakeIndex(ARowIndex, 0);
      end
      else
      begin
        if AToken.HasChildren then
          FIndex[AIndex] := AToken;
        Inc(AIndex);
      end;
      AToken := AToken.Next;
    end;
  end;

begin
  CalculateDimensions(FSize.cx, FSize.cy);
  SetLength(FIndex, Size.cx * Size.cy);
  FillChar(FIndex[0], Length(FIndex) * SizeOf(Pointer), 0);
  PopulateIndex;
end;

function TdxSpreadSheetFormulaArrayToken.CanConvertStrToNumber: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractColumn(const AIndex: Integer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  ARowIndex: Integer;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
  AValueErrorCode: TdxSpreadSheetFormulaErrorCode;
  AVector: TdxSpreadSheetSimpleVector;
begin
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.ColumnCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit(TdxSpreadSheetSimpleVector.Create);

  AVector := TdxSpreadSheetSimpleVector.Create(ADimension.RowCount);
  for ARowIndex := 0 to ADimension.RowCount - 1 do
  begin
    AToken := FIndex[MakeIndex(ARowIndex, AIndex)];
    if (AToken <> nil) and AToken.FirstChild.CanConvertStrToNumber then
    begin
      AToken.FirstChild.GetValue(AValue, AValueErrorCode);
      AVector.Add(AValue, AValueErrorCode);
    end
    else
    begin
      AErrorCode := ecValue;
      Break;
    end;
  end;
  Result := AVector;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractColumnAsArray
  (const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetFormulaArrayToken;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  ARow: Integer;
  AItemToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
begin
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.ColumnCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit;
  for ARow := 0 to ADimension.RowCount - 1 do
  begin
    AItemToken := TdxSpreadSheetFormulaToken.Create;
    AValue := GetValueFromArray(ARow, AIndex, AErrorCode);
    TdxSpreadSheetFormulaToken.AddChild(AItemToken,
      dxSpreadSheetCreateResultToken(AValue, AErrorCode));
    TdxSpreadSheetFormulaToken.AddChild(Result, AItemToken);
    if ARow < ADimension.RowCount - 1 then
      TdxSpreadSheetFormulaToken.AddChild(Result,
        TdxSpreadSheetFormulaArrayRowSeparator.Create);
  end;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractRow(const AIndex: Integer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AColumnIndex: Integer;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
  AValueErrorCode: TdxSpreadSheetFormulaErrorCode;
  AVector: TdxSpreadSheetSimpleVector;
begin
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.RowCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit(TdxSpreadSheetSimpleVector.Create);

  AVector := TdxSpreadSheetSimpleVector.Create(ADimension.ColumnCount);
  for AColumnIndex := 0 to ADimension.ColumnCount - 1 do
  begin
    AToken := FIndex[MakeIndex(AIndex, AColumnIndex)];
    if (AToken <> nil) and AToken.FirstChild.CanConvertStrToNumber then
    begin
      AToken.FirstChild.GetValue(AValue, AValueErrorCode);
      AVector.Add(AValue, AValueErrorCode);
    end
    else
    begin
      AErrorCode := ecValue;
      Break;
    end;
  end;
  Result := AVector;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractRowAsArray
  (const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetFormulaArrayToken;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AColumn: Integer;
  AItemToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
begin
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.RowCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit;
  for AColumn := 0 to ADimension.ColumnCount - 1 do
  begin
    AItemToken := TdxSpreadSheetFormulaToken.Create;
    AValue := GetValueFromArray(AIndex, AColumn, AErrorCode);
    TdxSpreadSheetFormulaToken.AddChild(AItemToken,
      dxSpreadSheetCreateResultToken(AValue, AErrorCode));
    TdxSpreadSheetFormulaToken.AddChild(Result, AItemToken);
  end;
end;

function TdxSpreadSheetFormulaArrayToken.ForEach
  (AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  AValue: Variant;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := Owner.IsArrayFormula;
  AToken := FirstChild;
  while (AToken <> nil) and (AErrorCode = ecNone) do
  begin
    if not(AToken is TdxSpreadSheetFormulaArrayRowSeparator) then
    begin
      if AToken.HasChildren then
        AToken.FirstChild.GetValue(AValue, AErrorCode)
      else
        AErrorCode := ecValue;

      if not Owner.IsArrayFormula then
        Result := (AErrorCode = ecNone) and AProc(AValue, CanConvertStrToNumber,
          AErrorCode, AData, nil) and (AErrorCode = ecNone)
      else
        AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, nil);
    end;
    AToken := AToken.Next;
  end;
end;

function TdxSpreadSheetFormulaArrayToken.MakeIndex(ARowIndex,
  AColumnIndex: Integer): Integer;
begin
  Result := ARowIndex * Size.cx + AColumnIndex;
end;

function TdxSpreadSheetFormulaArrayToken.GetArray(var AErrorCode
  : TdxSpreadSheetFormulaErrorCode): Variant;
var
  AColumn: Integer;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AIndex: Integer;
  ARow: Integer;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
begin
  ADimension := GetDimension(AErrorCode);
  if AErrorCode <> ecNone then
    Exit(Null);

  Result := VarArrayCreate([0, ADimension.RowCount - 1, 0,
    ADimension.ColumnCount - 1], varVariant);
  for ARow := 0 to ADimension.RowCount - 1 do
  begin
    AIndex := MakeIndex(ARow, 0);
    for AColumn := 0 to ADimension.ColumnCount - 1 do
    begin
      AToken := FIndex[AIndex + AColumn];
      if (AToken <> nil) and AToken.FirstChild.CanConvertStrToNumber then
      begin
        AToken.FirstChild.GetValue(AValue, AErrorCode);
        if AErrorCode = ecNone then
          Result[ARow, AColumn] := AValue;
      end
      else
        AErrorCode := ecValue;

      if AErrorCode <> ecNone then
        Exit(Null);
    end;
    if AErrorCode <> ecNone then
      Exit(Null);
  end;
end;

procedure TdxSpreadSheetFormulaArrayToken.GetValueAsArrayItem(const ARow,
  AColumn: Integer; var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AToken: TdxSpreadSheetFormulaToken;
begin
  ADimension := GetDimension(AErrorCode);
  if AErrorCode <> ecNone then
    Exit;
  if InRange(ARow, 0, ADimension.RowCount - 1) and
    InRange(AColumn, 0, ADimension.ColumnCount - 1) then
  begin
    AToken := FIndex[MakeIndex(ARow, AColumn)];
    if AToken <> nil then
      AToken.FirstChild.GetValue(AValue, AErrorCode)
    else
      AErrorCode := ecNA;
  end
  else
    AErrorCode := ecNA;
end;

procedure TdxSpreadSheetFormulaArrayToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := FirstChild;
  if AToken.HasChildren then
    AToken.FirstChild.GetValue(AValue, AErrorCode)
  else
    AErrorCode := ecValue;
end;

procedure TdxSpreadSheetFormulaArrayToken.ToString(AStack: TStringList);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    ABuffer.Append(dxLeftArrayParenthesis);
    ParametersToString(ABuffer);
    ABuffer.Append(dxRightArrayParenthesis);

    AStack.Add(ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

{$ENDREGION}
{$REGION 'Reference Tokens'}
{ TdxSpreadSheetFormulaReferenceToken }

constructor TdxSpreadSheetFormulaReferenceToken.Create(const ARow,
  AColumn: TdxSpreadSheetReference);
begin
  inherited Create;
  FColumn := AColumn;
  FRow := ARow;
end;

function TdxSpreadSheetFormulaReferenceToken.CanConvertStrToNumber: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetFormulaReferenceToken.EnumReferences
  (AProc: TdxSpreadSheetFormulaEnumReferenceTokensProc;
  AProcessDefinedNames: Boolean = False);
var
  AArea: TRect;
  AView: TObject;
begin
  inherited EnumReferences(AProc, AProcessDefinedNames);
  if ExtractReference(AView, AArea) then
    AProc(dxSpreadSheetGetRealArea(AArea), AView, Self);
end;

function TdxSpreadSheetFormulaReferenceToken.ExtractVector(ALength: Integer;
  AIsVertical: Boolean): TdxSpreadSheetVector;
begin
  if AIsVertical then
  begin
    if FRow.IsAllItems then
      Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False,
        ActualColumn, 0, dxSpreadSheetMaxRowCount)
    else
      Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False,
        ActualColumn, ActualRow, ALength);
  end
  else if FColumn.IsAllItems then
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True,
      ActualRow, 0, dxSpreadSheetMaxColumnCount)
  else
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True,
      ActualRow, ActualColumn, ALength);
end;

function TdxSpreadSheetFormulaReferenceToken.GetAbsoluteColumn: Boolean;
begin
  Result := FColumn.IsAbsolute;
end;

function TdxSpreadSheetFormulaReferenceToken.GetAbsoluteRow: Boolean;
begin
  Result := FRow.IsAbsolute;
end;

function TdxSpreadSheetFormulaReferenceToken.GetActualColumn: Integer;
begin
  Result := FColumn.ActualValue(AnchorColumn);
end;

function TdxSpreadSheetFormulaReferenceToken.GetActualRow: Integer;
begin
  Result := FRow.ActualValue(AnchorRow);
end;

function TdxSpreadSheetFormulaReferenceToken.GetAnchorColumn: Integer;
begin
  if Owner <> nil then
    Result := Owner.AnchorColumn
  else
    Result := 0;
end;

function TdxSpreadSheetFormulaReferenceToken.GetAnchorRow: Integer;
begin
  if Owner <> nil then
    Result := Owner.AnchorRow
  else
    Result := 0;
end;

function TdxSpreadSheetFormulaReferenceToken.GetIsError: Boolean;
begin
  Result := FRow.IsError or FColumn.IsError;
end;

function TdxSpreadSheetFormulaReferenceToken.GetR1C1Reference: Boolean;
begin
  Result := Owner.FormatSettings.R1C1Reference;
end;

procedure TdxSpreadSheetFormulaReferenceToken.SetIsError(AValue: Boolean);
begin
  FRow.IsError := True;
  FColumn.IsError := True;
end;

procedure TdxSpreadSheetFormulaReferenceToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  GetCellValue(Owner.View, ActualRow, ActualColumn, AValue, AErrorCode);
end;

function TdxSpreadSheetFormulaReferenceToken.ExtractReference
  (var AView: TObject; var AArea: TRect): Boolean;
begin
  AView := nil;
  AArea.Top := ActualRow;
  AArea.Left := ActualColumn;
  AArea.BottomRight := AArea.TopLeft;
  Result := (AArea.Top >= 0) and (AArea.Left >= 0);
  if Result then
  begin
    if FColumn.IsAllItems then
      AArea.Left := 0;
    if FRow.IsAllItems then
      AArea.Top := 0;
  end;
end;

function TdxSpreadSheetFormulaReferenceToken.ForEach
  (AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  ACellReference: TdxSpreadSheetCellReference;
  AValue: Variant;
begin
  GetValue(AValue, AErrorCode);
  ACellReference.ColumnIndex := ActualColumn;
  ACellReference.RowIndex := ActualRow;
  ACellReference.View := View;
  Result := AProc(AValue, CanConvertStrToNumber, AErrorCode, AData,
    @ACellReference) and (AErrorCode = ecNone);
end;

function TdxSpreadSheetFormulaReferenceToken.IsValid: Boolean;
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := CheckCellReference(ActualRow, ActualColumn, AErrorCode);
end;

procedure TdxSpreadSheetFormulaReferenceToken.Offset(DY, DX: Integer);
begin
  FRow.Move(DY);
  FColumn.Move(DX);
end;

function TdxSpreadSheetFormulaReferenceToken.ReferenceToString: string;
begin
  Result := TdxSpreadSheetReferenceAsString.Build('', Row, Column, AnchorRow,
    AnchorColumn, R1C1Reference);
end;

procedure TdxSpreadSheetFormulaReferenceToken.ToString(AStack: TStringList);
begin
  AStack.Add(ReferenceToString);
end;

procedure TdxSpreadSheetFormulaReferenceToken.UpdateReferences(AView: TObject;
  const AArea: TRect; const ATargetOrigin: TPoint;
  AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);
var
  AReferenceArea: TRect;
begin
  if (Owner.View = AView) or (View = AView) then
  begin
    UpdateReferencesCore(AView, AArea, ATargetOrigin, AMode, AModified);
    if not AModified and (AMode = urmMove) and
      ExtractReference(AView, AReferenceArea) then
      AModified := dxSpreadSheetIntersects(AReferenceArea,
        cxRectSetOrigin(AArea, ATargetOrigin));
    if AModified then
      ClearIsDimensionCalculated;
  end;
end;

procedure TdxSpreadSheetFormulaReferenceToken.UpdateReferencesCore
  (AView: TObject; const AArea: TRect; const ATargetOrigin: TPoint;
  AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);
var
  AIsAnchorInArea: Boolean;
  AIsReferenceInArea: Boolean;
begin
  if AMode <> urmMove then
  begin
    if InRange(AnchorColumn, AArea.Left, AArea.Right) then
      AModified := FRow.UpdateReference(AnchorRow, AArea.Top, AArea.Bottom,
        ATargetOrigin.Y, AView = View, Owner.View = AView) or AModified;
    if InRange(AnchorRow, AArea.Top, AArea.Bottom) then
      AModified := FColumn.UpdateReference(AnchorColumn, AArea.Left,
        AArea.Right, ATargetOrigin.X, AView = View, Owner.View = AView) or
        AModified;
  end
  else
  begin
    AIsAnchorInArea := (Owner.View = AView) and dxSpreadSheetContains(AArea,
      AnchorRow, AnchorColumn);
    AIsReferenceInArea := (AView = View) and dxSpreadSheetContains(AArea,
      ActualRow, ActualColumn);

    AModified := FRow.UpdateReference(AnchorRow, AArea.Top, AArea.Bottom,
      ATargetOrigin.Y, AIsReferenceInArea, AIsAnchorInArea) or AModified;
    AModified := FColumn.UpdateReference(AnchorColumn, AArea.Left, AArea.Right,
      ATargetOrigin.X, AIsReferenceInArea, AIsAnchorInArea) or AModified;
  end;
end;

{ TdxSpreadSheetFormulaAreaReferenceToken }

constructor TdxSpreadSheetFormulaAreaReferenceToken.Create(const ARow, AColumn,
  ARow2, AColumn2: TdxSpreadSheetReference);
begin
  inherited Create(ARow, AColumn);
  FColumn2 := AColumn2;
  FRow2 := ARow2;
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.
  CalculateActualRowAndColumnIndexes(var AActualRow, AActualRow2, AActualColumn,
  AActualColumn2: Integer);
begin
  AActualRow := ActualRow;
  AActualRow2 := ActualRow2;
  if FRow.IsAllItems or FRow2.IsAllItems then
  begin
    AActualRow := 0;
    AActualRow2 := dxSpreadSheetMaxRowIndex;
  end;
  AActualColumn := ActualColumn;
  AActualColumn2 := ActualColumn2;
  if FColumn.IsAllItems or FColumn2.IsAllItems then
  begin
    AActualColumn := 0;
    AActualColumn2 := dxSpreadSheetMaxColumnIndex;
  end;
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.CalculateDimension
  (var ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AActualRow, AActualRow2, AActualColumn, AActualColumn2: Integer;
begin
  inherited CalculateDimension(ADimension, AErrorCode);
  CalculateActualRowAndColumnIndexes(AActualRow, AActualRow2, AActualColumn,
    AActualColumn2);
  if (AActualColumn < 0) or (AActualColumn2 < 0) or (AActualRow < 0) or
    (AActualRow2 < 0) then
    AErrorCode := ecRefErr
  else
    ADimension.SetDimension(Abs(AActualRow2 - AActualRow) + 1,
      Abs(AActualColumn2 - AActualColumn) + 1);
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.ExchangeReferences(var ARef,
  ARef2: TdxSpreadSheetReference);
var
  A: TdxSpreadSheetReference;
begin
  A := ARef;
  ARef := ARef2;
  ARef2 := A;
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.Check;

  function GetRealIndex(ARef: TdxSpreadSheetReference;
    const ACellIndex: Integer): Integer;
  begin
    Result := ARef.ActualValue(0);
    if not ARef.IsAbsolute then
      Inc(Result, ACellIndex);
  end;

  procedure CheckReferences(var ARef, ARef2: TdxSpreadSheetReference;
    const ACellIndex: Integer);
  var
    AIndex, AIndex2: Integer;
  begin
    AIndex := GetRealIndex(ARef, ACellIndex);
    AIndex2 := GetRealIndex(ARef2, ACellIndex);
    if AIndex > AIndex2 then
      ExchangeReferences(ARef, ARef2);
  end;

begin
  CheckReferences(FRow, FRow2, AnchorRow);
  CheckReferences(FColumn, FColumn2, AnchorColumn);
end;

function TdxSpreadSheetFormulaAreaReferenceToken.GetValueAsText
  (out AText: string): Boolean;

  function GetCellValueAsText(const ARow, AColumn: Integer): string;
  var
    ACellData: IdxSpreadSheetCellData;
    AErrorCode: TdxSpreadSheetFormulaErrorCode;
    AFormula: TFormulaAccess;
    AViewData: IdxSpreadSheetViewData;
  begin
    AErrorCode := ecRefErr;
    if Supports(View, IdxSpreadSheetViewData, AViewData) and
      CheckCellReference(ARow, AColumn, AErrorCode) then
    begin
      ACellData := AViewData.GetCellData(ARow, AColumn);
      if ACellData = nil then
        Result := ''
      else
        case ACellData.DataType of
          cdtError:
            Result := dxSpreadSheetErrorCodeToString(ACellData.AsError);
          cdtFormula:
            begin
              AFormula := TFormulaAccess(ACellData.AsFormula);
              if AFormula.FResultValid then
                Result := AFormula.FResult
              else
                Result := '?';
            end;
        else
          Result := VarToStr(ACellData.AsVariant);
        end;
    end
    else
      Result := dxSpreadSheetErrorCodeToString(AErrorCode);
  end;

  function IsTooLargeArea(const AActualColumn, AActualColumn2, AActualRow,
    AActualRow2: Integer): Boolean;
  const
    MaxValueNumber = 10;
  begin
    Result := Min(AActualRow2 - ActualRow + 1, MaxValueNumber) *
      Min(ActualColumn2 - AActualColumn + 1, MaxValueNumber) > MaxValueNumber;
  end;

var
  AActualColumn: Integer;
  AActualColumn2: Integer;
  AActualRow: Integer;
  AActualRow2: Integer;
  ABuffer: TStringBuilder;
  ARow, AColumn: Integer;
  ARowSeparator: Char;
  AValueSeparator: Char;
begin
  Result := True;
  CalculateActualRowAndColumnIndexes(AActualRow, AActualRow2, AActualColumn,
    AActualColumn2);
  if (AActualRow = AActualRow2) and (AActualColumn = AActualColumn2) then
    AText := GetCellValueAsText(AActualRow, AActualColumn)
  else
  begin
    ABuffer := TdxStringBuilderManager.Get;
    try
      ARowSeparator := FormatSettings.ArraySeparator;
      AValueSeparator := FormatSettings.ListSeparator;
      ABuffer.Append(dxLeftArrayParenthesis);
      if IsTooLargeArea(AActualColumn, AActualColumn2, AActualRow, AActualRow2)
      then
        ABuffer.Append('..')
      else
        for ARow := AActualRow to AActualRow2 do
        begin
          for AColumn := AActualColumn to AActualColumn2 do
          begin
            ABuffer.Append(dxStringMarkChar);
            ABuffer.Append(GetCellValueAsText(ARow, AColumn));
            ABuffer.Append(dxStringMarkChar);
            if AColumn < AActualColumn2 then
              ABuffer.Append(AValueSeparator);
          end;
          if ARow < AActualRow2 then
            ABuffer.Append(ARowSeparator);
        end;
      ABuffer.Append(dxRightArrayParenthesis);
      AText := ABuffer.ToString;
    finally
      TdxStringBuilderManager.Release(ABuffer);
    end;
  end;
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.GetValueAsArrayItem
  (const ARow, AColumn: Integer; var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AActualColumn: Integer;
  AActualColumn2: Integer;
  AActualRow: Integer;
  AActualRow2: Integer;
begin
  CalculateActualRowAndColumnIndexes(AActualRow, AActualRow2, AActualColumn,
    AActualColumn2);
  GetCellValue(GetView, AActualRow + ARow, AActualColumn + AColumn, AValue,
    AErrorCode);
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.GetValueRelatedWithCell
  (const ACell: TPoint; var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);

  function InternalCheck(var AIndex: Integer;
    var AError: TdxSpreadSheetFormulaErrorCode;
    const AIndex1, AIndex2: Integer): Boolean;
  begin
    Result := (AIndex >= Min(AIndex1, AIndex2)) and
      (AIndex <= Max(AIndex1, AIndex2));
    if not Result then
      AIndex := Min(AIndex1, AIndex2);
    if Min(AIndex1, AIndex2) < 0 then
      AError := ecValue;
  end;

var
  AAreaColumn: Integer;
  AAreaColumn2: Integer;
  AAreaRow: Integer;
  AAreaRow2: Integer;
  AColumn: Integer;
  AIsColumnValid: Boolean;
  AIsOutOfRange: Boolean;
  AIsRowValid: Boolean;
  ARow: Integer;
  AFormulaArea: TRect;
begin
  CalculateActualRowAndColumnIndexes(AAreaRow, AAreaRow2, AAreaColumn,
    AAreaColumn2);
  if (AAreaRow <> AAreaRow2) and (AAreaColumn <> AAreaColumn2) then
    AErrorCode := ecValue
  else
  begin
    ARow := ACell.Y;
    AColumn := ACell.X;
    AIsRowValid := InternalCheck(ARow, AErrorCode, AAreaRow, AAreaRow2) and
      (AAreaColumn = AAreaColumn2);
    AIsColumnValid := InternalCheck(AColumn, AErrorCode, AAreaColumn,
      AAreaColumn2) and (AAreaRow = AAreaRow2);
    AIsOutOfRange := not AIsRowValid and not AIsColumnValid;
    if AIsOutOfRange and (AErrorCode = ecNone) then
    begin
      if TControllerAccess(Owner.Controller).IsPartOfArrayFormula(View, ACell.Y,
        ACell.X, AFormulaArea) = afpNone then
        AErrorCode := ecValue;
    end;
    if AErrorCode <> ecValue then
      GetCellValue(View, ARow, AColumn, AValue, AErrorCode);
  end;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.IsEnumeration: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.ExtractColumn
  (const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetVector;
var
  AActualRow, AActualRow2: Integer;
begin
  AErrorCode := ecNone;
  if FRow.IsAllItems then
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False,
      ActualColumn + AIndex, 0, dxSpreadSheetMaxRowCount)
  else
  begin
    AActualRow := ActualRow;
    AActualRow2 := ActualRow2;
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False,
      ActualColumn + AIndex, AActualRow, AActualRow2 - AActualRow + 1);
  end;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.ExtractRow
  (const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode)
  : TdxSpreadSheetVector;
var
  AActualColumn, AActualColumn2: Integer;
begin
  AErrorCode := ecNone;
  if FColumn.IsAllItems then
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True,
      ActualRow + AIndex, 0, dxSpreadSheetMaxColumnCount)
  else
  begin
    AActualColumn := ActualColumn;
    AActualColumn2 := ActualColumn2;
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True,
      ActualRow + AIndex, AActualColumn, AActualColumn2 - AActualColumn + 1);
  end;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.ForEach
  (AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  ARow, ARow2, AColumn, AColumn2: Integer;
begin
  CalculateActualRowAndColumnIndexes(ARow, ARow2, AColumn, AColumn2);
  Result := ForEachCell(View, AColumn, ARow, AColumn2, ARow2, AProc, AData,
    AErrorCode);
end;

function TdxSpreadSheetFormulaAreaReferenceToken.IsValid: Boolean;
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := inherited IsValid and CheckCellReference(ActualRow2, ActualColumn2,
    AErrorCode);
end;

function TdxSpreadSheetFormulaAreaReferenceToken.ExtractReference
  (var AView: TObject; var AArea: TRect): Boolean;
begin
  Result := inherited ExtractReference(AView, AArea);
  if Result then
  begin
    AArea.Bottom := ActualRow2;
    AArea.Right := ActualColumn2;
    Result := (AArea.Bottom >= 0) and (AArea.Right >= 0);
  end;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.ReferenceToString: string;
begin
  if (FColumn.IsError or FRow.IsError) and (FColumn2.IsError or FRow2.IsError)
  then
    Result := serRefError
  else
    Result := TdxSpreadSheetReferenceAsString.BuildArea(nil, Row, Column, nil,
      FRow2, FColumn2, AnchorRow, AnchorColumn, R1C1Reference);
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.Offset(DY, DX: Integer);
begin
  inherited Offset(DY, DX);
  FRow2.Move(DY);
  FColumn2.Move(DX);
end;

procedure TdxSpreadSheetFormulaAreaReferenceToken.UpdateReferencesCore
  (AView: TObject; const AArea: TRect; const ATargetOrigin: TPoint;
  AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);

  function CanMoveReference: Boolean;
  begin
    Result := (AMode <> urmMove) or dxSpreadSheetContains(AArea, ActualRow,
      ActualColumn) and dxSpreadSheetContains(AArea, ActualRow2, ActualColumn2);
  end;

  function CanUpdateColumnReference: Boolean;
  begin
    Result := (AArea.Left <> ATargetOrigin.X) and InRange(AnchorRow, AArea.Top,
      AArea.Bottom);
    if AMode = urmMove then
      Result := Result and InRange(AnchorColumn, AArea.Left, AArea.Right);
  end;

  function CanUpdateRowReference: Boolean;
  begin
    Result := (AArea.Top <> ATargetOrigin.Y) and
      InRange(AnchorColumn, AArea.Left, AArea.Right);
    if AMode = urmMove then
      Result := Result and InRange(AnchorRow, AArea.Top, AArea.Bottom);
  end;

begin
  if CanUpdateRowReference then
    AModified := FRow.UpdateAreaReference(AnchorRow, AArea.Top, AArea.Bottom,
      ATargetOrigin.Y, FRow2, CanMoveReference, Owner.View = AView) or
      AModified;
  if CanUpdateColumnReference then
    AModified := FColumn.UpdateAreaReference(AnchorColumn, AArea.Left,
      AArea.Right, ATargetOrigin.X, FColumn2, CanMoveReference,
      Owner.View = AView) or AModified;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.GetAbsoluteColumn2: Boolean;
begin
  Result := FColumn2.IsAbsolute;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.GetAbsoluteRow2: Boolean;
begin
  Result := FRow2.IsAbsolute;
end;

function TdxSpreadSheetFormulaAreaReferenceToken.GetActualColumn2: Integer;
begin
  Result := FColumn2.ActualValue(AnchorColumn);
end;

function TdxSpreadSheetFormulaAreaReferenceToken.GetActualRow2: Integer;
begin
  Result := FRow2.ActualValue(AnchorRow);
end;

{ TdxSpreadSheetFormula3DReferenceToken }

constructor TdxSpreadSheetFormula3DReferenceToken.Create
  (ALink: TdxSpreadSheet3DReferenceCustomLink;
  const ARow, AColumn: TdxSpreadSheetReference);
begin
  inherited Create(ARow, AColumn);
  SetLink(FLink, ALink);
end;

destructor TdxSpreadSheetFormula3DReferenceToken.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormula3DReferenceToken.GetValue(var AValue: Variant;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AView: TObject;
begin
  AView := View;
  if AView <> nil then
    GetCellValue(AView, ActualRow, ActualColumn, AValue, AErrorCode)
  else
    AErrorCode := ecRefErr;
end;

function TdxSpreadSheetFormula3DReferenceToken.ExtractReference
  (var AView: TObject; var AArea: TRect): Boolean;
begin
  Result := inherited ExtractReference(AView, AArea) and (View <> nil);
  if Result then
    AView := View;
end;

function TdxSpreadSheetFormula3DReferenceToken.GetView: TObject;
begin
  Result := Link.ActualData;
end;

function TdxSpreadSheetFormula3DReferenceToken.IsValid: Boolean;
begin
  Result := (View <> nil) and inherited IsValid;
end;

function TdxSpreadSheetFormula3DReferenceToken.ReferenceToString: string;
begin
  Result := TdxSpreadSheetReferenceAsString.Build(Link, Row, Column, AnchorRow,
    AnchorColumn, R1C1Reference);
end;

{ TdxSpreadSheet3DReferenceLink }

function TdxSpreadSheet3DReferenceLink.ToString: string;
var
  AView: TObject;
begin
  AView := Data;
  if AView = nil then
    Result := ''
  else if TdxSpreadSheetInvalidObject.IsInvalid(AView) then
    Result := serRefError
  else
  begin
    Result := TdxSpreadSheetReferenceAsString.BuildViewReference(AView);
    if Result = '' then
      Result := serRefError;
  end;
end;

{ TdxSpreadSheetFormula3DAreaReferenceToken }

constructor TdxSpreadSheetFormula3DAreaReferenceToken.Create(ALink,
  ALink2: TdxSpreadSheet3DReferenceCustomLink; const ARow, AColumn, ARow2,
  AColumn2: TdxSpreadSheetReference);
begin
  inherited Create(ARow, AColumn, ARow2, AColumn2);
  SetLink(FLink, ALink);
  SetLink(FLink2, ALink2);
end;

destructor TdxSpreadSheetFormula3DAreaReferenceToken.Destroy;
begin
  FreeAndNil(FLink);
  FreeAndNil(FLink2);
  inherited Destroy;
end;

function TdxSpreadSheetFormula3DAreaReferenceToken.ExtractReference
  (var AView: TObject; var AArea: TRect): Boolean;
begin
  Result := inherited ExtractReference(AView, AArea) and (View <> nil);
  if Result then
    AView := View;
end;

function TdxSpreadSheetFormula3DAreaReferenceToken.GetView: TObject;
begin
  Result := nil;
  if Link <> nil then
    Result := Link.ActualData;
  if (Result = nil) and (Link2 <> nil) then
    Result := Link2.ActualData;
end;

function TdxSpreadSheetFormula3DAreaReferenceToken.IsValid: Boolean;
begin
  Result := (View <> nil) and inherited IsValid;
end;

function TdxSpreadSheetFormula3DAreaReferenceToken.ReferenceToString: string;
begin
  Result := TdxSpreadSheetReferenceAsString.BuildArea(Link, Row, Column, Link2,
    FRow2, FColumn2, AnchorRow, AnchorColumn, R1C1Reference);
end;

{$ENDREGION}
{$REGION 'Functions'}
{ TdxSpreadSheetFormulaFunctionToken }

constructor TdxSpreadSheetFormulaFunctionToken.Create
  (AInformation: TdxSpreadSheetFunctionInfo);
begin
  FInformation := AInformation;
  FIsDirtyParamInfo := True;
end;

destructor TdxSpreadSheetFormulaFunctionToken.Destroy;
begin
  FreeAndNil(FCalculatedDataList);
  inherited Destroy;
end;

function TdxSpreadSheetFormulaFunctionToken.CreateArrayCopy
  (const AArray: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaToken;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn: Integer;
  AValue: Variant;
  AArrayCopy, AArrayItemToken: TdxSpreadSheetFormulaToken;
begin
  AArrayCopy := TdxSpreadSheetFormulaArrayToken.Create;
  ADimension := AArray.GetDimension(AErrorCode);
  for ARow := 0 to ADimension.RowCount - 1 do
    for AColumn := 0 to ADimension.ColumnCount - 1 do
    begin
      if (AColumn = 0) and (ARow > 0) then
        TdxSpreadSheetFormulaToken.AddChild(AArrayCopy,
          TdxSpreadSheetFormulaArrayRowSeparator.Create());

      AArrayItemToken := TdxSpreadSheetFormulaToken.Create;
      AValue := AArray.GetValueFromArray(ARow, AColumn, AErrorCode);
      TdxSpreadSheetFormulaToken.AddChild(AArrayItemToken,
        dxSpreadSheetCreateResultToken(AValue, AErrorCode));
      TdxSpreadSheetFormulaToken.AddChild(AArrayCopy, AArrayItemToken);
    end;
  Result := TdxSpreadSheetFormulaToken.Create;
  TdxSpreadSheetFormulaToken.AddChild(Result, AArrayCopy);
  CalculatedDataList.Add(Result);
end;

function TdxSpreadSheetFormulaFunctionToken.CreateErrorToken(const AErrorCode
  : TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
var
  AErrorToken: TdxSpreadSheetFormulaToken;
begin
  AErrorToken := TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode);
  Result := TdxSpreadSheetFormulaToken.Create;
  TdxSpreadSheetFormulaToken.AddChild(Result, AErrorToken);
  CalculatedDataList.Add(Result);
end;

function TdxSpreadSheetFormulaFunctionToken.CreateFakeToken
  (AParam: TdxSpreadSheetFormulaToken; const AIndex: Integer;
  var ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
begin
  Result := TdxSpreadSheetFormulaToken.Create;
  FFakeParams[AIndex] := AParam;
  ADimension := FFakeParams[AIndex].FirstChild.GetDimension(AErrorCode);
  CalculatedDataList.Add(Result);
end;

function TdxSpreadSheetFormulaFunctionToken.HasArrayArgument: Boolean;
var
  AToken, AChild: TdxSpreadSheetFormulaToken;
begin
  Result := False;
  AToken := FirstChild;
  while (AToken <> nil) and not Result do
  begin
    AChild := AToken.FirstChild;
    while (AChild <> nil) and not Result do
    begin
      if AChild is TdxSpreadSheetFormulaFunctionToken then
        Result := TdxSpreadSheetFormulaFunctionToken(AChild).HasArrayArgument
      else
        Result := AChild is TdxSpreadSheetFormulaArrayToken;
      AChild := AChild.Next;
    end;
    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  if (FInformation = nil) or not Assigned(FInformation.Proc) then
    AResult.SetError(ecName)
  else if (FInformation.ResultKind <> frkValue) or
    (FInformation.ResultKind = frkParamValue) or
    not(Owner.IsArrayFormula or HasArrayArgument) then
    DoInformationProc(AResult)
  else
  begin
    StoreChildrenOrder;
    try
      ADimension := GetDimension(AErrorCode);
      if AErrorCode <> ecNone then
      begin
        ClearTemporaryData;
        AResult.SetError(AErrorCode);
        FInformation.Proc(AResult, FirstChild)
      end
      else if (ADimension.RowCount = 1) and (ADimension.ColumnCount = 1) then
      begin
        ClearTemporaryData;
        DoInformationProc(AResult);
      end
      else
        CalculateAsArray(AResult);
    finally
      ClearTemporaryData;
    end;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CalculateAsArray
  (AResult: TdxSpreadSheetFormulaResult);
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn: Integer;
  AArrayItemFormulaToken: TdxSpreadSheetFormulaToken;
  AIterationResult: TdxSpreadSheetFormulaResult;
  AResultArray: TdxSpreadSheetFormulaArrayToken;
  AArrayToken: TdxSpreadSheetFormulaToken;
  ARepeatingWhenIsDirty: Boolean;
begin
  AResultArray := TdxSpreadSheetFormulaArrayToken.Create;
  AResult.AddTemporary(AResultArray);
  AIterationResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    ARepeatingWhenIsDirty := False;
    AArrayToken := nil;
    repeat
      TTokenAccess(AResultArray).IsDirty := False;
      for ARow := 0 to FDimension.RowCount - 1 do
        for AColumn := 0 to FDimension.ColumnCount - 1 do
        begin
          if (AColumn = 0) and (ARow > 0) then
            if not ARepeatingWhenIsDirty then
              TdxSpreadSheetFormulaToken.AddChild(AResultArray,
                TdxSpreadSheetFormulaArrayRowSeparator.Create())
            else
              AArrayToken := AArrayToken.Next;

          PopulateFakeTokensByChildren(ARow, AColumn);
          if not ARepeatingWhenIsDirty then
            AArrayItemFormulaToken := TdxSpreadSheetFormulaToken.Create
          else
          begin
            AArrayItemFormulaToken := AArrayToken;
            AArrayItemFormulaToken.FirstChild.Free;
          end;

          FInformation.Proc(AIterationResult, FFirstFakeToken);
          AErrorCode := AIterationResult.ErrorCode;
          TdxSpreadSheetFormulaToken.AddChild(AArrayItemFormulaToken,
            dxSpreadSheetCreateResultToken(AIterationResult.Value, AErrorCode));
          if not ARepeatingWhenIsDirty then
            TdxSpreadSheetFormulaToken.AddChild(AResultArray,
              AArrayItemFormulaToken);
          DestroyFakeTokensChildren(AIterationResult);
          AIterationResult.Clear;
          if ARepeatingWhenIsDirty then
            AArrayToken := AArrayToken.Next;
        end;
      TTokenAccess(AResultArray).ClearIsDimensionCalculated;
      ARepeatingWhenIsDirty := not ARepeatingWhenIsDirty and
        TTokenAccess(AResultArray).IsDirty;
      if ARepeatingWhenIsDirty then
        AArrayToken := AResultArray.FirstChild;
    until not ARepeatingWhenIsDirty;
  finally
    AIterationResult.Free;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CalculateDimension
  (var ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ACurrentParam: TdxSpreadSheetFormulaToken;
  AFakeToken: TdxSpreadSheetFormulaToken;
  AParamDimension: TdxSpreadSheetFormulaTokenDimension;
  APriorToken: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  inherited CalculateDimension(ADimension, AErrorCode);
  FFirstFakeToken := nil;
  APriorToken := nil;
  ACurrentParam := FirstChild;
  I := 0;
  while ACurrentParam <> nil do
  begin
    AParamDimension.SetDimension(1, 1);

    AErrorCode := ecNone;
    AFakeToken := GetFakeToken(I, ACurrentParam, AParamDimension, AErrorCode);

    ADimension.SetDimension(Max(ADimension.RowCount, AParamDimension.RowCount),
      Max(ADimension.ColumnCount, AParamDimension.ColumnCount));

    if APriorToken <> nil then
      TTokenAccess(APriorToken).SetNext(AFakeToken)
    else
      FFirstFakeToken := AFakeToken;

    APriorToken := AFakeToken;
    ACurrentParam := ACurrentParam.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CleanFunctionTokenCachedResult;
begin
  FIsCachedResultPresent := False;
  inherited CleanFunctionTokenCachedResult;
end;

procedure TdxSpreadSheetFormulaFunctionToken.DestroyFakeTokensChildren
  (var AResult: TdxSpreadSheetFormulaResult);
var
  AOwnership: TStreamOwnership;
  AParam: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  I := 0;
  AParam := FFirstFakeToken;
  while AParam <> nil do
  begin
    if (FFakeParams[I] <> nil) and (AParam.FirstChild <> nil) then
    begin
      AResult.ExtractToken(AParam.FirstChild, AOwnership);
      AParam.FirstChild.Free;
    end;
    AParam := AParam.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.DoCacheResult
  (AResult: TdxSpreadSheetFormulaResult);
var
  ALastToken: TdxSpreadSheetFormulaToken;
begin
  FIsCachedResultPresent := False;
  if Controller.CalculationInProcess then
  begin
    FIsCachedResultPresent := True;
    FCachedResultErrorCode := AResult.ErrorCode;
    if AResult.Count > 0 then
    begin
      ALastToken := AResult.LastItem;
      if ALastToken.IsEnumeration then
        FIsCachedResultPresent := False
      else
        ALastToken.GetValue(FCachedResultValue, FCachedResultErrorCode);
    end
    else
      FCachedResultValue := Unassigned;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.DoInformationProc
  (var AResult: TdxSpreadSheetFormulaResult);
begin
  if not GetCachedValue(AResult) then
  begin
    FInformation.Proc(AResult, FirstChild);
    DoCacheResult(AResult);
  end;
end;

function TdxSpreadSheetFormulaFunctionToken.ForEach
  (AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  AResult: TdxSpreadSheetFormulaResult;
  AValue: Variant;
begin
  if Owner = nil then
  begin
    AErrorCode := ecValue;
    Exit(False);
  end;

  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    AErrorCode := AResult.ErrorCode;
    if AErrorCode <> ecNone then
      Exit(False);
    if IsEnumeration then
      Result := TTokenAccess(AResult.LastItem).ForEach(AProc, AData, AErrorCode)
    else
    begin
      AValue := AResult.Value;
      Result := AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, nil) and
        (AErrorCode = ecNone);
    end;
  finally
    AResult.Free;
  end;
end;

function TdxSpreadSheetFormulaFunctionToken.GetCachedValue
  (var AResult: TdxSpreadSheetFormulaResult): Boolean;
begin
  Result := FIsCachedResultPresent;
  if not Result then
    Exit;
  AResult.SetError(FCachedResultErrorCode);
  if FCachedResultErrorCode = ecNone then
    AResult.AddValue(FCachedResultValue);
end;

function TdxSpreadSheetFormulaFunctionToken.GetCalculatedDataList
  : TcxObjectList;
begin
  if FCalculatedDataList = nil then
    FCalculatedDataList := TcxObjectList.Create;
  Result := FCalculatedDataList;
end;

function TdxSpreadSheetFormulaFunctionToken.GetFakeToken(AIndex: Integer;
  AParam: TdxSpreadSheetFormulaToken;
  var ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;

  function IsArray(const AInnerIndex: Integer;
    AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    Result := IsArrayInsteadValue(AInnerIndex, AToken,
      TdxSpreadSheetFormulaArrayToken) or IsArrayInsteadValue(AInnerIndex,
      AToken, TdxSpreadSheetFormulaAreaReferenceToken) and Owner.IsArrayFormula;
  end;

var
  AParamResultToken: TdxSpreadSheetFormulaToken;
  AParamResult: TdxSpreadSheetFormulaResult;
  ADirtyErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := AParam;
  if IsExpectedValueParam(AIndex) then
    if IsArray(AIndex, AParam) then
      Result := CreateFakeToken(AParam, AIndex, ADimension, AErrorCode)
    else if (AParam.ChildCount > 1) or
      (AParam.FirstChild is TdxSpreadSheetFormulaFunctionToken) or
      (AParam.FirstChild is TdxSpreadSheetFormulaParenthesesToken) then
    begin
      AParamResult := TFormulaAccess(Owner).Calculate(AParam.FirstChild);
      try
        AErrorCode := AParamResult.ErrorCode;
        if AErrorCode = ecNone then
        begin
          AParamResultToken := AParamResult.LastItem;
          if IsArray(AIndex, AParamResultToken) then
            Result := CreateFakeToken(CreateArrayCopy(AParamResultToken),
              AIndex, ADimension, AErrorCode)
        end
        else
          Result := CreateFakeToken(CreateErrorToken(AErrorCode), AIndex,
            ADimension, ADirtyErrorCode);
      finally
        AParamResult.Free;
      end;
    end;
end;

function TdxSpreadSheetFormulaFunctionToken.GetMaxParamCount: Integer;
begin
  if FIsDirtyParamInfo then
    InitializeParamInfo;
  Result := FMaxParamCount;
end;

function TdxSpreadSheetFormulaFunctionToken.GetParamKind
  : TdxSpreadSheetFunctionParamKindInfo;
begin
  if FIsDirtyParamInfo then
    InitializeParamInfo;
  Result := FParamKind;
end;

function TdxSpreadSheetFormulaFunctionToken.IsArrayInsteadValue
  (const AIndex: Integer; AParam: TdxSpreadSheetFormulaToken;
  ACheckedClass: TClass): Boolean;
begin
  Result := IsExpectedValueParam(AIndex) and
    ((AParam is ACheckedClass) or ((AParam.ChildCount = 1) and
    (AParam.FirstChild is ACheckedClass)));
end;

procedure TdxSpreadSheetFormulaFunctionToken.InitializeFakeParams;
var
  I: Integer;
begin
  SetLength(FFakeParams, MaxParamCount);
  for I := 0 to MaxParamCount - 1 do
    FFakeParams[I] := nil;
end;

procedure TdxSpreadSheetFormulaFunctionToken.InitializeParamInfo;
begin
  if Assigned(FInformation.ParamInfo) then
  begin
    FInformation.ParamInfo(FMaxParamCount, FParamKind);
    SpecifyMaxParamCount;
    FIsDirtyParamInfo := False;
    InitializeFakeParams;
  end;
end;

function TdxSpreadSheetFormulaFunctionToken.IsEnumeration: Boolean;
begin
  Result := FInformation.ResultKind = frkArray;
end;

function TdxSpreadSheetFormulaFunctionToken.IsExpectedValueParam
  (AIndex: Integer): Boolean;
begin
  Result := AIndex < MaxParamCount;
  if Result then
    Result := (ParamKind[AIndex] = fpkValue) or
      (ParamKind[AIndex] = fpkNonRequiredValue)
  else
    Result := (ParamKind[MaxParamCount - 1] = fpkUnlimited) or
      (ParamKind[MaxParamCount - 1] = fpkNonRequiredUnlimited);
end;

function TdxSpreadSheetFormulaFunctionToken.
  NeedAddFeatureFunctionPrefixToFunctionName: Boolean;
begin
  Result := (Information.Token = 255) and (Information.PrefixID > 0) and
    (Owner.Controller <> nil) and TControllerAccess(Owner.Controller)
    .NeedAddFeatureFunctionPrefixToFunctionName;
end;

function TdxSpreadSheetFormulaFunctionToken.
  NeedForceDimensionCalculating: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetFormulaFunctionToken.PopulateFakeTokensByChildren
  (const ARow, AColumn: Integer);
var
  AParam: TdxSpreadSheetFormulaToken;
  I: Integer;
  AParamValue: Variant;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  I := 0;
  AParam := FFirstFakeToken;
  while AParam <> nil do
  begin
    if FFakeParams[I] <> nil then
    begin
      AParamValue := FFakeParams[I].FirstChild.GetValueFromArray(ARow, AColumn,
        AErrorCode);
      TdxSpreadSheetFormulaToken.AddChild(AParam,
        dxSpreadSheetCreateResultToken(AParamValue, AErrorCode));
    end;
    AParam := AParam.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.RestoreChildrenOrder;
var
  AChild: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  AChild := FirstChild;
  I := 1;
  while AChild <> nil do
  begin
    if I < FChildrenOrder.Count then
      TTokenAccess(AChild).SetNext(FChildrenOrder[I])
    else
      TTokenAccess(AChild).SetNext(nil);

    AChild := AChild.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.SpecifyMaxParamCount;
var
  I, AOldCount, ANewCount: Integer;
begin
  AOldCount := FMaxParamCount;
  ANewCount := AOldCount;
  for I := 0 to AOldCount - 1 do
    if FParamKind[I] in [fpkUnlimited, fpkNonRequiredUnlimited] then
    begin
      ANewCount := Max(FMaxParamCount, ChildCount);
      Break;
    end;
  if ANewCount > AOldCount then
  begin
    dxSpreadSheetInitializeParamInfo(ANewCount, FMaxParamCount, FParamKind);
    for I := AOldCount + 1 to FMaxParamCount do
      FParamKind[I - 1] := fpkNonRequiredValue;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.StoreChildrenOrder;
var
  AChild: TdxSpreadSheetFormulaToken;
begin
  FChildrenOrder := TList.Create;
  AChild := FirstChild;
  if AChild <> nil then
    FFirstChildParent := AChild.Parent;
  while AChild <> nil do
  begin
    FChildrenOrder.Add(AChild);
    AChild := AChild.Next;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.ClearTemporaryData;
begin
  if FChildrenOrder <> nil then
    RestoreChildrenOrder;
  FreeAndNil(FChildrenOrder);
  FreeAndNil(FCalculatedDataList);
  FFirstFakeToken := nil;
end;

procedure TdxSpreadSheetFormulaFunctionToken.ToString(AStack: TStringList);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    if Information <> nil then
    begin
      if NeedAddFeatureFunctionPrefixToFunctionName then
        ABuffer.Append(Information.GetPrefix);
      if Information.NamePtr <> nil then
        ABuffer.Append(FormatSettings.GetFunctionName(Information.NamePtr))
      else
        ABuffer.Append(Information.Name);
    end;
    ABuffer.Append(dxLeftParenthesis);
    ParametersToString(ABuffer);
    ABuffer.Append(dxRightParenthesis);
    AStack.Add(ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;
{$ENDREGION}
{ TdxSpreadSheetFormulaParenthesesToken }

procedure TdxSpreadSheetFormulaParenthesesToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
begin
  if HasChildren then
    AResult.AddResultValue(TFormulaAccess(Owner).Calculate(FirstChild))
end;

procedure TdxSpreadSheetFormulaParenthesesToken.ToString(AStack: TStringList);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    ABuffer.Append(dxLeftParenthesis);
    dxSpreadSheetExpressionToText(ABuffer, FirstChild);
    ABuffer.Append(dxRightParenthesis);
    AStack.Add(ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

{$REGION 'Unknown Tokens'}
{ TdxSpreadSheetFormulaUnknownToken }

constructor TdxSpreadSheetFormulaUnknownToken.Create(const AName: string;
  ALink: TdxSpreadSheet3DReferenceCustomLink = nil);
begin
  FName := AName;
  Link := ALink;
end;

destructor TdxSpreadSheetFormulaUnknownToken.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

function TdxSpreadSheetFormulaUnknownToken.IsEnumeration: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetFormulaUnknownToken.Calculate
  (AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.SetError(ecName);
end;

function TdxSpreadSheetFormulaUnknownToken.LinkAsString: string;
begin
  if FLink = nil then
    Result := ''
  else if FormatSettings.ExpandExternalLinks then
    Result := dxStringMarkChar2 + FLink.ToString + dxStringMarkChar2 +
      dxRefSeparator
  else
    Result := FLink.ToString + dxRefSeparator
end;

procedure TdxSpreadSheetFormulaUnknownToken.SetLink
  (const Value: TdxSpreadSheet3DReferenceCustomLink);
begin
  inherited SetLink(FLink, Value);
end;

procedure TdxSpreadSheetFormulaUnknownToken.ToString(AStack: TStringList);
begin
  AStack.Add(LinkAsString + Name);
end;

{ TdxSpreadSheetFormulaUnknownFunctionToken }

procedure TdxSpreadSheetFormulaUnknownFunctionToken.ToString
  (AStack: TStringList);
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    ABuffer.Append(LinkAsString);
    ABuffer.Append(Name);
    ABuffer.Append(dxLeftParenthesis);
    ParametersToString(ABuffer);
    ABuffer.Append(dxRightParenthesis);

    AStack.Add(ABuffer.ToString);
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

{$ENDREGION}

end.
