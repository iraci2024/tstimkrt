{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressFilterControl }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL }
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
unit cxFilterControl;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI17}
  UITypes,
{$ENDIF}
  Windows, Messages, Types, Variants, Classes, SysUtils, Controls, Forms,
  Graphics,
  StdCtrls, ImgList, cxFilter, cxEdit, cxClasses, cxContainer, cxControls,
  cxGraphics, cxCustomData,
  cxLookAndFeels, cxLookAndFeelPainters, cxDropDownEdit, cxTextEdit,
  cxFilterControlUtils,
  cxDataStorage, cxFormats, cxListBox, dxMessages, cxGeometry, dxCoreGraphics,
  dxCoreClasses;

const
  cxFilterControlMaxDropDownRows = 12;
  cxFilterControlIncSearchDelay: Integer = 1000;

type
  TcxFilterControlCriteria = class;
  TcxCustomFilterControl = class;
  TcxCustomFilterControlClass = class of TcxCustomFilterControl;
  TcxFilterControlViewInfo = class;
  TcxFilterControlViewInfoClass = class of TcxFilterControlViewInfo;

  IcxFilterControl = interface
    ['{B9890E09-5400-428D-8F72-1FF8FD15937C}']
    function GetCaption(Index: Integer): string;
    function GetCount: Integer;
    function GetCriteria: TcxFilterCriteria;
    function GetItemLink(Index: Integer): TObject;
    function GetItemLinkID(Index: Integer): Integer;
    function GetItemLinkName(Index: Integer): string;
    function GetFieldName(Index: Integer): string;
    function GetProperties(Index: Integer): TcxCustomEditProperties;
    function GetValueType(Index: Integer): TcxValueTypeClass;

    property Captions[Index: Integer]: string read GetCaption;
    property Count: Integer read GetCount;
    property Criteria: TcxFilterCriteria read GetCriteria;
    property ItemLinkNames[Index: Integer]: string read GetItemLinkName;
    property ItemLinkIDs[Index: Integer]: Integer read GetItemLinkID;
    property ItemLinks[Index: Integer]: TObject read GetItemLink;
    property FieldNames[Index: Integer]: string read GetFieldName;
    property Properties[Index: Integer]: TcxCustomEditProperties
      read GetProperties;
    property ValueTypes[Index: Integer]: TcxValueTypeClass read GetValueType;
  end;

  IcxFilterControlEx = interface // for internal use only
    ['{06029B36-8508-4613-9CA2-A7907BA76A02}']
    function AllowExpressionEditing: Boolean;
    function GetExpressionProvider: TcxCustomExpressionProvider;
    procedure RegisterListener(ALisninter: TcxCustomFilterControl);
    procedure UnregisterListener(ALisninter: TcxCustomFilterControl);
    function UseTokens: Boolean;
  end;

  IcxFilterControlDialog = interface
    ['{D2369F8D-3B22-41A8-881E-B01BEB624B7D}']
    procedure SetDialogLinkComponent(ALink: TComponent);
  end;

  { TcxFilterControlCriteriaItem }

  TcxFilterControlCriteriaItem = class(TcxFilterCriteriaItem)
  strict private
  const
    InvariantExpressionFlag = #1;
  private
    function GetFilterControlCriteria: TcxFilterControlCriteria;
    function GetFilter: IcxFilterControl;
    function GetItemIndex: Integer;
    function ValidItem: Boolean;
  protected
    function GetDataValue(AData: TObject): Variant; override;
    function GetFieldCaption: string; override;
    function GetFieldName: string; override;
    function GetFilterOperatorClass: TcxFilterOperatorClass; override;
    function ReadExpression(AStream: TStream; AIsUnicode: Boolean)
      : string; override;
    procedure WriteExpression(AStream: TStream;
      const AExpression: string); override;

    property ItemIndex: Integer read GetItemIndex;
  public
    property Filter: IcxFilterControl read GetFilter;
    property Criteria: TcxFilterControlCriteria read GetFilterControlCriteria;
  end;

  { TcxFilterControlCriteria }

  TcxFilterControlCriteria = class(TcxFilterCriteria)
  private
    FControl: TcxCustomFilterControl;
  protected
    function GetIDByItemLink(AItemLink: TObject): Integer; override;
    function GetItemClass: TcxFilterCriteriaItemClass; override;
    function GetItemLinkByID(AID: Integer): TObject; override;
    // ver. 3
    function GetNameByItemLink(AItemLink: TObject): string; override;
    function GetItemLinkByName(const AName: string): TObject; override;

    property Control: TcxCustomFilterControl read FControl;
  public
    constructor Create(AOwner: TcxCustomFilterControl); virtual;
    procedure AssignEvents(Source: TPersistent); override;
  end;

  TcxFilterControlCriteriaClass = class of TcxFilterControlCriteria;

  TcxCustomRowViewInfo = class;
  TcxGroupViewInfo = class;
  TcxConditionViewInfo = class;

  TcxFilterControlHitTest = (fhtNone, fhtButton, fhtBoolOperator, fhtItem,
    fhtOperator, fhtValue, fhtAddCondition, fhtAddValue, fhtExpressionButton,
    fhtRemoveButton, fhtRowContent);

  TcxFilterControlHitTestInfo = record
    HitTest: TcxFilterControlHitTest;
    Mouse: TPoint;
    Shift: TShiftState;
    Row: TcxCustomRowViewInfo;
    ValueIndex: Integer;
  end;

  { TcxCustomRowViewInfo }

  TcxCustomRowViewInfo = class
  private
    FButtonDrawState: TcxButtonState;
    FButtonGlyphRect: TRect;
    FButtonRect: TRect;
    FButtonState: TcxButtonState;
    FButtonText: string;
    FContentRect: TRect;
    FControl: TcxCustomFilterControl;
    FCriteriaItem: TcxCustomFilterCriteriaItem;
    FLevel: Integer;
    FIndent: Integer;
    FParent: TcxCustomRowViewInfo;
    FRemoveButtonDrawState: TcxButtonState;
    FRemoveButtonGlyphRect: TRect;
    FRemoveButtonRect: TRect;
    FRemoveButtonState: TcxButtonState;
    FRowRect: TRect;
    function GetCondition: TcxConditionViewInfo;
    function GetFocused: Boolean;
    function GetGroup: TcxGroupViewInfo;
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure CalculateButton;
    function CalculateButtonDrawState: TcxButtonState;
    function CalculateButtonGlyphRect: TRect;
    function CalculateButtonRect: TRect;
    function CalculateButtonState: TcxButtonState;
    function CalculateButtonText: string;
    function CanFocusButtonOnKeyNavigation: Boolean;
    function GetButtonGlyphMargins: TRect;
    function GetButtonGlyphSize: TSize;
    function GetButtonStartBound: Integer; virtual;
    function HasButton: Boolean;
    function IsButtonVisible: Boolean;
    procedure CalculateRemoveButton;
    function CalculateRemoveButtonDrawState: TcxButtonState;
    function CalculateRemoveButtonGlyphRect: TRect;
    function CalculateRemoveButtonRect: TRect;
    function CalculateRemoveButtonState: TcxButtonState;
    function GetRemoveButtonStartBound: Integer; virtual;
    function HasRemoveButton: Boolean;
    function IsRemoveButtonVisible: Boolean;
    procedure CalculateContentRect;
    function GetContentFinishBound: Integer; virtual;
    function GetContentStartBound: Integer;
    function GetContentStartIndent: Integer;

    function CalculateIndent: Integer;
    function CalculateLevel: Integer;
    function GetElementsIndent: Integer;
    function GetWidth: Integer; virtual;
    function IsLast: Boolean;
    procedure ResetCalculatedInfo; virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AControl: TcxCustomFilterControl;
      AParent: TcxCustomRowViewInfo;
      ACriteriaItem: TcxCustomFilterCriteriaItem); virtual;
    destructor Destroy; override;

    procedure Calc(const ARowRect: TRect); virtual;
    procedure GetHitTestInfo(const P: TPoint;
      var HitInfo: TcxFilterControlHitTestInfo); virtual;
    function Ready: Boolean; virtual;

    property ButtonDrawState: TcxButtonState read FButtonDrawState;
    property ButtonGlyphRect: TRect read FButtonGlyphRect;
    property ButtonRect: TRect read FButtonRect write FButtonRect;
    property ButtonState: TcxButtonState read FButtonState write FButtonState;
    property ButtonText: string read FButtonText write FButtonText;
    property Condition: TcxConditionViewInfo read GetCondition;
    property ContentRect: TRect read FContentRect;
    property Control: TcxCustomFilterControl read FControl;
    property CriteriaItem: TcxCustomFilterCriteriaItem read FCriteriaItem;
    property Focused: Boolean read GetFocused;
    property Group: TcxGroupViewInfo read GetGroup;
    property Indent: Integer read FIndent;
    property Level: Integer read FLevel;
    property Parent: TcxCustomRowViewInfo read FParent;
    property RemoveButtonDrawState: TcxButtonState read FRemoveButtonDrawState;
    property RemoveButtonGlyphRect: TRect read FRemoveButtonGlyphRect;
    property RemoveButtonRect: TRect read FRemoveButtonRect;
    property RemoveButtonState: TcxButtonState read FRemoveButtonState;
    property RowRect: TRect read FRowRect write FRowRect;
    property Width: Integer read GetWidth;
  end;

  { TcxGroupViewInfo }

  TcxGroupViewInfo = class(TcxCustomRowViewInfo)
  private
    FBoolOperator: TcxFilterBoolOperatorKind;
    FBoolOperatorState: TcxButtonState;
    FBoolOperatorText: string;
    FBoolOperatorRect: TRect;
    FCaption: string;
    FCaptionRect: TRect;
    FRows: TList;
    function GetRow(Index: Integer): TcxCustomRowViewInfo;
    function GetRowCount: Integer;
    procedure SetRow(Index: Integer; const Value: TcxCustomRowViewInfo);
  protected
    procedure CalculateBoolOperator;
    function CalculateBoolOperatorRect: TRect;
    function CalculateBoolOperatorState: TcxButtonState;
    function GetBoolOperatorStartBound: Integer;
    procedure CalculateCaption;
    function CalculateCaptionRect: TRect;
    function GetCaptionStartBound: Integer;
    function HasCaption: Boolean;

    function GetButtonStartBound: Integer; override;
    function GetContentFinishBound: Integer; override;
    function GetWidth: Integer; override;
    procedure ResetCalculatedInfo; override;
  public
    constructor Create(AControl: TcxCustomFilterControl;
      AParent: TcxCustomRowViewInfo;
      ACriteriaItem: TcxCustomFilterCriteriaItem); override;
    destructor Destroy; override;

    procedure Add(ARow: TcxCustomRowViewInfo);
    procedure Remove(ARow: TcxCustomRowViewInfo);
    procedure Calc(const ARowRect: TRect); override;
    procedure GetHitTestInfo(const P: TPoint;
      var HitInfo: TcxFilterControlHitTestInfo); override;

    property BoolOperator: TcxFilterBoolOperatorKind read FBoolOperator
      write FBoolOperator;
    property BoolOperatorState: TcxButtonState read FBoolOperatorState;
    property BoolOperatorText: string read FBoolOperatorText
      write FBoolOperatorText;
    property BoolOperatorRect: TRect read FBoolOperatorRect;
    property Caption: string read FCaption write FCaption;
    property CaptionRect: TRect read FCaptionRect;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TcxCustomRowViewInfo read GetRow
      write SetRow;
  end;

  { TcxValuesViewInfo }

  TcxValueInfo = class
  private
    FValue: TcxEditValue;
    FValueContentRect: TRect;
    FValueRect: TRect;
    FValueState: TcxButtonState;
    FValueText: TCaption;
    FValueViewInfo: TcxCustomEditViewInfo;
    procedure SetValueViewInfo(const Value: TcxCustomEditViewInfo);
  public
    constructor Create;
    destructor Destroy; override;

    property Value: TcxEditValue read FValue write FValue;
    property ValueContentRect: TRect read FValueContentRect
      write FValueContentRect;
    property ValueRect: TRect read FValueRect write FValueRect;
    property ValueState: TcxButtonState read FValueState write FValueState;
    property ValueText: TCaption read FValueText write FValueText;
    property ValueViewInfo: TcxCustomEditViewInfo read FValueViewInfo
      write SetValueViewInfo;
  end;

  { TcxValuesViewInfo }

  TcxValuesViewInfo = class
  private
    FAddButtonGlyphRect: TRect;
    FAddButtonRect: TRect;
    FAddButtonState: TcxButtonState;
    FCondition: TcxConditionViewInfo;
    FList: TList;
    FSeparator: string;
    function GetControl: TcxCustomFilterControl;
    function GetValue(Index: Integer): TcxValueInfo;
    function GetWidth: Integer;
  protected
    function CalculateValueState(AIndex: Integer): TcxButtonState;
    function GetCount: Integer;
    function HasAddButton: Boolean;
    procedure UpdateEditorStyle(AStyle: TcxCustomEditStyle;
      AHighlighted, AEnabled: Boolean);
  public
    constructor Create(ACondition: TcxConditionViewInfo);
    destructor Destroy; override;

    procedure AddValue;
    procedure Calc;
    procedure Clear;
    procedure GetHitTestInfo(const P: TPoint;
      var HitInfo: TcxFilterControlHitTestInfo); virtual;
    procedure RemoveValue(AIndex: Integer);

    property AddButtonGlyphRect: TRect read FAddButtonGlyphRect;
    property AddButtonRect: TRect read FAddButtonRect;
    property AddButtonState: TcxButtonState read FAddButtonState;
    property Condition: TcxConditionViewInfo read FCondition;
    property Control: TcxCustomFilterControl read GetControl;
    property Count: Integer read GetCount;
    property Separator: string read FSeparator;
    property Values[Index: Integer]: TcxValueInfo read GetValue; default;
    property Width: Integer read GetWidth;
  end;

  { TcxConditionViewInfo }

  TcxConditionViewInfo = class(TcxCustomRowViewInfo)
  private
    FEditorHelper: TcxCustomFilterEditHelperClass;
    FEditorProperties: TcxCustomEditProperties;
    FExpression: string;
    FExpressionRect: TRect;
    FExpressionButtonRect: TRect;
    FExpressionButtonState: TcxButtonState;
    FItemIndex: Integer;
    FItemLink: TObject;
    FItemRect: TRect;
    FItemState: TcxButtonState;
    FItemText: string;
    FOperator: TcxFilterControlOperator;
    FOperatorRect: TRect;
    FOperatorState: TcxButtonState;
    FOperatorText: string;
    FProperties: TcxCustomEditProperties;
    FSupportedOperators: TcxFilterControlOperators;
    FValueType: TcxValueTypeClass;
    FValues: TcxValuesViewInfo;
    function GetItemIndex: Integer;
    procedure SetExpression(const AValue: string);
    procedure SetItemText(const AValue: string);
  protected
    ValueEditorData: TcxCustomEditData;

    procedure CalculateItem;
    function CalculateItemRect: TRect;
    function CalculateItemState: TcxButtonState;
    function GetItemStartBound: Integer;
    procedure CalculateOperator;
    function GetOperatorIndent: Integer;
    function CalculateOperatorRect: TRect;
    function CalculateOperatorState: TcxButtonState;
    function CalculateOperatorText: string;
    function GetOperatorStartBound: Integer;
    function NeedDrawOperatorAsImage: Boolean;
    procedure CalculateExpression;
    function CalculateExpressionRect: TRect;
    function GetExpressionStartBound: Integer;
    function IsExpressionVisible: Boolean; virtual;
    procedure CalculateExpressionButton;
    function CalculateExpressionButtonRect: TRect;
    function CalculateExpressionButtonState: TcxButtonState;
    function CanShowExpressionButton: Boolean;
    function GetExpresionButtonStartBound: Integer;
    function HasExpressionButton: Boolean; virtual;

    procedure AddValue;
    function GetContentFinishBound: Integer; override;
    function GetRemoveButtonStartBound: Integer; override;
    function GetWidth: Integer; override;
    function HasDisplayValues: Boolean; virtual;
    procedure InitValues(ASaveValue: Boolean);
    procedure InternalInit; virtual;
    function IsOperatorSupportedForExpression: Boolean; overload;
    function IsOperatorSupportedForExpression
      (AOperator: TcxFilterControlOperator): Boolean; overload;
    procedure ResetCalculatedInfo; override;
    procedure SetItem(AIndex: Integer);
    procedure UpdateSupportedOperators;
    procedure ValidateConditions;

    property Properties: TcxCustomEditProperties read FProperties;
  public
    constructor Create(AControl: TcxCustomFilterControl;
      AParent: TcxCustomRowViewInfo;
      ACriteriaItem: TcxCustomFilterCriteriaItem); override;
    destructor Destroy; override;

    procedure Calc(const ARowRect: TRect); override;
    procedure GetHitTestInfo(const P: TPoint;
      var HitInfo: TcxFilterControlHitTestInfo); override;
    function GetProperties: TcxCustomEditProperties;
    function Ready: Boolean; override;

    property EditorHelper: TcxCustomFilterEditHelperClass read FEditorHelper;
    property EditorProperties: TcxCustomEditProperties read FEditorProperties;
    property Expression: string read FExpression write SetExpression;
    property ExpressionRect: TRect read FExpressionRect;
    property ExpressionButtonRect: TRect read FExpressionButtonRect;
    property ExpressionButtonState: TcxButtonState read FExpressionButtonState;
    property ItemLink: TObject read FItemLink;
    property ItemIndex: Integer read FItemIndex;
    property ItemRect: TRect read FItemRect;
    property ItemState: TcxButtonState read FItemState;
    property ItemText: string read FItemText write SetItemText; property
    Operator: TcxFilterControlOperator read FOperator write FOperator;
    property OperatorRect: TRect read FOperatorRect;
    property OperatorState: TcxButtonState read FOperatorState;
    property OperatorText: string read FOperatorText write FOperatorText;
    property SupportedOperators: TcxFilterControlOperators
      read FSupportedOperators;
    property ValueType: TcxValueTypeClass read FValueType;
    property Values: TcxValuesViewInfo read FValues;
  end;

  { TcxFilterControlImagesHelper }

  TcxFilterControlImagesHelper = class
  strict private
    class var FFilterControlImages: TcxImageList;
    class var FColorProvider: IdxColorPalette;

    class function GetFilterControlImages: TcxImageList; static;
    class procedure LoadSvgImages(AImageList: TcxImageList; AStream: TStream);
    class procedure LoadPngImages(AImageList: TcxImageList; AStream: TStream);
  protected
    class procedure CreateImages;
    class procedure DestroyImages;

    class procedure DrawImage(ACanvas: TCanvas; AIndex: Integer;
      const ABounds: TRect; APalette: IdxColorPalette); overload;
    class procedure DrawImage(ACanvas: TCanvas; AIndex: Integer;
      const ABounds: TRect; APainter: TcxCustomLookAndFeelPainter;
      AAccentColor: TdxAlphaColor); overload;
    class function GetOperatorImageIndex(AOperator
      : TcxFilterControlOperator): Integer;

    class property FilterControlImages: TcxImageList
      read GetFilterControlImages;
  public
    class procedure DrawOperatorImage(ACanvas: TCanvas;
      AOperator: TcxFilterControlOperator; const ABounds: TRect;
      APainter: TcxCustomLookAndFeelPainter; AAccentColor: TdxAlphaColor);
    class function GetScaledSize(AScaleFactor: TdxScaleFactor): TSize;
    class function GetSize: TSize;
  end;

  { TcxFilterDropDownMenuInnerListBox }

  TcxFilterDropDownMenuInnerListBox = class(TdxCustomDropDownInnerListBox)
  private
    FShowShortCut: Boolean;
    function CheckAccelerators(AKey: Word): Boolean;
  protected
    procedure ClearIncrementalSearch; override;
    function DoIncrementalSearch(var Key: Char): Boolean; override;
    procedure DrawItemImage(const R: TRect; AItem: TdxCustomListBoxItem;
      AState: TcxButtonState); override;
    procedure DrawItemText(const R: TRect; AItem: TdxCustomListBoxItem;
      AState: TcxButtonState); override;
    function GetTextFlags: Integer; override;
    function IsIncSearch: Boolean;
    function IsIncSearchChar(AChar: Char): Boolean; override;
    function ProcessKeyPress(var Key: Char): Boolean; override;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState)
      : Boolean; override;
  public
    property ShowShortCut: Boolean read FShowShortCut write FShowShortCut;
  end;

  { TcxFilterControlDropDownMenuInnerListBox }

  TcxFilterControlDropDownMenuInnerListBox = class
    (TcxFilterDropDownMenuInnerListBox);

  { TcxFilterDropDownMenu }

  TcxFilterDropDownMenu = class(TdxCustomDropDownListBox)
  private
    function GetInnerListBox: TcxFilterDropDownMenuInnerListBox;
  protected
    function CreateInnerListBox: TdxCustomDropDownInnerListBox; override;

    property InnerListBox: TcxFilterDropDownMenuInnerListBox
      read GetInnerListBox;
  public
    constructor Create(AControl: TcxControl); reintroduce; virtual;

    procedure CreateConditionList(ASupportedOperators
      : TcxFilterControlOperators);
    procedure Popup(const AForBounds: TRect; const ACaption: string = '';
      AKey: Char = #0); reintroduce; virtual;
  end;

  { TcxFilterControlDropDownMenu }

  TcxFilterControlDropDownMenu = class(TcxFilterDropDownMenu)
  private
    FControl: TcxCustomFilterControl;
    FDroppedInfo: TcxFilterControlHitTestInfo;
    function GetInnerListBox: TcxFilterControlDropDownMenuInnerListBox;
    procedure SaveDroppedInfo;
  protected
    function CreateInnerListBox: TdxCustomDropDownInnerListBox; override;
    procedure DoCloseUp(AClosedViaKeyboard: Boolean); override;
    procedure DoSelectItem(AItem: TdxCustomListBoxItem;
      ASelectedViaKeyboard: Boolean); override;

    procedure ClearDroppedInfo;
    procedure CreateActionMenu;
    procedure CreateItemList(AList: TStrings);
    procedure CreateBoolOperatorList;
    function IsSameDroppedInfo(AInfo: TcxFilterControlHitTestInfo): Boolean;
    //
    property Control: TcxCustomFilterControl read FControl;
    property InnerListBox: TcxFilterControlDropDownMenuInnerListBox
      read GetInnerListBox;
  public
    constructor Create(AControl: TcxCustomFilterControl); reintroduce; virtual;
    procedure Popup(const AForBounds: TRect; const ACaption: string = '';
      AKey: Char = #0); override;
  end;

  { TcxCustomFilterControl }

  TFilterControlState = (fcsNormal, fcsSelectingAction, fcsSelectingItem,
    fcsSelectingBoolOperator, fcsSelectingCondition, fcsSelectingValue);

  TcxFilterControlFont = (fcfBoolOperator, fcfItem, fcfCondition, fcfValue,
    fcfExpression);
  TcxFilterControlFonts = set of TcxFilterControlFont;
  TcxActivateValueEditKind = (aveEnter, aveKey, aveMouse);

  TcxCustomFilterControl = class(TcxControl, IcxMouseTrackingCaller,
    IcxFormatControllerListener, IdxSkinSupport)
  private
    FAssignedFonts: TcxFilterControlFonts;
    FCriteria: TcxFilterControlCriteria;
    FCriteriaDisplayStyle: TcxFilterCriteriaDisplayStyle;
    FDropDownMenu: TcxFilterControlDropDownMenu;
    FFocusedInfo: TcxFilterControlHitTestInfo;
    FFonts: array [TcxFilterControlFont] of TFont;
    FHotTrack: TcxFilterControlHitTestInfo;
    FInplaceEditors: TcxInplaceEditList;
    FIsFontsChangedLocked: Boolean;
    FLeftOffset: Integer;
    FLockCount: Integer;
    FRoot: TcxCustomRowViewInfo;
    FRows: TList;
    FSortItems: Boolean;
    FState: TFilterControlState;
    FTextEditProperties: TcxTextEditProperties;
    FTopVisibleRow: Integer;

    FValueEditor: TcxCustomEdit;
    FValueEditorStyle: TcxCustomEditStyle;

    FViewInfo: TcxFilterControlViewInfo;
    FHotTrackOnUnfocused: Boolean;
    FNullstring: string;
    FShowLevelLines: Boolean;
    FWantTabs: Boolean;
    FWasError: Boolean;
    FOnApplyFilter: TNotifyEvent;
    procedure CreateFonts;
    procedure DoFontChanged(Sender: TObject);
    function GetFont(Index: Integer): TFont;
    function IsFontStored(Index: Integer): Boolean;
    procedure SetFont(Index: Integer; const Value: TFont);

    function FocusedRowIndex: Integer;
    function GetRow(Index: Integer): TcxCustomRowViewInfo;
    function GetRowCount: Integer;
    function GetFocusedRow: TcxCustomRowViewInfo;
    function GetValueEditorBackgroundColor: TColor;
    function GetValueEditorBounds: TRect;

    procedure DropDownMenuItemClick(AIndex: Integer);
    procedure SetFocusedRow(ARow: TcxCustomRowViewInfo);
    procedure ActionMenuClick(AIndex: Integer);
    function IsNullstringStored: Boolean;
    procedure ProcessHitTest(AHitTest: TcxFilterControlHitTest; AKey: Char);
    procedure ReadData(AStream: TStream);
    procedure RecalcRows;
    procedure RefreshFonts;
    procedure SetAssignedFonts(const Value: TcxFilterControlFonts);
    procedure SetCriteriaDisplayStyle(const AValue
      : TcxFilterCriteriaDisplayStyle);
    procedure SetLeftOffset(Value: Integer);
    procedure SetNullstring(const Value: string);
    procedure SetTopVisibleRow(Value: Integer);
    procedure SetShowLevelLines(const Value: Boolean);
    procedure SetWantTabs(const Value: Boolean);
    procedure ValidateEditorPos(const ABounds: TRect);
    procedure ValueEditorInit;
    // value editor events
    procedure ValueEditorAfterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValueEditorExit(Sender: TObject);
    procedure ValueEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValueEditorMouseLeave(ASender: TObject);
    procedure ValueEditorMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);

    procedure WriteData(AStream: TStream);

    procedure WMDropDownMenuClosed(var Message: TMessage);
      message DXWM_FILTERCONTROL_DROPDOWNMENUCLOSED;
  protected
    // override VCL
    procedure DefineProperties(Filer: TFiler); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    // override cxControl
    procedure BiDiModeChanged; override;
    procedure BoundsChanged; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure DoLayoutChange; virtual;
    procedure FocusChanged; override;
    procedure FontChanged; override;
    function GetBorderSize: Integer; override;
    procedure InitControl; override;
    procedure InitScrollBarsParameters; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    // work with rows
    procedure AddCondition(ARow: TcxCustomRowViewInfo);
    procedure AddGroup;
    procedure AddValue;
    procedure ClearRows;
    procedure Remove;
    procedure RemoveRow;
    procedure RemoveValue;
    // navigation
    procedure FocusNext(ATab: Boolean);
    procedure FocusPrev(ATab: Boolean);
    procedure FocusUp(ATab: Boolean);
    procedure FocusDown(ATab: Boolean);
    procedure RowNavigate(AElement: TcxFilterControlHitTest;
      ACellIndex: Integer = -1);
    procedure ValueEditorHide(AAccept: Boolean);

    procedure Recalculate;

    procedure EnsureRowVisible;
    procedure RefreshProperties;

    procedure BuildFromCriteria; virtual;
    procedure BuildFromRows;

    procedure CreateInternalControls; virtual;
    procedure DestroyInternalControls; virtual;
    procedure DoApplyFilter; virtual;
    function GetDefaultProperties: TcxCustomEditProperties; virtual;
    function GetDefaultPropertiesViewInfo: TcxCustomEditViewInfo;
    function GetFilterControlCriteriaClass
      : TcxFilterControlCriteriaClass; virtual;
    function GetViewInfoClass: TcxFilterControlViewInfoClass; virtual;
    function HasFocus: Boolean;
    function HasHotTrack: Boolean;
    procedure FillFilterItemList(AStrings: TStrings); virtual;
    procedure UpdateHotTrackInfo(Shift: TShiftState; X, Y: Integer);
    function UseTokens: Boolean;
    procedure ValidateConditions(var SupportedOperations
      : TcxFilterControlOperators); virtual;

    procedure CorrectOperatorClass(var AOperatorClass
      : TcxFilterOperatorClass); virtual;
    function GetFilterCaption: string; virtual;
    function GetFilterLink: IcxFilterControl; virtual;
    function GetFilterLinkEx: IcxFilterControlEx; virtual;
    function GetFilterText: string; virtual;
    procedure RemoveAction; virtual;
    procedure SelectAction; virtual;
    procedure SelectBoolOperator(AKey: Char); virtual;
    procedure SelectCondition(AKey: Char); virtual;
    procedure SelectExpression; virtual;
    procedure SelectItem(AKey: Char); virtual;
    procedure SelectValue(AActivateKind: TcxActivateValueEditKind;
      AKey: Char); virtual;

    // IcxMouseTrackingCaller
    procedure DoMouseLeave;
    procedure IcxMouseTrackingCaller.MouseLeave = DoMouseLeave;

    // IcxFormatControllerListener
    procedure FormatChanged;

    property Criteria: TcxFilterControlCriteria read FCriteria;
    property CriteriaDisplayStyle: TcxFilterCriteriaDisplayStyle
      read FCriteriaDisplayStyle write SetCriteriaDisplayStyle
      default fcdsDefault;
    property DropDownMenu: TcxFilterControlDropDownMenu read FDropDownMenu;
    property FilterLink: IcxFilterControl read GetFilterLink;
    property FilterLinkEx: IcxFilterControlEx read GetFilterLinkEx;
    property FocusedInfo: TcxFilterControlHitTestInfo read FFocusedInfo;
    property FocusedRow: TcxCustomRowViewInfo read GetFocusedRow
      write SetFocusedRow;
    property FontExpression: TFont index fcfExpression read GetFont
      write SetFont stored IsFontStored;
    property HotTrack: TcxFilterControlHitTestInfo read FHotTrack;
    property LeftOffset: Integer read FLeftOffset write SetLeftOffset;
    property Nullstring: string read FNullstring write SetNullstring
      stored IsNullstringStored;
    property Root: TcxCustomRowViewInfo read FRoot;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TcxCustomRowViewInfo read GetRow;
    property State: TFilterControlState read FState write FState;
    property TopVisibleRow: Integer read FTopVisibleRow write SetTopVisibleRow;
    property ValueEditor: TcxCustomEdit read FValueEditor;
    property ViewInfo: TcxFilterControlViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyFilter;
    procedure BeginUpdate;
    procedure Clear; virtual;
    procedure EndUpdate;
    function IsNeedSynchronize: Boolean;
    function IsValid: Boolean; virtual;
    function HasItems: Boolean;
    procedure LayoutChanged;
    procedure Localize;
    // save & restore
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    // IdxLocalizerListener
    procedure TranslationChanged; override;

    // properties
    property AssignedFonts: TcxFilterControlFonts read FAssignedFonts
      write SetAssignedFonts default [];
    property Color default clBtnFace;
    property FilterCaption: string read GetFilterCaption;
    property FilterText: string read GetFilterText;
    property FontBoolOperator: TFont index fcfBoolOperator read GetFont
      write SetFont stored IsFontStored;
    property FontCondition: TFont index fcfCondition read GetFont write SetFont
      stored IsFontStored;
    property FontItem: TFont index fcfItem read GetFont write SetFont
      stored IsFontStored;
    property FontValue: TFont index fcfValue read GetFont write SetFont
      stored IsFontStored;
    property HotTrackOnUnfocused: Boolean read FHotTrackOnUnfocused
      write FHotTrackOnUnfocused default True;
    property LookAndFeel;
    property ParentColor default False;
    property ShowLevelLines: Boolean read FShowLevelLines
      write SetShowLevelLines default True;
    property SortItems: Boolean read FSortItems write FSortItems default False;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default False;
    property OnApplyFilter: TNotifyEvent read FOnApplyFilter
      write FOnApplyFilter;
  end;

  { TcxFilterControlPainter }

  TcxFilterControlPainter = class
  private
    FControl: TcxCustomFilterControl;
    function GetCanvas: TcxCanvas;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetViewInfo: TcxFilterControlViewInfo;
    procedure DrawGroup(ARow: TcxGroupViewInfo);
    procedure DrawCondition(ARow: TcxConditionViewInfo);
    procedure DrawExpression(ARow: TcxConditionViewInfo);
    procedure DrawItemCaption(ARow: TcxConditionViewInfo);
    procedure DrawOperator(ARow: TcxConditionViewInfo);
    procedure DrawValues(ARow: TcxConditionViewInfo);
  protected
    function GetContentColor: TColor; virtual;
    procedure DrawBorder;
    procedure DrawDotLine(const R: TRect);
    procedure DrawRow(ARow: TcxCustomRowViewInfo); virtual;
    procedure TextDraw(X, Y: Integer; const AText: string);
  public
    constructor Create(AOwner: TcxCustomFilterControl); virtual;
    property Canvas: TcxCanvas read GetCanvas;
    property ContentColor: TColor read GetContentColor;
    property Control: TcxCustomFilterControl read FControl;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ViewInfo: TcxFilterControlViewInfo read GetViewInfo;
  end;

  TcxFilterControlPainterClass = class of TcxFilterControlPainter;

  { TcxFilterControlViewInfo }

  TcxFilterControlViewInfo = class
  private
    FControl: TcxCustomFilterControl;
    FAddConditionRect: TRect;
    FAddConditionCaption: string;
    FBitmap: TBitmap;
    FBitmapCanvas: TcxCanvas;
    FButtonState: TcxButtonState;
    FContentWidth: Integer;
    FEnabled: Boolean;
    FFocusRect: TRect;
    FIsMinTokenPaddingValid: Boolean;
    FIsTokenParamsValid: Boolean;
    FMaxRowWidth: Integer;
    FPainter: TcxFilterControlPainter;
    FRowHeight: Integer;
    FMinTokenPadding: TRect;
    FMinValueWidth: Integer;
    FTokenParams: TdxFilterTokenParams;

    procedure AdjustTokenPadding(var APadding: TRect);
    procedure CalcButtonState;
    procedure CheckBitmap;
    function GetCanvas: TcxCanvas;
    function GetEditHeight: Integer;
    function GetMinTokenPadding: TRect;
    function GetTokenParams: TdxFilterTokenParams;
  protected
    procedure CalcFocusRect; virtual;
    function GetPainterClass: TcxFilterControlPainterClass; virtual;
    function GetRowMargins: TRect;
    function GetTextMargins(AItem: TcxFilterControlHitTest): TRect;
    function HasAddConditionButton: Boolean;
    procedure InvalidateMinTokenPadding;
    procedure InvalidateTokenParams;
    procedure ResetContentWidth;

    property MinTokenPadding: TRect read GetMinTokenPadding;
    property TokenParams: TdxFilterTokenParams read GetTokenParams;
  public
    constructor Create(AOwner: TcxCustomFilterControl); virtual;
    destructor Destroy; override;
    procedure Calc;
    procedure GetHitTestInfo(AShift: TShiftState; const P: TPoint;
      var HitInfo: TcxFilterControlHitTestInfo); virtual;
    procedure Paint;
    procedure InvalidateRow(ARow: TcxCustomRowViewInfo);
    procedure Update;
    property AddConditionCaption: string read FAddConditionCaption;
    property AddConditionRect: TRect read FAddConditionRect;
    property ButtonState: TcxButtonState read FButtonState;
    property Canvas: TcxCanvas read GetCanvas;
    property Control: TcxCustomFilterControl read FControl;
    property ContentWidth: Integer read FContentWidth;
    property Enabled: Boolean read FEnabled;
    property MinValueWidth: Integer read FMinValueWidth;
    property Painter: TcxFilterControlPainter read FPainter;
    property RowHeight: Integer read FRowHeight;
  end;

  { TcxFilterControl }

  TcxFilterControl = class(TcxCustomFilterControl, IcxFilterControlDialog)
  private
    FLinkComponent: TComponent;
    function GetLinkComponent: TComponent;
    procedure RegisterLinkNotifications;
    procedure SetLinkComponent(Value: TComponent);
    procedure UnregisterLinkNotifications;
  protected
    // IcxFilterControlDialog
    procedure IcxFilterControlDialog.SetDialogLinkComponent = SetLinkComponent;

    function GetFilterLink: IcxFilterControl; override;
    function GetFilterLinkEx: IcxFilterControlEx; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    destructor Destroy; override;

    procedure UpdateFilter;
  published
    property Align;
    property Anchors;
    property AssignedFonts;
    property BiDiMode;
    property Color;
    property CriteriaDisplayStyle;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontBoolOperator;
    property FontCondition;
    property FontExpression;
    property FontItem;
    property FontValue;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property HotTrackOnUnfocused;
    property LinkComponent: TComponent read GetLinkComponent
      write SetLinkComponent;
    property LookAndFeel;
    property Nullstring; // lowercase because define Nullstring in CBuilder
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowLevelLines;
    property SortItems;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantTabs;
    property OnApplyFilter;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

function cxGetConditionText(AOperator: TcxFilterControlOperator): string;
function IsSupportFiltering(AClass: TcxCustomEditPropertiesClass): Boolean;
function cxGetMinTokenPadding(AScaleFactor: TdxScaleFactor;
  AUseRightToLeftAlignment: Boolean): TRect; // for internal use only

implementation

{$R FilterControlIcons.res}

uses
  Math, StrUtils, CommCtrl,
  dxCore, cxDrawTextUtils, cxVariants, cxFilterConsts, cxFilterControlStrs,
  dxDPIAwareUtils, dxZIPUtils,
  dxGDIPlusClasses, dxTypeHelpers;

type
  TWinControlAccess = class(TWinControl);

const
  cxFilterControlFontColors: array [TcxFilterControlFont] of TColor = (clRed,
    clGreen, clMaroon, clBlue, $2B29A3);
  cxExpressionButtonText: string = 'f(x)';
  cxTokenValueEditorBorderWidth = 1;

var
  cxBoolOperatorText: array [TcxFilterBoolOperatorKind] of string;
  cxConditionText: array [TcxFilterControlOperator] of string;
  HalftoneBrush: HBRUSH;

function cxGetConditionText(AOperator: TcxFilterControlOperator): string;
begin
  Result := cxConditionText[AOperator];
end;

function IsSupportFiltering(AClass: TcxCustomEditPropertiesClass): Boolean;
var
  Test: TcxCustomEditProperties;
begin
  Result := False;
  if AClass <> nil then
  begin
    Test := AClass.Create(nil);
    Result := esoFiltering in Test.GetSupportedOperations;
    Test.Free;
  end;
end;

function cxGetMinTokenPadding(AScaleFactor: TdxScaleFactor;
  AUseRightToLeftAlignment: Boolean): TRect;
begin
  Result := EditContentDefaultOffsets[True];
  TdxVisualRefinements.Padding.InflatePadding(Result, AScaleFactor,
    AUseRightToLeftAlignment);
  Inc(Result.Left, cxTokenValueEditorBorderWidth);
  Inc(Result.Top, cxTokenValueEditorBorderWidth);
  Inc(Result.Right, cxTokenValueEditorBorderWidth);
  Inc(Result.Bottom, cxTokenValueEditorBorderWidth);
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function WidthOf(const R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(const R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

procedure CenterRectVert(const ABounds: TRect; var R: TRect);
var
  H1, H2: Integer;
begin
  H1 := HeightOf(ABounds);
  H2 := HeightOf(R);
  OffsetRect(R, 0, (ABounds.Top - R.Top) + (H1 - H2) div 2);
end;

function cxStrFromBoolOperator(ABoolOperator
  : TcxFilterBoolOperatorKind): string;
begin
  case ABoolOperator of
    fboAnd:
      Result := cxGetResourceString(@cxSFilterBoolOperatorAnd);
    fboOr:
      Result := cxGetResourceString(@cxSFilterBoolOperatorOr);
    fboNotAnd:
      Result := cxGetResourceString(@cxSFilterBoolOperatorNotAnd);
    fboNotOr:
      Result := cxGetResourceString(@cxSFilterBoolOperatorNotOr);
  else
    Result := '';
  end;
end;

{ TcxFilterControlCriteriaItem }

function TcxFilterControlCriteriaItem.GetDataValue(AData: TObject): Variant;
begin
  Result := Null;
end;

function TcxFilterControlCriteriaItem.GetFieldCaption: string;
begin
  if ValidItem then
    Result := Filter.Captions[ItemIndex]
  else
    Result := '';
end;

function TcxFilterControlCriteriaItem.GetFieldName: string;
begin
  if ValidItem then
    Result := Filter.FieldNames[ItemIndex]
  else
    Result := '';
end;

function TcxFilterControlCriteriaItem.GetFilterOperatorClass
  : TcxFilterOperatorClass;
begin
  Result := inherited GetFilterOperatorClass;
  Criteria.Control.CorrectOperatorClass(Result);
end;

function TcxFilterControlCriteriaItem.ReadExpression(AStream: TStream;
  AIsUnicode: Boolean): string;
begin
  Result := inherited ReadExpression(AStream, AIsUnicode);
  if (Criteria.Control.FilterLinkEx <> nil) and (Result <> '') and
    (Result[1] = InvariantExpressionFlag) then
  begin
    Delete(Result, 1, 1);
    Result := Criteria.Control.FilterLinkEx.GetExpressionProvider.
      InvariantExpressionToExpression(Result);
  end;
end;

procedure TcxFilterControlCriteriaItem.WriteExpression(AStream: TStream;
  const AExpression: string);
var
  AValue: string;
begin
  AValue := AExpression;
  if (Criteria.Control.FilterLinkEx <> nil) and (AValue <> '') then
    AValue := InvariantExpressionFlag + Criteria.Control.FilterLinkEx.
      GetExpressionProvider.ExpressionToInvariantExpression(AValue);
  inherited WriteExpression(AStream, AValue);
end;

function TcxFilterControlCriteriaItem.GetFilter: IcxFilterControl;
begin
  if (Criteria <> nil) and (Criteria.Control <> nil) then
    Result := Criteria.Control.FilterLink
  else
    Result := nil;
end;

function TcxFilterControlCriteriaItem.GetItemIndex: Integer;
var
  I: Integer;
  AFilter: IcxFilterControl;
begin
  Result := -1;
  AFilter := Filter;
  if AFilter <> nil then
  begin
    for I := 0 to AFilter.Count - 1 do
      if AFilter.ItemLinks[I] = ItemLink then
      begin
        Result := I;
        break;
      end;
  end;
end;

function TcxFilterControlCriteriaItem.ValidItem: Boolean;
begin
  Result := (Filter <> nil) and (ItemIndex >= 0) and (ItemIndex < Filter.Count);
end;

function TcxFilterControlCriteriaItem.GetFilterControlCriteria
  : TcxFilterControlCriteria;
begin
  Result := TcxFilterControlCriteria(inherited Criteria);
end;

{ TcxFilterControlCriteria }

constructor TcxFilterControlCriteria.Create(AOwner: TcxCustomFilterControl);
begin
  inherited Create;
  FControl := AOwner;
  // ver 3
  Version := cxDataFilterVersion;
end;

procedure TcxFilterControlCriteria.AssignEvents(Source: TPersistent);
begin
  // don't assign events
end;

function TcxFilterControlCriteria.GetIDByItemLink(AItemLink: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Control.FilterLink.Count - 1 do
    if Control.FilterLink.ItemLinks[I] = AItemLink then
    begin
      Result := Control.FilterLink.ItemLinkIDs[I];
      break;
    end;
end;

function TcxFilterControlCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TcxFilterControlCriteriaItem;
end;

function TcxFilterControlCriteria.GetItemLinkByID(AID: Integer): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Control.FilterLink.Count - 1 do
    if Control.FilterLink.ItemLinkIDs[I] = AID then
    begin
      Result := Control.FilterLink.ItemLinks[I];
      break;
    end;
end;

function TcxFilterControlCriteria.GetNameByItemLink(AItemLink: TObject): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Control.FilterLink.Count - 1 do
    if Control.FilterLink.ItemLinks[I] = AItemLink then
    begin
      Result := Control.FilterLink.ItemLinkNames[I];
      break;
    end;
end;

function TcxFilterControlCriteria.GetItemLinkByName
  (const AName: string): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Control.FilterLink.Count - 1 do
    if SameText(Control.FilterLink.ItemLinkNames[I], AName) then
    begin
      Result := Control.FilterLink.ItemLinks[I];
      break;
    end;
end;

{ TcxCustomRowViewInfo }

constructor TcxCustomRowViewInfo.Create(AControl: TcxCustomFilterControl;
  AParent: TcxCustomRowViewInfo; ACriteriaItem: TcxCustomFilterCriteriaItem);
begin
  inherited Create;
  FControl := AControl;
  FParent := AParent;
  FCriteriaItem := ACriteriaItem;
  FButtonState := cxbsNormal;
  if AParent <> nil then
    AParent.Group.Add(Self);
end;

destructor TcxCustomRowViewInfo.Destroy;
begin
  if Parent <> nil then
    Parent.Group.Remove(Self);
  inherited Destroy;
end;

procedure TcxCustomRowViewInfo.Calc(const ARowRect: TRect);
begin
  ResetCalculatedInfo;
  FRowRect := ARowRect;
  FLevel := CalculateLevel;
  FIndent := CalculateIndent;
end;

procedure TcxCustomRowViewInfo.GetHitTestInfo(const P: TPoint;
  var HitInfo: TcxFilterControlHitTestInfo);
begin
  if HasButton and PtInRect(ButtonRect, P) then
    HitInfo.HitTest := fhtButton
  else if HasRemoveButton and PtInRect(RemoveButtonRect, P) then
    HitInfo.HitTest := fhtRemoveButton
  else if Control.UseTokens and not IsRectEmpty(ContentRect) and
    PtInRect(ContentRect, P) then
    HitInfo.HitTest := fhtRowContent;
end;

function TcxCustomRowViewInfo.Ready: Boolean;
begin
  Result := True;
end;

procedure TcxCustomRowViewInfo.CalculateButton;
begin
  if not Control.UseTokens then
    FButtonText := CalculateButtonText;
  FButtonRect := CalculateButtonRect;
  if Control.UseTokens then
    FButtonGlyphRect := CalculateButtonGlyphRect;
  FButtonState := CalculateButtonState;
  FButtonDrawState := CalculateButtonDrawState;
end;

function TcxCustomRowViewInfo.CalculateButtonDrawState: TcxButtonState;
begin
  if cxIsTouchModeEnabled and Control.UseTokens then
    Result := cxbsHot
  else
    Result := ButtonState;
end;

function TcxCustomRowViewInfo.CalculateButtonGlyphRect: TRect;
begin
  Result := cxRectCenter(ButtonRect, GetButtonGlyphSize);
end;

function TcxCustomRowViewInfo.CalculateButtonRect: TRect;
var
  ASize: TSize;
  ALeft: Integer;
begin
  Control.Canvas.Font.Assign(Control.Font);
  ASize.cy := HeightOf(RowRect) - ScaleFactor.Apply
    (IfThen(cxIsTouchModeEnabled, 12, 4));
  if Control.UseTokens then
    ASize.cx := GetButtonGlyphMargins.Left + GetButtonGlyphSize.cx +
      GetButtonGlyphMargins.Right
  else if Control.Root = Self then
    ASize.cx := Control.Canvas.TextWidth(ButtonText + '00')
  else
    ASize.cx := Control.Canvas.TextWidth('0') * Length(ButtonText);
  dxAdjustToTouchableSize(ASize.cx, ScaleFactor);
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetButtonStartBound -
    ASize.cx, GetButtonStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(RowRect, Result);
end;

function TcxCustomRowViewInfo.CalculateButtonState: TcxButtonState;
begin
  if not Control.ViewInfo.Enabled then
    Result := cxbsDisabled
  else if (Control.FocusedRow = Self) and
    (Control.FocusedInfo.HitTest = fhtButton) then
    if Control.UseTokens and Control.DropDownMenu.Active then
      Result := cxbsPressed
    else
      Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest = fhtButton) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxCustomRowViewInfo.CalculateButtonText: string;
begin
  if Parent <> nil then
    Result := '...'
  else
    Result := cxGetResourceString(@cxSFilterRootButtonCaption);
end;

function TcxCustomRowViewInfo.CanFocusButtonOnKeyNavigation: Boolean;
begin
  Result := HasButton and not Control.UseTokens;
end;

function TcxCustomRowViewInfo.GetButtonGlyphMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(4, 4, 4, 4));
end;

function TcxCustomRowViewInfo.GetButtonGlyphSize: TSize;
begin
  Result := ScaleFactor.Apply(Size(10, 10));
end;

function TcxCustomRowViewInfo.GetButtonStartBound: Integer;
begin
  Result := GetContentStartBound;
end;

function TcxCustomRowViewInfo.HasButton: Boolean;
begin
  Result := not IsRectEmpty(ButtonRect);
end;

function TcxCustomRowViewInfo.IsButtonVisible: Boolean;
begin
  Result := HasButton and (not Control.UseTokens or
    (ButtonDrawState in [cxbsHot, cxbsPressed]) or Control.HasHotTrack and
    (Control.HotTrack.Row = Self) and (Control.HotTrack.HitTest <> fhtNone));
end;

function TcxCustomRowViewInfo.HasRemoveButton: Boolean;
begin
  Result := not IsRectEmpty(RemoveButtonRect);
end;

function TcxCustomRowViewInfo.IsRemoveButtonVisible: Boolean;
begin
  Result := HasRemoveButton and
    ((RemoveButtonDrawState in [cxbsHot, cxbsPressed]) or
    Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest <> fhtNone));
end;

procedure TcxCustomRowViewInfo.CalculateContentRect;
begin
  FContentRect.Top := RowRect.Top - Control.ViewInfo.GetRowMargins.Top;
  FContentRect.Bottom := RowRect.Bottom + Control.ViewInfo.GetRowMargins.Bottom;
  FContentRect.Left := GetContentStartBound;
  FContentRect.Right := GetContentFinishBound;
end;

function TcxCustomRowViewInfo.GetContentFinishBound: Integer;
var
  ALastRect: TRect;
begin
  ALastRect := cxEmptyRect;
  if HasRemoveButton then
    ALastRect := RemoveButtonRect
  else if HasButton then
    ALastRect := ButtonRect;
  Result := IfThen(IsRectEmpty(ALastRect), GetContentStartBound,
    IfThen(Control.UseRightToLeftAlignment, ALastRect.Left, ALastRect.Right));
end;

function TcxCustomRowViewInfo.GetContentStartBound: Integer;
begin
  if not Control.UseRightToLeftAlignment then
    Result := RowRect.Left + GetContentStartIndent + Indent
  else
    Result := RowRect.Right - GetContentStartIndent - Indent;
end;

function TcxCustomRowViewInfo.GetContentStartIndent: Integer;
begin
  Result := ScaleFactor.Apply(IfThen(Control.UseTokens, 10, 4));
end;

procedure TcxCustomRowViewInfo.CalculateRemoveButton;
begin
  FRemoveButtonRect := CalculateRemoveButtonRect;
  FRemoveButtonGlyphRect := CalculateRemoveButtonGlyphRect;
  FRemoveButtonState := CalculateRemoveButtonState;
  FRemoveButtonDrawState := CalculateRemoveButtonDrawState;
end;

function TcxCustomRowViewInfo.CalculateRemoveButtonDrawState: TcxButtonState;
begin
  if cxIsTouchModeEnabled then
    Result := cxbsHot
  else
    Result := RemoveButtonState;
end;

function TcxCustomRowViewInfo.CalculateRemoveButtonGlyphRect: TRect;
begin
  Result := cxRectCenter(RemoveButtonRect, GetButtonGlyphSize);
end;

function TcxCustomRowViewInfo.CalculateRemoveButtonRect: TRect;
var
  ALeft: Integer;
  ASize: TSize;
begin
  ASize.cy := HeightOf(RowRect) - ScaleFactor.Apply
    (IfThen(cxIsTouchModeEnabled, 12, 4));
  ASize.cx := GetButtonGlyphMargins.Left + GetButtonGlyphSize.cx +
    GetButtonGlyphMargins.Right;
  dxAdjustToTouchableSize(ASize.cx, ScaleFactor);
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetRemoveButtonStartBound -
    ASize.cx, GetRemoveButtonStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(RowRect, Result);
end;

function TcxCustomRowViewInfo.CalculateRemoveButtonState: TcxButtonState;
begin
  if (Control.FocusedRow = Self) and
    (Control.FocusedInfo.HitTest = fhtRemoveButton) then
    Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest = fhtRemoveButton) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxCustomRowViewInfo.GetRemoveButtonStartBound: Integer;
begin
  Result := IfThen(Control.UseRightToLeftAlignment, ButtonRect.Left,
    ButtonRect.Right);
end;

function TcxCustomRowViewInfo.CalculateIndent: Integer;
begin
  Result := FLevel * HeightOf(RowRect);
end;

function TcxCustomRowViewInfo.CalculateLevel: Integer;
var
  AParent: TcxCustomRowViewInfo;
begin
  Result := 0;
  AParent := Parent;
  if AParent = nil then
    Exit;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    Inc(Result);
  end
end;

function TcxCustomRowViewInfo.GetElementsIndent: Integer;
begin
  if Control.UseTokens then
    Result := Control.ViewInfo.TokenParams.ElementsIndent
  else
    Result := ScaleFactor.Apply(8);
end;

function TcxCustomRowViewInfo.GetWidth: Integer;
begin
  Result := Indent + GetContentStartIndent;
  if HasButton then
  begin
    Inc(Result, WidthOf(FButtonRect));
    if not Control.UseTokens then
      Inc(Result, GetElementsIndent);
  end;
  if HasRemoveButton then
    Inc(Result, WidthOf(FRemoveButtonRect));
end;

function TcxCustomRowViewInfo.IsLast: Boolean;
begin
  Result := (FParent = nil) or
    (FParent.Group.GetRow(FParent.Group.GetRowCount - 1) = Self);
end;

procedure TcxCustomRowViewInfo.ResetCalculatedInfo;
begin
  FButtonText := '';
  FButtonRect := cxEmptyRect;
  FButtonState := cxbsNormal;
  FButtonDrawState := FButtonState;
  FButtonGlyphRect := cxEmptyRect;
  FRemoveButtonRect := cxEmptyRect;
  FRemoveButtonState := cxbsNormal;
  FRemoveButtonDrawState := FRemoveButtonState;
  FRemoveButtonGlyphRect := cxEmptyRect;
end;

function TcxCustomRowViewInfo.GetCondition: TcxConditionViewInfo;
begin
  Result := Self as TcxConditionViewInfo;
end;

function TcxCustomRowViewInfo.GetFocused: Boolean;
begin
  Result := Control.FocusedRow = Self;
end;

function TcxCustomRowViewInfo.GetGroup: TcxGroupViewInfo;
begin
  Result := Self as TcxGroupViewInfo;
end;

function TcxCustomRowViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Control.ScaleFactor;
end;

{ TcxGroupViewInfo }

constructor TcxGroupViewInfo.Create(AControl: TcxCustomFilterControl;
  AParent: TcxCustomRowViewInfo; ACriteriaItem: TcxCustomFilterCriteriaItem);
begin
  inherited;
  FRows := TList.Create;
  FCaption := cxGetResourceString(@cxSFilterGroupCaption);
  if ACriteriaItem <> nil then
    FBoolOperator := TcxFilterCriteriaItemList(ACriteriaItem).BoolOperatorKind
  else
    FBoolOperator := fboAnd;
end;

destructor TcxGroupViewInfo.Destroy;
begin
  while RowCount > 0 do
    Rows[0].Free;
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TcxGroupViewInfo.Add(ARow: TcxCustomRowViewInfo);
begin
  FRows.Add(ARow);
end;

procedure TcxGroupViewInfo.Remove(ARow: TcxCustomRowViewInfo);
begin
  FRows.Remove(ARow);
end;

procedure TcxGroupViewInfo.Calc(const ARowRect: TRect);
begin
  inherited Calc(ARowRect);
  if Control.UseTokens then
  begin
    CalculateBoolOperator;
    CalculateButton;
    CalculateRemoveButton;
  end
  else
  begin
    CalculateButton;
    CalculateBoolOperator;
    if Caption <> '' then
      CalculateCaption;
  end;
  CalculateContentRect;
end;

procedure TcxGroupViewInfo.GetHitTestInfo(const P: TPoint;
  var HitInfo: TcxFilterControlHitTestInfo);
begin
  inherited GetHitTestInfo(P, HitInfo);
  if (HitInfo.HitTest in [fhtNone, fhtRowContent]) and
    PtInRect(BoolOperatorRect, P) then
    HitInfo.HitTest := fhtBoolOperator;
end;

procedure TcxGroupViewInfo.CalculateBoolOperator;
begin
  FBoolOperatorText := cxBoolOperatorText[BoolOperator];
  FBoolOperatorRect := CalculateBoolOperatorRect;
  FBoolOperatorState := CalculateBoolOperatorState;
end;

function TcxGroupViewInfo.CalculateBoolOperatorRect: TRect;
var
  ASize: TSize;
  ALeft: Integer;
  AMargins: TRect;
begin
  if Control.UseTokens then
    Control.Canvas.Font.Assign(Control.Font)
  else
    Control.Canvas.Font.Assign(Control.FontBoolOperator);
  AMargins := Control.ViewInfo.GetTextMargins(fhtBoolOperator);
  ASize := Control.Canvas.TextExtent(FBoolOperatorText);
  ASize := cxSize(ASize.cx + AMargins.Left + AMargins.Right,
    ASize.cy + AMargins.Top + AMargins.Bottom);
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetBoolOperatorStartBound -
    ASize.cx, GetBoolOperatorStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(RowRect, Result);
end;

function TcxGroupViewInfo.CalculateBoolOperatorState: TcxButtonState;
begin
  if not Control.ViewInfo.Enabled then
    Result := cxbsDisabled
  else if (Control.FocusedRow = Self) and
    (Control.FocusedInfo.HitTest = fhtBoolOperator) then
    if Control.DropDownMenu.Active then
      Result := cxbsPressed
    else
      Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest = fhtBoolOperator) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxGroupViewInfo.GetBoolOperatorStartBound: Integer;
begin
  if Control.UseTokens then
    Result := GetContentStartBound
  else
    Result := IfThen(Control.UseRightToLeftAlignment,
      ButtonRect.Left - GetElementsIndent, ButtonRect.Right +
      GetElementsIndent);
end;

procedure TcxGroupViewInfo.CalculateCaption;
begin
  FCaptionRect := CalculateCaptionRect;
end;

function TcxGroupViewInfo.CalculateCaptionRect: TRect;
var
  ASize: TSize;
  ALeft: Integer;
begin
  Control.Canvas.Font.Assign(Control.Font);
  ASize := Control.Canvas.TextExtent(Caption);
  ASize := cxSize(ASize.cx + ScaleFactor.Apply(2),
    ASize.cy + ScaleFactor.Apply(2));
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetCaptionStartBound -
    ASize.cx, GetCaptionStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(FRowRect, Result);
end;

function TcxGroupViewInfo.GetCaptionStartBound: Integer;
begin
  Result := IfThen(Control.UseRightToLeftAlignment, BoolOperatorRect.Left -
    GetElementsIndent, BoolOperatorRect.Right + GetElementsIndent);
end;

function TcxGroupViewInfo.HasCaption: Boolean;
begin
  Result := not IsRectEmpty(CaptionRect);
end;

function TcxGroupViewInfo.GetButtonStartBound: Integer;
begin
  if Control.UseTokens then
    Result := IfThen(Control.UseRightToLeftAlignment, BoolOperatorRect.Left,
      BoolOperatorRect.Right)
  else
    Result := inherited GetButtonStartBound;
end;

function TcxGroupViewInfo.GetContentFinishBound: Integer;
var
  ALastRect: TRect;
begin
  if Control.UseTokens then
    Result := inherited GetContentFinishBound
  else
  begin
    if Caption <> '' then
      ALastRect := CaptionRect
    else
      ALastRect := BoolOperatorRect;
    Result := IfThen(Control.UseRightToLeftAlignment, ALastRect.Left,
      ALastRect.Right);
  end;
end;

function TcxGroupViewInfo.GetWidth: Integer;
begin
  Result := inherited GetWidth + WidthOf(FBoolOperatorRect);
  if not Control.UseTokens then
    Inc(Result, GetElementsIndent);
  if HasCaption then
    Inc(Result, WidthOf(FCaptionRect) + GetElementsIndent);
end;

procedure TcxGroupViewInfo.ResetCalculatedInfo;
begin
  inherited ResetCalculatedInfo;
  FCaptionRect := cxEmptyRect;
end;

function TcxGroupViewInfo.GetRow(Index: Integer): TcxCustomRowViewInfo;
begin
  Result := TcxCustomRowViewInfo(FRows[Index]);
end;

function TcxGroupViewInfo.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

procedure TcxGroupViewInfo.SetRow(Index: Integer;
  const Value: TcxCustomRowViewInfo);
begin
  FRows[Index] := Value;
end;

{ TcxValueInfo }

constructor TcxValueInfo.Create;
begin
  inherited Create;
  FValue := Null;
  FValueText := '';
  FValueContentRect := cxEmptyRect;
  FValueRect := cxEmptyRect;
end;

destructor TcxValueInfo.Destroy;
begin
  FreeAndNil(FValueViewInfo);
  inherited Destroy;
end;

procedure TcxValueInfo.SetValueViewInfo(const Value: TcxCustomEditViewInfo);
begin
  FValueViewInfo.Free;
  FValueViewInfo := Value;
end;

{ TcxValuesViewInfo }

constructor TcxValuesViewInfo.Create(ACondition: TcxConditionViewInfo);
begin
  inherited Create;
  FCondition := ACondition;
  FList := TList.Create;
end;

destructor TcxValuesViewInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TcxValuesViewInfo.AddValue;
var
  V: TcxValueInfo;
begin
  V := TcxValueInfo.Create;
  V.ValueViewInfo := TcxCustomEditViewInfo
    (Condition.GetProperties.GetViewInfoClass.Create);
  FList.Add(V);
end;

procedure TcxValuesViewInfo.Calc;
const
  AButtonTransparency: array [Boolean] of TcxEditButtonTransparency =
    (ebtHideInactive, ebtNone);
var
  AMargins: TRect;
  AHighlighted, AHotTrack, AUseDisplayValue: Boolean;
  AProperties: TcxCustomEditProperties;
  AProvider: IcxEditDefaultValuesProvider;
  ASize: TSize;
  ASizeProperties: TcxEditSizeProperties;
  ATopLeft, AMouse: TPoint;
  AValue: TcxEditValue;
  AViewData: TcxCustomEditViewData;
  I, AExtraSize, AWidth, ALeft: Integer;
begin
  if not Condition.HasDisplayValues or (Condition.Expression <> '') then
  begin
    for I := 0 to Count - 1 do
    begin
      Values[I].FValueRect := cxEmptyRect;
      Values[I].FValueContentRect := cxEmptyRect;
    end;
    Exit;
  end
  else
  begin
    ALeft := IfThen(Control.UseRightToLeftAlignment, Condition.OperatorRect.Left
      - Condition.GetOperatorIndent, Condition.OperatorRect.Right +
      Condition.GetOperatorIndent);
    ATopLeft := Point(ALeft, 0);
    if Condition.Operator in [fcoBetween, fcoNotBetween] then
    begin
      FSeparator := cxGetResourceString(@cxSFilterAndCaption);
      FSeparator := AnsiLowerCase(FSeparator);
    end
    else if (Condition.Operator in [fcoInList, fcoNotInList]) and not Control.UseTokens
    then
    begin
      Control.Canvas.Font.Assign(Control.FontValue);
      if not Control.UseRightToLeftAlignment then
        Inc(ATopLeft.X, Control.Canvas.TextWidth('('))
      else
        Dec(ATopLeft.X, Control.Canvas.TextWidth('('));
      FSeparator := ', ';
    end
    else
      FSeparator := '';
  end;
  AHotTrack := Control.HasHotTrack;
  AUseDisplayValue := (Condition.EditorHelper <> nil) and
    (Condition.EditorHelper.UseDisplayValue);
  for I := 0 to Count - 1 do
    with Values[I] do
    begin
      if VarIsNull(Value) then
      begin
        AProperties := Control.GetDefaultProperties;
        AValue := Control.Nullstring;
      end
      else
      begin
        AProperties := Condition.EditorProperties;
        if AUseDisplayValue then
          AValue := ValueText
        else
          AValue := Value;
        if Control.UseTokens and VarIsStr(AValue) and (AValue = '') then
          AValue := QuotedStr('');
      end;
      with AProperties do
      begin
        LockUpdate(True);
        AProvider := IDefaultValuesProvider;
        IDefaultValuesProvider := nil;
      end;
      try
        ValueViewInfo := TcxCustomEditViewInfo
          (AProperties.GetViewInfoClass.Create);
        with AProperties, Control do
        begin
          AHighlighted := AHotTrack and
            (((FHotTrack.Row = Condition) and (FHotTrack.HitTest = fhtValue) and
            (FHotTrack.ValueIndex = I) and (State = fcsNormal)) or
            (HasFocus and (FocusedRow = Condition) and
            (FFocusedInfo.HitTest = fhtValue) and
            (FFocusedInfo.ValueIndex = I)));
          UpdateEditorStyle(FValueEditorStyle, AHighlighted, ViewInfo.Enabled);
          ValueViewInfo.Transparent := UseTokens;
          AViewData := CreateViewData(FValueEditorStyle, True);
          AViewData.SupportsTouchMode := True;
          AViewData.Enabled := ViewInfo.Enabled;
          try
            AViewData.UseRightToLeftAlignment :=
              Control.UseRightToLeftAlignment;
            AViewData.UseRightToLeftReading := Control.UseRightToLeftReading;
            AViewData.UseRightToLeftScrollBar :=
              Control.UseRightToLeftScrollBar;
            // calculate ValueRect, ValueContentRect
            FValueContentRect.TopLeft := ATopLeft;
            ASizeProperties.MaxLineCount := 1;
            ASizeProperties.Width := -1;
            ASizeProperties.Height := -1;
            ASize := AViewData.GetEditSize(Canvas, AValue, ASizeProperties);
            if AHighlighted and (ASize.cx < ViewInfo.MinValueWidth) and
              (not UseTokens or (Control.State = fcsSelectingValue)) then
              ASize.cx := ViewInfo.MinValueWidth;
            if not Control.UseRightToLeftAlignment then
            begin
              FValueContentRect.Right := FValueContentRect.Left + ASize.cx;
              FValueContentRect.Bottom := FValueContentRect.Top + ASize.cy;
            end
            else
              FValueContentRect := Rect(ATopLeft.X - ASize.cx, ATopLeft.Y,
                ATopLeft.X, ATopLeft.Y + ASize.cy);
            CenterRectVert(Condition.RowRect, FValueContentRect);
            FValueRect := FValueContentRect;
            if UseTokens then
            begin
              FValueRect.Top := Condition.ItemRect.Top;
              FValueRect.Bottom := Condition.ItemRect.Bottom;
              FValueContentRect.Top := FValueRect.Top +
                cxTokenValueEditorBorderWidth;
              FValueContentRect.Bottom := FValueRect.Bottom -
                cxTokenValueEditorBorderWidth;
              AMargins := Control.ViewInfo.GetTextMargins(fhtValue);
              FValueContentRect := cxRectOffset(FValueContentRect,
                cxTokenValueEditorBorderWidth, 0);
              if AMargins.Left > Control.ViewInfo.MinTokenPadding.Left then
                FValueContentRect := cxRectOffset(FValueContentRect,
                  AMargins.Left - Control.ViewInfo.MinTokenPadding.Left, 0);
              FValueRect.Right := FValueContentRect.Right +
                cxTokenValueEditorBorderWidth;
              if AMargins.Right > Control.ViewInfo.MinTokenPadding.Right then
              begin
                FValueRect.Right := FValueRect.Right + AMargins.Right -
                  Control.ViewInfo.MinTokenPadding.Right;
                if Condition.EditorHelper.EditPropertiesHasButtons then
                  FValueContentRect.Right := FValueRect.Right -
                    cxTokenValueEditorBorderWidth;
              end;
            end;
            // calculate
            if not FilterEditsController.FindHelper(AProperties.ClassType).EditPropertiesHasButtons
            then
              AViewData.ButtonVisibleCount := 0;
            AViewData.EditValueToDrawValue(AValue, ValueViewInfo);
            AViewData.Calculate(Canvas, ValueContentRect, AMouse, cxmbNone, [],
              ValueViewInfo, True);
          finally
            FreeAndNil(AViewData);
          end;
          if FSeparator <> '' then
          begin
            AExtraSize := ScaleFactor.Apply(4);
            Canvas.Font.Assign(FontValue);
            Inc(AExtraSize, Canvas.TextWidth(FSeparator) +
              ScaleFactor.Apply(4));
          end
          else if UseTokens then
            AExtraSize := Condition.GetElementsIndent
          else
            AExtraSize := 0;
          AWidth := WidthOf(ValueRect);
          if not Control.UseRightToLeftAlignment then
            Inc(ATopLeft.X, AWidth + AExtraSize)
          else
            Dec(ATopLeft.X, AWidth + AExtraSize);
          ValueState := CalculateValueState(I);
        end;
      finally
        with AProperties do
        begin
          IDefaultValuesProvider := AProvider;
          LockUpdate(False);
        end;
      end;
    end;
  if Condition.Operator in [fcoInList, fcoNotInList] then
  begin
    ALeft := IfThen(Control.UseRightToLeftAlignment,
      Values[Count - 1].ValueRect.Left, Values[Count - 1].ValueRect.Right);
    if Control.UseTokens then
      ASize.cx := Condition.GetButtonGlyphMargins.Left +
        Condition.GetButtonGlyphSize.cx + Condition.GetButtonGlyphMargins.Right
    else
    begin
      Control.Canvas.Font.Assign(Control.FontValue);
      ALeft := IfThen(Control.UseRightToLeftAlignment,
        ALeft - Control.Canvas.TextWidth(')0'),
        ALeft + Control.Canvas.TextWidth(')0'));
      ASize.cx := Control.Canvas.TextWidth('000');
    end;
    ASize.cx := dxGetTouchableSize(ASize.cx, Control.ScaleFactor);
    ASize.cy := HeightOf(Condition.RowRect) - Control.ScaleFactor.Apply
      (IfThen(cxIsTouchModeEnabled, 12, 4));
    FAddButtonRect := Bounds(ALeft, 0, ASize.cx, ASize.cy);
    CenterRectVert(Condition.RowRect, FAddButtonRect);
    FAddButtonGlyphRect := cxRectCenter(FAddButtonRect,
      Condition.GetButtonGlyphSize);
    // get ButtonState
    if not Control.ViewInfo.Enabled then
      FAddButtonState := cxbsDisabled
    else
      with Control.FFocusedInfo do
        if (Row = Condition) and (HitTest = fhtAddValue) then
          if Control.UseTokens then
            FAddButtonState := cxbsPressed
          else
            FAddButtonState := cxbsDefault
        else
          with Control.FHotTrack do
            if AHotTrack and (Row = Condition) and (HitTest = fhtAddValue) then
              FAddButtonState := cxbsHot
            else
              FAddButtonState := cxbsNormal;
  end
  else
    FAddButtonRect := cxEmptyRect;
end;

procedure TcxValuesViewInfo.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Values[I].Free;
  FList.Clear;
end;

procedure TcxValuesViewInfo.GetHitTestInfo(const P: TPoint;
  var HitInfo: TcxFilterControlHitTestInfo);
var
  I: Integer;
begin
  if HasAddButton and PtInRect(AddButtonRect, P) then
    HitInfo.HitTest := fhtAddValue
  else
    for I := 0 to Count - 1 do
      if PtInRect(Values[I].ValueRect, P) then
      begin
        HitInfo.HitTest := fhtValue;
        HitInfo.ValueIndex := I;
        break;
      end;
end;

procedure TcxValuesViewInfo.RemoveValue(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FList.Count) then
    Exit;
  Values[AIndex].Free;
  FList.Delete(AIndex);
end;

function TcxValuesViewInfo.CalculateValueState(AIndex: Integer): TcxButtonState;
begin
  if not Control.ViewInfo.Enabled then
    Result := cxbsDisabled
  else if (Control.FocusedRow = Condition) and
    (Control.FocusedInfo.HitTest = fhtValue) and
    (Control.FocusedInfo.ValueIndex = AIndex) then
    Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Condition) and
    (Control.HotTrack.HitTest = fhtValue) and
    (Control.HotTrack.ValueIndex = AIndex) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxValuesViewInfo.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxValuesViewInfo.HasAddButton: Boolean;
begin
  Result := not IsRectEmpty(AddButtonRect);
end;

procedure TcxValuesViewInfo.UpdateEditorStyle(AStyle: TcxCustomEditStyle;
  AHighlighted, AEnabled: Boolean);
const
  ButtonTransparency: array [Boolean] of TcxEditButtonTransparency =
    (ebtHideInactive, ebtNone);
begin
  if AEnabled then
    if Control.UseTokens then
      AStyle.StyleData.FontColor := Control.ViewInfo.TokenParams.ValueTextColor
    else
      AStyle.StyleData.FontColor := Control.FontValue.Color
  else
    AStyle.StyleData.FontColor := clBtnShadow;
  AStyle.ButtonTransparency := ButtonTransparency
    [AHighlighted and not Control.UseTokens];
  if AHighlighted then
    AStyle.Color := clWindow
  else
    AStyle.Color := Control.ViewInfo.Painter.ContentColor;
end;

function TcxValuesViewInfo.GetControl: TcxCustomFilterControl;
begin
  Result := Condition.Control;
end;

function TcxValuesViewInfo.GetValue(Index: Integer): TcxValueInfo;
begin
  Result := TcxValueInfo(FList[Index]);
end;

function TcxValuesViewInfo.GetWidth: Integer;
begin
  case Condition.Operator of
    fcoBetween, fcoNotBetween:
      begin
        if not Control.UseRightToLeftAlignment then
          Result := Values[Count - 1].ValueRect.Right -
            Condition.OperatorRect.Right
        else
          Result := Condition.OperatorRect.Left - Values[Count - 1]
            .ValueRect.Left;
      end;
    fcoInList, fcoNotInList:
      begin
        if not Control.UseRightToLeftAlignment then
          Result := FAddButtonRect.Right - Condition.OperatorRect.Right
        else
          Result := Condition.OperatorRect.Left - FAddButtonRect.Left;
      end
  else
    if Condition.HasDisplayValues and (Condition.Expression = '') then
    begin
      if not Control.UseRightToLeftAlignment then
        Result := Values[Count - 1].ValueRect.Right -
          Condition.OperatorRect.Right
      else
        Result := Condition.OperatorRect.Left - Values[Count - 1]
          .ValueRect.Left;
    end
    else
      Result := 0;
  end;
end;

{ TcxConditionViewInfo }

constructor TcxConditionViewInfo.Create(AControl: TcxCustomFilterControl;
  AParent: TcxCustomRowViewInfo; ACriteriaItem: TcxCustomFilterCriteriaItem);
var
  I, J: Integer;
  S: string;
begin
  inherited Create(AControl, AParent, ACriteriaItem);
  FValues := TcxValuesViewInfo.Create(Self);
  if ACriteriaItem <> nil then
  begin
    FItemLink := TcxFilterCriteriaItem(ACriteriaItem).ItemLink;
    FItemIndex := GetItemIndex;
    if Control.HasItems and (FItemIndex >= 0) and
      (FItemIndex < Control.FilterLink.Count) then
      ItemText := Control.FilterLink.Captions[FItemIndex]
    else
      FilterControlError(cxGetResourceString(@cxSFilterErrorBuilding));
    with TcxFilterCriteriaItem(ACriteriaItem) do
    begin
      FOperator := GetFilterControlOperator(OperatorKind, ValueIsNull(Value) and
        (Expression = ''));
      case FOperator of
        fcoBetween, fcoNotBetween, fcoInList, fcoNotInList:
          begin
            S := DisplayValue;
            J := 1;
            for I := VarArrayLowBound(Value, 1) to VarArrayHighBound
              (Value, 1) do
            begin
              FValues.AddValue;
              FValues[I].Value := Value[I];
              FValues[I].ValueText := ExtractFilterDisplayValue(S, J);
            end;
          end;
      else
        begin
          FValues.AddValue;
          FValues[0].Value := Value;
          FValues[0].ValueText := DisplayValue;
        end;
      end;
      Self.FExpression := Expression;
    end;
    InternalInit;
  end
  else
  begin
    FItemIndex := -1;
    SetItem(-1);
  end;
end;

destructor TcxConditionViewInfo.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FEditorProperties);
  FreeAndNil(ValueEditorData);
  inherited Destroy;
end;

procedure TcxConditionViewInfo.Calc(const ARowRect: TRect);
begin
  inherited Calc(ARowRect);
  if not Control.UseTokens then
    CalculateButton;
  CalculateItem;
  CalculateOperator;
  Values.Calc;
  if Expression <> '' then
    CalculateExpression;
  if CanShowExpressionButton then
    CalculateExpressionButton;
  if Control.UseTokens then
    CalculateRemoveButton;
  CalculateContentRect;
end;

procedure TcxConditionViewInfo.GetHitTestInfo(const P: TPoint;
  var HitInfo: TcxFilterControlHitTestInfo);
begin
  inherited GetHitTestInfo(P, HitInfo);
  if not(HitInfo.HitTest in [fhtNone, fhtRowContent]) then
    Exit;
  if PtInRect(ItemRect, P) then
    HitInfo.HitTest := fhtItem
  else if PtInRect(OperatorRect, P) then
    HitInfo.HitTest := fhtOperator
  else if HasExpressionButton and PtInRect(ExpressionButtonRect, P) then
    HitInfo.HitTest := fhtExpressionButton
  else if HasDisplayValues and not IsExpressionVisible then
    Values.GetHitTestInfo(P, HitInfo);
end;

function TcxConditionViewInfo.GetProperties: TcxCustomEditProperties;
begin
  with Control do
  begin
    if HasItems and (ItemIndex >= 0) and (ItemIndex < FilterLink.Count) then
      Result := FilterLink.Properties[ItemIndex]
    else
      Result := GetDefaultProperties;
    if Result = nil then
      Result := GetDefaultProperties;
  end;
end;

function TcxConditionViewInfo.Ready: Boolean;
begin
  Result := (FItemText <> '') and
    ((FOperatorText <> '') or NeedDrawOperatorAsImage);
end;

procedure TcxConditionViewInfo.CalculateItem;
begin
  FItemRect := CalculateItemRect;
  FItemState := CalculateItemState;
end;

function TcxConditionViewInfo.CalculateItemRect: TRect;
var
  ASize: TSize;
  ALeft: Integer;
  AMargins: TRect;
begin
  Control.Canvas.Font.Assign(Control.FontItem);
  AMargins := Control.ViewInfo.GetTextMargins(fhtItem);
  ASize := Control.Canvas.TextExtent(ItemText);
  ASize := cxSize(ASize.cx + AMargins.Left + AMargins.Right,
    ASize.cy + AMargins.Top + AMargins.Bottom);
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetItemStartBound - ASize.cx,
    GetItemStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(RowRect, Result);
end;

function TcxConditionViewInfo.CalculateItemState: TcxButtonState;
begin
  if not Control.ViewInfo.Enabled then
    Result := cxbsDisabled
  else if (Control.FocusedRow = Self) and (Control.FocusedInfo.HitTest = fhtItem)
  then
    if Control.DropDownMenu.Active then
      Result := cxbsPressed
    else
      Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest = fhtItem) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxConditionViewInfo.GetItemStartBound: Integer;
begin
  if Control.UseTokens then
    Result := GetContentStartBound
  else
    Result := IfThen(Control.UseRightToLeftAlignment,
      ButtonRect.Left - GetElementsIndent, ButtonRect.Right +
      GetElementsIndent);
end;

procedure TcxConditionViewInfo.CalculateOperator;
begin
  if not NeedDrawOperatorAsImage then
    FOperatorText := CalculateOperatorText;
  FOperatorRect := CalculateOperatorRect;
  FOperatorState := CalculateOperatorState;
end;

function TcxConditionViewInfo.GetOperatorIndent: Integer;
begin
  Result := IfThen(NeedDrawOperatorAsImage, 0, GetElementsIndent);
end;

function TcxConditionViewInfo.CalculateOperatorRect: TRect;
var
  ASize: TSize;
  ALeft: Integer;
  AMargins: TRect;
begin
  if NeedDrawOperatorAsImage then
    ASize := TcxFilterControlImagesHelper.GetScaledSize(ScaleFactor)
  else
  begin
    Control.Canvas.Font.Assign(Control.FontCondition);
    AMargins := Control.ViewInfo.GetTextMargins(fhtOperator);
    ASize := Control.Canvas.TextExtent(OperatorText);
    ASize := cxSize(ASize.cx + AMargins.Left + AMargins.Right,
      ASize.cy + AMargins.Top + AMargins.Bottom);
  end;
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetOperatorStartBound -
    ASize.cx, GetOperatorStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(RowRect, Result);
end;

function TcxConditionViewInfo.CalculateOperatorState: TcxButtonState;
begin
  if not Control.ViewInfo.Enabled then
    Result := cxbsDisabled
  else if (Control.FocusedRow = Self) and
    (Control.FocusedInfo.HitTest = fhtOperator) then
    if Control.DropDownMenu.Active then
      Result := cxbsPressed
    else
      Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest = fhtOperator) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxConditionViewInfo.CalculateOperatorText: string;
begin
  Result := cxGetConditionText(&Operator);
end;

function TcxConditionViewInfo.GetOperatorStartBound: Integer;
begin
  Result := IfThen(Control.UseRightToLeftAlignment,
    ItemRect.Left - GetOperatorIndent, ItemRect.Right + GetOperatorIndent);
end;

function TcxConditionViewInfo.NeedDrawOperatorAsImage: Boolean;
begin
  Result := Control.UseTokens and (&Operator in [fcoEqual .. fcoGreaterEqual]);
end;

procedure TcxConditionViewInfo.CalculateExpression;
begin
  FExpressionRect := CalculateExpressionRect;
end;

function TcxConditionViewInfo.CalculateExpressionRect: TRect;
var
  ASize: TSize;
  ALeft: Integer;
begin
  Control.Canvas.Font.Assign(Control.FontExpression);
  ASize := Control.Canvas.TextExtent(Expression);
  ASize := cxSize(ASize.cx + Control.ScaleFactor.Apply(2),
    ASize.cy + Control.ScaleFactor.Apply(2));
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetExpressionStartBound -
    ASize.cx, GetExpressionStartBound);
  Result := Bounds(ALeft, 0, ASize.cx, ASize.cy);
  CenterRectVert(RowRect, Result);
end;

function TcxConditionViewInfo.GetExpressionStartBound: Integer;
begin
  Result := IfThen(Control.UseRightToLeftAlignment,
    OperatorRect.Left - GetElementsIndent, OperatorRect.Right +
    GetElementsIndent)
end;

function TcxConditionViewInfo.IsExpressionVisible: Boolean;
begin
  Result := not IsRectEmpty(ExpressionRect);
end;

procedure TcxConditionViewInfo.CalculateExpressionButton;
begin
  FExpressionButtonRect := CalculateExpressionButtonRect;
  FExpressionButtonState := CalculateExpressionButtonState;
end;

function TcxConditionViewInfo.CalculateExpressionButtonRect: TRect;
var
  ALeft, AWidth: Integer;
begin
  Control.Canvas.Font.Assign(Control.FontValue);
  AWidth := Control.Canvas.TextWidth('0') * Length(cxExpressionButtonText);
  dxAdjustToTouchableSize(AWidth, Control.ScaleFactor);
  ALeft := IfThen(Control.UseRightToLeftAlignment, GetExpresionButtonStartBound
    - AWidth, GetExpresionButtonStartBound);
  Result := Bounds(ALeft, 0, AWidth, HeightOf(RowRect) -
    Control.ScaleFactor.Apply(IfThen(cxIsTouchModeEnabled, 12, 4)));
  CenterRectVert(RowRect, Result);
end;

function TcxConditionViewInfo.CalculateExpressionButtonState: TcxButtonState;
begin
  if (Control.FocusedInfo.Row = Self) and
    (Control.FocusedInfo.HitTest = fhtExpressionButton) then
    Result := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.Row = Self) and
    (Control.HotTrack.HitTest = fhtExpressionButton) then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TcxConditionViewInfo.CanShowExpressionButton: Boolean;
begin
  Result := (Control.FilterLinkEx <> nil) and
    Control.FilterLinkEx.AllowExpressionEditing and HasDisplayValues and
    IsOperatorSupportedForExpression;
end;

function TcxConditionViewInfo.GetExpresionButtonStartBound: Integer;
var
  AIndent: Integer;
  ANeighbourRect: TRect;
begin
  if IsExpressionVisible then
    ANeighbourRect := ExpressionRect
  else
    ANeighbourRect := Values[Values.Count - 1].ValueRect;
  AIndent := Control.Canvas.TextWidth(')');
  Result := IfThen(Control.UseRightToLeftAlignment, ANeighbourRect.Left -
    AIndent, ANeighbourRect.Right + AIndent);
end;

function TcxConditionViewInfo.HasExpressionButton: Boolean;
begin
  Result := not IsRectEmpty(ExpressionButtonRect);
end;

procedure TcxConditionViewInfo.AddValue;
begin
  Values.AddValue;
  Control.LayoutChanged;
end;

function TcxConditionViewInfo.GetContentFinishBound: Integer;
var
  ALastRect: TRect;
begin
  if Control.UseTokens then
    Result := inherited GetContentFinishBound
  else
  begin
    if not HasDisplayValues then
      ALastRect := OperatorRect
    else if HasExpressionButton then
      ALastRect := ExpressionButtonRect
    else if Expression <> '' then
      ALastRect := ExpressionRect
    else if Values.HasAddButton then
      ALastRect := Values.AddButtonRect
    else
      ALastRect := Values[Values.Count - 1].ValueRect;
    Result := IfThen(Control.UseRightToLeftAlignment, ALastRect.Left,
      ALastRect.Right);
  end;
end;

function TcxConditionViewInfo.GetRemoveButtonStartBound: Integer;
var
  ANeighbourRect: TRect;
begin
  if HasExpressionButton then
    ANeighbourRect := ExpressionButtonRect
  else if IsExpressionVisible then
    ANeighbourRect := ExpressionRect
  else if not HasDisplayValues then
    ANeighbourRect := OperatorRect
  else if Values.HasAddButton then
    ANeighbourRect := Values.AddButtonRect
  else
    ANeighbourRect := Values[Values.Count - 1].ValueRect;
  Result := IfThen(Control.UseRightToLeftAlignment, ANeighbourRect.Left,
    ANeighbourRect.Right);
end;

function TcxConditionViewInfo.GetWidth: Integer;
begin
  Result := inherited GetWidth + WidthOf(FItemRect) + GetOperatorIndent +
    WidthOf(FOperatorRect);
  if IsExpressionVisible then
    Inc(Result, WidthOf(ExpressionRect))
  else
    Inc(Result, Values.Width);
  if HasExpressionButton then
    Inc(Result, WidthOf(ExpressionButtonRect));
end;

function TcxConditionViewInfo.HasDisplayValues: Boolean;
begin
  Result := not(Operator in [fcoBlanks, fcoNonBlanks, fcoYesterday, fcoToday,
    fcoTomorrow, fcoLast7Days, fcoLastWeek, fcoLast14Days, fcoLastTwoWeeks,
    fcoLast30Days, fcoLastMonth, fcoLastYear, fcoInPast, fcoThisWeek,
    fcoThisMonth, fcoThisYear, fcoNext7Days, fcoNextWeek, fcoNext14Days,
    fcoNextTwoWeeks, fcoNext30Days, fcoNextMonth, fcoNextYear, fcoInFuture]);
end;

procedure TcxConditionViewInfo.InitValues(ASaveValue: Boolean);
begin
  if ASaveValue and HasDisplayValues and (Expression = '') then
  begin
    case Operator of
      fcoBetween, fcoNotBetween:
        begin
          while Values.Count > 2 do
            Values.RemoveValue(2);
          if Values.Count < 2 then
            Values.AddValue;
        end;
      fcoInList, fcoNotInList:
        ;
    else
      begin
        while Values.Count > 1 do
          Values.RemoveValue(1);
        if Values.Count < 1 then
          Values.AddValue;
      end;
    end;
  end
  else
  begin
    Values.Clear;
    if Expression = '' then
    begin
      Values.AddValue;
      if Operator in [fcoBetween, fcoNotBetween] then
        Values.AddValue;
    end;
  end;
end;

procedure TcxConditionViewInfo.InternalInit;
var
  AEditClass: TcxCustomEditClass;
begin
  FreeAndNil(ValueEditorData);
  FreeAndNil(FEditorProperties);
  FProperties := GetProperties;
  with Control do
  begin
    FEditorHelper := FilterEditsController.FindHelper(Properties.ClassType);
    if FEditorHelper = nil then
      FEditorHelper := TcxFilterTextEditHelper;

    AEditClass := FEditorHelper.GetFilterEditClass;
    if AEditClass <> nil then
      FEditorProperties := AEditClass.GetPropertiesClass.Create(nil)
    else
      FEditorProperties := TcxCustomEditPropertiesClass(Properties.ClassType)
        .Create(nil);

    FEditorHelper.InitializeProperties(FEditorProperties, Properties, True);
    FValueType := FilterLink.ValueTypes[FItemIndex];
    UpdateSupportedOperators;
  end;
  ValidateConditions;
end;

function TcxConditionViewInfo.IsOperatorSupportedForExpression: Boolean;
begin
  Result := IsOperatorSupportedForExpression(Operator);
end;

function TcxConditionViewInfo.IsOperatorSupportedForExpression
  (AOperator: TcxFilterControlOperator): Boolean;
begin
  Result := cxFilter.IsOperatorSupportedForExpression
    (GetFilterOperatorKind(AOperator));
end;

procedure TcxConditionViewInfo.ResetCalculatedInfo;
begin
  inherited ResetCalculatedInfo;
  FExpressionRect := cxEmptyRect;
  FExpressionButtonRect := cxEmptyRect;
  FExpressionButtonState := cxbsNormal;
  FOperatorText := '';
end;

procedure TcxConditionViewInfo.SetItem(AIndex: Integer);
var
  AOperator: TcxFilterControlOperator;
  I: Integer;
  L: TStringList;
begin
  with Control, Control.FilterLink do
  begin
    if (AIndex >= 0) and (AIndex = ItemIndex) then
      Exit;
    if AIndex < 0 then
    begin
      AIndex := 0;
      if FSortItems then
      begin
        L := TStringList.Create;
        try
          L.BeginUpdate;
          for I := 0 to Count - 1 do
            L.AddObject(Captions[I], TObject(I));
          L.Sort;
          L.EndUpdate;
          if L.Count > 0 then
            AIndex := Integer(L.Objects[0]);
        finally
          L.Free;
        end;
      end;
    end;
    FItemIndex := AIndex;
    FItemLink := ItemLinks[AIndex];
    ItemText := Captions[AIndex];
    InternalInit;
    FExpression := '';
    if fcoLike in FSupportedOperators then
      FOperator := fcoLike
    else
      for AOperator := Low(TcxFilterControlOperator)
        to High(TcxFilterControlOperator) do
        if AOperator in FSupportedOperators then
        begin
          FOperator := AOperator;
          break;
        end;
    InitValues(False);
    Values.Calc;
    if FValueEditor <> nil then
      FInplaceEditors.RemoveItem(FValueEditor.ActiveProperties);
  end;
end;

procedure TcxConditionViewInfo.UpdateSupportedOperators;
var
  AOperator: TcxFilterControlOperator;
begin
  FSupportedOperators := FEditorHelper.GetSupportedFilterOperators(Properties,
    FValueType, True);
  if Expression <> '' then
    for AOperator := Low(TcxFilterControlOperator)
      to High(TcxFilterControlOperator) do
      if not IsOperatorSupportedForExpression(AOperator) or
        (AOperator in [fcoBlanks, fcoNonBlanks]) then
        FSupportedOperators := FSupportedOperators - [AOperator];
end;

procedure TcxConditionViewInfo.ValidateConditions;
begin
  Control.ValidateConditions(FSupportedOperators);
end;

function TcxConditionViewInfo.GetItemIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  with Control do
    for I := 0 to FilterLink.Count - 1 do
      if FilterLink.ItemLinks[I] = FItemLink then
      begin
        Result := I;
        break;
      end;
end;

procedure TcxConditionViewInfo.SetExpression(const AValue: string);
begin
  if Expression <> AValue then
  begin
    FExpression := AValue;
    InitValues(False);
    UpdateSupportedOperators;
  end;
end;

procedure TcxConditionViewInfo.SetItemText(const AValue: string);
begin
  FItemText := AValue;
  if Control.UseTokens and (FItemText = '') then
    FItemText := QuotedStr('');
end;

{ TcxFilterControlImagesHelper }

class procedure TcxFilterControlImagesHelper.DrawOperatorImage(ACanvas: TCanvas;
  AOperator: TcxFilterControlOperator; const ABounds: TRect;
  APainter: TcxCustomLookAndFeelPainter; AAccentColor: TdxAlphaColor);
begin
  DrawImage(ACanvas, GetOperatorImageIndex(AOperator), ABounds, APainter,
    AAccentColor);
end;

class function TcxFilterControlImagesHelper.GetScaledSize
  (AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := dxGetImageSize(nil, FilterControlImages, 0, AScaleFactor);
end;

class function TcxFilterControlImagesHelper.GetSize: TSize;
begin
  Result := GetScaledSize(dxSystemScaleFactor);
end;

class procedure TcxFilterControlImagesHelper.CreateImages;
var
  AResourceStream: TResourceStream;
begin
  FColorProvider := TdxAdvancedColorPalette.Create;
  FFilterControlImages := TcxImageList.CreateSize(16, 16);
  AResourceStream := TResourceStream.Create(HInstance,
    'FILTERCONTROLOPERATORIMAGES', RT_RCDATA);
  try
    LoadSvgImages(FilterControlImages, AResourceStream);
  finally
    AResourceStream.Free;
  end;
  AResourceStream := TResourceStream.Create(HInstance,
    'FILTERCONTROLGROUPIMAGES', RT_RCDATA);
  try
    LoadPngImages(FilterControlImages, AResourceStream);
  finally
    AResourceStream.Free;
  end;
end;

class procedure TcxFilterControlImagesHelper.DestroyImages;
begin
  FreeAndNil(FFilterControlImages);
end;

class procedure TcxFilterControlImagesHelper.DrawImage(ACanvas: TCanvas;
  AIndex: Integer; const ABounds: TRect; APainter: TcxCustomLookAndFeelPainter;
  AAccentColor: TdxAlphaColor);
var
  APalette: IdxColorPalette;
begin
  APalette := APainter.FilterControlMenuGetColorPalette;
  if (APalette = nil) and (AAccentColor <> TdxAlphaColors.Empty) then
  begin
    TdxAdvancedColorPalette(FColorProvider).FillColors['Black'] := AAccentColor;
    APalette := FColorProvider;
  end;
  DrawImage(ACanvas, AIndex, ABounds, APalette);
end;

class procedure TcxFilterControlImagesHelper.DrawImage(ACanvas: TCanvas;
  AIndex: Integer; const ABounds: TRect; APalette: IdxColorPalette);
begin
  cxDrawImage(ACanvas.Handle, ABounds, ABounds, nil, FilterControlImages,
    AIndex, idmNormal, False, 0, clNone, True, APalette);
end;

class function TcxFilterControlImagesHelper.GetOperatorImageIndex
  (AOperator: TcxFilterControlOperator): Integer;
const
  ImageMap: array [TcxFilterControlOperator] of Integer = (-1, 7, 5, 12, 13, 8,
    9, 14, 17, 3, 4, 1, 6, 11, 10, 2, 16, 0, 15, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
begin
  Result := ImageMap[AOperator];
end;

class function TcxFilterControlImagesHelper.GetFilterControlImages
  : TcxImageList;
begin
  if FFilterControlImages = nil then
    CreateImages;
  Result := FFilterControlImages;
end;

class procedure TcxFilterControlImagesHelper.LoadSvgImages
  (AImageList: TcxImageList; AStream: TStream);
var
  AZipData: TdxZIPStreamReader;
  ASvgStream: TMemoryStream;
  I: Integer;
  AImage: TdxSmartImage;
begin
  AZipData := TdxZIPStreamReader.Create(AStream);
  try
    for I := 0 to AZipData.Files.Count - 1 do
    begin
      ASvgStream := TMemoryStream.Create;
      try
        AZipData.Extract(AZipData.Files[I], ASvgStream);
        AImage := TdxSmartImage.Create;
        try
          ASvgStream.Position := 0;
          AImage.LoadFromStream(ASvgStream);
          AImageList.Add(AImage);
        finally
          AImage.Free;
        end;
      finally
        ASvgStream.Free;
      end;
    end;
  finally
    AZipData.Free;
  end;
end;

class procedure TcxFilterControlImagesHelper.LoadPngImages
  (AImageList: TcxImageList; AStream: TStream);
var
  AImage, AAllImages: TdxSmartImage;
  ACanvas: TdxGPCanvas;
  R: TRect;
  I: Integer;
begin
  AAllImages := TdxSmartImage.Create;
  try
    AAllImages.LoadFromStream(AStream);
    R.InitSize(0, 0, 13, 13);
    for I := 0 to 7 do
    begin
      AImage := TdxSmartImage.CreateSize(AImageList.Width, AImageList.Height);
      try
        ACanvas := AImage.CreateCanvas;
        try
          AAllImages.StretchDraw(ACanvas,
            TRect.CreateSize((AImage.Width - R.Width) div 2,
            (AImage.Height - R.Height) div 2, R.Width, R.Height), R, nil);
          AImageList.Add(AImage);
          R.Offset(R.Width, 0);
        finally
          ACanvas.Free;
        end;
      finally
        AImage.Free;
      end;
    end;
  finally
    AAllImages.Free;
  end;
end;

{ TcxFilterDropDownMenuInnerListBox }

function TcxFilterDropDownMenuInnerListBox.CheckAccelerators
  (AKey: Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if IsAccel(AKey, Items[I].Caption) then
    begin
      ItemIndex := I;
      DoSelectItem(True);
      Result := True;
      break;
    end;
end;

function TcxFilterDropDownMenuInnerListBox.GetTextFlags: Integer;
begin
  if ShowShortCut then
    Result := inherited GetTextFlags or cxShowPrefix
  else
  begin
    Result := CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or CXTO_END_ELLIPSIS;
    if UseRightToLeftAlignment then
      Result := Result or CXTO_RIGHT;
    if UseRightToLeftReading then
      Result := Result or CXTO_RTLREADING;
  end;
end;

function TcxFilterDropDownMenuInnerListBox.IsIncSearch: Boolean;
begin
  Result := IncrementalSearch and (SearchText <> '') and (Count > 0);
end;

function TcxFilterDropDownMenuInnerListBox.IsIncSearchChar(AChar: Char)
  : Boolean;
begin
  Result := (AChar >= ' ') or (AChar = #8);
end;

procedure TcxFilterDropDownMenuInnerListBox.ClearIncrementalSearch;
begin
  if SearchText <> '' then
  begin
    inherited ClearIncrementalSearch;
    Invalidate;
  end;
end;

function TcxFilterDropDownMenuInnerListBox.DoIncrementalSearch
  (var Key: Char): Boolean;
var
  ASearchText: string;
begin
  if ShowShortCut then
    Result := inherited DoIncrementalSearch(Key)
  else
  begin
    if Key = #8 then
    begin
      ASearchText := Copy(SearchText, 1, Length(SearchText) - 1);
      if ASearchText = '' then
      begin
        ClearIncrementalSearch;
        Result := True;
        Exit;
      end;
    end
    else
      ASearchText := SearchText + Key;
    Result := FocusItemWithText(ASearchText, 0, Count - 1);
    if Result then
    begin
      SearchText := ASearchText;
      Invalidate;
    end
    else if (Key = ' ') and Items.IsValidIndex(ItemIndex) then
    begin
      DoSelectItem(True);
      Result := True;
    end
    else
      Beep;
  end;
end;

procedure TcxFilterDropDownMenuInnerListBox.DrawItemImage(const R: TRect;
  AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AImageRect: TRect;
begin
  if IsImageAssigned(Images, AItem.ImageIndex) then
  begin
    AImageRect := cxRectCenter(R, ImageSize);
    TcxFilterControlImagesHelper.DrawImage(Canvas.Canvas, AItem.ImageIndex,
      AImageRect, LookAndFeelPainter,
      TdxAlphaColors.FromColor(GetTextColor(AItem, AState)));
  end;
end;

procedure TcxFilterDropDownMenuInnerListBox.DrawItemText(const R: TRect;
  AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  ABounds: TRect;
begin
  if ShowShortCut then
    inherited DrawItemText(R, AItem, AState)
  else
  begin
    ABounds := R;
    if IsIncSearch and AnsiStartsText(SearchText, AItem.Caption) then
    begin
      cxTextOut(Canvas.Canvas, AItem.Caption, ABounds, GetTextFlags, 0,
        Length(SearchText), Font, LookAndFeelPainter.DefaultSelectionColor,
        LookAndFeelPainter.DefaultSelectionTextColor, 0, 0, 0,
        GetTextColor(AItem, AState))
    end
    else
      cxTextOut(Canvas.Canvas, AItem.Caption, ABounds, GetTextFlags, Font, 0, 0,
        0, GetTextColor(AItem, AState));
  end;
end;

function TcxFilterDropDownMenuInnerListBox.ProcessKeyPress
  (var Key: Char): Boolean;
begin
  if (Key = ' ') and not IsIncSearch then
  begin
    DoSelectItem(True);
    Result := True;
  end
  else
    Result := (ShowShortCut and CheckAccelerators(Ord(Key))) or
      inherited ProcessKeyPress(Key);
end;

function TcxFilterDropDownMenuInnerListBox.ProcessNavigationKey(var Key: Word;
  Shift: TShiftState): Boolean;

  procedure ProcessNextItemWithText(AStartIndex, AFinishIndex: Integer);
  begin
    if FocusItemWithText(SearchText, AStartIndex, AFinishIndex) then
      Invalidate
    else
      Beep;
  end;

begin
  if (ssCtrl in Shift) and IsIncSearch then
  begin
    Result := False;
    case Key of
      VK_UP:
        ProcessNextItemWithText(FindNextItemIndex(ItemIndex - 1, False,
          LoopedNavigation), 0);
      VK_DOWN:
        ProcessNextItemWithText(FindNextItemIndex(ItemIndex + 1, True,
          LoopedNavigation), Count - 1);
    end;
    Key := 0;
  end
  else
    Result := inherited ProcessNavigationKey(Key, Shift);
end;

{ TcxFilterControlDropDownMenu }

constructor TcxFilterDropDownMenu.Create(AControl: TcxControl);
begin
  inherited CreateEx(AControl, AControl);
  DisplayRowsCount := cxFilterControlMaxDropDownRows;
  Images := TcxFilterControlImagesHelper.FilterControlImages;
end;

procedure TcxFilterDropDownMenu.Popup(const AForBounds: TRect;
  const ACaption: string = ''; AKey: Char = #0);
begin
  InnerListBox.ClearIncrementalSearch;
  if not InnerListBox.CanStartIncSearch(AKey) then
    ItemIndex := Items.IndexOfCaption(ACaption);
  PopupForBounds(AForBounds, AKey);
end;

function TcxFilterDropDownMenu.CreateInnerListBox
  : TdxCustomDropDownInnerListBox;
begin
  Result := TcxFilterDropDownMenuInnerListBox.Create(nil);
end;

procedure TcxFilterDropDownMenu.CreateConditionList(ASupportedOperators
  : TcxFilterControlOperators);
var
  AImageIndex: Integer;
  AOperator: TcxFilterControlOperator;
  AText: string;
  AData: TObject;
begin
  BeginUpdate;
  try
    Items.Clear;
    for AOperator := Low(AOperator) to High(AOperator) do
      if AOperator in ASupportedOperators then
      begin
        AImageIndex := TcxFilterControlImagesHelper.GetOperatorImageIndex
          (AOperator);
        AText := GetFilterControlOperatorText(AOperator);
        AData := TObject(AOperator);
        Items.Add(AText, AImageIndex, AData);
      end;
  finally
    EndUpdate;
  end;
end;

function TcxFilterDropDownMenu.GetInnerListBox
  : TcxFilterDropDownMenuInnerListBox;
begin
  Result := TcxFilterControlDropDownMenuInnerListBox(inherited InnerListBox);
end;

{ TcxFilterControlDropDownMenu }

constructor TcxFilterControlDropDownMenu.Create
  (AControl: TcxCustomFilterControl);
begin
  inherited Create(AControl);
  FControl := AControl;
end;

procedure TcxFilterControlDropDownMenu.Popup(const AForBounds: TRect;
  const ACaption: string = ''; AKey: Char = #0);
begin
  SaveDroppedInfo;
  TcxFilterControlDropDownMenuInnerListBox(InnerListBox).ShowShortCut :=
    FDroppedInfo.HitTest = fhtButton;
  inherited Popup(AForBounds, ACaption, AKey);
end;

function TcxFilterControlDropDownMenu.CreateInnerListBox
  : TdxCustomDropDownInnerListBox;
begin
  Result := TcxFilterControlDropDownMenuInnerListBox.Create(nil);
end;

procedure TcxFilterControlDropDownMenu.DoSelectItem(AItem: TdxCustomListBoxItem;
  ASelectedViaKeyboard: Boolean);
begin
  inherited DoSelectItem(AItem, ASelectedViaKeyboard);
  Control.DropDownMenuItemClick(AItem.Index);
end;

function TcxFilterControlDropDownMenu.IsSameDroppedInfo
  (AInfo: TcxFilterControlHitTestInfo): Boolean;
begin
  Result := (FDroppedInfo.Row = AInfo.Row) and
    (FDroppedInfo.HitTest = AInfo.HitTest);
end;

procedure TcxFilterControlDropDownMenu.CreateActionMenu;
begin
  BeginUpdate;
  try
    Items.Clear;
    Items.Add(cxGetResourceString(@cxSFilterAddCondition), 22);
    Items.Add(cxGetResourceString(@cxSFilterAddGroup), 23);
    if Control.UseTokens then
      Exit;
    if Control.FocusedRow <> Control.FRoot then
    begin
      Items.AddSeparator;
      Items.Add(cxGetResourceString(@cxSFilterRemoveRow), 25);
    end
    else if Control.FRoot.Group.RowCount > 0 then
    begin
      Items.AddSeparator;
      Items.Add(cxGetResourceString(@cxSFilterClearAll), 24);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterControlDropDownMenu.CreateItemList(AList: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    Items.Clear;
    for I := 0 to AList.Count - 1 do
      Items.Add(AList[I], -1, AList.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterControlDropDownMenu.CreateBoolOperatorList;
const
  ImageMap: array [fboAnd .. fboNotOr] of Integer = (18, 19, 20, 21);
var
  ABoolOperator: TcxFilterBoolOperatorKind;
begin
  BeginUpdate;
  try
    Items.Clear;
    for ABoolOperator := fboAnd to fboNotOr do
      Items.Add(cxBoolOperatorText[ABoolOperator], ImageMap[ABoolOperator],
        TObject(ABoolOperator));
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterControlDropDownMenu.DoCloseUp(AClosedViaKeyboard: Boolean);
begin
  inherited DoCloseUp(AClosedViaKeyboard);
  PostMessage(Control.Handle, DXWM_FILTERCONTROL_DROPDOWNMENUCLOSED, 0, 0);
  Control.State := fcsNormal;
end;

procedure TcxFilterControlDropDownMenu.ClearDroppedInfo;
begin
  FDroppedInfo.Row := nil;
  FDroppedInfo.HitTest := fhtNone;
end;

function TcxFilterControlDropDownMenu.GetInnerListBox
  : TcxFilterControlDropDownMenuInnerListBox;
begin
  Result := TcxFilterControlDropDownMenuInnerListBox(inherited InnerListBox);
end;

procedure TcxFilterControlDropDownMenu.SaveDroppedInfo;
begin
  FDroppedInfo := Control.FocusedInfo;
end;

{ TcxCustomFilterControl }

constructor TcxCustomFilterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WantTabs := False;
  FCriteria := GetFilterControlCriteriaClass.Create(Self);
  FRoot := TcxGroupViewInfo.Create(Self, nil, FCriteria.Root);
  FRows := TList.Create;
  FRows.Add(FRoot);
  FViewInfo := GetViewInfoClass.Create(Self);
  CreateFonts;
  CreateInternalControls;
  UpdateBoundsRect(Rect(0, 0, 300, 200));
  FFocusedInfo.Row := FRoot;
  FFocusedInfo.HitTest := fhtBoolOperator;
  FHotTrackOnUnfocused := True;
  FNullstring := cxGetResourceString(@cxSFilterControlNullString);
  FShowLevelLines := True;
  cxFormatController.AddListener(Self);
  ParentColor := False;
  Color := clBtnFace;
end;

destructor TcxCustomFilterControl.Destroy;
var
  AFont: TcxFilterControlFont;
begin
  cxFormatController.RemoveListener(Self);
  EndMouseTracking(Self);
  DestroyInternalControls;
  FreeAndNil(FCriteria);
  FreeAndNil(FViewInfo);
  FreeAndNil(FRows);
  FreeAndNil(FRoot);
  for AFont := Low(AFont) to High(AFont) do
    FreeAndNil(FFonts[AFont]);
  inherited Destroy;
end;

procedure TcxCustomFilterControl.ApplyFilter;
begin
  DoApplyFilter;
end;

procedure TcxCustomFilterControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxCustomFilterControl.Clear;
begin
  ValueEditorHide(False);
  BeginUpdate;
  try
    ClearRows;
    FCriteria.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomFilterControl.EndUpdate;
begin
  Dec(FLockCount);
  LayoutChanged;
end;

function TcxCustomFilterControl.IsNeedSynchronize: Boolean;
begin
  Result := (FilterLink <> nil) and (FilterLink.Criteria <> nil) and
    (FCriteria <> FilterLink.Criteria);
  if Result then
  begin
    BuildFromRows;
    Result := IsValid and not FCriteria.EqualItems(FilterLink.Criteria);
  end;
end;

function TcxCustomFilterControl.IsValid: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to RowCount - 1 do
    if not Rows[I].Ready then
    begin
      Result := False;
      break;
    end;
end;

function TcxCustomFilterControl.HasItems: Boolean;
begin
  Result := (FilterLink <> nil) and (FilterLink.Count > 0);
end;

procedure TcxCustomFilterControl.LayoutChanged;
begin
  if (FLockCount <> 0) or IsDestroying or IsLoading or not HandleAllocated { or
    ((DropDownMenu <> nil) and DropDownMenu.DroppedDown) } then
    Exit;
  Inc(FLockCount);
  try
    DoLayoutChange;
  finally
    Dec(FLockCount);
  end;
end;

procedure TcxCustomFilterControl.Localize;
var
  AOperator: TcxFilterControlOperator;
  ABoolOperator: TcxFilterBoolOperatorKind;
begin
  FRoot.Group.Caption := cxGetResourceString(@cxSFilterRootGroupCaption);
  FViewInfo.FAddConditionCaption :=
    cxGetResourceString(@cxSFilterFooterAddCondition);
  for AOperator := Low(AOperator) to High(AOperator) do
    cxConditionText[AOperator] := GetFilterControlOperatorText(AOperator);
  for ABoolOperator := fboAnd to fboNotOr do
    cxBoolOperatorText[ABoolOperator] := cxStrFromBoolOperator(ABoolOperator);
  FNullstring := cxGetResourceString(@cxSFilterControlNullString);
  LayoutChanged;
end;

procedure TcxCustomFilterControl.LoadFromFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TcxCustomFilterControl.LoadFromStream(AStream: TStream);
begin
  if not HasItems then
    FilterControlError(cxGetResourceString(@cxSFilterErrorBuilding));
  Clear;
  ReadData(AStream);
  BuildFromCriteria;
end;

procedure TcxCustomFilterControl.SaveToFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TcxCustomFilterControl.TranslationChanged;
begin
  Localize;
end;

procedure TcxCustomFilterControl.SaveToStream(AStream: TStream);
begin
  BuildFromRows;
  WriteData(AStream);
end;

procedure TcxCustomFilterControl.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  // TODO design-time editor ??
  // Filer.DefineBinaryProperty('FilterCriteria', ReadData, WriteData, FCriteria.Root.Count > 0);
end;

procedure TcxCustomFilterControl.Loaded;
begin
  inherited Loaded;
  LayoutChanged;
end;

procedure TcxCustomFilterControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT:
      FocusPrev(False);
    VK_RIGHT:
      FocusNext(False);
    VK_TAB:
      begin
        if ssCtrl in Shift then
          TWinControlAccess(Parent).SelectNext(Self,
            not(ssShift in Shift), True)
        else if Shift = [] then
          FocusNext(True)
        else if ssShift in Shift then
          FocusPrev(True);
        Key := 0;
      end;
    VK_UP:
      FocusUp(False);
    VK_DOWN:
      FocusDown(False);
    VK_DELETE:
      if ssCtrl in Shift then
        Remove;
    VK_INSERT:
      if Shift = [] then
        AddCondition(FocusedRow);
  end;
end;

procedure TcxCustomFilterControl.KeyPress(var Key: Char);
begin
  inherited;
  if Key <> #27 then
    ProcessHitTest(FFocusedInfo.HitTest, Key);
end;

procedure TcxCustomFilterControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FWasError or not HasItems then
    Exit;
  ViewInfo.GetHitTestInfo(Shift, Point(X, Y), FHotTrack);
  if (Button = mbLeft) and not(FHotTrack.HitTest in [fhtNone, fhtRowContent])
    and not DropDownMenu.IsSameDroppedInfo(FHotTrack) then
  begin
    FFocusedInfo := FHotTrack;
    EnsureRowVisible;
    ProcessHitTest(FHotTrack.HitTest, #0);
  end
  else
    DropDownMenu.ClearDroppedInfo;
end;

procedure TcxCustomFilterControl.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  Cursors: array [Boolean] of TCursor = (crDefault, crHandPoint);
begin
  inherited MouseMove(Shift, X, Y);
  BeginMouseTracking(Self, Bounds, Self);
  UpdateHotTrackInfo(Shift, X, Y);
  ViewInfo.Update;
  Cursor := Cursors[HasHotTrack and not(HotTrack.HitTest in [fhtNone,
    fhtRowContent])];
end;

procedure TcxCustomFilterControl.Paint;
begin
  ViewInfo.Paint;
end;

procedure TcxCustomFilterControl.SetEnabled(Value: Boolean);
begin
  inherited;
  LayoutChanged;
end;

procedure TcxCustomFilterControl.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  LayoutChanged;
end;

procedure TcxCustomFilterControl.BiDiModeChanged;
begin
  inherited;
  FDropDownMenu.BiDiMode := BiDiMode;
end;

procedure TcxCustomFilterControl.BoundsChanged;
begin
  inherited BoundsChanged;
  if HandleAllocated then
    LayoutChanged;
end;

procedure TcxCustomFilterControl.ChangeScaleEx(M, D: Integer;
  isDpiChange: Boolean);
var
  I: TcxFilterControlFont;
begin
  inherited;

  FIsFontsChangedLocked := True;
  try
    for I := Low(FFonts) to High(FFonts) do
    begin
      if I in AssignedFonts then
        FFonts[I].Height := MulDiv(FFonts[I].Height, M, D);
    end;
  finally
    FIsFontsChangedLocked := False;
    ViewInfo.InvalidateMinTokenPadding;
    ViewInfo.InvalidateTokenParams;
    LayoutChanged;
  end;
end;

procedure TcxCustomFilterControl.DoLayoutChange;

  function GetContentBottomBound: Integer;
  begin
    if ViewInfo.HasAddConditionButton then
      Result := ViewInfo.AddConditionRect.Bottom
    else
      Result := Rows[RowCount - 1].RowRect.Bottom;
  end;

  procedure CheckVertical;
  begin
    while (TopVisibleRow > 0) and (ClientBounds.Bottom - GetContentBottomBound
      >= ViewInfo.RowHeight + ScaleFactor.Apply(2)) do
    begin
      Dec(FTopVisibleRow);
      ViewInfo.Calc;
    end;
  end;

  procedure CheckHorizontal;
  var
    ADelta: Integer;
  begin
    ADelta := ClientBounds.Right - (ViewInfo.ContentWidth - LeftOffset) -
      ScaleFactor.Apply(2);
    if (LeftOffset > 0) and (ADelta > 0) then
    begin
      FLeftOffset := Max(0, FLeftOffset - ADelta);
      ViewInfo.Calc;
    end;
  end;

begin
  ViewInfo.ResetContentWidth;
  ViewInfo.Calc;
  CheckVertical;
  CheckHorizontal;
  UpdateScrollBars;
  Invalidate;
end;

procedure TcxCustomFilterControl.FocusChanged;
begin
  inherited FocusChanged;
  ViewInfo.GetHitTestInfo([], ScreenToClient(GetMouseCursorPos), FHotTrack);
  LayoutChanged;
end;

procedure TcxCustomFilterControl.FontChanged;
begin
  inherited;
  RefreshFonts;
end;

function TcxCustomFilterControl.GetBorderSize: Integer;
begin
  Result := LookAndFeel.Painter.BorderSize;
end;

procedure TcxCustomFilterControl.InitControl;
begin
  inherited;
  Localize;
end;

procedure TcxCustomFilterControl.InitScrollBarsParameters;
var
  APageSize: Integer;
begin
  if ViewInfo.RowHeight = 0 then
    Exit;
  APageSize := HeightOf(ClientBounds) div ViewInfo.RowHeight;
  if ViewInfo.HasAddConditionButton then
    Dec(APageSize, 1);
  SetScrollBarInfo(sbVertical, 0, RowCount - 1, 1, APageSize, TopVisibleRow,
    True, True);
  SetScrollBarInfo(sbHorizontal, 0, ViewInfo.ContentWidth, 1,
    WidthOf(ClientBounds), FLeftOffset, True, True);
end;

procedure TcxCustomFilterControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  ViewInfo.InvalidateMinTokenPadding;
  ViewInfo.InvalidateTokenParams;
  LayoutChanged;
end;

procedure TcxCustomFilterControl.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  BeginMouseTracking(Self, Bounds, Self);
end;

procedure TcxCustomFilterControl.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  EndMouseTracking(Self);
  FHotTrack.HitTest := fhtNone;
  FHotTrack.Row := nil;
  LayoutChanged;
end;

procedure TcxCustomFilterControl.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);

  procedure ScrollVertical;
  begin
    case AScrollCode of
      scLineUp:
        TopVisibleRow := TopVisibleRow - 1;
      scLineDown:
        TopVisibleRow := TopVisibleRow + 1;
      scTrack:
        TopVisibleRow := AScrollPos;
      scPageUp:
        TopVisibleRow := TopVisibleRow - VScrollBar.PageSize;
      scPageDown:
        TopVisibleRow := TopVisibleRow + VScrollBar.PageSize;
    end;
    AScrollPos := TopVisibleRow;
  end;

  procedure ScrollHorizontal;
  begin
    case AScrollCode of
      scLineUp:
        LeftOffset := LeftOffset - ScaleFactor.Apply(8);
      scLineDown:
        LeftOffset := LeftOffset + ScaleFactor.Apply(8);
      scTrack:
        LeftOffset := AScrollPos;
      scPageUp:
        LeftOffset := LeftOffset - HScrollBar.PageSize;
      scPageDown:
        LeftOffset := LeftOffset + HScrollBar.PageSize;
    end;
    AScrollPos := LeftOffset;
  end;

begin
  if AScrollBarKind = sbVertical then
    ScrollVertical
  else
    ScrollHorizontal;
end;

procedure TcxCustomFilterControl.AddCondition(ARow: TcxCustomRowViewInfo);
var
  ARowParent: TcxCustomRowViewInfo;
begin
  if not HasItems then
    Exit;
  if ARow <> nil then
    ARowParent := ARow
  else
    ARowParent := Rows[RowCount - 1];
  while not(ARowParent is TcxGroupViewInfo) do
    ARowParent := ARowParent.Parent;
  with FFocusedInfo do
    Row := TcxConditionViewInfo.Create(Self, ARowParent, nil);
  RecalcRows;
  if ViewInfo.HasAddConditionButton then
    FFocusedInfo.HitTest := fhtAddCondition // make sure last button visible
  else
    FFocusedInfo.HitTest := fhtItem;
  Recalculate;
  UpdateScrollBars;
  EnsureRowVisible;
  FFocusedInfo.HitTest := fhtItem;
  if ViewInfo.HasAddConditionButton then
    ViewInfo.CalcButtonState;
  ViewInfo.CalcFocusRect;
  Invalidate;
end;

procedure TcxCustomFilterControl.AddGroup;
var
  AGroup: TcxGroupViewInfo;
  ARowParent: TcxCustomRowViewInfo;
begin
  if not HasItems then
    Exit;
  ARowParent := FocusedRow;
  while not(ARowParent is TcxGroupViewInfo) do
    ARowParent := ARowParent.Parent;
  AGroup := TcxGroupViewInfo.Create(Self, ARowParent, nil);
  RecalcRows;
  AddCondition(AGroup);
end;

procedure TcxCustomFilterControl.AddValue;
begin
  if not HasItems then
    Exit;
  FocusedRow.Condition.Values.AddValue;
  FFocusedInfo.HitTest := fhtValue;
  FFocusedInfo.ValueIndex := FocusedRow.Condition.Values.Count - 1;
  Recalculate;
  FLeftOffset := Max((ViewInfo.ContentWidth + ScaleFactor.Apply(2)) -
    ClientBounds.Right, 0);
  LayoutChanged;
end;

procedure TcxCustomFilterControl.ClearRows;
begin
  BeginUpdate;
  try
    with FRoot.Group do
    begin
      while RowCount > 0 do
        Rows[0].Free;
      FRows.Clear;
    end;
    FRows.Clear;
    FRows.Add(FRoot);
    FFocusedInfo.Row := FRoot;
    FRoot.Group.BoolOperator := fboAnd;
    FFocusedInfo.HitTest := fhtBoolOperator;
    FHotTrack.Row := nil;
    FHotTrack.HitTest := fhtNone;
    FTopVisibleRow := 0;
    FLeftOffset := 0;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomFilterControl.Remove;
begin
  if (FFocusedInfo.HitTest = fhtValue) and
    (FocusedRow.Condition.Operator in [fcoInList, fcoNotInList]) and
    (FocusedRow.Condition.Values.Count > 1) then
    RemoveValue
  else
    RemoveRow;
end;

procedure TcxCustomFilterControl.RemoveRow;
var
  ARow: TcxCustomRowViewInfo;
begin
  if FocusedRow = FRoot then
    Exit;
  ARow := FocusedRow;
  // remove Group if last child
  while (ARow.Parent <> FRoot) and (ARow.Parent.Group.RowCount = 1) do
    ARow := ARow.Parent;
  FFocusedInfo.Row := Rows[FRows.IndexOf(ARow) - 1];
  FreeAndNil(ARow);
  if FocusedRow is TcxGroupViewInfo then
    FFocusedInfo.HitTest := fhtBoolOperator
  else
    FFocusedInfo.HitTest := fhtItem;
  FHotTrack.Row := nil;
  RecalcRows;
  Recalculate;
  UpdateScrollBars;
  EnsureRowVisible;
end;

procedure TcxCustomFilterControl.RemoveValue;
begin
  FocusedRow.Condition.Values.RemoveValue(FFocusedInfo.ValueIndex);
  if FFocusedInfo.ValueIndex > 0 then
    Dec(FFocusedInfo.ValueIndex);
  Recalculate;
  UpdateScrollBars;
  EnsureRowVisible;
end;

// navigation
procedure TcxCustomFilterControl.FocusDown(ATab: Boolean);
var
  ARow: TcxCustomRowViewInfo;
begin
  if FocusedRow <> Rows[RowCount - 1] then
  begin
    ARow := Rows[FocusedRowIndex + 1];
    if ATab and ARow.CanFocusButtonOnKeyNavigation then
    begin
      FFocusedInfo.Row := ARow;
      RowNavigate(fhtButton);
    end
    else
      FocusedRow := ARow;
  end
  else if ViewInfo.HasAddConditionButton then
    RowNavigate(fhtAddCondition);
end;

procedure TcxCustomFilterControl.FocusNext(ATab: Boolean);
var
  ANavigated: Boolean;
begin
  case FFocusedInfo.HitTest of
    fhtAddCondition:
      if not ATab then
        FocusedRow := Rows[RowCount - 1];
    fhtAddValue, fhtBoolOperator, fhtExpressionButton:
      if ATab then
        FocusDown(ATab);
    fhtButton:
      if FocusedRow is TcxGroupViewInfo then
        RowNavigate(fhtBoolOperator)
      else
        RowNavigate(fhtItem);
    fhtOperator:
      begin
        ANavigated := False;
        if FocusedRow.Condition.HasDisplayValues then
          if FocusedRow.Condition.Expression = '' then
          begin
            FFocusedInfo.ValueIndex := 0;
            RowNavigate(fhtValue);
            ANavigated := True;
          end
          else if FocusedRow.Condition.HasExpressionButton then
          begin
            RowNavigate(fhtExpressionButton);
            ANavigated := True
          end;
        if not ANavigated and ATab then
          FocusDown(ATab);
      end;
    fhtItem:
      RowNavigate(fhtOperator);
    fhtValue:
      begin
        if (FocusedRow.Condition.Values.Count - 1) > FFocusedInfo.ValueIndex
        then
        begin
          Inc(FFocusedInfo.ValueIndex);
          RowNavigate(fhtValue);
        end
        else if (FocusedRow.Condition.Operator in [fcoInList, fcoNotInList]) and
          not UseTokens then
          RowNavigate(fhtAddValue)
        else if FocusedRow.Condition.HasExpressionButton then
          RowNavigate(fhtExpressionButton)
        else if ATab then
          FocusDown(ATab);
      end;
  end;
end;

procedure TcxCustomFilterControl.FocusPrev(ATab: Boolean);
begin
  case FFocusedInfo.HitTest of
    fhtAddCondition:
      FocusUp(ATab);
    fhtButton:
      if ATab then
        FocusUp(ATab);
    fhtOperator:
      RowNavigate(fhtItem);
    fhtItem, fhtBoolOperator:
      if FFocusedInfo.Row.CanFocusButtonOnKeyNavigation then
        RowNavigate(fhtButton)
      else if ATab then
        FocusUp(ATab);
    fhtValue:
      if FFocusedInfo.ValueIndex > 0 then
      begin
        Dec(FFocusedInfo.ValueIndex);
        RowNavigate(fhtValue);
      end
      else
        RowNavigate(fhtOperator);
    fhtAddValue:
      RowNavigate(fhtValue);
    fhtExpressionButton:
      if FocusedRow.Condition.Expression = '' then
        RowNavigate(fhtValue)
      else
        RowNavigate(fhtOperator);
  end;
end;

procedure TcxCustomFilterControl.FocusUp(ATab: Boolean);

  procedure Select(ARow: TcxCustomRowViewInfo);
  var
    AHitItem: TcxFilterControlHitTest;
  begin
    if ATab then
    begin
      FFocusedInfo.Row := ARow;
      if FocusedRow is TcxGroupViewInfo then
        RowNavigate(fhtBoolOperator)
      else
        with FocusedRow.Condition do
          if HasExpressionButton then
            RowNavigate(fhtExpressionButton)
          else if HasDisplayValues and (Expression = '') then
          begin
            FFocusedInfo.ValueIndex := Values.Count - 1;
            if (&Operator in [fcoInList, fcoNotInList]) and not UseTokens then
              AHitItem := fhtAddValue
            else
              AHitItem := fhtValue;
            RowNavigate(AHitItem);
          end
          else
            RowNavigate(fhtOperator)
    end
    else
      FocusedRow := ARow;
  end;

begin
  if FFocusedInfo.HitTest = fhtAddCondition then
    Select(Rows[RowCount - 1])
  else if FocusedRow <> FRoot then
    Select(Rows[FocusedRowIndex - 1]);
end;

procedure TcxCustomFilterControl.RowNavigate(AElement: TcxFilterControlHitTest;
  ACellIndex: Integer = -1);
begin
  FFocusedInfo.HitTest := AElement;
  if (FocusedRow is TcxConditionViewInfo) and (ACellIndex >= 0) and
    (ACellIndex < TcxConditionViewInfo(FocusedRow).Values.Count) then
    FFocusedInfo.ValueIndex := ACellIndex;
  ViewInfo.Calc;
  EnsureRowVisible;
end;

procedure TcxCustomFilterControl.ValueEditorHide(AAccept: Boolean);
var
  V: Variant;
  S: TCaption;
begin
  FWasError := False;
  State := fcsNormal;
  if FValueEditor = nil then
    Exit;
  if AAccept then
  begin
    FValueEditor.Deactivate;
    with FocusedRow.Condition, Values[FFocusedInfo.ValueIndex] do
    begin
      EditorHelper.GetFilterValue(FValueEditor, GetProperties, V, S);
      try
        FilterControlValidateValue(FValueEditor, V, Operator, ValueType,
          EditorHelper);
      except
        FWasError := True;
        raise
      end;
      FValue := V;
      FValueText := S;
    end;
  end;
  if FValueEditor.Focused and Focused then
  begin
    FValueEditor.EditModified := False;
    FValueEditor.OnFocusChanged := nil;
  end;
  FValueEditor.Parent := nil;
  FValueEditor := nil;
  EnsureRowVisible;
end;

procedure TcxCustomFilterControl.Recalculate;
begin
  ViewInfo.ResetContentWidth;
  ViewInfo.Calc;
end;

procedure TcxCustomFilterControl.EnsureRowVisible;

  function GetFocusedRect: TRect;
  begin
    with FFocusedInfo do
      case HitTest of
        fhtButton:
          begin
            Result := Row.ButtonRect;
            if Row = FRoot then
            begin
              if not UseRightToLeftAlignment then
                Dec(Result.Left, ScaleFactor.Apply(4))
              else
                Inc(Result.Right, ScaleFactor.Apply(4));
            end;
          end;
        fhtBoolOperator:
          Result := Row.Group.BoolOperatorRect;
        fhtItem:
          Result := Row.Condition.ItemRect;
        fhtOperator:
          Result := Row.Condition.OperatorRect;
        fhtValue:
          Result := Row.Condition.Values[ValueIndex].ValueRect;
        fhtAddCondition:
          Result := ViewInfo.AddConditionRect;
        fhtAddValue:
          Result := Row.Condition.Values.AddButtonRect;
        fhtExpressionButton:
          Result := Row.Condition.ExpressionButtonRect;
        fhtRemoveButton:
          Result := Row.RemoveButtonRect;
      else
        Result := cxEmptyRect;
      end;
  end;

var
  AIndex, ALeft, ABottom: Integer;
  R: TRect;
begin
  AIndex := FocusedRowIndex;
  if AIndex < TopVisibleRow then
    FTopVisibleRow := AIndex
  else
  begin
    if FFocusedInfo.HitTest = fhtAddCondition then
      ABottom := ViewInfo.AddConditionRect.Bottom
    else
      ABottom := FocusedRow.RowRect.Bottom;
    while (ABottom > ClientBounds.Bottom) and (FTopVisibleRow < RowCount) do
    begin
      Dec(ABottom, ViewInfo.RowHeight);
      Inc(FTopVisibleRow);
    end;
  end;
  R := GetFocusedRect;
  ALeft := R.Left;
  if R.Right >= ClientBounds.Right then
    OffsetRect(R, ClientBounds.Right - R.Right - 2, 0);
  if R.Left < ClientBounds.Left then
    OffsetRect(R, ClientBounds.Left - R.Left, 0);
  Inc(FLeftOffset, ALeft - R.Left);
  ViewInfo.Calc;
  UpdateScrollBars;
  Invalidate;
end;

procedure TcxCustomFilterControl.BuildFromCriteria;

  procedure Build(ACriteriaList: TcxFilterCriteriaItemList;
    ARow: TcxCustomRowViewInfo);
  var
    I: Integer;
  begin
    with ACriteriaList do
    begin
      ARow.Group.BoolOperator := BoolOperatorKind;
      for I := 0 to Count - 1 do
        if Items[I].IsItemList then
          Build(TcxFilterCriteriaItemList(Items[I]),
            TcxGroupViewInfo.Create(Self, ARow, Items[I]))
        else
          TcxConditionViewInfo.Create(Self, ARow, Items[I]);
    end;
  end;

begin
  BeginUpdate;
  try
    ClearRows;
    Build(FCriteria.Root, FRoot);
    RecalcRows;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomFilterControl.BuildFromRows;

  function GetValueString(AValueInfo: TcxValueInfo;
    ASoftNull: Boolean = False): string;
  begin
    if VarIsNull(AValueInfo.Value) or
      (ASoftNull and VarIsStr(AValueInfo.Value) and (AValueInfo.Value = ''))
    then
      if UseTokens then
        Result := ''
      else
        Result := cxGetResourceString(@cxSFilterBlankCaption)
    else
      Result := AValueInfo.ValueText;
  end;

  procedure AddCondition(ACriteriaList: TcxFilterCriteriaItemList;
    ACondition: TcxConditionViewInfo);
  var
    I: Integer;
    AValue: Variant;
    AText: string;
  begin
    with ACondition do
      case Operator of
        fcoEqual, fcoNotEqual:
          if Expression <> '' then
            ACriteriaList.AddExpressionItem(ItemLink,
              GetFilterOperatorKind(Operator), Expression, Expression)
          else
            ACriteriaList.AddItem(ItemLink, GetFilterOperatorKind(Operator),
              Values[0].Value, GetValueString(Values[0]));
        fcoBetween, fcoNotBetween:
          begin
            // sort between
            if VarCompare(Values[0].Value, Values[1].Value) < 0 then
            begin
              AValue := VarBetweenArrayCreate(Values[0].Value, Values[1].Value);
              AText := GetValueString(Values[0]) + ';' +
                GetValueString(Values[1]);
            end
            else
            begin
              AValue := VarBetweenArrayCreate(Values[1].Value, Values[0].Value);
              AText := GetValueString(Values[1]) + ';' +
                GetValueString(Values[0]);
            end;
            ACriteriaList.AddItem(ItemLink, GetFilterOperatorKind(Operator),
              AValue, AText);
          end;
        fcoInList, fcoNotInList:
          begin
            AValue := VarListArrayCreate(Values[0].Value);
            AText := GetValueString(Values[0]);
            for I := 1 to Values.Count - 1 do
            begin
              VarListArrayAddValue(AValue, Values[I].Value);
              AText := AText + ';' + GetValueString(Values[I]);
            end;
            ACriteriaList.AddItem(ItemLink, GetFilterOperatorKind(Operator),
              AValue, AText);
          end;
        fcoBlanks, fcoNonBlanks:
          ACriteriaList.AddItem(ItemLink, GetFilterOperatorKind(Operator),
            Values[0].Value, cxGetResourceString(@cxSFilterBlankCaption));
      else
        if Expression <> '' then
          ACriteriaList.AddExpressionItem(ItemLink,
            GetFilterOperatorKind(Operator), Expression, Expression)
        else
          ACriteriaList.AddItem(ItemLink, GetFilterOperatorKind(Operator),
            Values[0].Value, GetValueString(Values[0], True));
      end;
  end;

  procedure Build(ACriteriaList: TcxFilterCriteriaItemList;
    ARow: TcxCustomRowViewInfo);
  var
    I: Integer;
  begin
    if ARow is TcxGroupViewInfo then
    begin
      ACriteriaList.BoolOperatorKind := ARow.Group.BoolOperator;
      for I := 0 to ARow.Group.RowCount - 1 do
        if ARow.Group.Rows[I] is TcxGroupViewInfo then
          Build(ACriteriaList.AddItemList(fboAnd), ARow.Group.Rows[I])
        else
          AddCondition(ACriteriaList, ARow.Group.Rows[I].Condition);
    end
    else
      AddCondition(ACriteriaList, ARow.Condition);
  end;

begin
  ValueEditorHide(True);
  if not IsValid then
    Exit;
  with FCriteria do
  begin
    BeginUpdate;
    try
      Clear;
      Build(Root, Self.FRoot);
      Prepare;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomFilterControl.CreateInternalControls;
begin
  // style for Values
  FValueEditorStyle := TcxCustomEditStyle.Create(Self, True);
  with FValueEditorStyle do
  begin
    Font := FontValue;
    LookAndFeel.MasterLookAndFeel := Self.LookAndFeel;
    ButtonTransparency := ebtHideUnselected;
  end;
  FDropDownMenu := TcxFilterControlDropDownMenu.Create(Self);
  // editors pool
  FInplaceEditors := TcxInplaceEditList.Create(Self);
  FTextEditProperties := TcxTextEditProperties.Create(nil);
end;

procedure TcxCustomFilterControl.DestroyInternalControls;
begin
  FreeAndNil(FInplaceEditors);
  FreeAndNil(FValueEditorStyle);
  FreeAndNil(FTextEditProperties);
  FreeAndNil(FDropDownMenu);
end;

procedure TcxCustomFilterControl.DoApplyFilter;
var
  ANeedSynchronize: Boolean;
begin
  if (FilterLink <> nil) and (FilterLink.Criteria <> nil) then
  begin
    BuildFromRows;
    ANeedSynchronize := FCriteria <> FilterLink.Criteria;
    FilterLink.Criteria.BeginUpdate;
    try
      if ANeedSynchronize then
        FilterLink.Criteria.AssignItems(FCriteria);
      if Assigned(FOnApplyFilter) then
        FOnApplyFilter(Self);
    finally
      FilterLink.Criteria.EndUpdate;
      if ANeedSynchronize then
        FCriteria.Assign(FilterLink.Criteria);
      BuildFromCriteria;
    end;
  end;
end;

function TcxCustomFilterControl.GetDefaultProperties: TcxCustomEditProperties;
begin
  Result := FTextEditProperties;
end;

function TcxCustomFilterControl.GetDefaultPropertiesViewInfo
  : TcxCustomEditViewInfo;
begin
  Result := TcxCustomEditViewInfo(GetDefaultProperties.GetViewInfoClass.Create);
end;

function TcxCustomFilterControl.GetFilterControlCriteriaClass
  : TcxFilterControlCriteriaClass;
begin
  Result := TcxFilterControlCriteria;
end;

function TcxCustomFilterControl.GetViewInfoClass: TcxFilterControlViewInfoClass;
begin
  Result := TcxFilterControlViewInfo;
end;

function TcxCustomFilterControl.HasFocus: Boolean;
begin
  Result := IsFocused or ((FValueEditor <> nil) and FValueEditor.IsFocused);
end;

function TcxCustomFilterControl.HasHotTrack: Boolean;
begin
  Result := Enabled and (FHotTrackOnUnfocused or HasFocus) and HasItems;
end;

procedure TcxCustomFilterControl.RefreshProperties;
var
  I: Integer;
  ARow: TcxCustomRowViewInfo;
begin
  if RowCount = 0 then
    Exit;
  for I := 0 to RowCount - 1 do
  begin
    ARow := GetRow(I);
    if ARow is TcxConditionViewInfo then
      ARow.Condition.InternalInit;
  end;
  LayoutChanged;
end;

procedure TcxCustomFilterControl.FillFilterItemList(AStrings: TStrings);
var
  I: Integer;
  AProperties: TcxCustomEditProperties;
begin
  if (AStrings = nil) or not HasItems then
    Exit;
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    with FilterLink do
      for I := 0 to Count - 1 do
      begin
        AProperties := Properties[I];
        if (AProperties <> nil) and
          (esoFiltering in AProperties.GetSupportedOperations) then
          AStrings.AddObject(Captions[I], TObject(I));
      end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TcxCustomFilterControl.UpdateHotTrackInfo(Shift: TShiftState;
  X, Y: Integer);
var
  APrevHotTrack: TcxFilterControlHitTestInfo;

  function SameRow: Boolean;
  begin
    Result := APrevHotTrack.Row = FHotTrack.Row;
  end;

  function SameHitTest: Boolean;
  begin
    Result := (APrevHotTrack.HitTest = FHotTrack.HitTest) and
      (APrevHotTrack.ValueIndex = FHotTrack.ValueIndex);
  end;

  function IsHitTestHot(AHitTest: TcxFilterControlHitTest): Boolean;
  begin
    Result := (AHitTest <> fhtNone) and
      (UseTokens or not(AHitTest in [fhtBoolOperator .. fhtOperator,
      fhtRowContent]));
  end;

begin
  APrevHotTrack := HotTrack;
  ViewInfo.GetHitTestInfo(Shift, Point(X, Y), FHotTrack);
  if not IsHitTestHot(APrevHotTrack.HitTest) and
    not IsHitTestHot(HotTrack.HitTest) then
    Exit;
  if SameRow then
    if SameHitTest then
      Exit
    else
      ViewInfo.InvalidateRow(HotTrack.Row)
  else
  begin
    if IsHitTestHot(APrevHotTrack.HitTest) then
      ViewInfo.InvalidateRow(APrevHotTrack.Row);
    if IsHitTestHot(HotTrack.HitTest) then
      ViewInfo.InvalidateRow(HotTrack.Row);
  end;
end;

function TcxCustomFilterControl.UseTokens: Boolean;
begin
  Result := (CriteriaDisplayStyle = fcdsTokens) or
    (CriteriaDisplayStyle = fcdsDefault) and
    ((FilterLinkEx <> nil) and FilterLinkEx.UseTokens or (FilterLinkEx = nil)
    and (dxFilterCriteriaDisplayStyle = fcdsTokens));
end;

procedure TcxCustomFilterControl.ValidateConditions(var SupportedOperations
  : TcxFilterControlOperators);
begin
  if not FCriteria.SupportedLike then
    SupportedOperations := SupportedOperations - [fcoLike, fcoNotLike,
      fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
end;

procedure TcxCustomFilterControl.CorrectOperatorClass(var AOperatorClass
  : TcxFilterOperatorClass);
begin
end;

function TcxCustomFilterControl.GetFilterCaption: string;
begin
  BuildFromRows;
  Result := FCriteria.FilterCaption
end;

function TcxCustomFilterControl.GetFilterLink: IcxFilterControl;
begin
  Result := nil;
end;

function TcxCustomFilterControl.GetFilterLinkEx: IcxFilterControlEx;
begin
  Result := nil;
end;

function TcxCustomFilterControl.GetFilterText: string;
begin
  BuildFromRows;
  Result := FCriteria.FilterText
end;

procedure TcxCustomFilterControl.RemoveAction;
begin
  if FocusedRow = Root then
    Clear
  else
    RemoveRow;
end;

procedure TcxCustomFilterControl.SelectAction;
begin
  if not HasItems then
    Exit;
  FState := fcsSelectingAction;
  DropDownMenu.CreateActionMenu;
  DropDownMenu.Popup(FocusedRow.ButtonRect);
end;

procedure TcxCustomFilterControl.SelectBoolOperator(AKey: Char);
begin
  if not HasItems then
    Exit;
  State := fcsSelectingBoolOperator;
  DropDownMenu.CreateBoolOperatorList;
  with FocusedRow.Group do
    DropDownMenu.Popup(BoolOperatorRect, BoolOperatorText, AKey);
end;

procedure TcxCustomFilterControl.SelectCondition(AKey: Char);
var
  ACondition: TcxConditionViewInfo;
begin
  if not HasItems then
    Exit;
  State := fcsSelectingCondition;
  ACondition := FocusedRow.Condition;
  ACondition.ValidateConditions;
  DropDownMenu.CreateConditionList(ACondition.SupportedOperators);
  DropDownMenu.Popup(ACondition.OperatorRect, ACondition.OperatorText, AKey);
end;

procedure TcxCustomFilterControl.SelectExpression;
var
  AExpression: string;
begin
  AExpression := FocusedRow.Condition.Expression;
  FilterLinkEx.GetExpressionProvider.EditExpression(AExpression);
  FocusedRow.Condition.Expression := AExpression;
  ViewInfo.Calc;
  EnsureRowVisible;
end;

procedure TcxCustomFilterControl.SelectItem(AKey: Char);
var
  I: Integer;
  AList: TStringList;
  AProperties: TcxCustomEditProperties;
begin
  if not HasItems then
    Exit;
  State := fcsSelectingItem;
  AList := TStringList.Create;
  try
    AList.BeginUpdate;
    with FilterLink do
      for I := 0 to Count - 1 do
      begin
        AProperties := Properties[I];
        if (AProperties <> nil) and
          (esoFiltering in AProperties.GetSupportedOperations) then
          AList.AddObject(Captions[I], TObject(I));
      end;
    if SortItems then
      AList.Sort;
    AList.EndUpdate;
    DropDownMenu.CreateItemList(AList);
  finally
    AList.Free;
  end;
  with FocusedRow.Condition do
    DropDownMenu.Popup(ItemRect, ItemText, AKey);
end;

procedure TcxCustomFilterControl.SelectValue(AActivateKind
  : TcxActivateValueEditKind; AKey: Char);
begin
  if not HasItems then
    Exit;
  State := fcsSelectingValue;
  EnsureRowVisible;
  ValueEditorInit;
  with FValueEditor do
  begin
    SendToBack;
    case AActivateKind of
      aveMouse:
        with FocusedRow.Condition do
          ActivateByMouse(FFocusedInfo.Shift, FFocusedInfo.Mouse.X,
            FFocusedInfo.Mouse.Y, ValueEditorData);
      aveKey:
        with FocusedRow.Condition do
          ActivateByKey(AKey, ValueEditorData);
    else
      with FocusedRow.Condition do
        Activate(ValueEditorData);
    end;
  end;
end;

// IcxMouseTrackingCaller
procedure TcxCustomFilterControl.DoMouseLeave;
begin
  MouseLeave(nil);
end;

// IcxFormatControllerListener
procedure TcxCustomFilterControl.FormatChanged;
begin
  LayoutChanged;
end;

procedure TcxCustomFilterControl.CreateFonts;
var
  AFont: TcxFilterControlFont;
begin
  FIsFontsChangedLocked := True;
  try
    for AFont := Low(AFont) to High(AFont) do
    begin
      FFonts[AFont] := TFont.Create;
      FFonts[AFont].Color := cxFilterControlFontColors[AFont];
      if not(AFont in [fcfValue, fcfExpression]) then
        FFonts[AFont].Style := [fsUnderline];
      FFonts[AFont].OnChange := DoFontChanged;
    end;
  finally
    FIsFontsChangedLocked := False;
  end;
end;

procedure TcxCustomFilterControl.DoFontChanged(Sender: TObject);
var
  AFont: TcxFilterControlFont;
begin
  if FIsFontsChangedLocked then
    Exit;
  for AFont := Low(AFont) to High(AFont) do
  begin
    if FFonts[AFont] = Sender then
      Include(FAssignedFonts, AFont);
  end;
  LayoutChanged;
end;

function TcxCustomFilterControl.GetFont(Index: Integer): TFont;
var
  AFont: TcxFilterControlFont absolute Index;
begin
  Result := FFonts[AFont];
end;

function TcxCustomFilterControl.IsFontStored(Index: Integer): Boolean;
var
  AFont: TcxFilterControlFont absolute Index;
begin
  Result := AFont in FAssignedFonts;
end;

procedure TcxCustomFilterControl.SetFont(Index: Integer; const Value: TFont);
var
  AFont: TcxFilterControlFont absolute Index;
begin
  FFonts[AFont].Assign(Value);
end;

function TcxCustomFilterControl.FocusedRowIndex: Integer;
begin
  Result := FRows.IndexOf(FFocusedInfo.Row);
  if Result < 0 then
    Result := 0;
end;

function TcxCustomFilterControl.GetRow(Index: Integer): TcxCustomRowViewInfo;
begin
  Result := TcxCustomRowViewInfo(FRows[Index]);
end;

function TcxCustomFilterControl.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TcxCustomFilterControl.GetFocusedRow: TcxCustomRowViewInfo;
begin
  Result := FFocusedInfo.Row;
end;

function TcxCustomFilterControl.GetValueEditorBackgroundColor: TColor;
begin
  if UseTokens then
    Result := ViewInfo.Painter.ContentColor
  else
    Result := clWindow;
end;

function TcxCustomFilterControl.GetValueEditorBounds: TRect;
begin
  Result := FocusedRow.Condition.Values[FocusedInfo.ValueIndex]
    .ValueContentRect;
end;

procedure TcxCustomFilterControl.DropDownMenuItemClick(AIndex: Integer);
begin
  with FocusedRow do
    case State of
      fcsSelectingAction:
        ActionMenuClick(AIndex);
      fcsSelectingBoolOperator:
        Group.BoolOperator := TcxFilterBoolOperatorKind
          (DropDownMenu.Items[AIndex].Data);
      fcsSelectingItem:
        begin
          Condition.SetItem(Integer(DropDownMenu.Items[AIndex].Data));
          FFocusedInfo.HitTest := fhtOperator;
          with Condition do
            if (fcoLike in SupportedOperators) and (ValueType <> nil) and
              ValueType.IsString then

              Operator := fcoLike;
              end;
        fcsSelectingCondition:
          begin
            Condition.Operator := TcxFilterControlOperator
              (DropDownMenu.Items[AIndex].Data);
            if Condition.Expression = '' then
              Condition.InitValues(True)
            else if not Condition.IsOperatorSupportedForExpression then
              Condition.Expression := '';
            if Condition.HasDisplayValues then
              if Condition.Expression = '' then
              begin
                FFocusedInfo.HitTest := fhtValue;
                FFocusedInfo.ValueIndex := 0;
              end
              else if Condition.HasExpressionButton then
                FFocusedInfo.HitTest := fhtExpressionButton;
          end;
        end;
      ViewInfo.Calc;
      EnsureRowVisible;
      end;

      procedure TcxCustomFilterControl.SetFocusedRow
        (ARow: TcxCustomRowViewInfo);
      begin FFocusedInfo.Row := ARow;
      if ARow is TcxGroupViewInfo then RowNavigate(fhtBoolOperator)
      else RowNavigate(fhtItem);
      end;

      procedure TcxCustomFilterControl.ActionMenuClick(AIndex: Integer);
      begin case AIndex of 0:
        AddCondition(FocusedRow);
      1:
        AddGroup;
      2:
        RemoveAction;
    end;
end;

function TcxCustomFilterControl.IsNullstringStored: Boolean;
begin
  Result := FNullstring <> cxGetResourceString(@cxSFilterControlNullString);
end;

procedure TcxCustomFilterControl.ProcessHitTest
  (AHitTest: TcxFilterControlHitTest; AKey: Char);
begin
  case AHitTest of
    fhtButton:
      SelectAction;
    fhtAddCondition:
      AddCondition(nil);
    fhtAddValue:
      AddValue;
    fhtBoolOperator:
      SelectBoolOperator(AKey);
    fhtItem:
      SelectItem(AKey);
    fhtOperator:
      SelectCondition(AKey);
    fhtExpressionButton:
      SelectExpression;
    fhtRemoveButton:
      RemoveAction;
    fhtValue:
      begin
        if AKey = #0 then
          SelectValue(aveMouse, #0)
        else if (AKey = #13) or (AKey = ' ') then
          SelectValue(aveEnter, #0)
        else
          SelectValue(aveKey, AKey);
      end;
  end
end;

procedure TcxCustomFilterControl.ReadData(AStream: TStream);
begin
  FCriteria.ReadData(AStream);
end;

procedure TcxCustomFilterControl.RecalcRows;

  procedure FillRows(ARow: TcxCustomRowViewInfo);
  var
    I: Integer;
  begin
    FRows.Add(ARow);
    if ARow is TcxGroupViewInfo then
      for I := 0 to ARow.Group.RowCount - 1 do
        FillRows(ARow.Group.Rows[I])
  end;

begin
  FRows.Clear;
  FillRows(FRoot);
end;

procedure TcxCustomFilterControl.RefreshFonts;
var
  AFont: TcxFilterControlFont;
begin
  FIsFontsChangedLocked := True;
  BeginUpdate;
  try
    for AFont := Low(AFont) to High(AFont) do
      if not(AFont in FAssignedFonts) then
      begin
        FFonts[AFont].Name := Font.Name;
        FFonts[AFont].Height := Font.Height;
        FFonts[AFont].Charset := Font.Charset;
        FFonts[AFont].Color := cxFilterControlFontColors[AFont];
        if AFont in [fcfValue, fcfExpression] then
          FFonts[AFont].Style := []
        else
          FFonts[AFont].Style := [fsUnderline];
      end;
  finally
    EndUpdate;
    FIsFontsChangedLocked := False;
  end;
end;

procedure TcxCustomFilterControl.SetAssignedFonts
  (const Value: TcxFilterControlFonts);
begin
  if FAssignedFonts <> Value then
  begin
    FAssignedFonts := Value;
    RefreshFonts;
  end;
end;

procedure TcxCustomFilterControl.SetCriteriaDisplayStyle
  (const AValue: TcxFilterCriteriaDisplayStyle);
begin
  if FCriteriaDisplayStyle <> AValue then
  begin
    FCriteriaDisplayStyle := AValue;
    LayoutChanged;
  end;
end;

procedure TcxCustomFilterControl.SetLeftOffset(Value: Integer);
begin
  Value := Min(Value, ViewInfo.ContentWidth - WidthOf(ClientBounds) + 1);
  if Value < 0 then
    Value := 0;
  if FLeftOffset <> Value then
  begin
    FLeftOffset := Value;
    LayoutChanged;
  end;
end;

procedure TcxCustomFilterControl.SetNullstring(const Value: string);
begin
  if FNullstring <> Value then
  begin
    FNullstring := Value;
    LayoutChanged;
  end;
end;

procedure TcxCustomFilterControl.SetTopVisibleRow(Value: Integer);
var
  AValue: Integer;
begin
  AValue := RowCount - HeightOf(ClientBounds) div ViewInfo.RowHeight;
  if ViewInfo.HasAddConditionButton then
    Inc(AValue, 1);
  Value := Min(Value, AValue);
  if Value < 0 then
    Value := 0;
  if FTopVisibleRow <> Value then
  begin
    FTopVisibleRow := Value;
    LayoutChanged;
  end;
end;

procedure TcxCustomFilterControl.SetShowLevelLines(const Value: Boolean);
begin
  if FShowLevelLines <> Value then
  begin
    FShowLevelLines := Value;
    Invalidate;
  end;
end;

procedure TcxCustomFilterControl.SetWantTabs(const Value: Boolean);
begin
  FWantTabs := Value;
  if Value then
    Keys := [kAll, kArrows, kChars, kTab]
  else
    Keys := [kAll, kArrows, kChars];
end;

procedure TcxCustomFilterControl.ValidateEditorPos(const ABounds: TRect);
begin
  if Assigned(FValueEditor) and FValueEditor.Showing and
    (FocusedInfo.HitTest = fhtValue) then
    FValueEditor.BoundsRect := ABounds;
end;

procedure TcxCustomFilterControl.ValueEditorInit;

  procedure SetValidChars;
  var
    AWildcardChars: set of AnsiChar;
  begin
    AWildcardChars := [AnsiChar(FCriteria.UnderscoreWildcard),
      AnsiChar(FCriteria.PercentWildcard)];
    if FValueEditor is TcxCustomTextEdit then
      if FocusedRow.Condition.Operator in [fcoLike, fcoNotLike, fcoContains,
        fcoNotContains, fcoBeginsWith, fcoEndsWith] then
        with TcxCustomTextEditProperties(FValueEditor.ActiveProperties) do
          ValidChars := ValidChars + AWildcardChars
  end;

var
  AProperties: TcxCustomEditProperties;
  AFilterEditor: TcxCustomFilterEditHelperClass;
begin
  AProperties := FocusedRow.Condition.GetProperties;
  AFilterEditor := FocusedRow.Condition.EditorHelper;
  FValueEditor := AFilterEditor.GetFilterEdit(AProperties, FInplaceEditors);
  FValueEditor.Parent := Self;
  AFilterEditor.SetFilterValue(FValueEditor, AProperties,
    FocusedRow.Condition.Values[FocusedInfo.ValueIndex].Value);
  SetValidChars;
  FValueEditor.Style.Assign(FValueEditorStyle);
  FValueEditor.Style.Color := GetValueEditorBackgroundColor;
  FValueEditor.BoundsRect := GetValueEditorBounds;
  FValueEditor.OnAfterKeyDown := ValueEditorAfterKeyDown;
  FValueEditor.OnExit := ValueEditorExit;
  FValueEditor.OnKeyDown := ValueEditorKeyDown;
  if UseTokens then
  begin
    FValueEditor.OnMouseMove := ValueEditorMouseMove;
    FValueEditor.OnMouseLeave := ValueEditorMouseLeave;
  end;
end;

procedure TcxCustomFilterControl.ValueEditorAfterKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) or (Key = VK_DOWN) then
    try
      ValueEditorHide(True);
      SetFocus;
      KeyDown(Key, Shift);
      Key := 0;
    except
      FValueEditor.SetFocus;
      raise;
    end;
end;

procedure TcxCustomFilterControl.ValueEditorExit(Sender: TObject);
begin
  try
    ValueEditorHide(True);
  except
    FValueEditor.SetFocus;
    raise;
  end;
end;

procedure TcxCustomFilterControl.ValueEditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        ValueEditorHide(True);
        SetFocus;
        Key := 0;
      end;
    VK_TAB:
      if FWantTabs then
      begin
        ValueEditorHide(True);
        SetFocus;
        if ssShift in Shift then
          FocusPrev(True)
        else
          FocusNext(True);
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        if FValueEditor <> nil then
          Key := 0;
        ValueEditorHide(False);
        SetFocus;
      end;
    VK_DELETE:
      if Shift = [ssCtrl] then
      begin
        ValueEditorHide(False);
        SetFocus;
        Remove;
        Key := 0;
      end;
  end;
end;

procedure TcxCustomFilterControl.ValueEditorMouseLeave(ASender: TObject);
begin
  FHotTrack.Row := nil;
  FHotTrack.HitTest := fhtNone;
  ViewInfo.InvalidateRow(FocusedRow);
end;

procedure TcxCustomFilterControl.ValueEditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
begin
  APoint := dxMapWindowPoint(ValueEditor.Handle, Handle, Point(X, Y));
  UpdateHotTrackInfo(Shift, APoint.X, APoint.Y);
end;

procedure TcxCustomFilterControl.WriteData(AStream: TStream);
begin
  FCriteria.WriteData(AStream);
end;

procedure TcxCustomFilterControl.WMDropDownMenuClosed(var Message: TMessage);
begin
  if not DropDownMenu.Visible then
  begin
    DropDownMenu.ClearDroppedInfo;
    if UseTokens and (FocusedInfo.HitTest = fhtButton) then
      RowNavigate(fhtBoolOperator);
  end;
end;

{ TcxFilterControlPainter }

constructor TcxFilterControlPainter.Create(AOwner: TcxCustomFilterControl);
begin
  inherited Create;
  FControl := AOwner;
end;

function TcxFilterControlPainter.GetContentColor: TColor;
begin
  Result := clDefault;
  if Control.UseTokens then
    Result := ViewInfo.TokenParams.FilterControlBackgroundColor;
  if (Result = clDefault) and (Control.LookAndFeel.SkinPainter <> nil) then
    Result := Control.LookAndFeel.SkinPainter.DefaultContentColor;
  if Result = clDefault then
    Result := Control.Color;
end;

procedure TcxFilterControlPainter.DrawBorder;
begin
  Painter.DrawBorder(Canvas, Control.Bounds);
end;

procedure TcxFilterControlPainter.DrawDotLine(const R: TRect);
var
  APrevBkColor, APrevTextColor: TColor;
begin
  APrevBkColor := GetBkColor(Canvas.Canvas.Handle);
  APrevTextColor := GetTextColor(Canvas.Canvas.Handle);
  SetBkColor(Canvas.Canvas.Handle, ColorToRGB(cl3DDkShadow));
  SetTextColor(Canvas.Canvas.Handle, ColorToRGB(ContentColor));
  Windows.FillRect(Canvas.Canvas.Handle, R, HalftoneBrush);
  SetBkColor(Canvas.Canvas.Handle, APrevBkColor);
  SetTextColor(Canvas.Canvas.Handle, APrevTextColor);
end;

procedure TcxFilterControlPainter.DrawGroup(ARow: TcxGroupViewInfo);
var
  AMargins: TRect;
begin
  Canvas.Brush.Style := bsClear;
  if ARow.HasCaption then
    TextDraw(ARow.CaptionRect.Left, ARow.CaptionRect.Top + 1, ARow.Caption);
  if ARow.BoolOperatorText <> '' then
  begin
    if Control.UseTokens then
    begin
      Painter.DrawScaledFilterBoolOperatorBackground(Canvas,
        ARow.BoolOperatorRect, ARow.BoolOperatorState, ARow.ScaleFactor);
      Canvas.Font.Color := Control.ViewInfo.TokenParams.BoolOperatorTextColor
    end
    else
      Canvas.Font.Assign(Control.FontBoolOperator);
    AMargins := Control.ViewInfo.GetTextMargins(fhtBoolOperator);
    TextDraw(ARow.BoolOperatorRect.Left + AMargins.Left,
      ARow.BoolOperatorRect.Top + AMargins.Top, ARow.BoolOperatorText);
  end;
end;

procedure TcxFilterControlPainter.DrawCondition(ARow: TcxConditionViewInfo);
begin
  Canvas.Brush.Style := bsClear;
  DrawItemCaption(ARow);
  DrawOperator(ARow);
  if ARow.HasDisplayValues then
    if ARow.Expression = '' then
      DrawValues(ARow)
    else
      DrawExpression(ARow);
  if ARow.HasExpressionButton then
  begin
    Canvas.Font.Assign(Control.Font);
    Painter.DrawScaledButton(Canvas, ARow.ExpressionButtonRect,
      cxExpressionButtonText, ARow.ExpressionButtonState, Control.ScaleFactor);
  end;
end;

procedure TcxFilterControlPainter.DrawExpression(ARow: TcxConditionViewInfo);
begin
  Canvas.Font.Assign(Control.FontExpression);
  TextDraw(ARow.ExpressionRect.Left + 1, ARow.ExpressionRect.Top + 1,
    ARow.Expression);
end;

procedure TcxFilterControlPainter.DrawItemCaption(ARow: TcxConditionViewInfo);
var
  AMargins: TRect;
begin
  if ARow.ItemText = '' then
    Exit;
  if Control.UseTokens then
  begin
    Painter.DrawScaledFilterItemCaptionBackground(Canvas, ARow.ItemRect,
      ARow.ItemState, ARow.ScaleFactor);
    Canvas.Font.Color := Control.ViewInfo.TokenParams.ItemCaptionTextColor;
  end
  else
    Canvas.Font.Assign(Control.FontItem);
  AMargins := Control.ViewInfo.GetTextMargins(fhtItem);
  TextDraw(ARow.ItemRect.Left + AMargins.Left, ARow.ItemRect.Top + AMargins.Top,
    ARow.ItemText);
end;

procedure TcxFilterControlPainter.DrawOperator(ARow: TcxConditionViewInfo);
var
  AMargins: TRect;
begin
  if ARow.NeedDrawOperatorAsImage then
    TcxFilterControlImagesHelper.DrawOperatorImage(Canvas.Canvas, ARow.Operator,
      ARow.OperatorRect, Painter,
      TdxAlphaColors.FromColor(Painter.DefaultContentTextColor))
  else if ARow.OperatorText <> '' then
  begin
    if Control.UseTokens then
    begin
      Painter.DrawScaledFilterOperatorBackground(Canvas, ARow.OperatorRect,
        ARow.OperatorState, ARow.ScaleFactor);
      Canvas.Font.Color := Control.ViewInfo.TokenParams.OperatorTextColor;
    end
    else
      Canvas.Font.Assign(Control.FontCondition);
    AMargins := Control.ViewInfo.GetTextMargins(fhtOperator);
    TextDraw(ARow.OperatorRect.Left + AMargins.Left, ARow.OperatorRect.Top +
      AMargins.Top, ARow.OperatorText);
  end;
end;

procedure TcxFilterControlPainter.DrawValues(ARow: TcxConditionViewInfo);
var
  I, APos: Integer;
begin
  with ARow do
  begin
    for I := 0 to Values.Count - 1 do
    begin
      if Control.UseTokens and not IsRectEmpty(Values[I].ValueRect) then
        if (Control.State = fcsSelectingValue) and
          (Values[I].ValueState = cxbsDefault) then
        begin
          Canvas.FillRect(Values[I].ValueRect, Control.ValueEditor.Style.Color);
          Control.LookAndFeelPainter.DrawBorder(Canvas, Values[I].ValueRect);
        end
        else
          Painter.DrawScaledFilterValueBackground(Canvas, Values[I].ValueRect,
            Values[I].ValueState, ScaleFactor);
      Values[I].ValueViewInfo.Paint(Self.Canvas);
    end;
    if Operator in [fcoBetween, fcoNotBetween] then
    begin
      Canvas.Brush.Color := ContentColor;
      if Control.UseTokens then
        Canvas.Font.Color := Control.ViewInfo.TokenParams.OperatorTextColor
      else
        Canvas.Font.Assign(Control.FontCondition);
      for I := 1 to Values.Count - 1 do
        with Values[I - 1] do
        begin
          APos := ValueRect.Right;
          TextDraw(APos + ScaleFactor.Apply(4),
            OperatorRect.Top + Control.ViewInfo.GetTextMargins(fhtOperator).Top,
            Values.Separator);
        end;
    end
    else if Operator in [fcoInList, fcoNotInList] then
    begin
      Canvas.Font.Assign(Control.FontValue);
      Canvas.Brush.Color := ContentColor;
      for I := 1 to Values.Count - 1 do
        with Values[I - 1] do
          TextDraw(ValueRect.Right + 2, ItemRect.Top + 1, Values.Separator);
      if not Control.UseTokens then
      begin
        TextDraw(OperatorRect.Right + ScaleFactor.Apply(4),
          OperatorRect.Top, '(');
        TextDraw(Values[Values.Count - 1].ValueRect.Right +
          ScaleFactor.Apply(3), OperatorRect.Top, ')');
        Painter.DrawScaledButton(Canvas, Values.AddButtonRect, '+',
          Values.AddButtonState, Control.ScaleFactor);
      end
      else if (Values.AddButtonState in [cxbsHot, cxbsPressed]) or
        Control.HasHotTrack and (Control.HotTrack.Row = ARow) and
        (Control.HotTrack.HitTest <> fhtNone) then
        Painter.DrawScaledFilterControlAddButtonGlyph(Canvas,
          Values.AddButtonGlyphRect, Values.AddButtonState, ScaleFactor)
    end;
  end;
end;

procedure TcxFilterControlPainter.DrawRow(ARow: TcxCustomRowViewInfo);
var
  ALeft, H: Integer;
  AParent: TcxCustomRowViewInfo;
begin
  with Canvas, ARow do
  begin
    Brush.Color := ContentColor;
    FillRect(ARow.RowRect);
    Font.Assign(Control.Font);
    if IsButtonVisible then
      if Control.UseTokens then
        Painter.DrawScaledFilterControlAddButtonGlyph(Self.Canvas,
          ButtonGlyphRect, ButtonDrawState, Control.ScaleFactor)
      else
        Painter.DrawScaledButton(Self.Canvas, ButtonRect, ButtonText,
          ButtonDrawState, Control.ScaleFactor);
    if ARow is TcxGroupViewInfo then
      DrawGroup(Group)
    else
      DrawCondition(Condition);
    if IsRemoveButtonVisible then
      Painter.DrawScaledFilterControlRemoveButtonGlyph(Self.Canvas,
        RemoveButtonGlyphRect, RemoveButtonDrawState, Control.ScaleFactor);
    // draw level's lines if need
    if Control.ShowLevelLines and (Level > 0) and not Control.UseTokens then
    begin
      Brush.Style := bsSolid;
      Brush.Color := clGray;
      if not Control.UseRightToLeftAlignment then
        ALeft := Parent.ButtonRect.Left + WidthOf(ButtonRect) div 2
      else
        ALeft := Parent.ButtonRect.Right - WidthOf(ButtonRect) div 2;
      H := HeightOf(RowRect) div 2;
      DrawDotLine(Bounds(ALeft, RowRect.Top, 1, H + 1));
      if not IsLast then
        DrawDotLine(Bounds(ALeft, RowRect.Top + H, 1, H + 1));
      Inc(H, RowRect.Top);
      if not Control.UseRightToLeftAlignment then
        DrawDotLine(Rect(ALeft, H, ButtonRect.Left -
          ScaleFactor.Apply(3), H + 1))
      else
        DrawDotLine(Rect(ALeft, H, ButtonRect.Right +
          ScaleFactor.Apply(3), H + 1));
      AParent := Parent;
      while AParent <> nil do
      begin
        if not AParent.IsLast then
        begin
          if not Control.UseRightToLeftAlignment then
            ALeft := AParent.Parent.ButtonRect.Left + WidthOf(ButtonRect) div 2
          else
            ALeft := AParent.Parent.ButtonRect.Right -
              WidthOf(ButtonRect) div 2;
          DrawDotLine(Rect(ALeft, RowRect.Top, ALeft + 1, RowRect.Bottom));
        end;
        AParent := AParent.Parent;
      end;
    end;
  end;
end;

procedure TcxFilterControlPainter.TextDraw(X, Y: Integer; const AText: string);
begin
  with Canvas.Canvas do
  begin
    if not ViewInfo.Enabled then
    begin
      Brush.Style := bsClear;
      Font.Color := clBtnHighlight;
      TextOut(X + 1, Y + 1, AText);
      Font.Color := clBtnShadow;
    end;
    TextOut(X, Y, AText);
  end;
end;

function TcxFilterControlPainter.GetCanvas: TcxCanvas;
begin
  Result := ViewInfo.Canvas;
end;

function TcxFilterControlPainter.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FControl.LookAndFeel.Painter;
end;

function TcxFilterControlPainter.GetViewInfo: TcxFilterControlViewInfo;
begin
  Result := FControl.FViewInfo;
end;

{ TcxFilterControlViewInfo }

constructor TcxFilterControlViewInfo.Create(AOwner: TcxCustomFilterControl);
begin
  inherited Create;
  FControl := AOwner;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pfDevice;
  FBitmapCanvas := TcxCanvas.Create(FBitmap.Canvas);
  FPainter := GetPainterClass.Create(AOwner);
  FButtonState := cxbsNormal;
  ResetContentWidth;
end;

destructor TcxFilterControlViewInfo.Destroy;
begin
  FreeAndNil(FPainter);
  FreeAndNil(FBitmapCanvas);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TcxFilterControlViewInfo.Calc;
const
  TokenAreaTopOffset = 9;
var
  ARow: TcxCustomRowViewInfo;
  ASize: TSize;
  I, AMarginsHeight, AIndent, ATopOffset: Integer;
  AFont: TcxFilterControlFont;
  AItem: TcxFilterControlHitTest;
  R: TRect;
begin
  CheckBitmap;
  FEnabled := Control.HasItems and Control.Enabled;
  FMinValueWidth := Canvas.TextWidth('0') * 12;
  dxAdjustToTouchableSize(FMinValueWidth, Control.ScaleFactor);
  FRowHeight := Canvas.FontHeight(Control.Font);
  if Control.UseTokens then
  begin
    AMarginsHeight := 0;
    for AItem := fhtBoolOperator to fhtValue do
      AMarginsHeight := Max(AMarginsHeight, GetTextMargins(AItem).Top +
        GetTextMargins(AItem).Bottom);
    Inc(FRowHeight, AMarginsHeight);
  end
  else
    for AFont := Low(AFont) to High(AFont) do
      FRowHeight := Max(FRowHeight, Canvas.FontHeight(Control.FFonts[AFont]));
  FRowHeight := Max(FRowHeight, GetEditHeight) + GetRowMargins.Top +
    GetRowMargins.Bottom;
  dxAdjustToTouchableSize(FRowHeight, Control.ScaleFactor);
  ATopOffset := IfThen(Control.UseTokens, TokenAreaTopOffset, 0);
  ATopOffset := Control.ScaleFactor.Apply(ATopOffset);
  R := Rect(Control.ClientBounds.Left - Control.LeftOffset,
    Control.ClientBounds.Top + ATopOffset, Control.ClientBounds.Right,
    Control.ClientBounds.Top + ATopOffset + FRowHeight);
  OffsetRect(R, 0, -Control.TopVisibleRow * FRowHeight);
  FMaxRowWidth := 0;
  for I := 0 to Control.RowCount - 1 do
  begin
    ARow := Control.Rows[I];
    if not Control.UseRightToLeftAlignment then
      ARow.Calc(R)
    else
      ARow.Calc(TdxRightToLeftLayoutConverter.ConvertRect(R,
        Control.ClientBounds));
    FMaxRowWidth := Max(FMaxRowWidth, ARow.Width);
    OffsetRect(R, 0, FRowHeight);
  end;
  if HasAddConditionButton then
  begin
    AIndent := Control.Rows[Control.RowCount - 1].Indent +
      Control.ScaleFactor.Apply(4);
    Inc(R.Left, AIndent);
    Canvas.Font.Assign(Control.Font);
    ASize := Canvas.TextExtent(FAddConditionCaption + '00');
    FAddConditionRect := Classes.Bounds(R.Left, 0, ASize.cx,
      HeightOf(R) - Control.ScaleFactor.Apply
      (IfThen(cxIsTouchModeEnabled, 12, 4)));
    CenterRectVert(R, FAddConditionRect);
    if Control.UseRightToLeftAlignment then
      FAddConditionRect := TdxRightToLeftLayoutConverter.ConvertRect
        (FAddConditionRect, Control.ClientBounds);
    FMaxRowWidth := Max(FMaxRowWidth, WidthOf(FAddConditionRect) + AIndent);
    CalcButtonState;
  end
  else
  begin
    FAddConditionRect := cxEmptyRect;
    FButtonState := cxbsNormal;
  end;
  FContentWidth := Max(FContentWidth, FMaxRowWidth);
  CalcFocusRect;
end;

procedure TcxFilterControlViewInfo.GetHitTestInfo(AShift: TShiftState;
  const P: TPoint; var HitInfo: TcxFilterControlHitTestInfo);
var
  I: Integer;
begin
  with Control do
  begin
    HitInfo.HitTest := fhtNone;
    HitInfo.Mouse := P;
    HitInfo.Row := FocusedRow;
    if HitInfo.Row = nil then
      HitInfo.Row := FRoot;
    HitInfo.Shift := AShift;
    HitInfo.ValueIndex := -1;
    if not PtInRect(ClientBounds, P) then
      Exit;
    if HasAddConditionButton and PtInRect(AddConditionRect, P) then
      HitInfo.HitTest := fhtAddCondition
    else
      for I := TopVisibleRow to RowCount - 1 do
        if PtInRect(Rows[I].RowRect, P) then
        begin
          HitInfo.Row := Rows[I];
          Rows[I].GetHitTestInfo(P, HitInfo);
          break;
        end;
  end;
end;

procedure TcxFilterControlViewInfo.Paint;

  function IsFocused: Boolean;
  begin
    Result := Control.IsFocused or ((Control.FValueEditor <> nil) and
      Control.FValueEditor.IsFocused);
  end;

var
  I: Integer;
  R: TRect;
begin
  Control.DrawScrollBars(Canvas);
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Painter.ContentColor;
    FillRect(Control.ClientBounds);
  end;
  for I := Control.TopVisibleRow to Control.RowCount - 1 do
    with Control.Rows[I] do
    begin
      if Canvas.RectVisible(RowRect) then
        Painter.DrawRow(Control.Rows[I]);
      if RowRect.Top > Control.ClientBounds.Bottom then
        break;
    end;
  if HasAddConditionButton then
  begin
    Canvas.Brush.Color := Painter.ContentColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Assign(Control.Font);
    Painter.Painter.DrawScaledButton(Self.Canvas, FAddConditionRect,
      FAddConditionCaption, FButtonState, Control.ScaleFactor);
  end;
  if not IsRectEmpty(FFocusRect) and IsFocused then
    Canvas.DrawFocusRect(FFocusRect);
  R := Bounds(0, 0, FBitmap.Width, FBitmap.Height);
  Painter.DrawBorder;
  BitBlt(Control.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height,
    Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TcxFilterControlViewInfo.InvalidateRow(ARow: TcxCustomRowViewInfo);
begin
  if ARow <> nil then
  begin
    ARow.Calc(ARow.RowRect);
    Control.InvalidateRect(ARow.RowRect, False);
  end;
end;

procedure TcxFilterControlViewInfo.Update;
var
  AState: TcxButtonState;
begin
  if HasAddConditionButton then
  begin
    AState := FButtonState;
    CalcButtonState;
    if AState <> FButtonState then
      Control.InvalidateRect(FAddConditionRect, True);
  end
  else
    FButtonState := cxbsNormal;
  CalcFocusRect;
end;

procedure TcxFilterControlViewInfo.CalcFocusRect;
begin
  if Control.UseTokens then
    FFocusRect := cxEmptyRect
  else if not Control.HasItems then
  begin
    FFocusRect := Control.ClientBounds;
    InflateRect(FFocusRect, -2, -2);
  end
  else
    with Control.FFocusedInfo do
    begin
      FFocusRect := cxEmptyRect;
      case HitTest of
        fhtBoolOperator:
          FFocusRect := Row.Group.BoolOperatorRect;
        fhtItem:
          FFocusRect := Row.Condition.ItemRect;
        fhtOperator:
          FFocusRect := Row.Condition.OperatorRect;
        fhtValue:
          begin
            FFocusRect := Row.Condition.Values[ValueIndex].ValueRect;
            Control.ValidateEditorPos(FFocusRect);
          end;
      end;
      if HitTest in [fhtBoolOperator, fhtItem, fhtOperator, fhtValue] then
        InflateRect(FFocusRect, 1, 1);
    end;
end;

function TcxFilterControlViewInfo.GetPainterClass: TcxFilterControlPainterClass;
begin
  Result := TcxFilterControlPainter;
end;

function TcxFilterControlViewInfo.GetRowMargins: TRect;
begin
  if Control.UseTokens then
    Result := Control.ScaleFactor.Apply(Rect(0, 1, 0, 1))
  else
    Result := Control.ScaleFactor.Apply(Rect(0, 3, 0, 3));
end;

function TcxFilterControlViewInfo.GetTextMargins
  (AItem: TcxFilterControlHitTest): TRect;
begin
  if Control.UseTokens then
    case AItem of
      fhtBoolOperator:
        Result := TokenParams.BoolOperatorTextMargins;
      fhtItem:
        Result := TokenParams.ItemCaptionTextMargins;
      fhtOperator:
        Result := TokenParams.OperatorTextMargins;
      fhtValue:
        Result := TokenParams.ValueTextMargins;
    else
      Result := cxEmptyRect;
    end
  else
    Result := Control.ScaleFactor.Apply(Rect(1, 1, 1, 1));
end;

function TcxFilterControlViewInfo.HasAddConditionButton: Boolean;
begin
  Result := not Control.UseTokens;
end;

procedure TcxFilterControlViewInfo.InvalidateMinTokenPadding;
begin
  FIsMinTokenPaddingValid := False;
end;

procedure TcxFilterControlViewInfo.InvalidateTokenParams;
begin
  FIsTokenParamsValid := False;
end;

procedure TcxFilterControlViewInfo.ResetContentWidth;
begin
  FContentWidth := -1;
end;

procedure TcxFilterControlViewInfo.AdjustTokenPadding(var APadding: TRect);
begin
  APadding.Left := Max(APadding.Left, MinTokenPadding.Left);
  APadding.Right := Max(APadding.Right, MinTokenPadding.Right);
  APadding.Top := Max(APadding.Top, MinTokenPadding.Top);
  APadding.Bottom := Max(APadding.Bottom, MinTokenPadding.Bottom);
end;

procedure TcxFilterControlViewInfo.CalcButtonState;
begin
  if not FEnabled then
    FButtonState := cxbsDisabled
  else if Control.FocusedInfo.HitTest = fhtAddCondition then
    FButtonState := cxbsDefault
  else if Control.HasHotTrack and (Control.HotTrack.HitTest = fhtAddCondition)
  then
    FButtonState := cxbsHot
  else
    FButtonState := cxbsNormal;
end;

procedure TcxFilterControlViewInfo.CheckBitmap;
begin
  FBitmap.Height := Control.Height; // only if needed
  FBitmap.Width := Control.Width; // only if needed
end;

function TcxFilterControlViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FBitmapCanvas;
end;

function TcxFilterControlViewInfo.GetEditHeight: Integer;
var
  AEditSizeProperties: TcxEditSizeProperties;
begin
  AEditSizeProperties.MaxLineCount := 1;
  AEditSizeProperties.Width := 10;
  AEditSizeProperties.Height := -1;
  if not Control.UseTokens then
    Canvas.Font.Assign(Control.FontValue);
  with Control do
    Result := FTextEditProperties.GetEditSize(Canvas, FValueEditorStyle, True,
      'Yy', AEditSizeProperties).cy;
  dxAdjustToTouchableSize(Result, Control.ScaleFactor);
end;

function TcxFilterControlViewInfo.GetMinTokenPadding: TRect;
begin
  if not FIsMinTokenPaddingValid then
  begin
    FMinTokenPadding := cxGetMinTokenPadding(Control.ScaleFactor,
      Control.UseRightToLeftAlignment);
    FIsMinTokenPaddingValid := True;
  end;
  Result := FMinTokenPadding;
end;

function TcxFilterControlViewInfo.GetTokenParams: TdxFilterTokenParams;
begin
  if not FIsTokenParamsValid then
  begin
    Painter.Painter.GetScaledFilterTokenParams(FTokenParams,
      Control.ScaleFactor);
    AdjustTokenPadding(FTokenParams.BoolOperatorTextMargins);
    AdjustTokenPadding(FTokenParams.ItemCaptionTextMargins);
    AdjustTokenPadding(FTokenParams.OperatorTextMargins);
    AdjustTokenPadding(FTokenParams.ValueTextMargins);
    FIsTokenParamsValid := True;
  end;
  Result := FTokenParams;
end;

{ TcxFilterControl }

destructor TcxFilterControl.Destroy;
begin
  LinkComponent := nil;
  inherited Destroy;
end;

procedure TcxFilterControl.UpdateFilter;
begin
  if (FilterLink <> nil) and (FilterLink.Criteria <> nil) then
  begin
    if FCriteria <> FilterLink.Criteria then
      FCriteria.Assign(FilterLink.Criteria);
    BuildFromCriteria;
  end
  else
    Clear;
end;

function TcxFilterControl.GetFilterLinkEx: IcxFilterControlEx;
begin
  if LinkComponent <> nil then
    Supports(LinkComponent, IcxFilterControlEx, Result)
  else
    Result := nil;
end;

function TcxFilterControl.GetFilterLink: IcxFilterControl;
begin
  if FLinkComponent <> nil then
    Supports(TObject(FLinkComponent), IcxFilterControl, Result)
  else
    Result := nil;
end;

procedure TcxFilterControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FLinkComponent) then
    LinkComponent := nil
end;

function TcxFilterControl.GetLinkComponent: TComponent;
begin
  Result := FLinkComponent;
end;

procedure TcxFilterControl.RegisterLinkNotifications;
begin
  if LinkComponent <> nil then
  begin
    LinkComponent.FreeNotification(Self);
    if FilterLinkEx <> nil then
      FilterLinkEx.RegisterListener(Self);
  end;
end;

procedure TcxFilterControl.SetLinkComponent(Value: TComponent);
begin
  if (Value <> nil) and not Supports(Value, IcxFilterControl) then
    Value := nil;
  if LinkComponent <> Value then
  begin
    UnregisterLinkNotifications;
    FLinkComponent := Value;
    RegisterLinkNotifications;
    if not IsDestroying then
      UpdateFilter;
  end;
end;

procedure TcxFilterControl.UnregisterLinkNotifications;
begin
  if (LinkComponent <> nil) and not(csDestroying in LinkComponent.ComponentState)
  then
  begin
    LinkComponent.RemoveFreeNotification(Self);
    if FilterLinkEx <> nil then
      FilterLinkEx.UnregisterListener(Self);
  end;
end;

procedure CreateHalftoneBrush;
var
  X, Y: Integer;
  DC: HDC;
  Pattern: HBITMAP;
begin
  Pattern := CreateBitmap(8, 8, 1, 1, nil);
  DC := CreateCompatibleDC(0);
  Pattern := SelectObject(DC, Pattern);
  FillRect(DC, Rect(0, 0, 8, 8), GetStockObject(WHITE_BRUSH));
  for Y := 0 to 7 do
    for X := 0 to 7 do
      if (Y mod 2) = (X mod 2) then
        SetPixel(DC, X, Y, 0);
  Pattern := SelectObject(DC, Pattern);
  DeleteDC(DC);
  HalftoneBrush := CreatePatternBrush(Pattern);
  DeleteObject(Pattern);
end;

procedure Init;
begin
  CreateHalftoneBrush;
end;

procedure Done;
begin
  TcxFilterControlImagesHelper.DestroyImages;
  if HalftoneBrush <> 0 then
    DeleteObject(HalftoneBrush);
end;

initialization

dxUnitsLoader.AddUnit(@Init, @Done);

finalization

dxUnitsLoader.RemoveUnit(@Done);

end.
