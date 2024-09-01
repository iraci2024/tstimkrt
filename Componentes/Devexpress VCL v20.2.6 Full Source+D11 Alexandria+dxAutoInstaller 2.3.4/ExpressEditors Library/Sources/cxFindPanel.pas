{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEditors }
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

unit cxFindPanel;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, Messages, cxClasses, cxControls, cxContainer,
  cxDropDownEdit, cxEdit,
  cxMRUEdit, dxCoreClasses, cxStyles, cxLookAndFeels, cxGraphics, cxCustomData;

const
  cxFindPanelDefaultMRUItemsListDropDownCount = 8;
  cxFindPanelDefaultMRUItemsListCount = 0;

type
  TcxFindPanelMRUEdit = class; // for internal use only
  TcxFindPanelMRUEditProperties = class; // for internal use only
  TcxCustomFindPanel = class; // for internal use only

  TcxFindPanelDisplayMode = (fpdmNever, fpdmManual, fpdmAlways);
  TcxFindPanelPosition = (fppTop, fppBottom);
  TcxFindPanelLayout = (fplDefault, fplCompact);

  TcxFindPanelItemKind = (fpikNone, fpikFindEdit, fpikCloseButton,
    fpikFindButton, fpikClearButton, fpikPreviousButton, fpikNextButton);
  // for internal use only
  TcxFindPanelValidateFocusedItemType = (fvtFocusNext, fvtFocusPrev,
    fvtFocusEdit); // for internal use only

  { TcxFindPanelMRUEditViewInfo }

  TcxFindPanelMRUEditViewInfo = class(TcxCustomComboBoxViewInfo)
  // for internal use only
  strict private
  const
    NextButtonGlyphSize = 12;
    PreviousButtonGlyphSize = 12;
  strict private
    function GetEdit: TcxFindPanelMRUEdit;
    function GetProperties: TcxFindPanelMRUEditProperties;
  protected
    procedure DrawClearButtonGlyph(ACanvas: TcxCanvas;
      const AButtonBounds: TRect; AState: TcxEditButtonState); virtual;
    procedure DrawEditButton(ACanvas: TcxCanvas;
      AButtonVisibleIndex: Integer); override;
    procedure DrawFindButtonGlyph(ACanvas: TcxCanvas;
      const AButtonBounds: TRect; const AState: TcxEditButtonState); virtual;
    procedure DrawInfoButtonText(ACanvas: TcxCanvas; const AButtonBounds: TRect;
      const AText: string); virtual;
    procedure DrawNextButtonGlyph(ACanvas: TcxCanvas;
      const AButtonBounds: TRect; const AState: TcxEditButtonState); virtual;
    procedure DrawPreviousButtonGlyph(ACanvas: TcxCanvas;
      const AButtonBounds: TRect; const AState: TcxEditButtonState); virtual;

    property Properties: TcxFindPanelMRUEditProperties read GetProperties;
  public
    property Edit: TcxFindPanelMRUEdit read GetEdit;
  end;

  { TcxFindPanelMRUEditViewData } // for internal use

  TcxFindPanelMRUEditViewData = class(TcxCustomComboBoxViewData)
  protected
    function GetEditContentDefaultOffsets: TRect; override;
  end;

  { TcxFindPanelMRUEditProperties }

  TcxFindPanelMRUEditProperties = class(TcxCustomMRUEditProperties)
  strict private
    FClearButton: TcxEditButton;
    FDropDownButton: TcxEditButton;
    FFindButton: TcxEditButton;
    FInfoButton: TcxEditButton;
    FNextButton: TcxEditButton;
    FPreviousButton: TcxEditButton;
  protected
    procedure AddClearButton; virtual;
    procedure AddFindButton; virtual;
    procedure AddInfoButton; virtual;
    procedure AddNextButton; virtual;
    procedure AddPreviousButton; virtual;
    procedure DoChanged; override;
    function GetButtonVisibleIndex(AButtonIndex: Integer): Integer; virtual;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function DropDownButtonVisibleIndex: Integer; override;
    procedure UpdateDropDownButtonVisibility; virtual;

    property ClearButton: TcxEditButton read FClearButton;
    property DropDownButton: TcxEditButton read FDropDownButton;
    property FindButton: TcxEditButton read FFindButton;
    property InfoButton: TcxEditButton read FInfoButton;
    property NextButton: TcxEditButton read FNextButton;
    property PreviousButton: TcxEditButton read FPreviousButton;
  public
    constructor Create(AOwner: TPersistent); override;

    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
  end;

  { TcxFindPanelMRUEditInnerEdit }

  TcxFindPanelMRUEditInnerEdit = class(TcxCustomComboBoxInnerEdit)
  // for internal use
  strict private
    function GetContainer: TcxFindPanelMRUEdit;
    //
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    property Container: TcxFindPanelMRUEdit read GetContainer;
  end;

  { TcxFindPanelMRUEdit }

  TcxFindPanelMRUEdit = class(TcxCustomMRUEdit)
  strict private
    FCachedMinHeight: Integer;
    FFindPanel: TcxCustomFindPanel;
    FNeedRecalculateMinHeight: Boolean;

    function GetProperties: TcxFindPanelMRUEditProperties;

    procedure WMMove(var Message: TWMMove); message WM_MOVE;
  protected
    procedure VisualRefinementsListenerChanged; override;

    procedure ChangeScaleEx(M: Integer; D: Integer;
      IsDPIChanged: Boolean); override;
    procedure CreateHandle; override;
    procedure DoButtonClick(AButtonVisibleIndex: Integer); override;
    procedure DoChange; override;
    procedure FocusChanged; override;
    function GetInnerEditClass: TControlClass; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetInternalEditValue(const Value: Variant); override;
    procedure UndoPerformed; override;

    property FindPanel: TcxCustomFindPanel read FFindPanel;
  public
    constructor Create(AFindPanel: TcxCustomFindPanel); reintroduce; virtual;
    destructor Destroy; override;

    procedure AddItem(const Value: string); override;
    function CalculateMinHeight: Integer; virtual;
    procedure ClearSelection; override;
    procedure CutToClipboard; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure Hide;
    procedure PasteFromClipboard; override;
    procedure SetFocus; override;
    procedure SetFocusAndSelectAll;
    procedure Show;
    procedure TranslationChanged; override;
    procedure UpdateLookAndFeel(ALookAndFeel: TcxLookAndFeel);
    procedure UpdateStyles(const AParams, AContentParams: TcxViewParams);

    property Properties: TcxFindPanelMRUEditProperties read GetProperties;
  end;

  TcxFindPanelOptions = class(TPersistent) // for internal use
  strict private
    FApplyInputDelay: Integer;
    FClearOnClose: Boolean;
    FDisplayMode: TcxFindPanelDisplayMode;
    FFindPanel: TcxCustomFindPanel;
    FFocusContentOnApply: Boolean;
    FHighlightSearchResults: Boolean;
    FInfoText: string;
    FInfoTextAssigned: Boolean;
    FIsUpdatingMRUItems: Boolean;
    FLayout: TcxFindPanelLayout;
    FMRUItems: TStringList;
    FMRUItemsListCount: Integer;
    FMRUItemsListDropDownCount: Integer;
    FPosition: TcxFindPanelPosition;
    FShowClearButton: Boolean;
    FShowCloseButton: Boolean;
    FShowFindButton: Boolean;
    FShowNextButton: Boolean;
    FShowPreviousButton: Boolean;
    FUseDelayedFind: Boolean;

    function GetBehavior: TcxDataFindCriteriaBehavior;
    function GetMRUItems: TStrings;
    function GetUseExtendedSyntax: Boolean;
    procedure SetApplyInputDelay(AValue: Integer);
    procedure SetBehavior(AValue: TcxDataFindCriteriaBehavior);
    procedure SetClearOnClose(AValue: Boolean);
    procedure SetDisplayMode(AValue: TcxFindPanelDisplayMode);
    procedure SetContentOnApply(AValue: Boolean);
    procedure SetHighlightSearchResults(AValue: Boolean);
    procedure SetLayout(AValue: TcxFindPanelLayout);
    procedure SetMRUItems(AValue: TStrings);
    procedure SetMRUItemsListCount(AValue: Integer);
    procedure SetMRUItemsListDropDownCount(AValue: Integer);
    procedure SetPosition(AValue: TcxFindPanelPosition);
    procedure SetShowClearButton(AValue: Boolean);
    procedure SetShowCloseButton(AValue: Boolean);
    procedure SetShowFindButton(AValue: Boolean);
    procedure SetShowNextButton(AValue: Boolean);
    procedure SetShowPreviousButton(AValue: Boolean);
    procedure SetUseDelayedFind(AValue: Boolean);
    procedure SetUseExtendedSyntax(AValue: Boolean);

    procedure CreateMRUItems;
    procedure DestroyMRUItems;
    procedure MRUItemsChangeHandler(Sender: TObject);

    function GetInfoText: string;
    procedure SetInfoText(AValue: string);
  protected
    procedure Changed;
    procedure CheckMRUItemListCount;

    property FindPanel: TcxCustomFindPanel read FFindPanel;
  public
    constructor Create(AFindPanel: TcxCustomFindPanel); reintroduce; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure UpdateMRUItems;

    property ApplyInputDelay: Integer read FApplyInputDelay
      write SetApplyInputDelay;
    property Behavior: TcxDataFindCriteriaBehavior read GetBehavior
      write SetBehavior;
    property ClearOnClose: Boolean read FClearOnClose write SetClearOnClose;
    property DisplayMode: TcxFindPanelDisplayMode read FDisplayMode
      write SetDisplayMode;
    property FocusContentOnApply: Boolean read FFocusContentOnApply
      write SetContentOnApply;
    property HighlightSearchResults: Boolean read FHighlightSearchResults
      write SetHighlightSearchResults;
    property InfoText: string read GetInfoText write SetInfoText;
    property InfoTextAssigned: Boolean read FInfoTextAssigned;
    property Layout: TcxFindPanelLayout read FLayout write SetLayout;
    property MRUItems: TStrings read GetMRUItems write SetMRUItems;
    property MRUItemsListCount: Integer read FMRUItemsListCount
      write SetMRUItemsListCount;
    property MRUItemsListDropDownCount: Integer read FMRUItemsListDropDownCount
      write SetMRUItemsListDropDownCount;
    property Position: TcxFindPanelPosition read FPosition write SetPosition;
    property ShowClearButton: Boolean read FShowClearButton
      write SetShowClearButton;
    property ShowCloseButton: Boolean read FShowCloseButton
      write SetShowCloseButton;
    property ShowFindButton: Boolean read FShowFindButton
      write SetShowFindButton;
    property ShowNextButton: Boolean read FShowNextButton
      write SetShowNextButton;
    property ShowPreviousButton: Boolean read FShowPreviousButton
      write SetShowPreviousButton;
    property UseDelayedFind: Boolean read FUseDelayedFind
      write SetUseDelayedFind;
    property UseExtendedSyntax: Boolean read GetUseExtendedSyntax
      write SetUseExtendedSyntax;
  end;

  { TcxFindPanel }

  TcxCustomFindPanel = class
  strict private
  const
    ButtonsFirstOffset = 12;
    ButtonsFirstOffsetForCompactLayout = 6;
    EditMaxWidth = 348;
    EditMaxWidthForCompactLayout = 210;
  strict private
    FDelayedFindTimer: TcxTimer;
    FEdit: TcxFindPanelMRUEdit;
    FFocusedItem: TcxFindPanelItemKind;
    FOptions: TcxFindPanelOptions;
    FVisible: Boolean;

    function GetEdit: TcxFindPanelMRUEdit;
    function GetEditHeight: Integer;
    function GetEditLookupItems: TStrings;
    function GetText: string;
    procedure SetFocusedItem(AValue: TcxFindPanelItemKind);
    procedure SetOptions(AValue: TcxFindPanelOptions);
    procedure SetText(const AValue: string);
    procedure SetVisible(AValue: Boolean);

    procedure CreateTimer;
    procedure DestroyTimer;
    procedure OnDelayedFindTimer(Sender: TObject);
  protected const
    ButtonMinHeight = 22;
    ButtonMinWidth = 60;
    ButtonsOffset = 4;
    EditMinWidth = 52;
  protected
    procedure AddTextInEditMRUItems; virtual;
    procedure ApplyText(const AText: string); virtual;
    function CanItemFocus(AItem: TcxFindPanelItemKind): Boolean; virtual;
    function CanHide: Boolean; virtual;
    procedure Clear; virtual;
    procedure ClearText; virtual;
    function DoGetItemCaption(AItem: TcxFindPanelItemKind): string; virtual;
    procedure EditDestroyed; virtual;
    procedure FocusControlContent; virtual;
    procedure FocusedItemChanged; virtual;
    procedure FocusedItemExecute; virtual;
    procedure FocusNextItem; virtual;
    procedure FocusPreviousItem; virtual;
    function GetActualLayout: TcxFindPanelLayout; virtual;
    function GetButtonsFirstOffset: Integer; virtual;
    function GetDefaultInfoText: string; virtual;
    function GetDefaultMRUItemsListCount: Integer; virtual;
    function GetDefaultMRUItemsListDropDownCount: Integer; virtual;
    function GetEditMaxWidth: Integer; virtual;
    function GetInfoButtonText: string; virtual;
    function HasEdit: Boolean; virtual;
    function HasItemCaption(AItem: TcxFindPanelItemKind): Boolean; virtual;
    procedure Invalidate(ARecalculate: Boolean = False); virtual;
    function IsButtonFocused: Boolean; virtual;
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    procedure StartDelayedFind; virtual;
    procedure UpdateEdit; virtual;
    procedure UpdateEditClearButton; virtual;
    procedure UpdateEditFindButton; virtual;
    procedure UpdateEditInfoButton; virtual;
    procedure UpdateEditLookAndFeel; virtual;
    procedure UpdateEditNextButton; virtual;
    procedure UpdateEditPreviousButton; virtual;
    procedure UpdateEditStyles; virtual;
    procedure UpdateEditValue; virtual;
    procedure UpdateStyles; virtual;
    procedure ValidateFocusedItem
      (AType: TcxFindPanelValidateFocusedItemType = fvtFocusEdit); virtual;
    procedure VisibilityChanged; virtual;

    // abstraction
    function GetControl: TcxControl; virtual; abstract;
    procedure GetControlContentViewParams(var AParams: TcxViewParams);
      virtual; abstract;
    function GetCriteria: TcxDataFindCriteria; virtual; abstract;
    function GetLookAndFeel: TcxLookAndFeel; virtual; abstract;
    function GetOwner: TComponent; virtual; abstract;
    procedure GetViewParams(var AParams: TcxViewParams); virtual; abstract;

    function CreateEdit: TcxFindPanelMRUEdit; virtual;
    function CreateOptions: TcxFindPanelOptions; virtual;
    procedure DestroyEdit; virtual;
    procedure DestroyOptions; virtual;

    property ActualLayout: TcxFindPanelLayout read GetActualLayout;
    property Criteria: TcxDataFindCriteria read GetCriteria;
    property Edit: TcxFindPanelMRUEdit read GetEdit;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property Text: string read GetText write SetText;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Apply; virtual;
    function CanShow: Boolean; virtual;
    procedure Changed; virtual; abstract;
    procedure ClearButtonExecute; virtual;
    procedure CloseButtonExecute; virtual;
    procedure ControlFocusChanged(AIsFocused: Boolean); virtual;
    procedure CriteriaChanged(AChanges: TcxDataFindCriteriaChanges); virtual;
    procedure DelayedFind; virtual;
    procedure DisplayModeChanged; virtual;
    procedure EditFocusChanged; virtual;
    procedure FindButtonExecute; virtual;
    procedure FindEditExecute; virtual;
    function GetItemCaption(AItem: TcxFindPanelItemKind): string; virtual;
    procedure Hide; virtual;
    procedure HideEdit; virtual;
    function IsFocused: Boolean; virtual;
    function IsEditFocused: Boolean; virtual;
    function IsItemEnabled(AItem: TcxFindPanelItemKind): Boolean; virtual;
    function IsItemFocused(AItem: TcxFindPanelItemKind): Boolean; virtual;
    function IsItemVisible(AItem: TcxFindPanelItemKind): Boolean; virtual;
    procedure ItemExecute(AItem: TcxFindPanelItemKind); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure LookAndFeelChanged; virtual;
    function NeedDelayedFindOnKeyDown(Key: Word; Shift: TShiftState)
      : Boolean; virtual;
    procedure NextButtonExecute; virtual;
    procedure PreviousButtonExecute; virtual;
    procedure Show(AFocusEdit: Boolean = True); virtual;
    procedure ShowEdit; virtual;
    procedure StylesChanged; virtual;
    procedure UpdateEditBounds(const ABounds: TRect); virtual;
    procedure UpdateEditMRUItems; virtual;
    procedure UpdateEditTextHint; virtual;
    procedure UpdateMRUItems; virtual;
    procedure UpdateOptionsMRUItems; virtual;
    procedure UpdateViewInfo(ARecalculate: Boolean = False); virtual;

    property Control: TcxControl read GetControl;
    property DefaultInfoText: string read GetDefaultInfoText;
    property DefaultMRUItemsListCount: Integer read GetDefaultMRUItemsListCount;
    property DefaultMRUItemsListDropDownCount: Integer
      read GetDefaultMRUItemsListDropDownCount;
    property EditHeight: Integer read GetEditHeight;
    property EditLookupItems: TStrings read GetEditLookupItems;
    property FocusedItem: TcxFindPanelItemKind read FFocusedItem
      write SetFocusedItem;
    property Options: TcxFindPanelOptions read FOptions write SetOptions;
    property Owner: TComponent read GetOwner;
    property Visible: Boolean read FVisible write SetVisible;
  end;

implementation

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Variants, Math, Graphics, dxCore, cxDrawTextUtils,
  cxLookAndFeelPainters, cxGeometry;

type
  TcxControlHelper = class helper for TcxControl
  protected
    procedure DoFocusChanged;
  end;

procedure TcxControlHelper.DoFocusChanged;
begin
  FocusChanged;
end;

{ TcxFindPanelMRUEditViewInfo }

procedure TcxFindPanelMRUEditViewInfo.DrawClearButtonGlyph(ACanvas: TcxCanvas;
  const AButtonBounds: TRect; AState: TcxEditButtonState);
var
  ARect: TRect;
begin
  ARect := cxRectCenter(AButtonBounds, Painter.GetScaledClearButtonGlyphSize
    (ScaleFactor));
  Painter.DrawScaledClearButtonGlyph(ACanvas, ARect,
    EditBtnStateToButtonState[AState], ScaleFactor);
end;

procedure TcxFindPanelMRUEditViewInfo.DrawEditButton(ACanvas: TcxCanvas;
  AButtonVisibleIndex: Integer);
var
  AButtonViewInfo: TcxEditButtonViewInfo;
begin
  AButtonViewInfo := ButtonsInfo[AButtonVisibleIndex];
  if not(AButtonViewInfo.ButtonIndex in [Properties.InfoButton.Index,
    Properties.ClearButton.Index]) then
    inherited DrawEditButton(ACanvas, AButtonVisibleIndex)
  else if not AButtonViewInfo.Data.BackgroundPartiallyTransparent then
    ACanvas.FillRect(AButtonViewInfo.Bounds,
      AButtonViewInfo.Data.BackgroundColor);
  if AButtonViewInfo.ButtonIndex = Properties.InfoButton.Index then
    DrawInfoButtonText(ACanvas, AButtonViewInfo.Bounds,
      AButtonViewInfo.Data.Caption)
  else if AButtonViewInfo.ButtonIndex = Properties.FindButton.Index then
    DrawFindButtonGlyph(ACanvas, AButtonViewInfo.Bounds,
      AButtonViewInfo.Data.State)
  else if AButtonViewInfo.ButtonIndex = Properties.ClearButton.Index then
    DrawClearButtonGlyph(ACanvas, AButtonViewInfo.Bounds,
      AButtonViewInfo.Data.State)
  else if AButtonViewInfo.ButtonIndex = Properties.NextButton.Index then
    DrawNextButtonGlyph(ACanvas, AButtonViewInfo.Bounds,
      AButtonViewInfo.Data.State)
  else if AButtonViewInfo.ButtonIndex = Properties.PreviousButton.Index then
    DrawPreviousButtonGlyph(ACanvas, AButtonViewInfo.Bounds,
      AButtonViewInfo.Data.State);
end;

procedure TcxFindPanelMRUEditViewInfo.DrawFindButtonGlyph(ACanvas: TcxCanvas;
  const AButtonBounds: TRect; const AState: TcxEditButtonState);
begin
  Painter.DrawScaledSearchEditButtonGlyph(ACanvas, AButtonBounds,
    EditBtnStateToButtonState[AState], ScaleFactor);
end;

procedure TcxFindPanelMRUEditViewInfo.DrawInfoButtonText(ACanvas: TcxCanvas;
  const AButtonBounds: TRect; const AText: string);
const
  AInfoTextFormat = CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or
    CXTO_SINGLELINE;
var
  ARect: TRect;
begin
  ARect := AButtonBounds;
  ARect.Top := TextRect.Top;
  ARect.Bottom := TextRect.Bottom;
  if UseSkins then
    ACanvas.Font.Color := Painter.DefaultEditorTextColor(True)
  else
    ACanvas.Font.Color := clBtnShadow;
  cxTextOut(ACanvas.Canvas, AText, ARect, AInfoTextFormat);
end;

procedure TcxFindPanelMRUEditViewInfo.DrawNextButtonGlyph(ACanvas: TcxCanvas;
  const AButtonBounds: TRect; const AState: TcxEditButtonState);
var
  AGlyphSize: TSize;
  AGlyphRect: TRect;
begin
  AGlyphSize := ScaleFactor.Apply(cxSize(NextButtonGlyphSize));
  AGlyphRect := cxRectCenter(AButtonBounds, AGlyphSize);
  Painter.DrawScaledFindPanelNextButtonGlyph(ACanvas, AGlyphRect,
    EditBtnStateToButtonState[AState], ScaleFactor);
end;

procedure TcxFindPanelMRUEditViewInfo.DrawPreviousButtonGlyph
  (ACanvas: TcxCanvas; const AButtonBounds: TRect;
  const AState: TcxEditButtonState);
var
  AGlyphSize: TSize;
  AGlyphRect: TRect;
begin
  AGlyphSize := ScaleFactor.Apply(cxSize(PreviousButtonGlyphSize));
  AGlyphRect := cxRectCenter(AButtonBounds, AGlyphSize);
  Painter.DrawScaledFindPanelPreviousButtonGlyph(ACanvas, AGlyphRect,
    EditBtnStateToButtonState[AState], ScaleFactor);
end;

function TcxFindPanelMRUEditViewInfo.GetEdit: TcxFindPanelMRUEdit;
begin
  Result := TcxFindPanelMRUEdit(inherited Edit);
end;

function TcxFindPanelMRUEditViewInfo.GetProperties
  : TcxFindPanelMRUEditProperties;
begin
  Result := Edit.Properties;
end;

{ TcxFindPanelMRUEditViewData }

function TcxFindPanelMRUEditViewData.GetEditContentDefaultOffsets: TRect;
begin
  Result := inherited GetEditContentDefaultOffsets;
  Inc(Result.Left, 1);
  Inc(Result.Top, 1);
  Inc(Result.Right, 1);
  Inc(Result.Bottom, 1);
  TdxVisualRefinements.Padding.InflatePadding(Result, ScaleFactor);
end;

{ TcxFindPanelMRUEditProperties }

constructor TcxFindPanelMRUEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDropDownButton := Buttons[0];
end;

class function TcxFindPanelMRUEditProperties.GetViewDataClass
  : TcxCustomEditViewDataClass;
begin
  Result := TcxFindPanelMRUEditViewData;
end;

class function TcxFindPanelMRUEditProperties.GetViewInfoClass
  : TcxContainerViewInfoClass;
begin
  Result := TcxFindPanelMRUEditViewInfo;
end;

procedure TcxFindPanelMRUEditProperties.AddClearButton;
begin
  FClearButton := Buttons.Add;
  ClearButton.Kind := bkGlyph;
  ClearButton.Index := 0;
end;

procedure TcxFindPanelMRUEditProperties.AddFindButton;
begin
  FFindButton := Buttons.Add;
  FindButton.Kind := bkGlyph;
end;

procedure TcxFindPanelMRUEditProperties.AddInfoButton;
begin
  FInfoButton := Buttons.Add;
  InfoButton.Kind := bkText;
  InfoButton.Index := 0;
end;

procedure TcxFindPanelMRUEditProperties.AddNextButton;
begin
  FNextButton := Buttons.Add;
  NextButton.Kind := bkGlyph;
end;

procedure TcxFindPanelMRUEditProperties.AddPreviousButton;
begin
  FPreviousButton := Buttons.Add;
  PreviousButton.Kind := bkGlyph;
end;

procedure TcxFindPanelMRUEditProperties.DoChanged;
begin
  inherited DoChanged;
  UpdateDropDownButtonVisibility;
end;

function TcxFindPanelMRUEditProperties.GetButtonVisibleIndex
  (AButtonIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AButtonIndex do
    if Buttons[I].Visible then
      Inc(Result);
end;

function TcxFindPanelMRUEditProperties.DropDownButtonVisibleIndex: Integer;
begin
  Result := GetButtonVisibleIndex(DropDownButton.Index);
end;

procedure TcxFindPanelMRUEditProperties.UpdateDropDownButtonVisibility;
begin
  if DropDownButton <> nil then
    DropDownButton.Visible := (LookupItems.Count > 0);
end;

{ TcxFindPanelMRUEditInnerEdit }

function TcxFindPanelMRUEditInnerEdit.GetContainer: TcxFindPanelMRUEdit;
begin
  Result := TcxFindPanelMRUEdit(inherited Container);
end;

procedure TcxFindPanelMRUEditInnerEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

{ TcxFindPanelMRUEdit }

constructor TcxFindPanelMRUEdit.Create(AFindPanel: TcxCustomFindPanel);
begin
  inherited Create(AFindPanel.Owner);
  FFindPanel := AFindPanel;
  AutoSize := False;
  BeepOnEnter := False;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := cxcbsNone;
  DoubleBuffered := False;
  Visible := False;
  FNeedRecalculateMinHeight := True;
  Properties.ShowEllipsis := False;
  Properties.AddClearButton;
  Properties.AddFindButton;
  Properties.AddInfoButton;
  Properties.AddPreviousButton;
  Properties.AddNextButton;
end;

destructor TcxFindPanelMRUEdit.Destroy;
begin
  FindPanel.EditDestroyed;
  inherited Destroy;
end;

procedure TcxFindPanelMRUEdit.AddItem(const Value: string);
begin
  inherited AddItem(Value);
  FindPanel.UpdateOptionsMRUItems;
end;

function TcxFindPanelMRUEdit.CalculateMinHeight: Integer;
var
  AEditSizeProperties: TcxEditSizeProperties;
begin
  if FNeedRecalculateMinHeight then
  begin
    AEditSizeProperties.Height := -1;
    AEditSizeProperties.Width := -1;
    AEditSizeProperties.MaxLineCount := 1;
    FCachedMinHeight := Properties.GetEditSize(cxScreenCanvas, Style, False,
      null, AEditSizeProperties).cy;
    FNeedRecalculateMinHeight := False;
  end;
  Result := FCachedMinHeight;
end;

procedure TcxFindPanelMRUEdit.ClearSelection;
begin
  inherited ClearSelection;
  FindPanel.DelayedFind;
end;

procedure TcxFindPanelMRUEdit.CutToClipboard;
begin
  inherited CutToClipboard;
  FindPanel.DelayedFind;
end;

class function TcxFindPanelMRUEdit.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TcxFindPanelMRUEditProperties;
end;

procedure TcxFindPanelMRUEdit.Hide;
begin
  if IsFocused then
    Windows.SetFocus(Parent.Handle);
  DestroyHandle;
  Parent := nil;
  Visible := False;
end;

procedure TcxFindPanelMRUEdit.PasteFromClipboard;
begin
  inherited PasteFromClipboard;
  FindPanel.DelayedFind;
end;

procedure TcxFindPanelMRUEdit.SetFocus;
begin
  if CanFocusEx then
    inherited SetFocus;
end;

procedure TcxFindPanelMRUEdit.SetFocusAndSelectAll;
begin
  SetFocus;
  SelectAll;
end;

procedure TcxFindPanelMRUEdit.Show;
begin
  Parent := FindPanel.Control;
  CheckHandle;
  Visible := True;
  if not IsFocused and (FindPanel.FocusedItem = fpikFindEdit) then
    SetFocusAndSelectAll;
end;

procedure TcxFindPanelMRUEdit.TranslationChanged;
begin
  inherited TranslationChanged;
  FindPanel.UpdateEditTextHint;
end;

procedure TcxFindPanelMRUEdit.UpdateLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  FNeedRecalculateMinHeight := True;
end;

procedure TcxFindPanelMRUEdit.UpdateStyles(const AParams, AContentParams
  : TcxViewParams);
var
  AEditParams: TcxViewParams;
begin
  AEditParams := AParams;
  AEditParams.Color := AContentParams.Color;
  Style.Init(AEditParams);
  FNeedRecalculateMinHeight := True;
end;

procedure TcxFindPanelMRUEdit.VisualRefinementsListenerChanged;
begin
  FNeedRecalculateMinHeight := True;
  InitContentParams;
  AdjustInnerEditPosition;
  ContainerStyleChanged(nil);
end;

procedure TcxFindPanelMRUEdit.ChangeScaleEx(M, D: Integer;
  IsDPIChanged: Boolean);
var
  ABounds: TRect;
begin
  ABounds := BoundsRect;
  try
    inherited ChangeScaleEx(M, D, IsDPIChanged);
  finally
    BoundsRect := ABounds;
  end;
end;

procedure TcxFindPanelMRUEdit.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure TcxFindPanelMRUEdit.DoButtonClick(AButtonVisibleIndex: Integer);
var
  AButtonIndex: Integer;
begin
  inherited DoButtonClick(AButtonVisibleIndex);
  AButtonIndex := ViewInfo.ButtonsInfo[AButtonVisibleIndex].ButtonIndex;
  if AButtonIndex = Properties.FindButton.Index then
    FindPanel.FindButtonExecute
  else if AButtonIndex = Properties.ClearButton.Index then
    FindPanel.ClearButtonExecute
  else if AButtonIndex = Properties.NextButton.Index then
    FindPanel.NextButtonExecute
  else if AButtonIndex = Properties.PreviousButton.Index then
    FindPanel.PreviousButtonExecute;
end;

procedure TcxFindPanelMRUEdit.DoChange;
begin
  inherited DoChange;
  FindPanel.UpdateEditClearButton;
end;

procedure TcxFindPanelMRUEdit.FocusChanged;
begin
  if csDestroying in Parent.ComponentState then
    Exit;
  inherited FocusChanged;
  FindPanel.EditFocusChanged;
end;

function TcxFindPanelMRUEdit.GetInnerEditClass: TControlClass;
begin
  Result := TcxFindPanelMRUEditInnerEdit;
end;

procedure TcxFindPanelMRUEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FindPanel.KeyDown(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TcxFindPanelMRUEdit.SetInternalEditValue(const Value: Variant);
begin
  inherited;
  FindPanel.Apply;
end;

procedure TcxFindPanelMRUEdit.UndoPerformed;
begin
  inherited UndoPerformed;
  FindPanel.DelayedFind;
end;

function TcxFindPanelMRUEdit.GetProperties: TcxFindPanelMRUEditProperties;
begin
  Result := TcxFindPanelMRUEditProperties(inherited Properties);
end;

procedure TcxFindPanelMRUEdit.WMMove(var Message: TWMMove);
begin
  inherited;
  DroppedDown := False;
end;

{ TcxGridFindPanelOptions }

constructor TcxFindPanelOptions.Create(AFindPanel: TcxCustomFindPanel);
begin
  inherited Create;
  CreateMRUItems;
  FApplyInputDelay := 1000;
  FClearOnClose := True;
  FDisplayMode := fpdmNever;
  FFindPanel := AFindPanel;
  FHighlightSearchResults := True;
  FInfoText := FindPanel.DefaultInfoText;
  FMRUItemsListDropDownCount := FindPanel.DefaultMRUItemsListDropDownCount;
  FMRUItemsListCount := FindPanel.DefaultMRUItemsListCount;
  FShowClearButton := True;
  FShowCloseButton := True;
  FShowFindButton := True;
  FShowNextButton := True;
  FShowPreviousButton := True;
  FUseDelayedFind := True;
end;

destructor TcxFindPanelOptions.Destroy;
begin
  DestroyMRUItems;
  inherited Destroy;
end;

procedure TcxFindPanelOptions.Assign(Source: TPersistent);
var
  AOptions: TcxFindPanelOptions;
begin
  if Source is TcxFindPanelOptions then
  begin
    AOptions := TcxFindPanelOptions(Source);
    ApplyInputDelay := AOptions.ApplyInputDelay;
    Behavior := AOptions.Behavior;
    ClearOnClose := AOptions.ClearOnClose;
    DisplayMode := AOptions.DisplayMode;
    FocusContentOnApply := AOptions.FocusContentOnApply;
    HighlightSearchResults := AOptions.HighlightSearchResults;
    InfoText := AOptions.InfoText;
    MRUItems := AOptions.MRUItems;
    MRUItemsListCount := AOptions.MRUItemsListCount;
    MRUItemsListDropDownCount := AOptions.MRUItemsListDropDownCount;
    Position := AOptions.Position;
    ShowClearButton := AOptions.ShowClearButton;
    ShowCloseButton := AOptions.ShowCloseButton;
    ShowFindButton := AOptions.ShowFindButton;
    ShowNextButton := AOptions.ShowNextButton;
    ShowPreviousButton := AOptions.ShowPreviousButton;
    UseDelayedFind := AOptions.UseDelayedFind;
    UseExtendedSyntax := AOptions.UseExtendedSyntax;
    Layout := AOptions.Layout;
  end;
end;

procedure TcxFindPanelOptions.UpdateMRUItems;
begin
  FIsUpdatingMRUItems := True;
  try
    if FindPanel.Visible then
      MRUItems := FindPanel.EditLookupItems
    else
      CheckMRUItemListCount;
  finally
    FIsUpdatingMRUItems := False;
  end;
end;

procedure TcxFindPanelOptions.Changed;
begin
  FindPanel.Changed;
end;

procedure TcxFindPanelOptions.CheckMRUItemListCount;
begin
  if MRUItemsListCount > 0 then
    while MRUItems.Count > MRUItemsListCount do
      MRUItems.Delete(MRUItems.Count - 1);
end;

function TcxFindPanelOptions.GetBehavior: TcxDataFindCriteriaBehavior;
begin
  Result := FindPanel.Criteria.Behavior;
end;

function TcxFindPanelOptions.GetMRUItems: TStrings;
begin
  Result := FMRUItems;
end;

function TcxFindPanelOptions.GetUseExtendedSyntax: Boolean;
begin
  Result := FindPanel.Criteria.UseExtendedSyntax;
end;

procedure TcxFindPanelOptions.SetApplyInputDelay(AValue: Integer);
begin
  if ApplyInputDelay <> AValue then
  begin
    FApplyInputDelay := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetBehavior(AValue: TcxDataFindCriteriaBehavior);
begin
  FindPanel.Criteria.Behavior := AValue;
end;

procedure TcxFindPanelOptions.SetClearOnClose(AValue: Boolean);
begin
  if ClearOnClose <> AValue then
  begin
    FClearOnClose := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetDisplayMode(AValue: TcxFindPanelDisplayMode);
begin
  if DisplayMode <> AValue then
  begin
    FDisplayMode := AValue;
    FindPanel.DisplayModeChanged;
  end;
end;

procedure TcxFindPanelOptions.SetContentOnApply(AValue: Boolean);
begin
  if FocusContentOnApply <> AValue then
  begin
    FFocusContentOnApply := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetHighlightSearchResults(AValue: Boolean);
begin
  if HighlightSearchResults <> AValue then
  begin
    FHighlightSearchResults := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetLayout(AValue: TcxFindPanelLayout);
begin
  if Layout <> AValue then
  begin
    FLayout := AValue;
    FindPanel.UpdateEditClearButton;
    FindPanel.UpdateEditFindButton;
    FindPanel.UpdateEditNextButton;
    FindPanel.UpdateEditPreviousButton;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetMRUItems(AValue: TStrings);
begin
  MRUItems.Assign(AValue);
end;

procedure TcxFindPanelOptions.SetMRUItemsListCount(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if MRUItemsListCount <> AValue then
  begin
    FMRUItemsListCount := AValue;
    FindPanel.UpdateMRUItems;
  end;
end;

procedure TcxFindPanelOptions.SetMRUItemsListDropDownCount(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if MRUItemsListDropDownCount <> AValue then
  begin
    FMRUItemsListDropDownCount := AValue;
    FindPanel.UpdateEditMRUItems;
  end;
end;

procedure TcxFindPanelOptions.SetPosition(AValue: TcxFindPanelPosition);
begin
  if Position <> AValue then
  begin
    FPosition := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowClearButton(AValue: Boolean);
begin
  if ShowClearButton <> AValue then
  begin
    FShowClearButton := AValue;
    FindPanel.UpdateEditClearButton;
    FindPanel.ValidateFocusedItem;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowCloseButton(AValue: Boolean);
begin
  if ShowCloseButton <> AValue then
  begin
    FShowCloseButton := AValue;
    FindPanel.ValidateFocusedItem;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowFindButton(AValue: Boolean);
begin
  if ShowFindButton <> AValue then
  begin
    FShowFindButton := AValue;
    FindPanel.UpdateEditFindButton;
    FindPanel.ValidateFocusedItem;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowNextButton(AValue: Boolean);
begin
  if ShowNextButton <> AValue then
  begin
    FShowNextButton := AValue;
    FindPanel.UpdateEditNextButton;
    FindPanel.ValidateFocusedItem;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowPreviousButton(AValue: Boolean);
begin
  if ShowPreviousButton <> AValue then
  begin
    FShowPreviousButton := AValue;
    FindPanel.UpdateEditPreviousButton;
    FindPanel.ValidateFocusedItem;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetUseDelayedFind(AValue: Boolean);
begin
  if UseDelayedFind <> AValue then
  begin
    FUseDelayedFind := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetUseExtendedSyntax(AValue: Boolean);
begin
  FindPanel.Criteria.UseExtendedSyntax := AValue;
end;

procedure TcxFindPanelOptions.CreateMRUItems;
begin
  FMRUItems := TStringList.Create;
  FMRUItems.OnChange := MRUItemsChangeHandler;
end;

procedure TcxFindPanelOptions.DestroyMRUItems;
begin
  FreeAndNil(FMRUItems);
end;

procedure TcxFindPanelOptions.MRUItemsChangeHandler(Sender: TObject);
begin
  if not FIsUpdatingMRUItems then
    FindPanel.UpdateMRUItems;
end;

function TcxFindPanelOptions.GetInfoText: string;
begin
  if FInfoTextAssigned then
    Result := FInfoText
  else
    Result := FindPanel.DefaultInfoText;
end;

procedure TcxFindPanelOptions.SetInfoText(AValue: string);
begin
  if FInfoText <> AValue then
  begin
    FInfoText := AValue;
    FInfoTextAssigned := FInfoText <> FindPanel.DefaultInfoText;
    FindPanel.UpdateEditTextHint;
  end;
end;

{ TcxFindPanel }

constructor TcxCustomFindPanel.Create;
begin
  inherited Create;
  FOptions := CreateOptions;
end;

destructor TcxCustomFindPanel.Destroy;
begin
  DestroyTimer;
  DestroyEdit;
  DestroyOptions;
  inherited Destroy;
end;

procedure TcxCustomFindPanel.ClearButtonExecute;
begin
  Clear;
  Edit.SetFocus;
end;

procedure TcxCustomFindPanel.CloseButtonExecute;
begin
  Hide;
end;

procedure TcxCustomFindPanel.ControlFocusChanged(AIsFocused: Boolean);
begin
  if IsButtonFocused and not AIsFocused then
    FocusedItem := fpikNone
  else if (FocusedItem = fpikFindEdit) and not IsEditFocused then
    Edit.SetFocusAndSelectAll;
end;

procedure TcxCustomFindPanel.CriteriaChanged
  (AChanges: TcxDataFindCriteriaChanges);
begin
  if fccText in AChanges then
    UpdateEditValue;
  UpdateViewInfo(fccBehavior in AChanges);
end;

procedure TcxCustomFindPanel.DelayedFind;
begin
  if Options.UseDelayedFind then
    StartDelayedFind;
end;

procedure TcxCustomFindPanel.DisplayModeChanged;
begin
  case Options.DisplayMode of
    fpdmManual:
      if IsDesigning then
        Hide
      else if Visible then
        Changed;
    fpdmAlways:
      if Visible then
        Changed
      else
        Show(False);
  else
    Hide
  end;
end;

procedure TcxCustomFindPanel.EditFocusChanged;
begin
  if IsEditFocused then
    FocusedItem := fpikFindEdit
  else if FocusedItem = fpikFindEdit then
    FocusedItem := fpikNone;
end;

procedure TcxCustomFindPanel.FindButtonExecute;
begin
  Apply;
  AddTextInEditMRUItems;
  Edit.SetFocusAndSelectAll;
end;

procedure TcxCustomFindPanel.FindEditExecute;
begin
  Apply;
end;

function TcxCustomFindPanel.GetItemCaption(AItem: TcxFindPanelItemKind): string;
begin
  if HasItemCaption(AItem) then
    Result := DoGetItemCaption(AItem)
  else
    Result := '';
end;

procedure TcxCustomFindPanel.Hide;
begin
  if CanHide then
    Visible := False;
end;

procedure TcxCustomFindPanel.HideEdit;
begin
  if HasEdit then
    Edit.Hide;
end;

function TcxCustomFindPanel.IsFocused: Boolean;
begin
  Result := FocusedItem <> fpikNone;
end;

function TcxCustomFindPanel.IsEditFocused: Boolean;
begin
  Result := Edit.IsFocused;
end;

function TcxCustomFindPanel.IsItemEnabled(AItem: TcxFindPanelItemKind): Boolean;
begin
  Result := AItem <> fpikNone;
  if Result then
    case AItem of
      fpikNextButton:
        Result := Criteria.GetNextMatchIndex <> -1;
      fpikPreviousButton:
        Result := Criteria.GetPreviousMatchIndex <> -1;
    end;
end;

function TcxCustomFindPanel.IsItemFocused(AItem: TcxFindPanelItemKind): Boolean;
begin
  Result := FocusedItem = AItem;
end;

function TcxCustomFindPanel.IsItemVisible(AItem: TcxFindPanelItemKind): Boolean;
begin
  Result := AItem <> fpikNone;
  if Result then
    case AItem of
      fpikCloseButton:
        Result := Options.ShowCloseButton and
          (Options.DisplayMode <> fpdmAlways);
      fpikFindButton:
        Result := Options.ShowFindButton and (ActualLayout = fplDefault) and
          (Options.Behavior <> fcbSearch);
      fpikClearButton:
        Result := Options.ShowClearButton and (ActualLayout = fplDefault) and
          (Options.Behavior <> fcbSearch);
      fpikNextButton:
        Result := Options.ShowNextButton and (ActualLayout = fplDefault) and
          (Options.Behavior = fcbSearch);
      fpikPreviousButton:
        Result := Options.ShowPreviousButton and (ActualLayout = fplDefault) and
          (Options.Behavior = fcbSearch);
    end;
end;

procedure TcxCustomFindPanel.ItemExecute(AItem: TcxFindPanelItemKind);
begin
  case AItem of
    fpikCloseButton:
      CloseButtonExecute;
    fpikFindButton:
      FindButtonExecute;
    fpikClearButton:
      ClearButtonExecute;
    fpikNextButton:
      NextButtonExecute;
    fpikPreviousButton:
      PreviousButtonExecute;
    fpikFindEdit:
      FindEditExecute;
  end;
end;

procedure TcxCustomFindPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        if (FocusedItem = fpikFindEdit) and (Options.Behavior = fcbSearch) and
          (Edit.Text = Text) and not Edit.HasPopupWindow then
          if Shift = [ssShift] then
            Criteria.GoToPreviousMatch
          else
            Criteria.GoToNextMatch
        else
        begin
          if FocusedItem <> fpikFindEdit then
            Key := 0;
          FocusedItemExecute;
        end;
      end;
    VK_SPACE:
      if IsButtonFocused then
        FocusedItemExecute;
    VK_TAB:
      begin
        if Shift = [ssShift] then
          FocusPreviousItem
        else
          FocusNextItem;
        Key := 0;
      end;
    VK_DOWN:
      begin
        if not(ssAlt in Shift) and not Edit.HasPopupWindow then
        begin
          FocusControlContent;
          Key := 0;
        end;
      end;
    VK_ESCAPE:
      begin
        if VarIsNull(Edit.EditingValue) or (Edit.EditingValue = '') then
          Hide
        else
          Clear;
        Key := 0;
      end;
    VK_F3:
      if (Options.Behavior = fcbSearch) and (FocusedItem = fpikFindEdit) then
      begin
        if Shift = [ssShift] then
          Criteria.GoToPreviousMatch
        else
          Criteria.GoToNextMatch;
        Key := 0;
      end;
  end;
  if NeedDelayedFindOnKeyDown(Key, Shift) then
    DelayedFind;
end;

procedure TcxCustomFindPanel.KeyPress(var Key: Char);
begin
  // do nothing
end;

procedure TcxCustomFindPanel.KeyUp(var Key: Word; Shift: TShiftState);
begin
  // do nothing
end;

procedure TcxCustomFindPanel.LookAndFeelChanged;
begin
  UpdateStyles;
end;

function TcxCustomFindPanel.NeedDelayedFindOnKeyDown(Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := not((Key in [0, VK_RETURN, VK_F4]) or (ssAlt in Shift) and
    (Key in [VK_DOWN, VK_UP]));
end;

procedure TcxCustomFindPanel.NextButtonExecute;
begin
  Criteria.GoToNextMatch;
end;

procedure TcxCustomFindPanel.PreviousButtonExecute;
begin
  Criteria.GoToPreviousMatch;
end;

procedure TcxCustomFindPanel.Show(AFocusEdit: Boolean = True);
begin
  if CanShow then
  begin
    Visible := True;
    if AFocusEdit then
      FocusedItem := fpikFindEdit;
  end;
end;

procedure TcxCustomFindPanel.ShowEdit;
begin
  Edit.Show;
  UpdateStyles;
end;

procedure TcxCustomFindPanel.StylesChanged;
begin
  UpdateStyles;
end;

procedure TcxCustomFindPanel.UpdateEditBounds(const ABounds: TRect);
begin
  Edit.BoundsRect := ABounds;
end;

procedure TcxCustomFindPanel.UpdateEditMRUItems;
begin
  if Visible then
  begin
    Edit.Properties.LookupItems := Options.MRUItems;
    Edit.Properties.MaxItemCount := Options.MRUItemsListCount;
    Edit.Properties.DropDownRows := Options.MRUItemsListDropDownCount;
  end;
end;

procedure TcxCustomFindPanel.UpdateEditTextHint;
begin
  if Visible then
    Edit.TextHint := Options.InfoText;
end;

procedure TcxCustomFindPanel.UpdateMRUItems;
begin
  UpdateEditMRUItems;
  UpdateOptionsMRUItems;
end;

procedure TcxCustomFindPanel.UpdateOptionsMRUItems;
begin
  Options.UpdateMRUItems;
end;

procedure TcxCustomFindPanel.UpdateViewInfo(ARecalculate: Boolean = False);
begin
  UpdateEditClearButton;
  UpdateEditFindButton;
  UpdateEditInfoButton;
  UpdateEditNextButton;
  UpdateEditPreviousButton;
  ValidateFocusedItem;
  Invalidate(ARecalculate);
end;

procedure TcxCustomFindPanel.AddTextInEditMRUItems;
begin
  Edit.AddItem(Text);
end;

procedure TcxCustomFindPanel.ApplyText(const AText: string);
begin
  Text := AText;
end;

function TcxCustomFindPanel.CanItemFocus(AItem: TcxFindPanelItemKind): Boolean;
begin
  Result := IsItemVisible(AItem) and IsItemEnabled(AItem);
end;

procedure TcxCustomFindPanel.Apply;
begin
  ApplyText(Edit.Text);
end;

function TcxCustomFindPanel.CanShow;
begin
  Result := Options.DisplayMode in [fpdmManual, fpdmAlways];
end;

function TcxCustomFindPanel.CanHide;
begin
  Result := Options.DisplayMode in [fpdmNever, fpdmManual];
end;

procedure TcxCustomFindPanel.Clear;
begin
  if Text = '' then
  begin
    DestroyTimer;
    if Visible then
      UpdateEditValue;
  end
  else
    ClearText;
end;

procedure TcxCustomFindPanel.ClearText;
begin
  Text := '';
end;

function TcxCustomFindPanel.DoGetItemCaption
  (AItem: TcxFindPanelItemKind): string;
begin
  case AItem of
    fpikFindButton:
      Result := 'Find';
    fpikClearButton:
      Result := 'Clear';
    fpikNextButton:
      Result := 'Next';
    fpikPreviousButton:
      Result := 'Previous';
  else
    Result := '';
  end;
end;

procedure TcxCustomFindPanel.EditDestroyed;
begin
  FEdit := nil;
  if not IsDestroying then
  begin
    FEdit := CreateEdit;
    UpdateEdit;
  end;
end;

procedure TcxCustomFindPanel.FocusControlContent;
begin
  FocusedItem := fpikNone;
  Control.SetFocus;
end;

procedure TcxCustomFindPanel.FocusedItemChanged;
begin
  if IsButtonFocused then
    Control.SetFocus
  else
  begin
    if FocusedItem = fpikFindEdit then
      Edit.SetFocusAndSelectAll;
    Control.DoFocusChanged;
  end;
  Invalidate;
end;

procedure TcxCustomFindPanel.FocusedItemExecute;
begin
  ItemExecute(FocusedItem);
end;

procedure TcxCustomFindPanel.FocusNextItem;
begin
  case FocusedItem of
    fpikNone, fpikCloseButton:
      FocusedItem := fpikFindEdit;
    fpikFindEdit:
      FocusedItem := fpikFindButton;
    fpikFindButton:
      FocusedItem := fpikClearButton;
    fpikClearButton:
      FocusedItem := fpikPreviousButton;
    fpikPreviousButton:
      FocusedItem := fpikNextButton;
    fpikNextButton:
      FocusedItem := fpikCloseButton;
  end;
  ValidateFocusedItem(fvtFocusNext);
end;

procedure TcxCustomFindPanel.FocusPreviousItem;
begin
  case FocusedItem of
    fpikCloseButton:
      FocusedItem := fpikNextButton;
    fpikFindEdit:
      FocusedItem := fpikCloseButton;
    fpikFindButton, fpikNone:
      FocusedItem := fpikFindEdit;
    fpikClearButton:
      FocusedItem := fpikFindButton;
    fpikPreviousButton:
      FocusedItem := fpikClearButton;
    fpikNextButton:
      FocusedItem := fpikPreviousButton;
  end;
  ValidateFocusedItem(fvtFocusPrev);
end;

function TcxCustomFindPanel.GetActualLayout: TcxFindPanelLayout;
begin
  Result := Options.Layout;
end;

function TcxCustomFindPanel.GetButtonsFirstOffset: Integer;
begin
  if ActualLayout = fplCompact then
    Result := ButtonsFirstOffsetForCompactLayout
  else
    Result := ButtonsFirstOffset;
end;

function TcxCustomFindPanel.GetDefaultInfoText: string;
begin
  Result := 'Enter text to search...';
end;

function TcxCustomFindPanel.GetDefaultMRUItemsListCount: Integer;
begin
  Result := cxFindPanelDefaultMRUItemsListCount;
end;

function TcxCustomFindPanel.GetDefaultMRUItemsListDropDownCount: Integer;
begin
  Result := cxFindPanelDefaultMRUItemsListDropDownCount;
end;

function TcxCustomFindPanel.GetEditMaxWidth: Integer;
begin
  if ActualLayout = fplCompact then
    Result := EditMaxWidthForCompactLayout
  else
    Result := EditMaxWidth;
end;

function TcxCustomFindPanel.GetInfoButtonText: string;
var
  AIndexStr, ACountStr: string;
begin
  AIndexStr := IntToStr(Criteria.GetCurrentMatchIndex + 1);
  ACountStr := IntToStr(Criteria.MatchCount);
  Result := AIndexStr + '/' + ACountStr;
end;

function TcxCustomFindPanel.HasEdit: Boolean;
begin
  Result := FEdit <> nil;
end;

function TcxCustomFindPanel.HasItemCaption(AItem: TcxFindPanelItemKind)
  : Boolean;
begin
  Result := (AItem in [fpikFindButton, fpikClearButton, fpikPreviousButton,
    fpikNextButton]) and (ActualLayout = fplDefault);
end;

procedure TcxCustomFindPanel.Invalidate(ARecalculate: Boolean = False);
begin
  // do nothing
end;

function TcxCustomFindPanel.IsButtonFocused: Boolean;
begin
  Result := FocusedItem in [fpikCloseButton .. fpikNextButton];
end;

function TcxCustomFindPanel.IsDesigning: Boolean;
begin
  Result := csDesigning in Owner.ComponentState;
end;

function TcxCustomFindPanel.IsDestroying: Boolean;
begin
  Result := csDestroying in Owner.ComponentState;
end;

procedure TcxCustomFindPanel.StartDelayedFind;
begin
  DestroyTimer;
  CreateTimer;
end;

procedure TcxCustomFindPanel.UpdateEdit;
begin
  UpdateStyles;
  UpdateEditMRUItems;
  UpdateEditValue;
  UpdateEditClearButton;
  UpdateEditFindButton;
  UpdateEditInfoButton;
  UpdateEditNextButton;
  UpdateEditPreviousButton;
  UpdateEditTextHint;
end;

procedure TcxCustomFindPanel.UpdateEditClearButton;
begin
  Edit.Properties.ClearButton.Visible := Options.ShowClearButton and
    (Edit.Text <> '') and ((Options.Behavior = fcbSearch) or
    (ActualLayout = fplCompact));
end;

procedure TcxCustomFindPanel.UpdateEditFindButton;
begin
  Edit.Properties.FindButton.Visible := (ActualLayout = fplCompact) and
    Options.ShowFindButton and (Criteria.Behavior <> fcbSearch);
end;

procedure TcxCustomFindPanel.UpdateEditInfoButton;
begin
  Edit.Properties.InfoButton.Visible := (Criteria.Behavior = fcbSearch) and
    (Text <> '');
  if Edit.Properties.InfoButton.Visible then
    Edit.Properties.InfoButton.Caption := GetInfoButtonText
  else
    Edit.Properties.InfoButton.Caption := '';
end;

procedure TcxCustomFindPanel.UpdateEditLookAndFeel;
begin
  Edit.UpdateLookAndFeel(LookAndFeel);
end;

procedure TcxCustomFindPanel.UpdateEditNextButton;
begin
  Edit.Properties.NextButton.Visible := (ActualLayout = fplCompact) and
    Options.ShowNextButton and (Criteria.Behavior = fcbSearch);
  Edit.Properties.NextButton.Enabled := Edit.Properties.NextButton.Visible and
    (Criteria.GetNextMatchIndex <> -1);
end;

procedure TcxCustomFindPanel.UpdateEditPreviousButton;
begin
  Edit.Properties.PreviousButton.Visible := (ActualLayout = fplCompact) and
    Options.ShowPreviousButton and (Criteria.Behavior = fcbSearch);
  Edit.Properties.PreviousButton.Enabled :=
    Edit.Properties.PreviousButton.Visible and
    (Criteria.GetPreviousMatchIndex <> -1);
end;

procedure TcxCustomFindPanel.UpdateEditStyles;
var
  AParams, AContentParams: TcxViewParams;
begin
  GetViewParams(AParams);
  GetControlContentViewParams(AContentParams);
  Edit.UpdateStyles(AParams, AContentParams);
end;

procedure TcxCustomFindPanel.UpdateEditValue;
var
  AEditValue: TcxEditValue;
begin
  if Text = '' then
    AEditValue := null
  else
    AEditValue := Text;
  Edit.EditValue := AEditValue;
end;

procedure TcxCustomFindPanel.UpdateStyles;
begin
  UpdateEditLookAndFeel;
  UpdateEditStyles;
end;

procedure TcxCustomFindPanel.ValidateFocusedItem
  (AType: TcxFindPanelValidateFocusedItemType = fvtFocusEdit);
begin
  if (FocusedItem = fpikNone) or CanItemFocus(FocusedItem) then
    Exit;
  case AType of
    fvtFocusNext:
      FocusNextItem;
    fvtFocusPrev:
      FocusPreviousItem;
  else
    FocusedItem := fpikFindEdit;
  end;
end;

procedure TcxCustomFindPanel.VisibilityChanged;
begin
  if Visible then
    UpdateEdit
  else
  begin
    if IsFocused then
    begin
      FocusedItem := fpikNone;
      Control.SetFocus;
    end;
    if Options.ClearOnClose then
      Clear;
  end;
  Changed;
end;

function TcxCustomFindPanel.CreateEdit: TcxFindPanelMRUEdit;
begin
  Result := TcxFindPanelMRUEdit.Create(Self);
end;

function TcxCustomFindPanel.CreateOptions: TcxFindPanelOptions;
begin
  Result := TcxFindPanelOptions.Create(Self);
end;

procedure TcxCustomFindPanel.DestroyEdit;
begin
  FreeAndNil(FEdit);
end;

procedure TcxCustomFindPanel.DestroyOptions;
begin
  FreeAndNil(FOptions);
end;

function TcxCustomFindPanel.GetEdit: TcxFindPanelMRUEdit;
begin
  if not HasEdit then
    FEdit := CreateEdit;
  Result := FEdit;
end;

function TcxCustomFindPanel.GetEditHeight: Integer;
begin
  Result := Edit.CalculateMinHeight;
end;

function TcxCustomFindPanel.GetEditLookupItems: TStrings;
begin
  Result := Edit.Properties.LookupItems;
end;

function TcxCustomFindPanel.GetText: string;
begin
  Result := Criteria.Text;
end;

procedure TcxCustomFindPanel.SetFocusedItem(AValue: TcxFindPanelItemKind);
begin
  if FocusedItem <> AValue then
  begin
    FFocusedItem := AValue;
    if not IsDestroying then
      FocusedItemChanged;
  end;
end;

procedure TcxCustomFindPanel.SetOptions(AValue: TcxFindPanelOptions);
begin
  Options.Assign(AValue);
end;

procedure TcxCustomFindPanel.SetText(const AValue: string);
var
  ANeedFocusContentOnApply: Boolean;
begin
  ANeedFocusContentOnApply := (Criteria.Text <> AValue) and
    Options.FocusContentOnApply;
  Criteria.Text := AValue;
  if ANeedFocusContentOnApply then
    FocusControlContent;
end;

procedure TcxCustomFindPanel.SetVisible(AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    VisibilityChanged;
  end;
end;

procedure TcxCustomFindPanel.CreateTimer;
begin
  FDelayedFindTimer := TcxTimer.Create(nil);
  FDelayedFindTimer.Interval := Options.ApplyInputDelay;
  FDelayedFindTimer.OnTimer := OnDelayedFindTimer;
end;

procedure TcxCustomFindPanel.DestroyTimer;
begin
  FreeAndNil(FDelayedFindTimer);
end;

procedure TcxCustomFindPanel.OnDelayedFindTimer(Sender: TObject);
begin
  DestroyTimer;
  Apply;
end;

end.
