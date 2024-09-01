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

unit dxFormattedLabel;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxEditUtils, cxClasses,
  cxGeometry, dxThemeManager, cxEditConsts, cxDataUtils, dxFormattedText,
  cxDrawTextUtils;

type
  TdxCustomFormattedLabel = class;

  { TdxFormattedLabelEditStyle }

  TdxFormattedLabelEditStyle = class(TcxEditStyle)
  protected
    function DefaultBorderStyle: TcxContainerBorderStyle; override;
    function DefaultHotTrack: Boolean; override;
  end;

  { TdxCustomFormattedLabelViewInfo }

  TdxCustomFormattedLabelViewInfo = class(TcxCustomTextEditViewInfo)
  strict private
    FAlignment: TcxEditAlignment;
    FFormattedText: TdxFormattedText;
    FHyperlinkColor: TColor;
    FInplaceOffset: TPoint;
    FWordWrap: Boolean;
  protected
    function GetURI(X, Y: Integer): string;
    function IsMouseOverHyperlink(X, Y: Integer): Boolean;

    property Alignment: TcxEditAlignment read FAlignment write FAlignment;
    property FormattedText: TdxFormattedText read FFormattedText;
    property HyperlinkColor: TColor read FHyperlinkColor write FHyperlinkColor;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  public
    FocusRect: TRect;
    LeftTop: TPoint;

    constructor Create; override;
    destructor Destroy; override;
    procedure DrawLabel(ACanvas: TcxCanvas);
    function GetTextBaseLine: Integer; override;
    procedure Offset(DX, DY: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  { TdxCustomFormattedLabelViewData }

  TdxCustomFormattedLabelProperties = class;

  TdxCustomFormattedLabelViewData = class(TcxCustomEditViewData)
  strict private
    procedure CalculateLabelViewInfoProps(AViewInfo: TcxCustomEditViewInfo);
    function GetProperties: TdxCustomFormattedLabelProperties;
    function GetShowEndEllipsis: Boolean;
  protected
    function CalculatePaintOptions: TcxEditPaintOptions; override;
    function CalculateTextSize(ACanvas: TcxCanvas; const R: TRect;
      AFormattedText: TdxFormattedText): TSize;
    procedure CalculateViewParams(ACanvas: TcxCanvas;
      AViewInfo: TdxCustomFormattedLabelViewInfo);
    function GetDrawTextFlags: Integer;
    function GetIsEditClass: Boolean;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue)
      : string; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    function GetEditContentSize(ACanvas: TcxCanvas;
      const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties;
      AErrorData: TcxEditValidateInfo = nil): TSize; override;

    property Properties: TdxCustomFormattedLabelProperties read GetProperties;
    property ShowEndEllipsis: Boolean read GetShowEndEllipsis;
  end;

  { TdxCustomFormattedLabelProperties }

  TdxFormattedLabelHyperlinkClickEvent = procedure(Sender: TObject;
    const AURI: string; var AHandled: Boolean) of object;
  TdxFormattedLabelHotTrackHyperlinkEvent = procedure(Sender: TObject;
    const AURI: string) of object;

  TdxCustomFormattedLabelProperties = class(TcxCustomEditProperties)
  strict private
    FHyperlinkColor: TColor;
    FShowEndEllipsis: Boolean;
    FWordWrap: Boolean;
    FOnHyperlinkClick: TdxFormattedLabelHyperlinkClickEvent;
    FOnHotTrackHyperlink: TdxFormattedLabelHotTrackHyperlinkEvent;

    procedure SetHyperlinkColor(AValue: TColor);
    procedure SetShowEndEllipsis(AValue: Boolean);
    procedure SetWordWrap(AValue: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function DoHyperlinkClick(ASender: TObject; const AURI: string): Boolean;
    procedure DoHotTrackHyperlink(ASender: TObject; const AURI: string);
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    property OnHotTrackHyperlink: TdxFormattedLabelHotTrackHyperlinkEvent
      read FOnHotTrackHyperlink write FOnHotTrackHyperlink;
  public
    constructor Create(AOwner: TPersistent); override;
    function CanCompareEditValue: Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetEditValueSource(AEditFocused: Boolean)
      : TcxDataEditValueSource; override;
    class function GetStyleClass: TcxCustomEditStyleClass; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsEditValueValid(var EditValue: TcxEditValue;
      AEditFocused: Boolean): Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;

    property HyperlinkColor: TColor read FHyperlinkColor write SetHyperlinkColor
      default clDefault;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis
      write SetShowEndEllipsis default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnHyperlinkClick: TdxFormattedLabelHyperlinkClickEvent
      read FOnHyperlinkClick write FOnHyperlinkClick;
  end;

  { TdxCustomFormattedLabel }

  TdxCustomFormattedLabel = class(TcxCustomEdit)
  strict private
    FLockCaption: Boolean;

    function GetActiveProperties: TdxCustomFormattedLabelProperties;
    function GetProperties: TdxCustomFormattedLabelProperties;
    function GetStyle: TdxFormattedLabelEditStyle;
    function GetViewInfo: TdxCustomFormattedLabelViewInfo;
    procedure SetProperties(Value: TdxCustomFormattedLabelProperties);
    procedure SetStyle(Value: TdxFormattedLabelEditStyle);
  protected
    // Input
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

    // TWinControl
    procedure Loaded; override;

    // TcxContainer
    function CanFocusOnClick: Boolean; override;
    function DefaultParentColor: Boolean; override;
    function GetDefaultSkinnedTextColor(AEnabled: Boolean): TColor; override;
    function GetEditStateColorKind: TcxEditStateColorKind; override;

    // TcxControl
    function GetBackgroundStyle: TcxControlBackgroundStyle; override;
    function GetCurrentCursor(X: Integer; Y: Integer): TCursor; override;

    // TcxCustomEdit
    function CanAutoSize: Boolean; override;
    function CanAutoHeight: Boolean; override;
    function CanAutoWidth: Boolean; override;
    function FadingCanFadeBackground: Boolean; override;
    procedure Initialize; override;
    procedure InternalSetEditValue(const Value: TcxEditValue;
      AValidateEditValue: Boolean); override;
    function IsHeightDependOnWidth: Boolean; override;
    procedure SetInternalDisplayValue(Value: TcxEditValue); override;
    procedure TextChanged; override;
    function UseAnchorX: Boolean; override;
    function UseAnchorY: Boolean; override;

    property ViewInfo: TdxCustomFormattedLabelViewInfo read GetViewInfo;
  public
    function CanFocus: Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetTextBaseLine: Integer; override;
    function HasTextBaseLine: Boolean; override;

    property ActiveProperties: TdxCustomFormattedLabelProperties
      read GetActiveProperties;
    property Properties: TdxCustomFormattedLabelProperties read GetProperties
      write SetProperties;
    property Style: TdxFormattedLabelEditStyle read GetStyle write SetStyle;
    property TabOrder stored False;
    property Transparent;
  end;

  { TdxFormattedLabelProperties }

  TdxFormattedLabelProperties = class(TdxCustomFormattedLabelProperties)
  published
    property Alignment;
    property HyperlinkColor;
    property ShowEndEllipsis;
    property WordWrap;
    property OnHyperlinkClick;
  end;

  { TdxFormattedLabel }

  TdxFormattedLabel = class(TdxCustomFormattedLabel)
  private
    function GetActiveProperties: TdxFormattedLabelProperties;
    function GetProperties: TdxFormattedLabelProperties;
    procedure SetProperties(Value: TdxFormattedLabelProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxFormattedLabelProperties
      read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property Properties: TdxFormattedLabelProperties read GetProperties
      write SetProperties;
    property TabOrder;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, dxCore,
  dxFormattedTextConverterBBCode,
  dxFormattedTextConverterRTF;

const
  TextFromBorderOffset = 1;

procedure CalculateCustomLabelViewInfo(ACanvas: TcxCanvas;
  AViewData: TdxCustomFormattedLabelViewData;
  AViewInfo: TdxCustomFormattedLabelViewInfo);

  procedure CheckFocusRectBounds;
  begin
    with AViewInfo do
    begin
      if FocusRect.Left < TextRect.Left - 1 then
        FocusRect.Left := TextRect.Left - 1;
      if FocusRect.Top < TextRect.Top - 1 then
        FocusRect.Top := TextRect.Top - 1;
      if FocusRect.Right > TextRect.Right + 1 then
        FocusRect.Right := TextRect.Right + 1;
      if FocusRect.Bottom > TextRect.Bottom + 1 then
        FocusRect.Bottom := TextRect.Bottom + 1;
    end;
  end;

begin
  with AViewInfo do
  begin
    if not IsInplace and Focused then
      if Length(Text) = 0 then
        FocusRect := cxEmptyRect
      else
      begin
        FocusRect := TextRect;
        InflateRect(FocusRect, 1, 1);
        CheckFocusRectBounds;
      end;
  end;
end;

{ TdxFormattedLabelEditStyle }

function TdxFormattedLabelEditStyle.DefaultBorderStyle: TcxContainerBorderStyle;
begin
  if ParentStyle = nil then
    Result := cbsNone
  else
    Result := inherited DefaultBorderStyle;
end;

function TdxFormattedLabelEditStyle.DefaultHotTrack: Boolean;
begin
  Result := False;
end;

{ TdxCustomFormattedLabelViewInfo }

constructor TdxCustomFormattedLabelViewInfo.Create;
begin
  inherited;
  FFormattedText := TdxFormattedText.Create;
  FFormattedText.MeasureTrailingSpaces := True;
  FAlignment := TcxEditAlignment.Create(nil);
  FInplaceOffset := Point(cxMaxRectSize, cxMaxRectSize);
end;

destructor TdxCustomFormattedLabelViewInfo.Destroy;
begin
  FreeAndNil(FAlignment);
  FreeAndNil(FFormattedText);
  inherited;
end;

procedure TdxCustomFormattedLabelViewInfo.DrawLabel(ACanvas: TcxCanvas);
begin
  FormattedText.Draw(ACanvas.Canvas, TextRect.TopLeft);
end;

function TdxCustomFormattedLabelViewInfo.GetTextBaseLine: Integer;
begin
  Result := TextRect.Top + FormattedText.GetBaseLine(0) + 1;
end;

procedure TdxCustomFormattedLabelViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  OffsetRect(FocusRect, DX, DY);
end;

procedure TdxCustomFormattedLabelViewInfo.Paint(ACanvas: TcxCanvas);
begin
  DrawCustomEdit(ACanvas, True, False);
  DrawLabel(ACanvas);
end;

function TdxCustomFormattedLabelViewInfo.GetURI(X, Y: Integer): string;
begin
  Result := FormattedText.GetUrl(FormattedText.HitTest(Point(X - TextRect.Left,
    Y - TextRect.Top)).HyperlinkIndex);
end;

function TdxCustomFormattedLabelViewInfo.IsMouseOverHyperlink(X,
  Y: Integer): Boolean;
var
  ATextBox: TdxFormattedTextLayoutBox;
begin
  ATextBox := FormattedText.HitTest(Point(X - TextRect.Left, Y - TextRect.Top));
  Result := (ATextBox <> nil) and ATextBox.IsHyperlink;
end;

{ TdxCustomFormattedLabelViewData }

procedure TdxCustomFormattedLabelViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  ALabelViewInfo: TdxCustomFormattedLabelViewInfo;
begin
  inherited;
  if ((ABounds.Bottom >= cxMaxRectSize) or (ABounds.Right >= cxMaxRectSize)) and IsInplace
  then
    Exit;
  ALabelViewInfo := TdxCustomFormattedLabelViewInfo(AViewInfo);

  ALabelViewInfo.IsEditClass := GetIsEditClass;
  ALabelViewInfo.DrawSelectionBar := False;
  ALabelViewInfo.HasPopupWindow := False;
  ALabelViewInfo.DrawTextFlags := GetDrawTextFlags;
  if AreVisualStylesMustBeUsed(ALabelViewInfo.NativeStyle, totButton) and
    (ALabelViewInfo.BorderStyle = ebsNone) then
    ALabelViewInfo.NativeStyle := False;
  if not IsInplace then
    ALabelViewInfo.Transparent := TdxCustomFormattedLabel(Edit).Transparent;

  CalculateLabelViewInfoProps(ALabelViewInfo);
  CalculateViewParams(ACanvas, ALabelViewInfo);
  CalculateCustomLabelViewInfo(ACanvas, Self, ALabelViewInfo);
  if not IsInplace then
    ALabelViewInfo.DrawSelectionBar := False;

  if Edit <> nil then
    ALabelViewInfo.LeftTop := Point(Edit.Left, Edit.Top);

  ACanvas.SaveState;
  try
    ACanvas.Font.Assign(Style.GetVisibleFont);
    ACanvas.Font.Color := ALabelViewInfo.TextColor;
    ALabelViewInfo.FormattedText.CalculateLayout(ACanvas.Canvas, ACanvas.Font,
      ALabelViewInfo.TextRect, ALabelViewInfo.DrawTextFlags or CXTO_CHARBREAK,
      ScaleFactor, 1, ALabelViewInfo.HyperlinkColor);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxCustomFormattedLabelViewData.EditValueToDrawValue(const AEditValue
  : TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  inherited;
  TdxCustomFormattedLabelViewInfo(AViewInfo).FormattedText.Import
    (EditValueToDisplayText(AEditValue), AViewInfo.Font);
end;

function TdxCustomFormattedLabelViewData.GetEditContentSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties;
  AErrorData: TcxEditValidateInfo): TSize;

  function CalculateLabelMinSize(ACanvas: TcxCanvas;
    AFormattedText: TdxFormattedText; const ATextRect: TRect;
    AContentSizeCorrection: TSize; AIsInplace, AWordWrap: Boolean): TSize;
  var
    ARect: TRect;
  begin
    ARect := cxRectOffset(ATextRect, -ATextRect.Left, -ATextRect.Top);
    if ARect.Right >= 0 then
    begin
      if AIsInplace then
        Dec(ARect.Right, AContentSizeCorrection.cx)
      else if Style.BorderStyle <> ebsNone then
        Dec(ARect.Right, TextFromBorderOffset * 2);
    end;
    Result := CalculateTextSize(ACanvas, ARect, AFormattedText);
    if AIsInplace then
    begin
      Inc(Result.cx, AContentSizeCorrection.cx);
      Inc(Result.cy, AContentSizeCorrection.cy);
    end
    else if Style.BorderStyle <> ebsNone then
      Inc(Result.cx, TextFromBorderOffset * 2);
  end;

var
  AFormattedText: TdxFormattedText;
begin
  ACanvas.SaveState;
  try
    ACanvas.Font := Style.GetVisibleFont;
    Result := Size(AEditSizeProperties.Width, 0);
    AFormattedText := TdxFormattedText.Create;
    try
      AFormattedText.MeasureTrailingSpaces := True;
      AFormattedText.Import(AEditValue, ACanvas.Font);
      Result := CalculateLabelMinSize(ACanvas, AFormattedText, cxRect(Result),
        GetEditContentSizeCorrection, IsInplace, Properties.WordWrap);
    finally
      AFormattedText.Free;
    end;
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxCustomFormattedLabelViewData.CalculatePaintOptions
  : TcxEditPaintOptions;
begin
  Result := inherited CalculatePaintOptions;
  if ShowEndEllipsis then
    Include(Result, epoShowEndEllipsis);
end;

function TdxCustomFormattedLabelViewData.CalculateTextSize(ACanvas: TcxCanvas;
  const R: TRect; AFormattedText: TdxFormattedText): TSize;
begin
  ACanvas.SaveState;
  try
    AFormattedText.CalculateLayout(ACanvas.Canvas, Style.GetVisibleFont, R,
      GetDrawTextFlags or CXTO_NOCLIP, ScaleFactor);
    Result := AFormattedText.TextSize;
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxCustomFormattedLabelViewData.CalculateViewParams
  (ACanvas: TcxCanvas; AViewInfo: TdxCustomFormattedLabelViewInfo);
begin
  if IsInplace then
    AViewInfo.TextRect := cxRectContent(AViewInfo.ClientRect,
      EditContentParams.Offsets)
  else
    AViewInfo.TextRect := AViewInfo.ClientRect;
end;

function TdxCustomFormattedLabelViewData.GetDrawTextFlags: Integer;

  function PrepareTextFlag(AHorzAlignments: TAlignment;
    AVertAlignments: TcxAlignmentVert;
    AShowEndEllipsis, AWordWrap: Boolean): Longint;
  const
    cxShowEndEllipsisArray: array [Boolean] of Integer = (0, cxShowEndEllipsis);
    cxWordWrapArray: array [Boolean] of Integer = (0, cxWordBreak);
  begin
    Result := cxAlignmentsHorz[AHorzAlignments] or cxAlignmentsVert
      [AVertAlignments] or cxShowEndEllipsisArray[AShowEndEllipsis] or
      cxWordWrapArray[AWordWrap];
  end;

begin
  Result := DrawTextFlagsTocxTextOutFlags
    (PrepareTextFlag(Properties.Alignment.Horz,
    TcxAlignmentVert(Ord(Properties.Alignment.Vert)), ShowEndEllipsis,
    Properties.WordWrap));
  if ShowEndEllipsis then
    Result := Result or CXTO_EDITCONTROL;
end;

function TdxCustomFormattedLabelViewData.GetIsEditClass: Boolean;
begin
  Result := False;
end;

function TdxCustomFormattedLabelViewData.InternalEditValueToDisplayText
  (AEditValue: TcxEditValue): string;
begin
  Result := VarToStr(AEditValue);
end;

procedure TdxCustomFormattedLabelViewData.CalculateLabelViewInfoProps
  (AViewInfo: TcxCustomEditViewInfo);
begin
  TdxCustomFormattedLabelViewInfo(AViewInfo).WordWrap := Properties.WordWrap;
  TdxCustomFormattedLabelViewInfo(AViewInfo)
    .Alignment.Assign(Properties.Alignment);
  TdxCustomFormattedLabelViewInfo(AViewInfo).HyperlinkColor :=
    Properties.HyperlinkColor;
end;

function TdxCustomFormattedLabelViewData.GetProperties
  : TdxCustomFormattedLabelProperties;
begin
  Result := TdxCustomFormattedLabelProperties(FProperties);
end;

function TdxCustomFormattedLabelViewData.GetShowEndEllipsis: Boolean;
begin
  Result := Properties.ShowEndEllipsis and
    ((Edit = nil) or not TdxCustomFormattedLabel(Edit).IsAutoWidth);
end;

{ TdxCustomFormattedLabelProperties }

constructor TdxCustomFormattedLabelProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FHyperlinkColor := clDefault;
end;

function TdxCustomFormattedLabelProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TdxCustomFormattedLabelProperties.GetContainerClass
  : TcxContainerClass;
begin
  Result := TdxFormattedLabel;
end;

function TdxCustomFormattedLabelProperties.GetDisplayText(const AEditValue
  : TcxEditValue; AFullText, AIsInplace: Boolean): string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  Result := ADisplayValue;
end;

function TdxCustomFormattedLabelProperties.GetEditValueSource
  (AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

class function TdxCustomFormattedLabelProperties.GetStyleClass
  : TcxCustomEditStyleClass;
begin
  Result := TdxFormattedLabelEditStyle;
end;

function TdxCustomFormattedLabelProperties.GetSupportedOperations
  : TcxEditSupportedOperations;
begin
  Result := [esoAutoHeight, esoShowingCaption, esoTransparency, esoAutoWidth];
end;

class function TdxCustomFormattedLabelProperties.GetViewInfoClass
  : TcxContainerViewInfoClass;
begin
  Result := TdxCustomFormattedLabelViewInfo;
end;

function TdxCustomFormattedLabelProperties.IsEditValueValid
  (var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := inherited IsEditValueValid(EditValue, AEditFocused);
end;

procedure TdxCustomFormattedLabelProperties.PrepareDisplayValue(const AEditValue
  : TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
var
  AFormattedText: TdxFormattedText;
begin
  AFormattedText := TdxFormattedText.Create;
  try
    AFormattedText.MeasureTrailingSpaces := True;
    AFormattedText.Import(AEditValue);
    DisplayValue := AFormattedText.GetDisplayText;
  finally
    AFormattedText.Free;
  end;
end;

procedure TdxCustomFormattedLabelProperties.DoAssign
  (AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TdxCustomFormattedLabelProperties then
  begin
    Self.ShowEndEllipsis := TdxCustomFormattedLabelProperties(AProperties)
      .ShowEndEllipsis;
    Self.WordWrap := TdxCustomFormattedLabelProperties(AProperties).WordWrap;
    Self.OnHyperlinkClick := TdxCustomFormattedLabelProperties(AProperties)
      .OnHyperlinkClick;
  end;
end;

function TdxCustomFormattedLabelProperties.DoHyperlinkClick(ASender: TObject;
  const AURI: string): Boolean;
begin
  Result := False;
  if Assigned(FOnHyperlinkClick) then
    FOnHyperlinkClick(ASender, AURI, Result);
end;

procedure TdxCustomFormattedLabelProperties.DoHotTrackHyperlink
  (ASender: TObject; const AURI: string);
begin
  if Assigned(FOnHotTrackHyperlink) then
    FOnHotTrackHyperlink(ASender, AURI);
end;

function TdxCustomFormattedLabelProperties.GetDisplayFormatOptions
  : TcxEditDisplayFormatOptions;
begin
  Result := [];
end;

class function TdxCustomFormattedLabelProperties.GetViewDataClass
  : TcxCustomEditViewDataClass;
begin
  Result := TdxCustomFormattedLabelViewData;
end;

function TdxCustomFormattedLabelProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

procedure TdxCustomFormattedLabelProperties.SetHyperlinkColor(AValue: TColor);
begin
  if FHyperlinkColor <> AValue then
  begin
    FHyperlinkColor := AValue;
    Changed;
  end;
end;

procedure TdxCustomFormattedLabelProperties.SetShowEndEllipsis(AValue: Boolean);
begin
  if FShowEndEllipsis <> AValue then
  begin
    FShowEndEllipsis := AValue;
    Changed;
  end;
end;

procedure TdxCustomFormattedLabelProperties.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap <> AValue then
  begin
    FWordWrap := AValue;
    Changed;
  end;
end;

{ TdxCustomFormattedLabel }

function TdxCustomFormattedLabel.CanFocus: Boolean;
begin
  Result := IsInplace;
end;

class function TdxCustomFormattedLabel.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomFormattedLabelProperties;
end;

function TdxCustomFormattedLabel.GetTextBaseLine: Integer;
begin
  Result := ViewInfo.GetTextBaseLine;
end;

function TdxCustomFormattedLabel.HasTextBaseLine: Boolean;
begin
  Result := True;
end;

procedure TdxCustomFormattedLabel.MouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
begin
  inherited;
  if ViewInfo.IsMouseOverHyperlink(X, Y) then
    ActiveProperties.DoHotTrackHyperlink(Self, ViewInfo.GetURI(X, Y));
end;

procedure TdxCustomFormattedLabel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
var
  AURI: string;
begin
  if (Button = mbLeft) and ViewInfo.IsMouseOverHyperlink(X, Y) then
  begin
    AURI := ViewInfo.GetURI(X, Y);
    if not ActiveProperties.DoHyperlinkClick(Self, AURI) and (AURI <> '') then
      dxShellExecute(AURI, SW_SHOWMAXIMIZED);
  end;
  inherited;
end;

procedure TdxCustomFormattedLabel.Loaded;
begin
  if CanAllocateHandle(Self) then
    HandleNeeded;
  inherited Loaded;
end;

function TdxCustomFormattedLabel.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and IsInplace;
end;

function TdxCustomFormattedLabel.DefaultParentColor: Boolean;
begin
  Result := True;
end;

function TdxCustomFormattedLabel.GetDefaultSkinnedTextColor
  (AEnabled: Boolean): TColor;
var
  AIntf: IdxCustomSkinnedContainer;
begin
  if Supports(Parent, IdxCustomSkinnedContainer, AIntf) then
    Result := AIntf.GetDefaultTextColor(AEnabled)
  else
    Result := inherited;
end;

function TdxCustomFormattedLabel.GetEditStateColorKind: TcxEditStateColorKind;
begin
  Result := cxEditStateColorKindMap[Enabled];
end;

function TdxCustomFormattedLabel.GetBackgroundStyle: TcxControlBackgroundStyle;
begin
  if IsTransparent then
    Result := bgTransparent
  else
    Result := bgOpaque;
end;

function TdxCustomFormattedLabel.GetCurrentCursor(X: Integer;
  Y: Integer): TCursor;
begin
  if not IsDesigning and ViewInfo.IsMouseOverHyperlink(X, Y) then
    Result := crHandPoint
  else
    Result := inherited;
end;

function TdxCustomFormattedLabel.CanAutoSize: Boolean;
begin
  Result := not IsScaleChanging and inherited CanAutoSize;
end;

function TdxCustomFormattedLabel.CanAutoHeight: Boolean;
begin
  Result := True;
end;

function TdxCustomFormattedLabel.CanAutoWidth: Boolean;
begin
  Result := True;
end;

function TdxCustomFormattedLabel.FadingCanFadeBackground: Boolean;
begin
  Result := False;
end;

procedure TdxCustomFormattedLabel.Initialize;
begin
  inherited;
  ControlStyle := ControlStyle - [csCaptureMouse];
  Width := 121;
  Height := 21;
end;

procedure TdxCustomFormattedLabel.InternalSetEditValue
  (const Value: TcxEditValue; AValidateEditValue: Boolean);
begin
  inherited;
  if not FLockCaption then
    Caption := VarToStr(Value);
  SetInternalDisplayValue(Caption);
end;

function TdxCustomFormattedLabel.IsHeightDependOnWidth: Boolean;
begin
  Result := ActiveProperties.WordWrap;
end;

procedure TdxCustomFormattedLabel.SetInternalDisplayValue(Value: TcxEditValue);
begin
  ViewInfo.FormattedText.Import(Value, ViewInfo.Font);
  ShortRefreshContainer(False);
end;

procedure TdxCustomFormattedLabel.TextChanged;
begin
  inherited;
  FLockCaption := True;
  try
    InternalEditValue := Caption;
  finally
    FLockCaption := False;
  end;
end;

function TdxCustomFormattedLabel.UseAnchorX: Boolean;
begin
  Result := ActiveProperties.Alignment.Horz <> taLeftJustify;
end;

function TdxCustomFormattedLabel.UseAnchorY: Boolean;
begin
  Result := ActiveProperties.Alignment.Vert <> taTopJustify;
end;

function TdxCustomFormattedLabel.GetActiveProperties
  : TdxCustomFormattedLabelProperties;
begin
  Result := TdxCustomFormattedLabelProperties(InternalGetActiveProperties);
end;

function TdxCustomFormattedLabel.GetProperties
  : TdxCustomFormattedLabelProperties;
begin
  Result := TdxCustomFormattedLabelProperties(inherited Properties);
end;

function TdxCustomFormattedLabel.GetStyle: TdxFormattedLabelEditStyle;
begin
  Result := TdxFormattedLabelEditStyle(FStyles.Style);
end;

function TdxCustomFormattedLabel.GetViewInfo: TdxCustomFormattedLabelViewInfo;
begin
  Result := TdxCustomFormattedLabelViewInfo(FViewInfo);
end;

procedure TdxCustomFormattedLabel.SetProperties
  (Value: TdxCustomFormattedLabelProperties);
begin
  Properties.Assign(Value);
end;

procedure TdxCustomFormattedLabel.SetStyle(Value: TdxFormattedLabelEditStyle);
begin
  FStyles.Style := Value;
end;

{ TdxFormattedLabel }

class function TdxFormattedLabel.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TdxFormattedLabelProperties;
end;

function TdxFormattedLabel.GetActiveProperties: TdxFormattedLabelProperties;
begin
  Result := TdxFormattedLabelProperties(InternalGetActiveProperties);
end;

function TdxFormattedLabel.GetProperties: TdxFormattedLabelProperties;
begin
  Result := TdxFormattedLabelProperties(inherited Properties);
end;

procedure TdxFormattedLabel.SetProperties(Value: TdxFormattedLabelProperties);
begin
  Properties.Assign(Value);
end;

initialization

GetRegisteredEditProperties.Register(TdxFormattedLabelProperties,
  scxSEditRepositoryFormattedLabelItem);

finalization

GetRegisteredEditProperties.Unregister(TdxFormattedLabelProperties);

end.
