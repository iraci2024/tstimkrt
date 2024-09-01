{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSkins Library }
{ }
{ Copyright (c) 2006-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING }
{ VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSkinsdxNavBarAccordionViewPainter;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Graphics, Classes, ImgList,
  dxCore, dxCoreGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxGraphics, cxClasses, dxSkinInfo,
  dxShapeBrushes,
  dxNavBar, dxNavBarBase, dxNavBarCollns, dxNavBarStyles, dxNavBarExplorerViews,
  dxNavBarGraphics, dxNavBarConsts, dxNavBarOffice11Views, dxNavBarOfficeViews,
  dxNavBarSkinBasedViews, dxSkinsCore, dxNavBarCustomPainters;

type
  { TdxNavBarAccordionViewSkinPainterHelper }

  TdxNavBarAccordionViewSkinPainterHelper = class
    (TdxNavBarSkinBasedPainterHelper)
  private
    FLookAndFeel: TcxLookAndFeel;
  protected
    function GetSkinInfoClass: TdxSkinInfoClass; override;
    function GetSkinName: TdxSkinName; override;
    function GetSkinPainterData(var AData: TdxSkinInfo): Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues);
    procedure PopulateSkinNames(AList: TStrings); override;
    procedure SetSkinName(AValue: TdxSkinName); override;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
  public
    constructor Create(ASkinName: TdxSkinName); override;
    destructor Destroy; override;

    function NavBarBackground: TdxSkinElement; override;
    function NavBarDragDropItemTarget: TdxSkinElement; override;
    function NavBarDistanceBetweenRootGroups: Integer; virtual;
    function NavBarChildGroup: TdxSkinElement; override;
    function NavBarChildGroupExpandButton(AIsClose: Boolean)
      : TdxSkinElement; override;
    function NavBarChildItemOffset: Integer; virtual;
    function NavBarGroupButtonCaption: TdxSkinElement; override;
    function NavBarGroupSigns(AIsClose: Boolean): TdxSkinElement; override;
    function NavBarItem: TdxSkinElement; override;
    function NavBarSeparator: TdxSkinElement; override;
    function NavPaneDoesGroupCaptionButtonRequireOffset: Boolean; override;
    function NavPanePopupControl: TdxSkinElement; override;
  end;

  { TdxNavBarHighlightEffectInfo }

  TdxNavBarHighlightEffectInfo = class
  strict private
    FBrush: TdxRadialGradientBrush;
    FCenterColor: TdxAlphaColor;
    FCenterPoint: TPoint;
    FEdgeColor: TdxAlphaColor;
  protected
    procedure InternalApply(ACanvas: TcxCanvas; const R: TRect);
    procedure RecreateBrush;
    property Brush: TdxRadialGradientBrush read FBrush;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply(ACanvas: TcxCanvas; const R: TRect); overload;
    procedure Apply(ACanvas: TcxCanvas; const R: TRect;
      const ARadius: Integer); overload;
    procedure Calculate(const P: TPoint);
    procedure SetColor(AValue: TdxAlphaColor);
  end;

  { TdxNavBarAccordionViewInfo }

  TdxNavBarAccordionViewInfo = class(TdxNavBarExplorerBarViewInfo)
  strict private
    FContentHighlightEffectInfo: TdxNavBarHighlightEffectInfo;
    FBorderHighlightEffectInfo: TdxNavBarHighlightEffectInfo;
    FPressedEffectInfo: TdxNavBarHighlightEffectInfo;
  protected
    function GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
    function AllowExpandAnimation: Boolean; override;
    procedure CalculateHighlightEffects(const APoint: TPoint); virtual;
    function CanSelectLinkByRect: Boolean; override;
    procedure ColorSchemeChanged;
    procedure CreateHighlightEffects; virtual;
    function GetItemCaptionOffsets: TRect; override;
    function GetChildItemOffset: Integer; override;
    function GetDefaultSmallImageWidth: Integer; override;
    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionSignSize: TSize; override;
    function GetGroupEdges: TPoint; override;
    function GetGroupSeparatorWidth: Integer; override;
    function GetScrollContentForegroundColor: TColor; override;
    procedure AssignFontStyle(AElement: TdxSkinElement;
      AStyle: TdxNavBarDefaultStyle);
    procedure UpdateEffectColors; virtual;

    property ContentHighlightEffectInfo: TdxNavBarHighlightEffectInfo
      read FContentHighlightEffectInfo;
    property BorderHighlightEffectInfo: TdxNavBarHighlightEffectInfo
      read FBorderHighlightEffectInfo;
    property PressedEffectInfo: TdxNavBarHighlightEffectInfo
      read FPressedEffectInfo;
  public
    constructor Create(APainter: TdxNavBarPainter); override;
    destructor Destroy; override;

    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemHotTrackedStyle; override;
    procedure AssignDefaultItemPressedStyle; override;
    procedure AssignDefaultChildGroupCaptionStyle; override;
    procedure AssignDefaultChildGroupCaptionHotTrackedStyle; override;
    procedure AssignDefaultChildGroupCaptionPressedStyle; override;
    procedure CalculateSizeGripBounds; override; // for internal use
  end;

  TdxNavBarAccordionGroupViewInfo = class
    (TdxNavBarSkinBasedExplorerBarGroupViewInfo)
  public
    function GetCaptionContentHeight: Integer; override;
  end;

  TdxNavBarAccordionChildGroupViewInfo = class(TdxNavBarChildGroupViewInfo)
  protected
    function GetExpandButtonOffset: TSize; override;
    function GetExpandButtonSize: TSize; override;
    function GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
  public
    function SelectionRect: TRect; override;
  end;

  TdxNavBarAccordionLinkViewInfo = class
    (TdxNavBarSkinBasedExplorerBarLinkViewInfo)
  protected
    function GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
  public
    function SelectionRect: TRect; override;
    function Style: TdxNavBarBaseStyle; override;
  end;

  TdxNavBarAccordionViewController = class
    (TdxNavBarOffice11ExplorerBarController)
  private
    function GetViewInfo: TdxNavBarAccordionViewInfo;
  protected
    procedure DoMouseLeave; override;
    procedure DoMouseMove(AShift: TShiftState; const APoint: TPoint); override;
    property ViewInfo: TdxNavBarAccordionViewInfo read GetViewInfo;
  end;

  { TdxNavBarAccordionViewPainter }

  TdxNavBarAccordionViewPainter = class(TdxNavBarSkinBasedExplorerBarPainter)
  private
    function GetLookAndFeel: TcxLookAndFeel;
    function GetSkinName: TdxSkinName;
    function GetSkinNameAssigned: Boolean;
    function GetViewInfo: TdxNavBarAccordionViewInfo;
    function IsSkinNameStored: Boolean;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetSkinName(const AValue: TdxSkinName);
    procedure SetSkinNameAssigned(AValue: Boolean);
  protected
    function AllowAcrylicBackground: Boolean; override;
    function AllowHighlightEffects: Boolean;
    function AllowUsualItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo)
      : Boolean; virtual;
    procedure ColorSchemeChanged(ASender: TObject); override;
    class function GetChildGroupViewInfoClass
      : TdxNavBarChildGroupViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetSkinPainterHelperClass
      : TdxNavBarSkinBasedPainterHelperClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
    function GetMasterLookAndFeel: TcxLookAndFeel; override;
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DrawElementHighlight(const R: TRect); virtual;
    procedure DrawElementHighlightBorders(const R: TRect;
      ABorders: TcxBorders = cxBordersAll);
    procedure DrawElementPressed(const R: TRect);
    procedure DrawGroupTopBorder(AGroupViewInfo: TdxNavBarGroupViewInfo);
    procedure DrawItemHighlightEffects(ALinkViewInfo
      : TdxNavBarLinkViewInfo); virtual;
    function GetAcrylicBorderColor: TdxAlphaColor; override;
    function GetAcrylicForegroundColor: TColor; override;
    function GetBackgroundColorForComposition: TColor; override;
    function GetControllerClass: TdxNavBarControllerClass; override;
    procedure Initialize; override;
    procedure UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel);
      override;
    property ViewInfo: TdxNavBarAccordionViewInfo read GetViewInfo;
  public
    procedure Assign(Source: TPersistent); override;

    procedure DrawBackground; override;
    procedure DrawChildGroupExpandButton(AChildGroupViewInfo
      : TdxNavBarChildGroupViewInfo); override;
    procedure DrawChildGroupSelection(AChildGroupViewInfo
      : TdxNavBarChildGroupViewInfo); override;
    procedure DrawGroupBackground(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawGroupBorder(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionButton(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControl(ACanvas: TCanvas; ARect: TRect;
      AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControlSplitter(AGroupViewInfo
      : TdxNavBarExplorerBarGroupViewInfo); override;
    procedure DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo); override;
    procedure DrawNavBarControl; override;
    procedure DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo);
      overload; override;

    function DrawItemState(ALinkViewInfo: TdxNavBarLinkViewInfo;
      const ARect: TRect; AState: TdxNavBarObjectStates): Boolean;

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel
      write SetLookAndFeel;
  published
    property SkinName: TdxSkinName read GetSkinName write SetSkinName
      stored IsSkinNameStored;
    property SkinNameAssigned: Boolean read GetSkinNameAssigned
      write SetSkinNameAssigned default False;
  end;

implementation

uses
  Math, SysUtils, cxGeometry, dxDPIAwareUtils, dxGdiPlusApi, dxGdiPlusClasses,
  dxTypeHelpers, dxNavBarViewsFact, dxSkinsStrs;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);

procedure DrawElementPart(AElement: TdxSkinElement; ACanvas: TcxCanvas;
  ADrawRect, AClipRect: TRect);
begin
  if AElement = nil then
    Exit;
  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(AClipRect), roIntersect);
    AElement.Draw(ACanvas.Handle, ADrawRect);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

{ TdxNavBarAccordionViewSkinPainterHelper }

constructor TdxNavBarAccordionViewSkinPainterHelper.Create
  (ASkinName: TdxSkinName);
begin
  inherited Create(ASkinName);
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FLookAndFeel.NativeStyle := False;
  FLookAndFeel.OnChanged := LookAndFeelChanged;
end;

destructor TdxNavBarAccordionViewSkinPainterHelper.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarBackground
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
  begin
    Result := ASkinInfo.NavBarAccordionControlBackground;
    if Result = nil then
      Result := inherited NavBarBackground;
  end;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarDragDropItemTarget
  : TdxSkinElement;
begin
  Result := nil;
end;

function TdxNavBarAccordionViewSkinPainterHelper.
  NavBarDistanceBetweenRootGroups: Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and
    (ASkinInfo.NavBarAccordionControlDistanceBetweenRootGroups <> nil) then
    Result := ASkinInfo.NavBarAccordionControlDistanceBetweenRootGroups.Value
  else
    Result := -1;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarChildGroup
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := inherited NavBarChildGroup;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavBarAccordionControlGroup;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarChildGroupExpandButton
  (AIsClose: Boolean): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    if AIsClose then
      Result := ASkinInfo.NavBarAccordionControlGroupOpenButton
    else
      Result := ASkinInfo.NavBarAccordionControlGroupCloseButton;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarChildItemOffset: Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and
    (ASkinInfo.NavBarAccordionControlChildItemOffset <> nil) then
    Result := ASkinInfo.NavBarAccordionControlChildItemOffset.Value
  else
    Result := -1;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarGroupButtonCaption
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
  begin
    Result := ASkinInfo.NavBarAccordionControlRootGroup;
    if Result = nil then
      Result := inherited NavBarGroupButtonCaption;
  end;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarGroupSigns
  (AIsClose: Boolean): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    if AIsClose then
      Result := ASkinInfo.NavBarAccordionControlRootGroupCloseButton
    else
      Result := ASkinInfo.NavBarAccordionControlRootGroupOpenButton;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarItem: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavBarAccordionControlItem;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarSeparator
  : TdxSkinElement;
begin
  Result := nil;
end;

function TdxNavBarAccordionViewSkinPainterHelper.
  NavPaneDoesGroupCaptionButtonRequireOffset: Boolean;
var
  ABoolProperty: TdxSkinBooleanProperty;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) then
    ABoolProperty := ASkinInfo.NavPaneOffsetGroupBorders
  else
    ABoolProperty := nil;

  if ABoolProperty = nil then
    Result := inherited NavPaneDoesGroupCaptionButtonRequireOffset
  else
    Result := ABoolProperty.Value;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavPanePopupControl
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavPaneFormBorder
  else
    Result := nil;
end;

procedure TdxNavBarAccordionViewSkinPainterHelper.PopulateSkinNames
  (AList: TStrings);
begin
  cxLookAndFeelPaintersManager.PopulateSkinNames(AList);
end;

function TdxNavBarAccordionViewSkinPainterHelper.GetSkinInfoClass
  : TdxSkinInfoClass;
begin
  Result := TdxSkinInfo;
end;

function TdxNavBarAccordionViewSkinPainterHelper.GetSkinName: TdxSkinName;
begin
  Result := FLookAndFeel.SkinName;
end;

function TdxNavBarAccordionViewSkinPainterHelper.GetSkinPainterData
  (var AData: TdxSkinInfo): Boolean;
begin
  Result := (FLookAndFeel.SkinPainter <> nil) and
    FLookAndFeel.SkinPainter.GetPainterData(AData);
end;

procedure TdxNavBarAccordionViewSkinPainterHelper.LookAndFeelChanged
  (Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  DoChanged;
end;

procedure TdxNavBarAccordionViewSkinPainterHelper.SetSkinName
  (AValue: TdxSkinName);
begin
  FLookAndFeel.SkinName := AValue;
end;

{ TdxNavBarHighlightEffectInfo }

constructor TdxNavBarHighlightEffectInfo.Create;
begin
  inherited Create;
  FCenterPoint := cxInvisiblePoint;
end;

destructor TdxNavBarHighlightEffectInfo.Destroy;
begin
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TdxNavBarHighlightEffectInfo.Apply(ACanvas: TcxCanvas;
  const R: TRect);
begin
  Apply(ACanvas, R, Max(R.Width, R.Height));
end;

procedure TdxNavBarHighlightEffectInfo.Apply(ACanvas: TcxCanvas; const R: TRect;
  const ARadius: Integer);
var
  ATargetRect: TRect;
begin
  ATargetRect := Rect(FCenterPoint.X - ARadius, FCenterPoint.Y - ARadius,
    FCenterPoint.X + ARadius, FCenterPoint.Y + ARadius);
  Brush.SetTargetRect(ATargetRect);
  InternalApply(ACanvas, R);
end;

procedure TdxNavBarHighlightEffectInfo.Calculate(const P: TPoint);
begin
  FCenterPoint := P;
end;

procedure TdxNavBarHighlightEffectInfo.SetColor(AValue: TdxAlphaColor);
begin
  FCenterColor := AValue;
  FEdgeColor := TdxAlphaColors.FromArgb(0, AValue);
  RecreateBrush;
end;

procedure TdxNavBarHighlightEffectInfo.InternalApply(ACanvas: TcxCanvas;
  const R: TRect);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  dxGPPaintCanvas.SetClipRect(R, gmIntersect);
  GdipCheck(GdipFillRectangle(dxGPPaintCanvas.Handle, Brush.Handle, R.Left,
    R.Top, R.Width, R.Height));
  dxGPPaintCanvas.EndPaint;
end;

procedure TdxNavBarHighlightEffectInfo.RecreateBrush;
begin
  FreeAndNil(FBrush);
  FBrush := TdxRadialGradientBrush.Create;
  FBrush.GradientPoints.Add(0, FCenterColor);
  FBrush.GradientPoints.Add(1, FEdgeColor);
end;

{ TdxNavBarAccordionViewInfo }

constructor TdxNavBarAccordionViewInfo.Create(APainter: TdxNavBarPainter);
begin
  inherited Create(APainter);
  CreateHighlightEffects;
end;

destructor TdxNavBarAccordionViewInfo.Destroy;
begin
  FreeAndNil(FContentHighlightEffectInfo);
  FreeAndNil(FBorderHighlightEffectInfo);
  FreeAndNil(FPressedEffectInfo);
  inherited Destroy;
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultChildGroupCaptionStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultChildGroupCaptionStyle;
  AElement := GetHelper.NavBarChildGroup;
  if AElement <> nil then
    NavBar.DefaultStyles.ChildGroupCaption.Font.Color := AElement.TextColor;
end;

procedure TdxNavBarAccordionViewInfo.
  AssignDefaultChildGroupCaptionHotTrackedStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultChildGroupCaptionHotTrackedStyle;
  AElement := GetHelper.NavBarChildGroup;
  if AElement <> nil then
    NavBar.DefaultStyles.ChildGroupCaptionHotTracked.Font.Color :=
      AElement.GetTextColor(cxbsHot);
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultChildGroupCaptionPressedStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultChildGroupCaptionPressedStyle;
  AElement := GetHelper.NavBarChildGroup;
  if AElement <> nil then
    NavBar.DefaultStyles.ChildGroupCaptionPressed.Font.Color :=
      AElement.GetTextColor(cxbsPressed);
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultGroupHeaderStyle;
begin
  inherited AssignDefaultGroupHeaderStyle;
  AssignFontStyle(GetHelper.NavBarGroupButtonCaption,
    NavBar.DefaultStyles.GroupHeader);
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultItemHotTrackedStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemHotTrackedStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.ItemHotTracked.Font.Color :=
      AElement.GetTextColor(cxbsHot);
  NavBar.DefaultStyles.ItemHotTracked.Font.Style :=
    NavBar.DefaultStyles.ItemHotTracked.Font.Style - [fsUnderline];
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultItemPressedStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemPressedStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.ItemPressed.Font.Color :=
      AElement.GetTextColor(cxbsPressed);
  NavBar.DefaultStyles.ItemPressed.Font.Style :=
    NavBar.DefaultStyles.ItemPressed.Font.Style - [fsUnderline];
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultItemStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.Item.Font.Color := AElement.TextColor;
end;

function TdxNavBarAccordionViewInfo.GetHelper
  : TdxNavBarAccordionViewSkinPainterHelper;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper
    (TdxNavBarAccordionViewPainter(Painter).FSkinBasedPainterHelper);
end;

function TdxNavBarAccordionViewInfo.AllowExpandAnimation: Boolean;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := GetHelper.GetSkinPainterData(ASkinInfo);
end;

procedure TdxNavBarAccordionViewInfo.CalculateHighlightEffects
  (const APoint: TPoint);
begin
  FBorderHighlightEffectInfo.Calculate(APoint);
  FContentHighlightEffectInfo.Calculate(APoint);
  FPressedEffectInfo.Calculate(APoint);
end;

function TdxNavBarAccordionViewInfo.CanSelectLinkByRect: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarAccordionViewInfo.ColorSchemeChanged;
begin
  UpdateEffectColors;
  TdxCustomNavBarAccess(NavBar).ResetAcrylicBackgroundColor;
end;

procedure TdxNavBarAccordionViewInfo.CreateHighlightEffects;
begin
  FBorderHighlightEffectInfo := TdxNavBarHighlightEffectInfo.Create;
  FContentHighlightEffectInfo := TdxNavBarHighlightEffectInfo.Create;
  FPressedEffectInfo := TdxNavBarHighlightEffectInfo.Create;
end;

function TdxNavBarAccordionViewInfo.GetItemCaptionOffsets: TRect;
begin
  if GetHelper.NavBarItem <> nil then
    Result := GetHelper.NavBarItem.ContentOffset.Rect
  else
    Result := inherited GetItemCaptionOffsets;
end;

function TdxNavBarAccordionViewInfo.GetChildItemOffset: Integer;
begin
  if GetHelper.NavBarChildItemOffset > 0 then
    Result := GetHelper.NavBarChildItemOffset
  else
    Result := inherited GetChildItemOffset;
end;

function TdxNavBarAccordionViewInfo.GetDefaultSmallImageWidth: Integer;
begin
  Result := 0;
end;

function TdxNavBarAccordionViewInfo.GetGroupBorderOffsets: TRect;
begin
  if TdxNavBarAccordionViewPainter(Painter).IsSkinAvailable then
    Result := GetSkinElementOffsets(GetHelper.NavBarGroupClient, ScaleFactor)
  else
    Result := inherited GetGroupBorderOffsets;
end;

function TdxNavBarAccordionViewInfo.GetGroupCaptionHeightAddon: Integer;
const
  ExpandCollapseButtonIndent = 3;
begin
  if GetHelper.NavBarGroupButtonCaption <> nil then
    Result := ScaleFactor.Apply
      (cxMarginsHeight(GetHelper.NavBarGroupButtonCaption.ContentOffset.Rect) +
      ExpandCollapseButtonIndent)
  else
    Result := inherited GetGroupCaptionHeightAddon;
end;

function TdxNavBarAccordionViewInfo.GetGroupCaptionSignSize: TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := GetHelper.NavBarGroupSigns(True);
  if AElement <> nil then
  begin
    Result := dxSkinGetElementSize(AElement, ScaleFactor);
    Inc(Result.cx, ScaleFactor.Apply
      (cxMarginsWidth(AElement.ContentOffset.Rect)));
    Inc(Result.cy, ScaleFactor.Apply
      (cxMarginsHeight(AElement.ContentOffset.Rect)));
  end
  else
    Result := inherited GetGroupCaptionSignSize;
end;

function TdxNavBarAccordionViewInfo.GetGroupEdges: TPoint;
begin
  Result := ScaleFactor.Apply(cxSimplePoint);
end;

function TdxNavBarAccordionViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := GetHelper.NavBarDistanceBetweenRootGroups;
  if Result = -1 then
    Result := inherited GetGroupSeparatorWidth
  else
    Result := ScaleFactor.Apply(Result);
end;

function TdxNavBarAccordionViewInfo.GetScrollContentForegroundColor: TColor;
var
  AElement: TdxSkinElement;
begin
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    Result := AElement.TextColor
  else
    Result := inherited GetScrollContentForegroundColor;
end;

procedure TdxNavBarAccordionViewInfo.AssignFontStyle(AElement: TdxSkinElement;
  AStyle: TdxNavBarDefaultStyle);
var
  AFontSize: Integer;
  AProperty: TdxSkinProperty;
begin
  if AElement <> nil then
  begin
    AStyle.Font.Color := AElement.TextColor;

    if AElement.GetPropertyByName(sdxFontSize, AProperty) then
      AFontSize := TdxSkinIntegerProperty(AProperty).Value
    else if AElement.GetPropertyByName(sdxCaptionFontDelta, AProperty) then
      AFontSize := 8 + TdxSkinIntegerProperty(AProperty).Value
    else
      AFontSize := -1;

    if AFontSize > 0 then
      AStyle.Font.Height := dxGetFontHeight(AFontSize, ScaleFactor);

    if AElement.GetPropertyByName(sdxFontBold, AProperty) and
      (AProperty is TdxSkinBooleanProperty) and
      TdxSkinBooleanProperty(AProperty).Value then
      AStyle.Font.Style := [fsBold]
    else
      AStyle.Font.Style := [];
  end;
end;

procedure TdxNavBarAccordionViewInfo.UpdateEffectColors;
var
  AEffectColor: TColor;
begin
  AEffectColor := clWhite;
  ContentHighlightEffectInfo.SetColor(dxColorToAlphaColor(AEffectColor, $60));
  BorderHighlightEffectInfo.SetColor(dxColorToAlphaColor(AEffectColor, $40));
  PressedEffectInfo.SetColor(dxColorToAlphaColor(AEffectColor, $20));
end;

procedure TdxNavBarAccordionViewInfo.CalculateSizeGripBounds;
var
  AOffsetSign: Integer;
begin
  inherited CalculateSizeGripBounds;
  if IsInternal and NavBar.ScrollBar.Visible and
    not TdxCustomNavBarAccess(NavBar).IsScrollBarUseClientArea then
  begin
    if NavBar.UseRightToLeftScrollBar then
      AOffsetSign := IfThen(GetExpandDirection = dirRight, 0, -1)
    else
      AOffsetSign := IfThen(GetExpandDirection = dirRight, 1, 0);
    FSizeGripRect := cxRectOffsetHorz(FSizeGripRect,
      AOffsetSign * NavBar.ScrollBar.Width);
  end;
end;

{ TdxNavBarAccordionGroupViewInfo }

function TdxNavBarAccordionGroupViewInfo.GetCaptionContentHeight: Integer;
begin
  Result := Max(inherited GetCaptionContentHeight,
    (ViewInfo as TdxNavBarAccordionViewInfo).GetGroupCaptionSignSize.cy);
end;

{ TdxNavBarAccordionChildGroupViewInfo }

function TdxNavBarAccordionChildGroupViewInfo.SelectionRect: TRect;
begin
  Result := CaptionRect;
end;

function TdxNavBarAccordionChildGroupViewInfo.GetExpandButtonOffset: TSize;
var
  AOffsetX: TdxSkinIntegerProperty;
begin
  Result := cxNullSize;
  if GetHelper.NavBarChildGroupExpandButton(True) <> nil then
  begin
    AOffsetX := GetHelper.NavBarChildGroupExpandButton(True)
      .GetPropertyByName(sdxOffsetX) as TdxSkinIntegerProperty;
    if AOffsetX <> nil then
      Result.cx := AOffsetX.Value;
  end;
end;

function TdxNavBarAccordionChildGroupViewInfo.GetExpandButtonSize: TSize;
begin
  if GetHelper.NavBarChildGroupExpandButton(True) <> nil then
    Result := dxSkinGetElementSize(GetHelper.NavBarChildGroupExpandButton(True),
      ScaleFactor)
  else
    Result := inherited GetExpandButtonSize;
end;

function TdxNavBarAccordionChildGroupViewInfo.GetHelper
  : TdxNavBarAccordionViewSkinPainterHelper;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper
    (TdxNavBarAccordionViewPainter(Painter).FSkinBasedPainterHelper);
end;

{ TdxNavBarAccordionLinkViewInfo }

function TdxNavBarAccordionLinkViewInfo.SelectionRect: TRect;
begin
  Result := Rect;
end;

function TdxNavBarAccordionLinkViewInfo.Style: TdxNavBarBaseStyle;
begin
  if sDisabled in State then
    Result := NavBar.DefaultStyles.ItemDisabled
  else if State * [sPressed, sSelected] <> [] then
    Result := NavBar.DefaultStyles.ItemPressed
  else
    Result := inherited Style;
end;

function TdxNavBarAccordionLinkViewInfo.GetHelper
  : TdxNavBarAccordionViewSkinPainterHelper;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper
    (TdxNavBarAccordionViewPainter(Painter).FSkinBasedPainterHelper);
end;

{ TdxNavBarAccordionViewController }

procedure TdxNavBarAccordionViewController.DoMouseLeave;
begin
  ViewInfo.BorderHighlightEffectInfo.Calculate(cxInvisiblePoint);
end;

procedure TdxNavBarAccordionViewController.DoMouseMove(AShift: TShiftState;
  const APoint: TPoint);
begin
  inherited DoMouseMove(AShift, APoint);
  if (Painter as TdxNavBarAccordionViewPainter).AllowHighlightEffects then
  begin
    ViewInfo.CalculateHighlightEffects(APoint);
    NavBar.Invalidate;
  end;
end;

function TdxNavBarAccordionViewController.GetViewInfo
  : TdxNavBarAccordionViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarAccordionViewInfo;
end;

{ TdxNavBarAccordionViewPainter }

procedure TdxNavBarAccordionViewPainter.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarAccordionViewPainter then
    LookAndFeel := TdxNavBarAccordionViewPainter(Source).LookAndFeel
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawBackground;

  function IsAnimationPrepare: Boolean;
  begin
    Result := cxCanvas <> NavBar.Canvas;
  end;

var
  R: TRect;
begin
  if FSkinBasedPainterHelper.NavBarBackground <> nil then
  begin
    R := ViewInfo.ClientRect;
    if IsAnimationPrepare then
    begin
      R.Top := ViewInfo.Groups[0].Rect.Top;
      R.Bottom := ViewInfo.Groups[ViewInfo.GroupCount - 1].Rect.Bottom;
    end;
    if IsAcrylicEnabled then
    begin
      dxGPPaintCanvas.BeginPaint(cxCanvas.Handle, R);
      dxGPPaintCanvas.FillRectangle(R, GetAcrylicBackgroundColor);
      dxGPPaintCanvas.EndPaint;
    end
    else
      cxCanvas.FillRect(R, FSkinBasedPainterHelper.NavBarBackground.Color)
  end
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawChildGroupExpandButton
  (AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
var
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(AChildGroupViewInfo.State);
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarChildGroupExpandButton
    (not(sExpanded in AChildGroupViewInfo.State)), Canvas,
    AChildGroupViewInfo.ExpandButtonRect, ScaleFactor, 0, AState) then
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawChildGroupSelection
  (AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
var
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(AChildGroupViewInfo.State);
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarChildGroup, Canvas,
    AChildGroupViewInfo.SelectionRect, ScaleFactor, 0, AState) then
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupBackground(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
begin
  if FSkinBasedPainterHelper.NavBarBackground <> nil then
  begin
    cxCanvas.SaveClipRegion;
    try
      cxCanvas.IntersectClipRect(AGroupViewInfo.Rect);
      cxCanvas.FillRect(AGroupViewInfo.Rect,
        FSkinBasedPainterHelper.NavBarBackground.Color);
    finally
      cxCanvas.RestoreClipRegion;
    end;
  end
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupBorder(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
begin
  // do nothing
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupCaptionButton
  (AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if not AllowHighlightEffects then
    inherited DrawGroupCaptionButton(AGroupViewInfo);
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupTopBorder(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
var
  AElement: TdxSkinElement;
  R: TRect;
begin
  AElement := FSkinBasedPainterHelper.NavBarGroupButtonCaption;
  if AElement <> nil then
  begin
    cxCanvas.SaveClipRegion;
    try
      R := AGroupViewInfo.ItemsRect;
      cxCanvas.SetClipRegion(TcxRegion.Create(cxRectSetHeight(R, 1)), roSet);
      R.Bottom := R.Top + 1;
      Dec(R.Top, AElement.Size.cy);
      AElement.Draw(cxCanvas.Handle, R);
    finally
      cxCanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxNavBarAccordionViewPainter.DrawItemHighlightEffects
  (ALinkViewInfo: TdxNavBarLinkViewInfo);
begin
  cxCanvas.SaveClipRegion;
  cxCanvas.ExcludeFrameRect(ALinkViewInfo.SelectionRect);
  if sHotTracked in ALinkViewInfo.State then
    DrawElementHighlight(ALinkViewInfo.SelectionRect);
  cxCanvas.RestoreClipRegion;
end;

function TdxNavBarAccordionViewPainter.GetAcrylicBorderColor: TdxAlphaColor;
var
  AColor: TColor;
begin
  if (FSkinBasedPainterHelper <> nil) and
    (FSkinBasedPainterHelper.NavBarGroupButtonCaption <> nil) then
  begin
    AColor := FSkinBasedPainterHelper.NavBarGroupButtonCaption.TextColor;
    Result := TdxAlphaColors.FromArgb(64,
      TdxAlphaColors.Blend(dxColorToAlphaColor(AColor, 165),
      dxColorToAlphaColor(GetBackgroundColorForComposition, 90)));
  end
  else
    Result := inherited GetAcrylicBorderColor;
end;

function TdxNavBarAccordionViewPainter.GetAcrylicForegroundColor: TColor;
begin
  if (FSkinBasedPainterHelper <> nil) and
    (FSkinBasedPainterHelper.NavBarGroupButtonCaption <> nil) then
    Result := FSkinBasedPainterHelper.NavBarGroupButtonCaption.TextColor
  else
    Result := inherited GetAcrylicForegroundColor;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupControl(ACanvas: TCanvas;
  ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  AcxCanvas: TcxCanvas;
begin
  if IsAcrylicEnabled and TdxCustomNavBarAccess(NavBar).IsGroupExpanding then
    Exit;
  if FSkinBasedPainterHelper.NavBarBackground <> nil then
  begin
    AcxCanvas := TcxCanvas.Create(ACanvas);
    AcxCanvas.SaveClipRegion;
    try
      AcxCanvas.SetClipRegion(TcxRegion.Create(ARect), roIntersect);
      if IsAcrylicEnabled then
      begin
        dxGPPaintCanvas.BeginPaint(AcxCanvas.Handle, ARect);
        dxGPPaintCanvas.FillRectangle(ARect, GetAcrylicBackgroundColor);
        dxGPPaintCanvas.EndPaint;
      end
      else
        AcxCanvas.FillRect(ARect,
          FSkinBasedPainterHelper.NavBarBackground.Color);
    finally
      AcxCanvas.RestoreClipRegion;
      AcxCanvas.Free;
    end;
  end
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupControlSplitter
  (AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo);
begin
  // do nothing
end;

procedure TdxNavBarAccordionViewPainter.DrawItemSelection
  (ALinkViewInfo: TdxNavBarLinkViewInfo);
begin
  if AllowUsualItemSelection(ALinkViewInfo) and not DrawItemState(ALinkViewInfo,
    ALinkViewInfo.SelectionRect, ALinkViewInfo.State) then
    inherited DrawItemSelection(ALinkViewInfo);
  if AllowHighlightEffects then
    DrawItemHighlightEffects(ALinkViewInfo);
end;

procedure TdxNavBarAccordionViewPainter.DrawNavBarControl;
begin
  inherited DrawNavBarControl;
  InternalDrawSizeGrip(cxCanvas);
end;

procedure TdxNavBarAccordionViewPainter.DrawSeparator(ALinkViewInfo
  : TdxNavBarLinkViewInfo);

  function DrawSkinnedSeparator(const AClipRect: TRect): Boolean;
  var
    ADrawRect: TRect;
  begin
    Result := (FSkinBasedPainterHelper.NavPaneHeader <> nil) and
      (FSkinBasedPainterHelper.NavPaneCaptionHeight <> nil);
    if Result then
    begin
      ADrawRect := cxRectSetBottom(AClipRect, AClipRect.Bottom,
        FSkinBasedPainterHelper.NavPaneCaptionHeight.Value);
      DrawElementPart(FSkinBasedPainterHelper.NavPaneHeader, cxCanvas,
        ADrawRect, AClipRect);
    end
  end;

begin
  if not DrawSkinnedSeparator(ALinkViewInfo.SeparatorRect) then
    inherited DrawSeparator(ALinkViewInfo);
end;

function TdxNavBarAccordionViewPainter.DrawItemState(ALinkViewInfo
  : TdxNavBarLinkViewInfo; const ARect: TRect;
  AState: TdxNavBarObjectStates): Boolean;
var
  ASkinElementState: TdxSkinElementState;
begin
  ASkinElementState := NavBarObjectStateToSkinState(AState);
  Result := DrawSkinElement(FSkinBasedPainterHelper.NavBarItem, Canvas, ARect,
    ScaleFactor, 0, ASkinElementState);
end;

function TdxNavBarAccordionViewPainter.AllowAcrylicBackground: Boolean;
begin
  Result := FSkinBasedPainterHelper.NavBarBackground <> nil;
end;

function TdxNavBarAccordionViewPainter.AllowHighlightEffects: Boolean;
begin
  Result := IsAcrylicEnabled and NavBar.OptionsBehavior.Common.RevealHighlight;
end;

function TdxNavBarAccordionViewPainter.AllowUsualItemSelection
  (ALinkViewInfo: TdxNavBarLinkViewInfo): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarAccordionViewPainter.ColorSchemeChanged(ASender: TObject);
begin
  inherited ColorSchemeChanged(ASender);
  (ViewInfo as TdxNavBarAccordionViewInfo).ColorSchemeChanged;
end;

class function TdxNavBarAccordionViewPainter.GetChildGroupViewInfoClass
  : TdxNavBarChildGroupViewInfoClass;
begin
  Result := TdxNavBarAccordionChildGroupViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetGroupViewInfoClass
  : TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarAccordionGroupViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetLinkViewInfoClass
  : TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarAccordionLinkViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetViewInfoClass
  : TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarAccordionViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetSkinPainterHelperClass
  : TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper;
end;

class function TdxNavBarAccordionViewPainter.SignPainterClass
  : TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarExplorerBarSignPainter;
end;

function TdxNavBarAccordionViewPainter.GetMasterLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxNavBarAccordionViewPainter.DoDrawSizeGrip(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavPaneSizeGrip,
    ACanvas.Canvas, ARect, ScaleFactor) then
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawElementHighlight(const R: TRect);
begin
  if not R.IsEmpty then
    ViewInfo.ContentHighlightEffectInfo.Apply(cxCanvas, R,
      Max(R.Width, R.Height) div 3);
end;

procedure TdxNavBarAccordionViewPainter.DrawElementHighlightBorders
  (const R: TRect; ABorders: TcxBorders = cxBordersAll);
var
  R1: TRect;
  AWidth: Integer;
  ARadius: Integer;
begin
  if not R.IsEmpty then
  begin
    AWidth := ScaleFactor.Apply(1);
    ARadius := ScaleFactor.Apply(65);
    if bBottom in ABorders then
    begin
      R1 := R;
      R1.Top := R1.Bottom - AWidth;
      ViewInfo.BorderHighlightEffectInfo.Apply(cxCanvas, R1, ARadius);
    end;
    if bTop in ABorders then
    begin
      R1 := R;
      R1.Bottom := R1.Top + AWidth;
      ViewInfo.BorderHighlightEffectInfo.Apply(cxCanvas, R1, ARadius);
    end;
    if bRight in ABorders then
    begin
      R1 := cxRectInflate(R, 0, -AWidth);
      R1.Left := R1.Right - AWidth;
      ViewInfo.BorderHighlightEffectInfo.Apply(cxCanvas, R1, ARadius);
    end;
    if bLeft in ABorders then
    begin
      R1 := cxRectInflate(R, 0, -AWidth);
      R1.Right := R1.Left + AWidth;
      ViewInfo.BorderHighlightEffectInfo.Apply(cxCanvas, R1, ARadius);
    end;
  end;
end;

procedure TdxNavBarAccordionViewPainter.DrawElementPressed(const R: TRect);
begin
  if not R.IsEmpty then
    ViewInfo.PressedEffectInfo.Apply(cxCanvas, R);
end;

function TdxNavBarAccordionViewPainter.GetBackgroundColorForComposition: TColor;
begin
  if (FSkinBasedPainterHelper <> nil) and
    (FSkinBasedPainterHelper.NavBarBackground <> nil) then
    Result := FSkinBasedPainterHelper.NavBarBackground.Color
  else
    Result := inherited GetBackgroundColorForComposition;
end;

function TdxNavBarAccordionViewPainter.GetControllerClass
  : TdxNavBarControllerClass;
begin
  Result := TdxNavBarAccordionViewController;
end;

procedure TdxNavBarAccordionViewPainter.Initialize;
begin
  TdxCustomNavBarAccess(NavBar).ResetAcrylicBackgroundColor;
  ViewInfo.UpdateEffectColors;
end;

procedure TdxNavBarAccordionViewPainter.UpdateScrollBarLookAndFeel
  (ALookAndFeel: TcxLookAndFeel);
begin
  ALookAndFeel.AssignedValues := [];
  ALookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

function TdxNavBarAccordionViewPainter.IsSkinNameStored: Boolean;
begin
  Result := SkinNameAssigned;
end;

procedure TdxNavBarAccordionViewPainter.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  LookAndFeel.Assign(AValue);
end;

procedure TdxNavBarAccordionViewPainter.SetSkinName(const AValue: TdxSkinName);
begin
  ColorSchemeName := AValue;
end;

function TdxNavBarAccordionViewPainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper(FSkinBasedPainterHelper)
    .FLookAndFeel;
end;

function TdxNavBarAccordionViewPainter.GetSkinName: TdxSkinName;
begin
  Result := ColorSchemeName;
end;

function TdxNavBarAccordionViewPainter.GetSkinNameAssigned: Boolean;
begin
  Result := lfvSkinName in LookAndFeel.AssignedValues;
end;

function TdxNavBarAccordionViewPainter.GetViewInfo: TdxNavBarAccordionViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarAccordionViewInfo;
end;

procedure TdxNavBarAccordionViewPainter.SetSkinNameAssigned(AValue: Boolean);
begin
  if AValue then
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues + [lfvSkinName]
  else
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues - [lfvSkinName];
end;

initialization

dxNavBarViewsFactory.RegisterView(dxNavBarAccordionView, 'AccordionView',
  TdxNavBarAccordionViewPainter);

finalization

UnRegisterView(dxNavBarAccordionView);

end.
