{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressNavBar }
{ }
{ Copyright (c) 2002-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING }
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

unit dxNavBarOverflowPanel;

{$I cxVer.inc}

interface

uses
  Types, Windows, Graphics, Classes, ImgList, Controls, Menus,
  cxGeometry, dxCoreClasses, dxNavBar, dxNavBarCollns;

const
  nbOverflowPanelSign = 0;
  nbOverflowPanelGroup = 1;

type
  TdxNavBarCustomOverflowPanelViewInfo = class;

  TdxNavBarOverflowPanelPopupMenuDrawItemEvent = procedure(ACanvas: TCanvas;
    ARect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
    AText: string; AState: TdxNavBarObjectStates) of object; // for internal use
  TdxNavBarOverflowPanelSignButtonClickEvent = procedure(const AClientMousePos
    : TPoint) of object; // for internal use

  { TdxNavBarOverflowPanelViewInfoItem }

  TdxNavBarOverflowPanelViewInfoItemClass = class of
    TdxNavBarOverflowPanelViewInfoItem;

  TdxNavBarOverflowPanelViewInfoItem = class
  strict protected
    FGroup: TdxNavBarGroup;

    function GetCaption: string; virtual;
    function GetImageIndex: Integer; virtual;
    function GetPartElement: TObject; virtual;
    function GetPartIndex: Integer; virtual;
    procedure SetGroup(const AValue: TdxNavBarGroup); virtual;

    function GetNavBar: TdxCustomNavBar;
  protected
    property PartElement: TObject read GetPartElement;
    property PartIndex: Integer read GetPartIndex;
  public
    Rect: TRect;
    SelectionRect: TRect;

    property Caption: string read GetCaption;
    property Group: TdxNavBarGroup read FGroup write SetGroup;
    property ImageIndex: Integer read GetImageIndex; // for internal use
  end;

  { TdxNavBarCustomOverflowPanelPopupMenu }

  TdxNavBarCustomOverflowPanelPopupMenu = class(TPopupMenu) // for internal use
  strict private
    FImageList: TImageList;
    FPanel: TdxNavBarCustomOverflowPanelViewInfo;

    function GetNavBar: TdxCustomNavBar;
    function GetScaleFactor: TdxScaleFactor;

    function AllowCustomizing: Boolean;
    procedure AddVisibleForCustomizationGroupItems(ADefaultImageIndex: Integer);
    procedure AssignImage(AImageList: TImageList; ASourceBitmap: TBitmap);
    procedure DoMeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure DoDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      ASelected: Boolean);
  strict protected
    procedure DoHiddenItemClick(Sender: TObject); virtual;
    procedure PopulateItems(ADefaultImageIndex: Integer); virtual;

    function AddItem(AParentItem: TMenuItem; ACaption: string;
      AEnabled: Boolean = True; AOnClick: TNotifyEvent = nil;
      AImageIndex: Integer = -1; AChecked: Boolean = False): TMenuItem;
    function GetImageIndex(AGroup: TdxNavBarGroup; ADefaultImageIndex: Integer)
      : Integer; overload;
    function GetImageIndex(AItem: TdxNavBarOverflowPanelViewInfoItem;
      ADefaultImageIndex: Integer): Integer; overload;
    function GetSmallImagesCount: Integer;
    procedure CreateHiddenGroupList(AParentMenuItem: TMenuItem;
      ADefaultImageIndex: Integer);
    procedure CreateVisibleForCustomizationGroupList(AParentMenuItem: TMenuItem;
      ADefaultImageIndex: Integer);
    procedure DoAddRemoveButtonsClick(Sender: TObject);

    property NavBar: TdxCustomNavBar read GetNavBar;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  protected
    OnDrawItem: TdxNavBarOverflowPanelPopupMenuDrawItemEvent;

    procedure Populate;
    procedure RecreateImageList;
    procedure Show(const APoint: TPoint);

    property ImageList: TImageList read FImageList;
  public
    constructor Create(APanel: TdxNavBarCustomOverflowPanelViewInfo);
      reintroduce;
    destructor Destroy; override;
  end;

  { TdxNavBarCustomOverflowPanelViewInfo }

  TdxNavBarOverflowPanelViewInfoClass = class of
    TdxNavBarCustomOverflowPanelViewInfo;

  TdxNavBarCustomOverflowPanelViewInfo = class(TdxNavBarPartViewInfo)
  // for internal use
  private
    FItems: TdxFastObjectList;
    FVisibleItemCount: Integer;

    function GetGroup(AIndex: Integer): TdxNavBarGroup;
    function GetItemCount: Integer;
    function GetItems(AIndex: Integer): TdxNavBarOverflowPanelViewInfoItem;
    function GetSmallImagesCount: Integer;
  strict protected
    FRect: TRect;
    FSignRect: TRect;
    function InternalAdd(AItemClass: TdxNavBarOverflowPanelViewInfoItemClass)
      : TdxNavBarOverflowPanelViewInfoItem;
  protected
    OnSignButtonClick: TdxNavBarOverflowPanelSignButtonClickEvent;

    function GetHeight: Integer; virtual;
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    function GetImageList: TCustomImageList;
    function GetImageIndex(AGroup: TdxNavBarGroup): Integer;
    function GetItemIndexAtPos(const pt: TPoint): Integer;
    function GetItemSelectionWidth: Integer; virtual;
    function GetGroupAtPos(const pt: TPoint): TdxNavBarGroup;
    function GetVisibleCount(const ABounds: TRect): Integer;
    function UseSmallImages: Boolean; virtual;

    // Calculation
    procedure CalculateBounds(X, Y: Integer);
    procedure CalculateClientRect(out ARect: TRect); virtual;
    procedure CalculateSignRect(const ARect: TRect); virtual;
    procedure OffsetElements(AHeightDifference: Integer);

    function AddItem: TdxNavBarOverflowPanelViewInfoItem;
    procedure Clear;
    procedure ClearItems;
    procedure ClearRects; virtual;

    procedure DoItemClick;
    procedure DoSignClick;

    // Conditions
    function IsVisible: Boolean;

    // Indents
    function GetImageWidthAddon: Integer; virtual;
    function GetSignWidth: Integer; virtual;
    function GetHeightAddon: Integer; virtual;
    function GetPopupMenuImageIndent: Integer; virtual;
    function GetPopupMenuTextIndent: Integer; virtual;
    function GetSeparator: Integer; virtual;

    function AllowCustomizing: Boolean; virtual;
    function GetClientOffset: TRect; virtual;
    procedure CalculateRect(X, Y: Integer); virtual;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo); override;
    destructor Destroy; override;

    function GetPartAtPos(const APoint: TPoint): TdxNavBarPart; virtual;
    function TryDoClick(const APart: TdxNavBarPart): Boolean; virtual;
    procedure DoRightToLeftConversion;

    property Group[AIndex: Integer]: TdxNavBarGroup read GetGroup;
    property ImageList: TCustomImageList read GetImageList;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TdxNavBarOverflowPanelViewInfoItem
      read GetItems;
    property PopupMenuImageIndent: Integer read GetPopupMenuImageIndent;
    property PopupMenuTextIndent: Integer read GetPopupMenuTextIndent;
    property Rect: TRect read FRect;
    property SignRect: TRect read FSignRect;
    property VisibleItemCount: Integer read FVisibleItemCount;
  end;

  { TdxNavBarOverflowPanelPainter }

  TdxNavBarOverflowPanelPainter = class // for internal use
  protected
    procedure DrawOverflowPanelItemBackground(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo; AItemIndex: Integer;
      const APart: TdxNavBarPart; const ARect: TRect); overload;
    procedure DrawOverflowPanelItemBackground(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo; AItemIndex: Integer;
      AState: TdxNavBarPartState; const ARect: TRect); overload; virtual;
  public
    procedure DrawBackground(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo); virtual;
    procedure DrawItem(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo;
      AItem: TdxNavBarOverflowPanelViewInfoItem); virtual;
    procedure DrawSign(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo); virtual;
    procedure DrawPopupMenuItem(APanel: TdxNavBarCustomOverflowPanelViewInfo;
      ACanvas: TCanvas; ARect: TRect; AImageList: TCustomImageList;
      AImageIndex: Integer; AText: string;
      AState: TdxNavBarObjectStates); virtual;

    procedure Draw(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo);
    procedure DrawItems(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo);
  end;

implementation

uses
  Contnrs, SysUtils, Math, RTLConsts,
  dxCore, dxTypeHelpers, cxGraphics, cxLookAndFeelPainters, dxOffice11,
  cxControls,
  dxNavBarGraphics, dxNavBarStyles, dxNavBarCustomPainters, dxNavBarConsts;

type
  TdxNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarControllerAccess = class(TdxNavBarController);
  TdxNavBarElementPainterAccess = class(TdxNavBarElementPainter);
  TdxNavBarViewInfoAccess = class(TdxNavBarViewInfo);

  { TdxNavBarOverflowPanelViewInfoItem }

function TdxNavBarOverflowPanelViewInfoItem.GetCaption: string;
begin
  Result := Group.Caption;
end;

function TdxNavBarOverflowPanelViewInfoItem.GetImageIndex: Integer;
begin
  if GetNavBar.NavigationPaneOverflowPanelUseSmallImages then
    Result := Group.SmallImageIndex
  else
    Result := Group.LargeImageIndex;
end;

function TdxNavBarOverflowPanelViewInfoItem.GetPartElement: TObject;
begin
  Result := Group;
end;

function TdxNavBarOverflowPanelViewInfoItem.GetPartIndex: Integer;
begin
  Result := nbOverflowPanelGroup;
end;

procedure TdxNavBarOverflowPanelViewInfoItem.SetGroup
  (const AValue: TdxNavBarGroup);
begin
  FGroup := AValue;
end;

function TdxNavBarOverflowPanelViewInfoItem.GetNavBar: TdxCustomNavBar;
begin
  Result := Group.Collection.ParentComponent as TdxCustomNavBar;
end;

{ TdxNavBarCustomOverflowPanelViewInfo }

constructor TdxNavBarCustomOverflowPanelViewInfo.Create
  (AViewInfo: TdxNavBarViewInfo);
begin
  inherited;
  FItems := TdxFastObjectList.Create;
end;

destructor TdxNavBarCustomOverflowPanelViewInfo.Destroy;
begin
  ClearItems;
  FreeAndNil(FItems);
  inherited;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetPartAtPos(const APoint: TPoint)
  : TdxNavBarPart;
begin
  Result.MajorPartIndex := nbNone;
  Result.MinorPartIndex := GetItemIndexAtPos(APoint);
  if (Result.MinorPartIndex <> nbNone) then
    Result.MajorPartIndex := Items[Result.MinorPartIndex].PartIndex
  else if PtInRect(SignRect, APoint) then
    Result.MajorPartIndex := nbOverflowPanelSign
end;

function TdxNavBarCustomOverflowPanelViewInfo.TryDoClick
  (const APart: TdxNavBarPart): Boolean;
begin
  Result := True;
  case APart.MajorPartIndex of
    nbOverflowPanelGroup:
      DoItemClick;
    nbOverflowPanelSign:
      DoSignClick;
  else
    Result := False;
  end;
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.CalculateBounds(X, Y: Integer);

  procedure CalculateItemRects(const AItemsRect: TRect);
  var
    I, AStartPos, ASelectionWidth: Integer;
    ASelectionRect, AItemRect: TRect;
  begin
    ASelectionWidth := GetItemSelectionWidth;
    FVisibleItemCount := GetVisibleCount(AItemsRect);

    AStartPos := AItemsRect.Right - FVisibleItemCount * ASelectionWidth;

    ASelectionRect := cxRect(0, 0, ASelectionWidth, cxRectHeight(AItemsRect));
    OffsetRect(ASelectionRect, AStartPos, AItemsRect.Top);

    AItemRect := cxRect(0, 0, ASelectionWidth, GetImageHeight);
    OffsetRect(AItemRect, AStartPos, AItemsRect.Top +
      (cxRectHeight(AItemsRect) - GetImageHeight) div 2);
    InflateRect(AItemRect, -GetImageWidthAddon, 0);

    for I := 0 to FVisibleItemCount - 1 do
    begin
      Items[I].SelectionRect := ASelectionRect;
      OffsetRect(Items[I].SelectionRect, ASelectionWidth * I, 0);
      Items[I].Rect := AItemRect;
      OffsetRect(Items[I].Rect, ASelectionWidth * I, 0);
    end;
  end;

var
  AClientRect, AItemsRect: TRect;
begin
  if not IsVisible then
    Exit;

  CalculateRect(X, Y);
  CalculateClientRect(AClientRect);
  CalculateSignRect(AClientRect);

  if TdxNavBarControllerAccess(TdxNavBarAccess(NavBar).Controller).Collapsed
  then
    AItemsRect := cxNullRect
  else
  begin
    AItemsRect := AClientRect;
    AItemsRect.Right := FSignRect.Left - GetSeparator;
  end;
  CalculateItemRects(AItemsRect);
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.CalculateClientRect
  (out ARect: TRect);
begin
  ARect := cxRectContent(FRect, GetClientOffset);
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.CalculateSignRect
  (const ARect: TRect);
begin
  FSignRect := ARect;
  FSignRect.Left := FSignRect.Right - GetSignWidth;
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.ClearRects;
begin
  FRect := cxNullRect;
  FSignRect := cxNullRect;
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.OffsetElements(AHeightDifference
  : Integer);
var
  I: Integer;
begin
  if not IsVisible then
    Exit;
  OffsetRect(FRect, 0, AHeightDifference);
  OffsetRect(FSignRect, 0, AHeightDifference);
  for I := 0 to FVisibleItemCount - 1 do
  begin
    OffsetRect(Items[I].Rect, 0, AHeightDifference);
    OffsetRect(Items[I].SelectionRect, 0, AHeightDifference);
  end;
end;

function TdxNavBarCustomOverflowPanelViewInfo.AddItem
  : TdxNavBarOverflowPanelViewInfoItem;
begin
  Result := InternalAdd(TdxNavBarOverflowPanelViewInfoItem);
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.Clear;
begin
  ClearItems;
  ClearRects;
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.ClearItems;
begin
  FVisibleItemCount := 0;
  FItems.Clear;
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.DoItemClick;
begin
  FNavBar.ActiveGroup := Group[NavBar.Controller.HotPart.MinorPartIndex];
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.DoSignClick;
var
  AControllerAccess: TdxNavBarControllerAccess;
  APoint: TPoint;
begin
  APoint.Y := cxRectCenter(SignRect).Y;
  if NavBar.UseRightToLeftAlignment then
    APoint.X := SignRect.Left
  else
    APoint.X := SignRect.Right;
  AControllerAccess := TdxNavBarControllerAccess(NavBar.Controller);
  AControllerAccess.DroppedDownPart := dxNavBarPart(nbOverflowPanelSign);
  if Assigned(OnSignButtonClick) then
    OnSignButtonClick(NavBar.ClientToScreen(APoint));
  AControllerAccess.DroppedDownPart := dxNavBarPart(nbNone);
  AControllerAccess.HotPart := GetPartAtPos(GetMouseCursorPos);
end;

function TdxNavBarCustomOverflowPanelViewInfo.IsVisible: Boolean;
begin
  Result := not NavBar.IsNavigationClient and
    NavBar.OptionsView.NavigationPane.ShowOverflowPanel;
end;

function TdxNavBarCustomOverflowPanelViewInfo.InternalAdd
  (AItemClass: TdxNavBarOverflowPanelViewInfoItemClass)
  : TdxNavBarOverflowPanelViewInfoItem;
begin
  Result := AItemClass.Create;
  FItems.Insert(0, Result);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetHeight: Integer;
begin
  if not IsVisible then
    Result := 0
  else
  begin
    Result := TdxNavBarViewInfoAccess(ViewInfo).GetOverflowPanelHeight +
      GetHeightAddon;
    dxAdjustToTouchableSize(Result, ScaleFactor);
  end;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetImageHeight: Integer;
begin
  if UseSmallImages then
    Result := TdxNavBarViewInfoAccess(ViewInfo).GetSmallImageHeight
  else
    Result := TdxNavBarViewInfoAccess(ViewInfo).GetLargeImageHeight;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetImageWidth: Integer;
begin
  if UseSmallImages then
    Result := TdxNavBarViewInfoAccess(ViewInfo).GetSmallImageWidth
  else
    Result := TdxNavBarViewInfoAccess(ViewInfo).GetLargeImageWidth;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetImageList: TCustomImageList;
begin
  if UseSmallImages then
    Result := NavBar.SmallImages
  else
    Result := NavBar.LargeImages;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetImageIndex
  (AGroup: TdxNavBarGroup): Integer;
begin
  if UseSmallImages then
    Result := AGroup.SmallImageIndex
  else
    Result := AGroup.LargeImageIndex;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetItemIndexAtPos
  (const pt: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FVisibleItemCount - 1 do
    if PtInRect(Items[I].SelectionRect, pt) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetItemSelectionWidth: Integer;
begin
  Result := GetImageWidth + 2 * GetImageWidthAddon;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetGroupAtPos(const pt: TPoint)
  : TdxNavBarGroup;
var
  AIndex: Integer;
begin
  AIndex := GetItemIndexAtPos(pt);
  if AIndex >= 0 then
  begin
    Result := Items[AIndex].Group;
    if (Result <> nil) and (csDestroying in Result.ComponentState) then
      Result := nil;
  end
  else
    Result := nil;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetVisibleCount
  (const ABounds: TRect): Integer;
begin
  Result := Min(ABounds.Width div GetItemSelectionWidth, ItemCount);
end;

function TdxNavBarCustomOverflowPanelViewInfo.UseSmallImages: Boolean;
begin
  Result := NavBar.NavigationPaneOverflowPanelUseSmallImages;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetHeightAddon: Integer;
begin
  Result := ScaleFactor.Apply(8);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetImageWidthAddon: Integer;
begin
  Result := ScaleFactor.Apply(3);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetPopupMenuImageIndent: Integer;
begin
  Result := ScaleFactor.Apply(3);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetPopupMenuTextIndent: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetSignWidth: Integer;
begin
  Result := ScaleFactor.Apply(18);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetSeparator: Integer;
begin
  Result := 0;
end;

function TdxNavBarCustomOverflowPanelViewInfo.AllowCustomizing: Boolean;
begin
  Result := False;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetClientOffset: TRect;
begin
  Result := cxNullRect;
  Result := cxRectOffset(Result, 0, ViewInfo.BorderWidth);
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.CalculateRect(X, Y: Integer);
begin
  FRect := Bounds(X, Y, TdxNavBarViewInfoAccess(ViewInfo).ClientWidth - 2 * X,
    GetHeight);
end;

procedure TdxNavBarCustomOverflowPanelViewInfo.DoRightToLeftConversion;
var
  I: Integer;
begin
  RTLConvert(FRect);
  RTLConvert(FSignRect);
  for I := 0 to ItemCount - 1 do
  begin
    RTLConvert(Items[I].Rect);
    RTLConvert(Items[I].SelectionRect);
  end;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetGroup(AIndex: Integer)
  : TdxNavBarGroup;
begin
  Result := Items[AIndex].Group;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetItems(AIndex: Integer)
  : TdxNavBarOverflowPanelViewInfoItem;
begin
  Result := TdxNavBarOverflowPanelViewInfoItem(FItems[AIndex]);
end;

function TdxNavBarCustomOverflowPanelViewInfo.GetSmallImagesCount: Integer;
begin
  if NavBar.SmallImages <> nil then
    Result := NavBar.SmallImages.Count
  else
    Result := 0;
end;

{ TdxNavBarOverflowPanelPainter }

procedure TdxNavBarOverflowPanelPainter.DrawBackground(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo);
begin
  with APanel.ViewInfo do
    TdxNavBarElementPainterAccess(APanel.ViewInfo.Painter)
      .OverflowPanelBackgroundPainterClass.DrawButton(ACanvas, APanel.Rect,
      OverflowPanelImage, OverflowPanelBackColor, OverflowPanelBackColor2,
      OverflowPanelAlphaBlend, OverflowPanelAlphaBlend2,
      OverflowPanelGradientMode, BorderColor, [], ScaleFactor);
end;

procedure TdxNavBarOverflowPanelPainter.DrawItem(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo;
  AItem: TdxNavBarOverflowPanelViewInfoItem);
begin
  if not AItem.Rect.IsEmpty and not TdxNavBarElementPainterAccess
    (APanel.ViewInfo.Painter).ImagePainterClass.DrawImage(ACanvas,
    APanel.GetImageList, AItem.ImageIndex, AItem.Rect) then
    cxDrawImage(ACanvas.Handle, AItem.Rect, AItem.Rect,
      TdxNavBarViewInfoAccess(APanel.ViewInfo).GetDefaultOverflowPanelBitmap,
      nil, -1, idmNormal);
end;

procedure TdxNavBarOverflowPanelPainter.DrawSign(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo);

  procedure InternalDrawOverflowPanelSign(R: TRect);
  begin
    DrawOverflowPanelItemBackground(ACanvas, APanel, -1,
      dxNavBarPart(nbOverflowPanelSign), APanel.SignRect);
    cxDrawImage(ACanvas.Handle, R, R, dxOffice11NavPaneOverflowPanelBitmap, nil,
      -1, idmNormal);
  end;

var
  ARect: TRect;
  ASize: TSize;
  APrevGraphicsMode: Integer;
  ATransform, APrevTransform: TXForm;
begin
  if dxOffice11NavPaneOverflowPanelBitmap <> nil then
  begin
    ARect := APanel.SignRect;
    if ARect.Left >= APanel.Rect.Left then
    begin
      ASize := APanel.ScaleFactor.Apply
        (cxSize(dxOffice11NavPaneOverflowPanelBitmap.Width,
        dxOffice11NavPaneOverflowPanelBitmap.Height));
      ARect := cxRectCenter(ARect, ASize);

      if APanel.NavBar.UseRightToLeftAlignment then
      begin
        APrevGraphicsMode := SetGraphicsMode(ACanvas.Handle, GM_ADVANCED);
        try
          GetWorldTransform(ACanvas.Handle, APrevTransform);
          ATransform := APrevTransform;
          ATransform.eM11 := -1;
          ATransform.eDx := 2 * ARect.Left + ARect.Width;
          SetWorldTransform(ACanvas.Handle, ATransform);
          try
            InternalDrawOverflowPanelSign(ARect);
          finally
            SetWorldTransform(ACanvas.Handle, APrevTransform);
          end;
        finally
          SetGraphicsMode(ACanvas.Handle, APrevGraphicsMode);
        end;
      end
      else
        InternalDrawOverflowPanelSign(ARect);
    end;
  end;
end;

procedure TdxNavBarOverflowPanelPainter.DrawPopupMenuItem
  (APanel: TdxNavBarCustomOverflowPanelViewInfo; ACanvas: TCanvas; ARect: TRect;
  AImageList: TCustomImageList; AImageIndex: Integer; AText: string;
  AState: TdxNavBarObjectStates);
var
  R: TRect;
  ANavBar: TdxCustomNavBar;
  APainter: TdxNavBarElementPainterAccess;
  ASmallImageWidth: Integer;
begin
  ANavBar := APanel.NavBar;
  APainter := TdxNavBarElementPainterAccess
    (ANavBar.Painter as TdxNavBarElementPainter);

  APainter.BackgroundPainterClass.DrawBackground(ACanvas, ARect, nil, False,
    clNone, clMenu, clMenu, 255, 255, gmVertical, APanel.ScaleFactor);

  ASmallImageWidth := TdxNavBarViewInfoAccess(APanel.ViewInfo)
    .GetSmallImageWidth;
  R := ARect;
  R.Right := R.Left + 2 * APanel.GetPopupMenuImageIndent + ASmallImageWidth;
  if ANavBar.UseRightToLeftAlignment then
    R := TdxRightToLeftLayoutConverter.ConvertRect(R, ARect);
  APainter.BackgroundPainterClass.DrawBackground(ACanvas, R, nil, False, clNone,
    dxOffice11NavPaneGroupCaptionColor1, dxOffice11NavPaneGroupCaptionColor2,
    255, 255, gmHorizontal, APanel.ScaleFactor);
  if AText <> '-' then
  begin
    InflateRect(R, -1, -1);
    if sSelected in AState then
    begin
      APainter.ButtonPainterClass.DrawButton(ACanvas, ARect, nil,
        dxOffice11NavPaneGroupCaptionHotColor1,
        dxOffice11NavPaneGroupCaptionHotColor2, 255, 255, gmVertical,
        dxOffice11NavPaneBorder, [], APanel.ScaleFactor);
      if sActive in AState then
        APainter.ButtonPainterClass.DrawButton(ACanvas, R, nil,
          dxOffice11NavPaneGroupCaptionPressedColor1,
          dxOffice11NavPaneGroupCaptionPressedColor2, 255, 255, gmVertical,
          dxOffice11NavPaneBorder, [], APanel.ScaleFactor);
    end
    else if sActive in AState then
      APainter.ButtonPainterClass.DrawButton(ACanvas, R, nil,
        dxOffice11NavPaneGroupCaptionHotColor1,
        dxOffice11NavPaneGroupCaptionHotColor1, 255, 255, gmVertical,
        dxOffice11NavPaneBorder, [], APanel.ScaleFactor);

    InflateRect(R, 1 - APanel.GetPopupMenuImageIndent,
      1 - APanel.GetPopupMenuImageIndent);
    cxDrawImage(ACanvas.Handle, R, R, nil, AImageList, AImageIndex,
      EnabledImageDrawModeMap[not(sDisabled in AState)]);

    R := ARect;
    R.Left := R.Left + 2 * APanel.GetPopupMenuImageIndent + ASmallImageWidth +
      APanel.GetPopupMenuTextIndent;
    if ANavBar.UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, ARect);
    if sDisabled in AState then
      ACanvas.Font.Color := clGrayText
    else if (sSelected in AState) and IsHighContrastWhite then
      ACanvas.Font.Color := clHighlightText
    else
      ACanvas.Font.Color := clMenuText;

    cxDrawText(ACanvas, AText, R, ANavBar.DrawTextBiDiModeFlags(DT_LEFT or
      DT_VCENTER or DT_SINGLELINE));
  end
  else
  begin
    ACanvas.Pen.Color := dxGetDarkerColor
      (dxOffice11NavPaneGroupCaptionColor2, 80);
    if ANavBar.UseRightToLeftAlignment then
    begin
      ACanvas.MoveTo(ARect.Left, R.Top + cxRectHeight(ARect) div 2);
      ACanvas.LineTo(R.Left - cxRectWidth(R) div 2,
        R.Top + cxRectHeight(ARect) div 2);
    end
    else
    begin
      ACanvas.MoveTo(R.Right + cxRectWidth(R) div 2,
        R.Top + cxRectHeight(ARect) div 2);
      ACanvas.LineTo(ARect.Right, R.Top + cxRectHeight(ARect) div 2);
    end;
  end;
end;

procedure TdxNavBarOverflowPanelPainter.Draw(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo);
begin
  DrawBackground(ACanvas, APanel);
  if APanel.SignRect.Width > 0 then
    DrawSign(ACanvas, APanel);
  DrawItems(ACanvas, APanel);
end;

procedure TdxNavBarOverflowPanelPainter.DrawItems(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo);
var
  I: Integer;
  AItem: TdxNavBarOverflowPanelViewInfoItem;
begin
  for I := 0 to APanel.ItemCount - 1 do
  begin
    AItem := APanel.Items[I];
    DrawOverflowPanelItemBackground(ACanvas, APanel, I,
      dxNavBarPart(AItem.PartIndex, I), AItem.SelectionRect);
    DrawItem(ACanvas, APanel, AItem);
  end;
end;

procedure TdxNavBarOverflowPanelPainter.DrawOverflowPanelItemBackground
  (ACanvas: TCanvas; APanel: TdxNavBarCustomOverflowPanelViewInfo;
  AItemIndex: Integer; const APart: TdxNavBarPart; const ARect: TRect);
begin
  DrawOverflowPanelItemBackground(ACanvas, APanel, AItemIndex,
    TdxNavBarControllerAccess(APanel.ViewInfo.NavBar.Controller)
    .GetPartState(APart), ARect);
end;

procedure TdxNavBarOverflowPanelPainter.DrawOverflowPanelItemBackground
  (ACanvas: TCanvas; APanel: TdxNavBarCustomOverflowPanelViewInfo;
  AItemIndex: Integer; AState: TdxNavBarPartState; const ARect: TRect);
var
  AColor1, AColor2: TColor;
begin
  case AState of
    oisPressed, oisHotCheck, oisDroppedDown:
      begin
        AColor1 := dxOffice11NavPaneGroupCaptionPressedHotColor1;
        AColor2 := dxOffice11NavPaneGroupCaptionPressedHotColor2;
      end;
    oisChecked:
      begin
        AColor1 := dxOffice11NavPaneGroupCaptionPressedColor1;
        AColor2 := dxOffice11NavPaneGroupCaptionPressedColor2;
      end;
    oisHot:
      begin
        AColor1 := dxOffice11NavPaneGroupCaptionHotColor1;
        AColor2 := dxOffice11NavPaneGroupCaptionHotColor2;
      end;
  else { oisNormal }
    AColor1 := clNone;
    AColor2 := clNone;
  end;

  if (AColor1 <> clNone) and (AColor2 <> clNone) then
    TdxNavBarElementPainterAccess(APanel.ViewInfo.Painter)
      .BackgroundPainterClass.DrawBackground(ACanvas, ARect, nil, False, clNone,
      AColor1, AColor2, 255, 255, TdxBarStyleGradientMode.gmVertical,
      APanel.ViewInfo.ScaleFactor);
end;

{ TdxNavBarCustomOverflowPanelPopupMenu }

constructor TdxNavBarCustomOverflowPanelPopupMenu.Create
  (APanel: TdxNavBarCustomOverflowPanelViewInfo);
begin
  inherited Create(APanel.NavBar);
  FPanel := APanel;
  RecreateImageList;
end;

destructor TdxNavBarCustomOverflowPanelPopupMenu.Destroy;
begin
  FreeAndNil(FImageList);
  inherited Destroy;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.Populate;
begin
  Items.Clear;
  RecreateImageList;

  if NavBar.SmallImages <> nil then
  begin
    ImageList.Height := ScaleFactor.Apply(NavBar.SmallImages.Height);
    ImageList.Width := ScaleFactor.Apply(NavBar.SmallImages.Width);
  end;
  AssignImage(ImageList, dxOffice11NavPaneArrowUpBitmap);
  AssignImage(ImageList, dxOffice11NavPaneArrowDownBitmap);
  AssignImage(ImageList, TdxNavBarViewInfoAccess(NavBar.ViewInfo)
    .GetDefaultOverflowPanelBitmap);

  Images := ImageList;

  PopulateItems(GetSmallImagesCount);
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.RecreateImageList;
begin
  FreeAndNil(FImageList);
  FImageList := TImageList.Create(FPanel.NavBar);
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.Show(const APoint: TPoint);
begin
  Populate;
  Popup(APoint.X, APoint.Y);
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.DoHiddenItemClick
  (Sender: TObject);
var
  AElement: TObject;
begin
  AElement := TObject(TMenuItem(Sender).Tag);
  if AElement is TdxNavBarGroup then
    NavBar.ActiveGroup := TdxNavBarGroup(AElement);
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.PopulateItems(ADefaultImageIndex
  : Integer);
begin
  AddVisibleForCustomizationGroupItems(ADefaultImageIndex);
  CreateHiddenGroupList(Items, ADefaultImageIndex + 2);
end;

function TdxNavBarCustomOverflowPanelPopupMenu.AddItem(AParentItem: TMenuItem;
  ACaption: string; AEnabled: Boolean = True; AOnClick: TNotifyEvent = nil;
  AImageIndex: Integer = -1; AChecked: Boolean = False): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := ACaption;
  Result.OnDrawItem := DoDrawItem;
  if ACaption <> '-' then
  begin
    Result.OnMeasureItem := DoMeasureItem;
    Result.OnClick := AOnClick;
    Result.Checked := AChecked;
    Result.Enabled := AEnabled;
    Result.ImageIndex := AImageIndex;
  end;
  AParentItem.Add(Result);
end;

function TdxNavBarCustomOverflowPanelPopupMenu.GetImageIndex
  (AGroup: TdxNavBarGroup; ADefaultImageIndex: Integer): Integer;
begin
  if IsImageAssigned(NavBar.SmallImages, AGroup.SmallImageIndex) then
    Result := AGroup.SmallImageIndex
  else
    Result := ADefaultImageIndex;
end;

function TdxNavBarCustomOverflowPanelPopupMenu.GetImageIndex
  (AItem: TdxNavBarOverflowPanelViewInfoItem;
  ADefaultImageIndex: Integer): Integer;
begin
  if IsImageAssigned(NavBar.SmallImages, AItem.ImageIndex) then
    Result := AItem.ImageIndex
  else
    Result := ADefaultImageIndex;
end;

function TdxNavBarCustomOverflowPanelPopupMenu.GetSmallImagesCount: Integer;
begin
  Result := FPanel.GetSmallImagesCount;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.CreateHiddenGroupList
  (AParentMenuItem: TMenuItem; ADefaultImageIndex: Integer);

  function IsChecked(AItem: TdxNavBarOverflowPanelViewInfoItem): Boolean;
  begin
    Result := (AItem.PartIndex = nbOverflowPanelGroup) and
      (NavBar.ActiveGroup = AItem.Group);
  end;

var
  AItem: TdxNavBarOverflowPanelViewInfoItem;
  I: Integer;
  AMenuItem: TMenuItem;
begin
  if AParentMenuItem.Count > 0 then
    AddItem(AParentMenuItem, '-');
  for I := FPanel.VisibleItemCount to FPanel.ItemCount - 1 do
  begin
    AItem := FPanel.Items[I];
    AMenuItem := AddItem(AParentMenuItem, AItem.Caption, True,
      DoHiddenItemClick, GetImageIndex(AItem, ADefaultImageIndex),
      IsChecked(AItem));
    AMenuItem.Tag := TdxNativeInt(AItem.PartElement);
  end;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.
  CreateVisibleForCustomizationGroupList(AParentMenuItem: TMenuItem;
  ADefaultImageIndex: Integer);
var
  I: Integer;
  AItem: TMenuItem;
begin
  for I := 0 to NavBar.Groups.Count - 1 do
  begin
    if NavBar.Groups[I].VisibleForCustomization then
    begin
      AItem := AddItem(AParentMenuItem, NavBar.Groups[I].Caption, True,
        DoAddRemoveButtonsClick, GetImageIndex(NavBar.Groups[I],
        ADefaultImageIndex), NavBar.Groups[I].Visible);
      AItem.AutoCheck := True;
      AItem.Tag := I;
    end;
  end;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.DoAddRemoveButtonsClick
  (Sender: TObject);
var
  AMenuItem: TMenuItem;
  AGroup: TdxNavBarGroup;
begin
  AMenuItem := Sender as TMenuItem;
  AGroup := NavBar.Groups[AMenuItem.Tag];
  AGroup.Visible := AMenuItem.Checked;
end;

function TdxNavBarCustomOverflowPanelPopupMenu.GetNavBar: TdxCustomNavBar;
begin
  Result := FPanel.NavBar;
end;

function TdxNavBarCustomOverflowPanelPopupMenu.GetScaleFactor: TdxScaleFactor;
begin
  Result := FPanel.ScaleFactor;
end;

function TdxNavBarCustomOverflowPanelPopupMenu.AllowCustomizing: Boolean;
begin
  Result := FPanel.AllowCustomizing;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.
  AddVisibleForCustomizationGroupItems(ADefaultImageIndex: Integer);
var
  AParentMenuItem: TMenuItem;
begin
  if AllowCustomizing then
  begin
    AParentMenuItem := AddItem(Items,
      cxGetResourceString(@sdxNavBarOffice11AddRemoveButtons),
      NavBar.Groups.Count > 0);
    CreateVisibleForCustomizationGroupList(AParentMenuItem,
      ADefaultImageIndex + 2);
  end;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.AssignImage
  (AImageList: TImageList; ASourceBitmap: TBitmap);
var
  ABitmap: TBitmap;
  ASideSize: Integer;
  AStretchRect: TRect;
  AMaskColor: TColor;
begin
  if AImageList.BkColor = clNone then
    AMaskColor := clFuchsia
  else
    AMaskColor := AImageList.BkColor;

  ABitmap := cxCreateBitmap(AImageList.Width, AImageList.Height);
  try
    ASideSize := Min(ABitmap.Width, ABitmap.Height);
    AStretchRect.Left := (ABitmap.Width - ASideSize) div 2;
    AStretchRect.Right := AStretchRect.Left + ASideSize;
    AStretchRect.Top := (ABitmap.Height - ASideSize) div 2;
    AStretchRect.Bottom := AStretchRect.Top + ASideSize;
    ABitmap.Canvas.Brush.Color := AMaskColor;
    ABitmap.Canvas.FillRect(cxRect(0, 0, ABitmap.Width, ABitmap.Height));
    ABitmap.Canvas.StretchDraw(AStretchRect, ASourceBitmap);
    AImageList.AddMasked(ABitmap, AMaskColor);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.DoDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; ASelected: Boolean);
var
  AMenuItem: TMenuItem;
  AState: TdxNavBarObjectStates;
  AImageList: TCustomImageList;
  AImageIndex: Integer;
begin
  AMenuItem := Sender as TMenuItem;
  AState := [];
  if ASelected then
    Include(AState, sSelected);
  if AMenuItem.Checked then
    Include(AState, sActive);
  if not AMenuItem.Enabled then
    Include(AState, sDisabled);

  AImageIndex := AMenuItem.ImageIndex;
  if IsImageAssigned(NavBar.SmallImages, AImageIndex) then
    AImageList := NavBar.SmallImages
  else
  begin
    AImageList := ImageList;
    Dec(AImageIndex, FPanel.GetSmallImagesCount);
  end;
  if Assigned(OnDrawItem) then
    OnDrawItem(ACanvas, ARect, AImageList, AImageIndex,
      AMenuItem.Caption, AState);
end;

procedure TdxNavBarCustomOverflowPanelPopupMenu.DoMeasureItem(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
begin
  Height := 2 * FPanel.GetPopupMenuImageIndent + TdxNavBarViewInfoAccess
    (FPanel.ViewInfo).GetSmallImageHeight;
  dxAdjustToTouchableSize(Height, FPanel.ScaleFactor);
end;

end.
