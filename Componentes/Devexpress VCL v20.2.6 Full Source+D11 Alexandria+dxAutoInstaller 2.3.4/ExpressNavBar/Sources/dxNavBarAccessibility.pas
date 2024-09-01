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

unit dxNavBarAccessibility; // for internal use

{$I cxVer.inc}

interface

uses
  Windows, Classes, RTLConsts, cxAccessibility, cxClasses, cxControls,
  dxCoreClasses,
  dxNavBar, dxNavBarBase, dxNavBarCollns;

type
  { TdxNavBarAccessibilityHelper }

  TdxNavBarAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetNavBar: TdxCustomNavBar;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;

    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarGroupContainerAccessibilityHelper }

  TdxNavBarGroupContainerAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper)
  private
    function GetNavBar: TdxCustomNavBar;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;

    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarGroupAccessibilityHelper }

  TdxNavBarGroupAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetGroup: TdxNavBarGroup;
    function GetGroupViewInfo: TdxNavBarGroupViewInfo;
    function GetNavBar: TdxCustomNavBar;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;

    property Group: TdxNavBarGroup read GetGroup;
    property GroupViewInfo: TdxNavBarGroupViewInfo read GetGroupViewInfo;
    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarGroupCaptionPanelAccessibilityHelper }

  TdxNavBarGroupCaptionPanelAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper)
  private
    function GetGroup: TdxNavBarGroup;
    function GetGroupViewInfo: TdxNavBarGroupViewInfo;
    function GetNavBar: TdxCustomNavBar;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    procedure DoDefaultAction(AChildID
      : TcxAccessibleSimpleChildElementID); override;
    function GetDefaultActionDescription
      (AChildID: TcxAccessibleSimpleChildElementID): string; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function CanBeFocusedByDefault: Boolean; override;
    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function GetClipBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;
    procedure MakeVisible; override;

    property Group: TdxNavBarGroup read GetGroup;
    property GroupViewInfo: TdxNavBarGroupViewInfo read GetGroupViewInfo;
    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarItemLinkContainerAccessibilityHelper }

  TdxNavBarItemLinkContainerAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper)
  private
    function GetGroup: TdxNavBarGroup;
    function GetGroupViewInfo: TdxNavBarGroupViewInfo;
    function GetNavBar: TdxCustomNavBar;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;
    function IsScrollable(out AInvisiblePartHeight, AChildMinTopScreenBound
      : Integer): Boolean; override;

    property Group: TdxNavBarGroup read GetGroup;
    property GroupViewInfo: TdxNavBarGroupViewInfo read GetGroupViewInfo;
    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarItemLinkAccessibilityHelper }

  TdxNavBarItemLinkAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper)
  private
    function GetLink: TdxNavBarItemLink;
    function GetLinkViewInfo: TdxNavBarLinkViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    procedure DoSelect(AFlags: Integer;
      AChildID: TcxAccessibleSimpleChildElementID); override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    // IdxNavBarAccessibilityHelper
    procedure RemoveFocus; override;

    function CanBeFocusedByDefault: Boolean; override;
    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function GetClipBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;
    procedure MakeVisible; override;

    property Link: TdxNavBarItemLink read GetLink;
    property LinkViewInfo: TdxNavBarLinkViewInfo read GetLinkViewInfo;
  end;

implementation

uses
  Math, Types, cxGraphics, cxGeometry;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarViewInfoAccess = class(TdxNavBarViewInfo);
  TdxNavBarGroupAccess = class(TdxNavBarGroup);

function GetWindowClientOrigin(AWnd: HWND): TPoint;
begin
  Result := cxNullPoint;
  ClientToScreen(AWnd, Result);
end;

{ TdxNavBarAccessibilityHelper }

function TdxNavBarAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarAccessibilityHelper.GetChild(AIndex: Integer)
  : TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
    Result := NavBar.GroupContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + 1;
end;

function TdxNavBarAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := 'Navigation';
end;

function TdxNavBarAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PANE;
end;

function TdxNavBarAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

function TdxNavBarAccessibilityHelper.GetBounds: TRect;
begin
  Result := cxGetWindowRect(OwnerObjectWindow);
  Result := cxRectOffset(Result,
    cxPointInvert(OwnerObjectControl.ClientOrigin));
end;

function TdxNavBarAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarAccessibilityHelper.GetNavBar: TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(FOwnerObject);
end;

{ TdxNavBarGroupContainerAccessibilityHelper }

function TdxNavBarGroupContainerAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetChild(AIndex: Integer)
  : TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
    Result := NavBar.RootGroups[AIndex - inherited GetChildCount]
      .IAccessibilityHelper.GetHelper;
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + NavBar.RootGroupCount;
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := NavBar.IAccessibilityHelper.GetHelper
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_OUTLINE;
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetBounds: TRect;
begin
  Result := TdxNavBarViewInfoAccess(NavBar.ViewInfo).GroupsArea;
end;

function TdxNavBarGroupContainerAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarGroupContainerAccessibilityHelper.GetNavBar: TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(FOwnerObject);
end;

{ TdxNavBarGroupAccessibilityHelper }

function TdxNavBarGroupAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarGroupAccessibilityHelper.GetChild(AIndex: Integer)
  : TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
  begin
    Dec(AIndex, inherited GetChildCount);
    // Requires
    Assert(AIndex < 2);
    //
    if AIndex = 0 then
      Result := Group.CaptionPanelIAccessibilityHelper.GetHelper
    else
      Result := Group.LinkContainerIAccessibilityHelper.GetHelper;
  end;
end;

function TdxNavBarGroupAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + 2;
end;

function TdxNavBarGroupAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := Group.Caption;
end;

function TdxNavBarGroupAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  if Group.Parent = nil then
    Result := NavBar.GroupContainerIAccessibilityHelper.GetHelper
  else
    Result := Group.Parent.LinkContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarGroupAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_OUTLINEITEM;
end;

function TdxNavBarGroupAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;
end;

function TdxNavBarGroupAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not Group.Visible or (GroupViewInfo = nil) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarGroupAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

function TdxNavBarGroupAccessibilityHelper.GetBounds: TRect;
begin
  Result := GroupViewInfo.Rect
end;

function TdxNavBarGroupAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarGroupAccessibilityHelper.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(FOwnerObject);
end;

function TdxNavBarGroupAccessibilityHelper.GetNavBar: TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(OwnerObjectControl);
end;

function TdxNavBarGroupAccessibilityHelper.GetGroupViewInfo
  : TdxNavBarGroupViewInfo;
begin
  Result := NavBar.ViewInfo.GetGroupViewInfoByGroup(Group);
end;

{ TdxNavBarGroupCaptionPanelAccessibilityHelper }

function TdxNavBarGroupCaptionPanelAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarGroupCaptionPanelAccessibilityHelper.DoDefaultAction
  (AChildID: TcxAccessibleSimpleChildElementID);
begin
  inherited;
  if TdxNavBarViewInfoAccess(TdxCustomNavBar(OwnerObjectControl).ViewInfo).CanHasActiveGroup
  then
    TdxCustomNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.
  GetDefaultActionDescription(AChildID
  : TcxAccessibleSimpleChildElementID): string;
begin
  if TdxNavBarViewInfoAccess(GroupViewInfo.ViewInfo).CanHasVisibleItemsInGroup
    (Group) then
    Result := 'Collapse'
  else
    Result := 'Expand';
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := Group.Caption;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := Group.IAccessibilityHelper.GetHelper;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_GROUPING;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and not GroupViewInfo.IsCaptionVisible
  then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE
  else
  begin
    Result := Result or cxSTATE_SYSTEM_FOCUSABLE;
    if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
      TdxNavBarViewInfoAccess(GroupViewInfo.ViewInfo).CanHasVisibleItemsInGroup
      (Group) then
      Result := Result or cxSTATE_SYSTEM_EXPANDED
    else
      Result := Result or cxSTATE_SYSTEM_COLLAPSED;
    if IsFocused then
      Result := Result or cxSTATE_SYSTEM_FOCUSED;
  end;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.
  CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarGroupCaptionPanelAccessibilityHelper.Click(AKey: Word);
var
  ACanNavBarHasActiveGroup: Boolean;
begin
  inherited Click(AKey);
  ACanNavBarHasActiveGroup := TdxNavBarViewInfoAccess
    (TdxCustomNavBar(OwnerObjectControl).ViewInfo).CanHasActiveGroup;
  if ACanNavBarHasActiveGroup then
    TdxCustomNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group)
  else
  begin
    if AKey in [VK_ADD, VK_SUBTRACT] then
      Group.Expanded := AKey = VK_ADD
    else
      Group.Expanded := not Group.Expanded;
  end;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetBounds: TRect;
begin
  Result := GroupViewInfo.CaptionRect;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetClipBounds: TRect;
var
  AGroup: TdxNavBarGroup;
begin
  AGroup := Group;
  if AGroup.Parent = nil then
    Result := inherited GetClipBounds
  else
  begin
    while AGroup.Parent <> nil do
      AGroup := AGroup.Parent;
    Result := AGroup.LinkContainerIAccessibilityHelper.GetHelper.GetScreenBounds
      (cxAccessibleObjectSelfID);
  end;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
var
  ACanNavBarHasActiveGroup: Boolean;
begin
  Result := inherited IsClickKey(AKey);
  if Result then
    Exit;
  ACanNavBarHasActiveGroup := TdxNavBarViewInfoAccess
    (TdxCustomNavBar(OwnerObjectControl).ViewInfo).CanHasActiveGroup;
  if ACanNavBarHasActiveGroup then
    Result := AKey in [VK_RETURN, VK_SPACE]
  else
    Result := Group.Expandable and (AKey in [VK_ADD, VK_SPACE, VK_SUBTRACT]);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.IsContainer: Boolean;
begin
  Result := False;
end;

procedure TdxNavBarGroupCaptionPanelAccessibilityHelper.MakeVisible;
begin
  TdxNavBarViewInfoAccess(TdxCustomNavBar(OwnerObjectControl).ViewInfo)
    .MakeGroupVisible(Group, False, False);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(FOwnerObject);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetGroupViewInfo
  : TdxNavBarGroupViewInfo;
begin
  Result := NavBar.ViewInfo.GetGroupViewInfoByGroup(Group);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetNavBar
  : TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(OwnerObjectControl);
end;

{ TdxNavBarItemLinkContainerAccessibilityHelper }

function TdxNavBarItemLinkContainerAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetChild(AIndex: Integer)
  : TcxAccessibilityHelper;
var
  AChild: TObject;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
  begin
    Result := nil;
    AChild := Group.Children[AIndex - inherited GetChildCount];
    if AChild is TdxNavBarItemLink then
      Result := TdxNavBarItemLink(AChild).IAccessibilityHelper.GetHelper
    else if AChild is TdxNavBarGroup then
      Result := TdxNavBarGroup(AChild).IAccessibilityHelper.GetHelper;
  end;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + Group.ChildCount;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := Group.IAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PANE;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    (not GroupViewInfo.IsItemsVisible or Group.UseControl and Group.ShowControl)
  then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetBounds: TRect;
begin
  Result := TdxNavBarGroupAccessibilityHelper(Parent).GroupViewInfo.ItemsRect;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.IsScrollable
  (out AInvisiblePartHeight, AChildMinTopScreenBound: Integer): Boolean;
var
  AViewInfo: TdxNavBarViewInfo;
begin
  AViewInfo := TdxCustomNavBar(OwnerObjectControl).ViewInfo;
  if TdxCustomNavBar(OwnerObjectControl).OptionsBehavior.Common.AllowChildGroups
  then
    Result := TdxCustomNavBarAccess(OwnerObjectControl)
      .ActiveGroupScrollBar.Visible
  else
    Result := AViewInfo.IsTopScrollButtonVisible or
      AViewInfo.IsBottomScrollButtonVisible;
  if Result then
  begin
    AInvisiblePartHeight := TdxNavBarItemLinkContainerAccessibilityHelper
      (Children[ChildCount - 1]).GetBounds.Bottom -
      TdxNavBarItemLinkContainerAccessibilityHelper(Children[0]).GetBounds.Top -
      cxRectHeight(GetBounds);
    AChildMinTopScreenBound := TdxNavBarItemLinkContainerAccessibilityHelper
      (Children[0]).GetScreenBounds(cxAccessibleObjectSelfID).Top;
  end;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(FOwnerObject);
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetGroupViewInfo
  : TdxNavBarGroupViewInfo;
begin
  Result := NavBar.ViewInfo.GetGroupViewInfoByGroup(Group);
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetNavBar
  : TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(OwnerObjectControl);
end;

{ TdxNavBarItemLinkAccessibilityHelper }

function TdxNavBarItemLinkAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemLinkAccessibilityHelper.DoSelect(AFlags: Integer;
  AChildID: TcxAccessibleSimpleChildElementID);
begin
  if AFlags and cxSELFLAG_TAKESELECTION > 0 then
  begin
    if CanFocus(False) then
      TdxCustomNavBarAccess(OwnerObjectControl).FocusedAccessibleObject := Self;
    TdxCustomNavBarAccess(OwnerObjectControl).DoLinkMouseUp(Link);
  end;
end;

function TdxNavBarItemLinkAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := Link.Item.Caption;
end;

function TdxNavBarItemLinkAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := Link.Group.LinkContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemLinkAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_OUTLINEITEM;
end;

function TdxNavBarItemLinkAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    ((Link.Item = nil) or not Link.Item.Visible) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE
  else if not Link.CanSelect then
    Result := Result or cxSTATE_SYSTEM_UNAVAILABLE
  else
  begin
    Result := Result or cxSTATE_SYSTEM_FOCUSABLE or cxSTATE_SYSTEM_SELECTABLE;
    if Selectable then
      Result := Result or cxSTATE_SYSTEM_SELECTABLE;
    if IsFocused then
      Result := Result or cxSTATE_SYSTEM_FOCUSED;
    if Link.Selected then
      Result := Result or cxSTATE_SYSTEM_SELECTED;
  end;
end;

function TdxNavBarItemLinkAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopSelect];
end;

procedure TdxNavBarItemLinkAccessibilityHelper.RemoveFocus;
var
  AGroupCaptionPanelAccessibilityHelper
    : TdxNavBarGroupCaptionPanelAccessibilityHelper;
begin
  AGroupCaptionPanelAccessibilityHelper :=
    TdxNavBarGroupCaptionPanelAccessibilityHelper
    (TdxNavBarItemLinkContainerAccessibilityHelper(Parent)
    .Group.CaptionPanelIAccessibilityHelper.GetHelper);
  if AGroupCaptionPanelAccessibilityHelper.CanFocus(True) then
    TdxCustomNavBarAccess(OwnerObjectControl).FocusedAccessibleObject :=
      AGroupCaptionPanelAccessibilityHelper
  else
    inherited RemoveFocus;
end;

function TdxNavBarItemLinkAccessibilityHelper.CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemLinkAccessibilityHelper.Click(AKey: Word);
begin
  inherited Click(AKey);
  TdxCustomNavBarAccess(OwnerObjectControl).DoLinkMouseUp(Link);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetBounds: TRect;
begin
  if (IsRectEmpty(LinkViewInfo.ImageRect) or
    cxRectContain(LinkViewInfo.SelectionRect, LinkViewInfo.ImageRect)) and
    (IsRectEmpty(LinkViewInfo.CaptionRect) or
    cxRectContain(LinkViewInfo.SelectionRect, LinkViewInfo.CaptionRect)) then
    Result := LinkViewInfo.SelectionRect
  else
    Result := cxRectUnion(LinkViewInfo.ImageRect, LinkViewInfo.CaptionRect);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetClipBounds: TRect;
var
  AGroup: TdxNavBarGroup;
begin
  AGroup := Link.Group;
  while AGroup.Parent <> nil do
    AGroup := AGroup.Parent;
  Result := AGroup.LinkContainerIAccessibilityHelper.GetHelper.GetScreenBounds
    (cxAccessibleObjectSelfID);
end;

function TdxNavBarItemLinkAccessibilityHelper.IsClickKey(AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarItemLinkAccessibilityHelper.IsContainer: Boolean;
begin
  Result := False;
end;

procedure TdxNavBarItemLinkAccessibilityHelper.MakeVisible;
begin
  TdxNavBarViewInfoAccess(TdxCustomNavBar(OwnerObjectControl).ViewInfo)
    .MakeLinkVisible(Link, False);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetLink: TdxNavBarItemLink;
begin
  Result := TdxNavBarItemLink(FOwnerObject);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetLinkViewInfo
  : TdxNavBarLinkViewInfo;
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  AGroupViewInfo := (Parent.Parent as TdxNavBarGroupAccessibilityHelper)
    .GroupViewInfo;
  Result := AGroupViewInfo.GetLinkViewInfoByLink(Link);
  // Ensures
  Assert(Result <> nil);
end;

end.
