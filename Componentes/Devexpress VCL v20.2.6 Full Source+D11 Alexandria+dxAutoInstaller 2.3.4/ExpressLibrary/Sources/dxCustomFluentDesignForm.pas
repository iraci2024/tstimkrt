{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library controls }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxCustomFluentDesignForm;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Graphics, Forms, Classes, Controls, SysUtils,
  dxCore, dxMessages, cxGeometry, dxCoreGraphics, dxCoreClasses, cxGraphics,
  cxClasses, cxLookAndFeels, dxForms, dxFluentDesignFormInterfaces;

const
  dxDefaultInlineModeThreshold = 650;
  dxDefaultOverlayModeThreshold = 450;

type
  TdxFluentDesignFormAdaptiveLayoutOptions = class(TcxOwnedPersistent)
  strict private
    FEnabled: Boolean;
    FInlineModeThreshold: Integer;
    FOverlayModeThreshold: Integer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInlineModeThreshold(Value: Integer);
    procedure SetOverlayModeThreshold(Value: Integer);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property InlineModeThreshold: Integer read FInlineModeThreshold
      write SetInlineModeThreshold default dxDefaultInlineModeThreshold;
    property OverlayModeThreshold: Integer read FOverlayModeThreshold
      write SetOverlayModeThreshold default dxDefaultOverlayModeThreshold;
  end;

{$REGION 'for internal use'}

  TdxFluentDesignContainerControlHelper = class
  public
    class procedure CheckStyle(AControl: TWinControl); overload;
    class procedure CheckStyle(AControl: TWinControl;
      AHasAcrylicParent: Boolean); overload;
  end;

  TdxAcrylicHostControlHelper = class
    class procedure DoUpdateControls(AParentControl: TWinControl;
      const AIsOnGlass: Boolean; AHostControl: IdxAcrylicHostControl);
  public
    class procedure UpdateChildControls(AControl: TWinControl;
      AIsOnGlass: Boolean; AHostControl: IdxAcrylicHostControl);
    class procedure UpdateControl(AControl: TControl; AIsOnGlass: Boolean;
      AHostControl: IdxAcrylicHostControl);
  end;
{$ENDREGION}
  { TdxCustomFluentDesignForm }

  TdxCustomFluentDesignForm = class(TdxForm, IdxAcrylicHostControl,
    IdxFluentDesignNavigationControlListener,
    IcxLookAndFeelNotificationListener)
  strict private
  type
{$REGION 'private types'}
    TdxNavigationControlSavedState = record
    private
      FAlign: TAlign;
      FAnchors: TAnchors;
      FBounds: TRect;
      FParent: TWinControl;
    public
      procedure CheckNotification(AComponent: TComponent);
      procedure Restore(AControl: TWinControl;
        AForm: TdxCustomFluentDesignForm);
      procedure Store(AControl: TWinControl; AForm: TdxCustomFluentDesignForm);
    end;
{$ENDREGION}
  strict private
    FAdaptiveLayoutOptions: TdxFluentDesignFormAdaptiveLayoutOptions;
    FEnableAcrylicEffects: Boolean;
    FIsAcrylic: Boolean;
    FLoadedEnableAcrylicEffects: Boolean;
    FLoadedNavigationControl: TWinControl;
    FNavigationControl: TWinControl;
    FNavigationControlSavedState: TdxNavigationControlSavedState;

    function IsEnableAcrylicEffectsStored: Boolean;
    procedure SetAdaptiveLayoutOptions
      (AValue: TdxFluentDesignFormAdaptiveLayoutOptions);
    procedure SetEnableAcrylicEffects(AValue: Boolean);
    procedure SetNavigationControl(AValue: TWinControl);
    procedure CMControlListChange(var Message: TCMControlListChange);
      message CM_CONTROLLISTCHANGE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    // IcxLookAndFeelNotificationListener
    function GetObject: TObject;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues);
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
    // IdxAcrylicHostControl
    function GetAcrylicColor: TdxAlphaColor;
    function IsAcrylic: Boolean;
    // IdxFluentDesignNavigationControlListener
    procedure IdxFluentDesignNavigationControlListener.Changed =
      NavigationControlChanged;
    procedure NavigationControlChanged
      (AChanges: TdxFluentDesignNavigationControlChangeTypes); virtual;
    procedure CreateWnd; override;
    procedure DoAcrylicNCActivate(var AMessage: TWMNCActivate);
    function GetCurrentAcrylicColor(AIsActive: Boolean): TdxAlphaColor;
    function GetNavigationControl: IdxFluentDesignNavigationControl;
    function GetPaintBlackOpaqueOnGlass: Boolean; virtual;
    procedure InitializeNewForm; override;
    function IsAcrylicEnabledByDefault: Boolean; virtual;
    function IsAcrylicSupported: Boolean; virtual;
    function IsNavigationControlValid: Boolean;
    procedure UpdateChildControls;
    procedure Loaded; override;
    procedure LookAndFeelChanged; virtual;
    procedure NavigationControlAssigned; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetAcrylic(AValue: Boolean); virtual;
    procedure UpdateFluentDesignContainers;
    procedure WndProc(var Message: TMessage); override;
    //
    property AdaptiveLayoutOptions: TdxFluentDesignFormAdaptiveLayoutOptions
      read FAdaptiveLayoutOptions write SetAdaptiveLayoutOptions;
    property EnableAcrylicEffects: Boolean read FEnableAcrylicEffects
      write SetEnableAcrylicEffects stored IsEnableAcrylicEffectsStored;
    property NavigationControl: TWinControl read FNavigationControl
      write SetNavigationControl;
  public
    destructor Destroy; override;
  end;

implementation

uses
  UxTheme, DwmApi, TypInfo, Menus, dxDpiAwareUtils, dxThreading,
  dxAcrylicEffect, cxControls;

{ TdxFluentDesignFormAdaptiveLayoutOptions }

constructor TdxFluentDesignFormAdaptiveLayoutOptions.Create
  (AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInlineModeThreshold := dxDefaultInlineModeThreshold;
  FOverlayModeThreshold := dxDefaultOverlayModeThreshold;
end;

procedure TdxFluentDesignFormAdaptiveLayoutOptions.DoAssign
  (Source: TPersistent);
var
  ASource: TdxFluentDesignFormAdaptiveLayoutOptions;
begin
  inherited DoAssign(Source);
  if Source is TdxFluentDesignFormAdaptiveLayoutOptions then
  begin
    ASource := TdxFluentDesignFormAdaptiveLayoutOptions(Source);
    Enabled := ASource.Enabled;
    InlineModeThreshold := ASource.InlineModeThreshold;
    OverlayModeThreshold := ASource.OverlayModeThreshold;
  end;
end;

procedure TdxFluentDesignFormAdaptiveLayoutOptions.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TdxFluentDesignFormAdaptiveLayoutOptions.SetInlineModeThreshold
  (Value: Integer);
begin
  FInlineModeThreshold := Value;
end;

procedure TdxFluentDesignFormAdaptiveLayoutOptions.SetOverlayModeThreshold
  (Value: Integer);
begin
  FOverlayModeThreshold := Value;
end;

{ TdxFluentDesignContainerControlHelper }

class procedure TdxFluentDesignContainerControlHelper.CheckStyle
  (AControl: TWinControl);
var
  AHostControl: IdxAcrylicHostControl;
begin
  if Supports(AControl.Parent, IdxAcrylicHostControl, AHostControl) then
    CheckStyle(AControl, AHostControl.IsAcrylic);
end;

class procedure TdxFluentDesignContainerControlHelper.CheckStyle
  (AControl: TWinControl; AHasAcrylicParent: Boolean);
var
  AContainer: IdxFluentDesignContainerControl;
begin
  if AControl.HandleAllocated then
    if AHasAcrylicParent then
    begin
      if (GetWindowLong(AControl.Handle, GWL_EXSTYLE) and WS_EX_LAYERED) = 0
      then
      begin
        cxSetLayeredWindowAttributes(AControl.Handle, 0,
          dxDefaultOpaqueContainerTransparentColor, LWA_COLORKEY);
        if Supports(AControl, IdxFluentDesignContainerControl, AContainer) then
          AContainer.NotifyTurnedOpaque;
      end;
    end
    else
      cxSetLayeredWindowAttributes(AControl.Handle, 0, 0, 0);
end;

class procedure TdxAcrylicHostControlHelper.DoUpdateControls(AParentControl
  : TWinControl; const AIsOnGlass: Boolean;
  AHostControl: IdxAcrylicHostControl);
var
  I: Integer;
  AControl: TControl;
begin
  for I := 0 to AParentControl.ControlCount - 1 do
  begin
    AControl := AParentControl.Controls[I];
    UpdateControl(AControl, AIsOnGlass, AHostControl);
  end;
end;

class procedure TdxAcrylicHostControlHelper.UpdateChildControls
  (AControl: TWinControl; AIsOnGlass: Boolean;
  AHostControl: IdxAcrylicHostControl);
begin
  DoUpdateControls(AControl, AIsOnGlass, AHostControl);
end;

class procedure TdxAcrylicHostControlHelper.UpdateControl(AControl: TControl;
  AIsOnGlass: Boolean; AHostControl: IdxAcrylicHostControl);
var
  AIsOpaqueContainer: Boolean;
  AContainer: IdxFluentDesignContainerControl;
  AAcrylicHostControlListener: IdxAcrylicHostControlListener;
begin
  Supports(AControl, IdxFluentDesignContainerControl, AContainer);
  AIsOpaqueContainer := (AContainer <> nil) and AContainer.CanBeOpaque and
    AIsOnGlass;
  if AIsOnGlass and not AIsOpaqueContainer then
    AControl.ControlState := AControl.ControlState + [csGlassPaint]
  else
    AControl.ControlState := AControl.ControlState - [csGlassPaint];
  if AControl is TWinControl then
  begin
    DoUpdateControls(TWinControl(AControl), AIsOnGlass and
      not AIsOpaqueContainer, AHostControl);
    if AIsOpaqueContainer or (AContainer <> nil) and not AIsOnGlass then
      TdxFluentDesignContainerControlHelper.CheckStyle(TWinControl(AControl),
        AIsOnGlass);
    if Supports(AControl, IdxAcrylicHostControlListener,
      AAcrylicHostControlListener) then
      AAcrylicHostControlListener.Changed(AHostControl, AIsOnGlass);
  end;
end;

{ TdxCustomFluentDesignForm }

destructor TdxCustomFluentDesignForm.Destroy;
begin
  NavigationControl := nil;
  FreeAndNil(FAdaptiveLayoutOptions);
  RootLookAndFeel.RemoveChangeListener(Self);
  inherited Destroy;
end;

// IcxLookAndFeelNotificationListener

function TdxCustomFluentDesignForm.GetObject: TObject;
begin
  Result := Self;
end;

procedure TdxCustomFluentDesignForm.MasterLookAndFeelChanged
  (Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  LookAndFeelChanged;
end;

procedure TdxCustomFluentDesignForm.MasterLookAndFeelDestroying
  (Sender: TcxLookAndFeel);
begin
end;

// IdxAcrylicHostControl

function TdxCustomFluentDesignForm.GetAcrylicColor: TdxAlphaColor;
begin
  Result := dxDefaultAcrylicColor;
end;

function TdxCustomFluentDesignForm.IsAcrylic: Boolean;
begin
  Result := FIsAcrylic;
end;

procedure TdxCustomFluentDesignForm.NavigationControlChanged
  (AChanges: TdxFluentDesignNavigationControlChangeTypes);
begin
end;

procedure TdxCustomFluentDesignForm.CMControlListChange
  (var Message: TCMControlListChange);
begin
  if IsAcrylic then
    if Message.Inserting then
      TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, UpdateChildControls)
    else
      TdxAcrylicHostControlHelper.UpdateControl(Message.Control, False, Self);
end;

procedure TdxCustomFluentDesignForm.CreateWnd;
var
  AWasAcrylic: Boolean;
begin
  inherited CreateWnd;
  AWasAcrylic := FIsAcrylic;
  if FEnableAcrylicEffects and IsAcrylicSupported then
    FIsAcrylic := dxApplyAcrylicAccent(Handle, True,
      GetCurrentAcrylicColor(Active))
  else
    FIsAcrylic := False;
  if AWasAcrylic <> FIsAcrylic then
    UpdateChildControls;
end;

procedure TdxCustomFluentDesignForm.DoAcrylicNCActivate
  (var AMessage: TWMNCActivate);
var
  AColor: TdxAlphaColor;
begin
  AColor := GetCurrentAcrylicColor(AMessage.Active);
  dxApplyAcrylicAccent(Handle, True, AColor);
end;

function TdxCustomFluentDesignForm.GetCurrentAcrylicColor(AIsActive: Boolean)
  : TdxAlphaColor;
begin
  if AIsActive then
    Result := GetAcrylicColor
  else
    Result := TdxAlphaColors.FromArgb($FF, GetAcrylicColor);
end;

function TdxCustomFluentDesignForm.GetNavigationControl
  : IdxFluentDesignNavigationControl;
begin
  Supports(FNavigationControl, IdxFluentDesignNavigationControl, Result);
end;

function TdxCustomFluentDesignForm.GetPaintBlackOpaqueOnGlass: Boolean;
begin
  Result := True;
end;

procedure TdxCustomFluentDesignForm.InitializeNewForm;
begin
  inherited;
  FEnableAcrylicEffects := IsAcrylicEnabledByDefault;
  FLoadedEnableAcrylicEffects := IsAcrylicEnabledByDefault;
  FAdaptiveLayoutOptions := TdxFluentDesignFormAdaptiveLayoutOptions.
    Create(Self);
  RootLookAndFeel.AddChangeListener(Self);
end;

function TdxCustomFluentDesignForm.IsAcrylicEnabledByDefault: Boolean;
begin
  Result := False;
end;

function TdxCustomFluentDesignForm.IsAcrylicSupported: Boolean;
begin
  Result := not(csDesigning in ComponentState) and
    IsWinSupportsAcrylicEffect and (Menu = nil) and
    not(FormStyle in [fsMDIChild, fsMDIForm]) and (Parent = nil);
end;

function TdxCustomFluentDesignForm.IsNavigationControlValid: Boolean;
begin
  Result := (FNavigationControl <> nil) and
    not(csDestroying in FNavigationControl.ComponentState);
end;

procedure TdxCustomFluentDesignForm.UpdateChildControls;
begin
  if not(csDesigning in ComponentState) then
    TdxAcrylicHostControlHelper.UpdateChildControls(Self, FIsAcrylic, Self);
end;

procedure TdxCustomFluentDesignForm.Loaded;
var
  AAcrylic: Boolean;
begin
  inherited Loaded;
  NavigationControl := FLoadedNavigationControl;
  if EnableAcrylicEffects <> FLoadedEnableAcrylicEffects then
    EnableAcrylicEffects := FLoadedEnableAcrylicEffects
  else if HandleAllocated then
  begin
    AAcrylic := EnableAcrylicEffects and IsAcrylicSupported;
    if FIsAcrylic <> AAcrylic then
      SetAcrylic(AAcrylic);
  end;
end;

procedure TdxCustomFluentDesignForm.LookAndFeelChanged;
begin
  if not(csLoading in ComponentState) and HandleAllocated then
    PostMessage(Handle, DXM_SKINS_POSTSKINCHANGED, 0, 0);
end;

procedure TdxCustomFluentDesignForm.NavigationControlAssigned;
begin
end;

procedure TdxCustomFluentDesignForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FNavigationControl then
      NavigationControl := nil
    else if FNavigationControl <> nil then
      FNavigationControlSavedState.CheckNotification(AComponent);
  end;
end;

procedure TdxCustomFluentDesignForm.SetAcrylic(AValue: Boolean);
begin
  FIsAcrylic := AValue;
  if FIsAcrylic then
    dxApplyAcrylicAccent(Handle, FIsAcrylic, GetCurrentAcrylicColor(Active))
  else
    dxApplyAcrylicAccent(Handle, FIsAcrylic, 0);
  UpdateChildControls;
end;

procedure TdxCustomFluentDesignForm.UpdateFluentDesignContainers;
var
  AControl: TWinControl;
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TWinControl then
    begin
      AControl := TWinControl(Controls[I]);
      if AControl.Visible and AControl.HandleAllocated and
        Supports(AControl, IdxFluentDesignContainerControl) and
        ((GetWindowLong(AControl.Handle, GWL_EXSTYLE) and WS_EX_LAYERED) <> 0)
        and IsWindowVisible(AControl.Handle) then
      begin
        cxRedrawWindow(AControl.Handle, RDW_ERASE or RDW_INVALIDATE or
          RDW_FRAME or RDW_ALLCHILDREN or RDW_UPDATENOW or RDW_ERASENOW);
      end;
    end;
end;

procedure TdxCustomFluentDesignForm.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    DXM_SKINS_POSTSKINCHANGED:
      begin
        if EnableAcrylicEffects then
          SetAcrylic(IsAcrylicSupported);
      end;
    WM_MOVE, WM_SIZE:
      begin
        if FIsAcrylic then
          DwmFlush;
        inherited;
      end
  else
    inherited;
  end;
end;

function TdxCustomFluentDesignForm.IsEnableAcrylicEffectsStored: Boolean;
begin
  Result := FEnableAcrylicEffects <> IsAcrylicEnabledByDefault;
end;

procedure TdxCustomFluentDesignForm.SetAdaptiveLayoutOptions
  (AValue: TdxFluentDesignFormAdaptiveLayoutOptions);
begin
  FAdaptiveLayoutOptions.Assign(AValue);
end;

procedure TdxCustomFluentDesignForm.SetEnableAcrylicEffects(AValue: Boolean);
var
  AAcrylic: Boolean;
begin
  if csLoading in ComponentState then
    FLoadedEnableAcrylicEffects := AValue
  else if FEnableAcrylicEffects <> AValue then
  begin
    FEnableAcrylicEffects := AValue;
    if HandleAllocated and (FIsAcrylic <> FEnableAcrylicEffects) then
    begin
      AAcrylic := FEnableAcrylicEffects and IsAcrylicSupported;
      SetAcrylic(AAcrylic);
    end;
  end;
end;

procedure TdxCustomFluentDesignForm.SetNavigationControl(AValue: TWinControl);
begin
  if csLoading in ComponentState then
    FLoadedNavigationControl := AValue
  else
  begin
    if not Supports(AValue, IdxFluentDesignNavigationControl) then
      AValue := nil;
    if FNavigationControl <> AValue then
    begin
      if IsNavigationControlValid then
      begin
        FNavigationControlSavedState.Restore(FNavigationControl, Self);
        GetNavigationControl.RemoveListener(Self);
        FNavigationControl.RemoveFreeNotification(Self);
      end;
      FNavigationControl := AValue;
      if FNavigationControl <> nil then
      begin
        FNavigationControl.FreeNotification(Self);
        GetNavigationControl.AddListener(Self);
        FNavigationControlSavedState.Store(FNavigationControl, Self);
        if FNavigationControl.Parent <> Self then
        begin
          FNavigationControl.Parent := Self;
          if FIsAcrylic then
            TdxAcrylicHostControlHelper.UpdateControl(FNavigationControl,
              True, Self);
        end;
      end;
      NavigationControlAssigned;
    end;
  end;
end;

procedure TdxCustomFluentDesignForm.WMPaint(var Message: TWMPaint);
begin
  if FIsAcrylic then
    dxPaintControlOnGlass(Self, GetPaintBlackOpaqueOnGlass)
  else
    inherited;
end;

procedure TdxCustomFluentDesignForm.TdxNavigationControlSavedState.
  CheckNotification(AComponent: TComponent);
begin
  if FParent = AComponent then
    FParent := nil;
end;

procedure TdxCustomFluentDesignForm.TdxNavigationControlSavedState.Restore
  (AControl: TWinControl; AForm: TdxCustomFluentDesignForm);
begin
  if (FParent <> nil) and (FParent <> AForm) then
    FParent.RemoveFreeNotification(AForm);
  AControl.Parent := FParent;
  AControl.Anchors := FAnchors;
  AControl.Align := FAlign;
  AControl.BoundsRect := dxGetScaleFactor(AControl).Apply(FBounds);
end;

procedure TdxCustomFluentDesignForm.TdxNavigationControlSavedState.Store
  (AControl: TWinControl; AForm: TdxCustomFluentDesignForm);
begin
  if (AControl.Parent <> nil) and (AControl.Parent <> AForm) then
    AControl.Parent.FreeNotification(AForm);
  FAnchors := AControl.Anchors;
  FAlign := AControl.Align;
  FBounds := dxGetScaleFactor(AControl).Revert(AControl.BoundsRect);
  FParent := AControl.Parent;
end;

end.
