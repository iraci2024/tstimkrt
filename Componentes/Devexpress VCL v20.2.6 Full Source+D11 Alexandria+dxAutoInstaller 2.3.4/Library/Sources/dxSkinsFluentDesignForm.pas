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

unit dxSkinsFluentDesignForm;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs, Messages, Windows,
  dxCore, dxCoreGraphics, dxCoreClasses, dxCustomFluentDesignForm,
  dxFluentDesignFormInterfaces,
  cxGeometry, cxLookAndFeels;

type
  TdxFluentDesignForm = class(TdxCustomFluentDesignForm, IdxFluentDesignForm)
  strict private
    FExtendNavigationControlToCaption: Boolean;
    procedure SetExtendNavigationControlToCaption(AValue: Boolean);
    procedure UpdateCaption;
    procedure CMShowingChanged(var Message: TMessage);
      message CM_SHOWINGCHANGED;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;
  protected
    procedure InitializeNewForm; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;

    function GetNavigationControlBackgroundColor: TdxAlphaColor;
    function GetNavigationControlForegroundColor: TColor;
    function GetExtendedNavigationControlCaptionWidth: Integer;
    function IsNavigationControlCollapsed: Boolean;
    // IdxFluentDesignNavigationControlListener
    procedure NavigationControlChanged
      (AChanges: TdxFluentDesignNavigationControlChangeTypes); override;

    function IsAcrylicEnabledByDefault: Boolean; override;
    function IsAcrylicSupported: Boolean; override;
    procedure NavigationControlAssigned; override;
    procedure SetAcrylic(AValue: Boolean); override;
  published
    property AdaptiveLayoutOptions;
    property EnableAcrylicEffects;
    property ExtendNavigationControlToCaption: Boolean
      read FExtendNavigationControlToCaption
      write SetExtendNavigationControlToCaption default True;
    property NavigationControl;
  end;

implementation

uses
  UxTheme, DwmApi, dxMessages, cxGraphics, cxControls, dxSkinsForm,
  dxAcrylicEffect;

procedure TdxFluentDesignForm.InitializeNewForm;
begin
  inherited InitializeNewForm;
  FExtendNavigationControlToCaption := True;
end;

procedure TdxFluentDesignForm.Resize;
var
  ANavigationControl: IdxFluentDesignNavigationControl;
begin
  inherited Resize;
  if (AdaptiveLayoutOptions <> nil) and AdaptiveLayoutOptions.Enabled then
  begin
    ANavigationControl := GetNavigationControl;
    if (ANavigationControl <> nil) and ANavigationControl.IsAdaptable and
      ((NavigationControl.Align = alLeft) and (ANavigationControl.GetDisplayMode
      <> ncdmOverlayMinimal) or (NavigationControl.Align = alTop) and
      (ANavigationControl.GetDisplayMode = ncdmOverlayMinimal)) then
    begin
      if Width >= ScaleFactor.Apply(AdaptiveLayoutOptions.InlineModeThreshold)
      then
        ANavigationControl.SetDisplayMode(ncdmInline)
      else if (Width < ScaleFactor.Apply
        (AdaptiveLayoutOptions.InlineModeThreshold)) and
        (Width >= ScaleFactor.Apply(AdaptiveLayoutOptions.OverlayModeThreshold))
      then
        ANavigationControl.SetDisplayMode(ncdmOverlay)
      else
        ANavigationControl.SetDisplayMode(ncdmOverlayMinimal);
    end;
  end;
end;

procedure TdxFluentDesignForm.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_NCACTIVATE:
      if IsAcrylic then
      begin
        DoAcrylicNCActivate(TWMNCActivate(Message));
        Message.Result := 1;
      end
      else
        inherited;
  else
    inherited;
  end;
end;

function TdxFluentDesignForm.GetNavigationControlBackgroundColor: TdxAlphaColor;
var
  ANavigationControl: IdxFluentDesignNavigationControl;
begin
  ANavigationControl := GetNavigationControl;
  if ANavigationControl <> nil then
    Result := ANavigationControl.GetBackgroundColor
  else
    Result := clDefault;
end;

function TdxFluentDesignForm.GetNavigationControlForegroundColor: TColor;
var
  ANavigationControl: IdxFluentDesignNavigationControl;
begin
  ANavigationControl := GetNavigationControl;
  if ANavigationControl <> nil then
    Result := ANavigationControl.GetForegroundColor
  else
    Result := clDefault;
end;

function TdxFluentDesignForm.GetExtendedNavigationControlCaptionWidth: Integer;
begin
  if FExtendNavigationControlToCaption and (Menu = nil) and
    (NavigationControl <> nil) and NavigationControl.Visible and
    (NavigationControl.Align = alLeft) and (NavigationControl.Left = 0) and
    (NavigationControl.Top = 0) and (GetNavigationControl <> nil) and
    GetNavigationControl.GetAcrylicEnabled and
    (GetNavigationControl.GetDisplayMode = ncdmInline) then
    Result := NavigationControl.Width
  else
    Result := 0;
end;

function TdxFluentDesignForm.IsNavigationControlCollapsed: Boolean;
var
  ANavigationControl: IdxFluentDesignNavigationControl;
begin
  ANavigationControl := GetNavigationControl;
  if ANavigationControl <> nil then
    Result := ANavigationControl.IsCollapsed
  else
    Result := False;
end;

procedure TdxFluentDesignForm.NavigationControlChanged
  (AChanges: TdxFluentDesignNavigationControlChangeTypes);
begin
  if ExtendNavigationControlToCaption then
    UpdateCaption;
end;

function TdxFluentDesignForm.IsAcrylicEnabledByDefault: Boolean;
begin
  Result := True;
end;

function TdxFluentDesignForm.IsAcrylicSupported: Boolean;
begin
  Result := inherited IsAcrylicSupported and
    TdxSkinWinController.IsSkinActive(Handle);
end;

procedure TdxFluentDesignForm.NavigationControlAssigned;
begin
  if NavigationControl <> nil then
    if GetNavigationControl.GetDisplayMode <> ncdmOverlayMinimal then
    begin
      if NavigationControl.Align <> alLeft then
        NavigationControl.Align := alLeft;
    end
    else
    begin
      if NavigationControl.Align <> alTop then
        NavigationControl.Align := alTop;
    end;
  if ExtendNavigationControlToCaption then
    UpdateCaption;
end;

procedure TdxFluentDesignForm.SetAcrylic(AValue: Boolean);
begin
  inherited SetAcrylic(AValue);
  TdxSkinController.Refresh(Self);
  Refresh;
end;

procedure TdxFluentDesignForm.SetExtendNavigationControlToCaption
  (AValue: Boolean);
begin
  if AValue <> FExtendNavigationControlToCaption then
  begin
    FExtendNavigationControlToCaption := AValue;
    UpdateCaption;
  end;
end;

procedure TdxFluentDesignForm.UpdateCaption;
begin
  if HandleAllocated then
    cxRedrawNCRect(Handle, cxEmptyRect);
end;

procedure TdxFluentDesignForm.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Visible and HandleAllocated and IsAcrylic then
    UpdateFluentDesignContainers;
end;

procedure TdxFluentDesignForm.WMWindowPosChanged(var Message
  : TWMWindowPosChanged);
const
  SWP_STATECHANGED = $8000;
var
  AWindowPos: TWindowPos;
begin
  inherited;
  AWindowPos := Message.WindowPos^;
  if (AWindowPos.Flags and SWP_STATECHANGED) = SWP_STATECHANGED then
    UpdateFluentDesignContainers;
end;

end.
