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

unit dxAcrylicEffect;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Graphics, dxCoreGraphics;

const
  dxDefaultAcrylicColor = $2C9B9B9B;
  dxDefaultOpaqueContainerTransparentColor = $FFFAFA;

type
  TdxAccentPolicy = record
    AccentState: Integer;
    AccentFlags: Integer;
    GradientColor: Integer;
    AnimationId: Integer;
  end;

  TdxWinCompAttrData = record
    Attribute: THandle;
    DataPointer: Pointer;
    DataSize: ULONG;
  end;

var
  SetWindowCompositionAttribute: function(Wnd: HWND;
    const AttrData: TdxWinCompAttrData): BOOL;
stdcall = nil;

function dxApplyAcrylicAccent(AHandle: HWND; AEnabled: Boolean;
  AColor: TdxAlphaColor): Boolean;

implementation

function dxApplyAcrylicAccent(AHandle: HWND; AEnabled: Boolean;
  AColor: TdxAlphaColor): Boolean;
const
  ACCENT_DISABLED = 0;
  ACCENT_ENABLE_BLURBEHIND = 3;
  ACCENT_ENABLE_ACRYLICBLURBEHIND = 4;
  WCA_ACCENT_POLICY = 19;
var
  AAccentPolicy: TdxAccentPolicy;
  AData: TdxWinCompAttrData;
begin
  if @SetWindowCompositionAttribute <> nil then
  begin
    if AEnabled then
      AAccentPolicy.AccentState := ACCENT_ENABLE_ACRYLICBLURBEHIND
    else
      AAccentPolicy.AccentState := ACCENT_DISABLED;
    AAccentPolicy.AnimationId := 0;
    AAccentPolicy.AccentFlags := $10;
    AData.Attribute := WCA_ACCENT_POLICY;
    AAccentPolicy.GradientColor := AColor;
    AData.DataSize := SizeOf(AAccentPolicy);
    AData.DataPointer := @AAccentPolicy;
    Result := SetWindowCompositionAttribute(AHandle, AData);
  end
  else
    Result := False;
end;

initialization

SetWindowCompositionAttribute := GetProcAddress(GetModuleHandle(user32),
  'SetWindowCompositionAttribute');

end.
