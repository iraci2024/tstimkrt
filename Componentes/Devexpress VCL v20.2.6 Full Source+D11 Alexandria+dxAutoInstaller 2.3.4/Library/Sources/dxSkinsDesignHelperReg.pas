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

unit dxSkinsDesignHelperReg;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, cxControls, cxLookAndFeels, Dialogs,
  Types, cxLookAndFeelPainters, dxSkinsLookAndFeelPainter, DesignIntf, Menus,
  DesignEditors, dxSkinsStrs, dxSkinsForm, ToolsApi, cxClasses;

procedure Register;

implementation

uses
  dxSkinsDesignHelper, dxSkinsReg, cxLibraryReg, cxScrollBar, cxButtons,
  cxRadioGroup;

procedure dxSkinsRequiresUnits(AProc: TGetStrProc);
var
  AItem: TdxSkinsUnitStateListItem;
  I: Integer;
begin
  dxSkinsProjectSettings.UpdateActiveProjectSettings;
  if dxSkinsProjectSettings.Enabled then
  begin
    AProc('dxSkinsCore');
    for I := 0 to dxSkinsProjectSettings.UnitStateList.Count - 1 do
    begin
      AItem := dxSkinsProjectSettings.UnitStateList.Item[I];
      if AItem.Enabled then
        AProc(AItem.SkinUnitName);
    end;
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
end;

initialization

FdxSkinsListFilterProc := dxSkinsListFilter;
FdxSkinModifyProjectOptionsProc := dxSkinsShowProjectOptionsDialog;
FdxSkinsRequiresUnits := dxSkinsRequiresUnits;

finalization

FdxSkinsRequiresUnits := nil;
FdxSkinModifyProjectOptionsProc := nil;
FdxSkinsListFilterProc := nil;

end.
