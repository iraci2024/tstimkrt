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

unit dxEditReg;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, DB, DesignIntf, DSDesign, FiltEdit,
  cxLookAndFeels;

procedure Register;

implementation

uses
  DesignEditors, VCLEditors, Windows, Dialogs, dximctrl, cxLibraryReg,
  cxEditReg, cxEditPropEditors,
  dxImagePropEditor, dxSpinImagePropEditor, dxShellDialogs;

type
  TdxImageControlItemProperties = class(TPropertyEditor)
  public
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TdxSpinImageItemsProperties = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TcxCommonFileDialogFileNameProperty }

  TcxCommonFileDialogFileNameProperty = class(TcxFileNameProperty)
  protected
    function CreateOpenDialog(AOwner: TComponent): TOpenDialog; override;
  end;

  { TdxCommonFileDialogComponentEditor }

  TdxCommonFileDialogComponentEditor = class
    (TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  public
    procedure Edit; override;
  end;

  { TdxImageControlItemProperties }

function TdxImageControlItemProperties.GetValue: string;
begin
  Result := Format('(%s)', [TStrings.ClassName]);
end;

function TdxImageControlItemProperties.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog];
end;

procedure TdxImageControlItemProperties.Edit;
begin
  if (ExpressImageItemsPropEditor(GetComponent(0) as TWinControl)) then
    Modified;
end;

{ TdxSpinImageItemsProperties }

function TdxSpinImageItemsProperties.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TdxSpinImageItemsProperties.Edit;
begin
  if (ExpressSpinImageItemsPropEditor(GetComponent(0) as TdxCustomSpinImage))
  then
    Modified;
end;

{ TcxCommonFileDialogFileNameProperty }

function TcxCommonFileDialogFileNameProperty.CreateOpenDialog
  (AOwner: TComponent): TOpenDialog;
begin
  Result := TdxOpenFileDialog.Create(AOwner);
end;

{ TdxCommonFileDialogComponentEditor }

procedure TdxCommonFileDialogComponentEditor.Edit;
begin
  if TdxCommonFileDialog(Component).Execute then
    Designer.Modified;
end;

function TdxCommonFileDialogComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxCommonFileDialog(Component).LookAndFeel;
end;

procedure TdxCommonFileDialogComponentEditor.InternalExecuteVerb
  (AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited InternalExecuteVerb(AIndex - 1);
end;

function TdxCommonFileDialogComponentEditor.InternalGetVerb
  (AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := '&Test Dialog'
  else
    Result := inherited InternalGetVerb(AIndex - 1);
end;

function TdxCommonFileDialogComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := inherited InternalGetVerbCount + 1;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(cxEditorsUtilitiesProductPage,
    [TdxImageListBox, TdxSpinImage, TdxImageComboBox, TdxOpenFileDialog,
    TdxSaveFileDialog]);

  RegisterComponentEditor(TdxImageListBox,
    TcxCustomEditorsLibraryComponentEditor);
  RegisterComponentEditor(TdxSpinImage, TcxCustomEditorsLibraryComponentEditor);
  RegisterComponentEditor(TdxImageComboBox,
    TcxCustomEditorsLibraryComponentEditor);
  RegisterComponentEditor(TdxCommonFileDialog,
    TdxCommonFileDialogComponentEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TdxCustomImageListBox, 'Items',
    TdxImageControlItemProperties);
  RegisterPropertyEditor(TypeInfo(TStrings), TdxImageComboBox, 'Items',
    TdxImageControlItemProperties);
  RegisterPropertyEditor(TypeInfo(TdxSpinImageItems), TdxCustomSpinImage,
    'Items', TdxSpinImageItemsProperties);
  RegisterPropertyEditor(TypeInfo(TFileName), TdxCommonFileDialog, 'FileName',
    TcxCommonFileDialogFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxCommonFileDialog, 'Filter',
    TFilterProperty);
end;

end.
