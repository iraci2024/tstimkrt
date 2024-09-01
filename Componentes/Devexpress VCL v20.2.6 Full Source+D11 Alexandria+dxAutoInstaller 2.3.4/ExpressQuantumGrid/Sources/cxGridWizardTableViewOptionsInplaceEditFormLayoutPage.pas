{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressQuantumGrid }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL }
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

unit cxGridWizardTableViewOptionsInplaceEditFormLayoutPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  dxCore, cxClasses, cxGridCustomView, cxGridWizardCustomPage, cxGridWizardStrs,
  dxCustomWizardControl,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutControlAdapters, dxLayoutContainer,
  dxLayoutLookAndFeels, dxLayoutControl, cxGridTableView, cxGridInplaceEditForm,
  cxGridViewLayoutContainer,
  cxGridViewLayoutCustomizationForm;

type
  TcxGridInplaceEditFormCustomizationControllerAccess = class
    (TcxGridTableViewInplaceEditFormCustomizationController);

  { TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame }

  TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame = class
    (TcxGridWizardCustomPageFrame)
    lcMainItem1: TdxLayoutItem;
    pnPreviewGrid: TPanel;
  private
    function GetCustomizationForm: TcxGridViewLayoutCustomizationForm;
    function GetEditFormCustomizationController
      : TcxGridInplaceEditFormCustomizationControllerAccess;
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplySettings; override;
    procedure BeforeDestruction; override;
    procedure GridViewChanged; override;
    procedure LoadSettings; override;
    procedure PageActivating; override;

    property CustomizationForm: TcxGridViewLayoutCustomizationForm
      read GetCustomizationForm;
    property EditFormCustomizationController
      : TcxGridInplaceEditFormCustomizationControllerAccess
      read GetEditFormCustomizationController;
  end;

implementation

uses
  cxGridWizardCustomHelper, cxGrid;

type
  TcxGridInplaceEditFormAccess = class(TcxGridInplaceEditForm);
  TcxGridTableControllerAccess = class(TcxGridTableController);
  TdxLayoutContainerAccess = class(TdxLayoutContainer);

{$R *.dfm}
  { TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame }

procedure TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  ApplySettings;
begin
  if CustomizationForm <> nil then
  begin
    CustomizationForm.ApplyChanges;
    EditFormCustomizationController.HideForm;
    EditFormCustomizationController.DoCustomization;
    Helper.Assign(PreviewGrid.ActiveView);
  end;
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  BeforeDestruction;
begin
  inherited BeforeDestruction;
  if CustomizationForm <> nil then
  begin
    EditFormCustomizationController.HideForm;
    EditFormCustomizationController.DoCustomization;
  end;
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  GridViewChanged;
begin
  Visible := (TcxGridTableView(Helper.GridView).OptionsBehavior.EditMode <>
    emInplace) and not TcxGridTableView(Helper.GridView)
    .EditForm.UseDefaultLayout;
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  LoadSettings;
begin
  Helper.PreparePreview(PreviewGrid.ActiveView);
  EditFormCustomizationController.DoCreateForm;
  EditFormCustomizationController.DoCustomization;
  CustomizationForm.Parent := pnPreviewGrid;
  CustomizationForm.BorderStyle := bsNone;
  CustomizationForm.Align := alClient;
  CustomizationForm.liOk.Visible := False;
  CustomizationForm.liCancel.Visible := False;
end;

procedure TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  PageActivating;
begin
  CustomizationForm.Show;
end;

function TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwInplaceEditFormLayoutPageDescription);
end;

function TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwInplaceEditFormLayoutPageTitle);
end;

function TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  GetCustomizationForm: TcxGridViewLayoutCustomizationForm;
begin
  Result := nil;
  if EditFormCustomizationController <> nil then
    Result := TcxGridViewLayoutCustomizationForm
      (EditFormCustomizationController.Form);
end;

function TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame.
  GetEditFormCustomizationController
  : TcxGridInplaceEditFormCustomizationControllerAccess;
var
  AController: TcxGridTableControllerAccess;
  AEditForm: TcxGridInplaceEditFormAccess;
begin
  Result := nil;
  if (PreviewGrid <> nil) and (PreviewGrid.ActiveView <> nil) and
    (PreviewGrid.ActiveView.Controller <> nil) then
  begin
    AController := TcxGridTableControllerAccess
      (PreviewGrid.ActiveView.Controller);
    AEditForm := TcxGridInplaceEditFormAccess(AController.InplaceEditForm);
    Result := TcxGridInplaceEditFormCustomizationControllerAccess
      (AEditForm.CustomizationController);
  end;
end;

end.
