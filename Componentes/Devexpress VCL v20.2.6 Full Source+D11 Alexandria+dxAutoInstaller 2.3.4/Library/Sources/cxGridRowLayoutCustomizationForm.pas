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

unit cxGridRowLayoutCustomizationForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls,
  ComCtrls, Forms, ImgList,
  ActnList, Dialogs, Menus, cxGraphics, cxControls, cxLookAndFeels, cxCheckBox,
  cxButtons, cxTreeView,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutPainters, dxLayoutCommon,
  dxLayoutCustomizeForm, dxLayoutControlAdapters, dxLayoutContainer,
  dxLayoutcxEditAdapters,
  dxLayoutControl, cxGridLayoutView, cxGridCustomView, dxLayoutLookAndFeels,
  cxStyles, cxClasses, cxGridLevel, cxGrid, cxGridViewLayoutContainer,
  cxGridTableView,
  cxGridViewLayoutCustomizationForm, cxGridRowLayout;

type
  { TcxGridRowLayoutCustomizationFormContainer }

  TcxGridRowLayoutCustomizationFormContainer = class
    (TcxGridViewLayoutCustomizationFormContainer)
  protected
    function GetCloneItemClass: TcxGridCustomLayoutItemClass; override;
  end;

  { TcxGridRowLayoutCustomizationFormLayoutControl }

  TcxGridRowLayoutCustomizationFormLayoutControl = class
    (TcxGridViewLayoutCustomizationFormLayoutControl)
  protected
    function GetContainerClass: TdxLayoutControlContainerClass; override;
  end;

  { TcxGridRowLayoutCustomizationForm }

  TcxGridRowLayoutCustomizationForm = class(TcxGridViewLayoutCustomizationForm)
  protected
    function GetGridViewContainerInstance: TdxLayoutContainer; override;
    function GetGridViewLayoutLookAndFeel: TdxLayoutCxLookAndFeel; override;
    function GetLayoutController: TcxGridRowLayoutController; virtual; abstract;
    procedure Load; override;
    procedure Save; override;

    procedure DoInitializeControl; override;
    function GetGridViewLayoutControlClass
      : TcxGridViewLayoutCustomizationFormLayoutControlClass; override;
  public
    procedure CancelChanges; override;
  end;

implementation

{$R *.dfm}
{ TcxGridRowLayoutCustomizationFormContainer }

function TcxGridRowLayoutCustomizationFormContainer.GetCloneItemClass
  : TcxGridCustomLayoutItemClass;
begin
  Result := TcxGridRowBaseLayoutItem;
end;

{ TcxGridRowLayoutCustomizationFormLayoutControl }

function TcxGridRowLayoutCustomizationFormLayoutControl.GetContainerClass
  : TdxLayoutControlContainerClass;
begin
  Result := TcxGridRowLayoutCustomizationFormContainer;
end;

{ TcxGridRowLayoutCustomizationForm }

procedure TcxGridRowLayoutCustomizationForm.CancelChanges;
begin
  inherited CancelChanges;
  if GetLayoutController.UseDefaultLayout and not GetLayoutController.Active
  then
    GetLayoutController.Container.DestroyLayout;
end;

function TcxGridRowLayoutCustomizationForm.GetGridViewContainerInstance
  : TdxLayoutContainer;
begin
  Result := GetLayoutController.Container;
end;

function TcxGridRowLayoutCustomizationForm.GetGridViewLayoutLookAndFeel
  : TdxLayoutCxLookAndFeel;
begin
  Result := GetLayoutController.LayoutLookAndFeel;
end;

procedure TcxGridRowLayoutCustomizationForm.Load;
var
  AContainer: TcxGridRowLayoutCustomizationFormContainer;
begin
  inherited Load;
  if GetLayoutController.UseDefaultLayout and not GetLayoutController.Container.IsLayoutCreated
  then
    GetLayoutController.Container.CreateLayout;
  GridViewLayoutControl.OptionsImage.Images := GridView.GetImages;
  AContainer := TcxGridRowLayoutCustomizationFormContainer
    (GridViewLayoutControl.Container);
  AContainer.CustomizationHelper.CopyStructure(GridViewContainer);
end;

procedure TcxGridRowLayoutCustomizationForm.Save;
begin
  inherited Save;
  GetLayoutController.UseDefaultLayout := False;
  GetLayoutController.CopyCustomizationSetting(GridViewLayoutControl.Container);
  SetDesignerModified(GridView);
end;

procedure TcxGridRowLayoutCustomizationForm.DoInitializeControl;
begin
  inherited DoInitializeControl;
  if GridView.IsDesigning then
    Caption := 'Layout Editor - ' + GridView.Name
end;

function TcxGridRowLayoutCustomizationForm.GetGridViewLayoutControlClass
  : TcxGridViewLayoutCustomizationFormLayoutControlClass;
begin
  Result := TcxGridRowLayoutCustomizationFormLayoutControl;
end;

end.
