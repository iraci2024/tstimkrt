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

unit cxGridWizardEMFTableViewHelper;

{$I cxVer.inc}

interface

uses
  SysUtils, Forms,
  dxCore, cxClasses, cxGraphics, dxCustomWizardControl, dxWizardControl,
  cxGridWizardStrs, cxGridCustomView,
  cxGridEMFTableView, cxGridWizardCustomHelper, cxGridWizardTableViewHelper,
  cxGridEMFDataDefinitions;

type

  { TcxGridWizardEMFTableViewHelper }

  TcxGridWizardEMFTableViewHelper = class(TcxGridWizardCustomTableViewHelper)
  private
    function GetDataController: TcxGridEMFDataController;
    function GetGridView: TcxGridEMFTableView;
    function GetItem(Index: Integer): TcxGridEMFColumn;
  protected
    procedure CheckComponentNames(AView: TcxCustomGridView); override;
    function GetItemFieldName(Index: Integer): string; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    procedure SetItemFieldName(Index: Integer; const AValue: string); override;
  public
    class function CanBeMasterView: Boolean; override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    function GetItemIndexByFieldName(const AFieldName: string)
      : Integer; override;

    property DataController: TcxGridEMFDataController read GetDataController;
    property GridView: TcxGridEMFTableView read GetGridView;
    property Items[Index: Integer]: TcxGridEMFColumn read GetItem;
  end;

implementation

{$R cxGridWizardEMFTableViewHelper.res}

uses
  cxGridWizardCustomPage, cxGridWizardEMFViewsDataSourcePage,
  cxGridWizardEMFViewsSelectItemsForDisplayPage,
  cxGridWizardCommonCustomizeItemsPage,
  cxGridWizardTableViewOptionsInterfaceElementsPage,
  cxGridWizardTableViewOptionsInplaceEditFormPage,
  cxGridWizardTableViewOptionsInplaceEditFormLayoutPage,
  cxGridWizardTableViewOptionsSummaryPage,
  cxGridWizardTableViewOptionsFilteringSortingPage,
  cxGridWizardTableViewOptionsBehaviorPage,
  cxGridWizardTableViewOptionsSizingPage,
  cxGridWizardUnboundViewsSelectItemsForDisplayPage, cxGridCommon;

const
  cxGridWizardEMFTableViewPages: array [0 .. 9]
    of TcxGridWizardCustomPageFrameClass =
    (TcxGridWizardEMFViewsDataSourcePageFrame,
    TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardCommonCustomizeItemsPageFrame,
    TcxGridWizardTableViewOptionsInterfaceElementsPageFrame,
    TcxGridWizardTableViewOptionsInplaceEditFormPageFrame,
    TcxGridWizardTableViewOptionsInplaceEditFormLayoutPageFrame,
    TcxGridWizardTableViewOptionsSummaryPageFrame,
    TcxGridWizardTableViewOptionsFilteringSortingPageFrame,
    TcxGridWizardTableViewOptionsBehaviorPageFrame,
    TcxGridWizardTableViewOptionsSizingPageFrame);

  { TcxGridWizardEMFTableViewHelper }

class function TcxGridWizardEMFTableViewHelper.CanBeMasterView: Boolean;
begin
  Result := True;
end;

class function TcxGridWizardEMFTableViewHelper.GetGridViewClass
  : TcxCustomGridViewClass;
begin
  Result := TcxGridEMFTableView;
end;

function TcxGridWizardEMFTableViewHelper.GetItemIndexByFieldName
  (const AFieldName: string): Integer;
var
  AColumn: TcxGridEMFColumn;
begin
  AColumn := GridView.GetColumnByFieldName(AFieldName);
  if AColumn <> nil then
    Result := AColumn.Index
  else
    Result := -1;
end;

procedure TcxGridWizardEMFTableViewHelper.CheckComponentNames
  (AView: TcxCustomGridView);
var
  ATableView: TcxGridEMFTableView;
  I: Integer;
begin
  ATableView := AView as TcxGridEMFTableView;
  for I := 0 to ATableView.ColumnCount - 1 do
    ATableView.Columns[I].Name :=
      CreateUniqueName(GetParentForm(ATableView.Control), ATableView,
      ATableView.Columns[I], ScxGridPrefixName,
      ATableView.Columns[I].DataBinding.FieldName);
end;

function TcxGridWizardEMFTableViewHelper.GetItemFieldName
  (Index: Integer): string;
begin
  Result := Items[Index].FieldName;
end;

function TcxGridWizardEMFTableViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardEMFTableViewPages);
end;

function TcxGridWizardEMFTableViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardEMFTableViewPages[Index];
end;

procedure TcxGridWizardEMFTableViewHelper.SetItemFieldName(Index: Integer;
  const AValue: string);
begin
  if Items[Index].DataBinding <> nil then
    Items[Index].DataBinding.FieldName := AValue;
end;

function TcxGridWizardEMFTableViewHelper.GetDataController
  : TcxGridEMFDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardEMFTableViewHelper.GetGridView: TcxGridEMFTableView;
begin
  Result := TcxGridEMFTableView(inherited GetGridView);
end;

function TcxGridWizardEMFTableViewHelper.GetItem(Index: Integer)
  : TcxGridEMFColumn;
begin
  Result := GridView.Columns[Index];
end;

initialization

cxGridWizardHelperInfoList.Add(TcxGridWizardEMFTableViewHelper, HInstance);

end.
