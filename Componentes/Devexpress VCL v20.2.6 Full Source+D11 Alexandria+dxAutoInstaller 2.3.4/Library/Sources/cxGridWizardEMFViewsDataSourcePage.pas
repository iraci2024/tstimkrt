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

unit cxGridWizardEMFViewsDataSourcePage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls,
  dxCore, cxGraphics, cxControls, cxClasses, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxCheckBox,
  cxDropDownEdit, cxLabel, dxLayoutControl, dxLayoutContainer,
  dxLayoutcxEditAdapters,
  dxLayoutLookAndFeels, cxGridCustomView, cxGridLevel, cxGridEMFDataDefinitions,
  cxGridWizardCustomPage,
  cxGridWizardStrs, cxTextEdit, cxMaskEdit, dxLayoutControlAdapters,
  cxRadioGroup;

type
  { TcxGridWizardEMFViewsDataSourcePageFrame }

  TcxGridWizardEMFViewsDataSourcePageFrame = class(TcxGridWizardCustomPageFrame)
    cbMasterView: TcxComboBox;
    chbDetail: TcxCheckBox;
    lbCommon: TcxLabel;
    lgDetailSeparator: TdxLayoutGroup;
    lgCommonSeparator: TdxLayoutGroup;
    lsiDetail: TdxLayoutSeparatorItem;
    lsiCommon: TdxLayoutSeparatorItem;
    lgCommonCaption: TdxLayoutGroup;
    lgDetail: TdxLayoutGroup;
    lgIsDetail: TdxLayoutGroup;
    liCommon: TdxLayoutItem;
    liDetail: TdxLayoutItem;
    licbMasterView: TdxLayoutItem;
    lesiDetailSettings: TdxLayoutEmptySpaceItem;
    lgDetailSettings: TdxLayoutGroup;
    lgCommonSettings: TdxLayoutGroup;
    cbDataSource: TcxComboBox;
    licbDataSource: TdxLayoutItem;
    lgKeyFields: TdxLayoutGroup;
    cbCollection: TcxComboBox;
    licbCollection: TdxLayoutItem;
    lgModeSettings: TdxLayoutGroup;
    lgQueryMode: TdxLayoutGroup;
    lgInMemoryMode: TdxLayoutGroup;
    rbQueryMode: TcxRadioButton;
    dxLayoutItem1: TdxLayoutItem;
    rbInMemoryMode: TcxRadioButton;
    dxLayoutItem2: TdxLayoutItem;
    cbMasterKeyFields: TcxComboBox;
    licbMasterKeyFields: TdxLayoutItem;
    cbDetailKeyFields: TcxComboBox;
    licbDetailKeyFields: TdxLayoutItem;
    procedure cbCollectionPropertiesEditValueChanged(Sender: TObject);
    procedure cbDataSourcePropertiesEditValueChanged(Sender: TObject);
    procedure cbDetailKeyFieldsPropertiesEditValueChanged(Sender: TObject);
    procedure cbMasterKeyFieldsPropertiesEditValueChanged(Sender: TObject);
    procedure cbMasterViewPropertiesEditValueChanged(Sender: TObject);
    procedure chbDetailPropertiesChange(Sender: TObject);
    procedure rbQueryModeClick(Sender: TObject);
  private
    function GetDataController: TcxGridEMFDataController;
  protected
    function GetCanJumpToNextPage: Boolean; override;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;

    procedure LoadPageContent;
    procedure PopulateMasterViews;

    property DataController: TcxGridEMFDataController read GetDataController;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

uses
  cxGridWizardCustomHelper, cxGridWizardEMFTableViewHelper, cxEMFData,
  cxGridEMFTableView, dxEMF.Metadata, Math;

{ TcxGridWizardEMFViewsDataSourcePageFrame }

procedure TcxGridWizardEMFViewsDataSourcePageFrame.ApplyLocalization;
begin
  lbCommon.Caption := cxGetResourceString(@scxgwCommonGroupCaptionCommon);

  licbDataSource.Caption := cxGetResourceString(@scxgwCommonDataSource);
  licbDataSource.CaptionOptions.Hint :=
    cxGetResourceString(@scxgwCommonDataSourceHint);

  chbDetail.Caption := cxGetResourceString(@scxgwDataSourcePageIsDetailView);

  licbMasterView.Caption := cxGetResourceString(@scxgwDataSourcePageMasterView);
  licbMasterView.CaptionOptions.Hint :=
    cxGetResourceString(@scxgwDataSourcePageMasterViewHint);

  licbMasterKeyFields.Caption := cxGetResourceString
    (@scxgwDataSourcePageMasterViewKeyFieldNames);
  licbMasterKeyFields.CaptionOptions.Hint :=
    cxGetResourceString(@scxgwEMFDataSourcePageMasterViewKeyFieldNamesHint);

  licbDetailKeyFields.Caption := cxGetResourceString
    (@scxgwDataSourcePageDetailKeyFieldNames);
  licbDetailKeyFields.CaptionOptions.Hint :=
    cxGetResourceString(@scxgwEMFDataSourcePageDetailKeyFieldNamesHint);

  rbInMemoryMode.Caption := cxGetResourceString
    (@scxgwEMFDataSourcePageInMemoryModeName);

end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.ApplySettings;
begin
  SetIsDetail(chbDetail.Checked);
  SetMasterGridView(cbMasterView.ItemObject as TcxCustomGridView);
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.LoadSettings;
begin
  LoadPageContent;

  cbDataSource.ItemIndex :=
    Max(0, cbDataSource.Properties.Items.IndexOfObject
    (DataController.DataSource));
  cbDataSource.ValidateEdit;
  if IsMultiLevelStructure and IsDetailViewCustomizing then
    cbMasterView.ItemIndex := cbMasterView.Properties.Items.IndexOf
      (MultiLevelStructureMasterViewName);
  cbMasterKeyFields.Text := DataController.MasterKeyFieldNames;
  cbDetailKeyFields.Text := DataController.DetailKeyFieldNames;
end;

function TcxGridWizardEMFViewsDataSourcePageFrame.GetCanJumpToNextPage: Boolean;
begin
  Result := (cbDataSource.Text <> '') and
    not(chbDetail.Checked and (cbMasterView.Text = ''));
end;

function TcxGridWizardEMFViewsDataSourcePageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwEMFDataSourcePageDescription);
end;

function TcxGridWizardEMFViewsDataSourcePageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwDataSourcePageTitle);
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.LoadPageContent;

  function CheckDataSource(AComponent: TComponent): Boolean;
  begin
    Result := AComponent is TdxEMFDataControllerCustomDataSource;
  end;

  procedure CheckMultiLevelStructure;
  begin
    if SelectedGridView <> nil then
      chbDetail.Checked := SelectedGridView.IsDetail
    else
    begin
      lesiDetailSettings.Visible := IsMultiLevelStructure and
        IsDetailViewCustomizing;
      lgDetailSettings.Visible := lesiDetailSettings.Visible;
      chbDetail.Checked := lesiDetailSettings.Visible;
    end;
  end;

var
  I: Integer;
begin
  if Tag = 0 then
  begin
    Helper.PopulateComponents(TdxEMFDataControllerCustomDataSource,
      @CheckDataSource, cbDataSource.Properties.Items);
    cbDataSource.Enabled := cbDataSource.Properties.Items.Count > 0;
    for I := 0 to cbDataSource.Properties.Items.Count - 1 do
      cbDataSource.Width := Min(Width - 50, Max(cbDataSource.Width,
        cbDataSource.Canvas.TextWidth(cbDataSource.Properties.Items[I]) + 30));
    CheckMultiLevelStructure;
    PopulateMasterViews;
    Tag := 1;
  end;
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.PopulateMasterViews;

  function CanBeMasterView(AView: TcxCustomGridView): Boolean;
  var
    AHelperClass: TcxGridWizardCustomHelperClass;
  begin
    AHelperClass := cxGridWizardHelperInfoList.GetHelperClassBy
      (AView.ClassType);
    Result := (AHelperClass <> nil) and AHelperClass.CanBeMasterView and
      (AView <> SelectedGridView) and (AView is TcxGridEMFTableView);
    if Result and (SelectedGridView <> nil) then
      Result := not AView.HasAsMaster(SelectedGridView);
  end;

var
  ACustomizedGridView: TcxCustomGridView;
  AMasterGridView: TcxCustomGridView;
  I: Integer;
begin
  AMasterGridView := nil;
  cbMasterView.Properties.Items.BeginUpdate;
  try
    cbMasterView.Properties.Items.Clear;
    for I := 0 to CustomizedGrid.ViewCount - 1 do
    begin
      ACustomizedGridView := CustomizedGrid.Views[I];
      if CanBeMasterView(ACustomizedGridView) then
        cbMasterView.Properties.Items.AddObject(ACustomizedGridView.Name,
          CustomizedGrid.Views[I]);
      if (SelectedGridView <> nil) and SelectedGridView.HasAsMaster
        (ACustomizedGridView) then
        AMasterGridView := ACustomizedGridView;
    end;
    if AMasterGridView <> nil then
      cbMasterView.ItemIndex := cbMasterView.Properties.Items.IndexOfObject
        (AMasterGridView);
  finally
    cbMasterView.Properties.Items.EndUpdate;
  end;
end;

function TcxGridWizardEMFViewsDataSourcePageFrame.GetDataController
  : TcxGridEMFDataController;
begin
  Result := (Helper as TcxGridWizardEMFTableViewHelper).DataController;
end;

{ Events }

procedure TcxGridWizardEMFViewsDataSourcePageFrame.
  cbCollectionPropertiesEditValueChanged(Sender: TObject);
begin
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.
  cbDataSourcePropertiesEditValueChanged(Sender: TObject);
begin
  DataController.DataSource :=
    cbDataSource.ItemObject as TdxEMFDataControllerCustomDataSource;
  cbDataSource.ValidateEdit;
  cbMasterViewPropertiesEditValueChanged(cbMasterView);
  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.
  cbDetailKeyFieldsPropertiesEditValueChanged(Sender: TObject);
begin
  DataController.DetailKeyFieldNames := cbDetailKeyFields.Text;
  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.
  cbMasterKeyFieldsPropertiesEditValueChanged(Sender: TObject);
begin
  DataController.MasterKeyFieldNames := cbMasterKeyFields.Text;
  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.
  cbMasterViewPropertiesEditValueChanged(Sender: TObject);

  procedure PopulateKeyFields(ATargetComboBox: TcxComboBox;
    ADataController: TdxEMFDataController);
  var
    I: Integer;
  begin
    ATargetComboBox.Properties.Items.BeginUpdate;
    try
      ATargetComboBox.Properties.Items.Clear;
      if (ADataController.DataSource <> nil) and
        (ADataController.DataSource.EntityInfo <> nil) then
        for I := 0 to ADataController.DataSource.EntityInfo.
          PersistentProperties.Count - 1 do
          ATargetComboBox.Properties.Items.Add
            (ADataController.DataSource.EntityInfo.PersistentProperties[I]
            .MemberName);
    finally
      ATargetComboBox.Properties.Items.EndUpdate;
    end;
  end;

var
  AMasterGridViewDataController: TcxGridEMFDataController;
begin
  if cbMasterView.ItemObject = nil then
    Exit;

  AMasterGridViewDataController :=
    (cbMasterView.ItemObject as TcxGridEMFTableView).DataController;

  PopulateKeyFields(cbMasterKeyFields, AMasterGridViewDataController);
  cbMasterKeyFields.Text := DataController.MasterKeyFieldNames;
  if cbMasterKeyFields.Text = '' then
    cbMasterKeyFields.Text := AMasterGridViewDataController.DetailKeyFieldNames;

  PopulateKeyFields(cbDetailKeyFields, DataController);
  cbDetailKeyFields.Text := DataController.DetailKeyFieldNames;

  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.chbDetailPropertiesChange
  (Sender: TObject);
begin
  lgDetail.Enabled := chbDetail.Checked;
  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardEMFViewsDataSourcePageFrame.rbQueryModeClick
  (Sender: TObject);
begin
end;

end.
