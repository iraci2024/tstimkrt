unit FilterDropDownDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Menus, DB, StdCtrls, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxClasses, cxControls,
  cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  XPMan, DemoBasicMain, MidasLib,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridCustomView,
  cxGrid, ImgList, cxVGrid, cxDBVGrid, cxInplaceContainer, dxmdaset,
  dxFilterValueContainer, dxFilterPopupWindow,
  cxCurrencyEdit;

type
  TfrmMain = class(TDemoBasicMainForm)
    VerticalGrid: TcxDBVerticalGrid;
    dsBodyStyle: TDataSource;
    mdBodyStyle: TdxMemData;
    mdBodyStyleID: TIntegerField;
    mdBodyStyleName: TWideStringField;
    mdCarOrders: TdxMemData;
    mdCarOrdersID: TIntegerField;
    mdCarOrdersTrademark: TStringField;
    mdCarOrdersName: TWideStringField;
    mdCarOrdersModification: TWideStringField;
    mdCarOrdersPrice: TBCDField;
    mdCarOrdersMPG_City: TIntegerField;
    mdCarOrdersMPG_Highway: TIntegerField;
    mdCarOrdersBodyStyleID: TIntegerField;
    mdCarOrdersCilinders: TIntegerField;
    mdCarOrdersSalesDate: TDateField;
    mdCarOrdersBodyStyle: TStringField;
    dsCarOrders: TDataSource;
    VerticalGridRecId: TcxDBEditorRow;
    VerticalGridID: TcxDBEditorRow;
    VerticalGridTrademark: TcxDBEditorRow;
    VerticalGridName: TcxDBEditorRow;
    VerticalGridModification: TcxDBEditorRow;
    VerticalGridPrice: TcxDBEditorRow;
    VerticalGridMPGCity: TcxDBEditorRow;
    VerticalGridMPGHighway: TcxDBEditorRow;
    VerticalGridBodyStyleID: TcxDBEditorRow;
    VerticalGridCilinders: TcxDBEditorRow;
    VerticalGridSalesDate: TcxDBEditorRow;
    VerticalGridBodyStyle: TcxDBEditorRow;
    acExcelModeApplyChangesImmediately: TAction;
    acExcelModeApplyChangesOnTabOrOKButtonClick: TAction;
    acExcelModeDateTimePageTypeTree: TAction;
    acExcelModeDateTimePageTypeList: TAction;
    acExcelModeNumericPageTypeRange: TAction;
    acExcelModeNumericPageTypeList: TAction;
    acClassicModeApplyChangesImmediately: TAction;
    acClassicModeApplyChangesOnButtonClick: TAction;
    miClassicModeMultiSelect: TMenuItem;
    acClassicModeMultiSelect: TAction;
    acExcelModeDateTimePageType: TAction;
    acExcelModeApplyChanges: TAction;
    acExcelModeNumericPageType: TAction;
    acClassicModeApplyChanges: TAction;
    acFilterPopupMode: TAction;
    miFilterPopup: TMenuItem;
    miFilterPopupMode: TMenuItem;
    miFilterPopupModeClassic: TMenuItem;
    miFilterPopupModeExcel: TMenuItem;
    acFilterPopupModeClassic: TAction;
    acFilterPopupModeExcel: TAction;
    miExcelModeApplyChanges: TMenuItem;
    miExcelModeDateTimePage: TMenuItem;
    miExcelModeNumericPage: TMenuItem;
    miExcelModeApplyChangesImmediatly: TMenuItem;
    miExcelModeTabOrOKButtonClick: TMenuItem;
    miDateTimePageTree: TMenuItem;
    miDateTimePageList: TMenuItem;
    miNumericPageTree: TMenuItem;
    miNumericPageList: TMenuItem;
    miClassicModeApplyChanges: TMenuItem;
    miClassicModeApplyChangesImmediatly: TMenuItem;
    miClassicModeApplyChangesButtonClick: TMenuItem;
    miMainSeparator: TMenuItem;
    miMainSeparator2: TMenuItem;
    miCriteriaDisplayStyle: TMenuItem;
    miCriteriaDisplayStyleText: TMenuItem;
    miCriteriaDisplayStyleTokens: TMenuItem;
    acCriteriaDisplayStyleText: TAction;
    acCriteriaDisplayStyleTokens: TAction;
    procedure acFilterPopupModeExecute(Sender: TObject);
    procedure acExcelModeApplyChangesExecute(Sender: TObject);
    procedure acDateTimePageTypeExecute(Sender: TObject);
    procedure acNumericPageTypeExecute(Sender: TObject);
    procedure acClassicModeApplyChangesExecute(Sender: TObject);
    procedure acClassicModeMultiSelectExecute(Sender: TObject);
    procedure acDoNothingExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acCriteriaDisplayStyleExecute(Sender: TObject);
  protected
    procedure UpdateFilterPopupActions;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFindPanel, Variants;

{$R *.dfm}

procedure TfrmMain.acClassicModeApplyChangesExecute(Sender: TObject);
begin
  if acClassicModeApplyChangesImmediately.Checked then
    VerticalGrid.Filtering.RowPopup.ApplyMultiSelectChanges := fpacImmediately
  else
    VerticalGrid.Filtering.RowPopup.ApplyMultiSelectChanges := fpacOnButtonClick;
end;

procedure TfrmMain.acClassicModeMultiSelectExecute(Sender: TObject);
begin
  VerticalGrid.Filtering.RowPopup.MultiSelect := acClassicModeMultiSelect.Checked;
end;

procedure TfrmMain.acCriteriaDisplayStyleExecute(Sender: TObject);
begin
  if acCriteriaDisplayStyleTokens.Checked then
    VerticalGrid.FilterBox.CriteriaDisplayStyle := fcdsTokens
  else
    VerticalGrid.FilterBox.CriteriaDisplayStyle := fcdsText;
end;

procedure TfrmMain.acDateTimePageTypeExecute(Sender: TObject);
begin
  if acExcelModeDateTimePageTypeTree.Checked then
    VerticalGrid.Filtering.RowExcelPopup.DateTimeValuesPageType := dvptTree
  else
    VerticalGrid.Filtering.RowExcelPopup.DateTimeValuesPageType := dvptList;
end;

procedure TfrmMain.acDoNothingExecute(Sender: TObject);
begin
//do nothing
end;

procedure TfrmMain.acExcelModeApplyChangesExecute(Sender: TObject);
begin
  if acExcelModeApplyChangesImmediately.Checked then
    VerticalGrid.Filtering.RowExcelPopup.ApplyChanges := efacImmediately
  else
    VerticalGrid.Filtering.RowExcelPopup.ApplyChanges := efacOnTabOrOKButtonClick;
end;

procedure TfrmMain.acFilterPopupModeExecute(Sender: TObject);
begin
  if acFilterPopupModeClassic.Checked then
    VerticalGrid.Filtering.RowPopupMode := fpmClassic
  else
    VerticalGrid.Filtering.RowPopupMode := fpmExcel;
  UpdateFilterPopupActions;
end;

procedure TfrmMain.acNumericPageTypeExecute(Sender: TObject);
begin
  if acExcelModeNumericPageTypeRange.Checked then
    VerticalGrid.Filtering.RowExcelPopup.NumericValuesPageType := nvptRange
  else
    VerticalGrid.Filtering.RowExcelPopup.NumericValuesPageType := nvptList;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ANow: TDate;
  APath: string;
  AEditValue: Variant;
  ADisplayText: string;
  AList: TcxFilterCriteriaItemList;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdBodyStyle.LoadFromBinaryFile(APath + 'CarsBodyStyle.dat');
  mdCarOrders.LoadFromBinaryFile(APath + 'CarOrders.dat');
  ANow := Now;
  mdCarOrders.DisableControls;
  try
    mdCarOrders.First;
    while not mdCarOrders.Eof do
    begin
      mdCarOrders.Edit;
      mdCarOrdersSalesDate.Value := ANow - Random(1500);
      mdCarOrders.Post;
      mdCarOrders.Next;
    end;
  finally
    mdCarOrders.EnableControls;
  end;
  VerticalGrid.DataController.Filter.Active := False;
  AEditValue := 46000;
  ADisplayText := VerticalGridPrice.Properties.EditProperties.GetDisplayText(AEditValue);
  VerticalGrid.DataController.Filter.AddItem(nil, VerticalGridPrice.Properties.ItemLink, foGreaterEqual, AEditValue, ADisplayText);
  AEditValue := 55000;
  ADisplayText := VerticalGridPrice.Properties.EditProperties.GetDisplayText(AEditValue);
  VerticalGrid.DataController.Filter.AddItem(nil, VerticalGridPrice.Properties.ItemLink, foLessEqual, AEditValue, ADisplayText);
  AList := VerticalGrid.DataController.Filter.Root.AddItemList(fboOr);
  VerticalGrid.DataController.Filter.AddItem(AList, VerticalGridSalesDate.Properties.ItemLink, foLastYear, Null, '');
  VerticalGrid.DataController.Filter.AddItem(AList, VerticalGridSalesDate.Properties.ItemLink, foThisYear, Null, '');
  VerticalGrid.DataController.Filter.Active := True;
end;

procedure TfrmMain.UpdateFilterPopupActions;
var
  AModeName: string;
begin
  acExcelModeDateTimePageType.Visible := VerticalGrid.Filtering.RowPopupMode = fpmExcel;
  acExcelModeNumericPageType.Visible := VerticalGrid.Filtering.RowPopupMode = fpmExcel;
  acExcelModeApplyChanges.Visible := VerticalGrid.Filtering.RowPopupMode = fpmExcel;
  acClassicModeApplyChanges.Visible := VerticalGrid.Filtering.RowPopupMode = fpmClassic;
  acClassicModeMultiSelect.Visible := VerticalGrid.Filtering.RowPopupMode = fpmClassic;
  if VerticalGrid.Filtering.RowPopupMode = fpmExcel then
    AModeName := 'Excel'
  else
    AModeName := 'Classic';
  acFilterPopupMode.Caption := 'Mode ' + '(' + AModeName + ')';
end;

end.
