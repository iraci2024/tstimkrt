unit FilterDropDownDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Menus, DB, StdCtrls, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxClasses, cxControls, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, DemoBasicMain, MidasLib, Dialogs, ImgList, cxTL,
  cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog,
  cxInplaceContainer, cxDBTL, cxTLData, dxmdaset, cxFilterControl, cxButtons,
  ExtCtrls, cxCalendar, cxCurrencyEdit, cxMemo, CarsData, cxTimeEdit, cxDBLookupComboBox;

type
  TfrmMain = class(TDemoBasicMainForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    TreeList: TcxDBTreeList;
    TreeListName: TcxDBTreeListColumn;
    TreeListModification: TcxDBTreeListColumn;
    TreeListPrice: TcxDBTreeListColumn;
    TreeListMPGCity: TcxDBTreeListColumn;
    TreeListMPGHighway: TcxDBTreeListColumn;
    TreeListCilinders: TcxDBTreeListColumn;
    TreeListSalesDate: TcxDBTreeListColumn;
    TreeListBodyStyle: TcxDBTreeListColumn;
    cxStyleRepository1: TcxStyleRepository;
    stMaroon: TcxStyle;
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
    miMainSeparator: TMenuItem;
    miMainSeparator2: TMenuItem;
    miCriteriaDisplayStyle: TMenuItem;
    miCriteriaDisplayStyleText: TMenuItem;
    miCriteriaDisplayStyleTokens: TMenuItem;
    acCriteriaDisplayStyleText: TAction;
    acCriteriaDisplayStyleTokens: TAction;
    procedure FormCreate(Sender: TObject);
    procedure TreeListStylesGetContentStyle(Sender: TcxCustomTreeList;
      AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure acFilterPopupModeExecute(Sender: TObject);
    procedure acExcelModeApplyChangesExecute(Sender: TObject);
    procedure acDateTimePageTypeExecute(Sender: TObject);
    procedure acNumericPageTypeExecute(Sender: TObject);
    procedure acClassicModeApplyChangesExecute(Sender: TObject);
    procedure acClassicModeMultiSelectExecute(Sender: TObject);
    procedure acDoNothingExecute(Sender: TObject);
    procedure acCriteriaDisplayStyleExecute(Sender: TObject);
  protected
    procedure UpdateFilterPopupActions;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFilterControlStrs, Variants, AboutDemoForm, dxFilterValueContainer, dxFilterPopupWindow;

{$R *.dfm}

procedure TfrmMain.acClassicModeApplyChangesExecute(Sender: TObject);
begin
  if acClassicModeApplyChangesImmediately.Checked then
    TreeList.Filtering.ColumnPopup.ApplyMultiSelectChanges := fpacImmediately
  else
    TreeList.Filtering.ColumnPopup.ApplyMultiSelectChanges := fpacOnButtonClick;
end;

procedure TfrmMain.acClassicModeMultiSelectExecute(Sender: TObject);
begin
  TreeList.Filtering.ColumnPopup.MultiSelect := acClassicModeMultiSelect.Checked;
end;

procedure TfrmMain.acCriteriaDisplayStyleExecute(Sender: TObject);
begin
  if acCriteriaDisplayStyleTokens.Checked then
    TreeList.FilterBox.CriteriaDisplayStyle := fcdsTokens
  else
    TreeList.FilterBox.CriteriaDisplayStyle := fcdsText;
end;

procedure TfrmMain.acDateTimePageTypeExecute(Sender: TObject);
begin
  if acExcelModeDateTimePageTypeTree.Checked then
    TreeList.Filtering.ColumnExcelPopup.DateTimeValuesPageType := dvptTree
  else
    TreeList.Filtering.ColumnExcelPopup.DateTimeValuesPageType := dvptList;
end;

procedure TfrmMain.acExcelModeApplyChangesExecute(Sender: TObject);
begin
  if acExcelModeApplyChangesImmediately.Checked then
    TreeList.Filtering.ColumnExcelPopup.ApplyChanges := efacImmediately
  else
    TreeList.Filtering.ColumnExcelPopup.ApplyChanges := efacOnTabOrOKButtonClick;
end;

procedure TfrmMain.acDoNothingExecute(Sender: TObject);
begin
//do nothing
end;

procedure TfrmMain.acFilterPopupModeExecute(Sender: TObject);
begin
  if acFilterPopupModeClassic.Checked then
    TreeList.Filtering.ColumnPopupMode := fpmClassic
  else
    TreeList.Filtering.ColumnPopupMode := fpmExcel;
  UpdateFilterPopupActions;
end;

procedure TfrmMain.acNumericPageTypeExecute(Sender: TObject);
begin
  if acExcelModeNumericPageTypeRange.Checked then
    TreeList.Filtering.ColumnExcelPopup.NumericValuesPageType := nvptRange
  else
    TreeList.Filtering.ColumnExcelPopup.NumericValuesPageType := nvptList;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  AEditValue: Variant;
  ADisplayText: string;
  AList: TcxFilterCriteriaItemList;
begin
  inherited;
  TreeList.DataController.Filter.Active := False;
  AEditValue := 46000;
  ADisplayText := TreeListPrice.Properties.GetDisplayText(AEditValue);
  TreeList.DataController.Filter.AddItem(nil, TreeListPrice, foGreaterEqual, AEditValue, ADisplayText);
  AEditValue := 55000;
  ADisplayText := TreeListPrice.Properties.GetDisplayText(AEditValue);
  TreeList.DataController.Filter.AddItem(nil, TreeListPrice, foLessEqual, AEditValue, ADisplayText);
  AList := TreeList.DataController.Filter.Root.AddItemList(fboOr);
  TreeList.DataController.Filter.AddItem(AList, TreeListSalesDate, foLastYear, Null, '');
  TreeList.DataController.Filter.AddItem(AList, TreeListSalesDate, foThisYear, Null, '');
  TreeList.DataController.Filter.Active := True;
  TreeList.FullExpand;
end;

procedure TfrmMain.TreeListStylesGetContentStyle(Sender: TcxCustomTreeList;
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
begin
  if (ANode <> nil) and (ANode.Level = 0) then
    AStyle := stMaroon;
end;

procedure TfrmMain.UpdateFilterPopupActions;
var
  AModeName: string;
begin
  acExcelModeDateTimePageType.Visible := TreeList.Filtering.ColumnPopupMode = fpmExcel;
  acExcelModeNumericPageType.Visible := TreeList.Filtering.ColumnPopupMode = fpmExcel;
  acExcelModeApplyChanges.Visible := TreeList.Filtering.ColumnPopupMode = fpmExcel;
  acClassicModeApplyChanges.Visible := TreeList.Filtering.ColumnPopupMode = fpmClassic;
  acClassicModeMultiSelect.Visible := TreeList.Filtering.ColumnPopupMode = fpmClassic;
  if TreeList.Filtering.ColumnPopupMode = fpmExcel then
    AModeName := 'Excel'
  else
    AModeName := 'Classic';
  acFilterPopupMode.Caption := 'Mode ' + '(' + AModeName + ')';
end;

end.
