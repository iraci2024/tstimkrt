unit CalculatedFieldsDemoMain;

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
  cxGrid, ImgList, cxVGrid, cxDBVGrid, cxInplaceContainer, dxmdaset, dxGallery, dxGalleryControl, cxButtons, cxCalendar,
  cxCurrencyEdit, cxImageList;

type
  TfrmMain = class(TDemoBasicMainForm)
    cxGroupBox1: TcxGroupBox;
    cxGroupBox2: TcxGroupBox;
    btnShowExpressionEditor: TcxButton;
    galColumns: TdxGalleryControl;
    dxGalleryControl1Group1: TdxGalleryControlGroup;
    dxGalleryControl1Group1Item1: TdxGalleryControlItem;
    dxGalleryControl1Group1Item2: TdxGalleryControlItem;
    VerticalGrid: TcxDBVerticalGrid;
    vgrCategoryRow1: TcxCategoryRow;
    vgrOrderID: TcxDBEditorRow;
    vgrOrderDate: TcxDBEditorRow;
    vgrCategoryRow2: TcxCategoryRow;
    vgrProductName: TcxDBEditorRow;
    vgrUnitPrice: TcxDBEditorRow;
    vgrQuantity: TcxDBEditorRow;
    vgrDiscount: TcxDBEditorRow;
    vgrCategoryRow3: TcxCategoryRow;
    vgrDiscountAmount: TcxDBEditorRow;
    vgrTotal: TcxDBEditorRow;
    cdsProducts2: TClientDataSet;
    cdsProducts2ProductID: TIntegerField;
    cdsProducts2ProductName: TStringField;
    cdsProducts2SupplierID: TIntegerField;
    cdsProducts2CategoryID: TIntegerField;
    cdsProducts2QuantityPerUnit: TStringField;
    cdsProducts2UnitPrice: TFloatField;
    cdsProducts2UnitsInStock: TIntegerField;
    cdsProducts2UnitsOnOrder: TIntegerField;
    cdsProducts2ReorderLevel: TIntegerField;
    cdsProducts2Discontinued: TBooleanField;
    cdsProducts2EAN13: TStringField;
    mdOrder2: TdxMemData;
    mdOrder2OrderID: TIntegerField;
    mdOrder2ProductID: TIntegerField;
    mdOrder2UnitPrice: TFloatField;
    mdOrder2Quantity: TIntegerField;
    mdOrder2Discount: TFloatField;
    mdOrder2OrderDate: TDateTimeField;
    mdOrder2ProductName: TStringField;
    dsOrder2: TDataSource;
    cxImageList1: TcxImageList;
    cxLabel1: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnShowExpressionEditorClick(Sender: TObject);
    procedure ValidateExpressionCell(Sender: TcxCustomEditorRowProperties;
      ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  dxSpreadSheetTypes, dxSpreadSheetUtils;

type
  TdxGalleryControlOptionsBehaviorAccess = class(TdxGalleryControlOptionsBehavior);

procedure TfrmMain.btnShowExpressionEditorClick(Sender: TObject);
begin
  if galColumns.Gallery.Groups[0].Items[0].Checked then
    vgrTotal.Properties.ShowExpressionEditor
  else
    vgrDiscountAmount.Properties.ShowExpressionEditor;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  cdsProducts2.LoadFromFile(APath + 'Products2.cds');
  mdOrder2.LoadFromBinaryFile(APath + 'Order2.dat');

  TdxGalleryControlOptionsBehaviorAccess(galColumns.OptionsBehavior).ItemHotTrack := False;
end;

procedure TfrmMain.ValidateExpressionCell(Sender: TcxCustomEditorRowProperties;
  ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorCode: Integer;
begin
  AErrorCode := VerticalGrid.DataController.ErrorCodes[ARecordIndex, Sender.ItemIndex];
  if AErrorCode > 0 then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := dxSpreadSheetErrorCodeToString(TdxSpreadSheetFormulaErrorCode(AErrorCode));
  end;
end;

end.
