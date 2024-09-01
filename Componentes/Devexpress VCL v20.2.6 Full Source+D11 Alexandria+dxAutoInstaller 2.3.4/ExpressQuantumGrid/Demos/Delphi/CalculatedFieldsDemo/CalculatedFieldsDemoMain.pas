unit CalculatedFieldsDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, DB, StdCtrls, dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, cxDBData, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGrid, cxDBLookupComboBox, cxEditRepositoryItems, ExtCtrls, BaseForm,
  cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, dxmdaset,
  DBClient, MidasLib, cxNavigator, dxDateRanges, cxGridCardView, cxContainer, dxGallery, dxGalleryControl, cxButtons,
  cxGroupBox, cxCurrencyEdit, cxSpinEdit, ImgList, cxImageList, cxLabel,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxBarBuiltInMenu, cxGridCustomPopupMenu, cxGridPopupMenu;

type
  TfrmMain = class(TfmBaseForm)
    cxGroupBox1: TcxGroupBox;
    cxGroupBox2: TcxGroupBox;
    btnShowExpressionEditor: TcxButton;
    galColumns: TdxGalleryControl;
    dxGalleryControl1Group1: TdxGalleryControlGroup;
    dxGalleryControl1Group1Item1: TdxGalleryControlItem;
    dxGalleryControl1Group1Item2: TdxGalleryControlItem;
    Grid: TcxGrid;
    tvOrders: TcxGridDBTableView;
    tvOrdersOrderID: TcxGridDBColumn;
    tvOrdersProductName: TcxGridDBColumn;
    tvOrdersUnitPrice: TcxGridDBColumn;
    tvOrdersQuantity: TcxGridDBColumn;
    tvOrdersDiscount: TcxGridDBColumn;
    tvOrdersDiscountAmount: TcxGridDBColumn;
    tvOrdersTotal: TcxGridDBColumn;
    GridLevel1: TcxGridLevel;
    cdsProducts: TClientDataSet;
    cdsProductsProductID: TIntegerField;
    cdsProductsProductName: TStringField;
    cdsProductsSupplierID: TIntegerField;
    cdsProductsCategoryID: TIntegerField;
    cdsProductsQuantityPerUnit: TStringField;
    cdsProductsUnitPrice: TFloatField;
    cdsProductsUnitsInStock: TIntegerField;
    cdsProductsUnitsOnOrder: TIntegerField;
    cdsProductsReorderLevel: TIntegerField;
    cdsProductsDiscontinued: TBooleanField;
    cdsProductsEAN13: TStringField;
    mdOrder: TdxMemData;
    mdOrderOrderID: TIntegerField;
    mdOrderProductID: TIntegerField;
    mdOrderUnitPrice: TFloatField;
    mdOrderQuantity: TIntegerField;
    mdOrderDiscount: TFloatField;
    mdOrderOrderDate: TDateTimeField;
    mdOrderProductName: TStringField;
    dsOrder: TDataSource;
    cxImageList1: TcxImageList;
    cxLabel1: TcxLabel;
    cxGridPopupMenu1: TcxGridPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure btnShowExpressionEditorClick(Sender: TObject);
    procedure ValidateExpressionColumn(
      Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      const AValue: Variant; AData: TcxEditValidateInfo);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dxSpreadSheetTypes, dxSpreadSheetUtils, AboutDemoForm;

{$R *.dfm}

type
  TdxGalleryControlOptionsBehaviorAccess = class(TdxGalleryControlOptionsBehavior);

procedure TfrmMain.btnShowExpressionEditorClick(Sender: TObject);
begin
  if galColumns.Gallery.Groups[0].Items[0].Checked then
    tvOrdersTotal.ShowExpressionEditor
  else
    tvOrdersDiscountAmount.ShowExpressionEditor;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  cdsProducts.LoadFromFile(APath + 'Products2.cds');
  mdOrder.LoadFromBinaryFile(APath + 'Order2.dat');
  tvOrders.DataController.Groups.FullExpand;

  TdxGalleryControlOptionsBehaviorAccess(galColumns.OptionsBehavior).ItemHotTrack := False;
end;

procedure TfrmMain.ValidateExpressionColumn(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorCode: Integer;
begin
  if ARecord.IsData then
  begin
    AErrorCode := Sender.GridView.DataController.ErrorCodes[ARecord.RecordIndex, Sender.Index];
    if AErrorCode > 0 then
    begin
      AData.ErrorType := eetError;
      AData.ErrorText := dxSpreadSheetErrorCodeToString(TdxSpreadSheetFormulaErrorCode(AErrorCode));
    end;
  end;
end;

end.

