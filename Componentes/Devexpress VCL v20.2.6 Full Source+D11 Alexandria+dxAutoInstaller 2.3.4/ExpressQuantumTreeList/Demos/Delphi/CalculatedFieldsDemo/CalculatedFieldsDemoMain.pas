unit CalculatedFieldsDemoMain;

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
  cxInplaceContainer, cxDBTL, cxTLData, dxmdaset, dxLayoutContainer, dxLayoutControl, cxCurrencyEdit, dxGallery,
  dxGalleryControl, dxLayoutControlAdapters, cxButtons, dxLayoutLookAndFeels;

type
  TfrmMain = class(TDemoBasicMainForm)
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
    IntegerField1: TIntegerField;
    mdOrderParentOrderID: TIntegerField;
    IntegerField2: TIntegerField;
    FloatField1: TFloatField;
    IntegerField3: TIntegerField;
    FloatField2: TFloatField;
    DateTimeField1: TDateTimeField;
    mdOrderProductName: TStringField;
    dsOrder: TDataSource;
    cxStyleRepository1: TcxStyleRepository;
    stGroup: TcxStyle;
    cxGroupBox1: TcxGroupBox;
    TreeList: TcxDBTreeList;
    tlcParentOrderID: TcxDBTreeListColumn;
    tlcOrderID: TcxDBTreeListColumn;
    tlcProductName: TcxDBTreeListColumn;
    tlcUnitPrice: TcxDBTreeListColumn;
    tlcQuantity: TcxDBTreeListColumn;
    tlcDiscount: TcxDBTreeListColumn;
    tlcAmountDiscount: TcxDBTreeListColumn;
    tlcTotal: TcxDBTreeListColumn;
    cxGroupBox2: TcxGroupBox;
    btnShowExpressionEditor: TcxButton;
    galColumns: TdxGalleryControl;
    dxGalleryControl1Group1: TdxGalleryControlGroup;
    dxGalleryControl1Group1Item1: TdxGalleryControlItem;
    dxGalleryControl1Group1Item2: TdxGalleryControlItem;
    cxLabel1: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure mdOrderCalcFields(DataSet: TDataSet);
    procedure btnShowExpressionEditorClick(Sender: TObject);
    procedure TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
    procedure TreeListStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
      ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure TreeListSummary(ASender: TcxCustomTreeList; const Arguments: TcxTreeListSummaryEventArguments;
      var OutArguments: TcxTreeListSummaryEventOutArguments);
    procedure ValidateExpressionColumn(Sender: TcxTreeListColumn;
      ANode: TcxTreeListNode; const AValue: Variant;
      AData: TcxEditValidateInfo);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dxSpreadSheetTypes, dxSpreadSheetUtils, AboutDemoForm;

{$R *.dfm}

type
  TdxGalleryControlOptionsBehaviorAccess = class(TdxGalleryControlOptionsBehavior);

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  cdsProducts.LoadFromFile(APath + 'Products2.cds');
  mdOrder.LoadFromBinaryFile(APath + 'Order2TL.dat');
  TdxGalleryControlOptionsBehaviorAccess(galColumns.OptionsBehavior).ItemHotTrack := False;
  TreeList.FullExpand;
end;

procedure TfrmMain.mdOrderCalcFields(DataSet: TDataSet);
begin
  if cdsProducts.FindKey([mdOrder.FieldByName('ProductID').AsInteger]) then
    mdOrder.FieldByName('ProductName').AsString := cdsProducts.FieldByName('ProductName').AsString
  else
    mdOrder.FieldByName('ProductName').AsString := Format('Order #%d', [mdOrder.FieldByName('OrderID').AsInteger]);
end;

procedure TfrmMain.TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  Allow := Allow and not TreeList.FocusedNode.IsGroupNode;
end;

procedure TfrmMain.TreeListStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode; var AStyle: TcxStyle);
begin
  if ANode.IsGroupNode then
    AStyle := stGroup
end;

procedure TfrmMain.TreeListSummary(ASender: TcxCustomTreeList; const Arguments: TcxTreeListSummaryEventArguments;
  var OutArguments: TcxTreeListSummaryEventOutArguments);
begin
  if Arguments.Node.IsGroupNode then
    OutArguments.Done := True;
end;

procedure TfrmMain.ValidateExpressionColumn(Sender: TcxTreeListColumn;
  ANode: TcxTreeListNode; const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorCode: Integer;
begin
  AErrorCode := ANode.ErrorCodes[Sender.ItemIndex];
  if AErrorCode > 0 then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := dxSpreadSheetErrorCodeToString(TdxSpreadSheetFormulaErrorCode(AErrorCode));
  end;
end;

procedure TfrmMain.btnShowExpressionEditorClick(Sender: TObject);
begin
  if galColumns.Gallery.Groups[0].Items[0].Checked then
    tlcTotal.ShowExpressionEditor
  else
    tlcAmountDiscount.ShowExpressionEditor;
end;

end.

