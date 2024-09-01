unit CalculatedFilterItemsDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Menus, DB, StdCtrls, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxClasses, cxControls,
  cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  XPMan, DemoBasicMain, MidasLib, CarsData,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridCustomView,
  cxGrid, ImgList, cxVGrid, cxDBVGrid, cxInplaceContainer, dxmdaset,
  dxScrollbarAnnotations;

type
  TfrmMain = class(TDemoBasicMainForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    alAction: TActionList;
    VerticalGrid: TcxDBVerticalGrid;
    VerticalGridRecId: TcxDBEditorRow;
    VerticalGridCarInfoCategory: TcxCategoryRow;
    VerticalGridTrademark: TcxDBEditorRow;
    VerticalGridName: TcxDBEditorRow;
    VerticalGridBodyStyle: TcxDBEditorRow;
    VerticalGridBodyStyleID: TcxDBEditorRow;
    VerticalGridOrderInfoCategory: TcxCategoryRow;
    VerticalGridPrice: TcxDBEditorRow;
    VerticalGridSalesPrice: TcxDBEditorRow;
    VerticalGridSalesDate: TcxDBEditorRow;
    VerticalGridDeliveryCategory: TcxCategoryRow;
    VerticalGridDeliveryFrom: TcxDBEditorRow;
    VerticalGridDeliveryTo: TcxDBEditorRow;
    VerticalGridDeliveryDate: TcxDBEditorRow;
    VerticalGridDeliveryComplete: TcxDBEditorRow;
    cxCheckBox1: TcxCheckBox;
    actExpressionEditing: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actExpressionEditingExecute(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxVariants;

{$R *.dfm}

procedure TfrmMain.actExpressionEditingExecute(Sender: TObject);
begin
  VerticalGrid.Filtering.ExpressionEditing := actExpressionEditing.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  AValue: Variant;
  ADisplayValue: string;
  AExpression: string;
  AItemList: TcxFilterCriteriaItemList;
begin
  AItemList := VerticalGrid.DataController.Filter.Root.AddItemList(fboOr);
  AExpression := '[' + VerticalGridPrice.Properties.Caption + '] * 95%';
  AItemList.AddExpressionItem(VerticalGridSalesPrice.Properties.ItemLink, foLessEqual, AExpression, AExpression);
  AExpression := '[' + VerticalGridPrice.Properties.Caption + '] * 105%';
  AItemList.AddExpressionItem(VerticalGridSalesPrice.Properties.ItemLink, foGreaterEqual, AExpression, AExpression);
  AValue := VarListArrayCreate('New York');
  ADisplayValue := 'New York';
  VarListArrayAddValue(AValue, 'Boston');
  ADisplayValue := ADisplayValue + ';Boston';
  VarListArrayAddValue(AValue, 'Buffalo');
  ADisplayValue := ADisplayValue + ';Buffalo';
  VerticalGrid.DataController.Filter.Root.AddItem(VerticalGridDeliveryFrom.Properties.ItemLink, foInList, AValue, ADisplayValue);
  AExpression := '[' + VerticalGridDeliveryTo.Properties.Caption + ']';
  VerticalGrid.DataController.Filter.Root.AddExpressionItem(VerticalGridDeliveryFrom.Properties.ItemLink, foNotEqual, AExpression, AExpression);
  VerticalGrid.DataController.Filter.Active := True;
end;

end.
