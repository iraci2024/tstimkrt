unit CalculatedFilterItemsDemoMain;

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
  cxInplaceContainer, cxDBTL, cxTLData, dxmdaset, dxScrollbarAnnotations,
  cxDBLookupComboBox, cxCurrencyEdit, cxCalendar;

type
  TfrmMain = class(TDemoBasicMainForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    alAction: TActionList;
    cxCheckBox1: TcxCheckBox;
    actExpressionEditing: TAction;
    AllowFilterExpressionEditing1: TMenuItem;
    cxStyleRepository1: TcxStyleRepository;
    stGroup: TcxStyle;
    tlDB: TcxDBTreeList;
    tlDBRecId: TcxDBTreeListColumn;
    tlDBModel: TcxDBTreeListColumn;
    tlDBBodyStyleID: TcxDBTreeListColumn;
    tlDBBodyStyle: TcxDBTreeListColumn;
    tlDBPrice: TcxDBTreeListColumn;
    tlDBSalesDate: TcxDBTreeListColumn;
    tlDBSalesPrice: TcxDBTreeListColumn;
    tlDBDeliveryDate: TcxDBTreeListColumn;
    tlDBDeliveryComplete: TcxDBTreeListColumn;
    tlDBDeliveryFrom: TcxDBTreeListColumn;
    tlDBDeliveryTo: TcxDBTreeListColumn;
    procedure FormCreate(Sender: TObject);
    procedure actExpressionEditingExecute(Sender: TObject);
    procedure tlDBEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
      var Allow: Boolean);
    procedure tlDBStylesGetContentStyle(Sender: TcxCustomTreeList;
      AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
  protected
    procedure SetFilter;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  CarsData, cxVariants;

procedure TfrmMain.actExpressionEditingExecute(Sender: TObject);
begin
  tlDB.Filtering.ExpressionEditing := actExpressionEditing.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetFilter;
  tlDB.FullExpand;
  ActiveControl := tlDB;
end;

procedure TfrmMain.SetFilter;
var
  AValue: Variant;
  ADisplayValue: string;
  AExpression: string;
  AItemList: TcxFilterCriteriaItemList;
begin
  AItemList := tlDB.DataController.Filter.Root.AddItemList(fboOr);
  AExpression := '[' + tlDBPrice.Caption.Text + '] * 95%';
  AItemList.AddExpressionItem(tlDBSalesPrice, foLessEqual, AExpression, AExpression);
  AExpression := '[' + tlDBPrice.Caption.Text + '] * 105%';
  AItemList.AddExpressionItem(tlDBSalesPrice, foGreaterEqual, AExpression, AExpression);
  AValue := VarListArrayCreate('New York');
  ADisplayValue := 'New York';
  VarListArrayAddValue(AValue, 'Boston');
  ADisplayValue := ADisplayValue + ';Boston';
  VarListArrayAddValue(AValue, 'Buffalo');
  ADisplayValue := ADisplayValue + ';Buffalo';
  tlDB.DataController.Filter.Root.AddItem(tlDBDeliveryFrom, foInList, AValue, ADisplayValue);
  AExpression := '[' + tlDBDeliveryTo.Caption.Text + ']';
  tlDB.DataController.Filter.Root.AddExpressionItem(tlDBDeliveryFrom, foNotEqual, AExpression, AExpression);
  tlDB.DataController.Filter.Active := True;
  if tlDB.AbsoluteVisibleCount > 0 then
    tlDB.AbsoluteVisibleItems[0].Focused := True;
end;

procedure TfrmMain.tlDBEditing(Sender: TcxCustomTreeList;
  AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  Allow := Allow and not tlDB.FocusedNode.IsGroupNode;
end;

procedure TfrmMain.tlDBStylesGetContentStyle(Sender: TcxCustomTreeList;
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
begin
  if ANode.IsGroupNode then
    AStyle := stGroup;
end;

end.
