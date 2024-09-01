unit CalculatedFilterItemsDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, MidasLib, dxDateRanges,
  dxScrollbarAnnotations, cxGridBandedTableView,
  cxGridDBBandedTableView,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxmdaset;

type
  TfrmMain = class(TfmBaseForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    alAction: TActionList;
    dsCarOrdersAndTransfer: TDataSource;
    mdCarOrdersAndTransfer: TdxMemData;
    mdCarOrdersAndTransferTrademark: TStringField;
    mdCarOrdersAndTransferName: TWideStringField;
    mdCarOrdersAndTransferBodyStyleID: TIntegerField;
    mdCarOrdersAndTransferBodyStyle: TStringField;
    mdCarOrdersAndTransferPrice: TCurrencyField;
    mdCarOrdersAndTransferSalesDate: TDateField;
    mdCarOrdersAndTransferSalesPrice: TCurrencyField;
    mdCarOrdersAndTransferDeliveryDate: TDateField;
    mdCarOrdersAndTransferDeliveryComplete: TBooleanField;
    mdCarOrdersAndTransferDeliveryFrom: TStringField;
    mdCarOrdersAndTransferDeliveryTo: TStringField;
    dsTowns: TDataSource;
    mdTowns: TdxMemData;
    mdTownsID: TAutoIncField;
    mdTownsName: TStringField;
    Grid: TcxGrid;
    BandedTableView: TcxGridDBBandedTableView;
    BandedTableViewRecId: TcxGridDBBandedColumn;
    BandedTableViewTrademark: TcxGridDBBandedColumn;
    BandedTableViewName: TcxGridDBBandedColumn;
    BandedTableViewBodyStyle: TcxGridDBBandedColumn;
    BandedTableViewPrice: TcxGridDBBandedColumn;
    BandedTableViewBodyStyleID: TcxGridDBBandedColumn;
    BandedTableViewSalesDate: TcxGridDBBandedColumn;
    BandedTableViewSalesPrice: TcxGridDBBandedColumn;
    BandedTableViewDeliveriFrom: TcxGridDBBandedColumn;
    BandedTableViewDeliveryTo: TcxGridDBBandedColumn;
    BandedTableViewDeliveryDate: TcxGridDBBandedColumn;
    BandedTableViewDeliveryComplete: TcxGridDBBandedColumn;
    GridLevel1: TcxGridLevel;
    cxCheckBox1: TcxCheckBox;
    actAllowExpressionEditing: TAction;
    miOptions: TMenuItem;
    miAllowExpressionEditing: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure actAllowExpressionEditingExecute(Sender: TObject);
  public
    procedure LoadCarOrdersAndTransfer;
    procedure LoadTowns;
    procedure SetFilter;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Forms, Math, Variants, AboutDemoForm, cxVariants;

{$R *.dfm}

procedure TfrmMain.actAllowExpressionEditingExecute(Sender: TObject);
begin
  BandedTableView.Filtering.ExpressionEditing := actAllowExpressionEditing.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadTowns;
  LoadCarOrdersAndTransfer;
  SetFilter;
end;

procedure TfrmMain.LoadCarOrdersAndTransfer;
var
  ANow: TDate;
  APrice: Currency;
  ATownCount: Integer;
begin
  ANow := Now;
  ATownCount := mdTowns.RecordCount;
  mdCarOrdersAndTransfer.Active := True;
  mdCarOrdersAndTransfer.DisableControls;
  dmGridCars.mdCarOrders.DisableControls;
  mdTowns.DisableControls;
  try
    dmGridCars.mdCarOrders.First;
    while not dmGridCars.mdCarOrders.Eof do
    begin
      mdCarOrdersAndTransfer.Insert;
      mdCarOrdersAndTransferTrademark.Value := dmGridCars.mdCarOrdersTrademark.Value;
      mdCarOrdersAndTransferName.Value := dmGridCars.mdCarOrdersName.Value;
      mdCarOrdersAndTransferBodyStyleID.Value := dmGridCars.mdCarOrdersBodyStyleID.Value;
      mdCarOrdersAndTransferPrice.Value := dmGridCars.mdCarOrdersPrice.Value;
      APrice := mdCarOrdersAndTransferPrice.Value;
      if Random(3) = 0 then
        APrice := APrice * (115 - Random(31)) / 100;
      mdCarOrdersAndTransferSalesPrice.Value := APrice;
      mdCarOrdersAndTransferSalesDate.Value := ANow - Random(10);
      mdCarOrdersAndTransferDeliveryFrom.Value := AnsiString(mdTowns.Lookup(mdTownsID.FieldName, Random(ATownCount), mdTownsName.FieldName));
      if Random(3) = 0 then
        mdCarOrdersAndTransferDeliveryTo.Value := AnsiString(mdTowns.Lookup(mdTownsID.FieldName, Random(ATownCount), mdTownsName.FieldName))
      else
        mdCarOrdersAndTransferDeliveryTo.Value := mdCarOrdersAndTransferDeliveryFrom.Value;
      mdCarOrdersAndTransferDeliveryDate.Value := Max(mdCarOrdersAndTransferSalesDate.Value, ANow + 5 - Random(15));
      mdCarOrdersAndTransferDeliveryComplete.Value := (Random(10) > 0) and (mdCarOrdersAndTransferDeliveryDate.Value <= ANow);
      mdCarOrdersAndTransfer.Post;
      dmGridCars.mdCarOrders.Next;
    end;
  finally
    mdTowns.EnableControls;
    dmGridCars.mdCarOrders.EnableControls;
    mdCarOrdersAndTransfer.EnableControls;
  end;
end;

procedure TfrmMain.LoadTowns;
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdTowns.LoadFromBinaryFile(APath + 'Towns.dat');
end;

procedure TfrmMain.SetFilter;
var
  AValue: Variant;
  ADisplayValue: string;
  AExpression: string;
  AItemList: TcxFilterCriteriaItemList;
begin
  AItemList := BandedTableView.DataController.Filter.Root.AddItemList(fboOr);
  AExpression := '[' + BandedTableViewPrice.Caption + '] * 95%';
  AItemList.AddExpressionItem(BandedTableViewSalesPrice, foLessEqual, AExpression, AExpression);
  AExpression := '[' + BandedTableViewPrice.Caption + '] * 105%';
  AItemList.AddExpressionItem(BandedTableViewSalesPrice, foGreaterEqual, AExpression, AExpression);
  AValue := VarListArrayCreate('New York');
  ADisplayValue := 'New York';
  VarListArrayAddValue(AValue, 'Boston');
  ADisplayValue := ADisplayValue + ';Boston';
  VarListArrayAddValue(AValue, 'Buffalo');
  ADisplayValue := ADisplayValue + ';Buffalo';
  BandedTableView.DataController.Filter.Root.AddItem(BandedTableViewDeliveriFrom, foInList, AValue, ADisplayValue);
  AExpression := '[' + BandedTableViewDeliveryTo.Caption + ']';
  BandedTableView.DataController.Filter.Root.AddExpressionItem(BandedTableViewDeliveriFrom, foNotEqual, AExpression, AExpression);
  BandedTableView.DataController.Filter.Active := True;
end;

end.
