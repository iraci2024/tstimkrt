unit MasterDetailEMFTableDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridLevel, cxControls, cxGrid, StdCtrls, ExtCtrls, Menus,
  cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGridCustomView, ComCtrls, cxStyles, cxCustomData,
  cxGraphics, cxFilter, cxData, cxEdit, DB, cxDBData, cxClasses,
  cxDataStorage, cxDBLookupComboBox, cxBlobEdit, cxLookAndFeels,
  cxLookAndFeelPainters, BaseForm, cxContainer, cxLabel, cxGridCardView, 
  cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog, dxDateRanges,
  cxGridEMFTableView, dxEMF.DataDefinitions, cxEMFData, cxCurrencyEdit, cxProgressBar, cxImage, dxEMF.DB.MSAccess,
  dxEMF.Core, dxEMF.DataProvider.ADO, ADODB, dxScrollbarAnnotations;

type
  TMasterDetailEMFTableDemoMainForm = class(TfmBaseForm)
    Bevel1: TBevel;
    lvCategories: TcxGridLevel;
    Grid: TcxGrid;
    Splitter: TSplitter;
    etvCategories: TcxGridEMFTableView;
    dxEMFDataSourceCategories: TdxEMFDataSource;
    etvCategoriesCategoryName: TcxGridEMFColumn;
    etvCategoriesDescription: TcxGridEMFColumn;
    dxEMFDataSourceProducts: TdxEMFDataSource;
    etvProducts: TcxGridEMFTableView;
    etvProductsProductName: TcxGridEMFColumn;
    etvProductsQuantityPerUnit: TcxGridEMFColumn;
    etvProductsUnitPrice: TcxGridEMFColumn;
    etvProductsUnitsInStock: TcxGridEMFColumn;
    etvProductsUnitsOnOrder: TcxGridEMFColumn;
    etvProductsReorderLevel: TcxGridEMFColumn;
    etvProductsDiscontinued: TcxGridEMFColumn;
    lvProducts: TcxGridLevel;
    etvCategoriesPicture: TcxGridEMFColumn;
    ADOConnection1: TADOConnection;
    dxEMFADODataProvider1: TdxEMFADODataProvider;
    dxEMFSession1: TdxEMFSession;
    dxEMFDataContext1: TdxEMFDataContext;
    procedure FormCreate(Sender: TObject);
  private
    procedure OnCategoriesExpanded(ADataController: TcxCustomDataController; ARecordIndex: Integer);
  end;

var
  MasterDetailEMFTableDemoMainForm: TMasterDetailEMFTableDemoMainForm;

implementation

{$R *.dfm}

uses
  cxGridEMFDataDefinitions, AboutDemoForm;

procedure TMasterDetailEMFTableDemoMainForm.FormCreate(Sender: TObject);
const
  DataPath = '..\..\..\Data\';
  DBFileName = 'nwind_foods.mdb';
begin
  ADOConnection1.Connected := False;
  ADOConnection1.Properties['Data Source'].Value := DataPath + DBFileName;
  ADOConnection1.Connected := True;
  dxEMFDataSourceCategories.Active := True;
  dxEMFDataSourceProducts.Active := True;

  etvCategories.DataController.OnDetailExpanded := OnCategoriesExpanded;
end;

procedure TMasterDetailEMFTableDemoMainForm.OnCategoriesExpanded(ADataController: TcxCustomDataController;
  ARecordIndex: Integer);
begin
  (ADataController.GetDetailDataController(ARecordIndex, 0) as TcxGridEMFDataController).Groups.FullExpand;
end;

end.
