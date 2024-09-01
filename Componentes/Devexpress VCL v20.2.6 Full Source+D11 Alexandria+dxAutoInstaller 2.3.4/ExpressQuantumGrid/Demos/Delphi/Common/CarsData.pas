unit CarsData;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, DB, dxmdaset, cxClasses, cxEdit, cxEditRepositoryItems, cxDBEditRepository, cxStyles, cxImageComboBox,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxNavigator, cxControls, cxHyperLinkEdit, cxDBData, cxMemo,
  cxImageList, cxExtEditRepositoryItems;

type

  { TdmCars }

  TdmCars = class(TDataModule)
    dsBodyStyle: TDataSource;
    dsCategory: TDataSource;
    dsModels: TDataSource;
    dsTrademark: TDataSource;
    dsTransmissionType: TDataSource;
    EditRepository: TcxEditRepository;
    EditRepositoryBodyStyleLookup: TcxEditRepositoryLookupComboBoxItem;
    EditRepositoryCategoryLookup: TcxEditRepositoryLookupComboBoxItem;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryImageBlob: TcxEditRepositoryBlobItem;
    EditRepositoryMemo: TcxEditRepositoryMemoItem;
    EditRepositoryMemoBlob: TcxEditRepositoryBlobItem;
    EditRepositoryTransmissionTypeCheckBox: TcxEditRepositoryCheckBoxItem;
    EditRepositoryTransmissionTypeLookup: TcxEditRepositoryLookupComboBoxItem;
    EditRepositoryTrademarkLogo: TcxEditRepositoryImageComboBoxItem;
    EditRepositoryModelRating: TcxEditRepositoryRatingControl;
    mdBodyStyle: TdxMemData;
    mdBodyStyleID: TIntegerField;
    mdBodyStyleName: TWideStringField;
    mdCategory: TdxMemData;
    mdCategoryID: TIntegerField;
    mdCategoryName: TWideStringField;
    mdCategoryPicture: TBlobField;
    mdModels: TdxMemData;
    mdModelsBodyStyleID: TIntegerField;
    mdModelsCategoryID: TIntegerField;
    mdModelsCilinders: TIntegerField;
    mdModelsDelivery_Date: TDateTimeField;
    mdModelsDescription: TWideMemoField;
    mdModelsDoors: TIntegerField;
    mdModelsFullName: TWideStringField;
    mdModelsHorsepower: TWideStringField;
    mdModelsHyperlink: TStringField;
    mdModelsID: TIntegerField;
    mdModelsImage: TBlobField;
    mdModelsInStock: TBooleanField;
    mdModelsModification: TWideStringField;
    mdModelsMPG_City: TIntegerField;
    mdModelsMPG_Highway: TIntegerField;
    mdModelsName: TWideStringField;
    mdModelsPhoto: TBlobField;
    mdModelsPrice: TBCDField;
    mdModelsTorque: TWideStringField;
    mdModelsTrademark: TWideStringField;
    mdModelsTrademarkID: TIntegerField;
    mdModelsTransmission_Speeds: TWideStringField;
    mdModelsTransmission_Type: TIntegerField;
    mdModelsTransmissionTypeName: TStringField;
    mdTrademark: TdxMemData;
    mdTrademarkDescription: TWideMemoField;
    mdTrademarkID: TIntegerField;
    mdTrademarkLogo: TBlobField;
    mdTrademarkName: TWideStringField;
    mdTrademarkSite: TWideStringField;
    mdTransmissionType: TdxMemData;
    mdTransmissionTypeID: TIntegerField;
    mdTransmissionTypeName: TWideStringField;
    mdModelsCategory: TStringField;
    mdModelsBodyStyle: TStringField;
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
    mdCarOrdersParentID: TIntegerField;
    dsTowns: TDataSource;
    mdTowns: TdxMemData;
    mdTownsID: TAutoIncField;
    mdTownsName: TStringField;
    dsCarOrdersAndDelivery: TDataSource;
    mdCarOrdersAndDelivery: TdxMemData;
    mdCarOrdersAndDeliveryName: TWideStringField;
    mdCarOrdersAndDeliveryBodyStyleID: TIntegerField;
    mdCarOrdersAndDeliveryBodyStyle: TStringField;
    mdCarOrdersAndDeliveryPrice: TCurrencyField;
    mdCarOrdersAndDeliverySalesDate: TDateField;
    mdCarOrdersAndDeliverySalesPrice: TCurrencyField;
    mdCarOrdersAndDeliveryDeliveryDate: TDateField;
    mdCarOrdersAndDeliveryDeliveryComplete: TBooleanField;
    mdCarOrdersAndDeliveryDeliveryFrom: TStringField;
    mdCarOrdersAndDeliveryDeliveryTo: TStringField;
    mdCarOrdersAndDeliveryParentID: TIntegerField;
    mdCarOrdersAndDeliveryID: TIntegerField;
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
    mdCarOrdersAndTransferModelID: TIntegerField;
    mdCarOrdersAndDeliveryTrademark: TStringField;
    mdCarOrdersAndDeliveryModelID: TIntegerField;
    mdModelsRating: TFloatField;
    ilLogo: TcxImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure mdModelsCalcFields(DataSet: TDataSet);
  protected
    procedure FillLogoDescription;
    procedure LoadCarOrders(const APath: string);
    procedure LoadCarOrdersAndDelivery;
    procedure LoadModelsData(const APath: string);
  end;

var
  dmCars: TdmCars;

implementation

uses
  Forms, Math, Variants;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmCars.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdBodyStyle.LoadFromBinaryFile(APath + 'CarsBodyStyle.dat');
  mdCategory.LoadFromBinaryFile(APath + 'CarsCategory.dat');
  LoadModelsData(APath);
  mdTrademark.LoadFromBinaryFile(APath + 'CarsTrademark.dat');
  mdTransmissionType.LoadFromBinaryFile(APath + 'CarsTransmissionType.dat');
  mdTowns.LoadFromBinaryFile(APath + 'Towns.dat');

  mdBodyStyle.Active := True;
  mdCategory.Active := True;
  mdTrademark.Active := True;
  mdTransmissionType.Active := True;
  mdModels.Active := True;
  mdTowns.Active := True;
  mdCarOrdersAndDelivery.Active := True;

  LoadCarOrders(APath);
  LoadCarOrdersAndDelivery;

  FillLogoDescription;
end;

procedure TdmCars.FillLogoDescription;
var
  I: Integer;
  AItems: TcxImageComboBoxItems;
begin
  AItems := EditRepositoryTrademarkLogo.Properties.Items;
  for I := 0 to AItems.Count - 1 do
    AItems[I].Description := mdTrademark.Lookup(mdTrademarkID.FieldName, AItems[I].Value, mdTrademarkName.FieldName);
end;

procedure TdmCars.LoadCarOrders(const APath: string);
var
  ANow: TDate;
begin
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
end;

procedure TdmCars.LoadCarOrdersAndDelivery;
var
  ANow: TDate;
  AValue: Variant;
  APrice: Currency;
  ATownCount: Integer;
begin
  ANow := Now;
  ATownCount := mdTowns.RecordCount;
  mdCarOrdersAndDelivery.DisableControls;
  mdCarOrders.DisableControls;
  mdTowns.DisableControls;
  try
    mdCarOrders.First;
    while not mdCarOrders.Eof do
    begin
      mdCarOrdersAndDelivery.Insert;
      mdCarOrdersAndDeliveryID.Value := mdCarOrdersID.Value;
      mdCarOrdersAndDeliveryParentID.Value := mdCarOrdersParentID.Value;
      mdCarOrdersAndDeliveryTrademark.Value := mdCarOrdersTrademark.Value;
      mdCarOrdersAndDeliveryName.Value := mdCarOrdersName.Value;
      AValue := mdModels.Lookup(mdModelsName.FieldName, mdCarOrdersAndDeliveryName.Value, mdModelsID.FieldName);
      if not VarIsNull(AValue) then
        mdCarOrdersAndDeliveryModelID.Value := AValue;
      mdCarOrdersAndDeliveryBodyStyleID.Value := mdCarOrdersBodyStyleID.Value;
      mdCarOrdersAndDeliveryPrice.Value := mdCarOrdersPrice.Value;
      APrice := mdCarOrdersAndDeliveryPrice.Value;
      if Random(3) = 0 then
        APrice := APrice * (115 - Random(31)) / 100;
      mdCarOrdersAndDeliverySalesPrice.Value := APrice;
      mdCarOrdersAndDeliverySalesDate.Value := ANow - Random(10);
      mdCarOrdersAndDeliveryDeliveryFrom.Value := AnsiString(mdTowns.Lookup(mdTownsID.FieldName, Random(ATownCount), mdTownsName.FieldName));
      if Random(3) = 0 then
        mdCarOrdersAndDeliveryDeliveryTo.Value := AnsiString(mdTowns.Lookup(mdTownsID.FieldName, Random(ATownCount), mdTownsName.FieldName))
      else
        mdCarOrdersAndDeliveryDeliveryTo.Value := mdCarOrdersAndDeliveryDeliveryFrom.Value;
      mdCarOrdersAndDeliveryDeliveryDate.Value := Max(mdCarOrdersAndDeliverySalesDate.Value, ANow + 5 - Random(15));
      mdCarOrdersAndDeliveryDeliveryComplete.Value := (Random(10) > 0) and (mdCarOrdersAndDeliveryDeliveryDate.Value <= ANow);
      mdCarOrdersAndDelivery.Post;
      mdCarOrders.Next;
    end;
    mdCarOrdersAndDelivery.SortedField := mdCarOrdersAndTransferModelID.FieldName;
    mdCarOrders.First;
    mdCarOrdersAndDelivery.First;
  finally
    mdTowns.EnableControls;
    mdCarOrders.EnableControls;
    mdCarOrdersAndDelivery.EnableControls;
  end;
end;

procedure TdmCars.LoadModelsData(const APath: string);
begin
  with mdModels do
  begin
    LoadFromBinaryFile(APath + 'CarsModel.dat');
    First;
    while not Eof do
    begin
      Edit;
      mdModelsRating.Value := 3.7 + Random(14) / 10;
      Post;
      Next;
    end;
    First;
  end;
end;

procedure TdmCars.mdModelsCalcFields(DataSet: TDataSet);
begin
  mdModelsFullName.Value := mdModelsTrademark.Value + ' ' + mdModelsName.Value;
end;

end.
