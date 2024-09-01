unit NWindFoodsModel.Entities;

interface

uses
  SysUtils,
  dxCoreClasses,
  dxEMF.Core,
  dxEMF.Attributes,
  dxEMF.Types,
  dxEMF.Metadata;

type
  TCategories = class;
  TProducts = class;

  [Entity]
  [Table('Categories')]
  TCategories = class
  private
    [Column, Generator(TdxGeneratorType.Identity), Key]
    FCategoryID: Integer;
    FCategoryName: string;
    FDescription: TdxNullableString;
    FPicture: TBytes;
    FIcon_17: TBytes;
    FIcon_25: TBytes;
    [Association('Categories_Products')]
    FProducts_Collection: IdxEMFCollection<TProducts>;
  public
    constructor Create;
    property CategoryID: Integer read FCategoryID;
    [Column, Size(15)]
    property CategoryName: string read FCategoryName write FCategoryName;
    [Column, Blob, Nullable]
    property Description: TdxNullableString read FDescription write FDescription;
    [Column, Nullable]
    property Picture: TBytes read FPicture write FPicture;
    [Column, Nullable]
    property Icon_17: TBytes read FIcon_17 write FIcon_17;
    [Column, Nullable]
    property Icon_25: TBytes read FIcon_25 write FIcon_25;
    property Products_Collection: IdxEMFCollection<TProducts> read FProducts_Collection;
  end;

  [Entity]
  [Table('Products')]
  TProducts = class
  private
    [Column, Generator(TdxGeneratorType.Identity), Key]
    FProductID: Integer;
    FProductName: string;
    FSupplierID: TdxNullableInteger;
    FCategoryID: TCategories;
    FQuantityPerUnit: TdxNullableString;
    FUnitPrice: TdxNullableCurrency;
    FUnitsInStock: TdxNullableSmallInt;
    FUnitsOnOrder: TdxNullableSmallInt;
    FReorderLevel: TdxNullableSmallInt;
    FDiscontinued: Boolean;
    FEAN13: TdxNullableString;
    procedure SetCategoryID(AValue: TCategories);
  public
    property ProductID: Integer read FProductID;
    [Column, Size(40)]
    property ProductName: string read FProductName write FProductName;
    [Column, Nullable, Default('0')]
    property SupplierID: TdxNullableInteger read FSupplierID write FSupplierID;
    [Association('Categories_Products'), Column, Nullable, Default('0')]
    property CategoryID: TCategories read FCategoryID write SetCategoryID;
    [Column, Size(20), Nullable]
    property QuantityPerUnit: TdxNullableString read FQuantityPerUnit write FQuantityPerUnit;
    [Column, Nullable, Default('0')]
    property UnitPrice: TdxNullableCurrency read FUnitPrice write FUnitPrice;
    [Column, Nullable, Default('0')]
    property UnitsInStock: TdxNullableSmallInt read FUnitsInStock write FUnitsInStock;
    [Column, Nullable, Default('0')]
    property UnitsOnOrder: TdxNullableSmallInt read FUnitsOnOrder write FUnitsOnOrder;
    [Column, Nullable, Default('0')]
    property ReorderLevel: TdxNullableSmallInt read FReorderLevel write FReorderLevel;
    [Column, Size(2), Default('=No')]
    property Discontinued: Boolean read FDiscontinued write FDiscontinued;
    [Column, Size(12), Nullable, Default('0')]
    property EAN13: TdxNullableString read FEAN13 write FEAN13;
  end;


implementation

{ TCategories }

constructor TCategories.Create;
begin
  inherited;
  FProducts_Collection := TdxEMFCollections.Create<TProducts>(Self, 'FProducts_Collection');
end;

{ TProducts }

procedure TProducts.SetCategoryID(AValue: TCategories);
begin
  FCategoryID := AValue;
  if FCategoryID <> nil then
    FCategoryID.Products_Collection.Add(Self);
end;


initialization
  EntityManager.RegisterEntities([
    TCategories,
    TProducts]);

finalization
  EntityManager.UnRegisterEntities([
    TCategories,
    TProducts]);
end.
