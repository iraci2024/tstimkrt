unit NWindFoodsModel.Linq;

interface

uses
  dxEMF.Linq,
  dxEMF.Linq.Expressions,
  NWindFoodsModel.Entities;

type
  ICategoriesExpression = interface;
  IProductsCollectionExpression = interface;
  IProductsExpression = interface;

  ICategoriesExpression = interface(IdxEntityInfo)
  ['{2D2A498C-58DF-4DF1-BB6F-A9666B7E9E36}']
    function CategoryID: TdxLinqExpression;
    function CategoryName: TdxLinqExpression;
    function Description: TdxLinqExpression;
    function Picture: TdxLinqExpression;
    function Icon_17: TdxLinqExpression;
    function Icon_25: TdxLinqExpression;
    function Products_Collection: IProductsCollectionExpression;
  end;

  IProductsExpression = interface(IdxEntityInfo)
  ['{AF3CD979-A2B4-48CD-9EC6-0A91B2321983}']
    function ProductID: TdxLinqExpression;
    function ProductName: TdxLinqExpression;
    function SupplierID: TdxLinqExpression;
    function CategoryID: ICategoriesExpression;
    function QuantityPerUnit: TdxLinqExpression;
    function UnitPrice: TdxLinqExpression;
    function UnitsInStock: TdxLinqExpression;
    function UnitsOnOrder: TdxLinqExpression;
    function ReorderLevel: TdxLinqExpression;
    function Discontinued: TdxLinqExpression;
    function EAN13: TdxLinqExpression;
  end;

  IProductsCollectionExpression = interface(IdxLinqCollectionExpression<IProductsExpression>)
  ['{602145E6-4189-4A54-86E5-01C4F9555D69}']
  end;

  INWindFoodsModelContext = interface(IdxDataContext)
  ['{04D43BDF-8271-4133-A7C9-6BEDA75CDFD8}']
    function Categories: ICategoriesExpression;
    function Products: IProductsExpression;
  end;

implementation

initialization
  TdxLinqExpressionFactory.Register<TCategories, ICategoriesExpression>;
  TdxLinqExpressionFactory.Register<TProducts, IProductsExpression>;
  TdxLinqExpressionFactory.Register<INWindFoodsModelContext>;

finalization
  TdxLinqExpressionFactory.UnRegister([
    TCategories,
    TProducts]);
  TdxLinqExpressionFactory.UnRegister<INWindFoodsModelContext>;

end.
