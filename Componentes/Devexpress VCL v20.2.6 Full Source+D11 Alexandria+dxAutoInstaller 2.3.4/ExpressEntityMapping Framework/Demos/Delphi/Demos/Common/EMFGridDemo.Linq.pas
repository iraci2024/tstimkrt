unit EMFGridDemo.Linq;

interface

uses
  dxEMF.Linq,
  dxEMF.Linq.Expressions,
  EMFGridDemo.Entities;

type
  IEMFGridCustomersDemoExpression = interface;
  IEMFGridOrdersDemoExpression = interface;
  IEMFGridTableDemoExpression = interface;

  IEMFGridCustomersDemoExpression = interface(IdxEntityInfo)
  ['{E3BEF7B2-197A-4174-988A-F9E1B9F3AA42}']
    function OID: TdxLinqExpression;
    function FirstName: TdxLinqExpression;
    function LastName: TdxLinqExpression;
    function Company: TdxLinqExpression;
    function Prefix: TdxLinqExpression;
    function Title: TdxLinqExpression;
    function Address: TdxLinqExpression;
    function City: TdxLinqExpression;
    function State: TdxLinqExpression;
    function ZipCode: TdxLinqExpression;
    function Source: TdxLinqExpression;
    function Customer: TdxLinqExpression;
    function HomePhone: TdxLinqExpression;
    function FaxPhone: TdxLinqExpression;
    function Description: TdxLinqExpression;
    function Email: TdxLinqExpression;
  end;

  IEMFGridOrdersDemoExpression = interface(IdxEntityInfo)
  ['{1CD5A979-4D0A-4D2A-9490-9087B187F497}']
    function OID: TdxLinqExpression;
    function CustomerID: TdxLinqExpression;
    function OrderDate: TdxLinqExpression;
    function Trademark: TdxLinqExpression;
    function Model: TdxLinqExpression;
    function HP: TdxLinqExpression;
    function Cyl: TdxLinqExpression;
    function TransmissSpeedCount: TdxLinqExpression;
    function TransmissAutomatic: TdxLinqExpression;
    function Category: TdxLinqExpression;
    function Price: TdxLinqExpression;
  end;

  IEMFGridTableDemoExpression = interface(IdxEntityInfo)
  ['{B481B0DB-A170-47D2-9892-69433E01026C}']
    function OID: TdxLinqExpression;
    function Subject: TdxLinqExpression;
    function From: TdxLinqExpression;
    function Sent: TdxLinqExpression;
    function Size: TdxLinqExpression;
    function HasAttachment: TdxLinqExpression;
    function Priority: TdxLinqExpression;
  end;

  IEMFGridDemoContext = interface(IdxDataContext)
  ['{614EE459-DF5B-40E7-B359-D0639BAB05BE}']
    function EMFGridCustomersDemo: IEMFGridCustomersDemoExpression;
    function EMFGridOrdersDemo: IEMFGridOrdersDemoExpression;
    function EMFGridTableDemo: IEMFGridTableDemoExpression;
  end;

implementation

initialization
  TdxLinqExpressionFactory.Register<TEMFGridCustomersDemo, IEMFGridCustomersDemoExpression>;
  TdxLinqExpressionFactory.Register<TEMFGridOrdersDemo, IEMFGridOrdersDemoExpression>;
  TdxLinqExpressionFactory.Register<TEMFGridTableDemo, IEMFGridTableDemoExpression>;
  TdxLinqExpressionFactory.Register<IEMFGridDemoContext>;

finalization
  TdxLinqExpressionFactory.UnRegister([
    TEMFGridCustomersDemo,
    TEMFGridOrdersDemo,
    TEMFGridTableDemo]);
  TdxLinqExpressionFactory.UnRegister<IEMFGridDemoContext>;

end.
