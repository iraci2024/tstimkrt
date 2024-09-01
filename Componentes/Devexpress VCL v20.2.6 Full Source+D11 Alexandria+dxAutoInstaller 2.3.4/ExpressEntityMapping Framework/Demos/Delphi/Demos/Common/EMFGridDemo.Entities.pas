unit EMFGridDemo.Entities;

interface

uses
  SysUtils,
  dxEMF.Core,
  dxEMF.Attributes,
  dxEMF.Types,
  dxEMF.Metadata;

type
  TEMFGridCustomersDemo = class;
  TEMFGridOrdersDemo = class;
  TEMFGridTableDemo = class;

  [Entity]
  [Table('EMFGridCustomersDemo')]
  [Indexes('OID')]
  TEMFGridCustomersDemo = class
  private
    [Column, Generator(TdxGeneratorType.Identity), Key]
    FOID: Integer;
    FFirstName: string;
    FLastName: string;
    FCompany: string;
    FPrefix: string;
    FTitle: string;
    FAddress: string;
    FCity: string;
    FState: string;
    FZipCode: string;
    FSource: string;
    FCustomer: Boolean;
    FHomePhone: string;
    FFaxPhone: string;
    FDescription: string;
    FEmail: string;
  public
    property OID: Integer read FOID;
    [Column, Size(10)]
    property FirstName: string read FFirstName write FFirstName;
    [Column, Size(10)]
    property LastName: string read FLastName write FLastName;
    [Column, Size(30)]
    property Company: string read FCompany write FCompany;
    [Column, Size(5)]
    property Prefix: string read FPrefix write FPrefix;
    [Column, Size(5)]
    property Title: string read FTitle write FTitle;
    [Column, Size(50)]
    property Address: string read FAddress write FAddress;
    [Column, Size(15)]
    property City: string read FCity write FCity;
    [Column, Size(2)]
    property State: string read FState write FState;
    [Column, Size(10)]
    property ZipCode: string read FZipCode write FZipCode;
    [Column, Size(10)]
    property Source: string read FSource write FSource;
    [Column, Nullable]
    property Customer: Boolean read FCustomer write FCustomer;
    [Column, Size(15)]
    property HomePhone: string read FHomePhone write FHomePhone;
    [Column, Size(15)]
    property FaxPhone: string read FFaxPhone write FFaxPhone;
    [Column, Size(2147483647)]
    property Description: string read FDescription write FDescription;
    [Column, Size(30)]
    property Email: string read FEmail write FEmail;
  end;

  [Entity]
  [Table('EMFGridOrdersDemo')]
  [Indexes('OID')]
  TEMFGridOrdersDemo = class
  private
    [Column, Generator(TdxGeneratorType.Identity), Key]
    FOID: Integer;
    FCustomerID: Int64;
    FOrderDate: TDateTime;
    FTrademark: string;
    FModel: string;
    FHP: Int64;
    FCyl: Int64;
    FTransmissSpeedCount: Int64;
    FTransmissAutomatic: Boolean;
    FCategory: string;
    FPrice: Int64;
  public
    property OID: Integer read FOID;
    [Column]
    property CustomerID: Int64 read FCustomerID write FCustomerID;
    [Column, Nullable]
    property OrderDate: TDateTime read FOrderDate write FOrderDate;
    [Column, Size(20)]
    property Trademark: string read FTrademark write FTrademark;
    [Column, Size(30)]
    property Model: string read FModel write FModel;
    [Column]
    property HP: Int64 read FHP write FHP;
    [Column]
    property Cyl: Int64 read FCyl write FCyl;
    [Column]
    property TransmissSpeedCount: Int64 read FTransmissSpeedCount write FTransmissSpeedCount;
    [Column, Nullable]
    property TransmissAutomatic: Boolean read FTransmissAutomatic write FTransmissAutomatic;
    [Column, Size(10)]
    property Category: string read FCategory write FCategory;
    [Column]
    property Price: Int64 read FPrice write FPrice;
  end;

  [Entity]
  [Table('EMFGridTableDemo')]
  [Indexes('From')]
  [Indexes('HasAttachment')]
  [Indexes('Priority')]
  [Indexes('Sent')]
  [Indexes('Size')]
  [Indexes('Subject')]
  [Indexes('OID')]
  TEMFGridTableDemo = class
  private
    [Column, Generator(TdxGeneratorType.Identity), Key]
    FOID: Integer;
    FSubject: string;
    FFrom: string;
    FSent: TDateTime;
    FSize: Int64;
    FHasAttachment: Boolean;
    FPriority: Integer;
  public
    property OID: Integer read FOID;
    [Column, Size(100), Nullable]
    property Subject: string read FSubject write FSubject;
    [Column, Size(100), Nullable]
    property From: string read FFrom write FFrom;
    [Column, Nullable]
    property Sent: TDateTime read FSent write FSent;
    [Column, Nullable]
    property Size: Int64 read FSize write FSize;
    [Column, Nullable]
    property HasAttachment: Boolean read FHasAttachment write FHasAttachment;
    [Column, Nullable]
    property Priority: Integer read FPriority write FPriority;
  end;


implementation


initialization
  EntityManager.RegisterEntities([
    TEMFGridCustomersDemo,
    TEMFGridOrdersDemo,
    TEMFGridTableDemo]);

finalization
  EntityManager.UnRegisterEntities([
    TEMFGridCustomersDemo,
    TEMFGridOrdersDemo,
    TEMFGridTableDemo]);
end.
