inherited MasterDetailEMFTableDemoMainForm: TMasterDetailEMFTableDemoMainForm
  Left = 100
  Top = 40
  Caption = 'ExpressQuantumGrid Master-Detail EMF Table Demo'
  ClientHeight = 632
  ClientWidth = 978
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 978
    Height = 32
    Caption = 
      'This demo shows how to use the ExpressQuantumGrid control and th' +
      'e EMF Table View provided by the ExpressEntityMapping Framework ' +
      'to visualize master-detail relationships defined in an entity mo' +
      'del.'
    Visible = False
  end
  object Bevel1: TBevel [1]
    Left = 0
    Top = 16
    Width = 978
    Height = 5
    Align = alTop
    Shape = bsTopLine
  end
  object Splitter: TSplitter [2]
    Left = 0
    Top = 611
    Width = 978
    Height = 2
    Cursor = crVSplit
    Align = alBottom
    Visible = False
  end
  inherited sbMain: TStatusBar
    Top = 613
    Width = 978
  end
  object Grid: TcxGrid [4]
    Left = 0
    Top = 21
    Width = 978
    Height = 590
    Align = alClient
    TabOrder = 0
    object etvCategories: TcxGridEMFTableView
      Navigator.Buttons.CustomButtons = <>
      ScrollbarAnnotations.CustomAnnotations = <>
      DataController.DataSource = dxEMFDataSourceCategories
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      NewItemRow.Visible = True
      OptionsView.CellAutoHeight = True
      OptionsView.GroupByBox = False
      object etvCategoriesPicture: TcxGridEMFColumn
        DataBinding.FieldName = 'Picture'
        PropertiesClassName = 'TcxImageProperties'
        Properties.GraphicClassName = 'TdxSmartImage'
        Width = 240
      end
      object etvCategoriesCategoryName: TcxGridEMFColumn
        Caption = 'Category Name'
        DataBinding.FieldName = 'CategoryName'
        Width = 268
      end
      object etvCategoriesDescription: TcxGridEMFColumn
        DataBinding.FieldName = 'Description'
        Width = 312
      end
    end
    object etvProducts: TcxGridEMFTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Visible = True
      ScrollbarAnnotations.CustomAnnotations = <>
      DataController.DataSource = dxEMFDataSourceProducts
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skSum
          Position = spFooter
          FieldName = 'UnitsInStock'
          Column = etvProductsUnitsInStock
        end
        item
          Kind = skSum
          FieldName = 'UnitsInStock'
          Column = etvProductsUnitsInStock
        end
        item
          Kind = skSum
          FieldName = 'UnitsOnOrder'
          Column = etvProductsUnitsOnOrder
        end
        item
          Kind = skSum
          Position = spFooter
          FieldName = 'UnitsOnOrder'
          Column = etvProductsUnitsOnOrder
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skSum
          FieldName = 'UnitsInStock'
          Column = etvProductsUnitsInStock
        end
        item
          Kind = skSum
          FieldName = 'UnitsOnOrder'
          Column = etvProductsUnitsOnOrder
        end>
      DataController.Summary.SummaryGroups = <>
      DataController.DetailKeyFieldNames = 'CategoryID'
      DataController.MasterKeyFieldNames = 'CategoryID'
      NewItemRow.Visible = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupFooters = gfAlwaysVisible
      object etvProductsProductName: TcxGridEMFColumn
        Caption = 'Product Name'
        DataBinding.FieldName = 'ProductName'
        Width = 300
      end
      object etvProductsQuantityPerUnit: TcxGridEMFColumn
        Caption = 'Quantity Per Unit'
        DataBinding.FieldName = 'QuantityPerUnit'
        Width = 150
      end
      object etvProductsUnitPrice: TcxGridEMFColumn
        Caption = 'Unit Price'
        DataBinding.FieldName = 'UnitPrice'
        PropertiesClassName = 'TcxCurrencyEditProperties'
        Width = 100
      end
      object etvProductsUnitsInStock: TcxGridEMFColumn
        Caption = 'Units In Stock'
        DataBinding.FieldName = 'UnitsInStock'
        Width = 100
      end
      object etvProductsUnitsOnOrder: TcxGridEMFColumn
        Caption = 'Units On Order'
        DataBinding.FieldName = 'UnitsOnOrder'
        Width = 100
      end
      object etvProductsReorderLevel: TcxGridEMFColumn
        Caption = 'Reorder Level'
        DataBinding.FieldName = 'ReorderLevel'
        PropertiesClassName = 'TcxProgressBarProperties'
        Properties.BarStyle = cxbsGradient
        Width = 200
      end
      object etvProductsDiscontinued: TcxGridEMFColumn
        DataBinding.FieldName = 'Discontinued'
        Visible = False
        GroupIndex = 0
      end
    end
    object lvCategories: TcxGridLevel
      GridView = etvCategories
      MaxDetailHeight = 500
      object lvProducts: TcxGridLevel
        GridView = etvProducts
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 72
    Top = 112
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 72
    Top = 216
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 72
    Top = 168
  end
  object dxEMFDataSourceCategories: TdxEMFDataSource
    DataContext = dxEMFDataContext1
    EntityName = 'NWindFoodsModel.Entities.TCategories'
    Session = dxEMFSession1
    Left = 432
    Top = 320
  end
  object dxEMFDataSourceProducts: TdxEMFDataSource
    DataContext = dxEMFDataContext1
    EntityName = 'NWindFoodsModel.Entities.TProducts'
    Session = dxEMFSession1
    Left = 432
    Top = 376
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 'Provider=Microsoft.Jet.OLEDB.4.0;Persist Security Info=False;'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 56
    Top = 376
  end
  object dxEMFADODataProvider1: TdxEMFADODataProvider
    Options.AutoCreate = SchemaAlreadyExists
    Options.DBEngine = 'MSAccess'
    Connection = ADOConnection1
    Left = 176
    Top = 376
  end
  object dxEMFSession1: TdxEMFSession
    DataProvider = dxEMFADODataProvider1
    Left = 304
    Top = 320
  end
  object dxEMFDataContext1: TdxEMFDataContext
    PackageName = 'NWindFoodsModel'
    Left = 304
    Top = 376
  end
end
