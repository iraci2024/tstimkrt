object GridViewDemoDataDM: TGridViewDemoDataDM
  OldCreateOrder = False
  Height = 261
  Width = 331
  object ADOConnection: TADOConnection
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 32
    Top = 72
  end
  object ADOQuery: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    Left = 128
    Top = 72
  end
  object EMFDataSource: TdxEMFDataSource
    DataContext = EMFDataContext
    EntityName = 'TEMFGridTableDemo'
    Session = EMFSession
    Left = 32
    Top = 144
  end
  object EMFSession: TdxEMFSession
    DataProvider = EMFADODataProvider
    Left = 32
    Top = 8
  end
  object EMFADODataProvider: TdxEMFADODataProvider
    Options.AutoCreate = DatabaseAndSchema
    Options.DBEngine = 'MSSQLServer'
    Connection = ADOConnection
    Left = 128
    Top = 8
  end
  object EMFDataContext: TdxEMFDataContext
    Left = 240
    Top = 8
  end
end
