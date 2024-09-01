object ScrollbarAnnotationsDemoDataDM: TScrollbarAnnotationsDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object mdDepartments: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0A000000040000000C000300494400040000000300
      0900504152454E544944000400000003000A004D414E41474552494400320000
      00010005004E414D450008000000060007004255444745540032000000010009
      004C4F434154494F4E00320000000100060050484F4E45003200000001000400
      46415800FF00000001000600454D41494C000200000005000800564143414E43
      5900}
    SortOptions = []
    Left = 136
    Top = 24
    object mdDepartmentsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdDepartmentsPARENTID: TIntegerField
      FieldName = 'PARENTID'
    end
    object mdDepartmentsMANAGERID: TIntegerField
      FieldName = 'MANAGERID'
    end
    object mdDepartmentsNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object mdDepartmentsBUDGET: TFloatField
      FieldName = 'BUDGET'
    end
    object mdDepartmentsLOCATION: TStringField
      FieldName = 'LOCATION'
      Size = 50
    end
    object mdDepartmentsPHONE: TStringField
      FieldName = 'PHONE'
      Size = 50
    end
    object mdDepartmentsFAX: TStringField
      FieldName = 'FAX'
      Size = 50
    end
    object mdDepartmentsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 255
    end
    object mdDepartmentsVACANCY: TBooleanField
      FieldName = 'VACANCY'
    end
  end
  object dsDepartments: TDataSource
    DataSet = mdDepartments
    Left = 136
    Top = 72
  end
end
