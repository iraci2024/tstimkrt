object dmDevAV: TdmDevAV
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 361
  Width = 276
  object mdPrefix: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000A005072656669785F4944
      000500000001000C005072656669785F4E616D65000100000000010200000044
      72010100000001020000004D72010200000001020000004D7301030000000104
      0000004D697373010400000001030000004D7273}
    SortOptions = []
    Left = 127
    Top = 295
    object mdPrefixPrefix_ID: TIntegerField
      FieldName = 'Prefix_ID'
    end
    object mdPrefixPrefix_Name: TStringField
      FieldName = 'Prefix_Name'
      Size = 5
    end
  end
  object dsPrefix: TDataSource
    DataSet = mdPrefix
    Left = 39
    Top = 295
  end
  object dsTasks: TDataSource
    DataSet = mdTasks
    Left = 39
    Top = 232
  end
  object dsEmployees: TDataSource
    DataSet = mdEmployees
    Left = 39
    Top = 176
  end
  object mdTasks: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 127
    Top = 232
    object mdTasksId: TIntegerField
      FieldName = 'Id'
    end
    object mdTasksSubject: TWideStringField
      FieldName = 'Subject'
      Size = 100
    end
    object mdTasksDescription: TWideStringField
      FieldName = 'Description'
      Size = 4096
    end
    object mdTasksRtfTextDescription: TWideStringField
      FieldName = 'RtfTextDescription'
      Size = 4096
    end
    object mdTasksStartDate: TDateTimeField
      FieldName = 'StartDate'
    end
    object mdTasksDueDate: TDateTimeField
      FieldName = 'DueDate'
    end
    object mdTasksStatus: TIntegerField
      FieldName = 'Status'
    end
    object mdTasksPriority: TIntegerField
      FieldName = 'Priority'
    end
    object mdTasksCompletion: TIntegerField
      FieldName = 'Completion'
    end
    object mdTasksReminder: TBooleanField
      FieldName = 'Reminder'
    end
    object mdTasksReminderDateTime: TDateTimeField
      FieldName = 'ReminderDateTime'
    end
    object mdTasksAssignedEmployeeId: TIntegerField
      FieldName = 'AssignedEmployeeId'
    end
    object mdTasksOwnerId: TIntegerField
      FieldName = 'OwnerId'
    end
    object mdTasksCustomerEmployeeId: TIntegerField
      FieldName = 'CustomerEmployeeId'
    end
    object mdTasksFollowUp: TIntegerField
      FieldName = 'FollowUp'
    end
    object mdTasksPrivate: TBooleanField
      FieldName = 'Private'
    end
    object mdTasksCategory: TWideStringField
      FieldName = 'Category'
      Size = 10
    end
    object mdTasksAttachedCollectionsChanged: TBooleanField
      FieldName = 'AttachedCollectionsChanged'
    end
  end
  object mdEmployees: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 127
    Top = 176
    object mdEmployeesId: TIntegerField
      FieldName = 'Id'
    end
    object mdEmployeesDepartment: TIntegerField
      FieldName = 'Department'
    end
    object mdEmployeesTitle: TWideStringField
      FieldName = 'Title'
      Size = 100
    end
    object mdEmployeesStatus: TIntegerField
      FieldName = 'Status'
    end
    object mdEmployeesHireDate: TDateTimeField
      FieldName = 'HireDate'
    end
    object mdEmployeesPersonalProfile: TWideStringField
      FieldName = 'PersonalProfile'
      Size = 4096
    end
    object mdEmployeesFirstName: TWideStringField
      FieldName = 'FirstName'
      Size = 50
    end
    object mdEmployeesLastName: TWideStringField
      FieldName = 'LastName'
      Size = 50
    end
    object mdEmployeesFullName: TWideStringField
      FieldName = 'FullName'
      Size = 50
    end
    object mdEmployeesPrefix: TIntegerField
      FieldName = 'Prefix'
    end
    object mdEmployeesHomePhone: TWideStringField
      FieldName = 'HomePhone'
      Size = 50
    end
    object mdEmployeesMobilePhone: TWideStringField
      FieldName = 'MobilePhone'
      Size = 50
    end
    object mdEmployeesEmail: TWideStringField
      FieldName = 'Email'
      Size = 50
    end
    object mdEmployeesSkype: TWideStringField
      FieldName = 'Skype'
      Size = 50
    end
    object mdEmployeesBirthDate: TDateTimeField
      FieldName = 'BirthDate'
    end
    object mdEmployeesPictureId: TIntegerField
      FieldName = 'PictureId'
    end
    object mdEmployeesAddress_Line: TWideStringField
      FieldName = 'Address_Line'
      Size = 50
    end
    object mdEmployeesAddress_City: TWideStringField
      FieldName = 'Address_City'
      Size = 30
    end
    object mdEmployeesAddress_State: TIntegerField
      FieldName = 'Address_State'
    end
    object mdEmployeesAddress_ZipCode: TWideStringField
      FieldName = 'Address_ZipCode'
      Size = 10
    end
    object mdEmployeesAddress_Latitude: TFloatField
      FieldName = 'Address_Latitude'
    end
    object mdEmployeesAddress_Longitude: TFloatField
      FieldName = 'Address_Longitude'
    end
    object mdEmployeesProbationReason_Id: TIntegerField
      FieldName = 'ProbationReason_Id'
    end
    object mdEmployeesPicture: TBlobField
      FieldName = 'Picture'
    end
    object mdEmployeesPrefixByID: TStringField
      FieldKind = fkLookup
      FieldName = 'PrefixByID'
      LookupDataSet = mdPrefix
      LookupKeyFields = 'Prefix_ID'
      LookupResultField = 'Prefix_Name'
      KeyFields = 'Prefix'
      Lookup = True
    end
    object mdEmployeesLevel: TIntegerField
      FieldName = 'Level'
    end
  end
  object mdDepartment: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 127
    Top = 120
    object mdDepartmentDepartment_ID: TAutoIncField
      FieldName = 'Department_ID'
    end
    object mdDepartmentDepartment_Name: TWideStringField
      FieldName = 'Department_Name'
      Size = 25
    end
  end
  object mdStates: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 127
    Top = 64
    object mdStatesID: TIntegerField
      FieldName = 'ID'
    end
    object mdStatesShortName: TStringField
      FieldName = 'ShortName'
      Size = 2
    end
    object mdStatesLongName: TWideStringField
      FieldName = 'LongName'
    end
    object mdStatesFlag48px: TBlobField
      FieldName = 'Flag48px'
    end
    object mdStatesFlag24px: TBlobField
      FieldName = 'Flag24px'
    end
  end
  object dsDepartment: TDataSource
    DataSet = mdDepartment
    Left = 39
    Top = 120
  end
  object dsStates: TDataSource
    DataSet = mdStates
    Left = 39
    Top = 64
  end
  object mdStatus: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000A005374617475735F4944
      000F00000001000C005374617475735F4E616D65000100000000010800000053
      616C61726965640101000000010A000000436F6D6D697373696F6E0102000000
      0108000000436F6E74726163740103000000010A0000005465726D696E617465
      64010400000001080000004F6E204C65617665}
    SortOptions = []
    Left = 127
    Top = 16
    object mdStatusStatus_ID: TIntegerField
      FieldName = 'Status_ID'
    end
    object mdStatusStatus_Name: TStringField
      FieldName = 'Status_Name'
      Size = 15
    end
  end
  object dsStatus: TDataSource
    DataSet = mdStatus
    Left = 39
    Top = 16
  end
  object erRepository: TcxEditRepository
    Left = 208
    Top = 16
    PixelsPerInch = 96
    object erpState: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'LongName'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsStates
    end
    object erpDepartment: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'Department_ID'
      Properties.ListColumns = <
        item
          FieldName = 'Department_Name'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsDepartment
    end
    object erpStatus: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'Status_ID'
      Properties.ListColumns = <
        item
          FieldName = 'Status_Name'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsStatus
    end
    object erpPhoto: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
    object erpHyperLink: TcxEditRepositoryHyperLinkItem
      Properties.Alignment.Horz = taLeftJustify
    end
    object erpMemo: TcxEditRepositoryMemoItem
      Properties.ScrollBars = ssVertical
    end
    object erpFullName: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'Id'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsEmployees
    end
    object erpProgress: TcxEditRepositoryProgressBar
    end
  end
  object srRepository: TcxStyleRepository
    Left = 208
    Top = 72
    PixelsPerInch = 96
    object stPerson: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 536870912
      Font.Height = 20
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
    end
    object stWorkInfo: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 536870912
      Font.Height = 16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
  end
end
