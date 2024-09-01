inherited cxGridWizardDBViewsDataSourcePageFrame: TcxGridWizardDBViewsDataSourcePageFrame
  inherited lcMain: TdxLayoutControl
    AutoSize = True
    object cbDataSource: TcxComboBox [0]
      Left = 28
      Top = 52
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbDataSourcePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 215
    end
    object lbCommon: TcxLabel [1]
      Left = 10
      Top = 10
      Caption = 'Common'
      Style.HotTrack = False
      Transparent = True
    end
    object chbDetail: TcxCheckBox [2]
      Left = 10
      Top = 141
      Caption = 'Is detail view'
      Properties.OnChange = chbDetailPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
    end
    object cbMasterView: TcxComboBox [3]
      Left = 28
      Top = 183
      Enabled = False
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbMasterViewPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 215
    end
    object lbDataSource: TcxLabel [4]
      Left = 28
      Top = 33
      Caption = 'Data Source:'
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbKeyFields: TcxLabel [5]
      Left = 28
      Top = 79
      Caption = 'Key Fields:'
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbMasterView: TcxLabel [6]
      Left = 28
      Top = 164
      Caption = 'Master View:'
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbMasterKeyFields: TcxLabel [7]
      Left = 28
      Top = 210
      Caption = 'Master Key Fields:'
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object lbDetailKeyField: TcxLabel [8]
      Left = 28
      Top = 256
      Caption = 'Detail Key Field:'
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = False
    end
    object cbKeyField: TcxComboBox [9]
      Left = 28
      Top = 98
      Properties.Sorted = True
      Properties.OnChange = cbKeyFieldPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 215
    end
    object cbMasterKeyField: TcxComboBox [10]
      Left = 28
      Top = 229
      Enabled = False
      Properties.Sorted = True
      Properties.OnChange = cbMasterKeyFieldPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Width = 215
    end
    object cbDetailKeyField: TcxComboBox [11]
      Left = 28
      Top = 275
      Enabled = False
      Properties.Sorted = True
      Properties.OnChange = cbDetailKeyFieldPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Width = 215
    end
    object lcDataSourcePageGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lciDataSource: TdxLayoutItem
      Parent = lcgCommonGroup
      AlignHorz = ahLeft
      Control = cbDataSource
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciCommon: TdxLayoutItem
      Parent = lcgCommonCaptionGroup
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbCommon
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciDetail: TdxLayoutItem
      Parent = lcgIsDetailGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbDetail
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgIsDetailGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object lcDataSourcePageGroup3: TdxLayoutGroup
      Parent = lcgIsDetailGroup
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcDataSourcePageSeparatorItem: TdxLayoutSeparatorItem
      Parent = lcDataSourcePageGroup3
      AlignVert = avCenter
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object lciMasterView: TdxLayoutItem
      Parent = lcgDetailGroup
      AlignHorz = ahLeft
      Control = cbMasterView
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgCommonGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 18
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcgDetailGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 18
      ButtonOptions.Buttons = <>
      Enabled = False
      Hidden = True
      ShowBorder = False
      Index = 4
    end
    object lcgCommonCaptionGroup: TdxLayoutGroup
      Parent = lcDataSourcePageGroup1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcDataSourcePageGroup6: TdxLayoutGroup
      Parent = lcgCommonCaptionGroup
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcDataSourcePageSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcDataSourcePageGroup6
      AlignVert = avCenter
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcgCommonGroup
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbDataSource
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcgCommonGroup
      CaptionOptions.Visible = False
      Control = lbKeyFields
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcgDetailGroup
      CaptionOptions.Visible = False
      Control = lbMasterView
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcgDetailGroup
      CaptionOptions.Visible = False
      Control = lbMasterKeyFields
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcgDetailGroup
      CaptionOptions.Visible = False
      Control = lbDetailKeyField
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcMainSpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcDataSourcePageGroup1
      Visible = False
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 2
    end
    object lciKeyField: TdxLayoutItem
      Parent = lcgCommonGroup
      AlignHorz = ahLeft
      Control = cbKeyField
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciMasterKeyField: TdxLayoutItem
      Parent = lcgDetailGroup
      AlignHorz = ahLeft
      Control = cbMasterKeyField
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciDetailKeyField: TdxLayoutItem
      Parent = lcgDetailGroup
      AlignHorz = ahLeft
      Control = cbDetailKeyField
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 5
    end
  end
end
