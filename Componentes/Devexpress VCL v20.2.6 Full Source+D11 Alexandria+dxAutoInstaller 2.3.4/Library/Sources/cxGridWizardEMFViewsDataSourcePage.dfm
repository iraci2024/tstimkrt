inherited cxGridWizardEMFViewsDataSourcePageFrame: TcxGridWizardEMFViewsDataSourcePageFrame
  inherited lcMain: TdxLayoutControl
    AutoSize = True
    CustomizeFormTabbedView = True
    object lbCommon: TcxLabel [0]
      Left = 10
      Top = 10
      Caption = 'Common'
      Style.HotTrack = False
      Transparent = True
    end
    object chbDetail: TcxCheckBox [1]
      Left = 10
      Top = 94
      Caption = 'Is detail view'
      Properties.OnChange = chbDetailPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
    end
    object cbMasterView: TcxComboBox [2]
      Left = 28
      Top = 135
      Enabled = False
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = cbMasterViewPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 215
    end
    object cbDataSource: TcxComboBox [3]
      Left = 28
      Top = 51
      Properties.DropDownListStyle = lsFixedList
      Properties.ValidationErrorIconAlignment = taRightJustify
      Properties.ValidationOptions = [evoShowErrorIcon, evoAllowLoseFocus]
      Properties.OnEditValueChanged = cbDataSourcePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 215
    end
    object cbCollection: TcxComboBox [4]
      Left = 303
      Top = 203
      Enabled = False
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = cbCollectionPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Width = 215
    end
    object rbQueryMode: TcxRadioButton [5]
      Left = 28
      Top = 162
      Caption = 'Query mode'
      Enabled = False
      TabOrder = 4
      OnClick = rbQueryModeClick
      AutoSize = True
    end
    object rbInMemoryMode: TcxRadioButton [6]
      Left = 285
      Top = 162
      Caption = 'In-memory mode'
      Enabled = False
      TabOrder = 7
      OnClick = rbQueryModeClick
      AutoSize = True
    end
    object cbMasterKeyFields: TcxComboBox [7]
      Left = 28
      Top = 203
      Enabled = False
      Properties.Sorted = True
      Properties.OnEditValueChanged = cbMasterKeyFieldsPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Width = 215
    end
    object cbDetailKeyFields: TcxComboBox [8]
      Left = 28
      Top = 248
      Enabled = False
      Properties.Sorted = True
      Properties.OnEditValueChanged = cbDetailKeyFieldsPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Width = 215
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 2
    end
    object liCommon: TdxLayoutItem
      Parent = lgCommonCaption
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbCommon
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liDetail: TdxLayoutItem
      Parent = lgIsDetail
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chbDetail
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgIsDetail: TdxLayoutGroup
      Parent = lgDetailSettings
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgDetailSeparator: TdxLayoutGroup
      Parent = lgIsDetail
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lsiDetail: TdxLayoutSeparatorItem
      Parent = lgDetailSeparator
      AlignVert = avCenter
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object licbMasterView: TdxLayoutItem
      Parent = lgDetail
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Master View:'
      CaptionOptions.Layout = clTop
      Control = cbMasterView
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgDetail: TdxLayoutGroup
      Parent = lgDetailSettings
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 18
      ButtonOptions.Buttons = <>
      Enabled = False
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object lgCommonCaption: TdxLayoutGroup
      Parent = lgCommonSettings
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgCommonSeparator: TdxLayoutGroup
      Parent = lgCommonCaption
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lsiCommon: TdxLayoutSeparatorItem
      Parent = lgCommonSeparator
      AlignVert = avCenter
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object lesiDetailSettings: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      Visible = False
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 1
    end
    object lgDetailSettings: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 2
    end
    object lgCommonSettings: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object licbDataSource: TdxLayoutItem
      Parent = lgCommonSettings
      AlignHorz = ahLeft
      Offsets.Left = 18
      CaptionOptions.Text = 'Data Source:'
      CaptionOptions.Layout = clTop
      Control = cbDataSource
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lgKeyFields: TdxLayoutGroup
      Parent = lgQueryMode
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object licbCollection: TdxLayoutItem
      Parent = lgInMemoryMode
      AlignHorz = ahLeft
      Offsets.Left = 18
      CaptionOptions.Text = 'Collection:'
      CaptionOptions.Layout = clTop
      Control = cbCollection
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lgModeSettings: TdxLayoutGroup
      Parent = lgDetail
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lgQueryMode: TdxLayoutGroup
      Parent = lgModeSettings
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object lgInMemoryMode: TdxLayoutGroup
      Parent = lgModeSettings
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 36
      Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgQueryMode
      Visible = False
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbQueryMode
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = lgInMemoryMode
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbInMemoryMode
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object licbMasterKeyFields: TdxLayoutItem
      Parent = lgKeyFields
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Master Key Fields:'
      CaptionOptions.Layout = clTop
      Control = cbMasterKeyFields
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object licbDetailKeyFields: TdxLayoutItem
      Parent = lgKeyFields
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Detail Key Fields:'
      CaptionOptions.Layout = clTop
      Control = cbDetailKeyFields
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 215
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
