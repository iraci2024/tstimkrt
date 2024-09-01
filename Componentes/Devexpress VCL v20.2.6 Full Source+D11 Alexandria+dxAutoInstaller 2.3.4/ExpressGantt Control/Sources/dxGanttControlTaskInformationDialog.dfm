object dxGanttControlTaskInformationDialogForm: TdxGanttControlTaskInformationDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Task Information'
  ClientHeight = 585
  ClientWidth = 769
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 769
    Height = 585
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    CustomizeFormTabbedView = True
    object meDuration: TdxMeasurementUnitEdit
      Left = 449
      Top = 10
      Properties.ValidationOptions = [evoAllowLoseFocus]
      Properties.OnChange = meDurationPropertiesChange
      Properties.OnEditValueChanged = meDurationPropertiesEditValueChanged
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 69
    end
    object btnDelete: TcxButton
      Left = 10
      Top = 216
      Width = 75
      Height = 25
      Caption = '&Delete'
      TabOrder = 9
      OnClick = btnDeleteClick
    end
    object btnOk: TcxButton
      Left = 434
      Top = 216
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 10
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton
      Left = 515
      Top = 216
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 11
      OnClick = btnOkClick
    end
    object edName: TcxTextEdit
      Left = 42
      Top = 10
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Text = 'edName'
      Width = 355
    end
    object mePercentComplete: TdxMeasurementUnitEdit
      Left = 108
      Top = 78
      Properties.ValidationOptions = [evoAllowLoseFocus]
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 121
    end
    object cmbScheduleMode: TcxComboBox
      Left = 108
      Top = 103
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbScheduleModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 121
    end
    object deDateStart: TcxDateEdit
      Left = 61
      Top = 146
      Properties.ImmediatePost = True
      Properties.OnCloseUp = deDateStartPropertiesCloseUp
      Properties.OnEditValueChanged = deDateStartPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 233
    end
    object deDateFinish: TcxDateEdit
      Left = 332
      Top = 146
      Properties.ImmediatePost = True
      Properties.OnCloseUp = deDateFinishPropertiesCloseUp
      Properties.OnEditValueChanged = deDateFinishPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Width = 236
    end
    object cmbConstraintType: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = cmbConstraintTypePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Visible = False
      Width = 197
    end
    object deConstraintDate: TcxDateEdit
      Left = 10000
      Top = 10000
      Properties.ImmediatePost = True
      Properties.OnCloseUp = deConstraintDatePropertiesCloseUp
      Properties.OnEditValueChanged = deConstraintDatePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Visible = False
      Width = 197
    end
    object cmbCalendar: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Visible = False
      Width = 173
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object liDuration: TdxLayoutItem
      Parent = lgTogGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Duration'
      Control = meDuration
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 69
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liBtnDelete: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liBtnOk: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liBtnCancel: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lgButtons: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object liName: TdxLayoutItem
      Parent = lgTogGroup
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Name'
      Control = edName
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 355
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liEstimated: TdxLayoutCheckBoxItem
      Parent = lgTogGroup
      AlignVert = avClient
      CaptionOptions.Text = 'Estimated'
      OnClick = liEstimatedClick
      Index = 2
    end
    object liPercentComplete: TdxLayoutItem
      Parent = lgGeneralGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Percent complete'
      Control = mePercentComplete
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liScheduleMode: TdxLayoutItem
      Parent = lgGeneralGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Schedule Mode'
      Control = cmbScheduleMode
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lgGeneralGroup: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'Main General'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lgDates: TdxLayoutGroup
      Parent = lgGeneralGroup
      CaptionOptions.Text = 'Dates'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object liDateStart: TdxLayoutItem
      Parent = lgDates
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Start'
      Control = deDateStart
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liDateFinish: TdxLayoutItem
      Parent = lgDates
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Finish'
      Control = deDateFinish
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liConstraintType: TdxLayoutItem
      Parent = lgConstrainTask
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Constraint type'
      Control = cmbConstraintType
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liConstraintDate: TdxLayoutItem
      Parent = lgConstrainTask
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Constraint date'
      Control = deConstraintDate
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lgAdvancedGroup: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'Main Advanced'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object lgConstrainTask: TdxLayoutGroup
      Parent = lgAdvancedGroup
      CaptionOptions.Text = 'Constrain Task'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgTogGroup: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgTabs: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Tabs'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 1
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lgAdvancedGroup
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
    object liCalendar: TdxLayoutItem
      Parent = lgAdvancedGroup
      AlignHorz = ahLeft
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 253
      CaptionOptions.Text = 'Calendar'
      Control = cmbCalendar
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liDisplayOnTimeline: TdxLayoutCheckBoxItem
      Parent = lgGeneralGroup
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Display on Timeline'
      Index = 3
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 512
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
