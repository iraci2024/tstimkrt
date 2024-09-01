object DemoBasicMainForm: TDemoBasicMainForm
  Left = 239
  Top = 187
  Caption = 'ExpressGanttControll Demo'
  ClientHeight = 680
  ClientWidth = 1184
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    TabOrder = 0
    Height = 680
    Width = 1184
    object lbDescription: TLabel
      Left = 2
      Top = 2
      Width = 1180
      Height = 16
      Align = alTop
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      WordWrap = True
    end
    object lcMain: TdxLayoutControl
      Left = 2
      Top = 18
      Width = 1180
      Height = 660
      Align = alClient
      TabOrder = 0
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      object GanttControl: TdxGanttControl
        Left = 10
        Top = 10
        Width = 907
        Height = 640
        TabOrder = 4
        ViewChart.Active = True
        ViewChart.OptionsSheet.Columns.Items = <
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnIndicator'
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnTaskMode'
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnTaskName'
            Width = 218
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnTaskDuration'
            Width = 72
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnTaskStart'
            Width = 96
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnTaskFinish'
            Width = 102
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnPredecessors'
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnResourceName'
            Width = 90
          end
          item
            ItemClass = 'TdxGanttControlViewChartSheetColumnPercentComplete'
          end>
        ViewChart.OptionsSheet.Width = 717
        ViewResourceSheet.OptionsSheet.Columns.Items = <
          item
            ItemClass = 'TdxGanttControlResourceSheetColumnName'
          end
          item
            ItemClass = 'TdxGanttControlResourceSheetColumnType'
          end
          item
            ItemClass = 'TdxGanttControlResourceSheetColumnGroup'
          end
          item
            ItemClass = 'TdxGanttControlResourceSheetColumnBaseCalendar'
          end>
        OnActiveViewChanged = GanttControlActiveViewChanged
        OnDataModelLoaded = GanttControlDataModelLoaded
        OnAssignmentChanged = GanttControlAssignmentChanged
        OnAssignmentDeleted = GanttControlAssignmentChanged
        OnAssignmentInserted = GanttControlAssignmentChanged
        OnResourceChanged = GanttControlResourceChanged
        OnResourceDeleted = GanttControlResourceChanged
        OnResourceInserted = GanttControlResourceChanged
        OnTaskChanged = GanttControlTaskChanged
        OnTaskDeleted = GanttControlTaskChanged
        OnTaskInserted = GanttControlTaskChanged
      end
      object seTimelineUnitMinWidth: TcxSpinEdit
        Left = 10000
        Top = 10000
        Properties.ImmediatePost = True
        Properties.Increment = 10.000000000000000000
        Properties.MaxValue = 100000.000000000000000000
        Properties.MinValue = 10.000000000000000000
        Properties.OnChange = seTimelineUnitMinWidthPropertiesChange
        Style.HotTrack = False
        Style.TransparentBorder = False
        TabOrder = 2
        Value = 50
        Visible = False
        Width = 177
      end
      object cmbTimelineScale: TcxComboBox
        Left = 10000
        Top = 10000
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'Automatic'
          'Hours'
          'Days'
          'Weeks'
          'Months'
          'Quarters'
          'Years')
        Properties.OnChange = cmbTimelineScalePropertiesChange
        Style.HotTrack = False
        Style.TransparentBorder = False
        TabOrder = 1
        Text = 'Automatic'
        Visible = False
        Width = 177
      end
      object cbSrcrollBars: TcxComboBox
        Left = 947
        Top = 347
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'Default'
          'Touch'
          'Classic'
          'Hybrid')
        Properties.OnEditValueChanged = cbSrcrollBarsPropertiesEditValueChanged
        Style.HotTrack = False
        Style.TransparentBorder = False
        TabOrder = 3
        Text = 'Default'
        Width = 199
      end
      object cmbChartTimescale: TcxComboBox
        Left = 957
        Top = 98
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'Hours'
          'Quarter Days'
          'Days'
          'Weeks'
          'Thirds of Months'
          'Months'
          'Quarters'
          'Half Years'
          'Years')
        Properties.OnChange = cmbChartTimescalePropertiesChange
        Style.HotTrack = False
        Style.TransparentBorder = False
        TabOrder = 0
        Text = 'Days'
        Width = 179
      end
      object lcMainGroup_Root: TdxLayoutGroup
        AlignHorz = ahClient
        AlignVert = avClient
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = -1
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'dxGanttControl1'
        CaptionOptions.Visible = False
        Control = GanttControl
        ControlOptions.OriginalHeight = 250
        ControlOptions.OriginalWidth = 350
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutGroup1: TdxLayoutGroup
        Parent = lcMainGroup_Root
        AlignHorz = ahClient
        AlignVert = avClient
        CaptionOptions.Text = 'New Group'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 0
      end
      object lgOptions: TdxLayoutGroup
        Parent = dxLayoutGroup1
        AlignHorz = ahRight
        AlignVert = avClient
        CaptionOptions.Text = 'Options'
        ButtonOptions.Buttons = <>
        Index = 0
      end
      object lgActiveView: TdxLayoutGroup
        Parent = lgOptions
        CaptionOptions.Text = 'Active View'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldTabbed
        OnTabChanged = lgActiveViewTabChanged
        Index = 0
      end
      object lchbDeleteConfirm: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup7
        CaptionOptions.Text = 'Delete Confirmation'
        OnClick = lchbDeleteConfirmClick
        Index = 0
      end
      object lchbDirectX: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup7
        CaptionOptions.Text = 'DirectX Hardware Acceleration'
        OnClick = lchbDirectXClick
        Index = 1
      end
      object lchbChartCellAutoHeight: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup5
        CaptionOptions.Text = 'Cell Auto Height'
        State = cbsChecked
        OnClick = lchbChartCellAutoHeightClick
        Index = 0
      end
      object lchbChartColumnHide: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup5
        CaptionOptions.Text = 'Column Hide'
        OnClick = lchbChartCellAutoHeightClick
        Index = 1
      end
      object lchbChartColumnMove: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup5
        CaptionOptions.Text = 'Column Move'
        State = cbsChecked
        OnClick = lchbChartCellAutoHeightClick
        Index = 2
      end
      object lchbChartColumnSize: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup5
        CaptionOptions.Text = 'Column Size'
        State = cbsChecked
        OnClick = lchbChartCellAutoHeightClick
        Index = 3
      end
      object lchbChartColumnQuickCustomization: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup5
        CaptionOptions.Text = 'Column Quick Customization'
        OnClick = lchbChartCellAutoHeightClick
        Index = 4
      end
      object lchbChartVisible: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup5
        CaptionOptions.Text = 'Visible'
        State = cbsChecked
        OnClick = lchbChartVisibleClick
        Index = 5
      end
      object dxLayoutGroup2: TdxLayoutGroup
        Parent = lgActiveView
        CaptionOptions.Text = 'Resource Sheet'
        ButtonOptions.Buttons = <>
        ItemIndex = 4
        Index = 1
      end
      object dxLayoutGroup3: TdxLayoutGroup
        Parent = lgActiveView
        CaptionOptions.Text = 'Chart'
        ButtonOptions.Buttons = <>
        Index = 0
      end
      object dxLayoutGroup4: TdxLayoutGroup
        Parent = lgActiveView
        CaptionOptions.Text = 'Timeline'
        ButtonOptions.Buttons = <>
        Index = 2
      end
      object dxLayoutGroup5: TdxLayoutGroup
        Parent = dxLayoutGroup3
        CaptionOptions.Text = 'Sheet Options'
        ButtonOptions.Buttons = <>
        ItemIndex = 5
        Index = 2
      end
      object lchbResourceSheetCellAutoHeight: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup2
        CaptionOptions.Text = 'Cell Auto Hieght'
        State = cbsChecked
        OnClick = lchbResourceSheetCellAutoHeightClick
        Index = 0
      end
      object lchbResourceSheetColumnHide: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup2
        CaptionOptions.Text = 'Column Hide'
        OnClick = lchbResourceSheetCellAutoHeightClick
        Index = 1
      end
      object lchbResourceSheetColumnMove: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup2
        CaptionOptions.Text = 'Column Move'
        State = cbsChecked
        OnClick = lchbResourceSheetCellAutoHeightClick
        Index = 2
      end
      object lchbResourceSheetColumnSize: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup2
        CaptionOptions.Text = 'Column Size'
        State = cbsChecked
        OnClick = lchbResourceSheetCellAutoHeightClick
        Index = 3
      end
      object lchbResourceSheetColumnQuickCustomization: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup2
        AlignHorz = ahLeft
        CaptionOptions.Text = 'Column Quick Customization'
        OnClick = lchbResourceSheetCellAutoHeightClick
        Index = 4
      end
      object lchbShowOnlyExplicitlyAddedTasks: TdxLayoutCheckBoxItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Only Show Explicitly Added Tasks'
        OnClick = lchbShowOnlyExplicitlyAddedTasksClick
        Index = 0
      end
      object dxLayoutItem4: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Min Width of Timescale Unit'
        CaptionOptions.Layout = clTop
        Control = seTimelineUnitMinWidth
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 3
      end
      object dxLayoutItem3: TdxLayoutItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Timescale'
        CaptionOptions.Layout = clTop
        Control = cmbTimelineScale
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 2
      end
      object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Separator'
        Index = 1
      end
      object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
        Parent = dxLayoutGroup3
        SizeOptions.Height = 10
        SizeOptions.Width = 10
        CaptionOptions.Text = 'Empty Space Item'
        Index = 1
      end
      object dxLayoutItem5: TdxLayoutItem
        Parent = dxLayoutGroup6
        Control = cbSrcrollBars
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutGroup6: TdxLayoutGroup
        Parent = lgOptions
        CaptionOptions.Text = 'Scrollbars'
        ButtonOptions.Buttons = <>
        Index = 1
      end
      object dxLayoutGroup7: TdxLayoutGroup
        Parent = lgOptions
        CaptionOptions.Text = 'Common'
        ButtonOptions.Buttons = <>
        ItemIndex = 1
        Index = 2
      end
      object dxLayoutItem2: TdxLayoutItem
        Parent = dxLayoutGroup3
        CaptionOptions.Text = 'Timescale'
        CaptionOptions.Layout = clTop
        Control = cmbChartTimescale
        ControlOptions.OriginalHeight = 19
        ControlOptions.OriginalWidth = 121
        ControlOptions.ShowBorder = False
        Index = 0
      end
    end
  end
  object mmMain: TMainMenu
    Left = 312
    Top = 8
    object miFile: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = 'New'
        ShortCut = 16462
        OnClick = New1Click
      end
      object Separator2: TMenuItem
        Caption = '-'
      end
      object Import1: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = Import1Click
      end
      object Export1: TMenuItem
        Caption = 'Save As...'
        ShortCut = 123
        OnClick = Export1Click
      end
      object Separator1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'E&xit'
        Hint = 'Press to quit the demo-program'
        ShortCut = 32856
        OnClick = FileExitExecute
      end
    end
    object miAbout: TMenuItem
      Caption = '&About this Demo'
      Hint = 'Displays the brief description of the current demo features'
      OnClick = miAboutClick
    end
  end
  object lfController: TcxLookAndFeelController
    Left = 536
    Top = 88
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files|*.xml'
    Left = 312
    Top = 128
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML Files|*.xml'
    Left = 312
    Top = 72
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 536
    Top = 144
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
