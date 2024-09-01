object frmCustomAnnotationSettings: TfrmCustomAnnotationSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Custom Annotation Settings'
  ClientHeight = 365
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 8
    Top = 8
    Width = 337
    Height = 313
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cxButton2: TcxButton
      Left = 192
      Top = 217
      Width = 75
      Height = 25
      Caption = '&Close'
      Default = True
      TabOrder = 0
      OnClick = cxButton2Click
    end
    object seMinHeight: TcxSpinEdit
      Left = 83
      Top = 106
      Properties.AssignedValues.MinValue = True
      Properties.OnEditValueChanged = seMinHeightPropertiesEditValueChanged
      Style.Edges = []
      Style.HotTrack = False
      Style.StyleController = cxEditStyleController1
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 171
    end
    object seMaxHeight: TcxSpinEdit
      Left = 83
      Top = 131
      Properties.AssignedValues.MinValue = True
      Properties.OnEditValueChanged = seMaxHeightPropertiesEditValueChanged
      Style.Edges = []
      Style.HotTrack = False
      Style.StyleController = cxEditStyleController1
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 171
    end
    object seWidth: TcxSpinEdit
      Left = 83
      Top = 156
      Properties.AssignedValues.MinValue = True
      Properties.OnEditValueChanged = seWidthPropertiesEditValueChanged
      Style.Edges = []
      Style.HotTrack = False
      Style.StyleController = cxEditStyleController1
      Style.TransparentBorder = False
      TabOrder = 5
      Width = 171
    end
    object seOffset: TcxSpinEdit
      Left = 83
      Top = 181
      Properties.OnEditValueChanged = seOffsetPropertiesEditValueChanged
      Style.Edges = []
      Style.HotTrack = False
      Style.Shadow = False
      Style.StyleController = cxEditStyleController1
      Style.TransparentBorder = False
      TabOrder = 6
      Width = 171
    end
    object cbAlignment: TcxComboBox
      Left = 83
      Top = 81
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbAlignmentPropertiesChange
      Style.Edges = []
      Style.HotTrack = False
      Style.StyleController = cxEditStyleController1
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 171
    end
    object pbColor: TPaintBox
      Left = 83
      Top = 54
      Width = 171
      Height = 19
      Color = clBtnFace
      ParentColor = False
      OnClick = cxButton3Click
      OnPaint = pbColorPaint
    end
    object cbAnnotationKind: TcxComboBox
      Left = 73
      Top = 11
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbAnnotationKindPropertiesChange
      Style.HotTrack = False
      Style.StyleController = cxEditStyleController1
      TabOrder = 1
      Width = 193
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = cxButton2
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Alignment:'
      Control = cbAlignment
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'MinHeight:'
      Control = seMinHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'MaxHeight:'
      Control = seMaxHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      Index = 3
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Width:'
      Control = seWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      Index = 4
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Offset:'
      Control = seOffset
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      Index = 5
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Color:'
      Control = pbColor
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 105
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Annotation:'
      Control = cbAnnotationKind
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 193
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Style'
      ButtonOptions.Buttons = <>
      Index = 2
    end
  end
  object dxColorDialog1: TdxColorDialog
    Left = 200
    Top = 72
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 208
    Top = 144
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object cxEditStyleController1: TcxEditStyleController
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    Left = 320
    Top = 88
    PixelsPerInch = 96
  end
end
