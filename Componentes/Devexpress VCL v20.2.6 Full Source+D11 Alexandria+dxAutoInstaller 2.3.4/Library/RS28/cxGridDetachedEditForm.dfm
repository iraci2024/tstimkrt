object cxGridDetachedEditForm: TcxGridDetachedEditForm
  Left = 0
  Top = 0
  ClientHeight = 121
  ClientWidth = 234
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
  object lcMainLayout: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 234
    Height = 121
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = llfMain
    object btnUpdate: TcxButton
      Left = 54
      Top = 88
      Width = 76
      Height = 23
      Caption = 'Update'
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TcxButton
      Left = 145
      Top = 88
      Width = 76
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object lcContentLayout: TdxLayoutControl
      Left = 0
      Top = 0
      Width = 234
      Height = 75
      TabOrder = 0
      LayoutLookAndFeel = llfContent
      object lgContentRoot: TdxLayoutGroup
        AlignHorz = ahLeft
        AlignVert = avTop
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = -1
      end
    end
    object lgRoot: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      UseIndent = False
      Index = -1
    end
    object lgPanel: TdxLayoutGroup
      Parent = lgRoot
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'lgPanel'
      CaptionOptions.Visible = False
      Offsets.Bottom = 10
      Offsets.Left = 13
      Offsets.Right = 13
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object liUpdate: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      Offsets.Right = 9
      SizeOptions.Height = 23
      SizeOptions.Width = 76
      CaptionOptions.Text = 'liUpdate'
      CaptionOptions.Visible = False
      Control = btnUpdate
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 76
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liCancel: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      SizeOptions.Height = 23
      SizeOptions.Width = 76
      CaptionOptions.Text = 'liCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 76
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lsiButtonsSeparator: TdxLayoutSeparatorItem
      Parent = lgPanel
      CaptionOptions.Text = 'lsiButtonsSeparator'
      Index = 1
    end
    object liContentLayout: TdxLayoutItem
      Parent = lgRoot
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'liContentLayout'
      CaptionOptions.Visible = False
      Control = lcContentLayout
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgButtons: TdxLayoutGroup
      Parent = lgPanel
      AlignVert = avBottom
      CaptionOptions.Text = 'lgButtons'
      CaptionOptions.Visible = False
      Offsets.Top = 1
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
  end
  object llflLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 72
    Top = 24
    object llfMain: TdxLayoutCxLookAndFeel
      Offsets.RootItemsAreaOffsetHorz = 0
      Offsets.RootItemsAreaOffsetVert = 0
      PixelsPerInch = 96
    end
    object llfContent: TdxLayoutCxLookAndFeel
      Offsets.RootItemsAreaOffsetHorz = 13
      Offsets.RootItemsAreaOffsetVert = 10
      PixelsPerInch = 96
    end
  end
end
