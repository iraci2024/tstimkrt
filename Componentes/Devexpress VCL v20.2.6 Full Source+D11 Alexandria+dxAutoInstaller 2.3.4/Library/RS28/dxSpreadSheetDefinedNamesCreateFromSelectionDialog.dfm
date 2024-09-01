object dxSpreadSheetCreateDefinedNamesFromSelectionDialogForm: TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  ClientHeight = 171
  ClientWidth = 374
  Color = clBtnFace
  Constraints.MinWidth = 380
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 374
    Height = 171
    Align = alTop
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnOK: TcxButton
      Left = 208
      Top = 136
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TcxButton
      Left = 289
      Top = 136
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object lbHeader: TcxLabel
      Left = 10
      Top = 10
      AutoSize = False
      Caption = 'lbHeader'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 16
      Width = 354
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 5
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Visible = False
      Control = lbHeader
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciBottom: TdxLayoutCheckBoxItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Check Box'
      Index = 3
    end
    object lciRight: TdxLayoutCheckBoxItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Check Box'
      Index = 4
    end
    object lciLeft: TdxLayoutCheckBoxItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Check Box'
      Index = 2
    end
    object lciTop: TdxLayoutCheckBoxItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Check Box'
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 312
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
