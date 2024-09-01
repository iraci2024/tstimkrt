object dxHyperlinkDialogForm: TdxHyperlinkDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Insert Hyperlink'
  ClientHeight = 335
  ClientWidth = 621
  Color = clBtnFace
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
    Width = 621
    Height = 335
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    HighlightRoot = False
    object btnOK: TcxButton
      Left = 214
      Top = 80
      Width = 85
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TcxButton
      Left = 305
      Top = 80
      Width = 85
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object edtAddress: TcxButtonEdit
      Left = 90
      Top = 53
      Properties.Buttons = <>
      Style.HotTrack = False
      TabOrder = 1
      Width = 300
    end
    object edtTextToDisplay: TcxTextEdit
      Left = 90
      Top = 26
      Properties.ReadOnly = False
      Properties.UseNullString = True
      Style.HotTrack = False
      TabOrder = 0
      Width = 300
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      ButtonOptions.Buttons = <>
      Hidden = True
      Padding.AssignedValues = [lpavBottom]
      ShowBorder = False
      Index = -1
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lcMainSpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 0
    end
    object lcMainGroup14: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcbtnOK: TdxLayoutItem
      Parent = lcMainGroup14
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Ok'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcbtnCancel: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcEditAddress: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Address:'
      Control = edtAddress
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcEditTextToDisplay: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = '&Text to display:'
      Control = edtTextToDisplay
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 424
    Top = 232
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
