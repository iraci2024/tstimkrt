inherited dxRichEditFontDialogForm: TdxRichEditFontDialogForm
  Caption = 'Font'
  ClientHeight = 465
  ClientWidth = 457
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 457
    Height = 465
    object cmbFontColor: TdxColorEdit [0]
      Left = 10
      Top = 168
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 6
      Width = 136
    end
    object cmbUnderlineStyle: TcxComboBox [1]
      Left = 152
      Top = 168
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 7
      Width = 106
    end
    object cmbUnderlineColor: TdxColorEdit [2]
      Left = 264
      Top = 168
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 8
      Width = 136
    end
    object btnCancel: TcxButton [3]
      Left = 325
      Top = 414
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 11
    end
    object btnOk: TcxButton [4]
      Left = 244
      Top = 414
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 10
    end
    object cmbFontName: TcxTextEdit [5]
      Left = 10
      Top = 28
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 21
      Width = 191
    end
    object cmbFontSize: TcxTextEdit [6]
      Left = 334
      Top = 28
      AutoSize = False
      Properties.ValidationOptions = [evoShowErrorIcon, evoAllowLoseFocus]
      Style.TransparentBorder = False
      TabOrder = 4
      OnKeyPress = cmbFontSizeKeyPress
      Height = 21
      Width = 66
    end
    object cmbFontStyle: TcxTextEdit [7]
      Left = 207
      Top = 28
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Height = 21
      Width = 121
    end
    object lbFontName: TcxListBox [8]
      Left = 10
      Top = 49
      Width = 191
      Height = 95
      TabStop = False
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
    end
    object lbFontStyle: TcxListBox [9]
      Left = 207
      Top = 49
      Width = 121
      Height = 95
      TabStop = False
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 3
    end
    object lbFontSize: TcxListBox [10]
      Left = 334
      Top = 49
      Width = 66
      Height = 95
      TabStop = False
      ItemHeight = 13
      Items.Strings = (
        '8'
        '9'
        '10'
        '11'
        '12'
        '14'
        '16'
        '18'
        '20'
        '22'
        '24'
        '26'
        '28'
        '36'
        '48'
        '72')
      Style.TransparentBorder = False
      TabOrder = 5
    end
    object srePreview: TdxSimpleRichEditControl [11]
      Left = 11
      Top = 305
      Width = 388
      Height = 64
      BorderStyle = cxcbsNone
      TabOrder = 9
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciFontColor: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Font Color:'
      CaptionOptions.Layout = clTop
      Control = cmbFontColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciUnderlineStyle: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignVert = avTop
      CaptionOptions.Text = 'Underline style:'
      CaptionOptions.Layout = clTop
      Control = cmbUnderlineStyle
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 106
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciUnderlineColor: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignVert = avTop
      CaptionOptions.Text = 'Underline color:'
      CaptionOptions.Layout = clTop
      Control = cmbUnderlineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 2
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Group5: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 10
      SizeOptions.Width = 84
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group6: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.Width = 84
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group7: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.Width = 84
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Button2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Button1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblEffects: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Effects'
      CaptionOptions.Visible = True
      Index = 2
    end
    object dxLayoutControl1Group8: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group9: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group10: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object lciFontName: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      CaptionOptions.Text = 'Font:'
      CaptionOptions.Layout = clTop
      Control = cmbFontName
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFontSize: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      AlignVert = avTop
      CaptionOptions.Text = 'Size:'
      CaptionOptions.Layout = clTop
      Control = cmbFontSize
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 66
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFontStyle: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      AlignVert = avTop
      CaptionOptions.Text = 'Font style:'
      CaptionOptions.Layout = clTop
      Control = cmbFontStyle
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbFontName
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item11: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      CaptionOptions.Text = 'cxListBox2'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbFontStyle
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item12: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      CaptionOptions.Text = 'cxListBox3'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbFontSize
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 66
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPreview: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Preview'
      CaptionOptions.Visible = True
      Index = 4
    end
    object dxLayoutControl1Item13: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'dxSimpleRichEditControl1'
      CaptionOptions.Visible = False
      Control = srePreview
      ControlOptions.OriginalHeight = 64
      ControlOptions.OriginalWidth = 388
      Enabled = False
      Index = 5
    end
    object liFontNameWarning: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      SizeOptions.Height = 32
      SizeOptions.Width = 388
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.VisibleElements = [cveText]
      CaptionOptions.WordWrap = True
      Index = 6
    end
    object cbStrikethrough: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'Stri&kethrough'
      Index = 0
    end
    object cbDoubleStrikethrough: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'Double strikethrou&gh'
      Index = 1
    end
    object cbUnderlineWordsOnly: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = '&Underline words only'
      Index = 2
    end
    object cbSuperscript: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group6
      CaptionOptions.Text = 'Su&perscript'
      Index = 0
    end
    object cbSubscript: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group6
      CaptionOptions.Text = 'Su&bscript'
      Index = 1
    end
    object cbAllCaps: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group7
      CaptionOptions.Text = '&All caps'
      Index = 0
    end
    object cbHidden: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group7
      CaptionOptions.Text = '&Hidden'
      Index = 1
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      Offsets.ItemOffset = 0
      PixelsPerInch = 96
    end
  end
end
