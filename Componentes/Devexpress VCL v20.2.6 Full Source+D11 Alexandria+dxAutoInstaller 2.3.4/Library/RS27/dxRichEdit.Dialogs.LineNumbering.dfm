inherited dxRichEditLineNumberingDialogForm: TdxRichEditLineNumberingDialogForm
  Caption = 'Line Numbers'
  ClientHeight = 245
  ClientWidth = 214
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 214
    Height = 245
    object edtStartAt: TcxSpinEdit [0]
      Left = 93
      Top = 33
      Properties.MaxValue = 32767.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidateOnEnter = False
      Properties.ValidationOptions = [evoAllowLoseFocus]
      Properties.OnValidate = edtStartAtPropertiesValidate
      TabOrder = 0
      Value = 1
      Width = 73
    end
    object edtCountBy: TcxSpinEdit [1]
      Left = 93
      Top = 60
      Properties.MaxValue = 100.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidateOnEnter = False
      Properties.ValidationOptions = [evoAllowLoseFocus]
      Properties.OnValidate = edtStartAtPropertiesValidate
      TabOrder = 1
      Value = 1
      Width = 73
    end
    object edtFromText: TdxMeasurementUnitEdit [2]
      Left = 93
      Top = 87
      Properties.ValidationOptions = [evoRaiseException, evoAllowLoseFocus]
      TabOrder = 2
      Width = 73
    end
    object btnOk: TcxButton [3]
      Left = 10
      Top = 203
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
    end
    object btnCancel: TcxButton [4]
      Left = 91
      Top = 203
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      ItemIndex = 1
    end
    object lcgControlsGroup: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 20
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object lciStartAt: TdxLayoutItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'Start &at:'
      Control = edtStartAt
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCountBy: TdxLayoutItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'Count &by:'
      Control = edtCountBy
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciFromText: TdxLayoutItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'From &text:'
      Control = edtFromText
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblNumbering: TdxLayoutLabeledItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'Numbering:'
      Index = 3
    end
    object cbAddLineNumbering: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Add &line numbering'
      Index = 0
    end
    object rbNumberingRestartEachPage: TdxLayoutRadioButtonItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'Restart each &page'
      Index = 0
    end
    object rbNumberingRestartEachSection: TdxLayoutRadioButtonItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'Restart each &section'
      Index = 1
    end
    object rbNumberingRestartContinuous: TdxLayoutRadioButtonItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = '&Continuous'
      Index = 2
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
