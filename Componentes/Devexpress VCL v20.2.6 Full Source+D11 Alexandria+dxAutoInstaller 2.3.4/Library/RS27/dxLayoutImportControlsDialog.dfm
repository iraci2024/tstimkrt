object fmImportControlsDialog: TfmImportControlsDialog
  Left = 143
  Top = 181
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Import'
  ClientHeight = 257
  ClientWidth = 569
  Color = clBtnFace
  Font.Height = -11
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 569
    Height = 257
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel2
    object seDeltaX: TcxSpinEdit
      Left = 45
      Top = 190
      Properties.MaxValue = 255.000000000000000000
      Properties.MinValue = 1.000000000000000000
      TabOrder = 1
      Value = 1
      Width = 51
    end
    object seDeltaY: TcxSpinEdit
      Left = 194
      Top = 190
      Properties.MaxValue = 255.000000000000000000
      Properties.MinValue = 1.000000000000000000
      TabOrder = 2
      Value = 1
      Width = 51
    end
    object btnImport: TcxButton
      Left = 121
      Top = 217
      Width = 97
      Height = 25
      Caption = '&Import'
      ModalResult = 1
      TabOrder = 3
    end
    object btnCancel: TcxButton
      Left = 224
      Top = 217
      Width = 97
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object cbContainers: TcxComboBox
      Left = 10
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Properties.DropDownRows = 16
      TabOrder = 0
      Width = 311
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Choose a control to import data from:'
      CaptionOptions.Layout = clTop
      Control = cbContainers
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 311
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group5: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      CaptionOptions.Text = 'Hidden Group'
      Offsets.Left = 20
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 6
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'X:'
      Control = seDeltaX
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 51
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1LabeledItem1: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group5
      AlignVert = avCenter
      CaptionOptions.Text = 'pixels wide OR'
      Index = 1
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Y:'
      Control = seDeltaY
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 51
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1LabeledItem2: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group5
      AlignVert = avCenter
      CaptionOptions.Text = 'pixels down'
      Index = 3
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnImport'
      CaptionOptions.Visible = False
      Control = btnImport
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 97
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 97
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object cbUseLabeledItems: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Import standalone labels as label items'
      SizeOptions.Height = 21
      State = cbsChecked
      Index = 1
    end
    object cbConvertPageControls: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Import page controls as tabbed groups'
      SizeOptions.Height = 21
      State = cbsChecked
      Index = 4
    end
    object cbAssociate: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Link labels to controls within the range:'
      SizeOptions.Height = 21
      State = cbsChecked
      OnClick = cbAssociateClick
      Index = 5
    end
    object cbConvertCheckBoxes: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Glyph.SourceDPI = 96
      CaptionOptions.Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C0000001B744558745469746C65005761726E696E673B4E6F7469
        6669636174696F6E3BB6E779860000024149444154785EA58F5F48536118C64F
        C5A02E9288EE2BA8816585ABFC83DA28EBA6CB6A12CC300BA2024D27589A0CB4
        ADA4DC859995D36DD1A4CD5DA428E94636D7E98FD236D79A04DBB42E14664E73
        5B46E982A7F73BD448362FA40F7E9C87F77D7EE77C87F3EA8EFE179C475B9812
        3A6BDC0F8F9410729657ECB91F1C4E8209439A823CCA7162C9DE949F4D3975F7
        DD5D69129D57F76F1A69967E9E1E7D8C90CB00CA7E7D55665AAA2E37AC295806
        9DB5FCED7C63D0AAC6AF6F9F108F05101C5081663AB64BEABF6ECC4BC0AE6EAB
        CF29766AE588935C76A51AE5443CE287B3ED349E37E4C8586799E368C84DD0A5
        906C1FAACF9D0FB98C589A0F407EE682C062D883A9B76DA0DD9CA952B2F55F87
        1BACCB12A83CBE4D64AB3DC87B9F5409C28F49072A2B140C217F9FE886C7580E
        EA0C49D337AFFBEB71D6EA03C2D57B15923A5E23A32F8F233A66C042B01BFA96
        1B30102C477D3A7A491FF8A693A06E0D7304F759452667BEB8279B9E8B730107
        621F3B11713523EA6EC5F4700B43C8C2CCDB8E49BE15AC4B4E1673B93BA776A6
        F55CDE17F49A9588BE6F4778508159470D621F1E41A3563258A6592DC22FAAF0
        F58D1A23F74B418EBFF1C48E8D9CE57C86DE7E538688CF84195B19A6FB4A0562
        3E230E498F31584ECCBF0C5CC20C7F0BD6EB85E83A97D1C1594A768527FA5588
        B8B59877DE436CCC44FF6AC3CFD028A2213F22535E2C8CF70BBBB9572A84EDD7
        30FBB21EC15E2598CBE98AC43DE6E274A4C27276AFC04AFB8E22F1538ECE1642
        4CEC5E25E23F2E2722D6131B56097344BF0168ADF1039DB8E68C000000004945
        4E44AE426082}
      CaptionOptions.Text = 'Import check boxes as check box items'
      CaptionOptions.VisibleElements = [cveText]
      SizeOptions.Height = 21
      OnClick = cbConvertCheckBoxesClick
      Index = 2
    end
    object cbConvertRadioButtons: TdxLayoutCheckBoxItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Glyph.SourceDPI = 96
      CaptionOptions.Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C0000001B744558745469746C65005761726E696E673B4E6F7469
        6669636174696F6E3BB6E779860000024149444154785EA58F5F48536118C64F
        C5A02E9288EE2BA8816585ABFC83DA28EBA6CB6A12CC300BA2024D27589A0CB4
        ADA4DC859995D36DD1A4CD5DA428E94636D7E98FD236D79A04DBB42E14664E73
        5B46E982A7F73BD448362FA40F7E9C87F77D7EE77C87F3EA8EFE179C475B9812
        3A6BDC0F8F9410729657ECB91F1C4E8209439A823CCA7162C9DE949F4D3975F7
        DD5D69129D57F76F1A69967E9E1E7D8C90CB00CA7E7D55665AAA2E37AC295806
        9DB5FCED7C63D0AAC6AF6F9F108F05101C5081663AB64BEABF6ECC4BC0AE6EAB
        CF29766AE588935C76A51AE5443CE287B3ED349E37E4C8586799E368C84DD0A5
        906C1FAACF9D0FB98C589A0F407EE682C062D883A9B76DA0DD9CA952B2F55F87
        1BACCB12A83CBE4D64AB3DC87B9F5409C28F49072A2B140C217F9FE886C7580E
        EA0C49D337AFFBEB71D6EA03C2D57B15923A5E23A32F8F233A66C042B01BFA96
        1B30102C477D3A7A491FF8A693A06E0D7304F759452667BEB8279B9E8B730107
        621F3B11713523EA6EC5F4700B43C8C2CCDB8E49BE15AC4B4E1673B93BA776A6
        F55CDE17F49A9588BE6F4778508159470D621F1E41A3563258A6592DC22FAAF0
        F58D1A23F74B418EBFF1C48E8D9CE57C86DE7E538688CF84195B19A6FB4A0562
        3E230E498F31584ECCBF0C5CC20C7F0BD6EB85E83A97D1C1594A768527FA5588
        B8B59877DE436CCC44FF6AC3CFD028A2213F22535E2C8CF70BBBB9572A84EDD7
        30FBB21EC15E2598CBE98AC43DE6E274A4C27276AFC04AFB8E22F1538ECE1642
        4CEC5E25E23F2E2722D6131B56097344BF0168ADF1039DB8E68C000000004945
        4E44AE426082}
      CaptionOptions.Text = 'Import radio buttons as radio button items'
      CaptionOptions.VisibleElements = [cveText]
      SizeOptions.Height = 21
      OnClick = cbConvertRadioButtonsClick
      Index = 3
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
      PixelsPerInch = 96
    end
  end
end
