object OptionsForm: TOptionsForm
  Left = 351
  Top = 244
  BorderStyle = bsDialog
  Caption = 'OrgChart Options'
  ClientHeight = 232
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TdxBevel
    Left = 9
    Top = 8
    Width = 356
    Height = 185
    Shape = dxbsFrame
  end
  object Label1: TcxLabel
    Left = 239
    Top = 25
    Caption = 'Indent X'
    Transparent = True
  end
  object Label2: TcxLabel
    Left = 239
    Top = 53
    Caption = 'Indent Y'
    Transparent = True
  end
  object Label3: TcxLabel
    Left = 239
    Top = 81
    Caption = 'Line width'
    Transparent = True
  end
  object GroupBox1: TcxGroupBox
    Left = 16
    Top = 15
    Caption = ' Edit Mode '
    TabOrder = 0
    Height = 93
    Width = 217
    object cbLeft: TcxCheckBox
      Left = 9
      Top = 16
      Caption = 'Left'
      TabOrder = 0
      Transparent = True
    end
    object cbCenter: TcxCheckBox
      Left = 9
      Top = 32
      Caption = 'Center'
      TabOrder = 1
      Transparent = True
    end
    object cbRight: TcxCheckBox
      Left = 9
      Top = 48
      Caption = 'Right'
      TabOrder = 2
      Transparent = True
    end
    object cbVCenter: TcxCheckBox
      Left = 9
      Top = 64
      Caption = 'Vert Center'
      TabOrder = 3
      Transparent = True
    end
    object cbWrap: TcxCheckBox
      Left = 112
      Top = 16
      Caption = 'Wrap'
      TabOrder = 4
      Transparent = True
    end
    object cbUpper: TcxCheckBox
      Left = 112
      Top = 32
      Caption = 'Upper'
      TabOrder = 5
      Transparent = True
    end
    object cbLower: TcxCheckBox
      Left = 112
      Top = 48
      Caption = 'Lower'
      TabOrder = 6
      Transparent = True
    end
    object cbGrow: TcxCheckBox
      Left = 112
      Top = 64
      Caption = 'Grow'
      TabOrder = 7
      Transparent = True
    end
  end
  object BitBtn1: TcxButton
    Left = 289
    Top = 199
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object BitBtn2: TcxButton
    Left = 208
    Top = 199
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object cbSelect: TcxCheckBox
    Left = 25
    Top = 112
    Caption = 'Show Select'
    TabOrder = 6
    Transparent = True
  end
  object cbFocus: TcxCheckBox
    Left = 25
    Top = 128
    Caption = 'Show Focus'
    TabOrder = 7
    Transparent = True
  end
  object cbButtons: TcxCheckBox
    Left = 25
    Top = 144
    Caption = 'Show Buttons'
    TabOrder = 8
    Transparent = True
  end
  object cbCanDrag: TcxCheckBox
    Left = 128
    Top = 112
    Caption = 'Can Drag'
    TabOrder = 9
    Transparent = True
  end
  object cbShowDrag: TcxCheckBox
    Left = 128
    Top = 128
    Caption = 'Show Drag'
    TabOrder = 10
    Transparent = True
  end
  object cbInsDel: TcxCheckBox
    Left = 128
    Top = 144
    Caption = 'Insert, Delete'
    TabOrder = 11
    Transparent = True
  end
  object cbEdit: TcxCheckBox
    Left = 25
    Top = 160
    Caption = 'Edit'
    TabOrder = 12
    Transparent = True
  end
  object cbShowImages: TcxCheckBox
    Left = 128
    Top = 160
    Caption = 'Show Images'
    TabOrder = 13
    Transparent = True
  end
  object seX: TcxSpinEdit
    Left = 291
    Top = 21
    Properties.AssignedValues.MinValue = True
    TabOrder = 1
    Width = 61
  end
  object seY: TcxSpinEdit
    Left = 291
    Top = 49
    Properties.AssignedValues.MinValue = True
    TabOrder = 2
    Width = 61
  end
  object seLineWidth: TcxSpinEdit
    Left = 291
    Top = 78
    Properties.MinValue = 1.000000000000000000
    TabOrder = 3
    Value = 1
    Width = 61
  end
end
