inherited ScKeysFrame: TScKeysFrame
  Width = 515
  Height = 221
  object Label2: TLabel [0]
    Left = 8
    Top = 8
    Width = 55
    Height = 13
    Caption = 'Key names:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  inherited lbItemName: TListBox
    Height = 156
    Style = lbOwnerDrawFixed
    OnDrawItem = lbItemNameDrawItem
  end
  inherited PanelItem: TPanel
    Width = 331
    Height = 205
    TabOrder = 3
    object Label1: TLabel
      Left = 14
      Top = 12
      Width = 47
      Height = 13
      Caption = 'Key name'
    end
    object Label3: TLabel
      Left = 14
      Top = 56
      Width = 45
      Height = 13
      Caption = 'Algorithm'
    end
    object lbKeyData: TLabel
      Left = 146
      Top = 56
      Width = 41
      Height = 13
      Caption = 'BitCount'
    end
    object Label5: TLabel
      Left = 14
      Top = 124
      Width = 52
      Height = 13
      Caption = 'Fingerprint'
    end
    object edKeyName: TEdit
      Left = 14
      Top = 26
      Width = 304
      Height = 21
      TabOrder = 0
      OnExit = edKeyNameExit
    end
    object edFingerPrint: TEdit
      Left = 14
      Top = 140
      Width = 304
      Height = 21
      ReadOnly = True
      TabOrder = 6
    end
    object cbIsPrivate: TCheckBox
      Left = 14
      Top = 101
      Width = 97
      Height = 17
      Caption = 'IsPrivate'
      Enabled = False
      TabOrder = 5
    end
    object cbAlgorithm: TComboBox
      Left = 14
      Top = 72
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = cbAlgorithmChange
      Items.Strings = (
        'EC'
        'RSA'
        'DSA')
    end
    object cbBitCount: TComboBox
      Left = 146
      Top = 72
      Width = 81
      Height = 21
      TabOrder = 2
      OnChange = cbAlgorithmChange
      OnKeyPress = cbBitCountKeyPress
      Items.Strings = (
        '1024'
        '2048'
        '4096')
    end
    object btImport: TButton
      Left = 14
      Top = 168
      Width = 100
      Height = 25
      Caption = '&Import from...'
      TabOrder = 7
      OnClick = btImportClick
    end
    object btExportPrivate: TButton
      Left = 117
      Top = 168
      Width = 100
      Height = 25
      Caption = '&Export private key'
      TabOrder = 8
      OnClick = btExportPrivateClick
    end
    object btExportPublic: TButton
      Left = 219
      Top = 168
      Width = 100
      Height = 25
      Caption = 'Export &public key'
      TabOrder = 9
      OnClick = btExportPublicClick
    end
    object btGenerate: TButton
      Left = 237
      Top = 70
      Width = 80
      Height = 24
      Caption = '&Generate'
      TabOrder = 4
      OnClick = btGenerateClick
    end
    object cbECName: TComboBox
      Left = 146
      Top = 72
      Width = 81
      Height = 21
      AutoComplete = False
      Style = csDropDownList
      DropDownCount = 27
      TabOrder = 3
      OnClick = cbAlgorithmChange
      OnKeyPress = cbBitCountKeyPress
    end
  end
  object btNew: TButton
    Left = 8
    Top = 188
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&New'
    TabOrder = 1
    OnClick = btNewClick
  end
  object btDelete: TButton
    Left = 87
    Top = 188
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 2
    OnClick = btDeleteClick
  end
end
