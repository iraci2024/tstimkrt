inherited ScKeysFrame: TScKeysFrame
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
    OnDrawItem = lbItemNameDrawItem
  end
  inherited Panel1: TPanel
    inherited PanelItem: TPanel
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
        Width = 43
        Height = 13
        Caption = 'Algorithm'
      end
      object Label4: TLabel
        Left = 146
        Top = 56
        Width = 42
        Height = 13
        Caption = 'Bit count'
      end
      object Label5: TLabel
        Left = 14
        Top = 124
        Width = 49
        Height = 13
        Caption = 'Fingerprint'
      end
      object edKeyName: TEdit
        Left = 14
        Top = 26
        Width = 304
        Height = 21
        TabOrder = 0
      end
      object edFingerPrint: TEdit
        Left = 14
        Top = 140
        Width = 304
        Height = 21
        TabOrder = 5
      end
      object cbIsPrivate: TCheckBox
        Left = 14
        Top = 101
        Width = 97
        Height = 17
        Caption = 'Private'
        TabOrder = 4
      end
      object cbAlgorithm: TComboBox
        Left = 14
        Top = 72
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cbAlgorithmChange
        Items.Strings = (
          'RSA'
          'DSA')
      end
      object cbBitCount: TComboBox
        Left = 146
        Top = 72
        Width = 81
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        OnChange = cbAlgorithmChange
        OnExit = cbBitCountExit
        Items.Strings = (
          '512'
          '768'
          '1024'
          '2048')
      end
      object btImport: TButton
        Left = 14
        Top = 168
        Width = 100
        Height = 25
        Caption = '&Import from...'
        TabOrder = 6
        OnClick = btImportClick
      end
      object btExportPrivate: TButton
        Left = 117
        Top = 168
        Width = 100
        Height = 25
        Caption = '&Export private key'
        TabOrder = 7
        OnClick = btExportPrivateClick
      end
      object btExportPublic: TButton
        Left = 219
        Top = 168
        Width = 100
        Height = 25
        Caption = 'Export &public key'
        TabOrder = 8
        OnClick = btExportPublicClick
      end
      object btGenerate: TButton
        Left = 237
        Top = 70
        Width = 80
        Height = 24
        Caption = '&Generate'
        TabOrder = 3
        OnClick = btGenerateClick
      end
    end
  end
  inherited btNew: TButton
    OnClick = btNewClick
  end
  inherited btDelete: TButton
    OnClick = btDeleteClick
  end
end
