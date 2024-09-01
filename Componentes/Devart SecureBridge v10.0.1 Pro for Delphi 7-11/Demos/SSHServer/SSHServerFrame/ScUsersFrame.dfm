inherited ScUsersFrame: TScUsersFrame
  object Label2: TLabel [0]
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'User names:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  inherited Panel1: TPanel
    inherited PanelItem: TPanel
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 331
        Height = 137
        Align = alTop
        Shape = bsBottomLine
      end
      object Label1: TLabel
        Left = 14
        Top = 12
        Width = 51
        Height = 13
        Caption = 'User name'
      end
      object Label6: TLabel
        Left = 14
        Top = 96
        Width = 68
        Height = 13
        Caption = 'Authentication'
      end
      object Label7: TLabel
        Left = 171
        Top = 56
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object Label9: TLabel
        Left = 14
        Top = 56
        Width = 36
        Height = 13
        Caption = 'Domain'
      end
      object edUserName: TEdit
        Left = 14
        Top = 26
        Width = 304
        Height = 21
        TabOrder = 0
        OnExit = edUserNameExit
      end
      object cbPublicKey: TCheckBox
        Left = 14
        Top = 113
        Width = 73
        Height = 17
        Caption = 'Public key'
        TabOrder = 3
        OnClick = cbPublicKeyClick
      end
      object cbPassword: TCheckBox
        Left = 112
        Top = 113
        Width = 81
        Height = 17
        Caption = 'Password'
        TabOrder = 4
        OnClick = cbPasswordClick
      end
      object cbOSAuthentication: TCheckBox
        Left = 208
        Top = 113
        Width = 105
        Height = 17
        Caption = 'OS authentication'
        TabOrder = 5
        OnClick = cbOSAuthenticationClick
      end
      object edPassword: TEdit
        Left = 171
        Top = 70
        Width = 147
        Height = 21
        PasswordChar = '*'
        TabOrder = 2
      end
      object PanelKey: TPanel
        Left = 4
        Top = 137
        Width = 318
        Height = 148
        Hint = 'The key must be transferred through a secure channel'
        BevelOuter = bvNone
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        object Label3: TLabel
          Left = 11
          Top = 24
          Width = 43
          Height = 13
          Caption = 'Algorithm'
        end
        object Label4: TLabel
          Left = 142
          Top = 24
          Width = 42
          Height = 13
          Caption = 'Bit count'
        end
        object Label5: TLabel
          Left = 12
          Top = 66
          Width = 49
          Height = 13
          Caption = 'Fingerprint'
        end
        object Label8: TLabel
          Left = 12
          Top = 6
          Width = 26
          Height = 13
          Caption = 'Key:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edFingerPrint: TEdit
          Left = 12
          Top = 82
          Width = 297
          Height = 21
          TabOrder = 2
        end
        object cbAlgorithm: TComboBox
          Left = 11
          Top = 40
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            'RSA'
            'DSA')
        end
        object cbBitCount: TComboBox
          Left = 142
          Top = 40
          Width = 85
          Height = 21
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            '512'
            '768'
            '1024'
            '2048')
        end
        object btImport: TButton
          Left = 12
          Top = 110
          Width = 105
          Height = 25
          Caption = '&Import from...'
          TabOrder = 3
          OnClick = btImportClick
        end
        object btExportPublic: TButton
          Left = 120
          Top = 110
          Width = 105
          Height = 25
          Caption = 'Export &public key'
          TabOrder = 4
          OnClick = btExportPublicClick
        end
      end
      object edDomain: TEdit
        Left = 14
        Top = 70
        Width = 147
        Height = 21
        TabOrder = 1
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
