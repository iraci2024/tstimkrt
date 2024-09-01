inherited ScUsersFrame: TScUsersFrame
  Width = 533
  Height = 378
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
  inherited lbItemName: TListBox
    Height = 313
  end
  inherited PanelItem: TPanel
    Width = 349
    Height = 362
    TabOrder = 3
    object Bevel1: TBevel
      Left = 1
      Top = 1
      Width = 347
      Height = 175
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
      Top = 99
      Width = 70
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
      Width = 35
      Height = 13
      Caption = 'Domain'
    end
    object Label10: TLabel
      Left = 14
      Top = 136
      Width = 112
      Height = 13
      Caption = 'SSH channel permission'
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
      Caption = 'PublicKey'
      TabOrder = 3
      OnClick = cbPublicKeyClick
    end
    object cbPassword: TCheckBox
      Left = 123
      Top = 113
      Width = 81
      Height = 17
      Caption = 'Password'
      TabOrder = 4
      OnClick = cbPasswordClick
    end
    object cbOSAuthentication: TCheckBox
      Left = 217
      Top = 113
      Width = 101
      Height = 17
      Caption = 'OSAuthentication'
      TabOrder = 5
      OnClick = cbOSAuthenticationClick
    end
    object edPassword: TEdit
      Left = 167
      Top = 70
      Width = 151
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object PanelKey: TPanel
      Left = 1
      Top = 182
      Width = 318
      Height = 151
      Hint = 'The key must be transferred through a secure channel'
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      object Label3: TLabel
        Left = 11
        Top = 24
        Width = 45
        Height = 13
        Caption = 'Algorithm'
      end
      object Label4: TLabel
        Left = 142
        Top = 24
        Width = 41
        Height = 13
        Caption = 'BitCount'
      end
      object Label5: TLabel
        Left = 12
        Top = 66
        Width = 52
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
        Width = 304
        Height = 21
        TabOrder = 2
      end
      object cbAlgorithm: TComboBox
        Left = 11
        Top = 40
        Width = 121
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        Items.Strings = (
          'EC'
          'RSA'
          'DSA')
      end
      object cbBitCount: TComboBox
        Left = 142
        Top = 40
        Width = 85
        Height = 21
        TabOrder = 1
        Items.Strings = (
          '1024'
          '2048'
          '4096')
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
    object cbShell: TCheckBox
      Left = 233
      Top = 150
      Width = 43
      Height = 17
      Caption = 'Shell'
      TabOrder = 8
      OnClick = cbLocalForwardingClick
    end
    object cbRemoteForwarding: TCheckBox
      Left = 116
      Top = 150
      Width = 110
      Height = 17
      Caption = 'Remote Forwarding'
      TabOrder = 7
      OnClick = cbLocalForwardingClick
    end
    object cbLocalForwarding: TCheckBox
      Left = 14
      Top = 150
      Width = 96
      Height = 17
      Caption = 'Local Forwarding'
      TabOrder = 6
      OnClick = cbLocalForwardingClick
    end
    object cbSFTP: TCheckBox
      Left = 276
      Top = 150
      Width = 42
      Height = 17
      Caption = 'SFTP'
      TabOrder = 9
      OnClick = cbLocalForwardingClick
    end
  end
  object btNew: TButton
    Left = 8
    Top = 345
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&New'
    TabOrder = 1
    OnClick = btNewClick
  end
  object btDelete: TButton
    Left = 87
    Top = 345
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 2
    OnClick = btDeleteClick
  end
end
