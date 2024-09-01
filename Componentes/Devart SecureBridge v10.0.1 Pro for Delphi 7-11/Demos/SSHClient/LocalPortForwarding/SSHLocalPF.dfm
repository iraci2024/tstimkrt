inherited SSHLocalPF: TSSHLocalPF
  Width = 443
  Height = 270
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 222
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    object Panel5: TPanel
      Tag = 1
      Left = 1
      Top = 2
      Width = 569
      Height = 218
      BevelOuter = bvNone
      Color = 48127
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 283
        Height = 216
        BevelOuter = bvNone
        TabOrder = 0
        object Label6: TLabel
          Left = 16
          Top = 116
          Width = 51
          Height = 13
          Caption = 'User name'
        end
        object Label1: TLabel
          Left = 8
          Top = 2
          Width = 98
          Height = 13
          Caption = 'SSH Connection '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 16
          Top = 64
          Width = 56
          Height = 13
          Caption = 'SSH Server'
        end
        object Label5: TLabel
          Left = 16
          Top = 90
          Width = 44
          Height = 13
          Caption = 'SSH Port'
        end
        object Label3: TLabel
          Left = 16
          Top = 19
          Width = 94
          Height = 13
          Caption = 'Authentication kind:'
        end
        object pnPrivateKey: TPanel
          Left = 10
          Top = 134
          Width = 265
          Height = 56
          BevelOuter = bvNone
          TabOrder = 7
          Visible = False
          object Label8: TLabel
            Left = 6
            Top = 8
            Width = 53
            Height = 13
            Caption = 'Private key'
          end
          object cbPrivateKey: TComboBox
            Left = 70
            Top = 4
            Width = 185
            Height = 21
            ItemHeight = 13
            TabOrder = 0
            OnChange = edSSHUserNameChange
            OnDropDown = cbPrivateKeyDropDown
          end
          object Panel12: TPanel
            Left = 129
            Top = 28
            Width = 125
            Height = 24
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 1
            object btKeyGen: TSpeedButton
              Left = 1
              Top = 1
              Width = 123
              Height = 22
              Caption = 'Generate key'
              Flat = True
              Transparent = False
              OnClick = btKeyGenClick
            end
          end
        end
        object pnPassword: TPanel
          Left = 10
          Top = 136
          Width = 265
          Height = 26
          BevelOuter = bvNone
          TabOrder = 6
          object lbPassword: TLabel
            Left = 6
            Top = 6
            Width = 46
            Height = 13
            Caption = 'Password'
          end
          object edSSHPassword: TEdit
            Left = 70
            Top = 2
            Width = 185
            Height = 21
            PasswordChar = '*'
            TabOrder = 0
            OnChange = edSSHUserNameChange
          end
        end
        object edSSHUserName: TEdit
          Left = 80
          Top = 112
          Width = 185
          Height = 21
          TabOrder = 5
          OnChange = edSSHUserNameChange
        end
        object edSSHHost: TEdit
          Left = 80
          Top = 60
          Width = 185
          Height = 21
          TabOrder = 3
          OnChange = edSSHUserNameChange
        end
        object rbPassword: TRadioButton
          Left = 16
          Top = 34
          Width = 69
          Height = 17
          Caption = 'Password'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbPasswordClick
        end
        object rbPublicKey: TRadioButton
          Left = 83
          Top = 34
          Width = 73
          Height = 17
          Caption = 'Public key'
          TabOrder = 1
          OnClick = rbPasswordClick
        end
        object Panel10: TPanel
          Left = 139
          Top = 3
          Width = 125
          Height = 24
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 10
          object btImportKey: TSpeedButton
            Left = 1
            Top = 1
            Width = 123
            Height = 22
            Caption = 'Import server key'
            Flat = True
            Transparent = False
            OnClick = btImportKeyClick
          end
        end
        object seSSHPort: TSpinEdit
          Left = 80
          Top = 86
          Width = 185
          Height = 22
          MaxValue = 65535
          MinValue = 0
          TabOrder = 4
          Value = 22
          OnChange = edSSHUserNameChange
        end
        object cbRandomization: TCheckBox
          Left = 18
          Top = 167
          Width = 119
          Height = 17
          Hint = 'Generation random data increase connection reliability'
          Caption = 'Silent randomization'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 9
        end
        object rbKeyboardInteractive: TRadioButton
          Left = 152
          Top = 34
          Width = 119
          Height = 17
          Caption = 'Keyboard-interactive'
          TabOrder = 2
          OnClick = rbPasswordClick
        end
        object Panel3: TPanel
          Left = 16
          Top = 189
          Width = 249
          Height = 24
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 8
          object btConnectSSH: TSpeedButton
            Left = 1
            Top = 1
            Width = 123
            Height = 22
            Caption = 'Connect SSH'
            Flat = True
            Transparent = False
            OnClick = btConnectSSHClick
          end
          object btDisconnectSSH: TSpeedButton
            Left = 125
            Top = 1
            Width = 123
            Height = 22
            Caption = 'Disconnect SSH'
            Enabled = False
            Flat = True
            Transparent = False
            OnClick = btDisconnectSSHClick
          end
        end
      end
      object Panel7: TPanel
        Left = 285
        Top = 1
        Width = 283
        Height = 216
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 16
          Top = 63
          Width = 47
          Height = 13
          Caption = 'Dest Host'
        end
        object Label9: TLabel
          Left = 16
          Top = 90
          Width = 44
          Height = 13
          Caption = 'Dest Port'
        end
        object Label10: TLabel
          Left = 16
          Top = 36
          Width = 56
          Height = 13
          Caption = 'Source Port'
        end
        object Label11: TLabel
          Left = 8
          Top = 2
          Width = 87
          Height = 13
          Caption = 'Port forwarding'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edDestHost: TEdit
          Left = 80
          Top = 59
          Width = 185
          Height = 21
          TabOrder = 1
          OnChange = edDestHostChange
        end
        object seDestPort: TSpinEdit
          Left = 80
          Top = 85
          Width = 185
          Height = 22
          MaxValue = 65535
          MinValue = 0
          TabOrder = 2
          Value = 23
          OnChange = edDestHostChange
        end
        object seSourcePort: TSpinEdit
          Left = 80
          Top = 31
          Width = 185
          Height = 22
          MaxValue = 65535
          MinValue = 0
          TabOrder = 0
          Value = 5001
          OnChange = edDestHostChange
        end
        object Panel8: TPanel
          Left = 16
          Top = 189
          Width = 249
          Height = 24
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 3
          object btStart: TSpeedButton
            Left = 1
            Top = 1
            Width = 123
            Height = 22
            Caption = 'Start Port forwarding'
            Enabled = False
            Flat = True
            Transparent = False
            OnClick = btStartClick
          end
          object btStop: TSpeedButton
            Left = 125
            Top = 1
            Width = 123
            Height = 22
            Caption = 'Stop Port forwarding'
            Enabled = False
            Flat = True
            Transparent = False
            OnClick = btStopClick
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 222
    Width = 443
    Height = 48
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object StringGrid: TStringGrid
      Left = 0
      Top = 28
      Width = 443
      Height = 20
      Align = alClient
      ColCount = 4
      DefaultRowHeight = 18
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      TabOrder = 0
      ColWidths = (
        37
        134
        205
        94)
    end
    object Panel9: TPanel
      Left = 0
      Top = 0
      Width = 443
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Panel4: TPanel
        Left = 17
        Top = 1
        Width = 249
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 0
        object btSConnect: TSpeedButton
          Left = 1
          Top = 1
          Width = 123
          Height = 22
          Caption = 'Create new connection'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btSConnectClick
        end
        object btSDisconnect: TSpeedButton
          Left = 125
          Top = 1
          Width = 123
          Height = 22
          Caption = 'Disconnect'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btSDisconnectClick
        end
      end
    end
  end
  object ScSSHClient: TScSSHClient
    HostKeyAlgorithms = <
      item
        Algorithm = aaRSA
      end
      item
        Algorithm = aaDSA
      end>
    HostName = 'localhost'
    User = 'test'
    Password = 'test'
    KeyStorage = ScFileStorage
    AfterConnect = ScSSHClientAfterConnect
    BeforeConnect = ScSSHClientBeforeConnect
    AfterDisconnect = ScSSHClientAfterDisconnect
    OnServerKeyValidate = ScSSHClientServerKeyValidate
    OnAuthenticationPrompt = ScSSHClientAuthenticationPrompt
    Left = 12
    Top = 248
  end
  object ScFileStorage: TScFileStorage
    Left = 72
    Top = 248
  end
  object ScSSHChannel: TScSSHChannel
    Client = ScSSHClient
    GatewayPorts = True
    Timeout = 15
    OnSocketConnect = ScSSHChannelSocketConnect
    OnSocketDisconnect = ScSSHChannelSocketDisconnect
    Left = 42
    Top = 248
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 104
    Top = 248
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'All formats |*.ssl;*.pem;*.ietf;*.pub;*.ietfpub|OpenSSL format (' +
      '*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.i' +
      'etf|Public key (*.pub)|*.pub|Public IETF key (*.ietfpub)|*.ietfp' +
      'ub|All files (*.*)|*.*'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Import key'
    Left = 136
    Top = 248
  end
end
