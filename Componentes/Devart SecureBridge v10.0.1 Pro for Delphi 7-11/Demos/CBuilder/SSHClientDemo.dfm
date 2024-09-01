object DemoForm: TDemoForm
  Left = 139
  Top = 45
  Width = 892
  Height = 532
  Caption = 'SecureBridge SSHClient demo for C++Builder'
  Constraints.MinHeight = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 45
    Width = 884
    Height = 453
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinWidth = 730
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 884
      Height = 238
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
      object Panel3: TPanel
        Left = 1
        Top = 8
        Width = 221
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 0
        object btConnectSSH: TSpeedButton
          Left = 1
          Top = 1
          Width = 109
          Height = 22
          Caption = 'Connect SSH'
          Flat = True
          Transparent = False
          OnClick = btConnectSSHClick
        end
        object btDisconnectSSH: TSpeedButton
          Left = 111
          Top = 1
          Width = 109
          Height = 22
          Caption = 'Disconnect SSH'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btDisconnectSSHClick
        end
      end
      object Panel5: TPanel
        Tag = 1
        Left = 1
        Top = 32
        Width = 568
        Height = 197
        BevelOuter = bvNone
        Color = 48127
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Panel6: TPanel
          Left = 1
          Top = 1
          Width = 294
          Height = 195
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
            Width = 102
            Height = 13
            Caption = ' SSH Connection '
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
            Left = 4
            Top = 36
            Width = 94
            Height = 13
            Caption = 'Authentication kind:'
          end
          object pnPrivateKey: TPanel
            Left = 10
            Top = 134
            Width = 265
            Height = 59
            BevelOuter = bvNone
            TabOrder = 6
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
              Left = 166
              Top = 32
              Width = 89
              Height = 24
              BevelOuter = bvNone
              Color = 48127
              TabOrder = 1
              object btKeyGen: TSpeedButton
                Left = 1
                Top = 1
                Width = 87
                Height = 22
                Caption = 'Generate key'
                Flat = True
                Transparent = False
                OnClick = btKeyGenClick
              end
            end
          end
          object edSSHUserName: TEdit
            Left = 80
            Top = 112
            Width = 185
            Height = 21
            TabOrder = 4
            OnChange = edSSHUserNameChange
          end
          object edSSHHost: TEdit
            Left = 80
            Top = 60
            Width = 185
            Height = 21
            TabOrder = 2
            OnChange = edSSHUserNameChange
          end
          object rbPassword: TRadioButton
            Left = 113
            Top = 34
            Width = 73
            Height = 17
            Caption = 'Password'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = rbPasswordClick
          end
          object rbPublicKey: TRadioButton
            Left = 195
            Top = 34
            Width = 76
            Height = 17
            Caption = 'Public key'
            TabOrder = 1
            OnClick = rbPublicKeyClick
          end
          object pnPassword: TPanel
            Left = 10
            Top = 136
            Width = 265
            Height = 29
            BevelOuter = bvNone
            TabOrder = 5
            object Label7: TLabel
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
          object Panel10: TPanel
            Left = 156
            Top = 3
            Width = 109
            Height = 24
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 7
            object btImportKey: TSpeedButton
              Left = 1
              Top = 1
              Width = 107
              Height = 22
              Caption = 'Import server key'
              Flat = True
              Transparent = False
              OnClick = btImportKeyClick
            end
          end
          object seSSHPort: TEdit
            Left = 80
            Top = 86
            Width = 185
            Height = 21
            TabOrder = 3
            Text = '22'
            OnChange = edSSHUserNameChange
          end
        end
        object Panel7: TPanel
          Left = 296
          Top = 1
          Width = 271
          Height = 195
          BevelOuter = bvNone
          TabOrder = 1
          object Label2: TLabel
            Left = 12
            Top = 58
            Width = 47
            Height = 13
            Caption = 'Dest Host'
          end
          object Label9: TLabel
            Left = 12
            Top = 85
            Width = 44
            Height = 13
            Caption = 'Dest Port'
          end
          object Label10: TLabel
            Left = 12
            Top = 31
            Width = 56
            Height = 13
            Caption = 'Source Port'
          end
          object edDestHost: TEdit
            Left = 76
            Top = 54
            Width = 185
            Height = 21
            TabOrder = 1
            OnChange = edDestHostChange
          end
          object seDestPort: TEdit
            Left = 76
            Top = 80
            Width = 185
            Height = 21
            TabOrder = 2
            Text = '5001'
            OnChange = edDestHostChange
          end
          object seSourcePort: TEdit
            Left = 76
            Top = 26
            Width = 185
            Height = 21
            TabOrder = 0
            Text = '5001'
            OnChange = edDestHostChange
          end
        end
      end
      object Panel8: TPanel
        Left = 296
        Top = 8
        Width = 273
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 2
        object btStart: TSpeedButton
          Left = 1
          Top = 1
          Width = 135
          Height = 22
          Caption = 'Start Port-forwarding'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btStartClick
        end
        object btStop: TSpeedButton
          Left = 137
          Top = 1
          Width = 135
          Height = 22
          Caption = 'Stop Port-forwarding'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btStopClick
        end
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 238
      Width = 884
      Height = 215
      Align = alClient
      BevelOuter = bvNone
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      object StringGrid: TStringGrid
        Left = 0
        Top = 28
        Width = 884
        Height = 187
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
        Width = 884
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Panel4: TPanel
          Left = 1
          Top = 0
          Width = 273
          Height = 26
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 0
          object btSConnect: TSpeedButton
            Left = 1
            Top = 1
            Width = 135
            Height = 24
            Caption = 'Create new connection'
            Enabled = False
            Flat = True
            Transparent = False
            OnClick = btSConnectClick
          end
          object btSDisconnect: TSpeedButton
            Left = 137
            Top = 1
            Width = 135
            Height = 24
            Caption = 'Disconnect'
            Enabled = False
            Flat = True
            Transparent = False
            OnClick = btSDisconnectClick
          end
        end
      end
    end
  end
  object pnTopLabel: TPanel
    Left = 0
    Top = 0
    Width = 884
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    Color = 48127
    TabOrder = 1
    object lbTitle: TLabel
      Left = 0
      Top = 0
      Width = 884
      Height = 45
      Cursor = crArrow
      Align = alTop
      AutoSize = False
      Caption = ' SecureBridge'
      Color = 48127
      Constraints.MinWidth = 130
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -35
      Font.Name = 'Verdana'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
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
    KeyStorage = ScFileStorage
    AfterConnect = ScSSHClientAfterConnect
    BeforeConnect = ScSSHClientBeforeConnect
    AfterDisconnect = ScSSHClientAfterDisconnect
    OnServerKeyValidate = ScSSHClientServerKeyValidate
    Left = 12
    Top = 264
  end
  object ScFileStorage: TScFileStorage
    Left = 72
    Top = 264
  end
  object ScSSHChannel: TScSSHChannel
    Client = ScSSHClient
    GatewayPorts = True
    OnSocketConnect = ScSSHChannelSocketConnect
    OnSocketDisconnect = ScSSHChannelSocketDisconnect
    Left = 42
    Top = 264
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 104
    Top = 264
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
    Top = 264
  end
end
