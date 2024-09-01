object fmSettings: TfmSettings
  Left = 324
  Top = 117
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 400
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 323
    Height = 366
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Connection'
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 315
        Height = 338
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel1: TPanel
          Tag = 1
          Left = 2
          Top = 2
          Width = 310
          Height = 290
          BevelOuter = bvNone
          Color = 48127
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object Panel11: TPanel
            Left = 1
            Top = 1
            Width = 308
            Height = 201
            BevelOuter = bvNone
            TabOrder = 0
            object Label6: TLabel
              Left = 16
              Top = 124
              Width = 51
              Height = 13
              Caption = 'User name'
            end
            object Label4: TLabel
              Left = 16
              Top = 72
              Width = 56
              Height = 13
              Caption = 'SSH Server'
            end
            object Label5: TLabel
              Left = 16
              Top = 98
              Width = 44
              Height = 13
              Caption = 'SSH Port'
            end
            object Label3: TLabel
              Left = 16
              Top = 26
              Width = 94
              Height = 13
              Caption = 'Authentication kind:'
            end
            object pnPrivateKey: TPanel
              Left = 10
              Top = 142
              Width = 287
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
                Left = 94
                Top = 4
                Width = 185
                Height = 21
                ItemHeight = 13
                TabOrder = 0
                OnDropDown = cbPrivateKeyDropDown
              end
              object Panel14: TPanel
                Left = 153
                Top = 29
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
              Top = 144
              Width = 287
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
                Left = 94
                Top = 2
                Width = 185
                Height = 21
                PasswordChar = '*'
                TabOrder = 0
              end
            end
            object edSSHUserName: TEdit
              Left = 104
              Top = 120
              Width = 185
              Height = 21
              TabOrder = 5
            end
            object edSSHHost: TEdit
              Left = 104
              Top = 68
              Width = 185
              Height = 21
              TabOrder = 3
            end
            object rbPassword: TRadioButton
              Left = 16
              Top = 44
              Width = 69
              Height = 17
              Caption = 'Password'
              Checked = True
              TabOrder = 0
              TabStop = True
              OnClick = rbPasswordClick
            end
            object rbPublicKey: TRadioButton
              Left = 95
              Top = 44
              Width = 73
              Height = 17
              Caption = 'Public key'
              TabOrder = 1
              OnClick = rbPasswordClick
            end
            object seSSHPort: TSpinEdit
              Left = 104
              Top = 94
              Width = 185
              Height = 22
              MaxValue = 65535
              MinValue = 0
              TabOrder = 4
              Value = 22
            end
            object rbKeyboardInteractive: TRadioButton
              Left = 173
              Top = 44
              Width = 119
              Height = 17
              Caption = 'Keyboard-interactive'
              TabOrder = 2
              OnClick = rbPasswordClick
            end
            object Panel15: TPanel
              Left = 164
              Top = 8
              Width = 125
              Height = 24
              BevelOuter = bvNone
              Color = 48127
              TabOrder = 8
              object btRandomize: TSpeedButton
                Left = 1
                Top = 1
                Width = 123
                Height = 22
                Caption = 'Randomize'
                Flat = True
                Transparent = False
                OnClick = btRandomizeClick
              end
            end
          end
          object Panel12: TPanel
            Left = 1
            Top = 203
            Width = 308
            Height = 55
            BevelOuter = bvNone
            TabOrder = 1
            object sbLogFile: TSpeedButton
              Left = 267
              Top = 28
              Width = 22
              Height = 21
              Anchors = [akTop, akRight]
              Caption = '...'
              Enabled = False
              Flat = True
              OnClick = sbLogFileClick
            end
            object cbLog: TCheckBox
              Left = 16
              Top = 8
              Width = 105
              Height = 17
              Caption = 'Log events to file'
              TabOrder = 0
              OnClick = cbLogClick
            end
            object edLogFile: TEdit
              Left = 16
              Top = 28
              Width = 251
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              Enabled = False
              TabOrder = 1
            end
          end
          object Panel13: TPanel
            Left = 1
            Top = 259
            Width = 308
            Height = 30
            BevelOuter = bvNone
            TabOrder = 2
            object cbStartShell: TCheckBox
              Left = 16
              Top = 8
              Width = 257
              Height = 17
              Caption = 'Start a shell after SSH connection establishment'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Security'
      ImageIndex = 1
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 315
        Height = 338
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel2: TPanel
          Tag = 1
          Left = 2
          Top = 2
          Width = 310
          Height = 333
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 0
          object Panel21: TPanel
            Left = 1
            Top = 1
            Width = 308
            Height = 99
            BevelOuter = bvNone
            TabOrder = 0
            object Label1: TLabel
              Left = 16
              Top = 8
              Width = 90
              Height = 13
              Caption = 'Encryption ciphers:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object UpBtn: TSpeedButton
              Left = 134
              Top = 33
              Width = 24
              Height = 24
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Pitch = fpVariable
              Font.Style = []
              Glyph.Data = {
                96090000424D9609000000000000360000002800000028000000140000000100
                1800000000006009000000000000000000000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8080808080808080808080
                80808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080808080808080808080
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF800000800000800000800000800000808080FFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFF800000800000800000800000800000808080FFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF0000FF0000FF0000FF0000800000808080FFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
                C0C0C0C0C0C0C0C0C0C0800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000
                FF0000FF0000800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0
                C0C0800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF00008000
                00808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0800000808080
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000800000808080FFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0800000808080FFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF0000FF0000FF0000FF0000800000808080808080808080FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
                C0C0C0C0C0C0C0C0C0C0800000808080808080808080FFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000
                FF0000FF0000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FF0000FF00
                00FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FF0000FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFC0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              NumGlyphs = 2
              ParentFont = False
              OnClick = UpBtnClick
            end
            object DownBtn: TSpeedButton
              Left = 134
              Top = 61
              Width = 24
              Height = 24
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Pitch = fpVariable
              Font.Style = []
              Glyph.Data = {
                96090000424D9609000000000000360000002800000028000000140000000100
                1800000000006009000000000000000000000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000808080808080FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFF800000808080808080FFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFF0000FF0000800000808080808080FFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFC0C0C0C0C0C0800000808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000
                FF0000FF0000800000808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0
                C0C0800000808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FF0000FF00
                00800000808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0800000
                808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFF0000FF0000FF0000FF0000FF0000FF0000800000800000800000FF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0800000800000800000FFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF0000FF0000FF0000FF0000800000808080FFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
                C0C0C0C0C0C0C0C0C0C0800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000
                FF0000FF0000800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0
                C0C0800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF00008000
                00808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0800000808080
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000800000808080FFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0800000808080FFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF0000FF0000FF0000FF0000800000808080FFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0
                C0C0C0C0C0C0C0C0C0C0800000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000
                FF0000FF0000800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0
                C0C0800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              NumGlyphs = 2
              ParentFont = False
              OnClick = DownBtnClick
            end
            object lbCiphers: TListBox
              Left = 16
              Top = 26
              Width = 113
              Height = 66
              ItemHeight = 13
              TabOrder = 0
              OnClick = lbCiphersClick
            end
          end
          object Panel22: TPanel
            Left = 1
            Top = 101
            Width = 308
            Height = 63
            BevelOuter = bvNone
            TabOrder = 1
            object Label2: TLabel
              Left = 16
              Top = 12
              Width = 71
              Height = 13
              Caption = 'Host key name'
            end
            object cbHostKeyName: TComboBox
              Left = 128
              Top = 8
              Width = 161
              Height = 21
              ItemHeight = 13
              TabOrder = 0
              OnDropDown = cbPrivateKeyDropDown
            end
            object Panel25: TPanel
              Left = 164
              Top = 35
              Width = 125
              Height = 24
              BevelOuter = bvNone
              Color = 48127
              TabOrder = 1
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
          end
          object Panel23: TPanel
            Left = 1
            Top = 165
            Width = 308
            Height = 62
            BevelOuter = bvNone
            TabOrder = 2
            object Timeout: TLabel
              Left = 16
              Top = 13
              Width = 90
              Height = 13
              Caption = 'Timeout  (seconds)'
            end
            object Label10: TLabel
              Left = 16
              Top = 38
              Width = 106
              Height = 13
              Caption = 'Max data before rekey'
            end
            object cbRekeyLimit: TComboBox
              Left = 128
              Top = 34
              Width = 89
              Height = 21
              ItemHeight = 13
              TabOrder = 1
              Items.Strings = (
                '128M'
                '256M'
                '512M'
                '1G'
                '2G'
                '4G')
            end
            object seTimeout: TSpinEdit
              Left = 128
              Top = 8
              Width = 89
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
          end
          object Panel24: TPanel
            Left = 1
            Top = 228
            Width = 308
            Height = 104
            BevelOuter = bvNone
            TabOrder = 3
            object Label7: TLabel
              Left = 16
              Top = 8
              Width = 61
              Height = 13
              Caption = 'Keep alive'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label13: TLabel
              Left = 16
              Top = 31
              Width = 53
              Height = 13
              Caption = 'Max count '
            end
            object Label12: TLabel
              Left = 16
              Top = 57
              Width = 87
              Height = 13
              Caption = 'Interval  (seconds)'
            end
            object seKeepAliveCountMax: TSpinEdit
              Left = 128
              Top = 26
              Width = 89
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
            object seKeepAliveInterval: TSpinEdit
              Left = 128
              Top = 52
              Width = 89
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object cbTCPKeepalive: TCheckBox
              Left = 16
              Top = 83
              Width = 153
              Height = 17
              Caption = 'Enable TCP keepalives'
              TabOrder = 2
            end
          end
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Port forwarding'
      ImageIndex = 2
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 315
        Height = 338
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel3: TPanel
          Tag = 1
          Left = 2
          Top = 2
          Width = 310
          Height = 266
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 0
          object Panel31: TPanel
            Left = 1
            Top = 1
            Width = 308
            Height = 264
            BevelOuter = bvNone
            TabOrder = 0
            object Label14: TLabel
              Left = 16
              Top = 8
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
            object Label9: TLabel
              Left = 16
              Top = 189
              Width = 55
              Height = 13
              Caption = 'Source port'
            end
            object Label15: TLabel
              Left = 16
              Top = 214
              Width = 76
              Height = 13
              Caption = 'Destination host'
            end
            object Label16: TLabel
              Left = 16
              Top = 241
              Width = 74
              Height = 13
              Caption = 'Destination port'
            end
            object rbLocal: TRadioButton
              Left = 104
              Top = 163
              Width = 65
              Height = 17
              Caption = 'Local'
              Checked = True
              TabOrder = 2
              TabStop = True
            end
            object rbRemote: TRadioButton
              Left = 184
              Top = 163
              Width = 65
              Height = 17
              Caption = 'Remote'
              TabOrder = 3
            end
            object edDestHost: TEdit
              Left = 104
              Top = 210
              Width = 185
              Height = 21
              TabOrder = 5
            end
            object seSourcePort: TSpinEdit
              Left = 104
              Top = 184
              Width = 185
              Height = 22
              MaxValue = 65535
              MinValue = 0
              TabOrder = 4
              Value = 5001
            end
            object seDestPort: TSpinEdit
              Left = 104
              Top = 236
              Width = 185
              Height = 22
              MaxValue = 65535
              MinValue = 0
              TabOrder = 6
              Value = 0
            end
            object lbPortForwarding: TListBox
              Left = 16
              Top = 26
              Width = 273
              Height = 97
              ItemHeight = 13
              TabOrder = 0
            end
            object Panel32: TPanel
              Left = 16
              Top = 131
              Width = 273
              Height = 24
              BevelOuter = bvNone
              Color = 48127
              TabOrder = 1
              object sbAddPortForwarding: TSpeedButton
                Left = 1
                Top = 1
                Width = 136
                Height = 22
                Caption = 'Add new forwarded port'
                Flat = True
                Transparent = False
                OnClick = sbAddPortForwardingClick
              end
              object sbDeletePortForwarding: TSpeedButton
                Left = 138
                Top = 1
                Width = 134
                Height = 22
                Caption = 'Delete'
                Flat = True
                Transparent = False
                OnClick = sbDeletePortForwardingClick
              end
            end
          end
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'SFTP options'
      ImageIndex = 3
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 315
        Height = 338
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel4: TPanel
          Tag = 1
          Left = 2
          Top = 2
          Width = 310
          Height = 190
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 0
          object Panel41: TPanel
            Left = 1
            Top = 1
            Width = 308
            Height = 132
            BevelOuter = bvNone
            TabOrder = 0
            object Label18: TLabel
              Left = 16
              Top = 57
              Width = 90
              Height = 13
              Caption = 'Timeout  (seconds)'
            end
            object Label19: TLabel
              Left = 16
              Top = 83
              Width = 98
              Height = 13
              Caption = 'Download block size'
            end
            object Label20: TLabel
              Left = 16
              Top = 109
              Width = 84
              Height = 13
              Caption = 'Upload block size'
            end
            object Label21: TLabel
              Left = 16
              Top = 30
              Width = 35
              Height = 13
              Caption = 'Version'
            end
            object Label17: TLabel
              Left = 16
              Top = 8
              Width = 32
              Height = 13
              Caption = 'SFTP'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object seSFTPTimeout: TSpinEdit
              Left = 128
              Top = 52
              Width = 89
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object seSFTPDowloadBlockSize: TSpinEdit
              Left = 128
              Top = 78
              Width = 89
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 2
              Value = 0
            end
            object seSFTPUploadBlockSize: TSpinEdit
              Left = 128
              Top = 104
              Width = 89
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object cbSFTPVersion: TComboBox
              Left = 128
              Top = 26
              Width = 89
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 0
            end
          end
          object Panel42: TPanel
            Left = 1
            Top = 134
            Width = 308
            Height = 55
            BevelOuter = bvNone
            TabOrder = 1
            object Label22: TLabel
              Left = 16
              Top = 8
              Width = 65
              Height = 13
              Caption = 'Start directory'
            end
            object lbStartDir: TSpeedButton
              Left = 267
              Top = 28
              Width = 22
              Height = 21
              Anchors = [akTop, akRight]
              Caption = '...'
              Flat = True
              OnClick = lbStartDirClick
            end
            object edStartDir: TEdit
              Left = 16
              Top = 28
              Width = 252
              Height = 21
              TabOrder = 0
            end
          end
        end
      end
    end
  end
  object btSave: TButton
    Left = 48
    Top = 372
    Width = 105
    Height = 22
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 1
    OnClick = btSaveClick
  end
  object btCancel: TButton
    Left = 160
    Top = 372
    Width = 105
    Height = 22
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
