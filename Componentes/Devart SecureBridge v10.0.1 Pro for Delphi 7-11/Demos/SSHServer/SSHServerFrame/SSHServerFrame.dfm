inherited SSHServerFrame: TSSHServerFrame
  Width = 443
  Height = 270
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 270
    Align = alClient
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
    object PageControl: TPageControl
      Left = 0
      Top = 0
      Width = 443
      Height = 234
      ActivePage = tsServer
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TabWidth = 81
      OnChange = PageControlChange
      OnChanging = PageControlChanging
      object tsServer: TTabSheet
        Caption = 'Server'
        object Panel11: TPanel
          Left = 0
          Top = 0
          Width = 435
          Height = 206
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Panel9: TPanel
            Tag = 1
            Left = 1
            Top = 8
            Width = 433
            Height = 191
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 0
            object Panel6: TPanel
              Left = 1
              Top = 1
              Width = 431
              Height = 189
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 0
              object Label9: TLabel
                Left = 16
                Top = 44
                Width = 75
                Height = 13
                Caption = 'Port for listening'
              end
              object Label1: TLabel
                Left = 16
                Top = 15
                Width = 68
                Height = 13
                Caption = 'Listen address'
              end
              object Label5: TLabel
                Left = 16
                Top = 71
                Width = 34
                Height = 13
                Caption = 'Banner'
              end
              object Label15: TLabel
                Left = 16
                Top = 98
                Width = 108
                Height = 13
                Caption = 'Path to keys and users'
              end
              object sbBanner: TSpeedButton
                Left = 393
                Top = 67
                Width = 22
                Height = 21
                Anchors = [akTop, akRight]
                Caption = '...'
                Flat = True
                OnClick = sbBannerClick
              end
              object sbStoragePath: TSpeedButton
                Left = 393
                Top = 94
                Width = 22
                Height = 21
                Anchors = [akTop, akRight]
                Caption = '...'
                Flat = True
                OnClick = sbStoragePathClick
              end
              object sbLogFile: TSpeedButton
                Left = 393
                Top = 121
                Width = 22
                Height = 21
                Anchors = [akTop, akRight]
                Caption = '...'
                Enabled = False
                Flat = True
                OnClick = sbLogFileClick
              end
              object shState: TShape
                Left = 104
                Top = 158
                Width = 17
                Height = 17
                Brush.Color = clRed
                Shape = stCircle
              end
              object sePort: TSpinEdit
                Left = 132
                Top = 39
                Width = 133
                Height = 22
                MaxValue = 65535
                MinValue = 0
                TabOrder = 1
                Value = 22
              end
              object cbbListenAddress: TComboBox
                Left = 132
                Top = 11
                Width = 133
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 0
              end
              object edBanner: TEdit
                Left = 132
                Top = 67
                Width = 261
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 2
              end
              object btConnectSSH: TButton
                Left = 132
                Top = 154
                Width = 106
                Height = 25
                Caption = 'Start server'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 6
                OnClick = btConnectSSHClick
              end
              object btDisconnectSSH: TButton
                Left = 243
                Top = 154
                Width = 106
                Height = 25
                Caption = 'Stop server'
                Enabled = False
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 7
                OnClick = btDisconnectSSHClick
              end
              object edStoragePath: TEdit
                Left = 132
                Top = 94
                Width = 261
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 3
                Text = '.'
              end
              object cbLog: TCheckBox
                Left = 16
                Top = 125
                Width = 105
                Height = 17
                Caption = 'Log events to file'
                TabOrder = 4
                OnClick = cbLogClick
              end
              object edLogFile: TEdit
                Left = 132
                Top = 121
                Width = 261
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                Enabled = False
                TabOrder = 5
              end
            end
          end
        end
      end
      object tsSecurity: TTabSheet
        Caption = 'Security'
        ImageIndex = 4
        object Panel10: TPanel
          Left = 0
          Top = 0
          Width = 435
          Height = 206
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Panel8: TPanel
            Tag = 1
            Left = 1
            Top = 8
            Width = 433
            Height = 305
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 0
            object Panel2: TPanel
              Left = 1
              Top = 1
              Width = 431
              Height = 76
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 0
              object Label2: TLabel
                Left = 16
                Top = 8
                Width = 94
                Height = 13
                Caption = 'Allowed ciphers:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object cb3DES: TCheckBox
                Left = 16
                Top = 30
                Width = 89
                Height = 17
                Caption = '3DES'
                Checked = True
                State = cbChecked
                TabOrder = 0
              end
              object cbBlowfish: TCheckBox
                Left = 16
                Top = 52
                Width = 89
                Height = 17
                Caption = 'Blowfish'
                Checked = True
                State = cbChecked
                TabOrder = 2
              end
              object cbAES128: TCheckBox
                Left = 128
                Top = 29
                Width = 89
                Height = 17
                Caption = 'AES128'
                Checked = True
                State = cbChecked
                TabOrder = 1
              end
              object cbCast128: TCheckBox
                Left = 235
                Top = 52
                Width = 89
                Height = 17
                Caption = 'Cast128'
                Checked = True
                State = cbChecked
                TabOrder = 3
              end
              object cbAES192: TCheckBox
                Left = 128
                Top = 52
                Width = 89
                Height = 17
                Caption = 'AES192'
                Checked = True
                State = cbChecked
                TabOrder = 4
              end
              object cbAES256: TCheckBox
                Left = 235
                Top = 29
                Width = 89
                Height = 17
                Caption = 'AES256'
                Checked = True
                State = cbChecked
                TabOrder = 5
              end
            end
            object Panel4: TPanel
              Left = 1
              Top = 78
              Width = 431
              Height = 54
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 1
              object Label3: TLabel
                Left = 16
                Top = 8
                Width = 140
                Height = 13
                Caption = 'Allowed authentications:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object cbPublicKey: TCheckBox
                Left = 16
                Top = 30
                Width = 89
                Height = 17
                Caption = 'Public key'
                Checked = True
                State = cbChecked
                TabOrder = 0
              end
              object cbPassword: TCheckBox
                Left = 128
                Top = 30
                Width = 89
                Height = 17
                Caption = 'Password'
                Checked = True
                State = cbChecked
                TabOrder = 1
              end
            end
            object Panel5: TPanel
              Left = 1
              Top = 133
              Width = 431
              Height = 85
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 2
              object Label4: TLabel
                Left = 16
                Top = 8
                Width = 61
                Height = 13
                Caption = 'Host keys:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object Label6: TLabel
                Left = 128
                Top = 8
                Width = 90
                Height = 13
                Caption = '(from Keys storage)'
              end
              object cbRSAKey: TCheckBox
                Left = 16
                Top = 30
                Width = 73
                Height = 17
                Caption = 'RSA key:'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = cbRSAKeyClick
              end
              object cbDSAKey: TCheckBox
                Left = 16
                Top = 58
                Width = 73
                Height = 17
                Caption = 'DSA key:'
                Checked = True
                State = cbChecked
                TabOrder = 2
                OnClick = cbRSAKeyClick
              end
              object cbbRSAKeyName: TComboBox
                Left = 128
                Top = 28
                Width = 289
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 13
                TabOrder = 1
                OnDropDown = cbbRSAKeyNameDropDown
              end
              object cbbDSAKeyName: TComboBox
                Left = 128
                Top = 56
                Width = 289
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 13
                TabOrder = 3
                OnDropDown = cbbRSAKeyNameDropDown
              end
            end
            object Panel12: TPanel
              Left = 1
              Top = 219
              Width = 431
              Height = 85
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 3
              object Label7: TLabel
                Left = 16
                Top = 8
                Width = 48
                Height = 13
                Caption = 'Options:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object Label8: TLabel
                Left = 16
                Top = 33
                Width = 97
                Height = 13
                Caption = 'ClientAliveCountMax'
              end
              object Label11: TLabel
                Left = 16
                Top = 53
                Width = 105
                Height = 26
                Caption = 'Max number of startup connections'
                WordWrap = True
              end
              object Label10: TLabel
                Left = 248
                Top = 33
                Width = 84
                Height = 13
                Caption = 'ClientAliveInterval'
              end
              object Label12: TLabel
                Left = 432
                Top = 33
                Width = 40
                Height = 13
                Caption = 'seconds'
              end
              object Label13: TLabel
                Left = 248
                Top = 61
                Width = 38
                Height = 13
                Caption = 'Timeout'
              end
              object Label14: TLabel
                Left = 432
                Top = 61
                Width = 40
                Height = 13
                Caption = 'seconds'
              end
              object seClientAliveCountMax: TSpinEdit
                Left = 128
                Top = 28
                Width = 89
                Height = 22
                Ctl3D = True
                MaxValue = 100
                MinValue = 1
                ParentCtl3D = False
                TabOrder = 0
                Value = 3
              end
              object seMaxStartups: TSpinEdit
                Left = 128
                Top = 56
                Width = 89
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 2
                Value = 20
              end
              object seClientAliveInterval: TSpinEdit
                Left = 340
                Top = 28
                Width = 89
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 1
                Value = 0
              end
              object seTimeout: TSpinEdit
                Left = 340
                Top = 56
                Width = 89
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 3
                Value = 15
              end
            end
          end
        end
      end
      object tsUsers: TTabSheet
        Caption = 'Users'
        ImageIndex = 1
        object PanelUsers: TPanel
          Left = 0
          Top = 0
          Width = 435
          Height = 206
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
      object tsKeyStorage: TTabSheet
        Caption = 'Keys storage'
        ImageIndex = 3
        object PanelKeys: TPanel
          Left = 0
          Top = 0
          Width = 435
          Height = 206
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
    object btOK: TButton
      Left = 230
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btOKClick
    end
    object btCancel: TButton
      Left = 335
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btCancelClick
    end
  end
  object ScFileStorage: TScFileStorage
    StoreUserPassword = False
    Left = 336
    Top = 16
  end
end
