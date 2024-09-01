inherited SFTPClientFrame: TSFTPClientFrame
  Width = 524
  Height = 470
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 224
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
      Width = 285
      Height = 219
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
        Height = 217
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
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 224
    Width = 524
    Height = 246
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinWidth = 285
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object Panel9: TPanel
      Left = 0
      Top = 0
      Width = 524
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Panel8: TPanel
        Left = 17
        Top = 1
        Width = 249
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 0
        object btStart: TSpeedButton
          Left = 1
          Top = 1
          Width = 123
          Height = 22
          Caption = 'Start SFTP'
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
          Caption = 'Stop SFTP'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btStopClick
        end
      end
    end
    object Panel13: TPanel
      Left = 0
      Top = 28
      Width = 421
      Height = 218
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Panel11: TPanel
        Tag = 1
        Left = 0
        Top = 0
        Width = 421
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 0
        object lbRootDir: TEdit
          Left = 18
          Top = 1
          Width = 402
          Height = 19
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          Text = '.'
        end
        object Panel7: TPanel
          Left = 1
          Top = 1
          Width = 17
          Height = 19
          BevelOuter = bvNone
          Caption = ' >'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object FileView: TTreeView
        Left = 0
        Top = 21
        Width = 421
        Height = 197
        Align = alClient
        BorderWidth = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        HideSelection = False
        Images = ImageList1
        Indent = 19
        ParentFont = False
        ReadOnly = True
        RowSelect = True
        ShowButtons = False
        ShowLines = False
        ShowRoot = False
        SortType = stText
        TabOrder = 1
        OnCompare = FileViewCompare
        OnDblClick = FileViewDblClick
        OnKeyDown = FileViewKeyDown
      end
    end
    object Panel14: TPanel
      Left = 421
      Top = 28
      Width = 103
      Height = 218
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object Panel4: TPanel
        Left = 5
        Top = 0
        Width = 93
        Height = 139
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 0
        object btMakeDir: TSpeedButton
          Left = 1
          Top = 1
          Width = 91
          Height = 22
          Caption = 'Make Dir'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btMakeDirClick
        end
        object btRemove: TSpeedButton
          Left = 1
          Top = 47
          Width = 91
          Height = 22
          Caption = 'Remove'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btRemoveClick
        end
        object btRename: TSpeedButton
          Left = 1
          Top = 24
          Width = 91
          Height = 22
          Caption = 'Rename'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btRenameClick
        end
        object btUpload: TSpeedButton
          Left = 1
          Top = 93
          Width = 91
          Height = 22
          Caption = 'Upload'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btUploadClick
        end
        object btDownload: TSpeedButton
          Left = 1
          Top = 70
          Width = 91
          Height = 22
          Caption = 'Download'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btDownloadClick
        end
        object btViewFile: TSpeedButton
          Left = 1
          Top = 116
          Width = 91
          Height = 22
          Caption = 'View file'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btViewFileClick
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
    KeyStorage = ScFileStorage
    AfterConnect = ScSSHClientAfterConnect
    BeforeConnect = ScSSHClientBeforeConnect
    AfterDisconnect = ScSSHClientAfterDisconnect
    OnServerKeyValidate = ScSSHClientServerKeyValidate
    OnAuthenticationPrompt = ScSSHClientAuthenticationPrompt
    Left = 44
    Top = 280
  end
  object ScFileStorage: TScFileStorage
    Left = 16
    Top = 280
  end
  object OpenDialog: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 280
  end
  object ImageList1: TImageList
    Left = 192
    Top = 280
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400C2A6A400C2A6A40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00C2A6A40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000042424200424242003E3E3E003E3E3E003A3A3A003A3A
      3A00363636003636360033333300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00C2A6A40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004747470075757500717171006D6D6D006D6D6D006D6D6D006A6A
      6A006A6A6A006A6A6A0035353500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEFA
      F500FEFCFB00FEFAF500FEFAF500FEFCFB00FEFAF500FEFAF500FEFCFB00FEFA
      F500FEFAF500C2A6A400000000000000000077848F006E7C8500626F7900535F
      6800444E5600333C4300252C3200191F240010151A0010151A0010151A001015
      1A0010151A0010151A0010151A00000000000000000000000000000000000000
      0000000000004A4A4A00818181007C7C7C007474740074747400787878007878
      78007B7B7B007B7B7B0038383800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEFA
      F500FEFAF500FEFAF500FEFAF500FEFAF500FEFAF500FEFAF500FEFAF500FEFA
      F500FEFAF500C2A6A400000000000000000079879200A2DFF00072CDEA004BB8
      E20039AEDB002FA9D90028A1D2002398C700228EBC002083AE001B81AB00177E
      A9001D799F0021749600191F2400000000000000000000000000000000000000
      0000000000004D4D4D008C8C8C00878787007D7D7D007D7D7D00848484008787
      87008A8A8A008A8A8A003B3B3B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEF7
      F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7
      F000FEF7F000C2A6A40000000000000000007C8A9500A9ECF60095E5FC0082DE
      FB0075D5FA006CD0F7005DC5F40052BEF00044B4EC0038ABE9002EA4E300229A
      DF001890D7001D799F0022292F00000000000000000000000000000000000000
      00000000000050505000949494008F8F8F00828282007F7F7F00464646004646
      460042424200424242003E3E3E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEF7
      F000FEF7F000FEF7F000FEF7F000FEF3E900FEF7F000FEF7F000FEF3E900FEF7
      F000FEF7F000C2A6A40000000000000000007F8E9800B6EEF8009BE8FB0095E5
      FC0082DEFB0075D5FA006ACAF4005DC5F4004FBCF00044B4EC0038ABE9002EA4
      E3001F96DC00177EA9002F363D00000000000000000000000000000000000000
      000000000000535353009D9D9D00989898008787870084848400494949000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEF3
      E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3
      E900FEF3E900C2A6A400000000000000000083959F00B6EEF800ABEDFB009BE8
      FB008BE2FC0082DEFB0075D5FA006ACAF4005DC5F4004FBCF00040B3ED0038AB
      E900269EE2001282B4003D454D00000000000000000000000000000000000000
      00000000000056565600A7A7A700A2A2A200929292008D8D8D004C4C4C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFF0
      E200FFF0E200FEF3E900FFEEDE00FEF3E900FFEEDE00FEF3E900FFEEDE00FEF3
      E900FFEEDE00C2A6A400000000000000000083959F00BEF0F900ABF0F700A5EC
      FB009BE8FB008BE2FC007EDCFC0075D5FA0065CCF7005DC5F4004FBCF00040B3
      ED002EA4E3000E8ABF004B555E000000000000000000000000005F5F5F005F5F
      5F005B5B5B0058585800AFAFAF00ABABAB009B9B9B00949494004E4E4E004E4E
      4E004A4A4A004A4A4A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEF3
      E900FFEEDE00FFF0E200FEF3E900FFEEDE00FFF0E200DDCFC200DDCFC200DDCF
      C200DDCFC200C2A6A4000000000000000000869AA300BEF0F900B6EEF800B6EE
      F800A5ECFB0095E5FC008BE2FC007EDCFC0075D5FA0065CCF7005DC5F4004FBC
      F00037A9E7000E8FC70059656D0000000000000000000000000061616100B4B4
      B400B1B1B100B1B1B100B6B6B600B3B3B300A6A6A6009D9D9D00979797009797
      9700A4A4A4004B4B4B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEE
      DE00FFEEDE00FFEEDE00FFEEDE00FFEEDE00FFEEDE00C3B4A800C3B4A800C3B4
      A800C3B4A800C2A6A40000000000000000008A9DA700BEF0F900B6EEF800B6EE
      F800ABF0F700A5ECFB0095E5FC008BE2FC007EDCFC0071D3FA0065CCF70057C3
      F3004FBCF0001B99CF0065727C00000000000000000000000000000000006262
      6200BEBEBE00C1C1C100C1C1C100BCBCBC00B6B6B600B0B0B000A8A8A800A8A8
      A800505050000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEE
      DE00FFEAD700FFEEDE00FFEAD700FFEAD700FFEEDE00B0A29600B0A29600B0A2
      9600B0A29600C2A6A40000000000000000008FA4AC00BEF0F900BEF0F900BEF0
      F900BEF0F900B4F2F800ABEDFB00A5ECFB0095E5FC008ADFFC007EDCFC0075D5
      FA006ACAF4005DC5F40065727C00000000000000000000000000000000000000
      000063636300C7C7C700C7C7C700C7C7C700C2C2C200C2C2C200BBBBBB005555
      5500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEA
      D700FFEAD700FFEAD700FFEAD700FFEAD700C9B9AC00FEFAF500FEF7F000E6DA
      D900C2A6A4000000000000000000000000008FA4AC008FA4AC008FA4AC008FA4
      AC008FA4AC008CA0A9008A9DA7008A9DA700869AA300869AA3008395A0008395
      A0008395A0008395A00000000000000000000000000000000000000000000000
      00000000000064646400CCCCCC00CCCCCC00CCCCCC00C8C8C8005A5A5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEA
      D700FFE6D000FFEAD700FFE6D000FFEAD700C5B5A900FEFAF500DDCFC200C2A6
      A400000000000000000000000000000000008FA4AC00B2E4EC00B6EEF800B6EE
      F800A9ECF60092E1F3008FA4AC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000065656500D1D1D100D1D1D1005F5F5F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFE6
      D000FFE6D000FFE6D000FFE6D000FFE6D000C9B9AC00DDCFC200C2A6A4000000
      000000000000000000000000000000000000000000008FA4AC008FA4AC008FA4
      AC008FA4AC008FA4AC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00C003FFFFFFFF0000C003FFFFFC010000
      C003FFFFF8010000C0030001F8010000C0030001F8010000C0030001F8010000
      C0030001F81F0000C0030001F81F0000C0030001C0030000C0030001C0030000
      C0030001E0070000C0030001F00F0000C0070003F81F0000C00F01FFFC3F0000
      C01F83FFFE7F0000C03FFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object ScSFTPClient: TScSFTPClient
    SSHClient = ScSSHClient
    OnDirectoryList = ScSFTPClientDirectoryList
    OnCreateLocalFile = ScSFTPClientCreateLocalFile
    Left = 80
    Top = 280
  end
  object SaveDialog: TSaveDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 152
    Top = 280
  end
end
