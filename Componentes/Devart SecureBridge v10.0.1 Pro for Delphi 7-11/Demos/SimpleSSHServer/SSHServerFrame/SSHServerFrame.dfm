inherited SSHServerFrame: TSSHServerFrame
  Width = 451
  Height = 305
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 305
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
      Width = 451
      Height = 305
      ActivePage = tsServer
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TabWidth = 81
      object tsServer: TTabSheet
        Caption = 'Server'
        object Panel11: TPanel
          Left = 0
          Top = 0
          Width = 443
          Height = 277
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Panel9: TPanel
            Tag = 1
            Left = 1
            Top = 8
            Width = 281
            Height = 163
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 0
            object PanelKey: TPanel
              Left = 1
              Top = 1
              Width = 279
              Height = 96
              BevelOuter = bvNone
              TabOrder = 0
              object Label5: TLabel
                Left = 14
                Top = 26
                Width = 50
                Height = 13
                Caption = 'FingerPrint'
              end
              object Label1: TLabel
                Left = 16
                Top = 3
                Width = 54
                Height = 13
                Caption = 'RSA key:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object edFingerPrint: TEdit
                Left = 14
                Top = 44
                Width = 249
                Height = 21
                Color = cl3DLight
                ReadOnly = True
                TabOrder = 0
              end
              object Panel7: TPanel
                Left = 14
                Top = 69
                Width = 249
                Height = 24
                BevelOuter = bvNone
                Color = 48127
                TabOrder = 1
                object btGenerate: TSpeedButton
                  Left = 1
                  Top = 1
                  Width = 123
                  Height = 22
                  Caption = 'Re&generate'
                  Flat = True
                  Transparent = False
                  OnClick = btGenerateClick
                end
                object btExportPublic: TSpeedButton
                  Left = 125
                  Top = 1
                  Width = 123
                  Height = 22
                  Caption = 'Export &public key'
                  Flat = True
                  Transparent = False
                  OnClick = btExportPublicClick
                end
              end
              object cbRandomization: TCheckBox
                Left = 149
                Top = 8
                Width = 116
                Height = 17
                Hint = 'Generation random data increase connection reliability'
                Caption = 'Silent randomization'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 2
              end
            end
            object Panel6: TPanel
              Left = 1
              Top = 98
              Width = 279
              Height = 64
              BevelOuter = bvNone
              TabOrder = 1
              object Label9: TLabel
                Left = 16
                Top = 15
                Width = 75
                Height = 13
                Caption = 'Port for listening'
              end
              object sePort: TSpinEdit
                Left = 139
                Top = 11
                Width = 123
                Height = 22
                MaxValue = 65535
                MinValue = 0
                TabOrder = 0
                Value = 22
                OnChange = sePortChange
              end
              object Panel3: TPanel
                Left = 14
                Top = 37
                Width = 249
                Height = 24
                BevelOuter = bvNone
                Color = 48127
                TabOrder = 1
                object btConnectSSH: TSpeedButton
                  Left = 1
                  Top = 1
                  Width = 123
                  Height = 22
                  Caption = 'Start server'
                  Flat = True
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                  Transparent = False
                  OnClick = btConnectSSHClick
                end
                object btDisconnectSSH: TSpeedButton
                  Left = 125
                  Top = 1
                  Width = 123
                  Height = 22
                  Caption = 'Stop server'
                  Enabled = False
                  Flat = True
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                  Transparent = False
                  OnClick = btDisconnectSSHClick
                end
              end
            end
          end
        end
      end
      object tsUsers: TTabSheet
        Caption = 'Users'
        ImageIndex = 1
        object Panel12: TPanel
          Left = 0
          Top = 0
          Width = 443
          Height = 277
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label3: TLabel
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
          object lbUserName: TListBox
            Left = 8
            Top = 24
            Width = 90
            Height = 215
            Style = lbOwnerDrawFixed
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
            OnClick = lbUserNameClick
          end
          object Panel8: TPanel
            Left = 8
            Top = 244
            Width = 249
            Height = 24
            Anchors = [akLeft, akBottom]
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 1
            object btNewUser: TSpeedButton
              Left = 1
              Top = 1
              Width = 123
              Height = 22
              Caption = '&New'
              Flat = True
              Transparent = False
              OnClick = btNewUserClick
            end
            object btDeleteUser: TSpeedButton
              Left = 125
              Top = 1
              Width = 123
              Height = 22
              Caption = '&Delete'
              Flat = True
              Transparent = False
              OnClick = btDeleteUserClick
            end
          end
          object Panel10: TPanel
            Tag = 1
            Left = 105
            Top = 8
            Width = 332
            Height = 260
            Anchors = [akTop, akRight, akBottom]
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 2
            object PanelUser: TPanel
              Left = 1
              Top = 1
              Width = 330
              Height = 258
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvNone
              TabOrder = 0
              object Label4: TLabel
                Left = 14
                Top = 12
                Width = 51
                Height = 13
                Caption = 'User name'
              end
              object Label7: TLabel
                Left = 14
                Top = 60
                Width = 46
                Height = 13
                Caption = 'Password'
              end
              object edUserName: TEdit
                Left = 14
                Top = 26
                Width = 304
                Height = 21
                TabOrder = 0
                OnExit = edUserNameExit
              end
              object edPassword: TEdit
                Left = 14
                Top = 76
                Width = 304
                Height = 21
                PasswordChar = '*'
                TabOrder = 1
                OnExit = edUserNameExit
              end
            end
          end
        end
      end
      object tsLog: TTabSheet
        Caption = 'Log'
        ImageIndex = 2
        object sgLog: TStringGrid
          Left = 0
          Top = 0
          Width = 443
          Height = 277
          Align = alClient
          ColCount = 7
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
          TabOrder = 0
          ColWidths = (
            63
            150
            130
            90
            64
            90
            64)
        end
      end
    end
  end
  object ScSSHServer: TScSSHServer
    KeyNameRSA = 'SBSSHServer_RSA'
    KeyNameDSA = 'SBSSHServer_DSA'
    Options.AllowEmptyPassword = True
    Storage = ScFileStorage
    SFTPServer = ScSFTPServer
    AfterClientConnect = ScSSHServerAfterClientConnect
    AfterClientDisconnect = ScSSHServerAfterClientDisconnect
    BeforeChannelConnect = ScSSHServerBeforeChannelConnect
    AfterChannelDisconnect = ScSSHServerAfterChannelDisconnect
    Left = 304
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Filter = 'OpenSSL format (*.pub)|*.pub|IETF format (*.ietfpub)|*.ietfpub'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Export key to...'
    Left = 336
    Top = 8
  end
  object ScFileStorage: TScFileStorage
    Left = 272
    Top = 8
  end
  object ScSFTPServer: TScSFTPServer
    Left = 376
    Top = 8
  end
end
