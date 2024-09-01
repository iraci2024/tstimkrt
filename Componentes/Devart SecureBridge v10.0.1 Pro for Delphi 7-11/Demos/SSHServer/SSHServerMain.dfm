inherited SSHServerForm: TSSHServerForm
  Left = 95
  Top = 103
  Width = 580
  Height = 493
  Caption = 'ServerForm'
  Constraints.MinHeight = 493
  Constraints.MinWidth = 580
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited MainPanel: TPanel
    Width = 572
    Height = 385
    Constraints.MinWidth = 550
    inherited shTopShape: TShape
      Width = 572
    end
    inherited pnDemo: TPanel
      Width = 572
      Height = 382
    end
    inherited pnSource: TPanel
      Width = 572
      Height = 382
      inherited Panel6: TPanel
        Top = 347
        Width = 572
      end
    end
  end
  inherited pnTopLabel: TPanel
    Width = 572
    inherited lbTitle: TLabel
      Width = 572
    end
    inherited lbAbout: TLabel
      Left = 862
    end
  end
  inherited pnTopPanel: TPanel
    Width = 572
    inherited pnShowSource: TPanel
      Left = 772
    end
  end
  object PopupMenu: TPopupMenu
    Left = 264
    Top = 45
    object miSettings: TMenuItem
      Caption = 'Settings...'
      Default = True
      OnClick = miSettingsClick
    end
    object miClose: TMenuItem
      Caption = 'Close'
      OnClick = miCloseClick
    end
  end
end
