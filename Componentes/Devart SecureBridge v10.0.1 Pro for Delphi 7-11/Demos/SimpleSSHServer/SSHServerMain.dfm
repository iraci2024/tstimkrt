inherited SSHServerForm: TSSHServerForm
  Left = 95
  Top = 103
  Width = 631
  Height = 390
  Caption = 'ServerForm'
  Constraints.MinHeight = 350
  PixelsPerInch = 96
  TextHeight = 13
  inherited MainPanel: TPanel
    Width = 623
    Height = 282
    Constraints.MinWidth = 550
    inherited shTopShape: TShape
      Width = 623
    end
    inherited pnDemo: TPanel
      Width = 623
      Height = 279
    end
    inherited pnSource: TPanel
      Width = 623
      Height = 279
      inherited Panel6: TPanel
        Top = 244
        Width = 623
      end
    end
  end
  inherited pnTopLabel: TPanel
    Width = 623
    inherited lbTitle: TLabel
      Width = 623
    end
    inherited lbAbout: TLabel
      Left = 863
    end
  end
  inherited pnTopPanel: TPanel
    Width = 623
    inherited pnShowSource: TPanel
      Left = 773
    end
  end
end
