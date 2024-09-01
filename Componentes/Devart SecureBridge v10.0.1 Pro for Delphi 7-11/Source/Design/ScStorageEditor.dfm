inherited ScStorageForm: TScStorageForm
  Left = 342
  Top = 232
  Width = 540
  Height = 500
  Constraints.MinHeight = 500
  Constraints.MinWidth = 540
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 425
    Width = 532
    inherited imCorner: TImage
      Left = 520
    end
    inherited btOk: TBitBtn
      Left = 448
      Caption = 'Close'
    end
    inherited btCancel: TBitBtn
      Left = 369
      Visible = False
    end
  end
  inherited PageControl: TPageControl
    Width = 516
    Height = 417
    ActivePage = shKeys
    object shKeys: TTabSheet
      Caption = '&Keys'
    end
    object shUsers: TTabSheet
      Caption = '&Users'
      ImageIndex = 1
    end
    object shCertificates: TTabSheet
      Caption = '&Certificates'
      ImageIndex = 2
    end
  end
end
