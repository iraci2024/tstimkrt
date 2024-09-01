inherited dxX509CertificatePasswordDialogForm: TdxX509CertificatePasswordDialogForm
  Caption = 'Private Key Protection'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited edPassword: TcxTextEdit
      Top = 60
    end
    inherited btnCancel: TcxButton
      Top = 132
    end
    inherited btnOk: TcxButton
      Top = 132
    end
    inherited edRepeatPassword: TcxTextEdit
      Top = 105
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      ItemIndex = 2
    end
    inherited liNotes: TdxLayoutLabeledItem
      CaptionOptions.Text = 
        'To maintain security, the private key was protected with a passw' +
        'ord.'
      Index = 1
    end
    inherited liPassword: TdxLayoutItem
      CaptionOptions.Text = 'Enter password:'
      Index = 3
    end
    inherited liRepeatPassword: TdxLayoutItem
      Index = 4
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
