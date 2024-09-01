inherited dxPDFViewerPasswordDialogForm: TdxPDFViewerPasswordDialogForm
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited edPassword: TcxTextEdit
      Left = 113
      Top = 30
      Width = 262
    end
    inherited btnCancel: TcxButton
      Top = 106
    end
    inherited btnOk: TcxButton
      Top = 106
    end
    inherited edRepeatPassword: TcxTextEdit
    end
    inherited liPassword: TdxLayoutItem
      CaptionOptions.Layout = clLeft
      Index = 2
    end
    inherited liNotes: TdxLayoutLabeledItem
      Index = 4
    end
    inherited liRepeatPassword: TdxLayoutItem
      Index = 3
    end
    object liDocumentIsProtected: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Document is password protected.'
      CaptionOptions.WordWrap = True
      Index = 1
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
