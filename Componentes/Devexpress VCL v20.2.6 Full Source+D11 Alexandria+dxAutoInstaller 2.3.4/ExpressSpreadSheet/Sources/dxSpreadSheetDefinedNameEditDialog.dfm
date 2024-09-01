inherited dxSpreadSheetDefinedNameEditDialogForm: TdxSpreadSheetDefinedNameEditDialogForm
  AutoSize = False
  BorderStyle = bsSizeable
  ClientHeight = 236
  ClientWidth = 344
  Constraints.MinHeight = 275
  Constraints.MinWidth = 360
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 344
    Height = 236
    Align = alClient
    AutoSize = False
    inherited beAreaSelector: TcxButtonEdit
      Width = 324
    end
    object teName: TcxTextEdit [1]
      Left = 73
      Top = 40
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 261
    end
    object meComment: TcxMemo [2]
      Left = 73
      Top = 94
      Properties.ScrollBars = ssVertical
      Properties.WordWrap = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Height = 71
      Width = 261
    end
    object beReference: TcxButtonEdit [3]
      Left = 73
      Top = 171
      RepositoryItem = ertiArea
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      OnEnter = beReferenceEnter
      OnExit = beReferenceExit
      Width = 261
    end
    object cbScope: TcxComboBox [4]
      Left = 73
      Top = 67
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 261
    end
    object btnOK: TcxButton [5]
      Left = 178
      Top = 201
      Width = 75
      Height = 25
      Caption = 'btnOK'
      Default = True
      ModalResult = 1
      TabOrder = 5
      OnClick = btnOKClick
    end
    object btnCancel: TcxButton [6]
      Left = 259
      Top = 201
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 6
      OnClick = btnCancelClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ItemIndex = 5
    end
    object liName: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'liName:'
      Control = teName
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liComment: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = 'liComment:'
      Control = meComment
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liReference: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'liReference:'
      Control = beReference
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liScope: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'liScope:'
      Control = cbScope
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liBtnOK: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liBtnCancel: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 5
    end
  end
  inherited LayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 496
    inherited LayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  inherited EditRepository: TcxEditRepository
    Left = 464
    PixelsPerInch = 96
  end
end
