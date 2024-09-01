object fmListViewItems: TfmListViewItems
  Left = 190
  Top = 158
  HelpContext = 26100
  BorderIcons = [biSystemMenu]
  Caption = 'ListView Items Editor'
  ClientHeight = 296
  ClientWidth = 654
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 550
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  PopupMode = pmAuto
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 654
    Height = 296
    Align = alClient
    TabOrder = 0
    AutoSize = True
    object btNewItem: TButton
      Left = 292
      Top = 28
      Width = 125
      Height = 29
      Anchors = [akTop, akRight]
      Caption = '&New Item'
      Default = True
      TabOrder = 1
      OnClick = btNewItemClick
    end
    object btDelete: TButton
      Left = 292
      Top = 98
      Width = 125
      Height = 29
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 3
      OnClick = btDeleteClick
    end
    object tvItems: TTreeView
      Left = 24
      Top = 30
      Width = 260
      Height = 207
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvSpace
      BevelKind = bkFlat
      BorderStyle = bsNone
      DragMode = dmAutomatic
      HideSelection = False
      Indent = 19
      TabOrder = 0
      OnChange = tvItemsChange
      OnChanging = tvItemsChanging
      OnDragDrop = tvItemsDragDrop
      OnDragOver = tvItemsDragOver
      OnEdited = tvItemsEdited
      OnEditing = tvItemsEditing
      OnKeyDown = tvItemsKeyDown
    end
    object btNewSubItem: TButton
      Left = 292
      Top = 63
      Width = 125
      Height = 29
      Anchors = [akTop, akRight]
      Caption = 'N&ew SubItem'
      TabOrder = 2
      OnClick = btNewSubItemClick
    end
    object edCaption: TEdit
      Left = 519
      Top = 30
      Width = 111
      Height = 17
      Anchors = [akTop, akRight]
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvSpace
      BorderStyle = bsNone
      TabOrder = 4
      OnChange = ValueChange
      OnExit = edCaptionExit
    end
    object edImageIndex: TEdit
      Left = 519
      Top = 57
      Width = 45
      Height = 17
      Anchors = [akTop, akRight]
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvSpace
      BorderStyle = bsNone
      TabOrder = 5
      OnChange = ValueChange
      OnExit = edImageIndexExit
    end
    object edStateImageIndex: TEdit
      Left = 519
      Top = 84
      Width = 45
      Height = 17
      Anchors = [akTop, akRight]
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvSpace
      BorderStyle = bsNone
      TabOrder = 6
      OnChange = ValueChange
      OnExit = edStateImageIndexExit
    end
    object cbGroupID: TComboBox
      Left = 517
      Top = 109
      Width = 113
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 7
      OnChange = cbGroupIDChange
    end
    object btOk: TButton
      Left = 266
      Top = 257
      Width = 90
      Height = 29
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 8
    end
    object btCancel: TButton
      Left = 362
      Top = 257
      Width = 90
      Height = 29
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 9
    end
    object btApply: TButton
      Left = 458
      Top = 257
      Width = 90
      Height = 29
      Anchors = [akRight, akBottom]
      Caption = '&Apply'
      TabOrder = 10
      OnClick = btApplyClick
    end
    object btHelp: TButton
      Left = 554
      Top = 257
      Width = 90
      Height = 29
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 11
      OnClick = btHelpClick
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = ' &Items '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'TreeView'
      CaptionOptions.Visible = False
      Control = tvItems
      ControlOptions.OriginalHeight = 225
      ControlOptions.OriginalWidth = 247
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'New'
      CaptionOptions.Visible = False
      Control = btNewItem
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'NewSub'
      CaptionOptions.Visible = False
      Control = btNewSubItem
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'Delete'
      CaptionOptions.Visible = False
      Control = btDelete
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object grpItemProperties: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Item Properties'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = grpItemProperties
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Caption:'
      Control = edCaption
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 111
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = 'I&mage Index:'
      Control = edImageIndex
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 45
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = '&State Index:'
      Control = edStateImageIndex
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 45
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Group:'
      Control = cbGroupID
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'OkButton'
      CaptionOptions.Visible = False
      Control = btOk
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Cancel'
      CaptionOptions.Visible = False
      Control = btCancel
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Apply'
      CaptionOptions.Visible = False
      Control = btApply
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Button7'
      CaptionOptions.Visible = False
      Control = btHelp
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
end
