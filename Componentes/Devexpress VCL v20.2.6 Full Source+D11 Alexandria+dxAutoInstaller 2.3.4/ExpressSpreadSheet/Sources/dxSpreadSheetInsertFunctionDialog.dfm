object dxSpreadSheetInsertFunctionDialogForm: TdxSpreadSheetInsertFunctionDialogForm
  Left = 0
  Top = 0
  ActiveControl = lbFunctions
  BorderIcons = [biSystemMenu]
  Caption = 'Insert Function'
  ClientHeight = 361
  ClientWidth = 424
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 440
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 424
    Height = 361
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object btnCancel: TcxButton
      Left = 329
      Top = 326
      Width = 85
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnOk: TcxButton
      Left = 238
      Top = 326
      Width = 85
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object cbCategory: TcxComboBox
      Left = 64
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbCategoryPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 350
    end
    object lbFunctions: TcxListBox
      Left = 10
      Top = 79
      Width = 404
      Height = 129
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 4
      OnClick = lbFunctionsClick
      OnDblClick = lbFunctionsDblClick
    end
    object teSearchBox: TcxButtonEdit
      Left = 10
      Top = 53
      Properties.Buttons = <
        item
          Glyph.SourceDPI = 96
          Glyph.SourceHeight = 16
          Glyph.SourceWidth = 16
          Glyph.Data = {
            3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
            462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
            A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
            6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
            2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
            20793D22307078222076696577426F783D223020302033322033322220737479
            6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
            2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
            262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
            73706163653D227072657365727665223E2E426C61636B7B66696C6C3A233732
            373237323B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23
            3131373744373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
            B95F32223E0D0A09093C7061746820636C6173733D22426C61636B2220643D22
            4D31332C31374C322C32386C322C326C31312D31316C312D316C2D322D324C31
            332C31377A222F3E0D0A09093C673E0D0A0909093C673E0D0A090909093C7061
            746820636C6173733D22426C75652220643D224D32302C34632D342E342C302D
            382C332E362D382C3873332E362C382C382C3873382D332E362C382D38533234
            2E342C342C32302C347A204D32302C3138632D332E332C302D362D322E372D36
            2D3673322E372D362C362D3673362C322E372C362C3620202623393B2623393B
            2623393B2623393B5332332E332C31382C32302C31387A222F3E0D0A0909093C
            2F673E0D0A09093C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
          Kind = bkGlyph
        end
        item
          Glyph.SourceDPI = 96
          Glyph.SourceHeight = 16
          Glyph.SourceWidth = 16
          Glyph.Data = {
            3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
            462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
            617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
            2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
            77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
            22307078222076696577426F783D2230203020333220333222207374796C653D
            22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
            3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
            303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
            63653D227072657365727665223E2E426C75657B66696C6C3A23313137374437
            3B7D262331333B262331303B2623393B2E59656C6C6F777B66696C6C3A234646
            423131353B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A
            233732373237323B7D262331333B262331303B2623393B2E477265656E7B6669
            6C6C3A233033394332333B7D262331333B262331303B2623393B2E5265647B66
            696C6C3A234431314331433B7D262331333B262331303B2623393B2E7374307B
            6F7061636974793A302E37353B7D262331333B262331303B2623393B2E737431
            7B6F7061636974793A302E353B7D3C2F7374796C653E0D0A3C672069643D2244
            656C657465223E0D0A09093C673E0D0A0909093C7061746820636C6173733D22
            426C61636B2220643D224D31382E382C31366C362E392D362E3963302E342D30
            2E342C302E342D312C302D312E346C2D312E342D312E34632D302E342D302E34
            2D312D302E342D312E342C304C31362C31332E324C392E312C362E33632D302E
            342D302E342D312D302E342D312E342C3020202623393B2623393B2623393B4C
            362E332C372E37632D302E342C302E342D302E342C312C302C312E346C362E39
            2C362E396C2D362E392C362E39632D302E342C302E342D302E342C312C302C31
            2E346C312E342C312E3463302E342C302E342C312C302E342C312E342C306C36
            2E392D362E396C362E392C362E3920202623393B2623393B2623393B63302E34
            2C302E342C312C302E342C312E342C306C312E342D312E3463302E342D302E34
            2C302E342D312C302D312E344C31382E382C31367A222F3E0D0A09093C2F673E
            0D0A09093C673E0D0A0909093C7061746820636C6173733D22426C61636B2220
            643D224D31382E382C31366C362E392D362E3963302E342D302E342C302E342D
            312C302D312E346C2D312E342D312E34632D302E342D302E342D312D302E342D
            312E342C304C31362C31332E324C392E312C362E33632D302E342D302E342D31
            2D302E342D312E342C3020202623393B2623393B2623393B4C362E332C372E37
            632D302E342C302E342D302E342C312C302C312E346C362E392C362E396C2D36
            2E392C362E39632D302E342C302E342D302E342C312C302C312E346C312E342C
            312E3463302E342C302E342C312C302E342C312E342C306C362E392D362E396C
            362E392C362E3920202623393B2623393B2623393B63302E342C302E342C312C
            302E342C312E342C306C312E342D312E3463302E342D302E342C302E342D312C
            302D312E344C31382E382C31367A222F3E0D0A09093C2F673E0D0A093C2F673E
            0D0A3C2F7376673E0D0A}
          Kind = bkGlyph
          Visible = False
        end>
      Properties.OnButtonClick = teSearchBoxButtonClick
      Properties.OnChange = teSearchBoxPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      OnKeyDown = teSearchBoxKeyDown
      Width = 404
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcMainItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liCategory: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Category:'
      Control = cbCategory
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liFunctions: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Layout = clTop
      Control = lbFunctions
      ControlOptions.OriginalHeight = 97
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lliFunctionDescription: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      SizeOptions.Height = 74
      Padding.Top = 8
      Padding.AssignedValues = [lpavTop]
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.WordWrap = True
      Index = 5
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 6
    end
    object lliFunctionDefinition: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      LayoutLookAndFeel = dxLayoutCxLookAndFeelBold
      SizeOptions.Height = 14
      CaptionOptions.WordWrap = True
      Index = 4
    end
    object liSearchBox: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Select a function:'
      CaptionOptions.Layout = clTop
      Control = teSearchBox
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 392
    Top = 8
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeelBold: TdxLayoutCxLookAndFeel
      ItemOptions.CaptionOptions.Font.Charset = DEFAULT_CHARSET
      ItemOptions.CaptionOptions.Font.Color = clWindowText
      ItemOptions.CaptionOptions.Font.Height = -11
      ItemOptions.CaptionOptions.Font.Name = 'Tahoma'
      ItemOptions.CaptionOptions.Font.Style = [fsBold]
      ItemOptions.CaptionOptions.UseDefaultFont = False
      PixelsPerInch = 96
    end
  end
  object Actions: TActionList
    Left = 360
    Top = 8
    object acFind: TAction
      SecondaryShortCuts.Strings = (
        'Ctrl+F')
      ShortCut = 114
      OnExecute = acFindExecute
    end
  end
end
