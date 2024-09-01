object fmConvertControlsDialog: TfmConvertControlsDialog
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Convert Embedded Controls'
  ClientHeight = 361
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 609
    Height = 361
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = llafCX
    object lvRadioButtonList: TcxListView
      Left = 10000
      Top = 10000
      Width = 561
      Height = 216
      Checkboxes = True
      Columns = <
        item
          MaxWidth = 27
          MinWidth = 27
          Width = 27
        end
        item
          Caption = 'Caption'
          Width = 170
        end
        item
          Caption = 'Name'
          Width = 170
        end
        item
          Caption = 'Class'
          Width = 90
        end
        item
          Alignment = taCenter
          Caption = 'Group Index'
          Width = 70
        end>
      ParentShowHint = False
      PopupMenu = ppmControls
      ReadOnly = True
      RowSelect = True
      ShowHint = True
      SmallImages = ilCheckBoxes
      TabOrder = 8
      ViewStyle = vsReport
      Visible = False
      OnChange = lvRadioButtonListChange
      OnChanging = lvChanging
      OnColumnClick = lvColumnClick
      OnCustomDrawItem = lvCustomDrawItemHandler
      OnInfoTip = lvInfoTipHandler
    end
    object btnConvert: TcxButton
      Left = 443
      Top = 318
      Width = 75
      Height = 25
      Caption = 'Convert'
      TabOrder = 9
      OnClick = btnConvertClick
    end
    object btnCancel: TcxButton
      Left = 524
      Top = 318
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 10
      OnClick = btnCancelClick
    end
    object btnRadioButtonListUncheckAll: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 25
      Action = acUncheckAll
      TabOrder = 7
      Visible = False
    end
    object btnRadioButtonListCheckAll: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 25
      Action = acCheckAll
      TabOrder = 6
      Visible = False
    end
    object lvLabelList: TcxListView
      Left = 10000
      Top = 10000
      Width = 561
      Height = 216
      Checkboxes = True
      Columns = <
        item
          MaxWidth = 27
          MinWidth = 27
          Width = 27
        end
        item
          Caption = 'Caption'
          Width = 220
        end
        item
          Caption = 'Name'
          Width = 220
        end
        item
          Caption = 'Class'
          Width = 60
        end>
      ParentShowHint = False
      PopupMenu = ppmControls
      ReadOnly = True
      RowSelect = True
      ShowHint = True
      SmallImages = ilCheckBoxes
      TabOrder = 5
      ViewStyle = vsReport
      Visible = False
      OnChange = lvChange
      OnChanging = lvChanging
      OnColumnClick = lvColumnClick
      OnCustomDrawItem = lvCustomDrawItemHandler
      OnInfoTip = lvInfoTipHandler
    end
    object lvCheckBoxList: TcxListView
      Left = 24
      Top = 75
      Width = 561
      Height = 216
      Checkboxes = True
      Columns = <
        item
          MaxWidth = 27
          MinWidth = 27
          Width = 27
        end
        item
          Caption = 'Caption'
          Width = 210
        end
        item
          Caption = 'Name'
          Width = 210
        end
        item
          Caption = 'Class'
          Width = 80
        end>
      ParentShowHint = False
      PopupMenu = ppmControls
      ReadOnly = True
      RowSelect = True
      ShowHint = True
      SmallImages = ilCheckBoxes
      TabOrder = 2
      ViewStyle = vsReport
      OnChange = lvChange
      OnChanging = lvChanging
      OnColumnClick = lvColumnClick
      OnCustomDrawItem = lvCustomDrawItemHandler
      OnInfoTip = lvInfoTipHandler
    end
    object btnLabelListCheckAll: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 25
      Action = acCheckAll
      TabOrder = 3
      Visible = False
    end
    object btnLabelListUncheckAll: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 25
      Action = acUncheckAll
      TabOrder = 4
      Visible = False
    end
    object btnCheckBoxListCheckAll: TcxButton
      Left = 429
      Top = 44
      Width = 75
      Height = 25
      Action = acCheckAll
      TabOrder = 0
    end
    object btnCheckBoxListUncheckAll: TcxButton
      Left = 510
      Top = 44
      Width = 75
      Height = 25
      Action = acUncheckAll
      TabOrder = 1
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object lilvRadioButtonList: TdxLayoutItem
      Parent = lgRadioButtonList
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Layout = clTop
      Control = lvRadioButtonList
      ControlOptions.OriginalHeight = 181
      ControlOptions.OriginalWidth = 465
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnConvert: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnConvert
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnCancel: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lgButtons: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lliRadioButtonListCaption: TdxLayoutLabeledItem
      Parent = lgRadioButtonListHeader
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'Controls to convert:'
      Index = 0
    end
    object lgRadioButtonListHeader: TdxLayoutGroup
      Parent = lgRadioButtonList
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object libtnRadioButtonListUncheckAll: TdxLayoutItem
      Parent = lgRadioButtonListHeader
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = btnRadioButtonListUncheckAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object libtnRadioButtonListCheckAll: TdxLayoutItem
      Parent = lgRadioButtonListHeader
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = btnRadioButtonListCheckAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lgRadioButtonList: TdxLayoutGroup
      Parent = lgControlLists
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Radio Buttons'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 2
    end
    object lilvLabelList: TdxLayoutItem
      Parent = lgLabelList
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lvLabelList
      ControlOptions.OriginalHeight = 219
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lilvCheckBoxList: TdxLayoutItem
      Parent = lgCheckBoxList
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lvCheckBoxList
      ControlOptions.OriginalHeight = 219
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnLabelListCheckAll: TdxLayoutItem
      Parent = lgLabelListHeader
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = btnLabelListCheckAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnLabelListUncheckAll: TdxLayoutItem
      Parent = lgLabelListHeader
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = btnLabelListUncheckAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object libtnCheckBoxListCheckAll: TdxLayoutItem
      Parent = lgCheckBoxListHeader
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = btnCheckBoxListCheckAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnCheckBoxListUncheckAll: TdxLayoutItem
      Parent = lgCheckBoxListHeader
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = btnCheckBoxListUncheckAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lgCheckBoxList: TdxLayoutGroup
      Parent = lgControlLists
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Check Boxes'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lgLabelList: TdxLayoutGroup
      Parent = lgControlLists
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Labels'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lliCheckBoxListCaption: TdxLayoutLabeledItem
      Parent = lgCheckBoxListHeader
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'Controls to convert:'
      Index = 0
    end
    object lliLabelListCaption: TdxLayoutLabeledItem
      Parent = lgLabelListHeader
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'Controls to convert:'
      Index = 0
    end
    object lgCheckBoxListHeader: TdxLayoutGroup
      Parent = lgCheckBoxList
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgLabelListHeader: TdxLayoutGroup
      Parent = lgLabelList
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lgControlLists: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldTabbed
      ShowBorder = False
      OnTabChanged = lgControlListsTabChanged
      Index = 0
    end
    object liiWarning: TdxLayoutImageItem
      Parent = lgButtons
      AlignHorz = ahClient
      AlignVert = avClient
      Offsets.Right = 25
      Visible = False
      CaptionOptions.Text = 
        'One or more elements of the selected controls cannot be converte' +
        'd. The controls that include such elements are grayed out in the' +
        ' respective lists. Hover the mouse pointer over these controls t' +
        'o see detailed information.'
      CaptionOptions.WordWrap = True
      CaptionOptions.Layout = clRight
      Image.SourceDPI = 96
      Image.Data = {
        89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
        F40000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C0000001B744558745469746C65005761726E696E673B4E6F7469
        6669636174696F6E3BB6E779860000049E49444154785EED94794CD37718C6B7
        653AC45DC9FE59B66CF18F89324427E31045C2983B5C9685C503C7C479016343
        03988906141430A3188E5105B103741C010B483946E528542BB08A4811390475
        18CA515A282D679167DFF797C589A2CE49354B6CF2499E3CEFFBE57979DFB4CF
        0178AA3CFBFC7F91C539BD517BCC793F6377E96107D3279DFF82FCA8F3F1F633
        07D1F6FB3E30CD27EF49853F5FC27358511BEF3C61D05D8541D70AD2E451CDE8
        E9D1DE8B4D6BE29C9A6ECAE230AE9673749C8DC11F7CA766BECF92B946FFEFCF
        45AD8CB822DC09C38002633D950437C415A10F582DCAA85B1087D9DB55473B1A
        467A6A31D625C178AF8C60BA02C34A19586DA238D4DED12843446EB5303DCB73
        68BC2189C1586F3546FB5B202911119C1E65435C2FE3E15CE4CA56BEF7E29767
        7CF592F0E5118A746F18FA9B31DA2D43B22001CB9C3610A4396F4C5909459A37
        586FEC8C6EA120C8D64E7A68B961B8A71EC31D45185156209E1F035BC7F51C09
        4762396FF88608FA0E0958EF447EA0CD4733324488ABD99CB290658D6D677818
        53D561A82D13237F1623EFD409583BAC2548934735364401AE1685A1FC807D1B
        7FFBA2571E7BF5C58136E11793B6C1A0BB097D4B06230D43EDB9904BF360B5FC
        6B82347954E37A86D9301793B7431C64F3783F50423F2BEB9240DBF1A1CE3AB6
        DE22E81A5338F44D69E86C966289DD570469F2FEA9B7A4437F5D0CF6F656B6BF
        D5C7FFE914BBBE986752F4D3878AE6BC8318533741DB701C5A45E26D46BBE5B0
        5AF625417A4A8D7A752D9968CEDD8FE2DDD6D762DDCD5F7DE4D5E7F92E0DAF3E
        B2915BFDE0E51468EB8E4C61E466253EFD7C0D419ABCA9D41F83BE35073547DD
        21F2B38A7FA4536478595A17FA2E1D1F6815B3750AA1A90AC7C085280CD4C6DE
        467FF53442020308D277D6A897DE3042A1A91380FDADC92CEFC5ABFED5297C57
        BD6B72FAC7258A4B277762A4B31AAA727FA84A7D197EE89304402DDD078D2C14
        DA4B020CB517328AA0AD17904735EAE17AB93765FED09C0F47FD096F887C3EB8
        11ED6AF6DA43579FE9B92854CA7361E1552C280CBDE21FEE415D1984868A9370
        FBD6036E6E1EA4C99BB69706E997C7401AE982535E96890F3C45CA26F3A5D99E
        96E3AADA340CD6A7A0A7D0635A066AE3E1E3E589F7AD5613A4C9BB6FBFAAD41F
        7D3571C8F1B49C4CDD6CF1D9B4A7D8E5FCCEECCC2D1675F2444F76D30274177D
        8F2ED177D3A23E1F81D0BD3B306F813DC7C13D3BC8A3DAF4E46F814AB2171705
        1E38B57551C76197F75EBF67F5A91B1786941F58CDC20BD157190C65AEDB7DE9
        CADF064D431612780104A7BB0AB63DF88D6833D4E70EA12CD819E9EEE682BB4F
        F16286BB797B47F92F186C4845DFD970287336A053B8EEFE64BBA2B76C0F419A
        BC87D29DEF816BA2BDA02C9639EBCE016627BB9A955445AD43BFFC283435B150
        571D865AF63387A63A8AF307EA92A055FC86C1C62CE89A73A16BCAC1E01521B4
        9733A0AD3F89FE0BF1E0DECA78505504A3B734003D625F74177841797A1394D9
        6E90867D82A4F566C52CF3A53B0798B573C55B8EBFAE9DDF94FECD8249068CC4
        A460CDFC86EDB66FDADCBD01BAC75CC6DB8C850C0B23B1F0EF0CD3E9BE8E64CC
        66CC312226943125FCD907C053E52F6A6A3577F3F307D50000000049454E44AE
        426082}
      Index = 0
    end
  end
  object llaflMain: TdxLayoutLookAndFeelList
    Left = 56
    Top = 104
    object llafCX: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
      PixelsPerInch = 96
    end
  end
  object ppmControls: TPopupMenu
    Left = 56
    Top = 152
    object miControlsCheckAll: TMenuItem
      Action = acCheckAll
    end
    object miControlsUncheckAll: TMenuItem
      Action = acUncheckAll
    end
  end
  object alMain: TActionList
    Left = 56
    Top = 200
    object acCheckAll: TAction
      Caption = 'Check All'
      OnExecute = acCheckAllExecute
    end
    object acUncheckAll: TAction
      Caption = 'Uncheck All'
      OnExecute = acUncheckAllExecute
    end
  end
  object ilCheckBoxes: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 16252984
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507E30A190E071E847545960000001D69
          545874436F6D6D656E7400000000004372656174656420776974682047494D50
          642E65070000003E4944415438CB63FCFFFF3F03258085818181819191F10CA9
          1AFFFFFF6F023700598018806C2113038560D4805103A862000BB6E4490A60A4
          343752EC0500EE110EEEDA14DA590000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507E30A190E082F523359630000001D69
          545874436F6D6D656E7400000000004372656174656420776974682047494D50
          642E65070000009C4944415438CBAD93CD0D83300C85BF877A61886CD11D58A6
          5736E1C422DC39B30273F4F87A31558A50214D2D599122BF1F27B66C53133700
          494B29D0F6FD4D905F5C895CB0A1329A02D5246992D41513484AC008ACC05C44
          B003F7B69F870492BAB098AE82F70EE6281CA3DF53F0F67D004B9C2D300053E4
          00B4B6C973ABB7FDF906A1D287EA77E57C120F481E7F9F835307BFEC03806AB7
          B1BA8517C79152FC6A5018FB0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507E30A190E0914FA2381060000001D69
          545874436F6D6D656E7400000000004372656174656420776974682047494D50
          642E65070000004D4944415438CB63FCFFFF3F03258085818181819191F10CA9
          1AFFFFFF6F023700598018806C2113038580059FE9B89C8D0C2876C1A801B488
          46521214755D404E7E6060606060A4343752EC050057A21623A86BB5BC000000
          0049454E44AE426082}
      end>
  end
end
