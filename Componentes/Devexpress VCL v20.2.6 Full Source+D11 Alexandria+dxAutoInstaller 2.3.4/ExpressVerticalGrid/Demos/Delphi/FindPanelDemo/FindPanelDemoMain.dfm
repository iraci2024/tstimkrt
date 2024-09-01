inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressVerticalGrid Find Panel Demo'
  ClientHeight = 551
  ClientWidth = 989
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 989
    Caption = 
      'This demo illustrates the data filter and search functionality o' +
      'f the built-in Find Panel. Enter a search string into the Find P' +
      'anel'#39's box and use the Behavior combo box to highlight search re' +
      'sults or filter out all records that do not meet the search crit' +
      'eria. Click Help | About this demo for details.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 989
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 32
    Align = alTop
    TabOrder = 2
    Height = 113
    Width = 989
    object cbClearFindOnClose: TcxCheckBox
      Left = 640
      Top = 10
      Action = actClearFindOnClose
      TabOrder = 0
      Transparent = True
    end
    object cbShowClearButton: TcxCheckBox
      Left = 814
      Top = 58
      Action = actShowClearButton
      TabOrder = 1
      Transparent = True
    end
    object cbShowCloseButton: TcxCheckBox
      Left = 814
      Top = 10
      Action = actShowCloseButton
      TabOrder = 2
      Transparent = True
    end
    object cbShowFindButton: TcxCheckBox
      Left = 814
      Top = 34
      Action = actShowFindButton
      TabOrder = 3
      Transparent = True
    end
    object cbHighlightSearchResults: TcxCheckBox
      Left = 640
      Top = 34
      Action = actHighlightSearchResults
      TabOrder = 4
      Transparent = True
    end
    object seFindDelay: TcxSpinEdit
      Left = 293
      Top = 58
      Properties.MaxValue = 5000.000000000000000000
      Properties.MinValue = 100.000000000000000000
      Properties.OnChange = seFindDelayPropertiesChange
      TabOrder = 5
      Value = 1000
      Width = 334
    end
    object lbSearchDelay: TcxLabel
      Left = 185
      Top = 60
      Caption = 'Search Delay, ms:'
      Transparent = True
    end
    object icbFindFilterColumns: TcxImageComboBox
      Left = 293
      Top = 82
      Properties.Items = <>
      Properties.OnChange = icbFindFilterColumnsPropertiesChange
      TabOrder = 7
      Width = 334
    end
    object lbSearchableColumns: TcxLabel
      Left = 185
      Top = 84
      Caption = 'Searchable Rows:'
      Transparent = True
    end
    object cbeFindPanelPosition: TcxComboBox
      Left = 293
      Top = 34
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Top'
        'Bottom')
      Properties.OnChange = cbFindPanelPositionPropertiesChange
      TabOrder = 9
      Text = 'Top'
      Width = 334
    end
    object lbFindPanelPosition: TcxLabel
      Left = 185
      Top = 36
      Caption = 'Find Panel Position:'
      Transparent = True
    end
    object cbeDisplayMode: TcxComboBox
      Left = 293
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Never'
        'Manual (Focus the grid and press Ctrl+F)'
        'Always')
      Properties.OnChange = cbDisplayModePropertiesChange
      TabOrder = 11
      Text = 'Manual (Focus the grid and press Ctrl+F)'
      Width = 334
    end
    object lbDisplayMode: TcxLabel
      Left = 185
      Top = 12
      Caption = 'Display Mode:'
      Transparent = True
    end
    object cbUseDelayedSearch: TcxCheckBox
      Left = 640
      Top = 58
      Action = actUseDelayedSearch
      TabOrder = 13
      Transparent = True
    end
    object cbUseExtendedSyntax: TcxCheckBox
      Left = 640
      Top = 82
      Action = actUseExtendedSyntax
      TabOrder = 14
      Transparent = True
    end
    object lbBehavior: TcxLabel
      Left = 12
      Top = 36
      Caption = 'Behavior:'
      Transparent = True
    end
    object cbeBehavior: TcxComboBox
      Left = 74
      Top = 34
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Filter'
        'Search')
      Properties.OnChange = cbeBehaviorPropertiesChange
      Style.TextStyle = []
      TabOrder = 16
      Text = 'Filter'
      Width = 97
    end
    object cbShowPrevAndNextButtons: TcxCheckBox
      Left = 814
      Top = 82
      Action = actShowPrevAndNextButtons
      Style.TransparentBorder = False
      TabOrder = 17
      Transparent = True
    end
    object cxLabel1: TcxLabel
      Left = 12
      Top = 11
      Caption = 'Layout:'
      Transparent = True
    end
    object cbeLayout: TcxComboBox
      Left = 74
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Default'
        'Compact')
      Properties.OnEditValueChanged = cbeLayoutPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 19
      Text = 'Compact'
      Width = 97
    end
  end
  inherited memAboutText: TMemo
    Lines.Strings = (
      
        'Enter text within the Find Panel'#8217's box and the grid will highlig' +
        'ht all found text matches. The Find box shows the ordinal number' +
        ' of the search result corresponding to the current focus positio' +
        'n and the total number of search results.'
      ''
      
        'Use the Behavior combo box to switch the Find Panel from Search ' +
        'to Filter mode. In the latter case, the grid also filters out al' +
        'l records that do not meet the specified criteria.'
      ''
      
        'Check the '#8220'Use Extended Syntax'#8221' option to provide multiple searc' +
        'h or filter criteria in a search string. The grid interprets wor' +
        'ds separated by a spacebar character as individual criteria join' +
        'ed with the OR logical operator. The grid highlights matching te' +
        'xt in all records that meet at least one of these criteria. To s' +
        'earch a string including one or more spacebars as a single crite' +
        'rion, enclose the string in quotation marks. In addition to spac' +
        'ebars, the extended syntax allows you to use the following speci' +
        'fiers and wildcards to narrow search results:'
      ''
      
        '- The "+" specifier. Preceding a criterion with this specifier c' +
        'auses the grid to highlight or display only those data rows that' +
        ' match this criterion. The "+" specifier implements the logical ' +
        'AND operator. There should be no space character between the "+"' +
        ' sign and the criterion.'
      
        '- The "'#8211'" specifier. Preceding a criterion with "'#8211'" excludes dat' +
        'a rows that match this criterion from search results. There shou' +
        'ld be no space between the "'#8211'" sign and the criterion.'
      
        '- The percent ("%") wildcard. This wildcard substitutes any numb' +
        'er of characters in a criterion.'
      
        '- The underscore ("_") wildcard. This wildcard substitutes any s' +
        'ingle character in a criterion.'
      ''
      'Try various options to adjust the Find Panel'#39's look & feel.'
      ''
      
        'All the editing capabilities are enabled in this demo for you to' +
        ' try them out in combination with the Find Panel.')
  end
  object VerticalGrid: TcxDBVerticalGrid [4]
    Left = 0
    Top = 145
    Width = 989
    Height = 387
    Align = alClient
    FindPanel.DisplayMode = fpdmManual
    FindPanel.Layout = fplCompact
    FindPanel.UseExtendedSyntax = True
    LayoutStyle = lsMultiRecordView
    LookAndFeel.ScrollbarMode = sbmClassic
    OptionsView.GridLineColor = clBtnFace
    OptionsView.ValueWidth = 170
    Navigator.Buttons.CustomButtons = <>
    ScrollbarAnnotations.CustomAnnotations = <>
    TabOrder = 3
    DataController.DataSource = dsEmployeesGroups
    Version = 1
    object VerticalGridRecId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'RecId'
      Visible = False
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object VerticalGridId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Id'
      Visible = False
      ID = 1
      ParentID = -1
      Index = 1
      Version = 1
    end
    object VerticalGridParentId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'ParentId'
      Visible = False
      ID = 2
      ParentID = -1
      Index = 2
      Version = 1
    end
    object VerticalGridJobTitle: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'JobTitle'
      ID = 3
      ParentID = -1
      Index = 3
      Version = 1
    end
    object VerticalGridFirstName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'FirstName'
      ID = 4
      ParentID = -1
      Index = 4
      Version = 1
    end
    object VerticalGridLastName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'LastName'
      ID = 5
      ParentID = -1
      Index = 5
      Version = 1
    end
    object VerticalGridCity: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'City'
      ID = 6
      ParentID = -1
      Index = 6
      Version = 1
    end
    object VerticalGridStateProvinceName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'StateProvinceName'
      ID = 7
      ParentID = -1
      Index = 7
      Version = 1
    end
    object VerticalGridPhone: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Phone'
      ID = 8
      ParentID = -1
      Index = 8
      Version = 1
    end
    object VerticalGridEmailAddress: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'EmailAddress'
      ID = 9
      ParentID = -1
      Index = 9
      Version = 1
    end
    object VerticalGridAddressLine1: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'AddressLine1'
      ID = 10
      ParentID = -1
      Index = 10
      Version = 1
    end
    object VerticalGridPostalCode: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'PostalCode'
      ID = 11
      ParentID = -1
      Index = 11
      Version = 1
    end
  end
  inherited mmMain: TMainMenu
    Left = 220
    Top = 192
    object miFindPanelOptions: TMenuItem [1]
      Caption = 'Find Panel Options'
      object ClearFindOnClose1: TMenuItem
        Action = actClearFindOnClose
        AutoCheck = True
      end
      object HighlightFindResult1: TMenuItem
        Action = actHighlightSearchResults
        AutoCheck = True
      end
      object miVisibleButtons: TMenuItem
        Caption = 'Button Visibility'
        object ShowClearButton2: TMenuItem
          Action = actShowClearButton
          AutoCheck = True
        end
        object ShowCloseButton2: TMenuItem
          Action = actShowCloseButton
          AutoCheck = True
        end
        object ShowFindButton2: TMenuItem
          Action = actShowFindButton
          AutoCheck = True
        end
      end
      object UseDelayedFind1: TMenuItem
        Action = actUseDelayedSearch
        AutoCheck = True
      end
      object UseExtendedSyntax1: TMenuItem
        Action = actUseExtendedSyntax
        AutoCheck = True
      end
    end
  end
  object erMain: TcxEditRepository
    Left = 380
    Top = 196
    PixelsPerInch = 96
    object erMainFlag: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
  end
  object alAction: TActionList
    Left = 304
    Top = 192
    object actClearFindOnClose: TAction
      AutoCheck = True
      Caption = 'Clear Find Filter Text On Close'
      Checked = True
      OnExecute = actClearFindOnCloseChange
    end
    object actShowClearButton: TAction
      AutoCheck = True
      Caption = 'Show Clear Button'
      Checked = True
      OnExecute = actShowClearButtonChange
    end
    object actShowCloseButton: TAction
      AutoCheck = True
      Caption = 'Show Close Button'
      Checked = True
      OnExecute = actShowCloseButtonChange
    end
    object actShowFindButton: TAction
      AutoCheck = True
      Caption = 'Show Find Button'
      Checked = True
      OnExecute = actShowFindButtonEChange
    end
    object actHighlightSearchResults: TAction
      AutoCheck = True
      Caption = 'Highlight Search Results'
      Checked = True
      OnExecute = actHighlightFindResultChange
    end
    object actUseDelayedSearch: TAction
      AutoCheck = True
      Caption = 'Use Delayed Search'
      Checked = True
      OnExecute = actUseDelayedSearchExecute
    end
    object actUseExtendedSyntax: TAction
      AutoCheck = True
      Caption = 'Use Extended Syntax'
      OnExecute = actUseExtendedSyntaxExecute
    end
    object actShowPrevAndNextButtons: TAction
      AutoCheck = True
      Caption = 'Show Prev/Next Buttons'
      Checked = True
      OnExecute = actShowPrevAndNextButtonsExecute
    end
  end
  object mdEmployeesGroups: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 216
    Top = 272
    object mdEmployeesGroupsId: TStringField
      FieldName = 'Id'
      Visible = False
      Size = 3
    end
    object mdEmployeesGroupsParentId: TStringField
      FieldName = 'ParentId'
      Visible = False
      Size = 3
    end
    object mdEmployeesGroupsJobTitle: TStringField
      DisplayLabel = 'Job Title'
      FieldName = 'JobTitle'
      Size = 40
    end
    object mdEmployeesGroupsFirstName: TStringField
      DisplayLabel = 'First Name'
      FieldName = 'FirstName'
      Size = 10
    end
    object mdEmployeesGroupsLastName: TStringField
      DisplayLabel = 'Last Name'
      FieldName = 'LastName'
      Size = 9
    end
    object mdEmployeesGroupsCity: TStringField
      DisplayLabel = 'Origin City'
      FieldName = 'City'
      Size = 12
    end
    object mdEmployeesGroupsStateProvinceName: TStringField
      DisplayLabel = 'Origin State'
      FieldName = 'StateProvinceName'
      Size = 13
    end
    object mdEmployeesGroupsPhone: TStringField
      FieldName = 'Phone'
      Size = 14
    end
    object mdEmployeesGroupsEmailAddress: TStringField
      DisplayLabel = 'E-mail'
      FieldName = 'EmailAddress'
      Size = 30
    end
    object mdEmployeesGroupsAddressLine1: TStringField
      DisplayLabel = 'Address Line'
      FieldName = 'AddressLine1'
      Size = 30
    end
    object mdEmployeesGroupsPostalCode: TStringField
      DisplayLabel = 'Postal Code'
      FieldName = 'PostalCode'
      Size = 5
    end
  end
  object dsEmployeesGroups: TDataSource
    DataSet = mdEmployeesGroups
    Left = 264
    Top = 272
  end
end
