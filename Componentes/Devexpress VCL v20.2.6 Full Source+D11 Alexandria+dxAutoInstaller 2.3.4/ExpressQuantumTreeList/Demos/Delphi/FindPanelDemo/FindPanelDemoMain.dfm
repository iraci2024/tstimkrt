inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumTreeList Find Panel Demo'
  ClientHeight = 551
  ClientWidth = 992
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 992
    Caption = 
      'This demo illustrates the data filter and search functionality o' +
      'f the built-in Find Panel. Enter a search string into the Find P' +
      'anel'#8217's box and use the Behavior combo box to highlight search re' +
      'sults or filter out all visible nodes that do not meet the searc' +
      'h criteria. Click the Help | About this demo for details.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 992
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 32
    Align = alTop
    TabOrder = 1
    Height = 113
    Width = 992
    object cbClearFindOnClose: TcxCheckBox
      Left = 641
      Top = 10
      Action = actClearFindOnClose
      TabOrder = 0
      Transparent = True
    end
    object cbShowClearButton: TcxCheckBox
      Left = 816
      Top = 58
      Action = actShowClearButton
      TabOrder = 1
      Transparent = True
    end
    object cbShowCloseButton: TcxCheckBox
      Left = 816
      Top = 10
      Action = actShowCloseButton
      TabOrder = 2
      Transparent = True
    end
    object cbShowFindButton: TcxCheckBox
      Left = 816
      Top = 34
      Action = actShowFindButton
      TabOrder = 3
      Transparent = True
    end
    object cbHighlightSearchResults: TcxCheckBox
      Left = 641
      Top = 34
      Action = actHighlightSearchResults
      TabOrder = 4
      Transparent = True
    end
    object seFindDelay: TcxSpinEdit
      Left = 294
      Top = 58
      Properties.MaxValue = 5000.000000000000000000
      Properties.MinValue = 100.000000000000000000
      Properties.OnChange = seFindDelayPropertiesChange
      TabOrder = 5
      Value = 1000
      Width = 334
    end
    object lbSearchDelay: TcxLabel
      Left = 186
      Top = 60
      Caption = 'Search Delay, ms:'
      Transparent = True
    end
    object icbFindFilterColumns: TcxImageComboBox
      Left = 294
      Top = 82
      Properties.Items = <>
      Properties.OnChange = icbFindFilterColumnsPropertiesChange
      TabOrder = 7
      Width = 334
    end
    object lbSearchableColumns: TcxLabel
      Left = 186
      Top = 84
      Caption = 'Searchable Columns:'
      Transparent = True
    end
    object cbeFindPanelPosition: TcxComboBox
      Left = 294
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
      Left = 186
      Top = 36
      Caption = 'Find Panel Position:'
      Transparent = True
    end
    object cbeDisplayMode: TcxComboBox
      Left = 294
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Never'
        'Manual (Focus the tree list and press Ctrl+F)'
        'Always')
      Properties.OnChange = cbDisplayModePropertiesChange
      TabOrder = 11
      Text = 'Manual (Focus the tree list and press Ctrl+F)'
      Width = 334
    end
    object lbDisplayMode: TcxLabel
      Left = 186
      Top = 13
      Caption = 'Display Mode:'
      Transparent = True
    end
    object cbUseDelayedSearch: TcxCheckBox
      Left = 641
      Top = 58
      Action = actUseDelayedSearch
      TabOrder = 13
      Transparent = True
    end
    object cbUseExtendedSyntax: TcxCheckBox
      Left = 641
      Top = 82
      Action = actUseExtendedSyntax
      TabOrder = 14
      Transparent = True
    end
    object cbShowPrevAndNextButtons: TcxCheckBox
      Left = 816
      Top = 82
      Action = actShowPrevAndNextButtons
      Style.TransparentBorder = False
      TabOrder = 15
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
      TabOrder = 17
      Text = 'Filter'
      Width = 97
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
      TabOrder = 18
      Text = 'Compact'
      Width = 97
    end
    object cxLabel1: TcxLabel
      Left = 12
      Top = 13
      Caption = 'Layout:'
      Transparent = True
    end
  end
  object TreeList: TcxDBTreeList [3]
    Left = 0
    Top = 145
    Width = 992
    Height = 387
    Align = alClient
    Bands = <
      item
      end>
    DataController.DataSource = dsEmployeesGroups
    DataController.ParentField = 'ParentId'
    DataController.KeyField = 'Id'
    FindPanel.DisplayMode = fpdmManual
    FindPanel.Layout = fplCompact
    FindPanel.UseExtendedSyntax = True
    LookAndFeel.ScrollbarMode = sbmClassic
    Navigator.Buttons.CustomButtons = <>
    OptionsView.ColumnAutoWidth = True
    OptionsView.Footer = True
    RootValue = -1
    ScrollbarAnnotations.CustomAnnotations = <>
    TabOrder = 2
    object tlDBRecId: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'RecId'
      Width = 100
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBId: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'Id'
      Width = 100
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBParentId: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ParentId'
      Width = 100
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBJobTitle: TcxDBTreeListColumn
      DataBinding.FieldName = 'JobTitle'
      Width = 190
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <
        item
          AlignHorz = taLeftJustify
          Kind = skCount
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBFirstName: TcxDBTreeListColumn
      DataBinding.FieldName = 'FirstName'
      Width = 100
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBLastName: TcxDBTreeListColumn
      DataBinding.FieldName = 'LastName'
      Width = 100
      Position.ColIndex = 5
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBCity: TcxDBTreeListColumn
      DataBinding.FieldName = 'City'
      Width = 100
      Position.ColIndex = 6
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBStateProvinceName: TcxDBTreeListColumn
      DataBinding.FieldName = 'StateProvinceName'
      Width = 100
      Position.ColIndex = 7
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBPhone: TcxDBTreeListColumn
      DataBinding.FieldName = 'Phone'
      Width = 100
      Position.ColIndex = 8
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBEmailAddress: TcxDBTreeListColumn
      DataBinding.FieldName = 'EmailAddress'
      Width = 100
      Position.ColIndex = 9
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBAddressLine1: TcxDBTreeListColumn
      DataBinding.FieldName = 'AddressLine1'
      Width = 100
      Position.ColIndex = 10
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlDBPostalCode: TcxDBTreeListColumn
      DataBinding.FieldName = 'PostalCode'
      Width = 100
      Position.ColIndex = 11
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
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
  object dsEmployeesGroups: TDataSource
    DataSet = mdEmployeesGroups
    Left = 184
    Top = 320
  end
  object mdEmployeesGroups: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 248
    Top = 320
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
end
