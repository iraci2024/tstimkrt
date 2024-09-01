inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Find Panel Demo'
  ClientHeight = 636
  ClientWidth = 1076
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 1076
    Height = 32
    Caption = 
      'This demo illustrates the data filter and search functionality o' +
      'f the built-in Find Panel. Enter a search string into the Find P' +
      'anel'#8217's box and use the Behavior combo box to highlight search re' +
      'sults or filter out all data rows that do not meet the search cr' +
      'iteria. Click '#8220'About this demo'#8221' for details.'
  end
  inherited sbMain: TStatusBar
    Top = 617
    Width = 1076
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 32
    Align = alTop
    TabOrder = 1
    Height = 113
    Width = 1076
    object cbClearFindOnClose: TcxCheckBox
      Left = 513
      Top = 10
      Action = actClearFindOnClose
      TabOrder = 0
      Transparent = True
    end
    object cbShowClearButton: TcxCheckBox
      Left = 687
      Top = 58
      Action = actShowClearButton
      TabOrder = 1
      Transparent = True
    end
    object cbShowCloseButton: TcxCheckBox
      Left = 687
      Top = 10
      Action = actShowCloseButton
      TabOrder = 2
      Transparent = True
    end
    object cbShowFindButton: TcxCheckBox
      Left = 687
      Top = 34
      Action = actShowFindButton
      TabOrder = 3
      Transparent = True
    end
    object cbHighlightSearchResults: TcxCheckBox
      Left = 513
      Top = 34
      Action = actHighlightSearchResults
      TabOrder = 4
      Transparent = True
    end
    object seFindDelay: TcxSpinEdit
      Left = 280
      Top = 58
      Properties.MaxValue = 5000.000000000000000000
      Properties.MinValue = 100.000000000000000000
      Properties.OnChange = seFindDelayPropertiesChange
      TabOrder = 5
      Value = 1000
      Width = 227
    end
    object lbSearchDelay: TcxLabel
      Left = 172
      Top = 59
      Caption = 'Search Delay, ms:'
      Transparent = True
    end
    object icbFindFilterColumns: TcxImageComboBox
      Left = 278
      Top = 82
      Properties.Items = <>
      Properties.OnChange = icbFindFilterColumnsPropertiesChange
      TabOrder = 7
      Width = 229
    end
    object lbSearchableColumns: TcxLabel
      Left = 172
      Top = 83
      Caption = 'Searchable Columns:'
      Transparent = True
    end
    object cbeFindPanelPosition: TcxComboBox
      Left = 280
      Top = 34
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Top'
        'Bottom')
      Properties.OnChange = cbFindPanelPositionPropertiesChange
      TabOrder = 9
      Text = 'Top'
      Width = 227
    end
    object lbFindPanelPosition: TcxLabel
      Left = 172
      Top = 36
      Caption = 'Find Panel Position:'
      Transparent = True
    end
    object cbeDisplayMode: TcxComboBox
      Left = 280
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Never'
        'Manual (Focus the grid and press Ctrl+F)'
        'Always')
      Properties.OnChange = cbDisplayModePropertiesChange
      TabOrder = 11
      Text = 'Manual (Focus the grid and press Ctrl+F)'
      Width = 227
    end
    object lbDisplayMode: TcxLabel
      Left = 172
      Top = 12
      Caption = 'Display Mode:'
      Transparent = True
    end
    object cbUseDelayedSearch: TcxCheckBox
      Left = 513
      Top = 58
      Action = actUseDelayedSearch
      TabOrder = 13
      Transparent = True
    end
    object cbUseExtendedSyntax: TcxCheckBox
      Left = 513
      Top = 82
      Action = actUseExtendedSyntax
      State = cbsChecked
      TabOrder = 14
      Transparent = True
    end
    object cbShowPrevAnNextButtons: TcxCheckBox
      Left = 687
      Top = 83
      Action = actShowPrevAndNextButtons
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
    end
    object cxLabel1: TcxLabel
      Left = 7
      Top = 12
      Caption = 'Behavior:'
      Transparent = True
    end
    object cbeBehavior: TcxComboBox
      Left = 60
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Filter'
        'Search')
      Properties.OnChange = cbeBehaviorPropertiesChange
      Style.TextStyle = []
      TabOrder = 17
      Text = 'Search'
      Width = 103
    end
    object cbSearchInGroupRows: TcxCheckBox
      Left = 835
      Top = 12
      Action = actSearchInGroupRows
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
    end
    object cbSearchPreview: TcxCheckBox
      Left = 835
      Top = 36
      Action = actSearchInPreview
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
    end
    object cbLocation: TcxComboBox
      Left = 60
      Top = 34
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Separate Panel'
        'Group By Box')
      Properties.OnEditValueChanged = cbLocationPropertiesEditValueChanged
      TabOrder = 20
      Text = 'Group By Box'
      Width = 103
    end
    object cxLabel2: TcxLabel
      Left = 7
      Top = 36
      Caption = 'Location:'
    end
    object cbLayout: TcxComboBox
      Left = 60
      Top = 58
      Enabled = False
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Default'
        'Compact')
      Properties.OnEditValueChanged = cbLayoutPropertiesEditValueChanged
      TabOrder = 22
      Text = 'Compact'
      Width = 103
    end
    object cxLabel3: TcxLabel
      Left = 7
      Top = 59
      Caption = 'Layout:'
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 145
    Width = 1076
    Height = 472
    Align = alClient
    TabOrder = 2
    LookAndFeel.ScrollbarMode = sbmClassic
    object TableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.First.Visible = True
      Navigator.Buttons.PriorPage.Visible = True
      Navigator.Buttons.Prior.Visible = True
      Navigator.Buttons.Next.Visible = True
      Navigator.Buttons.NextPage.Visible = True
      Navigator.Buttons.Last.Visible = True
      Navigator.Buttons.Insert.Visible = True
      Navigator.Buttons.Append.Visible = False
      Navigator.Buttons.Delete.Visible = True
      Navigator.Buttons.Edit.Visible = True
      Navigator.Buttons.Post.Visible = True
      Navigator.Buttons.Cancel.Visible = True
      Navigator.Buttons.Refresh.Visible = True
      Navigator.Buttons.SaveBookmark.Visible = True
      Navigator.Buttons.GotoBookmark.Visible = True
      Navigator.Buttons.Filter.Visible = True
      FindPanel.DisplayMode = fpdmManual
      FindPanel.UseExtendedSyntax = True
      FindPanel.Location = fplGroupByBox
      ScrollbarAnnotations.CustomAnnotations = <>
      DataController.DataSource = dsCustomers
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = TableViewContactName
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      Preview.Column = TableViewAddress
      object TableViewCompanyName: TcxGridDBColumn
        Caption = 'Company Name'
        DataBinding.FieldName = 'CompanyName'
      end
      object TableViewContactTitle: TcxGridDBColumn
        Caption = 'Contact Title'
        DataBinding.FieldName = 'ContactTitle'
      end
      object TableViewContactName: TcxGridDBColumn
        Caption = 'Contact Name'
        DataBinding.FieldName = 'ContactName'
      end
      object TableViewCountry: TcxGridDBColumn
        DataBinding.FieldName = 'Country'
      end
      object TableViewCity: TcxGridDBColumn
        DataBinding.FieldName = 'City'
      end
      object TableViewAddress: TcxGridDBColumn
        DataBinding.FieldName = 'Address'
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = TableView
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
  inherited StyleRepository: TcxStyleRepository
    Left = 128
    Top = 192
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 24
    Top = 192
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
    object actSearchInGroupRows: TAction
      AutoCheck = True
      Caption = 'Search In Group Rows'
      OnExecute = actSearchInGroupRowsExecute
    end
    object actSearchInPreview: TAction
      AutoCheck = True
      Caption = 'Search In Preview'
      OnExecute = actSearchInPreviewExecute
    end
  end
  object dsCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 24
    Top = 360
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\Customers2.xml'
    Params = <>
    Left = 176
    Top = 360
    object cdsCustomersCustomerID: TStringField
      FieldName = 'CustomerID'
      Size = 5
    end
    object cdsCustomersCompanyName: TStringField
      FieldName = 'CompanyName'
      Size = 36
    end
    object cdsCustomersContactName: TStringField
      FieldName = 'ContactName'
      Size = 23
    end
    object cdsCustomersContactTitle: TStringField
      FieldName = 'ContactTitle'
      Size = 30
    end
    object cdsCustomersAddress: TStringField
      FieldName = 'Address'
      Size = 46
    end
    object cdsCustomersCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object cdsCustomersPostalCode: TStringField
      FieldName = 'PostalCode'
      Size = 9
    end
    object cdsCustomersCountry: TStringField
      FieldName = 'Country'
      Size = 11
    end
    object cdsCustomersPhone: TStringField
      FieldName = 'Phone'
      Size = 17
    end
    object cdsCustomersFax: TStringField
      FieldName = 'Fax'
      Size = 17
    end
    object cdsCustomersRegion: TStringField
      FieldName = 'Region'
      Size = 13
    end
  end
end
