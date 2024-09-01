inherited GridViewDemoMainForm: TGridViewDemoMainForm
  Left = 112
  Top = 111
  Caption = 'ExpressQuantumGrid EMF Demo'
  ClientHeight = 552
  ClientWidth = 791
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 791
    Transparent = False
  end
  inherited sbMain: TStatusBar
    Top = 533
    Width = 791
    SimplePanel = True
  end
  object cxGrid1: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 791
    Height = 517
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnActiveTabChanged = cxGrid1ActiveTabChanged
    object cxGrid1EMFTableView1: TcxGridEMFTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.Refresh.Visible = False
      Navigator.Buttons.SaveBookmark.Visible = False
      Navigator.Buttons.GotoBookmark.Visible = False
      Navigator.InfoPanel.Visible = True
      Navigator.InfoPanel.Width = 100
      Navigator.Visible = True
      ScrollbarAnnotations.CustomAnnotations = <>
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          FieldName = 'OID'
          Column = cxGrid1EMFTableView1OID
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Format = 'Row count: #,###'
          Kind = skCount
          FieldName = 'From'
          Column = cxGrid1EMFTableView1From
        end
        item
          Kind = skMin
          FieldName = 'Sent'
          Column = cxGrid1EMFTableView1Sent
        end
        item
          Kind = skMax
          FieldName = 'Sent'
          Column = cxGrid1EMFTableView1Sent
        end>
      DataController.Summary.SummaryGroups = <>
      Images = ilImages
      OptionsBehavior.IncSearch = True
      OptionsCustomize.ColumnsQuickCustomization = True
      OptionsView.FocusRect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.FooterAutoHeight = True
      OptionsView.FooterMultiSummaries = True
      OptionsView.GroupFooterMultiSummaries = True
      OptionsView.Indicator = True
      object cxGrid1EMFTableView1OID: TcxGridEMFColumn
        DataBinding.FieldName = 'OID'
        Visible = False
        Options.FilteringPopup = False
        Options.Grouping = False
      end
      object cxGrid1EMFTableView1Subject: TcxGridEMFColumn
        DataBinding.FieldName = 'Subject'
        Visible = False
        GroupIndex = 0
        Width = 161
      end
      object cxGrid1EMFTableView1From: TcxGridEMFColumn
        DataBinding.FieldName = 'From'
        Width = 253
      end
      object cxGrid1EMFTableView1Sent: TcxGridEMFColumn
        DataBinding.FieldName = 'Sent'
        PropertiesClassName = 'TcxDateEditProperties'
        Properties.Kind = ckDateTime
        DateTimeGrouping = dtgByMonth
        Options.FilteringPopup = False
        Width = 170
      end
      object cxGrid1EMFTableView1HasAttachment: TcxGridEMFColumn
        Caption = 'Attachment'
        DataBinding.FieldName = 'HasAttachment'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Horz = taCenter
        Properties.Images = ilImages
        Properties.Items = <
          item
            Value = False
          end
          item
            ImageIndex = 0
            Value = True
          end>
        Properties.ShowDescriptions = False
        Width = 94
      end
      object cxGrid1EMFTableView1Priority: TcxGridEMFColumn
        DataBinding.FieldName = 'Priority'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Horz = taCenter
        Properties.Images = ilImages
        Properties.Items = <
          item
            Value = 0
          end
          item
            ImageIndex = 1
            Value = 1
          end
          item
            ImageIndex = 2
            Value = 2
          end>
        Properties.ShowDescriptions = False
        Width = 94
      end
      object cxGrid1EMFTableView1Size: TcxGridEMFColumn
        DataBinding.FieldName = 'Size'
        Width = 149
      end
    end
    object cxGrid1Level1: TcxGridLevel
      Caption = 'Table View'
      GridView = cxGrid1EMFTableView1
    end
  end
  inherited mmMain: TMainMenu
    Left = 16
    Top = 80
    object mOptions: TMenuItem [1]
      Caption = 'Options'
      object mEditing: TMenuItem
        AutoCheck = True
        Caption = 'Editing Records'
        Checked = True
        OnClick = UpdateOptionsDataView
      end
      object mInserting: TMenuItem
        AutoCheck = True
        Caption = 'Inserting Records'
        Checked = True
        OnClick = UpdateOptionsDataView
      end
      object mDeleting: TMenuItem
        AutoCheck = True
        Caption = 'Deleting Records'
        Checked = True
        OnClick = UpdateOptionsDataView
      end
      object mDeletingConfirmation: TMenuItem
        AutoCheck = True
        Caption = 'Confirming Record Deletion'
        Checked = True
        OnClick = UpdateOptionsDataView
      end
      object mCancelOnExit: TMenuItem
        AutoCheck = True
        Caption = 'Canceling Edit on Exit'
        Checked = True
        OnClick = UpdateOptionsDataView
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 72
    Top = 80
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 136
    Top = 80
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = cxGrid1
    PopupMenus = <>
    Left = 160
    Top = 192
  end
  object ilImages: TcxImageList
    SourceDPI = 96
    Height = 13
    Width = 13
    FormatVersion = 1
    DesignInfo = 12583272
    ImageInfo = <
      item
        Image.Data = {
          DA020000424DDA0200000000000036000000280000000D0000000D0000000100
          200000000000A402000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0060483000604830006050
          4000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0070504000FF00FF00FF00FF00FF00FF0070504000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0070584000FF00FF00FF00
          FF0060504000FF00FF00FF00FF0060483000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0070584000FF00FF0070504000FF00FF0060484000FF00
          FF0060483000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008068
          5000FF00FF0070605000FF00FF0070504000FF00FF0060504000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0090786000FF00FF0080706000FF00
          FF0080605000FF00FF0070584000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00A0807000FF00FF0090787000FF00FF0090706000FF00FF008068
          5000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A0807000FF00
          FF0090807000FF00FF0090706000FF00FF0080706000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A0887000FF00FF00FF00
          FF00FF00FF0090807000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00A0887000A088700090807000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          DA020000424DDA0200000000000036000000280000000D0000000D0000000100
          200000000000A402000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00A0503000A0502000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B0603000E0805000D078
          5000A0502000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00B0583000F0A07000F0A07000E0805000D0704000B0583000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00E9B29C00E5A48A00E090
          7000FFA88000F0885000B0603000A0502000A0502000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00E0906000FFA88000F0906000B058
          3000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00E0907000FFB09000FF906000A0502000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00E0907000FFB8
          9000FF986000A0502000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00E0987000FFB89000FF987000A0502000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00F0A08000FFC0A000FFB89000A0502000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00F0A57800E1A57800E19E
          7800D2876900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          DA020000424DDA0200000000000036000000280000000D0000000D0000000100
          200000000000A402000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00657BC8002F4FB100FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF006080F0002048
          D000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF009DB2EA008F9FDF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004970E5002F5CD800FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF003060F0000040FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF006383E5004B78
          F0000048FF001F50D500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF005078E0006088FF003060FF000038D000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF007088E00090A8FF006088FF002050D000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00788FE100697F
          E100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end>
  end
end
