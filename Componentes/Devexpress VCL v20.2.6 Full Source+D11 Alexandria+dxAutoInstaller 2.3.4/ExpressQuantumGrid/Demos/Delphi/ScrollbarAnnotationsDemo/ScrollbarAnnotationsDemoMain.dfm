inherited ScrollbarAnnotationsDemoMainForm: TScrollbarAnnotationsDemoMainForm
  Caption = 'ExpressQuantumGrid Scrollbar Annotations Demo'
  ClientHeight = 531
  ClientWidth = 890
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 890
    Height = 32
    Caption = 
      'This demo shows the colored scrollbar annotations that denote an' +
      'd bookmark specific data rows, search results, and validation er' +
      'rors in the grid for easier navigation. Hover the mouse over an ' +
      'annotation to read its hint. Click '#8220'About this demo'#8221' for additio' +
      'nal information.'
  end
  inherited sbMain: TStatusBar
    Top = 512
    Width = 890
  end
  object cxGrid1: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 890
    Height = 496
    Align = alClient
    TabOrder = 1
    LookAndFeel.ScrollbarMode = sbmClassic
    object cxGrid1DBTableView1: TcxGridDBTableView
      OnKeyDown = cxGrid1DBTableView1KeyDown
      Navigator.Buttons.CustomButtons = <>
      FindPanel.Behavior = fcbSearch
      FindPanel.DisplayMode = fpdmAlways
      ScrollbarAnnotations.Active = True
      ScrollbarAnnotations.CustomAnnotations = <
        item
          Color = -1610547456
        end
        item
          Alignment = saaFar
          Color = -1610547201
        end
        item
          Alignment = saaCenter
          Color = -1593900801
        end>
      ScrollbarAnnotations.ShowSelectedRows = False
      OnPopulateCustomScrollbarAnnotationRowIndexList = cxGrid1DBTableView1PopulateCustomScrollbarAnnotationRowIndexList
      OnGetScrollbarAnnotationHint = cxGrid1DBTableView1GetScrollbarAnnotationHint
      DataController.DataSource = dmCars.dsModels
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.DataRowFixing = True
      OptionsSelection.MultiSelect = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      Preview.Column = clDescription
      Preview.Visible = True
      object clName: TcxGridDBColumn
        DataBinding.FieldName = 'Name'
        Width = 118
      end
      object clModification: TcxGridDBColumn
        DataBinding.FieldName = 'Modification'
        Width = 186
      end
      object clPrice: TcxGridDBColumn
        DataBinding.FieldName = 'Price'
        PropertiesClassName = 'TcxCurrencyEditProperties'
        Properties.DisplayFormat = '$,0;($,0)'
      end
      object cxGrid1DBTableView1MPGCity: TcxGridDBColumn
        DataBinding.FieldName = 'MPG City'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ValidationOptions = [evoRaiseException, evoShowErrorIcon]
        OnValidateDrawValue = cxGrid1DBTableView1MPGCityValidateDrawValue
      end
      object cxGrid1DBTableView1MPGHighway: TcxGridDBColumn
        DataBinding.FieldName = 'MPG Highway'
      end
      object clDoorCount: TcxGridDBColumn
        DataBinding.FieldName = 'Doors'
      end
      object clCylinderCount: TcxGridDBColumn
        DataBinding.FieldName = 'Cilinders'
      end
      object cxGrid1DBTableView1Horsepower: TcxGridDBColumn
        DataBinding.FieldName = 'Horsepower'
        Width = 73
      end
      object cxGrid1DBTableView1TransmissionSpeeds: TcxGridDBColumn
        DataBinding.FieldName = 'Transmission Speeds'
        Width = 41
      end
      object clDescription: TcxGridDBColumn
        DataBinding.FieldName = 'Description'
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1DBTableView1
    end
  end
  inherited mmMain: TMainMenu
    object Scrollannotations1: TMenuItem [1]
      Caption = '&Scrollbar Annotation Options'
      object Active1: TMenuItem
        Action = actScrollAnnotationsActive
        AutoCheck = True
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ShowErrors1: TMenuItem
        Action = actShowErrors
        AutoCheck = True
      end
      object ShowSearchResults1: TMenuItem
        Action = actShowSearchResults
        AutoCheck = True
      end
      object ShowFocusedRow1: TMenuItem
        Action = actShowFocusedRow
        AutoCheck = True
      end
      object ShowSelectedRows1: TMenuItem
        Action = actShowSelectedRows
        AutoCheck = True
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Doorcountislessthan41: TMenuItem
        Action = actDoorCount
        AutoCheck = True
      end
      object Pricemorethan1000001: TMenuItem
        Action = actPrice
        AutoCheck = True
      end
      object Active2: TMenuItem
        Action = actCylinderCount
        AutoCheck = True
      end
      object Customannotationssettings1: TMenuItem
        Action = actCustomAnnotationSettings
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object ActionList1: TActionList
    Left = 336
    Top = 120
    object actScrollAnnotationsActive: TAction
      AutoCheck = True
      Caption = 'Active'
      Checked = True
      OnExecute = actScrollAnnotationsActiveExecute
    end
    object actDoorCount: TAction
      AutoCheck = True
      Caption = '2-Door Cars'
      Checked = True
      OnExecute = actCustomScrollAnnotationExecute
    end
    object actPrice: TAction
      Tag = 1
      AutoCheck = True
      Caption = 'Price is Greater Than 100000'
      Checked = True
      OnExecute = actCustomScrollAnnotationExecute
    end
    object actCylinderCount: TAction
      Tag = 2
      AutoCheck = True
      Caption = '6-Cylinder Cars'
      Checked = True
      OnExecute = actCustomScrollAnnotationExecute
    end
    object actShowErrors: TAction
      AutoCheck = True
      Caption = 'ShowErrors'
      Checked = True
      OnExecute = actShowErrorsExecute
    end
    object actShowSearchResults: TAction
      AutoCheck = True
      Caption = 'ShowSearchResults'
      Checked = True
      OnExecute = actShowSearchResultsExecute
    end
    object actShowFocusedRow: TAction
      AutoCheck = True
      Caption = 'ShowFocusedRow'
      Checked = True
      OnExecute = actShowFocusedRowExecute
    end
    object actShowSelectedRows: TAction
      AutoCheck = True
      Caption = 'ShowSelectedRows'
      OnExecute = actShowSelectedRowsExecute
    end
    object actCustomAnnotationSettings: TAction
      Caption = 'Custom Annotation Settings...'
      OnExecute = actCustomAnnotationSettingsExecute
    end
  end
end
