inherited FeaturesDemoMainForm: TFeaturesDemoMainForm
  Left = 199
  Top = 124
  Caption = 'ExpressGanttControl Features Demo'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Height = 32
    Caption = 
      'This demo illustrates the VCL Gantt Control with the base Gantt ' +
      'Chart functionality: tasks, summaries, milestones, and dependenc' +
      'ies. The control displays project tasks as horizontal bars organ' +
      'ized in a tree list. You can drag and resize tasks to change the' +
      'ir start/end date and duration and link them with dependencies.'
  end
  inherited lcMain: TdxLayoutControl
    Top = 32
    Height = 648
    inherited GanttControl: TdxGanttControl
      Height = 628
      TabOrder = 0
      ViewChart.OptionsSheet.Columns.Items = <
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnIndicator'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnTaskMode'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnTaskName'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnTaskDuration'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnTaskStart'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnTaskFinish'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnPredecessors'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnResourceName'
        end
        item
          ItemClass = 'TdxGanttControlViewChartSheetColumnPercentComplete'
        end>
      ViewResourceSheet.OptionsSheet.Columns.Items = <
        item
          ItemClass = 'TdxGanttControlResourceSheetColumnName'
        end
        item
          ItemClass = 'TdxGanttControlResourceSheetColumnType'
        end
        item
          ItemClass = 'TdxGanttControlResourceSheetColumnGroup'
        end
        item
          ItemClass = 'TdxGanttControlResourceSheetColumnBaseCalendar'
        end>
    end
    inherited seTimelineUnitMinWidth: TcxSpinEdit
      TabOrder = 3
    end
    inherited cmbTimelineScale: TcxComboBox
      TabOrder = 2
    end
    inherited cbSrcrollBars: TcxComboBox
      TabOrder = 4
    end
    inherited cmbChartTimescale: TcxComboBox
      TabOrder = 1
    end
    inherited dxLayoutItem1: TdxLayoutItem
      Index = 0
    end
    inherited dxLayoutGroup1: TdxLayoutGroup
      ItemIndex = 1
    end
    inherited lgOptions: TdxLayoutGroup
      Index = 1
    end
  end
  inherited mmMain: TMainMenu
    inherited miAbout: TMenuItem
      GroupIndex = 1
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
