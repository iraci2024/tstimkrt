object dxGanttControlSheetColumnsEditor: TdxGanttControlSheetColumnsEditor
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  ClientHeight = 241
  ClientWidth = 153
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbMain: TcxCustomizeListBox
    Left = 0
    Top = 0
    Width = 153
    Height = 241
    Align = alClient
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = pmMain
    TabOrder = 0
    OnClick = lbMainClick
    OnDragDrop = lbMainDragDrop
    OnDragOver = lbMainDragOver
    OnEndDrag = lbMainEndDrag
  end
  object pmMain: TPopupMenu
    Left = 58
    Top = 32
    object miAdd: TMenuItem
      Caption = 'Add Column'
    end
    object miSeparator1: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object miSelectAll: TMenuItem
      Caption = 'Select &All'
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
    object miSeparateor2: TMenuItem
      Caption = '-'
    end
    object miReset: TMenuItem
      Caption = 'Reset'
      OnClick = miResetClick
    end
  end
end
