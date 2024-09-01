object Form1: TForm1
  Left = 504
  Top = 225
  Width = 620
  Height = 366
  Caption = 'Smart Records Load. There are  10 000 records in this example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000003
    333300000000033B3BBB3300000003B3BBB77777777703333337FFFFFFF7033B
    3BB7F889FFF703B3BBB7F8FFFFF703333337F88CFFF7033B3BB7F8FFFFF703B3
    BBB7F8F889F703333337F8F8FFF7033BBBB7F882FFF700033337F8FFFFF70000
    0007F8FFFFF700000007FFFFFFF700000007777777F70000000000000000E0FF
    0000803F00008000000080000000800000008000000080000000800000008000
    00008000000080000000E0000000FE000000FE000000FE000000FFFF0000}
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DBTreeView1: TdxDBTreeView
    Left = 0
    Top = 0
    Width = 604
    Height = 243
    ShowNodeHint = False
    KeyField = 'id'
    ListField = 'name'
    ParentField = 'parent'
    RootValue = '-1'
    SeparatedSt = ' - '
    RaiseOnError = True
    DragMode = dmAutomatic
    Indent = 19
    Align = alClient
    ParentColor = False
    Options = [trDBCanDelete, trDBConfirmDelete, trCanDBNavigate, trSmartRecordLoad, trSmartRecordCopy, trCheckHasChildren]
    SortType = stBoth
    SelectedIndex = 0
    TabOrder = 0
    OnCreateNewKeyValue = DBTreeView1CreateNewKeyValue
  end
  object Panel1: TPanel
    Left = 0
    Top = 243
    Width = 604
    Height = 85
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 2
      Top = 5
      Width = 75
      Height = 25
      Caption = 'View DBGrid'
      TabOrder = 0
      OnClick = Button1Click
    end
    object BitBtnAdd: TBitBtn
      Left = 84
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Add Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = BitBtnAddClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333FF33333333FF333993333333300033377F3333333777333993333333
        300033F77FFF3333377739999993333333333777777F3333333F399999933333
        33003777777333333377333993333333330033377F3333333377333993333333
        3333333773333333333F333333333333330033333333F33333773333333C3333
        330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
        993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
        333333333337733333FF3333333C333330003333333733333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
    end
    object BitBtnAddChild: TBitBtn
      Left = 114
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Add Child Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = BitBtnAddChildClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333FF33333333FF333993333333300033377F3333333777333993333333
        300033F77FFF3333377739999993333333333777777F3333333F399999933333
        33003777777333333377333993333333330033377F3333333377333993333333
        3333333773333333333F333333333333330033333333F33333773333333C3333
        330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
        993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
        333333333337733333FF3333333C333330003333333733333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
    end
    object BitBtnEdit: TBitBtn
      Left = 144
      Top = 5
      Width = 25
      Height = 25
      Hint = 'Edit Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = BitBtnEditClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
        000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
        00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
        F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
        0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
        FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
        FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
        0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
        00333377737FFFFF773333303300000003333337337777777333}
      NumGlyphs = 2
    end
    object BitBtnDel: TBitBtn
      Left = 172
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Delete Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = BitBtnDelClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333FF33333333333330003333333333333777333333333333
        300033FFFFFF3333377739999993333333333777777F3333333F399999933333
        3300377777733333337733333333333333003333333333333377333333333333
        3333333333333333333F333333333333330033333F33333333773333C3333333
        330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
        993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
        333333377F33333333FF3333C333333330003333733333333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
    end
    object BitBtn2: TBitBtn
      Left = 202
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Make level higher'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = BitBtn2Click
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
        3333333333777F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
        3333333777737777F333333099999990333333373F3333373333333309999903
        333333337F33337F33333333099999033333333373F333733333333330999033
        3333333337F337F3333333333099903333333333373F37333333333333090333
        33333333337F7F33333333333309033333333333337373333333333333303333
        333333333337F333333333333330333333333333333733333333}
      NumGlyphs = 2
    end
    object Button2: TButton
      Left = 8
      Top = 55
      Width = 225
      Height = 25
      Caption = 'DBTreeView1.DataSource = DataSource1'
      TabOrder = 6
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 248
      Top = 55
      Width = 193
      Height = 25
      Caption = 'DBTreeView1.DataSource = Nil'
      TabOrder = 7
      OnClick = Button3Click
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 35
      Width = 121
      Height = 17
      Caption = 'trCanDBNavigate'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 128
      Top = 35
      Width = 121
      Height = 17
      Caption = 'trCheckHasChildren'
      TabOrder = 9
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 248
      Top = 35
      Width = 161
      Height = 17
      Caption = 'trSmartRecordCopy'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CheckBox3Click
    end
    object DBNavigator1: TDBNavigator
      Left = 242
      Top = 5
      Width = 240
      Height = 25
      DataSource = DataSource1
      TabOrder = 11
    end
    object DBEdit1: TDBEdit
      Left = 488
      Top = 5
      Width = 111
      Height = 21
      DataField = 'id'
      DataSource = DataSource1
      TabOrder = 12
    end
    object DBEdit2: TDBEdit
      Left = 488
      Top = 29
      Width = 111
      Height = 21
      DataField = 'parent'
      DataSource = DataSource1
      TabOrder = 13
    end
  end
  object DataSource1: TDataSource
    DataSet = dxMemData1
    Left = 88
    Top = 80
  end
  object dxMemData1: TdxMemData
    Indexes = <>
    SortOptions = []
    SortedField = 'parent'
    Left = 148
    Top = 128
    object dxMemData1id: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object dxMemData1parent: TIntegerField
      FieldName = 'parent'
      Required = True
    end
    object dxMemData1name: TStringField
      FieldName = 'name'
      Size = 100
    end
    object dxMemData1buffer: TStringField
      FieldName = 'buffer'
    end
  end
end
