object Form1: TForm1
  Left = 560
  Top = 290
  Width = 605
  Height = 374
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Example of using Express DBTreeView'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010002002020100000000000E80200002600000010101000000000002801
    00000E0300002800000020000000400000000100040000000000800200000000
    0000000000000000000000000000000000000000800000800000008080008000
    0000800080008080000080808000C0C0C0000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00003333330000000000000000000000333BBBBBBBB333000000000000000003
    3BBBBBBBBBBB33300000000000000003BBBBBBBBBBB3B3300000000000000003
    BBBBFFF33333333000000000000000033333BBBBBBB330300000000000000003
    3BBBBBB7777777777777777777700003BBBBFFFFFFFFFFFFFFFFFFFFFF700003
    3333BBBF7FFFFFDDDFFFFFFFFF7000033BBBBBBF7F7070DDDFFFFFFFFF700003
    BBBBFFFF7F0FFFDDDFFFFFFFFF7000033333BBBF7F7FFFFFFFFFFFFFFF700003
    3BBBBBBF7F0FFFFFFFFFFFFFFF700003BBBBFFFF7F7FFFCCCFFFFFFFFF700003
    3333BFFF7F0707CCCFFFFFFFFF70000333B3BBFF7F7FFFCCCFFFFFFFFF700003
    333B3BBF7F0FFFFFFFFFFFFFFF70000333B3B3BF7F7FFFFFFFF999FFFF700000
    33333B3F7F0FFFF0707999FFFF7000000000333F7F7FFFF7FFF999FFFF700000
    0000000F7F0FFFF0FFFFFFFFFF7000000000000F7F7FFF222FFFFFFFFF700000
    0000000F7F0707222FFFFFFFFF7000000000000F7F7FFF222FFFFFFFFF700000
    0000000F7F0FFFFFFFFFFFFFFF7000000000000F7FFFFFFFFFFFFFFFFF700000
    0000000F7777777777777777777000000000000FFFFFFFFFFFFFFFFFFFF00000
    0000000000000000000000000000FFFFFFFFFFFFFFFFFF03FFFFF0003FFFE000
    1FFFC0000FFFC0000FFFC0000FFFC0000000C0000000C0000000C0000000C000
    0000C0000000C0000000C0000000C0000000C0000000C0000000C0000000E000
    0000F0000000FF000000FFE00000FFE00000FFE00000FFE00000FFE00000FFE0
    0000FFE00001FFE00001FFFFFFFF280000001000000020000000010004000000
    0000C00000000000000000000000000000000000000000000000000080000080
    000000808000800000008000800080800000C0C0C000808080000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000003333300000000033B
    3BBB3300000003B3BBB88888888803333338FFFFFFF8033B3BB8F779FFF803B3
    BBB8F7FFFFF803333338F77CFFF8033B3BB8F7FFFFF803B3BBB8F7F779F80333
    3338F7F7FFF8033BBBB8F772FFF800033338F7FFFFF800000008F7FFFFF80000
    0008FFFFFFF800000008888888F80000000000000000E0FF0000803F00008000
    0000800000008000000080000000800000008000000080000000800000008000
    0000E0000000FE000000FE000000FE000000FFFF0000}
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 285
    Top = 0
    Width = 304
    Height = 265
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 11
      Top = 60
      Width = 18
      Height = 13
      Caption = 'Info'
    end
    object Label2: TLabel
      Left = 8
      Top = 38
      Width = 31
      Height = 13
      Caption = 'Parent'
    end
    object Label3: TLabel
      Left = 9
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Name'
    end
    object DBMemo1: TDBMemo
      Left = 48
      Top = 58
      Width = 254
      Height = 207
      DataField = 'Pr_info'
      DataSource = DS1
      TabOrder = 2
    end
    object DBLookUpTreeView1: TdxDBLookupTreeView
      Left = 48
      Top = 33
      Width = 256
      Height = 21
      CanSelectParents = True
      DropDownRows = 15
      ParentColor = False
      TabOrder = 1
      TabStop = True
      Text = 'DBLookUpTreeView1'
      TreeViewColor = clWindow
      TreeViewCursor = crDefault
      TreeViewFont.Charset = DEFAULT_CHARSET
      TreeViewFont.Color = clWindowText
      TreeViewFont.Height = -11
      TreeViewFont.Name = 'MS Sans Serif'
      TreeViewFont.Style = []
      TreeViewImages = ImageList1
      TreeViewIndent = 19
      TreeViewReadOnly = False
      TreeViewShowButtons = True
      TreeViewShowHint = False
      TreeViewShowLines = True
      TreeViewShowRoot = True
      TreeViewSortType = stNone
      DividedChar = '.'
      ListSource = DS2
      KeyField = 'Pr_id'
      ListField = 'Pr_name'
      Options = [trDBConfirmDelete, trCanDBNavigate, trSmartRecordCopy, trCheckHasChildren]
      ParentField = 'Pr_parent'
      RootValue = Null
      TextStyle = tvtsFull
      OnAddNewItem = DBLookUpTreeView1AddNewItem
      DataField = 'Pr_parent'
      DataSource = DS1
    end
    object DBEdit1: TDBEdit
      Left = 48
      Top = 6
      Width = 254
      Height = 21
      DataField = 'Pr_name'
      DataSource = DS1
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 285
    Height = 265
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 1
    object DBTreeView1: TdxDBTreeView
      Left = 0
      Top = 0
      Width = 281
      Height = 261
      ShowNodeHint = False
      OnCustomDraw = DBTreeView1CustomDraw
      OnDragDropTreeNode = DBTreeView1DragDropTreeNode
      DataSource = DS1
      KeyField = 'Pr_id'
      ListField = 'Pr_name'
      ParentField = 'Pr_parent'
      RootValue = Null
      SeparatedSt = ', id =  '
      RaiseOnError = True
      DragMode = dmAutomatic
      Indent = 19
      Align = alClient
      ParentColor = False
      Options = [trDBCanDelete, trDBConfirmDelete, trCanDBNavigate, trSmartRecordCopy, trCheckHasChildren]
      SelectedIndex = -1
      TabOrder = 0
      OnAddNewItem = DBTreeView1AddNewItem
      Images = ImageList1
      StateImages = ImageList1
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 265
    Width = 589
    Height = 71
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 514
      Top = 5
      Width = 70
      Height = 25
      TabOrder = 2
      Kind = bkClose
    end
    object CheckBox1: TCheckBox
      Left = 252
      Top = 52
      Width = 214
      Height = 18
      Caption = 'Automatically drag mode for dxDBTreeView'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 252
      Top = 35
      Width = 147
      Height = 15
      Caption = 'Use DisplayField property'
      TabOrder = 0
      OnClick = CheckBox2Click
    end
    object BitBtnAdd: TBitBtn
      Left = 2
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Add Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
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
      Left = 32
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Add Child Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
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
      Left = 62
      Top = 5
      Width = 25
      Height = 25
      Hint = 'Edit Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
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
    object BitBtnEdit1: TBitBtn
      Left = 92
      Top = 5
      Width = 28
      Height = 25
      Hint = 'Duplicate Name'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = BitBtnEdit1Click
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
      Left = 124
      Top = 5
      Width = 25
      Height = 25
      Hint = 'Delete Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
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
      Left = 155
      Top = 5
      Width = 26
      Height = 25
      Hint = 'Make on one level higher'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
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
    object BitBtn3: TBitBtn
      Left = 270
      Top = 5
      Width = 103
      Height = 24
      Caption = 'dxTreeView'
      TabOrder = 9
      OnClick = BitBtn3Click
    end
    object CheckBox3: TCheckBox
      Left = 2
      Top = 35
      Width = 250
      Height = 17
      Caption = 'Do not allow to drag treeNodes on the next level'
      TabOrder = 10
    end
    object CheckBox4: TCheckBox
      Left = 2
      Top = 52
      Width = 215
      Height = 17
      Caption = 'Do not allow to drag the root trenodes'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object Button2: TButton
      Left = 195
      Top = 5
      Width = 68
      Height = 25
      Caption = 'DBGrid'
      TabOrder = 12
      OnClick = Button2Click
    end
    object CheckBox5: TCheckBox
      Left = 479
      Top = 35
      Width = 87
      Height = 17
      Caption = 'Custom draw'
      Checked = True
      State = cbChecked
      TabOrder = 13
      OnClick = CheckBox5Click
    end
  end
  object DS1: TDataSource
    DataSet = mdProject1
    Left = 160
    Top = 40
  end
  object DS2: TDataSource
    DataSet = mdProject2
    Left = 504
    Top = 152
  end
  object ImageList1: TImageList
    DrawingStyle = dsTransparent
    ShareImages = True
    Left = 288
    Top = 216
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000007F7F7F007F7F7F000000
      0000FFFFFF00FFFFFF007F0000007F0000007F0000007F000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000007F7F7F007F007F0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00007F007F007F007F007F007F00000000000000000000000000000000000000
      0000000000000000000000000000000000007F7F7F00BFBFBF00BFBFBF007F7F
      7F0000000000FF000000FF000000FF000000FF000000FF000000FF0000007F00
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000007F7F7F00BFBFBF00FFFFFF00BFBFBF007F007F0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00007F007F0000000000000000007F007F007F007F007F007F007F007F007F00
      7F007F007F007F007F007F007F000000000000000000BFBFBF00BFBFBF00BFBF
      BF007F7F7F007F000000FF000000FF000000FF000000FF000000FF000000FF00
      00007F000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000007F7F
      7F00BFBFBF00FFFFFF00FFFFFF007F7F7F00BFBFBF00BFBFBF007F007F000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00007F007F00FFFFFF000000000000000000000000007F7F7F00BFBFBF00BFBF
      BF00FFFFFF00FFFFFF007F007F0000000000FFFFFF0000000000BFBFBF00BFBF
      BF00BFBFBF007F7F7F007F000000FF000000FF000000FF000000FF000000FF00
      0000FF0000007F000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007F7F7F00BFBFBF00FFFF
      FF00FFFFFF007F7F7F007F7F7F00000000007F7F7F00BFBFBF00BFBFBF007F00
      7F0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000007F00
      7F007F007F00FFFFFF0000000000BFBFBF00FFFFFF007F7F7F00000000000000
      000000000000FFFFFF007F007F0000000000FFFFFF00FFFFFF007F000000BFBF
      BF00BFBFBF00BFBFBF007F7F7F007F000000FF000000FF000000FF000000FF00
      0000FF000000FF00000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F007F007F7F7F00FFFFFF007F7F
      7F007F7F7F0000000000000000007F007F00000000007F7F7F00BFBFBF00BFBF
      BF007F007F0000000000FFFFFF00FFFFFF00FFFFFF00000000007F007F007F00
      7F00FFFFFF00FFFFFF0000000000BFBFBF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF007F7F7F00FFFFFF007F007F0000000000FFFFFF007F0000007F0000007F00
      0000BFBFBF00BFBFBF00BFBFBF007F7F7F007F0000007F0000007F0000007F00
      00007F0000007F0000007F000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F007F007F7F7F007F7F7F000000
      0000000000007F007F007F007F007F007F007F007F00000000007F7F7F00BFBF
      BF00BFBFBF007F007F0000000000FFFFFF00FFFFFF00000000007F007F00FFFF
      FF00FFFFFF00FFFFFF0000000000BFBFBF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF007F7F7F00FFFFFF007F007F0000000000FFFFFF007F0000007F0000007F00
      00007F000000BFBFBF00BFBFBF007F7F7F007F7F7F00000000007F0000007F00
      00007F0000007F0000007F000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F007F0000000000000000007F00
      7F007F007F007F007F007F007F007F007F007F007F007F007F00000000007F7F
      7F00BFBFBF00BFBFBF007F007F0000000000FFFFFF00000000007F007F00FFFF
      FF00FFFFFF00FFFFFF0000000000BFBFBF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF007F7F7F00FFFFFF007F007F0000000000FFFFFF007F0000007F0000007F00
      00007F0000007F000000BFBFBF007F7F7F007F7F7F00BFBFBF00000000007F7F
      7F007F7F7F007F7F7F007F000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F007F007F7F7F007F007F007F00
      7F007F007F007F007F0000FFFF00007F7F007F007F007F007F007F007F000000
      00007F7F7F00BFBFBF000000000000000000FFFFFF00000000007F007F00FFFF
      FF00FFFFFF00FFFFFF0000000000BFBFBF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF007F7F7F00FFFFFF007F007F0000000000FFFFFF007F0000007F0000007F00
      00007F0000007F0000007F7F7F007F7F7F00BFBFBF007F7F7F007F7F7F0000FF
      FF000000FF007F7F7F007F000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF007F007F007F7F7F007F00
      7F007F007F007F007F007F007F00BFBFBF0000FFFF0000FFFF007F007F007F00
      7F00000000007F7F7F0000000000FFFFFF00FFFFFF00000000007F007F00FFFF
      FF00FFFFFF00FFFFFF0000000000BFBFBF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF007F7F7F00FFFFFF007F007F0000000000FFFFFF00000000007F0000007F00
      00007F0000007F0000007F7F7F007F7F7F00BFBFBF00BFBFBF00BFBFBF0000FF
      FF000000FF000000FF007F000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF007F007F007F7F
      7F007F007F007F007F00007F7F00007F7F007F007F0000FFFF0000FFFF007F00
      7F007F007F000000000000000000FFFFFF00FFFFFF00000000007F007F00FFFF
      FF00FFFFFF00FFFFFF0000000000BFBFBF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF007F7F7F00FFFFFF007F007F0000000000FFFFFF00FFFFFF007F0000007F00
      00007F0000007F0000007F7F7F00BFBFBF007F7F7F0000FFFF0000FFFF0000FF
      FF000000FF000000FF007F7F7F00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF007F00
      7F007F7F7F007F007F007F007F0000FFFF0000FFFF0000FFFF00007F7F007F00
      7F007F007F007F007F0000000000FFFFFF00FFFFFF00000000007F007F00FFFF
      FF00FFFFFF00BFBFBF007F7F7F007F7F7F00FFFFFF00FFFFFF00FFFFFF0000FF
      FF007F7F7F000000000000000000FFFFFF00FFFFFF00FFFFFF00000000007F00
      00007F0000007F0000007F7F7F007F7F7F000000FF0000FFFF000000FF000000
      FF000000FF000000FF007F7F7F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007F007F007F7F7F007F007F007F007F007F007F007F007F007F007F007F00
      7F007F007F000000000000000000FFFFFF00FFFFFF00000000007F007F00FFFF
      FF00FFFFFF007F7F7F00FFFFFF00FFFFFF007F7F7F007F7F7F00FFFFFF00FFFF
      FF007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00007F0000007F0000007F0000007F7F7F007F7F7F000000FF000000FF000000
      FF000000FF000000FF007F7F7F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007F007F007F7F7F007F007F007F007F007F007F007F007F000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000007F007F00FFFF
      FF00BFBFBF007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F7F007F7F
      7F007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007F0000007F0000007F0000007F000000BFBFBF007F7F7F007F7F
      7F000000FF000000FF00BFBFBF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007F007F007F7F7F007F007F000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F7F00BFBF
      BF007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBF
      BF000000FF000000FF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007F007F0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F
      7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F
      7F007F7F7F0000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object mdProject1: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F06000000040000000300060050725F696400040000
      0003000A0050725F706172656E7400320000000100080050725F6E616D650004
      0000000900090050725F626461746500040000000900090050725F6564617465
      00000000000E00080050725F696E666F00}
    SortOptions = []
    AfterInsert = mdProject1AfterInsert
    AfterPost = mdProject1AfterPost
    Left = 124
    Top = 40
    object mdProject1Pr_id: TIntegerField
      FieldName = 'Pr_id'
    end
    object mdProject1Pr_parent: TIntegerField
      FieldName = 'Pr_parent'
      OnChange = mdProject1Pr_parentChange
    end
    object mdProject1Pr_name: TStringField
      FieldName = 'Pr_name'
      Size = 50
    end
    object mdProject1Pr_bdate: TDateField
      FieldName = 'Pr_bdate'
    end
    object mdProject1Pr_edate: TDateField
      FieldName = 'Pr_edate'
    end
    object mdProject1Pr_info: TMemoField
      FieldName = 'Pr_info'
      BlobType = ftMemo
    end
  end
  object mdProject2: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F06000000040000000300060050725F696400040000
      0003000A0050725F706172656E7400320000000100080050725F6E616D650004
      0000000900090050725F626461746500040000000900090050725F6564617465
      00000000000E00080050725F696E666F00}
    SortOptions = []
    Left = 469
    Top = 152
    object mdProject2Pr_id: TIntegerField
      FieldName = 'Pr_id'
    end
    object mdProject2Pr_parent: TIntegerField
      FieldName = 'Pr_parent'
    end
    object mdProject2Pr_name: TStringField
      FieldName = 'Pr_name'
      Size = 50
    end
    object mdProject2Pr_bdate: TDateField
      FieldName = 'Pr_bdate'
    end
    object mdProject2Pr_edate: TDateField
      FieldName = 'Pr_edate'
    end
    object mdProject2Pr_info: TMemoField
      FieldName = 'Pr_info'
      BlobType = ftMemo
    end
  end
end
