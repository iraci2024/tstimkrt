inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressVerticalGrid Calculated Filter Items Demo'
  ClientHeight = 551
  ClientWidth = 989
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 989
    Caption =
      'In this demo, the filter compares values from the Price row to a' +
      'n expression calculated from List Price multiplied by a percenta' +
      'ge. Check Editable Expressions and click the Customize... button' +
      ' to edit these expressions.'
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
    Height = 33
    Width = 989
    object cxCheckBox1: TcxCheckBox
      Left = 9
      Top = 11
      Action = actExpressionEditing
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
    end
  end
  inherited memAboutText: TMemo
    Lines.Strings = (
      
        'A calculated filter item compares another filter item to a formu' +
        'la expression result instead of a constant value. A calculated f' +
        'ilter item'#39's expression follows Microsoft Excel formula-compatib' +
        'le syntax, except for references, and can include constants, ref' +
        'erences to fields, and function calls separated by operators.  E' +
        'nclose a field'#39's name in square brackets to refer to that field ' +
        'in a calculated field item'#39's expression. Calculated filter items' +
        ' use values obtained from referred fields to calculate results f' +
        'or each record.'
      ''
      
        'Click the "Customize..." button to invoke the Filter builder dia' +
        'log to look at two filter conditions with calculated field items' +
        ' whose expressions refer to the "List Price" field.'
      
        'To edit these expressions, close the Filter builder dialog, chec' +
        'k "Editable expressions", and invoke the dialog again. Then, cli' +
        'ck any "f(x)" button to invoke the "Expression Editor" dialog. T' +
        'his dialog allows  you to preview and edit the corresponding fil' +
        'ter item'#39's expression. Select the Fields category in the dialog ' +
        'and double-click any listed field to insert it at the current ca' +
        'ret position. You can also type the field'#39's name enclosed in squ' +
        'are brackets or double-click a built-in constant, operator, or f' +
        'unction to add it to the expression. Click OK to apply all the c' +
        'hanges made to the expression.')
  end
  object VerticalGrid: TcxDBVerticalGrid [4]
    Left = 0
    Top = 65
    Width = 989
    Height = 467
    Align = alClient
    FilterBox.Visible = fvNonEmpty
    LayoutStyle = lsMultiRecordView
    OptionsView.CellAutoHeight = True
    OptionsView.GridLineColor = clGray
    OptionsView.RowHeaderWidth = 132
    OptionsView.RowHeight = 20
    OptionsView.ValueWidth = 170
    OptionsView.RecordsInterval = 2
    OptionsBehavior.RowFiltering = bTrue
    Navigator.Buttons.CustomButtons = <>
    ScrollbarAnnotations.CustomAnnotations = <>
    TabOrder = 3
    DataController.DataSource = dmCars.dsCarOrdersAndDelivery
    Version = 1
    ConditionalFormatting = {
      060000000A000000310000005400640078005300700072006500610064005300
      680065006500740043006F006E0064006900740069006F006E0061006C004600
      6F0072006D0061007400740069006E006700520075006C006500450078007000
      720065007300730069006F006E00D10000000B000000000000000B000000FFFF
      FF7F0001000000200B00000007000000430061006C0069006200720069000000
      00FF666600000000200000000020000000002000000000200000000020000700
      0000470045004E004500520041004C000000000000020000000000000000012E
      0000003D0041004E00440028004E004F00540028005B0043006F006D0070006C
      0065007400650064005D0029002C005B00440065006C00690076006500720079
      00200044006100740065005D003C0054004F0044004100590028002900290000
      0000003100000054006400780053007000720065006100640053006800650065
      00740043006F006E0064006900740069006F006E0061006C0046006F0072006D
      0061007400740069006E006700520075006C0065004500780070007200650073
      00730069006F006E00D10000000B000000000000000B000000FFFFFF7F000100
      0000200B00000007000000430061006C0069006200720069000000006EA69200
      0000002000000000200000000020000000002000000000200007000000470045
      004E004500520041004C000000000000020000000000000000012E0000003D00
      41004E00440028004E004F00540028005B0043006F006D0070006C0065007400
      650064005D0029002C005B00440065006C006900760065007200790020004400
      6100740065005D003E0054004F00440041005900280029002900000000003100
      0000540064007800530070007200650061006400530068006500650074004300
      6F006E0064006900740069006F006E0061006C0046006F0072006D0061007400
      740069006E006700520075006C00650045007800700072006500730073006900
      6F006E00D10000000B000000000000000B000000FFFFFF7F0001000000200B00
      000007000000430061006C006900620072006900000000B6D0E4000000002000
      000000200000000020000000002000000000200007000000470045004E004500
      520041004C000000000000020000000000000000012E0000003D0041004E0044
      0028004E004F00540028005B0043006F006D0070006C0065007400650064005D
      0029002C005B00440065006C0069007600650072007900200044006100740065
      005D003D0054004F004400410059002800290029000000000031000000540064
      0078005300700072006500610064005300680065006500740043006F006E0064
      006900740069006F006E0061006C0046006F0072006D0061007400740069006E
      006700520075006C006500450078007000720065007300730069006F006E009F
      000000060000000000000006000000FFFFFF7F0001008000000B000000070000
      00430061006C0069006200720069000001000000002000000020000000002000
      00000020000000002000000000200007000000470045004E004500520041004C
      00000000000002000000000000000001150000003D005B005000720069006300
      65005D003E005B004C006900730074002000500072006900630065005D000000
      0000310000005400640078005300700072006500610064005300680065006500
      740043006F006E0064006900740069006F006E0061006C0046006F0072006D00
      61007400740069006E006700520075006C006500450078007000720065007300
      730069006F006E009F000000060000000000000006000000FFFFFF7F00018000
      00000B00000007000000430061006C0069006200720069000001000000002000
      0000200000000020000000002000000000200000000020000700000047004500
      4E004500520041004C00000000000002000000000000000001150000003D005B
      00500072006900630065005D003C005B004C0069007300740020005000720069
      00630065005D00000000002E0000005400640078005300700072006500610064
      005300680065006500740043006F006E0064006900740069006F006E0061006C
      0046006F0072006D0061007400740069006E006700520075006C006500440061
      00740061004200610072003C000000050000000000000005000000FFFFFF7F00
      0000000000000000000001000000000000000000000000010000000000002000
      000020FFFFFF1F008BEF00}
    object VerticalGridRecId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'RecId'
      Visible = False
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object VerticalGridCarInfoCategory: TcxCategoryRow
      Properties.Caption = 'Car Info'
      ID = 1
      ParentID = -1
      Index = 1
      Version = 1
    end
    object VerticalGridTrademark: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Trademark'
      ID = 2
      ParentID = 1
      Index = 0
      Version = 1
    end
    object VerticalGridName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Name'
      ID = 3
      ParentID = 1
      Index = 1
      Version = 1
    end
    object VerticalGridBodyStyle: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'BodyStyle'
      ID = 4
      ParentID = 1
      Index = 2
      Version = 1
    end
    object VerticalGridBodyStyleID: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'BodyStyleID'
      Visible = False
      ID = 5
      ParentID = -1
      Index = 2
      Version = 1
    end
    object VerticalGridOrderInfoCategory: TcxCategoryRow
      Properties.Caption = 'Order Info'
      ID = 6
      ParentID = -1
      Index = 3
      Version = 1
    end
    object VerticalGridPrice: TcxDBEditorRow
      Properties.Caption = 'List Price'
      Properties.DataBinding.FieldName = 'Price'
      ID = 7
      ParentID = 6
      Index = 0
      Version = 1
    end
    object VerticalGridSalesPrice: TcxDBEditorRow
      Properties.Caption = 'Price'
      Properties.DataBinding.FieldName = 'SalesPrice'
      ID = 8
      ParentID = 6
      Index = 1
      Version = 1
    end
    object VerticalGridSalesDate: TcxDBEditorRow
      Properties.Caption = 'Sales Date'
      Properties.DataBinding.FieldName = 'SalesDate'
      ID = 9
      ParentID = 6
      Index = 2
      Version = 1
    end
    object VerticalGridDeliveryCategory: TcxCategoryRow
      Properties.Caption = 'Delivery Info'
      ID = 10
      ParentID = -1
      Index = 4
      Version = 1
    end
    object VerticalGridDeliveryFrom: TcxDBEditorRow
      Properties.Caption = 'From'
      Properties.DataBinding.FieldName = 'DeliveryFrom'
      ID = 11
      ParentID = 10
      Index = 0
      Version = 1
    end
    object VerticalGridDeliveryTo: TcxDBEditorRow
      Properties.Caption = 'To'
      Properties.DataBinding.FieldName = 'DeliveryTo'
      ID = 12
      ParentID = 10
      Index = 1
      Version = 1
    end
    object VerticalGridDeliveryDate: TcxDBEditorRow
      Properties.Caption = 'Delivery Date'
      Properties.DataBinding.FieldName = 'DeliveryDate'
      ID = 13
      ParentID = 10
      Index = 2
      Version = 1
    end
    object VerticalGridDeliveryComplete: TcxDBEditorRow
      Properties.Caption = 'Completed'
      Properties.DataBinding.FieldName = 'DeliveryComplete'
      ID = 14
      ParentID = 10
      Index = 3
      Version = 1
    end
  end
  inherited mmMain: TMainMenu
    Left = 220
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
    object actExpressionEditing: TAction
      AutoCheck = True
      Caption = 'Editable Expressions'
      OnExecute = actExpressionEditingExecute
    end
  end
end
