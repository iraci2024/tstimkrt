object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DBAdvResponsiveList Demo'
  ClientHeight = 675
  ClientWidth = 804
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    804
    675)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 72
    Height = 13
    Caption = '&Filter on name:'
    FocusControl = Edit1
  end
  object DBAdvResponsiveList1: TDBAdvResponsiveList
    Left = 8
    Top = 40
    Width = 785
    Height = 469
    Cursor = crDefault
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    BorderColor = clNone
    Conditions = <
      item
        Columns = 3
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = 300
        WidthTo = 400
        Tag = 0
      end
      item
        Columns = 3
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = 400
        WidthTo = 500
        Tag = 0
      end
      item
        Columns = 5
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = 500
        WidthTo = 600
        Tag = 0
      end
      item
        Columns = 6
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = 600
        WidthTo = 700
        Tag = 0
      end
      item
        Columns = 7
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = 700
        Tag = 0
      end
      item
        Columns = 2
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = 200
        WidthTo = 300
        Tag = 0
      end
      item
        Columns = 1
        HeaderTemplate = '<b>(#PRENAME) (#NAME)</b>'
        HeightFrom = -1
        ItemHeight = 200
        Template = 
          'Born: <b>(#YOB)</b>'#13#10'<IMG src="(#PICTURE)">'#13#10'Movie:<br><b>(#MOVI' +
          'E)</b>'#13#10
        WidthFrom = -1
        WidthTo = 200
        Tag = 0
      end>
    Items = <>
    Version = '1.1.1.0'
    DataSource = DataSource1
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 524
    Width = 785
    Height = 143
    Anchors = [akLeft, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Edit1: TEdit
    Left = 95
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 520
    Top = 528
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 520
    Top = 592
  end
end
