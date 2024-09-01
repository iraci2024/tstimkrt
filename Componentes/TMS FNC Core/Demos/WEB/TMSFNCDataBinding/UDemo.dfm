object Form1: TForm1
  Width = 782
  Height = 649
  Caption = 'TMS FNC DataBinding Demo'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormContainer = 'appcontent'
  ParentFont = False
  OnCreate = WebFormCreate
  object WebPanel1: TWebPanel
    Left = 0
    Top = 0
    Width = 782
    Height = 41
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    Align = alTop
    BorderColor = clSilver
    BorderStyle = bsSingle
    object WebButton1: TWebButton
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 96
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Activate'
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = WebButton1Click
      ExplicitLeft = -22
    end
    object WebButton2: TWebButton
      AlignWithMargins = True
      Left = 106
      Top = 5
      Width = 96
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Edit'
      ChildOrder = 1
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = WebButton2Click
      ExplicitLeft = 352
      ExplicitTop = 24
      ExplicitHeight = 25
    end
  end
  object WebListBox1: TWebListBox
    Left = 8
    Top = 56
    Width = 200
    Height = 200
    HeightPercent = 100.000000000000000000
    ItemHeight = 13
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object WebStringGrid1: TWebStringGrid
    Left = 8
    Top = 272
    Width = 753
    Height = 361
    TabOrder = 2
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object TMSFNCDataBinder1: TTMSFNCDataBinder
    Left = 664
    Top = 88
    Width = 26
    Height = 26
    Visible = True
    Items = <>
  end
  object WebDataSource1: TWebDataSource
    DataSet = WebClientDataSet1
    Left = 480
    Top = 208
  end
  object WebClientConnection1: TWebClientConnection
    Active = False
    Left = 536
    Top = 144
  end
  object WebClientDataSet1: TWebClientDataSet
    Connection = WebClientConnection1
    Params = <>
    Left = 624
    Top = 216
  end
end
