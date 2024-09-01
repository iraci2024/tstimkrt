object Form1: TForm1
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'TAdvResponsiveList Demo'
  ClientHeight = 351
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    259
    351)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 31
    Width = 33
    Height = 13
    Caption = 'Car list'
  end
  object Label2: TLabel
    Left = 34
    Top = 317
    Width = 158
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'Resize the form to see effect on the responsive list'
    WordWrap = True
    ExplicitTop = 311
  end
  object AdvResponsiveList1: TAdvResponsiveList
    Left = 32
    Top = 48
    Width = 196
    Height = 263
    Cursor = crDefault
    HorzScrollBar.Range = 174
    HorzScrollBar.Tracking = True
    VertScrollBar.Range = 447
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    Appearance.HotItemBorderColor = clHighlight
    Appearance.HotItemTextColor = clBlack
    Appearance.HotItemColor = clInfoBk
    BorderColor = clNone
    Conditions = <
      item
        Columns = 1
        HeightFrom = -1
        ItemHeight = 150
        WidthTo = 250
        Tag = 0
      end
      item
        Columns = 2
        HeightFrom = -1
        ItemHeight = 150
        WidthFrom = 250
        WidthTo = 500
        Tag = 0
      end
      item
        Columns = 3
        HeightFrom = -1
        ItemHeight = 150
        WidthFrom = 500
        WidthTo = 750
        Tag = 0
      end
      item
        Columns = 4
        HeightFrom = -1
        ItemHeight = 150
        WidthFrom = 750
        Tag = 0
      end>
    Items = <
      item
        Content = 'item 1'
        HeaderColor = clHighlight
        Visible = True
        ControlName = ''
      end
      item
        Content = 'item 2'
        HeaderColor = clHighlight
        Visible = True
        ControlName = ''
      end
      item
        Content = 'item 3'
        HeaderColor = clHighlight
        Visible = True
        ControlName = ''
      end>
    Version = '1.3.0.0'
  end
  object CheckBox1: TCheckBox
    Left = 32
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Multi-select'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
end
