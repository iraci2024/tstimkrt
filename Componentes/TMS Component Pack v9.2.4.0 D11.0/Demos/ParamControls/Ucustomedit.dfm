object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Custom parameter editor'
  ClientHeight = 291
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 303
    Height = 13
    Caption = 'Parameter VAL is edited with a custom inplace parameter editor'
  end
  object ParamListBox1: TParamListBox
    Left = 24
    Top = 49
    Width = 497
    Height = 177
    Items.Strings = (
      
        'This is a custom editor for a <a href="CUSTOM" class="CUSTOM" pr' +
        'ops="EXTRA" hint="HINT">VAL</a> parameter')
    TabOrder = 0
    AdvanceOnReturn = False
    EmptyParam = '?'
    Multiline = False
    ParamHint = False
    ShadowColor = clGray
    ShadowOffset = 1
    ShowSelection = True
    Version = '1.3.3.9'
    OnParamCustomEdit = ParamListBox1ParamCustomEdit
  end
  object CheckListEdit1: TCheckListEdit
    Left = 24
    Top = 248
    Width = 121
    Height = 21
    TabOrder = 1
    Visible = False
    AutoDropWidthSize = False
    DropWidth = 121
    DropFont.Charset = DEFAULT_CHARSET
    DropFont.Color = clWindowText
    DropFont.Height = -11
    DropFont.Name = 'Tahoma'
    DropFont.Style = []
    Items.Strings = (
      'BMW'
      'Porsche'
      'Mercedes'
      'Audi'
      'Tesla')
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clWindowText
    LabelFont.Height = -11
    LabelFont.Name = 'Tahoma'
    LabelFont.Style = []
    TextDelimiter = ','
    TextEndChar = ']'
    TextStartChar = '['
    OnClose = CheckListEdit1Close
    Version = '1.4.0.2'
  end
end
