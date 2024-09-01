object frmstart: Tfrmstart
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 129
  ClientWidth = 288
  Color = clHighlightText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 8
    Width = 214
    Height = 13
    Caption = 'B9297DB4-C17F-42DD-B67C-7A713E42F839'
  end
  object edt_k8: TEdit
    Left = 144
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edt_ms: TEdit
    Left = 144
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'megasena'
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 40
  end
end
