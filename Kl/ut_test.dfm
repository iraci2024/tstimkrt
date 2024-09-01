object TNT_TEST: TTNT_TEST
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsNone
  ClientHeight = 485
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Pepita: TPanel
    Left = 0
    Top = 0
    Width = 644
    Height = 485
    Align = alClient
    Caption = 'Pepita'
    ParentBackground = False
    TabOrder = 0
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 642
      Height = 483
      Align = alClient
      ExplicitLeft = 2
      ExplicitTop = 2
    end
    object Pbox: TPanel
      Left = 16
      Top = 112
      Width = 353
      Height = 313
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 440
    Top = 256
  end
end
