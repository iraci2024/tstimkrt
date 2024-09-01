object fmNewName: TfmNewName
  Left = 419
  Top = 210
  Width = 300
  Height = 143
  Caption = 'SFTP'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbCaption: TLabel
    Left = 16
    Top = 16
    Width = 36
    Height = 13
    Caption = 'Caption'
  end
  object edNewName: TEdit
    Left = 16
    Top = 40
    Width = 257
    Height = 21
    TabOrder = 0
  end
  object btOk: TButton
    Left = 16
    Top = 72
    Width = 123
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 152
    Top = 72
    Width = 123
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
