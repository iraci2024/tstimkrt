object fmPrompt: TfmPrompt
  Left = 369
  Top = 110
  BorderStyle = bsDialog
  Caption = 'Keyboard-interactive authentication'
  ClientHeight = 92
  ClientWidth = 233
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
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lbName: TLabel
      Left = 16
      Top = 8
      Width = 33
      Height = 13
      Caption = 'Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbInstr: TLabel
      Left = 16
      Top = 32
      Width = 20
      Height = 13
      Caption = 'Instr'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 56
    Width = 233
    Height = 36
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object btOK: TButton
      Left = 74
      Top = 3
      Width = 85
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
