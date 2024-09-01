object fmFileView: TfmFileView
  Left = 191
  Top = 114
  Width = 547
  Height = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 539
    Height = 430
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyDown = MemoKeyDown
  end
end
