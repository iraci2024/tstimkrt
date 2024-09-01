object AdvTrackBarGalleryForm: TAdvTrackBarGalleryForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TAdvTrackBar Gallery'
  ClientHeight = 294
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 247
    Width = 126
    Height = 13
    Caption = 'Name for saving to gallery'
  end
  object GroupBox1: TGroupBox
    Left = 208
    Top = 8
    Width = 273
    Height = 233
    Caption = 'Preview'
    TabOrder = 0
    object AdvTrackBar1: TAdvTrackBar
      Left = 10
      Top = 20
      Width = 150
      Height = 45
      BorderColor = clNone
      BorderColorDisabled = clNone
      Buttons.BorderColor = 9262895
      Buttons.BorderColorHot = 10079963
      Buttons.BorderColorDown = 4548219
      Buttons.Color = 15653832
      Buttons.ColorTo = 16178633
      Buttons.ColorHot = 15465983
      Buttons.ColorHotTo = 11332863
      Buttons.ColorDown = 7778289
      Buttons.ColorDownTo = 4296947
      Buttons.ColorMirror = 15586496
      Buttons.ColorMirrorTo = 16245200
      Buttons.ColorMirrorHot = 5888767
      Buttons.ColorMirrorHotTo = 10807807
      Buttons.ColorMirrorDown = 946929
      Buttons.ColorMirrorDownTo = 5021693
      Buttons.GradientMirror = ggVertical
      ColorTo = clNone
      ColorDisabled = clNone
      ColorDisabledTo = clNone
      Direction = gdHorizontal
      Color = clNone
      Slider.BorderColor = 12752500
      Slider.BorderColorDisabled = clBlack
      Slider.Color = clWhite
      Slider.ColorTo = clBlack
      Slider.ColorDisabled = clBlack
      Slider.ColorDisabledTo = clBlack
      Slider.ColorCompleted = clNone
      Slider.ColorCompletedTo = clNone
      Slider.ColorCompletedDisabled = clNone
      Slider.ColorCompletedDisabledTo = clNone
      Slider.Direction = gdHorizontal
      TabOrder = 0
      Thumb.BorderColor = 10317632
      Thumb.BorderColorHot = 10079963
      Thumb.BorderColorDown = 4548219
      Thumb.BorderColorDisabled = clBlack
      Thumb.Color = 15653832
      Thumb.ColorTo = 16178633
      Thumb.ColorDown = 7778289
      Thumb.ColorDownTo = 4296947
      Thumb.ColorHot = 15465983
      Thumb.ColorHotTo = 11332863
      Thumb.ColorDisabled = clBlack
      Thumb.ColorDisabledTo = clBlack
      Thumb.ColorMirror = 15586496
      Thumb.ColorMirrorTo = 16245200
      Thumb.ColorMirrorHot = 5888767
      Thumb.ColorMirrorHotTo = 10807807
      Thumb.ColorMirrorDown = 946929
      Thumb.ColorMirrorDownTo = 5021693
      Thumb.ColorMirrorDisabled = clBlack
      Thumb.ColorMirrorDisabledTo = clBlack
      Thumb.Gradient = ggVertical
      Thumb.GradientMirror = ggRadial
      Thumb.Shape = tsPointer
      TickMark.Color = clBlack
      TickMark.ColorDisabled = clBlack
      TickMark.Font.Charset = DEFAULT_CHARSET
      TickMark.Font.Color = clWindowText
      TickMark.Font.Height = -11
      TickMark.Font.Name = 'Tahoma'
      TickMark.Font.Style = []
      TrackHint = False
      TrackLabel.Font.Charset = DEFAULT_CHARSET
      TrackLabel.Font.Color = clWindowText
      TrackLabel.Font.Height = -11
      TrackLabel.Font.Name = 'Tahoma'
      TrackLabel.Font.Style = []
      TrackLabel.Format = 'Pos: %d'
      Version = '1.7.3.0'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 233
    Caption = 'Gallery'
    TabOrder = 1
    object ListBox1: TListBox
      Left = 7
      Top = 19
      Width = 170
      Height = 204
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
  end
  object Edit1: TEdit
    Left = 15
    Top = 263
    Width = 170
    Height = 21
    TabOrder = 2
  end
  object Button1: TButton
    Left = 191
    Top = 263
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 325
    Top = 263
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Button3: TButton
    Left = 406
    Top = 263
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
