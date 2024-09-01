object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'TAdvSignatureCapture demo'
  ClientHeight = 393
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object AdvSignatureCapture1: TAdvSignatureCapture
    Left = 16
    Top = 32
    Width = 400
    Height = 300
    ClearSig.X = 0
    ClearSig.Y = 0
    ClearSig.Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000300000
      003008060000005702F9870000000467414D410000B18F0BFC61050000000970
      48597300000EC100000EC101B8916BED0000001974455874536F667477617265
      005061696E742E4E45542076332E352E343E8DCC76000000DE4944415478DAED
      97D10D82401044A1034BA0045BB0144BB0042AD10E2CC116EC404AB004870F13
      4222DC5D769C6C3293CC17B7F05E48B8A3EF92A7570358400D6001358005D400
      16500358400D600135800502EE7140AFE8883E8933148119E4811ED1377A2A00
      6A99A1082C41BED9036A99F9ABC01650ED7ABA400D14053E42A0046E62C14709
      EC494C2CF848812D8975C2E0A3054A2442E119027306F4F5E3DA19BD453ECC6F
      A0029E22E1AF50977C1F48BD13A73F0BA53F8DAE81D2FD0F2C81EEE8A502A465
      8626208D05D4B1803A1650C702EA58401D0BA8935EE00353215C311F8C6C1C00
      00000049454E44AE426082}
    ParentColor = False
    Text = 'Sign here.'
    Version = '1.0.1.1'
  end
  object Label1: TLabel
    Left = 16
    Top = 13
    Width = 175
    Height = 13
    Caption = 'Draw with mouse or on touch screen'
  end
  object Label2: TLabel
    Left = 440
    Top = 13
    Width = 44
    Height = 13
    Caption = 'Pen color'
  end
  object PaintBox1: TPaintBox
    Left = 440
    Top = 168
    Width = 100
    Height = 105
    OnMouseDown = PaintBox1MouseDown
    OnPaint = PaintBox1Paint
  end
  object Label3: TLabel
    Left = 440
    Top = 149
    Width = 47
    Height = 13
    Caption = 'Pen width'
  end
  object Button1: TButton
    Left = 16
    Top = 346
    Width = 153
    Height = 25
    Caption = 'Save signature to image'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ColorGrid1: TColorGrid
    Left = 440
    Top = 32
    Width = 100
    Height = 100
    TabOrder = 1
    OnClick = ColorGrid1Click
  end
  object SaveDialog1: TSaveDialog
    Filter = 'PNG files|*.png'
    Left = 272
    Top = 336
  end
end
