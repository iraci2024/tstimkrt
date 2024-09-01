object Form1: TForm1
  Width = 640
  Height = 480
  object WebButton1: TWebButton
    Left = 8
    Top = 8
    Width = 96
    Height = 25
    Caption = 'Connect'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = WebButton1Click
  end
  object WebMemo1: TWebMemo
    Left = 8
    Top = 39
    Width = 449
    Height = 321
    AutoSize = False
    HeightPercent = 100.000000000000000000
    SelLength = 0
    SelStart = 0
    WidthPercent = 100.000000000000000000
  end
  object TMSFNCWhatsAppReceiver1: TTMSFNCWhatsAppReceiver
    Left = 480
    Top = 8
    Width = 26
    Height = 26
    Visible = True
    OnConnect = TMSFNCWhatsAppReceiver1Connect
    OnDisconnect = TMSFNCWhatsAppReceiver1Disconnect
    OnWhatsAppMessageReceived = TMSFNCWhatsAppReceiver1WhatsAppMessageReceived
  end
end
