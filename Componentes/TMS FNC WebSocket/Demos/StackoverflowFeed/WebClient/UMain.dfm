object Form1: TForm1
  Width = 1061
  Height = 924
  object WebLabel1: TWebLabel
    Left = 239
    Top = 48
    Width = 326
    Height = 19
    Caption = 'Latest questions posted to stackoverflow.com:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
  end
  object WebLabel2: TWebLabel
    Left = 16
    Top = 49
    Width = 173
    Height = 19
    Caption = 'New question posted to:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
  end
  object WebButton1: TWebButton
    Left = 16
    Top = 18
    Width = 96
    Height = 25
    Caption = 'Connect'
    ChildOrder = 1
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = WebButton1Click
  end
  object WebScrollBox1: TWebScrollBox
    Left = 239
    Top = 73
    Width = 561
    Height = 369
    BorderStyle = bsSingle
    ChildOrder = 2
    Color = clWhite
    ScrollBars = ssVertical
  end
  object WebListBox1: TWebListBox
    Left = 16
    Top = 74
    Width = 217
    Height = 369
    HeightPercent = 100.000000000000000000
    ItemHeight = 13
    WidthPercent = 100.000000000000000000
    ItemIndex = -1
  end
  object WebSocketClient1: TWebSocketClient
    Port = 80
    HostName = 'qa.sockets.stackexchange.com'
    OnConnect = WebSocketClient1Connect
    OnDisconnect = WebSocketClient1Disconnect
    OnDataReceived = WebSocketClient1DataReceived
    Left = 712
    Top = 16
  end
  object WebTimer1: TWebTimer
    Enabled = False
    Interval = 60000
    OnTimer = WebTimer1Timer
    Left = 624
    Top = 16
  end
end
