object Form1: TForm1
  Width = 640
  Height = 480
  FormContainer = 'appcontent'
  object lblTitle: TWebLabel
    Left = 368
    Top = 22
    Width = 111
    Height = 13
    Caption = 'WebSocket client demo'
    ElementID = 'title'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object lblDescr: TWebLabel
    Left = 448
    Top = 24
    Width = 678
    Height = 13
    Caption = 
      'This demo connects to a websocket server that allows collaborati' +
      've editing of this grid. In this demo only 15 characters are per' +
      'sisted in a cell.'
    ElementID = 'description'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object grid: TWebStringGrid
    Left = 0
    Top = 41
    Width = 640
    Height = 439
    Align = alClient
    ColCount = 10
    Enabled = False
    RowCount = 10
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goFixedRowDefAlign]
    TabOrder = 0
    OnSetEditText = gridSetEditText
    RangeEdit.Max = 100.000000000000000000
    RangeEdit.Step = 1.000000000000000000
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnValidateEdit = gridValidateEdit
  end
  object pnl: TWebPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 41
    Align = alTop
    BorderStyle = bsNone
    ChildOrder = 1
    object connBtn: TWebButton
      Left = 10
      Top = 9
      Width = 96
      Height = 25
      Caption = 'Connect'
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = connBtnClick
    end
  end
  object WebSocketClient1: TWebSocketClient
    UseSSL = True
    Port = 8960
    PathName = '/'
    OnConnect = WebSocketClient1Connect
    OnDisconnect = WebSocketClient1Disconnect
    OnDataReceived = WebSocketClient1DataReceived
    Left = 280
    Top = 88
  end
end
