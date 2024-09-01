object fmChatForm: TfmChatForm
  Left = 0
  Top = 0
  Caption = 'SignalR client'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 513
    Top = 49
    Height = 383
    MinSize = 20
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 49
    Align = alTop
    TabOrder = 0
    object lbNickName: TLabel
      Left = 8
      Top = 8
      Width = 46
      Height = 13
      Caption = 'NickName'
    end
    object edNickName: TEdit
      Left = 95
      Top = 8
      Width = 417
      Height = 21
      TabOrder = 0
      OnKeyDown = edNickNameKeyDown
    end
    object btConnect: TButton
      Left = 531
      Top = 10
      Width = 105
      Height = 33
      Caption = 'Connect'
      TabOrder = 1
      OnClick = btConnectClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 432
    Width = 640
    Height = 48
    Align = alBottom
    TabOrder = 1
    object lbMessage: TLabel
      Left = 16
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Message'
    end
    object edMessage: TEdit
      Left = 95
      Top = 6
      Width = 417
      Height = 21
      Enabled = False
      TabOrder = 1
      OnKeyDown = edMessageKeyDown
    end
    object btSend: TButton
      Left = 531
      Top = 6
      Width = 105
      Height = 33
      Caption = 'Send'
      Enabled = False
      TabOrder = 0
      OnClick = btSendClick
    end
  end
  object meChat: TMemo
    Left = 0
    Top = 49
    Width = 513
    Height = 383
    Align = alLeft
    ReadOnly = True
    TabOrder = 2
  end
  object lbUserName: TListBox
    Left = 516
    Top = 49
    Width = 124
    Height = 383
    Align = alClient
    ItemHeight = 13
    TabOrder = 3
  end
  object SignalRClient: TScHubConnection
    Url = 'http://127.0.0.1:57129/chat'
    HttpConnectionOptions.Url = 'http://127.0.0.1:57129/chat'
    HttpConnectionOptions.HeadersText = ''
    EventsCallMode = ecSynchronous
    Left = 56
    Top = 80
  end
end
