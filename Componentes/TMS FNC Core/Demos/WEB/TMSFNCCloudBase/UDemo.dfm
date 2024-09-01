object Form1: TForm1
  Width = 397
  Height = 567
  Caption = 'TMSFNCCloudBase Demo'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormContainer = 'appcontent'
  ParentFont = False
  OnCreate = WebFormCreate
  object WebImageControl1: TWebImageControl
    Left = 8
    Top = 392
    Width = 113
    Height = 153
    ChildOrder = 3
  end
  object WebLabel1: TWebLabel
    Left = 148
    Top = 392
    Width = 227
    Height = 78
    Caption = 
      'Search for the show you want. TMSFNCCloudBase will send a messag' +
      'e to the API and will retrieve a list of shows. Once you select ' +
      'one of them a simple GET request will be send with the same TMSF' +
      'NCCloudBase to retrieve the image of the show.'
    WordWrap = True
  end
  object WebEdit1: TWebEdit
    Left = 8
    Top = 8
    Width = 273
    Height = 25
    Text = 'CSI'
  end
  object WebButton1: TWebButton
    Left = 287
    Top = 8
    Width = 96
    Height = 25
    Caption = 'Search Show'
    ChildOrder = 1
    OnClick = WebButton1Click
  end
  object WebListBox1: TWebListBox
    Left = 8
    Top = 39
    Width = 375
    Height = 337
    ItemHeight = 13
    ItemIndex = -1
    OnClick = WebListBox1Click
  end
end
