object ScColFrame: TScColFrame
  Left = 0
  Top = 0
  Width = 566
  Height = 317
  TabOrder = 0
  object lbItemName: TListBox
    Left = 8
    Top = 24
    Width = 204
    Height = 254
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbItemNameClick
  end
  object Panel1: TPanel
    Tag = 1
    Left = 225
    Top = 8
    Width = 333
    Height = 302
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = 48127
    TabOrder = 3
    object PanelItem: TPanel
      Left = 1
      Top = 1
      Width = 331
      Height = 300
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object btNew: TButton
    Left = 8
    Top = 285
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&New'
    TabOrder = 1
  end
  object btDelete: TButton
    Left = 112
    Top = 285
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 2
  end
end
