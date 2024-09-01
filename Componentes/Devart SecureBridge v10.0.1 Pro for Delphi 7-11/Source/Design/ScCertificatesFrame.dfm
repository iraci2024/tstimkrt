inherited ScCertificatesFrame: TScCertificatesFrame
  Width = 515
  Height = 406
  object Label2: TLabel [0]
    Left = 8
    Top = 8
    Width = 84
    Height = 13
    Caption = 'Certificate names:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  inherited lbItemName: TListBox
    Height = 341
    Style = lbOwnerDrawFixed
    OnDrawItem = lbItemNameDrawItem
  end
  inherited PanelItem: TPanel
    Width = 331
    Height = 390
    TabOrder = 3
    object Label1: TLabel
      Left = 14
      Top = 12
      Width = 47
      Height = 13
      Caption = 'CertName'
    end
    object Label3: TLabel
      Left = 14
      Top = 54
      Width = 58
      Height = 13
      Caption = 'Properties'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 14
      Top = 308
      Width = 43
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Algorithm'
    end
    object Label7: TLabel
      Left = 146
      Top = 308
      Width = 40
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'BitCount'
    end
    object Label4: TLabel
      Left = 14
      Top = 292
      Width = 22
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Key'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edCertName: TEdit
      Left = 14
      Top = 26
      Width = 304
      Height = 21
      TabOrder = 0
      OnExit = edCertNameExit
    end
    object cbIsPrivate: TCheckBox
      Left = 246
      Top = 326
      Width = 67
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'IsPrivate'
      TabOrder = 6
    end
    object btImportKey: TButton
      Left = 14
      Top = 354
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Import key from...'
      TabOrder = 7
      OnClick = btImportKeyClick
    end
    object btExportPrivate: TButton
      Left = 117
      Top = 354
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Export private key'
      TabOrder = 8
      OnClick = btExportPrivateClick
    end
    object btExportPublic: TButton
      Left = 219
      Top = 354
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Export &public key'
      TabOrder = 9
      OnClick = btExportPublicClick
    end
    object mmProperties: TMemo
      Left = 14
      Top = 72
      Width = 304
      Height = 173
      Anchors = [akLeft, akTop, akBottom]
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object edAlgorithm: TEdit
      Left = 14
      Top = 324
      Width = 121
      Height = 21
      Anchors = [akLeft, akBottom]
      ReadOnly = True
      TabOrder = 4
    end
    object edBitCount: TEdit
      Left = 146
      Top = 324
      Width = 78
      Height = 21
      Anchors = [akLeft, akBottom]
      ReadOnly = True
      TabOrder = 5
    end
    object btImportCert: TButton
      Left = 14
      Top = 254
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Import from...'
      TabOrder = 2
      OnClick = btImportCertClick
    end
    object btExportCert: TButton
      Left = 117
      Top = 254
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Export to...'
      TabOrder = 3
      OnClick = btExportCertClick
    end
  end
  object btNew: TButton
    Left = 8
    Top = 373
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&New'
    TabOrder = 1
    OnClick = btNewClick
  end
  object btDelete: TButton
    Left = 87
    Top = 373
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 2
    OnClick = btDeleteClick
  end
end
