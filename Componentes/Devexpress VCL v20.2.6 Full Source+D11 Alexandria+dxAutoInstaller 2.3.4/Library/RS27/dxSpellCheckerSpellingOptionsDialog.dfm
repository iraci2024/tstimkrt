object fmSpellCheckerSpellingOptionsForm: TfmSpellCheckerSpellingOptionsForm
  Left = 279
  Top = 249
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'fmSpellCheckerSpellingOptionsForm'
  ClientHeight = 393
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000004004000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000050000
    001200000011000000050000000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF000000000013C56B0011C36900000000070000002C0981
    44A9087B4199000000180BBB62000BBA610009B9600009B85F0008B75E00FFFF
    FF00FFFFFF00FFFFFF000000000015C86D0000000008000000330C8D4BCC10C1
    67FF0EC066FF0000003D000000050CBC62000BBB61000ABA610009B95F00FFFF
    FF00FFFFFF00FFFFFF00000000000000000900000038109C55DB13C66BFF12C4
    6AFF10C268FF09753FB2000000190DBE65000DBD64000BBC62000ABB6100FFFF
    FF00FFFFFF00FFFFFF000000000A0000003D15AC5FE617CB70FF16C96EFF14C7
    6CFF12C56BFF12C369FF0000003F000000050EBF65000DBE64000DBC6300FFFF
    FF00FFFFFF00FFFFFF00000000201ABE6AEC1BD175FF1ACF73FF18CD71FF094F
    2BB115C46BFE13C66BFF0B7D43B80000001A0FC167000FC066000DBE6400FFFF
    FF00FFFFFF00FFFFFF000000001A179856BF1ED578FF1CCF74FE0C5C33890000
    00360F8549BC16C96EFF14C76DFF000000400000000610C268000FC16700FFFF
    FF00FFFFFF00FFFFFF00000000060000002D189E5BB814884D9D0000001F0000
    00080000003716B262EE17CB6FFF0E8549BE0000001B12C56A0011C368000000
    0000000000170000004F00000062000000530000003F0000005B000000650000
    004F000000260B59327F19CD72FE17CC71FF000000420000000613C66B000000
    00170000006EFFFFFFFFFFFFFFFFFFFFFFFF0000008FFFFFFFFFFFFFFFFFFFFF
    FFFF0000007000000037149352C91BD073FF12914FC50000001C15C96E000000
    003AFFFFFFFF000000B4000000CCFFFFFFFF000000ADFFFFFFFF0000009E0000
    008FFFFFFFFF000000530000003F1DD377FF1ACA71FB00000042000000070000
    00170000006EFFFFFFFFFFFFFFFFFFFFFFFF000000ADFFFFFFFF0000009E0000
    008FFFFFFFFF0000004F000000140E64398E1ED478FF149856C70000001D0000
    00000000002E0000008F000000C1FFFFFFFF0000009EFFFFFFFFFFFFFFFFFFFF
    FFFF0000006E00000017000000030000002719A25CD01FD377FD0000003C0000
    00000000003AFFFFFFFFFFFFFFFF0000006E00000074FFFFFFFF0000009E0000
    004F0000001725DE800025DD7F00000000070000003F21D97CFF158F528D0000
    0000000000170000004F0000004F000000170000004FFFFFFFFF0000004F28E2
    840028E1830027E0820025DF810024DD80000000000F0F5C34450000001B0000
    000000000000000000000000000000000000000000170000003A000000170000
    000000000000000000000000000000000000000000010000000400000001FE1F
    FFFFFC1FFFFFF80FFFFFF00FFFFFE007FFFFE007FFFFE003FFFFE003FFFF8001
    FFFF0001FFFF0000FFFF0000FFFF8000FFFF8030FFFF80F8FFFFF8F8FFFF}
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 393
    Height = 393
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cbIgnoreUpperCaseWords: TcxCheckBox
      Left = 60
      Top = 28
      Caption = 'cbIgnoreUpperCaseWords'
      Properties.OnChange = cbChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
    end
    object cbIgnoreMixedCaseWords: TcxCheckBox
      Left = 60
      Top = 51
      Caption = 'cbIgnoreMixedCaseWords'
      Properties.OnChange = cbChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
    end
    object cbIgnoreWordsWithNumbers: TcxCheckBox
      Left = 60
      Top = 74
      Caption = 'cbIgnoreWordsWithNumbers'
      Properties.OnChange = cbChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
    end
    object cbIgnoreRepeatedWords: TcxCheckBox
      Left = 60
      Top = 97
      Caption = 'cbIgnoreRepeatedWords'
      Properties.OnChange = cbChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
    end
    object cbIgnoreEmails: TcxCheckBox
      Left = 60
      Top = 120
      Caption = 'cbIgnoreEmails'
      Properties.OnChange = cbChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
    end
    object cbIgnoreUrls: TcxCheckBox
      Left = 60
      Top = 143
      Caption = 'cbIgnoreUrls'
      Properties.OnChange = cbChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
    end
    object ccbLanguages: TcxCheckComboBox
      Left = 120
      Top = 295
      Anchors = [akLeft, akTop, akRight]
      Properties.Items = <>
      Properties.OnChange = cbChange
      Style.HotTrack = False
      TabOrder = 7
      Width = 251
    end
    object btnOk: TcxButton
      Left = 152
      Top = 334
      Width = 73
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'btnOk'
      Default = True
      ModalResult = 1
      TabOrder = 8
      OnClick = btnApplyClick
    end
    object btnCancel: TcxButton
      Left = 231
      Top = 334
      Width = 73
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 9
    end
    object btnApply: TcxButton
      Left = 310
      Top = 334
      Width = 73
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'btnApply'
      TabOrder = 10
      OnClick = btnApplyClick
    end
    object btnEdit: TcxButton
      Left = 278
      Top = 216
      Width = 93
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'btnEdit'
      TabOrder = 6
      OnClick = btnEditClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object gbGeneralOptions: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'gbGeneralOptions'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = gbGeneralOptions
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cbIgnoreUpperCaseWords'
      CaptionOptions.Visible = False
      Control = cbIgnoreUpperCaseWords
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 147
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbIgnoreMixedCaseWords'
      CaptionOptions.Visible = False
      Control = cbIgnoreMixedCaseWords
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 146
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbIgnoreWordsWithNumbers'
      CaptionOptions.Visible = False
      Control = cbIgnoreWordsWithNumbers
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 158
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbIgnoreRepeatedWords'
      CaptionOptions.Visible = False
      Control = cbIgnoreRepeatedWords
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 141
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbIgnoreEmails'
      CaptionOptions.Visible = False
      Control = cbIgnoreEmails
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbIgnoreUrls'
      CaptionOptions.Visible = False
      Control = cbIgnoreUrls
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object gbCustomDictionary: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'gbCustomDictionary'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object gbInternationalDictionaries: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'gbInternationalDictionaries'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object lbLanguage: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'lbLanguage'
      Control = ccbLanguages
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 217
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup8: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnApply'
      CaptionOptions.Visible = False
      Control = btnApply
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutImageItem1: TdxLayoutImageItem
      Parent = gbInternationalDictionaries
      AlignHorz = ahLeft
      AlignVert = avClient
      Image.SourceDPI = 96
      Image.Data = {
        89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
        F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
        00206348524D00007A26000080840000FA00000080E8000075300000EA600000
        3A98000017709CBA513C00000AF34944415458479557095094471A6D2F904B06
        4414041C22210A8643C58031615040C220E15414914308370C8C8820301C8207
        37A85C0A88A388C8290A420C26F1C20BE550141415D718A3CB6E12ADDA24D6DB
        EEA9D1F240B33B55AFFA9F9EFEFB7DEFBBBA675C535313F9EBAFBF2478F1E245
        C39F7FFE99429FAF4ABF8BFEF8E38F64F64CE7EFD2318AA2913EB3EFB9F4D987
        8E1C8A531456D54FECC9FB3EE3274C241A3A9F1095E95A337F1A1E487FFAE86E
        F7F366B77C525D5D4DC46231A9A8A8E0949595A1A8A828B7A0A080E4E4E4901D
        3B7688D2D3D3BB535252780909090DB1B1B19D42A19044444488828383111010
        20F0F1F1E1AD5DBBD667D5AA5544CEF1C87BC10DEB258BB63E53D20DEB2DA4EB
        FE33D9AE52488D9527E5E5E5A4A4A484ECDAB54B909797D7999D9D3DBA6DDB36
        929A9A4A92929244F1F1F1C3313131228140301A1A1A2A080C0C24EBD7AF1F5E
        B76E5DEEEAD5AB89BBBB3B717272227C3EFFBDE43A41578859C6EF443F7A305D
        C9F528E4F8077229B936850E292C2C7CA976382323C3293939B99BAAF5A16A49
        7474B4283C3C7C342424A4D3DFDF7F94AAADF0F4F424542D9C9D9D798CD4C6C6
        86F0783CB278F1E2310D98E9DF45E6A78E12C3B8071B55D774BC907338543B51
        DFCD94927F44318D6CDDBA95A935A16A111717C7A36A2BA8DA86B0B03042D58A
        FCFCFC3AA95AE2E1E1C1A5A470707030B1B5B505257562A40B172E24262626C4
        C0C0E01D03A6FB9C26C6898F8949F2D38019BEA79FCAAD38DC256B91C4A7C4B3
        2966504C26542DA16A2B686C41630B165BAA1694944B494594B4939232A55C4A
        0A4ACA3333336B303636EE363434E4E8EBEB135D5D5DAEB6B6F61B06A8ADFD8E
        18C63F200BB7FCE6AA1D746548CEB1F697C9CB0A83A4CA67D25181623CA16A09
        8DED2825F5912A252E2E2EC3D4BD02AA54646969394A493BA9D25146CA945252
        1E2505251DD6D0D0E89E366D1A545555B92F9390B3AA8DCC89B943166CF99537
        3BF2C60505E7464CB62BCFA084BAD2D82BD17182A462BCBDBD5F2AE5B0985A5B
        5B134AEA646E6EEE434979464646A2B973E78AF4F4F4045C2E97435F19C7A0A6
        A6A64B4993959595454A4A4A26F2F2F2120F28B936133DC14DB220ED57C339B1
        F78EA9789C6049777082C667F3E87BB328542826B2B500C6AEDAD7CB494A385E
        6AF1443A4E7A0B6C8EA9616BC671437BC8FCB47FAB1B253DAE54F7F911720E35
        E764CC62ECE86F5C8AA914326CDD3B067CF5F37EE2F47315B13D1F2D51F292D8
        34F75BA3C02B6261E2BDD2DA94BBBB06D286B29E6DBEBAE9D9FA636B06F8C57E
        F5D6D9A1B15649BE669284A2CA9821A6A9FFDAAE1D781934EE8F275BE5054AC9
        D5A56B24E4EF18E0F8A892B8DCDD4DCCF65B48C8A7471C9AAD15DBB5E5EB96E6
        DB015D71703DFA355C1BF8F03BE189C0360FF836F2E15B6B8F80230E8868F018
        8EAE0FC9F4CBE57F6A9CF0D3C6D991032F145D9A3079F9DE1D52B76BD0519274
        AF7BF78D1058B6AF25A665AC3CC938F5F05C6BB5F01F5AB5E28EC334DF0B8699
        B3312BD1129AB11130DEBE018EFB57E2EB4A73B8EFB340489D2392DA7D91753E
        14FED57BFA0C370DFD53757507E4ECC50D1374961AD3FD34292449F776A77CC3
        808FB6E848C8A785155973427BCF714207A01ADE09F5A83CA8094AA112DA0E4E
        D065E8C5F7C2ABFE3C62BEAB4052E76E6C6E2B41C6C9642475E462595E0F66F8
        9EC51497AA7B320B851E52D7B3A49B34569B7E3B09C771023A667382AEB74E0D
        BB85A9CC80D07EA886F44235B8172A41740CBC068DF01EACAC7A8082BE8728BC
        58855DE713907F361B2EA517A0137C150A4E87B1245E386460ABC91A0E6BB772
        6FBB7ECC1C608B94837AB6A847F64033EA32B4A206A02DB80EEDC83E6845F442
        23F42A45373E4B1FC09673BF20EB8C08C5E7E321BEBA03FE074E432F6A1053DC
        8FC12A250BC24647B8E79895FD3F068C9B112636D68CEEB96D5D5207E7AA5C38
        148D6071DA2D98255EC7C2847ECC4FE88575F62D88BE7F88BCAE3C945D48445D
        7F0136357D8FB971FF809AD72998465522B12D169B8E3923E0F0170F1605CD64
        E5A72CAD8E57D93F9607C67FBE4B20E4EFCB46505B1484DF7E838CB37508120F
        C1A7F836220FDDC7B61F9E62E7956EE49C4940F9C5641C1D28454E67272CD247
        A0197011EAAB6B115D5388824B02C4B6BB607D93396C32B9ACFB4D9796DF1B15
        F076194E706B74A9F56DF382B03304DBBAE2B1B7271B7B2FD522B6A6170D377E
        47DBC8082A2E65407C653B5A6EECC1FE8B27B1A2E0366685F543D1B5195E3B0F
        E0405F3AB2BA822038B11C6B1B4CB0BC58A78D92B3EC6625F8C12A9818703AE8
        46CC652132FBD3201E2E46C3F07144D40DC26CCB136CA83E8B53C30D68BF25C6
        B78307D176F318FC2B07A027BC03D5352761155F87FD97CBB1F75A220DD16A04
        B458C0B5561F36159A7728313BF9A67CC800D6DB2746F7C73D4B1B4A41D9FD3C
        34FF7C105175DD30163D8551D2138AC7D8587D0617EE9FC0A59176C4D5F5423F
        6604D37DCF417B5D3DB25AEB5177B310B917C221E8B08347FDA77038A803EB7D
        339ED3BD3FA190F47F56E663352266C024E18D98672943F1281DC98478681F1C
        B7355282B330887B0883CD8F3077D343C41DB986DC0E6AD8E6616805D192A3DD
        2EA4AC19EDC3FB517E2D1989A756C1AF79119C6BF46053A5015E851A33602E85
        EADF1A10D6FDCD407C7F30F28636A1E6412E8E0E1F46E8DE16A87B366356681F
        F4378EE0E30DF7B120E936742306C1A1DDCE26B1098D7D7538743D0BDBCF0520
        A4D50A6EB57360776026538F2F4AA6B210CCF95F3C30D1BBD3AD2EB2DB03A21E
        1F140DC6A0E9413ECE3E3E82F4C6E39817DC0855CF93D08DBC05DDA8DB98E67D
        069A9EF5C83CD688A69BC5B421B1CAE1C39326DE8A83B3A8FA194C3D2C7254DA
        29F9C7D2526427E69821A0F3648279A14B8C57A735045D7CA45E5D8D3DB736E0
        28CD87561A8E8CC65A18853442DEA9016AEB4E43D1A519DEF9F5105F2A46CEF7
        B1886F778777BD191C0F70614DC99756AAC1B27C2A4C139533E9DE5C0AC5BFAB
        82F153BD37CF9F9FE536ECD5CA43D499A548BFE28EAD67FC904AFB4271673DB6
        1DEEC71CFFE3F4883D82B901ADA8BE7C138D8383A8EC7B82C8961438967F8C65
        25D36159AA2A215F52AC3AA2EB26EF4489D94938663B7EFD2C608928A3E253B8
        DD30CD8B2A59803575AC8E8DB1AA680382775F46EAC13E6CADE9876BDA8FF0CF
        EFC2CEF63BD879F6118AFB7FC3E6D3ADB0DF6B84CFF3556051A08CCF4B54609A
        3CA594EEA9278D3FBBC47CB0134AC2A060BFC340D9BBB8435B180CBDCD563048
        70C6BCF0A3B08AFB0EEE199438EF1C820BCFC12FFB07B8A49DC457A927B1A6B4
        1B319D7D70A95989F9D94A30C952C0BC34A5D36A3C195BA97A79B63723601FA6
        FA75BC9C67A3A41CA7B8E5F2A7ACA9BAA8EA57088EE7114C5F7B145CDF661805
        B7C022EA189644B7C03CA209F3FC0F43C7530CCD35627C99DA017E553016EF5E
        04DE4E876BC6C15FFAD1BD5807640D48A2FE6F0D587A80DD1725F73A5965B7B0
        150A6EE5271557364379650354561E81BA472D25AB81966735345789A1E65A89
        292BF640DEBE181CB73C18C625C1AE30EF47AB8D898C9C6DC6616195EEF94AE8
        7B3DB0741FFB9F20B1546284EC9224A3C90E953972CE87EE29B81C86BCF34128
        3989A1E4B80F8A2BCAA1C02FA357ED624CB22D848C6DE6889C554AB18291DD32
        FA2EBBF3B313F025F92BF51F0C8165851AB128E210DD14D631254630D729CA2C
        4958226B939F2C6B57DA226BBF7748D6BEFCF9A4AFCA9ECB2CCFBF2D6FBFA94D
        71B9DF5605B3A5ECF2C15C3E8DBD237D77BCF96E761F79F3F3A11C787B2DB39C
        258FAC7453D64ED9D1CA146A49C19ED91CFB8D11B3B59286F30EB374E26D03FE
        0BDC37439A84A8F4620000000049454E44AE426082}
      Index = 0
    end
    object dxLayoutImageItem2: TdxLayoutImageItem
      Parent = gbCustomDictionary
      AlignVert = avClient
      Image.SourceDPI = 96
      Image.Data = {
        89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
        F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
        00206348524D00007A26000080840000FA00000080E8000075300000EA600000
        3A98000017709CBA513C000009ED494441545847AD97795094E71DC7499B3A35
        5713C7366D629A3A353666DAE674622AB9D38E138F84E85AC00B3CA206149088
        1C02020222A2282B570884702887DC020ACBB9EC72BB07CBB90BC8C28AC82520
        7268BEFD3DCFB22B64F36777E633CFB33BCCFBF95DEFF3BE3C06C0CCDDC37DDD
        83070F1C676767CD681513E60F1F3E5CB0A7EF6282FFCEF63FFDF4933961461F
        3161CE36742DBE67BFB33D61CEAE4FDF4345C5A242F637261FF607F9F9F9A979
        7979829C9C1C4156569636232343907E255D909696A64D4949115CBE7C59909C
        9CAC4D4A4A122426260A121212B4045BF93E3E3E5E10171727888989D1464545
        092222220442A1501B1A1A2A08090911040505A532CF2F61A6E81EC3B973A1A8
        691D406DFB3002024FA15CAE43B972005EDE3E28ACED4661BD0E6EEEC79151A1
        46A6F8268EB9BA23B9B80D974A3570FED605F1D7544828EAC0A1C38E88B9AA44
        6C412BBEDE7F1091D93244E535232A578588EC2650F68F3156078CBFB86C5FDD
        DEC59BD2DF309375DD454868184A2A24189B9C45605008AE9756E22EED4FF805
        A2B0B88CEF3D7D4E22BFA80CA3B4F7F0F6C3D5EBA518BD370B374F1FE4158A30
        427B17772FE41414F3BDB38B3BB2AE5EC730ED872766119EADC4C153D958B5BF
        E8B9E5F6CA30924F139F9A356A462153752026FE1217352ADB101193C0F7F58A
        5608A3E3B9A85EDE82F3E1B1FCE235375408B910C5F7D206254E9D09E322719D
        1CBE81215C582E6D84C7097F0C4D3CE084652A702030132B0E29829FDE9C0B92
        07116666F5EA112E639919A18B312983497E0ECFCA00C98616A0170E8E33668D
        9CCF9063BDB7044BAC8B1E2EDE70396DD11BDFFC8DDAF15BB3BAF6111359CFAD
        21A3B47F781C8A1635E42D1DE8D2DD5920D3680778656A65CD0B6477483C9F81
        B1591C8CEEC04B7B2AB0787D926CD1BB6E9B48FE57E27766356D8F642CD35BC3
        13F0F1F1A1759C9752A66A878B8B0B2E845D84A3A3231AA82D2CBBEB2595D8BD
        7B377C4F06D0F0392097E6804B49C6848CDB630F38571AA7F077A7063C6B9186
        456B3C8E937825F122F18499B465D0584E56CA221A403737371ABE725ECA069A
        097B7B7B9EA1D7091F24A76640D6A281B5B535720B4446D97CA141DC7F970255
        4D617D90064F5A64E14D9B387627BC4FFC85789AF8B599B8D950567DEF4E0505
        23E9723AFCFC0378466CE06C6C6CF8AD676565852A1AB4D81F93F18DFD616386
        F385FD77A98A73D4744E6367D42D3C67790DEF1CC884C5A10B2C0076682D251E
        E74358D13460EC9FB2AD1B871D1CB878D7AE5D90356B48A8C0962D5B50592BC7
        A9E073707276A1BB241E76871CC164F385B746494EE888E65B3370BA34823FD8
        54E2DDC385B0F5CB84A573040BE02D62313B0F7800658ADBC6DE85854763EFDE
        7D38EE7502DB77ECC4E990F3A8A89661DDBA75247A8092AA7A989B9B23ABA004
        AB57AF86ACB5C728D48DCCA26F1E27F326F0D2FE06BC609D8D80843AB80BF3B1
        DDF57B16C0ABC46F989C072092F51B4BB96DFB0E3A0F92E91E962192B2DCB8E9
        0B88E6A4A1C2687CBE612376DAEEE5196EDC64810F3EFC182783CEC162F356EC
        B773442F05D03B3C8B30D13DAC706CC5535F65E340A818D5CA2E787F578A9D1E
        F12C0036FDBCFC3C80A2461D2F6393BA8F9756A31BE159A9FB46704E18856BE5
        35080E0D47D059212E44C6A2AD679867CA64917197E0E5771A176312A1EA1E44
        0FC913A59358EBDB8B255645F8DC438442693BE4EADBF08DAB3204B07C410085
        F57D5C68E81D2BE5CFCBC9339BCB4E4B12263232447BE22691AF98828570047F
        B495E01FFBF31195A34043AB0EAD7DE3F0FD418A1DEEFC2E5818C0D5DADE05BD
        3364F78BC23919170ECEA29B33C3A9D64C61DF0FA3F8F341199EB6B802F7EF6B
        51DBD20B55CF283AFAA7E047016C738D350D20577A93F78DC1B2336428D7DE27
        C98C313BBD502F6374CDD1796706EDFD33389676172B9C3AF08CE02AAC032B20
        51F440DE398C56DD7D74DC9E81CF0F12581F8B310D20ABAADBA49C493553D813
        370A617E2FD4FD93E81C98D64B49C6840C8D81811904178C63955B1F96EE28C3
        7B8ED79052DA8EC68E3B68EE9D441B05C7F08E95C0CA25DA34808CCAAEB972EA
        33CB964DE1CBF009BCE33F814F82871098A6419B6E021A0A424332869A326259
        B1354E3C8EB5FE0378615F1D9EB7CC827FD20DD4B6DE86B2671C2DBA29AAC034
        ADD3F0FABE0A9647A34C03482DD3E8CB4919B5DEA23EC60E907C1C6F9F1CC3DB
        7E63581B300CCF64355A7AEF51A9A7B998C1CA9E2B9BC417E707F0B2BD0A4F6D
        CEC19E1009A44D3ADCA047BCAAF73E9A49CC50119E31626CD51F440B87F05289
        5A5F4ECAAC9522768A96E315BB6ABCE93B82B77C47691DC53BBE43B08B54E27A
        632FD8B95122EF47764D3FB687EBB0C2B9931EB122AC7128A0A997215BAC4601
        0D7653DF343185A65E3DC7BFABC456A78BA60124D2AB95212BB6B650DF825254
        58ED20C2AB2EDD78DD7B08FFF41A2406E01CD701C5CD0934D3853DD287B1F2A8
        16CFDB4AB16C5B0ECEA435D16BDD1DC8BAC6A1A401566AA7A09887477405B638
        0A4D0388BFDE661C14362C2DD486A69E09248A3AB1C1AB8C0412BCE6A6C36B1E
        FD58E5AAC3D1A43E9C2DA0A03C7AB0EC800C4FD269F775680D242A7A37D0DC85
        BC6712721233643DF7F5DCBC0FB7A8726C76E00FA3852D88CD6FA6DE4F1B8785
        F5EC5A830EF9B55AB0EA58FA9763A9652E5EB66BC24A172D5EF9B607AF7B7461
        F9E10E3C4BA7DD1A87427AF99423BD52834CC94D644BB59C1B24653476DF4703
        E11A598EAF0E859A06109DA77A342CBC6F8F7AA7A46C1ABBC6E0F9A312ABF6E5
        63C93611963BB463B99306BFDF55853F5965E3746A33A42D43A8EF1C47230999
        8C611037744FA2AE7312C722CA60617FCE3480C85C85715894342CF37B672863
        63F73D08F3D478FD60219EF832134B778AE9419383DD676B206E1E448D7A8C02
        9824F124EA19B4673031A396381A5E8A2FED424C031066CA8DC322EFD1F72E8B
        4A9825EDA1923EE24A653782539558B93B8FBDD1E215DB5C9C4F5720B9A403A9
        159D443727A55C4FAD66123544B5E61EE75B6109BE3878C63480D0F4C605C332
        BF778FCA4865EDA26CBAEEE1DA8D41D85D6CC4C94BCD10B70CA3BA639C67C864
        F385D56A1213920E3DCE4211361E386D1AC0D99406DC58D033D6437D390D25E4
        2B09783959462495B48D41D23E06A97A828B18521219E0E2F6092EAFA2F54898
        081BF607B100D8FBE0E3867FD3CC822FD72128B916818935F04FA886DF8F527A
        744A7022B68A8E4F313CBF1383DDC3EED1E57C925D23CB386CA85C224AE142BD
        3D7A9151C2CBCC7026D911CA98498F8415E3C88562389D2FC6FAAF0359002FB1
        97516300ECDDEC3FFBCE80F1EFBDC1F86CCF6922089FEE266C4FE113DB407C62
        13808F6DFCF1D1AE93F86827B1C30F1F6EF7C587DB7CF0C1B61378DFDA9B636E
        E505734B4FACB53C8E7FFDD783F3DE5677BC2770C39A2DAE58237065012C217E
        B520005612E219E205E2E5B932B152FD3F5936275FC4923604F03F7582299BA0
        B223270000000049454E44AE426082}
      Index = 0
    end
    object dxLayoutImageItem3: TdxLayoutImageItem
      Parent = gbGeneralOptions
      Image.SourceDPI = 96
      Image.Data = {
        89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
        F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
        00206348524D00007A26000080840000FA00000080E8000075300000EA600000
        3A98000017709CBA513C0000076A494441545847B55709509357105E04727019
        6E927004B90F43B82A632BA62D622D681174145008C8A18210AE4610F9210A28
        F5603C6AADD654ABA50E8E78B5A3B54E9CD1EA580F6C6BED358AD52A9522A0B6
        D3A98ADB7D99C0540B0A2DCDCC2679C9FFF2EDB7DFB7FBBF981C3C78101E3F7E
        0C8F1E3D62A1E8EDEDBDF8F0E1C3BE35D05AF9E0C103C39A3EEFA1D78BC6F76C
        DDF7B98CDEB3606BF67DCFAE5DBB60488FE6E666686A6A829D3B778A743A1D6E
        D9B22561D3A64DB06EDD3A58B3660D343434606D6D2DD6D4D4606565256A341A
        7D494909141414C082050B203B3B5BA752A970CE9C39386BD62C4C4A4AC269D3
        A6C910118612B063C70ED8B66D1B6CDEBC59B571E3466C6C6CD4AD5EBD1A56AC
        58015AAD16AAAAAAB0A2A2425F5656066AB5BA252F2F0F73737395F3E6CD83B4
        B4B4B5C9C9C93873E64C5D424282282E2E4E161313A39E3871E290C05982C0D8
        AE5FBF9EB16D25B66D757575585D5D2D22B6406CA1B8B818172D5AA45FB870A1
        282B2BAB85B14D4D4D95115BD1F4E9D39140DB264D9A044AA512C68F1F0F9191
        91A0502840386DCF136133E310F816FF000AAED3CD3EE533AD60CA8E1892C814
        56AD5A05F5F5F50A628BC4565D5E5E8EC496C8AA213F3F1F882D66666622B1C5
        D9B3672381EAE2E3E321363696309548A06B2322220CA0818181E0EBEB0B63C6
        8C7902DC72FA3EF02EFC1622EBEE4B5DB3CE6C174E6DBECA9FB0228E12B002D2
        1688ED5A628B466D5B49DB5662CB4A0C46D056025512D3162328474C95212121
        181414C431504F4F4F707373038944024E4E4E4F2420CBFF1A5EA8FFDDDA33EF
        CB0D54993F05AF6EA8247019850D949696326DBB49DB3602E50854CF40131313
        15545EC61449533D2B2F63CA4089A99E4065048A04AA178BC5E0E8E808767676
        307AF468B0B6B6EE4FC07DFE0562FE1BF8167D5F679D74100593DF7B9B807D28
        9C2978CCC5AAF4F47456DE562AAF9E40F5642406AA8B8A8A62A02897CBF50101
        01E0EDEDAD94C964E8EAEADAE2E2E2C240F5048A04AA62A0161616228140C0F1
        F97C11F38034EB0C8469BB2168F1758D5DCAD15EE1EB1FEC33934D1E4FC0120A
        01850910680B8122818AA2A3A38181868585B58D1D3BB6DBDFDF1FBCBCBCD0C3
        C3A35B2A95EA9D9D9DD1C1C1016D6D6D95363636606565A520D06E02451E8FD7
        6A6E6EDE6D666686A6A6A609CEAA9310B2B4834CF76B8E4BC6892E61FC47E778
        E1453308D48DC2926294A10B264C98C08D1B374E151A1A0AC1C1C1E0E7E7C74C
        94E0EEEECE919E3202E5ECEDED399148C4114BCED2D25226140A815802810201
        8A08503D6AD4288E858989898C02822A7E86706DCF0CB7F9E7AF90E93AF9D12B
        D504EA61D09DDCCF2A6448803D3DDD32FF652D9A7518FCCBAE52E9BB5EF62ABC
        7C963A0005316FAF26504F0A3B0AF3BEDF1FF104AC930E80B7FA7B085F762FC8
        5F73ED13DBD94750F0DAFBCDA6CEE12104EC68301DE9FEBF2520CBFB0AC296DD
        75922FFD65BB93EA040AE33EFC821756184FA02E7DA6FB7B7547B402AE396709
        BC07426BBA1ADC72CF23E9FE2BE99E4FC052C3C021D33D2DED8825C01C2F5F7A
        1B42ABEF9478157ED76B95B81F0593DE596B74FC687A351BC8572392807DEA31
        08D05C67333E2540F353BB5DF251144CD97EC0D4FD15A6BB2D33DDDF751F5109
        6C667E0C3E457483A9EE8A96739DE75C324E21F5FB255ED492440275A0E00F06
        3E601B32170FB5052DDED80B6CC613B85708D7B9DF7DC145A4BD0F04AFACD318
        C7ACC540BA0F5A0171E6292AE53510CF3BCDDAA5BF55064B489A7D1642B83B3C
        7955C746EFA21F91AA412DA7DB4A7BC514D67DC3E65984FA3DE098761C42AA3A
        20B2F65E0A69794892713C831967B0449CD24FD0A46B87B195B73501E537D161
        EE71D6722779E1EA97680F33DDA0BA0F5801BFD2AB10B9FC6E6C64EDFDCB11B5
        BF21CDEF9B92B9478A9F1E1C6CB36DF2A7E0537205822B6F25072DB9DD2EC93E
        CB5AAE4BF072638E71D23D53F70113A0D30A3FA8FCC69E88DAFB18BEFC1ED224
        436AA96E49FA67ECBEDD3FBDA8BD48F74B40C0518115B7CE78E47F83564907A8
        F4DB1AE93A7B0AE1F3741FCC033C61FCEE1A71E6E79DA1DA6E0CD3F660288582
        BBF38744A5D7F695549A730E022BDA1D0316FFDCE45D7215ED528E51E9771D35
        97E7B29663C3C67093196A3C310798E682D87733AD13F79EF37FF31A92C1505E
        D589C14B3B1E8955FA7A9E34CA505A3FCDCDE5BE6537D039E3342B7D3BB93ED5
        68BA21E93E6817B01F674CF9CA351385AFEF6C76A69E0E2CBF85814B7EC180C5
        B7704CC1372B49FB1C9FB29FEEBACEBF88966CDABDA65B69341D4BEE1FA3F679
        95F8C724342661CA1B572EA5536B8365E2BE7B1E7997D0F7CD1BE8537A1DBD4A
        DB6E7B16FC88229A76C2F8A6C3BCB0023FDAC34E36C3067FE67980FD20331F31
        CC134EDD7DD92EF5187A16FE809E4557D031FD731A38CD1D748F4FA66BD8B019
        70CE3F8FFD730F24C66A9809266F8DA7242E5824B4A043DA49B44A3C8074B67B
        8BBE67C7AA61EB3EEC7B01AB0625F10219EE2BDA4CC6DB7D89FFE2B240FAFC5F
        E93EEC04D80696043FBA414C2DC7912C738DBA0FABE59E793B1EE84F24437DEA
        C1BAC494696E7C65EB411F43F963DADF0543BDF8FFB8EE2FE3B47D54FE8C760E
        0000000049454E44AE426082}
      Index = 0
    end
    object lbInternationalDictionaries: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'Label'
      CaptionOptions.WordWrap = True
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = gbInternationalDictionaries
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = gbCustomDictionary
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnEdit'
      CaptionOptions.Visible = False
      Control = btnEdit
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 93
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbEditCustomDictionary: TdxLayoutLabeledItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Label'
      CaptionOptions.WordWrap = True
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 352
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
