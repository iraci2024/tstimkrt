object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Dynamic ribbon/toolbar selection'
  ClientHeight = 501
  ClientWidth = 829
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
  inline ribbonframe: Tribbonframe
    Left = 32
    Top = 21
    Width = 785
    Height = 164
    TabOrder = 0
    Visible = False
    ExplicitLeft = 32
    ExplicitTop = 21
    ExplicitWidth = 785
    ExplicitHeight = 164
    inherited AdvToolBarPager1: TAdvToolBarPager
      Width = 785
      ExplicitWidth = 785
      inherited AdvPage1: TAdvPage
        Width = 785
        ExplicitWidth = 785
        inherited AdvToolBar4: TAdvToolBar
          Width = 126
          ExplicitWidth = 126
          inherited AdvGlowButton3: TAdvGlowButton
            Width = 40
            Height = 63
            Action = EditCut1
            ImageIndex = 0
            Transparent = True
            Layout = blGlyphTop
            ExplicitWidth = 40
            ExplicitHeight = 63
          end
          inherited AdvGlowButton4: TAdvGlowButton
            Left = 42
            Width = 40
            Height = 63
            Action = EditCopy1
            ImageIndex = 1
            Transparent = True
            Layout = blGlyphTop
            ExplicitLeft = 42
            ExplicitWidth = 40
            ExplicitHeight = 63
          end
          inherited AdvGlowButton5: TAdvGlowButton
            Left = 82
            Width = 40
            Height = 63
            Action = EditPaste1
            ImageIndex = 2
            Transparent = True
            Layout = blGlyphTop
            ExplicitLeft = 82
            ExplicitWidth = 40
            ExplicitHeight = 63
          end
        end
        inherited AdvToolBar3: TAdvToolBar
          Left = 132
          Width = 62
          ExplicitLeft = 132
          ExplicitWidth = 62
          inherited AdvToolBarButton1: TAdvToolBarButton
            Width = 47
            Action = Action2
            GlyphPosition = gpTop
            ImageIndex = 4
            ShowCaption = True
            ExplicitWidth = 47
          end
        end
      end
    end
  end
  inline toolbarframe: TToolBarFrame
    Left = 32
    Top = 191
    Width = 785
    Height = 76
    TabOrder = 1
    Visible = False
    ExplicitLeft = 32
    ExplicitTop = 191
    ExplicitWidth = 785
    ExplicitHeight = 76
    inherited AdvDockPanel1: TAdvDockPanel
      Width = 785
      Height = 43
      ExplicitWidth = 785
      ExplicitHeight = 43
      inherited AdvToolBar1: TAdvToolBar
        Width = 95
        Height = 28
        ExplicitWidth = 95
        ExplicitHeight = 28
        inherited AdvToolBarButton1: TAdvToolBarButton
          Width = 24
          Height = 24
          Action = EditCut1
          ImageIndex = 0
          ExplicitWidth = 24
          ExplicitHeight = 24
        end
        inherited AdvToolBarButton2: TAdvToolBarButton
          Left = 33
          Width = 24
          Height = 24
          Action = EditCopy1
          ImageIndex = 1
          ExplicitLeft = 33
          ExplicitWidth = 24
          ExplicitHeight = 24
        end
        inherited AdvToolBarButton3: TAdvToolBarButton
          Left = 57
          Width = 24
          Height = 24
          Action = EditPaste1
          ImageIndex = 2
          ExplicitLeft = 57
          ExplicitWidth = 24
          ExplicitHeight = 24
        end
      end
      inherited AdvToolBar2: TAdvToolBar
        Width = 89
        Height = 28
        ExplicitWidth = 89
        ExplicitHeight = 28
        inherited AdvToolBarButton4: TAdvToolBarButton
          Width = 66
          Height = 24
          Action = Action1
          ImageIndex = 3
          ExplicitWidth = 66
          ExplicitHeight = 24
        end
      end
    end
  end
  object Memo1: TMemo
    Left = 32
    Top = 288
    Width = 282
    Height = 177
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object AdvOfficeStatusBar1: TAdvOfficeStatusBar
    Left = 0
    Top = 482
    Width = 829
    Height = 19
    AnchorHint = False
    Panels = <
      item
        AppearanceStyle = psLight
        DateFormat = 'd/MM/yyyy'
        Progress.BackGround = clNone
        Progress.Indication = piPercentage
        Progress.Min = 0
        Progress.Max = 100
        Progress.Position = 0
        Progress.Level0Color = clLime
        Progress.Level0ColorTo = 14811105
        Progress.Level1Color = clYellow
        Progress.Level1ColorTo = 13303807
        Progress.Level2Color = 5483007
        Progress.Level2ColorTo = 11064319
        Progress.Level3Color = clRed
        Progress.Level3ColorTo = 13290239
        Progress.Level1Perc = 70
        Progress.Level2Perc = 90
        Progress.BorderColor = clBlack
        Progress.ShowBorder = False
        Progress.Stacked = False
        TimeFormat = 'h:mm:ss'
        Width = 80
      end
      item
        AppearanceStyle = psLight
        DateFormat = 'd/MM/yyyy'
        Progress.BackGround = clNone
        Progress.Indication = piPercentage
        Progress.Min = 0
        Progress.Max = 100
        Progress.Position = 0
        Progress.Level0Color = clLime
        Progress.Level0ColorTo = 14811105
        Progress.Level1Color = clYellow
        Progress.Level1ColorTo = 13303807
        Progress.Level2Color = 5483007
        Progress.Level2ColorTo = 11064319
        Progress.Level3Color = clRed
        Progress.Level3ColorTo = 13290239
        Progress.Level1Perc = 70
        Progress.Level2Perc = 90
        Progress.BorderColor = clBlack
        Progress.ShowBorder = False
        Progress.Stacked = False
        TimeFormat = 'h:mm:ss'
        Width = 100
      end
      item
        AppearanceStyle = psLight
        DateFormat = 'd/MM/yyyy'
        Progress.BackGround = clNone
        Progress.Indication = piPercentage
        Progress.Min = 0
        Progress.Max = 100
        Progress.Position = 0
        Progress.Level0Color = clLime
        Progress.Level0ColorTo = 14811105
        Progress.Level1Color = clYellow
        Progress.Level1ColorTo = 13303807
        Progress.Level2Color = 5483007
        Progress.Level2ColorTo = 11064319
        Progress.Level3Color = clRed
        Progress.Level3ColorTo = 13290239
        Progress.Level1Perc = 70
        Progress.Level2Perc = 90
        Progress.BorderColor = clBlack
        Progress.ShowBorder = False
        Progress.Stacked = False
        TimeFormat = 'h:mm:ss'
        Width = 130
      end
      item
        AppearanceStyle = psLight
        DateFormat = 'd/MM/yyyy'
        Progress.BackGround = clNone
        Progress.Indication = piPercentage
        Progress.Min = 0
        Progress.Max = 100
        Progress.Position = 0
        Progress.Level0Color = clLime
        Progress.Level0ColorTo = 14811105
        Progress.Level1Color = clYellow
        Progress.Level1ColorTo = 13303807
        Progress.Level2Color = 5483007
        Progress.Level2ColorTo = 11064319
        Progress.Level3Color = clRed
        Progress.Level3ColorTo = 13290239
        Progress.Level1Perc = 70
        Progress.Level2Perc = 90
        Progress.BorderColor = clBlack
        Progress.ShowBorder = False
        Progress.Stacked = False
        TimeFormat = 'h:mm:ss'
        Width = 50
      end>
    SimplePanel = False
    URLColor = clBlue
    Styler = AdvOfficeStatusBarOfficeStyler1
    Version = '1.4.0.0'
    ExplicitTop = 674
    ExplicitWidth = 1002
  end
  object ImageList1: TImageList
    Left = 400
    Top = 296
    Bitmap = {
      494C010105000800380010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000D1AB9900A1400000B0643F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CEA6
      9400A5420000F4950000DB740100A7461B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CBA08C00A641
      0000EA8B0000E98B0000F49A0700DE7C0700B352220000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CEA69400A5420000E98A
      0000EA8A0100E88C0500E88F0A00F6A71200E1861100B8521C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D1AA9A00A53C0000EA8C0000E98A
      0000E78A0400E8910A00EB961000EC9A1300F8B22200E6921900C16023000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AA511000F9A81500E57F0000E78B
      0400E8900800EA960F00ED9B1400EF9E1B00F0A62000FBBD2B00EA9B2000C663
      1D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B7643700EBAB3500F4A81800E587
      0000EA950F00EC9B1400EFA01B00F1A62000F2AA2500F1AF2900FCC53800EEA6
      2B00D07125000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B2502200EAB04400F6B1
      2500EA920700EEA01D00F1A41F00F2AA2500F3AE2B00F4B33000F5B83900FFCC
      4700EEAA3000D5741B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B7501B00EDB4
      5100F9B93400EF9C1200F2AB2600F2AE2A00F4B33000F6B63400F8BA3600FBC2
      3D00FFD45800F6C45600DF8D3200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C15E
      2200F0BF5D00F9BE3E00F1A41300F3B12900F4B53100FAC14200FBD16200F7CF
      7500FFEAB500FFFEC800F7CB6C00F5D5B0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CA6C2300FAE2A400FFDF8400F8CC6300FDD98600FEE08E00DA842500EEC4
      A200E59B4900FEEDBF00FFE89B00F3C88B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D2782500FAE2A800FFE79B00FEDA7F00F4C66800F0CEAF000000
      0000FEF7F100F3C68400FFF9BA00F3BA65000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D77D1F00F9E2AA00FFEBA800FBD57A00E7A34D00F9E8
      D500EFBB7700FAD68A00FFF9C400F3B54C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DF8B2A00FBE4AF00FFF8C000FEDE8800F3BD
      5D00FAD37900FFE79B00FFF6C400F4BB51000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E4952F00F8DB9E00FFEFB900FFFD
      CD00FFFCD500FFFAD600FFEDAF00F6BF59000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F6D6AC00F4CD8E00F5C0
      7200F5BB5D00F7C36400F7C36200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F4F5F500B2AEAD009A8B8400C1B2AE00FEFDFC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B4A0C0099753D00936E3600936E3600936E3600956E
      3700936E3700936E37009B783F006533000000000000328BBE003E8CC7003E8E
      C7003E8EC7003C8ECB003F9BDE0072543500BC976200B3997100B3986E00B498
      6E00B89C7300B89E7600BDA67E00936B35000000000000000000000000000000
      00009ABAE8000064D3004686D800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE00998F8B00A85D2800D48A4000C87B3200CD9F8200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C3AF8F00000000000000000000000000000000000000
      0000000000000000000000000000A989530049AFFF0049BBFF003FB8FF0041B8
      FF0040B6FF0041BCFF002BC1FF00737765000000000000000000000000000000
      0000000000000000000000000000C1AA8B0000000000000000000000000095B5
      E6000068D70000ADFF00008BF400276BCE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ECECED008F603D00D7914100F4E5CF00C4AA8800BA6E2500F4EBE4000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C3AF8F0000000000EFEBE100F1ECE400F1ECE800F1ED
      E200EEEADD00EDE8DF00000000009A753D0050AAFF00349FFF002CA0FF002FA0
      FF002D9DFF0031A4FF000CA2FF00696F6900FFEBD900F2F2ED00F1EEE700F1EE
      E700EFEDE300ECE9E10000000000BBA17B0000000000000000008DB4E6000072
      D80000B9FF0000ACFF0000ABFF00008BF2002E6FCF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009B6A3C00D59A670000000000BEBDBB00B36E2900EDD9C9000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6B2900000000000F2EFE700F3F0EA00F3F0EA00F3F0
      EA00F7F1ED00F3F1EA00000000009A753D004FAFFF003AAAFF0035A9FF0037A9
      FF0039A9FF003BADFF001CACFF006F726900FFEEDC00F4F4F100F3F1E900F4F1
      EB00F6F2ED00F8F1ED0000000000BBA580000000000095BCEA00007DDB0000CB
      FF0000C0FF0000B5FF0000ADFF0000B2FF00008DF200286BCE00000000000000
      000000000000000000000000000000000000FDFEFE00BBB9B800917D72009E8C
      7E00CFC9C400B29C8800CA7F3900AFA3A0007F675A00C67F3200F8F0E9000000
      0000000000000000000000000000000000008F672B00B6987000B0926700B092
      6700B0936900A6825200C1AC8A0000000000F2EFE700F3F0EA00F3F2EB00F6F2
      EC00FAF3F100F9F2ED00000000009A753D0050B1FF003EB1FF0039AAFF003BAC
      FF003BABFF003DB1FF0022B1FF006F766700FFEDD900F3F4ED00F3EFEA00F6F1
      EC00FBF7F400FBF4F10000000000BAA0780098C4EE000086DE0000DFFF0000D2
      FF0000C5FF0000C0FF0000B9FF0000B3FF0000B9FF000096F3002E73D2000000
      000000000000000000000000000000000000C7C5C50091583400D6822E00DE85
      3000D1833300B9977100BA8A5100C8803500D08C4600EDBF8800000000000000
      000000000000000000000000000000000000CBB8990000000000000000000000
      000000000000FFFEFF00BAA27B0000000000F2EEE600F2EDE800F6EFEC00F9F8
      F100FBFBF600F5EFE900000000009C77420052B1FF0043B5FF003CADFF003EAF
      FF003EAEFF0043B5FF0026B5FF00727D7000FFF5E400F8F9F500FBF7F300FBFB
      F700FBFCF800F2EEE900F2EFE900B2976D002E92E00037FFFF0000E2FF0000D6
      FF0000D1FF0000CBFF0000C6FF0000BFFF0000B8FF0000BEFF00009BF3002875
      D300000000000000000000000000000000009A8C8600D5863D00E5D0B300FCFB
      F900BDA59600CB863B00AF8E6200B7966900E3BE8A00D8C8B300E8E7E500F2F2
      F10000000000000000000000000000000000C4AF8E0000000000EFEAE300F1EC
      E700F2F0E900EDEADD00BFA9840000000000F6F3EA00FAF7F300F9F8F800F9FB
      FA00F7F5F200EBE3D900F6F6F4009A733A0057B3FF0048B9FF0040B3FF0043B4
      FF0041B0FF0044B7FF0029B7FF007B857900FFF8EE00FBFCFF00F9FCFB00FBFB
      FB00F5F1EC00E9E0D600E1D8C900A689580041A2E60070E0F7002FFDFF0000E2
      FF0000DCFF0000D7FF0000D0FF0000CBFF0000C4FF0000BDFF0000C4FF0000A1
      F4002E7CD600000000000000000000000000AEA39E00D6853900CDBDA700EEEE
      EE00958C8C00C47F3B00D6AD7300A2A8AD009D9D9A00ADA9A300ADA79C00A59E
      94009C989000AFACA700EDECEB0000000000C4AF8C0000000000F2EDE700F3F0
      EA00F3F2EC00F3EDE700C2AD8F0000000000F8FAF500F9F9FB00FBFBFD00F8F7
      F100EEEDE600E4DBD300E8DFD400926C2F0058B5FF004EBFFF0043B5FF0048B8
      FF0046B7FF0047BBFF002CBAFF0081898000FFF8ED00FBFDFF00FCFBF900F7F4
      ED00D9CEBA00CEBFA100BCA27C009E794400000000002E95E40072E0F80032FF
      FF0000E8FE0000E2FE0000DBFF0000D5FF0000CFFF0000CAFF0000C2FF0000C9
      FF0000A3F300287BD7000000000000000000F1EFEF00C28B6A00BB7225009E64
      2F00B36E2D00E0A66400D0B89B00A09E9900C6BDAF00D3CABF00F7EDE100FFFB
      EC00FFFBEE00F7F1E400C3BCB300EDEBE900C4AF8C0000000000F2EDE700F3F0
      E800F7F3ED00F6F1EB00C7B2920000000000F8F8F800F9FBFB00F9FAF300F1EE
      E300DED2C300C7B09500C9B493008C6324005AB8FF0053C2FF0046B9FF0049B9
      FF0049B6FF004ABBFF002BBAFF00818E8800FFF8EF00FBFEFF00F4F2EB00EAE1
      D800CAB59700FBFBF900F3EEEA00B0966A0000000000000000002899E40074DE
      F8002CFFFF0000EFFD0000EAFE0000E1FF0000DAFF0000D3FF0000CFFF0000CD
      FF0000D2FF000EB6F600378CDE00FEFFFF0000000000FBF8F700E8CBB600E5C6
      AB00EDD7BD00F8EEE3000000000096928C00F8EFDF00C8C1B400D9D3C900D7D0
      C500E4DCCF00F5E9DC00FEFCEE00CDC7BE00C2AD8A0000000000F3EBE600F6F0
      EB00F9F4F300FAF9F100C8B5990000000000F8FAFB00F9FAF600F2ECE200E4D9
      CA00D0BFA700ECE3DC00B5966B00815309005DB9FF0058C4FF004ABBFF0050CF
      FF0054D8FF0051D9FF003AD5FF009F9D8E00FFFFFC0000000000FDE6D000EACA
      AA00C1935900F4EFEB00AA8D5D000000000000000000000000000000000031A0
      E7007AE3F8002EFFFF0000F1FD0000EEFE0000E6FE0000E6FF0014E0FF0044CB
      FA0063E2FF004CF6FF0025BAF400ABD0F2000000000000000000000000000000
      000000000000000000000000000095918A00F8EEDF00D8D0C40000000000FEFE
      FD00FCFBFB00EAE7E100E0DAD300E0DAD100CBBBA00000000000F7F7EE00FBF9
      F900FBFBFD00F6F6F200C9B99D0000000000FAFAF700F2EDE500E3D9D000D1BD
      A200C5AE9000EAE6E100B89E7300000000005EBAFF005ECAFF0054CEFF003996
      BE00145876001E668D0002639D004E5C5A0090857000827E6A00869D9A00839B
      8D006C7D7000C395580000000000000000000000000000000000000000000000
      00003EA9EB0092EBFE0041FFFF0017FBFF0035FDFF003CF8FF002799E400A1CB
      F10052A2E6006FE8FD002DDFFF0081BEEE000000000000000000000000000000
      00000000000000000000000000009C979100F8EEE100E3D9CD00F6F4F1000000
      000000000000000000000000000000000000CEBEA30000000000F8F8F700F9F9
      FF00FCFDFC00EFEBDF00C9B7980000000000F9F8F600EAE0D500D6CBB500C7AF
      8B009B783A009F7F4C0000000000000000005DB8FE005AD4FF0044C1FB008171
      6E0091796D00887A74008D7B72007A7E8300787E87005D595D000074CB0025BE
      FF0016A4FF00BBDFFF0000000000000000000000000000000000000000000000
      00000000000045AFEF0093EEFE004FFFFF001CFFFF0036DEFA00AFD7F6000000
      0000EDF6FD0056CAF5003CF5FF0052B1EC000000000000000000000000000000
      0000000000000000000000000000ADAAA500F2E9DB00F5ECDC00E5E1DB000000
      000000000000000000000000000000000000D2C3A80000000000F8F8FA00FAFA
      F700FBFAF300C9B49900C5B09000C0A58400C0A58400C0A58400B79B7000A789
      54008A62200000000000000000000000000079C1FF0077D3FF004DB8F400CFC9
      C600FFFFFC00EFEFEE00FFFEFD00F1EFEF00FFFFFE00D7CDC4001166AC003AAC
      FF002799FB00D2E9FD0000000000000000000000000000000000000000000000
      0000000000000000000045B2EF0093EEFE004BFFFF0026F2FD0060B7F100D5EB
      FB0090C7F30041DFF90038FFFF0032ADEC000000000000000000000000000000
      0000000000000000000000000000C9C7C300E0D9CC00FFF9E900E2DDD4000000
      000000000000000000000000000000000000D1BFA60000000000F9FBF900F2EE
      EA00EBE3DA00CAB59800F1EAE500B6996C009C773B0000000000000000000000
      00000000000000000000000000000000000000000000C5E5FF00D1EAFD00BFC2
      C100FFFCF60000000000B3B1B100FCFEFE0000000000C5C8C500C1DAF000BADE
      FF00D9EDFE000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004BB5EF0092F0FE004FFFFF002BF2FD0029D4
      F80029E4FB0025FFFF002EFFFF0028B7EE000000000000000000000000000000
      0000000000000000000000000000FBFBFB00B7B3AA00FCF9EB00F0EAE1000000
      000000000000000000000000000000000000D4C9B30000000000FCFAF500EDE6
      DC00DCD0C100C0A78000A98C5900926929000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCFBFA00E1E2E200ABABAB00E4E2E2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004CB7F10080E2FA005CFBFF0058FF
      FF0050FFFF0045FFFF0042F8FF0055BBF2000000000000000000000000000000
      000000000000000000000000000000000000F3F3F200E0DCD600E3DDD5000000
      000000000000000000000000000000000000C6B29300EAE4D900D6C6B300C9BA
      9C00BCA37C00A5885200926C2B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F9F9F900A6A6A600DBDBDB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B1DFF80093D4F60071C8
      F40058C2F3004DC5F40063C1F30000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00F1FF000000000000E0FF000000000000
      C07F000000000000803F000000000000001F000000000000000F000000000000
      00070000000000008003000000000000C001000000000000E000000000000000
      F000000000000000F810000000000000FC00000000000000FE00000000000000
      FF00000000000000FF81000000000000F83FFC008000F1FFF03FFDFE00FEE0FF
      F01FFD020002C07FF91FFD020002803F001F01020002001F003F79020000000F
      000F4100000000070001410000008003000041000000C000820041000041E000
      FE2041010003F000FE1F41030003F810FE1F40070003FC00FE1F407F8487FE00
      FE1F40FFF0FFFF00FF1F01FFF8FFFF8100000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 344
    Top = 296
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object Action1: TAction
      Category = 'Edit'
      Caption = '&Ribbon'
      Hint = 'Switch to ribbon mode'
      ImageIndex = 3
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Category = 'Edit'
      Caption = '&Toolbar'
      Hint = 'Switch to toolbar mode'
      ImageIndex = 4
      OnExecute = Action2Execute
    end
  end
  object AdvOfficeStatusBarOfficeStyler1: TAdvOfficeStatusBarOfficeStyler
    Style = psOffice2010Blue
    BorderColor = 10981241
    PanelAppearanceLight.BorderColor = clNone
    PanelAppearanceLight.BorderColorHot = 5819121
    PanelAppearanceLight.BorderColorDown = 3181250
    PanelAppearanceLight.Color = 16643823
    PanelAppearanceLight.ColorTo = 15784647
    PanelAppearanceLight.ColorHot = 14285309
    PanelAppearanceLight.ColorHotTo = 9102333
    PanelAppearanceLight.ColorDown = 8122111
    PanelAppearanceLight.ColorDownTo = 7131391
    PanelAppearanceLight.ColorMirror = clNone
    PanelAppearanceLight.ColorMirrorTo = clNone
    PanelAppearanceLight.ColorMirrorHot = 9102333
    PanelAppearanceLight.ColorMirrorHotTo = 14285309
    PanelAppearanceLight.ColorMirrorDown = 7131391
    PanelAppearanceLight.ColorMirrorDownTo = 8122111
    PanelAppearanceLight.TextColor = 7551263
    PanelAppearanceLight.TextColorHot = 7551263
    PanelAppearanceLight.TextColorDown = 7551263
    PanelAppearanceLight.TextStyle = []
    PanelAppearanceDark.BorderColor = clNone
    PanelAppearanceDark.BorderColorHot = 5819121
    PanelAppearanceDark.BorderColorDown = 3181250
    PanelAppearanceDark.Color = 16181468
    PanelAppearanceDark.ColorTo = 10981241
    PanelAppearanceDark.ColorHot = 14285309
    PanelAppearanceDark.ColorHotTo = 9102333
    PanelAppearanceDark.ColorDown = 8122111
    PanelAppearanceDark.ColorDownTo = 7131391
    PanelAppearanceDark.ColorMirror = clNone
    PanelAppearanceDark.ColorMirrorTo = clNone
    PanelAppearanceDark.ColorMirrorHot = 9102333
    PanelAppearanceDark.ColorMirrorHotTo = 14285309
    PanelAppearanceDark.ColorMirrorDown = 7131391
    PanelAppearanceDark.ColorMirrorDownTo = 8122111
    PanelAppearanceDark.TextColor = 7551263
    PanelAppearanceDark.TextColorHot = 7551263
    PanelAppearanceDark.TextColorDown = 7551263
    PanelAppearanceDark.TextStyle = []
    Left = 456
    Top = 296
  end
end
