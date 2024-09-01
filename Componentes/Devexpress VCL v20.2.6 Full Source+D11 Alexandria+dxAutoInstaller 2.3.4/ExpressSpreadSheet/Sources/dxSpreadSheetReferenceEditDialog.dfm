object dxSpreadSheetReferenceEditDialogForm: TdxSpreadSheetReferenceEditDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 39
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 386
    Height = 39
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = LayoutCxLookAndFeel
    object beAreaSelector: TcxButtonEdit
      Left = 10
      Top = 10
      Properties.Buttons = <
        item
          Glyph.SourceDPI = 96
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000004649444154785ECDD3310A00200C43510F93FB5F310A8EC58690A10A
            5FB7072DB84846DDEB3C6E15304E0750F53DA073018E0078CC0E0934088C110A
            826407307610FC85A01CD878555AB18AC525EA0000000049454E44AE426082}
          Kind = bkGlyph
        end>
      Properties.ReadOnly = True
      Properties.OnButtonClick = beAreaSelectorPropertiesButtonClick
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 366
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object liCompactView: TdxLayoutItem
      Parent = lcMainGroup_Root
      Visible = False
      Control = beAreaSelector
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 366
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object LayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 40
    Top = 8
    object LayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object EditRepository: TcxEditRepository
    Left = 8
    Top = 8
    PixelsPerInch = 96
    object ertiArea: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Glyph.SourceDPI = 96
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000004C49444154785EBD92610A001014831D66F7BFE24352B2A219567BF2
            E7DB5BA488B0DC463954EF01A3F8EE03AE57E0C48504002543A84000181B7800
            541F559852D1C19F5E81F4F02339F60119D3D864CBC6C560CA0000000049454E
            44AE426082}
          Kind = bkGlyph
        end>
      Properties.ValidationErrorIconAlignment = taRightJustify
      Properties.ValidationOptions = [evoRaiseException, evoShowErrorIcon]
      Properties.OnButtonClick = ertiAreaPropertiesButtonClick
      Properties.OnValidate = ertiAreaPropertiesValidate
    end
  end
end
