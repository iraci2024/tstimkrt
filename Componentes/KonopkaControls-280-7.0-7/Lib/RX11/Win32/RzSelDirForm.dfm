object RzSelDirForm: TRzSelDirForm
  Left = 247
  Top = 117
  Caption = 'Select a Directory'
  ClientHeight = 369
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000088888888888888008FB7
    B7B7B7B7B8008F7B7B7B7B7B78008FB7B7B7B7B7B8008F7B7B7B7B7B78008FB7
    B7B7B7B7B8008F7B7B7B7B7B78008FB7B7B7B7B7B8008FFFFFFFFFFFF80087B7
    B7B788888800087B7B780000000000888880000000000000000000000000FFFF
    5403FFFF00FF8001C0C0000100FF0001C0C00001808000010000000180800001
    FF0000018080000180800001FFFF0003C0C080FF00FFC1FFC0C0FFFF00FF}
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnlFolders: TRzPanel
    Left = 0
    Top = 45
    Width = 400
    Height = 242
    Align = alClient
    BorderOuter = fsNone
    BorderWidth = 4
    TabOrder = 0
    object LblFolders: TLabel
      Left = 4
      Top = 4
      Width = 392
      Height = 17
      Align = alTop
      AutoSize = False
      Caption = 'Folders'
      ExplicitWidth = 400
    end
  end
  object PnlPrompt: TRzPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 45
    Align = alTop
    BorderOuter = fsNone
    BorderWidth = 4
    TabOrder = 1
    object LblDir: TLabel
      Left = 4
      Top = 21
      Width = 392
      Height = 13
      Align = alTop
      AutoSize = False
      Caption = 'd:\raize\rzcomps\source'
      ParentShowHint = False
      ShowHint = True
      ExplicitWidth = 400
    end
    object LblPrompt: TLabel
      Left = 4
      Top = 4
      Width = 392
      Height = 17
      Align = alTop
      AutoSize = False
      Caption = 'Folder Name'
      ExplicitWidth = 400
    end
    object EdtDir: TRzEdit
      Left = 4
      Top = 34
      Width = 392
      Height = 7
      Text = 'EdtDir'
      Align = alClient
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnEnter = EdtDirEnter
      OnExit = EdtDirExit
      ExplicitHeight = 21
    end
  end
  object PnlDrives: TRzPanel
    Left = 0
    Top = 287
    Width = 400
    Height = 46
    Align = alBottom
    BorderOuter = fsNone
    BorderWidth = 4
    TabOrder = 2
    object LblDrives: TLabel
      Left = 4
      Top = 4
      Width = 392
      Height = 17
      Align = alTop
      AutoSize = False
      Caption = 'Drives'
      ExplicitWidth = 400
    end
  end
  object PnlButtons: TRzDialogButtons
    Left = 0
    Top = 333
    Width = 400
    CaptionHelp = 'Help'
    OnClickOk = PnlButtonsClickOk
    OnClickHelp = PnlButtonsClickHelp
    TabOrder = 3
  end
end
