unit FormattedLabelDemoMain;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, BaseForm, Menus, StdCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, ComCtrls, ToolWin, cxMemo,
  cxTextEdit, cxRichEdit, dxFormattedLabel, cxSplitter, cxGroupBox, cxImageList,
  cxMaskEdit, cxDropDownEdit, cxFontNameComboBox, dxFormattedText, cxClasses,
  dxColorDialog, cxLabel, cxColorComboBox, cxCheckBox, cxRadioGroup, dxLayoutcxEditAdapters,
  dxLayoutContainer, dxLayoutControl, ActnList, dxLayoutLookAndFeels, ImgList;

type
  TdxFormattedLabelDemoForm = class(TfmBaseForm)
    flMain: TdxFormattedLabel;
    ToolBar1: TToolBar;
    tbBold: TToolButton;
    tbItalic: TToolButton;
    tbUnderline: TToolButton;
    ToolButton4: TToolButton;
    tbFont: TToolButton;
    tbFill: TToolButton;
    ToolButton8: TToolButton;
    tbHyperlink: TToolButton;
    cxImageList1: TcxImageList;
    tbStrikeOut: TToolButton;
    ActionList1: TActionList;
    acBold: TAction;
    acItalic: TAction;
    acUnderline: TAction;
    acStrikeout: TAction;
    acFont: TAction;
    acFontColor: TAction;
    acBackgroundColor: TAction;
    tbNoparse: TToolButton;
    ToolButton5: TToolButton;
    acHyperlink: TAction;
    acNoparse: TAction;
    dxColorDialog1: TdxColorDialog;
    tbFontColor: TToolButton;
    FontDialog1: TFontDialog;
    cbHyperlinkColor: TcxColorComboBox;
    reBBCode: TcxRichEdit;
    tbSup: TToolButton;
    tbSub: TToolButton;
    acSup: TAction;
    acSub: TAction;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    cbWordWrap: TdxLayoutCheckBoxItem;
    cbShowEndEllipsis: TdxLayoutCheckBoxItem;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lgAlignmentHorizontal: TdxLayoutGroup;
    lgAlignmentVertical: TdxLayoutGroup;
    lrbLeft: TdxLayoutRadioButtonItem;
    lrbRight: TdxLayoutRadioButtonItem;
    lrbCenterH: TdxLayoutRadioButtonItem;
    lrbTop: TdxLayoutRadioButtonItem;
    lrbBottom: TdxLayoutRadioButtonItem;
    lrbCenter: TdxLayoutRadioButtonItem;
    procedure reBBCodePropertiesChange(Sender: TObject);
    procedure acBoldExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure acUnderlineExecute(Sender: TObject);
    procedure acStrikeoutExecute(Sender: TObject);
    procedure acFontColorExecute(Sender: TObject);
    procedure acBackgroundColorExecute(Sender: TObject);
    procedure acHyperlinkExecute(Sender: TObject);
    procedure acNoparseExecute(Sender: TObject);
    procedure acSizeExecute(Sender: TObject);
    procedure acFontExecute(Sender: TObject);
    procedure acSupExecute(Sender: TObject);
    procedure acSubExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbHyperlinkColorPropertiesEditValueChanged(Sender: TObject);
    procedure cbWordWrapClick(Sender: TObject);
    procedure cbShowEndEllipsisClick(Sender: TObject);
    procedure lrbLeftClick(Sender: TObject);
    procedure lrbTopClick(Sender: TObject);
  end;

var
  dxFormattedLabelDemoForm: TdxFormattedLabelDemoForm;

implementation

{$R *.dfm}

uses
  dxFormattedTextConverterRTF, dxCoreGraphics, HyperlinkDialog;

procedure TdxFormattedLabelDemoForm.acBackgroundColorExecute(Sender: TObject);
begin
  if dxColorDialog1.Execute then
    reBBCode.SelText := Format('[BACKCOLOR=#%s]%s[/BACKCOLOR]', [TdxAlphaColors.ToHexCode(dxColorDialog1.Color, False),
      reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 12;
end;

procedure TdxFormattedLabelDemoForm.acBoldExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[B]%s[/B]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 4;
end;

procedure TdxFormattedLabelDemoForm.acFontColorExecute(Sender: TObject);
begin
  if dxColorDialog1.Execute then
  begin
    reBBCode.SelText := Format('[COLOR=#%s]%s[/COLOR]', [TdxAlphaColors.ToHexCode(dxColorDialog1.Color, False),
      reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 8;
  end;
end;

procedure TdxFormattedLabelDemoForm.acFontExecute(Sender: TObject);
var
  ASelectedText: string;
  ASelStartIndent: Integer;
begin
  inherited;
  if FontDialog1.Execute(reBBCode.Handle) then
  begin
    ASelStartIndent := 0;
    ASelectedText := reBBCode.SelText;
    if FontDialog1.Font.Name <> reBBCode.Style.Font.Name then
    begin
      ASelectedText := Format('[FONT=%s]%s[/FONT]', [FontDialog1.Font.Name,  ASelectedText]);
      ASelStartIndent := ASelStartIndent + 7;
    end;
    if FontDialog1.Font.Size <> reBBCode.Style.Font.Size then
    begin
      ASelectedText := Format('[SIZE=%s]%s[/SIZE]', [IntToStr(FontDialog1.Font.Size),  ASelectedText]);
      ASelStartIndent := ASelStartIndent + 7;
    end;
    if FontDialog1.Font.Color <> reBBCode.Style.Font.Color then
    begin
      ASelectedText := Format('[COLOR=#%.2x%.2x%.2x]%s[/COLOR]', [dxColorToRGBQuad(FontDialog1.Font.Color).rgbRed,
        dxColorToRGBQuad(FontDialog1.Font.Color).rgbGreen, dxColorToRGBQuad(FontDialog1.Font.Color).rgbBlue,  ASelectedText]);
      ASelStartIndent := ASelStartIndent + 8;
    end;
    if fsBold in FontDialog1.Font.Style then
    begin
      ASelectedText := Format('[B]%s[/B]', [ASelectedText]);
      ASelStartIndent := ASelStartIndent + 4;
    end;
    if fsItalic in FontDialog1.Font.Style then
    begin
      ASelectedText := Format('[I]%s[/I]', [ASelectedText]);
      ASelStartIndent := ASelStartIndent + 4;
    end;
    if fsUnderline in FontDialog1.Font.Style then
    begin
      ASelectedText := Format('[U]%s[/U]', [ASelectedText]);
      ASelStartIndent := ASelStartIndent + 4;
    end;
    if fsStrikeOut in FontDialog1.Font.Style then
    begin
      ASelectedText := Format('[S]%s[/S]', [ASelectedText]);
      ASelStartIndent := ASelStartIndent + 4;
    end;
    reBBCode.SelText := ASelectedText;
    reBBCode.SelStart := reBBCode.SelStart - ASelStartIndent;
  end;
end;

procedure TdxFormattedLabelDemoForm.acHyperlinkExecute(Sender: TObject);
var
  AHyperlinkDialog: TfmHyperlinkDialog;
begin
  AHyperlinkDialog := TfmHyperlinkDialog.Create(nil);
  try
    AHyperlinkDialog.edtTextToDisplay.Text := reBBCode.SelText;
    if AHyperlinkDialog.ShowModal = mrOk then
    begin
      reBBCode.SelText := Format('[URL=%s]%s[/URL]', [AHyperlinkDialog.edtAddress.Text, AHyperlinkDialog.edtTextToDisplay.Text]);
      reBBCode.SelStart := reBBCode.SelStart - 6;
    end;
  finally
    AHyperlinkDialog.Release;
  end;
end;

procedure TdxFormattedLabelDemoForm.acItalicExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[I]%s[/I]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 4;
end;

procedure TdxFormattedLabelDemoForm.acNoparseExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[NOPARSE]%s[/NOPARSE]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 10;
end;

procedure TdxFormattedLabelDemoForm.acSizeExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[SIZE=]%s[/SIZE]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 7;
end;

procedure TdxFormattedLabelDemoForm.acStrikeoutExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[S]%s[/S]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 4;
end;

procedure TdxFormattedLabelDemoForm.acSubExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[SUB]%s[/SUB]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 6;
end;

procedure TdxFormattedLabelDemoForm.acSupExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[SUP]%s[/SUP]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 6;
end;

procedure TdxFormattedLabelDemoForm.acUnderlineExecute(Sender: TObject);
begin
  inherited;
  reBBCode.SelText := Format('[U]%s[/U]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 4;
end;

procedure TdxFormattedLabelDemoForm.cbHyperlinkColorPropertiesEditValueChanged(Sender: TObject);
begin
  flMain.Properties.HyperlinkColor := cbHyperlinkColor.ColorValue;
end;

procedure TdxFormattedLabelDemoForm.cbShowEndEllipsisClick(Sender: TObject);
begin
  flMain.Properties.ShowEndEllipsis := cbShowEndEllipsis.Checked;
end;

procedure TdxFormattedLabelDemoForm.cbWordWrapClick(Sender: TObject);
begin
  flMain.Properties.WordWrap := cbWordWrap.Checked;
end;

procedure TdxFormattedLabelDemoForm.FormCreate(Sender: TObject);
begin
  inherited;
  reBBCode.Text := '[B]Questions?[/B]'#13#10'Visit our [URL=https://www.devexpress.com/support/]Support Center[/URL]';
end;

procedure TdxFormattedLabelDemoForm.lrbLeftClick(Sender: TObject);
begin
  flMain.Properties.Alignment.Horz := TcxEditHorzAlignment(TdxLayoutItem(Sender).Tag);
end;

procedure TdxFormattedLabelDemoForm.lrbTopClick(Sender: TObject);
begin
  flMain.Properties.Alignment.Vert  := TcxEditVertAlignment(TdxLayoutItem(Sender).Tag);
end;

procedure TdxFormattedLabelDemoForm.reBBCodePropertiesChange(Sender: TObject);
begin
  flMain.Caption := reBBCode.EditingText;
end;

end.
