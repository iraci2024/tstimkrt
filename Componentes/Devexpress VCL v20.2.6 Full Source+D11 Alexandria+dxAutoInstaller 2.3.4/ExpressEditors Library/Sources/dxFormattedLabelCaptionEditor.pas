{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEditors }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxFormattedLabelCaptionEditor; // for internal use

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Graphics, Controls, Forms, ActnList, Menus, Classes,
  Dialogs, cxGraphics, ComCtrls, StdCtrls,
  ToolWin, ImgList,
  dxCore, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses,
  cxContainer, dxForms,
  dxLayoutControl, dxLayoutContainer, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, dxFormattedLabel, dxFormattedText,
  dxColorDialog,
  cxButtons, cxImageList;

type

  { TdxHyperlinkEditorHelper }

  TdxHyperlinkEditorHelper = class
  strict private
    FFormattedText: TdxFormattedText;
    FSelectedRunsStartIndex: Integer;
    FSelectedRunsFinishIndex: Integer;
    FSelectedTextStart: PChar;
    FSelectedTextFinish: PChar;

    function ClosesHyperlink(ARun: TdxFormattedTextRun): Boolean;
    procedure DeletePairRuns(ARunIndex: Integer);
    procedure DeleteRuns(ARunIndexes: array of Integer);
    function GetCloseRunIndex(AOpenRunIndex: Integer): Integer;
    procedure MoveRun(ARunIndex, ANewIndex: Integer);
    function OpensHyperlink(ARun: TdxFormattedTextRun): Boolean;
    function RichCharPositionToBBCodeChar(AIndex: Integer;
      out ARunIndex: Integer): PChar;
  protected
    procedure ClearHyperlinks;
    function IsHyperlink(ASelStart, ASelLength: Integer): Boolean;
    function DeleteHyperlink: string;
    function EditHyperlink(const AText, AUrl: string): string; overload;
    function EditHyperlink(const AUrl: string): string; overload;
    function GetSelectedText(ASelStart, ASelLength: Integer): string;
    function GetSelectedHyperlinkText(ASelStart: Integer;
      out AUrl: string): string;
    function InsertHyperlink(const AUrl: string): string; overload;
    function InsertHyperlink(const AText, AUrl: string): string; overload;

    procedure RemoveEmptyUrlRuns;
    procedure RemoveAdditionalSymbolFromUrls;
    procedure InsertAdditionalSymbolFromUrls;
  public
    constructor Create(AFormattedText: TdxFormattedText); virtual;
  end;

  { TfrmFormattedLabelCaptionEditor }

  TfrmFormattedLabelCaptionEditor = class(TdxForm)
    cxImageList1: TcxImageList;
    ActionList1: TActionList;
    acBold: TAction;
    acItalic: TAction;
    acUnderline: TAction;
    acStrikeout: TAction;
    acFont: TAction;
    acFontColor: TAction;
    acBackgroundColor: TAction;
    acHyperlink: TAction;
    acNoparse: TAction;
    acSup: TAction;
    acSub: TAction;
    cdColor: TdxColorDialog;
    FontDialog1: TFontDialog;
    dxLayoutControl1: TdxLayoutControl;
    ToolBar1: TToolBar;
    tbBold: TToolButton;
    tbItalic: TToolButton;
    tbUnderline: TToolButton;
    tbStrikeOut: TToolButton;
    tbSup: TToolButton;
    tbSub: TToolButton;
    ToolButton4: TToolButton;
    tbFont: TToolButton;
    tbFontColor: TToolButton;
    tbFill: TToolButton;
    ToolButton8: TToolButton;
    tbHyperlink: TToolButton;
    ToolButton5: TToolButton;
    tbNoparse: TToolButton;
    reBBCode: TcxRichEdit;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    lgMain: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    lgTabGroup: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    lgEditor: TdxLayoutGroup;
    btnApply: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnCancel: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    btnOk: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    reRtf: TcxRichEdit;
    dxLayoutItem6: TdxLayoutItem;
    PopupMenu1: TPopupMenu;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    N1: TMenuItem;
    miHyperlink: TMenuItem;
    miEditHyperlink: TMenuItem;
    miRemoveHyperlink: TMenuItem;
    acRemoveHyperlink: TAction;
    acCut: TAction;
    acCopy: TAction;
    acPaste: TAction;
    acEditHyperlink: TAction;
    procedure reRtfPropertiesSelectionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lgTabGroupTabChanged(Sender: TObject);
    procedure acBoldExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure acUnderlineExecute(Sender: TObject);
    procedure acStrikeoutExecute(Sender: TObject);
    procedure acFontExecute(Sender: TObject);
    procedure acFontColorExecute(Sender: TObject);
    procedure acBackgroundColorExecute(Sender: TObject);
    procedure acHyperlinkExecute(Sender: TObject);
    procedure acNoparseExecute(Sender: TObject);
    procedure acSupExecute(Sender: TObject);
    procedure acSubExecute(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure acRemoveHyperlinkExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure reRtfMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure reBBCodeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    FFormattedText: TdxFormattedText;
    FHyperlinkEditorHelper: TdxHyperlinkEditorHelper;

    procedure SetText(const Value: string);
    procedure SettingStateButtons(ARichEdit: TcxRichEdit);
    procedure SettingPopupMenu;
  protected
    FFormattedLabel: TdxFormattedLabel;
    FFont: TFont;

    procedure Apply;
    function GetText: string;

    property Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function dxShowFormattedLabelCaptionEditor(AFormattedLabel
  : TdxFormattedLabel): Boolean;
procedure dxShowFormattedTextEditor(var AText: string; AFont: TFont);

implementation

{$R *.dfm}

uses
  cxRichEditUtils, dxFontHelpers, dxCoreGraphics, dxHyperlinkDialog,
  dxFormattedTextConverterBBCode, dxFormattedTextConverterRTF;

function dxShowFormattedLabelCaptionEditor(AFormattedLabel
  : TdxFormattedLabel): Boolean;
var
  AForm: TfrmFormattedLabelCaptionEditor;
  AOwnerCaption: string;
begin
  AForm := TfrmFormattedLabelCaptionEditor.Create(nil);
  try
    AForm.FFormattedLabel := AFormattedLabel;
    AForm.FFont.Assign(AFormattedLabel.Style.Font);
    AForm.FFont.Color := AFormattedLabel.Style.TextColor;
    AForm.FFont.Style := AFormattedLabel.Style.TextStyle;
    if AFormattedLabel.Owner <> nil then
      AOwnerCaption := AFormattedLabel.Owner.Name + '.'
    else
      AOwnerCaption := '';
    AForm.Caption := Format('%s%s - Editor',
      [AOwnerCaption, AFormattedLabel.Name]);
    AForm.Text := AFormattedLabel.Caption;
    Result := AForm.ShowModal = mrOk;
    if Result then
      AForm.Apply;
  finally
    AForm.Release;
  end;
end;

procedure dxShowFormattedTextEditor(var AText: string; AFont: TFont);
var
  AForm: TfrmFormattedLabelCaptionEditor;
begin
  AForm := TfrmFormattedLabelCaptionEditor.Create(nil);
  try
    AForm.btnApply.Visible := False;
    AForm.dxLayoutItem1.Visible := False;
    AForm.tbHyperlink.Visible := False;
    AForm.ToolButton8.Visible := False;
    AForm.Caption := 'Formatted Text Editor';
    AForm.FFormattedLabel := nil;
    AForm.FFont.Assign(AFont);
    AForm.Text := AText;
    if AForm.ShowModal = mrOk then
      AText := AForm.Text;
  finally
    AForm.Release;
  end;
end;

{ TdxHyperlinkEditorHelper }

constructor TdxHyperlinkEditorHelper.Create(AFormattedText: TdxFormattedText);
begin
  inherited Create;
  FFormattedText := AFormattedText;
end;

procedure TdxHyperlinkEditorHelper.ClearHyperlinks;
var
  ARunIndex: Integer;
begin
  ARunIndex := 0;
  while ARunIndex < FFormattedText.Runs.Count do
  begin
    if (FFormattedText.Runs[ARunIndex] is TdxFormattedTextURLRun) and
      (FFormattedText.Runs[ARunIndex].Action = traOpen) and
      (FFormattedText.Runs[ARunIndex].TextLength = 0) and
      not((FFormattedText.Runs[ARunIndex + 1].Action = traClose) and
      (FFormattedText.Runs[ARunIndex + 1] is TdxFormattedTextURLRun)) then
    begin
      if ((FFormattedText.Runs[ARunIndex + 1] is TdxFormattedTextUnderlineRun)
        and (FFormattedText.Runs[ARunIndex + 1].Action = traOpen) or
        (FFormattedText.Runs[ARunIndex + 1] is TdxFormattedTextColorRun) and
        (TdxFormattedTextColorRun(FFormattedText.Runs[ARunIndex + 1])
        .Color = clBlue) and (FFormattedText.Runs[ARunIndex + 1]
        .Action = traOpen)) then
        DeletePairRuns(ARunIndex + 1)
      else
      begin
        if FFormattedText.Runs[ARunIndex + 1].Action = traClose then
          MoveRun(ARunIndex, ARunIndex + 1);
        Inc(ARunIndex);
      end;
    end
    else
      Inc(ARunIndex);
  end;
end;

function TdxHyperlinkEditorHelper.IsHyperlink(ASelStart,
  ASelLength: Integer): Boolean;
var
  I, J: Integer;
  ACharCounter: Integer;
begin
  Result := False;
  ACharCounter := 0;
  for I := 0 to FFormattedText.Runs.Count - 1 do
  begin
    if OpensHyperlink(FFormattedText.Runs[I]) then
    begin
      Result := (ASelStart + ASelLength >= ACharCounter);
      ACharCounter := ACharCounter + 12 +
        Length(TdxFormattedTextURLRun(FFormattedText.Runs[I]).Hyperlink);
      FSelectedRunsStartIndex := I;
    end;
    if Result then
      Result := not ClosesHyperlink(FFormattedText.Runs[I]);

    for J := 0 to FFormattedText.Runs[I].TextLength - 1 do
      if (FFormattedText.Runs[I].TextStart + J)^ <> #10 then
        Inc(ACharCounter);
    if (Result and (ASelStart < ACharCounter)) or
      (ASelStart + ASelLength < ACharCounter) then
      Break;
  end;
  if Result then
    FSelectedRunsFinishIndex := GetCloseRunIndex(FSelectedRunsStartIndex);
end;

function TdxHyperlinkEditorHelper.DeleteHyperlink: string;
begin
  Result := Format('%s%s%s', [dxGetString(PChar(FFormattedText.Text),
    FFormattedText.Runs[FSelectedRunsStartIndex - 1].TextFinish),
    dxGetString(FFormattedText.Runs[FSelectedRunsStartIndex].TextStart,
    FFormattedText.Runs[FSelectedRunsFinishIndex - 1].TextFinish),
    dxGetString(FFormattedText.Runs[FSelectedRunsFinishIndex].TextStart,
    FFormattedText.Runs.Last.TextFinish)]);
end;

function TdxHyperlinkEditorHelper.EditHyperlink(const AText,
  AUrl: string): string;
var
  ABuilder: TStringBuilder;
  I: Integer;
  ARunCodeString: TdxRunCodeStringDictionary;
begin
  ABuilder := TStringBuilder.Create;
  try
    ABuilder.Append(dxGetString(PChar(FFormattedText.Text),
      FFormattedText.Runs[FSelectedRunsStartIndex - 1].TextFinish));
    ABuilder.Append('[URL=_');
    ABuilder.Append(AUrl);
    ABuilder.Append(']');
    ABuilder.Append(AText);
    ABuilder.Append('[/URL]');
    ARunCodeString := TdxRunCodeStringDictionary.Create;
    try
      TdxFormattedTextConverterBBCode.PopulateDictionary(ARunCodeString);
      for I := FSelectedRunsStartIndex + 1 to FSelectedRunsFinishIndex - 1 do
        TdxFormattedTextConverterBBCode.AddTextTagFromRun(ARunCodeString,
          FFormattedText.Runs[I], ABuilder);
    finally
      ARunCodeString.Free;
    end;
    ABuilder.Append(dxGetString(FFormattedText.Runs[FSelectedRunsFinishIndex]
      .TextStart, FFormattedText.Runs.Last.TextFinish));
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkEditorHelper.EditHyperlink(const AUrl: string): string;
var
  ABuilder: TStringBuilder;
begin
  ABuilder := TStringBuilder.Create;
  try
    ABuilder.Append(dxGetString(PChar(FFormattedText.Text),
      FFormattedText.Runs[FSelectedRunsStartIndex - 1].TextFinish));
    ABuilder.Append('[URL=_');
    ABuilder.Append(AUrl);
    ABuilder.Append(']');
    ABuilder.Append(dxGetString(FFormattedText.Runs[FSelectedRunsStartIndex]
      .TextStart, FFormattedText.Runs.Last.TextFinish));
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkEditorHelper.GetSelectedText(ASelStart,
  ASelLength: Integer): string;
var
  I: Integer;
begin
  FSelectedTextStart := RichCharPositionToBBCodeChar(ASelStart,
    FSelectedRunsStartIndex);
  FSelectedTextFinish := RichCharPositionToBBCodeChar(ASelStart + ASelLength,
    FSelectedRunsFinishIndex);
  if (FSelectedRunsFinishIndex > 0) and
    (FFormattedText.Runs[FSelectedRunsFinishIndex].Action = traOpen) and
    (FFormattedText.Runs[FSelectedRunsFinishIndex] is TdxFormattedTextURLRun)
  then
  begin
    FSelectedRunsFinishIndex := FSelectedRunsFinishIndex - 1;
    FSelectedTextFinish := FFormattedText.Runs[FSelectedRunsFinishIndex]
      .TextFinish;
  end;
  if FSelectedRunsStartIndex < FSelectedRunsFinishIndex then
  begin
    Result := dxGetString(FSelectedTextStart,
      FFormattedText.Runs[FSelectedRunsStartIndex].TextFinish);
    for I := FSelectedRunsStartIndex + 1 to FSelectedRunsFinishIndex - 1 do
      Result := Result + dxGetString(FFormattedText.Runs[I].TextStart,
        FFormattedText.Runs[I].TextFinish);
    Result := Result + dxGetString(FFormattedText.Runs[FSelectedRunsFinishIndex]
      .TextStart, FSelectedTextFinish);
  end
  else
    Result := dxGetString(FSelectedTextStart, FSelectedTextFinish);
end;

function TdxHyperlinkEditorHelper.GetSelectedHyperlinkText(ASelStart: Integer;
  out AUrl: string): string;
var
  I: Integer;
  ABuilder: TStringBuilder;
begin
  FSelectedTextStart := FFormattedText.Runs[FSelectedRunsStartIndex - 1]
    .TextFinish;
  FSelectedTextFinish := FFormattedText.Runs[FSelectedRunsFinishIndex]
    .TextStart;
  AUrl := TdxFormattedTextURLRun(FFormattedText.Runs[FSelectedRunsStartIndex])
    .Hyperlink;
  if AUrl[1] = '_' then
    Delete(AUrl, 1, 1);
  ABuilder := TStringBuilder.Create;
  try
    for I := FSelectedRunsStartIndex to FSelectedRunsFinishIndex - 1 do
      ABuilder.Append(dxGetString(FFormattedText.Runs[I].TextStart,
        FFormattedText.Runs[I].TextFinish));
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkEditorHelper.InsertHyperlink(const AUrl: string): string;
var
  ABuilder: TStringBuilder;
begin
  ABuilder := TStringBuilder.Create;
  try
    ABuilder.Append(dxGetString(PChar(FFormattedText.Text),
      FSelectedTextStart));
    ABuilder.Append('[URL=_');
    ABuilder.Append(AUrl);
    ABuilder.Append(']');
    ABuilder.Append(dxGetString(FSelectedTextStart, FSelectedTextFinish));
    ABuilder.Append('[/URL]');
    if FSelectedTextFinish <> nil then
      ABuilder.Append(dxGetString(FSelectedTextFinish,
        FFormattedText.Runs.Last.TextFinish));
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkEditorHelper.InsertHyperlink(const AText,
  AUrl: string): string;
var
  ABuilder: TStringBuilder;
  I: Integer;
  ARunCodeString: TdxRunCodeStringDictionary;
begin
  ABuilder := TStringBuilder.Create;
  try
    if FFormattedText.Text <> '' then
      ABuilder.Append(dxGetString(PChar(FFormattedText.Text),
        FSelectedTextStart));
    ABuilder.Append('[URL=_');
    ABuilder.Append(AUrl);
    ABuilder.Append(']');
    ABuilder.Append(AText);
    ABuilder.Append('[/URL]');
    ARunCodeString := TdxRunCodeStringDictionary.Create;
    try
      TdxFormattedTextConverterBBCode.PopulateDictionary(ARunCodeString);
      for I := FSelectedRunsStartIndex + 1 to FSelectedRunsFinishIndex do
        TdxFormattedTextConverterBBCode.AddTextTagFromRun(ARunCodeString,
          FFormattedText.Runs[I], ABuilder);
    finally
      ARunCodeString.Free;
    end;
    if FSelectedTextFinish <> nil then
      ABuilder.Append(dxGetString(FSelectedTextFinish,
        FFormattedText.Runs.Last.TextFinish));
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

procedure TdxHyperlinkEditorHelper.RemoveEmptyUrlRuns;
var
  ARunIndex: Integer;
begin
  ARunIndex := 1;
  while ARunIndex < FFormattedText.Runs.Count do
  begin
    if (FFormattedText.Runs[ARunIndex] is TdxFormattedTextURLRun) and
      (FFormattedText.Runs[ARunIndex].Action = traOpen) and
      (FFormattedText.Runs[GetCloseRunIndex(ARunIndex)].TextStart -
      FFormattedText.Runs[ARunIndex].TextStart - 6 = 0) then
      DeletePairRuns(ARunIndex)
    else
      Inc(ARunIndex);
  end;
end;

procedure TdxHyperlinkEditorHelper.RemoveAdditionalSymbolFromUrls;
var
  I: Integer;
  ANextPosition: PChar;
  ABuilder: TStringBuilder;
begin
  ANextPosition := FFormattedText.Runs[0].TextStart;
  ABuilder := TStringBuilder.Create;
  try
    for I := 0 to FFormattedText.Runs.Count - 1 do
    begin
      if (FFormattedText.Runs[I] is TdxFormattedTextURLRun) and
        (FFormattedText.Runs[I].Action = traOpen) and
        (Length(TdxFormattedTextURLRun(FFormattedText.Runs[I]).Hyperlink) > 0)
        and (TdxFormattedTextURLRun(FFormattedText.Runs[I]).Hyperlink[1] = '_')
      then
      begin
        ABuilder.Append(dxGetString(ANextPosition, ANextPosition + 5));
        ABuilder.Append(dxGetString(ANextPosition + 6,
          FFormattedText.Runs[I].TextFinish));
      end
      else
        ABuilder.Append(dxGetString(ANextPosition,
          FFormattedText.Runs[I].TextFinish));
      ANextPosition := FFormattedText.Runs[I].TextFinish;
    end;
    FFormattedText.Import(ABuilder.ToString);
  finally
    ABuilder.Free;
  end;
end;

procedure TdxHyperlinkEditorHelper.InsertAdditionalSymbolFromUrls;
var
  I: Integer;
  ABuilder: TStringBuilder;
  ANextPosition: PChar;
begin
  ANextPosition := FFormattedText.Runs[0].TextStart;
  ABuilder := TStringBuilder.Create;
  try
    for I := 0 to FFormattedText.Runs.Count - 1 do
    begin
      if (FFormattedText.Runs[I] is TdxFormattedTextURLRun) and
        (FFormattedText.Runs[I].Action = traOpen) then
      begin
        ABuilder.Append(dxGetString(ANextPosition, ANextPosition + 5));
        if ABuilder.Chars[ABuilder.Length - 1] = '=' then
          ABuilder.Append('_');
        ABuilder.Append(dxGetString(ANextPosition + 5,
          FFormattedText.Runs[I].TextFinish));
      end
      else
        ABuilder.Append(dxGetString(ANextPosition,
          FFormattedText.Runs[I].TextFinish));
      ANextPosition := FFormattedText.Runs[I].TextFinish;
    end;
    FFormattedText.Import(ABuilder.ToString);
  finally
    ABuilder.Free;
  end;
end;

procedure TdxHyperlinkEditorHelper.DeleteRuns(ARunIndexes: array of Integer);
var
  I: Integer;
  ABuilder: TStringBuilder;
  ACount: Integer;
begin
  ABuilder := TStringBuilder.Create;
  try
    ACount := Length(ARunIndexes);
    if ARunIndexes[0] > 0 then
      ABuilder.Append(dxGetString(PChar(FFormattedText.Text),
        FFormattedText.Runs[ARunIndexes[0] - 1].TextFinish));
    for I := 0 to ACount - 1 do
    begin
      if I < ACount - 1 then
        ABuilder.Append(dxGetString(FFormattedText.Runs[ARunIndexes[I]]
          .TextStart, FFormattedText.Runs[ARunIndexes[I + 1] - 1].TextFinish))
      else
        ABuilder.Append(dxGetString(FFormattedText.Runs[ARunIndexes[I]]
          .TextStart, FFormattedText.Runs.Last.TextFinish));
    end;
    FFormattedText.Import(ABuilder.ToString);
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkEditorHelper.GetCloseRunIndex(AOpenRunIndex
  : Integer): Integer;
var
  I: Integer;
begin
  Result := AOpenRunIndex;
  for I := AOpenRunIndex + 1 to FFormattedText.Runs.Count - 1 do
    if (FFormattedText.Runs[I].Action = traClose) and
      (FFormattedText.Runs[I].ClassName = FFormattedText.Runs[AOpenRunIndex]
      .ClassName) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxHyperlinkEditorHelper.ClosesHyperlink
  (ARun: TdxFormattedTextRun): Boolean;
begin
  Result := (ARun.Action = traClose) and (ARun is TdxFormattedTextURLRun);
end;

procedure TdxHyperlinkEditorHelper.DeletePairRuns(ARunIndex: Integer);
begin
  DeleteRuns([ARunIndex, GetCloseRunIndex(ARunIndex)]);
end;

procedure TdxHyperlinkEditorHelper.MoveRun(ARunIndex, ANewIndex: Integer);
var
  ABuilder: TStringBuilder;
  ARunCodeString: TdxRunCodeStringDictionary;
begin
  if ARunIndex < ANewIndex then
  begin
    ARunCodeString := TdxRunCodeStringDictionary.Create;
    try
      TdxFormattedTextConverterBBCode.PopulateDictionary(ARunCodeString);
      ABuilder := TStringBuilder.Create;
      try
        if ARunIndex > 0 then
          ABuilder.Append(dxGetString(PChar(FFormattedText.Text),
            FFormattedText.Runs[ARunIndex - 1].TextFinish));
        ABuilder.Append(dxGetString(FFormattedText.Runs[ARunIndex].TextStart,
          FFormattedText.Runs[ANewIndex - 1].TextFinish));
        TdxFormattedTextConverterBBCode.AddTextTagFromRun(ARunCodeString,
          FFormattedText.Runs[ARunIndex + 1], ABuilder);
        TdxFormattedTextConverterBBCode.AddTextTagFromRun(ARunCodeString,
          FFormattedText.Runs[ARunIndex], ABuilder);
        ABuilder.Append(dxGetString(FFormattedText.Runs[ANewIndex].TextStart,
          FFormattedText.Runs.Last.TextFinish));
        FFormattedText.Import(ABuilder.ToString);
      finally
        ABuilder.Free;
      end;
    finally
      ARunCodeString.Free;
    end;
  end;
end;

function TdxHyperlinkEditorHelper.OpensHyperlink
  (ARun: TdxFormattedTextRun): Boolean;
begin
  Result := (ARun.Action = traOpen) and (ARun is TdxFormattedTextURLRun);
end;

function TdxHyperlinkEditorHelper.RichCharPositionToBBCodeChar(AIndex: Integer;
  out ARunIndex: Integer): PChar;
var
  I, J: Integer;
  ACharCounter: Integer;
begin
  Result := nil;
  ARunIndex := -1;
  if FFormattedText.Runs.Count > 0 then
  begin
    Result := FFormattedText.Runs.Last.TextFinish;
    ARunIndex := FFormattedText.Runs.Count - 1;
    ACharCounter := 0;
    for I := 0 to FFormattedText.Runs.Count - 1 do
    begin
      if OpensHyperlink(FFormattedText.Runs[I]) then
        ACharCounter := ACharCounter + 12 +
          Length(TdxFormattedTextURLRun(FFormattedText.Runs[I]).Hyperlink);
      for J := 0 to FFormattedText.Runs[I].TextLength - 1 do
        if ACharCounter >= AIndex then
        begin
          ARunIndex := I;
          Result := FFormattedText.Runs[I].TextStart + J;
          Exit;
        end
        else if (FFormattedText.Runs[I].TextStart + J)^ <> dxLF then
          Inc(ACharCounter);
    end;
  end;
end;

{ TfrmFormattedLabelCaptionEditor }

constructor TfrmFormattedLabelCaptionEditor.Create(AOwner: TComponent);
begin
  inherited;
  FFormattedText := TdxFormattedText.Create;
  FHyperlinkEditorHelper := TdxHyperlinkEditorHelper.Create(FFormattedText);
  FFont := TFont.Create;
end;

destructor TfrmFormattedLabelCaptionEditor.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FHyperlinkEditorHelper);
  FreeAndNil(FFormattedText);
  inherited;
end;

procedure TfrmFormattedLabelCaptionEditor.Apply;
begin
  FFormattedText.Import(Text);
  if lgTabGroup.ItemIndex = 1 then
  begin
    FHyperlinkEditorHelper.RemoveAdditionalSymbolFromUrls;
    FFont.Color := clBlack;
  end;
  FFormattedLabel.Caption := FFormattedText.Text;
  if FFormattedLabel.Style.Font.Size <> FFont.Size then
    FFormattedLabel.Style.Font.Size := FFont.Size;
  if FFormattedLabel.Style.Font.Name <> FFont.Name then
    FFormattedLabel.Style.Font.Name := FFont.Name;
  if FFormattedLabel.Style.TextStyle <> FFont.Style then
    FFormattedLabel.Style.TextStyle := FFont.Style;
  if FFormattedLabel.Style.TextColor <> FFont.Color then
    FFormattedLabel.Style.TextColor := FFont.Color;
  btnApply.Enabled := False;
end;

function TfrmFormattedLabelCaptionEditor.GetText: string;
begin
  if lgTabGroup.ItemIndex = 1 then
  begin
    TdxFormattedTextConverterRTF.Import(FFormattedText, reRtf.EditValue,
      FFont, True);
    FHyperlinkEditorHelper.ClearHyperlinks;
  end
  else
    FFormattedText.Import(reBBCode.Text);
  Result := FFormattedText.Text;
end;

procedure TfrmFormattedLabelCaptionEditor.lgTabGroupTabChanged(Sender: TObject);
begin
  if FFormattedText = nil then
    Exit;
  case lgTabGroup.ItemIndex of
    0:
      begin
        if reRtf.EditModified then
        begin
          TdxFormattedTextConverterRTF.Import(FFormattedText, reRtf.EditValue,
            FFont, True);
          FHyperlinkEditorHelper.ClearHyperlinks;
          FFont.Color := clBlack;
        end;
        FHyperlinkEditorHelper.RemoveAdditionalSymbolFromUrls;
        reBBCode.Text := FFormattedText.Text;
        acNoparse.Enabled := True;
        SettingStateButtons(reBBCode);
      end;
  else
    begin
      FFormattedText.Import(reBBCode.Text);
      FHyperlinkEditorHelper.RemoveEmptyUrlRuns;
      FHyperlinkEditorHelper.InsertAdditionalSymbolFromUrls;
      reRtf.EditValue := TdxFormattedTextConverterRTF.Export(FFormattedText,
        FFont, True);
      acNoparse.Enabled := False;
      SettingStateButtons(reRtf);
    end;
  end;
  reRtf.EditModified := False;
end;

procedure TfrmFormattedLabelCaptionEditor.acBackgroundColorExecute
  (Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    if cdColor.Execute then
    begin
      reBBCode.SelText := Format('[BACKCOLOR=#%s]%s[/BACKCOLOR]',
        [TdxAlphaColors.ToHexCode(cdColor.Color, False), reBBCode.SelText]);
      reBBCode.SelStart := reBBCode.SelStart - 12;
    end;
  end
  else
  begin
    cdColor.Color := TdxAlphaColor(reRtf.SelAttributes2.BackgroundColor);
    if cdColor.Execute then
      reRtf.SelAttributes2.BackgroundColor :=
        TdxAlphaColors.ToColor(cdColor.Color);
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acBoldExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    reBBCode.SelText := Format('[B]%s[/B]', [reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 4;
  end
  else
  begin
    if acBold.Checked then
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style + [fsBold]
    else
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style - [fsBold];
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acCopyExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
    reBBCode.CopyToClipboard
  else
    reRtf.CopyToClipboard;
end;

procedure TfrmFormattedLabelCaptionEditor.acCutExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
    reBBCode.CutToClipboard
  else
    reRtf.CutToClipboard;
end;

procedure TfrmFormattedLabelCaptionEditor.acFontColorExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    if cdColor.Execute then
    begin
      reBBCode.SelText := Format('[COLOR=#%s]%s[/COLOR]',
        [TdxAlphaColors.ToHexCode(cdColor.Color, False), reBBCode.SelText]);
      reBBCode.SelStart := reBBCode.SelStart - 8;
    end;
  end
  else
  begin
    cdColor.Color := TdxAlphaColor(reRtf.SelAttributes.Color);
    if cdColor.Execute then
      reRtf.SelAttributes.Color := TdxAlphaColors.ToColor(cdColor.Color);
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acFontExecute(Sender: TObject);
var
  ASelectedText: string;
  ASelStartIndent: Integer;
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    if FontDialog1.Execute(reBBCode.Handle) then
    begin
      ASelStartIndent := 0;
      ASelectedText := reBBCode.SelText;
      if FontDialog1.Font.Name <> reBBCode.Style.Font.Name then
      begin
        ASelectedText := Format('[FONT=%s]%s[/FONT]',
          [FontDialog1.Font.Name, ASelectedText]);
        ASelStartIndent := ASelStartIndent + 7;
      end;
      if FontDialog1.Font.Size <> reBBCode.Style.Font.Size then
      begin
        ASelectedText := Format('[SIZE=%s]%s[/SIZE]',
          [IntToStr(FontDialog1.Font.Size), ASelectedText]);
        ASelStartIndent := ASelStartIndent + 7;
      end;
      if FontDialog1.Font.Color <> reBBCode.Style.Font.Color then
      begin
        ASelectedText := Format('[COLOR=#%.2x%.2x%.2x]%s[/COLOR]',
          [dxColorToRGBQuad(FontDialog1.Font.Color).rgbRed,
          dxColorToRGBQuad(FontDialog1.Font.Color).rgbGreen,
          dxColorToRGBQuad(FontDialog1.Font.Color).rgbBlue, ASelectedText]);
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
    end
  end
  else
  begin
    FontDialog1.Font.Assign(reRtf.SelAttributes);
    if FontDialog1.Execute then
      reRtf.SelAttributes.Assign(FontDialog1.Font);
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acHyperlinkExecute(Sender: TObject);
var
  ADialogForm: TdxHyperlinkDialogForm;
  AText, AUrl: string;
  AIsHyperlink: Boolean;
begin
  ADialogForm := TdxHyperlinkDialogForm.Create(nil);
  try
    if lgTabGroup.ItemIndex = 0 then
    begin
      ADialogForm.edtTextToDisplay.Text := reBBCode.SelText;
      if ADialogForm.ShowModal = mrOk then
      begin
        reBBCode.SelText := Format('[URL=%s]%s[/URL]',
          [ADialogForm.edtAddress.Text, ADialogForm.edtTextToDisplay.Text]);
        reBBCode.SelStart := reBBCode.SelStart - 6;
      end;
    end
    else
    begin
      TdxFormattedTextConverterBBCode.Import(FFormattedText, Text, FFont);
      FFont.Color := clBlack;
      AIsHyperlink := FHyperlinkEditorHelper.IsHyperlink(reRtf.SelStart,
        reRtf.SelLength);
      if AIsHyperlink then
        AText := FHyperlinkEditorHelper.GetSelectedHyperlinkText
          (reRtf.SelStart, AUrl)
      else
      begin
        AUrl := '';
        AText := FHyperlinkEditorHelper.GetSelectedText(reRtf.SelStart,
          reRtf.SelLength);
      end;
      ADialogForm.Address := AUrl;
      ADialogForm.TextToDisplay := AText;
      if ADialogForm.ShowModal = mrOk then
      begin
        if AIsHyperlink then
        begin
          if ADialogForm.TextToDisplay = AText then
            Text := TdxFormattedTextRtfHelper.GetRtfText(FFont,
              FHyperlinkEditorHelper.EditHyperlink(ADialogForm.Address))
          else
            Text := TdxFormattedTextRtfHelper.GetRtfText(FFont,
              FHyperlinkEditorHelper.EditHyperlink(ADialogForm.TextToDisplay,
              ADialogForm.Address));
        end
        else
        begin
          if ADialogForm.TextToDisplay = AText then
            Text := TdxFormattedTextRtfHelper.GetRtfText(FFont,
              FHyperlinkEditorHelper.InsertHyperlink(ADialogForm.Address))
          else
            Text := TdxFormattedTextRtfHelper.GetRtfText(FFont,
              FHyperlinkEditorHelper.InsertHyperlink(ADialogForm.TextToDisplay,
              ADialogForm.Address));
        end;
        reRtf.EditModified := True;
      end;
    end;
  finally
    ADialogForm.Free;
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acItalicExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    reBBCode.SelText := Format('[I]%s[/I]', [reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 4;
  end
  else
  begin
    if acItalic.Checked then
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style + [fsItalic]
    else
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style - [fsItalic];
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acNoparseExecute(Sender: TObject);
begin
  reBBCode.SelText := Format('[NOPARSE]%s[/NOPARSE]', [reBBCode.SelText]);
  reBBCode.SelStart := reBBCode.SelStart - 10;
end;

procedure TfrmFormattedLabelCaptionEditor.acPasteExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
    reBBCode.PasteFromClipboard
  else
    reRtf.PasteFromClipboard;
end;

procedure TfrmFormattedLabelCaptionEditor.acRemoveHyperlinkExecute
  (Sender: TObject);
begin
  Text := TdxFormattedTextRtfHelper.GetRtfText(FFont,
    FHyperlinkEditorHelper.DeleteHyperlink);
  reRtf.EditModified := True;
end;

procedure TfrmFormattedLabelCaptionEditor.acStrikeoutExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    reBBCode.SelText := Format('[S]%s[/S]', [reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 4;
  end
  else
  begin
    if acStrikeout.Checked then
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style + [fsStrikeOut]
    else
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style - [fsStrikeOut];
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acSubExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    reBBCode.SelText := Format('[SUB]%s[/SUB]', [reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 6;
  end
  else
  begin
    if acSub.Checked then
    begin
      reRtf.SelAttributes2.StyleEx := reRtf.SelAttributes2.StyleEx +
        [cffsSubscript];
      reRtf.SelAttributes2.StyleEx := reRtf.SelAttributes2.StyleEx -
        [cffsSuperscript];
    end
    else
      reRtf.SelAttributes2.StyleEx := reRtf.SelAttributes2.StyleEx -
        [cffsSubscript];
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acSupExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    reBBCode.SelText := Format('[SUP]%s[/SUP]', [reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 6;
  end
  else
  begin
    if acSup.Checked then
    begin
      reRtf.SelAttributes2.StyleEx := reRtf.SelAttributes2.StyleEx +
        [cffsSuperscript];
      reRtf.SelAttributes2.StyleEx := reRtf.SelAttributes2.StyleEx -
        [cffsSubscript];
    end
    else
      reRtf.SelAttributes2.StyleEx := reRtf.SelAttributes2.StyleEx -
        [cffsSuperscript];
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.acUnderlineExecute(Sender: TObject);
begin
  if lgTabGroup.ItemIndex = 0 then
  begin
    reBBCode.SelText := Format('[U]%s[/U]', [reBBCode.SelText]);
    reBBCode.SelStart := reBBCode.SelStart - 4;
  end
  else
  begin
    if acUnderline.Checked then
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style + [fsUnderline]
    else
      reRtf.SelAttributes2.Style := reRtf.SelAttributes2.Style - [fsUnderline];
  end;
end;

procedure TfrmFormattedLabelCaptionEditor.btnApplyClick(Sender: TObject);
begin
  Apply;
end;

procedure TfrmFormattedLabelCaptionEditor.FormShow(Sender: TObject);
begin
  FFormattedText.Import(reBBCode.Text);
end;

procedure TfrmFormattedLabelCaptionEditor.SetText(const Value: string);
begin
  if lgTabGroup.ItemIndex = 1 then
    reRtf.EditValue := Value
  else
    reBBCode.Text := Value;
end;

procedure TfrmFormattedLabelCaptionEditor.SettingStateButtons
  (ARichEdit: TcxRichEdit);
begin
  acBold.Checked := fsBold in ARichEdit.SelAttributes.Style;
  acItalic.Checked := fsItalic in ARichEdit.SelAttributes.Style;
  acUnderline.Checked := fsUnderline in ARichEdit.SelAttributes.Style;
  acStrikeout.Checked := fsStrikeOut in ARichEdit.SelAttributes.Style;
  acSub.Checked := cffsSubscript in ARichEdit.SelAttributes2.StyleEx;
  acSup.Checked := cffsSuperscript in ARichEdit.SelAttributes2.StyleEx;
end;

procedure TfrmFormattedLabelCaptionEditor.SettingPopupMenu;
var
  ARichEdit: TcxRichEdit;
begin
  if lgTabGroup.ItemIndex = 1 then
  begin
    ARichEdit := reRtf;
    FFormattedText.Import(Text, FFont);
    miHyperlink.Visible := not FHyperlinkEditorHelper.IsHyperlink
      (ARichEdit.SelStart, ARichEdit.SelLength);
    miEditHyperlink.Visible := not miHyperlink.Visible;
    miRemoveHyperlink.Visible := not miHyperlink.Visible;
  end
  else
  begin
    ARichEdit := reBBCode;
    miHyperlink.Visible := False;
    miEditHyperlink.Visible := False;
    miRemoveHyperlink.Visible := False;
  end;
  acCut.Enabled := ARichEdit.SelLength > 0;
  acCopy.Enabled := ARichEdit.SelLength > 0;
end;

procedure TfrmFormattedLabelCaptionEditor.reBBCodeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    SettingPopupMenu;
end;

procedure TfrmFormattedLabelCaptionEditor.reRtfMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    SettingPopupMenu;
end;

procedure TfrmFormattedLabelCaptionEditor.reRtfPropertiesSelectionChange
  (Sender: TObject);
begin
  SettingStateButtons(TcxRichEdit(Sender));
  btnApply.Enabled := True;
end;

end.
