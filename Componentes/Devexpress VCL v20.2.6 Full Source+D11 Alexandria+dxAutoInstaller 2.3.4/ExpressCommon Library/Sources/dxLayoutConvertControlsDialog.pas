{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressLayoutControl convert form }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxLayoutConvertControlsDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls,
  ActnList,
  dxForms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutContainer, cxContainer,
  cxEdit, cxClasses, dxLayoutLookAndFeels, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons, cxListView, cxImageList;

type
  { TfmConvertControlsDialog }

  TfmConvertControlsDialog = class(TdxForm)
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    llaflMain: TdxLayoutLookAndFeelList;
    llafCX: TdxLayoutCxLookAndFeel;
    lvRadioButtonList: TcxListView;
    lilvRadioButtonList: TdxLayoutItem;
    btnConvert: TcxButton;
    libtnConvert: TdxLayoutItem;
    btnCancel: TcxButton;
    libtnCancel: TdxLayoutItem;
    lgButtons: TdxLayoutGroup;
    ppmControls: TPopupMenu;
    miControlsCheckAll: TMenuItem;
    miControlsUncheckAll: TMenuItem;
    lliRadioButtonListCaption: TdxLayoutLabeledItem;
    lgRadioButtonListHeader: TdxLayoutGroup;
    libtnRadioButtonListUncheckAll: TdxLayoutItem;
    btnRadioButtonListUncheckAll: TcxButton;
    btnRadioButtonListCheckAll: TcxButton;
    libtnRadioButtonListCheckAll: TdxLayoutItem;
    lgRadioButtonList: TdxLayoutGroup;
    lilvLabelList: TdxLayoutItem;
    lvLabelList: TcxListView;
    lilvCheckBoxList: TdxLayoutItem;
    lvCheckBoxList: TcxListView;
    libtnLabelListCheckAll: TdxLayoutItem;
    btnLabelListCheckAll: TcxButton;
    libtnLabelListUncheckAll: TdxLayoutItem;
    btnLabelListUncheckAll: TcxButton;
    libtnCheckBoxListCheckAll: TdxLayoutItem;
    btnCheckBoxListCheckAll: TcxButton;
    libtnCheckBoxListUncheckAll: TdxLayoutItem;
    btnCheckBoxListUncheckAll: TcxButton;
    lgCheckBoxList: TdxLayoutGroup;
    lgLabelList: TdxLayoutGroup;
    lliCheckBoxListCaption: TdxLayoutLabeledItem;
    lliLabelListCaption: TdxLayoutLabeledItem;
    lgCheckBoxListHeader: TdxLayoutGroup;
    lgLabelListHeader: TdxLayoutGroup;
    lgControlLists: TdxLayoutGroup;
    alMain: TActionList;
    acCheckAll: TAction;
    acUncheckAll: TAction;
    liiWarning: TdxLayoutImageItem;
    ilCheckBoxes: TcxImageList;
    procedure acCheckAllExecute(Sender: TObject);
    procedure acUncheckAllExecute(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure lgControlListsTabChanged(Sender: TObject);
    procedure lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvChanging(Sender: TObject; Item: TListItem; Change: TItemChange;
      var AllowChange: Boolean);
    procedure lvColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvCustomDrawItemHandler(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvInfoTipHandler(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure lvRadioButtonListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  strict private
    FLayoutControl: TdxLayoutControl;
    FPrevChecked: Boolean;

    function GetControlsState: TcxCheckBoxState;
    function GetWarningMsg(AListItem: TListItem): string;
    procedure SetControlsState(const ACheckBoxState: TcxCheckBoxState);
  protected
    procedure ApplyLocalization;
    procedure CheckControls(AListView: TcxListView);
    procedure ConvertControls(AListView: TcxListView);
    function GetConversionWarning(AListItem: TListItem): string;
    procedure Initialize(ALayoutControl: TdxLayoutControl);
    procedure PopulateControlsList(AListView: TcxListView);
    procedure UpdateGroupState(AGroup: TdxLayoutGroup; AListView: TcxListView);
    procedure UpdateControlsStateImage;
    procedure UpdateStates;

    property ControlsState: TcxCheckBoxState read GetControlsState
      write SetControlsState;
    property LayoutControl: TdxLayoutControl read FLayoutControl;
  end;

  { TdxLayoutControlEmbeddedControlItemConversionHelper }

  TdxLayoutControlEmbeddedControlItemConversionHelper = class
  // for internal use
  private
    class function GetActualCaption(AItem: TdxLayoutItem;
      const AItemControlCaption: string): string;
  protected
    class procedure DoAssignCommonLabeledItemProperties
      (ASourceItem: TdxLayoutItem; ATargetItem: TdxCustomLayoutLabeledItem);
    class function DoConvertCheckBox(AItem: TdxLayoutItem)
      : TdxLayoutCheckBoxItem;
    class function DoConvertLabel(AItem: TdxLayoutItem): TdxLayoutLabeledItem;
    class function DoConvertRadioButton(AItem: TdxLayoutItem)
      : TdxLayoutRadioButtonItem;
    class procedure DoPopulateCustomizedInconvertibleProps(AControl: TControl;
      APersistent: TPersistent; var APropertiesList, AEventsList: TStringList;
      const APrefix: string = '');
    class function IsConvertibleProp(AControl: TControl;
      const APropName: string): Boolean;
    class procedure PopulateCustomizedInconvertibleProps(AControl: TControl;
      var APropertiesList, AEventsList: TStringList);
  public
    class function CanConvert(AItem: TdxCustomLayoutItem): Boolean;
    class function Convert(AItem: TdxCustomLayoutItem): TdxCustomLayoutItem;
  end;

procedure ShowConvertControlsDialog(ALayoutControl: TdxLayoutControl);

implementation

{$R *.dfm}

uses
  Contnrs, TypInfo, dxCore, cxLabel, cxCheckBox, cxRadioGroup, dxLayoutCommon,
  dxLayoutStrs;

const
  sdxLayoutControlConvertActionCheckAllCaption = 'Check All';
  sdxLayoutControlConvertActionUncheckAllCaption = 'Uncheck All';

  sdxLayoutControlConvertButtonCancelCaption = 'Cancel';
  sdxLayoutControlConvertButtonConvertCaption = 'Convert';

  sdxLayoutControlConvertConfirmationMsg =
    'One or more elements of the selected control(s) cannot be converted. ' +
    'Do you wish to discard these elements?';

  sdxLayoutControlConvertColumnClassCaption = 'Class';
  sdxLayoutControlConvertColumnCaptionCaption = 'Caption';
  sdxLayoutControlConvertColumnNameCaption = 'Name';
  sdxLayoutControlConvertColumnGroupIndexCaption = 'Group Index';

  sdxLayoutControlConvertCheckBoxesCaption = 'Check Boxes';
  sdxLayoutControlConvertLabelsCaption = 'Labels';
  sdxLayoutControlConvertRadioButtonsCaption = 'Radio Buttons';

  sdxLayoutControlConvertListCaption = 'Controls to convert:';

  sdxLayoutControlConvertWarningCaption =
    'Warning! The following elements of this control cannot be converted:';
  sdxLayoutControlConvertWarningEventCaption = 'Event handler:';
  sdxLayoutControlConvertWarningEventsCaption = 'Event handlers:';
  sdxLayoutControlConvertWarningPropertiesCaption = 'Property values:';
  sdxLayoutControlConvertWarningPropertyCaption = 'Property value:';

  sdxLayoutControlConvertWarningImageCaption =
    'One or more elements of the listed controls cannot be converted. ' +
    'The controls that include such elements are grayed out in the respective lists. '
    + 'Hover the mouse pointer over these controls to see detailed information.';

  sdxLayoutControlConvertSummaryMessageCheckBoxesCaption = 'Check boxes';
  sdxLayoutControlConvertSummaryMessageControlsPart = '%s: %d of %d converted.';
  sdxLayoutControlConvertSummaryMessageRadioButtonsCaption = 'Radio buttons';
  sdxLayoutControlConvertSummaryMessageSuccessfulConversionPart =
    'All the selected controls were converted.';
  sdxLayoutControlConvertSummaryMessageUnsuccessfulConversionPart =
    'Reopen the dialog to convert the remaining controls.';

type
  TControlAccess = class(TControl);

procedure ShowConvertControlsDialog(ALayoutControl: TdxLayoutControl);
var
  AForm: TfmConvertControlsDialog;
begin
  if ALayoutControl <> nil then
  begin
    AForm := TfmConvertControlsDialog.Create(nil);
    try
      AForm.Initialize(ALayoutControl);
      AForm.ShowModal;
    finally
      AForm.Free;
    end;
  end;
end;

{ TfmConvertControlsDialog }

procedure TfmConvertControlsDialog.ApplyLocalization;
begin
  acCheckAll.Caption := sdxLayoutControlConvertActionCheckAllCaption;
  acUncheckAll.Caption := sdxLayoutControlConvertActionUncheckAllCaption;

  lgCheckBoxList.Caption := sdxLayoutControlConvertCheckBoxesCaption;
  lgLabelList.Caption := sdxLayoutControlConvertLabelsCaption;
  lgRadioButtonList.Caption := sdxLayoutControlConvertRadioButtonsCaption;

  lliCheckBoxListCaption.Caption := sdxLayoutControlConvertListCaption;
  lliLabelListCaption.Caption := sdxLayoutControlConvertListCaption;
  lliRadioButtonListCaption.Caption := sdxLayoutControlConvertListCaption;

  lvCheckBoxList.Columns[1].Caption :=
    sdxLayoutControlConvertColumnCaptionCaption;
  lvCheckBoxList.Columns[2].Caption := sdxLayoutControlConvertColumnNameCaption;
  lvCheckBoxList.Columns[3].Caption :=
    sdxLayoutControlConvertColumnClassCaption;

  lvLabelList.Columns[1].Caption := sdxLayoutControlConvertColumnCaptionCaption;
  lvLabelList.Columns[2].Caption := sdxLayoutControlConvertColumnNameCaption;
  lvLabelList.Columns[3].Caption := sdxLayoutControlConvertColumnClassCaption;

  lvRadioButtonList.Columns[1].Caption :=
    sdxLayoutControlConvertColumnCaptionCaption;
  lvRadioButtonList.Columns[2].Caption :=
    sdxLayoutControlConvertColumnNameCaption;
  lvRadioButtonList.Columns[3].Caption :=
    sdxLayoutControlConvertColumnClassCaption;
  lvRadioButtonList.Columns[4].Caption :=
    sdxLayoutControlConvertColumnGroupIndexCaption;

  liiWarning.Caption := sdxLayoutControlConvertWarningImageCaption;

  btnConvert.Caption := sdxLayoutControlConvertButtonConvertCaption;
  btnCancel.Caption := sdxLayoutControlConvertButtonCancelCaption;
end;

procedure TfmConvertControlsDialog.CheckControls(AListView: TcxListView);
var
  AListItem: TListItem;
  AShowWarningImage: Boolean;
  I: Integer;
begin
  AShowWarningImage := False;
  AListView.Items.BeginUpdate;
  try
    for I := 0 to AListView.Items.Count - 1 do
    begin
      AListItem := AListView.Items[I];
      if GetConversionWarning(AListItem) = '' then
        AListItem.GroupID := 0
      else
      begin
        AListItem.GroupID := 1;
        AListItem.Checked := False;
        AShowWarningImage := True;
      end;
    end;
  finally
    AListView.Items.EndUpdate;
  end;
  liiWarning.Visible := liiWarning.Visible or AShowWarningImage;
  UpdateControlsStateImage;
end;

procedure TfmConvertControlsDialog.ConvertControls(AListView: TcxListView);
var
  I: Integer;
begin
  LayoutControl.BeginUpdate;
  try
    AListView.Tag := 0;
    for I := 0 to AListView.Items.Count - 1 do
      if AListView.Items[I].Checked then
        TdxLayoutControlEmbeddedControlItemConversionHelper.Convert
          (AListView.Items[I].Data)
      else
        AListView.Tag := AListView.Tag + 1;
  finally
    LayoutControl.EndUpdate;
  end;
end;

function TfmConvertControlsDialog.GetConversionWarning
  (AListItem: TListItem): string;
const
  CEventsCaptionForms: array [Boolean] of string =
    (sdxLayoutControlConvertWarningEventsCaption,
    sdxLayoutControlConvertWarningEventCaption);
  CPropertiesCaptionForms: array [Boolean] of string =
    (sdxLayoutControlConvertWarningPropertiesCaption,
    sdxLayoutControlConvertWarningPropertyCaption);
var
  AEventsList, APropertiesList: TStringList;
begin
  Result := '';
  AEventsList := TStringList.Create;
  APropertiesList := TStringList.Create;
  try
    TdxLayoutControlEmbeddedControlItemConversionHelper.
      PopulateCustomizedInconvertibleProps(TdxLayoutItem(AListItem.Data)
      .Control, APropertiesList, AEventsList);
    if APropertiesList.Count > 0 then
      Result := CPropertiesCaptionForms[APropertiesList.Count = 1] + ' ' +
        StringReplace(APropertiesList.DelimitedText, APropertiesList.Delimiter,
        ', ', [rfReplaceAll]);
    if AEventsList.Count > 0 then
    begin
      if Result <> '' then
        Result := Result + #13#10;
      Result := Result + CEventsCaptionForms[AEventsList.Count = 1] + ' ' +
        StringReplace(AEventsList.DelimitedText, AEventsList.Delimiter, ', ',
        [rfReplaceAll]);
    end;
  finally
    APropertiesList.Free;
    AEventsList.Free;
  end;
end;

procedure TfmConvertControlsDialog.Initialize(ALayoutControl: TdxLayoutControl);
begin
  FLayoutControl := ALayoutControl;
  ApplyLocalization;
  PopulateControlsList(lvCheckBoxList);
  PopulateControlsList(lvLabelList);
  PopulateControlsList(lvRadioButtonList);
  UpdateGroupState(lgRadioButtonList, lvRadioButtonList);
  UpdateGroupState(lgLabelList, lvLabelList);
  UpdateGroupState(lgCheckBoxList, lvCheckBoxList);
  UpdateStates;
end;

procedure TfmConvertControlsDialog.PopulateControlsList(AListView: TcxListView);

  function GetRadioButtonGroupIndex(AControl: TControl): Integer;
  begin
    Result := 0;
    if AControl is TcxRadioButton then
      Result := TcxRadioButton(AControl).GroupIndex;
  end;

  function GetListItemIndex(AControl: TControl; AOwner: TListItems): Integer;
  var
    AGroupIndex: Integer;
    I: Integer;
  begin
    Result := -1;
    if AControl is TRadioButton then
    begin
      AGroupIndex := GetRadioButtonGroupIndex(AControl);
      for I := 0 to AOwner.Count - 1 do
        if AGroupIndex < StrToInt(AOwner[I].SubItems[3]) then
        begin
          Result := I;
          Break;
        end;
    end;
  end;

var
  ACanPopulateControl: Boolean;
  AControl: TControl;
  ALayoutItem: TdxLayoutItem;
  AListItem: TListItem;
  I: Integer;
begin
  AListView.Items.BeginUpdate;
  try
    AListView.Items.Clear;
    for I := 0 to LayoutControl.AbsoluteItemCount - 1 do
      if TdxLayoutControlEmbeddedControlItemConversionHelper.CanConvert
        (LayoutControl.AbsoluteItems[I]) then
      begin
        ALayoutItem := TdxLayoutItem(LayoutControl.AbsoluteItems[I]);
        AControl := ALayoutItem.Control;
        ACanPopulateControl := ((AListView = lvCheckBoxList) and
          ((AControl is TCheckBox) or (AControl is TcxCheckBox))) or
          ((AListView = lvLabelList) and ((AControl is TLabel) or
          (AControl is TcxLabel))) or ((AListView = lvRadioButtonList) and
          (AControl is TRadioButton));
        if ACanPopulateControl then
        begin
          AListItem := AListView.Items.Insert(GetListItemIndex(AControl,
            AListView.Items));
          AListItem.Checked := True;
          AListItem.Data := ALayoutItem;
          AListItem.ImageIndex := -1;
          AListItem.SubItems.Add(RemoveAccelChars(TControlAccess(AControl)
            .Caption));
          AListItem.SubItems.Add(AControl.Name);
          AListItem.SubItems.Add(AControl.ClassName);
          if AControl is TRadioButton then
            AListItem.SubItems.Add
              (IntToStr(GetRadioButtonGroupIndex(AControl)));
        end;
      end;
    CheckControls(AListView);
  finally
    AListView.Items.EndUpdate;
  end;
end;

procedure TfmConvertControlsDialog.UpdateGroupState(AGroup: TdxLayoutGroup;
  AListView: TcxListView);
begin
  AGroup.Enabled := AListView.Items.Count > 0;
  if AGroup.Enabled then
    AGroup.Parent.ItemIndex := AGroup.Index;
end;

procedure TfmConvertControlsDialog.UpdateControlsStateImage;
var
  AListView: TcxListView;
begin
  case lgControlLists.ItemIndex of
    0:
      AListView := lvCheckBoxList;
    1:
      AListView := lvLabelList;
  else // 2:
    AListView := lvRadioButtonList;
  end;

  case ControlsState of
    cbsUnchecked:
      AListView.Column[0].ImageIndex := 0;
    cbsChecked:
      AListView.Column[0].ImageIndex := 1;
    cbsGrayed:
      AListView.Column[0].ImageIndex := 2;
  end;
end;

procedure TfmConvertControlsDialog.UpdateStates;

  function HasCheckedItems(AListView: TcxListView): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to AListView.Items.Count - 1 do
      if AListView.Items[I].Checked then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  btnConvert.Enabled := HasCheckedItems(lvCheckBoxList) or
    HasCheckedItems(lvLabelList) or HasCheckedItems(lvRadioButtonList);

  acCheckAll.Enabled := ((lgControlLists.ItemIndex = 0) and
    (lvCheckBoxList.Items.Count > 0)) or
    ((lgControlLists.ItemIndex = 1) and (lvLabelList.Items.Count > 0)) or
    ((lgControlLists.ItemIndex = 2) and (lvRadioButtonList.Items.Count > 0));
  acUncheckAll.Enabled := acCheckAll.Enabled;

  UpdateControlsStateImage;
end;

function TfmConvertControlsDialog.GetControlsState: TcxCheckBoxState;
var
  ACheckedCount, I: Integer;
  AListView: TcxListView;
begin
  case lgControlLists.ItemIndex of
    0:
      AListView := lvCheckBoxList;
    1:
      AListView := lvLabelList;
  else // 2:
    AListView := lvRadioButtonList;
  end;

  ACheckedCount := 0;
  for I := 0 to AListView.Items.Count - 1 do
    if AListView.Items[I].Checked then
      Inc(ACheckedCount);

  if ACheckedCount = 0 then
    Result := cbsUnchecked
  else if ACheckedCount = AListView.Items.Count then
    Result := cbsChecked
  else
    Result := cbsGrayed;
end;

function TfmConvertControlsDialog.GetWarningMsg(AListItem: TListItem): string;
begin
  Result := '';
  if AListItem <> nil then
    Result := GetConversionWarning(AListItem);
  if Result <> '' then
    Result := sdxLayoutControlConvertWarningCaption + #13#10 + Result;
end;

procedure TfmConvertControlsDialog.SetControlsState(const ACheckBoxState
  : TcxCheckBoxState);
var
  AListView: TcxListView;
  I: Integer;
begin
  case lgControlLists.ItemIndex of
    0:
      AListView := lvCheckBoxList;
    1:
      AListView := lvLabelList;
  else // 2:
    AListView := lvRadioButtonList;
  end;

  AListView.Items.BeginUpdate;
  try
    for I := 0 to AListView.Items.Count - 1 do
      AListView.Items[I].Checked := ACheckBoxState = cbsChecked;
  finally
    AListView.Items.EndUpdate;
  end;
end;

procedure TfmConvertControlsDialog.acCheckAllExecute(Sender: TObject);
begin
  ControlsState := cbsChecked;
end;

procedure TfmConvertControlsDialog.acUncheckAllExecute(Sender: TObject);
begin
  ControlsState := cbsUnchecked;
end;

procedure TfmConvertControlsDialog.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmConvertControlsDialog.btnConvertClick(Sender: TObject);

  function HasCheckedItemsWithWarnings(AListView: TcxListView): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to AListView.Items.Count - 1 do
      if AListView.Items[I].Checked and (AListView.Items[I].GroupID <> 0) then
      begin
        Result := True;
        Break;
      end;
  end;

  procedure ShowSummaryMessage;

    procedure AppendSummaryMessage(var AMsg: string; AListView: TcxListView;
      const ACaption: string);
    begin
      if AMsg <> '' then
        AMsg := AMsg + #13#10;
      AMsg := AMsg + Format(sdxLayoutControlConvertSummaryMessageControlsPart,
        [ACaption, AListView.Items.Count - AListView.Tag,
        AListView.Items.Count]);
    end;

  var
    AMsg: string;
  begin
    AMsg := '';
    AppendSummaryMessage(AMsg, lvCheckBoxList,
      sdxLayoutControlConvertSummaryMessageCheckBoxesCaption);
    AppendSummaryMessage(AMsg, lvLabelList,
      sdxLayoutControlConvertLabelsCaption);
    AppendSummaryMessage(AMsg, lvRadioButtonList,
      sdxLayoutControlConvertSummaryMessageRadioButtonsCaption);
    if lvCheckBoxList.Tag + lvLabelList.Tag + lvRadioButtonList.Tag <> 0 then
      AMsg := AMsg + #13#10 + #13#10 +
        sdxLayoutControlConvertSummaryMessageUnsuccessfulConversionPart
    else
      AMsg := sdxLayoutControlConvertSummaryMessageSuccessfulConversionPart;
    MessageDlg(AMsg, mtInformation, [mbOK], 0);
  end;

var
  ACanConvert: Boolean;
begin
  ACanConvert := True;
  if HasCheckedItemsWithWarnings(lvCheckBoxList) or
    HasCheckedItemsWithWarnings(lvLabelList) or
    HasCheckedItemsWithWarnings(lvRadioButtonList) then
    ACanConvert := MessageDlg(sdxLayoutControlConvertConfirmationMsg, mtWarning,
      [mbYes, mbNo], 0) = mrYes;
  if ACanConvert then
  begin
    ConvertControls(lvCheckBoxList);
    ConvertControls(lvLabelList);
    ConvertControls(lvRadioButtonList);
    ShowSummaryMessage;
    Close;
  end;
end;

procedure TfmConvertControlsDialog.lgControlListsTabChanged(Sender: TObject);
begin
  UpdateStates;
end;

procedure TfmConvertControlsDialog.lvChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Item.Checked <> FPrevChecked then
    UpdateStates;
end;

procedure TfmConvertControlsDialog.lvChanging(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
begin
  FPrevChecked := Item.Checked;
end;

procedure TfmConvertControlsDialog.lvColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.Index = 0 then
  begin
    if ControlsState = cbsChecked then
      ControlsState := cbsUnchecked
    else
      ControlsState := cbsChecked;
  end;
end;

procedure TfmConvertControlsDialog.lvCustomDrawItemHandler
  (Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
  if Item.GroupID <> 0 then
    Sender.Canvas.Font.Color := clGrayText;
end;

procedure TfmConvertControlsDialog.lvInfoTipHandler(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  InfoTip := GetWarningMsg(Item);
end;

procedure TfmConvertControlsDialog.lvRadioButtonListChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  I: Integer;
begin
  lvChange(Sender, Item, Change);
  if Item.Checked <> FPrevChecked then
  begin
    Item.Owner.BeginUpdate;
    try
      for I := 0 to Item.Owner.Count - 1 do
        if (Item.SubItems.Count > 3) and
          (Item.SubItems[3] = Item.Owner[I].SubItems[3]) then
          Item.Owner[I].Checked := Item.Checked;
    finally
      Item.Owner.EndUpdate;
    end;
  end;
end;

{ TdxLayoutControlEmbeddedControlItemConversionHelper }

class function TdxLayoutControlEmbeddedControlItemConversionHelper.Convert
  (AItem: TdxCustomLayoutItem): TdxCustomLayoutItem;
var
  AControl: TControl;
  AName: string;
begin
  Result := nil;
  if CanConvert(AItem) then
  begin
    AControl := TdxLayoutItem(AItem).Control;
    if (AControl is TLabel) or (AControl is TcxLabel) then
      Result := DoConvertLabel(TdxLayoutItem(AItem))
    else if (AControl is TCheckBox) or (AControl is TcxCheckBox) then
      Result := DoConvertCheckBox(TdxLayoutItem(AItem))
    else if AControl is TRadioButton then
      Result := DoConvertRadioButton(TdxLayoutItem(AItem));
  end;
  if Result <> nil then
  begin
    AName := TdxLayoutItem(AItem).Control.Name;
    AItem.AlignmentConstraint := nil;
    AItem.Parent := nil;
    TdxLayoutItem(AItem).Control.Free;
    Result.Name := AName;
  end;
end;

class function TdxLayoutControlEmbeddedControlItemConversionHelper.CanConvert
  (AItem: TdxCustomLayoutItem): Boolean;
var
  AControl: TControl;
begin
  Result := AItem is TdxLayoutItem;
  if Result then
  begin
    AControl := TdxLayoutItem(AItem).Control;
    Result := (AControl is TCheckBox) or (AControl is TcxCheckBox) or
      (AControl is TLabel) or (AControl is TcxLabel) or
      (AControl is TRadioButton);
  end;
end;

class procedure TdxLayoutControlEmbeddedControlItemConversionHelper.
  DoAssignCommonLabeledItemProperties(ASourceItem: TdxLayoutItem;
  ATargetItem: TdxCustomLayoutLabeledItem);
var
  ATargetItemCaptionOptions: TdxLayoutLabeledItemCaptionOptions;
begin
  ATargetItem.AlignHorz := ASourceItem.AlignHorz;
  ATargetItem.AlignmentConstraint := ASourceItem.AlignmentConstraint;
  ATargetItem.AlignVert := ASourceItem.AlignVert;
  ATargetItem.AllowRemove := ASourceItem.AllowRemove;
  ATargetItem.AutoAligns := ASourceItem.AutoAligns;
  ATargetItem.Enabled := ASourceItem.Enabled;
  ATargetItem.Index := ASourceItem.Index;
  ATargetItem.LayoutLookAndFeel := ASourceItem.LayoutLookAndFeel;
  ATargetItem.Offsets := ASourceItem.Offsets;
  ATargetItem.Padding := ASourceItem.Padding;
  ATargetItem.SizeOptions := ASourceItem.SizeOptions;
  if ASourceItem.Control.Tag = 0 then
    ATargetItem.Tag := ASourceItem.Tag
  else
    ATargetItem.Tag := ASourceItem.Control.Tag;
  ATargetItem.Visible := ASourceItem.Visible;

  ATargetItemCaptionOptions := ATargetItem.CaptionOptions as
    TdxLayoutLabeledItemCaptionOptions;
  if ASourceItem.Control.Cursor <> crDefault then
    ATargetItemCaptionOptions.Cursor := ASourceItem.Control.Cursor
  else
    ATargetItemCaptionOptions.Cursor := ASourceItem.CaptionOptions.Cursor;
  ATargetItemCaptionOptions.Glyph := ASourceItem.CaptionOptions.Glyph;
  if ASourceItem.Control.Hint <> '' then
    ATargetItemCaptionOptions.Hint := ASourceItem.Control.Hint
  else
    ATargetItemCaptionOptions.Hint := ASourceItem.CaptionOptions.Hint;
  ATargetItemCaptionOptions.ImageIndex := ASourceItem.CaptionOptions.ImageIndex;
  ATargetItemCaptionOptions.ShowAccelChar :=
    ASourceItem.CaptionOptions.ShowAccelChar;
  ATargetItemCaptionOptions.VisibleElements :=
    ASourceItem.CaptionOptions.VisibleElements;
  ATargetItemCaptionOptions.Width := ASourceItem.CaptionOptions.Width;
end;

class function TdxLayoutControlEmbeddedControlItemConversionHelper.
  DoConvertCheckBox(AItem: TdxLayoutItem): TdxLayoutCheckBoxItem;
const
  CStateMap: array [TCheckBoxState] of TcxCheckBoxState = (cbsUnchecked,
    cbsChecked, cbsGrayed);
begin
  Result := AItem.Container.CreateItem(TdxLayoutCheckBoxItem, AItem.Parent)
    as TdxLayoutCheckBoxItem;
  DoAssignCommonLabeledItemProperties(AItem, Result);
  if AItem.Control is TCheckBox then
  begin
    Result.Action := (AItem.Control as TCheckBox).Action;
    Result.Caption := GetActualCaption(AItem,
      (AItem.Control as TCheckBox).Caption);
    if ((AItem.Control as TCheckBox).Caption <> '') and
      not(cveText in AItem.CaptionOptions.VisibleElements) then
      Result.CaptionOptions.VisibleElements :=
        Result.CaptionOptions.VisibleElements + [cveText];
    Result.CaptionOptions.WordWrap := (AItem.Control as TCheckBox).WordWrap;
    Result.CheckBoxOptions.AllowGrayed := (AItem.Control as TCheckBox)
      .AllowGrayed;
    Result.State := CStateMap[(AItem.Control as TCheckBox).State];
    Result.TabStop := (AItem.Control as TCheckBox).TabStop;
    Result.OnClick := (AItem.Control as TCheckBox).OnClick;
    case (AItem.Control as TCheckBox).Alignment of
      taLeftJustify:
        Result.CaptionOptions.Layout := clLeft;
      taRightJustify:
        Result.CaptionOptions.Layout := clRight;
    end;
  end
  else
  begin
    Result.Action := (AItem.Control as TcxCheckBox).Action;
    Result.Caption := GetActualCaption(AItem,
      (AItem.Control as TcxCheckBox).Caption);
    if ((AItem.Control as TcxCheckBox).Caption <> '') and
      not(cveText in AItem.CaptionOptions.VisibleElements) then
      Result.CaptionOptions.VisibleElements :=
        Result.CaptionOptions.VisibleElements + [cveText];
    Result.CaptionOptions.WordWrap := (AItem.Control as TcxCheckBox)
      .ActiveProperties.MultiLine;
    Result.CheckBoxOptions.AllowGrayed := (AItem.Control as TcxCheckBox)
      .ActiveProperties.AllowGrayed;
    Result.CheckBoxOptions.Glyph.Assign((AItem.Control as TcxCheckBox)
      .ActiveProperties.Glyph);
    Result.CheckBoxOptions.GlyphCount := (AItem.Control as TcxCheckBox)
      .ActiveProperties.GlyphCount;
    Result.CheckBoxOptions.ReadOnly := (AItem.Control as TcxCheckBox)
      .ActiveProperties.ReadOnly;
    Result.State := (AItem.Control as TcxCheckBox).State;
    Result.TabStop := (AItem.Control as TcxCheckBox).TabStop;
    Result.OnClick := (AItem.Control as TcxCheckBox).OnClick;
    Result.CheckBoxOptions.OnChange := (AItem.Control as TcxCheckBox)
      .Properties.OnChange;
    case (AItem.Control as TcxCheckBox).ActiveProperties.Alignment of
      taLeftJustify:
        Result.CaptionOptions.Layout := clRight;
      taRightJustify:
        Result.CaptionOptions.Layout := clLeft;
    else // taCenter:
      begin
        Result.CaptionOptions.Visible := False;
        Result.CaptionOptions.Layout := clBottom;
        Result.AlignHorz := ahCenter;
      end;
    end;
  end;
  if Result.CaptionOptions.WordWrap then
    Result.Width := AItem.Control.Width;
  if not(AItem.Control is TcxCheckBox) or not(AItem.Control as TcxCheckBox).AutoSize
  then
  begin
    Result.Height := AItem.Control.Height;
    Result.Width := AItem.Control.Width;
  end;
end;

class function TdxLayoutControlEmbeddedControlItemConversionHelper.
  DoConvertLabel(AItem: TdxLayoutItem): TdxLayoutLabeledItem;
const
  CCXLabelAlignVertMap: array [TcxEditVertAlignment] of TdxAlignmentVert =
    (tavTop, tavBottom, tavCenter);
  CLabelAlignVertMap: array [TTextLayout] of TdxAlignmentVert = (tavTop,
    tavCenter, tavBottom);
begin
  Result := AItem.Container.CreateItem(TdxLayoutLabeledItem, AItem.Parent)
    as TdxLayoutLabeledItem;
  DoAssignCommonLabeledItemProperties(AItem, Result);
  if AItem.Control is TLabel then
  begin
    Result.Caption := GetActualCaption(AItem, (AItem.Control as TLabel)
      .Caption);
    if ((AItem.Control as TLabel).Caption <> '') and
      not(cveText in AItem.CaptionOptions.VisibleElements) then
      Result.CaptionOptions.VisibleElements :=
        Result.CaptionOptions.VisibleElements + [cveText];
    Result.CaptionOptions.WordWrap := (AItem.Control as TLabel).WordWrap;
    Result.CaptionOptions.AlignHorz := (AItem.Control as TLabel).Alignment;
    Result.CaptionOptions.AlignVert := CLabelAlignVertMap
      [(AItem.Control as TLabel).Layout];
    Result.CaptionOptions.ShowAccelChar := (AItem.Control as TLabel)
      .ShowAccelChar;
    Result.OnCaptionClick := (AItem.Control as TLabel).OnClick;
    if not(AItem.Control as TLabel).AutoSize then
    begin
      Result.Height := AItem.Control.Height;
      Result.Width := AItem.Control.Width;
    end;
  end
  else
  begin
    Result.Caption := GetActualCaption(AItem,
      (AItem.Control as TcxLabel).Caption);
    if ((AItem.Control as TcxLabel).Caption <> '') and
      not(cveText in AItem.CaptionOptions.VisibleElements) then
      Result.CaptionOptions.VisibleElements :=
        Result.CaptionOptions.VisibleElements + [cveText];
    Result.CaptionOptions.WordWrap := (AItem.Control as TcxLabel)
      .ActiveProperties.WordWrap;
    Result.CaptionOptions.AlignHorz := (AItem.Control as TcxLabel)
      .ActiveProperties.Alignment.Horz;
    Result.CaptionOptions.AlignVert := CCXLabelAlignVertMap
      [(AItem.Control as TcxLabel).ActiveProperties.Alignment.Vert];
    Result.CaptionOptions.Glyph.Assign((AItem.Control as TcxLabel)
      .ActiveProperties.Glyph);
    Result.CaptionOptions.ShowAccelChar := (AItem.Control as TcxLabel)
      .ActiveProperties.ShowAccelChar;
    Result.OnCaptionClick := (AItem.Control as TcxLabel).OnClick;
    if not(AItem.Control as TcxLabel).AutoSize then
    begin
      Result.Height := AItem.Control.Height;
      Result.Width := AItem.Control.Width;
    end;
  end;
  if Result.CaptionOptions.WordWrap then
    Result.Width := AItem.Control.Width;
end;

class function TdxLayoutControlEmbeddedControlItemConversionHelper.
  DoConvertRadioButton(AItem: TdxLayoutItem): TdxLayoutRadioButtonItem;
begin
  Result := AItem.Container.CreateItem(TdxLayoutRadioButtonItem, AItem.Parent)
    as TdxLayoutRadioButtonItem;
  DoAssignCommonLabeledItemProperties(AItem, Result);
  Result.Action := (AItem.Control as TRadioButton).Action;
  Result.Caption := GetActualCaption(AItem,
    (AItem.Control as TRadioButton).Caption);
  if ((AItem.Control as TRadioButton).Caption <> '') and
    not(cveText in AItem.CaptionOptions.VisibleElements) then
    Result.CaptionOptions.VisibleElements :=
      Result.CaptionOptions.VisibleElements + [cveText];
  Result.CaptionOptions.WordWrap := (AItem.Control as TRadioButton).WordWrap;
  if AItem.Control is TcxRadioButton then
    Result.GroupIndex := (AItem.Control as TcxRadioButton).GroupIndex;
  Result.Checked := (AItem.Control as TRadioButton).Checked;
  Result.TabStop := (AItem.Control as TRadioButton).TabStop;
  Result.OnClick := (AItem.Control as TRadioButton).OnClick;
  case (AItem.Control as TRadioButton).Alignment of
    taLeftJustify:
      Result.CaptionOptions.Layout := clLeft;
    taRightJustify:
      Result.CaptionOptions.Layout := clRight;
  end;
  if Result.CaptionOptions.WordWrap then
    Result.Width := AItem.Control.Width;
  if not(AItem.Control is TcxRadioButton) or
    not(AItem.Control as TcxRadioButton).AutoSize then
  begin
    Result.Height := AItem.Control.Height;
    Result.Width := AItem.Control.Width;
  end;
end;

class procedure TdxLayoutControlEmbeddedControlItemConversionHelper.
  DoPopulateCustomizedInconvertibleProps(AControl: TControl;
  APersistent: TPersistent; var APropertiesList, AEventsList: TStringList;
  const APrefix: string = '');
var
  AList: PPropList;
  ACount: Integer;
  AMethod: TMethod;
  AObject: TObject;
  APropName: string;
  ASize: Integer;
  I: Integer;
begin
  ACount := GetPropList(APersistent.ClassInfo, tkAny, nil);
  ASize := ACount * SizeOf(Pointer);
  GetMem(AList, ASize);
  GetPropList(APersistent, AList);
  SortPropList(AList, ACount);

  for I := 0 to ACount - 1 do
  begin
    APropName := GetPropName(AList[I]);
    if IsPublishedProp(APersistent, APropName) and
      not IsConvertibleProp(AControl, APrefix + APropName) then
    begin
      if PropIsType(APersistent, APropName, tkMethod) then
      begin
        AMethod := GetMethodProp(APersistent, AList[I]);
        if AMethod.Data <> nil then
          AEventsList.Add(APrefix + APropName);
      end
      else if not IsDefaultPropertyValue(APersistent, AList[I], nil) then
      begin
        if PropIsType(APersistent, APropName, tkClass) then
        begin
          AObject := GetObjectProp(APersistent, AList[I]);
          if AObject is TPersistent then
          begin
            if AObject is TcxLookAndFeel then
            begin
              if TcxLookAndFeel(AObject).AssignedValues <> [] then
                APropertiesList.Add(APrefix + APropName)
            end
            else if (AObject is TGraphic) and Assigned(AObject) then
              APropertiesList.Add(APrefix + APropName)
            else if APropName = 'AssignedValues' then
              Continue
            else
              DoPopulateCustomizedInconvertibleProps(AControl,
                TPersistent(AObject), APropertiesList, AEventsList,
                APrefix + APropName + '.');
          end
          else
            APropertiesList.Add(APrefix + APropName);
        end
        else
          APropertiesList.Add(APrefix + APropName);
      end;
    end;
  end;

  FreeMem(AList);
end;

class function TdxLayoutControlEmbeddedControlItemConversionHelper.
  IsConvertibleProp(AControl: TControl; const APropName: string): Boolean;
const
  CConvertibleCommonProps =
    'Caption;Cursor;Enabled;Hint;Name;OnClick;Tag;Visible;';
  CSkippableCommonProps =
    'Align;AlignWithMargins;Anchors;BiDiMode;Constraints;DragCursor;DragKind;DragMode;Height;'
    + 'HelpContext;HelpKeyword;HelpType;Left;Margins;ParentColor;ShowHint;TabOrder;Top;Width;';
var
  AConvertibleProperties: TStringList;
begin
  AConvertibleProperties := TStringList.Create;
  try
    AConvertibleProperties.Delimiter := ';';

    if AControl is TcxLabel then
      AConvertibleProperties.DelimitedText := CConvertibleCommonProps +
        CSkippableCommonProps +
        'Properties.Alignment.Horz;Properties.Alignment.Vert;Properties.Glyph;Properties.ShowAccelChar;'
        + 'Properties.WordWrap;' +
        'AutoSize;FakeStyleController;Properties.Transparent;Style;StyleDisabled;StyleFocused;StyleHot;Touch;Transparent'
    else if AControl is TLabel then
      AConvertibleProperties.DelimitedText := CConvertibleCommonProps +
        CSkippableCommonProps + 'Alignment;Layout;ShowAccelChar;WordWrap;' +
        'AutoSize;Color;Font;Touch;Transparent'
    else if AControl is TcxCheckBox then
      AConvertibleProperties.DelimitedText := CConvertibleCommonProps +
        CSkippableCommonProps +
        'Action;Checked;Properties.Alignment;Properties.AllowGrayed;Properties.Glyph;Properties.GlyphCount;'
        + 'Properties.MultiLine;Properties.OnChange;Properties.ReadOnly;State;TabStop;'
        + 'AutoSize;FakeStyleController;ParentBackground;Properties.Caption;Properties.ClearKey;'
        + 'Properties.DisplayChecked;Properties.DisplayGrayed;Properties.DisplayUnchecked;Properties.ImmediatePost;'
        + 'Properties.NullStyle;Properties.UseAlignmentWhenInplace;Properties.ValueChecked;Properties.ValueGrayed;'
        + 'Properties.ValueUnchecked;Style;StyleDisabled;StyleFocused;StyleHot;Touch;Transparent'
    else if AControl is TCheckBox then
      AConvertibleProperties.DelimitedText := CConvertibleCommonProps +
        CSkippableCommonProps +
        'Action;Alignment;AllowGrayed;Checked;State;TabStop;WordWrap;' +
        'Color;Ctl3D;DoubleBuffered;Font'
    else if AControl is TcxRadioButton then
      AConvertibleProperties.DelimitedText := CConvertibleCommonProps +
        CSkippableCommonProps +
        'Action;Alignment;Checked;GroupIndex;TabStop;WordWrap;' +
        'AutoSize;Color;Ctl3D;DoubleBuffered;Font;ParentBackground;Transparent'
    else if AControl is TRadioButton then
      AConvertibleProperties.DelimitedText := CConvertibleCommonProps +
        CSkippableCommonProps + 'Action;Alignment;Checked;TabStop;WordWrap;' +
        'Color;Ctl3D;DoubleBuffered;Font';

    Result := AConvertibleProperties.IndexOf(APropName) >= 0;
  finally
    AConvertibleProperties.Free;
  end;
end;

class procedure TdxLayoutControlEmbeddedControlItemConversionHelper.
  PopulateCustomizedInconvertibleProps(AControl: TControl;
  var APropertiesList, AEventsList: TStringList);
begin
  DoPopulateCustomizedInconvertibleProps(AControl, AControl, APropertiesList,
    AEventsList);
end;

class function TdxLayoutControlEmbeddedControlItemConversionHelper.
  GetActualCaption(AItem: TdxLayoutItem;
  const AItemControlCaption: string): string;
begin
  Result := AItemControlCaption;
  if Result = '' then
    Result := AItem.Caption;
end;

end.
