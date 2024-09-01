{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSpreadSheet }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL }
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

unit dxExpressionEditor;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, Contnrs, ImgList,
  Menus, StdCtrls, Generics.Collections, Generics.Defaults, dxCore, dxForms,
  cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxClasses, dxLayoutContainer, dxLayoutControl,
  cxContainer, cxEdit, dxLayoutcxEditAdapters,
  cxTextEdit, cxMemo, cxRichEdit, cxMaskEdit, cxListBox, cxButtonEdit,
  dxSpreadSheetTypes, dxSpreadSheetClasses, cxButtons,
  dxSpreadSheetFunctions, dxLayoutControlAdapters, dxSpreadSheetCoreFormulas,
  dxLayoutLookAndFeels, cxCustomData,
  dxTreeView, dxMessages, cxImageList, dxExpressionRichEdit,
  cxDataControllerSpreadSheetExpressionProvider,
  dxExpressionEditorHelpers, ExtCtrls;

type
  TdxExpressionEditor = class;

  { TdxExpressionEditorController }

  TdxExpressionEditorController = class(TdxCustomExpressionEditorController)
  strict private
    FEditor: TdxExpressionEditor;
  protected
    function GetDataController: TcxCustomDataController; override;
    function GetRichEdit: TdxExpressionRichEdit; override;
  public
    constructor Create(AEditor: TdxExpressionEditor);
  end;

  { TdxExpressionEditor }

  TdxExpressionEditorClass = class of TdxExpressionEditor;

  TdxExpressionEditor = class(TdxForm)
    beFilter: TcxButtonEdit;
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    ilActions: TcxImageList;
    ilTypes: TcxImageList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgButtons: TdxLayoutGroup;
    liDescription: TdxLayoutItem;
    liExpression: TdxLayoutItem;
    liFilter: TdxLayoutItem;
    liFunctions: TdxLayoutItem;
    liFunctionTypes: TdxLayoutItem;
    llLookAndFeelList: TdxLayoutLookAndFeelList;
    reDescription: TcxRichEdit;
    reExpression: TdxExpressionRichEdit;
    siExpression: TdxLayoutSplitterItem;
    tvCategories: TdxInternalTreeView;
    tvItems: TdxInternalTreeView;
    tValidation: TTimer;
    lliError: TdxLayoutLabeledItem;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;

    procedure beFilterPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure beFilterPropertiesChange(Sender: TObject);
    procedure beFilterPropertiesEditValueChanged(Sender: TObject);
    procedure reExpressionChange(Sender: TObject);
    procedure reExpressionGetSuggestions(Sender: TObject;
      AList: TdxExpressionRichEditSuggestionList);
    procedure reExpressionKeyPress(Sender: TObject; var Key: Char);
    procedure reExpressionPostSuggestion(Sender: TObject; const AText: string;
      AData: Pointer);
    procedure tvCategoriesSelectionChanged(Sender: TObject);
    procedure tvItemsCustomDrawNode(Sender: TdxCustomTreeView;
      Canvas: TcxCanvas; NodeViewInfo: TdxTreeViewNodeViewInfo;
      var Handled: Boolean);
    procedure tvItemsDblClick(Sender: TObject);
    procedure tvItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvItemsSelectionChanged(Sender: TObject);
    procedure reExpressionDrawSuggestionImage(Sender: TObject;
      ACanvas: TcxCanvas; const R: TRect; AItem: TdxCustomListBoxItem;
      AState: TcxButtonState; var AHandled: Boolean);
    procedure reExpressionGetSuggestionHint(Sender: TObject; AData: TObject;
      var AHintText: string);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure beFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvCategoriesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure reExpressionDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure reExpressionDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btnOkClick(Sender: TObject);
    procedure tValidationTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  strict private
  const
    ListItemHeight = 20;
    HighFunctionsCategoryIndex = Ord(High(TdxSpreadSheetFunctionType));
    FieldsCategoryIndex = HighFunctionsCategoryIndex + 1;
    ConstantsCategoryIndex = FieldsCategoryIndex + 1;
    OperatorsCategoryIndex = ConstantsCategoryIndex + 1;
    FunctionGroupsCategoryIndex = OperatorsCategoryIndex + 1;
    ValidationDelay = 1000;
  strict private
    FController: TdxExpressionEditorController;
    FDataController: TcxCustomDataController;
    FModified: Boolean;

    function GetExpression: string;
    function GetExpressionOwner: TObject;
    function GetSelectedCategory: Integer;
    procedure PostSelectedSuggestion;
    procedure SetDataController(AValue: TcxCustomDataController);
    procedure SetExpression(const AValue: string);
    procedure SetExpressionOwner(const Value: TObject);
  protected
    procedure DoShow; override;

    procedure ApplyFilter;
    procedure ApplyLocalization;
    procedure Initialize; virtual;

    procedure AddConstantCategory;
    procedure AddFieldCategory;
    procedure AddFunctionCategories(AGroupCategory: TdxTreeViewNode);
    procedure AddFunctionGroupCategory;
    procedure AddOperatorCategory;

    procedure PopulateItems;
    procedure PopulateCategories;
    procedure UpdateDescription;
    procedure UpdateItemCategories;
    procedure UpdateItems;
    procedure UpdateLookAndFeel;
    function Validate(out AErrorCode: TdxExpressionValidatorErrorCode)
      : Boolean; overload;
    procedure Validate; overload;

    property SelectedCategory: Integer read GetSelectedCategory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DataController: TcxCustomDataController read FDataController
      write SetDataController;
    property ExpressionOwner: TObject read GetExpressionOwner
      write SetExpressionOwner;
    property Expression: string read GetExpression write SetExpression;
  end;

var
  FExpressionEditor: TdxExpressionEditorClass = TdxExpressionEditor;

procedure ShowExpressionEditor(ADataController: TcxCustomDataController;
  AItemIndex: Integer); overload;
procedure ShowExpressionEditor(ADataController: TcxCustomDataController;
  var AExpression: string); overload;

implementation

{$R *.dfm}

uses
  CommCtrl, Math,
  cxDataControllerSpreadSheetDataProvider, dxSpreadSheetCoreDialogsStrs,
  dxSpreadSheetCoreStrs,
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetCoreFormulasParser,
  dxTypeHelpers, cxDataStorage, dxCustomTree, dxCharacters, dxStringHelper,
  cxGeometry;

procedure ShowExpressionEditor(ADataController: TcxCustomDataController;
  var AExpression: string; AExpressionOwner: TObject); overload;
var
  AEditor: TdxExpressionEditor;
begin
  AEditor := FExpressionEditor.Create(nil);
  try
    AEditor.DataController := ADataController;
    AEditor.ExpressionOwner := AExpressionOwner;
    AEditor.Expression := AExpression;
    if AEditor.ShowModal = mrOk then
      AExpression := AEditor.Expression;
  finally
    AEditor.Free;
  end;
end;

procedure ShowExpressionEditor(ADataController: TcxCustomDataController;
  AItemIndex: Integer);
var
  AExpression: string;
begin
  AExpression := ADataController.ItemExpressions[AItemIndex];
  ShowExpressionEditor(ADataController, AExpression,
    ADataController.GetItem(AItemIndex));
  ADataController.ItemExpressions[AItemIndex] := AExpression;
end;

procedure ShowExpressionEditor(ADataController: TcxCustomDataController;
  var AExpression: string);
begin
  ShowExpressionEditor(ADataController, AExpression, nil);
end;

{ TdxExpressionEditorController }

constructor TdxExpressionEditorController.Create(AEditor: TdxExpressionEditor);
begin
  inherited Create;
  FEditor := AEditor;
end;

function TdxExpressionEditorController.GetDataController
  : TcxCustomDataController;
begin
  Result := FEditor.DataController;
end;

function TdxExpressionEditorController.GetRichEdit: TdxExpressionRichEdit;
begin
  Result := FEditor.reExpression;
end;

{ TdxExpressionEditor }

constructor TdxExpressionEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ApplyLocalization;
  Initialize;
end;

destructor TdxExpressionEditor.Destroy;
begin
  FreeAndNil(FController);
  cxDialogsMetricsStore.StoreMetrics(Self);
  inherited Destroy;
end;

procedure TdxExpressionEditor.DoShow;
begin
  inherited DoShow;
  FModified := False;
end;

procedure TdxExpressionEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrCancel then
  begin
    if FModified then
    begin
      case MessageDlg
        (cxGetResourceString(@sdxExpressionEditorCloseDialogConfirmation),
        mtConfirmation, mbYesNoCancel, 0) of
        mrYes:
          btnOk.Click;
        mrCancel:
          ModalResult := mrNone;
      end;
    end;
  end;
  CanClose := ModalResult <> mrNone;
end;

procedure TdxExpressionEditor.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  if (Msg.CharCode = VK_ESCAPE) and not reExpression.IsAutoCompleteWindowVisible
  then
    btnCancel.Click;
end;

procedure TdxExpressionEditor.ApplyFilter;
var
  I: Integer;
  AText: string;
  AHasFilter: Boolean;
  AFilterValue: string;
  ASelectedNode: TdxTreeViewNode;
begin
  ASelectedNode := tvItems.FocusedNode;
  tvItems.BeginUpdate;
  try
    AFilterValue := Trim(beFilter.EditingText);
    AHasFilter := AFilterValue <> '';
    AText := UpperCase(AFilterValue);
    for I := 0 to tvItems.Root.Count - 1 do
      tvItems.Root.Items[I].Visible := not AHasFilter or
        (Pos(AText, UpperCase(tvItems.Root.Items[I].Caption)) > 0);
  finally
    tvItems.EndUpdate;
  end;
  if ASelectedNode <> nil then
    if ASelectedNode.Visible then
      tvItems.MakeVisible(ASelectedNode)
    else
      tvItems.FocusedNode := nil;
end;

procedure TdxExpressionEditor.ApplyLocalization;
begin
  btnOk.Caption := cxGetResourceString(@sdxExpressionEditorButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxExpressionEditorButtonCancel);
  Caption := cxGetResourceString(@sdxExpressionEditorCaption);
  TEdit(beFilter.InnerControl).TextHint :=
    cxGetResourceString(@sdxExpressionEditorFilterTextHint);
end;

procedure TdxExpressionEditor.beFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    tvItems.SetFocus;
end;

procedure TdxExpressionEditor.beFilterPropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  beFilter.EditValue := Null;
  ApplyFilter;
end;

procedure TdxExpressionEditor.beFilterPropertiesChange(Sender: TObject);
begin
  if beFilter.EditingText = '' then
    beFilter.ActiveProperties.Buttons[0].ImageIndex := 1
  else
    beFilter.ActiveProperties.Buttons[0].ImageIndex := 0;
  ApplyFilter;
end;

procedure TdxExpressionEditor.beFilterPropertiesEditValueChanged
  (Sender: TObject);
begin
  if beFilter.Text = '' then
    beFilter.EditValue := Null;
  PopulateItems;
end;

procedure TdxExpressionEditor.btnOkClick(Sender: TObject);
var
  AErrorCode: TdxExpressionValidatorErrorCode;
begin
  if not FModified or Validate(AErrorCode) then
    ModalResult := mrOk
  else if AErrorCode in [TdxExpressionValidatorErrorCode.UnknownFieldName,
    TdxExpressionValidatorErrorCode.UnknownFunctionName] then
  begin
    if MessageDlg(cxGetResourceString
      (@sdxExpressionEditorUnknownFieldOrFunctionConfirmation), mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
      ModalResult := mrOk
  end
  else
    raise EdxSpreadSheetError.Create
      (cxGetResourceString(@sdxExpressionEditorInvalidExpressionExceptionText));
end;

function TdxExpressionEditor.GetExpression: string;
begin
  Result := reExpression.Text;
end;

function TdxExpressionEditor.GetExpressionOwner: TObject;
begin
  Result := FController.ExpressionOwner;
end;

function TdxExpressionEditor.GetSelectedCategory: Integer;
begin
  if tvCategories.FocusedNode <> nil then
    Result := Integer(tvCategories.FocusedNode.Data)
  else
    Result := -1;
end;

procedure TdxExpressionEditor.SetDataController
  (AValue: TcxCustomDataController);
begin
  if AValue <> DataController then
  begin
    FDataController := AValue;
    UpdateItemCategories;
  end;
end;

procedure TdxExpressionEditor.Initialize;
begin
  FController := TdxExpressionEditorController.Create(Self);
  tvItems.OptionsView.ItemHeight := ListItemHeight;
  tvCategories.OptionsView.ItemHeight := ListItemHeight;
  reExpression.Properties.HideSelection := False;
  reExpression.Style.TransparentBorder := False;
  reExpression.OnGetSuggestionHint := reExpressionGetSuggestionHint;
  tValidation.Interval := ValidationDelay;
  cxDialogsMetricsStore.InitDialog(Self);
  UpdateLookAndFeel;
end;

procedure TdxExpressionEditor.AddConstantCategory;
begin
  tvCategories.Root.AddChild
    (cxGetResourceString(@sdxExpressionEditorConstantsCategoryCaption),
    Pointer(ConstantsCategoryIndex));
end;

procedure TdxExpressionEditor.AddFieldCategory;
begin
  tvCategories.FocusedNode := tvCategories.Root.AddChild
    (cxGetResourceString(@sdxExpressionEditorFieldsCategoryCaption),
    Pointer(FieldsCategoryIndex));
end;

procedure TdxExpressionEditor.AddFunctionCategories(AGroupCategory
  : TdxTreeViewNode);

  function GetFunctionCount(AType: TdxSpreadSheetFunctionType): Integer;
  var
    AResult: Integer;
  begin
    AResult := 0;
    FController.EnumFunctions(
      procedure(AFunctionInfo: TdxSpreadSheetFunctionInfo)
      begin
        if AFunctionInfo.TypeID = AType then
          Inc(AResult);
      end);
    Result := AResult;
  end;

var
  AType: TdxSpreadSheetFunctionType;
begin
  for AType := Low(TdxSpreadSheetFunctionType)
    to High(TdxSpreadSheetFunctionType) do
    if dxSpreadSheetFunctionsRepository.HasFunctions(AType) and
      FController.IsFunctionTypeAllowed(AType) and (GetFunctionCount(AType) > 0)
    then
      AGroupCategory.AddChild(dxSpreadSheetFunctionTypeNameAsString(AType),
        TObject(AType));
end;

procedure TdxExpressionEditor.AddFunctionGroupCategory;
var
  ANode: TdxTreeViewNode;
begin
  ANode := tvCategories.Root.AddChild
    (cxGetResourceString(@sdxExpressionEditorFunctionGroupsCategoryCaption),
    Pointer(FunctionGroupsCategoryIndex));
  AddFunctionCategories(ANode);
end;

procedure TdxExpressionEditor.AddOperatorCategory;
begin
  tvCategories.Root.AddChild
    (cxGetResourceString(@sdxExpressionEditorOperatorsCategoryCaption),
    Pointer(OperatorsCategoryIndex));
end;

procedure TdxExpressionEditor.PopulateItems;

  procedure PopulateSpreadSheetFunctions(AFilterByType: Boolean = False;
  AType: TdxSpreadSheetFunctionType = TdxSpreadSheetFunctionType.ftCommon);
  begin
    FController.EnumFunctions(
      procedure(AFunctionInfo: TdxSpreadSheetFunctionInfo)
      var
        ANode: TdxTreeViewNode;
      begin
        if not AFilterByType or (AFunctionInfo.TypeID = AType) then
        begin
          ANode := tvItems.Root.AddChild(AFunctionInfo.Name, AFunctionInfo);
          ANode.ImageIndex := 0;
        end;
      end);
  end;

  procedure PopulateFields;
  begin
    FController.EnumFields(procedure(AItemIndex: Integer)
      var
        ANode: TdxTreeViewNode;
      begin
        ANode := tvItems.Root.AddChild(DataController.ExpressionProvider.
          GetItemReferenceName(AItemIndex), DataController.GetItem(AItemIndex));
        ANode.ImageIndex := FController.GetFieldImageIndex(AItemIndex);
      end);
  end;

  procedure PopulateOperators;
  begin
    FController.EnumOperators(procedure(AInfo: TdxExpressionOperatorInfo)
      var
        ANode: TdxTreeViewNode;
      begin
        ANode := tvItems.Root.AddChild(AInfo.Caption, AInfo);
        ANode.ImageIndex := -1;
      end);
  end;

  procedure PopulateConstants;
  begin
    FController.EnumConstants(procedure(AInfo: TdxExpressionConstantInfo)
      var
        ANode: TdxTreeViewNode;
      begin
        ANode := tvItems.Root.AddChild(AInfo.Data.Name, AInfo);
        ANode.ImageIndex := -1;
      end);
  end;

begin
  if tvCategories.FocusedNode = nil then
    Exit;
  tvItems.BeginUpdate;
  try
    tvItems.Root.Clear;
    case SelectedCategory of
      FieldsCategoryIndex:
        PopulateFields;
      ConstantsCategoryIndex:
        PopulateConstants;
      OperatorsCategoryIndex:
        PopulateOperators;
      FunctionGroupsCategoryIndex:
        PopulateSpreadSheetFunctions;
    else
      PopulateSpreadSheetFunctions(True,
        TdxSpreadSheetFunctionType(SelectedCategory));
    end;
    ApplyFilter;
    if SelectedCategory in [ConstantsCategoryIndex, OperatorsCategoryIndex] then
      tvItems.OptionsView.Images := nil
    else
      tvItems.OptionsView.Images := ilTypes;
  finally
    tvItems.EndUpdate;
  end;
end;

procedure TdxExpressionEditor.PostSelectedSuggestion;
begin
  FController.PostSuggestion(tvItems.FocusedNode.Caption,
    tvItems.FocusedNode.Data, False);
end;

procedure TdxExpressionEditor.PopulateCategories;
begin
  tvCategories.BeginUpdate;
  try
    AddFieldCategory;
    AddConstantCategory;
    AddOperatorCategory;
    AddFunctionGroupCategory;
  finally
    tvCategories.EndUpdate;
  end;
end;

procedure TdxExpressionEditor.UpdateItems;
begin
  tvItems.BeginUpdate;
  try
    tvItems.Root.Clear;
    PopulateItems;
  finally
    tvItems.EndUpdate;
  end;
end;

procedure TdxExpressionEditor.UpdateLookAndFeel;
begin
  FController.UpdateLookAndFeel;

end;

procedure TdxExpressionEditor.UpdateItemCategories;
begin
  tvCategories.BeginUpdate;
  try
    tvCategories.Root.Clear;
    PopulateCategories;
  finally
    tvCategories.EndUpdate;
  end;
end;

procedure TdxExpressionEditor.UpdateDescription;
begin
  if (tvItems.FocusedNode = nil) or (tvItems.FocusedNode.Data = nil) then
    reDescription.Lines.Clear
  else
    reDescription.EditValue := FController.GetDescription
      (tvItems.FocusedNode.Data);
end;

function TdxExpressionEditor.Validate(out AErrorCode
  : TdxExpressionValidatorErrorCode): Boolean;
var
  AHighlighter: TdxExpressionErrorHighlighter;
  AStart, AFinish: Integer;
begin
  if Trim(reExpression.Text) = '' then
    Exit(True);
  tValidation.Enabled := False;
  Result := not FController.HasError(AErrorCode, AStart, AFinish);
  if not Result then
  begin
    lliError.Caption := FController.GetErrorMessage(AErrorCode);
    lliError.Visible := True;
    AHighlighter := TdxExpressionErrorHighlighter.Create(AStart, AFinish);
    try
      AHighlighter.Apply(reExpression);
    finally
      AHighlighter.Free;
    end;
  end
  else
    lliError.Visible := False;
end;

procedure TdxExpressionEditor.Validate;
var
  AErrorCode: TdxExpressionValidatorErrorCode;
begin
  Validate(AErrorCode);
end;

procedure TdxExpressionEditor.SetExpression(const AValue: string);
begin
  reExpression.Text := AValue;
  tValidation.Enabled := False;
  FModified := False;
end;

procedure TdxExpressionEditor.SetExpressionOwner(const Value: TObject);
begin
  FController.ExpressionOwner := Value;
  UpdateItems;
end;

procedure TdxExpressionEditor.tValidationTimer(Sender: TObject);
begin
  Validate;
end;

procedure TdxExpressionEditor.tvCategoriesKeyDown(Sender: TObject;
var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F3 then
    beFilter.SetFocus;
end;

procedure TdxExpressionEditor.tvCategoriesSelectionChanged(Sender: TObject);
begin
  tvItems.FocusedNode := nil;
  beFilter.EditValue := Null;
  PopulateItems;
  UpdateDescription;
end;

procedure TdxExpressionEditor.tvItemsCustomDrawNode(Sender: TdxCustomTreeView;
Canvas: TcxCanvas; NodeViewInfo: TdxTreeViewNodeViewInfo; var Handled: Boolean);
var
  AType: TdxExpressionContextParserContextType;
begin
  case SelectedCategory of
    FieldsCategoryIndex:
      AType := TdxExpressionContextParserContextType.Field;
    ConstantsCategoryIndex:
      AType := TdxExpressionContextParserContextType.Value;
    OperatorsCategoryIndex:
      AType := TdxExpressionContextParserContextType.&Operator;
  else
    AType := TdxExpressionContextParserContextType.&Function;
  end;
  NodeViewInfo.ImageColorPalette := FController.GetImageColorPalette(AType,
    NodeViewInfo.HasSelection);
end;

procedure TdxExpressionEditor.tvItemsDblClick(Sender: TObject);
begin
  if tvItems.FocusedNode <> nil then
    PostSelectedSuggestion;
end;

procedure TdxExpressionEditor.tvItemsKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (tvItems.FocusedNode <> nil) then
    PostSelectedSuggestion;
  if Key = VK_F3 then
    beFilter.SetFocus;
end;

procedure TdxExpressionEditor.tvItemsSelectionChanged(Sender: TObject);
begin
  UpdateDescription;
end;

procedure TdxExpressionEditor.reExpressionDragDrop(Sender, Source: TObject;
X, Y: Integer);
begin
  if (Source is TcxDragControlObject) and
    (TcxDragControlObject(Source).Control = tvItems) and
    (tvItems.FocusedNode <> nil) then
    PostSelectedSuggestion;
end;

procedure TdxExpressionEditor.reExpressionDragOver(Sender, Source: TObject;
X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  P: TPoint;
begin
  if (Source is TcxDragControlObject) and
    (TcxDragControlObject(Source).Control = tvItems) and
    (tvItems.FocusedNode <> nil) then
  begin
    Accept := True;
    if State = dsDragLeave then
      tvItems.SetFocus
    else
      reExpression.SetFocus;
    if reExpression.Focused then
    begin
      P := TPoint.Create(X, Y);
      reExpression.SelStart := SendMessage(reExpression.InnerControl.Handle,
        EM_CHARFROMPOS, 0, LParam(@P));
    end;
  end;
end;

procedure TdxExpressionEditor.reExpressionDrawSuggestionImage(Sender: TObject;
ACanvas: TcxCanvas; const R: TRect; AItem: TdxCustomListBoxItem;
AState: TcxButtonState; var AHandled: Boolean);
var
  AImageRect: TRect;
  AType: TdxExpressionContextParserContextType;
begin
  if AItem.ImageIndex = -1 then
    Exit;
  AImageRect := cxRectCenter(R, dxGetImageSize(nil, ilTypes, 0, ScaleFactor));
  if AItem.Data is TdxSpreadSheetFunctionInfo then
    AType := TdxExpressionContextParserContextType.&Function
  else
    AType := TdxExpressionContextParserContextType.Field;
  cxDrawImage(ACanvas, AImageRect, nil, ilTypes, AItem.ImageIndex, True,
    FController.GetImageColorPalette(AType, False), ScaleFactor);
  AHandled := True;
end;

procedure TdxExpressionEditor.reExpressionGetSuggestionHint(Sender: TObject;
AData: TObject; var AHintText: string);
begin
  AHintText := FController.GetDescription(AData);
end;

procedure TdxExpressionEditor.reExpressionChange(Sender: TObject);
begin
  if FController <> nil then
    FController.HighlightTokens;
  lliError.Visible := False;
  tValidation.Enabled := False;
  tValidation.Enabled := True;
  FModified := True;
end;

procedure TdxExpressionEditor.reExpressionGetSuggestions(Sender: TObject;
AList: TdxExpressionRichEditSuggestionList);
begin
  FController.PopulateSuggestions(AList);
end;

procedure TdxExpressionEditor.reExpressionKeyPress(Sender: TObject;
var Key: Char);
begin
  if Key = TcxDataControllerSpreadSheetFormulaParser.ItemReferenceStartMark then
  begin
    reExpression.SelText := '[]';
    reExpression.SelStart := reExpression.SelStart - 1;
    reExpression.ShowAutoCompleteWindow;
    Key := #0;
  end;
end;

procedure TdxExpressionEditor.reExpressionPostSuggestion(Sender: TObject;
const AText: string; AData: Pointer);
begin
  FController.PostSuggestion(AText, AData);
end;

end.
