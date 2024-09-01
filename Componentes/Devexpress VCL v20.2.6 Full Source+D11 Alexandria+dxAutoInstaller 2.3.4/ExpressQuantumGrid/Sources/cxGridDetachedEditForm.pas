unit cxGridDetachedEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  cxEdit, cxGraphics, cxControls, dxCoreClasses, cxLookAndFeels,
  cxLookAndFeelPainters,
  cxClasses, dxLayoutContainer, dxLayoutControl, cxGridInplaceEditForm,
  dxLayoutLookAndFeels,
  StdCtrls, dxLayoutControlAdapters, Menus, cxButtons,
  cxGridViewLayoutContainer, cxGridCustomTableView,
  Generics.Collections;

type
  TcxGridDetachedEditFormLayoutItem = class; // for internal use only

  { TcxGridDetachedEditFormLayoutItemCaptionViewInfo }

  TcxGridDetachedEditFormLayoutItemCaptionViewInfo = class
    (TdxLayoutControlItemCaptionViewInfo) // for internal use only
  protected
    function CalculatePadding: TRect; override;
  end;

  { TcxGridDetachedEditFormLayoutItemControlViewInfo }

  TcxGridDetachedEditFormLayoutItemControlViewInfo = class
    (TdxLayoutItemControlViewInfo) // for internal use only
  strict private
    FDataHeight: Integer;

    function GetGridView: TcxCustomGridTableView; inline;
    function GetItem: TcxGridDetachedEditFormLayoutItem; inline;
  protected
    function GetDefaultValueHeight: Integer; virtual;
    function GetMinValueWidth: Integer; virtual;
    function GetOriginalControlSize: TSize; override;
    function GetValueHeight: Integer; virtual;

    property GridView: TcxCustomGridTableView read GetGridView;
    property Item: TcxGridDetachedEditFormLayoutItem read GetItem;
  public
    procedure Calculate(const ABounds: TRect); override;
    function CalculateMinHeight: Integer; override;
    function CalculateMinWidth: Integer; override;
  end;

  { TcxGridDetachedEditFormLayoutItemViewInfo }

  TcxGridDetachedEditFormLayoutItemViewInfo = class(TdxLayoutItemViewInfo)
  // for internal use only
  protected
    function GetCaptionViewInfoClass
      : TdxCustomLayoutItemCaptionViewInfoClass; override;
    function GetControlViewInfoClass
      : TdxLayoutControlItemControlViewInfoClass; override;
  end;

  { TcxGridDetachedEditFormLayoutItem }

  TcxGridDetachedEditFormLayoutItem = class(TdxLayoutItem)
  // for internal use only
  strict private
    FGridViewItem: TcxCustomGridTableItem;
    FMinValueWidth: Integer;
  protected
    function GetViewInfoClass: TdxCustomLayoutItemViewInfoClass; override;
  public
    procedure Assign(Source: TPersistent); override;

    property GridViewItem: TcxCustomGridTableItem read FGridViewItem;
    property MinValueWidth: Integer read FMinValueWidth;
  end;

  { TcxGridDetachedEditForm }

  TcxGridDetachedEditForm = class(TcxGridCustomDetachedEditForm)
  // for internal use only
    llflLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    llfMain: TdxLayoutCxLookAndFeel;
    lgRoot: TdxLayoutGroup;
    lcMainLayout: TdxLayoutControl;
    lgPanel: TdxLayoutGroup;
    btnUpdate: TcxButton;
    liUpdate: TdxLayoutItem;
    btnCancel: TcxButton;
    liCancel: TdxLayoutItem;
    lsiButtonsSeparator: TdxLayoutSeparatorItem;
    lgContentRoot: TdxLayoutGroup;
    lcContentLayout: TdxLayoutControl;
    liContentLayout: TdxLayoutItem;
    llfContent: TdxLayoutCxLookAndFeel;
    lgButtons: TdxLayoutGroup;
  strict private
    FEditList: TObjectList<TcxCustomEdit>;
  protected
    procedure AdjustOptimalHeight; virtual;
    procedure AdjustOptimalWidth; virtual;
    function CloneGridLayoutItem(AItem: TcxGridInplaceEditFormLayoutItem;
      AParent: TdxCustomLayoutGroup)
      : TcxGridDetachedEditFormLayoutItem; virtual;
    procedure CloneGroupStructure(AFrom, ATo: TdxCustomLayoutGroup); virtual;
    function CreateEdit(ALayoutItem: TcxGridDetachedEditFormLayoutItem)
      : TcxCustomEdit; virtual;
    function CreateLayoutItem(AParent: TdxCustomLayoutGroup)
      : TcxGridDetachedEditFormLayoutItem; virtual;
    procedure DoShow; override;
    function FindEditByGridViewItem(AGridViewItem: TcxCustomGridTableItem)
      : TcxCustomEdit; virtual;
    function GetActiveEdit: TcxCustomEdit; override;
    procedure Initialize(AEditForm: TcxGridInplaceEditForm); override;
    procedure ModifiedChanged; override;
    procedure InitImages; virtual;
    procedure InitStructure; virtual;
    procedure InitStyles; virtual;
    procedure UpdateButtonCaptions; virtual;
    procedure UpdateButtonEnabled; virtual;
    procedure UpdateButtonVisibility; virtual;
    procedure UpdateButtons; virtual;
    procedure UpdateEditValues(ANeedCallEvent: Boolean = False); virtual;
    procedure UpdateModified; virtual;
    procedure ValuesChanged; override;

    procedure EditChangedHandler(ASender: TObject); virtual;
    procedure EditDblClickHandler(Sender: TObject); virtual;
    procedure EditEditingHandler(ASender: TObject;
      var CanEdit: Boolean); virtual;
    procedure EditFocusChangedHandler(ASender: TObject); virtual;
    procedure EditKeyDownHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure EditKeyPressHandler(Sender: TObject; var Key: Char); virtual;
    procedure EditKeyUpHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure EditValueChangedHandler(ASender: TObject); virtual;

    property EditList: TObjectList<TcxCustomEdit> read FEditList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ShowModal: Integer; override;
  end;

implementation

{$R *.dfm}

uses
  Types, ImgList, dxCore, cxContainer, cxGeometry, cxGridStrs, dxDPIAwareUtils;

type
  { TcxCustomEditHelper }

  TcxCustomEditHelper = class helper for TcxCustomEdit
  strict private
    function GetGridViewItem: TcxCustomGridTableItem;
    function GetLayoutItem: TcxGridDetachedEditFormLayoutItem;
    procedure SetLayoutItem(AValue: TcxGridDetachedEditFormLayoutItem);
  public
    procedure AssignOverridableEventHandlers
      (AEditForm: TcxGridDetachedEditForm);
    procedure AssignStaticEventHandlers(AEditForm: TcxGridDetachedEditForm);
    procedure Init(AEditForm: TcxGridDetachedEditForm;
      ALayoutItem: TcxGridDetachedEditFormLayoutItem);
    procedure UpdateEditValue(ANeedCallEvent: Boolean);
    procedure UpdateGridViewItemEditValue;
    procedure UpdateGridViewItemFocus;

    property GridViewItem: TcxCustomGridTableItem read GetGridViewItem;
    property LayoutItem: TcxGridDetachedEditFormLayoutItem read GetLayoutItem;
  end;

  { TcxCustomGridTableItemHelper }

  TcxCustomGridTableItemHelper = class helper for TcxCustomGridTableItem
  public
    procedure InitEditStyle(AEdit: TcxCustomEdit);
    function PropertiesForEdit: TcxCustomEditProperties;
  end;

  { TcxCustomGridTableViewHelper }

  TcxCustomGridTableViewHelper = class helper for TcxCustomGridTableView
  public
    procedure EditChangedHandler(AItem: TcxCustomGridTableItem);
    procedure EditDblClickHandler(AItem: TcxCustomGridTableItem;
      AEdit: TcxCustomEdit);
    function EditingHandler(AItem: TcxCustomGridTableItem): Boolean;
    procedure EditKeyDownHandler(AItem: TcxCustomGridTableItem;
      AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
    procedure EditKeyPressHandler(AItem: TcxCustomGridTableItem;
      AEdit: TcxCustomEdit; var Key: Char);
    procedure EditKeyUpHandler(AItem: TcxCustomGridTableItem;
      AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
    procedure EditValueChangedHandler(AItem: TcxCustomGridTableItem);
    function GetImages: TCustomImageList;
    procedure InitEditHandler(AItem: TcxCustomGridTableItem;
      AEdit: TcxCustomEdit);
    procedure InitEditValueHandler(AItem: TcxCustomGridTableItem;
      AEdit: TcxCustomEdit; var AValue: TcxEditValue);
  end;

  { TcxCustomEditHelper }

procedure TcxCustomEditHelper.AssignOverridableEventHandlers
  (AEditForm: TcxGridDetachedEditForm);
begin
  OnDblClick := AEditForm.EditDblClickHandler;
  OnKeyDown := AEditForm.EditKeyDownHandler;
  OnKeyPress := AEditForm.EditKeyPressHandler;
  OnKeyUp := AEditForm.EditKeyUpHandler;
end;

procedure TcxCustomEditHelper.AssignStaticEventHandlers
  (AEditForm: TcxGridDetachedEditForm);
begin
  OnEditing := AEditForm.EditEditingHandler;
  Properties.OnEditValueChanged := AEditForm.EditValueChangedHandler;
  Properties.OnChange := AEditForm.EditChangedHandler;
  OnFocusChanged := AEditForm.EditFocusChangedHandler;
end;

procedure TcxCustomEditHelper.Init(AEditForm: TcxGridDetachedEditForm;
  ALayoutItem: TcxGridDetachedEditFormLayoutItem);
begin
  AssignOverridableEventHandlers(AEditForm);
  SetLayoutItem(ALayoutItem);
  Properties := GridViewItem.GetProperties;
  GridViewItem.InitEditStyle(Self);
  AutoSize := False;
  Style.TransparentBorder := False;
  Properties.ReadOnly := not GridViewItem.GridView.EditingHandler(GridViewItem);
  GridViewItem.GridView.InitEditHandler(GridViewItem, Self);
  UpdateEditValue(True);
  AssignStaticEventHandlers(AEditForm);
end;

procedure TcxCustomEditHelper.UpdateEditValue(ANeedCallEvent: Boolean);
var
  AEditValue: Variant;
begin
  LockChangeEvents(True);
  try
    AEditValue := GridViewItem.EditValue;
    if ANeedCallEvent then
      GridViewItem.GridView.InitEditValueHandler(GridViewItem, Self,
        AEditValue);
    EditValue := AEditValue;
  finally
    LockChangeEvents(False, False);
  end;
end;

procedure TcxCustomEditHelper.UpdateGridViewItemEditValue;
begin
  GridViewItem.EditValue := EditValue;
end;

procedure TcxCustomEditHelper.UpdateGridViewItemFocus;
begin
  if IsFocused then
    GridViewItem.Focused := True;
end;

function TcxCustomEditHelper.GetGridViewItem: TcxCustomGridTableItem;
begin
  Result := LayoutItem.GridViewItem;
end;

function TcxCustomEditHelper.GetLayoutItem: TcxGridDetachedEditFormLayoutItem;
begin
  Result := TcxGridDetachedEditFormLayoutItem(Tag);
end;

procedure TcxCustomEditHelper.SetLayoutItem
  (AValue: TcxGridDetachedEditFormLayoutItem);
begin
  Tag := NativeInt(AValue);
end;

{ TcxCustomGridTableItemHelper }

procedure TcxCustomGridTableItemHelper.InitEditStyle(AEdit: TcxCustomEdit);
var
  AFont: TFont;
  AParams: TcxViewParams;
begin
  AFont := TFont.Create;
  try
    GridView.Styles.GetDataCellParams(GridView.Controller.FocusedRecord, Self,
      AParams, False, nil, True);
    AFont.Assign(AParams.Font);
    AFont.Height := dxGetScaleFactor(AEdit).Apply(AFont.Height,
      dxGetScaleFactor(GridView));
    AParams.Font := AFont;
    InitStyle(AEdit.Style, AParams, False);
    InitStyle(AEdit.StyleFocused, AParams, False);
  finally
    AFont.Free;
  end;
end;

function TcxCustomGridTableItemHelper.PropertiesForEdit
  : TcxCustomEditProperties;
begin
  Result := GetPropertiesForEdit;
end;

{ TcxCustomGridTableViewHelper }

procedure TcxCustomGridTableViewHelper.EditChangedHandler
  (AItem: TcxCustomGridTableItem);
begin
  DoEditChanged(AItem);
end;

procedure TcxCustomGridTableViewHelper.EditDblClickHandler
  (AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
begin
  DoEditDblClick(AItem, AEdit);
end;

function TcxCustomGridTableViewHelper.EditingHandler
  (AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := DoEditing(AItem);
end;

procedure TcxCustomGridTableViewHelper.EditKeyDownHandler
  (AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Word;
  Shift: TShiftState);
begin
  DoEditKeyDown(AItem, AEdit, Key, Shift);
end;

procedure TcxCustomGridTableViewHelper.EditKeyPressHandler
  (AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Char);
begin
  DoEditKeyPress(AItem, AEdit, Key);
end;

procedure TcxCustomGridTableViewHelper.EditKeyUpHandler
  (AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Word;
  Shift: TShiftState);
begin
  DoEditKeyUp(AItem, AEdit, Key, Shift);
end;

procedure TcxCustomGridTableViewHelper.EditValueChangedHandler
  (AItem: TcxCustomGridTableItem);
begin
  DoEditValueChanged(AItem);
end;

function TcxCustomGridTableViewHelper.GetImages: TCustomImageList;
begin
  Result := Images;
end;

procedure TcxCustomGridTableViewHelper.InitEditHandler
  (AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
begin
  DoInitEdit(AItem, AEdit);
end;

procedure TcxCustomGridTableViewHelper.InitEditValueHandler
  (AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit;
  var AValue: TcxEditValue);
begin
  DoInitEditValue(AItem, AEdit, AValue);
end;

{ TcxGridDetachedEditFormLayoutItemCaptionViewInfo }

function TcxGridDetachedEditFormLayoutItemCaptionViewInfo.
  CalculatePadding: TRect;
begin
  Result := cxRectInflate(inherited CalculatePadding, -1);
end;

{ TcxGridDetachedEditFormLayoutItemControlViewInfo }

procedure TcxGridDetachedEditFormLayoutItemControlViewInfo.Calculate
  (const ABounds: TRect);
begin
  inherited Calculate(cxRectInflate(ABounds, cxGridLayoutItemCellBorderWidth));
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.
  CalculateMinHeight: Integer;
begin
  if Item.ControlOptions.IsHeightUsual then
    Result := CalculateHeight
  else if Visible then
    Result := GetDefaultValueHeight
  else
    Result := 0;
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.
  CalculateMinWidth: Integer;
begin
  if Item.ControlOptions.IsWidthUsual then
    Result := CalculateWidth
  else if Visible then
    Result := GetControlAreaWidth(GetMinValueWidth)
  else
    Result := 0;
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.
  GetDefaultValueHeight: Integer;
var
  AFont: TFont;
  AParams: TcxViewParams;
begin
  AFont := TFont.Create;
  try
    GridView.Styles.GetContentParams(GridView.ViewData.EditingRecord,
      Item.GridViewItem, AParams);
    AFont.Assign(AParams.Font);
    AFont.Height := Item.ScaleFactor.Apply(AFont.Height,
      dxGetScaleFactor(GridView));
    Result := Item.GridViewItem.CalculateDefaultCellHeight
      (cxScreenCanvas, AFont);
  finally
    AFont.Free;
  end;
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.
  GetMinValueWidth: Integer;
begin
  Result := Item.MinValueWidth;
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.
  GetOriginalControlSize: TSize;
begin
  Result := cxSize(GetMinValueWidth, GetValueHeight);
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.
  GetValueHeight: Integer;
begin
  if FDataHeight = 0 then
  begin
    FDataHeight := GetDefaultValueHeight;
    dxAdjustToTouchableSize(FDataHeight);
  end;
  Result := FDataHeight;
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.GetGridView
  : TcxCustomGridTableView;
begin
  Result := Item.GridViewItem.GridView;
end;

function TcxGridDetachedEditFormLayoutItemControlViewInfo.GetItem
  : TcxGridDetachedEditFormLayoutItem;
begin
  Result := TcxGridDetachedEditFormLayoutItem(inherited Item);
end;

{ TcxGridDetachedEditFormLayoutItemViewInfo }

function TcxGridDetachedEditFormLayoutItemViewInfo.GetCaptionViewInfoClass
  : TdxCustomLayoutItemCaptionViewInfoClass;
begin
  Result := TcxGridDetachedEditFormLayoutItemCaptionViewInfo;
end;

function TcxGridDetachedEditFormLayoutItemViewInfo.GetControlViewInfoClass
  : TdxLayoutControlItemControlViewInfoClass;
begin
  Result := TcxGridDetachedEditFormLayoutItemControlViewInfo;
end;

{ TcxGridDetachedEditFormLayoutItem }

procedure TcxGridDetachedEditFormLayoutItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridDetachedEditFormLayoutItem then
  begin
    FGridViewItem := TcxGridDetachedEditFormLayoutItem(Source).GridViewItem;
    FMinValueWidth := TcxGridDetachedEditFormLayoutItem(Source).MinValueWidth;
  end
  else if Source is TcxGridInplaceEditFormLayoutItem then
  begin
    FGridViewItem := TcxGridInplaceEditFormLayoutItem(Source).GridViewItem;
    FMinValueWidth := TcxGridInplaceEditFormLayoutItem(Source).MinValueWidth;
  end;
end;

function TcxGridDetachedEditFormLayoutItem.GetViewInfoClass
  : TdxCustomLayoutItemViewInfoClass;
begin
  Result := TcxGridDetachedEditFormLayoutItemViewInfo;
end;

{ TcxGridDetachedEditForm }

constructor TcxGridDetachedEditForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditList := TObjectList<TcxCustomEdit>.Create;
end;

destructor TcxGridDetachedEditForm.Destroy;
begin
  FreeAndNil(FEditList);
  inherited Destroy;
end;

function TcxGridDetachedEditForm.ShowModal: Integer;
begin
  Result := inherited ShowModal;
  case Result of
    mrCancel:
      EditForm.CancelExecute;
    mrOk:
      begin
        UpdateEditValues;
        EditForm.UpdateExecute;
      end;
  end;
end;

procedure TcxGridDetachedEditForm.AdjustOptimalHeight;
var
  AAlignVert: TdxLayoutAlignVert;
begin
  AAlignVert := liContentLayout.AlignVert;
  try
    liContentLayout.AlignVert := avTop;
    lcContentLayout.ClientHeight := lcContentLayout.Container.ViewInfo.
      ItemsViewInfo.MinHeight;
    ClientHeight := lcMainLayout.Container.ViewInfo.ItemsViewInfo.MinHeight;
  finally
    liContentLayout.AlignVert := AAlignVert;
  end;
end;

procedure TcxGridDetachedEditForm.AdjustOptimalWidth;
var
  AAlignHorz: TdxLayoutAlignHorz;
begin
  AAlignHorz := liContentLayout.AlignHorz;
  try
    liContentLayout.AlignHorz := ahLeft;
    lcContentLayout.ClientWidth := lcContentLayout.Container.ViewInfo.
      ItemsViewInfo.MinWidth;
    ClientWidth := lcMainLayout.Container.ViewInfo.ItemsViewInfo.MinWidth;
  finally
    liContentLayout.AlignHorz := AAlignHorz;
  end;
end;

function TcxGridDetachedEditForm.CloneGridLayoutItem
  (AItem: TcxGridInplaceEditFormLayoutItem; AParent: TdxCustomLayoutGroup)
  : TcxGridDetachedEditFormLayoutItem;
begin
  Result := CreateLayoutItem(AParent);
  Result.Assign(AItem);
  Result.Enabled := Result.GridViewItem.Options.Focusing;
  Result.Control := CreateEdit(Result);
end;

procedure TcxGridDetachedEditForm.CloneGroupStructure(AFrom,
  ATo: TdxCustomLayoutGroup);
var
  I: Integer;
  AItem, ASourceItem: TdxCustomLayoutItem;
begin
  for I := 0 to AFrom.Count - 1 do
  begin
    ASourceItem := AFrom.Items[I];
    if ASourceItem is TcxGridInplaceEditFormLayoutItem then
      CloneGridLayoutItem(TcxGridInplaceEditFormLayoutItem(ASourceItem), ATo)
    else
    begin
      AItem := ATo.Container.CloneItem(ASourceItem, ATo);
      if AItem is TdxCustomLayoutGroup then
        CloneGroupStructure(TdxCustomLayoutGroup(ASourceItem),
          TdxCustomLayoutGroup(AItem));
    end;
  end;
end;

function TcxGridDetachedEditForm.CreateEdit(ALayoutItem
  : TcxGridDetachedEditFormLayoutItem): TcxCustomEdit;
var
  AEditClass: TcxCustomEditClass;
  AProperties: TcxCustomEditProperties;
  APropertiesClass: TcxCustomEditPropertiesClass;
begin
  AProperties := ALayoutItem.GridViewItem.PropertiesForEdit;
  APropertiesClass := TcxCustomEditPropertiesClass(AProperties.ClassType);
  AEditClass := TcxCustomEditClass(APropertiesClass.GetContainerClass);
  Result := AEditClass.Create(nil);
  Result.Init(Self, ALayoutItem);
  EditList.Add(Result);
end;

function TcxGridDetachedEditForm.CreateLayoutItem(AParent: TdxCustomLayoutGroup)
  : TcxGridDetachedEditFormLayoutItem;
begin
  Result := TcxGridDetachedEditFormLayoutItem
    (AParent.CreateItem(TcxGridDetachedEditFormLayoutItem));
end;

procedure TcxGridDetachedEditForm.DoShow;
begin
  if UseDefaultHeight then
    AdjustOptimalHeight;
  if UseDefaultWidth then
    AdjustOptimalWidth;
  inherited DoShow;
  UpdateButtons;
end;

function TcxGridDetachedEditForm.FindEditByGridViewItem(AGridViewItem
  : TcxCustomGridTableItem): TcxCustomEdit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to EditList.Count - 1 do
    if EditList[I].GridViewItem = AGridViewItem then
      Exit(EditList[I]);
end;

function TcxGridDetachedEditForm.GetActiveEdit: TcxCustomEdit;
begin
  Result := FindEditByGridViewItem(GridView.Controller.FocusedItem);
end;

procedure TcxGridDetachedEditForm.Initialize(AEditForm: TcxGridInplaceEditForm);
begin
  inherited Initialize(AEditForm);
  lcMainLayout.BeginUpdate;
  try
    lcMainLayout.OptionsImage.Images := EditForm.GridView.GetImages;
    lcContentLayout.BeginUpdate;
    try
      InitImages;
      InitStyles;
      InitStructure;
    finally
      lcContentLayout.EndUpdate;
    end;
  finally
    lcMainLayout.EndUpdate;
  end;
end;

procedure TcxGridDetachedEditForm.ModifiedChanged;
begin
  UpdateButtons;
end;

procedure TcxGridDetachedEditForm.InitImages;
begin
  lcContentLayout.OptionsImage.Images := EditForm.GridView.GetImages;
end;

procedure TcxGridDetachedEditForm.InitStructure;
begin
  lgContentRoot.Assign(EditForm.Container.Root);
  CloneGroupStructure(EditForm.Container.Root, lgContentRoot);
end;

procedure TcxGridDetachedEditForm.InitStyles;

  procedure AdjustFontSize(AOptions: TdxLayoutLookAndFeelCaptionOptions);
  begin
    AOptions.Font.Height := ScaleFactor.Apply(AOptions.Font.Height,
      dxGetScaleFactor(GridView));
  end;

begin
  llfMain.LookAndFeel.MasterLookAndFeel :=
    EditForm.LayoutLookAndFeel.LookAndFeel.MasterLookAndFeel;

  llfContent.Assign(EditForm.LayoutLookAndFeel);
  dxGetScaleFactor(llfContent).Assign(ScaleFactor);
  AdjustFontSize(llfContent.ItemOptions.CaptionOptions);
  AdjustFontSize(llfContent.GroupOptions.CaptionOptions);
  llfContent.Offsets.RootItemsAreaOffsetHorz :=
    EditForm.LayoutLookAndFeel.Offsets.RootItemsAreaOffsetHorz;
  llfContent.Offsets.RootItemsAreaOffsetVert :=
    EditForm.LayoutLookAndFeel.Offsets.RootItemsAreaOffsetVert;

  lgPanel.Offsets.Left := llfContent.GetRootItemsAreaOffsetHorz
    (lcContentLayout.Container);
  lgPanel.Offsets.Right := lgPanel.Offsets.Left;
end;

procedure TcxGridDetachedEditForm.UpdateButtonCaptions;
begin
  btnUpdate.Caption := cxGetResourceString(@scxGridInplaceEditFormButtonUpdate);
  if EditForm.IsUpdateButtonVisible then
    btnCancel.Caption := cxGetResourceString
      (@scxGridInplaceEditFormButtonCancel)
  else
    btnCancel.Caption := cxGetResourceString
      (@scxGridInplaceEditFormButtonClose);
end;

procedure TcxGridDetachedEditForm.UpdateButtonEnabled;
begin
  liUpdate.Enabled := EditForm.Modified;
end;

procedure TcxGridDetachedEditForm.UpdateButtonVisibility;
begin
  liUpdate.Visible := EditForm.IsUpdateButtonVisible;
end;

procedure TcxGridDetachedEditForm.UpdateButtons;
begin
  UpdateButtonCaptions;
  UpdateButtonEnabled;
  UpdateButtonVisibility;
end;

procedure TcxGridDetachedEditForm.UpdateEditValues(ANeedCallEvent
  : Boolean = False);
var
  I: Integer;
begin
  for I := 0 to EditList.Count - 1 do
    EditList[I].UpdateEditValue(ANeedCallEvent);
end;

procedure TcxGridDetachedEditForm.UpdateModified;
begin
  EditForm.UpdateModified;
  UpdateButtonEnabled;
end;

procedure TcxGridDetachedEditForm.ValuesChanged;
begin
  UpdateEditValues(True);
end;

procedure TcxGridDetachedEditForm.EditChangedHandler(ASender: TObject);
var
  AEdit: TcxCustomEdit absolute ASender;
begin
  UpdateModified;
  GridView.EditChangedHandler(AEdit.GridViewItem);
end;

procedure TcxGridDetachedEditForm.EditDblClickHandler(Sender: TObject);
var
  AEdit: TcxCustomEdit absolute Sender;
begin
  GridView.EditDblClickHandler(AEdit.GridViewItem, AEdit);
end;

procedure TcxGridDetachedEditForm.EditEditingHandler(ASender: TObject;
  var CanEdit: Boolean);
var
  AEdit: TcxCustomEdit absolute ASender;
begin
  CanEdit := GridView.DataController.CanInitEditing(AEdit.GridViewItem.Index)
    and AEdit.GridViewItem.Editable and
    not GridView.DataController.IsItemExpression(AEdit.GridViewItem.Index);
end;

procedure TcxGridDetachedEditForm.EditFocusChangedHandler(ASender: TObject);
var
  AEdit: TcxCustomEdit absolute ASender;
begin
  AEdit.UpdateGridViewItemFocus;
end;

procedure TcxGridDetachedEditForm.EditKeyDownHandler(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  AEdit: TcxCustomEdit absolute Sender;
begin
  GridView.EditKeyDownHandler(AEdit.GridViewItem, AEdit, Key, Shift);
end;

procedure TcxGridDetachedEditForm.EditKeyPressHandler(Sender: TObject;
  var Key: Char);
var
  AEdit: TcxCustomEdit absolute Sender;
begin
  GridView.EditKeyPressHandler(AEdit.GridViewItem, AEdit, Key);
end;

procedure TcxGridDetachedEditForm.EditKeyUpHandler(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  AEdit: TcxCustomEdit absolute Sender;
begin
  GridView.EditKeyUpHandler(AEdit.GridViewItem, AEdit, Key, Shift);
end;

procedure TcxGridDetachedEditForm.EditValueChangedHandler(ASender: TObject);
var
  AEdit: TcxCustomEdit absolute ASender;
begin
  AEdit.UpdateGridViewItemEditValue;
  UpdateEditValues;
  UpdateButtonEnabled;
  GridView.EditValueChangedHandler(AEdit.GridViewItem);
end;

end.
