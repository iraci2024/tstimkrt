{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressQuantumGrid }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL }
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

unit cxGridInplaceEditForm;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Graphics, Controls, Contnrs, ImgList, StdCtrls,
  dxCore, cxClasses, cxGraphics, cxControls, cxStyles, cxLookAndFeelPainters,
  cxGridCommon, cxGrid, dxCoreClasses, dxGDIPlusClasses,
  cxGridCustomView, cxGridCustomTableView, dxLayoutLookAndFeels, cxDataStorage,
  cxCustomData, cxEdit, dxLayoutContainer, dxLayoutSelection,
  dxLayoutCommon, Forms, cxNavigator, cxLookAndFeels, SysUtils,
  cxGridViewLayoutContainer, cxMaskEdit, dxForms, cxGridRowLayout;

type
  TcxGridInplaceEditFormStretch = TcxGridRowLayoutStretch;

const
  fsNone = TcxGridInplaceEditFormStretch.fsNone;
  fsHorizontal = TcxGridInplaceEditFormStretch.fsHorizontal;
  fsVertical = TcxGridInplaceEditFormStretch.fsVertical;
  fsClient = TcxGridInplaceEditFormStretch.fsClient;

type
  TcxGridInplaceEditFormContainer = class;
  TcxGridInplaceEditForm = class;
  TcxGridInplaceEditFormClass = class of TcxGridInplaceEditForm;

  TcxGridEditFormDisplayMode = (efdmInplace, efdmModal);
  // for internal use only

  { TcxGridInplaceEditFormLayoutItemCaptionOptions }

  TcxGridInplaceEditFormLayoutItemCaptionOptions = class
    (TcxGridRowLayoutItemCaptionOptions);

  { TcxGridInplaceEditFormLayoutItem }

  TcxGridInplaceEditFormLayoutItem = class(TcxGridRowLayoutItem)
  protected
    class function GetCaptionOptionsClass
      : TdxCustomLayoutItemCaptionOptionsClass; override;
  end;

  { TcxGridInplaceEditFormGroup }

  TcxGridInplaceEditFormGroup = class(TdxLayoutGroup);

  { TcxGridInplaceEditFormContainerViewInfo }

  TcxGridInplaceEditFormContainerViewInfo = class
    (TcxGridRowLayoutContainerViewInfo)
  strict private
    function GetContainer: TcxGridInplaceEditFormContainer;
    function GetInplaceEditForm: TcxGridInplaceEditForm;
  protected
    function GetRecordViewInfoInstance: TcxCustomGridRecordViewInfo; override;

    property InplaceEditForm: TcxGridInplaceEditForm read GetInplaceEditForm;
  public
    property Container: TcxGridInplaceEditFormContainer read GetContainer;
  end;

  { TcxGridInplaceEditFormContainer }

  TcxGridInplaceEditFormContainer = class(TcxGridRowLayoutContainer)
  strict private
    function GetInplaceEditForm: TcxGridInplaceEditForm;
  protected
    function GetDefaultGroupClass: TdxLayoutGroupClass; override;
    function GetItemClass: TcxGridRowLayoutItemClass; override;
    function GetViewInfoClass: TdxLayoutContainerViewInfoClass; override;
    procedure LayoutChanged(ANeedPack: Boolean = True); override;

    property InplaceEditForm: TcxGridInplaceEditForm read GetInplaceEditForm;
  public
    function CreateItem: TcxGridInplaceEditFormLayoutItem;
  end;

  { TcxGridCustomDetachedEditFormMetricsInfo }

  TcxGridCustomDetachedEditFormMetricsInfo = class(TcxDialogMetricsInfo)
  strict private
    FUseDefaultHeight: Boolean;
    FUseDefaultWidht: Boolean;
  public
    property UseDefaultHeight: Boolean read FUseDefaultHeight
      write FUseDefaultHeight;
    property UseDefaultWidht: Boolean read FUseDefaultWidht
      write FUseDefaultWidht;
  end;

  { TcxGridCustomDetachedEditForm }

  TcxGridCustomDetachedEditForm = class(TdxForm) // for internal use only
  strict private
  const
    ItemReferenceStartMark = '[';
    ItemReferenceFinishMark = ']';
  strict private
    FEditForm: TcxGridInplaceEditForm;
    FShowed: Boolean;
    FUseDefaultHeight: Boolean;
    FUseDefaultWidth: Boolean;

    function GetGridView: TcxCustomGridTableView;
  protected
    function BuildCaption(const ACaptionMask: string): string; virtual;
    procedure DoShow; override;
    procedure Finalize(AEditForm: TcxGridInplaceEditForm); virtual;
    function GetActiveEdit: TcxCustomEdit; virtual; abstract;
    function GetItemValue(const AItemCaption: string; out AValue: string)
      : Boolean; virtual;
    procedure Initialize(AEditForm: TcxGridInplaceEditForm); virtual;
    procedure ModifiedChanged; virtual; abstract;
    procedure Resize; override;
    procedure ValuesChanged; virtual; abstract;

    property ActiveEdit: TcxCustomEdit read GetActiveEdit;
    property EditForm: TcxGridInplaceEditForm read FEditForm;
    property GridView: TcxCustomGridTableView read GetGridView;
  public
    property UseDefaultHeight: Boolean read FUseDefaultHeight
      write FUseDefaultHeight;
    property UseDefaultWidth: Boolean read FUseDefaultWidth
      write FUseDefaultWidth;
  end;

  { TcxGridInplaceEditForm }

  TcxGridInplaceEditForm = class(TcxGridRowLayoutController)
  private
    FCaptionMask: string;
    FDetachedForm: TcxGridCustomDetachedEditForm;
    FDetachedFormMetrics: TcxGridCustomDetachedEditFormMetricsInfo;
    FDisplayMode: TcxGridEditFormDisplayMode;
    FEditingRecordIndex: Integer;
    FModified: Boolean;

    function GetContainer: TcxGridInplaceEditFormContainer;
    procedure SetCaptionMask(const AValue: string);
    procedure SetEditingRecordIndex(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure AdjustEditingItemOnVisibilityChange; virtual;
    function CreateDetachedForm: TcxGridCustomDetachedEditForm; virtual;
    procedure DetachedFormFinalize; virtual;
    procedure DetachedFormInitialize; virtual;
    function GetActiveEdit: TcxCustomEdit; virtual;
    function GetContainerClass: TcxGridRowLayoutContainerClass; override;
    function GetModified: Boolean; virtual;
    function GetResetDefaultLayoutQuery: string; override;
    function GetVisible: Boolean; virtual;
    procedure ShowModal; virtual;
    procedure ModifiedChanged; virtual;
    procedure UpdateInplacePosition; virtual;
    procedure ValidateFocusedItem; virtual;
    procedure ValuesChanged; virtual;

    function CloseOnRecordInserting: Boolean; virtual;
    procedure CloseOnFocusedRecordChanging(var AFocusingRecord
      : TcxCustomGridRecord); virtual;
    function CloseQuery(ARaiseAbortOnCancel: Boolean = True): TModalResult;
    procedure ResetEditingRecordIndex; virtual;

    procedure CreateDetachedFormMetrics; virtual;
    procedure ResetDetachedFormSize; virtual;
    procedure RestoreDetachedFormMetrics; virtual;
    procedure StoreDetachedFormMetrics; virtual;
    function UseDetachedFormDefaultHeight: Boolean; virtual;
    function UseDetachedFormDefaultWidth: Boolean; virtual;

    property ActiveEdit: TcxCustomEdit read GetActiveEdit;
    property CaptionMask: string read FCaptionMask write SetCaptionMask;
    property DetachedForm: TcxGridCustomDetachedEditForm read FDetachedForm;
    property DetachedFormMetrics: TcxGridCustomDetachedEditFormMetricsInfo
      read FDetachedFormMetrics;
    property DisplayMode: TcxGridEditFormDisplayMode read FDisplayMode
      write FDisplayMode;
    property EditingRecordIndex: Integer read FEditingRecordIndex
      write SetEditingRecordIndex;
  public
    constructor Create(AGridView: TcxCustomGridTableView); reintroduce; virtual;
    destructor Destroy; override;

    function Close(ARaiseAbortOnCancel: Boolean = True): Boolean; virtual;
    function IsUpdateButtonVisible: Boolean; virtual;
    procedure UpdateModified; virtual;

    procedure CancelExecute;
    procedure UpdateExecute;

    property Container: TcxGridInplaceEditFormContainer read GetContainer;
    property Modified: Boolean read FModified;
    property Visible: Boolean read GetVisible;
  end;

implementation

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  RTLConsts, cxGeometry, cxContainer, Dialogs, cxGridStrs,
  cxGridDetachedEditForm, dxThreading;

type
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TcxControlAccess = class(TcxControl);
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TcxCustomGridTableViewAccess = class(TcxCustomGridTableView);

  { TcxGridInplaceEditFormLayoutItem }

class function TcxGridInplaceEditFormLayoutItem.GetCaptionOptionsClass
  : TdxCustomLayoutItemCaptionOptionsClass;
begin
  Result := TcxGridInplaceEditFormLayoutItemCaptionOptions;
end;

{ TcxGridInplaceEditFormContainerViewInfo }

function TcxGridInplaceEditFormContainerViewInfo.GetRecordViewInfoInstance
  : TcxCustomGridRecordViewInfo;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := GridView.ViewData.GetRecordByIndex
    (InplaceEditForm.EditingRecordIndex);
  if (ARecord <> nil) then
  begin
    Result := ARecord.ViewInfo;
    if (Result <> nil) and not Result.Calculated then
      Result := nil;
  end
  else
    Result := inherited GetRecordViewInfoInstance;
end;

function TcxGridInplaceEditFormContainerViewInfo.GetContainer
  : TcxGridInplaceEditFormContainer;
begin
  Result := TcxGridInplaceEditFormContainer(inherited Container);
end;

function TcxGridInplaceEditFormContainerViewInfo.GetInplaceEditForm
  : TcxGridInplaceEditForm;
begin
  Result := Container.InplaceEditForm;
end;

{ TcxGridInplaceEditFormContainer }

function TcxGridInplaceEditFormContainer.CreateItem
  : TcxGridInplaceEditFormLayoutItem;
begin
  Result := TcxGridInplaceEditFormLayoutItem(inherited CreateItem);
end;

function TcxGridInplaceEditFormContainer.GetDefaultGroupClass
  : TdxLayoutGroupClass;
begin
  Result := TcxGridInplaceEditFormGroup;
end;

function TcxGridInplaceEditFormContainer.GetItemClass
  : TcxGridRowLayoutItemClass;
begin
  Result := TcxGridInplaceEditFormLayoutItem;
end;

function TcxGridInplaceEditFormContainer.GetViewInfoClass
  : TdxLayoutContainerViewInfoClass;
begin
  Result := TcxGridInplaceEditFormContainerViewInfo;
end;

procedure TcxGridInplaceEditFormContainer.LayoutChanged
  (ANeedPack: Boolean = True);
begin
  inherited LayoutChanged(ANeedPack);
  InplaceEditForm.ResetDetachedFormSize;
end;

function TcxGridInplaceEditFormContainer.GetInplaceEditForm
  : TcxGridInplaceEditForm;
begin
  Result := TcxGridInplaceEditForm(LayoutController);
end;

{ TcxGridCustomDetachedEditForm }

function TcxGridCustomDetachedEditForm.BuildCaption(const ACaptionMask
  : string): string;
var
  I: Integer;
  AChar: Char;
  AValue: string;
  AItemCaption: string;
  AIsItemCaptionParsing: Boolean;
begin
  Result := '';
  AItemCaption := '';
  AIsItemCaptionParsing := False;
  for I := 1 to Length(ACaptionMask) do
  begin
    AChar := ACaptionMask[I];
    case AChar of
      ItemReferenceStartMark:
        begin
          if AIsItemCaptionParsing then
          begin
            Result := Result + ItemReferenceStartMark + AItemCaption;
            AItemCaption := '';
          end;
          AIsItemCaptionParsing := True;
        end;
      ItemReferenceFinishMark:
        if AIsItemCaptionParsing then
        begin
          if not GetItemValue(AItemCaption, AValue) then
            AValue := ItemReferenceStartMark + AItemCaption +
              ItemReferenceFinishMark;
          Result := Result + AValue;
          AItemCaption := '';
          AIsItemCaptionParsing := False;
        end
        else
          Result := Result + AChar;
    else
      if AIsItemCaptionParsing then
        AItemCaption := AItemCaption + AChar
      else
        Result := Result + AChar;
    end;
  end;
  if AIsItemCaptionParsing then
    Result := Result + ItemReferenceStartMark + AItemCaption;
end;

procedure TcxGridCustomDetachedEditForm.DoShow;
begin
  inherited DoShow;
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, procedure()
    begin
      FShowed := True;
    end);
end;

procedure TcxGridCustomDetachedEditForm.Finalize
  (AEditForm: TcxGridInplaceEditForm);
begin
  AEditForm.StoreDetachedFormMetrics;
end;

function TcxGridCustomDetachedEditForm.GetItemValue(const AItemCaption: string;
out AValue: string): Boolean;
var
  I: Integer;
  AItem: TcxCustomGridTableItem;
begin
  AValue := '';
  Result := False;
  for I := 0 to GridView.ItemCount - 1 do
  begin
    AItem := GridView.Items[I];
    Result := AnsiSameText(AItemCaption, AItem.Caption);
    if Result then
    begin
      AValue := GridView.Controller.FocusedRecord.DisplayTexts[AItem.Index];
      Break;
    end;
  end;
end;

procedure TcxGridCustomDetachedEditForm.Initialize
  (AEditForm: TcxGridInplaceEditForm);
begin
  FEditForm := AEditForm;
  Caption := BuildCaption(EditForm.CaptionMask);
  BiDiMode := EditForm.GridView.Site.BiDiMode;
  AEditForm.RestoreDetachedFormMetrics;
  if UseDefaultHeight then
    Height := 0;
  if UseDefaultWidth then
    Width := 0;
end;

procedure TcxGridCustomDetachedEditForm.Resize;
begin
  inherited Resize;
  if FShowed then
  begin
    UseDefaultHeight := False;
    UseDefaultWidth := False;
  end;
end;

function TcxGridCustomDetachedEditForm.GetGridView: TcxCustomGridTableView;
begin
  Result := EditForm.GridView;
end;

{ TcxGridInplaceEditForm }

constructor TcxGridInplaceEditForm.Create(AGridView: TcxCustomGridTableView);
begin
  inherited Create(AGridView);
  FEditingRecordIndex := cxNullEditingRecordIndex;
end;

destructor TcxGridInplaceEditForm.Destroy;
begin
  FreeAndNil(FDetachedFormMetrics);
  inherited Destroy;
end;

function TcxGridInplaceEditForm.Close(ARaiseAbortOnCancel
  : Boolean = True): Boolean;
begin
  if GridView.DataController.IsEditing and
    (dceModified in GridView.DataController.EditState) then
    case CloseQuery(ARaiseAbortOnCancel) of
      mrYes:
        GridView.DataController.Post(True);
      mrNo:
        GridView.DataController.Cancel;
    end
  else
  begin
    ResetEditingRecordIndex;
    GridView.DataController.Cancel;
  end;
  Result := not Visible;
end;

function TcxGridInplaceEditForm.IsUpdateButtonVisible: Boolean;
begin
  Result := GridView.OptionsData.Editing;
end;

procedure TcxGridInplaceEditForm.UpdateModified;
var
  AModified: Boolean;
begin
  AModified := GetModified;
  if Modified <> AModified then
  begin
    FModified := AModified;
    ModifiedChanged;
  end;
end;

procedure TcxGridInplaceEditForm.CancelExecute;
begin
  if not GridView.DataController.IsEditing or (DisplayMode = efdmModal) then
    ResetEditingRecordIndex;
  GridView.DataController.Cancel;
end;

procedure TcxGridInplaceEditForm.UpdateExecute;
begin
  if not GridView.DataController.IsEditing or (DisplayMode = efdmModal) then
    ResetEditingRecordIndex;
  GridView.DataController.Post(True);
end;

procedure TcxGridInplaceEditForm.AdjustEditingItemOnVisibilityChange;
begin
  if Visible then
    GridView.Controller.EditingItem := GridView.Controller.FocusedItem
  else
    GridView.Controller.EditingController.HideEdit(False);
end;

function TcxGridInplaceEditForm.CreateDetachedForm
  : TcxGridCustomDetachedEditForm;
begin
  Result := TcxGridDetachedEditForm.Create(nil);
end;

procedure TcxGridInplaceEditForm.DetachedFormFinalize;
begin
  DetachedForm.Finalize(Self);
end;

procedure TcxGridInplaceEditForm.DetachedFormInitialize;
begin
  DetachedForm.Initialize(Self);
end;

function TcxGridInplaceEditForm.GetActiveEdit: TcxCustomEdit;
begin
  if DisplayMode = efdmModal then
    if DetachedForm <> nil then
      Result := DetachedForm.ActiveEdit
    else
      Result := nil
  else
    Result := GridView.Controller.EditingController.Edit;
end;

function TcxGridInplaceEditForm.GetContainerClass
  : TcxGridRowLayoutContainerClass;
begin
  Result := TcxGridInplaceEditFormContainer;
end;

function TcxGridInplaceEditForm.GetModified: Boolean;
begin
  Result := Visible and ((dceModified in GridView.DataController.EditState) or
    (ActiveEdit <> nil) and ActiveEdit.ModifiedAfterEnter);
end;

function TcxGridInplaceEditForm.GetResetDefaultLayoutQuery: string;
begin
  Result := GetResetCustomSettingLayoutQueryStr('Edit Form');
end;

function TcxGridInplaceEditForm.GetVisible: Boolean;
begin
  Result := Active;
end;

procedure TcxGridInplaceEditForm.ShowModal;
begin
  FDetachedForm := CreateDetachedForm;
  try
    DetachedFormInitialize;
    DetachedForm.ShowModal;
    DetachedFormFinalize;
  finally
    FreeAndNil(FDetachedForm);
  end;
end;

procedure TcxGridInplaceEditForm.ModifiedChanged;
begin
  if DetachedForm <> nil then
    DetachedForm.ModifiedChanged;
end;

procedure TcxGridInplaceEditForm.UpdateInplacePosition;
begin
  Changed(True);
  ValidateFocusedItem;
  AdjustEditingItemOnVisibilityChange;
end;

procedure TcxGridInplaceEditForm.ValidateFocusedItem;
begin
  GridView.Controller.FocusFirstAvailableItem;
end;

procedure TcxGridInplaceEditForm.ValuesChanged;
begin
  if DetachedForm <> nil then
    DetachedForm.ValuesChanged;
  UpdateModified;
end;

function TcxGridInplaceEditForm.CloseOnRecordInserting: Boolean;
begin
  Result := Close;
end;

procedure TcxGridInplaceEditForm.CloseOnFocusedRecordChanging
  (var AFocusingRecord: TcxCustomGridRecord);
var
  ARecordIndex: Integer;
begin
  ARecordIndex := AFocusingRecord.RecordIndex;
  if Close then
    AFocusingRecord := GridView.ViewData.GetRecordByRecordIndex(ARecordIndex);
end;

function TcxGridInplaceEditForm.CloseQuery(ARaiseAbortOnCancel: Boolean = True)
  : TModalResult;
var
  ADlgResult: Integer;
begin
  ADlgResult := MessageDlg
    (cxGetResourceString(@scxGridInplaceEditFormSaveChangesQuery), mtWarning,
    mbYesNoCancel, 0);
  if (ADlgResult = mrCancel) and ARaiseAbortOnCancel then
    Abort;
  Result := ADlgResult;
end;

procedure TcxGridInplaceEditForm.ResetEditingRecordIndex;
begin
  EditingRecordIndex := cxNullEditingRecordIndex;
end;

procedure TcxGridInplaceEditForm.CreateDetachedFormMetrics;
begin
  FDetachedFormMetrics := TcxGridCustomDetachedEditFormMetricsInfo.Create
    (DetachedForm);
end;

procedure TcxGridInplaceEditForm.ResetDetachedFormSize;
begin
  if DetachedFormMetrics <> nil then
  begin
    DetachedFormMetrics.UseDefaultHeight := True;
    DetachedFormMetrics.UseDefaultWidht := True;
  end;
end;

procedure TcxGridInplaceEditForm.RestoreDetachedFormMetrics;
begin
  if DetachedFormMetrics <> nil then
  begin
    DetachedForm.Position := poDesigned;
    DetachedFormMetrics.Restore(DetachedForm);
  end;
  DetachedForm.UseDefaultHeight := UseDetachedFormDefaultHeight;
  DetachedForm.UseDefaultWidth := UseDetachedFormDefaultWidth;
end;

procedure TcxGridInplaceEditForm.StoreDetachedFormMetrics;
begin
  if DetachedFormMetrics = nil then
    CreateDetachedFormMetrics
  else
    DetachedFormMetrics.Store(DetachedForm);
  DetachedFormMetrics.UseDefaultHeight := DetachedForm.UseDefaultHeight;
  DetachedFormMetrics.UseDefaultWidht := DetachedForm.UseDefaultWidth;
end;

function TcxGridInplaceEditForm.UseDetachedFormDefaultHeight: Boolean;
begin
  Result := (DetachedFormMetrics = nil) or DetachedFormMetrics.UseDefaultHeight;
end;

function TcxGridInplaceEditForm.UseDetachedFormDefaultWidth: Boolean;
begin
  Result := (DetachedFormMetrics = nil) or DetachedFormMetrics.UseDefaultWidht;
end;

function TcxGridInplaceEditForm.GetContainer: TcxGridInplaceEditFormContainer;
begin
  Result := TcxGridInplaceEditFormContainer(inherited Container);
end;

procedure TcxGridInplaceEditForm.SetCaptionMask(const AValue: string);
begin
  if CaptionMask <> AValue then
  begin
    FCaptionMask := AValue;
    Changed;
  end;
end;

procedure TcxGridInplaceEditForm.SetEditingRecordIndex(AValue: Integer);
begin
  if AValue <> EditingRecordIndex then
  begin
    FEditingRecordIndex := AValue;
    SetVisible(EditingRecordIndex <> cxNullEditingRecordIndex);
    UpdateModified;
    if DisplayMode = efdmInplace then
      UpdateInplacePosition
    else if Visible then
      ShowModal;
  end;
end;

procedure TcxGridInplaceEditForm.SetVisible(AValue: Boolean);
begin
  Active := AValue;
end;

initialization

RegisterClasses([TcxGridInplaceEditFormLayoutItem,
  TcxGridInplaceEditFormGroup]);

end.
