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

unit dxSpreadSheetReferenceEditDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, dxCore, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutContainer,
  cxEdit, cxClasses, cxButtons, dxLayoutControl, cxContainer, cxListBox,
  dxSpreadSheetCore, dxForms, cxCustomListBox,
  cxEditRepositoryItems, dxLayoutcxEditAdapters, cxTextEdit, cxMaskEdit,
  cxButtonEdit;

type
  TdxSpreadSheetReferenceEditDialogState = class;

  { TdxSpreadSheetReferenceEditDialogForm }

  TdxSpreadSheetReferenceEditDialogForm = class(TdxForm, IdxSpreadSheetListener,
    IdxSpreadSheetSelectionListener,
    IdxSpreadSheetTableViewSelectionModeListener)
    beAreaSelector: TcxButtonEdit;
    EditRepository: TcxEditRepository;
    ertiArea: TcxEditRepositoryButtonItem;
    LayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liCompactView: TdxLayoutItem;

    procedure beAreaSelectorPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure ertiAreaPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure ertiAreaPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
  strict private
  type
    TViewMode = (vmAuto, vmCompact);
  strict private
    FActiveSheet: TdxSpreadSheetTableView;
    FIsCompactView: Boolean;
    FSavedSelection: TMemoryStream;
    FSavedWindowState: TdxSpreadSheetReferenceEditDialogState;
    FSelectionMode: TdxSpreadSheetTableViewSelectionMode;
    FSheet: TdxSpreadSheetTableView;
    FSpreadSheet: TdxCustomSpreadSheet;
    FViewMode: TViewMode;

    procedure CheckViewMode;
    procedure RestoreSelection;
    procedure SaveSelection;
    //
    function GetDialogsLookAndFeel: TcxLookAndFeel;
    procedure SetActiveSheet(AValue: TdxSpreadSheetTableView);
    procedure SetIsCompactView(AValue: Boolean);
    procedure SetViewMode(AValue: TViewMode);
  protected
    FLoading: Boolean;

    procedure ApplyLocalizations; virtual;
    procedure Finalize; virtual;
    procedure Initialize(ASheet: TdxSpreadSheetTableView);
    procedure InitializeCore; virtual;
    procedure SetupWindow(AIsCompactView: Boolean); virtual;
    //
    procedure FinishEditing(AEdit: TcxButtonEdit);
    procedure StartEditing(AEdit: TcxButtonEdit;
      AMode: TdxSpreadSheetTableViewSelectionMode);
    //
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    // IdxSpreadSheetListener
    procedure DataChanged(Sender: TdxCustomSpreadSheet); virtual;
    // IdxSpreadSheetSelectionListener
    procedure SelectionChanged(Sender: TdxSpreadSheetCustomView); virtual;
    // IdxSpreadSheetTableViewSelectionModeListener
    procedure SelectionModeChanged(Sender: TdxSpreadSheetTableView;
      AMode: TdxSpreadSheetTableViewSelectionMode);
    //
    function AreaToString(const AArea: TRect;
      AView: TdxSpreadSheetTableView = nil): string;
    function SaveWindowState: TdxSpreadSheetReferenceEditDialogState; virtual;
    function TryStringToReference(const S: string;
      out AView: TdxSpreadSheetTableView; out AArea: TRect): Boolean; virtual;
    function ValidateReference(const S: string): Boolean; virtual;
    //
    property ActiveSheet: TdxSpreadSheetTableView read FActiveSheet
      write SetActiveSheet;
    property DialogsLookAndFeel: TcxLookAndFeel read GetDialogsLookAndFeel;
    property ViewMode: TViewMode read FViewMode write SetViewMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    //
    property Sheet: TdxSpreadSheetTableView read FSheet;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

  { TdxSpreadSheetReferenceEditDialogState }

  TdxSpreadSheetReferenceEditDialogState = class
  strict private
    FAutoSize: Boolean;
    FBorderStyle: TFormBorderStyle;
    FBounds: TRect;
    FLayoutAlign: TAlign;
    FLayoutAlignHorz: TdxLayoutAlignHorz;
    FLayoutAlignVert: TdxLayoutAlignVert;
    FLayoutAutoSize: Boolean;
    FMinHeight: Integer;
    FMinWidth: Integer;
  public
    constructor Create(AForm: TdxSpreadSheetReferenceEditDialogForm);
    procedure Restore(AForm: TdxSpreadSheetReferenceEditDialogForm); virtual;
    procedure Store(AForm: TdxSpreadSheetReferenceEditDialogForm); virtual;
  end;

implementation

uses
  dxSpreadSheetUtils, dxSpreadSheetCoreReferences, cxVariants,
  dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreFormulas;

{$R *.dfm}

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewSelectionAccess = class
    (TdxSpreadSheetTableViewSelection);

  { TdxSpreadSheetReferenceEditDialogState }

constructor TdxSpreadSheetReferenceEditDialogState.Create
  (AForm: TdxSpreadSheetReferenceEditDialogForm);
begin
  Store(AForm);
end;

procedure TdxSpreadSheetReferenceEditDialogState.Restore
  (AForm: TdxSpreadSheetReferenceEditDialogForm);
begin
  AForm.AutoSize := FAutoSize;
  AForm.lcMain.AutoSize := FLayoutAutoSize;
  AForm.lcMain.Align := FLayoutAlign;
  AForm.lcMainGroup_Root.AlignHorz := FLayoutAlignHorz;
  AForm.lcMainGroup_Root.AlignVert := FLayoutAlignVert;
  AForm.Constraints.MinHeight := FMinHeight;
  AForm.Constraints.MinWidth := FMinWidth;
  AForm.BorderStyle := FBorderStyle;
  AForm.BoundsRect := FBounds;
end;

procedure TdxSpreadSheetReferenceEditDialogState.Store
  (AForm: TdxSpreadSheetReferenceEditDialogForm);
begin
  FBounds := AForm.BoundsRect;
  FAutoSize := AForm.AutoSize;
  FBorderStyle := AForm.BorderStyle;
  FMinHeight := AForm.Constraints.MinHeight;
  FMinWidth := AForm.Constraints.MinWidth;
  FLayoutAlign := AForm.lcMain.Align;
  FLayoutAutoSize := AForm.lcMain.AutoSize;
  FLayoutAlignHorz := AForm.lcMainGroup_Root.AlignHorz;
  FLayoutAlignVert := AForm.lcMainGroup_Root.AlignVert;
end;

{ TdxSpreadSheetReferenceEditDialogForm }

constructor TdxSpreadSheetReferenceEditDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FSavedSelection := TMemoryStream.Create;
end;

destructor TdxSpreadSheetReferenceEditDialogForm.Destroy;
begin
  FreeAndNil(FSavedSelection);
  FreeAndNil(FSavedWindowState);
  inherited;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.BeforeDestruction;
begin
  inherited;
  Finalize;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.ApplyLocalizations;
begin
  // do nothing
end;

procedure TdxSpreadSheetReferenceEditDialogForm.CheckViewMode;
begin
  SetIsCompactView((beAreaSelector.Tag <> 0) and ((ViewMode = vmCompact) or
    (ViewMode = vmAuto) and (FSelectionMode <> smNone)));
end;

procedure TdxSpreadSheetReferenceEditDialogForm.Finalize;
begin
  if SpreadSheet <> nil then
  begin
    SpreadSheet.RemoveListener(Self);
    SpreadSheet.RemoveFreeNotification(Self);
  end;
  ActiveSheet := nil;
  FSpreadSheet := nil;
  FSheet := nil;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.Initialize
  (ASheet: TdxSpreadSheetTableView);
begin
  FSheet := ASheet;
  FSpreadSheet := ASheet.SpreadSheet;
  ActiveSheet := Sheet;

  SetControlLookAndFeel(Self, DialogsLookAndFeel);
  SpreadSheet.FreeNotification(Self);
  SpreadSheet.AddListener(Self);

  FLoading := True;
  try
    InitializeCore;
    ApplyLocalizations;
  finally
    FLoading := False;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.InitializeCore;
begin
  // do nothing
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SetupWindow(AIsCompactView
  : Boolean);
var
  AItem: TdxCustomLayoutItem;
  I: Integer;
begin
  lcMain.BeginUpdate;
  try
    for I := 0 to lcMainGroup_Root.Count - 1 do
    begin
      AItem := lcMainGroup_Root.Items[I];
      AItem.Visible := (AItem = liCompactView) = AIsCompactView;
    end;
  finally
    lcMain.EndUpdate(False);
  end;

  if AIsCompactView then
  begin
    Constraints.MinWidth := 0;
    Constraints.MinHeight := 0;
    lcMain.AutoSize := True;
    lcMain.Align := alNone;
    lcMainGroup_Root.AlignHorz := ahLeft;
    lcMainGroup_Root.AlignVert := avTop;
    BorderStyle := bsDialog;
    AutoSize := True;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.FinishEditing
  (AEdit: TcxButtonEdit);
begin
  if not FIsCompactView then
  begin
    beAreaSelector.Tag := 0;
    ActiveSheet.Controller.ForcedSelectionMode := smNone;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.StartEditing
  (AEdit: TcxButtonEdit; AMode: TdxSpreadSheetTableViewSelectionMode);
var
  AArea: TRect;
  AView: TdxSpreadSheetTableView;
begin
  beAreaSelector.Tag := TdxNativeUInt(AEdit);
  if TryStringToReference(AEdit.EditValue, AView, AArea) then
  begin
    ActiveSheet := AView;
    ActiveSheet.Selection.Add(AArea)
  end
  else
  begin
    ActiveSheet := Sheet;
    ActiveSheet.Selection.Clear;
  end;
  ActiveSheet.Active := True;
  ActiveSheet.Controller.ForcedSelectionMode := AMode;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = SpreadSheet) then
  begin
    Finalize;
    Release;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.DataChanged
  (Sender: TdxCustomSpreadSheet);
var
  AViewList: TdxSpreadSheetViewList;
begin
  AViewList := TdxCustomSpreadSheetAccess(Sender).FSheets;
  if (AViewList.IndexOf(Sheet) < 0) or (AViewList.IndexOf(ActiveSheet) < 0) then
  begin
    FActiveSheet := nil;
    FSheet := nil;
    Release;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SelectionChanged
  (Sender: TdxSpreadSheetCustomView);
var
  AView: TdxSpreadSheetTableView;
begin
  AView := Sender as TdxSpreadSheetTableView;
  beAreaSelector.Text := AreaToString(AView.Selection.Area, AView);
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SelectionModeChanged
  (Sender: TdxSpreadSheetTableView;
  AMode: TdxSpreadSheetTableViewSelectionMode);
begin
  FSelectionMode := AMode;
  CheckViewMode;
end;

function TdxSpreadSheetReferenceEditDialogForm.AreaToString(const AArea: TRect;
  AView: TdxSpreadSheetTableView = nil): string;
begin
  if AView <> nil then
    Result := dxReferenceToString(AArea, SpreadSheet.OptionsView.R1C1Reference,
      [croAbsoluteColumn, croAbsoluteRow, croSheetName], AView.Caption)
  else
    Result := dxReferenceToString(AArea, SpreadSheet.OptionsView.R1C1Reference,
      [croAbsoluteColumn, croAbsoluteRow])
end;

function TdxSpreadSheetReferenceEditDialogForm.ValidateReference
  (const S: string): Boolean;
var
  AAreaRect: TRect;
  AView: TdxSpreadSheetTableView;
begin
  Result := TryStringToReference(S, AView, AAreaRect);
end;

procedure TdxSpreadSheetReferenceEditDialogForm.RestoreSelection;
var
  AReader: TcxReader;
begin
  FSavedSelection.Position := 0;
  AReader := TcxReader.Create(FSavedSelection);
  try
    TdxSpreadSheetTableViewSelectionAccess(ActiveSheet.Selection)
      .LoadFromStream(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SaveSelection;
var
  AWriter: TcxWriter;
begin
  FSavedSelection.Size := 0;
  AWriter := TcxWriter.Create(FSavedSelection);
  try
    TdxSpreadSheetTableViewSelectionAccess(ActiveSheet.Selection)
      .SaveToStream(AWriter);
  finally
    AWriter.Free;
  end;
end;

function TdxSpreadSheetReferenceEditDialogForm.SaveWindowState
  : TdxSpreadSheetReferenceEditDialogState;
begin
  Result := TdxSpreadSheetReferenceEditDialogState.Create(Self);
end;

function TdxSpreadSheetReferenceEditDialogForm.TryStringToReference
  (const S: string; out AView: TdxSpreadSheetTableView;
  out AArea: TRect): Boolean;
var
  AScope: TObject;
begin
  Result := dxSpreadSheetParseCellReference(S, SpreadSheet.FormulaController,
    AScope, AArea);
  if Result then
  begin
    if AScope = nil then
      AView := Sheet
    else if AScope is TdxSpreadSheetTableView then
      AView := TdxSpreadSheetTableView(AScope)
    else
      Result := False;
  end;
end;

function TdxSpreadSheetReferenceEditDialogForm.GetDialogsLookAndFeel
  : TcxLookAndFeel;
begin
  Result := SpreadSheet.DialogsLookAndFeel;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SetIsCompactView
  (AValue: Boolean);

  procedure Prepare(ATarget, ASource: TcxButtonEdit);
  begin
    ATarget.EditValue := ASource.EditValue;
    if ATarget.CanFocusEx then
      ATarget.SetFocus;
  end;

var
  AEdit: TcxButtonEdit;
begin
  if AValue <> FIsCompactView then
  begin
    if not FIsCompactView then
    begin
      FreeAndNil(FSavedWindowState);
      FSavedWindowState := SaveWindowState;
    end;

    FIsCompactView := AValue;
    SetupWindow(FIsCompactView);

    AEdit := TObject(beAreaSelector.Tag) as TcxButtonEdit;
    if FIsCompactView then
      Prepare(beAreaSelector, AEdit)
    else
      Prepare(AEdit, beAreaSelector);

    if not FIsCompactView and (FSavedWindowState <> nil) then
    begin
      FSavedWindowState.Restore(Self);
      FreeAndNil(FSavedWindowState);
    end;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SetActiveSheet
  (AValue: TdxSpreadSheetTableView);
begin
  if FActiveSheet <> AValue then
  begin
    if (ActiveSheet <> nil) and not TdxSpreadSheetTableViewAccess(ActiveSheet).IsDestroying
    then
    begin
      FActiveSheet.Controller.ForcedSelectionMode := smNone;
      RestoreSelection;
      FActiveSheet := nil;
    end;
    if AValue <> nil then
    begin
      FActiveSheet := AValue;
      FActiveSheet.Active := True;
      SaveSelection;
    end;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.SetViewMode(AValue: TViewMode);
begin
  if AValue <> FViewMode then
  begin
    FViewMode := AValue;
    CheckViewMode;
  end;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.
  beAreaSelectorPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  ViewMode := vmAuto;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.ertiAreaPropertiesButtonClick
  (Sender: TObject; AButtonIndex: Integer);
begin
  ViewMode := vmCompact;
end;

procedure TdxSpreadSheetReferenceEditDialogForm.ertiAreaPropertiesValidate
  (Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := not ValidateReference(DisplayValue);
end;

end.
