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

unit cxEMFLookupEdit;

{$I cxVer.inc}

interface

uses
  Variants, Messages, Controls, SysUtils, Classes, Types, DB, Graphics,
  cxClasses, cxContainer,
  cxEdit, cxDBEdit, cxEditConsts, cxDB, cxDataUtils, cxDataStorage,
  cxCustomData,
  cxDBData, cxDropDownEdit, cxLookupEdit, cxLookupGrid, dxCoreClasses,
  cxEMFData, cxEMFLookupGrid,
  dxEMF.Metadata, cxMaskEdit, cxTextEdit, cxLookAndFeels, cxFilterControlUtils;

type

  TcxCustomEMFLookupEditProperties = class;
  TcxEMFLookupComboBoxProperties = class;

  { TcxCustomEMFLookupEditLookupData }

  TcxCustomEMFLookupEditLookupData = class(TcxCustomLookupEditLookupData)
  // for internal use
  private
    function GetDataController: TdxEMFDataController; inline;
    function GetProperties: TcxCustomEMFLookupEditProperties; inline;
  protected
    procedure DoSetCurrentKey(ARecordIndex: Integer); override;
    procedure DoSyncGrid; override;
    property DataController: TdxEMFDataController read GetDataController;
    property Properties: TcxCustomEMFLookupEditProperties read GetProperties;
  end;

  { TcxCustomEMFLookupEditProperties }

  TcxCustomEMFLookupEditProperties = class(TcxCustomLookupEditProperties)
  // for internal use
  protected
    // LookupGrid methods
    procedure EMFLookupGridBeginUpdate; virtual; abstract;
    procedure EMFLookupGridEndUpdate; virtual; abstract;
    function GetEMFLookupGridColumnField(AIndex: Integer): TdxMappingMemberInfo;
      virtual; abstract;
    function GetEMFLookupGridColumnFieldName(AIndex: Integer): string;
      virtual; abstract;
    function GetEMFLookupGridColumnIndexByFieldName(const AFieldName: string)
      : Integer; virtual; abstract;
    function GetEMFLookupGridDataController: TdxEMFDataController; virtual;

    function GetActualEntityInfo: TdxEntityInfo; virtual;

    function GetMemberInfo: TdxMappingMemberInfo;
    function GetLookupMemberInfo: TdxMappingMemberInfo;

    property ActualEntityInfo: TdxEntityInfo read GetActualEntityInfo;
    property DataController: TdxEMFDataController
      read GetEMFLookupGridDataController;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function AllowRepositorySharing: Boolean; override;
    function IsLookupField: Boolean; override;
    function GetDisplayText(const AEditValue: Variant;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;

    property IncrementalFiltering default False;
  end;

  { TcxCustomEMFLookupEdit }

  TcxCustomEMFLookupEdit = class(TcxCustomLookupEdit) // for internal use
  private
    function GetProperties: TcxCustomEMFLookupEditProperties;
    function GetActiveProperties: TcxCustomEMFLookupEditProperties;
    procedure SetProperties(Value: TcxCustomEMFLookupEditProperties);
  protected
    function IsValidChar(AChar: Char): Boolean; override;
    procedure PopupWindowBeforeClosing(Sender: TObject); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCustomEMFLookupEditProperties
      read GetActiveProperties;
    property Properties: TcxCustomEMFLookupEditProperties read GetProperties
      write SetProperties;
  end;

  { TcxCustomEMFLookupComboBox }

  TcxCustomEMFLookupComboBox = class(TcxCustomEMFLookupEdit)
  private
    function GetProperties: TcxEMFLookupComboBoxProperties;
    function GetActiveProperties: TcxEMFLookupComboBoxProperties;
    procedure SetProperties(Value: TcxEMFLookupComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxEMFLookupComboBoxProperties
      read GetActiveProperties;
    property EditValue;
    property Properties: TcxEMFLookupComboBoxProperties read GetProperties
      write SetProperties;
    property Text;
  end;

  { TcxEMFLookupComboBox }

  TcxEMFLookupComboBox = class(TcxCustomEMFLookupComboBox)
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property EditValue;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxEMFLookupEditDataBinding }

  TcxEMFLookupEditDataBinding = class(TcxDBTextEditDataBinding)
  // for internal use
  protected
    function IsLookupControl: Boolean; override;
  end;

  { TcxCustomEMFLookupComboBoxProperties }

  TcxCustomEMFLookupComboBoxProperties = class(TcxCustomEMFLookupEditProperties)
  strict private
    FDataSource: TdxEMFDataSource;
    FExternalListSource: TdxEMFDataSource;
    FGrid: TcxCustomEMFLookupGrid;
    function GetListColumns: TcxEMFLookupGridColumns;
    function GetListOptions: TcxEMFLookupGridOptions;
    function GetOnSortingChanged: TNotifyEvent;
    procedure SetListColumns(Value: TcxEMFLookupGridColumns);
    procedure SetListOptions(Value: TcxEMFLookupGridOptions);
    procedure SetExternalListSource(Value: TdxEMFDataSource);
    procedure SetOnSortingChanged(Value: TNotifyEvent);
  protected
    procedure CheckGridColumns; virtual;
    procedure CreateInternalDataSource;
    procedure FreeNotification(Sender: TComponent); override;
    function GetActualDataSource: TdxEMFDataSource; virtual;
    function GetActualEntityInfo: TdxEntityInfo; override;
    function GetLookupGridClass: TcxCustomEMFLookupGridClass; virtual;
    procedure ListOptionsChanged(Sender: TObject); virtual;
    procedure SetIncrementalFilteringOptions
      (Value: TcxTextEditIncrementalFilteringOptions); override;
    // LookupGrid methods
    function GetLookupGridColumnCount: Integer; override;
    function GetLookupGridColumnProperties(AIndex: Integer)
      : TcxCustomEditProperties; override;
    function GetLookupGridControl: TWinControl; override;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    function GetLookupGridDataController: TcxCustomDataController; override;
    function GetLookupGridVisualAreaPreferredWidth: Integer; override;
    function GetLookupGridNearestPopupHeight(AHeight: Integer)
      : Integer; override;
    function GetLookupGridPopupHeight(ADropDownRowCount: Integer)
      : Integer; override;
    function IsLookupGridMouseOverList(const P: TPoint): Boolean; override;
    procedure LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
      AOnCloseUp: cxLookupEdit.TcxLookupGridCloseUpEvent); override;
    procedure LookupGridInitialize; override;
    procedure LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
      AColor: TColor; AFont: TFont); override;
    procedure LookupGridLockMouseMove; override;
    procedure LookupGridMakeFocusedRowVisible; override;
    procedure LookupGridUnlockMouseMove; override;
    // EMFLookupGrid methods
    procedure EMFLookupGridBeginUpdate; override;
    procedure EMFLookupGridEndUpdate; override;
    function GetEMFLookupGridColumnField(AIndex: Integer)
      : TdxMappingMemberInfo; override;
    function GetEMFLookupGridColumnFieldName(AIndex: Integer): string; override;
    function GetEMFLookupGridColumnIndexByFieldName(const AFieldName: string)
      : Integer; override;
    function GetEMFLookupGridDataController: TdxEMFDataController; override;

    property Grid: TcxCustomEMFLookupGrid read FGrid;
    property ListColumns: TcxEMFLookupGridColumns read GetListColumns
      write SetListColumns;
    property ListOptions: TcxEMFLookupGridOptions read GetListOptions
      write SetListOptions;
    property ExternalListSource: TdxEMFDataSource read FExternalListSource
      write SetExternalListSource;
    property OnSortingChanged: TNotifyEvent read GetOnSortingChanged
      write SetOnSortingChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
  end;

  { TcxEMFLookupComboBoxProperties }

  TcxEMFLookupComboBoxProperties = class(TcxCustomEMFLookupComboBoxProperties)
  published
    property Alignment;
    property AutoSelect;
    property AssignedValues;
    property ButtonGlyph;
    property CharCase;
    property ClearKey;
    property DropDownAutoSize;
    property DropDownHeight;
    property DropDownListStyle;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property ListColumns;
    property ListOptions;
    property ExternalListSource;
    property MaxLength;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
    property UseLeftAlignmentOnEditing;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnSortingChanged;
    property OnValidate;
  end;

  { TcxEditRepositoryEMFLookupComboBoxItem }

  TcxEditRepositoryEMFLookupComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxEMFLookupComboBoxProperties;
    procedure SetProperties(Value: TcxEMFLookupComboBoxProperties);
  public
    class function GetEditPropertiesClass
      : TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxEMFLookupComboBoxProperties read GetProperties
      write SetProperties;
  end;

  { TcxFilterLookupComboBoxHelper }

  TcxFilterEMFLookupComboBoxHelper = class(TcxFilterComboBoxHelper)
  // for internal use
  protected
    class function IsIDefaultValuesProviderNeeded(AEditProperties
      : TcxCustomEditProperties): Boolean; override;
  public
    class function GetFilterDataType(AValueTypeClass: TcxValueTypeClass)
      : TcxFilterDataType; override;
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class procedure GetFilterValue(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties; var V: Variant;
      var S: TCaption); override;
    class function GetSupportedFilterOperators(AProperties
      : TcxCustomEditProperties; AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties, AEditProperties
      : TcxCustomEditProperties; AHasButtons: Boolean); override;
    class function IsValueValid(AValueTypeClass: TcxValueTypeClass;
      var Value: Variant): Boolean; override;
    class function UseDisplayValue: Boolean; override;
  end;

resourcestring
  cxSEditRepositoryEMFLookupComboBoxItem =
    'EMFLookupComboBox|Represents an ultra-advanced lookup using the QuantumGrid as its drop down control';

implementation

uses
  cxControls, dxEMF.Types, cxEMFEntityEditor;

type
  TcxControlAccess = class(TcxControl);
  TdxEMFDataControllerInfoAccess = class(TdxEMFDataControllerInfo);
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);

  { TcxCustomEMFLookupEditLookupData }

procedure TcxCustomEMFLookupEditLookupData.DoSetCurrentKey
  (ARecordIndex: Integer);
var
  ACollection: IdxEMFCollection;
begin
  ACollection := DataController.DataSource.Session.GetObjects
    (Properties.ActualEntityInfo.ClassAttributes.PersistentClass);
  FCurrentKey := NativeInt(Pointer(ACollection.Items[ARecordIndex]));
end;

procedure TcxCustomEMFLookupEditLookupData.DoSyncGrid;
var
  ARecordIndex: Integer;
  AObject: TObject;
  ACurrentKey: Variant;
begin
  if (DataController <> nil) and DataController.Active then
    try
      Properties.LockDataChanged;
      try
        ACurrentKey := GetCurrentKey;
        if VarIsNull(ACurrentKey) then
          AObject := nil
        else
          AObject := Pointer(NativeInt(ACurrentKey));
        ARecordIndex := TdxEMFDataControllerInfoAccess
          (DataController.DataControllerInfo).GetRecordIndex(AObject);
        DataController.FocusedRecordIndex := ARecordIndex;
      finally
        Properties.UnlockDataChanged;
      end;
    except
      on EVariantError do;
      on EDatabaseError do;
    end;
end;

function TcxCustomEMFLookupEditLookupData.GetDataController
  : TdxEMFDataController;
begin
  Result := Properties.DataController;
end;

function TcxCustomEMFLookupEditLookupData.GetProperties
  : TcxCustomEMFLookupEditProperties;
begin
  Result := TcxCustomEMFLookupEditProperties(inherited Properties);
end;

{ TcxCustomEMFLookupEditProperties }

constructor TcxCustomEMFLookupEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  IncrementalFiltering := False;
end;

function TcxCustomEMFLookupEditProperties.GetActualEntityInfo: TdxEntityInfo;
begin
  Result := nil;
end;

class function TcxCustomEMFLookupEditProperties.GetContainerClass
  : TcxContainerClass;
begin
  Result := TcxCustomEMFLookupEdit;
end;

function TcxCustomEMFLookupEditProperties.GetEMFLookupGridDataController
  : TdxEMFDataController;
begin
  Result := TdxEMFDataController(GetLookupGridDataController);
end;

function TcxCustomEMFLookupEditProperties.GetMemberInfo: TdxMappingMemberInfo;
var
  AProvider: IcxEMFDefaultValuesProvider;
begin
  if Supports(FIDefaultValuesProvider, IcxEMFDefaultValuesProvider, AProvider)
  then
    Result := AProvider.MemberInfo
  else
    Result := nil;
end;

function TcxCustomEMFLookupEditProperties.GetLookupMemberInfo
  : TdxMappingMemberInfo;
begin
  Result := GetMemberInfo;
  if (Result <> nil) and not Result.IsAssociation then
    Result := nil;
end;

function TcxCustomEMFLookupEditProperties.IsLookupField: Boolean;
begin
  Result := GetLookupMemberInfo <> nil;
end;

function TcxCustomEMFLookupEditProperties.GetDisplayText(const AEditValue
  : Variant; AFullText, AIsInplace: Boolean): string;
var
  AMemberInfo: TdxMappingMemberInfo;
begin
  AMemberInfo := GetMemberInfo;
  if AMemberInfo <> nil then
    Result := AMemberInfo.GetValueAsDisplayText(AEditValue)
  else
    Result := VarToStr(AEditValue);
end;

procedure TcxCustomEMFLookupEditProperties.PrepareDisplayValue(const AEditValue
  : TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  DisplayValue := GetDisplayText(AEditValue);
end;

function TcxCustomEMFLookupEditProperties.AllowRepositorySharing: Boolean;
begin
  Result := False;
end;

{ TcxCustomEMFLookupEdit }

function TcxCustomEMFLookupEdit.GetActiveProperties
  : TcxCustomEMFLookupEditProperties;
begin
  Result := TcxCustomEMFLookupEditProperties(InternalGetActiveProperties);
end;

function TcxCustomEMFLookupEdit.GetProperties: TcxCustomEMFLookupEditProperties;
begin
  Result := TcxCustomEMFLookupEditProperties(inherited Properties);
end;

class function TcxCustomEMFLookupEdit.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEMFLookupEditProperties;
end;

function TcxCustomEMFLookupEdit.IsValidChar(AChar: Char): Boolean;
begin
  Result := ActiveProperties.IsLookupField and not IsReadOnly;
end;

procedure TcxCustomEMFLookupEdit.PopupWindowBeforeClosing(Sender: TObject);
begin
  try
    ActiveProperties.DataController.CheckBrowseMode;
  except
    ActiveProperties.DataController.Cancel;
    raise
  end;
end;

procedure TcxCustomEMFLookupEdit.SetProperties
  (Value: TcxCustomEMFLookupEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxEMFLookupEditDataBinding }

function TcxEMFLookupEditDataBinding.IsLookupControl: Boolean;
begin
  Result := True;
end;

{ TcxEditRepositoryEMFLookupComboBoxItem }

class function TcxEditRepositoryEMFLookupComboBoxItem.GetEditPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TcxEMFLookupComboBoxProperties;
end;

function TcxEditRepositoryEMFLookupComboBoxItem.GetProperties
  : TcxEMFLookupComboBoxProperties;
begin
  Result := inherited Properties as TcxEMFLookupComboBoxProperties;
end;

procedure TcxEditRepositoryEMFLookupComboBoxItem.SetProperties
  (Value: TcxEMFLookupComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxCustomEMFLookupComboBoxProperties }

procedure TcxCustomEMFLookupComboBoxProperties.Assign(Source: TPersistent);
var
  ASource: TcxCustomEMFLookupComboBoxProperties absolute Source;
begin
  if Source is TcxCustomEMFLookupComboBoxProperties then
  begin
    BeginUpdate;
    try
      ListOptions := ASource.ListOptions;
      if not IsDefinedByLookup then
      begin
        ExternalListSource := ASource.ExternalListSource;
        ListColumns := ASource.ListColumns;
      end;
      OnSortingChanged := ASource.OnSortingChanged;
      inherited Assign(Source);
      if IsDefinedByLookup then
        ListColumns := ASource.ListColumns;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

constructor TcxCustomEMFLookupComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGrid := GetLookupGridClass.Create(nil);
  FGrid.IsPopupControl := True;
  FGrid.Options.OnChanged := ListOptionsChanged;
  InitializeDataController;
end;

destructor TcxCustomEMFLookupComboBoxProperties.Destroy;
begin
  DeinitializeDataController;
  FreeAndNil(FDataSource);
  FGrid.Free;
  FGrid := nil;
  inherited Destroy;
end;

procedure TcxCustomEMFLookupComboBoxProperties.EMFLookupGridBeginUpdate;
begin
  Grid.BeginUpdate;
end;

procedure TcxCustomEMFLookupComboBoxProperties.EMFLookupGridEndUpdate;
begin
  Grid.EndUpdate;
end;

class function TcxCustomEMFLookupComboBoxProperties.GetContainerClass
  : TcxContainerClass;
begin
  Result := TcxEMFLookupComboBox;
end;

function TcxCustomEMFLookupComboBoxProperties.GetEMFLookupGridColumnField
  (AIndex: Integer): TdxMappingMemberInfo;
begin
  Result := ListColumns[AIndex].MemberInfo;
end;

function TcxCustomEMFLookupComboBoxProperties.GetEMFLookupGridColumnFieldName
  (AIndex: Integer): string;
begin
  Result := ListColumns[AIndex].FieldName;
end;

function TcxCustomEMFLookupComboBoxProperties.
  GetEMFLookupGridColumnIndexByFieldName(const AFieldName: string): Integer;
var
  AColumn: TcxEMFLookupGridColumn;
begin
  AColumn := ListColumns.ColumnByFieldName(AFieldName);
  if AColumn <> nil then
    Result := AColumn.Index
  else
    Result := -1;
end;

function TcxCustomEMFLookupComboBoxProperties.GetEMFLookupGridDataController
  : TdxEMFDataController;
begin
  if Grid <> nil then
    Result := Grid.DataController
  else
    Result := nil;
end;

function TcxCustomEMFLookupComboBoxProperties.GetListColumns
  : TcxEMFLookupGridColumns;
begin
  Result := Grid.Columns;
end;

function TcxCustomEMFLookupComboBoxProperties.GetListOptions
  : TcxEMFLookupGridOptions;
begin
  Result := Grid.Options;
end;

procedure TcxCustomEMFLookupComboBoxProperties.CheckGridColumns;
var
  AInfo: TdxMappingMemberInfo;
begin
  if Grid.Columns.Count = 0 then
  begin
    AInfo := GetLookupMemberInfo;
    if AInfo <> nil then
      Grid.Columns.Add.FieldName := AInfo.LookupResultField;
  end;
end;

procedure TcxCustomEMFLookupComboBoxProperties.CreateInternalDataSource;
var
  AIntf: IdxEMFDataController;
  ADataController: TdxEMFDataController;
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := ActualEntityInfo;
  if (AEntityInfo <> nil) and Supports(FIDefaultValuesProvider,
    IdxEMFDataController, AIntf) then
  begin
    ADataController := AIntf.DataController;
    if (ADataController <> nil) and (ADataController.DataSource <> nil) then
    begin
      FDataSource := TdxEMFDataSource.Create(nil);
      FDataSource.Session := ADataController.DataSource.Session;
      FDataSource.EntityInfo := AEntityInfo;
      try
        FDataSource.Active := True;
      except
        FreeAndNil(FDataSource);
        raise;
      end;
    end;
  end;
end;

function TcxCustomEMFLookupComboBoxProperties.GetActualEntityInfo
  : TdxEntityInfo;
var
  AProvider: IcxEMFDefaultValuesProvider;
begin
  if Supports(FIDefaultValuesProvider, IcxEMFDefaultValuesProvider, AProvider)
    and (AProvider.MemberInfo <> nil) then
    Result := AProvider.MemberInfo.ReferenceType
  else
    Result := nil;
end;

procedure TcxCustomEMFLookupComboBoxProperties.FreeNotification
  (Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = ExternalListSource then
    ExternalListSource := nil;
end;

function TcxCustomEMFLookupComboBoxProperties.GetActualDataSource
  : TdxEMFDataSource;
begin
  Result := ExternalListSource;
  if Result = nil then
  begin
    if FDataSource = nil then
      CreateInternalDataSource;
    Result := FDataSource;
  end;
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridClass
  : TcxCustomEMFLookupGridClass;
begin
  Result := TcxCustomEMFLookupGrid;
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridColumnCount: Integer;
begin
  Result := ListColumns.Count;
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridColumnProperties
  (AIndex: Integer): TcxCustomEditProperties;
begin
  Result := ListColumns[AIndex].Properties;
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridControl: TWinControl;
begin
  Result := Grid;
end;

class function TcxCustomEMFLookupComboBoxProperties.GetLookupDataClass
  : TcxInterfacedPersistentClass;
begin
  Result := TcxCustomEMFLookupEditLookupData;
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridDataController
  : TcxCustomDataController;
begin
  Result := Grid.DataController
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridNearestPopupHeight
  (AHeight: Integer): Integer;
begin
  Result := Grid.GetNearestPopupHeight(AHeight);
end;

function TcxCustomEMFLookupComboBoxProperties.GetLookupGridPopupHeight
  (ADropDownRowCount: Integer): Integer;
begin
  Result := Grid.GetPopupHeight(ADropDownRowCount);
end;

function TcxCustomEMFLookupComboBoxProperties.
  GetLookupGridVisualAreaPreferredWidth: Integer;
var
  I: Integer;
  AListColumns: TcxEMFLookupGridColumns;
begin
  AListColumns := ListColumns;
  Result := Grid.ViewInfo.GridLineWidth * AListColumns.Count;
  for I := 0 to AListColumns.Count - 1 do
    Inc(Result, AListColumns[I].Width);
  if (DataController <> nil) and (DataController.GetRowCount > DropDownRows)
  then
    Result := Result + TcxControlAccess(Grid).GetVScrollBarDefaultAreaWidth;
end;

function TcxCustomEMFLookupComboBoxProperties.GetOnSortingChanged: TNotifyEvent;
begin
  Result := Grid.DataController.OnSortingChanged;
end;

function TcxCustomEMFLookupComboBoxProperties.IsLookupGridMouseOverList
  (const P: TPoint): Boolean;
begin
  Result := Grid.IsMouseOverList(P);
end;

procedure TcxCustomEMFLookupComboBoxProperties.ListOptionsChanged
  (Sender: TObject);
begin
  Changed;
end;

procedure TcxCustomEMFLookupComboBoxProperties.LookupGridInitEvents(AOnClick,
  AOnFocusedRowChanged: TNotifyEvent;
  AOnCloseUp: cxLookupEdit.TcxLookupGridCloseUpEvent);
begin
  Grid.OnClick := AOnClick;
  Grid.OnFocusedRowChanged := AOnFocusedRowChanged;
  Grid.OnCloseUp := AOnCloseUp;
end;

procedure TcxCustomEMFLookupComboBoxProperties.LookupGridInitialize;
begin
  inherited LookupGridInitialize;
  DataController.DataSource := GetActualDataSource;
  CheckGridColumns;
end;

procedure TcxCustomEMFLookupComboBoxProperties.LookupGridInitLookAndFeel
  (ALookAndFeel: TcxLookAndFeel; AColor: TColor; AFont: TFont);
begin
  Grid.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  Grid.Color := AColor;
  Grid.Font := AFont;
end;

procedure TcxCustomEMFLookupComboBoxProperties.LookupGridLockMouseMove;
begin
  Grid.LockPopupMouseMove;
end;

procedure TcxCustomEMFLookupComboBoxProperties.LookupGridMakeFocusedRowVisible;
begin
  Grid.MakeFocusedRowVisible;
end;

procedure TcxCustomEMFLookupComboBoxProperties.LookupGridUnlockMouseMove;
begin
  TcxControlAccess(Grid).MouseCapture := False;
end;

procedure TcxCustomEMFLookupComboBoxProperties.SetIncrementalFilteringOptions
  (Value: TcxTextEditIncrementalFilteringOptions);
begin
  inherited SetIncrementalFilteringOptions(Value);
  Grid.Options.HighlightIncrementalFilteringText :=
    TcxTextEditIncrementalFilteringOption.ifoHighlightSearchText in Value;
end;

procedure TcxCustomEMFLookupComboBoxProperties.SetListColumns
  (Value: TcxEMFLookupGridColumns);
begin
  if not Grid.Columns.Equals(Value) then
    Grid.Columns := Value;
end;

procedure TcxCustomEMFLookupComboBoxProperties.SetListOptions
  (Value: TcxEMFLookupGridOptions);
begin
  Grid.Options := Value;
end;

procedure TcxCustomEMFLookupComboBoxProperties.SetExternalListSource
  (Value: TdxEMFDataSource);
begin
  if FExternalListSource <> Value then
  begin
    if Value <> nil then
      FreeAndNil(FDataSource);
    FreeNotificator.RemoveSender(FExternalListSource);
    FExternalListSource := Value;
    FreeNotificator.AddSender(FExternalListSource);
    Changed;
  end;
end;

procedure TcxCustomEMFLookupComboBoxProperties.SetOnSortingChanged
  (Value: TNotifyEvent);
begin
  Grid.DataController.OnSortingChanged := Value;
end;

{ TcxCustomEMFLookupComboBox }

function TcxCustomEMFLookupComboBox.GetActiveProperties
  : TcxEMFLookupComboBoxProperties;
begin
  Result := TcxEMFLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomEMFLookupComboBox.GetProperties
  : TcxEMFLookupComboBoxProperties;
begin
  Result := TcxEMFLookupComboBoxProperties(inherited Properties);
end;

class function TcxCustomEMFLookupComboBox.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TcxEMFLookupComboBoxProperties;
end;

procedure TcxCustomEMFLookupComboBox.SetProperties
  (Value: TcxEMFLookupComboBoxProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterEMFLookupComboBoxHelper }

class function TcxFilterEMFLookupComboBoxHelper.GetFilterDataType
  (AValueTypeClass: TcxValueTypeClass): TcxFilterDataType;
begin
  Result := fdtLookup;
end;

class function TcxFilterEMFLookupComboBoxHelper.GetFilterEditClass
  : TcxCustomEditClass;
begin
  Result := TcxEMFLookupComboBox;
end;

class procedure TcxFilterEMFLookupComboBoxHelper.GetFilterValue
  (AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
  var V: Variant; var S: TCaption);
begin
  V := AEdit.EditValue;
  S := TcxCustomTextEditPropertiesAccess(AEditProperties)
    .GetDefaultDisplayValue(V, True);
end;

class function TcxFilterEMFLookupComboBoxHelper.GetSupportedFilterOperators
  (AProperties: TcxCustomEditProperties; AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
end;

class procedure TcxFilterEMFLookupComboBoxHelper.InitializeProperties
  (AProperties, AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomLookupEditProperties(AProperties) do
  begin
    DropDownAutoSize := True;
    DropDownListStyle := lsFixedList;
    DropDownSizeable := True;
  end;
  with TcxCustomEMFLookupComboBoxProperties(AProperties) do
  begin
    FIDefaultValuesProvider := TcxCustomEMFLookupComboBoxProperties
      (AEditProperties).FIDefaultValuesProvider;
    ExternalListSource := TcxCustomEMFLookupComboBoxProperties(AEditProperties)
      .ExternalListSource;
    ListColumns := TcxCustomEMFLookupComboBoxProperties(AEditProperties)
      .ListColumns;
    IncrementalFiltering := False;
  end;
end;

class function TcxFilterEMFLookupComboBoxHelper.IsValueValid(AValueTypeClass
  : TcxValueTypeClass; var Value: Variant): Boolean;
begin
  Result := True;
end;

class function TcxFilterEMFLookupComboBoxHelper.UseDisplayValue: Boolean;
begin
  Result := True;
end;

class function TcxFilterEMFLookupComboBoxHelper.IsIDefaultValuesProviderNeeded
  (AEditProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomEMFLookupComboBoxProperties(AEditProperties)
    .IsDefinedByLookup;
end;

initialization

RegisterClasses([TcxEMFLookupComboBoxProperties, TcxEMFLookupComboBox]);
GetRegisteredEditProperties.Register(TcxEMFLookupComboBoxProperties,
  cxSEditRepositoryEMFLookupComboBoxItem);
FilterEditsController.Register(TcxCustomEMFLookupComboBoxProperties,
  TcxFilterEMFLookupComboBoxHelper);

finalization

FilterEditsController.Unregister(TcxCustomEMFLookupComboBoxProperties,
  TcxFilterEMFLookupComboBoxHelper);
GetRegisteredEditProperties.Unregister(TcxEMFLookupComboBoxProperties);

end.
