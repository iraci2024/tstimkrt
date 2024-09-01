{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEntityMapping Framework }
{ }
{ Copyright (c) 2016-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK AND }
{ ALL ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
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

unit cxEMFData;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Types, SysUtils, Generics.Collections, TypInfo, Rtti, Classes,
  dxCoreClasses,
  dxDateRanges,
  cxCustomData, cxDataStorage,
  cxEdit, cxFilter,
  dxEMF.Core,
  dxEMF.DB.Criteria,
  dxEMF.Types,
  dxEMF.MetaData,
  dxEMF.Core.Loader,
  dxEMF.DataDefinitions,
  cxDataUtils;

type
  TdxEMFDataGroup = class;
  TdxEMFDataGroups = class;
  TdxEMFDataController = class;
  TdxEMFDataControllerInfo = class;
  TdxEMFDataField = class;
  TdxEMFDataGroupInfo = class;
  TdxEMFDataSummary = class;
  TdxEMFCriteria = class;
  TdxEMFDataProvider = class;

  { IdxEMFDataController }

  IdxEMFDataController = interface
    ['{030CBD48-8EC8-417A-811B-EC41704AC524}']
    function GetDataController: TdxEMFDataController;
    property DataController: TdxEMFDataController read GetDataController;
  end;

  { IcxEMFDefaultValuesProvider }

  IcxEMFDefaultValuesProvider = interface
    ['{BFF55DEC-CFC4-45FB-A488-27837752C354}']
    function GetFieldName: string;
    function GetMemberInfo: TdxMappingMemberInfo;
    procedure SetFieldName(const Value: string);
    procedure SetMemberInfo(const Value: TdxMappingMemberInfo);
    property FieldName: string read GetFieldName write SetFieldName;
    property MemberInfo: TdxMappingMemberInfo read GetMemberInfo
      write SetMemberInfo;
  end;

  { TcxEMFEntityValueType }

  TcxEMFEntityValueType = class(TcxObjectValueType)
  protected
    class procedure FreeBuffer(ABuffer: PAnsiChar); override;
  end;

  { TdxEMFCriteriaItem }

  TdxEMFCriteriaItem = class(TcxFilterCriteriaItem)
  private
    function GetEMFCriteria: TdxEMFCriteria; inline;
  protected
    function GetFieldCaption: string; override;
    function GetFieldName: string; override;
  public
    property Criteria: TdxEMFCriteria read GetEMFCriteria;
  end;

  { TdxEMFCriteria }

  TdxEMFCriteria = class(TcxFilterCriteria)
  protected
    function GetItemClass: TcxFilterCriteriaItemClass; override;
  end;

  { TdxEMFDataFilterCriteriaItem }

  TdxEMFDataFilterCriteriaItem = class(TcxDataFilterCriteriaItem)
  strict private
    function GetField: TdxEMFDataField; inline;
    function CreateOperands: TArray<IdxCriteriaOperator>;
    function CreateBinaryOperator(AType: TdxBinaryOperatorType)
      : IdxCriteriaOperator;
    class function CreateNotOperator(const ACriteriaOperator
      : IdxCriteriaOperator): IdxCriteriaOperator; static; inline;
    function CreateFunctionOperator(AType: TdxFunctionOperatorType)
      : IdxCriteriaOperator;
  protected
    function GetFilterOperatorClass: TcxFilterOperatorClass; override;
    function GetFieldName: string; override;
    function GetFieldCaption: string; override;
    property Field: TdxEMFDataField read GetField;

    function GetCriteriaOperator: IdxCriteriaOperator;
  end;

  { TdxEMFDataFilterCriteria }

  TdxEMFDataFilterCriteria = class(TcxDataFilterCriteria)
  protected
    function GetItemClass: TcxFilterCriteriaItemClass; override;
  public
    function GetCriteriaOperator: IdxCriteriaOperator;
  end;

  { TdxEMFDataFindCriteria }

  TdxEMFDataFindCriteria = class(TcxDataFindCriteria)
  strict private
    function GetDataController: TdxEMFDataController; inline;
  protected
    function GetEscapeWildcard: Char; override;
    function IsBehaviorSupported(const AValue: TcxDataFindCriteriaBehavior)
      : Boolean; override;
    function IsConditionsLowerCase: Boolean; override;
    function UseMatches: Boolean; override;

    property DataController: TdxEMFDataController read GetDataController;
  end;

  { TcxEMFSummaryItem }

  TdxEMFSummaryItem = class(TcxDataSummaryItem)
  private
    FDataField: TdxEMFDataField;
    FFieldName: string;
    function GetFieldName: string;
    function GetEMFDataController: TdxEMFDataController; inline;
    procedure SetFieldName(const Value: string);
    function GetEMFDataField: TdxEMFDataField; inline;
    function GetField: TdxEMFDataField;
  protected
    function AllowKind(Value: TcxSummaryKind): Boolean;
    function CanSetKind(Value: TcxSummaryKind): Boolean; override;
    function GetExpression: IdxCriteriaOperator;
    function IsCurrency(AVarType: TVarType): Boolean; override;
    function IsDataBinded: Boolean; override;

    property EMFDataField: TdxEMFDataField read GetEMFDataField;
  public
    procedure Assign(Source: TPersistent); override;
    property DataController: TdxEMFDataController read GetEMFDataController;
    function DataField: TcxCustomDataField; override;
    property Field: TdxEMFDataField read GetField;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TdxEMFDataSummary }

  TdxEMFDataSummary = class(TcxDataSummary)
  private
    function GetDataController: TdxEMFDataController; inline;
  protected
    procedure CalculateSummary(ASummaryItems: TcxDataSummaryItems;
      ABeginIndex, AEndIndex: Integer;
      var ACountValues: TcxDataSummaryCountValues;
      var ASummaryValues: TcxDataSummaryValues); override;
  public
    procedure CalculateGroupSummary; override;
    property DataController: TdxEMFDataController read GetDataController;
  end;

  { TdxEMFEditingRow }

  TdxEMFEditingRow = TDictionary<TdxMappingMemberInfo, Variant>;

  { TdxEMFDataControllerCustomDataSource }

  TdxEMFDataControllerCustomDataSource = class(TdxCustomEMFQueryDataSource)
  strict private
    FControllers: TList<TdxEMFDataController>;
    FLockCount: Integer;
    FBeforeClose: TNotifyEvent;
    FAfterOpen: TNotifyEvent;
    FAfterClose: TNotifyEvent;
    FBeforeOpen: TNotifyEvent;
    FChangedHandlers: TdxNotifyEventHandler;
    function GetIsLocked: Boolean; inline;
    function GetIsDestroying: Boolean; inline;
    function GetIsLoading: Boolean; inline;
  protected
    procedure ActiveChanged(AActive: Boolean); override;
    procedure CheckSession;
    procedure CreateClientList; override;
    procedure DestroyClientList; override;
    procedure RegisterController(ADataController: TdxEMFDataController);
    procedure UnregisterController(ADataController: TdxEMFDataController);

    function EntityHasProperty(AObject: TObject;
      const AProperties: TArray < TPair < string, TValue >> ): Boolean; virtual;
    procedure InitializeDataSource;
    procedure InternalOpen; virtual;
    procedure NotifyClients; override;

    procedure Delete(const ACriteria: IdxCriteriaOperator); overload; virtual;
    function CreateNewEntity: TObject; virtual; abstract;
    function PostEditingEntity(AObject: TObject; AEditingRow: TdxEMFEditingRow)
      : TObject; virtual; abstract;

    procedure DoAfterClose; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoBeforeOpen; virtual;

    property IsLoading: Boolean read GetIsLoading;
    property IsLocked: Boolean read GetIsLocked;
    property IsDestroying: Boolean read GetIsDestroying;

    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterClose: TNotifyEvent read FAfterClose write FAfterClose;
    property BeforeClose: TNotifyEvent read FBeforeClose write FBeforeClose;
    property ChangedHandlers: TdxNotifyEventHandler read FChangedHandlers;
  end;

  { TcxEMFDataSource }

  TdxEMFDataSource = class(TdxEMFDataControllerCustomDataSource)
  protected
    procedure InternalOpen; override;
    function CreateNewEntity: TObject; override;
    function PostEditingEntity(AObject: TObject; AEditingRow: TdxEMFEditingRow)
      : TObject; override;
  published
    property Active;
    property DataContext;
    property EntityName;
    property PackageName;
    property Session;
  end;

  { TcxEMFDataField }

  TdxEMFDataField = class(TcxCustomDataField)
  strict private
    FDateTimeGrouping: TdxDateTimeGrouping;
    FFieldName: string;
    FFields: TArray<TdxEMFDataField>;
    function GetDataController: TdxEMFDataController; inline;
    function GetMemberInfo: TdxMappingMemberInfo;
    procedure SetMemberInfo(const Value: TdxMappingMemberInfo);
    procedure SetFieldName(const Value: string);
    function GetFieldCount: Integer;
    function DateTimeGroupingByRelativeToToday(const AOperand
      : IdxCriteriaOperator): IdxCriteriaOperator;
  private
    FMember: TdxMappingMemberInfo;
  protected
    procedure ClearFields;
    function GetCriteria(AIsGroup: Boolean = False): IdxCriteriaOperator;
    function GetGroupCriteria: IdxCriteriaOperator; virtual;
    function GetValues(AObject: TObject): TArray<TValue>;
    function GetMembers: TArray<TdxMappingMemberInfo>;
    function IsDateTimeField: Boolean;
    function IsDateTimeGrouping: Boolean; virtual;
    function IsMemberFormatted(AMember: TdxMappingMemberInfo;
      AIsTextEdit: Boolean): Boolean;
    procedure SetPropertiesByMember(AMember: TdxMappingMemberInfo;
      AMemberChanged: Boolean);
    function TryCastVariant(const AValue: Variant): Variant;
    property DataController: TdxEMFDataController read GetDataController;

    property DateTimeGrouping: TdxDateTimeGrouping read FDateTimeGrouping
      write FDateTimeGrouping;
    property FieldCount: Integer read GetFieldCount;
    property Fields: TArray<TdxEMFDataField> read FFields write FFields;
  public
    destructor Destroy; override;

    property FieldName: string read FFieldName write SetFieldName;
    property MemberInfo: TdxMappingMemberInfo read GetMemberInfo
      write SetMemberInfo;
  end;

  { TdxEMFDataRelation }

  TdxEMFDataRelation = class(TcxCustomDataRelation)
  private
    FMasterKeyField: TdxEMFDataField;
    FMasterKeyFields: TArray<TdxMappingMemberInfo>;
    function GetDataController: TdxEMFDataController; inline;
    function GetDetailKeyFieldNames: string;
    function GetMasterKeyFieldCount: Integer;
    function GetMasterKeyFieldNames: string;
    function GetMasterKeyFields: TArray<TdxMappingMemberInfo>; inline;
    function GetRelationDataController: TdxEMFDataController; inline;
    function GetDataSource: TdxEMFDataControllerCustomDataSource; inline;
    function GetDetailDataController: TdxEMFDataController; inline;
  protected
    function GetMasterRecordValue(ARecordIndex: Integer): TArray<TValue>;
    function IsLinked: Boolean; virtual;
    procedure RemoveDataField(ADataField: TcxCustomDataField); override;
    procedure SetMasterKeyField(AMasterKeyField: TdxEMFDataField);
    procedure UpdateMasterDetailKeyFieldNames;

    property MasterKeyFields: TArray<TdxMappingMemberInfo>
      read GetMasterKeyFields;
    property MasterKeyFieldCount: Integer read GetMasterKeyFieldCount;
    property RelationDataController: TdxEMFDataController
      read GetRelationDataController;
    property DataSource: TdxEMFDataControllerCustomDataSource
      read GetDataSource;
  public
    destructor Destroy; override;
    property DataController: TdxEMFDataController read GetDataController;
    property DetailDataController: TdxEMFDataController
      read GetDetailDataController;
    property DetailKeyFieldNames: string read GetDetailKeyFieldNames;
    property MasterKeyFieldNames: string read GetMasterKeyFieldNames;
  end;

  { TdxEMFDataGroupInfoDictionary }

  TdxEMFDataGroupInfoDictionary = class
    (TObjectDictionary<Cardinal, TdxFastList>)
  public
    constructor Create; reintroduce;

    procedure AddGroup(AGroup: TdxEMFDataGroupInfo);
    function FindGroup(const AGroupValue: Variant): TdxEMFDataGroupInfo;
  end;

  { TdxEMFDataGroup }

  TdxEMFDataGroup = class(TcxDataGroupInfo)
  private
    FParentGroup: TdxEMFDataGroup;
    FCollection: IdxEMFCollection;
    FGroupValue: Variant;
    FEntity: TObject;
    FChildren: TObjectList<TdxEMFDataGroup>;
    FIsInconsistent: Boolean;
    function GetCollection: IdxEMFCollection;
    function GetDataGroupCollection: IdxEMFCollection;
    function GetOwner: TdxEMFDataGroups; inline;
    function GetHasChildrenInfo: Boolean;
    function GetDataSource: TdxEMFDataControllerCustomDataSource;
    procedure SetIsInconsistent(const Value: Boolean);
    procedure PrepareAssociationValues;
    function GetGroupValue: Variant;
    function GetIsEmpty: Boolean; inline;
  protected
    function ContainsExpressionsInGroupValues: Boolean;
    function GetGroupValues: TArray<TPair<string, TValue>>;
    function GetGroupCriteria: IdxCriteriaOperator;
    function GetField(AGroupedColumnIndex: Integer): TdxEMFDataField;
    function GetRowGroupValue(ARow: TdxSelectStatementResultRow): Variant;
    function IsLastLevel: Boolean;
    procedure SetDataGroupInfo(ARow: TdxSelectStatementResultRow;
      AFirstRecordListIndex: Integer);
    function PrepareChildren: Boolean;

    procedure DeleteRecords;
    procedure Clear;
    function GetEntity(ARecordIndex: Integer): TObject;
    function GetRecordIndex(AEntity: TObject): Integer;
    function ContainsEntity(AEntity: TObject): Boolean;
    procedure Remove(AEntity: TObject);
    function IndexOf(AObject: TObject): Integer;

    property Owner: TdxEMFDataGroups read GetOwner;
    property DataSource: TdxEMFDataControllerCustomDataSource
      read GetDataSource;
    property HasChildrenInfo: Boolean read GetHasChildrenInfo;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsInconsistent: Boolean read FIsInconsistent
      write SetIsInconsistent;
    property ParentGroup: TdxEMFDataGroup read FParentGroup write FParentGroup;
  public
    constructor Create(AOwner: TdxEMFDataGroups);
    destructor Destroy; override;
    property GroupValue: Variant read GetGroupValue;
    property Children: TObjectList<TdxEMFDataGroup> read FChildren;
    property Collection: IdxEMFCollection read GetCollection;
  end;

  { TdxEMFDataGroupInfo }

  TdxEMFDataGroupInfo = class(TcxDataGroupInfo)
  private
    FChildGroupDictionary: TdxEMFDataGroupInfoDictionary;
    FParentGroup: TdxEMFDataGroupInfo;
    FDataGroup: TdxEMFDataGroup;
    function GetDisplayText: string; inline;
    function GetOwner: TdxEMFDataGroups; inline;
    function GetGroupValue: Variant;
    procedure SetDataGroup(const Value: TdxEMFDataGroup);
    function GetIsInconsistent: Boolean;
  protected
    procedure AdjustByInsertingRecord(ARecordIndex: Integer;
      AAppending: Boolean);

    procedure ClearData; virtual;
    function IsLastLevel: Boolean; virtual;
    function IsTopGroupInfo: Boolean; virtual;
    procedure SetExpanded(const Value: Boolean);

    procedure AddChildGroup(const Value: TdxEMFDataGroupInfo);
    function FindChildGroup(const AGroupValue: Variant): TdxEMFDataGroupInfo;

    property DataGroup: TdxEMFDataGroup read FDataGroup write SetDataGroup;
    property DisplayText: string read GetDisplayText;
    property IsInconsistent: Boolean read GetIsInconsistent;
    property Owner: TdxEMFDataGroups read GetOwner;
  public
    constructor Create(AOwner: TcxDataGroups;
      AParentGroup: TdxEMFDataGroupInfo);
    destructor Destroy; override;

    property ParentGroup: TdxEMFDataGroupInfo read FParentGroup;
    property GroupValue: Variant read GetGroupValue;
  end;

  { TdxEMFDataTopGroupInfo }

  TdxEMFDataTopGroupInfo = class(TdxEMFDataGroupInfo)
  protected
    procedure Clear;
    procedure ClearData; override; final;
    function IsLastLevel: Boolean; override;
    function IsTopGroupInfo: Boolean; override;
  public
    constructor Create(AOwner: TcxDataGroups); reintroduce;
    destructor Destroy; override;
  end;

  { TdxEMFDataGroups }

  TdxEMFDataGroups = class(TcxDataGroups)
  strict private
    FIsFullExpanding: Boolean;
    FTopGroupInfo: TdxEMFDataTopGroupInfo;
    function GetDataControllerInfo: TdxEMFDataControllerInfo; inline;
    function GetIsInconsistent: Boolean;
    function GetItem(Index: Integer): TdxEMFDataGroupInfo; inline;
    function GetRecordCount: Integer; inline;
    function GetTopGroupInfo: TdxEMFDataTopGroupInfo; inline;
  protected
    function GetFirstDataRecordListIndex(AInfo: TcxDataGroupInfo)
      : Integer; override;
    function GetLastDataRecordListIndex(AInfo: TcxDataGroupInfo)
      : Integer; override;

    procedure CreateChildren(AParentGroup: TdxEMFDataGroupInfo);
    procedure CreateGroups; overload;
    procedure CreateGroups(AListGroup: TList<TdxEMFDataGroup>;
      AParentGroup: TdxEMFDataGroupInfo); overload;
    procedure CreateTopGroupInfo;
    function GetDataGroupInfoByRecordIndex(ARecordIndex: Integer)
      : TdxEMFDataGroupInfo;
    function GetIndexByRecordIndex(ARecordIndex: Integer): Integer;
    function GetEntityByRecordIndex(ARecordIndex: Integer): TObject;
    function GetRecordIndex(AObject: TObject): Integer;

    procedure ClearData;
    procedure SoftClearData;
    procedure ClearTopGroupInfo;
    procedure Clear; override;
    function HasTopGroupInfo: Boolean;

    property DataControllerInfo: TdxEMFDataControllerInfo
      read GetDataControllerInfo;
    property IsInconsistent: Boolean read GetIsInconsistent;
    property RecordCount: Integer read GetRecordCount;
    property TopGroupInfo: TdxEMFDataTopGroupInfo read GetTopGroupInfo;
  public
    destructor Destroy; override;
    procedure ChangeExpanding(ARowIndex: Integer;
      AExpanded, ARecursive: Boolean); override;
    procedure FullExpanding(AExpanded: Boolean); override;
    function GetChildCount(AIndex: Integer): Integer; override;
    function GetChildIndex(AParentIndex, AIndex: Integer): Integer; override;
    procedure Rebuild; override;

    property Items[Index: Integer]: TdxEMFDataGroupInfo read GetItem; default;
  end;

  { TdxEMFDataSelection }

  TdxEMFDataSelection = class(TcxDataSelection);

  { TdxEMFDataFocusingInfo }

  TdxEMFDataFocusingInfo = class(TcxDataFocusingInfo)
  strict private
    FData: TObject;
    FGroupValue: Variant;
    procedure ValidateByData;
    procedure ValidateByGroupValue;
    procedure CalculateGroupValue(ADataGroupIndex: Integer);
    function GetDataControllerInfo: TdxEMFDataControllerInfo; inline;
  protected
    procedure Assign(AFocusingInfo: TcxDataFocusingInfo); override;
    function IsEqual(AFocusingInfo: TcxDataFocusingInfo): Boolean; override;
    procedure SavePos; override;
    procedure Validate;

    procedure DataClear;
    function IsGroup: Boolean;
    function IsUndefined: Boolean;
    procedure Clear; override;
    procedure ResetPos; override;
    procedure SetRowData(AData: TObject);

    property Data: TObject read FData;
    property DataControllerInfo: TdxEMFDataControllerInfo
      read GetDataControllerInfo;
  end;

  { TcxEMFDataControllerInfo }

  TdxEMFDataControllerInfo = class(TcxCustomDataControllerInfo)
  strict private
    function GetDataController: TdxEMFDataController; inline;
    function GetDataGroups: TdxEMFDataGroups; inline;
    function GetFocusingInfo: TdxEMFDataFocusingInfo; inline;
    function GetSelection: TdxEMFDataSelection; inline;
    function GetInMemoryMode: Boolean; inline;
    function IsDataSourceValid: Boolean;
    function IsUpdateDataSourceNeeded: Boolean;
    procedure PopulateSummary(ASummaryItems: TcxDataSummaryItems;
      ARow: TdxSelectStatementResultRow;
      var ACountValues: TcxDataSummaryCountValues;
      var ASummaryValues: TcxDataSummaryValues); inline;
    procedure UpdateDataSource(ASummaryChanged: Boolean = False);
  protected
    procedure CheckExpanding; override;
    procedure CheckFocusing; override;
    procedure CreateGroups; override;
    procedure DoFilter; override;
    procedure DoLoad; override;
    procedure DoUpdate(ASummaryChanged: Boolean); override;
    function FindDataGroup(ARecordListIndex: Integer): Integer; override;
    function FindFocusedRow(ANearest: Boolean): Integer; override;
    function GetDataFocusingInfoClass: TcxDataFocusingInfoClass; override;
    function GetDataGroupsClass: TcxDataGroupsClass; override;
    function GetInternalRecordCount: Integer; override;
    function GetRecordListIndexByFocusingInfo: Integer; override;
    function GetRecordIndex(AObject: TObject): Integer;
    function GetFixedBottomRowCount: Integer; override;
    function GetFixedTopRowCount: Integer; override;
    procedure ResetFocusing; override;
    procedure SaveExpanding(ASaveStates: TcxDataExpandingInfoStateSet);
      override;

    procedure CalculateSummary(ASummaryItems: TcxDataSummaryItems;
      var ACountValues: TcxDataSummaryCountValues;
      var ASummaryValues: TcxDataSummaryValues); overload;
    function GetEMFSummaryInfo(AParentGroup: TdxEMFDataGroup)
      : TArray<TdxEMFSummaryItem>;
    function GetCollection(AParentGroup: TdxEMFDataGroup): IdxEMFCollection;
    function GetSummaryProperties(AParentGroup: TdxEMFDataGroup)
      : TArray<IdxCriteriaOperator>;
    function GetGroupInfoCollection(AParentGroup: TdxEMFDataGroup)
      : IdxEMFCollection;
    function GetGroupFields(ALevel: Integer): TArray<TdxEMFDataField>;
    function FindFocusedDataGroupIndex: Integer;
    function IsInsertInEmpty: Boolean;
    procedure PrepareTopGroupInfo(ADataGroupInfo: TdxEMFDataGroupInfo);
    procedure PopulateFilterValues(AList: TcxDataFilterValueList;
      ADataField: TdxEMFDataField; ACriteria: TcxFilterCriteria;
      var AUseFilteredRecords: Boolean; out ANullExists: Boolean);

    procedure DeleteEntity(ARecordIndex: Integer);
    function GetEntityByRecordIndex(ARecordIndex: Integer): TObject;
    function GetInternalRecordListIndex(ARecordIndex: Integer)
      : Integer; override;

    property FocusingInfo: TdxEMFDataFocusingInfo read GetFocusingInfo;
    property InMemoryMode: Boolean read GetInMemoryMode;
  public
    function GetRowInfo(ARowIndex: Integer): TcxRowInfo; override;

    function GetInternalRecordIndex(ARecordListIndex: Integer)
      : Integer; override;

    property DataController: TdxEMFDataController read GetDataController;
    property DataGroups: TdxEMFDataGroups read GetDataGroups;
    property Selection: TdxEMFDataSelection read GetSelection;
  end;

  { TcxEMFDataControllerSearch }

  TdxEMFDataControllerSearch = class(TcxDataControllerSearch)
  end;

  { TdxEMFDataControllerGroups }

  TdxEMFDataControllerGroups = class(TcxDataControllerGroups)
  strict private
    function GetDataController: TdxEMFDataController; inline;
    function GetDataGroups: TdxEMFDataGroups; inline;
  protected
    function GetCriteria(ARowIndex: Integer): IdxCriteriaOperator;
    function GetGroupDisplayText(ADataGroupIndex: TcxDataGroupIndex)
      : string; override;
    function GetGroupValue(ADataGroupIndex: TcxDataGroupIndex)
      : Variant; override;

    property DataGroups: TdxEMFDataGroups read GetDataGroups;
  public
    property DataController: TdxEMFDataController read GetDataController;
  end;

  { TdxEMFDataProvider }

  TdxEMFDataProvider = class(TcxCustomDataProvider)
  strict private
    FInCanInitEditing: Boolean;
    FCurrentIndex: Integer;
    FCollection: IdxEMFCollection;
    FEditingRow: TdxEMFEditingRow;
    FIsAppending: Boolean;
    procedure AddRecord(AIsAppending: Boolean);
    function GetDataController: TdxEMFDataController; inline;
    function GetDataSource: TdxEMFDataControllerCustomDataSource; inline;
    function GetDataControllerInfo: TdxEMFDataControllerInfo; inline;
    function GetEntityInfo: TdxEntityInfo; inline;
    function GetSession: TdxEMFCustomSession; inline;

    procedure ClearEditingRow; inline;
  protected
    function GetFieldList(const AFieldNames: string)
      : TArray<TdxMappingMemberInfo>;
    function GetCurrentObject: TObject;
    function GetObject(ARecordIndex: Integer): TObject;
    function GetRowValue(ARecordIndex: Integer;
      AField: TdxEMFDataField): TValue;
    function GetRowValueAsVariant(ARecordIndex: Integer;
      AField: TdxEMFDataField): Variant;
    function GetValueDefReaderClass: TcxValueDefReaderClass; override;
    function IsActive: Boolean; override;
    procedure Append; override;
    function CanDelete: Boolean; override;
    procedure Cancel; override;
    procedure Delete; override;
    procedure DeleteRecord(ARecordIndex: Integer); override;
    procedure DeleteRecords(ACriteria: IdxCriteriaOperator);
      reintroduce; overload;
    procedure Edit; override;
    function GetRecordIndex: Integer; override;
    procedure Post(AForcePost: Boolean = False); override;
    procedure PostEditingData; override;
    procedure RecordChanged; virtual;
    procedure InternalCancel;
    procedure InternalPost; virtual;
    procedure Insert; override;
    procedure ResetEditing; override;
    function SetEditingData(AField: TdxEMFDataField; ARecordIndex: Integer;
      const AValue: Variant): Boolean;
    function SetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField;
      const AValue: Variant; AEditValueSource: TcxDataEditValueSource)
      : Boolean; override;
    procedure UpdateData;
    procedure First; override;
    procedure Prev; override;
    procedure Next; override;
    procedure Last; override;
    function IsBOF: Boolean; override;
    function IsEOF: Boolean; override;
    procedure DoUpdateData; override;
    procedure DoInsertingRecord(AIsAppending: Boolean); override;

    procedure AssignItemValue(ARecordIndex: Integer; AField: TcxCustomDataField;
      const AValue: Variant); override;
    function CanInitEditing(ARecordIndex: Integer): Boolean; override;

    property DataController: TdxEMFDataController read GetDataController;
    property DataControllerInfo: TdxEMFDataControllerInfo
      read GetDataControllerInfo;
    property DataSource: TdxEMFDataControllerCustomDataSource
      read GetDataSource;
    property EntityInfo: TdxEntityInfo read GetEntityInfo;
    property Session: TdxEMFCustomSession read GetSession;

    property IsAppending: Boolean read FIsAppending;
    property InCanInitEditing: Boolean read FInCanInitEditing;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    destructor Destroy; override;
  end;

  { TdxValueDefEMFReader }

  TdxValueDefEMFReader = class(TcxValueDefReader)
  public
    function GetDisplayText(AValueDef: TcxValueDef): string; override;
    function GetValue(AValueDef: TcxValueDef): Variant; override;
    function IsInternal(AValueDef: TcxValueDef): Boolean; override;
    procedure Read(ABuffer: PAnsiChar; AValueDef: TcxValueDef); override;
  end;

  { TcxEMFDataRelationList }

  TcxEMFDataRelationList = class(TcxCustomDataRelationList)
  private
    FDetailObjects: TDictionary<TObject, TcxDetailObject>;
    function GetDataController: TdxEMFDataController; inline;
    function GetDetailObjectInfo(ARecordIndex: Integer)
      : TPair<TObject, TcxDetailObject>;
  protected
    procedure ChangeDetailObjectKey(AOldKey, ANewKey: TObject);
    procedure ClearDetailLinkObjects;
    procedure DeleteDetailObject(AEntity: TObject);
    function GetValueAsDetailObject(ARecordIndex: Integer)
      : TcxDetailObject; override;
    property DataController: TdxEMFDataController read GetDataController;
  public
    constructor Create(ADataController: TcxCustomDataController);
    destructor Destroy; override;
    function GetDetailObject(ARecordIndex: Integer): TcxDetailObject; override;
    property DetailObjects: TDictionary<TObject, TcxDetailObject>
      read FDetailObjects;
  end;

  { TcxEMFDetailObject }

  TcxEMFDetailObject = class(TcxDetailObject);

  { TdxEMFDataController }

  TdxEMFDataController = class(TcxCustomDataController)
  strict private
    FCollectionName: string;
    FDataSource: TdxEMFDataControllerCustomDataSource;
    FDataChangedFlag: Boolean;
    FDetailKeyFieldNames: string;
    FFreeNotificator: TcxFreeNotificator;
    FMasterKeyFieldNames: string;
    FMasterDetailKeyFields: TArray<TdxMappingMemberInfo>;
    FMasterDetailKeyValues: TArray<TValue>;
    FPrevFocusedRecordIndex: Integer;
    FSummaryChanged: Boolean;
    function GetField(Index: Integer): TdxEMFDataField; inline;
    function GetFieldsCount: Integer; inline;
    function GetFreeNotificator: TcxFreeNotificator;
    function GetProvider: TdxEMFDataProvider; inline;
    function GetDataControllerInfo: TdxEMFDataControllerInfo; inline;
    function GetDetailDataController(ADetailObject: TcxDetailObject;
      ARelationIndex: Integer): TcxCustomDataController; overload;
    function GetGroups: TdxEMFDataControllerGroups; inline;
    function GetFilter: TdxEMFDataFilterCriteria; inline;
    function GetSummaryChangedFlag: Boolean;
    function GetHasChanges: Boolean;
    function GetRelations: TcxEMFDataRelationList; inline;
    function GetEMFMasterRelation: TdxEMFDataRelation; inline;
    function GetInMemoryMode: Boolean;
    function GetMasterDetailKeyFields: TArray<TdxMappingMemberInfo>;
    function GetMasterDetailKeyFieldNames: string; inline;
    function GetSession: TdxEMFCustomSession; inline;
    procedure SetCollectionName(const Value: string);
    procedure SetDataSource(const Value: TdxEMFDataControllerCustomDataSource);
    procedure SetDetailKeyFieldNames(const Value: string);
    procedure SetEMFMasterRelation(const Value: TdxEMFDataRelation);
    procedure SetMasterKeyFieldNames(const Value: string);
    procedure UpdateField(ADataField: TdxEMFDataField;
      const AFieldNames: string; AIsLookup: Boolean);
    procedure UpdateRelationFields;
    class function IsDetailDataControllerExist(ADetailObject: TcxDetailObject;
      ARelationIndex: Integer): Boolean; overload; static;
  private
    FMasterEntity: TObject;
  protected
    class function GetValueTypeClassByMember(AMember: TdxMappingMemberInfo)
      : TcxValueTypeClass;

    function CreateDataControllerInfo: TcxCustomDataControllerInfo; override;
    function CreateDataRelationList: TcxCustomDataRelationList; override;
    function CreateFindCriteria: TcxDataFindCriteria; override;
    procedure DataSourceChangedHandler(Sender: TObject);
    function GetDataProviderClass: TcxCustomDataProviderClass; override;
    procedure FreeNotification(Sender: TComponent); virtual;
    function GetDataSelectionClass: TcxDataSelectionClass; override;
    function GetFieldClass: TcxCustomDataFieldClass; override;
    function GetFilterCriteriaClass: TcxDataFilterCriteriaClass; override;
    function GetGroupRowValueByItemIndex(const ARowInfo: TcxRowInfo;
      AItemIndex: Integer): Variant; override;
    function GetInternalDisplayText(ARecordIndex: Integer;
      AField: TcxCustomDataField): string; override;
    function GetInternalValue(ARecordIndex: Integer; AField: TcxCustomDataField)
      : Variant; override;
    function GetRelationClass: TcxCustomDataRelationClass; override;
    function GetSummaryClass: TcxDataSummaryClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    function IsMergedGroupsSupported: Boolean; override;
    function IsSmartLoad: Boolean; override;
    function NeedPrepareGroupValue(AEMFDataField: TdxEMFDataField)
      : Boolean; virtual;
    procedure PrepareField(AField: TcxCustomDataField); override;
    procedure ResetEntityInfo;

    function CanIgnoreTimeForFiltering(ADataField: TdxEMFDataField)
      : Boolean; virtual;
    procedure InitializeDateTimeGrouping(ADataField: TdxEMFDataField); virtual;

    function AppendInSmartLoad: Integer; override;
    procedure DoUpdateRecord(ARecordIndex: Integer); inline;
    procedure UpdateEditingRecord;
    procedure UpdateInternalField(const AFieldName: string;
      var AField: TdxEMFDataField);

    procedure ChangeFieldName(AItemIndex: Integer; const AFieldName: string);
    function DoGetGroupRowDisplayText(const ARowInfo: TcxRowInfo;
      var AItemIndex: Integer): string; override;
    function GetItemByFieldName(const AFieldName: string): TObject;
    function GetItemFieldName(AItemIndex: Integer): string; inline;
    function GetGroupsClass: TcxDataControllerGroupsClass; override;

    //
    function GetCriteria: IdxCriteriaOperator;
    function GetSortByExpressions: IdxSortByExpressions;
    function GetMemberInfo(AField: TdxEMFDataField): TdxMappingMemberInfo;

    function CanLoadData: Boolean; override;
    function CheckDetailsBrowseMode: Boolean; override;
    procedure ClearData;
    procedure ClearDetailsMasterRelation(ARelation
      : TcxCustomDataRelation); override;
    procedure DataSourceChanged;
    function GetClearDetailsOnCollapse: Boolean; override;
    function GetIsProviderMode: Boolean; override;
    function IsProviderDataSource: Boolean; override;
    function IsConversionNeededForCompare(AField: TcxCustomDataField)
      : Boolean; override;
    procedure LoadStorage; override;
    procedure SoftClearData;
    procedure UpdateRelations(ARelation: TcxCustomDataRelation); override;

    function PostEditingEntity(ARecordIndex: Integer;
      AEditingRow: TdxEMFEditingRow): TObject;
    procedure PopulateFilterValues(AList: TcxDataFilterValueList;
      AItemIndex: Integer; ACriteria: TcxFilterCriteria;
      var AUseFilteredRecords: Boolean; out ANullExists: Boolean;
      AUniqueOnly: Boolean; AFilteredRecordsShowFilteredItemsOnly
      : Boolean); override;
    function CanFocusRecord(ARecordIndex: Integer): Boolean; override;
    procedure DeleteInSmartLoad(ARecordIndex: Integer); override;

    procedure ActiveChanged(AActive: Boolean); override;
    procedure SummaryChanged(ARedrawOnly: Boolean); override;
    function GetRecordIndex: Integer; override;

    property FieldsCount: Integer read GetFieldsCount;
    property Fields[Index: Integer]: TdxEMFDataField read GetField;
    property MasterDetailKeyFields: TArray<TdxMappingMemberInfo>
      read GetMasterDetailKeyFields;
    property MasterDetailKeyFieldNames: string
      read GetMasterDetailKeyFieldNames;
    property MasterEntity: TObject read FMasterEntity;
    property MasterRelation: TdxEMFDataRelation read GetEMFMasterRelation
      write SetEMFMasterRelation;
    property Session: TdxEMFCustomSession read GetSession;

    property FreeNotificator: TcxFreeNotificator read GetFreeNotificator;
    property DataChangedFlag: Boolean read FDataChangedFlag
      write FDataChangedFlag;
    property SummaryChangedFlag: Boolean read GetSummaryChangedFlag;
    property HasChanges: Boolean read GetHasChanges;
    property CollectionName: string read FCollectionName
      write SetCollectionName;
    property DetailKeyFieldNames: string read FDetailKeyFieldNames
      write SetDetailKeyFieldNames;
    property MasterKeyFieldNames: string read FMasterKeyFieldNames
      write SetMasterKeyFieldNames;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Cancel; override;
    procedure CheckBrowseMode; override;
    procedure ClearDetails; override;
    function GetRecordCount: Integer; override;
    function InsertRecord(ARecordIndex: Integer): Integer; override;
    procedure SetMasterRelation(AMasterRelation: TcxCustomDataRelation;
      AMasterRecordIndex: Integer); override;
    procedure UpdateData; override;
    procedure ResetHasChildrenFlag; override;
    procedure DeleteFocused; override;
    procedure DeleteSelection; override;
    function ChangeFocusedRowIndex(ARowIndex: Integer): Boolean; override;

    property InMemoryMode: Boolean read GetInMemoryMode;
    property Groups: TdxEMFDataControllerGroups read GetGroups;
    property DataControllerInfo: TdxEMFDataControllerInfo
      read GetDataControllerInfo;
    property DataSource: TdxEMFDataControllerCustomDataSource read FDataSource
      write SetDataSource;
    property Filter: TdxEMFDataFilterCriteria read GetFilter;
    property Provider: TdxEMFDataProvider read GetProvider;
    property Relations: TcxEMFDataRelationList read GetRelations;
  end;

implementation

uses
  DB, Math, DateUtils, Variants, RTLConsts,
  cxClasses, dxCore, cxControls, cxVariants, cxDB, cxDateUtils,
  dxStringHelper, // cxEMFEntityEditor,
  dxEMF.Utils, dxEMF.Strs, dxEMF.Core.Collections, dxEMF.DB.Utils;

function BinaryOperatorCreate(const AProperty: IdxCriteriaOperator;
  const AValue: TValue;
  AType: TdxBinaryOperatorType = TdxBinaryOperatorType.Equal)
  : IdxCriteriaOperator; overload;
begin
  if AValue.IsNullOrEmpty then
    if AType = TdxBinaryOperatorType.Equal then
      Exit(TdxFunctionOperator.Create(TdxFunctionOperatorType.IsNull,
        [AProperty]))
    else if AType = TdxBinaryOperatorType.NotEqual then
      Exit(TdxUnaryOperator.Create(TdxUnaryOperatorType.&Not,
        TdxFunctionOperator.Create(TdxFunctionOperatorType.IsNull,
        [AProperty])));
  Result := TdxBinaryOperator.Create(AProperty,
    TdxOperandValue.Create(AValue), AType);
end;

function BinaryOperatorCreate(const APropertyName: string; const AValue: TValue;
  AType: TdxBinaryOperatorType = TdxBinaryOperatorType.Equal)
  : IdxCriteriaOperator; overload;
begin
  Result := BinaryOperatorCreate(TdxOperandProperty.Create(APropertyName),
    AValue, AType);
end;

function SummaryKindToAggregateFunctionType(ASummaryKind: TcxSummaryKind)
  : TdxAggregateFunctionType;
begin
  case ASummaryKind of
    skSum:
      Result := TdxAggregateFunctionType.Sum;
    skMin:
      Result := TdxAggregateFunctionType.Min;
    skMax:
      Result := TdxAggregateFunctionType.Max;
    skCount:
      Result := TdxAggregateFunctionType.Count;
    skAverage:
      Result := TdxAggregateFunctionType.Avg;
  else
    raise ENotSupportedException.Create('');
  end;
end;

function SortOrderToSortDirection(ASortOrder: TcxDataSortOrder)
  : TdxSortDirection;
begin
  if ASortOrder = TcxDataSortOrder.soDescending then
    Result := TdxSortDirection.Descending
  else
    Result := TdxSortDirection.Ascending;
end;

type
  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);
  TdxDataSummaryItemAccess = class(TcxDataSummaryItem);

  TdxDataSummaryItemHelper = class helper for TcxDataSummaryItem
  private
    function GetIsDataBinded: Boolean; inline;
  public
    property IsDataBinded: Boolean read GetIsDataBinded;
  end;

  { TdxSelectStatementResultRowDataGroupHelper }

  TdxSelectStatementResultRowDataGroupHelper = class helper for
    TdxSelectStatementResultRow
  public
    function GetGroupRecordCount: Integer;
    procedure UpdateSummary(ADataGroupInfo: TdxEMFDataGroup);
  end;

  { TcxEMFEntityValueType }

class procedure TcxEMFEntityValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
end;

{ TdxDataSummaryItemHelper }

function TdxDataSummaryItemHelper.GetIsDataBinded: Boolean;
begin
  Result := inherited IsDataBinded;
end;

{ TdxSelectStatementResultRowDataGroupHelper }

function TdxSelectStatementResultRowDataGroupHelper.GetGroupRecordCount
  : Integer;
begin
  Result := Values[0];
end;

procedure TdxSelectStatementResultRowDataGroupHelper.UpdateSummary
  (ADataGroupInfo: TdxEMFDataGroup);
var
  I, J: Integer;
  ADataSummary: TcxDataSummary;
  ASummaryItems: TcxDataSummaryItems;

  function GetSummaryInfosValues(ASummaryItems: TcxDataSummaryItems;
    var J: Integer): Variant;
  var
    I, ACount: Integer;
  begin
    Result := VarArrayCreate([0, ASummaryItems.Count], varVariant);
    ACount := Min(ASummaryItems.Count, Length(Values) - 2);
    for I := 0 to ACount - 1 do
    begin
      Result[I] := Values[J];
      Inc(J);
    end;
  end;

begin
  if (ADataGroupInfo.GroupedItemCount = 0) or
    ((Length(Values) - 2) < ADataGroupInfo.GroupedItemCount) then
  begin
    for I := 0 to ADataGroupInfo.GroupedItemCount - 1 do
      ADataGroupInfo.SummaryInfos[I].Values := Null;
  end
  else
  begin
    ADataSummary := ADataGroupInfo.Owner.DataControllerInfo.
      DataController.Summary;
    J := ADataGroupInfo.GroupedItemCount + 1;
    for I := 0 to ADataGroupInfo.GroupedItemCount - 1 do
    begin
      ASummaryItems := ADataSummary.GetGroupSummaryItems
        (ADataGroupInfo.Level, I);
      ADataGroupInfo.SummaryInfos[I].Values :=
        GetSummaryInfosValues(ASummaryItems, J);
    end;
  end;
end;

{ TcxEMFSummaryItem }

function TdxEMFSummaryItem.GetEMFDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFSummaryItem.GetEMFDataField: TdxEMFDataField;
begin
  Result := TdxEMFDataField(DataField);
end;

function TdxEMFSummaryItem.GetExpression: IdxCriteriaOperator;
begin
  Result := TdxAggregateOperand.Create(nil,
    TdxOperandProperty.Create(FieldName),
    SummaryKindToAggregateFunctionType(Kind), nil)
end;

function TdxEMFSummaryItem.AllowKind(Value: TcxSummaryKind): Boolean;
begin
  Result := not DataController.Active or DataController.IsLoading or
    (DataField = nil) or
    (Value in DataController.GetAllowedSummaryKinds(DataField));
end;

procedure TdxEMFSummaryItem.Assign(Source: TPersistent);
begin
  if Source is TdxEMFSummaryItem then
  begin
    BeginUpdate;
    try
      FieldName := TdxEMFSummaryItem(Source).FieldName;
      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxEMFSummaryItem.CanSetKind(Value: TcxSummaryKind): Boolean;
begin
  Result := inherited CanSetKind(Value) and AllowKind(Value);
end;

function TdxEMFSummaryItem.DataField: TcxCustomDataField;
begin
  if Assigned(FDataField) then
    Result := FDataField
  else
    Result := inherited DataField;
end;

function TdxEMFSummaryItem.GetField: TdxEMFDataField;
begin
  Result := TdxEMFDataField(inherited Field);
end;

function TdxEMFSummaryItem.GetFieldName: string;
begin
  if EMFDataField <> nil then
    Result := EMFDataField.FieldName
  else
    Result := FFieldName;
end;

function TdxEMFSummaryItem.IsCurrency(AVarType: TVarType): Boolean;
begin
  Result := inherited IsCurrency(AVarType);
end;

function TdxEMFSummaryItem.IsDataBinded: Boolean;
begin
  Result := inherited IsDataBinded and (EMFDataField.MemberInfo <> nil);
end;

procedure TdxEMFSummaryItem.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    DataController.UpdateInternalField(Value, FDataField);
    if not AllowKind(Kind) then
      Kind := skNone;
  end;
end;

{ TdxEMFDataControllerCustomDataSource }

procedure TdxEMFDataControllerCustomDataSource.Delete(const ACriteria
  : IdxCriteriaOperator);
begin
  raise ENotImplemented.Create('TdxEMFDataControllerCustomDataSource.Delete');
end;

function TdxEMFDataControllerCustomDataSource.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxEMFDataControllerCustomDataSource.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TdxEMFDataControllerCustomDataSource.GetIsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxEMFDataControllerCustomDataSource.ActiveChanged(AActive: Boolean);
begin
  if AActive then
  begin
    DoBeforeOpen;
    try
      InternalOpen;
      DoAfterOpen
    except
      InternalClose;
      raise;
    end;
  end
  else
  begin
    if not IsDestroying then
      DoBeforeClose;
    InternalClose;
    if not IsDestroying then
      DoAfterClose;
  end;
end;

procedure TdxEMFDataControllerCustomDataSource.CheckSession;
begin
  if Session = nil then
    DatabaseError(sdxMissingSession, Self);
end;

procedure TdxEMFDataControllerCustomDataSource.CreateClientList;
begin
  FControllers := TList<TdxEMFDataController>.Create;
end;

procedure TdxEMFDataControllerCustomDataSource.DestroyClientList;
begin
  FreeAndNil(FControllers);
end;

procedure TdxEMFDataControllerCustomDataSource.DoAfterClose;
begin
  if Assigned(FAfterClose) then
    FAfterClose(Self);
end;

procedure TdxEMFDataControllerCustomDataSource.DoAfterOpen;
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TdxEMFDataControllerCustomDataSource.DoBeforeClose;
begin
  if Assigned(FBeforeClose) then
    FBeforeClose(Self);
end;

procedure TdxEMFDataControllerCustomDataSource.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

function TdxEMFDataControllerCustomDataSource.EntityHasProperty
  (AObject: TObject; const AProperties: TArray < TPair < string, TValue >>
  ): Boolean;
var
  I: Integer;
  AProperty: TPair<string, TValue>;
  AMemberValue: TValue;
begin
  if Length(AProperties) = 0 then
    Exit(False);
  for AProperty in AProperties do
  begin
    I := EntityInfo.MemberAttributes.IndexOfName(AProperty.Key);
    if I < 0 then
      Exit(False);
    AMemberValue := EntityInfo.MemberAttributes[I].GetValue(AObject);
    if not AMemberValue.Equals(AProperty.Value) then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxEMFDataControllerCustomDataSource.InitializeDataSource;
begin
end;

procedure TdxEMFDataControllerCustomDataSource.InternalOpen;
begin
  InitializeDataSource;
end;

procedure TdxEMFDataControllerCustomDataSource.NotifyClients;
begin
  inherited NotifyClients;
  if not FChangedHandlers.Empty then
    FChangedHandlers.Invoke(Self);
end;

procedure TdxEMFDataControllerCustomDataSource.RegisterController
  (ADataController: TdxEMFDataController);
begin
  FControllers.Add(ADataController);
  FChangedHandlers.Add(ADataController.DataSourceChangedHandler);
end;

procedure TdxEMFDataControllerCustomDataSource.UnregisterController
  (ADataController: TdxEMFDataController);
begin
  if FControllers <> nil then
  begin
    FControllers.Remove(ADataController);
    FChangedHandlers.Remove(ADataController.DataSourceChangedHandler);
  end;
end;

{ TcxEMFDataSource }

function TdxEMFDataSource.CreateNewEntity: TObject;
begin
  Result := Session.CreateObject(EntityClass);
end;

procedure TdxEMFDataSource.InternalOpen;
begin
  CheckSession;
  if EntityInfo = nil then
  begin
    DoAssignEntity;
    CheckEntityInfo;
  end;
  inherited InternalOpen;
end;

function TdxEMFDataSource.PostEditingEntity(AObject: TObject;
  AEditingRow: TdxEMFEditingRow): TObject;
var
  AItem: TPair<TdxMappingMemberInfo, Variant>;
begin
  if AObject = nil then
    AObject := CreateNewEntity;
  Result := AObject;
  for AItem in AEditingRow do
    AItem.Key.SetAsVariant(AObject, AItem.Value);
end;

{ TcxEMFDataField }

destructor TdxEMFDataField.Destroy;
begin
  ClearFields;
  inherited Destroy;
end;

procedure TdxEMFDataField.ClearFields;
var
  AField: TdxEMFDataField;
begin
  for AField in FFields do
    AField.Free;
end;

function TdxEMFDataField.IsDateTimeField: Boolean;
begin
  Result := (MemberInfo <> nil) and
    (MemberInfo.DBColumnType = TdxDBColumnType.DateTime);
end;

function TdxEMFDataField.IsDateTimeGrouping: Boolean;
begin
  Result := IsDateTimeField and
    (DateTimeGrouping in [dtgByDateAndTime, dtgRelativeToToday, dtgByHour,
    dtgByDate, dtgByMonth, dtgByYear]);
end;

function TdxEMFDataField.DateTimeGroupingByRelativeToToday(const AOperand
  : IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Iif,
    [TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalLaterThisWeek, AOperand),
    // DayAfterTomorrow <= x < NextWeek
    TdxFunctionOperator.Create(TdxFunctionOperatorType.
    LocalDateTimeDayAfterTomorrow, []),
    TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalLaterThisMonth, AOperand), // TwoWeeksAway <= x < NextMonth
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeNextWeek,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalNextWeek, AOperand), // NextWeek <= x < TwoWeeksAway
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeNextWeek,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalLaterThisYear, AOperand), // NextMonth <= x < NextYear
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeNextMonth,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalBeyondThisYear, AOperand), // NextYear <= x
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeNextYear,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalTomorrow, AOperand), // Tomorrow <= x < DayAfterTomorrow
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeTomorrow,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalToday, AOperand), // Today <= x < Tomorrow
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeToday, []),
    TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalYesterday, AOperand), // Yesterday <= x < Today
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeYesterday,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalEarlierThisWeek, AOperand), // ThisWeek <= x < Yesterday
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeThisWeek,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalLastWeek, AOperand), // LastWeek <= x < ThisWeek
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeLastWeek,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalEarlierThisMonth, AOperand), // ThisMonth <= x < LastWeek
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeThisMonth,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalEarlierThisYear, AOperand), // ThisYear <= x < ThisMonth
    TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeThisYear,
    []), TdxFunctionOperator.Create(TdxFunctionOperatorType.
    IsOutlookIntervalPriorThisYear, AOperand), // x < ThisYear
    TdxFunctionOperator.Create(TdxFunctionOperatorType.IncYear,
    [TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeThisYear,
    []), TdxConstantValue.Create(-1)]), AOperand]);
end;

function TdxEMFDataField.GetGroupCriteria: IdxCriteriaOperator;
var
  AOperand: IdxCriteriaOperator;
begin
  DataController.InitializeDateTimeGrouping(Self);
  AOperand := TdxOperandProperty.Create(FieldName);
  if not IsDateTimeGrouping then
    Exit(AOperand);
  case DateTimeGrouping of
    dtgRelativeToToday:
      Result := DateTimeGroupingByRelativeToToday(AOperand);
    dtgByHour:
      Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.HourOf,
        [AOperand]);
    dtgByDate:
      Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DateOf,
        [AOperand]);
    dtgByMonth:
      Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DateOf,
        [TdxFunctionOperator.Create(TdxFunctionOperatorType.IncDay,
        [AOperand, TdxBinaryOperator.Create(TdxConstantValue.Create(1),
        TdxFunctionOperator.Create(TdxFunctionOperatorType.DayOf, [AOperand]),
        TdxBinaryOperatorType.Minus)])]);
    dtgByYear:
      Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.YearOf,
        [AOperand]);
  else
    Result := AOperand;
  end;
end;

function TdxEMFDataField.GetCriteria(AIsGroup: Boolean = False)
  : IdxCriteriaOperator;
begin
  if AIsGroup then
    Result := GetGroupCriteria
  else
  begin
    Result := TdxOperandProperty.Create(FieldName);
    if DataController.CanIgnoreTimeForFiltering(Self) then
      Result := TdxFunctionOperator.Create
        (TdxFunctionOperatorType.DateOf, Result);
  end;
end;

function TdxEMFDataField.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFDataField.GetFieldCount: Integer;
begin
  if ReferenceField <> nil then
    Result := (ReferenceField as TdxEMFDataField).FieldCount
  else
    Result := Length(FFields);
end;

function TdxEMFDataField.GetMemberInfo: TdxMappingMemberInfo;
begin
  if ReferenceField <> nil then
    Result := (ReferenceField as TdxEMFDataField).MemberInfo
  else
    Result := FMember;
end;

function TdxEMFDataField.GetMembers: TArray<TdxMappingMemberInfo>;
var
  AResult: TdxArray<TdxMappingMemberInfo>;
  AField: TdxEMFDataField;
  ACount: Integer;
begin
  ACount := FieldCount;
  if ACount = 0 then
    Result := TArray<TdxMappingMemberInfo>.Create(MemberInfo)
  else
  begin
    AResult := TdxArray<TdxMappingMemberInfo>.Create(FieldCount);
    for AField in FFields do
      AResult.AddRange(AField.GetMembers);
    Result := AResult.ToArray;
  end;
end;

function TdxEMFDataField.GetValues(AObject: TObject): TArray<TValue>;
var
  ACount: Integer;
  AResult: TdxArray<TValue>;
  AField: TdxEMFDataField;
begin
  ACount := FieldCount;
  if ACount = 0 then
    if MemberInfo = nil then
      Result := TArray<TValue>.Create(TValue.Empty)
    else
      Result := MemberInfo.GetValues(AObject)
  else
  begin
    AResult := TdxArray<TValue>.Create(ACount);
    for AField in FFields do
      AResult.AddRange(AField.GetValues(AObject));
    Result := AResult.ToArray;
  end;
end;

function TdxEMFDataField.IsMemberFormatted(AMember: TdxMappingMemberInfo;
  AIsTextEdit: Boolean): Boolean;
begin
  if AMember = nil then
    Exit(False);
  case AMember.FieldType of
    ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
      ftDBaseOle, ftTypedBinary, ftCursor, ftADT, ftArray, ftReference,
      ftDataSet, ftBoolean:
      Result := AIsTextEdit;
  else
    Result := True;
  end;
end;

procedure TdxEMFDataField.SetFieldName(const Value: string);
begin
  if FFieldName = Value then
    Exit;
  FFieldName := Value;
  DataController.PrepareField(Self);
end;

procedure TdxEMFDataField.SetMemberInfo(const Value: TdxMappingMemberInfo);
begin
  if ReferenceField <> nil then
    Exit;
  if FMember <> Value then
  begin
    FMember := Value;
    Changed;
  end;
end;

procedure TdxEMFDataField.SetPropertiesByMember(AMember: TdxMappingMemberInfo;
  AMemberChanged: Boolean);
var
  AValueTypeClass: TcxValueTypeClass;
  ATextStored, AChanged, AValueTypeClassChanged: Boolean;
begin
  if (ReferenceField <> nil) or (AMember = nil) then
    Exit;
  repeat
    if not IsInternal then
      ATextStored := IsMemberFormatted(AMember,
        DataController.GetItemValueSource(Index) = evsText)
    else
      ATextStored := IsMemberFormatted(AMember, False);
    AValueTypeClass := TdxEMFDataController.GetValueTypeClassByMember(AMember);
    AValueTypeClassChanged := (ValueTypeClass <> AValueTypeClass);
    AChanged := AValueTypeClassChanged or (TextStored <> ATextStored);
    if AChanged then
    begin
      BeginRecreateData;
      try
        TextStored := ATextStored;
        ValueTypeClass := AValueTypeClass;
      finally
        EndRecreateData;
      end;
      if AValueTypeClassChanged then
      begin
        DoPropertiesChanged;
        AMemberChanged := False;
      end;
    end
    else if AMemberChanged then
      DoPropertiesChanged;
  until not AChanged;
end;

function TdxEMFDataField.TryCastVariant(const AValue: Variant): Variant;
var
  AStr: string;
  ADateTime: TDateTime;
begin
  if VarIsSoftEmpty(AValue) then
    Exit(AValue);
  if IsDateTimeField and (VarType(AValue) <> varDate) then
  begin
    if VarIsStr(AValue) then
    begin
      AStr := VarToStr(AValue);
      if Pos('-', AStr) > 0 then
        ADateTime := TdxISO8601Helper.StringToDateTime(AStr)
      else
        ADateTime := cxTextToDateTime(AStr);
      Result := ADateTime;
    end
    else
      Result := VarToDateTime(AValue)
  end
  else
    Result := AValue;
end;

{ TdxEMFDataRelation }

destructor TdxEMFDataRelation.Destroy;
begin
  FreeAndNil(FMasterKeyField);
  inherited Destroy;
end;

function TdxEMFDataRelation.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFDataRelation.GetDetailDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DetailDataController);
end;

function TdxEMFDataRelation.GetDataSource: TdxEMFDataControllerCustomDataSource;
begin
  Result := DataController.DataSource;
end;

function TdxEMFDataRelation.GetRelationDataController: TdxEMFDataController;
var
  ADataController: TcxCustomDataController;
begin
  ADataController := DataController.GetPatternDataController;
  if ADataController <> nil then
    ADataController := ADataController.Relations[Self.Index]
      .DetailDataController;
  if ADataController is TdxEMFDataController then
    Result := TdxEMFDataController(ADataController)
  else
    Result := nil;
end;

function TdxEMFDataRelation.GetDetailKeyFieldNames: string;
var
  ADataController: TdxEMFDataController;
begin
  ADataController := RelationDataController;
  if ADataController <> nil then
    Result := ADataController.DetailKeyFieldNames
  else
    Result := '';
end;

function TdxEMFDataRelation.GetMasterKeyFieldCount: Integer;
begin
  if FMasterKeyField = nil then
    Result := 0
  else
  begin
    Result := FMasterKeyField.FieldCount;
    if Result = 0 then
      Result := 1;
  end;
end;

function TdxEMFDataRelation.GetMasterKeyFieldNames: string;
var
  ADataController: TdxEMFDataController;
begin
  ADataController := RelationDataController;
  if ADataController <> nil then
    Result := ADataController.MasterKeyFieldNames
  else
    Result := '';
end;

function TdxEMFDataRelation.GetMasterKeyFields: TArray<TdxMappingMemberInfo>;
begin
  if FMasterKeyFields = nil then
    FMasterKeyFields := FMasterKeyField.GetMembers;
  Result := FMasterKeyFields;
end;

function TdxEMFDataRelation.GetMasterRecordValue(ARecordIndex: Integer)
  : TArray<TValue>;
var
  AObject: TObject;
begin
  DataController.CheckRecordRange(ARecordIndex);
  AObject := DataController.Provider.GetObject(ARecordIndex);
  if (AObject = nil) or (FMasterKeyField = nil) then
    Result := nil
  else
    Result := FMasterKeyField.GetValues(AObject);
end;

function TdxEMFDataRelation.IsLinked: Boolean;
begin
  Result := (DetailDataController <> nil) and
    (DetailDataController.InMemoryMode or ((FMasterKeyField <> nil) and
    (DetailKeyFieldNames <> '')));
end;

procedure TdxEMFDataRelation.RemoveDataField(ADataField: TcxCustomDataField);
begin
  inherited RemoveDataField(ADataField);
  if FMasterKeyField = ADataField then
    FMasterKeyField := nil;
end;

procedure TdxEMFDataRelation.SetMasterKeyField(AMasterKeyField
  : TdxEMFDataField);
begin
  FMasterKeyField := AMasterKeyField;
end;

procedure TdxEMFDataRelation.UpdateMasterDetailKeyFieldNames;
begin
  if DataController.DetailMode <> dcdmClone then
    Changed;
end;

{ TdxEMFDataGroupInfo }

constructor TdxEMFDataGroupInfo.Create(AOwner: TcxDataGroups;
  AParentGroup: TdxEMFDataGroupInfo);
begin
  inherited Create(AOwner);
  FParentGroup := AParentGroup;
  if AParentGroup = nil then
    Level := 0
  else
    Level := AParentGroup.Level + 1;
  FChildGroupDictionary := TdxEMFDataGroupInfoDictionary.Create;
end;

destructor TdxEMFDataGroupInfo.Destroy;
begin
  FreeAndNil(FChildGroupDictionary);
  inherited Destroy;
end;

function TdxEMFDataGroupInfo.GetOwner: TdxEMFDataGroups;
begin
  Result := TdxEMFDataGroups(inherited Owner);
end;

function TdxEMFDataGroupInfo.GetDisplayText: string;
begin
  Result := VarToStr(GroupValue);
end;

function TdxEMFDataGroupInfo.GetGroupValue: Variant;
begin
  if DataGroup = nil then
    Result := Null
  else
    Result := DataGroup.GroupValue;
end;

function TdxEMFDataGroupInfo.GetIsInconsistent: Boolean;
var
  AEMFDataGroup: TdxEMFDataGroup;
begin
  if DataGroup <> nil then
    if DataGroup.IsInconsistent then
      Exit(True)
    else
      for AEMFDataGroup in DataGroup.Children do
        if AEMFDataGroup.IsInconsistent then
          Exit(True);
  Result := False;
end;

procedure TdxEMFDataGroupInfo.AdjustByInsertingRecord(ARecordIndex: Integer;
  AAppending: Boolean);
begin
  if Cardinal(ARecordIndex) < Cardinal(FirstRecordListIndex) then
  begin
    Inc(FirstRecordListIndex);
    Inc(LastRecordListIndex);
  end
  else if Cardinal(ARecordIndex) <= Cardinal(LastRecordListIndex) then
    Inc(LastRecordListIndex)
  else if AAppending and (ARecordIndex = LastRecordListIndex + 1) then
    Inc(LastRecordListIndex);
end;

procedure TdxEMFDataGroupInfo.ClearData;
begin
  FChildGroupDictionary.Clear;
  if DataGroup <> nil then
    DataGroup.Clear;
end;

function TdxEMFDataGroupInfo.IsLastLevel: Boolean;
begin
  Result := Level = Owner.LevelCount - 1;
end;

function TdxEMFDataGroupInfo.IsTopGroupInfo: Boolean;
begin
  Result := False;
end;

procedure TdxEMFDataGroupInfo.SetDataGroup(const Value: TdxEMFDataGroup);
begin
  if FDataGroup = Value then
    Exit;
  FDataGroup := Value;
  if FDataGroup <> nil then
    FDataGroup.AssignTo(Self);
end;

procedure TdxEMFDataGroupInfo.SetExpanded(const Value: Boolean);
begin
  if Expanded = Value then
    Exit;
  Expanded := Value;
  if Expanded then
  begin
    if not IsLastLevel then
      Owner.CreateChildren(Self);
  end;
end;

procedure TdxEMFDataGroupInfo.AddChildGroup(const Value: TdxEMFDataGroupInfo);
begin
  FChildGroupDictionary.AddGroup(Value);
end;

function TdxEMFDataGroupInfo.FindChildGroup(const AGroupValue: Variant)
  : TdxEMFDataGroupInfo;
begin
  Result := FChildGroupDictionary.FindGroup(AGroupValue);
end;

{ TdxEMFDataTopGroupInfo }

constructor TdxEMFDataTopGroupInfo.Create(AOwner: TcxDataGroups);
begin
  inherited Create(AOwner, nil);
  Level := -1;
end;

destructor TdxEMFDataTopGroupInfo.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDataGroup);
end;

procedure TdxEMFDataTopGroupInfo.Clear;
begin
  ClearData;
  FreeAndNil(FDataGroup);
end;

procedure TdxEMFDataTopGroupInfo.ClearData;
begin
  inherited ClearData;
  FirstRecordListIndex := 0;
end;

function TdxEMFDataTopGroupInfo.IsLastLevel: Boolean;
begin
  Result := False;
end;

function TdxEMFDataTopGroupInfo.IsTopGroupInfo: Boolean;
begin
  Result := True;
end;

{ TdxEMFDataGroup }

constructor TdxEMFDataGroup.Create(AOwner: TdxEMFDataGroups);
begin
  inherited Create(AOwner);
  FChildren := TObjectList<TdxEMFDataGroup>.Create;
end;

destructor TdxEMFDataGroup.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TdxEMFDataGroup.GetOwner: TdxEMFDataGroups;
begin
  Result := TdxEMFDataGroups(inherited Owner);
end;

function TdxEMFDataGroup.GetDataSource: TdxEMFDataControllerCustomDataSource;
begin
  Result := Owner.DataControllerInfo.DataController.DataSource;
end;

function TdxEMFDataGroup.GetIsEmpty: Boolean;
begin
  Result := FCollection = nil;
end;

function TdxEMFDataGroup.ContainsEntity(AEntity: TObject): Boolean;
begin
  if (Self = nil) or (ParentGroup = nil) then
    Result := False
  else
    Result := DataSource.EntityHasProperty(AEntity, GetGroupValues);
end;

function TdxEMFDataGroup.ContainsExpressionsInGroupValues: Boolean;
var
  I: Integer;
begin
  for I := 0 to GroupedItemCount - 1 do
    if not Supports(GetField(I).GetCriteria(True), IdxOperandProperty) then
      Exit(True);
  Result := False;
end;

procedure TdxEMFDataGroup.DeleteRecords;
var
  ADataGroup: TdxEMFDataGroup;
  AEntity: TObject;
  ASession: TdxEMFCustomSession;
begin
  if not IsLastLevel then
  begin
    if Children.Count = 0 then
      PrepareChildren;
    for ADataGroup in Children do
      ADataGroup.DeleteRecords;
  end
  else
  begin
    ASession := Owner.DataControllerInfo.DataController.DataSource.Session;
    for AEntity in Collection do
      ASession.Delete(AEntity);
  end;
  Owner.DataControllerInfo.DataController.DataChangedFlag := True;
end;

procedure TdxEMFDataGroup.Clear;
begin
  FCollection := nil;
  FIsInconsistent := False;
end;

function TdxEMFDataGroup.GetCollection: IdxEMFCollection;
begin
  if FCollection = nil then
    FCollection := GetDataGroupCollection;
  Result := FCollection;
end;

function TdxEMFDataGroup.GetDataGroupCollection: IdxEMFCollection;
begin
  if (Owner.LevelCount = 0) or IsLastLevel then
  begin
    Result := Owner.DataControllerInfo.GetCollection(Self);
    if RecordCount <> Result.Count then
      IsInconsistent := True;
  end
  else
    Result := Owner.DataControllerInfo.GetGroupInfoCollection(Self);
end;

function TdxEMFDataGroup.GetEntity(ARecordIndex: Integer): TObject;
var
  I: Integer;
  AItem: TdxEMFDataGroup;
  AEMFCollection: IdxEMFCollection;
begin
  if IsLastLevel then
  begin
    AEMFCollection := Collection;
    Dec(ARecordIndex, FirstRecordListIndex);
    if ARecordIndex >= AEMFCollection.Count then
      Result := nil
    else
      Result := AEMFCollection[ARecordIndex]
  end
  else
  begin
    Result := nil;
    if Children.Count = 0 then
      PrepareChildren;
    for I := 0 to Children.Count - 1 do
    begin
      AItem := Children{$IFDEF DELHIXE3}.List{$ENDIF}[I];
      if AItem.Contains(ARecordIndex) then
        Exit(AItem.GetEntity(ARecordIndex));
    end;
  end;
end;

function TdxEMFDataGroup.GetField(AGroupedColumnIndex: Integer)
  : TdxEMFDataField;
begin
  Result := TdxEMFDataField
    (Owner.DataControllerInfo.DataGroups.GetFieldByLevelGroupedFieldIndex(Level,
    AGroupedColumnIndex));
end;

function TdxEMFDataGroup.GetGroupCriteria: IdxCriteriaOperator;
var
  I: Integer;
begin
  if (Self = nil) or (ParentGroup = nil) then
    Exit(nil);
  if GroupedItemCount = 1 then
    Result := BinaryOperatorCreate(GetField(0).GetCriteria(True),
      TValue.FromVariant(FGroupValue))
  else
    for I := 0 to GroupedItemCount - 1 do
      Result := TdxGroupOperator.Combine(TdxGroupOperatorType.&And, Result,
        BinaryOperatorCreate(GetField(I).GetCriteria(True),
        TValue.FromVariant(FGroupValue[I])));
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.&And, Result,
    ParentGroup.GetGroupCriteria);
end;

function TdxEMFDataGroup.GetGroupValue: Variant;
begin
  if FEntity <> nil then
    Result := NativeInt(FEntity)
  else
    Result := FGroupValue;
end;

function TdxEMFDataGroup.GetGroupValues: TArray<TPair<string, TValue>>;
var
  I: Integer;
  AGroupValues, AParentGroupValues: TArray<TPair<string, TValue>>;
  ADataField: TdxEMFDataField;
begin
  if (Self = nil) or (ParentGroup = nil) then
    Exit(nil);
  AParentGroupValues := ParentGroup.GetGroupValues;
  if GroupedItemCount = 1 then
  begin
    ADataField := GetField(0);
    if (ADataField.MemberInfo.FieldType in [ftDate, ftTime, ftDateTime]) and
      (ADataField.DateTimeGrouping <> dtgDefault) then
      Exit(nil);
    AGroupValues := TArray < TPair < string,
      TValue >>.Create(TPair<string, TValue>.Create(ADataField.FieldName,
      TValue.FromVariant(GroupValue)))
  end
  else
  begin
    SetLength(AGroupValues, GroupedItemCount);
    for I := 0 to GroupedItemCount - 1 do
      AGroupValues[I] := TPair<string, TValue>.Create(GetField(I).FieldName,
        TValue.FromVariant(GroupValue[I]));
  end;
  if Length(AParentGroupValues) > 0 then
  begin
{$IFDEF DELPHIXE8}
    Result := AGroupValues + AParentGroupValues;
{$ELSE}
    SetLength(Result, Length(AParentGroupValues) + Length(AGroupValues));
    for I := 0 to Length(AParentGroupValues) - 1 do
      Result[I] := AParentGroupValues[I];
    for I := 0 to Length(AGroupValues) - 1 do
      Result[I + Length(AParentGroupValues)] := AGroupValues[I];
{$ENDIF}
  end
  else
    Result := AGroupValues;
end;

function TdxEMFDataGroup.GetHasChildrenInfo: Boolean;
begin
  Result := not((FCollection = nil) and (Children.Count = 0));
end;

function TdxEMFDataGroup.GetRecordIndex(AEntity: TObject): Integer;
var
  AEMFDataGroup: TdxEMFDataGroup;
begin
  if not ContainsExpressionsInGroupValues and not ContainsEntity(AEntity) then
    Result := -1
  else if IsLastLevel then
  begin
    Result := IndexOf(AEntity);
    if Result >= 0 then
      Inc(Result, FirstRecordListIndex);
  end
  else
  begin
    PrepareChildren;
    for AEMFDataGroup in Children do
    begin
      Result := AEMFDataGroup.GetRecordIndex(AEntity);
      if Result >= 0 then
        Exit;
    end;
    Result := -1;
  end;
end;

function TdxEMFDataGroup.GetRowGroupValue
  (ARow: TdxSelectStatementResultRow): Variant;
var
  I: Integer;
begin
  if GroupedItemCount = 1 then
    Result := ARow.Values[1]
  else
  begin
    Result := VarArrayCreate([0, GroupedItemCount], varVariant);
    for I := 0 to GroupedItemCount - 1 do
      Result[I] := ARow.Values[I + 1];
  end;
end;

function TdxEMFDataGroup.IndexOf(AObject: TObject): Integer;
begin
  Result := Collection.IndexOf(AObject);
end;

function TdxEMFDataGroup.IsLastLevel: Boolean;
begin
  Result := Level = Owner.LevelCount - 1;
end;

procedure TdxEMFDataGroup.PrepareAssociationValues;
var
  AEMFDataFields: TArray<TdxEMFDataField>;
  I, J: Integer;
  ANeedPrepareGroupValues: Boolean;
  AGroup: TdxEMFDataGroup;
  AObject: TObject;
  ASession: TdxEMFCustomSessionAccess;
  AReferenceType: TdxEntityInfo;
  AKeyValue: TValue;
  APersistentClass: TClass;
begin
  if Children.Count = 0 then
    Exit;
  AGroup := Children[0];
  SetLength(AEMFDataFields, AGroup.GroupedItemCount);
  ANeedPrepareGroupValues := False;
  for I := 0 to AGroup.GroupedItemCount - 1 do
  begin
    AEMFDataFields[I] := AGroup.GetField(I);
    if Owner.DataControllerInfo.DataController.NeedPrepareGroupValue
      (AEMFDataFields[I]) then
      ANeedPrepareGroupValues := True;
  end;
  if ANeedPrepareGroupValues then
  begin
    ASession := TdxEMFCustomSessionAccess(DataSource.Session);
    for J := 0 to Length(AEMFDataFields) - 1 do
    begin
      AReferenceType := AEMFDataFields[J].MemberInfo.ReferenceType;
      if AReferenceType = nil then
        Continue;
      APersistentClass := AReferenceType.ClassAttributes.PersistentClass;
      for I := 0 to Children.Count - 1 do
      begin
        AGroup := Children{$IFDEF DELPHIXE3}.List{$ENDIF}[I];
        if VarIsNull(AGroup.GroupValue) then
          Continue;
        AKeyValue := TValue.FromVariant(AGroup.GroupValue);
        AObject := ASession.GetLoadedObjectByKey(APersistentClass, AKeyValue);
        if AObject = nil then
        begin
          AObject := ASession.Find(APersistentClass, AKeyValue);
        end;
        AGroup.FEntity := AObject;
      end;
    end;
  end;
end;

function TdxEMFDataGroup.PrepareChildren: Boolean;
var
  AItem: TObject;
  ARow: TdxSelectStatementResultRow absolute AItem;
  AGroup: TdxEMFDataGroup;
  AFirstRecordListIndex: Integer;
  ACollection: IdxEMFCollection;
begin
  if HasChildrenInfo or IsLastLevel then
    Result := False
  else
  begin
    AFirstRecordListIndex := FirstRecordListIndex;
    ACollection := GetDataGroupCollection;
    for AItem in ACollection do
    begin
      AGroup := TdxEMFDataGroup.Create(Owner);
      AGroup.ParentGroup := Self;
      AGroup.Level := Level + 1;
      AGroup.SetDataGroupInfo(ARow, AFirstRecordListIndex);
      AFirstRecordListIndex := AGroup.LastRecordListIndex + 1;
      Children.Add(AGroup);
    end;
    PrepareAssociationValues;
    Result := True;
  end;
end;

procedure TdxEMFDataGroup.Remove(AEntity: TObject);
begin
  Collection.Remove(AEntity);
end;

procedure TdxEMFDataGroup.SetDataGroupInfo(ARow: TdxSelectStatementResultRow;
  AFirstRecordListIndex: Integer);
begin
  FirstRecordListIndex := AFirstRecordListIndex;
  LastRecordListIndex := FirstRecordListIndex + ARow.GetGroupRecordCount - 1;
  FGroupValue := GetRowGroupValue(ARow);
  ARow.UpdateSummary(Self);
end;

procedure TdxEMFDataGroup.SetIsInconsistent(const Value: Boolean);
begin
  FIsInconsistent := Value;
end;

{ TdxEMFDataGroupInfoDictionary }

constructor TdxEMFDataGroupInfoDictionary.Create;
begin
  inherited Create([doOwnsValues]);
end;

procedure TdxEMFDataGroupInfoDictionary.AddGroup(AGroup: TdxEMFDataGroupInfo);
var
  AGroupValueHash: Cardinal;
  AList: TdxFastList;
begin
  AGroupValueHash := GetVariantHash(AGroup.GroupValue);
  if TryGetValue(AGroupValueHash, AList) then
  begin
    AList.Add(AGroup);
  end
  else
  begin
    AList := TdxFastList.Create;
    AList.Add(AGroup);
    Add(AGroupValueHash, AList);
  end;
end;

function TdxEMFDataGroupInfoDictionary.FindGroup(const AGroupValue: Variant)
  : TdxEMFDataGroupInfo;
var
  AGroupValueHash: Cardinal;
  AList: TdxFastList;
  I: Integer;
begin
  AGroupValueHash := GetVariantHash(AGroupValue);
  if not TryGetValue(AGroupValueHash, AList) then
    Exit(nil);
  if AList.Count = 1 then
    Exit(TdxEMFDataGroupInfo(AList[0]));
  for I := 0 to AList.Count - 1 do
  begin
    Result := TdxEMFDataGroupInfo(AList[I]);
    if VarEquals(Result.GroupValue, AGroupValue) then
      Exit;
  end;
  Result := nil;
end;

{ TdxEMFDataGroups }

destructor TdxEMFDataGroups.Destroy;
begin
  FreeAndNil(FTopGroupInfo);
  inherited Destroy;
end;

procedure TdxEMFDataGroups.FullExpanding(AExpanded: Boolean);
var
  I: Integer;
begin
  FIsFullExpanding := AExpanded;
  try
    for I := 0 to FItems.Count - 1 do
      TcxDataGroupInfo(FItems[I]).Free;
    FItems.Clear;
    TopGroupInfo.FChildGroupDictionary.Clear;
    CreateGroups;
  finally
    FIsFullExpanding := False;
  end;
end;

function TdxEMFDataGroups.GetDataControllerInfo: TdxEMFDataControllerInfo;
begin
  Result := TdxEMFDataControllerInfo(inherited DataControllerInfo);
end;

function TdxEMFDataGroups.GetItem(Index: Integer): TdxEMFDataGroupInfo;
begin
  Result := TdxEMFDataGroupInfo(inherited Items[Index]);
end;

procedure TdxEMFDataGroups.ChangeExpanding(ARowIndex: Integer;
  AExpanded, ARecursive: Boolean);
var
  AItem: TcxDataGroupInfo;
  I, AItemIndex: Integer;
begin
  if DataControllerInfo.InMemoryMode then
  begin
    inherited;
    Exit;
  end;
  AItemIndex := Find(ARowIndex, AItem);
  if (AItemIndex <> -1) and (AItem <> nil) then
  begin
    (AItem as TdxEMFDataGroupInfo).SetExpanded(AExpanded);
    if ARecursive and (AItem.Level < LevelCount - 1) then
    begin
      for I := AItemIndex + 1 to Count - 1 do
        if Items[I].Level > AItem.Level then
          Items[I].SetExpanded(AExpanded)
        else
          Break;
    end;
    Rebuild;
  end;
end;

function TdxEMFDataGroups.GetChildCount(AIndex: Integer): Integer;
var
  AEMFDataGroupInfo: TdxEMFDataGroupInfo;
  ADataGroup: TdxEMFDataGroup;
begin
  if AIndex = -1 then
    Result := GetFirstLevelCount
  else
  begin
    AEMFDataGroupInfo := Items[AIndex];
    if AEMFDataGroupInfo.Level + 1 = LevelCount then
      Result := AEMFDataGroupInfo.RecordCount
    else
    begin
      if AEMFDataGroupInfo.DataGroup.PrepareChildren then
      begin
        CreateChildren(AEMFDataGroupInfo);
        Rebuild;
      end;
      ADataGroup := AEMFDataGroupInfo.DataGroup;
      if ADataGroup = nil then
        Result := 0
      else
        Result := ADataGroup.Children.Count;
    end;
  end;
end;

function TdxEMFDataGroups.GetChildIndex(AParentIndex, AIndex: Integer): Integer;
var
  I, AParentLevel, AGroupLevel, ACurrentChildGroupIndex: Integer;
begin
  if AParentIndex = -1 then
    Result := GetFirstLevelIndex(AIndex)
  else
  begin
    AParentLevel := Items[AParentIndex].Level;
    Result := -1;
    ACurrentChildGroupIndex := -1;
    for I := AParentIndex + 1 to Count - 1 do
    begin
      AGroupLevel := Items[I].Level;
      if AParentLevel < AGroupLevel then
      begin
        if AParentLevel = AGroupLevel - 1 then
        begin
          Inc(ACurrentChildGroupIndex);
          if ACurrentChildGroupIndex = AIndex then
          begin
            Result := I;
            Break;
          end;
        end;
      end
      else
        Break;
    end;
  end;
end;

function TdxEMFDataGroups.GetTopGroupInfo: TdxEMFDataTopGroupInfo;
begin
  if (FTopGroupInfo = nil) or (FTopGroupInfo.DataGroup = nil) then
    CreateTopGroupInfo;
  Result := FTopGroupInfo;
end;

function TdxEMFDataGroups.HasTopGroupInfo: Boolean;
begin
  Result := FTopGroupInfo <> nil;
end;

procedure TdxEMFDataGroups.CreateGroups(AListGroup: TList<TdxEMFDataGroup>;
  AParentGroup: TdxEMFDataGroupInfo);
var
  AItem: TdxEMFDataGroup;
  ADataGroupInfo: TdxEMFDataGroupInfo;
  ALastLevel: Boolean;
  I, AIndex: Integer;
  AProvider: TdxEMFDataProvider;
  AEditingRecordListIndex: Integer;
  AParentGroupInfo: TdxEMFDataGroupInfo;
begin
  if AParentGroup = nil then
    ALastLevel := LevelCount = 0
  else
    ALastLevel := AParentGroup.IsLastLevel;
  AIndex := FItems.IndexOf(AParentGroup, ldFromEnd);
  AProvider := DataControllerInfo.DataController.Provider;
  AEditingRecordListIndex := DataControllerInfo.GetInternalRecordListIndex
    (AProvider.EditingRecordIndex);
  if AParentGroup = nil then
    AParentGroupInfo := TopGroupInfo
  else
    AParentGroupInfo := AParentGroup;
  if AListGroup.Count = AParentGroupInfo.FChildGroupDictionary.Count then
    Exit;
  for I := 0 to AListGroup.Count - 1 do
  begin
    AItem := AListGroup[I];
    ADataGroupInfo := TdxEMFDataGroupInfo.Create(Self, AParentGroup);
    ADataGroupInfo.DataGroup := AItem;
    AParentGroupInfo.AddChildGroup(ADataGroupInfo);
    if AProvider.IsInserting then
      ADataGroupInfo.AdjustByInsertingRecord(AEditingRecordListIndex,
        AProvider.IsAppending);
    ADataGroupInfo.Expanded := DataControllerInfo.IsAlwaysExpanded or
      FIsFullExpanding;
    if AIndex < 0 then
      AIndex := FItems.Add(ADataGroupInfo)
    else
    begin
      Inc(AIndex);
      FItems.Insert(AIndex, ADataGroupInfo);
    end;
    if not ALastLevel and ADataGroupInfo.Expanded then
    begin
      CreateChildren(ADataGroupInfo);
      Inc(AIndex, ADataGroupInfo.DataGroup.Children.Count);
    end;
  end;
end;

function TdxEMFDataGroups.GetFirstDataRecordListIndex
  (AInfo: TcxDataGroupInfo): Integer;
begin
  if DataControllerInfo.InMemoryMode then
    Result := inherited
  else
    Result := AInfo.FirstRecordListIndex;
end;

function TdxEMFDataGroups.GetLastDataRecordListIndex
  (AInfo: TcxDataGroupInfo): Integer;
begin
  if DataControllerInfo.InMemoryMode then
    Result := inherited
  else
    Result := AInfo.LastRecordListIndex;
end;

procedure TdxEMFDataGroups.CreateChildren(AParentGroup: TdxEMFDataGroupInfo);
begin
  AParentGroup.DataGroup.PrepareChildren;
  CreateGroups(AParentGroup.DataGroup.Children, AParentGroup);
end;

procedure TdxEMFDataGroups.CreateGroups;
begin
  TopGroupInfo.DataGroup.PrepareChildren;
  CreateGroups(TopGroupInfo.DataGroup.Children, nil);
  Rebuild;
end;

procedure TdxEMFDataGroups.CreateTopGroupInfo;
begin
  FTopGroupInfo := TdxEMFDataTopGroupInfo.Create(Self);
  try
    DataControllerInfo.PrepareTopGroupInfo(FTopGroupInfo);
  except
    FreeAndNil(FTopGroupInfo);
    raise;
  end;
end;

function TdxEMFDataGroups.GetRecordCount: Integer;
begin
  Result := TopGroupInfo.RecordCount;
end;

function TdxEMFDataGroups.GetRecordIndex(AObject: TObject): Integer;
var
  I, ADataGroupCount: Integer;
  ADataGroup: TdxEMFDataGroup;
begin
  Result := -1;
  ADataGroupCount := Count;
  if ADataGroupCount = 0 then
  begin
    if TopGroupInfo.DataGroup.RecordCount > 0 then
      Result := TopGroupInfo.DataGroup.IndexOf(AObject)
  end
  else
    for I := 0 to Count - 1 do
    begin
      ADataGroup := Items[I].DataGroup;
      Result := ADataGroup.GetRecordIndex(AObject);
      if Result >= 0 then
        Exit;
    end;
end;

function TdxEMFDataGroups.GetDataGroupInfoByRecordIndex(ARecordIndex: Integer)
  : TdxEMFDataGroupInfo;
var
  AGroupIndex: Integer;
begin
  if Count = 0 then
    Result := TopGroupInfo
  else
  begin
    AGroupIndex := GetIndexByRecordIndex(ARecordIndex);
    Result := Items[AGroupIndex];
  end;
end;

procedure TdxEMFDataGroups.ClearTopGroupInfo;
begin
  FreeAndNil(FTopGroupInfo);
end;

procedure TdxEMFDataGroups.Clear;
begin
  inherited Clear;
  if FTopGroupInfo <> nil then
    FTopGroupInfo.FChildGroupDictionary.Clear;
end;

procedure TdxEMFDataGroups.ClearData;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].ClearData;
    Items[I].DataGroup := nil;
  end;
  ClearTopGroupInfo;
end;

procedure TdxEMFDataGroups.SoftClearData;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].IsLastLevel then
      Items[I].ClearData;
end;

procedure TdxEMFDataGroups.Rebuild;
var
  AIndex, ARowIndex: Integer;
  AGroup, ASubGroup: TcxDataGroupInfo;
begin
  AIndex := 0;
  ARowIndex := 0;
  while AIndex < Count do
  begin
    AGroup := Items[AIndex];
    AGroup.RowIndex := ARowIndex;
    if not AGroup.Expanded then
    begin
      Inc(AIndex);
      while AIndex < Count do
      begin
        ASubGroup := Items[AIndex];
        if ASubGroup.Level > AGroup.Level then
          ASubGroup.RowIndex := ARowIndex
        else
        begin
          Dec(AIndex);
          Break;
        end;
        Inc(AIndex)
      end;
    end;
    if AGroup.Expanded and (AGroup.Level = (LevelCount - 1)) then
      Inc(ARowIndex, AGroup.RecordCount);
    Inc(ARowIndex);
    Inc(AIndex)
  end;
end;

function TdxEMFDataGroups.GetIsInconsistent: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].IsInconsistent then
      Exit(True);
  Result := False;
end;

function TdxEMFDataGroups.GetIndexByRecordIndex(ARecordIndex: Integer): Integer;

  function FindMostInnerGroup(ARecordListIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Count - 1 downto 0 do
      if Items[I].Contains(ARecordListIndex) then
      begin
        Result := I;
        Break;
      end;
  end;

var
  AIndex: Integer;
begin
  Result := FindMostInnerGroup(ARecordIndex);
  while (Result >= 0) and (Items[Result].Level < (LevelCount - 1)) do
  begin
    Items[Result].SetExpanded(True);
    Rebuild;
    AIndex := FindMostInnerGroup(ARecordIndex);
    if AIndex >= 0 then
    begin
      if Result = AIndex then
        Exit(-1);
      Result := AIndex
    end
    else
      Break;
  end;
end;

function TdxEMFDataGroups.GetEntityByRecordIndex(ARecordIndex: Integer)
  : TObject;
var
  ADataGroupInfo: TdxEMFDataGroupInfo;
  AProvider: TdxEMFDataProvider;
begin
  if ARecordIndex < 0 then
    Exit(nil);
  ADataGroupInfo := GetDataGroupInfoByRecordIndex(ARecordIndex);
  AProvider := DataControllerInfo.DataController.Provider;
  if AProvider.FInserting then
  begin
    if AProvider.EditingRecordIndex = ARecordIndex then
      Exit(nil)
    else if DataControllerInfo.DataController.UseNewItemRowForEditing then
      Dec(ARecordIndex, ADataGroupInfo.FirstRecordListIndex -
        ADataGroupInfo.DataGroup.FirstRecordListIndex)
    else if (AProvider.EditingRecordIndex < ARecordIndex) then
      Dec(ARecordIndex);
    if ARecordIndex < 0 then
      Exit(nil);
  end;
  Result := ADataGroupInfo.DataGroup.GetEntity(ARecordIndex);
end;

{ TdxEMFDataControllerInfo }

function TdxEMFDataControllerInfo.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFDataControllerInfo.GetDataGroups: TdxEMFDataGroups;
begin
  Assert(inherited DataGroups is TdxEMFDataGroups);
  Result := TdxEMFDataGroups(inherited DataGroups);
end;

function TdxEMFDataControllerInfo.GetEntityByRecordIndex
  (ARecordIndex: Integer): TObject;
begin
  Result := DataGroups.GetEntityByRecordIndex(ARecordIndex);
end;

function TdxEMFDataControllerInfo.GetFixedBottomRowCount: Integer;
begin
  Result := 0;
end;

function TdxEMFDataControllerInfo.GetFixedTopRowCount: Integer;
begin
  Result := 0;
end;

function TdxEMFDataControllerInfo.GetFocusingInfo: TdxEMFDataFocusingInfo;
begin
  Result := TdxEMFDataFocusingInfo(inherited FocusingInfo);
end;

function TdxEMFDataControllerInfo.GetSelection: TdxEMFDataSelection;
begin
  Result := TdxEMFDataSelection(inherited Selection);
end;

function TdxEMFDataControllerInfo.GetInMemoryMode: Boolean;
begin
  Result := DataController.InMemoryMode;
end;

function TdxEMFDataControllerInfo.GetRowInfo(ARowIndex: Integer): TcxRowInfo;
var
  AGroupsRowInfo: TcxGroupsRowInfo;
begin
  if InMemoryMode then
    Result := inherited GetRowInfo(ARowIndex)
  else
  begin
    CheckRowIndex(ARowIndex);
    if IsGrouped then
    begin
      AGroupsRowInfo := DataGroups.RowInfo[ARowIndex];
      Result.DataRowIndex := AGroupsRowInfo.RecordListIndex;
      Result.RecordIndex := GetInternalRecordIndex
        (AGroupsRowInfo.RecordListIndex);
    end
    else
    begin
      AGroupsRowInfo.Level := 0;
      AGroupsRowInfo.Expanded := False;
      AGroupsRowInfo.RecordListIndex := ARowIndex;
      AGroupsRowInfo.Index := -1;
      Result.RecordIndex := GetInternalRecordIndex
        (AGroupsRowInfo.RecordListIndex);
      Result.DataRowIndex := ARowIndex;
    end;
    Result.Expanded := AGroupsRowInfo.Expanded;
    Result.Level := AGroupsRowInfo.Level;
    Result.RowIndex := ARowIndex;
    Result.GroupIndex := AGroupsRowInfo.Index;
  end;
end;

function TdxEMFDataControllerInfo.IsInsertInEmpty: Boolean;
begin
  Result := (DataController.RecordCount = 1) and
    (DataController.Provider.FInserting);
end;

procedure TdxEMFDataControllerInfo.PopulateFilterValues
  (AList: TcxDataFilterValueList; ADataField: TdxEMFDataField;
  ACriteria: TcxFilterCriteria; var AUseFilteredRecords: Boolean;
  out ANullExists: Boolean);
var
  AResult: IdxEMFCollection;
  ADataSource: TdxEMFDataControllerCustomDataSource;
  AProperties: IdxCriteriaOperatorCollection;
  ASortByExpressions: TdxSortByExpressions;
  AObjectRow: TObject;
  ARow: TdxSelectStatementResultRow absolute AObjectRow;
  AValue: Variant;
  AFilterCriteria: IdxCriteriaOperator;
  ADisplayText: string;

  AJoinCriteria: IdxCriteriaOperator;
  AJoinOperand: IdxJoinOperand;
  AEntityInfo, AJoinEntityInfo: TdxEntityInfo;
begin
  ADataSource := DataController.DataSource;
  if AUseFilteredRecords then
    AFilterCriteria := DataController.GetCriteria
  else
    AFilterCriteria := nil;
  if ADataField.MemberInfo.IsReference then
  begin
    ANullExists := True;
    AJoinEntityInfo := ADataSource.EntityInfo;
    AEntityInfo := ADataField.MemberInfo.AssociatedMember.Owner;
    // ¹ TODO: restriction: composite key is not supported. COMPLETION !!!
    AJoinCriteria := TdxBinaryOperator.Create
      (TdxOperandProperty.Create('^.' + AEntityInfo.KeyProperty.Member.
      MemberName), TdxOperandProperty.Create(ADataField.FieldName),
      TdxBinaryOperatorType.Equal);
    AJoinOperand := TdxJoinOperand.Create
      (AJoinEntityInfo.ClassAttributes.PersistentClass,
      TdxGroupOperator.Combine(TdxGroupOperatorType.&And, AJoinCriteria,
      AFilterCriteria), TdxAggregateFunctionType.Exists,
      TdxOperandProperty.Create(ADataField.FieldName));
    for AObjectRow in TdxEMFCustomSessionAccess(ADataSource.Session)
      .GetObjects(AEntityInfo.ClassAttributes.PersistentClass, AJoinOperand,
      nil, 0, ACriteria.MaxValueListCount) do
    begin
      AValue := NativeInt(AObjectRow);
      ADisplayText := ADataField.MemberInfo.GetValueAsDisplayText(AValue);
      AList.Add(fviValue, AValue, ADisplayText, True);
    end;
  end
  else
  begin
    ANullExists := False;
    AProperties := TdxCriteriaOperatorCollection.Create
      ([ADataField.GetCriteria]);
    ASortByExpressions := TdxSortByExpressions.Create
      ([TdxSortByExpression.Create(AProperties[0])]);
    AResult := TdxEMFCustomSessionAccess(ADataSource.Session)
      .GetProjections(ADataSource.EntityInfo.ClassAttributes.PersistentClass,
      AProperties, AFilterCriteria, AProperties, nil, False, 0,
      ACriteria.MaxValueListCount, ASortByExpressions);
    for AObjectRow in AResult do
    begin
      AValue := ARow.Values[0];
      if ACriteria.ValueIsNull(AValue) then
        ANullExists := True
      else
      begin
        AValue := ADataField.TryCastVariant(AValue);
        ADisplayText := ADataField.MemberInfo.GetValueAsDisplayText(AValue);
        AList.Add(fviValue, AValue, ADisplayText, True);
      end;
    end;
  end;
end;

procedure TdxEMFDataControllerInfo.PopulateSummary(ASummaryItems
  : TcxDataSummaryItems; ARow: TdxSelectStatementResultRow;
  var ACountValues: TcxDataSummaryCountValues;
  var ASummaryValues: TcxDataSummaryValues);
var
  I, AValueIndex: Integer;
  ADataSummaryItem: TcxDataSummaryItem;
begin
  AValueIndex := 0;
  for I := 0 to ASummaryItems.Count - 1 do
  begin
    ADataSummaryItem := ASummaryItems[I];
    if not ADataSummaryItem.IsDataBinded then
      Continue;
    case ADataSummaryItem.Kind of
      skNone:
        ;
      skCount:
        ACountValues[I] := ARow.Values[AValueIndex]
    else
      ASummaryValues[I] := ARow.Values[AValueIndex];
    end;
    Inc(AValueIndex);
  end;
end;

procedure TdxEMFDataControllerInfo.CalculateSummary(ASummaryItems
  : TcxDataSummaryItems; var ACountValues: TcxDataSummaryCountValues;
  var ASummaryValues: TcxDataSummaryValues);
var
  I: Integer;
  ADataSummaryItem: TdxEMFSummaryItem;
  AResult: IdxEMFCollection;
  AProperties: IdxCriteriaOperatorCollection;
  ARow: TdxSelectStatementResultRow;
begin
  AProperties := TdxCriteriaOperatorCollection.Create;
  for I := 0 to ASummaryItems.Count - 1 do
  begin
    ADataSummaryItem := TdxEMFSummaryItem(ASummaryItems[I]);
    if not ADataSummaryItem.IsDataBinded or
      (ADataSummaryItem.Kind = TcxSummaryKind.skNone) then
      AProperties.Add(TdxConstantValue.Create(TValue.Empty))
    else
      AProperties.Add(TdxAggregateOperand.Create(nil,
        TdxOperandProperty.Create(ADataSummaryItem.FieldName),
        SummaryKindToAggregateFunctionType(ADataSummaryItem.Kind), nil));
  end;
  if AProperties.Count = 0 then
    Exit;
  AResult := TdxEMFCustomSessionAccess(DataController.DataSource.Session)
    .GetProjections(DataController.DataSource.EntityInfo.ClassAttributes.
    PersistentClass, AProperties, DataController.GetCriteria, nil, nil, False,
    0, 0, nil);
  ARow := AResult.First as TdxSelectStatementResultRow;
  PopulateSummary(ASummaryItems, ARow, ACountValues, ASummaryValues);
end;

procedure TdxEMFDataControllerInfo.CheckExpanding;
var
  AExpandingInfoIndex: Integer;

  function GroupNeedsRestoration(AExpandingInfo: TcxDataExpandingInfo): Boolean;
  begin
    Result := (eisExpanded in AExpandingInfo.State) and
      (eisExpanded in ExpandingInfo.SaveStates)
  end;

  function IsEqualFields: Boolean;
  var
    I: Integer;
  begin
    Result := ExpandingInfo.FieldCount = DataGroups.GroupFieldInfoCount;
    if Result then
      for I := 0 to DataGroups.GroupFieldInfoCount - 1 do
        if ExpandingInfo.Fields[I] <> DataGroups.GroupFieldInfos[I].Field then
        begin
          Result := False;
          Break;
        end;
  end;

  procedure ExpandGroup(var AExpandingInfoIndex: Integer;
    AParent: TdxEMFDataGroupInfo);
  var
    AGroupIndex: Integer;
    AExpandingInfo: TcxDataExpandingInfo;
    AGroup: TdxEMFDataGroupInfo;
    ACanExpand: Boolean;
  begin
    if AExpandingInfoIndex >= ExpandingInfo.Count then
      Exit;
    AExpandingInfo := ExpandingInfo[AExpandingInfoIndex];
    AGroupIndex := AExpandingInfo.GroupIndex;
    AGroup := AParent.FindChildGroup(AExpandingInfo.Value);
    ACanExpand := (AGroup <> nil) and GroupNeedsRestoration(AExpandingInfo);
    Inc(AExpandingInfoIndex);
    if ACanExpand then
    begin
      AGroup.SetExpanded(True);
      DataGroups.Rebuild;
    end;
    while (AExpandingInfoIndex < ExpandingInfo.Count) and
      (AGroupIndex < ExpandingInfo[AExpandingInfoIndex].GroupIndex) do
    begin
      if ACanExpand then
        ExpandGroup(AExpandingInfoIndex, AGroup)
      else
        Inc(AExpandingInfoIndex);
    end;
  end;

  procedure ExpandGroups;
  begin
    AExpandingInfoIndex := 0;
    while AExpandingInfoIndex < ExpandingInfo.Count do
    begin
      ExpandGroup(AExpandingInfoIndex, DataGroups.TopGroupInfo)
    end;
  end;

begin
  if IsGrouped and IsAlwaysExpanded then
    DataGroups.FullExpanding(True);
  if ExpandingInfo.SaveStates <> [] then
  begin
    if IsEqualFields then
    begin
      PrepareSelectionBeforeExpandGroups;
      if IsGrouped then
        try
          ExpandGroups;
        finally
          RebuildSelectionAfterExpandGroups;
        end;
    end
    else
    begin
      if eisSelected in ExpandingInfo.SaveStates then
        Selection.ClearAll;
    end;
  end;
end;

procedure TdxEMFDataControllerInfo.CheckFocusing;
var
  ANewFocusedRowIndex: Integer;
begin
  if InMemoryMode then
  begin
    inherited CheckFocusing;
    Exit;
  end;
  ANewFocusedRowIndex := FindFocusedRow(False);
  if FocusingInfo.RowIndex <> ANewFocusedRowIndex then
    DoChangeFocusedRow(ANewFocusedRowIndex, True);
  if (dcicFocusedRow in Changes) and (PrevFocusingInfo.RowIndex >= GetRowCount)
  then
    PrevFocusingInfo.RowIndex := -1;
  if DataController.FilteredRecordCountChanged then
    CheckFocusingAfterFilter;
end;

procedure TdxEMFDataControllerInfo.DeleteEntity(ARecordIndex: Integer);
var
  ADataGroupInfo: TdxEMFDataGroupInfo;
  AObject: TObject;
begin
  ADataGroupInfo := DataGroups.GetDataGroupInfoByRecordIndex(ARecordIndex);
  AObject := ADataGroupInfo.DataGroup.Collection
    [ARecordIndex - ADataGroupInfo.FirstRecordListIndex];
  ADataGroupInfo.DataGroup.Remove(AObject);
  if not ADataGroupInfo.DataGroup.Collection.DeleteObjectOnRemove then
    DataController.DataSource.Session.Delete(AObject);
  Dec(ADataGroupInfo.LastRecordListIndex);
  DataController.DataChangedFlag := True;
end;

procedure TdxEMFDataControllerInfo.CreateGroups;
var
  AGroupInfo, AParentGroupInfo: TdxEMFDataGroupInfo;
  I: Integer;
begin
  if InMemoryMode then
    inherited CreateGroups
  else
  begin
    if IsInsertInEmpty then
    begin
      AParentGroupInfo := nil;
      for I := 0 to DataGroups.LevelCount - 1 do
      begin
        AGroupInfo := TdxEMFDataGroupInfo.Create(DataGroups, AParentGroupInfo);
        AGroupInfo.Expanded := I <> DataGroups.LevelCount - 1;
        AGroupInfo.RowIndex := I;
        AGroupInfo.LastRecordListIndex := 0;
        DataGroups.AddToList(AGroupInfo);
        AParentGroupInfo := AGroupInfo;
      end;
    end
    else
      DataGroups.CreateGroups;
  end;
end;

function TdxEMFDataControllerInfo.IsUpdateDataSourceNeeded: Boolean;
begin
  Result := IsDataSourceValid and DataController.HasChanges and
    ([dcicLoad, dcicGrouping, dcicSorting, dcicSummary, dcicView] *
    Changes <> []);
end;

procedure TdxEMFDataControllerInfo.DoFilter;
begin
  if DataController.RecordCount > 0 then
    DataController.FilteredRecordCountChanged := True;
  Selection.InternalClear;
end;

procedure TdxEMFDataControllerInfo.DoLoad;
begin
  RecordList.Count := DataController.RecordCount;
  if not DataController.Active then
  begin
    ClearInfo;
    FocusingInfo.Clear;
  end;
end;

procedure TdxEMFDataControllerInfo.DoUpdate(ASummaryChanged: Boolean);
var
  AIsDoGrouping: Boolean;
begin
  if DataController.IsDetailMode and DataController.InMemoryMode then
  begin
    inherited DoUpdate(ASummaryChanged);
    Exit;
  end;

  if IsUpdateDataSourceNeeded then
    UpdateDataSource(ASummaryChanged);

  AIsDoGrouping := False;
  if not((dcicLoad in Changes) or FocusingInfo.IsUndefined) then
  begin
    if (dcicGrouping in Changes) { or ASummaryChanged } then
    begin
      DataController.NotifyControl(TcxGroupingChangingInfo.Create);
      DoGrouping;
      AIsDoGrouping := True;
    end;
    if (FindFocusedRow(False) < 0) or
      (RecordList.Count <> DataController.RecordCount) or DataGroups.IsInconsistent
    then
    begin
      Include(FChanges, dcicLoad);
      if DataGroups.IsInconsistent then
      begin
        if IsGrouped then
          Include(FChanges, dcicGrouping);
        UpdateDataSource;
      end;
    end;
  end;

  if dcicLoad in Changes then
  begin
    DoLoad;
    DoFilter;
  end;
  if not AIsDoGrouping and (dcicGrouping in Changes) then
  begin
    DataController.NotifyControl(TcxGroupingChangingInfo.Create);
    DoGrouping;
  end;

  if ([dcicLoad, dcicSorting, dcicGrouping] * Changes <> []) then
    ResetFocusing;

  CheckInfo;

  if [dcicSorting, dcicGrouping, dcicRowFixing] * FChanges <> [] then
    DoFixRows;

  if ASummaryChanged then
    DataController.Summary.CalculateFooterSummary;
end;

function TdxEMFDataControllerInfo.FindDataGroup(ARecordListIndex
  : Integer): Integer;
begin
  Result := DataGroups.GetIndexByRecordIndex(ARecordListIndex);
end;

function TdxEMFDataControllerInfo.FindFocusedRow(ANearest: Boolean): Integer;

  function FindFocused(ARecordListIndex, AGroupIndex: Integer): Integer;
  begin
    if FocusingInfo.Level = -1 then
    begin
      if DataGroups[AGroupIndex].FirstRecordListIndex <> ARecordListIndex then
      // Seek Detail
        Result := LocateDetail(AGroupIndex, ARecordListIndex, True)
      else
        Result := LocateGroupByLevel(AGroupIndex, -1);
    end
    else
      Result := LocateDetail(AGroupIndex, ARecordListIndex, True);
  end;

  function FindParentGroup(AIndex, ALevel: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := AIndex downto 0 do
      if DataGroups[I].Level = ALevel then
      begin
        Result := I;
        Break;
      end;
  end;

  function FindNearest(ARecordListIndex, AGroupIndex: Integer): Integer;
  var
    I, ALevel: Integer;
    AItem: TcxDataGroupInfo;
  begin
    if AGroupIndex <> -1 then
    begin
      I := AGroupIndex;
      AItem := DataGroups.GetTopVisibleItem
        (DataGroups[AGroupIndex].RowIndex, I);
      ALevel := FocusingInfo.Level;
      if ALevel = DataGroups.LevelCount then // Detail
      begin
        if (AGroupIndex = I) and DataGroups[AGroupIndex].Expanded then
        begin
          Result := AItem.RowIndex;
          Inc(Result, ARecordListIndex - AItem.FirstRecordListIndex + 1);
          Exit;
        end
        else
          Dec(ALevel);
      end;
      if (ALevel < 0) and (AGroupIndex = I) then
      begin
        Result := AItem.RowIndex;
        Exit;
      end;
      AGroupIndex := FindParentGroup(AGroupIndex, ALevel);
      if AGroupIndex <> -1 then
        Result := DataGroups[AGroupIndex].RowIndex
      else
        Result := -1;
    end
    else
      Result := -1;
  end;

var
  ARecordListIndex, AGroupIndex: Integer;
begin
  FocusingInfo.Validate;
  if FocusingInfo.IsGroup then
    Result := FindFocusedDataGroupIndex
  else
  begin
    if DataController.InMemoryMode then
      Result := inherited FindFocusedRow(ANearest)
    else
    begin
      ARecordListIndex := GetRecordListIndexByFocusingInfo;
      if ARecordListIndex <> -1 then
      begin
        if IsGrouped then
        begin
          AGroupIndex := FindDataGroup(ARecordListIndex); // LastLevel Group
          if ANearest or (AGroupIndex < 0) then
            ARecordListIndex := FindNearest(ARecordListIndex, AGroupIndex)
          else
            ARecordListIndex := FindFocused(ARecordListIndex, AGroupIndex);
        end;
      end;
      Result := ARecordListIndex;
    end;
  end;
end;

function TdxEMFDataControllerInfo.GetSummaryProperties
  (AParentGroup: TdxEMFDataGroup): TArray<IdxCriteriaOperator>;
var
  AResult: TdxArray<IdxCriteriaOperator>;
  ASummaryInfos: TArray<TdxEMFSummaryItem>;
  ASummaryInfo: TdxEMFSummaryItem;
begin
  ASummaryInfos := GetEMFSummaryInfo(AParentGroup);
  AResult := TdxArray<IdxCriteriaOperator>.Create(Length(ASummaryInfos));
  for ASummaryInfo in ASummaryInfos do
    if ASummaryInfo.IsDataBinded and (ASummaryInfo.Kind <> skNone) then
      AResult.Add(ASummaryInfo.GetExpression)
    else
      AResult.Add(TdxConstantValue.Create(TValue.Empty));
  Result := AResult.ToArray;
end;

function TdxEMFDataControllerInfo.GetDataFocusingInfoClass
  : TcxDataFocusingInfoClass;
begin
  Result := TdxEMFDataFocusingInfo;
end;

function TdxEMFDataControllerInfo.GetDataGroupsClass: TcxDataGroupsClass;
begin
  Result := TdxEMFDataGroups;
end;

function TdxEMFDataControllerInfo.GetEMFSummaryInfo(AParentGroup
  : TdxEMFDataGroup): TArray<TdxEMFSummaryItem>;
var
  AResult: TdxArray<TdxEMFSummaryItem>;
  I, ALevel: Integer;
  ASummaryItems: TcxDataSummaryItems;
  ADataGroupFields: TArray<TdxEMFDataField>;

  procedure PopulateSummaries(const AResult: TdxArray<TdxEMFSummaryItem>;
    AItems: TcxDataSummaryItems; ALevel: Integer);
  var
    I: Integer;
    ASummary: TdxEMFSummaryItem;
  begin
    for I := 0 to AItems.Count - 1 do
    begin
      ASummary := AItems[I] as TdxEMFSummaryItem;
      if ASummary.IsDataBinded then
        AResult.Add(ASummary);
    end;
  end;

begin
  if AParentGroup = nil then
    Exit(nil);
  AResult := TdxArray<TdxEMFSummaryItem>.Create(4);
  ALevel := AParentGroup.Level + 1;
  ADataGroupFields := GetGroupFields(ALevel);
  for I := 0 to Length(ADataGroupFields) - 1 do
  begin
    ASummaryItems := DataController.Summary.GetGroupSummaryItems(ALevel, I);
    PopulateSummaries(AResult, ASummaryItems, ALevel);
  end;
  Result := AResult.ToArray;
end;

function TdxEMFDataControllerInfo.GetCollection(AParentGroup: TdxEMFDataGroup)
  : IdxEMFCollection;
var
  M: TdxMappingMemberInfo;
  AMasterRelation: TdxEMFDataRelation;
begin
  if InMemoryMode then
  begin
    if DataController.FMasterEntity = nil then
      Exit(nil);
    AMasterRelation := DataController.MasterRelation;

    M := TdxEMFDataController(AMasterRelation.DataController)
      .DataSource.EntityInfo.FindMember(DataController.CollectionName);
    if (M = nil) or not M.IsAssociationList then
      Exit(TdxEMFCollections.Create(TdxEMFDataController
        (AMasterRelation.DataController).DataSource.EntityInfo.ClassAttributes.
        PersistentClass));
    M.GetValue(DataController.FMasterEntity).AsInterface.QueryInterface
      (IdxEMFCollection, Result);
    Exit;
  end;

  Result := DataController.DataSource.Session.GetObjects
    (DataController.DataSource.EntityInfo.ClassAttributes.PersistentClass,
    TdxGroupOperator.Combine(TdxGroupOperatorType.&And,
    DataController.GetCriteria, AParentGroup.GetGroupCriteria),
    DataController.GetSortByExpressions);
end;

function TdxEMFDataControllerInfo.GetGroupFields(ALevel: Integer)
  : TArray<TdxEMFDataField>;
var
  AResult: TdxArray<TdxEMFDataField>;
  AGroupFieldInfo: TcxGroupFieldInfo;
  I, ACount: Integer;
begin
  ACount := DataGroups.GroupFieldInfoCount;
  AResult := TdxArray<TdxEMFDataField>.Create(ACount);
  for I := 0 to ACount - 1 do
  begin
    AGroupFieldInfo := DataGroups.GroupFieldInfos[I];
    if AGroupFieldInfo.Level = ALevel then
      AResult.Add(TdxEMFDataField(AGroupFieldInfo.Field));
  end;
  Result := AResult.ToArray;
end;

function TdxEMFDataControllerInfo.GetGroupInfoCollection
  (AParentGroup: TdxEMFDataGroup): IdxEMFCollection;
var
  ALevel: Integer;
  ADataSource: TdxEMFDataControllerCustomDataSource;
  AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
  AParentGroupCriteria: IdxCriteriaOperator;
  ADataGroupFields: TArray<TdxEMFDataField>;
  ASortByExpressions: TdxSortByExpressions;
  ABaseSortByExpressions: IdxSortByExpressions;
  ASortByExpression: IdxSortByExpression;
  AGroupByProperty: IdxCriteriaOperator;
  AGroupByProperties: TArray<IdxCriteriaOperator>;
  ASummaries: TArray<TdxEMFSummaryItem>;
  ADataGroupSummaryItems: TcxDataGroupSummaryItems;
  ADataSummaryItem: TdxEMFSummaryItem;
  AAggregateOperand: IdxAggregateOperand;
  AOperandProperty: IdxOperandProperty;
begin
  ADataSource := DataController.DataSource;
  if not((GroupingFieldList.Count = 0) or (AParentGroup = nil)) then
  begin
    ALevel := AParentGroup.Level + 1;
    ADataGroupFields := GetGroupFields(ALevel);
    AGroupByProperties := TdxArray<TdxEMFDataField>.Select<IdxCriteriaOperator>
      (ADataGroupFields,
      function(ADataField: TdxEMFDataField): IdxCriteriaOperator
      begin
        Result := ADataField.GetCriteria(True);
      end);
  end
  else
    ALevel := -1;
  AProperties := TdxCriteriaOperatorCollection.Create;
  AProperties.Add(TdxAggregateOperand.Create(nil, nil,
    TdxAggregateFunctionType.Count, nil));
  if AGroupByProperties <> nil then
  begin
    TdxCriteriaOperatorCollection(AProperties).AddRange(AGroupByProperties);
    TdxCriteriaOperatorCollection(AProperties)
      .AddRange(GetSummaryProperties(AParentGroup));
  end;
  if AGroupByProperties <> nil then
  begin
    ASummaries := GetEMFSummaryInfo(AParentGroup);
    AGroupProperties := TdxCriteriaOperatorCollection.Create
      (AGroupByProperties);
    ASortByExpressions := TdxSortByExpressions.Create;
    ABaseSortByExpressions := DataController.GetSortByExpressions;

    ADataGroupSummaryItems := DataController.Summary.GroupSummaryItems[ALevel];
    if ADataGroupSummaryItems <> nil then
    begin
      ADataSummaryItem := ADataGroupSummaryItems.SortedSummaryItem as
        TdxEMFSummaryItem;
      if (ADataSummaryItem <> nil) and ADataSummaryItem.Sorted and
        (ADataSummaryItem.Kind <> TcxSummaryKind.skNone) then
      begin
        for AGroupByProperty in AProperties do
          if (AGroupByProperty.QueryInterface(IdxAggregateOperand,
            AAggregateOperand) = 0) and
            (AAggregateOperand.AggregatedExpression <> nil) and
            (AAggregateOperand.AggregatedExpression.QueryInterface
            (IdxOperandProperty, AOperandProperty) = 0) then
            if SameText(TdxStringHelper.RemoveEnclosedPart
              (AOperandProperty.PropertyName, '[', ']'),
              ADataSummaryItem.FieldName) then
              ASortByExpressions.Add(ADataSummaryItem.GetExpression,
                SortOrderToSortDirection(GroupingFieldList[ALevel].SortOrder));
      end;
    end;

    for AGroupByProperty in AGroupByProperties do
    begin
      for ASortByExpression in ABaseSortByExpressions do
        if TdxCriteriaOperator.CriterionEquals(AGroupByProperty,
          ASortByExpression.Expression as IdxCriteriaOperator) then
        begin
          ASortByExpressions.Add(ASortByExpression);
          Continue;
        end;
      ASortByExpressions.Add(AGroupByProperty);
    end;
  end
  else
    ASortByExpressions := nil;
  if AParentGroup <> nil then
    AParentGroupCriteria := AParentGroup.GetGroupCriteria
  else
    AParentGroupCriteria := nil;
  Result := TdxEMFCustomSessionAccess(ADataSource.Session)
    .GetProjections(ADataSource.EntityInfo.ClassAttributes.PersistentClass,
    AProperties, TdxGroupOperator.Combine(TdxGroupOperatorType.&And,
    DataController.GetCriteria, AParentGroupCriteria), AGroupProperties, nil,
    False, 0, 0, ASortByExpressions);
end;

function TdxEMFDataControllerInfo.FindFocusedDataGroupIndex: Integer;
begin
  if not DataController.GroupingChangedFlag then
  begin
    if DataController.RowCount > FocusingInfo.RowIndex then
      Result := FocusingInfo.RowIndex
    else
      Result := -1;
  end
  else if DataController.RecordCount > 0 then
    Result := 0
  else
    Result := -1;
end;

function TdxEMFDataControllerInfo.GetInternalRecordCount: Integer;
begin
  Result := DataController.GetRecordCount;
end;

function TdxEMFDataControllerInfo.GetInternalRecordIndex(ARecordListIndex
  : Integer): Integer;
begin
  if InMemoryMode then
    Result := inherited GetInternalRecordIndex(ARecordListIndex)
  else
    Result := ARecordListIndex
end;

function TdxEMFDataControllerInfo.GetInternalRecordListIndex
  (ARecordIndex: Integer): Integer;
begin
  if InMemoryMode then
    Result := inherited GetInternalRecordListIndex(ARecordIndex)
  else
  begin
    if ARecordIndex >= DataController.RecordCount then
      Result := -1
    else
      Result := ARecordIndex
  end;
end;

function TdxEMFDataControllerInfo.GetRecordIndex(AObject: TObject): Integer;
begin
  Result := DataGroups.GetRecordIndex(AObject);
end;

function TdxEMFDataControllerInfo.GetRecordListIndexByFocusingInfo: Integer;
begin
  if (FocusingInfo.Data = nil) or DataController.InMemoryMode then
    Result := inherited GetRecordListIndexByFocusingInfo
  else if FocusingInfo.IsUndefined then
    Result := -1
  else
    Result := DataGroups.GetRecordIndex(FocusingInfo.Data);
end;

function TdxEMFDataControllerInfo.IsDataSourceValid: Boolean;
begin
  Result := not DataController.IsDestroying and
    (DataController.DataSource <> nil) and
    not DataController.DataSource.IsDestroying;
end;

procedure TdxEMFDataControllerInfo.PrepareTopGroupInfo(ADataGroupInfo
  : TdxEMFDataGroupInfo);
var
  ACollection: IdxEMFCollection;
  ARow: TdxSelectStatementResultRow;
  ACount: Integer;
begin
  if DataController.InMemoryMode then
  begin
    ACollection := ADataGroupInfo.DataGroup.Collection;
    if ACollection = nil then
      ACount := 0
    else
      ACount := ACollection.Count;
    ADataGroupInfo.FirstRecordListIndex := 0;
    ADataGroupInfo.LastRecordListIndex := ACount - 1;
  end
  else
  begin
    ACollection := GetGroupInfoCollection(ADataGroupInfo.DataGroup);
    ARow := ACollection.First as TdxSelectStatementResultRow;
    ADataGroupInfo.DataGroup := TdxEMFDataGroup.Create(ADataGroupInfo.Owner);
    ADataGroupInfo.DataGroup.Level := -1;
    ADataGroupInfo.DataGroup.SetDataGroupInfo(ARow, 0);
    ADataGroupInfo.DataGroup.AssignTo(ADataGroupInfo);
  end;
end;

procedure TdxEMFDataControllerInfo.ResetFocusing;
begin
  if DataController.MultiSelect then
    Selection.Clear;
  inherited ResetFocusing;
end;

procedure TdxEMFDataControllerInfo.SaveExpanding(ASaveStates
  : TcxDataExpandingInfoStateSet);

  procedure AddGroup(ADataGroupInfo: TdxEMFDataGroupInfo; AGroupIndex: Integer;
  AExpanded: Boolean);
  var
    ARecordIndex: Integer;
    AStateSet: TcxDataExpandingInfoStateSet;
  begin
    ARecordIndex := ADataGroupInfo.FirstRecordListIndex;
    if ARecordIndex < DataController.RecordCount then
    begin
      AStateSet := [];
      if AExpanded and ((Selection.Count > 0) or not IsAlwaysExpanded) then
        AStateSet := AStateSet + [eisExpanded];
      if AStateSet <> [] then
        ExpandingInfo.AddItem(ADataGroupInfo.Level, ADataGroupInfo.GroupValue,
          AStateSet);
    end;
  end;

  procedure AddGroups;
  var
    I: Integer;
    ADataGroupInfo: TdxEMFDataGroupInfo;
  begin
    for I := 0 to DataGroups.Count - 1 do
    begin
      ADataGroupInfo := DataGroups[I];
      if ADataGroupInfo.Expanded then
        AddGroup(ADataGroupInfo, I, True);
    end;
  end;

var
  I: Integer;
begin
  if ExpandingInfo.SaveStates <> ASaveStates then
  begin
    if (ASaveStates <> []) and IsValidDataGroupInfo then
    begin
      ExpandingInfo.SaveStates := ASaveStates;
      ExpandingInfo.ClearFields;
      for I := 0 to DataGroups.GroupFieldInfoCount - 1 do
        ExpandingInfo.AddField(DataGroups.GroupFieldInfos[I].Field);
      if IsGrouped then
        AddGroups;
    end
    else
    begin
      ExpandingInfo.Clear;
      ExpandingInfo.SaveStates := ASaveStates;
    end;
  end;
end;

procedure TdxEMFDataControllerInfo.UpdateDataSource(ASummaryChanged
  : Boolean = False);
begin
  GetTotalSortingFields;
  if (not IsGrouped or ([dcicGrouping] * Changes <> []) { or ASummaryChanged } )
  then
    DataController.ClearData
  else
    DataController.SoftClearData;
end;

{ TdxEMFDataFocusingInfo }

procedure TdxEMFDataFocusingInfo.Assign(AFocusingInfo: TcxDataFocusingInfo);
var
  AInfo: TdxEMFDataFocusingInfo absolute AFocusingInfo;
begin
  if AFocusingInfo is TdxEMFDataFocusingInfo then
  begin
    FData := AInfo.FData;
    FGroupValue := AInfo.FGroupValue;
  end;
  inherited Assign(AFocusingInfo);
end;

function TdxEMFDataFocusingInfo.IsEqual(AFocusingInfo
  : TcxDataFocusingInfo): Boolean;
begin
  Result := inherited IsEqual(AFocusingInfo) and
    (AFocusingInfo is TdxEMFDataFocusingInfo) and
    (TdxEMFDataFocusingInfo(AFocusingInfo).Data = Data);
end;

procedure TdxEMFDataFocusingInfo.SavePos;
var
  ARowInfo: TcxRowInfo;
begin
  if DataControllerInfo.InMemoryMode then
    inherited SavePos
  else if RowIndex <> -1 then
  begin
    ARowInfo := DataControllerInfo.GetRowInfo(RowIndex);
    RecordIndex := ARowInfo.RecordIndex;
    Level := ARowInfo.Level;
    if RecordIndex <> -1 then
    begin
      if Level = DataControllerInfo.DataGroups.LevelCount then
      begin
        if (DataControllerInfo.DataController.DetailMode =
          TcxDataControllerDetailMode.dcdmPattern) or DataControllerInfo.IsInsertInEmpty
        then
          FData := nil
        else
          FData := DataControllerInfo.GetEntityByRecordIndex(RecordIndex);
      end
      else
        FData := nil;
    end;
    CalculateGroupValue(ARowInfo.GroupIndex);
  end
  else
    ResetPos;
end;

procedure TdxEMFDataFocusingInfo.SetRowData(AData: TObject);
begin
  if FData = AData then
    Exit;
  FData := AData;
end;

procedure TdxEMFDataFocusingInfo.DataClear;
begin
  FData := nil;
  VarClear(FGroupValue);
end;

procedure TdxEMFDataFocusingInfo.Validate;
begin
  ValidateByData;
  ValidateByGroupValue;
end;

function TdxEMFDataFocusingInfo.IsGroup: Boolean;
begin
  Result := not VarIsEmpty(FGroupValue);
end;

function TdxEMFDataFocusingInfo.IsUndefined: Boolean;
begin
  Result := (RowIndex < 0) or ((FData = nil) and not IsGroup);
end;

procedure TdxEMFDataFocusingInfo.ResetPos;
begin
  inherited ResetPos;
  FData := nil;
end;

procedure TdxEMFDataFocusingInfo.ValidateByData;
begin
  if (Data = nil) or DataControllerInfo.InMemoryMode then
    Exit;
  RecordIndex := DataControllerInfo.DataGroups.GetRecordIndex(Data);
end;

procedure TdxEMFDataFocusingInfo.ValidateByGroupValue;
var
  I: Integer;
  AResultGroup, AGroup: TdxEMFDataGroupInfo;
begin
  if not VarIsArray(FGroupValue) or DataControllerInfo.DataController.NewItemRowFocused
  then
    Exit;
  AResultGroup := DataControllerInfo.DataGroups.TopGroupInfo;
  for I := VarArrayLowBound(FGroupValue, 1) to VarArrayHighBound
    (FGroupValue, 1) do
  begin
    AGroup := AResultGroup.FindChildGroup(FGroupValue[I]);
    if AGroup = nil then
      Break;
    AResultGroup := AGroup;
  end;
  if AResultGroup <> nil then
  begin
    RowIndex := AResultGroup.RowIndex;
    RecordIndex := AResultGroup.FirstRecordListIndex;
  end;
end;

procedure TdxEMFDataFocusingInfo.CalculateGroupValue(ADataGroupIndex: Integer);
var
  ALevel: Integer;
  AGroupInfo: TdxEMFDataGroupInfo;
begin
  FGroupValue := Unassigned;
  if (FData <> nil) or (ADataGroupIndex = -1) then
    Exit;
  ALevel := Level;
  FGroupValue := VarArrayCreate([0, ALevel], varVariant);
  AGroupInfo := DataControllerInfo.DataGroups[ADataGroupIndex];
  repeat
    FGroupValue[ALevel] := AGroupInfo.GroupValue;
    Dec(ALevel);
    AGroupInfo := AGroupInfo.ParentGroup;
  until AGroupInfo = nil;
end;

procedure TdxEMFDataFocusingInfo.Clear;
begin
  inherited Clear;
  DataClear;
end;

function TdxEMFDataFocusingInfo.GetDataControllerInfo: TdxEMFDataControllerInfo;
begin
  Result := TdxEMFDataControllerInfo(inherited DataControllerInfo);
end;

{ TdxEMFDataProvider }

constructor TdxEMFDataProvider.Create(ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
  FEditingRow := TdxEMFEditingRow.Create;
  FCurrentIndex := -1;
end;

destructor TdxEMFDataProvider.Destroy;
begin
  FreeAndNil(FEditingRow);
  inherited Destroy;
end;

function TdxEMFDataProvider.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFDataProvider.GetDataControllerInfo: TdxEMFDataControllerInfo;
begin
  Result := DataController.DataControllerInfo;
end;

function TdxEMFDataProvider.GetDataSource: TdxEMFDataControllerCustomDataSource;
begin
  Result := DataController.DataSource;
end;

function TdxEMFDataProvider.GetSession: TdxEMFCustomSession;
begin
  Result := DataSource.Session;
end;

procedure TdxEMFDataProvider.AddRecord(AIsAppending: Boolean);
begin
  if DataSource = nil then
    Exit;
  if DataControllerInfo.DataController.UseNewItemRowForEditing then
    FCurrentIndex := -1
  else
    FCurrentIndex := DataControllerInfo.FocusingInfo.RecordIndex;
  DataController.CheckBrowseMode;
  DataController.LockStateInfo(False);
  FInInserting := True;
  FIsAppending := AIsAppending;
  try
    ClearEditingRow;
    InsertingRecord(AIsAppending);
  finally
    FInInserting := False;
    DataController.Change([dccFocus, dccUpdateRecord]);
    DataController.UnlockStateInfo(False);
  end;
end;

procedure TdxEMFDataProvider.Append;
begin
  AddRecord(True);
end;

procedure TdxEMFDataProvider.AssignItemValue(ARecordIndex: Integer;
AField: TcxCustomDataField; const AValue: Variant);
var
  ADataField: TdxEMFDataField absolute AField;
begin
  if ADataField.MemberInfo <> nil then
    SetEditingData(ADataField, ARecordIndex, AValue)
  else
    inherited AssignItemValue(ARecordIndex, AField, AValue);
end;

procedure TdxEMFDataProvider.Cancel;
begin
  InternalCancel;
  inherited Cancel;
end;

function TdxEMFDataProvider.CanDelete: Boolean;
begin
  Result := inherited CanDelete and CanModify;
end;

function TdxEMFDataProvider.CanInitEditing(ARecordIndex: Integer): Boolean;
begin
  Result := True;
  FInCanInitEditing := True;
  try
    if DataController.UseNewItemRowForEditing and
      DataController.NewItemRowFocused and not IsInserting then
      Insert
    else
      Edit;
    SetChanging;
    if DataController.UseNewItemRowForEditing and DataController.NewItemRowFocused
    then
      DataController.Change([dccUpdateRecord]);
  finally
    FInCanInitEditing := False;
  end;
end;

procedure TdxEMFDataProvider.ClearEditingRow;
begin
  FEditingRow.Clear;
end;

function TdxEMFDataProvider.GetCurrentObject: TObject;
begin
  Result := GetObject(FCurrentIndex);
end;

function TdxEMFDataProvider.GetEntityInfo: TdxEntityInfo;
var
  ADataSource: TdxEMFDataControllerCustomDataSource;
begin
  ADataSource := DataSource;
  if ADataSource = nil then
    Result := nil
  else
    Result := ADataSource.EntityInfo;
end;

function TdxEMFDataProvider.GetFieldList(const AFieldNames: string)
  : TArray<TdxMappingMemberInfo>;
var
  AEntityInfo: TdxEntityInfo;
  ALen, APos: Integer;
  AMemberInfo: TdxMappingMemberInfo;
  AResult: TdxArray<TdxMappingMemberInfo>;
begin
  AEntityInfo := EntityInfo;
  if AEntityInfo = nil then
    Exit(nil);
  ALen := Length(AFieldNames);
  APos := 1;
  AResult := TdxArray<TdxMappingMemberInfo>.Create(4);
  while APos <= ALen do
  begin
    AMemberInfo := AEntityInfo.FindMember(ExtractFieldName(AFieldNames, APos));
    AResult.Add(AMemberInfo);
  end;
  Result := AResult.ToArray;
end;

procedure TdxEMFDataProvider.Insert;
begin
  AddRecord(False);
end;

procedure TdxEMFDataProvider.InternalCancel;
var
  ARecordCount: Integer;
begin
  ARecordCount := DataController.RecordCount;
  if IsInserting then
  begin
    if DataController.HasRelations then
      DataController.Relations.DeleteDetailObject(nil);
    Dec(ARecordCount);
  end;
  DataController.DataChangedFlag := False;
  if ARecordCount = 0 then
    DataController.DataControllerInfo.FocusingInfo.Clear
  else
    DataController.DataControllerInfo.FocusingInfo.DataClear;
end;

procedure TdxEMFDataProvider.InternalPost;
var
  ARecordIndex: Integer;
  AKey: TObject;
begin
  if IsModified then
  begin
    DataController.DoBeforePost;
    DataController.LockStateInfo;
    try
      ARecordIndex := EditingRecordIndex;
      DataController.DataChangedFlag := True;
      AKey := DataController.PostEditingEntity(ARecordIndex, FEditingRow);
      if AKey <> nil then
      begin
        if IsInserting and DataController.HasRelations then
          DataController.Relations.ChangeDetailObjectKey(nil, AKey);
        DataController.DataControllerInfo.FocusingInfo.SetRowData(AKey);
      end;
      DataController.DataControllerInfo.FChanges :=
        DataController.DataControllerInfo.FChanges + [dcicFocusedRow, dcicLoad,
        dcicGrouping, dcicSummary];
      ResetEditing;
    finally
      DataController.UnlockStateInfo;
    end;
    DataController.DoAfterPost;
  end;
end;

function TdxEMFDataProvider.IsActive: Boolean;
begin
  Result := DataController.InMemoryMode or
    ((DataSource <> nil) and DataSource.Active);
end;

function TdxEMFDataProvider.IsBOF: Boolean;
begin
  Result := FCurrentIndex = 0;
end;

function TdxEMFDataProvider.IsEOF: Boolean;
begin
  Result := (FCollection = nil) or (FCurrentIndex >= FCollection.Count);
end;

procedure TdxEMFDataProvider.Delete;
var
  ARecordIndex: Integer;
begin
  ARecordIndex := DataController.DataControllerInfo.FocusedRecordIndex;
  if ARecordIndex <> -1 then
  begin
    if FInserting then
      DataController.Cancel
    else
    begin
      DataController.DoBeforeDelete(ARecordIndex);
      if DataController.IsProviderMode then
        DataController.SaveKeys;
      try
        DataController.LockStateInfo;
        try
          DataController.DeleteRecord(ARecordIndex);
          DataController.CheckNearestFocusRow;
        finally
          DataController.UnlockStateInfo;
        end;
      finally
        if DataController.IsProviderMode then
          DataController.RestoreKeys;
      end;
      DataController.DoAfterDelete;
    end;
  end;
end;

procedure TdxEMFDataProvider.DeleteRecord(ARecordIndex: Integer);
begin
  DataController.LockStateInfo;
  try
    if not FInserting then
      DataController.DataControllerInfo.DeleteEntity(ARecordIndex);
    DataController.DataControllerInfo.FocusingInfo.Clear;
    DataChanged(dcTotal, -1, -1);
  finally
    DataController.UnlockStateInfo;
  end;
end;

procedure TdxEMFDataProvider.DeleteRecords(ACriteria: IdxCriteriaOperator);
begin
  if not IsInserting then
  begin
    DataController.LockStateInfo;
    try
      if not FInserting then
        DataSource.Delete(ACriteria);
      DataController.DataControllerInfo.FocusingInfo.Clear;
      DataChanged(dcTotal, -1, -1);
    finally
      DataController.UnlockStateInfo;
    end;
  end;
end;

procedure TdxEMFDataProvider.DoInsertingRecord(AIsAppending: Boolean);
begin
  inherited DoInsertingRecord(AIsAppending);
  DataController.DataControllerInfo.Refresh;
end;

procedure TdxEMFDataProvider.DoUpdateData;
begin
  inherited DoUpdateData;
end;

procedure TdxEMFDataProvider.Edit;
begin
  if (DataSource <> nil) and not IsEditing then
  begin
    ClearEditingRow;
    EditingRecord;
  end;
end;

procedure TdxEMFDataProvider.First;
begin
  inherited First;
  FCollection := DataControllerInfo.GetCollection(nil);
  FCurrentIndex := 0;
end;

procedure TdxEMFDataProvider.Last;
begin
  FCurrentIndex := FCollection.Count - 1;
end;

procedure TdxEMFDataProvider.Next;
begin
  Inc(FCurrentIndex);
end;

procedure TdxEMFDataProvider.Post(AForcePost: Boolean);
var
  ALink: TcxDataListenerLink;
begin
  if not IsActive then
    Exit;
  ALink := TdxEMFDataController.AddListenerLink(DataController);
  try
    DataController.LockStateInfo(True);
    try
      UpdateData;
      if AForcePost then
      begin
        InternalPost;
        FCurrentIndex := DataControllerInfo.FocusingInfo.RecordIndex;
      end
      else
        DataController.CheckBrowseMode;
    finally
      DataController.UnlockStateInfo(True);
    end;
  finally
    if ALink.Ref <> nil then
      InsertedRecordIndex := -1;
    TdxEMFDataController.RemoveListenerLink(ALink);
  end;
end;

procedure TdxEMFDataProvider.PostEditingData;
begin
  UpdateData;
end;

procedure TdxEMFDataProvider.Prev;
begin
  Dec(FCurrentIndex);
end;

procedure TdxEMFDataProvider.RecordChanged;
begin
  if IsEditing then
    DataController.UpdateEditingRecord;
  ResetChanging;
end;

procedure TdxEMFDataProvider.ResetEditing;
begin
  inherited ResetEditing;
  FIsAppending := False;
  ClearEditingRow;
end;

function TdxEMFDataProvider.SetEditingData(AField: TdxEMFDataField;
ARecordIndex: Integer; const AValue: Variant): Boolean;
var
  AEMFField: TdxEMFDataField absolute AField;
begin
  Result := (IsEditing or IsInserting) and (EditingRecordIndex = ARecordIndex)
    and (AEMFField.MemberInfo <> nil);
  if Result then
    FEditingRow.AddOrSetValue(AEMFField.MemberInfo, AValue);
end;

function TdxEMFDataProvider.SetEditValue(ARecordIndex: Integer;
AField: TcxCustomDataField; const AValue: Variant;
AEditValueSource: TcxDataEditValueSource): Boolean;
var
  AEMFField: TdxEMFDataField absolute AField;
begin
  Result := SetEditingData(AEMFField, ARecordIndex, AValue);
  if Result then
    SetModified;
end;

procedure TdxEMFDataProvider.UpdateData;
begin
  DoUpdateData;
  RecordChanged;
  ResetChanging;
end;

function TdxEMFDataProvider.GetObject(ARecordIndex: Integer): TObject;
begin
  if FCollection <> nil then
    Result := FCollection[ARecordIndex]
  else
    Result := DataControllerInfo.GetEntityByRecordIndex(ARecordIndex);
end;

function TdxEMFDataProvider.GetRecordIndex: Integer;
begin
  Result := FCurrentIndex;
end;

function TdxEMFDataProvider.GetRowValue(ARecordIndex: Integer;
AField: TdxEMFDataField): TValue;
var
  AObject: TObject;
begin
  AObject := GetObject(ARecordIndex);
  if AObject = nil then
    Result := TValue.Empty
  else if AField.MemberInfo.IsAssociationList then
    Result := '(Association)'
  else
    Result := AField.MemberInfo.GetValue(AObject);
end;

function TdxEMFDataProvider.GetRowValueAsVariant(ARecordIndex: Integer;
AField: TdxEMFDataField): Variant;

  function GetEditingRow: Variant;
  begin
    if not FEditingRow.TryGetValue(AField.MemberInfo, Result) then
      Result := Null;
  end;

var
  AObject: TObject;
  AMemberInfo: TdxMappingMemberInfo;
begin
  if (ARecordIndex = EditingRecordIndex) and
    (IsInserting or (IsEditing and not VarIsNull(GetEditingRow))) then
    Result := GetEditingRow
  else
  begin
    AObject := GetObject(ARecordIndex);
    if AObject = nil then
      Result := Null
    else
    begin
      AMemberInfo := AField.MemberInfo;
      if AMemberInfo.IsAssociationList then
        Result := '(Association)'
      else
        Result := AMemberInfo.GetAsVariant(AObject);
    end;
  end;
end;

function TdxEMFDataProvider.GetValueDefReaderClass: TcxValueDefReaderClass;
begin
  Result := TdxValueDefEMFReader;
end;

{ TdxValueDefEMFReader }

function TdxValueDefEMFReader.GetDisplayText(AValueDef: TcxValueDef): string;
begin
  Result := inherited GetDisplayText(AValueDef);
end;

function TdxValueDefEMFReader.GetValue(AValueDef: TcxValueDef): Variant;
var
  ADataField: TdxEMFDataField;
  AObject: TObject;
begin
  ADataField := TdxEMFDataField(AValueDef.LinkObject);
  if (ADataField <> nil) then
  begin
    AObject := ADataField.DataController.Provider.GetCurrentObject;
    Result := ADataField.MemberInfo.GetAsVariant(AObject);
  end
  else
    Result := inherited GetValue(AValueDef);
end;

function TdxValueDefEMFReader.IsInternal(AValueDef: TcxValueDef): Boolean;
begin
  Result := TdxEMFDataField(AValueDef.LinkObject).IsValueDefInternal;
end;

procedure TdxValueDefEMFReader.Read(ABuffer: PAnsiChar; AValueDef: TcxValueDef);
begin
  if TdxEMFDataField(AValueDef.LinkObject).IsExpression then
    ClearValue(ABuffer, AValueDef)
  else
    inherited Read(ABuffer, AValueDef);
end;

{ TdxEMFDataController }

destructor TdxEMFDataController.Destroy;
begin
  DataSource := nil;
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

function TdxEMFDataController.GetRelations: TcxEMFDataRelationList;
begin
  Result := TcxEMFDataRelationList(inherited Relations);
end;

function TdxEMFDataController.GetSession: TdxEMFCustomSession;
begin
  Result := DataSource.Session;
end;

procedure TdxEMFDataController.DeleteFocused;
var
  ARowIndex: Integer;
  ADataGroupInfo: TdxEMFDataGroupInfo;
  ADataGroupIndex: Integer;
begin
  ARowIndex := GetFocusedRowIndex;
  if (ARowIndex <> -1) and Provider.CanDelete then
  begin
    Provider.BeginDeleting;
    try
      if GetRowInfo(ARowIndex).Level < Groups.LevelCount then // It's Group Row
      begin
        ADataGroupIndex := Groups.DataGroupIndexByRowIndex[ARowIndex];
        ADataGroupInfo := DataControllerInfo.DataGroups[ADataGroupIndex];
        ADataGroupInfo.DataGroup.DeleteRecords;
        DataControllerInfo.FocusingInfo.Clear;
        Provider.DataChanged(dcTotal, -1, -1);
      end
      else
        DeleteFocusedRecord;
      ClearSelection;
    finally
      Provider.EndDeleting;
    end;
  end;
end;

procedure TdxEMFDataController.DeleteInSmartLoad(ARecordIndex: Integer);
begin
  Provider.DeleteRecord(ARecordIndex);
end;

procedure TdxEMFDataController.DeleteSelection;
begin
  inherited;
end;

procedure TdxEMFDataController.FreeNotification(Sender: TComponent);
begin
  if Sender = DataSource then
    DataSource := nil;
end;

function TdxEMFDataController.GetDataControllerInfo: TdxEMFDataControllerInfo;
begin
  Result := TdxEMFDataControllerInfo(inherited DataControllerInfo);
end;

function TdxEMFDataController.GetFilter: TdxEMFDataFilterCriteria;
begin
  Result := TdxEMFDataFilterCriteria(inherited Filter);
end;

function TdxEMFDataController.GetDataProviderClass: TcxCustomDataProviderClass;
begin
  Result := TdxEMFDataProvider;
end;

function TdxEMFDataController.GetDataSelectionClass: TcxDataSelectionClass;
begin
  Result := TdxEMFDataSelection;
end;

function TdxEMFDataController.GetDetailDataController(ADetailObject
  : TcxDetailObject; ARelationIndex: Integer): TcxCustomDataController;
begin
  Result := GetDetailDataControllerByLinkObject
    (ADetailObject.LinkObjects[ARelationIndex]);
end;

function TdxEMFDataController.GetEMFMasterRelation: TdxEMFDataRelation;
begin
  Result := TdxEMFDataRelation(GetMasterRelation);
end;

function TdxEMFDataController.GetClearDetailsOnCollapse: Boolean;
begin
  Result := True;
end;

function TdxEMFDataController.GetCriteria: IdxCriteriaOperator;
var
  I, ACount: Integer;
begin
  if Filter.IsFiltering then
    Result := Filter.GetCriteriaOperator;
  if IsDetailMode and (DetailMode = TcxDataControllerDetailMode.dcdmClone) then
  begin
    ACount := Length(FMasterDetailKeyValues);
    if ACount = 0 then
      Result := TdxBinaryOperator.Create(TdxOperandValue.Create(1),
        TdxOperandValue.Create(1), TdxBinaryOperatorType.NotEqual)
    else
      for I := 0 to ACount - 1 do
        Result := TdxGroupOperator.Combine(TdxGroupOperatorType.&And, Result,
          BinaryOperatorCreate(MasterDetailKeyFields[I].MemberName,
          FMasterDetailKeyValues[I]));
  end;
end;

procedure TdxEMFDataController.DataSourceChangedHandler(Sender: TObject);
var
  AActive: Boolean;
begin
  if not DataChangedNotifyLocked then
  begin
    AActive := (DataSource <> nil) and DataSource.Active;
    try
      ActiveChanged(AActive);
    except
      ActiveChanged(False);
      raise;
    end;
  end;
end;

procedure TdxEMFDataController.ActiveChanged(AActive: Boolean);
var
  ARelationCount, ARelationIndex: Integer;
  ARelations: TcxEMFDataRelationList;
  ADataRelation: TcxCustomDataRelation;
  ADetailObject: TcxDetailObject;
  ADataController: TdxEMFDataController;
begin
  if not AActive then
    ResetEntityInfo;
  if IsPattern then
  begin
    ADataRelation := GetMasterRelation;
    if ADataRelation <> nil then
    begin
      ARelations := ADataRelation.RelationList as TcxEMFDataRelationList;

      ARelationCount := ARelations.Count;
      if ARelationCount > 0 then
        for ADetailObject in ARelations.DetailObjects.Values do
          for ARelationIndex := 0 to ARelationCount - 1 do
            if IsDetailDataControllerExist(ADetailObject, ARelationIndex) then
            begin
              ADataController := GetDetailDataController(ADetailObject,
                ARelationIndex) as TdxEMFDataController;
              ADataController.ActiveChanged(AActive);
            end;
    end;
  end;
  inherited ActiveChanged(AActive);
end;

function TdxEMFDataController.AppendInSmartLoad: Integer;
begin
  Result := RecordCount;
  DataChanged(dcNew, -1, -1);
end;

function TdxEMFDataController.DoGetGroupRowDisplayText(const ARowInfo
  : TcxRowInfo; var AItemIndex: Integer): string;
var
  AGroupIndex: Integer;
  ADataGroupInfo: TdxEMFDataGroupInfo;
begin
  if InMemoryMode then
    Result := inherited
  else
  begin
    Provider.Freeze;
    try
      AGroupIndex := DataControllerInfo.DataGroups.GetIndexByRowIndex
        (ARowInfo.RowIndex);
      ADataGroupInfo := DataControllerInfo.DataGroups.Items[AGroupIndex];
      if ADataGroupInfo.GroupedItemCount = 1 then
        Result := VarToStr(ADataGroupInfo.GroupValue)
      else
        Result := VarToStr(ADataGroupInfo.GroupValue[AItemIndex]);
    finally
      Provider.Unfreeze;
    end;
  end;
end;

procedure TdxEMFDataController.DoUpdateRecord(ARecordIndex: Integer);
begin
  if not Provider.InCanInitEditing then
    Change([dccUpdateRecord]);
end;

function TdxEMFDataController.GetField(Index: Integer): TdxEMFDataField;
begin
  Result := TdxEMFDataField(inherited Fields[Index]);
end;

function TdxEMFDataController.GetFieldClass: TcxCustomDataFieldClass;
begin
  Result := TdxEMFDataField;
end;

function TdxEMFDataController.GetFieldsCount: Integer;
begin
  Result := inherited Fields.Count;
end;

function TdxEMFDataController.GetFreeNotificator: TcxFreeNotificator;
begin
  if FFreeNotificator = nil then
  begin
    FFreeNotificator := TcxFreeNotificator.Create(nil);
    FFreeNotificator.OnFreeNotification := FreeNotification;
  end;
  Result := FFreeNotificator;
end;

function TdxEMFDataController.GetProvider: TdxEMFDataProvider;
begin
  Result := TdxEMFDataProvider(inherited Provider);
end;

function TdxEMFDataController.GetItemFieldName(AItemIndex: Integer): string;
begin
  CheckItemRange(AItemIndex);
  Result := Fields[AItemIndex].FieldName;
end;

procedure TdxEMFDataController.CheckBrowseMode;
begin
  if not Provider.IsEditing then
    Exit;
  LockStateInfo(False);
  try
    Provider.PostEditingData;
    Provider.InternalPost;
    Cancel;
  finally
    UnlockStateInfo(False);
  end;
end;

function TdxEMFDataController.CheckDetailsBrowseMode: Boolean;
var
  ADetailObject: TcxDetailObject;
  ARelationIndex, ARelationCount: Integer;
  ADataController: TcxCustomDataController;
begin
  ARelationCount := Relations.Count;
  if ARelationCount > 0 then
    for ADetailObject in Relations.DetailObjects.Values do
      for ARelationIndex := 0 to ARelationCount - 1 do
        if IsDetailDataControllerExist(ADetailObject, ARelationIndex) then
        begin
          ADataController := GetDetailDataController(ADetailObject,
            ARelationIndex);
          ADataController.CheckBrowseMode;
        end;
  Result := True;
end;

procedure TdxEMFDataController.ClearData;
begin
  Groups.DataGroups.ClearData;
  FDataChangedFlag := False;
  FSummaryChanged := False;
end;

procedure TdxEMFDataController.ClearDetails;
begin
  BeginUpdate;
  try
    Relations.ClearDetailLinkObjects;
  finally
    EndUpdate;
  end;
end;

procedure TdxEMFDataController.ClearDetailsMasterRelation
  (ARelation: TcxCustomDataRelation);
var
  ARelationIndex: Integer;
  ADetailObject: TcxDetailObject;
  ADetailLinkObject: TObject;
begin
  ARelationIndex := ARelation.Index;
  for ADetailObject in Relations.DetailObjects.Values do
  begin
    ADetailLinkObject := ADetailObject.LinkObjects[ARelationIndex];
    if ADetailLinkObject <> nil then
      TdxEMFDataController(GetDetailDataControllerByLinkObject
        (ADetailLinkObject)).MasterRelation := nil;
  end;
end;

procedure TdxEMFDataController.SoftClearData;
begin
  Groups.DataGroups.SoftClearData;
  FDataChangedFlag := False;
  FSummaryChanged := False;
end;

procedure TdxEMFDataController.SummaryChanged(ARedrawOnly: Boolean);
begin
  if not ARedrawOnly then
    FSummaryChanged := True;
  inherited SummaryChanged(ARedrawOnly);
end;

function TdxEMFDataController.CreateDataControllerInfo
  : TcxCustomDataControllerInfo;
begin
  Result := TdxEMFDataControllerInfo.Create(Self);
end;

function TdxEMFDataController.CreateDataRelationList: TcxCustomDataRelationList;
begin
  Result := TcxEMFDataRelationList.Create(Self);
end;

function TdxEMFDataController.CreateFindCriteria: TcxDataFindCriteria;
begin
  Result := TdxEMFDataFindCriteria.Create(Self);
end;

procedure TdxEMFDataController.DataSourceChanged;
var
  ANewActive: Boolean;
begin
  ANewActive := (DataSource <> nil) and DataSource.Active;
  if Active <> ANewActive then
    ActiveChanged(ANewActive);
end;

procedure TdxEMFDataController.UpdateData;
begin
  // do nothing
end;

procedure TdxEMFDataController.UpdateEditingRecord;
begin
  if Provider.EditingRecordIndex = cxNullEditingRecordIndex then
    Exit;
  DoUpdateRecord(Provider.EditingRecordIndex);
end;

function TdxEMFDataController.GetFilterCriteriaClass
  : TcxDataFilterCriteriaClass;
begin
  Result := TdxEMFDataFilterCriteria;
end;

function TdxEMFDataController.GetGroupRowValueByItemIndex(const ARowInfo
  : TcxRowInfo; AItemIndex: Integer): Variant;
var
  AGroupIndex: Integer;
begin
  if InMemoryMode then
    Result := inherited
  else
  begin
    Provider.Freeze;
    try
      AGroupIndex := DataControllerInfo.DataGroups.GetIndexByRowIndex
        (ARowInfo.RowIndex);
      if AGroupIndex < 0 then
        Result := Null
      else
      begin
        Result := DataControllerInfo.DataGroups.Items[AGroupIndex].GroupValue;
        if VarIsArray(Result) then
          Result := Result[Groups.ItemGroupIndex[AItemIndex]];
      end;
    finally
      Provider.Unfreeze;
    end;
  end;
end;

function TdxEMFDataController.GetGroups: TdxEMFDataControllerGroups;
begin
  Result := TdxEMFDataControllerGroups(inherited Groups);
end;

function TdxEMFDataController.GetGroupsClass: TcxDataControllerGroupsClass;
begin
  Result := TdxEMFDataControllerGroups;
end;

function TdxEMFDataController.GetHasChanges: Boolean;
begin
  Result := ((FilterChangedFlag or GroupingChangedFlag or
    SortingBySummaryChangedFlag or SortingChangedFlag or StructureChanged or
    DataChangedFlag or SummaryChangedFlag) and not Provider.FInInserting) or
    not Active;
end;

procedure TdxEMFDataController.Assign(Source: TPersistent);
var
  ASource: TdxEMFDataController absolute Source;
begin
  inherited Assign(Source);
  if Source is TdxEMFDataController then
  begin
    DataSource := ASource.DataSource;
    CollectionName := ASource.CollectionName;
    DetailKeyFieldNames := ASource.DetailKeyFieldNames;
    MasterKeyFieldNames := ASource.MasterKeyFieldNames;
  end;
end;

procedure TdxEMFDataController.Cancel;
begin
  LockStateInfo(False);
  try
    inherited Cancel;
    CheckFocusedSelected;
  finally
    UnlockStateInfo(False);
  end;
end;

function TdxEMFDataController.CanFocusRecord(ARecordIndex: Integer): Boolean;
begin
  if (Provider.IsInserting and (ARecordIndex = Provider.EditingRecordIndex)) or
    (UseNewItemRowForEditing and (ARecordIndex < 0)) then
  begin
    if not(Provider.IsInserting or Provider.FInInserting) and (ARecordIndex < 0)
    then
      Post;
    Exit(True);
  end;
  if Provider.IsModified then
    Post
  else if Provider.IsInserting then
    Cancel;
  Result := inherited CanFocusRecord(ARecordIndex);
end;

function TdxEMFDataController.CanIgnoreTimeForFiltering
  (ADataField: TdxEMFDataField): Boolean;
begin
  Result := False;
end;

function TdxEMFDataController.CanLoadData: Boolean;
var
  AMasterRelation: TdxEMFDataRelation;
begin
  AMasterRelation := MasterRelation;
  Result := inherited CanLoadData and
    (((DetailMode <> dcdmClone) or (AMasterRelation <> nil) and
    AMasterRelation.IsLinked and (AMasterRelation.MasterKeyFieldCount = Length
    (MasterDetailKeyFields))));
end;

procedure TdxEMFDataController.ChangeFieldName(AItemIndex: Integer;
const AFieldName: string);
begin
  CheckItemRange(AItemIndex);
  if GetItemFieldName(AItemIndex) <> AFieldName then
    UpdateField(Fields[AItemIndex], AFieldName, False);
end;

function TdxEMFDataController.ChangeFocusedRowIndex(ARowIndex: Integer)
  : Boolean;
begin
  if (FocusedRecordIndex <> -1) and (ARowIndex = -1) then
    FPrevFocusedRecordIndex := FocusedRecordIndex;
  Result := inherited ChangeFocusedRowIndex(ARowIndex);
end;

function TdxEMFDataController.GetInMemoryMode: Boolean;
begin
  Result := CollectionName <> '';
end;

function TdxEMFDataController.GetInternalDisplayText(ARecordIndex: Integer;
AField: TcxCustomDataField): string;
var
  AEMFField: TdxEMFDataField absolute AField;
begin
  Provider.Freeze;
  try
    if IsDataField(AField) and (AEMFField.ValueDef <> nil) then
      try
        if AEMFField.MemberInfo <> nil then
          Result := AEMFField.MemberInfo.GetValueAsDisplayText
            (Provider.GetRowValueAsVariant(ARecordIndex, AEMFField))
        else
          Result := VarToStr(GetInternalValue(ARecordIndex, AField));
      except
        on EVariantError do
          Result := '';
      end
    else
      Result := '';
  finally
    Provider.Unfreeze;
  end;
end;

function TdxEMFDataController.GetInternalValue(ARecordIndex: Integer;
AField: TcxCustomDataField): Variant;
var
  AEMFField: TdxEMFDataField absolute AField;
begin
  Provider.Freeze;
  try
    if InMemoryMode and IsDataField(AField) and (ARecordIndex >= 0) then
    begin
      if AEMFField.MemberInfo = nil then
        Result := inherited GetInternalValue(ARecordIndex, AField)
      else
        Result := Provider.GetRowValueAsVariant(ARecordIndex, AEMFField);
    end
    else if not InMemoryMode and IsDataField(AField) and
      (AEMFField.ValueDef <> nil) then
    begin
      if AEMFField.MemberInfo = nil then
        Result := inherited GetInternalValue(ARecordIndex, AField)
      else
      begin
        CheckRecordRange(ARecordIndex);
        Result := Provider.GetRowValueAsVariant(ARecordIndex, AEMFField);
      end;
    end
    else
      Result := GetStoredValue(ARecordIndex, AField);
  finally
    Provider.Unfreeze;
  end;
end;

function TdxEMFDataController.GetRecordCount: Integer;
begin
  if (Provider.IsActive or DataControllerInfo.DataGroups.HasTopGroupInfo) and
    not IsPattern then
  begin
    if InMemoryMode and IsDetailMode and (FMasterEntity = nil) then
      Result := 0
    else
      Result := DataControllerInfo.DataGroups.RecordCount;
    if Provider.IsInserting and (Provider.EditingRecordIndex >= 0) then
      Inc(Result);
  end
  else
    Result := 0;
end;

function TdxEMFDataController.GetRecordIndex: Integer;
begin
  Result := Provider.GetRecordIndex;
  if Result = -1 then
    Result := DataControllerInfo.FocusedRecordIndex;
end;

function TdxEMFDataController.GetRelationClass: TcxCustomDataRelationClass;
begin
  Result := TdxEMFDataRelation;
end;

function TdxEMFDataController.GetSortByExpressions: IdxSortByExpressions;

  procedure Populate(ASortByExpressions: TdxSortByExpressions;
  AFieldList: TcxSortingFieldList; AIsGroupng: Boolean);
  var
    I: Integer;
    ADataSortFieldInfo: TcxDataSortFieldInfo;
    AEMFDataField: TdxEMFDataField;
  begin
    for I := 0 to AFieldList.Count - 1 do
    begin
      ADataSortFieldInfo := AFieldList[I];
      AEMFDataField := ADataSortFieldInfo.Field as TdxEMFDataField;
      ASortByExpressions.Add(AEMFDataField.GetCriteria(AIsGroupng),
        SortOrderToSortDirection(ADataSortFieldInfo.SortOrder));
    end;
  end;

var
  ASortByExpressions: TdxSortByExpressions;
begin
  if (DataControllerInfo.SortingFieldList.Count = 0) and
    (DataControllerInfo.GroupingFieldList.Count = 0) then
    Exit(nil);
  ASortByExpressions := TdxSortByExpressions.Create;
  Populate(ASortByExpressions, DataControllerInfo.GroupingFieldList, True);
  Populate(ASortByExpressions, DataControllerInfo.SortingFieldList, False);
  Result := ASortByExpressions;
end;

function TdxEMFDataController.GetSummaryChangedFlag: Boolean;
begin
  Result := FSummaryChanged;
end;

function TdxEMFDataController.GetSummaryClass: TcxDataSummaryClass;
begin
  Result := TdxEMFDataSummary;
end;

function TdxEMFDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TdxEMFSummaryItem;
end;

function TdxEMFDataController.GetMasterDetailKeyFieldNames: string;
begin
  if MasterKeyFieldNames <> '' then
    Result := DetailKeyFieldNames
  else
    Result := '';
end;

function TdxEMFDataController.GetMasterDetailKeyFields
  : TArray<TdxMappingMemberInfo>;
begin
  if FMasterDetailKeyFields = nil then
    FMasterDetailKeyFields := Provider.GetFieldList(MasterDetailKeyFieldNames);
  Result := FMasterDetailKeyFields;
end;

function TdxEMFDataController.GetMemberInfo(AField: TdxEMFDataField)
  : TdxMappingMemberInfo;
begin
  Result := AField.MemberInfo;
end;

function TdxEMFDataController.InsertRecord(ARecordIndex: Integer): Integer;
begin
  Result := ARecordIndex;
end;

function TdxEMFDataController.IsConversionNeededForCompare
  (AField: TcxCustomDataField): Boolean;
begin
  Result := False;
end;

class function TdxEMFDataController.IsDetailDataControllerExist
  (ADetailObject: TcxDetailObject; ARelationIndex: Integer): Boolean;
begin
  Result := (ADetailObject <> nil) and
    (((ARelationIndex = -1) and not ADetailObject.IsEmpty) or
    ((ARelationIndex <> -1) and (ADetailObject.LinkObjects[ARelationIndex]
    <> nil)));
end;

function TdxEMFDataController.IsMergedGroupsSupported: Boolean;
begin
  Result := True;
end;

procedure TdxEMFDataController.InitializeDateTimeGrouping
  (ADataField: TdxEMFDataField);
begin
end;

function TdxEMFDataController.IsProviderDataSource: Boolean;
begin
  Result := True;
end;

function TdxEMFDataController.GetIsProviderMode: Boolean;
begin
  Result := True;
end;

function TdxEMFDataController.IsSmartLoad: Boolean;
begin
  Result := not InMemoryMode;
end;

procedure TdxEMFDataController.LoadStorage;
begin
  if InMemoryMode then
    DataControllerInfo.DataGroups.ClearData;
  if not Provider.IsActive and HasRelations then
    Relations.ClearDetailLinkObjects;
  inherited LoadStorage;
end;

function TdxEMFDataController.NeedPrepareGroupValue(AEMFDataField
  : TdxEMFDataField): Boolean;
begin
  Result := False;
end;

procedure TdxEMFDataController.PopulateFilterValues
  (AList: TcxDataFilterValueList; AItemIndex: Integer;
ACriteria: TcxFilterCriteria; var AUseFilteredRecords: Boolean;
out ANullExists: Boolean; AUniqueOnly, AFilteredRecordsShowFilteredItemsOnly
  : Boolean);
begin
  if (DataSource <> nil) and (Fields[AItemIndex].MemberInfo <> nil) and
    DataSource.Active and not InMemoryMode then
    DataControllerInfo.PopulateFilterValues(AList,
      Fields[AItemIndex] as TdxEMFDataField, ACriteria, AUseFilteredRecords,
      ANullExists)
  else
    inherited PopulateFilterValues(AList, AItemIndex, ACriteria,
      AUseFilteredRecords, ANullExists, AUniqueOnly,
      AFilteredRecordsShowFilteredItemsOnly);
end;

function TdxEMFDataController.PostEditingEntity(ARecordIndex: Integer;
AEditingRow: TdxEMFEditingRow): TObject;
var
  AObject, AKey: TObject;
  AKeyClass: TClass;
  I: Integer;
  AMasterMemberInfo: TdxMappingMemberInfo;
  AOriginalRowValue: TdxEMFEditingRow;
  AItem: TPair<TdxMappingMemberInfo, Variant>;
  ADataGroup: TdxEMFDataGroup;
begin
  if ARecordIndex < RecordCount then
  begin
    AObject := DataControllerInfo.GetEntityByRecordIndex(ARecordIndex);
    if AObject = nil then
      AOriginalRowValue := nil
    else
    begin
      AOriginalRowValue := TdxEMFEditingRow.Create(AEditingRow);
      for AItem in AOriginalRowValue do
        AOriginalRowValue[AItem.Key] := AItem.Key.GetAsVariant(AObject);
    end;
  end
  else
  begin
    AObject := nil;
    AOriginalRowValue := nil;
  end;
  Result := DataSource.PostEditingEntity(AObject, AEditingRow);
  for I := 0 to Length(FMasterDetailKeyFields) - 1 do
  begin
    AMasterMemberInfo := FMasterDetailKeyFields[I];
    if AMasterMemberInfo.IsReference then
    begin
      AKeyClass := AMasterMemberInfo.ReferenceType.ClassAttributes.
        PersistentClass;
      AKey := TdxEMFCustomSessionAccess(Session).GetLoadedObjectByKey(AKeyClass,
        FMasterDetailKeyValues[I]);
      if AKey = nil then
        AKey := Session.Find(AKeyClass, FMasterDetailKeyValues[I]);
      AMasterMemberInfo.SetValue(Result, AKey)
    end
    else
      AMasterMemberInfo.SetValue(Result, FMasterDetailKeyValues[I]);
  end;
  try
    try
      if Provider.IsInserting then
      begin
        if ARecordIndex = -1 then
          ARecordIndex := FPrevFocusedRecordIndex;
        ADataGroup := DataControllerInfo.DataGroups.
          GetDataGroupInfoByRecordIndex(ARecordIndex).DataGroup;
        if ADataGroup = nil then
          Session.Save(Result)
        else
          ADataGroup.Collection.Add(Result);
      end
      else
        Session.Save(Result);
    except
      if AOriginalRowValue <> nil then
        for AItem in AOriginalRowValue do
          AItem.Key.SetAsVariant(AObject, AItem.Value);
      raise;
    end;
  finally
    AOriginalRowValue.Free;
  end;
end;

procedure TdxEMFDataController.PrepareField(AField: TcxCustomDataField);
var
  ADataField: TdxEMFDataField;
begin
  inherited PrepareField(AField);
  if (DataSource <> nil) and not DataSource.IsDestroying and
    DataSource.Active and (AField.ReferenceField = nil) and
    (DataSource.EntityInfo <> nil) then
  begin
    ADataField := AField as TdxEMFDataField;
    ADataField.MemberInfo := DataSource.EntityInfo.FindMember
      (ADataField.FieldName);
    if ADataField.MemberInfo <> nil then
    begin
      ADataField.SetPropertiesByMember(ADataField.MemberInfo,
        ADataField.ValueTypeClass <> GetValueTypeClassByMember
        (ADataField.MemberInfo));
    end;
  end;
end;

function TdxEMFDataController.GetItemByFieldName(const AFieldName
  : string): TObject;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    if AnsiCompareText(GetItemFieldName(I), AFieldName) = 0 then
      Exit(GetItem(I));
  Result := nil;
end;

class function TdxEMFDataController.GetValueTypeClassByMember
  (AMember: TdxMappingMemberInfo): TcxValueTypeClass;
begin
  case AMember.FieldType of
    ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftWideMemo,
      ftFixedWideChar:
      Result := TcxStringValueType;
    ftSmallint:
      Result := TcxSmallintValueType;
    ftInteger, ftAutoInc:
      Result := TcxIntegerValueType;
    ftWord:
      Result := TcxWordValueType;
    ftBoolean:
      Result := TcxBooleanValueType;
    ftSingle:
      Result := TcxSingleValueType;
    ftCurrency, ftFloat:
      Result := TcxFloatValueType;
    { ftCurrency, } ftBCD:
      Result := TcxCurrencyValueType;
    ftDate, ftTime, ftDateTime:
      Result := TcxDateTimeValueType;
    ftFMTBcd:
      Result := TcxFMTBcdValueType;
    ftLargeint:
      Result := TcxLargeIntValueType;
    ftTimeStamp:
      Result := TcxSQLTimeStampValueType;
    ftBlob, ftGraphic, ftOraBlob, ftOraClob:
      Result := TcxBLOBValueType;
  else
    begin
      if AMember.IsReference then
        Result := TcxEMFEntityValueType
      else
        Result := TcxVariantValueType;
    end;
  end;
end;

procedure TdxEMFDataController.ResetEntityInfo;
var
  I: Integer;
begin
  for I := 0 to FieldsCount - 1 do
    Fields[I].FMember := nil;
end;

procedure TdxEMFDataController.ResetHasChildrenFlag;
var
  ADetailObject: TcxDetailObject;
  AChanged: Boolean;
begin
  AChanged := False;
  for ADetailObject in Relations.DetailObjects.Values do
    if ADetailObject.ClearHasChildrenFlag then
      AChanged := True;
  if AChanged then
    DataControllerInfo.RefreshView;
end;

procedure TdxEMFDataController.SetCollectionName(const Value: string);
begin
  if FCollectionName = Value then
    Exit;
  FCollectionName := Value;
  if MasterRelation <> nil then
    MasterRelation.UpdateMasterDetailKeyFieldNames;
end;

procedure TdxEMFDataController.SetDataSource(const Value
  : TdxEMFDataControllerCustomDataSource);
begin
  if Value = FDataSource then
    Exit;
  if FDataSource <> nil then
  begin
    FreeNotificator.RemoveSender(FDataSource);
    FreeNotificator.Check(FFreeNotificator);
    FDataSource.UnregisterController(Self);
  end;
  FDataSource := Value;
  if FDataSource <> nil then
  begin
    FreeNotificator.AddSender(FDataSource);
    DataSource.RegisterController(Self);
  end;
  DataSourceChanged;
end;

procedure TdxEMFDataController.SetDetailKeyFieldNames(const Value: string);
begin
  if FDetailKeyFieldNames = Value then
    Exit;
  FDetailKeyFieldNames := Value;
  if MasterRelation <> nil then
    MasterRelation.UpdateMasterDetailKeyFieldNames;
end;

procedure TdxEMFDataController.SetEMFMasterRelation
  (const Value: TdxEMFDataRelation);
begin
  inherited MasterRelation := Value;
end;

procedure TdxEMFDataController.SetMasterKeyFieldNames(const Value: string);
begin
  if FMasterKeyFieldNames = Value then
    Exit;
  FMasterKeyFieldNames := Value;
  if MasterRelation <> nil then
    MasterRelation.UpdateMasterDetailKeyFieldNames;
end;

procedure TdxEMFDataController.SetMasterRelation(AMasterRelation
  : TcxCustomDataRelation; AMasterRecordIndex: Integer);
var
  AEMFMasterRelation: TdxEMFDataRelation absolute AMasterRelation;
begin
  BeginUpdate;
  try
    inherited SetMasterRelation(AMasterRelation, AMasterRecordIndex);
    if (AMasterRelation <> nil) and (AMasterRecordIndex <> -1) and
      (AMasterRelation is TdxEMFDataRelation) then
    begin
      FMasterEntity := AEMFMasterRelation.DataController.Provider.GetObject
        (AMasterRecordIndex);
      if not InMemoryMode then
      begin
        if AEMFMasterRelation.FMasterKeyField = nil then
          UpdateRelationFields;
        FMasterDetailKeyValues := AEMFMasterRelation.GetMasterRecordValue
          (AMasterRecordIndex);
      end;
      LayoutChanged([lcData]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxEMFDataController.UpdateField(ADataField: TdxEMFDataField;
const AFieldNames: string; AIsLookup: Boolean);

  function FieldExists(const AFieldName: string): Boolean;
  var
    I: Integer;
    ACurrentField: TdxEMFDataField;
    APrevValueTypeClass: TcxValueTypeClass;
  begin
    APrevValueTypeClass := ADataField.ValueTypeClass;
    ADataField.ReferenceField := nil;
    Result := False;
    if ADataField.FieldName = '' then
      ADataField.ValueTypeClass := nil
    else
    begin
      for I := 0 to FieldsCount - 1 do
      begin
        ACurrentField := Fields[I];
        if ACurrentField <> ADataField then
        begin
          if AnsiSameText(ACurrentField.FieldName, ADataField.FieldName) then
          begin
            ADataField.ReferenceField := ACurrentField;
            Result := True;
            Break;
          end;
        end;
      end;
    end;
    if (ADataField.ValueTypeClass <> APrevValueTypeClass) and not ADataField.IsInternal
    then
      DoValueTypeClassChanged(ADataField.Index);
  end;

var
  APos: Integer;
  ASubDataField: TdxEMFDataField;
  AFields: TdxArray<TdxEMFDataField>;
begin
  ADataField.ClearFields;
  if not AIsLookup then
  begin
    if ADataField.FieldName <> AFieldNames then
      inherited Fields.ReassignFields(ADataField);
    ADataField.FieldName := AFieldNames;
  end;
  if { AIsLookup or } IsMultipleFieldNames(AFieldNames) then
  begin
    BeginUpdate;
    try
      ADataField.ReferenceField := nil;
      APos := 1;
      AFields := TdxArray<TdxEMFDataField>.Create(4);
      while APos <= Length(AFieldNames) do
      begin
        ASubDataField := nil;
        UpdateInternalField(ExtractFieldName(AFieldNames, APos), ASubDataField);
        AFields.Add(ASubDataField);
      end;
      ADataField.Fields := AFields.ToArray;
    finally
      EndUpdate;
    end;
  end
  else if FieldExists(AFieldNames) then
    Change([dccData])
  else
    LayoutChanged([lcStructure]);
end;

procedure TdxEMFDataController.UpdateInternalField(const AFieldName: string;
var AField: TdxEMFDataField);
begin
  if AFieldName = '' then
  begin
    AField.Free;
    AField := nil;
    Change([dccData]);
  end
  else
  begin
    if AField = nil then
      AField := AddInternalField as TdxEMFDataField;
    UpdateField(AField, AFieldName, False);
    PrepareField(AField);
  end;
end;

procedure TdxEMFDataController.UpdateRelationFields;
var
  I: Integer;
  ADataRelation: TcxCustomDataRelation;
  AEMFDataRelation: TdxEMFDataRelation absolute ADataRelation;
  AMasterKeyFieldNames: string;
  AMasterKeyField: TdxEMFDataField;
begin
  for I := 0 to Relations.Count - 1 do
  begin
    ADataRelation := Relations[I];
    if ADataRelation is TdxEMFDataRelation then
    begin
      AMasterKeyFieldNames := AEMFDataRelation.MasterKeyFieldNames;
      AMasterKeyField := nil;
      UpdateInternalField(AMasterKeyFieldNames, AMasterKeyField);
      AEMFDataRelation.SetMasterKeyField(AMasterKeyField);
    end;
  end;
end;

procedure TdxEMFDataController.UpdateRelations
  (ARelation: TcxCustomDataRelation);
begin
  inherited UpdateRelations(ARelation);
  UpdateRelationFields;
end;

{ TdxEMFDataControllerGroups }

function TdxEMFDataControllerGroups.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFDataControllerGroups.GetDataGroups: TdxEMFDataGroups;
begin
  Result := TdxEMFDataGroups(inherited DataGroups);
end;

function TdxEMFDataControllerGroups.GetCriteria(ARowIndex: Integer)
  : IdxCriteriaOperator;
var
  AIndex: Integer;
begin
  AIndex := DataGroupIndexByRowIndex[ARowIndex];
  Result := DataGroups[AIndex].DataGroup.GetGroupCriteria;
end;

function TdxEMFDataControllerGroups.GetGroupDisplayText(ADataGroupIndex
  : TcxDataGroupIndex): string;
begin
  if DataController.InMemoryMode then
    Result := inherited
  else
    Result := DataGroups[ADataGroupIndex].DisplayText;
end;

function TdxEMFDataControllerGroups.GetGroupValue(ADataGroupIndex
  : TcxDataGroupIndex): Variant;
begin
  if DataController.InMemoryMode then
    Result := inherited
  else
    Result := DataGroups[ADataGroupIndex].GroupValue;
end;

{ TdxEMFCriteria }

function TdxEMFCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TdxEMFCriteriaItem;
end;

{ TdxEMFCriteriaItem }

function TdxEMFCriteriaItem.GetEMFCriteria: TdxEMFCriteria;
begin
  Result := TdxEMFCriteria(inherited Criteria);
end;

function TdxEMFCriteriaItem.GetFieldCaption: string;
begin
  Result := '';
end;

function TdxEMFCriteriaItem.GetFieldName: string;
begin
  Result := GetFieldCaption;
end;

{ TdxEMFDataFilterCriteriaItem }

function TdxEMFDataFilterCriteriaItem.GetField: TdxEMFDataField;
begin
  Result := TdxEMFDataField(inherited Field);
end;

function TdxEMFDataFilterCriteriaItem.CreateBinaryOperator
  (AType: TdxBinaryOperatorType): IdxCriteriaOperator;
var
  AValue: TValue;
begin
  if Field.MemberInfo = nil then
    AValue := TValue.FromVariant(Value)
  else if Field.MemberInfo.IsReference then
    AValue := TObject(NativeInt(Value))
  else
    AValue := TdxFieldValueConverter.ToValue(Value, Field.MemberInfo);
  Result := TdxBinaryOperator.Create(Field.GetCriteria,
    TdxOperandValue.Create(AValue), AType);
end;

function TdxEMFDataFilterCriteriaItem.CreateFunctionOperator
  (AType: TdxFunctionOperatorType): IdxCriteriaOperator;
begin
  case AType of
    TdxFunctionOperatorType.Log, TdxFunctionOperatorType.BigMul,
      TdxFunctionOperatorType.Round, TdxFunctionOperatorType.StartsWith,
      TdxFunctionOperatorType.EndsWith, TdxFunctionOperatorType.Contains,
      TdxFunctionOperatorType.AddTimeSpan,
      TdxFunctionOperatorType.IncTick .. TdxFunctionOperatorType.IncYear:
      Result := TdxFunctionOperator.Create(AType,
        [TdxOperandProperty.Create(Field.FieldName),
        TdxOperandValue.Create(TValue.FromVariant(Value))]);
    TdxFunctionOperatorType.LocalDateTimeThisYear .. TdxFunctionOperatorType.
      LocalDateTimeYearBeforeToday:
      Result := TdxFunctionOperator.Create(AType, []);
    TdxFunctionOperatorType.PadLeft, TdxFunctionOperatorType.PadRight,
      TdxFunctionOperatorType.Remove, TdxFunctionOperatorType.Replace,
      TdxFunctionOperatorType.Substring,
      TdxFunctionOperatorType.DateDiffTicks .. TdxFunctionOperatorType.
      YearsBetween:
      Result := TdxFunctionOperator.Create(AType,
        [TdxOperandProperty.Create(Field.FieldName),
        TdxOperandValue.Create(TValue.FromVariant(Value[0])),
        TdxOperandValue.Create(TValue.FromVariant(Value[1]))]);
  else
    Result := TdxFunctionOperator.Create(AType,
      [TdxOperandProperty.Create(Field.FieldName)]);
  end;
end;

function TdxEMFDataFilterCriteriaItem.CreateOperands
  : TArray<IdxCriteriaOperator>;
var
  I, ACount: Integer;
begin
  if not VarIsArray(Value) then
    raise EInvalidArgument.Create('');
  ACount := VarArrayHighBound(Value, 1) + 1;
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
    Result[I] := TdxOperandValue.Create(TValue.FromVariant(Value[I]));
end;

class function TdxEMFDataFilterCriteriaItem.CreateNotOperator
  (const ACriteriaOperator: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.&Not,
    ACriteriaOperator);
end;

function TdxEMFDataFilterCriteriaItem.GetCriteriaOperator: IdxCriteriaOperator;

  function MakeTypicalOutlookInterval(const AOp, ALowerBound,
    AUpperBound: IdxCriteriaOperator): IdxCriteriaOperator;
  begin
    Result := TdxGroupOperator.Create(TdxGroupOperatorType.&And,
      [TdxBinaryOperator.Create(AOp, ALowerBound,
      TdxBinaryOperatorType.GreaterOrEqual), TdxBinaryOperator.Create(AOp,
      AUpperBound, TdxBinaryOperatorType.Less)]);
  end;

  function MakeNextDays(ADays: Integer): IdxCriteriaOperator;
  begin
    Result := MakeTypicalOutlookInterval
      (TdxOperandProperty.Create(Field.FieldName),
      CreateFunctionOperator(TdxFunctionOperatorType.LocalDateTimeToday),
      TdxOperandValue.Create(IncDay(DateUtils.Today, ADays)));
  end;

  function MakeLastDays(ADays: Integer): IdxCriteriaOperator;
  begin
    Result := MakeTypicalOutlookInterval
      (TdxOperandProperty.Create(Field.FieldName),
      TdxOperandValue.Create(IncDay(DateUtils.Tomorrow, -ADays)),
      CreateFunctionOperator(TdxFunctionOperatorType.LocalDateTimeTomorrow));
  end;

var
  ADate: TDate;
begin
  case OperatorKind of
    foEqual:
      if VarIsNull(Value) then
        Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNull)
      else
        Result := CreateBinaryOperator(TdxBinaryOperatorType.Equal);
    foNotEqual:
      if VarIsNull(Value) then
        Result := CreateNotOperator
          (CreateFunctionOperator(TdxFunctionOperatorType.IsNull))
      else
        Result := CreateBinaryOperator(TdxBinaryOperatorType.NotEqual);
    foLess:
      Result := CreateBinaryOperator(TdxBinaryOperatorType.Less);
    foLessEqual:
      Result := CreateBinaryOperator(TdxBinaryOperatorType.LessOrEqual);
    foGreater:
      Result := CreateBinaryOperator(TdxBinaryOperatorType.Greater);
    foGreaterEqual:
      Result := CreateBinaryOperator(TdxBinaryOperatorType.GreaterOrEqual);
    foLike:
      if VarIsNull(Value) then
        Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNull)
      else
        Result := CreateFunctionOperator(TdxFunctionOperatorType.Contains);
    foNotLike:
      if VarIsNull(Value) then
        Result := CreateNotOperator
          (CreateFunctionOperator(TdxFunctionOperatorType.IsNull))
      else
        Result := CreateNotOperator
          (CreateFunctionOperator(TdxFunctionOperatorType.Contains));
    foBetween:
      Result := TdxBetweenOperator.Create(Field.FieldName,
        TValue.FromVariant(Value[0]), TValue.FromVariant(Value[1]));
    foNotBetween:
      Result := CreateNotOperator(TdxBetweenOperator.Create(Field.FieldName,
        TValue.FromVariant(Value[0]), TValue.FromVariant(Value[1])));
    foInList:
      Result := TdxInOperator.Create(TdxOperandProperty.Create(Field.FieldName),
        CreateOperands);
    foNotInList:
      Result := CreateNotOperator
        (TdxInOperator.Create(TdxOperandProperty.Create(Field.FieldName),
        CreateOperands));
    foYesterday:
      Result := CreateFunctionOperator
        (TdxFunctionOperatorType.IsOutlookIntervalYesterday);
    foToday:
      Result := CreateFunctionOperator
        (TdxFunctionOperatorType.IsOutlookIntervalToday);
    foTomorrow:
      Result := CreateFunctionOperator
        (TdxFunctionOperatorType.IsOutlookIntervalTomorrow);
    foLast7Days:
      Result := MakeLastDays(7);
    foLastWeek:
      Result := CreateFunctionOperator
        (TdxFunctionOperatorType.IsOutlookIntervalLastWeek);
    foLast14Days:
      Result := MakeLastDays(14);
    foLastTwoWeeks:
      begin
        ADate := StartOfTheWeek(DateUtils.Today);
        Result := MakeTypicalOutlookInterval
          (TdxOperandProperty.Create(Field.FieldName),
          TdxOperandValue.Create(IncDay(ADate, -14)),
          TdxOperandValue.Create(ADate));
      end;
    foLast30Days:
      Result := MakeLastDays(30);
    foLastMonth:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsLastMonth);
    foLastYear:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsLastYear);
    foInPast:
      Result := TdxBinaryOperator.Create
        (TdxOperandProperty.Create(Field.FieldName),
        CreateFunctionOperator(TdxFunctionOperatorType.LocalDateTimeToday),
        TdxBinaryOperatorType.LessOrEqual);
    foThisWeek:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsThisWeek);
    foThisMonth:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsThisMonth);
    foThisYear:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsThisYear);
    foNext7Days:
      Result := MakeNextDays(7);
    foNextWeek:
      Result := CreateFunctionOperator
        (TdxFunctionOperatorType.IsOutlookIntervalNextWeek);
    foNext14Days:
      Result := MakeNextDays(14);
    foNextTwoWeeks:
      begin
        ADate := IncDay(StartOfTheWeek(DateUtils.Today), 7);
        Result := MakeTypicalOutlookInterval
          (TdxOperandProperty.Create(Field.FieldName),
          TdxOperandValue.Create(ADate),
          TdxOperandValue.Create(IncDay(ADate, 14)));
      end;
    foNext30Days:
      Result := MakeNextDays(30);
    foNextMonth:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNextMonth);
    foNextYear:
      Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNextYear);
    foInFuture:
      Result := TdxBinaryOperator.Create
        (TdxOperandProperty.Create(Field.FieldName),
        CreateFunctionOperator(TdxFunctionOperatorType.LocalDateTimeTomorrow),
        TdxBinaryOperatorType.GreaterOrEqual);
    foContains:
      if VarIsNull(Value) then
        Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNull)
      else
        Result := CreateFunctionOperator(TdxFunctionOperatorType.Contains);
    foNotContains:
      if VarIsNull(Value) then
        Result := CreateNotOperator
          (CreateFunctionOperator(TdxFunctionOperatorType.IsNull))
      else
        Result := CreateNotOperator
          (CreateFunctionOperator(TdxFunctionOperatorType.Contains));
    foBeginsWith:
      if VarIsNull(Value) then
        Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNull)
      else
        Result := CreateFunctionOperator(TdxFunctionOperatorType.StartsWith);
    foEndsWith:
      if VarIsNull(Value) then
        Result := CreateFunctionOperator(TdxFunctionOperatorType.IsNull)
      else
        Result := CreateFunctionOperator(TdxFunctionOperatorType.EndsWith);
  else
    NotImplemented;
  end;
end;

function TdxEMFDataFilterCriteriaItem.GetFieldCaption: string;
begin
  Result := inherited GetFieldCaption;
  if Result = '' then
    Result := GetFieldName;
end;

function TdxEMFDataFilterCriteriaItem.GetFieldName: string;
begin
  Result := Field.FieldName;
end;

function TdxEMFDataFilterCriteriaItem.GetFilterOperatorClass
  : TcxFilterOperatorClass;
begin
  Result := inherited GetFilterOperatorClass;
end;

{ TdxEMFDataFilterCriteria }

function TdxEMFDataFilterCriteria.GetCriteriaOperator: IdxCriteriaOperator;

  function FilterToCriteriaOperator(ABranch: TcxFilterCriteriaItemList)
    : IdxCriteriaOperator;
  var
    I: Integer;
    AItem: TcxCustomFilterCriteriaItem;
    ACriteriaItem: TdxEMFDataFilterCriteriaItem absolute AItem;
    AOperatorType: TdxGroupOperatorType;
    ACriteriaOperator: IdxCriteriaOperator;
  begin
    Result := nil;
    if ABranch.BoolOperatorKind in [fboAnd, fboNotAnd] then
      AOperatorType := TdxGroupOperatorType.&And
    else
      AOperatorType := TdxGroupOperatorType.&Or;
    for I := 0 to ABranch.Count - 1 do
    begin
      AItem := ABranch[I];
      if AItem.IsItemList then
        ACriteriaOperator := FilterToCriteriaOperator
          ((TcxFilterCriteriaItemList(AItem)))
      else
        ACriteriaOperator := ACriteriaItem.GetCriteriaOperator;
      Result := TdxGroupOperator.Combine(AOperatorType, Result,
        ACriteriaOperator);
    end;

  end;

begin
  Result := FilterToCriteriaOperator(Root);
end;

function TdxEMFDataFilterCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TdxEMFDataFilterCriteriaItem;
end;

{ TdxEMFDataFindCriteria }

function TdxEMFDataFindCriteria.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

function TdxEMFDataFindCriteria.GetEscapeWildcard: Char;
begin
  Result := #0;
end;

function TdxEMFDataFindCriteria.IsBehaviorSupported(const AValue
  : TcxDataFindCriteriaBehavior): Boolean;
begin
  Result := AValue <> fcbSearch;
end;

function TdxEMFDataFindCriteria.IsConditionsLowerCase: Boolean;
begin
  Result := False;
end;

function TdxEMFDataFindCriteria.UseMatches: Boolean;
begin
  Result := False;
end;

{ TdxEMFDataSummary }

function TdxEMFDataSummary.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

procedure TdxEMFDataSummary.CalculateGroupSummary;
begin
end;

procedure TdxEMFDataSummary.CalculateSummary(ASummaryItems: TcxDataSummaryItems;
ABeginIndex, AEndIndex: Integer; var ACountValues: TcxDataSummaryCountValues;
var ASummaryValues: TcxDataSummaryValues);
begin
  if (DataController.DataSource = nil) or
    not DataController.DataSource.Active or (ABeginIndex > AEndIndex) then
    Exit;
  if DataController.InMemoryMode then
    inherited
  else
    DataController.DataControllerInfo.CalculateSummary(ASummaryItems,
      ACountValues, ASummaryValues);
end;

{ TcxEMFDataRelationList }

constructor TcxEMFDataRelationList.Create(ADataController
  : TcxCustomDataController);
begin
  inherited Create(ADataController);
  FDetailObjects := TObjectDictionary<TObject, TcxDetailObject>.Create
    ([doOwnsValues]);
end;

destructor TcxEMFDataRelationList.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDetailObjects);
end;

function TcxEMFDataRelationList.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

procedure TcxEMFDataRelationList.ChangeDetailObjectKey(AOldKey,
  ANewKey: TObject);
var
  AItem: TPair<TObject, TcxDetailObject>;
begin
  AItem := FDetailObjects.ExtractPair(AOldKey);
  if AItem.Value <> nil then
    FDetailObjects.Add(ANewKey, AItem.Value);
end;

procedure TcxEMFDataRelationList.ClearDetailLinkObjects;
begin
  if FDetailObjects.Count > 0 then
  begin
    FDetailObjects.Clear;
    DataController.Change([dccDetail]);
  end;
end;

procedure TcxEMFDataRelationList.DeleteDetailObject(AEntity: TObject);
begin
  FDetailObjects.Remove(AEntity);
end;

function TcxEMFDataRelationList.GetDetailObject(ARecordIndex: Integer)
  : TcxDetailObject;

  procedure ResetMasterRelations(ADetailObject: TcxDetailObject);
  var
    I: Integer;
    ADetailLinkObject: TObject;
  begin
    for I := 0 to ADetailObject.LinkObjectCount - 1 do
    begin
      ADetailLinkObject := ADetailObject.LinkObjects[I];
      if ADetailLinkObject <> nil then
        TdxEMFDataController(DataController.GetDetailDataControllerByLinkObject
          (ADetailLinkObject)).MasterRelation := nil;
    end;
  end;

var
  ADetailObjectInfo: TPair<TObject, TcxDetailObject>;
begin
  DataController.CheckRecordRange(ARecordIndex);
  ADetailObjectInfo := GetDetailObjectInfo(ARecordIndex);
  Result := ADetailObjectInfo.Value;
  if ADetailObjectInfo.Key = nil then
    Exit;
  if IsEmpty then
  begin
    if Result <> nil then
    begin
      ResetMasterRelations(Result);
      FDetailObjects.Remove(ADetailObjectInfo.Key);
      Result := nil;
    end;
    Exit;
  end;
  if Result = nil then
  begin
    Result := TcxEMFDetailObject.Create;
    Result.ActiveRelationIndex := DataController.GetDefaultActiveRelationIndex;
    FDetailObjects.Add(ADetailObjectInfo.Key, Result);
  end;
  TcxEMFDetailObject(Result).CorrectCount(Self);
end;

function TcxEMFDataRelationList.GetDetailObjectInfo(ARecordIndex: Integer)
  : TPair<TObject, TcxDetailObject>;
begin
  Result.Key := DataController.DataControllerInfo.GetEntityByRecordIndex
    (ARecordIndex);
  FDetailObjects.TryGetValue(Result.Key, Result.Value);
end;

function TcxEMFDataRelationList.GetValueAsDetailObject(ARecordIndex: Integer)
  : TcxDetailObject;
var
  AEntity: TObject;
begin
  AEntity := DataController.DataControllerInfo.GetEntityByRecordIndex
    (ARecordIndex);
  FDetailObjects.TryGetValue(AEntity, Result);
end;

end.
