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

unit dxEMF.DataDefinitions;

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, Classes,
  Variants, DB, dxCore, dxCoreClasses, TypInfo, Rtti,
  dxEMF.Core,
  dxEMF.Types,
  dxEMF.Metadata,
  dxEMF.DB.Criteria;

type

  TdxCustomEMFQueryProperties = class;
  TdxCustomEMFQueryDataSource = class;
  TdxFieldExpression = class;
  TdxCustomEMFDataSource = class;

  { TdxEMFCustomDataContext }

  TdxEMFCustomDataContext = class(TComponent)
  protected type

    TPackageController = class
    private
      FFileName: string;
      FModule: HMODULE;
    public
      constructor Create(const AFileName: string);
      destructor Destroy; override;
      procedure Load;
      procedure UnLoad;

      property FileName: string read FFileName;
    end;

  strict private
    FDataSources: TList<TdxCustomEMFDataSource>;
    FPackageName: string;
    FPackageFileName: string;
    FPackageController: TPackageController;
    procedure CloseDataSources;
    function CreatePackageController: TPackageController;
    function GetIsDestroying: Boolean;
    function GetPackageController: TPackageController;
    procedure SetPackageName(const Value: string);
    procedure SetPackageFileName(const Value: string);
  protected
    class function GetDataContexts: TArray<PTypeInfo>;
    class function GetEntityClasses(const APackageName: string): TArray<TClass>;
      overload; static;
    function GetEntityClasses: TArray<TClass>; overload;
    function GetEntityInfo(const AClassName: string): TdxEntityInfo; overload;
    procedure RegisterDataSources(ADataSource: TdxCustomEMFDataSource);
    procedure UnregisterDataSources(ADataSource: TdxCustomEMFDataSource);

    property IsDestroying: Boolean read GetIsDestroying;
    property PackageController: TPackageController read GetPackageController;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PackageName: string read FPackageName write SetPackageName;
    property PackageFileName: string read FPackageFileName
      write SetPackageFileName;
  end;

  { TdxEMFDataContext }

  TdxEMFDataContext = class(TdxEMFCustomDataContext)
  published
    property PackageName;
    property PackageFileName;
  end;

  { TdxCriteriaOperatorHolder }

  TdxCriteriaOperatorHolder = record
  private
    FCriteriaOperatorText: string;
    FCriteriaOperator: IdxCriteriaOperator;
    FOperandValue: TArray<IdxOperandValue>;
    function GetCriteriaOperatorText: string;
    function GetHasParams: Boolean;
  public
    class operator Implicit(const A: TdxCriteriaOperatorHolder)
      : IdxCriteriaOperator;
    class operator Implicit(const A: TdxCriteriaOperatorHolder): string;
    class operator Implicit(const ACriteriaOperator: IdxCriteriaOperator)
      : TdxCriteriaOperatorHolder;
    class operator Implicit(const ACriteriaOperatorText: string)
      : TdxCriteriaOperatorHolder;
    class operator Equal(const A: TdxCriteriaOperatorHolder;
      const ACriteriaOperator: IdxCriteriaOperator): Boolean;
    class operator Equal(const A: TdxCriteriaOperatorHolder;
      const ACriteriaOperatorText: string): Boolean;

    property CriteriaOperatorText: string read GetCriteriaOperatorText;
    property CriteriaOperator: IdxCriteriaOperator read FCriteriaOperator;
    property OperandValues: TArray<IdxOperandValue> read FOperandValue;
    property HasParams: Boolean read GetHasParams;
  end;

  { TdxCustomEMFDataSource }

  TdxCustomEMFDataSource = class(TComponent)
  strict private
    FActive: Boolean;
    FDataContext: TdxEMFCustomDataContext;
    FEntityClass: TClass;
    FEntityInfo: TdxEntityInfo;
    FEntityName: string;
    FIsExternalData: Boolean;
    FPackageName: string;
    FSession: TdxEMFCustomSession;
    FStreamedActive: Boolean;
    function FindDefaultComponent<T: TComponent>(AOwner: TComponent): T;
    function GetEntityInfo: TdxEntityInfo; inline;
    function GetHasLoader: Boolean;
    function GetIsDesigning: Boolean; inline;
    function GetRecordCount: Integer;
    property IsDesigning: Boolean read GetIsDesigning;
    function IsSessionOpen: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetDataContext(const Value: TdxEMFCustomDataContext);
    procedure SetEntityClass(const Value: TClass);
    procedure SetEntityInfo(const Value: TdxEntityInfo);
    procedure SetEntityName(const Value: string);
    procedure SetPackageName(const Value: string);
    procedure SetSession(const Value: TdxEMFCustomSession);
  protected
    FDataCollection: IdxEMFCollection;
    procedure ActiveChanged(AActive: Boolean); virtual; abstract;
    procedure CheckEntityInfo;
    procedure ConnectionStateChange(ASender: TObject;
      AConnecting: Boolean); virtual;
    procedure CreateClientList; virtual;
    procedure DestroyClientList; virtual;
    procedure DoAssignEntity;
    procedure Reset;
    procedure InternalClose; virtual;
    procedure CreateCollection;
    procedure FetchAll;
    function MoveNext: Boolean;
    procedure NotifyClients; virtual;

    procedure Add(AObject: TObject);
    procedure AssignData(AObject: TObject); overload;
    procedure AssignData(const AEMFCollection: IdxEMFCollection); overload;
    procedure AssignTo(ADest: TPersistent); override;
    procedure Delete(AObject: TObject);
    procedure Insert(AIndex: Integer; AObject: TObject);
    function IndexOf(AObject: TObject): Integer;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    property HasLoader: Boolean read GetHasLoader;
    property IsExternalData: Boolean read FIsExternalData;
    property DataCollection: IdxEMFCollection read FDataCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    property RecordCount: Integer read GetRecordCount;

    property Active: Boolean read FActive write SetActive default False;
    property DataContext: TdxEMFCustomDataContext read FDataContext
      write SetDataContext;
    property EntityName: string read FEntityName write SetEntityName;
    property EntityInfo: TdxEntityInfo read GetEntityInfo write SetEntityInfo;
    property EntityClass: TClass read FEntityClass write SetEntityClass;
    property PackageName: string read FPackageName write SetPackageName;
    property Session: TdxEMFCustomSession read FSession write SetSession;
  end;

  { TdxExpressionDefinition }

  TdxExpressionDefinitionClass = class of TdxExpressionDefinition;

  TdxExpressionDefinition = class(TCollectionItem)
  strict private
    FExpression: TdxCriteriaOperatorHolder;
  private
    function GetExpression: IdxCriteriaOperator;
    function GetExpressionText: string;
    procedure SetExpression(const Value: IdxCriteriaOperator);
    procedure SetExpressionText(const Value: string);
    function GetOperandValues: TArray<IdxOperandValue>; inline;
    function GetEMFDataSource: TdxCustomEMFQueryDataSource;
  protected
    procedure DoAssign(ASource: TdxExpressionDefinition); virtual;
    function GetDisplayName: string; override;
    procedure Update; virtual;
    property EMFDataSource: TdxCustomEMFQueryDataSource read GetEMFDataSource;
  public
    procedure Assign(ASource: TPersistent); override;
    property Expression: IdxCriteriaOperator read GetExpression
      write SetExpression;
    property OperandValues: TArray<IdxOperandValue> read GetOperandValues;
  published
    property ExpressionText: string read GetExpressionText
      write SetExpressionText;
  end;

  { TdxExpressionDefinitions }

  TdxExpressionDefinitions = class(TOwnedCollection)
  strict private
    function GetEMFDataSource: TdxCustomEMFQueryDataSource; inline;
  protected
    class function GetExpressionDefinitionClass
      : TdxExpressionDefinitionClass; virtual;
  public
    constructor Create(AEMFDataSource: TdxCustomEMFQueryDataSource);
    property EMFDataSource: TdxCustomEMFQueryDataSource read GetEMFDataSource;
  end;

  { TdxFieldExpression }

  TdxFieldExpression = class(TdxExpressionDefinition)
  end;

  { TdxFieldExpressions }

  TdxFieldExpressions = class(TdxExpressionDefinitions)
  strict private
    function GetFieldExpression(Index: Integer): TdxFieldExpression;
    procedure SetFieldExpression(Index: Integer; Value: TdxFieldExpression);
    procedure RemoveItem(AFieldExpression: TdxFieldExpression); inline;
  protected
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
    class function GetExpressionDefinitionClass
      : TdxExpressionDefinitionClass; override;
  public
    function Add: TdxFieldExpression;
    property Items[Index: Integer]: TdxFieldExpression read GetFieldExpression
      write SetFieldExpression; default;
  end;

  { TdxGroupByExpressionDefinition }

  TdxGroupByExpressionDefinition = class(TdxExpressionDefinition)
  protected
    procedure Update; override;
  end;

  { TdxGroupByExpressionDefinitions }

  TdxGroupByExpressionDefinitions = class(TdxExpressionDefinitions)
  strict private
    function GetItem(Index: Integer): TdxGroupByExpressionDefinition;
    procedure SetItem(Index: Integer;
      const Value: TdxGroupByExpressionDefinition);
  protected
    class function GetExpressionDefinitionClass
      : TdxExpressionDefinitionClass; override;
  public
    function Add: TdxGroupByExpressionDefinition;
    property Items[Index: Integer]: TdxGroupByExpressionDefinition read GetItem
      write SetItem; default;
  end;

  { TdxSortByExpressionDefinition }

  TdxSortByExpressionDefinition = class(TdxExpressionDefinition)
  strict private
    FSortDirection: TdxSortDirection;
  protected
    function GetDisplayName: string; override;
    procedure DoAssign(ASource: TdxExpressionDefinition); override;
  published
    property SortDirection: TdxSortDirection read FSortDirection
      write FSortDirection default TdxSortDirection.Ascending;
  end;

  { TdxSortByExpressionDefinitions }

  TdxSortByExpressionDefinitions = class(TdxExpressionDefinitions)
  strict private
    function GetItem(Index: Integer): TdxSortByExpressionDefinition;
    function GetSortByExpressions: IdxSortByExpressions;
    procedure SetSortByExpressions(const Value: IdxSortByExpressions);
    procedure SetItem(Index: Integer;
      const Value: TdxSortByExpressionDefinition);
  protected
    class function GetExpressionDefinitionClass
      : TdxExpressionDefinitionClass; override;
    property SortByExpressions: IdxSortByExpressions read GetSortByExpressions
      write SetSortByExpressions;
  public
    function Add: TdxSortByExpressionDefinition;
    property Items[Index: Integer]: TdxSortByExpressionDefinition read GetItem
      write SetItem; default;
  end;

  { TdxEMFQueryProperties }

  TdxCustomEMFQueryProperties = class(TPersistent)
  private
    FEMFDataSource: TdxCustomEMFQueryDataSource;
    FCriteria: TdxCriteriaOperatorHolder;
    FFieldExpressions: TdxFieldExpressions;
    FGroupCriteria: TdxCriteriaOperatorHolder;
    FGroupByExpressionDefinitions: TdxGroupByExpressionDefinitions;
    FSortByExpressionDefinitions: TdxSortByExpressionDefinitions;
    FSkipSelectedRecords: Integer;
    FTopSelectedRecords: Integer;
    FParams: TParams;
    function GetCriteria: IdxCriteriaOperator;
    function GetCriteriaText: string;
    function GetGroupByExpressionDefinitions: TdxGroupByExpressionDefinitions;
    function GetGroupCriteria: IdxCriteriaOperator;
    function GetGroupCriteriaText: string;
    function GetSortByExpressionDefinitions: TdxSortByExpressionDefinitions;
    function GetSortByExpressions: IdxSortByExpressions;
    procedure SetCriteria(const Value: IdxCriteriaOperator);
    procedure SetCriteriaText(const Value: string);
    procedure SetFieldExpressions(const Value: TdxFieldExpressions);
    procedure SetGroupByExpressionDefinitions(const Value
      : TdxGroupByExpressionDefinitions);
    procedure SetGroupCriteria(const Value: IdxCriteriaOperator);
    procedure SetGroupCriteriaText(const Value: string);
    procedure SetParams(const Value: TParams);
    procedure SetSkipSelectedRecords(const Value: Integer);
    procedure SetSortByExpressionDefinitions(const Value
      : TdxSortByExpressionDefinitions);
    procedure SetSortByExpressions(const Value: IdxSortByExpressions);
    procedure SetTopSelectedRecords(const Value: Integer);
    function GetOwnerComponent: TComponent; inline;
  protected
    class procedure CollectParams(AList: TParams;
      const AExpression: string); static;
    procedure ApplyParams(const AOperandValues: TArray<IdxOperandValue>;
      AIndex: Integer = 0); overload;
    function IsFieldExpressionsStored: Boolean;
    function IsGroupByExpressionDefinitionsStored: Boolean;
    function IsSortByExpressionDefinitionsStored: Boolean;
    function GetOwner: TPersistent; override;
    procedure RefreshParamList;
    procedure RemoveFieldExpression(AFieldExpression: TdxFieldExpression);

    property EMFDataSource: TdxCustomEMFQueryDataSource read FEMFDataSource;

    property Owner: TComponent read GetOwnerComponent;

    property Criteria: IdxCriteriaOperator read GetCriteria write SetCriteria;
    property CriteriaText: string read GetCriteriaText write SetCriteriaText;
    property GroupCriteria: IdxCriteriaOperator read GetGroupCriteria
      write SetGroupCriteria;
    property GroupCriteriaText: string read GetGroupCriteriaText
      write SetGroupCriteriaText;
    property GroupByExpressionDefinitions: TdxGroupByExpressionDefinitions
      read GetGroupByExpressionDefinitions write SetGroupByExpressionDefinitions
      stored IsGroupByExpressionDefinitionsStored;
    property FieldExpressions: TdxFieldExpressions read FFieldExpressions
      write SetFieldExpressions stored IsFieldExpressionsStored;
    property Params: TParams read FParams write SetParams;
    property SortByExpressions: IdxSortByExpressions read GetSortByExpressions
      write SetSortByExpressions;
    property SortByExpressionDefinitions: TdxSortByExpressionDefinitions
      read GetSortByExpressionDefinitions write SetSortByExpressionDefinitions
      stored IsSortByExpressionDefinitionsStored;
    property SkipSelectedRecords: Integer read FSkipSelectedRecords
      write SetSkipSelectedRecords default 0;
    property TopSelectedRecords: Integer read FTopSelectedRecords
      write SetTopSelectedRecords default 0;
  public
    constructor Create(AOwner: TdxCustomEMFQueryDataSource);
    destructor Destroy; override;
    procedure ApplyParams; overload;
  end;

  { TdxEMFQueryProperties }

  TdxEMFQueryProperties = class(TdxCustomEMFQueryProperties)
  public
    property Criteria;
    property GroupCriteria;
    property SortByExpressions;
  published
    property CriteriaText;
    property GroupByExpressionDefinitions;
    property GroupCriteriaText;
    property FieldExpressions;
    property SkipSelectedRecords;
    property TopSelectedRecords;
    property SortByExpressionDefinitions;
    property Params;
  end;

  { TdxEMFQueryDataSource }

  TdxCustomEMFQueryDataSource = class(TdxCustomEMFDataSource)
  strict private
    FQueryProperties: TdxEMFQueryProperties;
    procedure SetQueryProperties(const Value: TdxEMFQueryProperties);
  protected
    procedure ActiveChanged(AActive: Boolean); override;
    property QueryProperties: TdxEMFQueryProperties read FQueryProperties
      write SetQueryProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  IOUtils, DBConsts,
  FmtBCD,
  dxEMF.Utils.Exceptions,
  dxEMF.Strs,
  dxEMF.Utils,
  dxEMF.Linq;

type

  TdxEMFCustomCollectionAccess = class(TdxEMFCustomCollection);
  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);

  { TdxLinqExpressionFactoryHelper }

  TdxLinqExpressionFactoryHelper = class helper for TdxLinqExpressionFactory
  public
    class function GetDataContextEntityClasses(ADataContext: PTypeInfo)
      : TArray<TClass>;
    class function FindDataContext(const ADataContextName: string): PTypeInfo;
    class function IsRegistered(AEntity: TClass): Boolean; overload;
    class function GetEntities: TArray<TClass>;
    class function GetDataContexts: TArray<PTypeInfo>;
  end;

  { TdxLinqExpressionFactoryHelper }

class function TdxLinqExpressionFactoryHelper.FindDataContext
  (const ADataContextName: string): PTypeInfo;
var
  I: Integer;
begin
  for I := 0 to DataContexts.Count - 1 do
    if SameText(GetTypeName(DataContexts[I]), ADataContextName) then
      Exit(DataContexts[I]);
  Result := nil;
end;

class function TdxLinqExpressionFactoryHelper.GetDataContextEntityClasses
  (ADataContext: PTypeInfo): TArray<TClass>;
begin
  Result := DataContextEntityClasses(ADataContext);
end;

class function TdxLinqExpressionFactoryHelper.GetDataContexts
  : TArray<PTypeInfo>;
begin
  Result := DataContexts.ToArray;
end;

class function TdxLinqExpressionFactoryHelper.GetEntities: TArray<TClass>;
begin
  if Expressions = nil then
    Exit(nil);
  Result := Expressions.Keys.ToArray;
end;

class function TdxLinqExpressionFactoryHelper.IsRegistered
  (AEntity: TClass): Boolean;
begin
  Result := (Expressions <> nil) and Expressions.ContainsKey(AEntity);
end;

{ TdxEMFCustomDataContext.TPackageController }

constructor TdxEMFCustomDataContext.TPackageController.Create
  (const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

destructor TdxEMFCustomDataContext.TPackageController.Destroy;
begin
  UnLoad;
  inherited Destroy;
end;

procedure TdxEMFCustomDataContext.TPackageController.Load;
begin
  FModule := SysUtils.LoadPackage(FFileName);
end;

procedure TdxEMFCustomDataContext.TPackageController.UnLoad;
begin
  SysUtils.UnloadPackage(FModule);
  FModule := 0;
end;

{ TdxEMFCustomDataContext }

constructor TdxEMFCustomDataContext.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSources := TList<TdxCustomEMFDataSource>.Create;
end;

destructor TdxEMFCustomDataContext.Destroy;
begin
  FreeAndNil(FDataSources);
  FreeAndNil(FPackageController);
  inherited Destroy;
end;

procedure TdxEMFCustomDataContext.RegisterDataSources
  (ADataSource: TdxCustomEMFDataSource);
begin
  if IsDestroying then
    Exit;
  Assert(ADataSource <> nil);
  FDataSources.Add(ADataSource);
  FreeNotification(ADataSource);
end;

procedure TdxEMFCustomDataContext.UnregisterDataSources
  (ADataSource: TdxCustomEMFDataSource);
begin
  if IsDestroying then
    Exit;
  Assert(ADataSource <> nil);
  FDataSources.Remove(ADataSource);
  RemoveFreeNotification(ADataSource);
end;

procedure TdxEMFCustomDataContext.CloseDataSources;
var
  ADataSource: TdxCustomEMFDataSource;
begin
  for ADataSource in FDataSources do
    ADataSource.Close;
end;

function TdxEMFCustomDataContext.CreatePackageController: TPackageController;
begin
  Result := TPackageController.Create(PackageFileName);
  Result.Load;
end;

class function TdxEMFCustomDataContext.GetDataContexts: TArray<PTypeInfo>;
begin
  Result := TdxLinqExpressionFactory.GetDataContexts;
end;

class function TdxEMFCustomDataContext.GetEntityClasses(const APackageName
  : string): TArray<TClass>;
var
  ARttiPackage: TRttiPackage;
  APackages: TArray<TRttiPackage>;
  ATypes: TArray<TRttiType>;
  AType: TRttiType;
  AEntityInfo: TdxEntityInfo;
  AClass: TClass;
  AResult: TList<TClass>;
begin
  AResult := TList<TClass>.Create;
  try
    if APackageName = '' then
    begin
      for AEntityInfo in EntityManager do
        AResult.Add(AEntityInfo.ClassAttributes.PersistentClass);
    end
    else
    begin
      APackages := TRttiContext.Create.GetPackages;
      for ARttiPackage in APackages do
        if SameText(ARttiPackage.Name, APackageName) or
          SameText(TPath.GetFileNameWithoutExtension(ARttiPackage.Name),
          APackageName) then
        begin
          ATypes := ARttiPackage.GetTypes;
          for AType in ATypes do
            if AType.IsInstance then
            begin
              AClass := AType.Handle.TypeData.ClassType;
              if TdxLinqExpressionFactory.IsRegistered(AClass) then
                AResult.Add(AClass)
              else
              begin
                AEntityInfo := EntityManager.GetEntityInfo(AClass);
                if AEntityInfo <> nil then
                  AResult.Add(AClass);
              end;
            end;
        end;
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

function TdxEMFCustomDataContext.GetEntityClasses: TArray<TClass>;
begin
  if PackageName <> '' then
    Result := GetEntityClasses(PackageName)
  else if PackageFileName <> '' then
  begin
    if FPackageController = nil then
      FPackageController := CreatePackageController;
    Result := GetEntityClasses(PackageName)
  end
  else
    Result := nil;
end;

function TdxEMFCustomDataContext.GetEntityInfo(const AClassName: string)
  : TdxEntityInfo;
begin
  if PackageName <> '' then
    GetEntityClasses(PackageName)
  else if PackageFileName <> '' then
  begin
    if FPackageController = nil then
      FPackageController := CreatePackageController;
    GetEntityClasses(TPath.GetFileNameWithoutExtension(PackageFileName));
  end;
  Result := EntityManager.GetEntityInfo(AClassName)
end;

function TdxEMFCustomDataContext.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxEMFCustomDataContext.GetPackageController: TPackageController;
begin
  if FPackageController = nil then
    FPackageController := CreatePackageController;
  Result := nil;
end;

procedure TdxEMFCustomDataContext.SetPackageName(const Value: string);
begin
  if FPackageName = Value then
    Exit;
  CloseDataSources;
  PackageFileName := '';
  FreeAndNil(FPackageController);
  FPackageName := Value;
end;

procedure TdxEMFCustomDataContext.SetPackageFileName(const Value: string);
begin
  if FPackageFileName = Value then
    Exit;
  CloseDataSources;
  FPackageFileName := Value;
  FPackageName := '';
  FreeAndNil(FPackageController);
end;

{ TdxCriteriaOperatorHolder }

class operator TdxCriteriaOperatorHolder.Implicit(const ACriteriaOperator
  : IdxCriteriaOperator): TdxCriteriaOperatorHolder;
begin
  Result.FCriteriaOperator := ACriteriaOperator;
  Result.FCriteriaOperatorText := '';
  Result.FOperandValue := nil;
end;

class operator TdxCriteriaOperatorHolder.Equal
  (const A: TdxCriteriaOperatorHolder;
  const ACriteriaOperator: IdxCriteriaOperator): Boolean;
begin
  Result := A.FCriteriaOperator = ACriteriaOperator;
end;

class operator TdxCriteriaOperatorHolder.Equal
  (const A: TdxCriteriaOperatorHolder;
  const ACriteriaOperatorText: string): Boolean;
begin
  Result := A.FCriteriaOperatorText = ACriteriaOperatorText;
end;

function TdxCriteriaOperatorHolder.GetCriteriaOperatorText: string;
begin
  if (FCriteriaOperatorText <> '') then
    Result := FCriteriaOperatorText
  else
    Result := TdxCriteriaOperator.ToString(FCriteriaOperator);
end;

function TdxCriteriaOperatorHolder.GetHasParams: Boolean;
begin
  Result := Length(FOperandValue) > 0;
end;

class operator TdxCriteriaOperatorHolder.Implicit
  (const A: TdxCriteriaOperatorHolder): IdxCriteriaOperator;
begin
  Result := A.FCriteriaOperator;
end;

class operator TdxCriteriaOperatorHolder.Implicit(const ACriteriaOperatorText
  : string): TdxCriteriaOperatorHolder;
begin
  Result.FCriteriaOperatorText := ACriteriaOperatorText;
  Result.FCriteriaOperator := TdxCriteriaOperator.Parse(ACriteriaOperatorText,
    Result.FOperandValue);
end;

class operator TdxCriteriaOperatorHolder.Implicit
  (const A: TdxCriteriaOperatorHolder): string;
begin
  Result := A.CriteriaOperatorText;
end;

{ TdxFieldExpressions }

function TdxFieldExpressions.Add: TdxFieldExpression;
begin
  Result := TdxFieldExpression(inherited Add);
end;

class function TdxFieldExpressions.GetExpressionDefinitionClass
  : TdxExpressionDefinitionClass;
begin
  Result := TdxFieldExpression;
end;

function TdxFieldExpressions.GetFieldExpression(Index: Integer)
  : TdxFieldExpression;
begin
  Result := TdxFieldExpression(inherited Items[Index]);
end;

procedure TdxFieldExpressions.RemoveItem(AFieldExpression: TdxFieldExpression);
begin
  if csDestroying in EMFDataSource.ComponentState then
    Exit;
  EMFDataSource.QueryProperties.RemoveFieldExpression(AFieldExpression);
end;

procedure TdxFieldExpressions.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if Action in [TCollectionNotification.cnExtracting,
    TCollectionNotification.cnDeleting] then
    RemoveItem(TdxFieldExpression(Item))
  else
    inherited Notify(Item, Action);
end;

procedure TdxFieldExpressions.SetFieldExpression(Index: Integer;
  Value: TdxFieldExpression);
begin
  inherited Items[Index] := Value;
end;

procedure TdxFieldExpressions.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Item = nil then
  begin
    EMFDataSource.Reset;
    EMFDataSource.EntityInfo := nil;
  end;
end;

{ TdxExpressionDefinition }

procedure TdxExpressionDefinition.Assign(ASource: TPersistent);
var
  AExpressionDefinition: TdxExpressionDefinition absolute ASource;
begin
  if ASource is TdxExpressionDefinition then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      DoAssign(AExpressionDefinition);
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TdxExpressionDefinition.DoAssign(ASource: TdxExpressionDefinition);
begin
  ExpressionText := ASource.ExpressionText;
end;

function TdxExpressionDefinition.GetEMFDataSource: TdxCustomEMFQueryDataSource;
begin
  Result := TdxFieldExpressions(Collection).EMFDataSource;
end;

function TdxExpressionDefinition.GetDisplayName: string;
begin
  Result := ExpressionText;
end;

function TdxExpressionDefinition.GetExpression: IdxCriteriaOperator;
begin
  Result := FExpression;
end;

function TdxExpressionDefinition.GetExpressionText: string;
begin
  Result := FExpression;
end;

function TdxExpressionDefinition.GetOperandValues: TArray<IdxOperandValue>;
begin
  Result := FExpression.OperandValues;
end;

procedure TdxExpressionDefinition.SetExpression(const Value
  : IdxCriteriaOperator);
begin
  if FExpression = Value then
    Exit;
  FExpression := Value;
  Update;
end;

procedure TdxExpressionDefinition.SetExpressionText(const Value: string);
begin
  if FExpression = Value then
    Exit;
  FExpression := Value;
  Update;
end;

procedure TdxExpressionDefinition.Update;
begin
  EMFDataSource.QueryProperties.RefreshParamList;
end;

{ TdxExpressionDefinitions }

constructor TdxExpressionDefinitions.Create(AEMFDataSource
  : TdxCustomEMFQueryDataSource);
begin
  inherited Create(AEMFDataSource, GetExpressionDefinitionClass);
end;

function TdxExpressionDefinitions.GetEMFDataSource: TdxCustomEMFQueryDataSource;
begin
  Result := TdxCustomEMFQueryDataSource(GetOwner);
end;

class function TdxExpressionDefinitions.GetExpressionDefinitionClass
  : TdxExpressionDefinitionClass;
begin
  Result := TdxExpressionDefinition;
end;

{ TdxGroupByExpressionDefinition }

procedure TdxGroupByExpressionDefinition.Update;
var
  I: Integer;
  AFieldExpression: TdxFieldExpression;
begin
  if csReading in EMFDataSource.ComponentState then
    Exit;
  for I := 0 to EMFDataSource.QueryProperties.FieldExpressions.Count - 1 do
  begin
    AFieldExpression := EMFDataSource.QueryProperties.FieldExpressions[I];
    if SameText(AFieldExpression.ExpressionText, ExpressionText) then
    begin
      inherited Update;
      Exit;
    end;
  end;
  DatabaseErrorFmt(SFieldNotFound, [ExpressionText], EMFDataSource);
end;

{ TdxGroupByExpressionDefinitions }

function TdxGroupByExpressionDefinitions.Add: TdxGroupByExpressionDefinition;
begin
  Result := TdxGroupByExpressionDefinition(inherited Add);
end;

class function TdxGroupByExpressionDefinitions.GetExpressionDefinitionClass
  : TdxExpressionDefinitionClass;
begin
  Result := TdxGroupByExpressionDefinition;
end;

function TdxGroupByExpressionDefinitions.GetItem(Index: Integer)
  : TdxGroupByExpressionDefinition;
begin
  Result := TdxGroupByExpressionDefinition(inherited Items[Index]);
end;

procedure TdxGroupByExpressionDefinitions.SetItem(Index: Integer;
  const Value: TdxGroupByExpressionDefinition);
begin
  inherited Items[Index] := Value;
end;

{ TdxSortByExpressionDefinition }

procedure TdxSortByExpressionDefinition.DoAssign
  (ASource: TdxExpressionDefinition);
begin
  inherited DoAssign(ASource);
  FSortDirection := (ASource as TdxSortByExpressionDefinition).FSortDirection;
end;

function TdxSortByExpressionDefinition.GetDisplayName: string;
begin
  Result := ExpressionText;
  if SortDirection <> TdxSortDirection.Ascending then
    Result := Result + ', Descending';
end;

{ TdxSortByExpressionDefinitions }

function TdxSortByExpressionDefinitions.Add: TdxSortByExpressionDefinition;
begin
  Result := TdxSortByExpressionDefinition(inherited Add);
end;

class function TdxSortByExpressionDefinitions.GetExpressionDefinitionClass
  : TdxExpressionDefinitionClass;
begin
  Result := TdxSortByExpressionDefinition;
end;

function TdxSortByExpressionDefinitions.GetItem(Index: Integer)
  : TdxSortByExpressionDefinition;
begin
  Result := TdxSortByExpressionDefinition(inherited Items[Index]);
end;

function TdxSortByExpressionDefinitions.GetSortByExpressions
  : IdxSortByExpressions;
var
  ASortByExpressions: TdxSortByExpressions;
  I: Integer;
begin
  if Count = 0 then
    Exit(nil);
  ASortByExpressions := TdxSortByExpressions.Create;
  for I := 0 to Count - 1 do
    ASortByExpressions.Add(TdxSortByExpression.Create(Items[I].Expression,
      Items[I].SortDirection));
  Result := ASortByExpressions;
end;

procedure TdxSortByExpressionDefinitions.SetItem(Index: Integer;
  const Value: TdxSortByExpressionDefinition);
begin
  inherited Items[Index] := Value;
end;

procedure TdxSortByExpressionDefinitions.SetSortByExpressions
  (const Value: IdxSortByExpressions);
var
  ASortByExpression: IdxSortByExpression;
  ASortByDef: TdxSortByExpressionDefinition;
begin
  BeginUpdate;
  try
    Clear;
    for ASortByExpression in Value do
    begin
      ASortByDef := Add;
      ASortByDef.Expression := ASortByExpression.Expression as
        IdxCriteriaOperator;
      ASortByDef.SortDirection := ASortByExpression.SortDirection;
    end;
  finally
    EndUpdate;
  end;
end;

{ TdxCustomEMFDataSource }

constructor TdxCustomEMFDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateClientList;
  if IsDesigning then
  begin
    Session := FindDefaultComponent<TdxEMFCustomSession>(AOwner);
    DataContext := FindDefaultComponent<TdxEMFCustomDataContext>(AOwner);
  end;
end;

destructor TdxCustomEMFDataSource.Destroy;
begin
  Destroying;
  Close;
  EntityInfo := nil;
  Session := nil;
  DataContext := nil;
  DestroyClientList;
  inherited Destroy;
end;

procedure TdxCustomEMFDataSource.AssignData(AObject: TObject);
begin
  FDataCollection := nil;
  CreateCollection;
  Add(AObject);
end;

procedure TdxCustomEMFDataSource.Add(AObject: TObject);
begin
  FDataCollection.Add(AObject);
end;

procedure TdxCustomEMFDataSource.AssignData(const AEMFCollection
  : IdxEMFCollection);
begin
  FDataCollection := AEMFCollection;
  FIsExternalData := True;
end;

procedure TdxCustomEMFDataSource.AssignTo(ADest: TPersistent);
var
  ADestEMFDataSource: TdxCustomEMFDataSource absolute ADest;
begin
  if not(ADest is TdxCustomEMFDataSource) then
    inherited AssignTo(ADest);
  ADestEMFDataSource.EntityName := EntityName;
  ADestEMFDataSource.EntityInfo := EntityInfo;
  ADestEMFDataSource.PackageName := PackageName;
  ADestEMFDataSource.DataContext := DataContext;
  ADestEMFDataSource.Session := Session;
  ADestEMFDataSource.Tag := Tag;
end;

procedure TdxCustomEMFDataSource.InternalClose;
begin
  if not FIsExternalData then
    FDataCollection := nil;
end;

procedure TdxCustomEMFDataSource.CheckEntityInfo;
begin
  if EntityInfo = nil then
    if EntityName = '' then
      raise EdxNoEntityInfoException.CreateFmt
        (sdxEntityNameNotSpecified, [Name])
    else
      raise EdxNoEntityInfoException.CreateFmt(sdxEntityCannotBeFound,
        [EntityName, Name]);
end;

procedure TdxCustomEMFDataSource.Close;
begin
  Active := False;
end;

procedure TdxCustomEMFDataSource.ConnectionStateChange(ASender: TObject;
  AConnecting: Boolean);
begin
  Active := AConnecting;
  if not AConnecting then
    EntityInfo := nil;
end;

procedure TdxCustomEMFDataSource.CreateClientList;
begin
end;

procedure TdxCustomEMFDataSource.DestroyClientList;
begin
end;

procedure TdxCustomEMFDataSource.CreateCollection;
begin
  Assert(EntityClass <> nil);
  AssignData(TdxEMFCollections.Create(EntityClass));
end;

procedure TdxCustomEMFDataSource.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      Active := True;
  except
    if csDesigning in ComponentState then
      ApplicationHandleException(Self)
    else
      raise;
  end;
end;

procedure TdxCustomEMFDataSource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = DataContext then
      DataContext := nil;
    if AComponent = Session then
      Session := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TdxCustomEMFDataSource.Delete(AObject: TObject);
begin
  if not FDataCollection.DeleteObjectOnRemove then
    Session.Delete(AObject);
  FDataCollection.Remove(AObject);
end;

procedure TdxCustomEMFDataSource.DoAssignEntity;
begin
  if (Owner <> nil) and (csLoading in Owner.ComponentState) then
    Exit;
  if FEntityName = '' then
    EntityClass := nil
  else
  begin
    if FDataContext <> nil then
      EntityInfo := FDataContext.GetEntityInfo(FEntityName)
    else
      EntityInfo := EntityManager.GetEntityInfo(FEntityName);
    if FEntityInfo = nil then
      FEntityClass := nil
    else
      FEntityClass := EntityInfo.ClassAttributes.PersistentClass;
  end;
end;

procedure TdxCustomEMFDataSource.FetchAll;
begin
  if HasLoader then
    TdxEMFCustomCollectionAccess(TdxEMFCustomCollection(FDataCollection))
      .DoFetchAll;
end;

function TdxCustomEMFDataSource.FindDefaultComponent<T>(AOwner: TComponent): T;
var
  I: Integer;
begin
  Result := nil;
  while AOwner <> nil do
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[I] is T then
      begin
        Result := T(AOwner.Components[I]);
        Exit;
      end;
    AOwner := AOwner.Owner;
  end;
end;

function TdxCustomEMFDataSource.GetRecordCount: Integer;
begin
  if FDataCollection = nil then
    Result := 0
  else
    Result := FDataCollection.Count;
end;

function TdxCustomEMFDataSource.GetEntityInfo: TdxEntityInfo;
begin
  if FEntityInfo = nil then
    DoAssignEntity;
  Result := FEntityInfo;
end;

function TdxCustomEMFDataSource.GetHasLoader: Boolean;
begin
  Result := (FDataCollection <> nil) and
    (TdxEMFCustomCollection(FDataCollection).Loader <> nil);
end;

function TdxCustomEMFDataSource.GetIsDesigning: Boolean;
begin
  Result := ([csDesigning, csLoading] * ComponentState = [csDesigning]) and
    ((Owner = nil) or ([csDesigning, csLoading] * Owner.ComponentState =
    [csDesigning]));
end;

function TdxCustomEMFDataSource.IndexOf(AObject: TObject): Integer;
begin
  Result := -1;
  if FDataCollection = nil then
    Exit;
  Result := FDataCollection.IndexOf(AObject);
end;

procedure TdxCustomEMFDataSource.Insert(AIndex: Integer; AObject: TObject);
begin
  if AIndex = -1 then
    Add(AObject)
  else
    (FDataCollection as TdxEMFCustomCollectionAccess).Insert(AIndex, AObject);
end;

function TdxCustomEMFDataSource.MoveNext: Boolean;
begin
  if HasLoader then
    Result := (FDataCollection as TdxEMFCustomCollectionAccess).LoadNextObject
  else
    Result := False;
end;

procedure TdxCustomEMFDataSource.NotifyClients;
begin
  // do nothing
end;

procedure TdxCustomEMFDataSource.Open;
begin
  Active := True;
end;

procedure TdxCustomEMFDataSource.Reset;
begin
  FDataCollection := nil;
end;

procedure TdxCustomEMFDataSource.SetDataContext(const Value
  : TdxEMFCustomDataContext);
begin
  if FDataContext = Value then
    Exit;
  if not(csReading in ComponentState) then
    Active := False;
  if FDataContext <> nil then
    FDataContext.UnregisterDataSources(Self);
  FDataContext := Value;
  if FDataContext <> nil then
    FDataContext.RegisterDataSources(Self);
end;

procedure TdxCustomEMFDataSource.SetEntityClass(const Value: TClass);
begin
  if FEntityClass = Value then
    Exit;
  if not(csReading in ComponentState) then
    Active := False;
  FEntityClass := Value;
  EntityInfo := EntityManager.GetEntityInfo(Value);
  if FEntityInfo <> nil then
    FEntityName := FEntityInfo.GetQualifiedEntityClassName
  else
    FEntityName := '';
end;

function TdxCustomEMFDataSource.IsSessionOpen: Boolean;
begin
  if Session = nil then
    DatabaseError(sdxMissingSession, Self);
  Result := TdxEMFCustomSessionAccess(Session).IsReady;
end;

procedure TdxCustomEMFDataSource.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
    FStreamedActive := Value
  else
  begin
    if Value then
      Value := IsSessionOpen;
    if FActive <> Value then
      try
        try
          ActiveChanged(Value);
          FActive := Value;
        except
          FActive := False;
          raise;
        end;
      finally
        NotifyClients;
      end;
  end;
end;

procedure TdxCustomEMFDataSource.SetEntityInfo(const Value: TdxEntityInfo);
begin
  if FEntityInfo = Value then
    Exit;
  Active := False;
  if FEntityInfo <> nil then
    TdxEntityManager.UnregisterEntityClient(FEntityInfo, Self);
  FEntityInfo := Value;
  if FEntityInfo <> nil then
  begin
    FEntityClass := Value.ClassAttributes.PersistentClass;
    TdxEntityManager.RegisterEntityClient(FEntityInfo, Self,
      ConnectionStateChange);
  end
  else
  begin
    FEntityClass := nil;
  end;
end;

procedure TdxCustomEMFDataSource.SetEntityName(const Value: string);
begin
  if EntityName = Value then
    Exit;
  if not(csReading in ComponentState) then
    Active := False;
  FEntityName := Value;
  Reset;
  DoAssignEntity;
end;

procedure TdxCustomEMFDataSource.SetPackageName(const Value: string);
begin
  if FPackageName = Value then
    Exit;
  if not(csReading in ComponentState) then
    Active := False;
  FPackageName := Value;
end;

procedure TdxCustomEMFDataSource.SetSession(const Value: TdxEMFCustomSession);
begin
  if FSession = Value then
    Exit;
  if not(csReading in ComponentState) then
    Active := False;
  if FSession <> nil then
    TdxEMFCustomSessionAccess(FSession).UnregisterClient(Self);
  FSession := Value;
  if Value <> nil then
    TdxEMFCustomSessionAccess(Value).RegisterClient(Self,
      ConnectionStateChange);
end;

{ TdxCustomEMFQueryProperties }

constructor TdxCustomEMFQueryProperties.Create
  (AOwner: TdxCustomEMFQueryDataSource);
begin
  inherited Create;
  FEMFDataSource := AOwner;
  FFieldExpressions := TdxFieldExpressions.Create(EMFDataSource);
  FParams := TParams.Create(Self);
end;

destructor TdxCustomEMFQueryProperties.Destroy;
begin
  FreeAndNil(FFieldExpressions);
  FreeAndNil(FParams);
  FreeAndNil(FGroupByExpressionDefinitions);
  FreeAndNil(FSortByExpressionDefinitions);
  inherited Destroy;
end;

procedure TdxCustomEMFQueryProperties.ApplyParams(const AOperandValues
  : TArray<IdxOperandValue>; AIndex: Integer);
var
  AOperandValue: IdxOperandValue;
  AOperandParameter: IdxOperandParameter;
  AParam: TParam;
  I: Integer;
begin
  if (Length(AOperandValues) = 0) or (Params.Count = 0) then
    Exit;
  for I := 0 to Length(AOperandValues) - 1 do
  begin
    AOperandValue := AOperandValues[I];
    if Supports(AOperandValue, IdxOperandParameter, AOperandParameter) then
      AParam := Params.FindParam(AOperandParameter.ParameterName)
    else if (I + AIndex) < Params.Count then
      AParam := Params[I + AIndex]
    else
      AParam := nil;
    if AParam <> nil then
      case AParam.DataType of
        ftString, ftFixedChar, ftFixedWideChar, ftWideString, ftMemo,
          ftWideMemo:
          AOperandParameter.Value := AParam.AsString;
        ftShortInt:
          AOperandParameter.Value := TValue.From<ShortInt>(AParam.AsSmallInt);
        ftSmallint:
          AOperandParameter.Value := TValue.From<SmallInt>(AParam.AsSmallInt);
        ftWord:
          AOperandParameter.Value := TValue.From<Word>(AParam.AsSmallInt);
        ftByte:
          AOperandParameter.Value := TValue.From<Byte>(AParam.AsSmallInt);
        ftBoolean:
          AOperandParameter.Value := AParam.AsBoolean;
        ftInteger:
          AOperandParameter.Value := AParam.AsInteger;
        ftLargeInt:
          AOperandParameter.Value := AParam.AsLargeInt;
        ftFloat:
          AOperandParameter.Value := AParam.AsFloat;
        ftBCD, ftCurrency:
          AOperandParameter.Value := TValue.From<Currency>(AParam.AsCurrency);
        ftFMTBCD:
          AOperandParameter.Value := TValue.From<TBcd>(AParam.AsFMTBCD);
        TFieldType.ftDate:
          AOperandParameter.Value := TValue.From<TDate>(AParam.AsDateTime);
        TFieldType.ftTime:
          AOperandParameter.Value := TValue.From<TTime>(AParam.AsDateTime);
        TFieldType.ftDateTime:
          AOperandParameter.Value := TValue.From<TDateTime>(AParam.AsDateTime);
      else
        AOperandParameter.Value := TValue.FromVariant(AParam.Value);
      end;
  end;
end;

procedure TdxCustomEMFQueryProperties.ApplyParams;
var
  I: Integer;
  AIndex: Integer;
begin
  AIndex := 0;
  for I := 0 to FieldExpressions.Count - 1 do
  begin
    ApplyParams(FieldExpressions[I].OperandValues, AIndex);
    Inc(AIndex, Length(FieldExpressions[I].OperandValues));
  end;
  ApplyParams(FCriteria.OperandValues);
  ApplyParams(FGroupCriteria.OperandValues);
end;

class procedure TdxCustomEMFQueryProperties.CollectParams(AList: TParams;
  const AExpression: string);
var
  I: Integer;
  ALocalList: TParams;
begin
  ALocalList := TParams.Create;
  try
    ALocalList.ParseSQL(AExpression, True);
    for I := 0 to ALocalList.Count - 1 do
      if AList.FindParam(ALocalList[I].Name) = nil then
        AList.Add.Assign(ALocalList[I]);
  finally
    ALocalList.Free;
  end;
end;

function TdxCustomEMFQueryProperties.GetCriteria: IdxCriteriaOperator;
begin
  Result := FCriteria;
end;

function TdxCustomEMFQueryProperties.GetCriteriaText: string;
begin
  Result := FCriteria;
end;

function TdxCustomEMFQueryProperties.GetGroupByExpressionDefinitions
  : TdxGroupByExpressionDefinitions;
begin
  if FGroupByExpressionDefinitions = nil then
    if not(csWriting in Owner.ComponentState) then
      FGroupByExpressionDefinitions := TdxGroupByExpressionDefinitions.Create
        (EMFDataSource);
  Result := FGroupByExpressionDefinitions;
end;

function TdxCustomEMFQueryProperties.GetGroupCriteria: IdxCriteriaOperator;
begin
  Result := FGroupCriteria;
end;

function TdxCustomEMFQueryProperties.GetGroupCriteriaText: string;
begin
  Result := FGroupCriteria;
end;

function TdxCustomEMFQueryProperties.GetOwnerComponent: TComponent;
begin
  Result := TComponent(GetOwner);
end;

function TdxCustomEMFQueryProperties.GetOwner: TPersistent;
begin
  Result := FEMFDataSource;
end;

function TdxCustomEMFQueryProperties.GetSortByExpressionDefinitions
  : TdxSortByExpressionDefinitions;
begin
  if FSortByExpressionDefinitions = nil then
    if not(csWriting in Owner.ComponentState) then
      FSortByExpressionDefinitions := TdxSortByExpressionDefinitions.Create
        (EMFDataSource);
  Result := FSortByExpressionDefinitions;
end;

function TdxCustomEMFQueryProperties.GetSortByExpressions: IdxSortByExpressions;
begin
  if FSortByExpressionDefinitions = nil then
    Result := nil
  else
    Result := SortByExpressionDefinitions.SortByExpressions;
end;

function TdxCustomEMFQueryProperties.IsFieldExpressionsStored: Boolean;
begin
  Result := FFieldExpressions.Count > 0;
end;

function TdxCustomEMFQueryProperties.
  IsGroupByExpressionDefinitionsStored: Boolean;
begin
  Result := (FGroupByExpressionDefinitions <> nil) and
    (FGroupByExpressionDefinitions.Count > 0);
end;

function TdxCustomEMFQueryProperties.
  IsSortByExpressionDefinitionsStored: Boolean;
begin
  Result := (FSortByExpressionDefinitions <> nil) and
    (FSortByExpressionDefinitions.Count > 0);
end;

procedure TdxCustomEMFQueryProperties.RefreshParamList;
var
  I: Integer;
  AList: TParams;
begin
  AList := TParams.Create(Self);
  try
    for I := 0 to FieldExpressions.Count - 1 do
      CollectParams(AList, FieldExpressions[I].ExpressionText);
    CollectParams(AList, CriteriaText);
    CollectParams(AList, GroupCriteriaText);
    AList.AssignValues(FParams);
    FParams.Clear;
    FParams.Assign(AList);
  finally
    AList.Free;
  end;
end;

procedure TdxCustomEMFQueryProperties.RemoveFieldExpression(AFieldExpression
  : TdxFieldExpression);
var
  I: Integer;
  AItem: TdxGroupByExpressionDefinition;
begin
  if (FGroupByExpressionDefinitions <> nil) and
    (FGroupByExpressionDefinitions.Count > 0) then
  begin
    FGroupByExpressionDefinitions.BeginUpdate;
    try
      I := 0;
      while I < FGroupByExpressionDefinitions.Count do
      begin
        AItem := FGroupByExpressionDefinitions[I];
        if AnsiSameText(AItem.ExpressionText, AFieldExpression.ExpressionText)
        then
          FGroupByExpressionDefinitions.Delete(I)
        else
          Inc(I);
      end;
    finally
      FGroupByExpressionDefinitions.EndUpdate;
    end;
  end;
end;

procedure TdxCustomEMFQueryProperties.SetCriteria
  (const Value: IdxCriteriaOperator);
begin
  if FCriteria = Value then
    Exit;
  FCriteria := Value;
  RefreshParamList;
end;

procedure TdxCustomEMFQueryProperties.SetCriteriaText(const Value: string);
begin
  if FCriteria = Value then
    Exit;
  FCriteria := Value;
  RefreshParamList;
end;

procedure TdxCustomEMFQueryProperties.SetFieldExpressions
  (const Value: TdxFieldExpressions);
begin
  FFieldExpressions.Assign(Value);
  RefreshParamList;
end;

procedure TdxCustomEMFQueryProperties.SetGroupByExpressionDefinitions
  (const Value: TdxGroupByExpressionDefinitions);
begin
  GroupByExpressionDefinitions.Assign(Value);
end;

procedure TdxCustomEMFQueryProperties.SetGroupCriteria
  (const Value: IdxCriteriaOperator);
begin
  if FGroupCriteria = Value then
    Exit;
  FGroupCriteria := Value;
  RefreshParamList;
end;

procedure TdxCustomEMFQueryProperties.SetGroupCriteriaText(const Value: string);
begin
  if FGroupCriteria = Value then
    Exit;
  FGroupCriteria := Value;
  RefreshParamList;
end;

procedure TdxCustomEMFQueryProperties.SetParams(const Value: TParams);
begin
  FParams.Assign(Value);
end;

procedure TdxCustomEMFQueryProperties.SetSkipSelectedRecords
  (const Value: Integer);
begin
  if FSkipSelectedRecords = Value then
    Exit;
  FSkipSelectedRecords := Value;
end;

procedure TdxCustomEMFQueryProperties.SetSortByExpressionDefinitions
  (const Value: TdxSortByExpressionDefinitions);
begin

end;

procedure TdxCustomEMFQueryProperties.SetSortByExpressions
  (const Value: IdxSortByExpressions);
begin

end;

procedure TdxCustomEMFQueryProperties.SetTopSelectedRecords
  (const Value: Integer);
begin
  if FTopSelectedRecords = Value then
    Exit;
  FTopSelectedRecords := Value;
end;

{ TdxEMFQueryDataSource }

constructor TdxCustomEMFQueryDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueryProperties := TdxEMFQueryProperties.Create(Self);
end;

destructor TdxCustomEMFQueryDataSource.Destroy;
begin
  FreeAndNil(FQueryProperties);
  inherited Destroy;
end;

procedure TdxCustomEMFQueryDataSource.ActiveChanged(AActive: Boolean);
begin
end;

procedure TdxCustomEMFQueryDataSource.SetQueryProperties
  (const Value: TdxEMFQueryProperties);
begin
  FQueryProperties.Assign(Value);
end;

end.
