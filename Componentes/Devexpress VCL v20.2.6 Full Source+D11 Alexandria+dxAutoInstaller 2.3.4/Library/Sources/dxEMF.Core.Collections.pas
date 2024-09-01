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

unit dxEMF.Core.Collections;

interface

{$I dxEMF.inc}

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, RTTI,
  dxCoreClasses,
  dxEMF.Types,
  dxEMF.Metadata,
  dxEMF.Core.Pool,
  dxEMF.DB.Criteria,
  dxEMF.DB.AliasExpander,
  dxEMF.DB.Generator,
  dxEMF.Core.Loader,
  dxEMF.Core,
  dxEMF.Linq,
  dxEMF.Utils;

type

  { TdxEMFBaseCollection }

  TdxEMFBaseCollection = class(TdxEMFCustomCollection)
  strict private
    function GetIsEntity: Boolean; inline;
    function GetIsPersistentObject: Boolean; inline;
  protected
    class function IsClassEntity(AClass: TClass): Boolean; static;

    function BeginLoad(AForce: Boolean = False): TdxObjectsQuery;
    procedure EndLoad(const AObjects);
    procedure PrepareLoad;
    function GetRealFetchCriteria: IdxCriteriaOperator;

    procedure CheckDataType; virtual; abstract;
    property IsPersistentObject: Boolean read GetIsPersistentObject;
    property IsEntity: Boolean read GetIsEntity;
  public
    constructor Create; overload; virtual;
    constructor Create(ASession: TdxEMFCustomSession); overload;
    constructor Create(ASession: TdxEMFCustomSession; AOwner: TObject;
      AReferencedProperty: TdxMappingMemberInfo); overload;
    procedure Init(ASession: TdxEMFCustomSession); overload; override; final;
    procedure Init(ASession: TdxEMFCustomSession; AOwner: TObject;
      AReferencedProperty: TdxMappingMemberInfo); overload; override;
  end;

  // TdxEMFObjectList = class(TObjectList<TObject>);
  TdxEMFObjectList = class(TdxFastObjectList);

  { TdxEMFObjectCollection }

  TdxEMFObjectCollection = class(TdxEMFBaseCollection, IdxCollection, IdxList,
    IdxEMFCollection)
  strict private
  type

    TEnumerator = class(TInterfacedObject, IEnumerator)
    private
      FCollection: TdxEMFObjectCollection;
      FIndex: Integer;
    protected
      { IEnumerator }
      function GetCurrent: TObject;
    public
      constructor Create(ACollection: TdxEMFObjectCollection);
      function MoveNext: Boolean;
      procedure Reset;
      property Current: TObject read GetCurrent;
    end;

  strict private
    FList: TdxEMFObjectList;
    FNowAdding: TObject;
    FCollectionElementClass: TClass;
    function GetList: TdxEMFObjectList; inline;
  protected
    procedure Clear; override;
    function GetCollectionElementClass: TClass; override;
    function GetIsReadOnly: Boolean;
    procedure DetachSession; override;
    function InternalAppend(AObject: TObject): Integer; inline;
    function InternalAdd(AObject: TObject): Integer;
    function InternalGetEnumerator: IEnumerator; override;
    procedure SetCollectionElementClass(const Value: TClass); virtual;
    { IdxCollection }
    function GetCount: Integer;
    { IdxList }
    function Add(AObject: TObject): Integer;
    function Contains(AValue: TObject): Boolean;
    procedure Delete(AIndex: Integer);
    function GetItems(AIndex: Integer): TObject;
    procedure Remove(AObject: TObject);
    function IndexOf(AValue: TObject): Integer;
    { IdxEMFCollection }
    function First: TObject;
    function Last: TObject;
    function GetDeleteObjectOnRemove: Boolean;
    procedure SetDeleteObjectOnRemove(Value: Boolean);

    procedure FetchAll; override; final;
    procedure CheckDataType; override;
    function LoadNextObject: Boolean; override;
    function GetRealFetchCriteria: IdxCriteriaOperator;

    property List: TdxEMFObjectList read GetList;
  public
    constructor Create; overload; override;
    constructor Create(ASession: TdxEMFCustomSession); overload;
    constructor Create(ASession: TdxEMFCustomSession; AOwner: TObject;
      AReferencedProperty: TdxMappingMemberInfo); overload;
    destructor Destroy; override;
    procedure Insert(AIndex: Integer; AObject: TObject); override;
    procedure Load; override;

    property Count: Integer read GetCount;
    property CollectionElementClass: TClass read GetCollectionElementClass
      write SetCollectionElementClass;
    property DeleteObjectOnRemove: Boolean read GetDeleteObjectOnRemove
      write SetDeleteObjectOnRemove;
  end;

  { TdxEMFEntityCollection<T> }

  TdxEMFEntityCollection<T: class> = class(TdxEMFObjectCollection,
    IEnumerable<T>, IdxQueryable<T>, IdxCollection<T>, IdxList<T>,
    IdxEMFCollection<T>)
  strict private
  type

    TEnumerator = class(TInterfacedObject, IEnumerator<T>)
    private
      FCollection: TdxEMFEntityCollection<T>;
      FIndex: Integer;
    protected
      { IEnumerator }
      function GetCurrent: TObject;
      { IEnumerator<T> }
      function GetCurrentGeneric: T;
      function IEnumerator<T>.GetCurrent = GetCurrentGeneric;
    public
      constructor Create(ACollection: TdxEMFEntityCollection<T>);
      function MoveNext: Boolean;
      procedure Reset;
      property Current: T read GetCurrentGeneric;
    end;

  protected
    function InternalGetEnumerator: IEnumerator; override;
    { IEnumerable<T> }
    function GetEnumerator: IEnumerator<T>;
    { IdxQueryable<T> }
    function GetProvider: IdxQueryProvider<T>;
    { IdxCollection<T> }
    function Add(const AValue: T): Integer;
    procedure Remove(const AValue: T);
    function Contains(const AValue: T): Boolean;
    { IdxList<T> }
    function IndexOf(const AItem: T): Integer;
    procedure Insert(AIndex: Integer; const AItem: T); reintroduce;
    function GetItems(AIndex: Integer): T;
    { IdxEMFCollection<T> }
    function First: T;
    function Last: T;

    procedure CheckDataType; override;
    procedure SetCollectionElementClass(const Value: TClass); override;
    function GetCollectionElementClass: TClass; override;
  public
    function Find<TKey>(const AKey: TKey): T; overload;
    function Find(const AKey: TValue): T; overload;
    function Find(const AKeys: array of TValue): T; overload;
    function Find(APredicate: TdxPredicate<T>): T; overload;
    function GetObjects(APredicate: TdxPredicate<T>): TArray<T>; overload;
  end;

  { TdxXPReferencedCollectionHelperOneToMany }

  TdxReferencedCollectionHelperOneToMany = class(TdxReferencedCollectionHelper)
  strict private
    FAdded: TdxObjectSet;
    FRemoved: TdxObjectSet;
  protected
    function GetLoadCollectionOnModifyCore: Boolean; override;
  protected
    procedure BeforeAfterRemove(AObject: TObject); override;
    function GetObjectProperty: TdxMappingMemberInfo; inline;
    function GetObjectPropertyValue(AObject: TObject): TObject; inline;
    procedure SetObjectPropertyValue(AObject: TObject; ANewObject: TObject);
  public
    destructor Destroy; override;
    function GetHardcodedCriterion: IdxCriteriaOperator; override;
    procedure ClearChangesCache; override;
    procedure Save; override;
    procedure Add(ANewObject: TObject); override;
    procedure Remove(AObject: TObject); override;
    procedure Reload; override;
  end;

implementation

uses
  RTLConsts, TypInfo,
  dxCore,
  dxEMF.Strs,
  dxEMF.Utils.Exceptions;

type
  TdxEMFCollectionHelperAccess = class(TdxEMFCollectionHelper);
  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);
  TdxDataLoaderAccess = class(TdxDataLoader);

  { TdxReferencedCollectionHelperOneToMany }

destructor TdxReferencedCollectionHelperOneToMany.Destroy;
begin
  FreeAndNil(FAdded);
  FreeAndNil(FRemoved);
  inherited Destroy;
end;

function TdxReferencedCollectionHelperOneToMany.GetObjectProperty
  : TdxMappingMemberInfo;
begin
  Result := ReferencedProperty.AssociatedMember;
end;

function TdxReferencedCollectionHelperOneToMany.GetObjectPropertyValue
  (AObject: TObject): TObject;
begin
  Result := GetObjectProperty.GetValue(AObject).AsObject;
end;

function TdxReferencedCollectionHelperOneToMany.GetHardcodedCriterion
  : IdxCriteriaOperator;
begin
  Result := TdxBinaryOperator.Create
    (ReferencedProperty.AssociatedMember.MemberName,
    ReferencedProperty.Owner.KeyProperty.GetValue(OwnerObject));
end;

procedure TdxReferencedCollectionHelperOneToMany.ClearChangesCache;
begin
  FreeAndNil(FAdded);
  FreeAndNil(FRemoved);
end;

procedure TdxReferencedCollectionHelperOneToMany.Save;
var
  O: TObject;
  APool: TdxCustomEntityPool;
begin
  if not TdxEMFCustomSessionAccess(Session).IsUnitOfWork and ReferencedProperty.IsAggregated
  then
    ParentCollection.Load;
  if FRemoved <> nil then
  begin
    APool := TdxEMFCustomSessionAccess(Session).PoolClasses.GetPool
      (EntityInfo.ClassAttributes.PersistentClass);
    for O in FRemoved do
    begin
      if APool.IsNewObject(O) then
        Continue;
      Session.Save(O);
    end;
  end;
  if ReferencedProperty.IsAggregated and not TdxEMFCustomSessionAccess(Session).IsUnitOfWork
  then
  begin
    for O in ParentCollection do
      Session.Save(O);
  end
  else
  begin
    if FAdded <> nil then
      for O in FAdded do
        Session.Save(O);
  end;
end;

procedure TdxReferencedCollectionHelperOneToMany.SetObjectPropertyValue
  (AObject: TObject; ANewObject: TObject);
begin
  GetObjectProperty.SetValue(AObject, TValue.From<TObject>(ANewObject));
end;

procedure TdxReferencedCollectionHelperOneToMany.Add(ANewObject: TObject);
var
  AOldOwner: TObject;
begin
  AOldOwner := GetObjectPropertyValue(ANewObject);
  if AOldOwner <> OwnerObject then
    SetObjectPropertyValue(ANewObject, OwnerObject);
  inherited Add(ANewObject);
  if FAdded = nil then
    FAdded := TdxObjectSet.Create(4);
  FAdded.Add(ANewObject);
  if FRemoved <> nil then
    FRemoved.Remove(ANewObject);
end;

procedure TdxReferencedCollectionHelperOneToMany.Remove(AObject: TObject);
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  inherited Remove(AObject);
  if FAdded <> nil then
    FAdded.Remove(AObject);
  if FRemoved = nil then
    FRemoved := TdxObjectSet.Create(4);
  FRemoved.Add(AObject);
end;

procedure TdxReferencedCollectionHelperOneToMany.BeforeAfterRemove
  (AObject: TObject);
var
  AObjectProperty: TdxMappingMemberInfo;
begin
  inherited BeforeAfterRemove(AObject);
  AObjectProperty := GetObjectProperty;
  if (AObjectProperty.GetValue(AObject).AsObject = OwnerObject) and
    TdxEMFCustomSessionAccess(Session).CanNullifyAssociatedMember
    (AObjectProperty, AObject) then
    SetObjectPropertyValue(AObject, nil);
end;

procedure TdxReferencedCollectionHelperOneToMany.Reload;
begin
  ClearChangesCache;
  inherited Reload;
end;

function TdxReferencedCollectionHelperOneToMany.
  GetLoadCollectionOnModifyCore: Boolean;
begin
  Result := False;
end;

{ TdxEMFBaseCollection }

constructor TdxEMFBaseCollection.Create;
begin
  inherited Create;
  CheckDataType;
end;

constructor TdxEMFBaseCollection.Create(ASession: TdxEMFCustomSession);
begin
  Create;
  Init(ASession);
end;

constructor TdxEMFBaseCollection.Create(ASession: TdxEMFCustomSession;
  AOwner: TObject; AReferencedProperty: TdxMappingMemberInfo);
begin
  Create;
  Init(ASession, AOwner, AReferencedProperty);
end;

function TdxEMFBaseCollection.GetIsEntity: Boolean;
begin
  Result := CollectionProperties * [TCollectionProperty.IsEntity,
    TCollectionProperty.IsPersistentClass] <> [];
end;

function TdxEMFBaseCollection.GetIsPersistentObject: Boolean;
begin
  Result := GetCollectionProperties(TCollectionProperty.IsPersistentClass);
end;

function TdxEMFBaseCollection.BeginLoad(AForce: Boolean): TdxObjectsQuery;
const
  SkipSelectedRecords = 0;
  TopSelectedRecords = 0;
  SelectDeleted = False;
var
  ASorting: TdxSortByExpressions;
  AFetchCriteria: IdxCriteriaOperator;
begin
  ASorting := nil;
  AFetchCriteria := GetRealFetchCriteria;
  if not IsLoaded then
  begin
    Result := TdxObjectsQuery.Create(Helper.EntityInfo, AFetchCriteria,
      ASorting, SkipSelectedRecords, TopSelectedRecords,
      TdxCollectionCriteriaPatcher.Create(SelectDeleted), False);
    IsLoaded := True;
  end
  else
    Result := nil;
end;

procedure TdxEMFBaseCollection.EndLoad(const AObjects);
begin
  IsLoaded := True;
end;

class function TdxEMFBaseCollection.IsClassEntity(AClass: TClass): Boolean;
begin
  Result := EntityManager.GetEntityInfo(AClass) <> nil;
end;

function TdxEMFBaseCollection.GetRealFetchCriteria: IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.And,
    Helper.GetHardcodedCriterion, Helper.PatchCriteriaFromUserToFetch
    (Criteria));
end;

procedure TdxEMFBaseCollection.Init(ASession: TdxEMFCustomSession);
begin
  if Session = ASession then
    Exit;
  inherited Init(ASession);
  if Session <> nil then
  begin
    if (Helper <> nil) and (Loader = nil) and
      (Session.Options.Collections.LoadingStrategy = TdxLoadingStrategy.Eager)
    then
      PrepareLoad;
  end
  else
    Loader := nil;
end;

procedure TdxEMFBaseCollection.Init(ASession: TdxEMFCustomSession;
  AOwner: TObject; AReferencedProperty: TdxMappingMemberInfo);
begin
  if Session <> nil then
    Exit;
  if not AReferencedProperty.IsCollection then
    raise EInvalidOperation.CreateFmt(sdxCollectionsNotCollectionProperty,
      [AReferencedProperty.MemberName]);
  if Helper = nil then
  begin
    Helper := TdxReferencedCollectionHelperOneToMany.Create(Self, AOwner,
      AReferencedProperty);
    Helper.EntityInfo := AReferencedProperty.CollectionElementType;
  end;
  SetCollectionProperties(TCollectionProperty.IsLoading, True);
  Init(ASession);
end;

procedure TdxEMFBaseCollection.PrepareLoad;
var
  AQuery: TdxObjectsQuery;
begin
  AQuery := BeginLoad;
  if AQuery <> nil then
    try
      Loader := CreateLoader(AQuery, nil);
    finally
      AQuery.Free;
    end;
end;

{ TdxEMFObjectCollection }

constructor TdxEMFObjectCollection.Create;
begin
  inherited Create;
  FList := TdxEMFObjectList.Create(False);
end;

constructor TdxEMFObjectCollection.Create(ASession: TdxEMFCustomSession);
begin
  Create;
  Init(ASession);
end;

constructor TdxEMFObjectCollection.Create(ASession: TdxEMFCustomSession;
  AOwner: TObject; AReferencedProperty: TdxMappingMemberInfo);
begin
  Create;
  Init(ASession, AOwner, AReferencedProperty);
end;

procedure TdxEMFObjectCollection.Delete(AIndex: Integer);
begin
  Remove(FList[AIndex]);
end;

destructor TdxEMFObjectCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TdxEMFObjectCollection.GetList: TdxEMFObjectList;
begin
  if not IsLoaded then
    DoFetchAll;
  Result := FList;
end;

function TdxEMFObjectCollection.InternalAppend(AObject: TObject): Integer;
begin
  Result := FList.Add(AObject);
end;

procedure TdxEMFObjectCollection.SetCollectionElementClass(const Value: TClass);
begin
  FCollectionElementClass := Value;
  CheckDataType;
  List.OwnsObjects := (CollectionElementClass <> nil) and not IsEntity and
    not IsPersistentObject;
end;

function TdxEMFObjectCollection.Add(AObject: TObject): Integer;
begin
  if FNowAdding = AObject then
    Exit(-1);
  if FNowAdding <> nil then
    raise EInvalidOperation.Create(sdxCollectionsRecurringObjectAdd);
  FNowAdding := AObject;
  try
    if (Session <> nil) and not TdxEMFCustomSessionAccess(Session)
      .LoadingObjects.Contains(AObject) and
      ((Helper <> nil) and Helper.LoadCollectionOnModify) then
      DoFetchAll;
    Result := InternalAdd(AObject);
  finally
    FNowAdding := nil;
  end;
end;

function TdxEMFObjectCollection.Contains(AValue: TObject): Boolean;
begin
  Result := List.IndexOf(AValue) >= 0;
end;

procedure TdxEMFObjectCollection.DetachSession;
begin
  if not IsLoading then
    FList.Clear;
  inherited DetachSession;
end;

function TdxEMFObjectCollection.First: TObject;
begin
  if Count > 0 then
    Result := List[0]
  else
    Result := nil;
end;

function TdxEMFObjectCollection.GetCollectionElementClass: TClass;
begin
  Result := FCollectionElementClass;
end;

function TdxEMFObjectCollection.GetCount: Integer;
begin
  Result := List.Count;
end;

function TdxEMFObjectCollection.GetDeleteObjectOnRemove: Boolean;
begin
  Result := GetCollectionProperties(TCollectionProperty.DeleteObjectOnRemove);
end;

function TdxEMFObjectCollection.GetRealFetchCriteria: IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.And,
    Helper.GetHardcodedCriterion, Helper.PatchCriteriaFromUserToFetch
    (Criteria));
end;

function TdxEMFObjectCollection.GetItems(AIndex: Integer): TObject;
begin
  Result := List.Items[AIndex];
end;

function TdxEMFObjectCollection.GetIsReadOnly: Boolean;
begin
  Result := GetCollectionProperties(TCollectionProperty.IsReadOnly);
end;

procedure TdxEMFObjectCollection.FetchAll;
begin
  if IsLoading then
  begin
    if Loader <> nil then
      TdxDataLoaderAccess(Loader).BeginLoad;
    while LoadNextObject do;
  end;
end;

function TdxEMFObjectCollection.IndexOf(AValue: TObject): Integer;
begin
  Result := List.IndexOf(AValue);
end;

procedure TdxEMFObjectCollection.Insert(AIndex: Integer; AObject: TObject);
begin
  FList.Insert(AIndex, AObject);
  if (Helper <> nil) and (AObject <> nil) then
    Helper.Add(AObject);
end;

function TdxEMFObjectCollection.InternalAdd(AObject: TObject): Integer;
begin
  Result := FList.IndexOf(AObject);
  if Result >= 0 then
    Exit;
  if Helper <> nil then
    Helper.Add(AObject)
  else if IsEntity and (Session <> nil) then
  begin
    if Session.IsNewObject(AObject) then
      Session.Save(AObject);
  end;
  Result := InternalAppend(AObject);
end;

function TdxEMFObjectCollection.InternalGetEnumerator: IEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TdxEMFObjectCollection.CheckDataType;
begin
  SetCollectionProperties(TCollectionProperty.IsEntity,
    IsClassEntity(CollectionElementClass));
end;

procedure TdxEMFObjectCollection.Clear;
begin
  FList.Clear;
end;

function TdxEMFObjectCollection.Last: TObject;
begin
  if Count > 0 then
    Result := List[Count - 1]
  else
    Result := nil;
end;

procedure TdxEMFObjectCollection.Load;
begin
  PrepareLoad;
  DoFetchAll;
end;

function TdxEMFObjectCollection.LoadNextObject: Boolean;
var
  AObject: TObject;
begin
  if Loader = nil then
  begin
    if IsLoading then
      PrepareLoad;
    if Loader = nil then
      Exit(False);
    TdxDataLoaderAccess(Loader).BeginLoad;
  end;
  AObject := TdxDataLoaderAccess(Loader).GetNextObject;
  Result := AObject <> nil;
  if Result then
    if Helper = nil then
      InternalAppend(AObject)
    else
      InternalAdd(AObject)
  else
    Loader := nil;
end;

procedure TdxEMFObjectCollection.Remove(AObject: TObject);
var
  APool: TdxCustomEntityPool;
  AIsNewOrDeletedObject: Boolean;
begin
  DoFetchAll;
  if Session <> nil then
  begin
    APool := TdxEMFCustomSessionAccess(Session).PoolClasses.GetPool
      (CollectionElementClass);
    AIsNewOrDeletedObject := APool.IsNewObject(AObject);
  end
  else
    AIsNewOrDeletedObject := False;
  if (Helper <> nil) and not AIsNewOrDeletedObject then
  begin
    if not((Session <> nil) and Session.IsObjectToDelete(AObject)) then
      Helper.Remove(AObject);
  end;
  FList.Extract(AObject);
  if (Helper <> nil) and not AIsNewOrDeletedObject then
    TdxEMFCollectionHelperAccess(Helper).BeforeAfterRemove(AObject);
  if DeleteObjectOnRemove and not AIsNewOrDeletedObject then
    if Session = nil then
    begin
      if (Helper <> nil) and (Helper is TdxReferencedCollectionHelperOneToMany)
      then
        raise EdxEMFException.CreateFmt
          (sdxSessionCannotDeletedNotAssociatedObject,
          [AObject.ClassName, TdxReferencedCollectionHelperOneToMany(Helper)
          .OwnerObject.ClassName])
    end
    else
      Session.Delete(AObject);
end;

procedure TdxEMFObjectCollection.SetDeleteObjectOnRemove(Value: Boolean);
begin
  SetCollectionProperties(TCollectionProperty.DeleteObjectOnRemove, Value);
end;

{ TdxEMFEntityCollection.TEnumerator }

constructor TdxEMFObjectCollection.TEnumerator.Create
  (ACollection: TdxEMFObjectCollection);
begin
  inherited Create;
  FCollection := ACollection;
  FIndex := -1;
end;

function TdxEMFObjectCollection.TEnumerator.GetCurrent: TObject;
begin
  if FIndex >= 0 then
    Result := FCollection.List[FIndex]
  else
    Result := nil;
end;

function TdxEMFObjectCollection.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

procedure TdxEMFObjectCollection.TEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TdxEMFEntityCollection<T> }

procedure TdxEMFEntityCollection<T>.CheckDataType;
begin
  if not IsClassEntity(TClass(T)) then
    raise EdxNoEntityInfoException.Create('');
  inherited CheckDataType;
end;

function TdxEMFEntityCollection<T>.Contains(const AValue: T): Boolean;
begin
  Result := inherited Contains(TObject(AValue));
end;

function TdxEMFEntityCollection<T>.Add(const AValue: T): Integer;
begin
  Result := inherited Add(AValue);
end;

function TdxEMFEntityCollection<T>.GetCollectionElementClass: TClass;
begin
  Result := TClass(T);
end;

procedure TdxEMFEntityCollection<T>.Remove(const AValue: T);
begin
  inherited Remove(AValue);
end;

procedure TdxEMFEntityCollection<T>.SetCollectionElementClass
  (const Value: TClass);
begin
  raise ENotImplemented.Create('');
end;

function TdxEMFEntityCollection<T>.Find(APredicate: TdxPredicate<T>): T;
var
  AInnerList: IdxEMFCollection<T>;
  AItem: T;
begin
  AInnerList := Self;
  for AItem in AInnerList do
    if APredicate(AItem) then
      Exit(AItem);
  Result := nil;
end;

function TdxEMFEntityCollection<T>.Find(const AKeys: array of TValue): T;
var
  AList: IdxEMFCollection<T>;
  AObjectFinder: TdxObjectFinder;
  AItem: T;
begin
  AObjectFinder := ObjectFinder(T);
  AList := Self;
  for AItem in AList do
    if AObjectFinder.HasKey(AItem, AKeys) then
      Exit(AItem);
  Result := nil;
end;

function TdxEMFEntityCollection<T>.Find<TKey>(const AKey: TKey): T;
var
  AList: IdxEMFCollection<T>;
  AObjectFinder: TdxObjectFinder;
  AItem: T;
  AValue: TKey;
begin
  AObjectFinder := ObjectFinder(T);
  AList := Self;
  for AItem in AList do
    if AObjectFinder.HasKey<TKey>(AItem, AKey) then
      Exit(AItem);
  Result := nil;
end;

function TdxEMFEntityCollection<T>.First: T;
begin
  Result := T(inherited First);
end;

function TdxEMFEntityCollection<T>.Find(const AKey: TValue): T;
var
  AList: IdxEMFCollection<T>;
  AObjectFinder: TdxObjectFinder;
  AItem: T;
begin
  AObjectFinder := ObjectFinder(T);
  AList := Self;
  for AItem in AList do
    if AObjectFinder.HasKey(AItem, AKey) then
      Exit(AItem);
  Result := nil;
end;

function TdxEMFEntityCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  DoFetchAll;
  Result := TEnumerator.Create(Self);
end;

function TdxEMFEntityCollection<T>.GetItems(AIndex: Integer): T;
begin
  Result := T(List[AIndex]);
end;

function TdxEMFEntityCollection<T>.GetObjects(APredicate: TdxPredicate<T>)
  : TArray<T>;
var
  AInnerList: IdxEMFCollection<T>;
  AList: TdxArray<T>;
  AItem: T;
begin
  AInnerList := Self;
  AList := TdxArray<T>.Create(Count);
  for AItem in AInnerList do
    if APredicate(AItem) then
      AList.Add(AItem);
  Result := AList.ToArray;
end;

function TdxEMFEntityCollection<T>.GetProvider: IdxQueryProvider<T>;
begin
  Result := Session.GetQueryProvider<T>;
end;

function TdxEMFEntityCollection<T>.IndexOf(const AItem: T): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

procedure TdxEMFEntityCollection<T>.Insert(AIndex: Integer; const AItem: T);
begin
  inherited Insert(AIndex, AItem);
end;

function TdxEMFEntityCollection<T>.InternalGetEnumerator: IEnumerator;
begin
  Result := GetEnumerator;
end;

function TdxEMFEntityCollection<T>.Last: T;
begin
  Result := T(inherited Last);
end;

{ TdxEMFEntityCollection<T>.TEnumerator<T> }

constructor TdxEMFEntityCollection<T>.TEnumerator.Create
  (ACollection: TdxEMFEntityCollection<T>);
begin
  inherited Create;
  FCollection := ACollection;
  FIndex := -1;
end;

function TdxEMFEntityCollection<T>.TEnumerator.GetCurrent: TObject;
begin
  Result := GetCurrentGeneric;
end;

function TdxEMFEntityCollection<T>.TEnumerator.GetCurrentGeneric: T;
begin
  if FIndex >= 0 then
    Result := T(FCollection.List[FIndex])
  else
    Result := nil;
end;

function TdxEMFEntityCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

procedure TdxEMFEntityCollection<T>.TEnumerator.Reset;
begin
  FIndex := -1;
end;

end.
