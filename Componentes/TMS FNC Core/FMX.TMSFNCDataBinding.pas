{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCDataBinding;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, FMX.TMSFNCPersistence, FMX.TMSFNCTypes, FMX.TMSFNCCustomComponent,
  FMX.Controls, TypInfo
  {$IFNDEF LCLWEBLIB}
  , Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  , fgl
  {$ENDIF}

  {$IFDEF LCLWEBLIB}
  , DB
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  , Data.DB
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Fixed : Issue with incorrect Owner being passed to runtime editor

type
  {$IFNDEF WEBLIB}
  ITMSFNCDataBinderBase = interface
    ['{778B63C9-34E3-4B65-A6B8-85E3EB1D17C3}']
    procedure DataBeginUpdate;
    procedure DataEndUpdate;
  end;

  ITMSFNCDataBinderGrid = interface(ITMSFNCDataBinderBase)
    ['{D23BDEAA-49B1-451A-9401-0D0D11A9957A}']
    procedure SetDataColumnCount(AValue: Integer);
    procedure SetDataRowCount(AValue: Integer);
    procedure ClearData;
    function GetDataRowCount: Integer;
    procedure SetDataValue(AColumn, ARow: Integer; AValue: string);
    procedure SetDataHeader(AColumn: Integer; AValue: string);
    procedure DataInsertRow(AInsertPosition: Integer);
  end;
  {$ENDIF}

  ITMSFNCDataBinderSelection = interface
    ['{F7D3F4D2-F202-48C0-9036-3F03E642F1E5}']
    function DataGetItemIndex: Integer;
    procedure DataSetItemIndex(AValue: Integer);
  end;

  TTMSFNCDataBinder = class;

  TTMSFNCDataBinderItem = class;

  TTMSFNCDataBinderDataLinkMode = (dlmActiveChanged, dlmDataSetChanged, dlmDataSetScrolled, dlmRecordChanged, dlmUpdateData);

  TTMSFNCDataBinderDataLink = class(TDataLink)
  private
    FDataBinderItem: TTMSFNCDataBinderItem;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(ADataBinderItem: TTMSFNCDataBinderItem);
    destructor Destroy; override;
  end;

  TTMSFNCDataBinderBindType = (dbbtSingleValue, dbbtList, dbbtColumnList, dbbtGrid);

  TTMSFNCDataBinderPropertyName = class(TCollectionItem)
  private
    FDataBinderItem: TTMSFNCDataBinderItem;
    FValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataBinderItem: TTMSFNCDataBinderItem read FDataBinderItem;
  published
    property Value: string read FValue write SetValue;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCDataBinderPropertyNames = class(TTMSFNCOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCDataBinderPropertyNames = class({$IFDEF LCLLIB}specialize {$ENDIF}TTMSFNCOwnedCollection<TTMSFNCDataBinderPropertyName>)
  {$ENDIF}
  private
    FOwner: TTMSFNCDataBinderItem;
    function GetItemEx(Index: Integer): TTMSFNCDataBinderPropertyName;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCDataBinderPropertyName);
  protected
    function GetDataBinderPropertyNameClass: TCollectionItemClass; virtual;
  public
    constructor Create(AOwner: TTMSFNCDataBinderItem);
    function Add: TTMSFNCDataBinderPropertyName;
    function Insert(index: Integer): TTMSFNCDataBinderPropertyName;
    property Items[Index: Integer]: TTMSFNCDataBinderPropertyName read GetItemEx write SetItemEx; default;
  end;

  TTMSFNCDataBinderFieldName = class(TCollectionItem)
  private
    FDataBinderItem: TTMSFNCDataBinderItem;
    FValue: string;
    FHTMLTemplate: string;
    procedure SetValue(const Value: string);
    procedure SetHTMLTemplate(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataBinderItem: TTMSFNCDataBinderItem read FDataBinderItem;
  published
    property Value: string read FValue write SetValue;
    property HTMLTemplate: string read FHTMLTemplate write SetHTMLTemplate;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCDataBinderFieldNames = class(TTMSFNCOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCDataBinderFieldNames = class({$IFDEF LCLLIB}specialize {$ENDIF}TTMSFNCOwnedCollection<TTMSFNCDataBinderFieldName>)
  {$ENDIF}
  private
    FOwner: TTMSFNCDataBinderItem;
    function GetItemEx(Index: Integer): TTMSFNCDataBinderFieldName;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCDataBinderFieldName);
  protected
    function GetDataBinderFieldNameClass: TCollectionItemClass; virtual;
  public
    constructor Create(AOwner: TTMSFNCDataBinderItem);
    function Add: TTMSFNCDataBinderFieldName;
    function Insert(index: Integer): TTMSFNCDataBinderFieldName;
    property Items[Index: Integer]: TTMSFNCDataBinderFieldName read GetItemEx write SetItemEx; default;
  end;

  TTMSFNCDataBinderFindType = (bftDontCare, bftClass, bftString);

  TTMSFNCDataBinderItem = class(TCollectionItem)
  private
    FDirty: Boolean;
    FUpdating: Boolean;
    FDataBinder: TTMSFNCDataBinder;
    FDataLink: TTMSFNCDataBinderDataLink;
    FActive: Boolean;
    FBindType: TTMSFNCDataBinderBindType;
    FObject: TObject;
    FPropertyName: string;
    FFieldName: string;
    FSubPropertyNames: TTMSFNCDataBinderPropertyNames;
    FSubFieldNames: TTMSFNCDataBinderFieldNames;
    FColumnsPropertyName: string;
    FColumnsSubPropertyName: string;
    FBufferCount: Integer;
    FBooleanStringValue: string;
    FHTMLTemplate: string;
    FName: string;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetActive(const Value: Boolean);
    procedure SetBindType(const Value: TTMSFNCDataBinderBindType);
    procedure SetObject(const Value: TObject);
    procedure SetPropertyName(const Value: string);
    procedure SetFieldName(const Value: string);
    procedure SetColumnsPropertyName(const Value: string);
    procedure SetColumnsSubPropertyName(const Value: string);
    procedure SetBufferCount(const Value: Integer);
    function GetBindComponent: TComponent;
    procedure SetBindComponent(const Value: TComponent);
    procedure SetBooleanStringValue(const Value: string);
    procedure SetHTMLTemplate(const Value: string);
    function GetObjectInstance: TObject;
    procedure SetObjectInstance(const Value: TObject);
  protected
    procedure DoChanged(Sender: TObject);
    property BufferCount: Integer read FBufferCount write SetBufferCount default 0;
    function HTMLDBReplace(AValue: string; ADataset: TDataSet): string; virtual;
    function GetDisplayName: string; override;
    function InternalFindProperty(AObject: TObject; AName: string; AFindType: TTMSFNCDataBinderFindType = bftDontCare): TTMSFNCPropertyInfo;
    function FindProperty(var AObject: TObject; AName: string; AIndex: Integer = -1; AFindType: TTMSFNCDataBinderFindType = bftDontCare): TTMSFNCPropertyInfo;
    procedure DoBeforeWriteFieldToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField; var AAllow: Boolean); virtual;
    procedure DoAfterWriteFieldToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField); virtual;
    procedure DoBeforeWritePropertyToField(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField; var AAllow: Boolean); virtual;
    procedure DoAfterWritePropertyToField(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField); virtual;
    procedure DoBeforeWriteHTMLTemplateToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; var AHTMLTemplate: string; var AAllow: Boolean); virtual;
    procedure DoAfterWriteHTMLTemplateToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AHTMLTemplate: string); virtual;
    procedure WriteFieldToProperty(AObject: TObject; APropInfo: TTMSFNCPropertyInfo; AField: TField); virtual;
    procedure WritePropertyToField(AObject: TObject; APropInfo: TTMSFNCPropertyInfo; AField: TField); virtual;
    procedure WriteHTMLTemplateToProperty(AObject: TObject; APropInfo: TTMSFNCPropertyInfo; AHTMLTemplate: string); virtual;
    procedure ClearProperty(AObject: TObject; APropInfo: TTMSFNCPropertyInfo);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CheckDataSet: Boolean;
    function GetRecordCount: Integer;
    property DataLink: TTMSFNCDataBinderDataLink read FDataLink;
    procedure UpdateDataLink(AMode: TTMSFNCDataBinderDataLinkMode; ADistance: Integer = -1; AField: TField = nil);
    procedure WriteValueToField;
    property &Object: TObject read FObject write SetObject;
    property ObjectInstance: TObject read GetObjectInstance write SetObjectInstance;
    function DataSetLocate(AKeyFieldNames: string; AKeyValues: string): Boolean;
    function DataSetIsEditing: Boolean;
    function DataSetIsInserting: Boolean;
    procedure DataSetEdit;
    procedure DataSetPost;
    procedure DataSetInsert;
    procedure DataSetDelete;
    procedure DataSetCancel;
    procedure DataSetEnableControls;
    procedure DataSetDisableControls;
    function DataSetCanModify: Boolean;
  published
    property BindComponent: TComponent read GetBindComponent write SetBindComponent;
    property PropertyName: string read FPropertyName write SetPropertyName;
    property ColumnsPropertyName: string read FColumnsPropertyName write SetColumnsPropertyName;
    property ColumnsSubPropertyName: string read FColumnsSubPropertyName write SetColumnsSubPropertyName;
    property SubPropertyNames: TTMSFNCDataBinderPropertyNames read FSubPropertyNames write FSubPropertyNames;
    property SubFieldNames: TTMSFNCDataBinderFieldNames read FSubFieldNames write FSubFieldNames;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Active: Boolean read FActive write SetActive default True;
    property BindType: TTMSFNCDataBinderBindType read FBindType write SetBindType default dbbtSingleValue;
    property FieldName: string read FFieldName write SetFieldName;
    property HTMLTemplate: string read FHTMLTemplate write SetHTMLTemplate;
    property BooleanStringValue: string read FBooleanStringValue write SetBooleanStringValue;
    property Name: string read FName write FName;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCDataBinderItems = class(TTMSFNCOwnedCollection)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCDataBinderItems = class({$IFDEF LCLLIB}specialize {$ENDIF}TTMSFNCOwnedCollection<TTMSFNCDataBinderItem>)
  {$ENDIF}
  private
    FOwner: TTMSFNCDataBinder;
    function GetItemEx(Index: Integer): TTMSFNCDataBinderItem;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCDataBinderItem);
  protected
    function GetDataBinderItemClass: TCollectionItemClass; virtual;
  public
    constructor Create(AOwner: TTMSFNCDataBinder);
    function Add: TTMSFNCDataBinderItem;
    function Insert(index: Integer): TTMSFNCDataBinderItem;
    property Items[Index: Integer]: TTMSFNCDataBinderItem read GetItemEx write SetItemEx; default;
  end;

  TTMSFNCDataBinderStringArray = array of string;

  TTMSFNCDataBinderPropertyList =  array of TTMSFNCPropertyInfo;

  TTMSFNCDataBinderBeforeWriteFieldToPropertyEvent = procedure(Sender: TObject; AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField; var AAllow: Boolean) of object;
  TTMSFNCDataBinderAfterWriteFieldToPropertyEvent = procedure(Sender: TObject; AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField) of object;
  TTMSFNCDataBinderBeforeWritePropertyToFieldEvent = procedure(Sender: TObject; AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField; var AAllow: Boolean) of object;
  TTMSFNCDataBinderAfterWritePropertyToFieldEvent = procedure(Sender: TObject; AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField) of object;
  TTMSFNCDataBinderBeforeWriteHTMLTemplateToPropertyEvent = procedure(Sender: TObject; AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; var AHTMLTemplate: string; var AAllow: Boolean) of object;
  TTMSFNCDataBinderAfterWriteHTMLTemplateToPropertyEvent = procedure(Sender: TObject; AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AHTMLTemplate: string) of object;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCDataBinder = class(TTMSFNCCustomComponent)
  private
    FUpdateCount: Integer;
    FItems: TTMSFNCDataBinderItems;
    FActive: Boolean;
    FOnAfterWriteFieldToProperty: TTMSFNCDataBinderAfterWriteFieldToPropertyEvent;
    FOnBeforeWriteFieldToProperty: TTMSFNCDataBinderBeforeWriteFieldToPropertyEvent;
    FOnAfterWritePropertyToField: TTMSFNCDataBinderAfterWritePropertyToFieldEvent;
    FOnBeforeWritePropertyToField: TTMSFNCDataBinderBeforeWritePropertyToFieldEvent;
    FOnAfterWriteHTMLTemplateToProperty: TTMSFNCDataBinderAfterWriteHTMLTemplateToPropertyEvent;
    FOnBeforeWriteHTMLTemplateToProperty: TTMSFNCDataBinderBeforeWriteHTMLTemplateToPropertyEvent;
    procedure SetItems(const Value: TTMSFNCDataBinderItems);
    function GetItems: TTMSFNCDataBinderItems;
    procedure SetActive(const Value: Boolean);
    function GetItemByName(AName: string): TTMSFNCDataBinderItem;
  protected
    function GetDocURL: string; override;
    function GetInstance: NativeUInt; override;
    function GetVersion: string; override;
    function CreateItems: TTMSFNCDataBinderItems; virtual;
    procedure DoBeforeWriteFieldToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField; var AAllow: Boolean); virtual;
    procedure DoAfterWriteFieldToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField); virtual;
    procedure DoBeforeWritePropertyToField(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField; var AAllow: Boolean); virtual;
    procedure DoAfterWritePropertyToField(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AField: TField); virtual;
    procedure DoBeforeWriteHTMLTemplateToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; var AHTMLTemplate: string; var AAllow: Boolean); virtual;
    procedure DoAfterWriteHTMLTemplateToProperty(AObject: TObject; AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string; APropertyKind: TTypeKind; AHTMLTemplate: string); virtual;
    procedure RegisterRuntimeClasses; override;
    procedure UpdateDataLinks;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    function ItemCount: Integer;
    property ItemByName[AName: string]: TTMSFNCDataBinderItem read GetItemByName;
    procedure GetDataSources(AStrings: TStrings);
    class function GetPropertyList(AObject: TObject): TTMSFNCDataBinderPropertyList;
    class procedure BlobFieldToPicture(AField: TBlobField; ABitmap: TTMSFNCBitmap);
    function IsConnected(AObject: TObject): Boolean;
    function ConnectSingleHTMLTemplate(AObject: TObject; ADataSource: TDataSource; APropertyName: string; AHTMLTemplate: string): TTMSFNCDataBinderItem;
    function ConnectSingle(AObject: TObject; ADataSource: TDataSource; APropertyName: string; AFieldName: string): TTMSFNCDataBinderItem;
    function ConnectListHTMLTemplate(AObject: TObject; ADataSource: TDataSource; APropertyName: string; ASubPropertyNames: TTMSFNCDataBinderStringArray; ASubHTMLTemplates: TTMSFNCDataBinderStringArray): TTMSFNCDataBinderItem; overload;
    function ConnectList(AObject: TObject; ADataSource: TDataSource; APropertyName: string; ASubPropertyNames: TTMSFNCDataBinderStringArray; ASubFieldNames: TTMSFNCDataBinderStringArray): TTMSFNCDataBinderItem; overload;
    function ConnectListHTMLTemplate(AObject: TObject; ADataSource: TDataSource; APropertyName: string; AHTMLTemplate: string): TTMSFNCDataBinderItem; overload;
    function ConnectList(AObject: TObject; ADataSource: TDataSource; APropertyName: string; AFieldName: string): TTMSFNCDataBinderItem; overload;
    function ConnectColumnList(AObject: TObject; ADataSource: TDataSource; AListPropertyName: string; AColumnsPropertyName: string; AColumnsSubPropertyName: string; AListSubPropertyName: string): TTMSFNCDataBinderItem;
    function Connect(AObject: TObject; ADataSource: TDataSource; APropertyName: string; AFieldName: string = ''; AHTMLTemplate: string = ''; ASubPropertyNames: TTMSFNCDataBinderStringArray = nil; ASubFieldNames: TTMSFNCDataBinderStringArray = nil; ASubHTMLTemplates: TTMSFNCDataBinderStringArray = nil; ABindType: TTMSFNCDataBinderBindType = dbbtSingleValue): TTMSFNCDataBinderItem;
    function ConnectGrid(AObject: TObject; ADatasource: TDataSource): TTMSFNCDataBinderItem;
    procedure StartEditor(const AObject: TObject = nil);
  published
    property Items: TTMSFNCDataBinderItems read GetItems write SetItems;
    property Version: string read GetVersion;
    property Active: Boolean read FActive write SetActive default False;
    property OnBeforeWriteFieldToProperty: TTMSFNCDataBinderBeforeWriteFieldToPropertyEvent read FOnBeforeWriteFieldToProperty write FOnBeforeWriteFieldToProperty;
    property OnAfterWriteFieldToProperty: TTMSFNCDataBinderAfterWriteFieldToPropertyEvent read FOnAfterWriteFieldToProperty write FOnAfterWriteFieldToProperty;
    property OnBeforeWritePropertyToField: TTMSFNCDataBinderBeforeWritePropertyToFieldEvent read FOnBeforeWritePropertyToField write FOnBeforeWritePropertyToField;
    property OnAfterWritePropertyToField: TTMSFNCDataBinderAfterWritePropertyToFieldEvent read FOnAfterWritePropertyToField write FOnAfterWritePropertyToField;
    property OnBeforeWriteHTMLTemplateToProperty: TTMSFNCDataBinderBeforeWriteHTMLTemplateToPropertyEvent read FOnBeforeWriteHTMLTemplateToProperty write FOnBeforeWriteHTMLTemplateToProperty;
    property OnAfterWriteHTMLTemplateToProperty: TTMSFNCDataBinderAfterWriteHTMLTemplateToPropertyEvent read FOnAfterWriteHTMLTemplateToProperty write FOnAfterWriteHTMLTemplateToProperty;
  end;

implementation

uses
  FMX.Graphics, FMX.Forms, FMX.TMSFNCUtils, FMX.TMSFNCCustomControl, SysUtils,
  FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes, FMX.TMSFNCDataBindingEditor
  {$IFDEF VCLLIB}
  , JPEG
  {$ENDIF}
  ;

{$R 'TMSFNCDataBinding.res'}

var
  FGlobalUpdating: Boolean;

{ TTMSFNCDataBinderItem }

procedure TTMSFNCDataBinderItem.Assign(Source: TPersistent);
begin
  FActive := (Source as TTMSFNCDataBinderItem).Active;
  FSubPropertyNames.Assign((Source as TTMSFNCDataBinderItem).SubPropertyNames);
  FSubFieldNames.Assign((Source as TTMSFNCDataBinderItem).SubFieldNames);
  FBindType := (Source as TTMSFNCDataBinderItem).BindType;
  FFieldName := (Source as TTMSFNCDataBinderItem).FieldName;
  FHTMLTemplate := (Source as TTMSFNCDataBinderItem).HTMLTemplate;
  FPropertyName := (Source as TTMSFNCDataBinderItem).PropertyName;
  FObject := (Source as TTMSFNCDataBinderItem).&Object;
  FColumnsPropertyName := (Source as TTMSFNCDataBinderItem).ColumnsPropertyName;
  DataSource := (Source as TTMSFNCDataBinderItem).DataSource;
end;

procedure TTMSFNCDataBinderItem.DataSetCancel;
begin
  if CheckDataSet then
    DataLink.DataSet.Cancel;
end;

function TTMSFNCDataBinderItem.DataSetCanModify: Boolean;
begin
  Result := False;
  if CheckDataSet then
    Result := DataLink.DataSet.CanModify;
end;

function TTMSFNCDataBinderItem.CheckDataSet: Boolean;
begin
  Result := Active and Assigned(FDataLink.DataSet) and (FDataLink.DataSet.Active);
end;

procedure TTMSFNCDataBinderItem.ClearProperty(AObject: TObject;
  APropInfo: TTMSFNCPropertyInfo);
var
  pName, cn, cnv: string;
  k: TTypeKind;
  p: TTMSFNCPropertyInfo;
  o: TObject;
  i: Integer;
  s: string;
  b: Boolean;
  d: Double;
  ii: Int64;
begin
  o := AObject;
  p := APropInfo;
  pName := TTMSFNCPersistence.GetPropInfoName(p);
  k := TTMSFNCPersistence.GetPropInfoType(p);

  case k of
    tkInteger:
    begin
      cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') or (cn = 'TTMSFNCGraphicsColor') then
      begin
        cnv := '';
        if not TTMSFNCPersistence.IsReadOnly(p) then
        begin
          if cnv = 'gcNull' then
            SetOrdProp(o, pName, gcNull)
          else
            SetOrdProp(o, pName, TTMSFNCGraphics.HTMLToColor(cnv));
        end;
      end
      else
      begin
        i := 0;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, i);
      end;
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}:
    begin
      s := '';
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetStrProp(o, p, s);
    end;
    tkEnumeration:
      if TTMSFNCPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
      begin
        b := False;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, Integer(b))
      end
      else
      begin
        i := 0;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, i);
      end;
    {$IFDEF LCLWEBLIB}
    tkBool:
    begin
      b := False;
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, Integer(b))
    end;
    {$ENDIF}
    tkFloat:
    begin
      d := 0;
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetFloatProp(o, p, d)
    end;
    {$IFNDEF WEBLIB}
    tkInt64:
    begin
      ii := 0;
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, ii)
    end;
    {$ENDIF}
    tkSet:
    begin
      i := 0;
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, i);
    end;
  end;
end;

constructor TTMSFNCDataBinderItem.Create(Collection: TCollection);
begin
  inherited;
  FDirty := False;
  FActive := True;
  FBufferCount := 0;
  FDataBinder := (Collection as TTMSFNCDataBinderItems).FOwner;
  FDataLink := TTMSFNCDataBinderDataLink.Create(Self);
  FSubPropertyNames := TTMSFNCDataBinderPropertyNames.Create(Self);
  FSubFieldNames := TTMSFNCDataBinderFieldNames.Create(Self);
end;

procedure TTMSFNCDataBinderItem.DataSetDelete;
begin
  if CheckDataSet then
    DataLink.DataSet.Delete;
end;

procedure TTMSFNCDataBinderItem.DataSetDisableControls;
begin
  if CheckDataSet then
    DataLink.DataSet.DisableControls;
end;

destructor TTMSFNCDataBinderItem.Destroy;
begin
  FDataBinder := nil;
  FSubPropertyNames.Free;
  FSubFieldNames.Free;
  FDataLink.Free;
  inherited;
end;

procedure TTMSFNCDataBinderItem.DoAfterWriteFieldToProperty(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField);
begin
  if Assigned(FDataBinder) then
    FDataBinder.DoAfterWriteFieldToProperty(AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField);
end;

procedure TTMSFNCDataBinderItem.DoAfterWritePropertyToField(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField);
begin
  if Assigned(FDataBinder) then
    FDataBinder.DoAfterWritePropertyToField(AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField);
end;

procedure TTMSFNCDataBinderItem.DoAfterWriteHTMLTemplateToProperty(
  AObject: TObject; AItem: TTMSFNCDataBinderItem;
  APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string;
  APropertyKind: TTypeKind; AHTMLTemplate: string);
begin
  if Assigned(FDataBinder) then
    FDataBinder.DoAfterWriteHTMLTemplateToProperty(AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AHTMLTemplate);
end;

procedure TTMSFNCDataBinderItem.DoBeforeWritePropertyToField(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField;
  var AAllow: Boolean);
begin
  if Assigned(FDataBinder) then
    FDataBinder.DoBeforeWritePropertyToField(AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField, AAllow);
end;

procedure TTMSFNCDataBinderItem.DoBeforeWriteFieldToProperty(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField;
  var AAllow: Boolean);
begin
  if Assigned(FDataBinder) then
    FDataBinder.DoBeforeWriteFieldToProperty(AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField, AAllow);
end;

procedure TTMSFNCDataBinderItem.DoBeforeWriteHTMLTemplateToProperty(
  AObject: TObject; AItem: TTMSFNCDataBinderItem;
  APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string;
  APropertyKind: TTypeKind; var AHTMLTemplate: string; var AAllow: Boolean);
begin
  if Assigned(FDataBinder) then
    FDataBinder.DoBeforeWriteHTMLTemplateToProperty(AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AHTMLTemplate, AAllow);
end;

procedure TTMSFNCDataBinderItem.DoChanged(Sender: TObject);
begin
  UpdateDataLink(dlmActiveChanged);
end;

procedure TTMSFNCDataBinderItem.DataSetEdit;
begin
  if CheckDataSet then
    DataLink.DataSet.Edit;
end;

procedure TTMSFNCDataBinderItem.DataSetEnableControls;
begin
  if CheckDataSet then
    DataLink.DataSet.EnableControls;
end;

procedure TTMSFNCDataBinderItem.DataSetInsert;
begin
  if CheckDataSet then
    DataLink.DataSet.Insert;
end;

function TTMSFNCDataBinderItem.DataSetIsEditing: Boolean;
begin
  Result := False;
  if CheckDataSet then
    Result := (DataLink.DataSet.State = dsEdit);
end;

function TTMSFNCDataBinderItem.DataSetIsInserting: Boolean;
begin
  Result := False;
  if CheckDataSet then
    Result := (DataLink.DataSet.State = dsInsert);
end;

function TTMSFNCDataBinderItem.InternalFindProperty(AObject: TObject;
  AName: string; AFindType: TTMSFNCDataBinderFindType = bftDontCare): TTMSFNCPropertyInfo;
var
  {$IFNDEF WEBLIB}
  ci: Pointer;
  c: Integer;
  pl: PPropList;
  {$ENDIF}
  {$IFDEF WEBLIB}
  ci: TTypeInfoClass;
  a: TTypeMemberPropertyDynArray;
  {$ENDIF}
  p: TTMSFNCPropertyInfo;
  pName: string;
  I: Integer;
  k: TTypeKind;
begin
  Result := nil;
  if Assigned(AObject) then
  begin
    {$IFNDEF WEBLIB}
    ci := AObject.ClassInfo;
    c := GetPropList(ci, tkProperties, nil);
    GetMem(pl, c * SizeOf(TTMSFNCPropertyInfo));
    {$ENDIF}
    {$IFDEF WEBLIB}
    ci := TypeInfo(AObject);
    {$ENDIF}
    try
      {$IFNDEF WEBLIB}
      GetPropList(ci, tkProperties, pl);
      for I := 0 to c - 1 do
      begin
        p := pl^[i];
      {$ENDIF}
      {$IFDEF WEBLIB}
      a := GetPropList(ci, tkProperties);
      for I := 0 to Length(a) - 1 do
      begin
        p := a[I];
      {$ENDIF}

        pName := TTMSFNCPersistence.GetPropInfoName(p);
        k := TTMSFNCPersistence.GetPropInfoType(p);
        if (UpperCase(pName) = UpperCase(AName)) and ((AFindType = bftDontCare)
          or ((AFindType = bftClass) and (k = tkClass))
          or ((AFindType = bftString) and (k in [{$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}]))
          ) then
        begin
          Result := p;
          Break;
        end;
      end;

    finally
      {$IFNDEF WEBLIB}
      FreeMem(pl);
      {$ENDIF}
    end;
  end;
end;

function TTMSFNCDataBinderItem.DataSetLocate(AKeyFieldNames, AKeyValues: string): Boolean;
begin
  Result := False;
  if CheckDataSet then
    Result := DataLink.DataSet.Locate(AKeyFieldNames, AKeyValues, []);
end;

procedure TTMSFNCDataBinderItem.DataSetPost;
begin
  if CheckDataSet then
    DataLink.DataSet.Post;
end;

function TTMSFNCDataBinderItem.GetBindComponent: TComponent;
begin
  Result := nil;
  if Assigned(FObject) and (FObject is TComponent) then
    Result := TComponent(FObject);
end;

function TTMSFNCDataBinderItem.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

function TTMSFNCDataBinderItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  {$IFDEF WEBLIB}
  else
    Result := '';
  {$ENDIF}
  {$IFNDEF WEBLIB}
  else
    Result := inherited;
  {$ENDIF}
end;

function TTMSFNCDataBinderItem.GetObjectInstance: TObject;
begin
  Result := &Object;
end;

function TTMSFNCDataBinderItem.GetRecordCount: Integer;

  function RowsInDataSet: Integer;
  var
    cb: TBookMark;
    iseof, isbof: Boolean;
  begin
    Result := -1;
    if not CheckDataSet then
      Exit;

    iseof := DataLink.DataSet.Eof;
    isbof := DataLink.DataSet.Bof;

    if isbof and iseof then
      Result := 0
    else
      with DataLink.DataSet do
      begin
        cb := GetBookMark;

        if BookmarkValid(cb) then
        begin
          First;
          if (csDesigning in ComponentState) then
            Result := MoveBy(100)
          else
            Result := MoveBy($7FFFFFFF) + 1;

          GotoBookMark(cb);
        end;

        FreeBookMark(cb);
      end;

    if iseof then
      DataLink.DataSet.Next;

    if isbof then
      DataLink.DataSet.Prior;

    if DataLink.DataSet.State = dsInsert then
      Result := Result + 1;
  end;
begin
  Result := 0;
  if Assigned(FDataLink) then
  begin
    if Assigned(DataLink.DataSet) then
    begin
      if DataLink.DataSet.Active then
      begin
        DataLink.DataSet.DisableControls;
//        if Assigned(OnGetRecordCount) then
//          OnGetRecordCount(Self, Result)
//        else
        begin
          Result := RowsInDataSet;
          if Result = -1 then
            Result := DataLink.RecordCount;
        end;
        DataLink.DataSet.EnableControls;
      end;
    end;
  end;
end;

function TTMSFNCDataBinderItem.HTMLDBReplace(AValue: string; ADataset: TDataSet): string;
var
  beforetag, aftertag, fld, dbfld: string;
  i, j: integer;
  AField: TField;
begin
  beforetag := '';

  while Pos('<#', AValue) > 0 do
  begin
    i := pos('<#', AValue);
    beforetag := beforetag + copy(AValue, 1, i - 1); //part prior to the tag
    aftertag := copy(AValue, i, length(AValue)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(AValue, 1, i + j - 1);

    dbfld := '';
    if Assigned(ADataSet) then
    begin
      if ADataSet.Active then
      begin
        AField := ADataSet.FindField(fld);
        if Assigned(AField) then
        begin
          if AField.IsBlob then
          begin
            if Assigned(AField.OnGetText) then
              dbfld := AField.DisplayText
            else
              dbfld := AField.AsString;
          end
          else
           dbfld := AField.DisplayText;
        end;

//        if Assigned(FOnGetHTMLTemplateData) then
//          FOnGetHTMLTemplateData(Self, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else
      dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + AValue;
end;

function TTMSFNCDataBinderItem.FindProperty(var AObject: TObject;
  AName: string; AIndex: Integer = -1; AFindType: TTMSFNCDataBinderFindType = bftDontCare): TTMSFNCPropertyInfo;
var
  sl: TStringList;
  I: Integer;
  p: TTMSFNCPropertyInfo;
  o: TObject;
begin
  o := AObject;
  Result := nil;
  if not Assigned(o) then
    Exit;

  sl := TStringList.Create;
  try
    TTMSFNCUtils.Split('.', AName, sl);
    for I := 0 to sl.Count - 1 do
    begin
      p := InternalFindProperty(o, sl[I], AFindType);
      if Assigned(p) then
      begin
        if (I = 0) and (sl.Count > 1) and (TTMSFNCPersistence.GetPropInfoType(p) = tkClass) then
        begin
          o := GetObjectProp(o, p);
          if o is TCollection then
          begin
            if (AIndex >= 0) and (AIndex <= (o as TCollection).Count - 1) then
              o := (o as TCollection).Items[AIndex]
            else
              o := (o as TCollection).Add;
          end;
        end
        else
        begin
          Result := p;
          Break;
        end;
      end;
    end;          

    AObject := o;        
  finally
    sl.Free;
  end;
end;

procedure TTMSFNCDataBinderItem.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetBindComponent(const Value: TComponent);
begin
  &Object := Value;
end;

procedure TTMSFNCDataBinderItem.SetBooleanStringValue(const Value: string);
begin
  if FBooleanStringValue <> Value then
  begin
    FBooleanStringValue := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetBufferCount(const Value: Integer);
begin
  if FBufferCount <> Value then
  begin
    FBufferCount := Value;
    if Assigned(DataLink) then
      DataLink.BufferCount := FBufferCount;

    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetColumnsPropertyName(const Value: string);
begin
  if FColumnsPropertyName <> Value then
  begin
    FColumnsPropertyName := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetColumnsSubPropertyName(const Value: string);
begin
  if FColumnsSubPropertyName <> Value then
  begin
    FColumnsSubPropertyName := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetDataSource(const Value: TDataSource);
begin
  if Value = DataLink.Datasource then
    Exit;

  DataLink.DataSource := Value;

  if not Assigned(DataLink.DataSource) then
  begin
    FSubFieldNames.Clear;
    FSubPropertyNames.Clear;
    FFieldName := '';
    UpdateDataLink(dlmActiveChanged);
  end;

  if Value <> nil then
    Value.FreeNotification(FDataBinder);
end;

procedure TTMSFNCDataBinderItem.SetBindType(const Value: TTMSFNCDataBinderBindType);
begin
  if FBindType <> Value then
  begin
    FBindType := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    UpdateDataLink(dlmUpdateData);
  end;
end;

procedure TTMSFNCDataBinderItem.SetHTMLTemplate(const Value: string);
begin
  if FHTMLTemplate <> Value then
  begin
    FHTMLTemplate := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetObject(const Value: TObject);
begin
  if FObject <> Value then
  begin
    FObject := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.SetObjectInstance(const Value: TObject);
begin
  &Object := Value;
end;

procedure TTMSFNCDataBinderItem.SetPropertyName(const Value: string);
begin
  if FPropertyName <> Value then
  begin
    FPropertyName := Value;
    UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderItem.UpdateDataLink(AMode: TTMSFNCDataBinderDataLinkMode; ADistance: Integer = -1; AField: TField = nil);
var
  p, pcol, psub: TTMSFNCPropertyInfo;
  f: TField;
  h: string;
  k: TTypeKind;
  o, oc, no: TObject;
  its: TCollection;
  sl: TStrings;
  gls: TTMSFNCStringList;
  gli: TTMSFNCIntegerList;
  itv, it, cit: TObject;
  col: TCollection;
  colit: TObject;
  I, J, L: Integer;
  cb: TBookmark;
  ai: Integer;
  dbb: ITMSFNCDataBinderBase;
  dbg: ITMSFNCDataBinderGrid;
  em, um: Boolean;

  procedure UpdateItem(AItem: TObject);
  var
    kj: Integer;
  begin
    for kj := 0 to SubPropertyNames.Count - 1 do
    begin
      if (kj >= 0) and (kj <= SubFieldNames.Count - 1) then
      begin
        f := nil;
        if SubFieldNames[kj].Value <> '' then
          f := DataLink.DataSet.FieldByName(SubFieldNames[kj].Value);

        if Assigned(f) then
        begin
          itv := AItem;
          psub := FindProperty(itv, SubPropertyNames[kj].Value, kj);
          if Assigned(psub) then
            WriteFieldToProperty(itv, psub, f);
        end
        else if SubFieldNames[kj].HTMLTemplate <> '' then
        begin
          itv := AItem;
          psub := FindProperty(itv, SubPropertyNames[kj].Value, kj);
          if Assigned(psub) then
            WriteHTMLTemplateToProperty(itv, psub, SubFieldNames[kj].HTMLTemplate);
        end;
      end
      else if FieldName <> '' then
      begin
        f := DataLink.DataSet.FieldByName(FieldName);
        if Assigned(f) then
        begin
          itv := AItem;
          psub := FindProperty(itv, SubPropertyNames[kj].Value, kj);
          if Assigned(psub) then
            WriteFieldToProperty(itv, psub, f);
        end;
      end
      else if HTMLTemplate <> '' then
      begin
        itv := AItem;
        psub := FindProperty(itv, SubPropertyNames[kj].Value, kj);
        if Assigned(psub) then
          WriteHTMLTemplateToProperty(itv, psub, HTMLTemplate);
      end;
    end;
  end;

  procedure DoBeginUpdate;
  begin
    FUpdating := True;
    FGlobalUpdating := True;
    if no is TTMSFNCCustomComponent then
      (no as TTMSFNCCustomComponent).BeginUpdate
    else if no is TTMSFNCCustomControl then
      (no as TTMSFNCCustomControl).BeginUpdate
    else if Supports(no, ITMSFNCDataBinderGrid, dbg) then
      dbg.DataBeginUpdate
    else if Supports(no, ITMSFNCDataBinderBase, dbb) then
      dbb.DataBeginUpdate;
  end;

  procedure DoEndUpdate;
  begin
    if no is TTMSFNCCustomComponent then
      (no as TTMSFNCCustomComponent).EndUpdate
    else if no is TTMSFNCCustomControl then
      (no as TTMSFNCCustomControl).EndUpdate
    else if Supports(no, ITMSFNCDataBinderGrid, dbg) then
      dbg.DataEndUpdate
    else if Supports(no, ITMSFNCDataBinderBase, dbb) then
      dbb.DataEndUpdate;

    FUpdating := False;
    FGlobalUpdating := False;
  end;

  procedure SetActiveItemIndex;
  var
    bs: ITMSFNCDataBinderSelection;
  begin
    if Supports(no, ITMSFNCDataBinderSelection, bs) then
    begin
      if not DataSetIsInserting then
        bs.DataSetItemIndex(FDataLink.DataSource.DataSet.RecNo - 1)
      else
        bs.DataSetItemIndex(bs.DataGetItemIndex);
    end;
  end;

  function GetActiveItemIndex: Integer;
  var
    bs: ITMSFNCDataBinderSelection;
  begin
    Result := -1;
    if not DataSetIsInserting then
      Result := FDataLink.DataSource.DataSet.RecNo - 1
    else
    begin
      if Supports(no, ITMSFNCDataBinderSelection, bs) then
        Result := bs.DataGetItemIndex;
    end;
  end;
begin
  if FUpdating or FGlobalUpdating then
    Exit;

  if (csDestroying in FDataBinder.ComponentState) or (FDataBinder.FUpdateCount > 0) then
    Exit;

  if not Assigned(&Object) then
    Exit;

  if (PropertyName = '') and (BindType <> dbbtGrid) then
    Exit;

  p := nil;
  no := &Object;
  if not (BindType = dbbtGrid) then
  begin
    p := FindProperty(no, PropertyName);
    if not Assigned(p) then
      Exit;
  end;

  DoBeginUpdate;

  try
    if CheckDataSet and FDataBinder.Active then
    begin
      if (FDataLink.ReadOnly) and (AMode = dlmUpdateData) then
      begin
        DoEndUpdate;
        Exit;
      end;

      em := (DataLink.DataSet.State in [dsEdit, dsInsert]) and (AMode = dlmDataSetChanged);
      um := (DataLink.DataSet.State in [dsEdit, dsInsert]) and (AMode = dlmUpdateData) and FDirty;
      ai := GetActiveItemIndex;

      case BindType of
        dbbtSingleValue:
        begin
          if FieldName <> '' then
          begin
            f := DataLink.DataSet.FieldByName(FieldName);
            if Assigned(f) then
            begin
              if um then
              begin
                WritePropertyToField(no, p, f);
                FDirty := False;
              end
              else if em then
                ClearProperty(no, p)
              else
                WriteFieldToProperty(no, p, f);
            end;
          end
          else if (HTMLTemplate <> '') and (AMode <> dlmUpdateData) then
            WriteHTMLTemplateToProperty(no, p, HTMLTemplate);
        end;
        dbbtColumnList:
        begin
          case AMode of
            dlmDataSetChanged, dlmActiveChanged:
            begin
              k := TTMSFNCPersistence.GetPropInfoType(p);
              case k of
                tkClass:
                begin
                  o := GetObjectProp(no, p);
                  pcol := FindProperty(no, ColumnsPropertyName);
                  oc := nil;
                  if Assigned(pcol) then
                    oc := GetObjectProp(no, pcol);

                  if Assigned(oc) then
                  begin
                    its := nil;
                    if Assigned(o) and (o is TCollection) then
                    begin
                      its := o as TCollection;
                      if em then
                      begin
                        if ai > -1 then
                        begin
                          its.Insert(ai);
                          SetActiveItemIndex;
                        end;
                        DoEndUpdate;
                        Exit;
                      end;
                      its.Clear;
                    end;

                    if oc is TCollection then
                    begin
                      col := oc as TCollection;
                      col.Clear;

                      for I := 0 to DataLink.DataSet.FieldCount - 1 do
                      begin
                        colit := col.Add;
                        f := DataLink.DataSet.Fields[I];
                        if Assigned(f) then
                        begin
                          if ColumnsSubPropertyName <> '' then
                          begin
                            psub := FindProperty(colit, ColumnsSubPropertyName, -1, bftString);
                            if Assigned(psub) then
                              SetStrProp(colit, psub, f.FieldName);
                          end;
                        end;
                      end;

                      if Assigned(its) then
                      begin
                        DataLink.DataSet.DisableControls;
                        cb := DataLink.DataSet.GetBookmark;
                        DataLink.DataSet.First;
                        try
                          while not DataLink.DataSet.Eof do
                          begin
                            it := its.Add;
                            cit := it;
                            for I := 0 to DataLink.DataSet.FieldCount - 1 do
                            begin
                              f := DataLink.DataSet.Fields[I];
                              if Assigned(f) then
                              begin
                                for J := 0 to SubPropertyNames.Count - 1 do
                                begin
                                  itv := cit;
                                  {$IFNDEF WEBLIB}
                                  if f.DataType in [ftGraphic, ftBlob, ftTypedBinary] then
                                    psub := FindProperty(itv, SubPropertyNames[J].Value, -1, bftClass)
                                  else
                                  {$ENDIF}
                                    psub := FindProperty(itv, SubPropertyNames[J].Value);

                                  if Assigned(psub) then
                                    WriteFieldToProperty(itv, psub, f);
                                end;
                              end;
                            end;

                            DataLink.DataSet.Next;
                          end;
                        finally
                          DataLink.DataSet.GotoBookmark(cb);
                          DataLink.DataSet.FreeBookmark(cb);
                          DataLink.DataSet.EnableControls;
                        end;

                        SetActiveItemIndex;
                      end;
                    end;
                  end;
                end;
              end;
            end;
            dlmDataSetScrolled: SetActiveItemIndex;
            dlmRecordChanged:
            begin
              k := TTMSFNCPersistence.GetPropInfoType(p);
              case k of
                tkClass:
                begin
                  o := GetObjectProp(no, p);
                  pcol := FindProperty(no, ColumnsPropertyName);
                  oc := nil;
                  if Assigned(pcol) then
                    oc := GetObjectProp(no, pcol);

                  if Assigned(oc) then
                  begin
                    its := nil;
                    if Assigned(o) and (o is TCollection) then
                      its := o as TCollection;

                    if oc is TCollection then
                    begin
                      col := oc as TCollection;

                      for I := 0 to DataLink.DataSet.FieldCount - 1 do
                      begin
                        if (I >= 0) and (I <= col.Count - 1) then
                        begin
                          colit := col.Items[I];
                          f := DataLink.DataSet.Fields[I];
                          if Assigned(f) then
                          begin
                            if ColumnsSubPropertyName <> '' then
                            begin
                              psub := FindProperty(colit, ColumnsSubPropertyName, 0, bftString);
                              if Assigned(psub) then
                                SetStrProp(colit, psub, f.FieldName);
                            end;
                          end;
                        end;
                      end;

                      if Assigned(its) then
                      begin
                        try
                          if (ai >= 0) and (ai <= its.Count - 1) then
                          begin
                            it := its.Items[ai];
                            cit := it;
                            for I := 0 to DataLink.DataSet.FieldCount - 1 do
                            begin
                              f := DataLink.DataSet.Fields[I];
                              if Assigned(f) then
                              begin
                                for J := 0 to SubPropertyNames.Count - 1 do
                                begin
                                  itv := cit;
                                  {$IFNDEF WEBLIB}
                                  if f.DataType in [ftGraphic, ftBlob, ftTypedBinary] then
                                    psub := FindProperty(itv, SubPropertyNames[J].Value, I, bftClass)
                                  else
                                  {$ENDIF}
                                    psub := FindProperty(itv, SubPropertyNames[J].Value, I);

                                  if Assigned(psub) then
                                    WriteFieldToProperty(itv, psub, f);
                                end;
                              end;
                            end;
                          end;
                        finally
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
            dlmUpdateData: ;
          end;
        end;
        dbbtGrid:
        begin
          case AMode of
            dlmDataSetChanged, dlmActiveChanged:
            begin
              if Supports(no, ITMSFNCDataBinderGrid, dbg) then
              begin
                if em then
                begin
                  if ai > -1 then
                  begin
                    dbg.DataInsertRow(ai);
                    SetActiveItemIndex;
                  end;
                  DoEndUpdate;
                  Exit;
                end;

                dbg.SetDataColumnCount(DataLink.DataSet.FieldCount);

                for I := 0 to DataLink.DataSet.FieldCount - 1 do
                begin
                  f := DataLink.DataSet.Fields[I];
                  if Assigned(f) then
                    dbg.SetDataHeader(I, f.FieldName)
                end;

                DataLink.DataSet.DisableControls;
                cb := DataLink.DataSet.GetBookmark;
                DataLink.DataSet.First;
                try
                  L := 0;
                  while not DataLink.DataSet.Eof do
                  begin
                    for I := 0 to DataLink.DataSet.FieldCount - 1 do
                    begin
                      f := DataLink.DataSet.Fields[I];
                      if Assigned(f) then
                        dbg.SetDataValue(I, L, f.DisplayText);
                    end;

                    DataLink.DataSet.Next;
                    Inc(L);
                  end;

                  dbg.SetDataRowCount(L);
                finally
                  DataLink.DataSet.GotoBookmark(cb);
                  DataLink.DataSet.FreeBookmark(cb);
                  DataLink.DataSet.EnableControls;
                end;

                SetActiveItemIndex;
              end;
            end;
            dlmDataSetScrolled: SetActiveItemIndex;
            dlmRecordChanged:
            begin
              if Supports(no, ITMSFNCDataBinderGrid, dbg) then
              begin
                dbg.SetDataColumnCount(DataLink.DataSet.FieldCount);

                for I := 0 to DataLink.DataSet.FieldCount - 1 do
                begin
                  f := DataLink.DataSet.Fields[I];
                  if Assigned(f) then
                    dbg.SetDataHeader(I, f.FieldName)
                end;

                try
                  if (ai >= 0) and (ai <= dbg.GetDataRowCount - 1) then
                  begin
                    for I := 0 to DataLink.DataSet.FieldCount - 1 do
                    begin
                      f := DataLink.DataSet.Fields[I];
                      if Assigned(f) then
                        dbg.SetDataValue(I, ai, f.DisplayText);
                    end;
                  end;
                finally
                end;
              end;
            end;
            dlmUpdateData: ;
          end;
        end;
        dbbtList:
        begin
          case AMode of
            dlmDataSetChanged, dlmActiveChanged:
            begin
              if (SubFieldNames.Count > 0) or (FieldName <> '') or (HTMLTemplate <> '') then
              begin
                k := TTMSFNCPersistence.GetPropInfoType(p);
                case k of
                  tkClass:
                  begin
                    o := GetObjectProp(no, p);
                    if Assigned(o) then
                    begin
                      its := nil;
                      sl := nil;
                      gls := nil;
                      gli := nil;

                      if o is TCollection then
                      begin
                        its := (o as TCollection);
                        if em then
                        begin
                          if ai > -1 then
                          begin
                            its.Insert(ai);
                            SetActiveItemIndex;
                          end;
                          DoEndUpdate;
                          Exit;
                        end;

                        its.Clear;
                      end
                      else if o is TStrings then
                      begin
                        sl := (o as TStrings);

                        if em then
                        begin
                          if ai > -1 then
                          begin
                            sl.Insert(ai, '');
                            SetActiveItemIndex;
                          end;
                          DoEndUpdate;
                          Exit;
                        end;

                        sl.Clear;
                      end
                      else if TTMSFNCPersistence.IsGenericList(o.ClassType, 'String') then
                      begin
                        gls := TTMSFNCStringList(o);

                        if em then
                        begin
                          if ai > -1 then
                          begin
                            gls.Insert(ai, '');
                            SetActiveItemIndex;
                          end;
                          DoEndUpdate;
                          Exit;
                        end;

                        gls.Clear;
                      end
                      else if TTMSFNCPersistence.IsGenericList(o.ClassType, 'Integer') then
                      begin
                        gli := TTMSFNCIntegerList(o);

                        if em then
                        begin
                          if ai > -1 then
                          begin
                            gli.Insert(ai, 0);
                            SetActiveItemIndex;
                          end;
                          DoEndUpdate;
                          Exit;
                        end;

                        gli.Clear;
                      end;

                      DataLink.DataSet.DisableControls;
                      cb := DataLink.DataSet.GetBookmark;
                      DataLink.DataSet.First;

                      try
                        while not DataLink.DataSet.Eof do
                        begin
                          if Assigned(its) then
                          begin
                            it := its.Add;
                            UpdateItem(it);
                          end
                          else if (SubFieldNames.Count > 0) or (FieldName <> '') or (HTMLTemplate <> '') then
                          begin
                            if (SubFieldNames.Count > 0) and (SubFieldNames[0].Value <> '') then
                              f := DataLink.DataSet.FieldByName(SubFieldNames[0].Value)
                            else
                              f := DataLink.DataSet.FieldByName(FieldName);

                            if Assigned(f) then
                            begin
                              if Assigned(sl) then
                                sl.Add(f.AsString)
                              else if Assigned(gls) then
                                gls.Add(f.AsString)
                              else if Assigned(gli) then
                                gli.Add(f.AsInteger);
                            end
                            else
                            begin
                              h := HTMLDBReplace(HTMLTemplate, DataLink.DataSet);
                              if Assigned(sl) then
                                sl.Add(h)
                              else if Assigned(gls) then
                                gls.Add(h);
                            end;
                          end;

                          DataLink.DataSet.Next;
                        end;

                      finally
                        DataLink.DataSet.GotoBookmark(cb);
                        DataLink.DataSet.FreeBookmark(cb);
                        DataLink.DataSet.EnableControls;
                      end;

                      SetActiveItemIndex;
                    end;
                  end;
                end;
              end;
            end;
            dlmDataSetScrolled: SetActiveItemIndex;
            dlmRecordChanged:
            begin
              if (SubFieldNames.Count > 0) or (FieldName <> '') or (HTMLTemplate <> '') then
              begin
                k := TTMSFNCPersistence.GetPropInfoType(p);
                case k of
                  tkClass:
                  begin
                    o := GetObjectProp(no, p);
                    if Assigned(o) then
                    begin
                      its := nil;
                      sl := nil;
                      gls := nil;
                      gli := nil;

                      if o is TCollection then
                        its := (o as TCollection)
                      else if o is TStrings then
                        sl := (o as TStrings)
                      else if TTMSFNCPersistence.IsGenericList(o.ClassType, 'String') then
                        gls := TTMSFNCStringList(o)
                      else if TTMSFNCPersistence.IsGenericList(o.ClassType, 'Integer') then
                        gli := TTMSFNCIntegerList(o);

                      try
                        if Assigned(its) then
                        begin
                          if (ai >= 0) and (ai <= its.Count - 1) then
                          begin
                            it := its.Items[ai];
                            UpdateItem(it);
                          end;
                        end
                        else if (SubFieldNames.Count > 0) or (FieldName <> '') or (HTMLTemplate <> '') then
                        begin
                          if (SubFieldNames.Count > 0) and (SubFieldNames[0].Value <> '') then
                            f := DataLink.DataSet.FieldByName(SubFieldNames[0].Value)
                          else
                            f := DataLink.DataSet.FieldByName(FieldName);

                          if Assigned(f) then
                          begin
                            if Assigned(sl) then
                            begin
                              if (ai >= 0) and (ai <= sl.Count - 1) then
                                sl[ai] := f.AsString;
                            end
                            else if Assigned(gls) then
                            begin
                              if (ai >= 0) and (ai <= gls.Count - 1) then
                                gls[ai] := f.AsString;
                            end
                            else if Assigned(gli) then
                            begin
                              if (ai >= 0) and (ai <= gli.Count - 1) then
                                gli[ai] := f.AsInteger;
                            end;
                          end
                          else
                          begin
                            h := HTMLDBReplace(HTMLTemplate, DataLink.DataSet);
                            if Assigned(sl) then
                            begin
                              if (ai >= 0) and (ai <= sl.Count - 1) then
                                sl[ai] := h;
                            end
                            else if Assigned(gls) then
                            begin
                              if (ai >= 0) and (ai <= gls.Count - 1) then
                                gls[ai] := h;
                            end;
                          end;
                        end;
                      finally
                      end;
                    end;
                  end;
                end;
              end;
            end;
            dlmUpdateData: ;
          end;
        end;
      end;
    end
    else
    begin
      case BindType of
        dbbtSingleValue: ClearProperty(no, p);
        dbbtColumnList:
        begin
          k := TTMSFNCPersistence.GetPropInfoType(p);
          case k of
            tkClass:
            begin
              o := GetObjectProp(no, p);
              pcol := FindProperty(no, ColumnsPropertyName);
              oc := nil;
              if Assigned(pcol) then
                oc := GetObjectProp(no, pcol);

              if Assigned(o) and (o is TCollection) then
              begin
                its := o as TCollection;
                its.Clear;
              end;

              if Assigned(oc) and (oc is TCollection) then
              begin
                col := oc as TCollection;
                col.Clear;
              end;
            end;
          end;
        end;
        dbbtGrid:
        begin
          if Supports(no, ITMSFNCDataBinderGrid, dbg) then
            dbg.ClearData;
        end;
        dbbtList:
        begin
          k := TTMSFNCPersistence.GetPropInfoType(p);
          case k of
            tkClass:
            begin
              o := GetObjectProp(no, p);
              if Assigned(o) then
              begin
                if o is TCollection then
                begin
                  col := (o as TCollection);
                  col.Clear;
                end
                else if o is TStrings then
                begin
                  sl := (o as TStrings);
                  sl.Clear;
                end
                else if TTMSFNCPersistence.IsGenericList(o.ClassType, 'String') then
                begin
                  gls := TTMSFNCStringList(o);
                  gls.Clear;
                end
                else if TTMSFNCPersistence.IsGenericList(o.ClassType, 'Integer') then
                begin
                  gli := TTMSFNCIntegerList(o);
                  gli.Clear;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

  finally
    DoEndUpdate;
  end;
end;

procedure TTMSFNCDataBinderItem.WriteFieldToProperty(AObject: TObject;
  APropInfo: TTMSFNCPropertyInfo; AField: TField);
var
  a: Boolean;
  pName, cn, cnv, en: string;
  k: TTypeKind;
  p: TTMSFNCPropertyInfo;
  o: TObject;
  i: Integer;
  s: string;
  b: Boolean;
  d: Double;
  ii: Int64;
  ob: TObject;
  bt: TTMSFNCBitmap;
begin
  if not Assigned(AField) or not Assigned(AObject) or not Assigned(APropInfo) then
    Exit;

  o := AObject;
  p := APropInfo;
  pName := TTMSFNCPersistence.GetPropInfoName(p);
  k := TTMSFNCPersistence.GetPropInfoType(p);

  a := True;
  DoBeforeWriteFieldToProperty(o, Self, p, pName, k, AField, a);
  if a then
  begin
    case k of
      tkClass:
      begin
        {$IFNDEF WEBLIB}
        if AField.DataType in [ftGraphic, ftBlob, ftTypedBinary] then
        begin
          cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
          if (cn = 'TBitmap') or (cn = 'TPicture') or (cn = 'TTMSFNCBitmap') then
          begin
            ob := GetObjectProp(AObject, pName);
            bt := TTMSFNCBitmap.Create;
            try
              TTMSFNCDataBinder.BlobFieldToPicture(TBlobField(AField), bt);
              if ob is TPersistent then
                (ob as TPersistent).Assign(bt);
            finally
              bt.Free;
            end;
          end;
        end;
        {$ENDIF}
      end;
      tkInteger:
      begin
        cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
        if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') or (cn = 'TTMSFNCGraphicsColor') then
        begin
          cnv := AField.AsString;
          if not TTMSFNCPersistence.IsReadOnly(p) then
          begin
            if cnv = 'gcNull' then
              SetOrdProp(o, pName, gcNull)
            else
              SetOrdProp(o, pName, TTMSFNCGraphics.HTMLToColor(cnv));
          end;
        end
        else
        begin
          i := AField.AsInteger;
          if not TTMSFNCPersistence.IsReadOnly(p) then
            SetOrdProp(o, p, i);
        end;
      end;
      {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}:
      begin
        s := AField.AsString;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetStrProp(o, p, s);
      end;
      tkEnumeration:
        if TTMSFNCPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
        begin
          if AField.DataType = ftBoolean then
            b := AField.AsBoolean
          else
            b := AField.AsString = BooleanStringValue;

          if not TTMSFNCPersistence.IsReadOnly(p) then
            SetOrdProp(o, p, Integer(b))
        end
        else
        begin
          i := AField.AsInteger;
          if not TTMSFNCPersistence.IsReadOnly(p) then
            SetOrdProp(o, p, i);
        end;
      {$IFDEF LCLWEBLIB}
      tkBool:
      begin
        b := AField.AsBoolean;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, Integer(b))
      end;
      {$ENDIF}
      tkFloat:
      begin
        d := AField.AsFloat;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetFloatProp(o, p, d)
      end;
      {$IFNDEF WEBLIB}
      tkInt64:
      begin
        ii := AField.AsInteger;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, ii)
      end;
      {$ENDIF}
      tkSet:
      begin
        i := AField.AsInteger;
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, i);
      end
      else
      begin
        en := TTMSFNCPersistence.GetEnumName(TypeInfo(TTypeKind), Integer(k));
        //raise ETMSFNCReaderException.CreateFmt('Cannot read property %s with type %s', [pName, en]);
      end;
    end;

    DoAfterWriteFieldToProperty(o, Self, p, pName, k, AField);
  end;
end;

procedure TTMSFNCDataBinderItem.WriteHTMLTemplateToProperty(AObject: TObject;
  APropInfo: TTMSFNCPropertyInfo; AHTMLTemplate: string);
var
  a: Boolean;
  pName: string;
  k: TTypeKind;
  p: TTMSFNCPropertyInfo;
  o: TObject;
  s: string;
  t: string;
begin
  if (AHTMLTemplate = '') or not Assigned(AObject) or not Assigned(APropInfo) then
    Exit;

  o := AObject;
  p := APropInfo;
  pName := TTMSFNCPersistence.GetPropInfoName(p);
  k := TTMSFNCPersistence.GetPropInfoType(p);

  a := True;
  t := AHTMLTemplate;
  DoBeforeWriteHTMLTemplateToProperty(o, Self, p, pName, k, t, a);
  if a then
  begin
    case k of
      {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}:
      begin
        s := HTMLDBReplace(t, DataLink.DataSet);
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetStrProp(o, p, s);
      end;
    end;

    DoAfterWriteHTMLTemplateToProperty(o, Self, p, pName, k, t);
  end;
end;

procedure TTMSFNCDataBinderItem.WritePropertyToField(AObject: TObject;
  APropInfo: TTMSFNCPropertyInfo; AField: TField);
var
  a: Boolean;
  pName, cn, en: string;
  k: TTypeKind;
  p: TTMSFNCPropertyInfo;
  o: TObject;
  b: Boolean;
//  ob: TObject;
  bt: TTMSFNCBitmap;
begin
  if not Assigned(AField) or not Assigned(AObject) or not Assigned(APropInfo) then
    Exit;

  if not AField.CanModify then
    Exit;

  o := AObject;
  p := APropInfo;
  pName := TTMSFNCPersistence.GetPropInfoName(p);
  k := TTMSFNCPersistence.GetPropInfoType(p);

  a := True;
  DoBeforeWritePropertyToField(o, Self, p, pName, k, AField, a);
  if a then
  begin
    case k of
      tkClass:
      begin
        {$IFNDEF WEBLIB}
        if AField.DataType in [ftGraphic, ftBlob, ftTypedBinary] then
        begin
          cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
          if (cn = 'TBitmap') or (cn = 'TPicture') or (cn = 'TTMSFNCBitmap') then
          begin
//            ob := GetObjectProp(AObject, pName);
            bt := TTMSFNCBitmap.Create;
            try
              raise Exception.Create('Write picture to blob field');
//              TTMSFNCDataBinder.BlobFieldToPicture(TBlobField(AField), bt);
//              if ob is TPersistent then
//                (ob as TPersistent).Assign(bt);
            finally
              bt.Free;
            end;
          end;
        end;
        {$ENDIF}
      end;
      tkInteger:
      begin
        cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
        if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') or (cn = 'TTMSFNCGraphicsColor') then
        begin
          if not TTMSFNCPersistence.IsWriteOnly(p) then
            AField.AsString := TTMSFNCGraphics.ColorToHTML(GetOrdProp(o, pName));
        end
        else
        begin
          if not TTMSFNCPersistence.IsWriteOnly(p) then
            AField.AsInteger := GetOrdProp(o, p);
        end;
      end;
      {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}:
      begin
        if not TTMSFNCPersistence.IsWriteOnly(p) then
          AField.AsString := GetStrProp(o, p);
      end;
      tkEnumeration:
        if TTMSFNCPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
        begin
          if not TTMSFNCPersistence.IsReadOnly(p) then
          begin
            b := Boolean(GetOrdProp(o, p));
            if AField.DataType = ftBoolean then
            begin
              AField.AsBoolean := b;
            end
            else
            begin
              if b then
                AField.AsString := BooleanStringValue
              else
                AField.AsString := ''; //TODO
            end;
          end;
        end
        else
        begin
          if not TTMSFNCPersistence.IsWriteOnly(p) then
            AField.AsInteger := GetOrdProp(o, p);
        end;
      {$IFDEF LCLWEBLIB}
      tkBool:
      begin
        if not TTMSFNCPersistence.IsWriteOnly(p) then
          AField.AsBoolean := Boolean(GetOrdProp(o, p))
      end;
      {$ENDIF}
      tkFloat:
      begin
        if not TTMSFNCPersistence.IsWriteOnly(p) then
          AField.AsFloat := GetFloatProp(o, p)
      end;
      {$IFNDEF WEBLIB}
      tkInt64:
      begin
        if not TTMSFNCPersistence.IsWriteOnly(p) then
          AField.AsInteger := GetOrdProp(o, p)
      end;
      {$ENDIF}
      tkSet:
      begin
        if not TTMSFNCPersistence.IsWriteOnly(p) then
          AField.AsInteger := GetOrdProp(o, p);
      end
      else
      begin
        en := TTMSFNCPersistence.GetEnumName(TypeInfo(TTypeKind), Integer(k));
        //raise ETMSFNCReaderException.CreateFmt('Cannot read property %s with type %s', [pName, en]);
      end;
    end;

    DoAfterWritePropertyToField(o, Self, p, pName, k, AField);
  end;
end;

procedure TTMSFNCDataBinderItem.WriteValueToField;
begin
  if CheckDataSet and (DataSetIsEditing or DataSetIsInserting) then
  begin
    FDirty := True;
    UpdateDataLink(dlmUpdateData);
  end;
end;

{ TTMSFNCDataBinderFieldNames }

function TTMSFNCDataBinderFieldNames.Add: TTMSFNCDataBinderFieldName;
begin
  Result := TTMSFNCDataBinderFieldName(inherited Add);
end;

constructor TTMSFNCDataBinderFieldNames.Create(AOwner: TTMSFNCDataBinderItem);
begin
  inherited Create(AOwner, GetDataBinderFieldNameClass);
  FOwner := AOwner;
end;

function TTMSFNCDataBinderFieldNames.GetDataBinderFieldNameClass: TCollectionItemClass;
begin
  Result := TTMSFNCDataBinderFieldName;
end;

function TTMSFNCDataBinderFieldNames.GetItemEx(Index: Integer): TTMSFNCDataBinderFieldName;
begin
  Result := TTMSFNCDataBinderFieldName(inherited Items[Index]);
end;

function TTMSFNCDataBinderFieldNames.Insert(index: Integer): TTMSFNCDataBinderFieldName;
begin
  Result := TTMSFNCDataBinderFieldName(inherited Insert(Index));
end;

procedure TTMSFNCDataBinderFieldNames.SetItemEx(Index: Integer; const Value: TTMSFNCDataBinderFieldName);
begin
  inherited SetItem(Index, Value);
end;

{ TTMSFNCDataBinderPropertyNames }

function TTMSFNCDataBinderPropertyNames.Add: TTMSFNCDataBinderPropertyName;
begin
  Result := TTMSFNCDataBinderPropertyName(inherited Add);
end;

constructor TTMSFNCDataBinderPropertyNames.Create(AOwner: TTMSFNCDataBinderItem);
begin
  inherited Create(AOwner, GetDataBinderPropertyNameClass);
  FOwner := AOwner;
end;

function TTMSFNCDataBinderPropertyNames.GetDataBinderPropertyNameClass: TCollectionItemClass;
begin
  Result := TTMSFNCDataBinderPropertyName;
end;

function TTMSFNCDataBinderPropertyNames.GetItemEx(Index: Integer): TTMSFNCDataBinderPropertyName;
begin
  Result := TTMSFNCDataBinderPropertyName(inherited Items[Index]);
end;

function TTMSFNCDataBinderPropertyNames.Insert(index: Integer): TTMSFNCDataBinderPropertyName;
begin
  Result := TTMSFNCDataBinderPropertyName(inherited Insert(Index));
end;

procedure TTMSFNCDataBinderPropertyNames.SetItemEx(Index: Integer; const Value: TTMSFNCDataBinderPropertyName);
begin
  inherited SetItem(Index, Value);
end;


{ TTMSFNCDataBinderItems }

function TTMSFNCDataBinderItems.Add: TTMSFNCDataBinderItem;
begin
  Result := TTMSFNCDataBinderItem(inherited Add);
end;

constructor TTMSFNCDataBinderItems.Create(AOwner: TTMSFNCDataBinder);
begin
  inherited Create(AOwner, GetDataBinderItemClass);
  FOwner := AOwner;
end;

function TTMSFNCDataBinderItems.GetDataBinderItemClass: TCollectionItemClass;
begin
  Result := TTMSFNCDataBinderItem;
end;

function TTMSFNCDataBinderItems.GetItemEx(Index: Integer): TTMSFNCDataBinderItem;
begin
  Result := TTMSFNCDataBinderItem(inherited Items[Index]);
end;

function TTMSFNCDataBinderItems.Insert(index: Integer): TTMSFNCDataBinderItem;
begin
  Result := TTMSFNCDataBinderItem(inherited Insert(Index));
end;

procedure TTMSFNCDataBinderItems.SetItemEx(Index: Integer; const Value: TTMSFNCDataBinderItem);
begin
  inherited SetItem(Index, Value);
end;

{ TTMSFNCDataBinder }

procedure TTMSFNCDataBinder.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

class procedure TTMSFNCDataBinder.BlobFieldToPicture(AField: TBlobField;
  ABitmap: TTMSFNCBitmap);
{$IFDEF FMXLIB}
type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Integer              { Size not including header }
  end;
{$ENDIF}
var
  ms: TMemoryStream;
{$IFDEF FMXLIB}
  Size: Longint;
  Header: TBytes;
  GraphicHeader: TGraphicHeader;
{$ENDIF}
{$IFDEF CMNLIB}
  gcc: TGraphicClass;
  p: TGraphic;
  sig: word;
  i: Integer;
  bmp: TBitmap;
  pic: TPicture;
  oletype: Integer;
  oleoffset: Integer;
  b: Byte;
  ljpg: TJPEGImage;
{$ENDIF}
begin
  {$IFDEF CMNLIB}
  ms := TMemoryStream.Create;
  try
    AField.SaveToStream(ms);
    ms.Position := 0;
    oletype := -1;
    oleoffset := 0;
    if ms.Size > 2 then
    begin
      ms.Position := 0;
      sig := 0;
      ms.Read(sig, 2);
      case sig of
        $1C15: // OLE storage
        begin
          i := 0;
          b := 0;
          while (i < 512) do
          begin
            ms.Read(b, 1);
            inc(i);
            if (b = $FF) then
            begin
              ms.Read(b, 1);
              inc(i);
              if b = $D8 then
              begin
                oletype := 1;
                oleoffset := i;
                break;
              end;
            end;
            if (b = $47) then
            begin
              ms.Read(b, 1);
              inc(i);
              if b = $49 then
              begin
                oletype := 2;
                oleoffset := i;
                break;
              end;
            end;
            if (b = ord('B')) then
            begin
              ms.Read(b, 1);
              inc(i);
              if (b = ord('M')) then
              begin
                oletype := 0;
                oleoffset := i;
                Break;
              end;
            end;
          end;
          ms.Seek(oleoffset, 0);
          case oletype of
            0:
            begin
              bmp := TBitmap.Create;
              bmp.LoadFromStream(ms);
              ABitmap.Assign(bmp);
              bmp.Free;
            end;
            1:
            begin
              LJPg := TJPEGImage.Create;
              try
                ljpg.LoadFromStream(ms);
                pic := TPicture.Create;
                pic.Assign(ljpg);
                ABitmap.Assign(pic);
                pic.Free;
              finally
                FreeAndNil(ljpg);
              end;
            end;
            2:
            begin
              ms := TMemoryStream.Create;
              ms.CopyFrom(ms, ms.Size - ms.Position);
              ABitmap.Graphic.LoadFromStream(ms);
              ms.Free;
            end;
          end;
        end;
        else
        begin
          ms.Position := 0;
          if TTMSFNCUtils.FindGraphicClass(ms.Memory^, ms.Size, gcc) then
          begin
            p := gcc.Create;
            try
              ms.Position := 0;
              p.LoadFromStream(ms);
              ABitmap.Assign(p)
            finally
              p.Free;
            end;
          end
          else
            ABitmap.Assign(AField);
        end;
      end;
    end;
  finally
    ms.Free;
  end;
  {$ENDIF}
  {$IFDEF FMXLIB}
  ms := TMemoryStream.Create;
  try
    AField.SaveToStream(ms);
    ms.Position := 0;
    Size := ms.Size;
    if Size >= SizeOf(TGraphicHeader) then
    begin
      SetLength(Header, SizeOf(TGraphicHeader));
      ms.Read(Header, 0, Length(Header));
      System.Move(Header[0], GraphicHeader, SizeOf(TGraphicHeader));
      if (GraphicHeader.Count <> 1) or (GraphicHeader.HType <> $0100) or
        (GraphicHeader.Size <> Size - SizeOf(GraphicHeader)) then
        ms.Position := 0;
    end;

    ABitmap.LoadFromStream(ms);
  finally
    ms.Free;
  end;
  {$ENDIF}
end;

function TTMSFNCDataBinder.Connect(AObject: TObject; ADataSource: TDataSource; APropertyName: string; AFieldName: string = '';
  AHTMLTemplate: string = ''; ASubPropertyNames: TTMSFNCDataBinderStringArray = nil;
  ASubFieldNames: TTMSFNCDataBinderStringArray = nil; ASubHTMLTemplates: TTMSFNCDataBinderStringArray = nil;
  ABindType: TTMSFNCDataBinderBindType = dbbtSingleValue): TTMSFNCDataBinderItem;
var
  I: Integer;
begin
  Result := FItems.Add;
  Result.DataSource := ADataSource;
  Result.BindType := ABindType;
  Result.FieldName := AFieldName;
  Result.&Object := AObject;
  Result.PropertyName := APropertyName;
  Result.HTMLTemplate := AHTMLTemplate;

  if Assigned(ASubPropertyNames) then
  begin
    for I := 0 to Length(ASubPropertyNames) - 1 do
      Result.SubPropertyNames.Add.Value := ASubPropertyNames[I];
  end;

  if Assigned(ASubFieldNames) then
  begin
    for I := 0 to Length(ASubFieldNames) - 1 do
      Result.SubFieldNames.Add.Value := ASubFieldNames[I];
  end;

  if Assigned(ASubHTMLTemplates) then
  begin
    for I := 0 to Length(ASubHTMLTemplates) - 1 do
      Result.SubFieldNames.Add.HTMLTemplate := ASubHTMLTemplates[I];
  end;
end;

function TTMSFNCDataBinder.ConnectColumnList(AObject: TObject;
  ADataSource: TDataSource; AListPropertyName, AColumnsPropertyName,
  AColumnsSubPropertyName: string; AListSubPropertyName: string): TTMSFNCDataBinderItem;
begin
  Result := FItems.Add;
  Result.DataSource := ADataSource;
  Result.BindType := dbbtColumnList;
  Result.ColumnsPropertyName := AColumnsPropertyName;
  Result.ColumnsSubPropertyName := AColumnsSubPropertyName;
  Result.&Object := AObject;
  Result.PropertyName := AListPropertyName;
  Result.SubPropertyNames.Add.Value := AListSubPropertyName;
end;

function TTMSFNCDataBinder.ConnectGrid(AObject: TObject;
  ADatasource: TDataSource): TTMSFNCDataBinderItem;
begin
  Result := FItems.Add;
  Result.DataSource := ADataSource;
  Result.BindType := dbbtGrid;
  Result.&Object := AObject;
end;

function TTMSFNCDataBinder.ConnectList(AObject: TObject;
  ADataSource: TDataSource; APropertyName, AFieldName: string): TTMSFNCDataBinderItem;
begin
  Result := Connect(AObject, ADataSource, APropertyName, AFieldName, '', nil, nil, nil, dbbtList);
end;

function TTMSFNCDataBinder.ConnectListHTMLTemplate(AObject: TObject;
  ADataSource: TDataSource; APropertyName: string; ASubPropertyNames,
  ASubHTMLTemplates: TTMSFNCDataBinderStringArray): TTMSFNCDataBinderItem;
begin
  Result := Connect(AObject, ADataSource, APropertyName, '', '', ASubPropertyNames, nil, ASubHTMLTemplates, dbbtList);
end;

function TTMSFNCDataBinder.ConnectListHTMLTemplate(AObject: TObject;
  ADataSource: TDataSource; APropertyName,
  AHTMLTemplate: string): TTMSFNCDataBinderItem;
begin
  Result := Connect(AObject, ADataSource, APropertyName, '', AHTMLTemplate, nil, nil, nil, dbbtList);
end;

function TTMSFNCDataBinder.ConnectList(AObject: TObject;
  ADataSource: TDataSource; APropertyName: string; ASubPropertyNames,
  ASubFieldNames: TTMSFNCDataBinderStringArray): TTMSFNCDataBinderItem;
begin
  Result := Connect(AObject, ADataSource, APropertyName, '', '', ASubPropertyNames, ASubFieldNames, nil, dbbtList);
end;

function TTMSFNCDataBinder.ConnectSingle(AObject: TObject;
  ADataSource: TDataSource; APropertyName, AFieldName: string): TTMSFNCDataBinderItem;
begin
  Result := Connect(AObject, ADataSource, APropertyName, AFieldName);
end;

function TTMSFNCDataBinder.ConnectSingleHTMLTemplate(AObject: TObject;
  ADataSource: TDataSource; APropertyName,
  AHTMLTemplate: string): TTMSFNCDataBinderItem;
begin
  Result := Connect(AObject, ADataSource, APropertyName, '', AHTMLTemplate);
end;

constructor TTMSFNCDataBinder.Create(AOwner: TComponent);
begin
  inherited;
  FItems := CreateItems;
end;

constructor TTMSFNCDataBinder.Create;
begin
  Create(nil);
end;

function TTMSFNCDataBinder.CreateItems: TTMSFNCDataBinderItems;
begin
  Result := TTMSFNCDataBinderItems.Create(Self);
end;

destructor TTMSFNCDataBinder.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TTMSFNCDataBinder.DoAfterWritePropertyToField(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField);
begin
  if Assigned(OnAfterWritePropertyToField) then
    OnAfterWritePropertyToField(Self, AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField);
end;

procedure TTMSFNCDataBinder.DoAfterWriteFieldToProperty(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField);
begin
  if Assigned(OnAfterWriteFieldToProperty) then
    OnAfterWriteFieldToProperty(Self, AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField);
end;

procedure TTMSFNCDataBinder.DoAfterWriteHTMLTemplateToProperty(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AHTMLTemplate: string);
begin
  if Assigned(OnAfterWriteHTMLTemplateToProperty) then
    OnAfterWriteHTMLTemplateToProperty(Self, AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AHTMLTemplate);
end;

procedure TTMSFNCDataBinder.DoBeforeWriteFieldToProperty(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField;
  var AAllow: Boolean);
begin
  if Assigned(OnBeforeWriteFieldToProperty) then
    OnBeforeWriteFieldToProperty(Self, AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField, AAllow);
end;

procedure TTMSFNCDataBinder.DoBeforeWritePropertyToField(AObject: TObject;
  AItem: TTMSFNCDataBinderItem; APropertyInfo: TTMSFNCPropertyInfo;
  APropertyName: string; APropertyKind: TTypeKind; AField: TField;
  var AAllow: Boolean);
begin
  if Assigned(OnBeforeWritePropertyToField) then
    OnBeforeWritePropertyToField(Self, AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AField, AAllow);
end;

procedure TTMSFNCDataBinder.DoBeforeWriteHTMLTemplateToProperty(
  AObject: TObject; AItem: TTMSFNCDataBinderItem;
  APropertyInfo: TTMSFNCPropertyInfo; APropertyName: string;
  APropertyKind: TTypeKind; var AHTMLTemplate: string; var AAllow: Boolean);
begin
  if Assigned(OnBeforeWriteHTMLTemplateToProperty) then
    OnBeforeWriteHTMLTemplateToProperty(Self, AObject, AItem, APropertyInfo, APropertyName, APropertyKind, AHTMLTemplate, AAllow);
end;

procedure TTMSFNCDataBinder.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateDataLinks;
end;

procedure TTMSFNCDataBinder.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    UpdateDataLinks;
  end;
end;

procedure TTMSFNCDataBinder.SetItems(const Value: TTMSFNCDataBinderItems);
begin
  FItems.Assign(Value);
end;

procedure TTMSFNCDataBinder.StartEditor(const AObject: TObject = nil);
var
  e: TTMSFNCDataBindingEditor;
begin
  e := TTMSFNCDataBindingEditor.Create(Self);
  try
    e.DataBinder := Self;
    e.&Object := AObject;
    e.Execute
    {$IFDEF WEBLIB}
    (procedure(AResult: TModalResult)
    begin
      e.Free;
    end)
    {$ENDIF}
    ;
  finally
    {$IFNDEF WEBLIB}
    e.Free;
    {$ENDIF}
  end;
end;

procedure TTMSFNCDataBinder.UpdateDataLinks;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].UpdateDataLink(dlmActiveChanged);
end;

procedure TTMSFNCDataBinder.GetDataSources(AStrings: TStrings);
var
  f: TComponent;
  i: integer;
  K: Integer;
begin
  f := TTMSFNCUtils.GetOwnerForm(Self);
  if Assigned(f) then
  begin
    for i := 0 to f.ComponentCount - 1 do
    begin
      if f.Components[i] is TFrame then
      begin
        for K := 0 to (f.Components[i] as TFrame).ComponentCount - 1 do
        begin
          if ((f.Components[i] as TFrame).Components[K] is TDataSource) then
          begin
            AStrings.AddObject((f.Components[i] as TFrame).Components[K].Name, (f.Components[i] as TFrame).Components[K]);
          end;
        end;
      end
      else
      begin
        if (f.Components[i] is TDataSource) then
        begin
          AStrings.AddObject(f.Components[i].Name, f.Components[i]);
        end;
      end;
    end;
  end;
end;

function TTMSFNCDataBinder.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfnccore/components/ttmsfncdatabinding/';
end;

function TTMSFNCDataBinder.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCDataBinder.GetItemByName(AName: string): TTMSFNCDataBinderItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[I].Name <> '') and (Items[I].Name = AName) then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TTMSFNCDataBinder.GetItems: TTMSFNCDataBinderItems;
begin
  Result := FItems;
end;

class function TTMSFNCDataBinder.GetPropertyList(AObject: TObject): TTMSFNCDataBinderPropertyList;
var
  {$IFNDEF WEBLIB}
  ci: Pointer;
  c: Integer;
  pl: PPropList;
  {$ENDIF}
  {$IFDEF WEBLIB}
  ci: TTypeInfoClass;
  p: TTMSFNCPropertyInfo;
  a: TTypeMemberPropertyDynArray;
  {$ENDIF}
  I: Integer;
begin
  if Assigned(AObject) then
  begin
    {$IFNDEF WEBLIB}
    ci := AObject.ClassInfo;
    c := GetPropList(ci, tkProperties, nil);
    GetMem(pl, c * SizeOf(TTMSFNCPropertyInfo));
    {$ENDIF}
    {$IFDEF WEBLIB}
    ci := TypeInfo(AObject);
    {$ENDIF}
    try
      {$IFNDEF WEBLIB}
      GetPropList(ci, tkProperties, pl);
      for I := 0 to c - 1 do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := pl^[i];
      end;
      {$ENDIF}
      {$IFDEF WEBLIB}
      a := GetPropList(ci, tkProperties);
      for I := 0 to Length(a) - 1 do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := a[i];
      end;
      {$ENDIF}
    finally
      {$IFNDEF WEBLIB}
      FreeMem(pl);
      {$ENDIF}
    end;
  end;
end;

function TTMSFNCDataBinder.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

function TTMSFNCDataBinder.IsConnected(AObject: TObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ItemCount - 1 do
  begin
    if Items[I].&Object = AObject then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTMSFNCDataBinder.ItemCount: Integer;
begin
  Result := Items.Count;
end;

procedure TTMSFNCDataBinder.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin
  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    i := FItems.Count - 1;
    while (i >= 0) do
    begin
      if FItems[i].DataSource = AComponent then
        FItems[i].DataSource := nil;

      if FItems[I].&Object = AComponent then
        FItems[I].&Object := nil;
      Dec(i);
    end;
  end;
  inherited;
end;

procedure TTMSFNCDataBinder.RegisterRuntimeClasses;
begin
  RegisterClass(TTMSFNCDataBinder);
end;

{ TTMSFNCDataBinderDataLink }

procedure TTMSFNCDataBinderDataLink.ActiveChanged;
begin
  FDataBinderItem.UpdateDataLink(dlmActiveChanged);
end;

constructor TTMSFNCDataBinderDataLink.Create(ADataBinderItem: TTMSFNCDataBinderItem);
begin
  FDataBinderItem := ADataBinderItem;
  VisualControl := True;
end;

procedure TTMSFNCDataBinderDataLink.DataSetChanged;
begin
  FDataBinderItem.UpdateDataLink(dlmDataSetChanged);
end;

procedure TTMSFNCDataBinderDataLink.DataSetScrolled(Distance: Integer);
begin
  FDataBinderItem.UpdateDataLink(dlmDataSetScrolled, Distance);
end;

destructor TTMSFNCDataBinderDataLink.Destroy;
begin

  inherited;
end;

procedure TTMSFNCDataBinderDataLink.RecordChanged(Field: TField);
begin
  if Assigned(Field) then
    FDataBinderItem.UpdateDataLink(dlmRecordChanged, -1, Field);
end;

procedure TTMSFNCDataBinderDataLink.UpdateData;
begin
  FDataBinderItem.UpdateDataLink(dlmUpdateData);
end;

{ TTMSFNCDataBinderPropertyName }

procedure TTMSFNCDataBinderPropertyName.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCDataBinderPropertyName then
  begin
    FValue := (Source as TTMSFNCDataBinderPropertyName).Value;
  end;
end;

constructor TTMSFNCDataBinderPropertyName.Create(Collection: TCollection);
begin
  inherited;
  FDataBinderItem := (Collection as TTMSFNCDataBinderPropertyNames).FOwner;
end;

destructor TTMSFNCDataBinderPropertyName.Destroy;
begin

  inherited;
end;

procedure TTMSFNCDataBinderPropertyName.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(FDataBinderItem) then
      FDataBinderItem.UpdateDataLink(dlmActiveChanged);
  end;
end;


{ TTMSFNCDataBinderFieldName }

procedure TTMSFNCDataBinderFieldName.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCDataBinderFieldName then
  begin
    FValue := (Source as TTMSFNCDataBinderFieldName).Value;
    FHTMLTemplate := (Source as TTMSFNCDataBinderFieldName).HTMLTemplate;
  end;
end;

constructor TTMSFNCDataBinderFieldName.Create(Collection: TCollection);
begin
  inherited;
  FDataBinderItem := (Collection as TTMSFNCDataBinderFieldNames).FOwner;
end;

destructor TTMSFNCDataBinderFieldName.Destroy;
begin

  inherited;
end;

procedure TTMSFNCDataBinderFieldName.SetHTMLTemplate(const Value: string);
begin
  if FHTMLTemplate <> Value then
  begin
    FHTMLTemplate := Value;
    if Assigned(FDataBinderItem) then
      FDataBinderItem.UpdateDataLink(dlmActiveChanged);
  end;
end;

procedure TTMSFNCDataBinderFieldName.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(FDataBinderItem) then
      FDataBinderItem.UpdateDataLink(dlmActiveChanged);
  end;
end;

end.
