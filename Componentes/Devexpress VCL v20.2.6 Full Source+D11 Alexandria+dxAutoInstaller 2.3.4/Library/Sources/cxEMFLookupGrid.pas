unit cxEMFLookupGrid;

interface

uses
  Windows,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, DB,
  cxClasses, cxControls, cxGraphics, cxLookAndFeelPainters,
  cxEdit, cxDBEdit, cxCustomData, cxDB, cxDBData, cxEditRepositoryItems,
  cxLookupGrid, cxEMFData, dxEMF.Metadata, cxEMFDataDefinitions;

type

  TcxCustomEMFLookupGrid = class;
  TcxEMFLookupGridDataController = class;

  { TdxEMFLookupGridDataControllerInfo }

  TdxEMFLookupGridDataControllerInfo = class(TdxEMFDataControllerInfo)
  // for internal use
  strict private
    function GetDataController: TcxEMFLookupGridDataController;
  protected
    procedure DoFilter; override;
  public
    property DataController: TcxEMFLookupGridDataController
      read GetDataController;
  end;

  { TcxEMFLookupGridDataController }

  TcxEMFLookupGridDataController = class(TdxEMFDataController)
  // for internal use
  strict private
    function GetGrid: TcxCustomEMFLookupGrid;
  protected
    function CreateDataControllerInfo: TcxCustomDataControllerInfo; override;
    procedure DoIncrementalFiltering; virtual;
  public
    function GetItem(Index: Integer): TObject; override;

    property Grid: TcxCustomEMFLookupGrid read GetGrid;
  end;

  { TcxEMFLookupGridDefaultValuesProvider }

  TcxEMFLookupGridDefaultValuesProvider = class(TcxEMFDefaultValuesProvider);
  // for internal use

  { TcxEMFLookupGridColumn }

  TcxEMFLookupGridColumn = class(TcxLookupGridColumn)
  strict private
    function GetDataController: TcxEMFLookupGridDataController; inline;
    function GetMemberInfo: TdxMappingMemberInfo;
    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
  protected
    function GetDefaultValuesProviderClass
      : TcxCustomEditDefaultValuesProviderClass; override;
    procedure InitDefaultValuesProvider;
    property DataController: TcxEMFLookupGridDataController
      read GetDataController;
  public
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; override;
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function DefaultWidth: Integer; override;
    function Equals(Obj: TObject): Boolean; override;

    property MemberInfo: TdxMappingMemberInfo read GetMemberInfo;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TcxEMFLookupGridColumns }

  TcxEMFLookupGridColumns = class(TcxLookupGridColumns)
  strict private
    function GetColumn(Index: Integer): TcxEMFLookupGridColumn; inline;
    procedure SetColumn(Index: Integer; Value: TcxEMFLookupGridColumn); inline;
  public
    function Add: TcxEMFLookupGridColumn;
    function ColumnByFieldName(const AFieldName: string)
      : TcxEMFLookupGridColumn;
    function Equals(Obj: TObject): Boolean; override;
    //
    property Items[Index: Integer]: TcxEMFLookupGridColumn read GetColumn
      write SetColumn; default;
  end;

  TcxEMFLookupGridOptions = class(TcxLookupGridOptions);

  { TcxCustomEMFLookupGrid }

  TcxCustomEMFLookupGridClass = class of TcxCustomEMFLookupGrid;
  // for internal use

  TcxCustomEMFLookupGrid = class(TcxCustomLookupGrid) // for internal use
  strict private
    function GetColumns: TcxEMFLookupGridColumns; inline;
    function GetDataController: TcxEMFLookupGridDataController; inline;
    function GetDataSource: TdxEMFDataSource; inline;
    function GetOptions: TcxEMFLookupGridOptions; inline;
    procedure SetColumns(Value: TcxEMFLookupGridColumns);
    procedure SetDataController(Value: TcxEMFLookupGridDataController);
    procedure SetDataSource(Value: TdxEMFDataSource);
    procedure SetOptions(Value: TcxEMFLookupGridOptions);
  protected
    procedure CreateColumnsByFields(AFieldNames: TStrings); virtual;
    procedure DataChanged; override;
    function GetColumnClass: TcxLookupGridColumnClass; override;
    function GetColumnsClass: TcxLookupGridColumnsClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetOptionsClass: TcxLookupGridOptionsClass; override;
    procedure InitScrollBarsParameters; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
  public
    procedure CreateAllColumns;
    procedure CreateColumnsByFieldNames(const AFieldNames: string);
    property Align;
    property Anchors;
    property Color;
    property Columns: TcxEMFLookupGridColumns read GetColumns write SetColumns;
    property DataController: TcxEMFLookupGridDataController
      read GetDataController write SetDataController;
    property Font;
    property LookAndFeel;
    property Options: TcxEMFLookupGridOptions read GetOptions write SetOptions;
    property ParentFont;
    property Visible;
  published
    property DataSource: TdxEMFDataSource read GetDataSource
      write SetDataSource;
  end;

implementation

uses
  Math, dxEMF.DataDefinitions, cxEditDBRegisteredRepositoryItems;

{ TdxEMFLookupGridDataControllerInfo }

procedure TdxEMFLookupGridDataControllerInfo.DoFilter;
begin
  if DataController.RecordCount = 0 then
    Exit;
  DataController.DoIncrementalFiltering;
end;

function TdxEMFLookupGridDataControllerInfo.GetDataController
  : TcxEMFLookupGridDataController;
begin
  Result := TcxEMFLookupGridDataController(inherited DataController);
end;

{ TcxEMFLookupGridDataController }

function TcxEMFLookupGridDataController.CreateDataControllerInfo
  : TcxCustomDataControllerInfo;
begin
  Result := TdxEMFLookupGridDataControllerInfo.Create(Self);
end;

procedure TcxEMFLookupGridDataController.DoIncrementalFiltering;
begin
end;

function TcxEMFLookupGridDataController.GetGrid: TcxCustomEMFLookupGrid;
begin
  Result := GetOwner as TcxCustomEMFLookupGrid;
end;

function TcxEMFLookupGridDataController.GetItem(Index: Integer): TObject;
begin
  Result := Grid.Columns[Index];
end;

{ TcxEMFLookupGridColumn }

procedure TcxEMFLookupGridColumn.Assign(Source: TPersistent);
begin
  if Source is TcxEMFLookupGridColumn then
    FieldName := TcxEMFLookupGridColumn(Source).FieldName;
  inherited Assign(Source);
end;

function TcxEMFLookupGridColumn.DefaultCaption: string;
var
  AMemberInfo: TdxMappingMemberInfo;
begin
  AMemberInfo := MemberInfo;
  if AMemberInfo = nil then
    Result := FieldName
  else
    Result := AMemberInfo.MemberName;
end;

function TcxEMFLookupGridColumn.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  // Result := GetDefaultEditDBRepositoryItems.GetItemByField(MemberInfo.FieldType);
  Result := nil;
end;

function TcxEMFLookupGridColumn.DefaultWidth: Integer;
begin
  Result := inherited DefaultWidth
end;

function TcxEMFLookupGridColumn.Equals(Obj: TObject): Boolean;
begin
  Result := inherited Equals(Obj) and (Obj is TcxEMFLookupGridColumn) and
    (FieldName = TcxEMFLookupGridColumn(Obj).FieldName);
end;

function TcxEMFLookupGridColumn.GetDataController
  : TcxEMFLookupGridDataController;
begin
  Result := TcxEMFLookupGridDataController(inherited DataController);
end;

function TcxEMFLookupGridColumn.GetDefaultValuesProviderClass
  : TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxEMFLookupGridDefaultValuesProvider;
end;

function TcxEMFLookupGridColumn.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Index);
end;

function TcxEMFLookupGridColumn.GetMemberInfo: TdxMappingMemberInfo;
begin
  Result := DataController.Fields[Index].MemberInfo;
end;

procedure TcxEMFLookupGridColumn.InitDefaultValuesProvider;
begin
  TcxEMFDefaultValuesProvider(DefaultValuesProvider.GetInstance).MemberInfo :=
    MemberInfo;
end;

procedure TcxEMFLookupGridColumn.SetFieldName(const Value: string);
begin
  DataController.ChangeFieldName(Index, Value);
end;

{ TcxEMFLookupGridColumns }

function TcxEMFLookupGridColumns.GetColumn(Index: Integer)
  : TcxEMFLookupGridColumn;
begin
  Result := TcxEMFLookupGridColumn(inherited Items[Index]);
end;

procedure TcxEMFLookupGridColumns.SetColumn(Index: Integer;
  Value: TcxEMFLookupGridColumn);
begin
  inherited Items[Index] := Value;
end;

function TcxEMFLookupGridColumns.Add: TcxEMFLookupGridColumn;
begin
  Result := inherited Add as TcxEMFLookupGridColumn;
end;

function TcxEMFLookupGridColumns.ColumnByFieldName(const AFieldName: string)
  : TcxEMFLookupGridColumn;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if AnsiCompareText(Result.FieldName, AFieldName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TcxEMFLookupGridColumns.Equals(Obj: TObject): Boolean;
var
  I: Integer;
begin
  Result := (Obj is TcxEMFLookupGridColumns) and
    (TcxEMFLookupGridColumns(Obj).Count = Count);
  if Result then
  begin
    for I := 0 to Count - 1 do
      Result := Result and Items[I].Equals(TcxEMFLookupGridColumns(Obj)
        .Items[I]);
  end;
end;

{ TcxCustomEMFLookupGrid }

function TcxCustomEMFLookupGrid.GetColumns: TcxEMFLookupGridColumns;
begin
  Result := TcxEMFLookupGridColumns(inherited Columns);
end;

function TcxCustomEMFLookupGrid.GetDataController
  : TcxEMFLookupGridDataController;
begin
  Result := TcxEMFLookupGridDataController(FDataController);
end;

function TcxCustomEMFLookupGrid.GetDataSource: TdxEMFDataSource;
begin
  Result := DataController.DataSource as TdxEMFDataSource;
end;

procedure TcxCustomEMFLookupGrid.CreateAllColumns;
var
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
  AFieldNames: TStrings;
begin
  Columns.Clear;
  AEntityInfo := DataSource.EntityInfo;
  if AEntityInfo <> nil then
  begin
    AFieldNames := TStringList.Create;
    try
      for AMemberInfo in AEntityInfo.MemberAttributes do
        if AMemberInfo.IsColumn and not AMemberInfo.IsAssociationList then
          AFieldNames.Add(AMemberInfo.MemberName);
      CreateColumnsByFields(AFieldNames);
    finally
      AFieldNames.Free;
    end;
  end;
end;

procedure TcxCustomEMFLookupGrid.CreateColumnsByFieldNames
  (const AFieldNames: string);
var
  AFieldNamesList: TStrings;
begin
  Columns.Clear;
  AFieldNamesList := TStringList.Create;
  try
    GetFieldNames(AFieldNames, AFieldNamesList);
    CreateColumnsByFields(AFieldNamesList);
  finally
    AFieldNamesList.Free;
  end;
end;

procedure TcxCustomEMFLookupGrid.CreateColumnsByFields(AFieldNames: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AFieldNames.Count - 1 do
      Columns.Add.FieldName := AFieldNames[I];
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomEMFLookupGrid.DataChanged;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    Columns[I].InitDefaultValuesProvider;
  inherited DataChanged;
end;

function TcxCustomEMFLookupGrid.GetColumnClass: TcxLookupGridColumnClass;
begin
  Result := TcxEMFLookupGridColumn;
end;

function TcxCustomEMFLookupGrid.GetColumnsClass: TcxLookupGridColumnsClass;
begin
  Result := TcxEMFLookupGridColumns;
end;

function TcxCustomEMFLookupGrid.GetDataControllerClass
  : TcxCustomDataControllerClass;
begin
  Result := TcxEMFLookupGridDataController;
end;

function TcxCustomEMFLookupGrid.GetOptions: TcxEMFLookupGridOptions;
begin
  Result := TcxEMFLookupGridOptions(FOptions);
end;

function TcxCustomEMFLookupGrid.GetOptionsClass: TcxLookupGridOptionsClass;
begin
  Result := TcxEMFLookupGridOptions;
end;

procedure TcxCustomEMFLookupGrid.InitScrollBarsParameters;
begin
  inherited;
end;

procedure TcxCustomEMFLookupGrid.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited;

end;

procedure TcxCustomEMFLookupGrid.SetColumns(Value: TcxEMFLookupGridColumns);
begin
  inherited Columns := Value;
end;

procedure TcxCustomEMFLookupGrid.SetDataController
  (Value: TcxEMFLookupGridDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxCustomEMFLookupGrid.SetDataSource(Value: TdxEMFDataSource);
begin
  DataController.DataSource := Value;
end;

procedure TcxCustomEMFLookupGrid.SetOptions(Value: TcxEMFLookupGridOptions);
begin
  FOptions.Assign(Value);
end;

end.
