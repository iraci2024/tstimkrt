{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016                                      }
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

unit AdvKanbanBoardDatabaseAdapter;

interface

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

uses
  Classes, AdvKanbanBoard, {%H-}AdvTypes, SysUtils
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
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : Issue with access violation during scrolling in combination with TDBGrid

type
  TAdvKanbanBoardDatabaseAdapterItemSource = class;

  TAdvKanbanBoardDatabaseAdapter = class;

  TAdvKanbanBoardDatabaseAdapterItemDataLink = class(TDataLink)
  private
    FItem: TAdvKanbanBoardDatabaseAdapterItemSource;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
    function Adapter: TAdvKanbanBoardAdapter;
  public
    constructor Create(AItem: TAdvKanbanBoardDatabaseAdapterItemSource);
    destructor Destroy; override;
  end;

  TAdvKanbanBoardDatabaseAdapterItemSource = class(TPersistent)
  private
    FUpdateCount: Integer;
    FDataLink: TAdvKanbanBoardDatabaseAdapterItemDataLink;
    FAdapter: TAdvKanbanBoardAdapter;
    FTitle: String;
    FText: String;
    FDBKey: String;
    FColumn: String;
    FAutoIncrementDBKey: Boolean;
    procedure SetText(const Value: String);
    procedure SetTitle(const Value: String);
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDBKey(const Value: String);
    procedure SetColumn(const Value: String);
    procedure SetAutoIncrementDBKey(const Value: Boolean);
  protected
    function CheckDataSet: Boolean;
    procedure ClearItems(ADBKey: String; ADeleteItem: TAdvKanbanBoardItem = nil);
    procedure DeleteItem(AItem: TAdvKanbanBoardItem);
    procedure LocateItem(ADataSet: TDataSet; AItem: TAdvKanbanBoardItem);
    procedure InsertItem(AItem: TAdvKanbanBoardItem);
    procedure SelectItem(AItem: TAdvKanbanBoardItem);
    procedure ReadItem(AItem: TAdvKanbanBoardItem);
    procedure WriteItem(AItem: TAdvKanbanBoardItem);
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function IsUpdating: Boolean; virtual;
  public
    constructor Create(AAdapter: TAdvKanbanBoardAdapter);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Title: String read FTitle write SetTitle;
    property Text: String read FText write SetText;
    property Column: String read FColumn write SetColumn;
    property DBKey: String read FDBKey write SetDBKey;
    property AutoIncrementDBKey: Boolean read FAutoIncrementDBKey write SetAutoIncrementDBKey default True;
  end;

  TAdvKanbanBoardDatabaseAdapterItemLocateEvent = procedure(Sender: TObject; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardDatabaseAdapterItemToFieldsEvent = procedure(Sender: TObject; AItem: TAdvKanbanBoardItem; AFields: TFields) of object;
  TAdvKanbanBoardDatabaseAdapterFieldsToItemEvent = procedure(Sender: TObject; AFields: TFields; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardDatabaseAdapterColumnToFieldsEvent = procedure(Sender: TObject; AColumn: Integer; AFields: TFields; AColumnField: TField) of object;
  TAdvKanbanBoardDatabaseAdapterFieldsToColumnEvent = procedure(Sender: TObject; AFields: TFields; AColumnField: TField; var AColumn: Integer; var AAcceptItem: Boolean) of object;
  TAdvKanbanBoardDatabaseAdapterItemCreateDBKeyEvent = procedure(Sender: TObject; AItem: TAdvKanbanBoardItem; var ADBKey: string) of object;
  TAdvKanbanBoardDatabaseAdapterItemEvent = procedure(Sender: TObject; AItem: TAdvKanbanBoardItem) of object;
  TAdvKanbanBoardDatabaseAdapterItemsLoadedEvent = TNotifyEvent;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvKanbanBoardDatabaseAdapter = class(TAdvKanbanBoardAdapter)
  private
    FItem: TAdvKanbanBoardDatabaseAdapterItemSource;
    FOnFieldsToItem: TAdvKanbanBoardDatabaseAdapterFieldsToItemEvent;
    FOnItemToFields: TAdvKanbanBoardDatabaseAdapterItemToFieldsEvent;
    FOnItemCreateDBKey: TAdvKanbanBoardDatabaseAdapterItemCreateDBKeyEvent;
    FOnItemUpdated: TAdvKanbanBoardDatabaseAdapterItemEvent;
    FOnItemsLoaded: TAdvKanbanBoardDatabaseAdapterItemsLoadedEvent;
    FOnItemInserted: TAdvKanbanBoardDatabaseAdapterItemEvent;
    FOnItemRead: TAdvKanbanBoardDatabaseAdapterItemEvent;
    FOnFieldsToColumn: TAdvKanbanBoardDatabaseAdapterFieldsToColumnEvent;
    FOnColumnToFields: TAdvKanbanBoardDatabaseAdapterColumnToFieldsEvent;
    FOnItemLocate: TAdvKanbanBoardDatabaseAdapterItemLocateEvent;
    procedure SetItem(const Value: TAdvKanbanBoardDatabaseAdapterItemSource);
  protected
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateOrReadDBItem(AItem: TAdvKanbanBoardItem = nil); virtual;
    procedure ClearDBItems(ADBKey: String; ADeleteItem: TAdvKanbanBoardItem = nil);
    procedure ReadDBItem(AItem: TAdvKanbanBoardItem); virtual;
    procedure WriteDBItem(AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemToFields(AItem: TAdvKanbanBoardItem; AFields: TFields); virtual;
    procedure DoColumnToFields(AColumn: Integer; AFields: TFields; AColumnField: TField); virtual;
    procedure DoFieldsToItem(AFields: TFields; AItem: TAdvKanbanBoardItem); virtual;
    procedure DoFieldsToColumn(AFields: TFields; AColumnField: TField; var AColumn: Integer; var AAcceptItem: Boolean); virtual;
    procedure DoItemCreateDBKey(AItem: TAdvKanbanBoardItem; var ADBKey: String); virtual;
    procedure DoItemInserted(AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemUpdated(AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemRead(AItem: TAdvKanbanBoardItem); virtual;
    procedure DoItemsLoaded; virtual;
    function CreateKey: string; virtual;
    function GetVersion: string; override;
  public
    procedure GetItems; override;
    procedure LoadItems; override;
    procedure DeleteItem(AItem: TAdvKanbanBoardItem); override;
    procedure InsertItem(AItem: TAdvKanbanBoardItem); override;
    procedure ReadItem(AItem: TAdvKanbanBoardItem); virtual;
    procedure UpdateItem(AItem: TAdvKanbanBoardItem); override;
    procedure AfterUpdateItem({%H-}AItem: TAdvKanbanBoardItem); override;
    procedure SelectItem(AItem: TAdvKanbanBoardItem); override;
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Version: string read GetVersion;
    property Item: TAdvKanbanBoardDatabaseAdapterItemSource read FItem write SetItem;
    property OnItemToFields: TAdvKanbanBoardDatabaseAdapterItemToFieldsEvent read FOnItemToFields write FOnItemToFields;
    property OnFieldsToItem: TAdvKanbanBoardDatabaseAdapterFieldsToItemEvent read FOnFieldsToItem write FOnFieldsToItem;
    property OnItemLocate: TAdvKanbanBoardDatabaseAdapterItemLocateEvent read FOnItemLocate write FOnItemLocate;    
    property OnItemCreateDBKey: TAdvKanbanBoardDatabaseAdapterItemCreateDBKeyEvent read FOnItemCreateDBKey write FOnItemCreateDBKey;
    property OnItemInserted: TAdvKanbanBoardDatabaseAdapterItemEvent read FOnItemInserted write FOnItemInserted;
    property OnItemUpdated: TAdvKanbanBoardDatabaseAdapterItemEvent read FOnItemUpdated write FOnItemUpdated;
    property OnItemRead: TAdvKanbanBoardDatabaseAdapterItemEvent read FOnItemRead write FOnItemRead;
    property OnItemsLoaded: TAdvKanbanBoardDatabaseAdapterItemsLoadedEvent read FOnItemsLoaded write FOnItemsLoaded;
    property OnColumnToFields: TAdvKanbanBoardDatabaseAdapterColumnToFieldsEvent read FOnColumnToFields write FOnColumnToFields;
    property OnFieldsToColumn: TAdvKanbanBoardDatabaseAdapterFieldsToColumnEvent read FOnFieldsToColumn write FOnFieldsToColumn;
  end;

implementation

uses
  AdvUtils;

{$R AdvKanbanBoardDatabaseAdapter.res}

type
  TAdvCustomKanbanBoardOpen = class(TAdvCustomKanbanBoard);

procedure TAdvKanbanBoardDatabaseAdapter.AfterUpdateItem(
  AItem: TAdvKanbanBoardItem);
begin
end;

procedure TAdvKanbanBoardDatabaseAdapter.Assign(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardDatabaseAdapter then
    FItem.Assign((Source as TAdvKanbanBoardDatabaseAdapter).Item);
end;

procedure TAdvKanbanBoardDatabaseAdapter.ClearDBItems(ADBKey: String; ADeleteItem: TAdvKanbanBoardItem = nil);
begin
  if Assigned(FItem) then
    FItem.ClearItems(ADBKey, ADeleteItem);
end;

constructor TAdvKanbanBoardDatabaseAdapter.Create(AOwner: TComponent);
begin
  inherited;
  FItem := TAdvKanbanBoardDatabaseAdapterItemSource.Create(Self);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DeleteItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(FItem) then
    FItem.DeleteItem(AItem);
end;

destructor TAdvKanbanBoardDatabaseAdapter.Destroy;
begin
  FItem.Free;
  inherited;
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoColumnToFields(AColumn: Integer;
  AFields: TFields; AColumnField: TField);
begin
  if Assigned(OnColumnToFields) then
    OnColumnToFields(Self, AColumn, AFields, AColumnField);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoFieldsToItem(AFields: TFields; AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnFieldsToItem) then
    OnFieldsToItem(Self, AFields, AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoFieldsToColumn(AFields: TFields;
  AColumnField: TField; var AColumn: Integer; var AAcceptItem: Boolean);
begin
  if Assigned(OnFieldsToColumn) then
    OnFieldsToColumn(Self, AFields, AColumnField, AColumn, AAcceptItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoItemCreateDBKey(
  AItem: TAdvKanbanBoardItem; var ADBKey: String);
begin
  if Assigned(OnItemCreateDBKey) then
    OnItemCreateDBKey(Self, AItem, ADBKey)
  else
    ADBKey := CreateKey;
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoItemInserted(
  AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemInserted) then
    OnItemInserted(Self, AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoItemRead(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemRead) then
    OnItemRead(Self, AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoItemsLoaded;
begin
  if Assigned(OnItemsLoaded) then
    OnItemsLoaded(Self);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoItemToFields(AItem: TAdvKanbanBoardItem; AFields: TFields);
begin
  if Assigned(OnItemToFields) then
    OnItemToFields(Self, AItem, AFields);
end;

procedure TAdvKanbanBoardDatabaseAdapter.DoItemUpdated(
  AItem: TAdvKanbanBoardItem);
begin
  if Assigned(OnItemUpdated) then
    OnItemUpdated(Self, AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.GetItems;
var
  ds: TDataSet;
  b: TBookmark;
begin
  if not Assigned(KanbanBoard) then
    Exit;

  if not Item.CheckDataSet then
    Exit;

  if Item.IsUpdating then
    Exit;

  Item.BeginUpdate;
  ds := Item.DataSource.DataSet;
  if Assigned(ds) then
  begin
    KanbanBoard.BeginUpdate;
    ds.DisableControls;
    b := ds.GetBookmark;
    ds.First;
    while not ds.Eof do
    begin
      CreateOrReadDBItem;
      ds.Next;
    end;

    if ds.BookmarkValid(b) then
      ds.GotoBookmark(b);
    ds.EnableControls;
    ds.FreeBookmark(b);
    KanbanBoard.EndUpdate;
  end;
  Item.EndUpdate;

  DoItemsLoaded;
end;

function TAdvKanbanBoardDatabaseAdapter.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvKanbanBoardDatabaseAdapter.InsertItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(FItem) then
    FItem.InsertItem(AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.LoadItems;
var
  p: TAdvCustomKanbanBoardOpen;
begin
  if not Assigned(KanbanBoard) then
    Exit;

  p := TAdvCustomKanbanBoardOpen(KanbanBoard);

  p.BeginUpdate;
  p.ClearItems;

  if Active then
    GetItems;

  p.EndUpdate;
end;

procedure TAdvKanbanBoardDatabaseAdapter.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FItem.DataSource) then
     FItem.DataSource := nil;
end;

procedure TAdvKanbanBoardDatabaseAdapter.SelectItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(FItem) then
    FItem.SelectItem(AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.SetItem(
  const Value: TAdvKanbanBoardDatabaseAdapterItemSource);
begin
  FItem.Assign(Value);
end;

procedure TAdvKanbanBoardDatabaseAdapter.ReadDBItem(AItem: TAdvKanbanBoardItem);
begin
  if not Assigned(AItem) then
    Exit;

  CreateOrReadDBItem(AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.ReadItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(FItem) then
    FItem.ReadItem(AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvKanbanBoardDatabaseAdapter);
end;

procedure TAdvKanbanBoardDatabaseAdapter.UpdateItem(AItem: TAdvKanbanBoardItem);
begin
  if Assigned(FItem) then
    FItem.WriteItem(AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapter.WriteDBItem(AItem: TAdvKanbanBoardItem);
var
  ft, fn, fdbkey, fcol: TField;
  ds: TDataSet;
  it: TAdvKanbanBoardItem;
begin
  if not Assigned(AItem) then
    Exit;

  ds := Item.DataSource.DataSet;
  if Assigned(ds) then
  begin
    fdbkey := nil;
    if Item.DBKey <> '' then
      fdbkey := ds.FieldByName(Item.DBKey);

    ft := nil;
    if Item.Title <> '' then
      ft := ds.FieldByName(Item.Title);

    fn := nil;
    if Item.Text <> '' then
      fn := ds.FieldByName(Item.Text);

    fcol := nil;
    if Item.Column <> '' then
      fcol := ds.FieldByName(Item.Column);

    if Assigned(fdbkey) and Assigned(fcol) then
    begin
      it := AItem;

      if Assigned(ft) then
        ft.AsString := it.Title;
      if Assigned(fn) then
        fn.AsString := it.Text;
      if Assigned(fcol) then
        fcol.AsInteger := it.Column.Index;

      DoColumnToFields(it.Column.Index, ds.Fields, fcol);
      DoItemToFields(it, ds.Fields);
    end;
  end;
end;

procedure TAdvKanbanBoardDatabaseAdapter.CreateOrReadDBItem(AItem: TAdvKanbanBoardItem = nil);
var
  ft, fn, fdbkey, fcol: TField;
  ds: TDataSet;
  it, itn: TAdvKanbanBoardItem;
  c: Integer;
  col: TAdvKanbanBoardColumn;
  a: Boolean;
begin
  ds := Item.DataSource.DataSet;
  if Assigned(ds) then
  begin
    if Assigned(KanbanBoard) then
      KanbanBoard.BeginUpdate;

    fdbkey := nil;
    if Item.DBKey <> '' then
      fdbkey := ds.FieldByName(Item.DBKey);

    ft := nil;
    if Item.Title <> '' then
      ft := ds.FieldByName(Item.Title);

    fn := nil;
    if Item.Text <> '' then
      fn := ds.FieldByName(Item.Text);

    fcol := nil;
    if Item.Column <> '' then
      fcol := ds.FieldByName(Item.Column);

    if Assigned(fdbkey) and Assigned(fcol) then
    begin
      c := fcol.AsInteger;
      a := True;

      DoFieldsToColumn(ds.Fields, fcol, c, a);

      if a and (c >= 0) and (c <= TAdvCustomKanbanBoardOpen(KanbanBoard).Columns.Count - 1) then
      begin
        itn := nil;
        if Assigned(AItem) then
        begin
          it := AItem;
          itn := TAdvCustomKanbanBoardOpen(KanbanBoard).Columns[c].Items.Add;
        end
        else
          it := TAdvCustomKanbanBoardOpen(KanbanBoard).Columns[c].Items.Add;


        if Assigned(fcol) and Assigned(itn) then
        begin
          itn.Assign(it);
          col := it.Column;
          if Assigned(col) then
            col.Items.Delete(it.Index);

          it := itn;
        end;

        it.DBKey := fdbkey.AsString;
        if Assigned(ft) then
          it.Title := ft.AsString;
        if Assigned(fn) then
          it.Text := fn.AsString;

        DoFieldsToItem(ds.Fields, it);

        DoItemRead(it);
      end;
    end;

    if Assigned(KanbanBoard) then
      KanbanBoard.EndUpdate;
  end;
end;

function TAdvKanbanBoardDatabaseAdapter.CreateKey: string;
{$IFDEF LCLWEBLIB}
var
  g: TGUID;
{$ENDIF}
begin
  {$IFNDEF LCLWEBLIB}
  Result := TGuid.NewGuid.ToString;
  {$ENDIF}
  {$IFDEF LCLWEBLIB}
  CreateGUID(g);
  Result := GUIDToString(g);
  {$ENDIF}
end;

{ TAdvKanbanBoardDatabaseAdapterItemDataLink }

procedure TAdvKanbanBoardDatabaseAdapterItemDataLink.ActiveChanged;
var
  a: TAdvKanbanBoardAdapter;
begin
  inherited;
  a := Adapter;
  if Assigned(a) then
    a.LoadItems;
end;

function TAdvKanbanBoardDatabaseAdapterItemDataLink.Adapter: TAdvKanbanBoardAdapter;
begin
  Result := nil;
  if Assigned(FItem) then
    Result := FItem.FAdapter;
end;

constructor TAdvKanbanBoardDatabaseAdapterItemDataLink.Create(AItem: TAdvKanbanBoardDatabaseAdapterItemSource);
begin
  FItem := AItem;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemDataLink.DataSetChanged;
begin
  inherited;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;
end;

destructor TAdvKanbanBoardDatabaseAdapterItemDataLink.Destroy;
begin

  inherited;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemDataLink.RecordChanged(Field: TField);
var
  fdbkey: TField;
  a: TAdvKanbanBoardAdapter;
  it: TAdvKanbanBoardItem;
begin
  inherited;

  a := Adapter;
  if Assigned(a) and not a.Active then
    Exit;

  if not FItem.CheckDataSet or FItem.IsUpdating then
    Exit;

  FItem.BeginUpdate;
  if Assigned(DataSet) then
  begin
    if DataSet.Active then
    begin
      if DataSet.State = dsBrowse then
      begin
        fdbkey := nil;
        if FItem.DBKey <> '' then
          fdbkey := DataSet.FieldByName(FItem.DBKey);

        if Assigned(fdbkey) then
        begin
          it := nil;
          if Assigned(a.KanbanBoard) then
            it := a.KanbanBoard.FindItemWithDBKey(fdbkey.AsString);

          if Assigned(it) then
          begin
            if a is TAdvKanbanBoardDatabaseAdapter then
              (a as TAdvKanbanBoardDatabaseAdapter).ReadDBItem(it);
          end;

          it := nil;
          if Assigned(a.KanbanBoard) then
            it := a.KanbanBoard.FindItemWithDBKey(fdbkey.AsString);

          if Assigned(it) then
          begin
            if Assigned(a.KanbanBoard) then
              a.KanbanBoard.SelectItem(it);
          end;

        end;
      end;
    end;
  end;
  FItem.EndUpdate;
end;

{ TAdvKanbanBoardDatabaseAdapterItemSource }

procedure TAdvKanbanBoardDatabaseAdapterItemSource.Assign(Source: TPersistent);
begin
  if Source is TAdvKanbanBoardDatabaseAdapterItemSource then
  begin
    DataSource := (Source as TAdvKanbanBoardDatabaseAdapterItemSource).DataSource;
    FTitle := (Source as TAdvKanbanBoardDatabaseAdapterItemSource).Title;
    FText := (Source as TAdvKanbanBoardDatabaseAdapterItemSource).Title;
    FColumn := (Source as TAdvKanbanBoardDatabaseAdapterItemSource).Column;
    FDBKey := (Source as TAdvKanbanBoardDatabaseAdapterItemSource).DBKey;
    FAutoIncrementDBKey := (Source as TAdvKanbanBoardDatabaseAdapterItemSource).AutoIncrementDBKey;
  end;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TAdvKanbanBoardDatabaseAdapterItemSource.CheckDataSet: Boolean;
begin
  Result := False;

  if (DBKey = '') or (Column = '') then
  begin
    raise Exception.Create('DBKey/Column are not set in TAdvKanbanBoardDatabaseAdapter item source.');
    Exit;
  end;

  if Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      Result := DataSource.DataSet.Active;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.ClearItems(ADBKey: String; ADeleteItem: TAdvKanbanBoardItem = nil);
var
  I, K: Integer;
  a: TAdvKanbanBoardAdapter;
  it: TAdvKanbanBoardItem;
begin
  a := FAdapter;
  if not Assigned(a) then
    Exit;

  if Assigned(a.KanbanBoard) then
  begin
    a.KanbanBoard.BeginUpdate;
    for I := TAdvCustomKanbanBoardOpen(a.KanbanBoard).Columns.Count - 1 downto 0 do
    begin
      for K := TAdvCustomKanbanBoardOpen(a.KanbanBoard).Columns[I].Items.Count - 1 downto 0 do
      begin
        it := TAdvCustomKanbanBoardOpen(a.KanbanBoard).Columns[I].Items[K];
        if (it.DBKey = ADBKey) and (it <> ADeleteItem) then
          TAdvCustomKanbanBoardOpen(a.KanbanBoard).Columns[I].Items.Delete(K);
      end;
    end;
    a.KanbanBoard.EndUpdate;
  end;
end;

constructor TAdvKanbanBoardDatabaseAdapterItemSource.Create(AAdapter: TAdvKanbanBoardAdapter);
begin
  FUpdateCount := 0;
  FAutoIncrementDBKey := True;
  FAdapter := AAdapter;
  FDataLink := TAdvKanbanBoardDatabaseAdapterItemDataLink.Create(Self);
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.DeleteItem(
  AItem: TAdvKanbanBoardItem);
var
  ds: TDataSet;
begin
  if not CheckDataSet or not Assigned(AItem) then
    Exit;

  if IsUpdating then
    Exit;

  BeginUpdate;
  ds := DataSource.DataSet;
  if Assigned(ds) then
  begin
    ds.DisableControls;
    LocateItem(ds, AItem);        
    try
      ds.Delete;
    except
      ds.EnableControls;
      EndUpdate;
      raise Exception.Create('Could not delete record from dataset.');
    end;
    ds.EnableControls;
  end;
  EndUpdate;
end;

destructor TAdvKanbanBoardDatabaseAdapterItemSource.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TAdvKanbanBoardDatabaseAdapterItemSource.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.InsertItem(AItem: TAdvKanbanBoardItem);
var
  ds: TDataSet;
  b: TBookmark;
  k: String;
  fdbk: TField;
begin
  if not CheckDataSet then
    Exit;

  BeginUpdate;
  ds := DataSource.DataSet;
  if Assigned(ds) then
  begin
    ds.DisableControls;
    b := ds.GetBookmark;
    try
      try
        ds.Append;
      except
        if ds.BookmarkValid(b) then
          ds.GotoBookmark(b);

        ds.EnableControls;
        ds.FreeBookmark(b);
        EndUpdate;
        raise Exception.Create('Could not append new record.');
      end;

      fdbk := nil;
      if DBKey <> '' then
        fdbk := ds.FieldByName(DBKey);

      if not AutoIncrementDBKey then
      begin
        k := AItem.DBKey;
        if Assigned(FAdapter) and (FAdapter is TAdvKanbanBoardDatabaseAdapter) then
          (FAdapter as TAdvKanbanBoardDatabaseAdapter).DoItemCreateDBKey(AItem, k);

        if Assigned(fdbk) then
          fdbk.AsString := k;
      end;

      if Assigned(fdbk) then
        AItem.DBKey := fdbk.AsString;

      WriteItem(AItem);
      AItem.DBKey := fdbk.AsString;
    finally
      if ds.BookmarkValid(b) then
        ds.GotoBookmark(b);

      ds.EnableControls;
      ds.FreeBookmark(b);
    end;
  end;
  EndUpdate;

  if Assigned(FAdapter) and (FAdapter is TAdvKanbanBoardDatabaseAdapter) and not IsUpdating then
    (FAdapter as TAdvKanbanBoardDatabaseAdapter).DoItemInserted(AItem);
end;

function TAdvKanbanBoardDatabaseAdapterItemSource.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.LocateItem(ADataSet: TDataSet; AItem: TAdvKanbanBoardItem);
begin
  if Assigned((FAdapter as TAdvKanbanBoardDatabaseAdapter).OnItemLocate) then
    (FAdapter as TAdvKanbanBoardDatabaseAdapter).OnItemLocate(Self, AItem)
  else
  begin
    if Assigned(ADataSet) then  
      ADataSet.Locate(DBKey, AItem.DBKey, []);
  end;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.ReadItem(AItem: TAdvKanbanBoardItem);
var
  ds: TDataSet;
  b: TBookmark;
begin
  if not CheckDataSet or not Assigned(AItem) then
    Exit;

  BeginUpdate;
  ds := DataSource.DataSet;
  if Assigned(ds) then
  begin
    ds.DisableControls;
    b := ds.GetBookmark;
    try
      if ds.State = dsBrowse then
        LocateItem(ds, AItem);

      if FAdapter is TAdvKanbanBoardDatabaseAdapter then
        (FAdapter as TAdvKanbanBoardDatabaseAdapter).ReadDBItem(AItem);
    finally
      if ds.BookmarkValid(b) then
        ds.GotoBookmark(b);

      ds.EnableControls;
      ds.FreeBookmark(b);
    end;
  end;
  EndUpdate;

  if Assigned(FAdapter) and (FAdapter is TAdvKanbanBoardDatabaseAdapter) and not IsUpdating then
    (FAdapter as TAdvKanbanBoardDatabaseAdapter).DoItemRead(AItem);
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SelectItem(AItem: TAdvKanbanBoardItem);
var
  ds: TDataSet;
begin
  if not CheckDataSet or not Assigned(AItem) then
    Exit;

  if IsUpdating then
    Exit;

  BeginUpdate;
  ds := DataSource.DataSet;
  LocateItem(ds, AItem);
  EndUpdate;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SetAutoIncrementDBKey(
  const Value: Boolean);
begin
  FAutoIncrementDBKey := Value;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SetDataSource(const Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
    FDataLink.DataSource := Value;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SetDBKey(const Value: String);
begin
  FDBKey := Value;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SetColumn(
  const Value: String);
begin
  FColumn := Value;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SetText(const Value: String);
begin
  FText := Value;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

procedure TAdvKanbanBoardDatabaseAdapterItemSource.WriteItem(AItem: TAdvKanbanBoardItem);
var
  ds: TDataSet;
  b: TBookmark;
begin
  if not CheckDataSet then
    Exit;

  BeginUpdate;
  ds := DataSource.DataSet;
  if Assigned(ds) then
  begin
    ds.DisableControls;
    b := ds.GetBookmark;
    try
      if ds.State = dsBrowse then
      begin
        try
          LocateItem(ds, AItem);
          ds.Edit;
        except
          if ds.BookmarkValid(b) then
            ds.GotoBookmark(b);

          ds.EnableControls;
          ds.FreeBookmark(b);
          EndUpdate;
          raise Exception.Create('Could not put dataset in edit mode.');
        end;
      end;

      if (ds.State = dsEdit) or (ds.State = dsInsert) then
      begin
        if FAdapter is TAdvKanbanBoardDatabaseAdapter then
         (FAdapter as TAdvKanbanBoardDatabaseAdapter).WriteDBItem(AItem);
        try
          ds.Post;
        except
          if ds.BookmarkValid(b) then
            ds.GotoBookmark(b);

          ds.EnableControls;
          ds.FreeBookmark(b);
          EndUpdate;
          raise Exception.Create('Could not post to dataset.');
        end;
      end;
    finally
      if ds.BookmarkValid(b) then
        ds.GotoBookmark(b);

      ds.EnableControls;
      ds.FreeBookmark(b);
    end;
  end;
  EndUpdate;

  if (FAdapter is TAdvKanbanBoardDatabaseAdapter) and not IsUpdating then
    (FAdapter as TAdvKanbanBoardDatabaseAdapter).DoItemUpdated(AItem);
end;

end.
