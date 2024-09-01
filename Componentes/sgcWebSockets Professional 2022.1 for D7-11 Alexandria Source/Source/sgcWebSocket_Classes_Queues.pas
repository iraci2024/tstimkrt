{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Classes_Queues;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF NEXTGEN}System.Generics.Collections, System.Generics.Defaults, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes_SyncObjs;

type
  TsgcQueueItemBase = class
  protected
    FOrder: Int64;
    function GetOrder: Int64; virtual;
  protected
    FID: String;
    function GetID: String; virtual;
  public
    constructor Create; virtual;
  public
    property ID: String read GetID write FID;
    property Order: Int64 read GetOrder write FOrder;
  end;

  TsgcQueueNameItemBase = class(TsgcQueueItemBase)
  private
    FQueue: String;
  public
    constructor Create; override;
    property Queue: String read FQueue write FQueue;
  end;

  TsgcQueueCommon = class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  private
    FOwnObjects: Boolean;
    function GetCount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    property Count: Integer read GetCount;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
  end;

  TsgcQueueBase = class(TsgcQueueCommon)
  protected
    function GetItemByIndex(Index: Integer): TsgcQueueItemBase; virtual;
  public
    procedure AddItem(const aItem: TsgcQueueItemBase); virtual;
    function DeleteItem(const aID: String): Boolean; overload;
    function DeleteItem(const Index: Integer): Boolean; overload;
    function GetItem(const aID: String): TsgcQueueItemBase; overload;
    function GetItem(const Index: Integer): TsgcQueueItemBase; overload;
  public
    property Item[Index: Integer]: TsgcQueueItemBase
      read GetItemByIndex; default;
  end;

  TsgcFIFOQueue = class(TsgcQueueBase)
  private
    FMaxItems: Integer;
  public
    constructor Create; override;
  public
    procedure AddItem(const aItem: TsgcQueueItemBase); override;
  public
    property MaxItems: Integer read FMaxItems write FMaxItems;
  end;

  TsgcQueue = class(TsgcQueueBase)
  public
    procedure SortByOrder(aAscending: Boolean = True);
  end;

  TsgcCacheQueue = class(TComponent)
  private
    function GetCount: Integer;
  protected
    function GetItemByIndex(Index: Integer): TsgcQueueItemBase; virtual;
  private
    FCache: TsgcFIFOQueue;
    FQueue: TsgcQueue;
    FQueueName: String;
    function GetCache: TsgcFIFOQueue;
    function GetCacheSize: Integer;
    function GetOwnObjects: Boolean;
    function GetQueue: TsgcQueue;
    procedure SetCacheSize(const Value: Integer);
    procedure SetOwnObjects(const Value: Boolean);
  protected
    property Cache: TsgcFIFOQueue read GetCache write FCache;
    property Queue: TsgcQueue read GetQueue write FQueue;
  public
    procedure Clear;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AddItem(const aItem: TsgcQueueItemBase); virtual;
    function DeleteItem(const aID: String): Boolean;
    function GetItem(const aID: String): TsgcQueueItemBase;
  public
    property Item[Index: Integer]: TsgcQueueItemBase
      read GetItemByIndex; default;
  public
    property CacheSize: Integer read GetCacheSize write SetCacheSize;
    property Count: Integer read GetCount;
    property OwnObjects: Boolean read GetOwnObjects write SetOwnObjects;
    property QueueName: String read FQueueName write FQueueName;
  end;

  TsgcCacheQueueListBase = class(TsgcQueueCommon)
  private
    FCacheSize: Integer;
    FGroupLevel: Integer;
    function GetCount: Integer;
  private
    function GetQueueName(const aID: String): String;
  protected
    function AddQueue(const aQueueName: String): TsgcCacheQueue;
    function GetQueue(const aQueueName: String): TsgcCacheQueue;
    function DeleteQueue(const aQueueName: String): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure AddItem(const aItem: TsgcQueueItemBase); virtual;
    function DeleteItem(const aID: String): Boolean;
    function GetItem(const aID: String): TsgcQueueItemBase;
  public
    property CacheSize: Integer read FCacheSize write FCacheSize;
    property Count: Integer read GetCount;
    property GroupLevel: Integer read FGroupLevel write FGroupLevel;
  end;

  TsgcQueueNameBase = class(TsgcQueueBase)
  private
    FQueueName: String;
  public
    property QueueName: String read FQueueName write FQueueName;
  end;

  TsgcQueueNameListBase = class(TsgcQueueCommon)
  private
    function GetCount: Integer;
  protected
    function AddQueue(const aQueueName: String): TsgcQueueNameBase;
    function GetQueue(const aQueueName: String): TsgcQueueNameBase;
    function DeleteQueue(const aQueueName: String): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    function AddItem(const aItem: TsgcQueueNameItemBase): Boolean; virtual;
    procedure DeleteItem(const aID: String); overload;
    function DeleteItem(const aQueueName, aID: String): Boolean; overload;
    function GetQueueByName(const aQueueName: String): TsgcQueueNameBase;
    function GetQueuesDelimitedText: String;
  public
    property Count: Integer read GetCount;
  end;

  TsgcCacheQueueList = class(TComponent)
  private
    FQueueCache: TsgcCacheQueueListBase;
  private
    function GetCacheSize: Integer;
    function GetGroupLevel: Integer;
    procedure SetCacheSize(const Value: Integer);
    procedure SetGroupLevel(const Value: Integer);
  protected
    function GetCount: Integer; virtual;
  protected
    property QueueCache: TsgcCacheQueueListBase read FQueueCache
      write FQueueCache;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AddItem(const aItem: TsgcQueueItemBase); virtual;
    function DeleteItem(const aID: String): Boolean; virtual;
    function GetItem(const aID: String): TsgcQueueItemBase;
  public
    property CacheSize: Integer read GetCacheSize write SetCacheSize;
    property Count: Integer read GetCount;
    property GroupLevel: Integer read GetGroupLevel write SetGroupLevel;
  end;

  TsgcCacheQueueFullList = class(TsgcCacheQueueList)
  private
    FQueue: TsgcQueueBase;
  protected
    function GetCount: Integer; override;
  protected
    function GetItemByIndex(Index: Integer): TsgcQueueItemBase; virtual;
  protected
    property Queue: TsgcQueueBase read FQueue write FQueue;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AddItem(const aItem: TsgcQueueItemBase); override;
    function DeleteItem(const aID: String): Boolean; override;
  public
    property Item[Index: Integer]: TsgcQueueItemBase
      read GetItemByIndex; default;
  end;

implementation

uses
  sgcBase_Helpers, sgcWebSocket_Helpers;

procedure TsgcQueueBase.AddItem(const aItem: TsgcQueueItemBase);
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := LockList;
  Try
    oList.Add(aItem)
  Finally
    UnlockList;
  End;
end;

function TsgcQueueBase.DeleteItem(const aID: String): Boolean;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oQueueItem: TsgcQueueItemBase;
begin
  Result := False;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcQueueItemBase(oList[i]).ID = aID then
      begin
        Result := True;
        if OwnObjects then
        begin
          oQueueItem := TsgcQueueItemBase(oList.Items[i]);
          sgcFree(oQueueItem);
        end;
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcQueueBase.DeleteItem(const Index: Integer): Boolean;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oQueueItem: TsgcQueueItemBase;
begin
  Result := False;

  oList := LockList;
  Try
    if Index < oList.Count then
    begin
      Result := True;
      if OwnObjects then
      begin
        oQueueItem := TsgcQueueItemBase(oList.Items[Index]);
        sgcFree(oQueueItem);
      end;
      oList.Delete(Index);
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcQueueBase.GetItem(const aID: String): TsgcQueueItemBase;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcQueueItemBase(oList[i]).ID = aID then
      begin
        Result := TsgcQueueItemBase(oList[i]);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcQueueBase.GetItem(const Index: Integer): TsgcQueueItemBase;
begin
  Result := GetItemByIndex(Index);
end;

function TsgcQueueBase.GetItemByIndex(Index: Integer): TsgcQueueItemBase;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := nil;

  oList := LockList;
  Try
    if Index < oList.Count then
      Result := TsgcQueueItemBase(oList[Index]);
  Finally
    UnlockList;
  End;
end;

constructor TsgcCacheQueue.Create(aOwner: TComponent);
begin
  inherited;
  Cache.OwnObjects := False;
end;

destructor TsgcCacheQueue.Destroy;
begin
  Clear;
  sgcFree(FCache);
  sgcFree(FQueue);
  inherited;
end;

procedure TsgcCacheQueue.AddItem(const aItem: TsgcQueueItemBase);
begin
  Queue.AddItem(aItem);
end;

procedure TsgcCacheQueue.Clear;
begin
  Cache.Clear;
  Queue.Clear;
end;

function TsgcCacheQueue.DeleteItem(const aID: String): Boolean;
begin
  if Cache.MaxItems > 0 then
    Cache.DeleteItem(aID);
  Result := Queue.DeleteItem(aID);
end;

function TsgcCacheQueue.GetCache: TsgcFIFOQueue;
begin
  if not Assigned(FCache) then
    FCache := TsgcFIFOQueue.Create;
  Result := FCache;
end;

function TsgcCacheQueue.GetCacheSize: Integer;
begin
  Result := Cache.MaxItems;
end;

function TsgcCacheQueue.GetCount: Integer;
begin
  Result := Queue.Count;
end;

function TsgcCacheQueue.GetItem(const aID: String): TsgcQueueItemBase;
begin
  if Cache.MaxItems > 0 then
  begin
    Result := Cache.GetItem(aID);
    if not Assigned(Result) then
    begin
      Result := Queue.GetItem(aID);
      if Assigned(Result) then
        Cache.AddItem(Result);
    end;
  end
  else
    Result := Queue.GetItem(aID);
end;

function TsgcCacheQueue.GetItemByIndex(Index: Integer): TsgcQueueItemBase;
begin
  Result := Queue.Item[Index];
end;

function TsgcCacheQueue.GetOwnObjects: Boolean;
begin
  Result := Queue.OwnObjects;
end;

function TsgcCacheQueue.GetQueue: TsgcQueue;
begin
  if not Assigned(FQueue) then
    FQueue := TsgcQueue.Create;
  Result := FQueue;
end;

procedure TsgcCacheQueue.SetCacheSize(const Value: Integer);
begin
  Cache.MaxItems := Value;
end;

procedure TsgcCacheQueue.SetOwnObjects(const Value: Boolean);
begin
  Queue.OwnObjects := Value;
end;

constructor TsgcFIFOQueue.Create;
begin
  inherited;
  MaxItems := 0;
end;

procedure TsgcFIFOQueue.AddItem(const aItem: TsgcQueueItemBase);
begin
  if (MaxItems > 0) and (Count >= MaxItems) then
    DeleteItem(TsgcQueueItemBase(Item[0]).ID);
  inherited;
end;

constructor TsgcCacheQueueListBase.Create;
begin
  inherited;
  CacheSize := 0;
  GroupLevel := 1;
  OwnObjects := True;
end;

destructor TsgcCacheQueueListBase.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcCacheQueueListBase.AddItem(const aItem: TsgcQueueItemBase);
var
  oQueue: TsgcCacheQueue;
begin
  oQueue := GetQueue(GetQueueName(aItem.ID));
  if Assigned(oQueue) then
    oQueue.AddItem(aItem);
end;

function TsgcCacheQueueListBase.AddQueue(const aQueueName: String)
  : TsgcCacheQueue;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := TsgcCacheQueue.Create(nil);
  Result.CacheSize := CacheSize;
  Result.QueueName := aQueueName;
  Result.OwnObjects := True;

  oList := LockList;
  Try
    oList.Add(Result);
  Finally
    UnlockList;
  End;
end;

function TsgcCacheQueueListBase.DeleteItem(const aID: String): Boolean;
var
  oQueue: TsgcCacheQueue;
begin
  Result := False;

  oQueue := GetQueue(GetQueueName(aID));
  if Assigned(oQueue) then
    Result := oQueue.DeleteItem(aID);
end;

function TsgcCacheQueueListBase.DeleteQueue(const aQueueName: String): Boolean;
var
  i: Integer;
  oCacheQueue: TsgcCacheQueue;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := False;

  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcCacheQueue(oList[i]).QueueName = aQueueName then
      begin
        Result := True;
        if OwnObjects then
        begin
          oCacheQueue := TsgcCacheQueue(oList.Items[i]);
          sgcFree(oCacheQueue);
        end;
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcCacheQueueListBase.GetCount: Integer;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := 0;

  oList := LockList;
  Try
    for i := 0 to oList.Count do
      Result := Result + TsgcCacheQueue(oList[i]).Count;
  Finally
    UnlockList;
  End;
end;

function TsgcCacheQueueListBase.GetItem(const aID: String): TsgcQueueItemBase;
var
  oQueue: TsgcCacheQueue;
begin
  Result := nil;

  oQueue := GetQueue(GetQueueName(aID));
  if Assigned(oQueue) then
    Result := oQueue.GetItem(aID);
end;

function TsgcCacheQueueListBase.GetQueue(const aQueueName: String)
  : TsgcCacheQueue;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcCacheQueue(oList[i]).QueueName = aQueueName then
      begin
        Result := TsgcCacheQueue(oList[i]);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;

  if not Assigned(Result) then
    Result := AddQueue(aQueueName);
end;

function TsgcCacheQueueListBase.GetQueueName(const aID: String): String;
begin
  Result := '';
  if GroupLevel > 0 then
    Result := LeftStr(aID, GroupLevel);
end;

constructor TsgcQueueCommon.Create;
begin
  inherited;
  FOwnObjects := False;
end;

destructor TsgcQueueCommon.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcQueueCommon.Clear;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oObject: TObject;
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if OwnObjects then
      begin
        oObject := TObject(oList.Items[i]);
        sgcTryFree(oObject);
      end;
      oList.Delete(i);
    end;
  Finally
    UnlockList;
  End;
  inherited;
end;

function TsgcQueueCommon.GetCount: Integer;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := LockList;
  Try
    Result := oList.Count;
  Finally
    UnlockList;
  End;
end;

constructor TsgcCacheQueueList.Create(aOwner: TComponent);
begin
  inherited;
  FQueueCache := TsgcCacheQueueListBase.Create;
end;

destructor TsgcCacheQueueList.Destroy;
begin
  sgcFree(FQueueCache);
  inherited;
end;

procedure TsgcCacheQueueList.AddItem(const aItem: TsgcQueueItemBase);
begin
  FQueueCache.AddItem(aItem);
end;

function TsgcCacheQueueList.DeleteItem(const aID: String): Boolean;
begin
  Result := FQueueCache.DeleteItem(aID);
end;

function TsgcCacheQueueList.GetCacheSize: Integer;
begin
  Result := FQueueCache.CacheSize;
end;

function TsgcCacheQueueList.GetCount: Integer;
begin
  Result := FQueueCache.Count;
end;

function TsgcCacheQueueList.GetGroupLevel: Integer;
begin
  Result := FQueueCache.GroupLevel;
end;

function TsgcCacheQueueList.GetItem(const aID: String): TsgcQueueItemBase;
begin
  Result := FQueueCache.GetItem(aID);
end;

procedure TsgcCacheQueueList.SetCacheSize(const Value: Integer);
begin
  FQueueCache.CacheSize := Value;
end;

procedure TsgcCacheQueueList.SetGroupLevel(const Value: Integer);
begin
  FQueueCache.GroupLevel := Value;
end;

constructor TsgcQueueItemBase.Create;
begin
  inherited;
  FID := '';
  Order := 0;
end;

function TsgcQueueItemBase.GetID: String;
begin
  Result := FID;
end;

function TsgcQueueItemBase.GetOrder: Int64;
begin
  Result := FOrder;
end;

constructor TsgcCacheQueueFullList.Create(aOwner: TComponent);
begin
  inherited;
  FQueue := TsgcQueueBase.Create;
end;

destructor TsgcCacheQueueFullList.Destroy;
begin
  sgcFree(FQueue);
  inherited;
end;

procedure TsgcCacheQueueFullList.AddItem(const aItem: TsgcQueueItemBase);
begin
  inherited;
  FQueue.AddItem(aItem);
end;

function TsgcCacheQueueFullList.DeleteItem(const aID: String): Boolean;
begin
  FQueue.DeleteItem(aID);
  Result := inherited DeleteItem(aID);
end;

function TsgcCacheQueueFullList.GetCount: Integer;
begin
  Result := FQueue.Count
end;

function TsgcCacheQueueFullList.GetItemByIndex(Index: Integer)
  : TsgcQueueItemBase;
begin
  Result := FQueue.Item[Index];
end;

constructor TsgcQueueNameListBase.Create;
begin
  inherited;
  OwnObjects := True;
end;

destructor TsgcQueueNameListBase.Destroy;
begin
  Clear;
  inherited;
end;

function TsgcQueueNameListBase.AddItem(const aItem
  : TsgcQueueNameItemBase): Boolean;
var
  oQueue: TsgcQueueNameBase;
begin
  Result := False;
  oQueue := GetQueue(aItem.Queue);
  if not Assigned(oQueue) then
    oQueue := AddQueue(aItem.Queue);
  if Assigned(oQueue) then
  begin
    if not Assigned(oQueue.GetItem(aItem.ID)) then
    begin
      oQueue.AddItem(aItem);
      Result := True;
    end;
  end;
end;

function TsgcQueueNameListBase.AddQueue(const aQueueName: String)
  : TsgcQueueNameBase;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := TsgcQueueNameBase.Create;
  Result.OwnObjects := True;
  Result.QueueName := aQueueName;

  oList := LockList;
  Try
    oList.Add(Result);
  Finally
    UnlockList;
  End;
end;

procedure TsgcQueueNameListBase.DeleteItem(const aID: String);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
      TsgcQueueNameBase(oList[i]).DeleteItem(aID);
  Finally
    UnlockList;
  End;
end;

function TsgcQueueNameListBase.DeleteItem(const aQueueName,
  aID: String): Boolean;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := False;

  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcQueueNameBase(oList[i]).QueueName = aQueueName then
      begin
        Result := True;
        TsgcQueueNameBase(oList[i]).DeleteItem(aID);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcQueueNameListBase.DeleteQueue(const aQueueName: String): Boolean;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oQueueName: TsgcQueueNameBase;
begin
  Result := False;

  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcQueueNameBase(oList[i]).QueueName = aQueueName then
      begin
        Result := True;
        if OwnObjects then
        begin
          oQueueName := TsgcQueueNameBase(oList.Items[i]);
          sgcFree(oQueueName);
        end;
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcQueueNameListBase.GetCount: Integer;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := 0;

  oList := LockList;
  Try
    for i := 0 to oList.Count do
      Result := Result + TsgcCacheQueue(oList[i]).Count;
  Finally
    UnlockList;
  End;
end;

function TsgcQueueNameListBase.GetQueueByName(const aQueueName: String)
  : TsgcQueueNameBase;
begin
  Result := GetQueue(aQueueName);
end;

function TsgcQueueNameListBase.GetQueue(const aQueueName: String)
  : TsgcQueueNameBase;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcQueueNameBase(oList[i]).QueueName = aQueueName then
      begin
        Result := TsgcQueueNameBase(oList[i]);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;

  if not Assigned(Result) then
    Result := AddQueue(aQueueName);
end;

function TsgcQueueNameListBase.GetQueuesDelimitedText: String;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oQueues: TStringList;
begin
  Result := '';

  oQueues := TStringList.Create;
  Try
    oList := LockList;
    Try
      for i := oList.Count - 1 Downto 0 do
        oQueues.Add(TsgcQueueNameBase(oList[i]).QueueName);
    Finally
      UnlockList;
    End;

    Result := oQueues.DelimitedText;
  Finally
    sgcFree(oQueues);
  End;
end;

constructor TsgcQueueNameItemBase.Create;
begin
  inherited;
  Queue := '';
end;

function DoSortByOrderAsc(Item1, Item2: Pointer): Integer;
begin
  if TsgcQueueItemBase(Item1).Order > TsgcQueueItemBase(Item2).Order then
    Result := 1
  else if TsgcQueueItemBase(Item1).Order < TsgcQueueItemBase(Item2).Order then
    Result := -1
  else
    Result := 0;
end;

function DoSortByOrderDesc(Item1, Item2: Pointer): Integer;
begin
  if TsgcQueueItemBase(Item1).Order > TsgcQueueItemBase(Item2).Order then
    Result := -1
  else if TsgcQueueItemBase(Item1).Order < TsgcQueueItemBase(Item2).Order then
    Result := 1
  else
    Result := 0;
end;

procedure TsgcQueue.SortByOrder(aAscending: Boolean = True);
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := LockList;
  Try
{$IFDEF NEXTGEN}
    if aAscending then
      oList.Sort(TComparer<TObject>.Construct(
        function(const Item1, Item2: TObject): Integer
        begin
          if TsgcQueueItemBase(Item1).Order > TsgcQueueItemBase(Item2).Order
          then
            Result := 1
          else if TsgcQueueItemBase(Item1).Order < TsgcQueueItemBase(Item2).Order
          then
            Result := -1
          else
            Result := 0;
        end))
    else
      oList.Sort(TComparer<TObject>.Construct(
        function(const Item1, Item2: TObject): Integer
        begin
          if TsgcQueueItemBase(Item1).Order > TsgcQueueItemBase(Item2).Order
          then
            Result := -1
          else if TsgcQueueItemBase(Item1).Order < TsgcQueueItemBase(Item2).Order
          then
            Result := 1
          else
            Result := 0;
        end));
{$ELSE}
    if aAscending then
      oList.Sort(DoSortByOrderAsc)
    else
      oList.Sort(DoSortByOrderDesc);
{$ENDIF}
  Finally
    UnlockList;
  End;
end;

end.
