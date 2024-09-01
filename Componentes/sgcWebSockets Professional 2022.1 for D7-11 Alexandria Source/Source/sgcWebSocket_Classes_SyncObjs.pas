{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Classes_SyncObjs;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  {$IFDEF SGC_DICTIONARY}
  System.Generics.Collections,
  {$ELSE}
    {$IFDEF NEXTGEN}System.Generics.Collections, System.Types, System.RTLConsts, {$ELSE}Contnrs, {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, SyncObjs{$ELSE}SyncObjs{$ENDIF};

Type
  TsgcThreadListType = (tdlThreadDictionary, tdlOnlyThread, tdlOnlyDictionary);

  TsgcCriticalSection = class(TSynchroObject)
  private
    {$IFDEF SGC_SPINLOCKS}
    FSpinlock: TSpinLock;
    {$ENDIF}
    {$IFNDEF SGC_SPINLOCKS}
      {$IFNDEF SGC_TMONITOR}
        {$IFDEF MSWINDOWS}
        FSection: TRTLCriticalSection;
        {$ELSE}
        FSection: TCriticalSection;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Acquire; override;
    procedure Release; override;
    function TryAcquire: Boolean;
  end;


  {$IFNDEF NEXTGEN}
  TsgcThreadList = class
  private
    FList: TList;
    {$IFDEF SGC_TMONITOR}
    FLock: TObject;
    {$ELSE}
    FLock: TsgcCriticalSection;
    {$ENDIF}
    FDuplicates: TDuplicates;
    {$IFDEF SGC_DICTIONARY}
    FDictionary: TDictionary <string,TObject>;
    {$ENDIF}
    FListType: TsgcThreadListType;
    function GetIsEmpty: Boolean;
    function GetListType: TsgcThreadListType;
    procedure SetListType(const Value: TsgcThreadListType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Add(Item: Pointer);
    procedure AddKey(Item: Pointer; const aKey: String);
    procedure Delete(Index: Integer);
    procedure DeleteKey(Index: Integer; const aKey: String);
    function GetValue(const aKey: String): TObject;
    procedure Clear; virtual;
    function LockList: TList;
    procedure Remove(Item: Pointer); {$IFNDEF D7}inline;{$ENDIF}
    procedure RemoveItem(Item: Pointer{$IFDEF D2010}; Direction: TList.TDirection{$ENDIF});
    procedure UnlockList; {$IFNDEF D7}inline;{$ENDIF}
  public
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property IsEmpty: Boolean read GetIsEmpty;
    property ListType: TsgcThreadListType read GetListType write SetListType;
  end;
  {$ELSE}
  TsgcThreadList<T> = class
  private
    FList: TList<T>;
    FLock: TObject;
    FDuplicates: TDuplicates;
  private
    function GetIsEmpty: Boolean;
    function GetListType: TsgcThreadListType;
    procedure SetListType(const Value: TsgcThreadListType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Add(const Item: T);
    procedure AddKey(const Item: T; const aKey: String);
    function LockList: TList<T>;
    procedure Delete(Index: Integer);
    procedure DeleteKey(Index: Integer; const aKey: String);
    procedure Remove(const Item: T); inline;
    procedure RemoveItem(const Item: T; Direction: TDirection);
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    procedure Clear; virtual;
    function GetValue(const aKey: String): TObject;
  public
    property IsEmpty: Boolean read GetIsEmpty;
    property ListType: TsgcThreadListType read GetListType write SetListType;
  end;
  {$ENDIF}

  TsgcThreadSafeInteger = class
  private
    FLock: TsgcCriticalSection;
    FValue: Int64;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    function Inc: Int64;
    function Dec: Int64;
  public
    property Value: Int64 read FValue;
  end;

  TsgcThreadSafeStringList = class(TStringList)
  private
    FLock: TsgcCriticalSection;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
  end;


implementation

uses
  sgcBase_Helpers;

{$IFNDEF DXE2}
Type
  IntPtr  = NativeInt;
{$ENDIF}

resourcestring
  S_DuplicateItem = 'List does not allow duplicates ($0%x)';

procedure TsgcCriticalSection.Acquire;
begin
  Try
    {$IFDEF SGC_SPINLOCKS}
    if FSpinLock.IsLocked = False then
      FSpinLock.Enter
    else
      FSpinLock.TryEnter(10);
    {$ENDIF}
    {$IFDEF SGC_TMONITOR}
    System.TMonitor.Enter(Self);
    {$ENDIF}
    {$IFNDEF SGC_SPINLOCKS}
      {$IFNDEF SGC_TMONITOR}
        {$IFDEF MSWINDOWS}
        EnterCriticalSection(FSection);
        {$ELSE}
        FSection.Enter;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  Except
    // nothing: system errors
  End;
end;

constructor TsgcCriticalSection.Create;
begin
  inherited Create;
  {$IFDEF SGC_SPINLOCKS}
  FSpinlock := TSpinLock.Create(True);
  {$ENDIF}
  {$IFNDEF SGC_SPINLOCKS}
    {$IFNDEF SGC_TMONITOR}
      {$IFDEF MSWINDOWS}
      InitializeCriticalSection(FSection);
      {$ELSE}
      FSection := TCriticalSection.Create;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

destructor TsgcCriticalSection.Destroy;
begin
  {$IFNDEF SGC_SPINLOCKS}
    {$IFNDEF SGC_TMONITOR}
      {$IFDEF MSWINDOWS}
      DeleteCriticalSection(FSection);
      {$ELSE}
      if Assigned(FSection) then
        sgcFree(FSection);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  inherited;
end;

procedure TsgcCriticalSection.Release;
begin
  Try
    {$IFDEF SGC_SPINLOCKS}
    if FSpinLock.IsLocked then
      FSpinlock.Exit;
    {$ENDIF}
    {$IFDEF SGC_TMONITOR}
    System.TMonitor.Exit(Self);
    {$ENDIF}
    {$IFNDEF SGC_SPINLOCKS}
      {$IFNDEF SGC_TMONITOR}
        {$IFDEF MSWINDOWS}
        LeaveCriticalSection(FSection);
        {$ELSE}
        FSection.Leave;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  Except
    // nothing: system errors
  End;
end;

function TsgcCriticalSection.TryAcquire: Boolean;
begin
  {$IFDEF SGC_SPINLOCKS}
    result := FSpinlock.TryEnter;
  {$ENDIF}
  {$IFDEF SGC_TMONITOR}
    result := System.TMonitor.TryEnter(Self);
  {$ENDIF}
  {$IFNDEF SGC_SPINLOCKS}
    {$IFNDEF SGC_TMONITOR}
      {$IFDEF MSWINDOWS}
      Result := TryEnterCriticalSection(FSection);
      {$ELSE}
      Result := True;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

constructor TsgcThreadSafeInteger.Create;
begin
  inherited Create;
  FLock := TsgcCriticalSection.Create;
  FValue := 0;
end;

destructor TsgcThreadSafeInteger.Destroy;
begin
  sgcFree(FLock);
  inherited;
end;

function TsgcThreadSafeInteger.Dec: Int64;
begin
  FLock.Acquire;
  Try
    FValue := FValue - 1;
    result := FValue;
  Finally
    FLock.Release;
  End;
end;

function TsgcThreadSafeInteger.Inc: Int64;
begin
  FLock.Acquire;
  Try
    FValue := FValue + 1;
    result := FValue;
  Finally
    FLock.Release;
  End;
end;

constructor TsgcThreadSafeStringList.Create;
begin
  FLock := TsgcCriticalSection.Create;
  inherited Create;
end;

destructor TsgcThreadSafeStringList.Destroy;
begin
  sgcFree(FLock);
  inherited;
end;

function TsgcThreadSafeStringList.Add(const S: string): Integer;
begin
  FLock.Acquire;
  Try
    result := inherited Add(S);
  Finally
    FLock.Release;
  End;
end;

procedure TsgcThreadSafeStringList.Clear;
begin
  FLock.Acquire;
  Try
    inherited Clear;
  Finally
    FLock.Release;
  End;
end;

procedure TsgcThreadSafeStringList.Delete(Index: Integer);
begin
  FLock.Acquire;
  Try
    inherited Delete(Index);
  Finally
    FLock.Release;
  End;
end;

function TsgcThreadSafeStringList.IndexOf(const S: string): Integer;
begin
  FLock.Acquire;
  Try
    result := inherited IndexOf(S);
  Finally
    FLock.Release;
  End;
end;

function TsgcThreadSafeStringList.IndexOfName(const Name: string): Integer;
begin
  FLock.Acquire;
  Try
    result := inherited IndexOfName(Name);
  Finally
    FLock.Release;
  End;
end;

{ TsgcThreadList }

{$IFNDEF NEXTGEN}
constructor TsgcThreadList.Create;
begin
  inherited Create;
  {$IFDEF SGC_TMONITOR}
  FLock := TObject.Create;
  {$ELSE}
  FLock := TsgcCriticalSection.Create;
  {$ENDIF}
  {$IFDEF SGC_DICTIONARY}
  FDictionary := TDictionary<string,TObject>.Create;
  {$ENDIF}
  FList := TList.Create;
  FDuplicates := dupIgnore;
end;

destructor TsgcThreadList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
  {$IFDEF SGC_DICTIONARY}
  sgcFree(FDictionary);
  {$ENDIF}
end;

procedure TsgcThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
    begin
      {$IFDEF LAZARUS}
      raise Exception.Create('Duplicate error');
      {$ELSE}
      FList.Error(@S_DuplicateItem, IntPtr(Item));
      {$ENDIF}
    end;
  finally
    UnlockList;
  end;
end;

procedure TsgcThreadList.AddKey(Item: Pointer; const aKey: String);
begin
  case ListType of
    tdlThreadDictionary:
      begin
        Add(Item);
        {$IFDEF SGC_DICTIONARY}
        if aKey <> '' then
        begin
          if FDictionary.ContainsKey(aKey) then
            FDictionary.Remove(aKey);
          FDictionary.Add(aKey, Item);
        end;
        {$ENDIF}
      end;
    tdlOnlyThread:
      Add(Item);
    tdlOnlyDictionary:
      {$IFDEF SGC_DICTIONARY}
      if aKey <> '' then
      begin
        if FDictionary.ContainsKey(aKey) then
          FDictionary.Remove(aKey);
        FDictionary.Add(aKey, Item);
      end;
      {$ENDIF}
  end;
end;

procedure TsgcThreadList.Clear;
begin
  LockList;
  try
  case ListType of
    tdlThreadDictionary:
      begin
        FList.Clear;
        {$IFDEF SGC_DICTIONARY}
        if Assigned(FDictionary) then
          FDictionary.Clear;
        {$ENDIF}
      end;
    tdlOnlyThread:
      FList.Clear;
    tdlOnlyDictionary:
      {$IFDEF SGC_DICTIONARY}
      if Assigned(FDictionary) then
        FDictionary.Clear;
      {$ENDIF}
  end;
  finally
    UnlockList;
  end;
end;

procedure TsgcThreadList.Delete(Index: Integer);
begin
  LockList;
  Try
    FList.Delete(Index);
  Finally
    UnLockList;
  End;
end;

procedure TsgcThreadList.DeleteKey(Index: Integer; const aKey: String);
begin
  case ListType of
    tdlThreadDictionary:
      begin
        Delete(Index);
        {$IFDEF SGC_DICTIONARY}
        if aKey <> '' then
          FDictionary.Remove(aKey);
        {$ENDIF}
      end;
    tdlOnlyThread:
      Delete(Index);
    tdlOnlyDictionary:
      begin
        {$IFDEF SGC_DICTIONARY}
        if aKey <> '' then
          FDictionary.Remove(aKey);
        {$ENDIF}
      end;
  end;
end;

function TsgcThreadList.GetIsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TsgcThreadList.GetListType: TsgcThreadListType;
begin
  {$IFDEF SGC_DICTIONARY}
  Result := FListType;
  {$ELSE}
  Result := tdlOnlyThread;
  {$ENDIF}
end;

function TsgcThreadList.GetValue(const aKey: String): TObject;
begin
  result := nil;
  case ListType of
    tdlThreadDictionary, tdlOnlyDictionary:
      {$IFDEF SGC_DICTIONARY}FDictionary.TryGetValue(aKey, result){$ENDIF};
  end;
end;

function TsgcThreadList.LockList: TList;
begin
  {$IFDEF SGC_TMONITOR}
  TMonitor.Enter(FLock);
  {$ELSE}
  FLock.Acquire;
  {$ENDIF}
  Result := FList;
end;

procedure TsgcThreadList.Remove(Item: Pointer);
begin
  RemoveItem(Item{$IFDEF D2010}, TList.TDirection.FromBeginning{$ENDIF});
end;

procedure TsgcThreadList.RemoveItem(Item: Pointer{$IFDEF D2010}; Direction: TList.TDirection{$ENDIF});
begin
  LockList;
  try
    {$IFDEF D2010}
    FList.RemoveItem(Item, Direction);
    {$ELSE}
    FList.Remove(Item);
    {$ENDIF}
  finally
    UnlockList;
  end;
end;

procedure TsgcThreadList.SetListType(const Value: TsgcThreadListType);
begin
  {$IFDEF SGC_DICTIONARY}
  FListType := Value;
  {$ELSE}
  FListType := tdlOnlyThread;
  {$ENDIF}
end;

procedure TsgcThreadList.UnlockList;
begin
  {$IFDEF SGC_TMONITOR}
  TMonitor.Exit(FLock);
  {$ELSE}
  FLock.Release;
  {$ENDIF}
end;

{$ELSE}
constructor TsgcThreadList<T>.Create;
begin
  inherited Create;
  FLock := TObject.Create;
  FList := TList<T>.Create;
  FDuplicates := dupIgnore;
end;

destructor TsgcThreadList<T>.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

procedure TsgcThreadList<T>.Add(const Item: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      raise EListError.CreateFmt(SDuplicateItem, [FList.ItemValue(Item)]);
  finally
    UnlockList;
  end;
end;

procedure TsgcThreadList<T>.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TsgcThreadList<T>.GetListType: TsgcThreadListType;
begin
  result := tdlOnlyThread;
end;

procedure TsgcThreadList<T>.SetListType(const Value: TsgcThreadListType);
begin
  // always tdlOnlyThread;
end;

procedure TsgcThreadList<T>.AddKey(const Item: T; const aKey: String);
begin
  Add(Item);
end;

procedure TsgcThreadList<T>.DeleteKey(Index: Integer; const aKey: String);
begin
  Delete(Index);
end;

procedure TsgcThreadList<T>.Delete(Index: Integer);
var
  oList: TList<T>;
begin
  oList := LockList;
  Try
    oList.Delete(Index);
  Finally
    UnLockList;
  End;
end;

function TsgcThreadList<T>.GetIsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TsgcThreadList<T>.GetValue(const aKey: String): TObject;
begin
  result := nil;
end;

function TsgcThreadList<T>.LockList: TList<T>;
begin
  TMonitor.Enter(FLock);
  Result := FList;
end;

procedure TsgcThreadList<T>.Remove(const Item: T);
begin
  RemoveItem(Item, TDirection.FromBeginning);
end;

procedure TsgcThreadList<T>.RemoveItem(const Item: T; Direction: TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;

procedure TsgcThreadList<T>.UnlockList;
begin
  TMonitor.Exit(FLock);
end;


{$ENDIF}


end.
