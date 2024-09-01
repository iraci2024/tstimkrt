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

unit dxShellBreadcrumbEdit;

{$I cxVer.inc}

interface

uses
  Types, ActiveX, ShlObj, SysUtils, Classes, Messages, Graphics, ImgList,
  Controls,
  dxCustomTree, dxBreadcrumbEdit, cxShellCommon, cxShellControls,
  cxShellListView, cxShellComboBox, cxShellTreeView, dxCoreClasses;

type
  TdxShellBreadcrumbEditNode = class;
  TdxShellBreadcrumbEditRoot = class(TcxCustomShellRoot);

  IdxShellBreadcrumbEditEvents = interface(IdxBreadcrumbEditEvents)
    ['{73A3108B-0FC0-41D7-A885-B8F7C2431779}']
    procedure AddFolder(AFolder: TcxShellFolder; var ACanAdd: Boolean);
    procedure ShellChanged(AEventID: DWORD; APIDL1, APIDL2: PItemIDList);
  end;

  TdxShellBreadcrumbEditShellOptionsChange = (bcescContent, bcescRoot,
    bcescTracking);
  TdxShellBreadcrumbEditShellOptionsChanges = set of
    TdxShellBreadcrumbEditShellOptionsChange;
  TdxShellBreadcrumbEditShellOptionsChangeEvent = procedure(Sender: TObject;
    const AChanges: TdxShellBreadcrumbEditShellOptionsChanges) of object;

  { TdxShellBreadcrumbEditShellOptions }

  TdxShellBreadcrumbEditShellOptions = class(TcxOwnedPersistent)
  strict private
    FRoot: TdxShellBreadcrumbEditRoot;
    FShowHiddenFolders: Boolean;
    FTrackShellChanges: Boolean;
    FShowZipFilesWithFolders: Boolean;

    FOnChange: TdxShellBreadcrumbEditShellOptionsChangeEvent;

    procedure DoFolderChanged(Sender: TObject; Root: TcxCustomShellRoot);
    procedure DoSettingsChanged(Sender: TObject);
    procedure SetRoot(AValue: TdxShellBreadcrumbEditRoot);
    procedure SetShowHiddenFolders(AValue: Boolean);
    procedure SetShowZipFilesWithFolders(AValue: Boolean);
    procedure SetTrackShellChanges(AValue: Boolean);
  protected
    procedure Changed(AChanges
      : TdxShellBreadcrumbEditShellOptionsChanges); virtual;
    //
    property OnChange: TdxShellBreadcrumbEditShellOptionsChangeEvent
      read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Root: TdxShellBreadcrumbEditRoot read FRoot write SetRoot;
    property ShowHiddenFolders: Boolean read FShowHiddenFolders
      write SetShowHiddenFolders default False;
    property ShowZipFilesWithFolders: Boolean read FShowZipFilesWithFolders
      write SetShowZipFilesWithFolders default True;
    property TrackShellChanges: Boolean read FTrackShellChanges
      write SetTrackShellChanges default True;
  end;

  { TdxShellBreadcrumbEditProperties }

  TdxShellBreadcrumbEditProperties = class(TdxCustomBreadcrumbEditProperties)
  strict private
    FShellImageList: TCustomImageList;
    FShellOptions: TdxShellBreadcrumbEditShellOptions;

    FOnShellOptionsChanged: TdxShellBreadcrumbEditShellOptionsChangeEvent;

    function CreateShellImageList: TCustomImageList;
    procedure ShellOptionsChanged(Sender: TObject;
      const AChanges: TdxShellBreadcrumbEditShellOptionsChanges);
    procedure SetShellOptions(AValue: TdxShellBreadcrumbEditShellOptions);
  protected
    property OnShellOptionsChanged
      : TdxShellBreadcrumbEditShellOptionsChangeEvent
      read FOnShellOptionsChanged write FOnShellOptionsChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Borders;
    property Buttons;
    property ButtonImages;
    property DropDownIndent;
    property DropDownRows;
    property PathEditor;
    property ProgressBar;
    property ShellOptions: TdxShellBreadcrumbEditShellOptions read FShellOptions
      write SetShellOptions;
  end;

  { TdxShellBreadcrumbEditProducer }

  TdxShellBreadcrumbEditProducer = class(TcxCustomItemProducer)
  strict private
    FNode: TdxShellBreadcrumbEditNode;
    FShellItemInfo: TcxShellItemInfo;

    function GetIsInitialized: Boolean;
  protected
    function CanAddFolder(AFolder: TcxShellFolder): Boolean; override;
    function GetEnumFlags: Cardinal; override;
    function GetShowToolTip: Boolean; override;
    function SlowInitializationDone(AItem: TcxShellItemInfo): Boolean; override;
    procedure DoSlowInitialization(AItem: TcxShellItemInfo); override;
  public
    constructor Create(ANode: TdxShellBreadcrumbEditNode); reintroduce; virtual;
    procedure CheckInitialized;
    function GetNodeByPidl(APidl: PItemIDList)
      : TdxShellBreadcrumbEditNode; virtual;
    procedure ProcessItems; reintroduce; overload;
    procedure SetItemsCount(Count: Integer); override;
    //
    property IsInitialized: Boolean read GetIsInitialized;
    property Node: TdxShellBreadcrumbEditNode read FNode;
    property ShellItemInfo: TcxShellItemInfo read FShellItemInfo
      write FShellItemInfo;
  end;

  { TdxShellBreadcrumbEditNode }

  TdxShellBreadcrumbEditNode = class(TdxBreadcrumbEditNode)
  strict private
    FAbsolutePidl: PItemIDList;
    FProducer: TdxShellBreadcrumbEditProducer;
    FShellFolder: IShellFolder;

    procedure FreeShellObjects;
    function GetAbsolutePidl: PItemIDList;
    function GetEdit: IdxBreadcrumbEdit;
    function GetFolderName: string;
    function GetItem(AIndex: Integer): TdxShellBreadcrumbEditNode;
    function GetOptions: TdxShellBreadcrumbEditShellOptions;
    function GetParent: TdxShellBreadcrumbEditNode;
    function GetPidl: PItemIDList;
    function GetRealPath: string;
    function GetShellFolder: IShellFolder;
    procedure SetPidl(const Value: PItemIDList);
  protected
    function AreChildrenLoaded: Boolean; override;
    function GetPath: string; override;
    procedure Initialize(AItemInfo: TcxShellItemInfo); virtual;
    //
    property AbsolutePidl: PItemIDList read GetAbsolutePidl;
    property Edit: IdxBreadcrumbEdit read GetEdit;
    property Options: TdxShellBreadcrumbEditShellOptions read GetOptions;
    property Pidl: PItemIDList read GetPidl write SetPidl;
    property Producer: TdxShellBreadcrumbEditProducer read FProducer;
    property ShellFolder: IShellFolder read GetShellFolder;
  public
    constructor Create(AOwner: IdxTreeOwner); override;
    destructor Destroy; override;
    function AddChild: TdxShellBreadcrumbEditNode; overload;
    function Compare(const AName: string): Boolean; override;
    //
    property FolderName: string read GetFolderName;
    property Items[AIndex: Integer]: TdxShellBreadcrumbEditNode
      read GetItem; default;
    property Parent: TdxShellBreadcrumbEditNode read GetParent;
    property RealPath: string read GetRealPath;
  end;

  { TdxShellBreadcrumbEditController }

  TdxShellBreadcrumbEditController = class(TdxBreadcrumbEditController)
  public type
    TState = record
      DropDownMenuOwnerPidl: PItemIDList;
      DropDownMenuState: Boolean;
      SelectedPidl: PItemIDList;
    end;
  strict private
    FShellChangeNotifierData: TcxShellChangeNotifierData;
    FUpdateContentLockCount: Integer;

    function GetPathDelimiter: Char;
    function GetRoot: TdxShellBreadcrumbEditNode;
    function GetSelected: TdxShellBreadcrumbEditNode;
    function GetSelectedPidl: PItemIDList;
    function GetSelectedRealPath: string;
    function GetShellOptions: TdxShellBreadcrumbEditShellOptions;
    procedure SetSelected(AValue: TdxShellBreadcrumbEditNode);
    procedure SetSelectedPidl(AValue: PItemIDList);
    procedure ShellChangeNotify(AEventID: Longint; APIDL1, APIDL2: PItemIDList);
  protected
    function CreatePathEditingController
      : TdxBreadcrumbEditPathEditingController; override;
    procedure DoAfterSelect; override;
    procedure DoShellChangeNotify(AEventID: Longint;
      APIDL1, APIDL2: PItemIDList); virtual;
    function FindRootNodeForPath(var APath: string;
      out ANode: TdxBreadcrumbEditNode): Boolean; override;
    function GetPidlByPath(APath: string; out APidl: PItemIDList)
      : Boolean; virtual;
    procedure RemoveChangeNotification;
    procedure RestoreSelection(APidl: PItemIDList);
    procedure WndProc(var Message: TMessage); override;
  public
    function FindNodeByPath(APath: string): TdxBreadcrumbEditNode; override;
    function FindNodeByPidl(APidl: PItemIDList; AIgnoreFile: Boolean = False)
      : TdxShellBreadcrumbEditNode;
    procedure CancelSelectionChanges;
    procedure BeginUpdateContent(var AState: TState);
    procedure EndUpdateContent(var AState: TState);
    function IsUpdatingContent: Boolean;
    procedure UpdateContent;
    procedure UpdateTrackingSettings;
    //
    property PathDelimiter: Char read GetPathDelimiter;
    property Root: TdxShellBreadcrumbEditNode read GetRoot;
    property Selected: TdxShellBreadcrumbEditNode read GetSelected
      write SetSelected;
    property SelectedPidl: PItemIDList read GetSelectedPidl
      write SetSelectedPidl;
    property SelectedRealPath: string read GetSelectedRealPath;
    property ShellOptions: TdxShellBreadcrumbEditShellOptions
      read GetShellOptions;
  end;

  { TdxShellBreadcrumbPathEditingController }

  TdxShellBreadcrumbPathEditingController = class
    (TdxBreadcrumbEditPathEditingController)
  strict private
    function GetController: TdxShellBreadcrumbEditController; inline;
  protected
    procedure PopulateSuggestions(const APath: string;
      ASuggestions: TStringList); override;
  public
    procedure ReleasePathEditor; override;
    //
    property Controller: TdxShellBreadcrumbEditController read GetController;
  end;

  { TdxCustomShellBreadcrumbEdit }

  TdxCustomShellBreadcrumbEdit = class(TdxCustomBreadcrumbEdit,
    IcxShellDependedControls, IdxShellBreadcrumbEditEvents, IcxShellRoot)
  strict private
    FDependedControls: TcxShellDependedControls;
    FNavigationLockCount: Integer;
    FShellComboBox: TcxCustomShellComboBox;
    FShellListView: TcxCustomShellListView;
    FShellTreeView: TcxCustomShellTreeView;

    FOnAddFolder: TcxShellAddFolderEvent;
    FOnRootChanged: TcxRootChangedEvent;
    FOnShellChange: TcxShellChangeEvent;

    function GetController: TdxShellBreadcrumbEditController;
    function GetProperties: TdxShellBreadcrumbEditProperties;
    function GetRoot: TdxShellBreadcrumbEditNode;
    function GetSelectedPidl: PItemIDList;
    function IsParentLoading: Boolean;
    procedure ShellOptionsChangeHandler(Sender: TObject;
      const AChanges: TdxShellBreadcrumbEditShellOptionsChanges);
    procedure SetProperties(AValue: TdxShellBreadcrumbEditProperties);
    procedure SetSelectedPidl(AValue: PItemIDList);
    procedure SetShellComboBox(AValue: TcxCustomShellComboBox);
    procedure SetShellListView(AValue: TcxCustomShellListView);
    procedure SetShellTreeView(AValue: TcxCustomShellTreeView);
    //
    procedure DSMNavigate(var Message: TMessage); message DSM_DONAVIGATE;
    procedure DSMNotifyUpdateContents(var Message: TMessage);
      message DSM_NOTIFYUPDATECONTENTS;
    procedure DSMSynchronizeRoot(var Message: TMessage);
      message DSM_SYNCHRONIZEROOT;
  protected
    function CreateController: TdxBreadcrumbEditController; override;
    function CreateProperties: TdxCustomBreadcrumbEditProperties; override;
    function CreateRoot: TdxBreadcrumbEditNode; override;
    procedure DestroyWnd; override;
    procedure LoadChildren(ASender: TdxTreeCustomNode); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RootChanged; virtual;
    procedure SelectionChanged; override;
    procedure ShellOptionsChanged(const AChanges
      : TdxShellBreadcrumbEditShellOptionsChanges); virtual;
    procedure SynchronizeDependedControls; virtual;
    procedure SynchronizeRoot; virtual;
    //
    function GetSelectedPath: string; override;
    // IdxShellBreadcrumbEditEvents
    procedure AddFolder(AFolder: TcxShellFolder; var ACanAdd: Boolean);
    procedure ShellChanged(AEventID: DWORD; APIDL1, APIDL2: PItemIDList);
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function IcxShellRoot.GetRoot = GetShellRoot;
    function GetShellRoot: TcxCustomShellRoot;
    //
    property Controller: TdxShellBreadcrumbEditController read GetController;
    property DependedControls: TcxShellDependedControls read FDependedControls;
    property Root: TdxShellBreadcrumbEditNode read GetRoot;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateContent;
    //
    property Properties: TdxShellBreadcrumbEditProperties read GetProperties
      write SetProperties;
    property SelectedPidl: PItemIDList read GetSelectedPidl
      write SetSelectedPidl;
    property ShellComboBox: TcxCustomShellComboBox read FShellComboBox
      write SetShellComboBox;
    property ShellListView: TcxCustomShellListView read FShellListView
      write SetShellListView;
    property ShellTreeView: TcxCustomShellTreeView read FShellTreeView
      write SetShellTreeView;
    //
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder
      write FOnAddFolder;
    property OnRootChanged: TcxRootChangedEvent read FOnRootChanged
      write FOnRootChanged;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange
      write FOnShellChange;
  end;

  { TdxShellBreadcrumbEdit }

  TdxShellBreadcrumbEdit = class(TdxCustomShellBreadcrumbEdit)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Enabled;
    property Font;
    property LookAndFeel;
    property ParentBiDiMode;
    property ParentShowHint;
    property Properties;
    property ShellComboBox;
    property ShellListView;
    property ShellTreeView;
    property ShowHint;
    property TabOrder;
    property Transparent;
    property Visible;

    property OnAddFolder;
    property OnButtonClick;
    property OnPathEntered;
    property OnPathSelected;
    property OnPathValidate;
    property OnPopulateAutoCompleteSuggestions;
    property OnRootChanged;
    property OnShellChange;
  end;

implementation

uses
  ShellAPI, dxCore, cxImageList, dxDPIAwareUtils;

{ TdxShellBreadcrumbEditNode }

constructor TdxShellBreadcrumbEditNode.Create(AOwner: IdxTreeOwner);
begin
  inherited Create(AOwner);
  FProducer := TdxShellBreadcrumbEditProducer.Create(Self);
end;

destructor TdxShellBreadcrumbEditNode.Destroy;
begin
  FreeShellObjects;
  FreeAndNil(FProducer);
  inherited Destroy;
end;

function TdxShellBreadcrumbEditNode.AddChild: TdxShellBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode(inherited AddChild);
end;

function TdxShellBreadcrumbEditNode.Compare(const AName: string): Boolean;
begin
  Result := inherited Compare(AName) or SameText(FolderName, AName);
end;

procedure TdxShellBreadcrumbEditNode.FreeShellObjects;
begin
  FShellFolder := nil;
  DisposePidl(FAbsolutePidl);
  FAbsolutePidl := nil;
end;

procedure TdxShellBreadcrumbEditNode.Initialize(AItemInfo: TcxShellItemInfo);

  function GetRootNodeInfo(AShellRoot: TdxShellBreadcrumbEditRoot;
    out AFolder: TcxShellFolder; out APidl: PItemIDList): Boolean;
  begin
    if AShellRoot.ShellFolder = nil then
      AShellRoot.CheckRoot;
    Result := AShellRoot.IsValid;
    if Result then
    begin
      AFolder := AShellRoot.Folder;
      APidl := AShellRoot.Pidl;
    end;
  end;

  function GetNodeInfo(out AFolder: TcxShellFolder;
    out AIconIndex: Integer): Boolean;
  var
    AFileInfo: TShFileInfo;
    APidl: PItemIDList;
  begin
    if IsRoot then
      Result := GetRootNodeInfo(Options.Root, AFolder, APidl)
    else
    begin
      APidl := AItemInfo.FullPIDL;
      AFolder := AItemInfo.Folder;
      Result := True;
    end;

    if Result then
    begin
      cxShellGetThreadSafeFileInfo(PChar(APidl), 0, AFileInfo,
        SizeOf(AFileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX);
      AIconIndex := AFileInfo.iIcon;
    end;
  end;

var
  AFolder: TcxShellFolder;
  AIconIndex: Integer;
begin
  BeginUpdate;
  try
    DeleteChildren;
    FreeShellObjects;
    Producer.ShellItemInfo := AItemInfo;
    if GetNodeInfo(AFolder, AIconIndex) then
    begin
      FName := AFolder.DisplayName;
      HasChildren := AFolder.SubFolders;
      ImageIndex := AIconIndex;
      IsHidden := not Options.ShowHiddenFolders and
        (sfaHidden in AFolder.Attributes);
    end;
    Notify([tnStructure, tnData]);
  finally
    EndUpdate;
  end;
end;

function TdxShellBreadcrumbEditNode.GetAbsolutePidl: PItemIDList;
begin
  if FAbsolutePidl = nil then
  begin
    if IsRoot then
      FAbsolutePidl := GetPidlCopy(Pidl)
    else
      FAbsolutePidl := ConcatenatePidls(Parent.AbsolutePidl, Pidl);
  end;
  Result := FAbsolutePidl;
end;

function TdxShellBreadcrumbEditNode.GetEdit: IdxBreadcrumbEdit;
begin
  Result := FOwner as IdxBreadcrumbEdit;
end;

function TdxShellBreadcrumbEditNode.GetFolderName: string;
var
  ATempPath: string;
begin
  ATempPath := dxExcludeTrailingPathDelimiter(Path, PathDelimiter);
  Result := dxExtractFileName(ATempPath, PathDelimiter);
  if Result = '' then
    Result := ATempPath;
end;

function TdxShellBreadcrumbEditNode.GetItem(AIndex: Integer)
  : TdxShellBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode(inherited Items[AIndex]);
end;

function TdxShellBreadcrumbEditNode.AreChildrenLoaded: Boolean;
begin
  Result := not(nsHasChildren in State);
end;

function TdxShellBreadcrumbEditNode.GetPath: string;
begin
  Result := RealPath;
  if (Result = '') or IsRoot then
    Result := inherited GetPath;
end;

function TdxShellBreadcrumbEditNode.GetParent: TdxShellBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode(inherited Parent);
end;

function TdxShellBreadcrumbEditNode.GetPidl: PItemIDList;
begin
  if IsRoot then
    Result := Options.Root.Pidl
  else
    Result := Producer.ShellItemInfo.Pidl;
end;

function TdxShellBreadcrumbEditNode.GetRealPath: string;
begin
  Result := dxReplacePathDelimiter(GetPidlName(AbsolutePidl), PathDelim,
    PathDelimiter);
end;

function TdxShellBreadcrumbEditNode.GetShellFolder: IShellFolder;
begin
  if FShellFolder = nil then
  begin
    if IsRoot then
      FShellFolder := Options.Root.ShellFolder
    else if Failed(Parent.ShellFolder.BindToObject(Pidl, nil, IID_IShellFolder,
      FShellFolder)) then
      FShellFolder := nil;
  end;
  Result := FShellFolder;
end;

function TdxShellBreadcrumbEditNode.GetOptions
  : TdxShellBreadcrumbEditShellOptions;
begin
  Result := (Edit.GetProperties as TdxShellBreadcrumbEditProperties)
    .ShellOptions;
end;

procedure TdxShellBreadcrumbEditNode.SetPidl(const Value: PItemIDList);
var
  AShellFolder: TcxShellFolder;
begin
  if Parent <> nil then
  begin
    Producer.CheckInitialized;
    Producer.ShellItemInfo.SetNewPidl(Producer.ShellFolder,
      Producer.FolderPidl, Value);
    FreeShellObjects;

    AShellFolder := TcxShellFolder.Create(AbsolutePidl);
    try
      FName := AShellFolder.DisplayName;
    finally
      AShellFolder.Free;
    end;
    Notify([tnData]);
  end;
end;

{ TdxShellBreadcrumbEditProducer }

constructor TdxShellBreadcrumbEditProducer.Create
  (ANode: TdxShellBreadcrumbEditNode);
begin
  inherited Create(ANode.Edit.GetContainer);
  FNode := ANode;
end;

function TdxShellBreadcrumbEditProducer.CanAddFolder
  (AFolder: TcxShellFolder): Boolean;
var
  AEvents: IdxShellBreadcrumbEditEvents;
begin
  Result := (AFolder <> nil) and AFolder.IsFolder and
    (Node.Options.ShowZipFilesWithFolders or not cxShellIsZipFile(AFolder));
  if Result then
  begin
    if Supports(Node.Edit, IdxShellBreadcrumbEditEvents, AEvents) then
      AEvents.AddFolder(AFolder, Result);
  end;
end;

procedure TdxShellBreadcrumbEditProducer.CheckInitialized;
begin
  if not IsInitialized then
    Initialize(Node.ShellFolder, Node.AbsolutePidl);
end;

function TdxShellBreadcrumbEditProducer.SlowInitializationDone
  (AItem: TcxShellItemInfo): Boolean;
begin
  Result := AItem.Updated;
end;

procedure TdxShellBreadcrumbEditProducer.DoSlowInitialization
  (AItem: TcxShellItemInfo);
begin
  InitializeItem(AItem);
end;

procedure TdxShellBreadcrumbEditProducer.ProcessItems;
var
  AFolder: IShellFolder;
  APidl: PItemIDList;
begin
  ClearItems;
  AFolder := Node.ShellFolder;
  if AFolder <> nil then
    try
      APidl := GetPidlCopy(Node.AbsolutePidl);
      if APidl <> nil then
        try
          ProcessItems(AFolder, APidl, 0);
        finally
          DisposePidl(APidl);
        end;
    finally
      AFolder := nil;
    end;
end;

function TdxShellBreadcrumbEditProducer.GetEnumFlags: Cardinal;
begin
  Result := SHCONTF_FOLDERS;
  if Node.Options.ShowHiddenFolders then
    Result := Result or SHCONTF_INCLUDEHIDDEN;
end;

function TdxShellBreadcrumbEditProducer.GetIsInitialized: Boolean;
begin
  Result := ShellFolder <> nil;
end;

function TdxShellBreadcrumbEditProducer.GetNodeByPidl(APidl: PItemIDList)
  : TdxShellBreadcrumbEditNode;
var
  AIndex: Integer;
  AItemInfo: TcxShellItemInfo;
begin
  AIndex := GetItemIndexByPidl(APidl);
  if AIndex = -1 then
  begin
    LockWrite;
    try
      CheckInitialized;
      AItemInfo := InternalAddItem(APidl);
      if AItemInfo <> nil then
      begin
        Node.BeginUpdate;
        try
          Sort;
          InitializeItem(AItemInfo);
          SetItemsCount(Items.Count);
          AIndex := GetItemIndexByPidl(APidl);
        finally
          Node.EndUpdate;
        end;
      end;
    finally
      UnlockWrite;
    end;
  end;

  if (AIndex > -1) and (AIndex < Node.Count) then
    Result := Node[AIndex]
  else
    Result := nil;
end;

function TdxShellBreadcrumbEditProducer.GetShowToolTip: Boolean;
begin
  Result := False;
end;

procedure TdxShellBreadcrumbEditProducer.SetItemsCount(Count: Integer);
var
  APrevState: TdxTreeNodeStates;
  I: Integer;
begin
  LockRead;
  try
    Node.BeginUpdate;
    try
      APrevState := Node.State;
      try
        Node.DeleteChildren;
        for I := 0 to Count - 1 do
          Node.AddChild.Initialize(Items[I]);
      finally
        Node.State := APrevState;
      end;
    finally
      Node.EndUpdate;
    end;
  finally
    UnlockRead;
  end;
end;

{ TdxShellBreadcrumbEditProperties }

constructor TdxShellBreadcrumbEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FShellOptions := TdxShellBreadcrumbEditShellOptions.Create(Self);
  FShellOptions.OnChange := ShellOptionsChanged;
  FShellImageList := CreateShellImageList;
  Images := FShellImageList;
end;

destructor TdxShellBreadcrumbEditProperties.Destroy;
begin
  Images := nil;
  FreeAndNil(FShellImageList);
  FreeAndNil(FShellOptions);
  inherited Destroy;
end;

procedure TdxShellBreadcrumbEditProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Images := FShellImageList;
  if Source is TdxShellBreadcrumbEditProperties then
    ShellOptions := TdxShellBreadcrumbEditProperties(Source).ShellOptions;
end;

function TdxShellBreadcrumbEditProperties.CreateShellImageList
  : TCustomImageList;
begin
  Result := TcxShellImageList.Create(SHGFI_SMALLICON);
end;

procedure TdxShellBreadcrumbEditProperties.ShellOptionsChanged(Sender: TObject;
  const AChanges: TdxShellBreadcrumbEditShellOptionsChanges);
begin
  if Assigned(OnShellOptionsChanged) then
    OnShellOptionsChanged(Self, AChanges);
end;

procedure TdxShellBreadcrumbEditProperties.SetShellOptions
  (AValue: TdxShellBreadcrumbEditShellOptions);
begin
  FShellOptions.Assign(AValue);
end;

{ TdxShellBreadcrumbEditShellOptions }

constructor TdxShellBreadcrumbEditShellOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FRoot := TdxShellBreadcrumbEditRoot.Create(Self, 0);
  FRoot.OnSettingsChanged := DoSettingsChanged;
  FRoot.OnFolderChanged := DoFolderChanged;
  FShowZipFilesWithFolders := True;
  FTrackShellChanges := True;
end;

destructor TdxShellBreadcrumbEditShellOptions.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TdxShellBreadcrumbEditShellOptions.Assign(Source: TPersistent);
begin
  if Source is TdxShellBreadcrumbEditShellOptions then
  begin
    Root := TdxShellBreadcrumbEditShellOptions(Source).Root;
    ShowHiddenFolders := TdxShellBreadcrumbEditShellOptions(Source)
      .ShowHiddenFolders;
    TrackShellChanges := TdxShellBreadcrumbEditShellOptions(Source)
      .TrackShellChanges;
    ShowZipFilesWithFolders := TdxShellBreadcrumbEditShellOptions(Source)
      .ShowZipFilesWithFolders;
  end;
end;

procedure TdxShellBreadcrumbEditShellOptions.Changed
  (AChanges: TdxShellBreadcrumbEditShellOptionsChanges);
begin
  if Assigned(OnChange) then
    OnChange(Self, AChanges);
end;

procedure TdxShellBreadcrumbEditShellOptions.DoFolderChanged(Sender: TObject;
  Root: TcxCustomShellRoot);
begin
  Changed([bcescContent, bcescRoot]);
end;

procedure TdxShellBreadcrumbEditShellOptions.DoSettingsChanged(Sender: TObject);
begin
  Changed([bcescContent]);
end;

procedure TdxShellBreadcrumbEditShellOptions.SetRoot
  (AValue: TdxShellBreadcrumbEditRoot);
begin
  FRoot.Assign(AValue);
end;

procedure TdxShellBreadcrumbEditShellOptions.SetShowHiddenFolders
  (AValue: Boolean);
begin
  if FShowHiddenFolders <> AValue then
  begin
    FShowHiddenFolders := AValue;
    Changed([bcescContent]);
  end;
end;

procedure TdxShellBreadcrumbEditShellOptions.SetShowZipFilesWithFolders
  (AValue: Boolean);
begin
  if FShowZipFilesWithFolders <> AValue then
  begin
    FShowZipFilesWithFolders := AValue;
    Changed([bcescContent]);
  end;
end;

procedure TdxShellBreadcrumbEditShellOptions.SetTrackShellChanges
  (AValue: Boolean);
begin
  if FTrackShellChanges <> AValue then
  begin
    FTrackShellChanges := AValue;
    Changed([bcescTracking]);
  end;
end;

{ TdxShellBreadcrumbEditController }

procedure TdxShellBreadcrumbEditController.CancelSelectionChanges;
begin
  Exclude(FChanges, bcecSelection);
end;

function TdxShellBreadcrumbEditController.CreatePathEditingController
  : TdxBreadcrumbEditPathEditingController;
begin
  Result := TdxShellBreadcrumbPathEditingController.Create(Self);
end;

procedure TdxShellBreadcrumbEditController.DoAfterSelect;
begin
  inherited DoAfterSelect;
  UpdateTrackingSettings;
end;

procedure TdxShellBreadcrumbEditController.DoShellChangeNotify
  (AEventID: Longint; APIDL1, APIDL2: PItemIDList);
var
  AEvents: IdxShellBreadcrumbEditEvents;
begin
  if Supports(Control, IdxShellBreadcrumbEditEvents, AEvents) then
    try
      AEvents.ShellChanged(AEventID, APIDL1, APIDL2);
    finally
      AEvents := nil;
    end;
end;

function TdxShellBreadcrumbEditController.FindNodeByPath(APath: string)
  : TdxBreadcrumbEditNode;
var
  APidl: PItemIDList;
begin
  Result := nil;
  if GetPidlByPath(APath, APidl) then
    try
      Result := FindNodeByPidl(APidl);
    finally
      DisposePidl(APidl);
    end;
  if Result = nil then
    Result := inherited FindNodeByPath(APath);
end;

function TdxShellBreadcrumbEditController.FindNodeByPidl(APidl: PItemIDList;
  AIgnoreFile: Boolean = False): TdxShellBreadcrumbEditNode;
var
  AItemInfo: TcxShellItemInfo;
  AParentNode: TdxShellBreadcrumbEditNode;
  APartDestPidl: PItemIDList;
  ASourcePidl: PItemIDList;
  I: Integer;
begin
  Result := nil;
  ASourcePidl := ShellOptions.Root.Pidl;
  if GetPidlItemsCount(ASourcePidl) <= GetPidlItemsCount(APidl) then
  begin
    for I := 0 to GetPidlItemsCount(ASourcePidl) - 1 do
      APidl := GetNextItemID(APidl);

    Result := Root;
    for I := 0 to GetPidlItemsCount(APidl) - 1 do
    begin
      APartDestPidl := ExtractParticularPidl(APidl);
      APidl := GetNextItemID(APidl);
      if APartDestPidl <> nil then
        try
          AParentNode := Result;
          Result := Result.Producer.GetNodeByPidl(APartDestPidl);

          if Result = nil then
          begin
            if (AParentNode <> nil) and AIgnoreFile then
            begin
              AParentNode.Producer.CheckInitialized;
              AItemInfo := AParentNode.Producer.CreateShellItemInfo
                (APartDestPidl, False);
              try
                if not AItemInfo.IsFolder then
                  Result := AParentNode;
              finally
                AItemInfo.Free;
              end;
            end;
            Break;
          end;
        finally
          DisposePidl(APartDestPidl);
        end
      else
        Break;
    end;
  end;
end;

procedure TdxShellBreadcrumbEditController.ShellChangeNotify(AEventID: Longint;
  APIDL1, APIDL2: PItemIDList);
var
  AItemInfo: TcxShellItemInfo;
  ANode: TdxShellBreadcrumbEditNode;
  AProducer: TdxShellBreadcrumbEditProducer;
begin
  try
    case AEventID of
      SHCNE_RMDIR:
        if (Selected <> nil) and Selected.AreChildrenLoaded then
          FindNodeByPidl(APIDL1).Free;

      SHCNE_RENAMEFOLDER:
        if (Selected <> nil) and Selected.AreChildrenLoaded then
        begin
          ANode := FindNodeByPidl(APIDL1);
          if ANode <> nil then
            ANode.Pidl := GetLastPidlItem(APIDL2);
        end;

      SHCNE_MKDIR:
        if (Selected <> nil) and Selected.AreChildrenLoaded and
          (FindNodeByPidl(APIDL1) = nil) then
        begin
          BeginUpdate;
          try
            AProducer := Selected.Producer;
            AProducer.CheckInitialized;
            AItemInfo := AProducer.CreateShellItemInfo(APIDL1, False);
            if AItemInfo <> nil then
            begin
              AProducer.LockWrite;
              try
                AProducer.Items.Add(AItemInfo);
                AProducer.Sort;
                Selected.AddChild.Initialize(AItemInfo);
              finally
                AProducer.UnlockWrite;
              end;
            end
            else
              UpdateContent;
          finally
            EndUpdate;
          end;
        end;
    end;
  finally
    DoShellChangeNotify(AEventID, APIDL1, APIDL2);
  end;
end;

procedure TdxShellBreadcrumbEditController.BeginUpdateContent
  (var AState: TState);
begin
  Inc(FUpdateContentLockCount);
  AState.DropDownMenuState := IsNodeDropDownMenuWindowActive;
  AState.DropDownMenuOwnerPidl := nil;
  if DropDownMenuOwner <> nil then
    AState.DropDownMenuOwnerPidl :=
      GetPidlCopy((DropDownMenuOwner.Node as TdxShellBreadcrumbEditNode)
      .AbsolutePidl);
  AState.SelectedPidl := GetPidlCopy(SelectedPidl);
  BeginUpdate;
end;

procedure TdxShellBreadcrumbEditController.EndUpdateContent(var AState: TState);
var
  ANodeViewItem: TdxBreadcrumbEditNodeViewItem;
begin
  RestoreSelection(AState.SelectedPidl);
  EndUpdate;

  if AState.DropDownMenuState then
  begin
    if ViewInfo.NodesAreaViewInfo.FindViewItem
      (FindNodeByPidl(AState.DropDownMenuOwnerPidl), ANodeViewItem) then
      ShowNodeDropDownMenu(ANodeViewItem);
  end;

  DisposePidl(AState.DropDownMenuOwnerPidl);
  DisposePidl(AState.SelectedPidl);
  Dec(FUpdateContentLockCount);
end;

function TdxShellBreadcrumbEditController.IsUpdatingContent: Boolean;
begin
  Result := FUpdateContentLockCount > 0;
end;

procedure TdxShellBreadcrumbEditController.UpdateContent;
var
  AState: TState;
begin
  BeginUpdateContent(AState);
  try
    Root.Initialize(nil);
  finally
    EndUpdateContent(AState);
  end;
end;

procedure TdxShellBreadcrumbEditController.UpdateTrackingSettings;
begin
  if ShellOptions.TrackShellChanges then
    cxShellRegisterChangeNotifier(SelectedPidl, Handle,
      DSM_SYSTEMSHELLCHANGENOTIFY, False, FShellChangeNotifierData)
  else
    RemoveChangeNotification;
end;

procedure TdxShellBreadcrumbEditController.WndProc(var Message: TMessage);
var
  AEventID: Integer;
  APIDL1, APIDL2: PItemIDList;
begin
  if Message.Msg = DSM_SYSTEMSHELLCHANGENOTIFY then
  begin
    cxShellGetNotifyParams(Message, AEventID, APIDL1, APIDL2);
    try
      ShellChangeNotify(AEventID, APIDL1, APIDL2);
    finally
      DisposePidl(APIDL1);
      DisposePidl(APIDL2);
    end;
  end;
  inherited WndProc(Message);
end;

function TdxShellBreadcrumbEditController.FindRootNodeForPath(var APath: string;
  out ANode: TdxBreadcrumbEditNode): Boolean;
var
  ASavedPath: string;
  ARootRealPath: string;
begin
  ASavedPath := APath;
  Result := inherited FindRootNodeForPath(APath, ANode);
  if not Result then
  begin
    ARootRealPath := dxIncludeTrailingPathDelimiter(Root.RealPath,
      PathDelimiter);
    Result := Pos(LowerCase(ARootRealPath),
      LowerCase(dxIncludeTrailingPathDelimiter(ASavedPath, PathDelimiter))) = 1;
    if Result then
    begin
      APath := Copy(ASavedPath, Length(ARootRealPath) + 1, MaxInt);
      ANode := Root;
    end;
  end;
end;

function TdxShellBreadcrumbEditController.GetPidlByPath(APath: string;
  out APidl: PItemIDList): Boolean;

  function DecodePath(const AOriginalPath: string): string;
  begin
    Result := dxIncludeTrailingPathDelimiter
      (dxReplacePathDelimiter(AOriginalPath, PathDelimiter, PathDelim));
  end;

const
  ViewOptions = [svoShowFolders, svoShowHidden];
var
  ANode: TdxBreadcrumbEditNode;
  ANodeName: string;
  ASavedNode: TdxShellBreadcrumbEditNode;
  ASavedPath: string;
  ATempPidl: PItemIDList;
begin
  APidl := PathToAbsolutePIDL(DecodePath(APath), ShellOptions.Root,
    ViewOptions);
  if APidl = nil then
  begin
    if FindRootNodeForPath(APath, ANode) then
    begin
      repeat
        ASavedPath := APath;
        ASavedNode := ANode as TdxShellBreadcrumbEditNode;
        if not ParsePath(ANodeName, APath) then
          Break;
        if not ANode.FindNode(ANodeName, ANode) then
        begin
          ATempPidl := InternalParseDisplayName(ASavedNode.ShellFolder,
            DecodePath(ASavedPath), ViewOptions);
          if ATempPidl <> nil then
            try
              APidl := ConcatenatePidls(ASavedNode.AbsolutePidl, ATempPidl);
            finally
              DisposePidl(ATempPidl);
            end;
          Break;
        end;
      until False;
    end;
  end;
  Result := APidl <> nil;
end;

procedure TdxShellBreadcrumbEditController.RemoveChangeNotification;
begin
  cxShellUnregisterChangeNotifier(FShellChangeNotifierData);
end;

procedure TdxShellBreadcrumbEditController.RestoreSelection(APidl: PItemIDList);
var
  ANode: TdxShellBreadcrumbEditNode;
  AParentPidl: PItemIDList;
  ASelectedPidl: PItemIDList;
begin
  ASelectedPidl := APidl;
  while APidl <> nil do
  begin
    ANode := FindNodeByPidl(APidl);
    if ANode <> nil then
    begin
      if SelectPath(ANode) then
      begin
        if APidl = ASelectedPidl then
          CancelSelectionChanges;
      end;
      Break;
    end;
    if GetPidlItemsCount(APidl) = 0 then
      Break;
    AParentPidl := GetPidlParent(APidl);
    DisposePidl(APidl);
    APidl := AParentPidl;
  end;
end;

function TdxShellBreadcrumbEditController.GetPathDelimiter: Char;
begin
  Result := PathEditingController.Properties.PathDelimiter;
end;

function TdxShellBreadcrumbEditController.GetRoot: TdxShellBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode(inherited Root);
end;

function TdxShellBreadcrumbEditController.GetSelected
  : TdxShellBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode(inherited Selected);
end;

function TdxShellBreadcrumbEditController.GetSelectedPidl: PItemIDList;
begin
  if Selected <> nil then
    Result := TdxShellBreadcrumbEditNode(Selected).AbsolutePidl
  else
    Result := nil;
end;

function TdxShellBreadcrumbEditController.GetSelectedRealPath: string;
begin
  if Selected <> nil then
    Result := Selected.RealPath
  else
    Result := '';
end;

function TdxShellBreadcrumbEditController.GetShellOptions
  : TdxShellBreadcrumbEditShellOptions;
begin
  Result := (Control.GetProperties as TdxShellBreadcrumbEditProperties)
    .ShellOptions;
end;

procedure TdxShellBreadcrumbEditController.SetSelected
  (AValue: TdxShellBreadcrumbEditNode);
begin
  inherited Selected := AValue;
end;

procedure TdxShellBreadcrumbEditController.SetSelectedPidl(AValue: PItemIDList);
begin
  Selected := TdxShellBreadcrumbEditNode(FindNodeByPidl(AValue));
end;

{ TdxShellBreadcrumbPathEditingController }

procedure TdxShellBreadcrumbPathEditingController.PopulateSuggestions
  (const APath: string; ASuggestions: TStringList);
var
  AState: TdxShellBreadcrumbEditController.TState;
begin
  Controller.BeginUpdateContent(AState);
  try
    inherited;
  finally
    Controller.EndUpdateContent(AState);
  end;
end;

procedure TdxShellBreadcrumbPathEditingController.ReleasePathEditor;
begin
  if not Controller.IsUpdatingContent then
    inherited;
end;

function TdxShellBreadcrumbPathEditingController.GetController
  : TdxShellBreadcrumbEditController;
begin
  Result := TdxShellBreadcrumbEditController(inherited Controller);
end;

{ TdxCustomShellBreadcrumbEdit }

constructor TdxCustomShellBreadcrumbEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependedControls := TcxShellDependedControls.Create;
  Properties.OnShellOptionsChanged := ShellOptionsChangeHandler;
  UpdateContent;
end;

destructor TdxCustomShellBreadcrumbEdit.Destroy;
begin
  Controller.RemoveChangeNotification;
  ShellComboBox := nil;
  ShellListView := nil;
  ShellTreeView := nil;
  FreeAndNil(FDependedControls);
  inherited Destroy;
end;

function TdxCustomShellBreadcrumbEdit.CreateProperties
  : TdxCustomBreadcrumbEditProperties;
begin
  Result := TdxShellBreadcrumbEditProperties.Create(Self);
end;

function TdxCustomShellBreadcrumbEdit.CreateController
  : TdxBreadcrumbEditController;
begin
  Result := TdxShellBreadcrumbEditController.Create(ViewInfo);
end;

function TdxCustomShellBreadcrumbEdit.CreateRoot: TdxBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode.Create(Self);
end;

procedure TdxCustomShellBreadcrumbEdit.DestroyWnd;
begin
  Controller.RemoveChangeNotification;
  inherited DestroyWnd;
end;

procedure TdxCustomShellBreadcrumbEdit.DSMNavigate(var Message: TMessage);
begin
  if FNavigationLockCount = 0 then
  begin
    Inc(FNavigationLockCount);
    try
      Selected := Controller.FindNodeByPidl(PItemIDList(Message.WParam), True);
    finally
      Dec(FNavigationLockCount);
    end;
  end;
end;

procedure TdxCustomShellBreadcrumbEdit.DSMNotifyUpdateContents
  (var Message: TMessage);
begin
  if not(csLoading in ComponentState) then
    UpdateContent;
end;

procedure TdxCustomShellBreadcrumbEdit.DSMSynchronizeRoot
  (var Message: TMessage);
begin
  if not IsParentLoading then
    GetShellRoot.Update(TcxCustomShellRoot(Message.WParam));
end;

function TdxCustomShellBreadcrumbEdit.IsParentLoading: Boolean;
begin
  Result := (Parent <> nil) and (csLoading in Parent.ComponentState);
end;

procedure TdxCustomShellBreadcrumbEdit.LoadChildren(ASender: TdxTreeCustomNode);
var
  AState: TdxShellBreadcrumbEditController.TState;
begin
  if ASender.Count > 0 then
  begin
    Controller.BeginUpdateContent(AState);
    try
      TdxShellBreadcrumbEditNode(ASender).Producer.ProcessItems;
    finally
      Controller.EndUpdateContent(AState);
    end;
  end
  else
    TdxShellBreadcrumbEditNode(ASender).Producer.ProcessItems;
end;

procedure TdxCustomShellBreadcrumbEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ShellComboBox then
      ShellComboBox := nil;
    if AComponent = ShellListView then
      ShellListView := nil;
    if AComponent = ShellTreeView then
      ShellTreeView := nil;
  end;
end;

procedure TdxCustomShellBreadcrumbEdit.RootChanged;
begin
  if Assigned(OnRootChanged) then
    OnRootChanged(Self, GetShellRoot);
end;

procedure TdxCustomShellBreadcrumbEdit.SelectionChanged;
begin
  SynchronizeDependedControls;
  inherited SelectionChanged;
end;

procedure TdxCustomShellBreadcrumbEdit.AddFolder(AFolder: TcxShellFolder;
  var ACanAdd: Boolean);
begin
  if Assigned(OnAddFolder) then
    OnAddFolder(Self, AFolder, ACanAdd);
end;

procedure TdxCustomShellBreadcrumbEdit.ShellChanged(AEventID: DWORD;
  APIDL1, APIDL2: PItemIDList);
begin
  if Assigned(OnShellChange) then
    OnShellChange(Self, AEventID, APIDL1, APIDL2);
end;

procedure TdxCustomShellBreadcrumbEdit.ShellOptionsChangeHandler
  (Sender: TObject; const AChanges: TdxShellBreadcrumbEditShellOptionsChanges);
begin
  ShellOptionsChanged(AChanges);
end;

procedure TdxCustomShellBreadcrumbEdit.ShellOptionsChanged(const AChanges
  : TdxShellBreadcrumbEditShellOptionsChanges);
begin
  if [bcescRoot, bcescContent] * AChanges = [bcescRoot, bcescContent] then
    Root.Initialize(nil)
  else if bcescContent in AChanges then
    UpdateContent;

  if bcescRoot in AChanges then
  begin
    RootChanged;
    SynchronizeRoot;
  end;

  if bcescTracking in AChanges then
    Controller.UpdateTrackingSettings;
end;

procedure TdxCustomShellBreadcrumbEdit.SynchronizeDependedControls;
var
  APidl: PItemIDList;
begin
  if (Selected <> nil) and (FNavigationLockCount = 0) then
  begin
    Inc(FNavigationLockCount);
    try
      APidl := GetPidlCopy(SelectedPidl);
      try
        DependedControls.Navigate(APidl);
      finally
        DisposePidl(APidl);
      end;
    finally
      Dec(FNavigationLockCount);
    end;
  end;
end;

procedure TdxCustomShellBreadcrumbEdit.SynchronizeRoot;
begin
  if not IsParentLoading then
    DependedControls.SynchronizeRoot(GetShellRoot);
end;

procedure TdxCustomShellBreadcrumbEdit.UpdateContent;
begin
  Controller.UpdateContent;
end;

function TdxCustomShellBreadcrumbEdit.GetDependedControls
  : TcxShellDependedControls;
begin
  Result := FDependedControls;
end;

function TdxCustomShellBreadcrumbEdit.GetController
  : TdxShellBreadcrumbEditController;
begin
  Result := TdxShellBreadcrumbEditController(inherited Controller);
end;

function TdxCustomShellBreadcrumbEdit.GetProperties
  : TdxShellBreadcrumbEditProperties;
begin
  Result := TdxShellBreadcrumbEditProperties(inherited Properties);
end;

function TdxCustomShellBreadcrumbEdit.GetShellRoot: TcxCustomShellRoot;
begin
  Result := Properties.ShellOptions.Root;
end;

function TdxCustomShellBreadcrumbEdit.GetRoot: TdxShellBreadcrumbEditNode;
begin
  Result := TdxShellBreadcrumbEditNode(inherited Root);
end;

function TdxCustomShellBreadcrumbEdit.GetSelectedPidl: PItemIDList;
begin
  Result := Controller.SelectedPidl;
end;

function TdxCustomShellBreadcrumbEdit.GetSelectedPath: string;
begin
  Result := Controller.SelectedRealPath;
end;

procedure TdxCustomShellBreadcrumbEdit.SetProperties
  (AValue: TdxShellBreadcrumbEditProperties);
begin
  inherited Properties := AValue;
end;

procedure TdxCustomShellBreadcrumbEdit.SetSelectedPidl(AValue: PItemIDList);
begin
  Controller.SelectedPidl := AValue;
end;

procedure TdxCustomShellBreadcrumbEdit.SetShellComboBox
  (AValue: TcxCustomShellComboBox);
begin
  cxSetShellControl(Self, AValue, TWinControl(FShellComboBox));
end;

procedure TdxCustomShellBreadcrumbEdit.SetShellListView
  (AValue: TcxCustomShellListView);
begin
  cxSetShellControl(Self, AValue, TWinControl(FShellListView));
end;

procedure TdxCustomShellBreadcrumbEdit.SetShellTreeView
  (AValue: TcxCustomShellTreeView);
begin
  cxSetShellControl(Self, AValue, TWinControl(FShellTreeView));
end;

end.
