{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE }
{ PROGRAM ONLY. }
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

unit dxShellCustomDialog; // for internal use only

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ShlObj, StdCtrls,
  ShellAPI, ActiveX, ComObj, ImgList, Math, Types, SysUtils,
  Generics.Collections, Generics.Defaults,
  cxGraphics, cxControls, cxLookAndFeels, cxContainer, cxLookAndFeelPainters,
  cxClasses, dxCoreGraphics, dxForms,
  dxLayoutControl, dxLayoutContainer, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutLookAndFeels,
  cxEdit, cxTextEdit, dxBreadcrumbEdit, cxShellCommon, dxShellBreadcrumbEdit,
  cxButtons, cxMaskEdit, cxButtonEdit,
  cxDropDownEdit, cxImageList, dxShellControls, dxListView, ActnList,
  dxGenerics, dxCustomTree, dxTreeView,
  PropSys;

type
  { TdxfrmCommonFileCustomDialog }

  TdxfrmCommonFileCustomDialog = class(TdxForm)
  strict private
    FDefaultExt: string;
    FFiles: TStringList;
    FLookAndFeel: TcxLookAndFeel;
    FOptions: TOpenOptions;
    FOnFileOkClick: TCloseQueryEvent;
    FOnFolderChange: TNotifyEvent;
    FOnIncludeItem: TIncludeItemEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnTypeChange: TNotifyEvent;
    procedure UpdateSystemMenuItems(AMenu: HMENU);
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDblClk(var Message: TWMNCLButtonDblClk);
      message WM_NCLBUTTONDBLCLK;
  protected
    FLoadedInitialDirPidl: PItemIDList;
    FInitializeDir: string;
    function GetFileName: string; virtual; abstract;
    function GetFilterIndex: Integer; virtual; abstract;
    function GetTitle: string; virtual; abstract;
    function GetInitialDir: string; virtual; abstract;
    function GetViewStyle: TdxListViewStyle; virtual; abstract;
    procedure SetFileName(const AValue: string); virtual; abstract;
    procedure SetOptions(const AValue: TOpenOptions); virtual;
    procedure SetTitle(const AValue: string); virtual; abstract;
    procedure SetViewStyle(const AValue: TdxListViewStyle); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyLocalization; virtual; abstract;
    procedure InitializeFilter(const AFilter: string;
      const AFilterIndex: Integer); virtual; abstract;
    procedure InitializeFolder(const AInitialDir: string); virtual; abstract;
    procedure SetHistoryList(AHistoryList: TStrings); virtual; abstract;
    procedure WndProc(var Message: TMessage); override;

    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property FileName: string read GetFileName write SetFileName;
    property Files: TStringList read FFiles;
    property FilterIndex: Integer read GetFilterIndex;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
    property Options: TOpenOptions read FOptions write SetOptions;
    property Title: string read GetTitle write SetTitle;
    property OnFileOkClick: TCloseQueryEvent read FOnFileOkClick
      write FOnFileOkClick;
    property OnFolderChange: TNotifyEvent read FOnFolderChange
      write FOnFolderChange;
    property OnIncludeItem: TIncludeItemEvent read FOnIncludeItem
      write FOnIncludeItem;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange
      write FOnSelectionChange;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
  end;

  TdxfrmCommonFileCustomDialogClass = class of TdxfrmCommonFileCustomDialog;

  { TdxfrmCommonFileDialog }

  TdxfrmCommonFileDialog = class(TdxfrmCommonFileCustomDialog)
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    btnBack: TcxButton;
    libtnBack: TdxLayoutItem;
    btnForward: TcxButton;
    libtnForward: TdxLayoutItem;
    btnUp: TcxButton;
    libtnUp: TdxLayoutItem;
    sbePath: TdxShellBreadcrumbEdit;
    lisbePath: TdxLayoutItem;
    lgNavigationAndSearch: TdxLayoutGroup;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    lgFolders: TdxLayoutGroup;
    lgControls: TdxLayoutGroup;
    beSearch: TcxButtonEdit;
    libeSearch: TdxLayoutItem;
    btnOK: TcxButton;
    libtnOK: TdxLayoutItem;
    btnCancel: TcxButton;
    libtnCancel: TdxLayoutItem;
    cbName: TcxComboBox;
    licbName: TdxLayoutItem;
    cbFilter: TcxComboBox;
    licbFilter: TdxLayoutItem;
    lgButtons: TdxLayoutGroup;
    lgFilterAndButtons: TdxLayoutGroup;
    btnHistory: TcxButton;
    libtnHistory: TdxLayoutItem;
    liShellListView: TdxLayoutItem;
    liShellTreeView: TdxLayoutItem;
    dxLayoutSplitterItem: TdxLayoutSplitterItem;
    dxLayoutCxLookAndFeel_NoItemOffset: TdxLayoutCxLookAndFeel;
    lgNewFolderAndViews: TdxLayoutGroup;
    btnNewFolder: TcxButton;
    btnViews: TcxButton;
    libtnNewFolder: TdxLayoutItem;
    libtnViews: TdxLayoutItem;
    ilViews: TcxImageList;
    pmHistory: TPopupMenu;
    pmViews: TPopupMenu;
    pmiExtraLargeIcons: TMenuItem;
    pmiLargeIcons: TMenuItem;
    pmiMediumIcons: TMenuItem;
    pmiSmallIcons: TMenuItem;
    pmSeparator1: TMenuItem;
    pmiList: TMenuItem;
    pmSeparator2: TMenuItem;
    pmiDetails: TMenuItem;
    pmSeparator3: TMenuItem;
    pmiTiles: TMenuItem;
    pmSeparator4: TMenuItem;
    pmiContent: TMenuItem;
    actViews: TActionList;
    actExtraLarge: TAction;
    actLarge: TAction;
    actMedium: TAction;
    actSmall: TAction;
    actDetails: TAction;
    actList: TAction;
    //
    procedure DoButtonCustomDraw(Sender: TObject; ACanvas: TcxCanvas;
      AViewInfo: TcxButtonViewInfo; var AHandled: Boolean);
    procedure DoChooseFile(Sender: TObject; APIDL: PItemIDList;
      var AHandled: Boolean);
    procedure DoHistoryItemClick(Sender: TObject);
    procedure DoShellListViewAfterNavigation(Sender: TdxShellListView;
      APIDL: PItemIDList; ADisplayName: WideString);
    procedure DoShellListViewFolderChanged(Sender: TObject;
      Root: TcxCustomShellRoot);
    procedure DoShellTreeViewAddFolder(Sender: TObject; AFolder: TcxShellFolder;
      var ACanAdd: Boolean);
    procedure DoShellListViewAddFoler(Sender: TObject; AFolder: TcxShellFolder;
      var ACanAdd: Boolean);
    procedure DoViewClick(Sender: TObject);
    procedure DoViewChanged(Sender: TObject);
    //
    procedure btnBackClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnNewFolderClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnViewsClick(Sender: TObject);
    procedure cbFilterPropertiesEditValueChanged(Sender: TObject);
    procedure sbePathPathSelected(Sender: TObject);
    procedure cbNamePropertiesChange(Sender: TObject);
    procedure dxLayoutSplitterItemCanResize(Sender: TObject;
      AItem: TdxCustomLayoutItem; var ANewSize: Integer; var AAccept: Boolean);
  strict private
  type
{$REGION 'Private types'}
    TdxFileInfo = class
    private
      FFileName: string;
      FIsLink: Boolean;
      FPidl: PItemIDList;
      FTargetFileName: string;
      procedure SetPidl(AValue: PItemIDList);
    public
      destructor Destroy; override;
      property FileName: string read FFileName write FFileName;
      property IsLink: Boolean read FIsLink write FIsLink;
      property Pidl: PItemIDList read FPidl write SetPidl;
      property TargetFileName: string read FTargetFileName
        write FTargetFileName;
    end;

    TdxShellDialogTreeView = class(TdxShellTreeView)
    strict private
      FOnExpandStateChanged: TdxTreeViewNodeEvent;
      FOnPathChanged: TNotifyEvent;
      procedure DoPathChanged;
    protected
      procedure Collapsed(Sender: TdxTreeCustomNode); override;
      procedure DoFocusNodeByMouse(ANode: TdxTreeViewNode); override;
      procedure DoNodeExpandStateChanged(ANode: TdxTreeViewNode); override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      property OnExpandStateChanged: TdxTreeViewNodeEvent
        read FOnExpandStateChanged write FOnExpandStateChanged;
      property OnPathChanged: TNotifyEvent read FOnPathChanged
        write FOnPathChanged;
    end;
{$ENDREGION}
  strict private
    FCanSyncPaths: Boolean;
    FFileTypes: TFileTypeItems;
    FFileInfos: TObjectList<TdxFileInfo>;
    FFileName: string;
    FFilterIndex: Integer;
    FHideFileExt: Boolean;
    FHistoryUpdateCount: Integer;
    FIsSelectionChanging: Boolean;
    FMaxHistoryItemsCount: Integer;
    FSavedFileName: string;
    FShellListView: TdxShellListView;
    FShellTreeView: TdxShellDialogTreeView;
    FShowAllFolders: Boolean;
    function IsSelectableItem(AIndex: Integer): Boolean;
    procedure CheckHistoryItem(AMenuItem: TMenuItem);
    procedure DoShellTreeViewPathChanged(Sender: TObject);
    procedure DoShellTreeViewNodeExpandStateChanged(Sender: TdxCustomTreeView;
      Node: TdxTreeViewNode);
    function GetCheckedHistoryItemIndex: Integer;
    function GetCurrentPidl: PItemIDList;
    function GetViewStatePropertyBag(APIDL: PItemIDList;
      out APropertyBag: IPropertyBag): Boolean;
    function GetShellListViewSelectedItemIndices: TdxIntegerList;
    function ReadExpandedState(APersistStream: IPersistStream): Boolean;
    procedure RestoreExpandedState;
    procedure RestoreInitialDir;
    procedure SetFileTypes(const AValue: TFileTypeItems);
    procedure StoreExpandedState;
    procedure StoreInitialDir;
    procedure SyncTreeView(APIDL: PItemIDList);
    procedure SyncWithTreeView;
    procedure ViewChanged;
    procedure WriteExpandedState(APersistStream: IPersistStream);
    //
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure WMAppCommand(var Msg: TMessage); message WM_APPCOMMAND;
  protected
    procedure ApplyFilter(const AIndex: Integer);
    procedure BeginUpdateHistory;
    function CanUpdateHistory: Boolean;
    procedure ChangeView(AViewId: Integer);
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShellListViewSelectionChanged(Sender: TObject);
    procedure DoShow; override;
    procedure EndUpdateHistory;
    function FindMask(const AValue: string; out AIndex: Integer): Boolean;
    function GetInitialDir: string; override;
    function GetFileName: string; override;
    function GetFilterIndex: Integer; override;
    function GetTitle: string; override;
    function GetViewStyle: TdxListViewStyle; override;
    procedure InitializeFileTypes(const AFilter: string);
    procedure InitializeLookAndFeel;
    function IsMask(const AFileName: string; var AFileMask: string): Boolean;
    function IsOptionsCheckPassed(const AFileName: string): Boolean; virtual;
    procedure RestoreListViewState;
    procedure SetActiveMask(AValue: string);
    procedure SetFileName(const AValue: string); override;
    procedure SetOptions(const AValue: TOpenOptions); override;
    procedure SetTitle(const AValue: string); override;
    procedure SetViewStyle(const AValue: TdxListViewStyle); override;
    procedure StoreListViewState;
    procedure SyncPaths(APIDL: PItemIDList; ASyncControl: TWinControl = nil);
    procedure UpdateButtonsSize;
    procedure UpdateButtonsStates;
    procedure UpdateHistory(APIDL: PItemIDList);
    procedure UpdateViews;
    //
    property FileTypes: TFileTypeItems read FFileTypes write SetFileTypes;
    property ShellListViewSelectedItemIndices: TdxIntegerList
      read GetShellListViewSelectedItemIndices;
    property ShellListView: TdxShellListView read FShellListView;
    property ShellTreeView: TdxShellDialogTreeView read FShellTreeView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyLocalization; override;
    procedure InitializeFilter(const AFilter: string;
      const AFilterIndex: Integer); override;
    procedure InitializeFolder(const AInitialDir: string); override;
    procedure SetHistoryList(AHistoryList: TStrings); override;
    function ShowModal: Integer; override;
  end;

  { TdxfrmOpenFileDialog }

  TdxfrmOpenFileDialog = class(TdxfrmCommonFileDialog)
  protected
    function IsOptionsCheckPassed(const AFileName: string): Boolean; override;
    procedure SetOptions(const AValue: TOpenOptions); override;
  public
    procedure ApplyLocalization; override;
  end;

  { TdxfrmSaveFileDialog }

  TdxfrmSaveFileDialog = class(TdxfrmCommonFileDialog)
  protected
    function IsOptionsCheckPassed(const AFileName: string): Boolean; override;
  public
    procedure ApplyLocalization; override;
  end;

  { TdxFileDialogsSettings }

  TdxFileDialogsSettings = class
  protected type
    TdxFileDialogInfo = class
    public
      Changed: Boolean;
      Position: TPoint;
      SplitterPosition: Integer;
      Size: TSize;
    end;

  const
    DefaultSize: TSize = (cx: 999; cy: 585);
    DefaultSplitterPosition: Integer = 276;
    //
    SettingsRootKeyName: string = 'Software\Developer Express\Dialogs\';
    //
    HeightPropertyName: string = 'FormHeight';
    LastDirPropertyName: string = 'LastVisitedDialogDirectoryProperty';
    SplitterPositionPropertyName: string = 'SplitterPosition';
    WidthPropertyName: string = 'FormWidth';

  var
    Changed: Boolean;
    FInitialDir: string;
    FSettings: TObjectDictionary<TClass, TdxFileDialogInfo>;
    procedure DoLoad; virtual;
    procedure DoSave; virtual;
    function GetInfo(AClass: TClass): TdxFileDialogInfo; virtual;
    function GetSplitterPosition(AClass: TClass): Integer;
    function GetSize(AClass: TClass): TSize;
    procedure LoadFormInfo(AInstance: TdxfrmCommonFileCustomDialog); virtual;
    procedure SaveFormInfo(AInstance: TdxfrmCommonFileCustomDialog); virtual;
    procedure SetInitialDir(const AValue: string);
    procedure SetSplitterPosition(AClass: TClass; AValue: Integer);
    procedure SetSize(AClass: TClass; const AValue: TSize);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property InitialDir: string read FInitialDir write SetInitialDir;
    property SplitterPosition[AClass: TClass]: Integer read GetSplitterPosition
      write SetSplitterPosition;
    property Size[AClass: TClass]: TSize read GetSize write SetSize;
  end;

var
  dxFileDialogsSettings: TdxFileDialogsSettings;

implementation

{$R *.dfm}

uses
  StrUtils, Registry, dxThreading, dxCore, cxGeometry, cxEditConsts, dxHooks,
  cxShellControls,
  dxDPIAwareUtils, dxTypeHelpers, dxLayoutCommon, cxShellListView;

const
  SID_IFolderType = '{053B4A86-0DC9-40A3-B7ED-BC6A2E951F48}';
  SID_IMruDataList = '{FE787BCB-0EE8-44FB-8C89-12F508913C40}';
  SID_IMruDataList2 = '{D2C22919-91F5-4284-8807-58A2D64E561C}';
  SID_INewItemAdvisor = '{24D16EE5-10F5-4DE3-8766-D23779BA7A6D}';
  SID_IObjectArray = '{92CA9DCD-5622-4BBA-A805-5E9F541BD8C9}';
  SID_IObjectCollection = '{5632B1A4-E38A-400A-928A-D4CD63230295}';
  //
  CLSID_MruLongList: TGUID = '{53BD6B4E-3780-4693-AFC3-7161C2F3EE9C}';
  CLSID_ShellItemArrayAsCollection
    : TGUID = '{CDC82860-468D-4D4E-B7E7-C298FF23AB2C}';
  //
  IID_IFolderType: TGUID = SID_IFolderType;
  IID_INewItemAdvisor: TGUID = SID_INewItemAdvisor;
  IID_IObjectCollection: TGUID = SID_IObjectCollection;
  IID_IPropertyBag: TGUID = '{55272A00-42CB-11CE-8135-00AA004BB851}';
  PKEY_AddToTreeView: TGUID = '{5D76B67F-9B3D-44BB-B6AE-25DA4F638A67}';
  PKEY_ExpandState: TGUID = '{77C77E35-1BE3-4350-A48C-7563D727776D}';
  SExplorerRegPath = 'Software\Microsoft\Windows\CurrentVersion\Explorer\';
  SExplorerAdvanced = SExplorerRegPath + 'Advanced';
  SExplorerModules = SExplorerRegPath + 'Modules';
  SExplorerLastVisitedPidlMRU = SExplorerRegPath +
    'ComDlg32\LastVisitedPidlMRU';
  SNavPane = 'NavPane';
  SExpandedState = 'ExpandedState';
  SShowAllFolders = 'NavPaneShowAllFolders';
  SHideFileExt = 'HideFileExt';
  // viewmode
  SComdlg = 'Comdlg\';
  ID_GenericFolderTypeId: TGUID = '{5C4F28B5-F869-4E84-8E60-F11DB97C5CC7}';
  ID_IPersistStream: TGUID = '{00000109-0000-0000-C000-000000000046}';
  SLogicalViewMode = 'LogicalViewMode';
  SMode = 'Mode';
  SIconSize = 'IconSize';
  // ShellListView mode
  cmdExtraLargeIconId = 1;
  cmdLargeIconId = 2;
  cmdIconId = 3;
  cmdSmallIconId = 4;
  cmdListId = 5;
  cmdDetailId = 6;
  DefaultShellListViewMode = cmdIconId;

type
  TcxControlAccess = class(TcxControl);
  TcxCustomButtonAccess = class(TcxCustomButton);
  TcxButtonViewInfoAccess = class(TcxButtonViewInfo);
  TcxButtonPainterAccess = class(TcxButtonPainter);
  TdxShellListViewAccess = class(TdxShellListView);
  TcxButtonStandardLayoutCalculatorAccess = class
    (TcxButtonStandardLayoutCalculator);
  TdxListViewControllerAccess = class(TdxListViewController);
  TdxShellListViewItemProducerAccess = class(TdxShellListViewItemProducer);

  IFolderType = interface
    [SID_IFolderType]
    function GetFolderType(var AFolderTypeId: TGUID): HRESULT; stdcall;
  end;

  INewItemAdvisor = interface
    [SID_INewItemAdvisor]
    function IsTypeSupported(const AType: LPCWSTR): HRESULT; stdcall;
    function GetPropertiesToApply(APropertyStore: IPropertyStore;
      const iid: TIID; out pv): HRESULT; stdcall;
    function QueryObject(const iid1: TIID; const iid2: TIID; out pv)
      : HRESULT; stdcall;
  end;

  IMruDataList = interface
    [SID_IMruDataList]
    function InitData(uMax: UINT; flags: Integer; hKey: hKey;
      pszSubKey: LPCWSTR; pfnCompare: Pointer): HRESULT; stdcall;
    function AddData(const pData: Pointer; cbData: DWORD; var pdwSlot: DWORD)
      : HRESULT; stdcall;
    function FindData(const pData: Pointer; cbData: DWORD; var piIndex: Integer)
      : HRESULT; stdcall;
    function GetData(iIndex: Integer; pData: Pointer; cbData: DWORD)
      : HRESULT; stdcall;
    function QueryInfo(iIndex: Integer; pdwSlot: DWORD; var cbData: DWORD)
      : HRESULT; stdcall;
    function Delete(iIndex: Integer): HRESULT; stdcall;
  end;

  IMruDataList2 = interface
    [SID_IMruDataList2]
    function InitData(uMax: UINT; flags: Integer; hKey: hKey;
      pszSubKey: LPCWSTR; pfnCompare: Pointer): HRESULT; stdcall;
    function AddData(const pData: Pointer; cbData: DWORD; var pdwSlot: DWORD)
      : HRESULT; stdcall;
    function InsertData(AData: PChar; A1: Integer; A2: Cardinal;
      var A3: Cardinal): HRESULT; stdcall;
    function FindData(const pData: Pointer; cbData: DWORD; var piIndex: Integer)
      : HRESULT; stdcall;
    function GetData(iIndex: Integer; pData: Pointer; cbData: DWORD)
      : HRESULT; stdcall;
    function QueryInfo(iIndex: Integer; pdwSlot: DWORD; var cbData: DWORD)
      : HRESULT; stdcall;
    function Delete(iIndex: Integer): HRESULT; stdcall;
  end;

  IObjectArray = interface
    [SID_IObjectArray]
    function GetCount(out pcObjects: UINT): HRESULT; stdcall;
    function GetAt(uiIndex: UINT; riid: TIID; out ppv): HRESULT; stdcall;
  end;

  IObjectCollection = interface(IObjectArray)
    [SID_IObjectCollection]
    function AddObject(const punk: IUnknown): HRESULT; stdcall;
    function AddFromArray(const poaSource: IObjectArray): HRESULT; stdcall;
    function RemoveObjectAt(uiIndex: UINT): HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
  end;

  TdxMRULongList = class
  strict private
    FIsInitialized: Boolean;
    FProcessName: string;
    FProcessNameLength: Integer;
    FMRUDataList: IMruDataList;
    FMRUDataList2: IMruDataList2;
    procedure InitData;
    function AddData(AData: Pointer; ADataSize: Integer): Boolean;
    function FindData(out AIndex: Integer): Boolean;
    function GetData(AIndex: Integer; AData: Pointer;
      ABytesCount: Cardinal): Boolean;
    function QueryInfo(AIndex: Integer; out ABytesCount: Cardinal): Boolean;
    function Delete(AIndex: Integer): Boolean;
    function UseNewInterface: Boolean;
  protected
    procedure ClearData;
  public
    constructor Create;
    destructor Destroy; override;
    function ReadInitialDirPidl(out APIDL: PItemIDList): Boolean;
    function WriteInitialDirPidl(APIDL: PItemIDList): Boolean;
  end;

  { TdxMRULongList }

constructor TdxMRULongList.Create;
begin
  inherited Create;
  InitData;
end;

destructor TdxMRULongList.Destroy;
begin
  FMRUDataList := nil;
  FMRUDataList2 := nil;
  inherited;
end;

function TdxMRULongList.ReadInitialDirPidl(out APIDL: PItemIDList): Boolean;
var
  AIndex: Integer;
  ABytesCount: Cardinal;
  AData: Pointer;
begin
  Result := False;
  APIDL := nil;
  if FIsInitialized and FindData(AIndex) and QueryInfo(AIndex, ABytesCount) then
  begin
    AData := cxAllocMem(ABytesCount);
    if GetData(AIndex, AData, ABytesCount) then
    begin
      APIDL := ShiftPointer(AData, FProcessNameLength);
      APIDL := GetPidlCopy(APIDL);
      Result := APIDL <> nil;
    end;
    cxFreeMem(AData);
  end;
end;

function TdxMRULongList.WriteInitialDirPidl(APIDL: PItemIDList): Boolean;
var
  AData: Pointer;
  APidlSize: Integer;
begin
  Result := False;
  if FIsInitialized then
  begin
    ClearData;
    APidlSize := GetPidlSize(APIDL) + 2;
    AData := cxAllocMem(FProcessNameLength + APidlSize);
    try
      cxCopyData(PChar(FProcessName), AData, FProcessNameLength);
      cxCopyData(APIDL, AData, 0, FProcessNameLength, APidlSize);
      Result := AddData(AData, FProcessNameLength + APidlSize);
    finally
      cxFreeMem(AData);
    end;
  end;
end;

procedure TdxMRULongList.ClearData;
var
  AIndex: Integer;
begin
  if FIsInitialized and FindData(AIndex) then
    Delete(AIndex);
end;

procedure TdxMRULongList.InitData;
begin
  if UseNewInterface then
    FIsInitialized := Succeeded(CoCreateInstance(CLSID_MruLongList, nil,
      CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IMruDataList2, FMRUDataList2)
      ) and Succeeded(FMRUDataList2.InitData($19, 1, HKEY_CURRENT_USER,
      PChar(SExplorerLastVisitedPidlMRU), nil))
  else
    FIsInitialized := Succeeded(CoCreateInstance(CLSID_MruLongList, nil,
      CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IMruDataList, FMRUDataList))
      and Succeeded(FMRUDataList.InitData($19, 1, HKEY_CURRENT_USER,
      PChar(SExplorerLastVisitedPidlMRU), nil));
  if FIsInitialized then
  begin
    FProcessName := ExtractFileName(Application.ExeName);
    FProcessNameLength := Length(FProcessName) * 2 + 2;
  end;
end;

function TdxMRULongList.AddData(AData: Pointer; ADataSize: Integer): Boolean;
var
  ASlot: Cardinal;
begin
  if UseNewInterface then
    Result := Succeeded(FMRUDataList2.AddData(AData, ADataSize, ASlot))
  else
    Result := Succeeded(FMRUDataList.AddData(AData, ADataSize, ASlot));
end;

function TdxMRULongList.FindData(out AIndex: Integer): Boolean;
begin
  if UseNewInterface then
    Result := Succeeded(FMRUDataList2.FindData(PChar(FProcessName),
      FProcessNameLength, AIndex))
  else
    Result := Succeeded(FMRUDataList.FindData(PChar(FProcessName),
      FProcessNameLength, AIndex));
end;

function TdxMRULongList.GetData(AIndex: Integer; AData: Pointer;
  ABytesCount: Cardinal): Boolean;
begin
  if UseNewInterface then
    Result := Succeeded(FMRUDataList2.GetData(AIndex, AData, ABytesCount))
  else
    Result := Succeeded(FMRUDataList.GetData(AIndex, AData, ABytesCount));
end;

function TdxMRULongList.QueryInfo(AIndex: Integer;
  out ABytesCount: Cardinal): Boolean;
begin
  if UseNewInterface then
    Result := Succeeded(FMRUDataList2.QueryInfo(AIndex, 0, ABytesCount))
  else
    Result := Succeeded(FMRUDataList.QueryInfo(AIndex, 0, ABytesCount));
end;

function TdxMRULongList.Delete(AIndex: Integer): Boolean;
begin
  if UseNewInterface then
    Result := Succeeded(FMRUDataList2.Delete(AIndex))
  else
    Result := Succeeded(FMRUDataList.Delete(AIndex));
end;

function TdxMRULongList.UseNewInterface: Boolean;
begin
  Result := IsWin8OrLater;
end;

procedure dxMouseWheelHook(ACode: Integer; wParam: wParam; lParam: lParam;
  var AHookResult: LRESULT);
var
  AControl: TControl;
  AMHS: PMouseHookStructEx;
begin
  if (ACode < 0) or (wParam <> WM_MOUSEWHEEL) or not Mouse.WheelPresent then
    Exit;
  AMHS := PMouseHookStructEx(lParam);
  AControl := FindVCLWindow(AMHS^.MouseHookStruct.pt);
  if not((AControl is TdxShellTreeView) or (AControl is TdxShellListView)) then
    Exit;
  if TcxControlAccess(AControl).DoMouseWheel(KeyboardStateToShiftState,
    SmallInt(HiWord(AMHS.mouseData)), AMHS^.MouseHookStruct.pt) then
    AHookResult := 1;
end;

{ TdxfrmCommonFileCustomDialog }

constructor TdxfrmCommonFileCustomDialog.Create(AOwner: TComponent);
begin
  inherited Create(Application);
  Font := Screen.IconFont;
  Font.Height := MulDiv(Font.Height, ScaleFactor.TargetDPI,
    dxSystemScaleFactor.TargetDPI);
  FFiles := TStringList.Create;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  if not IsWin10 then
    dxSetHook(htMouse, dxMouseWheelHook);
end;

destructor TdxfrmCommonFileCustomDialog.Destroy;
begin
  DisposePidl(FLoadedInitialDirPidl);
  TdxUIThreadSyncService.Unsubscribe(Self);
  if not IsWin10 then
    dxReleaseHook(dxMouseWheelHook);
  FreeAndNil(FFiles);
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TdxfrmCommonFileCustomDialog.UpdateSystemMenuItems(AMenu: HMENU);
begin
  if WindowState = wsMaximized then
  begin
    EnableMenuItem(AMenu, SC_MAXIMIZE, MF_BYCOMMAND or MF_GRAYED);
    EnableMenuItem(AMenu, SC_RESTORE, MF_BYCOMMAND);
  end
  else
  begin
    EnableMenuItem(AMenu, SC_MAXIMIZE, MF_BYCOMMAND);
    EnableMenuItem(AMenu, SC_RESTORE, MF_BYCOMMAND or MF_GRAYED);
  end;
end;

procedure TdxfrmCommonFileCustomDialog.WMGetMinMaxInfo
  (var Message: TWMGetMinMaxInfo);
var
  AMinMaxInfo: PMinMaxInfo;
  AWorkarea, ABounds: TRect;
  AMonitor: TMonitor;
begin
  inherited;
  if not(csReading in ComponentState) then
  begin
    AMonitor := Screen.MonitorFromWindow(Handle);
    if AMonitor <> nil then
    begin
      AWorkarea := AMonitor.WorkareaRect;
      ABounds := AMonitor.BoundsRect;
      AMinMaxInfo := Message.MinMaxInfo;
      with AMinMaxInfo^ do
      begin
        ptMaxPosition.Init(AWorkarea.Left - ABounds.Left,
          AWorkarea.Top - ABounds.Top);
        ptMaxSize.Init(AWorkarea.Width, AWorkarea.Height);
      end;
    end;
  end;
end;

procedure TdxfrmCommonFileCustomDialog.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if IsZoomed(Handle) then
    if Message.Result in [HTBORDER, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT,
      HTLEFT, HTRIGHT, HTTOP, HTTOPLEFT, HTTOPRIGHT] then
      Message.Result := HTCLIENT;
end;

procedure TdxfrmCommonFileCustomDialog.WMNCLButtonDblClk
  (var Message: TWMNCLButtonDblClk);
var
  P: TPoint;
begin
  if not(ofEnableSizing in Options) then
    Exit;
  P := GetMouseCursorPos;
  if SendMessage(Handle, WM_NCHITTEST, 0, MakeLong(P.X, P.Y)) = HTCAPTION then
  begin
    if WindowState = wsNormal then
      TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, procedure()
        begin
          WindowState := wsMaximized;
        end)
    else
      TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, procedure()
        begin
          WindowState := wsNormal;
        end)
  end;
end;

procedure TdxfrmCommonFileCustomDialog.WndProc(var Message: TMessage);
begin
  with Message do
  begin
    case Msg of
      WM_INITMENU:
        begin
          Message.wParam := GetSystemMenu(Handle, False);
          LockWindowUpdate(Handle);
          try
            DeleteMenu(Message.wParam, SC_MINIMIZE, MF_BYCOMMAND);
            if not(ofEnableSizing in Options) then
            begin
              DeleteMenu(Message.wParam, SC_MAXIMIZE, MF_BYCOMMAND);
              DeleteMenu(Message.wParam, SC_RESTORE, MF_BYCOMMAND);
              DeleteMenu(Message.wParam, SC_SIZE, MF_BYCOMMAND);
            end;
            UpdateSystemMenuItems(Message.wParam);
            inherited WndProc(Message);
          finally
            LockWindowUpdate(0);
          end;
        end;
    else
      inherited;
    end;
  end;
end;

{
  procedure TdxfrmCommonFileCustomDialog.GetBorderStyles(var Style, ExStyle, ClassStyle: Cardinal);
  begin
  inherited GetBorderStyles(Style, ExStyle, ClassStyle);
  Style := Style or (WS_CAPTION or WS_THICKFRAME);
  if not (ofEnableSizing in Options) then
  Style := Style or not WS_THICKFRAME;
  end;

}

procedure TdxfrmCommonFileCustomDialog.SetOptions(const AValue: TOpenOptions);
begin
  FOptions := AValue;
end;

{ TdxfrmCommonFileDialog }

procedure TdxfrmCommonFileDialog.DoButtonCustomDraw(Sender: TObject;
ACanvas: TcxCanvas; AViewInfo: TcxButtonViewInfo; var AHandled: Boolean);
begin
  AHandled := AViewInfo.State in [cxbsNormal, cxbsDisabled];
  if AHandled then
  begin
    cxDrawTransparentControlBackground(Sender as TcxButton, ACanvas,
      AViewInfo.Bounds);
    TcxButtonViewInfoAccess(AViewInfo).DrawContent(ACanvas);
    if AViewInfo.HasDropDownButton and
      not cxRectIsEmpty(AViewInfo.DropDownArrowRect) and
      (AViewInfo.Painter.LookAndFeelStyle = lfsSkin) then
      AViewInfo.Painter.LookAndFeelPainter.DrawScaledDropDownButtonArrow
        (ACanvas, AViewInfo.DropDownArrowRect, AViewInfo.DropDownButtonState,
        TcxButtonPainterAccess(AViewInfo.Painter).ScaleFactor);
  end;
end;

procedure TdxfrmCommonFileDialog.DoChooseFile(Sender: TObject;
APIDL: PItemIDList; var AHandled: Boolean);
begin
  AHandled := True;
  btnOK.Click;
end;

procedure TdxfrmCommonFileDialog.DoHistoryItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
begin
  BeginUpdateHistory;
  try
    AMenuItem := Sender as TMenuItem;
    CheckHistoryItem(AMenuItem);
    SyncPaths(PItemIDList(AMenuItem.Tag));
  finally
    EndUpdateHistory;
  end;
end;

procedure TdxfrmCommonFileDialog.DoShellListViewAfterNavigation
  (Sender: TdxShellListView; APIDL: PItemIDList; ADisplayName: WideString);
var
  I: Integer;
  AShellItem: IShellItem;
  AAttr: Cardinal;
  ANewItemAdvisor: INewItemAdvisor;
begin
  SyncPaths(APIDL, FShellListView);
  RestoreListViewState;
  for I := 0 to FFileInfos.Count - 1 do
  begin
    FFileInfos[I].Pidl := nil;
    FFileInfos[I].FileName := ExtractFileName(FFileInfos[I].FileName);
    FFileInfos[I].IsLink := False;
    FFileInfos[I].TargetFileName := '';
  end;
  if IsWinSevenOrLater and Succeeded(SHCreateItemFromIDList(APIDL,
    IID_IShellItem, AShellItem)) then
  begin
    if Succeeded(AShellItem.BindToHandler(nil, BHID_SFViewObject,
      IID_INewItemAdvisor, ANewItemAdvisor)) then
      libtnNewFolder.Visible :=
        Succeeded(ANewItemAdvisor.IsTypeSupported(PChar('Folder')))
    else if Succeeded(AShellItem.GetAttributes(SFGAO_READONLY or SFGAO_STORAGE,
      AAttr)) then
      libtnNewFolder.Visible := AAttr and (SFGAO_READONLY or SFGAO_STORAGE)
        = SFGAO_STORAGE
    else
      libtnNewFolder.Visible := False;
  end
  else
    libtnNewFolder.Visible := FShellListView.Path <> '';
end;

procedure TdxfrmCommonFileDialog.DoShellListViewSelectionChanged
  (Sender: TObject);
var
  ASelectedCount, I: Integer;
  AItemIndex: Integer;
  AFolder: TcxShellFolder;
  AItemInfo: TcxShellItemInfo;
  ASelection: TdxIntegerList;
  ASelectedItems: TStringBuilder;
  AFileName, ATargetFileName: string;
  APIDL, ATargetPidl: PItemIDList;
  ALink: IShellLink;
  AIsLink: Boolean;
  AFileInfos: TList<TdxFileInfo>;
  AFileInfo: TdxFileInfo;
begin
  FIsSelectionChanging := True;
  try
    ASelectedItems := TStringBuilder.Create;
    AFileInfos := TList<TdxFileInfo>.Create;
    try
      ASelectedCount := 0;
      ASelection := ShellListViewSelectedItemIndices;
      for I := 0 to ASelection.Count - 1 do
      begin
        AItemIndex := ASelection[I];
        AFolder := FShellListView.Folders[AItemIndex];
        if IsSelectableItem(AItemIndex) then
        begin
          AFileName := AFolder.PathName;
          APIDL := AFolder.AbsolutePIDL;
          ATargetFileName := '';
          AIsLink := False;
          if not(ofNoDereferenceLinks in Options) then
          begin
            AItemInfo := TdxShellListViewAccess(FShellListView)
              .GetItemInfo(AItemIndex);
            if AItemInfo.IsZipFolderLink or AItemInfo.IsLink and not AItemInfo.IsFolderLink
            then
            begin
              if Succeeded(TdxShellListViewItemProducerAccess
                (AItemInfo.ItemProducer).ShellFolder.BindToObject
                (AItemInfo.Pidl, nil, IID_IShellLink, ALink)) and
                Succeeded(ALink.GetIDList(ATargetPidl)) then
              begin
                ATargetFileName := GetPidlName(ATargetPidl);
                AIsLink := ATargetFileName <> '';
              end;
            end;
          end;

          if AFileName <> '' then
          begin
            AFileInfo := TdxFileInfo.Create;
            AFileInfo.FileName := AFileName;
            AFileInfo.Pidl := GetPidlCopy(APIDL);
            AFileInfo.IsLink := AIsLink;
            AFileInfo.TargetFileName := ATargetFileName;
            AFileInfos.Add(AFileInfo);
          end;

          if ASelectedCount > 0 then
            ASelectedItems.Append(#32);
          ASelectedItems.Append(#34);
          ASelectedItems.Append(AFolder.DisplayName);
          ASelectedItems.Append(#34);
          Inc(ASelectedCount);
        end;
      end;
      if ASelectedItems.Length > 0 then
      begin
        if ASelectedCount = 1 then
        begin
          ASelectedItems.Remove(0, 1);
          ASelectedItems.Remove(ASelectedItems.Length - 1, 1);
        end;
        cbName.Text := ASelectedItems.ToString;
        FFileInfos.Clear;
      end;
      for I := 0 to AFileInfos.Count - 1 do
        FFileInfos.Add(AFileInfos[I]);
    finally
      AFileInfos.Free;
      ASelectedItems.Free;
    end;
    dxCallNotify(OnSelectionChange, Self);
  finally
    FIsSelectionChanging := False;
  end;
end;

procedure TdxfrmCommonFileDialog.DoShellListViewFolderChanged(Sender: TObject;
Root: TcxCustomShellRoot);
begin
  dxCallNotify(OnFolderChange, Self);
end;

procedure TdxfrmCommonFileDialog.DoShellTreeViewAddFolder(Sender: TObject;
AFolder: TcxShellFolder; var ACanAdd: Boolean);
var
  AShellItem2: IShellItem2;
  APropertyKey: TPropertyKey;
  AValue: LongBool;
begin
  if ACanAdd then
  begin
    if IsWinSevenOrLater and not FShowAllFolders and
      (GetDesktopIShellFolder = AFolder.ParentShellFolder) and
      Succeeded(SHCreateItemWithParent(nil, AFolder.ParentShellFolder,
      AFolder.RelativePIDL, IID_IShellItem2, AShellItem2)) then
    begin
      APropertyKey.fmtid := PKEY_AddToTreeView;
      APropertyKey.pid := 2;
      if Succeeded(AShellItem2.GetBool(APropertyKey, AValue)) and not AValue
      then
        ACanAdd := False;
    end;
    ACanAdd := ACanAdd and ((sfscFileSysAncestor in AFolder.StorageCapabilities)
      or cxShellIsZipFile(AFolder));
  end;
end;

procedure TdxfrmCommonFileDialog.DoShellListViewAddFoler(Sender: TObject;
AFolder: TcxShellFolder; var ACanAdd: Boolean);
begin
  if ACanAdd and AFolder.IsFolder then
  begin
    ACanAdd := (sfscFileSysAncestor in AFolder.StorageCapabilities) or
      cxShellIsZipFile(AFolder) and TdxShellListViewAccess(FShellListView)
      .CheckFileMask(AFolder);
  end;
end;

procedure TdxfrmCommonFileDialog.DoViewClick(Sender: TObject);
begin
  ChangeView(TComponent(Sender).Tag);
end;

procedure TdxfrmCommonFileDialog.dxLayoutSplitterItemCanResize(Sender: TObject;
AItem: TdxCustomLayoutItem; var ANewSize: Integer; var AAccept: Boolean);
var
  ADelta: Integer;
begin
  ADelta := ANewSize - AItem.Width;
  if liShellListView.Control.Width - ADelta < liShellListView.ControlOptions.MinWidth
  then
    AAccept := False;
end;

procedure TdxfrmCommonFileDialog.DoViewChanged(Sender: TObject);
begin
  ViewChanged;
end;

procedure TdxfrmCommonFileDialog.btnBackClick(Sender: TObject);
begin
  pmHistory.Items[GetCheckedHistoryItemIndex + 1].Click;
end;

procedure TdxfrmCommonFileDialog.btnForwardClick(Sender: TObject);
begin
  pmHistory.Items[GetCheckedHistoryItemIndex - 1].Click;
end;

procedure TdxfrmCommonFileDialog.btnNewFolderClick(Sender: TObject);
begin
  TdxShellListViewAccess(FShellListView).CreateNewFolder;
end;

procedure TdxfrmCommonFileDialog.btnUpClick(Sender: TObject);
begin
  FShellListView.BrowseParent;
  UpdateButtonsStates;
end;

procedure TdxfrmCommonFileDialog.btnViewsClick(Sender: TObject);
var
  I: Integer;
begin
  I := TdxShellListViewAccess(FShellListView).GetCurrentViewId;
  if I > actViews.ActionCount - 1 then
    I := IfThen(IsWinSevenOrLater, 0, 1)
  else if not IsWinSevenOrLater and (I = 3) then
    Inc(I, 1);
  actViews.Actions[I].Execute;
end;

procedure TdxfrmCommonFileDialog.cbFilterPropertiesEditValueChanged
  (Sender: TObject);
begin
  ApplyFilter(cbFilter.ItemIndex);
  dxCallNotify(OnTypeChange, Self);
end;

procedure TdxfrmCommonFileDialog.cbNamePropertiesChange(Sender: TObject);
begin
  if FIsSelectionChanging then
    Exit;
  FShellListView.ClearSelection;
  FFileInfos.Clear;
end;

procedure TdxfrmCommonFileDialog.sbePathPathSelected(Sender: TObject);
begin
  SyncPaths(sbePath.SelectedPidl, sbePath);
end;

constructor TdxfrmCommonFileDialog.Create(AOwner: TComponent);
var
  ARegistry: TRegistry;
begin
  inherited Create(AOwner);
  FCanSyncPaths := True;
  FFileInfos := TObjectList<TdxFileInfo>.Create;
  FFileTypes := TFileTypeItems.Create(TFileTypeItem);
  FFilterIndex := 1;
  FHistoryUpdateCount := 0;
  FMaxHistoryItemsCount := 10;
  sbePath.AutoSize := True;

  FShowAllFolders := not IsWinSevenOrLater;
  ARegistry := TRegistry.Create;
  try
    ARegistry.RootKey := HKEY_CURRENT_USER;
    if ARegistry.OpenKeyReadOnly(SExplorerAdvanced) then
    begin
      if ARegistry.ValueExists(SShowAllFolders) then
        FShowAllFolders := ARegistry.ReadBool(SShowAllFolders);
      if ARegistry.ValueExists(SHideFileExt) then
        FHideFileExt := ARegistry.ReadBool(SHideFileExt);
      ARegistry.CloseKey;
    end;
  finally
    ARegistry.Free;
  end;

  FShellListView := TdxShellListView.Create(Self);
  FShellListView.ShellOptions.CurrentFolderContextMenu := True;
  FShellListView.ShellOptions.ShowZipFilesWithFolders := False;
  FShellListView.AfterNavigation := DoShellListViewAfterNavigation;
  FShellListView.OnExecuteItem := DoChooseFile;
  FShellListView.OnSelectionChanged := DoShellListViewSelectionChanged;
  FShellListView.OnAddFolder := DoShellListViewAddFoler;
  FShellListView.OnRootChanged := DoShellListViewFolderChanged;
  TdxShellListViewAccess(FShellListView).OnViewChanged := DoViewChanged;
  FShellListView.Sorting := True;
  liShellListView.Control := FShellListView;

  FShellTreeView := TdxShellDialogTreeView.Create(Self);
  FShellTreeView.OnPathChanged := DoShellTreeViewPathChanged;
  FShellTreeView.OnAddFolder := DoShellTreeViewAddFolder;
  FShellTreeView.OnExpandStateChanged := DoShellTreeViewNodeExpandStateChanged;
  FShellTreeView.BeginUpdate;
  FShellTreeView.Width := 50;
  FShellTreeView.ShellOptions.BeginUpdate;
  FShellTreeView.ShellOptions.ShowNonFolders := False;
  FShellTreeView.ShellOptions.ShowToolTip := False;
  FShellTreeView.ShellOptions.EndUpdate;
  FShellTreeView.FirstLevelNodesVisible := FShowAllFolders;
  FShellTreeView.FocusNodeOnMouseUp := True;
  FShellTreeView.ShowHint := True;
  liShellTreeView.Control := FShellTreeView;
  FShellTreeView.EndUpdate;

  TcxCustomButtonAccess(btnBack).ShowFocusRect := False;
  TcxCustomButtonAccess(btnForward).ShowFocusRect := False;
  TcxCustomButtonAccess(btnHistory).ShowFocusRect := False;
  TcxCustomButtonAccess(btnUp).ShowFocusRect := False;
  TcxCustomButtonAccess(btnNewFolder).ShowFocusRect := False;
  TcxCustomButtonAccess(btnViews).ShowFocusRect := False;

  actExtraLarge.Visible := IsWinSevenOrLater;
  actSmall.Visible := IsWinSevenOrLater;

  RestoreListViewState;

  InitializeLookAndFeel;

  RestoreExpandedState;
end;

destructor TdxfrmCommonFileDialog.Destroy;
var
  I: Integer;
begin
  StoreExpandedState;
  for I := 0 to pmHistory.Items.Count - 1 do
    DisposePidl(PItemIDList(pmHistory.Items[I].Tag));
  FreeAndNil(FShellTreeView);
  FreeAndNil(FShellListView);
  FreeAndNil(FFileTypes);
  FreeAndNil(FFileInfos);
  inherited Destroy;
end;

procedure TdxfrmCommonFileDialog.ApplyLocalization;
var
  ALibraryHandle: THandle;
  I: Integer;
begin
  ALibraryHandle := LoadLibraryEx('shell32.dll', 0, LOAD_LIBRARY_AS_DATAFILE);
  try
    btnCancel.Caption := dxGetLocalizedSystemResourceString(cxSEditButtonCancel,
      ALibraryHandle, 28743);
    btnNewFolder.Caption := dxGetLocalizedSystemResourceString
      (sdxFileDialogNewFolderCaption, ALibraryHandle,
      IfThen(IsWinSevenOrLater, 16859, 30320));
    for I := 0 to actViews.ActionCount - 1 do
      TAction(actViews.Actions[I]).Caption :=
        TdxShellListViewAccess(FShellListView)
        .GetViewCaption(actViews.Actions[I].Tag, ALibraryHandle);
  finally
    FreeLibrary(ALibraryHandle);
  end;

  ALibraryHandle := LoadLibraryEx(PWideChar(IfThen(IsWinSevenOrLater,
    'comdlg32.dll', 'shell32.dll')), 0, LOAD_LIBRARY_AS_DATAFILE);
  try
    licbName.Caption := dxGetLocalizedSystemResourceString
      (sdxFileDialogFileNameCaption, ALibraryHandle,
      IfThen(IsWinSevenOrLater, 433, 9124));
  finally
    FreeLibrary(ALibraryHandle);
  end;

  beSearch.Properties.Nullstring :=
    cxGetResourceString(@sdxFileDialogSearchNullstring);
end;

procedure TdxfrmCommonFileDialog.InitializeFilter(const AFilter: string;
const AFilterIndex: Integer);
var
  I: Integer;
begin
  FFilterIndex := AFilterIndex;
  InitializeFileTypes(AFilter);
  licbFilter.Visible := FileTypes.Count > 0;
  if not licbFilter.Visible then
  begin
    licbName.AlignVert := avCenter;
    Exit;
  end;
  cbFilter.Properties.BeginUpdate;
  try
    for I := 0 to FileTypes.Count - 1 do
      cbFilter.Properties.Items.Add(FileTypes[I].DisplayName);
    if InRange(FilterIndex, 0, FileTypes.Count) then
      cbFilter.ItemIndex := IfThen(FilterIndex <> 0, FilterIndex - 1)
    else
      cbFilter.ItemIndex := FileTypes.Count - 1;
  finally
    cbFilter.Properties.EndUpdate;
  end;
end;

procedure TdxfrmCommonFileDialog.InitializeFolder(const AInitialDir: string);
begin
  if IsWinSevenOrLater and (FLoadedInitialDirPidl = nil) and (AInitialDir = '')
  then
    RestoreInitialDir;
  if FLoadedInitialDirPidl = nil then
  begin
    if (AInitialDir = '') and IsWinSevenOrLater then
      FInitializeDir := GetCurrentDir
    else
      FInitializeDir := AInitialDir;
    if FInitializeDir <> '' then
      FShellListView.Path := FInitializeDir;
  end;
end;

procedure TdxfrmCommonFileDialog.SetHistoryList(AHistoryList: TStrings);
begin
  cbName.Properties.Items.Assign(AHistoryList);
end;

procedure TdxfrmCommonFileDialog.ApplyFilter(const AIndex: Integer);
begin
  FFilterIndex := AIndex + 1; // due to specific VCL's property behavior
  SetActiveMask(FileTypes[AIndex].FileMask);
end;

procedure TdxfrmCommonFileDialog.BeginUpdateHistory;
begin
  Inc(FHistoryUpdateCount);
end;

function TdxfrmCommonFileDialog.CanUpdateHistory: Boolean;
begin
  Result := FHistoryUpdateCount = 0;
end;

procedure TdxfrmCommonFileDialog.ChangeView(AViewId: Integer);
begin
  TdxShellListViewAccess(FShellListView).ChangeView(AViewId);
  ViewChanged;
end;

procedure TdxfrmCommonFileDialog.DoClose(var Action: TCloseAction);

  function IsFirstSelectedItemBrowsable: Boolean;
  var
    AItemInfo: TcxShellItemInfo;
    AIndex: Integer;
  begin
    Result := ShellListViewSelectedItemIndices.Count > 0;
    if Result then
    begin
      AIndex := ShellListViewSelectedItemIndices[0];
      AItemInfo := TdxShellListViewAccess(FShellListView).GetItemInfo(AIndex);
      Result := AItemInfo.IsFolder and not cxShellIsZipFile(AItemInfo.Folder) or
        AItemInfo.IsFolderLink and not AItemInfo.IsZipFolderLink and
        not(ofNoDereferenceLinks in Options);
    end;
  end;

  procedure UpdateFileName;
  var
    AExt, ASelectedExt: string;
    AValue: string;
  begin
    if ModalResult = mrOk then
    begin
      if FFileInfos.Count > 0 then
        if FFileInfos[0].IsLink then
          AValue := FFileInfos[0].TargetFileName
        else
          AValue := FFileInfos[0].FileName
      else
        AValue := cbName.Text;
      if AValue = '' then
        Exit;
      if ExtractFilePath(AValue) = '' then
        AValue := IncludeTrailingPathDelimiter(FShellListView.Path) + AValue;
      ASelectedExt := '';
      if cbFilter.ItemIndex >= 0 then
        ASelectedExt := ExtractFileExt(FileTypes[cbFilter.ItemIndex].FileMask);
      if (ASelectedExt = '.*') or (ASelectedExt = '') then
      begin
        ASelectedExt := '';
        if DefaultExt <> '' then
          ASelectedExt := '.' + DefaultExt;
      end;
      AExt := ExtractFileExt(AValue);
      if (AExt = '') or (not FileExists(AValue) and
        ((cbFilter.ItemIndex = -1) or not SameText(AExt, ASelectedExt))) then
        AValue := AValue + ASelectedExt;
    end
    else
      AValue := FSavedFileName;
    FFileName := AValue;
  end;

var
  AFileMask: string;
  AFilterItemIndex: Integer;
  ACanClose: Boolean;
begin
  inherited DoClose(Action);
  UpdateFileName;
  if ModalResult = mrOk then
  begin
    if IsFirstSelectedItemBrowsable then
    begin
      TdxShellListViewAccess(FShellListView)
        .DoItemDblClick(FShellListView.Controller.FocusedItemIndex);
      Action := caNone;
    end
    else
    begin
      if IsMask(cbName.Text, AFileMask) then
      begin
        if FindMask(AFileMask, AFilterItemIndex) then
          cbFilter.ItemIndex := AFilterItemIndex
        else
          SetActiveMask(AFileMask);
        Action := caNone;
      end
      else if (cbName.Text = '') or not IsOptionsCheckPassed(FileName) then
        Action := caNone;
    end;
    if (Action <> caNone) and Assigned(OnFileOkClick) then
    begin
      OnFileOkClick(Self, ACanClose);
      if not ACanClose then
        Action := caNone;
    end;
  end;
end;

procedure TdxfrmCommonFileDialog.DoShow;
begin
  inherited DoShow;
  UpdateButtonsSize;
  UpdateButtonsStates;
  cbName.SetFocus;
end;

procedure TdxfrmCommonFileDialog.EndUpdateHistory;
begin
  Dec(FHistoryUpdateCount);
end;

function TdxfrmCommonFileDialog.FindMask(const AValue: string;
out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FileTypes.Count - 1 do
    if AnsiSameText(FileTypes[I].FileMask, AValue) then
    begin
      Result := True;
      AIndex := I;
      Break;
    end;
end;

procedure TdxfrmCommonFileDialog.InitializeFileTypes(const AFilter: string);

  function GetPartItem(var AStartPosition: Integer; var APart: string): Boolean;
  var
    ANextPosition: Integer;
  begin
    APart := '';
    if AStartPosition > Length(AFilter) then
      Exit(False);
    ANextPosition := PosEx('|', AFilter, AStartPosition);
    if ANextPosition <= 0 then
      ANextPosition := Length(AFilter) + 1;
    APart := Copy(AFilter, AStartPosition, ANextPosition - AStartPosition);
    AStartPosition := ANextPosition + 1;
    Result := Length(APart) > 0;
  end;

var
  APart: string;
  ACurrentPos: Integer;
  AFilterItem: TFileTypeItem;
begin
  if Pos('|', AFilter) = 0 then
    Exit;
  ACurrentPos := 1;
  while GetPartItem(ACurrentPos, APart) do
  begin
    AFilterItem := FileTypes.Add;
    AFilterItem.DisplayName := APart;
    if GetPartItem(ACurrentPos, APart) then
      AFilterItem.FileMask := APart
    else
      Break;
  end;
end;

procedure TdxfrmCommonFileDialog.SyncPaths(APIDL: PItemIDList;
ASyncControl: TWinControl = nil);
begin
  if FCanSyncPaths then
  begin
    FCanSyncPaths := False;
    try
      BeginUpdateHistory;
      try
        if ASyncControl <> sbePath then
          sbePath.SelectedPidl := APIDL;
        if ASyncControl <> FShellListView then
          FShellListView.AbsolutePIDL := APIDL;
        if ASyncControl <> FShellTreeView then
          SyncTreeView(APIDL);
      finally
        EndUpdateHistory;
      end;
      UpdateHistory(APIDL);
      UpdateButtonsStates;
    finally
      FCanSyncPaths := True;
    end;
  end;
end;

procedure TdxfrmCommonFileDialog.UpdateButtonsSize;

  procedure DoUpdateButtonSize(AButton: TcxButton);
  var
    ACalculator: TcxButtonStandardLayoutCalculatorAccess;
    ADesirededWidth, ACalculatedWidth: Integer;
  begin
    ACalculator := TcxButtonStandardLayoutCalculatorAccess.Create(AButton);
    try
      ADesirededWidth := TdxTextMeasurer.TextSizeDT(AButton.Font,
        AButton.Caption).cx + cxMarginsWidth(ACalculator.GetTextOffsets) +
        IfThen(AButton.OptionsImage.Glyph <> nil,
        ACalculator.OptionsImage.Spacing);

      ACalculator.Calculate;
      ACalculatedWidth := ACalculator.TextAreaSize.cx;

      if ADesirededWidth > ACalculatedWidth then
        AButton.Width := AButton.Width + (ADesirededWidth - ACalculatedWidth);
    finally
      ACalculator.Free;
    end;
  end;

begin
  DoUpdateButtonSize(btnNewFolder);
  DoUpdateButtonSize(btnOK);
  DoUpdateButtonSize(btnCancel);
end;

procedure TdxfrmCommonFileDialog.UpdateButtonsStates;
var
  ACheckedHistoryItemIndex: Integer;
  APIDL: PItemIDList;
begin
  ACheckedHistoryItemIndex := GetCheckedHistoryItemIndex;
  btnBack.Enabled := (ACheckedHistoryItemIndex >= 0) and
    (ACheckedHistoryItemIndex < (pmHistory.Items.Count - 1));
  btnForward.Enabled := ACheckedHistoryItemIndex > 0;
  APIDL := GetCurrentPidl;
  try
    btnUp.Enabled := APIDL.mkid.cb <> 0;
  finally
    DisposePidl(APIDL);
  end;
end;

procedure TdxfrmCommonFileDialog.UpdateHistory(APIDL: PItemIDList);
var
  ACheckedItemIndex: Integer;
  ADisplayName: string;
  AMenuItem: TMenuItem;
  I: Integer;
begin
  if CanUpdateHistory then
  begin
    ADisplayName := GetPIDLDisplayName(APIDL);
    if ADisplayName <> '' then
    begin
      AMenuItem := TMenuItem.Create(pmHistory);
      AMenuItem.Caption := ADisplayName;
      AMenuItem.Tag := NativeInt(GetPidlCopy(APIDL));
      AMenuItem.OnClick := DoHistoryItemClick;

      ACheckedItemIndex := GetCheckedHistoryItemIndex;
      if ACheckedItemIndex <> 0 then
        for I := ACheckedItemIndex - 1 downto 0 do
        begin
          DisposePidl(PItemIDList(pmHistory.Items[I].Tag));
          pmHistory.Items.Delete(I);
        end;
      pmHistory.Items.Insert(0, AMenuItem);
      if pmHistory.Items.Count > FMaxHistoryItemsCount then
      begin
        DisposePidl(PItemIDList(pmHistory.Items[pmHistory.Items.Count -
          1].Tag));
        pmHistory.Items.Delete(pmHistory.Items.Count - 1);
      end;
      CheckHistoryItem(AMenuItem);
    end;
    btnHistory.Enabled := pmHistory.Items.Count > 1;
  end;
end;

procedure TdxfrmCommonFileDialog.UpdateViews;
var
  ActionIndex: Integer;
  AAction: TCustomAction;
begin
  ActionIndex := TdxShellListViewAccess(FShellListView).GetCurrentViewId - 1;
  AAction := actViews.Actions[ActionIndex] as TCustomAction;
  AAction.Checked := True;
  btnViews.OptionsImage.ImageIndex := AAction.ImageIndex;
end;

function TdxfrmCommonFileDialog.IsSelectableItem(AIndex: Integer): Boolean;
begin
  Result := not ShellListView.Folders[AIndex].IsFolder or
    cxShellIsZipFile(ShellListView.Folders[AIndex]);
end;

procedure TdxfrmCommonFileDialog.CheckHistoryItem(AMenuItem: TMenuItem);
var
  I: Integer;
begin
  for I := 0 to pmHistory.Items.Count - 1 do
    pmHistory.Items[I].Checked := False;
  AMenuItem.Checked := True;
end;

procedure TdxfrmCommonFileDialog.DoShellTreeViewPathChanged(Sender: TObject);
begin
  SyncWithTreeView;
end;

procedure TdxfrmCommonFileDialog.DoShellTreeViewNodeExpandStateChanged
  (Sender: TdxCustomTreeView; Node: TdxTreeViewNode);
begin
  if (Node = Sender.FocusedNode) and Node.Expanded then
    ShellTreeView.FocusedNode := FShellTreeView.GetNodeByPIDL
      (FShellListView.ShellRoot.Pidl, True, True, Node);
end;

function TdxfrmCommonFileDialog.GetCheckedHistoryItemIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to pmHistory.Items.Count - 1 do
    if pmHistory.Items[I].Checked then
    begin
      Result := I;
      Break;
    end;
end;

function TdxfrmCommonFileDialog.GetCurrentPidl: PItemIDList;
begin
  Result := FShellListView.AbsolutePIDL;
end;

function TdxfrmCommonFileDialog.GetViewStatePropertyBag(APIDL: PItemIDList;
out APropertyBag: IPropertyBag): Boolean;

  function GetBagFlagsByPath(const APath: string): Cardinal;
  const
    SHGVSPB_FOLDER = 5;
    SHGVSPB_ROAM = 32;
  begin
    Result := SHGVSPB_FOLDER;
    if dxPathIsUNC(PChar(APath)) then
      Result := Result or SHGVSPB_ROAM;
  end;

  function GetStrProperty(ABag: IPropertyBag; const AName: string): string;
  var
    pvar: OleVariant;
  begin
    if Succeeded(ABag.Read(PChar(AName), pvar, nil)) and VarIsStr(pvar) then
      Result := VarToStr(pvar)
    else
      Result := '';
  end;

var
  AViewStatePropertyBagName: string;
  AFolderTypeId: TGUID;
  AFolderCanonicalName: string;
  AShellItem: IShellItem;
  AShellFolder: IShellFolder;
  AFolderType: IFolderType;
begin
  Result := False;
  if not IsWinSevenOrLater then
    Exit;
  AFolderTypeId := ID_GenericFolderTypeId;
  if Assigned(dxSHGetFolderTypeFromCanonicalName) then
    if Succeeded(dxSHGetViewStatePropertyBag(APIDL, PChar('Shell'), 5,
      IID_IPropertyBag, APropertyBag)) then
    begin
      AFolderCanonicalName := GetStrProperty(APropertyBag, 'FolderType');
      if AFolderCanonicalName = '' then
        AFolderCanonicalName := GetStrProperty(APropertyBag,
          'SniffedFolderType');
      if AFolderCanonicalName <> '' then
        dxSHGetFolderTypeFromCanonicalName(PChar(AFolderCanonicalName),
          AFolderTypeId)
      else if Succeeded(SHCreateItemFromIDList(APIDL, IID_IShellItem,
        AShellItem)) and Succeeded(AShellItem.BindToHandler(nil, BHID_SFObject,
        IID_IShellFolder, AShellFolder)) and
        Succeeded(AShellFolder.QueryInterface(IID_IFolderType, AFolderType))
      then
        AFolderType.GetFolderType(AFolderTypeId);
    end;
  AViewStatePropertyBagName := SComdlg + GUIDToString(AFolderTypeId);
  if Succeeded(dxSHGetViewStatePropertyBag(APIDL,
    PChar(AViewStatePropertyBagName), GetBagFlagsByPath(GetPidlName(APIDL)),
    IID_IPropertyBag, APropertyBag)) then
    Result := True;
end;

function TdxfrmCommonFileDialog.GetShellListViewSelectedItemIndices
  : TdxIntegerList;
begin
  Result := TdxListViewControllerAccess(FShellListView.Controller)
    .SelectedIndices;
end;

function TdxfrmCommonFileDialog.ReadExpandedState(APersistStream
  : IPersistStream): Boolean;
var
  AStream: TStream;
  AStreamAdapter: TStreamAdapter;
  ARegistry: TRegistryIniFile;
begin
  AStream := TMemoryStream.Create;
  try
    ARegistry := TRegistryIniFile.Create(SExplorerModules, KEY_READ);
    try
      Result := ARegistry.ReadBinaryStream(SNavPane, SExpandedState,
        AStream) > 0;
    finally
      ARegistry.Free;
    end;

    if Result then
    begin
      AStreamAdapter := TStreamAdapter.Create(AStream, soReference);
      try
        Result := Succeeded(APersistStream.Load(AStreamAdapter));
      finally
        AStreamAdapter.Free;
      end;
    end;

  finally
    AStream.Free;
  end;
end;

procedure TdxfrmCommonFileDialog.RestoreExpandedState;
var
  AShellItemArray: IShellItemArray;
  AShellItem: IShellItem;
  AShellItem2: IShellItem2;
  APropertyStore: IPropertyStore;
  APIDL: PItemIDList;
  APersistStream: IPersistStream;
  AItemCount: Cardinal;
  I: Integer;
  ANode: TdxTreeViewNode;
  APropertyKey: TPropertyKey;
  AProperty: TPropVariant;
begin
  if not IsWinSevenOrLater then
    Exit;
  if Succeeded(CoCreateInstance(CLSID_ShellItemArrayAsCollection, nil,
    CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, ID_IPersistStream,
    APersistStream)) and ReadExpandedState(APersistStream) and
    Succeeded(APersistStream.QueryInterface(IID_IShellItemArray,
    AShellItemArray)) and Succeeded(AShellItemArray.GetCount(AItemCount)) then
    for I := 0 to AItemCount - 1 do
      if Succeeded(AShellItemArray.GetItemAt(I, AShellItem)) and
        Succeeded(AShellItem.QueryInterface(IID_IShellItem2, AShellItem2)) and
        Succeeded(AShellItem2.GetPropertyStore(GPS_TEMPORARY,
        IID_IPropertyStore, APropertyStore)) then
      begin
        APropertyKey.fmtid := PKEY_ExpandState;
        APropertyKey.pid := 2;
        if Succeeded(APropertyStore.GetValue(APropertyKey, AProperty)) and
          Succeeded(SHGetIDListFromObject(AShellItem, APIDL)) then
          try
            ANode := FShellTreeView.GetNodeByPIDL(APIDL);
            if ANode <> nil then
              ANode.Expanded := AProperty.bool;
          finally
            DisposePidl(APIDL);
          end;
      end;
end;

procedure TdxfrmCommonFileDialog.RestoreInitialDir;
var
  AMRULongList: TdxMRULongList;
begin
  AMRULongList := TdxMRULongList.Create;
  try
    if AMRULongList.ReadInitialDirPidl(FLoadedInitialDirPidl) then
      FShellListView.AbsolutePIDL := FLoadedInitialDirPidl;
  finally
    AMRULongList.Free;
  end;
end;

function TdxfrmCommonFileDialog.GetInitialDir: string;
begin
  Result := FShellListView.Path;
end;

function TdxfrmCommonFileDialog.GetFileName: string;
begin
  Result := FFileName;
end;

function TdxfrmCommonFileDialog.GetFilterIndex: Integer;
begin
  Result := FFilterIndex;
end;

function TdxfrmCommonFileDialog.GetTitle: string;
begin
  Result := Caption;
end;

function TdxfrmCommonFileDialog.GetViewStyle: TdxListViewStyle;
begin
  Result := FShellListView.ViewStyle;
end;

procedure TdxfrmCommonFileDialog.InitializeLookAndFeel;
begin
  dxLayoutCxLookAndFeel_NoItemOffset.LookAndFeel.MasterLookAndFeel :=
    LookAndFeel;
  if LookAndFeel.SkinPainter <> nil then
    dxLayoutCxLookAndFeel_NoItemOffset.Offsets.ItemOffset := 0
  else
    dxLayoutCxLookAndFeel_NoItemOffset.Offsets.ItemOffset := 1;
  dxLayoutCxLookAndFeel.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  FShellTreeView.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  TdxShellListViewAccess(FShellListView).LookAndFeel.MasterLookAndFeel :=
    LookAndFeel;
  btnBack.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnForward.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnUp.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  sbePath.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  beSearch.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnOK.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnCancel.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  cbName.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  cbFilter.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnHistory.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnNewFolder.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnViews.LookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

function TdxfrmCommonFileDialog.IsMask(const AFileName: string;
var AFileMask: string): Boolean;
begin
  AFileMask := ExtractFileName(AFileName);
  Result := (Pos('*', AFileMask) > 0) or (Pos('?', AFileMask) > 0);
end;

function TdxfrmCommonFileDialog.IsOptionsCheckPassed(const AFileName
  : string): Boolean;
var
  ACheckFileExists: Boolean;
  ALibraryHandle: THandle;
  AMsg: string;
begin
  ACheckFileExists := (FFileInfos.Count > 0) and FFileInfos[0].IsLink or
    (ofFileMustExist in Options);
  Result := not ACheckFileExists or FileExists(AFileName);
  if not Result then
  begin
    MessageBox(0, PChar(AFileName + cxGetResourceString
      (@sdxFileDialogFileNotExistWarning)), PChar(Caption), MB_ICONWARNING or
      MB_OK or MB_TASKMODAL);
    Exit;
  end;

  Result := not(FileIsReadOnly(AFileName) and (ofNoReadOnlyReturn in Options));
  if not Result then
  begin
    ALibraryHandle := LoadLibraryEx('comdlg32.dll', 0,
      LOAD_LIBRARY_AS_DATAFILE);
    try
      AMsg := dxGetSystemResourceString(ALibraryHandle, 396);
      AMsg := StringReplace(AMsg, '%1', '%s', []);
      MessageBox(0, PChar(Format(AMsg, [AFileName])), PChar(Caption),
        MB_ICONWARNING or MB_OK or MB_TASKMODAL);
    finally
      FreeLibrary(ALibraryHandle);
    end;
  end;
end;

procedure TdxfrmCommonFileDialog.RestoreListViewState;

  function GetIntProperty(ABag: IPropertyBag; const AName: string): Int64;
  var
    pvar: OleVariant;
  begin
    if Succeeded(ABag.Read(PChar(AName), pvar, nil)) and VarIsNumeric(pvar) then
      Result := pvar
    else
      Result := -1;
  end;

var
  { AMode, } ALogicalViewMode, AIconSize: Int64;
  AShellListViewMode: Integer;
  APropertyBag: IPropertyBag;
begin
  AShellListViewMode := DefaultShellListViewMode;
  if GetViewStatePropertyBag(ShellListView.ShellRoot.Pidl, APropertyBag) then
  begin
    ALogicalViewMode := GetIntProperty(APropertyBag, SLogicalViewMode);
    // AMode := GetIntProperty(APropertyBag, SMode);
    AIconSize := GetIntProperty(APropertyBag, SIconSize);
    if ALogicalViewMode in [FLVM_ICONS, FLVM_TILES, FLVM_CONTENT] then
    begin
      if AIconSize < 32 then
        AShellListViewMode := cmdSmallIconId
      else if AIconSize < 96 then
        AShellListViewMode := cmdIconId
      else if AIconSize < 256 then
        AShellListViewMode := cmdLargeIconId
      else
        AShellListViewMode := cmdExtraLargeIconId;
    end
    else if ALogicalViewMode = FLVM_DETAILS then
      AShellListViewMode := cmdDetailId
    else if ALogicalViewMode = FLVM_LIST then
      AShellListViewMode := cmdListId;
  end;

  TdxShellListViewAccess(FShellListView).ChangeView(AShellListViewMode);
  UpdateViews;
end;

procedure TdxfrmCommonFileDialog.SetActiveMask(AValue: string);
begin
  FShellListView.ShellOptions.FileMask := ExtractFileName(AValue);
end;

procedure TdxfrmCommonFileDialog.SetFileName(const AValue: string);
var
  AFileInfo: TdxFileInfo;
begin
  FSavedFileName := AValue;
  if FHideFileExt then
    cbName.Text := ChangeFileExt(AValue, '')
  else
    cbName.Text := AValue;
  if AValue <> '' then
  begin
    AFileInfo := TdxFileInfo.Create;
    AFileInfo.FileName := AValue;
    FFileInfos.Add(AFileInfo);
  end;
end;

procedure TdxfrmCommonFileDialog.SetOptions(const AValue: TOpenOptions);
begin
  inherited SetOptions(AValue);
  FShellListView.ShellOptions.ShowHidden := ofForceShowHidden in Options;
  FShellTreeView.ShellOptions.ShowHidden := ofForceShowHidden in Options;
  sbePath.Properties.ShellOptions.ShowHiddenFolders := ofForceShowHidden
    in Options;
  FShellListView.ShellOptions.ShowReadOnly := not(ofHideReadOnly in Options);
  TdxShellListViewAccess(FShellListView).NavigateFolderLinks :=
    not(ofNoDereferenceLinks in Options);
end;

procedure TdxfrmCommonFileDialog.SetFileTypes(const AValue: TFileTypeItems);
begin
  if AValue <> nil then
    FFileTypes.Assign(AValue);
end;

procedure TdxfrmCommonFileDialog.StoreExpandedState;
var
  AShellItemArray: IShellItemArray;
  AShellItem2: IShellItem2;
  APropertyStore: IPropertyStore;
  APersistStream: IPersistStream;
  I: Integer;
  ANode: TdxTreeViewNode;
  APropertyKey: TPropertyKey;
  AProperty: TPropVariant;
  AObjectCollection: IObjectCollection;
begin
  if not IsWinSevenOrLater then
    Exit;

  if Succeeded(CoCreateInstance(CLSID_ShellItemArrayAsCollection, nil,
    CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IID_IShellItemArray,
    AShellItemArray)) then
  begin
    if Succeeded(AShellItemArray.QueryInterface(IID_IObjectCollection,
      AObjectCollection)) then
      for I := 0 to FShellTreeView.ShellRootNode.Count - 1 do
      begin
        ANode := FShellTreeView.ShellRootNode.Items[I];
        if Succeeded(SHCreateItemFromIDList(FShellTreeView.GetShellItemInfo
          (ANode).Pidl, IID_IShellItem2, AShellItem2)) and
          Succeeded(AObjectCollection.AddObject(AShellItem2)) and
          Succeeded(AShellItem2.GetPropertyStore(GPS_TEMPORARY,
          IID_IPropertyStore, APropertyStore)) then
        begin
          APropertyKey.fmtid := PKEY_ExpandState;
          APropertyKey.pid := 2;
          ZeroMemory(@AProperty, SizeOf(AProperty));
          AProperty.vt := VT_BOOL;
          AProperty.boolVal := ANode.Expanded;
          APropertyStore.SetValue(APropertyKey, AProperty);
        end;
      end;
    if Succeeded(AShellItemArray.QueryInterface(ID_IPersistStream,
      APersistStream)) then
      WriteExpandedState(APersistStream);
  end;
end;

procedure TdxfrmCommonFileDialog.StoreInitialDir;
var
  AMRULongList: TdxMRULongList;
  APIDL: PItemIDList;
begin
  APIDL := nil;
  if (FFileInfos.Count > 0) and (FFileInfos[0].Pidl <> nil) then
    APIDL := GetPidlCopy(GetPidlParent(FFileInfos[0].Pidl))
  else if (FFileInfos.Count = 0) or not FFileInfos[0].IsLink then
    APIDL := PathToAbsolutePIDL(ExtractFileDir(FileName),
      FShellListView.ShellRoot, [svoShowFiles, svoShowFolders,
      svoShowHidden], False);
  if APIDL = nil then
    APIDL := FShellListView.AbsolutePIDL;
  if not EqualPIDLs(APIDL, FLoadedInitialDirPidl) and (APIDL <> nil) then
    try
      AMRULongList := TdxMRULongList.Create;
      try
        AMRULongList.WriteInitialDirPidl(APIDL);
      finally
        AMRULongList.Free;
      end;
    finally
      DisposePidl(APIDL);
    end;
end;

procedure TdxfrmCommonFileDialog.SyncTreeView(APIDL: PItemIDList);
var
  ANode: TdxTreeViewNode;
begin
  ANode := FShellTreeView.GetNodeByPIDL(APIDL, True);
  if ANode <> nil then
  begin
    FShellTreeView.FocusedNode := ANode;
    FShellTreeView.MakeVisible(FShellTreeView.FocusedNode);
  end;
end;

procedure TdxfrmCommonFileDialog.SyncWithTreeView;
var
  ATreeViewPidl, ACurrentPidl: PItemIDList;
begin
  if (FShellTreeView.FocusedNode <> nil) and not FShellTreeView.FocusedNode.IsRoot
  then
  begin
    ACurrentPidl := GetCurrentPidl;
    ATreeViewPidl := FShellTreeView.AbsolutePIDL;
    try
      if not EqualPIDLs(ATreeViewPidl, ACurrentPidl) then
        SyncPaths(ATreeViewPidl, FShellTreeView);
    finally
      DisposePidl(ATreeViewPidl);
      DisposePidl(ACurrentPidl);
    end;
  end;
end;

procedure TdxfrmCommonFileDialog.ViewChanged;
begin
  UpdateViews;
  StoreListViewState;
end;

procedure TdxfrmCommonFileDialog.WriteExpandedState(APersistStream
  : IPersistStream);
var
  AStream: TStream;
  AStreamAdapter: TStreamAdapter;
  ARegistry: TRegistryIniFile;
begin
  AStream := TMemoryStream.Create;
  try
    AStreamAdapter := TStreamAdapter.Create(AStream, soReference);
    try
      APersistStream.Save(AStreamAdapter, True);
    finally
      AStreamAdapter.Free;
    end;

    ARegistry := TRegistryIniFile.Create(SExplorerModules);
    try
      AStream.Position := 0;
      ARegistry.WriteBinaryStream(SNavPane, SExpandedState, AStream);
    finally
      ARegistry.Free;
    end;

  finally
    AStream.Free;
  end;
end;

procedure TdxfrmCommonFileDialog.CMDialogKey(var Message: TCMDialogKey);
begin
  if GetKeyState(VK_MENU) >= 0 then
    case Message.CharCode of
      VK_RETURN:
        begin
          if ShellTreeView.Focused or sbePath.Focused then
          begin
            Message.Result := 0;
            Exit;
          end;
        end;
    end;
  inherited;
end;

procedure TdxfrmCommonFileDialog.WMAppCommand(var Msg: TMessage);
begin
  case GET_APPCOMMAND_LPARAM(Msg.lParam) of
    APPCOMMAND_BROWSER_BACKWARD:
      begin
        if btnBack.Enabled then
          btnBack.Click;
        Msg.Result := 1;
      end;
    APPCOMMAND_BROWSER_FORWARD:
      begin
        if btnForward.Enabled then
          btnForward.Click;
        Msg.Result := 1;
      end;
  end;
end;

procedure TdxfrmCommonFileDialog.SetTitle(const AValue: string);
begin
  Caption := AValue;
end;

procedure TdxfrmCommonFileDialog.SetViewStyle(const AValue: TdxListViewStyle);
begin
  if AValue <> FShellListView.ViewStyle then
  begin
    FShellListView.ViewStyle := AValue;
    UpdateViews;
  end;
end;

procedure TdxfrmCommonFileDialog.StoreListViewState;

  procedure SetIntProperty(ABag: IPropertyBag; const AName: string;
  AValue: Integer);
  var
    pvar: OleVariant;
  begin
    pvar := AValue;
    ABag.Write(PChar(AName), pvar);
  end;

var
  AViewId: Integer;
  AMode, ALogicalViewMode, AIconSize: Integer;
  APropertyBag: IPropertyBag;
begin
  if GetViewStatePropertyBag(FShellListView.ShellRoot.Pidl, APropertyBag) then
  begin
    AViewId := TdxShellListViewAccess(FShellListView).GetCurrentViewId;
    if AViewId = cmdDetailId then
    begin
      ALogicalViewMode := FLVM_DETAILS;
      AIconSize := -1;
      AMode := FVM_DETAILS;
    end
    else if AViewId = cmdListId then
    begin
      ALogicalViewMode := FLVM_LIST;
      AIconSize := -1;
      AMode := FVM_LIST;
    end
    else
    begin
      ALogicalViewMode := FLVM_ICONS;
      if AViewId = cmdSmallIconId then
      begin
        AIconSize := 16;
        AMode := FVM_SMALLICON;
      end
      else if AViewId = cmdIconId then
      begin
        AIconSize := 48;
        AMode := FVM_ICON;
      end
      else if AViewId = cmdLargeIconId then
      begin
        AIconSize := 96;
        AMode := FVM_ICON;
      end
      else // cmdExtraLargeIconId
      begin
        AIconSize := 256;
        AMode := FVM_ICON;
      end;
    end;

    SetIntProperty(APropertyBag, SLogicalViewMode, ALogicalViewMode);
    SetIntProperty(APropertyBag, SMode, AMode);
    if AIconSize <> -1 then
      SetIntProperty(APropertyBag, SIconSize, AIconSize);
  end;
end;

function TdxfrmCommonFileDialog.ShowModal: Integer;
var
  I: Integer;
  AFileName: string;
  AInitialDir: string;
begin
  dxFileDialogsSettings.LoadFormInfo(Self);
  Result := inherited ShowModal;
  if Result = mrOk then
  begin
    AFileName := FileName;
    if (FFileInfos.Count > 0) and FFileInfos[0].IsLink then
      AInitialDir := FShellListView.Path
    else
      AInitialDir := ExtractFileDir(AFileName);
    dxFileDialogsSettings.InitialDir := AInitialDir;
    if (AFileName <> '') and (FFileInfos.Count = 0) then
      Files.Add(AFileName)
    else
    begin
      if AInitialDir <> '' then
        AInitialDir := IncludeTrailingPathDelimiter(AInitialDir);
      for I := 0 to FFileInfos.Count - 1 do
      begin
        if FFileInfos[I].IsLink then
          AFileName := FFileInfos[I].TargetFileName
        else
        begin
          AFileName := FFileInfos[I].FileName;
          if ExtractFilePath(AFileName) = '' then
            AFileName := AInitialDir + AFileName;
        end;
        Files.Add(AFileName);
      end;
    end;
    if IsWinSevenOrLater then
      StoreInitialDir;
  end;
  dxFileDialogsSettings.SaveFormInfo(Self);
end;

{ TdxfrmCommonFileDialog.TdxFileInfo }

destructor TdxfrmCommonFileDialog.TdxFileInfo.Destroy;
begin
  Pidl := nil;
  inherited;
end;

procedure TdxfrmCommonFileDialog.TdxFileInfo.SetPidl(AValue: PItemIDList);
begin
  DisposePidl(FPidl);
  FPidl := AValue;
end;

{ TdxfrmCommonFileDialog.TdxShellDialogTreeView }

procedure TdxfrmCommonFileDialog.TdxShellDialogTreeView.Collapsed
  (Sender: TdxTreeCustomNode);
begin
  inherited Collapsed(Sender);
  if (FocusedNode <> nil) and FocusedNode.HasAsParent(Sender) then
    FocusedNode := Sender as TdxTreeViewNode;
end;

procedure TdxfrmCommonFileDialog.TdxShellDialogTreeView.DoFocusNodeByMouse
  (ANode: TdxTreeViewNode);
begin
  inherited DoFocusNodeByMouse(ANode);
  DoPathChanged;
end;

procedure TdxfrmCommonFileDialog.TdxShellDialogTreeView.DoNodeExpandStateChanged
  (ANode: TdxTreeViewNode);
begin
  inherited DoNodeExpandStateChanged(ANode);
  if Assigned(FOnExpandStateChanged) then
    FOnExpandStateChanged(Self, ANode);
end;

procedure TdxfrmCommonFileDialog.TdxShellDialogTreeView.DoPathChanged;
begin
  CallNotify(FOnPathChanged, Self);
end;

procedure TdxfrmCommonFileDialog.TdxShellDialogTreeView.KeyDown(var Key: Word;
Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_RETURN, VK_SPACE:
      DoPathChanged;
  end;
end;

{ TdxfrmOpenFileDialog }

procedure TdxfrmOpenFileDialog.ApplyLocalization;
var
  ALibraryHandle: THandle;
begin
  inherited ApplyLocalization;
  ALibraryHandle := LoadLibraryEx('comdlg32.dll', 0, LOAD_LIBRARY_AS_DATAFILE);
  try
    btnOK.Caption := dxGetLocalizedSystemResourceString
      (sdxOpenFileDialogOkCaption, ALibraryHandle, 370);
    Caption := dxGetLocalizedSystemResourceString(sdxOpenFileDialogDefaultTitle,
      ALibraryHandle, 384);
  finally
    FreeLibrary(ALibraryHandle);
  end;
end;

function TdxfrmOpenFileDialog.IsOptionsCheckPassed(const AFileName
  : string): Boolean;
var
  ALibraryHandle: THandle;
  AMsg: string;
begin
  Result := inherited IsOptionsCheckPassed(AFileName);
  if Result and (ofCreatePrompt in Options) and not FileExists(AFileName) then
  begin
    ALibraryHandle := LoadLibraryEx('comdlg32.dll', 0,
      LOAD_LIBRARY_AS_DATAFILE);
    try
      AMsg := dxGetSystemResourceString(ALibraryHandle, 402);
      AMsg := StringReplace(AMsg, '%1', '%s', []);
      Result := MessageBox(0, PChar(Format(AMsg, [AFileName])), PChar(Caption),
        MB_ICONWARNING or MB_YESNO or MB_TASKMODAL) = mrYes;
    finally
      FreeLibrary(ALibraryHandle);
    end;
  end;
end;

procedure TdxfrmOpenFileDialog.SetOptions(const AValue: TOpenOptions);
begin
  inherited SetOptions(AValue);
  ShellListView.Options.MultiSelect := ofAllowMultiSelect in Options;
end;

{ TdxfrmSaveFileDialog }

procedure TdxfrmSaveFileDialog.ApplyLocalization;
var
  ALibraryHandle: THandle;
begin
  inherited ApplyLocalization;
  ALibraryHandle := LoadLibraryEx('comdlg32.dll', 0, LOAD_LIBRARY_AS_DATAFILE);
  try
    btnOK.Caption := dxGetLocalizedSystemResourceString
      (sdxSaveFileDialogOkCaption, ALibraryHandle, 369);
    Caption := RemoveAccelChars(dxGetLocalizedSystemResourceString
      (sdxSaveFileDialogDefaultTitle, ALibraryHandle, 369));
  finally
    FreeLibrary(ALibraryHandle);
  end;
end;

function TdxfrmSaveFileDialog.IsOptionsCheckPassed(const AFileName
  : string): Boolean;
var
  ALibraryHandle: THandle;
  AMsg: string;
begin
  Result := inherited IsOptionsCheckPassed(AFileName);
  if Result and (ofOverwritePrompt in Options) and FileExists(AFileName) then
  begin
    ALibraryHandle := LoadLibraryEx('comdlg32.dll', 0,
      LOAD_LIBRARY_AS_DATAFILE);
    try
      AMsg := dxGetSystemResourceString(ALibraryHandle, 257);
      AMsg := StringReplace(AMsg, '%1', '%s', []);
      Result := MessageBox(0, PChar(Format(AMsg, [AFileName])),
        PChar(dxGetSystemResourceString(ALibraryHandle, 435)), MB_ICONWARNING or
        MB_YESNO or MB_DEFBUTTON2 or MB_TASKMODAL) = mrYes;
    finally
      FreeLibrary(ALibraryHandle);
    end;
  end;
end;

{ TdxFileDialogsSettings }

constructor TdxFileDialogsSettings.Create;
begin
  FSettings := TObjectDictionary<TClass, TdxFileDialogInfo>.Create
    ([doOwnsValues]);
end;

destructor TdxFileDialogsSettings.Destroy;
begin
  FSettings.Free;
end;

procedure TdxFileDialogsSettings.DoLoad;
var
  ARegistry: TRegistry;
  AKeys: TStringList;
  AKey: string;
  AClass: TClass;
  AInfo: TdxFileDialogInfo;
begin
  ARegistry := TRegistry.Create(KEY_READ);
  try
    ARegistry.RootKey := HKEY_CURRENT_USER;
    if ARegistry.OpenKeyReadOnly(SettingsRootKeyName) then
      FInitialDir := ARegistry.ReadString(LastDirPropertyName)
    else
      Exit;
    AKeys := TStringList.Create;
    try
      ARegistry.GetKeyNames(AKeys);
      ARegistry.CloseKey;
      for AKey in AKeys do
      begin
        AClass := GetClass('Tdxfrm' + StringReplace(AKey, 'Form', '',
          [rfReplaceAll, rfIgnoreCase]));
        if (AClass = nil) or not AClass.InheritsFrom
          (TdxfrmCommonFileCustomDialog) then
          Continue;
        if (AClass <> nil) and ARegistry.OpenKeyReadOnly
          (SettingsRootKeyName + AKey) then
          try
            if ARegistry.ValueExists(SplitterPositionPropertyName) then
            begin
              AInfo := GetInfo(TdxfrmCommonFileCustomDialogClass(AClass));
              if ARegistry.ValueExists(SplitterPositionPropertyName) then
                AInfo.SplitterPosition :=
                  StrToInt(ARegistry.ReadString(SplitterPositionPropertyName));
              if ARegistry.ValueExists(WidthPropertyName) then
                AInfo.Size.cx :=
                  StrToInt(ARegistry.ReadString(WidthPropertyName));
              if ARegistry.ValueExists(HeightPropertyName) then
                AInfo.Size.cy :=
                  StrToInt(ARegistry.ReadString(HeightPropertyName));
            end;
          finally
            ARegistry.CloseKey;
          end;
      end;
    finally
      AKeys.Free;
    end;
  finally
    ARegistry.Free;
  end;
  Changed := False;
end;

procedure TdxFileDialogsSettings.DoSave;
var
  ARegistry: TRegistry;
  AKey: string;
  AClass: TClass;
  AInfo: TdxFileDialogInfo;
begin
  if not Changed then
    Exit;
  ARegistry := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    ARegistry.RootKey := HKEY_CURRENT_USER;
    if not IsWinSevenOrLater and ARegistry.OpenKey(SettingsRootKeyName, True)
    then
    begin
      ARegistry.WriteString(LastDirPropertyName, FInitialDir);
      ARegistry.CloseKey;
    end;
    for AClass in FSettings.Keys do
      if FSettings.TryGetValue(AClass, AInfo) and AInfo.Changed then
      begin
        AKey := StringReplace(AClass.ClassName, 'Tdxfrm', '',
          [rfReplaceAll, rfIgnoreCase]) + 'Form';
        if ARegistry.OpenKey(SettingsRootKeyName + AKey, True) then
          try
            ARegistry.WriteString(SplitterPositionPropertyName,
              IntToStr(AInfo.SplitterPosition));
            ARegistry.WriteString(WidthPropertyName, IntToStr(AInfo.Size.cx));
            ARegistry.WriteString(HeightPropertyName, IntToStr(AInfo.Size.cy));
            AInfo.Changed := False;
          finally
            ARegistry.CloseKey;
          end;
      end;
  finally
    Changed := False;
    ARegistry.Free;
  end;
end;

function TdxFileDialogsSettings.GetInfo(AClass: TClass): TdxFileDialogInfo;
begin
  if FSettings.TryGetValue(AClass, Result) then
    Exit;
  Result := TdxFileDialogInfo.Create;
  Result.Position := cxInvisiblePoint;
  Result.Size := Self.DefaultSize;
  Result.SplitterPosition := Self.DefaultSplitterPosition;
  FSettings.Add(AClass, Result);
end;

function TdxFileDialogsSettings.GetSize(AClass: TClass): TSize;
begin
  Result := GetInfo(AClass).Size;
end;

procedure TdxFileDialogsSettings.LoadFormInfo
  (AInstance: TdxfrmCommonFileCustomDialog);
var
  R, AScreenBounds: TRect;
  AInfo: TdxFileDialogInfo;
begin
  try
    DoLoad;
  except
    on ERegistryException do;
  end;
  AInfo := GetInfo(AInstance.ClassType);
  AInstance.Width := AInstance.ScaleFactor.Apply(AInfo.Size.cx);
  AInstance.Height := AInstance.ScaleFactor.Apply(AInfo.Size.cy);
  AInstance.Position := poMainFormCenter;
  if AInstance is TdxfrmCommonFileDialog and (AInfo.SplitterPosition > 0) then
  begin
    TdxfrmCommonFileDialog(AInstance).liShellTreeView.Width :=
      AInstance.ScaleFactor.Apply(AInfo.SplitterPosition);
    if (Application.MainForm <> nil) and Application.MainForm.HandleAllocated
    then
    begin
      R := cxRectCenter(Application.MainForm.BoundsRect, AInstance.Width,
        AInstance.Height);
      AScreenBounds := Application.MainForm.Monitor.BoundsRect;
      if (R.Left < AScreenBounds.Left) or (R.Top < AScreenBounds.Top) then
      begin
        AInstance.Position := poDesigned;
        AInstance.Left := Max(AScreenBounds.Left, AInfo.Position.X);
        AInstance.Top := Max(AScreenBounds.Top, AInfo.Position.Y);
      end;
    end;
  end;
  if not cxPointIsEqual(AInfo.Position, cxInvisiblePoint) then
  begin
    AInstance.Position := poDesigned;
    AInstance.Left := AInfo.Position.X;
    AInstance.Top := AInfo.Position.Y;
  end
  else
    AInstance.Position := poMainFormCenter;

  if not IsWinSevenOrLater and (AInstance.FInitializeDir = '') and
    (AInstance.GetFileName = '') then
    AInstance.InitializeFolder(InitialDir);
end;

procedure TdxFileDialogsSettings.SaveFormInfo
  (AInstance: TdxfrmCommonFileCustomDialog);
var
  AInfo: TdxFileDialogInfo;
begin
  AInfo := GetInfo(AInstance.ClassType);
  AInfo.Position := Point(AInstance.Left, AInstance.Top);
  AInfo.Size := AInstance.ScaleFactor.Revert(cxSize(AInstance.Width,
    AInstance.Height));
  if AInstance is TdxfrmCommonFileDialog then
    AInfo.SplitterPosition := AInstance.ScaleFactor.Revert
      (TdxfrmCommonFileDialog(AInstance).liShellTreeView.Width);
  AInfo.Changed := True;
  Changed := True;
  try
    DoSave;
  except
    on ERegistryException do;
  end;
end;

function TdxFileDialogsSettings.GetSplitterPosition(AClass: TClass): Integer;
begin
  Result := GetInfo(AClass).SplitterPosition;
end;

procedure TdxFileDialogsSettings.SetSize(AClass: TClass; const AValue: TSize);
var
  AInfo: TdxFileDialogInfo;
begin
  AInfo := GetInfo(AClass);
  if cxSizeIsEqual(AValue, AInfo.Size) then
    Exit;
  AInfo.Size := AValue;
  AInfo.Changed := True;
  Changed := True;
end;

procedure TdxFileDialogsSettings.SetInitialDir(const AValue: string);
begin
  if AValue <> FInitialDir then
  begin
    FInitialDir := AValue;
    Changed := True;
  end;
end;

procedure TdxFileDialogsSettings.SetSplitterPosition(AClass: TClass;
AValue: Integer);
var
  AInfo: TdxFileDialogInfo;
begin
  AInfo := GetInfo(AClass);
  if AValue = AInfo.SplitterPosition then
    Exit;
  AInfo.SplitterPosition := AValue;
  AInfo.Changed := True;
  Changed := True;
end;

initialization

dxFileDialogsSettings := TdxFileDialogsSettings.Create;
RegisterClasses([TdxfrmOpenFileDialog, TdxfrmSaveFileDialog]);

finalization

FreeAndNil(dxFileDialogsSettings);

end.
