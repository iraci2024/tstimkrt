unit FlatSysex;
//download by http://www.codefans.net
{$I FlatStyle.inc}

interface

uses
 Windows, Messages, Controls, Forms, SysUtils, DB, DBCtrls, DBGrids,
 MMSystem, Classes, DBConsts, Grids, FlatExcfm, FlatUtils, Dialogs,
 Menus, Graphics, ShellApi, ExtCtrls, FlatWatet;

type
  //导出数据选项,
  //dmDefault为导出的数据默认为字段类型的数据,
  //dmString为导出的所有数据全部转换为字符类型
  TEduceType  = (dmDefault,dmString);
  TEduceMode  = (emDefault,emSingle);
  TEduceData  = class;
  TEduceDatas = class;
  TEduceLink  = class;
  { TDefineExcel }
  TDefineExcel  = Class(TVersionComponent)
  Private
    fCol        : word;
    fRow        : word;
    ExcelStream : TStream;
    FEduceType  : TEduceType;
    FColumns    : TEduceDatas;
    FUpdateLock : Byte;
    FLayoutLock : Byte;
    FDataLink   : TEduceLink;
    FLayoutSet  : Boolean;
    FEduceTitle : Boolean;
    FExcelForm  : TExcelForm;
    FInterval   : integer;
    FShowProgress: boolean;
    FFileName: String;
    FEduceMode: TEduceMode;
    FDefaultExt: String;
    function  GetFieldCount: Integer;
    function  GetDataSource: TDataSource;
    function  GetColumnCount: integer;
    function  GetEduceCount: integer;
    procedure SeTEduceType(const Value: TEduceType);
    procedure EndProgress;
    procedure StartProgress(Max: Integer);
    procedure SetColumns(const Value: TEduceDatas);
    procedure SetDataSource(const Value: TDataSource);
    procedure DefineFieldMap;
    function  GetFields(FieldIndex: Integer): TField;
    procedure SetDefaultExt(Value: String);
  protected
    // 以下是导出到 MS-Excel 操作过程
    procedure WriteData(Field: TField);
    procedure WriteTitle;
    procedure WriteBlankCell;
    procedure WriteFloatCell(const AValue: Double);
    procedure WriteIntegerCell(const AValue: Integer);
    procedure WriteStringCell(const AValue: string);
    procedure WritePrefix;
    procedure WriteSuffix;
    procedure WriteDataCells;
    procedure SaveExcel(Save: TStream);
    // 结束 MS-Excel 操作过程
    procedure BeginLayout;
    procedure EndLayout;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LayoutChanged; virtual;
    procedure LinkActive(Value: Boolean); virtual;
    procedure CancelLayout;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadColumns(Reader: TReader);
    procedure WriteColumns(Writer: TWriter);
    procedure Loaded; override;
    procedure InitColumns;
    procedure IncColRow;
    function  CreateDataLink: TEduceLink; dynamic;
    function  CreateColumns: TEduceDatas;
    function  AcquireLayoutLock: Boolean;
    property  UpdateLock: Byte read FUpdateLock;
    property  LayoutLock: Byte read FLayoutLock;
    property  DataLink: TEduceLink read FDataLink;
    property  LayoutSet: Boolean read FLayoutSet write FLayoutSet;
    property  EduceType: TEduceType read FEduceType write SeTEduceType default dmDefault;
    property  EduceDatas: TEduceDatas read FColumns write SetColumns;
    property  DataSource: TDataSource read GetDataSource write SetDataSource;
    property  EduceTitle: Boolean read FEduceTitle write FEduceTitle default true;
    property  ExcelForm: TExcelForm read FExcelForm;
    property  Interval: integer read FInterval write FInterval default 500;
    property  ShowProgress: boolean read FShowProgress write FShowProgress default true;
    property  FileName: String read FFileName write FFileName;
    property  EduceMode: TEduceMode read FEduceMode write FEduceMode default emSingle;
    property  DefaultExt: String read FDefaultExt write SetDefaultExt;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExportAll;
    procedure ExecuteSave;

    procedure InitFields;
    procedure RestoreFields;
    procedure ClearFields;
    property  Fields[FieldIndex: Integer]: TField read GetFields;
    property  FieldCount: Integer read GetFieldCount;
    property  ColumnCount: integer read GetColumnCount;
    property  EduceCount: integer read GetEduceCount;
  end;
  { FlatExcel }
  TFlatExcel  = Class(TDefineExcel)
  published
    property  EduceType;
    property  EduceDatas stored False;
    property  DataSource;
    property  EduceTitle;
    property  Interval;
    property  ShowProgress;
    property  FileName;
    property  EduceMode;
    property  DefaultExt;
  end;
  { TEduceLink }
  TEduceLink = class(TDataLink)
  private
    FCells: TDefineExcel;
    FFieldCount: Integer;
    FFieldMap: array of Integer;
    FModified: Boolean;
    FSparseMap: Boolean;
    function GetDefaultFields: Boolean;
    function GetFields(I: Integer): TField;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    function  GetMappedIndex(ColIndex: Integer): Integer;
    function  IsAggRow(Value: Integer): Boolean; virtual;
  public
    constructor Create(ADSExcel: TDefineExcel);
    destructor Destroy; override;
    procedure ClearMapping;
    function AddMapping(const FieldName: string): Boolean;
    property DefaultFields: Boolean read GetDefaultFields;
    property FieldCount: Integer read FFieldCount;
    property Fields[I: Integer]: TField read GetFields;
    property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Cells: TDefineExcel read FCells;
  end;
  { TEduceData }
  TEduceData = class(TCollectionItem)
  private
    FFieldName: string;
    FVisible: Boolean;
    FStored: Boolean;
    FCaption: String;
    FField: TField;
    procedure SetCaption(const Value: String);
    procedure SetField(Value: TField);
    function  GetField: TField;
    procedure SetFieldName(const Value: String);
    procedure SetVisible(const Value: Boolean);
  protected
    function  GetExcel: TDefineExcel;
    function  GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property  Cells: TDefineExcel read GetExcel;
    property  Field: TField read GetField write SetField;
    property  IsStored: Boolean read FStored write FStored default false;
  published
    property  Caption: string read fCaption write SetCaption;
    property  FieldName: String read fFieldName write SetFieldName;
    property  Visible: Boolean read FVisible write SetVisible;
  end;
  TEduceDataClass = class of TEduceData;
  TEduceDatasState = (csDefault, csCustomized);
  { TEduceDatas }
  TEduceDatas = class(TCollection)
  private
    FCells: TDefineExcel;
    function  GetColumn(Index: Integer): TEduceData;
    function  GetState: TEduceDatasState;
    procedure SetColumn(Index: Integer; Value: TEduceData);
    procedure SetState(NewState: TEduceDatasState);
  protected
    function  GetOwner: TPersistent; override;
    function  InternalAdd: TEduceData;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(DSExcel: TDefineExcel; ColumnClass: TEduceDataClass);
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure RebuildColumns;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    function  Add: TEduceData;
    property  State: TEduceDatasState read GetState write SetState;
    property  Cells: TDefineExcel read FCells;
    property  Items[Index: Integer]: TEduceData read GetColumn write SetColumn; default;
  end;
  { TFlatSound }
  TSoundEvent = (seBtnClick, seMenu, seMenuClick, seMoveIntoBtn, sePanelExpand);

  TFlatSound = class(TVersionComponent)
  private
    FEvent: TSoundEvent;
  public
    procedure Play;
    procedure PlayThis(ThisEvent: TSoundEvent);
    constructor Create(AOwner: TComponent); override;
  published
    property Event: TSoundEvent read FEvent write FEvent;
  end;

  { TFlatAnimWnd }
  TFlatAnimWnd = class;

  TFlatAnimHookWnd = class(TWinControl)
  private
    FAnimateWindow: TFlatAnimWnd;
    procedure WMCreate (var Message: TMessage); message WM_CREATE;
    procedure WMDestroy (var Message: TMessage); message WM_DESTROY;
  public
    constructor Create (AOwner: TComponent); override;
  end;

  TFlatAnimWnd = class(TVersionComponent)
  private
    FOwner: TComponent;
    FNewProc, FOldProc, FNewAppProc, FOldAppProc: TFarProc;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    procedure NewWndProc (var Message: TMessage);
    procedure NewAppWndProc (var Message: TMessage);
    procedure MinimizeWnd;
    procedure RestoreWnd;
    procedure OwnerWndCreated;
    procedure OwnerWndDestroyed;
  protected
    FHookWnd: TFlatAnimHookWnd;
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Minimize;
  published
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
  end;

  { TDefineSingle }
  TDefineSingle = class(TVersionComponent)
  private
    { Private declarations }
    FActive: boolean;
    FCaption: string;
    FTitle: string;
    procedure SetActive(Value: boolean);
    procedure SetCaption(const Value: string);
    procedure SetTitle(const Value: string);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Run(State:Boolean; Title:String);
    property Active: boolean read FActive write SetActive default True;
    property Caption: string read FCaption write SetCaption;
    property Title: string read FTitle write SetTitle;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

  TFlatSingle = class(TDefineSingle)
  published
    { Published declarations }
    property Active;
    property Caption;
    property Title;
  end;

  { TDefineTimer }
  TDefineTimer = class(TVersionComponent)
  private
    uTimerID: MMRESULT;
    FInterval: Cardinal;
    FPeriod: Cardinal;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetPeriod(Value: Cardinal); //设置分辨率
  protected
    procedure Timer; dynamic;
    procedure UpdateTimer;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property Period: Cardinal read FPeriod write SetPeriod default 10;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TFlatTimer }
  TFlatTimer = class(TDefineTimer)
  published
    property Enabled;
    property Interval;
    property Period;
    property OnTimer;
  end;

  { TDefineTaskbarIcon }
  TDefineTaskbarMode = (thDefault,thCustom);
  TDefineTaskbarIcon = class(TVersionComponent)
  private
    FActive: Boolean;
    FHint: string;
    FIcon: TIcon;
    FHandle: HWnd;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnRightClick: TNotifyEvent;
    FOnMouseMove: TNotifyEvent;
    FWMTaskbarCreated: UINT;
    FPopupMenu: TPopupMenu;
    FIconMode: TDefineTaskbarMode;
    FHintMode: TDefineTaskbarMode;
    //FOnMinimize: TNotifyEvent;
    //FOnRestore: TNotifyEvent;
    procedure SetActive(Value: Boolean);
    procedure SetHint(Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetHintMode(const Value: TDefineTaskbarMode);
    procedure SetIconMode(const Value: TDefineTaskbarMode);
  protected
    procedure PrivateWndProc(var Message: TMessage);
    procedure WndProc(var Message: TMessage); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function AppHook(var Message: TMessage): Boolean;
    function AddIcon: Boolean; dynamic;
    function DeleteIcon: Boolean; dynamic;
    function ModifyIcon(Aspect: Integer): Boolean; dynamic;
    function DoIcon(Action: DWORD; Aspect: UINT): Boolean; dynamic;
    property Handle: HWnd read FHandle;
    property Active: Boolean read FActive write SetActive;
    property Hint: string read FHint write SetHint;
    property HintMode: TDefineTaskbarMode read FHintMode write SetHintMode default thDefault;
    property Icon: TIcon read FIcon write SetIcon;
    property IconMode: TDefineTaskbarMode read FIconMode write SetIconMode default thDefault;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
    property OnMouseMove: TNotifyEvent read FOnMouseMove write FOnMouseMove;
    //property OnAppMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    //property OnAppRestore: TNotifyEvent read FOnRestore write FOnRestore;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFlatTaskbarIcon = class(TDefineTaskbarIcon)
  published
    property Active;
    property Hint;
    property HintMode;
    property Icon;
    property IconMode;
    property PopupMenu;
    property OnClick;
    property OnDblClick;
    property OnRightClick;
    property OnMouseMove;
    //property OnAppMinimize;
    //property OnAppRestore;
  end;

  { TDefineAnimation }
  TDefineAnimation = class(TVersionControl)
  private
    FTransparent: Boolean;
    FAnimation: TBitmap;
    FFrames: Integer;
    FFrameWidth: Integer;
    FFrame: Integer;
    FInterval: Integer;
    FTransColor: TColor;
    FActive: Boolean;
    FLoop: Boolean;
    FReverse: Boolean;
    FTimer: TTimer;
    FBorderColor: TColor;
    FBorder: Boolean;
    FFrameChange: TOnFrameChange;
    FAnimationLayout: TAnimationLayout;
    procedure SetAnimation(Value: TBitmap);
    procedure SetFrames(Value: Integer);
    procedure SetFrameWidth(Value: Integer);
    procedure SetFrame(Value: Integer);
    procedure SetActive(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetLoop(Value: Boolean);
    procedure SetReverse(Value: Boolean);
    procedure SetInterval(Value: Integer);
    procedure SetBorder(Value: Boolean);
    procedure DoTimer(Sender: TObject);
    procedure SetColors(Index: Integer; Value: TColor);
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetAnimationLayout(const Value: TAnimationLayout);
  protected
    procedure Paint; override;
    property Animation: TBitmap read FAnimation write SetAnimation;
    property Frames: Integer read FFrames write SetFrames;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    property Frame: Integer read FFrame write SetFrame default 1;
    property Interval: Integer read FInterval write SetInterval;
    property ColorTransparent: TColor index 0 read FTransColor write SetColors default clFuchsia;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default DefaultBorderColor;
    property Active: Boolean read FActive write SetActive;
    property Loop: Boolean read FLoop write SetLoop;
    property Reverse: Boolean read FReverse write SetReverse;
    property Border: Boolean read FBorder write SetBorder default false;
    property AnimationLayout: TAnimationLayout read FAnimationLayout write SetAnimationLayout;
    property OnFrameChange: TOnFrameChange read FFrameChange write FFrameChange;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TFlatAnimation }
  TFlatAnimation = class(TDefineAnimation)
  published
    property Color;
    property Animation;
    property Frames;
    property FrameWidth;
    property Frame;
    property Interval;
    property ColorTransparent;
    property ColorBorder;
    property Active;
    property Loop;
    property Reverse;
    property Border;
    property AnimationLayout;
    property OnFrameChange;
    property Transparent;
    property Align;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;
  { TDefineHint }
  TDefineHint = class(TVersionComponent)
  private
    FHintFont: TFont;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FArrowBackgroundColor: TColor;
    FArrowColor: TColor;
    FHintWidth: Integer;
    FOnShowHint: TShowHintEvent;
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetHintFont (Value: TFont);
    procedure GetHintInfo (var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    property ColorBackground: TColor index 0 read FBackgroundColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default clBlack;
    property ColorArrowBackground: TColor index 2 read FArrowBackgroundColor write SetColors default $0053D2FF;
    property ColorArrow: TColor index 3 read FArrowColor write SetColors default clBlack;
    property MaxWidth: Integer read FHintWidth write FHintWidth default 200;
    property Font: TFont read FHintFont write SetHintFont;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
  end;
  { TDefineHintWindow }
  TDefineHintWindow = class(THintWindow)
  private
    FArrowPos: TArrowPos;
    FArrowPoint: TPoint;
    FHint: TDefineHint;
    function FindFlatHint: TDefineHint;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure ActivateHint(HintRect: TRect; const AHint: string); Override;
  end;
  TFlatHint = class(TDefineHint)
  published
    property ColorBackground;
    property ColorBorder;
    property ColorArrowBackground;
    property ColorArrow;
    property MaxWidth;
    property Font;
    property OnShowHint;
  end;
  { TBaseWater }
  TBaseWater = class(TVersionComponent)
  private
    FInterval: Cardinal;
    FHandle: HWND;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
  protected
    procedure Timer; dynamic;
    procedure UpdateTimer;
    procedure WndProc(var Msg: TMessage);
    property  Enabled: Boolean read FEnabled write SetEnabled default True;
    property  Interval: Cardinal read FInterval write SetInterval default 50;
    property  OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  TDefineImage = class(TVersionGraphic)
  private
    FBitmap: TBitmap;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FProportional: Boolean;
    FAutoShowCursor: Boolean;
    FAutoImage: Boolean;
    FLeaveImage: TBitmap;
    FEnterImage: TBitmap;
    FAutoCursor: TCursor;
    FMouseState: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TBitmap);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetEnterImage(const Value: TBitmap);
    procedure SetLeaveImage(const Value: TBitmap);
  protected
    procedure MouseEnter(Var Msg:TMessage);message CM_MouseEnter;
    procedure MouseLeave(Var Msg:TMessage);message CM_MouseLeave;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    function GetCanvas: TCanvas;
    procedure Paint; override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
              PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    property Center: Boolean read FCenter write SetCenter default False;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property Proportional: Boolean read FProportional write SetProportional default false;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property BMPEnter:TBitmap read FEnterImage write SetEnterImage;
    property BMPLeave:TBitmap read FLeaveImage write SetLeaveImage;
    property AutoImage:Boolean read FAutoImage write FAutoImage default false;
    property AutoCursor:TCursor read FAutoCursor Write FAutoCursor default crHandPoint;
    property AutoShowCursor:Boolean read FAutoShowCursor write FAutoShowCursor default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetPicture;
    property Canvas: TCanvas read GetCanvas;
    property OnMouseMove;
  end;   
  { TDefineWater }
  TDefineWater = class(TBaseWater)
  private
    FState: Integer;
    FParam: TOtherParam;
    FDamping: TWaterDamping;
    FBitmap: TBitmap;
    FImage: TDefineImage;
    FPlayState: boolean;
    FItems: TStringList;
    procedure SetDamping(Value: TWaterDamping);
    procedure SetItems(const Value: TStringList);
  protected
    FWater: TDefineWatet;
    FMoveHeight: Integer;
    TextHht: integer;
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    procedure Play(sender: TObject);
    procedure InitiateWater;
    property  Bitmap: TBitmap read FBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Damping: TWaterDamping read FDamping write SetDamping;
    property CtrlImage: TDefineImage read FImage write FImage;
    property Items: TStringList read FItems write SetItems;
    property Enabled;
    property Interval;
  end;
  { TFlatImage }
  TFlatImage = class(TDefineImage)
  published
    property AutoImage;
    property AutoCursor;
    property AutoShowCursor;
    property Align;
    property Anchors;
    property AutoSize;
    property Center;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property IncrementalDisplay;
    property ParentShowHint;
    property Bitmap;
    property BMPEnter;
    property BMPLeave;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnProgress;
    property OnStartDock;
    property OnStartDrag;
  end;
  { TFlatWater }
  TFlatWater = class(TDefineWater)
  published
    property Damping;
    property CtrlImage;
    property Items;
    property Enabled;
    property Interval;
  end;

implementation

{$R FlatSysex.res}

uses FlatExcpt, FlatCnsts;  

{ Error reporting }

procedure RaiseGridError(const S: string);
begin
  raise EInvalidGridOperation.Create(S);
end;

{ TEduceData }

constructor TEduceData.Create(Collection: TCollection);
var
  Excel: TDefineExcel;
begin
  Excel := nil;
  if Assigned(Collection) and (Collection is TEduceDatas) then
     Excel := TEduceDatas(Collection).Cells;
  if Assigned(Excel) then Excel.BeginLayout;
  try
   inherited Create(Collection);
   FVisible  := True;
   FStored   := True;
  finally
   if Assigned(Excel) then Excel.EndLayout;
  end;
end;

destructor TEduceData.Destroy;
begin
  inherited Destroy;
end;

procedure TEduceData.Assign(Source: TPersistent);
begin
  if Source is TEduceData then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      FieldName := TEduceData(Source).FieldName;
      FCaption  := TEduceData(Source).Caption;
      FVisible  := TEduceData(Source).Visible;
      Changed(false);
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end else inherited Assign(Source);
end;

function TEduceData.GetExcel: TDefineExcel;
begin
  if Assigned(Collection) and (Collection is TEduceDatas) then
     Result := TEduceDatas(Collection).Cells
  else
     Result := nil;
end;

function TEduceData.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
     Result := inherited GetDisplayName;
end;

procedure TEduceData.SetCaption(const Value: String);
begin
  if (Value <> FCaption) then
  begin
     FCaption := Value;
     Changed(false);
  end;
end;  

procedure TEduceData.SetField(Value: TField);
begin
  if FField = Value then Exit;
  if Assigned(FField) and (GetExcel <> nil) then
     FField.RemoveFreeNotification(GetExcel);
  if Assigned(Value) and (csDestroying in Value.ComponentState) then
     Value := nil;
  FField := Value;
  if Assigned(Value) then
  begin
    if GetExcel <> nil then
       FField.FreeNotification(GetExcel);
    FFieldName := Value.FullName;
    if (Length(FCaption)=0) and (Length(FieldName) > 0) then
    begin
       if Value.DisplayLabel = '' then
          FCaption := Value.FullName
       else
          FCaption := Value.DisplayLabel;
    end;
  end;
  if not IsStored then
  begin
    if Value = nil then
       FFieldName := '';
  end;
  Changed(False);
end;

function TEduceData.GetField: TField;
var
  Cell: TDefineExcel;
begin
  Cell := GetExcel;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Cell) and
      Assigned(Cell.DataLink.DataSet) then
  begin
  with Cell.Datalink.Dataset do
    if Active or (not DefaultFields) then
       SetField(FindField(FieldName));
  end;
  Result := FField;
end;

procedure TEduceData.SetFieldName(const Value: String);
var
  AField: TField;
  Cells: TDefineExcel;
begin
  AField := nil;
  Cells := GetExcel;
  if Assigned(Cells) and Assigned(Cells.DataLink.DataSet) and
    not (csLoading in Cells.ComponentState) and (Length(Value) > 0) then
      AField := Cells.DataLink.DataSet.FindField(Value); { no exceptions }
  FFieldName := Value;
  SetField(AField);
  Changed(False);
end;

procedure TEduceData.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
     FVisible := Value;
     Changed(false);
  end;
end;

{ TEduceDatas }

constructor TEduceDatas.Create(DSExcel: TDefineExcel; ColumnClass: TEduceDataClass);
begin
  inherited Create(ColumnClass);
  FCells := DSExcel;
end;

function TEduceDatas.Add: TEduceData;
begin
  Result := TEduceData(inherited Add);
end;

function TEduceDatas.GetColumn(Index: Integer): TEduceData;
begin
  Result := TEduceData(inherited Items[Index]);
end;

function TEduceDatas.GetOwner: TPersistent;
begin
  Result := FCells;
end;

procedure TEduceDatas.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

{ TEduceWrapper }
type
  TEduceWrapper = class(TComponent)
  private
    FColumns: TEduceDatas;
  published
    property Columns: TEduceDatas read FColumns write FColumns;
  end;

procedure TEduceDatas.LoadFromStream(S: TStream);
var
  Wrapper: TEduceWrapper;
begin
  Wrapper := TEduceWrapper.Create(nil);
  try
    Wrapper.Columns := FCells.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TEduceDatas.RebuildColumns;

  procedure AddFields(Fields: TFields; Depth: Integer);
  var
    I: Integer;
  begin
    Inc(Depth);
    for I := 0 to Fields.Count-1 do
    begin
      Add.FieldName := Fields[I].FullName;
      if Fields[I].DataType in [ftADT, ftArray] then
         AddFields((Fields[I] as TObjectField).Fields, Depth);
    end;
  end;

begin
  if Assigned(FCells) and Assigned(FCells.DataSource) and
     Assigned(FCells.Datasource.DataSet) then
  begin
    FCells.BeginLayout;
    try
      Clear;
      AddFields(FCells.DataSource.DataSet.Fields, 0);
    finally
      FCells.EndLayout;
    end
  end
  else
    Clear;
end;

procedure TEduceDatas.SaveToFile(const Filename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TEduceDatas.SaveToStream(S: TStream);
var
  Wrapper: TEduceWrapper;
begin
  Wrapper := TEduceWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

procedure TEduceDatas.SetColumn(Index: Integer; Value: TEduceData);
begin
  Items[Index].Assign(Value);
end;

procedure TEduceDatas.SetState(NewState: TEduceDatasState);
begin
  if NewState = State then Exit;
  if NewState = csDefault then
     Clear
  else
     RebuildColumns;
end;

function TEduceDatas.InternalAdd: TEduceData;
begin
  Result := Add;
  Result.FStored := False;
end;

function TEduceDatas.GetState: TEduceDatasState;
begin
  Result := TEduceDatasState((Count > 0) and Items[0].IsStored);
end;

procedure TEduceDatas.Update(Item: TCollectionItem);
begin
  if (FCells = nil) or (csLoading in FCells.ComponentState) then Exit;
  if Item = nil then
  begin
    FCells.LayoutChanged;
  end;
end;

{ TDefineExcel }

var
  ExcelBof    : array[0..5] of Word = ($809,  8, 0, $10,   0, 0);
  ExcelEof    : array[0..1] of Word = ($0A,  00);
  ExcelLabel  : array[0..5] of Word = ($204,  0, 0,   0,   0, 0);
  ExcelNum    : array[0..4] of Word = ($203, 14, 0,   0,   0);
  ExcelRec    : array[0..4] of Word = ($27E, 10, 0,   0,   0);
  ExcelBlank  : array[0..4] of Word = ($201,  6, 0,   0, $17);

Constructor TDefineExcel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FColumns      := CreateColumns;
 FDatalink     := CreateDatalink;
 FEduceType    := dmDefault;
 FEduceTitle   := true;
 FInterval     := 500;
 FShowProgress := true;
 FFileName     := '未命名表格文件';
 FEduceMode    := emSingle;
 FDefaultExt   := '.xls';
end;

destructor TDefineExcel.Destroy;
begin
  FColumns.Free;
  FColumns := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TDefineExcel.CreateColumns: TEduceDatas;
begin
  Result := TEduceDatas.Create(Self, TEduceData);
end;

procedure TDefineExcel.IncColRow;
begin
 if fCol = EduceCount - 1 then
 begin
    Inc(fRow);
    fCol :=0;
 end else
    Inc(fCol);
end;
//写空单元
procedure TDefineExcel.WriteBlankCell;
begin
 ExcelBlank[2] := fRow;
 ExcelBlank[3] := fCol;
 ExcelStream.WriteBuffer(ExcelBlank, SizeOf(ExcelBlank));
 IncColRow;
end;
//写浮点单元
procedure TDefineExcel.WriteFloatCell(const AValue: Double);
begin
 ExcelNum[2] := fRow;
 ExcelNum[3] := fCol;
 ExcelStream.WriteBuffer(ExcelNum, SizeOf(ExcelNum));
 ExcelStream.WriteBuffer(AValue, 8);
 IncColRow;
end;
//写整数单元
procedure TDefineExcel.WriteIntegerCell(const AValue: Integer);
var V: Integer;
begin
 ExcelRec[2] := fRow;
 ExcelRec[3] := fCol;
 ExcelStream.WriteBuffer(ExcelRec, SizeOf(ExcelRec));
 V := (AValue shl 2) or 2;
 ExcelStream.WriteBuffer(V, 4);
 IncColRow;
end;
//写字符单元
procedure TDefineExcel.WriteStringCell(const AValue: string);
var
 L: Word;
begin
 L := Length(AValue);
 ExcelLabel[1] := 8 + L;
 ExcelLabel[2] := fRow;
 ExcelLabel[3] := fCol;
 ExcelLabel[5] := L;
 ExcelStream.WriteBuffer(ExcelLabel, SizeOf(ExcelLabel));
 ExcelStream.WriteBuffer(Pointer(AValue)^, L);
 IncColRow;
end;
//写前缀
procedure TDefineExcel.WritePrefix;
begin
 ExcelStream.WriteBuffer(ExcelBof, SizeOf(ExcelBof));
end;
//写后缀
procedure TDefineExcel.WriteSuffix;
begin
 ExcelStream.WriteBuffer(ExcelEof, SizeOf(ExcelEof));
end;
//写标题
procedure TDefineExcel.WriteTitle;
var n: word;
begin
if FEduceTitle then
begin
 for n:= 0 to FColumns.Count - 1 do
 begin
   if FColumns[n].Visible then WriteStringCell(FColumns[n].Caption);
 end;
end;
end;

procedure TDefineExcel.StartProgress(Max:Integer);
begin
 if (not Assigned(FExcelForm))and(FShowProgress) then
    Application.CreateForm(TExcelForm, FExcelForm);
 if Assigned(FExcelForm) then
 begin
  with FExcelForm do
  begin
    ProGauge.Max     :=Max;
    ProGauge.Min     :=0;
    ProGauge.Progress:=0;
    Show;
    BringToFront;
  end;
 end;
end;

procedure TDefineExcel.EndProgress;
begin
 if Assigned(FExcelForm) then
 begin
  with FExcelForm do
  begin
    ProGauge.Progress := ProGauge.Progress+1;
    if ProGauge.Progress >= ProGauge.Max then
    begin
       Sleep(FInterval);
       Close;
    end;
  end;
  Application.ProcessMessages;
 end;
end;

procedure TDefineExcel.WriteData(Field:TField);
begin
   if Field.IsNull then
      WriteBlankCell
   else
   case FEduceType of
       dmDefault:
             case Field.DataType of
               ftSmallint,
               ftInteger,
               ftWord,
               ftAutoInc,
               ftBytes: WriteIntegerCell(Field.AsInteger);
               ftFloat,
               ftCurrency,
               ftBCD: WriteFloatCell(Field.AsFloat);
             else
               WriteStringCell(Field.AsString);
             end;
       dmString:WriteStringCell(Field.AsString);
   end;
end;
//正式写入Excel表的数据
procedure TDefineExcel.WriteDataCells;
var n: word;
    fBookMark : TBookmark;
begin
 //写入 Excel 文件开始格式
 WritePrefix;
 //写入标题名称
 WriteTitle;
 //开始写入各字段数据
 with FDataLink.DataSet do
 begin
  //禁止在数据感知控件中显示
  DisableControls;
  //初始化处理进度
  StartProgress(RecordCount);
  //记录当记录的位置
  fBookMark := GetBookmark;
  //指向第一条记录
  First;
  while not Eof do begin
   for n := 0 to ColumnCount - 1 do
   begin
    case FEduceMode of
     emSingle:
     begin
      if FColumns[n].Visible then
         WriteData(FColumns[n].Field);
     end;
     emDefault:
     begin
         WriteData(FColumns[n].Field);
     end;
    end;
   end;
   EndProgress;
   Next;
  end;
  //还原处理前的记录位置
  GotoBookmark(fBookMark);
  //充许在数据感知控件中显示
  EnableControls;
 end;
 //写入 Excel 文件结束标识
 WriteSuffix;
end;

procedure TDefineExcel.SaveExcel(Save: TStream);
begin
  fCol    := 0;
  fRow    := 0;
  ExcelStream := Save;
  WriteDataCells;
end;

procedure TDefineExcel.DefineFieldMap;
var
  I: Integer;
begin
  if FColumns.State = csCustomized then
  begin
    FDataLink.SparseMap := True;
    for I := 0 to FColumns.Count-1 do
      FDataLink.AddMapping(FColumns[I].FieldName);
  end
  else
  begin
    FDataLink.SparseMap := False;
    with FDataLink.Dataset do
      for I := 0 to FieldList.Count - 1 do
        with FieldList[I] do if Visible then FDataLink.AddMapping(FullName);
  end;
end;

procedure TDefineExcel.InitColumns;

  function FieldIsMapped(F: TField): Boolean;
  var
    X: Integer;
  begin
    Result := False;
    if F = nil then Exit;
    for X := 0 to FDataLink.FieldCount-1 do
      if FDataLink.Fields[X] = F then
      begin
        Result := True;
        Exit;
      end;
  end;

  procedure CheckForPassthroughs;  // check for Columns.State flip-flop
  var
    SeenPassthrough: Boolean;
    I, J: Integer;
    Column: TEduceData;
  begin
    SeenPassthrough := False;
    for I := 0 to FColumns.Count-1 do
      if not FColumns[I].IsStored then
        SeenPassthrough := True
      else if SeenPassthrough then
      begin
        for J := FColumns.Count-1 downto 0 do
        begin
          Column := FColumns[J];
          if not Column.IsStored then
            Column.Free;
        end;
        Exit;
      end;
  end;

  procedure ResetColumnFieldBindings;
  var
    I, J, K: Integer;
    Fld: TField;
    Column: TEduceData;
  begin
    if FColumns.State = csDefault then
    begin
      if (not FDataLink.Active) and (FDataLink.DefaultFields) then
          FColumns.Clear
      else
      begin
        for J := FColumns.Count-1 downto 0 do
        begin
          with FColumns[J] do
          begin
           if not Assigned(Field) or not FieldIsMapped(Field) then
              Free;
          end;
        end;
      end;
      I := FDataLink.FieldCount;
      //if (I = 0) and (FColumns.Count = 0) then
      //    Inc(I);
      for J := 0 to I-1 do
      begin
        Fld := FDataLink.Fields[J];
        if Assigned(Fld) then
        begin
          K := J;
          while (K < FColumns.Count) and (FColumns[K].Field <> Fld) do
            Inc(K);
          if K < FColumns.Count then
             Column := FColumns[K]
          else
          begin
             Column := FColumns.InternalAdd;
             Column.Field := Fld;
          end;
        end
        else
          Column := FColumns.InternalAdd;
        Column.Index := J;
      end;
    end
    else
    begin
      for I := 0 to FColumns.Count-1 do
          FColumns[I].Field := nil;
    end;
  end;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then
     Exit;
  CheckForPassthroughs;
  FDatalink.ClearMapping;
  if FDatalink.Active then
     DefineFieldMap;
  ResetColumnFieldBindings;
end;

procedure TDefineExcel.SeTEduceType(const Value: TEduceType);
begin
 if FEduceType <> Value then
    FEduceType := Value;
end;

procedure TDefineExcel.SetColumns(const Value: TEduceDatas);
begin
  FColumns.Assign(Value);
end;

procedure TDefineExcel.DefineProperties(Filer: TFiler);
var
  StoreIt: Boolean;
  vState: TEduceDatasState;
begin
  vState := EduceDatas.State;
  if Filer.Ancestor = nil then
    StoreIt := vState = csCustomized
  else
    if vState <> TDefineExcel(Filer.Ancestor).EduceDatas.State then
      StoreIt := True
    else
      StoreIt := (vState = csCustomized) and
        (not CollectionsEqual(EduceDatas, TDefineExcel(Filer.Ancestor).EduceDatas, Self, TDefineExcel(Filer.Ancestor)));

  Filer.DefineProperty('Columns', ReadColumns, WriteColumns, StoreIt);
  inherited DefineProperties(Filer);
end;

procedure TDefineExcel.ReadColumns(Reader: TReader);
begin
  EduceDatas.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(EduceDatas);
end;

procedure TDefineExcel.WriteColumns(Writer: TWriter);
begin
  if EduceDatas.State = csCustomized then
     Writer.WriteCollection(EduceDatas)
  else  // ancestor state is customized, ours is not
     Writer.WriteCollection(nil);
end;

function TDefineExcel.GetFieldCount: Integer;
begin
  if Assigned(FDataLink.DataSet) then
     result := FDataLink.FieldCount
  else
     result := 0;
end;

procedure TDefineExcel.BeginLayout;
begin
  BeginUpdate;
  if FLayoutLock = 0 then
     EduceDatas.BeginUpdate;
  Inc(FLayoutLock);
end;

procedure TDefineExcel.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TDefineExcel.EndLayout;
begin
  if FLayoutLock > 0 then
  begin
    try
      try
        if FLayoutLock = 1 then
           InitColumns;
      finally
        if FLayoutLock = 1 then
           FColumns.EndUpdate;
      end;
    finally
      Dec(FLayoutLock);
      EndUpdate;
    end;
  end;
end;

procedure TDefineExcel.EndUpdate;
begin
  if FUpdateLock > 0 then
     Dec(FUpdateLock);
end;

procedure TDefineExcel.LayoutChanged;
begin
  if AcquireLayoutLock then
     EndLayout;
end;

function TDefineExcel.AcquireLayoutLock: Boolean;
begin
  Result := (FUpdateLock = 0) and (FLayoutLock = 0);
  if Result then BeginLayout;
end;

procedure TDefineExcel.Loaded;
begin
  inherited Loaded;
  LayoutChanged;
end;

function TDefineExcel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDefineExcel.SetDataSource(const Value: TDataSource);
begin
  if Value = FDatalink.Datasource then Exit;
  if Assigned(Value) then
    if Assigned(Value.DataSet) then
      if Value.DataSet.IsUnidirectional then
         DatabaseError(SDataSetUnidirectional);
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TDefineExcel.LinkActive(Value: Boolean);
begin
  try
    LayoutChanged;
  finally
    //
  end;
end;

function TDefineExcel.CreateDataLink: TEduceLink;
begin
  Result := TEduceLink.Create(Self);
end;

function TDefineExcel.GetColumnCount: integer;
begin
  Result := FColumns.Count;
end;

function TDefineExcel.GetEduceCount: integer;
var
  i:integer;
begin
  result := 0;
  for i:= 0 to FColumns.Count - 1 do
      if FColumns[i].Visible then result := result + 1;
end;

procedure TDefineExcel.ExportAll;
var i:integer;
begin
  for i:=0 to ColumnCount - 1 do FColumns[i].Visible := True;
end;

function TDefineExcel.GetFields(FieldIndex: Integer): TField;
begin
  Result := FDatalink.Fields[FieldIndex];
end;

procedure TDefineExcel.CancelLayout;
begin
  if FLayoutLock > 0 then
  begin
    if FLayoutLock = 1 then
       EduceDatas.EndUpdate;
    Dec(FLayoutLock);
    EndUpdate;
  end;
end;

procedure TDefineExcel.ExecuteSave;
var
  SaveDlg: TSaveDialog;
  FileStream: TFileStream;
  inx: integer;
  UseState: boolean;
  tFile:String;
begin
case FEduceMode of
 emSingle:
 begin
 FieldForm := TFieldForm.Create(self);
 try
  FieldForm.FieldBox.Items.Clear;
  for inx := 0 to FColumns.Count - 1 do
  begin
      FieldForm.FieldBox.Items.Add(FColumns[inx].Caption);
      FieldForm.FieldBox.Checked[inx] := FColumns[inx].Visible;
  end;
  FieldForm.ShowModal;
  if FieldForm.ModalResult = mrOk then
  begin
   for inx := 0 to FieldForm.FieldBox.Items.Count - 1 do
       FColumns[inx].Visible := FieldForm.FieldBox.Checked[inx];
   SaveDlg := TSaveDialog.Create(self);
   try
    SaveDlg.DefaultExt := FDefaultExt;
    SaveDlg.Filter     := '微软电子表格(MS-EXCEL文件)|*.XLS';
    SaveDlg.Title      := '保存为';
    SaveDlg.FileName   := FFileName;
    if SaveDlg.Execute then
    begin
     if Assigned(FDataLink.DataSet) then
     begin
      useState := true;
      if FileExists(SaveDlg.FileName) then
         useState := DeleteFile(SaveDlg.FileName);
      if useState then
      begin
       FileStream := TFileStream.Create(SaveDlg.FileName, fmCreate);
       try
        SaveExcel(FileStream);
       Finally
        FileStream.Free;
       end;
      end
      else ShowMessage('文件正在使用中,不能覆盖文件!');
     end;
    end;
   finally
    SaveDlg.Free;
   end;
  end;
 finally
  FieldForm.Free;
  FieldForm := Nil;
 end;
 end;
 emDefault:
 begin
   if Assigned(FDataLink.DataSet) then
     begin
      useState := true;
      tFile    := FFileName;
      if UpperCase(ExtractFileExt(FFileName))<>UpperCase(FDefaultExt) then
         tFile := FFileName + FDefaultExt;
      if FileExists(tFile) then
         useState := DeleteFile(tFile);
      if useState then
      begin
       FileStream := TFileStream.Create(tFile, fmCreate);
       try
        SaveExcel(FileStream);
       Finally
        FileStream.Free;
       end;
      end
      else ShowMessage('文件正在使用中,不能覆盖文件!');
     end;
 end;
end;
end;

procedure TDefineExcel.InitFields;
var
  inx: integer;
  Col: TEduceData;
begin
 if Assigned(FDataLink.DataSet) then
 begin
 with FDataLink.DataSet.FieldDefs do
 begin
  if (not FDataLink.Active) and (Count > 0) then
  begin
    FColumns.BeginUpdate;
    FColumns.Clear;
    for inx:=0 to Count - 1 do
    begin
      Col := FColumns.Add;
      Col.FieldName := Items[inx].Name;
      Col.Caption   := Items[inx].Name;
    end;
    FColumns.EndUpdate;
  end;
 end;
 end;
end;

procedure TDefineExcel.ClearFields;
begin
 FColumns.BeginUpdate;
 FColumns.Clear;
 FColumns.EndUpdate;
end;

procedure TDefineExcel.RestoreFields;
var
 inx : integer;
 col : TEduceData;
begin
 FColumns.BeginUpdate;
 for inx:=0 to FColumns.Count - 1 do
 begin
   Col := FColumns[inx];
   Col.Caption  := Col.FieldName;
   Col.Visible  := True;
 end;
 FColumns.EndUpdate;
end;

procedure TDefineExcel.SetDefaultExt(Value: String);
begin
  if FDefaultExt <> Value then
  begin
     if Value[1] <> '.' then
        Value := '.'+value;
     FDefaultExt := Value;
  end;
end;

{ TEduceLink }

const
  MaxMapSize = (MaxInt div 2) div SizeOf(Integer);

type
  TIntArray = array[0..MaxMapSize] of Integer;
  PIntArray = ^TIntArray;

constructor TEduceLink.Create(ADSExcel: TDefineExcel);
begin
  inherited Create;
  FCells        := ADSExcel;
  VisualControl := True;
end;

destructor TEduceLink.Destroy;
begin
  ClearMapping;
  inherited Destroy;
end;

function TEduceLink.GetDefaultFields: Boolean;
var
  I: Integer;
begin
  Result := True;
  if DataSet <> nil then
     Result := DataSet.DefaultFields;
  if Result and SparseMap then
  for I := 0 to FFieldCount-1 do
    if FFieldMap[I] < 0 then
    begin
      Result := False;
      Exit;
    end;
end;

function TEduceLink.GetFields(I: Integer): TField;
begin
  if (0 <= I) and (I < FFieldCount) and (FFieldMap[I] >= 0) then
    Result := DataSet.FieldList[FFieldMap[I]]
  else
    Result := nil;
end;

function TEduceLink.AddMapping(const FieldName: string): Boolean;
var
  Field: TField;
  NewSize: Integer;
begin
  Result := True;
  if FFieldCount >= MaxMapSize then
     RaiseGridError(STooManyColumns);
  if SparseMap then
     Field := DataSet.FindField(FieldName)
  else
     Field := DataSet.FieldByName(FieldName);

  if FFieldCount = Length(FFieldMap) then
  begin
    NewSize := Length(FFieldMap);
    if NewSize = 0 then
       NewSize := 8
    else
       Inc(NewSize, NewSize);
    if (NewSize < FFieldCount) then
        NewSize := FFieldCount + 1;
    if (NewSize > MaxMapSize) then
        NewSize := MaxMapSize;
    SetLength(FFieldMap, NewSize);
  end;
  if Assigned(Field) then
  begin
    FFieldMap[FFieldCount] := Dataset.FieldList.IndexOfObject(Field);
    Field.FreeNotification(FCells);
  end
  else
    FFieldMap[FFieldCount] := -1;
  Inc(FFieldCount);
end;

procedure TEduceLink.ActiveChanged;
begin
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
         DatabaseError(SDataSetUnidirectional);
  FCells.LinkActive(Active);
  FModified := False;
end;

procedure TEduceLink.ClearMapping;
begin
  FFieldMap   := nil;
  FFieldCount := 0;
end;

procedure TEduceLink.LayoutChanged;
var
  SaveState: Boolean;
begin
  SaveState := FCells.LayoutSet;
  FCells.LayoutSet := True;
  try
    FCells.LayoutChanged;
  finally
    FCells.LayoutSet := SaveState;
  end;
  inherited LayoutChanged;
end;

function TEduceLink.GetMappedIndex(ColIndex: Integer): Integer;
begin
  if (0 <= ColIndex) and (ColIndex < FFieldCount) then
    Result := FFieldMap[ColIndex]
  else
    Result := -1;
end;

function TEduceLink.IsAggRow(Value: Integer): Boolean;
begin
  Result := False;
end;
{ TFlatSound }
const
  Flags = SND_RESOURCE or SND_SYNC;

constructor TFlatSound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Event := seBtnClick;
end;

procedure TFlatSound.Play;
begin
  case FEvent of
    seBtnClick:    PlaySound('ENC_001',0,Flags);
    seMenu:        PlaySound('ENC_002',0,Flags);
    seMenuClick:   PlaySound('ENC_003',0,Flags);
    seMoveIntoBtn: PlaySound('ENC_004',0,Flags);
    sePanelExpand: PlaySound('ENC_005',0,Flags);
  end;
end;

procedure TFlatSound.PlayThis(ThisEvent: TSoundEvent);
begin
  case ThisEvent of
    seBtnClick:    PlaySound('ENC_001',0,Flags);
    seMenu:        PlaySound('ENC_002',0,Flags);
    seMenuClick:   PlaySound('ENC_003',0,Flags);
    seMoveIntoBtn: PlaySound('ENC_004',0,Flags);
    sePanelExpand: PlaySound('ENC_005',0,Flags);
  end;
end;
{ TFlatAnimWnd }
var
  OwnerList: TList;

constructor TFlatAnimHookWnd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimateWindow := TFlatAnimWnd(AOwner);
end;

procedure TFlatAnimHookWnd.WMCreate(var Message: TMessage);
begin
  inherited;
  FAnimateWindow.OwnerWndCreated;
end;

procedure TFlatAnimHookWnd.WMDestroy(var Message: TMessage);
begin
  FAnimateWindow.OwnerWndDestroyed;
  inherited;
end;

constructor TFlatAnimWnd.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  if OwnerList.IndexOf(FOwner) <> -1 then
  begin
    FOwner := nil;
    raise Exception.Create('Owner must be TFORM');
  end;
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    FHookWnd := TFlatAnimHookWnd.Create(Self);
    if Application.MainForm = nil then
    begin
      FNewAppProc := MakeObjectInstance(NewAppWndProc);
      FOldAppProc := Pointer(GetWindowLong(Application.Handle, GWL_WNDPROC));
      SetWindowLong(Application.Handle, GWL_WNDPROC, Longint(FNewAppProc));
    end;
  end;
  OwnerList.Add(FOwner);
end;

destructor TFlatAnimWnd.Destroy;
begin
  if not(csDesigning in ComponentState) then
  begin
    if Application.MainForm = nil then
    begin
      SetWindowLong(Application.Handle, GWL_WNDPROC, Longint(FOldAppProc));
      FreeObjectInstance(FNewAppProc);
    end;
  end;
  if OwnerList.IndexOf(FOwner) <> -1 then
    OwnerList.Remove(FOwner);
  inherited Destroy;
end;

procedure TFlatAnimWnd.SetParentComponent(Value: TComponent);
begin
  inherited SetParentComponent(Value);
  if not(csDesigning in ComponentState) then
    if Value is TWinControl then
      FHookWnd.Parent := TWinControl(Value);
end;

procedure TFlatAnimWnd.OwnerWndCreated;
begin
  FNewProc := MakeObjectInstance(NewWndProc);
  FOldProc := Pointer(GetWindowLong((FOwner as TForm).Handle, GWL_WNDPROC));
  SetWindowLong((FOwner as TForm).Handle, GWL_WNDPROC, Longint(FNewProc));
end;

procedure TFlatAnimWnd.OwnerWndDestroyed;
begin
  SetWindowLong((FOwner as TForm).Handle, GWL_WNDPROC, Longint(FOldProc));
  FreeObjectInstance(FNewProc);
end;

procedure TFlatAnimWnd.NewAppWndProc(var Message: TMessage);
begin
  with Message do
  begin
    if Msg = WM_SYSCOMMAND then
      case WParam of
        SC_MINIMIZE:
          MinimizeWnd;
        SC_RESTORE:
          RestoreWnd;
      end;
    Result := CallWindowProc(FOldAppProc, Application.Handle, Msg, wParam, lParam);
  end;
end;

procedure TFlatAnimWnd.NewWndProc(var Message: TMessage);
begin
  with Message do
  begin
    if (Msg = WM_SYSCOMMAND) and (WParam = SC_MINIMIZE) then
    begin
      if Application.MainForm = FOwner then
        MinimizeWnd
      else
        PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    end
    else
    begin
      if (Msg = WM_WINDOWPOSCHANGING) and (PWindowPos(lParam)^.flags = (SWP_NOSIZE or SWP_NOMOVE)) then
      begin
        if IsIconic(Application.Handle) then
          PostMessage(Application.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
      end
    end;
    Result := CallWindowProc(FOldProc, (FOwner as TForm).Handle, Msg, wParam, lParam);
  end;
end;

procedure TFlatAnimWnd.MinimizeWnd;
var
  Rect: TRect;
begin
  with Application do
  begin
    if not(IsWindowEnabled(Handle)) then
      EnableWindow(Handle, True);
    GetWindowRect((FOwner as TForm).Handle, Rect);
    SetForegroundWindow(Handle);
    SetWindowPos(Handle, 0, Rect.Left, Rect.Top, Rect.Right - Rect.Left, 0, SWP_NOZORDER);
    DefWindowProc(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    ShowWindow(Handle, SW_MINIMIZE);
  end;
  if Assigned(FOnMinimize) then
    FOnMinimize(Application);
end;

procedure TFlatAnimWnd.RestoreWnd;
var
  MainFormPlacement: TWindowPlacement;
  AppWndPlacement: TWindowPlacement;
begin
  with Application do
  begin
    MainFormPlacement.length := SizeOf(TWindowPlacement);
    MainFormPlacement.flags  := 0;
    GetWindowPlacement(MainForm.Handle, @MainFormPlacement);
    AppWndPlacement.length := SizeOf(TWindowPlacement);
    AppWndPlacement.flags  := 0;
    GetWindowPlacement(Handle, @AppWndPlacement);
    AppWndPlacement.rcNormalPosition := MainFormPlacement.rcNormalPosition;
    AppWndPlacement.rcNormalPosition.Bottom := AppWndPlacement.rcNormalPosition.Top;
    SetWindowPlacement(Handle, @AppWndPlacement);
    SetForegroundWindow(Handle);
    DefWindowProc(Application.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
    ShowWindow(Handle, SW_RESTORE);
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER);
    if not(MainForm.Visible) then
    begin
      ShowWindow(MainForm.Handle, SW_RESTORE);
      MainForm.Visible := True;
    end;
  end;
  if Assigned(FOnRestore) then
    FOnRestore(Application);
end;

procedure TFlatAnimWnd.Minimize;
begin
  SendMessage((FOwner as TForm).Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{ TDefineSingle }

constructor TDefineSingle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive  := True;
  FTitle   := 'This program already run!';
  FCaption := '&Exit';
end;

procedure TDefineSingle.Loaded;
begin
  inherited Loaded;
  Run(Active,Title);
end;

procedure TDefineSingle.Run(State: Boolean;Title:String);
begin
  if (State)and(not(csDesigning in ComponentState)) then
  begin
   //with Application do
   //begin
    try
      if OpenMutex(MUTEX_ALL_ACCESS, False, pchar(Application.Title)) = 0 then
      begin
        inherited;
        ReleaseMutex(CreateMutex(nil, False, pchar(Application.Title)));
      end else begin
        Application.ShowMainForm := False;
        ShowDialog(Title, Caption);
        Application.Terminate;
      end;
    finally
    end;
   //end;
  end;
end;

procedure TDefineSingle.SetActive(Value: boolean);
begin
  if FActive <> Value then begin
     FActive := Value;
     Run(FActive,FTitle);
  end;
end;

procedure TDefineSingle.SetTitle(const Value: string);
begin
  if FTitle <> Value then FTitle := Value;
end;

procedure TDefineSingle.SetCaption(const Value: string);
begin
  if FCaption <> Value then FCaption := Value;
end;

{ TDefineTimer }
procedure TimerCallback(uTimerID, uMessage: Cardinal; dwUser, dw1, dw2: DWORD_PTR); stdcall;
var
  FlatTimer: TDefineTimer;
begin
  FlatTimer := TDefineTimer(dwUser);
  if Assigned(FlatTimer) then FlatTimer.Timer;
end;

constructor TDefineTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled  := True;
  FInterval := 1000;
  FPeriod   := 10;
  uTimerID  := 0;
end;

destructor TDefineTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  inherited Destroy;
end;

procedure TDefineTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
     FEnabled := Value;
     UpdateTimer;
  end;
end;

procedure TDefineTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then begin
     FInterval := Value;
     UpdateTimer;
  end;
end;

procedure TDefineTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TDefineTimer.SetPeriod(Value: Cardinal);
var Caps: TTimeCaps;
begin
  if (Value <> FPeriod) and (timeGetDevCaps(@Caps, Sizeof(TTimeCaps)) <> 0) then
  begin
    if Value < Caps.wPeriodMin then //小于最小分辨率
       Value := 0
    else if Value > Caps.wPeriodMax then //大于最小分辨率
       Value := Caps.wPeriodMax;
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TDefineTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(self);
end;

procedure TDefineTimer.UpdateTimer;
var lpProc: TFNTimeCallBack;
begin
  if uTimerID <> 0 then timeKillEvent(uTimerID); //销毁
  if (FInterval > 0) and FEnabled and Assigned(FOnTimer) then
  begin
    lpProc   := TimerCallback;
    uTimerID := TimeSetEvent(FInterval,FPeriod,lpProc,DWORD(Self),TIME_PERIODIC);
    if uTimerID = 0 then begin
       FEnabled := FALSE;
       raise Exception.Create('Failed to create Timer!');
    end;
  end;
end;

{ TDefineTaskbarIcon }

const WM_TASKICON = WM_USER;

constructor TDefineTaskbarIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := AllocateHWnd(PrivateWndProc);
  FWMTaskBarCreated := RegisterWindowMessage('TaskbarCreated');
  Application.HookMainWindow(AppHook);
  FIcon     := TIcon.Create;
  FHintMode := thDefault;
  FIconMode := thDefault;
end;

destructor TDefineTaskbarIcon.Destroy;
begin
  if FActive then SetActive(False);
  Application.UnhookMainWindow(AppHook);
  FIcon.Free;
  if FHandle <> 0 then DeallocateHwnd(FHandle);
  inherited Destroy;
end;

procedure TDefineTaskbarIcon.PrivateWndProc(var Message: TMessage);
begin
  WndProc(Message);
end;

procedure TDefineTaskbarIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PopupMenu) then
      PopupMenu := nil;
end;

procedure TDefineTaskbarIcon.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if Value then
       AddIcon
    else
       DeleteIcon;
  end;
end;

procedure TDefineTaskbarIcon.SetHint(Value: string);
begin
  FHint := Value;
  ModifyIcon(NIF_TIP);
end;

procedure TDefineTaskbarIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  ModifyIcon(NIF_ICON);
end;

function TDefineTaskbarIcon.DoIcon(Action: DWORD; Aspect: UINT): Boolean;
var
  Data: TNotifyIconData; SIZEOFDATA,SIZEOFSZTIP:INTEGER;
begin
  SIZEOFDATA:=SizeOf(Data);
  SIZEOFSZTIP := SizeOf(DATA.szTip);
  with Data do
  begin
    cbSize := SIZEOFDATA;
    wnd    := FHandle;
    uID    := 0;
    uFlags := Aspect or NIF_MESSAGE;
    uCallbackMessage := WM_TASKICON;
    if Aspect and NIF_ICON <> 0 then
     case FIconMode of
      thCustom:
      if FIcon.Handle <> 0 then
         hIcon := FIcon.Handle
      else
         hIcon := LoadIcon(0, IDI_WINLOGO);
      thDefault:
         hIcon := Application.Icon.Handle;
     end;
    if Aspect and NIF_TIP <> 0 then
       Case FHintMode of
        thDefault: StrPLCopy(szTip, PChar(Application.Title), SIZEOFSZTIP);
        thCustom : StrLCopy(szTip, PChar(FHint), SIZEOFSZTIP);
       end;
  end;
  if not (csDesigning in ComponentState) then begin
     Result := Shell_NotifyIcon(Action, @Data);
  end else
     Result := False;
end;

function TDefineTaskbarIcon.AddIcon: Boolean;
begin
  Result := DoIcon(NIM_ADD, NIF_TIP or NIF_ICON);
end;

function TDefineTaskbarIcon.ModifyIcon(Aspect: Integer): Boolean;
begin
  if FActive then
     Result := DoIcon(NIM_MODIFY, Aspect)
  else
     Result := False;
end;

function TDefineTaskbarIcon.DeleteIcon: Boolean;
begin
  Result := DoIcon(NIM_DELETE, 0);
end;

procedure TDefineTaskbarIcon.WndProc(var Message: TMessage);
var Pt: TPoint;
begin
  with Message do
  begin
    if Msg = WM_TASKICON then
    case LParam of
      WM_LBUTTONUP:
        if Assigned(FOnClick) then FOnClick(Self);
      WM_LBUTTONDBLCLK:
        if Assigned(FOnDblClick) then FOnDblClick(Self);
      WM_RBUTTONUP:
        if Assigned(FOnRightClick) then
           FOnRightClick(Self)
        else if Assigned(FPopupMenu) then begin 
           SetForegroundWindow(FHandle);
           GetCursorPos(Pt);
           FPopupMenu.Popup(Pt.X, Pt.Y);
           PostMessage(FHandle, WM_USER, 0, 0);
        end;
      WM_MOUSEMOVE:
        if Assigned(FOnMouseMove) then FOnMouseMove(Self);
    end
    else
      Result := DefWindowProc(Handle, Msg, WParam, LParam);
  end;
end;

function TDefineTaskbarIcon.AppHook(var Message: TMessage): Boolean;
begin
  Result := Message.Msg = FWMTaskbarCreated;
  if Result then AddIcon;
end;


procedure TDefineTaskbarIcon.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TDefineTaskbarIcon.SetHintMode(const Value: TDefineTaskbarMode);
begin
  if FHintMode <> Value then
  begin
     FHintMode := Value;
     ModifyIcon(NIF_TIP);
  end;
end;

procedure TDefineTaskbarIcon.SetIconMode(const Value: TDefineTaskbarMode);
begin
  if FIconMode <> Value then
  begin
     FIconMode := Value;
     ModifyIcon(NIF_ICON);
  end;
end;

procedure TDefineTaskbarIcon.Loaded;
begin
  inherited Loaded;
  if FActive then AddIcon;
end;

{ TDefineAnimation }
constructor TDefineAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimation   := TBitmap.Create;
  ControlStyle := ControlStyle + [csOpaque];
  SetBounds(0, 0, 60, 60);
  FTransColor  := clFuchsia;
  FBorderColor := DefaultBorderColor;
  FBorder      := false;
  FTransparent := false;
  FActive      := False;
  FLoop        := True;
  FInterval    := 100; // 1 Second
  FFrameWidth  := 30;
  FFrames      := 1;
  FFrame       := 0;
end;

destructor TDefineAnimation.Destroy;
begin
  FAnimation.Free;       
  inherited Destroy;
end;

procedure TDefineAnimation.Paint;
var
  X, Y, Pos, W, H: Integer;
  SrcRect, DestRect: TRect;
  memGlyph: TBitmap;
begin
 W := FAnimation.Width div FFrames;
 H := FAnimation.Height div FFrames;
 case FAnimationLayout of
    alAcross:
      begin
        X   := (Width - W) div 2;
        Y   := (Height - FAnimation.Height) div 2;
        Pos := W * FFrame;
        DestRect := Rect(X, Y, X + W, Y + FAnimation.Height);
        SrcRect  := Rect(Pos, 0, Pos + W, FAnimation.Height);
      end;
    alDown:
      begin
        X   := (Width - FFrameWidth) div 2;
        Y   := (Height - H) div 2;
        Pos := H * FFrame;
        DestRect := Rect(X, Y, X + FFrameWidth, Y + H);
        SrcRect  := Rect(0, Pos, FFrameWidth, Pos + FFrameWidth);
      end;
 end;
 memGlyph := TBitmap.Create;
 try
  memGlyph.Height := Height;
  memGlyph.Width  := Width;
  with memGlyph.Canvas do
  begin
   Brush.Style := bsClear;
   Brush.Color := Color;
   FillRect(ClipRect);
   if FTransparent then begin
      DrawParentImage(self, memGlyph.Canvas);
      Brush.Style := bsClear;
      Brush.Color := FTransColor; 
      BrushCopy(DestRect, FAnimation, SrcRect, FTransColor);
   end else begin
      CopyRect(DestRect, FAnimation.Canvas, SrcRect);
   end;
   if (csDesigning in ComponentState) and (not FBorder) then
   begin
      Pen.Style   := psDot;
      Pen.Color   := clBlack;
      Brush.Style := bsClear;
      Rectangle(ClipRect);
   end else if FBorder then begin
      DrawButtonBorder(memGlyph.Canvas, ClipRect, FBorderColor, 1);
   end;
  end;
  Canvas.CopyRect(ClientRect, memGlyph.Canvas, ClientRect);
 finally
  memGlyph.Free;
 end;
end;

procedure TDefineAnimation.SetAnimation(Value: TBitmap);
begin
  if Value <> FAnimation then
  begin
    FAnimation.Assign(Value);
    if not FAnimation.Empty then
    begin
      if FAnimation.Width > FAnimation.Height then
         FAnimationLayout := alAcross
      else
         FAnimationLayout := alDown;
      case FAnimationLayout of
        alAcross:
          if FAnimation.Width mod FAnimation.Height = 0 then
            FFrames := FAnimation.Width div FAnimation.Height;
        alDown:
          if FAnimation.Height mod FAnimation.Width = 0 then
            FFrames := FAnimation.Height div FAnimation.Width;
      end;
      FFrame := 1;
      case FAnimationLayout of
        alAcross:
          FFrameWidth := FAnimation.Width div FFrames;
        alDown:
          FFrameWidth := FAnimation.Height div FFrames;
      end;
      FTransColor := FAnimation.Canvas.Pixels[0, FAnimation.Height - 1];
    end;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetFrames(Value: Integer);
begin
  if Value <> FFrames then
  begin
    FFrames := Value;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetFrameWidth(Value: Integer);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetFrame(Value: Integer);
var
  Temp: Integer;
begin
  if Value < 0 then
     Temp := FFrames - 1
  else
     Temp := Value mod FFrames;
  if Temp <> FFrame then
  begin
    FFrame := Temp;
    if Assigned(FFrameChange) then
    begin
      FFrameChange(Self,FFrame);
    end;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if not Value then
    begin
      FTimer.Free;
      FTimer := nil;
    end
    else
      if FInterval > 0 then
      begin
        FTimer := TTimer.Create(Self);
        FTimer.Interval := FInterval;
        FTimer.OnTimer := DoTimer;
      end;
  end;
end;

procedure TDefineAnimation.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
     FTransparent := Value;
     Invalidate;
  end;
end;

procedure TDefineAnimation.SetLoop(Value: Boolean);
begin
  if Value <> FLoop then
  begin
    FLoop := Value;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetReverse(Value: Boolean);
begin
  if Value <> FReverse then
  begin
     FReverse := Value;
     Invalidate;
  end;
end;

procedure TDefineAnimation.SetInterval(Value: Integer);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    if FActive then
       FTimer.Interval := Value;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetBorder(Value: Boolean);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TDefineAnimation.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FTransColor := Value;
    1: FBorderColor := Value;
  end;
  Invalidate;
end;

procedure TDefineAnimation.CMSysColorChange (var Message: TMessage);
begin
  inherited;
  if (ParentColor)and(Parent<>nil) then
  begin
    ParentColor := True;
    Color := TForm(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineAnimation.CMParentColorChanged (var Message: TWMNoParams);
begin
  inherited;
  if (ParentColor)and(Parent<>nil) then
  begin
    ParentColor := True;
    Color := TForm(Parent).Color;
  end;
  Invalidate;
end;

procedure TDefineAnimation.WMSize (var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure TDefineAnimation.DoTimer(Sender: TObject);
  procedure ChkStop;
  begin
    if not FLoop then
    begin
      FActive := False;
      FTimer.Free;
      FTimer := nil;
    end;
  end;
begin
  if FReverse then
  begin
    Frame := Frame - 1;
    if FFrame = 0 then ChkStop;
  end
  else
  begin
    Frame := Frame + 1;
    if FFrame = Frames - 1 then ChkStop;
  end;
end;

procedure TDefineAnimation.SetAnimationLayout(const Value: TAnimationLayout);
begin
  FAnimationLayout := Value;
  Invalidate;
end;

{ TDefineHint }
constructor TDefineHint.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintFont := TFont.Create;
  if not (csDesigning in ComponentState) then
  begin
    HintWindowClass := TDefineHintWindow;
    with Application do
    begin
      ShowHint   := not ShowHint;
      ShowHint   := not ShowHint;
      OnShowHint := GetHintInfo;
      HintShortPause := 25;
      HintPause      := 500;
      HintHidePause  := 5000;
    end;
  end;
  FBackgroundColor      := clWhite;
  FBorderColor          := clBlack;
  FArrowBackgroundColor := $0053D2FF;
  FArrowColor           := clBlack;
  FHintWidth            := 200;
end;

destructor TDefineHint.Destroy;
begin
  FHintFont.Free;
  inherited Destroy;
end;

procedure TDefineHint.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FBackgroundColor      := Value;
    1: FBorderColor          := Value;
    2: FArrowBackgroundColor := Value;
    3: FArrowColor           := Value;
  end;
end;

procedure TDefineHint.SetHintFont (Value: TFont);
begin
  FHintFont.Assign(Value);
end;

procedure TDefineHint.GetHintInfo (var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if Assigned(FOnShowHint) then
     FOnShowHint(HintStr, CanShow, HintInfo);
end;

{ TDefineHintWindow }

function TDefineHintWindow.FindFlatHint: TDefineHint;
var
  curInx: Integer;
begin
  Result := nil;

  with Application.MainForm do
    for curInx := 0 to ComponentCount - 1 do
      if Components[curInx] is TDefineHint then
      begin
        Result := TDefineHint(Components[curInx]);
        Break;
      end;
end;

procedure TDefineHintWindow.CreateParams (var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style - WS_BORDER;
end;

procedure TDefineHintWindow.Paint;
var
  ArrowRect, TextRect: TRect;
begin
  // Set the Rect's
  case FArrowPos of
    NW, SW:
      begin
        ArrowRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Left + 15, ClientRect.Bottom - 1);
        TextRect  := Rect(ClientRect.Left + 15, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
      end;
    NE, SE:
      begin
        ArrowRect := Rect(ClientRect.Right - 15, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
        TextRect  := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 15, ClientRect.Bottom - 1);
      end;
  end;

  // DrawBackground
  canvas.brush.color := FHint.FArrowBackgroundColor;
  canvas.FillRect(ArrowRect);
  canvas.brush.color := FHint.FBackgroundColor;
  canvas.FillRect(TextRect);

  // DrawBorder
  canvas.Brush.Color := FHint.FBorderColor;
  canvas.FrameRect(ClientRect);

  // DrawArrow
  case FArrowPos of
    NW: FArrowPoint := Point(ArrowRect.Left + 2, ArrowRect.Top + 2);
    NE: FArrowPoint := Point(ArrowRect.Right - 3, ArrowRect.Top + 2);
    SW: FArrowPoint := Point(ArrowRect.Left + 2, ArrowRect.Bottom - 3);
    SE: FArrowPoint := Point(ArrowRect.Right - 3, ArrowRect.Bottom - 3);
  end;
  canvas.Pen.Color := FHint.FArrowColor;
  case FArrowPos of
    NW: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y + 6),
                         Point(FArrowPoint.x + 1, FArrowPoint.y + 6), Point(FArrowPoint.x + 1, FArrowPoint.y),
                         Point(FArrowPoint.x + 6, FArrowPoint.y),     Point(FArrowPoint.x + 6, FArrowPoint.y + 1),
                         Point(FArrowPoint.x + 2, FArrowPoint.y + 1), Point(FArrowPoint.x + 2, FArrowPoint.y + 4),
                         Point(FArrowPoint.x + 5, FArrowPoint.y + 7), Point(FArrowPoint.x + 6, FArrowPoint.y + 7),
                         Point(FArrowPoint.x + 3, FArrowPoint.y + 4), Point(FArrowPoint.x + 3, FArrowPoint.y + 3),
                         Point(FArrowPoint.x + 6, FArrowPoint.y + 6), Point(FArrowPoint.x + 7, FArrowPoint.y + 6),
                         Point(FArrowPoint.x + 3, FArrowPoint.y + 2), Point(FArrowPoint.x + 4, FArrowPoint.y + 2),
                         Point(FArrowPoint.x + 7, FArrowPoint.y + 5), Point(FArrowPoint.x + 7, FArrowPoint.y + 6)]);
    NE: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y + 6),
                         Point(FArrowPoint.x - 1, FArrowPoint.y + 6), Point(FArrowPoint.x - 1, FArrowPoint.y),
                         Point(FArrowPoint.x - 6, FArrowPoint.y),     Point(FArrowPoint.x - 6, FArrowPoint.y + 1),
                         Point(FArrowPoint.x - 2, FArrowPoint.y + 1), Point(FArrowPoint.x - 2, FArrowPoint.y + 4),
                         Point(FArrowPoint.x - 5, FArrowPoint.y + 7), Point(FArrowPoint.x - 6, FArrowPoint.y + 7),
                         Point(FArrowPoint.x - 3, FArrowPoint.y + 4), Point(FArrowPoint.x - 3, FArrowPoint.y + 3),
                         Point(FArrowPoint.x - 6, FArrowPoint.y + 6), Point(FArrowPoint.x - 7, FArrowPoint.y + 6),
                         Point(FArrowPoint.x - 3, FArrowPoint.y + 2), Point(FArrowPoint.x - 4, FArrowPoint.y + 2),
                         Point(FArrowPoint.x - 7, FArrowPoint.y + 5), Point(FArrowPoint.x - 7, FArrowPoint.y + 6)]);
    SW: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y - 6),
                         Point(FArrowPoint.x + 1, FArrowPoint.y - 6), Point(FArrowPoint.x + 1, FArrowPoint.y),
                         Point(FArrowPoint.x + 6, FArrowPoint.y),     Point(FArrowPoint.x + 6, FArrowPoint.y - 1),
                         Point(FArrowPoint.x + 2, FArrowPoint.y - 1), Point(FArrowPoint.x + 2, FArrowPoint.y - 4),
                         Point(FArrowPoint.x + 5, FArrowPoint.y - 7), Point(FArrowPoint.x + 6, FArrowPoint.y - 7),
                         Point(FArrowPoint.x + 3, FArrowPoint.y - 4), Point(FArrowPoint.x + 3, FArrowPoint.y - 3),
                         Point(FArrowPoint.x + 6, FArrowPoint.y - 6), Point(FArrowPoint.x + 7, FArrowPoint.y - 6),
                         Point(FArrowPoint.x + 3, FArrowPoint.y - 2), Point(FArrowPoint.x + 4, FArrowPoint.y - 2),
                         Point(FArrowPoint.x + 7, FArrowPoint.y - 5), Point(FArrowPoint.x + 7, FArrowPoint.y - 6)]);
    SE: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y - 6),
                         Point(FArrowPoint.x - 1, FArrowPoint.y - 6), Point(FArrowPoint.x - 1, FArrowPoint.y),
                         Point(FArrowPoint.x - 6, FArrowPoint.y),     Point(FArrowPoint.x - 6, FArrowPoint.y - 1),
                         Point(FArrowPoint.x - 2, FArrowPoint.y - 1), Point(FArrowPoint.x - 2, FArrowPoint.y - 4),
                         Point(FArrowPoint.x - 5, FArrowPoint.y - 7), Point(FArrowPoint.x - 6, FArrowPoint.y - 7),
                         Point(FArrowPoint.x - 3, FArrowPoint.y - 4), Point(FArrowPoint.x - 3, FArrowPoint.y - 3),
                         Point(FArrowPoint.x - 6, FArrowPoint.y - 6), Point(FArrowPoint.x - 7, FArrowPoint.y - 6),
                         Point(FArrowPoint.x - 3, FArrowPoint.y - 2), Point(FArrowPoint.x - 4, FArrowPoint.y - 2),
                         Point(FArrowPoint.x - 7, FArrowPoint.y - 5), Point(FArrowPoint.x - 7, FArrowPoint.y - 6)]);
  end;

  // DrawHintText
  canvas.brush.Style := bsClear;
  InflateRect(TextRect, -3, -1);
  {$IFDEF DFS_COMPILER_4_UP}
  if BidiMode = bdRightToLeft then
    DrawText(canvas.handle, PChar(Caption), Length(Caption), TextRect, DT_RIGHT or DT_WORDBREAK or DT_NOPREFIX)
  else
    DrawText(canvas.handle, PChar(Caption), Length(Caption), TextRect, DT_WORDBREAK or DT_NOPREFIX);
  {$ELSE}
  DrawText(canvas.handle, PChar(Caption), Length(Caption), TextRect, DT_WORDBREAK or DT_NOPREFIX);
  {$ENDIF}
end;

procedure TDefineHintWindow.ActivateHint (HintRect: TRect; const AHint: string);
var
  curWidth: Byte;
  Pnt: TPoint;
  HintHeight, HintWidth: Integer;
  NordWest, NordEast, SouthWest, SouthEast: TRect;
begin
  Caption := AHint;
  FHint   := FindFlatHint;

  if FHint <> nil then
    Canvas.Font.Assign(FHint.Font);

  // Calculate width and height
  HintRect.Right := HintRect.Left + FHint.MaxWidth - 22;

  {$IFDEF DFS_COMPILER_4_UP}
  if BidiMode = bdRightToLeft then
    DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_RIGHT or DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX)
  else
    DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
  {$ELSE}
  DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
  {$ENDIF}


  DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
  Inc(HintRect.Right, 22);
  Inc(HintRect.Bottom, 6);

  // Divide the screen in 4 pices
  NordWest :=  Rect(0, 0, Screen.Width div 2, Screen.Height div 2);
  NordEast :=  Rect(Screen.Width div 2, 0, Screen.Width, Screen.Height div 2);
  SouthWest := Rect(0, Screen.Height div 2, Screen.Width div 2, Screen.Height);
  SouthEast := Rect(Screen.Width div 2, Screen.Height div 2, Screen.Width, Screen.Height);

  GetCursorPos(Pnt);

  if PtInRect(NordWest, Pnt) then
    FArrowPos := NW
  else
    if PtInRect(NordEast, Pnt) then
      FArrowPos := NE
    else
      if PtInRect(SouthWest, Pnt) then
        FArrowPos := SW
      else
        FArrowPos := SE;

  // Calculate the position of the hint
  if FArrowPos = NW then
    curWidth := 12
  else
    curWidth := 5;

  HintHeight := HintRect.Bottom - HintRect.Top;
  HintWidth  := HintRect.Right - HintRect.Left;

  case FArrowPos of
    NW: HintRect := Rect(Pnt.x + curWidth, Pnt.y + curWidth, Pnt.x + HintWidth + curWidth, Pnt.y + HintHeight + curWidth);
    NE: HintRect := Rect(Pnt.x - HintWidth - curWidth, Pnt.y + curWidth, Pnt.x - curWidth, Pnt.y + HintHeight + curWidth);
    SW: HintRect := Rect(Pnt.x + curWidth, Pnt.y - HintHeight - curWidth, Pnt.x + HintWidth + curWidth, Pnt.y - curWidth);
    SE: HintRect := Rect(Pnt.x - HintWidth - curWidth, Pnt.y - HintHeight - curWidth, Pnt.x - curWidth, Pnt.y - curWidth);
  end;

  BoundsRect := HintRect;

  Pnt := ClientToScreen(Point(0, 0));

  SetWindowPos(Handle, HWND_TOPMOST, Pnt.X, Pnt.Y, 0, 0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
end;

{ TBaseWater }

constructor TBaseWater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 50;
{$IFDEF MSWINDOWS}
  FHandle := Classes.AllocateHWnd(WndProc);
{$ENDIF}
{$IFDEF LINUX}
  FHandle := WinUtils.AllocateHWnd(WndProc);
{$ENDIF}
end;

destructor TBaseWater.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
{$IFDEF MSWINDOWS}
  Classes.DeallocateHWnd(FHandle);
{$ENDIF}
{$IFDEF LINUX}
  WinUtils.DeallocateHWnd(FHandle);
{$ENDIF}
  inherited Destroy;
end;

procedure TBaseWater.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        Timer;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TBaseWater.UpdateTimer;
begin
  KillTimer(FHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
end;

procedure TBaseWater.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TBaseWater.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TBaseWater.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TBaseWater.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

{ TDefineWater }
const
  RAND_MAX = $7FFF;

constructor TDefineWater.Create(AOwner: TComponent);
begin
  FBitmap    := TBitmap.Create;
  FWater     := TDefineWatet.Create;
  FItems     := TStringList.Create;
  inherited Create(AOwner);
  FDamping   := csDefDamping;
  FPlayState := true;
  OnTimer    := Play;
end;

destructor TDefineWater.Destroy;
begin
  FItems.Free;
  FWater.Free;
  FBitmap.Free;
  inherited Destroy;
end;

procedure TDefineWater.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FWater.Blob(x,y,1,5000);
end;

procedure TDefineWater.SetDamping(Value: TWaterDamping);
begin
  if (Value >= Low(TWaterDamping)) and (Value <= High(TWaterDamping)) then
  begin
    FDamping       := Value;
    FWater.Damping := Value;
  end;
end;

procedure TDefineWater.InitiateWater;
var inx:Integer;
    TitleValue:String;
begin
  FBitmap.Assign(FImage.Bitmap);
  FImage.OnMouseMove := OnMouseMove;
  with FImage do
  begin
   Bitmap.FreeImage;
   Bitmap.Width  := FBitmap.Width;
   Bitmap.Height := FBitmap.Height;
  end;
  FWater.SetSize(FBitmap);
  FState      := FBitmap.Height;
  FPlayState  := false;
  FMoveHeight := 10;
  for inx := 0 to FItems.Count - 1 do
  begin
      TitleValue := FItems.Strings[inx];
      GetTitleParam(FParam, TitleValue);
      with FImage.Canvas do
      begin
       Font.Name   := FParam.Name;
       Font.Size   := FParam.Size;
       Font.Style  := FParam.Style;
       Font.Pitch  := FParam.Pitch;
       FMoveHeight := FMoveHeight + TextHeight(TitleValue)+FParam.Row;
      end;
  end;
  TextHht := FMoveHeight+20;
  if FMoveHeight < FImage.Height then
     FMoveHeight := FImage.Height+10;
end;

procedure TDefineWater.Play;
var
  TitleValue:String;
  Inx,Cur: Integer;
begin
  if (FImage <> nil)and(not(csDesigning in ComponentState)) then
  begin
  if FPlayState then
     InitiateWater;
  if Random(8)= 1 then
     FWater.Blob(-1,-1,Random(1)+1,Random(500)+50);
  FWater.Render(Bitmap,FImage.Bitmap);
  FState:=FState-1;
  if (FState<-FMoveHeight)or(FState < -TextHht) then
     FState:=FImage.height+20;
  with FImage.Canvas do
    begin
      Brush.Style:=bsClear;
      Cur := FState;
      for inx:=0 to FItems.Count - 1 do
      begin
       TitleValue := FItems.Strings[inx];
       GetTitleParam(FParam, TitleValue);
       Font.Name  := FParam.Name;
       Font.Size  := FParam.Size;
       Font.Style := FParam.Style;
       Font.Pitch := FParam.Pitch;
       if FParam.Draw3D then
       begin
          Font.Color := 0;
          case FParam.Align of
           wpLeft  :TextOut(21,Cur,TitleValue);
           wpCenter:TextOut((FImage.Width - TextWidth(TitleValue))div 2+1,Cur,TitleValue);
           wpRight :TextOut((FImage.Width - TextWidth(TitleValue))-21,Cur,TitleValue);
          end;
       end;
       Font.Color := FParam.Color;
       case FParam.Align of
        wpLeft  :TextOut(20,Cur,TitleValue);
        wpCenter:TextOut((FImage.Width - TextWidth(TitleValue))div 2,Cur,TitleValue);
        wpRight :TextOut((FImage.Width - TextWidth(TitleValue))-20,Cur,TitleValue);
       end;
       Cur := Cur+TextHeight('H')+FParam.Row;
      end;
      if FItems.Count <= 0 then
      begin
         TextOut((FImage.Width - TextWidth(''))div 2,Cur,'');
      end;
    end;
  end;
end;

procedure TDefineWater.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TDefineWater.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent <> nil) then
  begin
    if AComponent=FImage then CtrlImage:=nil;
  end;
end;

{ TDefineImage }

constructor TDefineImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle       := ControlStyle + [csReplicatable];
  FEnterImage        := TBitmap.Create;
  FLeaveImage        := TBitmap.Create;
  FMouseState        := True;
  FAutoImage         := False;
  FAutoCursor        := crHandPoint;
  FAutoShowCursor    := false;
  FBitmap            := TBitmap.Create;
  FBitmap.OnChange   := PictureChanged;
  FBitmap.OnProgress := Progress;
  Height := 105;
  Width := 105;
end;

destructor TDefineImage.Destroy;
begin
  FEnterImage.Free;
  FLeaveImage.Free;
  FBitmap.Free;
  inherited Destroy;
end;

function TDefineImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FBitmap <> nil then
	Result := FBitmap.Palette;
end;

function TDefineImage.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w  := Bitmap.Width;
  h  := Bitmap.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
	if Proportional and (w > 0) and (h > 0) then
	begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if Center then
	OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

procedure TDefineImage.Paint;
var
  Save: Boolean;
begin
  if csDesigning in ComponentState then
	with inherited Canvas do
	begin
	  Pen.Style   := psDash;
	  Brush.Style := bsClear;
	  Rectangle(0, 0, Width, Height);
	end;
  Save     := FDrawing;
  FDrawing := True;
  try
	with inherited Canvas do
  begin
     StretchDraw(DestRect, Bitmap);
  end;
  finally
	FDrawing := Save;
  end;
end;

function TDefineImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp    := Bitmap;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
	(Tmp.PaletteModified) then
  begin
	if (Tmp.Palette = 0) then
	  Tmp.PaletteModified := False
	else
	begin
	  ParentForm := GetParentForm(Self);
	  if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
	  begin
		if FDrawing then
		  ParentForm.Perform(wm_QueryNewPalette, 0, 0)
		else
		  PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
		Result := True;
		Tmp.PaletteModified := False;
	  end;
	end;
  end;
end;

procedure TDefineImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
	if DoPaletteChange then Update
	else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TDefineImage.GetCanvas: TCanvas;
var
  fBit: TBitmap;
begin
  if Bitmap = nil then
  begin
	fBit := TBitmap.Create;
	try
	  fBit.Width    := Width;
	  fBit.Height   := Height;
	  fBit          := Bitmap;
	finally
	  fBit.Free;
	end;
  end;
  if Bitmap is TBitmap then
	   Result := TBitmap(Bitmap).Canvas
  else
	raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

procedure TDefineImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
	FCenter := Value;
	PictureChanged(Self);
  end;
end;

procedure TDefineImage.SetPicture(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TDefineImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
	FStretch := Value;
	PictureChanged(Self);
  end;
end;

procedure TDefineImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
	 FTransparent := Value;
	 PictureChanged(Self);
  end;
end;

procedure TDefineImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
	FProportional := Value;
	PictureChanged(Self);
  end;
end;

procedure TDefineImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if AutoSize and (Bitmap.Width > 0) and (Bitmap.Height > 0) then
	SetBounds(Left, Top, Bitmap.Width, Bitmap.Height);
  G := Bitmap;
  if G <> nil then
  begin
	if not ((G is TMetaFile) or (G is TIcon)) then
	  G.Transparent := FTransparent;
  D := DestRect;
	if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
	   (D.Right >= Width) and (D.Bottom >= Height) then
	  ControlStyle := ControlStyle + [csOpaque]
	else  // picture might not cover entire clientrect
	  ControlStyle := ControlStyle - [csOpaque];
	if DoPaletteChange and FDrawing then Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;
end;

function TDefineImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Bitmap.Width > 0) and
    (Bitmap.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
       NewWidth := Bitmap.Width;
    if Align in [alNone, alTop, alBottom] then
       NewHeight := Bitmap.Height;
  end;
end;

procedure TDefineImage.MouseEnter(var Msg: TMessage);
begin
if not(csDesigning in ComponentState) then
begin
 if FAutoImage and FMouseState Then
  begin
    Bitmap.Assign(FEnterImage);
    FMouseState := False;
  end;
 If FAutoShowCursor Then
    Cursor := FAutoCursor;
end;
end;

procedure TDefineImage.MouseLeave(var Msg: TMessage);
begin
if not(csDesigning in ComponentState) then
begin
 if FAutoImage and not FMouseState Then
  begin
    Bitmap.Assign(FLeaveImage);
    FMouseState := True;
  end;
end;
end;

procedure TDefineImage.SetEnterImage(const Value: TBitmap);
begin
  FEnterImage.Assign(Value);
end;

procedure TDefineImage.SetLeaveImage(const Value: TBitmap);
begin
  FLeaveImage.Assign(Value);
end;

initialization
  OwnerList := TList.Create;

finalization
  OwnerList.Free;


end.
