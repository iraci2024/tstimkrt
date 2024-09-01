{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressCommonLibrary }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL }
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

unit cxLookAndFeels;

{$I cxVer.inc}

interface

uses
  Types, Messages, SysUtils, Controls, Forms, Classes, Graphics,
  dxCore, dxCoreClasses, dxThemeManager, cxClasses, dxOffice11, cxGeometry,
  cxLookAndFeelPainters;

type
  TcxLookAndFeelKind = (lfFlat, lfStandard, lfUltraFlat, lfOffice11);
  TcxLookAndFeelValue = (lfvKind, lfvNativeStyle, lfvSkinName, lfvScrollbarMode,
    lfvRenderMode, lfvScrollMode);
  TcxLookAndFeelValues = set of TcxLookAndFeelValue;
  TdxScrollbarMode = (sbmDefault, sbmTouch, sbmClassic, sbmHybrid);
  TdxScrollMode = (scmDefault, scmClassic, scmSmooth);
  TdxRenderMode = (rmDefault, rmGDI, rmDirectX);

const
  cxDefaultIsTouchModeEnabled = False;
  cxDefaultIsTouchScrollUIModeEnabled =
  {$IFDEF USETOUCHSCROLLUIMODEASDEFAULT}True{$ELSE}False{$ENDIF};
  cxDefaultLookAndFeelKind = lfUltraFlat;
  cxDefaultLookAndFeelNativeStyle =
  {$IFDEF USENATIVELOOKANDFEELASDEFAULT}True{$ELSE}False{$ENDIF};
  cxDefaultLookAndFeelRenderMode = rmGDI;
  cxDefaultLookAndFeelScrollMode = scmClassic;
  cxDefaultLookAndFeelSkinName = '';
  cxDefaultUseSkins = True;

  cxIsTouchModeEnabled: Boolean = cxDefaultIsTouchModeEnabled;
  cxUseSkins: Boolean = cxDefaultUseSkins;

type
  TcxLookAndFeel = class;
  TcxSystemPaletteChangedNotifier = class;

  IcxLookAndFeelNotificationListener = interface
    ['{205538BF-F19E-4285-B11F-B182D9635881}']
    function GetObject: TObject;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues);
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
  end;

  IcxLookAndFeelNotificationListener2 = interface
    ['{392FC2E2-3F2A-4885-B9E6-43982962F475}']
    procedure MasterLookAndFeelBeginChange;
    procedure MasterLookAndFeelEndChange;
  end;

  { IdxCustomSkinnedContainer }

  IdxCustomSkinnedContainer = interface
    ['{8A0FF5C0-C25A-4191-AE0F-B231D2651F2F}']
    function GetDefaultTextColor(AEnabled: Boolean): TColor;
  end;

  { IdxSkinSupport }

  IdxSkinSupport = interface
    ['{EF3FF483-9B69-46DF-95A4-D3A3810F63A5}']
  end;

  { IdxSkinSupport2 }

  IdxSkinSupport2 = interface(IdxSkinSupport)
    ['{01C435BA-7DA1-44A4-BE8B-BCE9CE0562AB}']
    function IsSkinnable: Boolean;
  end;

  { IdxTouchModeSupport }

  IdxTouchModeSupport = interface
    ['{9107444B-867E-44C5-BF9A-DAB4EE879827}']
    procedure Disable;
    procedure Enable;
  end;

  IdxVisualRefinementsListener = interface
    ['{287BA6FB-1282-4933-AFDB-45DC4D2E3F65}']
    procedure Changed;
  end;

  { IcxLookAndFeelContainer }

  IcxLookAndFeelContainer = interface
    ['{6065B58B-C557-4464-A67D-64183FD13F25}']
    function GetLookAndFeel: TcxLookAndFeel;
  end;

  { TcxLookAndFeel }

  TcxLookAndFeelChangedEvent = procedure(Sender: TcxLookAndFeel;
    AChangedValues: TcxLookAndFeelValues) of object;
  TdxTouchScrollUIMode = (tsmDefault, tsmEnabled, tsmDisabled); { deprecated }

  TcxLookAndFeelData = record
    Kind: TcxLookAndFeelKind;
    NativeStyle: Boolean;
    SkinName: string;
    ScrollbarMode: TdxScrollbarMode;
    ScrollMode: TdxScrollMode;
    RenderMode: TdxRenderMode;
    UseSkins: Boolean;
  end;

  TcxLookAndFeel = class(TcxInterfacedPersistent,
    IcxLookAndFeelNotificationListener, IcxLookAndFeelNotificationListener2)
  private
    FAssignedValues: TcxLookAndFeelValues;
    FChangedValues: TcxLookAndFeelValues;
    FChangeListenerList: TcxObjectList;
    FCurrentState: TcxLookAndFeelData;
    FData: TcxLookAndFeelData;
    FIsDestruction: Boolean;
    FIsRootLookAndFeel: Boolean;
    FMasterLookAndFeel: TcxLookAndFeel;
    FPainter: TcxCustomLookAndFeelPainter;
    FSkinPainter: TcxCustomLookAndFeelPainter;
    FPrevState: TcxLookAndFeelData;
    FSystemPaletteChangedNotifier: TcxSystemPaletteChangedNotifier;
    FUpdateLockCount: Integer;

    FOnChanged: TcxLookAndFeelChangedEvent;
    FOnMasterBeginChange: TNotifyEvent;
    FOnMasterEndChange: TNotifyEvent;

    function GetActiveStyle: TcxLookAndFeelStyle;
    function GetKind: TcxLookAndFeelKind;
    function GetMasterLookAndFeel: TcxLookAndFeel;
    function GetNativeStyle: Boolean;
    function GetRenderMode: TdxRenderMode;
    function GetScrollbarMode: TdxScrollbarMode;
    function GetScrollMode: TdxScrollMode;
    function GetSkinName: TdxSkinName;
    function GetTouchScrollUIMode: TdxTouchScrollUIMode;
    procedure SetAssignedValues(Value: TcxLookAndFeelValues);
    procedure SetKind(Value: TcxLookAndFeelKind);
    procedure SetMasterLookAndFeel(Value: TcxLookAndFeel);
    procedure SetNativeStyle(Value: Boolean);
    procedure SetRenderMode(const Value: TdxRenderMode);
    procedure SetScrollbarMode(const Value: TdxScrollbarMode);
    procedure SetScrollMode(const Value: TdxScrollMode);
    procedure SetSkinName(const Value: TdxSkinName);
    procedure SetTouchScrollUIMode(const Value: TdxTouchScrollUIMode);

    function GetDefaultKind: TcxLookAndFeelKind;
    function GetDefaultNativeStyle: Boolean;
    function GetDefaultRenderMode: TdxRenderMode;
    function GetDefaultScrollbarMode: TdxScrollbarMode;
    function GetDefaultScrollMode: TdxScrollMode;
    function GetDefaultSkinName: string;
    function GetDefaultSkinPainter: TcxCustomLookAndFeelPainter;

    function IsKindStored: Boolean;
    function IsNativeStyleStored: Boolean;
    function IsRenderModeStored: Boolean;
    function IsScrollbarModeStored: Boolean;
    function IsScrollModeStored: Boolean;
    function IsSkinNameStored: Boolean;

    procedure CheckStateChanges;
    procedure ReadSkinName(Reader: TReader);
    procedure ReadTouchScrollUIMode(Reader: TReader);
    procedure SaveState;
    procedure WriteSkinName(Writer: TWriter);
  protected
    procedure Changed(AChangedValues: TcxLookAndFeelValues);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoMasterLookAndFeelChanged(AChangedValues: TcxLookAndFeelValues);
    procedure DoMasterLookAndFeelDestroying;
    function GetSkinPainter(const ASkinName: string)
      : TcxCustomLookAndFeelPainter; virtual;
    procedure InitializeCurrentState; virtual;
    procedure InitializePainter; virtual;
    function InternalGetKind: TcxLookAndFeelKind; virtual;
    function InternalGetNativeStyle: Boolean; virtual;
    function InternalGetRenderMode: TdxRenderMode; virtual;
    function InternalGetScrollbarMode: TdxScrollbarMode; virtual;
    function InternalGetScrollMode: TdxScrollMode; virtual;
    function InternalGetSkinName: string; virtual;
    function InternalGetSkinPainter: TcxCustomLookAndFeelPainter; virtual;
    procedure NotifyChanged;
    procedure SystemPaletteChanged; virtual;

    // IcxLookAndFeelNotificationListener2
    function GetObject: TObject;
    procedure MasterLookAndFeelBeginChange;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues);
    procedure MasterLookAndFeelEndChange;
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);

    property ChangeListenerList: TcxObjectList read FChangeListenerList;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetAvailablePainter(ANeededThemedObjectType: TdxThemedObjectType)
      : TcxCustomLookAndFeelPainter; overload;
    function GetAvailablePainter(ANeededThemedObjectTypes
      : TdxThemedObjectTypes = []): TcxCustomLookAndFeelPainter;
      overload; virtual;
    procedure Refresh;
    procedure Reset;
    procedure SetStyle(Value: TcxLookAndFeelStyle);
    //
    procedure AddChangeListener(AListener: TObject);
    procedure RemoveChangeListener(AListener: TObject);
    //
    property ActiveStyle: TcxLookAndFeelStyle read GetActiveStyle;
    property MasterLookAndFeel: TcxLookAndFeel read GetMasterLookAndFeel
      write SetMasterLookAndFeel;
    property Painter: TcxCustomLookAndFeelPainter read FPainter;
    property SkinPainter: TcxCustomLookAndFeelPainter read FSkinPainter
      write FSkinPainter;
    property TouchScrollUIMode: TdxTouchScrollUIMode read GetTouchScrollUIMode
      write SetTouchScrollUIMode; { deprecated }

    property OnChanged: TcxLookAndFeelChangedEvent read FOnChanged
      write FOnChanged;
    property OnMasterBeginChange: TNotifyEvent read FOnMasterBeginChange
      write FOnMasterBeginChange;
    property OnMasterEndChange: TNotifyEvent read FOnMasterEndChange
      write FOnMasterEndChange;
  published
    property AssignedValues: TcxLookAndFeelValues read FAssignedValues
      write SetAssignedValues stored False;
    property Kind: TcxLookAndFeelKind read GetKind write SetKind
      stored IsKindStored;
    property NativeStyle: Boolean read GetNativeStyle write SetNativeStyle
      stored IsNativeStyleStored;
    property RenderMode: TdxRenderMode read GetRenderMode write SetRenderMode
      stored IsRenderModeStored;
    property ScrollbarMode: TdxScrollbarMode read GetScrollbarMode
      write SetScrollbarMode stored IsScrollbarModeStored;
    property ScrollMode: TdxScrollMode read GetScrollMode write SetScrollMode
      stored IsScrollModeStored;
    property SkinName: TdxSkinName read GetSkinName write SetSkinName
      stored IsSkinNameStored;
  end;

  { TcxRootLookAndFeel }

  TcxRootLookAndFeel = class(TcxLookAndFeel)
  public
    constructor Create; reintroduce;
  end;

  { TcxLookAndFeelController }

  TcxLookAndFeelController = class(TcxCustomComponent,
    IcxLookAndFeelNotificationListener)
  strict private
    function GetKind: TcxLookAndFeelKind;
    function GetNativeStyle: Boolean;
    function GetRenderMode: TdxRenderMode;
    function GetScrollbarMode: TdxScrollbarMode;
    function GetScrollMode: TdxScrollMode;
    function GetSkinName: TdxSkinName;
    function GetTouchMode: Boolean;
    function GetTouchScrollUIMode: Boolean;
    function IsSkinNameStored: Boolean;
    procedure SetKind(Value: TcxLookAndFeelKind);
    procedure SetNativeStyle(Value: Boolean);
    procedure SetRenderMode(AValue: TdxRenderMode);
    procedure SetScrollbarMode(const Value: TdxScrollbarMode);
    procedure SetScrollMode(const Value: TdxScrollMode);
    procedure SetSkinName(const Value: TdxSkinName);
    procedure SetTouchMode(Value: Boolean);
    procedure SetTouchScrollUIMode(const Value: Boolean);

    procedure ReadTouchScrollUIMode(AReader: TReader);
    procedure Modified;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    { IcxLookAndFeelNotificationListener }
    function GetObject: TObject;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Reset; virtual;
    //
    property TouchScrollUIMode: Boolean read GetTouchScrollUIMode
      write SetTouchScrollUIMode; { deprecated }
  published
    property Kind: TcxLookAndFeelKind read GetKind write SetKind
      default cxDefaultLookAndFeelKind;
    property NativeStyle: Boolean read GetNativeStyle write SetNativeStyle
      default cxDefaultLookAndFeelNativeStyle;
    property ScrollbarMode: TdxScrollbarMode read GetScrollbarMode
      write SetScrollbarMode default sbmDefault;
    property ScrollMode: TdxScrollMode read GetScrollMode write SetScrollMode
      default scmDefault;
    property SkinName: TdxSkinName read GetSkinName write SetSkinName
      stored IsSkinNameStored;
    property RenderMode: TdxRenderMode read GetRenderMode write SetRenderMode
      default rmDefault;
    property TouchMode: Boolean read GetTouchMode write SetTouchMode
      default cxDefaultIsTouchModeEnabled;
  end;

  { TcxSystemPaletteChangedNotifier }

  TcxSystemPaletteChangedEvent = procedure of object;

  TcxSystemPaletteChangedNotifier = class
  private
    FIsPrimary: Boolean;
    FOnSystemPaletteChanged: TcxSystemPaletteChangedEvent;
  protected
    procedure DoChanged; virtual;
  public
    constructor Create(AIsPrimary: Boolean = False); virtual;
    destructor Destroy; override;
    property OnSystemPaletteChanged: TcxSystemPaletteChangedEvent
      read FOnSystemPaletteChanged write FOnSystemPaletteChanged;
  end;

  { TdxVisualRefinements }

  TdxPadding = record
    Left, Top, Right, Bottom: Integer;
  strict private
    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;
  public
    class operator Equal(const L, R: TdxPadding): Boolean; // for internal use
    class operator NotEqual(const L, R: TdxPadding): Boolean;
    // for internal use
    class operator Implicit(const AValue: TRect): TdxPadding;
    // for internal use
    class operator Implicit(const AValue: TdxPadding): TRect;
    // for internal use

    procedure Deflate(var R: TRect; AScaleFactor: TdxScaleFactor;
      ARightToLeft: Boolean = False); overload; inline; // for internal use
    procedure Deflate(var R: TRect; ARightToLeft: Boolean = False); overload;
      inline; // for internal use
    procedure Inflate(var R: TRect; AScaleFactor: TdxScaleFactor;
      ARightToLeft: Boolean = False); overload; inline; // for internal use
    procedure Inflate(var R: TRect; ARightToLeft: Boolean = False); overload;
      inline; // for internal use
    procedure InflatePadding(var APadding: TRect; AScaleFactor: TdxScaleFactor;
      ARightToLeft: Boolean = False); overload; inline; // for internal use
    procedure InflatePadding(var APadding: TRect;
      ARightToLeft: Boolean = False); overload; inline; // for internal use
    function ScaledWidth(AScaleFactor: TdxScaleFactor): Integer; inline;
    // for internal use
    function ScaledHeight(AScaleFactor: TdxScaleFactor): Integer; inline;
    // for internal use

    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  TdxVisualRefinements = class
  strict private
  class var
    FPadding: TdxPadding;
    FHeaderPadding: TdxPadding;
    FListeners: TdxFastObjectList;
    class procedure Changed; static;
    class procedure SetPadding(const AValue: TdxPadding); static;
    class procedure SetHeaderPadding(const AValue: TdxPadding); static;
  protected
    class procedure Initialize; static;
    class procedure Finalize; static;
  public
    class procedure AddListener(AListener: TObject); static; // for internal use
    class procedure RemoveListener(AListener: TObject); static;
    // for internal use

    class property Padding: TdxPadding read FPadding write SetPadding;
    class property HeaderPadding: TdxPadding read FHeaderPadding
      write SetHeaderPadding;
  end;

  TdxClassSupportsSkinProc = function(AClass: TPersistent): Boolean;
  TdxGetSkinNamesProc = procedure(AList: TStrings);
  TdxGetSkinPainterProc = function(const ASkinName: string)
    : TcxCustomLookAndFeelPainter;

const
  cxLookAndFeelKindMap: array [TcxLookAndFeelStyle] of TcxLookAndFeelKind =
    (lfFlat, lfStandard, lfUltraFlat, lfStandard, lfOffice11, lfUltraFlat);
  cxLookAndFeelStyleMap: array [TcxLookAndFeelKind] of TcxLookAndFeelStyle =
    (lfsFlat, lfsStandard, lfsUltraFlat, lfsOffice11);

var
  ClassSupportsSkinProc: TdxClassSupportsSkinProc;
  GetSkinNamesProc: TdxGetSkinNamesProc;
  GetSkinPainterProc: TdxGetSkinPainterProc;

  dxSkinShowFormShadow: TdxDefaultBoolean = bDefault;

function cxIsTouchScrollUIModeEnabled: Boolean;
function IsRootLookAndFeelAssigned: Boolean;
function RootLookAndFeel: TcxLookAndFeel;
procedure SetControlLookAndFeel(AControl: TControl;
  AMasterLookAndFeel: TcxLookAndFeel); overload;
procedure SetControlLookAndFeel(AControl: TControl; AKind: TcxLookAndFeelKind;
  ANativeStyle: Boolean); overload;

implementation

uses
  RTLConsts, Windows, TypInfo, cxControls, dxSkinsCore, dxTypeHelpers;

const
  LookAndFeelValueAll = [Low(TcxLookAndFeelValue) .. High(TcxLookAndFeelValue)];

type
  { TcxSystemPaletteChangedListener }

  TcxSystemPaletteChangedListener = class
  private
    FNotifierList: TList;
    FPrimaryNotifierList: TList;
    FWindowHandle: TcxHandle;
    procedure DoChange;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotifier(ANotifier: TcxSystemPaletteChangedNotifier;
      AIsPrimary: Boolean);
    procedure RemoveNotifier(ANotifier: TcxSystemPaletteChangedNotifier;
      AIsPrimary: Boolean);
  end;

var
  FLookAndFeelControllerCount: Integer;
  FRootLookAndFeel: TcxRootLookAndFeel;
  FSystemPaletteChangedListener: TcxSystemPaletteChangedListener;
  FSystemPaletteChangedListenerRefCount: Integer;
  FUnitIsFinalized: Boolean;
  cxIsTouchSettingsChanging: Boolean;

procedure SetControlLookAndFeel(AControl: TControl;
  AMasterLookAndFeel: TcxLookAndFeel);
var
  AIntf: IcxLookAndFeelContainer;
  I: Integer;
begin
  if Supports(AControl, IcxLookAndFeelContainer, AIntf) then
    AIntf.GetLookAndFeel.MasterLookAndFeel := AMasterLookAndFeel
  else
    for I := 0 to AControl.ComponentCount - 1 do
    begin
      if Supports(AControl.Components[I], IcxLookAndFeelContainer, AIntf) then
        AIntf.GetLookAndFeel.MasterLookAndFeel := AMasterLookAndFeel;
    end;

  if AControl is TWinControl then
  begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      SetControlLookAndFeel(TWinControl(AControl).Controls[I],
        AMasterLookAndFeel);
  end;
end;

procedure SetControlLookAndFeel(AControl: TControl; AKind: TcxLookAndFeelKind;
  ANativeStyle: Boolean);
var
  AIntf: IcxLookAndFeelContainer;
  I: Integer;
begin
  if Supports(AControl, IcxLookAndFeelContainer, AIntf) then
    with AIntf.GetLookAndFeel do
    begin
      Kind := AKind;
      NativeStyle := ANativeStyle;
    end;

  if AControl is TWinControl then
  begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      SetControlLookAndFeel(TWinControl(AControl).Controls[I], AKind,
        ANativeStyle);
  end;
end;

procedure AddRefSystemPaletteChangedListener;
begin
  if FSystemPaletteChangedListenerRefCount = 0 then
    FSystemPaletteChangedListener := TcxSystemPaletteChangedListener.Create;
  Inc(FSystemPaletteChangedListenerRefCount);
end;

procedure ReleaseRefSystemPaletteChangedListener;
begin
  Dec(FSystemPaletteChangedListenerRefCount);
  if FSystemPaletteChangedListenerRefCount = 0 then
    FreeAndNil(FSystemPaletteChangedListener);
end;

function cxIsTouchScrollUIModeEnabled: Boolean;
begin
  if FRootLookAndFeel <> nil then
    Result := (FRootLookAndFeel.ScrollbarMode = sbmTouch) or
      (FRootLookAndFeel.ScrollbarMode = sbmDefault) and
      cxDefaultIsTouchScrollUIModeEnabled
  else
    Result := cxDefaultIsTouchScrollUIModeEnabled;
end;

function IsRootLookAndFeelAssigned: Boolean;
begin
  Result := FRootLookAndFeel <> nil;
end;

function RootLookAndFeel: TcxLookAndFeel;
begin
  if (FRootLookAndFeel = nil) and not FUnitIsFinalized then
    FRootLookAndFeel := TcxRootLookAndFeel.Create;
  Result := FRootLookAndFeel;
end;

{ TcxSystemPaletteChangedListener }

constructor TcxSystemPaletteChangedListener.Create;
begin
  inherited Create;
  CreateOffice11Colors;
  FWindowHandle := AllocateHWnd(WndProc);
  FNotifierList := TList.Create;
  FPrimaryNotifierList := TList.Create;
end;

destructor TcxSystemPaletteChangedListener.Destroy;
begin
  FreeAndNil(FPrimaryNotifierList);
  FreeAndNil(FNotifierList);
  DeallocateHWnd(FWindowHandle);
  ReleaseOffice11Colors;
  inherited Destroy;
end;

procedure TcxSystemPaletteChangedListener.AddNotifier
  (ANotifier: TcxSystemPaletteChangedNotifier; AIsPrimary: Boolean);
begin
  if AIsPrimary then
  begin
    if FPrimaryNotifierList <> nil then
      FPrimaryNotifierList.Add(ANotifier);
  end
  else if FNotifierList <> nil then
    FNotifierList.Add(ANotifier);
end;

procedure TcxSystemPaletteChangedListener.RemoveNotifier
  (ANotifier: TcxSystemPaletteChangedNotifier; AIsPrimary: Boolean);
begin
  if AIsPrimary then
  begin
    if FPrimaryNotifierList <> nil then
      FPrimaryNotifierList.Remove(ANotifier);
  end
  else if FNotifierList <> nil then
    FNotifierList.Remove(ANotifier);
end;

procedure TcxSystemPaletteChangedListener.DoChange;
var
  I: Integer;
begin
  RefreshOffice11Colors;
  for I := FPrimaryNotifierList.Count - 1 downto 0 do
    // for I := 0 to FPrimaryNotifierList.Count - 1 do
    TcxSystemPaletteChangedNotifier(FPrimaryNotifierList[I]).DoChanged;
  for I := FNotifierList.Count - 1 downto 0 do
    // for I := 0 to FNotifierList.Count - 1 do
    TcxSystemPaletteChangedNotifier(FNotifierList[I]).DoChanged;
end;

procedure TcxSystemPaletteChangedListener.WndProc(var Msg: TMessage);
begin
  with Msg do
    try
      if Msg = WM_SYSCOLORCHANGE then
        DoChange;
    finally
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
    end;
end;

{ TcxLookAndFeel }

constructor TcxLookAndFeel.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FChangeListenerList := TcxObjectList.Create(False);
  FData.Kind := cxDefaultLookAndFeelKind;
  FData.NativeStyle := cxDefaultLookAndFeelNativeStyle;
  FData.RenderMode := rmDefault;
  FData.ScrollbarMode := sbmDefault;
  FData.ScrollMode := scmDefault;
  FCurrentState := FData;
  FSystemPaletteChangedNotifier := TcxSystemPaletteChangedNotifier.Create;
  FSystemPaletteChangedNotifier.OnSystemPaletteChanged := SystemPaletteChanged;
  if not FIsRootLookAndFeel then
  begin
    RootLookAndFeel.AddChangeListener(Self);
    InitializeCurrentState;
  end;
  InitializePainter;
end;

destructor TcxLookAndFeel.Destroy;
begin
  FreeAndNil(FSystemPaletteChangedNotifier);
  DoMasterLookAndFeelDestroying;
  FreeAndNil(FChangeListenerList);

  if MasterLookAndFeel <> nil then
    MasterLookAndFeel.RemoveChangeListener(Self);

  if FIsRootLookAndFeel then
  begin
    FRootLookAndFeel := nil;
    FIsRootLookAndFeel := False;
  end;
  inherited Destroy;
end;

procedure TcxLookAndFeel.AddChangeListener(AListener: TObject);
var
  AIsLookAndFeelController: Boolean;
  AIntf: IcxLookAndFeelNotificationListener;
begin
  if (AListener = nil) or not Supports(AListener,
    IcxLookAndFeelNotificationListener, AIntf) then
    Exit;
  AIsLookAndFeelController := AIntf.GetObject is TcxLookAndFeelController;
  if not FIsRootLookAndFeel and AIsLookAndFeelController then
    Exit;
  if ChangeListenerList.IndexOf(AListener) >= 0 then
    Exit;

  if FIsRootLookAndFeel and AIsLookAndFeelController then
    Inc(FLookAndFeelControllerCount);
  ChangeListenerList.Add(AListener);
end;

procedure TcxLookAndFeel.Assign(Source: TPersistent);
begin
  if Source is TcxLookAndFeel then
    with Source as TcxLookAndFeel do
    begin
      Self.SaveState;
      Self.FData := FData;
      Self.FAssignedValues := FAssignedValues;
      Self.MasterLookAndFeel := MasterLookAndFeel;
      Self.CheckStateChanges;
    end
  else
    inherited Assign(Source);
end;

procedure TcxLookAndFeel.Refresh;
begin
  Changed(LookAndFeelValueAll);
end;

procedure TcxLookAndFeel.RemoveChangeListener(AListener: TObject);
var
  AIsLookAndFeelController: Boolean;
  AIntf: IcxLookAndFeelNotificationListener;
begin
  if (AListener = nil) or not Supports(AListener,
    IcxLookAndFeelNotificationListener, AIntf) then
    Exit;

  AIsLookAndFeelController := AIntf.GetObject is TcxLookAndFeelController;
  if ChangeListenerList.IndexOf(AListener) < 0 then
    Exit;

  if not FIsDestruction then
    ChangeListenerList.Remove(AListener);

  if FIsRootLookAndFeel and AIsLookAndFeelController then
  begin
    Dec(FLookAndFeelControllerCount);
    if FLookAndFeelControllerCount = 0 then
      Reset;
  end;
end;

procedure TcxLookAndFeel.Reset;
begin
  if FIsRootLookAndFeel then
    Painter.ResetLookAndFeelSettings;

  AssignedValues := [];

  if FIsRootLookAndFeel then
  begin
    dxSkinShowFormShadow := bDefault;
    dxSkinsUseImageSet := imsDefault;
    cxUseSkins := cxDefaultUseSkins;
    cxIsTouchModeEnabled := cxDefaultIsTouchModeEnabled;
  end;
end;

procedure TcxLookAndFeel.SetStyle(Value: TcxLookAndFeelStyle);
begin
  NativeStyle := Value = lfsNative;
  if not NativeStyle then
    Kind := cxLookAndFeelKindMap[Value];
end;

procedure TcxLookAndFeel.SetTouchScrollUIMode(const Value
  : TdxTouchScrollUIMode);
var
  AScrollbarMode: TdxScrollbarMode;
begin
  case Value of
    tsmDefault:
      AScrollbarMode := sbmDefault;
    tsmEnabled:
      AScrollbarMode := sbmTouch;
  else // tsmDisabled
    AScrollbarMode := sbmClassic;
  end;
  ScrollbarMode := AScrollbarMode;
end;

procedure TcxLookAndFeel.BeginUpdate;
begin
  Inc(FUpdateLockCount);
end;

procedure TcxLookAndFeel.EndUpdate;
begin
  Dec(FUpdateLockCount);
  if FUpdateLockCount = 0 then
  begin
    Changed(FChangedValues);
    FChangedValues := [];
  end;
end;

procedure TcxLookAndFeel.Changed(AChangedValues: TcxLookAndFeelValues);
begin
  if not FIsDestruction then
  begin
    if FCurrentState.UseSkins <> cxUseSkins then
      AChangedValues := AChangedValues + [lfvSkinName];
    if (AChangedValues <> []) or cxIsTouchSettingsChanging then
    begin
      InitializeCurrentState;
      InitializePainter;
      if FUpdateLockCount = 0 then
      begin
        DoMasterLookAndFeelChanged(AChangedValues);
        if Assigned(FOnChanged) then
          FOnChanged(Self, AChangedValues);
      end
      else
        FChangedValues := FChangedValues + AChangedValues;
    end;
  end;
end;

procedure TcxLookAndFeel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SkinName', ReadSkinName, WriteSkinName,
    IsSkinNameStored and (SkinName = ''));
  Filer.DefineProperty('TouchScrollUIMode', ReadTouchScrollUIMode, nil, False);
end;

procedure TcxLookAndFeel.DoMasterLookAndFeelChanged(AChangedValues
  : TcxLookAndFeelValues);
var
  APrevCount: Integer;
  I: Integer;
  AIntf: IcxLookAndFeelNotificationListener;
  AIntf2: IcxLookAndFeelNotificationListener2;
begin
  I := 0;
  while I < ChangeListenerList.Count do
  begin
    APrevCount := ChangeListenerList.Count;
    if Supports(ChangeListenerList[I], IcxLookAndFeelNotificationListener2,
      AIntf2) then
    begin
      AIntf2.MasterLookAndFeelBeginChange;
      AIntf2 := nil;
    end;
    if APrevCount = ChangeListenerList.Count then
      Inc(I);
  end;

  I := 0;
  while I < ChangeListenerList.Count do
  begin
    APrevCount := ChangeListenerList.Count;
    ChangeListenerList[I].GetInterface
      (IcxLookAndFeelNotificationListener, AIntf);
    AIntf.MasterLookAndFeelChanged(Self, AChangedValues);
    AIntf := nil;
    if APrevCount = ChangeListenerList.Count then
      Inc(I);
  end;

  I := 0;
  while I < ChangeListenerList.Count do
  begin
    APrevCount := ChangeListenerList.Count;
    if Supports(ChangeListenerList[I], IcxLookAndFeelNotificationListener2,
      AIntf2) then
    begin
      AIntf2.MasterLookAndFeelEndChange;
      AIntf2 := nil;
    end;
    if APrevCount = ChangeListenerList.Count then
      Inc(I);
  end;
end;

procedure TcxLookAndFeel.DoMasterLookAndFeelDestroying;
var
  I: Integer;
  AIntf: IcxLookAndFeelNotificationListener;
begin
  FIsDestruction := True;
  for I := 0 to ChangeListenerList.Count - 1 do
  begin
    ChangeListenerList[I].GetInterface
      (IcxLookAndFeelNotificationListener, AIntf);
    AIntf.MasterLookAndFeelDestroying(Self);
    AIntf := nil;
  end;
  FIsDestruction := False;
end;

function TcxLookAndFeel.GetSkinPainter(const ASkinName: string)
  : TcxCustomLookAndFeelPainter;
begin
  if cxUseSkins and (ASkinName <> '') then
  begin
    Result := cxLookAndFeelPaintersManager.GetPainter(ASkinName);
    if (Result = nil) and Assigned(GetSkinPainterProc) then
      Result := GetSkinPainterProc(ASkinName);
  end
  else
    Result := nil;
end;

function TcxLookAndFeel.GetTouchScrollUIMode: TdxTouchScrollUIMode;
begin
  case ScrollbarMode of
    sbmDefault:
      Result := tsmDefault;
    sbmTouch:
      Result := tsmEnabled;
  else // sbmClassic, sbmHybrid
    Result := tsmDisabled;
  end;
end;

function TcxLookAndFeel.GetScrollbarMode: TdxScrollbarMode;
begin
  Result := FCurrentState.ScrollbarMode;
end;

function TcxLookAndFeel.GetScrollMode: TdxScrollMode;
begin
  Result := FCurrentState.ScrollMode;
end;

procedure TcxLookAndFeel.ReadSkinName(Reader: TReader);
begin
  SkinName := Reader.ReadString;
end;

procedure TcxLookAndFeel.ReadTouchScrollUIMode(Reader: TReader);
begin
  TouchScrollUIMode := TdxTouchScrollUIMode
    (GetEnumValue(TypeInfo(TdxTouchScrollUIMode), Reader.ReadIdent));
end;

procedure TcxLookAndFeel.WriteSkinName(Writer: TWriter);
begin
  Writer.WriteString(SkinName);
end;

procedure TcxLookAndFeel.InitializeCurrentState;
begin
  FCurrentState.Kind := InternalGetKind;
  FCurrentState.NativeStyle := InternalGetNativeStyle;
  FCurrentState.SkinName := InternalGetSkinName;
  FCurrentState.ScrollbarMode := InternalGetScrollbarMode;
  FCurrentState.ScrollMode := InternalGetScrollMode;
  FCurrentState.RenderMode := InternalGetRenderMode;
  FCurrentState.UseSkins := cxUseSkins;
  if FCurrentState.NativeStyle then
    FSkinPainter := nil
  else
    FSkinPainter := InternalGetSkinPainter;
end;

procedure TcxLookAndFeel.InitializePainter;
begin
  FPainter := GetAvailablePainter;
end;

function TcxLookAndFeel.InternalGetKind: TcxLookAndFeelKind;
begin
  if lfvKind in FAssignedValues then
    Result := FData.Kind
  else
    Result := GetDefaultKind;
end;

function TcxLookAndFeel.InternalGetNativeStyle: Boolean;
begin
  if lfvNativeStyle in FAssignedValues then
    Result := FData.NativeStyle
  else
    Result := GetDefaultNativeStyle;
end;

function TcxLookAndFeel.InternalGetRenderMode: TdxRenderMode;
begin
  if lfvRenderMode in FAssignedValues then
    Result := FData.RenderMode
  else
    Result := GetDefaultRenderMode;
end;

function TcxLookAndFeel.InternalGetSkinName: string;
begin
  if lfvSkinName in FAssignedValues then
    Result := FData.SkinName
  else
    Result := GetDefaultSkinName;
end;

function TcxLookAndFeel.InternalGetSkinPainter: TcxCustomLookAndFeelPainter;
begin
  if lfvSkinName in FAssignedValues then
    Result := GetSkinPainter(FData.SkinName)
  else
    Result := GetDefaultSkinPainter;
end;

function TcxLookAndFeel.InternalGetScrollbarMode: TdxScrollbarMode;
begin
  if lfvScrollbarMode in FAssignedValues then
    Result := FData.ScrollbarMode
  else
    Result := GetDefaultScrollbarMode;
end;

function TcxLookAndFeel.InternalGetScrollMode: TdxScrollMode;
begin
  if lfvScrollMode in FAssignedValues then
    Result := FData.ScrollMode
  else
    Result := GetDefaultScrollMode;
end;

procedure TcxLookAndFeel.NotifyChanged;
var
  AListener: TObject;
  APrevCount, I: Integer;
  AIntf: IcxLookAndFeelNotificationListener;
begin
  if FIsDestruction then
    Exit;
  I := 0;
  while I < ChangeListenerList.Count do
  begin
    APrevCount := ChangeListenerList.Count;
    ChangeListenerList[I].GetInterface
      (IcxLookAndFeelNotificationListener, AIntf);
    AListener := AIntf.GetObject;
    AIntf := nil;
    if AListener is TcxLookAndFeel then
      TcxLookAndFeel(AListener).NotifyChanged;
    if APrevCount = ChangeListenerList.Count then
      Inc(I);
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self, []);
end;

procedure TcxLookAndFeel.SystemPaletteChanged;
begin
  Changed([lfvNativeStyle]);
end;

function TcxLookAndFeel.GetObject: TObject;
begin
  Result := Self;
end;

procedure TcxLookAndFeel.MasterLookAndFeelBeginChange;
begin
  dxCallNotify(FOnMasterBeginChange, Self);
end;

procedure TcxLookAndFeel.MasterLookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
var
  AOwnChangedValues: TcxLookAndFeelValues;
begin
  AOwnChangedValues := (LookAndFeelValueAll - FAssignedValues) * AChangedValues;
  Changed(AOwnChangedValues);
end;

procedure TcxLookAndFeel.MasterLookAndFeelEndChange;
begin
  dxCallNotify(FOnMasterEndChange, Self);
end;

procedure TcxLookAndFeel.MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
begin
  MasterLookAndFeel := nil;
end;

function TcxLookAndFeel.GetActiveStyle: TcxLookAndFeelStyle;
begin
  if NativeStyle and AreVisualStylesAvailable then
    Result := lfsNative
  else if SkinPainter <> nil then
    Result := lfsSkin
  else
    Result := cxLookAndFeelStyleMap[Kind];
end;

function TcxLookAndFeel.GetAvailablePainter(ANeededThemedObjectType
  : TdxThemedObjectType): TcxCustomLookAndFeelPainter;
begin
  Result := GetAvailablePainter([ANeededThemedObjectType]);
end;

function TcxLookAndFeel.GetAvailablePainter(ANeededThemedObjectTypes
  : TdxThemedObjectTypes = []): TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager = nil then
    Result := nil
  else if NativeStyle and AreVisualStylesAvailable(ANeededThemedObjectTypes)
  then
    Result := cxLookAndFeelPaintersManager.GetPainter(lfsNative)
  else if SkinPainter <> nil then
    Result := SkinPainter
  else
    Result := cxLookAndFeelPaintersManager.GetPainter(ActiveStyle);
end;

function TcxLookAndFeel.GetKind: TcxLookAndFeelKind;
begin
  Result := FCurrentState.Kind;
end;

function TcxLookAndFeel.GetMasterLookAndFeel: TcxLookAndFeel;
begin
  if FIsRootLookAndFeel then
    Result := nil
  else if FMasterLookAndFeel = nil then
    Result := FRootLookAndFeel
  else
    Result := FMasterLookAndFeel;
end;

function TcxLookAndFeel.GetNativeStyle: Boolean;
begin
  Result := FCurrentState.NativeStyle;
end;

function TcxLookAndFeel.GetRenderMode: TdxRenderMode;
begin
  Result := FCurrentState.RenderMode;
end;

function TcxLookAndFeel.GetSkinName: TdxSkinName;
begin
  Result := FCurrentState.SkinName;
end;

procedure TcxLookAndFeel.SetAssignedValues(Value: TcxLookAndFeelValues);
begin
  if Value <> FAssignedValues then
  begin
    SaveState;
    FAssignedValues := Value;
    CheckStateChanges;
  end;
end;

procedure TcxLookAndFeel.SetKind(Value: TcxLookAndFeelKind);
var
  AOldKind: TcxLookAndFeelKind;
begin
  AOldKind := Kind;
  Include(FAssignedValues, lfvKind);
  FData.Kind := Value;
  if AOldKind <> InternalGetKind then
    Changed([lfvKind]);
end;

procedure TcxLookAndFeel.SetMasterLookAndFeel(Value: TcxLookAndFeel);
begin
  if FIsRootLookAndFeel or (Value = Self) then
    Exit;
  if Value <> MasterLookAndFeel then
  begin
    SaveState;
    if MasterLookAndFeel <> nil then
      MasterLookAndFeel.RemoveChangeListener(Self);
    FMasterLookAndFeel := Value;
    if MasterLookAndFeel <> nil then
      MasterLookAndFeel.AddChangeListener(Self);
    CheckStateChanges;
  end;
end;

procedure TcxLookAndFeel.SetNativeStyle(Value: Boolean);
var
  AOldNativeStyle: Boolean;
begin
  AOldNativeStyle := NativeStyle;
  Include(FAssignedValues, lfvNativeStyle);
  FData.NativeStyle := Value;
  if AOldNativeStyle <> InternalGetNativeStyle then
    Changed([lfvNativeStyle]);
end;

procedure TcxLookAndFeel.SetSkinName(const Value: TdxSkinName);
var
  AOldSkinName: string;
begin
  AOldSkinName := SkinName;
  Include(FAssignedValues, lfvSkinName);
  FData.SkinName := Value;
  if AOldSkinName <> InternalGetSkinName then
    Changed([lfvSkinName]);
end;

procedure TcxLookAndFeel.SetRenderMode(const Value: TdxRenderMode);
var
  AOldRenderMode: TdxRenderMode;
begin
  AOldRenderMode := RenderMode;
  Include(FAssignedValues, lfvRenderMode);
  FData.RenderMode := Value;
  if AOldRenderMode <> InternalGetRenderMode then
    Changed([lfvRenderMode]);
end;

procedure TcxLookAndFeel.SetScrollbarMode(const Value: TdxScrollbarMode);
var
  AOldScrollbarMode: TdxScrollbarMode;
begin
  AOldScrollbarMode := ScrollbarMode;
  Include(FAssignedValues, lfvScrollbarMode);
  FData.ScrollbarMode := Value;
  if AOldScrollbarMode <> InternalGetScrollbarMode then
    Changed([lfvScrollbarMode]);
end;

procedure TcxLookAndFeel.SetScrollMode(const Value: TdxScrollMode);
var
  AOldScrollMode: TdxScrollMode;
begin
  AOldScrollMode := ScrollMode;
  Include(FAssignedValues, lfvScrollMode);
  FData.ScrollMode := Value;
  if AOldScrollMode <> InternalGetScrollMode then
    Changed([lfvScrollMode]);
end;

procedure TcxLookAndFeel.CheckStateChanges;
var
  AChangedValues: TcxLookAndFeelValues;
begin
  AChangedValues := [];
  if FPrevState.Kind <> InternalGetKind then
    Include(AChangedValues, lfvKind);
  if FPrevState.NativeStyle <> InternalGetNativeStyle then
    Include(AChangedValues, lfvNativeStyle);
  if FPrevState.SkinName <> InternalGetSkinName then
    Include(AChangedValues, lfvSkinName);
  if FPrevState.ScrollbarMode <> InternalGetScrollbarMode then
    Include(AChangedValues, lfvScrollbarMode);
  if FPrevState.ScrollMode <> InternalGetScrollMode then
    Include(AChangedValues, lfvScrollMode);
  if FPrevState.RenderMode <> InternalGetRenderMode then
    Include(AChangedValues, lfvRenderMode);
  Changed(AChangedValues);
end;

function TcxLookAndFeel.GetDefaultKind: TcxLookAndFeelKind;
begin
  if FIsRootLookAndFeel then
    Result := cxDefaultLookAndFeelKind
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.Kind
  else if FRootLookAndFeel = nil then
    Result := cxDefaultLookAndFeelKind
  else
    Result := FRootLookAndFeel.Kind
end;

function TcxLookAndFeel.GetDefaultNativeStyle: Boolean;
begin
  if FIsRootLookAndFeel then
    Result := cxDefaultLookAndFeelNativeStyle
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.NativeStyle
  else if FRootLookAndFeel = nil then
    Result := cxDefaultLookAndFeelNativeStyle
  else
    Result := FRootLookAndFeel.NativeStyle
end;

function TcxLookAndFeel.GetDefaultRenderMode: TdxRenderMode;
begin
  if FIsRootLookAndFeel then
    Result := rmDefault
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.RenderMode
  else if FRootLookAndFeel <> nil then
    Result := FRootLookAndFeel.RenderMode
  else
    Result := rmDefault;
end;

function TcxLookAndFeel.GetDefaultSkinName: string;
begin
  if FIsRootLookAndFeel then
    Result := cxDefaultLookAndFeelSkinName
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.SkinName
  else if FRootLookAndFeel = nil then
    Result := cxDefaultLookAndFeelSkinName
  else
    Result := FRootLookAndFeel.SkinName
end;

function TcxLookAndFeel.GetDefaultSkinPainter: TcxCustomLookAndFeelPainter;
begin
  if FIsRootLookAndFeel then
    Result := nil
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.GetSkinPainter(FMasterLookAndFeel.SkinName)
  else if FRootLookAndFeel = nil then
    Result := nil
  else
    Result := FRootLookAndFeel.GetSkinPainter(FRootLookAndFeel.SkinName);
end;

function TcxLookAndFeel.GetDefaultScrollbarMode: TdxScrollbarMode;
begin
  if FIsRootLookAndFeel then
    Result := sbmDefault
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.ScrollbarMode
  else if FRootLookAndFeel = nil then
    Result := sbmDefault
  else
    Result := FRootLookAndFeel.ScrollbarMode
end;

function TcxLookAndFeel.GetDefaultScrollMode: TdxScrollMode;
begin
  if FIsRootLookAndFeel then
    Result := scmDefault
  else if FMasterLookAndFeel <> nil then
    Result := FMasterLookAndFeel.ScrollMode
  else if FRootLookAndFeel = nil then
    Result := scmDefault
  else
    Result := FRootLookAndFeel.ScrollMode
end;

function TcxLookAndFeel.IsKindStored: Boolean;
begin
  Result := lfvKind in FAssignedValues;
end;

function TcxLookAndFeel.IsNativeStyleStored: Boolean;
begin
  Result := lfvNativeStyle in FAssignedValues;
end;

function TcxLookAndFeel.IsRenderModeStored: Boolean;
begin
  Result := lfvRenderMode in FAssignedValues;
end;

function TcxLookAndFeel.IsScrollbarModeStored: Boolean;
begin
  Result := lfvScrollbarMode in FAssignedValues;
end;

function TcxLookAndFeel.IsScrollModeStored: Boolean;
begin
  Result := lfvScrollMode in FAssignedValues;
end;

function TcxLookAndFeel.IsSkinNameStored: Boolean;
begin
  Result := lfvSkinName in FAssignedValues;
end;

procedure TcxLookAndFeel.SaveState;
begin
  FPrevState.Kind := Kind;
  FPrevState.NativeStyle := NativeStyle;
  FPrevState.SkinName := SkinName;
  FPrevState.ScrollbarMode := ScrollbarMode;
  FPrevState.ScrollMode := ScrollMode;
  FPrevState.RenderMode := RenderMode;
end;

{ TcxRootLookAndFeel }

constructor TcxRootLookAndFeel.Create;
begin
  FIsRootLookAndFeel := True;
  inherited Create(nil);
end;

{ TcxLookAndFeelController }

constructor TcxLookAndFeelController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootLookAndFeel.AddChangeListener(Self);
end;

destructor TcxLookAndFeelController.Destroy;
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.RemoveChangeListener(Self);
  inherited Destroy;
end;

procedure TcxLookAndFeelController.BeginUpdate;
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.BeginUpdate;
end;

procedure TcxLookAndFeelController.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TouchScrollUIMode', ReadTouchScrollUIMode, nil, False);
end;

procedure TcxLookAndFeelController.EndUpdate;
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.EndUpdate;
end;

procedure TcxLookAndFeelController.Reset;
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.Reset;
end;

function TcxLookAndFeelController.GetKind: TcxLookAndFeelKind;
begin
  if IsRootLookAndFeelAssigned then
    Result := RootLookAndFeel.Kind
  else
    Result := cxDefaultLookAndFeelKind
end;

function TcxLookAndFeelController.GetNativeStyle: Boolean;
begin
  if IsRootLookAndFeelAssigned then
    Result := RootLookAndFeel.NativeStyle
  else
    Result := cxDefaultLookAndFeelNativeStyle
end;

function TcxLookAndFeelController.GetRenderMode: TdxRenderMode;
begin
  if IsRootLookAndFeelAssigned then
    Result := RootLookAndFeel.RenderMode
  else
    Result := cxDefaultLookAndFeelRenderMode;
end;

function TcxLookAndFeelController.GetTouchScrollUIMode: Boolean;
begin
  Result := cxIsTouchScrollUIModeEnabled;
end;

function TcxLookAndFeelController.GetSkinName: TdxSkinName;
begin
  if IsRootLookAndFeelAssigned then
    Result := RootLookAndFeel.SkinName
  else
    Result := cxDefaultLookAndFeelSkinName;
end;

function TcxLookAndFeelController.GetTouchMode: Boolean;
begin
  Result := cxIsTouchModeEnabled;
end;

function TcxLookAndFeelController.GetScrollbarMode: TdxScrollbarMode;
begin
  if IsRootLookAndFeelAssigned then
    Result := RootLookAndFeel.ScrollbarMode
  else
    Result := sbmDefault;
end;

function TcxLookAndFeelController.GetScrollMode: TdxScrollMode;
begin
  if IsRootLookAndFeelAssigned then
    Result := RootLookAndFeel.ScrollMode
  else
    Result := cxDefaultLookAndFeelScrollMode
end;

function TcxLookAndFeelController.IsSkinNameStored: Boolean;
begin
  Result := SkinName <> '';
end;

procedure TcxLookAndFeelController.SetKind(Value: TcxLookAndFeelKind);
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.Kind := Value;
end;

procedure TcxLookAndFeelController.SetNativeStyle(Value: Boolean);
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.NativeStyle := Value;
end;

procedure TcxLookAndFeelController.SetRenderMode(AValue: TdxRenderMode);
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.RenderMode := AValue;
end;

procedure TcxLookAndFeelController.SetSkinName(const Value: TdxSkinName);
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.SkinName := Value;
end;

procedure TcxLookAndFeelController.SetTouchMode(Value: Boolean);
begin
  if Value <> cxIsTouchModeEnabled then
  begin
    cxIsTouchSettingsChanging := True;
    cxIsTouchModeEnabled := Value;
    RootLookAndFeel.Refresh;
    cxIsTouchSettingsChanging := False;
  end;
end;

procedure TcxLookAndFeelController.SetScrollbarMode
  (const Value: TdxScrollbarMode);
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.ScrollbarMode := Value;
end;

procedure TcxLookAndFeelController.SetScrollMode(const Value: TdxScrollMode);
begin
  if IsRootLookAndFeelAssigned then
    RootLookAndFeel.ScrollMode := Value;
end;

procedure TcxLookAndFeelController.ReadTouchScrollUIMode(AReader: TReader);
begin
  TouchScrollUIMode := AReader.ReadBoolean;
end;

procedure TcxLookAndFeelController.Modified;
begin
  SetDesignerModified(Self);
end;

function TcxLookAndFeelController.GetObject: TObject;
begin
  Result := Self;
end;

procedure TcxLookAndFeelController.MasterLookAndFeelChanged
  (Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  Modified;
end;

procedure TcxLookAndFeelController.MasterLookAndFeelDestroying
  (Sender: TcxLookAndFeel);
begin
  Modified;
end;

procedure TcxLookAndFeelController.SetTouchScrollUIMode(const Value: Boolean);
begin
  if Value <> TouchScrollUIMode then
  begin
    if Value then
      ScrollbarMode := sbmTouch
    else
      ScrollbarMode := sbmClassic;
  end;
end;

{ TcxSystemPaletteChangedNotifier }

constructor TcxSystemPaletteChangedNotifier.Create(AIsPrimary: Boolean = False);
begin
  inherited Create;
  FIsPrimary := AIsPrimary;
  AddRefSystemPaletteChangedListener;
  FSystemPaletteChangedListener.AddNotifier(Self, AIsPrimary);
end;

destructor TcxSystemPaletteChangedNotifier.Destroy;
begin
  FSystemPaletteChangedListener.RemoveNotifier(Self, FIsPrimary);
  ReleaseRefSystemPaletteChangedListener;
  inherited Destroy;
end;

procedure TcxSystemPaletteChangedNotifier.DoChanged;
begin
  if Assigned(FOnSystemPaletteChanged) then
    FOnSystemPaletteChanged;
end;

function ClassSupportsSkinHandler(AClass: TPersistent): Boolean;
var
  AHelper: IdxSkinSupport2;
begin
  if Supports(AClass, IdxSkinSupport2, AHelper) then
    Result := AHelper.IsSkinnable
  else
    Result := Supports(AClass, IdxSkinSupport);
end;

{ TdxPadding }

class operator TdxPadding.Equal(const L, R: TdxPadding): Boolean;
begin
  Result := (L.Left = R.Left) and (L.Top = R.Top) and (L.Right = R.Right) and
    (L.Bottom = R.Bottom);
end;

class operator TdxPadding.NotEqual(const L, R: TdxPadding): Boolean;
begin
  Result := not(L = R);
end;

function TdxPadding.GetHeight: Integer;
begin
  Result := Top + Bottom;
end;

function TdxPadding.GetWidth: Integer;
begin
  Result := Left + Right;
end;

class operator TdxPadding.Implicit(const AValue: TRect): TdxPadding;
begin
  Result.Left := AValue.Left;
  Result.Top := AValue.Top;
  Result.Right := AValue.Right;
  Result.Bottom := AValue.Bottom;
end;

class operator TdxPadding.Implicit(const AValue: TdxPadding): TRect;
begin
  Result.Left := AValue.Left;
  Result.Top := AValue.Top;
  Result.Right := AValue.Right;
  Result.Bottom := AValue.Bottom;
end;

procedure TdxPadding.Deflate(var R: TRect; AScaleFactor: TdxScaleFactor;
  ARightToLeft: Boolean = False);
begin
  Inc(R.Top, AScaleFactor.Apply(Top));
  Dec(R.Bottom, AScaleFactor.Apply(Bottom));
  if ARightToLeft then
  begin
    Inc(R.Left, AScaleFactor.Apply(Right));
    Dec(R.Right, AScaleFactor.Apply(Left));
  end
  else
  begin
    Inc(R.Left, AScaleFactor.Apply(Left));
    Dec(R.Right, AScaleFactor.Apply(Right));
  end;
end;

procedure TdxPadding.Deflate(var R: TRect; ARightToLeft: Boolean = False);
begin
  Inc(R.Top, Top);
  Dec(R.Bottom, Bottom);
  if ARightToLeft then
  begin
    Inc(R.Left, Right);
    Dec(R.Right, Left);
  end
  else
  begin
    Inc(R.Left, Left);
    Dec(R.Right, Right);
  end;
end;

procedure TdxPadding.Inflate(var R: TRect; AScaleFactor: TdxScaleFactor;
  ARightToLeft: Boolean = False);
begin
  Dec(R.Top, AScaleFactor.Apply(Top));
  Inc(R.Bottom, AScaleFactor.Apply(Bottom));
  if ARightToLeft then
  begin
    Dec(R.Left, AScaleFactor.Apply(Right));
    Inc(R.Right, AScaleFactor.Apply(Left));
  end
  else
  begin
    Dec(R.Left, AScaleFactor.Apply(Left));
    Inc(R.Right, AScaleFactor.Apply(Right));
  end;
end;

procedure TdxPadding.Inflate(var R: TRect; ARightToLeft: Boolean = False);
begin
  Dec(R.Top, Top);
  Inc(R.Bottom, Bottom);
  if ARightToLeft then
  begin
    Dec(R.Left, Right);
    Inc(R.Right, Left);
  end
  else
  begin
    Dec(R.Left, Left);
    Inc(R.Right, Right);
  end;
end;

procedure TdxPadding.InflatePadding(var APadding: TRect;
  AScaleFactor: TdxScaleFactor; ARightToLeft: Boolean = False);
begin
  Inc(APadding.Top, AScaleFactor.Apply(Top));
  Inc(APadding.Bottom, AScaleFactor.Apply(Bottom));
  if ARightToLeft then
  begin
    Inc(APadding.Left, AScaleFactor.Apply(Right));
    Inc(APadding.Right, AScaleFactor.Apply(Left));
  end
  else
  begin
    Inc(APadding.Left, AScaleFactor.Apply(Left));
    Inc(APadding.Right, AScaleFactor.Apply(Right));
  end;
end;

procedure TdxPadding.InflatePadding(var APadding: TRect;
  ARightToLeft: Boolean = False);
begin
  Inc(APadding.Top, Top);
  Inc(APadding.Bottom, Bottom);
  if ARightToLeft then
  begin
    Inc(APadding.Left, Right);
    Inc(APadding.Right, Left);
  end
  else
  begin
    Inc(APadding.Left, Left);
    Inc(APadding.Right, Right);
  end;
end;

function TdxPadding.ScaledHeight(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(Top) + AScaleFactor.Apply(Bottom);
end;

function TdxPadding.ScaledWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(Left) + AScaleFactor.Apply(Right);
end;

{ TdxVisualRefinements }

class procedure TdxVisualRefinements.AddListener(AListener: TObject);
begin
  if Supports(AListener, IdxVisualRefinementsListener) then
    FListeners.Add(AListener);
end;

class procedure TdxVisualRefinements.Changed;
var
  I: Integer;
  AIntf: IdxVisualRefinementsListener;
begin
  for I := FListeners.Count - 1 downto 0 do
    if Supports(FListeners[I], IdxVisualRefinementsListener, AIntf) then
      AIntf.Changed;
end;

class procedure TdxVisualRefinements.Finalize;
begin
  FreeAndNil(FListeners);
end;

class procedure TdxVisualRefinements.Initialize;
begin
  FListeners := TdxFastObjectList.Create(False);
  FPadding := TRect.Null;
  FHeaderPadding := TRect.Null;
end;

class procedure TdxVisualRefinements.RemoveListener(AListener: TObject);
begin
  if FListeners <> nil then
    FListeners.Remove(AListener);
end;

class procedure TdxVisualRefinements.SetPadding(const AValue: TdxPadding);
begin
  if FPadding <> AValue then
  begin
    FPadding := AValue;
    Changed;
  end;
end;

class procedure TdxVisualRefinements.SetHeaderPadding(const AValue: TdxPadding);
begin
  if FHeaderPadding <> AValue then
  begin
    FHeaderPadding := AValue;
    Changed;
  end;
end;

procedure UnregisterAssistants;
begin
  FUnitIsFinalized := True;
  FreeAndNil(FRootLookAndFeel);
end;

initialization

FUnitIsFinalized := False; // D10 bug
GroupDescendentsWith(TcxLookAndFeelController, TForm);
ClassSupportsSkinProc := ClassSupportsSkinHandler;
dxUnitsLoader.AddUnit(nil, @UnregisterAssistants);
TdxVisualRefinements.Initialize;

finalization

dxUnitsLoader.RemoveUnit(@UnregisterAssistants);
TdxVisualRefinements.Finalize;

end.
