{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressXPThemeManager }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSXPTHEMEMANAGER AND ALL }
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

unit dxThemeManager; // for internal use

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Controls, Graphics, Forms,
  Generics.Collections, Generics.Defaults,
  dxUxTheme, cxGeometry;

type
  TdxThemedObjectType = (totButton, totClock, totComboBox, totEdit,
    totExplorerBar, totHeader, totListBox, totListView, totMenu, totPage,
    totProgress, totRebar, totScrollBar, totSpin, totStartPanel, totStatus,
    totTab, totTaskBand, totTaskBar, totToolBar, totToolTip, totTrackBar,
    totTrayNotify, totTreeView, totWindow, totNavigation, totExplorerListView,
    // for Windows Vista and newer
    totTextStyle, totItemsViewListView, // for Windows Vista and newer
    totItemsViewHeader // for Windows Vista and newer
    );

  TdxThemeSizeScaleSource = (tssNoScaling, tssSystemDpi, tssThemeDpi);

  TdxThemedObjectTypes = set of TdxThemedObjectType;

  { TdxThemeChangedNotificator }

  TdxThemeChangedEvent = procedure of object;

  TdxThemeChangedNotificator = class
  private
    FOnThemeChanged: TdxThemeChangedEvent;
  protected
    procedure DoThemeChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property OnThemeChanged: TdxThemeChangedEvent read FOnThemeChanged
      write FOnThemeChanged;
  end;

function AreVisualStylesAvailable(ANeededThemedObjectType: TdxThemedObjectType)
  : Boolean; overload;
function AreVisualStylesAvailable(ANeededThemedObjectTypes
  : TdxThemedObjectTypes = []): Boolean; overload;
procedure CloseAllThemes;
function CloseTheme(AThemedObjectType: TdxThemedObjectType): HRESULT;
function GetThemeScaledPartSize(AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; APartId: Integer; AStateId: Integer;
  AScaleSource: TdxThemeSizeScaleSource = tssThemeDpi): TSize; overload;
function GetThemeScaledPartSize(AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; APartId: Integer; out ASize: TSize;
  AStateId: Integer = 0; AScaleSource: TdxThemeSizeScaleSource = tssThemeDpi)
  : Boolean; overload;
function GetThemeScaleFactor(ATheme: TdxTheme): TdxScaleFactor;
function cxGetThemeColor(AObjectType: TdxThemedObjectType;
  APartId, AStateId, APropId: Integer): TColor; overload; inline;
function cxGetThemeColor(AObjectType: TdxThemedObjectType;
  APartId, AStateId, APropId: Integer; out AColor: TColor): Boolean; overload;

function IsStandardTheme: Boolean;
function IsThemeScaled(ATheme: TdxTheme): Boolean;
function OpenTheme(AThemedObjectType: TdxThemedObjectType;
  AClassNameList: PWideChar = nil): TdxTheme; overload;
function OpenTheme(AThemedObjectType: TdxThemedObjectType; ADpi: Integer;
  AClassNameList: PWideChar = nil): TdxTheme; overload;
function OpenTheme(AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; AClassNameList: PWideChar = nil)
  : TdxTheme; overload;

function dxDrawThemeBackground(DC: HDC; AThemedObjectType: TdxThemedObjectType;
  const R: TRect; APartId, AStateId: Integer): Boolean; overload;
function dxDrawThemeBackground(DC: HDC; AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; const R: TRect; APartId, AStateId: Integer)
  : Boolean; overload;

implementation

uses
  dxCore, Classes, dxThemeConsts, dxDPIAwareUtils, StrUtils;

type

  { TdxThemeData }

  TdxThemeData = class
  protected
    ClassNameList: string;
    ScaleFactor: TdxScaleFactor;
    Theme: TdxTheme;
  public
    constructor Create(const ATheme: TdxTheme; AClassNameList: PWideChar;
      ADpi: Integer = 0);
    destructor Destroy; override;
  end;

  { TdxThemeDataManager }

  TdxThemeDataManager = class
  strict private
    FHandleMap: TDictionary<TdxTheme, TdxThemeData>;
    FThemes: TObjectDictionary<Int64, TdxThemeData>;
    FValidDpi: TList;

    function GetValidDpiList: TList;
    procedure ValidateDpiValue(var AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseAll;
    function GetThemeScaleFactor(ATheme: TdxTheme): TdxScaleFactor;
    function Open(AThemedObjectType: TdxThemedObjectType; ADpi: Integer;
      AClassNameList: PWideChar = nil): TdxTheme;
  end;

  { TdxThemeChangedEventReceiver }

  TdxThemeChangedEventReceiver = class
  strict private
    FWindowHandle: HWND;

    procedure WndProc(var Message: TMessage);
  protected
    procedure DoNotifyListeners;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  dxThemedObjectNameA: array [TdxThemedObjectType] of WideString = ('Button',
    'Clock', 'ComboBox', 'Edit', 'ExplorerBar', 'Header', 'ListBox', 'ListView',
    'Menu', 'Page', 'Progress', 'Rebar', 'ScrollBar', 'Spin', 'StartPanel',
    'Status', 'Tab', 'TaskBand', 'TaskBar', 'ToolBar', 'ToolTip', 'TrackBar',
    'TrayNotify', 'TreeView', 'Window', 'Navigation', 'Explorer::ListView',
    'TEXTSTYLE', 'ItemsView::ListView', 'ItemsView::Header');

var
  FIsGlobalThemeActive: Boolean = False;
  FThemeChangedEventReceiver: TdxThemeChangedEventReceiver = nil;
  FThemeChangedNotificatorList: TList;
  FThemeDataManager: TdxThemeDataManager;

function AreVisualStylesAvailable(ANeededThemedObjectType
  : TdxThemedObjectType): Boolean;
begin
  Result := FIsGlobalThemeActive and IsThemeLibraryLoaded and
    (OpenTheme(ANeededThemedObjectType) <> TC_NONE);
end;

function AreVisualStylesAvailable(ANeededThemedObjectTypes
  : TdxThemedObjectTypes = []): Boolean;
var
  AThemedObjectType: TdxThemedObjectType;
begin
  Result := FIsGlobalThemeActive and IsThemeLibraryLoaded;
  if Result and (ANeededThemedObjectTypes <> []) then
    for AThemedObjectType := Low(TdxThemedObjectType)
      to High(TdxThemedObjectType) do
    begin
      if (AThemedObjectType in ANeededThemedObjectTypes) and
        (OpenTheme(AThemedObjectType) = TC_NONE) then
        Exit(False);
    end;
end;

function CloseTheme(AThemedObjectType: TdxThemedObjectType): HRESULT;
begin
  if AreVisualStylesAvailable then
    Result := S_OK
  else
    Result := S_FALSE;
end;

procedure CloseAllThemes;
begin
  FThemeDataManager.CloseAll;
end;

function OpenTheme(AThemedObjectType: TdxThemedObjectType;
  AClassNameList: PWideChar = nil): TdxTheme;
begin
  Result := OpenTheme(AThemedObjectType, 0, AClassNameList);
end;

function OpenTheme(AThemedObjectType: TdxThemedObjectType; ADpi: Integer;
  AClassNameList: PWideChar = nil): TdxTheme;
begin
  if AreVisualStylesAvailable then
    Result := FThemeDataManager.Open(AThemedObjectType, ADpi, AClassNameList)
  else
    Result := 0;
end;

function OpenTheme(AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; AClassNameList: PWideChar = nil): TdxTheme;
begin
  if AScaleFactor <> nil then
    Result := OpenTheme(AThemedObjectType, AScaleFactor.TargetDPI,
      AClassNameList)
  else
    Result := OpenTheme(AThemedObjectType, AClassNameList);
end;

function GetThemeScaledPartSize(AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; APartId: Integer; AStateId: Integer;
  AScaleSource: TdxThemeSizeScaleSource = tssThemeDpi): TSize;
begin
  if not GetThemeScaledPartSize(AThemedObjectType, AScaleFactor, APartId,
    Result, AStateId, AScaleSource) then
    Result := cxNullSize;
end;

function GetThemeScaledPartSize(AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; APartId: Integer; out ASize: TSize;
  AStateId: Integer = 0;
  AScaleSource: TdxThemeSizeScaleSource = tssThemeDpi): Boolean;
var
  APartScaleFactor: TdxScaleFactor;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AThemedObjectType, AScaleFactor);
  Result := (ATheme <> 0) and (GetThemePartSize(ATheme, 0, APartId, AStateId,
    TS_TRUE, ASize) = S_OK);
  if Result then
  begin
    case AScaleSource of
      tssNoScaling:
        APartScaleFactor := dxDefaultScaleFactor;
      tssThemeDpi:
        APartScaleFactor := GetThemeScaleFactor(ATheme);
    else
      APartScaleFactor := dxSystemScaleFactor;
    end;
    ASize := AScaleFactor.Apply(ASize, APartScaleFactor);
  end;
end;

function GetThemeScaleFactor(ATheme: TdxTheme): TdxScaleFactor;
begin
  if AreVisualStylesAvailable then
    Result := FThemeDataManager.GetThemeScaleFactor(ATheme)
  else
    Result := nil;

  if Result = nil then
    Result := dxSystemScaleFactor;
end;

function cxGetThemeColor(AObjectType: TdxThemedObjectType;
  APartId, AStateId, APropId: Integer): TColor;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AObjectType);
  if (ATheme = 0) or (GetThemeColor(ATheme, APartId, AStateId, APropId, Result)
    <> S_OK) then
    Result := clDefault;
end;

function cxGetThemeColor(AObjectType: TdxThemedObjectType;
  APartId, AStateId, APropId: Integer; out AColor: TColor): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AObjectType);
  Result := (ATheme <> 0) and (GetThemeColor(ATheme, APartId, AStateId, APropId,
    AColor) = S_OK);
end;

function dxDrawThemeBackground(DC: HDC; AThemedObjectType: TdxThemedObjectType;
  const R: TRect; APartId, AStateId: Integer): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AThemedObjectType);
  Result := (ATheme <> 0) and Succeeded(DrawThemeBackground(ATheme, DC, APartId,
    AStateId, R))
end;

function dxDrawThemeBackground(DC: HDC; AThemedObjectType: TdxThemedObjectType;
  AScaleFactor: TdxScaleFactor; const R: TRect;
  APartId, AStateId: Integer): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AThemedObjectType, AScaleFactor);
  Result := (ATheme <> 0) and Succeeded(DrawThemeBackground(ATheme, DC, APartId,
    AStateId, R))
end;

function IsStandardTheme: Boolean;
const
  SZ_MAX_CHARS = 1024;
  StandardThemeFileNames: array [0 .. 1] of string = ('LUNA.MSSTYLES',
    'ROYALE.MSSTYLES');
var
  AThemeFileName: PWideChar;
  I: Integer;
  S: string;
begin
  Result := False;
  if AreVisualStylesAvailable then
  begin
    AThemeFileName := AllocMem(2 * SZ_MAX_CHARS);
    try
      if GetCurrentThemeName(AThemeFileName, SZ_MAX_CHARS, nil, 0, nil, 0) = S_OK
      then
      begin
        S := UpperCase(ExtractFileName(AThemeFileName));
        for I := 0 to High(StandardThemeFileNames) do
        begin
          if S = StandardThemeFileNames[I] then
            Exit(True);
        end;
      end;
    finally
      FreeMem(AThemeFileName);
    end;
  end;
end;

function IsThemeScaled(ATheme: TdxTheme): Boolean;
begin
  if AreVisualStylesAvailable and IsThemeScalingSupported then
    Result := FThemeDataManager.GetThemeScaleFactor(ATheme) <> nil
  else
    Result := False;
end;

{ TdxThemeData }

constructor TdxThemeData.Create(const ATheme: TdxTheme;
  AClassNameList: PWideChar; ADpi: Integer = 0);
begin
  Theme := ATheme;
  ClassNameList := AClassNameList;
  if ADpi > 0 then
  begin
    ScaleFactor := TdxScaleFactor.Create;
    ScaleFactor.Assign(ADpi, dxDefaultDPI);
  end;
end;

destructor TdxThemeData.Destroy;
begin
  CloseThemeData(Theme);
  FreeAndNil(ScaleFactor);
  inherited;
end;

{ TdxThemeDataManager }

constructor TdxThemeDataManager.Create;
begin
  FHandleMap := TDictionary<TdxTheme, TdxThemeData>.Create;
  FThemes := TObjectDictionary<Int64, TdxThemeData>.Create([doOwnsValues],
    (Ord(High(TdxThemedObjectType)) + 1) * 3);
end;

destructor TdxThemeDataManager.Destroy;
begin
  FreeAndNil(FHandleMap);
  FreeAndNil(FValidDpi);
  FreeAndNil(FThemes);
  inherited;
end;

procedure TdxThemeDataManager.CloseAll;
begin
  FreeAndNil(FValidDpi);
  FHandleMap.Clear;
  FThemes.Clear;
end;

function TdxThemeDataManager.GetThemeScaleFactor(ATheme: TdxTheme)
  : TdxScaleFactor;
var
  AData: TdxThemeData;
begin
  if FHandleMap.TryGetValue(ATheme, AData) then
    Result := AData.ScaleFactor
  else
    Result := nil;
end;

function TdxThemeDataManager.Open(AThemedObjectType: TdxThemedObjectType;
  ADpi: Integer; AClassNameList: PWideChar): TdxTheme;
var
  AData: TdxThemeData;
  AKey: Int64;
begin
  ValidateDpiValue(ADpi);

  AKey := dxMakeInt64(Ord(AThemedObjectType), ADpi);
  if FThemes.TryGetValue(AKey, AData) then
  begin
    if (AClassNameList = nil) or
      (AnsiUpperCase(AClassNameList) = AnsiUpperCase(AData.ClassNameList)) then
      Exit(AData.Theme);
    if (AClassNameList = nil) and (AData.ClassNameList <> '') then
      AClassNameList := PWideChar(AData.ClassNameList);
  end;

  if AClassNameList = nil then
    AClassNameList := PWideChar(dxThemedObjectNameA[AThemedObjectType]);

  Result := OpenThemeData(0, AClassNameList, ADpi);
  AData := TdxThemeData.Create(Result, AClassNameList, ADpi);
  FThemes.AddOrSetValue(AKey, AData);
  FHandleMap.AddOrSetValue(Result, AData);
end;

function TdxThemeDataManager.GetValidDpiList: TList;
var
  I: Integer;
begin
  if FValidDpi = nil then
  begin
    FValidDpi := TList.Create;
    FValidDpi.Capacity := Screen.MonitorCount;
    for I := 0 to Screen.MonitorCount - 1 do
      FValidDpi.Add(Pointer(dxGetMonitorDPI(Screen.Monitors[I])));
  end;
  Result := FValidDpi;
end;

procedure TdxThemeDataManager.ValidateDpiValue(var AValue: Integer);
var
  ACloserValue: Integer;
  AList: TList;
  AMinSigma: Integer;
  ASigma: Integer;
  I: Integer;
begin
  if IsThemeScalingSupported and (AValue >= dxDefaultDPI) then
  begin

    AMinSigma := MaxInt;
    ACloserValue := 0;
    AList := GetValidDpiList;
    for I := 0 to AList.Count - 1 do
    begin
      ASigma := Abs(Integer(AList.List[I]) - AValue);
      if ASigma < AMinSigma then
      begin
        ACloserValue := Integer(AList.List[I]);
        AMinSigma := ASigma;
      end;
    end;
    AValue := ACloserValue;
  end
  else
    AValue := 0;
end;

{ TdxThemeChangedNotificator }

constructor TdxThemeChangedNotificator.Create;
begin
  inherited Create;
  if IsThemeLibraryLoaded and (FThemeChangedNotificatorList <> nil) then
    FThemeChangedNotificatorList.Add(Self);
end;

destructor TdxThemeChangedNotificator.Destroy;
begin
  if IsThemeLibraryLoaded and (FThemeChangedNotificatorList <> nil) then
    FThemeChangedNotificatorList.Remove(Self);
  inherited Destroy;
end;

procedure TdxThemeChangedNotificator.DoThemeChanged;
begin
  if Assigned(FOnThemeChanged) then
    FOnThemeChanged;
end;

{ TdxThemeChangedEventReceiver }

constructor TdxThemeChangedEventReceiver.Create;
begin
  inherited Create;
  FWindowHandle := Classes.AllocateHWnd(WndProc);
end;

destructor TdxThemeChangedEventReceiver.Destroy;
begin
  FIsGlobalThemeActive := False;
  Classes.DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

procedure TdxThemeChangedEventReceiver.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_SETTINGCHANGE) then
  begin
    if IsThemeScalingSupported then
      CloseAllThemes;
  end;

  if (Message.Msg = WM_THEMECHANGED) or (Message.Msg = WM_SYSCOLORCHANGE) then
    try
      CloseAllThemes;
      DefWindowProc(FWindowHandle, Message.Msg, Message.wParam, Message.lParam);
      FIsGlobalThemeActive := IsThemeActive;
      DoNotifyListeners;
      Message.Result := 0;
    except
      Application.HandleException(Self);
    end
  else
    Message.Result := DefWindowProc(FWindowHandle, Message.Msg, Message.wParam,
      Message.lParam);
end;

procedure TdxThemeChangedEventReceiver.DoNotifyListeners;
var
  I: Integer;
begin
  if FThemeChangedNotificatorList <> nil then
  begin
    for I := 0 to FThemeChangedNotificatorList.Count - 1 do
      TdxThemeChangedNotificator(FThemeChangedNotificatorList[I])
        .DoThemeChanged;
  end;
end;

//

procedure RegisterAssistants;
begin
  if IsThemeLibraryLoaded then
  begin
    FIsGlobalThemeActive := IsThemeActive;
    FThemeDataManager := TdxThemeDataManager.Create;
    FThemeChangedNotificatorList := TList.Create;
    FThemeChangedEventReceiver := TdxThemeChangedEventReceiver.Create;
  end;
end;

procedure UnregisterAssistants;
begin
  if IsThemeLibraryLoaded then
  begin
    FreeAndNil(FThemeChangedEventReceiver);
    CloseAllThemes;
    FreeAndNil(FThemeChangedNotificatorList);
    FreeAndNil(FThemeDataManager);
  end;
end;

initialization

dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization

dxUnitsLoader.RemoveUnit(@UnregisterAssistants);

end.
