{***************************************************************************}
{ TAdvStyleIF interface                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2022                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvStyleIF;

interface

{$I TMSDEFS.INC}

uses
  Classes, Registry, SysUtils, Windows, Messages, Controls, Forms, Graphics
  {$IFDEF FREEWARE}
  , TMSTrial
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  , VCL.Themes, System.UITypes
  {$ENDIF}
  ;

const
  //TMSMETROFONT = 'Segoe UI';

  WM_OFFICETHEMECHANGED  = WM_USER + 1969;

  WM_THEMECHANGED = $031A;
  {$IFDEF DELPHI2006_LVL}
  {$EXTERNALSYM WM_THEMECHANGED}
  {$ENDIF}

//Need to redeclare the API function - instead of BOOL is uses DWORD.
function RegNotifyChangeKeyValue(hKey: HKEY; bWatchSubtree: DWORD; dwNotifyFilter: DWORD; hEvent: THandle; fAsynchronus: DWORD): Longint; stdcall;
  external 'advapi32.dll' name 'RegNotifyChangeKeyValue';
{$EXTERNALSYM RegNotifyChangeKeyValue}

type
  TWindowsVersion = (winxp, win7, win8, win10);
  TMetroStyle = (msLight, msDark);

  TTMSStyle = (tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsOffice2003Classic,
    tsOffice2007Luna, tsOffice2007Obsidian, tsWindowsXP, tsWhidbey, tsCustom, tsOffice2007Silver, tsWindowsVista,
    tsWindows7, tsTerminal, tsOffice2010Blue, tsOffice2010Silver, tsOffice2010Black, tsWindows8,
    tsOffice2013White, tsOffice2013LightGray, tsOffice2013Gray,
    tsWindows10,
    tsOffice2016White, tsOffice2016Gray, tsOffice2016Black,
    tsOffice2019White, tsOffice2019Gray, tsOffice2019Black);

  TColorTone = record
    BrushColor: TColor;
    BorderColor: TColor;
    TextColor: TColor;
  end;

  TColorTones = record
    Background: TColorTone;
    Foreground: TColorTone;
    Selected: TColorTone; // =Down
    Hover: TColorTone;
    Disabled: TColorTone;
  end;

  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

  TOfficeVersion = (ov2003, ov2007, ov2010, ov2013, ov2016);

  TOfficeTheme = (ot2003Blue,ot2003Silver,ot2003Olive,ot2003Classic,ot2007Blue,ot2007Silver,ot2007Black,ot2010Blue,ot2010Silver,ot2010Black,ot2013White,ot2013Silver,ot2013Gray,ot2016White,ot2016Gray,ot2016Black,ot2019White,ot2019Gray,ot2019Black,otUnknown);

  ITMSTones = interface
  ['{1F492643-6699-4F25-8B34-3233FA735036}']
     procedure SetColorTones(ATones: TColorTones);
  end;

  ITMSStyle = interface
  ['{11AC2DDC-C087-4298-AB6E-EA1B5017511B}']
    procedure SetComponentStyle(AStyle: TTMSStyle);
  end;

  ITMSStyleEx = interface(ITMSStyle)
  ['{037BA87F-7CBD-4FDD-854E-2B3F0BCC06AE}']
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
  end;

  ITMSMetro = interface
  ['{A7E8D091-0327-446D-83D6-7069760B3320}']
    function IsMetro: boolean;
  end;

  THandleList = class(TList)
  private
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer):Integer;
  public
    constructor Create;
    procedure DeleteValue(Value: Integer);
    function HasValue(Value: Integer): Boolean;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: Integer);
    procedure Insert(Index,Value: Integer);
    procedure Delete(Index: Integer);
  end;

  TRegMonitorThread = class(TThread)
  private
    FReg: TRegistry;
    FEvent: THandle;
    FKey: string;
    FRootKey: HKey;
    FWatchSub: boolean;
    FFilter: integer;
    FWnd: THandle;
    FWinList: THandleList;
    procedure InitThread;
    procedure SetFilter(const Value: integer);
    function GetFilter: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop;
    property Key: string read FKey write FKey;
    property RootKey: HKey read FRootKey write FRootKey;
    property WatchSub: boolean read FWatchSub write FWatchSub;
    property Filter: integer read GetFilter write SetFilter;
    property Wnd: THandle read FWnd write FWnd;
    property WinList: THandleList read FWinList;
  protected
    procedure Execute; override;
  end;

  TThemeNotifierWindow = class(TWinControl)
  private
    FOnOfficeThemeChange: TNotifyEvent;
  protected
    procedure WndProc(var Msg: TMessage); override;
  published
    property OnOfficeThemeChange: TNotifyEvent read FOnOfficeThemeChange write FOnOfficeThemeChange;
  end;

  TThemeNotifier = class(TComponent)
  private
    RegMonitorThread : TRegMonitorThread;
    FNotifier: TThemeNotifierWindow;
    FOnOfficeThemeChange: TNotifyEvent;
  protected
    procedure OfficeThemeChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterWindow(Hwnd: THandle);
    procedure UnRegisterWindow(Hwnd: THandle);
  published
    property OnOfficeThemeChange: TNotifyEvent read FOnOfficeThemeChange write FOnOfficeThemeChange;
  end;

  TBalloonType = (btNone, btInfo, btWarning, btError);

function ThemeNotifier(AParent: TWinControl): TThemeNotifier;
function IsVista: boolean;
function IsWinXP: Boolean;
function GetOfficeVersion: TOfficeVersion;
function GetOfficeTheme: TOfficeTheme;
function GetWindowsTheme: TTMSStyle;
function GetOSVersion: TWindowsVersion;
function GetUIStyle(OnlyOffice: Boolean = false): TTMSStyle;
function IsThemedApp: boolean;

function GetMetroFont: string;
function DefaultMetroTones: TColorTones;
function CreateMetroTones(Light: boolean; Color, TextColor: TColor): TColorTones;
function ClearTones: TColorTones;
function IsClearTones(ATones: TColorTones): boolean;

function ChangeBrightness(Color: TColor; Perc: Integer): TColor;
function ChangeColor(Color: TColor; Perc: integer): TColor;

function CalcDPIScale(FHDC: HDC; FormScaled: boolean): single; overload;
function CalcDPIScale(Canvas: TCanvas; Form: TCustomForm = nil): single; overload;
function CalcFontSize(ACanvas: TCanvas; AFont: TFont; DPIScale: single): integer;

function GetDPIScale(AControl: TControl; ACanvas: TCanvas): single;
procedure AdaptFormHeightToVisibleControls(Form: TForm);

{$IFDEF DELPHIXE2_LVL}
function CheckVCLStylesEnabled(AStyleServices: TCustomStyleServices; ADesigning: Boolean): Boolean;
function GetVCLStyleTones: TColorTones;
{$ENDIF}

function GetDefaultStyle(AComponent: TComponent = nil; InitStyle: TTMSStyle = tsOffice2019White): TTMSStyle;
function SetParentFontForStyle(AStyle: TTMSStyle): Boolean;

procedure FillStyleList(Items: TStrings);

function FontIsEqual(Font1,Font2: TFont): boolean;

var
  ThemeNotifierInstance: TThemeNotifier;
  TMSDISABLEWITHCOLORSATURATION: boolean = true;

implementation

uses
  Dialogs, Math, AdvAppStyler;


var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;


//------------------------------------------------------------------------------

procedure AdaptFormHeightToVisibleControls(Form: TForm);
var
  i, MaxH, MinTop: Integer;
begin
  MinTop := MaxInt;
  MaxH := 0;
  for i := 0 to Form.ControlCount-1 do
  if Form.Controls[i].Visible then
   begin
     if Form.Controls[i].Top + Form.Controls[i].Height > MaxH then
       MaxH := Form.Controls[i].Top + Form.Controls[i].Height;
    if Form.Controls[i].Top<MinTop then
       MinTop := Form.Controls[i].Top;
   end;
  if MinTop = MaxInt then
    MinTop := 0;
  Form.ClientHeight := MaxH + MinTop;
end;

//------------------------------------------------------------------------------

function IsThemedApp: Boolean;
var
  i: Integer;
begin
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

function GetOfficeVersion: TOfficeVersion;
var
  reg: TRegistry;
begin
  Result := ov2003;

  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;

  if reg.KeyExists('Software\Microsoft\Office\11.0\Common\General') then
    Result := ov2003;

  if reg.KeyExists('Software\Microsoft\Office\12.0\Common\General') then
    Result := ov2007;

  if reg.KeyExists('Software\Microsoft\Office\14.0\Common\General') then
    Result := ov2010;

  if reg.KeyExists('Software\Microsoft\Office\15.0\Common\General') then
    Result := ov2013;

  if reg.KeyExists('Software\Microsoft\Office\16.0\Common\General') then
    Result := ov2016;

  reg.Free;
end;

//------------------------------------------------------------------------------

function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

//------------------------------------------------------------------------------
function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;

  Result := xpNone;

  if not IsWinXP then
    Exit;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib,'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive and IsThemedApp then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib,'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);

            GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255);

            {$IFDEF DELPHI_UNICODE}
            if StrPos('NormalColor',PWideChar(ColorScheme)) <> nil then
              Result := xpBlue
            else if StrPos('HomeStead',PWideChar(ColorScheme)) <> nil then
              Result := xpGreen
            else if StrPos('Metallic',PWideChar(ColorScheme)) <> nil then
              Result := xpGray
            else
              Result := xpNone;

            {$ENDIF}
            {$IFNDEF DELPHI_UNICODE}
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
            {$ENDIF}
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;

//------------------------------------------------------------------------------

function GetOfficeThemeValue(reg: TRegistry; key: string): integer;
const
  lTheme = 'Theme';
begin
  Result := -1;

  if reg.KeyExists(key) then
  begin
    reg.OpenKey(key, false);
    if reg.ValueExists(lTheme) then
      Result := reg.ReadInteger(lTheme);
    reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------

function GetOfficeUIThemeValue(reg: TRegistry; key: string): integer;
const
  lUITheme = 'UI Theme';
var
  i: integer;
{$IFDEF DELPHIXE_LVL}
  s: string;
  e: integer;
{$ENDIF}

begin
  Result := -1;

  if reg.KeyExists(key) then
  begin
    Result := 0;
    reg.OpenKey(key, false);
    if reg.ValueExists(lUITheme) then
    begin
      {$IFNDEF DELPHIXE_LVL}
      try
        i := reg.ReadInteger(lUITheme);
        Result := i;
      except
      end;
      {$ENDIF}

      {$IFDEF DELPHIXE_LVL}
      s := reg.GetDataAsString(lUITheme);
      val(s, i, e);
      if (e = 0)  then
        Result := i;
      {$ENDIF}
    end;
    reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------

function GetWindowsAppLightThemeValue(reg: TRegistry; key: string): integer;
const
  lUITheme = 'AppsUseLightTheme';
var
  i: integer;
{$IFDEF DELPHIXE_LVL}
  s: string;
  e: integer;
{$ENDIF}

begin
  Result := -1;

  if reg.KeyExists(key) then
  begin
    Result := 0;
    reg.OpenKey(key, false);
    if reg.ValueExists(lUITheme) then
    begin
      {$IFNDEF DELPHIXE_LVL}
      try
        i := reg.ReadInteger(lUITheme);
        Result := i;
      except
      end;
      {$ENDIF}

      {$IFDEF DELPHIXE_LVL}
      s := reg.GetDataAsString(lUITheme);
      val(s, i, e);
      if (e = 0)  then
        Result := i;
      {$ENDIF}
    end
    else
      Result := -1;
    reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------

function GetOfficeTheme: TOfficeTheme;
var
//  reg: TRegistry;
//  i: integer;
  st: TTMSStyle;
begin
  Result := otUnknown;

  st := GetUIStyle(True);

  case st of
    tsOffice2003Blue: Result := ot2003Blue;
    tsOffice2003Silver: Result := ot2003Silver;
    tsOffice2003Olive: Result := ot2003Olive;
    tsOffice2003Classic: Result := ot2003Classic;
    tsOffice2007Luna: Result := ot2007Blue;
    tsOffice2007Obsidian: Result := ot2007Black;
    tsCustom: Result := ot2019Black;
    tsOffice2007Silver: Result := ot2007Silver;
    tsOffice2010Blue: Result := ot2010Blue;
    tsOffice2010Silver: Result := ot2010Silver;
    tsOffice2010Black: Result := ot2010Black;
    tsOffice2013White: Result := ot2013White;
    tsOffice2013LightGray: Result := ot2013Silver;
    tsOffice2013Gray: Result := ot2013Gray;
    tsOffice2016White: Result := ot2016White;
    tsOffice2016Gray: Result := ot2016Gray;
    tsOffice2016Black: Result := ot2016Black;
    tsOffice2019White: Result := ot2019White;
    tsOffice2019Gray: Result := ot2019Gray;
    tsOffice2019Black: Result := ot2019Black;
  end;
end;

//------------------------------------------------------------------------------

function GetWindowsTheme: TTMSStyle;
var
  reg: TRegistry;
  i: integer;

begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;

  //Windows10 Light or Dark
  i := GetWindowsAppLightThemeValue(reg,'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize');
  if (i = 0) then
    Result := tsOffice2019Black
  else
    Result := tsWindows10;

  reg.Free;
end;


function GetOSVersion: TWindowsVersion;
{$IFDEF DELPHI_UNICODE}
type
  pfnRtlGetVersion = function(var RTL_OSVERSIONINFOEXW): DWORD; stdcall;
{$ENDIF}
type
  TOSVersionInfoEx = record
    dwOSVersionInfoSize:DWORD;
    dwMajorVersion:DWORD;
    dwMinorVersion:DWORD;
    dwBuildNumber:DWORD;
    dwPlatformId:DWORD;
    szCSDVersion: array[0..127] of Char;
    wServicePackMajor:WORD;
    wServicePackMinor:WORD;
    wSuiteMask:WORD;
    wProductType:BYTE;
    wReserved:BYTE;
  End;

var
  osVerInfo: TOSVersionInfoEx;
  majorVer, minorVer: Cardinal;

{$IFDEF DELPHI_UNICODE}
  ver: RTL_OSVERSIONINFOEXW;
  RtlGetVersion: pfnRtlGetVersion;


  procedure GetUnmanistedVersion(var majv,minv: cardinal);
  begin
    @RtlGetVersion := GetProcAddress(GetModuleHandle('ntdll.dll'), 'RtlGetVersion');
    if Assigned(RtlGetVersion) then
    begin
      ZeroMemory(@ver, SizeOf(ver));
      ver.dwOSVersionInfoSize := SizeOf(ver);

      if RtlGetVersion(ver) = 0 then
      begin
        majv := ver.dwMajorVersion;
        minv := ver.dwMinorVersion;
      end;
    end;
  end;
{$ENDIF}

begin
  Result := winxp;

  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  if GetVersionEx(Windows.POSVersionInfo(@osVerInfo)^) then
  begin
    majorVer := osVerInfo.dwMajorVersion;
    minorVer := osVerInfo.dwMinorVersion;

    {$IFDEF DELPHI_UNICODE}
    if (majorVer = 6) and (minorVer = 2) then
      GetUnmanistedVersion(majorVer, minorVer);
    {$ENDIF}

    if (majorVer = 6) and (minorVer = 1) then
      Result := win7
    else
    if (majorVer = 6) and (minorVer = 2) then
      Result := win8
    else
    if (majorVer = 6) and (minorVer = 3) then
      Result := win8
    else
    if (majorVer = 6) and (minorVer = 4) then
      Result := win10
    else
    if (majorVer = 10) then
      Result := win10;
  end;
end;

//------------------------------------------------------------------------------

function GetUIStyle(OnlyOffice: Boolean): TTMSStyle;
var
  reg: TRegistry;
  i: integer;

begin
  Result := tsCustom;

  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;

  // Office 2016 has same key as Office 2019
  i := GetOfficeUIThemeValue(reg,'Software\Microsoft\Office\16.0\Common');
  if (i <> -1) then
  begin
    case i of
    3: Result := tsOffice2019Gray;
    4: Result := tsOffice2019Black;
    else
      Result := tsOffice2019White;
    end;
  end
  else
  begin
    // Office 2013?
    i := GetOfficeUIThemeValue(reg,'Software\Microsoft\Office\15.0\Common');

    if (i <> -1) then
    begin
      case i of
      0: Result := tsOffice2013White;
      1: Result := tsOffice2013LightGray;
      2: Result := tsOffice2013Gray;
      end;
    end
    else
    begin
      // Office 2010?
      i := GetOfficeThemeValue(reg,'Software\Microsoft\Office\14.0\Common');

      if (i <> -1) then
      begin
        case i of
        1: Result := tsOffice2010Blue;
        2: Result := tsOffice2010Silver;
        3: Result := tsOffice2010Black;
        end;
      end
      else
      begin
        // Office 2007?
        i := GetOfficeThemeValue(reg,'Software\Microsoft\Office\12.0\Common');

        if (i <> -1) then
        begin
          case i of
          1: Result := tsOffice2007Luna;
          2: Result := tsOffice2007Silver;
          3: Result := tsOffice2007Obsidian;
          end;
        end
        else
        begin
          if not OnlyOffice then
          begin
            case GetOSVersion of
              win8: Result := tsWindows8;
              win10: Result := GetWindowsTheme;
              else
              begin
                Result := tsWindows7
              end;
            end;
          end
          else
          begin
            case CurrentXPTheme of
              xpNone: Result := tsOffice2003Classic;
              xpBlue: Result := tsOffice2003Blue;
              xpGreen: Result := tsOffice2003Olive;
              xpGray: Result := tsOffice2003Silver;
            end;
          end;
        end;
      end;
    end;
  end;

  reg.Free;
end;

//------------------------------------------------------------------------------


function ThemeNotifier(AParent: TWinControl): TThemeNotifier;
begin
  if not Assigned(ThemeNotifierInstance) then
  begin
    if Assigned(Application) and Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
    begin
      ThemeNotifierInstance := TThemeNotifier.Create(Application.MainForm)
    end
    else
    begin
      ThemeNotifierInstance := TThemeNotifier.Create(AParent);
    end;
  end;
  Result := ThemeNotifierInstance;
end;

//------------------------------------------------------------------------------

{ TRegMonitorThread }
constructor TRegMonitorThread.Create;
begin
  // Execute won’t be called until after Resume is called.
  inherited Create(True);
  FReg := TRegistry.Create;
  FWinList := THandleList.Create;
end;

destructor TRegMonitorThread.Destroy;
begin
  if Assigned(FWinList) then
    FreeAndNil(FWinList);
  if Assigned(FReg) then
    FreeAndNil(FReg);
  if FEvent <> 0 then
    CloseHandle(FEvent);

  inherited Destroy;
end;

procedure TRegMonitorThread.InitThread;
begin
  FReg.RootKey := RootKey;
  if not FReg.OpenKeyReadOnly(Key) then
  begin
    raise Exception.Create('Unable to open registry key ' + Key);
  end;
  FEvent := CreateEvent(nil, True, False, 'RegMonitorChange');
  RegNotifyChangeKeyValue(FReg.CurrentKey, 1, Filter, FEvent, 1);
end;

procedure TRegMonitorThread.Execute;
var
  i: integer;
begin
  InitThread;

  while not Terminated do
  begin
    if WaitForSingleObject(FEvent, INFINITE) = WAIT_OBJECT_0 then
    begin
      if Terminated then
        Exit;

      // Notify notifier Window
      if Wnd <> 0 then
        SendMessage(Wnd, WM_OFFICETHEMECHANGED, RootKey, LParam(PChar(Key)));

      for i := 0 to WinList.Count - 1 do
        SendMessage(WinList.Items[i], WM_OFFICETHEMECHANGED, RootKey, LParam(PChar(Key)));

      ResetEvent(FEvent);
      RegNotifyChangeKeyValue(FReg.CurrentKey, 1, Filter, FEvent, 1);
    end;
  end;
end;

procedure TRegMonitorThread.SetFilter(const Value: integer);
begin
  if fFilter <> Value then
    fFilter := Value;
end;

procedure TRegMonitorThread.Stop;
begin
  Terminate;
  Windows.SetEvent(FEvent);
end;

function TRegMonitorThread.GetFilter: integer;
begin
  if fFilter = 0 then
  begin
    fFilter := REG_NOTIFY_CHANGE_NAME or
      REG_NOTIFY_CHANGE_ATTRIBUTES or
      REG_NOTIFY_CHANGE_LAST_SET or
      REG_NOTIFY_CHANGE_SECURITY;
  end;
  Result := fFilter;
end;

//------------------------------------------------------------------------------

{ TNotifierWindow }

procedure TThemeNotifierWindow.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_OFFICETHEMECHANGED) then
  begin
    if Assigned(FOnOfficeThemeChange) then
    begin
      FOnOfficeThemeChange(Self);
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

{ TThemeNotifier }

constructor TThemeNotifier.Create(AOwner: TComponent);
var
  ov: TOfficeVersion;
begin
  inherited;
  RegMonitorThread := nil;

  if not (csDesigning in ComponentState) then
  begin
    ov := GetOfficeVersion;

    if (ov in [ov2007, ov2010, ov2013, ov2016]) then
    begin
      FNotifier := TThemeNotifierWindow.Create(Self);
      FNotifier.Parent := (AOwner as TWinControl);
      FNotifier.Visible := false;
      FNotifier.OnOfficeThemeChange := OfficeThemeChanged;

      RegMonitorThread := TRegMonitorThread.Create;

      with RegMonitorThread do
      begin
        FreeOnTerminate := True;
        Wnd := FNotifier.Handle;
        Filter := REG_NOTIFY_CHANGE_LAST_SET;

        case ov of
          ov2007: Key := 'Software\Microsoft\Office\12.0\Common';
          ov2010: Key := 'Software\Microsoft\Office\14.0\Common';
          ov2013: Key := 'Software\Microsoft\Office\15.0\Common';
          ov2016: Key := 'Software\Microsoft\Office\16.0\Common';
        end;

        RootKey := HKEY_CURRENT_USER;
        WatchSub := True;
        {$IFDEF DELPHI2010_LVL}
        Start;
        {$ENDIF}
        {$IFNDEF DELPHI2010_LVL}
        Resume;
        {$ENDIF}
      end;
    end;
  end;
end;

destructor TThemeNotifier.Destroy;
begin
  if Assigned(RegMonitorThread) then
    RegMonitorThread.Stop;
  ThemeNotifierInstance := nil;
  inherited;
end;

procedure TThemeNotifier.OfficeThemeChanged(Sender: TObject);
begin
  if Assigned(OnOfficeThemeChange) then
    OnOfficeThemeChange(Self);
end;

procedure TThemeNotifier.RegisterWindow(Hwnd: THandle);
begin
  if Assigned(RegMonitorThread) and not RegMonitorThread.WinList.HasValue(Hwnd) then
    RegMonitorThread.WinList.Add(Hwnd);
end;

procedure TThemeNotifier.UnRegisterWindow(Hwnd: THandle);
begin
  if Assigned(RegMonitorThread) then
  begin
    RegMonitorThread.WinList.DeleteValue(Hwnd);
  end;
end;

//------------------------------------------------------------------------------

{ THandleList }

constructor THandleList.Create;
begin
  inherited Create;
end;

procedure THandleList.SetInteger(Index:Integer;Value:Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

function THandleList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure THandleList.DeleteValue(Value: Integer);
var
  i: integer;
begin
  i := IndexOf(Pointer(Value));

  if i <> -1 then
    Delete(i);
end;

function THandleList.HasValue(Value: Integer): Boolean;
begin
  Result := IndexOf(Pointer(Value)) <> -1;
end;

procedure THandleList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));
end;

procedure THandleList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure THandleList.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Pointer(Value));
end;

function DarkenColor(Color: TColor; Perc: integer): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  r := (l AND $FF);
  g := ((l AND $FF00) shr 8) and $FF;
  b := ((l AND $FF0000) shr 16) and $FF;

  r := Min(255,Round(r * (100 - Perc)/100));
  g := Min(255,Round(g * (100 - Perc)/100));
  b := Min(255,Round(b * (100 - Perc)/100));

  Result := (b shl 16) or (g shl 8) or r;
end;

function ChangeColor(Color: TColor; Perc: integer): TColor;
begin
  Result := DarkenColor(Color, Perc);
end;

function CreateMetroTones(Light: boolean; Color, TextColor: TColor): TColorTones;
begin
  if Light then
  begin
    Result.Background.BrushColor := clWhite;
    Result.Background.TextColor := clBlack;
    Result.Background.BorderColor := Color;
  end
  else
  begin
    Result.Background.BrushColor := clBlack;
    Result.Background.TextColor := clWhite;
    Result.Background.BorderColor := Color;
  end;

  Result.Selected.BrushColor := Color;
  Result.Selected.TextColor := clWhite;
  Result.Selected.BorderColor := Color;

  Result.Disabled.BrushColor := clSilver;
  Result.Disabled.TextColor := clGray;
  Result.Disabled.BorderColor := clGray;

  Result.Foreground.BrushColor := clSilver;
  Result.Foreground.TextColor := clBlack;
  Result.Foreground.BorderColor := clSilver;

  Result.Hover.BrushColor := DarkenColor(Color,20);
  Result.Hover.TextColor := clWhite;
  Result.Hover.BorderColor := Result.Hover.BrushColor;
end;

function DefaultMetroTones: TColorTones;
begin
  Result.Background.BrushColor := clWindow;
  Result.Background.TextColor := clWindowText;
  Result.Background.BorderColor := clHighLight;

  Result.Selected.BrushColor := clHighlight;
  Result.Selected.TextColor := clHighlightText;
  Result.Selected.BorderColor := clHighLight;

  Result.Foreground.BrushColor := clSilver;
  Result.Foreground.TextColor := clBlack;
  Result.Foreground.BorderColor := clSilver;

  Result.Hover.BrushColor := clGray;
  Result.Hover.TextColor := clWhite;
  Result.Hover.BorderColor := clGray;

  Result.Disabled.BrushColor := clWindow;
  Result.Disabled.TextColor := clSilver;
  Result.Disabled.BorderColor := clSilver;
end;

function ClearTones: TColorTones;
begin
  Result.Background.BrushColor := clNone;
  Result.Background.TextColor := clNone;
  Result.Background.BorderColor := clNone;

  Result.Selected.BrushColor := clNone;
  Result.Selected.TextColor := clNone;
  Result.Selected.BorderColor := clNone;

  Result.Foreground.BrushColor := clNone;
  Result.Foreground.TextColor := clNone;
  Result.Foreground.BorderColor := clNone;

  Result.Hover.BrushColor := clNone;
  Result.Hover.TextColor := clNone;
  Result.Hover.BorderColor := clNone;

  Result.Disabled.BrushColor := clNone;
  Result.Disabled.TextColor := clNone;
  Result.Disabled.BorderColor := clNone;
end;

function IsClearTones(ATones: TColorTones): boolean;
begin
  Result := (ATones.Background.BrushColor = clNone) and
            (ATones.Background.TextColor = clNone) and
            (ATones.Background.BorderColor = clNone);
end;

function GetMetroFont: string;
begin
  if IsVista then
    Result := 'Segoe UI'
  else
    Result := 'Tahoma';
end;

procedure RGBToHSV(const R,G,B: double; var H,S,V: double);
var
  Delta: double;
  Min : double;
begin
  Min := MinValue( [R, G, B] );
  V := MaxValue( [R, G, B] );

  Delta := V - Min;

  if V = 0.0 then
    S := 0
  else
    S := Delta / V;

  if S = 0.0 then
    H := NaN
  else
  begin
    if R = V then
      H := 60.0 * (G - B) / Delta
    else
      if G = V then
        H := 120.0 + 60.0 * (B - R) / Delta
      else
        if B = V then
          H := 240.0 + 60.0 * (R - G) / Delta;
    if H < 0.0 then
      H := H + 360.0
  end;
end;

procedure HSVtoRGB(const H,S,V: double; var R,G,B: double);
var
  f : double;
  i : INTEGER;
  hTemp,p,q,t: double;
begin
  if  S = 0.0 then
  begin
    if IsNaN(H) then
    begin
      R := V;
      G := V;
      B := V
    end
    else
      raise Exception.Create('HSVtoRGB: S = 0 and H has a value');
  end
  else
  begin
    if  H = 360.0 then
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60;
    i := Trunc(hTemp);
    f := hTemp - i;

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    case i of
      0: begin R := V; G := t;  B := p  end;
      1: begin R := q; G := V; B := p  end;
      2: begin R := p; G := V; B := t   end;
      3: begin R := p; G := q; B := V  end;
      4: begin R := t;  G := p; B := V  end;
      5: begin R := V; G := p; B := q  end;
    end;
  end;
end;

function ChangeBrightness(Color: TColor; Perc: Integer): TColor;
var
  r,g,b: double;
  rb,gb,bb: integer;
  l: longint;
  h,s,v: double;
begin
  l := ColorToRGB(Color);

  r := (l AND $FF0000) shr 16;
  g := (l AND $00FF00) shr 8;
  b := (l AND $0000FF);

  RGBToHSV(R,G,B,H,S,V);

  if V < 255 then
    V :=  Max(255, V + (V * Perc/100))
  else
    S :=  S + (S * Perc/100);

  HSVToRGB(H,S,V,R,G,B);

  rb := round(r);
  gb := round(g);
  bb := round(b);

  rb := ((rb and $FF) shl 16) and $FF0000;
  gb := ((gb and $FF) shl 8) and $FF00;
  bb := (bb and $FF);

  Result := rb or gb or bb;
end;

function BrightnessColor(Col: TColor; Brightness: integer): TColor;
var
  r1,g1,b1: Integer;
begin
  Col := ColorToRGB(Col);
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,Brightness)
  else
    r1 := Round( Min(100,(100 + Brightness))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,Brightness)
  else
    g1 := Round( Min(100,(100 + Brightness))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,Brightness)
  else
    b1 := Round( Min(100,(100 + Brightness))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

function CalcFontSize(ACanvas: TCanvas; AFont: TFont; DPIScale: single): integer;
begin
  ACanvas.Font.Assign(AFont);
  ACanvas.Font.Height := Round(int(DPIScale * AFont.Height));
  Result := ACanvas.Font.Size;
end;

function CalcDPIScale(Canvas: TCanvas; Form: TCustomForm = nil): single;
var
  FHDC: HDC;
begin
  if Canvas.HandleAllocated then
    FHDC := Canvas.Handle
  else
    FHDC := 0;

  if not Assigned(Form) then
    Result := CalcDPIScale(FHDC, true)
  else
  begin
    if (Form is TForm) then
      Result := CalcDPIScale(FHDC, (Form as TForm).Scaled)
    else
      Result := CalcDPIScale(FHDC, false);
  end;
end;

function CalcDPIScale(FHDC: HDC; FormScaled: boolean): single;
var
  FDPI: integer;
  FCreatedDC: boolean;

  function FontHeightAtDpi(iDPI, iFontSize: Integer): Integer;
  var
    FTmpCanvas: TCanvas;
    FNHDC: HDC;
  begin
    FTmpCanvas := TCanvas.Create;
    try
      FNHDC := GetDC(0);
      FTmpCanvas.Handle := FNHDC;
      FTmpCanvas.Font.PixelsPerInch := iDPI; //must be set BEFORE size
      FTmpCanvas.Font.Size := iFontSize;
      Result := FTmpCanvas.TextHeight('0');
      ReleaseDC(0, FNHDC);
    finally
      FTmpCanvas.Free;
    end;
  end;

begin
  Result := 1.0;
  FCreatedDC := false;
  if FormScaled then
  begin
    if FHDC = 0 then
    begin
      FHDC := GetDC(0);
      FCreatedDC := true;
    end;
    try
      FDPI := GetDeviceCaps(FHDC, LOGPIXELSX);
      if FDPI <> 96 then
        Result := FontHeightAtDpi(FDPI, 9) / FontHeightAtDpi(96, 9);
    finally
      if FCreatedDC then
        ReleaseDC(0, FHDC);
    end;
  end;

  if Result <= 0 then
    Result := 1;
end;

function FontIsEqual(Font1,Font2: TFont): boolean;
begin
  Result := (Font1.Name = Font2.Name) and (Font1.Size = Font2.Size) and (Font1.Style = Font2.Style)  and (Font1.Charset = Font2.Charset);
end;


function GetDPIScale(AControl: TControl; ACanvas: TCanvas): single;
var
  {$IFDEF DELPHIXE10_LVL}
  frm: TCustomForm;
  {$ENDIF}
  FHDC: HDC;
begin
  {$IFDEF DELPHIXE10_LVL}
//  Result := 1;
  frm := nil;
  if Assigned(AControl) then
    frm := GetParentForm(AControl);

  if Assigned(frm) and (frm is TForm) then
  begin
    if (frm as TForm).Scaled then
      Result := TForm(frm).Monitor.PixelsPerInch / 96
    else
      Result := 1;
  end
  else if Assigned(ACanvas) and (ACanvas.HandleAllocated) then
  {$ENDIF}

  {$IFNDEF DELPHIXE10_LVL}
  if Assigned(ACanvas) and (ACanvas.HandleAllocated) then
  {$ENDIF}
  begin
    Result := GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 96;
  end
  else
  begin
    FHDC := GetDC(0);
    Result := GetDeviceCaps(FHDC, LOGPIXELSX) / 96;
    ReleaseDC(0, FHDC);
  end;


  if Result = 0 then
    Result := 1;
end;

function GetDefaultStyle(AComponent: TComponent; InitStyle: TTMSStyle): TTMSStyle;
var
  I: Integer;
  prtFrm: TCustomForm;
begin
  Result := InitStyle;

  if AComponent <> nil then
  begin
    if AComponent is TCustomForm then
    begin
      for I := 0 to AComponent.ComponentCount - 1 do
      begin
        if (AComponent.Components[i] is TAdvFormStyler) and (AComponent.Components[i] as TAdvFormStyler).Enabled and not ((AComponent.Components[i] as TAdvFormStyler).Style = tsCustom) then
        begin
          Result := (AComponent.Components[i] as TAdvFormStyler).Style;
          Break;
        end;
      end;
    end
    else if AComponent is TControl then
    begin
      prtFrm := GetParentForm((AComponent as TControl));
      if Assigned(prtFrm) then
      begin
        for I := 0 to prtFrm.ComponentCount - 1 do
        begin
          if (prtFrm.Components[i] is TAdvFormStyler) and (prtFrm.Components[i] as TAdvFormStyler).Enabled and not ((prtFrm.Components[i] as TAdvFormStyler).Style = tsCustom) then
          begin
            Result := (prtFrm.Components[i] as TAdvFormStyler).Style;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function SetParentFontForStyle(AStyle: TTMSStyle): Boolean;
begin
  if AStyle in [tsOffice2016Black, tsOffice2019Gray, tsOffice2019Black] then
    Result := False
  else
    Result := True;
end;

procedure FillStyleList(Items: TStrings);
begin
  Items.Clear;
  Items.AddObject('Windows 7', TObject(tsWindows7));
  Items.AddObject('Windows 8', TObject(tsWindows8));
  Items.AddObject('Windows 10', TObject(tsWindows10));
  Items.AddObject('Terminal', TObject(tsTerminal));
  //Items.AddObject('Office 2003 Olive', TObject(tsOffice2003Olive));
  //Items.AddObject('Office 2003 Silver', TObject(tsOffice2003Silver));
  //Items.AddObject('Office 2003 Blue', TObject(tsOffice2003Blue));
  //Items.AddObject('Office 2007 Luna', TObject(tsOffice2007Luna));
  //Items.AddObject('Office 2007 Silver', TObject(tsOffice2007Silver));
  //Items.AddObject('Office 2007 Obsidian', TObject(tsOffice2007Obsidian));
  Items.AddObject('Office 2010 Blue', TObject(tsOffice2010Blue));
  Items.AddObject('Office 2010 Silver', TObject(tsOffice2010Silver));
  Items.AddObject('Office 2010 Black', TObject(tsOffice2010Black));
  Items.AddObject('Office 2013 White', TObject(tsOffice2013White));
  Items.AddObject('Office 2013 Light gray', TObject(tsOffice2013LightGray));
  Items.AddObject('Office 2013 Gray', TObject(tsOffice2013Gray));
  Items.AddObject('Office 2016 White', TObject(tsOffice2016White));
  Items.AddObject('Office 2016 Gray', TObject(tsOffice2016Gray));
  Items.AddObject('Office 2016 Black', TObject(tsOffice2016Black));
  Items.AddObject('Office 2019 White', TObject(tsOffice2019White));
  Items.AddObject('Office 2019 Gray', TObject(tsOffice2019Gray));
  Items.AddObject('Office 2019 Black', TObject(tsOffice2019Black));
end;

{$IFDEF DELPHIXE2_LVL}
function CheckVCLStylesEnabled(AStyleServices: TCustomStyleServices; ADesigning: Boolean): Boolean;
begin
  {$IFDEF DELPHIXE13_LVL}
  Result := AStyleServices.Enabled and (not ADesigning and (AStyleServices.Name <> 'Windows')) or (ADesigning and (AStyleServices.Name <> 'Windows10') and (AStyleServices.Name <> 'Mountain_Mist'));
  {$ENDIF}
  {$IFNDEF DELPHIXE13_LVL}
  Result := AStyleServices.Enabled and (AStyleServices.Name <> 'Windows')
  {$ENDIF}

  //Except for AdvGraphicStyles
end;

function GetVCLStyleTones: TColorTones;
var
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  clr: TColor;
begin
  LStyle := StyleServices;
  Result.Background.BrushColor := clNone;

  if LStyle.Enabled and (LStyle.Name <> 'Windows') {$IFDEF DELPHIXE13_LVL} and (LStyle.Name <> 'Windows10') {$ENDIF} then
  begin
    LDetails := LStyle.GetElementDetails(tgCellNormal);

    if LStyle.GetElementColor(LDetails, ecFillColor, clr) and (clr <> clNone) then
      Result.Foreground.BrushColor := clr;

    if LStyle.GetElementColor(LDetails, ecBorderColor, clr) and (clr <> clNone) then
      Result.Foreground.BorderColor := clr;

    if LStyle.GetElementColor(LDetails, ecTextColor, clr) and (clr <> clNone) then
      Result.Foreground.TextColor := clr;

    LDetails := LStyle.GetElementDetails(tgFixedCellNormal);

    if LStyle.GetElementColor(LDetails, ecFillColor, clr) and (clr <> clNone) then
      Result.Background.BrushColor := clr;

    if LStyle.GetElementColor(LDetails, ecBorderColor, clr) and (clr <> clNone) then
      Result.Background.BorderColor := clr;

    if LStyle.GetElementColor(LDetails, ecTextColor, clr) and (clr <> clNone) then
      Result.Background.TextColor := clr;

    if Result.Background.TextColor = Result.Background.BrushColor then
      Result.Background.TextColor :=  Result.Background.BrushColor xor $FFFFFF;

    LDetails := LStyle.GetElementDetails(tgCellSelected);

    if LStyle.GetElementColor(LDetails, ecFillColor, clr) and (clr <> clNone) then
      Result.Selected.BrushColor := clr
    else
      Result.Selected.BrushColor := LStyle.GetStyleColor(TStyleColor.scButtonPressed);

    if LStyle.GetElementColor(LDetails, ecBorderColor, clr) and (clr <> clNone) then
      Result.Selected.BorderColor := clr
    else
      Result.Selected.BorderColor := LStyle.GetStyleColor(TStyleColor.scButtonPressed);

    if LStyle.GetElementColor(LDetails, ecTextColor, clr) and (clr <> clNone) then
      Result.Selected.TextColor := clr
    else
      Result.Selected.TextColor := LStyle.GetStyleFontColor(TStyleFont.sfButtonTextPressed);

    Result.Hover.BrushColor := LStyle.GetStyleColor(TStyleColor.scButtonHot);
    Result.Hover.TextColor := LStyle.GetStyleFontColor(TStyleFont.sfButtonTextHot);
    Result.Hover.BorderColor := Result.Hover.BrushColor;

    Result.Disabled.BrushColor := LStyle.GetStyleColor(TStyleColor.scButtonDisabled);
    Result.Disabled.TextColor := LStyle.GetStyleFontColor(TStyleFont.sfButtonTextDisabled);
    Result.Disabled.BorderColor := Result.Disabled.BrushColor;
  end;

end;

{$ENDIF}


initialization
  ThemeNotifierInstance  := nil;

finalization

end.

