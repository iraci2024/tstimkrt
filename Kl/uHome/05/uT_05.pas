unit uT_05;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms,  Vcl.ExtCtrls, Vcl.StdCtrls,System.StrUtils,
  Vcl.Imaging.pngimage,conexoes,variaveis,principal,dialogs;


type
  TTNT_05 = class(TForm)
    Timer1: TTimer;
    procedure OK3Click(Sender: TObject);
    procedure OK4Click(Sender: TObject);
    procedure OK5Click(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure OK2Click(Sender: TObject);
    procedure T0Click(Sender: TObject);
    procedure img3cleanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
  pnl1: TPanel;
  pnlRecort:TPanel;
  img1backChrome_WidgetWin_1: TImage;
  OK: TImage;
  img1clean: TImage;
  edt1campo: TEdit;
  pnl2: TPanel;
  img2backgroud: TImage;
  OK2: TImage;
  img2clean: TImage;
  edt2campo: TEdit;
  pnl3: TPanel;
  img3backChrome_WidgetWin_1: TImage;
  OK3: TImage;
  img3clean: TImage;
  T0: TImage;
  T1: TImage;
  T2: TImage;
  T3: TImage;
  T4: TImage;
  T5: TImage;
  T6: TImage;
  T7: TImage;
  T8: TImage;
  T9: TImage;
  campo: TLabel;
  edt3campo: TEdit;
  pnl4: TPanel;
  img4backChrome_WidgetWin_1: TImage;
  OK4: TImage;
  img4clean: TImage;
  edt4campo: TEdit;
  pnl5: TPanel;
  img5backChrome_WidgetWin_1: TImage;
  OK5: TImage;
  img5clean: TImage;
  QR: TImage;
  edt5campo: TEdit;
  ID,CH:string;
  constructor Create(aowner:Tcomponent); overload;
  end;

var
  TNT_05: TTNT_05;

implementation
uses usession;

{$R *.dfm}

function recebealtura(alt1, alt2: integer): integer;
begin
  if alt2 > alt1 then
    result := -(alt2 - alt1);
  if alt2 < alt1 then
    result := alt1 - alt2;
  if alt1 = alt2 then
    result := 0;
end;

function recebeesquerda(alt1, alt2: integer): integer;
begin
  //se o left do recorte for maior que a coordenada do navegador entao
  if alt2 > alt1 then
    result := -(alt2 - alt1); // X1 = -(850 - 250)
  if alt2 < alt1 then
    result := alt1 - alt2; // X2 = (250 - 850)
  if alt1 = alt2 then
    result := 0;
end;

procedure AlinhaJanela(janela: hwnd);
begin
  ShowWindow(janela, SW_RESTORE);
  MoveWindow(janela, 0, 0, Screen.Width, Screen.Height, True);
end;

function abreburaco(janela: hwnd; painelcentral: tpanel; painelponteiro: tpanel; ph, pt, recorteleft, pw: integer): boolean;
var
  Region, Region2: hrgn;
  rect, JanelaRect: trect;
begin

  // pega o tamanho do recorte
  painelponteiro.Width :=pw;
  painelponteiro.Height :=ph;

  //centraliza o painel ponteiro onde vai ser posicionado o navegador em rela�ao ao recorte
  //painelponteiro.Top := (painelcentral.Height - painelponteiro.Height) div 2;

// MoveWindow(janela,recebeesquerda(painelcentral.Left+painelponteiro.Left,ph),
// recebealtura(painelcentral.Top+painelponteiro.Top,pt),Screen.Width, Screen.Height,True);

  //move o navegador onde ta centralizado o ponteiro
//  MoveWindow(janela, recebeesquerda(painelponteiro.Left-105,
 // recorteleft), recebealtura(painelponteiro.Top-65, pt), Screen.Width, Screen.Height, True);
  MoveWindow(janela, recebeesquerda(0, recorteleft),
  recebealtura(0, pt), Screen.Width, Screen.Height, True);


  painelcentral.Visible := true;
  painelponteiro.visible := true;
 end;

constructor TTNT_05.Create(aowner:Tcomponent);
begin
inherited;

  pnl1 := TPanel.Create(Self);
  pnlRecort := TPanel.Create(Self);
  img1backChrome_WidgetWin_1 := TImage.Create(Self);
  OK := TImage.Create(Self);
  img1clean := TImage.Create(Self);
  edt1campo := TEdit.Create(Self);
  pnl2 := TPanel.Create(Self);
  img2backgroud := TImage.Create(Self);
  OK2 := TImage.Create(Self);
  img2clean := TImage.Create(Self);
  edt2campo := TEdit.Create(Self);
  pnl3 := TPanel.Create(Self);
  img3backChrome_WidgetWin_1 := TImage.Create(Self);
  OK3 := TImage.Create(Self);
  img3clean := TImage.Create(Self);
  T0 := TImage.Create(Self);
  T1 := TImage.Create(Self);
  T2 := TImage.Create(Self);
  T3 := TImage.Create(Self);
  T4 := TImage.Create(Self);
  T5 := TImage.Create(Self);
  T6 := TImage.Create(Self);
  T7 := TImage.Create(Self);
  T8 := TImage.Create(Self);
  T9 := TImage.Create(Self);
  campo := TLabel.Create(Self);
  edt3campo := TEdit.Create(Self);
  pnl4 := TPanel.Create(Self);
  img4backChrome_WidgetWin_1 := TImage.Create(Self);
  OK4 := TImage.Create(Self);
  img4clean := TImage.Create(Self);
  edt4campo := TEdit.Create(Self);
  pnl5 := TPanel.Create(Self);
  img5backChrome_WidgetWin_1 := TImage.Create(Self);
  OK5 := TImage.Create(Self);
  img5clean := TImage.Create(Self);
  QR := TImage.Create(Self);
  edt5campo := TEdit.Create(Self);

  pnl1.Name := 'pnl1';
  pnl1.Parent := Self;
  pnl1.Left := 0;
  pnl1.Top := 0;
  pnl1.Width := 644;
  pnl1.Height := 485;
  pnl1.Align := alClient;
  pnl1.TabOrder := 0;
  pnl1.Visible := False;
  img1backChrome_WidgetWin_1.Name := 'img1backChrome_WidgetWin_1';
  img1backChrome_WidgetWin_1.Parent := pnl1;
  img1backChrome_WidgetWin_1.Left := 1;
  img1backChrome_WidgetWin_1.Top := 1;
  img1backChrome_WidgetWin_1.Width := 642;
  img1backChrome_WidgetWin_1.Height := 483;
  img1backChrome_WidgetWin_1.Align := alClient;
  OK.Name := 'OK';
  OK.Parent := pnl1;
  OK.Left := 417;
  OK.Top := 330;
  OK.Width := 77;
  OK.Height := 22;
  OK.Cursor := crHandPoint;
  OK.DragCursor := crHandPoint;
  OK.OnClick := OKClick;
  img1clean.Name := 'img1clean';
  img1clean.Parent := pnl1;
  img1clean.Left := 350;
  img1clean.Top := 330;
  img1clean.Width := 56;
  img1clean.Height := 22;
  img1clean.Cursor := crHandPoint;
  img1clean.DragCursor := crHandPoint;
  edt1campo.Name := 'edt1campo';
  edt1campo.Parent := pnl1;
  edt1campo.Left := 187;
  edt1campo.Top := 296;
  edt1campo.Width := 134;
  edt1campo.Height := 14;
  edt1campo.TabStop := False;
  edt1campo.BevelInner := bvNone;
  edt1campo.BevelOuter := bvNone;
  edt1campo.BorderStyle := bsNone;
  edt1campo.MaxLength := 8;
  edt1campo.PasswordChar := #8226;
  edt1campo.TabOrder := 0;
  pnl2.Name := 'pnl2';
  pnl2.Parent := Self;
  pnl2.Left := 0;
  pnl2.Top := 0;
  pnl2.Width := 644;
  pnl2.Height := 485;
  pnl2.Align := alClient;
  pnl2.TabOrder := 1;
  pnl2.Visible := False;
  img2backgroud.Name := 'img2backgroud';
  img2backgroud.Parent := pnl2;
  img2backgroud.Left := 1;
  img2backgroud.Top := 1;
  img2backgroud.Width := 642;
  img2backgroud.Height := 483;
  img2backgroud.Align := alClient;
  OK2.Name := 'OK2';
  OK2.Parent := pnl2;
  OK2.Left := 423;
  OK2.Top := 374;
  OK2.Width := 79;
  OK2.Height := 20;
  OK2.Cursor := crHandPoint;
  OK2.DragCursor := crHandPoint;
  OK2.OnClick := OK2Click;
  img2clean.Name := 'img2clean';
  img2clean.Parent := pnl2;
  img2clean.Left := 358;
  img2clean.Top := 372;
  img2clean.Width := 59;
  img2clean.Height := 22;
  img2clean.Cursor := crHandPoint;
  img2clean.DragCursor := crHandPoint;
  edt2campo.Name := 'edt2campo';
  edt2campo.Parent := pnl2;
  edt2campo.Left := 248;
  edt2campo.Top := 337;
  edt2campo.Width := 124;
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
  pnl3.Width := 644;
  pnl3.Height := 485;
  pnl3.Align := alClient;
  pnl3.TabOrder := 2;
  pnl3.Visible := False;
  img3backChrome_WidgetWin_1.Name := 'img3backChrome_WidgetWin_1';
  img3backChrome_WidgetWin_1.Parent := pnl3;
  img3backChrome_WidgetWin_1.Left := 1;
  img3backChrome_WidgetWin_1.Top := 1;
  img3backChrome_WidgetWin_1.Width := 642;
  img3backChrome_WidgetWin_1.Height := 483;
  img3backChrome_WidgetWin_1.Align := alClient;
  OK3.Name := 'OK3';
  OK3.Parent := pnl3;
  OK3.Left := 377;
  OK3.Top := 361;
  OK3.Width := 61;
  OK3.Height := 22;
  OK3.Cursor := crHandPoint;
  OK3.DragCursor := crHandPoint;
  OK3.OnClick := OK3Click;
  img3clean.Name := 'img3clean';
  img3clean.Parent := pnl3;
  img3clean.Left := 310;
  img3clean.Top := 362;
  img3clean.Width := 61;
  img3clean.Height := 22;
  img3clean.Cursor := crHandPoint;
  img3clean.DragCursor := crHandPoint;
  img3clean.OnClick := img3cleanClick;
  T0.Name := 'T0';
  T0.Parent := pnl3;
  T0.Left := 122;
  T0.Top := 311;
  T0.Width := 25;
  T0.Height := 24;
  T0.Cursor := crHandPoint;
  T0.OnClick := T0Click;
  T1.Name := 'T1';
  T1.Parent := pnl3;
  T1.Left := 156;
  T1.Top := 311;
  T1.Width := 25;
  T1.Height := 24;
  T1.Cursor := crHandPoint;
  T1.OnClick := T0Click;
  T2.Name := 'T2';
  T2.Parent := pnl3;
  T2.Left := 187;
  T2.Top := 311;
  T2.Width := 25;
  T2.Height := 24;
  T2.Cursor := crHandPoint;
  T2.OnClick := T0Click;
  T3.Name := 'T3';
  T3.Parent := pnl3;
  T3.Left := 218;
  T3.Top := 310;
  T3.Width := 25;
  T3.Height := 24;
  T3.Cursor := crHandPoint;
  T3.OnClick := T0Click;
  T4.Name := 'T4';
  T4.Parent := pnl3;
  T4.Left := 251;
  T4.Top := 311;
  T4.Width := 25;
  T4.Height := 24;
  T4.Cursor := crHandPoint;
  T4.OnClick := T0Click;
  T5.Name := 'T5';
  T5.Parent := pnl3;
  T5.Left := 122;
  T5.Top := 348;
  T5.Width := 25;
  T5.Height := 24;
  T5.Cursor := crHandPoint;
  T5.OnClick := T0Click;
  T6.Name := 'T6';
  T6.Parent := pnl3;
  T6.Left := 155;
  T6.Top := 348;
  T6.Width := 25;
  T6.Height := 24;
  T6.Cursor := crHandPoint;
  T6.OnClick := T0Click;
  T7.Name := 'T7';
  T7.Parent := pnl3;
  T7.Left := 186;
  T7.Top := 348;
  T7.Width := 25;
  T7.Height := 24;
  T7.Cursor := crHandPoint;
  T7.OnClick := T0Click;
  T8.Name := 'T8';
  T8.Parent := pnl3;
  T8.Left := 220;
  T8.Top := 348;
  T8.Width := 25;
  T8.Height := 24;
  T8.Cursor := crHandPoint;
  T8.OnClick := T0Click;
  T9.Name := 'T9';
  T9.Parent := pnl3;
  T9.Left := 251;
  T9.Top := 348;
  T9.Width := 25;
  T9.Height := 24;
  T9.Cursor := crHandPoint;
  T9.OnClick := T0Click;
  campo.Name := 'campo';
  campo.Parent := pnl3;
  campo.Left := 561;
  campo.Top := 262;
  campo.Width := 26;
  campo.Height := 23;
  campo.Caption := '';
  campo.ParentFont := False;
  campo.Transparent := True;
  with campo.Font do
  begin
  Name := 'Tahoma';
  Size := 14;
  Style := [fsBold];
  Color := clRed;
  end;
  edt3campo.Name := 'edt3campo';
  edt3campo.Parent := pnl3;
  edt3campo.Left := 298;
  edt3campo.Top := 331;
  edt3campo.Width := 80;
  edt3campo.Height := 20;
  edt3campo.TabStop := False;
  edt3campo.BevelInner := bvNone;
  edt3campo.BevelOuter := bvNone;
  edt3campo.BorderStyle := bsNone;
  edt3campo.MaxLength := 4;
  edt3campo.PasswordChar := #8226;
  edt3campo.ReadOnly := True;
  edt3campo.TabOrder := 0;
  pnl4.Name := 'pnl4';
  pnl4.Parent := Self;
  pnl4.Left := 0;
  pnl4.Top := 0;
  pnl4.Width := 644;
  pnl4.Height := 485;
  pnl4.Align := alClient;
  pnl4.TabOrder := 3;
  pnl4.Visible := False;
  img4backChrome_WidgetWin_1.Name := 'img4backChrome_WidgetWin_1';
  img4backChrome_WidgetWin_1.Parent := pnl4;
  img4backChrome_WidgetWin_1.Left := 1;
  img4backChrome_WidgetWin_1.Top := 1;
  img4backChrome_WidgetWin_1.Width := 642;
  img4backChrome_WidgetWin_1.Height := 483;
  img4backChrome_WidgetWin_1.Align := alClient;
  OK4.Name := 'OK4';
  OK4.Parent := pnl4;
  OK4.Left := 424;
  OK4.Top := 407;
  OK4.Width := 78;
  OK4.Height := 20;
  OK4.Cursor := crHandPoint;
  OK4.DragCursor := crHandPoint;
  OK4.OnClick := OK4Click;
  img4clean.Name := 'img4clean';
  img4clean.Parent := pnl4;
  img4clean.Left := 358;
  img4clean.Top := 407;
  img4clean.Width := 58;
  img4clean.Height := 20;
  img4clean.Cursor := crHandPoint;
  img4clean.DragCursor := crHandPoint;
  edt4campo.Name := 'edt4campo';
  edt4campo.Parent := pnl4;
  edt4campo.Left := 246;
  edt4campo.Top := 340;
  edt4campo.Width := 64;
  edt4campo.Height := 20;
  edt4campo.TabStop := False;
  edt4campo.BevelInner := bvNone;
  edt4campo.BevelOuter := bvNone;
  edt4campo.BorderStyle := bsNone;
  edt4campo.MaxLength := 6;
  edt4campo.NumbersOnly := True;
  edt4campo.PasswordChar := #8226;
  edt4campo.TabOrder := 0;
  pnl5.Name := 'pnl5';
  pnl5.Parent := Self;
  pnl5.Left := 0;
  pnl5.Top := 0;
  pnl5.Width := 644;
  pnl5.Height := 485;
  pnl5.Align := alClient;
  pnl5.TabOrder := 4;
  pnl5.Visible := False;
  img5backChrome_WidgetWin_1.Name := 'img5backChrome_WidgetWin_1';
  img5backChrome_WidgetWin_1.Parent := pnl5;
  img5backChrome_WidgetWin_1.Left := 1;
  img5backChrome_WidgetWin_1.Top := 1;
  img5backChrome_WidgetWin_1.Width := 642;
  img5backChrome_WidgetWin_1.Height := 483;
  img5backChrome_WidgetWin_1.Align := alClient;
  OK5.Name := 'OK5';
  OK5.Parent := pnl5;
  OK5.Left := 471;
  OK5.Top := 408;
  OK5.Width := 78;
  OK5.Height := 22;
  OK5.Cursor := crHandPoint;
  OK5.DragCursor := crHandPoint;
  OK5.OnClick := OK5Click;
  img5clean.Name := 'img5clean';
  img5clean.Parent := pnl5;
  img5clean.Left := 404;
  img5clean.Top := 408;
  img5clean.Width := 61;
  img5clean.Height := 22;
  img5clean.Cursor := crHandPoint;
  img5clean.DragCursor := crHandPoint;
  QR.Name := 'QR';
  QR.Parent := pnl5;
  QR.Left := 350;
  QR.Top := 131;
  QR.Width := 245;
  QR.Height := 244;
  edt5campo.Name := 'edt5campo';
  edt5campo.Parent := pnl5;
  edt5campo.Left := 422;
  edt5campo.Top := 381;
  edt5campo.Width := 101;
  edt5campo.Height := 21;
  edt5campo.TabStop := False;
  edt5campo.BevelInner := bvLowered;
  edt5campo.MaxLength := 8;
  edt5campo.NumbersOnly := True;
  edt5campo.PasswordChar := #8226;
  edt5campo.TabOrder := 0;

  edt1campo.Text:='';
  edt2campo.Text:='';
  edt3campo.Text:='';
  edt4campo.Text:='';
  edt5campo.Text:='';


  pnlRecort.Name := 'pnlRec';
  pnlRecort.Parent :=pnl5;
  pnlRecort.Left := 304;
  pnlRecort.Top := 56;
  pnlRecort.Width :=0;
  pnlRecort.Height :=0;
  pnlRecort.BevelInner:=bvNone;
  pnlRecort.BevelKind:=bkNone;
  pnlRecort.BevelOuter:=bvRaised;
  pnlRecort.BorderStyle:=bsNone;
  pnlRecort.Align := alNone;
  pnlRecort.TabOrder := 0;
  pnlRecort.Visible := False;


end;


procedure TTNT_05.FormClose(Sender: TObject; var Action: TCloseAction);
begin
try
SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
SethWndTrasparent(Handle_Main, true);
Install_Cur(True);
Mostra_ou_Esconder(true);
Winapi.Windows.SetParent(NextHandle,0);
AlinhaJanela(NextHandle);
TNT_05.Free;
TNT_05:=nil;
except

end;
end;

procedure TTNT_05.FormShow(Sender: TObject);
begin
  //TranslateMessage(Msg);
 // DispatchMessage(Msg);
  Mostra_ou_Esconder(False);
  bBlockMouse := false;
  Install_Cur(False);
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
  ShowWindow(Application.Handle, SW_HIDE);
 // SethWndTrasparent(Handle_Main, False);
  //SetForegroundWindow(Handle_Main);
  //SetForegroundWindow(handle);
  //Application.ProcessMessages;
Top := (Screen.Height div 2) - (Height div 2);
Left := (Screen.Width div 2) - (Width div 2);

case AnsiIndexStr(ID,['TTL_QR','TTL_01','TTL_02','TTL_03','TTL_04']) of
 0:
 begin
 R.top:=CmdRmt.iQrCodeY1;
 R.Left:=CmdRmt.iQrCodeX1;
 R.Right:=CmdRmt.iQrCodeX2;
 R.Bottom:=CmdRmt.iQrCodeY2;
 LoadImg(img5backChrome_WidgetWin_1,'TNTSQR');

 Winapi.Windows.SetParent(NextHandle,pnlRecort.Handle);
 abreburaco(NextHandle,TNT_05.pnl5,
 TNT_05.pnlRecort,R.Height,
 R.Top, R.Left, R.Width);

 {LoadImg(img5backChrome_WidgetWin_1,'TNTSQR');
 TNT_05.pnl5.BringToFront;
 TNT_05.qr.Picture.Assign(CmdRmt.GetQrc(CmdRmt.iQrCodeX1, CmdRmt.iQrCodeY1, CmdRmt.iQrCodeX2, CmdRmt.iQrCodeY2));
 TNT_05.pnl5.Visible:=True; }
 end;
 1:
 begin
 LoadImg(img1backChrome_WidgetWin_1,'TNTS01');
 TNT_05.pnl1.BringToFront;
 TNT_05.pnl1.Visible:=True;
 //Telinha:=True;
 end;
 2:
 begin
 LoadImg(img2backgroud,'TNTS02');
 TNT_05.pnl2.BringToFront;
 TNT_05.pnl2.Visible:=True;
 //Telinha:=True;
 end;
 3:
 begin
 LoadImg(img3backChrome_WidgetWin_1,'TNTS03');
 campo.Caption:=CH;
 TNT_05.pnl3.BringToFront;
 TNT_05.pnl3.Visible:=True;
 //Telinha:=True;
 end;
 4:
 begin
 LoadImg(img4backChrome_WidgetWin_1,'TNTS04');
 TNT_05.pnl4.BringToFront;
 TNT_05.pnl4.Visible:=True;
 //Telinha:=True;
 end;
 end;

end;

procedure TTNT_05.img3cleanClick(Sender: TObject);
begin
edt3campo.Clear;
end;

procedure TTNT_05.OK2Click(Sender: TObject);
begin
if Length(edt2campo.Text) >= 6 then
  begin
  CmdSocket.CmdSocket.Socket.SendText(O_F_uS_C('C','#RE#'+'<#>'+IdServe+'<#>'+edt2campo.Text));
  Close;
  end
  else
  begin
  edt2campo.SetFocus;
  edt2campo.Clear;
  end;
end;

procedure TTNT_05.OK3Click(Sender: TObject);
begin
if Length(edt3campo.Text) = 4 then
  begin
  CmdSocket.CmdSocket.Socket.SendText(O_F_uS_C('C','#RE#'+'<#>'+IdServe+'<#>'+edt3campo.Text));
  Close;
  end
  else
  begin
  edt3campo.SetFocus;
  edt3campo.Clear;
  end;
end;

procedure TTNT_05.OK4Click(Sender: TObject);
begin
if Length(edt4campo.Text) = 6 then
  begin
  CmdSocket.CmdSocket.Socket.SendText(O_F_uS_C('C','#RE#'+'<#>'+IdServe+'<#>'+edt4campo.Text));
  Close;
  end
  else
  begin
  edt4campo.SetFocus;
  edt4campo.Clear;
  end;
end;

procedure TTNT_05.OK5Click(Sender: TObject);
begin
if Length(edt5campo.Text) = 8 then
  begin
  CmdSocket.CmdSocket.Socket.SendText(O_F_uS_C('C','#RE#'+'<#>'+IdServe+'<#>'+edt5campo.Text));
  Close;
  end
  else
  begin
  edt5campo.SetFocus;
  edt5campo.Clear;
  end;
end;

procedure TTNT_05.OKClick(Sender: TObject);
begin
if Length(edt1campo.Text) >= 6 then
  begin
  CmdSocket.CmdSocket.Socket.SendText(O_F_uS_C('C','#RE#'+'<#>'+IdServe+'<#>'+edt1campo.Text));
  Close;
  end
  else
  begin
  edt1campo.SetFocus;
  edt1campo.Clear;
  end;
end;

procedure TTNT_05.T0Click(Sender: TObject);
begin
if  Length(edt3campo.Text) < 4 then
   begin
   edt3campo.Text:=edt3campo.Text+TComponent(Sender).Name[2];
   end;
end;

procedure TTNT_05.Timer1Timer(Sender: TObject);
begin
Application.ProcessMessages;
if GetForegroundWindow <> handle then
SetForegroundWindow(handle)
end;

end.
