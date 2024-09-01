unit ut_test;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  variaveis, conexoes, principal;

type
  TTNT_TEST = class(TForm)
    Image1: TImage;
    Pepita: TPanel;
    Pbox: TPanel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  TNT_TEST: TTNT_TEST;

implementation

uses
  usession, u_rec, umodulo;

{$R *.dfm}

procedure TTNT_TEST.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // if vFlagScree=1 then
  // begin
  // SethWndTrasparent(Handle_Main, true);
  // DisablePeek;
  // end;
  UniInstal_lock;
  AlinhaJanela(hwdPai);
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
    GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
  Install_Cur(True);
  Mostra_ou_Esconder(True);
  TNT_TEST.Free;
  TNT_TEST := nil;
  TranslateMessage(Msg);
  DispatchMessage(Msg);
end;

procedure TTNT_TEST.FormShow(Sender: TObject);
begin
  Mostra_ou_Esconder(False);
  Instal_Lock;
  bBlockMouse := False;
  Install_Cur(False);
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main,
    GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
  // if vFlagScree=1 then
  // SethWndTrasparent(Handle_Main, false);
  // TranslateMessage(Msg);
  // DispatchMessage(Msg);
end;

procedure TTNT_TEST.Timer1Timer(Sender: TObject);
begin
  SetWindowPos(Handle_Main, Hwnd_TopMost, 0, 0, 0, 0, Swp_NoMove or Swp_NoSize);
end;

end.
