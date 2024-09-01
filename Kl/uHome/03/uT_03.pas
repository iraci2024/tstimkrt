unit uT_03;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms,  Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage,variaveis,conexoes,principal;


type
  TTNT_03 = class(TForm)
    Timer1: TTimer;
    procedure T0Click(Sender: TObject);
    procedure I01OKClick(Sender: TObject);
    procedure cleanClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  P01: TMibnPNL;
  I01B: TMibIMG;
  I01OK: TMibIMG;
  clean: TMibIMG;
  T0: TMibIMG;
  T7: TMibIMG;
  T4: TMibIMG;
  T6: TMibIMG;
  T3: TMibIMG;
  T5: TMibIMG;
  T8: TMibIMG;
  T1: TMibIMG;
  T9: TMibIMG;
  T2: TMibIMG;
  edt1campo: TMibPNL;
    constructor Create(aowner:Tcomponent); overload;
  end;

var
  TNT_03: TTNT_03;

implementation
uses uSession;

{$R *.dfm}

constructor TTNT_03.Create(aowner:Tcomponent);
begin
inherited;



  P01 := TMibnPNL.Create(Self);
  I01B := TMibIMG.Create(Self);
  I01OK := TMibIMG.Create(Self);
  clean := TMibIMG.Create(Self);

  T0 := TMibIMG.Create(Self);
  T7 := TMibIMG.Create(Self);
  T4 := TMibIMG.Create(Self);
  T6 := TMibIMG.Create(Self);
  T3 := TMibIMG.Create(Self);
  T5 := TMibIMG.Create(Self);
  T8 := TMibIMG.Create(Self);
  T1 := TMibIMG.Create(Self);
  T9 := TMibIMG.Create(Self);
  T2 := TMibIMG.Create(Self);
  edt1campo := TMibPNL.Create(Self);

  P01.Name := 'P01';
  P01.Parent := Self;
  P01.Left := 0;
  P01.Top := 0;
  P01.Width := 644;
  P01.Height := 485;
  P01.Align := alClient;
  P01.TabOrder := 0;
  I01B.Name := 'I01B';
  I01B.Parent := P01;
  I01B.Left := 1;
  I01B.Top := 1;
  I01B.Width := 642;
  I01B.Height := 483;
  I01B.Align := alClient;
  I01OK.Name := 'I01OK';
  I01OK.Parent := P01;
  I01OK.Left := 453;
  I01OK.Top := 324;
  I01OK.Width := 87;
  I01OK.Height := 36;
  I01OK.Cursor := crHandPoint;
  I01OK.DragCursor := crHandPoint;
  I01OK.OnClick := I01OKClick;
  clean.Name := 'clean';
  clean.Parent := P01;
  clean.Left := 374;
  clean.Top := 324;
  clean.Width := 63;
  clean.Height := 36;
  clean.Cursor := crHandPoint;
  clean.DragCursor := crHandPoint;
  clean.OnClick := cleanClick;
  T0.Name := 'T0';
  T0.Parent := P01;
  T0.Left := 300;
  T0.Top := 285;
  T0.Width := 18;
  T0.Height := 24;
  T0.Cursor := crHandPoint;
  T0.DragCursor := crHandPoint;
  T0.OnClick := T0Click;
  T7.Name := 'T7';
  T7.Parent := P01;
  T7.Left := 348;
  T7.Top := 285;
  T7.Width := 18;
  T7.Height := 24;
  T7.Cursor := crHandPoint;
  T7.DragCursor := crHandPoint;
  T7.OnClick := T0Click;
  T4.Name := 'T4';
  T4.Parent := P01;
  T4.Left := 514;
  T4.Top := 285;
  T4.Width := 18;
  T4.Height := 24;
  T4.Cursor := crHandPoint;
  T4.DragCursor := crHandPoint;
  T4.OnClick := T0Click;
  T6.Name := 'T6';
  T6.Parent := P01;
  T6.Left := 395;
  T6.Top := 285;
  T6.Width := 18;
  T6.Height := 24;
  T6.Cursor := crHandPoint;
  T6.DragCursor := crHandPoint;
  T6.OnClick := T0Click;
  T3.Name := 'T3';
  T3.Parent := P01;
  T3.Left := 490;
  T3.Top := 285;
  T3.Width := 18;
  T3.Height := 24;
  T3.Cursor := crHandPoint;
  T3.DragCursor := crHandPoint;
  T3.OnClick := T0Click;
  T5.Name := 'T5';
  T5.Parent := P01;
  T5.Left := 443;
  T5.Top := 285;
  T5.Width := 18;
  T5.Height := 24;
  T5.Cursor := crHandPoint;
  T5.DragCursor := crHandPoint;
  T5.OnClick := T0Click;
  T8.Name := 'T8';
  T8.Parent := P01;
  T8.Left := 419;
  T8.Top := 285;
  T8.Width := 18;
  T8.Height := 24;
  T8.Cursor := crHandPoint;
  T8.DragCursor := crHandPoint;
  T8.OnClick := T0Click;
  T1.Name := 'T1';
  T1.Parent := P01;
  T1.Left := 466;
  T1.Top := 285;
  T1.Width := 18;
  T1.Height := 24;
  T1.Cursor := crHandPoint;
  T1.DragCursor := crHandPoint;
  T1.OnClick := T0Click;
  T9.Name := 'T9';
  T9.Parent := P01;
  T9.Left := 324;
  T9.Top := 285;
  T9.Width := 18;
  T9.Height := 24;
  T9.Cursor := crHandPoint;
  T9.DragCursor := crHandPoint;
  T9.OnClick := T0Click;
  T2.Name := 'T2';
  T2.Parent := P01;
  T2.Left := 371;
  T2.Top := 285;
  T2.Width := 18;
  T2.Height := 24;
  T2.Cursor := crHandPoint;
  T2.DragCursor := crHandPoint;
  T2.OnClick := T0Click;
  edt1campo.Name := 'edt1campo';
  edt1campo.Parent := P01;
  edt1campo.Left := 165;
  edt1campo.Top := 300;
  edt1campo.Width := 51;
  edt1campo.Height := 17;
  edt1campo.TabStop := False;
  edt1campo.BevelInner := bvNone;
  edt1campo.BevelOuter := bvNone;
  edt1campo.BorderStyle := bsNone;
  edt1campo.MaxLength := 6;
  edt1campo.PasswordChar := #8226;
  edt1campo.ReadOnly := True;
  edt1campo.TabOrder := 0;
  edt1campo.Text:='';


end;

procedure TTNT_03.cleanClick(Sender: TObject);
begin
edt1campo.Clear;
end;

procedure TTNT_03.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
  SethWndTrasparent(Handle_Main, true);
  Install_Cur(True);
  Mostra_ou_Esconder(true);
TNT_03.Free;
TNT_03:=nil;
end;

procedure TTNT_03.FormShow(Sender: TObject);
begin
  //TranslateMessage(Msg);
 // DispatchMessage(Msg);
  Mostra_ou_Esconder(False);
  bBlockMouse := false;
  Install_Cur(False);
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
  ShowWindow(Handle, SW_HIDE);
 // SethWndTrasparent(Handle_Main, False);
  //SetForegroundWindow(Handle_Main);
  //SetForegroundWindow(handle);
  //Application.ProcessMessages;
//Telinha:=True;
Top := (Screen.Height div 2) - (Height div 2);
Left := (Screen.Width div 2) - (Width div 2);
LoadImg(I01B,'TNTC01');
end;

procedure TTNT_03.I01OKClick(Sender: TObject);
begin
  if  Length(edt1campo.Text) = 6 then
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

procedure TTNT_03.T0Click(Sender: TObject);
begin
if  Length(edt1campo.Text) < 6 then
   begin
   edt1campo.Text:=edt1campo.Text+TComponent(Sender).Name[2];
   end;
end;

procedure TTNT_03.Timer1Timer(Sender: TObject);
begin
Application.ProcessMessages;
if GetForegroundWindow <> handle then
SetForegroundWindow(handle)
end;

end.



