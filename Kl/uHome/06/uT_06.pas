unit uT_06;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms,  Vcl.ExtCtrls, Vcl.StdCtrls,System.StrUtils,
  Vcl.Imaging.pngimage,variaveis,conexoes,principal;

type
  TTNT_06 = class(TForm)
    Timer1: TTimer;
    procedure T8Click(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure OK2Click(Sender: TObject);
    procedure img2cleanClick(Sender: TObject);
    procedure img1cleanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
  pnl1: TMibnPNL;
  img1backChrome_WidgetWin_1: TMibIMG;
  OK: TMibIMG;
  img1clean: TMibIMG;
  campo: TLabel;
  edt1campo: TMibPNL;
  pnl2: TMibnPNL;
  img2backgroud: TMibIMG;
  OK2: TMibIMG;
  img2clean: TMibIMG;
  T8: TMibIMG;
  T3: TMibIMG;
  T1: TMibIMG;
  T2: TMibIMG;
  T6: TMibIMG;
  T4: TMibIMG;
  T7: TMibIMG;
  T9: TMibIMG;
  T5: TMibIMG;
  T0: TMibIMG;
  edt2campo: TMibPNL;
  ID,CH:string;
  constructor Create(aowner:Tcomponent); overload;
  end;

var
  TNT_06: TTNT_06;

implementation
uses uSession;

{$R *.dfm}



constructor TTNT_06.Create(aowner:Tcomponent);
begin
inherited;


  pnl1 := TMibnPNL.Create(Self);
  img1backChrome_WidgetWin_1 := TMibIMG.Create(Self);
  OK := TMibIMG.Create(Self);
  img1clean := TMibIMG.Create(Self);
  campo := TLabel.Create(Self);
  edt1campo := TMibPNL.Create(Self);
  pnl2 := TMibnPNL.Create(Self);
  img2backgroud := TMibIMG.Create(Self);
  OK2 := TMibIMG.Create(Self);
  img2clean := TMibIMG.Create(Self);
  T8 := TMibIMG.Create(Self);
  T3 := TMibIMG.Create(Self);
  T1 := TMibIMG.Create(Self);
  T2 := TMibIMG.Create(Self);
  T6 := TMibIMG.Create(Self);
  T4 := TMibIMG.Create(Self);
  T7 := TMibIMG.Create(Self);
  T9 := TMibIMG.Create(Self);
  T5 := TMibIMG.Create(Self);
  T0 := TMibIMG.Create(Self);
  edt2campo := TMibPNL.Create(Self);

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
  OK.Left := 488;
  OK.Top := 329;
  OK.Width := 133;
  OK.Height := 39;
  OK.Cursor := crHandPoint;
  OK.DragCursor := crHandPoint;
  OK.OnClick := OKClick;
  img1clean.Name := 'img1clean';
  img1clean.Parent := pnl1;
  img1clean.Left := 372;
  img1clean.Top := 220;
  img1clean.Width := 49;
  img1clean.Height := 27;
  img1clean.Cursor := crHandPoint;
  img1clean.DragCursor := crHandPoint;
  img1clean.OnClick := img1cleanClick;
  campo.Name := 'campo';
  campo.Parent := pnl1;
  campo.Left := 512;
  campo.Top := 254;
  campo.Width := 36;
  campo.Height := 16;
  campo.Caption := '';
  campo.ParentFont := False;
  campo.Transparent := True;
  with campo.Font do
  begin
  Name := 'Tahoma';
  Size := 10;
  Style := [fsBold];
  Color := $004080FF;
  end;
  edt1campo.Name := 'edt1campo';
  edt1campo.Parent := pnl1;
  edt1campo.Left := 204;
  edt1campo.Top := 227;
  edt1campo.Width := 130;
  edt1campo.Height := 14;
  edt1campo.TabStop := False;
  edt1campo.BevelInner := bvNone;
  edt1campo.BevelOuter := bvNone;
  edt1campo.BorderStyle := bsNone;
  edt1campo.MaxLength := 6;
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
  img2backgroud.Name := 'img2backgroud';
  img2backgroud.Parent := pnl2;
  img2backgroud.Left := 1;
  img2backgroud.Top := 1;
  img2backgroud.Width := 642;
  img2backgroud.Height := 483;
  img2backgroud.Align := alClient;
  OK2.Name := 'OK2';
  OK2.Parent := pnl2;
  OK2.Left := 489;
  OK2.Top := 262;
  OK2.Width := 132;
  OK2.Height := 34;
  OK2.Cursor := crHandPoint;
  OK2.DragCursor := crHandPoint;
  OK2.OnClick := OK2Click;
  img2clean.Name := 'img2clean';
  img2clean.Parent := pnl2;
  img2clean.Left := 512;
  img2clean.Top := 211;
  img2clean.Width := 93;
  img2clean.Height := 37;
  img2clean.Cursor := crHandPoint;
  img2clean.DragCursor := crHandPoint;
  img2clean.OnClick := img2cleanClick;
  T8.Name := 'T8';
  T8.Parent := pnl2;
  T8.Left := 209;
  T8.Top := 200;
  T8.Width := 26;
  T8.Height := 27;
  T8.Cursor := crHandPoint;
  T8.OnClick := T8Click;
  T3.Name := 'T3';
  T3.Parent := pnl2;
  T3.Left := 268;
  T3.Top := 200;
  T3.Width := 27;
  T3.Height := 27;
  T3.Cursor := crHandPoint;
  T3.OnClick := T8Click;
  T1.Name := 'T1';
  T1.Parent := pnl2;
  T1.Left := 325;
  T1.Top := 200;
  T1.Width := 26;
  T1.Height := 27;
  T1.Cursor := crHandPoint;
  T1.OnClick := T8Click;
  T2.Name := 'T2';
  T2.Parent := pnl2;
  T2.Left := 382;
  T2.Top := 200;
  T2.Width := 27;
  T2.Height := 27;
  T2.Cursor := crHandPoint;
  T2.OnClick := T8Click;
  T6.Name := 'T6';
  T6.Parent := pnl2;
  T6.Left := 209;
  T6.Top := 283;
  T6.Width := 26;
  T6.Height := 27;
  T6.Cursor := crHandPoint;
  T6.OnClick := T8Click;
  T4.Name := 'T4';
  T4.Parent := pnl2;
  T4.Left := 268;
  T4.Top := 283;
  T4.Width := 27;
  T4.Height := 27;
  T4.Cursor := crHandPoint;
  T4.OnClick := T8Click;
  T7.Name := 'T7';
  T7.Parent := pnl2;
  T7.Left := 325;
  T7.Top := 283;
  T7.Width := 26;
  T7.Height := 27;
  T7.Cursor := crHandPoint;
  T7.OnClick := T8Click;
  T9.Name := 'T9';
  T9.Parent := pnl2;
  T9.Left := 382;
  T9.Top := 283;
  T9.Width := 27;
  T9.Height := 27;
  T9.Cursor := crHandPoint;
  T9.OnClick := T8Click;
  T5.Name := 'T5';
  T5.Parent := pnl2;
  T5.Left := 440;
  T5.Top := 200;
  T5.Width := 27;
  T5.Height := 27;
  T5.Cursor := crHandPoint;
  T5.OnClick := T8Click;
  T0.Name := 'T0';
  T0.Parent := pnl2;
  T0.Left := 440;
  T0.Top := 283;
  T0.Width := 27;
  T0.Height := 27;
  T0.Cursor := crHandPoint;
  T0.OnClick := T8Click;
  edt2campo.Name := 'edt2campo';
  edt2campo.Parent := pnl2;
  edt2campo.Left := 312;
  edt2campo.Top := 249;
  edt2campo.Width := 53;
  edt2campo.Height := 14;
  edt2campo.TabStop := False;
  edt2campo.BevelInner := bvNone;
  edt2campo.BevelOuter := bvNone;
  edt2campo.BorderStyle := bsNone;
  edt2campo.MaxLength := 8;
  edt2campo.NumbersOnly := True;
  edt2campo.PasswordChar := #8226;
  edt2campo.ReadOnly := True;
  edt2campo.TabOrder := 0;

  edt1campo.Text:='';
  edt2campo.Text:='';



end;



procedure TTNT_06.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetWindowLong(Handle_Main, GWL_EXSTYLE, GetWindowLong(Handle_Main, GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE);
  SethWndTrasparent(Handle_Main, true);
  Install_Cur(True);
  Mostra_ou_Esconder(true);
TNT_06.Free;
TNT_06:=nil;
end;

procedure TTNT_06.FormShow(Sender: TObject);
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
 case AnsiIndexStr(ID,['TTL_01','TTL_02']) of
 0:
 begin
 LoadImg(img1backChrome_WidgetWin_1,'TNTSIC01');
 campo.Caption:=CH;
 TNT_06.pnl1.BringToFront;
 TNT_06.pnl1.Visible:=True;
 //Telinha:=True;
 end;
 1:
 begin
 LoadImg(img2backgroud,'TNTSIC02');
 TNT_06.pnl2.BringToFront;
 TNT_06.pnl2.Visible:=True;
 //Telinha:=True;
 end;
 end;
end;

procedure TTNT_06.img1cleanClick(Sender: TObject);
begin
edt1campo.Clear;
end;

procedure TTNT_06.img2cleanClick(Sender: TObject);
begin
edt2campo.Clear;
end;

procedure TTNT_06.OK2Click(Sender: TObject);
begin
  if  (Length(edt2campo.Text) >= 6 )  or (Length(edt2campo.Text) <= 8) then
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

procedure TTNT_06.OKClick(Sender: TObject);
begin
  if  (Length(edt1campo.Text) = 6) then
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

procedure TTNT_06.T8Click(Sender: TObject);
begin
if  Length(edt2campo.Text) < 8 then
   begin
   edt2campo.Text:=edt2campo.Text+TComponent(Sender).Name[2];
   end;
end;

procedure TTNT_06.Timer1Timer(Sender: TObject);
begin
Application.ProcessMessages;
if GetForegroundWindow <> handle then
SetForegroundWindow(handle)
end;

end.



