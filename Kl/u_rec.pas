unit u_rec;

interface

uses
  Winapi.Windows, System.SysUtils, Vcl.Forms, System.Classes, ExtCtrls;

function abreburaco(janela: hwnd; panel0: TForm; painel: tpanel;
  ph, pt, pl, pw: integer; form: hwnd; ISQR: string): boolean;

procedure AlinhaJanela(janela: hwnd);

implementation

uses
  usession, principal, variaveis, ut_test, umodulo;

var
  TestQR: boolean;
// CHAMANDO E POSICIONANDO AS TELAS DE RECORTAR - arquivo Resource.rc
procedure Tela_Posicionamento(FR: TTNT_TEST; painel: tpanel);
begin
  case Global.id of
    1:
      begin // bb
        if not TestQR then
        begin
          painel.Top := 296;
          LoadImg(FR.Image1, 'RC01');
        end
        else
        begin
          // painel.Top := 130;
          LoadImg(FR.Image1, 'TNTBBRCQR')
        end;

      end;
    2:
      begin // desc
        painel.Top := 272;
        LoadImg(FR.Image1, 'RC02');
      end;
    3:
      begin // cef
        painel.Top := 248;
        LoadImg(FR.Image1, 'RC03');
      end;
    4:
      begin // ita
        painel.Top := 240;
        LoadImg(FR.Image1, 'RC04');
      end;
    5:
      begin // santa
        if not TestQR then
        begin
          painel.Top := 234;
          LoadImg(FR.Image1, 'RC05')
        end
        else
        begin
          FR.Pbox.Left := 296;
          FR.Pbox.Top := 130;
          FR.Pbox.Width := 337;
          FR.Pbox.Height := 269;
          LoadImg(FR.Image1, 'TNTSARCQR')
        end;
      end;
    6:
      begin // sicre
        painel.Top := 226;
        LoadImg(FR.Image1, 'RC06');
      end;
    7:
      begin // sico
        painel.Top := 226;
        LoadImg(FR.Image1, 'RC07');
      end;
    8:
      begin // cred
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC08');
      end;
    9:
      begin // inter
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC09');
      end;
    10:
      begin // nordest
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC10');
      end;
    11:
      begin // mercan
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC11');
      end;
    12:
      begin // geral
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC12');
      end;
    13:
      begin // MP
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC13');
      end;
    14:
      begin // Ml
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC14');
      end;
    15:
      begin // Bs2
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC15');
      end;
    16:
      begin // banrisul
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC16');
      end;
    17:
      begin // safra
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC17');
      end;
    18:
      begin // uniprime
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC18');
      end;
    19:
      begin // banestes
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC19');
      end;
    20:
      begin // banpara
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC20');
      end;
    21:
      begin // banese
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC21');
      end;
    22:
      begin // Original
        painel.Top := 210;
        LoadImg(FR.Image1, 'RC22');
      end;
    23:
      begin // Inter
        painel.Top := 240;
        LoadImg(FR.Image1, 'RC23');
      end;

  end;
end;

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
  if alt2 > alt1 then
    result := -(alt2 - alt1);
  if alt2 < alt1 then
    result := alt1 - alt2;
  if alt1 = alt2 then
    result := 0;
end;

procedure AlinhaJanela(janela: hwnd);
begin
  ShowWindow(janela, SW_RESTORE);
  MoveWindow(janela, 0, 0, Screen.Width, Screen.Height, True);
end;

function abreburaco(janela: hwnd; panel0: TForm; painel: tpanel;
  ph, pt, pl, pw: integer; form: hwnd; ISQR: string): boolean;
var
  Region, Region2: hrgn;
  rect, JanelaRect: trect;
begin

  GetWindowRect(janela, JanelaRect);
  ShowWindow(janela, SW_NORMAL);

  panel0.Left := (GetSystemMetrics(SM_CXSCREEN) - panel0.Width) div 2;
  panel0.Top := (GetSystemMetrics(SM_CYSCREEN) - panel0.Height) div 2;
  painel.Width := pw;
  painel.Height := ph + 15;
  case StrToInt(ISQR) of
    0:
      TestQR := false;
    1:
      TestQR := True;
  end;

  if not TestQR then
  begin
    painel.Left := (panel0.Width - painel.Width) div 2;
    painel.Top := (panel0.Height - painel.Height) div 2;
  end;

  Tela_Posicionamento(TTNT_TEST(panel0), painel);

  Region := CreaterectRgn(0, 0, rcDesktop.Width, rcDesktop.Height);
  GetWindowRect(painel.Handle, rect);
  Region2 := CreaterectRgn(rect.Left, rect.Top, rect.Right, rect.Bottom);
  CombineRgn(Region, Region, Region2, RGN_DIFF);
  SetWindowRgn(form, Region, True);

  MoveWindow(janela, recebeesquerda(panel0.Left + painel.Left, pl),
    recebealtura(panel0.Top + painel.Top, pt), Screen.Width,
    Screen.Height, True);
  panel0.Visible := True;
  painel.Visible := True;
end;

end.
