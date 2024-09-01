unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IdContext,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdServerIOHandler, IdSSL, IdSSLOpenSSL, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, IdGlobal, FMX.StdCtrls,
  FMX.TMSFNCWhatsAppServer, FMX.TMSFNCCustomComponent, FMX.TMSFNCWebSocketServer,
  FMX.TMSFNCWebSocketCommon;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    TMSFNCWhatsAppServer1: TTMSFNCWhatsAppServer;
    procedure FormCreate(Sender: TObject);
    procedure TMSFNCWhatsAppServer1GetSSLPassword(Sender: TObject;
      var APassword: string);
    procedure TMSFNCWhatsAppServer1RawMessage(Sender: TObject; AMessage: string;
      var ABroadcast: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFNCWhatsAppServer1.VerifyToken := 'YourToken';
  TMSFNCWhatsAppServer1.CertificateFile := 'YourCertificateFile';
  TMSFNCWhatsAppServer1.CertificateKeyfile := 'YourCertificateKeyFile';
  TMSFNCWhatsAppServer1.RootCertificateFile := 'YourRootCertificateFile';
  TMSFNCWhatsAppServer1.Active := True;
end;

procedure TForm1.TMSFNCWhatsAppServer1GetSSLPassword(Sender: TObject;
  var APassword: string);
begin
  APassword := '';
end;

procedure TForm1.TMSFNCWhatsAppServer1RawMessage(Sender: TObject;
  AMessage: string; var ABroadcast: Boolean);
begin
  Memo1.Lines.Add(AMessage);
end;

end.
