unit UMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.TMSFNCWhatsAppReceiver,
  Vcl.StdCtrls, WEBLib.StdCtrls, VCL.TMSFNCCustomComponent,
  VCL.TMSFNCWebSocketClient, VCL.TMSFNCWhatsAppReceiver,
  VCL.TMSFNCWebSocketCommon;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    WebMemo1: TWebMemo;
    TMSFNCWhatsAppReceiver1: TTMSFNCWhatsAppReceiver;
    procedure WebButton1Click(Sender: TObject);
    procedure TMSFNCWhatsAppReceiver1Connect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWhatsAppReceiver1Disconnect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWhatsAppReceiver1WhatsAppMessageReceived(Sender: TObject;
      AMessage: TTMSFNCWhatsAppReceiverMessage);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TMSFNCWhatsAppReceiver1Connect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  WebButton1.Enabled := False;
  WebMemo1.Lines.Add('Connected');
end;

procedure TForm1.TMSFNCWhatsAppReceiver1Disconnect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  WebButton1.Enabled := True;
end;
procedure TForm1.TMSFNCWhatsAppReceiver1WhatsAppMessageReceived(Sender: TObject;
  AMessage: TTMSFNCWhatsAppReceiverMessage);
begin
  case AMessage.MessageType of
    wamtText:
    begin
      WebMemo1.Lines.Add('Received text message:');
      WebMemo1.Lines.Add(AMessage.Text.Body);
    end;
    wamtImage: WebMemo1.Lines.Add('Received an image.');
    wamtDocument: WebMemo1.Lines.Add('Received a document.');
    wamtContacts: WebMemo1.Lines.Add('Received a contact.');
    wamtAudio: WebMemo1.Lines.Add('Received an audio.');
    wamtLocation: WebMemo1.Lines.Add('Received a location.');
    wamtVideo: WebMemo1.Lines.Add('Received a video.');
    wamtSticker: WebMemo1.Lines.Add('Received a sticker.');
  end;
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  TMSFNCWhatsAppReceiver1.Connect;
end;

end.