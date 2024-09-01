unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.TMSFNCWhatsAppReceiver, FMX.TMSFNCWebSocketCommon,
  FMX.TMSFNCCustomComponent, FMX.TMSFNCWebSocketClient;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    TMSFNCWhatsAppReceiver1: TTMSFNCWhatsAppReceiver;
    procedure Button1Click(Sender: TObject);
    procedure TMSFNCWhatsAppReceiver1Connect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWhatsAppReceiver1WhatsAppMessageReceived(Sender: TObject;
      AMessage: TTMSFNCWhatsAppReceiverMessage);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  TMSFNCWhatsAppReceiver1.Connect;
  Button1.Enabled := False;
end;

procedure TForm2.TMSFNCWhatsAppReceiver1Connect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  Memo1.Lines.Add('Connected');
end;

procedure TForm2.TMSFNCWhatsAppReceiver1WhatsAppMessageReceived(Sender: TObject;
  AMessage: TTMSFNCWhatsAppReceiverMessage);
begin
  case AMessage.MessageType of
    wamtText:
    begin
      Memo1.Lines.Add('Received text message:');
      Memo1.Lines.Add(AMessage.Text.Body);
    end;
    wamtImage: Memo1.Lines.Add('Received an image.');
    wamtDocument: Memo1.Lines.Add('Received a document.');
    wamtContacts: Memo1.Lines.Add('Received a contact.');
    wamtAudio: Memo1.Lines.Add('Received an audio.');
    wamtLocation: Memo1.Lines.Add('Received a location.');
    wamtVideo: Memo1.Lines.Add('Received a video.');
    wamtSticker: Memo1.Lines.Add('Received a sticker.');
  end;
end;

end.
