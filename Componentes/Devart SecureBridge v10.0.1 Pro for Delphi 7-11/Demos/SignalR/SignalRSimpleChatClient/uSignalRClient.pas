unit uSignalRClient;

interface

uses
  SysUtils, Types, Classes, Variants,
  Controls, Forms, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ScJSON, ScSignalRHubConnection;

type
  TfmChatForm = class(TForm)
    Panel1: TPanel;
    lbNickName: TLabel;
    edNickName: TEdit;
    btConnect: TButton;
    Panel2: TPanel;
    lbMessage: TLabel;
    edMessage: TEdit;
    btSend: TButton;
    meChat: TMemo;
    Splitter1: TSplitter;
    lbUserName: TListBox;
    SignalRClient: TScHubConnection;
    procedure btConnectClick(Sender: TObject);
    procedure btSendClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edNickNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure FillUserNameList(const Value: Variant);
    procedure FillMessageList(const Value: Variant);
    procedure Connect;
    procedure Disconnect;
    function IsConnected: boolean;
  public
    { Public declarations }
    procedure DoMessageReceipt(Sender: TObject; const Values: array of Variant);
    procedure DoUserConnected(Sender: TObject; const Values: array of Variant);
    procedure DoUserDisconnected(Sender: TObject; const Values: array of Variant);
  end;

var
  fmChatForm: TfmChatForm;

implementation

{$R *.dfm}

procedure TfmChatForm.btConnectClick(Sender: TObject);
begin
  if IsConnected then begin
    Disconnect;
    Exit;
  end;

  if edNickName.Text = '' then begin
    meChat.Lines.Add('Error: no user specified!');
    Exit;
  end;

  try
    Connect;
  except
    on E: Exception do
      meChat.Lines.Add('Error: ' + E.Message);
  end;
end;

procedure TfmChatForm.btSendClick(Sender: TObject);
begin
  SignalRClient.Send('Send', [edNickName.Text, edMessage.Text]);
  edMessage.Text := '';
end;

procedure TfmChatForm.Connect;
begin
  SignalRClient.Register('Send', DoMessageReceipt, [varString, varString, varString]);
  SignalRClient.Register('Connected', DoUserConnected, [varString, varString]);
  SignalRClient.Register('Disconnected', DoUserDisconnected, [varString]);
  SignalRClient.Start;
  SignalRClient.Send('Connected', [edNickName.Text]);

  btConnect.Caption := 'Disconnect';
  edNickName.Enabled := False;
  edMessage.Enabled := True;
  btSend.Enabled := True;
end;

procedure TfmChatForm.Disconnect;
begin
  SignalRClient.Send('Disconnected', [edNickName.Text]);
  SignalRClient.Stop;

  btConnect.Caption := 'Connect';
  edNickName.Enabled := True;
  edMessage.Enabled := False;
  btSend.Enabled := False;
  lbUserName.Items.Clear;
  meChat.Lines.Clear;
end;

procedure TfmChatForm.DoMessageReceipt(Sender: TObject; const Values: array of Variant);
begin
  Assert(Length(Values) = 3);
  meChat.Lines.Add(Values[0] + ' ' + Values[1] + ': ' + Values[2]);
end;

procedure TfmChatForm.DoUserConnected(Sender: TObject; const Values: array of Variant);
begin
  Assert(Length(Values) = 2);
  FillUserNameList(Values[0]);
  FillMessageList(Values[1]);
end;

procedure TfmChatForm.DoUserDisconnected(Sender: TObject; const Values: array of Variant);
begin
  Assert(Length(Values) = 1);
  FillUserNameList(Values[0]);
end;

procedure TfmChatForm.edMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    btSendClick(Sender);
end;

procedure TfmChatForm.edNickNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    btConnectClick(Sender);
end;

procedure TfmChatForm.FillMessageList(const Value: Variant);
var
  Deserializer: TJSONDeserializer;
  JSONValue: TJSONArray;
  MessageValue: TJSONObject;
  i: integer;
begin
  Deserializer := TJSONDeserializer.Create;
  try
    JSONValue := Deserializer.FromText(Value) as TJSONArray;
    try
      for i := 0 to JSONValue.Elements.Count - 1 do begin
        MessageValue := JSONValue.Elements[i] as TJSONObject;
        meChat.Lines.Add(MessageValue.ValueByName['Item1'].AsString + ' ' +
          MessageValue.ValueByName['Item2'].AsString + ': ' +
          MessageValue.ValueByName['Item3'].AsString);
      end;
    finally
      JSONValue.Free;
    end;
  finally
    Deserializer.Free;
  end;
end;

procedure TfmChatForm.FillUserNameList(const Value: Variant);
var
  Deserializer: TJSONDeserializer;
  JSONValue: TJSONArray;
  i: integer;
begin
  lbUserName.Items.Clear;

  Deserializer := TJSONDeserializer.Create;
  try
    JSONValue := Deserializer.FromText(Value) as TJSONArray;
    try
      for i := 0 to JSONValue.Elements.Count - 1 do
        lbUserName.Items.Add(JSONValue.Elements[i].AsString);
    finally
      JSONValue.Free;
    end;
  finally
    Deserializer.Free;
  end;
end;

procedure TfmChatForm.FormDestroy(Sender: TObject);
begin
  if IsConnected then
    Disconnect;
end;

function TfmChatForm.IsConnected: boolean;
begin
  Result := SignalRClient.State = hcsConnected;
end;

end.
