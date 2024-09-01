unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.TMSFNCCustomControl, FMX.TMSFNCHTMLText, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts, FMX.TMSFNCWebSocketClient, FMX.TMSFNCWebSocketCommon,
  FMX.ListBox, FMX.TMSFNCCustomComponent;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    VertScrollBox1: TVertScrollBox;
    ListBox1: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    TMSFNCWebsocketClient1: TTMSFNCWebsocketClient;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TMSFNCWebsocketClient1Connect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWebsocketClient1Disconnect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWebsocketClient1MessageReceived(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    counter: Integer;
    linkList: TStringList;
    procedure TextClick(Sender: TObject);
  public
    { Public declarations }
    procedure AddNewMessage(ATitle, ADescription, AUser, ASite, AURL: string);
    procedure OpenURL(AURL: string);
  end;

var
  Form4: TForm4;

implementation

uses
  {$IFDEF MSWINDOWS}
  ShellApi, Windows, FMX.Platform.Win,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF}
  System.NetEncoding, JSON, FMX.Objects;

{$R *.fmx}

procedure TForm4.AddNewMessage(ATitle, ADescription, AUser, ASite, AURL: string);
var
  pnl: TRectangle;
  usr, desc: TLabel;
  title: TText;
begin
  ListBox1.Items.Add(ASite);

  if ASite = 'stackoverflow.com' then
  begin
    Inc(counter);

    pnl := TRectangle.Create(VertScrollBox1);
    pnl.Parent := VertScrollBox1;
    pnl.Align := TAlignLayout.Top;
    pnl.Fill.Color := gcWhite;
    pnl.Height := 100;
    pnl.Position.Y := 999999;

    title := TText.Create(pnl);
    title.Height := 22;
    title.Align := TAlignLayout.Top;
    title.Margins.Left := 5;
    title.Margins.Right := 5;
    title.Font.Size := 14;
    title.Parent := pnl;
    title.TextSettings.FontColor := gcBlue;
    title.TextSettings.Font.Style := [TFontStyle.fsUnderline];
    title.TextSettings.HorzAlign := TTextAlign.Leading;
    title.TextSettings.VertAlign := TTextAlign.Leading;
    title.TextSettings.Trimming := TTextTrimming.Word;
    title.Cursor := crHandPoint;
    title.OnClick := TextClick;
    title.Text := TNetEncoding.HTML.Decode(ATitle);
    title.Tag := counter;

    usr := TLabel.Create(pnl);
    usr.Parent := pnl;
    usr.Margins.Left := 5;
    usr.Margins.Right := 5;
    usr.Text := 'post by - ' + AUser;
    usr.Align := TAlignLayout.Bottom;
    usr.TextSettings.HorzAlign := TTextAlign.Trailing;
    usr.Font.Size := 10;

    desc := TLabel.Create(pnl);
    desc.Parent := pnl;
    desc.Margins.Right := 5;
    desc.Margins.Left := 5;
    desc.Text := TNetEncoding.HTML.Decode(ADescription);
    desc.Align := TAlignLayout.Client;
    desc.TextSettings.VertAlign := TTextAlign.Leading;
    desc.TextSettings.Trimming := TTextTrimming.Word;
    desc.WordWrap := True;
    desc.Font.Size := 10;

    linkList.Add(IntToStr(counter) + '=' + AURL);

    VertScrollBox1.ScrollBy(0, -100);
  end;
  ListBox1.ScrollToItem(ListBox1.ItemByIndex(ListBox1.Count - 1));
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  if Button1.Text = 'Connect' then
  begin
    Button1.Text := 'Disconnect';
    TMSFNCWebsocketClient1.Connect;
  end
  else
  begin
    Button1.Text := 'Connect';
    //Server does not reply with a close frame for some reason
    //So just force disconnect
    TMSFNCWebsocketClient1.Disconnect(False);
  end;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //"Abort" connection
  TMSFNCWebsocketClient1.Disconnect(False);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  counter := 0;
  linkList := TStringList.Create;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  linkList.Free;
end;

procedure TForm4.OpenURL(AURL: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(FmxHandleToHWND(Handle), 'open', PChar(AURL), nil,  nil, SW_NORMAL);
  {$ENDIF}
  {$IFDEF POSIX}
  _system(PAnsiChar({$IFDEF LINUX}'xdg-' + {$ENDIF}'open ' + '"' + AnsiString(AURL) + '"'));
  {$ENDIF}
end;

procedure TForm4.TextClick(Sender: TObject);
var
  url: string;
begin
  url := linkList.Values[IntToStr((Sender as TText).Tag)];
  OpenURL(url);
end;

procedure TForm4.Timer1Timer(Sender: TObject);
begin
  //Keep connection alive
  //Interestingly, ping frames are not taken into account
  //So send ping message instead. Server will reply with "pong" string.
  TMSFNCWebsocketClient1.Send('ping');
end;

procedure TForm4.TMSFNCWebsocketClient1Connect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  Button1.Enabled := True;
  Timer1.Enabled := True;

  TMSFNCWebsocketClient1.Send('155-questions-active');
end;

procedure TForm4.TMSFNCWebsocketClient1Disconnect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  Button1.Enabled := True;
  Timer1.Enabled := False;
end;

procedure TForm4.TMSFNCWebsocketClient1MessageReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
var
  o, dataObj: TJSONObject;
  action, data: string;
  title, desc, usr, site, url: string;
begin
  o := TJSONObject(TJSONObject.ParseJSONValue(AMessage));
  o.TryGetValue<string>('action', action);
  if action = '155-questions-active' then
  begin
    o.TryGetValue<string>('data', data);
    dataObj := TJSONObject(TJSONObject.ParseJSONValue(data));
    dataObj.TryGetValue<string>('titleEncodedFancy', title);
    dataObj.TryGetValue<string>('bodySummary', desc);
    dataObj.TryGetValue<string>('ownerDisplayName', usr);
    dataObj.TryGetValue<string>('siteBaseHostAddress', site);
    dataObj.TryGetValue<string>('url', url);
    dataObj.Free;

    AddNewMessage(title, desc, usr, site, url);
  end;
  o.Free;
end;

end.
