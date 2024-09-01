unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Objects,
  FMX.TMSFNCWebSocketClient, FMX.TMSFNCWebSocketCommon, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.TMSFNCCustomComponent;

type
  TForm4 = class(TForm)
    sendBtn: TButton;
    footerPnl: TPanel;
    VertScrollBox1: TVertScrollBox;
    OpenDialog1: TOpenDialog;
    attachImg: TImage;
    imgLbl: TLabel;
    img: TImage;
    headerPnl: TPanel;
    usrEdt: TEdit;
    usrLbl: TLabel;
    connBtn: TButton;
    textEdt: TEdit;
    TMSFNCWebsocketClient1: TTMSFNCWebsocketClient;
    procedure sendBtnClick(Sender: TObject);
    procedure textEdtKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure attachImgClick(Sender: TObject);
    procedure imgClick(Sender: TObject);
    procedure connBtnClick(Sender: TObject);
    procedure TMSFNCWebsocketClient1Connect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWebsocketClient1Disconnect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWebsocketClient1MessageReceived(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
  private
    { Private declarations }
    FFilename: string;
    pdfLinks: TStringList;
    pdfCount: Integer;
  public
    { Public declarations }
    procedure AddMessage(AUser: string; AMessage: string; AImage: string; APDF: string; ASameUser: Boolean);
    procedure SendUserMessage;
    procedure ClearAttachedImage;
    procedure SendSocketMessage(const AUser, AMessage, AImage, APDF: string);
    procedure ShowPDFClick(Sender: TObject);
    procedure OpenPDFFile(APath: string);
    procedure DeletePDFFiles;
  end;

var
  Form4: TForm4;

implementation

uses
  {$IFDEF MSWINDOWS}
  ShellAPI, Windows, FMX.Platform.Win,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF}
  JSON, Math, IOUtils,
  FMX.MultiResBitmap, FMX.DialogService,
  FMX.TMSFNCUtils;

{$R *.fmx}

procedure TForm4.AddMessage(AUser: string; AMessage: string; AImage: string; APDF: string; ASameUser: Boolean);
var
  cr: TCalloutRectangle;
  usr, msg: TLabel;
  img: TImage;
  bmp: TFixedBitmapItem;
  strm: TFileStream;
  btn: TButton;
  tempFile, tempPdf: string;
begin
  cr := TCalloutRectangle.Create(Self);
  cr.Parent := VertScrollBox1;
  cr.Align := TAlignLayout.Top;
  cr.Height := 25;
  if ASameUser then
  begin
    cr.CalloutPosition := TCalloutPosition.Right;
    cr.Margins.Left := 100;
    cr.Margins.Right := 15;
  end
  else
  begin
    cr.CalloutPosition := TCalloutPosition.Left;
    cr.Margins.Left := 15;
    cr.Margins.Right := 100;
  end;
  cr.Margins.Top := 15;
  cr.XRadius := 10;
  cr.YRadius := 10;
  cr.Position.Y := 999999;

  usr := TLabel.Create(Self);
  usr.Parent := cr;
  usr.Align := TAlignLayout.Top;
  usr.Text := AUser;
  usr.StyledSettings := usr.StyledSettings - [TStyledSetting.Style];
  usr.TextSettings.Font.Style := [TFontStyle.fsBold];
  usr.Margins.Top := 5;
  if ASameUser then
    usr.Margins.Left := 15
  else
    usr.Margins.Left := 25;

  if AImage <> '' then
  begin
    img := TImage.Create(Self);
    img.Parent := cr;
    img.Align := TAlignLayout.Top;
    if ASameUser then
    begin
      img.Margins.Left := 15;
      img.Margins.Right := 25;
    end
    else
    begin
      img.Margins.Left := 25;
      img.Margins.Right := 15;
    end;
    img.Position.Y := 999999;
    bmp := img.MultiResBitmap.Add;
    TTMSFNCUtils.LoadBitmapFromBase64(AImage, bmp.Bitmap);
    img.Height := Min(bmp.Height, 150);

    cr.Height := cr.Height + img.Height;
  end;

  if APDF <> '' then
  begin
    tempFile := TPath.GetTempFileName;
    strm := TFileStream.Create(tempFile, fmOpenReadWrite);
    try
      TTMSFNCUtils.LoadStreamFromBase64(APDF, strm);
    finally
      strm.Free;
    end;
    tempPdf := StringReplace(tempFile, '.tmp', '.pdf', []);
    RenameFile(tempFile, tempPdf);

    btn := TButton.Create(Self);
    btn.Parent := cr;
    btn.Align := TAlignLayout.Top;
    btn.Margins.Right := 25;
    btn.Margins.Left := 15;
    btn.Text := 'Open PDF';
    Inc(pdfCount);
    btn.Tag := pdfCount;
    pdfLinks.Add(IntToStr(pdfCount) + '=' + tempPdf);
    btn.OnClick := ShowPDFClick;
    btn.Position.Y := 999999;

    cr.Height := cr.Height + btn.Height + 5;
  end;

  if AMessage <> '' then
  begin
    msg := TLabel.Create(Self);
    msg.Parent := cr;
    msg.AutoSize := True;
    msg.Align := TAlignLayout.Client;
    msg.TextSettings.VertAlign := TTextAlign.Leading;
    msg.Text := AMessage;
    msg.WordWrap := True;
    msg.Margins.Right := 25;
    if ASameUser then
      msg.Margins.Left := 15
    else
      msg.Margins.Left := 25;

    cr.Height := cr.Height + msg.Canvas.TextHeight(AMessage) + msg.Canvas.TextWidth(AMessage) / msg.Width * msg.Canvas.TextHeight(AMessage);
  end;

  VertScrollBox1.ScrollBy(0, -95);
end;

procedure TForm4.ClearAttachedImage;
begin
  if FFileName <> '' then
  begin
    FFilename := '';
    imgLbl.Text := '';
    img.Visible := False;
    footerPnl.Height := 45;
  end;
end;

procedure TForm4.connBtnClick(Sender: TObject);
begin
  if usrEdt.Text = '' then
  begin
    ShowMessage('Username cannot be empty.');
    Exit;
  end;

  connBtn.Enabled := False;
  if connBtn.Text = 'Connect' then
  begin
    usrEdt.Enabled := False;
    Application.ProcessMessages;
    TMSFNCWebsocketClient1.Connect;
  end
  else
  begin
    Application.ProcessMessages;
    TMSFNCWebsocketClient1.Disconnect;
  end;
end;

procedure TForm4.DeletePDFFiles;
var
  I: Integer;
  s: string;
begin
  for I := 0 to pdfLinks.Count - 1 do
  begin
    s := pdfLinks.ValueFromIndex[I];
    if FileExists(s) then
      DeleteFile(PWideChar(s));
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FFilename := '';
  pdfLinks := TStringList.Create;
  pdfCount := 0;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  DeletePDFFiles;
  pdfLinks.Free;
end;

procedure TForm4.imgClick(Sender: TObject);
begin
  ClearAttachedImage;
end;

procedure TForm4.OpenPDFFile(APath: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(FmxHandleToHWND(Handle), nil, PChar(APath), nil,  nil, SW_SHOWNORMAL);
  {$ENDIF}
  {$IFDEF POSIX}
  _system(PAnsiChar({$IFDEF LINUX}'xdg-' + {$ENDIF}'open ' + '"' + AnsiString(APath) + '"'));
  {$ENDIF}
end;

procedure TForm4.attachImgClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FFilename := OpenDialog1.FileName;
    imgLbl.Text := ExtractFileName(FFileName);
    img.Visible := True;
    footerPnl.Height := 70;
  end;
end;

procedure TForm4.sendBtnClick(Sender: TObject);
begin
  if not sendBtn.Enabled then
    Exit;

  try
    SendUserMessage;
    textEdt.Text := '';
  except
    TMSFNCWebsocketClient1Disconnect(nil, nil);
    ShowMessage('Connection to the server closed. Message could not be sent.');
  end;
end;

procedure TForm4.SendSocketMessage(const AUser, AMessage, AImage, APDF: string);
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  o.AddPair('user', AUser);
  o.AddPair('message', AMessage);
  o.AddPair('image', AImage);
  o.AddPair('pdf', APDF);
  o.AddPair('filename', ExtractFileName(FFileName));
  TMSFNCWebsocketClient1.Send(o.ToJSON);
  o.Free;
end;

procedure TForm4.SendUserMessage;
var
  strm: TStringStream;
  base: string;
begin
  if (FFilename = '') and (textEdt.Text = '') then
    Exit;

  base := '';
  if FFileName <> '' then
  begin
    strm := TStringStream.Create;
    try
      strm.LoadFromFile(FFilename);
      base := TTMSFNCUtils.SaveStreamToBase64(strm);
    finally
      strm.Free;
    end;
  end;

  if UpperCase(ExtractFileExt(FFileName)) = '.PDF' then
  begin
    SendSocketMessage(usrEdt.Text, textEdt.Text, '', base);
    AddMessage(usrEdt.Text, textEdt.Text, '', base, True);
  end
  else
  begin
    SendSocketMessage(usrEdt.Text, textEdt.Text, base, '');
    AddMessage(usrEdt.Text, textEdt.Text, base, '', True);
  end;

  ClearAttachedImage;
end;

procedure TForm4.ShowPDFClick(Sender: TObject);
var
  pdfPath: string;
begin
  pdfPath := pdfLinks.Values[IntToStr((Sender as TButton).Tag)];
  OpenPDFFile(pdfPath);
end;

procedure TForm4.textEdtKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
    sendBtnClick(nil);
end;

procedure TForm4.TMSFNCWebsocketClient1Connect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  sendBtn.Enabled := True;
  textEdt.Enabled := True;
  attachImg.Enabled := True;
  connBtn.Text := 'Disconnect';
  connBtn.Enabled := True;
end;

procedure TForm4.TMSFNCWebsocketClient1Disconnect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  sendBtn.Enabled := False;
  textEdt.Enabled := False;
  attachImg.Enabled := False;
  connBtn.Text := 'Connect';
  connBtn.Enabled := True;
end;

procedure TForm4.TMSFNCWebsocketClient1MessageReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
var
  o: TJSONObject;
  usr, msg, img, pdf, fn: string;
begin
  o := TJSONObject(TJSONObject.ParseJSONValue(AMessage));
  o.TryGetValue<string>('user', usr);
  o.TryGetValue<string>('message', msg);
  o.TryGetValue<string>('image', img);
  o.TryGetValue<string>('pdf', pdf);
  o.TryGetValue<string>('filename', fn);
  o.Free;

  AddMessage(usr, msg, img, pdf, False);
end;

end.
