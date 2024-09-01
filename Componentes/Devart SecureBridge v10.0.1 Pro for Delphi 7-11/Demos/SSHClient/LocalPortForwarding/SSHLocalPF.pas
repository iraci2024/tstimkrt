unit SSHLocalPF;

{$I ..\Base\SBDemo.inc}
interface

uses
  {$IFDEF MSWINDOWS}WinSock,{$ELSE}Libc,{$ENDIF}
{$IFNDEF VER130}
  Types,
{$ENDIF}
  Classes, SysUtils, Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ToolWin, ComCtrls, Buttons, Grids, Spin, SyncObjs,
  ScUtils, ScTypes, DemoFrame, ScVioTcp, ScBridge, ScSSHClient, ScSSHChannel;

type
  TSSHLocalPF = class(TDemoFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    Label6: TLabel;
    edSSHUserName: TEdit;
    lbPassword: TLabel;
    edSSHPassword: TEdit;
    Label8: TLabel;
    cbPrivateKey: TComboBox;
    pnPassword: TPanel;
    pnPrivateKey: TPanel;
    ScSSHClient: TScSSHClient;
    Label4: TLabel;
    Label5: TLabel;
    edSSHHost: TEdit;
    Label3: TLabel;
    rbPassword: TRadioButton;
    rbPublicKey: TRadioButton;
    Panel12: TPanel;
    btKeyGen: TSpeedButton;
    ScFileStorage: TScFileStorage;
    ScSSHChannel: TScSSHChannel;
    Label2: TLabel;
    edDestHost: TEdit;
    Label9: TLabel;
    seDestPort: TSpinEdit;
    Label10: TLabel;
    seSourcePort: TSpinEdit;
    StringGrid: TStringGrid;
    Timer: TTimer;
    Panel9: TPanel;
    Panel4: TPanel;
    btSConnect: TSpeedButton;
    btSDisconnect: TSpeedButton;
    Panel10: TPanel;
    btImportKey: TSpeedButton;
    OpenDialog: TOpenDialog;
    seSSHPort: TSpinEdit;
    Label11: TLabel;
    Panel3: TPanel;
    btConnectSSH: TSpeedButton;
    btDisconnectSSH: TSpeedButton;
    Panel8: TPanel;
    btStart: TSpeedButton;
    btStop: TSpeedButton;
    cbRandomization: TCheckBox;
    rbKeyboardInteractive: TRadioButton;
    procedure rbPasswordClick(Sender: TObject);
    procedure edSSHUserNameChange(Sender: TObject);
    procedure cbPrivateKeyDropDown(Sender: TObject);
    procedure btConnectSSHClick(Sender: TObject);
    procedure btDisconnectSSHClick(Sender: TObject);
    procedure ScSSHClientAfterConnect(Sender: TObject);
    procedure ScSSHClientAfterDisconnect(Sender: TObject);
    procedure btKeyGenClick(Sender: TObject);
    procedure ScSSHClientServerKeyValidate(Sender: TObject;
      NewServerKey: TScKey; var Accept: Boolean);
    procedure ScSSHClientBeforeConnect(Sender: TObject);
    procedure ScSSHChannelSocketConnect(Sender: TObject; const SockAddr: PSockAddr);
    procedure ScSSHChannelSocketDisconnect(Sender: TObject; const SockAddr: PSockAddr);
    procedure btSConnectClick(Sender: TObject);
    procedure btSDisconnectClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure edDestHostChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btImportKeyClick(Sender: TObject);
    procedure ScSSHClientAuthenticationPrompt(Sender: TObject; const Name,
      Instruction: string; const Prompts: TStringDynArray;
      var Responses: TStringDynArray);
  private
    FVio: TCRVioTcp;
    FLockEvent: TCriticalSection;
    FEvStrings: TStringList;

    procedure CheckRandomize;
    procedure ShowSSHButtons(Connected: boolean);
    procedure DisconnectChannel;
    procedure DisconnectAll;
    procedure WriteLog(const Event: string; const SockAddr: PSockAddr);
    function LoadState: boolean;
    function SaveState: boolean;
    function KeyPath: string;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
  end;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Registry,
  ScSSHUtils, ScSSHSocket, SSHClientDemoForm, PromptForm;

destructor TSSHLocalPF.Destroy;
begin
  DisconnectAll;
  FEvStrings.Free;
  FLockEvent.Free;

  inherited;
end;

procedure TSSHLocalPF.Initialize;
begin
  inherited;
  FLockEvent := TCriticalSection.Create;
  FEvStrings := TStringList.Create;

  LoadState;
  edSSHHost.Text := ScSSHClient.HostName;
  seSSHPort.Value := ScSSHClient.Port;
  edSSHUserName.Text := ScSSHClient.User;

  StringGrid.Cells[0, 0] := 'Num';
  StringGrid.Cells[1, 0] := 'Status';
  StringGrid.Cells[2, 0] := 'From host';
  StringGrid.Cells[3, 0] := 'From port';
end;

procedure TSSHLocalPF.Finalize;
begin
  SaveState;

  FreeAndNil(FVio);
  inherited;
end;

function TSSHLocalPF.SaveState: boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do begin
      OpenKey(KeyPath + '\' + TSSHLocalPF.ClassName, True);
      WriteString('SSHHost', ScSSHClient.HostName);
      WriteInteger('SSHPort', ScSSHClient.Port);
      WriteString('SSHUserName', ScSSHClient.User);
      WriteString('DestHost', edDestHost.Text);
      WriteInteger('DestPort', seDestPort.Value);
      WriteInteger('SourcePort', seSourcePort.Value);
      WriteBool('Silent randomization', cbRandomization.Checked);
    end;
  finally
    Registry.Free;
  end;

  Result := True;
end;

function TSSHLocalPF.LoadState: boolean;
var
  Registry: TRegistry;
begin
  Result := False;
  Registry := TRegistry.Create;
  try
    with Registry do begin
      if OpenKey(KeyPath + '\' + TSSHLocalPF.ClassName, False) then begin
        if ValueExists('SSHHost') then
          ScSSHClient.HostName := ReadString('SSHHost');
        if ValueExists('SSHPort') then
          ScSSHClient.Port := ReadInteger('SSHPort');
        if ValueExists('SSHUserName') then
          ScSSHClient.User := ReadString('SSHUserName');
        if ValueExists('DestHost') then
          edDestHost.Text := ReadString('DestHost');
        if ValueExists('DestPort') then
          seDestPort.Value := ReadInteger('DestPort');
        if ValueExists('SourcePort') then
          seSourcePort.Value := ReadInteger('SourcePort');
        if ValueExists('Silent randomization') then
          cbRandomization.Checked := ReadBool('Silent randomization');
        Result := True;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSHLocalPF.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\SecureBridge\Demos';
end;

procedure TSSHLocalPF.CheckRandomize;
begin
  if not SSHClientForm.Randomized and not cbRandomization.Checked then begin
    SSHClientForm.Randomize;
    if not SSHClientForm.Randomized and not cbRandomization.Checked then
      raise Exception.Create('Data for the random generator has not been generated');
  end;
end;

procedure TSSHLocalPF.DisconnectAll;
begin
  DisconnectChannel;
  ScSSHClient.Disconnect;
  ShowSSHButtons(False);
end;

procedure TSSHLocalPF.ShowSSHButtons(Connected: boolean);
begin
  btConnectSSH.Enabled := not Connected;
  btDisconnectSSH.Enabled := Connected;
  btStart.Enabled := Connected;
end;

procedure TSSHLocalPF.DisconnectChannel;
begin
  try
    ScSSHChannel.Disconnect;
  finally
    btStart.Enabled := ScSSHClient.Connected;
    btStop.Enabled := False;
    btSConnect.Enabled := False;
    btSDisconnect.Enabled := False;
  end;
end;


procedure TSSHLocalPF.ScSSHClientAfterConnect(Sender: TObject);
begin
  ShowSSHButtons(True);
end;

procedure TSSHLocalPF.ScSSHClientAfterDisconnect(Sender: TObject);
begin
  ShowSSHButtons(False);
  DisconnectChannel;
end;

procedure TSSHLocalPF.btConnectSSHClick(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ScSSHClient.Connect;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TSSHLocalPF.btDisconnectSSHClick(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSSHLocalPF.rbPasswordClick(Sender: TObject);
var
  KeyboardInteractive: boolean;
begin
  KeyboardInteractive := Sender = rbKeyboardInteractive;
  pnPassword.Visible := (Sender = rbPassword) or KeyboardInteractive;
  pnPrivateKey.Visible := not pnPassword.Visible;
  lbPassword.Enabled := not KeyboardInteractive;
  edSSHPassword.Enabled := not KeyboardInteractive;
  if KeyboardInteractive then
    edSSHPassword.Color := cl3DLight
  else
    edSSHPassword.Color := clWindow;
    
  Repaint;
  DisconnectAll;
end;

procedure TSSHLocalPF.edSSHUserNameChange(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSSHLocalPF.cbPrivateKeyDropDown(Sender: TObject);
begin
  ScFileStorage.Keys.GetKeyNames(cbPrivateKey.Items);
end;

procedure TSSHLocalPF.btKeyGenClick(Sender: TObject);
begin
  CheckRandomize;

  if cbPrivateKey.Text = '' then
    cbPrivateKey.Text := 'client_key';

  SSHClientForm.GenerateClientKey(ScFileStorage, cbPrivateKey.Text);
end;

procedure TSSHLocalPF.ScSSHClientServerKeyValidate(Sender: TObject;
  NewServerKey: TScKey; var Accept: Boolean);
var
  CurHostKeyName: string;
begin
  if ScSSHClient.HostKeyName = '' then
    CurHostKeyName := ScSSHClient.HostName
  else
    CurHostKeyName := ScSSHClient.HostKeyName;

  SSHClientForm.DoServerKeyValidate(ScFileStorage, CurHostKeyName, NewServerKey, Accept);
end;

procedure TSSHLocalPF.ScSSHClientBeforeConnect(Sender: TObject);
begin
  CheckRandomize;

  ScSSHClient.HostName := edSSHHost.Text;
  ScSSHClient.Port := seSSHPort.Value;
  ScSSHClient.User := edSSHUserName.Text;

  if rbPassword.Checked then begin
    ScSSHClient.Authentication := atPassword;
    ScSSHClient.Password := edSSHPassword.Text;
  end
  else
  if rbPublicKey.Checked then begin
    ScSSHClient.Authentication := atPublicKey;
    ScSSHClient.PrivateKeyName := cbPrivateKey.Text;
    if ScFileStorage.Keys.FindKey(ScSSHClient.PrivateKeyName) = nil then
      raise EScError.Create('Private key can not be empty');
  end
  else
    ScSSHClient.Authentication := atKeyboardInteractive;
end;

procedure TSSHLocalPF.btStartClick(Sender: TObject);
begin
  ScSSHChannel.SourcePort := seSourcePort.Value;
  ScSSHChannel.DestHost := edDestHost.Text;
  ScSSHChannel.DestPort := seDestPort.Value;
  ScSSHChannel.Connect;

  btStart.Enabled := False;
  btStop.Enabled := True;
  btSConnect.Enabled := True;
  btSDisconnect.Enabled := False;
end;

procedure TSSHLocalPF.btStopClick(Sender: TObject);
begin
  DisconnectChannel;
end;

procedure TSSHLocalPF.btSConnectClick(Sender: TObject);
begin
  Assert(FVio = nil);
  FVio := TCRVioTcp.Create;
  try
    FLockEvent.Acquire;
    try
      FVio.Host := 'localhost';
      FVio.Port := ScSSHChannel.SourcePort;
      FVio.ConnectionTimeout := ScSSHChannel.Timeout;
      FVio.Connect;

      btSConnect.Enabled := False;
      btSDisconnect.Enabled := True;
    finally
      FLockEvent.Release;
    end;
  except
    FreeAndNil(FVio);
    raise;
  end;
end;

procedure TSSHLocalPF.btSDisconnectClick(Sender: TObject);
begin
  if FVio <> nil then
    FVio.Close;
end;

procedure TSSHLocalPF.edDestHostChange(Sender: TObject);
begin
  DisconnectChannel;
end;

procedure TSSHLocalPF.WriteLog(const Event: string; const SockAddr: PSockAddr);
var
  Sock: TIPEndPoint;
begin
  Sock := TIPEndPoint.Create(PSockAddrIn(SockAddr));

  FLockEvent.Acquire;
  try
    FEvStrings.AddObject(Event, Sock);
  finally
    FLockEvent.Release;
  end;
end;

procedure TSSHLocalPF.ScSSHChannelSocketConnect(Sender: TObject; const SockAddr: PSockAddr);
begin
  WriteLog('Connect', SockAddr);
end;

procedure TSSHLocalPF.ScSSHChannelSocketDisconnect(Sender: TObject; const SockAddr: PSockAddr);
begin
  FLockEvent.Acquire;
  try
    btSConnect.Enabled := ScSSHChannel.Connected;
    btSDisconnect.Enabled := False;
    FreeAndNil(FVio);
  finally
    FLockEvent.Release;
  end;

  WriteLog('Disconnect', SockAddr);
end;

procedure TSSHLocalPF.TimerTimer(Sender: TObject);
var
  Sock: TIPEndPoint;
begin
  FLockEvent.Acquire;
  try
    while FEvStrings.Count > 0 do begin
      Sock := TIPEndPoint(FEvStrings.Objects[0]);

      try
        if StringGrid.Cells[0, 1] <> '' then
          StringGrid.RowCount := StringGrid.RowCount + 1;

        StringGrid.Cells[0, StringGrid.RowCount - 1] := IntToStr(StringGrid.RowCount - 1);
        StringGrid.Cells[1, StringGrid.RowCount - 1] := FEvStrings.Strings[0];
        StringGrid.Cells[2, StringGrid.RowCount - 1] := Sock.ToString;
        StringGrid.Cells[3, StringGrid.RowCount - 1] := IntToStr(Sock.Port);
      finally
        Sock.Free;
        FEvStrings.Delete(0);
      end;
    end;
  finally
    FLockEvent.Release;
  end;
end;

procedure TSSHLocalPF.btImportKeyClick(Sender: TObject);
var
  KeyName: string;
begin
  KeyName := edSSHHost.Text;
  SSHClientForm.ImportServerKey(ScFileStorage, KeyName);
  edSSHHost.Text := KeyName;
end;

procedure TSSHLocalPF.ScSSHClientAuthenticationPrompt(Sender: TObject;
  const Name, Instruction: string; const Prompts: TStringDynArray;
  var Responses: TStringDynArray);
begin
  if (Name = '') and (Length(Prompts) = 0) then
    Exit;
  fmPrompt.InitForm(Name, Instruction, Prompts);
  fmPrompt.ShowModal;
  fmPrompt.GetResponse(Responses);
end;

end.
