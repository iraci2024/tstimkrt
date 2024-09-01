unit SSHServerFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
  WinSock, Classes, SysUtils,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, StdCtrls, ToolWin, ComCtrls, Buttons, Grids, Spin,
  SyncObjs, DemoFrame,
  ScUtils, ScBridge, ScSSHServer, ScSSHUtils, ScSSHSocket, ScSFTPServer;

type
  TSSHServerFrame = class(TDemoFrame)
    Panel1: TPanel;
    ScSSHServer: TScSSHServer;
    PageControl: TPageControl;
    tsServer: TTabSheet;
    Panel11: TPanel;
    tsUsers: TTabSheet;
    Panel9: TPanel;
    PanelKey: TPanel;
    Label5: TLabel;
    edFingerPrint: TEdit;
    Panel7: TPanel;
    btGenerate: TSpeedButton;
    btExportPublic: TSpeedButton;
    Panel12: TPanel;
    lbUserName: TListBox;
    Panel8: TPanel;
    btNewUser: TSpeedButton;
    btDeleteUser: TSpeedButton;
    Panel10: TPanel;
    PanelUser: TPanel;
    Label4: TLabel;
    Label7: TLabel;
    edUserName: TEdit;
    edPassword: TEdit;
    Label3: TLabel;
    Panel6: TPanel;
    Label9: TLabel;
    sePort: TSpinEdit;
    Label1: TLabel;
    SaveDialog: TSaveDialog;
    Panel3: TPanel;
    btConnectSSH: TSpeedButton;
    btDisconnectSSH: TSpeedButton;
    ScFileStorage: TScFileStorage;
    tsLog: TTabSheet;
    sgLog: TStringGrid;
    cbRandomization: TCheckBox;
    ScSFTPServer: TScSFTPServer;
    procedure btGenerateClick(Sender: TObject);
    procedure btExportPublicClick(Sender: TObject);
    procedure lbUserNameClick(Sender: TObject);
    procedure btNewUserClick(Sender: TObject);
    procedure btDeleteUserClick(Sender: TObject);
    procedure btConnectSSHClick(Sender: TObject);
    procedure btDisconnectSSHClick(Sender: TObject);
    procedure sePortChange(Sender: TObject);
    procedure edUserNameExit(Sender: TObject);
    procedure ScSSHServerAfterClientConnect(Sender: TObject;
      ClientInfo: TScSSHClientInfo);
    procedure ScSSHServerAfterClientDisconnect(Sender: TObject;
      ClientInfo: TScSSHClientInfo);
    procedure ScSSHServerBeforeChannelConnect(Sender: TObject;
      ChannelInfo: TScSSHChannelInfo; var Direct: Boolean);
    procedure ScSSHServerAfterChannelDisconnect(Sender: TObject;
      ChannelInfo: TScSSHChannelInfo);
  private
    FOldUserIndex: integer;
    FLockEvent: TCriticalSection;
    procedure InitKeys;
    procedure GenerateKey(Key: TScKey; const Algorithm: TScAsymmetricAlgorithm);
    procedure InitUsers;
    procedure SelectUser;
    procedure SaveUser;

    procedure ShowButton(Enabled: boolean);
    procedure CheckRandomize;
    function LoadState: boolean;
    function SaveState: boolean;
    function KeyPath: string;
    procedure WriteLog(const Event: string; Obj: TObject);
  public
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
  Registry, ScConsts, ScVioTcp, SSHServerMain;

const
  ServerKeyNameRSA = 'SBSSHServer_RSA';
  ServerKeyNameDSA = 'SBSSHServer_DSA';
  ServerKeyNameEC = 'SBSSHServer_EC';

procedure TSSHServerFrame.Initialize;
begin
  inherited;

  FLockEvent := TCriticalSection.Create;

  InitKeys;
  InitUsers;
  LoadState;

  ScSSHServer.Port := sePort.Value;
  ScSSHServer.KeyNameRSA := ServerKeyNameRSA;
  ScSSHServer.KeyNameDSA := ServerKeyNameDSA;
  ScSSHServer.KeyNameEC := ServerKeyNameEC;

  sgLog.Cells[0, 0] := 'Time';
  sgLog.Cells[1, 0] := 'Event';
  sgLog.Cells[2, 0] := 'User name';
  sgLog.Cells[3, 0] := 'From host';
  sgLog.Cells[4, 0] := 'From port';
  sgLog.Cells[5, 0] := 'To host';
  sgLog.Cells[6, 0] := 'To port';
end;

procedure TSSHServerFrame.Finalize;
begin
  SaveState;
  FreeAndNil(FLockEvent);

  inherited;
end;

procedure TSSHServerFrame.CheckRandomize;
begin
  if not fmSSHServerForm.Randomized and not cbRandomization.Checked then begin
    fmSSHServerForm.Randomize;
    if not fmSSHServerForm.Randomized and not cbRandomization.Checked then
      raise Exception.Create('Data for the random generator has not been generated');
  end;
end;

procedure TSSHServerFrame.InitKeys;

  procedure CheckKey(const KeyName: string; const Algorithm: TScAsymmetricAlgorithm);
  var
    Key: TScKey;
  begin
    Key := ScFileStorage.Keys.FindKey(KeyName);

    if Key = nil then begin
      Key := TScKey.Create(nil);
      try
        GenerateKey(Key, Algorithm);
        Key.KeyName := KeyName;
        ScFileStorage.Keys.Add(Key);
      except
        Key.Free;
        raise;
      end;
    end;
  end;

var
  fp: string;
begin
  ScFileStorage.Keys.Refresh;
  CheckKey(ServerKeyNameRSA, aaRSA);
  CheckKey(ServerKeyNameDSA, aaDSA);
  CheckKey(ServerKeyNameEC, aaEC);

  ScFileStorage.Keys.KeyByName(ServerKeyNameRSA).GetFingerprint(haMD5, fp);
  edFingerPrint.Text := fp;
end;

procedure TSSHServerFrame.GenerateKey(Key: TScKey; const Algorithm: TScAsymmetricAlgorithm);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    if Algorithm = aaEC then
      Key.GenerateEC(secp256r1)
    else
      Key.Generate(Algorithm, 2048);
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TSSHServerFrame.btGenerateClick(Sender: TObject);
var
  fp: string;
  Key: TScKey;
begin
  if MessageDlg('Do you want regenarate the server key?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    CheckRandomize;
    edFingerPrint.Text := '';
    Key := ScFileStorage.Keys.KeyByName(ServerKeyNameRSA);
    GenerateKey(Key, aaRSA);
    Key.GetFingerprint(haMD5, fp);
    edFingerPrint.Text := fp;
  end;
end;

procedure TSSHServerFrame.btExportPublicClick(Sender: TObject);
var
  Key: TScKey;
  KeyFormat: TScKeyFormat;
  Ext: string;
begin
  Key := ScFileStorage.Keys.KeyByName(ServerKeyNameRSA);
  Key.Ready := True;
  if SaveDialog.FileName = '' then
    SaveDialog.FileName := GetHostName;

  if SaveDialog.Execute then begin
    case SaveDialog.FilterIndex of
      1: begin
        KeyFormat := kfDefault;
        Ext := '.pub';
      end;
      else begin
        KeyFormat := kfIETF;
        Ext := '.ietfpub';
      end;
    end;

    Key.ExportTo(ChangeFileExt(SaveDialog.Filename, Ext), True, '', saTripleDES_cbc, KeyFormat);
  end;
end;

procedure TSSHServerFrame.InitUsers;
begin
  ScFileStorage.Users.Refresh;
  ScFileStorage.Users.GetUserNames(lbUserName.Items);

  if FOldUserIndex < lbUserName.Items.Count then
    lbUserName.ItemIndex := FOldUserIndex
  else
    lbUserName.ItemIndex := lbUserName.Items.Count - 1;
  SelectUser;
end;

procedure TSSHServerFrame.SelectUser;
begin
  PanelUser.Enabled := lbUserName.ItemIndex <> - 1;

  if PanelUser.Enabled then begin
    edUserName.Text := lbUserName.Items[lbUserName.ItemIndex];
    edPassword.Text := ScFileStorage.Users.UserByName(edUserName.Text).Password;
  end
  else begin
    edUserName.Text := '';
    edPassword.Text := '';
  end;
  FOldUserIndex := lbUserName.ItemIndex;
end;

procedure TSSHServerFrame.lbUserNameClick(Sender: TObject);
begin
  try
    SaveUser;
  except
    lbUserName.ItemIndex := FOldUserIndex;
    raise;
  end;

  if lbUserName.ItemIndex <> FOldUserIndex then
    SelectUser;
end;

procedure TSSHServerFrame.SaveUser;
var
  User: TScUser;
begin
  if FOldUserIndex >= 0 then begin
    User := ScFileStorage.Users.UserByName(lbUserName.Items[FOldUserIndex]);
    try
      User.UserName := edUserName.Text;
    except
      edUserName.SetFocus;
      raise;
    end;

    User.Password := edPassword.Text;
    lbUserName.Items[FOldUserIndex] := edUserName.Text;
  end;
end;

procedure TSSHServerFrame.btNewUserClick(Sender: TObject);
var
  OldCursor: TCursor;
  User: TScUser;
  NewUsername: string;
  i: Integer;
begin
  SaveUser;

  i := 0;
  repeat
    Inc(i);
    NewUsername := 'user' + IntToStr(i);
  until lbUserName.Items.IndexOf(NewUsername) < 0;

  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    User := TScUser.Create(ScFileStorage.Users);
    User.UserName := NewUsername;
    User.Authentications := [uaPassword];
  finally
    Screen.Cursor := OldCursor;
  end;

  lbUserName.Items.Add(NewUsername);
  lbUserName.ItemIndex := lbUserName.Items.Count - 1;
  SelectUser;
end;

procedure TSSHServerFrame.btDeleteUserClick(Sender: TObject);
var
  User: TScUser;
  UserName: string;
begin
  if lbUserName.ItemIndex < 0 then
    Exit;

  UserName := lbUserName.Items[lbUserName.ItemIndex];
  if MessageDlg('Do you want delete "' + UserName + '" user?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  User := ScFileStorage.Users.UserByName(UserName);
  try
    ScFileStorage.Users.Remove(User);
  finally
    User.Free;
  end;

  InitUsers;
end;

procedure TSSHServerFrame.edUserNameExit(Sender: TObject);
begin
  SaveUser;
end;

procedure TSSHServerFrame.ShowButton(Enabled: boolean);
begin
  btConnectSSH.Enabled := not Enabled;
  btDisconnectSSH.Enabled := Enabled;
end;

procedure TSSHServerFrame.btConnectSSHClick(Sender: TObject);
begin
  try
    CheckRandomize;
    ScSSHServer.Active := True;
  finally
    ShowButton(ScSSHServer.Active);
  end;
end;

procedure TSSHServerFrame.btDisconnectSSHClick(Sender: TObject);
begin
  try
    ScSSHServer.Active := False;
  finally
    ShowButton(ScSSHServer.Active);
  end;
end;

procedure TSSHServerFrame.sePortChange(Sender: TObject);
begin
  ScSSHServer.Port := sePort.Value;
  ShowButton(ScSSHServer.Active);
end;

procedure TSSHServerFrame.ScSSHServerAfterClientConnect(Sender: TObject;
  ClientInfo: TScSSHClientInfo);
begin
  WriteLog('Client connect', ClientInfo);
end;

procedure TSSHServerFrame.ScSSHServerAfterClientDisconnect(Sender: TObject;
  ClientInfo: TScSSHClientInfo);
begin
  WriteLog('Client disconnect', ClientInfo);
end;

procedure TSSHServerFrame.ScSSHServerBeforeChannelConnect(Sender: TObject;
  ChannelInfo: TScSSHChannelInfo; var Direct: Boolean);
begin
  WriteLog('Channel connect', ChannelInfo);
end;

procedure TSSHServerFrame.ScSSHServerAfterChannelDisconnect(
  Sender: TObject; ChannelInfo: TScSSHChannelInfo);
begin
  WriteLog('Channel disconnect', ChannelInfo);
end;

procedure TSSHServerFrame.WriteLog(const Event: string; Obj: TObject);
var
  TCPConnection: TScTCPConnection;
  UserName, DestHost, DestPort: string;
begin
  if FLockEvent = nil then
    Exit;

  FLockEvent.Acquire;
  try
    if sgLog.Cells[0, 1] <> '' then
      sgLog.RowCount := sgLog.RowCount + 1;

    TCPConnection := nil;
    if Obj is TScSSHClientInfo then begin
      TCPConnection := TScSSHClientInfo(Obj).TCPConnection;
      UserName := TScSSHClientInfo(Obj).User;
      DestHost := '';
      DestPort := '';
    end
    else
    if Obj is TScSSHChannelInfo then begin
      TCPConnection := TScSSHChannelInfo(Obj).Client.TCPConnection;
      UserName := TScSSHChannelInfo(Obj).Client.User;
      DestHost := TScSSHChannelInfo(Obj).DestHost;
      DestPort := IntToStr(TScSSHChannelInfo(Obj).DestPort);
    end
    else
      Assert(False);

    sgLog.Cells[0, sgLog.RowCount - 1] := TimeToStr(Now);
    sgLog.Cells[1, sgLog.RowCount - 1] := Event;
    sgLog.Cells[2, sgLog.RowCount - 1] := UserName;
    sgLog.Cells[3, sgLog.RowCount - 1] := TCPConnection.GetRemoteIP;
    sgLog.Cells[4, sgLog.RowCount - 1] := IntToStr(TCPConnection.GetRemotePort);
    sgLog.Cells[5, sgLog.RowCount - 1] := DestHost;
    sgLog.Cells[6, sgLog.RowCount - 1] := DestPort;
    sgLog.Row := sgLog.RowCount - 1;
  finally
    FLockEvent.Release;
  end;
end;

function TSSHServerFrame.SaveState: boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(KeyPath, True);
      WriteInteger('ServerPort', sePort.Value);
      WriteString('KeyNameRSA', ScSSHServer.KeyNameRSA);
      WriteString('Path', ExpandFileName(ScFileStorage.Path));
      WriteBool('Silent randomization', cbRandomization.Checked);
    end;
  finally
    Registry.Free;
  end;

  Result := True;
end;

function TSSHServerFrame.LoadState: boolean;
var
  Registry: TRegistry;
begin
  Result := False;
  Registry := TRegistry.Create;
  try
    with Registry do begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(KeyPath, False) then begin
        if ValueExists('ServerPort') then
          sePort.Value := ReadInteger('ServerPort');
        if ValueExists('Silent randomization') then
          cbRandomization.Checked := ReadBool('Silent randomization');

        Result := True;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSHServerFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\SecureBridge\Demos\SimpleSSHServer';
end;

end.
