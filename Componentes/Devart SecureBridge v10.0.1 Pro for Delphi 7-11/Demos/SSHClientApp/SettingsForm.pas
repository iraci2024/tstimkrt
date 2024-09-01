unit SettingsForm;

{$I ..\Base\SBDemo.inc}
interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons,
  ScBridge, ScSSHUtils, ScSSHClient, ScSSHChannel, ScSFTPClient;

type
  TScSSHChannels = array of TScSSHChannel;

type
  TfmSettings = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Panel5: TPanel;
    Panel1: TPanel;
    Panel11: TPanel;
    Label6: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    pnPrivateKey: TPanel;
    Label8: TLabel;
    cbPrivateKey: TComboBox;
    Panel14: TPanel;
    btKeyGen: TSpeedButton;
    pnPassword: TPanel;
    lbPassword: TLabel;
    edSSHPassword: TEdit;
    edSSHUserName: TEdit;
    edSSHHost: TEdit;
    rbPassword: TRadioButton;
    rbPublicKey: TRadioButton;
    seSSHPort: TSpinEdit;
    rbKeyboardInteractive: TRadioButton;
    Panel12: TPanel;
    sbLogFile: TSpeedButton;
    cbLog: TCheckBox;
    edLogFile: TEdit;
    Panel6: TPanel;
    Panel2: TPanel;
    Panel21: TPanel;
    Label1: TLabel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    lbCiphers: TListBox;
    Panel22: TPanel;
    Label2: TLabel;
    cbHostKeyName: TComboBox;
    Panel25: TPanel;
    btImportKey: TSpeedButton;
    Panel23: TPanel;
    Timeout: TLabel;
    Label10: TLabel;
    cbRekeyLimit: TComboBox;
    seTimeout: TSpinEdit;
    Panel24: TPanel;
    Label7: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    seKeepAliveCountMax: TSpinEdit;
    seKeepAliveInterval: TSpinEdit;
    cbTCPKeepalive: TCheckBox;
    Panel7: TPanel;
    Panel3: TPanel;
    Panel31: TPanel;
    Label14: TLabel;
    Label9: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    rbLocal: TRadioButton;
    rbRemote: TRadioButton;
    edDestHost: TEdit;
    seSourcePort: TSpinEdit;
    seDestPort: TSpinEdit;
    lbPortForwarding: TListBox;
    Panel32: TPanel;
    sbAddPortForwarding: TSpeedButton;
    sbDeletePortForwarding: TSpeedButton;
    Panel8: TPanel;
    Panel4: TPanel;
    Panel41: TPanel;
    btSave: TButton;
    btCancel: TButton;
    Label18: TLabel;
    seSFTPTimeout: TSpinEdit;
    Label19: TLabel;
    seSFTPDowloadBlockSize: TSpinEdit;
    Label20: TLabel;
    seSFTPUploadBlockSize: TSpinEdit;
    Label21: TLabel;
    cbSFTPVersion: TComboBox;
    Label17: TLabel;
    Panel13: TPanel;
    cbStartShell: TCheckBox;
    Panel15: TPanel;
    btRandomize: TSpeedButton;
    Panel42: TPanel;
    Label22: TLabel;
    edStartDir: TEdit;
    lbStartDir: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure rbPasswordClick(Sender: TObject);
    procedure cbPrivateKeyDropDown(Sender: TObject);
    procedure btKeyGenClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure lbCiphersClick(Sender: TObject);
    procedure btImportKeyClick(Sender: TObject);
    procedure cbLogClick(Sender: TObject);
    procedure sbLogFileClick(Sender: TObject);
    procedure sbAddPortForwardingClick(Sender: TObject);
    procedure sbDeletePortForwardingClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btRandomizeClick(Sender: TObject);
    procedure lbStartDirClick(Sender: TObject);
  private
    ScFileStorage: TScFileStorage;
    ScSSHClient: TScSSHClient;
    ScSFTPClient: TScSFTPClient;

    procedure SetButtons;
    function GetFilename(var Filename: string): Boolean;
    procedure AddPortForwarding(Remote: Boolean; SourcePort: Integer;
      const DestHost: string; DestPort: Integer);
  public
    SSHChannelList: TScSSHChannels;
    procedure Init(SSHClient: TScSSHClient; SFTPClient: TScSFTPClient;
      FileStorage: TScFileStorage; const SSHChannelList: TScSSHChannels);
  end;

var
  fmSettings: TfmSettings;

implementation

uses
{$IFNDEF VER130}
  StrUtils,
{$ENDIF}
  SSHClientDemoForm, ScUtils, ScFunctions, TypInfo, FileCtrl, ScSFTPUtils;

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TfmSettings.FormCreate(Sender: TObject);
var
  i: Integer;
begin
{$IFDEF XPMAN}
  if SSHClientForm.GetIsXPMan then
    SSHClientForm.ReplaceFlatStyle(Self, False);
{$ENDIF}

  for i := GetTypeData(TypeInfo(TScSFTPVersion)){$IFNDEF CLR}^{$ENDIF}.MinValue to GetTypeData(TypeInfo(TScSFTPVersion)){$IFNDEF CLR}^{$ENDIF}.MaxValue do
    cbSFTPVersion.Items.Add(GetEnumName(TypeInfo(TScSFTPVersion), i));
end;

procedure TfmSettings.Init(SSHClient: TScSSHClient; SFTPClient: TScSFTPClient;
  FileStorage: TScFileStorage; const SSHChannelList: TScSSHChannels);
var
  i: Integer;
begin
  ScFileStorage := FileStorage;
  ScSSHClient := SSHClient;
  ScSFTPClient := SFTPClient;

  case SSHClient.Authentication of
    atPublicKey:
      rbPublicKey.Checked := True;
    atPassword:
      rbPassword.Checked := True;
    atKeyboardInteractive:
      rbKeyboardInteractive.Checked := True;
  else
    Assert(False);
  end;

  edSSHHost.Text := SSHClient.HostName;
  seSSHPort.Value := SSHClient.Port;
  edSSHUserName.Text := SSHClient.User;
  edSSHPassword.Text := SSHClient.Password;
  cbPrivateKey.Text := SSHClient.PrivateKeyName;
  cbHostKeyName.Text := SSHClient.HostKeyName;

  lbCiphers.Items.Clear;
  for i := 0 to SSHClient.CiphersClient.Count - 1 do
    lbCiphers.Items.Add(TScSSHCipherItem(SSHClient.CiphersClient.Items[i]).AsString);
  SetButtons;

  seTimeout.Value := SSHClient.Timeout;
  cbRekeyLimit.Text := SSHClient.Options.RekeyLimit;
  seKeepAliveCountMax.Value := SSHClient.Options.ServerAliveCountMax;
  seKeepAliveInterval.Value := SSHClient.Options.ServerAliveInterval;
  cbTCPKeepalive.Checked := SSHClient.Options.TCPKeepAlive;

  lbPortForwarding.Items.Clear;
  for i := 0 to Length(SSHChannelList) - 1 do
    AddPortForwarding(SSHChannelList[i].Remote, SSHChannelList[i].SourcePort,
      SSHChannelList[i].DestHost, SSHChannelList[i].DestPort);

  cbSFTPVersion.ItemIndex := cbSFTPVersion.Items.IndexOf(GetEnumName(TypeInfo(TScSFTPVersion), Integer(SFTPClient.Version)));
  seSFTPTimeout.Value := SFTPClient.Timeout;
  seSFTPDowloadBlockSize.Value := SFTPClient.ReadBlockSize;
  seSFTPUploadBlockSize.Value := SFTPClient.WriteBlockSize;
end;

procedure TfmSettings.btSaveClick(Sender: TObject);
var
  SSHChannel: TScSSHChannel;
  CiphersList: string;
  s: string;
  i, p1, p2: Integer;
begin
  if rbPublicKey.Checked then
    ScSSHClient.Authentication := atPublicKey
  else
  if rbPassword.Checked then
    ScSSHClient.Authentication := atPassword
  else
    ScSSHClient.Authentication := atKeyboardInteractive;

  ScSSHClient.HostName := edSSHHost.Text;
  ScSSHClient.Port := seSSHPort.Value;
  ScSSHClient.User := edSSHUserName.Text;
  ScSSHClient.Password := edSSHPassword.Text;
  ScSSHClient.PrivateKeyName := cbPrivateKey.Text;
  ScSSHClient.HostKeyName := cbHostKeyName.Text;

  CiphersList := '';
  for i := 0 to lbCiphers.Items.Count - 1 do begin
    if i > 0 then
      CiphersList := CiphersList + ',';
    CiphersList := CiphersList + lbCiphers.Items[i];
  end;
  ScSSHClient.CiphersClient.AsString := CiphersList;
  ScSSHClient.CiphersServer.AsString := CiphersList;

  ScSSHClient.Timeout := seTimeout.Value;
  ScSSHClient.Options.RekeyLimit := cbRekeyLimit.Text;
  ScSSHClient.Options.ServerAliveCountMax := seKeepAliveCountMax.Value;
  ScSSHClient.Options.ServerAliveInterval := seKeepAliveInterval.Value;
  ScSSHClient.Options.TCPKeepAlive := cbTCPKeepalive.Checked;

  SetLength(SSHChannelList, lbPortForwarding.Items.Count);
  for i := 0 to lbPortForwarding.Items.Count - 1 do begin
    SSHChannel := TScSSHChannel.Create(ScSSHClient);
    SSHChannel.GatewayPorts := True;
    SSHChannel.Client := ScSSHClient;
    SSHChannel.Timeout := ScSSHClient.Timeout;

    s := lbPortForwarding.Items[i];
    SSHChannel.Remote := s[1] = 'R';
    p1 := Pos(':', s);
    SSHChannel.SourcePort := StrToInt(Copy(s, 3, p1 - 3));
    p2 := PosEx(':', s, p1 + 1);
    SSHChannel.DestHost := Trim(Copy(s, p1 + 1, p2 - p1 - 1));
    SSHChannel.DestPort := StrToInt(Copy(s, p2 + 1, Length(s)));

    SSHChannelList[i] := SSHChannel;
  end;

  ScSFTPClient.Version := TScSFTPVersion(GetEnumValue(TypeInfo(TScSFTPVersion), cbSFTPVersion.Text));
  ScSFTPClient.Timeout := seSFTPTimeout.Value;
  ScSFTPClient.ReadBlockSize := seSFTPDowloadBlockSize.Value;
  ScSFTPClient.WriteBlockSize := seSFTPUploadBlockSize.Value;
end;

procedure TfmSettings.rbPasswordClick(Sender: TObject);
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
end;

procedure TfmSettings.cbPrivateKeyDropDown(Sender: TObject);
begin
  ScFileStorage.Keys.GetKeyNames((Sender as TComboBox).Items);
end;

procedure TfmSettings.btKeyGenClick(Sender: TObject);
begin
  if cbPrivateKey.Text = '' then
    cbPrivateKey.Text := 'client_key';

  SSHClientForm.GenerateClientKey(ScFileStorage, cbPrivateKey.Text);
end;

procedure TfmSettings.SetButtons;
begin
  UpBtn.Enabled := lbCiphers.ItemIndex > 0;
  DownBtn.Enabled := (lbCiphers.ItemIndex >= 0) and (lbCiphers.ItemIndex < lbCiphers.Items.Count - 1);
end;

procedure TfmSettings.UpBtnClick(Sender: TObject);
var
  SelIndex: Integer;
begin
  SelIndex := lbCiphers.ItemIndex;
  if SelIndex > 0 then begin
    lbCiphers.Items.Move(SelIndex, SelIndex - 1);
    lbCiphers.ItemIndex := SelIndex - 1;
  end;
  SetButtons;
end;

procedure TfmSettings.DownBtnClick(Sender: TObject);
var
  SelIndex: Integer;
begin
  SelIndex := lbCiphers.ItemIndex;
  if (SelIndex >= 0) and (SelIndex < lbCiphers.Items.Count - 1) then begin
    lbCiphers.Items.Move(SelIndex, SelIndex + 1);
    lbCiphers.ItemIndex := SelIndex + 1;
  end;
  SetButtons;
end;

procedure TfmSettings.lbCiphersClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TfmSettings.btImportKeyClick(Sender: TObject);
var
  KeyName: string;
begin
  KeyName := cbHostKeyName.Text;
  SSHClientForm.ImportServerKey(ScFileStorage, KeyName);
  cbHostKeyName.Text := KeyName;
end;

procedure TfmSettings.cbLogClick(Sender: TObject);
begin
  edLogFile.Enabled := cbLog.Checked;
  sbLogFile.Enabled := cbLog.Checked;
  if edLogFile.Enabled then
    edLogFile.Color := clWindow
  else
    edLogFile.Color := clBtnFace;
end;

procedure TfmSettings.sbLogFileClick(Sender: TObject);
var
  Filename: string;
begin
  if GetFilename(Filename) then
    edLogFile.Text := Filename;
end;

function TfmSettings.GetFilename(var Filename: string): Boolean;
var
  FileOpen: TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(Application);
  try
    FileOpen.Filename := '';
    FileOpen.Filter := 'All files (*.*)|*.*|Text files (*.txt)|*.txt|Log files (*.log)|*.log';
    FileOpen.Options := FileOpen.Options + [ofPathMustExist, ofFileMustExist];

    Result := FileOpen.Execute;
    if Result then
      Filename := FileOpen.Filename;
  finally
    FileOpen.Free;
  end;
end;

procedure TfmSettings.AddPortForwarding(Remote: Boolean; SourcePort: Integer;
  const DestHost: string; DestPort: Integer);
var
  s: string;
begin
  if Remote then
    s := 'R '
  else
    s := 'L ';
  s := s + IntToStr(SourcePort) + ': ' + DestHost + ':' + IntToStr(DestPort);
  lbPortForwarding.Items.Add(s);
end;

procedure TfmSettings.sbAddPortForwardingClick(Sender: TObject);
begin
  if seSourcePort.Value = 0 then
    raise Exception.Create('You need to specify a source port');
  if edDestHost.Text = '' then
    raise Exception.Create('You need to specify a destination host');
  if seDestPort.Value = 0 then
    raise Exception.Create('You need to specify a destination port');

  AddPortForwarding(rbRemote.Checked, seSourcePort.Value, edDestHost.Text, seDestPort.Value);
  rbLocal.Checked := True;
  seSourcePort.Value := 0;
  edDestHost.Text := '';
  seDestPort.Value := 0;
end;

procedure TfmSettings.sbDeletePortForwardingClick(Sender: TObject);
begin
  if lbPortForwarding.ItemIndex <> -1 then
    lbPortForwarding.Items.Delete(lbPortForwarding.ItemIndex);
end;

procedure TfmSettings.btRandomizeClick(Sender: TObject);
begin
  SSHClientForm.Randomize;
end;

procedure TfmSettings.lbStartDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edStartDir.Text;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    edStartDir.Text := Dir;
end;

end.

