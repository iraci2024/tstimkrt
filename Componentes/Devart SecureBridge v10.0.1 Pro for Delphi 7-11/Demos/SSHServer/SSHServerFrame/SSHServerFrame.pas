unit SSHServerFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
  Classes, SysUtils, Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, StdCtrls, ToolWin, ComCtrls,
  Buttons, Spin, Grids, SyncObjs, Winsvc, DemoFrame, 
  ScColFrame, ScKeysFrame, ScUsersFrame, ScBridge;

type
  TSSHServerFrame = class(TDemoFrame)
    Panel1: TPanel;
    PageControl: TPageControl;
    tsServer: TTabSheet;
    Panel11: TPanel;
    tsUsers: TTabSheet;
    Panel9: TPanel;
    PanelUsers: TPanel;
    Panel6: TPanel;
    Label9: TLabel;
    sePort: TSpinEdit;
    ScFileStorage: TScFileStorage;
    tsKeyStorage: TTabSheet;
    PanelKeys: TPanel;
    tsSecurity: TTabSheet;
    Panel10: TPanel;
    Panel8: TPanel;
    Panel2: TPanel;
    Label2: TLabel;
    cb3DES: TCheckBox;
    cbBlowfish: TCheckBox;
    cbAES128: TCheckBox;
    cbCast128: TCheckBox;
    Panel4: TPanel;
    Label3: TLabel;
    cbPublicKey: TCheckBox;
    cbPassword: TCheckBox;
    Panel5: TPanel;
    Label4: TLabel;
    cbRSAKey: TCheckBox;
    cbDSAKey: TCheckBox;
    cbbRSAKeyName: TComboBox;
    cbbDSAKeyName: TComboBox;
    Label6: TLabel;
    Panel12: TPanel;
    Label7: TLabel;
    seClientAliveCountMax: TSpinEdit;
    seMaxStartups: TSpinEdit;
    Label8: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    seClientAliveInterval: TSpinEdit;
    Label12: TLabel;
    Label13: TLabel;
    seTimeout: TSpinEdit;
    Label14: TLabel;
    cbbListenAddress: TComboBox;
    edBanner: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    btConnectSSH: TButton;
    btDisconnectSSH: TButton;
    btOK: TButton;
    btCancel: TButton;
    edStoragePath: TEdit;
    Label15: TLabel;
    sbBanner: TSpeedButton;
    sbStoragePath: TSpeedButton;
    cbLog: TCheckBox;
    edLogFile: TEdit;
    sbLogFile: TSpeedButton;
    shState: TShape;
    cbAES192: TCheckBox;
    cbAES256: TCheckBox;
    procedure btConnectSSHClick(Sender: TObject);
    procedure btDisconnectSSHClick(Sender: TObject);
    procedure cbRSAKeyClick(Sender: TObject);
    procedure cbbRSAKeyNameDropDown(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure PageControlChange(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure sbBannerClick(Sender: TObject);
    procedure sbStoragePathClick(Sender: TObject);
    procedure cbLogClick(Sender: TObject);
    procedure sbLogFileClick(Sender: TObject);
  private
    FKeysFrame: TScKeysFrame;
    FUsersFrame: TScUsersFrame;
    function GetActiveFrame: TScColFrame;
    function GetFilename(var Filename: string): Boolean;

    procedure ShowServerState;
    procedure LoadState;
    procedure SaveState;
    function KeyPath: string;
  public
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Activate; override;
  end;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Registry, FileCtrl,
  ScConsts, ScUtils, ScVioSocket, SSHServerMain;

const
  ServiceName = 'SSHServerSrv';
  ServerKeyNameRSA = 'SBSSHServer_RSA';
  ServerKeyNameDSA = 'SBSSHServer_DSA';
  LogFilename = 'SBSSHServer.log';
  SAllInterfaces = '0.0.0.0 (All interfaces)';

{$IFDEF VER130}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}

procedure TSSHServerFrame.Initialize;
begin
  inherited;

  try
    FKeysFrame := TScKeysFrame.Create(nil);
    FKeysFrame.Parent := PanelKeys;
    FKeysFrame.Align := alClient;
    FKeysFrame.Name := FKeysFrame.Parent.Name + FKeysFrame.ClassName;
    FKeysFrame.Storage := ScFileStorage;

    FUsersFrame := TScUsersFrame.Create(nil);
    FUsersFrame.Parent := PanelUsers;
    FUsersFrame.Align := alClient;
    FUsersFrame.Name := FUsersFrame.Parent.Name + FUsersFrame.ClassName;
    FUsersFrame.Storage := ScFileStorage;

    cbbRSAKeyName.Text := ServerKeyNameRSA;
    cbbDSAKeyName.Text := ServerKeyNameDSA;
    GetIPInterfaces(cbbListenAddress.Items);
    cbbListenAddress.Items.Insert(0, SAllInterfaces);
    cbbListenAddress.ItemIndex := 0;
    edLogFile.Text := LogFilename;
  except
    on E: Exception do
      ApplicationHandleException(E);
  end;
end;

procedure TSSHServerFrame.Finalize;
begin
  SaveState;
  FKeysFrame.Free;
  FUsersFrame.Free;

  inherited;
end;

procedure TSSHServerFrame.Activate;
begin
  LoadState;
  ScFileStorage.Path := ExpandFileName(edStoragePath.Text);

  if PageControl.ActivePage = tsServer then
    ShowServerState;
end;

procedure TSSHServerFrame.SaveState;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(KeyPath, True);

      if edStoragePath.Text <> '' then
        WriteString('Storage Path', ExpandFileName(edStoragePath.Text))
      else
        WriteString('Storage Path', '');
      if cbbListenAddress.Text = SAllInterfaces then
        WriteString('ListenAddress', '0.0.0.0')
      else
        WriteString('ListenAddress', cbbListenAddress.Text);

      WriteInteger('ServerPort', sePort.Value);
      WriteString('Banner', edBanner.Text);
      WriteBool('Log events', cbLog.Checked);
      if edLogFile.Text <> '' then
        WriteString('Log file', ExpandFileName(edLogFile.Text))
      else
        WriteString('Log file', '');

      WriteBool('ServerCipher_3DES', cb3DES.Checked);
      WriteBool('ServerCipher_Blowfish', cbBlowfish.Checked);
      WriteBool('ServerCipher_AES128', cbAES128.Checked);
      WriteBool('ServerCipher_AES192', cbAES192.Checked);
      WriteBool('ServerCipher_AES256', cbAES256.Checked);
      WriteBool('ServerCipher_Cast128', cbCast128.Checked);
      WriteBool('SSHServerAuthentication_PublicKey', cbPublicKey.Checked);
      WriteBool('SSHServerAuthentication_Password', cbPassword.Checked);
      WriteString('RSAKeyName', cbbRSAKeyName.Text);
      WriteString('DSAKeyName', cbbDSAKeyName.Text);
      WriteInteger('ClientAliveCountMax', seClientAliveCountMax.Value);
      WriteInteger('ClientAliveInterval', seClientAliveInterval.Value);
      WriteInteger('MaxStartups', seMaxStartups.Value);
      WriteInteger('ServerTimeout', seTimeout.Value);
    end;
  finally
    Registry.Free;
  end;
end;

procedure TSSHServerFrame.LoadState;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(KeyPath, False) then begin
        if ValueExists('Storage Path') then
          edStoragePath.Text := ReadString('Storage Path');

        if ValueExists('ListenAddress') then begin
          cbbListenAddress.ItemIndex := cbbListenAddress.Items.IndexOf(ReadString('ListenAddress'));
          if cbbListenAddress.ItemIndex = -1 then
            cbbListenAddress.ItemIndex := 0;
        end;

        if ValueExists('ServerPort') then
          sePort.Value := ReadInteger('ServerPort');
        if ValueExists('Banner') then
          edBanner.Text := ReadString('Banner');
        if ValueExists('Log events') then
          cbLog.Checked := ReadBool('Log events');
        if ValueExists('Log file') then
          edLogFile.Text := ReadString('Log file');

        if ValueExists('ServerCipher_3DES') then
          cb3DES.Checked := ReadBool('ServerCipher_3DES');
        if ValueExists('ServerCipher_Blowfish') then
          cbBlowfish.Checked := ReadBool('ServerCipher_Blowfish');
        if ValueExists('ServerCipher_AES128') then
          cbAES128.Checked := ReadBool('ServerCipher_AES128');
        if ValueExists('ServerCipher_AES192') then
          cbAES192.Checked := ReadBool('ServerCipher_AES192');
        if ValueExists('ServerCipher_AES256') then
          cbAES256.Checked := ReadBool('ServerCipher_AES256');
        if ValueExists('ServerCipher_Cast128') then
          cbCast128.Checked := ReadBool('ServerCipher_Cast128');
        if ValueExists('SSHServerAuthentication_PublicKey') then
          cbPublicKey.Checked := ReadBool('SSHServerAuthentication_PublicKey');
        if ValueExists('SSHServerAuthentication_Password') then
          cbPassword.Checked := ReadBool('SSHServerAuthentication_Password');

        if ValueExists('RSAKeyName') then
          cbbRSAKeyName.Text := ReadString('RSAKeyName');
        cbRSAKey.Checked := cbbRSAKeyName.Text <> '';
        if ValueExists('DSAKeyName') then
          cbbDSAKeyName.Text := ReadString('DSAKeyName');
        cbDSAKey.Checked := cbbDSAKeyName.Text <> '';

        if ValueExists('ClientAliveCountMax') then
          seClientAliveCountMax.Value := ReadInteger('ClientAliveCountMax');
        if ValueExists('ClientAliveInterval') then
          seClientAliveInterval.Value := ReadInteger('ClientAliveInterval');
        if ValueExists('MaxStartups') then
          seMaxStartups.Value := ReadInteger('MaxStartups');
        if ValueExists('ServerTimeout') then
          seTimeout.Value := ReadInteger('ServerTimeout');
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSHServerFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\SecureBridge\SSHServer';
end;

procedure TSSHServerFrame.ShowServerState;
var
  schSCManager, schService: DWORD;
  ss: TServiceStatus;
  ServiceActive: Boolean;
begin
  ServiceActive := False;
  try
    schSCManager := OpenSCManager(nil, nil, GENERIC_READ);
    if schSCManager = 0 then
      RaiseLastOSError;

    try
      schService := OpenService(schSCManager, {$IFNDEF CLR}PChar{$ENDIF}(ServiceName), GENERIC_READ);
      if schService = 0 then
        RaiseLastOSError;

      try
        ControlService(schService, SERVICE_CONTROL_INTERROGATE, ss);
        ServiceActive := (ss.dwCurrentState = SERVICE_RUNNING) or (ss.dwCurrentState = SERVICE_START_PENDING);
      finally
        CloseServiceHandle(schService);
      end;
    finally
      CloseServiceHandle(schSCManager);
    end;

  finally
    btConnectSSH.Enabled := not ServiceActive;
    btDisconnectSSH.Enabled := ServiceActive;
    if ServiceActive then
      shState.Brush.Color := clGreen
    else
      shState.Brush.Color := clRed;
  end;
end;

procedure TSSHServerFrame.btConnectSSHClick(Sender: TObject);
var
  schSCManager, schService: DWORD;
  p: {$IFDEF CLR}IntPtr{$ELSE}PChar{$ENDIF};
begin
  try
    schSCManager := OpenSCManager(nil, nil, GENERIC_READ);
    if schSCManager = 0 then
      RaiseLastOSError;

    try
      schService := OpenService(schSCManager, {$IFNDEF CLR}PChar{$ENDIF}(ServiceName), SERVICE_START);
      if schService = 0 then
        RaiseLastOSError;

      try
        p := nil;
        if not StartService(schService, 0, p) then
          RaiseLastOSError;
      finally
        CloseServiceHandle(schService);
      end;
    finally
      CloseServiceHandle(schSCManager);
    end;
  finally
    ShowServerState;
  end;
end;

procedure TSSHServerFrame.btDisconnectSSHClick(Sender: TObject);
var
  schSCManager, schService: DWORD;
  ss: TServiceStatus;
begin
  try
    schSCManager := OpenSCManager(nil, nil, GENERIC_READ);
    if schSCManager = 0 then
      RaiseLastOSError;

    try
      schService := OpenService(schSCManager, {$IFNDEF CLR}PChar{$ENDIF}(ServiceName), SERVICE_STOP);
      if schService = 0 then
        RaiseLastOSError;

      try
        if not ControlService(schService, SERVICE_CONTROL_STOP, ss) then
          RaiseLastOSError;
      finally
        CloseServiceHandle(schService);
      end;
    finally
      CloseServiceHandle(schSCManager);
    end;
  finally
    ShowServerState;
  end;
end;

procedure TSSHServerFrame.cbRSAKeyClick(Sender: TObject);
begin
  cbbRSAKeyName.Enabled := cbRSAKey.Checked;
  if not cbbRSAKeyName.Enabled then
    cbbRSAKeyName.Text := '';

  cbbDSAKeyName.Enabled := cbDSAKey.Checked;
  if not cbbDSAKeyName.Enabled then
    cbbDSAKeyName.Text := '';
end;

procedure TSSHServerFrame.cbbRSAKeyNameDropDown(Sender: TObject);
begin
  ScFileStorage.Keys.GetKeyNames((Sender as TComboBox).Items);
end;

function TSSHServerFrame.GetActiveFrame: TScColFrame;
begin
  if PageControl.ActivePage = tsKeyStorage then
    Result := FKeysFrame
  else if PageControl.ActivePage = tsUsers then
    Result := FUsersFrame
  else
    Result := nil;
end;

procedure TSSHServerFrame.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if GetActiveFrame <> nil then
    try
      AllowChange := GetActiveFrame.Save;
      if AllowChange then
        GetActiveFrame.Finish;
    except
      on E: Exception do begin
        AllowChange := False;
        ApplicationHandleException(E);
      end;
    end;
end;

procedure TSSHServerFrame.PageControlChange(Sender: TObject);
begin
  if GetActiveFrame <> nil then
    GetActiveFrame.Activate;

  if PageControl.ActivePage = tsServer then
    ShowServerState;
end;

procedure TSSHServerFrame.btOKClick(Sender: TObject);
begin
  SaveState;

  if GetActiveFrame <> nil then begin
    if GetActiveFrame.Save then
      fmSSHServerForm.Close;
  end
  else
    fmSSHServerForm.Close;
end;

procedure TSSHServerFrame.btCancelClick(Sender: TObject);
begin
  fmSSHServerForm.Close;
end;

function TSSHServerFrame.GetFilename(var Filename: string): Boolean;
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

procedure TSSHServerFrame.sbBannerClick(Sender: TObject);
var
  Filename: string;
  sl: TStringList;
begin
  if GetFilename(Filename) then begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(Filename);
      edBanner.Text := sl.Text;
    finally
      sl.Free;
    end;
  end;
end;

procedure TSSHServerFrame.sbStoragePathClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edStoragePath.Text;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    edStoragePath.Text := Dir;
end;

procedure TSSHServerFrame.cbLogClick(Sender: TObject);
begin
  edLogFile.Enabled := cbLog.Checked;
  sbLogFile.Enabled := cbLog.Checked;
  if edLogFile.Enabled then
    edLogFile.Color := clWindow
  else
    edLogFile.Color := clBtnFace;
end;

procedure TSSHServerFrame.sbLogFileClick(Sender: TObject);
var
  Filename: string;
begin
  if GetFilename(Filename) then
    edLogFile.Text := Filename;
end;

{$IFDEF VER130}
initialization
  ApplicationHandleException := Application.HandleException;
{$ENDIF}
end.
