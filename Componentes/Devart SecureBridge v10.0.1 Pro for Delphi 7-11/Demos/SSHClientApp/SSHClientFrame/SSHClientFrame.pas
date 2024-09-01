unit SSHClientFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
{$IFDEF MSWINDOWS}WinSock,{$ELSE}Libc,{$ENDIF}
{$IFNDEF VER130}
  Types,
{$ENDIF}
  Classes, SysUtils, Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ToolWin, ComCtrls, Buttons, Grids, Spin, SyncObjs,
  ImgList, ScCLRClasses, ScTypes, ScUtils, ScBridge, ScSSHClient, ScSSHChannel,
  ScSFTPClient, ScSFTPUtils, ScSSHSocket, DemoFrame, SettingsForm;

type
  TfrSSHClientFrame = class(TDemoFrame)
    ScSSHClient: TScSSHClient;
    ScFileStorage: TScFileStorage;
    pnTopPanel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    btConnect: TSpeedButton;
    btDisconnect: TSpeedButton;
    btSettings: TSpeedButton;
    PageControl: TPageControl;
    tsSFTP: TTabSheet;
    tsShell: TTabSheet;
    Panel3: TPanel;
    Panel13: TPanel;
    Panel11: TPanel;
    edRootDir: TEdit;
    Panel7: TPanel;
    FileView: TTreeView;
    Panel14: TPanel;
    Panel4: TPanel;
    btMakeDir: TSpeedButton;
    btRemove: TSpeedButton;
    btRename: TSpeedButton;
    btUpload: TSpeedButton;
    btDownload: TSpeedButton;
    btViewFile: TSpeedButton;
    ScSFTPClient: TScSFTPClient;
    ScSSHShell: TScSSHShell;
    ImageList1: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Panel6: TPanel;
    Panel8: TPanel;
    edCommand: TEdit;
    Panel9: TPanel;
    sbExecute: TSpeedButton;
    mmShell: TMemo;
    procedure ScSSHClientAfterConnect(Sender: TObject);
    procedure ScSSHClientAfterDisconnect(Sender: TObject);
    procedure ScSSHClientServerKeyValidate(Sender: TObject;
      NewServerKey: TScKey; var Accept: Boolean);
    procedure btSettingsClick(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure ScSSHClientAuthenticationPrompt(Sender: TObject; const Name,
      Instruction: string; const Prompts: TStringDynArray;
      var Responses: TStringDynArray);
    procedure FileViewCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure ScSFTPClientCreateLocalFile(Sender: TObject; const LocalFileName,
      RemoteFileName: string; Attrs: TScSFTPFileAttributes; var Handle: Cardinal);
    procedure ScSFTPClientDirectoryList(Sender: TObject; const Path: string;
      const Handle: TBytes; FileInfo: TScSFTPFileInfo; EOF: Boolean);
    procedure ScSFTPClientError(Sender: TObject; Operation: TScSFTPOperation;
      const FileName: string; const Handle: TBytes; ErrorCode: Integer;
      const ErrorMessage: string; var Fail: Boolean);
    procedure FileViewDblClick(Sender: TObject);
    procedure FileViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btMakeDirClick(Sender: TObject);
    procedure btRenameClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure btViewFileClick(Sender: TObject);
    procedure ScSSHShellAsyncReceive(Sender: TObject);
    procedure sbExecuteClick(Sender: TObject);
    procedure edCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ScSSHShellDisconnect(Sender: TObject);
    procedure ScSFTPClientDisconnect(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
  private
    FLockLog: TCriticalSection;
    FNeedForLog: Boolean;
    FLogFilename: string;
    FStartShell: Boolean;
    FSSHChannelList: TScSSHChannels;
    FSFTPStartDir: string;

    procedure Log(Message: string);
    function SSHChannelLogMessage(SSHChannel: TScSSHChannel;
      const Event: string; IsError: Boolean = False): string;
    procedure LoadState;
    procedure SaveState;
    function KeyPath: string;
    procedure DisconnectAll;
    procedure ShowSSHButtons;
    procedure ShowSFTPButtons(Connected: boolean);
    procedure OpenDir(Path: string; const SelectedName: string = '');
    function GetRootDir: string;
    function GetSelectedNode: TTreeNode;

    procedure OnScSSHChannelDisconnect(Sender: TObject);
    procedure OnScSSHChannelError(Sender: TObject; E: Exception);
    procedure OnSocketConnect(Sender: TObject; const SockAddr: PSockAddr);
    procedure OnSocketDisconnect(Sender: TObject; const SockAddr: PSockAddr);
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
  Registry, ScSSHUtils, SSHClientDemoForm, PromptForm, NewNameForm,
  FileViewForm;

const
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
const
  SNoFilesSelected = 'No files selected!';

destructor TfrSSHClientFrame.Destroy;
begin
  DisconnectAll;
  FLockLog.Free;

  inherited;
end;

procedure TfrSSHClientFrame.Initialize;
begin
  inherited;

  FLockLog := TCriticalSection.Create;

  FStartShell := True;
  FLogFilename := '';
  FNeedForLog := False;
  FSFTPStartDir := '.';
  LoadState;

  edRootDir.Text := FSFTPStartDir;
  ScSSHShell.Timeout := ScSSHClient.Timeout;
  ShowSSHButtons;
  tsShell.TabVisible := FStartShell;
end;

procedure TfrSSHClientFrame.Finalize;
begin
  SaveState;

  inherited;
end;

procedure TfrSSHClientFrame.SaveState;
var
  Registry: TRegistry;
  ChannelList: TStringList;
  i: Integer;
begin
  ChannelList := nil;
  Registry := TRegistry.Create;

  try
    with Registry do begin
      OpenKey(KeyPath, True);
      WriteInteger('Authentication', Integer(ScSSHClient.Authentication));
      WriteString('HostName', ScSSHClient.HostName);
      WriteInteger('Port', ScSSHClient.Port);
      WriteString('User', ScSSHClient.User);
//      WriteString('Password', ScSSHClient.Password);
      WriteString('PrivateKeyName', ScSSHClient.PrivateKeyName);
      WriteString('HostKeyName', ScSSHClient.HostKeyName);
      WriteString('CiphersList', ScSSHClient.CiphersClient.AsString);
      WriteInteger('Timeout', ScSSHClient.Timeout);
      WriteString('RekeyLimit', ScSSHClient.Options.RekeyLimit);
      WriteInteger('ServerAliveCountMax', ScSSHClient.Options.ServerAliveCountMax);
      WriteInteger('ServerAliveInterval', ScSSHClient.Options.ServerAliveInterval);
      WriteBool('TCPKeepAlive', ScSSHClient.Options.TCPKeepAlive);

      WriteInteger('SFTPVersion', Integer(ScSFTPClient.Version));
      WriteInteger('SFTPTimeout', ScSFTPClient.Timeout);
      WriteInteger('SFTPDowloadBlockSize', ScSFTPClient.ReadBlockSize);
      WriteInteger('SFTPUploadBlockSize', ScSFTPClient.WriteBlockSize);

      WriteString('SFTP start directory', FSFTPStartDir);
      WriteBool('Start shell', FStartShell);
      WriteBool('Log events', FNeedForLog);
      if FLogFilename <> '' then
        WriteString('Log file', ExpandFileName(FLogFilename))
      else
        WriteString('Log file', '');

      ChannelList := TStringList.Create;
      GetKeyNames(ChannelList);
      for i := 0 to ChannelList.Count - 1 do
        Registry.DeleteKey(ChannelList[i]);
      CloseKey;

      for i := 0 to Length(FSSHChannelList) - 1 do begin
        OpenKey(KeyPath + '\PortForwarding' + IntToStr(i), True);
        WriteBool('Remote', FSSHChannelList[i].Remote);
        WriteInteger('SourcePort', FSSHChannelList[i].SourcePort);
        WriteString('DestHost', FSSHChannelList[i].DestHost);
        WriteInteger('DestPort', FSSHChannelList[i].DestPort);
        CloseKey;
      end;
    end;
  finally
    Registry.Free;
    ChannelList.Free;
  end;
end;

procedure TfrSSHClientFrame.LoadState;
var
  SSHChannel: TScSSHChannel;
  Registry: TRegistry;
  ChannelList: TStringList;
  i: Integer;
begin
  ChannelList := nil;
  Registry := TRegistry.Create;

  try
    with Registry do begin
      if OpenKey(KeyPath, False) then begin
        if ValueExists('Authentication') then
          ScSSHClient.Authentication := TScSSHAuthentication(ReadInteger('Authentication'));
        if ValueExists('HostName') then
          ScSSHClient.HostName := ReadString('HostName');
        if ValueExists('Port') then
          ScSSHClient.Port := ReadInteger('Port');
        if ValueExists('User') then
          ScSSHClient.User := ReadString('User');
        if ValueExists('Password') then
          ScSSHClient.Password := ReadString('Password');
        if ValueExists('PrivateKeyName') then
          ScSSHClient.PrivateKeyName := ReadString('PrivateKeyName');
        if ValueExists('HostKeyName') then
          ScSSHClient.HostKeyName := ReadString('HostKeyName');
        if ValueExists('CiphersList') then
          ScSSHClient.CiphersClient.AsString := ReadString('CiphersList');
        ScSSHClient.CiphersServer.AsString := ScSSHClient.CiphersClient.AsString;

        if ValueExists('Timeout') then
          ScSSHClient.Timeout := ReadInteger('Timeout');
        if ValueExists('RekeyLimit') then
          ScSSHClient.Options.RekeyLimit := ReadString('RekeyLimit');
        if ValueExists('ServerAliveCountMax') then
          ScSSHClient.Options.ServerAliveCountMax := ReadInteger('ServerAliveCountMax');
        if ValueExists('ServerAliveInterval') then
          ScSSHClient.Options.ServerAliveInterval := ReadInteger('ServerAliveInterval');
        if ValueExists('TCPKeepAlive') then
          ScSSHClient.Options.TCPKeepAlive := ReadBool('TCPKeepAlive');

        if ValueExists('SFTPVersion') then
          ScSFTPClient.Version := TScSFTPVersion(ReadInteger('SFTPVersion'));
        if ValueExists('SFTPTimeout') then
          ScSFTPClient.Timeout := ReadInteger('SFTPTimeout');
        if ValueExists('SFTPDowloadBlockSize') then
          ScSFTPClient.ReadBlockSize := ReadInteger('SFTPDowloadBlockSize');
        if ValueExists('SFTPUploadBlockSize') then
          ScSFTPClient.WriteBlockSize := ReadInteger('SFTPUploadBlockSize');

        if ValueExists('SFTP start directory') then
          FSFTPStartDir := ReadString('SFTP start directory');
        if ValueExists('Start shell') then
          FStartShell := ReadBool('Start shell');
        if ValueExists('Log file') then
          FLogFilename := ReadString('Log file');
        if ValueExists('Log events') then
          FNeedForLog := ReadBool('Log events');

        ChannelList := TStringList.Create;
        GetKeyNames(ChannelList);
        CloseKey;

        SetLength(FSSHChannelList, ChannelList.Count);
        for i := 0 to ChannelList.Count - 1 do begin
          if OpenKey(KeyPath + '\' + ChannelList[i], False) then begin
            SSHChannel := TScSSHChannel.Create(ScSSHClient);
            SSHChannel.GatewayPorts := True;
            SSHChannel.Client := ScSSHClient;
            SSHChannel.Timeout := ScSSHClient.Timeout;

            if ValueExists('Remote') then
              SSHChannel.Remote := ReadBool('Remote');
            if ValueExists('SourcePort') then
              SSHChannel.SourcePort := ReadInteger('SourcePort');
            if ValueExists('DestHost') then
              SSHChannel.DestHost := ReadString('DestHost');
            if ValueExists('DestPort') then
              SSHChannel.DestPort := ReadInteger('DestPort');

            FSSHChannelList[i] := SSHChannel;
            CloseKey;
          end;
        end;
      end;
    end;
  finally
    Registry.Free;
    ChannelList.Free;
  end;
end;

function TfrSSHClientFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\SecureBridge\SSHClient';
end;

procedure TfrSSHClientFrame.Log(Message: string);
var
  fs: TFileStream;
  Buf: TBytes;
begin
  if not (FNeedForLog and (FLogFilename <> '')) then
    Exit;

  fs := nil;
  FLockLog.Enter;
  try
    SetLength(Buf, 0);
    try
      fs := TFileStream.Create(FLogFilename, fmOpenReadWrite + fmShareDenyNone);
    except
      fs := TFileStream.Create(FLogFilename, fmCreate);
    end;

    fs.Seek(0, soFromEnd);
    Message := TimeToStr(Now) + #9 + Message + #13#10;
    Buf := Encoding.ASCII.GetBytes(Message);
    fs.Write(Buf[0], Length(Buf));
  finally
    fs.Free;
    FLockLog.Leave;
  end;
end;

procedure TfrSSHClientFrame.btSettingsClick(Sender: TObject);
var
  i: Integer;
begin
  fmSettings.Init(ScSSHClient, ScSFTPClient, ScFileStorage, FSSHChannelList);
  fmSettings.cbStartShell.Checked := FStartShell;
  fmSettings.cbLog.Checked := FNeedForLog;
  fmSettings.edLogFile.Text := FLogFilename;
  fmSettings.edStartDir.Text := FSFTPStartDir;
  fmSettings.ShowModal;

  if fmSettings.ModalResult = mrOk then begin
    for i := 0 to Length(FSSHChannelList) - 1 do
      FSSHChannelList[i].Free;

    FSSHChannelList := fmSettings.SSHChannelList;

    FStartShell := fmSettings.cbStartShell.Checked;
    tsShell.TabVisible := FStartShell;
    FNeedForLog := fmSettings.cbLog.Checked;
    FLogFilename := fmSettings.edLogFile.Text;
    FSFTPStartDir := fmSettings.edStartDir.Text;
    edRootDir.Text := FSFTPStartDir;
  end;
end;

procedure TfrSSHClientFrame.ScSSHClientServerKeyValidate(Sender: TObject;
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

procedure TfrSSHClientFrame.ScSSHClientAuthenticationPrompt(Sender: TObject;
  const Name, Instruction: string; const Prompts: TStringDynArray;
  var Responses: TStringDynArray);
begin
  if (Name = '') and (Length(Prompts) = 0) then
    Exit;

  fmPrompt.InitForm(Name, Instruction, Prompts);
  fmPrompt.ShowModal;
  fmPrompt.GetResponse(Responses);
end;

function TfrSSHClientFrame.SSHChannelLogMessage(SSHChannel: TScSSHChannel;
  const Event: string; IsError: Boolean = False): string;
begin
  if SSHChannel.Remote then
    Result := 'Remote'
  else
    Result := 'Local';

  Result := Result + ' port forwarding from port ' +
    IntToStr(SSHChannel.SourcePort) + ' to ' + SSHChannel.DestHost + ':' +
    IntToStr(SSHChannel.DestPort);

  If IsError then
    Result := 'Error on ' + Result + ' :';

  Result := Result + ' ' + Event + '.';
end;

procedure TfrSSHClientFrame.btConnectClick(Sender: TObject);
const
  ErrMsg = 'The following error is arised on %s: '#13#10 +
    '%s' + #13#10 + 'Disconnect SSH connection?';
var
  OldCursor: TCursor;
  msg: string;
  i: Integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    // SSH connection
    try
      ScSSHClient.Connect;
    except
      on E: Exception do begin
        msg := 'Error on establishing SSH connection to ' + ScSSHClient.HostName +
          ':' + IntToStr(ScSSHClient.Port) + ' : ' + E.Message;
        Log(msg);
        raise;
      end;
    end;

    // Port forwarding (local/remote)
    for i := 0 to Length(FSSHChannelList) - 1 do begin
      try
        FSSHChannelList[i].OnDisconnect := OnScSSHChannelDisconnect;
        FSSHChannelList[i].OnSocketConnect := OnSocketConnect;
        FSSHChannelList[i].OnSocketDisconnect := OnSocketDisconnect;
        FSSHChannelList[i].OnError := OnScSSHChannelError;
        FSSHChannelList[i].Connect;

        Log(SSHChannelLogMessage(FSSHChannelList[i], 'established'));
      except
        on E: Exception do begin
          Log(SSHChannelLogMessage(FSSHChannelList[i], E.Message, True));

          if MessageDlg(Format(ErrMsg, ['establishing port forwarding', E.Message]),
            mtError, [mbYes, mbNo], 0) = mrYes then begin
            DisconnectAll;
            Exit;
          end;
        end;
      end;
    end;

    // SFTP connection
    try
      ScSFTPClient.Initialize;
      Log('SFTP connection initialized.');
      ShowSFTPButtons(True);
      OpenDir(FSFTPStartDir);
    except
      on E: Exception do begin
        Log('Error on establishing SFTP connection: ' + E.Message);


        if MessageDlg(Format(ErrMsg, ['establishing SFTP connection', E.Message]),
          mtError, [mbYes, mbNo], 0) = mrYes then begin
          DisconnectAll;
          Exit;
        end;
      end;
    end;

    // Command shell
    if FStartShell then
      try
        ScSSHShell.Connect;
        Log('Command shell opened.');
      except
        on E: Exception do begin
          Log('Error on opening command shell: ' + E.Message);

          if MessageDlg(Format(ErrMsg, ['opening command shell', E.Message]),
            mtError, [mbYes, mbNo], 0) = mrYes then begin
            DisconnectAll;
            Exit;
          end;
        end;
      end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfrSSHClientFrame.btDisconnectClick(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TfrSSHClientFrame.DisconnectAll;
var
  i: Integer;
begin
  for i := 0 to Length(FSSHChannelList) - 1 do begin
    try
      FSSHChannelList[i].Disconnect;
    except
      on E: Exception do
        ApplicationHandleException(E);
    end;
  end;

  try
    ScSSHShell.Disconnect;
    ScSFTPClient.Disconnect;
  finally
    ScSSHClient.Disconnect;
    ShowSSHButtons;
  end;
end;

procedure TfrSSHClientFrame.ShowSSHButtons;
var
  Connected: boolean;
begin
  Connected := ScSSHClient.Connected;
  btConnect.Enabled := not Connected;
  btDisconnect.Enabled := Connected;
  btSettings.Enabled := not Connected;
  PageControl.Enabled := Connected;

  if PageControl.Enabled then
    FileView.Color := clWindow
  else
    FileView.Color := cl3DLight;

  edRootDir.Text := FSFTPStartDir;
  FileView.Items.Clear;
  edCommand.Text := '';
  mmShell.Clear;
end;

procedure TfrSSHClientFrame.ScSSHClientAfterConnect(Sender: TObject);
begin
  ShowSSHButtons;
  Log('SSH connection to server: "' + ScSSHClient.HostName + '" port: ' +
    IntToStr(ScSSHClient.Port) + ' established.');
end;

procedure TfrSSHClientFrame.ScSSHClientAfterDisconnect(Sender: TObject);
begin
  ShowSSHButtons;
  Log('SSH connection to server: "' + ScSSHClient.HostName + '" port: ' +
    IntToStr(ScSSHClient.Port) + ' disconnected.');
  DisconnectAll;
end;

procedure TfrSSHClientFrame.ShowSFTPButtons(Connected: boolean);
begin
  btMakeDir.Enabled := Connected;
  btRename.Enabled := Connected;
  btRemove.Enabled := Connected;
  btDownload.Enabled := Connected;
  btUpload.Enabled := Connected;
  btViewFile.Enabled := Connected;
end;

procedure TfrSSHClientFrame.ScSFTPClientDisconnect(Sender: TObject);
begin
  ShowSFTPButtons(False);
  Log('SFTP connection disconnected.');
end;

procedure TfrSSHClientFrame.ScSFTPClientError(Sender: TObject;
  Operation: TScSFTPOperation; const FileName: string;
  const Handle: TBytes; ErrorCode: Integer; const ErrorMessage: string;
  var Fail: Boolean);
begin
  Log('SFTP connection error: ' + ErrorMessage);
end;

procedure TfrSSHClientFrame.OpenDir(Path: string; const SelectedName: string = '');
var
  OldCursor: TCursor;
  Handle: TScSFTPFileHandle;
  i: Integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    Path := Trim(Path);
    Handle := ScSFTPClient.OpenDirectory(Path);
    try
      FileView.Items.Clear;
      repeat
        ScSFTPClient.ReadDirectory(Handle);
      until ScSFTPClient.EOF;
    finally
      ScSFTPClient.CloseHandle(Handle);
    end;

    try
      edRootDir.Text := ScSFTPClient.RetrieveAbsolutePath(Path);
    except
      edRootDir.Text := Path;
    end;

    if SelectedName <> '' then begin
      for i := 0 to FileView.Items.Count - 1 do
        if LowerCase(FileView.Items[i].Text) = LowerCase(SelectedName) then begin
          FileView.Selected := FileView.Items[i];
          Exit;
        end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

function TfrSSHClientFrame.GetRootDir: string;
begin
  Result := edRootDir.Text;
  if IsDelimiter('/', Result, Length(Result)) and (Result[Length(Result)] = '/') then
    Exit;
  if IsDelimiter('\', Result, Length(Result)) and (Result[Length(Result)] = '\') then
    Exit;

  Result := Result + '/';
end;

function TfrSSHClientFrame.GetSelectedNode: TTreeNode;
begin
  Result := nil;
  if FileView.Items.Count = 0 then
    Exit;

  Result := FileView.Selected;
  if Result = nil then
    Result := FileView.Items[0];
end;

procedure TfrSSHClientFrame.FileViewCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare := Node2.ImageIndex - Node1.ImageIndex;
  if Compare = 0 then
    if LowerCase(Node1.Text) >= LowerCase(Node2.Text) then
      Compare := 1
    else
      Compare := -1;
end;

procedure TfrSSHClientFrame.FileViewDblClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := GetSelectedNode;
  if Node = nil then
    Exit;

  case Node.ImageIndex of
    0:
      btViewFileClick(Sender);
    1:
      OpenDir(GetRootDir + Node.Text, '..');
    2:
      OpenDir(GetRootDir + Node.Text, ExtractFileName(StringReplace(edRootDir.Text, '/', PathDelim, [rfReplaceAll])));
  end;
end;

procedure TfrSSHClientFrame.FileViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FileViewDblClick(Sender);
end;

procedure TfrSSHClientFrame.ScSFTPClientCreateLocalFile(Sender: TObject;
  const LocalFileName, RemoteFileName: string;
  Attrs: TScSFTPFileAttributes; var Handle: Cardinal);
var
  dwFlags: DWORD;
begin
  if aAttrs in Attrs.ValidAttributes then begin
    dwFlags := 0;
    if faReadonly in Attrs.Attrs then
      dwFlags := dwFlags or FILE_ATTRIBUTE_READONLY;
    if faSystem in Attrs.Attrs then
      dwFlags := dwFlags or FILE_ATTRIBUTE_SYSTEM;
    if faHidden in Attrs.Attrs then
      dwFlags := dwFlags or FILE_ATTRIBUTE_HIDDEN;
    if faArchive in Attrs.Attrs then
      dwFlags := dwFlags or FILE_ATTRIBUTE_ARCHIVE;
    if faCompressed in Attrs.Attrs then
      dwFlags := dwFlags or FILE_ATTRIBUTE_COMPRESSED;
  end
  else
    dwFlags := FILE_ATTRIBUTE_NORMAL;

  Handle := CreateFile(PChar(LocalFileName),
    GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, dwFlags, 0);
end;

procedure TfrSSHClientFrame.ScSFTPClientDirectoryList(Sender: TObject;
  const Path: string; const Handle: TBytes; FileInfo: TScSFTPFileInfo; EOF: Boolean);
var
  Node: TTreeNode;
begin
  if (FileInfo = nil) or (FileInfo.Filename = '.') then
    Exit;

  Node := FileView.Items.Add(nil, '');
  if FileInfo.Filename = '..' then begin
    Node.ImageIndex := 2;
    Node.SelectedIndex := 2;
  end
  else if (Length(FileInfo.Longname) > 0) and (FileInfo.Longname[1] = 'd') then begin
    Node.ImageIndex := 1;
    Node.SelectedIndex := 1;
  end;

  Node.Text := FileInfo.Filename; // for sorting
end;

procedure TfrSSHClientFrame.btMakeDirClick(Sender: TObject);
var
  DirName: string;
begin
  fmNewName.lbCaption.Caption := 'New directory';
  fmNewName.edNewName.Text := '';

  if fmNewName.ShowModal = mrOk then begin
    DirName := Trim(fmNewName.edNewName.Text);
    if DirName <> '' then
      ScSFTPClient.MakeDirectory(GetRootDir + DirName);
    OpenDir(edRootDir.Text, DirName);
  end;
end;

procedure TfrSSHClientFrame.btRenameClick(Sender: TObject);
const
  RENAME_MSG = 'Rename "%s" to';
var
  Node: TTreeNode;
  NewName: string;
begin
  Node := GetSelectedNode;
  if (Node = nil) or (Node.ImageIndex = 2) then begin
    MessageDlg(SNoFilesSelected, mtConfirmation, [mbOk], 0);
    Exit;
  end;

  fmNewName.lbCaption.Caption := Format(RENAME_MSG, [Node.Text]);
  fmNewName.edNewName.Text := Node.Text;

  if fmNewName.ShowModal = mrOk then begin
    NewName := Trim(fmNewName.edNewName.Text);
    if (NewName <> '') and (NewName <> Node.Text) then begin
      ScSFTPClient.RenameFile(GetRootDir + Node.Text, GetRootDir + NewName);
      OpenDir(edRootDir.Text, NewName);
    end;
  end;
end;

procedure TfrSSHClientFrame.btRemoveClick(Sender: TObject);
const
  DEL_MSG = 'Do you really want to delete the %s %s?';
var
  Node: TTreeNode;
begin
  Node := GetSelectedNode;
  if (Node = nil) or (Node.ImageIndex = 2) then begin
    MessageDlg(SNoFilesSelected, mtConfirmation, [mbOk], 0);
    Exit;
  end;

  case Node.ImageIndex of
    0: begin
      if MessageDlg(Format(DEL_MSG, ['file', Node.Text]), mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
        ScSFTPClient.RemoveFile(GetRootDir + Node.Text);
        OpenDir(edRootDir.Text);
      end;
    end;
    1: begin
      if MessageDlg(Format(DEL_MSG, ['directory', Node.Text]), mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
        ScSFTPClient.RemoveDirectory(GetRootDir + Node.Text);
        OpenDir(edRootDir.Text);
      end;
    end;
  end;
end;

procedure TfrSSHClientFrame.btDownloadClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := GetSelectedNode;
  if (Node = nil) or (Node.ImageIndex <> 0) then begin
    MessageDlg(SNoFilesSelected, mtConfirmation, [mbOk], 0);
    Exit;
  end;

  SaveDialog.Title := 'Download file';
  SaveDialog.Filter := 'All files (*.*)|*.*';
  SaveDialog.FileName := Node.Text;
  if SaveDialog.Execute then
    ScSFTPClient.DownloadFile(GetRootDir + Node.Text, SaveDialog.FileName, False);
end;

procedure TfrSSHClientFrame.btUploadClick(Sender: TObject);
begin
  OpenDialog.Title := 'Upload file';
  OpenDialog.Filter := 'All files (*.*)|*.*';
  if OpenDialog.Execute then begin
    ScSFTPClient.UploadFile(OpenDialog.FileName, GetRootDir + ExtractFileName(OpenDialog.FileName), False);
    OpenDir(edRootDir.Text, ExtractFileName(OpenDialog.FileName));
  end;
end;

procedure TfrSSHClientFrame.btViewFileClick(Sender: TObject);
var
  Node: TTreeNode;
  Handle: TScSFTPFileHandle;
  FileOffset: Int64;
  Buffer: TBytes;
  Count: Integer;
begin
  SetLength(Handle, 0);
  Node := GetSelectedNode;
  if (Node = nil) or (Node.ImageIndex <> 0) then begin
    MessageDlg(SNoFilesSelected, mtConfirmation, [mbOk], 0);
    Exit;
  end;

  fmFileView.Caption := Node.Text;
  fmFileView.Memo.Clear;

  Handle := ScSFTPClient.OpenFile(GetRootDir + Node.Text, [foRead]);
  try
    SetLength(Buffer, 32768);
    FileOffset := 0;

    repeat
      Count := ScSFTPClient.ReadFile(Handle, FileOffset, Buffer, 0, Length(Buffer));
      if Count > 0 then
        fmFileView.Memo.Lines.Text := fmFileView.Memo.Lines.Text + string(Encoding.ASCII.GetString(Buffer, 0, Count));
      FileOffset := FileOffset + Count;
    until ScSFTPClient.EOF;
  finally
    ScSFTPClient.CloseHandle(Handle);
  end;

  fmFileView.ShowModal;
end;

procedure TfrSSHClientFrame.ScSSHShellAsyncReceive(Sender: TObject);
begin
  mmShell.Lines.Text := mmShell.Lines.Text + ScSSHShell.ReadString;
  mmShell.SelStart := Length(mmShell.Lines.Text);
  mmShell.SelLength := 0;
end;

procedure TfrSSHClientFrame.sbExecuteClick(Sender: TObject);
begin
  if edCommand.Text <> '' then begin
    ScSSHShell.WriteString(edCommand.Text + #13#10);
    edCommand.Text := '';
  end;
end;

procedure TfrSSHClientFrame.edCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    sbExecuteClick(Sender);
end;

procedure TfrSSHClientFrame.ScSSHShellDisconnect(Sender: TObject);
begin
  Log('Command shell closed.');
end;

procedure TfrSSHClientFrame.OnScSSHChannelDisconnect(Sender: TObject);
begin
  Log(SSHChannelLogMessage(Sender as TScSSHChannel, 'closed'));
end;

procedure TfrSSHClientFrame.OnScSSHChannelError(Sender: TObject; E: Exception);
begin
  Log(SSHChannelLogMessage(Sender as TScSSHChannel, E.Message, True));
end;

procedure TfrSSHClientFrame.OnSocketConnect(Sender: TObject;
  const SockAddr: PSockAddr);
var
  Sock: TIPEndPoint;
  s: string;
begin
  Sock := TIPEndPoint.Create(PSockAddrIn(SockAddr));

  s := 'Connection from IP ' + Sock.ToString + ' to ' +
    (Sender as TScSSHChannel).DestHost + ':' +
    IntToStr((Sender as TScSSHChannel).DestPort) + ' established.';
  Log(s);
end;

procedure TfrSSHClientFrame.OnSocketDisconnect(Sender: TObject;
  const SockAddr: PSockAddr);
var
  Sock: TIPEndPoint;
  s: string;
begin
  Sock := TIPEndPoint.Create(PSockAddrIn(SockAddr));

  s := 'Connection from IP ' + Sock.ToString + ' to ' +
    (Sender as TScSSHChannel).DestHost + ':' +
    IntToStr((Sender as TScSSHChannel).DestPort) + ' disconnected.';
  Log(s);
end;

{$IFDEF VER130}
initialization
  ApplicationHandleException := Application.HandleException;
{$ENDIF}
end.
