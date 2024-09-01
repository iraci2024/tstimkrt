unit SFTPClientFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
{$IFNDEF VER130}
  Types,
{$ENDIF}
  Classes, SysUtils, Windows, Messages, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, ToolWin, ComCtrls,
  Buttons, DemoFrame, Grids, Spin, SyncObjs, ImgList,
  ScCLRClasses, ScTypes, ScUtils, ScBridge, ScSSHClient, ScSFTPUtils, ScSFTPClient;

type
  TSFTPClientFrame = class(TDemoFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
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
    rbKeyboardInteractive: TRadioButton;
    Panel12: TPanel;
    btKeyGen: TSpeedButton;
    ScFileStorage: TScFileStorage;
    Panel10: TPanel;
    btImportKey: TSpeedButton;
    OpenDialog: TOpenDialog;
    seSSHPort: TSpinEdit;
    cbRandomization: TCheckBox;
    ImageList1: TImageList;
    ScSFTPClient: TScSFTPClient;
    Panel3: TPanel;
    btConnectSSH: TSpeedButton;
    btDisconnectSSH: TSpeedButton;
    SaveDialog: TSaveDialog;
    Panel9: TPanel;
    Panel8: TPanel;
    btStart: TSpeedButton;
    btStop: TSpeedButton;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel11: TPanel;
    FileView: TTreeView;
    Panel4: TPanel;
    btMakeDir: TSpeedButton;
    btRemove: TSpeedButton;
    btRename: TSpeedButton;
    btUpload: TSpeedButton;
    btDownload: TSpeedButton;
    btViewFile: TSpeedButton;
    lbRootDir: TEdit;
    Panel7: TPanel;
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
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btImportKeyClick(Sender: TObject);
    procedure ScSFTPClientDirectoryList(Sender: TObject; const Path: string;
      const Handle: TBytes; FileInfo: TScSFTPFileInfo; EOF: Boolean);
    procedure FileViewCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure FileViewDblClick(Sender: TObject);
    procedure FileViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btMakeDirClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btRenameClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btViewFileClick(Sender: TObject);
    procedure ScSSHClientAuthenticationPrompt(Sender: TObject; const Name,
      Instruction: string; const Prompts: TStringDynArray;
      var Responses: TStringDynArray);
    procedure ScSFTPClientCreateLocalFile(Sender: TObject; const LocalFileName,
      RemoteFileName: string; Attrs: TScSFTPFileAttributes; var Handle: Cardinal);
  private
    procedure CheckRandomize;
    procedure ShowSSHButtons(Connected: boolean);
    procedure ShowSFTPButtons(Connected: boolean);
    procedure DisconnectSFTP;
    procedure DisconnectAll;
    function LoadState: boolean;
    function SaveState: boolean;
    function KeyPath: string;
    procedure OpenDir(const Path: string; const SelectedName: string = '');
    procedure RemoveDir(const Path: string);
    function GetRootDir: string;
    function GetSelectedNode: TTreeNode;

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
  ScSSHUtils, SFTPClientDemoForm, ScConsts, NewNameForm, FileViewForm,
  PromptForm;

const
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
const
  SNoFilesSelected = 'No files selected!';

destructor TSFTPClientFrame.Destroy;
begin
  DisconnectAll;

  inherited;
end;

procedure TSFTPClientFrame.Initialize;
begin
  inherited;

  LoadState;
  edSSHHost.Text := ScSSHClient.HostName;
  seSSHPort.Value := ScSSHClient.Port;
  edSSHUserName.Text := ScSSHClient.User;
  lbRootDir.Text := '.';
end;

procedure TSFTPClientFrame.Finalize;
begin
  SaveState;

  inherited;
end;

function TSFTPClientFrame.SaveState: boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do begin
      OpenKey(KeyPath + '\' + TSFTPClientFrame.ClassName, True);
      WriteString('SSHHost', ScSSHClient.HostName);
      WriteInteger('SSHPort', ScSSHClient.Port);
      WriteString('SSHUserName', ScSSHClient.User);
      WriteBool('Silent randomization', cbRandomization.Checked);
    end;
  finally
    Registry.Free;
  end;

  Result := True;
end;

function TSFTPClientFrame.LoadState: boolean;
var
  Registry: TRegistry;
begin
  Result := False;
  Registry := TRegistry.Create;
  try
    with Registry do begin
      if OpenKey(KeyPath + '\' + TSFTPClientFrame.ClassName, False) then begin
        if ValueExists('SSHHost') then
          ScSSHClient.HostName := ReadString('SSHHost');
        if ValueExists('SSHPort') then
          ScSSHClient.Port := ReadInteger('SSHPort');
        if ValueExists('SSHUserName') then
          ScSSHClient.User := ReadString('SSHUserName');
        if ValueExists('Silent randomization') then
          cbRandomization.Checked := ReadBool('Silent randomization');
        Result := True;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSFTPClientFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\SecureBridge\Demos';
end;

procedure TSFTPClientFrame.CheckRandomize;
begin
  if not SFTPClientForm.Randomized and not cbRandomization.Checked then begin
    SFTPClientForm.Randomize;
    if not SFTPClientForm.Randomized and not cbRandomization.Checked then
      raise Exception.Create('Data for the random generator has not been generated');
  end;
end;

procedure TSFTPClientFrame.DisconnectAll;
begin
  DisconnectSFTP;
  ScSSHClient.Disconnect;
  ShowSSHButtons(False);
end;

procedure TSFTPClientFrame.ShowSSHButtons(Connected: boolean);
begin
  btConnectSSH.Enabled := not Connected;
  btDisconnectSSH.Enabled := Connected;
  btStart.Enabled := Connected;
end;

procedure TSFTPClientFrame.ShowSFTPButtons(Connected: boolean);
begin
  btStart.Enabled := not Connected and ScSSHClient.Connected;
  btStop.Enabled := Connected;
  btUpload.Enabled := Connected;
  btDownload.Enabled := Connected;
  btViewFile.Enabled := Connected;
  btMakeDir.Enabled := Connected;
  btRemove.Enabled := Connected;
  btRename.Enabled := Connected;
end;

procedure TSFTPClientFrame.DisconnectSFTP;
begin
  try
    ScSFTPClient.Disconnect;
  finally
    ShowSFTPButtons(False);
    lbRootDir.Text := '.';
    FileView.Items.Clear;
  end;
end;


procedure TSFTPClientFrame.ScSSHClientAfterConnect(Sender: TObject);
begin
  ShowSSHButtons(True);
end;

procedure TSFTPClientFrame.ScSSHClientAfterDisconnect(Sender: TObject);
begin
  ShowSSHButtons(False);
  DisconnectSFTP;
end;

procedure TSFTPClientFrame.btConnectSSHClick(Sender: TObject);
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

procedure TSFTPClientFrame.btDisconnectSSHClick(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSFTPClientFrame.rbPasswordClick(Sender: TObject);
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

procedure TSFTPClientFrame.edSSHUserNameChange(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSFTPClientFrame.cbPrivateKeyDropDown(Sender: TObject);
begin
  ScFileStorage.Keys.GetKeyNames(cbPrivateKey.Items);
end;

procedure TSFTPClientFrame.btKeyGenClick(Sender: TObject);
begin
  CheckRandomize;

  if cbPrivateKey.Text = '' then
    cbPrivateKey.Text := 'client_key';

  SFTPClientForm.GenerateClientKey(ScFileStorage, cbPrivateKey.Text);
end;

procedure TSFTPClientFrame.ScSSHClientServerKeyValidate(Sender: TObject;
  NewServerKey: TScKey; var Accept: Boolean);
var
  CurHostKeyName: string;
begin
  if ScSSHClient.HostKeyName = '' then
    CurHostKeyName := ScSSHClient.HostName
  else
    CurHostKeyName := ScSSHClient.HostKeyName;

  SFTPClientForm.DoServerKeyValidate(ScFileStorage, CurHostKeyName, NewServerKey, Accept);
end;

procedure TSFTPClientFrame.btImportKeyClick(Sender: TObject);
var
  KeyName: string;
begin
  KeyName := edSSHHost.Text;
  SFTPClientForm.ImportServerKey(ScFileStorage, KeyName);
  edSSHHost.Text := KeyName;
end;

procedure TSFTPClientFrame.ScSSHClientBeforeConnect(Sender: TObject);
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

procedure TSFTPClientFrame.btStartClick(Sender: TObject);
begin
  ScSFTPClient.Initialize;
  ShowSFTPButtons(True);
  OpenDir(lbRootDir.Text);
end;

procedure TSFTPClientFrame.btStopClick(Sender: TObject);
begin
  DisconnectSFTP;
end;

procedure TSFTPClientFrame.OpenDir(const Path: string; const SelectedName: string = '');
var
  OldCursor: TCursor;
  Handle: TScSFTPFileHandle;
  RootDir: string;
  i: Integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    RootDir := ScSFTPClient.RetrieveAbsolutePath(Trim(Path));
    Handle := ScSFTPClient.OpenDirectory(RootDir);
    lbRootDir.Text := RootDir;

    try
      FileView.Items.Clear;
      while not ScSFTPClient.EOF(Handle) do
        ScSFTPClient.ReadDirectory(Handle);
    finally
      ScSFTPClient.CloseHandle(Handle);
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

procedure TSFTPClientFrame.ScSFTPClientDirectoryList(Sender: TObject;
  const Path: string; const Handle: TBytes; FileInfo: TScSFTPFileInfo;
  EOF: Boolean);
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

procedure TSFTPClientFrame.FileViewCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare := Node2.ImageIndex - Node1.ImageIndex;
  if Compare = 0 then
    if LowerCase(Node1.Text) >= LowerCase(Node2.Text) then
      Compare := 1
    else
      Compare := -1;
end;

function TSFTPClientFrame.GetRootDir: string;
begin
  Result := lbRootDir.Text;
  if IsDelimiter('/', Result, Length(Result)) and (Result[Length(Result)] = '/') then
    Exit;
  if IsDelimiter('\', Result, Length(Result)) and (Result[Length(Result)] = '\') then
    Exit;

  Result := Result + '/';
end;

function TSFTPClientFrame.GetSelectedNode: TTreeNode;
begin
  Result := nil;
  if FileView.Items.Count = 0 then
    Exit;

  Result := FileView.Selected;
  if Result = nil then
    Result := FileView.Items[0];
end;

procedure TSFTPClientFrame.FileViewDblClick(Sender: TObject);
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
      OpenDir(GetRootDir + Node.Text, ExtractFileName(StringReplace(lbRootDir.Text, '/', PathDelim, [rfReplaceAll])));
  end;
end;

procedure TSFTPClientFrame.FileViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FileViewDblClick(Sender);
end;

procedure TSFTPClientFrame.btMakeDirClick(Sender: TObject);
var
  DirName: string;
begin
  fmNewName.lbCaption.Caption := 'New directory';
  fmNewName.edNewName.Text := '';

  if fmNewName.ShowModal = mrOk then begin
    DirName := Trim(fmNewName.edNewName.Text);
    if DirName <> '' then
      ScSFTPClient.MakeDirectory(GetRootDir + DirName);
    OpenDir(lbRootDir.Text, DirName);
  end;
end;

procedure TSFTPClientFrame.RemoveDir(const Path: string);
var
  OldCursor: TCursor;
  Handle: TScSFTPFileHandle;
  List: TCRObjectList;
  FileInfo: TScSFTPFileInfo;
  i: integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    Handle := ScSFTPClient.OpenDirectory(Path);
    try
      List := TCRObjectList.Create;
      try
        ScSFTPClient.ReadDirectoryToList(Handle, List);

        for i := 0 to List.Count - 1 do begin
          FileInfo := TScSFTPFileInfo(List[i]);
          if (FileInfo.Filename = '.') or (FileInfo.Filename = '..') then
            Continue;

          if (Length(FileInfo.Longname) > 0) and (FileInfo.Longname[1] = 'd') then
            RemoveDir(Path + '/' + FileInfo.Filename)
          else
            ScSFTPClient.RemoveFile(Path + '/' + FileInfo.Filename);
        end;
      finally
        List.Free;
      end;
    finally
      ScSFTPClient.CloseHandle(Handle);
    end;

    ScSFTPClient.RemoveDirectory(Path);
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TSFTPClientFrame.btRemoveClick(Sender: TObject);
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
        OpenDir(lbRootDir.Text);
      end;
    end;
    1: begin
      if MessageDlg(Format(DEL_MSG, ['directory', Node.Text +  ' and all its contents']), mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
        try
          RemoveDir(GetRootDir + Node.Text);
        finally
          OpenDir(lbRootDir.Text);
        end;
      end;
    end;
  end;
end;

procedure TSFTPClientFrame.btRenameClick(Sender: TObject);
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
      OpenDir(lbRootDir.Text, NewName);
    end;
  end;
end;

procedure TSFTPClientFrame.ScSFTPClientCreateLocalFile(Sender: TObject;
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

procedure TSFTPClientFrame.btUploadClick(Sender: TObject);
begin
  OpenDialog.Title := 'Upload file';
  OpenDialog.Filter := 'All files (*.*)|*.*';
  if OpenDialog.Execute then begin
    ScSFTPClient.UploadFile(OpenDialog.FileName, GetRootDir + ExtractFileName(OpenDialog.FileName), False);
    OpenDir(lbRootDir.Text, ExtractFileName(OpenDialog.FileName));
  end;
end;

procedure TSFTPClientFrame.btDownloadClick(Sender: TObject);
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

procedure TSFTPClientFrame.btViewFileClick(Sender: TObject);
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
    while not ScSFTPClient.EOF(Handle) do begin
      Count := ScSFTPClient.ReadFile(Handle, FileOffset, Buffer, 0, Length(Buffer));
      if Count > 0 then
        fmFileView.Memo.Lines.Text := fmFileView.Memo.Lines.Text + string(Encoding.ASCII.GetString(Buffer, 0, Count));
      FileOffset := FileOffset + Count;
    end;
  finally
    ScSFTPClient.CloseHandle(Handle);
  end;

  fmFileView.ShowModal;
end;

procedure TSFTPClientFrame.ScSSHClientAuthenticationPrompt(Sender: TObject;
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
