
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  ScDesign
//////////////////////////////////////////////////

{$I SB.inc}

unit ScDesign;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls,
{$IFDEF FPC}
  PropEdits, ComponentEditors, //Fieldseditor,
{$ELSE}
  DesignIntf, DesignEditors,
{$ENDIF}
  SysUtils, Classes, TypInfo, {$IFNDEF FPC}Consts,{$ENDIF}
  ScDesignUtils, ScStorageEditor, ScCollectionEditor, ScUtils,
  ScBridge, ScCMS, ScHttp, ScFTPClient, ScSMTPClient,
  ScWebSocketClient, ScSignalRHttpConnection;

type
  TCustomClientClass = class of TComponent;

  TScPasswordProperty = class(TStringProperty)
  protected
    FActivated: boolean;
  public
    procedure Initialize; override;
    procedure Activate; override;
    function GetValue: string; override;
  end;

{$IFNDEF FPC}
  TScClientList = class
  private
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);

  protected
    Items: TStrings;
    Form: TForm;

    procedure StrProc(const S: string);
    function GetClientType: TCustomClientClass; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function GetClient(Component: TComponent; Designer: IDesigner): TComponent;
  end;

  TScSSHClientList = class(TScClientList)
  protected
    function GetClientType: TCustomClientClass; override;
  end;

  TScStorageList = class(TScClientList)
  protected
    function GetClientType: TCustomClientClass; override;
  end;
{$ENDIF}

  TKeyNameEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TScStorageEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
  end;

  TScRekeyLimitEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TScPathEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{$IFDEF MSWINDOWS}
  TScRootKeyEditor = class(TInt64Property)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TProviderNameEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TCertStoreNameEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
{$ENDIF}

  TScCMSProcessorEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
  end;

{$IFNDEF FPC}
  TScDesignNotification = class(TInterfacedObject, IDesignNotification)
  protected
    FItem: TPersistent;
    FClientList: TScClientList;
    procedure StrProc(const S: string);
  public
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); virtual;
    //overide this method on Product level and add all product specific classess
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); virtual; abstract;
    procedure ItemsModified(const ADesigner: IDesigner); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean); virtual;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); virtual;

    function CreateClientList: TScClientList; virtual; abstract;
    function GetClientPropertyName: string; virtual; abstract;
  end;

  TScSSHDesignNotification = class(TScDesignNotification)
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); override;
    function CreateClientList: TScClientList; override;
    function GetClientPropertyName: string; override;
  end;

  TScStorageDesignNotification = class(TScDesignNotification)
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); override;
    function CreateClientList: TScClientList; override;
    function GetClientPropertyName: string; override;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); override;
  end;
{$ENDIF}

  TScHeadersEditor = class (TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  function ShowDialogYesOrNo(const Msg: string): boolean;

procedure Register;

implementation

uses
  {$IFNDEF FPC}ToolsAPI,{$ENDIF}
  ScAlgorithmSupport, ScSSHUtils, FileCtrl,
  ScSSHClient, ScSSHChannel,
{$IFNDEF STD}
  ScSSHServer,
{$ENDIF}
  ScSSLTypes, ScCipherSuites, ScSSLClient, ScSSLServer,
  ScCryptoAPIStorage,
  ScSFTPClient, ScStringMapEditor;

{$IFDEF MSWINDOWS}
const
{$IFDEF FPC}
  RootKeyNames: array[DWORD(HKEY_CLASSES_ROOT) - DWORD(HKEY_CLASSES_ROOT) .. DWORD(HKEY_DYN_DATA) - DWORD(HKEY_CLASSES_ROOT), 0..1] of string = (
{$ELSE}
  RootKeyNames: array[HKEY_CLASSES_ROOT..HKEY_DYN_DATA, 0..1] of string = (
{$ENDIF}
    ('HKEY_CLASSES_ROOT', 'HKCR'),
    ('HKEY_CURRENT_USER', 'HKCU'),
    ('HKEY_LOCAL_MACHINE', 'HKLM'),
    ('HKEY_USERS', 'HKU'),
    ('HKEY_PERFORMANCE_DATA', 'HKPD'),
    ('HKEY_CURRENT_CONFIG', 'HKCC'),
    ('HKEY_DYN_DATA', 'HKDD'));
{$ENDIF}

function ShowDialogYesOrNo(const Msg: string): boolean;
begin
  Result := MessageDlg(Msg, {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtConfirmation,
    [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbYes, {$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbNo], 0) = mrYes;
end;

{ TScPasswordProperty }

procedure TScPasswordProperty.Initialize;
begin
  inherited;
  
  FActivated := False;
end;

function TScPasswordProperty.GetValue: string;
var
  i: Integer;
begin
  Result := inherited GetValue;
  if not FActivated then begin
    for i := 1 to Length(Result) do
      Result[i] := '*';
  end
  else
    FActivated := False;
end;

procedure TScPasswordProperty.Activate;
begin
  inherited;
  
  FActivated := True;
end;

{$IFNDEF FPC}
{ TScClientList }

constructor TScClientList.Create;
begin
  inherited;

  Items := TStringList.Create;
end;

destructor TScClientList.Destroy;
begin
  Items.Free;

  inherited;
end;

procedure TScClientList.StrProc(const S: string);
begin
end;

procedure TScClientList.ListBoxDblClick(Sender: TObject);
begin
  Form.ModalResult := mrOk;
end;

procedure TScClientList.ListBoxKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
      Form.ModalResult := mrOk;
    #27:
      Form.ModalResult := mrCancel;
  end;
end;

function TScClientList.GetClient(Component: TComponent; Designer: IDesigner): TComponent;
const
  Width = 124;
  Height = 180;
var
  ListBox: TListBox;
  TypeData: TTypeData;
  DesignOffset: TPoint;
begin
  TypeData.ClassType := GetClientType;
  Designer.GetComponentNames(@TypeData, StrProc);

  if Items.Count = 0 then
    Result := nil
  else
    if Items.Count = 1 then
      Result := TComponent(Designer.GetComponent(Items[0]))
    else begin
      Form := TForm.Create(nil);
      ListBox := TListBox.Create(Form);
    {$IFDEF MSWINDOWS}
      Form.BorderStyle := bsSizeToolWin;
    {$ENDIF}
      if Designer.Root is TForm then
        DesignOffset := TForm(Designer.Root).BoundsRect.TopLeft
      else
        DesignOffset := Point(LongRec(Designer.Root.DesignInfo).Lo, LongRec(Designer.Root.DesignInfo).Hi);
      Form.Left := DesignOffset.X + Word(Component.DesignInfo) - Width div 3;
      Form.Top := DesignOffset.Y + Word(Component.DesignInfo shr 16) - 5;
      Form.Width := Width;
      Form.Height := Height;
      Form.Caption := 'Client List';
      Form.InsertControl(TControl(ListBox));
      ListBox.Items.Assign(Items);
      ListBox.Align := alClient;
      ListBox.ItemIndex := 0;
      ListBox.OnDblClick := ListBoxDblClick;
      ListBox.OnKeyPress := ListBoxKeyPress;
      if Form.ShowModal = mrOk then
        Result := TComponent(Designer.GetComponent(Items[ListBox.ItemIndex]))
      else
        Result := nil;
      Form.Free;
    end;
end;

{ TScSSHClientList }

function TScSSHClientList.GetClientType: TCustomClientClass;
begin
  Result := TScSSHClient;
end;

{ TScStorageList }

function TScStorageList.GetClientType: TCustomClientClass;
begin
  Result := TScStorage;
end;

{$ENDIF}

{ TKeyNameEditor }

function TKeyNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TKeyNameEditor.GetValues(Proc: TGetStrProc);
var
  List: TStrings;
  i: integer;
begin
  List := TStringList.Create;
  try
    if GetComponent(0) is TScSSHClient then begin
      if TScSSHClient(GetComponent(0)).KeyStorage <> nil then
        TScSSHClient(GetComponent(0)).KeyStorage.Keys.GetKeyNames(List);
    end
  {$IFNDEF STD}
    else if GetComponent(0) is TScSSHServer then begin
      if TScSSHServer(GetComponent(0)).Storage <> nil then
        TScSSHServer(GetComponent(0)).Storage.Keys.GetKeyNames(List);
    end
  {$ENDIF}
    else if GetComponent(0) is TScSSLClient then begin
      if TScSSLClient(GetComponent(0)).Storage <> nil then
        TScSSLClient(GetComponent(0)).Storage.Certificates.GetCertificateNames(List);
    end
    else if GetComponent(0) is TScCMSProcessor then begin
      if TScCMSProcessor(GetComponent(0)).Storage <> nil then
        TScCMSProcessor(GetComponent(0)).Storage.Certificates.GetCertificateNames(List);
    end;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TScStorageEditor }

function TScStorageEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TScStorageEditor.GetVerb(Index: integer): string;
begin
  Result := 'Edit';
end;

procedure TScStorageEditor.ExecuteVerb(Index: integer);
var
  fmStorageForm: TScStorageForm;
begin
  fmStorageForm := TScStorageForm.Create(nil, TScDesignUtils);
  try
    fmStorageForm.InitialProperty := 'Keys';
    fmStorageForm.Component := Component;
    fmStorageForm.ShowModal;
  finally
    fmStorageForm.Free;
  end;
end;

{ TScRekeyLimitEditor }

function TScRekeyLimitEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure TScRekeyLimitEditor.GetValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(ListRekeyLimit) to High(ListRekeyLimit) do
    Proc(ListRekeyLimit[i]);
end;

{$IFNDEF FPC}
{ TScDesignNotification }

procedure TScDesignNotification.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin

end;

procedure TScDesignNotification.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
begin

end;

procedure TScDesignNotification.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TScDesignNotification.StrProc(const S: string);
begin
  FClientList.Items.Add(S);
end;

procedure TScDesignNotification.ItemsModified(const ADesigner: IDesigner);
begin
  if FItem <> nil then
  try
    FClientList := CreateClientList;
    ADesigner.GetComponentNames(GetTypeData(FClientList.GetClientType.ClassInfo), StrProc);
    SetObjectProp(FItem, GetClientPropertyName, FClientList.GetClient(TComponent(FItem), ADesigner));
  finally
    FItem := nil;
    FreeAndNil(FClientList);
    ADesigner.Modified;
  end;
end;

procedure TScDesignNotification.SelectionChanged(
  const ADesigner: IDesigner; const ASelection: IDesignerSelections);
begin

end;

{ TScSSHDesignNotification }

procedure TScSSHDesignNotification.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem <> nil) and ((AItem is TScSSHCustomChannel) or (AItem is TScSFTPClient)) then
    FItem := AItem;
end;

function TScSSHDesignNotification.CreateClientList: TScClientList;
begin
  Result := TScSSHClientList.Create;
end;

function TScSSHDesignNotification.GetClientPropertyName: string;
begin
  Assert(FItem <> nil);

  if FItem is TScSFTPClient then
    Result := 'SSHClient'
  else
    Result := 'Client';
end;

{ TScStorageDesignNotification }

procedure TScStorageDesignNotification.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem <> nil) and
    ((AItem is TScSSHClient) or
  {$IFNDEF STD}
     (AItem is TScSSHServer) or
  {$ENDIF}
     (AItem is TScSSLClient) or
     (AItem is TScCMSProcessor)) then
    FItem := AItem;
end;

function TScStorageDesignNotification.CreateClientList: TScClientList;
begin
  Result := TScStorageList.Create;
end;

function TScStorageDesignNotification.GetClientPropertyName: string;
begin
  Assert(FItem <> nil);

  if FItem is TScSSHClient then
    Result := 'KeyStorage'
  else
    Result := 'Storage';
end;

procedure TScStorageDesignNotification.SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
var
  ModuleServices: IOTAModuleServices;
  CurrentModule: IOTAModule;
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
  s: string;
begin
  CurrentProjectOutputDir := '';

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  CurrentModule := ModuleServices.CurrentModule;
  if CurrentModule.OwnerCount = 0 then
    Exit;

  Project := CurrentModule.Owners[0];
  ProjectOptions := Project.ProjectOptions;
  CurrentProjectOutputDir := Trim(ProjectOptions.Values['OutputDir']);

  if ((CurrentProjectOutputDir <> '') and (CurrentProjectOutputDir[1] = '.')) or
     (CurrentProjectOutputDir = '') then begin
    s := Trim(ExtractFilePath(Project.FileName));
    if s = '' then
      CurrentProjectOutputDir := ''
    else
      CurrentProjectOutputDir := IncludeTrailingBackslash(s) + CurrentProjectOutputDir;
  end;
end;
{$ENDIF}

{ TScPathEditor }

procedure TScPathEditor.Edit;
var
  Dir: string;
begin
  Assert(GetComponent(0) is TScFileStorage);
  Dir := TScFileStorage(GetComponent(0)).Path;
  if (Dir <> '') and (Dir[1] = '.') and (CurrentProjectOutputDir <> '') then
     Dir := IncludeTrailingBackslash(CurrentProjectOutputDir) + Dir;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    TScFileStorage(GetComponent(0)).Path := Dir;
end;

function TScPathEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{$IFDEF MSWINDOWS}
{ TScRootKeyEditor }

function TScRootKeyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

procedure TScRootKeyEditor.GetValues(Proc: TGetStrProc);
var
  i: cardinal;
begin
  for i := Low(RootKeyNames) to High(RootKeyNames) do
    Proc(RootKeyNames[i][0]);
end;

function TScRootKeyEditor.GetValue: string;
begin
  Result := RootKeyNames[GetInt64Value][0];
end;

procedure TScRootKeyEditor.SetValue(const Value: string);
var
  i: cardinal;
begin
  for i := Low(RootKeyNames) to High(RootKeyNames) do
    if (Value = RootKeyNames[i][0]) or (Value = RootKeyNames[i][1]) then begin
      SetOrdValue(i);
      Exit;
    end;
end;

{ TProviderNameEditor }

function TProviderNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure TProviderNameEditor.GetValues(Proc: TGetStrProc);
var
  List: TStrings;
  i: integer;
begin
  Assert(GetComponent(0) is TScCryptoAPIStorage);

  List := TStringList.Create;
  try
    TScCryptoAPIStorage(GetComponent(0)).GetProviderNames(List);

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TCertStoreNameEditor }

function TCertStoreNameEditor.GetAttributes: TPropertyAttributes;
begin
  Assert(GetComponent(0) is TScCryptoAPIStorage);

  if (TScCryptoAPIStorage(GetComponent(0)).CertProviderType = ptSystem) or
     (TScCryptoAPIStorage(GetComponent(0)).CertProviderType = ptSystemRegistry) then
    Result := [paMultiSelect, paValueList]
  else
    Result := inherited GetAttributes;
end;

procedure TCertStoreNameEditor.GetValues(Proc: TGetStrProc);
var
  i: integer;
begin
  Assert(GetComponent(0) is TScCryptoAPIStorage);

  if (TScCryptoAPIStorage(GetComponent(0)).CertProviderType = ptSystem) or
     (TScCryptoAPIStorage(GetComponent(0)).CertProviderType = ptSystemRegistry) then
    for i := Low(ListSystemStoreNames) to High(ListSystemStoreNames) do
      Proc(ListSystemStoreNames[i]);
end;
{$ENDIF}

{ TScCMSProcessorEditor }

function TScCMSProcessorEditor.GetVerbCount: integer;
begin
  Result := 6;
end;

function TScCMSProcessorEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0:
      Result := 'Encrypt file';
    1:
      Result := 'Decrypt file';
    2:
      Result := 'Sign file';
    3:
      Result := 'Check file signature';
    4:
      Result := 'Sign and encrypt file';
    5:
      Result := 'Decrypt and check file signature';
  else
    Result := '';
  end;
end;

procedure TScCMSProcessorEditor.ExecuteVerb(Index: integer);
var
  CMSProcessor: TScCMSProcessor;
  InFileName, OutFileName: string;
  FileOpen: TOpenDialog;
  FileSave: TSaveDialog;
begin
  CMSProcessor := Component as TScCMSProcessor;

  FileOpen := TOpenDialog.Create(Application);
  try
    FileOpen.Title := 'Input file';
    FileOpen.Filename := '';
    FileOpen.Filter := 'All files (*.*)|*.*';
    FileOpen.Options := FileOpen.Options + [ofPathMustExist, ofFileMustExist];

    if FileOpen.Execute then
      InFileName := FileOpen.Filename
    else
      Exit;
  finally
    FileOpen.Free;
  end;

  if Index <> 3 then begin
    FileSave := TSaveDialog.Create(Application);
    try
      FileSave.Title := 'Output file';
      FileSave.Filename := '';
      FileSave.Filter := 'All files (*.*)|*.*';
      FileSave.Options := FileSave.Options + [ofPathMustExist];
      if FileSave.Execute then
        OutFileName := FileSave.Filename
      else
        Exit;
    finally
      FileSave.Free;
    end;
  end;

  case Index of
    0:
      CMSProcessor.Encrypt(InFileName, OutFileName, cePEM);
    1:
      CMSProcessor.Decrypt(InFileName, OutFileName);
    2:
      CMSProcessor.Sign(InFileName, OutFileName, False, cePEM);
    3:
      CMSProcessor.CheckSignature(InFileName);
    4:
      CMSProcessor.SignAndEncrypt(InFileName, OutFileName, '', cePEM);
    5:
      CMSProcessor.DecryptAndCheckSignature(InFileName, OutFileName);
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TScStorage, 'Password', TScPasswordProperty);
  RegisterPropertyEditor(TypeInfo(string), TScSSHClient, 'Password', TScPasswordProperty);
  RegisterPropertyEditor(TypeInfo(string), TScFTPClient, 'Password', TScPasswordProperty);
  RegisterPropertyEditor(TypeInfo(string), TScSMTPClient, 'Password', TScPasswordProperty);

{$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(TScSSHCiphers), TScSSHClient, 'CiphersClient', TScCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TScSSHCiphers), TScSSHClient, 'CiphersServer', TScCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TScSSHHMacAlgorithms), TScSSHClient, 'HMACAlgorithms', TScCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TScSSHHostKeyAlgorithms), TScSSHClient, 'HostKeyAlgorithms', TScCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TScSSHKeyExchangeAlgorithms), TScSSHClient, 'KeyExchangeAlgorithms', TScCollectionEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TScSSHClientOptions, 'RekeyLimit', TScRekeyLimitEditor);
{$IFNDEF STD}
  RegisterPropertyEditor(TypeInfo(string), TScSSHServerOptions, 'RekeyLimit', TScRekeyLimitEditor);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TScFileStorage, 'Path', TScPathEditor);
{$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(cardinal), TScRegStorage, 'RootKey', TScRootKeyEditor);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TScSSHClient, 'PrivateKeyName', TKeyNameEditor);
  RegisterPropertyEditor(TypeInfo(string), TScSSHClient, 'HostKeyName', TKeyNameEditor);
{$IFNDEF STD}
  RegisterPropertyEditor(TypeInfo(string), TScSSHServer, 'KeyNameRSA', TKeyNameEditor);
  RegisterPropertyEditor(TypeInfo(string), TScSSHServer, 'KeyNameDSA', TKeyNameEditor);
{$ENDIF}

{$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(TScSSLCipherSuites), {$IFDEF VER7P}nil{$ELSE}TComponent{$ENDIF}, 'CipherSuites', TScCollectionEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TScSSLClient, 'CertName', TKeyNameEditor);
  RegisterPropertyEditor(TypeInfo(string), TScSSLClient, 'CACertName', TKeyNameEditor);
  RegisterPropertyEditor(TypeInfo(string), TScSSLServerConnection, 'CACertName', TKeyNameEditor);
{$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(string), TScCryptoAPIStorage, 'ProviderName', TProviderNameEditor);
  RegisterPropertyEditor(TypeInfo(string), TScCryptoAPIStorage, 'CertStoreName', TCertStoreNameEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TScCMSProcessor, 'CertificateName', TKeyNameEditor);
  RegisterComponentEditor(TScCMSProcessor, TScCMSProcessorEditor);

  RegisterComponentEditor(TScMemoryStorage, TScStorageEditor);
  RegisterComponentEditor(TScFileStorage, TScStorageEditor);
{$IFDEF MSWINDOWS}
  RegisterComponentEditor(TScRegStorage, TScStorageEditor);
{$ENDIF}
{$IFNDEF LINUX}
{$IFNDEF LINUX_BSD}
{$IFNDEF ANDROID}
  RegisterComponentEditor(TScCryptoAPIStorage, TScStorageEditor);
{$ENDIF}
{$ENDIF}
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TScWebHeaderCollection), TScHttpWebRequest, 'Headers', TScHeadersEditor);
  RegisterPropertyEditor(TypeInfo(TScWebHeaderCollection), TScWebSocketClientOptions, 'RequestHeaders', TScHeadersEditor);
  RegisterPropertyEditor(TypeInfo(TScWebHeaderCollection), TScHttpConnectionOptions, 'Headers', TScHeadersEditor);
end;

{$IFNDEF FPC}
var
  SSHNotificator: TScSSHDesignNotification;
  StorageNotificator: TScStorageDesignNotification;
{$ENDIF}

{ TScHeadersEditor }

function TScHeadersEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TScHeadersEditor.GetValue: string;
begin
  Result := '(TScWebHeaderCollection)';
end;

procedure TScHeadersEditor.Edit;
var
  Obj: TPersistent;
  Component: TComponent;
  EditForm: TScStringMapEditorForm;
begin
  Obj := GetComponent(0);
  if Obj is TComponent then
    Component := Obj as TComponent
  else
  if Obj is TScWebSocketClientOptions then
    Component := TScWebSocketClientUtils.GetOptionsOwner(TScWebSocketClientOptions(Obj))
  else
  if Obj is TScHttpConnectionOptions then
    Component := TScHttpConnectionOptionsUtils.GetOptionsOwner(TScHttpConnectionOptions(Obj))
  else
    Component := nil;

  if not Assigned(Component) then
    Exit;

  EditForm := TScStringMapEditorForm.Create(nil, TScDesignUtils);
  try
    EditForm.Headers := TScWebHeaderCollection(GetOrdValue);
    EditForm.Component := Component;

    EditForm.ShowModal;
    if EditForm.ModalResult = mrOk then
      {$IFDEF FPC}FindRootDesigner(Component){$ELSE}Designer{$ENDIF}.Modified;
  finally
    EditForm.Free;
  end;
end;

initialization
  ShowDlgYesOrNo := ShowDialogYesOrNo;
{$IFNDEF FPC}
  SSHNotificator := TScSSHDesignNotification.Create;
  RegisterDesignNotification(SSHNotificator);

  StorageNotificator := TScStorageDesignNotification.Create;
  RegisterDesignNotification(StorageNotificator);
{$ENDIF}

{$IFNDEF FPC}
finalization
  UnRegisterDesignNotification(SSHNotificator);
  UnRegisterDesignNotification(StorageNotificator);
{$ENDIF}

end.

