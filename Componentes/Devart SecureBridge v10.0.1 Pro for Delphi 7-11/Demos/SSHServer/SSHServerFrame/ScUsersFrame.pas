unit ScUsersFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Classes, SysUtils, Buttons,
  ScColFrame, ScUtils, ScAlgorithmSupport, ScSSHUtils, ScBridge;

type
  TScUserItem = class(TCollectionItem)
  protected
    FUserName: string;
    FNewUserName: string;
    FDomain: string;
    FPassword: string;
    FAuthentications: TScUserAuthentications;
    FFingerPrint: string;
    FKey: TScKey;
    FKeyChanged: Boolean;
    procedure SetFromUser(User: TScUser);
    function IndexOf(const AName: string): Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TScUsersFrame = class(TScColFrame)
    edUserName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbPublicKey: TCheckBox;
    Label6: TLabel;
    cbPassword: TCheckBox;
    cbOSAuthentication: TCheckBox;
    Label7: TLabel;
    edPassword: TEdit;
    PanelKey: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    edFingerPrint: TEdit;
    cbAlgorithm: TComboBox;
    cbBitCount: TComboBox;
    btImport: TButton;
    btExportPublic: TButton;
    Bevel1: TBevel;
    Label9: TLabel;
    edDomain: TEdit;
    procedure btImportClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btExportPublicClick(Sender: TObject);
    procedure cbPublicKeyClick(Sender: TObject);
    procedure cbPasswordClick(Sender: TObject);
    procedure edUserNameExit(Sender: TObject);
    procedure lbItemNameClick(Sender: TObject);
    procedure cbOSAuthenticationClick(Sender: TObject);

  protected
    FItems: TCollection;
    FStorage: TScStorage;

    function GetCurItem: TScUserItem;
    function GetItems: TCollection; override;
    function GetItemName(Item: TCollectionItem): string; override;
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure ControlsToItem(Item: TCollectionItem); override;
    procedure UpdateControlsState; override;
    procedure SetUserName;

    procedure DoActivate; override;
    procedure DoFinish; override;
    function DoSave: Boolean; override;
    property CurItem: TScUserItem read GetCurItem;
  public
    property Storage: TScStorage read FStorage write FStorage;
  end;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ScConsts, ScKeysFrame;

const
  WrongKeyFormat = 'Wrong key format or wrong password!';
  UnknownPassword = '*****';

{ TScUserItem }

constructor TScUserItem.Create(Collection: TCollection);
begin
  inherited;
  FKey := TScKey.Create(nil);
end;

destructor TScUserItem.Destroy;
begin
  FKey.Free;
  inherited;
end;

function TScUserItem.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Collection.Count - 1 do
    if AnsiCompareText(TScUserItem(Collection.Items[Result]).FUserName, AName) = 0 then
      Exit;
  Result := -1;
end;

procedure TScUserItem.SetFromUser(User: TScUser);
begin
  FUserName := User.UserName;
  FNewUserName := User.UserName;
  FDomain := User.Domain;
  FPassword := User.Password;
  if (FPassword = '') and (User.HashPassword <> '') then
    FPassword := UnknownPassword;
  FAuthentications := User.Authentications;

  if User.Key <> nil then begin
    FKey.Assign(User.Key);

    if User.Key.Ready then
      User.Key.GetFingerPrint(haMD5, FFingerPrint)
    else
      FFingerPrint := SKeyNotReady;
  end;
end;

{ TScUsersFrame }

function TScUsersFrame.GetItems: TCollection;
begin
  Result := FItems;
end;

function TScUsersFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := TScUserItem(Item).FUserName;
end;

function TScUsersFrame.GetCurItem: TScUserItem;
begin
  if Items = nil then begin
    Result := nil;
    Exit;
  end;

  if FOldItemIndex = -1 then
    Result := Items.Items[lbItemName.ItemIndex] as TScUserItem
  else
    Result := Items.Items[FOldItemIndex] as TScUserItem;
end;

procedure TScUsersFrame.ItemToControls(Item: TCollectionItem);
var
  UserItem: TScUserItem;
begin
  UserItem := Item as TScUserItem;

  edUserName.Text := UserItem.FNewUserName;
  edDomain.Text := UserItem.FDomain;
  edPassword.Text := UserItem.FPassword;
  cbPublicKey.Checked := uaPublicKey in UserItem.FAuthentications;
  cbPassword.Checked := uaPassword in UserItem.FAuthentications;
  cbOSAuthentication.Checked := uaOSAuthentication in UserItem.FAuthentications;
  edPassword.Enabled := cbPassword.Checked;
  edDomain.ReadOnly := not cbOSAuthentication.Checked;

  edFingerPrint.Text := UserItem.FFingerPrint;
  cbAlgorithm.ItemIndex := cbAlgorithm.Items.IndexOf(CipherFactory.PublicKeyAlgorithmToName(UserItem.FKey.Algorithm));
  cbBitCount.Text := IntToStr(UserItem.FKey.BitCount);
end;

procedure TScUsersFrame.ControlsToItem(Item: TCollectionItem);
var
  User: TScUser;
  UserItem: TScUserItem;
begin
  if FInStoreItem then
    SetUserName;

  UserItem := Item as TScUserItem;
  UserItem.FDomain := edDomain.Text;
  UserItem.FPassword := edPassword.Text;

  UserItem.FAuthentications := [];
  if cbPublicKey.Checked then
    UserItem.FAuthentications := UserItem.FAuthentications + [uaPublicKey];
  if cbPassword.Checked then
    UserItem.FAuthentications := UserItem.FAuthentications + [uaPassword];
  if cbOSAuthentication.Checked then
    UserItem.FAuthentications := UserItem.FAuthentications + [uaOSAuthentication];

  if cbPublicKey.Checked and UserItem.FKey.Ready then
    UserItem.FKey.GetFingerPrint(haMD5, UserItem.FFingerPrint)
  else
    UserItem.FFingerPrint := SKeyNotReady;

  if FInStoreItem and not FStorage.ReadOnly then begin
    User := FStorage.Users.UserByName(UserItem.FUserName);
    User.Domain := UserItem.FDomain;
    if UserItem.FPassword <> UnknownPassword then
      User.Password := UserItem.FPassword;
    User.Authentications := UserItem.FAuthentications;

    if not cbPublicKey.Checked then
      UserItem.FKey.Ready := False;

    if (User.Key <> nil) and UserItem.FKeyChanged then
      User.Key.Assign(UserItem.FKey);
    UserItem.FKeyChanged := False;
  end;
end;

procedure TScUsersFrame.UpdateControlsState;
var
  ChangeAble: boolean;
begin
  ChangeAble := not FStorage.ReadOnly;
  btNew.Enabled := ChangeAble;
  edUserName.ReadOnly := not ChangeAble;
  edDomain.ReadOnly := edDomain.ReadOnly or not ChangeAble;
  edPassword.ReadOnly := not ChangeAble;
  edFingerPrint.ReadOnly := True;

  if lbItemName.ItemIndex <> - 1 then begin
    cbPublicKey.Enabled := ChangeAble;
    cbPassword.Enabled := ChangeAble;
    cbOSAuthentication.Enabled := ChangeAble;
    btDelete.Enabled := ChangeAble;
    PanelKey.Enabled := cbPublicKey.Checked;
  end
  else begin
    cbPublicKey.Enabled := False;
    cbPassword.Enabled := False;
    cbOSAuthentication.Enabled := False;
    btDelete.Enabled := False;
    PanelKey.Enabled := False;
  end;

  inherited;

  if lbItemName.ItemIndex <> - 1 then
    btExportPublic.Enabled := cbPublicKey.Checked and CurItem.FKey.Ready;
  btImport.Enabled := ChangeAble and cbPublicKey.Checked;
  cbAlgorithm.Enabled := False;
  cbBitCount.Enabled := False;
end;

procedure TScUsersFrame.DoActivate;
var
  User: TScUser;
  i: Integer;
begin
  FItems := TCollection.Create(TScUserItem);

  FStorage.Users.Refresh;
  for i := 0 to FStorage.Users.Count - 1 do begin
    try
      User := FStorage.Users.Users[i];
      (FItems.Add as TScUserItem).SetFromUser(User);
    except
      on E: Exception do
        ApplicationHandleException(E);
    end;
  end;

  inherited;
end;

procedure TScUsersFrame.DoFinish;
begin
  inherited;

  FreeAndNil(FItems);
end;

function TScUsersFrame.DoSave: Boolean;
begin
  Result := inherited DoSave;

  StoreItem;
end;

procedure TScUsersFrame.btImportClick(Sender: TObject);
var
  fp: string;
begin
  try
    CurItem.FKeyChanged := True;
    try
      if not TScKeyEdit.ImportKey(CurItem.FKey) then
        CurItem.FKeyChanged := False;
    finally
      if CurItem.FKeyChanged then begin
        if CurItem.FKey.Ready then begin
          CurItem.FKey.GetFingerPrint(haMD5, fp);
          CurItem.FFingerPrint := fp;
        end
        else
          CurItem.FFingerPrint := WrongKeyFormat;

        if CurItem.FKey.IsPrivate then
          MessageDlg('You have loaded a private key.'#13#10'Probably this key is counterfeited, as user key must be public!', mtWarning, [mbOk], 0);
      end;
    end;

  finally
    SelectItem;
    lbItemName.Repaint;
  end;
end;

procedure TScUsersFrame.btExportPublicClick(Sender: TObject);
begin
  Assert(CurItem.FKey.Ready);
  TScKeyEdit.ExportPublicKey(CurItem.FKey);
end;

procedure TScUsersFrame.btDeleteClick(Sender: TObject);
var
  User: TScUser;
begin
  if MessageDlg('Do you want delete "' + CurItem.FUserName + '" user?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  User := FStorage.Users.UserByName(CurItem.FUserName);
  try
    FStorage.Users.Remove(User);
    CurItem.Free;
    User.Free;
  finally
    InitItems;
  end;
end;

procedure TScUsersFrame.btNewClick(Sender: TObject);
var
  User: TScUser;
  NewUsername: string;
  UserItem: TScUserItem;
  i: Integer;
begin
  StoreItem;

  i := 1;
  NewUsername := 'user' + IntToStr(i);
  if lbItemName.ItemIndex <> - 1 then
    while CurItem.IndexOf(NewUsername) > -1 do begin
      Inc(i);
      NewUsername := 'user' + IntToStr(i);
    end;

  User := TScUser.Create(FStorage.Users);
  User.UserName := NewUsername;
  UserItem := FItems.Add as TScUserItem;
  UserItem.SetFromUser(User);

  lbItemName.Items.Add(GetItemName(UserItem));
  lbItemName.ItemIndex := lbItemName.Items.Count - 1;

  SelectItem;
end;

procedure TScUsersFrame.cbPublicKeyClick(Sender: TObject);
begin
  if not FInSelectItem and (lbItemName.ItemIndex <> - 1) then begin
    try
      ControlsToItem(CurItem);
    finally
      SelectItem;
    end;
  end;
end;

procedure TScUsersFrame.cbPasswordClick(Sender: TObject);
begin
  edPassword.Enabled := cbPassword.Checked;
  UpdateControlsState;
end;

procedure TScUsersFrame.cbOSAuthenticationClick(Sender: TObject);
begin
  edDomain.ReadOnly := not cbOSAuthentication.Checked;
  UpdateControlsState;
end;

procedure TScUsersFrame.SetUserName;
var
  User: TScUser;
begin
  if edUserName.Text <> CurItem.FUserName then begin
    User := FStorage.Users.UserByName(CurItem.FUserName);
    try
      try
        User.UserName := edUserName.Text;
      except
        edUserName.SetFocus;
        raise;
      end;

      CurItem.FUserName := edUserName.Text;
    finally
      lbItemName.Items[CurItem.Index] := GetItemName(CurItem);
    end;
  end;
end;

procedure TScUsersFrame.edUserNameExit(Sender: TObject);
begin
  if CurItem <> nil then
    CurItem.FNewUserName := edUserName.Text;
end;

procedure TScUsersFrame.lbItemNameClick(Sender: TObject);
begin
  if lbItemName.ItemIndex = FOldItemIndex then
    SetUserName;

  inherited;
end;

end.

