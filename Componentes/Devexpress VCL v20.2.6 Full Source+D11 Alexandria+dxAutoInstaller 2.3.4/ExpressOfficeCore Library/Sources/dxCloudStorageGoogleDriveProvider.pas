{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressOfficeCore Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxCloudStorageGoogleDriveProvider;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Forms, Windows,
  Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxWinInet, cxClasses, dxThreading,
  dxCloudStorage, dxAuthorizationAgents;

type
  TdxCloudStorageGoogleDriveFileTransferManager = class;
  TdxCloudStorageGoogleDriveProvider = class;

  { TdxCloudStorageGoogleDriveProviderMoveItemToCustomTask }

  TdxCloudStorageGoogleDriveProviderMoveItemToCustomTask = class abstract
    (TdxCloudStorageProviderMoveItemToCustomTask)
  protected
    function GetResultObject: TdxJSONObject; virtual; abstract;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override; final;
  end;

  { TdxCloudStorageGoogleDriveProviderCopyItemToTask }

  TdxCloudStorageGoogleDriveProviderCopyItemToTask = class
    (TdxCloudStorageGoogleDriveProviderMoveItemToCustomTask)
  protected
    procedure DoComplete; override;
    function GetResultObject: TdxJSONObject; override;
  end;

  { TdxCloudStorageGoogleDriveProviderMoveItemToTask }

  TdxCloudStorageGoogleDriveProviderMoveItemToTask = class
    (TdxCloudStorageGoogleDriveProviderMoveItemToCustomTask)
  protected
    procedure DoComplete; override;
    function GetResultObject: TdxJSONObject; override;
  end;

  { TdxCloudStorageGoogleDriveProviderDeleteItemTask }

  TdxCloudStorageGoogleDriveProviderDeleteItemTask = class
    (TdxCloudStorageProviderDeleteItemTask)
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageGoogleDriveProviderDeletePermissionTask }

  TdxCloudStorageGoogleDriveProviderDeletePermissionTask = class
    (TdxCloudStorageProviderDeletePermissionTask)
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask }

  TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask = class
    (TdxCloudStorageProviderMoveItemToTrashTask)
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
    function GetObjectName: string; virtual;
  end;

  { TdxCloudStorageGoogleDriveProviderRestoreItemFromTrashTask }

  TdxCloudStorageGoogleDriveProviderRestoreItemFromTrashTask = class
    (TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask)
  protected
    procedure DoComplete; override;
    function GetObjectName: string; override;
  end;

  { TdxCloudStorageGoogleDriveProviderUpdateFolderTask }

  TdxCloudStorageGoogleDriveProviderUpdateFolderTask = class
    (TdxCloudStorageProviderUpdateFolderCustomTask)
  strict private
    function GetProvider: TdxCloudStorageGoogleDriveProvider; inline;
    function GetID: string;
  protected
    function AssignData(const ACancelStatus: TdxTaskCancelCallback;
      var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus; override;
    function PopulateItems(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;

    function GetQ: string; virtual;
    function GetUri(const APageToken: string): string; virtual;
    function ItemCountLimit: Integer; virtual;

    property Provider: TdxCloudStorageGoogleDriveProvider read GetProvider;
  end;

  { TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask }

  TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask = class
    (TdxCloudStorageGoogleDriveProviderUpdateFolderTask)
  strict private
    function GetFolder: TdxCloudStorageSpecialFolder; inline;
  protected
    function AssignData(const ACancelStatus: TdxTaskCancelCallback;
      var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus; override;
    function GetQ: string; override;
    function GetUri(const APageToken: string): string; override;
    function ItemCountLimit: Integer; override;
  public
    property Folder: TdxCloudStorageSpecialFolder read GetFolder;
  end;

  { TdxCloudStorageGoogleDriveProviderFetchAllTask }

  TdxCloudStorageGoogleDriveProviderFetchAllTask = class
    (TdxCloudStorageProviderFetchAllCustomTask)
  strict private
    function GetProvider: TdxCloudStorageGoogleDriveProvider; inline;
  protected
    function AssignRoot(var ARoot: TdxCloudStorageItemData;
      const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
    function PopulateItems(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
    property Provider: TdxCloudStorageGoogleDriveProvider read GetProvider;
  end;

  { TdxCloudStorageGoogleDriveProviderFetchPermissionsTask }

  TdxCloudStorageGoogleDriveProviderFetchPermissionsTask = class
    (TdxCloudStorageProviderFetchPermissionsCustomTask)
  strict private
    FSharedLink: string;
    function GetSharedLink(var AResult: string): Boolean;
    function GetUri(const APageToken: string): string;
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageGoogleDriveProviderCreateFolderTask }

  TdxCloudStorageGoogleDriveProviderCreateFolderTask = class
    (TdxCloudStorageProviderCreateFolderTask)
  protected
    function GetRequestObject: TdxJSONObject; override;
    function PostRequest(AObject: TdxJSONObject): TdxJSONObject; override;
    function IsError(AObject: TdxJSONObject): Boolean; override;
  end;

  { TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask }

  TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask = class
    (TdxCloudStorageProviderCreateSharedLinkCustomTask)
  strict private
    function GetRequestObject: TdxJSONObject;
    function GetResultObject(const ACancelStatus: TdxTaskCancelCallback;
      out AResult: TdxJSONObject): TdxTaskCompletedStatus;
    function GetSharedLink(var AResult: string): Boolean;
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageGoogleDriveProviderInviteUsersTask }

  TdxCloudStorageGoogleDriveProviderInviteUsersTask = class
    (TdxCloudStorageProviderInviteUsersCustomTask)
  strict private
    FSharedLink: string;
    function GetResultObject(const AUser: string;
      const ACancelStatus: TdxTaskCancelCallback; out AResult: TdxJSONObject)
      : TdxTaskCompletedStatus;
    function GetRequestObject(const AUser: string): TdxJSONObject;
    function GetSharedLink(var AResult: string): Boolean;
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback)
      : TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageGoogleDriveFiles }

  TdxCloudStorageGoogleDriveFiles = class(TdxCloudStorageFiles)
  protected
    procedure PopulateSpecialFolders; override;
  end;

  { TdxCloudStorageGoogleDriveUploadTask }

  TdxCloudStorageGoogleDriveUploadTask = class abstract
    (TdxCloudStorageUploadTask)
  strict private
    function GetProvider: TdxCloudStorageGoogleDriveProvider; inline;
  protected
    function GetServerName: string; override;
    function GetVerb: string; override;

    function IsSuccess(ARequest: TdxInternet): Boolean; override;

    property Provider: TdxCloudStorageGoogleDriveProvider read GetProvider;
  end;

  { TdxCloudStorageGoogleDriveUpdateFileStreamTask }

  TdxCloudStorageGoogleDriveUpdateFileStreamTask = class
    (TdxCloudStorageGoogleDriveUploadTask)
  strict private
    FContentMIMEType: string;
    FFileID: string;
    FMIMEType: string;
  protected
    function GetHeader: string; override;
    function GetObjectName: string; override;

    property ContentMIMEType: string read FContentMIMEType;
  public
    constructor Create(AProvider: TdxCloudStorageGoogleDriveProvider;
      AFile: TdxCloudStorageFile; AStream: TStream); reintroduce;
  end;

  { TdxCloudStorageGoogleDriveUploadFileTask }

  TdxCloudStorageGoogleDriveUploadFileTask = class
    (TdxCloudStorageGoogleDriveUploadTask)
  strict private
    class var FBoundary: string;
  private
    class procedure Initialize; static;
  strict private
    FFileName: string;
    FParentID: string;
    procedure UpdateStream;
  protected
    procedure DoComplete; override;
    function GetHeader: string; override;
    function GetObjectName: string; override;
    function GetVerb: string; override;
    procedure UpdateRequestParams; override;
  public
    constructor Create(AProvider: TdxCloudStorageGoogleDriveProvider;
      AParent: TdxCloudStorageFolder; const AFileName: string); reintroduce;
  end;

  { TdxCloudStorageGoogleDriveFileTransferManager }

  TdxCloudStorageGoogleDriveFileTransferManager = class
    (TdxCloudStorageFileTransferManager)
  strict private
    function GetProvider: TdxCloudStorageGoogleDriveProvider; inline;
  protected
    function CreateUpdateFileStreamTask(AFile: TdxCloudStorageFile;
      AStream: TStream): TdxCloudStorageUploadTask; override;
    function CreateUploadFileTask(AParent: TdxCloudStorageFolder;
      const AFileName: string): TdxCloudStorageUploadTask; override;

    property Provider: TdxCloudStorageGoogleDriveProvider read GetProvider;
  end;

  { TdxCloudStorageGoogleDriveProvider }

  TdxCloudStorageGoogleDriveProvider = class(TdxCloudStorageOAuth2Provider)
  protected const
    FFolderMIMEType = 'application/vnd.google-apps.folder';
    FServerName = 'www.googleapis.com';
  strict private
  class var
    FDefaultMIMETypeExportTypeDictionary: TDictionary<string, string>;
    FDefaultMIMETypeToExtensionDictionary: TDictionary<string, string>;
    FDefaultExtensionToMIMETypeDictionary: TDictionary<string, string>;
  protected
    class procedure Initialize; static;
    class procedure Finalize; static;
    class function GetSharedLink(const AUserAgent, AHeader: string;
      AItem: TdxCloudStorageItem; var AResult: string): Boolean; static;
  strict private
    function GetAuthorizationAgent
      : TdxGoogleAPIOAuth2AuthorizationAgent; inline;
    function GetFiles: TdxCloudStorageGoogleDriveFiles; inline;
    function GetObjectParents(AParents: TdxJSONArray): TArray<string>;
    procedure SetAuthorizationAgent(const Value
      : TdxGoogleAPIOAuth2AuthorizationAgent); inline;
  protected
    function CreateFiles: TdxCloudStorageFiles; override;
    function CreateFileTransferManager
      : TdxCloudStorageFileTransferManager; override;

    function CreateCopyItemToTask(AItem: TdxCloudStorageItem;
      AParent: TdxCloudStorageFolder)
      : TdxCloudStorageProviderMoveItemToCustomTask; override;
    function CreateCreateFolderTask(AParent: TdxCloudStorageFolder;
      const AName: string): TdxCloudStorageProviderCreateFolderTask; override;
    function CreateCreateSharedLinkTask(AItem: TdxCloudStorageItem;
      AReadOnly: Boolean; AScope: TdxCloudStorageItemPermissionScope)
      : TdxCloudStorageProviderCreateSharedLinkCustomTask; override;
    function CreateDeleteItemTask(AItem: TdxCloudStorageItem)
      : TdxCloudStorageProviderDeleteItemTask; override;
    function CreateDeletePermissionTask(APermission
      : TdxCloudStorageItemPermission)
      : TdxCloudStorageProviderDeletePermissionTask; override;
    function CreateFetchAllTask
      : TdxCloudStorageProviderFetchAllCustomTask; override;
    function CreateFetchPermissionsTask(AItem: TdxCloudStorageItem)
      : TdxCloudStorageProviderFetchPermissionsCustomTask; override;
    function CreateMoveItemToTask(AItem: TdxCloudStorageItem;
      AParent: TdxCloudStorageFolder)
      : TdxCloudStorageProviderMoveItemToCustomTask; override;
    function CreateMoveItemToTrashTask(AItem: TdxCloudStorageItem)
      : TdxCloudStorageProviderMoveItemToTrashTask; override;
    function CreateRestoreItemFromTrashTask(AItem: TdxCloudStorageItem)
      : TdxCloudStorageProviderItemCustomTask; override;
    function CreateUpdateFolderTask(AFolder: TdxCloudStorageFolder)
      : TdxCloudStorageProviderUpdateFolderCustomTask; override;
    function CreateUpdateSpecialFolderTask
      (AFolder: TdxCloudStorageSpecialFolder)
      : TdxCloudStorageProviderUpdateFolderCustomTask; override;

    function CreateItem(const AMIMEType: string): TdxCloudStorageItem; override;
    function DoGetSpecialFolderData(const AType
      : TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData; override;
    function ObjectToData(const AObject: TObject)
      : TdxCloudStorageItemData; override;

    class function CreateParentsObject(const AParents: TArray<string>)
      : TdxJSONArray; overload; static;
    class function CreateParentsObject(const AParent: string): TdxJSONArray;
      overload; static;

    function GetScopes: TStringList; override;
    function CreateInviteUsersTask(AItem: TdxCloudStorageItem;
      const AUsers: System.TArray<System.string>; AReadOnly: Boolean;
      ARequireSignIn: Boolean; ASendInvitation: Boolean; const AMessage: string)
      : TdxCloudStorageProviderInviteUsersCustomTask; override;

    property Files: TdxCloudStorageGoogleDriveFiles read GetFiles;
  public
    class function GetDisplayName: string; override;
    function GetExtension(AItem: TdxCloudStorageItem): string; override;

    class property DefaultMIMETypeExportTypeDictionary
      : TDictionary<string, string> read FDefaultMIMETypeExportTypeDictionary;
    class property DefaultMIMETypeToExtensionDictionary
      : TDictionary<string, string> read FDefaultMIMETypeToExtensionDictionary;
    class property DefaultExtensionToMIMETypeDictionary
      : TDictionary<string, string> read FDefaultExtensionToMIMETypeDictionary;
  published
    property AuthorizationAgent: TdxGoogleAPIOAuth2AuthorizationAgent
      read GetAuthorizationAgent write SetAuthorizationAgent;
    property RecentFileCountLimit;
    property RecentDayCountLimit;
  end;

implementation

uses
  Math,
  cxDateUtils, dxUriRecord, dxStringHelper,
  dxCloudStorageStrs;

type
  TdxHttpHelperAccess = class(TdxHttpHelper);
  TdxCloudStorageFolderAccess = class(TdxCloudStorageFolder);

function GetID(AItem: TdxCloudStorageItem): string;
begin
  if (AItem is TdxCloudStorageFolder) and TdxCloudStorageFolder(AItem).IsRoot
  then
    Result := 'root'
  else
    Result := AItem.ID;
end;

function ValueToPermissionData(const AValue: TdxJSONObject;
  var AData: TdxCloudStorageItemPermissionData): Boolean;
begin
  Result := AValue.HasParam('id') and
    (AValue.GetParamValue('kind') = 'drive#permission');
  if not Result then
    Exit;
  AData.ID := AValue.GetParamValue('id');
  AData.ReadOnly := AValue.GetParamValue('role') = 'reader';
  AData.InheritedFrom := AValue.GetChild('permissionDetails',
    'inherited').IsTrue;
  AData.InheritedFromDriveID := '';
  AData.InheritedFromID := AValue.GetChildParamValue('permissionDetails',
    'inheritedFrom');
  AData.InheritedFromName := '';
  if AValue.HasParam('emailAddress') then
    AData.User := AValue.GetParamValue('emailAddress');
end;

{ TdxCloudStorageGoogleDriveProviderMoveItemToCustomTask }

function TdxCloudStorageGoogleDriveProviderMoveItemToCustomTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  AResult := GetResultObject;
  try
    if AResult = nil then
      Exit(TdxTaskCompletedStatus.Fail);
    if AResult.GetParamValue('kind') = 'drive#file' then
    begin
      AssignData(AResult);
      Result := TdxTaskCompletedStatus.Success;
    end
    else
      DoError(AResult);
  finally
    AResult.Free;
  end;
end;

{ TdxCloudStorageGoogleDriveProviderCopyItemToTask }

procedure TdxCloudStorageGoogleDriveProviderCopyItemToTask.DoComplete;
begin
  TdxCloudStorageGoogleDriveProvider(Provider).CopyItemToComplete(Item,
    Parent, Data);
end;

function TdxCloudStorageGoogleDriveProviderCopyItemToTask.GetResultObject
  : TdxJSONObject;
var
  ARequest: TdxJSONObject;
begin
  UpdateRequestParams;
  ARequest := TdxJSONObject.Create;
  try
    ARequest.AddPair('parents',
      TdxCloudStorageGoogleDriveProvider.CreateParentsObject(Parent.ID));
    Result := TdxHttpHelper.PostRequest(UserAgent,
      TdxCloudStorageGoogleDriveProvider.FServerName,
      Format('drive/v2/files/%s/copy', [Item.ID]), Header, ARequest);
  finally
    ARequest.Free;
  end;
end;

{ TdxCloudStorageGoogleDriveProviderMoveItemToTask }

procedure TdxCloudStorageGoogleDriveProviderMoveItemToTask.DoComplete;
begin
  TdxCloudStorageGoogleDriveProvider(Provider).MoveItemToComplete(Item, Parent);
end;

function TdxCloudStorageGoogleDriveProviderMoveItemToTask.GetResultObject
  : TdxJSONObject;
var
  ARequest: TdxJSONObject;
begin
  UpdateRequestParams;
  ARequest := TdxJSONObject.Create;
  try
    ARequest.AddPair('parents',
      TdxCloudStorageGoogleDriveProvider.CreateParentsObject(Parent.ID));
    Result := TdxHttpHelper.PutRequest(UserAgent,
      TdxCloudStorageGoogleDriveProvider.FServerName,
      Format('drive/v2/files/%s', [Item.ID]), Header, ARequest);
  finally
    ARequest.Free;
  end;
end;

{ TdxCloudStorageGoogleDriveProviderDeleteItemTask }

function TdxCloudStorageGoogleDriveProviderDeleteItemTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  UpdateRequestParams;
  AResult := TdxHttpHelper.DeleteRequest(UserAgent,
    TdxCloudStorageGoogleDriveProvider.FServerName, Format('drive/v2/files/%s',
    [Item.ID]), Header, nil);
  try
    if AResult <> nil then
      DoError(AResult)
    else
      Result := TdxTaskCompletedStatus.Success;
  finally
    AResult.Free;
  end;
end;

{ TdxCloudStorageGoogleDriveProviderDeletePermissionTask }

function TdxCloudStorageGoogleDriveProviderDeletePermissionTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResponse: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  UpdateRequestParams;
  AResponse := TdxHttpHelper.DeleteRequest(UserAgent,
    TdxCloudStorageGoogleDriveProvider.FServerName,
    Format('drive/v2/files/%s/permissions/%s', [GetID(Item), Permission.ID]),
    Header, nil);
  try
    if AResponse <> nil then
      DoError(AResponse)
    else
      Result := TdxTaskCompletedStatus.Success;
  finally
    AResponse.Free;
  end;
end;

{ TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask }

function TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  UpdateRequestParams;
  AResult := TdxHttpHelper.PostRequest(UserAgent,
    TdxCloudStorageGoogleDriveProvider.FServerName, GetObjectName, Header, nil);
  try
    if AResult <> nil then
    begin
      if AResult.GetParamValue('kind') = 'drive#file' then
      begin
        AssignData(AResult);
        Result := TdxTaskCompletedStatus.Success;
      end
      else
        DoError(AResult)
    end;
  finally
    AResult.Free;
  end;
end;

function TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask.
  GetObjectName: string;
begin
  Result := Format('drive/v2/files/%s/trash', [Item.ID]);
end;

{ TdxCloudStorageGoogleDriveProviderRestoreItemFromTrashTask }

procedure TdxCloudStorageGoogleDriveProviderRestoreItemFromTrashTask.DoComplete;
begin
  TdxCloudStorageGoogleDriveProvider(Provider).RestoreItemFromTrashComplete
    (Item, Data);
end;

function TdxCloudStorageGoogleDriveProviderRestoreItemFromTrashTask.
  GetObjectName: string;
begin
  Result := Format('drive/v2/files/%s/untrash', [Item.ID]);
end;

{ TdxCloudStorageGoogleDriveProviderUpdateFolderTask }

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.AssignData
  (const ACancelStatus: TdxTaskCancelCallback;
  var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
  AUri: string;
begin
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);
  AUri := Format('https://www.googleapis.com/drive/v2/files/%s', [GetID]);

  UpdateRequestParams;
  AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
  try
    if (AObject = nil) or not AObject.HasParam('id') then
    begin
      if AObject <> nil then
        DoError(AObject);
      Exit(TdxTaskCompletedStatus.Fail);
    end;
    AData := Provider.ObjectToData(AObject);
  finally
    AObject.Free;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.PopulateItems
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  APageToken: string;
  AUri: string;
  AObject: TdxJSONObject;
  AFile: TdxJSONObject;
  AFiles: TdxJSONArray;
  I: Integer;
  AData: TdxCloudStorageItemData;
begin
  APageToken := '';
  while True do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    if not IsValid then
      Exit(TdxTaskCompletedStatus.Fail);
    AUri := GetUri(APageToken);
    UpdateRequestParams;
    AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
    try
      if (AObject = nil) or not AObject.HasParam('items') or
        (AObject.GetParamValue('kind') <> 'drive#fileList') then
      begin
        if AObject <> nil then
          DoError(AObject);
        Exit(TdxTaskCompletedStatus.Fail);
      end;
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      if not IsValid then
        Exit(TdxTaskCompletedStatus.Fail);
      APageToken := AObject.GetParamValue('nextPageToken');
      AFiles := TdxJSONArray(AObject.GetValue('items'));
      for I := 0 to AFiles.Count - 1 do
      begin
        AFile := TdxJSONObject(AFiles.Items[I]);
        AData := Provider.ObjectToData(AFile);
        Children.Add(AData);
        if Children.Count >= ItemCountLimit then
          Exit(TdxTaskCompletedStatus.Success);
      end;
    finally
      AObject.Free;
    end;
    if APageToken = '' then
      Break;
  end;

  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.GetQ: string;
begin
  Result := Format('''%s'' in parents and trashed = %s',
    [GetID, BoolToStr(Folder.Trashed, True)]);
end;

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.GetUri
  (const APageToken: string): string;
var
  Q: string;
begin
  Q := TdxURI.EscapeDataString(GetQ);
  if APageToken = '' then
    Result := Format('https://www.googleapis.com/drive/v2/files?q=%s', [Q])
  else
    Result := Format
      ('https://www.googleapis.com/drive/v2/files?pageToken=%s&q=%s',
      [APageToken, Q]);
end;

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.
  ItemCountLimit: Integer;
begin
  Result := MaxInt;
end;

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.GetID: string;
begin
  if Folder.ID <> '' then
    Result := Folder.ID
  else if Folder.IsRoot then
    Result := 'root'
  else
  begin
    Result := '';
  end;
end;

function TdxCloudStorageGoogleDriveProviderUpdateFolderTask.GetProvider
  : TdxCloudStorageGoogleDriveProvider;
begin
  Result := TdxCloudStorageGoogleDriveProvider(inherited Provider);
end;

{ TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask }

function TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask.AssignData
  (const ACancelStatus: TdxTaskCancelCallback;
  var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus;
begin
  Result := TdxTaskCompletedStatus.Success;
  AData := Provider.GetSpecialFolderData(Folder.&Type);
end;

function TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask.GetQ: string;
var
  ADateLimit: TDate;
begin
  case Folder.&Type of
    TdxCloudStorageSpecialFolder.TType.Recent:
      begin
        Result := 'trashed = false and not mimeType contains ''folder''';
        ADateLimit := Trunc(Now) - Provider.RecentDayCountLimit;
        Result := Result +
          Format(' and (modifiedDate >= ''%0:s'' or lastViewedByMeDate >= ''%0:s'')',
          [TdxISO8601Helper.DateTimeToString(ADateLimit)]);
      end;
    TdxCloudStorageSpecialFolder.TType.SharedWithMe:
      Result := 'trashed = false and sharedWithMe = true';
    TdxCloudStorageSpecialFolder.TType.Starred:
      Result := 'trashed = false and starred = true';
    TdxCloudStorageSpecialFolder.TType.Trash:
      Result := 'trashed = true';
  else
    Result := '';
  end;
end;

function TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask.GetUri
  (const APageToken: string): string;
begin
  Result := inherited GetUri(APageToken);
  if Folder.&Type = TdxCloudStorageSpecialFolder.TType.Recent then
    Result := Result + Format('&orderBy=%s',
      [TdxURI.EscapeDataString('recency desc')]);
end;

function TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask.
  ItemCountLimit: Integer;
begin
  if Folder.&Type = TdxCloudStorageSpecialFolder.TType.Recent then
    Result := Provider.RecentFileCountLimit
  else
    Result := inherited ItemCountLimit;
end;

function TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask.GetFolder
  : TdxCloudStorageSpecialFolder;
begin
  Result := TdxCloudStorageSpecialFolder(inherited Folder);
end;

{ TdxCloudStorageGoogleDriveProviderFetchAllTask }

function TdxCloudStorageGoogleDriveProviderFetchAllTask.AssignRoot
  (var ARoot: TdxCloudStorageItemData;
  const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
  AUri: string;
begin
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);
  AUri := 'https://www.googleapis.com/drive/v2/files/root';
  UpdateRequestParams;
  AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
  try
    if (AObject = nil) or not AObject.HasParam('id') or
      not AObject.HasParam('parents') or (AObject.GetParamValue('parents') <> '')
    then
    begin
      if AObject <> nil then
        DoError(AObject);
      Exit(TdxTaskCompletedStatus.Fail);
    end;
    ARoot := Provider.ObjectToData(AObject);
  finally
    AObject.Free;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageGoogleDriveProviderFetchAllTask.GetProvider
  : TdxCloudStorageGoogleDriveProvider;
begin
  Result := TdxCloudStorageGoogleDriveProvider(inherited Provider);
end;

function TdxCloudStorageGoogleDriveProviderFetchAllTask.PopulateItems
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  APageToken: string;
  AUri: string;
  AObject: TdxJSONObject;
  AFile: TdxJSONObject;
  AFiles: TdxJSONArray;
  I: Integer;
  AData: TdxCloudStorageItemData;
begin
  APageToken := '';
  while True do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    if not IsValid then
      Exit(TdxTaskCompletedStatus.Fail);
    if APageToken = '' then
      AUri := Format('https://www.googleapis.com/drive/v2/files?q=%s',
        [TdxURI.EscapeDataString('trashed=false')])
    else
      AUri := Format
        ('https://www.googleapis.com/drive/v2/files?pageToken=%s&q=%s',
        [APageToken, TdxURI.EscapeDataString('trashed=false')]);
    UpdateRequestParams;
    AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
    try
      if (AObject = nil) or not AObject.HasParam('items') or
        (AObject.GetParamValue('kind') <> 'drive#fileList') then
      begin
        if AObject <> nil then
          DoError(AObject);
        Exit(TdxTaskCompletedStatus.Fail);
      end;
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      if not IsValid then
        Exit(TdxTaskCompletedStatus.Fail);
      APageToken := AObject.GetParamValue('nextPageToken');
      AFiles := TdxJSONArray(AObject.GetValue('items'));
      for I := 0 to AFiles.Count - 1 do
      begin
        AFile := TdxJSONObject(AFiles.Items[I]);
        AData := Provider.ObjectToData(AFile);
        Items.Add(AData);
      end;
    finally
      AObject.Free;
    end;
    if APageToken = '' then
      Break;
  end;

  Result := TdxTaskCompletedStatus.Success;
end;

{ TdxCloudStorageGoogleDriveProviderFetchPermissionsTask }

function TdxCloudStorageGoogleDriveProviderFetchPermissionsTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AData: TdxCloudStorageItemPermissionData;
  AResponse: TdxJSONObject;
  AValues: TdxJSONArray;
  I: Integer;
  AUri: string;
  APageToken: string;
begin
  Result := TdxTaskCompletedStatus.Fail;
  Items.Clear;
  UpdateRequestParams;
  AUri := GetUri('');
  repeat
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    AResponse := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
    try
      if not AResponse.GetValue('permissions').IsArray or
        (AResponse.GetParamValue('kind') <> 'drive#permissionList') then
      begin
        DoError(AResponse);
        Exit;
      end;
      AValues := TdxJSONArray(AResponse.GetValue('permissions'));
      for I := 0 to AValues.Count - 1 do
      begin
        if ACancelStatus then
          Exit(TdxTaskCompletedStatus.Cancelled);
        if AValues.Items[I].GetParamValue('role') = 'owner' then
          Continue;
        if not ValueToPermissionData(TdxJSONObject(AValues.Items[I]), AData)
        then
        begin
          DoError(AValues.Items[I]);
          Exit;
        end;
        if not GetSharedLink(AData.Link) then
        begin
          DoError(AValues.Items[I]);
          Exit;
        end;
        Items.Add(AData);
      end;
      APageToken := AResponse.GetParamValue('nextPageToken');
      if APageToken <> '' then
        AUri := GetUri(APageToken)
      else
        AUri := '';
    finally
      AResponse.Free;
    end;
  until AUri = '';
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageGoogleDriveProviderFetchPermissionsTask.GetSharedLink
  (var AResult: string): Boolean;
begin
  Result := FSharedLink <> '';
  if not Result then
  begin
    UpdateRequestParams;
    Result := TdxCloudStorageGoogleDriveProvider.GetSharedLink(UserAgent,
      Header, Item, FSharedLink);
    if Result then
      AResult := FSharedLink;
  end
  else
    AResult := FSharedLink;
end;

function TdxCloudStorageGoogleDriveProviderFetchPermissionsTask.GetUri
  (const APageToken: string): string;
begin
  Result := Format('https://www.googleapis.com/drive/v3/files/%s/permissions',
    [GetID(Item)]);
  if APageToken <> '' then
    Result := Format('%s?pageToken=%s', [Result, APageToken])
end;

{ TdxCloudStorageGoogleDriveProviderCreateFolderTask }

function TdxCloudStorageGoogleDriveProviderCreateFolderTask.GetRequestObject
  : TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('title', Name);
  Result.AddPair('mimeType',
    TdxCloudStorageGoogleDriveProvider.FFolderMIMEType);
  Result.AddPair('parents',
    TdxCloudStorageGoogleDriveProvider.CreateParentsObject(ParentID));
end;

function TdxCloudStorageGoogleDriveProviderCreateFolderTask.PostRequest
  (AObject: TdxJSONObject): TdxJSONObject;
begin
  Result := TdxHttpHelper.PostRequest(UserAgent,
    TdxCloudStorageGoogleDriveProvider.FServerName, 'drive/v2/files',
    Header, AObject);
end;

function TdxCloudStorageGoogleDriveProviderCreateFolderTask.IsError
  (AObject: TdxJSONObject): Boolean;
begin
  Result := AObject.GetParamValue('kind') <> 'drive#file'
end;

{ TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask }

function TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
begin
  Result := GetResultObject(ACancelStatus, AResult);
  try
    if Result = TdxTaskCompletedStatus.Success then
    begin
      Result := TdxTaskCompletedStatus.Fail;
      if AResult = nil then
        Exit;
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      if ValueToPermissionData(AResult, FData) then
      begin
        if not GetSharedLink(FData.Link) then
          Exit;
        Result := TdxTaskCompletedStatus.Success;
      end
      else
        DoError(AResult);
    end;
  finally
    AResult.Free;
  end;
end;

function TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask.GetRequestObject
  : TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  if Scope = TdxCloudStorageItemPermissionScope.Anonymous then
    Result.AddPair('type', 'anyone')
  else
    Result.AddPair('type', 'group');
  if ReadOnly then
    Result.AddPair('role', 'reader')
  else
    Result.AddPair('role', 'writer');
end;

function TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask.GetResultObject
  (const ACancelStatus: TdxTaskCancelCallback; out AResult: TdxJSONObject)
  : TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
begin
  AObject := GetRequestObject;
  try
    UpdateRequestParams;
    AResult := TdxHttpHelper.PostRequest(UserAgent,
      TdxCloudStorageGoogleDriveProvider.FServerName,
      Format('drive/v3/files/%s/permissions', [Item.ID]), Header, AObject);
    Result := TdxTaskCompletedStatus.Success;
  finally
    AObject.Free;
  end;
end;

function TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask.GetSharedLink
  (var AResult: string): Boolean;
begin
  UpdateRequestParams;
  Result := TdxCloudStorageGoogleDriveProvider.GetSharedLink(UserAgent, Header,
    Item, AResult);
end;

{ TdxCloudStorageGoogleDriveProviderInviteUsersTask }

function TdxCloudStorageGoogleDriveProviderInviteUsersTask.DoRun
  (const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
  I: Integer;
  AData: TdxCloudStorageItemPermissionData;
begin
  for I := 0 to Length(Users) - 1 do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    Result := GetResultObject(Users[I], ACancelStatus, AResult);
    try
      if Result = TdxTaskCompletedStatus.Success then
      begin
        if AResult = nil then
          Exit;
        if ACancelStatus then
          Exit(TdxTaskCompletedStatus.Cancelled);
        if ValueToPermissionData(AResult, AData) then
        begin
          Items.Add(AData);
          if not GetSharedLink(AData.Link) then
            Exit;
        end
        else
          DoError(AResult);
      end;
    finally
      AResult.Free;
    end;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageGoogleDriveProviderInviteUsersTask.GetResultObject
  (const AUser: string; const ACancelStatus: TdxTaskCancelCallback;
  out AResult: TdxJSONObject): TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
begin
  AObject := GetRequestObject(AUser);
  try
    UpdateRequestParams;
    AResult := TdxHttpHelper.PostRequest(UserAgent,
      TdxCloudStorageGoogleDriveProvider.FServerName,
      Format('drive/v3/files/%s/permissions?emailMessage=%s&sendNotificationEmail=%s',
      [Item.ID, TdxURI.EscapeDataString(Message), BoolToStr(SendInvitation,
      True)]), Header, AObject);
    Result := TdxTaskCompletedStatus.Success;
  finally
    AObject.Free;
  end;
end;

function TdxCloudStorageGoogleDriveProviderInviteUsersTask.GetRequestObject
  (const AUser: string): TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('type', 'user');
  if ReadOnly then
    Result.AddPair('role', 'reader')
  else
    Result.AddPair('role', 'writer');
  Result.AddPair('emailAddress', AUser);
end;

function TdxCloudStorageGoogleDriveProviderInviteUsersTask.GetSharedLink
  (var AResult: string): Boolean;
begin
  Result := FSharedLink <> '';
  if not Result then
  begin
    UpdateRequestParams;
    Result := TdxCloudStorageGoogleDriveProvider.GetSharedLink(UserAgent,
      Header, Item, FSharedLink);
    if Result then
      AResult := FSharedLink;
  end
  else
    AResult := FSharedLink;
end;

{ TdxCloudStorageGoogleDriveFiles }

procedure TdxCloudStorageGoogleDriveFiles.PopulateSpecialFolders;
begin
  AddSpecialFolder(TdxCloudStorageSpecialFolder.TType.SharedWithMe);
  AddSpecialFolder(TdxCloudStorageSpecialFolder.TType.Recent);
  AddSpecialFolder(TdxCloudStorageSpecialFolder.TType.Starred);
  AddSpecialFolder(TdxCloudStorageSpecialFolder.TType.Trash);
end;

{ TdxCloudStorageGoogleDriveUploadTask }

function TdxCloudStorageGoogleDriveUploadTask.GetServerName: string;
begin
  Result := TdxCloudStorageGoogleDriveProvider.FServerName;
end;

function TdxCloudStorageGoogleDriveUploadTask.GetVerb: string;
begin
  Result := 'PUT';
end;

function TdxCloudStorageGoogleDriveUploadTask.IsSuccess
  (ARequest: TdxInternet): Boolean;
var
  AObject: TdxJSONObject;
begin
  AObject := TdxHttpHelperAccess(TdxHttpHelper).GetJSONObject(ARequest);
  try
    Result := (AObject <> nil) and
      (AObject.GetParamValue('kind') = 'drive#file');
    if not Result then
      DoError(AObject)
    else
      AssignData(AObject);
  finally
    AObject.Free;
  end;
end;

function TdxCloudStorageGoogleDriveUploadTask.GetProvider
  : TdxCloudStorageGoogleDriveProvider;
begin
  Result := TdxCloudStorageGoogleDriveProvider(inherited Provider);
end;

{ TdxCloudStorageGoogleDriveUpdateFileStreamTask }

constructor TdxCloudStorageGoogleDriveUpdateFileStreamTask.Create
  (AProvider: TdxCloudStorageGoogleDriveProvider; AFile: TdxCloudStorageFile;
  AStream: TStream);
begin
  inherited Create(AProvider, AFile.ID,
    Format('https://www.googleapis.com/upload/drive/v2/files/%s', [AFile.ID]
    ), AStream);
  FFileID := AFile.ID;
  FMIMEType := AFile.MIMEType;
  if not TdxCloudStorageGoogleDriveProvider.DefaultMIMETypeExportTypeDictionary.
    TryGetValue(FMIMEType, FContentMIMEType) then
    FContentMIMEType := FMIMEType;
end;

function TdxCloudStorageGoogleDriveUpdateFileStreamTask.GetHeader: string;
begin
  Result := inherited GetHeader;
  Result := TdxHttpHelper.ConcatenateHeaders
    ([Result, Format('Content-Type: %s', [ContentMIMEType]),
    Format('Content-Length: %d', [Stream.Size])]);
end;

function TdxCloudStorageGoogleDriveUpdateFileStreamTask.GetObjectName: string;
begin
  Result := Format('/upload/drive/v2/files/%s?uploadType=media', [FFileID]);
end;

{ TdxCloudStorageGoogleDriveUploadFileTask }

class procedure TdxCloudStorageGoogleDriveUploadFileTask.Initialize;
begin
  FBoundary := dxGenerateGUID;
end;

constructor TdxCloudStorageGoogleDriveUploadFileTask.Create
  (AProvider: TdxCloudStorageGoogleDriveProvider;
  AParent: TdxCloudStorageFolder; const AFileName: string);
begin
  inherited Create(AProvider, AFileName,
    'https://www.googleapis.com/upload/drive/v2/files', nil);
  FFileName := AFileName;
  FParentID := AParent.ID;
end;

procedure TdxCloudStorageGoogleDriveUploadFileTask.DoComplete;
begin
  Provider.Files.AddChild(FParentID, Data);
  inherited DoComplete;
end;

function TdxCloudStorageGoogleDriveUploadFileTask.GetHeader: string;
begin
  Result := inherited GetHeader;
  Result := TdxHttpHelper.ConcatenateHeaders
    ([Result, Format('Content-Type: multipart/related; boundary=%s',
    [FBoundary]), Format('Content-Length: %d', [Stream.Size])]);
end;

function TdxCloudStorageGoogleDriveUploadFileTask.GetObjectName: string;
begin
  Result := '/upload/drive/v2/files?uploadType=multipart';
end;

function TdxCloudStorageGoogleDriveUploadFileTask.GetVerb: string;
begin
  Result := 'POST';
end;

procedure TdxCloudStorageGoogleDriveUploadFileTask.UpdateRequestParams;
begin
  UpdateStream;
  inherited UpdateRequestParams;
end;

procedure TdxCloudStorageGoogleDriveUploadFileTask.UpdateStream;
var
  ABytes: TBytes;
  AObject: TdxJSONObject;
  ATitle: string;
  AMIMEType: string;
  AFileStream: TBytesStream;
begin
  Stream.Clear;
  ABytes := TEncoding.ASCII.GetBytes
    (Format('--%s'#13#10'Content-Type: application/json'#13#10#13#10,
    [FBoundary]));
  Stream.Write(ABytes[0], Length(ABytes));
  AObject := TdxJSONObject.Create;
  try
    ATitle := ExtractFileName(FFileName);
    AObject.AddPair('title', ATitle);
    if not TdxCloudStorageProvider.RegistryExtensionToMIMEType.TryGetValue
      (LowerCase(ExtractFileExt(FFileName)), AMIMEType) then
      AMIMEType := 'application/octet-stream';
    AObject.AddPair('mimeType', AMIMEType);
    AObject.AddPair('parents',
      TdxCloudStorageGoogleDriveProvider.CreateParentsObject(FParentID));
    ABytes := TEncoding.UTF8.GetBytes(AObject.ToString);
    Stream.Write(ABytes[0], Length(ABytes));
  finally
    AObject.Free;
  end;

  ABytes := TEncoding.ASCII.GetBytes
    (Format(#13#10'--%s'#13#10'Content-Type: %s'#13#10#13#10,
    [FBoundary, AMIMEType]));
  Stream.Write(ABytes[0], Length(ABytes));

  AFileStream := TBytesStream.Create;
  try
    Synchronize(procedure
      begin
        AFileStream.LoadFromFile(FFileName);
      end);
    Stream.Write(AFileStream.Bytes[0], AFileStream.Size);
  finally
    AFileStream.Free;
  end;
  ABytes := TEncoding.ASCII.GetBytes(Format(#13#10'--%s--', [FBoundary]));
  Stream.Write(ABytes[0], Length(ABytes));
end;

{ TdxCloudStorageGoogleDriveFileTransferManager }

function TdxCloudStorageGoogleDriveFileTransferManager.
  CreateUpdateFileStreamTask(AFile: TdxCloudStorageFile; AStream: TStream)
  : TdxCloudStorageUploadTask;
begin
  Result := TdxCloudStorageGoogleDriveUpdateFileStreamTask.Create(Provider,
    AFile, AStream);
end;

function TdxCloudStorageGoogleDriveFileTransferManager.CreateUploadFileTask
  (AParent: TdxCloudStorageFolder; const AFileName: string)
  : TdxCloudStorageUploadTask;
begin
  Result := TdxCloudStorageGoogleDriveUploadFileTask.Create(Provider, AParent,
    AFileName);
end;

function TdxCloudStorageGoogleDriveFileTransferManager.GetProvider
  : TdxCloudStorageGoogleDriveProvider;
begin
  Result := TdxCloudStorageGoogleDriveProvider(inherited Provider);
end;

{ TdxCloudStorageGoogleDriveProvider }

class procedure TdxCloudStorageGoogleDriveProvider.Initialize;
begin
  FDefaultMIMETypeExportTypeDictionary := TDictionary<string, string>.Create;
  FDefaultMIMETypeExportTypeDictionary.Add
    ('application/vnd.google-apps.document',
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document');
  FDefaultMIMETypeExportTypeDictionary.Add
    ('application/vnd.google-apps.spreadsheet',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
  FDefaultMIMETypeExportTypeDictionary.Add
    ('application/vnd.google-apps.presentation', 'application/pdf');

  FDefaultMIMETypeToExtensionDictionary := TDictionary<string, string>.Create;
  FDefaultMIMETypeToExtensionDictionary.Add
    ('application/vnd.google-apps.document', '.docx');
  FDefaultMIMETypeToExtensionDictionary.Add
    ('application/vnd.google-apps.spreadsheet', '.xlsx');
  FDefaultMIMETypeToExtensionDictionary.Add('application/pdf', '.pdf');

  FDefaultExtensionToMIMETypeDictionary := TDictionary<string, string>.Create;
  FDefaultExtensionToMIMETypeDictionary.Add('.docx',
    'application/vnd.google-apps.document');
  FDefaultExtensionToMIMETypeDictionary.Add('.xlsx',
    'application/vnd.google-apps.spreadsheet');
  FDefaultExtensionToMIMETypeDictionary.Add('.pdf', 'application/pdf');
end;

class procedure TdxCloudStorageGoogleDriveProvider.Finalize;
begin
  FreeAndNil(FDefaultMIMETypeToExtensionDictionary);
  FreeAndNil(FDefaultExtensionToMIMETypeDictionary);
  FreeAndNil(FDefaultMIMETypeExportTypeDictionary);
end;

class function TdxCloudStorageGoogleDriveProvider.GetSharedLink
  (const AUserAgent, AHeader: string; AItem: TdxCloudStorageItem;
var AResult: string): Boolean;
var
  AResponse: TdxJSONObject;
  AUri: string;
begin
  AUri := Format('https://www.googleapis.com/drive/v2/files/%s',
    [GetID(AItem)]);
  AResponse := TdxHttpHelper.GetRequest(AUserAgent, AUri, AHeader);
  try
    if AResponse = nil then
      Exit(False);
    AResult := AResponse.GetParamValue('alternateLink');
  finally
    AResponse.Free;
  end;
  Result := True;
end;

class function TdxCloudStorageGoogleDriveProvider.GetDisplayName: string;
begin
  Result := 'Google Drive';
end;

function TdxCloudStorageGoogleDriveProvider.GetExtension
  (AItem: TdxCloudStorageItem): string;
begin
  if AItem = nil then
    Exit('');
  if not DefaultMIMETypeToExtensionDictionary.TryGetValue(AItem.MIMEType, Result)
  then
    Result := inherited GetExtension(AItem);
end;

function TdxCloudStorageGoogleDriveProvider.CreateFiles: TdxCloudStorageFiles;
begin
  Result := TdxCloudStorageGoogleDriveFiles.Create(Self);
end;

function TdxCloudStorageGoogleDriveProvider.CreateFileTransferManager
  : TdxCloudStorageFileTransferManager;
begin
  Result := TdxCloudStorageGoogleDriveFileTransferManager.Create(Self);
end;

function TdxCloudStorageGoogleDriveProvider.CreateCopyItemToTask
  (AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder)
  : TdxCloudStorageProviderMoveItemToCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderCopyItemToTask.Create(Self,
    AItem, AParent);
end;

function TdxCloudStorageGoogleDriveProvider.CreateDeleteItemTask
  (AItem: TdxCloudStorageItem): TdxCloudStorageProviderDeleteItemTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderDeleteItemTask.Create
    (Self, AItem);
end;

function TdxCloudStorageGoogleDriveProvider.CreateDeletePermissionTask
  (APermission: TdxCloudStorageItemPermission)
  : TdxCloudStorageProviderDeletePermissionTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderDeletePermissionTask.Create(Self,
    APermission);
end;

function TdxCloudStorageGoogleDriveProvider.CreateCreateFolderTask
  (AParent: TdxCloudStorageFolder; const AName: string)
  : TdxCloudStorageProviderCreateFolderTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderCreateFolderTask.Create(Self,
    AParent, AName);
end;

function TdxCloudStorageGoogleDriveProvider.CreateCreateSharedLinkTask
  (AItem: TdxCloudStorageItem; AReadOnly: Boolean;
AScope: TdxCloudStorageItemPermissionScope)
  : TdxCloudStorageProviderCreateSharedLinkCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderCreateSharedLinkTask.Create(Self,
    AItem, AReadOnly, AScope);
end;

function TdxCloudStorageGoogleDriveProvider.CreateFetchAllTask
  : TdxCloudStorageProviderFetchAllCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderFetchAllTask.Create(Self);
end;

function TdxCloudStorageGoogleDriveProvider.CreateFetchPermissionsTask
  (AItem: TdxCloudStorageItem)
  : TdxCloudStorageProviderFetchPermissionsCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderFetchPermissionsTask.Create
    (Self, AItem);
end;

function TdxCloudStorageGoogleDriveProvider.CreateMoveItemToTask
  (AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder)
  : TdxCloudStorageProviderMoveItemToCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderMoveItemToTask.Create(Self,
    AItem, AParent);
end;

function TdxCloudStorageGoogleDriveProvider.CreateMoveItemToTrashTask
  (AItem: TdxCloudStorageItem): TdxCloudStorageProviderMoveItemToTrashTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderMoveItemToTrashTask.Create
    (Self, AItem);
end;

function TdxCloudStorageGoogleDriveProvider.CreateRestoreItemFromTrashTask
  (AItem: TdxCloudStorageItem): TdxCloudStorageProviderItemCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderRestoreItemFromTrashTask.Create
    (Self, AItem);
end;

function TdxCloudStorageGoogleDriveProvider.CreateUpdateFolderTask
  (AFolder: TdxCloudStorageFolder)
  : TdxCloudStorageProviderUpdateFolderCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderUpdateFolderTask.Create
    (Self, AFolder);
end;

function TdxCloudStorageGoogleDriveProvider.CreateUpdateSpecialFolderTask
  (AFolder: TdxCloudStorageSpecialFolder)
  : TdxCloudStorageProviderUpdateFolderCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderUpdateSpecialFolderTask.Create
    (Self, AFolder);
end;

function TdxCloudStorageGoogleDriveProvider.CreateInviteUsersTask
  (AItem: TdxCloudStorageItem; const AUsers: System.TArray<System.string>;
AReadOnly, ARequireSignIn, ASendInvitation: Boolean; const AMessage: string)
  : TdxCloudStorageProviderInviteUsersCustomTask;
begin
  Result := TdxCloudStorageGoogleDriveProviderInviteUsersTask.Create(Self,
    AItem, AUsers, AReadOnly, ARequireSignIn, ASendInvitation, AMessage);
end;

function TdxCloudStorageGoogleDriveProvider.CreateItem(const AMIMEType: string)
  : TdxCloudStorageItem;
begin
  if AMIMEType = FFolderMIMEType then
    Result := TdxCloudStorageFolder.Create(Files)
  else
    Result := TdxCloudStorageFile.Create(Files);
end;

function TdxCloudStorageGoogleDriveProvider.DoGetSpecialFolderData
  (const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData;
begin
  Result := inherited DoGetSpecialFolderData(AType);
  case AType of
    TdxCloudStorageSpecialFolder.TType.Recent:
      Result.Name := cxGetResourceString
        (@sdxCloudStorageGoogleDriveProviderRecentFolderName);
    TdxCloudStorageSpecialFolder.TType.SharedWithMe:
      Result.Name := cxGetResourceString
        (@sdxCloudStorageGoogleDriveProviderSharedWithMeFolderName);
    TdxCloudStorageSpecialFolder.TType.Starred:
      Result.Name := cxGetResourceString
        (@sdxCloudStorageGoogleDriveProviderStarredFolderName);
    TdxCloudStorageSpecialFolder.TType.Trash:
      Result.Name := cxGetResourceString
        (@sdxCloudStorageGoogleDriveProviderTrashFolderName);
  end;
  Result.MIMEType := FFolderMIMEType;
end;

function TdxCloudStorageGoogleDriveProvider.ObjectToData(const AObject: TObject)
  : TdxCloudStorageItemData;
var
  AJson: TdxJSONObject;
  AParents: TdxJSONValue;
  AExportMIMEType: string;
begin
  AJson := TdxJSONObject(AObject);
  with Result do
  begin
    ID := AJson.GetParamValue('id');
    Name := AJson.GetParamValue('title');
    MIMEType := AJson.GetParamValue('mimeType');
    AParents := AJson.GetValue('parents');
    Parents := GetObjectParents(TdxJSONArray(AParents));
    IconLink := AJson.GetParamValue('iconLink');
    CreatedDate := TdxISO8601Helper.StringToDateTime
      (AJson.GetParamValue('createdDate'));
    ModifiedDate := TdxISO8601Helper.StringToDateTime
      (AJson.GetParamValue('modifiedDate'));
    Shared := AJson.GetValue('shared').IsTrue;
    Trashed := AJson.GetChild('labels', 'trashed').IsTrue;

    if AJson.HasParam('downloadUrl') then
      DownloadLink :=
        Format('https://www.googleapis.com/drive/v2/files/%s?alt=media', [ID])
    else if AJson.HasParam('exportLinks') then
    begin
      if FDefaultMIMETypeExportTypeDictionary.TryGetValue(MIMEType,
        AExportMIMEType) then
        DownloadLink :=
          Format('https://www.googleapis.com/drive/v2/files/%s/export?mimeType=%s',
          [ID, TdxURI.EscapeDataString(AExportMIMEType)])
    end
    else
      DownloadLink := '';

    if not TryStrToInt(AJson.GetParamValue('fileSize'), FileSize) then
      FileSize := -1;
  end;
end;

class function TdxCloudStorageGoogleDriveProvider.CreateParentsObject
  (const AParents: TArray<string>): TdxJSONArray;
var
  AParent: string;
begin
  Result := TdxJSONArray.Create;
  for AParent in AParents do
    Result.AddElement(TdxJSONObject.Create(TdxJSONPair.Create('id', AParent)));
end;

class function TdxCloudStorageGoogleDriveProvider.CreateParentsObject
  (const AParent: string): TdxJSONArray;
begin
  Result := CreateParentsObject(TArray<string>.Create(AParent));
end;

function TdxCloudStorageGoogleDriveProvider.GetScopes: TStringList;
begin
  Result := inherited GetScopes;
  Result.Add('https://www.googleapis.com/auth/drive');
end;

function TdxCloudStorageGoogleDriveProvider.GetObjectParents
  (AParents: TdxJSONArray): TArray<string>;
var
  AResult: TStringList;
  AParent: TdxJSONValue;
  I: Integer;
begin
  if AParents.Count = 0 then
    Exit(nil);
  AResult := TStringList.Create;
  try
    for I := 0 to AParents.Count - 1 do
    begin
      AParent := AParents.Items[I];
      if AParent.GetParamValue('kind') = 'drive#parentReference' then
        AResult.Add(AParent.GetParamValue('id'))
    end;
    Result := TdxStringHelper.Split(AResult.Text, [#13#10]);
  finally
    AResult.Free;
  end;
end;

function TdxCloudStorageGoogleDriveProvider.GetAuthorizationAgent
  : TdxGoogleAPIOAuth2AuthorizationAgent;
begin
  Result := TdxGoogleAPIOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

function TdxCloudStorageGoogleDriveProvider.GetFiles
  : TdxCloudStorageGoogleDriveFiles;
begin
  Result := TdxCloudStorageGoogleDriveFiles(inherited Files);
end;

procedure TdxCloudStorageGoogleDriveProvider.SetAuthorizationAgent
  (const Value: TdxGoogleAPIOAuth2AuthorizationAgent);
begin
  inherited AuthorizationAgent := Value;
end;

initialization

TdxCloudStorageGoogleDriveUploadFileTask.Initialize;
TdxCloudStorageGoogleDriveProvider.Initialize;
TdxCloudStorageGoogleDriveProvider.Register;

finalization

TdxCloudStorageGoogleDriveProvider.Unregister;
TdxCloudStorageGoogleDriveProvider.Finalize;

end.
