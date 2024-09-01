
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Storage Editor
//////////////////////////////////////////////////

{$I SB.inc}

unit ScStorageEditor;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, SysUtils, Classes,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  ScEditor, ScTabEditor, ScFrame, ScKeysFrame, ScUsersFrame,
  ScCertificatesFrame, ScBridge;

type
  TScStorageForm = class(TScTabEditorForm)
    shKeys: TTabSheet;
    shUsers: TTabSheet;
    shCertificates: TTabSheet;
  protected
    FKeysFrame: TScKeysFrame;
    FUsersFrame: TScUsersFrame;
    FCertificatesFrame: TScCertificatesFrame;

    FStorage: TScStorage;
//    FLocalComponent: TComponent;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoFinish; override;
    procedure DoSave; override;

    function GetLocalComponent: TComponent; override;
    function GetFrameByInitProp: TScFrame; override;

  public
    property KeysFrame: TScKeysFrame read FKeysFrame;
    property UsersFrame: TScUsersFrame read FUsersFrame;
    property CertificatesFrame: TScCertificatesFrame read FCertificatesFrame;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  ScCryptoAPIStorage;

function TScStorageForm.GetComponent: TComponent;
begin
  Result := FStorage;
end;

procedure TScStorageForm.SetComponent(Value: TComponent);
begin
  FStorage := Value as TScStorage;
end;

{$IFDEF LINUX}
  {$UNDEF CRYPTOAPI_USED}
{$ELSE}
{$IFDEF LINUX_BSD}
  {$UNDEF CRYPTOAPI_USED}
{$ELSE}
{$IFDEF ANDROID}
  {$UNDEF CRYPTOAPI_USED}
{$ELSE}
  {$DEFINE CRYPTOAPI_USED}
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TScStorageForm.DoInit;
begin
  inherited;

  FKeysFrame := AddTab(TScKeysFrame, shKeys) as TScKeysFrame;

{$IFDEF CRYPTOAPI_USED}
  if FStorage is TScCryptoAPIStorage then
    shUsers.TabVisible := False
  else
{$ENDIF}
  begin
    shUsers.TabVisible := True;
    FUsersFrame := AddTab(TScUsersFrame, shUsers) as TScUsersFrame;
  end;

  FCertificatesFrame := AddTab(TScCertificatesFrame, shCertificates) as TScCertificatesFrame;

  try
//    FLocalComponent := TComponentClass(FComponent.ClassType).Create(nil);
//    FLocalComponent.Assign(FComponent);
  finally
    Assert(FKeysFrame <> nil);
  {$IFDEF CRYPTOAPI_USED}
    if not (FStorage is TScCryptoAPIStorage) then
  {$ENDIF}
      Assert(FUsersFrame <> nil);
    Assert(FCertificatesFrame <> nil);

    Modified := False;
  end;
end;

procedure TScStorageForm.DoActivate;
begin
  inherited;
end;

procedure TScStorageForm.DoFinish;
begin
//  FLocalComponent.Free;
//  FLocalComponent := nil;
  inherited;
end;

procedure TScStorageForm.DoSave;
begin
  inherited;
//  FComponent.Assign(FLocalComponent);
end;

function TScStorageForm.GetLocalComponent: TComponent;
begin
  Result := nil; //FLocalComponent;
end;

function TScStorageForm.GetFrameByInitProp: TScFrame;
begin
  if InitialProperty = 'Keys' then
    Result := FKeysFrame
  else
  if InitialProperty = 'Users' then
    Result := FUsersFrame
  else
  if InitialProperty = 'Certificates' then
    Result := FCertificatesFrame
  else
    Result := inherited GetFrameByInitProp;
end;

end.
