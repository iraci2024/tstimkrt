
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Collection Editor
//////////////////////////////////////////////////

{$I SB.inc}

unit ScCollectionEditor;

interface

uses
{$IFDEF FPC}
  PropEdits, ComponentEditors,
{$ELSE}
  DesignIntf, DesignEditors,
{$ENDIF}
  Classes, Controls, Forms, TypInfo, ScDualListEditor, ScDesignUtils,
  ScUtils, ScBridge;

type
  TScCollectionEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TScAlgorithmsForm = class(TScDualListEditorForm)
  private
    FCurType: PTypeInfo;
    FCollection: TScCollection;
  protected
    procedure DoInit; override;
    procedure DoSave; override;
    procedure GetSrcListItems(Items: TStrings); override;
    procedure GetDstListItems(Items: TStrings); override;
    function GetComponent: TComponent; override;
  public
    property Collection: TScCollection read FCollection write FCollection;
  end;


implementation

uses
  ScSSHUtils, ScAlgorithmSupport,
  ScSSLTypes, ScCipherSuites, ScSSLClient;

{ TScCollectionEditor }

procedure TScCollectionEditor.Edit;
var
  EditForm: TScAlgorithmsForm;
begin
  EditForm := TScAlgorithmsForm.Create(nil, TScDesignUtils);
  try
    EditForm.Collection := TScCollection({$IFDEF FPC}{$IFDEF LINUX_BSD}GetObjectValue{$ELSE}GetOrdValue{$ENDIF}{$ELSE}GetOrdValue{$ENDIF});
    EditForm.ShowModal;
    if EditForm.ModalResult = mrOk then
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
  finally
    EditForm.Free;
  end;
end;

function TScCollectionEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TScCollectionEditor.GetValue: string;
var
  Collection: TScCollection;
begin
  Collection := TScCollection({$IFDEF FPC}{$IFDEF LINUX_BSD}GetObjectValue{$ELSE}GetOrdValue{$ENDIF}{$ELSE}GetOrdValue{$ENDIF});
  if Collection is TScSSHCiphers then
    Result := TScSSHCiphers(Collection).AsString
  else if Collection is TScSSHHMacAlgorithms then
    Result := TScSSHHMacAlgorithms(Collection).AsString
  else if Collection is TScSSHKeyExchangeAlgorithms then
    Result := TScSSHKeyExchangeAlgorithms(Collection).AsString
  else if Collection is TScSSHHostKeyAlgorithms then
    Result := TScSSHHostKeyAlgorithms(Collection).AsString
  else if Collection is TScSSLCipherSuites then
    Result := TScSSLCipherSuites(Collection).AsString
  else
    Assert(False);
end;

procedure TScCollectionEditor.SetValue(const Value: string);
var
  Collection: TScCollection;
begin
  Collection := TScCollection({$IFDEF FPC}{$IFDEF LINUX_BSD}GetObjectValue{$ELSE}GetOrdValue{$ENDIF}{$ELSE}GetOrdValue{$ENDIF});
  if Collection is TScSSHCiphers then begin
    if TScSSHCiphers(Collection).AsString <> Value then begin
      TScSSHCiphers(Collection).AsString := Value;
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
    end;
  end
  else if Collection is TScSSHHMacAlgorithms then begin
    if TScSSHHMacAlgorithms(Collection).AsString <> Value then begin
      TScSSHHMacAlgorithms(Collection).AsString := Value;
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
    end;
  end
  else if Collection is TScSSHKeyExchangeAlgorithms then begin
    if TScSSHKeyExchangeAlgorithms(Collection).AsString <> Value then begin
      TScSSHKeyExchangeAlgorithms(Collection).AsString := Value;
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
    end;
  end
  else if Collection is TScSSHHostKeyAlgorithms then begin
    if TScSSHHostKeyAlgorithms(Collection).AsString <> Value then begin
      TScSSHHostKeyAlgorithms(Collection).AsString := Value;
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
    end;
  end
  else if Collection is TScSSLCipherSuites then begin
    if TScSSLCipherSuites(Collection).AsString <> Value then begin
      TScSSLCipherSuites(Collection).AsString := Value;
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
    end;
  end
  else
    Assert(False);
end;

{ TScAlgorithmsForm }

procedure TScAlgorithmsForm.DoInit;
begin
  Assert(FCollection <> nil);

  if FCollection.ItemClass = TScSSHCipherItem then
    FCurType := TypeInfo(TScSymmetricAlgorithm)
  else if FCollection.ItemClass = TScSSHHMacAlgorithmItem then
    FCurType := TypeInfo(TScHMACAlgorithm)
  else if FCollection.ItemClass = TScSSHKeyExchangeAlgorithmItem then
    FCurType := TypeInfo(TScKeyExchangeAlgorithm)
  else if FCollection.ItemClass = TScSSHHostKeyAlgorithmItem then
    FCurType := TypeInfo(TScAsymmetricAlgorithm)
  else if FCollection.ItemClass = TScSSLCipherSuiteItem then
    FCurType := TypeInfo(TScSSLCipherAlgorithm)
  else begin
    FCurType := nil;
    Assert(False);
  end;

  inherited;
  Caption := Component.Name + '.' + FCollection.ClassName;
end;

procedure TScAlgorithmsForm.DoSave;
var
  i: Integer;
  list: string;
begin
  Assert(FCollection <> nil);

  list := '';
  for i := 0 to DstList.Items.Count - 1 do begin
    if i > 0 then
      list := list + ',';
    list := list + DstList.Items[i];
  end;

  if FCollection is TScSSHCiphers then
    (FCollection as TScSSHCiphers).AsString := list
  else if FCollection is TScSSHHMacAlgorithms then
    (FCollection as TScSSHHMacAlgorithms).AsString := list
  else if FCollection is TScSSHKeyExchangeAlgorithms then
    (FCollection as TScSSHKeyExchangeAlgorithms).AsString := list
  else if FCollection is TScSSHHostKeyAlgorithms then
    (FCollection as TScSSHHostKeyAlgorithms).AsString := list
  else if FCollection is TScSSLCipherSuites then
    (FCollection as TScSSLCipherSuites).AsString := list;
end;

procedure TScAlgorithmsForm.GetDstListItems(Items: TStrings);
var
  i: Integer;
begin
  Assert(FCollection <> nil);

  Items.Clear;
  for i := 0 to FCollection.Count - 1 do begin
    if FCollection is TScSSHCiphers then
      Items.Add((TScSSHCiphers(FCollection).Items[i] as TScSSHCipherItem).AsString)
    else if FCollection is TScSSHHMacAlgorithms then
      Items.Add((TScSSHHMacAlgorithms(FCollection).Items[i] as TScSSHHMacAlgorithmItem).AsString)
    else if FCollection is TScSSHKeyExchangeAlgorithms then
      Items.Add((TScSSHKeyExchangeAlgorithms(FCollection).Items[i] as TScSSHKeyExchangeAlgorithmItem).AsString)
    else if FCollection is TScSSHHostKeyAlgorithms then
      Items.Add((TScSSHHostKeyAlgorithms(FCollection).Items[i] as TScSSHHostKeyAlgorithmItem).AsString)
    else if FCollection is TScSSLCipherSuites then
      Items.Add((TScSSLCipherSuites(FCollection).Items[i] as TScSSLCipherSuiteItem).AsString);
  end;
end;

procedure TScAlgorithmsForm.GetSrcListItems(Items: TStrings);
var
  i: Integer;
begin
  Assert(FCollection <> nil);

  Items.Clear;
  for i := GetTypeData(FCurType)^.MinValue to GetTypeData(FCurType)^.MaxValue do begin
    if FCurType = TypeInfo(TScSymmetricAlgorithm) then
      Items.Add(CipherFactory.CipherAlgorithmToSSH2Name(TScSymmetricAlgorithm(i)))
    else if FCurType = TypeInfo(TScHMACAlgorithm) then
      Items.Add(CipherFactory.HMACAlgorithmToSSH2Name(TScHMACAlgorithm(i)))
    else if FCurType = TypeInfo(TScKeyExchangeAlgorithm) then
      Items.Add(CipherFactory.KeyExchangeAlgorithmToSSH2Name(TScKeyExchangeAlgorithm(i)))
    else if FCurType = TypeInfo(TScAsymmetricAlgorithm) then
      Items.Add(CipherFactory.PublicKeyAlgorithmToSSH2Name(TScAsymmetricAlgorithm(i)))
    else if FCurType = TypeInfo(TScSSLCipherAlgorithm) then
      Items.Add(TCipherSuites.CipherAlgorithmToName(TScSSLCipherAlgorithm(i)));
  end;
end;

function TScAlgorithmsForm.GetComponent: TComponent;
begin
  if FCollection <> nil then
    Result := FCollection.Owner as TComponent
  else
    Result := nil;
end;

end.
