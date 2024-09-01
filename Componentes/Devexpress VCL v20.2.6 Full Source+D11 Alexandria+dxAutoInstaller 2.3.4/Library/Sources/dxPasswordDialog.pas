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

unit dxPasswordDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Forms, Menus, StdCtrls, Controls,
  Dialogs,
  cxGraphics, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, dxForms,
  cxControls, cxContainer,
  dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxEdit,
  cxTextEdit, cxButtons;

type
  TdxPasswordDialogFormMode = (pdmNew, pdmQuery, pdmConfirmation);
  // for internal use

  { TdxPasswordDialogForm }

  TdxPasswordDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edPassword: TcxTextEdit;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liNotes: TdxLayoutLabeledItem;
    liPassword: TdxLayoutItem;
    liRepeatPassword: TdxLayoutItem;
    edRepeatPassword: TcxTextEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  strict private
    FLookAndFeel: TcxLookAndFeel;
    FMode: TdxPasswordDialogFormMode;
    FPassword: string;
  protected
    class function GetPasswordDialogPasswordNotMatchText: string; virtual;
    procedure ApplyLocalizations; virtual;
    procedure Initialize(ALookAndFeel: TcxLookAndFeel;
      AMode: TdxPasswordDialogFormMode); virtual;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
    property Mode: TdxPasswordDialogFormMode read FMode;
    property Password: string read FPassword write FPassword;
  public
    class function Execute(AOwner: TComponent; ALookAndFeel: TcxLookAndFeel;
      AMode: TdxPasswordDialogFormMode; var APassword: string;
      const ACaption: string = ''): Boolean; // for internal use
    class function ExecuteConfirmation(AOwner: TComponent;
      ALookAndFeel: TcxLookAndFeel; const APassword: string): Boolean;
    // for internal use
  end;

implementation

uses
  dxCore, cxGeometry, dxCoreGraphics, dxPasswordDialogStrs;

{$R *.dfm}
{ TdxPasswordDialogForm }

class function TdxPasswordDialogForm.Execute(AOwner: TComponent;
  ALookAndFeel: TcxLookAndFeel; AMode: TdxPasswordDialogFormMode;
  var APassword: string; const ACaption: string = ''): Boolean;
var
  ADialog: TdxPasswordDialogForm;
begin
  ADialog := Create(AOwner);
  try
    ADialog.Initialize(ALookAndFeel, AMode);
    if ACaption <> '' then
      ADialog.Caption := ACaption;
    if APassword <> '' then
      ADialog.edPassword.Text := APassword;
    ADialog.Password := APassword;
    Result := ADialog.ShowModal = mrOk;
    if Result then
      APassword := ADialog.edPassword.Text;
  finally
    ADialog.Free;
  end;
end;

class function TdxPasswordDialogForm.ExecuteConfirmation(AOwner: TComponent;
  ALookAndFeel: TcxLookAndFeel; const APassword: string): Boolean;
var
  AConfirmedPassword: string;
begin
  Result := False;
  if Execute(AOwner, ALookAndFeel, pdmConfirmation, AConfirmedPassword, '') then
  begin
    if APassword <> AConfirmedPassword then
      MessageDlg(GetPasswordDialogPasswordNotMatchText, mtWarning, [mbOK], 0)
    else
      Result := True;
  end;
end;

procedure TdxPasswordDialogForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  AText: string;
begin
  if ModalResult <> mrOk then
    Exit;
  if Mode = pdmNew then
  begin
    if not liRepeatPassword.Visible then
    begin
      if (edPassword.Text <> '') and (FPassword <> edPassword.Text) then
      begin
        lcMain.BeginUpdate;
        liRepeatPassword.Visible := True;
        liPassword.Visible := False;
        lcMain.EndUpdate;
        edRepeatPassword.SetFocus;
        CanClose := False;
        Exit;
      end;
    end
    else
    begin
      CanClose := edPassword.Text = edRepeatPassword.Text;
      if not CanClose then
      begin
        AText := cxGetResourceString(@sdxPasswordDialogPasswordNotMatch);
        Application.MessageBox(PChar(AText), PChar(Application.Title),
          MB_OK + MB_ICONWARNING);
        Exit;
      end
    end;
  end;
  FPassword := edPassword.Text;
end;

class function TdxPasswordDialogForm.
  GetPasswordDialogPasswordNotMatchText: string;
begin
  Result := cxGetResourceString(@sdxPasswordDialogPasswordNotMatch);
end;

procedure TdxPasswordDialogForm.ApplyLocalizations;
begin
  if Mode = pdmConfirmation then
  begin
    Caption := cxGetResourceString(@sdxPasswordDialogCaptionConfirm);
    liPassword.Caption := cxGetResourceString
      (@sdxPasswordDialogPasswordConfirmation)
  end
  else
  begin
    Caption := cxGetResourceString(@sdxPasswordDialogCaption);
    liPassword.Caption := cxGetResourceString(@sdxPasswordDialogPassword);
  end;
  liRepeatPassword.Caption := cxGetResourceString
    (@sdxPasswordDialogPasswordConfirmation);
  liNotes.Caption := cxGetResourceString(@sdxPasswordDialogPasswordNotes);
  btnCancel.Caption := cxGetResourceString(@sdxPasswordDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxPasswordDialogButtonOK);
end;

procedure TdxPasswordDialogForm.Initialize(ALookAndFeel: TcxLookAndFeel;
  AMode: TdxPasswordDialogFormMode);
begin
  FMode := AMode;
  FLookAndFeel := ALookAndFeel;
  SetControlLookAndFeel(Self, FLookAndFeel);

  liNotes.Visible := Mode = pdmConfirmation;
  liRepeatPassword.Visible := Mode = pdmNew;

  edPassword.Properties.ShowPasswordRevealButton := Mode <> pdmNew;
  edRepeatPassword.Properties.ShowPasswordRevealButton :=
    edPassword.Properties.ShowPasswordRevealButton;

  ApplyLocalizations;
  ActiveControl := edPassword;
end;

end.
