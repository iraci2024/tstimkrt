{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressRichEditControl }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL }
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

unit dxRichEdit.Dialogs.QueryPassword;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus, StdCtrls,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels, dxForms,
  cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer,
  dxLayoutControl,
  cxContainer, cxEdit, dxLayoutcxEditAdapters, cxTextEdit,
  dxLayoutControlAdapters, cxButtons,
  dxPasswordDialog;

type
  TdxQueryPasswordKind = (Query, SetNewWithConfirmation,
    SetNewWithOptionalConfirmation);

  TdxQueryPasswordForm = class(TdxPasswordDialogForm)
  protected
    class function GetPasswordDialogPasswordNotMatchText: string; override;
    procedure ApplyLocalizations; override;
  end;

function ShowQueryPasswordDialog(AKind: TdxQueryPasswordKind;
  var APassword: string; ACaption: string = '';
  AOwner: TComponent = nil): Boolean;

implementation

uses
  dxRichEdit.Dialogs.Strs, dxCore;

{$R *.dfm}

function ShowQueryPasswordDialog(AKind: TdxQueryPasswordKind;
  var APassword: string; ACaption: string = '';
  AOwner: TComponent = nil): Boolean;
const
  ModeMap: array [TdxQueryPasswordKind] of TdxPasswordDialogFormMode =
    (pdmQuery, pdmNew, pdmNew);
var
  ALookAndFeel: TcxLookAndFeel;
  AOwnerLookAndFeel: IcxLookAndFeelContainer;
begin
  ALookAndFeel := nil;
  if Supports(AOwner, IcxLookAndFeelContainer, AOwnerLookAndFeel) then
    ALookAndFeel := AOwnerLookAndFeel.GetLookAndFeel;
  Result := TdxQueryPasswordForm.Execute(AOwner, ALookAndFeel, ModeMap[AKind],
    APassword, ACaption);
end;

{ TdxQueryPasswordForm }

class function TdxQueryPasswordForm.
  GetPasswordDialogPasswordNotMatchText: string;
begin
  Result := cxGetResourceString
    (@sdxQueryNewPasswordInvalidPasswordConfirmation);
end;

procedure TdxQueryPasswordForm.ApplyLocalizations;
begin
  inherited ApplyLocalizations;
  Caption := cxGetResourceString(@sdxQueryPasswordForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  case Mode of
    pdmQuery:
      liPassword.CaptionOptions.Text :=
        cxGetResourceString(@sdxQueryPasswordPassword);
    pdmNew, pdmConfirmation:
      begin
        liPassword.CaptionOptions.Text :=
          cxGetResourceString(@sdxQueryNewPasswordPassword);
        liRepeatPassword.CaptionOptions.Text :=
          cxGetResourceString(@sdxQueryNewPasswordRepeatPassword);
      end;
  end;
end;

end.
