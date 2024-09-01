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

unit dxX509CertificatePasswordDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Forms, Menus, StdCtrls, Controls,
  cxGraphics, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, dxForms,
  cxControls, cxContainer,
  dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxEdit,
  cxTextEdit, cxButtons, dxPasswordDialog;

type
  { TdxX509CertificatePasswordDialogForm }

  TdxX509CertificatePasswordDialogForm = class(TdxPasswordDialogForm)
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
  protected
    procedure ApplyLocalizations; override;
    procedure Initialize(ALookAndFeel: TcxLookAndFeel;
      AMode: TdxPasswordDialogFormMode); override;
  public
    class function Execute(AOwner: TComponent; ALookAndFeel: TcxLookAndFeel;
      var APassword: string): Boolean; overload; static;
  end;

implementation

uses
  dxCore, dxX509CertificatePasswordDialogStrs;

{$R *.dfm}
{ TdxX509CertificatePasswordDialogForm }

class function TdxX509CertificatePasswordDialogForm.Execute(AOwner: TComponent;
  ALookAndFeel: TcxLookAndFeel; var APassword: string): Boolean;
begin
  Result := Execute(AOwner, ALookAndFeel, pdmQuery, APassword, '');
end;

procedure TdxX509CertificatePasswordDialogForm.ApplyLocalizations;
begin
  inherited ApplyLocalizations;
  Caption := cxGetResourceString(@sdxX509CertificatePasswordDialogCaption);
  liPassword.Caption := cxGetResourceString
    (@sdxX509CertificatePasswordDialogPassword);
  liNotes.Caption := cxGetResourceString
    (@sdxX509CertificatePasswordDialogPasswordNotes);
end;

procedure TdxX509CertificatePasswordDialogForm.Initialize
  (ALookAndFeel: TcxLookAndFeel; AMode: TdxPasswordDialogFormMode);
begin
  inherited Initialize(ALookAndFeel, pdmQuery);
  liNotes.Visible := True;
end;

end.
