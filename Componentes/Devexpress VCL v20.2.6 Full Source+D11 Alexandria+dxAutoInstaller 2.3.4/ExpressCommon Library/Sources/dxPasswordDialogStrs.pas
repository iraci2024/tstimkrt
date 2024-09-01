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

unit dxPasswordDialogStrs;

{$I cxVer.inc}

interface

resourcestring
  sdxPasswordDialogCaption = 'Password Protection';
  sdxPasswordDialogCaptionConfirm = 'Confirm Password';
  sdxPasswordDialogPassword = '&Enter password:';
  sdxPasswordDialogPasswordConfirmation = 'Reenter &password to proceed:';
  sdxPasswordDialogPasswordNotMatch = 'The passwords do not match.';
  sdxPasswordDialogPasswordNotes =
    'Caution: If you lose or forget the password, it cannot be recovered. It''s recommended that you keep it in a safe place.';
  sdxPasswordDialogButtonCancel = 'Cancel';
  sdxPasswordDialogButtonOK = 'OK';

implementation

uses
  dxCore;

procedure AddResourceStringsPart(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxPasswordDialogButtonCancel', @sdxPasswordDialogButtonCancel);
  AProduct.Add('sdxPasswordDialogButtonOK', @sdxPasswordDialogButtonOK);
  AProduct.Add('sdxPasswordDialogCaption', @sdxPasswordDialogCaption);
  AProduct.Add('sdxPasswordDialogCaptionConfirm',
    @sdxPasswordDialogCaptionConfirm);
  AProduct.Add('sdxPasswordDialogPasswordNotes',
    @sdxPasswordDialogPasswordNotes);
  AProduct.Add('sdxPasswordDialogPassword', @sdxPasswordDialogPassword);
  AProduct.Add('sdxPasswordDialogPasswordConfirmation',
    @sdxPasswordDialogPasswordConfirmation);
  AProduct.Add('sdxPasswordDialogPasswordNotMatch',
    @sdxPasswordDialogPasswordNotMatch);
end;

initialization

dxResourceStringsRepository.RegisterProduct('ExpressOfficeCore Library',
  @AddResourceStringsPart);

finalization

dxResourceStringsRepository.UnRegisterProduct('ExpressOfficeCore Library');

end.
