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

unit dxX509CertificatePasswordDialogStrs;

{$I cxVer.inc}

interface

resourcestring
  sdxX509CertificatePasswordDialogCaption = 'Private Key Protection';
  sdxX509CertificatePasswordDialogPassword = 'Enter password:';
  sdxX509CertificatePasswordDialogPasswordNotes =
    'To maintain security, the private key was protected with a password.';

implementation

uses
  dxCore;

procedure AddResourceStringsPart(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxX509CertificatePasswordDialogCaption',
    @sdxX509CertificatePasswordDialogCaption);
  AProduct.Add('sdxX509CertificatePasswordDialogPasswordNotes',
    @sdxX509CertificatePasswordDialogPasswordNotes);
  AProduct.Add('sdxX509CertificatePasswordDialogPassword',
    @sdxX509CertificatePasswordDialogPassword);
end;

initialization

dxResourceStringsRepository.RegisterProduct('ExpressOfficeCore Library',
  @AddResourceStringsPart);

finalization

dxResourceStringsRepository.UnRegisterProduct('ExpressOfficeCore Library');

end.
