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

unit dxAuthorizationAgentsStrs;

{$I cxVer.inc}

interface

resourcestring
  sdxGoogleAPIOAuth2AuthorizationFormCaption = 'Google Authorization';
  sdxGoogleAPIOAuth2AuthorizationFormMessage =
    '[B]Finish authorization in a browser.[/B]'#13#10'Close this dialog to abort authorization.';
  sdxGoogleAPIOAuth2AuthorizationFormResponseContent =
    '<html><head><meta http-equiv="refresh" content="10;url=https://google.com"></head><body>The client application is now authorized. You can close this window.</body></html>';

implementation

uses
  dxCore;

procedure AddResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxGoogleAPIOAuth2AuthorizationFormCaption',
    @sdxGoogleAPIOAuth2AuthorizationFormCaption);
  AProduct.Add('sdxGoogleAPIOAuth2AuthorizationFormMessage',
    @sdxGoogleAPIOAuth2AuthorizationFormMessage);
  AProduct.Add('sdxGoogleAPIOAuth2AuthorizationFormResponseContent',
    @sdxGoogleAPIOAuth2AuthorizationFormResponseContent);
end;

initialization

dxResourceStringsRepository.RegisterProduct('ExpressOfficeCore Library',
  @AddResourceStringNames);

finalization

dxResourceStringsRepository.UnRegisterProduct('ExpressOfficeCore Library',
  @AddResourceStringNames);

end.
