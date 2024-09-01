{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSkins Library }
{ }
{ Copyright (c) 2006-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING }
{ VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSkinsFluentDesignFormReg;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Menus, TypInfo, Windows, VCLEditors, Graphics, Forms,
  Controls,
  DesignIntf, DesignEditors, DesignConst, DesignMenus, ToolsApi,
  dxDesignHelpers,
  dxCore, dxFluentDesignFormInterfaces, dxSkinsFluentDesignForm,
  dxSkinsFluentDesignFormWizard;

procedure Register;

implementation

type

  { TdxFluentDesignFormNavigationControlProperty }

  TdxFluentDesignFormNavigationControlProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TdxFluentDesignFormNavigationControlProperty.CheckComponent
  (const Value: string);
var
  AComponent: TComponent;
begin
  AComponent := Designer.GetComponent(Value);
  if Supports(AComponent, IdxFluentDesignNavigationControl) then
    FProc(Value);
end;

procedure TdxFluentDesignFormNavigationControlProperty.GetValues
  (Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

procedure Register;
var
  APersonality: TdxOTAPersonality;
begin
  ForceDemandLoadState(dlDisable);

  for APersonality := Low(TdxOTAPersonality) to High(TdxOTAPersonality) do
  begin
{$IF DEFINED(DELPHI14) AND NOT DEFINED(DELPHIXE)}
    if APersonality = dxopCBuilder then
      Continue;
{$IFEND}
    dxRegisterPackageWizard(TdxOTAFluentDesignApplicationWizard.Create
      (APersonality));
    dxRegisterPackageWizard(TdxOTAFluentDesignFormWizard.Create(APersonality));
  end;

  RegisterNoIcon([TdxFluentDesignForm]);
  RegisterCustomModule(TdxFluentDesignForm, TCustomModule);
  RegisterPropertyEditor(TypeInfo(TWinControl), TdxFluentDesignForm,
    'NavigationControl', TdxFluentDesignFormNavigationControlProperty);
end;

end.
