{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LCLTMSFNCCorePkgDE;

{$warn 5023 off : no warning about unused units}
interface

uses
  LCLTMSFNCBitmapContainerDE, LCLTMSFNCBitmapContainerRegDE, 
  LCLTMSFNCGeneralDE, LCLTMSFNCGeneralRegDE, LCLTMSFNCDataBindingDE, 
  LCLTMSFNCDataBindingEditorDE, LCLTMSFNCDataBindingEditorRegDE, 
  LCLTMSFNCDataBindingRegDE, LCLTMSFNCBitmapEditorDE, 
  LCLTMSFNCBitmapEditorRegDE, LCLTMSFNCGraphicsAppearanceEditorDE, 
  LCLTMSFNCGraphicsAppearanceEditorRegDE, LCLTMSFNCBitmapContainerEditorDE, 
  LCLTMSFNCBitmapContainerEditorRegDE, LCLTMSFNCResponsiveManagerDE, 
  LCLTMSFNCResponsiveManagerRegDE, LCLTMSFNCResponsiveManagerDimensionsEditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LCLTMSFNCBitmapContainerRegDE', 
    @LCLTMSFNCBitmapContainerRegDE.Register);
  RegisterUnit('LCLTMSFNCGeneralRegDE', @LCLTMSFNCGeneralRegDE.Register);
  RegisterUnit('LCLTMSFNCDataBindingEditorRegDE', 
    @LCLTMSFNCDataBindingEditorRegDE.Register);
  RegisterUnit('LCLTMSFNCDataBindingRegDE', @LCLTMSFNCDataBindingRegDE.Register
    );
  RegisterUnit('LCLTMSFNCBitmapEditorRegDE', 
    @LCLTMSFNCBitmapEditorRegDE.Register);
  RegisterUnit('LCLTMSFNCGraphicsAppearanceEditorRegDE', 
    @LCLTMSFNCGraphicsAppearanceEditorRegDE.Register);
  RegisterUnit('LCLTMSFNCBitmapContainerEditorRegDE', 
    @LCLTMSFNCBitmapContainerEditorRegDE.Register);
  RegisterUnit('LCLTMSFNCResponsiveManagerRegDE', 
    @LCLTMSFNCResponsiveManagerRegDE.Register);
end;

initialization
  RegisterPackage('LCLTMSFNCCorePkgDE', @Register);
end.
