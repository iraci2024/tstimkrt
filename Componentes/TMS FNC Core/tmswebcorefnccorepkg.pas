{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TMSWebCoreFNCCorePkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  WEBLib.TMSFNCBitmapContainer, WEBLib.TMSFNCCoreWEBReg, 
  WEBLib.TMSFNCCustomComponent, WEBLib.TMSFNCCustomControl, 
  WEBLib.TMSFNCCustomScrollControl, WEBLib.TMSFNCGraphics.General, 
  WEBLib.TMSFNCGraphics, WEBLib.TMSFNCGraphics.WEB, 
  WEBLib.TMSFNCGraphicsTypes, WEBLib.TMSFNCHTMLEngine, WEBLib.TMSFNCPopup, 
  WEBLib.TMSFNCScrollBar, WEBLib.TMSFNCStyles, WEBLib.TMSFNCTypes, 
  WEBLib.TMSFNCUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('TMSWebCoreFNCCorePkg', @Register);
end.
