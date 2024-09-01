{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit dclsbridge10; 

interface

uses
  ScFrame, ScEditor, ScDesign, ScDesignUtils, ScCertificatesFrame, 
  ScColFrame, ScCollectionEditor, ScDualListEditor, ScKeysFrame, ScReg,
  ScStorageEditor, ScTabEditor, ScUsersFrame, ScStringMapEditor,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ScDesign', @ScDesign.Register); 
  RegisterUnit('ScReg', @ScReg.Register); 
end; 

initialization
  RegisterPackage('dclsbridge10', @Register); 
end.
