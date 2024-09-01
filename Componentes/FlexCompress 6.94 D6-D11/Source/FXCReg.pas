 //==============================================================================
 // Product name: FlexCompress
 // Copyright ComponentAce, 2003
 //==============================================================================

{$I FXCVer.inc}

{$DEFINE FC_VERSION}
{DEFINE ZF_VERSION}

unit FXCReg;

interface

uses Classes,
 {$IFDEF D6H}
  DesignIntf, DesignEditors
 {$ELSE}
  DSGNINTF
 {$ENDIF} ;

type

  // file open dialog - for selecting archive file
  TFXCArchiveFileName = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end; // TFXCArchiveFileName

  // file open dialog - for selecting SFX stub
  TFXCSFXFileName = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end; // TFXCSFXFileName

  // folder open dialog - for selecting paths
  TFXCDirectory = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end; //TFXCDirectory


procedure Register; // register

implementation

uses
{$IFNDEF D16H}
Forms, Dialogs,
{$ELSE}
VCL.Forms, VCL.Dialogs,
{$ENDIF}
FlexCompress, FXCFolderDialog;

////////////////////////////////////////////////////////////////////////////////

//   TFXCArchiveFileName

////////////////////////////////////////////////////////////////////////////////

 //------------------------------------------------------------------------------
 // file name editor (extension is subtracted from name)
 //------------------------------------------------------------------------------
procedure TFXCArchiveFileName.Edit;
var
  td: TOpenDialog;
  s:  string;
begin
  td := TOpenDialog.Create(Application);
  td.Options := [ofFileMustExist];
{$IFDEF FC_VERSION}
  td.Filter := 'FlexCompress files (*.fxc)|*.FXC';
{$ENDIF}
{$IFDEF ZF_VERSION}
  td.Filter := 'ZIP files (*.zip)|*.ZIP';
{$ENDIF}
  if (td.Execute) then
  begin
    s := td.FileName;
    SetStrValue(s);
  end;
  td.Free;
end; // TFXCArchiveFileName.Edit


 //------------------------------------------------------------------------------
 // file name editor's attributes (paDialog - for ... button in design mode)
 //------------------------------------------------------------------------------
function TFXCArchiveFileName.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end; //TFXCArchiveFileName.GetAttributes


////////////////////////////////////////////////////////////////////////////////

//   TFXCSFXFileName

////////////////////////////////////////////////////////////////////////////////

 //------------------------------------------------------------------------------
 // file name editor (extension is subtracted from name)
 //------------------------------------------------------------------------------
procedure TFXCSFXFileName.Edit;
var
  td: TOpenDialog;
  s:  string;
begin
  td := TOpenDialog.Create(Application);
  td.Options := [ofFileMustExist];
  td.Filter := 'Executable files (*.exe)|*.EXE';
  if (td.Execute) then
  begin
    s := td.FileName;
    SetStrValue(s);
  end;
  td.Free;
end; // TFXCSFXFileName.Edit


 //------------------------------------------------------------------------------
 // file name editor's attributes (paDialog - for ... button in design mode)
 //------------------------------------------------------------------------------
function TFXCSFXFileName.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end; //TFXCSFXFileName.GetAttributes


////////////////////////////////////////////////////////////////////////////////

//   TFXCZIPDirectory

////////////////////////////////////////////////////////////////////////////////

 //------------------------------------------------------------------------------
 // path property editor
 //------------------------------------------------------------------------------
procedure TFXCDirectory.Edit;
var
  fb: TPBFolderDialog;
begin
  fb := TPBFolderDialog.Create(Application);
  fb.Flags := [ShowPath];
  // fb.BrowseFlags := [bfDirsOnly];
  fb.Folder := GetStrValue;
  if (fb.Execute) then
    SetStrValue(fb.Folder);
  fb.Free;
end; // TFXCDirectory.Edit


 //------------------------------------------------------------------------------
 // file name editor's attributes (paDialog - for ... button in design mode)
 //------------------------------------------------------------------------------
function TFXCDirectory.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end; //TFXCDirectory.GetAttributes


procedure Register;
begin
{$IFDEF FC_VERSION}
  RegisterComponents('ComponentAce Compression', [TFlexCompress]);
{$ENDIF}
{$IFDEF ZF_VERSION}
  RegisterComponents('ComponentAce Compression', [TZipForge]);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TBaseArchiver, 'FileName',
    TFXCArchiveFileName);
  RegisterPropertyEditor(TypeInfo(string), TBaseArchiver, 'SFXStub',
    TFXCSFXFileName);
  RegisterPropertyEditor(TypeInfo(string), TBaseArchiver, 'BaseDir',
    TFXCDirectory);
  RegisterPropertyEditor(TypeInfo(string), TBaseArchiver, 'TempDir',
    TFXCDirectory);
end;

end.
