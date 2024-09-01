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

unit dxRichEdit.Control.Reg;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

procedure Register;

implementation

uses
  SysUtils, VCLEditors, DesignIntf, DesignEditors, DesignMenus, Classes,
  TypInfo, Menus,
  StrUtils, Generics.Collections,
  ToolsAPI,
  dxCore, dxCoreClasses, dxCoreReg, dxBuiltInPopupMenu, cxClasses,
  cxLibraryReg, cxPC, cxEditPropEditors, dxUIGeneratorDesignHelpers,
  dxRichEdit.Utils.Types,
  dxEncoding,
  dxSymbolListBox,
  dxContainerListBox,
  dxRichEditFontNameComboBox,
  dxRichEditDialogsSimpleControl,
  dxRichEditBorderLineWeightComboBox,
  dxRichEdit.View.Core,
  dxRichEdit.NativeApi,
  dxRichEdit.Options,
  dxRichEdit.Control,
  dxRichEdit.DocumentServer;

const
  dxRichEditControlProductName = 'ExpressRichEditControl Suite';
  dxPackageSuffix =
{$IFDEF VER210}14{$ENDIF}
{$IFDEF VER220}15{$ENDIF}
{$IFDEF VER230}16{$ENDIF}
{$IFDEF VER240}17{$ENDIF}
{$IFDEF VER250}18{$ENDIF}
{$IFDEF VER260}19{$ENDIF}
{$IFDEF VER270}20{$ENDIF}
{$IFDEF VER280}21{$ENDIF}
{$IFDEF VER290}22{$ENDIF}
{$IFDEF VER300}23{$ENDIF}
{$IFDEF VER310}24{$ENDIF}
{$IFDEF VER320}25{$ENDIF}
{$IFDEF VER330}26{$ENDIF}
{$IFDEF VER340}27{$ENDIF}
{$IFDEF VER350}28{$ENDIF}
    ;

type

  TdxRichEditControlOptionsBaseAccess = class(TdxRichEditControlOptionsBase);

  { TdxRichFormNotifier }

  TdxRichFormNotifier = class(TInterfacedObject, IOTANotifier,
    IOTAModuleNotifier)
  public const
    Packages: array [TdxRichEditDocumentFormat] of string = ('', '', '',
      'dxRichEditControlHtmlFormatRS', 'dxRichEditControlOpenXMLFormatRS',
      'dxRichEditControlDocFormatRS');
  private
    class var FRichFormNotifiers: TList<TdxRichFormNotifier>;
    class procedure UpdateProjectFiles(const AProject: IOTAProject;
      ARichEditDocumentFormats: TdxRichEditDocumentFormats); static;
  protected
    FId: Integer;
    FModule: IOTAModule;
    FFormats: TdxRichEditDocumentFormats;
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAModuleNotifier
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
  protected
    class constructor Create;
    class destructor Destroy;

    procedure UnRegisterNotifier;
    class function FindNotifier(const AModule: IOTAModule): TdxRichFormNotifier;
  public
    constructor Create;

    class procedure RegisterFormat(AFormat: TdxRichEditDocumentFormat);
    class procedure UnRegisterFormat(AFormat: TdxRichEditDocumentFormat);
  end;

  { TdxCustomRichEditControlComponentEditor }

  TdxCustomRichEditControlComponentEditor = class(TdxUIGeneratorComponentEditor)
  protected
    function GetProductName: string; override;
    function InternalGetVerbCount: Integer; override;
    procedure UpdateFormats(AFormat: TdxRichEditDocumentFormat;
      var AFormats: TdxRichEditDocumentFormats);
  public
    function GetVerb(Index: Integer): string; override;
  end;

  { TdxRichEditControlComponentEditor }

  TdxRichEditControlComponentEditor = class
    (TdxCustomRichEditControlComponentEditor)
  protected
    procedure AddUnitClick(Sender: TObject);
  public
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  { TdxRichEditDocumentServerComponentEditor }

  TdxRichEditDocumentServerComponentEditor = class
    (TdxCustomRichEditControlComponentEditor)
  protected
    procedure AddUnitClick(Sender: TObject);
  public
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  { TdxRichEditSelectionEditor }

  TdxRichEditSelectionEditor = class abstract(TSelectionEditor)
  public const
    UsesFormats: array [TdxRichEditDocumentFormat] of string = ('',
      'dxRichEdit.PlainText', 'dxRichEdit.Rtf', 'dxRichEdit.Html',
      'dxRichEdit.OpenXML', 'dxRichEdit.Doc');
  protected
    FComponentsList: TStringList;
    procedure AddComponent(const Name: string);
  end;

  { TdxRichEditControlSelectionEditor }

  TdxRichEditControlSelectionEditor = class(TdxRichEditSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxRichEditDocumentServerSelectionEditor }

  TdxRichEditDocumentServerSelectionEditor = class(TdxRichEditSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxOptionsEncodingPropertyEditor }

  TdxOptionsEncodingPropertyEditor = class(TIntegerProperty)
  strict private
    FDisplayNames: TStringList;
    function CreateDisplayNames: TStringList;
  strict protected
    property DisplayNames: TStringList read FDisplayNames;
  public
    constructor Create(const ADesigner: IDesigner;
      APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEncoding: Word; virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxImportOptionsEncodingPropertyEditor }

  TdxImportOptionsEncodingPropertyEditor = class
    (TdxOptionsEncodingPropertyEditor)
  strict private
    function GetOptions: TdxDocumentImporterOptions;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    property Options: TdxDocumentImporterOptions read GetOptions;
  end;

  { TdxExportOptionsEncodingPropertyEditor }

  TdxExportOptionsEncodingPropertyEditor = class
    (TdxOptionsEncodingPropertyEditor)
  strict private
    function GetOptions: TdxDocumentExporterOptions;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    property Options: TdxDocumentExporterOptions read GetOptions;
  end;

  { TdxRichFormNotifier }

constructor TdxRichFormNotifier.Create;
begin
  inherited Create;
  FId := -1;
end;

class constructor TdxRichFormNotifier.Create;
begin
  FRichFormNotifiers := TList<TdxRichFormNotifier>.Create;
end;

class destructor TdxRichFormNotifier.Destroy;
begin
  FreeAndNil(FRichFormNotifiers);
end;

procedure TdxRichFormNotifier.AfterSave;
var
  I: Integer;
begin
  if FFormats <> [] then
    for I := 0 to FModule.OwnerCount - 1 do
      UpdateProjectFiles(FModule.Owners[I], FFormats);
  UnRegisterNotifier;
end;

procedure TdxRichFormNotifier.BeforeSave;
begin

end;

function TdxRichFormNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TdxRichFormNotifier.Destroyed;
begin
  UnRegisterNotifier;
end;

class function TdxRichFormNotifier.FindNotifier(const AModule: IOTAModule)
  : TdxRichFormNotifier;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FRichFormNotifiers.Count - 1 do
    if FRichFormNotifiers[I].FModule = AModule then
      Exit(FRichFormNotifiers[I]);
end;

procedure TdxRichFormNotifier.Modified;
begin

end;

procedure TdxRichFormNotifier.ModuleRenamed(const NewName: string);
begin

end;

class procedure TdxRichFormNotifier.RegisterFormat
  (AFormat: TdxRichEditDocumentFormat);
var
  AModule: IOTAModule;
  ARichFormNotifier: TdxRichFormNotifier;
begin
  AModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  ARichFormNotifier := FindNotifier(AModule);
  if (ARichFormNotifier = nil) then
  begin
    ARichFormNotifier := TdxRichFormNotifier.Create;
    ARichFormNotifier.FId := AModule.AddNotifier(ARichFormNotifier);
    ARichFormNotifier.FModule := AModule;
    FRichFormNotifiers.Add(ARichFormNotifier);
  end;
  Include(ARichFormNotifier.FFormats, AFormat);
end;

class procedure TdxRichFormNotifier.UnRegisterFormat
  (AFormat: TdxRichEditDocumentFormat);
var
  AModule: IOTAModule;
  ARichFormNotifier: TdxRichFormNotifier;
begin
  AModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  ARichFormNotifier := FindNotifier(AModule);
  if ARichFormNotifier = nil then
    Exit;
  Exclude(ARichFormNotifier.FFormats, AFormat);
end;

procedure TdxRichFormNotifier.UnRegisterNotifier;
var
  I: Integer;
begin
  FModule.RemoveNotifier(FId);
  FModule := nil;
  FId := -1;
  for I := 0 to FRichFormNotifiers.Count - 1 do
    if FRichFormNotifiers[I] = Self then
    begin
      FRichFormNotifiers.Remove(Self);
      Exit;
    end;
end;

class procedure TdxRichFormNotifier.UpdateProjectFiles(const AProject
  : IOTAProject; ARichEditDocumentFormats: TdxRichEditDocumentFormats);

  procedure AddPackageName(const APropertyName: string;
    const AConfig: IOTABuildConfiguration; const AExtName: string = '');
  var
    AAPropertyValue: string;
    ARichEditDocumentFormat: TdxRichEditDocumentFormat;
    ASB: TStringBuilder;
    APackageName: string;
  begin
    if AConfig.PropertyExists(APropertyName) then
    begin
      AAPropertyValue := AConfig.Value[APropertyName];
      ASB := TStringBuilder.Create;
      try
        for ARichEditDocumentFormat in ARichEditDocumentFormats do
        begin
          APackageName := Packages[ARichEditDocumentFormat];
          if (APackageName <> '') and not ContainsText(AAPropertyValue,
            APackageName) then
            ASB.Append(';').Append(APackageName).Append(dxPackageSuffix)
              .Append(AExtName);
        end;
        AConfig.Value[APropertyName] := AAPropertyValue + ASB.ToString;
      finally
        ASB.Free;
      end;
    end;
  end;

  procedure ProcessConfig(const AConfig: IOTABuildConfiguration);
  const
    ConfigKey = {$IFDEF DELPHIXE2}'Base_Win'{$ELSE}'Base'{$ENDIF};
  var
    I: Integer;
    APlatformName: string;
  begin
    if ContainsText(AProject.Personality, 'CPlusPlusBuilder') and
      ContainsText(AConfig.Key, 'Base') then
      AddPackageName('AllPackageLibs', AConfig, '.lib');
    if ContainsText(AConfig.Key, ConfigKey) then
    begin
      if ContainsText(AProject.Personality, 'Delphi') then
        AddPackageName('DCC_UsePackage', AConfig)
      else
        AddPackageName('PackageImports', AConfig);
      Exit;
    end
{$IFDEF DELPHIXE2}
    else
    begin
      for I := 0 to AConfig.ChildCount - 1 do
        ProcessConfig(AConfig.Children[I]);
      for APlatformName in AConfig.GetPlatforms do
        ProcessConfig(AConfig.GetPlatformConfiguration(APlatformName));
    end;
{$ENDIF}
  end;

var
  AProjectConfig: IOTAProjectOptionsConfigurations;
{$IFNDEF DELPHIXE2}
  I: Integer;
{$ENDIF}
begin
  if ARichEditDocumentFormats = [] then
    Exit;
  if Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations,
    AProjectConfig) then
  begin
{$IFDEF DELPHIXE2}
    ProcessConfig(AProjectConfig.BaseConfiguration);
{$ELSE}
    for I := 0 to AProjectConfig.ConfigurationCount - 1 do
      ProcessConfig(AProjectConfig.Configurations[I]);
{$ENDIF}
  end;
end;

{ TdxCustomRichEditControlComponentEditor }

function TdxCustomRichEditControlComponentEditor.GetVerb
  (Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Add Unit to Support'
  else
    Result := inherited GetVerb(Index);
end;

function TdxCustomRichEditControlComponentEditor.GetProductName: string;
begin
  Result := dxRichEditControlProductName;
end;

function TdxCustomRichEditControlComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxCustomRichEditControlComponentEditor.UpdateFormats
  (AFormat: TdxRichEditDocumentFormat;
  var AFormats: TdxRichEditDocumentFormats);
begin
  if AFormat in AFormats then
  begin
    Exclude(AFormats, AFormat);
    TdxRichFormNotifier.UnRegisterFormat(AFormat);
  end
  else
  begin
    Include(AFormats, AFormat);
    TdxRichFormNotifier.RegisterFormat(AFormat);
  end;
end;

{ TdxRichEditControlComponentEditor }

procedure TdxRichEditControlComponentEditor.AddUnitClick(Sender: TObject);
var
  AMenuItem: TMenuItem absolute Sender;
  AFormat: TdxRichEditDocumentFormat;
  AFormats: TdxRichEditDocumentFormats;
begin
  AFormat := TdxRichEditDocumentFormat(AMenuItem.Tag);
  AFormats := TdxRichEditControlOptionsBaseAccess
    ((Component as TdxCustomRichEditControl).Options).RichEditDocumentFormats;
  UpdateFormats(AFormat, AFormats);
  TdxRichEditControlOptionsBaseAccess((Component as TdxCustomRichEditControl)
    .Options).RichEditDocumentFormats := AFormats;
  Designer.Modified;
end;

procedure TdxRichEditControlComponentEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);

  procedure AddFormatItem(const AFormatName: string;
    AFormat: TdxRichEditDocumentFormat);
  var
    ASubItem: IMenuItem;
    AFormats: TdxRichEditDocumentFormats;
  begin
    AFormats := TdxRichEditControlOptionsBaseAccess
      ((Component as TdxCustomRichEditControl).Options).RichEditDocumentFormats;
    ASubItem := AItem.AddItem(AFormatName, 0, AFormat in AFormats, True,
      AddUnitClick);
    ASubItem.Tag := Ord(AFormat);
  end;

begin
  if Index = 0 then
  begin
    AddFormatItem('Office Open XML (DOCX)', TdxRichEditDocumentFormat.OpenXml);
    AddFormatItem('Word Binary File (DOC)', TdxRichEditDocumentFormat.Doc);
    AddFormatItem('HyperText Markup Language (HTML)',
      TdxRichEditDocumentFormat.Html);
  end
  else
    inherited;
end;

{ TdxRichEditDocumentServerComponentEditor }

procedure TdxRichEditDocumentServerComponentEditor.AddUnitClick
  (Sender: TObject);
var
  AMenuItem: TMenuItem absolute Sender;
  AFormat: TdxRichEditDocumentFormat;
  AFormats: TdxRichEditDocumentFormats;
begin
  AFormat := TdxRichEditDocumentFormat(AMenuItem.Tag);
  AFormats := TdxRichEditControlOptionsBaseAccess
    ((Component as TdxRichEditCustomDocumentServer).Options)
    .RichEditDocumentFormats;
  UpdateFormats(AFormat, AFormats);
  TdxRichEditControlOptionsBaseAccess
    ((Component as TdxRichEditCustomDocumentServer).Options)
    .RichEditDocumentFormats := AFormats;
  Designer.Modified;
end;

procedure TdxRichEditDocumentServerComponentEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);

  procedure AddFormatItem(const AFormatName: string;
    AFormat: TdxRichEditDocumentFormat);
  var
    ASubItem: IMenuItem;
    AFormats: TdxRichEditDocumentFormats;
  begin
    AFormats := TdxRichEditControlOptionsBaseAccess
      ((Component as TdxRichEditCustomDocumentServer).Options)
      .RichEditDocumentFormats;
    ASubItem := AItem.AddItem(AFormatName, 0, AFormat in AFormats, True,
      AddUnitClick);
    ASubItem.Tag := Ord(AFormat);
  end;

begin
  if Index = 0 then
  begin
    AddFormatItem('Rich Text Format (RTF)', TdxRichEditDocumentFormat.Rtf);
    AddFormatItem('Office Open XML (DOCX)', TdxRichEditDocumentFormat.OpenXml);
    AddFormatItem('Word Binary File (DOC)', TdxRichEditDocumentFormat.Doc);
    AddFormatItem('HyperText Markup Language (HTML)',
      TdxRichEditDocumentFormat.Html);
  end
  else
    inherited;
end;

{ TdxRichEditSelectionEditor }

procedure TdxRichEditSelectionEditor.AddComponent(const Name: string);
begin
  FComponentsList.Add(Name);
end;

{ TdxRichEditControlSelectionEditor }

procedure TdxRichEditControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  ARichEditControl: TdxRichEditControl;
  AUsesFormat: TdxRichEditDocumentFormat;
  AUsesFormats: TdxRichEditDocumentFormats;
  I: Integer;
begin
  inherited RequiresUnits(Proc);
  Proc('dxCore');
  Proc('dxCoreClasses');
  Proc('cxGraphics');
  Proc('dxGDIPlusAPI');
  Proc('dxGDIPlusClasses');
  Proc('dxRichEdit.NativeApi');
  Proc('dxRichEdit.Types');
  Proc('dxRichEdit.Options');
  Proc('dxRichEdit.Control');
  Proc('dxRichEdit.Control.SpellChecker');
  Proc('dxRichEdit.Dialogs.EventArgs');
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    Proc(cxGetUnitName(TdxBuiltInPopupMenuAdapterManager.
      GetActualAdapterClass));

  FComponentsList := TStringList.Create;
  try
    AUsesFormats := [];
    Designer.GetComponentNames(GetTypeData(TypeInfo(TdxRichEditControl)),
      AddComponent);
    for I := 0 to FComponentsList.Count - 1 do
    begin
      ARichEditControl := Designer.GetComponent(FComponentsList[I])
        as TdxRichEditControl;
      AUsesFormats := AUsesFormats + TdxRichEditControlOptionsBaseAccess
        (ARichEditControl.Options).RichEditDocumentFormats;
      TdxRichEditControlOptionsBaseAccess(ARichEditControl.Options)
        .RichEditDocumentFormats := [];
    end;
  finally
    FComponentsList.Free;
  end;

  for AUsesFormat in AUsesFormats do
    Proc(UsesFormats[AUsesFormat]);
end;

{ TdxRichEditDocumentServerSelectionEditor }

procedure TdxRichEditDocumentServerSelectionEditor.RequiresUnits
  (Proc: TGetStrProc);
var
  ADocumentServer: TdxRichEditDocumentServer;
  AUsesFormat: TdxRichEditDocumentFormat;
  AUsesFormats: TdxRichEditDocumentFormats;
  I: Integer;
begin
  inherited RequiresUnits(Proc);
  Proc('dxCore');
  Proc('dxCoreClasses');
  Proc('dxRichEdit.NativeApi');
  Proc('dxRichEdit.Types');
  Proc('dxRichEdit.PlainText');
  FComponentsList := TStringList.Create;
  try
    AUsesFormats := [];
    Designer.GetComponentNames(GetTypeData(TypeInfo(TdxRichEditDocumentServer)),
      AddComponent);
    for I := 0 to FComponentsList.Count - 1 do
    begin
      ADocumentServer := Designer.GetComponent(FComponentsList[I])
        as TdxRichEditDocumentServer;
      AUsesFormats := AUsesFormats + TdxRichEditControlOptionsBaseAccess
        (ADocumentServer.Options).RichEditDocumentFormats;
      TdxRichEditControlOptionsBaseAccess(ADocumentServer.Options)
        .RichEditDocumentFormats := [];
    end;
  finally
    FComponentsList.Free;
  end;

  for AUsesFormat in AUsesFormats do
    Proc(UsesFormats[AUsesFormat]);
end;

{ TdxOptionsEncodingPropertyEditor }

constructor TdxOptionsEncodingPropertyEditor.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  FDisplayNames := CreateDisplayNames;
end;

function TdxOptionsEncodingPropertyEditor.CreateDisplayNames: TStringList;
var
  AEncodings: TArray<TEncoding>;
  AEncoding: TEncoding;
begin
  Result := TStringList.Create;
  AEncodings := TdxEncoding.Encodings;
  for AEncoding in AEncodings do
    Result.AddObject(AEncoding.DisplayName, AEncoding);
  Result.Sort;
end;

destructor TdxOptionsEncodingPropertyEditor.Destroy;
begin
  FDisplayNames.Free;
  inherited Destroy;
end;

function TdxOptionsEncodingPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TdxOptionsEncodingPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FDisplayNames.Count - 1 do
    Proc(FDisplayNames[I]);
end;

{ TdxImportOptionsEncodingPropertyEditor }

function TdxImportOptionsEncodingPropertyEditor.GetOptions
  : TdxDocumentImporterOptions;
begin
  Result := Safe<TdxDocumentImporterOptions>.Cast(GetComponent(0));
end;

function TdxImportOptionsEncodingPropertyEditor.GetValue: string;
begin
  if Options <> nil then
  begin
    Result := Options.ActualEncoding.DisplayName;
    if Options.IsDefaultEncoding then
      Result := Format('%s - Default', [Result]);
  end
  else
    Result := '';
end;

procedure TdxImportOptionsEncodingPropertyEditor.SetValue(const Value: string);
var
  AIndex: Integer;
begin
  if Options <> nil then
  begin
    AIndex := DisplayNames.IndexOf(Value);
    if (AIndex >= 0) and
      (Options.ActualEncoding <> TEncoding(DisplayNames.Objects[AIndex])) then
    begin
      Options.ActualEncoding := TEncoding(DisplayNames.Objects[AIndex]);
      Modified;
    end;
  end;
end;

{ TdxExportOptionsEncodingPropertyEditor }

function TdxExportOptionsEncodingPropertyEditor.GetOptions
  : TdxDocumentExporterOptions;
begin
  Result := Safe<TdxDocumentExporterOptions>.Cast(GetComponent(0));
end;

function TdxExportOptionsEncodingPropertyEditor.GetValue: string;
begin
  if Options <> nil then
  begin
    Result := Options.ActualEncoding.DisplayName;
    if Options.IsDefaultEncoding then
      Result := Format('%s - Default', [Result]);
  end
  else
    Result := '';
end;

procedure TdxExportOptionsEncodingPropertyEditor.SetValue(const Value: string);
var
  AIndex: Integer;
begin
  if Options <> nil then
  begin
    AIndex := DisplayNames.IndexOf(Value);
    if (AIndex >= 0) and
      (Options.ActualEncoding <> TEncoding(DisplayNames.Objects[AIndex])) then
    begin
      Options.ActualEncoding := TEncoding(DisplayNames.Objects[AIndex]);
      Modified;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents(dxCoreLibraryProductPage,
    [TdxRichEditControl, TdxRichEditDocumentServer]);
  RegisterComponentEditor(TdxRichEditControl,
    TdxRichEditControlComponentEditor);
  RegisterComponentEditor(TdxRichEditDocumentServer,
    TdxRichEditDocumentServerComponentEditor);
  RegisterPropertyEditor(TypeInfo(TShortCut), TdxHyperlinkOptions,
    'ModifierKeys', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(Word), TdxDocumentImporterOptions, 'Encoding',
    TdxImportOptionsEncodingPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Word), TdxDocumentExporterOptions, 'Encoding',
    TdxExportOptionsEncodingPropertyEditor);

  RegisterNoIcon([TdxSymbolListBox, TdxSimpleSymbolListBox, TdxContainerListBox,
    TdxSimpleRichEditControl, TdxRichEditFontNameComboBox,
    TdxBorderLineWeightComboBox]);
  RegisterSelectionEditor(TdxRichEditControl,
    TdxRichEditControlSelectionEditor);
  RegisterSelectionEditor(TdxRichEditDocumentServer,
    TdxRichEditDocumentServerSelectionEditor);
end;

end.
