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

unit dxSkinsFluentDesignFormWizard;

{$I cxVer.inc}

interface

uses
  Classes, Windows, DesignIntf, ToolsApi, DesignEditors,
  dxCoreReg, dxDesignHelpers;

const
  sdxFluentDesignApplicationWizardComment =
    'Create a Fluent Design VCL application';
  sdxFluentDesignApplicationWizardName =
    'DevExpress VCL %version% Fluent Design Application';
  sdxFluentDesignFormWizardComment = 'Create a Fluent Design VCL form';
  sdxFluentDesignFormWizardName = 'DevExpress VCL %version% Fluent Design Form';

type
  { TdxOTACustomFluentDesignWizard }

  TdxOTACustomFluentDesignWizard = class(TdxOTACustomRepositoryWizard,
    IOTAFormWizard)
  strict private
    FPersonality: TdxOTAPersonality;
  protected
    function ExpandText(const AText: string): string;
    function GetGlyph: Cardinal; override;
    function GetIDString: string; override;
    function GetPersonality: TdxOTAPersonality; override;
  public
    constructor Create(APersonality: TdxOTAPersonality);
    //
    property Personality: TdxOTAPersonality read GetPersonality;
  end;

  { TdxOTAFluentDesignApplicationWizard }

  TdxOTAFluentDesignApplicationWizard = class(TdxOTACustomFluentDesignWizard,
    IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetCategory: TdxOTARepositoryCategory; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

  { TdxOTAFluentDesignApplicationCreator }

  TdxOTAFluentDesignApplicationCreator = class(TdxOTAApplicationCreator)
  protected
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
  end;

  { TdxOTARibbonFormWizard }

  TdxOTAFluentDesignFormWizard = class(TdxOTACustomFluentDesignWizard,
    IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

  { TdxOTAFluentDesignFormCreator }

  TdxOTAFluentDesignFormCreator = class(TdxOTAFormCreator)
  strict private
    FPersonality: TdxOTAPersonality;
  protected
    function LoadTemplate(const AName: string): string;
    procedure LoadTemplates(out AFormTemplate, AImplTemplate,
      AIntfTemplate: string);
  public
    constructor Create(APersonality: TdxOTAPersonality);
    //
    property Personality: TdxOTAPersonality read FPersonality;
  end;

implementation

uses
  SysUtils, dxCore;

{$R dxFluentDesignFormWizard.res}

function LoadDXWizardResourceTemplate(const AName: string): string;
var
  AList: TStringList;
  AStream: TResourceStream;
begin
  AList := TStringList.Create;
  try
    AStream := TResourceStream.Create(HInstance, AName, 'DXWIZARDTEMPLATES');
    try
      AList.LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
    Result := AList.Text;
  finally
    AList.Free;
  end;
end;

{ TdxOTACustomFluentDesignWizard }

constructor TdxOTACustomFluentDesignWizard.Create(APersonality
  : TdxOTAPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

function TdxOTACustomFluentDesignWizard.ExpandText(const AText: string): string;
begin
  Result := StringReplace(AText, '%version%', dxGetShortBuildNumberAsString,
    [rfReplaceAll]);
end;

function TdxOTACustomFluentDesignWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'DXFLUENTDESIGNFORMWIZARD');
end;

function TdxOTACustomFluentDesignWizard.GetIDString: string;
const
  PersonalitiesMap: array [TdxOTAPersonality] of string = ('Delphi',
    'CBuilder');
begin
  Result := inherited GetIDString + '.' + PersonalitiesMap[Personality];
end;

function TdxOTACustomFluentDesignWizard.GetPersonality: TdxOTAPersonality;
begin
  Result := FPersonality;
end;

function TdxOTAFluentDesignApplicationWizard.GetCategory
  : TdxOTARepositoryCategory;
begin
  Result := dxcNewProject;
end;

{ TdxOTAFluentDesignApplicationWizard }

procedure TdxOTAFluentDesignApplicationWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
  begin
    AModuleServices.CreateModule(TdxOTAFluentDesignApplicationCreator.Create
      (Personality));
    AModuleServices.CreateModule(TdxOTAFluentDesignFormCreator.Create
      (Personality));
  end;
end;

function TdxOTAFluentDesignApplicationWizard.GetComment: string;
begin
  Result := ExpandText(sdxFluentDesignApplicationWizardComment);
end;

function TdxOTAFluentDesignApplicationWizard.GetName: string;
begin
  Result := ExpandText(sdxFluentDesignApplicationWizardName);
end;

{ TdxOTAFluentDesignApplicationCreator }

function TdxOTAFluentDesignApplicationCreator.NewProjectSource(const ProjectName
  : string): IOTAFile;
{$IFNDEF DELPHI10SEATTLE}
var
  ATemplate: string;
{$ENDIF}
begin
{$IFDEF DELPHI10SEATTLE}
  Result := inherited NewProjectSource(ProjectName);
{$ELSE}
  if GetPersonality = dxopCBuilder then
    ATemplate := LoadDXWizardResourceTemplate('FLUENTDESIGNCBUILDERAPP')
  else
    ATemplate := StringReplace
      (LoadDXWizardResourceTemplate('FLUENTDESIGNDELPHIAPP'), '%ProjectIdent%',
      ProjectName, []);
  Result := TOTAFile.Create(ATemplate);
{$ENDIF}
end;

{ TdxOTAFluentDesignFormWizard }

procedure TdxOTAFluentDesignFormWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
    AModuleServices.CreateModule(TdxOTAFluentDesignFormCreator.Create
      (Personality));
end;

function TdxOTAFluentDesignFormWizard.GetComment: string;
begin
  Result := ExpandText(sdxFluentDesignFormWizardComment);
end;

function TdxOTAFluentDesignFormWizard.GetName: string;
begin
  Result := ExpandText(sdxFluentDesignFormWizardName);
end;

{ TdxOTAFluentDesignFormCreator }

constructor TdxOTAFluentDesignFormCreator.Create(APersonality
  : TdxOTAPersonality);
var
  AFormTemplate, AImplTemplate, AIntfTemplate: string;
begin
  FPersonality := APersonality;
  LoadTemplates(AFormTemplate, AImplTemplate, AIntfTemplate);
  inherited Create(AFormTemplate, AImplTemplate, AIntfTemplate);
end;

function TdxOTAFluentDesignFormCreator.LoadTemplate
  (const AName: string): string;
begin
  Result := LoadDXWizardResourceTemplate(AName);
end;

procedure TdxOTAFluentDesignFormCreator.LoadTemplates(out AFormTemplate,
  AImplTemplate, AIntfTemplate: string);
begin
  AFormTemplate := LoadTemplate('FLUENTDESIGNFORM');
  if FPersonality = dxopCBuilder then
  begin
    AIntfTemplate := LoadTemplate('FLUENTDESIGNCBUILDERHEADER');
    AImplTemplate := LoadTemplate('FLUENTDESIGNCBUILDERUNIT');
  end
  else
  begin
    AImplTemplate := LoadTemplate('FLUENTDESIGNDELPHIUNIT');
    AIntfTemplate := '';
  end;
end;

end.
