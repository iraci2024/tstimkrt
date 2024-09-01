{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressPDFViewer }
{ }
{ Copyright (c) 2015-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL }
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

unit dxPDFDocumentProcessor;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, dxPDFBase, dxPDFTypes, dxPDFDocument;

type
  { TdxPDFDocumentProcessor }

  TdxPDFDocumentProcessor = class
  private
    FDocument: TdxPDFDocument;
    procedure FreeDocument;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CreateEmptyDocument;
    procedure LoadDocumentFromFile(const AFileName: string);
    procedure LoadDocumentFromStream(AStream: TStream);
    procedure SaveDocumentToFile(const AFileName: string;
      const AOptions: TdxPDFSaveOptions);
    procedure SaveDocumentToStream(AStream: TStream;
      const AOptions: TdxPDFSaveOptions);
  end;

implementation

type
  TdxPDFDocumentAccess = class(TdxPDFDocument);

  { TdxPDFDocumentProcessor }

constructor TdxPDFDocumentProcessor.Create;
begin
  inherited Create;
end;

destructor TdxPDFDocumentProcessor.Destroy;
begin
  FreeDocument;
  inherited Destroy;
end;

procedure TdxPDFDocumentProcessor.CreateEmptyDocument;
begin
  FreeDocument;
  FDocument := TdxPDFDocument.Create;
end;

procedure TdxPDFDocumentProcessor.LoadDocumentFromFile(const AFileName: string);
begin
  CreateEmptyDocument;
  FDocument.LoadFromFile(AFileName);
end;

procedure TdxPDFDocumentProcessor.LoadDocumentFromStream(AStream: TStream);
begin
  CreateEmptyDocument;
  FDocument.LoadFromStream(AStream);
end;

procedure TdxPDFDocumentProcessor.SaveDocumentToFile(const AFileName: string;
  const AOptions: TdxPDFSaveOptions);
begin
  if FDocument <> nil then
    FDocument.SaveToFile(AFileName);
end;

procedure TdxPDFDocumentProcessor.SaveDocumentToStream(AStream: TStream;
  const AOptions: TdxPDFSaveOptions);
begin
  if FDocument <> nil then
    FDocument.SaveToStream(AStream);
end;

procedure TdxPDFDocumentProcessor.FreeDocument;
begin
  FreeAndNil(FDocument);
end;

end.
