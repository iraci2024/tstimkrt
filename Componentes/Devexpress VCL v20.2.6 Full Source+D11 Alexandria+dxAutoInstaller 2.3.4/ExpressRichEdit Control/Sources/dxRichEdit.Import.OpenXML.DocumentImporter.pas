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

unit dxRichEdit.Import.OpenXML.DocumentImporter; // for internal use

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.Import.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.ImportExportHelper;

type
  TdxOpenXmlDocumentImporter = class(TInterfacedObject,
    IdxImporter<TdxRichEditDocumentFormat, Boolean>, IdxFormatRatingCalculator)
  public
    // IdxImporter
    function Filter: TdxFileDialogFilter;
    function Format: TdxRichEditDocumentFormat;
    function LoadDocument(const ADocumentModel: TdxCustomDocumentModel;
      AStream: TStream; const AOptions: IdxImporterOptions): Boolean;
    function SetupLoading: TObject;
    // IdxFormatRatingCalculator
    function Calculate(AStream: TStream): Integer;
  end;

implementation

uses
  StrUtils,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Options,
  dxZIPUtils;

{ TdxOpenXmlDocumentImporter }

function TdxOpenXmlDocumentImporter.Calculate(AStream: TStream): Integer;
var
  APreprocessedStream: TdxPreprocessedStream;
  AReader: TdxZIPStreamReader;
  AItem: TdxZIPFileEntry;
  I: Integer;
  AFileName: string;
begin
  Result := 0;
  APreprocessedStream := TdxPreprocessedStream.Create(AStream);
  if not APreprocessedStream.CheckSignature(TdxPreprocessedStream.ZipSignature)
  then
    Exit;
  APreprocessedStream.Reset;
  AReader := TdxZIPStreamReader.Create(AStream);
  try
    for I := 0 to AReader.Files.Count - 1 do
    begin
      AItem := AReader.Files[I];
      AFileName := string(AItem.Name);
      if ContainsText(AFileName, '.rels') or
        (ContainsText(AFileName, 'document.xml')) then
        Inc(Result);
    end;
  finally
    AReader.Free;
  end;
end;

function TdxOpenXmlDocumentImporter.Filter: TdxFileDialogFilter;
begin
  Result := TdxFileDialogFilter.OpenXMLFiles;
end;

function TdxOpenXmlDocumentImporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.OpenXML;
end;

function TdxOpenXmlDocumentImporter.LoadDocument(const ADocumentModel
  : TdxCustomDocumentModel; AStream: TStream;
  const AOptions: IdxImporterOptions): Boolean;
var
  AModel: TdxDocumentModel absolute ADocumentModel;
begin
  AModel.InternalAPI.LoadDocumentOpenXmlContent(AStream,
    TdxOpenXmlDocumentImporterOptions(AOptions));
  Result := True;
end;

function TdxOpenXmlDocumentImporter.SetupLoading: TObject;
begin
  Result := TdxOpenXmlDocumentImporterOptions.Create;
end;

end.
