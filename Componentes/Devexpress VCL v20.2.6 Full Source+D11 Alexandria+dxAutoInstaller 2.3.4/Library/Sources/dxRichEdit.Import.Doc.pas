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

unit dxRichEdit.Import.Doc; // for internal use

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.NativeApi,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.Formats,
  dxRichEdit.DocumentModel.Core;

type
  { TdxImportDocFormat }

  TdxImportDocFormat = class(TdxImportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter; override;
    function GetDocumentImporter
      : IdxImporter<TdxRichEditDocumentFormat, Boolean>; override;
  end;

  { TdxDocDocumentImporter }

  TdxDocDocumentImporter = class(TInterfacedObject,
    IdxImporter<TdxRichEditDocumentFormat, Boolean>, IdxFormatRatingCalculator)
  strict private
  const
    DocSignature: array [0 .. 7] of byte = ($D0, $CF, $11, $E0, $A1, $B1,
      $1A, $E1);
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
  dxRichEdit.Options,
  dxRichEdit.Import.Doc.DocImporter,
  dxRichEdit.DocumentModel.PieceTable;

{ TdxImportDocFormat }

class function TdxImportDocFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Doc;
end;

function TdxImportDocFormat.GetDocumentImporter
  : IdxImporter<TdxRichEditDocumentFormat, Boolean>;
begin
  Result := TdxDocDocumentImporter.Create;
end;

function TdxImportDocFormat.GetImporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
begin
  Result := TdxDocImporter.Create(TdxDocumentModel(ADocumentModel),
    TdxDocDocumentImporterOptions(AOptions));
end;

{ TdxDocDocumentImporter }

function TdxDocDocumentImporter.Calculate(AStream: TStream): Integer;
var
  APreprocessedStream: TdxPreprocessedStream;
begin
  APreprocessedStream := TdxPreprocessedStream.Create(AStream);
  if APreprocessedStream.CheckSignature(DocSignature) then
    Result := {$IFDEF DELPHIXE4}SmallInt.MaxValue{$ELSE}32767{$ENDIF}
  else
    Result := 0;
end;

function TdxDocDocumentImporter.Filter: TdxFileDialogFilter;
begin
  Result := TdxFileDialogFilter.DocFiles;
end;

function TdxDocDocumentImporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Doc;
end;

function TdxDocDocumentImporter.LoadDocument(const ADocumentModel
  : TdxCustomDocumentModel; AStream: TStream;
  const AOptions: IdxImporterOptions): Boolean;
var
  AModel: TdxDocumentModel absolute ADocumentModel;
begin
  AModel.InternalAPI.LoadDocumentDocContent(AStream,
    TdxDocDocumentImporterOptions(AOptions));
  Result := True;
end;

function TdxDocDocumentImporter.SetupLoading: TObject;
begin
  Result := TdxDocDocumentImporterOptions.Create;
end;

end.
