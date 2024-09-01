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

unit dxRichEdit.Import.Rtf.DocumentImporter; // for internal use

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.Import.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.ImportExportHelper;

type
  TdxRtfDocumentImporter = class(TInterfacedObject,
    IdxImporter<TdxRichEditDocumentFormat, Boolean>, IdxFormatRatingCalculator)
  strict private
  const
    RtfSignature: array [0 .. 4] of byte = ($7B, $5C, $72, $74, $66);
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
  dxRichEdit.Import.Rtf,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Options;

{ TdxRtfDocumentImporter }

function TdxRtfDocumentImporter.Calculate(AStream: TStream): Integer;
var
  APreprocessedStream: TdxPreprocessedStream;
begin
  APreprocessedStream := TdxPreprocessedStream.Create(AStream);
  if APreprocessedStream.CheckSignature(RtfSignature) then
    Result := {$IFDEF DELPHIXE4}SmallInt.MaxValue{$ELSE}32767{$ENDIF}
  else
    Result := 0;
end;

function TdxRtfDocumentImporter.Filter: TdxFileDialogFilter;
begin
  Result := TdxFileDialogFilter.RtfFiles;
end;

function TdxRtfDocumentImporter.Format: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Rtf;
end;

function TdxRtfDocumentImporter.LoadDocument(const ADocumentModel
  : TdxCustomDocumentModel; AStream: TStream;
  const AOptions: IdxImporterOptions): Boolean;
var
  AModel: TdxDocumentModel absolute ADocumentModel;
begin
  AModel.InternalAPI.LoadDocumentRtfContent(AStream,
    TdxRtfDocumentImporterOptions(AOptions));
  Result := True;
end;

function TdxRtfDocumentImporter.SetupLoading: TObject;
begin
  Result := TdxRtfDocumentImporterOptions.Create;
end;

end.
