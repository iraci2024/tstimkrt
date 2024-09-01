{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSpreadSheet }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetPasswordDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses,
  dxPasswordDialog, dxSpreadSheetCore,
  cxContainer, dxLayoutContainer, dxLayoutControl, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, cxButtons, cxTextEdit;

type
  TdxSpreadSheetPasswordDialogMode = TdxPasswordDialogFormMode;

const
  pdmNew = TdxPasswordDialogFormMode.pdmNew;
  pdmQuery = TdxPasswordDialogFormMode.pdmQuery;
  pdmConfirmation = TdxPasswordDialogFormMode.pdmConfirmation;

type
  { TdxSpreadSheetPasswordDialogForm }

  TdxSpreadSheetPasswordDialogFormClass = class of
    TdxSpreadSheetPasswordDialogForm;

  TdxSpreadSheetPasswordDialogForm = class(TdxPasswordDialogForm)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet;
      AMode: TdxSpreadSheetPasswordDialogMode); reintroduce; virtual;
    //
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  public
    // for internal use
    class function Execute(AOwner: TComponent;
      ASpreadSheet: TdxCustomSpreadSheet;
      AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string)
      : Boolean; overload;
    class function ExecuteConfirmation(AOwner: TComponent;
      ASpreadSheet: TdxCustomSpreadSheet; const APassword: string)
      : Boolean; overload;
  end;

var
  dxSpreadSheetPasswordDialogClass: TdxSpreadSheetPasswordDialogFormClass =
    TdxSpreadSheetPasswordDialogForm;

function ShowPasswordDialog(ASpreadSheet: TdxCustomSpreadSheet;
  AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string): Boolean;

implementation

uses
  dxCore;

{$R *.dfm}

function ShowPasswordDialog(ASpreadSheet: TdxCustomSpreadSheet;
  AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string): Boolean;
begin
  Result := dxSpreadSheetPasswordDialogClass.Execute
    (GetParentForm(ASpreadSheet), ASpreadSheet, AMode, APassword);
end;

{ TdxSpreadSheetPasswordDialogForm }

class function TdxSpreadSheetPasswordDialogForm.Execute(AOwner: TComponent;
  ASpreadSheet: TdxCustomSpreadSheet; AMode: TdxSpreadSheetPasswordDialogMode;
  out APassword: string): Boolean;
begin
  Result := Execute(AOwner, ASpreadSheet.DialogsLookAndFeel, AMode, APassword);
end;

class function TdxSpreadSheetPasswordDialogForm.ExecuteConfirmation
  (AOwner: TComponent; ASpreadSheet: TdxCustomSpreadSheet;
  const APassword: string): Boolean;
begin
  Result := ExecuteConfirmation(AOwner, ASpreadSheet.DialogsLookAndFeel,
    APassword);
end;

procedure TdxSpreadSheetPasswordDialogForm.Initialize
  (ASpreadSheet: TdxCustomSpreadSheet; AMode: TdxSpreadSheetPasswordDialogMode);
begin
  inherited Initialize(ASpreadSheet.DialogsLookAndFeel, AMode);
  FSpreadSheet := ASpreadSheet;
end;

end.
