{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEditors }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL }
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

unit dxShellDialogs;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, SysUtils, Forms, Dialogs, Controls,
  dxCore, cxClasses, cxLookAndFeels,
  dxShellCustomDialog;

type
  TdxCommonFileDialog = class(TOpenDialog, IdxSkinSupport)
  strict private
    FDialogForm: TdxfrmCommonFileCustomDialog;
    FLookAndFeel: TcxLookAndFeel;
    procedure InternalCanClose(Sender: TObject; var CanClose: Boolean);
    procedure InternalFolderChange(Sender: TObject);
    procedure InternalIncludeItem(const OFN: TOFNotifyEx; var Include: Boolean);
    procedure InternalSelectionChange(Sender: TObject);
    procedure InternalTypeChange(Sender: TObject);
    procedure SetLookAndFeel(const Value: TcxLookAndFeel);
  protected
    function CreateForm(AParentWnd: HWND): TdxfrmCommonFileCustomDialog;
      virtual; abstract;

    property DialogForm: TdxfrmCommonFileCustomDialog read FDialogForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Execute(ParentWnd: HWND): Boolean; override;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
  end;

  TdxOpenFileDialog = class(TdxCommonFileDialog)
  protected
    function CreateForm(AParentWnd: HWND)
      : TdxfrmCommonFileCustomDialog; override;
  published
    property DefaultExt;
    property FileName;
    property Filter;
    property FilterIndex;
    property HelpContext;
    property InitialDir;
    property LookAndFeel;
    property Options;
    property Title;
    property OnCanClose;
    property OnClose;
    property OnFolderChange;
    property OnSelectionChange;
    property OnShow;
    property OnTypeChange;
    property OnIncludeItem;
  end;

  TdxSaveFileDialog = class(TdxOpenFileDialog)
  protected
    function CreateForm(AParentWnd: HWND)
      : TdxfrmCommonFileCustomDialog; override;
  published
    property DefaultExt;
    property FileName;
    property Filter;
    property FilterIndex;
    property HelpContext;
    property InitialDir;
    property LookAndFeel;
    property Options;
    property Title;
    property OnCanClose;
    property OnClose;
    property OnFolderChange;
    property OnSelectionChange;
    property OnShow;
    property OnTypeChange;
    property OnIncludeItem;
  end;

var
  OpenFileDialogFormClass: TdxfrmCommonFileCustomDialogClass =
    TdxfrmOpenFileDialog;
  SaveFileDialogFormClass: TdxfrmCommonFileCustomDialogClass =
    TdxfrmSaveFileDialog;

implementation

function ApplicationMainHandle: HWND;
begin
  if Application.MainFormOnTaskBar and (Application.MainForm <> nil) then
    Result := Application.MainFormHandle
  else
    Result := Application.Handle;
end;

{ TdxCommonFileDialog }

constructor TdxCommonFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
end;

destructor TdxCommonFileDialog.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TdxCommonFileDialog.Assign(Source: TPersistent);
begin
  if Source is TdxCommonFileDialog then
    LookAndFeel := TdxCommonFileDialog(Source).LookAndFeel;
  inherited Assign(Source);
end;

function TdxCommonFileDialog.Execute(ParentWnd: HWND): Boolean;
var
  AStandardDialog: TOpenDialog;
begin
  if IsWinVista then
  begin
    if Self is TdxSaveFileDialog then
      AStandardDialog := TOpenDialog.Create(Owner)
    else
      AStandardDialog := TSaveDialog.Create(Owner);
    try
      AStandardDialog.Assign(Self);
      Result := AStandardDialog.Execute();
    finally
      AStandardDialog.Free;
    end;
    Exit;
  end;
  FDialogForm := CreateForm(ParentWnd);
  try
    if not(ofEnableSizing in Options) then
      FDialogForm.BorderStyle := bsSingle;
    FDialogForm.LookAndFeel.MasterLookAndFeel := LookAndFeel;
    FDialogForm.InitializeFilter(Filter, FilterIndex);
    FDialogForm.DefaultExt := DefaultExt;
    FDialogForm.SetHistoryList(HistoryList);
    FDialogForm.ApplyLocalization;
    if Title <> '' then
      FDialogForm.Title := Title;
    FDialogForm.OnFileOkClick := InternalCanClose;
    FDialogForm.OnFolderChange := InternalFolderChange;
    FDialogForm.OnIncludeItem := InternalIncludeItem;
    FDialogForm.OnSelectionChange := InternalSelectionChange;
    FDialogForm.OnTypeChange := InternalTypeChange;
    FDialogForm.Options := Options;
    FDialogForm.InitializeFolder(InitialDir);
    FDialogForm.FileName := FileName;

    DoShow;
    try
      Result := FDialogForm.ShowModal = mrOk;
      if Result then
      begin
        FileName := FDialogForm.FileName;
        Files.Assign(FDialogForm.Files);
        HistoryList.AddStrings(FDialogForm.Files);
        FilterIndex := FDialogForm.FilterIndex;
      end;
    finally
      DoClose;
    end;
  finally
    FreeAndNil(FDialogForm);
  end;
end;

procedure TdxCommonFileDialog.InternalCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := DoCanClose;
end;

procedure TdxCommonFileDialog.InternalFolderChange(Sender: TObject);
begin
  DoFolderChange;
end;

procedure TdxCommonFileDialog.InternalIncludeItem(const OFN: TOFNotifyEx;
  var Include: Boolean);
begin
  DoIncludeItem(OFN, Include);
end;

procedure TdxCommonFileDialog.InternalSelectionChange(Sender: TObject);
begin
  DoSelectionChange;
end;

procedure TdxCommonFileDialog.InternalTypeChange(Sender: TObject);
begin
  DoTypeChange;
end;

procedure TdxCommonFileDialog.SetLookAndFeel(const Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

{ TdxOpenFileDialog }

function TdxOpenFileDialog.CreateForm(AParentWnd: HWND)
  : TdxfrmCommonFileCustomDialog;
begin
  Result := OpenFileDialogFormClass.Create(Self);
end;

{ TdxSaveFileDialog }

function TdxSaveFileDialog.CreateForm(AParentWnd: HWND)
  : TdxfrmCommonFileCustomDialog;
begin
  Result := SaveFileDialogFormClass.Create(Self);
end;

end.
