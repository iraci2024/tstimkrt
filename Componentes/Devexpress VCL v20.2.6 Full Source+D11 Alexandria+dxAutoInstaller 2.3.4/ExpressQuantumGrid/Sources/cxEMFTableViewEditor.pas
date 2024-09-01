{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressQuantumGrid }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL }
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

unit cxEMFTableViewEditor;

{$I cxVer.inc}

interface

uses
  Variants, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls,
  cxViewEditor, cxCustomTableViewEditor, cxGridCustomView, cxGridTableView,
  cxLookAndFeelPainters, cxButtons, cxPC, cxControls, cxGraphics,
  cxLookAndFeels, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, cxContainer, cxEdit,
  cxListBox, dxLayoutControl,
  cxGridEMFTableView, cxTableViewEditor, cxEMFData;

type
  TcxEMFTableViewEditor = class(TcxCustomTableViewEditor);

  TcxGridEMFTableViewMenuProvider = class(TcxGridTableViewMenuProvider)
  private
    FEMFDataSourceNames: TStringList;
    procedure GetEMFDataSourceName(const S: string);
    function GetEMFViewDataSource: TdxEMFDataControllerCustomDataSource;
    procedure SetEMFViewDataSource(const Value
      : TdxEMFDataControllerCustomDataSource);
    function GetEMFGridView: TcxCustomGridView;
  protected
    procedure InitDataBindingItems; override;
    procedure InitEMFDataBindingItems; virtual;
    function GetEMFDataSourceNames: TStringList;
    procedure LinkToEMFDataSource(Sender: TcxGridViewMenuItem);

    property EMFViewDataSource: TdxEMFDataControllerCustomDataSource
      read GetEMFViewDataSource write SetEMFViewDataSource;
    property EMFGridView: TcxCustomGridView read GetEMFGridView;
  end;

implementation

uses
  TypInfo;

{$R *.dfm}
{ TcxGridEMFTableViewMenuProvider }

procedure TcxGridEMFTableViewMenuProvider.GetEMFDataSourceName(const S: string);
begin
  FEMFDataSourceNames.Add(S);
end;

function TcxGridEMFTableViewMenuProvider.GetEMFDataSourceNames: TStringList;
begin
  Result := TStringList.Create;
  FEMFDataSourceNames := Result;
  Designer.GetComponentNames
    (GetTypeData(TypeInfo(TdxEMFDataControllerCustomDataSource)),
    GetEMFDataSourceName);
  FEMFDataSourceNames := nil;
  Result.Sort;
end;

function TcxGridEMFTableViewMenuProvider.GetEMFGridView: TcxCustomGridView;
begin
  Result := GridView;
  if not(Result.DataController is TdxEMFDataController) then
    Result := nil;
end;

function TcxGridEMFTableViewMenuProvider.GetEMFViewDataSource
  : TdxEMFDataControllerCustomDataSource;
begin
  Result := TdxEMFDataController(EMFGridView.DataController).DataSource;
end;

procedure TcxGridEMFTableViewMenuProvider.InitDataBindingItems;
begin
  inherited InitDataBindingItems;
  if EMFGridView <> nil then
  begin
    InitEMFDataBindingItems;
    Items.AddSeparator;
  end;
end;

procedure TcxGridEMFTableViewMenuProvider.InitEMFDataBindingItems;
var
  I: Integer;
  AItem: TcxGridViewMenuItem;
  ADataSourceNames: TStringList;

  function IsDataSourceLinked(AIndex: Integer): Boolean;
  begin
    Result := Designer.GetComponent(ADataSourceNames[AIndex])
      = EMFViewDataSource;
  end;

begin
  ADataSourceNames := GetEMFDataSourceNames;
  try
    if ADataSourceNames.Count = 0 then
      Exit;
    if ADataSourceNames.Count = 1 then
      Items.AddItem('Link to ' + ADataSourceNames[0], LinkToEMFDataSource, True,
        IsDataSourceLinked(0), TObject(0))
    else
    begin
      AItem := Items.AddItem('Link to DataSource', nil, True, False);
      for I := 0 to ADataSourceNames.Count - 1 do
        AItem.AddItem(ADataSourceNames[I], LinkToEMFDataSource, True,
          IsDataSourceLinked(I), TObject(I));
    end;
  finally
    ADataSourceNames.Free;
  end;
end;

procedure TcxGridEMFTableViewMenuProvider.LinkToEMFDataSource
  (Sender: TcxGridViewMenuItem);
var
  ADataSourceNames: TStringList;
  ADataSource: TdxEMFDataControllerCustomDataSource;
begin
  ADataSourceNames := GetEMFDataSourceNames;
  try
    ADataSource := TdxEMFDataControllerCustomDataSource
      (Designer.GetComponent(ADataSourceNames[Integer(Sender.Data)]));
    if EMFViewDataSource <> ADataSource then
    begin
      (EMFGridView.DataController as IcxCustomGridDataController)
        .DeleteAllItems;
      EMFViewDataSource := ADataSource;
      (EMFGridView.DataController as IcxCustomGridDataController)
        .CreateAllItems(False);
      DesignerModified;
    end;
  finally
    ADataSourceNames.Free;
  end;
end;

procedure TcxGridEMFTableViewMenuProvider.SetEMFViewDataSource
  (const Value: TdxEMFDataControllerCustomDataSource);
begin
  TdxEMFDataController(EMFGridView.DataController).DataSource := Value;
end;

initialization

RegisterViewEditorClass(TcxGridEMFTableView, TcxEMFTableViewEditor);
RegisterViewMenuProviderClass(TcxGridEMFTableView,
  TcxGridEMFTableViewMenuProvider);

finalization

UnregisterViewEditorClass(TcxGridEMFTableView, TcxEMFTableViewEditor);
UnregisterViewMenuProviderClass(TcxGridEMFTableView,
  TcxGridEMFTableViewMenuProvider);

end.
