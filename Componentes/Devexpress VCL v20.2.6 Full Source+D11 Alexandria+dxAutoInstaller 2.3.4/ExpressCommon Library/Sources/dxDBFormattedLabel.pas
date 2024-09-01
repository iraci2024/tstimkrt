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

unit dxDBFormattedLabel;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, DB, Dialogs, Forms, Graphics, Messages,
  StdCtrls, SysUtils, cxControls, cxDBEdit, cxEdit, dxFormattedLabel;

type
  { TdxDBFormattedLabel }

  TdxDBFormattedLabel = class(TdxCustomFormattedLabel)
  private
    function GetActiveProperties: TdxFormattedLabelProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TdxFormattedLabelProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TdxFormattedLabelProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure Initialize; override;
    procedure SetEditAutoSize(Value: Boolean); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxFormattedLabelProperties
      read GetActiveProperties;
  published
    property Anchors;
    property AutoSize default False;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding
      write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TdxFormattedLabelProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  VDBConsts;

{ TdxDBFormattedLabel }

class function TdxDBFormattedLabel.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TdxFormattedLabelProperties;
end;

class function TdxDBFormattedLabel.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

procedure TdxDBFormattedLabel.Initialize;
begin
  inherited Initialize;
  AutoSize := False;
end;

procedure TdxDBFormattedLabel.SetEditAutoSize(Value: Boolean);
begin
  if Value and Assigned(DataBinding) and Assigned(DataBinding.DataLink) and
    DataBinding.DataLink.DataSourceFixed then
    DatabaseError(SDataSourceFixed);
  inherited SetEditAutoSize(Value);
end;

function TdxDBFormattedLabel.GetActiveProperties: TdxFormattedLabelProperties;
begin
  Result := TdxFormattedLabelProperties(InternalGetActiveProperties);
end;

function TdxDBFormattedLabel.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TdxDBFormattedLabel.GetProperties: TdxFormattedLabelProperties;
begin
  Result := TdxFormattedLabelProperties(inherited Properties);
end;

procedure TdxDBFormattedLabel.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TdxDBFormattedLabel.SetProperties(Value: TdxFormattedLabelProperties);
begin
  Properties.Assign(Value);
end;

procedure TdxDBFormattedLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.
