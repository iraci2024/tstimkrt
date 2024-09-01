{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEntityMapping Framework }
{ }
{ Copyright (c) 2016-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK AND }
{ ALL ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
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

unit cxEMFDataDefinitions;

interface

uses
  SysUtils, Classes, Rtti,
  cxEdit, cxCustomData, dxEMF.Metadata, cxEMFData;

type
  { TcxEMFDefaultValuesProvider }

  TcxEMFDefaultValuesProvider = class(TcxCustomEditDefaultValuesProvider,
    IcxEMFDefaultValuesProvider, IdxEMFDataController)
  private
    FFieldName: string;
    FMemberInfo: TdxMappingMemberInfo;
    function IsDefaultDataAvailable: Boolean; inline;
    // IcxEMFDefaultValuesProvider
    function GetFieldName: string;
    function GetMemberInfo: TdxMappingMemberInfo;
    procedure SetFieldName(const Value: string);
    procedure SetMemberInfo(const Value: TdxMappingMemberInfo);
    // IdxEMFDataController
    function GetDataController: TdxEMFDataController;
  public
    function IsBlob: Boolean; override;
    function DefaultBlobKind: TcxBlobKind; override;
    function DefaultDisplayFormat: string; override;
    function DefaultEditFormat: string; override;
    function DefaultEditMask: string; override;
    function DefaultIsFloatValue: Boolean; override;
    function DefaultMaxLength: Integer; override;
    function DefaultMaxValue: Double; override;
    function DefaultMinValue: Double; override;
    function DefaultPrecision: Integer; override;
    function DefaultReadOnly: Boolean; override;
    function DefaultRequired: Boolean; override;
    function IsValidChar(AChar: Char): Boolean; override;

    property FieldName: string read FFieldName write FFieldName;
    property MemberInfo: TdxMappingMemberInfo read FMemberInfo
      write SetMemberInfo;
  end;

implementation

uses
  TypInfo, DB;

{ TcxEMFDefaultValuesProvider }

function TcxEMFDefaultValuesProvider.IsDefaultDataAvailable: Boolean;
begin
  Result := FMemberInfo <> nil;
end;

function TcxEMFDefaultValuesProvider.IsValidChar(AChar: Char): Boolean;
begin
  if IsDefaultDataAvailable then
    Result := MemberInfo.IsValidChar(AChar)
  else
    Result := inherited IsValidChar(AChar);
end;

function TcxEMFDefaultValuesProvider.DefaultBlobKind: TcxBlobKind;
begin
  if IsDefaultDataAvailable then
    case FMemberInfo.FieldType of
      ftWideMemo:
        Result := bkMemo;
      ftGraphic:
        Result := bkGraphic;
      ftBlob:
        Result := bkBlob;
    else
      Result := bkNone;
    end
  else
    Result := bkNone;
end;

function TcxEMFDefaultValuesProvider.DefaultDisplayFormat: string;
begin
  if IsDefaultDataAvailable and FMemberInfo.DisplayFormat.HasValue then
    Result := FMemberInfo.DisplayFormat
  else
    Result := inherited DefaultDisplayFormat;
end;

function TcxEMFDefaultValuesProvider.DefaultEditFormat: string;
begin
  if IsDefaultDataAvailable and FMemberInfo.EditFormat.HasValue then
    Result := FMemberInfo.EditFormat
  else
    Result := inherited DefaultEditFormat;
end;

function TcxEMFDefaultValuesProvider.DefaultEditMask: string;
begin
  if IsDefaultDataAvailable and FMemberInfo.EditMask.HasValue then
    Result := FMemberInfo.EditMask
  else
    Result := inherited DefaultEditMask;
end;

function TcxEMFDefaultValuesProvider.DefaultIsFloatValue: Boolean;
begin
  Result := IsDefaultDataAvailable and
    (FMemberInfo.TypeKind = TTypeKind.tkFloat);
end;

function TcxEMFDefaultValuesProvider.DefaultMaxLength: Integer;
begin
  if IsDefaultDataAvailable and
    (FMemberInfo.FieldType in [ftString, ftWideString]) and
    (FMemberInfo.DBColumnSize > 0) then
    Result := FMemberInfo.DBColumnSize
  else
    Result := inherited DefaultMaxLength;
end;

function TcxEMFDefaultValuesProvider.DefaultMaxValue: Double;
begin
  if IsDefaultDataAvailable and FMemberInfo.MaxValue.HasValue then
    Result := FMemberInfo.MaxValue
  else
    Result := inherited DefaultMaxValue;
end;

function TcxEMFDefaultValuesProvider.DefaultMinValue: Double;
begin
  if IsDefaultDataAvailable and FMemberInfo.MinValue.HasValue then
    Result := FMemberInfo.MinValue
  else
    Result := inherited DefaultMinValue;
end;

function TcxEMFDefaultValuesProvider.DefaultPrecision: Integer;
begin
  if IsDefaultDataAvailable and FMemberInfo.Precision.HasValue then
    Result := FMemberInfo.Precision
  else
    Result := inherited DefaultPrecision;
end;

function TcxEMFDefaultValuesProvider.DefaultReadOnly: Boolean;
begin
  Result := (FMemberInfo = nil) or FMemberInfo.IsReadOnly;
end;

function TcxEMFDefaultValuesProvider.DefaultRequired: Boolean;
begin
  Result := IsDefaultDataAvailable and FMemberInfo.IsRequired;
end;

function TcxEMFDefaultValuesProvider.GetDataController: TdxEMFDataController;
var
  AIntf: IdxEMFDataController;
begin
  if Supports(Owner, IdxEMFDataController, AIntf) then
    Result := AIntf.DataController
  else
    Result := nil;
end;

function TcxEMFDefaultValuesProvider.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TcxEMFDefaultValuesProvider.GetMemberInfo: TdxMappingMemberInfo;
begin
  Result := FMemberInfo;
end;

function TcxEMFDefaultValuesProvider.IsBlob: Boolean;
begin
  Result := IsDefaultDataAvailable and FMemberInfo.IsBlob;
end;

procedure TcxEMFDefaultValuesProvider.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TcxEMFDefaultValuesProvider.SetMemberInfo
  (const Value: TdxMappingMemberInfo);
begin
  FMemberInfo := Value;
end;

end.
