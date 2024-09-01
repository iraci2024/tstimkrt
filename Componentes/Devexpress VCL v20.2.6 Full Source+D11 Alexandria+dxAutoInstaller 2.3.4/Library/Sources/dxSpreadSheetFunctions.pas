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

unit dxSpreadSheetFunctions;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, SysUtils, Types,
  // CX
  dxCore,
  dxCoreClasses,
  cxClasses,
  // SpreadSheet
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreStrs,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

type
  TdxSpreadSheetFunction = procedure(Sender: TdxSpreadSheetFormulaResult;
    const AParams: TdxSpreadSheetFormulaToken);
  TdxSpreadSheetFunctionParamInfo = procedure(var AParamCount: Integer;
    var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

  TdxSpreadSheetFunctionType = (ftCommon, ftCompatibility, ftDateTime, ftMath,
    ftFinancial, ftInformation, ftLookupAndReference, ftLogical, ftStatistical,
    ftText, ftCube, ftDatabase, ftEngineering);

  { TdxSpreadSheetFunctionInfo }

  TdxSpreadSheetFunctionInfo = class
  public
    AvailableInContainerControls: Boolean;
    DescriptionPtr: TcxResourceStringID;
    NamePtr: TcxResourceStringID;
    Name: string;
    PrefixID: Integer;
    Proc: TdxSpreadSheetFunction;
    ParamInfo: TdxSpreadSheetFunctionParamInfo;
    ResultKind: TdxSpreadSheetFunctionResultKind;
    Token: Word;
    TypeID: TdxSpreadSheetFunctionType;

    function GetPrefix: string;
    function IsValid: Boolean;
    function ToString: string; override;
    procedure UpdateInfo(AFormatSettings: TdxSpreadSheetFormatSettings);
  end;

  { TdxSpreadSheetFunctionsRepository }

  TdxSpreadSheetFunctionsRepository = class(TcxObjectList)
  strict private
    FSorted: Boolean;
    function GetItem(AIndex: Integer): TdxSpreadSheetFunctionInfo;
    procedure SetSorted(AValue: Boolean);
  protected
    procedure InternalAdd(const AName: TcxResourceStringID;
      AProc: TdxSpreadSheetFunction;
      AParamInfo: TdxSpreadSheetFunctionParamInfo;
      AResultKind: TdxSpreadSheetFunctionResultKind; AToken: Word;
      AType: TdxSpreadSheetFunctionType = ftCommon;
      const ADescription: TcxResourceStringID = nil; APrefixID: Integer = 0;
      AAvailableInContainerControls: Boolean = True);
    property Sorted: Boolean read FSorted write SetSorted;
  public
    procedure Add(const AName: TcxResourceStringID;
      AProc: TdxSpreadSheetFunction;
      AParamInfo: TdxSpreadSheetFunctionParamInfo;
      AResultKind: TdxSpreadSheetFunctionResultKind; AToken: Word;
      AType: TdxSpreadSheetFunctionType = ftCommon;
      const ADescription: TcxResourceStringID = nil; APrefixID: Integer = 0;
      AAvailableInContainerControls: Boolean = True);
    function AddUnknown(const AName: string;
      AType: TdxSpreadSheetFunctionType = ftCommon): TdxSpreadSheetFunctionInfo;
    function GetInfoByID(const ID: Integer): TdxSpreadSheetFunctionInfo; inline;
    function GetInfoByName(const AName: string): TdxSpreadSheetFunctionInfo;
      overload; inline;
    function GetInfoByName(const AName: PChar; ALength: Integer)
      : TdxSpreadSheetFunctionInfo; overload; inline;
    procedure Remove(const AName: TcxResourceStringID);
    procedure TranslationChanged(AFormatSettings: TdxSpreadSheetFormatSettings);

    function HasFunctions(AFunctionType: TdxSpreadSheetFunctionType): Boolean;

    property Items[Index: Integer]: TdxSpreadSheetFunctionInfo
      read GetItem; default;
  end;

function dxSpreadSheetFunctionsRepository: TdxSpreadSheetFunctionsRepository;
function dxSpreadSheetFunctionTypeNameAsResString
  (AType: TdxSpreadSheetFunctionType): Pointer;
function dxSpreadSheetFunctionTypeNameAsString
  (AType: TdxSpreadSheetFunctionType): string;

implementation

uses
  dxStringHelper,
  dxSpreadSheetFunctionsCompatibility,
  dxSpreadSheetFunctionsDateTime,
  dxSpreadSheetFunctionsFinancial,
  dxSpreadSheetFunctionsInformation,
  dxSpreadSheetFunctionsLogical,
  dxSpreadSheetFunctionsLookup,
  dxSpreadSheetFunctionsMath,
  dxSpreadSheetFunctionsStatistical,
  dxSpreadSheetFunctionsText,
  dxSpreadSheetFunctionsStrs;

var
  Repository: TdxSpreadSheetFunctionsRepository;

function dxSpreadSheetFunctionsRepository: TdxSpreadSheetFunctionsRepository;
begin
  if Repository = nil then
    Repository := TdxSpreadSheetFunctionsRepository.Create;
  Result := Repository;
end;

function dxSpreadSheetFunctionTypeNameAsResString
  (AType: TdxSpreadSheetFunctionType): Pointer;
begin
  case AType of
    ftCompatibility:
      Result := @sfnCategoryCompatibility;
    ftDateTime:
      Result := @sfnCategoryDateTime;
    ftMath:
      Result := @sfnCategoryMath;
    ftFinancial:
      Result := @sfnCategoryFinancial;
    ftInformation:
      Result := @sfnCategoryInformation;
    ftLookupAndReference:
      Result := @sfnCategoryLookupAndReference;
    ftLogical:
      Result := @sfnCategoryLogical;
    ftStatistical:
      Result := @sfnCategoryStatistical;
    ftText:
      Result := @sfnCategoryText;
    ftCube:
      Result := @sfnCategoryCube;
    ftDatabase:
      Result := @sfnCategoryDatabase;
    ftEngineering:
      Result := @sfnCategoryEngineering;
  else // ftCommon
    Result := @sfnCategoryCommon;
  end;
end;

function dxSpreadSheetFunctionTypeNameAsString
  (AType: TdxSpreadSheetFunctionType): string;
begin
  Result := cxGetResourceString
    (dxSpreadSheetFunctionTypeNameAsResString(AType));
end;

function dxCompareFunctions(AInfo1, AInfo2: TdxSpreadSheetFunctionInfo)
  : Integer;
begin
  if AInfo1 = AInfo2 then
    Result := 0
  else
    Result := dxSpreadSheetCompareText(AInfo1.Name, AInfo2.Name)
end;

{ TdxSpreadSheetFunctionInfo }

function TdxSpreadSheetFunctionInfo.GetPrefix: string;
const
  AResult: array [0 .. 1] of string = ('', dxSpreadSheetFeatureFunctionPrefix);
begin
  Result := AResult[PrefixID];
end;

function TdxSpreadSheetFunctionInfo.IsValid: Boolean;
begin
  Result := Assigned(Proc);
end;

function TdxSpreadSheetFunctionInfo.ToString: string;
var
  ABuffer: TStringBuilder;
  AParamCount: Integer;
  AParamKind: TdxSpreadSheetFunctionParamKind;
  AParamName: Pointer;
  AParamsKind: TdxSpreadSheetFunctionParamKindInfo;
  I: Integer;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    ABuffer.Append(Name);
    if Assigned(ParamInfo) then
    begin
      ParamInfo(AParamCount, AParamsKind);
      ABuffer.Append('(');
      for I := 0 to AParamCount - 1 do
      begin
        AParamKind := AParamsKind[I];
        if AParamKind in [fpkNonRequiredValue, fpkNonRequiredArray,
          fpkNonRequiredUnlimited] then
          ABuffer.Append('[');
        if AParamKind in [fpkArray, fpkNonRequiredArray] then
          AParamName := @sfnParamArray
        else
          AParamName := @sfnParamValue;

        ABuffer.Append(cxGetResourceString(AParamName)).Append(I + 1);
        if AParamKind in [fpkNonRequiredValue, fpkNonRequiredArray,
          fpkNonRequiredUnlimited] then
          ABuffer.Append(']');
        if AParamKind in [fpkUnlimited, fpkNonRequiredUnlimited] then
          ABuffer.Append(', ...')
        else if I + 1 < AParamCount then
          ABuffer.Append(', ');
      end;
      ABuffer.Append(')');
    end
    else
      ABuffer.Append('(?)');

    Result := ABuffer.ToString;
  finally
    ABuffer.Free;
  end;
end;

procedure TdxSpreadSheetFunctionInfo.UpdateInfo(AFormatSettings
  : TdxSpreadSheetFormatSettings);
begin
  if NamePtr = nil then
    Exit;
  if AFormatSettings <> nil then
    Name := AFormatSettings.GetFunctionName(NamePtr)
  else
    Name := dxSpreadSheetUpperCase(cxGetResourceString(NamePtr));
end;

{ TdxSpreadSheetFunctionsRepository }

procedure TdxSpreadSheetFunctionsRepository.Add(const AName
  : TcxResourceStringID; AProc: TdxSpreadSheetFunction;
  AParamInfo: TdxSpreadSheetFunctionParamInfo;
  AResultKind: TdxSpreadSheetFunctionResultKind; AToken: Word;
  AType: TdxSpreadSheetFunctionType = ftCommon;
  const ADescription: TcxResourceStringID = nil; APrefixID: Integer = 0;
  AAvailableInContainerControls: Boolean = True);
var
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AInfo := GetInfoByName(cxGetResourceString(AName));
  if AInfo = nil then
    InternalAdd(AName, AProc, AParamInfo, AResultKind, AToken, AType,
      ADescription, APrefixID, AAvailableInContainerControls)
  else
  begin
    AInfo.NamePtr := AName;
    AInfo.DescriptionPtr := ADescription;
    AInfo.PrefixID := APrefixID;
    AInfo.Proc := AProc;
    AInfo.ParamInfo := AParamInfo;
    AInfo.ResultKind := AResultKind;
    AInfo.Token := AToken;
    AInfo.TypeID := AType;
    AInfo.AvailableInContainerControls := AAvailableInContainerControls;
    AInfo.UpdateInfo(nil);
  end;
end;

function TdxSpreadSheetFunctionsRepository.AddUnknown(const AName: string;
  AType: TdxSpreadSheetFunctionType = ftCommon): TdxSpreadSheetFunctionInfo;
begin
  Result := TdxSpreadSheetFunctionInfo.Create;
  Result.NamePtr := nil;
  Result.Name := AName;
  Result.Proc := nil;
  Result.ParamInfo := nil;
  inherited Add(Result);
  Sorted := False;
end;

function TdxSpreadSheetFunctionsRepository.GetInfoByID(const ID: Integer)
  : TdxSpreadSheetFunctionInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Token = ID then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxSpreadSheetFunctionsRepository.GetInfoByName(const AName: string)
  : TdxSpreadSheetFunctionInfo;
begin
  Result := GetInfoByName(@AName[1], Length(AName));
end;

function TdxSpreadSheetFunctionsRepository.GetInfoByName(const AName: PChar;
  ALength: Integer): TdxSpreadSheetFunctionInfo;
var
  L, H, I, C: TdxNativeInt;
begin
  if (ALength > Length(dxSpreadSheetFeatureFunctionPrefix)) and
    (dxSpreadSheetCompareText(dxSpreadSheetFeatureFunctionPrefix, Pointer(AName)
    ) = 0) then
  begin
    Dec(ALength, Length(dxSpreadSheetFeatureFunctionPrefix));
    Result := GetInfoByName(ShiftPointer(AName,
      Length(dxSpreadSheetFeatureFunctionPrefix) * 2), ALength);
    Exit;
  end;
  Sorted := True;
  Result := nil;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := dxSpreadSheetCompareText(TdxSpreadSheetFunctionInfo(List[I]).Name,
      AName, ALength);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := TdxSpreadSheetFunctionInfo(List[I]);
        Break;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetFunctionsRepository.Remove
  (const AName: TcxResourceStringID);
var
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AInfo := GetInfoByName(cxGetResourceString(AName));
  if AInfo <> nil then
  begin
    inherited Remove(AInfo);
    AInfo.Free;
  end;
end;

procedure TdxSpreadSheetFunctionsRepository.TranslationChanged;
var
  I: Integer;
begin
  Sorted := False;
  for I := 0 to Count - 1 do
    Items[I].UpdateInfo(AFormatSettings);
  Sorted := True;
end;

function TdxSpreadSheetFunctionsRepository.HasFunctions(AFunctionType
  : TdxSpreadSheetFunctionType): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].IsValid and (Items[I].TypeID = AFunctionType) then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxSpreadSheetFunctionsRepository.InternalAdd
  (const AName: TcxResourceStringID; AProc: TdxSpreadSheetFunction;
  AParamInfo: TdxSpreadSheetFunctionParamInfo;
  AResultKind: TdxSpreadSheetFunctionResultKind; AToken: Word;
  AType: TdxSpreadSheetFunctionType = ftCommon;
  const ADescription: TcxResourceStringID = nil; APrefixID: Integer = 0;
  AAvailableInContainerControls: Boolean = True);
var
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AInfo := TdxSpreadSheetFunctionInfo.Create;
  AInfo.NamePtr := AName;
  AInfo.DescriptionPtr := ADescription;
  AInfo.PrefixID := APrefixID;
  AInfo.Proc := AProc;
  AInfo.ParamInfo := AParamInfo;
  AInfo.ResultKind := AResultKind;
  AInfo.Token := AToken;
  AInfo.TypeID := AType;
  AInfo.AvailableInContainerControls := AAvailableInContainerControls;
  AInfo.UpdateInfo(nil);
  Sorted := False;
  inherited Add(AInfo);
end;

function TdxSpreadSheetFunctionsRepository.GetItem(AIndex: Integer)
  : TdxSpreadSheetFunctionInfo;
begin
  Result := TdxSpreadSheetFunctionInfo(inherited Items[AIndex]);
end;

procedure TdxSpreadSheetFunctionsRepository.SetSorted(AValue: Boolean);
begin
  if FSorted <> AValue then
  begin
    FSorted := AValue;
    if Sorted then
      Sort(@dxCompareFunctions);
  end;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  // Register known, but not implemented functions

  // Cube functions
  ARepository.InternalAdd(@sfnCubeKPIMember, nil, nil, frkValue, 255, ftCube);
  ARepository.InternalAdd(@sfnCubeMember, nil, nil, frkValue, 255, ftCube);
  ARepository.InternalAdd(@sfnCubeMemberProperty, nil, nil, frkValue,
    255, ftCube);
  ARepository.InternalAdd(@sfnCubeRankedMember, nil, nil, frkValue,
    255, ftCube);
  ARepository.InternalAdd(@sfnCubeSet, nil, nil, frkValue, 255, ftCube);
  ARepository.InternalAdd(@sfnCubeSetCount, nil, nil, frkValue, 255, ftCube);
  ARepository.InternalAdd(@sfnCubeValue, nil, nil, frkValue, 255, ftCube);

  // Database functions
  ARepository.InternalAdd(@sfnDAverage, nil, nil, frkValue, 42, ftDatabase);
  ARepository.InternalAdd(@sfnDCount, nil, nil, frkValue, 40, ftDatabase);
  ARepository.InternalAdd(@sfnDCountA, nil, nil, frkValue, 199, ftDatabase);
  ARepository.InternalAdd(@sfnDGet, nil, nil, frkValue, 235, ftDatabase);
  ARepository.InternalAdd(@sfnDMax, nil, nil, frkValue, 44, ftDatabase);
  ARepository.InternalAdd(@sfnDMin, nil, nil, frkValue, 43, ftDatabase);
  ARepository.InternalAdd(@sfnDProduct, nil, nil, frkValue, 189, ftDatabase);
  ARepository.InternalAdd(@sfnDStDev, nil, nil, frkValue, 45, ftDatabase);
  ARepository.InternalAdd(@sfnDStDevP, nil, nil, frkValue, 195, ftDatabase);
  ARepository.InternalAdd(@sfnDSum, nil, nil, frkValue, 41, ftDatabase);
  ARepository.InternalAdd(@sfnDVar, nil, nil, frkValue, 47, ftDatabase);
  ARepository.InternalAdd(@sfnDVarP, nil, nil, frkValue, 196, ftDatabase);

  // Engineering functions
  ARepository.InternalAdd(@sfnBesselI, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBesselJ, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBesselK, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBesselY, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBin2Dec, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBin2Hex, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBin2Oct, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnBitAnd, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnBitLShift, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnBitOr, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnBitRShift, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnBitXor, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnComplex, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnConvert, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnDec2Bin, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnDec2Hex, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnDec2Oct, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnDelta, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnERF, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnERF_Precise, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnERFC, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnERFC_Precise, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnGestep, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnHex2Bin, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnHex2Dec, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnHex2Oct, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImAbs, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImAginary, nil, nil, frkValue, 255,
    ftEngineering);
  ARepository.InternalAdd(@sfnImArgument, nil, nil, frkValue, 255,
    ftEngineering);
  ARepository.InternalAdd(@sfnImConjugate, nil, nil, frkValue, 255,
    ftEngineering);
  ARepository.InternalAdd(@sfnImCos, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImCosh, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImCot, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImCsc, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImCsch, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImDiv, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImExp, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImLn, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImLog10, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImLog2, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImPower, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImProduct, nil, nil, frkValue, 255,
    ftEngineering);
  ARepository.InternalAdd(@sfnImReal, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImSec, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImSech, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImSin, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImSinh, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnImSqrt, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImSub, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImSum, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnImTan, nil, nil, frkValue, 255,
    ftEngineering, nil, 1);
  ARepository.InternalAdd(@sfnOct2Bin, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnOct2Dec, nil, nil, frkValue, 255, ftEngineering);
  ARepository.InternalAdd(@sfnOct2Hex, nil, nil, frkValue, 255, ftEngineering);
end;

initialization

RegisterFunctions(dxSpreadSheetFunctionsRepository);

finalization

FreeAndNil(Repository);

end.
