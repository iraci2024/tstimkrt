{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressGanttControl }
{ }
{ Copyright (c) 2020 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSGANTTCONTROL AND ALL }
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

unit dxGanttControlUtils;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Generics.Defaults, Generics.Collections, Classes,
  dxCore, dxCoreClasses, cxCustomCanvas;

type
  EdxGanttControlException = class(EdxException);

  { TdxGanttControlExceptions }

  TdxGanttControlExceptions = class // for internal use
  public
    class procedure ThrowTasksCannotBeLinkedTwiceException; static;
    class procedure ThrowImageNotFoundException; static;
    class procedure ThrowInvalidFileFormatException; static;
    class procedure
      ThrowOutlineChangeWouldCreateCircularRelationshipException; static;
    class procedure ThrowUnsupportedFileFormatException; static;
    class procedure ThrowTasksAreAlreadyLinkedException; static;
    class procedure ThrowCannotLinkSummaryTaskToItsSubtaskException; static;
    class procedure
      ThrowTasksAreAlreadyLinkedThroughAnotherTaskChainException; static;
  end;

  { TdxGanttControlUtils }

  TdxGanttControlUtils = class
  public
    class function GenerateGUID: string; static;

    class function DateTimeToMilliseconds(const ADateTime: TDateTime)
      : Int64; static;
    class function GetShortDateTimeFormat: string; static;
    class function MeasureTextHeight(ATextLayout: TcxCanvasBasedTextLayout;
      AWidth: Integer; const AText: string; AFont: TcxCanvasBasedFont;
      AIsSingleLine: Boolean): Integer;
    class function MeasureTextWidth(ATextLayout: TcxCanvasBasedTextLayout;
      const AText: string; AFont: TcxCanvasBasedFont): Integer;
  end;

implementation

uses
  cxDrawTextUtils,
  dxCultureInfo,
  dxGanttControlStrs;

{ TdxGanttControlExceptions }

class procedure TdxGanttControlExceptions.ThrowImageNotFoundException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString(@sdxGanttControlExceptionImageNotFound));
end;

class procedure TdxGanttControlExceptions.ThrowInvalidFileFormatException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString(@sdxGanttControlExceptionInvalidFileFormat));
end;

class procedure TdxGanttControlExceptions.
  ThrowOutlineChangeWouldCreateCircularRelationshipException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString
    (@sdxGanttControlExceptionOutlineChangeWouldCreateCircularRelationship));
end;

class procedure TdxGanttControlExceptions.ThrowUnsupportedFileFormatException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString(@sdxGanttControlExceptionUnsupportedFileFormat));
end;

class procedure TdxGanttControlExceptions.
  ThrowCannotLinkSummaryTaskToItsSubtaskException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString
    (@sdxGanttControlExceptionCannotLinkSummaryTaskToItsSubtask));
end;

class procedure TdxGanttControlExceptions.ThrowTasksAreAlreadyLinkedException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString(@sdxGanttControlExceptionTasksAreAlreadyLinked));
end;

class procedure TdxGanttControlExceptions.
  ThrowTasksCannotBeLinkedTwiceException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString(@sdxGanttControlExceptionTasksCannotBeLinkedTwice));
end;

class procedure TdxGanttControlExceptions.
  ThrowTasksAreAlreadyLinkedThroughAnotherTaskChainException;
begin
  raise EdxGanttControlException.Create
    (cxGetResourceString
    (@sdxGanttControlExceptionTasksAreAlreadyLinkedThroughAnotherTaskChain));
end;

{ TdxGanttControlUtils }

class function TdxGanttControlUtils.DateTimeToMilliseconds(const ADateTime
  : TDateTime): Int64;
var
  ATimeStamp: TTimeStamp;
begin
  ATimeStamp := DateTimeToTimeStamp(ADateTime);
  Result := ATimeStamp.Date;
  Result := (Result * MSecsPerDay) + ATimeStamp.Time;
end;

class function TdxGanttControlUtils.GenerateGUID: string;
begin
  Result := dxGenerateID;
end;

class function TdxGanttControlUtils.GetShortDateTimeFormat: string;
begin
  Result := TdxCultureInfo.CurrentCulture.FormatSettings.ShortDateFormat + ' ' +
    TdxCultureInfo.CurrentCulture.FormatSettings.ShortTimeFormat;
end;

class function TdxGanttControlUtils.MeasureTextHeight
  (ATextLayout: TcxCanvasBasedTextLayout; AWidth: Integer; const AText: string;
  AFont: TcxCanvasBasedFont; AIsSingleLine: Boolean): Integer;
const
  ASingleLineFlagsMap: array [Boolean] of Integer = (CXTO_WORDBREAK,
    CXTO_SINGLELINE);
begin
  ATextLayout.SetFlags(ASingleLineFlagsMap[AIsSingleLine] or CXTO_CALCRECT);
  ATextLayout.SetFont(AFont);
  ATextLayout.SetText(AText);
  ATextLayout.SetLayoutConstraints(AWidth, MaxInt);
  Result := ATextLayout.MeasureSize.cy;
end;

class function TdxGanttControlUtils.MeasureTextWidth
  (ATextLayout: TcxCanvasBasedTextLayout; const AText: string;
  AFont: TcxCanvasBasedFont): Integer;
begin
  ATextLayout.SetFlags(CXTO_SINGLELINE or CXTO_CALCRECT);
  ATextLayout.SetFont(AFont);
  ATextLayout.SetText(AText);
  ATextLayout.SetLayoutConstraints(MaxInt, MaxInt);
  Result := ATextLayout.MeasureSize.cx;
end;

end.
