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

unit dxGanttControlCursors;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
  Windows, Controls;

type
  { TdxGanttControlCursors }

  TdxGanttControlCursors = class
  protected
    class procedure Initialize; static;
    class procedure Finalize; static;
  public
    class function ArrowLeft: TCursor; static;
    class function ArrowRight: TCursor; static;
    class function ColumnResize: TCursor; static;
    class function HideColumn: TCursor; static;
    class function HSize: TCursor; static;
    class function HSplit: TCursor; static;
    class function HResize: TCursor; static;

    class function TaskCreating: TCursor; static;
    class function TaskMove: TCursor; static;
    class function TaskMoving: TCursor; static;
    class function TaskResize: TCursor; static;
    class function TaskComplete: TCursor; static;
    class function TaskLinking: TCursor; static;
  end;

implementation

{$R 'dxGanttControlCursors.res'}

uses
  Forms, cxLibraryConsts, cxControls;

{ TdxGanttControlCursors }

class procedure TdxGanttControlCursors.Initialize;
begin
  dxRegisterCursor(crdxGanttControlTaskComplete, HInstance,
    'DXGANTTCONTROLTASKCOMPLETE');
  dxRegisterCursor(crdxGanttControlTaskMove, HInstance,
    'DXGANTTCONTROLTASKMOVE');
end;

class procedure TdxGanttControlCursors.Finalize;
begin
  dxUnregisterCursor(crdxGanttControlTaskMove);
  dxUnregisterCursor(crdxGanttControlTaskComplete);
end;

class function TdxGanttControlCursors.HideColumn: TCursor;
begin
  Result := crcxRemove;
end;

class function TdxGanttControlCursors.HSize: TCursor;
begin
  Result := crcxHorzSize;
end;

class function TdxGanttControlCursors.ColumnResize: TCursor;
begin
  Result := crcxHorzSize;
end;

class function TdxGanttControlCursors.ArrowLeft: TCursor;
begin
  Result := crcxLeftArrow;
end;

class function TdxGanttControlCursors.ArrowRight: TCursor;
begin
  Result := crcxRightArrow;
end;

class function TdxGanttControlCursors.TaskComplete: TCursor;
begin
  Result := crdxGanttControlTaskComplete;
end;

class function TdxGanttControlCursors.TaskCreating: TCursor;
begin
  Result := crcxCross;
end;

class function TdxGanttControlCursors.TaskLinking: TCursor;
begin
  Result := crcxTaskLink;
end;

class function TdxGanttControlCursors.TaskMove: TCursor;
begin
  Result := crdxGanttControlTaskMove;
end;

class function TdxGanttControlCursors.TaskMoving: TCursor;
begin
  Result := crcxDrag;
end;

class function TdxGanttControlCursors.TaskResize: TCursor;
begin
  Result := crcxHorzResize;
end;

class function TdxGanttControlCursors.HSplit: TCursor;
begin
  Result := crHSplit;
end;

class function TdxGanttControlCursors.HResize: TCursor;
begin
  Result := crcxHorzResize;
end;

initialization

TdxGanttControlCursors.Initialize;

finalization

TdxGanttControlCursors.Finalize;

end.
