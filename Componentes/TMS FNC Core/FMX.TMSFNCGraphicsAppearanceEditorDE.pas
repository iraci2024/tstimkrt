{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCGraphicsAppearanceEditorDE;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes
  {$IFNDEF LCLWEBLIB}
  ,DesignEditors, DesignIntf
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf, WEBLib.Forms
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits
  {$ENDIF}
  ;

type
  TTMSFNCGraphicsFillEditorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit{$IFDEF WEBLIB}(AProc: TModalResultProc){$ENDIF}; override;
  end;

  TTMSFNCGraphicsStrokeEditorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit{$IFDEF WEBLIB}(AProc: TModalResultProc){$ENDIF}; override;
  end;

implementation

uses
  FMX.TMSFNCUtils, FMX.TMSFNCTypes, SysUtils, FMX.TMSFNCGraphicsTypes, FMX.TMSFNCGraphicsAppearanceEditor
  {$IFNDEF LCLLIB}
  ,UITypes
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Forms
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCL.Forms
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,Forms, Controls
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.Graphics
  {$ENDIF}
  ;

{ TTMSFNCGraphicsFillEditorProperty }

procedure TTMSFNCGraphicsFillEditorProperty.Edit{$IFDEF WEBLIB}(AProc: TModalResultProc){$ENDIF};
var
  e: TTMSFNCGraphicsFillEditor;
  f: TTMSFNCGraphicsFill;
  p: TPersistent;
begin
  {$IFNDEF LCLWEBLIB}
  p := TPersistent(GetOrdValue);
  {$ELSE}
  p := TPersistent(GetObjectValue);
  {$ENDIF}

  if (p is TTMSFNCGraphicsFill) then
  begin
    f := TTMSFNCGraphicsFill(p);
    e := TTMSFNCGraphicsFillEditor.Create(Application);
    try
      e.Fill := f;
      e.Execute
      {$IFDEF WEBLIB}
      (procedure(AResult: TModalResult)
      begin
        e.Free;
        if Assigned(AProc) then
          AProc(AResult);
      end)
      {$ENDIF}
      ;
    finally
      {$IFNDEF WEBLIB}
      e.Free;
      {$ENDIF}
    end;
  end;
end;

function TTMSFNCGraphicsFillEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog {$IFNDEF WEBLIB}, paSubProperties{$ENDIF}];
end;

function TTMSFNCGraphicsFillEditorProperty.GetValue: string;
begin
  Result := '(TTMSFNCGraphicsFill)';
end;

procedure TTMSFNCGraphicsFillEditorProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
begin
//  inherited;
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

end;

{ TTMSFNCGraphicsStrokeEditorProperty }

procedure TTMSFNCGraphicsStrokeEditorProperty.Edit{$IFDEF WEBLIB}(AProc: TModalResultProc){$ENDIF};
var
  e: TTMSFNCGraphicsStrokeEditor;
  s: TTMSFNCGraphicsStroke;
  p: TPersistent;
begin
  {$IFNDEF LCLWEBLIB}
  p := TPersistent(GetOrdValue);
  {$ELSE}
  p := TPersistent(GetObjectValue);
  {$ENDIF}

  if (p is TTMSFNCGraphicsStroke) then
  begin
    s := TTMSFNCGraphicsStroke(p);
    e := TTMSFNCGraphicsStrokeEditor.Create(Application);
    try
      e.Stroke := s;
      e.Execute
      {$IFDEF WEBLIB}
      (procedure(AResult: TModalResult)
      begin
        e.Free;
        if Assigned(AProc) then
          AProc(AResult);
      end)
      {$ENDIF}
      ;
    finally
      {$IFNDEF WEBLIB}
      e.Free;
      {$ENDIF}
    end;
  end;
end;

function TTMSFNCGraphicsStrokeEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog {$IFNDEF WEBLIB}, paSubProperties{$ENDIF}];
end;

function TTMSFNCGraphicsStrokeEditorProperty.GetValue: string;
begin
  Result := '(TTMSFNCGraphicsStroke)';
end;

procedure TTMSFNCGraphicsStrokeEditorProperty.GetValues(Proc: TGetStrProc);
begin
  inherited;

end;

end.


