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

unit VCL.TMSFNCBitmapContainerEditorDE;

{$I VCL.TMSFNCDefines.inc}

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
  TTMSFNCBitmapContainerEditorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit{$IFDEF WEBLIB}(AProc: TModalResultProc){$ENDIF}; override;
  end;

implementation

uses
  VCL.TMSFNCBitmapContainerEditor, VCL.TMSFNCBitmapContainer, VCL.TMSFNCUtils, VCL.TMSFNCTypes, SysUtils
  {$IFNDEF LCLLIB}
  ,UITypes
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,VCL.Forms
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

{ TTMSFNCBitmapContainerEditorProperty }

procedure TTMSFNCBitmapContainerEditorProperty.Edit{$IFDEF WEBLIB}(AProc: TModalResultProc){$ENDIF};
var
  e: TTMSFNCBitmapContainerEditor;
  bc: TTMSFNCBitmapContainer;
  p: TPersistent;
begin
  {$IFNDEF LCLWEBLIB}
  p := TPersistent(GetOrdValue);
  {$ELSE}
  p := TPersistent(GetObjectValue);
  {$ENDIF}

  if (p is TTMSFNCBitmapCollection) and (TTMSFNCBitmapCollection(p).Owner is TTMSFNCBitmapContainer) then
  begin
    bc := TTMSFNCBitmapContainer(TTMSFNCBitmapCollection(p).Owner);
    e := TTMSFNCBitmapContainerEditor.Create(Application);
    try
      e.BitmapContainer := bc;
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

function TTMSFNCBitmapContainerEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TTMSFNCBitmapContainerEditorProperty.GetValue: string;
begin
  Result := '(TTMSFNCBitmapCollection)';
end;

end.


