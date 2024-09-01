{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c) 2021                                            }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit WEBLib.TMSFNCBitmapContainerEditorRegDE;

{$I WEBLib.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, WEBLib.TMSFNCTypes, WEBLib.TMSFNCBitmapContainerEditorDE, WEBLib.TMSFNCBitmapContainer
  {$IFNDEF LCLWEBLIB}
  ,DesignIntF
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TTMSFNCBitmapCollection), TPersistent, 'Items', TTMSFNCBitmapContainerEditorProperty);
end;

{$IFDEF WEBLIB}
initialization
  Register;
{$ENDIF}

end.

