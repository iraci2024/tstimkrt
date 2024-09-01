﻿{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2022                                           }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit FMX.TMSFNCResponsiveManagerRegDE;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, FMX.TMSFNCResponsiveManagerDE, FMX.TMSFNCResponsiveManager,
  FMX.TMSFNCTypes, FMX.TMSFNCStateManager
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
  RegisterComponentEditor(TTMSFNCResponsiveManager, TTMSFNCResponsiveManagerEditor);
  {$IFNDEF LCLWEBLIB}
  UnlistPublishedProperty(TTMSFNCStateManagerItem, 'Content');
  UnlistPublishedProperty(TTMSFNCCustomStateManager, 'OnCanLoadProperty');
  UnlistPublishedProperty(TTMSFNCCustomStateManager, 'OnCanSaveProperty');
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(Integer),TTMSFNCResponsiveManager,'ActiveState', TTMSFNCResponsiveManagerStateProperty);
end;

{$IFDEF WEBLIB}
initialization
  Register;
{$ENDIF}

end.

