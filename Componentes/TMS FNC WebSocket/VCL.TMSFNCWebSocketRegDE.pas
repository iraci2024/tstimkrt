{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2022                                      }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit VCL.TMSFNCWebSocketRegDE;

{$I VCL.TMSFNCDefines.inc}

interface

uses
  Classes, VCL.TMSFNCWebSocketDE, VCL.TMSFNCWebSocketClient, VCL.TMSFNCWebSocketServer
  {$IFNDEF LCLLIB}
  ,DesignIntf
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,PropEdits, ComponentEditors
  {$ENDIF}
  ;

procedure Register;

implementation

{$IFDEF VCLLIB}
uses
  ToolsApi, System.SysUtils, VCL.Dialogs, VCL.Graphics;

const TMSPRODUCTOFFSET = 700;
{$I TMSProductSplash.inc}
{$ENDIF}

procedure Register;
begin
  {$IFDEF VCLLIB}
  ForceDemandLoadState(dlDisable);
  AddSplash;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  RegisterSelectionEditor(TTMSFNCCustomWebSocketServer, TTMSFNCWebSocketCommonSelectionEditor);
  RegisterSelectionEditor(TTMSFNCCustomWebsocketClient, TTMSFNCWebSocketCommonSelectionEditor);
  {$ENDIF}
end;

{$IFDEF WEBLIB}
initialization
  Register;
{$ENDIF}

end.

