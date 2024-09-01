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

unit FMX.TMSFNCWebSocketDE;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes
  {$IFNDEF LCLLIB}
  ,UITypes,DesignIntf, DesignEditors
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,LCLType, ComponentEditors, PropEdits, Controls
  {$ENDIF}
  ;

type
  {$IFNDEF LCLLIB}
  TTMSFNCWebSocketCommonSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
  {$ENDIF}

implementation

{$IFNDEF LCLLIB}

{ TTMSFNCWebSocketCommonSelectionEditor }

procedure TTMSFNCWebSocketCommonSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('FMX.TMSFNCWebSocketCommon');
end;
{$ENDIF}

end.

