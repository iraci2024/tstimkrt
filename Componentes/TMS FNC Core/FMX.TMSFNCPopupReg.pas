{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2022                               }
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

unit FMX.TMSFNCPopupReg;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes, FMX.TMSFNCPopup
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntF
  {$ENDIF}
  ;

{$IFNDEF LCLLIB}
{$R 'TMSFNCPopupComp.dcr'}
{$ENDIF}
{$IFDEF LCLLIB}
{$R 'TMSFNCPopupCompSmall.dcr'}
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS FNC UI', [TTMSFNCPopup]);
end;

end.
