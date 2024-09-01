{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2019 - 2022                               }
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

unit AdvGraphicsPDFIOReg;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvGraphicsPDFEngine
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntF
  {$ENDIF}
  ;

{$IFNDEF FNCLIB}
{$R 'AdvGraphicsPDFIO.dcr'}
{$ENDIF}
{$IFDEF FNCLIB}
{$IFNDEF LCLLIB}
{$R 'AdvGraphicsPDFIOComp.dcr'}
{$ENDIF}
{$IFDEF LCLLIB}
{$R 'AdvGraphicsPDFIOCompSmall.dcr'}
{$ENDIF}
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS UI', [TAdvGraphicsPDFIO]);
end;

end.


