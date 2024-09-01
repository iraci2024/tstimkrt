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

unit AdvPDFLibReg;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvPDFLib
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntF
  {$ENDIF}
  ;

{$IFNDEF FNCLIB}
{$R 'AdvPDFLib.dcr'}
{$ENDIF}
{$IFDEF FNCLIB}
{$IFNDEF LCLLIB}
{$R 'AdvPDFLibComp.dcr'}
{$ENDIF}
{$IFDEF LCLLIB}
{$R 'AdvPDFLibCompSmall.dcr'}
{$ENDIF}
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS UI', [TAdvPDFLib]);
end;

end.


