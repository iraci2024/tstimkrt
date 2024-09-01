{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2023                                      }
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

unit AdvPDFViewerReg;

{$I TMSDEFS.inc}

interface

uses
  Classes, AdvPDFViewer
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  ;

{$R 'AdvPDFViewerComp.dcr'}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS PDF', [TAdvPDFViewer]);
end;

end.
