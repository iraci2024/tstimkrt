{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
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

unit LCLTMSFNCWebBrowserReg;

{$IFNDEF LCLLIB}
{$R 'TMSFNCWebBrowserComp.dcr'}
{$ENDIF}
{$IFDEF LCLLIB}
{$R 'TMSFNCWebBrowserCompSmall.dcr'}
{$ENDIF}

{$I LCLTMSFNCDefines.inc}

interface

uses
  Classes, LCLTMSFNCWebBrowser
  {$IFNDEF LCLLIB}
  ,LCLTMSFNCWebCoreClientBrowser
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntF
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS FNC UI', [TTMSFNCWebBrowser]);
  {$IFNDEF LCLLIB}
  RegisterComponents('TMS FNC UI', [TTMSFNCWebCoreClientBrowser]);
  {$ENDIF}
end;

end.


