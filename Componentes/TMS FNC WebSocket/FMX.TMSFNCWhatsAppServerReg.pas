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

unit FMX.TMSFNCWhatsAppServerReg;

interface

{$I FMX.TMSFNCDefines.inc}

uses
  Classes, SysUtils,
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  System.RTTI,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  WEBLib.DesignIntf,
  {$ENDIF}
  FMX.TMSFNCWhatsAppServer;

type
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
{$HINTS OFF}
  THackRttiObject = class abstract
  private
    FHandle: Pointer;
    FRttiDataSize: Integer;
    [Weak] FPackage: TRttiPackage;
    [Weak] FParent: TRttiObject;
    FAttributeGetter: TFunc<TArray<TCustomAttribute>>;
  end;
{$IF COMPILERVERSION > 34}
  //Check System.Rtti
  //if https://quality.embarcadero.com/browse/RSP-11620 is fixed then this code
  //can be removed.
{$IFEND}
{$HINTS ON}

  TRttiObjectHelper = class helper for TRttiObject
  public
    procedure ReleaseCachedAttributes;
  end;
  {$ENDIF}
  {$ENDIF}

  TTMSIDERegister = record
  private
    class procedure Register; static;
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
    class procedure Unregister; static;
  {$ENDIF}
  {$ENDIF}
  end;

{$R 'TMSFNCWhatsAppServerComp.dcr'}

procedure Register;

implementation

class procedure TTMSIDERegister.Register;
begin
  RegisterComponents('TMS FNC WebSocket', [TTMSFNCWhatsAppServer]);
end;

procedure Register;
begin
  TTMSIDERegister.Register;
end;

{$IFNDEF LCLLIB}
{$IFNDEF WEBLIB}
class procedure TTMSIDERegister.UnRegister;
var
  lContext: TRttiContext;
begin
  lContext.GetType(TTMSFNCWhatsAppServer).ReleaseCachedAttributes;
end;

{ TRttiObjectHelper }

procedure TRttiObjectHelper.ReleaseCachedAttributes;
begin
  THackRttiObject(Self).FAttributeGetter := nil;
end;
{$ENDIF}
{$ENDIF}

initialization

{$IFNDEF LCLLIB}
{$IFNDEF WEBLIB}
finalization
  TTMSIDERegister.Unregister;
{$ENDIF}
{$ENDIF}

end.


