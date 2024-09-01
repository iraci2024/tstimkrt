{********************************************************************}
{ THTMDialog component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{  Written by                                                        }
{    TMS Software                                                    }
{    Copyright © 2001 - 2020                                         }
{    Email : info@tmssoftware.com                                    }
{    Web : http://www.tmssoftware.com                                }
{********************************************************************}

unit htmldlgregde;

interface

{$I TMSDEFS.INC}

uses
  Classes, HTMLDialog, HTMLDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(TStringList), THTMLDialog, 'HTMLText', THTMLTextProperty);
  {$ENDIF}
end;

end.

