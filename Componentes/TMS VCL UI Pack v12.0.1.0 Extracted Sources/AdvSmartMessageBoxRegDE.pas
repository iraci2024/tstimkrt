{********************************************************************}
{ AdvSmartMessageBoxRegDE components                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012 - 2020                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit AdvSmartMessageBoxRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmartMessageBox, htmlde, DesignIntf, DesignEditors;


procedure Register;

implementation

procedure Register;
begin
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(String), TDefaultSmartMessage, 'Text', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), TAdvSmartMessage, 'Text', THTMLStringProperty);
  {$ENDIF}
end;



end.

