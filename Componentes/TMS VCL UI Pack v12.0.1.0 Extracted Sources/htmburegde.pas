{********************************************************************}
{ THTMLButtons components                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright � 1999 - 2020                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit htmburegde;

interface

{$I TMSDEFS.INC}

uses
  Classes, htmlbtns, htmlde, DesignIntf, DesignEditors;


procedure Register;

implementation

procedure Register;
begin
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(String), THTMLCheckbox, 'Caption', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), THTMLRadioButton, 'Caption', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), THTMLButton, 'Caption', THTMLStringProperty);
  {$ENDIF}
end;



end.

