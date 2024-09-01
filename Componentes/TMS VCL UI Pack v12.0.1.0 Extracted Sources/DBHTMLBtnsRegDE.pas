{********************************************************************}
{ TDBHTMLCheckBox & TDBHTMLRadioGroup component                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999 - 2020                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit DBHTMLBtnsRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, htmlbtns, htmlde, DBHTMLBtns, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(String), TDBHTMLCheckbox, 'Caption', THTMLStringProperty);
  {$ENDIF}
end;



end.

