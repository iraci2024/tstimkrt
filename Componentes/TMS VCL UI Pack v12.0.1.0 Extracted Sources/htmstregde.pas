{**************************************************************************}
{ THTMLStaticText component                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000 - 2020                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{**************************************************************************}

unit htmstregde;

interface

{$I TMSDEFS.INC}

uses
  Classes, HTMLText, HTMLDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(TStrings), THTMLStaticText, 'HTMLText', THTMLTextProperty);
  RegisterComponentEditor(THTMLStaticText, THTMLDefaultEditor);
  {$ENDIF}
end;

end.

