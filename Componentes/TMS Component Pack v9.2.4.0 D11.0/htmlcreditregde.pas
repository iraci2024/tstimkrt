{**************************************************************************}
{ THTMLCredit component                                                    }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright � 2003 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{**************************************************************************}

unit HTMLCreditRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, HTMLCredit, HTMLDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStrings), THTMLCredit, 'HTMLText', THTMLTextProperty);
  RegisterComponentEditor(THTMLCredit, THTMLDefaultEditor);  
end;

end.

