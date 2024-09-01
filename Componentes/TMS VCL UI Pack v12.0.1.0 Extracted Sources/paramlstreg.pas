{********************************************************************}
{ TPARAMLISTBOX component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2023                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit paramlstreg;
{$I TMSDEFS.INC}
interface

uses
  Paramlistbox, Classes, ParamListProp;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Param', [TParamListBox]);
  RegisterComponents('TMS Param', [TParamListBoxEditor]);
end;



end.

