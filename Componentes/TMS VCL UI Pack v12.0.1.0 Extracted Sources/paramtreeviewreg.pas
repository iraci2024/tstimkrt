{********************************************************************}
{ TPARAMTREEVIEW component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000-2023                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit paramtreeviewreg;

{$I TMSDEFS.INC}
interface

uses
  ParamTreeview, Classes, ParamTreeProp;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Param', [TParamTreeview]);
  RegisterComponents('TMS Param', [TParamTreeviewEditor]);
end;



end.

