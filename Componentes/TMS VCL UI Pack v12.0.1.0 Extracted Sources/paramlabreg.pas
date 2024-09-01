{********************************************************************}
{ TPARAMLABEL component                                              }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{           copyright © 2000 - 2023                                  }
{           Email : info@tmssoftware.com                             }
{           Web : http://www.tmssoftware.com                         }
{********************************************************************}

unit paramlabreg;

{$I TMSDEFS.INC}
interface

uses
  ParamLabel, Classes, ParamProp;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Param', [TParamLabel]);
  RegisterComponents('TMS Param', [TParamLabelEditor]);
end;



end.

