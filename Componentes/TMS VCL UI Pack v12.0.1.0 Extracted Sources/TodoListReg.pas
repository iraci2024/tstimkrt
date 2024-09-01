{*************************************************************************}
{ TTodoList component                                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}

unit TodoListReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, TodoList;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TTodoList]);
end;


end.
 