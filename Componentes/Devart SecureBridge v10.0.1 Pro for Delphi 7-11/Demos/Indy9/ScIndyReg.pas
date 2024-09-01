unit ScIndyReg;

interface

uses
  Classes, ScIndy;

procedure Register;

implementation

{$R ScIndy.res}
{$IFDEF VER180}
  {$R ScIndy10p.res}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('SecureBridge', [TScIdIOHandler]);
end;

end.

