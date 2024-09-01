unit TMSTrial;

interface

{$I TMSDEFS.INC}

implementation

{$IFDEF FREEWARE}
uses
  Classes, Windows, Controls, SysUtils, Forms, Graphics,
  Registry, StdCtrls, ExtCtrls, ShellApi;

  {$IFDEF DELPHIXE2_LVL}
  {$I TMSProductTrial.inc}
  {$ENDIF}
  {$IFNDEF DELPHIXE2_LVL}
  {$I TRIAL.INC}
  {$ENDIF}

{$ENDIF}

end.
