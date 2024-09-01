{ ***************************************************************************
  sgcIoT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcIoT_Classes;

interface

{$I sgcVer.inc}

{$IFDEF SGC_IOT}

uses
  Classes,
  // sgcTCP
  sgcTCP_Classes;

type
  TsgcIoTComponent_Base = class(TsgcTCPComponent_Base)
  end;

  TsgcIoTComponent_Base_Client = class(TsgcIoTComponent_Base)

  end;
  
{$ENDIF}  

implementation

end.
